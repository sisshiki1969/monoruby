# CRuby 互換 C API（`monoruby_capi/`）

CRuby 向けに書かれた C 拡張を、**ソースのまま** monoruby 用にビルドして
`require` できるようにする層。`doc/native_extension_loading.md` §4 の段階 5
（「必要なら A を `mr_*` の上に積む」）にあたる。設計検討の経緯は
`doc/c_extention.md`（2026-08）。

---

## 1. 使い方

```sh
root=~/.monoruby/v<version>      # globals::install_root()
cc -shared -fPIC -I "$root/include" -o foo.so foo.c "$root/capi/capi.c"
monoruby -I . -e 'require "foo"; ...'
```

- `include/` に `ruby.h`、`ruby/{ruby,intern,encoding,thread,version,…}.h`
  と、その土台の `monoruby_ext.h` が入る（`build.rs` が
  `monoruby_capi/include` と `monoruby_ext_sys/include` から置く）。
- `capi/capi.c` が `rb_*` の実装。**拡張ごとに静的にリンクする**（§3.1）。
- できた `.so` は `$LOAD_PATH` 上の普通の `require "foo"` / `require "x/foo"`
  で読める。初期化関数は CRuby と同じ `void Init_foo(void)`。
- CRuby 用にビルド済みの `.so` は読めない（§2）。読もうとすると
  `LoadError`（「monoruby's C API headers でビルドせよ」）になる。

`extconf.rb` のある拡張は CRuby と同じ手順でビルドできる（§3.10）:

```sh
monoruby extconf.rb && make          # RbConfig が $root/include と
                                     # $root/capi/libmonoruby_capi.a を指す
monoruby "$root/bin/gem" install foo # C 拡張付き gem も同じ経路でビルド
```

`gem install` したものは monoruby 専用の gem home
（`~/.monoruby/gems/<api version>`、`Gem.default_dir`）に入る。

## 2. 互換の範囲: API 互換、ABI 非互換

`VALUE` は monoruby の `Value` のビット列そのもの（`value.rs`）。

| | CRuby | monoruby |
|---|---|---|
| `Qnil` / `Qfalse` / `Qtrue` / `Qundef` | 0x08 / 0x00 / 0x14 / 0x24 | 0x04 / 0x14 / 0x1c / 0x00 |
| Fixnum | `(i << 1) \| 1` | 同じ |
| Flonum | 下位 2 bit = `10` | 同じタグ（回転は別。マクロは関数経由） |
| Static Symbol | `(id << 8) \| 0x0c` | `(id << 32) \| 0x0c` |
| `ID` | `uintptr_t` | `uintptr_t`（中身は `IdentId`） |

即値はヘッダのマクロに焼き込まれるので、CRuby のヘッダでビルドした
`.so` は原理的に動かない。必ず再コンパイルする。境界で値を変換する案
（TruffleRuby の handle 方式）は、monoruby の GC が precise / non-moving で
あることから不要と判断した: 生の `Value` を C に渡し、生存は §3.3 で保証する。

拡張から見える CRuby との差:

- **`Qfalse != 0`。** `if (v)` で真偽を見るコードは誤動作する（`RTEST` /
  `NIL_P` を使っていれば問題ない）。
- オブジェクトレイアウトのマクロ（`RSTRING_PTR`、`RARRAY_LEN`、`RBASIC`
  …）は関数。`RBasic` / `RString` 構造体は無い。`RARRAY_PTR` は読み取り
  専用（書くときは `rb_ary_store` / `RARRAY_ASET`）。
- 世代別 GC が無いので write barrier（`RB_OBJ_WRITE` 等）は代入だけ。

## 3. 設計

### 3.1 シムは拡張に静的リンクする

`rb_*` の実体は C で書いた `capi.c` で、各拡張の `.so` に一緒にリンク
される。monoruby 本体は `rb_*` を一切 export しない。

- `rb_raise` は longjmp、`rb_funcall(recv, mid, n, ...)` は可変長引数で、
  どちらも Rust では素直に書けない（`setjmp` は Rust から呼べず、可変長の
  定義は nightly 機能）。C で書くのが自然。
- 本体から `--export-dynamic` で出す案は、テストバイナリを含む全バイナリの
  シンボル表を膨らませ、`rb_*` の ABI をコアに固定する。静的リンクなら
  `.so` は自己完結し、`monoruby_ext.h` の表（`MrApi`）だけが境界になる
  （`native_extension_loading.md` §4.3 の「拡張は monoruby をリンクしない」
  をそのまま守る）。
- 判別: ローダは `dlsym(handle, "monoruby_capi_init")` があれば C API 拡張
  とみなし、`monoruby_capi_init(ctx, Init_foo)` を呼ぶ（`ext.rs`
  `load_capi_extension`）。`$LOAD_PATH` 上で見つかった `.so` は
  `ext::is_capi_extension` が `RTLD_NOW` で試し開きして判定する。CRuby 用の
  `.so` は `rb_*` が未解決で `dlopen` 自体が失敗する（初期化コードは走らない）。

### 3.2 フレームと例外

`capi.c` はスレッドごとに「現在のフレーム」`capi_cur` を持つ:
`MrContext*`、渡されたブロック、`jmp_buf`、外側のフレーム。

- Ruby → C の入口（`capi_dispatch`、`monoruby_capi_init`）と `rb_protect`
  がフレームを積み、`setjmp` する。
- API 呼び出しが失敗すると（`MR_UNDEF` が返る / `-1` が返る）、エラーは
  インタプリタ側に保留されたまま `capi_unwind` が一番内側のフレームへ
  `longjmp` する。越えるのは**拡張自身の C フレームだけ**で、Rust / JIT の
  フレームは一切越えない（`native_extension_loading.md` §4.3 規則 2）。
  `capi_dispatch` は `MR_UNDEF` を返し、保留中のエラーがそのまま Ruby の
  例外になる。
- `rb_protect` はエラーを `error_take` で例外オブジェクトとして取り出し
  `rb_errinfo` に置く。`rb_jump_tag` / `rb_ensure` / `rb_rescue2` はこの上。
- Ruby を呼ぶ API（`rb_funcall`、`rb_yield`、`rb_proc_call`、
  `rb_thread_call_without_gvl`）は戻ったあと `capi_cur` を呼ぶ前の値に戻す。
  green thread の切り替えは LIFO でないので、呼び出し中に別スレッドが同じ
  OS スレッドで拡張に入っても自分のフレームが残る。

### 3.3 GC: 呼び出し中は全部生かす

CRuby は C スタックを保守的に走査するので、拡張はローカル変数の `VALUE`
を気にせず `rb_funcall` を挟める。monoruby の GC は precise なので、層が
C に渡す**ヒープ上の `VALUE` はすべて temp stack に積む**（`root()`）。
temp stack はメソッド呼び出し（と `Init_`）が戻ったときにインタプリタが
切り詰める。TruffleRuby の `ExtensionCallStack` と同じ考え方
（`doc/c_extention.md` §3.4）。

呼び出しを越えて C のグローバル変数に持つ値は CRuby と同じく
`rb_gc_register_address` / `rb_global_variable`（アドレスを毎回読む新しい
ABI 項目 `gc_register_address`）か `rb_gc_register_mark_object`（pin）。

代償: 1 回の呼び出しの中で大量に値を作るループは、戻るまで全部生かす。

### 3.4 メソッド

`rb_define_method(klass, name, func, argc)` は `capi_method { func, arity }`
を確保し、新しい ABI 項目 `define_method_data` で「`capi_dispatch` +
そのポインタ」として登録する。`capi_dispatch` が arity（-2、-1、0〜15）に
応じて CRuby の呼び出し規約で `func` を呼ぶ。引数個数のチェックは固定
arity ならコア側の builtin と同じ。

`rb_define_attr` は Ruby の `attr_reader` / `attr_writer` を呼んだあと
`public` にする（C から呼ぶと require された場所の既定の可視性、
トップレベルなら private を拾うため）。

### 3.5 文字列

`RSTRING_PTR` は新しい ABI 項目 `str_cptr` を呼ぶ: 共有中なら切り離し、
長さの直後（予備容量）に NUL を置き、code range のキャッシュを捨てて、
String 自身のバッファを返す。書き込み可能で、`rb_str_new(NULL, n)` して
`RSTRING_PTR` に書く CRuby の定石がそのまま動く。String は動かない
（non-moving）ので、リサイズされるか回収されるまで有効。
`rb_str_resize` / `rb_str_set_len` は `str_resize`、`rb_str_cat` は `str_cat`。

### 3.6 そのほか

- `ID` は `IdentId`。`rb_id2name` は ID ごとに一度だけ複製した C 文字列を
  返す（プロセスの寿命の間有効）。
- `rb_cObject` などのグローバル変数は `monoruby_capi_init` が定数参照で
  埋める。拡張の `static` は 1 プロセスに 1 つなので、テストハーネスの
  ように 1 プロセスで複数のインタプリタを作ると後のものが勝つ（`Init_`
  はインタプリタごとに走る）。ネイティブ拡張全般の既知の制約
  （`native_extension_loading.md` §6.2）。
- `rb_encoding` は名前ごとの小さな記録。添字 0/1/2 は CRuby と同じく
  ASCII-8BIT / UTF-8 / US-ASCII。
- `rb_thread_call_without_gvl` は `call_blocking`（別スレッドで実行、呼んだ
  Ruby スレッドは待つ）。ubf は呼ばない。
- 多くの関数は Ruby のメソッドを `rb_funcall` で呼ぶだけの薄い実装
  （TruffleRuby の `Truffle::CExt` と同じ方針）。正しさ優先で、ホットな
  ものから順に ABI 側へ寄せる。

### 3.7 Data / TypedData と alloc 関数（段階 2）

`Data` / `TypedData` のオブジェクトは本体のネイティブオブジェクト
（`native_new`、種別 `data_ops`）で、ペイロードは層の記録
`capi_data { data, dmark, dfree, type }`。`ruby.h` の `struct RData` /
`struct RTypedData` はこの記録のビューなので、`RDATA(obj)->data` /
`DATA_PTR(obj)` は lvalue として書ける。`data_ops` の mark は
`type->function.dmark`（型無しなら `dmark`）を、free は `dfree`
（`RUBY_DEFAULT_FREE` なら `xfree`）を呼んで記録を解放する。
`dsize` / `dcompact` は読まない（non-moving）。

alloc 関数はコアの `alloc_func`（`fn(ClassId, &mut Globals) -> Value`、
`Executor` 無し、エラー不可）から直接は呼べないので遅延する:

1. `rb_define_alloc_func(klass, f)` は新しい ABI 項目 `class_set_alloc` で
   `klass` のインスタンスをネイティブオブジェクトにし（既定の alloc は
   「ペイロード無しで作る」`ext_alloc_func`）、`f` を層の表に記録する。
   Ruby で定義したサブクラスはコアの継承でそのまま同じになる。
2. `new` / `allocate` / `dup` が作った空のインスタンスを、`DATA_PTR` /
   `TypedData_Get_Struct` / `rb_check_typeddata` / `RTYPEDDATA_P` が最初に
   見たとき、クラス（と祖先）の alloc 関数を呼び、できたオブジェクトから
   記録を `native_take`（解放せずに外す）で取り、`native_set` で本物に
   付ける。CRuby で `allocate` が alloc 関数を呼ぶのと同じ結果になる。
   `initialize_copy` の中の `TypedData_Get_Struct(copy)` もこれで埋まる。
3. alloc 関数を登録していないクラスに `Data_Wrap_Struct` したときも
   その場で `class_set_alloc` する（`klass == 0` の隠しオブジェクトは層の
   匿名クラス）。インスタンスが既にあるクラスでやると JIT の ivar 配置の
   前提が崩れるので、拡張の `Init_` で済ませること。
4. `rb_undef_alloc_func` は `MR_ALLOC_UNDEF`（`new` が
   「allocator undefined for X」）。

`rb_check_typeddata` のエラー文（「wrong argument type X (expected Y)」、
nil / true / false / 型無し Data / 別の型名）と `parent` による型の継承は
CRuby と同じ。

### 3.8 C 関数をブロックにする

`rb_block_call` / `rb_proc_new` / `rb_iterate` は、層の隠しクラス
（`Class.new(Object)`、ネイティブ）のインスタンスに `{func, arg, block}` を
持たせ、その `call(*args)` を `Method#to_proc` で Proc にして渡す。
`call` の入口 `block_dispatch` は `capi_dispatch` と同じフレームを積み、
そのフレームのブロックを「作った時点のメソッドのブロック」（Proc に
しておく）にするので、コールバック内の `rb_yield` は CRuby と同じく
`rb_block_call` を呼んだメソッドのブロックに届く。`rb_block_call` の
callback が NULL なら自分のブロックをそのまま渡す。

`rb_iter_break` / `rb_iter_break_value` は層の隠し例外クラス
（`Exception` 直下なので Ruby 側の `rescue` に捕まらず、`ensure` は走る）を
投げ、`rb_block_call` / `rb_iterate` が受け止めて値を返す。

### 3.9 `rb_call_super` と Struct

`rb_call_super` は新しい ABI 項目 `call_super`: コンテキストのフレーム
（この C メソッド自身の lfp）の `FuncId` と名前、`self` のクラスから
`Store::check_super` で次の定義を引き、`invoke_func_named` で呼ぶ。
ブロックは呼び出し元のものを渡す（CRuby の `rb_call_super` と同じ）。
メソッドの外（`Init_`）では RuntimeError。

`rb_struct_define` / `rb_struct_define_under` は `Struct.new` に、
`rb_struct_new` / `aref` / `aset` / `getmember` / `members` / `size` は
それぞれの Ruby メソッドに委ねる。`rb_obj_call_init` は private の
`initialize` をブロック付きで、`rb_class_new` / `rb_module_new` は
`Class.new` / `Module.new`。

### 3.10 mkmf / `gem install`（段階 3）

CRuby の `extconf.rb` は `mkmf` が `RbConfig` から得る値だけでビルドを
組み立てる: ヘッダの場所（`rubyhdrdir` / `rubyarchhdrdir`）、コンパイラと
フラグ、そして拡張がリンクする「`libruby`」（`LIBRUBYARG`）。vendored の
`rbconfig.rb` はスナップショット元の CRuby の値を答えるので、そのままでは
CRuby のヘッダでビルドした（monoruby では読めない）`.so` ができる。
`builtins/capi_config.rb`（startup.rb の最後に読む）が
`RbConfig.fire_update!` でこれらを monoruby のものに差し替える:

| キー | 値 |
|---|---|
| `rubyhdrdir` / `rubyarchhdrdir` / `topdir` | `<install root>/include` |
| `LIBRUBYARG` / `LIBRUBYARG_SHARED` / `LIBRUBYARG_STATIC` | `-L<install root>/capi -lmonoruby_capi` |
| `LIBRUBY` / `LIBRUBY_A` / `LIBRUBY_SO` | `libmonoruby_capi.a` |
| macOS のみ: `CC` / `LDSHARED` / `DLDFLAGS` / `warnflags` … | clang と `-bundle` リンク（CRuby の macOS 版 rbconfig 相当）。`DLEXT` は startup.rb が `bundle` にしている |

- **`libmonoruby_capi.a`** は `build.rs`（`build_capi_archive`）が `cc`
  crate で `capi.c` をコンパイルして `<install root>/capi/` に置く
  アーカイブ。§3.1 の「拡張ごとに静的リンク」を、mkmf の `$(LIBS)` に
  `-lmonoruby_capi` が入ることで実現する。mkmf の `have_func("rb_...")`
  は conftest をこのアーカイブとリンクして判定するので、層に無い関数は
  正しく「no」になる。monoruby 本体にはリンクしない。
- `ruby/config.h`（ruby.h の設定部を分離）、`ruby/backward.h` /
  `subst.h`（中身は無い; `st.h` は段階 5 で本物になった、§3.12）を足した。mkmf は生成する Makefile で
  これらを全オブジェクトの前提に並べるので、存在しないと `make` が
  止まる。
- `Kernel#system` が `system(env, *cmd)` と spawn オプションを受けるように
  なった（mkmf の `xsystem` が環境ハッシュ付きで呼ぶ）。env ハッシュか
  オプションがあるときは `builtins::spawn` で `spawn` + `waitpid`。
- **`gem install`**: rubygems の `ExtConfBuilder` は `Gem.ruby`（= 今の
  monoruby）で `extconf.rb` を走らせ `make` するだけなので、上の RbConfig
  でそのまま通る。`.gem` の作成（`gem build`）に必要だった
  `Digest::Instance#new` を足した。
- **gem home は monoruby 専用**（`stdlib/rubygems/defaults/operating_system.rb`、
  rubygems が `$LOAD_PATH` から読む packager 向けフック）。`Gem.default_dir`
  は `~/.monoruby/gems/<api version>`、`Gem.user_dir` は
  `~/.local/share/gem/monoruby/<api version>`。rubygems はビルド済み拡張を
  `extensions/<platform>/<api version>/<gem>` だけで区別するので、ホスト
  CRuby の gem home に入れると同じ gem の CRuby ビルドと上書きし合う。
  ホストの gem ディレクトリは従来どおり `GEM_PATH`（`Gem.path`）に残り、
  default gem の spec（`Gem.default_specifications_dir`）もホストのまま。
- **`require` の gem フォールバック**: monoruby の `require` はホストの
  gem の `lib/` をプローブ結果から知っているが、monoruby 自身の gem home
  に入った gem は知らない。`$LOAD_PATH` にもそこにも無いときに
  `Gem.try_activate(feature)`（rubygems の `Kernel#require` が LoadError
  で行うこと）を呼び、activate されたら探し直す
  （`Executor::activate_gem_for`）。rubygems の起動中・相対 / 絶対パス・
  `--disable-gems` では呼ばない。
- `<install root>/bin/gem`（`bin/dist` では `bin/gem`）は CRuby の
  `bin/gem` そのもの。`monoruby "$root/bin/gem" install foo` で使う。
- `tests/mkmf.rs`: `tests/capi/capi_gem/`（`extconf.rb` 付きの gem）を
  `extconf.rb` + `make` で、また `gem build` + `gem install` で monoruby と
  ホスト CRuby の双方にビルド・インストールし、同じスクリプトの出力が
  一致することを見る。

### 3.11 実 gem で面積を詰める（段階 4）

`gem install` でビルドした実 gem を CRuby と比較して足りない API を埋めた。
`tests/capi_gems.rs` がホスト CRuby の gem cache にある `.gem` を monoruby
の `bin/gem` で専用 home に入れ、`tests/capi/gems/<gem>.rb` を双方で走らせて
出力を比べる（1 行目は読み込んだ拡張の名前なので、stand-in でなく C 拡張が
動いたことも見る）。

| gem | 状態 | 使う面 |
|---|---|---|
| bcrypt 3.1.22 | 一致 | `rb_str_new` / `StringValuePtr` / `NUM2INT`、特異メソッド定義 |
| msgpack 1.8.5 | 一致（Buffer / Packer / Unpacker / Factory / ext 型 / Timestamp / IO 相手 / pool） | TypedData + alloc、`rb_num2ll` / `rb_num2ull` と Bignum、`rb_hash_foreach`、`rb_io_write`、`rb_enc_*`、`rb_ext_ractor_safe`、`-fvisibility=hidden` |
| json 2.18.0 | ビルドとロードは通るが生成器が動かない（§6） | `RUBY_INTEGER_UNIFICATION`、`SIZEOF_UINT64_T`、`ALLOCV`、`rb_enc_interned_str` の `have_func` 判定 |
| strscan 3.1.8 | 一致（§3.13） | `ruby/re.h`: `rb_reg_onig_match` / `rb_reg_prepare_re` + `onig_search` / `onig_match`、`struct re_registers` をスタックに置く、`onig_foreach_name`、`rb_memsearch`、`rb_enc_precise_mbclen`（EUC-JP）、`rb_get_kwargs`、`_((args))` |

macOS の CI で msgpack の `extconf.rb` が monoruby ごと落ちた（スタック
オーバーフロー）のは層ではなく JIT の問題だった。mkmf が呼ぶ FileUtils の
`Entry_#postorder_traverse`（`children.each { |c| c.traverse { |e| yield e } };
yield self` の形）で、ブロック literal 付きの再帰呼び出しが特殊化の深さ
制限を素通りして無限に特殊化していた（再帰防止を入れた）。その上で同じ形
が、特殊化フレームから外に出たブロック（yield する literal / `&b` / `super`
を汎用呼び出しに渡す）の書き込みを呼び出し元の定数化ローカルが見ない既存
バグを露わにした。どちらも `tests/given_block_escape.rs` が回帰テストで、
層とは独立の修正（`codegen/jitgen/compile/method_call.rs` の
`note_given_block_escape`）。

層に足したもの:

- **可視性**: `RUBY_FUNC_EXPORTED` を `__attribute__((visibility("default")))`
  に、`RUBY_SYMBOL_EXPORT_BEGIN` / `END` を pragma に。msgpack は
  `-fvisibility=hidden` でビルドし `Init_msgpack` だけこれで出すので、
  空マクロだと `Init_` が隠れて「not a Ruby extension」になる。
- **config.h**: `RUBY_INTEGER_UNIFICATION`（無いと json が `Fixnum` /
  `Bignum` のジェネレータモジュールを作る）、`SIZEOF_INT64_T` /
  `SIZEOF_UINT64_T` / `HAVE_(U)INT64_T`。
- **Bignum 系**: `rb_big2ll` / `rb_big2ull` / `rb_big2long` / `rb_big2ulong`
  / `rb_big2dbl` / `rb_big2str` / `rb_dbl2big`、`rb_big_cmp` / `eq` / 四則 /
  ビット演算 / シフト（すべて `rb_funcall` 経由）、`RBIGNUM_SIGN` /
  `RBIGNUM_POSITIVE_P`、`rb_absint_size`（`abs.bit_length` から）、
  `rb_absint_singlebit_p`。`rb_num2ll` は Fixnum が 63 bit なので
  `long long` の端 1/64 が T_BIGNUM になることを踏まえて範囲を判定し、
  範囲外は CRuby と同じ `'long long'` / `'unsigned long long'` の文言で
  RangeError にする（`'long'` ではなく）。
- **ALLOCV**: `rb_alloc_tmp_buffer(_with_count)` / `rb_free_tmp_buffer`、
  `RB_ALLOCV` / `RB_ALLOCV_N` / `RB_ALLOCV_END`。CRuby は大きいものを
  String に載せて GC に解放させるが、ここでは `ruby_xmalloc` の
  ポインタを VALUE スロットに入れて `ALLOCV_END` で `free` する。
- **encoding.h**: `ENCODING_GET` / `SET`（`_INLINED` 含む）、coderange
  定数と `ENC_CODERANGE(obj)`（`valid_encoding?` / `ascii_only?` から毎回
  計算、`SET` / `CLEAR` は no-op）、`rb_enc_check` / `rb_enc_compatible` /
  `rb_enc_strlen` / `rb_enc_mbclen` / `rb_enc_precise_mbclen` /
  `rb_enc_codelen` / `rb_enc_codepoint_len` / `rb_enc_mbminlen` /
  `mbmaxlen` / `unicode_p` / `dummy_p`、`rb_enc_raise`、`rb_intern3`、
  `MBCLEN_*` マクロ。
- **ruby/util.h**（新設）: `ruby_strdup` / `ruby_strtoul` / `ruby_strtod` /
  `ruby_scan_oct` / `ruby_scan_hex` / `ruby_scan_digits` / `ruby_qsort`
  （比較関数に `void *d` を渡す CRuby 形。マージソートで実装）/
  `ruby_getcwd`。
- **ほか**: `rb_ext_ractor_safe`、`rb_proc_arity` / `rb_proc_lambda_p`、
  `rb_io_write` / `flush` / `puts` / `print` / `gets` / `getbyte` / `close` /
  `eof` / `binmode` / `addstr` / `printf` / `get_io` / `check_io`、
  `rb_str_replace`、`rb_str_to_interned_str`、`RB_OBJ_FROZEN_RAW`、
  `rb_isxxx` / `ISXXX` / `TOLOWER` / `TOUPPER`。`ruby.h` が `<stdio.h>` と
  `<inttypes.h>` を include する（mkmf の conftest は宣言なしに `printf`
  を呼ぶ）。
- **require の優先順**: stand-in（`stub/`）が当たった feature でも、
  `$LOAD_PATH` 上に monoruby 向けにビルドされた同名の `.so`
  （`ext::is_capi_extension`）があればそれを読む
  （`Store::built_extension_on_load_path`、`require.rs`）。monoruby の
  gem home から activate した gem の `lib/` はそこに入るので、
  `gem "msgpack"; require "msgpack"` は gem 自身の `msgpack.so` に届く。
  CRuby でビルドされた `.so` は `dlopen` できないので数えない。
- 未捕捉例外の報告（`x (MessagePack::UnknownExtTypeError)`）が
  クラスの完全な名前を出すようになった（以前は `UnknownExtTypeError`
  のみ。`MonorubyErr::class_name` の `Other` 腕）。

### 3.12 `st_table`（段階 5）

`ruby/st.h` の中身を入れた。実装は CRuby の `st.c`（public domain、
`ruby_4_0` ブランチ）をそのまま `monoruby_capi/src/st.c` に持ち込んだもので、
Ruby 内部ヘッダへの依存（`internal/bits.h` の `nlz_intptr`、`RUBY_ASSERT`、
`NO_SANITIZE`）を冒頭の数行で置き換え、`RHASH` に入り込む
`rb_hash_bulk_insert_into_st_table` とその裏の rehash 補助、内部用の
`set_table` を落としただけ。`st.h` も CRuby の宣言と `struct st_table` の
レイアウトをそのまま持ち、CRuby と同じく全関数は `rb_st_*` で、`st_insert`
などは `#define` で届く。`ruby.h` が末尾で `ruby/st.h` を include する
（CRuby と同じく、`<ruby/st.h>` を自分で include しない拡張のため。
`enum st_retval` はそちらへ移した）。メモリは `ruby_xmalloc` 系
（GC 圧力に数える）、テーブルが `st_index_t` を越えると RuntimeError
（オリジナルの `RUBY` 構成どおり）。

同じコードなので、挿入順の走査、`st_hash` / `st_hash_uint` / `st_numhash`
の値、`st_memsize` まで CRuby と一致する。`tests/capi/capi_st.c` が
numtable（1000 件の挿入・再挿入・`delete` / `shift` / `foreach` の
`ST_DELETE` / `ST_STOP`、`update` の 3 経路、`insert2`、`add_direct`、
`copy` / `clear`、bins の無い小テーブル）、strtable / strcasetable
（別ポインタ同内容の検索、`st_strcasecmp`）、独自 `st_hash_type`
（衝突だらけの hash で `foreach_check` / `foreach_with_replace` /
`keys_check` / `delete_safe` + `cleanup_safe`）、hash 関数群、
VALUE を鍵と値に持つ TypedData（mark で `st_foreach`）を CRuby と比べる。
`rb_memhash` は `st_hash(ptr, len, 0)`（CRuby はプロセスごとの seed を
混ぜるが、同一プロセス内で等しいことだけが契約）。

ホストの gem cache にある C 拡張で `st_table` を使うのは nokogiri だけ
（monoruby では `ext/nokogiri` が肩代わり）なので、実 gem での検証は無い。
oj / ruby-prof / stackprof のような `st_table` を使う gem がこれで
ビルドできる。

### 3.13 `ruby/re.h`（段階 6）

strscan のように Onigmo を自分で回す拡張のための面。`ruby/onigmo.h` は
monoruby がリンクしている `onigmo-regex` crate の `onigmo/onigmo.h`
そのもの（Onigmo 6.2）で、`OnigRegion` / `struct re_registers` は本物の
構造体なので `regs->beg[i]` の読み書きは CRuby と同じ。ただし crate の
Onigmo は `RUBY` を定義せずにビルドされていて `OnigRegion` が
`history_root` を持つ（32 バイト、CRuby は 24）ため、ヘッダの先頭で
`USE_CAPTURE_HISTORY` を強制し、拡張がスタックに置く構造体をライブラリと
そろえる（そろえないと `onig_region_init` がスタックを壊す）。

拡張の `.so` は monoruby のバイナリのシンボルを解決できないので、
`onig_search` / `onig_match` / `onig_region_*` / `onig_free` /
`onig_name_to_backref_number` / `onig_foreach_name` /
`onig_number_of_*` / `onig_error_code_to_str` は capi.c の薄い関数で、
`MrApi::onig_api()` が返す `MrOnigApi` テーブル（`void *` で型を消した
関数ポインタ）経由で monoruby 内の Onigmo を呼ぶ。`onig_new` と
エンコーディング / 構文のテーブル（データシンボル）は出さない。
パターンは Regexp を作って得る（`rb_reg_regcomp`）。

`regex_t` は monoruby のもの。`rb_reg_prepare_re(re, str)` は
`Regexp#match` が `str` を相手に使う codec（`RegexpInner::capi_compile_key`:
`check_match_encoding` で互換性を検査し、ネイティブ codec なら素のパターン
バイト列とその encoding、さもなくば engine が持つ変換済みパターン）で
`onig_new` したバッファを `(pattern, option, encoding)` 鍵のキャッシュから
返す（`ext::capi_regex`、プロセスの寿命まで解放しない）。`RREGEXP_PTR(re)`
（`rb_reg_ptr`）は Regexp 自身の codec のもので、`onig_foreach_name` などの
パターン側の問い合わせ用。CRuby の `struct RRegexp` は無いので
`RREGEXP(re)->ptr` は書けず、`RREGEXP_PTR` / `RREGEXP_SRC` は呼び出し。
どちらの `regex_t` も `onig_free` してはいけない。

`rb_reg_onig_match(re, str, match_fn, args, regs)` は CRuby 3.3 の形
（`ONIG_MISMATCH` を返す、`ONIGERR_TIMEOUT` は `Regexp::TimeoutError`、
ほかの負値は `RegexpError`）、`rb_reg_region_copy` は `onig_region_copy`
に失敗時の `rb_memerror`、`onig_region_memsize` は CRuby の計算。
`rb_memsearch` はバイト検索（`enc` は使わない。ASCII 互換 encoding では
CRuby も同じ）。`rb_long2int`、`rb_deprecate_constant`、`_((args))` /
`__((args))`（`ruby/backward/2/stdarg.h` の K&R 移行マクロ、strscan が
まだ使う）も足した。

`rb_enc_precise_mbclen` は monoruby 自身の走査（`value::precise_mbclen`:
UTF-8、EUC-JP / Shift_JIS 系、UTF-16 / 32、それ以外は 1 バイト）を
`MrApi::enc_precise_mbclen` で呼び、答えは CRuby と同じ Onigmo の符号化
（見つかれば長さ、`-1` が不正、`-1-n` が n バイト不足）。`ruby/encoding.h`
の `MBCLEN_*` は CRuby どおり `ONIGENC_MBCLEN_*` の別名で、後者は
`onigmo.h` とトークン単位で同じ定義を `#ifndef` で持つ（両方をどの順で
include してもよい）。`rb_enc_mbclen` / `rb_enc_strlen` / `rb_enc_codepoint_len`
はその上に乗る（strscan の `charpos` が EUC-JP で合うのはこれ）。

`require` の解決も二つ変えた。(1) stand-in（`stub/strscan.rb`）が当たり、
`$LOAD_PATH` 上に monoruby でビルドした `.so` があるとき、同じディレクトリ
に gem 自身の `.rb`（strscan の `lib/strscan.rb`: `require "strscan.so"`
の後に Ruby 側のメソッドを足す）があればそちらを返す。(2) `.so` /
`.bundle` 候補の `$LOAD_PATH` 走査で、monoruby が読めないもの（プローブ
結果に入るホスト CRuby の `x86_64-linux/strscan.so`）は飛ばして先を見る。
読めるものが無ければ最初のそれを返して従来の LoadError を出す。

検証: `tests/capi/capi_re.c`（`tests/capi.rs` の 4 本目）が `onig_search` /
`onig_match`（UTF-8 / バイナリ / Shift_JIS / EUC-JP の対象、
`Encoding::CompatibilityError`、不正バイト列の `ArgumentError`）、
名前付き捕獲、`rb_reg_region_copy`、`rb_memsearch`、`rb_reg_regcomp` /
`rb_reg_quote` / `rb_reg_options` / `rb_reg_nth_match`、`rb_long2int` の
RangeError、`rb_enc_precise_mbclen` の `MBCLEN_*` 判定を CRuby と比較。
`tests/capi_gems.rs` の `strscan`（3.1.8、CI は `gem install strscan`）が
`StringScanner` のほぼ全メソッド（`scan_integer` の Ruby 側含む）を
CRuby と比較する。残る差は `$LOADED_FEATURES` の順（monoruby は読む前に、
CRuby は読んだ後に登録する）だけ。

## 4. ABI への追加（`MrApi`、末尾に追記、版は据え置き）

| 項目 | 用途 |
|---|---|
| `define_method_data` | `data` ポインタ付きのメソッド登録（`capi_dispatch` 1 本で全メソッドを受ける） |
| `str_cptr` | `RSTRING_PTR`: NUL 終端・書き込み可能な自前バッファ |
| `str_resize` / `str_cat` | `rb_str_resize` / `rb_str_set_len` / `rb_str_cat`（frozen チェック付き） |
| `ary_ptr` | `RARRAY_CONST_PTR` / `RARRAY_LEN` |
| `gc_register_address` / `gc_unregister_address` | `rb_gc_register_address` |
| `class_set_alloc` | 定義済みクラスをネイティブにする（`rb_define_alloc_func`、`Data_Wrap_Struct`）/ alloc を外す（`rb_undef_alloc_func`） |
| `native_take` | ペイロードを解放せずに外す（遅延 alloc でできたオブジェクトから本物へ移す） |
| `call_super` | `rb_call_super` |
| `regexp_prepare` / `regexp_ptr` | `rb_reg_prepare_re` / `RREGEXP_PTR`: monoruby が持つ `regex_t`（§3.13） |
| `onig_api` | `MrOnigApi`: monoruby 内の Onigmo の関数群（`onig_search` ほか） |
| `enc_precise_mbclen` | `rb_enc_precise_mbclen` を monoruby の encoding 走査で |

あわせて `ary_push` / `hash_set` が frozen なレシーバを拒むようにした
（以前は素通りしていた）。

## 5. 段階計画

| 段階 | 内容 | 状態 |
|---|---|---|
| 1 | ローダ（`monoruby_capi_init` 判別、`$LOAD_PATH` 上の `.so`）、`ruby.h` 互換ヘッダ、`capi.c`: クラス / モジュール / メソッド定義（arity -2〜15）、即値・数値・文字列・シンボル・配列・ハッシュ・Range、`rb_funcall` 系、ブロック（`rb_yield` 系、`rb_block_proc`）、例外（`rb_raise`、`rb_protect` / `rb_ensure` / `rb_rescue2` / `rb_jump_tag`）、`rb_scan_args` / `rb_get_kwargs`、ivar / 定数 / cvar / gvar、GC 登録、`rb_sprintf`（`PRIsVALUE`）、最小限の encoding、`rb_thread_call_without_gvl` | 済（`tests/capi.rs`: 同じ C ソースを CRuby と monoruby で動かし出力一致） |
| 2 | `Data` / `TypedData`（`TypedData_Make_Struct`、`DATA_PTR` の lvalue、dmark / dfree）、`rb_define_alloc_func` / `rb_undef_alloc_func`、`rb_block_call` / `rb_proc_new` / `rb_iterate` / `rb_iter_break`（C 関数をブロックとして渡す）、`rb_call_super`、`rb_struct_*`、`rb_obj_call_init`、`rb_class_new` | 済（§3.7〜3.9、`tests/capi/capi_data.c`）。`rb_keyword_given_p` は未 |
| 3 | `RbConfig::CONFIG`（`rubyhdrdir` などを install root に）、`mkmf` が monoruby を相手に `Makefile` を作れること、`capi.c` を `libmonoruby_capi.a` として `LIBRUBYARG` に、`gem install` で C 拡張 gem がビルドされ `extensions/<arch>/…` から読めること | 済（§3.10、`tests/mkmf.rs`）。gem home は monoruby 専用 |
| 4 | 実 gem で面積を詰める（`rb_io_*`、`rb_enc_*`、Bignum 系、`ALLOCV`、`ruby/util.h`、シンボル可視性）。`bcrypt` と `msgpack` が CRuby と一致 | 済（§3.11、`tests/capi_gems.rs`）。`json` は `Qfalse != 0` に当たる（§6）、`strscan` は段階 6 |
| 5 | `st_table`（`ruby/st.h` + CRuby の `st.c`） | 済（§3.12、`tests/capi/capi_st.c`） |
| 6 | `ruby/re.h`（Onigmo を `MrOnigApi` 経由で、`rb_reg_prepare_re` / `rb_reg_onig_match`、`rb_enc_precise_mbclen` を本物に、`require` の `.so` 解決） | 済（§3.13、`tests/capi/capi_re.c`、`tests/capi_gems.rs` の strscan） |

## 6. 既知の制約

- `rb_keyword_given_p` は常に 0。`rb_scan_args` の `:` は「末尾の引数が
  Hash ならキーワード」とみなす（`f({a: 1})` と `f(a: 1)` を区別しない）。
- `rb_protect` は `break` / `throw` などの非例外の大域脱出も例外オブジェクト
  として取り出すので、`rb_jump_tag` で再送すると種類が失われる。
- `rb_iter_break` は隠し例外で実装しているので、`rb_block_call` /
  `rb_iterate` の外（`rb_funcall` で呼んだメソッドに渡した
  `rb_proc_new` の Proc の中など）で使うとその例外が外へ出る。
- `rb_proc_new` の Proc は `Method#to_proc` 由来なので `lambda?` が true
  （CRuby は false）。arity は同じ -1。
- alloc 関数は遅延して呼ばれる（§3.7）。`allocate` 直後にペイロードを
  読まずに `ObjectSpace` 等で観察しても alloc 関数はまだ走っていない。
- `dsize` / `dcompact` は使わない。`RUBY_TYPED_*` のフラグはすべて無視。
- `rb_warn` は `Kernel#warn("warning: ...")`（ファイル名・行番号は付かない）。
- `rb_gv_get` / `rb_gv_set` は `eval` 経由。
- mkmf の `create_header` は使えるが、`$(hdrdir)/ruby/backward/*` の中身は
  無い。`RbConfig::CONFIG["CC"]` は Linux では
  スナップショットの `gcc` のまま（`CC` 環境変数で上書きできる）。
- **json 2.18 の C 拡張は動かない。** ビルドとロードは通るが、
  `fbuffer.h` の `if (fb->io)` と generator.c の `if (!data->vstate)` が
  `Qfalse == 0` を前提にしていて（§2）、`JSON.generate` が `false` に
  `write` を呼ぶ / `false` を State として扱う。層側では直せない
  （`VALUE` の変換を境界に入れると `RARRAY_PTR` 等の生メモリ経路と
  食い違う）。vendored の stand-in（`gem/json/`、Rust 移植）が従来どおり
  使われる。`if (v)` で真偽を見る拡張は同じ理由で動かない。
- `ruby/re.h` は `onig_new` と Onigmo のテーブル群を出さない（§3.13）。
  `OnigRegion` は CRuby より 8 バイト大きい（`history_root`）。
  `rb_reg_prepare_re` のバッファは解放されない。
- `rb_enc_codepoint_len` が符号位置を組み立てるのは UTF-8 だけ（ほかの
  多バイト encoding では先頭バイトを返す）。`ENC_CODERANGE` は呼ぶたびに
  計算する。
- `ALLOCV` の確保は例外で巻き戻ると解放されない（CRuby は String に
  載せて GC に任せる）。
- ホスト CRuby に同じ gem があると、`require "msgpack"` の Ruby 側
  （`lib/msgpack.rb`）はプローブ結果にあるホストの `lib/` から読まれ、
  `.so` だけ monoruby の gem home のものになる。`gem "msgpack"` で
  monoruby 側を先に activate すれば両方そろう。
- `gem install` は monoruby の gem home に入れる。ホスト CRuby で
  入れた C 拡張 gem の `.so` は monoruby では読めない（stub のあるものは
  stub が読まれる）ので、monoruby で使うには monoruby で入れ直す。
