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
  `st.h` / `subst.h`（中身は無い）を足した。mkmf は生成する Makefile で
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

あわせて `ary_push` / `hash_set` が frozen なレシーバを拒むようにした
（以前は素通りしていた）。

## 5. 段階計画

| 段階 | 内容 | 状態 |
|---|---|---|
| 1 | ローダ（`monoruby_capi_init` 判別、`$LOAD_PATH` 上の `.so`）、`ruby.h` 互換ヘッダ、`capi.c`: クラス / モジュール / メソッド定義（arity -2〜15）、即値・数値・文字列・シンボル・配列・ハッシュ・Range、`rb_funcall` 系、ブロック（`rb_yield` 系、`rb_block_proc`）、例外（`rb_raise`、`rb_protect` / `rb_ensure` / `rb_rescue2` / `rb_jump_tag`）、`rb_scan_args` / `rb_get_kwargs`、ivar / 定数 / cvar / gvar、GC 登録、`rb_sprintf`（`PRIsVALUE`）、最小限の encoding、`rb_thread_call_without_gvl` | 済（`tests/capi.rs`: 同じ C ソースを CRuby と monoruby で動かし出力一致） |
| 2 | `Data` / `TypedData`（`TypedData_Make_Struct`、`DATA_PTR` の lvalue、dmark / dfree）、`rb_define_alloc_func` / `rb_undef_alloc_func`、`rb_block_call` / `rb_proc_new` / `rb_iterate` / `rb_iter_break`（C 関数をブロックとして渡す）、`rb_call_super`、`rb_struct_*`、`rb_obj_call_init`、`rb_class_new` | 済（§3.7〜3.9、`tests/capi/capi_data.c`）。`rb_keyword_given_p` は未 |
| 3 | `RbConfig::CONFIG`（`rubyhdrdir` などを install root に）、`mkmf` が monoruby を相手に `Makefile` を作れること、`capi.c` を `libmonoruby_capi.a` として `LIBRUBYARG` に、`gem install` で C 拡張 gem がビルドされ `extensions/<arch>/…` から読めること | 済（§3.10、`tests/mkmf.rs`）。gem home は monoruby 専用 |
| 4 | 実 gem で面積を詰める（`rb_io_*`、`rb_time_*`、`st_*`、`rb_enc_*` の残り、Bignum 系）。小さなものから: `bcrypt`、`strscan` 相当、`json` の C 拡張、`msgpack` | 未 |

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
- mkmf の `create_header` は使えるが、`$(hdrdir)/ruby/backward/*` や
  `st.h` の中身（`st_table`）は無い。`RbConfig::CONFIG["CC"]` は Linux では
  スナップショットの `gcc` のまま（`CC` 環境変数で上書きできる）。
- `gem install` は monoruby の gem home に入れる。ホスト CRuby で
  入れた C 拡張 gem の `.so` は monoruby では読めない（stub のあるものは
  stub が読まれる）ので、monoruby で使うには monoruby で入れ直す。
