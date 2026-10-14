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

`extconf.rb` / `mkmf` / `gem install` からのビルドはまだ通らない（§5 段階 3）。

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

## 4. ABI への追加（`MrApi`、末尾に追記、版は据え置き）

| 項目 | 用途 |
|---|---|
| `define_method_data` | `data` ポインタ付きのメソッド登録（`capi_dispatch` 1 本で全メソッドを受ける） |
| `str_cptr` | `RSTRING_PTR`: NUL 終端・書き込み可能な自前バッファ |
| `str_resize` / `str_cat` | `rb_str_resize` / `rb_str_set_len` / `rb_str_cat`（frozen チェック付き） |
| `ary_ptr` | `RARRAY_CONST_PTR` / `RARRAY_LEN` |
| `gc_register_address` / `gc_unregister_address` | `rb_gc_register_address` |

あわせて `ary_push` / `hash_set` が frozen なレシーバを拒むようにした
（以前は素通りしていた）。

## 5. 段階計画

| 段階 | 内容 | 状態 |
|---|---|---|
| 1 | ローダ（`monoruby_capi_init` 判別、`$LOAD_PATH` 上の `.so`）、`ruby.h` 互換ヘッダ、`capi.c`: クラス / モジュール / メソッド定義（arity -2〜15）、即値・数値・文字列・シンボル・配列・ハッシュ・Range、`rb_funcall` 系、ブロック（`rb_yield` 系、`rb_block_proc`）、例外（`rb_raise`、`rb_protect` / `rb_ensure` / `rb_rescue2` / `rb_jump_tag`）、`rb_scan_args` / `rb_get_kwargs`、ivar / 定数 / cvar / gvar、GC 登録、`rb_sprintf`（`PRIsVALUE`）、最小限の encoding、`rb_thread_call_without_gvl` | 済（`tests/capi.rs`: 同じ C ソースを CRuby と monoruby で動かし出力一致） |
| 2 | `Data` / `TypedData`（`TypedData_Make_Struct`、`DATA_PTR` の lvalue、dmark / dfree / dsize）、`rb_define_alloc_func` / `rb_undef_alloc_func`、`rb_block_call` / `rb_iterate`（C 関数をブロックとして渡す）、`rb_call_super`、`rb_struct_*`、`rb_keyword_given_p` を正しく | 未 |
| 3 | `RbConfig::CONFIG`（`rubyhdrdir` などを install root に）、`mkmf` が monoruby を相手に `Makefile` を作れること、`capi.c` を `libmonoruby_capi.a` として `LIBRUBYARG` に、`gem install` で C 拡張 gem がビルドされ `extensions/<arch>/…` から読めること | 未 |
| 4 | 実 gem で面積を詰める（`rb_io_*`、`rb_time_*`、`st_*`、`rb_enc_*` の残り、Bignum 系）。小さなものから: `bcrypt`、`strscan` 相当、`json` の C 拡張、`msgpack` | 未 |

### 5.1 段階 2 の要点（alloc 関数）

コアの alloc 関数は `extern "C" fn(ClassId, &mut Globals) -> Value` で、
エラーを返せず `Executor` も無いので、C の alloc 関数をそのまま呼べない。
`MR_CLASS_NATIVE` の「ペイロード無しで作り、最初に触った所で埋める」
（`native_extension_loading.md` §6.2「遅延 payload」）を使い、
`rb_define_alloc_func` されたクラスはネイティブクラスにして、
`DATA_PTR` / `TypedData_Get_Struct` が空のペイロードを見たら記録しておいた
alloc 関数を呼んで作ったオブジェクトからペイロードを移す、を予定している。
そのためにペイロードを解放せずに取り出す ABI 項目（`native_take`）と、
定義済みクラスをネイティブにする項目が要る。

## 6. 既知の制約（段階 1）

- `rb_keyword_given_p` は常に 0。`rb_scan_args` の `:` は「末尾の引数が
  Hash ならキーワード」とみなす（`f({a: 1})` と `f(a: 1)` を区別しない）。
- `rb_protect` は `break` / `throw` などの非例外の大域脱出も例外オブジェクト
  として取り出すので、`rb_jump_tag` で再送すると種類が失われる。
- `rb_iter_break` は未対応（NotImplementedError）。
- `rb_warn` は `Kernel#warn("warning: ...")`（ファイル名・行番号は付かない）。
- `rb_gv_get` / `rb_gv_set` は `eval` 経由。
