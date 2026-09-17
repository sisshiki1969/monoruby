# ネイティブライブラリの動的ロード — 同梱をやめる方法の検討

monoruby には C 拡張を含め、外部の C / Rust ライブラリを実行時にロードする
仕組みがない。そのため C 拡張 gem に相当する機能は「gem の Ruby 半分を
vendoring し、ネイティブ半分を Rust で書き直し、依存する C ライブラリを
`cc` で同梱する」形で **すべてコアのバイナリとワークスペースの中に** 抱えて
いる。本稿は、この状態を解消する方法として、(A) CRuby C API 互換層、(B)
monoruby 独自の動的拡張 ABI、(C) システム共有ライブラリの dlopen、(D) Cargo
feature 分割、(E) 別プロセス、(F) WASM、(G) Fiddle 上の Ruby バインディング
を比較し、B を土台に段階的に進める案を推奨する。

`doc/c_extention.md`（CRuby C 拡張ロードの設計検討、2026-08）と
`doc/nokogiri.md` §2（nokogiri での A / B / 純 Rust の比較）の続きにあたる。
前者の Path B / C の結論は変わらないが、本稿は「新しい拡張を書けるように
する」より先に「**今抱えているものを外へ出す**」ことを最初の目標に置く。

---

## 1. 現状

### 1.1 何をどれだけ抱えているか

`target/release`（x86-64 Linux、2026-09-17、master `7692aee`）の実測:

| 項目 | 規模 |
|---|---|
| バイナリ | 16.5 MB。`ldd` は libc / libm / libgcc_s のみ（完全静的リンク） |
| 同梱 C ソース（ワークスペース内） | libxml2 5.3 MB + libxslt 1.4 MB + gumbo 1.3 MB（`libxml2-src/vendor/`）+ SQLite 9.5 MB（`libsqlite3-src/vendor/`）= **17.5 MB** |
| crate 経由で同梱ビルドされる C | zlib（`libz-sys` `static`）、libzstd 1.5.7（`zstd-sys`）、libffi（`libffi-sys`、macOS/aarch64 のみ system）、libyaml は Rust 移植（`libyaml-safer`） |
| リンクされる C オブジェクト | libxml2.a 2.7 MB、libsqlite3.a 2.6 MB、libxslt.a 0.7 MB、libgumbo.a 0.6 MB ≒ **6.6 MB**（prism 1.5 MB / onigmo 1.1 MB はコアの一部） |
| ネイティブ半分の Rust | 約 11k 行 — `sqlite3.rs` 2.2k、`nokogiri/` 5k、`zlib.rs` / `zstd.rs` / `yaml.rs` / `json.rs` / `cipher.rs` / `digest.rs` |
| Ruby 半分の vendoring | `gem/` 3.3 MB — prism 1.8 MB、nokogiri 0.6 MB、psych 0.2 MB、sqlite3 0.1 MB、msgpack / yajl / strptime / cool.io / zstd-ruby / gosu / ffi_c / bigdecimal |

`monoruby/src` でこれらの C crate に依存しているファイルは 6 つだけ
（`fiddle.rs`, `nokogiri.rs`, `sqlite3.rs`, `yaml.rs`, `zlib.rs`, `zstd.rs`）で、
依存は一方向（コア → 拡張）である。これは後述の分離が現実的である根拠になる。

### 1.2 この形になった 3 つの判断

いずれも正しく、本稿でも前提として引き継ぐ。

1. **CRuby ビルド済みの `.so` は原理的に読めない。** `Value` のビット配置が
   CRuby の `VALUE` と違う（`Qnil` = `0x04` vs `0x08`、Fixnum タグ位置）ので、
   `.so` に埋め込まれた即値定数がそのままでは意味を持たない
   （`doc/c_extention.md` §5）。「ロードできる」ためには最低でも monoruby の
   ヘッダでの再コンパイルが要る。
2. **汎用 FFI（Fiddle）経由はホットなライブラリには遅すぎる。** activerecord
   では Fiddle 版 sqlite3 が 1 反復の **35 %** を占め、native builtin 化で 0 に
   なった（`doc/activerecord_liquid_il_rack_investigation_2026-09.md` §3.1）。
   1 クエリあたり 30 回前後の FFI 呼び出しで、各呼び出しの引数変換
   （`value_to_carg` / `bits_to_value`）と `ffi_call` が支配する。
3. **CRuby と出力を byte 単位で一致させるには同じライブラリを使うしかない。**
   nokogiri のエラーメッセージ・シリアライズ・XPath の細部は libxml2
   （nokogiri のパッチ入り）の挙動そのもの（`doc/nokogiri.md` §1）。
   zlib / zstd / libyaml も「同じ実装だから byte 一致」で検証している。

### 1.3 既にある土台

- **builtin の ABI は既に `extern "C"`**:
  `extern "C" fn(&mut Executor, &mut Globals, Lfp, BytecodePtr) -> Option<Value>`
  （`executor.rs:16`）。JIT の inline cache も VM もこの関数ポインタを呼ぶ
  だけなので、動的ロードした関数を登録する受け口は事実上ある。
- **`dlopen` / `dlsym` は実装済み**: `Kernel.___dlopen` / `___dlsym`
  （`builtins/kernel.rs:4333`、`libc` の生 API、`dlerror` の排水込み）。
- **ネイティブ資源を持つオブジェクトの型がある**: `ObjTy::NATIVE` と
  `NativeData { mark, dup, as_any }`（`value/rvalue.rs:258`）。
  `define_class_with_instance_ty(NATIVE)` で JIT がペイロードを ivar と
  誤認しない。GC は precise / non-moving なので、外部が持つ `Value` は
  「どこにあるかを教えてもらえれば」そのままマークできる（TruffleRuby の
  handle 二重化は不要）。
- **ブロッキング呼び出しのオフロード**: `NativeOp::{Ffi, Sqlite, Flock,
  Fcntl, Open}`（`doc/threads.md` §9）。`Fiddle.___prepare` の
  `PREPARE_BLOCKING` フラグで Ruby 側から申告できる。
- **per-version の install root**: `~/.monoruby/v<ver>/{lib,stub,builtins}`
  （`build.rs`）。ここに `ext/` を足せば、ABI の版違いは自然に避けられる。
- **require の解決順**: `stub/` が `$LOAD_PATH` より先に pin されており
  （`globals/require.rs:263`）、`.rb` 候補が `.so` 候補より先に探索される。
  gem 自身の `.so` を stand-in が上書きする仕組みはこれで成立している。
- **例外は戻り値で伝える方針が確定している**: builtin 内の panic は JIT
  フレーム（unwind 情報なし）越しに巻き戻せないので捕捉しない
  （`executor.rs:17` 付近のコメント）。`Option<Value>` + `set_error`。

---

## 2. 選択肢

### A. CRuby C API 互換層（`doc/c_extention.md` の Path A / C）

`ruby.h` 互換ヘッダと `rb_*` シムを提供し、`gem install` の再コンパイルで
既存 gem の C 拡張を monoruby 向けに生成して動かす。

長所:
- gem エコシステムがそのまま手に入る。nokogiri / sqlite3 / pg / bcrypt /
  markly … を個別に移植する必要がなくなり、`gem/` の vendoring も要らなく
  なる。**「抱えている」問題を根本から解く唯一の案**。
- `gem install` のフロー（`Gem::Ext::ExtConfBuilder` → `mkmf` → `make`）は
  Ruby で書かれているので、`RbConfig::CONFIG` を monoruby 値にすれば流用
  できる（旧稿 §6–7）。

短所:
- **面積が巨大。** nokogiri 一つで `rb_*` 201 シンボル、`TypedData_Get_Struct`
  77 箇所、`StringValueCStr` 85 箇所、`rb_funcall` 64 箇所。「よく使われる
  順に」積んでも、最初の実 gem が通るまでに数百シンボルが要る。
- **`rb_raise` の longjmp 型例外。** C 拡張は任意の深さから `rb_raise` を
  呼ぶ。monoruby は JIT フレームを unwind できないので、`rb_protect` 相当を
  `setjmp` で作り、C 呼び出しの境界（builtin trampoline の直下、JIT
  フレームを跨がない場所）で必ず受けてから `set_error` に変換する設計が
  要る。C 側のリソース解放は `rb_ensure` に頼る CRuby 流儀に従わせる。
- **GVL がなく green thread。** `rb_thread_call_without_gvl` を
  `NativeOp` オフロードに写像する。`rb_thread_create` 等は対象外にする。
- **レイアウト前提マクロ。** `RSTRING_PTR` / `RARRAY_PTR` は lazy
  materialize + pin、`RTYPEDDATA` は co-allocation で吸収し、`RARRAY_PTR[i] =`
  の lvalue 書き込みは諦める（旧稿 §4）。
- **性能。** C 拡張のメソッドは JIT から不透明。今 `__take` / `___read` 等に
  付いているインライナは効かず、「CRuby と同じ速さ」が上限。
- 前回検討時から進んだ点: `ObjTy::NATIVE` + `NativeData` がそのまま
  `TypedData` の受け皿になる（`doc/nokogiri.md` §5 の見立て通り）。

### B. monoruby 独自の動的拡張 ABI（C / Rust の cdylib を必要時に dlopen）

`mr_*` 接頭辞の **C ABI** を定義し、`Init_<name>(MrContext*)` を `dlsym` して
呼ぶ。今ある sqlite3 / nokogiri / zlib / zstd / psych のネイティブ半分を、
この ABI の上に載る**別 crate（cdylib）**に分離し、`~/.monoruby/v<ver>/ext/`
に置く。

長所:
- コアバイナリから 6.6 MB の C オブジェクトと 11k 行の Rust が出ていき、
  コアのビルドから `cc` の C ビルドが消える。
- **C も Rust も同格の拡張**になる。libxml2 のパッチや SQLite の版は
  コアの `Cargo.lock` から切り離せる。C で書く方が自然なもの（libxml2 の
  glue）は C のまま書ける。
- VM 側の受け口はほぼそのまま（§1.3）。`dlopen` も実装済み。
- 新しい拡張を書く人向けの API が生まれる。A を後で積むときの下層にも
  そのままなる。

短所・設計上の要点:
- **Rust cdylib は `monoruby` crate にリンクしてはいけない。** Rust ABI は
  不安定、toolchain は nightly 固定、そしてコアには `thread_local!` が 31 個
  （`CODEGEN`、`ALLOC` を含む）ある。拡張側が `monoruby` をリンクすると
  静的変数が**二重化**して GC とコード生成が壊れる。拡張は `MrContext` に
  載せた**関数ポインタ表（vtable）**経由でしか VM に触れない、が鉄則。
  Rust 側には `monoruby-ext-sys`（C ヘッダの bindgen 相当）と薄い安全
  ラッパを別 crate で用意する（magnus / rb-sys の monoruby 版）。
- **`Lfp` / `BytecodePtr` を ABI に晒さない。** 晒すとフレームレイアウトが
  凍結される。拡張向けには
  `fn(MrContext*, self, argc, argv, block) -> MrValue`（エラーはコンテキスト
  側のフラグ）の trampoline を挟む。引数コピー 1 回のコストは、実際に
  ネイティブ処理をする呼び出しに対しては測定できない。
- 例外は戻り値で伝える（現行規約と同じ）。unwind は境界を跨がない。
  Rust 拡張は `panic = "abort"` か、境界で `catch_unwind` する。
- GC: 拡張が保持する `Value` は (a) `NativeData::mark` 相当のコールバック
  （オブジェクトに属するもの）と (b) per-call の pin stack（呼び出し中の
  一時値、旧稿 §3.6）で見せる。`mr_gc_register_address` 型の永続ルートも
  用意する（クラス定数などに使う）。
- Green thread: ブロッキング呼び出しは拡張側が `mr_call_blocking(fn, arg)`
  で申告し、`NativeOp` に落とす（今の `NativeOp::Sqlite` と同じ）。
  コールバック再入（sqlite3 の `create_function`、nokogiri の XPath
  ハンドラ）は `mr_funcall` で行い、その中で raise された例外は
  「stash して C から戻ってから再 raise」の現行規約を ABI として明文化する。
- 失うもの: 拡張が定義するメソッドには JIT インライナを付けられない。
  今インライン化されているのは `Fiddle.___read` / `___write` などコア側の
  primitive なので、**それらはコアに残せば**実害はない。
- 完全静的リンクの単一バイナリという配布上の利点は部分的に失う。ただし
  `ext/` を `build.rs` が同じ install root に置くなら、ユーザーから見た
  体験（`cargo install` して終わり）は変わらない。

### C. システムの共有ライブラリを dlopen（Rust の glue はコアに残す）

libxml2 / libsqlite3 / libz / libzstd を同梱せず、OS のものを実行時に
`dlopen`。`libz-sys` の `static` feature を外すのと同じ発想を全ライブラリに
広げる。

長所:
- 一番小さい一歩。17.5 MB の C ソースと `cc` ビルドが消える。Rust の glue
  はそのまま。
- Linux ディストリと macOS はどれも OS に持っている（macOS:
  `/usr/lib/libxml2.2.dylib`、`libsqlite3.dylib`、`libz.1.dylib`）。libffi は
  既に macOS/aarch64 で system 版に切り替えているので前例がある。

短所:
- **版のブレ。** nokogiri のパッチ入り libxml2 でなくなるので出力一致が
  崩れうる（nokogiri 自身が `--use-system-libraries` を公式にサポートして
  いるので「許容されるモード」ではある）。SQLite / zlib / zstd のマイナー
  版差は実用上ほぼ影響しない。
- 動的ロードの**仕組み**は生まれない。「同梱を減らす」だけ。
- 位置づけとしては B と組み合わせ、**拡張ごとに `bundled` / `system` を
  選べる**ようにするのが良い（`libz-sys` がやっている通り）。

### D. Cargo feature でのビルド時分割

`nokogiri` / `sqlite3` / `zstd` / `psych` / `zlib` を optional feature にし、
既定では全部付ける。

長所: 半日で入る。不要な人のフットプリントは即減る。依存が一方向である
ことの確認になり、B で crate 境界を切るときの下ごしらえになる。

短所: 動的ロードではない。配布バイナリは結局全部入り。

### E. 別プロセス + IPC

ライブラリを helper プロセスで動かし、Ruby 側とは IPC。

長所: クラッシュ隔離。green thread と相性が良い（ブロッキングが問題に
ならない）。

短所: 呼び出し粒度が細かい API（nokogiri の Node 操作、sqlite3 の `step`
ごと）には遅すぎる。Fiddle の 35 % より悪くなる。バッチ型（画像変換など）
以外には不適。

### F. WASM コンポーネント（wasmtime）

長所: サンドボックス、可搬性、ABI が仕様として安定。

短所: wasmtime 自体が 10 MB 級で「フットプリント削減」と矛盾する。
C ライブラリは wasi-sdk で別ビルドが要る。性能も落ちる。TruffleRuby が
Sulong（同種の間接実行）を捨ててネイティブ実行に戻った理由
（起動時間・warmup・互換性・配布サイズ、旧稿 §3.5）がそのまま当てはまる。
見送り。

### G. Fiddle / ffi 上の Ruby バインディング（コールドなライブラリ向け）

`gem/ffi_c.rb` が既にこの形（ffi gem の Ruby 半分を `Fiddle.___*` primitive
の上で動かす）。gosu / cool.io のように「ロードできて落ちなければ良い」
段階のものにはこれで十分で、ネイティブ層は要らない。ホットなものには
使えない（§1.2 判断 2）。B の対象を絞る基準として使う。

---

## 3. 比較

| | A: CRuby API 互換 | B: 独自 C ABI 拡張 | C: システム lib dlopen | D: feature 分割 | G: Fiddle 上の Ruby |
|---|---|---|---|---|---|
| 同梱 C ソースが消える | ◎（gem が持つ） | ◎（拡張 crate へ） | ◎ | ×（feature 内に残る） | ◎ |
| コアバイナリが縮む | ◎ | ◎ | ○（glue は残る） | ○（無効化時のみ） | ◎ |
| 既存 gem がそのまま動く | ◎ | ×（移植要） | × | × | × |
| 出力の CRuby 一致 | ◎ | ◎（同梱時） | △（版依存） | ◎ | ◎ |
| ホットな用途の速度 | ○（CRuby 並） | ◎（今と同じ） | ◎ | ◎ | ×（Fiddle 35 %） |
| JIT インライナ | × | ×（コア primitive は可） | ◎ | ◎ | ◎（`___read` 等） |
| 例外モデルの追加作業 | 大（setjmp 境界） | 小（現行規約のまま） | なし | なし | なし |
| 作業量 | 大（数百シンボル + 例外 / GVL モデル） | 中（ABI 設計 + 5 拡張の分離） | 小〜中 | 小 | 小 |
| 他 gem への波及 | 大 | 中（新規は書きやすい） | なし | なし | なし |

---

## 4. 推奨: B を土台に段階的に、A はその上に

**B を土台にして段階的に進め、A は必要になったらその上に積む。** 旧稿の
Path C と同じ結論だが、順序を「まず既存の同梱物を外へ出す」に変える。
理由:

- B の機構（dlopen / `Init_*` / vtable / pin / ブロッキング申告 / コール
  バック再入）は A を作るときにも**丸ごと下層として必要**で、無駄にならない。
- 逆に A から始めると、最初の実 gem が通るまでの間、抱えている問題は
  何も減らない。
- B の作業は既に書いてある 11k 行を**ABI の向こうへ動かす**ことが主で、
  新しい振る舞いをほとんど含まない。出力比較テスト
  （`tests/{nokogiri,sqlite3,zstd,…}.rs`）がそのまま回帰テストになる。

### 4.1 順序

1. **D を先に入れる**（数時間）。`nokogiri` / `sqlite3` / `zstd` / `psych` /
   `zlib` を feature にして、`builtins.rs` の登録が feature で切れる形に
   する。依存が一方向であることをコンパイラに確認させる。
2. **`mr_*` C ABI を定義し、`monoruby-ext-sys` を切り出す。** 載せるのは
   今の 5 拡張が実際に使っている面だけ（§4.2）。`MrContext` の先頭に
   `abi_version: u32`。
3. **既存 5 拡張を cdylib crate に分離**し、`build.rs` が
   `~/.monoruby/v<ver>/ext/<name>.so` に置く。`require "nokogiri/nokogiri"`
   は stub ツリー解決（`require.rs:263`）の手前で `ext/` を探す分岐を足す
   だけ。**sqlite3 から始める**: 2.2k 行で自己完結し、`create_function` の
   コールバック再入と `NativeOp` オフロードの両方を含むので ABI の試金石に
   なる。次に zlib / zstd（handle table 型で最も単純）、psych（イベント
   コールバック）、最後に nokogiri（`NativeData` の寿命モデルが最も込み入る）。
4. **C を拡張ごとの feature にする**（`bundled` / `system`）。既定は
   `bundled` のままで出力一致を守り、ディストリ向けに `system` を選べる。
5. その後、必要なら **A** を `mr_*` の上に「使われる順」で積む。
   `rb_define_method` → `mr_define_method`、`rb_raise` → setjmp を切った
   境界で `mr_set_error`、`TypedData_*` → `mr_native_*`、という薄い写像で
   始められる。

### 4.2 ABI に載せるもの（5 拡張の実使用から）

| 区分 | 関数（案） | 今の対応物 |
|---|---|---|
| 登録 | `mr_define_class(ctx, name, super, instance_ty)`, `mr_define_module`, `mr_define_method(ctx, class, name, fn, min, max, rest, kw[])`, `mr_define_singleton_method` | `define_class_with_instance_ty`, `define_builtin_func_with_kw` |
| 値の生成・取り出し | `mr_str_new(ctx, ptr, len)`, `mr_str_ptr_len`, `mr_int`, `mr_float`, `mr_sym`, `mr_ary_new` / `push` / `len` / `aref`, `mr_hash_new` / `aset` / `aref`, `mr_nil` / `true` / `false` | `Value::string_from_*`, `as_bytes`, `Value::integer` … |
| ネイティブ資源 | `mr_native_new(ctx, class, data, vtable{mark, drop, dup})`, `mr_native_get(obj) -> data` | `ObjTy::NATIVE`, `NativeData` |
| GC | `mr_gc_mark(ctx, value)`（mark コールバック内でのみ有効）, `mr_gc_pin` / `unpin`（永続ルート） | `Allocator::mark`, `GCRoot` |
| 呼び出し・例外 | `mr_funcall(ctx, recv, name, argc, argv, block)`, `mr_yield`, `mr_set_error(ctx, class, msg)`, `mr_error_pending(ctx)` | `vm.invoke_method`, `set_error` |
| スレッド | `mr_call_blocking(ctx, fn, arg)` | `NativeOp::Ffi` / `Sqlite` |
| 文脈 | `mr_abi_version(ctx)`, `mr_ruby_version(ctx)` | — |

`Lfp` / `BytecodePtr` / `IdentId` / `ClassId` の内部表現は出さない
（名前は `const char*`、クラスは不透明 handle）。

### 4.3 決めておくべき不変条件

1. 拡張は `monoruby` crate をリンクしない（Rust の型を一切晒さない C ABI）。
2. C / Rust 境界を unwind は跨がない。例外は `mr_set_error` + 戻り値。
3. 拡張が保持する `Value` は必ず `mark` コールバックか pin で GC に見せる
   （C メモリに隠したものは collector から見えない — sqlite3 の
   `sqlite3_aggregate_context` の slot 番号方式が前例）。
4. カーネルでブロックしうる呼び出しは `mr_call_blocking` で申告する。
5. `Init_*` はロード時に一度だけ呼ばれ、以後クラス定義を変えない
   （JIT の class version を不用意に動かさない）。
6. ABI の版は `MrContext.abi_version` で判定し、不一致は `LoadError`。
   同梱拡張は per-version install root で自然に一致する。

---

## 5. 判断が必要な点

- **`ext/` の配布形態**: `cargo install` 時に `build.rs` がビルドして
  install root に置く（今の stub と同じ体験）か、別パッケージにするか。
  前者から始めるのが自然。
- **`system` を既定にするライブラリ**: zlib / zstd は OS 版で出力が
  一致するので `system` 既定でよい可能性がある。libxml2 は nokogiri の
  パッチがあるので `bundled` 既定。
- **A に進むか**: pg / bcrypt / markly など「Rust で書き直す価値が薄く、
  数が多い」gem が本当に必要になった時点で判断する。それまでは B + G
  （ホット: 独自拡張、コールド: Fiddle 上の Ruby）で足りる。

## 6. 実装状況（2026-09）

| 段階 | 状態 | 場所 |
|---|---|---|
| 1. feature 分割 | 済（役目を終えて撤去） | 5 つを default-on の feature にし、off で builtin が消え `build.rs` がその stand-in を install しない形にした。3 で全部が拡張に出たので、feature も `build.rs` の gating も CI の `--no-default-features` チェックも取り除いた。 |
| 2. C ABI | 済 | `monoruby_ext_sys/`（`MrValue` / `MrContext` / `MrApi`、`include/monoruby_ext.h`）、`monoruby/src/ext.rs`（表の実装、trampoline、`ExtNative`、loader）、`monoruby_ext/`（Rust 向け安全ラッパ: `Ctx` / `Value` / `method!` / `native!`）。`tests/native_ext.rs` が C で書いた拡張をヘッダから `cc` でビルドして全項目を通す。 |
| 3. sqlite3 の分離 | 済 | `ext/sqlite3/`（crate `sqlite3_native`、cdylib）。`gem/sqlite3/sqlite3_native.rb` が `require "sqlite3_native.so"` する。`tests/sqlite3.rs` の 23 本は無変更で通る。コアから `src/builtins/sqlite3.rs`（2.2k 行）と `libsqlite3-src` 依存が消えた。 |
| 3. zlib / zstd の分離 | 済 | `ext/zlib/`（`zlib_native`、checksum と `__zstream_*`）、`ext/zstd/`（`zstd_native`）。`stdlib/zlib.rb` / `gem/zstd-ruby/zstdruby.rb` が `require "…_native.so"` する。コアから `libz-sys` / `zstd-safe` が消えた。rubygems が `zlib` を要るので、インストール時は `bin/install` が 3 拡張を `<install root>/ext/` に置く。 |
| 3. psych の分離 | 済 | `ext/psych/`（`psych_native`、`__yaml_parse` は `Psych::Handler` を `funcall` で駆動）。`gem/psych/psych.rb` が `require "psych_native.so"` する。コアから `libyaml-safer` が消えた。 |
| 3. nokogiri の分離 | 済 | `ext/nokogiri/`（`nokogiri_native`、12 ファイル ~7k 行）。`gem/nokogiri/nokogiri.rb` が `require "nokogiri_native.so"` する。コアから `src/builtins/nokogiri/` と `libxml2-src` 依存が消え、`ObjTy::NATIVE` を使う builtin はコア側に無くなった（`ext.rs` の `ExtNative` だけ）。`tests/nokogiri.rs` の 23 本（CRuby の gem と出力比較）はスクリプト無変更。 |
| 4. `bundled` / `system` feature | 未 | |
| 5. CRuby API 互換層 | 未 | |

### 6.1 実際の ABI（§4.2 との差）

§4.2 の案から変わった点:

- **`MrContext` は呼び出しごと**（`api` と VM 側の不透明ポインタ）。`Init_<name>(MrContext*)` も同じ形。C コールバック（sqlite の `xFunc`）からは、その呼び出しの `ctx` をそのまま使う（park を跨いでも有効）。
- **クラスは `MrValue`**（クラスオブジェクトそのもの）。別の handle 型は作らなかった。
- **alloc 関数は ABI に無い。** `MR_CLASS_NATIVE` のクラスは core 側の汎用 alloc がペイロード無しの instance を作り、拡張の `initialize` / `open` が `native_set` で埋める。`native_data(obj, ops)` は `ops` の一致で種類を判定する（`MrNativeOps` のアドレスが型の identity）。
- **メソッドは `fn(ctx, self, argc, argv, block) -> MrValue`**、`argc` は固定数か `MR_ARGC_VARIADIC`。`Lfp` / `BytecodePtr` は出さない。`MR_UNDEF`（= 0、`Value` の niche）が「エラー保留」。
- **例外の stash は exception object で**: `error_take` が保留中のエラーを exception object として取り出し、`raise_exception` で戻す（C フレームを跨ぐ間は temp stack に載せる）。
- `str_encoding`（エンコーディング名）を追加。sqlite3 の BLOB 判定に要った。
- `mr_call_blocking` は `fn(void*) -> i64` + 引数ポインタ。Rust ラッパでは closure。

### 6.2 分かったこと

- **拡張の `static` はプロセス共通、`Init_` はインタプリタごと。** テストハーネスは 1 プロセスに多数の `Globals` を作るので、拡張が定義したクラスを `static` に持つと 2 つ目のインタプリタで壊れる。`Ctx::interpreter_id`（globals ポインタ）で key する。sqlite3 は thread-local + id 照合にした。
- **`require "x/4.0/x_native"` は `.rb` stand-in に届かなければならない。** ext の探索は bare な `require "x_native.so"` だけに限定した（gem の nested `.so` 名で ext に飛ぶと stand-in が持つ Ruby 側の定義を飛ばす）。
- **テストから拡張をビルドするときは別の target dir**（`target/ext/`）。外側の `cargo test` が target dir のロックを持ったままテストを走らせるので、同じ dir への nested `cargo build` は待ち続ける。aarch64 の qemu 実行では `--target` を明示する必要がある。
- **alloc 関数が無い代わりの「遅延 payload」。** nokogiri の `SAX::Parser` / `SAX::PushParser` / `NodeSet` はコア側で alloc 関数を持ち、`new` の時点でペイロードを作っていた。拡張では汎用 alloc がペイロード無しの instance を作るので、最初にペイロードへ触るアクセサ（`handler_ptr` / `install_push_ctxt` / `set_ptr`）が `is_kind_of` で確かめて `native_set` で埋める。`Object#dup` のコピーも同じ経路で埋まる。
- **variadic の引数は平らに渡る。** コアの rest builtin は `lfp.arg(0)` が rest 配列だったが、trampoline は `MR_ARGC_VARIADIC` の `argv` に要素を展開する。移植で `ary_vec(args[0])` を残すと最初の引数を配列として読んで壊れる（nokogiri で 6 箇所）。
- **C パーサの出力が入力バッファを指す場合は `str_bytes` の生ポインタを使う。** gumbo の error record は入力の中を指す。`str_vec`（コピー）だと `add_errors` が別のメモリに対して診断を描くので、`Ctx::str_bytes` の `(ptr, len)` を取り出して渡す（collector は動かさないので String が生きている限り有効）。
- 配布: `cargo build`（workspace root）で `.so` がバイナリの隣にできる。`cargo install` はバイナリしか置かないので、`bin/install` が拡張をビルドして `<install root>/ext/` にコピーする（`bin/spec` もこれを使う）。`bin/test` / `bin/test-aarch64` はベンチマーク用バイナリの隣に拡張をビルドする。

## 7. 参考

- `doc/c_extention.md` — CRuby C API 互換層の設計検討（Path A / B / C、
  TruffleRuby の事例、レイアウト不一致の吸収、`gem install` のフロー）
- `doc/nokogiri.md` §2, §5 — nokogiri での選択肢比較と `NativeData` の位置づけ
- `doc/threads.md` §9–10 — native offload（`NativeOp`）と残る制約
- `doc/activerecord_liquid_il_rack_investigation_2026-09.md` §3.1 — Fiddle
  経由 sqlite3 の 35 % と native 化後の数字
