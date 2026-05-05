# monoruby における Ruby C 拡張サポートの設計検討

本ドキュメントは、monoruby で CRuby の C 拡張 (`.so`) をロード・実行する仕組みを導入する際の検討内容をまとめたものである。

---

## 1. monoruby 側の現状調査

### 1.1 既存インフラ（活用できるもの）

- **`Value` の C ABI 互換性**: `Value` は `#[repr(transparent)] struct Value(NonZeroU64)` で定義されており (`monoruby/src/value.rs:148`)、C ABI 上は単なる `u64` として渡せる。
- **ビルトイン関数の安定 ABI**: `extern "C" fn(&mut Executor, &mut Globals, Lfp, BytecodePtr) -> Option<Value>` という固定シグネチャ (`monoruby/src/executor.rs:16`)。
- **`libc::dlopen` / `dlsym` の利用例**: Fiddle/FFI 用に既に実装済み (`monoruby/src/builtins/kernel.rs:1628-1692`)。`libloading` クレートは使わず、`libc` の生 API を直接呼ぶパターン。
- **`require.rs` は `.so` を認識**: ただし現状は `~/.monoruby/lib/` 内の `.rb` スタブにリダイレクトするのみで、ダイナミックロードは行わない (`monoruby/src/globals/require.rs:157-171`)。

### 1.2 不足しているもの

- CRuby C API 互換シム（`rb_define_method`, `rb_funcall`, `VALUE`, `ID` 等）は **一切存在しない**。
- 外部 C コードが保持する `Value` を GC ルートとして登録する仕組みも未整備。`GCRoot` は `Globals`/`Executor` ツリーのみを辿る (`monoruby/src/alloc.rs:39-47`)。

---

## 2. 設計方針：3 つの選択肢

### Path A — CRuby C API 完全互換層

`ruby.h` 互換ヘッダと `libruby.so` 相当のシムを書き、`nokogiri` 等の既存 gem を無改造で動かす。TruffleRuby/Artichoke 系のアプローチ。

- **長所**: gem エコシステムが手に入る。
- **短所**: API 面積が膨大（数百シンボル）。`Value` のタグレイアウトが CRuby `VALUE` と異なる（`Qnil=8 vs 0x04`、Fixnum タグ位置等）ため二値互換は不可能で、全境界に変換が要る。`rb_protect`/`setjmp` モデルや GVL も必要。

### Path B — monoruby ネイティブ拡張 API

`mr_*` プレフィックスの新 API を定義し、新規に C 拡張を書く人向けにする。

- **長所**: 小さく完結する。`Value` をそのまま `MrValue = uint64_t` として晒せる。
- **短所**: 既存 gem は移植が必要。

### Path C — B を土台に A を段階的に積む（推奨）

まず B の機構（`.so` ロード / `Init_xxx` 呼び出し / GC ピン留め / 登録 API）を作り、その上に CRuby 互換シム関数を「よく使われるものから順に」薄く積み上げる。

### Path B の具体ステップ（最小ゴール：hello-world `.so`）

1. **`require.rs`** — `search_load_path` で `.so` を見つけたら、現行の `.rb` スタブ置換ではなく新関数 `load_native_extension(path)` へ分岐。
2. **dlopen** — `kernel.rs` の既存パターンを再利用して `.so` をロード。
3. **`Init_<basename>` を dlsym** して `extern "C" fn(*mut MrContext)` として呼ぶ。
4. **`monoruby_ext.h`** を新設し、以下を C 側に export：
   - `typedef uint64_t MrValue;`
   - `MrClassId mr_define_class(MrContext*, const char* name, MrClassId super);`
   - `void mr_define_method(MrContext*, MrClassId, const char* name, MrBuiltinFn fn, int arity);`
   - `MrValue mr_str_new(MrContext*, const char*, size_t);` / `mr_int_value` / `mr_funcall` / `mr_gc_register`
5. **GC ルート登録** — `alloc.rs` の `GCRoot` ツリーに「外部 C 側から保持中の Value」コンテナを追加（`Globals` に `Vec<Value>` の固定根として）。
6. **ABI バージョン** — `Init_xxx` には `MrContext` の先頭に `abi_version: u32` を入れて将来拡張可能に。

### 主要な落とし穴

- **`Value` の bit layout は CRuby と非互換** — Path A を将来やる場合、境界での変換テーブルが必須。
- **例外伝播** — `rb_raise` 互換は `setjmp/longjmp` か Rust の `panic=unwind` を経由する必要があり、JIT のフレームを巻き戻せるか要検証。
- **`Lfp` / `BytecodePtr`** — C 側へ晒すと ABI が固定化されて VM リファクタを縛るので、`MrContext` で抽象化するのが安全。
- **mkmf** — `extconf.rb` が CRuby の `ruby.h` を見つけにいく前提なので、monoruby 専用の `mkmf` 置換 or `ruby.h` 互換ヘッダ生成が必要。
- **スレッド/Fiber** — `rb_thread_create` 等は monoruby が単一ネイティブスレッド前提なので、Path A でも対応外にする方が現実的。

---

## 3. TruffleRuby のアプローチ（参考事例）

### 3.1 Sulong 時代（過去の主流）

C 拡張を **LLVM ビットコード** にコンパイルし、GraalVM 内蔵の LLVM インタプリタ（Sulong）が同一 JVM プロセス内で実行。本来不透明な「ネイティブコード内での VALUE 操作」を Truffle/Graal 側からインターセプトできた。

### 3.2 VALUE の表現

CRuby の `VALUE` (`unsigned long`) を直接受け取らず、**`ValueWrapper`** という Java/Truffle 側のラッパーオブジェクトと、ネイティブ整数ハンドルの二形態を行き来させる：

- C 境界に渡す前に **wrap**、Ruby に戻すときに **unwrap**。
- `nil`/`true`/`false`/小整数/即値はインライン化（特別ケース）。
- それ以外は **4096 エントリ単位の handle block** に **WeakReference** で格納し、`RubyFiber` が `HandleBlockHolder` で管理。

つまり CRuby の VALUE と二値互換にはせず、**境界で必ず変換する** 設計を選んでいる。

### 3.3 C API の実装場所

`rb_define_method` 等の数百個ある関数は **C ではなく Ruby (および Java/Truffle ノード) で実装** されている：

> *"Most API functions are defined in the C header file, the C implementation file, and then either implemented as a call to a method... using polyglot_invoke to do a foreign call from C into Ruby, or we implement the function in Ruby in the `Truffle::CExt` module."*

C 側には薄いシムだけ置き、本体は Ruby 側 `Truffle::CExt` モジュールにある。互換 API の実装コストを劇的に下げる戦略。

### 3.4 GC 統合

TruffleRuby の GC は VALUE を理解しないため、3 層のレシピで生存性を担保：

1. **`ExtensionCallStack`** — C 呼び出しごとに `preservedObjects` リストを積み、その呼び出し中に作られた ValueWrapper を強参照で pin。
2. **Handle blocks** は弱参照だが、**ValueWrapper 側がブロックを強参照** することで、wrap が生きている限りブロックも生きる。
3. **`DATA_PTR` 経由の mark 関数** を記録しておき、C 呼び出し終了時に実行して可達オブジェクトを集める。

### 3.5 最近の方針転換：Sulong からネイティブ実行へ

近年、**Sulong 経由をやめてシステムツールチェイン（gcc/clang）でビルド・ネイティブ実行する方式に移行**している：

> *"C/C++ extensions are now compiled using the system toolchain and executed natively instead of using GraalVM LLVM (Sulong), which leads to faster startup, no warmup, better compatibility, smaller distribution and faster installation."*

LLVM 経由の透過性メリットより、起動時間・warmup・互換性・配布サイズの実利が勝った。

### 3.6 monoruby への示唆

1. **Bit 互換は捨てる** — TruffleRuby ほど高度な仕組みを使っても VALUE は wrap/unwrap が必須だった。
2. **C API の大半を Ruby/Rust 側で実装する戦略が有効** — TruffleRuby 式に「C シムは数十行、本体は `Monoruby::CExt` モジュール」にすれば互換シム数百個の重みが下がる。
3. **GC は per-call pin stack 方式** — C 呼び出しスコープごとに `Vec<Value>` を一つ push し、その間に作られた外部参照可能な Value を pin、呼び出し終了で pop。
4. **LLVM ビットコード解釈は不要** — TruffleRuby ですら捨てた手段なので、monoruby は最初から `dlopen`/`dlsym` のネイティブ実行で良い。

---

## 4. オブジェクトレイアウト不一致の吸収方法

CRuby C 拡張は `RSTRING_PTR`/`RARRAY_PTR`/`RBASIC`/`RDATA`/`RTYPEDDATA` 等のマクロを多用し、その多くは CRuby のメモリレイアウトを前提としている。これをどう吸収するか。

### 4.1 TruffleRuby の 3 層対策

#### ① Sulong 時代：LLVM load 命令そのものをインターセプト

> *"in our C interpreter rather than the `struct` field reads just being a load from an address in memory, we can instead insert any logic we want."*

C ソースを `gcc -emit-llvm` でビットコードにし、Sulong が解釈実行。`obj->len` のフィールドアクセスは LLVM ビットコード上では `getelementptr + load` 命令だが、**その load 命令自体を Ruby メソッド呼び出しに差し替える**。インラインキャッシュも入る。

#### ② C ソースのプリプロセッサ・パイプライン

`gcc`/`clang` に渡す前に **C ソースを書き換える**。`ruby.h` 互換ヘッダの再定義と組み合わせて、問題のあるマクロを関数呼び出しに変換する。ネイティブ実行モードに移行した今、**主たる手段**になっている。

#### ③ 個別マクロごとの吸収戦略

| マクロ | TruffleRuby での扱い |
|---|---|
| `RSTRING_PTR(s)` | 初回呼び出しで「rope」表現に**永久変換**してネイティブメモリに固定。以降そのポインタを返す。 |
| `RARRAY_PTR(a)` | `VALUE*` に見える**プロキシオブジェクト**を返す。読み書き・ポインタ算術は配列＋オフセットを内部的に追跡。 |
| `RDATA(obj)->data = ptr;` | プロキシポインタ。`data` フィールドへの代入を**インスタンス変数への書き込みに転送**。 |
| `RTYPEDDATA` | TypedData とデータ構造を**常に同一アロケーションに置く**（embedded TypedData 相当）。 |
| `RBASIC->flags`/`->klass` | フィールドアクセスを intercept → Ruby 側の状態/クラスにマップ。 |
| `RB_TYPE_P` 等の判定マクロ | ヘッダ側で関数呼び出しに再定義。 |

### 4.2 monoruby への適用案

monoruby はネイティブ実行しか取らない以上、**Sulong 式の load 命令インターセプトは使えない**。TruffleRuby が現在採っている②③の組み合わせが現実的：

#### A. `ruby.h` 互換ヘッダで全マクロを関数化

```c
// monoruby が提供する ruby.h
#define RSTRING_PTR(s)   mr_rstring_ptr((MrValue)(s))
#define RSTRING_LEN(s)   mr_rstring_len((MrValue)(s))
#define RARRAY_LEN(a)    mr_rarray_len((MrValue)(a))
#define RARRAY_AREF(a,i) mr_rarray_aref((MrValue)(a),(i))
#define RB_TYPE_P(o,t)   mr_type_p((MrValue)(o),(t))
#define NIL_P(o)         ((o) == Qnil)
```

CRuby 自身も近年マクロ群の多くを `static inline` 関数化しており、**ABI ではなく API 互換**で十分動く拡張は多い。

#### B. lazy materialization（必要時にネイティブ化）

- `RSTRING_PTR(s)`: 初回呼び出しで `RValue::String` のバイト列をネイティブ固定アロケータにコピー＆pin。`Globals` の pin テーブルに登録。以降は同じポインタを返す。書き込み後は `rb_str_modify` 呼出を要求する CRuby 慣習に乗る。
- `RARRAY_PTR(a)`: 同様だが、書き込みのある場所では使わない方針を貫く（CRuby でも `rb_ary_store(a, i, v)` 推奨）。読み専用なら lazy ネイティブコピー。

#### C. `RTYPEDDATA` は co-allocation 設計

`TypedData_Make_Struct` を呼ぶと、monoruby の `RValue` ヘッダ + ユーザデータ構造を**同一ネイティブアロケーション**として確保。`((struct RTypedData*)obj)->data` への直接アクセスが成立するよう、レイアウトを CRuby 互換にする（GC も普通に mark 関数を呼ぶだけ）。

#### D. 諦めるべきマクロ

- `RARRAY_PTR(a)[i] = v` の **lvalue 書き込み** — TruffleRuby はプロキシで吸収したがネイティブ実行では不可能。これに依存する gem は未対応とし、porting 時に `rb_ary_store` への置換を要求。
- `RHASH` の内部構造直接アクセス — CRuby 自身が 3.0 頃から非推奨化済み。
- `RSTRUCT_PTR` 等の lvalue 系も同様。

### 4.3 まとめ

「マクロをどう吸収するか」の答えは **3 層**：

1. **ABI 互換は諦める**（TruffleRuby も諦めた）。
2. **API 互換ヘッダ + マクロを関数呼び出しに再定義**（最も効くてこ）。
3. **書き換え不能なポインタ寿命保証は lazy native materialize + pin、書き込み系は co-allocation で物理レイアウトを合わせる**。

---

## 5. 再コンパイルの必要性

### 5.1 結論：再コンパイルは必須

CRuby でビルド済みの `.so` をそのまま monoruby にロードすることは原理的に不可能：

| 不一致点 | 影響 |
|---|---|
| **`Value` のビットレイアウト** | `Qnil = 0x04 vs 0x08`、Fixnum タグ位置等が違う。`.so` 内に埋め込まれた即値定数がそのまま意味を持たない。 |
| **エクスポートシンボル名** | `.so` は `rb_define_method`、`rb_str_new` 等を `dlopen` 時にリンク解決する。monoruby はこれらを自前のシムで実装するので、**シンボルテーブル自体は同名で提供できる**が、ABI が合わなければ即クラッシュ。 |
| **マクロ展開結果** | `RSTRING_PTR(s)` を CRuby ヘッダで展開するか monoruby ヘッダで展開するかで生成コードが全く違う。これは**ヘッダ依存**＝**ビルド時依存**。 |
| **構造体オフセット** | `((RBasic*)obj)->klass` を生コード上のオフセットとしてアクセスされた場合、monoruby の RValue 配置と合わない限り破綻。 |

### 5.2 ただし「再コンパイル」は実用上ほぼ自動化される

これは TruffleRuby・JRuby・Artichoke でも同じ前提で、ユーザ体験としては **`gem install nokogiri` するだけ** で済むよう作る：

1. **monoruby 用 `mkmf` を提供** — `extconf.rb` が `require "mkmf"` したときに、monoruby の include パス（`~/.monoruby/include/ruby.h`）と link フラグを返す。
2. **`gem install` のフロー**:
   ```
   $ monoruby -S gem install nokogiri
   → extconf.rb 実行（monoruby の mkmf）
   → Makefile 生成（-I ~/.monoruby/include, -lmonoruby_ext 等）
   → gcc/clang でビルド → nokogiri.so が monoruby ヘッダで再生成される
   → ~/.monoruby/gems/... に配置
   ```
3. **ユーザは何も意識しない** — 「初回 gem install でビルドが走る」のは CRuby でも同じ体験。

つまり「再コンパイル必須」は **ABI 互換を諦める代わりに API 互換を取る** という現実解で、ユーザの心理的負担はほぼゼロ。

---

## 6. `gem install` での C 拡張コンパイル詳細

### 6.1 全体フロー

```
gem install nokogiri
  │
  ▼
① Gem::Installer            # gemspec読込、依存解決、ファイル展開
  │
  ▼
② Gem::Ext::Builder.build_extensions   # gemspecの extensions 配列を順次処理
  │
  ▼ extconf.rb がある場合
③ Gem::Ext::ExtConfBuilder.build
  │  ├─ tmpdir 作成
  │  ├─ ruby extconf.rb [build_args]    # ← ここで Makefile 生成
  │  ├─ make DESTDIR=...
  │  ├─ make install DESTDIR=...
  │  └─ make clean & tmpdir削除
  │
  ▼
④ .so を gemの lib/ と extensions ディレクトリへ配置
  │
  ▼
⑤ Gem::Specification 登録 → require できる状態に
```

`Gem::Ext::Builder` は他に `RakeBuilder`（`Rakefile`）/`ConfigureBuilder`（`configure`）/`CmakeBuilder`（`CMakeLists.txt`）も持つ。

### 6.2 extconf.rb の中身

典型例：

```ruby
require "mkmf"

# システム探査
have_header("zlib.h")               or abort "zlib.h not found"
have_library("z", "deflate")        or abort "libz not found"
have_func("strlcpy", "string.h")
find_executable("xml2-config")

# プリプロセッサ定数生成
$CFLAGS  << " -Wall -O2"
$LDFLAGS << " -lpthread"

create_header                       # extconf.h を生成
create_makefile("nokogiri/nokogiri") # ← Makefile を生成
```

`have_header`/`have_library`/`have_func` は **実際にテスト用の小さな C ソースをコンパイル＆リンクして判定** している（`try_compile`/`try_link`）。

### 6.3 mkmf が参照する RbConfig キー

**これが monoruby 移植の核**：

| キー | 用途 | 例 |
|---|---|---|
| `rubyhdrdir` | `ruby.h` の場所 | `/usr/include/ruby-3.4.0` |
| `rubyarchhdrdir` | アーキ別ヘッダ | `/usr/include/ruby-3.4.0/x86_64-linux` |
| `archdir` | 標準ライブラリ `.so` の場所 | `/usr/lib/ruby/3.4.0/x86_64-linux` |
| `sitearchdir` | サイト拡張 `.so` の配置先 | `/usr/local/lib/ruby/site_ruby/3.4.0/x86_64-linux` |
| `vendorarchdir` | ベンダ拡張 `.so` の配置先 | （配布パッケージ用） |
| `CC` | C コンパイラ | `gcc` |
| `CXX` | C++ コンパイラ | `g++` |
| `CFLAGS`/`CPPFLAGS` | コンパイル時フラグ | `-O3 -fPIC ...` |
| `LDSHARED` | 共有ライブラリのリンクコマンド | `gcc -shared` |
| `DLDFLAGS` | リンク時フラグ | `-Wl,--no-undefined` |
| `LIBRUBYARG` | Ruby ランタイムのリンク引数 | `-lruby` |
| `DLEXT` | 共有ライブラリ拡張子 | `so`/`bundle`/`dll` |
| `arch` | プラットフォーム識別子 | `x86_64-linux` |
| `ruby_version` | ABI バージョン | `3.4.0` |
| `target_os`/`target_cpu` | クロスコンパイル制御 | |

### 6.4 生成される Makefile の典型形

```makefile
SHELL = /bin/sh
RUBYARCHDIR = $(sitearchdir)$(target_prefix)/nokogiri
RUBYHDRDIR  = /usr/include/ruby-3.4.0
arch_hdrdir = /usr/include/ruby-3.4.0/x86_64-linux

CC      = gcc
LDSHARED = gcc -shared
CFLAGS  = -fPIC -O3 -Wall ...
INCFLAGS = -I. -I$(arch_hdrdir) -I$(RUBYHDRDIR) -I$(srcdir)
DLDFLAGS = -Wl,--no-undefined
DLEXT    = so
TARGET   = nokogiri
DLLIB    = $(TARGET).$(DLEXT)

OBJS = nokogiri.o xml_node.o ...

$(DLLIB): $(OBJS)
	$(LDSHARED) -o $@ $(OBJS) $(LIBPATH) $(DLDFLAGS) $(LIBS)

install: $(DLLIB)
	$(INSTALL_PROG) $(DLLIB) $(RUBYARCHDIR)
```

ポイント：
- **`-I$(RUBYHDRDIR)`** で `ruby.h` を取りに行く（=monoruby ではここが我々のヘッダ群を指す必要あり）。
- **`LDSHARED`** で `.so` を作る。多くの環境では `libruby` への動的リンクは**しない**。Ruby シンボルは**実行時にメインプロセスが提供する**前提。
- **インストール先**は `$(RUBYARCHDIR) = $(sitearchdir)/<gem-name>` 配下。

### 6.5 ビルド成果物の配置

RubyGems は `.so` を **2 か所**に配置する：

```
~/.gem/ruby/3.4.0/
  ├── gems/nokogiri-1.16.0/lib/nokogiri/nokogiri.so   # require先
  └── extensions/x86_64-linux/3.4.0/nokogiri-1.16.0/  # ビルド成果保管
       ├── nokogiri.so
       ├── gem_make.out                                # ビルドログ
       └── mkmf.log                                    # mkmfの探査ログ
```

`extensions/<arch>/<abi>/<gem>` の階層で**プラットフォーム＆Ruby ABI ごとに別管理**される。

### 6.6 require できるようになる仕組み

1. `Gem::Specification` が gem の `lib/` を `$LOAD_PATH` に追加。
2. `require "nokogiri"` で `nokogiri.rb` をロード。
3. `nokogiri.rb` 内部で `require "nokogiri/nokogiri"` → `.so` をロード。
4. `dlopen` 後、`Init_nokogiri()` が呼ばれて `rb_define_class` 等で世界に登録。

### 6.7 失敗時の挙動とログ

- `mkmf.log` に **試行コンパイルの全コマンドと出力** が残る。
- `gem_make.out` に make の標準出力/エラー全文。
- `Gem::Ext::ExtConfBuilder` は Cause exception を投げ、エラーメッセージにこれらのパスを含める。

### 6.8 主要ファイル（RubyGems 側）

| パス | 役割 |
|---|---|
| `lib/rubygems/installer.rb` | `Gem::Installer` — gem 展開、ext 呼び出し |
| `lib/rubygems/ext/builder.rb` | `Gem::Ext::Builder` — 種別判定とディスパッチ |
| `lib/rubygems/ext/ext_conf_builder.rb` | extconf.rb 方式の実行 |
| `lib/rubygems/ext/rake_builder.rb` 等 | 別方式（Rake/configure/cmake） |
| `lib/mkmf.rb` | Ruby 本体に同梱、`MakeMakefile` モジュール |

---

## 7. monoruby に持ち込むときの押さえどころ

「再コンパイル必須」を前提にすると、**monoruby 側でやることは以下 5 点**に絞れる：

### 7.1 `RbConfig::CONFIG` を提供

monoruby では `RbConfig::CONFIG` ハッシュを上書きまたは新規定義し、上記キーを **monoruby 用の値** に設定：

```ruby
RbConfig::CONFIG["rubyhdrdir"]    = "#{ENV['HOME']}/.monoruby/include"
RbConfig::CONFIG["sitearchdir"]   = "#{ENV['HOME']}/.monoruby/site/#{arch}"
RbConfig::CONFIG["LDSHARED"]      = "gcc -shared"
RbConfig::CONFIG["DLEXT"]         = "so"
RbConfig::CONFIG["arch"]          = "x86_64-linux-monoruby"   # ← 重要
RbConfig::CONFIG["ruby_version"]  = "monoruby-0.x"
RbConfig::CONFIG["LIBRUBYARG"]    = "-lmonoruby_ext"
RbConfig::CONFIG["CFLAGS"]        << " -DMONORUBY=1"
```

**`arch` を独自値にする** ことで `extensions/<arch>/...` が CRuby とぶつからない。

### 7.2 `~/.monoruby/include/` に互換 `ruby.h` を配置

マクロを関数化したヘッダ群を `build.rs` で配布。

### 7.3 `mkmf.rb` はそのまま使える可能性が高い

mkmf は Ruby で書かれており `RbConfig::CONFIG` を読むだけなので、 **CRuby 由来の mkmf.rb を `~/.monoruby/lib/` にそのまま置く** ことで動く可能性が高い。細部の `try_link` がリンカ呼び出しをするので、`LDSHARED`/`LIBRUBYARG` が正しく定義されていれば良い。

### 7.4 RubyGems も大部分流用可能

RubyGems も Ruby スクリプト群なので、**そのまま monoruby で実行できれば良い**。`Gem.dir`/`Gem.path` を monoruby 向けに上書きする小さなパッチで済む見込み。

### 7.5 シムライブラリ `libmonoruby_ext.so` を配布

`rb_define_method` 等のシンボルを export。`-lmonoruby_ext` で `.so` がリンクし、`dlopen` 時に monoruby プロセス側でこれらの実体を提供する（あるいは `--export-dynamic` で monoruby 本体が提供）。

---

## 8. 実装最小ライン

「`gem install` が monoruby でも動く」を最初のマイルストーンにするなら：

1. RubyGems を monoruby 上で起動できるところまで（`gem` コマンドが立ち上がる）。
2. `RbConfig::CONFIG` を monoruby 値で完備。
3. 互換 `ruby.h` を配布。
4. ダミーシム `libmonoruby_ext.so`（`rb_define_method` を 1 個だけ実装）。
5. **hello-world の C 拡張 gem** を作って `gem install ./hello.gem` が通ることを確認。

ここから順に実装する `rb_*` シンボル数を増やしていけば、徐々に実 gem が通るようになる。TruffleRuby も事実上この道を辿った。

---

## 9. 参考リンク

- TruffleRuby cexts.md (contributor docs): https://github.com/oracle/truffleruby/blob/master/doc/contributor/cexts.md
- TruffleRuby cext-values.md (handle/GC management): https://github.com/oracle/truffleruby/blob/master/doc/contributor/cext-values.md
- Better support for C extensions in TruffleRuby (aardvark179): https://aardvark179.github.io/blog/capi.html/
- Very High Performance C Extensions For JRuby+Truffle (Chris Seaton): https://chrisseaton.com/truffleruby/cext/
- Ruby Objects as C Structs and Vice Versa (Chris Seaton): https://chrisseaton.com/truffleruby/structs/
- Issue #1772: Cannot load more than one byte from RSTRING_PTR: https://github.com/oracle/truffleruby/issues/1772
- Feature #21853: Make Embedded TypedData a public API: https://bugs.ruby-lang.org/issues/21853
- A Rubyist's Walk Along the C-side (Part 7): TypedData Objects: https://blog.peterzhu.ca/ruby-c-ext-part-7/
- Gems with Extensions - RubyGems Guides: https://guides.rubygems.org/gems-with-extensions/
- RubyGems Ext::ExtConfBuilder source: https://github.com/rubygems/rubygems/blob/master/lib/rubygems/ext/ext_conf_builder.rb
- ruby/lib/mkmf.rb (Ruby 本体): https://github.com/ruby/ruby/blob/master/lib/mkmf.rb
- MakeMakefile module documentation: https://www.rubydoc.info/stdlib/mkmf/MakeMakefile
- Hacking extconf.rb (Yorick Peterse): https://yorickpeterse.com/articles/hacking-extconf-rb/
- Don't be terrified of building native extensions (Pat Shaughnessy): https://patshaughnessy.net/2011/10/31/dont-be-terrified-of-building-native-extensions
- RbConfig module reference: https://docs.ruby-lang.org/en/3.0/RbConfig.html
