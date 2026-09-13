# mail / lee / grape が monoruby で遅い原因の調査（2026-09-13）

調査日 2026-09-13。対象コミットは `c1d65ddd`（ツリーは master `a39a7019` と同一）。
比較対象は rbenv でビルドした **CRuby 4.0.6**（`--yjit` / `--zjit` / 素のインタプリタ）。
ベンチ本体は [ruby/ruby-bench](https://github.com/ruby/ruby-bench) を `harness-warmup`
（`MAX_TIME=60`）で走らせ、後半半分の反復の中央値を使う。gem は grape 4.0.0 + rack 3.2.7、
mail 2.8.1、lee は同梱の純 Ruby 実装。

計測機は x86-64 / Linux 4 vCPU（コンテナ）。このカーネル用の linux-tools が無く `perf` は
使えないので、関数別の内訳は **callgrind の差分法**で採った（反復数だけを変えた 2 回の
プロファイルを引き算し、起動とウォームアップを相殺する。手順は §6）。

先行調査 [`yjit_bench_slow_investigation_2026-09.md`](yjit_bench_slow_investigation_2026-09.md)、
[`ruby_bench_low_cost_ideas_2026-09.md`](ruby_bench_low_cost_ideas_2026-09.md)、
[`sequel_mail_liquid_investigation_2026-09.md`](sequel_mail_liquid_investigation_2026-09.md)
の続き。mail は 3 本目で一度扱っているが、当時の主因（`send(*args)` や Fiddle）とは別の、
もっと大きい穴が見つかったので改めて書く。

---

## 1. 結論（先に要点）

### 1.1 現状の数字（この機械での実測、ms）

| ベンチ | monoruby | CRuby --yjit | CRuby --zjit | CRuby | monoruby --no-jit | 対 YJIT |
|---|---:|---:|---:|---:|---:|---:|
| mail | 164 | 98 | 140 | 154 | 233 | **0.60** |
| grape | 792 | 398 | 554 | 820 | 2245 | **0.50** |
| lee | 378 | 624 | 845 | 946 | 1665 | **1.65** |

ポータル（x86-64, `f11b379`, 2026-09-13）もほぼ同じ: mail 166/101、grape 844/470、
lee 465/724。**lee だけは YJIT に 1.5〜1.65 倍勝っている**ので、「遅い」に当たるのは
ポータルの ZJIT 列（ruby-head でビルドされた ZJIT: mail 108 / grape 408 / **lee 273**）と
比べたときだけである。手元の 4.0.6 リリース版 ZJIT は lee で 845 ms と遅く、
ruby-head の ZJIT がこの 3 本すべてで monoruby より速い、というのが正確な現状。

### 1.2 原因（1 行ずつ）

- **mail**: 実行命令の **51 %** が `Encoding.find` の中にある。登録エンコーディング
  **107 個の線形走査**で、候補ごとに `to_uppercase` + `replace` の **String を 2 個**
  作って比較している。1 通あたり **80 回**呼ばれ、引数は全部 `"UTF-8"`（§2）。
- **grape**: 単一のホットスポットは無い。命令数が YJIT の 1.35 倍、時間が 1.99 倍で、
  差の内訳は「広く薄い」。いちばん構造的なのは **malloc/free が YJIT の 7.7 倍**
  （8.3k vs 1.1k Ir/req）で、1 リクエストで **Ruby オブジェクト 40 個 + malloc 38 回**
  を払っている（§3）。
- **lee**: `Lee::Matrix#index` の境界チェック `(0...@height).include?(y)` が、
  1 反復で **Range を 377 万個**作り、Ruby で書かれた `include?` → `cover?` →
  `__cover_val_q` を経由して `Integer#<=>` を **2 回**呼ぶ。この 2 項目だけで
  1 反復の命令数の **31 %**（§4）。

### 1.3 この調査から入れた 2 つの修正

原因の裏取りを兼ねて §5 の B（`Encoding.find`）と A（Range の数値 fast path）を
実装した。いずれも挙動は変えていない（§2.3, §4.3）。

| ベンチ | 変更前 | B: `Encoding.find` の O(1) 経路 | A: Range 数値 fast path |
|---|---:|---:|---:|
| mail | 164 ms | **111 ms（−32 %）** | — |
| lee | 382 ms | — | **360 ms（−5.8 %）** |
| grape | 778 ms | — | **755 ms（−3.0 %）** |

mail は対 YJIT 0.60 → **0.89** になる。grape が A で縮むのは、Rack と Mustermann が
`Range#===` / `cover?` を経路判定に使うため。

Range の方は**再ビルドしながら交互に測った**値（lee: 382/382 → 363/357、
grape: 788/768 → 755/755）。`builtins/*.rb` は共有のインストールルート
（`~/.monoruby/v<version>/`）に置かれるので、バイナリだけ差し替えて A/B すると
同じ `range.rb` を 2 回測ることになる。

---

## 2. mail — `Encoding.find` が命令数の半分

1 通（`Mail.new(raw).to_s`）あたり **24.4 M 命令**。CRuby+YJIT は 13.0 M で、
**比 1.88 倍**。実時間の比 1.75 倍とほぼ一致するので、mail の差は
「命令を余計に実行している」だけで説明が付く（IPC の差ではない）。

### 2.1 内訳（Ir/通、callgrind 差分）

| バケット | monoruby | % | CRuby+YJIT | % | 比 |
|---|---:|---:|---:|---:|---:|
| その他（後述） | 8,188 k | 33.5 | 2,884 k | 22.2 | 2.84x |
| libc malloc/free | 4,950 k | 20.3 | 756 k | 5.8 | **6.55x** |
| String | 2,663 k | 10.9 | 2,699 k | 20.8 | 0.99x |
| GC + オブジェクト確保 | 2,376 k | 9.7 | 1,599 k | 12.3 | 1.49x |
| Regexp | 1,923 k | 7.9 | 1,894 k | 14.6 | 1.01x |
| JIT コード | 1,625 k | 6.7 | 1,364 k | 10.5 | 1.19x |
| メソッド/定数/gvar 探索 | 941 k | 3.9 | — | — | — |
| 合計 | **24,426 k** | 100 | **13,002 k** | 100 | **1.88x** |

個別関数の上位は `_int_free` 2,030 k、`<str>::replace` 1,907 k、`malloc` 1,623 k、
`RurubyAlloc::alloc` 1,315 k、`<str>::to_uppercase` 1,299 k、`free` 1,159 k、
`encoding::enc_find` 1,104 k、`match_at`（正規表現）945 k。

### 2.2 `Encoding.find` の形

`enc_find`（`src/builtins/encoding.rs`）は、まず**登録されている全 Encoding 定数を
線形に走査**して canonical 名の「正規化一致」を探す:

```rust
let norm = |s: &str| s.to_uppercase().replace(['-', '_'], "");
let want = norm(&name);
for cname in globals.store.get_constant_names(enc_class) {
    … get_constant_noautoload … get_ivar(_ENCODING) … .to_string()
    && norm(&es) == want    // 候補ごとに String を 2 個確保
}
```

callgrind の呼び出し数（1 通あたり）:

| 項目 | 回数/通 |
|---|---:|
| `Encoding.find` | **80** |
| `get_constant_names`（`Vec<IdentId>` を毎回確保） | 80 |
| `str::to_uppercase` | **8,560** |
| `str::replace` | **8,560** |
| `enc_find` の inclusive 命令数 | **12.38 M（全体の 50.7 %）** |

1 回の `find` が 107 候補を舐め、候補ごとに `to_string()` + `to_uppercase()` +
`replace()` で **3 回 malloc** する。つまり 1 通で 2 万回近い一時 String を
「UTF-8 かどうか」を判定するためだけに作っている。§2.1 の malloc/free 20 % と
「その他」の大半はこれ。

呼び出し元は mail 側の `Mail::Utilities.convert_to_encoding`（charset 文字列を
`Encoding` に直す）で、Ruby 側で 80 回呼ぶこと自体は CRuby でも同じ。CRuby の
`rb_enc_find` は別名表のハッシュ 1 回で O(1) なので差が出ない。

引数の実測（`Encoding.find` を Ruby でラップして数えた）:

```
80.0 calls/mail  ["String", "UTF-8"]
```

**全部 `"UTF-8"`**。最頻の名前が最悪ケースになっている。

### 2.3 検証

別名表 `enc_name_to_const`（`"UTF-8"` → `UTF_8`）を先に引き、その定数の canonical 名が
正規化一致したときだけ即返す。併せて名前の比較を無確保にした（`enc_name_eq`: ASCII の
大文字小文字と `-` / `_` を無視して 1 バイトずつ突き合わせる）:

- mail **164 → 111 ms（−32 %）**、対 YJIT 0.60 → 0.89
- `Encoding.find("Big5-HKSCS")` のような「別名表が前方一致で誤解決しうる」名前は
  一致判定で弾かれて従来の走査に落ちるので、挙動は変わらない
  （`utf8` / `binary` / `ASCII-8BIT` / `Encoding` オブジェクト / 未知名 →
  `ArgumentError` を確認済み）

---

## 3. grape — ホットスポットが無く、広く 1.35 倍

1 リクエストあたり monoruby **84.7 k 命令**、CRuby+YJIT **62.7 k 命令**（比 1.35 倍）。
一方で実時間は 13.7 µs vs 7.5 µs（比 1.83 倍）なので、**残りの 1.4 倍は IPC**
（malloc/free とハッシュ表と TLS アクセスが増やすキャッシュミス）と読める。

### 3.1 内訳（Ir/req）

| バケット | monoruby | % | CRuby+YJIT | % | 比 |
|---|---:|---:|---:|---:|---:|
| JIT コード | 20,628 | 24.4 | 11,013 | 17.6 | 1.87x |
| その他 | 17,643 | 20.8 | 9,599 | 15.3 | 1.84x |
| GC + オブジェクト確保 | 9,050 | 10.7 | 9,475 | 15.1 | 0.96x |
| Hash | 8,405 | 9.9 | 9,169 | 14.6 | 0.92x |
| **libc malloc/free** | **8,288** | 9.8 | **1,074** | 1.7 | **7.72x** |
| String | 6,887 | 8.1 | 4,064 | 6.5 | 1.69x |
| Regexp | 4,147 | 4.9 | 3,844 | 6.1 | 1.08x |
| VM/呼び出し | 3,807 | 4.5 | 8,823 | 14.1 | 0.43x |
| メソッド/定数/gvar 探索 | 3,334 | 3.9 | — | — | — |
| ivar/shape | 501 | 0.6 | 4,684 | 7.5 | 0.11x |
| 合計 | **84,659** | 100 | **62,661** | 100 | **1.35x** |

読み方: monoruby は **呼び出しと ivar アクセスでは勝っている**（VM/call 0.43x,
ivar/shape 0.11x — インライン化とオブジェクトのインライン ivar スロットが効いている）。
負けているのは malloc/free・String・生成コード量。

### 3.2 1 リクエストで払っているもの

| 項目 | 回数/req | 備考 |
|---|---:|---|
| Ruby オブジェクト確保（`Allocator::alloc`） | 40 | TLS + `RefCell` の借用だけで 1,565 Ir/req |
| malloc（`RurubyAlloc::alloc`） | 38 | 16 B クラスが 63 個/req |
| `Hash#include?` | 13 | 160 Ir/回 |
| `$gvar` 読み出し（`runtime::get_global_var`） | 14 | JIT がグローバル変数読みをキャッシュせず毎回ハッシュ表 |
| `Kernel#dup` | 4 | 684 Ir/回（`env_template.dup` ほか） |
| 文字列補間（`concatenate_string`） | 5 | 1,065 Ir/回 |
| `to_s` の Rust からの起動（`invoke_tos`） | 16 | |
| `String#include?` | 3 | 700 Ir/回。§5 D の無駄な確保あり |
| グローバルメソッドキャッシュ参照 | 6 | `JSON::Generator::State.===` ×3、`Rack::Headers#default` ×2 ほか |

GC 自体は 23,000 req で 16 回（うちメジャー 1 回）なので、GC 頻度の問題ではない。
**「1 オブジェクトごとに malloc が付いてくる」構造**が効いている: CRuby はスロット
確保が bump に近く、短い String や小さい Array は埋め込みで malloc しない。

`grape` に固有の病理（ポリモーフィック化、再コンパイル嵐、deopt ループ）は見つからなかった:
`--features profile` の再コンパイル統計は起動時の数十件だけ、定常状態の deopt はゼロ。

---

## 4. lee — 境界チェック 1 個が命令数の 31 %

まず前提として、**lee は YJIT より速い**: 1 反復の命令数は monoruby 4.35 G に対して
CRuby+YJIT 8.20 G（0.53 倍）。それでも ruby-head の ZJIT（ポータル 273 ms）には
負けているので、中身を見た。

### 4.1 犯人

`benchmarks/lee/lib/lee/matrix.rb`:

```ruby
def index(y, x)
  raise unless (0...@height).include?(y)
  raise unless (0...@width).include?(x)
  y * @width + x
end
```

`Matrix#[]` / `#[]=` は 1 反復で約 188 万回呼ばれる。したがって:

| 項目 | 回数/反復 | inclusive Ir/反復 | % |
|---|---:|---:|---:|
| `runtime::gen_range`（Range オブジェクト確保） | **3,769,238** | 769 M | **17.7 %** |
| `Integer#<=>`（`numeric::integer::cmp`） | **7,545,887** | 573 M | **13.2 %** |

`(0...h).include?(y)` 1 回の内訳を micro で確認すると **gen_range 1 回 + `Integer#<=>` 2 回**。
理由は 2 つある。

1. **`Range#include?` / `#cover?` / `#===` は Ruby で書かれている**
   （`monoruby/builtins/range.rb`）。Rust 側の `range_include_impl`（Fixnum 同士なら
   比較 2 回で終わる完成した実装）は `include?` として登録されているのに、
   Ruby 定義に上書きされて**到達しない**。実際の経路は
   `include?` → `is_a?` 4 回 → `cover?` → `is_a?(Range)` → `__cover_val_q` →
   `self.begin` / `self.end` / `nil?` ×2 / `<=>` ×2 / `exclude_end?`。
   境界チェック 1 回にディスパッチが約 10 回。
2. **Range リテラルが毎回確保される**。`gen_range` は 204 Ir/個で、うち 2 回は
   `Value::get_class_obj`（両端のクラスが違うときだけ `<=>` 検証するための判定）。
   377 万個/反復は GC バケット 16.7 % の主因でもある。

### 4.2 その他

| 項目 | 回数/反復 | Ir/反復 | 備考 |
|---|---:|---:|---|
| `Struct#initialize` | 570,208 | 90 M | `Lee::Point` は `Struct.new(:x, :y)` |
| ↑ の中の `IdentId::get_id` | 1,140,416 | 164 M | **メンバ名を確保のたびに intern している**（`get_members` と `initialize` で 1 回ずつ） |
| Ruby オブジェクト確保 | 5.4 M | — | 1 反復あたり。GC は 8 回/反復 |

`Struct` のメンバ名は定義時に `IdentId` として持てるはずで、インスタンス生成のたびに
文字列を intern し直すのは素直に無駄（1 反復 164 M 命令 = 3.8 %）。

### 4.3 検証

`Range#include?` / `#cover?` / `#===` の先頭で、両端と引数がすべて数値なら Rust の
`range_include_impl` に直行する `__cover_num_q` を呼ぶ（数値でなければ `nil` を返して
従来の Ruby 実装に落ちる。Integer / Float の `<=>` が再定義されていたら fast path は
降りる）:

- lee **382 → 360 ms（−5.8 %）**、grape **778 → 755 ms（−3.0 %）**
- `(0...100).include?(50)` 100 万回: 82.5 → 71.9 ms
- Range の**確保自体は残っている**（`include?` を呼ぶ前に Range を作るのは
  bytecode の形のまま）ので、17.7 % の方はまだ丸ごと残っている（§5 の C）

---

## 5. 対策候補（実装コスト順）

| # | 施策 | 変更箇所 | 実測/見積り |
|---|---|---|---|
| **B**（実施済み） | `Encoding.find` を別名表 → O(1) で解決し、名前比較を無確保にする。外れたときだけ従来の走査 | `builtins/encoding.rs` | **mail −32 %**（実測） |
| **B'** | 正規化名 → Encoding の memo 表を class_version でキャッシュし、走査そのものを消す | 同上 | 走査に落ちる名前も O(1) 化 |
| **A**（実施済み） | `Range#include?` / `#cover?` / `#===` に数値 fast path | `builtins/range.rb` + `builtins/range.rs` | **lee −5.8 %、grape −3.0 %**（実測） |
| **C** | Range リテラル + `include?` / `cover?` / `===` の畳み込み（レシーバが他で使われないなら Range を確保せず比較 2 個に落とす） | bytecodegen または TraceIR のピープホール | lee の 17.7 % と GC 分 |
| **D** | `String#include?` が針を毎回 `into_owned()` してコピーしている（`Cow::Borrowed` でも確保する） | `builtins/string.rs:2112` | grape で malloc 3 回/req |
| **E** | `Struct` のメンバ名を `IdentId` で保持し、生成のたびの intern をやめる | `builtins/struct_class.rs` | lee 3.8 % |
| **F** | JIT のグローバル変数読みをインラインキャッシュ化 | `codegen/` | grape 14 回/req の runtime 呼び出し |
| **G** | ペイロードの malloc を減らす（短い String / 小さい Array / 小さい Hash の埋め込み、あるいは size-class 別のフリーリスト） | `alloc.rs`, `value/rvalue/*` | grape の malloc 9.8 %（YJIT 比 7.7 倍）。重いが効く範囲は広い |

A・B はこのブランチで実装した。C・G は設計が要る。

---

## 6. 計測手順（再現）

```sh
git clone --depth 1 https://github.com/ruby/ruby-bench.git ../ruby-bench
cd ../ruby-bench/benchmarks/{grape,mail,lee} && bundle install   # それぞれ
cargo install --path monoruby

# 中央値（ハーネスは後半半分の中央値を出す）
cd ../ruby-bench
MAX_TIME=60 monoruby      -I harness-warmup benchmarks/mail/benchmark.rb
MAX_TIME=60 ruby --yjit   -I harness-warmup benchmarks/mail/benchmark.rb
```

`perf` が使えないので、関数別は callgrind の**差分法**を使う。反復数だけを変えた
2 回のプロファイルを取り、`calls=` 行の inclusive コストと自己コストを引き算すると、
起動・ウォームアップ・GC の初期化が相殺されて定常状態だけが残る:

```sh
N=2000  valgrind --tool=callgrind --callgrind-out-file=grape.s.cg --cache-sim=no monoruby benchmarks/grape/small.rb
N=22000 valgrind --tool=callgrind --callgrind-out-file=grape.l.cg --cache-sim=no monoruby benchmarks/grape/small.rb
```

`small.rb` はハーネスを使わず固定回数だけ回す薄いドライバ（本稿では grape 2,000/22,000 req、
mail 50/550 通、lee 1/5 反復）。差分と呼び出し元の集計は素の Python で `.cg` を読めばよい
（`fn=` / `cfn=` / `calls=` の 3 行組を拾うだけ）。

deopt・再コンパイル・グローバルメソッドキャッシュ・GC の統計は
`cargo build --release --features profile,gc-log` のバイナリが終了時に stderr へ出す。
