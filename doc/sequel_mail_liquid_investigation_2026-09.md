# sequel / mail / liquid-il / liquid-render が monoruby で遅い原因の調査（2026-09-10）

調査日 2026-09-10。対象コミットは `9c32041`（master, "&block parameter as a slot with a
lazy, cached Proc" #1305）。比較対象は rbenv でビルドした CRuby 4.0.2（`--yjit` あり / なし）。
ベンチ本体は [ruby/ruby-bench](https://github.com/ruby/ruby-bench) を `harness-warmup` で走らせ、
後半半分の反復の中央値を使う。gem は sequel 5.94 + sqlite3 1.7.3（monoruby 側は
`gem/sqlite3` の Fiddle 実装）、mail 2.7.1、liquid v5.4.0、liquid-il `1b07d16`。

計測機は x86-64 / Linux 4 vCPU（コンテナ、`perf` なし）。関数別の内訳は
`valgrind --tool=callgrind` の命令数で採った（§6 に手順）。先行調査
[`yjit_bench_slow_investigation_2026-09.md`](yjit_bench_slow_investigation_2026-09.md)、
[`ruby_bench_low_cost_ideas_2026-09.md`](ruby_bench_low_cost_ideas_2026-09.md) の続き。

---

## 1. 結論（先に要点）

| ベンチ | monoruby | CRuby --yjit | 速度比 | CRuby インタプリタ | monoruby `--no-jit` |
|---|---:|---:|---:|---:|---:|
| liquid-il | 524 ms | 193 ms | 0.37 | 422 ms | 818 ms |
| liquid-render | 174 ms | 77 ms | 0.44 | 192 ms | 309 ms |
| mail | 186 ms | 120 ms | 0.65 | 174 ms | 267 ms |
| sequel | 112 ms | 60 ms | 0.53 | 82 ms | 167 ms |

（monoruby 列は調査開始時点。§2.1・§2.2・§5.2 の修正後は liquid-il ≈ 360 ms、
liquid-render ≈ 140 ms、sequel ≈ 88 ms。）

4 本とも **JIT を切った CRuby のインタプリタと同等かそれ以下**で、先行調査の headline
4 本と同じく、差は JIT の生成コードではなくランタイム側にある。ただし今回は JIT の
**方針**に起因する 2 つの大きな穴が見つかった。

1. **liquid-il（原因の 1/3 = 命令数の 25.7 %）: `String#<<` のインライン fast path が、
   コードレンジ未計算の追記片を畳み込んでバッファを Unknown にし、次の別エンコーディング
   の追記（`buf << 1.to_s`、US-ASCII → UTF-8）でバッファ全体を再走査していた。**
   `sub` / `gsub` の結果は再タグ付けで必ず未計算だったので、テンプレートが値を escape して
   バッファに書くたびに O(バッファ長) を払っていた（1 反復で 14.8 万回、`Encoding::classify`
   が全命令の 25.7 %）。→ 修正済み（§2.1）: liquid-il **519 → 341〜367 ms（−32 %）**。
2. **liquid-render: `Liquid::Context#find_variable` が 8 秒間に 4,556 回再コンパイル
   されていた**（理由 `BecamePolymorphic`）。再コンパイルで作った新しい本体が新しい
   deopt 予算（10 回）を持つので、受信側クラスが増え続けるサイトでは
   deopt → 再コンパイル → deopt … が止まらない。JIT コードも解放されないので RSS が
   CRuby の 4.8 倍（192 MB vs 40 MB）になっていた。→ 「同じメソッドの
   `BecamePolymorphic` 再コンパイルは 4 回まで」の上限を入れた（§2.2）:
   liquid-render **177 → 133〜147 ms（−21 %）、RSS 177 → 83 MB**。
3. 残りは先行調査と同じランタイム側の項目の組み合わせ: `respond_to?` の 2 段グローバル
   キャッシュ探索（liquid-render で 8 秒間に 1,200 万回）、`case obj when String`
   （`Module#===`）がグローバルキャッシュ経由（liquid-il で 850 万回）、
   PIC 容量（4）を超えた `nil?` サイトの deopt 嵐（liquid-il で 510 万回）、
   `send(name, *args)` の splat 経路が YJIT の 3.4 倍遅い（mail の `method_missing`
   委譲、liquid のフィルタ呼び出し）、mail の `Encoding.find` の定数線形走査、
   sequel の Fiddle 経由 SQLite と 1 クエリあたり 9 回の正規表現（§3〜§5）。
4. **sequel（1 反復の 64 %）: `Time.parse`。** sqlite の `datetime` 列を Sequel が
   1 列 2 回 `Date._parse` にかけ（1 反復 4,000 回）、monoruby の Ruby 実装
   `stdlib/date_core.rb` が 1 回 17.9 µs（CRuby の C 実装 7.6 µs）だった。月名パターンを
   毎回組み立て直す・文字種を見ずに全サブパーサを試す・数字だけの値にも正規表現を 5 回
   かける、の 3 点を直して **11.3 µs（−37 %）**（§5.2）。残りの差の主因は「7 bit の対象に
   対しても UTF-8 でコンパイルした正規表現で照合する」ことによる `/i` の遅さで、
   CRuby と同じく US-ASCII でコンパイルしたものを 7 bit の対象に使うようにした（§2.3、
   `Date._parse` 8.4 µs、regex を使う全コードに効く）。

## 2. 直した / 試した 2 件

### 2.1 `String#<<` のコードレンジ畳み込み（liquid-il）

callgrind（1 反復 + 起動）の self 上位:

| 関数 | 命令数 | 割合 |
|---|---:|---:|
| `Encoding::classify` | 1.55 G | **25.7 %** |
| malloc / free 系 | 0.55 G | 9.1 % |
| JIT 生成コード（無名） | ≈ 0.3 G | ≈ 5 % |
| `GlobalMethodCache::get` + `find_method` + `check_method_*` | 0.24 G | 4.0 % |
| `Module#===`（`teq`）+ `cmp_teq_values_impl` | 0.09 G | 1.5 % |

`classify` の呼び出し元は `RStringInner::compatible_encoding`（148,651 回、1 回平均
1 万命令 = 1 KB 前後のバッファを毎回走査）← `extend` ← `shl_inner` ←
`string_shl`（**JIT インライン `<<` のフォールバックヘルパ**、171,735 回）。

仕組み: `emit_string_shl`（両バックエンド）は同じエンコーディングの String 追記を
インラインで行い、コードレンジを「両側 SevenBit → SevenBit、両側 {SevenBit, Valid} →
Valid、**それ以外 → Unknown**」で畳み込む。追記片の `cr` が未計算（Unknown）だと受信側が
Unknown になる。次に別エンコーディングの片（`Integer#to_s` / `Float#to_s` は US-ASCII）が
来るとインラインは不可でヘルパへ行き、`extend` → `compatible_encoding` →
`Encoding::compatible(self.encoding(), self.code_range(), …)` が **受信側全体を
`classify`** する。これがバッファの長さに比例するので、追記回数 N に対して O(N²)。

`cr` が未計算のまま返ってくる生成元を `MONORUBY_CR_DEBUG` 付きの一時ビルドで
数えた（300 回追記して受信側が Unknown になった回数）:

| 追記片の生成元 | 回数 | | 生成元 | 回数 |
|---|---:|---|---|---:|
| `gsub` / `sub`（正規表現・String パターン・block・Hash とも） | 297 | | `tr`, `strip`, `downcase`, `reverse`, `center`, `delete`, `squeeze`, `succ` | 0 |
| `force_encoding`（同じエンコーディングでも） | 299 | | `split`+`join`, `chars.join`, `encode`, `byteslice`, `unpack1` | 0 |
| `CGI.escapeHTML`, `JSON.generate` | 297–298 | | リテラル, `to_s`, 補間, `+`, `*`, `%`, `[]`, `upcase` | 0 |

Liquid テンプレートは値を `escape`（`gsub`）してから `_O << …` するので、liquid-il は
まさにこの形（1 反復に 17 万回の別エンコーディング追記）。

修正（コミット `c2e6acd`）:
- インライン `<<`（x86-64 / aarch64）: 追記片の `cr` が SevenBit / Valid でなければ
  インラインせずヘルパへ。ヘルパは短い片を 1 回 `classify` して片自身にキャッシュする
  ので、2 回目以降はインラインに戻る。
- `RStringInner::set_encoding`: 同じエンコーディングへの再タグ付け（`sub` / `gsub` 結果の
  `apply_template_encoding`、同じ encoding への `force_encoding`）では `cr` を捨てない。

| | 前 | 後 |
|---|---:|---:|
| `o << "a<b".gsub("<","&lt;"); o << i.to_s` × 4000 | 38.4 ms | 1.8 ms |
| 同 × 1000 / 2000 | 2.6 / 9.9 ms（二次関数的） | 0.39 / 0.90 ms（線形） |
| liquid-il（2 ラウンド交互） | 519 / 517 ms | 341 / 367 ms |
| liquid-render / mail / sequel | 177 / 188 / 110 ms | 178 / 191 / 112 ms（±0） |

CRuby+YJIT 193 ms に対してまだ 1.8 倍。残りは §3。

### 2.2 再コンパイルの上限（liquid-render）

`--features profile` の 8 秒間（≈ 30 反復）の統計:

```
jit recompile stats
  Liquid::Context#find_variable   Liquid::Context   BecamePolymorphic   4556
deoptimization stats
  Liquid::Context#lookup_and_evaluate [:00023]  674,551  %5 = %4.is_a?(%5)      POLYMORPHIC [NilClass]
  Liquid::Context#evaluate            [:00002]  236,639  %2 = %1.respond_to?(%2) POLYMORPHIC [NilClass]
  block in Liquid::VariableLookup#evaluate      59,840  respond_to?             POLYMORPHIC [ForloopDrop]
  Liquid::BlockBody#render_to_output_buffer     53,074  instance_of?            POLYMORPHIC [Liquid::If]
  Liquid::Context#find_variable       [:00019]  45,560  %4 = %4.to_liquid()     POLYMORPHIC [ForloopDrop]
```

`find_variable` は `@scopes.find_index { |s| s.key?(key) }` → `variable.to_liquid` →
`variable.respond_to?(:context=)` の 3 つの多相サイトを持ち、値の型が
String / Integer / nil / Hash / Array / 各 Drop … と render ごとに変わる。
`to_liquid` サイトの deopt 数 45,560 は再コンパイル数 4,556 のちょうど 10 倍
（= `COUNT_DEOPT_RECOMPILE`）で、同じサイトが本体ごとに予算を使い切って再コンパイルを
要求し続けている。

「一度多相になったサイトは戻らない」というラチェットは、VM の POLY バイトと、
単相ガードの `RecvMissMode::Learn`（PMC に 2 クラス以上あれば plain deopt）については
成り立つ。止まらないのは **PIC（`compile/pic.rs`）の最終腕の出口**で、こちらは
「admitted < `PIC_WAYS`（4）なら再コンパイル出口、満杯なら plain deopt」をコンパイル
ごとに PMC から再評価する。`--features deopt` で PIC の判定を出すと（`### pic pmc` /
`### pic built`、今回追加した診断）:

```
### pic pmc   to_liquid entries=[("Hash", 4), ("NilClass", 4), ("String", 3), ("Liquid::ForloopDrop", 1)] overflow=0 observations=12
### pic built to_liquid admitted=3 arms=[(Hash), (NilClass), (String)]      ← 5 回とも同じ
```

- ForloopDrop は count 1 × `PMC_SET_SHARE_DIVISOR`（8）< observations 12 なので
  「稀な尾」として腕を作らず落とされ、admitted = 3 < 4 で出口は再コンパイル出口になる。
- ところが **ForloopDrop の count は増えない**: PMC は VM の slow path（インライン
  キャッシュ miss）でしか記録されず、deopt した ForloopDrop は最初の 1 回で VM の単相
  キャッシュに入り、以後の deopt 再実行はキャッシュ hit で記録されない。他のクラスは
  JIT 側が処理するので VM には来ない。つまり deopt しているクラスこそ count が 1 で
  止まる（`entries=[…, ("Liquid::ForloopDrop", 1)]` が 5 回のコンパイルで不変）。
- 再コンパイルは同じ PMC から同じ 3 腕の鎖を作り直し、新しい本体は新しい 10 回の
  予算を持つので、10 回 deopt → 再コンパイル → … が止まらない。1 回の再コンパイルは
  数百 µs で、8 秒に 4,556 回 = 1〜2 秒。古い本体は解放されないので JIT 領域も
  増え続ける（RSS 192 MB、CRuby は 40 MB）。

**対処**（コミット `83b86e6`、`compile/pic.rs`）: 3 点をまとめて直した。

1. **腕に空きがある限り、PMC の全クラスに腕を作る**（share 閾値を PIC から撤廃）。
   PMC の count は miss の回数であって呼び出し回数ではないので、share は交通量を
   表さない。deopt しているクラス 1 つの compare は deopt より桁違いに安い。
   クラス集合ガード（`pmc_same_target_classes`）の閾値は残した: そちらは全員が同じ
   target なので、稀なクラスを入れても compare が増えるだけで得るものがない。
2. **再コンパイル出口の条件を「admitted < 4」から「PMC がまだ学べる」に変更**
   （`can_learn` = entries < `PMC_WAYS` かつ overflow なし）。再構築で腕が増えるのは
   VM が新しいクラスを記録できるときだけで、「admitted < 4」は落としたクラスがある
   ときや 5 つ目のクラスが overflow したときにも真になり、どちらも再構築で腕は増えない。
3. **腕を作れなかったクラス（`dropped`: 解決不能、可視性、capture、`&blk` 引数の callee、
   非正規の accessor 呼び出し）は鎖の手前で plain deopt に振り分ける**。これらは既に
   PMC にあるので再構築しても再び落とされる。再コンパイル出口が生きている本体でだけ
   1 つの `BrClassNotIn` を払う。

これで PIC 出口からの再コンパイルは「PMC が 1 エントリ増えた後」にしか起きず、PMC は
最大 4 エントリなので **1 サイトあたり最大 4 回**、メソッドあたり 4 × PIC サイト数で
必ず止まる（ラチェット）。`--features deopt` の liquid-render で確認:

```
### pic built to_liquid admitted=3 dropped=[] can_learn=true  arms=[Hash, NilClass, String]
   → ForloopDrop が miss → VM が記録 → 1 回だけ再構築 →
### pic built to_liquid admitted=4 dropped=[] can_learn=false arms=[Hash, NilClass, String, ForloopDrop]
```

8 秒間の `BecamePolymorphic` 再コンパイルは全体で **4 回**（前: 4,556 回。§2.2 冒頭の
上限は保険として残す）。ベンチ時間は上限 4 回入りの直前バイナリと比べてばらつきの範囲内
（liquid-render 118 / 119 → 121 / 123 ms、liquid-il 338 / 341 → 337 / 348 ms、sequel・mail
同様）: 上限で時間はほぼ回収済みで、今回の変更は上限に頼らず構造的に止めるためのもの。

他の 2 つの `BecamePolymorphic` 出口は元からラチェットになっている: 二項演算
（`binary_op.rs`）は POLY ビットが立った後のコンパイルでは plain deopt、単相ガード
（`method_call.rs` の `RecvMissMode::Learn`）は PMC に 2 エントリ以上あれば plain deopt。
残る前提は「miss したクラスは VM の slow path で PMC に記録される」で、表現の違いで
記録されない Integer / Bignum は既に除外済み。

対策（コミット `c7cdd1a`、`Codegen::recompile_counts` /
`Codegen::recompile_budget_exhausted`）: (iseq, self class) ごとの
**`BecamePolymorphic` 理由の**再コンパイル回数を数え、`MAX_RECOMPILES_PER_METHOD = 4`
を超えたら再コンパイルせず現在の本体を残す（予算は使い切っているので、そのサイトは
以後 plain deopt になる）。`ClassVersionGuardFailed` / `ConstVersionGuardFailed` は
数えない。こちらはプログラムが本当にメソッド・定数を定義し直した場合で、何度起きても
作り直さないと古い本体が使えないままになる（autoload の多いアプリで hot メソッドが
インタプリタに固定されるのを避ける）。

| | 前（cr 修正後） | 上限あり |
|---|---:|---:|
| liquid-render（2 ラウンド交互、中央値） | 177 / 177 ms | **133 / 147 ms（−21 %）** |
| liquid-render RSS | 177 MiB | **83 MiB（−53 %）** |
| liquid-il | 374 / 339 ms | 367 / 361 ms（差はばらつき ±3 % の範囲内） |
| mail | 190 / 199 ms | 186 / 184 ms |
| sequel | 112 / 108 ms | 114 / 113 ms |

liquid-il は、上限に達したメソッドが本来なら 5 回目以降の再コンパイルで多相化できた
サイトを plain deopt に固定してしまう可能性があるが、2 ラウンドの計測では
ばらつきの範囲内だった（liquid-render の 4,556 回に対しては 4 でも 50 でも同じ効果に
なるので、必要なら上限だけ上げればよい）。

これは止血であって、本来の形は先行調査 §5.5 の「PIC 容量を超えたサイトは受信側クラスに
依らない汎用呼び出し（`find_method` + インラインキャッシュ、deopt なし）に落とす」。
上限に達したメソッドはそのサイトで毎回 deopt して残りを VM で走るので、liquid-render は
まだ CRuby インタプリタ並みに留まる。

### 2.3 7 bit の対象は US-ASCII でコンパイルした正規表現で照合する

§5.2 で見つかった「`/i` の文字クラスだけ CRuby の 4.5 倍遅い」の対処
（コミット `583aae3`、`RegexpInner::engine_for` / `ascii_engine`）。

CRuby は `rb_reg_prepare_enc` で、ソースが 7 bit でエンコーディングが固定されていない
正規表現を US-ASCII のままコンパイルし、対象文字列が 7 bit（cr = 7BIT）ならそれで照合する。
monoruby は常に UTF-8 でコンパイルしていたので、Onigmo が 1 位置ごとに `mbc_enc_len` /
`onigenc_mbclen_approximate` を呼び、`/i` では Unicode の case fold 表を引いていた
（sequel の warm プロファイルで 3.7 %、`Date._parse` では 6.5 %）。7 bit のデータに
対する結果は両者で同じ。

実装: `RegexpInner` が既に持つ native エンコーディング用スロット（`native` /
`native_enc`、BINARY の対象向けに同じ US-ASCII コンパイルを使う）をそのまま流用し、
`engine_for(given, known_ascii)` が照合ごとにエンジンを選ぶ。対象が 7 bit かどうかは
呼び出し側の cr（`StringScanner`、`gsub` のブロック版）か、2 KiB までの `is_ascii()`
プローブで決める（それより長い対象は UTF-8 のまま。`index(re, pos)` を長い文字列に
繰り返す経路で O(n²) にしないため）。`scan` / `gsub` / `captures_iter` の走査は 1 回だけ
プローブしてループ全体で同じエンジンを使う。対象外: 非 ASCII を含むソース、
`fixed_encoding`、`\p{…}` / `\P{…}`（CRuby もこれで UTF-8 に固定する — ついでに
`fixed_encoding?` を CRuby に合わせた）、`\u` エスケープ、US-ASCII でコンパイルに失敗する
もの（1 回だけ試して結果を覚える）。

| マイクロベンチ（19 文字の 7 bit 文字列） | 前 | 後 | CRuby |
|---|---:|---:|---:|
| `s.match?(/[a-z]/i)` | 414 ns | **95 ns** | 91 ns |
| `s =~ /\b(sun|mon|…)[^-\/\d\s]*/i`（miss） | 223 ns | 171 ns | 130 ns |
| `s =~ TIME_PAT`（`/x` の大きな交替、hit） | 1,938 ns | 1,596 ns | 2,324 ns |
| `s.gsub(/[^-+',.\/:@[:alnum:]\[\]]+/, " ")` | 1,327 ns | 1,048 ns | 1,540 ns |
| `Date._parse("2026-09-10 08:40:12")` | 11.3 µs | **8.4 µs** | 7.6 µs |

ベンチ全体（2 ラウンド交互、中央値）:

| | 前（§5.2 まで） | US-ASCII 照合 |
|---|---:|---:|
| sequel | 87 / 84 ms | 82 / 84 ms |
| mail | 189 / 185 ms | 183 / 193 ms |
| liquid-il | 355 / 353 ms | 347 / 355 ms |
| liquid-render | 126 / 131 ms | 118 / 125 ms（−5 %） |

ベンチ全体では liquid-render の −5 % 以外はばらつきの範囲内: 各ベンチの正規表現の
比率が限られている（sequel は `Date._parse` 整理後で 1 反復の 1 割程度）ためで、
効果はマイクロベンチの通り正規表現 1 回あたり 10〜75 %。`/i` や文字クラスを多用する
コード（テンプレートエンジン、パーサ、`StringScanner` ベースの字句解析）ほど効く。

## 3. liquid-il に残っている分

`--features profile` の 8 秒間（≈ 15 反復）:

```
deoptimization stats
  LiquidIL::RuntimeHelpers.lookup_prop_fast [:00006]  5,129,845  %4 = %3.nil?()  POLYMORPHIC [String]
  LiquidIL::RuntimeHelpers.t                [:00001]    465,024  to_liquid       POLYMORPHIC [Float]
  LiquidIL::RuntimeHelpers.compare          [:00007]    255,025  to_liquid_value POLYMORPHIC [Integer]
  LiquidIL::RuntimeHelpers.lookup           [:00007]    135,007  to_liquid       POLYMORPHIC [Hash]
global method cache stats
  ===   #<Class:String>   6,016,109      <<   String   2,093,200      nil?  String  810,049
  ===   #<Class:Integer>  2,490,338      ===  #<Class:Hash> 765,889   nil?  Float   735,106
polymorphic sites
  nil?  overflow 540,036  String x809,945 | Float x674,952 | Integer x46 | BOOL x65
```

- **PIC 超過の deopt 嵐**: `lookup_prop_fast` の `v.nil?` は String / Float / Integer /
  true / nil / Array … と 5 種類以上の受信側を見る。PIC は 4 way なので溢れ、
  `RecvMissMode::Learn` の ratchet（「PMC が多相なら再コンパイルしても同じ本体になる」）で
  plain deopt に固定され、1 反復 ≈ 34 万回、呼び出しごとに deopt + 残りを VM で実行。
  マイクロベンチでは 4 クラスまでの `nil?` は 4.5 ns、6 クラスで 25 ns、8 クラスで
  39 ns（YJIT 50 ns）なので deopt 自体は安いが、`lookup_prop_fast` の残り
  （`!obj.key?(key) ? obj[key.to_sym] : v`）が VM で走る分が乗る。
- **`case value when String … when Integer …`（`output_append`）**: 受信側が Class
  オブジェクトの `===` は `cmp_teq_values_impl` の `_ =>` 腕で `invoke_method(TEQ)` →
  `find_method` → グローバルメソッドキャッシュ → builtin `teq`。8 秒で 930 万回。
  マイクロベンチ（4 クラスを巡回する `case`）で 139 ns vs YJIT 110 ns、単相なら 11 vs 46。
- **`String#<<` のグローバルキャッシュ 209 万回**: 受信側クラスが確定しないサイト
  （`output_append` の `output`）の汎用経路。
- 生成コード（`render`）はラムダ（partial）をローカルに束縛するので、フレームが
  捕捉されて loop JIT の対象外（先行調査 §4.3）。`--no-jit` 818 ms → JIT 524 ms で
  1.56 倍しか効いていない。

cr 修正後の、起動を除いた 1 反復（23.0 億命令）の self 上位:

| 関数群 | 割合 | 内容 |
|---|---:|---|
| JIT 生成コード（無名アドレス） | ≈ 15 % | |
| `GlobalMethodCache::get` + `Executor::find_method` + `check_method_for_class_with_version` + `check_method_with_refinements` + `invoke_method` + `runtime::find_method` + `get_class_obj` + `class_for_ic` | **15.7 %** | 汎用ディスパッチ（`===` / `<<` / `nil?` / `to_liquid` の多相・PIC 超過サイト） |
| `Module#===`（`teq`）+ `cmp_teq_values_impl` + `Kernel#is_a?` + `expect_class_or_module` | 7.2 % | `case value when String …`、`obj.is_a?(Hash)` |
| String キー Hash（`string_key_eq_c` + `RStringInner::hash` + `string_digest_c`）| 3.6 % | `obj[key]`（fixture の Hash） |
| `ruby_float_to_s` + grisu | 2.0 % | 価格などの Float 出力 |
| malloc / free / RValue alloc | ≈ 9 % | |

つまり残りの半分近くが「JIT が受信側クラスを確定できないサイトの汎用ディスパッチ」で、
§6 の 2〜4（PIC 超過サイトの汎用 IC 化、`===` の直接判定、`respond_to?` キャッシュ）が
そのまま効く。

## 4. liquid-render に残っている分

```
global method cache stats（8 秒）
  respond_to_missing?  String  4,105,592    evaluate  String            2,535,720
  evaluate  Liquid::VariableLookup 1,141,721    to_liquid_value  String  1,121,320
  respond_to_missing?  Hash      975,801    context=  Hash               894,200
  []        Hash                 861,017    respond_to_missing?  Integer 533,121
  respond_to_missing?  NilClass  520,895    context=  String             445,400
  to_ary    Hash                 122,401    []  NilClass                 121,720
```

- **`respond_to?` が 1 回に 2 段のグローバルキャッシュ探索**: `Kernel#respond_to?`
  builtin は `check_method_with_refinements(class, name)` で見つからないと
  `check_method(self, :respond_to_missing?)` を引いてから `ConstReturn` ヒントで畳む。
  `context.evaluate(obj)` の `obj.respond_to?(:evaluate)`、`find_variable` の
  `respond_to?(:context=)` が全変数参照で走り、8 秒で 1,200 万回。単相サイトでは JIT が
  1.5 ns に畳むが（先行調査 §8 施策 5）、多相サイトは builtin 呼び出しに落ちる。
- **多相 `obj[key]`**（Hash / ForloopDrop / nil …）: `Hash#[]` がグローバルキャッシュ経由
  86 万回。
- **フィルタ呼び出し `strainer.invoke(method, *args)` → `send(method, *args)`**: splat 付き
  `send` は `jit_handle_arguments_no_block_for_send_splat` の汎用経路で、
  `s.send(:size, *[])` が 241 ns vs YJIT 71 ns（3.4 倍）。splat なしは 12.6 ns。
- `variable.to_liquid` / `value.is_a?(Proc)` / `node.instance_of?(String)` の多相 deopt
  （上の deopt 表）。

## 5. mail と sequel

### 5.1 mail

```
global method cache stats（8 秒、≈ 40 反復）
  to_str  Encoding  240,000     to_s  Regexp  202,072     method_missing  Mail::Field  196,000
  respond_to?  NilClass 112,015  main_type  Mail::ContentTypeField  74,006
deoptimization stats
  Mail::Field#new_field          57,943  %4.new(...)          POLYMORPHIC [各 Field クラス]
  Mail::Field#method_missing     42,000  %4.send(%5,*%6,&%7)  POLYMORPHIC
  Mail::Utilities.blank?         23,999  kind_of?             POLYMORPHIC [NilClass]
```

deopt は少なく、1 反復（50 通の parse + `to_s`）あたりの固定費が薄く広く乗っている。
- `Mail::Field` は全メソッドを `method_missing` → `@field.send(name, *args, &blk)` で委譲する
  （8 秒で 19.6 万回）。splat + block 付き `send` は 281 ns vs YJIT 76 ns（3.7 倍）。
- `to_str Encoding 24 万回`: `force_encoding(Encoding::UTF_8)` / `encode(Encoding::…)` の
  引数を String に coerce しようとして Encoding オブジェクトに `to_str` を探し、
  見つからずに `encoding_of_object` へ落ちる。`Regexp#to_s` 20 万回は `/#{re}/` 形の
  正規表現補間（Ragel 生成パーサ）。
- Ragel 生成のパーサ本体（`data[p].ord`、`_trans_keys[k]`、`case _trans_actions[_trans]`）は
  マイクロベンチでは monoruby の方が速い（63 vs 248 ns / ステップ）。
- 起動を除いた 1 反復（12.8 億命令、§7 の差分法）の self 上位:

  | 関数 | 割合 | 内容 |
  |---|---:|---|
  | `_int_free` + `malloc` + `free` + `RurubyAlloc::alloc` + `finish_grow` | 26 % | 割り当て（String 生成が多い） |
  | `str::replace` + `str::to_uppercase` + `builtins::encoding::enc_find` | **16.8 %** | `Encoding.find(name)` |
  | `match_at` + `forward_search_range` + `onigenc_*` + `mbc_enc_len` | 9.5 % | 正規表現（ヘッダ解析） |
  | `RValue::get_ivar` + ivar 表の indexmap + `Vec<IdentId>` 生成 | 4.6 % | `instance_variable_get` / `instance_variables` |
  | `check_utf8` + `classify` | 2.4 % | |

  **`Encoding.find` が 1 反復の 1/6**: `enc_find` は名前を受け取ると `Encoding` の
  全定数を線形に走査し、定数ごとに `get_constant` → `get_ivar(_ENCODING)` → `to_string` →
  `to_uppercase().replace(['-','_'], "")` で正規化して比較する（`builtins/encoding.rs`）。
  mail は `Mail::Utilities` / `Ruby19` 経由で文字列名（`"UTF-8"`, `"us-ascii"` …）で
  `Encoding.find` / `encode` / `force_encoding` を呼ぶので、ヘッダ 1 個ごとに数十回の
  定数走査が走る。`Encoding::try_from_str`（スタック上で正規化する既存の fast path）で
  `Encoding` に解決し、`find_encoding_object` で定数オブジェクトに戻せば走査は不要。

### 5.2 sequel

1 反復 = `Post[i]` 1,000 クエリ。1 クエリの経路: `primary_key_lookup` → `fetch_rows` →
`execute` → Fiddle で `sqlite3_prepare_v2` / `step` / 列ごとに `column_type` +
`column_*`（9 列）+ `column_decltype` / `column_name`（`result.types` / `result.columns`）→
`base_type_name`（`t =~ /^(.*?)\(/` を **列ごと**）→ `type_proc.call`（`datetime` 2 列は
`to_application_timestamp` → `Time`）→ `Model.call`（`allocate` + ivar）。

warm 1 反復の callgrind 差分（2 反復 − 1 反復、6.09 億命令）:

```
  match_at（onigmo）                    17.2 %    ffi_call_int + fiddle_invoke     3.4 %
  malloc / free 系                     11.6 %    RValue alloc / free / GC         6.1 %
  onig_search_gpos + forward_search    2.8 %    num_bigint shr / sub / Rational  3.6 %（Time）
  mbclen / mbc_enc_len / mbc_case_fold 3.7 %    regexp 構築（pre_validate 等）    2.6 %
  Utf8Chunks::next（文字列の UTF-8 検証） 0.8 %   GvarTable::lookup（`$1`）         0.5 %
```

**最大の項目は `Time.parse` だった。** sqlite は `datetime` 列を文字列で返し、Sequel は
`convert_input_timestamp` で **1 列につき `Date._parse` を 2 回**（`Time.parse` の中と、
オフセット有無を見る `_date_parse(v)`）呼ぶ。`created_at` / `updated_at` の 2 列で
1 クエリ 4 回、1 反復 4,000 回。monoruby の `Date._parse` は `stdlib/date_core.rb` の
Ruby 実装（CRuby は C の `date_parse.c`）で、`"2026-09-10 08:40:12"` 1 回に
**17.9 µs**（CRuby 7.6 µs）かかっていた: 4,000 回 × 17.9 µs = **72 ms = 1 反復 112 ms の
64 %**。

内訳と直したこと（コミット `a3d0352`）:

| 項目 | 前 | 対策 | 後 |
|---|---:|---|---:|
| `/…#{MONTH_PAT}…/i` を **メソッド本体で毎回組み立て**（`__parse_eu` / `__parse_us` / `__parse_vms` / `__parse_mon`）。ソース連結 + `pre_validate` + `\u` 展開 + キャッシュ探索で 1 回 ≈ 6,000 命令 | 17.9 µs | 定数に hoist（CRuby の C 実装もパターンごとに 1 回だけコンパイルする） | 15.2 µs |
| **文字がないパターンも全部試す**: 数字だけの ISO 文字列に対して月名の `/i` 交替（`__parse_eu` 4.7 µs、`__parse_us` も同程度）を走らせていた。CRuby は `check_class` で英字 / 数字 / `-` / `.` / `/` の有無を先に見て、必要な文字がないサブパーサは呼ばない | 15.2 µs | 同じゲートを入れる（`alpha` / `digit` / `dash` / `dot` / `slash`） | 13.3 µs |
| `__s3e`（年月日の並べ替え）が数字だけの値にも正規表現を 5 回かけていた | 13.3 µs | 数字だけなら `to_i` | **11.3 µs** |

`Date._parse` **17.9 → 11.3 µs（−37 %）**、`Time.parse` 18.1 → 10.3 µs。結果は 40 種の
入力で変更前後とも一致（うち `"2000 Jan 1st"` と `"12.30"` は変更前から CRuby と
異なる、既存の問題）。sequel 全体（同じバイナリで `date_core.rb` だけ入れ替え、
2 ラウンド交互）:

| | 前 | `Date._parse` 整理後 |
|---|---:|---:|
| sequel 1 反復（中央値） | 108 / 109 ms | **83 / 94 ms（−14〜23 %）** |
| mail | 181 / 186 ms | 193 / 187 ms（差なし） |

残る 11.3 µs の内訳（ISO 文字列 1 本）: 先頭の `gsub`（1.6 µs）、`TIME_PAT`（`/x` の
大きな交替、1.9 µs）と時刻の内側 `=~`、`__parse_iso` の `sub!` + `__s3e`（3.1 µs）、
`$1` 系（`GvarTable::lookup` + `current_match_data`）。正規表現 1 回あたりの engine 側は
CRuby とほぼ同じ（`/'?(\d+)-(\d+)-('?-?\d+)/` ヒット: 562 ns vs 426 ns、
月名 `/i` の miss: 4.7 µs vs 5.6 µs）だが、**`/i` の文字クラスだけ 4.5 倍遅い**
（`s.match?(/[a-z]/i)`: 414 ns vs 91 ns、`/\b(sun|mon|…)/i` の miss: 223 vs 130 ns）。
原因は monoruby が正規表現を常に UTF-8 でコンパイルすること: CRuby は 7 bit のソースを
US-ASCII でコンパイルし、対象文字列が 7 bit なら（`rb_reg_prepare_enc`）そのまま
US-ASCII のまま照合するので、`mbc_enc_len` / `onigenc_mbclen_approximate` /
`mbc_case_fold`（上の表で 3.7 %）が 1 バイト固定になる。→ §6 の 3。

- 正規表現の残り: `base_type_name` の 9 回/クエリ、`literal_append` の
  `quote`（`gsub("'", "''")`）、`output_identifier`。
- Fiddle: 1 クエリ 30 回前後の FFI 呼び出し（先行調査 §5.8 と同じ）。
- Time: `to_application_timestamp` → `Time` 生成が Rational / BigInt 経由
  （mail でも `local_to_utc Integer` / `coerce Rational` / `+ Rational` が各 1.6 万回）。
  `Rational(1, 10)` が 377 ns（CRuby 97 ns）。

## 6. 提案（コスト順）

| 順 | 施策 | 効くベンチ | コスト | 見込み |
|---|---|---|---|---|
| 1 | `String#<<` インラインの cr 畳み込み修正（**済**、§2.1） | liquid-il、`gsub` 結果を追記する全コード | 低 | liquid-il −32 % |
| 2 | 再コンパイル回数の上限と、PIC 出口のラチェット化（§2.2、**済**）。残るのは PIC 超過（5 クラス以上）サイトの汎用呼び出し化 | liquid-render、activerecord（先行調査 §5.5） | 低 / 中 | liquid-render −21 %、RSS −53 % |
| 3 | 7 bit のソースは US-ASCII でもコンパイルし、対象文字列が 7 bit ならそちらで照合する（**済**、§2.3） | regex を使う全コード（sequel、mail、liquid、activerecord …） | 低 | `/[a-z]/i` 414 → 95 ns、`Date._parse` −26 % |
| 4 | `Date._parse` の Ruby 実装の整理（**済**、§5.2） | sequel、`Time.parse` / `DateTime.parse` を使う全般 | 低 | `Date._parse` −37 % |
| 5 | `Kernel#respond_to?` の (class, name, class_version) キャッシュ | liquid-render、rack、activerecord | 低 | 2 段探索 1,200 万回/8 s → 0 |
| 6 | `cmp_teq_values_impl` に Class 受信側の直接 `is_kind_of` 腕（BOP 未再定義時） | liquid-il、`case x when Klass` 全般 | 低 | `===` 930 万回/8 s のグローバルキャッシュ探索 → 0 |
| 7 | `Encoding.find(name)` を `try_from_str` + `find_encoding_object` で解決（定数の線形走査をやめる）。`force_encoding` / `encode` の Encoding オブジェクト引数で `to_str` を探さない | mail、`encode` を使う全般 | 低 | mail 1 反復の 16.8 % |
| 8 | `send(sym, *args[, &blk])` の splat 経路を非 splat と同じ inline 経路に | mail、liquid-render、`method_missing` 委譲全般 | 中 | 241 → ≈ 15 ns |
| 9 | `String#count` / `delete` の範囲指定（`"a-zA-Z"`）のテーブル構築（672 ns、CRuby 386 ns）、`Rational(a, b)` 生成（377 ns、CRuby 97 ns）、`Time` 生成の Rational / BigInt 経路 | sequel、mail、Time を使う全般 | 低〜中 | |
| 10 | sqlite3 の行読み出しを 1 回の builtin に（先行調査 §5.8） | sequel、activerecord | 中 | FFI 30 回/クエリ → 1 |

## 7. 計測手順

```sh
# 4 本の計測（gem は各 benchmarks/*/Gemfile を bundle install）
cd ../ruby-bench
MAX_TIME=30 ./run_benchmarks.rb --no-sudo --no-pinning --rss --harness=harness-warmup \
  -e "monoruby::/tmp/monoruby" -e "yjit::$(command -v ruby) --yjit" sequel mail liquid-il liquid-render
# JIT なし / CRuby インタプリタ
(cd benchmarks/liquid-il && MAX_TIME=15 /tmp/monoruby --no-jit -I ../../harness-warmup benchmark.rb)
(cd benchmarks/liquid-il && MAX_TIME=15 ruby -I ../../harness-warmup benchmark.rb)

# deopt / 再コンパイル / グローバルキャッシュ統計
cargo build --release --features profile
(cd benchmarks/liquid-render && MAX_TIME=8 target/release/monoruby -I ../../harness-warmup benchmark.rb)

# 関数別の内訳（perf のない環境）: 起動込み 1 反復
(cd benchmarks/liquid-il && MIN_ITERS=2 MIN_TIME=0 MAX_TIME=1 valgrind --tool=callgrind --smc-check=all \
   --callgrind-out-file=cg.out /tmp/monoruby -I ../../harness-warmup benchmark.rb)
callgrind_annotate --tree=caller cg.out     # 呼び出し元を辿る（classify ← compatible_encoding ← extend ← string_shl）
# 起動を除いた 1 反復 = 「ちょうど 2 反復」−「ちょうど 1 反復」の関数別差分
MIN_ITERS=1 MIN_TIME=0 MAX_TIME=1000000 MAD_TARGET=1e9 valgrind ... ; MIN_ITERS=2 ... ; 差分を取る
```
