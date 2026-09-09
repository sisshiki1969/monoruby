# ruby-bench で失敗する 4 本（liquid-c / hexapdf / lobsters / splay）の調査記録（2026-09）

ポータルで「失敗」と出ていた 4 本を、`run_benchmarks.rb --harness=harness-warmup`
と同じ条件（`-I harness-warmup benchmark.rb`）で monoruby と CRuby 4.0.2+YJIT の
両方で走らせて分類した。結論は「例外で落ちるもの 2 本、遅くてハーネスの収束判定に
かからないもの 2 本」で、後者 2 本に対して GC のマークと組み込みメソッドを直した。
関連: `ruby_bench_low_cost_ideas_2026-09.md`（前回の全体像）、`gc.md` §6.3/§6.4。

## 1. 分類

| ベンチ | 症状 | 原因 | 対処 |
|---|---|---|---|
| liquid-c | `LoadError: liquid_c` | C 拡張（`liquid_c.so`）。純 Ruby の代替なし | 対象外（下記 §4） |
| lobsters | 起動時 LoadError | `ripper` → 対処後は `bcrypt` / `markly` / `nokogiri`（すべて C 拡張） | Ripper は解決、残りは対象外 |
| hexapdf | exit 0 だが 1 反復 2991 ms（YJIT 1493）、MAD 3–7% | JIT の有無で速度が変わらない＝組み込み側のコスト | 組み込み 3 点を改善（§3）→ 2708 ms |
| splay | exit 0 だが 1 反復 153 ms（YJIT 66）、MAD 40% | GC。GC の無い反復は CRuby と同速 | マークを 2 倍速く（§2）→ 119 ms |

### 「タイムアウト」の正体

`harness-warmup` は反復時間の MAD（中央絶対偏差）が閾値（0.1% + 0.01%/反復）を
下回るまで回し、`MAX_TIME`（既定 20 分）で打ち切って `timed out after N seconds`
と出す。**打ち切っても結果は出て exit 0** なので、ポータル上の「失敗」は
これとは別の（ポータル側の）時間制限に当たったもの。

- hexapdf: monoruby は反復ごとの GC 回数が 91–117 回（CRuby 16 回）で MAD が
  5–7%、収束に 700 反復 ≈ 35 分かかるので 20 分では収束しない。CRuby は MAD 3.9%
  で約 10 分で収束する。
- splay: 反復時間が「GC なし 55–80 ms」と「GC あり 140–240 ms」の二峰で、MAD 40%。
  **CRuby も MAD 22% で収束しない**（60 秒制限で `timed out`）。

## 2. splay: GC のマークが 1 オブジェクト 60–90 ns だった

`GC::Profiler` と一時的なフェーズ計測（`perf` はコンテナで使えない）で分解:

| フェーズ（マイナー GC 1 回） | 変更前 | 変更後 |
|---|---|---|
| マーク走査（remembered set から + drain） | 44–113 ms、**57–75 ns/obj** | **28–35 ns/obj** |
| `apply_aging`（生存者全員を 2 度目に舐める） | 9–45 ms（20–30%） | 1–20 ms（昇格分のみ） |
| sweep | 1–20 ms（メジャー 34–48） | 同じ |

原因は 1 オブジェクトあたり 1 回の DRAM ミスが直列化していたこと。`RValue::mark` が
子へ再帰する前にヘッダ（`is_live` / `is_promotable`）を読むので、次のアドレスが
分かるのはミスが完了した後だった。

変更（PR #1300、`alloc.rs`）:

1. `Allocator::mark` はページのビットマップだけ触ってキューに積む。ヘッダは
   `drain_mark_queue` が取り出すときに読み、その時点で `MARK_PREFETCH_DISTANCE`
   （8）個先のエントリを `prefetch` しておく。32 段までの再帰（`MARK_RECURSION_LIMIT`）
   は廃止。キューだけ先読みしてヘッダはマーク時に読む版は効果ゼロだったので、
   ヘッダ読みを drain に移すことが本体。
2. 加齢・昇格も drain で、取り出した生ポインタ経由で行う（`&T` を作る前なので
   エイリアスしない）。昇格したものだけ `promoted` に残し、マーク後の
   `remember_promoted` で remembered / armed に分類する。
3. 年齢を 2 枚のページビットマップに持つ版も試したが同じだったので不採用。

結果: splay 12 反復の GC 合計 1415 → 714 ms。ハーネス中央値 160 → 119 ms
（CRuby 66）。他: gcbench 539 → 505、binarytrees 80 → 77、erubi 220 → 214、
nbody 16 → 15、psych-load ±0（1580/1582 vs 1596/1541）。

残り: GC ありの反復はまだ CRuby より遅い。総マーク量は割り当て量 × 約 2.6
（3 回生存で昇格）で CRuby と同じなので、次は 1 マークあたりの残り 28 ns
（ヘッダ行 + ivar 走査）か、GC 1 回の固定費（root 走査・sweep）。

## 3. hexapdf: JIT は関係なく、組み込み 3 点

`--no-jit` でも 2713 ms（JIT 2678 ms）なので Ruby コードの実行速度ではない。
gdb を約 200 回アタッチしてネイティブスタックを集計（`perf` 代替）:

| 自己時間 | 関数 |
|---|---|
| 20% | JIT コード（シンボル無し） |
| 20% | GC（`execute_gc` 以下） |
| 10% | `SmallVec<[Value;5]>::extend`（`Array#[]` のスライス）+ その下の malloc 8% |
| 7% | `regexp::match_`（`captures_from_pos`） |

マイクロベンチで CRuby と比べると、hexapdf が使う他の操作（`Class#new`、ivar
アクセサ、`Array#each/map/sum`、`Hash#[]`、`Fiber#resume`、`dup`）は monoruby の
方が速く、遅いのは次の 3 つだけだった（PR #1301）:

| 操作 | CRuby | 変更前 | 変更後 |
|---|---|---|---|
| `String#codepoints`（172 バイト） | 2.9 µs | 28 µs（Ruby 実装） | 0.56 µs（Rust） |
| `a200[10..]` | 87 ns（共有配列で O(1)） | 206 ns（要素ごと push） | 188 ns（memcpy） |
| `Regexp#match?`（1 文字） | 158 ns | 240 ns（キャプチャ付き検索） | 195 ns（region なし） |

結果: 2991 → 2708 ms。残りの差（1.8x）は GC の回数・割り当て・メソッドディスパッチ・
JIT コードに分散していて、5% を超える単独項目は無い。配列スライスの残り差は
CRuby の共有配列（コピーしない）によるもので、別の設計変更になる。

## 4. lobsters / liquid-c: C 拡張

- liquid-c は `liquid_c.so`（Liquid のパーサ・レンダラの C 実装）そのもの。
  純 Ruby の liquid で代替すると liquid-render と同じものになり、ベンチの意味が
  変わるので対象外。
- lobsters（Rails 8.1）は起動順に `ripper` → `bcrypt` → `markly`（cmark）→
  `nokogiri`（libxml2）。`ripper` だけは Rails が無条件に require して
  サブクラスを定義するだけなので、prism の Ripper 翻訳層で解決した（§5）。
  残り 3 つは C 拡張で、nokogiri は rails-html-sanitizer 経由で railsbench も
  同じ場所で止まる。Fiddle で libxml2 を包む規模ではない。

## 5. Ruby レベルの Prism（Ripper の代替）

`require "prism"` は gem の C 拡張か ffi gem を要求するので monoruby では
失敗していた。monoruby は libprism（`ruby-prism` クレート、1.9.0）を静的リンク
しているので、シリアライザ（`pm_serialize_parse` 等）を `Prism.__serialize` として
出し（`src/builtins/prism.rs`）、gem の FFI バックエンドと同じ組み立て
（`gem/prism/prism.rb`）で純 Ruby の `Prism::Serialize` にノード木を作らせる。
Ruby 4.0.2 の既定 prism は 1.8.1 でシリアライズ形式が違うため、1.9.0 gem の
Ruby 側を `gem/prism/` に同梱している（クレート更新時は再同梱が要る）。

`stdlib/ripper.rb` は `Prism::Translation::Ripper`（gem 同梱、純 Ruby）に委譲。
`Ripper.sexp` / `sexp_raw` / `lex` / `tokenize` と `on_*` サブクラスは Rails や
render parser が使う範囲で CRuby の Ripper と同じ出力になる。翻訳なので
`on_sp` / `on_nl` / 区切り記号など一部イベントは発火しない。

副産物として言語側のバグを 2 つ直した: `#to_ary` によるブロック引数の自動展開が
(a) `respond_to?` を持たない `BasicObject` 派生（`respond_to_missing?` +
`method_missing` で答える prism の `LexCompat::Token`）で行われない、(b) `Proc#call`
や組み込みからの直接呼び出し（`Array#to_h` など）で行われない。

これで Rails は Ripper ではなく `PrismRenderParser` を使う。rubocop / ruby-lsp は
それぞれ別の障壁（bundler の `benchmark` gem バージョン衝突、`rbs_extension`
C 拡張）で止まっていて、Prism だけでは動かない。

## 6. 測定メモ

- `perf` は `perf_event_paranoid=2` とカーネル用バイナリの不在で使えない。
  `valgrind --tool=callgrind` は monoruby の brk 領域で落ちる。gdb の繰り返し
  アタッチ（`bt` 集計）が唯一のネイティブプロファイル手段だった。
- green thread の `Thread#backtrace` によるサンプリングはセーフポイントの位置に
  偏る（メソッド入口に集中）ので、時間配分の指標にはならない。
