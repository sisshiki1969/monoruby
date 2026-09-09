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

## 6. railsbench: 起動から 2000 リクエスト完走まで

lobsters と同じ Rails 8.1 のアプリで、Prism（§5）の後に何が要るかを、
`nokogiri` だけを使い捨ての空モジュールで差し替えて追った（`require` を
通すためだけの探索用スタブで、同梱はしていない）。順に当たったのは次の 7 点で、
すべて monoruby 側の不備だった。

| 順 | 症状 | 原因 | 対処 |
|---|---|---|---|
| 1 | `Errno is not a module`（webrick/compat.rb） | `Errno` をクラスとして定義していた | モジュールに（`errno.rs`、`builtins/error.rb`） |
| 2 | `uninitialized constant ApplicationController`（Zeitwerk eager load） | `const_get(name, false)` が autoload 先のファイルで起きた例外を握りつぶして NameError にしていた | `lookup_constant_path` が例外を伝播（`module.rs`） |
| 3 | `undefined method 'anonymous?' for nil`（ParamsWrapper） | `Struct#to_h` / `to_a` / `[]` / `each` … がメンバのアクセサ経由で読んでいた。CRuby は生スロット（`RSTRUCT_GET`）を読むので、アクセサを上書きしたサブクラスで挙動が違う | `__slot_get` / `__slot_set` 組み込みを追加し `builtins/struct.rb` を全面的にそれ経由に |
| 4 | `database configuration does not specify adapter` | YAML のマージキー `<<: *default` 未対応（`"<<"` というキーになっていた）。`key: &a {…}` のようにアンカー付きのフロー値も文字列になっていた | `store_pair`（マージキー、`Hash#merge!` 意味論、配列形も）とアンカー付き値の一般化（`stdlib/psych.rb`） |
| 5 | `undefined method 'add_builtin_type' for Psych` | ActiveSupport の `omap` 登録 | `add_builtin_type` / `add_domain_type` / `safe_dump` を追加、`add_tag` のテーブル向きを Psych と同じに |
| 6 | `OpenSSL::Digest::SHA256 is expected to implement hexdigest` | `stdlib/openssl.rb` の Digest / HMAC / KDF が空のスタブだった | 実装に置き換え（下記） |
| 7 | `undefined method 'key_len' for OpenSSL::Cipher`（flash → セッション Cookie の暗号化） | `OpenSSL::Cipher` も空のスタブ | AES-GCM / AES-CBC を Rust で実装（下記） |
| 8 | `stackprof.so` の LoadError | `gem "stackprof", platforms: :mri` は monoruby（`RUBY_ENGINE == "ruby"`）でも `Bundler.require` される | `gem/stackprof/stackprof.rb`（API だけの不活性版、`start` は false を返して一度だけ警告） |

### OpenSSL を本物にした範囲

- `OpenSSL::Digest`（`< ::Digest::Class`、`SHA1` / `SHA256` / `SHA384` /
  `SHA512` / `MD5`、`new("sha256")` / `new("SHA-256")`）、`OpenSSL::HMAC`
  （`digest` / `hexdigest` / `base64digest`、インスタンス API）、
  `OpenSSL::PKCS5.pbkdf2_hmac`、`OpenSSL::KDF.pbkdf2_hmac` / `hkdf`。
  いずれも monoruby の `Digest`（`String.__digest`、Rust の sha2 / md-5）の上の
  純 Ruby で、出力は CRuby の openssl とバイト単位で一致する
  （`tests/openssl_digest.rs`）。
- `OpenSSL::Cipher`: `aes-{128,192,256}-{gcm,cbc}`。`src/builtins/cipher.rs` が
  RustCrypto の `aes-gcm` / `aes` + `cbc` クレートで一括処理し（`update` は
  バッファするだけで `final` で計算）、`AuthTagError` / `CipherError`、
  `AES256.new(:GCM)` などのクラス形も openssl と同じ（`tests/openssl_cipher.rs`）。
  Rails の `MessageEncryptor`（Cookie）と ActiveRecord Encryption が使う範囲。
  `padding = 0` は未対応。
- `Digest::Class` が `Digest::Instance` を include するようにした（CRuby と同じ
  祖先順）。

### 結果

`nokogiri` の探索スタブ込みで railsbench（2000 リクエスト × 反復）が完走する:

| | 1 反復 | RSS |
|---|---|---|
| CRuby 4.0.2（YJIT なし） | 4063 ms | 115 MiB |
| monoruby | 3623–3732 ms | 350 MiB |

### 残り: nokogiri

`actiontext`（`require "nokogiri"` 無条件）と `rails-html-sanitizer` → `loofah`
（`Nokogiri.uses_gumbo?` 等を読み込み時に呼ぶ）が要求する。libxml2 + gumbo の
C 拡張で、Fiddle で包む規模ではなく、読み込みだけ通す偽物は HTML を解析しないので
同梱しない。railsbench 自身のビュー（scaffold の posts）は sanitize を使わないため、
本物の nokogiri があれば残りはそのまま動く。CRuby 側との出力差（起動時に
`character class has duplicated range` の警告が出る、CRuby は `-w` 時のみ）は
別件として残す。

## 7. 測定メモ

- `perf` は `perf_event_paranoid=2` とカーネル用バイナリの不在で使えない。
  `valgrind --tool=callgrind` は monoruby の brk 領域で落ちる。gdb の繰り返し
  アタッチ（`bt` 集計）が唯一のネイティブプロファイル手段だった。
- green thread の `Thread#backtrace` によるサンプリングはセーフポイントの位置に
  偏る（メソッド入口に集中）ので、時間配分の指標にはならない。
