# ruby-bench の fluentd / lobsters が完走しない問題の調査記録（2026-09）

ruby-bench の headline 2 本、fluentd と lobsters が monoruby で `exit 1` になる
問題を、CRuby 4.0.2 と突き合わせて追った。結論は「入口は C 拡張の LoadError、
その先に言語・組み込み側の不備が 12 件」で、後者はすべて直した（この記録と同じ
PR）。C 拡張の代替は未着手で、§4 に何が要るかをまとめる。
関連: `ruby_bench_failures_2026-09.md`（lobsters の起動順 `ripper → bcrypt →
markly → nokogiri` を記録した前回）、`nokogiri.md`（同じ手法で libxml2 を同梱した例）。

## 1. 症状と手順

どちらも `benchmark.rb` の冒頭、`require` の連鎖で `LoadError` になる:

| ベンチ | 最初に止まる場所 |
|---|---|
| fluentd | `fluent/config/literal_parser.rb` → `require 'yajl'` → `yajl/yajl.so` |
| lobsters | `Bundler.require` → `bcrypt.rb` → `bcrypt_ext.so` |

C 拡張の先を見るため、使い捨てのスタブ（yajl は JSON に委譲、msgpack は
純 Ruby の最小実装、cool.io / strptime / zstd-ruby は空の殻、bcrypt は
ベンチが使う 1 ユーザの digest を返すだけ、markly は段落をエスケープするだけ、
`SQLite3::Backup` は `ATTACH` してテーブルをコピー）を `-I` で差し込み、止まる
たびに原因を最小コードで切り分けて monoruby 側を直す、を繰り返した。
スタブは同梱していない（ベンチの意味が変わるため）。

## 2. 見つかった monoruby 側の不備（すべて修正済み）

| # | 症状 | 原因 | 修正 |
|---|---|---|---|
| 1 | fluentd: `include ::Socket::Constants` が `TypeError` | `Socket::Constants` を Class で定義していた | Module に（`socket.rs`） |
| 2 | lobsters: `require 'enumerator'` が `LoadError` | CRuby が起動時に `$LOADED_FEATURES` へ入れる組み込み feature 名（`enumerator.so` `fiber.so` `rational.so` `complex.so` `ruby2_keywords.rb` `set.rb`）が未登録。`fiber` は `stub/fiber.rb` が先に見つかって `true` を返していた | 初期リストに追加し、`require` は**全候補を loaded feature と照合してから**ロードパスを探すように（`globals.rs`, `require.rs`） |
| 3 | lobsters: `uninitialized constant StringScanner::Version` | rexml 3.4 が参照する定数が無い | `"3.1.6"` を追加（`stdlib/strscan.rb`） |
| 4 | lobsters: Rails の `MiddlewareStackProxy#use(...)` が登録したクラスが整数 120873 | `def m(...)` の中の **`-> { g(...) }`** から転送すると、lazy forwarding の marker（`Fixnum(callid)`）がそのまま渡る。`forwarding_no_escape` がブロックリテラルは escape 扱いするのにラムダリテラル（`BytecodeInst::Lambda`）を見ていなかった | `Lambda` も escape に（`encode.rs`） |
| 5 | lobsters: `class << self; delegate ...` で `Forwardable#instance_delegate` の arity エラー | **singleton class への `extend`** が、その singleton class 自身のメタクラスではなく `S(Class)`（全クラス共通）へモジュールを入れていた。`get_singleton` が「class フィールドが singleton なら返す」で、S(Foo).class == S(Class) の付き先を確かめていなかった | CLASS 型のオブジェクトは `get_metaclass` へ（`class.rs`） |
| 6 | lobsters: net-imap の `/#{Regexp.union(/[\xC2-\xDF][\x80-\xBF]/n, …)}+/` が `RegexpError: too short multibyte code string` | 動的 Regexp リテラルは連結後の String の encoding だけで判断していて、固定 encoding の Regexp 断片（BINARY）でピン留めしない。`Regexp.new("[\xC2-\xDF]".b)` も UTF-8 でコンパイルしていた | 断片に fixed-encoding の Regexp があればそれで固定（`runtime.rs`）。BINARY 文字列の `\xHH` エスケープは BINARY・ASCII エンジン（`regexp.rs`, `rvalue/regexp.rs`） |
| 7 | 同上の副産物: `/\xC3/n.match?("\xC3".b)` が `false` | `Regexp#match?` だけが非 UTF-8 の subject を lossy に読んでいた（`match` / `=~` はバイト経路） | バイト経路に（`regexp.rs`） |
| 8 | lobsters: `form_with` のビルダが `nil` | ActionView の `capture(*, **) { yield(*, **) }`: **匿名 `*` / `**` をブロック・ラムダの中から転送**すると depth 0（ブロック自身の枠）を読んでいた | 束縛したスコープの深さで解決（`prism_backend.rs`） |
| 9 | lobsters: `undefined method 'to_date' for Time` | `require "date"` が Time に足す `to_date` / `to_datetime` / `to_time` が無い。`Date#gregorian` 等の暦切替も無い（ActiveSupport の `Time#advance` は `to_date.gregorian.advance`） | 追加。`DateTime.new` の String オフセットと小数秒も正規化（`stdlib/date_core.rb`） |
| 10 | lobsters: `ActiveSupport::Cache::Coder#load` で `nil < 0` | **`unpack1("@3E")`** が `nil`。`unpack1` は解析したテンプレートを先頭 1 要素に切り詰めていて、位置指定（`@` `x` `X`）で止まっていた。さらに `offset:` 付きの `@n` を相対にしていた | 最初の値を出す指示までを残す。文字列全体を渡して `@n` は絶対位置に（`pack.rs`, `string.rs`） |
| 11 | lobsters: `Story#send_referrer?` の `created_at <= 1.hour` が `ArgumentError` | `Time#<` `<=` `>` `>=` が組み込みで、Time 以外を即拒否していた。CRuby は Comparable 経由なので ActiveSupport が再定義した `<=>` を通る。`Date#<=>` も Numeric と `coerce` を受けない。`Comparable#between?` は `min <=> self` を呼んでいた | 非 Time は `<=>` をディスパッチ（`time.rs`）、`Date#<=>` を CRuby の `d_lite_cmp` 相当に（`ajd` 追加）、`between?` を `self <=> min` に |
| 12 | lobsters: 終了時に `Exception in finalizer: undefined method 'close' for nil` ×N | monoruby の `object_id` はアドレスで、finalizer 登録済みオブジェクトが死ぬとそのアドレス（= id）が別オブジェクトに再利用される。Tempfile の `FinalizerManager` は `object_id` をキーにするので衝突 | 登録中のオブジェクトを registry が保持（mark）する。finalizer は終了時にしか走らないので観測可能な違いは無い（`globals.rs`, `kernel.rs`, `op.rs`） |

回帰テストは `tests/gem_boot_compat.rs`（CRuby oracle 比較、12 件）。

## 3. 到達点

スタブ込みで両方とも完走する（同一マシン、`-I harness`）:

| | monoruby | CRuby 4.0.2（YJIT なし） |
|---|---:|---:|
| fluentd（LTSV 25 万行 parse、1 反復） | 562 ms | 649 ms |
| lobsters（114 ルート、1 反復） | 2934 ms | 1657 ms |
| lobsters RSS | 1.09 GiB | 308 MiB |

lobsters の RSS 差は、finalizer 保持（§2 #12）ではなく元からの差（修正前の
1 回目の完走時点で 1.08 GiB）。速度差の内訳は未調査。

## 4. 残り: C 拡張の代替

ベンチをスタブ無しで走らせるには次が要る。いずれも monoruby がすでにやっている
2 通り（Ruby で書き直す / C ソースを `cc` で同梱して FFI を書く）のどちらか。

**fluentd** — 起動に必要で、ベンチの計測経路（`LabeledTSVParser#parse`）には
どれも乗らない:

- `yajl-ruby`: `Yajl::Parser` / `Encoder` の C クラス。JSON の上に Ruby で
  書ける（スタブがそれ）。
- `msgpack`: `Packer` / `Unpacker` / `Factory` / `Buffer` / `ExtensionValue`。
  gem の Ruby 側（`lib/msgpack/*.rb`）はこれらを reopen する。純 Ruby で 300 行
  程度（スタブは ~250 行で fluentd の起動を通した）。
- `strptime`: `Strptime` / `Strftime`。`Time.strptime` の上に書ける。
- `cool.io`: `Coolio::Loop` / `IOWatcher` / `TimerWatcher` / `StatWatcher`。
  起動時は定義だけで良いが、実際にイベントループを回す plugin_helper が使う。
  libev の同梱は大きい。
- `zstd-ruby`: `Zlib` と同じ形で libzstd を同梱するか、`compressable.rb` の
  ロードだけ通す殻か。

**lobsters**:

- `bcrypt`: `BCrypt::Engine.__bc_salt` / `__bc_crypt`。gem の `ext/mri/` は
  crypt_blowfish（C、約 1,800 行、public domain）で、`cc` 同梱が素直。ログイン
  1 回に使うだけ。
- `markly`: cmark-gfm（`ext/markly/`、C 約 1.1 MB）。`Markly::Parser` /
  `Node`（木の走査・編集・`to_html`）。`Markdowner` が本文の描画に毎リクエスト
  使うので計測経路に乗る。nokogiri と同じ形の同梱になる。
- `SQLite3::Backup`: `sqlite3_backup_init` / `step` / `finish` / `remaining` /
  `pagecount` の 5 関数。`src/builtins/sqlite3.rs` への追加で小さい。
