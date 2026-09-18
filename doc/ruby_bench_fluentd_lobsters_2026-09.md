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
どれも乗らない。**5 つとも実装済み**（`monoruby/gem/` の stand-in、いずれも
本物の gem を CRuby で動かした結果と突き合わせる `tests/{msgpack,yajl,strptime,
coolio,zstd}.rs` 付き）。`-I stubs` 無しで `benchmarks/fluentd/benchmark.rb`
が最後まで走る:

- `yajl-ruby` → `gem/yajl/yajl.rb`: yajl 1.x の字句解析器・パーサ状態機械・
  ジェネレータと gem のビルダ callback を Ruby に移植。エラー文（`(right
  here) ------^` の整形まで）、複数値ストリーム、`on_parse_complete`、
  `pretty` / `html_safe` / `entities` / `terminator`、`to_json` 経由の
  オブジェクト、数値は callback を発火しないという癖も同じ。
- `msgpack` → `gem/msgpack/msgpack.rb`: `Buffer` / `Packer` / `Unpacker` /
  `Factory` / `ExtensionValue` と例外クラス。型ディスパッチ（core クラスそのもの
  は拡張型を引かない）、文字列の encoding による str / bin 選択、拡張型レジストリ
  の探索順、再帰拡張型、resumable な unpacker（`feed_each` でチャンクを跨ぐ）、
  frozen な Packer / Unpacker（`Factory::Pool`）、`skip_nil` が消費しない癖。
- `strptime` → `gem/strptime/strptime.rb`: `strptime.c` / `strftime.c` の移植。
  対応 directive だけを受け付け（他は `invalid format`）、幅までの桁読み、月名
  の大文字小文字無視、未指定フィールドの既定値、`%z` の有無で固定オフセット /
  UTC / ローカル。
- `cool.io` → `gem/cool.io_ext.rb`: libev の代わりに `IO.select` で回すループ。
  `@watchers` / `@active_watchers` の帳簿、`attached?` が 0 を返す癖、attach 済み
  watcher の再 attach が ArgumentError になる癖、`TimerWatcher#reset`、
  `StatWatcher` のポーリング、`Buffer`（`read_from` / `write_to` は nonblock）。
- `zstd-ruby` → `gem/zstd-ruby/zstdruby.rb` + `src/builtins/zstd.rs`
  （`String.__zstd_*`、zstd-sys 同梱の libzstd 1.5.7 = gem が link する版）。
  `Zlib` と同じ handle 方式で、`ZSTD_compress2` / `compressStream2` を拡張と
  同じ手順で呼ぶので圧縮結果はバイト単位で一致する。`CDict` / `DDict`、
  `StreamingCompress` / `StreamingDecompress`、skippable frame。

**lobsters**:

- `bcrypt`: `BCrypt::Engine.__bc_salt` / `__bc_crypt`。gem の `ext/mri/` は
  crypt_blowfish（C、約 1,800 行、public domain）で、`cc` 同梱が素直。ログイン
  1 回に使うだけ。
- `markly`: cmark-gfm（`ext/markly/`、C 約 1.1 MB）。`Markly::Parser` /
  `Node`（木の走査・編集・`to_html`）。`Markdowner` が本文の描画に毎リクエスト
  使うので計測経路に乗る。nokogiri と同じ形の同梱になる。
- `SQLite3::Backup`: `sqlite3_backup_init` / `step` / `finish` / `remaining` /
  `pagecount` の 5 関数。`src/builtins/sqlite3.rs` への追加で小さい。

## 5. lobsters の C 拡張代替 3 点の実装（2026-09-17）

§4 の lobsters 側 3 点を実装し、`benchmarks/lobsters/benchmark.rb` がスタブ無しで
最後まで走る（114 ルートすべて 200、25 反復）。いずれも本物の gem を CRuby で
動かした結果と突き合わせるテスト付き。

- `SQLite3::Backup` → `src/builtins/sqlite3.rs` に `ObjTy::NATIVE` クラスとして
  追加（`initialize` / `step` / `finish` / `remaining` / `pagecount`）。
  `sqlite3_backup_init` の失敗は接続側の errcode で gem のコード別例外クラスに
  変換する（`unknown database nope` → `SQLException`）。`tests/sqlite3.rs`
  `sqlite3_backup`。
- `bcrypt` → C ソース同梱ではなく `bcrypt` crate（crypt_blowfish 互換）を使い、
  `src/builtins/bcrypt.rs` が `String.__bcrypt_salt` / `__bcrypt_crypt` を、
  `gem/bcrypt_ext.rb` が `BCrypt::Engine.__bc_salt` / `__bc_crypt` を与える。
  同じ salt なら 60 バイトのハッシュがバイト単位で一致する（salt の 22 文字目の
  正規化、72 バイト打ち切り、NUL を含む文字列の `ArgumentError` も同じ）。
  `tests/bcrypt.rs`。
- `markly` → cmark-gfm 同梱ではなく `comrak` crate（cmark-gfm 互換の Rust 実装）
  を使う。`gem/markly/markly.rb` が `Markly::Parser` / `Node` / `Error` /
  `Markly.extensions` を Ruby で定義し、木は Ruby 側の双方向リンク（cmark と同じ
  `parent` / `first_child` / `next` …と `can_contain` の規則）で持つ。
  `src/builtins/markly.rs` は木を入れ子 Array に写して parse / render する
  （`String.__markly_parse` / `__markly_render_{html,commonmark,plaintext}`）。
  HTML 出力は cmark-gfm と一致（GFM 拡張 5 種、SMART / UNSAFE / HARD_BREAKS /
  FOOTNOTES / SOURCE_POSITION / GITHUB_PRE_LANG / FULL_INFO_STRING）。
  `to_plaintext` は `plaintext.c` の移植。`to_commonmark` は comrak 自身の
  整形（箇条書きのインデント、fence 後の空白、hard break の `\`）で、gem が
  `dup` に使う再パースには影響しない。リストの終端 source position も comrak
  のもの。`tests/markly.rs`。

副産物: String のサブクラスが定義した `==` が `str == x` / `str != x` で呼ばれて
いなかった（VM の `eq_values` は `String#==` の中身を直接走らせ、JIT が解決する
`String#!=` の本体も同じ）。`BCrypt::Password == secret` がこれで常に false に
なっていた。`Executor::eq_values_vis` と `String#!=` を、受け手が `String` そのもの
でなければ `==` の探索に回すよう修正（CRuby の `opt_eq` fast path も `rb_cString`
限定）。`tests/builtin_operator_binding.rs` `string_subclass_eq_is_dispatched`。

fluentd は現行 master でそのまま完走する（io-event の C 拡張が無い旨の warning
だけが出る）。

