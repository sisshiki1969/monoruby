# ruby/spec ハング対策: `skip.txt` → `tags/` 移行

monoruby が ruby/spec スイートのハングをどう回避しているか、そして粗い
ファイル単位スキップリストを「本当に救えない5ファイル」まで絞り込んだ監査の
記録。`#899` 以降の単一スレッド化ランタイムを前提とする（最新反映時点）。

## 背景

外部の **rubyspec-stats** CI が ruby/spec を monoruby に対して定期実行し、
passing/total の推移を追跡している。*ハングする*（返ってこない）spec は
カテゴリ実行全体を止めてしまうため除外が必要。除外手段は2つある。

- **`spec/skip.txt`** — **ファイル単位**の粗い除外リスト
  （`spec/ruby/core/thread/backtrace_spec.rb` のようなパス）。ファイルごと
  除外すると*全 example* が分子・分母の両方から落ち、真の合格率を過小評価する。
- **`spec/tags/`** — mspec ネイティブの **example 単位**タグ機構。spec ファイル
  `<cat>/<name>_spec.rb` に対し、mspec は `tags/<cat>/<name>_tags.txt` を
  自動読み込みする。`fails:<full description>` の1行で、その example 1件だけを
  `mspec ci` が除外し、ファイル内の残りは実行・カウントされる。

本移行の目的: *少数*の特定 example だけでハングするファイルについて、
ファイル単位スキップを example 単位の `fails:` タグへ置き換え、生き残る
example を統計へ復帰させること。

### タグファイル形式

1行1タグ: `<class>:<full description>`。

- `<class>` — `fails:` を使う（`mspec ci` が自動除外）。
- `<full description>` — ネストした `describe`/`context` 文字列と `it` 文字列を
  半角スペースで連結したもの。例:

  ```
  # spec/tags/core/io/select_tags.txt
  fails:IO.select returns supplied objects when they are ready for I/O
  fails:IO.select returns the pipe read end in read set if the pipe write end is closed concurrently
  ```

### mspec のタグパス解決

`spec/default.mspec` の `tags_patterns` により、spec リポジトリのルート基準で
`core/thread/backtrace_spec.rb` → `tags/core/thread/backtrace_tags.txt` と
変換される。rubyspec-stats CI は monoruby の `spec/tags/` を spec チェックアウト
側へ配置して mspec に読ませる。ローカル検証では symlink で同じ配線を再現する。

```sh
ln -sfn /path/to/monoruby/spec/tags /path/to/spec/tags
```

## bisect 手法（ローカル）

CI には "Bisect monoruby core hang" ワークフローがあるが、監査全体は
現 master バイナリ・sibling の `spec/`・`mspec/` チェックアウト・timeout が
あればローカルで再現できる。

**重要:** bisect は必ず*現 master* バイナリで行うこと。インストール済みの
リリースは関連修正より古い場合がある。例えば `#899`（「ruby/spec のハングを
止めるため単一スレッドの最小面へ削減」）は多くのハングを解消したため、古い
バイナリはハングを見落とすと同時に、無いはずのハングを作り出す。

```sh
cargo install --path monoruby --force        # 現 master バイナリ
```

ファイルごとに specdoc フォーマッタ + timeout で実行する。specdoc は各 example
の説明を*実行前に*出力するので、**timeout 直前の最後の `- …` 行がハングしている
example** になる。

```sh
timeout 45 mspec run -t monoruby -fs <cat>/<name>_spec.rb
```

exit code による分類:

| exit | 意味 |
| ---- | ---- |
| `0` / `1` | 完走（1 = failures/errors あり。それでも実行は完了） |
| `124` | **ハング** — `timeout` 発火。最後の `- …` 行が犯人 |
| `134` (SIGABRT) | monoruby が abort（`extern "C"` 境界での Rust panic） |
| `143` (SIGTERM) | プロセスがシグナルで死亡（シグナル配送系 spec） |

**複数**の example がハングするファイルは反復する: 見つけた example の
`fails:` タグを追加し、タグを有効にして再実行
（`mspec run -fs --excl-tag fails …` または `mspec ci …`）して次を炙り出し、
ファイルが完走するまで繰り返す。

> バッファリングの罠: mspec を `timeout` 下で `grep`/`tail` にパイプすると、
> SIGTERM でバッファ済み stdout が失われることがある。ファイルへリダイレクト
> してから読み直すこと。

## 監査結果（元の `skip.txt` 40ファイル）

| 分類 | 件数 | 処置 |
| ---- | ---- | ---- |
| **HANG**（少数 example） | 6 | 5件を `fails:` タグへ移行、1件は skip 維持 |
| **COMPLETED**（もうハングしない） | 30 | skip から除去、タグ不要 |
| **CRASH — SIGABRT** | 1 | skip 維持（monoruby バグ） |
| **CRASH — SIGTERM** | 3 | skip 維持（シグナル配送） |

大きな COMPLETED バケットが最大の発見: **`skip.txt` は陳腐化していた** —
`#899` 以降、リストの4分の3はもうハングしない（代わりに即エラー/失敗で完走）
ため、盲目的に skip 維持することが合格率を過小評価していた。

### タグへ移行（6ファイル）

| spec ファイル | タグ付けした example |
| ------------- | -------------------- |
| `core/argf/read_spec.rb` | `ARGF.read reads the contents of a special device file`（`/dev/zero` を `read(100)` — 長さ制限を無視して無限読み込み） |
| `core/argf/readlines_spec.rb` | `ARGF.readlines returns an empty Array when end of stream reached` |
| `core/io/copy_stream_spec.rb` | `IO.copy_stream with a destination that does partial reads calls #write repeatedly on the destination Object` |
| `core/io/select_spec.rb` | `IO.select returns supplied objects when they are ready for I/O` **と** `IO.select returns the pipe read end in read set if the pipe write end is closed concurrently` |
| `library/socket/socket/tcp_server_loop_spec.rb` | `Socket.tcp_server_loop when a connection is available yields a Socket and an Addrinfo` |
| `library/socket/socket/udp_server_loop_spec.rb` | `Socket.udp_server_loop when a connection is available yields the message and a Socket::UDPSource` |

各ファイルは `mspec ci`（`fails:` タグを除外）で完走し、残りのハングが無いことを
検証済み。

### `skip.txt` に維持（5ファイル）

タグでは救えないもの — プロセスごと死ぬか、全 example がハングする:

| spec ファイル | 理由 |
| ------------- | ---- |
| `core/enumerator/new_spec.rb` | `builtins::array::eq`（`Array#==`）での非巻き戻し Rust panic → SIGABRT。隠すのではなく修正すべき monoruby バグ。 |
| `core/exception/signal_exception_spec.rb` | 実際にシグナルを配送 → SIGTERM でプロセス死 |
| `core/exception/signm_spec.rb` | 先頭 example で死亡（SIGTERM） |
| `core/exception/signo_spec.rb` | 先頭 example で死亡（SIGTERM） |
| `library/expect/expect_spec.rb` | 6 example 全てが `IO#expect` でブロック。全部タグ付けはファイルごと skip と等価 |

## 解除した30件の検証

`mspec ci` はカテゴリ内のファイルを**1プロセス**で順次実行するため、以前
スキップされていたファイルがグローバル状態（fd, `at_exit`, シグナル trap,
Mutex/Queue/Thread の状態）を残し、*後続*ファイルをハングさせる恐れがある。
これが起きないことを2段階で確認した。

1. **解除30ファイルを1つの `mspec ci` プロセスで実行** — 約35秒で完走、
   815 examples、ハング0。
2. **状態依存が強いカテゴリの全体実行**（未スキップの隣接 spec も込み）:
   `core/thread`（全53ファイル）・`core/mutex`・`core/queue`・
   `core/sizedqueue` — 全て完走、ハングなし。

単一スレッド化（`#899`）により残留バックグラウンドスレッド起因のハングは
起こりにくく、この結果と整合する。I/O 依存の非常に大きいカテゴリ
（`core/io`・`core/kernel`・`core/file`）は全体実行していない — 無関係な
ブロッキング spec が誤検知ハングを生むため。該当ファイル群は検証(1)で
カバー済み。

## 最終状態

- `spec/skip.txt`: **40 → 5** ファイル（救えない CRASH/全ハング集合のみ）。
- `spec/tags/`: `*_tags.txt` 6ファイル、タグ付き example は計7件
  （rubyspec-stats の再監査で `core/argf/read_spec.rb` の `/dev/zero` 無限読み
  込みを追加。それ以外の「まだハングする」報告6件はタグ適用（`--excl-tag
  fails`）で完走することを再確認済み — `library/cgi/unescapeURIComponent_spec.rb`
  はそもそもハングせず約0.35秒で完走）。

## フォローアップ

- **`core/enumerator/new_spec.rb`** — `builtins::array::eq` の非巻き戻し panic を
  修正し、`skip.txt` から外す。
- **signal 系 spec** — 先頭のプロセス死 example をタグ付けすれば残りが走るか
  調査し、3件の skip をタグへ変換できるか検討する。
- **`core/io` の fd 二重クローズ crash** — `core/io` をカテゴリ一括実行すると
  `fatal runtime error: IO Safety violation: owned file descriptor already
  closed` で abort する（上記「I/O 依存の大きいカテゴリは全体実行していない」
  の実体）。abort 地点はプロセス内に蓄積した fd 状態に依存して移動する
  （`open`/`new` を除外しても example 448→888 へずれるだけ）ため、**特定ファイルの
  skip では回避できない**。monoruby の IO fd 所有権（raw close と Rust の
  `OwnedFd` Drop の二重解放）を修正すべき本体バグ。tag/skip 対象ではない。
