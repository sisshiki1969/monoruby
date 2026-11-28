# ruby/spec のハング対策: example 単位のタグ

monoruby が ruby/spec スイートのハングをどう避けているかをまとめる。

## 背景

外部の **rubyspec-stats** CI と、このリポジトリの spec-core / spec-library
ワークフローが ruby/spec を monoruby で定期実行し、passing/total の推移を
追跡している。*ハングする*（返ってこない）spec はカテゴリ実行全体を止めて
しまうので、除外が必要になる。

除外は mspec の **example 単位**のタグ（`spec/tags/`）で行う。spec ファイル
`<cat>/<name>_spec.rb` に対し、mspec は `tags/<cat>/<name>_tags.txt` を
読み込み、`fails:<full description>` の 1 行でその example 1 件だけを
`mspec ci`（または `--excl-tag fails`）が除外する。ファイルの残りは実行され、
集計に入る。ファイル単位の除外リスト（`spec/skip.txt`）は使わない。ファイルごと
除外すると全 example が分子・分母の両方から落ち、合格率を過小評価するためである。

方針は 2 つある。

- **タグはハング専用。** ハングしないセマンティクスの差は統計に出す。
- **陳腐化したタグは剪定する。** `fails:` タグは pass する example も集計から
  落とすので、残しておくと合格率を過小評価する。

## タグファイル形式

1 行 1 タグ: `<class>:<full description>`。

- `<class>` — `fails:` を使う（`mspec ci` が自動で除外する）。ハングは
  `critical(hangs):` でもよい。
- `<full description>` — ネストした `describe` / `context` の文字列と `it` の
  文字列を半角スペースで連結したもの。例:

  ```
  # spec/tags/core/io/select_tags.txt
  fails:IO.select returns supplied objects when they are ready for I/O
  ```

## mspec のタグパス解決

mspec は spec ファイルの**実パス**に `tags_patterns` を当ててタグファイルの
パスを導出する。config が設定しなければ組み込み既定の
`[[%r(spec/), 'spec/tags/'], [/_spec.rb$/, '_tags.txt']]` が使われる。

- **rubyspec-stats** は ruby/spec を `spec/ruby/` に clone するので、
  `spec/ruby/core/io/select_spec.rb` の導出先は
  `spec/tags/ruby/core/io/select_tags.txt`（`ruby/` セグメント入り）になる。
  setup action（`rubyspec: "true"`）は monoruby の `spec/tags/` を
  `spec/tags/ruby/` と `spec/ruby/tags/` の両方にコピーする。
- **spec-core / spec-library** ワークフローは ruby/spec を `../spec` に
  clone し、`spec/tags/` を `../spec/tags/` にコピーする。

ローカルで `spec/ruby` を ruby/spec の checkout への symlink にした
workspace では、タグを `<checkout>/tags/...` に置かないと "0 tagged" のまま
黙って全例が走る。CI と同じ 2 か所に置けば `mspec ci` が `N tagged` と報告する。

このリポジトリの `spec/default.mspec` は setup action がワークスペース直下の
`spec/` にコピーする設定ファイルで、`tags_patterns` は設定しない。中身は
example トレースで、各 example の description を実行直前に stderr へ出す。
mspec 自身の `--timeout` は対象プロセス内の監視スレッドなので、メイン
スレッドがカーネル内（`waitpid` 等）でブロックすると発火せず、外側の
`timeout(1)` に kill されて結果ファイルが空になる。トレースがあれば CI ログの
最後の description がハングした example であり、そのままタグに書ける。

## ハングする example の特定（ローカル）

現 master のバイナリ、sibling の `spec/`・`mspec/` checkout、`timeout` が
あれば再現できる。インストール済みの古いバイナリでは、関連する修正の有無で
結果が変わるので、必ず現 master をインストールしてから行う。

```sh
bin/install                                   # 現 master バイナリ
timeout 45 mspec run -t monoruby -fs <cat>/<name>_spec.rb
```

specdoc フォーマッタは各 example の説明を*実行前に*出力するので、timeout
直前の最後の `- …` 行がハングしている example である。exit code の意味:

| exit | 意味 |
| ---- | ---- |
| `0` / `1` | 完走（1 = failures/errors あり。それでも実行は完了） |
| `124` | **ハング** — `timeout` 発火。最後の `- …` 行が犯人 |
| `134` (SIGABRT) | monoruby が abort（`extern "C"` 境界での Rust panic など） |
| `143` (SIGTERM) | プロセスがシグナルで死亡（シグナル配送系 spec） |

複数の example がハングするファイルでは、見つけた example にタグを付け、
タグを有効にして（`mspec run -fs --excl-tag fails …` または `mspec ci …`）
再実行し、ファイルが完走するまで繰り返す。

> バッファリングの罠: mspec を `timeout` 下で `grep` / `tail` にパイプすると、
> SIGTERM でバッファ済みの stdout が失われることがある。ファイルへ
> リダイレクトしてから読み直すこと。

GitHub runner の上でだけ起きるハングもある（fork した子を `Process.wait`
する spec で過去に観測した）。ローカルで通ることはタグを外す根拠にならず、
外すときは CI で確かめる。

## タイムアウトの自動検知とタグ付け（spec-core モニタ）

`.github/workflows/spec-core.yml` と `.github/scripts/bisect-spec-timeouts.sh`
がハングする example の特定を自動化している。

1. **モニタ自身がタグを適用する。** `spec/tags/` を ruby/spec の checkout へ
   コピーし、全ランを `--excl-tag fails` で実行する。タグ済みの example が
   毎回ファイル予算を食い潰すのを防ぐ。
2. **検知。** ファイル単位の `timeout -k 5 60` で完走しなかったファイルが
   `timeouts.csv` に記録される。
3. **同定。** `bisect-spec-timeouts.sh` が各タイムアウトファイルの example
   一覧を `mspec --dry-run -f s` で取得し（describe の連結 = タグ名）、
   1 example ずつ `timeout -k 5 60` 付きで個別に実行する。
   - exit 124/137 → `hang`（真のハング）
   - 単独で 30 秒以上 → `slow`（単独で予算を圧迫する）
   - どちらでもない → `cumulative-only`（合算での超過。タグ付けせず、予算の
     見直し対象として報告のみ）
   - dry-run にも 60 秒、1 ファイルの bisect に 900 秒の上限がある。
4. **タグ更新と PR。** 見つかった犯人を `spec/tags/<cat>/<file>_tags.txt` へ
   重複排除で追記し、差分があれば固定ブランチ `auto/spec-timeout-tags` に
   commit して PR を作る（既存の PR があれば同じ PR を更新する）。**マージは
   人間が判断する**: タグが妥当（既知の制限）か、退行として直すべきかを
   レビューする。結果の表はランの summary、PR の本文、artifact
   （`timeout_culprits.{csv,md}`）に出る。

rubyspec-stats の run が空の結果を公開した場合は
`.github/scripts/rubyspec_stats_watch.rb` が issue を立て、次の run が
空でない結果を公開すると閉じる。

## 現在のタグ

`spec/tags/` には 1 ファイルだけがある。

- `core/refinement/import_methods_tags.txt` — `Refinement#import_methods when
  methods are not defined in Ruby code raises ArgumentError when importing
  methods from C extension`。ハングではなく、spec の前提が成り立たなかった
  ために付けたタグである。この spec は「C 拡張のメソッドは import できない」
  ことを `Zlib` で確かめるが、タグを付けた時点の monoruby の `Zlib` は Ruby
  実装だったので import が成功していた。現在の `Zlib` は CRuby の
  `ext/zlib` を C API 層の上でビルドした `zlib.so`（`doc/c_api.md`）で、
  `import_methods` は iseq でないメソッドを拒否する（`builtins/module.rs`）
  ので、この example は通るはずである。CI で確かめてからタグを外す候補である。
