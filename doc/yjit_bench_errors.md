# yjit-bench Error Investigation

monoruby を `https://github.com/Shopify/yjit-bench` の全ベンチマークに
対して `target/release/monoruby <benchmark.rb>` 形式で実行し、エラー
終了したものをカテゴリ別に分類した。

- monoruby: branch `claude/investigate-yjit-bench-errors-TiRVg`, `cargo build --release`
- yjit-bench: master (cloned at `/home/user/yjit-bench`)
- host: x86-64 Linux, system Ruby = **3.3.6** (rbenv), Ruby 4.0+ は未インストール
- benchmark 実行時の harness 環境変数: `WARMUP_ITRS=1 MIN_BENCH_ITRS=1 MIN_BENCH_TIME=1`, タイムアウト 60s

## Summary

| Result | 件数 |
|---|---|
| 成功 (`rc=0`) | 40 |
| 失敗 (`rc=1`) | 30 |
| タイムアウト (`rc=124`) | 3 |
| **合計** | **73** |

### 成功したベンチマーク (40)

純粋な Ruby のみで `harness/loader.rb` のみを require する単体スクリプトはほぼすべて完走した。
gem 依存があってもベンダリングされていれば動く。

```
30k_ifelse, 30k_methods, attr_accessor, binarytrees, blurhash, cfunc_itself, etanni,
fannkuchredux, fib, gcbench, getivar, getivar-module, gvl_release_acquire, keyword_args,
knucleotide, loops-times, matmul, nbody, nqueens, object-new, object-new-initialize,
object-new-no-escape, optcarrot, protoboeuf, protoboeuf-encode, respond_to, ruby-xor,
rubykon, send_bmethod, send_cfunc_block, send_rubyfunc_block, setivar, setivar_object,
setivar_young, splay, str_concat, structaref, structaset, sudoku, throw
```

## 失敗カテゴリと原因

### Category A: bundler / gem 解決エラー (環境依存・monoruby 本体のバグではない)

**症状:** `bundler/definition.rb:702: ... GemNotFound: Could not find <gem> ...`

**根本原因:** monoruby の `build.rs` は CRuby **4.0 以上** が `PATH` に
無いと以下の 3 ファイルを生成しない (Warning: failed to read library
path file を出す):

- `~/.monoruby/library_path` ($LOAD_PATH 用)
- `~/.monoruby/gem_path` (GEM_PATH 用)
- `~/.monoruby/ruby_version`

`build.rs` の `MIN_RUBY_VERSION = (4, 0)` (`monoruby/build.rs:7`) でガード
されているため、ホスト Ruby が 3.x だとファイルが書かれず、monoruby は
GEM_PATH = 空・RUBY_VERSION = "4.0.0" (defaults) で起動する。
ベンチマークの `use_gemfile`
(`yjit-bench/harness/harness-common.rb:62`) が `Bundler.setup` を
呼ぶと、ロックファイルに書かれた gem を `~/.monoruby/lib/<gem>` でも
GEM_PATH でも見つけられず `GemNotFound` で落ちる。

`GEM_PATH=/opt/rbenv/versions/3.3.6/lib/ruby/gems/3.3.0` を手で渡しても
今度はベンダリングされた rubygems が Ruby 3.3 用ビルドの gem を弾く
(`Gem::MissingSpecVersionError`, "Could not find 'json' (= 2.19.1) - did
find: [json-2.19.1, ...]")。これはホスト Ruby のメジャー版と monoruby
が報告する `RUBY_VERSION` の不一致による required_ruby_version 検査
失敗で、本質的にビルド環境の前提 (Ruby 4.0+) が満たされていないこと
が原因。

該当 (27 件):

```
activerecord, addressable-equality, addressable-getters, addressable-join,
addressable-merge, addressable-new, addressable-normalize, addressable-parse,
addressable-setters, addressable-to-s, chunky-png, erubi, erubi-rails, fluentd,
graphql, graphql-native, hexapdf, json_parse_float, json_parse_string, lee, mail,
psych-load, rack, railsbench, rubocop, ruby-lsp, tinygql
```

→ Ruby 4.0+ をホストにインストールして `cargo build` を再実行すれば
解消が期待される (`build.rs` が `gem_path` と `library_path` を生成する)。

### Category B: `bundle install` 実行待ちのタイムアウト

**症状:** stdout に `Fetching <gem>` / `Installing <gem>` が大量に並び、
60s 制限を超えて kill された (`rc=124`)。

`use_gemfile` は最初に `bundle check 2> /dev/null || bundle install` を
shell で実行する。`bundle check` が失敗するとネットワークから gem を
取りに行くため非常に時間がかかる。完了したとしても **Category A** に
帰着する (vendored bundler / gems が解決できない)。

該当 (3 件): `lobsters`, `sequel`, `shipit`

### Category C: Git source 解決失敗

**症状:** `bundler/source/git.rb:233: ... is not yet checked out. Run 'bundle install' first. (GitError)`

Gemfile に `gem "liquid", git: "https://github.com/Shopify/liquid.git", ref: "..."`
のような git ソースがあり、ベンダリング bundler が `git clone` 済み
ディレクトリを必要とする。事前に `bundle install` が走れなかったため失敗。

該当 (3 件): `liquid-c`, `liquid-compile`, `liquid-render`

### Category D: monoruby の実装ギャップ — Ractor 未サポート

**症状:** `NameError: uninitialized constant Ractor`

monoruby は **設計上シングルスレッドで Ractor は未実装**。`yjit-bench`
側の Ractor 専用ベンチは harness で `Ractor.make_shareable` を呼ぶ前に
`defined?(Ractor.make_shareable)` でガードしているので大抵動くが、
`knucleotide-ractor` はベンチ本体が直接 `Ractor.make_shareable(...)`
を呼ぶため落ちる。

```
/home/user/yjit-bench/benchmarks/knucleotide-ractor/benchmark.rb:56:
  TEST_SEQUENCE = Ractor.make_shareable(generate_test_sequence(100_000))
                  ^^^^^^
```

該当 (1 件): `knucleotide-ractor`

> `gvl_release_acquire`, `json_parse_float`, `json_parse_string` は
> `harness-ractor` を使う設計だが、`harness/loader.rb` 経由なら
> `make_shareable` は `obj` を返す noop に置き換わるので Ractor 自体は
> 呼ばれない。

### Category E: monoruby の実装バグ — StringScanner ストブが String を受け付けない

**症状:** `NoMethodError: undefined method 'source' for {`

`/root/.monoruby/lib/strscan.rb` は StringScanner を pure Ruby で再実装
したスタブだが、`_match_at_pos` が引数を **Regexp 前提** で扱っている:

```ruby
# /root/.monoruby/lib/strscan.rb:214
def _match_at_pos(pattern, advance, return_string)
  ...
  m = rest_str.match(Regexp.new("\\A(?:#{pattern.source})", pattern.options))
                                          ^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^
```

CRuby の StringScanner は `scan("{")` / `skip("{")` のように **String** を
渡すと文字列リテラルとしてマッチする (`scan_str_until`)。
`ruby-json/benchmark.rb:68` は `skip("{")` を使うため即座に落ちる。

最小再現:

```ruby
require "strscan"
StringScanner.new("{x}").skip("{")
# => undefined method 'source' for { (NoMethodError)
```

修正方針: `_match_at_pos` / `_match_forward` の入口で
`pattern.is_a?(String)` なら `Regexp.escape(pattern)` で正規表現化
する分岐を追加する。

該当 (1 件): `ruby-json`

### Category F: Category A の派生 (Git source + bundler 内部エラー)

**症状:** rubyboy は Gemfile に git source (RubyBoy 本体) があり、また
gem も不足。`bundler/lazy_specification.rb` → `materialize_for_installation`
の経路で internal な NoMethodError が出る。
ホストに Ruby 4.0 + 該当 gem が揃って初めて切り分けできる。

該当 (1 件): `rubyboy`

---

## Category A の派生で見つかったその他の monoruby ギャップ

GEM_PATH を手で設定して bundler をバイパスし `chunky-png` を実行すると、
次の monoruby 側の不足が表面化した:

**Zlib モジュールの定数不足:** `NameError: uninitialized constant Zlib::NO_COMPRESSION`

`monoruby/stdlib/zlib.rb` には `VERSION`, `ZLIB_VERSION`, `CRC_TABLE`
しか定義がなく、CRuby が持つ以下の標準定数が欠落している:

- `Zlib::NO_COMPRESSION` (= 0)
- `Zlib::BEST_SPEED` (= 1)
- `Zlib::DEFAULT_COMPRESSION` (= -1)
- `Zlib::BEST_COMPRESSION` (= 9)
- 他に `Zlib::DEFLATED`, `Zlib::FILTERED`, `Zlib::HUFFMAN_ONLY`, ...

`chunky-png` を Category A 修正後に動かすには別途これらの追加が必要。
(本件は今回の調査対象から派生したついでの発見なので別 PR にする想定。)

---

## 失敗一覧と分類

| benchmark | rc | category |
|---|---|---|
| activerecord | 1 | A |
| addressable-equality | 1 | A |
| addressable-getters | 1 | A |
| addressable-join | 1 | A |
| addressable-merge | 1 | A |
| addressable-new | 1 | A |
| addressable-normalize | 1 | A |
| addressable-parse | 1 | A |
| addressable-setters | 1 | A |
| addressable-to-s | 1 | A |
| chunky-png | 1 | A (+ Zlib 定数欠落) |
| erubi | 1 | A |
| erubi-rails | 1 | A |
| fluentd | 1 | A |
| graphql | 1 | A |
| graphql-native | 1 | A |
| hexapdf | 1 | A |
| json_parse_float | 1 | A |
| json_parse_string | 1 | A |
| knucleotide-ractor | 1 | **D: Ractor 未実装** |
| lee | 1 | A |
| liquid-c | 1 | C |
| liquid-compile | 1 | C |
| liquid-render | 1 | C |
| lobsters | 124 | B |
| mail | 1 | A |
| psych-load | 1 | A |
| rack | 1 | A |
| railsbench | 1 | A |
| rubocop | 1 | A |
| ruby-json | 1 | **E: StringScanner スタブのバグ** |
| ruby-lsp | 1 | A |
| rubyboy | 1 | F (Category A 派生) |
| sequel | 124 | B |
| shipit | 124 | B |
| tinygql | 1 | A |

## 再現コマンド

```sh
# 1. monoruby build
cd /home/user/monoruby
cargo build --release

# 2. yjit-bench clone (浅い)
cd /home/user
git clone --depth 1 https://github.com/Shopify/yjit-bench.git

# 3. ベンチを 1 本ずつ実行 (例)
WARMUP_ITRS=1 MIN_BENCH_ITRS=1 MIN_BENCH_TIME=1 \
  /home/user/monoruby/target/release/monoruby \
  /home/user/yjit-bench/benchmarks/ruby-json/benchmark.rb
```

`/tmp/run_all_benchmarks.sh` に一括実行用のスクリプトを置いた
(成果物は `/tmp/yjit-bench-results/<name>.{out,err}` に保存される)。

## monoruby 側で取り組む価値があるもの

Category A/B/C/F は基本的にホスト環境を Ruby 4.0+ + gem 一式に揃え
直せば前進する。一方 Category D/E は monoruby 自身の不足:

1. **`ruby-json`**: `~/.monoruby/lib/strscan.rb` (= `monoruby/stdlib/strscan.rb`)
   の `_match_at_pos` / `_match_forward` に String→Regexp 変換を追加。
   工数小・効果大 (CRuby 互換性向上)。
2. **`chunky-png` 派生**: `monoruby/stdlib/zlib.rb` に CRuby 互換の
   圧縮レベル定数を追加。
3. **`knucleotide-ractor`**: Ractor 未実装は monoruby の設計上の制約
   なので、yjit-bench 側でスキップ扱いにするのが現実的。
