# ruby-bench で見た monoruby vs CRuby+YJIT と、低コストで広く効く高速化案（2026-09-08）

調査日 2026-09-08。対象コミットは `702e362`（master, "JIT: settle a constant store's
range window at compile time" #1290）。比較対象は rbenv でビルドした
**CRuby 4.0.2 `--enable-yjit`**（`ruby 4.0.2 (2026-03-17 revision d3da9fec82) +YJIT +PRISM`）。
ベンチ本体は [ruby/ruby-bench](https://github.com/ruby/ruby-bench)（旧 yjit-bench）を
`harness-warmup` で走らせ、後半半分の反復の中央値を使う（CI の `bench.yml` と同じ条件）。

計測機は x86-64 / Linux 4 vCPU（コンテナ、`perf` なし）。[ベンチマークポータル](https://sisshiki1969.github.io/monoruby/)
の x86-64 最新スナップショットが同じコミット `702e362`（2026-09-07 16:33 UTC）なので、
全体像はポータルの値（§2）を使い、原因の切り分けだけをこの機械で行った。

`ruby-json` は別セッションで調査中のため、本稿では対象外とする。

先行調査 [`yjit_bench_slow_investigation_2026-09.md`](yjit_bench_slow_investigation_2026-09.md)
（activerecord / erubi / rack / graphql の perf 解析と、その §8 で入れた対策）の続きに
あたる。そちらで扱った headline 4 本は再解析せず、残っている案を §5 で参照するに留める。

---

## 1. 結論（先に要点）

- ポータル（`702e362`, x86-64）の 60 本で幾何平均 **1.54x**（monoruby が速い）。24 本で
  YJIT に負けており、負け方は 3 つの塊に分かれる（§3）:
  1. **コード量が巨大な 30k 系**（`30k_methods` 0.35x, `30k_ifelse` 0.42x）— JIT 済みでも
     1 呼び出し 36 ns（YJIT 14 ns）。cachegrind で L1 命令キャッシュミスが 1 呼び出しあたり
     約 10 回。呼び出しサイト 1 つ ≈ 52 B + メソッド固定部 ≈ 60 B + 呼び出し先 ≈ 90 B に
     加え、実行されない side-exit 領域が 1 メソッドあたり 416 B。
  2. **ライブヒープが大きい GC 負荷**（`splay` 0.37x）— GC を除いた時間は同等
     （82 ms vs 85 ms）で、差は **GC の回数**（monoruby 1.3 回/反復、CRuby 0.55 回/反復）。
  3. **メソッド呼び出しの固定費**（`send_bmethod` 0.33x, `fib` 0.79x, `keyword_args` 0.80x,
     `send_rubyfunc_block` 0.80x）— 1 呼び出し ≈ 5 ns vs YJIT ≈ 4 ns。既に床に近く、
     `send_bmethod` だけは「引数をそのまま返すメソッド」を YJIT がインライン化する差。
- 汎用 Ruby 操作 150 項目のマイクロベンチ（§4）で、ベンチ名に出てこない **monoruby 固有の
  遅さ**をいくつか見つけた。実装コストが低く、しかも普通のコードで頻出するものを §5 に
  コスト順で並べた。上位 3 つ:

| 順 | 施策 | 変更箇所 | 効果（ns/op, monoruby → 見込み, YJIT） | 広さ |
|---|---|---|---|---|
| 1 | Hash リテラルの String キーを frozen リテラルとして emit | `bytecodegen/expression.rs::gen_hash`（数十行） | `{'a'=>1,'b'=>2,'c'=>i}` 386 → ≈120（YJIT 154）、5 ペア 779 → ≈245（187） | `frozen_string_literal` なしの全コード |
| 2 | `String#index` / `#count` / `#sub` の String パターンに memmem + ASCII 直索引の fast path | `builtins/string.rs`（`substring_char_index`, `sub_main`, `count`） | `index` 200 B 文字列 1828 → ≈100（145）、`'hello world'.index('wor')` 211 → ≈60（105）、`count('l')` 300 → ≈60（78）、`sub('o','0')` 756 → ≈300（425） | 文字列処理全般 |
| 3 | GC 割り当て予算を在籍ページの 1/16 → 1/4（またはサバイバル率で適応） | `alloc.rs::GC_HEAP_FRACTION`（1 行） | splay −15 %（RSS +5 %）。erubi / rack / activerecord は §3.2 の A/B 参照 | ライブヒープの大きいプログラム |

否定した仮説も残す（§4.3）: `Proc#call` が 4 倍遅く見えたのは、**proc を作ったフレームの
ループが JIT されない**（`toplevel_binding.md` の「captured frame は loop JIT に乗らない」）
ことによる計測の artefact で、`Proc#call` 自体は YJIT と同等（52 vs 59 ns）。

## 2. ruby-bench 全体（ポータル `702e362`, x86-64, 2026-09-07）

`monoruby_ms / yjit_ms` の逆数を「速度比」とし、1 未満が monoruby の負け。

| 速度比 | ベンチ（速度比） |
|---|---|
| < 0.5 | send_bmethod 0.33, 30k_methods 0.35, splay 0.37, 30k_ifelse 0.42 |
| 0.5–0.8 | activerecord 0.59, erubi 0.69, rack 0.71, send_cfunc_block 0.78, setivar 0.79, fib 0.79, keyword_args 0.80, send_rubyfunc_block 0.80 |
| 0.8–1.0 | psych-load 0.81, graphql 0.81, protoboeuf-encode 0.82, protoboeuf 0.88, addressable-merge 0.91, tinygql 0.92, chunky-png 0.93, addressable-new/-setters/-equality/-normalize 0.94–0.95, etanni 0.97 |
| 1.0–2.0 | addressable-parse/-join/-to-s/-getters 1.00–1.06, getivar 1.16, rubykon 1.17, str_concat 1.21, rubyboy 1.21, knucleotide 1.26, respond_to 1.31, attr_accessor 1.47, lee 1.49, optcarrot 1.50, structaref 1.58, loops-times 1.62, gcbench 1.81, blurhash 1.92 |
| ≥ 2.0 | sudoku 2.02, send_rubyfunc_inline 2.09, nqueens 2.10, fannkuchredux 2.17, nbody 2.18, binarytrees 2.31, ruby-xor 2.49, matmul 3.24, object-new-no-escape 3.69, throw 4.90, getivar-module 5.32, object-new-initialize 5.85, cfunc_itself 5.99, setivar_object 6.49, setivar_young 6.65, structaset 7.26, object-new 9.51, string_malloc_pressure 15.2, 30k_variables 28.9 |

60 本の幾何平均 1.539、24 本が 1 未満。monoruby が `exit 1` で完走しないもの（速度比の
母数に入らない）: shipit, sequel, fluentd, graphql-native, ruby-lsp, railsbench, liquid-c,
liquid-compile, liquid-render, liquid-il, mail, lobsters（+ erubi-rails / rubocop / hexapdf は
YJIT 側も失敗）。互換性の問題なので本稿の対象外だが、headline 16 本中 9 本が走らない
ことは、ポータルの幾何平均を読むときに注意がいる。

この機械での micro カテゴリの再計測（`MAX_TIME=30`）も傾向は同じ:
30k_ifelse 330 / 132 ms（0.40）、30k_methods 230 / 90（0.39）、send_bmethod 23.7 / 10.9
（0.46）、fib 51.5 / 43.6（0.85）、keyword_args 33.8 / 28.4（0.84）、send_cfunc_block
133 / 116（0.87）、send_rubyfunc_block 27.7 / 24.4（0.88）。残りの 19 本は monoruby が速い。

## 3. 負けている塊ごとの切り分け

### 3.1 30k_methods / 30k_ifelse — コード量と命令キャッシュ

`30k_methods` は 3 万個のメソッドが 1 段ずつ呼び合う（1 反復 = 600 万呼び出し）。

| | 1 反復 | 1 呼び出し |
|---|---:|---:|
| monoruby（JIT） | 214 ms | 36 ns |
| monoruby `--no-jit` | 396 ms | 66 ns |
| CRuby 4.0.2 `--yjit` | 85 ms | 14 ns |
| CRuby 4.0.2（インタプリタ） | 879 ms | 147 ns |

`fib` の再帰呼び出しが 5 ns なのに、ここでは 36 ns。JIT のログ（`--features emit-asm`）
では再コンパイル・deopt はなく、全メソッドが 1 反復目で JIT 済み。
`valgrind --tool=cachegrind --smc-check=all` の結果（1 反復 + 起動）:

| | 回数 | 1 呼び出しあたり |
|---|---:|---:|
| 命令フェッチ | 2.82 G | 470 |
| L1 命令キャッシュミス | 61.9 M | **≈ 10** |
| L1 データキャッシュミス | 23.2 M | ≈ 4 |
| LL ミス | 5.1 M | < 1 |

つまり L2 から命令を引き直す時間で、生成コードの**フットプリント**の問題。1 メソッドの
コード量（`emit-asm` の `(wm0 bytes, wm1 bytes)`）:

| メソッド | 本体 (wm0) | side-exit 領域 (wm1) |
|---|---:|---:|
| `def leaf(x); @a = x; end` | 87 B | 57 B |
| `def one_call(x); leaf(x); end` | 112 B | 416 B |
| `def two_call(x); leaf(x); leaf(x); end` | 164 B | 431 B |

呼び出しサイト 1 つ = 52 B（版ガード、スタック検査、フレーム 6 ワードのストア、cfp の
push/pop、エラー検査。`fib` の逆アセンブルで 30 命令）、メソッド固定部 ≈ 60 B
（プロローグ、ローカルの nil 初期化、safepoint poll、エピローグ）。3 万メソッドで本体だけ
で 3.4 MB、wm1 を足すと 16 MB。YJIT も同じ問題を抱えている（通常 4 ns の呼び出しが
14 ns）が、フットプリントが小さい分だけ軽い。

対策は §5.6（呼び出しシーケンスの共通スタブ化と side-exit 領域の縮小）。

### 3.2 splay — GC の回数

`splay`（V8 の splay ツリーの移植）は 8,000 ノードの木に、各ノードが深さ 5 の
ペイロード（≈ 100 オブジェクト）を持つ。ライブ 100 万オブジェクト、1 反復で 54 万
オブジェクトを割り当て、**そのほとんどが木に繋がれて生き残る**（世代仮説が成り立たない
ワークロード）。

反復ごとの時間と GC（16 反復、同じ機械）:

| | 反復時間（GC なしの反復） | GC 回数 / 16 反復 | GC 時間 / 反復 | ヒープ |
|---|---:|---:|---:|---:|
| monoruby | 74–82 ms | minor 16 + major 5 | **147 ms** | 654 ページ = 2.6 M スロット（ライブ 1.03 M） |
| CRuby --yjit | 72–92 ms | minor 7 + major 2 | 72 ms | 4.2 M スロット（ライブ 1.04 M） |

GC を挟まない反復は同じ速さ（マイクロベンチでも `splay_run` ×50 が 82 vs 85 ms）で、
差は丸ごと GC。1 回の minor GC のマーク時間は両者とも 60–140 ms（若い世代がほぼ全部
生きているので、生き残りを数えるだけで 1 反復分のマークになる）、major は 60–70 ms で
同等。違うのは**回数**: CRuby はヒープをライブの 4 倍まで育てて GC 間隔を空ける。
monoruby の割り当て予算は `max(32 ページ, 在籍ページ / 16)`（`alloc.rs::GC_HEAP_FRACTION`）
で、この規模では 41 ページ ≈ 16 万オブジェクトごとに minor が走る。

`GC_HEAP_FRACTION` を 16 → 4 にした A/B（release と交互 3 ラウンド、`MAX_TIME=20`）:

| ベンチ | 1/16 | 1/4 | RSS |
|---|---|---|---|
| splay | 273 / 261 / 297 ms | 252 / 226 / 232 ms（**−15 %**） | 378 → 396 MiB |
| gcbench | 572 / 558 / 592 ms | 560 / 599 / 601 ms（±0） | 55 → 56 MiB |

回数が 1/4 になっても −15 % に留まるのは、若い世代が全部生きているために minor 1 回の
マーク量が回数に反比例して増え、総マーク量が変わらないから（減るのはルート走査と
sweep の固定費と major の回数）。CRuby との残りの差は、生き残りを **3 回**（`RGENGC_OLD_AGE`）
マークしてから昇格させる規則が同じである以上、マーク 1 オブジェクトあたりの速さか
major の頻度（5 vs 2）にある。`perf` のない環境ではこれ以上分解できなかった。

erubi / rack / activerecord での A/B は §3.2.1 に追記する。

### 3.3 send_bmethod / fib / keyword_args / send_*_block — 呼び出しの固定費

`emit-asm` で見た `fib` の自己再帰 1 回は、呼び出し側 30 命令（版ガード、スタック検査、
`sub rsp`、呼び出しサイトの pc を `movabs`+store、self / 引数 / outer / meta / svar / block
の 6 ストア、lfp 計算、cfp の push、`call`、cfp の pop、r14 復元、`add rsp`、エラー検査）+
呼び出し先 8 命令（`push rbp; mov rbp,rsp; sub rsp`、ローカル nil 初期化、safepoint poll、
`leave; ret`）。YJIT は 4 ns、monoruby は 5 ns で、1 ns の差は命令数の差そのもの。
これ以上は「呼び出しごとに何を保存するか」の設計に踏み込むので、コスト低の範囲外。

`send_bmethod` だけは別で、`define_method(:zero) { :b }` は `ISeqHint::ConstReturn` で
呼び出しごと消えている一方、`define_method(:one) { |arg| arg }` は毎回フレームを作る
（specialized callee として `push rbp … jmp` の実体を 9 箇所インライン展開している）。
YJIT は `putobject; leave` / `getlocal; leave` 型の葉メソッドをインライン化する。
`ISeqHint::ArgReturn(idx)` を足せば同じになる（§5.5）。

### 3.4 headline（activerecord / erubi / rack / graphql / psych-load）

先行調査の対象で、ランタイム側（String キー Hash、GC ルート、例外、StringScanner）に
原因が集中していることが分かっている。この機械での再計測（`MAX_TIME=30`、gem は
rack 3.2 / graphql 2.5 / erubi 1.13 / activerecord 8.1 + sqlite3 の `bundle install` 時点の
最新）:

| ベンチ | monoruby | CRuby 4.0.2 --yjit | 速度比 | ポータル（`702e362`） |
|---|---:|---:|---:|---:|
| activerecord | 276 ms | 177 ms | 0.64 | 0.59 |
| erubi | 320 ms | 249 ms | 0.78 | 0.69 |
| graphql | 51 ms | 40 ms | 0.78 | 0.81 |
| psych-load | 2442 ms | 2117 ms | 0.87 | 0.81 |
| rack | 85 ms | 68 ms | 0.80 | 0.71 |

（erubi は `LANG` 未設定のコンテナだと CRuby 側が `JSON.load` で
`Encoding::InvalidByteSequenceError` になる。`LC_ALL=C.UTF-8` を付けて計測した。）

## 4. 汎用操作のマイクロベンチ（monoruby vs YJIT）

ベンチ名に現れない差を拾うため、普通の Ruby コードに出る操作 150 項目を
`while` ループで 20 万回まわして ns/op を測った（3 ラウンドの最小、JIT ウォームアップ
済み。**結果を捨てる式は CRuby が消してしまう**ので、全項目で結果を `@k` に足し込む）。
スクリプトは §6。

### 4.1 monoruby が遅い項目（比率 = monoruby / YJIT）

| 項目 | monoruby | YJIT | 比率 | 原因（コードを読んだ範囲） |
|---|---:|---:|---:|---|
| `s.index('needle')`（206 B の s） | 1828 | 145 | **12.6** | `substring_char_index` が毎回全文字境界の `Vec` を作って線形走査。memmem なし |
| `{'a'=>1,'b'=>2,'c'=>3,'d'=>4,'e'=>i}`（pragma なし） | 779 | 187 | **4.2** | 非静的な Hash リテラルは String キーを非 frozen リテラル（複製）→ `frozen_hash_key` で再複製の **2 アロケーション/キー** |
| `s.count('l')` | 300 | 78 | 3.9 | 呼び出しごとに `tr_set_view` + `Charset::parse` |
| `{'a'=>1,'b'=>2,'c'=>i}`（pragma なし） | 386 | 154 | 2.5 | 同上（pragma 付きなら 120 vs 147） |
| `instance_variable_get(:@a)` / `_set` | 116 / 123 | 53 / 57 | 2.2 | 名前 → IvarId の探索を毎回 |
| `s.index('wor')`（11 B） | 211 | 105 | 2.0 | 同上（境界 Vec） |
| `format('%05d-%s', i, 'x')` | 681 | 346 | 2.0 | |
| `catch(:t) { throw :t }` | 412 | 224 | 1.8 | 先行調査 §5.7 |
| `s.sub('o', '0')` | 756 | 425 | 1.8 | String パターンでも `RegexpInner::replace_one`（正規表現エンジン）を通る |
| `{a:1,b:2,c:3,d:4,e:i}`（4 ペア以上 = boxed） | 260 | 161 | 1.6 | boxed map をペアごとに構築（3 ペア以下のインライン表現なら 22 ns） |
| `a.map(&:to_s)`（10 要素） | 2534 | 1574 | 1.6 | 先行調査 §5.9 |
| `raise ArgumentError` / `rescue` | 1677 | 1151 | 1.45 | 先行調査 §5.7 |
| `s.unpack1('C')` | 148 | 97 | 1.5 | |
| `s.each_char.count` | 2168 | 1465 | 1.5 | |
| `'%d' % i` | 481 | 346 | 1.4 | |
| `a + b`（Array 10 + 2） | 144 | 109 | 1.3 | |
| `case o when Integer … when String …` | 153 | 123 | 1.25 | `Module#===` の呼び出し |
| `s.tr('o', '0')` | 319 | 260 | 1.2 | |
| `s =~ /(wor)/; $1` | 421 | 458 | 0.92 | （先行調査で `$~` 保存が重いとしていたが、使う形では同等） |

### 4.2 monoruby が速い項目（抜粋）

ivar get/set 10 vs 134、attr_reader 5 vs 80、`Object.new` + ivar 18 vs 129、
`Integer#times` 10 回 81 vs 402、`Array#each` 10 要素 112 vs 424、`each_with_index`
100 vs 871、`Array#map` 237 vs 629、`Array#sort_by` 469 vs 1441、`Hash#each` 283 vs 455、
`respond_to?` 2.7 vs 49、`is_a?` 3.0 vs 64、`send(:sym)` 17 vs 71、kwargs 呼び出し
9.4 vs 62、`Float` 算術 5.4 vs 47、`String#split` 333 vs 512、`String#+` 102 vs 161、
文字列補間 158 vs 299、`Float#to_s` 298 vs 607、`Integer#to_s` 112 vs 147、
`Hash#dup` 198 vs 217、`Hash#merge` 104 vs 202、`String#==` 21 vs 44、`Struct` 生成
141 vs 212、`rand` 30 vs 152、`Range#to_a` 247 vs 819。

### 4.3 否定した仮説

- 「`Proc#call` / `lambda.()` が 4 倍遅い（208 vs 50 ns）」— proc を作った**同じメソッド**の
  ループで測っていた。そのメソッドのフレームは proc に捕捉されヒープに移るので loop JIT
  の対象外になり、ループ全体がインタプリタで走る（何もしないループでも 30.5 ns/反復、
  通常 2.7 ns。YJIT は 26 ns で変わらない）。proc を定数や引数で受け取って呼ぶと
  52–58 ns（YJIT 56–59）で同等。`&blk` を受ける側・`proc {}` をループ内で作る側は
  monoruby の方が速い（104 vs 205、101 vs 185）。
  → 「捕捉されたフレームのループも JIT する」は独立した改善項目（§5.8）。
- 「Hash リテラルが 17 倍遅い」— 結果を捨てる式を CRuby が消していた。使うと 1.6–4.2 倍
  （上表）。

## 5. 提案（実装コスト順）

「コスト」は変更範囲の目安（低 = 1 ファイル数十行、中 = 複数サブシステム、高 = 設計）。
「広さ」はどれだけ普通のコードで効くか。

### 5.1 Hash リテラルの String キーを frozen リテラルとして emit — コスト低、広さ大

`gen_hash`（`bytecodegen/expression.rs`）は全ペアが静的なら template を作って
`Literal` で複製する（CRuby の `duphash` 相当）が、値に 1 つでも式があると
`push_expr(k)` でキーも通常のリテラルとして emit する。`frozen_string_literal` が
ない場合、String リテラルは評価のたびに複製され（`TraceIr::Literal` の `deep_copy_lit`）、
その直後 `emit_hash` → `frozen_hash_key` でもう一度複製 + freeze される。キーは
Hash に入った時点で必ず frozen になるので、**キー側だけは pragma に関係なく
`static_hash_key` と同じ frozen リテラルを emit してよい**（CRuby も `{"a" => x}` の
キーは frozen literal として compile する）。値側は今のまま。

見込み: `{'a'=>1,'b'=>2,'c'=>i}` 386 → ≈ 120 ns、5 ペア 779 → ≈ 245 ns（pragma 付きの
実測値と同じになる）。rack の `env` 生成、オプション Hash、JSON 風の構築など、pragma を
付けていない gem 全般に効く。変更は `gen_hash` の非静的経路でキーノードが
`NodeKind::String` のときに `emit_literal(frozen)` を使う数十行 + テスト
（`String#hash` / `eql?` 再定義時の挙動は `frozen_hash_key` と同じ規則）。

### 5.2 String パターンの検索を memmem に — コスト低、広さ大

- `String#index` / `rindex`（String パターン）: `substring_char_index` は
  「全文字境界を `Vec<usize>` に積む（アロケーション + O(n)）→ 各境界で
  `starts_with`」。ASCII-only（`is_ascii_only()` は既にキャッシュ済み）なら
  `memchr::memmem::find`（`Cargo.lock` に既にある）でバイト位置 = 文字位置。UTF-8 なら
  memmem のバイト位置を `byte_to_char_index` で変換し、境界チェックは見つかった
  位置だけで行う。
- `String#count` / `delete` / `squeeze`: 引数が 1 文字（ASCII）の最頻ケースは
  `Charset::parse` を通さず `bytecount`。
- `String#sub` / `sub!` / `gsub`（String パターン）: `RegexpInner::replace_one` を経由
  せず、memmem で見つけて splice。ただし CRuby は String パターンでも `$~` を
  `MatchData` にする（`'hello'.sub('l','L'); $~ #=> #<MatchData "l">`）ので、
  `$~` は今と同じく設定する（位置だけ分かっているので、正規表現エンジンを通さず
  region 1 個の MatchData を作る）。`index` / `count` は CRuby も `$~` を触らない。

見込み: `index` 12.6x → 1x、`count` 3.9x → 1x、`sub` 1.8x → ≈ 0.7x。テンプレート、
パーサ、`Rack::Utils`、`URI` など文字列処理はどこにでもある。

### 5.3 GC 割り当て予算 — コスト最低、広さ中（メモリとのトレードオフ）

`GC_HEAP_FRACTION` 16 → 4 で splay −15 %、RSS +5 %。gcbench は変わらない。先行調査で
`PAGES_PER_GC_TRIGGER` 8 → 32 が graphql −8 % / activerecord −6.5 % だったのと同じ種類の
効果で、headline での A/B（§3.2.1）を見て決める。より筋のよい形は CRuby の
`heap_free_slots_goal_ratio` 相当 —「前回の minor で若い世代の生存率が高かったら
次の予算を伸ばす」— で、生存率は既に `GC.stat` 用に数えているので、`alloc.rs` に
閉じた十数行で済む。

### 5.4 `instance_variable_get` / `_set`、`format` — コスト低、広さ中

- `instance_variable_get(:@a)`: Symbol → `IvarId` の解決を builtin 内で毎回行っている。
  呼び出しサイトの `CallSiteInfo` に（クラス, 名前）→ `IvarId` の 1 エントリキャッシュを
  置くか、`ClassInfo` の ivar 表を Symbol で直接引く。ActiveModel / シリアライザ /
  `Struct`-like の gem が多用する。2.2x → 1x。
- `format` / `String#%`: 単純な指示子（`%d` `%s` `%05d` `%.2f`）だけの書式は
  解析結果をリテラルごとにキャッシュする。2x → 1x。

### 5.5 `ISeqHint::ArgReturn` — コスト低、広さ小

`def name(x) = x` / `{ |x| x }` / `def name(*) = self` 型の葉を、`ConstReturn` /
`SelfReturn` と同じく呼び出しサイトで畳む。`send_bmethod` 0.33x → ≈ 1x。
ただし実コードでの頻度は低い（`itself` 相当、`define_method(:x) { |v| v }`）。

### 5.6 呼び出しサイトのコード量 — コスト中、広さ中（大きなアプリで効く）

§3.1 の通り 1 サイト 52 B + メソッド固定部 60 B + side-exit 416 B/メソッド。
1. フレーム構築（6 ストア + cfp push）と後始末（cfp pop + r14 復元）を **VM 領域の
   共通スタブ**にして、サイト側は `movabs`（callee pc, meta）+ `call stub` にする。
   1 サイト ≈ 20 B。x86-64 / aarch64 の両バックエンド。
2. side-exit（deopt）スタブは「戻り先 pc + 生きているレジスタの書き戻し」しか違わないので、
   書き戻し表をデータにして共通ルーチンに飛ばす（wm1 416 B → 数十 B）。
Rails 級のコード量では L1i / L2 に収まらないので広く効くが、この環境では計測できない。
30k 系だけなら 2.5x の差の相当部分が埋まる見込み。

### 5.7 先行調査で挙がっている中コストの項目

- boxed Hash リテラル（4 ペア以上）を template + 動的な値の差し込みにする（1.6x）。
- `catch` / `throw` を例外オブジェクトなしで巻き戻す（1.8x）、`raise` のメッセージ複製と
  インターンをやめる（1.45x）。
- `Symbol#to_proc` のブロック呼び出し経路（1.6x）。
- StringScanner primitive の `onig_match` 化（graphql）。

### 5.8 捕捉されたフレームの loop JIT — コスト高

proc / lambda / binding / `method(:x)` を作ったメソッド内のループがインタプリタに
留まる（§4.3）。YJIT とは同等なので「負け」ではないが、monoruby の他のループとの
差が 11 倍あるので、ヒープ化したフレームでも `rbp` 相対アクセスを間接化して JIT する
価値はある。`toplevel_binding.md` の議論の続き。

## 6. 計測手順（再現用）

```sh
# CRuby 4.0.2 + YJIT
RUBY_CONFIGURE_OPTS="--enable-yjit --disable-install-doc" rbenv install 4.0.2
rbenv global 4.0.2 && ruby --yjit -v     # ... +YJIT +PRISM
gem install --no-document bigdecimal erubi bundler

# monoruby
cargo build --release && cp target/release/monoruby /tmp/monoruby-release
cargo build --release --features emit-asm && cp target/release/monoruby /tmp/monoruby-asm

# ruby-bench（旧 yjit-bench）
git clone --depth 1 https://github.com/ruby/ruby-bench.git ../ruby-bench
cd ../ruby-bench
MAX_TIME=30 ./run_benchmarks.rb --no-sudo --no-pinning --rss --harness=harness-warmup \
  --category=micro -e "monoruby::/tmp/monoruby-release" -e "yjit::$(command -v ruby) --yjit"

# ポータルの生データ（同じコミットの全ベンチ）
curl -sSO https://sisshiki1969.github.io/monoruby/bench/latest.json

# 30k_methods の命令キャッシュ
INTERNAL_ITRS=200 MIN_ITERS=3 MAX_TIME=1 valgrind --tool=cachegrind --cache-sim=yes \
  --smc-check=all /tmp/monoruby-release -I harness-warmup benchmarks/30k_methods.rb

# splay の反復ごとの GC（§3.2 の表）: benchmark.rb の run_benchmark を外して
# GC.stat の差分を出すスクリプト（scratchpad/splay-iter.rb）
```

マイクロベンチ（§4）は各項目を

```ruby
def case_N(n); <setup>; i = 0; t0 = clock; while i < n; <body>; i += 1; end; clock - t0; end
```

に展開し、`n/10` で 2 回ウォームアップしてから `n = 200_000` を 3 回、最小値を採る。
`@k += (式)` の形で結果を必ず使う。
