# lobsters の定常状態プロファイルと多相呼び出しサイトの調査（2026-09）

ruby-bench の lobsters（Rails アプリ）が CRuby+YJIT の 0.67 倍で走っている原因を、
master a980b66b の perf プロファイル、`profile` フィーチャの統計、uprobe による
呼び出し回数の計測で追った。単一のホットスポットはなく、メソッド呼び出しの入口経路、
多相サイトでの deopt の反復、malloc、再コンパイルによるコード肥大が積み上がっている。
中でも deopt の約半分は、**全クラスが同じ ISeq に解決される多相サイト**がどの多相経路にも
乗らないことから来ている（§4）。同じ問題を ZJIT がどう扱っているかをソースで確認し、
§5 に対比をまとめた。

関連: `ruby_bench_fluentd_lobsters_2026-09.md`（lobsters を完走させた前回の記録）、
`yjit_bench_slow_investigation_2026-09.md`（activerecord などの同種の調査）、
`polymorphic_call.md`（多相サイトの設計メモ）。

## 1. 測定条件と全体の数字

4 コア x86-64 VM。perf はソフトウェアクロック（HW カウンタは使えない環境）。
`WARMUP_ITRS=5 MIN_BENCH_ITRS=10`。

| ランタイム | 1 iteration 平均 | RSS | boot + 終了 |
|---|---|---|---|
| monoruby a980b66b | 1458 ms | 797 MiB | 9.3 s |
| ruby 4.0.6（インタプリタ） | 1654 ms | 305 MiB | 10.6 s |
| ruby 4.0.6 --yjit | 977 ms | 346 MiB | 6.4 s |

boot は CRuby と同等で、差は定常状態の iteration にある。プロセス全体の perf は
boot（require、parse、JIT コンパイルで 17% 超）に汚染されるので、以下は
`perf record -D 32000` で warmup 後の 20 iteration だけを取ったもの。

## 2. 定常状態の時間の内訳（self time）

| 分類 | 割合 |
|---|---|
| JIT 本体（名前付き） | 22.5% |
| 名前なし JIT 領域: クラスガード stub（§3.1） | 約 9〜12% |
| 名前なし JIT 領域: VM インタプリタ | 約 3% |
| libc malloc / free / memmove | 13.0% |
| sqlite（`libsqlite3_native`） | 9.9% |
| VM ランタイムの呼び出し補助（`invoke_func`、`set_*_arguments` など） | 6.4% |
| メソッド探索（`find_method` ×2、`search_method_refined`、`GlobalMethodCache::get`、`is_kind_of`） | 5.6% |
| GC（`execute_gc` の inclusive は 3.9%） | 4.6% |
| Hash 操作 | 4.1% |
| String 操作 | 3.1% |
| カーネル（うち `smaps_account` 0.68% はハーネスの RSS 読み取り） | 3.1% |
| Regexp / Onigmo | 2.7% |
| 定常状態での JIT コンパイル（inclusive 2.5%） | 1.9% |
| IdentId の intern | 1.3% |
| BigInt 演算（`Rational` の正規化と `Time` の exact subsec。§3.5） | 約 1.4% |

sqlite は YJIT 側でも約 160 ms/iteration で絶対時間が同じなので、monoruby 由来ではない。

## 3. 個別の所見

### 3.1 メソッド呼び出しの入口経路

定常状態のサンプルの 9〜12% が、各コンパイル済み本体の直後にある名前なしの stub の
**先頭 1 命令**に集中していた。位置と大きさ（本体末尾から 0 バイト、次の本体まで
39〜72 バイト）は `compile_patch` が本体の後ろに出す `class_guard_stub` に一致する。
`jit_entry` が未解決の呼び出しは、wrapper → パッチされた jmp → ガード stub → 本体と
taken jump を重ねて本体に入る。

この帰属は位置からの推定で、HW カウンタで命令フェッチの停止を確かめたわけではない。
ガードを本体エントリの直前に置いて fall-through させる配置は試す価値があるが、
効くかどうかは perf を取り直すまで分からない。

### 3.2 メソッド探索とランタイム側のディスパッチ

uprobe で数えた 1 iteration あたりの呼び出し回数（定常状態、probe 込みで約 2.3 s/iteration の実行を 18 iteration 分数えて割ったもの）:

| 関数 | 回数 / iteration |
|---|---|
| `Executor::find_method` | 約 350k |
| `GlobalMethodCache::get` | 約 500k |
| `Executor::invoke_method_inner_vis`（Rust 側からのディスパッチ） | 約 133k |
| `Kernel#respond_to?` | 約 32k |
| `Kernel#send` | 約 9k |
| `Store::intern_frozen_str`（`String#-@`） | 約 12k |
| `time::exact_subsec_parts`（`Time#strftime` から） | 約 12k |
| `args::block_arg_to_ary` | 約 0.4k |
| `Globals::class_version_inc` | 0 |

- `find_method` のうち約 220k は、VM / JIT の呼び出しサイトで 1 エントリのインライン
  キャッシュがミスした分。deopt 後にインタプリタで走る多相サイトが 2 クラスの間を
  往復するたびに、祖先チェーンの探索とグローバルキャッシュ引きをやり直している。
  `runtime::find_method` は既に PMC に記録しているので、探索の前に同じ PMC を
  `(callid, class)` で引けば大半がハッシュ 1 回で済む。
- クラスバージョンは定常状態では動いていない。`GeneratedAttributeMethods` の
  `method_added`（3,073 回）でバージョンが動くのは boot 中だけ。
- `Time#strftime` は毎回 `exact_subsec_parts` で `BigInt` を 2 つ作ってから
  `preprocess_strftime` に渡している。`%N` / `%L` を含まないフォーマットでも作るが、
  費用は 0.05% 以下で、`strftime` 全体（0.43%）の大半は書式の組み立てと chrono による
  2 回目の整形にある。
- `intern_frozen_str` は pool を引くたびに `bytes.to_vec()` で確保していた（対処済み。
  pool を借用のバイト列で引くようにした）。呼び出しの大半は ActiveRecord::Result の
  `columns.each(&:-@)` で、受け手は SQLite から得た frozen でない列名（平均 12 バイト）。
  frozen な pool の本体が受け手になるのは 0.05% しかなく、「pool 由来なら自身を返す」
  近道は効果がない。

`profile` ビルドのグローバルメソッドキャッシュ統計（15 iteration の実行全体の累計）では、
`==`（Object）446k、`String#-@` 370k、`==`（Thread）279k、`hash` 166k、
`===`（Time）154k、`respond_to?` / `respond_to_missing?` / `method_missing` 各約 130k、
`to_ary`（String / Integer）118k / 90k が上位だった。

### 3.3 malloc / free（13%）

YJIT も malloc 系に 8% を使うが、iteration が短いので絶対時間は約 195 ms 対 80 ms。
呼び出し元は次の 3 系統。

- **`RValue` のヒープ部分:** `create_array`（0.37%）、`fill_positional_args` の rest 配列、
  `object_with_ivar_capacity`、`escape_html`、`string_substring` など。
- **Vec の伸長（realloc）:** `HashRefMut::insert`、`Array#push`、捕捉ブロックの
  `wrap_promoted_frame`。
- **`String::clone`** と、GC sweep 内の free。

`malloc_consolidate` と `unlink_chunk` で 3% あり、中サイズのブロックが断片化している。

既存の `mimalloc` フィーチャで同じコミットを A/B した結果は以下で、差はノイズの範囲だった。
malloc の費用はアロケータの遅さではなく確保回数から来ている。

| ビルド | 1 iteration 平均（2 回） | MAXRSS |
|---|---|---|
| 既定 | 1446 / 1456 ms | 797 MiB |
| `--features mimalloc` | 1439 / 1429 ms | 820 MiB |

### 3.4 再コンパイルとコード肥大

実行終了時の JIT コードマップ（`/tmp/perf-<pid>.map`）:

| 項目 | 値 |
|---|---|
| hot ページの本体数 | 23,707（名前は 4,844 種類） |
| hot ページの大きさ | 15.4 MB（うち 11.8 MB は同名の 2 つ目以降のコピー） |
| cold ページ（side exit）の大きさ | 49.4 MB |
| 10 コピー以上ある名前 | 361 |

コピー数の上位は `Class#new` 2,120（706 KB）、`Thread::Mutex#synchronize` 419、
`Array#each` 319、`Concurrent::Collection::MriMapBackend#compute_if_absent` 308、
`cached_find_by` 系が各約 290。`profile` の再コンパイル統計の上位は
`Thread::Mutex#*` の `NotCached`（各 142 回）と、`cached_find_by` のブロックなどの
`ClassVersionGuardFailed`（各約 60 回）だった。

### 3.5 BigInt 演算（約 1.4%、同じ PR で対処）

BigInt の中で止まっている標本は、§2 と同じ計測で 1.42% あった。呼び出し元の内訳は次の通り。

| monoruby 側の呼び出し元 | 割合 |
|---|---|
| `Rational` の正規化（`RationalInner::normalize`） | 0.75% |
| `Time` の exact subsec の保存（`store_exact_subsec`） | 0.19% |
| BigInt の比から f64 への変換（`bigint_ratio_to_f64`） | 0.12% |
| `Time#+` の exact な加算（`shift_by_exact`） | 0.10% |

Ruby 側では ActiveSupport の `Time#change`、TZInfo の `Timestamp.for_time`、
Duration を足し引きする `Time#plus_with_duration` / `minus_with_duration` から来ている。
原因は 2 つあった。

- **`Rational` が常に BigInt の組だった。** `Rational(3, 4)` でも `Box` と BigInt 2 つを確保し、
  生成のたびに BigInt の gcd と除算を行っていた。
- **`Time` の exact subsec の計算が常に BigInt だった。** 秒以下を任意精度の有理数で保つ
  仕様上、Float や分母の大きな Rational を足すと分母は i64 を超えるので BigInt 自体は必要。
  しかし整数や小さな Rational を足すありふれた場合まで BigInt で計算し、
  `store_exact_subsec` は「ナノ秒未満の端数はない」という結論を出すために BigInt の
  gcd と剰余を使っていた。

どちらも、値が i64 に収まる間は i128 で計算して溢れたら BigInt に移る形にした
（`Rational` は分子と分母を `IntegerRepr` で持ち、`RValue` に直接置く）。
同じコミットの前後を同条件で計測し直した比較では、BigInt の標本は 1.22% から 0.21% に下がった。
`Time#+` の包含コストは、`Rational` の変更後の 0.58% から `Time` の変更後の 0.15% に下がった。

## 4. 多相サイトの deopt

`profile` ビルドでの deopt は上位 20 サイトだけで 1,118,239 回（15 iteration の実行全体の
累計、1 iteration あたり約 74.5k）。クラスガードの失敗は上位 20 で 697,688 回。
PMC は 54,566 サイトを記録し、うち 3,713 が多相、1,505 が megamorphic だった。

### 4.1 上位サイトの形

上位の多くは、**全クラスが同じ ISeq に解決される単純呼び出しで、受け手のクラスが多い**
サイトだった。右端の列は同名メソッドの PMC 記録で、deopt のサイトと 1 対 1 に
対応づけて確かめたものではない。

| deopt サイト | 回数 | PMC の記録 |
|---|---|---|
| `ConnectionHandling#connection_specification_name` の `connection_specification_name()` | 175,832 | 2 クラス（`ApplicationRecord` と `ActiveRecord::Base` の特異クラス）が各 88,116 回、同じ FuncId、overflow 0 |
| `Preloader::Association#derive_key` の `_read_attribute(%2)` | 94,459 | Story / User / Tagging / Comment ほか、overflow 526 |
| `BelongsToReflection#association_primary_key` の `has_query_constraints?()` | 59,220 | 各モデルの特異クラス、overflow 750 |
| `AbstractReflection#primary_key` の `primary_key()` | 59,219 | 各モデルの特異クラス、overflow 752 |
| `Preloader::Association#loaded?` の `association(%2)` | 57,154 | Tagging / Tag / Story / Comment ほか、overflow 465 |
| `Delegation::ClassMethods#relation_class_for` の `relation_delegate_class(%2)` | 39,219 | 各モデルの特異クラス、overflow 12,638 |
| `Arel::Table#[]` の `attribute_aliases()` | 37,758 | 各モデルの特異クラス、overflow 11,873 / 6,650（2 サイト） |
| `Preloader::Branch#preloaders_for_reflection` のブロックの `association(%4)` | 26,153 | 同上 |

この 8 サイトで上位 20 サイトの deopt の約 49%（549,014 回）を占める。
`connection_specification_name` は `superclass.connection_specification_name` の
再帰で 2 つの特異クラスが交互に来るので、2 回に 1 回 deopt している。

### 4.2 どの多相経路にも乗らない理由

このようなサイトは、monoruby の 2 つの多相経路のどちらからも外れる。

1. **クラス集合ガード（`pmc_same_target_classes`、`compile/method_call.rs`）は使われない。**
   特殊化できる ISeq の単純呼び出しは、あえて単一クラスガードに残す方針になっている。
   コメントによれば、無制限に集合ガードを使うと activerecord で deopt が 40% 減る一方、
   支配的なクラスの callee をインライン化できなくなって全体が 7% 遅くなった。
2. **PIC（`pic_groups`、`compile/pic.rs`）は「single-target」で拒否する。**
   アームは解決先の FuncId ごとに作られ、同じ FuncId のクラスは 1 つのアームにまとめられる
   （例外は attr_reader / attr_writer だけ）。2 クラスが同じ ISeq に解決されるとアームは
   1 本になり、PIC の条件を満たさない。
3. **ミス時に再コンパイルもしない。** 受け手クラスのミスで再コンパイルするのは PMC が
   1 クラスのときだけで、2 クラス以上記録されていれば、多相経路は検討済みとして
   以後は通常の deopt を繰り返す。

加えて、PMC は 4 ウェイ（`PMC_WAYS`）で、モデルクラスを受け手とするサイトはこれを溢れる。
仮に集合ガードや PIC を使えても、5 クラス目以降は deopt する。

### 4.3 アームごとに特殊化できない理由

「2 クラスなら特殊化した経路を 2 本出して合流させればよい」が、今はそうならない。
上記 2 の FuncId によるまとめに加えて、`compile_method_call` は分岐アームの中では
`specialized_iseq` を呼ばない（`in_dispatch_arm()` のチェック）。コメントにある理由は
2 つで、どちらも越えられない壁ではない。

- **`CompileError` を途中から取り消せない。** ただし `CompileError` はアームの外でも
  コンパイル単位全体を放棄させ、そのメソッドは以後インタプリタで走る
  （`codegen/compiler.rs` の `Err(_)` 分岐）。単相の経路でも同じ失敗は起こりうるので、
  増えるのは「2 つ目のクラスの callee の特殊化だけが失敗する」場合に限られる。
- **`Cease`（callee が正常に return しない）だとアームから合流点への経路がない。**
  PIC は全アームが `Continue` で終わることを `debug_assert` しているが、`Cease` の
  アームは合流点への bridge を省けば済む。全アームが `Cease` のときだけ命令全体を
  `Cease` にすればよい。

`compile/pic.rs` の冒頭コメントによれば、以前の PIC は全アームが同じ状態で合流することを
要求していた。今の合流は事前宣言型（`compile/dispatch.rs`）で、アームごとに状態が違っても
合流できるので、アーム内の特殊化禁止はその旧設計の名残に近い。

ただし事前宣言型の合流は `dst` を型不明の `Value` にし、キャッシュした不変条件も捨てる。
アーム内で特殊化しても、戻り値の定数やクラスの情報は合流後に残らない。

## 5. ZJIT での扱い

ruby/ruby master（57213d4、2026-09-25）と Shopify/ruby master（d41370a、2026-09-22）の
`zjit/` で確認した。両者で多相の扱いは同じで、差は `CondBranchHasType` の中身の Box 化などの
リファクタリングだけだった。以下のファイル名は `zjit/src/` からの相対パス。

1. **プロファイルは実行数で数える。** 各命令の最初の数回の実行で、インタプリタが
   8 種類までの型分布を記録する（`profile.rs`）。分布は `distribution.rs` で
   Monomorphic / Polymorphic / SkewedPolymorphic（最多が 75% 以上）/ Megamorphic /
   SkewedMegamorphic に分類される。
2. **HIR 構築の時点で型ごとに分岐させる。** Polymorphic と SkewedPolymorphic の send に
   対し、観測した型ごとに `CondBranchHasType`、受け手の型を確定させる `RefineType`、
   新しい `Send` を出し、戻り値を合流ブロックの引数として渡す（`hir.rs` の
   `opt_send_without_block` / `send` の変換）。**分岐は型で重複排除し、解決先の
   メソッドではまとめない。** どの型にも当たらない受け手は汎用の `Send`
   （理由 `SendPolymorphicFallback`）に落ちてから合流し、deopt しない。
3. **特殊化とインライン化は後のパスで行う。** `type_specialize` が分岐ごとの `Send` を
   `SendDirect` に書き換え、`inline_methods` が callee を分岐ごとにインライン展開する。
4. **インライン化の失敗はその呼び出しだけを巻き戻す。** 命令表とブロック表の長さを
   記録しておき、callee の変換が失敗したら切り詰める。return する経路がない callee も
   同様に巻き戻し、呼び出しのまま残す。
5. **単相ガードのミスは回数制限付きで再コンパイルする。** 5 回外れると版を無効化して
   プロファイルを取り直す。版は 4 つまでで、最終版は side exit しうる投機をしない
   （`CompilePolicy::no_side_exits`）。
6. **megamorphic は汎用 send のまま。** 特殊化もしないが、deopt もしない。

受け手が特異クラスでも `assume_no_singleton_classes` は無条件に通るので、
`connection_specification_name` のようなクラスメソッドのサイトも 2 の分岐の対象になる。

| 項目 | ZJIT の既定値 |
|---|---|
| プロファイルする型の数 | 8 |
| 偏りとみなす閾値 | 75% |
| プロファイルする実行回数 | 5 |
| 版を無効化するまでの side exit 回数 | 5 |
| 版の上限 | 4 |
| インライン化する callee の上限 | 30 バイト |
| 呼び出し側の命令数の予算 | 200 |

monoruby との対応:

| monoruby | ZJIT |
|---|---|
| PIC は同じ FuncId のクラスを 1 アームにまとめる | 型で重複排除し、同じメソッドでも分岐は分かれる |
| アームの中では特殊化しない | 分岐は普通の基本ブロックで、特殊化は後のパスがグラフ全体に対して行う |
| `CompileError` でコンパイル単位全体を放棄する | インライン化ごとに IR を切り詰めて巻き戻す |
| `Cease` のアームは合流点へつなげない | return しない callee はインライン化しない |
| 合流点の状態を保守的に事前宣言する | SSA の合流ブロックで、型推論も合流後に行う |
| 最後のアームのミスは deopt | 汎用 send に落ちて合流する |
| PMC はミスを数える | プロファイル窓の中の実行を数える |

根本の違いはコンパイラの構造にある。ZJIT は SSA の制御フローグラフ（HIR）上で、分岐の生成、
特殊化、インライン化を別々のパスとして行う。monoruby はバイトコードから AsmIR へ、抽象状態を
持ちながら 1 パスで出力するので、アームの中で特殊化すると失敗を取り消す手段がない。

ZJIT でもインライン化されるのは小さな callee だけで、多くの分岐の実体は「型ガードと直接呼び出し」
である。効果の大半は deopt が消えることから来ていると考えられる。

## 6. 対策案（実装量の少ない順）

1. **（実施済み）`Rational` と `Time` の exact subsec の BigInt をなくす。** §3.5。
2. **（実施済み）`String#-@` の pool 検索で確保しない。** pool を借用のキーで引く。
   `String#-@` の包含コストは 0.62% から 0.49% に下がり、残りはハッシュ計算と
   バイト列の比較。
3. **インラインキャッシュのミス時に PMC を先に引く。** `runtime::find_method` で
   探索の前に `(callid, class)` の PMC を引く。メソッド探索 5.6% の半分弱を見込む。
4. **`respond_to?` の否定キャッシュ。** `respond_to_missing?` が既定実装の受け手には
   「見つからない」を直接返す。約 1%。
5. **同じ ISeq に解決される多相サイトの deopt をなくす。** 段階は次の通り。
   - 同じ FuncId でもクラスごとにアームを分け、single-target を PIC の拒否条件から外す。
     アームが特殊化しない普通の呼び出しでも deopt は消える。
   - 最後のアームのミスを汎用呼び出しにする。4 ウェイを溢れたクラスの deopt も消える。
     JIT 側の汎用呼び出し（x86-64 の `send_not_cached`）は現在コメントアウトされており、
     aarch64 も含めた作業が要る。
   - 単一クラスのアームで特殊化を許し、`Cease` のアームを合流点につながないようにし、
     アーム内の特殊化に深さの上限を設ける。`connection_specification_name` は自己再帰なので、
     上限がないとコード量がアーム数の深さ乗で増える。
   - 集合ガードを PMC の形（3 クラス以上か overflow あり、または 2 クラスがほぼ同数）で
     使い分ける案もある。PMC の件数はミス数なので判定は目安にしかならず、
     activerecord と lobsters の両方で A/B が要る。
6. **クラスガード stub の配置を変える実験。** §3.1。

`mimalloc` の既定化は効果がないので勧めない（§3.3）。

## 7. 計測手順

```sh
# ベースライン（ruby-bench/benchmarks/lobsters で）
WARMUP_ITRS=5 MIN_BENCH_ITRS=10 MIN_BENCH_TIME=0 monoruby benchmark.rb

# 定常状態の perf（perf フィーチャ付きビルド。JIT シンボルは /tmp/perf-<pid>.map）
WARMUP_ITRS=5 MIN_BENCH_ITRS=20 MIN_BENCH_TIME=0 \
  perf record -g --call-graph fp -D 32000 -o lob-ss.perf.data -- monoruby benchmark.rb

# deopt / 再コンパイル / PMC / グローバルメソッドキャッシュの統計（終了時に stderr へ）
cargo build --release --features profile
WARMUP_ITRS=5 MIN_BENCH_ITRS=10 MIN_BENCH_TIME=0 monoruby benchmark.rb 2> lob-prof.err

# 関数ごとの呼び出し回数（uprobe。シンボルは mangled 名で指定する）
perf probe -x monoruby --no-demangle <mangled symbol>
perf stat -e probe_monoruby:<event> -D 32000 -- monoruby benchmark.rb
```
