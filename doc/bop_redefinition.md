# 基本演算の再定義（BOP redefinition）

`1 + 2` のような演算子式を、メソッド探索を挟まずインライン展開してよいのは
「`Integer#+` が再定義されていない」という**仮定**が成り立つ間だけである。Ruby
はその仮定をいつでも壊せる（`class Integer; def +(o); …; end; end`）ので、
処理系は「仮定を置く」「壊れたことを検知する」「壊れたあと辻褄を合わせる」の
3 点を用意しなければならない。

本書は monoruby の現行実装をコードに即して記述し、CRuby の戦略と実測で比較し、
**なぜ今の設計を変えるべきか**と、どの順で変えるべきかを記録する。

> 本書は design record である（`doc/README.md` の分類）。測定値は
> 2026-08-10 時点、`076cd951`、Linux/x86-64、release ビルドのもの。

関連: `doc/refinements.md` §6.7（refinements と基本演算）、`doc/jit.md`、
`doc/inline.md`

---

## 1. 出発点の実装（§5 Step 1 で置き換え済み）

> **注**: 本節は Step 1 *以前*の実装を記述する。検知まわり（§1.1・§1.2・§1.4）は
> `globals/store/basic_op.rs` の静的表に置き換わった（§5 Step 1）。反応
> （§1.3）とコスト（§1.5）は現在も同じで、Step 2 の対象である。問題の形を
> 残すために原文のまま置く。

### 1.1 「basic op」として登録されていたもの

登録の入口は 1 つだけで、`Globals::define_basic_op`
（`globals/method.rs`）→ `new_basic_op` → `add_basic_op_method` →
`add_method_inner(.., is_basic_op = true, ..)` と流れ、
`MethodTableEntry::is_basic_op` を立てる。

呼び出し箇所は**全部で 10 個**:

| クラス | 演算子 |
| --- | --- |
| `Integer` | `+` `-` `*` `/` `!=` |
| `Float` | `+` `-` `*` `!=` |
| `String` | `!=` |

これが「再定義を検知できる集合」の全てである。VM / JIT がインライン展開して
いる演算子はこれよりはるかに多い(§1.4)。

### 1.2 検知

`ClassInfoTable::insert_method`（`globals/store/class.rs`）は、メソッド表を
上書きしたとき **元のエントリが `is_basic_op` だったか**だけを見る:

```rust
fn insert_method(&mut self, class_id: ClassId, name: IdentId, entry: MethodTableEntry) {
    Globals::class_version_inc();
    if let Some(old) = self.classes[class_id].methods.insert(name, entry)
        && old.is_basic_op
    {
        self.set_bop_redefine();
    }
}
```

**どの演算子が・どのクラスで再定義されたかは記録されない。** 呼ばれた事実だけが
伝わる。

### 1.3 反応 — グローバルかつ恒久

`Store::set_bop_redefine`（同ファイル）が一度に次を行う:

1. `Codegen::set_bop_redefine` — `bop_redefined_flags: u32` に `!0` を書く。
   **以後クリアされない。**
2. `Store::invalidate_jit_code()` — **全 iseq** の `jit_invalidated = true`。
   これも一方向ラッチで、以後その iseq は method-JIT の対象から外れる
   （`compiler.rs` / `patch.rs` が `jit_invalidated()` で早期 return）。
3. x86-64: 全コンパイル済みメソッドの entry を `apply_jmp_patch_address` で
   `vm_entry` に書き戻す。aarch64: `jit_slot` / `jit_guard_free_slot` の
   dispatch word をゼロ化（`invalidate_jit_code` 内）。
4. `remove_vm_bop_optimization()` — **VM のディスパッチ表を `_no_opt` 版に
   恒久的に差し替える**。算術・比較・単項の fixnum fast path が全て
   フルディスパッチになる。`dispatch[14]`（`loop_start`）も no-opt になるので、
   **OSR ループ JIT が二度と起動しない**。

以上でオフスタックのコードは片付くが、**今スタックに乗っている** JIT フレームは
まだ古い本体を実行し続ける。その始末は `set_bop_redefine` の中ではなく、
呼び出し元の `Executor::{add_method, add_method_with_original,
alias_method_for_class}` が担う。メソッド表を書き換えた**後**・`method_added`
フックを呼ぶ**前**に `Codegen::check_bop_redefine(cfp)` を通し、フラグが非 0 なら
`immediate_eviction` が CFP 鎖を遡って各フレームの return address を deopt に
パッチする（`patch_return_to_deopt`）。戻ってきた時点で VM に落ちる。
`method_added` は任意の Ruby を走らせるので、その前に片付けておく必要がある。

この配置には 2 つ性質がある。

- **レベルトリガであってエッジトリガではない。** `check_bop_redefine` は
  「今回の定義が basic op を潰したか」ではなく「フラグが立っているか」を見る。
  フラグは一度立つとクリアされないので、**以後プロセス内のあらゆるメソッド定義が
  毎回 CFP 鎖の全走査と return address パッチを行う**。実測では他の要因
  (JIT が止まることで再コンパイル churn も消える)に埋もれて有意差は出なかったが、
  構造としては無駄が残り続ける。
- **経路が 3 つの funnel に限られる。** `remove_method` はここを通らない
  (§1.4 の但し書き)。

JIT コード内でのランタイム検査は `AsmInst::CheckBOP`（フラグのロード + 分岐）
だが、発行箇所は `MethodDef` / `SingletonMethodDef` の直後のみ
（`jitgen/compile.rs`）。**演算そのものにはガードが無い** — これは意図的で、
定数畳み込み（`100 * 100` → `10000`）とレジスタ常駐を成立させるための選択
（`jitgen/compile/binary_op.rs` のコメント参照）。

### 1.4 網羅の穴

インライン展開しているのに basic op として登録されていない演算子が多数ある。
48 ケースを CRuby 4.0.2 と差分テストした結果、**22 件が食い違った**。

**VM ティアで再定義が完全に無視されるもの:**

```
Integer   %  **  <<  >>  &  |  ^  ==  <  <=  >  >=  -@
Float     /  ==  <  >
String    ==
Array     []        Hash  []        Symbol  ==
```

**JIT ティア**は挙動が分かれる:

- **正しい** — `Integer#%`、`Array#[]`、`Hash#[]`、`Symbol#==`。
  `BinOpK::{Shl, Shr, Exp, Rem}` は「常にメソッド呼び出しにコンパイル」する
  分岐に入り、インラインキャッシュ + class-version ガードを経由するため。
- **誤り** — `& | ^`、比較全般、`-@`。`binop_integer` / 比較の無条件インライン
  展開で、ガードが無い。

`send(:+)` 経由は常に正しい（メソッド表を引くため）。**構文としての演算子だけが
壊れる。**

> 上記 22 件のうち 1 件（`Array#size`）は種類が違う。monoruby は `Array#size`
> を Ruby で実装しているため、再定義が `Kernel#p` の内部を壊して
> `ArgumentError` になる。BOP フックの欠落ではなく「ビルトインが Ruby 実装で
> あることの露出」で、CRuby（C 実装）には無い問題。切り分けて扱うべき。

**`remove_method` は検知経路そのものが無い。** 検知は
`ClassInfoTable::insert_method`（「上書きされた古いエントリが basic op か」）に
だけ置かれているが、`ClassInfoTable::remove_method` は `methods.remove()` を
直接呼び、`is_basic_op` を一切見ない。結果:

| 操作 | monoruby | CRuby |
| --- | --- | --- |
| `Integer.remove_method(:+)` → `1 + 2` | **`3`** | `NoMethodError` |
| `Integer.undef_method(:+)` → `1 + 2` | `NoMethodError` | `NoMethodError` |
| `Integer.alias_method(:+, :-)` → `1 + 2` | `-1` | `-1` |

`undef_method` が通るのは `add_empty_method` 経由で `insert_method` を踏むため。
CRuby は追加・削除・`prepend` のいずれでも
`rb_vm_check_redefinition_opt_method` を引くので取りこぼさない。検知点は
「メソッド表を変更する全経路」に置く必要がある。

### 1.5 コスト（実測）

`fib(30)`、`Float#+` を再定義。**ワークロードは Float を一切使わない。**

| 条件 | 時間 |
| --- | ---: |
| JIT・再定義なし | **0.022 s** |
| `--no-jit`（純 VM）・再定義なし | 0.069 s |
| JIT・`Float#+` 再定義後 | **0.51 s** |
| JIT・再定義**後に定義した**メソッド | 0.52 s |

無関係なクラスの再定義 1 回で **JIT ありの 24 倍、純 VM の 7.5 倍**遅くなる。
`_no_opt` ハンドラは fixnum インラインパスを捨てて毎回フルディスパッチするため、
「JIT を止める」より悪い。再定義後に定義したコードも救われない
（`remove_vm_bop_optimization` の効果はプロセス全体・恒久のため）。

---

## 2. CRuby の戦略

- **`ruby_vm_redefined_flag[BOP_xxx]`** — BOP ごとに 1 ワード、その中の**ビットが
  クラス**（`INTEGER_REDEFINED_OP_FLAG`、`FLOAT_…`、`STRING_…`、`ARRAY_…`、
  `HASH_…`、`SYMBOL_…` …）。粒度は **(演算子, クラス) の組**。
- **`vm_opt_method_defs`** — `(クラス, メソッド名) → BOP` の対応表。
  `rb_vm_check_redefinition_opt_method` がメソッドの追加・削除のたびに引き、
  一致したときだけ該当ビットを立てる。
- **インタプリタ** — `opt_plus` 等の中で、オペランドの型チェックの**後**に
  `BASIC_OP_UNREDEFINED_P(BOP_PLUS, INTEGER_REDEFINED_OP_FLAG)`
  （ロード + AND + 分岐）を実行する。**実行のたびに検査する**が、対象は 1 ワード
  なので分岐予測がほぼ完全に効く。
- **YJIT / ZJIT** — `assume_bop_not_redefined()` で「その (class, bop) は未再定義」
  という *invariant* にコードブロックを登録する。再定義時は**その invariant に
  依存するブロックだけ**を無効化する。
- **通知** — Ruby 3.4+ は `-W:performance` で
  `Redefining 'Integer#+' disables interpreter and JIT optimizations` を出す。

つまり CRuby は**インタプリタでは実行時チェック、JIT では invariant + 局所無効化**
という二本立てで、どちらも粒度は (演算子, クラス) である。

---

## 3. 比較（`fib(29)` × 3 回、実測）

| | baseline | `Float#+` 再定義後 | `String#+` 再定義後 |
| --- | --- | --- | --- |
| CRuby | 0.062 0.061 0.059 | 0.059 0.059 0.061 | 0.058 0.059 0.059 |
| monoruby | 0.014 0.013 0.013 | **0.310 0.321 0.328** | 0.012 0.012 0.012 |

CRuby には**測定できるほどの影響が無い**。monoruby の `String#+` が無影響なのは、
そもそもフックが無い（未登録）からであって、良い意味ではない。

| 軸 | CRuby | monoruby |
| --- | --- | --- |
| 粒度 | (演算子, クラス) | グローバル 1 ビット |
| 検査方式 | fast path 内の実行時テスト（VM）/ invariant（JIT） | 事前の一括無効化 |
| 影響範囲 | その組を使うコードのみ | **プロセス全体** |
| 可逆性 | 不要（他が影響を受けない） | **不可逆** |
| 網羅 | 約 30 op × 十数クラス | **10 エントリ** |
| 未登録 op | そもそも fast path を持たない | **fast path はあるがフックが無い → 誤答** |
| 通知 | `-W:performance` | 無言 |

---

## 4. 評価 — 何が正しく、何が間違っているか

**戦略は正しい。** monoruby の JIT が採る「ガード無しでインライン展開し、
再定義時に無効化する」は YJIT と同じ方向であり、CRuby *インタプリタ*式の
「毎回フラグを読む」を JIT に持ち込むと、いま効いている定数畳み込みと
レジスタ常駐が成立しなくなる。ここは維持すべきである。

**間違っているのは粒度と網羅である。**

1. **粒度** — 「どれか 1 つでも再定義されたか」しか持たないので、反応は
   プロセス全体を落とすしかない。`bop_redefined_flags` が既に `u32` である
   にもかかわらず、0 か全ビットかの真偽値としてしか使われていない。
2. **網羅** — インライン展開している (クラス, 演算子) の大半が未登録で、
   再定義が**無言で無視される**。速い誤答は遅い正答より悪い。
3. **恒久性** — `jit_invalidated` も VM ディスパッチ表の差し替えも一方向で、
   回復手段が無い。粒度 1 の設計から必然的にこうなっている。

---

## 5. 方針

### Step 1 — 網羅を閉じる（正しさ、最優先）— **実装済み**

`globals/store/basic_op.rs` に **62 組の `(クラス, メソッド)` の静的表**
（`BASIC_OP_DEFS`）を置き、検知をそこに移した。CRuby の `vm_opt_method_defs`
と同型である。

設計上の要点は **判定キーを「上書きされたエントリのフラグ」から「(クラス,
メソッド) の組」に変えた**こと。§1.4 の内訳を見ると、`Integer#!` / `#+@` /
`#~`、`NilClass#==` などは**そのクラスにエントリを持たない**（`Object` /
`BasicObject` からの継承）。`class Integer; def !; …` は上書きではなく
**挿入**なので、いくらエントリにフラグを立てても旧方式では原理的に検知できない。
CRuby が静的表を引くのも同じ理由による。

あわせて:

- `remove_method` にも検知を入れた（§1.4 の但し書き。`undef_method` は
  `add_empty_method` 経由で `insert_method` を踏むので既に通っていた）。
- ブートストラップ用ラッチ `armed` を追加。ビルトイン自身の定義がこの表そのもの
  なので、`startup.rb` とgem のロードが終わるまで報告しない
  （`Store::arm_basic_ops`、`Executor` の `startup_flag` と同じ地点）。
- ディスパッチ表を持たない Rust 側の fast path（`runtime::{get_index,
  set_index}` の `Array#[]` / `Hash#[]` / `Array#[]=`）は `_no_opt` 版に
  差し替えられないので、`BasicOpTable::redefined` を直接読ませた。

**結果（実測）:**

| | 変更前 | 変更後 |
| --- | ---: | ---: |
| 演算子再定義スイープ（257 ケース） | 51 件が CRuby と食い違い | **0 件** |
| `remove_method(:+)` → `1 + 2` | `3` | `NoMethodError` |
| core 全体スペック（単一プロセス） | 347 F / 219 E | **345 F / 218 E** |
| `cargo test` | 59/59 | 59/59 |
| `--features emit-asm`（BOP 再定義なし） | — | **バイト単位で同一** |

### Step 1 が露出させたもの — Ruby 実装ビルトインの脆さ

再定義が**実際に効くようになった**結果、monoruby が Ruby で書いている
ビルトイン（`builtins/*.rb`、約 9,000 行）がその再定義を踏むようになった。

```ruby
class Integer; def <(o); :OV; end; end
[1, 2, 3].map { |x| x * 2 }
#   CRuby    => [2, 4, 6]        (Array#map は C)
#   monoruby => NoMethodError    (array.rb の `while i < size` が壊れる)
```

`Integer.remove_method(:<)` はさらに露骨で、`Comparable#<`（`comparable.rb`）が
`res < 0` で自分自身に再帰し StackOverflow になる。CRuby の `Comparable#<` は
C なのでディスパッチしない。

これは **Step 1 が作った欠陥ではなく、Step 1 が可視化した既存の構造的弱点**で
ある。変更前は再定義そのものが無視されていたので、ユーザのコードでも
ビルトインでも一様に「元の演算子」が使われ、辻褄だけは合っていた。

トレードオフを正直に言えば、Step 1 は「`1 < 2` が誤答」を「`1 < 2` は正答だが
`Array#map` が壊れる」に置き換えた。前者はサイレント、後者はラウド。
`cargo test` と ruby/spec には影響が無い（どちらも組み込みクラスの演算子を
再定義しない）ため実害は測定されていないが、**基本演算を monkey patch する
実プログラムは動かない**。

塞ぐには、`builtins/*.rb` の演算子をビルトインに束縛する（＝ CRuby が C で
書くことで無料で得ている性質を明示的に作る）必要がある。粒度とは独立の課題
なので Step 2 の前提ではないが、Step 1 の帰結として記録しておく。

### Step 2a — VM を (クラス) 粒度にする — **実装済み**

**鍵になった観測**: VM の asm fast path は**すべて fixnum 限定**である。
`vm_binops_opt` / `vm_cmp_opt!` / `vm_neg` … はどれも
`guard_rdi_rsi_fixnum` / `guard_rdi_fixnum` で始まり、fixnum でないオペランドは
必ず Rust ヘルパ（`add_values`、`cmp_lt_values`、`eq_values_vis`、`not_value`、
`get_index` …）に落ちる。

したがって:

- **Integer の再定義** → asm が無効になる → ディスパッチ表の差し替えが要る。
- **それ以外のクラス**（Float / String / Symbol / nil / true / false / Complex /
  Array / Hash）→ **asm は書かれたとおり正しいまま**。無効になるのは Rust
  ヘルパのそのクラス用アームだけなので、**ディスパッチ表に触る必要がない**。

実装は 2 点だけ。

1. `BasicOpTable` が「何が再定義されたか」を持つ（`redefined_set` と
   `integer_redefined`）。`Store::set_bop_redefine` は **Integer が初めて
   再定義されたときだけ** `remove_vm_bop_optimization()` を呼ぶ。
2. ネイティブに答える Rust ヘルパが、そのアームに入る前に
   `basic_op_redefined_for(受け手のクラス, 演算子名)` を確認する。
   グローバル bool で門番しているので、**再定義しないプログラムの追加コストは
   bool 1 個**（しかも呼び出し自体が既に C-ABI）。

対象ヘルパ: `binop_values!`（add/sub/mul）、手書きの `div_values` /
`rem_values` / `pow_values` / `shl_values` / `shr_values`、`cmp_values!`
（lt/le/gt/ge）、`eq_values_vis`、`cmp_teq_values_impl`、`not_value` /
`neg_value` / `pos_value` / `bitnot_value`、`get_index` / `set_index`。
（`!=` は `custom_neq` が既にメソッド表を引くので変更不要。）

**結果（`fib(29)` × 3、実測）:**

| 条件 | Step 1 まで | **Step 2a** |
| --- | ---: | ---: |
| baseline（JIT） | 0.009 | 0.009 |
| `Float#+` 再定義後 | 0.31 | **0.030** |
| `String#==` 再定義後 | 0.31 | **0.031** |
| `Array#[]` 再定義後 | 0.30 | **0.030** |
| `Symbol#==` 再定義後 | 0.30 | **0.030** |
| `Complex#*` 再定義後 | 0.31 | **0.030** |
| （参考）`--no-jit` | 0.030 | 0.030 |
| （参考）CRuby | 0.051 | 0.051 |

**再定義後の 0.030 は `--no-jit` の 0.030 と完全に一致する。** つまり VM 側の
劣化は完全に消え、残る 3.3 倍（0.009 → 0.030）は**まるごと JIT の
グローバル無効化**である。これが Step 2b の対象。

正しさは 257 ケースのスイープが 0 差分を維持、`cargo test` 59/59、core 全体
スペックは 345F/218E → **344F/218E**、`--features emit-asm` は Step 1 前の
master とバイト単位で同一のまま。

### Step 2b — JIT を invariant 単位にする（残り）

`jit_invalidated` のグローバル一方向ラッチをやめ、「この iseq がどの
(op, class) invariant に依存したか」を記録して該当分だけ無効化する。
`InlineCacheEntry` と class-version ラベルという前例がある。あわせて
`dispatch[14]`（`loop_start`）の no-opt 化と `immediate_eviction` の
レベルトリガ（§1.3）もここで直す。

### Step 2 — 粒度を (演算子, クラス) へ（性能）

`bop_redefined_flags: u32` を **BOP ごとのワード × クラスビット**に変える
（CRuby と同型）。

- **VM**: `_no_opt` への全面差し替えをやめ、各 fast path で該当ビットだけを
  テストする。fixnum パスに分岐 1 個を足すコストは、現状の 7.5 倍劣化に比べれば
  無視できる。
- **JIT**: `jit_invalidated` のグローバル一方向ラッチをやめ、「この iseq が
  どの (op, class) invariant に依存したか」を記録して該当分だけ無効化する。
  `InlineCacheEntry` と class-version ラベルという前例がある。
- **`immediate_eviction` は残す** — オンスタックのフレームを片付ける手段は
  粒度に関係なく必要である。ただし §1.3 の 2 性質を直す: 「フラグが非 0 か」の
  レベルトリガをやめて**マスクのビットが 0→1 に遷移したときだけ**走らせ
  （エッジトリガ）、走査対象もその (op, class) に依存したフレームに絞る。
- 効果: 「無関係な `Float#+` の再定義で fib が 24 倍遅くなる」が消える。

### Step 3 — refinements の基本演算（#1066）

Step 2 の (op, class) ビットマスクができれば、#1066 が要求する
「refinement セットごとの BOP ビットマスク」はその自然な拡張になる。

逆に、**今の 1 ビット設計のままでは #1066 は実装できない。** refinement は
「このレキシカルスコープでだけ再定義」であって、グローバルビットとは意味が
異なるためである。したがって **#1066 の前提として Step 1・2 を先に行うべき**で、
順序を逆にすると設計をやり直すことになる。

### 検討した候補 — `hash` / `eql?` を BOP にする（見送り）

`Hash` / `Set` のルックアップは `hash` → `eql?` と連鎖するので、これらを BOP に
すれば速くなるのではないか、という案。**測定した結果、利得はゼロだった。**

理由は BOP 表の性質そのものにある。表は「この仮定を置いてよい」という*許可*と
*検知*であって、速度は「仮定を使う fast path」を書いて初めて出る。そして
`hash` / `eql?` については **その fast path が既にあり、無条件に効いている** —
`Value::ruby_hash` / `Value::eql`（`value.rs`）が `ObjTy` で分岐して Rust 側で
直接計算する:

| | ネイティブ計算（ディスパッチ無し） |
| --- | --- |
| `ruby_hash` | Fixnum, Flonum, nil/true/false/Symbol, BigInt, Float, String, Array, Hash, Range |
| `eql` | 同一 id, 両方 immediate, BigInt, Float, Complex, String, Array, Range, Hash |

基本クラスのキーには消せるディスパッチが最初から無い。登録しても得るものは無く、
グローバル崖のトリップワイヤが増えるだけである。正しさの穴も無い（`String#hash`
を再定義しても Hash のバケッティング・`Array#hash` とも CRuby と一致する。CRuby
も基本クラスのキーは `rb_str_hash` 等で直接計算する）。

実測（各 300k 回ルックアップ）:

| | monoruby | CRuby |
| --- | ---: | ---: |
| `Hash[String]` | **0.014 s** | 0.028 s |
| `Hash[Symbol]` | **0.011 s** | 0.016 s |
| `Hash[Integer]` | 0.016 s | 0.014 s |
| `Hash[ユーザ定義 hash/eql?]` | **0.030 s** | 0.100 s |
| `Set#include?` | 0.016 s | 0.017 s |

**ただし派生案は有効。** `hash` / `eql?` を定義していない素のオブジェクト
（identity ハッシュ）は `ruby_hash` の `_` アームに落ち、**ルックアップのたびに
`Object#hash` へディスパッチしている**。5 項目中ここだけ monoruby が負ける:

| | monoruby | CRuby |
| --- | ---: | ---: |
| `Hash[素のオブジェクト]` | 0.031 s | **0.023 s** |
| `Set#include?[素のオブジェクト]` | 0.031 s | **0.024 s** |
| `obj.hash` 単体 | **0.008 s** | 0.016 s |

`obj.hash` 単体では monoruby が 2 倍速いので、差はハッシュ計算ではなく
**ルックアップ 1 回ごとのディスパッチ往復**にある。`(Object, hash)` を表に入れ、
`_` アームで「このクラスの `hash` はまだ `Object#hash` か」を確かめてから
identity ハッシュを直接計算すれば省ける（`eql` 側は既に `id()` 比較で短絡済み）。

**これは Step 2 の後に行う。** グローバル 1 ビットでは「どこかで誰かが `hash` を
定義した」しか分からず、`Object#hash` に触れていない大多数のクラスまで巻き添えに
なる。(演算子, クラス) 粒度が前提となる派生案である。

### 併せて検討するもの

- **`-W:performance` 相当の警告** — 実装は小さく、デバッグ価値は高い。現状の
  monoruby は完全に無言で 24 倍遅くなる。
- **Ruby 実装ビルトインの露出**（§1.4 但し書き）— BOP とは別問題だが、Step 1 で
  誤答が減ると顕在化しやすくなるので、同時に把握しておく。

### 検討して採らなかった案

- **JIT にも実行時チェックを入れる（CRuby インタプリタ方式）** — 定数畳み込みが
  できなくなる。invariant + 無効化を維持する。
- **`jit_invalidated` を解除可能にする（再コンパイル許可）** — 複雑な割に、
  Step 2 を行えば「そもそも無効化されない」ので不要になる。
- **basic op 登録をやめて全演算子をメソッド呼び出しにする** — 正しさは得られるが、
  monoruby の性能特性そのものを捨てることになる。

---

## 6. 受け入れ条件（Step 1・2 共通）

`doc/refinements.md` で用いた手法をそのまま使う。

1. **コード生成の同一性** — BOP 再定義を含まないワークロードで
   `--features emit-asm` の出力が変更前とバイト単位で一致すること。
2. **差分テスト** — §1.4 の 48 ケースが CRuby と一致すること（`Array#size` を除く）。
3. **性能回帰なし** — `fib` / optcarrot が baseline を維持すること。
4. **Step 2 の効果** — `Float#+` 再定義後の `fib` が baseline に戻ること
   （現状 0.51 s → 目標 0.022 s 近傍）。
