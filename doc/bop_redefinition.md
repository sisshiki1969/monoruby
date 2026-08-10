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

## 1. 現行実装

### 1.1 「basic op」として登録されているもの

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
5. `Codegen::check_bop_redefine` → `immediate_eviction` — オンスタックの
   フレームの return address を deopt にパッチし、戻ってきた時点で VM に落とす。

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

### Step 1 — 網羅を閉じる（正しさ、最優先）

現在インライン展開している**すべての (クラス, 演算子)** を basic op として
登録する。既存の仕組みに乗せるだけで、粒度は変えない。

- 効果: 21 件の誤答が解消する（`Array#size` の 1 件は §1.4 の但し書きの通り別件）。
- 副作用: これまで無言で無視していた再定義が全体 deopt を起こすようになるため、
  **遅くなるケースは増える**。Step 2 とセットが望ましい。
- それでも Step 1 単独で実施する価値はある。「速い誤答」より「遅い正答」を採る。

### Step 2 — 粒度を (演算子, クラス) へ（性能）

`bop_redefined_flags: u32` を **BOP ごとのワード × クラスビット**に変える
（CRuby と同型）。

- **VM**: `_no_opt` への全面差し替えをやめ、各 fast path で該当ビットだけを
  テストする。fixnum パスに分岐 1 個を足すコストは、現状の 7.5 倍劣化に比べれば
  無視できる。
- **JIT**: `jit_invalidated` のグローバル一方向ラッチをやめ、「この iseq が
  どの (op, class) invariant に依存したか」を記録して該当分だけ無効化する。
  `InlineCacheEntry` と class-version ラベルという前例がある。
- 効果: 「無関係な `Float#+` の再定義で fib が 24 倍遅くなる」が消える。

### Step 3 — refinements の基本演算（#1066）

Step 2 の (op, class) ビットマスクができれば、#1066 が要求する
「refinement セットごとの BOP ビットマスク」はその自然な拡張になる。

逆に、**今の 1 ビット設計のままでは #1066 は実装できない。** refinement は
「このレキシカルスコープでだけ再定義」であって、グローバルビットとは意味が
異なるためである。したがって **#1066 の前提として Step 1・2 を先に行うべき**で、
順序を逆にすると設計をやり直すことになる。

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
