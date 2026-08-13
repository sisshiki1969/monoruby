# Polymorphic call サイトの最適化 — 設計メモ

**Kind**: plan(第一歩の `nil?` 耐性ガードは実装済み・このブランチに同梱。
一般化はここでの設計判断待ち)

このメモは、ruby-bench の binarytrees / protoboeuf-encode 調査
(String#`<<` インライン化・trailing-splat fast path は #1109 で master 入り)
の過程で見つかった **レシーバ多相なコールサイトの deopt 問題** について、
実測・実装済みの第一歩・残された設計空間をまとめたものである。

---

## 1. 現状のコールサイト設計と、その盲点

JIT のメソッド呼び出しは **monomorphic 前提** で組み立てられている:

1. VM がインラインキャッシュを温める(receiver class → FuncId)。
2. JIT はキャッシュされた 1 クラスで **レシーバクラスガード** を発行し、
   ガードの内側で FuncId を確定させ、インライン生成器
   (`inline_info.get_inline`)か直接呼び出しへ落とす。
3. ガード失敗は deopt。`BinCmp` サイトのみ、ミス回数が温まると
   `RecompileReason::BecamePolymorphic` で「多相対応版」へ再コンパイル
   する経路(Part B)がある。ふつうのメソッド呼び出しにはこれが無く、
   **ガード失敗は永久に毎回 deopt** する。

盲点は「nil との二相」である。Ruby の慣用句はレシーバが nil かもしれない
場所でこそメソッドを呼ぶ:

```ruby
return 1 if left.nil?        # left は Array か nil(binarytrees の item_check)
limit = to unless to.nil?    # to は Numeric か nil(numeric.rb の step)
step ||= 1
if by == nil ...             # 同型の == バリアント
```

このため `nil?` / `== nil` サイトは **設計上 50/50 で二相** になり、
毎回 deopt が観測された。

## 2. 実測(2026-08、x86-64 Linux、release)

- binarytrees(#1109 適用後): `item_check` の `left.nil?` サイトで
  **1 実行あたり 458 万回の deopt**(`profile` フィーチャで観測。
  deopt ログは `POLYMORPHIC [NilClass]`)。
- マイクロベンチ(2^16 ノードの木 ×40 周、YJIT 比):

  | 変種 | monoruby(修正前) | 修正後(§3) | CRuby+YJIT |
  |---|---|---|---|
  | `left.nil?` | 353ms | **64ms** | 87ms |
  | `left`(truthiness) | 317ms | 62ms | 90ms |
  | `left == nil` | 885ms | **489ms(未対策)** | 193ms |

- ベンチ全体: binarytrees 118ms → **100ms**(−15%)。
- `== nil` が truthiness より 2.8 倍遅いのは、`==` サイトも同じ
  nil/非 nil 二相 deopt を踏んでいるからで、**未回収の最大候補**。

## 3. 実装済みの第一歩: `nil?` サイトの nil 耐性ガード

このブランチに含まれる(master 未マージ)。構成は 3 点:

### 3.1 `GuardClassOrNil`(AsmIR / LIR / 両アーキ lowering)

「レシーバが nil なら通過、そうでなければ従来のクラスガード」。
nil ケースは比較 1 回+分岐 1 回で、deopt と違い定常コストがほぼ無い。

```
cmp  recv, NIL_VALUE
jeq  nil_ok
<従来の guard_class(recv_class), 失敗は deopt>
nil_ok:
```

### 3.2 フロントエンドのゲート(`compile_method_call`)

適用条件(健全性の根拠ごと):

- サイト名が `nil?`、simple、引数 0。
- `recv_class != NIL_CLASS`(nil 単相なら従来ガードで十分)。
- **`jit_check_call(NIL_CLASS, name)` の解決 FuncId が、キャッシュ
  クラスでの解決 FuncId と一致**すること。これが健全性の核で、
  「nil が来ても非 nil が来ても呼ぶべきメソッドは同一」をコンパイル時に
  証明する。直前に発行済みの class version guard が再定義を無効化する。
  `NilClass#nil?` だけが再定義されていれば一致せず、自動的に従来経路へ
  戻る。第三のクラス(独自 `nil?` を持つ等)は従来どおり deopt して
  正しくディスパッチされる。
- ガード通過後、**abstract state は refine しない**(レシーバは
  「nil か recv_class」なので、クラスを確定させたら嘘になる)。
  未確定状態の `kernel_nil` 生成器は `recv == nil` の値比較
  (`is_nil_to_bool`)を出すので、これがそのまま両ケースの正解になる。

### 3.3 副作用: page 規律との衝突(修正済み、ただし全面解消ではない)

state を refine しないことの下流コスト: `to.nil?` の後で `to` を Float
として使うコード(numeric.rb `step`)では、Float unbox(ガード付き
Value→f64)が **cold ブロック(page 1 に out-of-line 発行される基本
ブロック)内** に現れるようになった。x86-64 バックエンドの多くの
lowering は「hot(page 0)から呼ばれ、自分の cold スニペットを page 1 に
置く」前提で `assert_eq!(0, get_page())` を持っており、ここで abort した
(`numeric_step` テストで決定的に再現)。

クラッシュ経路の `float_to_f64` / `float_val_to_f64` は、`guard_class` の
fail ラッパが既にやっている二形態(**page 0 なら cold スニペットを
page 1 へ、page 1 で発行中ならその場に inline 化して飛び越す**)に直して
解消した。ただし同種の assert は `binary_op.rs` 等にも十数箇所残って
おり、「refine しない state を広げる」設計はこの規律と面で衝突する
(§5.4)。

## 4. なぜ `nil?` に限定したか

仕組み(FuncId 一致検証 + `GuardClassOrNil`)自体は名前非依存だが、
ガード通過後に走る **インライン生成器の前提** が問題になる。生成器の
多くは「ガード済み recv_class のレシーバ」を前提に書かれている
(例: `Hash#[]` は `as_hash` へ直行)。nil が素通りすれば即クラッシュ
なので、無条件の一般化は生成器全数の nil 安全性監査を要求する。
`kernel_nil` は未確定状態で値比較を出すだけなので唯一安全と言い切れた。

## 5. 設計空間(これから決めること)

### 5.1 nil 耐性ガードの一般化 — 最小コストで `== nil` を回収する

適用条件を「名前 = `nil?`」から次へ広げる:

> `jit_check_call(NIL_CLASS, name) == func_id` **かつ**
> (a) `func_id` にインライン生成器が無い、または
> (b) 生成器が nil 安全(`InlineFuncInfo` に flag を追加)

(a) が成り立つ場合、通常呼び出しは FuncId 直接ディスパッチなので
レシーバが nil でも健全(フレーム構築・visibility はクラス非依存)。
候補: `==`(NilClass は独自 `==` を持つため一致しないケースが多い点に
注意 — 要確認)、`is_a?` / `kind_of?`、`respond_to?`、`frozen?` など
Object/Kernel から継承される述語群。`left == nil` の 885→489ms(§2)が
最初の回収目標。

コスト: 小。リスク: 生成器 flag の付け間違い。§3 の機構をそのまま使う。

### 5.2 Polymorphic Inline Cache(N-way ディスパッチ)

一般解。サイトごとに観測クラス列 (class, FuncId) を 2〜4 way まで持ち、
線形比較チェーンで分岐、全ミスで deopt/再コンパイル。`BinCmp` の
Part B(`BecamePolymorphic` 再コンパイル)を method call 全般へ拡張する
形が自然で、「初回は monomorphic でコンパイル → ミスが温まったら
polymorphic 版へ再コンパイル」という既存のポリシーに乗る。

論点:

- **インライン生成器との併用**: way ごとに生成器を展開するか(コード
  サイズ増)、polymorphic サイトは生成器を捨てて直接呼び出しに統一
  するか。nil way だけ値比較特例(§3)を残す折衷が現実的。
- **abstract state**: way ごとに事後状態が異なる。合流で全 way の
  join を取る(≒ Value に落ちる)なら §5.3 の問題に帰着する。
- 観測クラスの記録場所: 現行のインラインキャッシュは 1 エントリ。
  deopt 側で「ガードを外したクラス」を蓄積する器が要る。

### 5.3 abstract state に union を持たせるか

現在の `Guarded` ラティスは単一クラス(+ Fixnum/Float 系の特例)。
`nil?` 対応で「refine しない」を選んだが、`recv_class ∪ NilClass` の
ような 2 要素 union を表現できれば、非 nil 側の下流(`unless to.nil?`
の then 側など)は分岐条件から `recv_class` 単相へ **絞り直せる**。
truthiness 分岐(`if x` / `unless x.nil?`)で nil を落とす flow-sensitive
な絞り込みは、§5.1/5.2 のどちらを選んでも効く直交した改善。ただし
ラティス拡張は merge/bridge 全体に波及するので、最も工数が大きい。

### 5.4 page 規律の全面解消(前提整備)

「cold ブロック内で page-1 スニペットを使う lowering が走り得る」は、
refine しない state を広げるほど踏みやすくなる。選択肢:

1. §3.3 の二形態化を、assert を持つ全 lowering(`binary_op.rs` ほか
   十数箇所)へ横展開する。機械的だが確実。
2. LIR 化(`doc/lir.md`)の進行に合わせ、cold スニペット配置を encoder
   の責務にして assert 自体を消す。方向としては正しいが待ちが長い。

§5.1 だけなら影響面は小さい(`==` 述語群の下流は Float unbox を含み
にくい)が、§5.2/5.3 をやるなら 1. を先に済ませるべき。

### 5.5 やらないと決めたこと

- **ガード全廃(値比較のみで nil? を実装)**: 「どのクラスも `nil?` を
  再定義していない」というグローバル性質が必要で、Kernel を include
  しない BasicObject 系レシーバ(`nil?` 呼び出しは NoMethodError に
  なるべき)で誤答する。クラスガードは第三クラスの安全網として残す。

## 6. 提案する順序

1. **§5.1 の最小一般化**(nil-safe flag + `==` / 述語群) — `== nil`
   885→489ms の残り半分を回収。計測: §2 のマイクロベンチと
   binarytrees。
2. **§5.4-1 の page 二形態化の横展開** — §3.3 と同じパターンの機械的
   適用。gc-stress full を再実行して検証。
3. **§5.2 の PIC + BecamePolymorphic 拡張** — optcarrot 等、真に
   多クラスなワークロードで設計・計測。§5.3(union 型)はその結果を
   見てから判断。

## 7. このブランチに含まれる実装

- `JIT: nil-tolerant receiver guard for nil? call sites` — §3.1/3.2。
- `x86_64: make the Float-unbox guards page-tolerant` — §3.3。

検証済み: フルスイート green(§3.3 修正後)、`Weird#nil?` 等の第三
クラス・オーバーライドは CRuby と一致、binarytrees 118→100ms。
