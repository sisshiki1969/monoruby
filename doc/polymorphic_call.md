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

### 5.2 Polymorphic Inline Cache(N-way ディスパッチ)→ **採用方針。§7 の調査記録を参照**

一般解。サイトごとに観測クラス列 (class, FuncId) を 2〜4 way まで持ち、
線形比較チェーンで分岐、全ミスで deopt/再コンパイル。`BinCmp` の
Part B(`BecamePolymorphic` 再コンパイル)を method call 全般へ拡張する
形が自然で、「初回は monomorphic でコンパイル → ミスが温まったら
polymorphic 版へ再コンパイル」という既存のポリシーに乗る。

**方針決定(2026-08)**: VM が実行時に polymorphic を検出して
バイトコードへ書き込む既存機構(`opcode_sub`)を土台に、
`CallSiteInfo` に 4-way 程度の PMC を蓄積し JIT コンパイル時に利用する。
`nil?` はこの一般機構で自然に扱える(§7.2)。`==` など二項演算子命令へ
の適用可否は §7.3 の調査結果のとおり「検出機構は既にあり有効、
ただしキャッシュがペア形で置き場所の追加が要る」。

論点(§7 の調査で一部解決):

- **インライン生成器との併用**: way ガード通過後はレシーバクラスが
  確定するので、way ごとに生成器を適用しても ③ の unrefined-state
  問題は起きない。ただし最小実装は「全 way が同一 FuncId のときだけ
  生成器適用(`nil?` がこれ)、それ以外は way → 直接 FuncId 呼び出し」。
- **abstract state**: way ごとに事後状態が異なる。合流で全 way の
  join を取る(≒ Value に落ちる)なら §5.3 の問題に帰着する。
  最小実装はチェーン全体を 1 命令として扱い、結果は Value で合流。

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

## 6. 提案する順序(§7 の方針決定で改訂)

1. **VM 駆動 4-way PMC(メソッド呼び出し)** — §7.2 の実装点に沿って
   `CallSiteInfo` に PMC を蓄積し、JIT が `_polymorphic` サイトで
   ガードチェーンを発行する。`nil?` はこの一般機構に吸収され、§3 の
   `GuardClassOrNil` 特例は「全 way 同一 FuncId の縮退形」として整理
   し直す(または撤去する)。
2. **§5.4-1 の page 二形態化の横展開** — way 分岐後のコード配置が
   cold 側へ広がるため、PMC より先に済ませるのが安全。
3. **二項演算子命令へのペア PMC 拡張** — §7.3。`== nil`
   (現況 489ms vs YJIT 193ms)を計測基準にする。

## 7. 調査記録: VM 駆動 4-way PMC の実装点(2026-08)

### 7.1 既存の検出機構(前提の確認)

VM は **メソッド呼び出し・二項演算の両方で** polymorphic を実行時検出
し、バイトコードの `opcode_sub` バイトに書き込む機構を既に持つ:

- **メソッド呼び出し**(`vmgen/method_call.rs` slow_path):
  インラインキャッシュ(バイトコード内の CACHED_CLASS / VERSION /
  FUNCID、1 エントリ)が populated かつ receiver class 不一致のとき
  `opcode_sub = 1`。直後に必ず `runtime::find_method(vm, globals,
  callid, recv)` が呼ばれ、(class_for_ic, FuncId) を解決してキャッシュを
  再タグ付けする。
- **二項演算**(`vmgen.rs` `vm_save_binary_class`):
  バイトコード内 IC に (lhs_class, rhs_class) の 1 ペアを保存し、
  どちらかの変化を検出したら `opcode_sub = 1`。

TraceIR はどちらも読み取り済みで、`BinCmp`/`BinCmpBr`/`BinOp` は
`polymorphic` を **消費している**(単相 → ペアガード +
recompile-on-miss(Part B)、多相 → ガードなし generic C-call +
Eq/Ne は即値 fast path(Part C))。一方 `TraceIr::MethodCall` の
`_polymorphic` は **未使用**(`compile.rs` で `_polymorphic: _`)。
つまりメソッド呼び出し側は「検出はあるが JIT が利用していない」状態で、
PMC 案はまさにこの穴を埋める。

### 7.2 メソッド呼び出し側の実装点

1. **記録**: `runtime::find_method` は `&mut Globals` と `CallSiteId` を
   受けて解決結果 (cache_class, fid) を計算済みなので、ここで
   `CallSiteInfo` に固定長 4-way の `(ClassId, FuncId)` 列を追記するのが
   最小変更。既存の `class_for_ic`(Bool 統合)と `cacheable = false`
   (frame-dependent super — class タグ 0 で毎回再解決)の扱いを
   そのまま流用でき、cacheable でないサイトは PMC 対象外にマークする。
   5 クラス目が観測されたら megamorphic フラグ(PMC 打ち切り、JIT は
   ガードなし generic dispatch を選ぶ)。
2. **消費**: `compile_method_call` で `_polymorphic` が真なら、単一
   キャッシュではなく PMC を読み、way ごとの class 比較チェーンを発行:
   - way ガード通過後はレシーバクラスが確定するので、既存の
     monomorphic 経路(インライン生成器を含む)を way 内で再利用できる
     余地がある。ただし最小実装は「way → FuncId 直接呼び出し、結果は
     Value で合流」。
   - **同一 FuncId の way は 1 経路に畳む**。`nil?` は全 way が
     Kernel#nil? に解決されるため、畳んだ結果が §3 のガードレス値比較と
     一致する — nil? 特例が一般機構の縮退形になる、というのがこの案の
     利点。
   - 全 way ミス: deopt(+ 観測が増えたら再コンパイル)か、その場で
     generic dispatch へ落とすかは選択。Part B の単調再コンパイル
     不変量(多相化は一方向)に合わせるなら後者。
3. **無効化**: PMC エントリは class version guard の傘の下にある
   (再定義で全サイト再コンパイル)ので、エントリ個別の無効化は不要。

### 7.3 二項演算子命令(`==` ほか)への適用可否

**有効。ただしキャッシュの形と置き場所が method call と異なる。**

- 検出(`opcode_sub`)は §7.1 のとおり既にあり、多相サイトは現在
  「ガードなし generic C-call」へ落ちている。`left == nil`
  (left: Array/nil 二相)の実測 489ms(YJIT 193ms)の残りコストは
  この generic C-call: Part C の即値 fast path は **両オペランドが
  非 heap** のときだけ効くので、Array 側の呼び出しが毎回
  `cmp_eq_values`(メソッド解決+実行)を払う。
- binop の IC は (lhs_class, rhs_class) **ペア**で、バイトコード内の
  8 バイト(2×u32)に 1 ペアしか置けない。4-way 化はバイトコード内では
  無理なので、method call と同じくサイド構造に置く。置き場所の候補:
  - binop サイトの一部は `IseqInfo::callsite_map`(bc_pos → CallSiteId)
    で `CallSiteInfo` に到達できる(polymorphic 分岐が is_func_call
    判定に既に使っている)。ペア PMC を `CallSiteInfo` に置くなら
    これに乗るが、**全 binop 命令に callsite があるかは未確認**
    (`get_callsite_id` は Option)。無いサイトには `IseqInfo` 側に
    bc_pos キーのテーブルを足す。
  - 記録タイミング: `vm_save_binary_class` はアセンブラ内でクラス保存
    まで。ペア追記は set_poly 分岐(既に slow path)から Rust ヘルパを
    呼ぶ形になる。generic C ヘルパ(`cmp_eq_values` 等)は pc を
    受けない規約なので、記録はヘルパ側でなく VM 命令側で行う。
- way 特化の価値: 第一段階は「ペアガード → FuncId 直接呼び出し」で
  解決コストを消すだけでよい。第二段階として、組み込みが確定する
  ペアには特化コードの余地がある(例: (Array, NilClass) の `==` は
  Array#== が組み込みなら定数 false)。どちらが効くかは
  `cmp_eq_values` の内部コスト内訳(解決 vs 本体実行)を測ってから
  決める。

### 7.4 検証結果: callsite 記録と poly 検出の全数確認(2026-08)

**callsite 記録(`bytecodegen/encode.rs`)**: UnOp(121-124)/
BinOp(160+)/ Cmp 両形(140-146, 150-156)/ Index(132)/
StoreIndex(133)は全て `new_callsite` + `new_callsite_map_entry` で
`CallSiteId` に到達できる。例外は **RescueTEq(157)のみ**(rescue 節
マッチ専用、常にランタイムヘルパ経由で IC なし — PMC 対象外で問題ない)。
`get_callsite_id` が `Option` なのはこの 1 命令のためで、ペア PMC を
`CallSiteInfo` に置く設計は追加テーブルなしで成立する。

**VM の poly 検出(x86_64 / aarch64 でミラーを確認)**:

| 経路 | 保存 | `opcode_sub` 検出 |
|---|---|---|
| binop/cmp generic(`vm_generic_binop` → `vm_save_binary_class`) | ✓ | ✓ |
| binop/cmp fixnum fast path(`vm_save_binary_integer`) | 上書きのみ | ✗(無害 — 下記) |
| unop generic(`vm_generic_unop` → `vm_save_lhs_class`) | ✓ | **✗ 欠落** |
| unop fixnum fast path(`vm_lhs_integer`) | 上書きのみ | ✗ |
| Index / StoreIndex(`runtime::get_index`/`set_index` が ClassIdSlot へ無条件代入) | ✓ | **✗ 欠落** |
| メソッド呼び出し slow path | ✓ | ✓ |

- binop/cmp fast path の欠落は実質無害: fast path しか通らないサイトは
  単相で、多相サイトは必ず generic を通ってそこで検出される(最大
  1 実行遅れるだけ)。**binop/cmp は現状のまま PMC の前提を満たす**。
- **unop の検出欠落 → 実装済み**(このブランチ): `vm_save_lhs_class` /
  `a64_save_lhs_class` に `vm_save_binary_class` と同じ
  「キャッシュ populated かつクラス変化 → `opcode_sub = 1`」を追加した
  (generic 側のみ、両アーキ)。`TraceIr::UnOp` にも `_polymorphic` を
  配線済み(JIT 消費は PMC 本実装で)。
- **Index/StoreIndex の検出欠落 → 実装済み**(このブランチ): クラス
  記録をランタイムヘルパから **VM 命令内の機械語へ移した**。binop と
  同じ `[pc+8]`/`[pc+12]` レイアウトなので `vm_save_binary_class` /
  `a64_save_binary_class` をそのまま流用でき、検出も同時に付く。
  `get_index` / `set_index` は `ClassIdSlot` ポインタ引数(bit 0 に
  is_func_call を折り込むハック)を廃止して素の `is_func_call` を
  受ける形に単純化した。`TraceIr::Index` / `IndexAssign` にも
  `_polymorphic` を配線済み。Index は binop と違い fixnum fast path を
  持たないため、検出は全実行に効く(遅延なし)。

### 7.5 実装済み: PMC の記録と profile ダンプ(2026-08)

記録側を実装した(JIT 消費は未着手)。設計:

- **格納**: `CallSiteInfo::pmc: PolyCache`(`store.rs`)。最大
  `PMC_WAYS = 4` エントリ `(recv, Option<arg>, Option<fid>, count)` +
  megamorphic overflow カウンタ。キー形状は
  **MethodCall / UnOp = レシーバのみ、BinOp / Cmp / Index /
  StoreIndex = レシーバ + 第一引数**。
- **記録点(すべて slow path のみ、定常状態のコストゼロ)**:
  - MethodCall: `runtime::find_method`(単一エントリキャッシュのミス時に
    呼ばれる)で `class_for_ic` と解決済み FuncId を記録。
    `cacheable = false`(frame-dependent super)は記録しない。
  - BinOp / Cmp / Index / StoreIndex: `vm_save_binary_class` /
    `a64_save_binary_class` の「初回 population」と「poly 遷移」の 2 分岐
    から `runtime::pmc_record_binary(vm, globals, pc)` を呼ぶ。クラスは
    直前にバイトコード IC へ書いた値を pc から読み戻すので値渡し不要。
    callsite は `cfp → iseq → get_pc_index → callsite_map` で解決
    (record 分岐のみのコスト)。
  - UnOp: 同様に `vm_save_lhs_class` / `a64_save_lhs_class` から
    `pmc_record_unary`(レシーバのみ)。
- **ダンプ**: `--features profile` の終了時統計(`Store::show_stats`)に
  「polymorphic method cache」節を追加。サイト数サマリ
  (recorded / polymorphic / megamorphic)と、slow-path 観測数順の
  多相サイト top 40 を `class(/arg)(=resolved-func) xN` 形式で表示。
  count は **slow-path 観測数**(fast path は記録しないので呼び出し
  総数ではない)。
- 実測例(gem 起動込みの小スクリプト): 3,984 サイト記録・35 多相・
  13 megamorphic。`initialize` / `__builtin_allocate__`(overflow 196)や
  Errno 網羅の `is_a?`(overflow 130)が megamorphic として正しく
  弁別され、`nil?` は `Array=Kernel#nil? | NilClass=Kernel#nil? | …` と
  全 way 同一 FuncId が観測できる — §7.2 の「同一 FuncId way の畳み込み」
  の実データがそのまま得られる。

## 8. このブランチに含まれる実装

- `JIT: nil-tolerant receiver guard for nil? call sites` — §3.1/3.2。
  §6-1 の一般機構が入った時点で縮退形として整理し直す予定。
- `x86_64: make the Float-unbox guards page-tolerant` — §3.3。

検証済み: フルスイート green(§3.3 修正後)、`Weird#nil?` 等の第三
クラス・オーバーライドは CRuby と一致、binarytrees 118→100ms。
