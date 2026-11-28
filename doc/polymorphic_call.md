# レシーバ多相なコールサイト

**Kind**: reference。レシーバのクラスが実行ごとに変わるコールサイトを JIT が
どう扱うかをまとめる。観測（VM の多相検出と PMC）、消費（集合ガード、PIC、
二項演算・`[]` の dispatch、generic send の残余）、残っている設計上の論点の
順に述べる。

自フレームの self クラスによる本体の切り替え（メソッド入口のクラス dispatch
stub と megamorphic gate）は別の仕組みで、`docs/src/jit-compiler.md` と
`codegen/class_dispatch.rs` を参照。

---

## 1. 背景: 単相前提のガード

JIT のメソッド呼び出しは、VM のインラインキャッシュ（receiver class →
FuncId、1 エントリ）を元に組み立てる。キャッシュされたクラスでレシーバ
クラスガードを発行し、ガードの内側で FuncId を確定させ、インライン生成器
（`inline_info.get_inline`）か直接呼び出しへ落とす。単相のサイトではこれで
十分だが、多相のサイトではガード失敗のたびに side exit する。

典型は nil との二相である。Ruby の慣用句はレシーバが nil かもしれない
場所でこそメソッドを呼ぶ。

```ruby
return 1 if left.nil?        # left は Array か nil（binarytrees の item_check）
limit = to unless to.nil?    # to は Numeric か nil（numeric.rb の step）
if by == nil ...
```

二項演算子でも同じことが起きる。rubykon の `group_id_of(id) ==
captured.identifier` は左辺が `Array` の要素で、ヒットなら `Integer`、
ミスなら `NilClass` になる。

## 2. VM による多相の検出

VM は実行時に多相を検出し、バイトコードの `opcode_sub` バイトに書き込む。
TraceIR はそれを `polymorphic` として読む。

| 経路 | クラスの保存 | `opcode_sub` の検出 |
|---|---|---|
| メソッド呼び出し slow path（`vmgen/method_call.rs`） | ✓ | ✓ |
| binop / cmp generic（`vm_generic_binop` → `vm_save_binary_class`） | ✓ | ✓ |
| binop / cmp fixnum fast path（`vm_save_binary_integer`） | 上書きのみ | ✗（多相サイトは必ず generic を通るので無害） |
| unop generic（`vm_save_lhs_class` / `a64_save_lhs_class`） | ✓ | ✓ |
| Index / StoreIndex（VM 命令内で `vm_save_binary_class` を流用） | ✓ | ✓ |

callsite はメソッド呼び出しのほか UnOp / BinOp / Cmp / Index / StoreIndex
にもあり（`bytecodegen/encode.rs` の `new_callsite` +
`new_callsite_map_entry`）、`IseqInfo::callsite_map` で `CallSiteInfo` に
届く。例外は `RescueTEq` だけである（rescue 節のマッチ専用で IC を持たない）。

## 3. 集合ガード — 観測クラスが同じメソッドに解決するサイト

観測したクラスがすべて同じ **native** メソッドに解決するサイト（`nil?`、
`is_a?`、`frozen?` …）は、クラス集合のメンバーシップガード `GuardClassIn`
（`pmc_same_target_classes`）の後で通常呼び出しを出す。FuncId は
`jit_check_call` で再計算し、PMC が保存した値は使わない。ガードを通った
クラスはすべて同じメソッドに解決し直してあるので、クラスごとに呼び分ける
必要がない。iseq ターゲットはレシーバクラスで特殊化されるため対象外である。

集合ガードの後でもインライン生成器は発火しうる（§9）。

## 4. PIC — クラスごとに解決先が違うサイト

観測クラスが別々のメソッドに解決するサイトは、`compile/pic.rs` が N-way の
クラス dispatch を出す（`compile_pic_call`、最大 `PIC_WAYS` = `PMC_WAYS`
アーム）。

```text
        load recv -> rdi
        guard_class_version                 (全アームを支配する 1 回)
        br_class_ne rdi, C0 -> L1
        <call C0#name>
        br merge
  L1:   br_class_ne rdi, C1 -> L2
        <call C1#name>
        br merge
  L2:   guard_class rdi, C2 -> 残余         (最終アーム)
        <call C2#name>
  merge:
```

合流は `compile/dispatch.rs` のブロック内マージで、**宣言型**である。分岐の
前にオペランドをスタックへ flush し、`dst` を未知の `Value` とし、アームが
任意の Ruby を実行しうるのでキャッシュ済みの invariant をすべて落とした
状態を合流状態として先に決める。各アームはそこへ bridge する
（`AbstractState::gen_bridge_all`）。アームの事後状態が揃っている必要は
ないので、ゲートは健全でない形とコンパイルを abort させる形だけを除外すれば
よい。

PMC が 1 クラスしか持たないサイトは、abstract state がレシーバを特定でき
なくても単相とみなして単一ガードのままにする。実測ではこれがゲートの拒否
理由の大半を占める。

## 5. 設計上の論点

### 5.1 合流状態の精度

宣言型の合流は精度を失う。2 つのアームがどちらもレジスタに `Fixnum` を
残しても合流でボックス化される。結果が不透明な `Value` で終わる呼び出しの
dispatch では損失はないが、数値の dispatch（fixnum アーム / float アーム /
generic 残余）では効く。格子には `LinkMode::Sf(fpr, SfGuarded::FixnumOrFloat)`
（ボックス化したスロット + 生きた float の影）という要素があるので、
`declare_merge` に呼び出し側が合流形を渡せるようにするのが次の段階である。

### 5.3 abstract state に union を持たせるか

`Guarded` 格子は単一クラス（+ Fixnum/Float 系の特例）しか表現しない。
`recv_class ∪ NilClass` のような 2 要素 union を表現できれば、非 nil 側の
下流（`unless to.nil?` の then 側など）を分岐条件から `recv_class` 単相へ
絞り直せる。truthiness 分岐で nil を落とす flow-sensitive な絞り込みは、
集合ガードとも PIC とも直交する改善である。ただし格子の拡張は merge / bridge
全体に波及するので、工数は最も大きい。未実装。

### 5.5 採らない案

- **`nil?` のガード全廃（値比較のみ）** — 「どのクラスも `nil?` を再定義して
  いない」というグローバル性質が必要で、`Kernel` を include しない
  `BasicObject` 系レシーバ（`nil?` は NoMethodError になるべき）で誤答する。
  クラスガードは第三のクラスに対する安全網として残す。

## 6. 二項演算子と `[]` の dispatch

二項演算子の多相サイトは、ホットなクラスをインラインのアームに、それ以外を
generic な C 呼び出しの残余に振り分ける（`binary_dispatch`、
`dispatch_inline_class`）。片方だけでは足りない。単一クラスガードで
インライン化すると他のクラスで毎回 side exit し（rubykon の `==` 1 箇所で
180 万回）、generic な比較だけにするとホットなクラスまで C 呼び出しになる。

- アームのクラスはインラインキャッシュではなく PMC から選ぶ。キャッシュは
  最後に来たクラスを持つだけで、交互に来るサイトでは偶然に左右される。
- nil が観測されたサイトは、nil に生ビット比較のアームを与える。nil の
  シェアが小さくても、単相ガードのままなら現れるたびに side exit するため、
  2 番手のシェアによらず dispatch する。nil 以外の少数派は 1/8 のシェア規則に
  従う（そのアームは generic な呼び出しなので、相応の頻度が要る）。
- `Integer` のアームガードは fixnum タグを見るので、`Bignum` は deopt せず
  残余アームに落ちる。`IntegerOverflow` の exit が印を付けたサイト
  （`compile_binary` 2a）は、PMC によらず Integer のアームを持つ。

単相の BinCmp サイトは、ガード失敗が温まると
`RecompileReason::BecamePolymorphic` で再コンパイルし、上の dispatch に移る
（`RecvMissMode::PartB`）。

多相な `[]` サイトは、インライン化するクラスのアームと
`invoke_method(:[])` の残余からなる 2 アームの dispatch になる
（`compile/index.rs`）。

## 7. PMC の記録と profile ダンプ

- **格納**: `CallSiteInfo::pmc: PolyCache`（`globals/store.rs`）。最大
  `PMC_WAYS` = 4 エントリの `PolyCacheEntry { recv, arg, fid, count }` と、
  5 個目以降の観測を数える overflow カウンタ（megamorphic）。キーは
  **MethodCall / UnOp = レシーバのみ、BinOp / Cmp / Index / StoreIndex =
  レシーバ + 第一引数**。
- **記録点（すべて slow path、定常状態のコストはゼロ）**:
  - MethodCall: `runtime::find_method`（単一エントリキャッシュのミス時）で
    `class_for_ic` と解決済み FuncId を記録する。`cacheable = false`
    （frame-dependent な super）は記録しない。
  - BinOp / Cmp / Index / StoreIndex: `vm_save_binary_class` /
    `a64_save_binary_class` の「初回 population」と「多相への遷移」の 2 分岐
    から `runtime::pmc_record_binary(vm, globals, pc, old_lhs, old_rhs)` を
    呼ぶ。置き換えられた旧ペアも一緒に記録する。fixnum fast path は IC に
    記録せずスタンプするので、置き換えの瞬間がそのペアを PMC に残す唯一の
    機会である。一度も置き換えられなかったスタンプは PMC に載らないが IC には
    残るので、消費側は **IC の現内容と PMC の和集合**を観測集合として扱う。
  - UnOp: `vm_save_lhs_class` / `a64_save_lhs_class` から
    `pmc_record_unary`。
- **count** は slow path での観測数であって呼び出し総数ではない。
- **ダンプ**: `--features profile` の終了時統計（`Store::show_stats`）の
  「polymorphic method cache」節。サイト数（recorded / polymorphic /
  megamorphic）と、観測数順の多相サイト上位 40 件を
  `class(/arg)(=resolved-func) xN` の形で表示する。

PMC のエントリは class-version ガードの傘の下にある（再定義でサイトを
含む本体が無効化される）ので、エントリ個別の無効化は要らない。

## 9. 集合ガードの背後のインライン生成器

`InlineGen`（`globals.rs`）は受け手と引数のクラスを `Option<CachedClass>`
で受け取る。生成器を発火させること自体が「実行時に受け手がどのクラスでも、
このサイトの名前はこの生成器のメソッドに解決する」という呼び出し側の保証で
あり、集合ガードと PIC のアームはこれを満たす。`Some(class)` はクラスが
静的にも確定していることを加え、`None` は確定していないことを示す。生成する
コードがクラスに依存する生成器は `None` で辞退し（`false` を返す）、値の
表現だけを読む生成器（`nil?`、`frozen?`（`emit_frozen_pred`）、`__id__`、
`object_id`）は発火し続ける。登録経路は 1 つで、クラス非依存版を別に
登録する必要はない。

`is_a?` / `instance_of?` / `respond_to?` 系は静的なクラスで畳み込むので、
多相サイトでは辞退してビルトイン呼び出しになる。

## 10. generic send の残余

PIC の最終アームで学習の余地がない（PMC 満杯・overflow）とき、および PMC が
既に多相を観測しているサイトの単相 / 集合ガードのミスは、deopt ではなく
**generic send** の残余に落ちる。多相サイトが永久に deopt し続けることはない。

- **形**: 残余は `dispatch.rs` の宣言型マージの 1 アーム
  （`RecvMissMode::Residual`、`pic.rs` の最終アーム /
  `method_call_with_residual`）。ガードのミス先が残余ラベルになる。
- **探索**: `GuardSendResolvable` がレシーバクラスを 1-way キャッシュ
  （`runtime::GenericSendCache`: class, class version, fid）と照合し、ミスなら
  `jit_generic_send_check` で探索して書き戻す。解決できない（未定義・private・
  `method_missing` 経由）か、呼び先が `binding` / `eval` 系の効果を持つときは
  0 を返し、そのときだけ deopt する。
- **呼び出し**: `GenericSend` はキャッシュの fid から FuncData を取り、
  wrapper 経由で呼ぶ（self 特化本体へは wrapper が dispatch する）。splat・kw・
  ブロックのない 8 引数以下の呼び出しは、呼び先が単純なシグネチャ
  （`META_KIND` の simple bit と `FUNCDATA_MIN == pos_num`）ならインラインで
  コピーし、それ以外は `vm_handle_arguments` で束縛する。
- **間接的な binding**: 呼び先が `send` や `Method#call` のように名前で
  呼び戻す builtin だと、admission の判定をすり抜けて `Kernel#binding` に
  届き、JIT フレームがヒープへ昇格する。残余アームでは `no_capture_guard` を
  落とし、send 後の capture ガードで deopt してヒープ側のローカルを読む。
- **`...` 転送サイト**: 実行時の束縛は VM と同じ `vm_handle_arguments` なので、
  転送された rest が実 `Array` でも遅延転送マーカーでもそのまま束縛できる。
  JIT 側では `settle_operands_for_runtime_binder` が elided のままの rest /
  `**kwrest` を実体化し、K1 の kw スキップに拒否権を出す
  （`arg_forwarding_jit.md`）。
- **メソッド本体に直接書かれた `super`** も対象になる
  （`generic_send_eligible`）。ブロック付きの呼び出しは対象外である。

ミスの扱いは `RecvMissMode` が決める。PMC がまだ単相のサイトのミスは
再コンパイルを伴う exit（`Learn`）で、多相への切り替えの機会を与える。多相の
ゲートが既に辞退したサイトでは、再コンパイルしても同じ本体になるので通常の
deopt にとどめる。

テスト: `pic.rs` の `pic_*`、`pic_overflow_generic_residual`（アリティ差・kw・
splat+ブロック引数・`method_missing`・private・`alias tag binding`）、
`pic_residual_indirect_binding`、`mono_guard_generic_residual`。
