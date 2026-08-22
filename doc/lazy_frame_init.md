# Lazy frame-slot initialization

JIT メソッドプロローグは従来、引数以外の全スロット(locals + temps)を毎呼び出し
nil で埋めていた(`reg_num − arg_num` ストア/call)。抽象インタプリタは各スロットの
初期状態を既に知っている — locals は `C(nil)`、temps は `V` — ので、この fill は
原理的に不要である。現在プロローグはこれらのスロットに**何も書かない**。代わりに
「フレームを観測しうる全ポイントでの実体化」が担保する。

## 不変条件

**コンパイル単位の外へ制御が出る瞬間、フレームの全スロットの stack home は
有効な `Value` を保持している。**

GC はセーフポイントでしか走らず(アロケーションは poll word を arm するだけ、
`alloc.rs`)、セーフポイントに到達するには C 呼び出しが必要である。よって
メソッドチェーン A→B→C で C が poll したとき、suspended な A・B のフレームは
どれも「単位外へ出る瞬間」を必ず通過済みであり、スキャン可能である。

## 実体化ポイント

1. **`get_using_fpr` チョークポイント**(`state/slot.rs`)。既存設計により
   *全ての C-ABI 呼び出し*(send / yield / inline `__send__` / method_missing
   ディスパッチ / fiber 切替 / Ruby 再入ヘルパ)はここを通る。
   `take_invalid_homes()` が未実体化スロットを収集し
   `AsmInst::MaterializeHomes` を emit する:
   - `C(v)` → 定数そのもの(モードは C のまま — 定数畳み込みは保持)
   - `V` / `F(_)` → nil(`F` の真値は fpr にあり、home はコンパイル済みコードから
     読まれない。f64 はヒープ参照を持たないので nil が正しい GC 表現。boxing は
     行わない — 割り付けを伴うため)
   - `S` / `Sf` / `None` / `MaybeNone` → ストア不要(home は既に有効、または
     スキャナがスキップする 0)
2. **明示 poll**(entry / LoopStart の `exec_gc`)は従来通り cold path で
   literal + void を write back する(現行フレームの担保)。
3. **deopt / recompile-deopt / error 側出口**は `get_write_back()` を使い、これは
   `void`(V スロットへの nil)を**含む**。VM に落ちたフレームはその後 VM の
   コールサイトで suspend し全スロットがスキャンされるため。これらの出口の
   時点で live callee は存在しない(ガードは呼び出しの合間に発火し、error 出口は
   callee の return 後に走る)ので nil 書きはエイリアスフリー。
4. **`ChainExitSpec`(chain-deopt の suspended-frame replay)**は逆に
   `get_chain_write_back()` = void **抜き**を使う。replay は callee が live な
   まま走り、caller の above-sp 領域は callee フレーム(制御ワード・cont frame・
   FprSave 領域・staged 引数)と物理的にエイリアスするため、無条件の nil 書きは
   callee を破壊する。suspended フレームの V temps はチョークポイントが呼び出し
   前に実体化済みなので、replay 側での担保は不要。

## validity 追跡

`SlotState::valid_home: Vec<bool>`。物理的 validity はフレーム内で単調
(一度書かれたスロットは stale でも Value のまま)なので false→true のみ、
join は積(両パスで確立された場合のみ生存)。追跡は保守的で、通常のストア
emission は bit を更新しない — 次のチョークポイントで冗長な 1 ストアが出るだけ。
初期値: self と全引数スロット(caller が書く。未渡し optional は scanner-safe な
0)= true、destruct スロット = true(下記)、locals / temps = false。
loop-JIT フレームは VM プロローグ経由(eager nil 済み)なので全 true。

既知のコスト特性: ループ頭の join は積なので、ループ本体内のコールサイトは
「ループ entry 時点で invalid だったスロット」への nil をイテレーション毎に
再ストアする(静的に 1 回 emit、実行はループ毎)。ループ突入前ブロックへの
hoist は将来の改良。

## プロローグに残るもの

- **destruct スロット**(`|(a, b)|`): 引数領域内にあり、caller は書かず、
  抽象状態は `S`(= どの write-back too 対象外)。本物の nil fill を恒久維持。
- **`nil_block_arg`**: `(...)` forwarding 形のブロックパラメータスロット。
  `FnInitInfo.arg_num`(args_names 長)は block を**含まず**、フレームの
  総引数数(`total_args()`)は**含む**。lazy-marker 経路等の caller は
  このスロットを書かず、`S` 扱いなのでどの write-back too 対象外 — fill 範囲に
  落ちる場合のみ本物の nil を 1 ストア(旧プロローグの被覆の正確な再現)。

## 検証で見つかった穴(すべて poison が特定)

1. `(...)` の隠れブロックスロット(`Class#new` %3)— `nil_block_arg` で修復。
2. suspended フレームの V temps — チョークポイント実体化で修復
   (GC 時 walk + chain テーブルによる fixup は、単位間連鎖で VM リターンに
   遮られ届かないため**廃案**。この教訓が「呼び出し前 eager 実体化」への
   転換点)。
3. deopt→VM 後の V temps(method_missing / `__send__` inline / specialized
   decline)— deopt write-back への void 追加で修復。

## `frame-poison` — 被覆の検証器

被覆は「実行時チェック」ではなく上記の議論で担保されるが、書き戻し機構を
変更したときに再検証できるよう、検証器を feature として残してある。

`--features frame-poison` でプロローグの fill が復活する。ただし nil ではなく
`POISON_VALUE`(`0xDEAD_DEAD_DEAD_DE04`、other-immediate なので何も
dereference しない)を書き、`Lfp::mark_contents` が明示比較で検出して
FuncId・スロット・フレーム全体を添えて abort する。被覆の穴は「どこかの GC が
スタックごみをマークした」ではなく「自ら名乗る located failure」になる。

`gc-stress`(全セーフポイントで収集)と併用するのが本来の使い方で、CI の
gc-stress workflow はこの組み合わせで回る。

なお poison は GC 走査だけでなく**値としての読み出し**も検証している:
未実体化スロットを nil のつもりで読むコードがあれば POISON という異常値を
受け取って可視的に壊れるが、全テストが通ったのでそのような読み手は存在しない。

## 効果(x86-64、back-to-back 測定)

dead local を持つメソッドの call コスト:

| dead locals | fill あり | fill なし(現在) |
|---|---|---|
| 1  | 6.9 ns | 6.3 ns |
| 8  | 7.5 ns | 5.8 ns |
| 16 | 9.6 ns | 6.3 ns |
| 32 | 14.4 ns | 5.9 ns |
| 64 | 24.2 ns | 6.3 ns |

スロット数比例項(~0.28ns/slot)が消え、**call コストはフレーム幅に依存
しなくなる**。optcarrot は checksum 一致・fps 変化なし(fill はそこの
ボトルネックではない)。
