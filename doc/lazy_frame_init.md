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
   - `V` → nil を書き、**mode を `C(nil)` に変更**。nil が本当にスロットに入った
     以上それは `C(nil)` であり、`V` は「対応するスタックスロットが**未初期化**
     である」ことだけを表す正準マーカーになる。`V` のまま残すと以降の全
     write-back の `void` リストが同じスロットを再び nil 埋めし続ける — ガードが
     密でガードごとに側方出口を持つ `--opt` のようなコードでは、これが無視でき
     ない量の重複ストアになる(実測 ~0.9%)。
   - `F(_)` → nil(`F` の真値は fpr にあり、home はコンパイル済みコードから
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

   ただし `void` に載せるのは **`valid_home` が false の `V` スロットだけ**で
   ある。`V` は「一度も書いていない」ではない — `clear()` はスロットが再定義・
   discard されるたびに `V` に戻すし、sp は実行とともに上下するので、`V` 集合は
   現在の sp より上に限られもしなければ、sp より上を覆いもしない。nil ストアが
   要るかどうかを決めるのは物理的な述語 `valid_home` のほうで、ユニット内で
   既に書かれた(あるいはコールサイトで実体化された)`V` スロットの home には
   stale だが有効な `Value` が入っている。GC スキャナが求めるのはそれだけであり、
   VM から見ても同じ — JIT が discard したスロットはバイトコード側の liveness
   でも dead で、素の VM でも stale な値がそのまま残る。実測(`optcarrot --opt`
   1 回)で `V` 総数 281,861 に対し実際にストアが要るのは 5,504(**98% 減**)。

   #### 捨てたもの: dead スロットのゴミ清掃

   フィルタ前の `void` リストは、副作用として dead な `V` スロットを nil で
   潰していた。オブジェクト参照を握ったまま死んだ temp が解放されるので、
   浮遊ゴミが減る効果がある。フィルタはこれを手放す。

   代わりに `get_gc_write_back()`(entry / LoopStart の明示 poll)だけ
   フィルタ無しに戻す案を実装して計測したが、**採用しなかった**:

   - `optcarrot`(既定モード)で GC 回数は 3 者とも 16 回で同一、peak RSS も
     85.0–85.3 MB で差なし。清掃を復元しても減るものが観測できない。
   - 構造的な理由がある。`get_gc_write_back()` が清掃できるのは **poll を
     実行しているフレーム自身**だけで、チェーン上流の suspended フレームには
     届かない。保持が問題になるのは主に後者であり、そこを清掃するには
     `take_invalid_homes()`(コールサイトのチョークポイント)で dead スロットも
     潰す必要があるが、それはフレーム幅に比例したストアを呼び出しごとに
     復活させることであり、この最適化が消した当のコスト(64 dead locals で
     24.2ns → 6.2ns)そのものである。

   つまり清掃の利得は原理的には実在するが、この構造では安く買えない。
4. **`ChainExitSpec`(chain-deopt の suspended-frame replay)**は逆に
   `get_chain_write_back()` = void **抜き**を使う。replay は callee が live な
   まま走り、caller の above-sp 領域は callee フレーム(制御ワード・cont frame・
   FprSave 領域・staged 引数)と物理的にエイリアスするため、無条件の nil 書きは
   callee を破壊する。suspended フレームの V temps はチョークポイントが呼び出し
   前に実体化済みなので、replay 側での担保は不要。

## validity 追跡

`SlotState::valid_home: Vec<bool>`。物理的 validity はフレーム内で単調
(一度書かれたスロットは stale でも Value のまま)なので false→true のみ。
初期値: self と全引数スロット(caller が書く。未渡し optional は scanner-safe な
0)= true、destruct スロット = true(下記)、locals / temps = false。
loop-JIT フレームは VM プロローグ経由(eager nil 済み)なので全 true。
追跡は保守的で、通常のストア emission は bit を更新しない — 次のチョーク
ポイントで冗長な 1 ストアが出るだけ。

### join は積ではなく和(楽観的)+ エッジ補正

保守的な読みは積(両パスで確立された場合のみ生存)だが、それだと**ループ頭が
本体の成果を忘れる**: entry エッジは未実体化のまま到達するので merge は
「invalid」となり、ループ**本体内**のコールサイトが毎イテレーション再ストア
する。これは置き換えたはずの eager fill より確実に遅い(実測: 32 dead locals +
8 回転の内側ループを持つメソッドが 57ns → 122ns)。

そこで join は**和**を取り、義務をエッジ側へ移す。`gen_bridge` が各流入エッジで
`bridge_invalid_homes` を呼び、その前任者に不足している分だけを実体化する
(値は **target 側の mode** から: `C(v)` は定数、`V`/`F` は nil、`S`/`Sf` は
bridge 自身が home に書くので不要)。back edge を含む全エッジが `gen_bridge` を
通るため健全。

さらにループ頭では `set_all_valid_home()` で**全スロットを valid と宣言**する。
ループ本体は 1 回コンパイルされて何度も走るので、本体内に実体化が落ちること
自体を禁じる形にした。結果、entry エッジで 1 回だけストアされ、back edge は
何も負わない。ループ頭の merge が back edge の成果を「証明」するには本体の
コンパイル完了を待つ必要があり、それは順序的に不可能なので、この宣言が要る。

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
しなくなる**。

内側ループを持つ形状(メソッド + 8 回転ループ + ループ内 call)では、上記の
エッジ補正 + ループ頭宣言により master と同等に収まる(32 dead locals で
master 57ns / 本実装 62ns、0 dead locals では 59ns / 47ns と逆に速い)。

実ベンチはいずれも同等〜わずかに改善:

| bench | master | lazy init |
|---|---|---|
| app_fib | 0.23 s | 0.22 s |
| so_nbody | 0.38 / 0.34 s | 0.33 / 0.33 s |
| binarytrees | 0.29 s | 0.30 s |
| quick_sort | 0.85 / 0.77 s | 0.84 / 0.86 s |
| app_aobench | 7.27 / 7.16 s | 7.16 / 7.23 s |
| optcarrot | 157.2 / 155.9 / 147.7 fps | 159.3 / 158.9 / 159.2 fps |

optcarrot は checksum 一致(59662)。

### `--opt`: 退行は再現せず(2026-08-22 再計測)

以前ここには「`optcarrot --opt` で master 比 -1.6% の退行が残る」と記録して
いたが、**現在のホストでは再現しない**。当時の計測は 547–643 fps という遅い
レジームで取ったもので、その後ホストは 750–880 fps 帯に移っている。同一の
最大値基準で取り直した結果:

| モード | master(最大) | lazy init(最大) | 差 |
|---|---|---|---|
| `--opt`(24 反復) | 866.2 fps | 880.5 fps | **+1.65%** |
| 既定(8 反復) | 207.0 fps | 209.3 fps | **+1.10%** |

`wb_void` の `valid_home` フィルタを入れた後、16 反復で取り直した最大値:

| | master | フィルタ前 | フィルタ後 |
|---|---|---|---|
| `--opt` | 857.2 fps | 870.8 fps | **876.8 fps** |
| 既定 | 209.9 fps | 212.0 fps | 207.8 fps |

`--opt` はフィルタ後が最良(master 比 +2.3%、フィルタ前比 +0.7%)。既定モードは
バッチ間で順位が入れ替わり、ホストのノイズ帯(±3% 程度)に収まっている。

(比較対象は master `102f9d8d`。ホストのノイズは干渉による**減速**のみなので、
最大値が真値に最も近い。)

### 側方出口の void ストアは範囲埋めに向かない

退行対策として検討していた「per-slot ストアを `next_sp .. reg_num` の範囲埋めに
置き換える」案は、`get_write_back()` の 4 つの利用箇所を計測した結果**採らない**。
`optcarrot --opt` 1 回の実測(累計):

| 側方出口 | 出口数 | void スロット数 | 平均 | `next_sp` 未満 | 連続領域 |
|---|---|---|---|---|---|
| `new_deopt_with_pc` | 93,394 | 263,620 | 2.86 | 10,070 (3.8%) | 92,175 / 92,343 (99.8%) |
| `new_error_with_pc` | 5,582 | 15,068 | 2.93 | 2,127 (14%) | 4,787 / 5,137 (93%) |
| `new_recompile_deopt` | 1,018 | 2,317 | 2.33 | 72 (3.1%) | 990 / 995 (99.5%) |
| `deopt_from_point` | 6 | 12 | 2.0 | — | — |

分かったのは 2 点:

1. **`next_sp` を下限にはできない。** `V` スロットの 3.8–14% は `next_sp` より
   下にある。`next_sp .. reg_num` の範囲埋めはそれらを取りこぼし、
   scannable な home に POISON を残す。そもそも `V` は `clear()` が付ける状態で
   あって「未初期化」ではなく、sp は実行とともに上下するので、`V` 集合と
   sp の間にはどちら向きの包含関係もない(この観察が上の `valid_home` フィルタ
   につながった)。
2. **そもそも埋める量が少ない。** 1 出口あたり平均 2.9 スロット。void 集合は
   ほぼ常に連続領域(99.8%)なので `[lo, hi]` の範囲埋め自体は書けるが、
   n≈3 では per-slot ストア(nil は小さい即値なので 1 命令 8 バイト)を
   並べたほうが、ループ/セットアップを伴う範囲埋めより短い。

退行が再現しない以上、閉じるべき差もない。この節は再調査の出発点として残す。
