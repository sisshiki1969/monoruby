# 世代別ガベージコレクタ（Generational GC）導入検討

monoruby の現行 GC を世代別化するための設計検討メモ。実装方針・データ構造・
書き込みバリア・段階的導入計画・リスクをまとめる。CRuby の **RGenGC**
（非移動・世代別 mark & sweep）を参照モデルとする。

---

## 1. 背景と目的

monoruby の GC は **シングルスレッド・stop-the-world のマーク&スイープ**で、
毎回ヒープ全体をマークする「フル GC」しか持たない。弱い世代別仮説
（ほとんどのオブジェクトは若くして死ぬ）に従えば、生存し続ける老齢オブジェクト
を毎回マークし直すのは無駄が大きい。

世代別 GC の狙いは **マイナー GC でのマーク対象を「若い世代＋老齢→若い参照」に
限定**し、ライブヒープが大きいワークロード（長命オブジェクトを多数抱えたまま
短命オブジェクトを大量に生成するアプリ、Rails 系・optcarrot 等）での
GC 一時停止時間とマークコストを削減することにある。

monoruby の GC は **非移動（non-moving）**であり、これは RGenGC 系の設計と
相性が良い（コピー/コンパクションを伴わず、既存のページ・フリーリスト・
スイープ機構をほぼそのまま再利用できる）。

---

## 2. 現状アーキテクチャの要点（実装ベース）

`monoruby/src/alloc.rs`, `monoruby/src/value/rvalue.rs`, `executor.rs` を調査した結果。

### 2.1 アロケータ / ページ
- `Allocator<T>`（`alloc.rs:60`）。256KB ページ（`ALLOC_SIZE = PAGE_LEN * GCBOX_SIZE`）。
- `Page<T>`（`alloc.rs:602`）は `data: [T; DATA_LEN]`（4032 セル）＋
  `mark_bits: [u64; SIZE-1]`（63 ワード = ページ単位のマークビットマップ）。
- 割り当て：フリーリスト優先 → 現在ページのバンプ割り当て → ページ枯渇で新規/再利用ページ。
- `THRESHOLD` 到達で **alloc_flag** を立て、JIT が次のセーフポイントで GC を起動。

### 2.2 マーク
- トレイト `GC<T>`（`alloc.rs:39`）の `mark(&self, alloc)`。ルートは `Root`
  （`executor.rs:2648`）で、`YIELDER` / `Globals::mark`（`globals.rs:158`）/
  `Executor::mark`（`executor.rs:157`：`temp_stack`・`cfp` 連鎖の各 `lfp`・
  `lexical_class`・`exception`）を辿る。
- `RValue::mark`（`rvalue.rs:724`）が ivar テーブル・インライン ivar・
  各種コンテナ（Array/Hash/Range…）を再帰マーク。
- `gc_check_and_mark`（`alloc.rs:413`）が **ページビットマップ**へ test-and-set。
  **マークビットはヘッダに無く、ページビットマップにのみ存在**。

### 2.3 スイープ / フリーリスト
- `gc()`（`alloc.rs:361`）：`clear_mark` → `root.mark` → `salvage_empty_pages`
  → `sweep`。
- `sweep`（`alloc.rs:467`）は `trailing_ones()` による高速ビット走査で未マーク
  セルをフリーリストへ連結。フリーリスト連結はヘッダ union の `next` を使用。
- マーク後に malloc 量から `malloc_threshold` を更新（`alloc.rs:403`）。

### 2.4 RValue ヘッダ（重要）
`rvalue.rs:2030` 付近。`Header` は `union { meta: Metadata, next: ... }`。
```
struct Metadata { flag: u16, ty: Option<ObjTy>, _padding: u8, class: Option<ClassId> }
```
`flag` の使用済みビット：
- bit0 `0b001` = live、bit1 `0b010` = frozen、bit2 `0b100` = chilled。
- **bit3〜bit15（13bit）と `_padding: u8` は未使用** → 世代別メタデータの格納先に使える。

### 2.5 書き込みバリア（現状なし）
- `write_barrier` 相当は皆無。シングルスレッド・セーフポイント GC のため不要だった。
- フィールド変更はすべて直接書き込み：
  - Rust 側 ivar：`set_ivar_by_ivarid`（`rvalue.rs:943`）等。
  - **JIT 側 ivar / Struct スロット：生の `movq [rdi + offset], src` を直接発行**
    （`codegen/jitgen/asmir/compile/variables.rs:96, 148, 167`）。Rust 関数を
    経由しないため、ここに **バリアを差し込まない限り世代別 GC は不健全**になる。
  - Array/Hash 要素ストアも Rust 側で直接。

### 2.6 トリガ / フラグ
- `gc_enabled`（`--no-gc`）、`gc-stress`（毎割り当てで GC）、`gc-debug` / `gc-log`。
- 既に `heap_frames`（スタックから昇格した `Box<[u64]>` フレーム）に対し
  **2 サイクルの猶予**を持つ簡易な「世代的」延命機構がある（`alloc.rs:137, 215`）。

---

## 3. 方式選定：非移動・世代別 mark & sweep（RGenGC 型）

| 候補 | 概要 | monoruby 適合性 |
|------|------|----------------|
| **RGenGC（採用）** | 非移動。old/young の 2 世代＋remembered set＋WB | 既存のページ/スイープ/フリーリストをほぼ流用可。非移動で安全 |
| コピー GC（Cheney 等） | young をセミスペースでコピー | **不可**：オブジェクトが移動 → ポインタが固定であることを前提とする JIT・C 拡張・`Value` 表現を破壊 |
| Sticky mark-bit のみ | バリアなしの簡易世代別 | **不健全**：old→young 参照を追えず dangling を生む |

→ **RGenGC 型**（2 世代、非移動、マイナー/メジャー GC、書き込みバリア＋
remembered set、WB-unprotected オブジェクトの混在許容）を採用する。

---

## 4. データ構造の変更

### 4.1 ヘッダのフラグビット拡張（`rvalue.rs` Header）
`flag: u16` の空きビットに高速判定用フラグを追加（書き込みバリアは毎ストアで
走るため、ページ索引計算なしに RValue から即読めることが重要）：

| ビット | 名前 | 意味 |
|--------|------|------|
| bit3 `0x08` | `OLD` | 老齢（昇格済み）。マイナー GC で回収・再走査しない |
| bit4 `0x10` | `WB_UNPROTECTED` | 書き込みバリア未保護（shady）。昇格させず毎マイナーで走査 |
| bit5 `0x20` | `REMEMBERED` | remembered set に登録済み（重複登録防止用） |

昇格ポリシーで年齢カウンタを使う場合は `_padding: u8` を `age: u8` に転用する。

`Header::new`（`rvalue.rs:2047`）は `flag = 1` で生成するので **新規オブジェクト
は常に young**（OLD/REMEMBERED が落ちている）。フリーリスト上は union の `next`
が flag 領域を上書きするが、解放済みオブジェクトに世代情報は不要なので問題なし。

### 4.2 ページに `old_bits` ビットマップを追加（`alloc.rs` Page）
スイープを高速ビット走査のまま保つため、`mark_bits` と並列の `old_bits`
を追加：
```
struct Page<T> {
    data: [T; DATA_LEN],
    mark_bits: [u64; SIZE - 1],
    old_bits:  [u64; SIZE - 1],   // 追加
}
```
- 役割：マイナー GC のマーク前に `mark_bits = old_bits` で**老齢を「既マーク」と
  みなしてシード**する。これにより
  - 既存 `gc_check_and_mark` が old を即 true 返し（**子を再走査しない**）、
  - 既存 `sweep` が old を生存とみなし（**回収しない**）、
  既存コードをほぼ変えずに「老齢を飛ばす」挙動が得られる。
- ヘッダの `OLD` ビットと `old_bits` は昇格時に**両方**セットして同期させる
  （ヘッダ＝バリアの高速判定、ビットマップ＝スイープ/シードの高速走査）。

### 4.3 remembered set
`Allocator` に `remembered: Vec<NonNull<RValue>>` を追加。
- 書き込みバリアで「old なオブジェクトが young を指した」ものを登録。
- マイナー GC 開始時、各要素のフィールドを走査して young の子をマークする
  （要素自身は old のまま）。

---

## 5. アルゴリズム

### 5.1 マイナー GC（minor）
1. `mark_bits := old_bits`（各ページ。`clear_mark` を「ゼロ詰め」から
   「old_bits コピー」に変更）。`WB_UNPROTECTED` な old は `old_bits` に含めず、
   通常通り走査対象に残す（後述）。
2. **remembered set を走査**：各 old オブジェクトのフィールドをマーク
   （young の子に到達 → マーク）。
3. ルート（`Root`）をマーク。old に到達した時点で `gc_check_and_mark` が
   true を返し子を辿らない（老齢サブグラフを丸ごとスキップ）。
4. `sweep`：未マーク（= old でも到達でもない young の死体）を回収。
5. **昇格**：このマイナーで生存（マーク）した young を old に昇格
   （`OLD` ビット＋ `old_bits` セット）。WB_UNPROTECTED は昇格させない。
6. remembered set のクリア方針：メジャー GC まで保持（保守的・安全）。

### 5.2 メジャー GC（major / full）
従来のフル GC と等価：
1. `mark_bits` と `old_bits` を**両方ゼロ**（old を含め全回収候補に）。
2. ルートから全グラフをマーク（old も普通に辿る）。
3. `sweep` で全世代を回収。
4. `remembered` をクリア（`REMEMBERED` ビットも落とす）。
5. 生存オブジェクトの世代を再構築（昇格ポリシーに従い old を再付与）。

### 5.3 マイナー/メジャーの選択
- 通常 alloc_flag 起因の GC は **マイナー**。
- 以下でメジャーへ昇格：
  - malloc 量が `malloc_threshold` 超過（外部メモリは世代別管理外のため）。
  - old 世代サイズが前回メジャー後比で一定割合増（old がフラグメント蓄積した時）。
  - remembered set が肥大化（マイナーの利得が薄れた時）。
  - 明示要求（`GC.start`）。

---

## 6. 書き込みバリア（最重要・最大の工数）

世代別 GC の健全性は **「old → young 参照を必ず remember する」**ことに依存する。
取りこぼし＝ use-after-free（dangling）になるため、網羅性が決定的に重要。

### 6.1 バリア本体（Rust）
```rust
#[inline]
fn write_barrier(parent: &mut RValue, child: Value) {
    if parent.is_old() {
        if let Some(rv) = child.try_rvalue() {
            if !rv.is_old() && !parent.is_remembered() {
                parent.set_remembered();
                ALLOC.with(|a| a.borrow_mut().remember(parent));
            }
        }
    }
}
```
これを **すべての参照フィールド変更点**に挿入する：
- ivar：`set_ivar_by_ivarid` / `set_ivar`（`rvalue.rs:943` 付近）。
- Array：`push` / `insert` / `[]=` / `concat` / `fill` 等の要素書き込み。
- Hash：キー/値の挿入・更新。
- Struct スロット、Range/Complex/Rational 等の可変フィールド（あれば）。
- グローバル変数・定数テーブルは基本ルート側なので old 化対象外（要確認）。

### 6.2 JIT インラインストアの扱い（核心的課題）
JIT は ivar / Struct スロットへ **生の `movq [rdi+offset], src`** を直接発行する
（`asmir/compile/variables.rs:96,148,167`）。ここは Rust バリアを通らない。
対策の選択肢：

- **(A) JIT にバリアを発行する**：ストア後に
  `parent.is_old()` を 1 命令でテスト（ヘッダの flag を読む `testw`）し、
  old なら slow path（remember 関数呼び出し）へ。young が圧倒的多数なので
  分岐予測でほぼゼロコスト。**最終的にはこれが本筋**。

  > **重要（Phase 4 調査）**：JIT の **ヒープ ivar ストア**
  > （`asmir/compile/variables.rs` `store_ivar_heap_inner`）には、スロットが
  > 既に存在する場合の **高速パス（生 `movq [rdx+idx*8], src`、バリアなし）**
  > がある。`var_table` が `None`／容量不足のときだけ slow path で
  > `set_ivar`（= バリア保護の Rust 経路）を呼ぶ。つまり**ある ivar スロットの
  > 初回作成（None→Some / resize）は必ずバリアを通る**が、既存スロットへの
  > 上書きはバリアを通らない。
  > OBJECT 型の **inline ivar** ストア（`store_ivar_inline`）は常に生 `movq` で
  > バリアを通らない。
  > → これらに old 判定＋remember を発行するのが Phase 6。それまでは
  > ivar を持ち得る型を old に昇格させない（後述 Phase 4 の昇格ポリシー）。
- **(B) 暫定：当該型を WB_UNPROTECTED 扱い**：JIT インラインストアの対象になり
  得る型（OBJECT＝ivar 持ち、STRUCT、ARRAY 等）を当面 **WB-unprotected** とし、
  - 昇格させない（常に young 扱い）、
  - 毎マイナーで必ず走査（= remembered set に常駐 or ルート同様に走査）、
  ことで **バリア取りこぼしでも不健全化しない**ことを保証する。
  generational の利得は当該型で得られないが、安全に段階導入できる。

→ **(B) で正しさを担保したまま導入 → (A) を型ごとに実装して WB-protected へ
昇格**、という順序を推奨。CRuby も WB-protected/unprotected の混在で漸進導入した。

### 6.2.1 Rust 側 mutation site マップ（調査結果）

書き込みバリアを要する「heap オブジェクトの参照フィールドへの Value ストア」箇所
の全体像（Phase 3 で調査・確定）：

| 種別 | 直接の mutator | 親 RValue の取得 | バリア配線フェーズ |
|------|----------------|------------------|--------------------|
| **ivar** | `RValue::set_ivar_by_ivarid`（`rvalue.rs`）| `self` が RValue（clean な choke point）| **Phase 3 済** |
| **Array 要素** | `ArrayInner::{push,insert,insert_many,fill,resize,extend,extend_from_slice,set_index,set_index2,replace}`（`rvalue/array.rs`）| 内部型に後方ポインタ無し。`Array(Value)` ラッパ／builtin 呼び出し側に Value あり | Phase 5（Rust）＋ Phase 6（JIT）|
| **Hash 要素** | `HashmapInner::{insert,set_defalut_value,set_defalut_proc}`（`rvalue/hash.rs`）| 同上（`Hashmap(Value)`）| Phase 5 |
| **Struct スロット** | `StructInner::set`（`rvalue/struct_inner.rs`）| 呼び出し側に `self_val: Value` | Phase 5（Rust）＋ Phase 6（JIT）|
| Range / Rational / Proc / Binding / MatchData / Exception / ArithmeticSequence | — | 構築時 write-once（生成直後は young）| **不要** |

重要：Array/Hash/Struct の mutator は後方ポインタを持たない**内部型**（`ArrayInner`
等）上にあり、`Array`/`Hashmap` ラッパは `DerefMut` で内部型へ委譲する。したがって
Rust レベルに単一の choke point は無く、バリアは**呼び出し側（builtin）に
`Value::write_barrier(child)` を挿入**する形になる（Phase 5 の型ごと作業）。JIT が
直接発行する inline ストア（`asmir/compile/{variables,index}.rs`）は Phase 6。

### 6.3 C 拡張・FFI
外部 C コードからのフィールド変更も WB を通らない。現状 monoruby の C 拡張
表面積は限定的だが、公開する書き込み API は WB-unprotected として扱うか、
専用の `rb_obj_write` 相当を経由させる方針を明記しておく。

---

## 7. 昇格（promotion）ポリシー

- **初期実装**：1 回のマイナー生存で即 old 化（実装が単純・remembered set も
  早く安定）。
- **チューニング版**：`age: u8`（`_padding` 転用）を導入し、`RGENGC_OLD_AGE`
  （例 2〜3）回生存で昇格。短命だがやや長生きするオブジェクトの早期昇格
  （= early promotion による old フラグメント増）を抑える。
- WB_UNPROTECTED は age に関わらず昇格禁止。

---

## 8. 段階的実装計画

各フェーズ末で `cargo test`／`bin/test`／`--features gc-stress` を緑にしてから次へ。

1. **計測基盤**：`gc-log` を拡張し、マーク数・スイープ数・GC 回数・(将来の)
   minor/major 別時間を出す。現行フル GC のベースライン取得。
2. **ヘッダ/ビットマップ整備**：`OLD`/`WB_UNPROTECTED`/`REMEMBERED` フラグ、
   `old_bits` ビットマップ、`is_old`/`set_old` 等のアクセサを追加（挙動は不変、
   ビットは未使用のまま）。
3. **remembered set ＋ Rust バリア**：`remember()` と `write_barrier()` を実装し、
   Rust 側の変更点に挿入。ただしこの段階では昇格は行わない（全 young）ので
   バリアは inert（`is_old()` が常に false）。
   - 実装済：remembered set（`Allocator::{remembered,remember}`、major GC で
     クリア）、`RValue::write_barrier` / `Value::write_barrier`、**ivar choke
     point**（`set_ivar_by_ivarid`）への配線。§6.2.1 のマップを確定。
   - Array/Hash/Struct は内部型に後方ポインタが無く呼び出し側が分散するため
     Phase 5 で型ごとに配線（JIT は Phase 6）。
   → バリアは inert なので gc-stress でフル GC 結果と差異が出ないことを確認。
4. **マイナー GC 機構の実装（昇格は空）**：minor/major の二経路を実装。
   - 実装済：`GcKind`＋`decide_gc_kind`（`MINORS_PER_MAJOR` ごとに major）、
     minor の `seed_marks`（`mark_bits = old_bits`）、major の `clear_mark`＋
     `clear_old`、`mark_remembered`（old の子を走査）、`filter_remembered`
     （sweep 前に死んだ entry を除去）、`RValue::mark` を `mark`＋`mark_children`
     に分割（GCBox に `mark_children` 追加）、ページ投入時の `old_bits` ゼロ化
     （arena は未初期化のため）、gc-log の minor/major カウンタ。
   - 検証：default 全テスト green（既知の Ruby4.0 inspect 差 2 件を除く）、
     gc-stress で array/hash/gc green、gc-debug でフリーリスト整合 assert 成立。

4b. **昇格の有効化（安全な最小集合）＋ gc-verify**：
   - 昇格条件 `RValue::is_promotable`：`var_table.is_none()` かつ
     `ty ∈ {STRING, BIGNUM, FLOAT}`（reference-free leaf）。昇格時に外向き
     Value 参照が皆無なので、minor で skip しても生存オブジェクトを取りこぼさ
     ない。後から ivar を得ても**初回ストアは必ずバリアを通る**（`var_table`
     None→Some / resize は JIT slow path = `set_ivar`）ため恒久的に remembered。
   - 昇格は `gc_check_and_mark` 内で「生存（新規マーク）した promotable」に
     `old_bits` を立てる（`self.promoting` が真のときのみ）。
   - **old-ness の単一の真実は `old_bits` ビットマップ**。書き込みバリアは
     `Allocator::is_old_obj`（bitmap）で判定する。ヘッダ `OLD` ビットは major
     降格後に stale になり得るため**使わない**（特に child 側を stale-old と
     誤判定すると remember 漏れ＝dangling になる）。`REMEMBERED` は生存
     オブジェクトでは stale しないのでヘッダビットで dedup。
   - **`gc-verify` フィーチャ**：minor GC の sweep 後に root から独立にフル
     再マーク。minor が到達可能オブジェクトを誤って解放していれば、その
     dangling 参照を辿った時点で `RValue::mark` の `is_live` assert が発火し
     バグを即検出（`self.promoting=false` で副作用なし）。
   - 検証：`gc-verify,gc-stress` で昇格ストレス（promoted string に young ivar
     を付与）が CRuby と一致（ivar ターゲット無傷）。`gc-verify` 下で
     string/array/hash/object/struct/gc green（取りこぼし 0）。gc-debug の
     フリーリスト整合 assert も昇格下で成立。gc-log に old-gen 数を追加し
     昇格の発生を確認（promoted ≫ 0）。
   - **既知のトレードオフ**：現状は「初回 minor 生存で即昇格」（aggressive）
     のため、major 間で短命 old（floating garbage）が溜まる。CRuby 同様に
     `age` カウンタ（survive N 回で昇格）を入れれば緩和できる（今後の調整）。
5. **コンテナの Rust バリア配線 ＋ バリア高速化**：
   - **バリア高速化（必須前提）**：旧バリアは young 親でも `old_bits` 参照のため
     ALLOC を borrow していた。これをホットな配列 push 等に入れると劣化するため、
     **ヘッダ `OLD` ビット参照に変更**（young 親は 1 命令で return、borrow なし）。
     子の世代は見ず「親が old なら remember」（over-approx・安全）＋即値スキップ。
     ヘッダ `OLD` はマーク中の `&self` エイリアスを避けるため**マーク後の
     `apply_promotions` で設定**（`newly_promoted` に収集 → 後でヘッダ書き込み）。
     major 降格後の stale-old は no-child-check により安全（最悪 over-remember）。
   - **Array / Hash の Rust バリア配線**：`Array`/`Hashmap` ラッパに barrier 付き
     inherent メソッド（push/insert/insert_many/fill/resize/extend/
     extend_from_slice/set_index/set_index2/replace、insert/set_defalut_*）を追加。
     deref より inherent が優先されるため既存呼び出しを自動的に横取り。要素を
     **格納**する操作のみ（pop/remove/clear 等は不要）。`write_barrier_bulk`
     （複数要素用、親が old なら remember）も追加。builtins の配列/Hash 変更は
     全てラッパ経由（`as_array_inner_mut` 直叩き 0 件）で網羅。
   - **コンテナ昇格は依然 無効**：`is_promotable` は leaf 限定のまま。**重要な
     発見**：`var_table.is_none()` は leaf（参照ゼロ）には十分だが、**コンテナは
     要素という外向き参照を常に持つ**。昇格前から young 要素を指していた場合、
     その old→young エッジはバリア成立前に生成済み＝remembered されず、minor で
     取りこぼす（gc-verify が検出）。安全なコンテナ昇格には **age ベース昇格
     （子が全て old になってから昇格）or remember-on-promote** が必要（次フェーズ）。
     配線済みバリアはその前提として機能する。
   - 検証：default 全テスト green（既知 inspect 2 件除く）、default/no-jit ともに
     gc-verify で array/hash/string/object/gc green、gc-verify,gc-stress で leaf
     昇格ストレス（promo_stress）が CRuby と一致。

6. **age ベース昇格（floating garbage 削減）**：
   - ヘッダの未使用 `_padding: u8` を **`age: u8`** に転用（`Header::new` で 0 →
     alloc/再利用時に自然リセット）。`RGENGC_OLD_AGE = 3`。
   - gc_check_and_mark は promotable survivor を `aging` Vec に収集するだけ
     （即昇格しない）。マーク後の **`apply_aging`** で age を +1 し、閾値到達で
     `old_bits`＋ヘッダ `OLD` を設定して昇格（遅延パスなのでマーク中の `&self`
     エイリアス無し）。既に old の object は minor で seeded-marked のため
     gc_check_and_mark を早期 return し aging に入らない。
   - 効果：promo_stress で old-gen が **13401 → 10641** に減少し、old-gen ≤ live
     の健全な比率に（以前は old > live で過剰昇格を示唆）。「初回生存で即昇格」で
     短命オブジェクトを誤って old に積む floating garbage を抑制。
   - 検証：default 全テスト green（既知 inspect 2 件除く）、gc-verify,gc-stress で
     promo_stress が CRuby と一致、gc-verify で string/array/gc green。

7. **remember-on-promote ＋ JIT ivar バリア → OBJECT を JIT 下で昇格**：
   - **remember-on-promote**：`GCBox::young_child_exists`（ivar＝var_table ＋
     型別の子〔OBJECT inline ivar / Array 要素〕を走査し young heap 参照の有無を
     判定。再追加した `Allocator::is_old`〔old_bits〕を使用）。`apply_aging` で
     昇格時に young 子があれば `REMEMBERED` を立て remembered set に登録。これで
     **昇格前から存在する old→young エッジ**（バリア成立前に生成）を救済。新規
     ストアはバリアが担当。
   - **JIT ivar write barrier**：`emit_write_barrier_rdi` を ivar inline ストア
     （`store_ivar_object_inline`）と ivar heap 高速パス（`store_ivar_heap_inner`）
     の直後に発行。fast path は `testb [rdi+FLAG], OLD/REMEMBERED` ＋ child の
     即値判定のみ（スクラッチ不要・borrow なし）、rare な slow path は
     `save_registers`/`restore_registers` で全 caller-saved を退避して
     `jit_write_barrier` を呼ぶ完全透過方式（生存解析不要）。
   - **昇格対象に OBJECT＋leaf を JIT 下でも追加**（`is_promotable`）。ivar ストア
     は Rust（`set_ivar`）も JIT（inline/heap）も全てバリア済みのため安全。Array は
     JIT 要素ストアのバリア未実装ゆえ no-jit 限定のまま。HASH/Struct も deferred。
   - 検証：default 全テスト green（既知 inspect 2 件除く）、**JIT** の
     gc-verify,gc-stress で object_stress/promo_stress が CRuby 一致（取りこぼし 0・
     クラッシュ無し）、gc-verify で object/method/string/struct/range green。

8. **残作業**：Array/Struct の JIT inline 要素・スロットストアへのバリア発行
   （`asmir/compile/index.rs` の `array_index_assign`、`store_struct_slot_*`）→
   Array/Struct を JIT 下でも昇格可能に。HASH は default 取得 getter を足して
   `young_child_exists` を精緻化のうえ昇格対象に。minor/major 閾値・`RGENGC_OLD_AGE`
   の最適化、optcarrot 等でのベンチ。
6. **JIT バリア最適化（6.2 A）**：インラインストアに old 判定＋slow path を発行。
7. **ヒューリスティクス調整**：minor/major しきい値、`RGENGC_OLD_AGE`、
   remembered set 肥大時のメジャー昇格を最適化。optcarrot 等でベンチ。

---

## 9. リスクと検証戦略

- **最大リスク：バリア取りこぼし → dangling**。
  - 緩和：フェーズ 4 まで全型 WB_UNPROTECTED で正しさを担保し、型単位で慎重に
    protected 化。
  - 検証：`gc-stress`（毎割り当て GC）下で、**「マイナー GC の結果がフル GC と
    一致する」検証モード**を追加（デバッグ機能 `gc-verify`：マイナー後に
    全グラフ走査して、回収されたセルが本当に到達不能か assert）。
  - ruby/spec・既存テストスイートの全緑を各フェーズで要求。
- **early promotion / フラグメント**：非移動ゆえ old のフリーリスト断片化が
  進む可能性。`salvage_empty_pages` は維持しつつ、メジャー GC 頻度で調整。
- **remembered set の偽陽性肥大**：保守的保持で over-scan。`REMEMBERED` ビットで
  重複登録は防止。メジャー時にリセット。
- **外部メモリ（malloc）**：world は別管理。`malloc_threshold` 超過は必ずメジャー。
- **fiber / YIELDER / heap_frames**：既存の昇格フレーム機構（`alloc.rs:137`）は
  ルート扱いを維持。fiber スタックも従来どおりルートから辿る。

---

## 10. 期待効果と限界

- **効果**：ライブ old ヒープが大きいワークロードでマイナー GC のマーク量が
  激減 → 平均ポーズ短縮・スループット向上。短命オブジェクト主体のベンチで顕著。
- **限界**：
  - 非移動のため**断片化は解消されない**（コンパクションは別課題）。
  - 全 young・短命のマイクロベンチでは利得が小さく、バリア分のオーバーヘッドが
    わずかに乗る可能性（JIT バリアの分岐予測でほぼ吸収可能）。
  - JIT インラインストアへのバリア実装が完了するまでは、主要型が
    WB_UNPROTECTED に留まり効果が限定的。

---

## 11. まとめ（推奨）

monoruby の非移動マーク&スイープ・シングルスレッド・セーフポイント GC は
**RGenGC 型の世代別化に非常に適している**。鍵は唯一、**書き込みバリアの網羅**
（特に JIT が直接発行する ivar/Struct ストア）。

推奨ロードマップ：
1. 既存コードを変えないメタデータ整備（ヘッダ flag＋`old_bits`）、
2. 全型 WB_UNPROTECTED の安全側でマイナー GC を稼働、
3. `gc-verify`＋`gc-stress` でバリア網羅を保証しつつ型単位で WB-protected 化、
4. 最後に JIT バリアを最適化、

という **「安全第一・漸進」**の順序で進めるのが最もリスクが低い。
