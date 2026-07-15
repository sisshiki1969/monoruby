# monoruby の GC — 機構と実装

monoruby のガベージコレクタの**現行実装**を、コードに即して解説するドキュメント。
設計検討の経緯・代替案・移行計画は `doc/generational_gc_plan.md`(計画メモ)を
参照。本書は「いま実際に動いているもの」を対象とする。

> 補足: `CLAUDE.md` は GC を「mark-and-sweep」と一言で書いているが、現行実装は
> より正確には **非移動(non-moving)・単一スレッド・stop-the-world の
> 世代別 mark & sweep**(CRuby の RGenGC に相当)である。世代別化は既に有効で、
> オブジェクトは実際に old 世代へ昇格し、マイナー/メジャー GC が使い分けられる。
> `alloc.rs` に残る一部コメント(「old_bits is always empty」「not enabled yet」等)は
> 計画段階の記述で、実装より古い。実挙動は本書と該当コードを正とする。

主な実装ファイル:

| 対象 | ファイル |
| --- | --- |
| アロケータ・ページ・GC 本体 | `monoruby/src/alloc.rs` |
| `RValue` のヘッダ / マーク / 書き込みバリア | `monoruby/src/value/rvalue.rs` |
| セーフポイント・ルート走査・`execute_gc` | `monoruby/src/executor.rs` |
| GC poll のコード生成 | `monoruby/src/codegen/arch/{x86_64,aarch64}/…` |
| `GC` モジュールのビルトイン | `monoruby/src/builtins/gc.rs` |

---

## 1. 全体像

- **非移動 (non-moving)**: オブジェクトは一度確保したセルから動かない。コピーや
  コンパクションを行わないので、生ポインタ(`*const RValue`)を保持したまま GC を
  跨いでも安全。ページ・フリーリスト・スイープ機構をそのまま世代別化に流用できる。
- **単一スレッド・stop-the-world**: monoruby の VM は 1 本の OS スレッドで走る。
  GC は VM セーフポイントで同期的に実行され、並行 GC やインクリメンタル GC は
  持たない。
- **世代別 (generational)**: 弱い世代別仮説(多くのオブジェクトは若くして死ぬ)に
  基づき、マイナー GC ではマーク対象を「若い世代 + old→young 参照」に限定する。
  長命オブジェクトを多数抱えるワークロード(Rails 系・optcarrot 等)でのマーク
  コストを削減する。
- **保守的ではない (precise)**: ルートは明示的に列挙してマークする(スタックの
  値スキャンではない)。JIT コンパイル済みコードのセーフポイントでは、生きた
  レジスタをスタックに退避してからマークする。

コンパイルパイプライン全体における GC の位置づけは `CLAUDE.md` の
"Custom GC (`alloc.rs`)" と本書を対応させて読むとよい。

---

## 2. ヒープのレイアウト

### 2.1 アロケータ

```
thread_local! { pub static ALLOC: RefCell<Allocator<RValue>> }   // alloc.rs
```

`Allocator<RValue>` はスレッドローカルなシングルトン(`alloc.rs:153`)。
`RValue` は 64 バイト固定(`GCBOX_SIZE`、`Allocator::new` で
`assert_eq!(64, GCBOX_SIZE)`)。

主なフィールド(`alloc.rs:298` 付近):

| フィールド | 意味 |
| --- | --- |
| `current_page` / `head_page` / `pages` | 現ページ / 最上位ページ / 割り当て済みページ一覧 |
| `used_in_current` | 現ページのバンプ位置 |
| `free` / `free_list_count` | フリーリスト先頭と要素数 |
| `free_pages` | 空きになって再利用待ちのページ |
| `total_gc_counter` / `minor_gc_count` / `major_gc_count` | GC 回数の各カウンタ |
| `minors_since_major` | 直近メジャー以降のマイナー回数(kind 判定に使用) |
| `old_count` | old 世代オブジェクト数(昇格で +1、メジャーで 0 リセット) |
| `old_major_threshold` | 適応的メジャー閾値(`old_count` がこれに達したら次はメジャー) |
| `promoting` | マーク中に昇格候補を収集するか(実マーク中のみ true) |
| `aging` | 今サイクルで生存した昇格候補(マーク後に加齢) |
| `remembered` | remembered set(old→young 参照を持つ old オブジェクト) |
| `alloc_flag` | GC 起動フラグ(`u32`)のアドレス |
| `heap_frames` | ヒープに退避したフレームバッファの登録表(§9) |

### 2.2 アリーナとページ

```
const SIZE: usize        = 64;
const GCBOX_SIZE: usize  = size_of::<RValue>();          // 64
const PAGE_LEN: usize    = 64 * SIZE;                     // 4096 セル/ページ
const DATA_LEN: usize    = 64 * (SIZE - 1);               // 4032 データセル
const THRESHOLD: usize   = 64 * (SIZE - 2);               // 3968(alloc_flag を立てる位置)
const ALLOC_SIZE: usize  = PAGE_LEN * GCBOX_SIZE;         // 262144 = 256KB
const MAX_PAGES: usize   = 8192;
```

- アリーナは起動時に **`ALLOC_SIZE * MAX_PAGES`(= 2GB)を 1 回だけ予約**する
  (`Allocator::new`、`System.alloc`)。実 RSS はページを使うぶんだけ増える
  (予約は仮想アドレス空間)。ページは 256KB 境界に整列。
- ページからポインタへの逆引きは**アドレスマスク**で O(1):
  `get_page(ptr) = ptr & !(ALLOC_SIZE - 1)`(`alloc.rs:1159`)。これにより任意の
  `*const RValue` から所属ページ(とマークビット)を即座に求められる。

`Page<T>`(`alloc.rs:1233`)の構造:

```
struct Page<T> {
    data:      [T; DATA_LEN],       // 4032 セル
    mark_bits: [u64; SIZE - 1],     // 63 ワード = セル1つにつき1ビットのマークビットマップ
    old_bits:  [u64; SIZE - 1],     // 63 ワード = old 世代ビットマップ(mark_bits と並行)
}
```

`size_of::<Page<T>>() <= ALLOC_SIZE` が `Allocator::new` で保証される。
`data` の後ろにビットマップ 2 枚が同居する(セル本体の外にマークを置く
mark-external 方式なので、生存中のオブジェクト内容を汚さない)。

---

## 3. 割り当て(`Allocator::alloc`)

`alloc.rs:674`。順序は以下:

1. **フリーリスト**が空でなければそこから 1 セル pop(`self.free`)。直前の GC で
   スイープされたセルの再利用。
2. 空でなければ**現ページのバンプ割り当て**。
   - `used_in_current == THRESHOLD`(3968)に達したら `set_alloc_flag()` で
     `alloc_flag += 1`(GC を要求;§4)。
   - `used_in_current == DATA_LEN`(4032)でページ満杯 → `free_pages` から再利用、
     なければ `new_page()` で新規ページ。新ページは `clear_old_bits()` で
     old ビットマップを 0 初期化(マイナー GC のシード整合性のため)。

### JIT インライン高速パス

フリーリストからの pop は JIT がインライン展開できるよう、アロケータが
生アドレスを公開している:

- `free_list_head_addr()`(`self.free`) — `alloc.rs:559`
- `free_list_count_addr()` / `total_allocated_addr()` — 統計の同期用

JIT コードはセーフポイント外でのみこれらを触る(Rust 側が `ALLOC` を借用中や
`gc()` 実行中は触らない)ため、単一スレッド前提でエイリアスは生じない。

---

## 4. GC のトリガとセーフポイント

GC は「アロケーションの延長で即実行」はしない。JIT の生きたレジスタが未退避の
まま GC ルート走査に入るのは危険なため、**フラグを立てて次のセーフポイントで
実行**する。

### 4.1 起動フラグ `alloc_flag`(`u32`)

VM/JIT が参照する単一の `u32`。**8 以上でトリガ帯**。これを立てる経路:

| 経路 | 実装 | フラグ操作 |
| --- | --- | --- |
| ページ充填 | `set_alloc_flag`(`alloc.rs:582`) | ほぼ満杯ページごとに `+= 1`(約 8 ページで 8 に到達) |
| malloc 圧(§8) | `request_gc_if_malloc_over`(`alloc.rs:114`) | 8 未満なら `8` を書く |
| `GC.start` | `request_gc(true)`(`alloc.rs:84`) | 8 未満なら `8` を書く + メジャー強制 |
| シグナル配送 | シグナルハンドラ(`jit_module.rs`) | `+= 10`(§`doc/signal_handling.md`) |

「8 未満のときだけ 8 を書く」ことで、ページ充填の累積値やシグナルの `+10` を
踏み潰さず、ちょうど 1 回の収集を要求する。

### 4.2 poll のコード生成

`execute_gc_inner`(`codegen/arch/x86_64/jit_module.rs:247`)が poll を出力:

```asm
cmpl [rip + alloc_flag], 8
jge  gc          ; フラグ >= 8 なら収集パスへ
exit:
; gc: (別ページ)
;   write_back(生きたレジスタを退避)
;   call exec_gc      ; = execute_gc()
;   testq rax, rax
;   jne  exit         ; nil 以外(=正常)なら復帰
;   jmp  error        ; None(=例外/シグナル)なら伝播
```

この poll は**メソッド呼び出しやループのバックエッジ**などのセーフポイントで
実行される(`vm_execute_gc`;`vmgen/method_call.rs` ほか)。aarch64 backend も
同等のフラグ比較を出力する。

### 4.3 `execute_gc`(`executor.rs:3688`)

セーフポイントから呼ばれる `extern "C"` 関数。順に:

1. `watchdog::poll()` — ハングウォッチドッグのカウントダウンをリセット。
2. **保留シグナルの処理** — `pending_signals` ビットマップを drain し、最小番号の
   シグナルを `Signal.trap` ハンドラ呼び出し / 既定例外(SIGINT ⇒ `Interrupt` 等)に
   変換(`doc/signal_handling.md`)。
3. `parent_fiber` を辿って**ルート Executor(最上位ファイバ)**へ。
4. `ALLOC.with(|a| a.borrow_mut().gc(&Root { globals, executor }))` で収集実行。

---

## 5. オブジェクトヘッダとフラグ

`RValue` 先頭の `Header` は union(`rvalue.rs`):

```
union Header { next: Option<NonNull<RValue>>, meta: Metadata }

struct Metadata {          // rvalue.rs:2247
    flag:  u16,
    ty:    Option<ObjTy>,  // 1 バイト
    _padding: u8,          // JIT の 2 バイト cmpw が隣接バイトを読むため 0 固定
    class: Option<ClassId>,
}
```

- フリーリスト上のセルは `next`(次の空きセル)として解釈され、生存セルは `meta`。
- `_padding` を 0 に固定するのは、JIT が型判定に `cmpw [ty]`(2 バイト)を使い
  隣接バイトも読むため。したがって**世代別 GC の age は `ty` の隣ではなく
  `flag` の上位バイトに置く**。

### `flag: u16` のビット割り当て(`rvalue.rs:2293` 以降)

| ビット | マスク | 意味 |
| --- | --- | --- |
| 0 | `0b0000_0001` | LIVE(生存;確保時 `flag = 1`) |
| 1 | `0b0000_0010` | FROZEN |
| 2 | `0b0000_0100` | CHILLED(`Symbol#to_s` 由来の準 frozen 文字列) |
| 3 | `0b0000_1000` | **OLD**(old 世代へ昇格済み) |
| 4 | `0b0001_0000` | WB_UNPROTECTED(shady 用に予約。現状**未使用** — §7.3) |
| 5 | `0b0010_0000` | **REMEMBERED**(remembered set に登録済み) |
| 6 | `0b0100_0000` | **WB_PENDING / armed**(old かつ未 remembered = 書き込みバリアの slow path 対象) |
| 8..15 | 上位バイト | **age**(生存回数;`RGENGC_OLD_AGE` で昇格) |

新規オブジェクトは `flag == 1` なので、OLD / REMEMBERED / WB_PENDING はすべて 0
(= young・バリア対象外)、age は 0 から始まる。

`arm_barrier`(WB_PENDING を立て REMEMBERED を落とす)と
`enter_remembered`(REMEMBERED を立て WB_PENDING を落とす)は、この 2 ビットを
**排他**に保つ。書き込みバリアの高速パスがこの 1 ビット(WB_PENDING)テストだけで
済むのはこの排他性による。

---

## 6. 世代別 GC 本体(`Allocator::gc`, `alloc.rs:750`)

### 6.1 マイナー / メジャーの選択(`decide_gc_kind`, `alloc.rs:740`)

```
old_count >= old_major_threshold  ||  minors_since_major >= MAX_MINORS_PER_MAJOR
    → Major     それ以外 → Minor
```

- **適応的メジャー閾値** `old_major_threshold`: メジャー直後に
  `max(old_count * OLD_GROWTH_FACTOR, OLD_OBJECT_FLOOR)` へ再設定
  (`OLD_GROWTH_FACTOR = 2`, `OLD_OBJECT_FLOOR = 16384`)。old 世代が安定していれば
  メジャーは稀(世代別の利得を保つ)、浮遊ゴミを昇格し続けるワークロードでは
  頻繁にメジャーして RSS を抑える。CRuby の `RGENGC_OLD_OBJECT_LIMIT_FACTOR` に相当。
- **`MAX_MINORS_PER_MAJOR = 64`**: 安全上限。適応閾値が発火しなくても、64 回に 1 度は
  必ずメジャーして remembered set を作り直し、浮遊 old ゴミを回収する。
- `GC.start` は `GC_FORCE_MAJOR` を立てるので、次の収集は無条件にメジャー。

### 6.2 マークビットマップの準備

| kind | 操作 |
| --- | --- |
| **Major** | `clear_mark()`(mark_bits=0)+ `clear_old()`(old_bits=0, `old_count=0`)+ `remembered.clear()`。全オブジェクトが収集候補に戻り、ルートから再マーク・全スイープ。 |
| **Minor** | `seed_marks()`。各ページで `mark_bits ← old_bits` をコピー(`seed_mark_from_old`)。old オブジェクトは**最初からマーク済み**とみなされ、再走査もスイープもされない。 |

### 6.3 マークフェーズ

1. `self.promoting = true` にしてから `root.mark(self)`(ルートは §8)。
2. `RValue::mark`(`rvalue.rs:729`)は `gc_check_and_mark` でビットを立て、未マーク
   だった場合のみ `mark_children` で子を辿る(深さ優先)。
3. `gc_check_and_mark`(`alloc.rs:889`)は、初めてマークしたセルが `promoting` かつ
   `is_promotable()` なら `aging` に積む(昇格候補の収集)。ヘッダ書き換えは
   マーク走査が握る `&self` とエイリアスしないよう**マーク後に遅延**する。
4. **Minor のみ** `mark_remembered()`:remembered set の各 old オブジェクトの
   *子だけ*を `mark_children` で辿る(親 old は既にシードマーク済み)。これにより
   「old からしか参照されていない young オブジェクト」に到達する。走査後、若い子が
   いなくなった entry は set から外して `arm_barrier`(自己クリーニング;
   `alloc.rs:1020`)。
5. `self.promoting = false`。

### 6.4 加齢と昇格(`apply_aging`, `alloc.rs:920`)

マーク完了後(生きた `&self` が無い状態)に:

- **Pass 1**: `aging` の各生存者の age を +1(`age_and_check_promote`)。
  `age >= RGENGC_OLD_AGE`(= 3)に達したものを昇格:`old_bits` をセット +
  ヘッダ OLD をセット + `old_count += 1`。
  → **即時昇格ではなく「3 回生存したら昇格」**。1 回の収集でたまたま生きていた
    短命オブジェクトを old に上げてしまい浮遊ゴミ化するのを避ける。
- **Pass 2**: remember-on-promote。昇格したオブジェクトが**まだ young を参照して
  いる**(`young_child_exists`)なら remembered set に追加(バリア導入前から存在した
  old→young 辺をカバー)。young 参照が無ければ `arm_barrier` して以後の young ストアに
  備える。

### 6.5 マイナー後の検証(`gc-verify` フィーチャ)

マイナー GC の後、シード無し・昇格無しでルートから全ライブグラフを独立に再マーク
する(`alloc.rs:851`)。もしマイナーが到達可能なオブジェクトを解放していれば
(バリア漏れ/remembered set 漏れ)、この走査が解放済みセルに到達し
`RValue::mark` の `is_live` アサートが発火する。世代別 GC の健全性テスト。

---

## 7. 書き込みバリア

world 停止型・非移動なので、必要なのは **old→young 辺を remembered set に記録する
だけ**の単純なバリア。

### 7.1 実体(`RValue::write_barrier`, `rvalue.rs:1072`)

```rust
pub(crate) fn write_barrier(&mut self, child: Value) {
    if self.is_wb_pending() && !child.is_packed_value() {
        self.enter_remembered_set();
    }
}
```

- **高速パスはヘッダ 1 ビットのテスト**(`is_wb_pending` = WB_PENDING ビット)。
  young オブジェクトも、既に remembered な old オブジェクトも、このビットが 0 なので
  即 return(アロケータに触れない)。
- 子の世代は見ない(old→old を覚える過剰近似は無害)。即値(`is_packed_value`)は除外。
- `write_barrier_bulk`(`rvalue.rs:1085`)は `Array#concat` / `Hash#[]=` などの
  複数要素ストア用。個々の子を見ず、armed なら無条件に記録する過剰近似。

呼び出しは「参照型フィールド(ivar / 配列・ハッシュ要素 / struct スロット)へ
`child` を格納した**後**」。インタプリタ経路(`set_ivar`、Array/Hash ラッパ、
`Value::set_struct_slot` 等)と、JIT が出力するインラインバリア
(`emit_write_barrier_rdi`)の両方でカバーされる。

### 7.2 状態遷移

```
young(flag=1) ──[age>=3 で昇格]──▶ old
   昇格時に young 子あり ─▶ enter_remembered (REMEMBERED=1, WB_PENDING=0) ── remembered set 登録
   昇格時に young 子なし ─▶ arm_barrier      (WB_PENDING=1, REMEMBERED=0) ── 以後の young ストアを待つ
   armed な old に young ストア ─▶ write_barrier ─▶ enter_remembered_set ── set 登録 + WB_PENDING=0
   minor 走査で young 子が消えた remembered ─▶ arm_barrier に戻す(自己クリーニング)
```

REMEMBERED と WB_PENDING は常に排他。remembered set の大きさは「生きた old→young 辺の
数」に比例し続ける(かつて young 子を持っていた全昇格オブジェクトには比例しない)。

### 7.3 昇格可能性(`is_promotable`, `rvalue.rs:875`)

昇格してよいのは「**そのオブジェクトへの Value 格納経路がすべてバリア保護されている**」
型のみ。現状 `ty()` で判定し、以下が `true`:

```
OBJECT | STRING | BIGNUM | FLOAT | ARRAY | STRUCT | HASH
```

- OBJECT と各リーフ(String バイト列 / Bignum / ヒープ Float)は ivar 経由でしか
  Value を持たず、ivar ストアは全経路バリア済み。
- Array/Struct の要素ストア、Hash ストアもインタプリタ・JIT 双方でバリア済み。
- それ以外の型は昇格しない(マイナーで毎回走査される young のまま)。

> **WB_UNPROTECTED(bit4)** は「shady(バリアで追えない)オブジェクトは昇格しない」
> ための予約フラグだが、現状 `is_promotable` は型のみで判定し、このフラグは
> 参照されていない(`set_wb_unprotected` の呼び出し箇所は無い)。将来のための予約。

---

## 8. ルート(マーク開始点)

`Root`(`executor.rs:3662`)の `mark` が起点:

```
Root::mark → YIELDER.mark   (ブロック/ファイバの yielder)
           → Globals::mark  (globals.rs)
           → Executor::mark (executor.rs:197)
```

`Executor::mark`(`executor.rs:234`)が辿るもの:

- `temp_stack` の全 Value(ビルトインが GC を跨いで生かしたい一時値の退避先)。
- `cfp` 連鎖の各 `lfp()`(= すべての生きたスタックフレームのローカル変数・レシーバ等)。
- `lexical_class` 上の `DefinitionContext::Receiver(Value)`
  (`instance_eval`/`instance_exec` 中のレシーバ)。
- 保留例外 `exception`(`MonorubyErr` は packed Value を持つ;`MonorubyErr::mark`)。
- マッチ処理の一時退避 `sp_match_regex` / `sp_match_haystack`。
- `deferred_unwind`(ensure で中断した `MethodReturn`/`Throw` が握る Value と Lfp)。

`Globals::mark` はクラステーブル・定数・グローバル変数・呼び出しサイト等の
恒久ルートをマークする。

---

## 9. スイープと空きページの回収

### スイープ(`sweep`, `alloc.rs:1098`)

ページごとに `mark_bits` を 64 ビット単位で走査(`sweep_bits`)。未マークセルを
`free()`(型に応じて `ManuallyDrop::drop`;`rvalue.rs:746`)してフリーリストに連結。
`trailing_ones` でマーク済みの連続領域を一気に飛ばす最適化がある。最後に
`self.free` がフリーリスト先頭に、`free_list_count` が回収数になる。

`free()` は多重呼び出しに耐える(`is_live()` を先頭で確認)。フリーリスト上のセルは
次のスイープでもう一度 free されうるため。

### 空きページの回収(`salvage_empty_pages`, `alloc.rs:1079`)

スイープ前に、全セルが未マーク(`all_dead`)のページを `pages` から外して
中身をドロップし `free_pages` へ戻す。以後の割り当てで再利用される(OS へは返さず、
アリーナ予約内で回す)。

---

## 10. ヒープに退避したフレーム(`heap_frames`)

クロージャ等でスタックフレームがその生成メソッドより長生きする場合、フレームは
`move_frame_to_heap` / `heap_frame` により `Box<[u64]>` としてヒープへ退避され、
`Box::into_raw` でリークされる。この生バッファを GC が回収できるよう、LFP アドレスを
キーに `heap_frames` へ登録する(`register_heap_frame`, `alloc.rs:464`)。

- マーク時、生きた LFP から到達したフレームに `marked` を立てる。
- `sweep_heap_frames`(`alloc.rs:504`)が、**2 サイクル連続で未マーク**だった
  フレームの `Box<[u64]>` を解放する(1 サイクルの猶予は昇格→ルート格納の窓を
  カバーするため)。
- キーは 8 バイト整列の LFP アドレスなので、既定の SipHash ではなく Fibonacci ハッシュ
  1 回(`AddrHasher`)で引く(`gc-stress` 下では毎確保ごとに引かれるため速度が効く)。

`heap_frames` が空のときは関連処理を丸ごとスキップし、コスト 0(optcarrot 等は
フレーム退避が稀)。

---

## 11. malloc 連動トリガ(外部バッファ圧)

`RValue` アリーナの圧力だけでは、`String#<<` ループのように **`RValue` をほとんど
作らずに malloc メモリだけ膨らむ**ケースを検知できない。そこでグローバル
アロケータ自身が外部バッファ量を追跡する:

- `RurubyAlloc`(`#[global_allocator]`, `alloc.rs:7`)が `alloc`/`dealloc` で
  `MALLOC_AMOUNT` を増減。
- **`MALLOC_TRACK_LIMIT = 64MB` 以上の確保は無視**。これは JIT メモリ予約
  (monoasm が起動時に 3 × 256MB を確保)のような一過性インフラ確保を除外するため。
  無視すると閾値が GB 級に張り付き、通常の String/Array/Hash 成長で永遠に GC が
  発火しなくなる。同じ判定で `dealloc` も gate するので `MALLOC_AMOUNT` は
  アンダーフローしない。
- `request_gc_if_malloc_over`(`alloc.rs:114`)が `MALLOC_AMOUNT >= MALLOC_GC_THRESHOLD`
  で `alloc_flag` を 8 に持ち上げる(GC 要求;割り当てフリーで安全)。
- 閾値 `MALLOC_GC_THRESHOLD` は各 GC 後に
  `malloced + max(malloced/2, MALLOC_THRESHOLD)` へ再設定(`alloc.rs:868`)。
  加算のみだと巨大ヒープでも 256KB ごとに GC してしまうので、乗算項で比例させる。
- この経路の収集は**メジャー強制しない**(一過性バッファは若くして死ぬのでマイナーで
  回収でき、old のバッファゴミは §6.1 のメジャートリガが拾う)。

---

## 12. GC の制御(`GC` モジュール, `builtins/gc.rs`)

| メソッド | 実装 | 挙動 |
| --- | --- | --- |
| `GC.start` | `request_gc(true)` | 次セーフポイントで**メジャー**収集を要求。引数(`full_mark:` 等)は互換のため受けるが無視(常に full)。インライン実行は危険なので即実行はしない。 |
| `GC.disable` / `GC.enable` | `Globals::gc_enable(false/true)` | GC の有効/無効を切り替え、直前の disable 状態を bool で返す。`GC_ENABLED`(§4 の malloc 経路が参照)も同期。 |
| `GC.count` | `total_gc_counter` | 総 GC 回数。 |
| `GC.stat` | `stat`(CRuby 互換キー) | `count` 等一部のキーだけが実カウンタに対応、他は 0 を返す。 |

コマンドラインでは `--no-gc` で GC を無効化できる。GC 無効時は `gc()` が即 return
するため、`request_gc_if_malloc_over` は `GC_ENABLED` を見て要求自体をスキップする
(さもないとフラグがトリガ帯に張り付いて poll が空回りする)。

---

## 13. デバッグ・検証用フィーチャ

| フィーチャ | 効果 |
| --- | --- |
| `gc-log` | 終了時に GC 統計を出力(old 数の実 popcount 等)。 |
| `gc-debug` | GC 中の各種アサート・ダンプ。`old_count` と実 popcount の一致検証など。 |
| `gc-stress` | **毎回のアロケーションで GC** を走らせる(`bin/test` が使用)。世代別のバリア/remembered set 漏れを最も強く炙り出す。 |
| `gc-verify` | マイナー GC 後に独立フル再マークで健全性検証(§6.5)。 |

---

## 14. まとめ

- monoruby の GC は **非移動・単一スレッド・stop-the-world の世代別 mark & sweep**。
- 256KB ページ + マーク/old の 2 枚のビットマップ(mark-external)で、非移動と
  世代別を両立。ページはアドレスマスクで O(1) 逆引き。
- 割り当てはフリーリスト → バンプ。閾値到達で `alloc_flag` を立て、**次のセーフ
  ポイント**で `execute_gc` が同期収集する(JIT レジスタ退避のため即実行はしない)。
- 世代別の心臓部は、**3 回生存で昇格(aging)**・**適応的メジャー閾値**・
  **1 ビット高速パスの書き込みバリア + remembered set(自己クリーニング付き)**。
  マイナーは old をシードマークして young + old→young 辺だけを辿る。
- 外部 malloc 圧・シグナル・`GC.start` も同じ `alloc_flag` 経由で同一のセーフ
  ポイント収集に集約される。

設計上のトレードオフ・却下案・今後の拡張余地は `doc/generational_gc_plan.md` を参照。
