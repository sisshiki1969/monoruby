# x86-64 と aarch64 で JIT ロジックが異なる点

monoruby の JIT は **front-end（TraceIR → AsmIR）が arch 中立**で、x86-64 と
aarch64 で共有されます。一方 **AsmIR → 機械語 lowering とディスパッチ／再最適化
の機構はアーキごとに異なります**。本書はその「ロジックが異なる箇所」を検証付きで
列挙します（2026-06 時点。aarch64 の JIT 出力は段階的に成長中なので、未対応分は
今後変わり得ます）。

x86 の挙動が「正」、aarch64 は移植途上、という関係です。差分の多くは
**aarch64 がランタイムの分岐パッチ（self-modifying jump patch）を持たない**こと、
および **再最適化（recompile）機構が未実装**であることに起因します。

---

## 0. ビルド cfg モデル

`monoruby/build.rs` が 2 つの cfg を立てます:

| cfg | 意味 | 立つ条件 |
|-----|------|----------|
| `jit` | front-end（bytecode→TraceIR→AsmIR）を有効化 | `(x86_64 \|\| arm64) && !no-jit` |
| `jit_x86` | **x86-64 の機械語 emission** | `x86_64 && !no-jit` |

つまり **aarch64 = `jit && !jit_x86`**。`#[cfg(jit_x86)]` が x86 専用、
`#[cfg(all(jit, not(jit_x86)))]` が aarch64 専用の lowering です。

モジュール分割（[jitgen.rs](../monoruby/src/codegen/jitgen.rs)）:

| 役割 | x86-64 (`jit_x86`) | aarch64 (`not(jit_x86)`) |
|------|--------------------|---------------------------|
| AsmIR→機械語 | `asmir/compile/`（ディレクトリ） | [`asmir/compile_stub.rs`](../monoruby/src/codegen/jitgen/asmir/compile_stub.rs) |
| 型/クラスガード生成 | [`guard.rs`](../monoruby/src/codegen/jitgen/guard.rs) | [`guard_stub.rs`](../monoruby/src/codegen/jitgen/guard_stub.rs) |
| VM 段バックエンド | `arch/x86_64/` | `arch/aarch64/` |

aarch64 の `compile_asmir` は未対応 `AsmInst` で `false` を返し、`compile()` が
`None` を返してそのメソッドは VM 解釈のままになります（段階移植）。

---

## 1. JIT エントリのインストール／ディスパッチ機構

**最大の構造差**。継承された共有メソッド（1 つの `FuncId` を A/B 両クラスが使う）は、
本体が `self == self_class` を前提に内部の `self.foo` を単相に解決します。これを担保
するため両アーキとも **self クラスガードでメソッド本体を前置**しますが、実装が違います。

### x86-64（分岐パッチ + クラス別エントリ表）
[`patch.rs` `compile_patch`(`jit_x86`)](../monoruby/src/codegen/patch.rs):

- `compile_method` でコンパイル → `class_guard_stub(self_class, …)` を生成
  （`self.class != self_class` なら recompile stub へ）。
- `store[iseq].add_jit_code(self_class, patch_point, …)` で **`self_class` ごとに
  エントリを登録**。
- `apply_jmp_patch_address(entry, &guard)` で **メソッド入口の `jmp` を実行時に
  書き換え**てガードへ向ける。
- 別クラスの受信は guard miss → recompile stub が新クラス用に再コンパイルし、
  パッチでチェーンを延長。

### aarch64（間接ディスパッチ slot + ガードスタブのチェーン）
[`patch.rs` `compile_patch`(`not(jit_x86)`)](../monoruby/src/codegen/patch.rs) +
[`wrapper.rs` `a64_gen_class_guard_stub`](../monoruby/src/codegen/arch/aarch64/wrapper.rs):

- aarch64 は**ランタイム分岐パッチを使わない**。代わりに各メソッドの wrapper が
  ヒープに leak した **`jit_slot`（u64）を `ldr`→`br`** する間接ディスパッチ。
- `compile_patch` はコンパイル後、self クラスガードスタブを生成し、その**アドレスを
  slot に書き込む**。
- ガードスタブ: `self.class == self_class` なら jit code、不一致なら **`next_slot`
  （別の leak u64、初期 0）を辿る**。未設定なら専用カウンタを countdown して
  新クラス用に `jit_compile_patch` を呼び、チェーンを延長。

> 検証: 継承メソッドのポリモーフィックディスパッチ（`polymorphic_call`）。aarch64 は
> 当初 self ガード無しの単一 slot だったため、B でコンパイルした `S#g` を A も実行して
> `[2,2,..]` を返した（commit `83f92586` で修正）。

---

## 2. raise 時の PC 規約（off-by-one）

`handle_error`（[jit_module.rs](../monoruby/src/codegen/jit_module.rs)、arch 中立）は
**PC が「例外を起こした命令そのもの」を指す**前提です（例外表 `get_exception_dest`
の検索キー、retry/redo の disp 読み出し）。各アーキはこの「現在命令」を**別経路**で
作ります。

| | raise サイトが設定する PC | entry_raise が handle_error に渡す PC | 実効値 |
|--|--------------------------|--------------------------------------|--------|
| **x86-64** | 次の命令（`pc + 1`） | `r13 - 16`（1 bytecode=16B 戻す） | 現在命令 ✓ |
| **aarch64** | **現在の命令（`pc`）** | そのまま（減算なし） | 現在命令 ✓ |

- x86: [`init` の `raise:`](../monoruby/src/codegen/arch/x86_64/jit_module.rs) が
  `subq rcx, 16`。raise サイト（`method_return` / `gen_handle_error`）は `pc+1`。
- aarch64: [`a64_gen_entry_raise`](../monoruby/src/codegen/arch/aarch64/codegen.rs) は
  `mov x3, x(PC.0)`（減算なし）。VM の raise op（`a64_op_err1` 等）は PC を進めず
  現在命令のまま。

> 検証: `a64_method_ret` / `a64_gen_handle_error` が x86 を真似て `pc+1` にしていたが、
> aarch64 entry_raise には `-16` が無いため 1 命令ずれ、`ensure`/`rescue` がスキップ
> された（`method_return2`）。両サイトを `pc`（現在命令）に修正（commit `a9676377`）。

---

## 3. ガードミス時の再最適化（recompile） — **aarch64 は未実装**

x86 はガードミスで**メソッドを再コンパイル**します。aarch64 は**deopt するだけ**です。

| 契機 | x86-64 | aarch64 |
|------|--------|---------|
| class-version guard miss | `gen_recompile`（[guard.rs](../monoruby/src/codegen/jitgen/guard.rs) `guard_class_version`） | deopt のみ（[guard_stub.rs](../monoruby/src/codegen/jitgen/guard_stub.rs) `a64_guard_class_version`） |
| `AsmInst::RecompileDeopt` | `recompile_and_deopt`（counter 付き再コンパイル） | **plain deopt 扱い**（reason/position を無視、[compile_stub.rs](../monoruby/src/codegen/jitgen/asmir/compile_stub.rs) `RecompileDeoptimize`） |
| recompile API | `recompile_method` 等（[compiler.rs](../monoruby/src/codegen/compiler.rs)、`jit_x86`） | 無し |

aarch64 が recompile を持たない理由も**分岐パッチ非対応**（既存エントリを新コードへ
差し替える手段が無い）と、間接ディスパッチ slot 更新型 recompile が未実装なため。
deopt は VM 解釈へフォールバックするので**正しさは保たれる**（自己最適化しないだけ）。

---

## 4. 基本演算子（BOP）の再定義への追従

`Integer#*` 等の再定義時、JIT が畳み込み／fast-path した演算をどう無効化するかが
異なります。

- **共通**: `set_bop_redefine`（[codegen.rs](../monoruby/src/codegen.rs)）が
  `bop_redefined_flags` を立て、VM ハンドラを no-opt 版へ差し替える
  （`remove_vm_bop_optimization`）。**class_version も別途インクリメント**される
  （メソッド定義経由）。

- **x86-64**: 共有 front-end は **定数畳み込み（`100*100`→`10000`）自体に BOP ガードを
  付けない**（`ir.check_bop` は `MethodDef`/`SingletonMethodDef` の直後のみ。
  `trace_ir.rs` の op 160–170 は再定義の有無に関わらず常に `TraceIr::BinOp`）。x86 は
  `CheckBOP` lowering（[asmir/compile.rs](../monoruby/src/codegen/jitgen/asmir/compile.rs) `AsmInst::CheckBOP`）と
  class-version guard ミス時の `gen_recompile`（§3）という**再最適化インフラを持つ**ので、
  再定義はそれらで吸収される。
  > 注: x86 が `redefine_test2` 相当をどの経路で正しく処理するかの**逐次検証は本作業では
  > 未実施**（本機は arm64-darwin で x86 を実行できない）。確実なのは「x86 は recompile +
  > CheckBOP の両インフラを持つ」「aarch64 は recompile を持たない」という構造差まで。

- **aarch64**: recompile が無いので、**整数の定数畳み込みの直前に `AsmInst::CheckBOP` を
  emit**（[binary_op.rs](../monoruby/src/codegen/jitgen/compile/binary_op.rs)、
  `cfg(not(jit_x86))`）し、[`compile_stub.rs`](../monoruby/src/codegen/jitgen/asmir/compile_stub.rs) が
  `bop_redefined_flags != 0` なら **その BOP 命令へ deopt**。VM が no-opt ハンドラで
  再実行し、再定義メソッドを呼ぶ。これは「畳み込んだ値を捨て、その場で再解釈する」
  方式で、recompile に依存しない。

> 検証: `redefine_test2`（`100*100` を再定義後 42 に）。aarch64 は当初 deopt のみ＆
> ガード無しで `10000` を返し続けた。CheckBOP lowering + 畳み込み前ガードで修正
> （commit `bcd9d3e8`）。
>
> **限定理由**: 全整数 fast-path に CheckBOP を付けると、テストハーネスの
> **スレッドローカル CODEGEN 共有**で `bop_redefined_flags` が他テストから汚染され、
> deopt が多発して lib テストがフレーキー化した（3 回中 1 回 328 失敗）。
> **定数畳み込みケースのみ**に限定して安定（lib 連続 2 回 1631/0）。
> 既知の制限: レジスタ fast-path の整数演算は未ガード（従来挙動、テスト未カバー）。

---

## 5. 分岐の到達距離（aarch64 固有の制約）

aarch64 の**条件分岐は imm19 = ±1MB**、無条件 `b` は B26 = ±128MB。x86 は near/far の
区別が実質不要（32bit rel + パッチ）。

aarch64 では**遅延生成されるコード**（メソッド wrapper、BOP 再定義時に再生成される
VM ハンドラ、ガードスタブ）が起動時に置かれた `entry_raise` / `vm_entry` から
**±1MB を超える**ことがあり、それらへの**条件分岐は overflow**します。対策として
「条件分岐を反転して near ラベルを跨ぎ、`entry_raise`/`vm_entry` へは無条件 `b`」
パターンを使います。

該当箇所（いずれも commit `83f92586` / `2bed491a` で対応）:
- [wrapper.rs](../monoruby/src/codegen/arch/aarch64/wrapper.rs) の JIT トリガ（`cbnz vm_entry` → 反転 + `b vm_entry`）。
- [vmgen.rs](../monoruby/src/codegen/arch/aarch64/vmgen.rs) `a64_checked_store_next`（`cbz x0, raise` → 反転 + `b raise`）。BOP 再定義の再生成で発火。

---

## 6. ループ解析（fixpoint）での型 widening — aarch64 で露出した差

これは厳密には arch 差ではなく、**aarch64 がループ JIT 未配線でメソッド JIT 経由で
ループをコンパイルする**ために露出した front-end の挙動です。

ポリモーフィックでキャッシュ未充填の binop（`x += @x`）は、ループ先頭の widening で
オペランド型が不明になると `Recompile` で解析が打ち切られ、戻りエッジの fixpoint が
壊れて `(G, C)` のマージ unreachable に達しました。**解析パスでは未キャッシュ binop を
`S`（型不明スタック値）に widening して継続**するよう修正（commit `34feceb3`、
[binary_op.rs](../monoruby/src/codegen/jitgen/compile/binary_op.rs) `binop_uncached`、
`codegen_mode()` で解析/実コンパイルを区別）。x86 は内側ループが先にループ JIT され
この経路に入らないため顕在化していませんでした。

---

## 7. その他の x86 専用ロジック

- **インライン ASM**（`InlineFuncInfo::InlineGen`）は `jit_x86` 限定
  （[method_call.rs](../monoruby/src/codegen/jitgen/compile/method_call.rs)）。aarch64 は
  通常のメソッド呼び出しにフォールバック。
- **特殊化（Specialized）コンパイル / `MethodRetSpecialized` / `MethodDef` /
  `ClassDef` / `SingletonClassDef` の lowering** は aarch64 未対応。これらを含む
  メソッドは VM 解釈のまま。
- FP（xmm）の deopt write-back は aarch64 では未対応箇所で bail（→ VM）。

---

## まとめ（一行）

> front-end は共有。**emission・self ガードのインストール・raise の PC 規約・
> recompile の有無・BOP 再定義追従・分岐到達距離**が x86 と aarch64 で異なり、
> その大半は aarch64 が「ランタイム分岐パッチ非対応」「recompile 未実装」である
> ことに由来する。aarch64 側はいずれも**正しさを保ったフォールバック**（VM 解釈 /
> deopt / 間接ディスパッチ）で実装されている。
