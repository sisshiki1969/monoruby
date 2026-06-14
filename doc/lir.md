# Unified low-level IR (LIR)

Design notes and migration plan for the arch-neutral, machine-level IR that
sits between `AsmIR` and the per-arch `monoasm!` / `monoasm_arm64!` byte
emission. This is **Phase-1 item ①** ("a低レベルIR that describes amd64 and
aarch64 uniformly").

Status: **Stage 1 — data model defined, not yet wired into the pipeline.**
The model lives in
[`monoruby/src/codegen/jitgen/lir.rs`](../monoruby/src/codegen/jitgen/lir.rs).

---

## 1. Motivation (post-`#704`)

The original justification for a unified IR was "close the aarch64 *bail* gap".
That justification is **gone**: the full aarch64 port (`#704`) made aarch64
lower every `AsmInst` (large immediates are materialized through scratch
registers; see `doc/arch_difference.md`). The `bool` bail return is now
vestigial.

The remaining — and still real — motivation is **description unification**:

- The AsmIR → machine-code step is two parallel sets of emission primitives:
  ≈145 `emit_*` methods on x86 (`arch/x86_64/compile/*.rs`) and ≈143 on aarch64
  (`arch/aarch64/compile.rs`). Each backend re-implements register moves, frame
  addressing, immediate-range handling and FP-pool save/restore by hand.
- The aarch64 backend already factors immediate-range handling into helpers
  (`a64_frame_load`, `a64_field_load`, `a64_sp_sub`, `a64_addr_sub`, …), each of
  which folds a small displacement into the instruction or materializes a large
  one into scratch `x9`/`x10`. That pattern is exactly what a low-level IR with
  an addressing-mode abstraction formalizes — **once**, instead of per
  primitive.
- LIR is also the concrete **code-generation target** that the future
  interpreter/JIT DSL (Phase-1 item ③, "derive interpreter + JIT from one
  description via partial evaluation") lowers to. Building it now gives ③ a
  well-defined, hand-validated target.

So Stage 1 is worth doing for unification and as ③'s foundation — but its
priority should be weighed against that, not against a (now non-existent)
coverage gap.

---

## 2. Where LIR sits

```text
TraceIR
  → AsmIR (AsmInst, arch-neutral, register-allocated)
  → [compile_asmir dispatcher]                       jitgen/asmir/compile_shared.rs
  → LIR (arch-neutral machine ops + logical addressing)   ← NEW (this layer)
  → [per-arch LirEncode + legalize]                  arch/<arch>/lir_encode.rs (future)
  → monoasm! / monoasm_arm64!
  → bytes
```

LIR does **not** replace `AsmIR`. `AsmIR` stays the register-allocated,
arch-neutral *front-end* output (one `AsmInst` per semantic operation, often a
macro-op that expands to a runtime call). LIR is the *machine-level* tier below
it: a small set of real machine instructions with logical operands.

---

## 3. Data model (Stage 1)

Defined in `lir.rs`. The model deliberately starts small, covering the integer
move / memory / ALU / branch core migrated first in Stage 2.

| Type        | Role |
| ----------- | ---- |
| `GP`        | General-purpose register. **Reused as-is** from `codegen.rs`; already arch-neutral (maps to x86 regs / A64 `x0..x23` per `target_arch`). |
| `FPReg`     | Virtual FP register (phys-xmm-or-spill). Reused; FP `LInst`s come in a later stage. |
| `LOperand`  | `Reg(GP)` or `Imm(i64)` — an ALU/compare source that the encoder folds or materializes. |
| `LMem`      | *Logical* memory location with **unbounded** displacement: `Slot(SlotId)` (LFP-relative, negative disp), `Field { base, disp }` (object field, positive disp), `RspRel { disp }` (callee-frame arg slot). |
| `LCond`     | Signed integer branch condition (`Eq/Ne/Lt/Le/Gt/Ge`), with `from_int_cmp` / `invert`. Float/NaN-aware compares get dedicated variants later. |
| `LAluOp`    | `Add/Sub/Mul/And/Or/Xor/Shl/Sar`, with `from_binop`. |
| `LInst`     | One machine instruction: `Mov`, `LoadImm`, `Load`, `Store`, `StoreImm`, `Alu`, `Cmp`, `Label`, `Br`, `CondBr`. |
| `Lir`       | A straight-line `Vec<LInst>` with an ergonomic builder (`.mov().load().alu()…`). |

Branch targets use the existing front-end `JitLabel` (resolved to a monoasm
`DestLabel` by `JitContext::resolve_label`), matching how `AsmInst` carries
labels.

### The legalization contract (the core idea)

`LMem` displacements and `LOperand::Imm` values are **unbounded**. The per-arch
encoder is solely responsible for legalizing them:

- **x86-64**: `[reg + disp32]` and `imm32`/`imm64` cover essentially everything;
  legalization is mostly a no-op.
- **aarch64**: fixed-width encodings allow only small immediates. The encoder
  folds a fitting displacement into the instruction (`ldur`/`stur` ±256,
  scaled `ldr`/`str` ≤ 32760, `sub sp` ≤ 4095) and otherwise materializes the
  byte offset into reserved scratch `x9`/`x10` and uses a register-offset form —
  i.e. exactly what `a64_frame_load` / `a64_field_load` / `a64_addr_sub` do
  today, but in one place.

This is the single biggest structural win: the ~two dozen ad-hoc range checks
currently scattered across the aarch64 primitives collapse into one legalizer.

---

## 4. The per-arch encoder seam

Stage 2 adds a per-arch lowering implemented on `Codegen`:

```rust
trait LirEncode {
    /// Emit one already-register-allocated `LInst`, legalizing immediates and
    /// displacements. May use the arch's reserved scratch registers freely
    /// (x9/x10 on aarch64).
    fn encode(&mut self, inst: &LInst, labels: &SideExitLabels);
}
```

`x86_64` impl emits via `monoasm!`; `aarch64` impl via `monoasm_arm64!` plus the
existing `a64_*` addressing helpers. The seam is declared (commented) in
`lir.rs` for visibility but left unimplemented in Stage 1.

---

## 5. Migration plan (family by family, behind the dispatcher)

Each `AsmInst` family is migrated independently and is independently
revertible, because the change is local to its `emit_*` primitive: the primitive
builds an `Lir` and runs it through `encode` instead of emitting bytes directly.
`compile_asmir`'s dispatch is untouched.

1. **Stage 2 — move / memory / ALU / branch core.** `RegMove`, `RegToAcc`,
   `AccToStack`, `RegToStack`, `StackToReg`, `LitToReg`, `LitToStack`,
   `RegAdd`, `RegSub`, `IntegerBinOp` (Add/Sub/Mul fast path), `IntegerCmp`,
   `IntegerCmpBr`, `CondBr`. These are the families behind the simplest
   primitives and exercise the whole `LMem` + legalizer path.
2. **Stage 3 — FP transfer / compare.** `FprMove`, `FprSwap`, `F64ToFpr`,
   `FixnumToFpr`, `FloatToFpr`, `FprToStack`, `FloatBinOp`, `FloatUnOp`,
   `FloatCmp(Br)`, `I64ToBoth`. Adds FP `LInst` variants; NaN-aware compares are
   dedicated ops (x86 `ucomisd`+`setp` vs aarch64 `fcmp`+MI/LS).
3. **Stage 4 — runtime-call macro-ops.** `NewArray`/`NewHash`/`defined?`/
   `GenericBinOp`/`class_def`/… lower to an `LInst::Call` sequence with FP-pool
   `Save`/`Restore` expressed in LIR, so the save/restore is written once.
4. **Stage 5 — patch / recompile machinery.** Return-address patch points and
   in-place recompile (the two remaining x86/aarch64 *non-coverage*
   asymmetries, see `doc/arch_difference.md` §4). Hardest; may stay partly
   arch-specific.
5. **Stage 6 — cleanup.** Drop the now-vestigial `bool` returns of migrated
   families; delete dead per-arch code; the aarch64 `compile.rs` monolith
   shrinks and can be split to mirror x86.

---

## 6. Verification strategy (replaces the old bail-metric)

Because bails no longer exist, the Stage-0 "count aarch64 bails" metric is moot.
The safety net is instead **behaviour preservation**:

- **x86 byte-identity.** For a fixed corpus (`benchmark/`, selected `tests/`),
  the machine code emitted before and after each family migration must be
  byte-identical on x86 (the reference backend is not changing *what* it emits,
  only *how* the emission is expressed). The `emit-asm` feature dumps the
  generated asm for diffing.
- **aarch64 correctness.** `bin/test-aarch64` (qemu / native arm64) must stay
  green; where aarch64 output legitimately changes (e.g. a large-offset path now
  goes through the unified legalizer), correctness is checked against CRuby via
  the normal `run_test` harness rather than byte-identity.
- **Unit tests** on the LIR model itself (`lir.rs` `#[cfg(test)]`): condition
  inversion, builder ordering, `from_binop` / `from_int_cmp` mappings.

The north-star is "two `emit_*` sets collapse toward one shared description with
no behaviour change", measured by lines of per-arch lowering removed and the
green test matrix — not by a bail counter.
