# Unified low-level IR (LIR)

LIR is the arch-neutral, machine-level instruction set that sits between
`AsmIR` and the per-arch `monoasm!` / `monoasm_arm64!` byte emission. Its
model lives in `monoruby/src/codegen/jitgen/lir.rs`; the lowering
dispatcher is `Codegen::compile_asmir` in
`monoruby/src/codegen/jitgen/asmir/compile_shared.rs`, and each backend
defines one `Codegen::encode_linst` in `arch/<arch>/compile/mod.rs`.

---

## 1. Where LIR sits

```text
TraceIR
  → AsmIR (AsmInst, arch-neutral, register-allocated)
  → compile_asmir                         jitgen/asmir/compile_shared.rs
      ├─ LInst (arch-neutral machine ops)  jitgen/lir.rs
      │    → Codegen::encode_linst          arch/<arch>/compile/mod.rs
      └─ emit_* helpers (per arch)          arch/<arch>/compile/*.rs
  → monoasm! / monoasm_arm64! → bytes
```

`AsmIR` is the front end's output: one `AsmInst` per semantic operation,
frame-independent, with floating-point registers already virtual
(`FPReg`). `compile_asmir` lowers each `AsmInst` in one of two ways:

- **Through LIR.** The operations whose lowering is the same shape on both
  arches — moves, slot / field memory, ALU and compares, branches, the
  write barrier, guards, fixnum and float arithmetic, side exits — are
  built as one or more `LInst`s and handed to `encode_linst`. The per-arch
  encoder owns the instruction selection and the legalization (§3).
- **Directly.** An operation that is a single call of a per-arch `emit_*`
  helper with the `AsmInst`'s own fields (array / hash / string
  construction, `defined?`, variable access, definitions, `yield`, …) is
  lowered by calling that helper from its `compile_asmir` arm. An `LInst`
  that only carried those fields across would add nothing.

Emission is single-pass: each `LInst` is encoded as soon as it is built.
`encode_linst` is an inherent `Codegen` method defined once per arch in the
module the `compile` path includes for the active `target_arch`, so `cfg`
selects the encoder and no dynamic dispatch is involved.

---

## 2. Data model

`LInst` is `Debug` but neither `Clone` nor `PartialEq`: branch targets
carry resolved monoasm `DestLabel`s, and the `Inline` carrier wraps a
move-only generator closure. An `LInst` is built, encoded once and dropped.

### Operands

| Type | Role |
| --- | --- |
| `GP` | A physical general-purpose register (`codegen.rs`); arch-neutral names mapped per `target_arch`. |
| `VReg` | `Pinned(GP)` or `Alloc(u32)`. The front end emits only `Pinned` (`GP_ALLOC_POOL` is empty), so `VReg::phys()` is the identity. |
| `LReg` | `Gp(VReg)` or `Scratch`, the per-arch reserved scratch pointer (`rdx` on x86-64, `x9` on aarch64) for intermediate pointers that must not clobber an allocated value. |
| `FPReg` | Virtual FP register: a physical xmm / d register or a stack spill, resolved by the encoder from the frame's spill `base`. |
| `LOperand` | `Reg(VReg)` or `Imm(i64)`, an ALU / compare source the encoder folds or materializes. |
| `LMem` | A logical memory location with an **unbounded** displacement: `Slot(SlotId)` (frame slot), `LfpSlot(SlotId)` (always addressed through the LFP, so it follows a frame moved to the heap), `Field { base, disp }` (object field), `RspRel { disp }` (callee argument slot). |
| `LCond` | Signed integer condition `Eq/Ne/Lt/Le/Gt/Ge`. |

### Instructions

| Group | Variants |
| --- | --- |
| Move / memory | `Mov`, `LoadImm`, `Load`, `Store`, `StoreImm` |
| ALU / compare / branch | `Alu`, `Cmp`, `TestBit`, `Label`, `Br`, `CondBr`, `BranchTruthy`, `BranchIfNil`, `BranchIfNonzero` |
| Small inline readers | `BoolFieldToReg`, `ArrayLenFixnum`, `StringLenFixnum`, `IsNilToBool`, `NotToBool`, `BoolBinOp`, `BlockGiven` |
| GC / ivars | `WriteBarrier`, `IvarTyCheck`, `IvarUnset`, `NilIfZero` |
| Guards (carry a side-exit label) | `GuardClass`, `GuardClassIn`, `BrClassNe`, `BrClassNotIn`, `KindOfConst`, `ClassInToBool`, `GuardArrayTy`, `GuardFrozen`, `GuardConstBaseClass`, `GuardConstVersion`, `GuardCapture`, `BrIfNotCaptured`, `CheckBOP` |
| Integer arithmetic | `IntegerBinOp`, `IntegerBinOpImm`, `IntegerDouble`, `IntegerSucc`, `FixnumNeg`, `FixnumBitNot` |
| Floating point | `FprMove`, `F64ToFpr`, `FixnumToFpr`, `FprToStack`, `FprSwap`, `FloatToFpr`, `I64ToBoth`, `FloatBinOp`, `FloatUnOp`, `MathSqrt`, `FloatCmp`, `FloatCmpFlags`, `FloatCondBr`, `FprSave`, `FprRestore`, `CFunc_F_F`, `CFunc_FF_F`, `FloatRetStore`, `FloatRetLoad`, `FloatArgMove`, `LoadDynVarSpecF`, `StoreDynVarSpecF` |
| Control | `HandleError`, `InlineRet`, `MethodRet`, `ChainExit`, `SideExit { kind }`, `DeoptTrampoline` (x86-64 `deopt` builds) |
| Ordering pseudo-ops | `SelectPage`, `BindLabel`, `SourcePos` |
| Carrier | `Inline(InlineProcedure)` |

`SideExit` is the cold handler a guard branches to. Its `LSideExitKind` is
`Deopt { chain }`, `Evict`, `RecompileDeopt { reason, target, chain }`,
`Redo` / `Materialize` (the two exits of a frameless specialized callee)
or `Error { chain }`; `chain` is the number of suspended frames the handler
converts (`doc/chain_deopt.md`).

### The three side entries of `encode_linst`

Each backend's `encode_linst` matches the variants it encodes and ends in
`other => self.encode_linst_macro(other)`. Three groups do not reach the
per-arch match:

- **`encode_linst_macro`** (arch-neutral, `compile_shared.rs`): the
  ordering pseudo-ops `SelectPage` / `BindLabel`, and the four ops built in
  more than one place whose body is a shared emitter — `HandleError`,
  `InlineRet`, `MethodRet` and `ChainExit` (which records the call site's
  replay data and emits no bytes).
- **`encode_linst_inline`**: `LInst::Inline`. An inline builtin generator
  closure that still emits per-arch asm directly needs the compile context
  (`&Store`, the side-exit labels, the frame base), which the store-free
  `encode_linst` does not have.
- **`encode_linst_frame`**: `SourcePos`, which records the current code
  position in the frame's source map.

The specialized-call family (`MethodRetSpecialized`,
`BlockBreakSpecialized`, `SpecializedCall`, `SetupYieldFrame`,
`SpecializedYield`) and the array-index generators go through the same
`Inline` carrier (`lower_via_inline`): `compile_asmir` resolves their
frame-local labels first, and the closure emits the per-arch code.

---

## 3. The legalization contract

`LMem` displacements and `LOperand::Imm` values are unbounded; the encoder
legalizes them:

- **x86-64**: `[reg + disp32]` and `imm32` cover nearly everything; a
  64-bit immediate goes through `movabs`.
- **aarch64**: the encoder folds a displacement that fits
  (`ldur` / `stur` ±256, scaled `ldr` / `str`, `sub sp`) and otherwise
  materializes it into the reserved scratch registers `x9` / `x10` (the
  `a64_frame_*` / `a64_field_*` / `a64_rsp_slot_addr` helpers).

This is what lets the aarch64 backend lower every `AsmInst` without
declining a compile (`doc/arch_difference.md`).

---

## 4. General-purpose registers

LIR does not allocate GP registers itself. The fixed VM registers
(executor, globals, pc, LFP; the accumulator `r15` / `x23` is VM-tier only)
and the C-ABI / inline-builtin convention registers are pinned by the
operations that use them. Fixnum values are kept in registers by the
per-basic-block allocator in `jitgen/gp_alloc.rs`, which runs during the
AsmIR walk over `GP_ALLOC_SET` (`r8`–`r11`): integer binops and compares,
literal defs, call-result parking and slot copies read and write residents,
every other operation flushes the dirty residents to their frame homes
first, and the file is empty at every basic-block boundary. A pool
register is never a GC root, so only Fixnums live there.

---

## 5. Verification

A change to an encoder or to a `compile_asmir` arm is checked by the full
test suite on both arches (`bin/test`, `bin/test-aarch64` or the native
arm64 CI job) and, for a change meant to be byte-identical, by comparing
`--features emit-asm` dumps before and after.
