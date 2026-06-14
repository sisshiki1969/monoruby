# Unified low-level IR (LIR)

Design notes and migration log for the arch-neutral, machine-level IR that sits
between `AsmIR` and the per-arch `monoasm!` / `monoasm_arm64!` byte emission.
This is **Phase-1 item ①** ("a low-level IR that describes amd64 and aarch64
uniformly").

Status: **actively migrating `AsmInst` families onto LIR, family by family.**
The model lives in
[`monoruby/src/codegen/jitgen/lir.rs`](../monoruby/src/codegen/jitgen/lir.rs);
the per-arch encoder is `Codegen::encode_linst`, defined once in each
`arch/<arch>/compile/…` file. As of this writing the integer + control-flow
core, all addressing modes, the GC write barrier, the common guard/check family,
fixnum arithmetic (with overflow deopt), and the first floating-point family are
all lowered through `encode_linst`.

---

## 1. Motivation

The original justification ("close the aarch64 *bail* gap") is **gone**: the
full aarch64 port (`#704`) made aarch64 lower every `AsmInst` (see
`doc/arch_difference.md`). The remaining — and real — motivation is
**description unification**:

- The AsmIR → machine-code step used to be two parallel sets of `emit_*`
  primitives (x86 `arch/x86_64/compile/*.rs`, aarch64 `arch/aarch64/compile.rs`).
  LIR makes `encode_linst` the **single seam** through which all migrated
  families emit code.
- LIR is also the concrete **code-generation target** the future
  interpreter/JIT DSL (Phase-1 item ③, "derive interpreter + JIT from one
  description") lowers to. A hand-written, test-validated LIR gives ③ a
  well-defined target and a correctness oracle.

---

## 2. Where LIR sits

```text
TraceIR
  → AsmIR (AsmInst, arch-neutral, register-allocated)
  → [compile_asmir dispatcher]                         jitgen/asmir/compile_shared.rs
  → LIR (LInst, arch-neutral machine ops)              jitgen/lir.rs            ← this layer
  → [per-arch Codegen::encode_linst]                   arch/<arch>/compile/…
  → monoasm! / monoasm_arm64!
  → bytes
```

`AsmIR` stays the register-allocated, arch-neutral *front-end* output (one
`AsmInst` per semantic operation). The arch-neutral dispatcher
`compile_asmir` ([compile_shared.rs](../monoruby/src/codegen/jitgen/asmir/compile_shared.rs))
lowers each migrated `AsmInst` into one or more `LInst`s and feeds them to
`encode_linst`; not-yet-migrated families still call their per-arch `emit_*`
directly from the dispatcher.

`encode_linst` is an inherent `Codegen` method, **defined once per arch** in the
file that the `compile` module includes for the active `target_arch`. Because
only one compiles per target, no trait/dynamic dispatch is needed — `cfg`
selects the right encoder.

---

## 3. Data model

Defined in `lir.rs`. Branch and side-exit targets carry a **resolved monoasm
`DestLabel`** (the encoder runs after label resolution), so `LInst` is
`#[derive(Debug, Clone, PartialEq)]` — `DestLabel` is `Clone` but not `Copy`.

### Operands

| Type        | Role |
| ----------- | ---- |
| `GP`        | General-purpose register. Reused from `codegen.rs`; already arch-neutral. |
| `LReg`      | `Gp(GP)` or `Scratch` — a register that may be the per-arch reserved **scratch pointer** (`rdx` on x86, `x9` on aarch64). For intermediate pointers (heap var-table derefs) that must not clobber an allocated value; aarch64's x9 is outside `GP`'s allocatable map, so it needs its own kind. `From<GP>`. |
| `FPReg`     | Virtual FP register (physical xmm/d-reg **or** a stack spill); the encoder resolves the spill via `FPReg::loc(base)`, so FP `LInst`s carry the frame's spill `base`. |
| `LOperand`  | `Reg(GP)` or `Imm(i64)` — an ALU/compare source the encoder folds or materializes. |
| `LMem`      | A *logical* memory location with **unbounded** displacement: `Slot(SlotId)` (LFP-relative, negative), `Field { base: LReg, disp }` (object field / scratch-relative, positive), `RspRel { disp }` (callee-frame arg slot). The encoder legalizes the displacement per arch. |
| `LCond`     | Signed integer branch condition (`Eq/Ne/Lt/Le/Gt/Ge`), with `from_int_cmp` / `invert`. |
| `LAluOp`    | `Add/Sub/Mul/And/Or/Xor/Shl/Sar`, with `from_binop`. |

### Instructions (`LInst`)

| Group | Variants |
| ----- | -------- |
| Move / immediate | `Mov`, `LoadImm` |
| Memory | `Load`, `Store`, `StoreImm` (over `LMem::Slot` / `Field` / `RspRel`) |
| ALU / compare | `Alu`, `Cmp` |
| Branch | `Label`, `Br`, `CondBr { cond: LCond, … }`, `BranchTruthy { negate }`, `BranchIfNil`, `BranchIfNonzero` |
| GC / nil | `WriteBarrier { parent, value }`, `NilIfZero { reg }` |
| Guards (carry a side-exit `deopt`) | `GuardClass`, `GuardArrayTy`, `GuardFrozen`, `GuardConstBaseClass`, `GuardConstVersion`, `GuardCapture`, `CheckBOP` |
| Arithmetic w/ deopt | `IntegerBinOp { kind, mode, lhs, rhs, deopt }` |
| Floating-point | `FprMove`, `F64ToFpr`, `FixnumToFpr`, `FprToStack` (carry the spill `base`) |

`Lir` is a thin `Vec<LInst>` builder used where a multi-instruction sequence is
convenient.

### The legalization contract (the core idea)

`LMem` displacements and `LOperand::Imm` values are **unbounded**; the per-arch
encoder legalizes them:

- **x86-64**: `[reg + disp32]` / `imm32` cover essentially everything — mostly a
  no-op.
- **aarch64**: fixed-width encodings allow only small immediates, so the encoder
  folds a fitting displacement into the instruction (`ldur`/`stur` ±256, scaled
  `ldr`/`str`, `sub sp`) and otherwise materializes the offset into reserved
  scratch `x9`/`x10` (the `a64_frame_*` / `a64_field_*` / `a64_rsp_slot_addr`
  helpers).

---

## 4. The two model extensions (Stage 3)

Stages 2-A…2-J were *byte-for-byte family ports* that fit the data model as-is.
Stage 3 extended the model so the harder families could be expressed:

1. **Scratch register operand (`LReg::Scratch`).** Lets the LIR express
   intermediate pointers that map to a different physical register per arch
   (x86 `rdx`, aarch64 `x9`). Without it, an abstract `GP` would either pick a
   register that aarch64 keeps allocatable (clobber hazard) or have no name for
   aarch64's reserved scratch. First user: the self heap-ivar store
   (`Load{Scratch ← [rdi+VAR]} ; Load{Scratch ← [Scratch+MONOVEC_PTR]} ;
   Store{[Scratch+idx*8] ← src} ; WriteBarrier`).

2. **Deopt side-exits.** Guard ops carry the *resolved* side-exit `DestLabel`
   they fall through to, making deopt a first-class LIR concept. The
   arch-neutral dispatcher resolves `labels[deopt]` and builds the guard; the
   encoder branches to it. This pattern carries the overflow exit of
   `IntegerBinOp` and every guard/check op.

---

## 5. Encoding styles: decomposition vs. macro-op

Migrated families use one of two encoder styles, both **byte-identical** to the
prior code:

- **Decomposition** — the LIR sequence is built from reusable primitives and the
  per-arch lowering lives in `encode_linst`. Used by the move/memory/ALU/branch
  core, the field load/store + write-barrier, and the FP transfer/convert ops
  (whose spill-aware bodies were *moved* from `emit_*` into the encoder arms,
  deleting those `emit_*`).
- **Macro-op delegation** — a single `LInst` whose encoder calls an existing
  per-arch helper that stays (the tag-test / page-split / tagged-arith logic is
  irreducibly per-arch). Used by the guards (delegate to `guard_class`,
  `a64_guard_class`, `guard_capture`, …) and `IntegerBinOp` (delegate to
  `integer_binop` / `a64_integer_binop`, which keep the x86 cold-page overflow
  handler). The thin `emit_*` wrappers are deleted; the substantive helper stays.

---

## 6. Migration log

Each stage routes one family through `encode_linst`, commits, and is verified by
the full `cargo test --lib` suite (see §7). All stages are byte-identical
(modulo eliding a redundant self-`mov` when a value is already in the
accumulator).

| Stage | Family | Notes |
| ----- | ------ | ----- |
| 0 / 1 | LIR data model + this doc | scaffolding |
| 2-A | `Mov` (`emit_reg_move`) | first `encode_linst` user |
| 2-B | slot memory (`Load`/`Store`/`LoadImm`/`StoreImm` over `Slot`) | frame legalization |
| 2-C | reg-imm ALU (`emit_reg_add/sub` → `Alu`) | `LAluOp` / `LOperand` |
| 2-D | integer compare-branch (`Cmp` + `CondBr`) | `LCond`; built in shared dispatcher |
| 2-E | conditional branches (`BranchTruthy` / `BranchIfNil` / `BranchIfNonzero`) | |
| 2-F | inline struct-slot load (`Load` over `LMem::Field`) | first field-offset legalization |
| 2-G | inline ivar/struct stores (`Store{Field}` + `WriteBarrier`) | introduces `WriteBarrier` |
| 2-H | inline ivar load (`Load{Field}` + `NilIfZero`) | introduces `NilIfZero` |
| 2-I | heap struct-slot load/store | composed from existing ops — no new op |
| 2-J | rsp-relative arg stores (`LMem::RspRel`) | completes the addressing modes |
| 3-A | **scratch operand** + self heap-ivar store | model extension ① |
| 3-B | **deopt model** + class / array-ty / frozen guards | model extension ② |
| 3-C | const-base-class / const-version / capture / BOP guards | |
| 3-D | fixnum `IntegerBinOp` (overflow deopt) | hottest arithmetic path |
| 3-E | FP transfer/convert (`FprMove` / `F64ToFpr` / `FixnumToFpr` / `FprToStack`) | first FP family; real decomposition |

---

## 7. Verification

Because the migrations are pure refactors, the safety net is **behaviour
preservation under a correct reference Ruby**:

- The harness compares JIT output against the system `ruby`, and monoruby targets
  **CRuby 4.0+**. With an older Ruby (e.g. 3.3) ~33 `--lib` tests fail on format
  differences (3.4 `Hash#inspect`, etc.) — *unrelated to LIR*. Every stage here
  is verified under **CRuby 4.0.5**, where the baseline is **1702 passed / 0
  failed**; each migration keeps it at 1702 / 0 with no new warnings.
- Each family migration is local to its `emit_*` / dispatcher arm and is
  independently revertible.

> Building CRuby 4.x from source in a restricted-network sandbox: the official
> tarball host (`cache.ruby-lang.org`) may be blocked while GitHub is reachable.
> Fetch the git tag source archive from `codeload.github.com`, install `gperf`
> (needed to generate `lex.c`), empty `gems/bundled_gems` to skip the
> bundled-gem download (or set `SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt`
> so the bundled-gem fetch trusts the egress proxy CA), then
> `autogen → configure → make → make install`.

---

## 8. Remaining work

Families still lowered directly via `emit_*` (not yet through `encode_linst`):

- **Floating point (rest):** `FprSwap`, `FloatToFpr` (deopt), `FloatBinOp` /
  `FloatUnOp` / `FloatCmp` / `FloatCmpBr` (NaN-correct conditions), `I64ToBoth`,
  the FP C-calls `CFunc_F_F` / `CFunc_FF_F`, and `XmmSave` / `XmmRestore`.
- **Bounds-checked heap ivar** load/store (non-`self`): needs a bounds-check →
  deopt/nil branch on top of the scratch model.
- **Runtime-call macro-ops:** `NewArray` / `NewHash` / `ConcatStr` / `defined?`
  family / `GenericBinOp` / class & method definition / the method-call prologue
  (`SetupMethodFrame`, `SetArguments`, `Call`, `Init`) / exceptions / `Yield` /
  the specialized inlined-frame family. These are large C-call sequences; most
  would migrate as macro-ops carrying the relevant labels.
- **Patch / recompile machinery** (return-address eviction, in-place recompile)
  — the x86/aarch64 *non-coverage* asymmetry from `doc/arch_difference.md` §4;
  the hardest to model.

These extend, but do not invalidate, the model: each needs either a macro-op
(carrying its labels) or, where the per-arch shape genuinely differs (FP spill,
NaN compares, page-split deopt), a dedicated op whose encoder reproduces the
arch sequence byte-for-byte.
