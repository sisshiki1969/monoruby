# Unified low-level IR (LIR)

Design notes and migration log for the arch-neutral, machine-level IR that sits
between `AsmIR` and the per-arch `monoasm!` / `monoasm_arm64!` byte emission.
This is **Phase-1 item ①** ("a low-level IR that describes amd64 and aarch64
uniformly").

Status: **the great majority of `AsmInst` families now lower through
`encode_linst`.** The model lives in
[`monoruby/src/codegen/jitgen/lir.rs`](../monoruby/src/codegen/jitgen/lir.rs);
the per-arch encoder is `Codegen::encode_linst`, defined once in each
`arch/<arch>/compile/…` file. As of this writing the integer + control-flow
core, all addressing modes, the GC write barrier, the common guard/check family,
fixnum arithmetic (with overflow deopt), the **whole floating-point family**,
the bounds-checked heap-ivar load/store, and the large **runtime-call macro-op**
families (array/hash/string/range construction, `defined?`, generic binops,
class/method definition, exceptions, `yield`, the method-prologue guards, …) are
all lowered through `encode_linst`, as is the method-call argument-setup family
(the dispatcher pre-resolves the store/frame-dependent values and the encoder
stays store-free). What remains in the dispatcher is the hot-path `Call`
(deferred, not excluded), the specialized inlined-frame family, and the
patch/recompile bookkeeping that is *not* byte emission — see §8.

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
`#[derive(Debug, Clone)]` — `DestLabel` is `Clone` but not `Copy`. `PartialEq`
is intentionally **not** derived: several payload types reached by the macro-op
variants (`WriteBack`, `AsmEvict`, `FnInitInfo`, …) are not `PartialEq`, and
deriving it would force a `PartialEq` cascade across unrelated types for no
benefit. The `lir.rs` unit test uses `matches!` instead of `==`.

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
| Integer arithmetic | `IntegerBinOp { …, deopt }`, `IntegerCmp`, `FixnumNeg { …, deopt }`, `FixnumBitNot` |
| Floating-point | `FprMove`, `F64ToFpr`, `FixnumToFpr`, `FprToStack`, `FprSwap`, `FloatToFpr` (deopt), `I64ToBoth`, `FloatBinOp`, `FloatUnOp`, `FloatCmp`, `FloatCmpBr`, `XmmSave`, `XmmRestore`, `CFunc_F_F`, `CFunc_FF_F` (FP ops carry the spill `base`) |
| Heap ivar (macro-op) | `LoadIVarHeap`, `StoreIVarHeap` |
| Construction (macro-op) | `CreateArray`, `NewArray`, `NewHash`, `HashInsert`, `ArrayConcat`, `NewRange`, `ConcatStr`, `ConcatRegexp`, `ToA`, `DeepCopyLit`, `ExpandArray` |
| Variables (macro-op) | `StoreConstant`, `LoadGVar`, `StoreGVar`, `LoadCVar`, `CheckCVar`, `StoreCVar`, `LoadDynVar`, `StoreDynVar`, `AliasGvar` |
| `defined?` (macro-op) | `DefinedYield`, `DefinedSuper`, `DefinedGvar`, `DefinedCvar`, `DefinedConst`, `DefinedMethod`, `DefinedIvar` |
| Dispatch helpers (macro-op) | `GenericBinOp`, `OptEqCmp`, `ArrayTEq`, `CheckKwRest`, `RestKw` |
| Definition (macro-op) | `MethodDef`, `SingletonMethodDef`, `ClassDef`, `SingletonClassDef`, `AliasMethod`, `UndefMethod` |
| Method-prologue guards (macro-op) | `GuardClassVersion`, `RecompileDeopt`, `CheckStack`, `ExecGc`, `Init`, `LoopJitRspBump` |
| Control flow / exceptions (macro-op) | `Ret`, `MethodRet`, `BlockBreak`, `Raise`, `Retry`, `Redo`, `EnsureEnd`, `Yield`, `BlockArgProxy`, `BlockArg`, `ImmediateEvict`, `Deopt`, `HandleError`, `Unreachable` |

`Lir` is a thin `Vec<LInst>` builder used where a multi-instruction sequence is
convenient.

The macro-op variants do not lower to primitives inside `encode_linst`; instead
each backend's `encode_linst` ends with `other => self.encode_linst_macro(other)`,
and the **arch-neutral** `encode_linst_macro`
([compile_shared.rs](../monoruby/src/codegen/jitgen/asmir/compile_shared.rs))
delegates each macro-op to the existing per-arch `emit_*` helper. This keeps the
delegation in one place (rather than duplicating it in both backends) while
still funnelling *all* emission through `encode_linst`. See §5.

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
  per-arch helper that stays (the tag-test / page-split / tagged-arith / C-call
  sequence is irreducibly per-arch). Two sub-cases:
  - *decomposed-style delegation* (guards, `IntegerBinOp`): the per-arch
    `encode_linst` matches the variant directly and calls the substantive helper
    (`guard_class` / `a64_guard_class`, `integer_binop` / `a64_integer_binop`, …);
    the thin `emit_*` wrapper is deleted.
  - *`encode_linst_macro`-style delegation* (the large runtime-call families):
    the variant falls through each backend's `other =>` arm into the
    **arch-neutral** `encode_linst_macro`, which calls the per-arch `emit_*`
    helper. The `emit_*` helper is retained verbatim, so the migration is a pure
    routing change — the dispatcher arm now builds an `LInst` and hands it to
    `encode_linst` instead of calling `emit_*` directly. This is how the
    construction / variable / `defined?` / definition / control-flow families
    were migrated (batches A and B).

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
| 3-F | FP swap / `FloatToFpr` (deopt) / `I64ToBoth` | |
| 3-G | FP arithmetic & compare (`FloatBinOp` / `FloatUnOp` / `FloatCmp` / `FloatCmpBr`) | NaN-correct conditions |
| 3-H | FP C-calls (`CFunc_F_F` / `CFunc_FF_F`) + `XmmSave` / `XmmRestore` | completes the FP family |
| A | bounds-checked heap-ivar load/store (`LoadIVarHeap` / `StoreIVarHeap`) | first macro-op via `encode_linst_macro` |
| B1 | variable + construction macro-ops (g/c/dyn-var, array/hash/range/str, `to_a`, `defined?`, generic binop, alias/undef, …) | bulk macro-op routing |
| B2 | remaining construction / `defined?` / dispatch-helper macro-ops | |
| B3 | control-flow / exception / definition macro-ops (`Ret`, `MethodRet`, `Raise`, `Retry`, `Redo`, `EnsureEnd`, `Yield`, `MethodDef`, `Init`, `CheckStack`, `ExecGc`, `IntegerCmp`, `BlockArg`, …) | `Raise`/`Retry`/`Redo`/`EnsureEnd` carry `loop_jit_spill_bytes` for the aarch64 loop-JIT sp unwind |
| B4 | class-def + method-prologue guards (`ClassDef`, `SingletonClassDef`, `GuardClassVersion`, `RecompileDeopt`) | last non-store/non-frame arms |
| B5 | elementary moves (`RegMove`/`RegToAcc`/`AccToStack`/`RegToStack`/`StackToReg`/`LitToReg`/`LitToStack`/`RegAdd`/`RegSub`) | dispatcher lowers straight to `LInst`; the thin `emit_*` move wrappers deleted from both backends |
| B6 | method-call / argument-setup (`SetupMethodFrame`, `SetArguments`, `SetArgumentsForwardedHelper`, `Preparation`, `OptCase`) | dispatcher pre-resolves the store/frame-dependent values (offset, block info, heap-ivar length, jump-table labels) into the `LInst`; per-arch helpers made store-free. `Call` deliberately left out (see §8) |

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

The arms still handled **directly in the `compile_asmir` dispatcher** (not routed
through `encode_linst`) fall into a few groups.

- **Store-/frame-dependent values are pre-resolved, not a blocker.** The
  method-call / argument-setup family (`SetupMethodFrame`, `SetArguments`,
  `SetArgumentsForwardedHelper`, `Preparation`, `OptCase`) was migrated in B6:
  the dispatcher — which holds `&Store` / `&mut AsmInfo` — resolves the
  store/frame-dependent values (callee scratch `offset`, block `(fid, arg)`,
  heap-ivar table length, jump-table `DestLabel`s) and carries them in the
  `LInst`, so the per-arch helper stays store-free. The same recipe applies to
  the remaining `Call`, which is left for later only because it pre-resolves
  more fields (codeptr / pc / jit-entry / iseq-ness from `store[callee_fid]`)
  and sits on the hottest path; it is *deferred, not excluded*.
- **Specialized inlined-frame family.** `MethodRetSpecialized`,
  `BlockBreakSpecialized`, `SpecializedCall`, `SetupYieldFrame`,
  `SpecializedYield`, and `Inline` lower an inlined callee/block frame; they
  resolve frame-local labels and patch points and are dispatched to a per-arch
  method of the same name (`arch/<arch>/compile/…`). Same rationale.
The elementary register/stack moves (`RegMove`, `RegToAcc`, `AccToStack`,
`RegToStack`, `StackToReg`, `LitToReg`, `LitToStack`, `RegAdd`, `RegSub`) are
now lowered to `LInst::{Mov,Load,Store,LoadImm,Alu}` straight in the dispatcher;
the thin `emit_reg_move` / `emit_reg_to_stack` / `emit_stack_to_reg` /
`emit_lit_to_reg` / `emit_lit_to_stack` / `emit_reg_add` / `emit_reg_sub`
wrappers (which only forwarded to `encode_linst`) were removed from both
backends.

Beyond the dispatcher, the **patch / recompile machinery** (return-address
eviction, in-place recompile) is the x86/aarch64 *non-coverage* asymmetry
described in `doc/arch_difference.md` §4. This is **explicitly out of scope** for
LIR: it does not *emit bytes* — it records code-position metadata
(`return_addr_table` patch points, which emit zero bytes), mutates `Codegen`
tables, and triggers recompilation. Forcing it into `LInst` would add zero-byte
"instructions" that only touch side tables, degrading the machine-op
abstraction. The clean invariant is therefore:

> `encode_linst` is the single seam for byte **emission**; code-position
> metadata / patch / recompile bookkeeping is *not* emission and stays in the
> dispatcher / `Codegen`.

None of this invalidates the model: `encode_linst` (with `encode_linst_macro`)
is the single seam for emission, and the residual dispatcher arms either are
bookkeeping rather than emission, or are deferred (`Call`) by effort rather than
by a modelling limit.
