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
all lowered through `encode_linst`, as is the **entire** method-call family —
argument setup *and* the call itself (the dispatcher pre-resolves the
store/frame-dependent values and the encoder stays store-free) — and the cold
**deopt side-exit handler** blocks. Even the `AsmInst::Inline` builtin escape
hatch now lowers to a carrier `LInst::Inline` (dispatched via the
context-carrying `encode_linst_inline`), so **every** `AsmInst` reaching the
dispatcher lowers to `LInst`. What remains outside `encode_linst` is the
specialized inlined-frame family and the zero-byte patch/recompile bookkeeping
that is *not* byte emission — see §8.

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
| Floating-point | `FprMove`, `F64ToFpr`, `FixnumToFpr`, `FprToStack`, `FprSwap`, `FloatToFpr` (deopt), `I64ToBoth`, `FloatBinOp`, `FloatUnOp`, `FloatCmp`, `FloatCmpBr`, `FprSave`, `FprRestore`, `CFunc_F_F`, `CFunc_FF_F` (FP ops carry the spill `base`) |
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
| 3-H | FP C-calls (`CFunc_F_F` / `CFunc_FF_F`) + `FprSave` / `FprRestore` | completes the FP family |
| A | bounds-checked heap-ivar load/store (`LoadIVarHeap` / `StoreIVarHeap`) | first macro-op via `encode_linst_macro` |
| B1 | variable + construction macro-ops (g/c/dyn-var, array/hash/range/str, `to_a`, `defined?`, generic binop, alias/undef, …) | bulk macro-op routing |
| B2 | remaining construction / `defined?` / dispatch-helper macro-ops | |
| B3 | control-flow / exception / definition macro-ops (`Ret`, `MethodRet`, `Raise`, `Retry`, `Redo`, `EnsureEnd`, `Yield`, `MethodDef`, `Init`, `CheckStack`, `ExecGc`, `IntegerCmp`, `BlockArg`, …) | `Raise`/`Retry`/`Redo`/`EnsureEnd` carry `loop_jit_spill_bytes` for the aarch64 loop-JIT sp unwind |
| B4 | class-def + method-prologue guards (`ClassDef`, `SingletonClassDef`, `GuardClassVersion`, `RecompileDeopt`) | last non-store/non-frame arms |
| B5 | elementary moves (`RegMove`/`RegToAcc`/`AccToStack`/`RegToStack`/`StackToReg`/`LitToReg`/`LitToStack`/`RegAdd`/`RegSub`) | dispatcher lowers straight to `LInst`; the thin `emit_*` move wrappers deleted from both backends |
| B6 | method-call / argument-setup (`SetupMethodFrame`, `SetArguments`, `SetArgumentsForwardedHelper`, `Preparation`, `OptCase`) | dispatcher pre-resolves the store/frame-dependent values (offset, block info, heap-ivar length, jump-table labels) into the `LInst`; per-arch helpers made store-free |
| B7 | the call itself (`Call`) | dispatcher pre-resolves `codeptr` / `is_iseq` / callee+call-site pcs / x86 JIT entry (`get_jit_entry`); `do_call`/`a64_do_call` made store-free. x86 still records the return-address patch point inside the call's encoder (it is captured at the emission point); aarch64 ignores the x86-only fields |
| B8 | cold side-exit / deopt handlers (`LInst::SideExit { kind }`) | both `gen_asm` side-exit loops build an `LInst::SideExit` and route through `encode_linst`; the per-arch encoder dispatches on `LSideExitKind` (Deopt / Evict / RecompileDeopt / Error) to the existing handler emitters (x86 `gen_*_with_label` / `gen_handle_error`; aarch64 `a64_gen_deopt` / `a64_gen_handle_error`, which collapse Evict/RecompileDeopt to a plain deopt) |

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

First, what is **no longer** a blocker. The **whole method-call family** is now
migrated — argument setup (`SetupMethodFrame`, `SetArguments`,
`SetArgumentsForwardedHelper`, `Preparation`, `OptCase`; B6) **and** the call
itself (`Call`; B7) lower through `encode_linst`. The dispatcher — which holds
`&Store` / `&mut AsmInfo` — resolves every store/frame-dependent value (callee
scratch `offset`, block `(fid, arg)`, heap-ivar table length, jump-table
`DestLabel`s, and for `Call` the `codeptr` / `is_iseq` / callee & call-site pcs /
x86 JIT entry) and carries them in the `LInst`, so the per-arch helpers
(`do_call`/`a64_do_call`, …) are store-free. `Call`'s encoder also records the
return-address patch point, which is captured *at* the emission point and so
belongs with the call's emission. (The elementary register/stack moves were
likewise inlined into the dispatcher in B5, deleting their forwarding wrappers.)

The **cold side-exit / deopt handler blocks** that guards branch *to* — laid
out by `gen_asm` *before* the main instruction loop, not by an `AsmInst` — are
also byte emission, and as of B8 they route through `encode_linst` via
`LInst::SideExit` (write-back + sp-unwind + VM-resume / unwind). Both arches'
`gen_asm` side-exit loops now build an `LInst::SideExit` and the per-arch
encoder dispatches on `LSideExitKind`.

The remaining things handled **directly** (not through `encode_linst`):

- **Specialized inlined-frame family.** `MethodRetSpecialized`,
  `BlockBreakSpecialized`, `SpecializedCall`, `SetupYieldFrame`, and
  `SpecializedYield` lower an inlined callee/block frame; they resolve
  frame-local labels and patch points and are dispatched to a per-arch method of
  the same name (`arch/<arch>/compile/…`). (`AsmInst::Inline` is no longer in
  this group — it lowers to `LInst::Inline`; see below.)
- **The `AsmInst::Inline` escape hatch.** Most builtin inline generators (e.g.
  `emit_math_sqrt`) still run a closure that emits arch asm directly via `gen`.
  As of the AsmIR→LIR consolidation, `AsmInst::Inline` *does* lower to a
  carrier LIR op — `LInst::Inline(InlineProcedure)` — so **every** `AsmInst`
  reaching the dispatcher now lowers to `LInst`. `LInst::Inline` is the one LIR
  op whose emit is *not* store-free: its wrapped closure needs the compile
  context (`&Store`, `&SideExitLabels`, frame `base`), so it is dispatched at
  the lowering boundary via `encode_linst_inline` rather than through the
  store-free `encode_linst`. (If it ever reached `encode_linst`/`_macro` it hits
  the `unreachable!` fallthrough.) This is a transitional carrier: migrating the
  closures onto typed, arch-neutral LIR ops — so the variant can eventually go
  away — is **goal 2** of the long-term plan ("express the inline-builtin codegen
  once, arch-neutrally"). The migratable category is the *pure* generators —
  property/field readers and trivial C-function wrappers, whose codegen is one
  existing LIR op with no arch-specific control flow:
  - **64-bit field readers** → `AsmIr::load_field_to_reg` →
    `AsmInst::LoadFieldToReg` → existing `LInst::Load { Field }` (no new op,
    byte-identical on both arches). Done: `Range#begin/end` (hand-written
    `emit_range_begin/end` deleted) and `ArithmeticSequence#begin/#end/#step`
    (via the shared `inline_field_load` helper; `emit_load_value_field` deleted
    from both backends).
  - **Bool field readers** → `AsmIr::bool_field_to_reg` →
    `AsmInst::BoolFieldToReg` → `LInst::BoolFieldToReg` (a small macro-op:
    32-bit load + `shl 3` + `or FALSE_VALUE`, deduping the two byte-identical
    `emit_*_exclude_end` emitters into one encode arm per arch). Done:
    `Range#exclude_end?`, `ArithmeticSequence#exclude_end?`.
  - **Container length** → `AsmIr::array_len_fixnum` / `string_len_fixnum` →
    `AsmInst::ArrayLenFixnum` / `StringLenFixnum` → the matching `LInst` (a
    macro-op: load inline capa + conditional-select the heap length when capa
    exceeds the inline cap + fixnum-tag; the conditional select is the only
    per-arch part, x86 `cmov` / aarch64 `csel`). Done: `Array#size`,
    `String#bytesize` — one op each (differing only in the inline-cap constant,
    `ARRAY_INLINE_CAPA` vs `STRING_INLINE_CAP`), replacing the four
    `emit_array_size`/`emit_string_bytesize` emitters.
  - **Fixnum → float** (`Integer#to_f`) → the *existing* `AsmIr::fixnum2fpr` →
    `LInst::FixnumToFpr` op (untag + `cvtsi2sd`/`scvtf` straight into the result
    fpr). No new primitive; deletes `emit_int_to_float` from both backends (and
    drops a redundant `xmm0` round-trip the old emitter always did).
  - **Bool predicates** (`Object#nil?`, `BasicObject#!`) → `AsmIr::is_nil_to_bool`
    / `not_to_bool` → `LInst::IsNilToBool` / `NotToBool` (a macro-op: compare +
    conditional-select TRUE/FALSE; the select is the per-arch part, `cmov`/`csel`).
    Replaces the `emit_kernel_nil` / `emit_object_not` emitters.
  - **C-function wrappers** (e.g. `Math.sin/cos/atan2`, `Float#**`) are *already*
    arch-neutral: they route through the typed `AsmInst::CFunc_F_F` /
    `CFunc_FF_F` (→ existing `LInst::CFunc_*`), not the closure escape hatch.

  What stays a closure is the genuinely arch-specific shapes — generators with
  control flow whose condition-code mapping differs per arch (`Math.sqrt`'s
  NaN/negative guard: x86 `ucomisd`+`jp`/`jb`, aarch64 `fcmp`+`fsqrt`), object
  allocation, `send`, etc. Those need a few new FP/branch LIR primitives first.
- **Pure patch / recompile *bookkeeping*** — the x86/aarch64 *non-coverage*
  asymmetry of `doc/arch_difference.md` §4. The deopt *handler emission* now
  goes through LIR (above); what stays out is the part that **emits no bytes**:
  `ImmediateEvict` records a `return_addr_table` patch point (zero bytes), and
  the actual return-address overwrite happens at BOP-redefinition time, not at
  compile time. The clean invariant is therefore:

> `encode_linst` is the single seam for byte **emission**; zero-byte
> code-position metadata / runtime patch bookkeeping is *not* emission and stays
> in the dispatcher / `Codegen`.

None of this invalidates the model: `encode_linst` (with `encode_linst_macro`,
plus the context-carrying `encode_linst_inline` for the one `LInst::Inline`
op) is the single seam for byte emission. The only arms still handled directly
in the dispatcher are the specialized inlined-frame family (which resolve
frame-local labels / patch points) and the patch/recompile bookkeeping, which is
*not* emission by the invariant above.

---

## §9 The two-phase pipeline: whole-function buffering + virtual GP + physical allocation

Everything above is the **single-pass** seam: `compile_asmir` lowers each
`AsmInst` to one or more `LInst`s and *emits them immediately* through
`encode_linst`. That gave us one byte-emission description per op, but it cannot
host a register allocator: allocation needs the **whole function's** instruction
stream in hand to compute liveness, so an op cannot be encoded the instant it is
produced.

The next architectural step (the `AsmIr → LIR` pipeline) splits emission into two
phases while keeping `AsmIr` and `LIR` as **separate layers** (collapsing them
would just reconstruct `AsmIr`):

```text
AsmIr (AsmInst, frame-INDEPENDENT, FP already virtual, GP physical)
  │  lower (resolve frame size / labels / patch points)
  ▼
LIR  = Vec<LInst>   ← whole compiled unit, offsets baked in, GP *virtual*
  │  arch-dependent physical-register allocation pass (GP virtual → physical)
  ▼
encode (drain → encode_linst → bytes)
```

- **LIR is barely-arch-independent and offset-baked.** Frame base, slot
  displacements, field offsets, and labels are all resolved when `AsmIr` lowers
  to `LIR` (`AsmInst` is frame-independent; `LInst` already carries `base`). What
  `LIR` does *not* yet bake is the physical register assignment.
- **Registers are virtual in LIR.** FP registers already are (`FPReg` =
  phys-or-spill, resolved by `PhysMap` at encode time — §25/§27). GP registers
  are *not*: today `LInst` names concrete `GP`s (`R15` = accumulator, `R12` =
  globals, `R14` = LFP, plus scratch temporaries). The pipeline introduces a
  **virtual GP** (`VReg`) for the *allocatable* GP uses; the fixed globals
  (acc / pc / lfp / globals / executor — see `CLAUDE.md`) stay pinned and are
  **not** virtualized. Only the temporary/scratch GP traffic becomes virtual and
  is assigned in the post-LIR phase.

### Blockers (from the current driver, `asmir.rs::compile` ~2293–2400)

The single-pass driver interleaves byte emission with **ordering operations** that
are not themselves `LInst`s emitted through `encode_linst`:

1. `self.jit.label()` / `bind_label(..)` — label creation and binding.
2. `self.jit.select_page(1)` — cold/hot page selection (x86 lays side-exit
   handlers on the cold page).
3. `frame.sourcemap.push((i, pos))` — source-position records keyed on the
   *current* code position.
4. Patch-point / return-address-table bookkeeping (`doc/arch_difference.md` §4),
   which emits zero bytes but is position-sensitive.

If LIR buffered only the `encode_linst` calls and left (1)–(4) inline, replaying
the buffer later would desync every label, page boundary, and source mapping.
Therefore the buffer must model these as **ordering pseudo-ops** in the same
`Vec<LInst>` (e.g. `LInst::BindLabel`, `LInst::SelectPage`, `LInst::SourcePos`),
so the *entire* emission stream — bytes **and** position metadata — is one
ordered sequence the allocator can walk and the drainer can replay faithfully.

### Staged increments

- **9a — Make the emission stream fully reified.** Add the ordering pseudo-ops so
  `compile_asmir` + the side-exit loop produce a single `Vec<LInst>` covering
  *all* of byte emission, label binding, page selection, and sourcemap. Verify
  **byte-identical** output by draining the buffer immediately (no allocation
  pass yet) — a pure refactor, gated/shadowed like the §24 placement harness.
- **9b — Introduce `VReg` (virtual GP) with an identity map.** Replace the
  *allocatable* GP operands with `VReg`, lowered through a trivial
  virtual→physical map that reproduces today's assignment. Still byte-identical;
  this only changes the *type* carried, establishing the seam.
- **9c — Carry liveness / loop metadata into LIR.** Per §27.1, re-deriving
  liveness after LIR-flatten cost 2.5×, so the allocator must consume the
  metadata the ① fixpoint already computed (loop-carried sets, etc.) rather than
  recompute it. Thread it onto the buffer.
- **9d — The arch-dependent physical-allocation pass.** Walk the buffered LIR,
  assign physical GPs to `VReg`s (spilling to frame slots under pressure, exactly
  as `FPReg` does for FP), then drain → encode. This is the first point the
  output may legitimately differ from today's bytes; like `phys-loop-aware`
  (§42), it is a perf experiment gated behind a flag + the M1 A/B bench gate, and
  the shadow digest becomes a delta meter, not an equality check.

9a/9b are byte-identical refactors (safe to land once verified); 9c/9d are the
substantive allocator work and stay behind a flag until the bench gate clears.
