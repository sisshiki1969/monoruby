# x86-64 / aarch64 JIT backend differences

A survey of how the two JIT machine-code backends differ today, focused on
**AsmInst coverage** and **lowering logic**. It is current as of the full
aarch64 port (`#704`) and the chunked-literal frame-size fix (`#709`).

- x86-64 backend: `monoruby/src/codegen/arch/x86_64/` — `compile/` (split into
  `mod.rs`, `binary_op.rs`, `method_call.rs`, `variables.rs`, `index.rs`,
  `builtin.rs`, `defined.rs`, `definition.rs`, `constants.rs`,
  `init_method.rs`) + `guard.rs`.
- aarch64 backend: `monoruby/src/codegen/arch/aarch64/` — `compile.rs`
  (one ~4.8 k-line file) + `guard.rs`.
- Shared front-end + dispatcher: `monoruby/src/codegen/jitgen/` (TraceIR →
  AsmIR) and `jitgen/asmir/compile_shared.rs` (the arch-neutral AsmInst
  lowering dispatcher).

> **History.** An earlier revision of this document (pre-`#704`) described
> aarch64 as a *streaming port that bails to the VM* on any instruction shape it
> could not lower, and catalogued ~two dozen bail sites. **That is no longer
> true.** As of `#704` aarch64 lowers every `AsmInst` and every side exit, so it
> never bails out of JIT compilation. The sections below describe the current,
> bail-free state; §4 covers the asymmetries that *do* remain (recompilation
> strategy and eviction patching — not coverage).

---

## 1. The big picture: one front-end, two backends, coverage-symmetric

Both backends consume the **same** arch-neutral `AsmIR` produced by `jitgen`
(TraceIR → register-allocated AsmIR). They diverge only at the final
AsmIR → machine-code step, driven by a single shared dispatcher,
`Codegen::compile_asmir`
([compile_shared.rs:25](../monoruby/src/codegen/jitgen/asmir/compile_shared.rs#L25)),
which lowers each `AsmInst` by one of two routes:

1. **Shared arm** — the `match` in `compile_asmir` handles the instruction
   structurally and calls a tiny per-arch **emission primitive**
   (`emit_reg_move`, `emit_reg_to_stack`, `emit_guard_class`,
   `emit_integer_binop`, …). Only the emitted bytes differ per arch.
2. **Per-arch arm** — the `other =>` fallthrough calls `compile_asmir_arch`,
   the backend-private match
   ([x86_64/compile/mod.rs:23](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L23),
   [aarch64/compile.rs:4730](../monoruby/src/codegen/arch/aarch64/compile.rs#L4730)).
   On both arches this handles only the *same three* specialized inlined-frame
   variants (`GuardClassVersionSpecialized`, `RecompileDeoptSpecialized`,
   `SetArgumentsForwarded`); everything else is handled by the shared arm.

### The `bool` return is now vestigial

Every emission primitive (and `compile_asmir` itself) returns a `bool`. In the
pre-`#704` port this was the aarch64 "not-yet-ported / out-of-range → bail to
the VM" signal. **Today both backends always return `true`:**

- x86-64 is the original fully-featured reference backend; it never declines.
- aarch64 now lowers everything too — large frame/field/sp offsets are
  materialized through scratch registers rather than bailing, and the one shape
  that needed unported caller-relative codegen (the `...`-forwarding *deferral*)
  is disabled upstream in `forward_rest_deferral` instead of bailing in the
  backend. See the header comment at
  [aarch64/compile.rs:1](../monoruby/src/codegen/arch/aarch64/compile.rs#L1):
  *"Every `AsmInst` and side exit is lowered … aarch64 never bails out of JIT
  compilation; `compile_asmir`'s `bool` is vestigial."*

There is no `return false` anywhere in the aarch64 lowering (`compile.rs`,
`guard.rs`). The driver chain (`gen_asm` / `gen_machine_code` / `jit_compile`)
no longer acts on the result either; the `bool` is kept only because flipping
~150 signatures to `()` is pure churn.

---

## 2. AsmInst coverage — full on both arches

The large majority of `AsmInst` variants are dispatched through the shared
`compile_asmir` match and lowered by per-arch emit primitives. Structurally
identical families covered on both arches include:

- Register / stack moves: `RegMove`, `RegToAcc`, `AccToStack`, `RegToStack`,
  `StackToReg`, `LitToReg`, `LitToStack`.
- Control flow: `CondBr`, `NilBr`, `CheckLocal`, `OptCase`, `Deopt`,
  `HandleError`, `Ret`, `MethodRet`, `BlockBreak`.
- Guards: `GuardClass`, `GuardClassVersion`, `GuardConstBaseClass`,
  `GuardConstVersion`, `GuardArrayTy`, `GuardFrozen`, `GuardCapture`,
  `CheckBOP`, `CheckStack`, `ExecGc`.
- Arithmetic: `IntegerBinOp`, `IntegerCmp`, `IntegerCmpBr`, `FloatBinOp`,
  `FloatUnOp`, `FloatCmp`, `FloatCmpBr`, `FixnumNeg`, `FixnumBitNot`,
  `RegAdd`, `RegSub`.
- FP transfer: `FprMove`, `FprSwap`, `F64ToFpr`, `FixnumToFpr`, `FloatToFpr`,
  `FprToStack`, `I64ToBoth`, `FprSave`, `FprRestore`, `CFunc_F_F`,
  `CFunc_FF_F`.
- Allocation / C-call: `CreateArray`, `NewArray`, `NewHash`, `NewRange`,
  `ConcatStr`, `ToA`, `DeepCopyLit`, `ConcatRegexp`, `ExpandArray`,
  `GenericBinOp`, `OptEqCmp`, `ArrayTEq`.
- Variables: `LoadGVar`, `StoreGVar`, `LoadCVar`, `StoreCVar`, `CheckCVar`,
  `LoadDynVar`, `StoreDynVar`, ivar/struct-slot inline & heap loads/stores,
  constants (`StoreConstant`, `GuardConst*`).
- `defined?` family, method/class definition (`MethodDef`, `ClassDef`,
  `SingletonClassDef`, …), method-call prologue (`GuardClassVersion`,
  `SetupMethodFrame`, `SetArguments`, `Call`, `Init`, `Preparation`),
  exceptions (`Raise`, `Retry`, `Redo`, `EnsureEnd`), `Yield`, `Inline`,
  and the specialized inlined-frame family.

Both backends emit all of these unconditionally. The aarch64 wildcard in
`compile_asmir_arch` is `unreachable!("handled by the shared compile_asmir
dispatcher")` — it can no longer be a bail.

### How aarch64 lowers what used to bail

The pre-`#704` bail sites were overwhelmingly **12-bit immediate-range limits**
(aarch64 fixed-width instructions encode only small immediates). They are now
handled, not declined:

- **LFP-relative frame offsets, callee-frame / prologue / loop-JIT `sub sp`,
  RSP-relative argument stores, block-arg offsets, class-def field offsets** —
  offsets that overflow the field are materialized into a scratch register
  (`mov xN, #imm` + register-offset addressing) instead of bailing. See the
  `a64_frame_*` / `a64_sp_*` / `a64_rsp_*` helpers in `compile.rs`.
- **RValue heap-field offsets** (inline/heap ivar & struct-slot access) — same
  scratch-materialization treatment.
- **Float `FloatBinOp` / `FloatUnOp`** — the full `BinOpK` / `UnOpK` set is
  lowered (the old port handled only `Add|Sub|Mul|Div` / `Neg|Pos`).
- **Live FP-pool register across a runtime call** — the runtime-call primitives
  save/restore the live xmm pool (`emit_fpr_save` / `emit_fpr_restore`) around
  the call, so they no longer bail on a live pool register.
- **Deopt write-back & forwarded arguments** — the side-exit generator
  reconstructs live frame state for all shapes; the single unported shape (the
  deferred-source `...`-forwarding deferral, `g(*rest, **kw, &blk)`) is
  prevented upstream by `forward_rest_deferral`, so it never reaches the
  backend.

---

## 3. (former §3 "aarch64 bail conditions" — removed)

This section catalogued the aarch64 bail sites. With the full port (`#704`)
there are no bail sites left; the content has been folded into §2's "How
aarch64 lowers what used to bail". The remaining *non-bail* asymmetries are in
§4.

---

## 4. Remaining asymmetry: recompilation & eviction patching (not coverage)

Two mechanisms still differ, both centered on **patching / recompiling
already-emitted code**, and both scoped to **non-specialized** frames. Neither
is a coverage gap: where x86 patches or recompiles in place, aarch64 deopts to
the VM, which then re-JITs through the normal warm-up counters. Correctness is
identical; only the recompile *strategy* (and thus steady-state performance
after a class-version change or BOP redefinition) differs.

### 4.1 Class-version-miss recompilation

- **x86-64:** `guard_class_version`
  ([x86_64/guard.rs:28](../monoruby/src/codegen/arch/x86_64/guard.rs#L28))
  emits a fast inline version check (page 0) plus an outlined
  recompile-and-recover slow path (page 1) via `gen_recompile`, distinguishing
  loop vs. method recompiles via `position` and offering a `with_recovery`
  jump-back. On a version miss it recompiles the whole method/loop **in place**
  and resumes.
- **aarch64:** `a64_guard_class_version`
  ([aarch64/guard.rs:89](../monoruby/src/codegen/arch/aarch64/guard.rs#L89))
  emits the inline check and **just deopts on miss** — *"Unlike x86 we do not
  recompile on miss yet — just deopt."* It ignores the x86 recompile params
  (`position`, `with_recovery`) it has no recompiler for.
- **Specialized frames are symmetric:** the specialized class-version guard
  *does* recompile on both arches. x86 uses `guard_class_version_specialized` /
  `gen_recompile_specialized`
  ([x86_64/guard.rs:57](../monoruby/src/codegen/arch/x86_64/guard.rs#L57));
  aarch64 uses `GuardClassVersionSpecialized` / `RecompileDeoptSpecialized` →
  `a64_call_recompile_specialized`
  ([aarch64/compile.rs:4755](../monoruby/src/codegen/arch/aarch64/compile.rs#L4755)),
  which rewrites the specialized body's `SpecializedCall` `bl`.

So the gap is specifically the **non-specialized method/loop class-version
guard**: x86 recompiles, aarch64 deopts.

### 4.2 Eviction via return-address patching (BOP redefinition)

- **x86-64:** the regular `Call` / `Yield` records, per call site, the return
  address plus a patch point (`emit_call` → `set_deopt_with_return_addr`,
  [x86_64/compile/mod.rs:1285](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L1285)).
  On **BOP (basic-op) redefinition** it rewrites the live return path to
  redirect into a deopt handler — *without* recompiling.
- **aarch64:** `a64_do_call`
  ([aarch64/compile.rs:825](../monoruby/src/codegen/arch/aarch64/compile.rs#L825))
  **skips** the return-address patching for the regular `Call`/`Yield` —
  *"The eviction-on-return patching (`set_deopt_with_return_addr`) is x86-only
  (runtime branch patching), so it is skipped — class-version changes are
  caught by `GuardClassVersion` deopts instead."*
- **Specialized calls/yields are symmetric:** aarch64 *does* implement
  return-address patching for the specialized inlined-frame path —
  `do_specialized_call`
  ([aarch64/compile.rs:1068](../monoruby/src/codegen/arch/aarch64/compile.rs#L1068))
  records the return address via `set_deopt_with_return_addr`
  ([aarch64/compile.rs:2433](../monoruby/src/codegen/arch/aarch64/compile.rs#L2433)),
  and `emit_immediate_evict`
  ([aarch64/compile.rs:2416](../monoruby/src/codegen/arch/aarch64/compile.rs#L2416))
  overwrites the recorded instruction on eviction.

So the gap is specifically the **non-specialized `Call`/`Yield`**: x86 patches
the return path for BOP eviction, aarch64 relies on the inline class-version
deopt.

---

## 5. Guard logic comparison

| Guard                         | x86-64                                                            | aarch64                                                                 |
| ----------------------------- | ---------------------------------------------------------------- | ---------------------------------------------------------------------- |
| `guard_class` immediates      | Fixnum/nil/true/false/symbol/float via `testq`/`cmpq`            | Fixnum/nil/true/false/symbol/float via `tbz`/`tbnz`/`cmp` ([guard.rs:14](../monoruby/src/codegen/arch/aarch64/guard.rs#L14)) |
| `guard_class` heap            | `guard_rvalue` (low-3-bits + class compare)                      | `a64_guard_rvalue` (same logic, `and`/`cbnz`/`ldr w`, [guard.rs:68](../monoruby/src/codegen/arch/aarch64/guard.rs#L68)) |
| `guard_class2` (BigNum→VM)    | yes — x86-only helper ([guard.rs:177](../monoruby/src/codegen/arch/x86_64/guard.rs#L177)), called from the monomorphic method-entry patch path (`codegen/patch.rs:158`) | not present                                                            |
| `guard_array_ty`              | yes (`ObjTy::ARRAY` at `RVALUE_OFFSET_TY`)                        | yes                                                                    |
| `guard_capture`               | yes (`branch_if_captured`)                                       | yes                                                                    |
| `float_to_f64` unbox          | yes (flonum / heap-Float, 0.0 sign-bit trick)                    | yes (mirrored)                                                         |
| class-version guard           | inline check **+ recompile + recovery** (page split, §4.1)       | inline check, **deopt only** for non-specialized; recompile for specialized frames (§4.1) |
| eviction on BOP redefinition  | return-address patching for regular & specialized calls (§4.2)  | return-address patching for **specialized** calls only; regular calls rely on class-version deopt (§4.2) |

Both `a64_guard_class` and `a64_guard_rvalue` always emit (they return a `bool`
for symmetry with x86, but never return `false` — every `ClassId` is handled,
immediates inline and everything else via the heap fallback).

---

## 5b. Local-slot addressing: rbp (x86) vs LFP (aarch64)

The two backends address a frame's own local/temporary slots through different
base registers, and this leaks into one correctness-relevant corner:

| | x86-64 | aarch64 |
| --- | --- | --- |
| `LMem::Slot` lowering | `[rbp - rbp_local(slot)]` (native frame pointer) | `[x22 - (slot*8 + LFP_SELF)]` (LFP) |
| deopt write-back (`wb.gp`) | `[r14 - conv(slot)]` (LFP) | `[x22 - …]` (LFP) |

Normally rbp and the LFP point at the same stack frame, so the choice is
invisible. They **diverge after `move_frame_to_heap`**: when a callee captures
the caller's frame (e.g. turns a block into a Proc — `to_enum(:m) { size }`,
`lazy`, …), the live frame becomes a heap copy that the LFP (reloaded from
`cfp.lfp` after the call) points at, while rbp still names the abandoned stack
frame. The JIT handles this by emitting a `guard_capture` after such a call that
deopts to the VM when capture happened; the deopt's write-back re-homes
register-resident (`wb.gp` / `wb.fpr`) slots **via the LFP**, so they reach the
heap copy.

A slot in `LinkMode::S` (value already at its stack home) is *not* in the
write-back — it is assumed materialized. On x86 that materialization is
rbp-relative, so a call **result** written to an `S` slot after a capturing call
lands on the dead stack frame and is lost (the VM then reads the stale heap
copy). With a non-empty GP pool this was masked because results stayed pool-
resident (`G`) and the deopt re-homed them via the LFP; it surfaces once the
pool is empty (the aarch64 default, and the x86 `GP_ALLOC_POOL = &[]` config).
aarch64 never had the bug because *all* its slot stores are already LFP-relative.

The fix is `AsmInst::RegToLfpStack` / `LMem::LfpSlot` (this commit): the result
of a possibly-capturing call (the `send` / `compile_yield` paths, gated on
`!no_capture_guard()`) is stored via the LFP (`def_rax2acc_capturing`) so it
follows the frame onto the heap — matching what aarch64 does for every slot, and
what the deopt write-back does for `G`/`F` slots. On aarch64 `LfpSlot` lowers
identically to `Slot`.

---

## 6. Practical consequences

- **Correctness is equal.** Both backends produce correct results.
- **Coverage is equal.** Both backends JIT every method/loop the front-end
  produces; aarch64 no longer falls back to the VM for any instruction shape.
- **Steady-state recompile behavior differs** in two narrow, non-specialized
  cases (§4): after a class-version change, x86 recompiles the method/loop in
  place while aarch64 deopts and re-JITs via warm-up counters; and on BOP
  redefinition, x86 patches the live return path of regular calls while aarch64
  relies on the inline class-version deopt. Both eventually reach an equivalent
  JIT-compiled steady state; the difference is the transition cost.
- **`guard_class2` / BigNum routing** is an x86-only method-entry guard helper
  (`patch.rs`); aarch64 has no equivalent on that path.

### One-line summary

> x86-64 and aarch64 now share the entire AsmIR front-end *and* full AsmInst
> coverage — aarch64 lowers everything (large immediates via scratch
> registers), so the `bool` bail return is vestigial. The only remaining
> asymmetries are non-coverage: x86 recompiles-in-place / patches live return
> addresses for **non-specialized** class-version misses and BOP eviction,
> where aarch64 deopts to the VM and re-JITs (specialized frames are symmetric);
> plus the x86-only `guard_class2` BigNum-routing helper.
