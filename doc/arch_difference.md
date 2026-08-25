# x86-64 / aarch64 JIT backend differences

A survey of how the two JIT machine-code backends differ today, focused on
**AsmInst coverage** and **lowering logic**. Current as of the loop-JIT entry
pin (`#1176`).

The two trees are mirrors of each other — same file names, 20 files and
~12.1 k lines each:

```
monoruby/src/codegen/arch/{x86_64,aarch64}/
    codegen.rs  compile/  guard.rs  invoker.rs  jit_module.rs
    vmgen.rs    vmgen/    wrapper.rs
```

- Shared front-end + dispatcher: `monoruby/src/codegen/jitgen/` (TraceIR →
  AsmIR) and `jitgen/asmir/compile_shared.rs` (the arch-neutral AsmInst
  lowering dispatcher).

> Line-number links below are omitted on purpose: both backends have been
> re-split since this document was first written (`#994` broke the aarch64
> `compile.rs` / `vmgen.rs` monoliths into the `compile/` and `vmgen/`
> directories above), and the previous revision's line anchors had all gone
> stale. Grep for the function names instead.

> **History.** An earlier revision of this document (pre-`#704`) described
> aarch64 as a *streaming port that bails to the VM* on any instruction shape it
> could not lower, and catalogued ~two dozen bail sites. **That is no longer
> true.** As of `#704` aarch64 lowers every `AsmInst` and every side exit, so it
> never bails out of JIT compilation. The sections below describe the current,
> bail-free state; §4 covers the asymmetry that *does* remain (recompilation
> strategy — not coverage).

---

## 1. The big picture: one front-end, two backends, coverage-symmetric

Both backends consume the **same** arch-neutral `AsmIR` produced by `jitgen`
(TraceIR → register-allocated AsmIR). They diverge only at the final
AsmIR → machine-code step, driven by a single shared dispatcher,
`Codegen::compile_asmir`
(`jitgen/asmir/compile_shared.rs`),
which lowers each `AsmInst` by one of two routes:

1. **Shared arm** — the `match` in `compile_asmir` handles the instruction
   structurally and calls a tiny per-arch **emission primitive**
   (`emit_reg_move`, `emit_reg_to_stack`, `emit_guard_class`,
   `emit_integer_binop`, …). Only the emitted bytes differ per arch.
2. **Per-arch arm** — the `other =>` fallthrough calls `compile_asmir_arch`,
   the backend-private match
   (`compile_asmir_arch` in each backend's `compile/mod.rs`).
   On both arches this handles only the *same five* specialized inlined-frame
   variants (`LoadCallerSlot`, `GuardClassVersionSpecialized`,
   `GuardConstVersionSpecialized`, `RecompileDeoptSpecialized`,
   `SetArgumentsForwarded`); everything else is handled by the shared arm.

### The `bool` return is now vestigial

Every emission primitive (and `compile_asmir` itself) returns a `bool`. In the
pre-`#704` port this was the aarch64 "not-yet-ported / out-of-range → bail to
the VM" signal. **Today both backends always return `true`:**

- x86-64 is the original fully-featured reference backend; it never declines.
- aarch64 now lowers everything too — large frame/field/sp offsets are
  materialized through scratch registers rather than bailing, and the
  `...`-forwarding *deferral*, once disabled upstream for aarch64, is lowered
  there as well (`a64_set_arguments_forwarded_deferred`).

There is no `return false` anywhere in the aarch64 lowering (`compile/`,
`guard.rs`), and `compile_asmir_arch`'s wildcard arm is `unreachable!()`. The driver chain (`gen_asm` / `gen_machine_code` / `jit_compile`)
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
  `a64_frame_*` / `a64_sp_*` / `a64_addr_*` helpers in `compile/mod.rs`.
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

## 4. Remaining asymmetry: recompilation strategy (not coverage)

One mechanism still differs — **recompiling already-emitted code** on a
class-version miss, and only for **non-specialized** frames. It is not a
coverage gap: where x86 recompiles in place, aarch64 deopts to the VM, which
then re-JITs through the normal warm-up counters. Correctness is identical;
only the recompile *strategy* (and thus steady-state performance after a
class-version change) differs. §4.2 records an eviction asymmetry that no
longer exists.

### 4.1 Class-version-miss recompilation

**The guard itself is symmetric now.** Both `guard_class_version` and
`a64_guard_class_version` compare the global version word against the *unit's
patchable snapshot word* (the `class_version_label` `jit_compile` creates), so
a successful salvage re-validates the unit's code in place on either arch by
storing the current version into that word (`Codegen::set_class_version`). An
earlier revision of this document quoted an aarch64 comment saying *"we do not
recompile on miss yet — just deopt"*; that text is gone.

The **recovery jump-back** is ported too: both arches now have a
`jit_recompile_method_with_recovery` (the aarch64 one returns a tri-state —
salvaged / recompiled / recompile-panicked — since aarch64 surfaces a
recompile panic as a Ruby `FatalError`, which x86 does not). On either arch a
non-specialized class-version miss whose salvage succeeds jumps straight back
into the compiled body; only a genuine change pays the deopt. The aarch64
call helper saves the full x86 `save_registers` equivalent (x1-x8 + d2-d7;
d8-d15/x19-x28 are callee-saved under AAPCS64, and x0 is the return register
and dead at a guard, like rax on x86).

**Specialized frames are symmetric:** the specialized class-version guard
recompiles on both arches. x86 uses `guard_class_version_specialized` /
`gen_recompile_specialized`; aarch64 uses `GuardClassVersionSpecialized` /
`RecompileDeoptSpecialized` → `a64_call_recompile_specialized`, which rewrites
the specialized body's `SpecializedCall` `bl`.

### 4.2 On-stack eviction (BOP redefinition) — no longer asymmetric

This section used to record an asymmetry: x86 recorded a return-address patch
point at every call site and, on **BOP (basic-op) redefinition**, wrote a `jmp`
into the live return path so the suspended frame deopted; aarch64 did that for
specialized calls only, and relied on the inline class-version deopt for the
rest.

Both halves are gone. Every call/yield site on both arches now records its
return address (`set_deopt_with_return_addr`) purely as a *key*, and BOP
redefinition runs the arch-neutral chain-deopt walk (`Codegen::chain_deopt`),
which converts each suspended JIT frame into an interpreter frame from the
stack alone — no code is patched, on either arch. See `doc/chain_deopt.md`
§10.

---

## 5. Guard logic comparison

| Guard                         | x86-64                                                            | aarch64                                                                 |
| ----------------------------- | ---------------------------------------------------------------- | ---------------------------------------------------------------------- |
| `guard_class` immediates      | Fixnum/nil/true/false/symbol/float via `testq`/`cmpq`            | same set via `tbz`/`tbnz`/`cmp`                                        |
| `guard_class` heap            | `guard_rvalue` (low-3-bits + class compare)                      | `a64_guard_rvalue` (same logic, `and`/`cbnz`/`ldr w`)                  |
| `guard_class2` (BigNum→VM)    | yes, from the monomorphic method-entry patch path (`codegen/patch.rs`) | yes — `a64_guard_class2`, from `wrapper.rs`; only `INTEGER_CLASS` differs |
| `guard_array_ty`              | yes (`ObjTy::ARRAY` at `RVALUE_OFFSET_TY`)                        | yes                                                                    |
| `guard_capture`               | yes (`branch_if_captured`)                                       | yes                                                                    |
| `float_to_f64` unbox          | yes (flonum / heap-Float, 0.0 sign-bit trick)                    | yes (mirrored)                                                         |
| class-version guard           | unit snapshot word + recovery jump-back (§4.1)                    | same — recovery jump-back ported (§4.1)                                 |
| eviction on BOP redefinition  | arch-neutral chain-deopt walk, no code patching (§4.2)          | identical (§4.2)                                                        |

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
pool is empty, which `GP_ALLOC_POOL = &[]` now makes unconditional on both
arches (`LinkMode::G` was abolished; `jitgen/gp_alloc.rs` drives GP reuse
locally instead, identically on both).
aarch64 never had the bug because *all* its slot stores are already LFP-relative.

The fix is `AsmInst::RegToLfpStack` / `LMem::LfpSlot` (this commit): the result
of a possibly-capturing call (the `send` / `compile_yield` paths, gated on
`!no_capture_guard()`) is stored via the LFP (`def_rax2acc_capturing`) so it
follows the frame onto the heap — matching what aarch64 does for every slot, and
what the deopt write-back does for `G`/`F` slots. On aarch64 `LfpSlot` lowers
identically to `Slot`.

---

## 5c. Other current asymmetries

Neither a coverage gap nor a guard difference, but worth knowing:

| | x86-64 | aarch64 |
| --- | --- | --- |
| **Installing / re-pointing JIT code** | patches branches in place (`apply_jmp_patch_address`, the `patch_point` `call`) | indirect heap slots — `ISeqInfo::jit_slot` and `jit_guard_free_slot` are `#[cfg(target_arch = "aarch64")]` fields — plus `Codegen::patch_call_to_entry`, a single `bl` rewrite under the MAP_JIT writable/executable flip |
| **Cold-code placement** | cold handlers go on page 1 (`select_page`, ~90 sites) | page 1 is past `B`/`BL` range from page 0, so cold blocks are laid inline (~7 sites). In exchange aarch64 has an optimization x86 has no use for: `AsmIr::as_pure_deopt` / `pure_deopt_target` (`#[cfg(target_arch = "aarch64")]`) emit a deopt-only block's handler *at* the block label, so predecessors branch straight onto the deopt code |
| **Branch range** | never a constraint | a large loop body can put a `TBZ`/`TBNZ` further from its deopt than imm14 (+/-32 KiB) reaches, which panics the emit. `jit_compile_loop` catches it and leaves the codeptr unpublished; `a64_op_loop_start`'s tri-state slot (`0` / `1` sentinel / codeptr) stops the retry loop |
| **`RecompileDeopt` error exit** | `error: None` | needs `Some(ir.new_error(state))` — a recompile-time panic surfaces as a Ruby `FatalError` to branch to (`jitgen/compile.rs`) |
| **GC frame tracing** | `alloc.rs`'s `record_frames` walks `rbp` via inline asm | not implemented (x86-only debug aid) |
| **Loop-JIT entry `sp`** | `lea rsp, [rbp - depth]` | `sub`+`mov sp` to the same depth | 

The last row is symmetric by design as of `#1176`: both pin the entry to
`total - PROLOGUE_OVERHEAD` rather than subtracting from the `sp` they inherit,
because the frame may have been built by either the VM's `init_method` or a JIT
prologue and only the latter has already reserved the unit's spill region.

---

## 6. Practical consequences

- **Correctness is equal.** Both backends produce correct results.
- **Coverage is equal.** Both backends JIT every method/loop the front-end
  produces; aarch64 no longer falls back to the VM for any instruction shape.
- **Recompile behavior is symmetric** (§4.1): a salvaged class-version miss
  resumes compiled code in place on both arches; a genuine change recompiles
  and pays one deopt.
- **A very large loop body may stay interpreted on aarch64** (§5c, branch
  range). x86 has no such limit.

### One-line summary

> x86-64 and aarch64 share the entire AsmIR front-end *and* full AsmInst
> coverage — aarch64 lowers everything (large immediates via scratch
> registers), so the `bool` bail return is vestigial. What remains is
> non-coverage: different **code-installation** mechanisms (branch patching
> vs indirect slots) and **cold-code placement** (page 1 vs inline, which
> buys aarch64 the `as_pure_deopt` collapse), and aarch64's **branch-range**
> ceiling on very large loop bodies (§5c). Guards, the recovery jump-back,
> BOP eviction, block/method inlining and the GP pool are all symmetric.
