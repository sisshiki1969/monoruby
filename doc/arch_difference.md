# x86-64 / aarch64 JIT backend differences

How the two JIT machine-code backends differ, focused on **AsmInst
coverage** and **lowering logic**.

The two trees are mirrors of each other — same file names, 20 files each
(~15 k lines on x86-64, ~16 k on aarch64):

```
monoruby/src/codegen/arch/{x86_64,aarch64}/
    codegen.rs  compile/  guard.rs  invoker.rs  jit_module.rs
    vmgen.rs    vmgen/    wrapper.rs
```

- Shared front-end + dispatcher: `monoruby/src/codegen/jitgen/` (TraceIR →
  AsmIR) and `jitgen/asmir/compile_shared.rs` (the arch-neutral AsmInst
  lowering dispatcher).

Line-number links are omitted on purpose; grep for the function names.

---

## 1. The big picture: one front-end, two backends, coverage-symmetric

Both backends consume the **same** arch-neutral `AsmIR` produced by `jitgen`
(TraceIR → register-allocated AsmIR). They diverge only at the final
AsmIR → machine-code step, driven by a single shared dispatcher,
`Codegen::compile_asmir` (`jitgen/asmir/compile_shared.rs`), which lowers
each `AsmInst` by one of two routes:

1. **Shared arm** — the `match` in `compile_asmir` handles the instruction
   structurally: it builds `LInst`s for the per-arch `encode_linst`
   (`doc/lir.md`), or calls a per-arch emission helper (`emit_*`,
   `guard_class` / `a64_guard_class`, `integer_binop` /
   `a64_integer_binop`, …). Only the emitted bytes differ per arch.
2. **Per-arch arm** — the `other =>` fallthrough calls `compile_asmir_arch`
   (each backend's `compile/mod.rs`). On both arches it handles the *same
   four* specialized variants (`GuardClassVersionSpecialized`,
   `GuardConstVersionSpecialized`, `RecompileDeoptSpecialized`,
   `SetArgumentsForwarded`); its wildcard arm is `unreachable!()`.

### The `bool` return is vestigial

`compile_asmir` and many emission helpers return a `bool`, once the "decline
this compile" signal. **Both backends always return `true`**: aarch64
lowers everything too — large frame / field / sp offsets are materialized
through scratch registers, and the `...`-forwarding deferral is lowered
there as well (`a64_set_arguments_forwarded_deferred`). The driver chain
(`gen_asm` / `gen_machine_code` / `jit_compile`) does not act on the result;
the `bool` stays only because changing the signatures to `()` is churn.

---

## 2. AsmInst coverage — full on both arches

Every `AsmInst` variant is lowered on both arches, almost all through the
shared `compile_asmir` match:

- Register / stack moves: `RegMove`, `RegToStack`, `RegToLfpStack`,
  `StackToReg`, `LitToReg`, `LitToStack`, `RegAdd`, `RegSub`.
- Control flow: `CondBr`, `NilBr`, `Br`, `BrFlags`, `CheckLocal`,
  `OptCase`, `Deopt`, `HandleError`, `Ret`, `MethodRet`, `BlockBreak`.
- Guards: `GuardClass`, `GuardClassIn`, `GuardClassVersion`,
  `GuardConstBaseClass`, `GuardConstVersion`, `GuardArrayTy`, `GuardFrozen`,
  `GuardCapture`, `CheckBOP`, `CheckStack`, `ExecGc`.
- Arithmetic: `IntegerBinOpReg`, `IntegerBinOpImm`, `IntegerCmpReg`,
  `IntegerCmpImm`, `IntegerDouble`, `IntegerSucc`, `FloatBinOp`,
  `FloatUnOp`, `FloatCmp`, `FixnumNeg`, `FixnumBitNot`, `MathSqrt`.
- Condition flags: `CmpFlags`, `CmpImmFlags`, `TestBitFlags`,
  `FloatCmpFlags` (set the flags only) and `BrFlags` (branch on them).
- FP transfer: `FprMove`, `FprSwap`, `FixnumToFpr`, `FloatToFpr`,
  `FprToStack`, `FprSave`, `FprRestore`, `FloatArgMove`,
  `FloatRetStore` / `FloatRetLoad`, the C calls `CFunc`.
- Allocation / C-call: `CreateArray`, `CreateArrayFrom`, `NewArray`,
  `NewHash`, `NewRange`, `CreateRange`, `ConcatStr`, `ToA`, `DeepCopyLit`,
  `ConcatRegexp`, `ExpandArray`, `GenericBinOp`, `GenericSend`, `OptEqCmp`,
  `ArrayTEq`, the `Hash*` probes.
- Variables: `LoadGVar`, `StoreGVar`, `LoadCVar`, `StoreCVar`, `CheckCVar`,
  `LoadDynVar`, `StoreDynVar`, ivar / struct-slot inline and heap loads and
  stores, constants (`StoreConstant`, `GuardConst*`).
- `defined?` family, method / class definition (`MethodDef`, `ClassDef`,
  `SingletonClassDef`, …), method-call prologue (`SetupMethodFrame`,
  `SetArguments`, `Call`, `Init`, `Preparation`), exceptions (`Raise`,
  `Retry`, `Redo`, `EnsureEnd`), `Yield`, `Inline`, and the specialized
  inlined-frame family.

### How aarch64 lowers large immediates

aarch64's fixed-width instructions encode only small immediates. Offsets
that overflow the field — LFP-relative frame offsets, callee-frame /
prologue / loop-JIT `sub sp`, RSP-relative argument stores, block-arg
offsets, RValue heap-field offsets — are materialized into a scratch
register (`mov xN, #imm` + register-offset addressing): the `a64_frame_*`,
`a64_field_*`, `a64_sp_*` and `a64_rsp_slot_addr` helpers in
`compile/mod.rs`. A live FP-pool register across a runtime call is saved
and restored around it (`emit_fpr_save` / `emit_fpr_restore`), and the deopt
write-back reconstructs the live frame state for every shape, the elided
objects of the escape analysis included (`a64_gen_virtual_materialize`).

---

## 3. Recompilation and eviction — symmetric

Recovering from a broken assumption works the same way on both arches; only
the helpers that implement it are per-arch.

### 3.1 Class-version-miss recompilation

**The guard.** Both `guard_class_version` and
`a64_guard_class_version` compare the global version word against the *unit's
patchable snapshot word* (the `class_version_label` `jit_compile` creates), so
a successful salvage re-validates the unit's code in place on either arch by
storing the current version into that word (`Codegen::set_class_version`).

The **recovery jump-back** is symmetric too: both arches have a
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

### 3.2 On-stack eviction (BOP redefinition) — symmetric

Every call / yield site on both arches records its return address
(`set_deopt_with_return_addr`) purely as a *key*, and BOP redefinition runs
the arch-neutral chain-deopt walk (`Codegen::chain_deopt_into`), which
converts each suspended JIT frame into an interpreter frame from the stack
alone — no code is patched, on either arch. See `doc/chain_deopt.md`.

---

## 4. Guard logic comparison

| Guard                         | x86-64                                                            | aarch64                                                                 |
| ----------------------------- | ---------------------------------------------------------------- | ---------------------------------------------------------------------- |
| `guard_class` immediates      | Fixnum/nil/true/false/bool/symbol/float via `testq`/`cmpq`       | same set via `tbz`/`tbnz`/`cmp`                                        |
| `guard_class` heap            | `guard_rvalue` (low-3-bits + class compare)                      | `a64_guard_rvalue` (same logic, `and`/`cbnz`/`ldr w`)                  |
| `guard_class2` (BigNum→VM)    | yes, from the monomorphic method-entry patch path (`codegen/patch.rs`) | yes — `a64_guard_class2`, from `wrapper.rs`; only `INTEGER_CLASS` differs |
| `guard_array_ty`              | yes (`ObjTy::ARRAY` at `RVALUE_OFFSET_TY`)                        | yes                                                                    |
| `guard_capture`               | yes (`branch_if_captured`)                                       | yes                                                                    |
| `float_to_f64` unbox          | yes (flonum / heap-Float, 0.0 sign-bit trick)                    | yes (mirrored)                                                         |
| class-version guard           | unit snapshot word + recovery jump-back (§3.1)                    | same — recovery jump-back ported (§3.1)                                 |
| eviction on BOP redefinition  | arch-neutral chain-deopt walk, no code patching (§3.2)          | identical (§3.2)                                                        |
| deopt recording (`deopt` / `profile`) | `log_deoptimize` from every deopt handler, per-guard trampolines, class-guard miss recorder | `log_deoptimize` from every deopt handler; no trampolines (`guard: unknown`, see `doc/deopt_log.md`) and no class-guard miss recorder |

Both `a64_guard_class` and `a64_guard_rvalue` always emit (they return a `bool`
for symmetry with x86, but never return `false` — every `ClassId` is handled,
immediates inline and everything else via the heap fallback).

---

## 5. Local-slot addressing: rbp (x86) vs LFP (aarch64)

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
pool is empty, as `GP_ALLOC_POOL = &[]` makes it on both arches
(`jitgen/gp_alloc.rs` keeps Fixnums in registers only within a basic block).
aarch64 never had the problem because *all* its slot stores are already
LFP-relative.

`AsmInst::RegToLfpStack` / `LMem::LfpSlot` handles it: the result
of a possibly-capturing call (the `send` / `compile_yield` paths, gated on
`!no_capture_guard()`) is stored via the LFP (`def_rax2acc_capturing`) so it
follows the frame onto the heap — matching what aarch64 does for every slot, and
what the deopt write-back does for `G`/`F` slots. On aarch64 `LfpSlot` lowers
identically to `Slot`.

---

## 6. Other current asymmetries

Neither a coverage gap nor a guard difference, but worth knowing:

| | x86-64 | aarch64 |
| --- | --- | --- |
| **Installing / re-pointing JIT code** | patches branches in place (`apply_jmp_patch_address`, the `patch_point` `call`) | indirect heap-leaked dispatch words — `ISeqInfo::jit_slot` and `jit_guard_free_slot` are `#[cfg(target_arch = "aarch64")]` fields |
| **Cold-code placement** | cold handlers go on page 1 (`select_page`, ~90 sites) | page 1 is past `B`/`BL` range from page 0, so side-exit handlers, and the slow paths of the safepoint poll and the class- / const-version guards (`Codegen::a64_cold_stub`), go into *side-exit islands* in the same code stream: one at the frame's end, behind a `b skip` only where code can fall into it, plus range thunks every 8 KiB of hot code (`a64_drain_side_exits`, `a64_thunk_side_exits`). The hot path is one conditional branch to cold code, as on x86. In exchange aarch64 has an optimization x86 has no use for: `AsmIr::as_pure_deopt` / `pure_deopt_target` (`#[cfg(target_arch = "aarch64")]`) emit a deopt-only block's handler *at* the block label, so predecessors branch straight onto the deopt code |
| **Branch range** | never a constraint | a large loop body can put a `TBZ`/`TBNZ` further from its deopt than imm14 (+/-32 KiB) reaches, which panics the emit. `jit_compile_loop` catches it and leaves the codeptr unpublished; `a64_op_loop_start`'s tri-state slot (`0` / `1` sentinel / codeptr) stops the retry loop |
| **A panic inside a compile** | contained at every entry point (`compiler.rs` `contain_compile_panic`): nothing is published, the warm-up counter has run out so the method / loop stays interpreted, and a failed recompile leaves the old code deopting as before | the same for a first compile; a *recompile* panic additionally sets a Ruby `FatalError` (the row below) |
| **`RecompileDeopt` error exit** | `error: None` | needs `Some(ir.new_error(state))` — a recompile-time panic surfaces as a Ruby `FatalError` to branch to (`jitgen/compile.rs`) |
| **GC frame tracing** | `alloc.rs`'s `record_frames` walks `rbp` via inline asm | not implemented (x86-only debug aid) |
| **Loop-JIT entry `sp`** | `lea rsp, [rbp - depth]` | `sub`+`mov sp` to the same depth | 

The last row is symmetric by design: both pin the entry to
`total - PROLOGUE_OVERHEAD` rather than subtracting from the `sp` they inherit,
because the frame may have been built by either the VM's `init_method` or a JIT
prologue and only the latter has already reserved the unit's spill region.

---

## 7. Practical consequences

- **Correctness is equal.** Both backends produce correct results.
- **Coverage is equal.** Both backends JIT every method/loop the front-end
  produces; aarch64 does not fall back to the VM for any instruction shape.
- **Recompile behavior is symmetric** (§3.1): a salvaged class-version miss
  resumes compiled code in place on both arches; a genuine change recompiles
  and pays one deopt.
- **A very large loop body may stay interpreted on aarch64** (§6, branch
  range). x86 has no such limit.

### One-line summary

> x86-64 and aarch64 share the entire AsmIR front-end *and* full AsmInst
> coverage — aarch64 lowers everything (large immediates via scratch
> registers), so the `bool` bail return is vestigial. What remains is
> non-coverage: different **code-installation** mechanisms (branch patching
> vs indirect slots) and **cold-code placement** (page 1 vs in-stream
> islands, which buys aarch64 the `as_pure_deopt` collapse), and aarch64's **branch-range**
> ceiling on very large loop bodies (§6). Guards, the recovery jump-back,
> BOP eviction, block/method inlining and the GP pool are all symmetric.
