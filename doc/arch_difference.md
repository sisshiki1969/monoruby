# x86-64 / aarch64 JIT backend differences

A survey of how the two JIT machine-code backends differ today, focused on
**AsmInst coverage** and **lowering logic**. It is current as of the
`codegen/arch/` consolidation (commits up to #702).

- x86-64 backend: `monoruby/src/codegen/arch/x86_64/` — `compile/` (split into
  `mod.rs`, `binary_op.rs`, `method_call.rs`, `variables.rs`, `index.rs`,
  `builtin.rs`, `defined.rs`, `definition.rs`, `constants.rs`,
  `init_method.rs`) + `guard.rs`.
- aarch64 backend: `monoruby/src/codegen/arch/aarch64/` — `compile.rs`
  (one ~4.7 k-line file) + `guard.rs`.
- Shared front-end + dispatcher: `monoruby/src/codegen/jitgen/` (TraceIR →
  AsmIR) and `jitgen/asmir/compile_shared.rs` (the arch-neutral AsmInst
  lowering dispatcher).

---

## 1. The big picture: one front-end, two backends, one asymmetry

Both backends consume the **same** arch-neutral `AsmIR` produced by
`jitgen` (TraceIR → register-allocated AsmIR). They diverge only at the final
AsmIR → machine-code step. That step is driven by a single shared dispatcher,
`Codegen::compile_asmir`
([compile_shared.rs:23](../monoruby/src/codegen/jitgen/asmir/compile_shared.rs#L23)),
which lowers each `AsmInst` by one of two routes:

1. **Shared arm** — the `match` in `compile_asmir` handles the instruction
   structurally and calls a tiny per-arch **emission primitive**
   (`emit_reg_move`, `emit_reg_to_stack`, `emit_guard_class`,
   `emit_integer_binop`, …). Only the emitted bytes differ per arch.
2. **Per-arch arm** — the `other =>` fallthrough calls
   `compile_asmir_arch`, the backend-private match
   ([x86_64/compile/mod.rs:23](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L23),
   [aarch64/compile.rs:4687](../monoruby/src/codegen/arch/aarch64/compile.rs#L4687)).

### The defining asymmetry: bail vs. always-emit

Every emission primitive returns a `bool`:

- **x86-64 always returns `true`.** It is the fully-featured reference
  backend; it never declines to emit an instruction. Its `compile_asmir_arch`
  ends with `true` ([mod.rs:195](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L195)),
  and emit primitives carry comments like *"Always succeeds on x86 (no
  immediate-range limit); the bool result exists for the aarch64 twin."*
- **aarch64 may return `false` to *bail*.** A `false` propagates up through
  `compile_asmir` and aborts the whole compilation; the method/loop stays
  **VM-interpreted** instead. aarch64 is a streaming port that bails on any
  instruction shape it cannot yet lower.

So the `bool` return is, in practice, the aarch64 "not-yet-ported / out-of-range"
signal. The entire difference between the two backends can be read off as
*"the set of cases where the aarch64 primitive returns `false`."* The rest of
this document catalogs that set.

> Consequence: aarch64 never produces *wrong* code for an unported case — it
> falls back to the VM. The cost is coverage, not correctness. A single bailing
> instruction anywhere in a method forces the entire method back to the
> interpreter
> ([aarch64/compile.rs:160](../monoruby/src/codegen/arch/aarch64/compile.rs#L160)).

---

## 2. AsmInst coverage

### 2.1 Shared instructions (≈120 variants)

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
  `FprToStack`, `I64ToBoth`, `XmmSave`, `XmmRestore`, `CFunc_F_F`,
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

On x86-64 all of these emit unconditionally. On aarch64 each can bail per the
conditions in §3.

### 2.2 Per-arch `compile_asmir_arch` — identical, tiny

Notably, **both** backends route only the *same three* specialized
inlined-frame variants through their private `compile_asmir_arch`:

| AsmInst                          | x86-64                      | aarch64                       |
| -------------------------------- | --------------------------- | ----------------------------- |
| `GuardClassVersionSpecialized`   | [mod.rs:158](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L158) | [compile.rs:4703](../monoruby/src/codegen/arch/aarch64/compile.rs#L4703) |
| `RecompileDeoptSpecialized`      | [mod.rs:166](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L166) | [compile.rs:4720](../monoruby/src/codegen/arch/aarch64/compile.rs#L4720) |
| `SetArgumentsForwarded`          | [mod.rs:170](../monoruby/src/codegen/arch/x86_64/compile/mod.rs#L170) | [compile.rs:4742](../monoruby/src/codegen/arch/aarch64/compile.rs#L4742) |

The difference is the wildcard:

- x86-64: `_` cannot be reached for a real instruction (the shared dispatcher
  already handled it); the function returns `true`.
- aarch64: `_ => return false`
  ([compile.rs:4749](../monoruby/src/codegen/arch/aarch64/compile.rs#L4749)) —
  *"Anything still reaching the wildcard is not yet ported, so bail and keep
  the method VM-interpreted."*

So there is **no AsmInst that aarch64 handles in `compile_asmir_arch` but
x86 does not** — the two private matches are the same three arms. All real
divergence is in the *emit primitives* (whether they bail) and in the
patching/recompile machinery (§4).

---

## 3. aarch64 bail conditions (the coverage gap)

These are the cases where the aarch64 emit primitives return `false` and force
a VM fallback. x86-64 emits all of them.

### 3.1 Immediate-range limits — the dominant category

aarch64 fixed-width instructions encode small immediates only, so any offset
that overflows the field cannot be emitted inline:

- **LFP-relative frame offsets, 12-bit (`> 4095` bytes)**. Slot access is
  `[lfp, #(slot*8 + LFP_SELF)]`. Roughly two dozen emit primitives guard
  `off > 4095` and bail, covering `LitToStack`
  ([compile.rs:1445](../monoruby/src/codegen/arch/aarch64/compile.rs#L1445)),
  `StoreGVar`, `Load/StoreDynVar`, `CreateArray`, `NewHash`, `NewRange`,
  `ConcatStr`, `ToA`, `StoreCVar`, `AliasMethod`, the whole `defined?` family,
  `GenericBinOp`, `ArrayTEq`, `OptEqCmp`, `ConcatRegexp`, `CheckKwRest`,
  `ExpandArray`, `SingletonMethodDef`, and the deopt write-back path.
- **RValue heap-field offsets, 12-bit scaled (`> 32760` or non-8-aligned)**,
  via `a64_field_off_ok`. Bails the inline/heap ivar & struct-slot
  loads/stores ([compile.rs:2729–3000](../monoruby/src/codegen/arch/aarch64/compile.rs#L2729)).
- **Callee-frame `sub sp, sp, #imm` (`> 4080`, 16-aligned)** — argument setup
  ([compile.rs:762](../monoruby/src/codegen/arch/aarch64/compile.rs#L762)) and
  the forwarded helper ([compile.rs:814](../monoruby/src/codegen/arch/aarch64/compile.rs#L814)).
- **Method prologue frame `sub sp` (`> 4095`)** in `emit_init`
  ([compile.rs:2595](../monoruby/src/codegen/arch/aarch64/compile.rs#L2595)).
- **Loop-JIT entry stack bump (`> 4095`)**
  ([compile.rs:2869](../monoruby/src/codegen/arch/aarch64/compile.rs#L2869)).
- **RSP-relative argument stores (`> 4095`)** via `a64_rsp_slot_addr` —
  `RegToRSPOffset`, `ZeroToRSPOffset`, `U64ToRSPOffset`
  ([compile.rs:4523](../monoruby/src/codegen/arch/aarch64/compile.rs#L4523)).
- **Block-arg frame offset / tag (`> 4095`)**
  ([compile.rs:4590](../monoruby/src/codegen/arch/aarch64/compile.rs#L4590)).
- **`class`/`module` definition field offsets (`> 4095`)** in `class_def` /
  `singleton_class_def`
  ([compile.rs:1298](../monoruby/src/codegen/arch/aarch64/compile.rs#L1298)).

On x86-64 these are all `[reg + disp32]` / `sub rsp, imm32` — no comparable
limit, so nothing bails.

### 3.2 FP / xmm-pool related

- **Float `FloatBinOp`** supports only `Add | Sub | Mul | Div`; anything else
  bails ([compile.rs:2207](../monoruby/src/codegen/arch/aarch64/compile.rs#L2207)).
- **Float `FloatUnOp`** supports only `Neg`/`Pos`; others bail
  ([compile.rs:2242](../monoruby/src/codegen/arch/aarch64/compile.rs#L2242)).
- **Spilled FP operands** — several FP paths (`emit_fpr_to_stack`, the C-func
  calls) bail when a pool register has been spilled and the spill slot is
  out of range.
- **Live xmm/FP-pool register across a runtime call** — a number of
  runtime-call primitives (`UndefMethod`, `AliasGvar`, `CheckCVar`,
  `StoreCVar`, `AliasMethod`, the `defined?` family, `class_def`, …) bail when
  any pool FP register is live, because aarch64 has not yet ported the
  save/restore of the FP pool around those calls. The shared dispatcher
  comments flag each of these as *"aarch64 bails when an xmm pool register is
  live"*.

x86-64 implements xmm save/restore for all of these and supports the full
`BinOpK`/`UnOpK` set.

### 3.3 Unsupported integer ops

`a64_integer_binop` lowers only `Add | Sub | Mul | Div`; the `_ => return
false` arm ([compile.rs:396](../monoruby/src/codegen/arch/aarch64/compile.rs#L396))
bails the rest (in practice `Rem` and the bit-ops are lowered as method calls
upstream, so they should not reach the JIT — but the guard is there).

### 3.4 Deopt / write-back path

The side-exit (deopt/error-handler) generator `a64_gen_write_back_for_deopt`
bails when it cannot reconstruct live frame state:

- `forward_rest` is non-empty (rest-arg forwarding in write-back not ported)
  ([compile.rs:249](../monoruby/src/codegen/arch/aarch64/compile.rs#L249)).
- a live FP register cannot be spilled, or an immediate / accumulator slot
  offset exceeds 4095
  ([compile.rs:256–274](../monoruby/src/codegen/arch/aarch64/compile.rs#L256)).
- an unexpected side-exit variant (`SideExit::Evict(None)`) is seen
  ([compile.rs:147](../monoruby/src/codegen/arch/aarch64/compile.rs#L147)).

### 3.5 Forwarded arguments

`a64_set_arguments_forwarded` bails on the *deferred-source* shape
(`g(*rest, **kw, &blk)`), where the rest array is built on the generic path but
was skipped on the JIT path
([compile.rs:797](../monoruby/src/codegen/arch/aarch64/compile.rs#L797)).

---

## 4. x86-only machinery with no aarch64 equivalent

Beyond the bail conditions, x86-64 has two whole mechanisms aarch64 does not
implement, both centered on **patching already-emitted code**.

### 4.1 Return-address patching / deopt patch points

x86-64 records, for each call site, the return address plus a patch point so
that on **BOP (basic-op) redefinition** it can rewrite the live code to
redirect into a deopt handler — *without* recompiling.

- State: `return_addr_table` and `asm_return_addr_table` on `Codegen`
  ([codegen.rs:399](../monoruby/src/codegen.rs#L399)).
- `AsmInst::ImmediateEvict` records the patch-point position
  ([mod.rs `emit_immediate_evict`](../monoruby/src/codegen/arch/x86_64/compile/mod.rs)).
- `emit_call` / `emit_yield` record the return-address deopt patch point via
  `set_deopt_with_return_addr`.

aarch64 has **no branch patching**. `ImmediateEvict` is effectively a no-op,
`Call`/`Yield` record no patch point, and BOP-redefinition safety is covered by
the inline class-version guards instead. The shared dispatcher documents this
at `AsmInst::Call` / `ImmediateEvict`
([compile_shared.rs:230–285](../monoruby/src/codegen/jitgen/asmir/compile_shared.rs#L230)).

### 4.2 In-place recompilation + recovery

x86-64's class-version guard can, on a miss, **recompile the whole method (or
loop body) in place** and resume, via `gen_recompile` /
`gen_recompile_specialized`
([compiler.rs:71](../monoruby/src/codegen/compiler.rs#L71)), driven by
`guard_class_version(position, with_recovery, …)`
([x86_64/guard.rs:28](../monoruby/src/codegen/arch/x86_64/guard.rs#L28)). It
emits a fast inline version check (page 0) plus an outlined recompile-and-
recover slow path (page 1), distinguishing loop vs. method recompiles via
`position` and offering a `with_recovery` jump-back.

aarch64's **non-specialized** class-version guard `a64_guard_class_version`
just deopts on miss — no recompile, no recovery
([aarch64/guard.rs:89](../monoruby/src/codegen/arch/aarch64/guard.rs#L89)):
*"Unlike x86 we do not recompile on miss yet — just deopt."* It also ignores
the x86 recompile params (`position`, `with_recovery`) it has no recompiler
for.

> Exception worth noting: the **specialized** (inlined-frame) variants *do*
> recompile on aarch64. `GuardClassVersionSpecialized` and
> `RecompileDeoptSpecialized` both call `a64_call_recompile_specialized`,
> rewriting the specialized body's `SpecializedCall` `bl`
> ([compile.rs:4703–4736](../monoruby/src/codegen/arch/aarch64/compile.rs#L4703)).
> So the "no recompile" gap is specifically the *non-specialized* method/loop
> class-version guard.

---

## 5. Guard logic comparison

| Guard                         | x86-64                                                            | aarch64                                                                 |
| ----------------------------- | ---------------------------------------------------------------- | ---------------------------------------------------------------------- |
| `guard_class` immediates      | Fixnum/nil/true/false/symbol/float/**bool** via `testq`/`cmpq`   | Fixnum/nil/true/false/symbol/float via `tbz`/`tbnz`/`cmp`; no dedicated bool collapse |
| `guard_class` heap            | `guard_rvalue` (low-3-bits + class compare)                      | `a64_guard_rvalue` (same logic, `and`/`cbnz`/`ldr w`)                   |
| `guard_class2` (BigNum→VM)    | yes — BigNum always routed to VM entry                           | not present                                                            |
| `guard_array_ty`              | yes (`ObjTy::ARRAY` at `RVALUE_OFFSET_TY`)                        | yes                                                                    |
| `guard_capture`               | yes (`branch_if_captured`)                                       | yes                                                                    |
| `float_to_f64` unbox          | yes (flonum / heap-Float, 0.0 sign-bit trick)                    | yes (mirrored)                                                         |
| class-version guard           | inline check **+ recompile + recovery** (page split)             | inline check, **deopt only** (recompile only for specialized frames)  |

The `a64_guard_class` primitive returns `bool` for symmetry with x86, but in
its current form it handles every `ClassId` (immediates inline, everything else
via the heap `a64_guard_rvalue` fallback) and **always returns `true`** — i.e.
class guards themselves do not currently bail
([aarch64/guard.rs:14–63](../monoruby/src/codegen/arch/aarch64/guard.rs#L14)).
(The shared dispatcher's "aarch64 bails for not-yet-supported class kinds"
comment is forward-looking; no class kind bails today.)

---

## 6. Practical consequences

- **Correctness is equal.** Both backends produce correct results; aarch64
  simply executes more code in the VM tier.
- **Coverage differs by frame size and feature mix.** Methods with large
  local frames, many locals/ivars (large offsets), the full float/integer op
  set on spilled operands, live-FP-across-runtime-call shapes, rest-arg
  forwarding in deopt, or the deferred-source forwarding shape will JIT on
  x86-64 but fall back to the VM on aarch64.
- **The dominant gap is immediate-range encoding** (≈80% of aarch64 bail
  sites), not missing instruction semantics. Closing it means emitting
  materialize-offset-into-scratch-register sequences in those primitives.
- **The structural gaps are patching-based**: BOP-eviction via return-address
  patching, and non-specialized method/loop recompile-on-version-miss. aarch64
  substitutes plain deopt-to-VM for both.

### One-line summary

> x86-64 is the complete reference backend that always emits and can patch
> live code; aarch64 shares the entire AsmIR front-end and the same instruction
> dispatch, but each emit primitive may *bail to the VM* — overwhelmingly on
> 12-bit immediate-range limits, plus a few not-yet-ported feature shapes (full
> FP/int op set, xmm-save-around-call, code patching, non-specialized
> recompile).
