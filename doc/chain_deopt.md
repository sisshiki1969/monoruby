# Chain deopt — converting suspended JIT frames to interpreter frames

**Kind:** reference. Chain deopt drops a whole suspended chain of compiled
frames back to the interpreter in one step, without touching machine code.
It is the only way an on-stack JIT frame is dropped to the interpreter. This
document describes the mechanism, the frame-layout facts it depends on, and
its three callers: basic-op redefinition, escalated side exits, and captures.

---

## 1. What this is for

A compiled frame that is suspended at a call cannot be protected by its
callee's guards. Three situations need such frames converted:

* **Basic-op redefinition.** Redefining `Integer#+` inside a callee makes
  the caller's compiled continuation stale, because it inlined the builtin
  operator. Every suspended compiled frame on every thread must stop running
  its compiled body (`doc/bop_redefinition.md`, §10.1 below).
* **Side exits inside a compilation unit.** Specialized callees, frameless
  bodies and yielded blocks are compiled into the caller's unit, and the
  caller's compiled continuation relies on facts derived from them: a
  narrowed return state (§6), and outer-frame `Float` locals that a
  specialized block reads and writes unboxed (§5 step 5,
  `doc/trace_chain_joins.md` §5). When such a callee deopts, the
  suspended frames above it in the same unit must not resume their
  compiled code.
* **Captures.** A `binding` / `eval` that reaches a compiled frame through
  a path the JIT did not anticipate converts that frame first, so the
  capture reads homed slots (`Codegen::convert_for_capture`).

## 2. The mechanism

On a chain deopt every frame stays where it is on the stack. For each
suspended compiled frame the walk:

1. writes the frame's register- and spill-resident state back into its
   local slots (boxing floats, materializing elided objects);
2. rewrites the frame's **return-address slot** to one shared continuation
   stub that resumes the interpreter after the call (§8.2).

Control then never re-enters the compiled body: as each callee returns, its
`ret` lands in the stub. Because no machine code is rewritten, a compiled
body that is still valid for future invocations survives.

## 3. Frame facts the mechanism relies on

### 3.1 The VM continuation after a call

The VM's send handler resumes after a call by popping the cont frame (which
holds the suspended pc), re-deriving the destination slot from the bytecode
at that pc, storing the result and dispatching the next instruction. It is
stack-driven and carries no per-site state. That sequence assumes a 2-unit
`send`, though, while 1-unit operator sites (`BinOp`, `Index`, …) dispatch
through the same JIT `send` emitter, which is why the walk targets a
dedicated stub rather than the raw VM continuation (§8.2).

### 3.2 The suspended pc

`AsmInst::ContFramePc { call_site_pc }` stores the call-site pc into the
outgoing cont-frame slot at every JIT call and yield emitter (ordinary
`send`, `send_specialized`, generic and specialized yield). It is the slot
`Kernel#caller` reads (`Cfp::caller_pc_slot`), and the stub reads it too.

### 3.3 What a write-back contains

`WriteBack` (`jitgen.rs`) has six kinds of entry:

```rust
fpr: Vec<(FPReg, Vec<SlotId>)>,                            // float -> one or more slots
literal: Vec<(Value, SlotId)>,
void: Vec<SlotId>,
gp: Vec<(GP, SlotId)>,
forward_kwrest: Vec<(SlotId, Box<[(IdentId, SlotId)]>)>,   // build the kwrest Hash
virtual_obj: Vec<(SlotId, VirtualSrc)>,                    // elided objects to allocate
```

Replaying one needs the `WriteBack` and the frame's FP spill `base`, and
`virtual_obj` / `forward_kwrest` need runtime calls (`create_array` or
`value_deep_copy`, `correct_rest_kw`), so it is not a pure memory shuffle.

### 3.4 The registration key

A suspended frame is identified by the return address in its callee's
return-address slot, which is all the walk has to go on.
`Codegen::chain_deopt_table` maps that return address to the site's
conversion stub and the calling frame's depth in its compilation unit.
`AsmInst::ChainExit` is pushed after the call, when the return address is no
longer at hand, so it names its site through `asm_return_addr_table`, keyed
by the site's `AsmEvict` id.

## 4. Why the target is a stub, not the frame's own deopt handler

Pointing the return-address slot at the call site's own side-exit handler
looks simpler, but it is wrong. The site's write-back is captured *after*
the result store (`def_rax2acc_return`) is modelled, so its handler assumes
the callee's return value is already in its slot. Entering it by `ret` skips
that store and the return value is lost. The continuation stub performs the
store itself (§8.2).

## 5. The steps

1. **Per-site conversion.** Each call and yield site's replay data is
   compiled into a conversion stub and registered in `chain_deopt_table`
   (§8.1).
2. **The continuation.** One shared continuation stub per arch (§8.2).
3. **The walk.** `Codegen::chain_deopt_into` walks the CFP chain from a
   given frame, converting each registered compiled frame (§8.5).
4. **Escalation.** An interpreter-resuming side exit in a frame that is not
   its unit's root runs the walk over its unit's suspended frames before it
   resumes the interpreter (§8.5).
5. **Unboxed outer locals.** A specialized block reads and writes an outer
   frame's `Float` local directly in that frame's FP save / spill area
   (`LInst::LoadDynVarSpecF` / `StoreDynVarSpecF`, a single `f64` move at
   `[rbp/x29 + offset + disp]`). The claims are part of the abstract state
   (`doc/trace_chain_joins.md` §5). The frame's slot does not hold the
   value meanwhile, so step 4 is what keeps a deopt below such a block
   sound: the walk boxes the value back into the slot before the
   interpreter can read it.

## 6. Return-state narrowing across a deopt-able callee

A specialized callee's computed return state (for example
`Guarded::Class(Integer)`) is consumed by the caller's compiled
continuation. A callee that deopts resumes in the interpreter and may return
a value outside that class. The compiler used to widen the return state to
`Value` for any callee that *could* deopt (`frame_had_deopt`, PR #505).

Escalation makes that widening unnecessary, and it is gone:

* if the callee completes in compiled code, the inferred state was derived
  from exactly those paths, so it holds;
* if the callee deopts, the escalated walk converts the caller as well, so
  the compiled continuation that consumed the narrowed state never runs.

Two conditions apply:

* **Every deopt below a consumer of a narrowed return state must
  escalate.** This is enforced mechanically, not by review: the escalation
  count is stamped onto the `AsmIr` and every side-exit constructor reads
  it (§8.5).
* **`has_exception_handler` still widens.** An exception raised and rescued
  *inside* the callee returns normally without a deopt, so the walk never
  fires and the happy-path inference would be wrong (issue #405).
  `taint_for_unmodeled_rescue` (`state.rs`) does this widening.

## 7. Why unboxed values stay out of the slots

`Lfp::mark` walks the frame's register slots and marks each as a `Value`,
so a raw `f64` in a local slot would be scanned as a heap pointer. An
unboxed outer local therefore lives in the owning frame's FP save / spill
area, and the specialized block is pointed there (§5 step 5). Anything that
reads the slot expecting a boxed `Value` (a captured `Proc`, `binding`,
generic block invocation, moving the frame to the heap, the interpreter
after a deopt) must see the value boxed back first, which is what the walk
and the capture conversion (§1) guarantee.

## 8. Implementation

### 8.1 The per-site conversion stub

At compile time each site's `ChainReplay` (`jitgen.rs`: the site's
`WriteBack`, the frame's spill `base`, the site's `UsingFpr`, the call's
`dst` slot, and the call-site pc) is lowered by `Codegen::gen_chain_replay_stub`
into a compiled conversion stub. The walk calls the stub with three frame
pointers (callee bp, caller bp, caller `Lfp`), and the stub writes the
suspended caller frame back.

The layout facts the replay depends on:

* Every frame (VM, JIT, native wrapper) establishes `bp == cfp + BP_CFP` in
  its prologue (`Cfp::frame_bp`), so `callee_rbp = callee_cfp + 8` and the
  caller's saved bp is `[callee_rbp]`.
* Pool-resident floats live across the call in the cont-mode save area
  (`fpr_save_with_cont` / `emit_fpr_save`), allocated `rsp`-relative at the
  call: one 8-byte slot per set bit of the site's `UsingFpr`, in bit order,
  at `[rsp + 16 + 8i]`, which after the call and prologue is
  **`callee_rbp + 32 + 8i`**. The formula is the same on aarch64
  (`stp x29, x30` is the same 16-byte adjustment as `call` + `pushq rbp`),
  so one arch-neutral replay serves both.
* Spilled `FPReg`s (ids `>= PHYS_FPR_POOL`) live `base`-relative in the
  caller frame, at `[caller_rbp - (base - 24 + 8n)]` (`PhysMap::resolve`).
* The caller's LFP is read through `caller_cfp.lfp()`, which follows a frame
  moved to the heap, as the emitted deopt write-back's LFP register does.

`virtual_obj` / `forward_kwrest` call `runtime::create_array` (or
`value_deep_copy` for a literal template) and `runtime::correct_rest_kw`,
the same helpers and addressing that the deopt path's
`gen_virtual_materialize` / `gen_forward_kwrest_materialize` use. They run
after the fpr / literal / void stores, so every deferred `dst` slot already
holds `nil` and the frame stays GC-consistent across the allocating calls:
slots always hold whole, valid (possibly stale) `Value`s.

The stub reaches nothing on `Codegen`, so the walk applies each conversion
in place, under the `CODEGEN` borrow it already holds. Frames are converted
in walk order; the replays touch disjoint frames.

### 8.2 The shared continuation stub

The rewritten slots point at **one** stub per arch (`gen_chain_cont_stub`,
emitted with the VM handlers, address in `Codegen::chain_cont_stub`). Its
per-site input is a continuation word that the site's conversion stub stores
into the callee's **cont-frame pad slot** (CFP+32, the second half of the
16-byte cont frame, which nothing reads on the normal return path). The word
packs `conv(dst)` (0 = none) in the high 32 bits and the byte advance to the
next instruction (`pc.next() - pc`: 16 or 32) in the low 32 bits, so 1-unit
operator sites resume correctly.

Entered by `ret`, the stub:

1. runs the `pop_frame` the hijacked `ret` skipped, derived from
   `rbp`/`x29`, restoring `Executor::cfp` and the LFP register;
2. reads the call-site pc from the cont frame's pc slot and the
   continuation word from the pad, then drops the cont frame;
3. on the error signal (result 0), hands the call-site pc to `entry_raise`
   under each arch's convention (x86-64 `pc + 1` before the `-16`, aarch64
   `pc` unchanged);
4. otherwise stores the result into `dst` (if any) and resumes the fetch
   loop at `pc + advance`.

The stub never allocates. The pool registers are dead at the stub, because
the replay consumed the save area.

### 8.3 Registration

Every call and yield site registers (`AbstractState::chain_exit` in
`jitgen/compile/method_call.rs`, lowered to `AsmInst::ChainExit` /
`LInst::ChainExit`, which emit no code). An unregistered site would be one
that BOP eviction cannot convert, and §6's return-state argument depends on
registration too. `tests/redefine.rs` (`redefine_bop_onstack_caller` and
others) checks the conversion positively: a suspended caller that resumed
its compiled body would return the stale inlined result.

### 8.4 The unwind path goes through the rewritten slot

An exception does not bypass a rewritten slot. `entry_raise` with no
in-frame handler unwinds by running the frame's epilogue and `ret`ing with
the error signal in `rax`/`x0`, so a propagating exception lands in the
continuation stub, whose error branch re-raises from the converted frame at
the call site (running its `rescue` / `ensure` there, or unwinding
further). Non-local exits (`MethodReturn`, `Break`) take the same route,
including `method_return_specialized`, whose `ret` lands in the stub through
the slot of the *outermost* inlined call, the one whose `dst` the value is
destined for.

### 8.5 The walk and escalation

`Codegen::chain_deopt_into(cfp, frames)` walks the CFP chain from `cfp`,
rewriting each suspended caller through the return address its callee
holds. `frames` bounds the walk (`None` walks the whole chain). Frames whose
return address is VM code or an invoker are skipped, but the walk continues
past them, since a chain may alternate VM and JIT frames. A frame converted
by an earlier walk is skipped the same way, because its rewritten return
address is the stub, a VM address. This matters: interpreted inner frames
may have updated its slots through `outer_lfp` since, and a second replay
would clobber them with stale floats.

Escalation is decided in one place. `JitContext::chain_deopt_frames` is the
frame's depth in its compilation unit; `AsmIr::new` stamps it onto the IR
(`AsmIr::chain_frames`), and every side-exit constructor (`new_deopt`,
`new_deopt_with_pc`, `deopt_from_point`, `new_recompile_deopt`, `new_error`)
reads it, so no emitter can forget. A `Deoptimize` / `RecompileDeoptimize`
/ `Error` handler with a non-zero count calls `runtime::chain_deopt(vm,
frames)` after its own write-back, before resuming the fetch loop or
entering `entry_raise`. The walk starts at the deopting frame, so its own
return-address slot is rewritten too, which converts its caller when the
now-interpreted frame returns.

A frame at depth 0 is its unit's root and does not escalate. Both things
escalation protects are confined to one unit: a narrowed return state is
applied only by `def_rax2acc_return` on the specialized path, and only a
specialized block inside the unit reads an outer frame's unboxed `Float`.
An unrewritten slot of a root frame that raises lands in the caller's
compiled post-call error check, the ordinary JIT error path.

`Error` exits escalate because a raise can be rescued in-frame (an
interpreter resume like any deopt) or unwind through the suspended callers
(§8.4). `Evict` handlers are never entered: the `AsmEvict` id survives only
as the key under which a call site's return address is recorded (§3.4).

BOP eviction (`Codegen::check_bop_redefine`) walks whole chains, because it
must convert frames across unit boundaries. A capture
(`Codegen::convert_for_capture`) walks the capturing call site's unit.

### 8.6 Where the code is

| Piece | Location |
|---|---|
| `ChainExitSpec` (compile time), `ChainReplay`, `gen_chain_replay_stub` (x86-64) | `jitgen.rs` (aarch64 form in `arch/aarch64/compile/mod.rs`) |
| `AsmInst::ChainExit` (registration, no code) | `jitgen/asmir.rs`; lowered in `jitgen/asmir/compile_shared.rs` |
| `LInst::ChainExit` | `jitgen/lir.rs` |
| Producers (call, specialized call, yield, specialized yield) | `AbstractState::chain_exit`, `jitgen/compile/method_call.rs` |
| Shared continuation stub | `gen_chain_cont_stub` (`arch/x86_64/vmgen.rs`, `arch/aarch64/vmgen.rs`), address in `Codegen::chain_cont_stub` |
| Deferred rest / kwrest materialization | `runtime::create_array`, `runtime::correct_rest_kw` (`codegen/runtime.rs`) |
| Runtime table, the walk | `chain_deopt_table`, `register_chain_exit`, `chain_deopt_into`, `convert_for_capture` (`codegen.rs`) |
| Escalation count and runtime entry | `JitContext::chain_deopt_frames` (`jitgen/context.rs`), `AsmIr::chain_frames` (`jitgen/asmir.rs`), `runtime::chain_deopt` (`codegen/runtime.rs`) |
| Frame readers the walk uses | `Cfp::return_addr`, `Cfp::frame_bp`, `Cfp::lfp` (`executor/frame.rs`); the slot writes are emitted into the replay stub |

## 9. Eager conversion

### 9.3 The whole chain is converted at deopt time

The walk replays every suspended frame's write-back *during* the walk and
points its return address at the shared continuation stub. A lazy form that
wrote each frame back only when control returned to it is not sound: until
then, anything reading an outer frame's locals through `outer_lfp` (a block
body, `binding`, a backtrace) would read stale slots, which matters as soon
as a frame's slots do not hold its locals (§5 step 5). The eager form also
needs no per-site handlers: the rewritten slot always points at the one
shared stub, and the per-site part is the conversion stub the walk calls.

## 10. Chain conversion replaced code patching

Before chain conversion existed, a basic-op redefinition dropped suspended
frames by patching each call's return continuation in the code with a jump
to that site's `Evict` handler. Chain conversion replaced that entirely:

* a compiled body still valid for future invocations survives, because no
  machine code is rewritten;
* aarch64 loses a self-modifying-code path and its writable / I-cache
  dance around every patched word;
* there is one walk to reason about instead of two that must agree.

### 10.1 The other threads' chains, and the frame at a poll

A basic-op redefinition is the one caller of the walk whose stale frames
are not all on the redefining thread's stack. Every other Ruby thread is
suspended under it (a kernel thread waiting for the GVL, `doc/threads.md`
§12), and each may hold compiled frames that inlined the operator just
replaced. `check_bop_redefine` therefore walks every chain: its own, then
the chain of each executor `scheduler::suspended_executors` returns (the
context a thread parked or yielded in, published by `park`, `pass` and
`without_gvl`, followed by its `parent_fiber` links, so a thread parked
inside a fiber contributes the fiber's frames and the resumer's). Those
frames are as quiescent as the caller's own.

The walk converts *callers*, through the return address their callee
holds, so a chain's innermost compiled frame is beyond it. On the
redefining thread that frame is the one running the `def`, and
`AsmInst::CheckBOP` after `MethodDef` handles it. On another thread the
innermost frame is whatever it let go from: a call site (`sleep`,
`Queue#pop`, `Thread.pass`, all of which push a frame whose return address
the walk finds) or a **safepoint poll** (a timeslice tick or a signal taken
at a `LoopStart` / method-entry poll), where there is no callee frame. That
frame leaves from the poll itself: `ISeqInfo::bop_evictions` counts
evictions per iseq, `executor::execute_gc` snapshots the polling frame's
count on entry and, if it moved while the frame was away, returns
`POLL_DEOPT` instead of `nil`; the compiled poll's cold path compares for it
and jumps to the poll's own deopt exit (`AsmInst::ExecGc::deopt`,
`AsmIr::new_poll_deopt`), resuming the interpreter at `LoopStart` or at the
instruction after `InitMethod`. That exit does not set `had_deopt`: by the
time it can fire, the walk has already rewritten every caller of the frame,
and marking it would put `had_deopt` on every body, since every method
entry polls. The same exit covers the redefining thread's own poll when a
trap handler or a finalizer run from the poll does the redefinition.

A fiber that is *suspended* (not parked) inside one of those threads stays
out of reach: it is in no chain the walk sees, and it polls nothing until
resumed. Regression tests: `builtins::thread::tests::bop_redefinition_reaches_*`
(parked, parked in a fiber, yielded through `Thread.pass`, preempted at a
poll).
