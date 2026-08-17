# Chain deopt — handoff note

**Kind:** plan. **Status:** the *mechanism* (§5 steps 1–3) is implemented and
exercised end-to-end; the speculation it exists for (§5 steps 4–5) and the
return-state recovery (§6) are not. §8 records what was built, and the one
place the built thing deliberately differs from the design below.

---

## 1. What this is for

The optimization we want is **speculative unboxed `Float` locals across a
block call**.

Today a method call that passes a block demotes every local to
`LinkMode::S` before the call:

```rust
// compile/method_call.rs
// We must write back all local vars to the stack and set the state to
// LinkMode::S when they are possibly accessed or captured from inner blocks.
if callsite.block_fid.is_some() {
    state.locals_to_S(ir);
}
```

so a `Float` local is boxed on every block call, because the inner block
might read or write it. The speculation is to **stop demoting**: keep the
local unboxed in the frame's FP spill area and let the (specialized,
non-capturable) block read and write it there. If the block ever stores a
non-`Float` into such a local the speculation is dead — the outer frame's
compiled code assumes an `f64` at a spill offset that no longer holds one —
and the whole suspended chain has to fall back to the interpreter at once.

The motivating shape is real and hot. `benchmark/app_aobench.rb`:

```ruby
occlusion = 0.0
nphi.times do |j|
  ntheta.times do |i|
    ...
    occlusion += 1.0     # outer Float local, mutated from two blocks deep
  end
end
```

**Chain deopt is the enabling mechanism, not the optimization.** It also
pays for itself a second time — see §6.

## 2. The mechanism

On a deep deopt, leave every frame where it is and convert the whole
suspended chain from JIT frames into interpreter frames:

1. for every JIT frame on the stack, write its spilled xmm/d registers back
   into its local slots (boxing the floats);
2. rewrite each frame's **return-address slot on the stack** to the VM's
   post-call continuation;
3. the continuation reads the suspended pc off the stack, so that slot has
   to be right (it is — see §3.2).

Control then never re-enters JIT code: each `ret` lands in the interpreter.

This is **not** what `Codegen::immediate_eviction` does. That one writes a
`jmp deopt` into the *code* at the call's return continuation, so control
returns into JIT code and deopts one instruction later. Chain deopt does not
touch code and does not return into JIT code at all.

## 3. What was verified in the source

### 3.1 The VM entry already exists and is shared

`arch/x86_64/vmgen/method_call.rs:81`, the send opcode's post-call sequence:

```asm
done:
  pop_cont_frame     ; popq r13 (suspended pc); addq rsp, 8
                     ; movzxw rdi, [r13 + RET_REG_FROM_CALLSITE]  <- dst slot, from the bytecode
                     ; addq r13, 32                               <- past the 2-unit send
  vm_handle_error
  vm_store_rdi(rax)  ; store the return value into dst
  fetch_and_dispatch
```

It is **entirely stack-driven and carries no per-site state**: it recovers
the pc from the stack, re-derives the destination slot from the bytecode at
that pc, stores `rax`, and resumes. So a *single* address serves every call
site. Bind a label there and expose its address.

### 3.2 The suspended pc is already correct — no prerequisite work

`AsmInst::ContFramePc { call_site_pc }` (`asmir.rs:1678`) stores the
call-site pc into the outgoing cont-frame slot — documented as "the slot
`Kernel#caller` reads". Its producers cover every JIT call/yield emitter:

| `compile/method_call.rs` | emitter | path |
|---|---|---|
| 822 | `compile_yield_specialized` | specialized yield |
| 1419 | `send` | ordinary call |
| 1484 | `send_specialized` | specialized call |
| 1520 | `compile_yield` | generic yield |

`Cfp::caller_pc_slot`'s "not every dispatch path writes it" caveat is about
non-JIT paths (the invoker's zero sentinel). Filling the pc lazily during
the walk remains available as belt-and-braces, but is not needed to start.

### 3.3 The write-back is heavier than a memcpy

`WriteBack` (`jitgen.rs:175`) has six kinds of entry:

```rust
fpr: Vec<(FPReg, Vec<SlotId>)>,                            // float -> one or more slots
literal: Vec<(Value, SlotId)>,
void: Vec<SlotId>,
gp: Vec<(GP, SlotId)>,                                     // empty in shipping builds
forward_rest: Vec<(SlotId, SlotId, u16)>,                  // D1: build the rest Array
forward_kwrest: Vec<(SlotId, Box<[(IdentId, SlotId)]>)>,   // K1: build the kwrest Hash
```

To replay one from Rust the walker needs the `WriteBack` **and the frame's
spill base** (`base`, today only a compile-time argument to
`gen_write_back_for_deopt`). `forward_rest` / `forward_kwrest` additionally
need `create_array` / `correct_rest_kw` calls, so this is not a pure memory
shuffle.

Why replaying from Rust is possible at all: xmm is caller-saved, so
`FprSave` has already spilled every live float of a **suspended** frame into
that frame's own spill area. Only the innermost frame still has live values
in registers, and that one deopts through its compiled handler as it does
today.

### 3.4 The registration key already exists

`return_addr_table` (`codegen.rs`) maps a suspended call's return address to
its side exit, filled by `set_deopt_with_return_addr` from all four emitters
above. The runtime table wants the same key.

## 4. A rejected attempt — do not repeat it

Rewriting the stack's return address to point at that frame's **`Evict`
handler** looks like it should work. It does not, and it fails loudly
(`caller_lines_survive_specialized_frame_eviction`, SIGABRT inside
`pmc_record_binary`).

The instruction order at a specialized call is:

```
call callee            <- the return address points here
<result-store code emitted by def_rax2acc_return>
patch_point            <- where immediate_eviction writes its jmp
```

and `compile/method_call.rs:835` fixes the contract:

```rust
let res = state.def_rax2acc_return(ir, dst, return_state);  // emits the result store
state.immediate_evict(ir, evict);                            // records patch_point + write_back
```

The `Evict` write-back is captured **after** the result store is modelled,
so the handler assumes that store has already run. Entering it directly by
`ret` skips the store, the callee's return value never reaches its slot, and
garbage propagates.

`immediate_eviction` patches at `patch_point` — *after* the result store —
precisely for this reason. Chain deopt sidesteps the whole issue by
targeting the VM entry (§3.1), where `vm_store_rdi` does the store.

## 5. Order of work

Steps 1–3 are done — see §8, and note that step 1 was built differently
(and much more cheaply) than written here.

1. **Runtime table** `return_addr -> (WriteBack, spill base)`, plus the Rust
   routine that replays one against a frame's `rbp`/`lfp`.
2. **Expose the VM entry**: bind a label at `done:` and publish its address.
3. **The walk**: `Cfp::set_return_addr` (a writer next to the existing
   `return_addr()` at `frame.rs:76`), then walk the CFP chain applying 1 and
   2. Model the loop on `immediate_eviction`, which already skips VM frames
   via `check_vm_address` and handles recursion by visiting every frame.
4. **Firing point**: the `Float` guard on a block's `StoreDynVar` into an
   outer unboxed local. Write back *before* performing the offending store,
   or the write-back overwrites it.
5. **Relax `locals_to_S`** for qualifying block calls, and point the block's
   `Load/StoreDynVar` at the outer frame's spill area.
6. **Recover the return-state narrowing** — see §6.

## 6. The second payoff: `frame_had_deopt`

`compile/method_call.rs:1243` currently gives up all return-type inference
for any callee that *could* deopt:

```rust
if self.store[iseq_id].has_exception_handler() || frame_had_deopt {
    s.taint_for_unmodeled_rescue();     // ret -> ReturnValue::Value
}
```

and propagates the fact one level out (`current_frame_mut().had_deopt = true`),
so one deopt-able site anywhere in an inlined subtree flattens every
enclosing return state to `Value`. This is PR #505's fix for a real
unsoundness: a deopted callee resumes in the interpreter and can return a
value outside the class the abstract interpreter predicted.

Chain deopt licenses removing the `frame_had_deopt` half:

* callee completes in JIT — the inference was derived from exactly those
  compiled paths, so it holds;
* callee deopts — the caller is converted too, its compiled code never
  resumes, and the `Guarded::Class(..)` tag is never acted on.

Two conditions on that argument:

* **Every deopt below a site that consumed a narrowed return state must
  escalate to chain deopt.** Blanket escalation would make today's cheap
  per-frame deopts pay a chain walk and throw away the caller's compiled
  execution, so gate it per site. Specialization compiles the callee body
  *per call site*, so the flag can be baked in at compile time. This is the
  quietest thing in the design to get wrong — a missed escalation shows up
  only as a wrong class tag — so it wants a mechanical guarantee (a frame
  flag every side-exit emitter in that frame consults), not review
  discipline.
* **`has_exception_handler` stays.** An exception raised and rescued *inside*
  the callee returns normally with no deopt at all, so chain deopt never
  fires and the happy-path-only inference is still wrong (issue #405).

Note also that `taint_for_unmodeled_rescue` (`state.rs:598`) bundles the
`ret` downgrade with clearing `invariants.side_effect_guard`; splitting the
deopt reason from the rescue reason means deciding both, and the
`side_effect_guard` half needs its own written argument.

## 7. Gating for the speculation itself (§5.4–5.5)

Agreed scope: **specialized `iseq_block` only, and only where the frame
cannot be captured** (`possibly_capture_without_block` / `has_block_arg`
false). Generic block invocation is out of scope for now.

The constraints that force this:

* **GC.** `Lfp::mark` (`frame.rs:284`) walks `meta.regs()` and marks every
  slot as a `Value`. A raw `f64` in a local slot would be scanned as a heap
  pointer, so unboxed values must stay in the FP spill area and the block
  must be pointed *there*, not at `[outer_lfp - slot]`.
* **Escape paths.** Anything that reads `[outer_lfp - slot]` expecting a
  boxed `Value` breaks the speculation: a captured `Proc` called later,
  `binding` / `Binding#local_variable_get`, generic (non-specialized) block
  invocation, `move_frame_to_heap`, backtraces — and the interpreter itself
  once *the block* deopts and the VM runs the block's body.

## 8. What is implemented (§5 steps 1–3)

### 8.1 The write-back is replayed by emitted code, not from Rust

Step 1 above asks for a runtime table `return_addr -> (WriteBack, spill
base)` and a Rust routine that replays a `WriteBack` against a frame. That
is **not** what was built, for two reasons found while building it.

First, §3.3's premise is wrong in an important detail. It says `FprSave` has
already spilled a suspended frame's live floats "into that frame's own spill
area" — the `base`-relative region a Rust replay could read. It has not:
`fpr_save_with_cont` saves the *pool-resident* registers to an `rsp`-relative
area it allocates at the call (`[rsp + 16 + 8i]`, one slot per set bit of
that site's `UsingFpr`, in bit order). Only the already-*spilled* `FPReg`s —
ids `>= PHYS_FPR_POOL`, the minority — are `base`-relative. The values are
still recoverable (the save area sits at `callee_rbp + 32 + 8i`, since
`callee_rbp == rsp_after_FprSave - 16`), but a Rust replay would have to
carry each site's `UsingFpr`, reproduce that layout, and do it again for
aarch64's differently-shaped `d`-register area.

Second, the replay is not a memory shuffle even after that: `forward_rest` /
`forward_kwrest` (§3.3) mean re-implementing `create_array` /
`correct_rest_kw` argument marshalling in Rust, against the same two frame
layouts.

So instead each call site gets a **chain-exit handler** on the cold page —
`SideExit::ChainExit` / `LSideExitKind::ChainExit`, emitted by
`gen_chain_exit_with_label` (x86) and `a64_gen_chain_exit` (aarch64), and the
runtime table is `return_addr -> DestLabel` (`Codegen::chain_deopt_table`).
The handler reuses `gen_write_back_for_deopt` verbatim, so there is one
write-back implementation per arch rather than two, and no chance of the
Rust copy drifting from the emitted one. The cost is cold code per call
site, roughly doubling what the existing per-site `Evict` handler already
costs — which is why emission is gated for now (§8.3).

The handler is entered by `ret`, so it starts from the register state the
normal return continuation would see — `rbp` restored, `rax` holding the
callee's result, `r14` still the *callee's* LFP, `rsp` on the cont frame with
the `FprSave` area above it. It therefore:

1. runs the `pop_frame` the hijacked `ret` skipped — restoring
   `Executor::cfp` and `r14`. This has to be **first**, not merely before the
   VM hand-off: the write-back boxes floats and can materialize a deferred
   rest `Array`, both of which allocate, and the GC marks from
   `Executor::cfp`, which on entry still names the callee frame that has just
   been torn down;
2. reloads the FP pool from the save area **without** popping it
   (`fpr_reload_cont`) — `rsp` has to stay on the cont frame;
3. stashes `rax` in the callee's dead frame header (a 16-byte adjustment, so
   the boxing calls in the write-back stay 16-byte aligned);
4. runs `gen_write_back_for_deopt`;
5. restores `rax` and jumps to the VM's post-call continuation (which repeats
   the `pop_frame`; it is idempotent).

### 8.2 The VM entry is one instruction earlier than §3.1 says

§3.1 names `done:` as the entry to bind. `done:` sits *after*
`vm_call`'s trailing `pop_frame`, and a hijacked `ret` skips the JIT frame's
own `pop_frame` — so a frame resumed at `done:` would run with
`Executor::cfp` and `r14` still pointing at the callee. The published address
(`Codegen::vm_call_continuation`) is therefore the return address of
`call_funcdata`'s `call`, one step earlier, so the VM's own `pop_frame` runs.
Everything else in §3.1 holds: the sequence is entirely stack-driven, so the
one address serves every send and yield site, on both arches
(x86 `vm_send`, aarch64 `a64_op_send` just past its `blr`).

### 8.3 How it is exercised without a firing point

Steps 4–5 do not exist yet, so nothing in a normal build calls
`Codegen::chain_deopt`. The `chain-deopt` cargo feature (default-off) routes
`check_bop_redefine` through the walk instead of `immediate_eviction`. The
two are observationally equivalent — both convert every suspended JIT frame
into an interpreter frame — and `tests/redefine.rs` already covers exactly
this shape: `redefine_bop_onstack_caller` fails with the stale inlined `+`
(8 instead of 999) if the suspended caller resumes its compiled body, so the
test passing under the feature is a positive signal that the conversion
happened, not just that nothing crashed.

What this validates: the walk, the return-address rewrite, the handler's
frame/LFP/FP-pool restore, the write-back, and the hand-off to the VM
continuation, across normal calls, generic yields, and specialized
calls/yields. What it does **not** validate is the case chain deopt exists
for — a frame whose local is *unboxed at the moment of conversion* — because
under BOP eviction the innermost frame still deopts through its own handler
first.

The producers (`AbstractState::chain_exit`) are gated on the same feature, so
a default build emits no chain-exit handlers at all and pays nothing. When
step 4 lands, that gate becomes the per-site decision §6 argues for rather
than a build-wide switch.

### 8.4 A rewritten return-address slot is also on the unwind path

Worth knowing before building step 4, because it is not obvious: an
exception does **not** bypass the rewritten slot. `entry_raise` with no
in-frame handler unwinds by running the frame's ordinary epilogue and
`ret`ing with the error signal in `rax`/`x0` — so a propagating exception
lands in the chain-exit handler too, and the handler's tail hands it to the
VM continuation's `vm_handle_error`, which re-raises from the now-converted
frame at the call site. Non-local exits (`MethodReturn`, `Break`) take the
same route, including `method_return_specialized`, which lands in the
handler belonging to the *outermost* inlined call — the one whose `dst` the
value is destined for.

This is what we want (the frame is converted before the VM inspects it for
a `rescue`), and it means the handler must be correct for `rax == 0`, not
only for a normal return. It is: the write-back does not read `rax`, and the
stash/restore preserves the zero.

### 8.5 Where the code is

| Piece | Location |
|---|---|
| `AsmChain`, `SideExit::ChainExit`, `AsmInst::ChainExit`, `new_chain_exit` | `jitgen/asmir.rs` |
| `LSideExitKind::ChainExit`, `LInst::ChainExit` | `jitgen/lir.rs` |
| Producers (call / specialized call / yield / specialized yield) | `AbstractState::chain_exit`, `jitgen/compile/method_call.rs` |
| Handler emission | `gen_chain_exit_with_label` (`jitgen.rs`), `a64_gen_chain_exit` (`arch/aarch64/compile/mod.rs`) |
| FP-pool reload without popping | `fpr_reload_cont` / `a64_fpr_reload_cont` |
| Runtime table, VM entry, the walk | `chain_deopt_table`, `vm_call_continuation`, `register_chain_exit`, `chain_deopt` (`codegen.rs`) |
| Return-address writer | `Cfp::set_return_addr` (`executor/frame.rs`) |
