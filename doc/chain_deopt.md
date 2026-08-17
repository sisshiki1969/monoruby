# Chain deopt — handoff note

**Kind:** plan. **Status:** nothing implemented; this records what was
verified, what was tried and rejected, and the order the work should go in.

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
