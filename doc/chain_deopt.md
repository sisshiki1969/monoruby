# Chain deopt — handoff note

**Kind:** plan. **Status:** the *mechanism* (§5 steps 1–3) is implemented and
exercised end-to-end, and the **escalation half of step 4** — the per-frame
switch that makes every interpreter-resuming side exit run the chain-deopt
walk, plus the runtime entry it calls — is in place (§8.6). The speculation
itself (the `Float` guard and the `locals_to_S` relaxation, §5 steps 4–5) and
the return-state recovery (§6) are not. §8 records what was built.

**⚠ Before starting §5 step 5, read §9.** The built mechanism writes the
suspended frames back **lazily** (each frame converts as its `ret` reaches
its chain-exit handler). That deviates from the intended design — the walk
is supposed to write every frame back **eagerly**, at deopt time — and the
deviation is masked only for as long as `locals_to_S` still boxes every
local before a block call. The speculation removes exactly that masking, so
the lazy form must be converted to eager *first*.

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

### 8.2 The handler carries its own continuation — §3.1's shared VM entry was wrong

§3.1 proposes tailing every handler into the VM's shared post-call
continuation, on the grounds that the sequence is entirely stack-driven and
carries no per-site state. That was **built first and then torn out**, for a
reason §3.1 missed: the shared continuation re-derives the destination slot
and the resume pc from the bytecode **assuming a 2-unit send instruction**
(`movzxw rdi, [r13 + 4]`; `addq r13, 32`; on error `entry_raise`'s
`r13 - 16` lands on the send's second unit, which the exception table still
covers *for sends*). But operator call sites — `BinOp` / `Index` / …,
**1-unit** bytecodes — dispatch through the same `send` emitter and get
chain-exit handlers too. At such a site the shared continuation reads a
garbage destination slot, resumes one whole instruction too far, and on a
propagating exception looks up the exception table at the *following*
instruction — which silently skips an `ensure` whose protected range ends at
the operator (found by `Enumerator.new { |y| begin; y << 1; ensure; … }`
with a raising consumer block, once every error exit escalated).

So the handler now runs its **own per-site continuation** — it statically
knows the call's `dst` slot and site pc: drop the cont frame, test the
result; store into `dst` and resume the fetch loop at `pc.next()` (which is
per-opcode-size correct), or on the error signal hand the *call-site* pc to
`entry_raise` under each arch's convention (x86 `pc + 1` before the `-16`,
aarch64 `pc` unchanged). The `Codegen::vm_call_continuation` publication has
been removed. The cont-frame pc slot is still written at every site
(`Kernel#caller` reads it) but the handler no longer consumes it.

A hijacked `ret` still skips the JIT frame's own `pop_frame`, so the handler
performs it first — before the write-back, whose boxing/materializing calls
can GC, and the mark walk starts from `Executor::cfp`.

### 8.3 How it is exercised without a firing point

Steps 4–5's speculation does not exist yet, so nothing in a normal build
calls `Codegen::chain_deopt`. Under the `chain-deopt` cargo feature
(default-off) two things exercise it:

* **BOP eviction** converts by chain instead of by patching: the walks are
  observationally equivalent — both convert every suspended JIT frame into
  an interpreter frame — and `tests/redefine.rs` already covers exactly this
  shape: `redefine_bop_onstack_caller` fails with the stale inlined `+`
  (8 instead of 999) if the suspended caller resumes its compiled body, so
  the test passing under the feature is a positive signal that the
  conversion happened, not just that nothing crashed.
* **Every side exit escalates** (§8.6): each deopt / recompile-deopt / error
  exit taken anywhere in the suite fires the walk from the deopting frame,
  so the deopt → walk → handler-cascade path — the one the speculation will
  actually take — is exercised at every deopt site the suite reaches, not
  only at BOP redefinitions.

What this validates: the walk, the return-address rewrite, the handler's
frame/LFP/FP-pool restore, the write-back, and the hand-off to the VM
continuation, across normal calls, generic yields, and specialized
calls/yields. What it does **not** validate is the case chain deopt exists
for — a frame whose local is *unboxed at the moment of conversion* — because
without the `locals_to_S` relaxation no suspended frame ever holds one.

The producers (`AbstractState::chain_exit`) are gated on the same feature, so
a default build emits no chain-exit handlers at all and pays nothing. When
step 5 lands, that gate becomes the per-site decision §6 argues for rather
than a build-wide switch.

### 8.4 A rewritten return-address slot is also on the unwind path

Worth knowing before building step 4, because it is not obvious: an
exception does **not** bypass the rewritten slot. `entry_raise` with no
in-frame handler unwinds by running the frame's ordinary epilogue and
`ret`ing with the error signal in `rax`/`x0` — so a propagating exception
lands in the chain-exit handler too, and the handler's error branch (§8.2)
hands the call-site pc to `entry_raise`, which re-raises from the
now-converted frame at the call site — running its `rescue`/`ensure` there
or unwinding further. Non-local exits (`MethodReturn`, `Break`) take the
same route, including `method_return_specialized`, which lands in the
handler belonging to the *outermost* inlined call — the one whose `dst` the
value is destined for.

This is what we want (the frame is converted before the VM inspects it for
a `rescue`), and it means the handler must be correct for `rax == 0`, not
only for a normal return. It is: the write-back does not read `rax`, and the
stash/restore preserves the zero.

### 8.5 Escalating side exits (§5 step 4's mechanism, §6's mechanical guarantee)

A frame compiled under the speculation must convert its suspended callers on
**every** path that resumes the interpreter in-frame, not just the `Float`
guard on the offending store: once the interpreter runs any of the frame's
bytecode it can reach an outer local through `Load/StoreDynVar`, a `binding`,
or a capture, and would read the stale slot. §6 asks for a mechanical
guarantee rather than review discipline, and this is it:

* `JitContext::escalate_side_exits` is the single decision point. It is
  stamped onto each `AsmIr` at construction, and **every** side-exit
  constructor (`new_deopt` / `new_deopt_with_pc` / `deopt_from_point` /
  `new_recompile_deopt` / `new_error`) reads it — an emitter cannot forget
  to escalate because emitters do not choose. Today it returns
  `cfg!(feature = "chain-deopt")`; the per-frame speculation flag ORs in
  here when step 5 lands.
* An escalated `Deoptimize` / `RecompileDeoptimize` / `Error` handler calls
  `runtime::chain_deopt(vm)` **after** its write-back (the frame is fully
  homed, and the walk itself allocates nothing) and before resuming the
  fetch loop / entering `entry_raise`. The walk starts at the deopting
  frame's own cfp, so its own return-address slot is rewritten too — that is
  what converts its caller when the now-interpreted frame eventually
  returns.
* `Error` exits escalate because a raise can be rescued *in-frame* (an
  interpreter resume like any deopt) or unwind through the suspended callers
  — §8.4's path, which requires the slots to have been rewritten.
* `Evict` handlers do **not** escalate: they are only entered through the
  chain-wide eviction walk below, which has already converted (or patched)
  every suspended frame in one pass.

The walk stops converting where the table stops: an unregistered return
address marks the boundary of the compilation region that speculated, and
frames beyond it hold no unboxed cross-frame state, so they may resume their
compiled bodies. (Under the validation feature every site registers, so the
walk converts everything — strictly more conversion than needed, which is
sound for the same reason BOP eviction is.)

BOP eviction itself now goes through one unified walk
(`Codegen::evict_suspended_frames`): per suspended frame it prefers the
site's chain-exit handler (rewrite the return-address slot, leave the code
untouched) and falls back to the recorded patch point (`jmp deopt` written
into the code). A default build has no chain entries and degenerates to pure
patching; the feature build is pure chain deopt; with per-site speculation
the two coexist in one chain.

### 8.6 Where the code is

| Piece | Location |
|---|---|
| `AsmChain`, `SideExit::ChainExit`, `AsmInst::ChainExit`, `new_chain_exit` | `jitgen/asmir.rs` |
| `LSideExitKind::ChainExit`, `LInst::ChainExit` | `jitgen/lir.rs` |
| Producers (call / specialized call / yield / specialized yield) | `AbstractState::chain_exit`, `jitgen/compile/method_call.rs` |
| Handler emission | `gen_chain_exit_with_label` (`jitgen.rs`), `a64_gen_chain_exit` (`arch/aarch64/compile/mod.rs`) |
| FP-pool reload without popping | `fpr_reload_cont` / `a64_fpr_reload_cont` |
| Runtime table, the walks | `chain_deopt_table`, `register_chain_exit`, `chain_deopt`, `evict_suspended_frames` (`codegen.rs`) |
| Escalation switch + runtime entry | `JitContext::escalate_side_exits` (`jitgen/context.rs`), `AsmIr::escalate_exits` (`jitgen/asmir.rs`), `runtime::chain_deopt` (`codegen/runtime.rs`) |
| Return-address writer | `Cfp::set_return_addr` (`executor/frame.rs`) |

## 9. The lazy write-back is a deviation — make it eager before step 5

### 9.1 What the built mechanism actually does

`Codegen::chain_deopt` (`codegen.rs`) rewrites each suspended frame's
return-address slot to that site's chain-exit handler — **and nothing
else**. The frame's write-back runs only when control returns to it: the
innermost frame `ret`s, lands in the handler, the handler writes that one
frame back and tails into the VM continuation; then the next frame out, and
so on. Only the frame that *raised* the deopt is written back eagerly (by
its own escalated side exit, before it calls `runtime::chain_deopt`).

So immediately after a deopt in `A → B → C` at `C`:

| frame | local slots |
|---|---|
| C | correct (its own side exit wrote them) |
| B, A | **stale** — write-back deferred to their `ret`s |

### 9.2 Why that is wrong, and why nothing catches it today

The intended design (§2) is that the walk converts the whole chain at deopt
time: **all** frames written back, return addresses pointed at the **VM's
shared post-call continuation** — control never re-enters JIT code, and no
per-site handler exists at all. (Note the tension in the built form: a
per-site handler is only reachable *because* the return address points at
it rather than at the VM — the lazy write-back and the per-site handlers
are two faces of the same deviation.)

The soundness gap: once `C` is running in the interpreter, anything that
reads an outer frame's locals through `outer_lfp` — a block body touching
`A`'s variables, `binding`, a backtrace with argument values — reads `A`'s
**slots**, which still hold whatever was there before `A`'s floats were
promoted to `LinkMode::F`. Today this cannot be observed solely because
`locals_to_S` boxes every local into its slot before *every* block-passing
call, so every frame on an `outer` chain happens to have correct slots.
§5 step 5 removes exactly that guarantee; the lazy form breaks the moment
it lands.

The `chain-deopt` feature suite (3203/3203 green) is therefore a regression
base for the *conversion machinery* — stack rewriting, `ret`-entry handler
state, unwind interaction — not evidence that lazy write-back is sound.

### 9.3 The conversion

1. Extend the walk to replay each suspended frame's write-back **during**
   `chain_deopt`, then point its return address at the shared VM entry
   (§3.1 / §8.2's corrected form of it).
2. The replay needs, per return address: the `WriteBack`, the frame's spill
   `base`, and the site's `UsingFpr` save-area layout. §8.1 already
   recorded the layout facts: pool registers live `rsp`-relative at the
   call (`callee_rbp + 32 + 8i`, one slot per set bit of `UsingFpr`, bit
   order); spilled `FPReg`s (`>= PHYS_FPR_POOL`) are `base`-relative.
   `forward_rest` / `forward_kwrest` can call the existing `create_array` /
   `correct_rest_kw` from Rust — GC-safe because every source value is in a
   scanned frame slot.
3. Remove the per-site chain-exit handlers and the `DestLabel` table —
   `chain_deopt_table` maps `return_addr` to the replay data instead.
   This also deletes the §8.2/§8.4 complexity (handler continuations, the
   unwind-path interaction) that existed only to serve `ret`-entry.
4. Re-run the `chain-deopt` feature suite; it must stay green through the
   conversion.

Ordering: this precedes §5 steps 4–5. The firing point (step 4's guard) can
land together with it, but the `locals_to_S` relaxation must not land before
it.
