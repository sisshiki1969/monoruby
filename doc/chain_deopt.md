# Chain deopt — handoff note

**Kind:** plan. **Status:** the *mechanism* (§5 steps 1–3) is implemented and
exercised end-to-end **in its eager form** — the walk replays every suspended
frame's write-back at deopt time and points its return-address slot at one
shared VM continuation stub (§9.3's conversion has landed; §8 describes the
implementation). Registration is unconditional in every build, and chain
conversion is now the **only** way an on-stack JIT frame is dropped to the
interpreter: immediate eviction — the code-patching mechanism this document
was written against — is gone (§10). The **escalation half of step 4** — the
per-frame switch that makes every interpreter-resuming side exit run the
chain-deopt walk, plus the runtime entry it calls — is in place (§8.6). The
speculation itself (the `Float` guard and the `locals_to_S` relaxation, §5
steps 4–5) and the return-state recovery (§6) are not. §9 stays as the record
of why the original lazy build was wrong.

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

This is **not** what `Codegen::immediate_eviction` did. That one wrote a
`jmp deopt` into the *code* at the call's return continuation, so control
returned into JIT code and deopted one instruction later. Chain deopt does not
touch code and does not return into JIT code at all — which is why it could
replace immediate eviction outright rather than sit beside it (§10).

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
patch_point            <- where immediate_eviction wrote its jmp
```

and `compile/method_call.rs:835` fixed the contract:

```rust
let res = state.def_rax2acc_return(ir, dst, return_state);  // emits the result store
state.immediate_evict(ir, evict);                            // records patch_point + write_back
```

The `Evict` write-back is captured **after** the result store is modelled,
so the handler assumes that store has already run. Entering it directly by
`ret` skips the store, the callee's return value never reaches its slot, and
garbage propagates.

`immediate_eviction` patched at `patch_point` — *after* the result store —
precisely for this reason. Chain deopt sidesteps the whole issue by
targeting the VM entry (§3.1), where `vm_store_rdi` does the store. (The
argument is preserved because it is the reason the *stub*, not the frame's
own `Evict` handler, is the conversion target; the patching mechanism it
describes no longer exists — §10.)

## 5. Order of work

Steps 1–3 are done, essentially as written (after a detour through a lazy
per-site-handler form — §9) — see §8.

1. **Runtime table** `return_addr -> (WriteBack, spill base)`, plus the Rust
   routine that replays one against a frame's `rbp`/`lfp`.
2. **Expose the VM entry**: bind a label at `done:` and publish its address.
3. **The walk**: `Cfp::set_return_addr` (a writer next to the existing
   `return_addr()` at `frame.rs:76`), then walk the CFP chain applying 1 and
   2. Model the loop on `immediate_eviction`, which already skips VM frames
   via `check_vm_address` and handles recursion by visiting every frame.
   (The chain walk has since *become* that loop — §10.)
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

## 8. What is implemented (§5 steps 1–3, eager form)

### 8.1 The write-back is replayed from Rust, at deopt time

Step 1's runtime table exists as designed: `Codegen::chain_deopt_table` maps
a call's return address to a `ChainReplay` (`jitgen.rs`) carrying the site's
`WriteBack`, the frame's spill `base`, the site's `UsingFpr`, the call's
`dst` slot, and the call-site pc. The walk clones the entry and
`ChainReplay::replay` writes the suspended caller frame back **during the
walk**, in Rust.

Layout facts the replay depends on (each re-verified against the emission
code; they are load-bearing now):

* Every frame — VM, JIT, native wrapper — establishes `bp == cfp + BP_CFP`
  in its prologue (`Cfp::frame_bp`), so `callee_rbp = callee_cfp + 8` and
  the caller's saved bp is `[callee_rbp]`.
* §3.3's premise was wrong in one detail: `FprSave` does **not** spill the
  pool-resident floats into the frame's `base`-relative spill area. The
  cont-mode save (`fpr_save_with_cont` / `emit_fpr_save`) puts them in an
  `rsp`-relative area allocated at the call — one 8-byte slot per set bit of
  the site's `UsingFpr`, in bit order, at `[rsp + 16 + 8i]` — which after
  the call/prologue sits at **`callee_rbp + 32 + 8i`** (since
  `callee_rbp == rsp_after_FprSave - 16`). The formula is byte-identical on
  aarch64 (`emit_fpr_save` mirrors the x86 shape; `stp x29, x30` is the same
  16-byte adjustment as `call` + `pushq rbp`), so one arch-neutral replay
  serves both.
* Spilled `FPReg`s (ids `>= PHYS_FPR_POOL`) live `base`-relative in the
  caller frame: `[caller_rbp - (base - 24 + 8n)]` (`PhysMap::resolve`,
  shared by both arches).
* The caller's LFP is read through `caller_cfp.lfp()` — the cfp slot is
  redirected when a frame is promoted to the heap, exactly like the `r14`
  the emitted deopt write-back uses.

`forward_rest` / `forward_kwrest` call `runtime::create_array` /
`runtime::kwrest_hash` (the `correct_rest_kw` equivalent) from Rust, against
the dynamic caller's frame reached through the saved bp — the same
addressing the emitted `gen_forward_rest_materialize` /
`gen_forward_kwrest_materialize` use. They run after the fpr/literal/void
stores, so every deferred `dst` slot already holds its `nil` and the frame
stays GC-consistent across the allocating helper calls. GC safety overall:
slots always hold whole, valid (possibly stale) `Value`s, and the GC scans
them as such, so the replay may allocate at any point.

Because the replay allocates, it must not run under the `CODEGEN` borrow:
the walk (`Codegen::chain_deopt`) only **collects** a `Vec<ChainConversion>`
plan; the callers (`runtime::chain_deopt`, `Codegen::check_bop_redefine`)
apply it after the borrow is released. Write-back order within one frame is fixed (deferred
materialization last); frames are applied in walk order, which is sound
because the replays touch disjoint frames (rest/kwrest sources are raw
slots, valid regardless of conversion order).

### 8.2 One shared continuation stub — §3.1's raw VM entry is still wrong, the pad slot fixes it

§3.1 proposes pointing every rewritten return-address slot at the VM's
shared post-call continuation, on the grounds that the sequence is entirely
stack-driven and carries no per-site state. That is wrong for a reason §3.1
missed: the shared continuation re-derives the destination slot and the
resume pc from the bytecode **assuming a 2-unit send instruction**
(`movzxw rdi, [r13 + 4]`; `addq r13, 32`; on error `entry_raise`'s
`r13 - 16` lands on the send's second unit, which the exception table still
covers *for sends*). But operator call sites — `BinOp` / `Index` / …,
**1-unit** bytecodes — dispatch through the same `send` emitter and register
for chain deopt too. At such a site the raw VM continuation reads a garbage
destination slot, resumes one whole instruction too far, and on a
propagating exception looks up the exception table at the *following*
instruction — which silently skips an `ensure` whose protected range ends at
the operator (found by `Enumerator.new { |y| begin; y << 1; ensure; … }`
with a raising consumer block, once every error exit escalated).

The fix keeps §3.1's "one address serves every site" without decoding
bytecode: **one** stub per arch (`gen_chain_cont_stub`, emitted with the VM
handlers), plus a per-site continuation word the walk stores into the
callee's **cont-frame pad slot** (`Cfp::set_cont_frame_data`, CFP+32 — the
second half of the 16-byte cont frame, reserved by every caller and read by
nothing on the normal return path). The word packs `conv(dst)` (0 = none)
in the high 32 bits and the byte advance to the next instruction
(`pc.next() - pc`, per-opcode-size correct: 16 or 32) in the low 32 bits.

Entered by `ret` — the frame's write-back already replayed at deopt time —
the stub only has to:

1. run the `pop_frame` the hijacked `ret` skipped (`rbp`/`x29`-derived, so
   correct whatever the callee left in the global registers): restore
   `Executor::cfp` and the LFP register;
2. read the call-site pc from the cont frame's pc slot (`ContFramePc` wrote
   it at every JIT site; `Kernel#caller` reads the same slot) and the
   continuation word from the pad, then drop the cont frame;
3. on the error signal (result 0), hand the call-site pc to `entry_raise`
   under each arch's convention (x86 `pc + 1` before the `-16`, aarch64 `pc`
   unchanged);
4. otherwise store the result into `dst` (if any) and resume the fetch loop
   at `pc + advance`.

The stub never allocates, so no GC concern; the dead `FprSave` area above
the cont frame is simply left below the frame, as every side exit leaves
it. There are no per-site handlers, no `SideExit::ChainExit`, and no
`fpr_reload_cont` — the pool registers are dead at the stub (the replay
consumed the save area).

### 8.3 How it is exercised without a firing point

Steps 4–5's speculation does not exist yet, so the `Float` guard that will
fire the walk does not either. Two things exercise it meanwhile:

* **BOP eviction**, in *every* build: it is now the walk's only production
  caller, so `tests/redefine.rs` covers the mechanism unconditionally.
  `redefine_bop_onstack_caller` fails with the stale inlined `+` (8 instead
  of 999) if the suspended caller resumes its compiled body, so the test
  passing is a positive signal that the conversion happened, not just that
  nothing crashed.
* **Every side exit escalates** (§8.6), under the `chain-deopt` cargo
  feature (default-off): each deopt / recompile-deopt / error exit taken
  anywhere in the suite fires the walk from the deopting frame, so the
  deopt → replay → stub-return path — the one the speculation will actually
  take — is exercised at every deopt site the suite reaches, not only at BOP
  redefinitions.

What this validates: the walk, the eager replay, the return-address rewrite,
the stub's frame/LFP restore and result/raise hand-off, across normal calls,
generic yields, and specialized calls/yields. What it does **not** validate
is the case chain deopt exists for — a frame whose local is *unboxed at the
moment of conversion* — because without the `locals_to_S` relaxation no
suspended frame ever holds one.

The producers (`AbstractState::chain_exit`) are **not** gated: every call and
yield site registers in every build, because with immediate eviction gone a
site the table does not know is a site BOP eviction cannot convert (§10).
The metadata cost is accepted. When step 5 lands, the per-site decision §6
argues for rides on top of unconditional registration, not instead of it.

### 8.4 A rewritten return-address slot is also on the unwind path

Worth knowing before building step 4, because it is not obvious: an
exception does **not** bypass the rewritten slot. `entry_raise` with no
in-frame handler unwinds by running the frame's ordinary epilogue and
`ret`ing with the error signal in `rax`/`x0` — so a propagating exception
lands in the continuation stub too, and the stub's error branch (§8.2)
hands the call-site pc to `entry_raise`, which re-raises from the
now-converted frame at the call site — running its `rescue`/`ensure` there
or unwinding further. Non-local exits (`MethodReturn`, `Break`) take the
same route, including `method_return_specialized`, whose `ret` lands in the
stub via the slot belonging to the *outermost* inlined call — the one whose
`dst` the value is destined for.

This is what we want (the frame is converted before the VM inspects it for
a `rescue`), and it means the stub must be correct for `rax == 0`, not only
for a normal return. It is: the error branch touches neither `dst` nor the
advance, and the frame's write-back already ran at deopt time.

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
  homed) and before resuming the fetch loop / entering `entry_raise`. The
  runtime entry collects the plan under the `CODEGEN` borrow and applies it
  (replay + slot rewrite) after releasing it — the replay allocates (§8.1).
  The walk starts at the deopting frame's own cfp, so its own
  return-address slot is rewritten too — that is what converts its caller
  when the now-interpreted frame eventually returns.
* `Error` exits escalate because a raise can be rescued *in-frame* (an
  interpreter resume like any deopt) or unwind through the suspended callers
  — §8.4's path, which requires the slots to have been rewritten.
* `Evict` handlers do **not** escalate — and are in fact no longer entered
  at all: immediate eviction was their only entry (§10). The side-exit slot
  survives because `AsmEvict` is the id under which a call site's return
  address is recorded for `chain_exit`.

Every JIT call/yield site registers, so the walk converts every suspended
JIT frame it reaches — strictly more conversion than the speculation will
need, which is sound for the same reason BOP eviction is. A frame converted
by an earlier walk is skipped the same way a VM frame is — its rewritten
return address *is* a VM address (the stub) — which is load-bearing:
interpreted inner frames may have updated its slots through `outer_lfp`
since, and a second replay would clobber them with stale floats.

BOP eviction goes through the same walk (`Codegen::chain_deopt`) as an
escalated side exit; there is one CFP walk and one conversion mechanism.

### 8.6 Where the code is

| Piece | Location |
|---|---|
| `ChainExitSpec` (compile-time), `ChainReplay` + the Rust replay, `ChainConversion` | `jitgen.rs` |
| `AsmInst::ChainExit` (registration, no code emitted) | `jitgen/asmir.rs`; lowered in `jitgen/asmir/compile_shared.rs` |
| `LInst::ChainExit` | `jitgen/lir.rs` |
| Producers (call / specialized call / yield / specialized yield) | `AbstractState::chain_exit`, `jitgen/compile/method_call.rs` |
| Shared continuation stub | `gen_chain_cont_stub` (`arch/x86_64/vmgen.rs`, `arch/aarch64/vmgen.rs`), address in `Codegen::chain_cont_stub` |
| `kwrest_hash` (the `correct_rest_kw` equivalent for the replay) | `codegen/runtime.rs` |
| Runtime table, the walk | `chain_deopt_table`, `register_chain_exit`, `chain_deopt` (`codegen.rs`) |
| Escalation switch + runtime entry | `JitContext::escalate_side_exits` (`jitgen/context.rs`), `AsmIr::escalate_exits` (`jitgen/asmir.rs`), `runtime::chain_deopt` (`codegen/runtime.rs`) |
| Frame writers/readers | `Cfp::set_return_addr`, `Cfp::set_cont_frame_data`, `Cfp::frame_bp` (`executor/frame.rs`) |

## 9. The lazy write-back was a deviation — record of the eager conversion

**Resolved.** The mechanism was first built in a lazy, per-site-handler
form; this section records what that form did, why it was unsound to build
the speculation on, and the conversion plan — which has since landed (§8
describes the eager implementation). §9.1–9.2 describe the *former* build.

### 9.1 What the built mechanism actually did

`Codegen::chain_deopt` (`codegen.rs`) rewrote each suspended frame's
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

The `chain-deopt` feature suite (3203/3203 green) was therefore a regression
base for the *conversion machinery* — stack rewriting, `ret`-entry handler
state, unwind interaction — not evidence that lazy write-back was sound.

### 9.3 The conversion — landed

1. ~~Extend the walk to replay each suspended frame's write-back **during**
   `chain_deopt`, then point its return address at the shared VM entry.~~
   Done — with §8.2's correction: the raw VM send continuation is wrong for
   1-unit operator sites, so the rewritten slots point at one shared stub
   (`gen_chain_cont_stub`) that reads the per-site `dst`/advance word the
   walk stored in the cont-frame pad slot.
2. ~~The replay needs, per return address: the `WriteBack`, the frame's
   spill `base`, and the site's `UsingFpr` save-area layout.~~ Done —
   `ChainReplay` carries exactly that (plus `dst` and the site pc for the
   stub's continuation word); the layout facts are restated, verified, in
   §8.1. `forward_rest` / `forward_kwrest` call `runtime::create_array` /
   `runtime::kwrest_hash` from Rust — GC-safe because every source value is
   in a scanned frame slot.
3. ~~Remove the per-site chain-exit handlers and the `DestLabel` table —
   `chain_deopt_table` maps `return_addr` to the replay data instead.~~
   Done — `SideExit::ChainExit` / `LSideExitKind::ChainExit`,
   `gen_chain_exit_with_label` / `a64_gen_chain_exit`, and
   `fpr_reload_cont` / `a64_fpr_reload_cont` are deleted; §8.4's unwind
   path now terminates in the stub's error branch.
4. ~~Re-run the `chain-deopt` feature suite; it must stay green through the
   conversion.~~ Done — 3203/3203 green in the eager form (and the default
   suite unchanged).

Ordering: this preceded §5 steps 4–5, as required. The `locals_to_S`
relaxation is now unblocked.

## 10. Immediate eviction is gone

**Landed.** Chain conversion is the only mechanism that drops an on-stack JIT
frame to the interpreter.

Immediate eviction existed because a basic-op redefinition inside a callee
makes the *caller's* already-compiled continuation stale — its inlined integer
arithmetic and constant folds assume the builtin op — and the callee's entry
guards cannot protect a frame that is already suspended. With no way to
convert a suspended frame, the only lever was the code itself: record each
call's return continuation as a patch point and, on redefinition, overwrite it
with a `jmp`/`B` to that site's `Evict` handler, so the frame deopted one
instruction after it resumed.

Chain conversion subsumes that completely — it drops the same frames to the
interpreter, from the stack rather than from the code — with three things the
patching form could not offer:

* a compiled body still valid for *future* invocations survives, because no
  machine code is rewritten;
* on aarch64 it removes a self-modifying-code path, with its
  writable/I-cache-invalidate dance around every patched word (the remaining
  SMC users are `patch_call_to_entry` and the recompilation patch points, which
  are unrelated);
* it is the mechanism the unboxed-locals speculation needs anyway, so there is
  one walk to reason about instead of two that must agree.

The single precondition was that **every** call and yield site register a
`chain_deopt_table` entry, since an unregistered site is one the walk leaves
running its stale body. §8.3's feature gate on `AbstractState::chain_exit` was
therefore removed; every build pays the metadata.

Removed with it: `Codegen::patch_return_to_deopt` (both arches),
`get_deopt_with_return_addr` and the `return_addr_table` it read,
`emit_immediate_evict` (both arches), and `AsmInst::ImmediateEvict` /
`LInst::ImmediateEvict` with their dispatch arms. `asm_return_addr_table`
stays — it is how `AsmInst::ChainExit`, pushed after the call when the return
address is no longer at hand, names the site it belongs to. `AsmEvict` and
`SideExit::Evict` stay for the same reason, though nothing branches to an
`Evict` handler any more; retiring that emission is a separate cleanup.

`Codegen::evict_suspended_frames` and `Codegen::chain_deopt` were the same CFP
walk once the patch fallback was gone, and are now one function
(`chain_deopt`), used by both `check_bop_redefine` and `runtime::chain_deopt`.
