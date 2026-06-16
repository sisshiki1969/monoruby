# Separating the abstract interpreter from register allocation

Design study for **Phase-1 item ②** ("separate the abstract interpreter /
fixpoint search from register allocation"). This is the structural prerequisite
for the longer-term goals: collapsing AsmIR into LIR (goal 1) and deriving the
VM and JIT from one description via partial evaluation (goal 3 / item ③).

Status: **design proposal only** — no code has moved yet.

---

## 1. Where the two concerns are fused today

The JIT's middle end runs a single abstract-interpretation pass over TraceIR
that *simultaneously* infers types and assigns physical storage. The fusion
lives in three places:

### `LinkMode` — one enum, three concerns

`monoruby/src/codegen/jitgen/state/slot.rs:1229`

```rust
enum LinkMode {
    V,                  // no value
    None / MaybeNone,   // optional-arg sentinels
    S(Guarded),         // boxed on the stack      + type guard
    G(Guarded),         // boxed in GP r15 (acc)   + type guard
    F(FPReg),           // unboxed f64 in an xmm    (type = Float)
    Sf(FPReg, SfGuarded // unboxed in xmm + boxed cache on stack + type
    C(Value),           // compile-time constant   (type = guarded(v))
}
```

Each variant encodes **all three** of: the abstract *type* (`Guarded` class /
float-ness / concrete value), the *representation* (boxed `Value` vs unboxed
`f64`), and the *location* (stack home / GP `r15` / xmm pool).

### `SlotState` — type lattice and allocation map in one struct

`monoruby/src/codegen/jitgen/state/slot.rs:4`

```rust
struct SlotState {
    slots: Vec<LinkMode>,    // per-slot fused type+repr+location
    liveness: Vec<IsUsed>,   // analysis
    vfpr: Vec<Vec<SlotId>>,  // allocation: reverse map xmm/spill -> slots
    r15: Option<SlotId>,     // allocation: who owns the accumulator
    pinned_vfpr: Vec<FPReg>, // allocation directive (anti-aliasing)
    …
}
```

### Allocation decisions are taken *inside* the dataflow

- `alloc_xmm` (`slot.rs:370`) does greedy linear-scan allocation (find a vacant
  physical xmm `0..PHYS_XMM_POOL`; else demote an `Sf` cache; else spill to
  `FPReg(N≥PHYS_XMM_POOL)`).
- `def_F` / `def_Sf_*` (`slot.rs:554`) allocate a register **and** set the slot's
  type in one call.
- `AbstractFrame::join` (`state/join.rs:46`) merges *type guards* and
  *reconciles registers* together — it can even allocate a fresh xmm mid-merge
  (`try_set_new_F`) when two predecessors hold a value in different xmms.

### What is already factored out

`FPReg` (`codegen.rs:181`) is **already a virtual register**: `FPReg(0..13)` →
`xmm2..xmm15`, `FPReg(14+)` → an 8-byte stack spill, resolved late by
`FPReg::loc(base)` (`codegen.rs:188`). `AsmInst` operands carry `FPReg`, so the
*operand* layer is virtual. What is **not** factored out is *when/where the
FPReg assignment is decided* — it happens inline with type inference.

**Net:** type analysis, representation (box/unbox), and register allocation are
one pass over one fused `LinkMode`/`SlotState`. ② is about teasing these apart.

---

## 2. Why separate (the payoff)

- **Goal 1 (collapse AsmIR into LIR).** Once allocation is a distinct step that
  *emits*, it can emit `LInst` directly; the `AsmInst` layer (already ~isomorphic
  to `LInst` after the B-migrations) stops carrying its own existence.
- **Goal 3 (one description → VM + JIT).** Partial evaluation needs the *analysis*
  to be reusable under two different allocation policies:
  - **JIT residual** = analysis with inline-cache types + the greedy xmm
    allocator (today's behaviour).
  - **VM residual** = analysis with ⊤ (no specialization) + a *fixed-convention*
    allocator (everything boxed in its stack home, no pool). 
  You cannot instantiate two allocators while allocation is welded to inference.
- **Maintainability.** The `join` table conflates a type lattice with a register
  reconciler; splitting them makes each independently testable.

---

## 3. Target architecture

Three layers, with a typed IR in the middle:

```
TraceIR
  │  ① analysis pass  (fixpoint; types + liveness only — NO locations)
  ▼
Typed IR            per-slot `Guarded` type lattice + liveness; operands are
  │                 (slot, representation) — still virtual, no phys regs
  │  ② allocation + lowering pass  (pluggable Allocator)
  ▼
LIR (LInst)         concrete regs/spills; emitted straight to encode_linst
  ▼  encode_linst → bytes
```

### Layer ① — the type lattice (analysis)

A pure lattice element per slot, *no* location. This is the **existing**
`Guarded` enum — no new type is needed:

```rust
enum Guarded { Value /*⊤*/, Fixnum, Float, Class(ClassId) }
```

`join` over `Guarded` is a *pure* lattice meet (`Guarded::join` already exists) —
no register churn. Liveness stays here. This is what goal 3's partial evaluator
parameterizes (feed ⊤ for the VM residual, IC-narrowed types for the JIT
residual).

**`Sf` is not a type.** Per review, the `Sf` linkage ("Integer def'd, Float
use'd, kept coerced to `f64`") is **not** a lattice element but a *representation
decision* taken by a separate analysis. In the typed IR an `Sf` slot lowers to
its plain boxed type (`Fixnum`); a dedicated def-use + loop pass then *marks* it
for the xmm-coerced representation when:

> the slot is def'd as Integer **and** use'd as Float, **and** it is a
> constant/literal **or** def'd outside a loop and use'd inside it
> (i.e. the coercion is loop-invariant and worth hoisting into an xmm).

That mark *is* the `XmmStack` placement. Keeping it out of the type lattice is
what lets the VM residual (no marks, everything boxed) and the JIT residual
(marks applied) share one analysis.

### Layer ② — representation + allocation

Given the typed IR + liveness, a separate step decides:
1. **Representation**: keep a `Float`/`Fixnum` value unboxed where it is consumed
   by FP arithmetic, else boxed. (Today: the `F` vs `S` vs `Sf` choice.)
2. **Placement**: assign each live unboxed value an `FPReg` (pool or spill) and
   each boxed value its stack home / the `r15` accumulator. Insert transfer /
   spill / φ-move code at edges.

This is the swappable `Allocator`. The default is the current greedy policy;
the VM policy is "no pool, everything in its stack home."

---

## 4. Incremental migration path

Each step is independently shippable and verified at **1702/0** (behaviour
preservation under CRuby 4.0+). Order chosen so the risky structural change
comes last, after the data is already decoupled.

| Step | Change | Risk |
| ---- | ------ | ---- |
| **0a. Decomposition + test** ✅ | Add the location-only `Placement` enum, plus `LinkMode::{placement, from_parts}` projections, with a round-trip test proving `LinkMode ≅ (Placement, Guarded)`. The type lattice is the existing `Guarded` (no new type — per review, `Sf` is a representation mark, not a type; its `SfGuarded` refinement is recovered from the paired `Guarded`). Additive scaffolding — no live state touched. *Done; suite 1703/0.* | none |
| **0b. Storage split** ✅ | (0b-i) Encapsulate every `self.slots` access behind `mode()`/`set_mode()`/`all_regs()`/`slots_len()` (the two in-place mutations become local-copy RMW). (0b-ii) Replace `SlotState.slots: Vec<LinkMode>` with `place: Vec<Placement>` + `ty: Vec<Guarded>`; `mode()` composes via `from_parts`, `set_mode()` decomposes. Behaviour-identical. *Done; suite 1703/0.* | done |
| **0c. Factor the type meet** ✅ | Extract the analysis-layer join as a reusable primitive: relocate `Guarded::join` next to `Guarded` and add `SlotState::join_ty` (element-wise `Guarded::join` over the `ty` vec). Verified arm-by-arm that the fused `AbstractFrame::join`'s resulting *type* equals this meet for every non-sentinel slot, so the fused join's remaining work is purely placement reconciliation (which carries the allocation side-effects and moves to the `Allocator` in steps 1–2). *Done; suite 1703/0.* | done |
| **1. Allocator seam** ✅ | Extract `vfpr` + `pinned_vfpr` and the pure pool primitives into an `XmmAllocator` struct owned by `SlotState`; the `xmm_*` methods delegate to it. The policy (`try_alloc_xmm`/`alloc_xmm`) stays on `SlotState` (it also mutates slot placements). *Done; suite 1703/0.* | done |
| **2. Standalone analysis** | Run the `Guarded`/liveness fixpoint as its own pass producing a typed IR, *before* the allocation+lowering pass consumes it. The lowering pass becomes `fn(typed_ir, &mut Allocator) -> Vec<LInst>`. This is the real separation. **Spike done** — see §9. | high |
| **3. AsmIR → LIR (goal 1)** | The lowering pass emits `LInst` directly; retire `AsmInst` as a distinct stream (its `AsmIr` bookkeeping — `side_exit`, flags — moves to the lowering driver). | med |
| **4. Two allocators (goal 3 enabler)** | Add the fixed-convention VM `Allocator`; spike VM-residual generation for one bytecode. | research |

Step 0a is shipped (`Placement` + projections + round-trip test). Steps 0b–1
are mechanical decoupling that pay off immediately (clearer code, testable
lattice) and de-risk step 2. Steps 3–4 are where goals 1 and 3 land.

---

## 5. Hard parts / open questions

- **Join-time reallocation.** Today `join` may allocate a fresh xmm when two
  predecessors hold a value in different registers. Separated, the allocator must
  resolve this as an SSA-φ with **edge moves** (insert `FprMove`s on the
  CFG edges) rather than reallocation during the meet. This is the crux: step 2
  effectively turns the fused greedy pass into a proper linear-scan / SSA
  allocator with edge fixups. Codegen quality must not regress (the current
  greedy is decent on the FP-heavy benchmarks).
- **`Sf` (xmm + stack cache).** *Resolved in review:* `Sf` is a representation
  decision, not a type. The typed IR carries the plain boxed type (`Fixnum`); a
  separate def-use + loop analysis marks the slot for the xmm-coerced
  representation (the `XmmStack` placement) using the heuristic in §3. The
  *demote-on-pressure* logic (`try_alloc_xmm` phase 1) is a further allocation
  policy that moves into the `Allocator`. (In the current fused state the
  `SfGuarded` refinement still round-trips losslessly through the paired
  `Guarded`, since `SfGuarded → Guarded` is injective.)
- **`r15` accumulator.** The single GP "accumulator" slot is its own tiny
  allocation problem fused into `SlotState.r15`; it follows the same split
  (type vs placement) but is simpler than the xmm pool.
- **Spill-region sizing across joins** (`grow_xmm_to`, `gen_bridge`) becomes the
  allocator's responsibility once placement is its own layer.

---

## 6. Progress

**Step 0a is done** (revised per review). `LinkMode` now has the `placement()` /
`from_parts()` projections, and a unit test (`linkmode_placement_roundtrip`)
proves it is isomorphic to `(Placement, Guarded)`. The type lattice is the
existing `Guarded`; `Sf` is treated as a representation mark (the `XmmStack`
placement), not a type, with its refinement recovered from the paired `Guarded`.
No live state changed; suite at 1703/0.

**Steps 0a–0c are done.** `SlotState` is backed by `place: Vec<Placement>` +
`ty: Vec<Guarded>`; the type lattice is a standalone per-slot vector with a
reusable meet (`join_ty`). Crucially, the fused `AbstractFrame::join`'s *type*
result is exactly that meet for every non-sentinel slot — so the join's residual
work is **purely placement reconciliation** (the register/φ reconciliation that
carries allocation side-effects). That confirms the clean split point: the type
analysis is already separable; what remains entangled is allocation, which is
precisely what steps 1–2 pull out.

**Step 1 is done.** The xmm allocation state (`vfpr` + `pinned_vfpr`) and its
pure pool primitives now live in an `XmmAllocator` struct owned by `SlotState`,
physically separated from the slot type/placement state. The allocation policy
still sits on `SlotState`.

**Next: step 2 (standalone analysis pass)** — run the `Guarded`/liveness fixpoint
as its own pass (consuming `join_ty`) producing a typed IR, before the
allocation+lowering pass. This is the high-risk structural change; goals 1/3
fall out of steps 3–4 afterward.

---

## 9. Step-2 spike: where the analysis/emission fusion actually lives

Before committing to step 2 (the big rewrite), a spike traced exactly *how*
analysis and emission are entangled in `compile_instruction`. The finding
reshapes the plan.

### What the spike found

The bytecode handlers (`compile_instruction`, ~100 `TraceIr` arms) are **not**
where analysis and emission are knotted together. A handler like `LoadGvar` is
just `discard(dst); push(LoadGVar); def_rax2acc(dst)`. Following that down:

```
def_rax2acc → def_reg2acc_guarded → def_G → writeback_acc
```

the *only* emission on the whole chain is at the very bottom, in a handful of
**transfer / eviction primitives** — `writeback_acc` (evict the `r15`
accumulator owner to its stack home), the xmm spill/swap emitters, etc. Almost
everything else (the `Guarded` lattice, liveness, placement bookkeeping in
`place`/`ty`/`XmmAllocator`) is *already pure state*.

And those transfer primitives split **cleanly**. `writeback_acc` was:

```rust
fn writeback_acc(&mut self, ir) {
    if let Some(slot) = self.r15 {
        self.set_mode(slot, S(self.guarded(slot)));  // state
        self.r15 = None;                              // state
        ir.acc2stack(slot);                           // emission
    }
}
```

The spike split it into `writeback_acc_state() -> Option<SlotId>` (the pure
state transition, returns *which* slot was evicted) and the residual
`writeback_acc` = `if let Some(slot) = self.writeback_acc_state() { ir.acc2stack(slot) }`.
The emission is fully determined by the slot the state half returns — i.e. the
transfer primitive is `(state-mutation that yields a transfer record) + (emit
from that record)`. Behaviour-identical; suite 1703/0.

### How this reshapes step 2

Step 2 is therefore **not** "split ~100 op handlers". It is:

1. Split each **transfer/eviction primitive** (a bounded set — `writeback_acc`,
   the xmm spill/swap/`float_to_fpr` emitters, `def_*`'s eviction step) into a
   state half that *returns a transfer record* and an emit half that consumes
   it. The records are exactly the **typed IR** the analysis pass produces.
2. The standalone **analysis pass** runs the handlers with the state halves and
   collects the transfer records (no `AsmIr`). It already exists in skeleton
   form: `analyse_basic_block` reuses `compile_instruction` but discards its
   `AsmIr` — today that discard is wasteful (it builds `AsmInst` only to drop
   them); after the split it would call the state halves and skip emission.
3. The **lowering pass** replays the records, emitting `LInst` via the emit
   halves + `encode_linst`.

This is a far more bounded and mechanical change than a per-handler rewrite, and
each primitive split is independently shippable and behaviour-verifiable at
1703/0 (the `writeback_acc` split is the first). It also subsumes goal 1: once
lowering is its own pass, it emits `LInst` directly and `AsmInst` retires.

### Progress on the transfer-primitive split

Split so far, each behaviour-identical at 1703/0:

- **Stack writebacks** → the `Spill` record (`None` / `Fpr` / `Lit` / `Acc`):
  `writeback_acc`, `write_back_slot`, `to_S_unguarded`.
- **FP-register transfers** → the `FpXfer` record (`Move` / `Swap`): `to_sf`
  (`gen_xmm_swap` was already a clean state-line + emit-line).

Each primitive now has a `*_state` half that performs the abstract-state
transition and returns the record, plus a thin codegen wrapper `record.emit(ir)`.
The records (`Spill`, `FpXfer`) are the growing **typed IR** vocabulary.

### The hard tail: deopt-carrying transfers

The unbox loads (`load_xmm` and friends) are *not* a clean `(state) + (record →
emit)` split, because they create a **deopt side-exit** mid-flight:

```rust
let deopt = ir.new_deopt(self);     // captures state.get_write_back() — a
self.use_as_float(slot);            // SNAPSHOT of the live placement state
match self.mode(slot) { S(_) => { let x = self.set_new_Sf(..); 
    ir.stack2reg(slot, Rdi); ir.float_to_fpr(Rdi, x, deopt); x } … }
```

`new_deopt` snapshots `get_write_back()` — *which* values are unboxed/in-acc and
must be restored to the stack if the float guard fails. That snapshot is the
placement state **at this program point**. So in the separated design the deopt
is created by the **codegen pass**, reconstructing the write-back from the
analysis-precomputed placement at that point; the typed IR records the deopt
program point (pc), not a frozen `AsmDeopt`. This is the main wrinkle that
distinguishes the FP-load transfers from the simple evictions, and it is where
the typed IR must carry per-point placement (which the analysis already tracks).

**Resolved (`load_xmm` split).** `load_xmm` / `load_xmm_fixnum` now split into a
`load_xmm_state` half (allocate the xmm, bind the slot) returning an `XmmLoad`
record (`None` / `FromStack` / `FromAcc` / `FromF64` / `FromFixnum`), plus a
wrapper that creates the deopt **first** (so its write-back snapshot is the
pre-load placement) and passes it as `Option<AsmDeopt>` to `XmmLoad::emit`. The
deopt is therefore supplied by the codegen side, **not** frozen into the record;
the guard-free numeric variants pass `None`. This confirms the resolution above
concretely — behaviour-identical at 1703/0.

### The guard primitive

`guard_class` (the guard primitive behind `guard_fixnum` / `load_fixnum` /
`load_array_ty`) splits into `guard_class_state(slot, class) -> bool` (refine the
slot's type; return whether a runtime guard must be emitted) plus the emit
`if guard_class_state { ir.push(GuardClass(r, class, deopt)) }`. `load_fixnum`
and `load_array_ty` then *compose* split primitives (`load` + the guard).

### `load_xmm_fixnum`: the interleaving dissolves (single record after all)

`load_xmm_fixnum`'s `S`/`G` arms *looked* like the case that could not reduce to
a single `(state) + (record → emit)` pair, because they interleave a load, a
`new_deopt`, a guard and the conversion:

```
stack2reg(Rdi)            // emit  — load the boxed value
new_deopt                 // deopt
guard_class_state         // state (type)
push GuardClass(deopt)    // emit  — Integer guard
set_new_Sf                // state (placement — allocates the xmm)
fixnum2fpr(Rdi, x)        // emit  — int → f64
```

The deopt snapshot (`get_write_back`) must precede the placement change
(`set_new_Sf`). The apparent obstacle was that the load (`stack2reg`) "must"
precede the deopt. But **`stack2reg`/`reg2stack` are pure emits on `ir`** — they
push an `AsmInst` and never touch the frame's placement state — so `new_deopt`
**commutes** with them. Reordered, the dependency chain is just
`new_deopt → {guard_class_state, set_new_Sf}`, the same shape `load_xmm` already
solved: create the deopt up front, run the (now reorderable) state half, defer
all emission into the record. The guard folds into the record as a bool
(`guard_class_state`'s verdict). So `load_xmm_fixnum` splits into
`load_xmm_fixnum_state -> (FPReg, XmmFixnumLoad)` plus a wrapper that creates the
deopt *only* for the guarded `S`/`G` arms (peeking the mode — `use_as_value`
only marks liveness, so the peek is stable) and supplies it to `XmmFixnumLoad::emit`.
Behaviour-identical at 1703/0.

The lesson: **a "pure emit" instruction between a state mutation and a `new_deopt`
is not a true interleaving** — it commutes out to the emit half. The
single-record model is therefore more general than first thought, and with this
split *every* transfer/eviction primitive in the table is now decomposed into a
`*_state` analysis half returning a typed-IR record (`Spill` / `FpXfer` /
`XmmLoad` / `GpLoad` / `XmmFixnumLoad`, plus the `guard_class_state` verdict) and
a record-replaying emit half. That completes the prerequisite for the two-pass
wiring: the analysis pass calls the `*_state` halves and collects the records;
codegen replays `record.emit(...)`. The remaining step is plumbing those two
passes through `compile_instruction` / `analyse_basic_block`.

## 10. The analysis/codegen seam already exists: `codegen_mode`

Before building a two-pass from scratch, a closer read of the driver shows the
seam is **already present**, which reframes step 2.

### What is actually there

`AsmIr` carries `codegen_mode: bool` (`asmir.rs:76`), seeded from
`JitContext::codegen_mode()`. Crucially **`AsmIr::push` is already gated on it**:

```rust
fn push(&mut self, inst: AsmInst) {
    if self.codegen_mode { self.inst.push(inst); }   // no-op in analysis mode
}
```

Two passes already run the *same* `compile_instruction` under the two modes:

- **Loop-analysis pre-pass** — `JitContext::loop_analysis` sets
  `codegen_mode: false` (`context.rs:687`). `analyse_backedge_fixpoint` →
  `analyse_basic_block` runs the handlers to compute the loop's back-edge /
  liveness fix-point. `push` is suppressed, so **no `AsmInst` is built** — it
  produces *abstract state*, not an instruction stream.
- **Codegen pass** — `traceir_to_asmir` → `compile_basic_block` runs with
  `codegen_mode: true`, doing analysis **and** emission in one fused walk.

Handlers that must diverge between the two modes already branch on
`self.codegen_mode()` (e.g. `binop_uncached` in `binary_op.rs:25` widens to `S`
during analysis but emits a per-instruction deopt+recompile during codegen).

**Correction to §9:** the claim that the analysis pass "builds `AsmInst` only to
drop them" is wrong — `push` is gated, so analysis never accumulates the stream.
The only residual analysis-mode waste is computing a transfer record and then
calling its no-op `emit`, plus ungated `side_exit` growth (`new_deopt` /
`new_label` are not gated, but their results are unused in analysis).

### What this means for step 2

The separation is **not** "introduce an analysis pass" — that exists. It is two
remaining, independent pieces:

1. **De-fuse allocation from the dataflow (the §5 crux).** Today *both* passes
   allocate: `alloc_xmm` / `join`'s register reconciliation mutate placement
   *inside* the abstract-interpretation walk. So the loop pre-pass is "analysis +
   allocation with emission suppressed," not pure type/liveness analysis. The real
   work is pulling placement out of the join — turning join-time reallocation into
   SSA-φ **edge moves** — so the analysis pass computes *only* `Guarded` + liveness
   and the allocator runs as the second pass over that result. This is the
   high-risk core; codegen quality (the greedy xmm policy) must not regress.
2. **Record-driven lowering (goal 1).** Once allocation is its own pass, the
   codegen walk replays the typed-IR records (`Spill` / `FpXfer` / `XmmLoad` /
   `GpLoad` / `XmmFixnumLoad` / guard) emitting `LInst` directly, and `AsmInst`
   retires. The transfer-primitive split (now complete) is exactly what makes the
   replay possible; the open wrinkle is the deopt, which must become a *program
   point* reconstructed from the analysis-precomputed placement at that pc rather
   than the frozen `AsmDeopt` the records carry today (§9 deopt note).

So the two-mode `compile_instruction` is the chassis; the remaining engineering
is (1) then (2). (1) is the architectural fork — re-execution-based (generalize
the `codegen_mode` pre-pass to all code, feeding precomputed states forward) vs.
record-stream-based (collect records, lower from them) — and is the decision to
take deliberately, since it sets how allocation is staged.

## 11. Record collection: the `TransferIR` stream

The first concrete step of record-driven lowering (chosen over the §5 allocation
de-fusing as the lower-risk groundwork): collect the transfer records into a
stream so a later pass can replay them.

- **Unified element.** The five per-primitive records (`Spill`, `FpXfer`,
  `GpLoad`, `XmmLoad`, `XmmFixnumLoad`) now share one enum
  `TransferIR` (`state/read_slot.rs`) with a single `emit` dispatch. The two
  deopt-carrying variants still freeze an `AsmDeopt` (lifting it to a program
  point is §9's open item, the next wall).
- **One funnel.** `AsmIr::transfer(t)` is the sole sink: it pushes `t` onto the
  new `transfers: Vec<TransferIR>` (codegen mode only, so it stays in lock-step
  with the `codegen_mode`-gated `inst`) and then emits via `t.emit(self)`. Every
  transfer/eviction wrapper (`load`, `load_xmm`, `load_xmm_fixnum`,
  `write_back_slot`, `to_S_unguarded`, `to_sf`) now calls `ir.transfer(...)`
  instead of `record.emit(ir, …)`.
- **Faithful by construction.** The collected `t` *is* the record that gets
  emitted (same value, same call), so the stream is exactly the emitted transfer
  sequence — no shadow comparison needed; the suite (1703/0) confirms emission is
  byte-identical. `save`/`restore` truncates `transfers` alongside `inst`, so the
  stream survives speculative-emit rollback intact.

The `transfers` stream is **collected but not yet consumed** — that is the
groundwork. The next steps are (1) lift the deopt to a program point so the
stream is codegen-independent, then (2) drive lowering from the stream (replay
`TransferIR` → `LInst`) and retire the corresponding direct `AsmInst` emission.
