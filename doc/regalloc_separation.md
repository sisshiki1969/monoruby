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

- `alloc_fpr` (`slot.rs:370`) does greedy linear-scan allocation (find a vacant
  physical xmm `0..PHYS_FPR_POOL`; else demote an `Sf` cache; else spill to
  `FPReg(N≥PHYS_FPR_POOL)`).
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

That mark *is* the `FprStack` placement. Keeping it out of the type lattice is
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
| **1. Allocator seam** ✅ | Extract `vfpr` + `pinned_vfpr` and the pure pool primitives into an `FprAllocator` struct owned by `SlotState`; the `xmm_*` methods delegate to it. The policy (`try_alloc_fpr`/`alloc_fpr`) stays on `SlotState` (it also mutates slot placements). *Done; suite 1703/0.* | done |
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
  representation (the `FprStack` placement) using the heuristic in §3. The
  *demote-on-pressure* logic (`try_alloc_fpr` phase 1) is a further allocation
  policy that moves into the `Allocator`. (In the current fused state the
  `SfGuarded` refinement still round-trips losslessly through the paired
  `Guarded`, since `SfGuarded → Guarded` is injective.)
- **`r15` accumulator.** The single GP "accumulator" slot is its own tiny
  allocation problem fused into `SlotState.r15`; it follows the same split
  (type vs placement) but is simpler than the xmm pool.
- **Spill-region sizing across joins** (`grow_fpr_to`, `gen_bridge`) becomes the
  allocator's responsibility once placement is its own layer.

---

## 6. Progress

**Step 0a is done** (revised per review). `LinkMode` now has the `placement()` /
`from_parts()` projections, and a unit test (`linkmode_placement_roundtrip`)
proves it is isomorphic to `(Placement, Guarded)`. The type lattice is the
existing `Guarded`; `Sf` is treated as a representation mark (the `FprStack`
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
pure pool primitives now live in an `FprAllocator` struct owned by `SlotState`,
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
`place`/`ty`/`FprAllocator`) is *already pure state*.

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
  (`gen_fpr_swap` was already a clean state-line + emit-line).

Each primitive now has a `*_state` half that performs the abstract-state
transition and returns the record, plus a thin codegen wrapper `record.emit(ir)`.
The records (`Spill`, `FpXfer`) are the growing **typed IR** vocabulary.

### The hard tail: deopt-carrying transfers

The unbox loads (`load_fpr` and friends) are *not* a clean `(state) + (record →
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

**Resolved (`load_fpr` split).** `load_fpr` / `load_fpr_fixnum` now split into a
`load_fpr_state` half (allocate the xmm, bind the slot) returning an `FprLoad`
record (`None` / `FromStack` / `FromAcc` / `FromF64` / `FromFixnum`), plus a
wrapper that creates the deopt **first** (so its write-back snapshot is the
pre-load placement) and passes it as `Option<AsmDeopt>` to `FprLoad::emit`. The
deopt is therefore supplied by the codegen side, **not** frozen into the record;
the guard-free numeric variants pass `None`. This confirms the resolution above
concretely — behaviour-identical at 1703/0.

### The guard primitive

`guard_class` (the guard primitive behind `guard_fixnum` / `load_fixnum` /
`load_array_ty`) splits into `guard_class_state(slot, class) -> bool` (refine the
slot's type; return whether a runtime guard must be emitted) plus the emit
`if guard_class_state { ir.push(GuardClass(r, class, deopt)) }`. `load_fixnum`
and `load_array_ty` then *compose* split primitives (`load` + the guard).

### `load_fpr_fixnum`: the interleaving dissolves (single record after all)

`load_fpr_fixnum`'s `S`/`G` arms *looked* like the case that could not reduce to
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
`new_deopt → {guard_class_state, set_new_Sf}`, the same shape `load_fpr` already
solved: create the deopt up front, run the (now reorderable) state half, defer
all emission into the record. The guard folds into the record as a bool
(`guard_class_state`'s verdict). So `load_fpr_fixnum` splits into
`load_fpr_fixnum_state -> (FPReg, FprFixnumLoad)` plus a wrapper that creates the
deopt *only* for the guarded `S`/`G` arms (peeking the mode — `use_as_value`
only marks liveness, so the peek is stable) and supplies it to `FprFixnumLoad::emit`.
Behaviour-identical at 1703/0.

The lesson: **a "pure emit" instruction between a state mutation and a `new_deopt`
is not a true interleaving** — it commutes out to the emit half. The
single-record model is therefore more general than first thought, and with this
split *every* transfer/eviction primitive in the table is now decomposed into a
`*_state` analysis half returning a typed-IR record (`Spill` / `FpXfer` /
`FprLoad` / `GpLoad` / `FprFixnumLoad`, plus the `guard_class_state` verdict) and
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
   allocate: `alloc_fpr` / `join`'s register reconciliation mutate placement
   *inside* the abstract-interpretation walk. So the loop pre-pass is "analysis +
   allocation with emission suppressed," not pure type/liveness analysis. The real
   work is pulling placement out of the join — turning join-time reallocation into
   SSA-φ **edge moves** — so the analysis pass computes *only* `Guarded` + liveness
   and the allocator runs as the second pass over that result. This is the
   high-risk core; codegen quality (the greedy xmm policy) must not regress.
2. **Record-driven lowering (goal 1).** Once allocation is its own pass, the
   codegen walk replays the typed-IR records (`Spill` / `FpXfer` / `FprLoad` /
   `GpLoad` / `FprFixnumLoad` / guard) emitting `LInst` directly, and `AsmInst`
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
  `GpLoad`, `FprLoad`, `FprFixnumLoad`) now share one enum
  `TransferIR` (`state/read_slot.rs`) with a single `emit` dispatch. The two
  deopt-carrying variants still freeze an `AsmDeopt` (lifting it to a program
  point is §9's open item, the next wall).
- **One funnel.** `AsmIr::transfer(t)` is the sole sink: it pushes `t` onto the
  new `transfers: Vec<TransferIR>` (codegen mode only, so it stays in lock-step
  with the `codegen_mode`-gated `inst`) and then emits via `t.emit(self)`. Every
  transfer/eviction wrapper (`load`, `load_fpr`, `load_fpr_fixnum`,
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

### Shadow harness: the records are self-contained

A debug-only shadow check in `AsmIr::transfer` replays each record alone into a
fresh scratch `AsmIr` and asserts it reproduces *exactly* the `AsmInst`s the real
emit just appended (compared via `Debug`, since `AsmInst` is not `PartialEq`).
This proves `TransferIR::emit` is a **pure function of the record** — it reads
nothing from `self`/the frame beyond the record's own payload. That self-
containment is precisely what record-driven lowering needs (it will replay the
stream with no analysis frame in hand), and the assert is a standing guard
against a future transfer whose emit sneaks in a state dependence. All transfer
emit helpers are single deterministic `push`es, so the check holds for the whole
suite (1703/0, debug build, every codegen-mode transfer exercised).

### Deopt program-point-ification: the stream is now codegen-independent

The one remaining codegen dependence in the `TransferIR` stream was the frozen
`AsmDeopt` (an index into the codegen pass's `side_exit` table) carried by the
guarded `FprLoad` / `FprFixnumLoad` records. That index is meaningless without
the exact `side_exit` table it points into — so a standalone lowering pass could
not replay the stream.

Resolved as doc §9 foretold: the records now carry a **`DeoptPoint`** — the
program point `(pc, write_back)`, both pure analysis values the frame already
tracks (`get_write_back()` is the placement snapshot restored on guard failure).
The analysis half (`load_fpr` / `load_fpr_fixnum` wrappers) records the point via
`deopt_point()` (no `side_exit` push); the **emit half** materializes the actual
side-exit via `AsmIr::deopt_from_point`, so `side_exit` construction lives
entirely on the codegen side. `new_deopt` is no longer called from the transfer
wrappers.

Ordering is preserved exactly: within `load_fpr{,_fixnum}` the only `side_exit`
push was this one deopt, created "first" — `deopt_from_point` runs at the top of
the emit arm, at the same relative position, so the `side_exit` table is
byte-identical. The guarded-but-`guard == false` `S`/`G` arms still materialize
the (dead) deopt, matching the pre-split wrapper.

`TransferIR` is consequently `Clone` (not `Copy` — a `DeoptPoint` owns a
`WriteBack`). The shadow harness was strengthened accordingly: emit is now a pure
function of the record **and the `side_exit` cursor** (a guarded record
materializes `AsmDeopt(side_exit.len())`), so the replay pre-pads the scratch's
`side_exit` to the same length and asserts both the produced `AsmInst`s *and* the
produced `SideExit`s match (Debug-compared; neither is `PartialEq`). Suite
1703/0, no replay mismatches — the stream is now fully codegen-independent, the
last prerequisite for record-driven lowering (step 3).

### Analysis pass skips emission (doc §9 step 2, realized)

With the deopt program-point-ified, `AsmIr::transfer` now **returns immediately
in analysis mode** (`codegen_mode == false`). The abstract-state mutation already
happened in the wrapper's `*_state` half; `emit` only writes to `ir`, and the
loop pre-pass discards its local `AsmIr` (`analyse_basic_block` drops it — the
`loop_analysis` context "emits AsmIr only for analysis, it is never codegen'd",
context.rs:692). So emission in analysis mode was pure dead work — and, since
`new_deopt`/`deopt_from_point` are not `push`-gated, it also grew a `side_exit`
table nobody reads (the waste §10 flagged).

This is provably safe: `emit`'s signature, `fn emit(self, ir: &mut AsmIr)`, cannot
touch frame/abstract state — the same property the shadow check independently
verifies. So the analysis pass now literally "calls the state halves and skips
emission" for every transfer primitive, exactly the §9-step-2 shape. The §10
deopt wrinkle ("the open wrinkle is the deopt … frozen `AsmDeopt`") is closed:
the records carry a `DeoptPoint`, and the only thing left fusing analysis and
codegen for the *transfer primitives* is gone. Suite 1703/0.

What remains for full record-driven lowering is the §10-item-1 core (de-fuse
allocation from the join — the SSA-φ edge-move rewrite) and lowering the *op
handlers'* direct emissions through records too; the transfer primitives are
done.

## 12. §5 crux, first cut: de-fusing allocation from the join

The §5 crux is that `AbstractFrame::join` *allocates* (`try_set_new_F` /
`try_set_new_Sf`) during the meet — fusing register allocation into the
dataflow. First de-fusion step, mirroring the transfer state/emit split:

The per-slot meet table is now split into
- **`decide_join(other, i) -> JoinAction`** — a pure, read-only function of the
  two predecessors' `LinkMode`s: the merge *decision*; and
- **`apply_join(i, action)`** — which performs the placement mutation and is the
  **only** place the meet allocates an xmm.

`JoinAction` reifies the nine meet outcomes (`Nop` / `SetMaybeNone` / `Discard` /
`TryFreshFKeep` / `TryFreshFElseS` / `SetSf` / `TryFreshSfElseKeep` /
`TryFreshSfElseS` / `SetS`). The fresh-xmm rebinds (`TryFresh*`) are the
join-time reallocation §5 targets; they are now isolated in `apply_join`, behind
one seam. Behaviour is identical (the same operations, reorganized) — suite
1703/0.

This is the structural prerequisite for the real change: an allocator pass that
consumes the `JoinAction` stream and assigns registers + inserts edge moves
(`bridge` already emits the FprMove/swap edge fixups), instead of `apply_join`
allocating inline during the meet. The `decide`/`apply` boundary is exactly where
that pass plugs in. Not yet done: turning the `TryFresh*` inline allocations into
allocator-assigned φ registers (the SSA-φ edge-move rewrite) — but the meet is
now cleanly type-decision (`decide_join`) vs placement-allocation (`apply_join`).

### §5 stage 1: the merge as a replayable record stream

With the meet split into `decide_join` / `apply_join`, stage 1 of the safe
allocator de-fusion records the per-slot `JoinAction` stream as the merge runs,
then (debug) **replays it from a clone of the pre-merge frame and asserts it
reproduces the identical placement** (every slot's resulting `LinkMode`). This is
the allocation analog of the transfer shadow harness: it locks the property the
separated allocator pass relies on — the decision stream plus `apply_join` is a
*complete*, replayable record of the meet — and becomes the regression harness
future allocator changes shadow against. Suite 1703/0, no replay mismatches.

**Key finding for the allocator design — the meet has cross-slot coupling.** A
`TryFresh*` action's allocation (`try_alloc_fpr` phase 1) can **demote other
slots'** `Sf` bindings to `S` to free a physical xmm. So a later slot's
`decide_join` may read a `LinkMode` that an earlier slot's `apply_join` mutated:
`decide` and `apply` are *not* separable into two clean passes over the slots —
they must interleave. The replay shadow confirms this reproduces faithfully (it
replays `apply_join` in order, demotions included). This rules out the naive
"decide-all then allocate-all" staging and tells us the allocator pass must model
the pool as evolving across the merge's slots (a linear-scan-style sweep), not a
batch assignment — the constraint that shapes stage 2.

### §5 stage 2: type-meet separability is now a standing invariant

Stage 2 promotes doc §6's once-checked claim — "the fused join's type result is
exactly `join_ty` for every non-sentinel slot" — to a **standing debug
assertion** in `verify_join_replay`: after each merge, `self.guarded(i)` equals
the standalone `join_ty(pre, other)[i]` (the allocation-free `Guarded` meet) for
every non-sentinel slot. Verified arm-by-arm and then across the whole suite
(1703/0): every meet arm's result type is `join_ty`, because the `SfGuarded →
Guarded` projection is a **join homomorphism** (`FixnumOrFloat ↦ Value`, matching
`join_ty(Fixnum, Float) = Value`), so even the `Sf` arms that look like they
refine the type actually agree with the plain `Guarded` meet.

This nails down the type/placement split *at the merge*: a standalone
type+liveness analysis pass — running `join_ty` with **no xmm allocation** —
computes types identical to the fused meet, and all allocation is isolated in
`apply_join`. Combined with §5 stage 1 (the merge is a replayable record stream)
the merge is now cleanly factored into (a) a separable, allocation-free type meet
and (b) a recorded placement-allocation stream. What remains for stage 3 is the
behaviour-changing switch — an allocator that assigns φ registers (reusing a
predecessor's where it lowers edge-move cost) instead of `apply_join`'s inline
`TryFresh*` grab — which is benchmark-gated (codegen quality must not regress) and
will diverge from the stage-1 placement shadow by construction.

## 13. §5 stage 3 design: lifting allocation out of the dataflow (the high-risk core)

Stages 1–2 finished the *de-fusion inside the merge*: the meet is now
`decide_join` (pure, allocation-free, type result proven `== join_ty`) + a
recorded `apply_join` placement-allocation stream. Stage 3 is the architectural
switch — the §10-item-1 / §4-step-2 core — and it is **behaviour-changing and
benchmark-gated**: the stage-1/2 shadows are necessary scaffolding for it but
stop applying the moment placements are allowed to diverge.

### 13.1 What the bridge investigation changed about the plan

A read of the edge-move machinery (`AbstractFrame::bridge`, slot.rs:1707; driver
`gen_bridge`, state.rs:89; merge in merge.rs:60–116) settled the key question:

- **Edge moves already exist.** The actual φ-reconciliation MOVs/swaps/spills are
  emitted by `bridge`, *not* by the merge. `bridge` pattern-matches
  `(pred.mode(slot), target.mode(slot))` and has both placements in hand.
- **The merge is commutative and predecessor-blind.** `decide_join` sees only the
  two `LinkMode`s; it does **not** know which predecessor carried `F(xmm2)` vs
  `F(xmm3)`, nor how many predecessors there are. So a "reuse predecessor *p*'s
  register" policy is **not expressible at merge time** — only the bridge, or a
  later pass with per-predecessor placement, can express it.

Consequence: the quick "prefer-keep-`l` instead of grab-fresh" heuristic in the
`TryFresh*` arms is a *weak, commutative* lever (it biases toward whichever frame
happens to be `self`), not the principled fix. **We do not pursue it as stage 3.**
The principled fix is to move allocation to a pass that runs after the
type/liveness fixpoint and can see global/per-predecessor placement — exactly the
§3 Layer ② `Allocator`.

### 13.2 What stage 3 actually is

Today *both* compile passes call `AbstractFrame::join` = `decide_join` (types) +
`apply_join` (allocation):

- the **analysis pre-pass** (`loop_analysis`, `codegen_mode:false`, context.rs:687)
  allocates with emission suppressed — so it is "analysis **+** allocation," not
  pure type/liveness;
- the **codegen pass** (`codegen_mode:true`) allocates *and* emits in one walk,
  and `bridge` turns the per-edge placement deltas into MOVs.

Stage 2 proved the analysis pass does not *need* `apply_join`: its type result is
exactly `join_ty`. Stage 3 acts on that:

> **The analysis pre-pass computes only `join_ty` + liveness (no `apply_join`,
> no xmm allocation). The codegen pass owns *all* placement/allocation, and the
> existing `bridge` already turns the resulting per-edge placement deltas into
> edge moves.**

This is the de-fusion §10 item 1 calls "the high-risk core; codegen quality (the
greedy xmm policy) must not regress."

### 13.3 Decomposition (re-narrowing the safe regime)

The naive view is "stage 3 is all benchmark-gated." It is not — one more slice
stays shadow-able:

- **3a — safe: the analysis fixpoint is allocation-independent.** The goal was to
  prove stripping allocation from the analysis pass does not perturb the type +
  liveness it exists to compute. This splits into two halves:
  - **Merge half — already discharged by stage 2.** `AbstractState::join_entries`
    (state.rs:81) calls `AbstractFrame::join`, where the stage-2 assertion
    (`self.guarded(i) == join_ty(pre, other)[i]`) lives. `join_entries` is reached
    from both `incoming_context` *and* `analyse_backedge_fixpoint` (merge.rs:77,
    79, 107), and the analysis pre-pass `loop_analysis` (context.rs:682,
    `codegen_mode:false`) drives them. So stage 2 already runs on every
    analysis-pass merge **and** every back-edge fixpoint merge; the suite exercises
    loop JIT and passes 1704/0 with it active. Merge-level type meet is therefore
    allocation-independent by an assertion that is *already live* — no duplicate
    fixpoint harness needed (building one would be disproportionate).
  - **Transfer half — by construction, confirmed by the gate.** Between merges the
    fixpoint runs the per-instruction transfer handlers. Their *type* result is
    computed from operand `Guarded`s + IC classes, never from placement (handlers
    branch on `codegen_mode()` only to choose *emission*, e.g. `binop_uncached`
    widens to `S` in analysis — the `Guarded` it records is the same). A full
    static shadow of every handler is disproportionate; this half is covered
    empirically by 3b's benchmark gate plus the exact CRuby-diff correctness suite
    (any type-fixpoint perturbation would change output, which the suite catches
    exactly).

  Net: the safe regime of stage 3 is essentially complete — the merge half is
  formally asserted (live), the transfer half rests on the handlers' type/emission
  split and is confirmed by the gate. The next implementable step is 3b.

- **3b — benchmark-gated: actually strip allocation from the analysis pass.**
  Make `loop_analysis` (and any other `codegen_mode:false` walk) call the
  type-only meet; let the codegen pass allocate from a type-only loop-entry
  frame. The final asm *will* differ (the codegen pass no longer inherits the
  pre-pass's placements), so the stage-1 placement shadow is expected to diverge
  and must be scoped off for the `codegen_mode:false` path. Gate: §13.4.

- **3c — benchmark-gated: improve the allocator with its new global view.** Only
  now is "reuse a predecessor's register / minimise edge moves" expressible,
  because the allocator pass can see per-predecessor placements (the
  `BranchEntry` states, jitgen.rs:114) instead of the commutative merge. Linear
  scan over the type/liveness result; spill = today's `try_alloc_fpr` phase-1
  demotion generalised. Each policy change is an independent benchmark-gated diff.

### 13.4 The benchmark gate

Baseline must be captured **before** any 3b change, from a `--release` build (the
debug shadows compile out, so they do not affect it):

1. `cargo build --release` at the pre-3b commit; record `bin/bench` numbers and
   `optcarrot` fps for the standard set (`benchmark/*.rb`: `app_fib`, the binary-
   trees / so_* set, optcarrot). M1 `bin/test` already passing is the correctness
   baseline; the gate adds the *speed* baseline.
2. Acceptance for promoting 3b/3c to default: **no benchmark regresses beyond
   noise (≈2 %)** vs baseline, and the headline JIT benchmarks (optcarrot,
   app_fib) are within noise or better. A regression that is real and not
   quickly recoverable parks the change behind a runtime flag (mirroring
   `--no-jit`) rather than flipping the default.
3. Run on both backends (x86-64 CI + M1 aarch64) before default-flip, since the
   bridge emits per-arch and the aarch64 backend deopts on unported insts.

### 13.5 Where the existing harness applies / stops

- Stage-2 type-meet assertion: **still valid through 3a/3b** — types never depend
  on allocation, so it keeps guarding the analysis pass after allocation is
  stripped. Keep it.
- Stage-1 placement replay shadow: **valid until 3b** — it asserts the recorded
  stream reproduces *the current* placements; once the analysis pass stops
  allocating, the `codegen_mode:false` path has no placement stream to replay, so
  the shadow is scoped to the codegen pass only (or retired). It is *not* a
  correctness oracle for 3b's intended divergence.
- Net: 3a is covered by shadows; 3b/3c are covered by the benchmark gate plus the
  full CRuby-diff suite (output correctness is still an exact oracle — only
  *speed/codegen* is what the gate watches).

### 13.6 Open questions / risks

1. **Deopt as a program point.** §10 item 2 / §9's deopt note: once placement is
   decided in a later pass, a deopt must be reconstructed from the analysis-
   precomputed placement at that pc, not the frozen `AsmDeopt` the records carry.
   3b can sidestep this only if the codegen pass still decides placement in its
   own forward walk (it does today) — i.e. 3b strips allocation from *analysis*
   but keeps codegen single-walk. Full Layer-② extraction (allocation as a
   distinct pass feeding codegen) is a later step and is where the deopt-program-
   point work lands.
2. **Loop-entry placement quality.** The pre-pass's allocation currently seeds the
   loop body with sensible xmm bindings; a type-only fixpoint hands codegen a
   placement-free entry, so the codegen pass must pick loop-carried xmm bindings
   itself. This is the most likely source of a 3b regression (loop bodies are the
   hot JIT path) and is what the optcarrot/app_fib gate specifically watches.
3. **Phase-1 demotion as spill.** The cross-slot demotion (stage-1 finding) is the
   allocator's only spill mechanism today; a linear-scan allocator (3c) subsumes
   it but must preserve the "stack is canonical, dropping the xmm cache needs no
   asm" property that makes demotion free.

### 13.7 §5 stage 3b: landed behind a default-off feature

3b is implemented as the `loop-type-only-entry` cargo feature (default off, so the
shipping build is bit-identical). When on, `incoming_context` strips the analysis
pass's loop-carried `backedge` frame to a type-only projection
(`AbstractState::strip_fpr_to_stack`: every `F`/`Sf` slot → `S(guarded)`) before
`target.join(&backedge)`, so the codegen pass re-derives the loop-entry xmm
bindings itself via the liveness pass (`liveness_analysis` → `use_float`'s
`try_set_new_Sf`) instead of inheriting them. This is the minimal, reversible lever
for "the codegen pass owns allocation; the analysis pass contributes only types +
liveness."

Verified:
- **Default (off): unchanged** — the strip is `#[cfg]`-compiled out; the join takes
  the placed backedge verbatim.
- **Feature on: correct** — suite 1704/0 (stage-1/2 shadows active) and the sample
  programs match CRuby (`249750.0`, `1249925000.0`).
- **Codegen neutral on the canonical float loop** — `emit-asm` for the
  `x += i*0.5` while-loop is *identical* off vs on: the loop-carried accumulator
  was already `Sf` (boxed per iteration) in the baseline, and `use_float`
  re-promotes it to the same `Sf`, so the analysis-pass backedge placement was
  redundant with liveness-driven promotion here. This is the intended
  behaviour-preserving result — 3b removes the analysis-pass allocation without
  regressing this hot path.

What 3b does **not** do: it does not *improve* the per-iteration box (keeping the
float in an xmm across the back-edge is the 3c allocator-policy change). 3b only
establishes that de-fusing analysis-pass allocation is codegen-neutral on the
canonical case. Cases where liveness-promotion and the analysis backedge diverge
(the regression risk surface) are what the broad benchmark A/B on M1 must probe;
build `--release` twice (with/without the feature) and compare `bin/bench` +
optcarrot before considering a default flip / removing the analysis-pass
allocation outright.

### 13.8 §5 stage 3b benchmark verdict: REGRESSION — the analysis-pass backedge is load-bearing

**Corrects §13.7.** The canonical `x += i*0.5` loop produced identical asm off/on,
which I wrongly generalised to "codegen neutral." The real float-heavy benchmarks
say otherwise. M1 `bin/bench` (iter/sec, higher = faster) and an independent
x86-64 wall-clock A/B (seconds, lower = faster) both show 3b **regressing**:

| benchmark | base | 3b | verdict |
|---|---|---|---|
| mandelbrot (M1, iter/s) | 24.903 | 9.716 | **2.56× slower** |
| nbody (M1, iter/s) | 11.353 | 10.184 | ~10% slower |
| mandelbrot (x86-64, wall-clock) | 0.792 s | 1.479 s | **1.87× slower** |
| fib / aobench / bf / nqueen / sudoku / matmul / bedcov | — | — | flat |

So 3b fails the §13.4 gate (>2% regression on the hot float path). It stays
**default-off** — nothing shipped, and the gate did its job.

**The finding (the experiment's real value).** The analysis pre-pass's backedge
placement is **load-bearing** for float-heavy loops: it captures good loop-carried
xmm bindings (floats kept in registers across the back-edge) that liveness-driven
re-derivation (`use_float`) does **not** recover. The canonical loop was too
trivial to show this (its accumulator was already `Sf`/boxed-per-iteration, so
both paths agreed); mandelbrot/nbody carry several live floats across the loop
where the fixpoint's placement genuinely beats a from-scratch `use_float` pass.
This **refutes** the "backedge placement is redundant with liveness" hypothesis
from §13.7.

**Consequence for the architecture.** De-fusing allocation from the analysis pass
cannot simply *discard* the loop-carried placement and re-derive it from liveness
— that information is real and the greedy fixpoint computes it well. The allocator
pass (stage 3c / §3 Layer ②) must **reconstruct or carry forward** at least the
quality of the current analysis-pass backedge bindings, i.e. a proper loop-aware
allocation (linear-scan with loop-carried liveness), not the liveness-hint
promotion `use_float` does today. The keep-`Sf`-cache, drop-xmm-free demotion
property (stage-1 finding) and the backedge fixpoint together are the bar 3c must
clear. Net: 3b is retained as a **negative result / regression probe** behind its
feature flag; the next real step is designing 3c to match-or-beat the backedge,
not to replace it with liveness promotion.

## 14. §5 stage 3c design: a separable allocator that matches the backedge

Stage 3b established the hard constraint: the analysis pre-pass's loop **backedge
fixpoint** computes loop-carried xmm placements that are *load-bearing* for tight
float loops (mandelbrot 1.9–2.6× slower without them), and a naive
liveness-only re-derivation does not recover them. optcarrot was flat base-vs-3b,
so the placement quality that matters is localized to tight loops carrying several
live floats across the back-edge — that is the regression surface 3c must not
touch.

### 14.1 The realization: loop allocation is *already* a fixpoint

`analyse_backedge_fixpoint` (compile/loop_analysis.rs) iterates `analyse_loop`
(≤10 times) until the back-edge `AbstractState` stops changing (`be.equiv`). Each
iteration runs the per-BB walk with `apply_join` allocation, so the loop's
placement is the fixed point of the greedy per-merge allocator. The fusion is that
this single fixpoint co-evolves **types** *and* **placements**. Stages 2/3a proved
the type half is allocation-independent. So the separation is not "remove
allocation from the loop" (3b's mistake) — it is **sequence two fixpoints instead
of fusing one**:

```
fused today:     one fixpoint over (types + placements)        [analyse_loop ×N]
3c target:       fixpoint-1 over (types + liveness)   — NO placement
                 fixpoint-2 over (placements)         — greedy alloc on fixed types
```

Because fixpoint-2 runs the *same* greedy `apply_join` allocation, just sequenced
after type analysis rather than interleaved with it, it converges to the **same
loop-carried placements** — that is what makes the separation behaviour-preserving
and shadow-verifiable. 3b failed precisely because it replaced fixpoint-2 with a
single `use_float` liveness hint, not a placement fixpoint.

### 14.2 Decomposition

- **3c-i — safe / shadow-able: extract the allocation fixpoint as its own pass.**
  Run a type+liveness-only fixpoint first (the §3 Layer-① analysis; the type meet
  is already `join_ty`, the liveness is already separate), then run the greedy
  allocation fixpoint over the frozen types to produce placements. The allocator
  is now a distinct, pluggable component running today's greedy policy. Verify
  with the stage-1 placement replay shadow that the placements equal the fused
  result, slot-for-slot, including the backedge. No behaviour change — this is the
  real Layer-② extraction, and the thing 3b should have been. *Next implementable
  step (behind a feature until the shadow is green across the suite + benches).*

- **3c-ii — benchmark-gated: swap the greedy fixpoint for linear scan.** With the
  allocator extracted, replace the iterate-to-fixpoint greedy policy with a
  loop-aware linear-scan over live intervals (below). Gate against the
  mandelbrot/nbody bar (must match-or-beat the backedge) and optcarrot (must stay
  flat). Each policy change is an independent gated diff.

### 14.3 3c-ii allocator shape (the linear-scan)

Inputs (all allocation-independent, already computed by Layer ①):
- per-slot `Guarded` type at each program point;
- the representation decision **kept separate from placement**: "this slot is used
  as f64" (today's `use_float` liveness) decides *unboxed-float-ness*; the
  allocator then decides *which* xmm (or spill) — splitting `Sf`'s two jobs (mark
  vs register) per §3 Layer ②;
- live intervals per slot, with **loop-carried intervals** (live across the
  back-edge) flagged so the scan keeps them resident across the whole loop body —
  this is what reproduces the backedge's "float stays in xmm across iterations."

Output: a placement per (slot, point) + edge moves. The edge moves already exist —
`AbstractFrame::bridge` emits the φ-reconciliation MOV/swap/spill from
`(pred.mode, target.mode)`; the allocator only chooses the target registers and
the bridge lowers them (so the recently-fixed `fpr_swap` and the F/Sf/S bridge
arms are reused unchanged).

Two properties the scan must preserve (both already in the codebase):
1. **Free spill of read-only caches.** `try_alloc_fpr` phase-1 demotes an all-`Sf`
   register to `S` with *no* asm (stack is canonical). A linear-scan spill of an
   `Sf` interval must keep this — spilling a clean float cache costs nothing.
2. **Loop-carried priority.** The fixpoint today keeps loop-carried floats in xmm
   by construction; the scan must give intervals that span the back-edge higher
   priority than intra-loop temporaries when registers are scarce, or it will
   reintroduce the 3b regression.

### 14.4 Hard parts carried over

- **Deopt as a program point (§10 item 2).** Once placement is decided in
  fixpoint-2, a deopt's register/stack map must be reconstructed from the
  allocator's result at that pc, not the frozen `AsmDeopt` the transfer records
  carry today. 3c-i sidesteps this only if fixpoint-2 still emits in a forward
  walk that knows placements at each pc (it does, today). Full record-driven
  lowering (goal 1) is where the deopt-program-point work lands.
- **Representation vs placement split.** Today `Sf`/`F`/`S` bundle "unboxed?" with
  "which register?". 3c separates them: liveness marks unboxed-float slots; the
  allocator assigns registers. The typed IR carries the mark, not the register.

### 14.5 Why this is the right order

3c-i is the behaviour-preserving separation the whole §5 effort has been building
toward (decide/apply split, record stream, type-meet invariant all feed it), and
it is shadow-verifiable against the fused result. 3c-ii is the only step that may
regress, and it is gated on the exact benchmarks 3b flagged. 3b is retained as the
negative-result probe that calibrated the bar: any allocator that cannot match the
backedge on mandelbrot/nbody is not ready to land.

### 14.6 The 3b regression, diagnosed at the asm level (what 3c-ii must reproduce)

Diffing the JIT asm of the mandelbrot kernel (`for dummy in 0..ITER` with several
live floats) base-vs-3b pins the mechanism exactly. do_it grows from **340 → 412
instructions (+21%)** under 3b. The delta is not the back-edge box (both box the
same loop-carried results); it is the **operand loads inside the loop body**:

- **base** keeps the loop-invariant / loop-carried floats (`cr`, `ci`, …) resident
  in xmm across the loop, so each use is a direct `movq xmmA,xmmCr; mulsd …`:
  ```
  movq xmm9,xmm2 ; mulsd xmm9,xmm2      # cr already in xmm2
  ```
- **3b** demotes them to their boxed stack home, so *every use* re-loads and
  re-decodes the flonum (~10 insts: tag tests + the `sar/add/and/or/rol/movq`
  flonum-decode) before the `mulsd`:
  ```
  mov rdi,[rbp-N]; test rdi,1; jne…; test rdi,2; je…; …; rol rdi,0x3d; movq xmm2,rdi
  movq xmm3,xmm2 ; mulsd xmm3,xmm2
  ```

Why `use_float` does not recover it: the inner loop has more simultaneously-live
floats than the xmm pool, so the per-entry best-effort `try_set_new_Sf` promotion
loses the race for some of them and they stay boxed — re-decoded every use. The
backedge **fixpoint** instead converges on a stable assignment that keeps the
hot floats resident. So the missing quality is not the `Sf` *mark* (liveness has
it) but the *spill choice* under pressure.

**Spec for 3c-ii, made concrete.** The loop-aware linear scan must keep floats
whose live interval **spans the loop body** (loop-invariant operands and
loop-carried accumulators) resident in xmm with priority over intra-iteration
temporaries, i.e. choose spill victims by furthest next-use across the whole loop
— exactly what the backedge fixpoint approximates and what greedy per-entry
`use_float` does not. This is the property §14.3 named, now backed by the asm:
optcarrot stayed flat because its hot loops do not exceed the float pool, so the
spill choice never bites; mandelbrot/nbody do, so it dominates.

### 14.7 Negative result: the cheap spill-policy lever does not fix 3b

Tested the simplest 3c-ii lever directly: make `use_float`'s promotion spill an
unboxed float (`set_new_Sf` → VirtFPReg) instead of leaving it boxed in `S` when
the physical pool is full. **No-op** — the mandelbrot kernel's JIT asm was
byte-identical to plain 3b (412 insts, same box count). So `try_alloc_fpr` was
already succeeding for the slots `use_float` touches; the boxed-operand decodes
that cause the regression do **not** originate from `use_float`'s best-effort
fallback. Reverted.

Two refinements this pins down:
1. **The regression is specific to `for…in` loops.** A `while`-loop float kernel
   (zr/zi loop-carried) produces *identical* asm base-vs-3b — no regression.
   mandelbrot/nbody use `for…in` (inlined, multiple loop_starts within one iseq);
   that is where the placement divergence lives. The earlier "simple float loop is
   codegen-neutral" (§13.7) was right *and* misleading: the neutral case is the
   `while` loop; the `for…in` case is where 3b loses.
2. **It is not a local heuristic miss.** A point fix to the promotion policy
   cannot recover it, because the boxed operands are not the ones the promotion
   pass decides. The analysis-pass backedge fixpoint reaches a globally-consistent
   loop placement that a single forward `use_float` pass over a stripped
   (all-`S`) entry simply does not reconstruct.

**Conclusion — abandon "strip + re-derive" (3b) as the separation mechanism.**
3b's value was diagnostic (it calibrated the bar and proved the backedge is
load-bearing). The behaviour-preserving separation is §14.2's **3c-i: extract the
existing allocation fixpoint as its own pass, unchanged**, so the placements are
*identical* to today (shadow-verified, zero regression) — not stripped and
re-derived. A better *policy* (3c-ii linear scan) comes only after that seam
exists and is benchmark-gated. The `loop-type-only-entry` feature stays as the
negative-result probe.

## 15. §5 stage 3c-i implementation plan: extract the allocator, unchanged

The behaviour-preserving separation (per §14.7's conclusion). Surface map of where
the JIT allocates an xmm today (outside the merge, which is already decide/apply
split):

- operand loads: `load_fpr` / `load_fpr_fixnum` / `load_binary_fpr` /
  `fetch_float_assume` (state/read_slot.rs, compile/binary_op.rs) — allocate an
  xmm for an operand and emit the load (the per-use flonum decode seen in §14.6);
- destination defs: `def_F` / `def_Sf_float` (compile/binary_op.rs,
  method_call.rs, variables.rs, compile.rs);
- the merge: `apply_join`'s `TryFresh*` (already isolated, §5 stage 1).

**Every one of these funnels through two primitives** — `SlotState::try_alloc_fpr`
(phase-0 vacant / phase-1 Sf-demote) and `alloc_fpr` (+ phase-2 spill). So those
two are the universal allocation seam, and the loop-aware spill-victim choice that
3c-ii needs (demote/spill by furthest next-use across the loop, §14.3) lives
exactly in phase-1 of `try_alloc_fpr`.

Increment sequence (each behaviour-identical, suite + stage-1 shadow verified):

1. **Extract the register-selection policy** into a named `alloc_policy` unit:
   `try_alloc_fpr` / `alloc_fpr` move out of the `SlotState` impl into a child
   module taking `&mut SlotState`; the methods delegate. No field, no dispatch
   yet — the seam is the module boundary. *This increment.*
2. **Thread an `AllocCtx`** (the live-interval / loop-membership info 3c-ii's
   victim choice needs) into the policy, computed by the existing liveness pass.
   Default greedy ignores it → identical placements.
3. **Add the loop-aware policy** (3c-ii) behind the seam: phase-1 picks the
   victim with the furthest next-use instead of the first all-`Sf` register;
   gated on mandelbrot/nbody (match-or-beat backedge) + optcarrot (flat).

The full Layer-② "type-only fixpoint then allocation pass" (un-welding the type
computation from the operand-load/def handlers above) is the larger, later arc;
3c-i increments 1–3 deliver the swappable allocator and the measured win first,
since that is where the §14.6 regression and the latent base-case back-edge boxing
both live.

### 15.1 Two no-op experiments locate the lever: it is the merge, not the allocator

Increment 1 gave a clean `alloc_policy` seam, but two targeted experiments behind
it both came back **byte-identical** on the mandelbrot kernel:

- **`use_float` spill fallback** (§14.7): promote a pool-full float to an unboxed
  spill (`set_new_Sf`) instead of leaving it boxed `S`. No-op.
- **`liveness-aware-spill`**: in `try_alloc_fpr` phase 1, prefer demoting a clean
  `Sf` register whose slots are *dead* over one still *live* (using the `IsUsed`
  liveness `SlotState` already carries). No-op (340→340 insts, same decode count).

Two independent per-call allocation levers changing nothing means the
per-iteration boxing of loop-carried floats is **not** decided in `alloc_policy`
or `use_float`. It is decided at the **loop-header merge** — `decide_join` /
`apply_join` choose the loop-carried float's mode (`F` pure-xmm vs `Sf`/`S`
boxed-cache), and *that* is what the body inherits and re-decodes each iteration
(§14.6). The allocator only places what the merge already decided to keep
unboxed; it never gets the chance to keep a value the merge boxed.

**Redirect for 3c-ii.** The lever is the loop-header join's float placement: why
the meet demotes a loop-carried `F` to `Sf`/`S` instead of keeping it `F`. That is
in the already-split `decide_join` table (the `F/F`, `F/Sf`, `F/S` arms) — and it
governs both the 3b regression *and* the latent base-case back-edge box. Increment
1's `alloc_policy` seam stays as valid structural cleanup, but 3c-ii's measured
win must come from the join arms, not the spill policy. The next concrete step is
to read the loop-header join decision for a loop-carried float and determine
whether keeping it `F` across the back-edge (no boxed cache) is sound and cheaper.

### 15.2 jit-debug confirms: loop-carried floats *can* be `F`; pressure forces `S`

`jit-debug` on the float kernel shows the same loop compiled two ways:

- one specialization keeps the loop-carried floats `F(FPReg0)` / `F(FPReg1)` —
  pure xmm, **no per-iteration box**, back-edge is an xmm move;
- another puts them in `S(Value)` (boxed) while the loop-*invariant* operands take
  the physical pool as `Sf` — so the loop-carried values lose the pool and box
  every iteration.

So `F` for a loop-carried float is achievable and is the good outcome. The `S`
fallback comes from the loop-header join's `C/F` arm `TryFreshFElseS`
(join.rs:228) and the `F/F` arm `TryFreshFKeep`: both *try* a fresh/kept xmm and
**fall back to `S` only when no physical xmm is free**. The per-iteration box is
exactly that fallback firing under register pressure — the loop-carried value
losing the pool to other live values.

This closes the diagnosis loop: every lever (use_float promotion, phase-1 spill
victim, join arm) ultimately bottoms out at the same thing — **which live values
hold the physical pool across the loop**. Today that is decided greedily in
allocation order; the loop-carried/invariant floats must instead win the pool over
short-lived temporaries. There is no cheaper intermediate fix (three no-op probes
confirm it). 3c-ii is therefore necessarily the loop-aware **linear scan over live
intervals** of §14.3: rank pool occupancy by interval length / loop membership,
not allocation order. The seam (incr. 1) and the calibrated bar (mandelbrot/nbody
regress, optcarrot flat) are in place; what remains is the interval analysis +
the priority allocator, a substantial standalone implementation.

### 15.3 CORRECTION: it is not register pressure — the loop-entry merge discards the fixpoint's `F`

§15.2's "register pressure forces the `S` box" is **wrong**. Re-verified facts:

- **No pressure.** The float kernel uses 2 of 14 physical xmm; base mandelbrot
  uses 10 of 14. The pool is never exhausted.
- **Spilling is unboxed by design.** `alloc_fpr` phase-2 (`push_spill`) hands back
  a `VirtFPReg` that lives on the stack as a raw `f64` (`movsd`), never a boxed
  `Value`. A spilled float is *not* re-decoded. So boxing ≠ spilling.

The actual mechanism, from `jit-debug` on the kernel loop:

```
fixed: 1 { … [%3(zr): F(FPReg4)] [%4(zi): F(FPReg6)] … }   ← backedge fixpoint: F (good)
target:  { … [%3(zr): S(Value)]  [%4(zi): S(Value)]  … }   ← codegen loop-entry: S (boxed!)
```

The **back-edge fixpoint already computes the loop-carried floats as `F` (pure
xmm)** — the right answer. But codegen's loop-entry target is
`incoming.join(backedge)` (merge.rs), and the forward entry (`incoming`, the first
loop entry from outside) holds the loop-carried float as `S` — the boxed initial
value (`zr = 0.0` materialised boxed). `decide_join` has **no `S`/`F` arm**, so
`(S, F)` falls to the default `_ => SetS` → `S`. The merge therefore *discards the
fixpoint's `F`* and collapses the whole loop body to boxed, decoding+re-boxing the
loop-carried float every iteration — with 12 xmm sitting free.

So the lever is neither the allocator, the spill policy, nor register pressure: it
is the **loop-entry merge letting the forward entry's boxed initial value win over
the back-edge's unboxed steady-state placement**. The fix is to make the loop
header adopt the back-edge's `F`/`Sf` placement for loop-carried floats (a
one-time unbox of the forward entry at the pre-header bridge, which the bridge's
`S -> F`/`Sf` arms already emit) instead of `SetS`. This is a loop-header-local
change to how `incoming_context` builds the target, not a new allocator — and it
fixes the latent base-case box, not just the 3b regression. (The earlier no-op
probes were no-ops precisely because they targeted the allocator/promotion, while
the value was being boxed by the *merge* upstream of them.)

### 15.4 Prototype result: the box CAN be eliminated, but it is blocked by a TYPE loss, not placement

Prototyped the §15.3 fix (`loop-keep-float`): a new `S -> F` (and `Sf -> F`)
bridge arm + `keep_backedge_floats`, which re-adopts the back-edge fixpoint's `F`
for loop-carried floats the loop-entry merge collapsed.

- **Concept proven (x86-64).** With an *unguarded* promotion, the mandelbrot do_it
  loses **all** boxing: `call float_to_value` count 16 → **0**, the inner-loop
  body becomes pure xmm (`movq xmm,xmm; mulsd`), and the per-iteration
  flonum-decode moves to a single pre-header unbox. The optimisation is real.
- **But it is unsound as written**, and that exposed the actual blocker. The
  loop-carried float is typed **`S(Value)` at the loop-entry merge, not
  `S(Float)`** (verified in `jit-debug`: the pre-header forward entry holds
  `%3: S(Value)` even though it is `zr = 0.0`). The fixpoint correctly has it as
  `F` (Float); the codegen merge `SetS(join(Value, Float)) = Value` degrades it.
  Forcing `F` on a `Value`-typed slot then panics at the `C(non-float) -> F`
  bridge (a *different* slot that genuinely is a non-float const in some path) —
  and would be silently wrong for any slot that is actually sometimes non-float,
  since an `F` slot carries no runtime guard.
- **The sound guard (`guarded == Float`) makes it a no-op**, because the
  loop-carried floats are `Value`-typed: suite 1704/0, but mandelbrot is byte-
  identical to base (340/16). The placement fix has nothing to act on until the
  *type* is `Float`.

**So the lever is the type analysis, not placement or allocation.** A loop-carried
pure float (`zr = 0.0` then float arithmetic) is typed `Value` at the loop-entry
merge instead of `Float`; fix that precision loss and the (already-prototyped,
sound) `guarded == Float` promotion fires and removes the box — on the shipping
build, not just under 3b. This also finally explains the whole §15 thread: every
allocator/placement lever was downstream of a value the *type meet* had already
widened to `Value`. The `loop-keep-float` feature (default off, 1704/0) and the
`S -> F` / `Sf -> F` bridge arms are kept as the staging ground; the next step is
the loop-carried-float type precision fix. (aarch64 unverified here — no cross
toolchain in this container; the new bridge arms reuse `float_to_fpr` / `fpr_move`,
which both backends already lower, so they are arch-neutral by construction, but
this needs an M1 `bin/test` to confirm.)

### 15.5 Root cause + sound fix: loop-JIT conservative entry typing

The "type loss" is **not** a literal-handling bug. `jit-debug` on a loop-JIT
(`start:[:loop_start]`) shows the loop-entry forward state has *every* local as
`S(Value)` — because a **loop JIT does not see the values produced before the
loop** (`x = 0.0` ran in the VM). So a loop-carried float necessarily enters from
the VM as a conservative boxed `S(Value)`, even though the back-edge fixpoint
proves it is a `Float (F)`. The merge `join(S(Value), F)` → `S(Value)` then forces
the body to decode+rebox it every iteration (§15.3/§15.4).

**Sound fix (`loop-keep-float`, suite 1704/0, default-off).** At the loop header,
adopt the back-edge fixpoint's `F` for such a slot. The forward entry is unboxed
**once** at the pre-header by the new `S -> F` bridge arm, whose `float_to_fpr`
carries the **runtime float guard** (deopt if the VM value is not a float) — so the
specialization is sound for a runtime value. Soundness across *all* predecessors
is enforced by `keep_backedge_floats`'s `promotable(i)` gate: promote only when
every predecessor entry has a valid `_ -> F` bridge (`F`/`S`/`Sf`/float-`C`); a
non-float-`C` path is genuinely not a float, so it is left boxed (this is the gate
the earlier `guarded == Float` over-approximated, which made it a no-op — §15.4).

Result on the mandelbrot kernel: **`call float_to_value` 16 → 0**, the hot inner
loop becomes pure xmm, and the per-iteration flonum decode collapses to a single
guarded pre-header unbox. Correct on the whole suite (1704/0), including the
`fpr_swap`/bridge regression cases. Static `do_it` grows 340 → 450 (the decodes
move to the per-loop-entry pre-headers), so the win is dynamic (hot loop) — to be
confirmed by an M1 `--release` bench A/B (and aarch64, which reuses the same
`float_to_fpr`/`fpr_move` AsmIR ops). This is the first measured *improvement* over
base in the §5 line, and it lands as a guarded loop-entry type specialization —
the same shape YJIT uses — rather than a new allocator.

### 15.6 Confirmed on both arches

M1 (`bin/bench`, i/s) base vs `loop-keep-float`: **mandelbrot 24.326 → 27.372
(+12.5 %)**, everything else flat (fib/nbody/aobench/bf/nqueen/sudoku within
noise). Matches the x86-64 local `--release` result (mandelbrot ~0.80 s → ~0.70 s,
~12 %). So the guarded loop-entry float specialization is a real, arch-neutral win
(the `S -> F` / `Sf -> F` bridge arms reuse `float_to_fpr` / `fpr_move`, which both
backends already lower — confirmed on aarch64). `fib -1.6 %` is noise: fib has no
float loop, so `keep_backedge_floats` never fires and its codegen is byte-identical.

Remaining before default-on: a full `bin/bench` incl. **optcarrot** (headline) and
the other float loops (matmul/bedcov, which may also improve), confirming no
regression; then flip the cargo `default` to include the feature (and fold the
`S -> F` / `Sf -> F` bridge arms in unconditionally, as they are general-purpose).

### 15.7 Landed: default-on

Bench gate cleared on both arches, so the loop-entry float specialization is now
**default** (no feature flag): mandelbrot +12.5 %, matmul +2.4 %, optcarrot
184.8 → 186.2 fps (checksum unchanged, 59662), everything else flat, no
regressions; suite **1705/0**, mandelbrot do_it `call float_to_value` 0 in the
default build. `keep_backedge_floats` + the predecessor-gated promotion in
`incoming_context`, and the `S -> F` / `Sf -> F` bridge arms, are now
unconditional. The two experimental features (`loop-keep-float`,
`loop-type-only-entry`) and the dead `strip_fpr_to_stack` probe are removed;
`loop-type-only-entry`'s lesson (the analysis-pass backedge is load-bearing — a
naive type-only strip regresses 2.5×) is retained in §13–14 as the calibration
that led here. Added `test_loop_carried_float_kept_unboxed`.

### 15.8 §5 stage 3c-i increment 2: the allocator consults an explicit `AllocCtx`

Increment 1 (§15, commit `37ac994`) extracted the two universal xmm-allocation
primitives into the `alloc_policy` module. Increment 2 takes the next planned
step: thread an explicit **`AllocCtx`** into the policy so the spill-victim
decision consults a *named analysis-facts input* instead of reaching into the
fused `SlotState` ad hoc — the structural shape the Layer-② allocator needs.

Concretely, `try_alloc_fpr` phase 1 ("demote the first xmm whose linked slots are
all `Sf`") is refactored from a `for 0..len { … return first }` scan into

```rust
candidates.min_by_key(|&xmm| ctx.victim_rank(xmm))
```

The default `AllocCtx::victim_rank` is the physical-pool index, so `min_by_key`
selects the same lowest-index register the prior scan returned — **placements are
byte-identical**. Phase 0 (vacant) and phase 2 (spill) are policy-invariant and
unchanged. This is exactly the seam 3c-ii's loop-aware policy plugs in:
`victim_rank` becomes a furthest-next-use / non-loop-carried key fed by the
live-interval + loop-membership fields `AllocCtx` will carry, and **no allocation
call site changes** (operand loads, defs, and the merge's `apply_join` all funnel
through `set_new_*`/`try_set_new_*` → these two primitives).

**Verified behaviour-identical.** Built under `stress-spill-pool` (forces
`PHYS_FPR_POOL` to 2, so almost every Float-resident slot is driven through the
phase-1 demote path this refactor touches) and ran the lib suite with and without
the change: the pass/fail set is identical — 1671 passed; the 34 failures are the
pre-existing environment mismatches (CRuby version / timezone / missing
`bigdecimal` gem), present in both runs, the only `diff` being the wall-clock
line. So the refactor exercises the touched path under maximal pressure and does
not perturb a single placement.

**Honest scope note.** §15.1's two no-op probes already established that a
per-call spill-victim change is neutral on mandelbrot — the per-iteration box was
the *merge* discarding the fixpoint's `F` (fixed in §15.5–15.7), not the
allocator's victim order. So increment 3's *measured* win is likely small or
subsumed by the shipped merge fix; the value of increments 2–3 is the **structural
separation** (a swappable allocator fed by an explicit analysis-facts input,
decoupled from the placement state), not a fresh benchmark delta. The remaining
headline §5 arc stays the larger Layer-② extraction (§4 step 2): a type-only +
liveness fixpoint feeding a *distinct* allocation/lowering pass, which is where
"abstract interpretation + fixpoint search" is finally, fully separated from
physical register allocation.

### 15.9 Closing the allocator-policy axis: phase 1 already protects every `F`

A precise reading of `try_alloc_fpr` (the universal allocation seam, §15) settles
why **every** spill-victim probe in this thread (§14.7, §15.1's two no-ops) came
back neutral — and retires the 3c-ii "loop-aware victim" line as a performance
lever:

- **Phase 1 demotes only an all-`Sf` register.** It scans for an xmm whose linked
  slots are *all* `Sf` (Integer-def'd / Float-use'd, kept coerced — the stack
  already holds the canonical boxed value), demotes them to `S`, and reuses the
  freed xmm. The demote emits **no asm** (the stack is canonical) and the value
  reloads lazily on its next float use. An xmm holding any `F` slot is **skipped**.
- **An `F` (pure unboxed float) therefore never loses its xmm to phase 1**, and
  when the pool is genuinely full, phase 2 (`push_spill`) hands back a `VirtFPReg`
  that lives on the stack as a **raw `f64`** (`movsd`), still unboxed (§15.3). So
  **no allocation decision ever boxes an `F`.**

Consequently the only freedom the victim policy has is *which already-`Sf` cache
to drop* — a free, reversible, lazily-reloaded choice among loop-invariant
coerced operands. That cannot change the count of per-iteration boxes, which is
why §15.1's dead-vs-live probe and §14.7's spill-fallback were both byte-for-byte
no-ops. The per-iteration box was always upstream — the **merge** deciding a
loop-carried value's *representation* (`F` vs `Sf` vs `S`), fixed in §15.5–15.7 by
adopting the back-edge's `F` at the loop header.

**Verdict.** The allocator-policy axis (3c-i increment 3 / 3c-ii furthest-next-use)
is **closed as a performance lever**: it is provably neutral by the phase-1
all-`Sf` restriction. The `AllocCtx` seam (increment 2) is retained, but its
justification is corrected: it exists for **goal 3** (§3 Layer ②) — plugging in a
*different allocation strategy*, namely the VM-residual allocator's fixed "no
pool, every value in its stack home" convention — not a better JIT victim rank.

**Where the §5 work goes from here.** With the merge-representation lever shipped
(§15.7) and the allocator-policy lever shown neutral (this section), the remaining
separation is purely structural, not perf-seeking: the Layer-② extraction (§4
step 2) — run the `Guarded` + liveness fixpoint as a standalone, allocation-free
pass producing a typed IR, then a distinct allocation/lowering pass consumes it.
That is the last place "abstract interpretation + fixpoint search" and "physical
register allocation" remain interleaved (in the single forward codegen walk). It
is behaviour-preserving *by intent* (same final placements) but a large structural
change, and the one remaining benchmark-gated risk is the loop-entry placement
quality the analysis pre-pass currently seeds (§13.8) — now partly de-risked
because §15.7's `keep_backedge_floats` already reconstructs the load-bearing
loop-carried-`F` placement at the loop header from the back-edge frame.

## 16. Layer-② extraction: the concrete increment plan

§15 closed the *performance* line: the merge-representation lever shipped (§15.7)
and the allocator-policy lever is provably neutral (§15.9). What remains is the
**structural** separation goal — §4 step 2 / §3 Layer ② — and it is now the only
place "abstract interpretation + fixpoint search" and "physical register
allocation" are still interleaved: the single forward codegen walk, and the loop
**analysis pre-pass** that allocates while it computes types + liveness.

### 16.1 The target and the blocker

> **Target.** The analysis pass computes **types + liveness only** (no xmm
> allocation), producing a typed IR; a **distinct** allocation/lowering pass
> consumes it. The fixpoint searches over `Guarded` types (stage-2-proven
> allocation-independent); placement becomes a separate layer.

> **Blocker (why 3b regressed 2.5×, §13.8).** The analysis pre-pass's allocation
> is not dead — it computes the loop-carried **placement** (the back-edge frame),
> which the codegen pass consumes in two places:
> - **(a) float adoption** — `keep_backedge_floats` reads `backedge.mode(i) == F`
>   (§15.7);
> - **(b) placement reconciliation** — `target.join(backedge)` folds the
>   back-edge placement into the loop-entry target (the φ/edge-move seed).
>
> Naively stripping the allocation (3b) forced codegen to re-derive (a)+(b) from
> liveness alone, which is worse — hence the regression.

### 16.2 Strategy: decouple the consumers, then strip

Reroute each consumer of the analysis-pass **placement** to read the analysis-pass
**types + liveness** instead (both allocation-free, stage-2-proven). When *every*
consumer reads only types+liveness, the analysis-pass allocation has no consumer
and can be removed — at which point the analysis pass is the pure Layer-① pass.

| Increment | Change | Risk / gate |
|---|---|---|
| **L2-0** ✅ | Split `keep_backedge_floats` into *mechanism* + a caller-supplied *adoption policy* (`adopt(i)`). Default policy = placement-based (`mode == F`), **byte-identical**. | none (behaviour-preserving; suite) |
| **L2-1** | Swap the adoption policy (a) to **type + liveness**: adopt `F` when the back-edge type is `Float` **and** the slot is used-as-float in the loop (`Liveness::loop_used_as_float`), instead of reading `mode == F`. Decouples consumer (a) from the analysis-pass placement. | benchmark-gated (default-off flag → M1 bench → flip); §13.4 |
| **L2-2** | Decouple consumer (b): reconstruct the loop-carried xmm bindings in the **codegen** pass from types + liveness (a loop-aware allocation that matches the fixpoint's quality — the 3b regression surface). This is where the real linear-scan / loop-aware allocation lives; §15.9 (phase-1 protects every `F`) + L2-1's typed float adoption are the tools that make it tractable now. | benchmark-gated; high |
| **L2-3** | With (a)+(b) reading only types+liveness, make `analyse_loop` **type + liveness only** (drop `apply_join` allocation and the handlers' `def_F`/`load_fpr` placement). It returns `(Liveness, backedge_types)` — a pure typed IR. Deopt-as-program-point (§13.6) lands here. | benchmark-gated; high |
| **L2-4** | Standalone allocation/lowering pass emitting `LInst` (goal 1); add the VM-residual fixed-convention allocator (goal 3, the reason the `AllocCtx` seam exists — §15.9). | research |

Each increment is correctness-verified by the exact CRuby-diff suite here; the
behaviour-changing ones (L2-1/2/3) are benchmark-gated on M1 (mandelbrot / nbody /
optcarrot, no >2 % regression, §13.4) before any default flip. The order keeps the
high-risk placement reconstruction (L2-2) behind the cheap, well-understood float
decoupling (L2-1), and the irreversible analysis-pass strip (L2-3) last.

### 16.3 L2-0 landed (this step)

`keep_backedge_floats` no longer hard-codes the adoption condition: it takes an
`adopt: impl Fn(SlotId) -> bool` policy from the caller and applies the mechanism
("adopt `F` for a loop-carried slot the boxed loop-entry left `S`/`Sf`, when a
physical xmm is free"). `incoming_context` supplies the current placement-based
policy (`be.mode(i) == F`), so the result is byte-identical — but the
representation **decision** is now a named, swappable policy at the call site,
exactly the Layer-② seam L2-1 plugs the type+liveness policy into. Suite green.

### 16.4 L2-1 landed behind `layer2-float-by-type` (benchmark probe + a design finding)

L2-1 swaps consumer (a)'s adoption policy from placement (`be.mode(i) == F`) to
the allocation-free **type + liveness** signal (`be.is_float_typed(i)` ∧ the slot
is in `Liveness::loop_used_as_float`), behind a default-off feature so the shipping
build stays byte-identical.

Verified:
- **Default (off): byte-identical** — the type policy is `#[cfg]`-compiled out.
- **Feature on: correct** — full lib suite identical to baseline (1671 passed; the
  34 failures are the pre-existing env mismatches), so the CRuby-diff oracle holds:
  the type policy never produces a wrong result.
- **It is not a no-op** — `emit-asm` on the canonical loop-carried-float kernel
  (`x = x*1.5 + i*0.5; y = y - x*0.25`) differs (code 347 → 379 bytes,
  ~176 normalised asm lines). Both keep the **hot loop body at zero boxing**
  (`float_to_value` count 0 either way — §15.7 already won that); the diff is
  entirely in the **pre-header**.

**Design finding (why L2-1 is a *probe*, not an obvious win).** The type+liveness
policy promotes the *superset* "`Float`-typed ∧ used-as-float," whereas the
placement policy promotes exactly the floats the back-edge fixpoint *chose* to
keep in `F`. On the no-pressure kernel the superset is strictly larger, so L2-1
adds pre-header unboxes that buy nothing in the body — a *mild regression risk*. In
other words, consumer (a) has the same load-bearing-placement property §13.8 found
for consumer (b): the fixpoint's `F`-selection is real information, and a naive
type+liveness re-derivation over-approximates it. `try_set_new_F`'s self-limiting
(promote only when a physical xmm is free) bounds the damage but does not restore
the selectivity.

**Consequence for the plan.** A quality-preserving consumer-(a) decoupling must
carry the fixpoint's per-slot **`F`-preference** forward as an explicit
*allocation-free annotation* on the typed IR (a derived bit computed during the
fixpoint), not re-derive it from `type ∧ liveness`. That annotation is the same
object L2-2 needs for consumer (b) — so L2-1 and L2-2 share one missing piece: a
**loop-carried-`F` preference set** produced by the analysis pass as typed-IR
metadata (distinct from the live xmm placement). L2-1 stays a default-off
benchmark probe (mirroring `loop-type-only-entry` in §13.8) until the M1
mandelbrot / nbody / optcarrot A/B says whether the superset is neutral in
practice; the likely outcome, per this finding, is that the next real increment is
the `F`-preference annotation, after which both consumers decouple cleanly.

### 16.5 Sharpening §16.4: the `F`-selection *is* allocation — the real fork

§16.4 floated carrying the fixpoint's `F`-preference forward as an "allocation-free
annotation (a bit computed during the fixpoint)." That phrasing is imprecise and
worth correcting, because it changes what L2-1's bench actually decides.

Two facts settle it:

1. **Consumer (a) already reads the analysis *output*, not the live codegen
   placement.** `incoming_context` derives the adoption set from
   `loop_info(bbid)`'s stored back-edge frame (`be.mode(i) == F`) — the analysis
   pre-pass's result, cloned into `backedge_for_floats`. So the dependency we are
   trying to remove is specifically on the back-edge frame's *`F`-placement*.
2. **That `F`-placement is produced by allocation, and §13.8 proved the selection
   is load-bearing** (liveness re-derivation regresses 2.5×). The fixpoint chooses
   *which* `Float`-typed, used-as-float slots win the limited pool — and that
   choice *is* a register-allocation decision, not a type/liveness fact.

So a "bit computed during the fixpoint" is just `be.mode(i) == F` renamed: it still
requires the analysis pass to allocate. There is **no** allocation-free annotation
that reproduces the selection byte-for-byte — the selection is allocation.

**The real fork L2-1's M1 bench decides:**

- **(i) Approximate, allocation-free** — accept L2-1's `type ∧ liveness` superset
  (and let `try_set_new_F`'s self-limit bound the over-promotion). If the
  mandelbrot / nbody / optcarrot A/B is within noise, this *is* the consumer-(a)
  decoupling: the analysis pass no longer needs to allocate *for consumer (a)*.
- **(ii) Exact, allocation-bearing** — if L2-1 regresses (the §16.4 over-promotion
  finding predicts a mild one), the `F`-selection genuinely needs allocation
  quality, so consumer (a) cannot be decoupled in isolation. It folds into **L2-2**:
  the codegen-side loop-aware allocator reproduces the fixpoint's selection (a real
  linear scan over the loop's live intervals), and *that* pass owns the `F` choice
  for both consumers (a) and (b) at once.

Either way the next concrete action is the **L2-1 A/B on M1**; its result picks
(i) vs (ii) and is the first hard data on whether the loop-carried `F`-selection
can be made allocation-free at all. (This supersedes §16.4's "shared `F`-preference
annotation" as the immediate next step — there is no such free annotation; there
is a bench that tells us whether we need the L2-2 allocator.)

### 16.6 L2-1 bench verdict (x86-64): REGRESSION — path (ii) confirmed

x86-64 `--release` wall-clock A/B (default vs `layer2-float-by-type`, steady-state
median of 6/5 runs, seconds, lower = faster):

| benchmark | default | L2-1 | verdict |
|---|---|---|---|
| so_mandelbrot | ~0.181 | ~0.189 | **~4 % slower** |
| so_nbody | ~0.254 | ~0.260 | **~2.4 % slower** |
| app_fib (no float loop) | ~0.154 | ~0.150 | flat (noise; `keep_backedge_floats` never fires) |

L2-1 fails the §13.4 gate (>2 %) on both float loops — exactly the §16.4
over-promotion prediction, now measured. So **§16.5's fork resolves to (ii)**: the
loop-carried `F`-selection genuinely needs allocation quality; the allocation-free
`type ∧ liveness` superset cannot reproduce it (it promotes loop-invariant /
fixpoint-rejected floats, adding pre-header unboxes that cost without body
benefit). L2-1 stays **default-off as a negative-result probe** (mirroring
`loop-type-only-entry`, §13.8), and `keep_backedge_floats`'s L2-0 mechanism/policy
split is retained as the clean seam.

**What this proves.** This is the *second* empirical confirmation — after §13.8 for
consumer (b) — that the greedy fixpoint's loop-carried-`F` selection is
load-bearing and **not reproducible allocation-free**. Both consumers (a) and (b)
need it. So decoupling them is not "read types instead of placement"; it requires a
**codegen-side loop-aware allocator that reproduces the fixpoint's selection**
(L2-2). And critically, since the fixpoint's greedy selection is already good
(§15.9: it never boxes an `F`; §15.7 shipped the merge win), L2-2 is **parity at
best on perf** — its sole payoff is the goal-3 enabler (a swappable allocator for
VM-residual codegen / the unified DSL), at real reimplementation risk (it must
match the greedy fixpoint byte-for-byte on the hot float loops or regress).

**Strategic state of the §5 line.** The perf wins are shipped (§15.7) and the two
remaining fusion points (consumer (a) here, consumer (b) §13.8) are both proven to
need allocation quality. The separation is therefore *complete as far as it pays
for itself*: what remains (L2-2/L2-3 — reimplement the greedy loop-carried
selection as a standalone allocation pass, then strip the analysis-pass allocation)
is a large, perf-neutral, regression-risky refactor whose only return is the
research-grade goal-3. That is a deliberate investment decision, not an incremental
win — recorded here so the call is explicit rather than drifted into.

## 17. L2-2 design: the swappable allocator for goal 3 (the right scoping)

The user chose to invest in L2-2 for **goal 3** (VM-residual codegen / the unified
interpreter-JIT DSL). The first design task is scoping it correctly, because the
naïve scope hits the §16.6 wall and goal 3 does not require crossing it.

### 17.1 Key reframing: goal 3 does not need to reproduce the fixpoint's selection

§16.6 proved that reproducing the JIT fixpoint's loop-carried-`F` selection
*allocation-free* is hard (it is greedy/emergent) and perf-neutral. But that is the
requirement of **§4-step-2 for the JIT** (eliminate the JIT's own fixpoint), which
is *not* goal 3. Goal 3 needs a **swappable allocation strategy** so a *different*
policy plugs in; the JIT keeps its fixpoint as the default strategy. Two strategies:

- **`JitGreedy` (default)** — today's behaviour: the xmm pool + the greedy fixpoint
  selection (`F`/`Sf` where profitable, `S` otherwise). Must stay byte-identical.
- **`VmResidual`** — the VM's fixed convention: **no pool, no unboxed
  representation; every value lives boxed in its stack home** (`S`). There is *no
  selection problem* here — it is the trivial "always `S`" policy. This is the
  residual the partial-evaluator emits for the VM (⊤ types, no IC narrowing).

So L2-2 is **not** "reimplement the fixpoint's selection." It is "thread an
allocation **strategy** through the representation/placement decisions; default
`JitGreedy` (byte-identical); add `VmResidual`." The §16.6 reproduction problem is
sidestepped — the JIT keeps its fixpoint.

### 17.2 Where the strategy must be consulted

`VmResidual` is a **representation-level** decision, not just a pool-size knob: it
must prevent *any* `F`/`Sf` from being created, so every value stays `S`. The sites
that create an unboxed float representation, and what each does under `VmResidual`:

| Site | `JitGreedy` (today) | `VmResidual` |
|---|---|---|
| `try_set_new_F` / `try_set_new_Sf` | allocate xmm if free | return `None` → caller keeps `S` |
| `def_F` / `def_Sf_float` (mandatory) | `alloc_fpr` (pool or spill) | **must not exist** — the float-op handler emits the *boxed* op (VM-style) instead |
| `use_float` (liveness promotion) | `try_set_new_Sf` | no-op (skip promotion) |
| merge `apply_join` `TryFresh*` / `keep_backedge_floats` | allocate / adopt `F` | skip (stay `S`) |

The `try_*` and merge sites are easy (they already have a "stay `S`" fallback). The
hard one is **`def_F`**: a float binary op currently *commits* to `F` and emits xmm
arithmetic. Under `VmResidual` the same handler must emit the boxed path (the VM's
`float + float → boxed Float`). That is exactly generating VM-equivalent code — the
goal-3 payoff — and it touches every float-op handler. So the bulk of L2-2 is
giving the float-op handlers a `VmResidual` lowering, gated so `JitGreedy` is
untouched.

### 17.3 Increment plan (each `JitGreedy`-byte-identical; M1-gated)

- **L2-2.1** — define `AllocStrategy { JitGreedy, VmResidual }` and thread it on
  `AllocCtx` (default `JitGreedy`). Route the *easy* sites (`try_set_new_*`,
  `use_float`, the merge `TryFresh*`/`keep_backedge_floats`) through it: under
  `VmResidual` they skip xmm creation. `JitGreedy` byte-identical. `VmResidual`
  not yet constructed (so float-op handlers still `def_F` — incomplete, but the
  representation seam exists and is exercised by a unit smoke test).
- **L2-2.2** — give the float-op handlers (`binop_float`, `gen_cmp_float`, the
  unary/`def_F` consumers) a `VmResidual` boxed lowering, selected by the strategy.
  This is the bulk; `JitGreedy` path unchanged at each.
- **L2-2.3** — goal-3 spike: drive a `VmResidual` codegen for one float bytecode and
  validate its output equals the VM's, end to end.
- **L2-3** (separate, optional, *not* goal 3) — only if we later want the JIT
  fixpoint gone: the §16.6 loop-aware reproduction. Parked behind goal 3.

This order delivers goal 3's swappable allocator without paying the §16.6 cost, and
keeps every step a `JitGreedy`-byte-identical, M1-benchable diff. L2-2.1 is the next
code increment.

### 17.4 Deferred: goal-3 / `VmResidual` not pursued now (per user)

Per the user, **VM support is not needed at this point**, so the goal-3 /
`VmResidual` direction designed in §17.1–17.3 is **deferred**. The L2-2.1 code
(the `AllocStrategy { JitGreedy, VmResidual }` enum, the `SlotState.alloc_strategy`
field, the `try_alloc_fpr` VM gate, and the `force-vm-residual` validation feature)
has been **reverted** to keep the tree focused on the JIT. §17.1–17.3 remain as the
record of the goal-3 plan for whenever VM-residual codegen is revisited.

**Refocus.** The active goal returns to the *JIT-internal*, behaviour-preserving
separation of "abstract interpretation + fixpoint" from "physical register
allocation" — §4 step 2 done as a **structural refactor that preserves the current
greedy placement**, not the VM application. Consequence of deferring goal 3: the
deepest remaining separation (un-welding the per-instruction handlers'
type/representation decision from their allocation+emission — e.g. `binop_float` =
`load_binary_ret_fpr` (alloc) + `fpr_binop` (emit)) loses its near-term *functional*
payoff (it was the enabler for the swappable VM allocator). What is already done
de-fuses the merge and the allocation seam (data-model split `place`/`ty`,
`alloc_policy`/`AllocCtx`, `decide_join`/`apply_join`, the `keep_backedge_floats`
mechanism/policy split); the remaining handler-level un-welding is a large,
IR-introducing refactor whose value, with goal 3 deferred, is architectural
cleanliness rather than a feature. That trade-off is the open decision.

## 18. Handler-level separation (JIT-internal, behaviour-preserving)

With goal-3 deferred (§17.4), the remaining fusion is *inside* the per-instruction
handlers: each float-op handler interleaves the **type/representation decision**
(what the result is, what representation) with **allocation** (which xmm) and
**emission** (the AsmInst). Un-welding these — behaviour-preserving, the current
greedy placement unchanged — is the last structural step of §4-step-2 done as a
refactor.

### 18.1 The template: decision (Layer-①) vs execution (Layer-②), on `binop_float`

`binop_float` is split into a pure decision and an execution:

- `plan_binop_float(&self, …) -> FloatBinOpPlan` — a **pure, `&self`,
  allocation-free** function (Layer-①): it decides `Fold(f64)` (both operands
  const floats, result a flonum immediate) vs `FprOp`, **without** allocating an
  xmm or emitting.
- `binop_float` — **executes** the plan (Layer-②): `Fold` → `def_C_float` (a pure
  constant, no xmm); `FprOp` → `load_binary_ret_fpr` (alloc) + `fpr_binop` (emit).

What this concretely fixes: the original folded the *decision* and a *side effect*
together — `… && self.def_C_float(dst, result)` put the constant **definition**
inside the `if` condition (the fold "succeeded" only if `def_C_float` mutated the
slot). The split lifts the flonum-representability check into the pure `plan_*`
(`Immediate::flonum(result).is_some()`) so the decision is a value with no side
effect, and the definition happens only in the execute half. Behaviour-identical
(suite 1671 passed, baseline-identical failure set; the 34 are the env mismatches).

### 18.2 Scope: the fold decision separates cleanly; the xmm path needs a virtual-operand IR

This lifts out the **fold** decision (a pure Layer-① constant). The `FprOp`
execution still fuses allocation and emission *internally*: `load_binary_ret_fpr`
both *allocates* an xmm per operand/dest **and** *emits* the load, with the xmm
identities threading through (operand pins, `dst == lhs` aliasing). Separating
alloc from emit there requires a **virtual-operand IR** — the float op recorded
with *slot* operands, lowered to physical xmm by a distinct allocation pass —
because the allocation *produces* the operands the emission consumes. That is the
substantial next step; this increment establishes the decision/execution seam and
the `FloatBinOpPlan` value that a virtual-operand lowering would carry. The same
`plan_*`/execute shape applies to the other handlers that fold-or-emit
(`gen_cmp_*`, `binop_integer`) as they are migrated.

### 18.3 Correction: the FprOp alloc/emit is already split (§9/§11); (b) is record-driven lowering

§18.2 claimed the `FprOp` path "still fuses allocation and emission internally."
Tracing it precisely, **that is wrong** — the primitive-level split is already done
by the §9/§11 transfer work:

- `load_fpr` / `load_fpr_fixnum` are each `(*_state)` + `transfer(TransferIR::…)`:
  the **state half** (`load_fpr_state`) allocates the xmm and binds the slot (pure
  abstract-state mutation, no emission); the **record** (`TransferIR::FprLoad`,
  carrying its deopt as a `DeoptPoint` *program point*) is what emits. `transfer()`
  collects the record into `self.transfers` *and* (today) emits it inline, with a
  debug **shadow check** proving the record replays to the identical
  `AsmInst`/`SideExit` — i.e. the record is self-contained.
- `def_F` (the dst) is **pure allocation** (no emission); `fpr_binop` is **pure
  emission** (no allocation — its operands are already placed).

So in `binop_float`'s `FprOp` arm, every call is *either* allocation (state) *or*
emission (a record / a pure `AsmInst`); they are not fused, only sequenced. And the
handler already consumes **virtual operands** (`FBinOpInfo` = slots) and produces
physical xmm — it *is* a virtual→physical lowering. §18.2's "needs a
virtual-operand IR" mis-stated the situation.

**What (b) actually is: record-driven lowering (the deferred two-pass).** The
transfer records are *collected but not yet consumed* — emission still happens
inline during the analysis/allocation walk. The remaining separation is to make
emission a **distinct pass that replays the record stream**, instead of emitting
inline. The records already carry everything needed (deopt program points; physical
regs assigned during the walk), so replay is behaviour-preserving — the §11 shadow
proves it per-call.

**The one concrete blocker.** The stream is not yet *complete*: transfers
(value-movements) are recorded, but **operations** (`fpr_binop`, `FloatCmp`, the
integer ops) and dst-defs are emitted directly to `inst`, outside `transfers`. A
deferred replay of `transfers` alone would drop the ops and lose their ordering
relative to the loads. So the prerequisite for record-driven lowering is a
**single ordered record stream that includes the operations**, after which a pass-2
replay can emit the whole method from records.

**Scope honesty.** This is a large, global restructure (route all emission through
one ordered record stream; then split the codegen walk into collect-then-replay),
not an op-by-op behaviour-preserving edit — the value-movement primitives are
already split, so the increments left are (i) inert scaffolding (widen the record
stream to cover ops, which does nothing until a replay pass exists) or (ii) the
replay pass itself (the deferred two-pass). With goal 3 deferred (§17.4), this
restructure's payoff is purely architectural. The boundary of the *primitive-level*
separation has been reached; crossing into the deferred two-pass is the open,
large-investment decision.

## 19. (B) Record-driven lowering: toward a single ordered codegen record

Per the user the goal is **clean architectural layering so optimization logic is
easy to add**. The record stream (`TransferIR`, built by §9/§11) is the emerging IR
layer between the handlers and the AsmIR/machine code. Today it covers
value-movement *transfers* only; *operations* emit directly to `inst`, outside the
stream (the §18.3 blocker). (B) unifies them into one ordered stream, then makes
lowering **replay** it.

### 19.1 Step 1: operations join the record stream (float binop)

The first operation routed in: the float binary op. `binop_float`'s `FprOp` arm now
emits `TransferIR::FloatBinOp { kind, lhs, rhs, dst }` through `transfer()` instead
of a direct `fpr_binop`. The record is **pure data** (Clone, no closure, no
abstract-state read), so `transfer()` collects it into the `transfers` stream and
the debug **shadow check replays it to the identical `AsmInst`** — exactly like the
transfer records. Behaviour-preserving: lib suite 1671 passed (baseline-identical
failure set), zero replay mismatches.

This proves the pattern: **data-only operations can join the record stream and be
shadow-verified.** In analysis mode `transfer()` skips emission (the op has no
state half, and the pass discards its `AsmIr`), matching the prior behaviour.

### 19.2 The plan to a replayable stream

1. **Migrate the data-only operations to records** — float/integer arithmetic and
   comparison (`FloatCmp`, `IntegerCmp`, `IntegerBinOp`, …). Each: add a record
   variant, route the handler through `transfer()`, verify via the shadow check.
   *(This step: float binop.)*
2. **Generalize the type** — once it carries both transfers and ops, rename
   `TransferIR` to a unified `LowerRecord` / `CodegenIR`; `Transfer` no longer fits.
3. **Handle the closure-carrying ops** (inlined calls, C-func trampolines): these
   variants of `AsmInst` are *not* `Clone` (they own `FnOnce` closures), so they
   cannot be recorded the same way. Either represent the inlined body as a record
   sub-stream (data), or keep a move-only escape-hatch variant the replay emits in
   place. This is the hard part of the unification.
4. **Build the replay (lowering) pass** — collect the full ordered stream during
   the codegen walk, then emit `inst` by replaying the records, removing the inline
   emit. The stream becomes the clean IR layer that optimization passes operate on
   (the goal). Behaviour-preserving by the per-record shadow property.

Each step (1) is behaviour-preserving and shadow/suite-verified; the risk
concentrates in (3) and the (4) switch. Starting from the data-only ops keeps the
early increments safe while the stream grows toward completeness.

## 20. (B) De-closuring the array-index codegen

The array integer-index read/assign sites were the densest remaining
`ir.inline(|gen| …)` closures on the hot path. The 8 closures (x86 + aarch64,
read + assign, with/without bounds info) became **typed data records**
`AsmInst::ArrayIndex` / `ArrayIndexAssign` (carrying an `ArrayIndexKind`), with
the moved closure bodies living in per-arch `gen_array_index` /
`gen_array_index_assign`. The two `array_integer_index{,_assign}` builders in
`jitgen/compile/index.rs` are now cfg-gated twins that just `ir.push` the same
arch-neutral variant.

This is independently valuable (it removes opaque closures from `inst`, making
those ops inspectable by any pass over the stream) and is the concrete precedent
for §19's step (3): a closure-carrying op *can* be turned into inspectable data
when its body is arch-uniform enough. Behaviour-preserving (lib suite identical,
zero replay mismatches). Commit `818c15c`.

## 21. The realization: `inst` already *is* the replayable stream — add the seam

§19.2's step (4) sketched a *move-based rewrite* — records own the `AsmInst`s, a
replay pass rebuilds `inst` — as the way to reach "a clean IR layer optimization
passes operate on". That rewrite is **unnecessary**: `inst: Vec<AsmInst>` *is*
already the ordered, replayable stream. Handlers build it during the
analysis/codegen walk (`traceir_to_asmir`); `Codegen::gen_machine_code` replays it
afterwards to emit machine code (`compile_asmir`, one match arm per `AsmInst`). The
hot ops are already **typed** variants in it (`FloatBinOp`, `IntegerBinOp`,
`IntegerCmp`, `ArrayIndex`, … — §19/§20), directly inspectable.

So Path 2's actual goal — *a place to add optimization logic over a typed,
arch-neutral instruction stream* — is reached simply by **adding an optimization
hook over `inst` between AsmIR construction and emission**, not by the §19.2(4)
rewrite, and not by making `AsmInst: Clone` (blocked anyway: ~52 inline-builtin
closures across the `builtins/*.rs` use `ir.inline`, not the 8 array-index sites).

### 21.1 The seam

`AsmIr::optimize_peephole(&mut self) -> usize` (in `asmir.rs`, where `inst` is
private) runs peephole passes over a block's stream and returns the count removed.
`gen_machine_code` calls it just after `frame.detach_ir()` — over every main block
*and* every inline/outline bridge `AsmIr` — before the emission loop. Optimizing
the bridges before `thread_empty_outline_bridges` lets one that collapses to
nothing be jump-threaded away as usual.

**Soundness of dropping instructions here:** branch targets are *labels*
(`JitLabel`); deopt targets index the `side_exit` vec (`AsmEvict` / `AsmDeopt`),
**never `inst` positions**. So removing an instruction cannot perturb control-flow
or deopt resolution. (`BcIndex` source-map markers are likewise emission-time, not
position-indexed.) No block can be emptied by the pass — each carries a leading
`Label` — so `live_bb` / `is_empty` accounting is unaffected.

### 21.2 Pass 1: self-move elimination

`AsmInst::is_self_move` flags `RegMove(r, r)` / `FprMove(r, r)` (`dst == src`); the
pass `retain`s the rest. A self-move is inert (`mov r, r` does not even set flags;
`movapd x, x` is a no-op). Under the `jit-log` feature the call site prints the
removed count.

**Observed:** across the lib suite and the benchmarks (`app_fib`, `so_nbody`,
`app_aobench`, …) the count is **0** — the allocator does not currently emit
self-moves (its move-insertion guards against `src == dst`). That is the expected
"safe first pass that rarely fires"; the **seam is the deliverable** — the typed
stream now has a layer boundary where real passes (peephole arithmetic identities,
dead `FprSave`-pair removal, redundant load/store elision) attach with no further
plumbing. Behaviour-preserving: lib suite **1705 passed, 0 failed**
(this environment's CRuby-4.0.2 baseline), zero `transfer()` replay mismatches.

## 22. The ①/②/③ reframing and the `PhysMap` seam (phase-0/1)

A fresh framing of the whole §5 effort, stated as three phases:

> **①** abstract interpretation + fixpoint subtyping **and assignment to virtual
> registers** (types + liveness + representation + a *virtual* FP register per
> value); **②** **physical** register allocation (virtual → the 14-wide `xmm`
> pool ∪ spill slots); **③** machine-code generation.

This is a better cut than §3's "analysis with NO locations vs allocation+lowering"
because it puts the **representation decision in ①**, which dissolves the wall
§13.8 and §16.6 hit twice.

### 22.1 Why this cut dissolves the §13.8/§16.6 wall

Those sections proved the fixpoint's loop-carried-`F` *selection* is load-bearing
and **not** reproducible allocation-free (a liveness re-derivation regresses
2.5×). But that "selection" fuses two decisions:

- **(A) representation** — is a value *unboxed* (`F`/`Sf`) or *boxed* (`S`)? This
  is the load-bearing part (§14.6: boxing → a ~10-inst flonum decode every use).
- **(B) physical placement** — which `xmm`, and who spills under pressure? §15.9
  proved this is **perf-neutral** (phase-1 only demotes all-`Sf` caches; an `F` is
  never boxed; pool overflow spills `F` as **raw f64**, never re-decoded).

The ①/②/③ cut keeps **(A) in ①** (the fixpoint, unchanged) and moves only the
perf-neutral **(B) into ②**. We never try to reproduce the selection
allocation-free — that was the 3b/L2-1 mistake. So the regression driver (boxing)
is *structurally absent* from ②.

### 22.2 The f64-spill axiom (②'s cost model)

The defining Float constraint — *boxing/unboxing is expensive, so an overflowing
unboxed float spills as f64 rather than boxing* — fixes ②'s spill cost model and
its vocabulary. ② may place a value in {physical `xmm`, raw-f64 spill slot, or
(for an `Sf` cache) drop-the-cache}; **boxing an `F` is not in ②'s vocabulary**:

| ② action | cost now / on next use |
|---|---|
| box an `F` | **forbidden (∞)** |
| drop an `Sf` coercion cache (→`S`) | 0 now / one flonum *decode* on next float use |
| raw-f64 spill of an `F` | one `movsd` now / one `movsd` on next use |
| keep loop-carried `F`/`Sf` resident | preferred under pressure (§14.6 bar) |

The worst case ② can produce is therefore a `movsd` (or one decode for an `Sf`
cache), **never** the per-use decode storm that sank 3b — the downside is capped
by construction. Re-cast in canonical/cache terms, the three `LinkMode`s split
cleanly across the layers: `F` = unboxed-canonical (① picks it; ② places it in
`xmm` or f64-spill); `S`/`Sf` = boxed-canonical-with-optional-cache (① marks
"float-used / cache-eligible"; **② decides whether the coercion cache is
materialised in an `xmm` (`Sf`) or dropped (`S`)**).

**Key soundness fact.** Because pressure never changes representation today
(§15.9: spill ≠ box), `F`/`Sf`/`S` counts are already pressure-independent — which
is *exactly* the precondition that lets "representation in ①, placement in ②" be
behaviour-preserving. (Open check before P2: confirm the "`Sf` canonical = boxed"
premise holds across all arms, incl. `keep_backedge_floats`'s `Sf→F` promotion,
§15.7.)

### 22.3 The linchpin: decouple `FPReg` from its physical slot

The one structural blocker is that `FPReg(usize)` **conflates** the virtual id
with the physical slot: `loc()` was `id < PHYS_FPR_POOL ? xmm(id+2) : spill`, so
the pool-vs-spill decision *is* the `FPReg` number, chosen greedily inside the ①
fixpoint by `try_alloc_fpr`. (This corrects §1's "`FPReg` is already a virtual
register" — it is pool-number-encoded, not a true virtual register.) The fix is a
`PhysMap: FPReg → FPRegLoc` produced by ②; ① assigns *unbounded* virtual
`FPReg`s, ② maps them to physical, ③ resolves through the map.

### 22.4 Increment plan

| Step | Change | Risk |
|---|---|---|
| **P0** ✅ | Introduce `PhysMap` (codegen.rs): the single `resolve(FPReg) -> FPRegLoc` chokepoint, today the pool-vs-spill formula. | none |
| **P1** ✅ | Route every emission-site `FPReg::loc(base)` (22 sites, both arches) through `PhysMap::resolve`; delete `FPReg::loc`. | none |
| **P2** | Make ① assign *unbounded* virtual `FPReg`s (drop `try_alloc_fpr`'s pool/spill phases from the fixpoint); representation stays in ①. Paired with P3 behind a feature. | high · bench |
| **P3** | ② = a distinct pass that reproduces today's greedy `FPReg→FPRegLoc` *exactly*; verify byte-identical via the stage-1 placement shadow. The real, zero-regression separation. | high · shadow + M1 bench |
| **P4** | Swap ②'s greedy for a loop-aware **linear scan** over live intervals (the f64-spill cost model of §22.2; loop-carried priority §14.3). Parity-at-best on perf (§16.6). | bench-gated |
| **P5** | Deopt-as-program-point (§13.6) now that placement is a ② product; extend ③'s `optimize_peephole` (§21) with post-allocation passes. | med |

### 22.5 P0/P1 landed

`PhysMap` is the lone `FPReg → FPRegLoc` resolver; all 22 arch emission sites call
`PhysMap::new(base).resolve(reg)` instead of the deleted `FPReg::loc`. Pure
seam-creation, behaviour-identical: both x86-64 and aarch64 build clean; lib suite
**1706 passed, 0 failed** (1705 baseline + `physmap_resolve_formula`). ② will later
swap the formula for an explicit per-`FPReg` table behind this same `resolve`,
with no emission-site change.

## 23. Pre-P2 verification: the premise holds, with one coupling to preserve

Before touching the fixpoint (P2), §22.2's open check — *"is `Sf` always
boxed-canonical, and does placement ever change representation?"* — was resolved
against the code. Result: **the correctness premise holds unconditionally**, and
there is **exactly one** representation↔placement coupling, which is perf-load-
bearing (not correctness-bearing).

### 23.1 Correctness premise — holds at the type level

`LinkMode` (slot.rs) encodes the split directly:

- `F(fpr)` — "*mutation of the fpr lazily affects the stack slot*": **unboxed-
  canonical**, stack is stale. Canonical value lives in the fpr.
- `Sf(fpr, _)` — "*on the stack slot **and** the fpr which is **read-only***":
  **boxed-canonical** on the stack, fpr is a droppable read-only cache.
- `S(_)` — boxed on the stack only.

The placement phases (`alloc_policy::try_alloc_fpr`, slot.rs:73–127) respect this:

| Phase | Action | Touches representation? |
|---|---|---|
| 0 vacant | return lowest-index free fpr | no |
| 1 demote | victim filter is `slots.all(Sf)` (line 93–99); demote `Sf→S` losslessly, **no asm** (stack is canonical) | only drops a *cache* (`Sf→S`), never boxes/unboxes |
| 2 spill | `push_spill` appends a **new** `FPReg(N≥POOL)` for the value *being* allocated | no — never evicts an existing `F` |

So **no placement action ever boxes an `F`, unboxes anything, or evicts an `F`.**
An `F` (stale-stack) is structurally excluded from Phase-1 victims, and Phase-2
only ever hands a fresh spill slot to a *new* allocation. The `_ => unreachable!()`
at slot.rs:107 is the live guard for "Phase-1 demotion only touches `Sf`"; the full
x86-64 (1706) and aarch64 (22 float/numeric) suites exercise it without firing —
the empirical confirmation. ② can therefore be split out with **zero correctness
risk**.

### 23.2 The one coupling: the promotion gate (perf-load-bearing)

`keep_backedge_floats` (merge.rs:123/130 → slot.rs:648) is the loop-back-edge
representation decision: for each loop-carried slot that is `S|Sf` and float-typed
(`adopt && promotable`), promote it to **unboxed `F`** — *but only via*
`try_set_new_F` → `try_alloc_fpr()?` (slot.rs:703), i.e. **Phase 0/1 only, no
spill**. If only a Phase-2 spill could free an fpr, it returns `None` and the slot
**stays boxed** (`S`/`Sf`).

That is the crux: **① decides representation (box vs unbox) by querying ②'s
physical-pool occupancy** ("can the greedy allocator seat this in `xmm2..15`
without spilling?"). This is precisely the §13.8/§16.6 load-bearing *selection* —
benign for correctness (the fallback "stay boxed" is always sound) but it
determines *which* loop floats are unboxed, which is the entire perf delta.

### 23.3 Consequence for P2 (refines §22.4)

P2 is therefore **not** merely "drop the pool/spill encoding from `try_alloc_fpr`".
The promotion gate must keep producing the *same* decisions, so:

1. ① cannot ask a raw physical question ("is `FPReg(i)` vacant?") once VRegs are
   unbounded. The gate must be re-expressed as a **virtual-pressure predicate**
   that P3's greedy ② reproduces exactly: *"would ② seat this VReg in the
   `PHYS_FPR_POOL`-wide pool rather than spill it, at this program point?"*
2. The low-risk route to byte-identity: **share one allocator oracle.** Keep
   `keep_backedge_floats` calling `try_alloc_fpr` (now ②'s policy object), let ①
   record the tentative virtual assignment it returns, and have ② reuse that same
   assignment. Since P3's ② *is* today's greedy `try_alloc_fpr`, ①'s gate and ②'s
   placement read identical occupancy ⇒ byte-identical output by construction. The
   `PhysMap` seam (§22.5) is what lets ②'s *final* physical slot later diverge from
   the gate's tentative one without touching ① or ③.
3. **Validation:** P2/P3 land behind a feature flag; perf-neutrality of the
   promotion gate must be confirmed on **real Apple-silicon / x86 hardware** (qemu
   gives no perf signal), A/B against the stage-1 placement shadow, before default-
   on. Correctness is already covered by §23.1 + the shadow's byte-compare.

**Net:** the separation is *more tractable than §16.6 feared* — ① and ② already
meet at a single oracle (`try_alloc_fpr`), so the job is to make that oracle a
shared ② object rather than to re-derive a second allocator. The risk is confined
to the promotion gate's perf, which the feature flag + M1 A/B gate-keeps.

## 24. Stage-1 placement shadow (the P3 byte-identity oracle)

Per §23.3, P2/P3 stay perf-neutral only if the separated ② emits the *same*
physical placements as today's greedy allocator. §24 lands the **verification
harness** for that, ahead of P2 (so the oracle exists before the risky change).

### 24.1 What it captures

The shadow records, **in emission order, every `FPRegLoc` that `PhysMap::resolve`
returns** during a compilation — one entry per resolve at the §22.5 chokepoint.
Because every arch emission site (all 22) funnels through `resolve`, this Vec is a
faithful, order-preserving fingerprint of ③'s entire FP-placement output, on both
x86-64 and aarch64, with **zero per-site plumbing**. API (codegen.rs,
`placement_shadow`, feature `shadow-placement`, default-off):

- `begin()` — start recording.
- `record(loc)` — append (called inside `resolve` under `#[cfg]`; no-op when idle).
- `take() -> Option<Vec<FPRegLoc>>` — stop and return the fingerprint.

`FPRegLoc` now derives `PartialEq, Eq, Hash`, so two fingerprints compare with
plain `Vec` equality.

### 24.2 How P3 uses it

```text
baseline = { begin(); jit_compile(f, greedy ②);      take() }   // shipping
candidate = { begin(); jit_compile(f, separated ②);   take() }   // P2/P3 build
assert_eq!(baseline, candidate);   // byte-identical placement ⇒ perf-neutral
```

Today there is only one allocator, so the harness is validated on the **identity /
determinism** case: `placement_shadow_fingerprint` (codegen.rs tests) asserts the
fingerprint matches the resolve order exactly and is byte-identical across two
passes, and that recording is correctly scoped (off after `take`). The full lib
suite under `--features shadow-placement` builds and passes (1706 + the gated
test); the default build is byte-for-byte untouched (the `record` call is
`#[cfg]`-gated, `resolve` is otherwise unchanged).

### 24.3 Why this shape

The fingerprint is the *emission-order sequence of physical locations*, not a
per-`FPReg` table, because that is precisely what must be invariant for ③ to
produce identical machine code: P2/P3 may **renumber** virtual `FPReg`s freely (①
assigns unbounded VRegs; ② maps them to physical), and renumbering is invisible to
③ **iff** the resolved `FPRegLoc` stream is unchanged. Comparing the resolved
stream — rather than the VReg ids — is therefore the right and minimal oracle: it
permits VReg renumbering while pinning the observable placement.

### 24.4 Wired into `jit_compile`; validated on real emission

The shadow is now bracketed around the whole ③ emission in `jit_compile`
(jitgen.rs): `begin()` before `gen_machine_code`, `take()` after — and since the
driver recurses into inlined callees, one bracket captures the entire compilation
unit. Under `--features shadow-placement` each compile logs
`[shadow] iseq=<id> type=entry|loop n=<len> digest=<fnv64>` (a compact FNV-1a-64
of the fingerprint; order-sensitive, so any placement *or ordering* change shows
up). M1 `bin/test` already runs the P0/P1 + shadow code green on real Apple
silicon.

Validated end-to-end on x86-64:

- A hot float loop (`while i<N: s += i.to_f*1.5 + 0.25`) JITs to
  `iseq=… type=loop n=12 digest=0x01531d81c82b05c4`, **byte-identical across
  repeated runs** — the deterministic baseline a P3 candidate must reproduce.
- Most non-float compilations log `n=0` (no FP placement emitted) — expected; the
  fingerprint only grows where ③ actually lowers FP operands.
- Default build (feature off) is byte-for-byte unchanged; the lib suite under the
  feature passes (1706 + the gated `placement_shadow_fingerprint`, now also
  asserting digest stability + order-sensitivity).

**Baseline-capture recipe (run on M1 before P2 flips ② on):**

```sh
cargo run --features shadow-placement -- benchmark/so_nbody.rb 2>&1 \
  | grep '\[shadow\]' | sort > baseline.txt
# … after P2/P3 land behind their flag, same command → candidate.txt
diff baseline.txt candidate.txt   # must be empty ⇒ ② is byte-identical ⇒ perf-neutral
```

## 25. P2 step 1: extract ②'s placement policy out of ③ (byte-identical)

`FPReg(usize)` is *deeply* physical in ①: `FprAllocator.vfpr` is indexed by
`fpr.0`, and `add`/`remove`/`clear`/`swap`/`pin` plus every `LinkMode::F(fpr)` /
`Sf(fpr)` store the physical id. A full virtual-id renumbering of ① therefore
touches a large, swap/pin-coupled surface and is deferred. P2 starts at the
**other end** — the cheap, byte-identical half — by moving the *placement policy*
to where ② will own it:

### 25.1 What moved

Before, `PhysMap::resolve` (③) hardcoded the rule `id < PHYS_FPR_POOL ?
xmm(id+2) : spill(base-24+8·(id-POOL))`. Now:

- **②** (`codegen::phys_alloc`) owns the rule as `policy(i) -> PhysSlot`, where
  `PhysSlot` is a **frame-independent** placement (`Xmm(p)` or `Spill(n)`, the
  `n`-th f64 slot). With feature `phys-table`, `policy` is memoised into an
  explicit per-compilation table (`phys_alloc::slot`, grown lazily); without it,
  `policy` is called directly. Both yield identical `PhysSlot`s.
- **③** (`PhysMap::resolve`) is now **policy-free**: ask ② for the virtual fpr's
  `PhysSlot`, then `apply_base` turns a `Spill(n)` into the frame's concrete
  `[rbp/x29 - off]`. The frame base is the *only* thing ③ still contributes.

This is the clean split the §22 framing wants: *which physical resource* is a ②
decision (frame-independent), *where on this frame's stack* is a ③ mechanic
(`base`-relative). P4 swaps `phys_alloc`'s table for a loop-aware allocation with
**zero** change to ③ or the 22 emission sites.

### 25.2 Why byte-identical, and the evidence

`policy(i)` reproduces the old formula termwise (`Xmm(i+2)` for the pool;
`Spill(i-POOL)` + `apply_base`'s `base-24+8n` for the rest), so the resolved
`FPRegLoc` stream is unchanged. Verified with the §24 shadow:

- hot float loop: `digest=0x01531d81c82b05c4` **identical** with and without
  `phys-table`.
- `so_nbody`: all 79 per-compilation fingerprints **byte-identical** across the
  flag (`diff` empty).
- x86-64 lib suite **1706 passed / 0 failed** with `--features phys-table`;
  default build and aarch64 build both clean.

### 25.3 What is *not* yet done (P2 step 2)

① still hands out physical ids (`fpr.0` = pool/spill index); the `phys-table` is
therefore still the identity policy. The remaining, riskier half — making ①
assign *unbounded virtual* ids and having ② pack them into `PhysSlot`s (the
genuine decoupling, §22.3) — requires threading a `virt→phys` indirection through
`FprAllocator`'s swap/pin/`vfpr` surface and every `LinkMode::F/Sf` store. With ③
already policy-free and the table seam in place, that change is now confined to ①
+ `phys_alloc`, and its output stays checkable by the same shadow `diff`.

## 26. P2 step 2 finding: true virtual-id decoupling is *not* byte-identical

Designing step 2 (① assigns unbounded virtual ids; ② packs them into `PhysSlot`s)
surfaced a hard fact that reshapes the roadmap.

### 26.1 Greedy placement is time-varying

`FPReg.0` is not just "physical" — a *live* value's physical register **changes
over its lifetime**. At every control-flow merge / loop back-edge, the bridge
reconciles the current state against the target block's expected assignment
(slot.rs ~1806):

```rust
(LinkMode::F(l), LinkMode::F(r)) => if l != r {
    if self.is_fpr_vacant(r) { self.set_F(slot, r); ir.fpr_move(l, r); }
    else { self.gen_fpr_swap(ir, l, r); }   // move a LIVE value l -> r
}
```

So a loop-carried float can occupy `xmm5` in the body and `xmm7` at the header,
reconciled by an `FprMove`/`FprSwap` **mid-life**. The physical placement is a
*function of program point*, not a single value per virtual register.

### 26.2 Why that blocks a byte-identical step 2

A genuine ② (the point of decoupling) assigns each virtual register **one**
physical location for its whole live range — SSA/linear-scan style. That:

- **eliminates** the back-edge `FprMove`/`FprSwap` reconciliations greedy emits
  (a global assignment needs no per-edge shuffle for a value it pins), and
- **changes** which physical register each value sits in.

Both change ③'s emitted `FPRegLoc` stream. Therefore the §24 shadow `diff`
**cannot** be empty for a real step 2 — byte-identity and decoupling are mutually
exclusive here. (Reuse of a physical slot across *non-overlapping* lifetimes is
fine for stable mapping; only the **mid-life moves** of §26.1 are the obstacle,
and they are intrinsic to greedy's per-edge reconciliation.)

### 26.3 Consequence: step 1 is the byte-identical terminus

The separation splits cleanly into two regimes:

| | byte-identical? | risk | gate |
|---|---|---|---|
| **Step 1** (③ policy-free, ② owns the placement table) — *done* | yes | low | shadow `diff` empty ✓ |
| **Step 2** (① virtual ids, ② global allocation) | **no** — different placement + fewer swaps | high (§16.6 regressed before) | M1 perf A/B; shadow = *delta measurement*, not equality |

Step 2 is thus a **perf experiment**, not a safe refactor: it must beat greedy on
real hardware to be worth the §16.6-class risk, and the shadow's role flips from
"prove zero change" to "quantify the placement delta". The earlier "feature flag +
shadow diff = byte-identical step 2" framing (§22.4/§23.3) was wrong on this
point: it assumed a stable mapping could reproduce greedy, but greedy has no
stable mapping to reproduce.

## 27. The real ② global allocator: constraints and staging

Per the §26 decision to build a genuine global ② (not the byte-identical
relabel), investigation fixed two hard constraints that shape *how*:

### 27.1 Two constraints

1. **Not as an AsmIR post-pass.** Re-deriving liveness/CFG from the flattened
   AsmIR and allocating there is exactly the allocation-free re-derivation §13.8 /
   §16.6 measured at a **2.5× regression**. The allocator must live **inside the ①
   fixpoint**, where types, liveness and loop structure are already computed.
2. **The lever is Phase 0/2, not victim_rank.** §15.9 (and the `AllocCtx` header)
   prove the Phase-1 victim choice is performance-neutral: it only drops an `Sf`
   *read-only cache*, never boxes an `F`. The decisions that move the needle are
   **Phase 0** (which physical register a fresh value gets, hence pool-vs-spill at
   the boundary) and **Phase 2** (spill vs. keep). A real ② must steer those.

### 27.2 Stage 1 (landed): widen the seam to Phase 0

`AllocCtx` gains `pick_vacant(state) -> Option<FPReg>` — the Phase-0 placement
hook — alongside the existing `victim_rank`. The default reproduces the historical
lowest-physical-index first-fit, so it is **byte-identical** (hot-float-loop
digest `0x01531d81c82b05c4` unchanged; x86 lib suite 1706/0; aarch64 builds). The
seam now covers the real lever: a global policy overrides `pick_vacant` to seat
loop-carried values where they will not be evicted, with **zero** default-path
cost (the default *is* the original Phase-0 scan).

### 27.3 Stage 2+ (next): the loop-aware policy

The perf-bearing work, all inside the fixpoint and all **non-byte-identical** (so
shadow `diff` becomes a delta *measurement*, M1 A/B is the gate):

| Stage | Work | Needs |
|---|---|---|
| 2a | Collect per-value **live intervals + loop-carried set** from the fixpoint's existing liveness / `keep_backedge_floats` predicates (the info §16.6's post-pass *lacked*). | fixpoint hook |
| 2b | A loop-aware `AllocCtx` (`pick_vacant`/`victim_rank` driven by 2a) that pins loop-carried `F`/`Sf` in the pool and pushes short-lived temporaries to spill first — fewer in-loop reloads. | 2a |
| 2c | Measure: shadow delta (placements changed, back-edge `FprMove`/`FprSwap` removed) + **M1 A/B** on mandelbrot/nbody/optcarrot. Default-on only if it wins. | M1 |

Stage 1 keeps the shipping build byte-identical while making Stage 2 a contained,
fixpoint-local change behind `AllocCtx`, gated by the §24 shadow (now a delta
meter) and real-hardware benchmarks.

## 28. Stage 2b design: capacity-reservation, the real loop-aware lever

Scoping the loop-aware policy pinned down *what actually moves the needle*, which
is subtler than "pick a better vacant register".

### 28.1 Why `pick_vacant`'s choice is (mostly) neutral, and what isn't

Choosing *which* vacant pool register a value lands in is a renaming — perf-
neutral (§15.9). And an already-resident `F` is never evicted (Phase 2 spills the
*new* value; Phase 1 only drops `Sf` caches). So the **only** way a loop-carried
float ends up non-resident is at *promotion* time: `keep_backedge_floats` →
`try_set_new_F` → `try_alloc_fpr` returns `None` because the pool is full (no
vacant, no all-`Sf` victim), so the slot stays boxed `S`/`Sf` and reloads every
loop iteration. The lever is therefore **pool capacity at promotion**, not victim
choice.

### 28.2 The mechanism: reserve pool capacity for loop-carried floats

The loop-carried-float set is already available at the back-edge merge
(`liveness.loop_used_as_float()` ∧ `be.is_float_typed`, merge.rs ~118-130). A
loop-aware `AllocCtx` uses the **previous fixpoint iteration's** set `L` (the loop
re-runs to convergence, so the prior pass's `L` is known when allocating the
current one) to:

- **Reserve** the top `min(|L|, PHYS_FPR_POOL)` pool slots: `pick_vacant` for a
  *non*-loop-carried allocation skips reserved slots (falls through to Phase 1/2,
  i.e. spills a short-lived temporary instead of consuming a slot a loop-carried
  value will need).
- Loop-carried allocations may use any slot, so their `try_set_new_F` promotion
  succeeds where it previously overflowed → resident across the loop → no per-
  iteration reload.

This needs the allocation entry points (`set_new_F`/`try_set_new_F`/… → `alloc_fpr`)
to pass *which slot* / *is-loop-carried* into the policy — a signature widening of
`alloc_fpr`/`try_alloc_fpr` and their ~10 callers (FPReg stays an opaque handle;
only the policy reads the context).

### 28.3 Correctness, convergence, verification

- **Correctness:** reservation only changes *placement* (resident vs spilled-
  unboxed / boxed), never representation soundness — a spilled or boxed float is
  always a valid materialisation. The full suite must pass with the flag **on**
  (different digests, still correct).
- **Convergence:** the reserve count comes from the prior iteration's `L`; since
  `L` is monotone over the back-edge fixpoint (§14.1) the reserve count stabilises
  with it. Cap reservation at `PHYS_FPR_POOL - 1` so a degenerate `|L|` can never
  starve the allocator into livelock.
- **Risk:** over-reserving forces more temporary spills; net effect is empirical.
  Gate: §24 shadow as a **delta meter** (placements changed, back-edge
  `FprMove`/`FprSwap` removed) + **M1 A/B** on mandelbrot/nbody/optcarrot. Default-
  on only on a win; this is the §16.6-class risk the experiment exists to test.

Stage 1 (§27) already exposes the `pick_vacant` hook this rides on; Stage 2b is
the contained `AllocCtx` body + the allocation-entry signature widening, behind a
new default-off feature.

## 29. Stage 2b result: the reservation policy is inert (forward-fixpoint limit)

Stage 2b (§28) was implemented behind `phys-loop-aware`: `target: SlotId` threaded
through the six FP allocation entry points → `alloc_fpr`/`try_alloc_fpr` →
`AllocCtx::should_reserve`, a `loop_float` set captured in `liveness_analysis`, and
the capacity-reservation gate at the top of `try_alloc_fpr_ctx`. It is **correct**
(full lib suite **1706/0** under `stress-spill-pool,phys-loop-aware`; default build
byte-identical, hot-float digest unchanged) — but **empirically inert**.

### 29.1 The measurement

`should_reserve` **never fires**. Instrumentation showed `state.loop_float` is
**always empty** at every allocation, on every probe — `so_nbody` (pool 14), a
16-accumulator loop, and a single-accumulator loop under `stress-spill-pool`
(pool 2). Placement digests are identical with and without the feature in all
cases (`diff` empty).

### 29.2 Why — the same forward-fixpoint wall

The loop-carried set `L` is produced by `liveness.loop_used_as_float()` and
consumed at the **back-edge merge** (`liveness_analysis`, merge.rs:89). But the
body's float registers are allocated **once**, during the forward body walk, which
runs *before* the back-edge is reached. So at every allocation decision `L` is not
yet known (`loop_float` empty); by the time `L` exists, the placements are
committed. This is exactly the §13.8/§16.6 limitation — the information a better
allocation needs is downstream of the point that needs it — now confirmed at the
allocation seam itself. (And `loop_used_as_float` at that merge was empty for the
probes anyway, a second symptom of the same ordering.)

### 29.3 Consequence

A *binding* loop-aware allocator cannot be a tweak inside the forward fixpoint; it
needs `L` (per-loop float liveness) **surfaced to the body walk** — i.e. a
preliminary liveness pass feeding the allocation pass. That is a two-pass
re-architecture, and §16.6 measured a 2.5× regression for the nearest prior
attempt at allocating with re-derived (rather than fixpoint-native) liveness. So
Stage 2b's lever, as a forward-fixpoint reservation, **does not bind**, and the
two-pass alternative is high-risk.

**Standing decision:** Stage 1 (§27 — ③ policy-free, ② owns the placement table,
Phase-0 seam) remains the byte-identical, shipping terminus. The Stage 2b seam
(`should_reserve` + `target` threading + `loop_float`) is **kept, feature-gated and
inert**, as the documented attachment point: a future two-pass `L`-surfacing pass
plugs a binding policy in here, gated by the §24 shadow (delta meter) + M1 A/B.

## 30. Correction: the bridge *can* spill — the bottleneck is the join's target policy

§29 (and the prior explanation) leaned on "the merge has no AsmIr, so a
pool-overflow loop float must stay boxed". **That framing is wrong**, and the
correction reshapes the real lever.

### 30.1 The bridge materialises S→F, spill included

`AbstractFrame::bridge(ir: &mut AsmIr, target, slot, pc)` — the per-incoming-edge
reconciliation stub — **has AsmIr** and already handles `S → F` (slot.rs:1957):

```rust
(LinkMode::S(_), LinkMode::F(x)) => {            // boxed home -> unboxed fpr
    ir.stack2reg(slot, GP::Rax);
    let deopt = ir.new_deopt_with_pc(&self, pc + 1);
    if self.is_fpr_vacant(x) {
        ir.float_to_fpr(GP::Rax, x, deopt);      // box→f64 decode, on the edge
        self.set_F(slot, x);
    } else {
        let tmp = self.set_new_F(slot);          // ← spill-capable, WITH AsmIr
        ir.float_to_fpr(GP::Rax, tmp, deopt);
        self.gen_fpr_swap(ir, x, tmp);
    }
}
```

So the edge stub can decode a boxed float into an fpr — and into a **spilled** `F`
(`set_new_F` → `push_spill`, line 1968) when no physical reg is free. Every
incoming edge to a merge/back-edge gets such a stub. Materialisation of an
f64-spill loop float is therefore fully supported infrastructure.

### 30.2 The real division of labour

- **join / `apply_join` / `keep_backedge_floats` (no AsmIr)** *decide the target
  representation*. They use `try_set_new_F` (no Phase-2 spill): set the target to
  `F` **only when a physical fpr is free**, otherwise leave it `S`/`Sf`.
- **bridge (AsmIr)** *materialises* each edge into that target, spilling if needed.

So a pool-overflow loop float stays boxed **because the join declined to make the
target `F`**, not because materialisation is impossible. §28's "reserve a physical
slot" was the wrong lever; the actual knob is the *target-representation policy* at
the join.

### 30.3 The correct Stage 2b retry (replaces §28)

For a **known** loop float — `be.is_float_typed(i) ∧ loop_used_as_float(i)` (the
non-speculative signal already wired in merge.rs:128, feature
`layer2-float-by-type`) — commit the target to `F` via the **spill-capable** path
rather than `try_set_new_F`:

```rust
// keep_backedge_floats, for confirmed loop floats only:
self.set_new_F(i);            // instead of try_set_new_F(i)
```

Then the bridge's `S → F` arm materialises an **f64-spill resident** binding: one
box→f64 decode per *edge* (loop pre-header + back-edge), and raw `movsd` f64 in the
body — eliminating the §29 per-iteration decode at the boxed home. No physical
reservation, no `L`-at-allocation-time problem (§29): the decision is at the
back-edge, exactly where `L` is known.

### 30.4 Risks to clear first

- **The noted past bug.** `keep_backedge_floats` deliberately uses the no-spill
  variant because spill-promotion was *"exercised wrongly under register pressure
  (the `stress-spill-pool` path)"* (slot.rs:758-763). Diagnose that failure mode
  before re-enabling — start by switching only the `layer2-float-by-type`
  confirmed-loop-float arm and running the suite under `stress-spill-pool`.
- **Cost trade.** Gains one f64-spill home (per-iteration `movsd`) but pays a
  decode on each incoming edge and grows the spill region by `|overflow loop
  floats|`. Net is empirical: gate on the §24 shadow (now a delta meter — expect
  `S→F` edges to appear) + **M1 A/B** on float-heavy loops that overflow the
  14-wide pool. Only such loops benefit; with ≤14 live floats nothing changes.

This is the architecturally-supported lever the §28 reservation should have been:
edge-materialised f64-spill residency for confirmed loop floats, driven by the
back-edge `L` and the existing `S→F` bridge arm.

## 31. Stage 2b/§30 result: inconclusive — confounded by a *pre-existing* layer2 bug

§30 (edge f64-spill residency: `keep_backedge_floats` using the spill-capable
`set_new_F` for confirmed loop floats) was implemented under `phys-loop-aware`. The
meaningful test needs the **confirmed** loop-float signal, i.e. running together
with `layer2-float-by-type`. Under
`layer2-float-by-type,phys-loop-aware,stress-spill-pool`,
`test_join_float_register_disagreement` failed with a garbage f64
(`-1.0 != 6.9…e-310`, an uninitialised spill slot). I first attributed this to §30.

**That attribution was wrong.** The same test fails with
`layer2-float-by-type,stress-spill-pool` **without** `phys-loop-aware`, and — the
decisive check — it fails identically at commit `bfc4ef1` (P0/P1, *before any of
this Stage-2 work existed*). So:

- **There is a pre-existing latent bug in `layer2-float-by-type` under register
  pressure** (`stress-spill-pool`). The two flags are never combined in CI
  (`stress-spill-pool` runs without `layer2-float-by-type`), so it went unnoticed.
  `layer2-float-by-type`'s broader type+liveness adoption promotes more slots to
  loop-carried `F`, and something in that path reads an uninitialised spill under
  pool=2. This is independent of §28/§30 and is the real bug to file.
- **§30's own soundness is therefore inconclusive.** Its only meaningful exercise
  (the confirmed signal) is confounded by the layer2 bug, and without
  `layer2-float-by-type` the `be.mode == F` adopt signal barely fires under
  pressure, so §30 could not be cleanly evaluated either way. The §31-draft claim
  that §30 was "architecturally unsound" was not actually demonstrated; the
  ordering concern it raised (`keep_backedge_floats` overrides the loop entry
  *after* `analyse_backedge_fixpoint` has frozen the body placements, merge.rs:79
  vs 123) remains a real *risk hypothesis*, not a proven cause.

### 31.1 Actions

- §28 (reservation) and §30 (`set_new_F` at the back-edge) code are **reverted**;
  the tree is restored to **§27 Stage 1** (③ policy-free, ② owns the placement
  table, Phase-0 `pick_vacant` seam — byte-identical, shipping-safe). The
  `phys-loop-aware` feature is removed.
- The pre-existing `layer2-float-by-type` × `stress-spill-pool` failure is recorded
  here as a separate bug to fix before that feature's M1 bench gate.

### 31.2 Standing conclusion (§27–§31)

§27 Stage 1 is the durable result of the whole "real ② allocator" effort. Three
distinct attempts to make ② *beat* greedy failed, and a fourth was confounded:
§28 (reservation) inert (`L` unknown at allocation, §29); §30 (entry spill-promote)
inconclusive + risky (post-fixpoint override); §16.6 (post-pass) regressed. The
robust invariant stands: **a better FP placement must be decided *inside* the
back-edge fixpoint with its native type/liveness/CFG state.** Before any such work,
the `layer2-float-by-type` pressure bug (§31) must be fixed, since that feature is
the intended carrier of the confirmed loop-float signal.

## 32. Diagnosis of the pre-existing `layer2-float-by-type` × `stress-spill-pool` bug

§31 flagged that `test_join_float_register_disagreement` fails under
`layer2-float-by-type,stress-spill-pool` independently of §28/§30. This section
roots it out.

### 32.1 Reproduction and symptom

Reproduced standalone by replicating `run_test`'s wrapper (`__res = (CODE); for
__i in 0..24 { (CODE) }; (CODE)` — the outer `for` loop-JITs and the snippet
redefines+calls `test` each iteration) with the test-mode thresholds. Only the
**first** loop iteration's float is wrong: `res[0]` is a denormalised garbage f64
(`6.9…e-310`) instead of `-1.0`; iterations 1–4 are correct. Default (pool 14) and
default-adopt (no `layer2-float-by-type`) are both correct.

### 32.2 Root cause: entry promotion inconsistent with the body fixpoint

Instrumenting `keep_backedge_floats` shows the **only** difference: under
`layer2-float-by-type` it promotes slot `%3` = `endv` (the loop-carried `1.0`)
from `Sf(FPReg0)` to `F(FPReg1)`; the default adopt promotes nothing.

`endv` is float-*typed* and used-as-float in the loop, so layer2's
type+liveness `adopt` fires — **even though the back-edge fixpoint
(`analyse_backedge_fixpoint`, merge.rs:79, run *before* `keep_backedge_floats` at
:123) placed `endv` as `Sf`, not `F`.** The entry state is thus overridden to a
representation (`F`) the already-frozen loop body was never analysed for. Under
`stress-spill-pool` (pool 2) the freshly allocated `FPReg1` is exactly a register
the body reuses for a temporary, so the loop-entry binding and the body disagree
and the value is read before it is materialised → uninitialised garbage.

This is the **§31.3 invariant**, now confirmed from the opposite direction: a
loop-entry placement/representation must be a subset of what the body fixpoint
produced. The default adopt (`be.mode == F`) is sound precisely because it only
re-adopts placements the body *already* made `F`. `layer2-float-by-type`'s whole
premise — decouple adoption from the analysis-pass placement, drive it from
type+liveness — violates that invariant whenever the body kept the value boxed.

### 32.3 Why the obvious fixes do not work

- *Reuse the incumbent fpr* (`Sf(x) → F(x)` instead of allocating a fresh one):
  tried, still corrupts. The fault is the **`F` vs `Sf` representation** mismatch
  between entry and body, not which register.
- *Restrict to free fprs* (`try_set_new_F`, already the case): does not help — the
  body still treats `endv` as `Sf`.

A correct fix must keep layer2's adoption **consistent with the body fixpoint's
representation** — i.e. either re-run the body fixpoint after adoption, or only
adopt slots the body's back-edge state already carries as `F`. The latter is
essentially the default adopt, so `layer2-float-by-type` as specified cannot be
made sound without folding the adoption decision *into* `analyse_backedge_fixpoint`
(the §31.4 conclusion: placement lives inside the fixpoint).

### 32.4 Status

`layer2-float-by-type` is default-off and explicitly "flip after the M1 bench gate
clears", so this latent bug ships to nobody. It is left as-is with this diagnosis;
fixing it is the same fixpoint-internal-placement work the §27–§31 arc converged
on, and is the prerequisite for that feature (the intended carrier of the confirmed
loop-float signal) ever being enabled.
