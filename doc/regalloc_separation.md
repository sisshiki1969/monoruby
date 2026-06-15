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
| **1. Allocator seam** | Route `alloc_xmm` / `def_F` / `pin_xmm` / spill sizing through one `Allocator` abstraction (default = today's greedy). Inference calls the seam instead of mutating `vfpr` directly. | low–med |
| **2. Standalone analysis** | Run the `Guarded`/liveness fixpoint as its own pass producing a typed IR, *before* the allocation+lowering pass consumes it. The lowering pass becomes `fn(typed_ir, &mut Allocator) -> Vec<LInst>`. This is the real separation. | high |
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

**Next: step 1 (Allocator seam)** — route `alloc_xmm` / `def_F` / `pin_xmm` /
spill sizing through one `Allocator` abstraction (default = today's greedy), so
the placement reconciliation stops mutating `vfpr` directly. Then step 2 (the
standalone analysis pass consuming `join_ty`), after which goals 1/3 fall out of
steps 3–4.
