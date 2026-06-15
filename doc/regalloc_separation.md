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
Typed IR            per-slot AbstractType lattice + liveness; operands are
  │                 (slot, representation) — still virtual, no phys regs
  │  ② allocation + lowering pass  (pluggable Allocator)
  ▼
LIR (LInst)         concrete regs/spills; emitted straight to encode_linst
  ▼  encode_linst → bytes
```

### Layer ① — the type lattice (analysis)

A pure lattice element per slot, *no* location:

```rust
enum AbstractType {
    Top,                 // unknown (⊤)
    Class(ClassId),      // boxed value of known class (+ guard)
    Float,               // value known to be Float (unboxable)
    Fixnum,              // value known to be Fixnum
    Concrete(Value),     // compile-time constant (⊥-ish)
    Bottom,              // unreachable
}
```

`join` over this is a *pure* lattice meet — no register churn. Liveness stays
here. This pass is what goal 3's partial evaluator parameterizes (feed ⊤ for the
VM residual, IC-narrowed types for the JIT residual).

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
| **0a. Decomposition + test** ✅ | Add `Placement` (the location/representation dual of the existing `Guarded` type lattice) + `LinkMode::{placement, from_parts}` projections, with a round-trip test proving `LinkMode ≅ (Placement, Guarded)`. Additive scaffolding — no live state touched. *Done; suite 1703/0.* | none |
| **0b. Storage split** | Replace `SlotState.slots: Vec<LinkMode>` with `ty: Vec<Guarded>` + `place: Vec<Placement>`; `mode()`/`set_*` become `from_parts`/decompose shims. `join` splits into `join_ty` (pure lattice meet) + `reconcile_place`. Behaviour-identical. | med |
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
- **`Sf` (xmm + stack cache).** It is a *representation* optimisation that blurs
  type and placement. In the split it is `(ty = Float|Fixnum, place = both)`; the
  *demote-on-pressure* logic (`try_alloc_xmm` phase 1) is an allocation policy
  that must move wholesale into the `Allocator`.
- **`r15` accumulator.** The single GP "accumulator" slot is its own tiny
  allocation problem fused into `SlotState.r15`; it follows the same split
  (type vs placement) but is simpler than the xmm pool.
- **Spill-region sizing across joins** (`grow_xmm_to`, `gen_bridge`) becomes the
  allocator's responsibility once placement is its own layer.

---

## 6. Progress

**Step 0a is done.** `LinkMode` now has the `placement()` / `from_parts()`
projections, and a unit test (`linkmode_placement_roundtrip`) proves it is
isomorphic to `(Placement, Guarded)`. The type lattice already existed as
`Guarded` (`Value`=⊤, `Fixnum`, `Float`, `Class(c)`) with a `join`; step 0a
adds its location dual. No live state changed; suite at 1703/0.

**Next: step 0b (storage split)** — back `SlotState` with separate `Vec<Guarded>`
+ `Vec<Placement>` and split `join` into a pure type meet plus a placement
reconciler, keeping behaviour identical. Then the `Allocator` seam (step 1) and
the standalone analysis pass (step 2), after which goals 1/3 fall out of
steps 3–4.
