# Handoff — JIT codegen layering / record-driven lowering (Path 2)

**Branch:** `claude/wizardly-bell-n3k3po` (develop + push here).
**Latest commit:** `818c15c` (array-index `ir.inline` closures → typed `AsmInst` variants).
**Design record:** `doc/regalloc_separation.md` §12–§20 — read it; this handoff is the summary.

---

## 1. The goal

Cleanly stratify the JIT codegen into layers so optimization logic is easy to add.
This grew out of a longer effort (§5) to separate *abstract interpretation +
fixpoint search* from *physical register allocation*.

---

## 2. What is committed and its standing value

All changes are **behaviour-preserving** (lib suite `1671 passed`, baseline-identical;
the `transfer()` debug shadow checks pass with **zero replay mismatches**).

**Independent value (keep regardless of what happens next):**
- **§20 — array-index codegen de-closured.** The 8 `ir.inline(|gen| …)` closures for
  array integer index read/assign (x86 + aarch64) became typed data records
  (`AsmInst::ArrayIndex` / `ArrayIndexAssign` + `ArrayIndexKind`), with per-arch
  `gen_array_index` / `gen_array_index_assign` holding the moved closure bodies.
- **§18 — `binop_float` split** into a pure, allocation-free *decision*
  (`plan_binop_float` → `FloatBinOpPlan`) and an *execution*.
- **§15.9 / §16.5 / §16.6 — settled findings** (see §4 below). These close
  dead-end lines so they are not re-attempted.

**Latent scaffolding (pays off only if Path 2 continues):**
- **§19 — float/integer arith+cmp routed through the record stream**
  (`TransferIR::{FloatBinOp, FloatCmp, IntegerCmp, IntegerBinOp}`). These are
  *collected* into `AsmIr.transfers` but **nothing consumes that collection yet**,
  so today they only add the records to an unused vec (tiny release overhead) while
  still emitting inline. Their payoff arrives when the optimization seam (§5) lands.

---

## 3. THE KEY INSIGHT (act on this next)

`inst: Vec<AsmInst>` **is already the ordered, replayable stream.** Handlers build it
during the analysis/codegen walk (`traceir_to_asmir`); `gen_machine_code` replays it
afterward to emit machine code (`compile_asmir` per `AsmInst`). The hot ops are
already **typed** `AsmInst` variants (`FloatBinOp`, `IntegerBinOp`, `IntegerCmp`,
`ArrayIndex`, `ArrayIndexAssign`, …), inspectable in `inst`.

Therefore the "clean layer boundary for optimizations" (the whole point of Path 2)
is reached by **adding an optimization-pass hook over `inst` between AsmIR
construction and machine-code emission** — NOT by the move-based record rewrite I
earlier sketched.

Things that are therefore **NOT needed** (do not chase them):
- `AsmInst: Clone` is **not** required. (It is blocked anyway: `ir.inline` is used by
  **~52** inline-builtin closures across ~13 `builtins/*.rs` files — `Integer#+`,
  `Float` ops, `Array`/`String`/`Hash`/`Range`/`Math`, etc. — not the 8 array-index
  sites I first assumed. Converting all 52 to typed data, or `Rc<dyn Fn>`-wrapping
  them, is unnecessary for the opt-seam approach.) The aborted "remove `Inline` +
  derive `Clone`" step was reverted for this reason.
- The move-based "records own `AsmInst`, replay builds `inst`" rewrite and the
  guards-deopt-program-point completion are unnecessary: `gen_machine_code` is
  already the replay pass; `inst` is already the stream.

---

## 4. Findings that redirect future work (don't redo these)

- **Allocator victim-policy is perf-neutral (§15.9).** `try_alloc_xmm` phase 1 only
  demotes an **all-`Sf`** register (stack is canonical → free, lazy reload); it never
  touches an `F`. Phase-2 spill is unboxed (`VirtFPReg`). So **no allocation choice
  ever boxes an `F`**, and every spill-victim probe (§14.7, §15.1) was a no-op. The
  `AllocCtx` seam exists but a better victim rank cannot move the needle.
- **Loop-carried-`F` selection is inherently allocation (§16.5/§16.6).** Re-deriving it
  from `type ∧ liveness` (the `layer2-float-by-type` feature, default-off) **regresses**
  (x86 wall-clock: mandelbrot ~4 %, nbody ~2.4 %). Kept as a negative-result probe.
- **The shipped win** (already in merged PR #741, default-on): loop-entry float
  specialization `keep_backedge_floats` — mandelbrot +12.5 %, optcarrot 184.8→186.2.
  This session's `keep_backedge_floats` mechanism/policy split (§18/L2-0) is the
  current form (takes an `adopt` predicate; default reads `backedge.mode(i) == F`).
- **goal-3 (VM-residual codegen) is deferred per the user (§17.4).** The L2-2.1
  `AllocStrategy { JitGreedy, VmResidual }` work was reverted. Revisit only when VM
  support is wanted again; §17.1–17.3 holds that design.

---

## 5. Concrete next step (Path 2, realized as the opt seam)

1. **Add the AsmIR optimization seam.** In `Codegen::gen_machine_code`
   (`monoruby/src/codegen/jitgen.rs`, just after `let ir_vec = frame.detach_ir();`,
   before the emission loop), run an optimization pass over each `AsmIr`'s `inst`.
   Add `AsmIr::optimize_peephole(&mut self)` in `asmir.rs` (the `inst` field is
   private to the `asmir` module, so the pass lives there; expose `pub(super)`).
2. **First safe pass: self-move elimination** — drop `AsmInst::RegMove(s, d) if s == d`
   (and `FprMove` self-moves once the variant's src/dst order is confirmed). Safe
   because: side-exit indices (`AsmDeopt`) index the **`side_exit` vec, not `inst`
   positions**, so removing an inst does not break deopt; a self-move is a no-op with
   no control-flow effect. Instrument with a count to confirm it fires; if it rarely
   fires, the **seam** is still the deliverable and real passes (peephole / dead-code
   / redundant `XmmSave`-pair removal) follow.
3. Also optimize the inline/outline **bridge** `AsmIr`s (`frame.remove_inline_bridge`
   / the outline bridges) for completeness, or note they are skipped.
4. **Document as §21** in `doc/regalloc_separation.md`: the seam, why `inst` is the
   stream, and that this is Path 2's realization.

This is small, behaviour-preserving, and delivers the actual goal (a place to add
optimization logic over a typed, arch-neutral instruction stream).

---

## 6. Practical notes / gotchas

- **Verify:** `cargo test -p monoruby --lib` → expect **`1671 passed; 34 failed`**. The
  34 failures are **pre-existing environment mismatches** (CRuby version / timezone /
  missing `bigdecimal` gem), present at baseline — NOT regressions. The fastest check
  is the pass count (1671) + zero `replay mismatch` lines.
- **`transfer()` shadow check** (debug builds) replays each `TransferIR` record into a
  scratch and asserts identical `AsmInst` + `SideExit`. Routing a new *data-only* op
  through `transfer()` is automatically shadow-verified. Deopt-carrying records
  (`XmmLoad`, `IntegerBinOp`) carry a `DeoptPoint` program point (§9); `emit`
  materializes the side-exit via `deopt_from_point`.
- **aarch64:** this environment builds/tests **x86-64 only**. The user runs M1
  `bin/test` each turn to confirm aarch64. **Risk to confirm:** `818c15c`'s aarch64
  `gen_array_index*` were moved verbatim from the existing aarch64 closures, but I did
  **not** verify whether `arch/aarch64/compile.rs::compile_asmir_arch` needs an
  explicit match arm for `ArrayIndex`/`ArrayIndexAssign`. It does not list `Inline`
  (which `compile_shared` also handles), so it likely has a catch-all and builds —
  **but get an M1 build to confirm `818c15c` before building on top.**
- **Minor cleanup:** the x86 and aarch64 `array_integer_index` / `_assign` in
  `jitgen/compile/index.rs` are now cfg-gated twins that both just `ir.push` the same
  arch-neutral variant; they could be merged into one.
- **Commit/push:** end commit messages with the `Co-Authored-By:` +
  `Claude-Session:` trailers used on this branch. Push with
  `git push -u origin claude/wizardly-bell-n3k3po`.

---

## 7. Commit map (this branch, newest first)

```
818c15c §20 B  array-index ir.inline → typed AsmInst variants (8 sites, both arches)
ed0e481 §19 B  guarded integer binop → TransferIR::IntegerBinOp (deopt program point)
a62ae1c §19 B  integer cmp → TransferIR::IntegerCmp
daf1cf9 §19 B  float cmp → TransferIR::FloatCmp
8732ad6 §19 B  float binop → TransferIR::FloatBinOp (+ §19 plan)
58171a7 §18.3   correction: XmmOp alloc/emit already split (§9/§11); (b) = record-driven lowering
bd46db8 §18     binop_float split into pure decision + execution
69e318b §17.4   defer goal-3/VmResidual; refocus on JIT-internal separation
3461edb         Revert L2-2.1 (AllocStrategy/VmResidual — goal-3, deferred)
1d823ff §17     L2-2 design (goal-3 swappable allocator) — deferred
7c18d6e §16.6   L2-1 bench verdict: REGRESSION (path ii)
5db296e §16.5   the F-selection is allocation — L2-1's fork
ab6dda9 §16 L2-1 type+liveness float-adoption behind layer2-float-by-type (probe)
d4f7b4e §18 L2-0 keep_backedge_floats mechanism/policy split
e71c7f6 §15.9   close the allocator-policy axis (phase 1 protects every F)
a059f37 §15 3c-i incr2  thread AllocCtx into the xmm policy
```
(Everything from `818c15c` down is on top of the merged PR #741.)
