# Handoff — JIT codegen layering / record-driven lowering (Path 2)

**Branch:** `claude/wizardly-bell-n3k3po` (develop + push here).
**Latest commit:** `60e613a` (§21: the AsmIR optimization seam over `inst`).
**Design record:** `doc/regalloc_separation.md` §12–§21 — read it; this handoff is the summary.

> **Update (this session):** §3's key insight was acted on — the optimization
> **seam landed** (§21, commit `60e613a`). `AsmIr::optimize_peephole` now runs over
> every main block and bridge in `gen_machine_code` after `detach_ir()`. The first
> pass (self-move elimination) is wired but fires **0 times** (the allocator emits
> no self-moves), exactly the predicted "safe first pass"; the **seam itself** is the
> delivered layer boundary. Behaviour-preserving (lib suite **1705 passed, 0 failed**
> on this env's CRuby-4.0.2 baseline — note: the old "1671/34" figure was pre-CRuby-4
> install; with Ruby 4.0.2 + `bigdecimal` the suite is now fully green). The **new
> next step is §5 below** (a peephole pass that actually fires).

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

- **Allocator victim-policy is perf-neutral (§15.9).** `try_alloc_fpr` phase 1 only
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

## 5. Concrete next step — a peephole pass that actually fires

The seam (§21) is in place; `optimize_peephole` is the place to add passes. The
self-move pass never fires, so the next increment is the **first pass with a real
hit rate**, proving the seam pays off. Candidates (pick one, smallest first):

1. **Redundant store/reload elision** — a `RegToStack(r, s)` immediately followed by
   `StackToReg(s, r)` (same slot, same reg, no intervening write to `s` or `r`) makes
   the reload dead. This is the most likely frequent hit. Start with the strict
   adjacent-pair form, then widen the window. Verify the no-intervening-write
   guard against `AsmInst`'s slot/reg operands (the `fpr_operands` pattern shows how
   to enumerate operands for a variant).
2. **Dead `FprSave`/`FprRestore`-pair removal** — the handoff's other suggested pass;
   needs the same "no use in between" liveness check over `inst`.
3. **Arithmetic identity folds** on the typed `IntegerBinOp`/`FloatBinOp` records.

Method: add the pass body inside `AsmIr::optimize_peephole` (asmir.rs), gate any
counter behind `jit-log`, confirm it fires on a benchmark (`cargo run --features
jit-log -- benchmark/app_fib.rb | grep peephole`), then verify `cargo test -p
monoruby --lib` stays green (**1705 passed, 0 failed**) with zero replay mismatches.
Record the pass under a new §22 (or extend §21.2) in `doc/regalloc_separation.md`.

**De-closuring more ops** (orthogonal, lets passes see through more of the stream):
§20 turned the 8 array-index closures into typed data; the same can be done for
other arch-uniform `ir.inline` sites so optimization passes inspect them too. The
~52 inline-builtin closures (`builtins/*.rs`) are the bulk and the hard part (§19.3).

---

## 6. Practical notes / gotchas

- **Verify:** `cargo test -p monoruby --lib` → with **CRuby 4.0.2 + `bigdecimal`
  installed** (now the case on this env) expect **`1705 passed; 0 failed`**. If you see
  ~34 failures it means the env reverted to an older `ruby` / missing `bigdecimal`
  (CRuby version / timezone / `bigdecimal` gem mismatches) — those are env, NOT
  regressions. The fastest check is the pass count + zero `replay mismatch` lines.
  **Flake note:** the full suite spawns a `ruby` subprocess per comparison test;
  under parallel load a handful of `require`-heavy string tests (`string_to_r`,
  `string_undump_*`, `string_unicode_normalized_p`) can transiently fail at
  `tests.rs:208` (a *monoruby* run-Err, not an output mismatch). Re-running them in
  isolation passes — treat an isolated-pass + clean re-run as green.
- **`transfer()` shadow check** (debug builds) replays each `TransferIR` record into a
  scratch and asserts identical `AsmInst` + `SideExit`. Routing a new *data-only* op
  through `transfer()` is automatically shadow-verified. Deopt-carrying records
  (`FprLoad`, `IntegerBinOp`) carry a `DeoptPoint` program point (§9); `emit`
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
60e613a §21     AsmIR optimization seam over `inst` (+ self-move pass, 0 hits) ← HEAD
818c15c §20 B  array-index ir.inline → typed AsmInst variants (8 sites, both arches)
ed0e481 §19 B  guarded integer binop → TransferIR::IntegerBinOp (deopt program point)
a62ae1c §19 B  integer cmp → TransferIR::IntegerCmp
daf1cf9 §19 B  float cmp → TransferIR::FloatCmp
8732ad6 §19 B  float binop → TransferIR::FloatBinOp (+ §19 plan)
58171a7 §18.3   correction: FprOp alloc/emit already split (§9/§11); (b) = record-driven lowering
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
