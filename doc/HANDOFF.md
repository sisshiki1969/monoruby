# HANDOFF — rubykon polymorphic-JIT work

Branch: `claude/handoff-review-nyy4u`. Base: `master`.
Last update: 2026-05-16. State: **Parts 3-B, B, C implemented,
regression-verified, AND rubykon-measured (§3a): the targeted
`[Symbol][NilClass]` `== nil` deopt storm collapses millions → 10
(+1 recompile), ~7% best wall-time. One follow-up ("§5 item 1") was
tried, measured, and reverted — it disproved its own premise and
revealed the next bottleneck is class-version churn, not BinCmp
polymorphism (§5a/§5b). Remaining: §5b investigation + PR (§5c).**

> Rule: never push to `master`; this work stays on
> `claude/handoff-review-nyy4u` and lands via PR. Verify the branch
> before any commit (a merged PR may auto-switch the checkout to
> `master`).

---

## 0. Commits on this branch (over `master`)

```
0f0f087 jit: inline immediate fast path for polymorphic ==/!= (Part C)
61b9a89 jit: fix Part B recompile target (use JIT entry position, not cmp pc)
3bb3512 jit: one-shot recompile of monomorphic BinCmp on polymorphism (Part B)
415208f jit: non-deopting polymorphic BinCmp via generic C-call (Part 3-B)
256b461 doc: add HANDOFF.md for rubykon polymorphic-JIT work
f11603a jit: record BinCmp/BinOp receiver polymorphism (POLY flag plumbing)
```

`f11603a` is the original POLY-flag plumbing/diagnostics (behavior
unchanged). Everything from `415208f` on is the new behavior.

---

## 1. Problem (unchanged)

ruby-bench `rubykon` runs ~1.5× slower than CRuby 4.0.1 `--yjit`.

| Engine | best (19×19, 100 iters, best of 10) |
|---|---|
| CRuby 4.0.1 `--yjit` | ~367 ms |
| monoruby (pre-this-work) | ~559 ms |
| CRuby 4.0.1 interpreter | ~850 ms |

Root cause (confirmed via `--features profile` deopt-stats):
>10M deoptimizations on a single `==` in
`MoveValidator#spot_unoccupied?` (`board[id] == Board::EMPTY`, i.e.
`== nil`), plus millions more in `no_suicide_move?`,
`gain_liberties_from_capture_of`, etc. The `==` receiver class
oscillates `NilClass`↔`Symbol`. monoruby compiled these sites
monomorphic (empty board ⇒ only `nil == nil` for the first ≥20
calls), then the receiver-class guard plain-deopted forever with no
recompile. See git history of this file / `f11603a` message for the
full original analysis.

---

## 2. What was done (this branch)

The agreed plan (handoff §5, ratified by the user) was **B then C**:
Part 3-B as the non-deopting foundation, Part B to flip
monomorphic-compiled sites onto it, then Part C as the `==`/`!=`
fast-path optimisation.

### Part 3-B — non-deopting polymorphic BinCmp (`415208f`)

`monoruby/src/codegen/jitgen/compile/binary_op.rs`:
`binary_cmp` / `binary_cmp_br`, in the
`BinaryOpType::Other(Some(_), _)` arm, now branch on `polymorphic`:

- `polymorphic == true` → `emit_generic_cmp`: write back operands,
  keep the **class-version** guard (it tracks the global
  class-version counter, not the receiver class, so it never fires
  on class variance — preserves the Part B monotone invariant),
  then a generic `cmp_*_values` C-call with **no receiver-class
  guard**, `handle_error`, then invalidate cached guards. Result →
  acc/dst (or cond-br for the `_br` form).
- `polymorphic == false` → unchanged `call_binary_method`
  (monomorphic, recv-class-guarded), *but* now flagged for Part B
  (see below).

New plumbing: `AsmInst::GenericBinOp` + `AsmIr::generic_binop`
(modelled on `ArrayTEq`), and a `CmpKind → BinaryOpFn` map
(`cmp_generic_fn`, the same `cmp_eq_values`/… helpers the VM's
generic path uses). Monomorphic sites are byte-for-byte unaffected.

### Part B — one-shot recompile on becoming polymorphic (`3bb3512`, fixed by `61b9a89`)

The POLY bit is set by the VM **after** monomorphic compilation, so
Part 3-B alone never triggers for rubykon. Part B makes a
monomorphic-compiled BinCmp site's **receiver-class-guard miss**
recompile (once the miss counter is exhausted) instead of
plain-deopting forever; the recompile re-runs `binary_cmp`, which by
then sees `polymorphic == true` (the VM set POLY during the deopt
fallbacks) and emits the Part 3-B path — which has **no recv-class
guard**, so it can never re-trigger. Monotone, one-shot, bounded.

Implementation:
- `RecompileReason::BecamePolymorphic` (`executor.rs`).
- `SideExit::RecompileDeoptimize(pc, wb, reason, position)`
  (`asmir.rs`) + `AsmIr::new_recompile_deopt`. `binary_cmp`'s
  non-polymorphic `Other` arm now calls `call_binary_method(…,
  recompile_on_recv_miss = true)`; the bool is threaded through
  `call_binary_method` → `compile_method_call`, where the
  receiver-class guard's deopt becomes a `new_recompile_deopt`
  instead of `new_deopt`. Every other caller passes `false`
  (general MethodCall, arithmetic BinOp, index, ternary) — they keep
  plain deopt, so no infinite-recompile risk for paths that have no
  non-deopting alternative.
- Side-exit codegen: `gen_recompile_deopt_with_label` →
  `side_exit_with_label` with an optional
  `(RecompileReason, position)`. The recompile (counter-gated,
  `COUNT_DEOPT_RECOMPILE`, one-shot) is emitted **after** the deopt
  write-back and **before** `jmp fetch`.

Two bugs were found here and fixed before this state:

1. **GC use-after-free.** Initial version recompiled *before* the
   deopt write-back, so a GC inside `jit_recompile_loop` freed Ruby
   values still live only in registers → "Dead object" panic in
   `cmp_eq_values`. Fix: write-back must precede the recompile call
   (now structurally enforced by emitting the recompile inside
   `side_exit_with_label` after `gen_write_back_for_deopt`).
   Validated under `--features gc-stress`.
2. **Wrong recompile target (`61b9a89`).** Passed the cmp-site `pc`
   to `gen_recompile`, driving `jit_recompile_loop` at a
   non-loop-header bytecode → `is_loop_begin(bb).unwrap()` panic
   (e.g. `builtins::enumerator::tests::one_`). Fix: thread the JIT
   **entry** position (`JitContext::position()` — `None` for a full
   method/block JIT, `Some(loop-header pc)` for a loop JIT) into the
   side exit, exactly as the existing `recompile_and_deopt` does.
   `JitType::Specialized` recompiles via an idx, not a position, so
   it is gated **out** of Part B (stays on plain deopt) — safe and
   regression-free.

### Part C — inline immediate fast path for `==`/`!=` (`0f0f087`)

YJIT-`opt_eq`-style. `emit_generic_cmp` now, for `CmpKind::Eq` /
`CmpKind::Ne`, emits `AsmInst::OptEqCmp` instead of `GenericBinOp`
(other cmp kinds keep the plain generic call). Lowering
(`asmir/compile.rs::opt_eq_cmp`): with `rdx = lhs`, `rcx = rhs`, if
**both** operands are non-heap, non-flonum immediates
(`bits & 0b111 != 0` and `bits & 0b011 != 0b010` — i.e. Fixnum /
nil / true / false / Symbol) the Ruby `==`/`!=` result is exactly
bit (identity) equality and is produced inline (`xorq rax,rax;
cmpq rdx,rcx; set_eq/set_ne`). Float (`-0.0`/`0.0`, `NaN`), heap
(`String` content, custom `==`), `BigInt`, and mixed numeric all
fall through to the same generic `cmp_*_values` C-call as Part 3-B
(correct for them). No receiver-class guard. The slow-path tail
reuses the exact `BinaryOpFn` calling convention; the fast path
clobbers only `rax`.

This is a pure optimisation layered on a correct Part 3-B; its
*benefit* needs the rubykon measurement (below), but its
*correctness* is exhaustively tested.

---

## 3. Verification done (in this container)

- **Correctness vs CRuby 4.0.4** (see §4 for the Ruby build): a
  large polymorphic-cmp matrix — `==`/`!=`/`<`/`<=>`/`===`, branch
  form, NilClass↔Symbol↔custom↔Integer oscillation, custom `==` /
  `<=>` with `Comparable`, exception propagation through the C
  helper, `coerce`/`Rational`, `BigInt`, Float `NaN`/`-0.0`/`0.0`,
  String content equality, `Integer == Float` — all match exactly,
  run >300× (well past the JIT thresholds, so the
  monomorphic→polymorphic→recompile→Part 3-B/C path is exercised).
  Repro scripts: `/tmp/poly_cmp.rb`, `/tmp/poly_edge.rb`,
  `/tmp/partc.rb`, `/tmp/mixed.rb` (regenerate as needed; they are
  not committed).
- **gc-stress**: `cargo build --features gc-stress`; the transition
  tests + a heavy mixed workload pass with correct results (this is
  what caught and now guards the Part B GC-ordering bug).
- **Full lib suite**: `2053 passed / 0 failed` against Ruby 4.0.4
  (with `LANG=C.UTF-8`). Integration suites (`arith`, `comparable`,
  `case`, `bang_operator`, `method_call`, `enumerable`) all green.
- No new warnings; `cargo build` clean. (One-off `rust-lld`
  "undefined hidden symbol" link errors seen mid-session were
  stale-incremental-cache corruption from flipping the `gc-stress`
  feature + a `build.rs` rerun — resolved by `cargo clean`, **not** a
  code issue.)

### 3a. rubykon measured (this container, 2026-05-16)

`ruby/ruby-bench` cloned at `/home/user/ruby-bench` (it mirrors
yjit-bench — identical refs — so `benchmarks/rubykon/lib` is the
exact lib the original analysis used). Pure Ruby, loads under
monoruby & CRuby 4.0.4. Self-contained runner `/tmp/rubykon_run.rb`
(19×19, `ITERATIONS=100`, best-of-10; env: `RUBYKON_ITERS/RUNS/LIB`)
— mirrors yjit-bench `benchmark.rb`, no YJIT/CSV harness so it runs
identically under both.

> Absolute ms are NOT comparable to the original handoff numbers
> (different machine). What matters is same-machine relative.

Wall time, best-of-10, 19×19/100, this container:

| build | best | avg |
|---|---|---|
| baseline `256b461` (no 3-B/B/C) | 793.6 ms | 805.0 ms |
| **this branch (3-B/B/C)** | **738.9 ms** | **767.1 ms** |
| CRuby 4.0.4 `--yjit` | 471.6 ms | 503.1 ms |
| CRuby 4.0.4 interp | 1047.0 ms | 1091.0 ms |

⇒ Parts 3-B/B/C: **~7% best / ~5% avg faster** than baseline. Real
but modest (see §5 for why).

Deopt counts (`--features profile`, 2 runs × 100 iters), the
decisive evidence — **baseline → this branch**, per site:

| site (`POLY … == …`) | baseline | branch |
|---|---:|---:|
| `MoveValidator#spot_unoccupied?` `[Symbol][NilClass]` | 1,390,933 | **10** |
| `block in no_suicide_move?` `[Symbol][NilClass]` | 418,415 | **10** |
| `EyeDetector#candidate_eye_color` `[Symbol][NilClass]` | 145,612 | **10** |
| `block in candidate_eye_color` `[NilClass][Symbol]` | 31,721 | ~0 |
| `Group#already_counted_as_liberty?` `[Integer][NilClass]` | 137,805 | 50,550 |

Each fixed site shows exactly **one `BecamePolymorphic` recompile**
then runs deopt-free. The targeted `[Symbol][NilClass]` `== nil`
pattern is reduced **5–6 orders of magnitude (millions → 10)** —
Part B's monotone one-shot transition is confirmed working exactly
as designed. (Baseline at 2×100 iters ≈ the original ">10M" at the
full 10-run measurement — consistent.)

> The 31 lib failures originally seen were entirely the wrong
> reference Ruby (3.3.6) + a non-UTF-8 locale; 29 vanished under
> Ruby 4.0.4 and the last 2 (`string_inspect`,
> `symbol_inspect_unquoted`) under `LANG=C.UTF-8`. None were
> related to this work.

---

## 4. Environment notes (this container)

- **Ruby 4.0.4 built from source.** `ruby/ruby` tag `v4.0.4` →
  `~/.ruby-4.0` (`./autogen.sh && ./configure
  --prefix=$HOME/.ruby-4.0 --disable-install-doc && make -j4 &&
  make install`). Bundled gems were **skipped** (emptied
  `gems/bundled_gems`) because the network policy blocks
  `cache.ruby-lang.org` (the gem mirror — 403 `host_not_allowed`);
  `rubygems.org` *is* reachable. Core interpreter + default gems
  are intact. `gem install bigdecimal` done (for
  `tests/bigdecimal.rs`, since bigdecimal is a bundled gem in 4.0).
- `/usr/local/bin/{ruby,gem,bundle}` were symlinks to
  `/opt/ruby-3.3.6/…`; repointed to `/root/.ruby-4.0/bin/…`. The
  project's `build.rs` and `monoruby/src/tests.rs` both enforce
  `MIN_RUBY_VERSION = (4, 0)` and resolve `ruby` from PATH; with
  this repoint `ruby --version` → `4.0.4`, `~/.monoruby/ruby_version`
  → `4.0.4`, and `monoruby -e 'puts RUBY_VERSION'` → `4.0.4`.
- `~/.bashrc` exports `LANG=C.UTF-8 LC_ALL=C.UTF-8` so the
  reference Ruby's `inspect` output (US-ASCII default external
  encoding otherwise) matches monoruby's UTF-8.
- rubykon **is** now available here: `ruby/ruby-bench` cloned at
  `/home/user/ruby-bench` (sibling of the repo). Runner
  `/tmp/rubykon_run.rb`. Measurement done — see §3a. `perf` is
  unusable; use `--features profile` deopt-stats (printed at process
  exit) for hot-spot analysis.
- monoasm uses 64-bit register names even for 32-bit ops
  (`movl r8, [mem]`, not `r8d`); `testq`/`cmpl` supported,
  `testl`/`r8d` are not. `0b…` immediates work in `monoasm!`.

---

## 5. Remaining work (ordered)

Implementation, correctness, and rubykon measurement are **done**
(§3, §3a). Parts 3-B/B/C fully solve the *targeted* problem (the
`[Symbol][NilClass]` `== nil` deopt storm: millions → 10 + one
recompile). The ~7% (not larger) wall-time gain is explained below.

### 5a. Investigated & rejected — "§5 item 1" (commit reverted)

A first follow-up hypothesis was that
`Group#already_counted_as_liberty?` (`%3 == %2`, ~50K residual
deopts, the single largest remaining cmp deopt) deopted because
`binop_type` returned `BinaryOpType::Integer` so `binary_cmp` took
the numeric fast-path arm (integer type guard) instead of the
`Other`/generic arm. A change routing the polymorphic
`Integer`/`Float` arms to `emit_generic_cmp` was implemented
(`bf91ecc`), correctness-verified, **measured, and reverted**
(`bbdb8df`).

**Why reverted — the hypothesis was wrong.** With the change the
site *still* deopted ~50K (`50,550 → 49,833`); only the recorded
operand-class label moved (`[Integer][NilClass]` →
`[NilClass][NilClass]`). With the numeric/recv-class guards removed
by `emit_generic_cmp`, the *only* deopt-capable instruction left on
that path is `guard_class_version` (Part C's opt_eq fast path has
no deopt; `handle_error` only fires on a C-call error, which
`nil == nil` never hits). So **`already_counted_as_liberty?`'s
~50K deopts are class-version-guard misses, i.e. class-version
*churn*, not receiver/numeric polymorphism at all.** The change
gave no deopt reduction and a slight wall-time *regression* (best
738.9 → 755.0 ms — the per-call opt_eq tag-check overhead on the
hot `Integer == Integer` path, with no offsetting deopt saving).
Per "don't ship changes that don't earn their cost", it was
reverted. The branch is back to the validated Parts 3-B/B/C state.

> Lesson for the next continuation: confirm the *actual*
> deopt-causing instruction (it is recorded at the cmp pc but may be
> a guard `emit_generic_cmp`/the numeric arm inserted, not the cmp
> itself) before optimising. `--features deopt` prints the deopt
> reason symbol (`__version_guard`, `_bop_guard`, the operand value,
> …) — use it to disambiguate.

### 5b. The real remaining rubykon gap vs CRuby `--yjit`

From §3a deopt-stats, what still dominates (none in the ratified
B-then-C / BinCmp-`Other` scope):

1. **Class-version churn.** `already_counted_as_liberty?` ~50K
   class-version-guard deopts. Something in rubykon's MCTS hot path
   keeps bumping the global class-version counter (suspects:
   per-node `MCTS::Node`/`Root` object/singleton creation,
   `define_method`, frozen/`extend`, or an inline-cache
   invalidation feeding the class-version bump). Investigate with
   `--features deopt` (confirm reason `__version_guard`) and
   `--features profile` recompile stats; the fix is to stop the
   churn (or make the class-version guard at a hot site
   recompile-once like Part B rather than deopt-per-iter), **not**
   more BinCmp polymorphism work. This is the highest-value next
   item and is a *different subsystem* from Parts 3-B/B/C.
2. **Arithmetic `binary_op` polymorphism.** `GameScorer#score_board`
   / `score_empty_cutting_point` `%6 + %7 [Integer][Integer]` deopt
   ~35K total (occasional non-Integer). `binary_op` has no
   `polymorphic` arg yet; the VM already records the POLY bit for
   BinOp (`f11603a`). Mirror Part 3-B/B for `binary_op` (generic
   `*_values` C-call, recompile-on-recv-miss, §6 invariant). NOTE:
   given 5a, first confirm via `--features deopt` that these are
   actually operand-type guards and not class-version churn too.
3. **Polymorphic method-call PIC (handoff option A).** `.nil?`
   (`Game.pass?`, `Enumerable#inject`), `.root?`
   (`Node#backpropagate`) deopt as `POLYMORPHIC … FuncId` — general
   MethodCall receiver polymorphism. Needs the N-way PIC / the
   currently-dead `send_not_cached`
   (`asmir/compile/method_call.rs`). Largest scope; deferred by the
   original handoff. CRuby `--yjit` gets these via its send PIC,
   which is much of its remaining lead.

### 5c. Ship the verified work

4. **Regression** in a benchmark env with optcarrot present:
   `bin/test` (full test + coverage + benchmark diff + optcarrot).
   §3 already shows lib `2053/0` + integration green under Ruby
   4.0.4; mirror that.
5. **PR** `claude/handoff-review-nyy4u` → `master`. Summarise Parts
   3-B / B / C, the monotone-recompile safety invariant (§6), the
   §3a before/after deopt + wall-time numbers, and explicitly that
   it solves the `[Symbol][NilClass]` `==` storm. List 5b items as
   scoped-out follow-ups. The reverted 5a commit pair (`bf91ecc` +
   `bbdb8df`) can stay in history or be dropped via interactive
   rebase before the PR — either is fine; nothing depends on it.

If a *wrong result* (not perf) ever shows in rubykon but not in the
§3 correctness matrix, first suspects: Part C's immediate-tag check
vs the `value.rs` tag table, and the Part B counter
(`COUNT_DEOPT_RECOMPILE`) loop-vs-method-JIT interaction
(`--features deopt`/`jit-log`).

---

## 6. Design invariant (must hold — re-check if extending)

> Part 3 (the polymorphic path) emits **no class-variance deopt**.
> Therefore Part B is a *monotone, one-shot* monomorphic→polymorphic
> transition: after recompile the site has no receiver-class guard
> left to fail, so it can never re-trigger recompilation. Bounded ⇒
> no infinite deopt→recompile loop.

Corollary, if you extend Part B to any new site (e.g. arithmetic
`binary_op`): only set `recompile_on_recv_miss = true` for sites
that have a non-deopting polymorphic alternative compiled when
`polymorphic == true`. Otherwise the recompile re-emits the same
guarded code and loops. The class-version guard is intentionally
**kept** on the generic path — it guards the global class-version
counter, not the receiver class, so it only fires on a real
`==`/`<=>` redefinition (rare, also monotone), never on class
variance.

---

## 7. Key source pointers (current)

- `monoruby/src/codegen/jitgen/compile/binary_op.rs` —
  `binary_cmp` / `binary_cmp_br` (polymorphic dispatch),
  `emit_generic_cmp` (Part 3-B/C emission), `cmp_generic_fn`
  (`CmpKind → BinaryOpFn`).
- `monoruby/src/codegen/jitgen/compile/method_call.rs` —
  `compile_method_call` (`recompile_on_recv_miss`; recv-class guard
  → `new_recompile_deopt` vs `new_deopt`; `JitType::Specialized`
  gated out). `guard_class_version` is now `pub(super)`.
- `monoruby/src/codegen/jitgen/compile.rs` — `call_binary_method`
  (threads `recompile_on_recv_miss`), `recompile_and_deopt`
  (pre-existing `AsmInst::RecompileDeopt` path; mirrored for the
  `position`/`JitType` handling).
- `monoruby/src/codegen/jitgen/asmir.rs` —
  `AsmInst::GenericBinOp` / `OptEqCmp`, `AsmIr::generic_binop` /
  `opt_eq_cmp` / `new_recompile_deopt`;
  `SideExit::RecompileDeoptimize`; `gen_asm` side-exit loop.
- `monoruby/src/codegen/jitgen/asmir/compile.rs` —
  `generic_binop` (Part 3-B slow path) and `opt_eq_cmp` (Part C
  fast path + slow fallback) lowering.
- `monoruby/src/codegen/jitgen.rs` — `side_exit_with_label`
  (now takes `recompile: Option<(RecompileReason,
  Option<BytecodePtr>)>`; recompile emitted **after** write-back,
  **before** `jmp fetch`), `gen_recompile_deopt_with_label`.
- `monoruby/src/codegen/jitgen/deoptimize.rs` —
  `recompile_and_deopt` (the pre-existing counter+recompile the
  Part B side exit's design mirrors).
- `monoruby/src/executor.rs` — `RecompileReason::BecamePolymorphic`.
- `monoruby/src/executor/op.rs` — `cmp_{eq,ne,lt,le,gt,ge,teq}_values`
  (the generic `BinaryOpFn`s; same ones the VM's generic path uses).
- `monoruby/src/codegen/vmgen.rs` — `vm_save_binary_class`
  (sets the POLY bit; original mechanism unchanged).

Memory note: project memory `project_rubykon_polymorphic_jit.md`
(index in MEMORY.md) tracks the same conclusions for cross-session
recall.
