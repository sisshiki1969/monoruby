# HANDOFF — rubykon polymorphic-JIT work

Branch: `claude/handoff-review-nyy4u`. Base: `master`.
Last update: 2026-05-16. State: **Parts 3-B, B, C implemented &
regression-verified. Remaining: rubykon perf measurement, then PR.**

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
- This container has **no rubykon benchmark** and is **not** the
  perf-measurement environment. `perf` is unusable on the original
  WSL2 kernel; use `--features profile` deopt-stats (printed at
  process exit) for hot-spot analysis there.
- monoasm uses 64-bit register names even for 32-bit ops
  (`movl r8, [mem]`, not `r8d`); `testq`/`cmpl` supported,
  `testl`/`r8d` are not. `0b…` immediates work in `monoasm!`.

---

## 5. Remaining work (ordered) — for the benchmark environment

The implementation and correctness are done. What is left needs the
rubykon benchmark + profiling, which this container does not have.

1. **Measure rubykon** with a clean release build of this branch:
   - deopt counts via `cargo build --release --features profile`
     (deopt-stats printed at exit). Expectation: the >10M
     `spot_unoccupied?` / `no_suicide_move?` `==` deopts collapse to
     ~0 (Part B recompiles the site once; Part 3-B/C then has no
     recv-class guard to miss).
   - best-of-10 wall time (19×19, 100 iters) vs the ~559 ms
     baseline and CRuby 4.0.1 `--yjit` ~367 ms.
   - Reproduction: the original owner's `/tmp/rubykon_run.rb`
     (`require "rubykon"` from
     `…/yjit-bench/benchmarks/rubykon/lib`).
2. **If a perf regression or wrong result appears in rubykon**
   specifically (not reproduced by the §3 correctness tests), the
   first suspects are: Part C's immediate-tag check (verify against
   `value.rs` tag table), and the Part B counter
   (`COUNT_DEOPT_RECOMPILE`) interacting with the loop vs method JIT
   at that site (check `--features deopt`/`jit-log`).
3. **Optional further opt** only if measurement shows `==`/`!=`
   slow-path still hot: extend Part 3-B/C to `binary_op`
   (arithmetic) the same way (`binary_op` does not yet take a
   `polymorphic` arg; the POLY bit is already recorded for BinOp by
   the VM per `f11603a`).
4. **Regression** in the benchmark env: `bin/test` (full
   test + coverage + benchmark diff + optcarrot). In a Ruby-4.0+
   env this should be clean; mirror the §3 results.
5. **PR** `claude/handoff-review-nyy4u` → `master`. Summarise Parts
   3-B / B / C, the monotone-recompile safety invariant, and the
   rubykon before/after numbers from step 1.

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
