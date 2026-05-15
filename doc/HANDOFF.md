# HANDOFF — rubykon polymorphic-JIT work

Branch: `jit-poly-bincmp` (commit `f11603ad`). Base: `master`.
Date: 2026-05-16. Owner handing off → Claude Web continuation.

> Rule: never push to `master`; this work stays on `jit-poly-bincmp`
> and lands via PR. Verify branch before any commit (a merged PR may
> auto-switch the checkout to `master`).

---

## 1. Problem

ruby-bench `rubykon` runs ~1.5× slower than CRuby 4.0.1 `--yjit`.

Measured (19×19 board, 100 iters, best of 10; clean release build):

| Engine | best |
|---|---|
| CRuby 4.0.1 `--yjit` | ~367 ms |
| monoruby | ~559 ms |
| CRuby 4.0.1 interpreter | ~850 ms |

Reproduction script: `/tmp/rubykon_run.rb` (requires
`requires "rubykon"` from `/home/monochrome/yjit-bench/benchmarks/rubykon/lib`).
Profiling build: `cargo build --release --features profile`
(toolchain note in §6). `perf` does NOT work on this WSL2 kernel —
use the `profile` feature's deopt-stats instead.

## 2. Root cause (confirmed)

`profile` deopt-stats: **>10M deoptimizations on a single `==`** in
`MoveValidator#spot_unoccupied?`, millions more in `no_suicide_move?`,
`gain_liberties_from_capture_of`, `candidate_eye_color`,
`already_counted_as_liberty?`, etc. Recompile count is ~1–2, so these
are pure per-call side-exits, not recompiles.

Trigger: rubykon's board representation
(`/home/monochrome/yjit-bench/benchmarks/rubykon/lib/rubykon/board.rb`):

```ruby
EMPTY = nil; BLACK = :black; WHITE = :white
def spot_unoccupied?(id, board) = board[id] == Board::EMPTY   # i.e. == nil
```

`board[id]` is `nil` (empty point) or a `Symbol` (occupied). The `==`
receiver class oscillates `NilClass`↔`Symbol`.

monoruby's JIT compiles non-numeric `==` via
`binary_cmp` → `BinaryOpType::Other` → `call_binary_method` →
`compile_method_call`, which emits a **monomorphic receiver-class
guard that plain-deopts on mismatch**. Because the board starts empty,
the first ~20 calls see only `nil == nil`; the method is JIT-compiled
monomorphic for `NilClass` at the call threshold, then deopts forever
once `Symbol` appears. It is never recompiled.

YJIT's `opt_eq` inlines immediate/special-const equality with a
generic `send` fallback that stays in JIT code — it never side-exits
on class variance, so it avoids this entirely.

## 3. What is done (committed in `f11603ad`)

POLY-flag **plumbing + diagnostics only**. Behavior is unchanged
(flag recorded & surfaced, not yet used to alter codegen). Verified:
comparison semantics match CRuby incl. polymorphic `== nil`; rubykon
still ~559 ms; no regression.

- **VM (`monoruby/src/codegen/vmgen.rs`)** — `vm_save_binary_class`
  now compares the freshly observed `(lhs,rhs)` class pair against
  the inline-cache-cached pair and, once the cache is populated
  (cached lhs class ≠ 0), sets `opcode_sub = 1` (the POLY flag) on a
  class mismatch. Mirrors the existing MethodCall mechanism in
  `vmgen/method_call.rs` slow_path. New module const
  `OPCODE_SUB: i64 = 7 - 16`. `get_class` only clobbers `rax`, so
  `r8`/`r9` are safe scratch. This save routine is shared by the
  generic path of **both** cmp (op 140..=146 / 150..=156) and
  arithmetic binops (via `vm_generic_binop`), so the bit is recorded
  for BinOp too.
- **TraceIR (`monoruby/src/codegen/jitgen/trace_ir.rs`)** —
  `TraceIr::BinCmp` / `BinCmpBr` carry `polymorphic: bool`
  (`= pc.opcode_sub() == 1`). `cmp_fmt` prints a `POLY` prefix so
  `dump_iseq` and profile deopt-stats show which cmp sites are
  polymorphic. (MethodCall already printed `POLYMORPHIC`.)
- **Compile glue (`compile.rs`, `compile/binary_op.rs`)** —
  `binary_cmp` / `binary_cmp_br` take `polymorphic: bool`
  (currently `let _ = polymorphic;` — threaded, not consumed).

Verification done: a minimal repro (`/tmp/poly_repro.rb`) shows the
`POLY` marker on `Object#spot ... POLY %2 = %1 == %2`. In the rubykon
profile run, every 10M-deopt site now prints `POLY`, confirming the
VM sets the bit — **but it is set lazily, after monomorphic
compilation** (see §4).

## 4. Key finding that shapes the remaining design

The POLY bit is set by the VM **only after** it observes a second
class. rubykon's hot sites are JIT-compiled monomorphic *before* that
(empty board ⇒ only `nil == nil` for the first ≥20 calls). The
recv-class guard then fails and **plain-deopts with no recompile**:

`SideExit::Deoptimize` → `jitgen.rs::side_exit_with_label` =
write-back regs, `r13 = pc`, `jmp fetch` (back to interpreter for the
rest of that call). **There is no deopt counter and no recompile.**
Recompilation only happens via the `RecompileDeopt` AsmInst →
`jit_recompile_method` (`codegen/compiler.rs`), emitted only for
compile-time-unresolvable reasons (`NotCached` / `MethodNotFound` /
`IvarIdNotFound`) — never for a receiver-class guard miss.

This is true for **both** MethodCall and BinCmp (they share
`compile_method_call`). Neither recompiles on receiver-class
polymorphism today; the MethodCall `_polymorphic` flag is likewise
read but ignored (`compile.rs`, `_polymorphic: _`).

⇒ Fixing rubykon needs **two** cooperating pieces:

- **Part 3** — a polymorphic compilation that does **not** deopt on
  class variance.
- **Part B** — a one-shot recompile that flips a monomorphic-compiled
  site to the Part 3 path once it has become polymorphic.

## 5. Design decisions / tradeoffs (agreed direction)

### Invariant that prevents infinite deopt→recompile→deopt

> **Part 3 must emit code with no class-variance deopt.** Then Part B
> is a *monotone, one-shot* monomorphic→polymorphic transition: after
> recompile the site has no class guard left to fail, so it can never
> re-trigger recompilation. Bounded ⇒ no infinite loop.

Corollary: **decide and implement Part 3 before Part B.** If Part 3
still deopts on class variance, Part B is unsafe and must not ship.

### Part 3 options

- **A — Polymorphic Inline Cache (PIC):** N-way class dispatch +
  runtime resolve on miss (YJIT-style). Most general; needed
  eventually for general MethodCall polymorphism. High complexity:
  requires wiring the currently **dead** `send_not_cached`
  (`asmir/compile/method_call.rs:20`, zero callers) and PIC state
  management. Out of scope for the BinCmp-priority pass.
- **B — Generic C-call:** call the same generic C helper the VM's
  generic path uses (`cmp_<op>_values` — see the `vm_cmp_opt!` /
  `vm_generic_binop` macros in `vmgen.rs`) with **no class guard**.
  Inherently non-deopting (only the separate class-version / BOP
  guards remain, which fire on real redefinition, not class
  variance). Low risk. ~tens-of-ns per op, but a deopt is hundreds
  of ns + re-JIT-entry, so still a large win. monoruby's JIT
  currently has **no** generic-cmp C-call path, so this needs a new
  AsmInst that calls a `BinaryOpFn`.
- **C — Immediate fast-path + generic fallback (YJIT `opt_eq`):**
  for `==`/`!=`, inline identity/value compare for immediates
  (nil/Symbol/Integer/true/false) then C-fallback for heap objects.
  rubykon's 10M deopts are *all* `==`/`!=` with immediate operands
  (Symbol/nil/Integer), so this is fastest for the actual workload.

**Recommended path:** B as the foundation (makes Part B safe and
already removes the 10M deopts), then layer C onto `==`/`!=` as an
optimization. A (PIC) deferred to a future general-MethodCall pass.
Open sub-questions: `_opt` vs `_no_opt` C variant (use `_opt`, keeps
the fixnum fast path); confirm `==` redefinition is absorbed inside
the C helper's internal dispatch so only the class-version guard is
needed; cover non-`==` cmp (`<`,`<=>`,…) with the B path too.

This recommendation was presented; **user wants the Part 3 design
nailed down before coding Part B**. No final A/B/C selection
ratified yet — that is the first decision for the continuation.

## 6. Environment gotchas

- `/usr/bin/rustc` is an old 1.75 that shadows the rustup proxy and
  breaks `cargo build`. Build with:
  `RUSTC=$HOME/.rustup/toolchains/nightly-2025-10-15-x86_64-unknown-linux-gnu/bin/rustc PATH="$HOME/.rustup/toolchains/nightly-2025-10-15-x86_64-unknown-linux-gnu/bin:$PATH" cargo build --release [--features profile]`
- `perf` is unusable on this WSL2 kernel. Use `--features profile`
  deopt-stats (printed at process exit) for hot-spot analysis.
- monoasm uses 64-bit register names even for 32-bit ops
  (`movl r8, [mem]`, not `r8d`); `testq`/`cmpl` supported,
  `testl`/`r8d` are not.
- CRuby for comparison: `~/.rbenv/versions/4.0.1/bin/ruby [--yjit]`.

## 7. Next steps (ordered)

1. **Ratify Part 3 strategy** (A / B / B-then-C). Recommendation: B
   foundation + C for `==`/`!=`.
2. **Implement Part 3.** Add an AsmInst that calls the generic
   `BinaryOpFn` C helper (no class guard, error-checked, result →
   dst/acc); emit it from `binary_cmp`/`binary_cmp_br` (and later
   `binary_op`) when `polymorphic && BinaryOpType::Other`. Keep the
   class-version/BOP guards. Confirm no class-variance deopt remains.
3. **Implement Part B.** Route the recv-class guard in
   `compile_method_call` to a `RecompileDeopt` with a new
   `RecompileReason` (e.g. `BecamePolymorphic`) **only** when the
   originating site's POLY flag is set; otherwise keep plain deopt.
   Gate so existing monomorphic sites are unaffected.
4. **Measure** rubykon deopt counts (`--features profile`) and
   best-of-10 wall time vs the ~559 ms / YJIT ~367 ms baseline.
5. **Regression**: `bin/test` (or at least `cargo nextest` + the
   comparison-semantics check in `/tmp/cmp_correctness.rb`).
6. PR from `jit-poly-bincmp` → `master`.

## 8. Key source pointers

- `monoruby/src/codegen/vmgen.rs` — `vm_save_binary_class`
  (POLY-set), `vm_cmp_opt!` / `vm_generic_binop` (generic C-call the
  VM uses; Part 3 should reuse the same `*_values` fns).
- `monoruby/src/codegen/vmgen/method_call.rs` — original MethodCall
  POLY (`OPCODE_SUB`, `slow_path`).
- `monoruby/src/codegen/jitgen/trace_ir.rs` — BinCmp/BinCmpBr decode
  (op 140..=146 / 150..=156), `polymorphic` field, `cmp_fmt`.
- `monoruby/src/codegen/jitgen/compile/binary_op.rs` —
  `binary_cmp` / `binary_cmp_br` (Part 3 emission point).
- `monoruby/src/codegen/jitgen/compile/method_call.rs` —
  `compile_method_call` (shared recv-class guard, Part B point).
- `monoruby/src/codegen/jitgen.rs` — `side_exit_with_label`
  (plain deopt = jmp fetch, no recompile).
- `monoruby/src/codegen/compiler.rs` — `gen_recompile`,
  `recompile_method`, `jit_recompile_method`, `RecompileReason`.
- `monoruby/src/codegen/jitgen/asmir/compile/method_call.rs:20` —
  `send_not_cached` (dead code; the PIC building block for option A).
- `monoruby/src/codegen/jitgen/asmir/compile.rs` — `AsmInst::Deopt`
  vs `AsmInst::RecompileDeopt` lowering.

Memory note: project memory `project_rubykon_polymorphic_jit.md`
(index in MEMORY.md) tracks the same conclusions for cross-session
recall.
