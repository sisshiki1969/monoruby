# `doc/` — index

Design records and implementation notes for monoruby's runtime. Start
here; `CLAUDE.md` at the repository root covers the layout, the build and
the conventions, and points back into this directory for anything deeper.

Two things to know before reading:

- **Language.** Some documents are written in Japanese and some in
  English; the table below says which.
- **Kind.** A document is one of three things, and they age differently:
  - **reference** — describes what the code does now, and is kept in step
    with it. Trust it, and update it when you change the subsystem.
  - **design record** — why a subsystem is shaped the way it is, with the
    measurements and the rejected alternatives. Still true about the
    *reasoning* even where the code has moved on.
  - **plan / history** — a proposal or a snapshot in time. Read for
    context, not as a description of the present.

---

## Execution core

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`stack_frame.md`](stack_frame.md) | EN | reference | What a local frame looks like in memory: LFP / CFP offsets, where `self`, the block and the registers sit. |
| [`method_args.md`](method_args.md) | JA | reference | What `pos_num`, `req_num`, `optional`, `rest` actually count, and what they exclude. |
| [`native_func.md`](native_func.md) | EN | reference | How to declare a builtin that takes optional / rest / keyword parameters. |
| [`super_resolution.md`](super_resolution.md) | JA | design record | The two non-obvious questions `super` has to answer — *which* name, and *which* position in the chain — and how monoruby answers them from the frame rather than from a method entry. |
| [`exception_handling.md`](exception_handling.md) | EN | reference | Raise, unwind, catch and report, and where monoruby is deliberately lazier than CRuby (backtraces are not formatted until asked for). |
| [`cref.md`](cref.md) | EN | reference | The lexical state that is not local variables: default definee, constant scopes, `Module.nesting`, visibility toggles. Contrasts CRuby's per-frame CREF with monoruby's stack. |
| [`refinements.md`](refinements.md) | EN | design record | What refinements do to method resolution, why every cache in the tree was keyed without the caller's scope, and the interned-set design that adds them without costing a refinement-free program anything. |

## Codegen and IR

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`jit.md`](jit.md) | EN | reference | The stub a method starts life behind, and how it is rewritten as the method goes from cold to compiled. |
| [`inline.md`](inline.md) | EN | reference | Inline asm builtins: emitting a method's body straight into the caller instead of dispatching, and trial-inlining. |
| [`lir.md`](lir.md) | EN | design record | The arch-neutral machine-level IR between `AsmIR` and per-arch emission — the design and the migration log. |
| [`regalloc_separation.md`](regalloc_separation.md) | EN | design record | Separating the abstract interpreter from register allocation. The largest document here; the structural groundwork the LIR work builds on. |
| [`trace_chain_joins.md`](trace_chain_joins.md) | EN | design record | Why the JIT's abstract state spans every frame of the trace being compiled: returns as merge edges with per-path kept-constant surrender, the resume asymmetry, the outer-frame float claims as ordinary state, and what the parked frame copies still are. |
| [`arch_difference.md`](arch_difference.md) | EN | reference | How the x86-64 and aarch64 backends differ, `AsmInst` by `AsmInst`. Read before touching either. |
| [`jit_invariants.md`](jit_invariants.md) | JA | reference | The invariants compiled code speculates on — method/constant resolution, basic ops, types, frame capture, `eval`, the absent TracePoint, a non-moving GC — how each break is detected, and the shared write-back / pc-restore / chain-conversion path back to the interpreter. Read first; the mechanism documents below are its details. |
| [`jit_invalidation.md`](jit_invalidation.md) | EN | reference | What moves the class / constant version counters, and why a version-guard failure repairs the compiled body ("salvage") instead of recompiling it. Also the megamorphic gate that decides whether a receiver class is compiled at all. |
| [`deopt_log.md`](deopt_log.md) | EN | reference | How to read a `--features deopt` record, and how the log names the guard that actually branched despite exits being deduplicated. The `DeoptCause` invariant every new guard has to satisfy, and the three things the log refuses to guess. |
| [`arg_forwarding_jit.md`](arg_forwarding_jit.md) | JA | design record | How `def f(a, ...) g(...) end` is optimized: the four-tier argument setup, the D1/K1 deferral that skips the rest `Array` / kwrest `Hash` entirely (with its deopt-time materialization), the VM-tier lazy `(...)` marker convention, and what still falls back. |
| [`polymorphic_call.md`](polymorphic_call.md) | JA | plan | Receiver-polymorphic call sites: why `x.nil?` / `x == nil` deopt on every nil receiver, the implemented nil-tolerant guard, and the design space (predicate generalization, PIC, page discipline) with measurements. |
| [`bop_redefinition.md`](bop_redefinition.md) | JA | design record | What licenses inlining `1 + 2`, what happens when Ruby revokes it, and why the current all-or-nothing response is both too narrow (silently wrong answers) and too blunt (24× slower, permanently). Measured against CRuby. |
| [`handoff_record_stream.md`](handoff_record_stream.md) | EN | plan / history | Handoff note for the record-driven lowering work; a summary of `regalloc_separation.md` §12–21 as of that branch. |
| [`chain_deopt.md`](chain_deopt.md) | EN | plan | Dropping a whole suspended JIT chain back to the interpreter in one step, so a speculation broken deep inside can be undone. What the VM already provides, one attempt that fails and why, and the return-type inference the mechanism would buy back. §8 records the implemented mechanism (and where it departs from the plan); §10 records the retirement of immediate eviction, which it replaced. The speculation itself is still unbuilt. |

## Runtime services

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`safepoint.md`](safepoint.md) | JA | reference | The one mechanism GC, preemption and signal delivery all go through. Read this before any of the three below. |
| [`gc.md`](gc.md) | JA | reference | The collector as it actually is — non-moving, single-threaded, stop-the-world, generational — plus the `GC` module's real numbers. |
| [`signal.md`](signal.md) | JA | reference | Deferred signal delivery: set a flag, convert it to a Ruby exception or a `Signal.trap` handler at the next safepoint. |
| [`threads.md`](threads.md) | JA | reference | M:1 green threads, Fibers, non-blocking IO and time-slice preemption. |
| [`scheduler_state_diagram.md`](scheduler_state_diagram.md) | JA | reference | `ThreadState` and `FiberState` transitions — the companion diagrams to `threads.md`. |

Diagrams referenced by the above: [`fiber_state_diagram.svg`](fiber_state_diagram.svg),
[`thread_state_diagram.svg`](thread_state_diagram.svg),
[`gc_state_transitions.svg`](gc_state_transitions.svg),
[`gc_write_barrier.svg`](gc_write_barrier.svg).

## Strings and encodings

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`encoding_char_iteration_design.md`](encoding_char_iteration_design.md) | EN | plan | Removing the "every String is UTF-8" assumption via a per-encoding character-boundary layer. Marked *proposed*. |

## Compatibility and performance

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`ruby_spec_skip_tags.md`](ruby_spec_skip_tags.md) | JA | reference | How the ruby/spec suite avoids hangs, and the audit that cut a coarse file-level skip list down to the handful that genuinely cannot run. |
| [`optcarrot_opt_profile.md`](optcarrot_opt_profile.md) | JA | design record | Where `bin/optcarrot --opt` spends its time, measured with `perf` and `--features profile`, and the optimizations that came out of it. |

## Plans and history

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`c_extention.md`](c_extention.md) | JA | plan | Design study for loading CRuby C extensions (`.so`). |
| [`plan-activerecord.md`](plan-activerecord.md) | JA | plan | Staged plan for running ActiveRecord, and what it depends on. |
| [`progress_2025-2026.md`](progress_2025-2026.md) | EN | history | What changed over ~500 commits, April 2025 to April 2026. |

Images: [`benchmark.png`](benchmark.png), [`chart.png`](chart.png),
[`optcarrot_benchmark.png`](optcarrot_benchmark.png),
[`optcarrot_fps_history.png`](optcarrot_fps_history.png),
[`optcarrot_fps_history_opt.png`](optcarrot_fps_history_opt.png).

---

## Where to start

- **Adding a builtin** → `native_func.md`, then `method_args.md`.
- **Changing method dispatch** → `cref.md` and `refinements.md` for what
  resolution depends on; `super_resolution.md` if `super` is involved.
- **Touching the JIT** → `jit.md` for the entry states, `arch_difference.md`
  before anything arch-specific, `lir.md` and `regalloc_separation.md` for
  the layering.
- **Anything asynchronous** (GC, signals, threads) → `safepoint.md` first.
- **A ruby/spec failure that hangs** → `ruby_spec_skip_tags.md`.

## Adding a document

Put it here, add a row above, and say which of the three kinds it is. A
design record earns its place by recording the *rejected* options and the
measurements, not just the chosen one — that is what makes it still
useful once the code has moved.
