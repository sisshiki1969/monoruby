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
  - **plan** — a proposal for work not yet done. Read for context, not as a
    description of the present.

---

## Execution core

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`stack_frame.md`](stack_frame.md) | EN | reference | What a local frame looks like in memory: LFP / CFP offsets, where `self`, the block and the registers sit. |
| [`hidden_ivars.md`](hidden_ivars.md) | EN | reference | Hidden (`/`-named) instance variables: how a built-in class written in Ruby keeps state that CRuby keeps in a C struct — out of `inspect` / `instance_variables`, safe from subclasses, writable when frozen — and the `# monoruby-hidden-ivars: true` magic comment that lets a Ruby file name one as `@__name`. |
| [`block_param.md`](block_param.md) | EN | design record | The `&block` parameter: a sentinel-initialized local slot, lazy `Proc` with write-back, proxy forwarding (CRuby's scheme). |
| [`method_args.md`](method_args.md) | JA | reference | What `pos_num`, `req_num`, `optional`, `rest` actually count, and what they exclude. |
| [`native_func.md`](native_func.md) | EN | reference | How to declare a builtin that takes optional / rest / keyword parameters. |
| [`super_resolution.md`](super_resolution.md) | JA | design record | The two non-obvious questions `super` has to answer — *which* name, and *which* position in the chain — and how monoruby answers them from the frame rather than from a method entry. |
| [`exception_handling.md`](exception_handling.md) | EN | reference | Raise, unwind, catch and report, and where monoruby is deliberately lazier than CRuby (backtraces are not formatted until asked for). |
| [`error_highlight.md`](error_highlight.md) | EN | reference | The snippet under an uncaught NameError / TypeError / ArgumentError: the vendored gem loaded on first use instead of at boot, and how a backtrace location answers the prism node id the gem asks `RubyVM::AbstractSyntaxTree` for — from the node's extent, through a private `ErrorHighlight::RubyVM`. Also did_you_mean and syntax_suggest, loaded on first use the same way. |
| [`cref.md`](cref.md) | EN | reference | The lexical state that is not local variables: default definee, constant scopes, `Module.nesting`, visibility toggles. Contrasts CRuby's per-frame CREF with monoruby's stack. |
| [`refinements.md`](refinements.md) | EN | design record | What refinements do to method resolution, why every cache in the tree was keyed without the caller's scope, and the interned-set design that adds them without costing a refinement-free program anything. |
| [`toplevel_binding.md`](toplevel_binding.md) | EN | design record | Why the main script stopped running inside `TOPLEVEL_BINDING` — a binding's heap frame is a *captured* frame, and captured frames never reach the loop JIT — and when the binding is built instead. |

## Codegen and IR

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`jit.md`](jit.md) | EN | reference | The stub a method starts life behind, and how it is rewritten as the method goes from cold to compiled. |
| [`inline.md`](inline.md) | EN | reference | Inline asm builtins: emitting a method's body straight into the caller instead of dispatching, and trial-inlining. |
| [`lir.md`](lir.md) | EN | reference | The arch-neutral machine-level IR between `AsmIR` and per-arch emission: operands, instruction groups, the side entries of `encode_linst`, and the legalization contract each backend implements. |
| [`trace_chain_joins.md`](trace_chain_joins.md) | EN | design record | Why the JIT's abstract state spans every frame of the trace being compiled: returns as merge edges with per-path kept-constant surrender, the resume asymmetry, the outer-frame float claims as ordinary state, and what the parked frame copies still are. |
| [`arch_difference.md`](arch_difference.md) | EN | reference | How the x86-64 and aarch64 backends differ, `AsmInst` by `AsmInst`. Read before touching either. |
| [`jit_invariants.md`](jit_invariants.md) | JA | reference | The invariants compiled code speculates on — method/constant resolution, basic ops, types, frame capture, `eval`, the absent TracePoint, a non-moving GC — how each break is detected, and the shared write-back / pc-restore / chain-conversion path back to the interpreter. Read first; the mechanism documents below are its details. |
| [`jit_invalidation.md`](jit_invalidation.md) | EN | reference | What moves the class / constant version counters, and why a version-guard failure repairs the compiled body ("salvage") instead of recompiling it. Also the megamorphic gate that decides whether a receiver class is compiled at all. |
| [`singleton_class_facts.md`](singleton_class_facts.md) | EN | reference | Why a class proof carried across a call (or folded from a constant) can go stale when the object gets a singleton class, and the per-class latch + dependency index that keeps compiled code right. |
| [`ivar_type_tracking.md`](ivar_type_tracking.md) | EN | reference | How the JIT types an ivar load by what each (class, ivar) slot has ever held, where stores are observed, and what invalidates a typed load when a slot's type widens. |
| [`deopt_log.md`](deopt_log.md) | EN | reference | How to read a `--features deopt` record, and how the log names the guard that actually branched despite exits being deduplicated. The `DeoptCause` invariant every new guard has to satisfy, and the three things the log refuses to guess. |
| [`escape_analysis.md`](escape_analysis.md) | EN | reference | How the JIT elides an Array or Range literal used only as a receiver (`[a, b].max`, `(lo..hi).include?(x)`, `when 1..5`), and `*rest` handed into specialized callees: the elided object in the abstract state, the receiver escape summaries that decide what a callee may be handed, and where the object is materialized after all. |
| [`arg_forwarding_jit.md`](arg_forwarding_jit.md) | JA | design record | How `def f(a, ...) g(...) end` is optimized: the four-tier argument setup, the D1/K1 deferral that skips the rest `Array` / kwrest `Hash` entirely (with its deopt-time materialization), the VM-tier lazy `(...)` marker convention, and what still falls back. |
| [`polymorphic_call.md`](polymorphic_call.md) | JA | reference | Receiver-polymorphic call sites: the VM's polymorphism detection and the polymorphic method cache (PMC), the class-set guard, the N-way PIC, binary-operator and `[]` dispatch, the generic-send residual, and the open questions (merge precision, union types). |
| [`bop_redefinition.md`](bop_redefinition.md) | JA | reference | What licenses inlining `1 + 2` and what happens when Ruby revokes it: the `(class, op)` table, detection (including mixins and refinements), the VM's per-operator flag word, per-iseq JIT eviction and on-stack conversion, and how Ruby-written builtins are kept out of a redefinition's reach. Contrasted with CRuby. |
| [`chain_deopt.md`](chain_deopt.md) | EN | reference | Converting a whole suspended chain of compiled frames to interpreter frames in one step, without touching machine code: the per-site conversion stub, the shared continuation stub, the escalation count that bounds the walk to one compilation unit, and its callers (basic-op redefinition on every thread, side exits, captures). |

## Runtime services

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`safepoint.md`](safepoint.md) | JA | reference | The one mechanism GC, preemption and signal delivery all go through. Read this before any of the three below. |
| [`gc.md`](gc.md) | JA | reference | The collector as it actually is — non-moving, single-threaded, stop-the-world, generational — plus the `GC` module's real numbers. |
| [`signal.md`](signal.md) | JA | reference | Deferred signal delivery: set a flag, convert it to a Ruby exception or a `Signal.trap` handler at the next safepoint. |
| [`threads.md`](threads.md) | JA | reference | Threads: the 1:1 model (kernel threads under the GVL, §12), the thread registry, Fibers, non-blocking IO, interrupts and time-slice preemption. |
| [`scheduler_state_diagram.md`](scheduler_state_diagram.md) | JA | reference | `ThreadState` and `FiberState` transitions — the companion diagrams to `threads.md`. |

Diagrams referenced by the above: [`fiber_state_diagram.svg`](fiber_state_diagram.svg),
[`gc_state_transitions.svg`](gc_state_transitions.svg),
[`gc_write_barrier.svg`](gc_write_barrier.svg).

## Runtime optimization (`runtime_optimization/`)

How the core classes are represented and what the VM and the JIT do to
make their hot operations cheap, one document per class, each closing with
the places where the implementation knowingly differs from CRuby's. All in
Japanese; [`runtime_optimization/README.md`](runtime_optimization/README.md)
is the entry point.

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`runtime_optimization/array.md`](runtime_optimization/array.md) | JA | reference | How an `Array` is laid out, what `[]` / `[]=` / `<<` / `each` cost in the VM and in compiled code, and where CRuby's embedded / shared arrays have no counterpart. |
| [`runtime_optimization/hash.md`](runtime_optimization/hash.md) | JA | reference + design record | What one `Hash#[]` actually costs: the three representations (inline / boxed / identity-keyed), the vm-free prehashed probe, the machine-code probe, the optimizations landed so far with their measurements, and a ranked list of what is left. Contrasts CRuby's `ar_table` / `st_table`. |
| [`runtime_optimization/string.md`](runtime_optimization/string.md) | JA | reference | Inline vs heap storage, shared substrings and copy-on-write, frozen literals and the literal pool, the encoding model, and how strings are hashed as keys. |
| [`runtime_optimization/regexp.md`](runtime_optimization/regexp.md) | JA | reference | Onigmo behind `Regexp`, how compiled patterns and `$~` are stored, which `String` methods take a regexp fast path, and what CRuby does differently. |

## Compatibility and performance

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`ruby_spec_skip_tags.md`](ruby_spec_skip_tags.md) | JA | reference | How the ruby/spec suite avoids hangs: per-example tags, how mspec finds a tag file, finding the hanging example, the spec-core monitor that tags timeouts automatically, and the tags that exist today. |
| [`profiler.md`](profiler.md) | EN | reference | The built-in profiler (`--features prof`, `MONORUBY_PROF=cpu,alloc,malloc`): Ruby methods by self / total time split by runtime tier, JIT units, native symbols, allocations by class and method, GC and JIT compile times, `malloc` call sites; the self-contained HTML viewer it writes (time line, flame graphs, sortable tables); how a sample is taken safely from a signal handler, and what it cannot see. |

## Native extensions

| Document | Lang | Kind | Answers |
|---|---|---|---|
| [`native_extension_loading.md`](native_extension_loading.md) | JA | design record | The native halves (sqlite3 / zstd / psych / nokogiri / markly / bcrypt / digest) as dlopen'd extensions behind monoruby's own C ABI (`MrApi`) instead of in the core binary: the options compared (CRuby C-API layer, own ABI, system libraries, feature gating, out-of-process, WASM, Fiddle), the invariants, the actual ABI and what the move taught, `bundled` / `system`, and how an install is produced (§7: the extensions as Cargo artifact dependencies, and the relocatable tree `bin/dist` stages). |
| [`c_api.md`](c_api.md) | JA | reference | The CRuby-compatible C API (`monoruby_capi/`): a C extension written for CRuby rebuilt against monoruby's `ruby.h` and loaded with a plain `require`. Why API- not ABI-compatible, why the `rb_*` layer is C linked into each extension, how `rb_raise` longjmps only across the extension's own frames, how every VALUE handed to C stays rooted for the call, TypedData / alloc funcs, mkmf and `gem install`, and the standard-library C extensions built on it. |
| [`nokogiri.md`](nokogiri.md) | JA | design record | Nokogiri on monoruby: the C extension rewritten in Rust over the bundled libxml2 (`libxml2-src`), the object / lifetime model, what is implemented, versus a C-API layer or a pure-Rust engine. |

Images: [`benchmark.png`](benchmark.png), [`chart.png`](chart.png),
[`optcarrot_benchmark.png`](optcarrot_benchmark.png),
[`optcarrot_fps_history.png`](optcarrot_fps_history.png),
[`optcarrot_fps_history_opt.png`](optcarrot_fps_history_opt.png).

---

## Where to start

- **Adding a builtin** → `native_func.md`, then `method_args.md`.
- **Changing method dispatch** → `cref.md` and `refinements.md` for what
  resolution depends on; `super_resolution.md` if `super` is involved.
- **Touching the JIT** → `jit_invariants.md` for what compiled code assumes,
  `jit.md` for the entry states, `arch_difference.md` before anything
  arch-specific, `lir.md` for the layering.
- **Anything asynchronous** (GC, signals, threads) → `safepoint.md` first.
- **A ruby/spec failure that hangs** → `ruby_spec_skip_tags.md`.

## Adding a document

Put it here, add a row above, and say which of the three kinds it is. Add it to `docs/src/SUMMARY.md` too if it should appear in the book. A
design record earns its place by recording the *rejected* options and the
measurements, not just the chosen one — that is what makes it still
useful once the code has moved.
