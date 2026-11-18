# The built-in profiler (`--features prof`)

A profiler for tuning monoruby itself: which Ruby methods the time goes to,
and in which part of the runtime (JIT code, the interpreter, builtins, GC,
the JIT compiler, the parser, `malloc`); what allocates objects; what the
collector costs; what the JIT compiles and recompiles; and who calls
`malloc`. The code is `monoruby/src/prof.rs`.

It complements the older measurement features rather than replacing them:
`profile` (deopt sites, recompile reasons, method caches), `jit-log`
(per-unit compile logs), `deopt`, `gc-log` (exit totals and the
`malloc` size-class histogram) and `perf` (a perf map for `perf record`).

## Enabling it

```sh
cargo build --release --features prof
MONORUBY_PROF=cpu target/release/monoruby script.rb        # report on stderr
MONORUBY_PROF=all MONORUBY_PROF_OUT=/tmp/p target/release/monoruby script.rb
#   -> /tmp/p.txt, /tmp/p.html (the viewer: open it in a browser),
#      /tmp/p.cpu.folded, /tmp/p.alloc.folded, /tmp/p.malloc.folded
```

A build without the feature has none of this code. A `prof` build that is
not asked to profile anything (`MONORUBY_PROF` unset) differs from a plain
release build by one relaxed load per `malloc` / `free` and per phase
change; measured with cachegrind it is within ±0.4% Ir of master
(so_nbody +0.38%, binarytrees −0.20%, both mostly code layout).

| Variable | Meaning | Default |
|---|---|---|
| `MONORUBY_PROF` | Comma list of `cpu`, `alloc`, `malloc`, or `all` | (off) |
| `MONORUBY_PROF_CLOCK` | `wall`: a high-resolution timer on the main thread's wall clock. `cpu`: the process's CPU time (`ITIMER_PROF`), every thread, but only as fine as the kernel tick (often 4 ms, i.e. ~250 Hz) | `wall` |
| `MONORUBY_PROF_HZ` | Sampling rate | 1000 |
| `MONORUBY_PROF_ALLOC_EVERY` | Record the whole stack of every N-th object allocation (the per-class / per-method counts are exact regardless) | 64 |
| `MONORUBY_PROF_MALLOC_EVERY` | Record the stacks of every N-th `malloc` | 64 |
| `MONORUBY_PROF_OUT` | Write the report to `PREFIX.txt`, [the viewer](#the-viewer-prefixhtml) to `PREFIX.html` and the stacks to `PREFIX.{cpu,alloc,malloc}.folded` | stderr, nothing else |
| `MONORUBY_PROF_INLINE` | `1`: show frameless-inlined callees as frames of their own (see [below](#frameless-inlined-callees-monoruby_prof_inline1)) | (off) |
| `MONORUBY_PROF_TOP` | Rows per table | 30 |
| `MONORUBY_PROF_BUF_MB` | Sample buffer (reserved lazily; samples past it are counted as dropped) | 512 |

The `.folded` files are Brendan Gregg's folded-stack format: feed them to
`flamegraph.pl`, `inferno-flamegraph` or speedscope. In the CPU stacks the
last two frames are the tier (`[jit]`, `[vm]`, `[native]`, `[gc]`, …) and,
for samples outside generated code, the native function the PC was in.

## The viewer (`PREFIX.html`)

With `MONORUBY_PROF_OUT`, the run also writes `PREFIX.html`: the same
profile as a page to explore in a browser. It is one self-contained file
— the page (`monoruby/src/prof/viewer.html`, compiled into a `prof`
binary) with the run's data in it — so it needs no server and no network,
and can be copied, attached or archived as it is. Light and dark follow
the system (or the button at the top right).

Two pages to try, written by real runs (optcarrot with all three profiles,
aobench with `cpu` alone), are on the documentation site:
<https://sisshiki1969.github.io/monoruby/docs/profiler.html>.

- **Time line** (when `cpu` is on) — the run cut into up to 300 equal
  buckets of CPU samples, each drawn as its mix of tiers, with the objects
  allocated and the bytes `malloc`ed in each below. **Drag over a part of
  it** and every table and flame graph on the page is recomputed for that
  part: the warm-up apart from the steady state, one GC burst, the
  stretch where the JIT was compiling. A click, or *Whole run*, clears the
  selection. **Click a tier in the legend** to pick it out: it moves to
  the bottom of each bar in its own color, so its share reads off the
  axis, and the other tiers turn gray; a second click (or Escape) brings
  them back.
- **Overview** — the totals, the tiers, the hottest methods, the most
  allocated classes, the collector.
- **Flame graph** — of CPU time, objects allocated, `malloc` calls or
  `malloc` bytes. Ruby frames are neutral; the colored frame under each
  stack is what the sample *was*: its tier and, outside generated code,
  the Rust / C function the PC was in (for objects, the class; for
  `malloc`, the Rust call site). A frameless-inlined callee is in italics
  and marked `[inlined]`. Click a frame to zoom into it, type in the box
  to highlight frames by name (and read off their share), and switch to
  **Callers** to turn the graph over: leaves at the top, their callers
  below — who is responsible for `memmove`, or for the `[GC]` tier.
- **Methods** — self and total per method for the same four measures,
  sortable, with the self time's mix of tiers as a bar. Selecting a
  method shows its callers, its callees and what its self time (or its
  allocations) consisted of; *Show in flame graph* highlights it there.
- **Native functions**, **JIT** (every compilation unit with its samples,
  code size and compile time; what was compiled more than once; the
  frameless-inlined callees), **Objects & GC**, **malloc** — the report's
  other tables, sortable.

The numbers are the report's: the page computes them from the same
records. Two things differ. The object tables are the exact counts for
the whole run, but for a selected part of the run they are estimates from
the sampled stacks (one allocation in `MONORUBY_PROF_ALLOC_EVERY`), and
the page says so. And nothing is cut to `MONORUBY_PROF_TOP` rows.

The data is the stacks themselves, not the tables: every distinct stack
once (`View` in `prof.rs` describes the format), and per bucket of the
time line how often each occurred. A record's bucket is the number of CPU
samples taken before it, so the time line needs `cpu`; without it there
is one bucket and no time line. Sizes are modest: the page of a
600-frame optcarrot run with all three profiles is about 240 KB, 65 KB of
it the viewer.

The report is written at process exit, after `at_exit` handlers and
finalizers. `exit!`, `exec` and a crash write nothing; a forked child
writes nothing either (only the process that started profiling reports).

## What the report contains

Every table starts with its title, a caption (the lines starting with
`# `: what the rows are and how to read the columns) and its column
heads. `grep -v '^  #'` drops the captions.

### `cpu`

- **where the time went** — every sample put in one tier, in this order
  of precedence: the runtime *phase* the thread was in (GC, JIT compile,
  parse / bytecodegen, the global allocator, the profiler's own
  bookkeeping), else where the PC was: the VM's code ("VM interpreter"), a
  JIT unit's code ("JIT code"), elsewhere in JIT memory ("JIT stubs /
  invokers / wrappers" — the native-method wrappers live there), or
  outside it ("builtins / runtime": Rust and libc).
- **methods by self time** — self is the newest Ruby frame (a native
  method such as `Array#sort` is a frame of its own), split by tier;
  total is every sample with the method anywhere on the stack.
- **methods by total time**.
- **JIT units by samples in their code** — which compiled units the JIT
  time is spent in, with their code size.
- **native functions by self samples** — the symbol the PC was in, for
  every sample outside generated code, tagged with its tier.

### JIT

Every compile (with or without `cpu`): count, loop compiles, abandoned
compiles, total compile time and code size; the slowest compiles; and the
`(method, self class)` pairs compiled more than once, with their total
time — the recompiles.

### `alloc` and GC

Object allocations counted exactly by class, by allocating method (the
newest Ruby frame when `RValue::pack` runs), and by both. Every collection
is timed (with or without `alloc`): minor and major separately, mark and
sweep, objects marked per collection.

**`alloc` changes the code being measured.** The JIT's inline allocation
(`emit_cell_acquire`) never calls into the runtime, so with `alloc` on it
is not emitted and every allocation takes the runtime path. Counts are
exact; time is not representative (binarytrees at depth 18 runs 2.2×
slower). Profile `cpu` in a separate run.

### `malloc`

Exact counts and bytes of `malloc` / `free` through the global allocator,
and for every N-th `malloc` its Ruby stack and its native call chain: the
estimated calls and bytes by Ruby method, and by Rust call site (the first
native frame that is not allocator plumbing — `alloc::`, `core::`,
`hashbrown::`, `smallvec::`, …).

## Frameless-inlined callees (`MONORUBY_PROF_INLINE=1`)

A callee the JIT inlined without a frame (frameless specialization,
`jitgen/compile/frameless_call.rs`) has no control frame, so the frame
walk cannot see it. By default its time, allocations and
`malloc`s are reported against the method whose frame it ran in, and the
callee's own row only holds what it did while it still had a frame (in the
interpreter, or from a call site that was not inlined). A small row for a
getter says nothing about how much time is spent in it.

With `MONORUBY_PROF_INLINE=1` the callee gets a frame in the report:

```
methods by self time:
     self      %   total      %    jit  ...  method
      151  33.7%     151  33.7%    151  ...  V#dot (inl.rb:4)
      129  28.8%     448 100.0%    129  ...  Object#work (inl.rb:13)
       85  19.0%     121  27.0%     85  ...  V#each2 (inl.rb:7)
       36   8.0%      36   8.0%     36  ...  block in Object#work (inl.rb:18)

frameless-inlined callees by self samples, callee <- the method it ran in:
      151  33.7%  V#dot (inl.rb:4) <- Object#work (inl.rb:13)
       36   8.0%  block in Object#work (inl.rb:18) <- V#each2 (inl.rb:7)
```

- **methods by self / total time**, the allocation tables and `malloc`'s
  "by Ruby method" count an inlined callee like a called one. A method's
  row is the sum of its framed and its inlined executions.
- **frameless-inlined callees by self samples** is the inlined part
  alone, per (callee, method whose frame it ran in). A callee nested in
  another frameless callee is listed under the method that has the frame.
- In the folded stacks the callee is a frame marked `[inlined]`.
- "JIT units" is unchanged: the callee's code is part of the unit.

How: the JIT reports the address range of every frameless body it emits
(main and cold page), with the callee and the function whose frame the
body runs in. Bodies nest as the inlining does. Each CPU sample,
allocation and sampled `malloc` records its *site*: the PC if it is in
JIT memory, otherwise the first return address into JIT memory on the
native frame-pointer chain (looked for up to 64 frames up), which is the
call from generated code into the runtime. At exit a site inside a body
gives the chain of callees, applied only if the sample's newest Ruby
frame is the function the body was compiled into.

What it costs: nothing when off. When on, the JIT records two address
pairs per frameless body; a CPU sample walks a few more native frames; an
object allocation walks the native stack up to the JIT frame (a loop
that does nothing but allocate ran 38% more instructions under
`alloc` with it than without; more on a fiber's stack, where every read
is probed). `malloc` and `cpu` do not change measurably.

What it cannot tell:

- An object the escape analysis kept virtual in the callee and
  materialized after the body returned (an Array the callee returns,
  stored by the caller) is allocated by the caller's code, and is charged
  to the caller.
- A sample in the few instructions around the body (filling the callee's
  window, restoring the caller's float registers) is the caller's.
- Callees inlined by other means (attribute readers and writers, the
  builtin methods the JIT expands inline) have no body of their own and
  stay with the caller.

## How a sample is taken

Each OS thread knows the `Executor` running on it: `prof::start` sets it
for the main program, `prof::enter_thread` for a native thread's body, and
`prof::set_executor` follows every fiber switch (`Fiber#resume` /
`#transfer`, the enumerator fibers) and green-thread resume. Each thread
also carries a phase byte, set by guards around `Allocator::gc`, the JIT
compile in `Codegen::compile`, the parser and `bytecode_compile`, and the
global allocator.

A sample is the phase, the PC, the `FuncId` of every Ruby frame (the `cfp`
chain, read from the executor's `cfp` field; each control frame points at
its local frame, whose `Meta` holds the `FuncId`) and up to 8 native
return addresses (the frame-pointer chain, which `-Cforce-frame-pointers`
keeps intact, generated code included). It goes into a lock-free,
lazily-committed buffer and is decoded only at exit.

The CPU sample is taken inside the `SIGPROF` handler, so it may interrupt
any instruction. A call publishes its control frame before it stores the
frame's `lfp`, so for two instructions the newest frame holds a stale
`lfp`. The handler therefore reads through a fault-free reader: addresses
in the interrupted thread's own stack directly, anything else (heap
frames, a fiber's stack) after probing it with `process_vm_readv`, which
fails instead of faulting. A stale `FuncId` that names no function is
dropped at report time. The allocation hooks are called synchronously,
where frames are consistent, and read directly.

PCs are classified at exit, when every region is known: the VM's code
(`vm_code_position`), each JIT unit's main and cold ranges (recorded when
the unit is compiled; old code is never reused, so the ranges stay
unique), and the rest of the JIT pages (`JitMemory::include`). Native PCs
are symbolized from the executable's own `.symtab` (which `backtrace`
alone misses for LLVM's `.llvm.*` local symbols), then with `backtrace`,
then as `object+offset`.

## Limits

- Frameless-inlined callees have no frame, so their time and allocations
  go to the method they were inlined into unless `MONORUBY_PROF_INLINE=1`
  is set. A method JIT unit that inlines callees shows up as one row in
  "JIT units" either way.
- The wall-clock mode samples the main thread only. `MONORUBY_PROF_CLOCK=cpu`
  samples whichever thread is on the CPU, at the kernel tick's rate.
- The program's boot (`Executor::init`'s prelude) runs before the timer is
  armed; its `malloc`s are sampled, with no Ruby frame.
- `SIGPROF` belongs to the profiler while `cpu` is on; a Ruby
  `trap("PROF")` would replace its handler.
- Linux only for `cpu` (it reads the signal context and uses
  `timer_create` / `process_vm_readv`); `alloc`, `malloc`, GC and JIT do
  not depend on the OS, though a `prof` build has so far been tried on
  Linux only (x86-64, and aarch64 under qemu).
