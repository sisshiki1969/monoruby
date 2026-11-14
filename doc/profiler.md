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
#   -> /tmp/p.txt, /tmp/p.cpu.folded, /tmp/p.alloc.folded, /tmp/p.malloc.folded
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
| `MONORUBY_PROF_OUT` | Write the report to `PREFIX.txt` and the stacks to `PREFIX.{cpu,alloc,malloc}.folded` | stderr, no stacks |
| `MONORUBY_PROF_TOP` | Rows per table | 30 |
| `MONORUBY_PROF_BUF_MB` | Sample buffer (reserved lazily; samples past it are counted as dropped) | 512 |

The `.folded` files are Brendan Gregg's folded-stack format: feed them to
`flamegraph.pl`, `inferno-flamegraph` or speedscope. In the CPU stacks the
last two frames are the tier (`[jit]`, `[vm]`, `[native]`, `[gc]`, …) and,
for samples outside generated code, the native function the PC was in.

The report is written at process exit, after `at_exit` handlers and
finalizers. `exit!`, `exec` and a crash write nothing; a forked child
writes nothing either (only the process that started profiling reports).

## What the report contains

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
  go to the method they were inlined into. A method JIT unit that inlines
  callees shows up as one row in "JIT units".
- The wall-clock mode samples the main thread only. `MONORUBY_PROF_CLOCK=cpu`
  samples whichever thread is on the CPU, at the kernel tick's rate.
- The program's boot (`Executor::init`'s prelude) runs before the timer is
  armed; its `malloc`s are sampled, with no Ruby frame.
- `SIGPROF` belongs to the profiler while `cpu` is on; a Ruby
  `trap("PROF")` would replace its handler.
- Linux only for `cpu` (it reads the signal context and uses
  `timer_create` / `process_vm_readv`); `alloc`, `malloc`, GC and JIT do
  not depend on the OS, though a `prof` build has so far been tried on
  x86-64 Linux only.
