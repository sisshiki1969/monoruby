# Built-in Profiler

monoruby carries a profiler of its own, behind the `prof` Cargo feature. It
answers the questions that come up when tuning a Ruby program on monoruby, or
monoruby itself:

- which Ruby methods the time goes to, and in which part of the runtime it is
  spent — JIT-compiled code, the interpreter, builtins, the garbage collector,
  the JIT compiler, the parser, `malloc`;
- what allocates objects, and what the collector costs;
- what the JIT compiles, how long each compile takes, and what it compiles
  more than once;
- who calls `malloc`, from Ruby and from Rust.

The result is a text report and a self-contained HTML page with a time line,
flame graphs and sortable tables. This page is the introduction;
[The built-in profiler — reference](design/profiler.md) has every table, how
a sample is taken, and the limits.

## Quick start

```sh
cargo build --release --features prof

# The report on stderr
MONORUBY_PROF=cpu target/release/monoruby script.rb

# The report, the HTML page and the folded stacks in files
MONORUBY_PROF=cpu MONORUBY_PROF_OUT=/tmp/p target/release/monoruby script.rb
#   -> /tmp/p.txt          the report
#      /tmp/p.html         the viewer: open it in a browser
#      /tmp/p.cpu.folded   folded stacks (flamegraph.pl, inferno, speedscope)
```

`MONORUBY_PROF` selects what to measure, as a comma-separated list:

| Mode | Measures |
| --- | --- |
| `cpu` | Samples the Ruby stack and the PC 1000 times a second |
| `alloc` | Counts every object allocation by class and by allocating method, and records the stack of every 64th |
| `malloc` | Counts every `malloc` / `free` and records the Ruby stack and the Rust call site of every 64th `malloc` |
| `all` | All three |

JIT compiles and garbage collections are timed in every mode.

A binary built without the feature has none of this code. A `prof` binary that
is not asked to profile (`MONORUBY_PROF` unset) runs within ±0.4% of a plain
release build, measured in instructions, so it can serve as the everyday
binary.

## An example: optcarrot

The page below is a real one, written by

```sh
MONORUBY_PROF=all MONORUBY_PROF_INLINE=1 MONORUBY_PROF_OUT=optcarrot \
    monoruby bin/optcarrot -b --frames 600 examples/Lan_Master.nes
```

and embedded as it came out. It is live: drag over the time line, switch
tabs, click a frame of the flame graph.
**[Open it in a tab of its own](profiler/optcarrot.html)** for the full
width.

<iframe src="profiler/optcarrot.html" title="monoruby profiler viewer: 600 frames of optcarrot" loading="lazy" style="width: 100%; height: 860px; border: 1px solid var(--table-border-color); border-radius: 6px;"></iframe>

Things to try in it:

- **Drag over the first 300 ms of the time line.** Every table and flame
  graph on the page is recomputed for just that part, and it is a different
  program from the steady state: optcarrot is building its lookup tables
  (`Array#transpose`, `PPU#setup_lut`), the JIT compiler takes a fifth of the
  samples, and JIT code only a quarter instead of 70%. A click, or *Whole
  run*, clears the selection.
- **Click *JIT compile* in the legend above the time line.** The other
  tiers turn gray and the compiler's share stands on the baseline, so the
  bursts of compiling are easy to find: most of them in the first 500 ms,
  and a tall one at 600 ms. A second click brings the other tiers back.
- **[Flame graph](profiler/optcarrot.html#flame).** Ruby frames are neutral;
  the colored frame under each stack is the tier the sample was counted in.
  `Optcarrot::PPU#render_pixel` is in italics and marked `[inlined]`: the JIT
  compiled it into `main_loop`'s block without a frame, and
  `MONORUBY_PROF_INLINE=1` shows it as a frame anyway. Switch the selector
  from *CPU time* to *Objects allocated* or *malloc bytes*, or switch to
  **Callers** to turn the graph over and see who is responsible for the `[GC]`
  tier.
- **[Methods](profiler/optcarrot.html#methods).** Self and total per method,
  sortable, with the self time's mix of tiers as a bar. Selecting a row shows
  the method's callers and callees.
- **[JIT](profiler/optcarrot.html#jit).** Every compilation unit with its
  samples, code size and compile time, and the methods compiled more than
  once.
- **[Objects & GC](profiler/optcarrot.html#objects)** and
  **[malloc](profiler/optcarrot.html#malloc)**. Allocations by class and by
  method, every collection timed, and `malloc` by Ruby method and by Rust
  call site.

The page is one file with the run's data in it (about 240 KB here, 65 KB of
it the viewer), so it needs no server and no network and can be attached to
an issue or archived as it is. The same run's text report is
[optcarrot.txt](profiler/optcarrot.txt).

## A second example: timings alone

The yellow banner at the top of the page above is there for a reason:
counting objects turns the JIT's inline allocation off, so every allocation
takes the slower runtime path and the CPU times are no longer those of a
normal run. For timings, profile `cpu` in a run of its own:

```sh
cd benchmark
MONORUBY_PROF=cpu MONORUBY_PROF_OUT=aobench-cpu monoruby app_aobench.rb
```

**[Open the aobench page](profiler/aobench-cpu.html)** (93 KB), or read
[its text report](profiler/aobench-cpu.txt), which starts like this:

```text
---- CPU: 3193 samples, main thread's wall clock at 1000 Hz ----
sampled run: 3.195 s wall, 3.181 s CPU; one sample = 1.001 ms (effective 999 Hz)

where the time went
  tier                                 samples       %
  JIT code                                1961   61.4%
  VM interpreter                             1    0.0%
  JIT stubs / invokers / wrappers          174    5.4%
  builtins / runtime (Rust, libc)          710   22.2%
  GC                                       311    9.7%
  JIT compile                               25    0.8%
  parse / bytecodegen                        1    0.0%
  malloc / free                             10    0.3%

methods by self time (top 30)
     self      %   total      %    jit    vm  stub native    gc  ...  method
     1347  42.2%    1348  42.2%    974     0    97     12   263  ...  Sphere#intersect (app_aobench.rb:75)
      970  30.4%    2869  89.9%    547     0    28    395     0  ...  block (2 levels) in Scene#ambient_occlusion (app_aobench.rb:205)
      380  11.9%     380  11.9%    321     0    21      0    37  ...  Plane#intersect (app_aobench.rb:106)
      234   7.3%     234   7.3%      0     0    19    215     0  ...  Kernel#rand (native)
```

Every sample is in exactly one tier, and a method's self time is split by
tier: `Sphere#intersect` spent 974 of its 1347 samples in its own compiled
code and 263 in the collector, which ran for the vectors it allocates.
`Kernel#rand` is a native method and a frame of its own.

## Options

| Variable | Meaning | Default |
| --- | --- | --- |
| `MONORUBY_PROF` | Comma list of `cpu`, `alloc`, `malloc`, or `all` | (off) |
| `MONORUBY_PROF_OUT` | Write `PREFIX.txt`, `PREFIX.html` and `PREFIX.{cpu,alloc,malloc}.folded` | report on stderr, nothing else |
| `MONORUBY_PROF_INLINE` | `1`: show callees the JIT inlined without a frame as frames of their own, marked `[inlined]` | (off: counted as the method they were inlined into) |
| `MONORUBY_PROF_CLOCK` | `wall`: the main thread's wall clock, at a high resolution. `cpu`: the process's CPU time on every thread, at the kernel tick's resolution (often 4 ms) | `wall` |
| `MONORUBY_PROF_HZ` | Sampling rate | 1000 |
| `MONORUBY_PROF_ALLOC_EVERY` | Record the stack of every N-th object allocation (the counts are exact regardless) | 64 |
| `MONORUBY_PROF_MALLOC_EVERY` | Record the stacks of every N-th `malloc` | 64 |
| `MONORUBY_PROF_TOP` | Rows per table of the text report (the HTML page has them all) | 30 |
| `MONORUBY_PROF_BUF_MB` | Sample buffer, reserved lazily; samples past it are counted as dropped | 512 |

## Good to know

- **Profile `cpu` and `alloc` separately** when the timings matter, as above.
  `MONORUBY_PROF_INLINE=1` does not change what `cpu` costs measurably.
- **The report is written at process exit**, after `at_exit` handlers and
  finalizers. `exit!`, `exec` and a crash write nothing, and a forked child
  writes nothing either.
- **`cpu` is Linux only**, and a `prof` build has so far been tried on Linux
  only (x86-64, and aarch64 under qemu); the other modes do not depend on the
  OS. The wall clock samples the main thread; use `MONORUBY_PROF_CLOCK=cpu`
  for a program whose work is on other threads.
- **`SIGPROF` belongs to the profiler** while `cpu` is on: a Ruby
  `trap("PROF")` would replace its handler.
- The profiler complements the older measurement features rather than
  replacing them: `profile` (deopt sites and recompile reasons), `jit-log`,
  `deopt`, `gc-log` and `perf` — see
  [Development and Build Options](development.md#cargo-features).

The two example pages were written by monoruby 0.3.0 at commit `eb09379` on
x86-64 Linux; the numbers in them are that machine's.
