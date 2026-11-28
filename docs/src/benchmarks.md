# Benchmark

monoruby is performance-focused, and performance is measured continuously
rather than quoted from a README. This page covers the live dashboards and how
to reproduce a measurement locally.

## Live dashboards

Every push to `master` that touches the interpreter re-runs the benchmark
suite and republishes the [project
portal](https://sisshiki1969.github.io/monoruby/):

| Dashboard | What it shows |
| --- | --- |
| [Performance vs YJIT (x86-64)](https://sisshiki1969.github.io/monoruby/bench/) | Per-benchmark speed relative to CRuby + YJIT and + ZJIT, plus a history chart per benchmark |
| [Performance vs YJIT (aarch64)](https://sisshiki1969.github.io/monoruby/bench-arm64/) | The same suite on an Apple Silicon runner |

The portal also hosts the two ruby/spec dashboards; those are covered in
[Compatibility](compatibility.md).

Methodology, from `.github/workflows/bench.yml`:

- The suite is [yjit-bench](https://github.com/Shopify/yjit-bench), run with
  `--rss --harness=harness-warmup`. Three engines are measured on the same
  runner in the same job: monoruby, `ruby --yjit`, and `ruby --zjit` built
  from ruby-head.
- The reference CRuby is 4.0.9 (`ruby/setup-ruby`), and monoruby is installed
  with `cargo install --path monoruby --locked`.
- Each benchmark gets a 1000-second timeout **per engine** — the same rule
  for all three, since they are being compared. A benchmark that times out or
  exits non-zero is recorded as a failure rather than silently dropped, and
  shows on the dashboard as a gap.
- `harness-warmup` iterates until its median absolute deviation falls under a
  threshold that relaxes as it goes. The workflow asks for 2% and gives two
  points back per iteration (`MAD_TARGET=0.02`,
  `MAD_INCREASE_PER_ITER=0.02`) rather than the 0.1% default: on a shared
  GitHub runner with several of these jobs in flight, the tighter default is
  a precision the environment cannot deliver, and chasing it cost hundreds of
  iterations per benchmark.
- The published `ratio` is **× YJIT, higher = faster**, 1× being parity — for
  both engines, each measured as YJIT's time over its own. `latest.json` and
  `data/history.csv` next to each dashboard hold the raw numbers if you want
  to plot them yourself.

As a snapshot: on the x86-64 run of commit `a29f308` (2026-11-08), 77 of the
79 benchmarks produced a ratio on both interpreters; over those the geometric
mean was **1.55× YJIT**, with monoruby ahead on 49 of them. (ZJIT, measured
the same way on the same run, came out at 1.51×.) The spread is wide in both
directions — from ~26× on `30k_variables` down to ~0.4× on `30k_methods` —
which is the point of reading the dashboard rather than a single headline
number.

## Reproducing a measurement locally

The helper scripts in `bin/` wrap the two harnesses the project uses. Most of
them assume [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver)
and `rbenv`-managed reference Rubies; adjust the version strings inside to
match what you have installed.

| Script | What it runs |
| --- | --- |
| `bin/bench` | The standard set (`app_fib`, `so_nbody`, `so_mandelbrot`, `app_aobench`, plb2) via benchmark-driver, against `4.0.9 --yjit` and `4.0.9 --zjit` |
| `bin/compare` | Comparing **two git refs of monoruby** against each other — `bin/compare HEAD~1 HEAD` by default, over `app_fib`, `so_nbody`, `so_mandelbrot`, `quick_sort`, `integer`, `vm_send`, `vm_block`, `vm_yield` |
| `bin/ruby-bench` | The full yjit-bench suite (expects a `../ruby-bench` checkout), the same harness CI uses |
| `bin/optcarrot` | optcarrot on ruby, `ruby --yjit` and monoruby in turn (expects `../optcarrot`) |
| `bin/opt.rb` | optcarrot fps history over 3000 frames, the data behind the fps-history charts |
| `bin/index` | Array / Hash element access (`benchmark/index.yaml`) |
| `bin/inline` | Integer and Math methods, i.e. the inline-asm builtins, also run with `--no-jit` for contrast |
| `bin/ivar` | Instance-variable get/set, generic and `attr_`-generated |
| `bin/send` | Method dispatch, `Class.new`, Array literals and constant lookup |
| `bin/times` | `Integer#times`, JIT'ed Array / Hash work, `block_given?` |

`bin/compare` is the one to reach for when you want to know whether a change
you just made helped:

```sh
bin/compare                          # HEAD~1 vs HEAD, standard benchmarks
bin/compare abc1234 def5678          # two specific commits
bin/compare HEAD~5 HEAD app_fib.yml  # one benchmark
```

For a single script, a release build is enough:

```sh
cargo build --release
target/release/monoruby benchmark/app_fib.rb
```

Benchmark scripts and their benchmark-driver YAML configs live in
`benchmark/`. Passing `--no-jit` gives you the VM-only baseline for the same
script, which is often more informative than an absolute number.

## Profiling

monoruby has a profiler of its own that knows about Ruby frames and the
runtime's tiers — see [Built-in Profiler](profiler.md), which includes a live
example of the HTML page it writes:

```sh
cargo build --release --features prof
MONORUBY_PROF=cpu MONORUBY_PROF_OUT=/tmp/p target/release/monoruby benchmark/app_fib.rb
# -> /tmp/p.txt, /tmp/p.html, /tmp/p.cpu.folded
```

Linux `perf` works too, and sees what a sampling profiler inside the process
cannot (the kernel, other processes):

```sh
# Flame graph via Linux perf (needs ../FlameGraph)
bin/perf benchmark/app_fib.rb

# or by hand
cargo build --release --features perf
perf record target/release/monoruby benchmark/app_fib.rb
perf report
```

The `perf` feature makes monoruby emit perf-compatible symbol maps so
JIT-compiled frames get names instead of raw addresses.
`.cargo/config.toml` sets `-Cforce-frame-pointers=yes` globally, which is what
makes the stacks walkable.

`--features profile` collects deopt and recompile statistics instead of a time
profile — see [Development and Build Options](development.md#measurement-and-profiling).
