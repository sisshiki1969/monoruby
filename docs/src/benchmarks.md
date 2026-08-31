# Benchmarks

monoruby is performance-focused, and performance is measured continuously
rather than quoted from a README. This page covers the live dashboards, how to
reproduce a measurement locally, and the historical one-off comparisons that
used to live in the project wiki.

## Live dashboards

Every push to `master` that touches the interpreter re-runs the benchmark
suite and republishes the [project
portal](https://sisshiki1969.github.io/monoruby/):

| Dashboard | What it shows |
| --- | --- |
| [Performance vs YJIT (x86-64)](https://sisshiki1969.github.io/monoruby/bench/) | Per-benchmark speed relative to CRuby + YJIT, plus a history chart per benchmark |
| [Performance vs YJIT (aarch64)](https://sisshiki1969.github.io/monoruby/bench-arm64/) | The same suite on an Apple Silicon runner |
| [ruby/spec core dashboard](https://sisshiki1969.github.io/monoruby/spec/) | Pass rates for the `core` spec group |
| [ruby/spec library dashboard](https://sisshiki1969.github.io/monoruby/library/) | Pass rates for the `library` spec group |

Methodology, from `.github/workflows/bench.yml`:

- The suite is [yjit-bench](https://github.com/Shopify/yjit-bench), run with
  `--rss --harness=harness-warmup`, monoruby against `ruby --yjit` on the same
  runner in the same job.
- The reference CRuby is 4.0.2 (`ruby/setup-ruby`), and monoruby is installed
  with `cargo install --path monoruby --locked`.
- Each benchmark gets a 400-second timeout per interpreter; a benchmark that
  times out or exits non-zero is recorded as a failure rather than silently
  dropped, and shows on the dashboard as a gap.
- The published `ratio` is **× YJIT, higher = monoruby faster**, 1× being
  parity. `latest.json` and `data/history.csv` next to each dashboard hold the
  raw numbers if you want to plot them yourself.

As a snapshot: on the x86-64 run of commit `a13bfe0` (2026-08-31), 59 of the
76 benchmarks produced a ratio on both interpreters; over those the geometric
mean was **1.28× YJIT**, with monoruby ahead on 30 of them. The spread is
wide in both directions — from ~16× on `string_malloc_pressure` down to ~0.3×
on `send_bmethod` — which is the point of reading the dashboard rather than a
single headline number.

## Reproducing a measurement locally

The helper scripts in `bin/` wrap the two harnesses the project uses. Most of
them assume [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver)
and `rbenv`-managed reference Rubies; adjust the version strings inside to
match what you have installed.

| Script | What it runs |
| --- | --- |
| `bin/bench` | The standard set (`app_fib`, `so_nbody`, `so_mandelbrot`, `app_aobench`, plb2) via benchmark-driver, against `4.0.5 --yjit` and `4.0.5 --zjit` |
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
[Where optcarrot --opt spends its time](design/optcarrot_opt_profile.md) is a
worked example of both.

## Historical measurements

The two comparisons below are one-off measurements previously published in the
project wiki. They are kept for the record and are **not** re-measured; for
current numbers use the live dashboards above.

### optcarrot (April 2024)

Measured with [optcarrot](https://github.com/mame/optcarrot).

Rubies:

- ruby 3.4.0dev (2024-04-27T08:56:20Z master 9ea77cb351) [x86_64-linux]
- truffleruby 24.0.1, like ruby 3.2.2, Oracle GraalVM JVM [x86_64-linux]
- truffleruby 24.0.1, like ruby 3.2.2, Oracle GraalVM Native [x86_64-linux]
- monoruby 3e348afd4141c40978342e67ad26d42dc0b8d2a7

![optcarrot benchmark](design/optcarrot_benchmark.png)

fps history, 0–3000 frames:

![optcarrot fps history](design/optcarrot_fps_history.png)

With `--opt` (optcarrot's self-rewriting optimization mode):

![optcarrot fps history, --opt](design/optcarrot_fps_history_opt.png)

### yjit-bench (December 2024)

Speed ratio against truffleruby; higher is better. Measured with
[yjit-bench](https://github.com/Shopify/yjit-bench) using
`--rss --harness=harness-warmup`. Benchmark sources are from
[ruby/ruby's `benchmark/`](https://github.com/ruby/ruby/tree/master/benchmark)
and [plb2](https://github.com/attractivechaos/plb2).

Rubies:

- monoruby 0.3.0
- ruby 3.4.1 (2024-12-25 revision 48d4efcb85) +YJIT +PRISM [x86_64-linux]
- truffleruby 24.1.1, like ruby 3.2.4, Oracle GraalVM Native [x86_64-linux]

![micro benchmarks](design/chart.png)

Raw data — execution time in milliseconds, resident set size (RSS) in MiB.
`monoruby/yjit` and `monoruby/truffle` are time ratios; above 1 means monoruby
is faster.

| bench         | monoruby (ms) | RSS (MiB) | yjit (ms) | RSS (MiB) | truffle (ms) | RSS (MiB) | monoruby/yjit | monoruby/truffle |
| :------------ | ------------: | --------: | --------: | --------: | -----------: | --------: | ------------: | ---------------: |
| bedcov        |        4412.0 |     234.3 |    4803.9 |     413.8 |       1881.8 |    1909.5 |         0.918 |            2.345 |
| binarytrees   |         175.0 |      28.7 |     137.0 |      22.0 |         31.7 |    1126.4 |         1.278 |            5.525 |
| matmul        |          39.7 |      35.0 |     121.8 |      22.8 |          1.4 |     803.2 |         0.326 |           29.059 |
| nbody         |           8.5 |      27.7 |      21.9 |      14.0 |          1.1 |     690.2 |         0.389 |            7.473 |
| nqueens       |          14.7 |      24.7 |      31.0 |      14.2 |          7.2 |     637.9 |         0.475 |            2.044 |
| optcarrot     |         520.4 |      79.0 |     720.9 |      54.7 |        432.2 |    1506.3 |         0.722 |            1.204 |
| rubykon       |         214.9 |      34.9 |     348.2 |      18.6 |         65.2 |    2279.9 |         0.617 |            3.298 |
| so_mandelbrot |          39.9 |      22.9 |     509.5 |      14.5 |         26.3 |     548.8 |         0.078 |            1.517 |
| sudoku        |          41.6 |      23.9 |      88.8 |      14.9 |         17.5 |    1165.2 |         0.469 |            2.381 |
| fib           |          16.7 |      23.5 |      17.4 |      15.0 |          8.8 |     483.4 |         0.960 |            1.895 |

### Machine

Both historical runs used the same machine:

- Architecture: x86_64
- CPU(s): 32 — 13th Gen Intel(R) Core(TM) i9-13900HX, 16 cores / 2 threads per core
- Caches (sum of all): L1d 768 KiB (16), L1i 512 KiB (16), L2 32 MiB (16), L3 36 MiB (1)
