# Compatibility

monoruby aims at CRuby 4.0 compatibility, and measures it against
[ruby/spec](https://github.com/ruby/spec) — the executable specification
suite CRuby itself is tested with. The suite is re-run on every push to
`master` that touches the interpreter, and the results are published as live
dashboards.

## Dashboards

| Dashboard | Scope |
| --- | --- |
| [ruby/spec core](https://sisshiki1969.github.io/monoruby/spec/) | The `core/` group — the built-in classes and modules |
| [ruby/spec library](https://sisshiki1969.github.io/monoruby/library/) | The `library/` group — the standard library |
| [rubyspec-stats](https://sisshiki1969.github.io/rubyspec-stats/) | Daily pass rates for monoruby, CRuby, TruffleRuby and JRuby side by side |

Each dashboard carries a per-category table, a history chart, and the raw
`latest.json` / `data/history.csv` behind it. The core dashboard also
publishes per-branch pages for branches under active spec work.

## Where things stand

As of commit `a13bfe0` (2026-08-31):

| Group | Examples | Passing | Pass rate | Categories at 100% |
| --- | ---: | ---: | ---: | ---: |
| `core` | 23029 | 22468 | **97.6%** | 39 / 59 |
| `library` | 5330 | 3148 | **59.1%** | 23 / 59 |

Additionally, as of July 2026 monoruby passed 100% of the command-line specs
and 99.6% of the language specs.

The core figure is the result of a sustained compliance push through 2026 —
the first published measurement, on 2026-04-27, was **59.5%**:

| Date | Commit | Examples | Pass rate |
| --- | --- | ---: | ---: |
| 2026-04-27 | `7d2dd48` | 22520 | 59.5% |
| 2026-08-31 | `a13bfe0` | 23029 | 97.6% |

The largest core categories are in good shape — `array` 100% over 2898
examples, `file` 99.6%, `kernel` 99.5%, `module` 98.9%, `io` 96.2%, and
`string` 95.4% over 3905. What remains is concentrated rather than spread out:
`tracepoint` (TracePoint is deliberately not implemented — the JIT's
speculation assumes its absence, see [Invariants compiled code speculates
on](design/jit_invariants.md)), `objectspace` at 39.3%, then `time`,
`marshal` and `encoding` in the 89–95% band.

The library group is the frontier. `matrix` and `net-http` are essentially
complete (100% and 99.5%), while the biggest gaps are `socket` (40.2% over
1129 examples), `stringio` (67.5%), `net-ftp` (54.9%) and `bigdecimal`
(68.6%). Libraries needing facilities monoruby does not have — `openssl`,
`coverage`, `mkmf`, `irb` — sit at 0%.

## How the numbers are produced

Both dashboards come from the same workflow shape
(`.github/workflows/spec-core.yml` and `spec-library.yml`):

- Every `*_spec.rb` under the group is run through `mspec`, category by
  category, in batches of 10 files.
- Each batch runs under a hard 60-second deadline with `timeout -k 5`. The
  `-k` matters: monoruby installs its own `SIGTERM` handler which is deferred
  to a VM poll point, so a process stuck in a blocking read never dies to a
  plain `TERM`.
- A killed batch prints no summary line, which would lose the tally for all
  ten files, so the batch is re-run one file at a time — completing files are
  counted after all, and the hanging file is pinned down and recorded in
  `data/timeouts.csv`. Both groups currently record **zero** timeouts.
- `--excl-tag fails` excludes examples tagged in this repository's
  `spec/tags/`. That list is deliberately tiny — **6 examples** in 4 files at
  present, covering `Refinement#import_methods` from a C extension,
  `$LOAD_PATH.resolve_feature_path` for a `.so`, the `/o` Regexp modifier,
  and three refinement-driven pattern-matching cases. The published pass rate
  is therefore very close to an unfiltered one.

The audit that cut the skip list down from coarse file-level exclusions to
that handful is written up in [ruby/spec hang
countermeasures](design/ruby_spec_skip_tags.md). Green threads removed the
last structural hangs: blocking IO now parks the calling thread on the
scheduler's fd poller instead of blocking the process, so specs like
`core/io/copy_stream_spec.rb` and `core/io/select_spec.rb` run to completion.

## Running the specs yourself

ruby/spec and mspec are cloned alongside the monoruby checkout, not inside
it. See [Development and Build Options →
ruby/spec](development.md#rubyspec) for the layout and the commands.

## Deliberate differences from CRuby

A few behaviours differ by design rather than by omission:

- **TracePoint is not implemented.** Compiled code speculates on its absence.
- **Backtraces are formatted lazily.** Raise, unwind and catch record what is
  needed; the backtrace string is not built until something asks for it. See
  [Exception handling](design/exception_handling.md).
- **The garbage collector is non-moving**, single-threaded and stop-the-world
  (generational since June 2026). Compiled code relies on objects not moving.
- **Threads are M:1 green threads** with time-slice preemption, not OS
  threads. See [Thread / Fiber / non-blocking IO /
  preemption](design/threads.md).
- **C extensions cannot be loaded.** Libraries that CRuby backs with a C
  extension — `json`, `date`, `digest`, `stringio`, `zlib`, `openssl` and
  others — are either reimplemented in Rust/Ruby and shipped in the vendored
  tree, or unavailable. [C extension support](design/c_extention.md) is a
  design study, not a shipped feature.
