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

As of commit `ebfcdca` (2026-11-08):

| Group | Examples | Passing | Pass rate | Categories at 100% |
| --- | ---: | ---: | ---: | ---: |
| `core` | 23264 | 23159 | **99.6%** | 49 / 59 |
| `library` | 6384 | 6287 | **98.5%** | 42 / 59 |

Additionally, as of July 2026 monoruby passed 100% of the command-line specs
and 99.6% of the language specs. Those two groups have no dashboard of their
own, so that figure is not re-measured on every push the way the two above
are.

Both numbers are the result of a sustained compliance push through 2026:

| Date | Commit | Group | Examples | Pass rate |
| --- | --- | --- | ---: | ---: |
| 2026-04-27 | `7d2dd48` | `core` | 22520 | 59.5% |
| 2026-08-31 | `a13bfe0` | `core` | 23029 | 97.6% |
| 2026-11-08 | `ebfcdca` | `core` | 23264 | **99.6%** |
| 2026-07-22 | `f27e893` | `library` | 5334 | 57.6% |
| 2026-11-08 | `ebfcdca` | `library` | 6384 | **98.5%** |

In `core`, only nine categories are below 100% at all, and eight of those are
within half a percent of it — `filetest` 98.9%, `time` 99.4%, `io` 99.5% over
1666 examples, `process` 99.5%, `file` 99.6%, `hash` and `module` 99.8%,
`kernel` 99.9%. The largest categories are clean: `string` 100% over 3963
examples, `array` 100% over 2910, `encoding` 100%.

The one substantial gap is deliberate. `tracepoint` sits at 0% over 79
examples because TracePoint is not implemented — the JIT's speculation
assumes its absence, see [Invariants compiled code speculates
on](design/jit_invariants.md). (`ruby` reports 0% too, but it contains no
examples.)

`library` was the frontier and no longer is: it went from 57.6% to 98.5% over
2026, and the step that did most of the work was the CRuby-compatible C API
layer landed in October. Libraries CRuby backs with a C extension now run
that extension rather than a stand-in, and the categories that used to sit
near zero are complete — `stringio`, `net-ftp`, `net-http`, `bigdecimal`,
`date` and `stringscanner` are all at 100%, and `socket` is at 99.9% over
1861 examples, the group's largest. What is left is small and scattered:
`etc` 54.0% (50 examples), `monitor` 75.0% (12), `cgi` 76.7% (43), `digest`
87.1% (124), `rbconfig` 87.5% (24), `securerandom` 88.1% (42), `erb` 92.5%
(53), `prime` 92.6% (68), `delegate` 95.9% (74) and `tempfile` 97.8% (45).
`coverage`, `readline` and `win32ole` report 0% but contain no examples.

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
  `spec/tags/`. That list is deliberately tiny, and has shrunk as the
  implementation caught up — **2 examples** in 2 files at present:
  `Refinement#import_methods` from a C extension, and
  `$LOAD_PATH.resolve_feature_path` for a `.so`. The published pass rate is
  therefore within a rounding error of an unfiltered one.

The audit that cut the skip list down from coarse file-level exclusions to
that handful is written up in [ruby/spec hang
countermeasures](design/ruby_spec_skip_tags.md). The structural hangs are
gone under either thread model: a blocking read releases the GVL (1:1) or
parks the calling thread on the scheduler's fd poller (M:1) instead of
blocking the process, so specs like `core/io/copy_stream_spec.rb` and
`core/io/select_spec.rb` run to completion.

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
- **The garbage collector is non-moving**, single-threaded and
  stop-the-world (generational since June 2026). Compiled code relies on
  objects not moving. See [GC — mechanism and
  implementation](design/gc.md).
- **Threads are real kernel threads behind a GVL.** Each Ruby `Thread` is an
  OS thread that holds a global lock while it runs Ruby, releasing it at
  safepoints and around blocking syscalls; the older M:1 green scheduler,
  with time-slice preemption, is still there behind
  `MONORUBY_THREAD_MODEL=green`. See [Thread / Fiber / non-blocking IO /
  preemption](design/threads.md).

C extensions are no longer on this list. Since October 2026 monoruby ships a
CRuby-compatible C API layer: an extension written for CRuby is rebuilt
against monoruby's headers (API- rather than ABI-compatible) and loaded by a
plain `require`, `extconf.rb` + `make` and `gem install` work against it, and
CRuby's own `ext/date`, `ext/stringio`, `ext/zlib`, `ext/socket`,
`ext/openssl` and `ext/bigdecimal` are built from unmodified upstream source.
That is what moved the `library` group from 57.6% to 98.5%. The layer is
documented in [CRuby-compatible C API layer](design/c_api.md) and [Native
extension loading](design/native_extension_loading.md); the older [C
extension support](design/c_extention.md) design study predates it and
describes the problem, not the shipped solution.
