# ruby/spec core Report — 2026-03-31 (master with all PRs merged)

Build: master @ `0c815719` (PRs #257-#261 merged)

## Overall Summary

| Metric | Value | vs Previous (r5) |
|--------|------:|------------------:|
| Categories | 58 | — |
| Examples | 17,686 | — |
| Expectations | 39,289 | — |
| **Failures** | **3,922** | +120 |
| **Errors** | **6,469** | +132 |
| **F+E** | **10,391** | **+252** |
| Pass rate | 73.6% | — |
| Crashes | 0 | — |
| Hangs | 2 (io/copy_stream, io/select) | — |

## Categories by F+E

| # | Category | F+E | Fail | Err | Examples | Note |
|---|----------|----:|-----:|----:|---------:|------|
| 1 | io | 1366 | 460 | 906 | 1375 |  |
| 2 | kernel | 1160 | 543 | 617 | 2187 |  |
| 3 | array | 992 | 353 | 639 | 3055 |  |
| 4 | file | 621 | 176 | 445 | 924 |  |
| 5 | encoding | 564 | 377 | 187 | 588 |  |
| 6 | string | 562 | 270 | 292 | 1101 |  |
| 7 | enumerator | 366 | 46 | 320 | 403 |  |
| 8 | process | 325 | 75 | 250 | 297 |  |
| 9 | thread | 297 | 95 | 202 | 285 |  |
| 10 | enumerable | 289 | 128 | 161 | 547 |  |
| 11 | time | 287 | 84 | 203 | 308 |  |
| 12 | module | 260 | 65 | 195 | 275 |  |
| 13 | range | 240 | 71 | 169 | 435 |  |
| 14 | hash | 216 | 110 | 106 | 597 |  |
| 15 | numeric | 198 | 121 | 77 | 273 |  |
| 16 | integer | 182 | 115 | 67 | 576 |  |
| 17 | exception | 179 | 89 | 90 | 246 |  |
| 18 | argf | 148 | 18 | 130 | 148 |  |
| 19 | method | 144 | 40 | 104 | 200 |  |
| 20 | complex | 142 | 35 | 107 | 181 |  |
| 21 | struct | 125 | 30 | 95 | 173 |  |
| 22 | env | 124 | 102 | 22 | 239 |  |
| 23 | dir | 121 | 64 | 57 | 336 |  |
| 24 | fiber | 119 | 24 | 95 | 140 |  |
| 25 | objectspace | 114 | 18 | 96 | 111 |  |
| 26 | matchdata | 106 | 9 | 97 | 130 |  |
| 27 | proc | 100 | 47 | 53 | 202 |  |
| 28 | data | 91 | 1 | 90 | 93 |  |
| 29 | float | 85 | 50 | 35 | 328 |  |
| 30 | regexp | 79 | 31 | 48 | 96 |  |
| 31 | sizedqueue | 78 | 33 | 45 | 129 |  |
| 32 | tracepoint | 76 | 1 | 75 | 75 |  |
| 33 | filetest | 70 | 23 | 47 | 89 |  |
| 34 | rational | 63 | 42 | 21 | 135 |  |
| 35 | random | 55 | 10 | 45 | 87 |  |
| 36 | set | 50 | 20 | 30 | 157 |  |
| 37 | signal | 48 | 34 | 14 | 52 |  |
| 38 | unboundmethod | 46 | 24 | 22 | 78 |  |
| 39 | queue | 42 | 20 | 22 | 88 |  |
| 40 | symbol | 36 | 16 | 20 | 231 |  |
| 41 | mutex | 31 | 3 | 28 | 34 |  |
| 42 | warning | 27 | 18 | 9 | 27 |  |
| 43 | gc | 25 | 3 | 22 | 25 |  |
| 44 | refinement | 24 | 0 | 24 | 24 |  |
| 45 | math | 23 | 5 | 18 | 226 |  |
| 46 | class | 19 | 11 | 8 | 46 |  |
| 47 | binding | 15 | 1 | 14 | 15 |  |
| 48 | builtin_constants | 11 | 5 | 6 | 17 |  |
| 49 | conditionvariable | 11 | 1 | 10 | 11 |  |
| 50 | nil | 9 | 2 | 7 | 26 |  |
| 51 | threadgroup | 8 | 1 | 7 | 8 |  |
| 52 | main | 6 | 0 | 6 | 1 |  |
| 53 | basicobject | 3 | 0 | 3 | 178 |  |
| 54 | false | 3 | 1 | 2 | 12 |  |
| 55 | marshal | 3 | 0 | 3 | 20 |  |
| 56 | systemexit | 3 | 0 | 3 | 6 |  |
| 57 | true | 3 | 1 | 2 | 12 |  |
| 58 | comparable | 1 | 0 | 1 | 28 |  |

## Error Type Distribution

| Error Type | Count | Share |
|-----------|------:|------:|
| NoMethodError | 3836 | 57.4% |
| NameError | 832 | 12.4% |
| TypeError | 757 | 11.3% |
| ArgumentError | 416 | 6.2% |
| RuntimeError | 273 | 4.1% |
| RangeError | 195 | 2.9% |
| ThreadError | 74 | 1.1% |
| SyntaxError | 70 | 1.0% |
| LocalJumpError | 56 | 0.8% |
| IOError | 39 | 0.6% |
| LoadError | 37 | 0.6% |
| Encoding::Converter | 26 | 0.4% |
| NotImplementedError | 26 | 0.4% |
| FrozenError | 19 | 0.3% |
| Errno::ENOENT | 12 | 0.2% |

## Top 30 Missing Methods (NoMethodError)

| # | Method | Count |
|---|--------|------:|
| 1 | `raise` | 98 |
| 2 | `refine` | 83 |
| 3 | `step` | 77 |
| 4 | `parameters` | 52 |
| 5 | `convert` | 45 |
| 6 | `primitive_convert` | 44 |
| 7 | `spawn` | 41 |
| 8 | `not` | 39 |
| 9 | `wakeup` | 36 |
| 10 | `writable?` | 36 |
| 11 | `const_source_location` | 35 |
| 12 | `deconstruct_keys` | 34 |
| 13 | `readlines` | 33 |
| 14 | `define` | 30 |
| 15 | `kill` | 30 |
| 16 | `take` | 30 |
| 17 | `lock` | 29 |
| 18 | `putc` | 27 |
| 19 | `backtrace_locations` | 26 |
| 20 | `values_at` | 26 |
| 21 | `quo` | 24 |
| 22 | `String` | 24 |
| 23 | `byterindex` | 23 |
| 24 | `srand` | 23 |
| 25 | `truncate` | 21 |
| 26 | `pread` | 21 |
| 27 | `size` | 20 |
| 28 | `to_a` | 20 |
| 29 | `sysread` | 20 |
| 30 | `write` | 20 |

## Top 20 Missing Constants (NameError)

| # | Constant | Count |
|---|----------|------:|
| 1 | `Buffer` | 170 |
| 2 | `ARGF` | 129 |
| 3 | `ObjectSpace` | 95 |
| 4 | `TracePoint` | 75 |
| 5 | `Measure` | 49 |
| 6 | `Product` | 30 |
| 7 | `Chain` | 17 |
| 8 | `RLIMIT_CORE` | 15 |
| 9 | `UTF_7` | 13 |
| 10 | `ThreadGroup` | 10 |
| 11 | `ClosedQueueError` | 9 |
| 12 | `DataWithOverriddenInitialize` | 5 |
| 13 | `ArithmeticSequence` | 4 |
| 14 | `Sys` | 4 |
| 15 | `CS_CONSTX` | 3 |
| 16 | `ASCII` | 3 |
| 17 | `EWOULDBLOCK` | 3 |
| 18 | `UncaughtThrowError` | 3 |
| 19 | `Refinement` | 3 |
| 20 | `DefineSingletonMethodSpecClass` | 3 |

## Prioritized Improvement Candidates

| # | Fix | Impact | Diff | Score | Description |
|---|-----|-------:|-----:|------:|-------------|
| 1 | **IO::Buffer stub class** | 158 | ★☆☆☆☆ | 158 | Empty class → eliminates 158 NameErrors |
| 2 | **ObjectSpace stub module** | 95 | ★★☆☆☆ | 47 | Module with each_object, count_objects stubs |
| 3 | **ARGF stub object** | 129 | ★★★☆☆ | 43 | Global object reading from ARGV files or stdin |
| 4 | **Numeric#step / Float#step** | 77 | ★★☆☆☆ | 38 | Step iterator from self to limit |
| 5 | **Hash#deconstruct_keys** | 34 | ★☆☆☆☆ | 34 | Pattern matching: filter self by keys |
| 6 | **IO#putc** | 27 | ★☆☆☆☆ | 27 | Write single char |
| 7 | **TracePoint stub** | 75 | ★★★☆☆ | 25 | Class for event tracing |
| 8 | **Thread#raise** | 98 | ★★★★☆ | 24 | Raise in thread — single-threaded: raise in main |
| 9 | **Random.srand** | 23 | ★☆☆☆☆ | 23 | Set random seed |
| 10 | **Kernel#spawn / Process.spawn** | 41 | ★★☆☆☆ | 20 | Process spawning via fork+exec |
| 11 | **IO#binmode stub** | 19 | ★☆☆☆☆ | 19 | Binary mode flag stub |
| 12 | **Method#parameters** | 52 | ★★★☆☆ | 17 | Return parameter info array |
| 13 | **Module#refine** | 83 | ★★★★★ | 16 | Full refinement support — very complex |
| 14 | **IO#readlines / File.readlines** | 33 | ★★☆☆☆ | 16 | Read all lines into array |
| 15 | **String#byterindex** | 32 | ★★☆☆☆ | 16 | Reverse byte search |
| 16 | **Encoding::Converter#convert** | 45 | ★★★☆☆ | 15 | Encoding conversion methods |
| 17 | **Process::RLIMIT_* constants** | 15 | ★☆☆☆☆ | 15 | Add RLIMIT constants |
| 18 | **Numeric#quo / #fdiv** | 24 | ★★☆☆☆ | 12 | Rational/Float division |
| 19 | **Module#const_source_location** | 35 | ★★★☆☆ | 11 | Return file:line for constant |
| 20 | **Data.define** | 30 | ★★★☆☆ | 10 | Data class with define/members |
