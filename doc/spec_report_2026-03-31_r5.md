# ruby/spec core Report — 2026-03-31

Build: `claude/fix-crash-bugs` (master + crash fixes + constants + missing methods + Integer#[] fix)

## Overall Summary

| Metric | Value | vs Previous |
|--------|------:|------------:|
| Categories | 57 | — |
| Examples | 17,317 | — |
| Expectations | 38,291 | — |
| **Failures** | **3,802** | — |
| **Errors** | **6,337** | — |
| **F+E** | **10,139** | **-156** |
| Pass rate | 73.5% | — |
| Crashes | 1 (IO.for_fd → fd panic) | 4→1 |

## Categories by F+E

| # | Category | F+E | Fail | Err | Examples | Note |
|---|----------|----:|-----:|----:|---------:|------|
| 1 | io | 1248 | 375 | 873 | 1222 | excl 5 crash/hang |
| 2 | kernel | 1160 | 543 | 617 | 2187 |  |
| 3 | array | 978 | 342 | 636 | 3011 |  |
| 4 | encoding | 564 | 377 | 187 | 588 |  |
| 5 | string | 553 | 267 | 286 | 1065 |  |
| 6 | file | 521 | 164 | 357 | 809 |  |
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
| 18 | argf | 148 | 14 | 134 | 144 |  |
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
| 48 | conditionvariable | 11 | 1 | 10 | 11 |  |
| 49 | nil | 9 | 2 | 7 | 26 |  |
| 50 | threadgroup | 8 | 1 | 7 | 8 |  |
| 51 | main | 6 | 0 | 6 | 1 |  |
| 52 | basicobject | 3 | 0 | 3 | 178 |  |
| 53 | false | 3 | 1 | 2 | 12 |  |
| 54 | marshal | 3 | 0 | 3 | 20 |  |
| 55 | systemexit | 3 | 0 | 3 | 6 |  |
| 56 | true | 3 | 1 | 2 | 12 |  |
| 57 | comparable | 1 | 0 | 1 | 28 |  |

## Error Type Distribution

| Error Type | Count | Share |
|-----------|------:|------:|
| NoMethodError | 3810 | 58.1% |
| NameError | 819 | 12.5% |
| TypeError | 682 | 10.4% |
| ArgumentError | 409 | 6.2% |
| RuntimeError | 269 | 4.1% |
| RangeError | 193 | 2.9% |
| ThreadError | 75 | 1.1% |
| SyntaxError | 70 | 1.1% |
| LocalJumpError | 56 | 0.9% |
| LoadError | 37 | 0.6% |
| IOError | 37 | 0.6% |
| Encoding::Converter | 26 | 0.4% |
| NotImplementedError | 26 | 0.4% |
| FrozenError | 19 | 0.3% |
| Errno::ENOENT | 14 | 0.2% |

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
| 8 | `wakeup` | 36 |
| 9 | `writable?` | 36 |
| 10 | `const_source_location` | 35 |
| 11 | `deconstruct_keys` | 34 |
| 12 | `readlines` | 33 |
| 13 | `byterindex` | 32 |
| 14 | `define` | 30 |
| 15 | `take` | 30 |
| 16 | `lock` | 29 |
| 17 | `kill` | 29 |
| 18 | `putc` | 27 |
| 19 | `backtrace_locations` | 26 |
| 20 | `values_at` | 26 |
| 21 | `not` | 25 |
| 22 | `quo` | 24 |
| 23 | `String` | 24 |
| 24 | `srand` | 23 |
| 25 | `pread` | 21 |
| 26 | `truncate` | 21 |
| 27 | `size` | 20 |
| 28 | `to_a` | 20 |
| 29 | `sysread` | 20 |
| 30 | `write` | 20 |

## Top 20 Missing Constants (NameError)

| # | Constant | Count |
|---|----------|------:|
| 1 | `Buffer` | 158 |
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

## Prioritized Improvement Candidates (impact ÷ difficulty)

| # | Fix | Impact | Diff | Score | Description |
|---|-----|-------:|-----:|------:|-------------|
| 1 | **IO::Buffer stub class** | 158 | ★☆☆☆☆ | 158 | Empty class with new/allocate → eliminates 158 NameErrors |
| 2 | **IO.for_fd fd validation** | 58 | ★☆☆☆☆ | 58 | Add fd validation to IO.new (same fix as File.open) |
| 3 | **ObjectSpace stub module** | 95 | ★★☆☆☆ | 48 | Module with each_object, count_objects stubs |
| 4 | **ARGF stub object** | 122 | ★★★☆☆ | 41 | Global object reading from ARGV files or stdin |
| 5 | **Numeric#step / Float#step** | 77 | ★★☆☆☆ | 38 | Step iterator from self to limit |
| 6 | **Hash#deconstruct_keys** | 34 | ★☆☆☆☆ | 34 | Pattern matching: filter self by keys |
| 7 | **IO#putc (already in builtins)** | 27 | ★☆☆☆☆ | 27 | Should already work — verify |
| 8 | **TracePoint stub** | 75 | ★★★☆☆ | 25 | Class for event tracing |
| 9 | **Thread#raise** | 98 | ★★★★☆ | 24 | Raise exception in thread (single-threaded: raise in main) |
| 10 | **Random.srand** | 23 | ★☆☆☆☆ | 23 | Set random seed |
| 11 | **Kernel#spawn** | 41 | ★★☆☆☆ | 20 | Process spawning via fork+exec |
| 12 | **IO#binmode** | 19 | ★☆☆☆☆ | 19 | Binary mode flag stub |
| 13 | **Method#parameters** | 52 | ★★★☆☆ | 17 | Return parameter info array |
| 14 | **Module#refine** | 83 | ★★★★★ | 17 | Full refinement support — very complex |
| 15 | **IO#readlines / File.readlines** | 33 | ★★☆☆☆ | 16 | Read all lines into array |
| 16 | **String#byterindex** | 32 | ★★☆☆☆ | 16 | Reverse byte search |
| 17 | **Encoding::Converter#convert** | 45 | ★★★☆☆ | 15 | Encoding conversion methods |
| 18 | **Enumerator#take / Lazy#take** | 30 | ★★☆☆☆ | 15 | Take first N from enumerator |
| 19 | **Process::RLIMIT_* constants** | 15 | ★☆☆☆☆ | 15 | Add RLIMIT constants |
| 20 | **File#lock / File.flock** | 29 | ★★☆☆☆ | 14 | File locking |
| 21 | **Numeric#quo / #fdiv** | 24 | ★★☆☆☆ | 12 | Rational/Float division |
| 22 | **Module#const_source_location** | 35 | ★★★☆☆ | 12 | Return file:line for constant |
| 23 | **IO#truncate / File.truncate** | 21 | ★★☆☆☆ | 10 | Truncate file to size |
| 24 | **IO#pread / IO#sysread** | 21 | ★★☆☆☆ | 10 | Low-level positioned/unbuffered read |
| 25 | **Data.define** | 30 | ★★★☆☆ | 10 | Data class with define/members |
