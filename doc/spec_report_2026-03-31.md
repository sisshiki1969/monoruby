# ruby/spec core Report — 2026-03-31

Build: master @ `5f41566` (includes block.call fix)

## Overall Summary

| Metric | Value |
|--------|------:|
| Categories | 58 |
| Examples | 17,327 |
| Expectations | 37,894 |
| Failures | 3,674 |
| Errors | 6,626 |
| **F+E** | **10,300** |
| Pass rate | **72.8%** |
| Crashes | 3 (array init, file open fd, string byteindex) |
| Hangs | 5 (io: copy_stream/popen/select/new, kernel: readlines) |

Excluded specs:
- Crash: `array/initialize_spec`, `array/new_spec`, `file/new_spec`, `file/open_spec`, `string/byteindex_spec`
- Hang/timeout: `io/copy_stream_spec`, `io/popen_spec`, `io/select_spec`, `io/new_spec`, `kernel/readlines_spec`

## Top 25 Categories by F+E

| # | Category | F+E | Fail | Err | Examples |
|---|----------|----:|-----:|----:|---------:|
| 1 | io | 1,321 | 387 | 934 | 1,279 |
| 2 | kernel | 1,120 | 520 | 600 | 2,169 |
| 3 | array | 981 | 339 | 642 | 3,011 |
| 4 | string | 575 | 244 | 331 | 1,065 |
| 5 | encoding | 562 | 348 | 214 | 550 |
| 6 | file | 551 | 146 | 405 | 808 |
| 7 | enumerator | 366 | 46 | 320 | 403 |
| 8 | process | 325 | 75 | 250 | 297 |
| 9 | thread | 297 | 95 | 202 | 285 |
| 10 | enumerable | 289 | 128 | 161 | 547 |
| 11 | time | 287 | 84 | 203 | 308 |
| 12 | module | 260 | 65 | 195 | 275 |
| 13 | range | 240 | 71 | 169 | 435 |
| 14 | hash | 216 | 110 | 106 | 597 |
| 15 | numeric | 198 | 121 | 77 | 273 |
| 16 | integer | 182 | 113 | 69 | 576 |
| 17 | exception | 179 | 89 | 90 | 246 |
| 18 | argf | 148 | 14 | 134 | 137 |
| 19 | method | 144 | 40 | 104 | 200 |
| 20 | complex | 142 | 35 | 107 | 181 |
| 21 | sizedqueue | 129 | 0 | 129 | 129 |
| 22 | struct | 125 | 30 | 95 | 173 |
| 23 | env | 124 | 102 | 22 | 239 |
| 24 | dir | 122 | 61 | 61 | 336 |
| 25 | fiber | 119 | 24 | 95 | 140 |

## Error Type Breakdown

| Error Type | Count | Share |
|-----------|------:|------:|
| NoMethodError | ~5,036 | 57.4% |
| NameError | ~1,309 | 14.9% |
| TypeError | ~927 | 10.6% |
| ArgumentError | ~559 | 6.4% |
| RuntimeError | ~344 | 3.9% |
| RangeError | ~202 | 2.3% |
| SyntaxError | ~77 | 0.9% |
| IOError | ~73 | 0.8% |
| LocalJumpError | ~61 | 0.7% |
| LoadError | ~49 | 0.6% |
| ThreadError | ~46 | 0.5% |
| FrozenError | ~28 | 0.3% |
| NotImplementedError | ~26 | 0.3% |

## Crash Bugs

1. **array::initialize** — `capacity_overflow` panic when `Array.new` is called with huge size argument. Fix: add size guard before allocation.
2. **file::open** — `fd != -1` panic in `IoInner::from_raw_fd` when `File.open` receives invalid fd (-1). Fix: validate fd before creating IoInner.
3. **string::byteindex** — char boundary panic in onigmo captures with multibyte strings (`わ`). Fix: use byte-safe string slicing in `save_capture_special_variables`.

## Top 30 Missing Constants (NameError)

| # | Constant | Count | Fix |
|---|----------|------:|-----|
| 1 | Encoding::Converter | 171 | Stub class |
| 2 | IO::Buffer | 158 | Stub class |
| 3 | SizedQueue | 129 | Expose Thread::Queue at top-level |
| 4 | ARGF | 122 | Stub object |
| 5 | ObjectSpace | 95 | Stub module |
| 6 | TracePoint | 75 | Stub class |
| 7 | Encoding::CompatibilityError | 71 | Stub exception class |
| 8 | Measure | 49 | Benchmark::Measure stub |
| 9 | File::FNM_PATHNAME | 48 | Add constant |
| 10 | Enumerator::Product | 30 | Stub class |
| 11 | Encoding::UTF_7 | 19 | Add encoding |
| 12 | File::FNM_EXTGLOB | 17 | Add constant |
| 13 | FloatDomainError | 17 | Stub exception class |
| 14 | Enumerator::Chain | 17 | Stub class |
| 15 | Process::RLIMIT_CORE | 15 | Add constants |
| 16 | ConditionVariable | 11 | Stub class |
| 17 | ThreadGroup | 10 | Stub class |
| 18 | GC::Profiler | 10 | Stub module |
| 19 | IO::SEEK_CUR | 8 | Add constant |
| 20 | File::FNM_NOESCAPE | 7 | Add constant |
| 21 | IO::SEEK_END | 6 | Add constant |
| 22 | TOPLEVEL_BINDING | 5 | Add constant |
| 23 | IO::SEEK_SET | 5 | Add constant |
| 24 | Enumerator::ArithmeticSequence | 4 | Stub class |

## Top 30 Missing Methods (NoMethodError)

| # | Method | Count | Likely Class |
|---|--------|------:|-------------|
| 1 | for_fd | 116 | IO |
| 2 | raise | 98 | Thread |
| 3 | step | 95 | Float/Numeric |
| 4 | readlines | 91 | IO/File |
| 5 | refine | 83 | Module |
| 6 | byterindex | 62 | String |
| 7 | parameters | 52 | Method/UnboundMethod |
| 8 | each_codepoint | 50 | String |
| 9 | not | 50 | Regexp |
| 10 | values_at | 44 | Array |
| 11 | truncate | 44 | IO/File |
| 12 | pread | 42 | IO |
| 13 | spawn | 41 | Kernel/Process |
| 14 | writable? | 40 | File/FileTest |
| 15 | sysread | 40 | IO |
| 16 | write | 40 | IO/File |
| 17 | zero? | 39 | File/FileTest |
| 18 | binmode | 38 | IO |
| 19 | wakeup | 36 | Thread |
| 20 | putc | 36 | IO/Kernel |
| 21 | const_source_location | 35 | Module |
| 22 | binwrite | 35 | IO/File |
| 23 | codepoints | 34 | String |
| 24 | deconstruct_keys | 34 | Hash/Struct/MatchData |
| 25 | pos= | 34 | IO |
| 26 | define | 30 | Data |
| 27 | take | 30 | Enumerator/Fiber |
| 28 | write_nonblock | 30 | IO |
| 29 | lock | 29 | Mutex/File |
| 30 | kill | 29 | Process/Thread |

## Prioritized Improvement Candidates

### Tier 1: Crash Bug Fixes (Low difficulty, high urgency)
- array::initialize capacity overflow
- file::open invalid fd
- string::byteindex multibyte char boundary

### Tier 2: Easy Constants (~500 errors, trivial stubs)
- File::FNM_PATHNAME/FNM_EXTGLOB/FNM_NOESCAPE
- IO::SEEK_SET/SEEK_CUR/SEEK_END
- FloatDomainError exception class
- Encoding::CompatibilityError exception class
- TOPLEVEL_BINDING

### Tier 3: Easy Method Stubs (~400 errors)
- IO.for_fd (alias for IO.new)
- File.zero? / FileTest.zero?
- IO#binmode (no-op stub)
- IO#putc
- Array#values_at
- Array#intersection
- Hash#fetch_values

### Tier 4: Medium Methods (~500 errors)
- String#codepoints / String#each_codepoint
- String#byterindex
- Float#step / Numeric#step
- Hash#deconstruct_keys
- Method#parameters
- Module#const_source_location
- IO#readlines / File.readlines

### Tier 5: Stub Classes (~500 errors)
- Encoding::Converter (stub)
- IO::Buffer (stub)
- ObjectSpace (stub module)
- TracePoint (stub class)
- Data.define

## Progress History

| Date | PR(s) | F+E | Delta |
|------|-------|----:|------:|
| 2026-02-09 | baseline | ~14,000 | — |
| 2026-03-28 | #244–#248 | 13,170 | -830 |
| 2026-03-28 | #249 | 12,017 | -1,153 |
| 2026-03-29 | #250 | 11,956 | -61 |
| 2026-03-29 | #251 | 10,785 | -1,171 |
| 2026-03-29 | #252 | 10,203 | -582 |
| 2026-03-31 | master+block.call fix | 10,300 | — |
