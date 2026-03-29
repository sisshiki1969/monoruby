# ruby/spec core Report — 2026-03-29

Branch: `claude/spec-v3-pr` (PR #252)
Base: master (commit `973e2b5`, includes PRs #244–#251)

## Overall Summary

| Metric | Master (#251) | PR #252 | Delta |
|--------|:------------:|:-------:|:-----:|
| Categories | 58/58 ok | 58/58 ok | — |
| Examples | 17,123 | 16,398 | -725 |
| Expectations | 37,335 | 36,479 | -856 |
| **Failures** | **3,240** | **3,071** | **-169** |
| **Errors** | **7,545** | **7,132** | **-413** |
| **F+E** | **10,785** | **10,203** | **-582** |
| Pass rate | 91.3% | 91.6% | +0.3pp |
| Crashes | 0 | 0 | — |
| Panics | 0 | 0 | — |
| SEGV | 0 | 0 | — |

Excluded spec files (hanging): `io/copy_stream_spec.rb`, `io/popen_spec.rb`, `io/select_spec.rb`, `kernel/readlines_spec.rb`

## Worst 15 Categories

| Category | F+E | Failures | Errors |
|----------|----:|--------:|---------:|
| io | 1,382 | 338 | 1,044 |
| kernel | 921 | 300 | 621 |
| string | 845 | 337 | 508 |
| file | 668 | 145 | 523 |
| encoding | 562 | 347 | 215 |
| array | 523 | 113 | 410 |
| enumerator | 371 | 41 | 330 |
| enumerable | 322 | 105 | 217 |
| thread | 316 | 98 | 218 |
| process | 299 | 44 | 255 |
| time | 297 | 84 | 213 |
| range | 282 | 79 | 203 |
| module | 257 | 61 | 196 |
| hash | 213 | 80 | 133 |
| numeric | 198 | 116 | 82 |

## Error Type Breakdown

| Error Type | Count |
|-----------|------:|
| NoMethodError | ~4,090 |
| NameError | ~1,165 |
| ArgumentError | ~774 |
| TypeError | ~673 |
| RuntimeError | ~469 |
| LoadError | ~207 |
| Errno::ENOTDIR | 119 |
| SyntaxError | 63 |
| RangeError | 59 |
| LocalJumpError | 54 |
| ThreadError | 45 |
| IOError | 37 |

## Changes in PR #252

### 1. Replace coerce_to_i64 with coerce_to_int
- Array index/range operations now call `to_int` on non-integer objects
- Affects `Integer#step`, `Integer#[]`, `File.chmod`, `Dir.mkdir`

### 2. Fix load/require path resolution
- Absolute paths loaded directly (no `$LOAD_PATH` search)
- Use `is_file()` instead of `exists()` to avoid loading directories
- `canonicalize()` failure no longer causes LoadError
- Error message matches CRuby format

### 3. Implement pack/unpack p/P and sprintf %a/%A
- pack("p")/unpack("p"): null-terminated string pointer
- pack("P")/unpack("P"): fixed-length pointer
- sprintf %a/%A: hexadecimal floating-point notation

## Progress History

| Date | PR(s) | F+E | Delta |
|------|-------|----:|------:|
| 2026-02-09 | baseline | ~14,000 | — |
| 2026-03-28 | #244–#248 | 13,170 | -830 |
| 2026-03-28 | #249 | 12,017 | -1,153 |
| 2026-03-29 | #250 | 11,956 | -61 |
| 2026-03-29 | #251 | 10,785 | -1,171 |
| 2026-03-29 | #252 | 10,203 | -582 |
| **Total** | | | **-3,797** |
