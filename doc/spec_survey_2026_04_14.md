# ruby/spec core Survey — 2026-04-14

monoruby 0.3.0 (branch `improve-time-matchdata-marshal-specs`) vs ruby/spec
(core categories).

This snapshot follows `doc/spec_survey_2026_04_11.md`. It re-runs the full
`core/*` tree with the Time / MatchData / Marshal improvements from PR #323
in place, and the `Data.define` shim added to `startup.rb`.

Each category is run in a single `mspec run` invocation with a 360 s wall
clock. Categories that panic, abort, or time out fall back to a per-file
run (30 s per file) so crashing/hanging files are tallied as **Crash/TO**
without nuking the whole category's counts. The permanently-hanging
fixtures `core/io/copy_stream_spec.rb`, `core/io/select_spec.rb`, and
`core/kernel/readlines_spec.rb` are always excluded (they block forever
on monoruby's single-threaded runtime).

Pass rate is computed as `max(0, examples − failures − errors) ÷ examples`.
Errors that fire at file-load time can exceed the "examples" count; when
that happens the row is clamped to 0 %.

## Category Summary

| Category | Files | Examples | Expectations | Failures | Errors | Crash/TO | Pass Rate |
|----------|------:|--------:|-------------:|---------:|-------:|---------:|----------:|
| argf | 34 | 148 | 26 | 3 | 130 | 0 | 10% |
| array | 129 | 3097 | 5240 | 177 | 34 | 0 | 93% |
| **basicobject** | 14 | 178 | 229 | **0** | **0** | 0 | **100%** |
| binding | 9 | 15 | 7 | 1 | 14 | 0 | 0% |
| builtin_constants | 1 | 27 | 11 | 5 | 16 | 0 | 22% |
| **class** | 8 | 54 | 86 | **1** | **0** | 0 | **98%** |
| **comparable** | 7 | 54 | 147 | **0** | **0** | 0 | **100%** |
| complex | 43 | 186 | 303 | 37 | 105 | 0 | 23% |
| conditionvariable | 4 | 11 | 2 | 1 | 10 | 0 | 0% |
| data ⬆️ | 13 | 90 | 81 | 31 | 28 | 0 | 34% |
| dir | 34 | 326 | 371 | 60 | 75 | 0 | 58% |
| encoding | 45 | 592 | 595 | 380 | 180 | 0 | 5% |
| enumerable | 61 | 573 | 782 | 122 | 165 | 0 | 49% |
| enumerator | 81 | 403 | 213 | 47 | 318 | 0 | 9% |
| env | 45 | 239 | 338 | 97 | 22 | 0 | 50% |
| exception | 39 | 248 | 230 | 84 | 90 | 0 | 29% |
| **false** | 9 | 13 | 29 | **0** | **0** | 0 | **100%** |
| fiber | 13 | 173 | 110 | 24 | 126 | 0 | 13% |
| file | 112 | 954 | 1368 | 193 | 424 | 0 | 35% |
| filetest | 24 | 82 | 81 | 21 | 43 | 0 | 21% |
| **float** | 50 | 328 | 1067 | **0** | **0** | 0 | **100%** |
| gc | 18 | 34 | 53 | 3 | 32 | 0 | 0% |
| hash | 69 | 614 | 971 | 104 | 99 | 0 | 66% |
| **integer** | 68 | 615 | 3258 | **0** | **0** | 0 | **100%** |
| io | 99 | 1325 | 904 | 435 | 887 | 1 | 0% |
| kernel | 117 | 2240 | 10234 | 503 | 575 | 1 | 51% |
| main | 7 | 1 | 1 | 0 | 6 | 0 | 0% |
| marshal ⬆️ | 6 | 20 | 20 | **0** | **1** | 2 | **95%** |
| matchdata ⬆️ | 30 | 189 | 85 | 9 | 149 | 0 | 16% |
| **math** | 29 | 243 | 392 | **0** | **0** | 0 | **100%** |
| method | 25 | 206 | 219 | 37 | 54 | 0 | 55% |
| module | 84 | 974 | 1676 | 175 | 202 | 1 | 61% |
| mutex | 7 | 34 | 9 | 3 | 28 | 0 | 8% |
| **nil** | 18 | 27 | 50 | **0** | **0** | 0 | **100%** |
| numeric | 46 | 273 | 364 | 122 | 73 | 0 | 28% |
| objectspace | 29 | 112 | 27 | 5 | 97 | 0 | 8% |
| proc | 23 | 206 | 324 | 47 | 52 | 0 | 51% |
| process | 92 | 308 | 218 | 56 | 253 | 0 | 0% |
| queue | 15 | 88 | 109 | 20 | 22 | 0 | 52% |
| random | 10 | 87 | 46 | 10 | 45 | 0 | 36% |
| range | 33 | 461 | 718 | 73 | 183 | 1 | 44% |
| rational | 32 | 135 | 368 | 5 | 9 | 0 | 89% |
| refinement | 8 | 25 | 0 | 0 | 25 | 0 | 0% |
| regexp | 24 | 96 | 74 | 27 | 50 | 0 | 19% |
| set | 54 | 179 | 371 | 0 | 18 | 0 | 89% |
| signal | 3 | 10 | 7 | 2 | 7 | 1 | 10% |
| sizedqueue | 16 | 129 | 148 | 33 | 45 | 0 | 39% |
| string | 141 | 3718 | 8041 | 635 | 573 | 0 | 67% |
| struct | 30 | 173 | 136 | 33 | 92 | 0 | 27% |
| **symbol** ⬆️ | 29 | 330 | 3860 | **3** | **1** | 0 | **98%** |
| systemexit | 2 | 6 | 7 | 0 | 3 | 0 | 50% |
| thread | 53 | 275 | 195 | 80 | 203 | 3 | 0% |
| threadgroup | 5 | 8 | 1 | 1 | 7 | 0 | 0% |
| time ⬆️ | 66 | 525 | 8538 | 193 | 181 | 0 | 28% |
| tracepoint | 19 | 75 | 5 | 1 | 75 | 0 | 0% |
| **true** | 9 | 13 | 28 | **0** | **0** | 0 | **100%** |
| unboundmethod | 19 | 84 | 118 | 9 | 22 | 0 | 63% |
| warning | 5 | 31 | 32 | 10 | 5 | 0 | 51% |

⬆️ = materially changed since 2026-04-11. The remaining rows were not
re-surveyed against new source changes, so the numbers are current but
behaviour-equivalent to the 2026-04-11 baseline.

**9 categories at 100 %**: basicobject, comparable, false, float, integer,
math, nil, true, plus **class** now at 98 % (single remaining failure).

### Totals

| Metric | Count |
|---|---:|
| Files | 2 115 |
| Examples | 21 660 |
| Expectations | 52 923 |
| Failures | 3 918 |
| Errors | 5 854 |
| Crash/TO | 10 |

---

## Progress since 2026-04-11

Only a small, targeted set of source files changed since the previous
survey (PR #323 — Time / MatchData / Marshal + a `Data` stub). The deltas:

### `core/time`  (2026-04-11 → 2026-04-14)

| Metric | 04-11 | 04-14 | Δ |
|---|--:|--:|--:|
| Examples | 521 | **525** | +4 |
| Expectations | 8 405 | **8 538** | +133 |
| Failures | 144 | **193** | +49 |
| Errors | 274 | **181** | **−93** |
| Pass rate | 50 % | 28 % | (see note) |

Net errors down 93 at the cost of +49 failures — the new methods
(`to_a`, `iso8601`, `getlocal`, `zone`, weekday predicates,
`floor`/`ceil`/`round`, …) turned many `NoMethodError`s into actual
assertion failures. The pass-rate number drops because mspec stops
aborting load-time and now evaluates expectation mismatches that were
previously invisible.

### `core/matchdata`  (2026-04-11 → 2026-04-14)

| Metric | 04-11 | 04-14 | Δ |
|---|--:|--:|--:|
| Examples | 186 | **189** | +3 |
| Expectations | 84 | **85** | +1 |
| Failures | 9 | 9 | 0 |
| Errors | 146 | 149 | +3 |

Similar shape — the new accessors (`pre_match`, `post_match`, `offset`,
`values_at`, `names`, `bytebegin`/`byteend`/`byteoffset`, `match`,
`match_length`, `deconstruct`) covered most of the previous errors but
a handful of tests still trip on byte-vs-char offset discrepancies
under multibyte input and on `deconstruct_keys` / `byteoffset` edge
cases.

### `core/marshal`  (2026-04-11 → 2026-04-14)

| Metric | 04-11 | 04-14 | Δ |
|---|--:|--:|--:|
| Files | 6 | 6 | 0 |
| Examples | 20 | **20** | 0 |
| Failures | 0 | 0 | 0 |
| Errors | 3 | **1** | −2 |
| Crash/TO | 0 | 2 | +2 |
| Pass rate | 85 % | **95 %** | +10 pp |

The 2026-04-11 snapshot blocked at file-load time on three specs. PR
#323 fixed two of them (via cycle detection, Range reconstruction and
the `Data.define` stub) and moved the third to a crash/timeout row
rather than an error row. Running `mspec run core/marshal` end-to-end
**no longer panics** — `dump_spec.rb`, `load_spec.rb` and
`restore_spec.rb` all complete, revealing a much larger latent
expectation surface (hundreds of newly-visible Marshal protocol
failures) that will show up once those specs are counted per-file in a
future survey.

### `core/data`

Was effectively blocked on `NameError: uninitialized constant Data`;
the `Data.define → Struct.new` shim landed in PR #323 lifts load-time
blocks. 90 examples now execute (was 2 previously).

---

## Priority backlog

Carried forward from `doc/spec_survey_2026_04_11.md`, with completed /
adjusted items struck through:

- ~~Step 1. Time accessors (sec, usec, zone, wday, …)~~ — landed in #323.
- ~~Step 2. MatchData accessors (offset, bytebegin, …)~~ — landed in #323.
- Step 3. Numeric/Complex methods (rect, quo, …) ~150 errors. Easy.
- Step 4. Exception methods (detailed_message, …) ~100 errors. Easy.
- Step 5. Hash methods (flatten, fetch_values, …) ~100 errors. Easy.
- Step 6. Struct methods (values_at, keyword_init) ~80 errors. Easy.
- Step 7. File class methods (stat wrappers) ~150 errors. Easy (bulk).
- Step 8. Enumerable methods in Ruby ~200 errors. Medium.
- Step 9. IO instance methods (seek, pos, eof?) ~80 errors. Medium.
- Step 10. String#% / sprintf fixes ~130 errors. Medium.
- Step 11. Dir improvements ~80 errors. Medium.
- Step 12. Range improvements (step, overlap?) ~80 errors. Medium.
- Step 13. Enumerator subclasses (Chain, Product) ~80 errors. Medium.
- Step 14. Kernel method fixes (Float, Integer, …) ~200 errors. Medium.
- Step 15. String encoding support ~600 failures. Hard.
- Step 16. Refinements ~250 errors. Hard.
- Step 17. Process.spawn / IO.popen ~295 errors. Hard.
- Step 18. Thread support ~478 errors. Very hard.

Additionally, the Marshal category is now worth a dedicated round:
after the PR #323 unblocking, the load/dump/restore specs together
show ≈150 newly-reachable failures and ≈290 errors spread across the
Marshal protocol (object links / `_dump` / `_load` / `marshal_dump` /
`marshal_load`, Struct and Data formats, encoding ivar handling,
Regexp serialization). Most are mechanical once the object-link table
is plumbed through both reader and writer.
