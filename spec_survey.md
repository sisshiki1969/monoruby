# ruby/spec `core` Survey — monoruby

Branch: `claude/enumerator-size-master` (HEAD `7fc67ca3`).
Run date: 2026-04-19.
Reference Ruby: 4.0.2 (rbenv, used as the spec runtime's target validator).
Runner: `../mspec/bin/mspec <spec_files> -t monoruby --format dotted`,
timeout 180 s per category.
Known hangs excluded (`core/io/copy_stream_spec.rb`,
`core/io/select_spec.rb`, `core/kernel/readlines_spec.rb`).

## Overall totals

| metric | count |
|---|---:|
| total examples | **22 058** |
| passing       | **12 264** |
| failures      | 4 249 |
| errors        | 5 545 |
| **pass rate** | **55.6 %** |

## Per-category results (impact-sorted)

| category | files | ex | fail | err | F+E | note |
|---|---:|---:|---:|---:|---:|---|
| string | 141 | 4 216 | 986 | 625 | **1 611** | |
| io | 99 | 1 338 | 440 | 943 | **1 383** | |
| kernel | 117 | 2 289 | 486 | 496 | **982** | |
| file | 112 | 948 | 184 | 416 | **600** | |
| encoding | 45 | 592 | 379 | 180 | **559** | |
| marshal | 6 | 704 | 167 | 286 | **453** | |
| enumerator | 81 | 403 | 43 | 314 | **357** | |
| time | 66 | 525 | 232 | 100 | **332** | |
| process | 92 | 313 | 55 | 265 | **320** | |
| thread | 53 | 310 | 99 | 218 | **317** | |
| enumerable | 61 | 573 | 123 | 164 | **287** | |
| range | 33 | 466 | 81 | 183 | **264** | |
| array | 129 | 3 097 | 178 | 33 | **211** | |
| hash | 69 | 614 | 108 | 70 | **178** | |
| exception | 39 | 248 | 84 | 89 | **173** | |
| fiber | 13 | 173 | 24 | 126 | **150** | |
| argf | 34 | 142 | 21 | 84 | **105** | |
| objectspace | 29 | 112 | 5 | 97 | **102** | |
| proc | 23 | 206 | 48 | 46 | **94** | |
| struct | 30 | 173 | 33 | 92 | **125** | |
| matchdata | 30 | 189 | 20 | 76 | **96** | |
| method | 25 | 206 | 37 | 54 | **91** | |
| env | 45 | 239 | 101 | 14 | **115** | |
| dir | 34 | 337 | 64 | 55 | **119** | |
| sizedqueue | 16 | 129 | 33 | 45 | **78** | |
| filetest | 24 | 89 | 24 | 45 | **69** | |
| tracepoint | 19 | 75 | 1 | 75 | **76** | |
| data | 13 | 90 | 31 | 28 | **59** | |
| complex | 43 | 186 | 29 | 4 | **33** | |
| regexp | 24 | 107 | 29 | 48 | **77** | |
| queue | 15 | 88 | 20 | 22 | **42** | |
| gc | 18 | 34 | 3 | 32 | **35** | |
| signal | 3 | 52 | 31 | 15 | **46** | **crash** (SIGSEGV while running) |
| random | 10 | 87 | 10 | 45 | **55** | |
| refinement | 8 | 25 | 0 | 25 | **25** | |
| warning | 5 | 31 | 10 | 5 | **15** | |
| unboundmethod | 19 | 84 | 9 | 22 | **31** | |
| conditionvariable | 4 | 11 | 1 | 10 | **11** | |
| mutex | 7 | 34 | 3 | 28 | **31** | |
| class | 8 | 54 | 1 | 0 | **1** | |
| binding | 9 | 15 | 1 | 14 | **15** | |
| builtin_constants | 1 | 27 | 5 | 16 | **21** | |
| numeric | 46 | 273 | 6 | 3 | **9** | |
| integer | 68 | 615 | 0 | 2 | **2** | |
| threadgroup | 5 | 8 | 1 | 7 | **8** | |
| systemexit | 2 | 6 | 0 | 3 | **3** | |
| symbol | 29 | 330 | 3 | 1 | **4** | |
| main | 7 | 1 | 0 | 6 | **6** | 7 files, only 1 example parsed → 6 load errors |
| **module** | ? | ? | ? | ? | **?** | **`mspec` interrupted** (bad spec resets `@current` state) |

Clean (0F / 0E): `basicobject, comparable, false, float, math, nil, rational, true` — 8 categories.

## Irregular categories

- **`core/module`** → mspec's "`did not reset the current example`" guard
  fires in `core/module/define_method_spec.rb`, killing the whole run
  without producing a summary. Large blast radius — fixing this one
  spec would likely surface a few hundred newly-running tests.
- **`core/signal`** → emits SIGSEGV mid-run but continues enough to
  produce 31 F / 15 E. Stack trace likely originates from Signal.trap
  or a fiber-signal handler mismatch.

## Top missing features / methods (by occurrences)

### core/string (+1 611)

```
 23  #undump
 23  #byterindex
 19  #scrub
 12  #tr_s
 12  #grapheme_clusters
 12  #each_grapheme_cluster
 11  #unicode_normalized?
  8  #chr
```

### core/io (+1 383)

```
160  IO::Buffer (constant + class; absent entirely)
 30  #readlines
 25  .not (something calling `.not` on IO in a spec; likely mspec helper)
 21  #pread
 20  #write (some subtype of)
 18  #sysread, #binwrite
 17  #seek, #pos=, #closed?
```

### core/kernel (+982)

```
 30  #writable?   (likely the `test(?w, ...)` spec scaffolding)
 18  #catch
 16  #load_wrap_specs_top_level_method
  6  #set_scheduler
  5  #untrace_var, #singleton_method, #reopen
  4  #trace_var, #local_variables
```

### core/file (+600)

```
 21  File#truncate
 14  #not, #ftype
 13  #world_readable?
 12  #mkfifo, #chown
 10  #realdirpath, #link
```

### core/encoding (+559)

```
 45  Encoding::Converter#convert
 44  Encoding::Converter#primitive_convert
 10  #replacement
  9  UTF constant (maybe UTF_8_MAC?)
```

### core/marshal (+453)

```
  7  Marshal#_dump
  4  FIXEDENCODING
  … mostly specific encoding tags unrecognised (`tag: 0x64 'd'`) during parse
```

### core/enumerator (+357)

```
 30  Enumerator::Product   (class)
 19  #take (on an Enumerator)
 17  Enumerator::Chain     (class)
 13  #map, 12 #product, 12 #grep_v, 12 #grep
 11  #filter, 10 #with_index / #size / #collect, 9 #zip
```

### core/process (+320)

```
 90  Process.spawn
 86  RLIMIT_* constants
 64  Process.reopen (actually an IO method used in specs)
 18  #getpgid, #detach
 14  #exit, #exec
```

### core/thread (+317)

```
 86  Thread#raise
 36  Thread#wakeup
 26  Thread#backtrace_locations
 18  #thread_variable?, #pending_interrupt?, #fetch, #backtrace
 14  #run
```

### core/enumerable (+287)

```
 15  #cycle  (Enumerable#cycle — distinct from Array#cycle)
 11  #take, #find_index, #detect
 10  #grep_v
  7  #slice_before / #slice_after / #minmax_by
  6  #drop_while / #drop
  5  #each_entry
```

### core/range (+264)

```
 78  Range#step                (biggest single win)
 14  #reverse_each
  9  #overlap?
  8  #size (on some edge case)
```

### core/argf (+105)

```
  9  #read_nonblock, #closed?
  8  #readpartial
  6  #each_codepoint
  5  #each_char
```

### core/fiber (+150)

```
 47  Fiber#raise
 19  Fiber#transfer
 10  #set_scheduler
  8  #alive?, 6 #blocking?
```

## Suggested next steps (prioritized by ROI)

ROI here is rough "issues removed per line of code / per day". Items
marked **ROI:★★★** are close to one-liners once we know the class
shape; **★★** is a module-sized addition; **★** is a multi-file
feature.

### Tier 1 — near-linear gains (expect > 200 issues)

1. **`IO::Buffer` as a stub class** (io: **-160 errors**, ★★★).
   `Class.new` under `IO::` is enough to unblock specs that merely
   reference the constant. The ones that exercise behavior will
   convert to specific failures we can tackle later.
2. **`core/module` unblock** — fix or tag the one spec that trips
   `did not reset the current example`, then re-run to surface the
   full category (**likely several hundred now-running tests**). ★★
3. **`Range#step`** (range: **-78 errors**, ★★). Implement on top of
   the new `Numeric#step` — most call shapes overlap.
4. **Ruby-level `Enumerable` gaps** (enumerable: about **-75 errors**,
   ★★★). `cycle`, `take`, `find_index`, `detect` (alias of `find`),
   `grep_v`, `slice_before`, `slice_after`, `minmax_by`, `drop`,
   `drop_while`. All implementable in `builtins/enumerable.rb` with
   each/yield.
5. **`Enumerator::Chain` / `Enumerator::Product`** (enumerator:
   **-47 errors**, ★★). Register as subclasses of `Enumerator` (the
   monoruby limitation where `Enumerator.new` ignores the subclass
   receiver needs to be fixed first — see Tier 2).

### Tier 2 — infrastructure that multiplies the gains above

6. **Honor subclass receiver in `Enumerator.new`** (★★). Currently
   it always constructs a base `Enumerator`. Blocks several
   `Enumerator::Chain` / `::Product` / `::ArithmeticSequence` specs
   from passing at the `instance_of?` check.
7. **`to_enum` size hints on the remaining heavy users** (★★★). The
   new slot (`EnumeratorInner.size`) is in place; migrate each site
   away from method-name dispatch and attach a size block or
   explicit Integer. Candidates: `Array#each`, `Array#map`,
   `Array#each_index`, `Array#collect`, `Array#select`, `Hash#each`,
   `String#each_char`, `String#each_byte`, `String#each_line`.
   Each unlocks a "returned Enumerator size returns the enumerable
   size" spec across the hosting category.
8. **`Integer#upto` / `#downto` lazy-validation** (integer: **-2
   errors**, ★). Ruby wrapper was tried here but broke
   `Array#permutation`'s internal `(n-1).downto(0) { ... break ... }`
   via `&block` forwarding. Root-cause that forwarding bug first,
   then re-apply the Ruby wrapper cleanly.

### Tier 3 — per-category fills

9. **`core/string`** methods: `#undump`, `#byterindex`, `#scrub`,
   `#tr_s`, `#grapheme_clusters`, `#each_grapheme_cluster`,
   `#unicode_normalized?`, `#chr`. **-110 errors** for the top 8.
10. **`core/file`** methods: `#truncate` (libc wrapper), `#ftype`,
    `#world_readable?`, `#mkfifo`, `#chown`, `#realdirpath`, `#link`.
    **-92 errors**.
11. **`core/encoding` `Encoding::Converter`**: `#convert` and
    `#primitive_convert` alone account for 89 errors; likely maps to
    an `iconv`-ish wrapper around Onigmo's encoding support.
12. **`core/kernel` `#catch` / `#throw`** — 18 errors for one
    control-flow primitive. Needs a Fiber / label-return pair
    internally.
13. **`core/thread` `#raise` / `#wakeup`** — 86 / 36 errors. Requires
    real preemptive signalling, which monoruby does not have. Low
    ROI until a thread backend exists.

### Tier 4 — investigations

14. **`core/signal` SIGSEGV** — find the offending handler.
15. **`core/numeric` residual 6 F / 3 E**: `#quo` TypeError,
    `#remainder` coerce mock, `#singleton_method_added` error class,
    `Enumerator::ArithmeticSequence` load (blocked by #6).
16. **Deep JIT / VM**: the remaining `extern "C"` boundaries that can
    still abort the process (the panic-catch was landed in #328, but
    several direct callbacks like `vm_*` aren't wrapped yet).

## Methodology notes

- Per-category run used `timeout 180s` and `--format dotted`.
- Summaries parsed with `grep -aoE` from the category log
  (`/tmp/specrun/logs2/<cat>.log`).
- The "top errors" section uses
  `grep -aoE "NameError: uninitialized constant [A-Za-z:]+|NoMethodError: undefined method \`[A-Za-z_?!=]+'" core/<cat>/*.log | sort | uniq -c | sort -rn`
  so numbers are *distinct method names*, not example counts —
  a single missing method can be referenced by many spec examples.
