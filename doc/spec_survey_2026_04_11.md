# ruby/spec core Survey — 2026-04-11

monoruby 0.3.0 (branch `spec-improvements-core-class-module`) vs ruby/spec (core categories).

This file is a re-survey snapshot showing the current state of `core/class`
and `core/module` after the work tracked in `doc/spec_survey_2026_04_08.md`.
The numbers for **class** and **module** were re-collected on 2026-04-11; all
other categories carry over the 2026-04-08 baseline (those source files were
not touched).

Each spec file is run individually with a 30s timeout. Excluded from runs:
`copy_stream_spec`, `select_spec`, `readlines_spec` (hang under
single-threaded runtime). One file (`core/module/define_method_spec.rb`)
crashes the mspec runner partway through and is reported as
1 file × 0 examples.

## Category Summary

| Category | Files | Examples | Expectations | Failures | Errors | Crash/TO | Pass Rate |
|----------|------:|--------:|-------------:|---------:|-------:|---------:|----------:|
| argf | 34 | 148 | 26 | 3 | 130 | 0 | 10% |
| array | 129 | 2985 | 4935 | 133 | 206 | 0 | 89% |
| **basicobject** | 14 | 178 | 229 | **0** | **0** | 0 | **100%** |
| binding | 9 | 15 | 7 | 1 | 14 | 0 | 7% |
| builtin_constants | 1 | 27 | 11 | 5 | 16 | 0 | 22% |
| **class** ⬆️ | 8 | **54** | **84** | **6** | **1** | 0 | **87%** |
| **comparable** | 7 | 54 | 147 | **0** | **0** | 0 | **100%** |
| complex | 43 | 186 | 293 | 38 | 112 | 0 | 19% |
| conditionvariable | 4 | 11 | 2 | 1 | 10 | 0 | 9% |
| data | 13 | 2 | 1 | 0 | 14 | 0 | 0% |
| dir | 34 | 320 | 367 | 60 | 69 | 0 | 60% |
| encoding | 45 | 589 | 585 | 376 | 187 | 0 | 4% |
| enumerable | 61 | 573 | 759 | 126 | 185 | 0 | 46% |
| enumerator | 81 | 403 | 213 | 46 | 319 | 0 | 10% |
| env | 45 | 239 | 980 | 101 | 18 | 0 | 50% |
| exception | 39 | 246 | 227 | 80 | 93 | 0 | 30% |
| **false** | 9 | 13 | 29 | **0** | **0** | 0 | **100%** |
| fiber | 13 | 163 | 108 | 24 | 118 | 0 | 13% |
| file | 112 | 904 | 1268 | 151 | 438 | 0 | 35% |
| filetest | 24 | 82 | 79 | 13 | 45 | 0 | 29% |
| **float** | 50 | 328 | 1067 | **0** | **0** | 0 | **100%** |
| gc | 18 | 34 | 53 | 3 | 32 | 0 | 6% |
| hash | 69 | 606 | 960 | 100 | 103 | 0 | 67% |
| **integer** | 67 | 603 | 3238 | **0** | **0** | 0 | **100%** |
| io | 98 | 1326 | 901 | 435 | 888 | 1 | 0% |
| kernel | 116 | 2233 | 10147 | 497 | 644 | 1 | 49% |
| main | 7 | 1 | 1 | 0 | 6 | 0 | 0% |
| marshal | 6 | 20 | 20 | 0 | 3 | 0 | 85% |
| matchdata | 29 | 186 | 84 | 9 | 146 | 0 | 17% |
| **math** | 29 | 243 | 392 | **0** | **0** | 0 | **100%** |
| method | 25 | 206 | 165 | 41 | 104 | 0 | 30% |
| **module** ⬆️ | 84 | **974** | **1662** | **178** | **209** | 0 | **60%** |
| mutex | 7 | 34 | 9 | 3 | 28 | 0 | 9% |
| **nil** | 18 | 27 | 50 | **0** | **0** | 0 | **100%** |
| numeric | 46 | 273 | 364 | 122 | 73 | 0 | 28% |
| objectspace | 29 | 112 | 27 | 5 | 97 | 0 | 9% |
| proc | 23 | 206 | 321 | 47 | 53 | 0 | 52% |
| process | 92 | 298 | 175 | 43 | 252 | 0 | 1% |
| queue | 15 | 88 | 109 | 20 | 22 | 0 | 52% |
| random | 10 | 87 | 46 | 10 | 45 | 0 | 37% |
| range | 33 | 459 | 711 | 70 | 188 | 0 | 44% |
| rational | 32 | 135 | 343 | 11 | 12 | 0 | 83% |
| refinement | 8 | 25 | 0 | 0 | 25 | 0 | 0% |
| regexp | 24 | 99 | 75 | 31 | 53 | 0 | 15% |
| set | 54 | 162 | 319 | 19 | 6 | 0 | 85% |
| signal | 3 | 52 | 44 | 31 | 14 | 0 | 13% |
| sizedqueue | 16 | 129 | 148 | 33 | 45 | 0 | 40% |
| string | 141 | 3718 | 7907 | 646 | 623 | 0 | 66% |
| struct | 30 | 173 | 135 | 31 | 93 | 0 | 28% |
| symbol | 29 | 235 | 3613 | 11 | 18 | 0 | 88% |
| systemexit | 2 | 6 | 7 | 0 | 3 | 0 | 50% |
| thread | 53 | 310 | 216 | 99 | 217 | 0 | 0% |
| threadgroup | 5 | 8 | 1 | 1 | 7 | 0 | 0% |
| time | 66 | 521 | 8405 | 144 | 274 | 0 | 50% |
| tracepoint | 19 | 75 | 5 | 1 | 75 | 0 | 0% |
| **true** | 9 | 13 | 28 | **0** | **0** | 0 | **100%** |
| unboundmethod | 19 | 84 | 90 | 25 | 22 | 0 | 44% |
| warning | 5 | 31 | 25 | 10 | 12 | 0 | 29% |

⬆️ = re-surveyed on 2026-04-11. Other rows are unchanged from the 2026-04-08
baseline (those source files were not modified).

**8 categories at 100%**: basicobject, comparable, false, float, integer,
math, nil, true.

---

## Progress on `core/class` since 2026-04-08

| Metric | 2026-04-08 | 2026-04-11 | Δ |
|---|--:|--:|--:|
| Examples | 46 | **54** | +8 |
| Expectations | 66 | **84** | +18 |
| Failures | 11 | **6** | **−5** |
| Errors | 8 | **1** | **−7** |
| Pass rate | 59% | **87%** | +28pp |

### Implemented (`core/class`)

- **`Class#attached_object`** — returns the attached object of a singleton
  class, or raises `TypeError` for non-singleton classes.
- **`Class#initialize`** — registered as private. Raises `TypeError` when
  called on already-initialized classes (heuristic: name *or* superclass set).
  Rejects `Class` itself as the requested superclass.
- **`Module.nesting`** — best-effort implementation reading the lexical class
  stack maintained by the executor. Accurate at module/class body scope;
  partial inside method bodies. Notably this unblocks several spec files
  (e.g. `subclasses_spec.rb`) from loading.
- **`Module#subclasses`** — direct subclasses of a class via linear scan of
  the class table.
- **`Class.new` edge cases** — TypeError on metaclass argument; CRuby-style
  `superclass must be an instance of Class (given an instance of …)`
  error message for non-class arguments.
- **`BasicObject.dup`** — raises `TypeError` (`can't copy the root class`).
- **`Kernel#private_methods`** — added (was missing entirely).

### Remaining (8 issues)

The remaining failures in `core/class` need deeper changes:

| Test | Why deferred |
|---|---|
| `Class#dup` (name nil / new constant rename) | Needs deep dup that allocates a new ClassId and copies methods/constants |
| `Class#allocate raises TypeError for #superclass` | Needs distinct uninitialized-class state |
| `Class.new` block runs `inherited` after block | Hook ordering rework |
| `Class.new` constant naming for `MyClass::MyClass::Nested` | Constant-name resolution chain |
| `Class.inherited` subclass-as-constant ordering | Compile-time vs run-time const set ordering |
| `Class#new` uses internal allocator (skips user `allocate`) | `Class#new` slow path currently dispatches to `allocate` |
| `Class#subclasses` concurrent test | Single-threaded runtime |

---

## Progress on `core/module` since 2026-04-08

| Metric | 2026-04-08 | 2026-04-11 | Δ |
|---|--:|--:|--:|
| Files reporting examples | 84 | 84 (1 crashes runner) | 0 |
| Examples | 275 | **974** | **+699** |
| Expectations | 245 | **1662** | **+1417** |
| Failures | 61 | **178** | +117 |
| Errors | 195 | **209** | +14 |
| **Passing examples** (estimated as `examples − failures − errors`) | 19 | **587** | **+568** |
| Pass rate | 7% | **60%** | **+53pp** |

The raw failure/error counts went *up* because so many spec files that
previously failed to load (each of which was counted as 1 error in the old
baseline) now load and run their full set of examples. The "passing
examples" line is a much better measure of progress.

### Implemented (`core/module`) — 2026-04-08 → 2026-04-11

#### Reflection / introspection
- **`Module#protected_instance_methods`** + `get_protected_method_names{,_inherit}` store helpers.
- **`Module#class_variable_defined?`**, **`Module#remove_class_variable`** (with `ClassInfo::remove_cvar`).
- **`Module#included_modules`**, **`Module#subclasses`**.
- **`Module#name`** — now returns a *frozen, identity-stable* cached string.
- **`Module#to_s`** — distinguishes anonymous Modules (`#<Module:0x..>`) from
  anonymous Classes (`#<Class:0x..>`).
- **`Module#const_source_location`** — `(file, line)` is captured at compile
  time inside `ConstSiteInfo` and stored on the defining `ClassInfo`. Walks
  the ancestor chain at lookup.
- **`Module#autoload?`** — returns the registered file path or nil; walks
  ancestors when `inherit` is true.
- **`Kernel#protected_methods`** / **`Kernel#public_methods`**.
- **`Module#used_refinements`** — stub returning `[]`.

#### Constant handling
- **`Module#private_constant`** / **`Module#public_constant`** — real
  implementations. Validate that every name is defined directly on the
  receiver (raise `NameError` otherwise) and toggle the visibility on the
  corresponding `ConstState`.
- **Constant visibility model** — `ConstState` is now a struct with
  `kind: ConstStateKind` (Loaded/Autoload) and `visibility: ConstVisibility`
  (Public/Private). Visibility persists across `Autoload → Loaded` transitions
  and is cleared automatically when a constant is removed.
- **Visibility enforcement** — `Executor::find_constant` checks visibility on
  *syntactic* qualified access (`Foo::Bar`, `obj::Bar`, `::Bar`). Lexical
  access from inside the defining scope is unaffected. `Module#const_get`
  and `Module#const_defined?` intentionally bypass the check (CRuby 3.0+
  reflection semantics).
- **`Module#const_added` hook** — fires for class/module definitions
  (`class C; end`, `module M; end`) **before** `inherited` (matching CRuby).
  Also fires for `const_set` and `autoload`.
- **`Module#autoload`** — validates the constant name (uppercase first
  letter, `[A-Za-z0-9_]`), rejects empty filenames, and uses
  `coerce_to_path_rstring` so `to_path`/`to_str` mock conversions work.

#### Naming of nested classes/modules
- **Anonymous parent rendering** — `m = Module.new; module m::N; end` now
  reports `m::N.name == "#<Module:0x..>::N"` instead of `"N"`.
- **Constant naming convention** — switched to a leaf-name + walk-and-join
  convention (`get_parents`) so:
  - `class M; module Inner; end; end` → `"M::Inner"` ✔
  - `Outer::Inner = Class.new` → `"Outer::Inner"` ✔ (was
    `"Outer::Outer::Inner"`)
  - `m::N` with anonymous `m` → `"#<Module:0x..>::N"` ✔ (was `"N"`)

#### Eval / metaprogramming
- **`Module#class_exec` / `module_exec`** — block-only counterpart of
  `class_eval`. Pushes the class context, forwards splat args to the block.
  Also unblocks several `ruby2_keywords_spec.rb` tests.
- **`Module#attr`** (legacy form) — supports both `attr(:foo, :bar)` and
  `attr(:foo, true)` (which defines a setter too).

### Remaining hot-spots in `core/module`

| Area | Count | Why deferred |
|---|--:|---|
| Refinements (`refine`, `using`, `used_refinements`) | ~70 | Architectural — requires changes to method lookup |
| `autoload` corner cases | ~50 | Various semantic gaps (thread interactions, dup'ed modules sharing state, defined? interaction, …) |
| `const_source_location` corner cases | ~10 | Static class/module definition sites still have no location entry |
| `Module#name` anonymous-module rules | ~10 | Inner classes don't update when their anonymous outer is later named |
| `class_eval`/`module_eval(String)` from C frames | ~24 | "eval requires a Ruby method context" — caller's outermost iseq must be Ruby |
| `Module#deprecate_constant` | ~5 | Method missing (only the `#private_constant` plumbing landed) |
| `set_temporary_name` corner cases | ~8 | Need stricter name validation and re-naming rules |
| Various: `module_function` toggle, `Module#==` for IClass, … | ~10 | Per-test fixes |

---

## Root Cause Classification (sorted by impact / difficulty ratio)

This section is unchanged from the 2026-04-08 baseline; only the items that
have been completed are annotated below. Items related to `class`/`module`
have been moved into the dedicated progress sections above.

### Priority 1 — High Impact, Low Difficulty

#### 1-A. Time accessor methods (~270 errors fixed)
**Category**: time
**Root cause**: Time objects are missing most accessor methods beyond `to_s`.
**Missing methods**: `sec`, `usec`/`tv_usec`, `nsec`/`tv_nsec`, `utc_offset`/`gmt_offset`, `zone`, `wday`, `yday`, `hour`, `min`, `to_i`/`tv_sec`, `to_f`, `subsec`, `ceil`, `floor`, `round`, `dst?`/`isdst`, `utc?`/`gmt?`, `deconstruct_keys`, `xmlschema`/`iso8601`, `_dump`/`_load`
**Difficulty**: Easy — these are straightforward accessors on the internal time representation.

#### 1-B. MatchData accessor methods (~155 errors fixed)
**Category**: matchdata
**Missing methods**: `bytebegin`, `byteend`, `offset`, `byteoffset`, `values_at`, `deconstruct_keys`, `names`, `regexp`, `string`, `pre_match`, `post_match`, `named_captures`, `captures`
**Difficulty**: Easy — simple accessors on the already-captured Onigmo match data.

#### 1-C. Missing Hash methods (~100 errors fixed)
**Category**: hash
**Missing methods**: `flatten`, `fetch_values`, `to_proc`, `ruby2_keywords_hash`, `ruby2_keywords_hash?`
**Also**: FrozenError not raised for frozen hash mutations, `Hash.[]` edge cases
**Difficulty**: Easy (flatten, fetch_values, to_proc) to Medium (ruby2_keywords).

#### 1-D. Missing Numeric/Complex methods (~150 errors fixed)
**Categories**: numeric, complex
**Missing methods**: Complex `rectangular`/`rect`, `polar`, `real`, `imaginary`/`imag`, Numeric `quo`, `fdiv`, `remainder`, `coerce`, `zero?` on subclasses, `rationalize`
**Difficulty**: Easy — straightforward arithmetic.

#### 1-E. Exception methods (~170 errors fixed)
**Category**: exception
**Missing methods**: `detailed_message`, `exception`, Errno subclass completeness (EWOULDBLOCK etc.)
**Also**: Wrong error messages in some cases, missing UncaughtThrowError
**Difficulty**: Easy for most methods.

#### 1-F. Struct improvements (~124 errors fixed)
**Category**: struct
**Missing**: `values_at`, `each_pair`, `keyword_init:` parameter handling, `deconstruct_keys`
**Difficulty**: Easy-Medium.

---

### Priority 2 — High Impact, Medium Difficulty

#### 2-A. Enumerable/Enumerator methods (~400 errors fixed)
**Categories**: enumerator, enumerable, range, array
**Missing Enumerable methods**: `grep_v`, `cycle`, `detect`, `find_index`, `slice_before`, `slice_after`, `chunk`, `chunk_while`, `minmax_by`, `reject`, `take`, `drop`, `drop_while`, `uniq`, `collect_concat`/`flat_map`, `zip`, `tally`, `filter_map`
**Missing classes**: `Enumerator::Chain`, `Enumerator::Product`, `Enumerator::ArithmeticSequence`
**Also**: `Range#step` should return ArithmeticSequence
**Difficulty**: Medium — most Enumerable methods can be implemented in Ruby (`startup/enumerable.rb`). New Enumerator subclasses need Rust scaffolding.

#### 2-B. File class methods (~150 errors fixed)
**Category**: file, filetest
**Missing class methods**: `empty?`, `readable?`, `writable?`, `executable?`, `ftype`, `world_readable?`, `world_writable?`, `readlink`, `link`, `symlink`, `truncate`, `rename`, `utime`, `lutime`, `mkfifo`, `realdirpath`
**Missing instance methods**: `truncate`, `path`, `flock`, `chmod`, `to_path`, `zero?`, `ftype`
**Difficulty**: Easy per method (thin wrappers around libc/syscalls). Many methods to implement.

#### 2-C. IO instance methods (~80+ errors fixed)
**Category**: io
**Missing**: `seek`, `pos`/`tell`, `pos=`, `eof?`, `rewind`, `readlines`, `lineno`, `lineno=`, `each_line`, `each_byte`, `each_char`, `read(n)`, `write`
**Difficulty**: Medium — requires proper IO buffering.

#### 2-D. String#% (sprintf) fixes (~130 errors fixed)
**Category**: string
**Root cause**: Format string parsing is incomplete. Wrong type coercion for `%s` (should call `to_s`), `%d` (should call `to_int`), wrong handling of flags (`#`, `0`, `-`, `+`, space), missing `%a`/`%A`.
**Difficulty**: Medium — significant format string parsing work.

#### 2-E. Dir methods (~129 errors fixed)
**Category**: dir
**Missing**: `glob` pattern edge cases, `children`, `each_child`, `home`, `tmpdir`
**Also**: `Dir.entries` behavior, `.` and `..` handling
**Difficulty**: Medium.

#### 2-F. Kernel methods (~200+ errors fixed)
**Category**: kernel
**Missing/broken**: `Float()` edge cases (hex, underscore, whitespace), `Integer()` edge cases, `system`, `exec`, `open`, `pp`, `printf`, `sprintf` (same as String#%), `caller`, `caller_locations`, `__dir__`, `autoload`, `autoload?`
**Difficulty**: Medium overall, varies per method.

---

### Priority 3 — High Impact, Hard Difficulty

#### 3-A. String encoding support (~600+ failures)
**Categories**: string, encoding, io, hash, array
**Root cause**: monoruby treats all strings as UTF-8. Does not track US-ASCII, ASCII-8BIT, ISO-8859-1, etc. Missing `encoding`, `force_encoding`, `encode`, `valid_encoding?`, `bytes` behavior.
**Difficulty**: Hard — requires pervasive changes to String representation and every string operation.

#### 3-B. Thread support (~478 errors)
**Categories**: thread, queue, sizedqueue, mutex, conditionvariable, threadgroup
**Root cause**: monoruby is single-threaded. Thread, Mutex, Queue, ConditionVariable are stubs.
**Difficulty**: Very Hard — architectural change.

#### 3-C. Refinement support (~250 errors)
**Categories**: module, refinement
**Root cause**: `refine`, `using`, `Module#refine` not implemented.
**Difficulty**: Hard — requires changes to method lookup.

#### 3-D. Encoding::Converter class (~150 errors)
**Category**: encoding
**Root cause**: Transcoding infrastructure not implemented.
**Difficulty**: Hard.

#### 3-E. Process.spawn / fork-exec (~295 errors)
**Category**: process
**Root cause**: `Process.spawn`, `IO.popen`, subprocess management.
**Difficulty**: Hard.

---

## Recommended Implementation Order

Unchanged from 2026-04-08; the table below now omits the class/module work
that has already landed.

| Step | Target | Est. Fix | Difficulty | Categories Helped |
|------|--------|----------|------------|-------------------|
| 1 | Time accessors (sec, usec, zone, wday, ...) | ~270 | Easy | time |
| 2 | MatchData accessors (offset, bytebegin, ...) | ~155 | Easy | matchdata |
| 3 | Numeric/Complex methods (rect, quo, ...) | ~150 | Easy | numeric, complex |
| 4 | Exception methods (detailed_message, ...) | ~100 | Easy | exception |
| 5 | Hash methods (flatten, fetch_values, ...) | ~100 | Easy | hash |
| 6 | Struct methods (values_at, keyword_init) | ~80 | Easy | struct |
| 7 | File class methods (stat wrappers) | ~150 | Easy (bulk) | file, filetest |
| 8 | Enumerable methods in Ruby | ~200 | Medium | enumerable, enumerator |
| 9 | IO instance methods (seek, pos, eof?) | ~80 | Medium | io |
| 10 | String#% / sprintf fixes | ~130 | Medium | string |
| 11 | Dir improvements | ~80 | Medium | dir |
| 12 | Range improvements (step, overlap?) | ~80 | Medium | range |
| 13 | Enumerator subclasses (Chain, Product) | ~80 | Medium | enumerator |
| 14 | Kernel method fixes (Float, Integer, ...) | ~200 | Medium | kernel |
| 15 | Encoding support | ~600 | Hard | string, encoding, io |
| 16 | Refinements | ~250 | Hard | module, refinement |
| 17 | Process.spawn / IO.popen | ~295 | Hard | process, io |
| 18 | Thread support | ~478 | Very Hard | thread, queue, mutex |

Steps 1-6 alone could fix **~855 errors** with relatively low effort.
Steps 1-14 could fix **~1,675+ errors** before tackling the hard
architectural changes.
