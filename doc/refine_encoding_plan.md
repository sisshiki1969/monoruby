# Encoding subsystem refinement — handoff plan

Status: **handoff document** for a future session. No encoding code has
been changed yet; this captures the diagnosis, concrete code pointers,
a phased plan, and verification commands so the work can resume cold.

Reference CRuby in this environment: **4.0.4** (mspec oracle). Target
binary for spec runs: `target/release/monoruby`.

---

## 1. Why this work exists

A ruby/spec survey of `core/{symbol,range,hash,set,array}` showed that a
large, recurring share of the *remaining* failures across **symbol,
string, hash, array** trace back to **one root cause: monoruby's
non-UTF / multibyte encoding support is incomplete**. It manifests four
ways:

1. **`Encoding.list` is almost empty.** It returns only
   `[UTF-8, ASCII-8BIT]` (see §3), so any spec that does
   `Encoding.list.select(&:dummy?)` / `.reject(&:ascii_compatible?)`
   iterates an empty list. mspec then reports *"No behavior expectation
   was found"* and counts the example as a **failure** even though the
   tested method (`Symbol#inspect`, `String#inspect`, …) may be fine.
2. **Missing transcoders.** e.g. `UTF-8 → UTF-32LE` raises
   `Encoding::ConverterNotFoundError`; several `casecmp`/`encode`
   paths depend on it.
3. **Cross-encoding compatibility rules are too eager.** e.g.
   `ASCII-8BIT` vs `UTF-16BE` raises `Encoding::CompatibilityError`
   where CRuby decides compatibility by content.
4. **`inspect` escaping** for non-ASCII / non-UTF / dummy-encoding
   strings & symbols (`:"\x61\x62"`, `\uXXXX`) is not exercised /
   not correct because (1) prevents the specs from running and the
   default-external-encoding rules aren't applied.

This is the continuation of the **"encoding P0–P3"** line of work
already on `master` (see §6).

---

## 2. Concrete remaining failures attributable to encoding

Numbers are from the re-baseline against a fresh `master` build
(commit `7c1137b` era; rerun to refresh — see §7).

### core/symbol — 2 failures, 3 errors (330 examples) — **100% encoding**
- ERROR `Symbol#casecmp with Symbol returns 0 for empty strings in different encodings` → `ConverterNotFoundError: UTF-8 to UTF-32LE`
- ERROR `Symbol#casecmp? returns true for empty symbols in different encodings` → same
- ERROR `Symbol#end_with? checks that we are starting to match at the head of a character` → `CompatibilityError: ASCII-8BIT and UTF-16BE`
- FAIL `Symbol#inspect quotes symbols in non-ASCII-compatible encodings` (inspect_spec.rb:118) → empty-loop artifact: `Encoding.list.reject(&:ascii_compatible?).reject(&:dummy?)` is empty
- FAIL `Symbol#inspect quotes and escapes symbols in dummy encodings` (inspect_spec.rb:125) → empty-loop artifact: `Encoding.list.select(&:dummy?)` is empty

### core/hash & core/array — encoding-attributable subset
- `Hash#inspect`/`#to_s` and `Array#inspect`/`#to_s` "can be evaled" /
  "not default external encoding": ~6 (hash) + ~4 (array) examples want
  symbol-key quoting and `\uXXXX` escaping under a non-ASCII default
  external encoding. Same inspect/encoding root cause.

### Not encoding (do NOT chase here)
- hash/array mock `#hash`/`#eql?` dispatch (deferred item 5),
  `Set#divide` enumerator arity (deferred item 2), `Array#pack`
  formats, `fill` arg validation. Tracked separately.

Expected payoff of this work: clears the 5 `core/symbol` items and a
chunk (~10) of hash/array inspect failures in one coherent effort.

---

## 3. Codebase map (file:line as of writing — re-grep before editing)

All paths under `monoruby/`.

| What | Where |
| --- | --- |
| Encoding builtins (class, predicates, transcode, encode) | `src/builtins/encoding.rs` (~3.1k lines) |
| `Encoding.list` — **returns only `[UTF-8, ASCII-8BIT]`** | `src/builtins/encoding.rs` `fn enc_list` ~line 2492 |
| `Encoding.find` | `fn enc_find` ~2510 |
| Registered constant names (UTF_16LE/BE, UTF_32*, SJIS, EUC…) | encoding.rs ~line 58–165 (`canonical_encoding_name`, constant list) |
| `Encoding#ascii_compatible?` / `#dummy?` builtins | `fn enc_ascii_compatible_p` ~3077, `fn enc_dummy_p` ~3097 |
| CRuby "dummy" name set | `fn is_cruby_dummy_name` ~3048 (UTF_7/16/32, ISO-2022-JP, CESU-8…) |
| Transcoder entry point | `fn transcode_bytes_with_opts` ~531 (uses `encoding_rs`; has ASCII fast paths; raises ConverterNotFound for unhandled pairs) |
| `String#encode` plumbing | `fn encode` ~934 (calls `transcode_bytes_with_opts`) |
| `Encoding::Converter` (stub) | encoding.rs ~321 (validates pairs; runtime `convert`/`primitive_convert` partly implemented — see encoding_tests.rs) |
| `Encoding` value type / per-encoding props | `src/value.rs` (`crate::value::Encoding`, `is_ascii_compatible`, width/validity) |
| Per-encoding char iteration (P0–P3) | `src/value/rvalue/string.rs`; design doc `doc/encoding_char_iteration_design.md` |
| `Symbol#inspect` / `String#inspect` escaping | `src/builtins/symbol.rs`, `src/builtins/string.rs` (search `inspect`), and `Value::inspect` |
| Symbol cross-encoding ops (casecmp/end_with?) | `monoruby/builtins/symbol.rb` (e.g. `:42` end_with? at symbol.rb:42) + symbol.rs |
| Dummy-encoding `#inspect` form `#<Encoding:NAME (dummy)>` | encoding.rs ~3040 |
| Existing design docs | `doc/encoding_char_iteration_design.md`, `doc/string_api_design.md` |

> Build note: Ruby-side files under `monoruby/builtins/*.rb` are copied
> to `~/.monoruby/` by `build.rs` **only on build**. After editing them
> run `touch monoruby/build.rs && cargo build --release`.

---

## 4. Phased plan (each phase: implement → mspec verify → commit)

### Phase A — Register the full encoding table in `Encoding.list`
**Goal:** make `Encoding.list` return the encodings monoruby already
has constants for (UTF-8, ASCII-8BIT, US-ASCII, the ISO-8859 family,
Windows-125x, Shift_JIS, EUC-JP, UTF-16{,LE,BE}, UTF-32{,LE,BE}, and
the CRuby "dummy" set from `is_cruby_dummy_name`).

- Rewrite `enc_list` (encoding.rs ~2492) to enumerate every registered
  `Encoding::*` constant (it currently hard-returns 2). The constant
  list already exists near encoding.rs ~58–165 — reuse it as the
  single source of truth.
- Ensure each listed object answers `name`, `ascii_compatible?`,
  `dummy?` consistently (predicates already exist; verify they cover
  UTF-16/32 ⇒ not ascii_compatible, and `is_cruby_dummy_name` ⇒
  dummy?).
- **Expected:** the 2 `core/symbol` inspect "failures" start *running*
  their bodies. They will then pass **iff** `Symbol#inspect` already
  emits `:"foo"` for non-ascii-compat and `:"\x61\x62\x63\x64"` for
  dummy encodings (verify; if not, small fix in Phase D).
- Verify: `core/symbol`, `core/encoding`, `core/string` (regression —
  many specs introspect `Encoding.list`; watch for new failures from
  specs that now iterate a bigger list).

### Phase B — Transcoders for UTF-16/UTF-32 (both endians)
**Goal:** kill `ConverterNotFoundError: UTF-8 to UTF-32LE` (and the
symmetric/sibling pairs).

- In `transcode_bytes_with_opts` (encoding.rs ~531) add the UTF-8 ↔
  UTF-16{LE,BE} and UTF-8 ↔ UTF-32{LE,BE} pairs. `encoding_rs` handles
  UTF-16; UTF-32 likely needs a small hand-rolled codec (4-byte
  units, BOM/endianness, surrogate validation, `Undefined`/`Invalid`
  → `Encoding::UndefinedConversionError`/`InvalidByteSequenceError`
  consistent with the existing error helpers).
- Route `Encoding::Converter` and `String#encode` through the same
  path (they already call `transcode_bytes_with_opts`).
- Verify: `core/symbol/casecmp_spec.rb`, `casecmp_query_spec.rb`,
  `core/string/encode_spec.rb`, `core/encoding/converter` (regression).

### Phase C — Cross-encoding compatibility rules
**Goal:** fix `CompatibilityError: ASCII-8BIT and UTF-16BE` in
`Symbol#end_with?` (and the shared String compatibility predicate).

- Find the compatibility check (search `CompatibilityError` /
  `Encoding.compatible?` in encoding.rs + string.rs). Implement CRuby's
  `rb_enc_compatible` semantics: two strings are compatible if equal
  encodings; if one is ASCII-only its encoding "yields"; ASCII-8BIT
  with only ASCII bytes is compatible with any ASCII-compatible
  encoding; otherwise incompatible. The current code raises too
  eagerly for the empty/ASCII-only operands the specs use.
- Verify: `core/symbol/end_with_spec.rb`, `core/string/{end_with,
  start_with,concat,plus}_spec.rb` (regression).

### Phase D — inspect escaping under non-ASCII default external
**Goal:** the hash/array (and symbol) inspect "can be evaled" /
"not default external encoding" examples.

- Make `Symbol#inspect` / `String#inspect` / `Array#inspect` /
  `Hash#inspect` escape non-ASCII as `\uXXXX` (or `\xNN` for
  non-UTF/dummy) when the relevant default-external rule applies, and
  quote symbol keys that need quoting (`:"!":` vs `!:`). Centralize in
  `Value::inspect` if possible so all four share one implementation.
- NB: a separate local artifact exists where the *sandbox* CRuby
  (4.0.4 with unset `LANG`) escapes non-ASCII in `inspect`; that makes
  `builtins::string::tests::string_inspect` /
  `symbol_inspect_unquoted` fail under `cargo test` locally but pass
  on CI. Do **not** "fix" monoruby to match the broken-locale oracle;
  match CRuby-with-UTF-8-locale (what CI uses). Validate with `LANG=
  C.UTF-8 ruby` vs `monoruby`.
- Verify: `core/symbol/inspect_spec.rb`, `core/string/inspect_spec.rb`,
  `core/hash/{to_s,inspect}_spec.rb`, `core/array/{inspect,to_s}_spec.rb`.

### Phase E — full regression sweep
Run the five categories plus `core/string` and `core/encoding`
end-to-end; confirm net failures strictly decrease and no new
errors/regressions. Commit per phase with the before/after tallies in
the message (matches the project's commit-message convention).

---

## 5. Verification commands

```sh
# build (Ruby-side builtins need the touch)
touch monoruby/build.rs && cargo build --release

# one category, summary only
cd ../spec && timeout 300 ../mspec/bin/mspec run core/symbol \
  -t /home/user/monoruby/target/release/monoruby --format dotted | tail -3

# one spec file with failure detail
cd ../spec && ../mspec/bin/mspec run core/symbol/inspect_spec.rb \
  -t /home/user/monoruby/target/release/monoruby 2>&1 | grep -A6 'FAILED\|ERROR'

# big categories: batch to avoid timeouts
cd ../spec && find core/array -name '*_spec.rb' | xargs -n 40 sh -c \
  'timeout 240 ../mspec/bin/mspec "$@" -t /home/user/monoruby/target/release/monoruby --format dotted 2>&1 | tail -1' _
```

Also run the in-tree harness for regressions:
`cargo test -p monoruby --lib builtins::encoding::tests builtins::string::tests`
(and with `MONORUBY_PARSER=ruruby`). Use `--test-threads=1`; parallel
runs are flaky in this sandbox (pre-existing, env-related).

---

## 6. Prior art on `master` (do not duplicate)

`git log --oneline -- monoruby/src/builtins/encoding.rs monoruby/src/value/rvalue/string.rs`:
- `encoding P2: per-encoding validity + scrub for EUC-JP / Shift_JIS (#538)`
- `encoding P1: char-index String#[] / #slice for EUC-JP / Shift_JIS (#537)`
- `encoding: per-encoding char-iteration design + P0 (EUC-JP/Shift_JIS width) (#536)`
- design: `doc/encoding_char_iteration_design.md`

This plan is the **"P4: full encoding table + UTF-16/32 transcode +
compat rules + inspect"** continuation.

---

## 7. Baseline snapshot & branch state for continuity

Spec tallies captured this session (rerun §5 to refresh — `master`
moves):

| category | failures | errors | notes |
| --- | --- | --- | --- |
| core/symbol | 2 | 3 | **all encoding** (this plan) |
| core/range | 0 | 0 | fixed this session (committed) |
| core/set | 1 | 1 | item 2 deferred (Enumerator arity) |
| core/hash | 24 | 4 | after subclass-default fix; ~6 inspect = this plan |
| core/array | 23 | 8 | after defensive-copy fix; ~4 inspect = this plan |

Related spec-fix work committed on branch
`claude/spec-fixes-1-5` (off `master`), independent of this plan:
1. `range: iterate single ASCII-char String/Symbol endpoints by codepoint`
3. `array: defensive-copy combination/repeated_combination; lazy permutation enumerator`
4. `hash: honour subclass #default in Hash#[]; Hash#slice bypasses subclass #[]`

Two items were **deferred as architectural** (separate from encoding,
noted here only so the next session has the full map):
- **Item 2 — `Set#divide` enumerator arity:** monoruby's generic
  Enumerator inserts a yielder proc, so predicate-consuming methods
  (`divide`/`classify`) can't see the user block's arity. Needs the
  Enumerator to propagate the downstream block arity to the driven
  method. 1–2 specs.
- **Item 5 — Ruby-level `#hash`/`#eql?` dispatch in the native hash
  table:** `Value::ruby_hash` already dispatches to Ruby `#hash` for
  generic objects, but object-key lookups in the vendored `rubymap`
  (`rubymap/src/{map,set}.rs`, traits in `ruby_traits`) don't
  consistently route through Ruby `#hash`/`#eql?`, so mock specs
  (`Hash#[] compares key via hash`, `Hash#==` via `eql?`) and
  nested-`Set` equality fail. Core data-structure change, broad
  regression surface — do it on its own branch with full regression.

---

## 8. Suggested order & rationale

A → B → C → D → E. Phase A is the highest-leverage, lowest-risk step
(it unblocks the empty-loop "failures" across symbol/string/hash/array
and is mostly enumerating data monoruby already has). B and C are
contained codec/predicate work. D is the broadest (touches four
`inspect`s) — centralize it. Keep each phase a separate, spec-verified
commit so the handoff stays bisectable.
