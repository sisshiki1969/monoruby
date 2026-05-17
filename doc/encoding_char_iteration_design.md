# Design: Per-Encoding Character Iteration Layer

Status: **proposed** (foundation design; precedes implementation)

## 1. Goal

Remove the implicit "every String is UTF-8" assumption from monoruby's
String operations by introducing a per-encoding character-boundary
layer, so that character-indexed operations (`length`, `[]`, `chars`,
`each_char`, `reverse`, `slice`, `scrub`, `=~`, …) are correct for
non-UTF-8 encodings (EUC-JP, Shift_JIS, ISO-2022-JP, ISO-8859-*,
UTF-16/32) the way CRuby's `rb_enc_*` / `mbclen` machinery is.

Out of scope (separate efforts, tracked elsewhere): `rb_enc_compatible`
unification (analysis item ③), `Encoding::Converter` (⑤), Onigmo
multi-encoding regex scanning (⑦), source-encoding propagation (⑥).
This document is strictly the **character-boundary foundation** (①).

## 2. Current state (code-grounded)

`RStringInner` (`monoruby/src/value/rvalue/string.rs`) already stores
`content: Vec<u8>` + `ty: Encoding` + `cr: Cell<CodeRange>`, and a
partial foundation exists:

- `char_length()` — per-encoding length, but **EUC-JP/Shift_JIS fall
  back to byte count** (`Encoding::EucJp | Encoding::Sjis(_) =>
  self.content.len()`), and ISO-2022-JP round-trips through
  `encoding_rs` only for the count.
- `iter_char_bytes() -> CharByteIter` — encoding-aware byte-slice
  iterator, but its `next()` width table treats **EUC-JP, Shift_JIS,
  ISO-2022-JP as 1 byte/char** (incorrect: those are multibyte).
- `conv_char_index` / `byte_to_char_index` / `from_substring` — exist
  but assume the same (incomplete) width logic.
- `to_str() -> Cow<str>` — for non-UTF-8-compatible encodings returns a
  **`\xHH`-escaped rendering**, not the real characters. Many builtins
  call `to_str()` and therefore misbehave on non-UTF-8 input.

So the structural seam (`CharByteIter`) is in place; the missing piece
is **correct per-encoding character-width decoding** plus a disciplined
migration of UTF-8-assuming call sites onto that seam.

## 3. Design

### 3.1 `EncodingCodec` — the character-boundary trait

Introduce a single decision function (not a trait object; a `match` on
`Encoding` keeps it allocation-free and inlinable, matching the
existing `CharByteIter` style):

```
/// Length in bytes of the character starting at `bytes[0]` under
/// `enc`, given the *preceding* decoder state (for stateful
/// encodings). Returns `CharLen`:
///   - `Char(n)`   : a well-formed character of `n` bytes
///   - `Invalid(n)`: `n` bytes that do not form a valid character
///                   (CRuby counts these as 1 "character" each for
///                    `length`, and `scrub` replaces them)
enum CharLen { Char(usize), Invalid(usize) }

fn char_len(enc: Encoding, st: &mut DecodeState, bytes: &[u8]) -> CharLen
```

`DecodeState` is `()` for all stateless encodings and a small enum only
for `Iso2022Jp` (`Ascii | Jisx0208 | Jisx0201`), updated when an `ESC`
sequence is consumed. Stateless encodings ignore it; this keeps the hot
path (UTF-8/ASCII) branch-predictable.

Per-encoding rules:

| Encoding | rule |
|---|---|
| `UsAscii` | `b < 0x80 → Char(1)`; else `Invalid(1)` |
| `Ascii8` | always `Char(1)` (binary: every byte is a "character") |
| `Iso8859(_)` | always `Char(1)` |
| `Utf8` | existing UTF-8 lead-byte logic (reuse current code) |
| `Utf16Le/Be` | surrogate-pair aware: `Char(2)` or `Char(4)`; trailing odd byte `Invalid(1)` |
| `Utf32Le/Be` | `Char(4)`; trailing `<4` bytes `Invalid(rem)` |
| `EucJp` | `0x00–0x8D,0x90–0x9F → Char(1)`; `0x8E → Char(2)` (JIS X 0201 kana); `0x8F → Char(3)` (JIS X 0212); `0xA1–0xFE` lead → `Char(2)`; malformed → `Invalid(1)` |
| `Sjis(_)` | `0x00–0x80,0xA0,0xFD–0xFF → Char(1)`; `0xA1–0xDF → Char(1)` (half-width kana); `0x81–0x9F,0xE0–0xFC` lead + valid trail `0x40–0x7E,0x80–0xFC → Char(2)`; else `Invalid(1)` |
| `Iso2022Jp` | `ESC`-sequence → consume the 3-byte escape as zero characters (state change); in ASCII/JISX0201 state `Char(1)`; in JISX0208 state `Char(2)`; malformed → `Invalid(1)` |

These tables are the canonical Ruby `onigenc_mbc_enc_len` equivalents;
they are pure functions over bytes (+ ISO-2022-JP state) and fully unit
-testable against CRuby (`"...".force_encoding(e).each_char.to_a`).

### 3.2 Wiring it in

1. **`CharByteIter::next`** becomes the single consumer of `char_len`
   (carrying `DecodeState`). Every other character operation already
   funnels through `iter_char_bytes()` or should be migrated to.
2. **`char_length()`**: `iter_char_bytes().count()` for the
   non-fixed-width encodings (drop the EUC-JP/SJIS byte-count
   fallback). Keep O(1) fast paths for `SevenBit` and fixed-width.
3. **`conv_char_index` / `byte_to_char_index` / `from_substring` /
   `reverse` / `scrub`**: reimplement on top of `iter_char_bytes()`
   (offsets are the iterator's running `pos`), so they are correct for
   every encoding by construction.
4. **`to_str()` policy**: this is the riskiest seam (§5). Introduce
   `chars_lossy()` / explicit byte APIs and migrate builtins that do
   character work off `to_str()` for non-UTF-8 strings, rather than
   silently `\xHH`-escaping.

### 3.3 Invariants

- `iter_char_bytes()` yields slices whose concatenation == `content`
  (no byte is dropped or duplicated) for **every** encoding/byte input,
  including broken input. This is the property regression tests assert.
- `char_length() == iter_char_bytes().count()` for all inputs.
- ASCII-only content under any ASCII-compatible encoding stays the
  existing O(1) `SevenBit` path (no perf regression for the common
  case — the optcarrot/benchmark strings are ASCII UTF-8).

## 4. Migration plan (incremental, zero-regression per step)

Each step is an independently shippable, spec-diffed PR (same
methodology used for the #525–#535 series):

- **P0 — codec tables + tests.** Add `char_len`/`DecodeState`, rewrite
  `CharByteIter` to use them. No public behavior change for
  UTF-8/ASCII/fixed-width (proven by `cargo test` + `core/string`
  diff). Adds correctness only for EUC-JP/SJIS/ISO-2022-JP iteration.
- **P1 — length/index.** Route `char_length`, `conv_char_index`,
  `byte_to_char_index` through the iterator; delete the byte-count
  fallbacks. Target specs: `String#length`, `#[]`, `#slice` for
  non-UTF-8.
- **P2 — chars/each_char/reverse/scrub.** Migrate these builtins off
  `to_str()`+`chars()` onto `iter_char_bytes()`.
- **P3 — `to_str()` callers.** Audit the ~40 `coerce_to_str`/`to_str()`
  call sites in `builtins/string.rs`; split into "needs bytes"
  (unchanged) vs "needs characters" (move to the iterator). This is the
  largest step and is itself sub-divided per method family.

Ordering rationale: P0 is pure addition (lowest risk, unlocks
everything); P1/P2 are mechanical given P0; P3 is the long tail and can
proceed method-family by method-family without blocking.

## 5. Risks & mitigations

- **`to_str()` blast radius (highest).** It is the de-facto "give me
  the string" accessor across builtins; changing its non-UTF-8
  semantics wholesale would regress widely. *Mitigation*: do **not**
  change `to_str()` semantics in P0–P2; only add new explicit
  character/byte APIs and migrate callers individually in P3 with a
  per-family spec diff.
- **Performance.** The hot path is ASCII UTF-8. *Mitigation*: keep the
  `SevenBit` O(1) short-circuits in `char_length`; `char_len`'s first
  match arm is the UTF-8 `b < 0x80 → Char(1)` case (same as today).
- **Onigmo coupling.** Regex scanning still only knows ASCII/UTF-8
  (analysis item ⑦). This layer makes *String* correct but does **not**
  make `=~` correct for EUC-JP patterns; that is explicitly out of
  scope and must be documented in each P-step PR to avoid scope creep.
- **CodeRange cache coherence.** `cr` must be invalidated/recomputed
  consistently with the new width logic. *Mitigation*: `classify()`
  and `char_len` share the same per-encoding validity definition;
  add a debug-assert (`cfg(debug_assertions)`) that
  `iter_char_bytes()` concatenation == `content`.
- **Spec oracle.** All tables are validated by `run_tests` against
  CRuby 4.0.2 (`s.force_encoding(enc).each_char.map(&:bytes)` golden
  vectors) and a `core/string` + `core/encoding` regression diff per
  PR, consistent with the established zero-regression workflow.

## 6. Validation strategy

- Unit: golden `char_len` vectors per encoding vs CRuby
  (`force_encoding` + `each_char`/`length`/`reverse`).
- Property: for random byte buffers, assert
  `concat(iter_char_bytes()) == content` and
  `char_length() == iter_char_bytes().count()` for every `Encoding`.
- Spec: `core/string`, `core/encoding`, `core/symbol`, `core/regexp`
  (MatchData captures) regression diff vs `origin/master` — **zero
  regressions** gate per PR; both Prism and ruruby parsers.

## 7. Decisions (confirmed)

1. **ISO-2022-JP statefulness — deferred.** P0 implements the
   *stateless* encodings natively (EUC-JP, Shift_JIS; ISO-8859-*,
   ASCII-8BIT, US-ASCII, UTF-16/32 are already fixed-width). The
   `DecodeState` machine for ISO-2022-JP is **out of P0**; ISO-2022-JP
   continues to route through `encoding_rs` for length/iteration (it
   is rare in specs) and lands in a later step. `char_len`'s
   signature still carries `&mut DecodeState` so the later step is
   additive.
2. **`to_str()` long-term — `display_lossy()` split + convention.**
   The `\xHH` fallback rendering moves to a clearly-named
   `display_lossy()` accessor; `to_str()` is reserved for "real
   characters / bytes" and character-work-on-non-UTF-8 via `to_str()`
   is forbidden by review convention. This split happens in **P3**
   (not P0–P2, which keep `to_str()` semantics unchanged).

