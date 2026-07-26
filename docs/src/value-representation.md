A monoruby `Value` is a 64-bit non-zero integer (`NonZeroU64`) using a **tagged-union** scheme: the lower 3 bits encode the kind of value. It is *not* NaN-boxing. Because `Value` is always a single machine word, values can live directly in VM registers, JIT machine registers, and GC-scanned stack slots.

## Dispatch on the lower 3 bits

| Lower bits (`& 0b111`) | Kind |
| --- | --- |
| `???????1` (bit 0 = 1) | **Fixnum** — integer stored in bits 63:1 as i63 (`value >> 1`) |
| `??????10` (bits 1:0 = `10`) | **Flonum** — double-precision float encoded inline (bit-rotated) |
| `?????000` (bits 2:0 = `000`) | **Heap pointer** — raw pointer to a GC-managed `RValue` |
| other (bit 2 = 1, bits 1:0 ≠ `10`) | **Other immediate** — `nil` / `true` / `false` / Symbol |

`is_packed_value()` tests `bits & 0b0111 != 0`; if true, the value is an immediate and `try_rvalue()` returns `None`. If false, the bits are a valid `*const RValue` pointer (RValues are 8-byte aligned, so their low 3 bits are always zero).

## Immediate tag constants

| Constant | Hex | Binary | Meaning |
| --- | --- | --- | --- |
| `NIL_VALUE` | `0x04` | `0000_0100` | `nil` |
| `FALSE_VALUE` | `0x14` | `0001_0100` | `false` |
| `TRUE_VALUE` | `0x1c` | `0001_1100` | `true` |
| `TAG_SYMBOL` | `0x0c` | `0000_1100` | Symbol (`IdentId` packed in the upper 32 bits) |

`FLOAT_ZERO` (`(0b1000 << 60) | 0b10`) is the flonum encoding of `0.0`.

## Consequences

- **Fixnum** covers 63-bit signed integers. Integer results that overflow i63 are promoted to heap-allocated Bignum objects (backed by `BigInt`).
- **Flonum** covers most doubles; floats whose exponent falls outside the encodable range are heap-allocated as `RValue`s of class `Float`.
- `nil` / `false` are the only falsy values, and both have bit patterns distinguishable with a single mask — which the JIT exploits for cheap truthiness tests.
- Equality on immediates (Fixnum, Symbol, `nil`, `true`, `false`) is plain 64-bit comparison.

## Heap values: `RValue`

Everything that is not an immediate lives on the GC heap as an `RValue` (defined under `monoruby/src/value/rvalue/`): Strings, Arrays, Hashes, objects with instance variables, Bignums, non-flonum Floats, Ranges, Procs, Fibers, and so on. `RValue`s are allocated from the GC's page-based arena and carry the object's class, flags (including the generational-GC age bits), and kind-specific payload. See [Garbage Collection](garbage-collection.md).

## Relevant source

- [`monoruby/src/value.rs`](https://github.com/sisshiki1969/monoruby/blob/master/monoruby/src/value.rs) — the `Value` type and tag scheme
- [`monoruby/src/value/numeric.rs`](https://github.com/sisshiki1969/monoruby/blob/master/monoruby/src/value/numeric.rs) — Fixnum / Flonum / BigInt helpers
- [`monoruby/src/value/rvalue/`](https://github.com/sisshiki1969/monoruby/tree/master/monoruby/src/value/rvalue) — heap object representation
