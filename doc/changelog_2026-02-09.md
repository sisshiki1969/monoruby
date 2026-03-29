# Changelog: 2026-02-09 ~ 2026-03-29

## Summary

ruby/spec core compatibility improvements across PRs #158–#250.

- **Crashes/Panics**: 14 → 0
- **ruby/spec F+E**: ~14,000 → 11,956 (~15% improvement)
- **cargo test**: 568 pass / 10 fail → 756 pass / 0 fail

---

## Critical Bug Fixes

### JIT: Wrong condition codes for NaN float comparison (#186)

JIT-compiled code used incorrect x86-64 condition codes for floating-point comparisons, causing wrong results when NaN was involved.

```ruby
x = 0.0 / 0.0  # NaN
x < 1.0   # JIT returned true (should be false)
x == x    # JIT returned true (should be false)
```

### GC crash: Type safety for JIT constant slots (#185)

During garbage collection, constant slots stored by JIT could be interpreted as invalid pointers, causing SEGV. Fixed by ensuring proper type tagging when JIT stores constant values into slots.

### JIT: Panic after BOP (basic operation) redefinition (#220)

When a basic operation like `Integer#+` was redefined, JIT invalidation cleared entries but recompilation tried to access them, causing a panic. Added `is_iseq()` checks and `catch_unwind` safety net.

```ruby
class Integer
  def +(other) = 42  # Redefine Integer#+
end
1 + 2  # JIT panicked during recompilation
```

### Set#flatten infinite recursion / stack overflow (#220)

Recursive sets caused infinite recursion in `flatten`. Added cycle detection with a `seen` HashSet.

```ruby
s = Set.new
s.add(s)
s.flatten  # Stack overflow → SEGV
```

### IO.popen close deadlock (#221)

Parent process waited for child to exit while the pipe was still open, causing a deadlock. Fixed by dropping stdout before calling wait.

```ruby
io = IO.popen("cat", "r+")
io.write("hello")
io.close  # Deadlock: parent waits for child, child waits for pipe to close
```

### Proc#call: Panic on yield in detached context (#211)

When a block was captured and later called after the original method returned, `yield` panicked because the target frame was gone. Fixed by converting to `LocalJumpError`.

```ruby
def foo(&b) = b
pr = foo { |x| x * 2 }
pr.call(5)  # Panicked: yield target disappeared
```

### String: Panic on non-UTF-8 byte sequences (#209, #210, #212)

Rust's `str` operations panicked on invalid UTF-8 bytes. Fixed `is_str()` to return `None` for non-UTF-8 strings.

```ruby
s = "\xFF\xFE".force_encoding("UTF-8")
s.casecmp("hello")  # Panicked (Rust str operation on invalid UTF-8)
s.index("a")        # Panicked
```

### eval("return ...") not returning from method (#232)

`return` inside `eval` did not propagate to the enclosing method. Fixed bytecode generation to set the return flag for eval.

```ruby
def foo
  eval("return 42")
  99
end
foo  # Before: 99, After: 42
```

### Array#fill: Panic with no arguments (#244)

`lfp.arg(0)` was called without checking if the argument existed, causing a panic when `fill` was called with no arguments.

```ruby
[1, 2, 3].fill  # Panicked: lfp.arg(0) returned None
```

### Array#fill: Capacity overflow on huge sizes (#244)

`resize` was called with extremely large values, causing an abort. Added size check and `checked_add` to raise `ArgumentError` instead.

```ruby
[1, 2, 3].fill(10, 1, 2**62)  # Aborted: capacity overflow in resize
```

### Array#[]=: Frozen array bypass via JIT (#241)

JIT fast path for array index assignment bypassed the freeze check. Added frozen guard in both `runtime::set_index` and JIT assembly.

```ruby
a = [1, 2, 3].freeze
a[0] = 99  # JIT fast path allowed this (should raise FrozenError)
```

### String#upto infinite loop (#250)

When `succ` produced a string longer than `max`, the loop never terminated. Added `current.length <= max.length` guard.

```ruby
"z".upto("c") { |s| puts s }  # "z".succ = "aa", length keeps growing
```

---

## New Features / Method Implementations

### Core Classes

- **Rational class** (#224) — Pure Ruby implementation (arithmetic, comparison, to_f/to_i/to_r)
- **Complex#inspect / Complex#to_s** (#250) — CRuby-compatible formatting `(1+2i)`
- **Thread class** (#222) — Minimal single-threaded implementation
- **Process::Status, Signal.list** (#237)

### Kernel / Format

- **Kernel#sprintf / format** (#162, #249) — `%g/%G/%B/%u/%p/%o`, `#`/` `/`+`/`-`/`*` flags, `%{name}` named references, scientific notation normalization
- **Kernel#puts/print/p/printf** (#235) — Redefined in Ruby to delegate to `$stdout`

### String

- **pack/unpack templates** (#237, #249) — Integer types (`s/S/i/I/l/L/q/Q/j/J/C/c/n/N/v/V`), float types (`d/f/e/E/g/G`), modifiers (`!/_`, `>/<`), `@/#`/whitespace
- **String#succ!/next!** (#236), **String#insert** (#236)
- **String#byteindex** (#250) — Byte-position search
- **String#to_c, String#to_r** (#250) — Complex/Rational parsing
- **String#encode/encode!** (#249) — Stub for encoding compatibility
- **String#concat/prepend/reverse/chop/squeeze/partition/upto** (#250) — Ruby-defined with coercion

### Numeric

- **24+ missing methods** (#223, #189) — Integer, Float, Numeric
- **Math module functions** (#192) — All standard functions
- **Numeric#to_c, Numeric#to_r, NilClass#to_r** (#250)

### Hash

- `Hash.[]`, `shift`, `key`, `keep_if`, `delete_if`, `reject!`, `assoc`, `rassoc`, `default_proc=`, comparison operators (`<`, `<=`, `>`, `>=`) (#195–#200)

### Array

- **Array#replace** (#190), **Array#intersect?** (#202)

### Symbol

- Missing Symbol methods (#198)

### Method / UnboundMethod

- `name`, `owner`, `unbind`, etc. (#216)

### Range

- `Range#min/max/count/minmax` (#215) — Endless range support

### IO

- `IO.popen` r+/w mode (#221), `IO.select` (#230), `IO#fileno`, `IO#write` (variadic), `IO#syswrite`, `IO.sysopen`
- `IO#set_encoding/external_encoding/internal_encoding` (#249) — Stubs (UTF-8 fixed)

### File

- `File.delete/chmod/symlink/readlines` (#237), `File.stat/File::Stat` (#236)
- `File.size/size?` class methods (#247), `File#size` instance method (#250)
- `File.umask/fnmatch/absolute_path/split` (#237)

### Dir

- `Dir.mkdir/rmdir/entries` (#237), `Dir.exist?` (#208)

### Encoding

- `Encoding.find` expanded (#246) — ~70 encoding names, aliases, case-insensitive lookup
- ~60 Encoding constants (#242) — US_ASCII, ISO_8859_*, Shift_JIS, EUC_JP, IBM*, etc.

---

## CRuby Compatibility Improvements

### Implicit Type Conversion

- **to_int/to_str/to_f/to_ary/to_hash** (#225, #226, #241, #243, #249) — Applied `coerce_to_*` across all builtin methods in Array, String, Hash, IO, Kernel, etc.
- **Coerce protocol** (#250) — Numeric binary operators (`+`, `-`, `*`, `/`, `%`, `**`, `|`, `&`, `^`, `>>`, `<<`) now call `rhs.coerce(lhs)` on type mismatch
- **Comparison operators** (#250) — Try coerce protocol, raise `ArgumentError` on failure (not TypeError)

### Error Handling

- **Errno exceptions** (#248, #250) — `RuntimeError` → proper `Errno::ENOENT/EACCES/EISDIR/ENOTDIR/EEXIST` across all File/IO/Dir system calls
- **IOError** (#239) — Closed stream errors changed from `RuntimeError` to `IOError`
- **TypeError messages** (#249, #250) — CRuby-compatible format (`"no implicit conversion of X into Y"`, `"X can't be coerced into Y"`)

### Output Format

- **Float#inspect/to_s** (#241) — CRuby-compatible (inf, -0.0, scientific notation thresholds)
- **Frozen object support** (#240) — Proper freeze/frozen? behavior

### Method Arity

- **30+ arity fixes** (#243, #245) — IO, File, Hash, Kernel, Time, Enumerator, etc.

---

## Infrastructure

- **bin/spec** (#236, #244) — Script to run all 58 ruby/spec core categories with timeout/batch support
- **Codecov coverage tests** added for PRs #244–#249
- **`--disable-gems` CLI option** support
- **Parser**: Support `self`/`true`/`false` as keyword arg labels
