monoruby's exception machinery is built around **laziness**: at `raise` time it stores the minimum needed to unwind, and defers every expensive step — building the Ruby exception object, walking callers, formatting strings — until (and unless) something actually asks for it. This page is an overview; the full design document, including a detailed contrast with CRuby, is [`doc/exception_handling.md`](design/exception_handling.md).

## In-flight errors

While an exception is propagating it is **not a Ruby object** but a Rust struct, `MonorubyErr`, stored in the executor. Its kind covers both real exceptions (`TypeError`, `NameError`, `ArgumentError`, …, plus `Other(ClassId)` for user classes) and **control-flow pseudo-exceptions**: `MethodReturn` (non-local `return`), `BlockBreak`, `Throw` (`Kernel#throw`), `Retry`, and `Redo`. Both families share one unwinder, mirroring how CRuby routes `break`/`return` through its `THROW_DATA` tags.

## Unwinding

The unwinder runs once per frame. For each interpreted frame it:

1. Dispatches control-flow kinds first — `return`/`break`/`throw`/`retry`/`redo` are resolved **before** any backtrace capture, so non-local control flow never pays for a backtrace or allocates an exception object.
2. Records the frame's source location into the incremental trace (frames are being destroyed, so this is the only chance).
3. Consults the function's **exception table** for the innermost region covering the current pc, yielding a rescue target, an ensure target, and the slot for the error value. Rescue jumps materialize the exception object and set `$!`; ensure jumps *defer* the in-flight unwind, resuming it when the ensure body finishes (a new exception raised inside `ensure` supersedes the deferred one).
4. Otherwise returns the error to the caller frame and repeats.

`$!` is scoped per rescue region: its previous value is saved on region entry and restored on exit, so nested and non-local exits observe CRuby's semantics.

## Lazy backtraces

Backtrace cost is split into three deferred stages: frames between raise and rescue are captured incrementally during unwinding; frames *above* the rescuer are filled in only at the catch point (the last moment the stack is coherent), by walking each caller's saved call-site pc — the same mechanism behind `Kernel#caller`; and formatting into strings happens only when `Exception#backtrace` is first called, then is memoized. CRuby, by contrast, captures the full backtrace eagerly at raise time. The practical result: `rescue`-based control flow in hot code is far cheaper than in CRuby, and `StopIteration` under `Kernel#loop` is caught at the Rust level with near-zero cost.

## Exception objects

The Ruby exception object is created only at a catch point or at top level. Re-raising an exception object preserves its identity; implicit `cause` chaining from `$!` and the explicit `raise ..., cause:` keyword follow CRuby's rules. Class-specific payloads (`LoadError#path`, `SystemExit#status`, `NoMethodError#name`/`#receiver`, …) ride along as hidden instance variables.

Native builtins participate through a simple protocol: a builtin returns a `Result<Value>`, and on error the `#[monoruby_builtin]` wrapper stores the `MonorubyErr` in the executor and returns a sentinel that routes into the same unwinder.

## Further reading

- [`doc/exception_handling.md`](design/exception_handling.md) — full mechanism, exception-table format, `$!` restoration, ensure deferral, and the CRuby (`catch_table` / `THROW_DATA`) comparison
