# Exception handling in monoruby — mechanism and CRuby contrast

How monoruby raises, unwinds, catches, and reports exceptions, and how the
design differs from CRuby. The through-line is **laziness**: monoruby stores
the minimum at raise time and defers the expensive work (exception-object
materialization, backtrace string formatting) until something actually asks
for it. This keeps the raise path — including the control-flow "pseudo
exceptions" (`return` from a block, `break`, `throw`, `retry`, and internal
`StopIteration`) that reuse the same machinery — cheap.

Primary sources:

- `../monoruby/src/globals/error.rs` — `MonorubyErr`, `MonorubyErrKind`,
  backtrace formatting.
- `../monoruby/src/executor.rs` — `set_error`/`take_error`, `$!` handling,
  `take_ex_obj` (materialization), `complete_backtrace_for_rescue`, ensure
  deferral.
- `../monoruby/src/codegen/jit_module.rs` — `handle_error` (the unwinder).
- `../monoruby/src/globals/store/iseq.rs` — the per-method exception table
  (`get_exception_dest`, `errinfo_restore_slots`).
- `../monoruby/src/builtins/exception.rs`, `../monoruby/builtins/startup.rb`
  — the Ruby-visible `Exception` API (`#backtrace`, `#backtrace_locations`,
  `#set_backtrace`, `#cause`, …).

---

## 1. The big picture

```
   raise / error in a builtin or VM op
        │  vm.set_error(MonorubyErr)                (executor.rs:1074)
        ▼
   error sentinel returned  ──►  entry_raise  ──►  handle_error(vm, globals, meta, pc)
                                                        │   (jit_module.rs:85)
        ┌───────────────────────────────────────────────┤
        │  For the *current* frame:                      │
        │   1. dispatch control-flow kinds early         │
        │      (MethodReturn / Throw / BlockBreak /      │
        │       Retry / Redo) — may resume or redirect   │
        │   2. push this frame's (loc, sourceinfo, fid)  │  ← incremental
        │      onto err.trace                            │    trace capture
        │   3. consult the frame's exception table:      │
        │      • rescue dest?  → complete backtrace,     │
        │        materialize object, goto rescue         │
        │      • ensure dest?  → defer unwind, goto      │
        │        ensure                                  │
        │      • neither?      → return error to caller  │
        └───────────────────────────────────────────────┘
                                   │ unwind one frame, re-enter handle_error
                                   ▼
                        … up to the top level (main.rs) if never caught
```

`handle_error` runs **once per frame** as the exception unwinds. There is no
separate "raise" bytecode that snapshots the whole stack; the stack is
recorded incrementally, one frame at a time, as control leaves each frame.

### CRuby contrast

CRuby captures the backtrace **eagerly at raise time**
(`rb_ec_setup_exception` → `rb_vm_get_backtrace` walks the whole control-frame
stack and stores it on the exception object) before unwinding starts. That is
simple and makes `#backtrace` a stored-field read, but it pays the full
stack-walk cost on *every* raise — including the many raises that are caught
immediately and whose backtrace is never inspected. monoruby instead pays only
for the frames it actually unwinds through, defers the caller frames to the
catch point, and defers string formatting to `#backtrace`.

---

## 2. `MonorubyErr` — the in-flight error

`MonorubyErr` (`error.rs:9`) is the value held in `Executor.exception` while an
error is propagating. It is a Rust struct, **not** a Ruby object:

| field            | purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| `kind`           | `MonorubyErrKind` — the error class / control-flow tag (see §3)         |
| `message`        | the message string                                                      |
| `trace`          | `Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)>` — the backtrace, built incrementally as cheap tuples (no strings) |
| `original`       | when re-raising an existing exception object (`raise exc`), that `Value`, so identity + ivars survive |
| `explicit_cause` | an explicit `cause:` keyword (`Some(nil)` for `cause: nil`)             |
| `payload`        | kind-specific extra data surfaced as hidden ivars on materialization (e.g. `LocalJumpError#exit_value`, `StopIteration#result`) |

The exception object (`RVALUE` of class `RuntimeError`, etc.) is **not**
created here. It is materialized lazily by `take_ex_obj` (§8) only when a
`rescue` actually binds it or the top level needs to print it. Deferring
`Value` allocation is the first half of the laziness story.

`MonorubyErr::mark` (`error.rs:91`) participates in GC: while an error is in
flight it is not a Ruby object, so the GC cannot reach the `Value`s it smuggles
(`original`, `explicit_cause`, `payload`, and the receiver/tag/value payloads
of a few kinds) through the normal object graph — `mark` roots them explicitly.

---

## 3. Two families of `MonorubyErrKind`

`MonorubyErrKind` (`error.rs:1111` and above) mixes **two conceptually
different things** into one enum, because monoruby routes both through the same
unwinder:

### 3a. Real exceptions (catchable by `rescue`)

`Runtime`, `NotMethod`, `Name`, `Type`, `Index`, `Key`, `Frozen`, `Load`,
`Range`, `DivideByZero`, `StopIteration`, `SystemExit`, `IO`, `Arguments`,
`Syntax`, `Other(ClassId)` (any user-defined subclass), … Each maps to a Ruby
exception class via `from_class_id` (`error.rs:1143`) / a class id, and each
may carry structured data (e.g. `NotMethod { name, receiver }`) that becomes
hidden ivars on the materialized object.

### 3b. Control-flow pseudo-exceptions (NOT ordinary `rescue` targets)

These reuse the unwinding machinery to implement non-local control flow, the
same way CRuby uses its `throw`/catch-table `TAG_*` mechanism:

| kind                       | Ruby construct                              | how it stops unwinding                                             |
| -------------------------- | ------------------------------------------- | ----------------------------------------------------------------- |
| `MethodReturn(val, lfp)`   | `return` from a block/proc/lambda           | stops at the target frame `lfp`                                   |
| `BlockBreak(val, fid, lfp)`| `break` out of a block                      | resumes the block's defining call, or degrades to `LocalJumpError`|
| `Throw(tag, val)`          | `Kernel#throw` / `Kernel#catch`             | intercepted only by a matching `catch`, never by `rescue`         |
| `Retry`                    | `retry` in a rescue clause                  | redirected to the begin-region start                              |
| `Redo`                     | `redo` in a loop                            | redirected to the loop body start                                 |
| `Fatal`                    | a Rust `panic!` caught at an `extern "C"` boundary | uncatchable — propagates straight to the top level         |

The crucial property, exploited for performance, is that **`handle_error`
dispatches every control-flow kind *before* it touches `err.trace`**
(`jit_module.rs:118`–`238`, all ahead of the `push_error_location` at
`jit_module.rs:242`). A `MethodReturn` / `Throw` / `BlockBreak` therefore
never accumulates a backtrace tuple and never materializes an exception object.
`return` from a block and `break` are as cheap as they can be while still
threading through `ensure` bodies correctly.

### CRuby contrast

CRuby likewise implements `return`/`break`/`next`/`redo`/`retry`/`throw` with
its internal tag mechanism rather than real exceptions, and likewise does not
build a Ruby backtrace for them. `Fatal` corresponds to CRuby's `rb_fatal` /
uncatchable `fatal` class. The taxonomy is deliberately parallel; monoruby just
folds it into one Rust enum.

---

## 4. The unwinder: `handle_error`

`handle_error` (`jit_module.rs:85`) is the heart of the mechanism. For the
current frame's `FuncKind`:

**ISeq (Ruby) frames:**

1. **Retry/Redo** (`jit_module.rs:106`) — take the error and `goto` the
   begin-region / loop start encoded in the instruction. No trace, no object.
2. **MethodReturn** (`jit_module.rs:118`) — if this frame is the target `lfp`,
   return the value here; if an `ensure` sits in the way, defer the unwind
   across it; otherwise keep propagating. `$!` is restored from the
   region-entry save on the way out (`restore_errinfo_on_exit`).
3. **Throw** (`jit_module.rs:157`) — run any intervening `ensure`, else keep
   propagating (a matching `Kernel#catch` frame consumes it).
4. **BlockBreak** (`jit_module.rs:173`) — at the block's defining frame, if the
   in-progress call site is the one that received this block, resume it with
   the break value (CRuby's `BREAK` catch table); otherwise degrade to
   `LocalJumpError` ("break from proc-closure").
5. **Incremental trace capture** (`jit_module.rs:242`) —
   `push_error_location(loc, sourceinfo, fid)` appends this frame's cheap
   tuple. Only *real* exceptions reach here.
6. **Fatal** (`jit_module.rs:247`) — never caught; skip `rescue`/`ensure`,
   propagate to the top.
7. **Exception table lookup** (`jit_module.rs:251`) — `get_exception_dest(pc)`
   returns `(rescue_pc, ensure_pc, err_slot)` for the innermost region
   covering `pc`:
   - **rescue** → call `complete_backtrace_for_rescue` (§7), materialize the
     object with `take_ex_obj`, store it into `$!` and the handler's error
     slot, and `goto` the rescue clause.
   - **ensure** → `defer_unwind` (§6) and `goto` the ensure body.
   - **neither** → `return ErrorReturn::return_err()`, unwinding one frame; the
     caller re-enters `handle_error`.

**Builtin (native) frames** (`jit_module.rs:265`): only the control-flow kinds
that can pass through a builtin are handled (`MethodReturn`, `Throw`,
`BlockBreak`); a real exception records an *internal* trace frame
(`push_internal_error_location`, no source location — printed as `<internal>`)
and unwinds. Builtins have no Ruby-level `rescue`.

The exception table itself is built by bytecodegen and stored per method
(`iseq.rs:504`). Entries nest innermost-first, so `get_exception_dest` returns
the tightest enclosing region.

### CRuby contrast

CRuby's unwinder (`vm_exec_handle_exception` / the `catch_table` on each ISEQ)
is structurally the same idea: a per-ISEQ table of `(type, start, end,
cont, sp)` entries scanned as the stack unwinds, with `CATCH_TYPE_RESCUE`,
`ENSURE`, `RETRY`, `BREAK`, `REDO`, `NEXT`. monoruby's `ExceptionMapEntry`
plays the role of a `catch_table` entry; `ErrorReturn::{goto, return_err,
return_normal}` plays the role of CRuby's `THROW_DATA` / continuation.

---

## 5. `$!` (errinfo) and the deferred-unwind stack

`Executor.errinfo` holds Ruby's `$!` — the exception currently being handled —
and is set when a `rescue` catches (`set_errinfo`, `executor.rs:1092`). Because
control can leave a frame while it is *suspended inside a rescue clause* (a
`return`/`break` jumping out mid-handler), the region-entry value of `$!` is
saved into a bytecode slot, and `restore_errinfo_on_exit`
(`jit_module.rs:72`) replays those saves (outermost wins) when such a frame is
exited. `errinfo_restore_slots` (`iseq.rs:543`) enumerates the relevant slots.

---

## 6. `ensure` and deferred unwind

`ensure` complicates unwinding because the ensure body must run *with an empty
error slot* (so it can itself `raise`/`return`), yet the original in-flight
error must be re-raised afterwards unless the body overrides it. monoruby models
this with a **deferred-unwind stack** (`executor.rs:1102`–`1153`):

- `defer_unwind(lfp)` moves the in-flight error out of `exception` and stashes
  it keyed by frame, then `goto`es the ensure body.
- `finish_ensure(lfp)` (the `EnsureEnd` hook) re-raises the deferred error —
  **unless** the ensure body left a new error pending, in which case the new
  one wins (CRuby: a `raise`/`return`/`throw` inside `ensure` supersedes).
- `discard_deferred_unwind(lfp)` drops a deferral when the frame leaves by some
  other path so its `EnsureEnd` will not consume it.

This mirrors CRuby's `CATCH_TYPE_ENSURE` continuation plus the "ensure result
overrides pending throw" rule.

---

## 7. Backtrace construction — the key contrast

This is where the laziness pays off and where the recent work (PR #896)
focused. A backtrace has three cost components, and monoruby defers each:

**(a) The raise→rescue frames.** These are captured incrementally by
`push_error_location` as `handle_error` unwinds each frame (§4 step 5). They
must be captured *during* unwinding because those frames are destroyed as the
stack pops — they cannot be walked later. Cost: one 3-word tuple push per
frame, **no string formatting**.

**(b) The frames *above* the rescuing frame** (the rest of the live stack at
raise time). The incremental capture never sees these, because unwinding stops
at the rescuing frame. CRuby includes them (its eager snapshot walked the whole
stack). monoruby fills them in at the **catch point** with
`Executor::complete_backtrace_for_rescue` (`executor.rs:complete_backtrace_for_rescue`,
called from `jit_module.rs:256` just before `take_ex_obj`):

```rust
// Walk the rescuing frame's callers via each inner frame's saved
// call-site pc — the same mechanism as Kernel#caller — appending the
// cheap (loc, sourceinfo, fid) tuples. No strings; formatting stays lazy.
```

Why the catch point, and not lazily at `#backtrace` time? Because it is the
**last moment the full stack is coherent**: the raise→rescue tuples are already
collected in (a), and the caller frames are still live (we are about to run a
`rescue` clause nested inside them). If we deferred this walk to `#backtrace`,
an exception object that escaped its rescue clause and was inspected later would
find those caller frames gone — yielding a truncated, wrong backtrace. CRuby
avoids the problem by snapshotting everything eagerly at raise; monoruby
snapshots the *caller half* at catch, which is strictly cheaper (only exceptions
that reach a real `rescue` pay for it) while remaining correct.

**(c) String formatting.** Fully deferred to `Exception#backtrace`
(`exception.rs:backtrace`), which turns the tuples into `"file:line:in
'method'"` strings and **memoizes** the resulting Array in the `/backtrace`
hidden ivar, so repeated calls return the *same* mutable object (matching
CRuby's `e.backtrace.equal?(e.backtrace)` and `e.backtrace.unshift(x)`
visibility). `#set_backtrace` writes the same `/backtrace` ivar, unifying the
explicit store with the memo.

`#backtrace_locations` is intentionally decoupled from the string backtrace via
the `__raise_backtrace` intrinsic (raise-time capture only), so
`set_backtrace(strings)` on a never-raised exception keeps
`#backtrace_locations` nil, while an Array of `Thread::Backtrace::Location`
sets both — matching CRuby 3.4+.

### Cost summary for the hot paths

| scenario                              | backtrace cost in monoruby                          |
| ------------------------------------- | --------------------------------------------------- |
| `return` from block, `break`, `throw` | **none** — dispatched before trace capture (§3b)    |
| `StopIteration` caught by `loop`      | a few tuple pushes only — `loop` catches at the Rust level (`err.is_stop_iteration()`, `kernel.rs:908`), so it never hits a bytecode `rescue`, so `complete_backtrace_for_rescue` and `take_ex_obj` are **never called** |
| exception caught by a Ruby `rescue`   | raise→rescue tuples + one caller-stack walk (tuples only); strings only if `#backtrace` is called |
| uncaught exception (top level)        | full tuple trace; formatted once by the reporter    |

### CRuby contrast (backtrace)

- **When captured:** CRuby eagerly at raise; monoruby incrementally on unwind +
  once at catch.
- **What is stored:** CRuby a `rb_backtrace_t` (frame snapshots); monoruby cheap
  `(loc, sourceinfo, fid)` tuples.
- **`#backtrace` strings:** both format lazily and memoize; monoruby in the
  `/backtrace` ivar.
- **Control-flow tags:** neither builds a Ruby backtrace for them.
- **Frame labels:** monoruby renders owners with their fully-qualified name
  (`Ns::Cx.foo`, special-casing `Object#foo`) in `func_description`
  (`../monoruby/src/globals/store.rs`), matching CRuby's `Ns::Cx.foo`.

---

## 8. Materializing the exception object — `take_ex_obj`

`take_ex_obj` (`executor.rs:1174`) converts the in-flight `MonorubyErr` into a
Ruby `Value`, called only at a catch point or the top level:

- **Re-raise** (`err.original` set): return the *same* object, filling its
  `trace` only if still empty (CRuby assigns a backtrace only when the
  exception lacks one).
- **Fresh object:** allocate `Value::new_exception(err)` and attach
  kind-specific hidden ivars — `LoadError#path`, `SystemExit#status`,
  `NoMethodError#{name,receiver}`, `NameError#{name,receiver}`,
  `KeyError#{receiver,key}`, `FrozenError#receiver`, `LocalJumpError#exit_value`
  + `#reason`, `StopIteration#result`, `SyntaxError#path`, … Hidden ivars use
  `/`-prefixed names so they are excluded from `#instance_variables`.
- **Cause chaining** (`chain_cause`, `executor.rs:1335`): an explicit `cause:`
  keyword wins; otherwise CRuby's `exc_setup_cause` — if a different exception
  is currently being handled (`$!`), record it as `/cause`. `cause: nil`
  suppresses the implicit chain.

### CRuby contrast

CRuby builds the exception object at `raise` (it *is* the raise). monoruby's
split — Rust `MonorubyErr` while in flight, Ruby object only at catch — is what
lets it skip object allocation entirely for the immediately-caught and
control-flow cases. The materialized object's ivar layout and cause semantics
are kept CRuby-compatible.

---

## 9. Fatal errors

A Rust `panic!` caught at an `extern "C"` trampoline becomes
`MonorubyErrKind::Fatal`. `is_fatal()` (`error.rs:1137`) makes `handle_error`
skip **both** `rescue` and `ensure` and propagate straight to the top
(`jit_module.rs:247`), because VM/interpreter state may be inconsistent after a
panic. This matches CRuby's uncatchable `fatal` — not interceptable even by
`rescue Exception`.

---

## 10. Top-level reporting

An exception that reaches the top uncaught is printed by the reporter in
`error.rs` (`show_error_message_and_all_loc`, `error.rs:167`): the message line
plus each caller frame as `\tfrom <file>:<line>:in '<method>'`, honouring
`--backtrace-limit=N` (extra frames collapse into `\t ... K levels...`). The
compact single-location form (`show_error_message_and_loc`) is used where CRuby
prints only the origin (e.g. `SyntaxError`, which also gets a source excerpt).

---

## 11. File map

| concern                              | location                                             |
| ------------------------------------ | ---------------------------------------------------- |
| in-flight error type + kinds         | `../monoruby/src/globals/error.rs`                   |
| unwinder                             | `../monoruby/src/codegen/jit_module.rs` (`handle_error`) |
| set/take error, `$!`, ensure defer   | `../monoruby/src/executor.rs`                        |
| catch-time caller walk               | `../monoruby/src/executor.rs` (`complete_backtrace_for_rescue`) |
| object materialization + cause       | `../monoruby/src/executor.rs` (`take_ex_obj`, `chain_cause`) |
| per-method exception table           | `../monoruby/src/globals/store/iseq.rs`              |
| frame-label rendering                | `../monoruby/src/globals/store.rs` (`func_description`) |
| Ruby `Exception` API (Rust side)     | `../monoruby/src/builtins/exception.rs`              |
| Ruby `Exception` API (Ruby side)     | `../monoruby/builtins/startup.rb`                    |
| `Kernel#raise` / `#loop` / `#caller` | `../monoruby/src/builtins/kernel.rs`                 |
| differential tests                   | `../monoruby/tests/backtrace.rs`, `tests/exception_api.rs` |

---

## 12. Design summary

monoruby's exception mechanism is CRuby-compatible at the Ruby surface
(`rescue`/`ensure`/`retry`/`redo`, `Exception` API, cause chaining, backtrace
format, uncatchable fatals) while diverging in **when** work happens:

- **Raise stores the minimum** — a Rust `MonorubyErr` with cheap trace tuples;
  no Ruby object, no formatted strings.
- **The stack is recorded incrementally on unwind**, not snapshotted eagerly.
- **Caller frames are completed once, at the catch point** — the last coherent
  moment — not eagerly at raise and not unsafely late at `#backtrace`.
- **Control-flow constructs pay nothing for backtraces** because they are
  dispatched before trace capture, and internal `StopIteration` (via `loop`) is
  caught at the Rust level below the bytecode-`rescue` path.

The net effect is CRuby-equivalent observable behavior with the backtrace cost
concentrated on exactly the exceptions that are genuinely caught and inspected.
