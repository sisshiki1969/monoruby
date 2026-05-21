# Signal handling and hang prevention

Plan for extending monoruby's runtime so that:

1. Signals beyond SIGINT are caught and surfaced as Ruby exceptions.
2. Common single-threaded-runtime hangs (mutex / sizedqueue, argf, file)
   no longer terminate the test process.

Tracked in this document; updated as items land.

---

## Current architecture (baseline)

- A single AOT-generated asm stub is registered for `SIGINT` only in
  [monoruby/src/codegen.rs:892](../monoruby/src/codegen.rs#L892).
  The stub does:

  ```asm
  addl [rip + alloc_flag], 10   ; nudge GC poll
  movl [rip + sigint_flag], 1   ; mark SIGINT pending
  ret
  ```

- Polling happens only inside `execute_gc`
  ([monoruby/src/executor.rs:2557](../monoruby/src/executor.rs#L2557)).
  If the flag is set, the executor's error slot is filled with
  `MonorubyErr::runtimeerr("Interrupt")` — a plain RuntimeError, **not**
  a real `Interrupt`. `Interrupt` as a class does not exist yet
  (`SignalException` does, in
  [monoruby/src/builtins/exception.rs:42](../monoruby/src/builtins/exception.rs#L42)).
- There is no `Signal.trap` / `Kernel#trap`. Only `Signal.list` and
  `Signal.signame` exist.

## A. Signal handling generalization

### A1. Pending-signal bitmap

Replace the single-purpose `sigint_flag: i32` with a 32-bit
`pending_signals: i32` bitmap. Bit `n` corresponds to signal `n + 1`.
Reads/writes use `OR` / `XCHG`, both async-signal-safe.

### A2. Per-signal asm stubs

Generalize `signal_handler` in
[monoruby/src/codegen.rs:248](../monoruby/src/codegen.rs#L248) to a
factory: for each supported signal, emit a tiny stub that sets its own
bit and nudges `alloc_flag`. `Codegen::new` iterates the supported list
and `sigaction`s each.

### A3. Registered set

The runtime *infrastructure* (bitmap, mapping table, async-safe stub
emitter) supports any of the catchable POSIX signals. What is
unconditionally installed at startup is a smaller set, because the
CRuby default for most signals is "terminate the process", not "raise":

| Signal                                                    | Default install | Mapped class                       |
|-----------------------------------------------------------|-----------------|------------------------------------|
| SIGINT                                                    | **yes**         | `Interrupt`                         |
| SIGTERM, SIGHUP, SIGUSR1, SIGUSR2, SIGQUIT, SIGPIPE       | only via A7     | `SignalException` ("SIG…")          |
| **Not catchable**: SIGKILL, SIGSTOP                       | —               | —                                   |
| **Left to kernel**: SIGSEGV, SIGBUS, SIGFPE, SIGILL, SIGABRT | —            | (core-dump on genuine bugs)         |

Unconditionally catching SIGTERM/HUP/etc would break process-status
tests that rely on `Process.kill("TERM", child); $?.signaled? == true`
(CRuby's default is "kernel terminates the child", and `wait` reports
the signal — not an exception). Once A7 (`Signal.trap`) is implemented,
the runtime can `sigaction` the additional signals on first trap and
deinstall on `:DEFAULT`. The mapping table is already pre-wired so A7
only needs to flip the install bit.

### A4. `Interrupt` class

Add `Interrupt < SignalException` so the existing SIGINT path raises the
right class. Currently it raises `RuntimeError` with the message
`"Interrupt"`, which silently passes most user code but breaks
`rescue Interrupt` and ruby/spec checks.

### A5. Polling frequency

Today only `execute_gc` polls. A tight CPU loop with no allocations
never sees the flag. Candidates for additional poll sites:

- Backward branch (loop iteration) in the VM dispatch table.
- Method entry, sampled (e.g. once every 64 calls).

Defer until measurements show the GC-only poll is insufficient.

### A6. Signal → exception mapping table

A small Rust const table drives the conversion at poll time:

```rust
static SIGNAL_TABLE: &[(c_int, &str, fn() -> MonorubyErr)] = &[
    (SIGINT,  "INT",  || MonorubyErr::interrupt("Interrupt")),
    (SIGTERM, "TERM", || MonorubyErr::signalexception("SIGTERM")),
    // …
];
```

The poller walks the bitmap, clears the bit, looks up the mapping, sets
the error on the executor. Priority is bit order — lowest signo first.

### A7. `Signal.trap` (deferred)

Out of scope for the first cut. Sketch:

- `Globals` carries `Vec<Option<Value>>` of length 32, indexed by signo.
- Poll-time conversion checks the slot first; if a Proc is present,
  invoke it; otherwise fall back to A6's default mapping.
- `Signal.trap("INT") { ... }` writes the slot. `Signal.trap("INT", "DEFAULT")`
  clears it. `Signal.trap("INT", "IGNORE")` installs a sentinel that swallows.

---

## B. Hang prevention

### B1. `core/mutex`, `core/sizedqueue` (single-thread-incompatible)

Root cause: `Thread.new` returns a Thread that never gets scheduled
because monoruby is single-threaded, so `join` / `value` / mutex+CV
patterns block forever.

Two approaches, pick one:

- **Run-block-synchronously**: `Thread.new { … }` runs the block on the
  caller's stack to completion before returning the Thread. `join` /
  `value` then return immediately. Closely matches what most tests
  expect under cooperative concurrency. Risk: tests that depend on
  *concurrent* visibility of mutated state will still fail, but at
  least they won't hang.
- **Eager ThreadError**: any blocking thread op (`Thread#join`,
  `ConditionVariable#wait`, `Queue#pop` empty, …) raises
  `ThreadError("can't block — monoruby is single-threaded")` immediately.
  Safer, more honest; converts hangs to failures.

Recommendation: start with eager ThreadError (cheaper, no semantic
surprises). Revisit run-synchronously if too many tests would benefit.

### B2. `core/argf`

ARGF reads stdin when ARGV is empty. mspec may not redirect stdin;
the parent shell may leave it open to the tty. Investigation needed:

- Identify the spec file that hangs (the survey output stops at a
  specific dotted position).
- Determine whether the underlying issue is monoruby-specific
  (`Kernel#gets` not returning nil at EOF when stdin is the terminal)
  or a missing `<` redirect in the mspec invocation.

No code change planned without that diagnosis.

### B3. `core/file` — **done**

Caused by a subprocess (`ruby_exe` in mspec) hitting the
`Thread.pass called too many times` guard. The pure-Ruby `Thread.pass`
counted its calls and raised `ThreadError` after 1000, so the subprocess
aborted and the parent waited on its pipe forever.

Resolved via option 1: the artificial guard is gone and `Thread.pass`
is now a native `sched_yield(2)` wrapper
([monoruby/src/builtins/thread.rs](../monoruby/src/builtins/thread.rs),
`std::thread::yield_now()`). It returns nil and never raises — a cheap,
legitimate no-op when nothing else is runnable. `Thread` is created in
Rust (subclass of `Object`) and reopened by the pure-Ruby class in
`startup.rb`, matching how `Dir` / `File` / `IO` are split.

Option 2 (raising `ThreadError` eagerly at genuinely-blocking call sites)
is tracked separately as B1 / #3.

### B+. Watchdog (safety net) — **done**

Optional environment toggle `MONORUBY_HANG_WATCHDOG_SEC=N`. After N
seconds of no interpreter progress, abort the process with a clear
message. Default off. Catches unknown hangs in CI and converts them to
errors instead of opaque timeouts.

Implemented in
[monoruby/src/watchdog.rs](../monoruby/src/watchdog.rs):

- `init` (called once from `Globals::new`) reads the env var; when
  `N > 0` it installs a `SIGALRM` handler and a 1 Hz `setitimer`. Unset
  / `<= 0` ⇒ no syscalls installed, fully disabled.
- A `COUNTDOWN` (seconds) is reset to `N` at the VM poll point
  (`execute_gc`, via `watchdog::poll`) — i.e. "progress was made".
- The `SIGALRM` handler decrements `COUNTDOWN` once per second and, on
  reaching zero, `write(2)`s a message and `_exit(134)`s. The abort
  decision lives in the handler, *not* the poll point, because a genuine
  hang never reaches the poll point. The handler touches only an atomic,
  `write` and `_exit`, all async-signal-safe.

**Limitation:** the only poll point today is the allocation-driven GC
poll, so the watchdog catches hangs that make *no* heap allocations
(tight non-allocating loops, `sleep`/`Thread.pass`-until-flag spins —
the common single-thread hang shapes). A spin loop that keeps
allocating resets the countdown and is *not* caught. Adding the A5
backward-branch poll point would close that gap; until then a watchdog
budget should be set comfortably above the slowest legitimate
allocating example.

Coverage: [monoruby/tests/watchdog.rs](../monoruby/tests/watchdog.rs)
(fires on a non-allocating hang; silent when unset; no false-fire when
the program finishes within budget).

---

## Recommended order

| # | Item                                                          | Size      | Outcome                                                                       |
|---|---------------------------------------------------------------|-----------|-------------------------------------------------------------------------------|
| 1 | A1–A4 + A6 (signal generalization + Interrupt class)          | ~150 LoC  | `core/signal` and `core/exception` stop killing the process                   |
| 2 | B3 (`Thread.pass` guard revisit)                              | small     | `core/file` hang clears                                                       |
| 3 | B1 (eager `ThreadError` on blocking thread ops)               | medium    | `core/mutex`, `core/sizedqueue` complete (with failures, not hangs)           |
| 4 | B+ (watchdog)                                                 | small     | Safety net for the long tail                                                  |
| 5 | A5 (extra poll points), B2 (argf), A7 (`Signal.trap`)         | as needed | Responsiveness, remaining argf hang, full user API                            |

---

## Status

- [x] #1 Signal generalization infrastructure + `Interrupt` class
  (default install limited to SIGINT — see A3 above)
- [x] #2 `Thread.pass` guard removed; now a native `sched_yield(2)`
  wrapper ([monoruby/src/builtins/thread.rs](../monoruby/src/builtins/thread.rs))
- [ ] #3 Eager `ThreadError`
- [x] #4 Watchdog — `MONORUBY_HANG_WATCHDOG_SEC=N`, SIGALRM-driven,
  default off ([monoruby/src/watchdog.rs](../monoruby/src/watchdog.rs))
- [ ] #5 Stretch items
