# Signal handling and hang prevention

monoruby runs on a single OS thread with a register-based VM and an
x86-64 JIT. Two consequences shaped this subsystem:

1. POSIX signals must be turned into Ruby-level behaviour (exceptions or
   `Signal.trap` handlers) **without** running Ruby code in
   async-signal context.
2. Several single-threaded-runtime situations would otherwise hang the
   process — and, in CI, the test runner — instead of failing.

This document describes **what is implemented today** and **what is
planned next**.

---

## Current implementation

### Signal capture pipeline

Signals are never serviced inside the OS handler. A tiny
async-signal-safe asm stub only *records* the signal; the real work
happens at the next interpreter *poll point*.

- **Pending-signal bitmap.** `JitModule` holds a 32-bit
  `pending_signals` label — bit `n` corresponds to signal `n + 1` (so
  `SIGINT` = 2 ⇒ bit 1). Writes are an async-safe `OR`; the poll point
  reads-and-clears.
  ([monoruby/src/codegen.rs](../monoruby/src/codegen.rs))
- **Per-signal asm stubs.** `JitModule::signal_handler_for(signo)` emits,
  for each signal:

  ```asm
  addl [rip + alloc_flag], 10        ; force the next allocation to hit the GC poll
  orl  [rip + pending_signals], bit  ; mark this signal pending (async-safe OR)
  ret
  ```

  A stub is pre-generated at `Codegen::new` for **every** signal in
  `signal_table::TRAPPABLE_SIGNALS`, but only the default-install set
  (`POSIX_SIGNALS`, currently just `SIGINT`) is `sigaction(2)`'d at
  startup. The rest are armed on demand by `Signal.trap`, so trapping
  never has to JIT-emit code into a live buffer.
- **Poll point.** `execute_gc` (the allocation-driven GC poll) drains the
  bitmap via `signal_table::lowest_pending_signo` and dispatches the
  lowest-numbered pending signal. This is currently the **only** poll
  point. ([monoruby/src/executor.rs](../monoruby/src/executor.rs))

### Signal → exception mapping

When no user handler is installed, `signal_table::signo_to_error` maps a
signo to the Ruby error raised at the poll point:

| Signal                                            | Mapped class                  |
|---------------------------------------------------|-------------------------------|
| `SIGINT`                                          | `Interrupt` (real class)      |
| `SIGTERM`/`SIGHUP`/`SIGUSR1`/`SIGUSR2`/`SIGQUIT`/`SIGPIPE` | `SignalException` ("SIG…")     |

`Interrupt < SignalException` is a real class
([monoruby/src/builtins/exception.rs](../monoruby/src/builtins/exception.rs)),
so `rescue Interrupt` works. Only `SIGINT` is installed by default:
CRuby's default action for the others is *terminate the process*, and
unconditionally catching them would break
`Process.kill("TERM", child); $?.signaled?` patterns that fork tests
rely on.

### `Signal.trap` / `Kernel#trap`

`Signal.trap(sig, command = nil, &block)` and the private `Kernel#trap`
([monoruby/src/builtins/process.rs](../monoruby/src/builtins/process.rs),
`signal_trap`).

- **Per-signo disposition table.** `Globals::signal_handlers:
  Vec<SignalDisposition>`, with `Handler` objects marked as GC roots in
  `Globals::mark`. `SignalDisposition`
  ([monoruby/src/codegen/signal_table.rs](../monoruby/src/codegen/signal_table.rs))
  is one of:

  | variant                 | OS disposition                       | reported back as              |
  |-------------------------|--------------------------------------|-------------------------------|
  | `Default`               | re-arm stub (if default-installed) else `SIG_DFL` | `"DEFAULT"`        |
  | `SystemDefault`         | `SIG_DFL`                            | `"SYSTEM_DEFAULT"`            |
  | `Ignore { from_nil }`   | `SIG_IGN`                            | `nil` (set via `nil`) / `"IGNORE"` |
  | `Handler(Value)`        | the pre-generated stub               | the callable (by identity)    |

  The initial state is `Default` for the default-installed set (`SIGINT`)
  and `SystemDefault` for every other signal — matching CRuby's initial
  `"DEFAULT"` vs `"SYSTEM_DEFAULT"` reports.
- **Install.** `trap` only `sigaction(2)`s the pre-generated stub:
  `install_signal_stub` / `_ignore` / `_default` / `_system_default`
  flip the OS disposition to stub / `SIG_IGN` / re-arm-or-`SIG_DFL` /
  `SIG_DFL`.
- **Dispatch.** At the poll point a `Handler` is invoked via
  `#call(signo)`, so a `Proc`, `Method`, or any callable works; a
  non-callable raises `NoMethodError` at delivery, per CRuby. `Ignore`
  drops; `Default` / `SystemDefault` fall back to the exception mapping.
- **CRuby-compatible parsing & errors:**
  - **Signal arg:** Integer (validated; `#to_int` is never called),
    String / `#to_str`-able, or Symbol — otherwise
    `ArgumentError: bad signal type <Class>`. Out-of-range Integer ⇒
    `invalid signal number (N)`; unknown name ⇒
    `unsupported signal 'SIG…'`.
  - **Reserved:** `SIGKILL` / `SIGSTOP` ⇒
    `ArgumentError: Signal already used by VM or OS` (CRuby may raise
    `Errno::EINVAL`; the spec accepts either);
    `SIGSEGV` / `SIGBUS` / `SIGILL` / `SIGFPE` / `SIGVTALRM` ⇒
    `can't trap reserved signal: SIG…`. The fault signals are left to the
    kernel's core-dump path on purpose.
  - **Command:** `"DEFAULT"`/`"SIG_DFL"`, `"SYSTEM_DEFAULT"`,
    `"IGNORE"`/`"SIG_IGN"`/`""`/`nil`, or any callable.

**ruby/spec `core/signal`: 47 / 52** (`list` 4/4, `signame` 6/6,
`trap` 37/42).

Coverage: registration / return-value / error semantics as `process.rs`
unit tests; signal *delivery* (Proc/Method/callable handlers,
non-callable `NoMethodError`, signo argument, IGNORE swallows, DEFAULT
re-terminates, SIGINT ⇒ Interrupt) as subprocess-isolated tests in
[monoruby/tests/signal_trap.rs](../monoruby/tests/signal_trap.rs).
Delivery cannot be tested in-process: sending a process-wide signal is
unsafe under cargo's parallel test threads.

### Hang prevention

- **`Thread.pass` is a native `sched_yield(2)`**
  ([monoruby/src/builtins/thread.rs](../monoruby/src/builtins/thread.rs),
  `std::thread::yield_now()`) — returns `nil`, never raises. It replaced
  an artificial "called too many times" guard that aborted `ruby_exe`
  subprocesses and hung `core/file`. `Thread` is a Rust class reopened by
  the pure-Ruby class in `startup.rb` (like `Dir` / `File` / `IO`).
- **Optional hang watchdog**
  ([monoruby/src/watchdog.rs](../monoruby/src/watchdog.rs)) —
  `MONORUBY_HANG_WATCHDOG_SEC=N`, default off. A `SIGALRM` + 1 Hz
  `setitimer` handler decrements a countdown that the poll point resets
  ("progress was made"); on reaching zero it `write(2)`s a message and
  `_exit(134)`s, turning an opaque CI timeout into a fast, labelled
  failure. The handler touches only an atomic, `write`, and `_exit` (all
  async-signal-safe). Because the GC poll is the only poll point, it
  catches hangs that make *no* allocations (the common single-thread
  shapes); an allocating spin resets the countdown and is not caught.
  Coverage: [monoruby/tests/watchdog.rs](../monoruby/tests/watchdog.rs).

---

## Future plans

### Signal handling

- **More poll points.** The GC poll is the only one today, so a signal
  delivered during a tight *non-allocating* loop is seen only once the
  loop allocates — affecting both `Signal.trap` responsiveness and the
  watchdog. A backward-branch (loop-iteration) poll in the VM dispatch,
  and possibly a sampled method-entry poll, would close this gap.
  Deferred until measurements show the GC-only poll is insufficient.
- **Remaining `core/signal` examples (5):**
  - `:EXIT` pseudo-signal — `Signal.trap(:EXIT, …)` must run before
    `at_exit` handlers; needs at-exit integration.
  - Creating a new `Thread` from inside a handler — blocked on the
    single-threaded `Thread` model.
  - `SIGPIPE` `"SYSTEM_DEFAULT"` — CRuby ignores `SIGPIPE` by default
    (write errors surface as `Errno::EPIPE`) and reports the initial
    handler as `nil`; monoruby currently leaves it at `SIG_DFL`.

### Hang prevention

- **Eager `ThreadError` on blocking thread ops.** `Thread#join` /
  `value`, `ConditionVariable#wait`, empty `Queue#pop`, etc. cannot make
  progress on a single thread, so they should raise `ThreadError`
  immediately rather than block forever — converting the remaining
  `core/mutex` / `core/sizedqueue` hangs into honest failures. (An
  alternative, running `Thread.new` synchronously on the caller's stack,
  is recorded but not chosen.)
- **`core/argf` hang.** ARGF reads stdin when ARGV is empty; the hang
  needs diagnosis (monoruby `Kernel#gets` not returning `nil` at tty EOF
  vs a missing `<` redirect in the mspec invocation) before any code
  change.
