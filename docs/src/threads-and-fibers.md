monoruby runs each Ruby `Thread` on its own **kernel thread**, under a **GVL** (global VM lock), as CRuby does: only the thread holding the GVL touches the interpreter, so there is no parallel Ruby execution, but a thread that blocks in the kernel (IO, `sleep`, a lock, a blocking C call) releases the GVL and the others keep running. This page is an overview; the full design document is [`doc/threads.md`](design/threads.md).

## Threads and the GVL

`Thread.new` registers the thread in the interpreter's thread registry (`monoruby/src/scheduler.rs`, a GC root) and starts a kernel thread (`monoruby/src/scheduler/native.rs`) that *adopts* the spawner's interpreter instance and takes the GVL (`monoruby/src/gvl.rs`) before running the body. Each thread has its own `Executor` and runs on its own kernel stack.

## Cooperative and preemptive switching

A thread gives the GVL up:

- **At blocking points** — `sleep`, `Thread.stop`, `#join`, `Thread.pass`, blocking IO, and synchronization-primitive waits. A blocked thread *parks*: it releases the GVL and `poll(2)`s its own wake pipe, plus the fds it waits for.
- **Preemptively** — a timer OS thread ticks every **10 ms** (only while ≥ 2 threads are live) and arms the shared poll flag; the running thread then hands the GVL to a waiting thread at its next safepoint. `MONORUBY_NO_PREEMPT=1` disables it; `MONORUBY_PREEMPT_STRESS=1` switches at every poll site.

Every GVL release happens **only at a VM safepoint** — the same callee-entry / loop-back-edge polls used by the GC (see [Garbage Collection](garbage-collection.md) and [`doc/safepoint.md`](design/safepoint.md)) — so a waiting thread's frames are always in a GC-complete state. A consequence: Rust builtins are atomic with respect to other threads (like C functions under CRuby's GVL), but sequences of pure-Ruby statements can interleave, so Ruby code must use locks for compound state transitions.

## Blocking IO and kernel waits

Blocking-IO builtins go through a common wrapper that checks buffered data, probes readiness with a zero-timeout poll, and otherwise **parks the thread on the fd**, releasing the GVL. `IO.select`, non-blocking TCP `connect`, and `accept` retry-loops use the same park. Syscalls that cannot be waited for on an fd (`flock`, a FIFO `open`, `waitpid`, `getaddrinfo`, a blocking FFI or C-extension call) run on the calling kernel thread with the GVL released; an interrupt reaches them through `SIGVTALRM` (`EINTR`).

## Synchronization primitives and interrupts

`Mutex`, `Queue`, `SizedQueue`, and `ConditionVariable` are implemented **in Ruby** (in `builtins/startup.rb`), relying on safepoint-free straight-line test-and-set plus a *park permit* mechanism that closes the classic lost-wakeup race. Locks abandoned by a dead thread are reclaimed by the next acquirer, and `Mutex#owned?` is per-Fiber.

`Thread#kill` / `#raise` are queued on the target: a parked target is woken and unwinds **from its exact blocking point** (running `ensure` blocks); a running target is caught at its next safepoint, so even busy loops are killable. `Thread.handle_interrupt` masking is honored at mask boundaries. Signals are converted on the main thread, as in CRuby — see [`doc/signal.md`](design/signal.md).

## Fibers

Fibers switch stacks within a thread (an asymmetric resume/yield chain); they are unrelated to thread switching, which is the GVL handoff. A thread may park while deep inside a nested Fiber and simply continues there when it wakes.

## State diagrams

![Fiber state transitions](design/fiber_state_diagram.svg)

The thread states are drawn in [`doc/scheduler_state_diagram.md`](design/scheduler_state_diagram.md).

## Further reading

- [`doc/threads.md`](design/threads.md) — full design document (the 1:1 model, preemption, IO parking, sync primitives, kill/raise delivery)
- [`doc/scheduler_state_diagram.md`](design/scheduler_state_diagram.md) — the state diagrams with commentary
- [`doc/safepoint.md`](design/safepoint.md) — the unified GC / preemption / signal poll mechanism
- [`doc/signal.md`](design/signal.md) — async-signal-safe handlers and deferred delivery
