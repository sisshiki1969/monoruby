monoruby implements **M:1 green threads**: all Ruby `Thread`s are multiplexed onto a single OS thread that runs the VM. There is no parallel Ruby execution (and no GVL — there is simply one VM-running OS thread); short-lived helper OS threads exist only for blocking-syscall offload and the preemption timer, and they never touch the Ruby heap. This page is an overview; the full design document is [`doc/threads.md`](design/threads.md).

## Scheduler

The scheduler (`monoruby/src/scheduler.rs`) is a per-OS-thread singleton. Its event loop runs **on the main thread's stack**: main enters it as an ordinary function call when it parks, and returns from it when main becomes runnable again; green threads never call the loop themselves — they switch into its saved context. The scheduler tracks live threads (a GC root), a FIFO run queue, sleepers with deadlines, and fd waiters. When idle it `poll(2)`s the waited fds or sleeps to the nearest deadline; if no thread can ever run again it raises a fatal deadlock error.

Context switching reuses the Fiber stack-switching machinery (`rsp` exchange): each thread gets its own 256 KiB stack with a guard page and its own `Executor`.

## Cooperative and preemptive switching

Switching is hybrid:

- **Cooperative** — at blocking points: `sleep`, `Thread.stop`, `#join`, `Thread.pass`, blocking IO, and synchronization-primitive waits.
- **Preemptive** — a dedicated timer OS thread ticks every **10 ms** (spawned only while ≥ 2 threads are live) and arms the shared poll flag, which acts as if the running thread called `Thread.pass` at its next safepoint. `MONORUBY_NO_PREEMPT=1` disables it; `MONORUBY_PREEMPT_STRESS=1` switches at every poll site.

Both kinds of switch happen **only at VM safepoints** — the same callee-entry / loop-back-edge polls used by the GC (see [Garbage Collection](garbage-collection.md) and [`doc/safepoint.md`](design/safepoint.md)) — so a suspended thread's frames are always in a GC-complete state. A consequence: Rust builtins are atomic with respect to other threads (like C functions under CRuby's GVL), but sequences of pure-Ruby statements can interleave, so Ruby code must use locks for compound state transitions.

## Blocking IO

Blocking-IO builtins go through a common wrapper that checks buffered data, probes readiness with a zero-timeout poll, and otherwise **parks the thread on the scheduler's fd poller** instead of blocking the process. While other threads are live, fds are temporarily set to non-blocking so that a mid-operation would-block parks and resumes without data loss. `IO.select`, non-blocking TCP `connect`, and `accept` retry-loops are integrated with the same poller. The only truly blocking syscalls (`flock`, FIFO `open`) are offloaded to short-lived native helper threads that signal completion through a self-pipe registered with the poller.

## Synchronization primitives and interrupts

`Mutex`, `Queue`, `SizedQueue`, and `ConditionVariable` are implemented **in Ruby** (in `builtins/startup.rb`), relying on safepoint-free straight-line test-and-set plus a *park permit* mechanism that closes the classic lost-wakeup race. Locks abandoned by a dead thread are reclaimed by the next acquirer, and `Mutex#owned?` is per-Fiber.

`Thread#kill` / `#raise` are queued and delivered by the scheduler: a parked target is woken and unwinds **from its exact blocking point** (running `ensure` blocks); a running target is caught by preemption at its next safepoint, so even busy loops are killable. `Thread.handle_interrupt` masking is honored at mask boundaries.

## Fibers

Threads are built on Fiber's stack switching, but the two remain distinct: Fibers form an asymmetric resume/yield chain within a thread, while the scheduler schedules threads only. A green thread may park while deep inside a nested Fiber and be resumed exactly there. Signal handling uses the same deferred safepoint model — see [`doc/signal.md`](design/signal.md).

## State diagrams

![Thread state transitions](design/thread_state_diagram.svg)

![Fiber state transitions](design/fiber_state_diagram.svg)

## Further reading

- [`doc/threads.md`](design/threads.md) — full design document (scheduler internals, preemption, IO parking, sync primitives, kill/raise delivery)
- [`doc/scheduler_state_diagram.md`](design/scheduler_state_diagram.md) — the state diagrams with commentary
- [`doc/safepoint.md`](design/safepoint.md) — the unified GC / preemption / signal poll mechanism
- [`doc/signal.md`](design/signal.md) — async-signal-safe handlers and deferred delivery
