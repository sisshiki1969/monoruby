# Timeslice preemption (Phase 1)

monoruby's green threads are M:1 and historically switched only at
explicit blocking points inside builtins (`sleep`, `join`, IO, `pass`).
Phase 1 adds **timer-driven preemption at safepoints**, keeping the
single-OS-thread model: fairness problems (busy loops starving other
threads, undeliverable `Thread#kill`, mspec's `--timeout` watchdog
thread never running) are solved without touching the GC, the JIT
backends, or the thread-local singletons.

Kernel-blocking syscalls (flock(2), FIFO `open(2)`) are *not* addressed
by preemption; that is Phase 2 (native worker offload), tracked
separately.

## How it works

Preemption is exactly **"as if every thread called `Thread.pass` at its
next safepoint"** — it reuses the existing cooperative switching
machinery unchanged.

1. A dedicated timer OS thread (10 ms tick, `preempt.rs`) runs while
   two or more green threads are alive. Each tick ORs `PREEMPT_BIT`
   (bit 30) into the interpreter's GC poll flag.
2. The VM and JIT poll that flag (`>= 8` → `execute_gc`) at every
   **callee entry** (`vm_init` / JIT `InitMethod`, i.e. inside the new
   frame right after the prologue) and at every loop back-edge, so the
   running thread soon reaches `execute_gc` — with its live registers
   written back, exactly as for a GC.
3. `execute_gc` strips the bit (`preempt::consume_poll_flag`) and calls
   `scheduler::pass`: the main thread dispatches the ready queue and
   continues; a green thread re-enqueues itself and switches back to
   the scheduler. Pending interrupts (`Thread#kill` / `#raise`) are
   delivered through the same path, so they now reach busy-looping
   threads.
4. A collection runs only when the flag's *base* value (bit stripped)
   is in the `>= 8` trigger band — a pure preempt tick never causes a
   spurious full GC, and page-fill accumulation below the band is
   preserved.

Poll placement is **callee-entry + loop back-edge** (the classic
JVM/CRuby scheme), not call-site. The entry poll sits inside the callee
frame right after the prologue — frame linked, `rsp` below it, args
homed into rooted slots, remaining registers nil-filled — so the GC
root scan sees a fully consistent frame, and it fires on *every*
dispatch path uniformly, including the Rust invokers
(`invoke_method` / `invoke_block`), which must never poll on the caller
side: Rust callers hold unrooted heap `Value`s in locals across those
calls (a caller-side poll in generic `invoke_block` corrupted
`File.open {}`, whose receiver lives only in a Rust local after the
block returns). That entry poll is what makes Rust-side iteration
builtins (`Kernel#loop`, `Array#each`, `invoke_block_iter1`-style
helpers) preemptible and signal-responsive per iteration even when the
block body itself is poll-free — no per-builtin audits needed. Call
sites keep only the stack-overflow check (`CheckStack` /
`vm_check_stack`); native (non-Ruby) callees have no entry poll, but
any unbounded execution passes a loop back-edge or a Ruby-frame entry,
so the poll-to-poll distance stays bounded.

`scheduler.rs` gained a `machinery` marker: preemption is suppressed
while the scheduler's own machinery (dispatch loop, fd polling) runs in
the main context, and enabled exactly while a dispatched thread's Ruby
code runs. `pass` also promotes due sleepers now, so a green thread's
expired `sleep` is noticed even if main never parks.

## Why safepoint-granularity switching is safe (and CRuby-compatible)

- Switches happen only where a GC could already happen, with the same
  register write-back; every suspended frame is GC-complete.
- Builtins are atomic with respect to other threads (no switch inside a
  builtin except at its own blocking/poll points) — the same guarantee
  CRuby gives C functions under the GVL.
- JIT register caching of locals cannot leak stale values across
  threads: another thread can only reach a frame's locals through a
  captured (heap-promoted) frame, and the JIT already forces captured
  locals into stack slots — block-passing call sites write back and
  unlink all locals (`locals_to_S`), loop-tier compilation guards on
  the frame being uncaptured, and outer-variable specialization is
  guarded by `no_capture_guard`. Uncaptured frames are unreachable from
  other threads, so their register caches are unobservable.

## Flag protocol

The poll flag is one `u32` in JIT memory with several writers:

| writer                    | operation            |
|---------------------------|----------------------|
| RValue arena (page fill)  | `+= 1`               |
| signal stubs              | `+= 10`              |
| malloc trigger, `GC.start`| lift into `>= 8` band|
| preempt timer             | `|= 1 << 30`         |

Bit 30, not 31: the x86-64 poll is `cmpl ...; jge` — a *signed*
compare, so bit 31 would read as negative and never fire. All flag
accesses are atomic now (the timer writes from another OS thread).

The timer's write is guarded by a mutex around the flag address;
`Codegen::drop` clears the address under the same mutex, so the timer
can never write into freed JIT memory (relevant for the multi-interpreter
test harness).

## Switches

- `MONORUBY_NO_PREEMPT=1` — never start the timer (cooperative-only).
- `MONORUBY_PREEMPT_STRESS=1` — every poll-site visit attempts a
  switch (deterministic torture mode, the scheduling analog of
  `gc-stress`); used to shake out latent "unexpected switch here"
  state bugs.

## Synchronization primitives under preemption

The pure-Ruby Mutex / ConditionVariable / Queue in `builtins/startup.rb`
were written against the cooperative model ("no switch between two
non-blocking statements"). Preemption invalidates that; they are kept
correct by two mechanisms (see the comment block in startup.rb):

1. Test-and-set sequences are safepoint-free straight-line code (calls
   like `Thread.current` hoisted before the test — `Mutex#try_lock`).
2. A `Thread#wakeup` that lands on a *running* thread arms its **park
   permit** (`ThreadInner::park_permit`): the target's next park returns
   immediately, closing every "register as waiter → (preempted; waker
   runs) → park forever" lost-wakeup window. All park sites sit in
   retry loops, so the early return is re-checked.

Known follow-up: `Queue#pop`'s `@items.empty?` / `@items.shift` pair has
a safepoint between the two calls, so two poppers racing can make one
return nil as if an element were nil. Restructuring Queue on top of the
(now preemption-safe) Mutex + ConditionVariable, as CRuby layers it,
would close this; not yet observed in ruby/spec stress runs.

## Phase 2 (planned): blocking-syscall offload

A small native worker pool for kernel-blocking operations (flock,
FIFO open, `fcntl(F_SETLKW)`, …): the builtin packages the op (fds and
raw bytes only — no heap references), parks the green thread, and the
worker signals completion through an eventfd registered with the
scheduler's fd poller. This removes the remaining class of
whole-process hangs and the associated spec tags.
