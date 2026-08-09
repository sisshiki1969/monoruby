//! The safepoint poll word and its lane protocol.
//!
//! One process-global 32-bit word, living in JIT data memory so the VM and
//! JIT can poll it at every method call and loop back-edge with a single
//! zero test (`cmpl [rip + flag], 0; jne` on x86-64, `ldr; cbnz` on
//! aarch64). Any non-zero value means "reach `execute_gc`"; *what* the
//! poll should do there is encoded in independent byte lanes:
//!
//! | byte | lane      | set by                                   | cleared by |
//! |------|-----------|------------------------------------------|------------|
//! | 0    | `GC`      | arena page pressure, the malloc trigger, `GC.start`, `GC.stress` re-arm | the collection that answers it (`Allocator::ack_gc_request`) |
//! | 1    | `PREEMPT` | the preempt timer (another OS thread), stress re-arm | [`consume_preempt`] at poll entry |
//! | 2    | `SIGNAL`  | the signal handler (async-signal context) | the poll that drains `PENDING_SIGNALS` (main-thread delivery) |
//! | 3    | reserved  |                                          |            |
//!
//! Every writer uses an idempotent atomic op — `fetch_or` to set a lane,
//! `fetch_and` to clear exactly its own byte — so concurrent writers can
//! never lose each other's lanes. (The predecessor design packed a
//! page-fill counter, a `+10` signal nudge, and a preempt bit into one
//! arithmetic `>= 8` band; the signal handler's non-atomic `addl` could
//! race the timer thread's `fetch_or` and drop one of the two updates.)
//!
//! Lane-clear ownership is what makes the protocol compose:
//! - The `GC` lane survives a poll that *defers* its collection (a pending
//!   signal it may not drain — see `execute_gc`), because only a completed
//!   (or `--no-gc`-skipped) collection clears it.
//! - The `SIGNAL` lane survives non-main polls, keeping the word non-zero
//!   until the main thread drains the pending bitmap — this is also what
//!   keeps an allocation-free `nil until flag` spin on main polling.
//! - A signal arriving *during* a collection survives the GC-lane clear
//!   (the old design wiped the whole word at GC end, delaying delivery
//!   until the next unrelated nudge).
//!
//! ## Storage and registration
//!
//! The word itself is `JitModule`'s `poll_flag` data slot (it must be
//! rip-relative-addressable from JIT code, so it cannot be a Rust static).
//! `Codegen::new` registers its address here; `Codegen::drop` unregisters
//! it. Two views exist:
//! - a thread-local address for the owning interpreter thread (the VM,
//!   the allocator, and `execute_gc` all run there);
//! - a process-global address for the signal handler, which can run on
//!   any OS thread. Multiple `Codegen`s (test threads) overwrite it
//!   last-registration-wins, mirroring how `sigaction` itself is
//!   process-wide. The preempt timer keeps its own mutex-guarded copy
//!   (see preempt.rs) because it must exclude teardown, which an atomic
//!   pointer alone cannot.

use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

/// Set-value and whole-byte clear mask of the GC lane (byte 0). The set
/// value doubles as the `data_i32` initializer under `gc-stress`.
pub(crate) const GC_LANE: u32 = 0x0000_0001;
const GC_LANE_MASK: u32 = 0x0000_00ff;
/// Timeslice-preemption lane (byte 1).
pub(crate) const PREEMPT_LANE: u32 = 0x0000_0100;
const PREEMPT_LANE_MASK: u32 = 0x0000_ff00;
/// Pending-signal lane (byte 2).
pub(crate) const SIGNAL_LANE: u32 = 0x0001_0000;
const SIGNAL_LANE_MASK: u32 = 0x00ff_0000;

thread_local! {
    /// This interpreter thread's poll word (0 = none registered), like
    /// `ALLOC` / `CODEGEN`: each test thread runs its own interpreter, and
    /// per-thread addressing keeps one thread's `GC.start` from arming
    /// another thread's word. A `Cell<usize>` with a `const` initializer:
    /// reading it allocates nothing and runs no destructor, because
    /// [`set_gc`] is reached from *inside the global allocator* (the
    /// malloc-threshold trigger), where the non-reentrant `ALLOC` — and
    /// any allocating TLS — is off limits.
    static FLAG_ADDR: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// The poll word the *signal handler* targets. Process-global because
/// `sigaction` is: whichever `Codegen` registered last owns the process's
/// signal handlers, so it also owns this slot.
static SIGNAL_FLAG_ADDR: AtomicUsize = AtomicUsize::new(0);

/// Register the interpreter's poll-word address (from `Codegen::new`).
pub(crate) fn register(addr: *mut u32) {
    FLAG_ADDR.with(|a| a.set(addr as usize));
    SIGNAL_FLAG_ADDR.store(addr as usize, Ordering::Release);
}

/// Signal handlers were just `sigaction`ed to point at this interpreter
/// (e.g. `Signal.trap`): make the handler-side registry follow, so the
/// SIGNAL lane lands in *this* interpreter's word. This mirrors the old
/// per-`Codegen` stub table, where re-arming a signal re-baked the
/// calling interpreter's own flag address — without it, a forked child
/// (or a parallel test interpreter) that traps a signal would have the
/// lane armed on whichever interpreter happened to register last, and
/// its own allocation-free `nil until flag` spin would starve.
pub(crate) fn adopt_signal_registry() {
    let addr = FLAG_ADDR.try_with(|a| a.get()).unwrap_or(0);
    if addr != 0 {
        SIGNAL_FLAG_ADDR.store(addr, Ordering::Release);
    }
}

/// `Codegen` is being dropped: detach `addr` wherever it is still
/// registered, so neither this thread nor the signal handler can write
/// into freed JIT memory. A later registration by another `Codegen` is
/// left untouched (compare-exchange).
pub(crate) fn unregister(addr: *mut u32) {
    // `try_with`: thread-local teardown order is unspecified.
    let _ = FLAG_ADDR.try_with(|a| {
        if a.get() == addr as usize {
            a.set(0);
        }
    });
    let _ = SIGNAL_FLAG_ADDR.compare_exchange(
        addr as usize,
        0,
        Ordering::AcqRel,
        Ordering::Relaxed,
    );
}

/// Run `f` on this thread's poll word, or return `None` if no interpreter
/// is registered on this thread.
fn with_flag<R>(f: impl FnOnce(&AtomicU32) -> R) -> Option<R> {
    let addr = FLAG_ADDR.try_with(|a| a.get()).unwrap_or(0);
    if addr == 0 {
        return None;
    }
    // SAFETY: owning-thread access — the word is JIT data in this
    // thread's `Codegen`, alive for as long as this thread executes
    // Ruby code (unregistered in `Codegen::drop` before it is freed).
    Some(f(unsafe { &*(addr as *const AtomicU32) }))
}

/// Request a collection at the next safepoint.
pub(crate) fn set_gc() {
    with_flag(|f| f.fetch_or(GC_LANE, Ordering::Relaxed));
}

/// A collection answered (or `--no-gc` voided) the pending request.
pub(crate) fn clear_gc() {
    with_flag(|f| f.fetch_and(!GC_LANE_MASK, Ordering::Relaxed));
}

/// Whether a collection is currently requested. Defensive default when no
/// flag is registered on this thread: `true`, so a direct `execute_gc`
/// call still collects (the pre-lane behavior).
pub(crate) fn gc_requested() -> bool {
    with_flag(|f| f.load(Ordering::Relaxed) & GC_LANE_MASK != 0).unwrap_or(true)
}

/// `gc-stress` builds force a collection at every safepoint. This guard
/// re-arms the GC lane on drop, so `execute_gc` leaves the poll word
/// non-zero on every exit path (normal, deferred, or error) and the next
/// poll takes the slow path again.
#[cfg(feature = "gc-stress")]
pub(crate) struct StressRearm;
#[cfg(feature = "gc-stress")]
impl Drop for StressRearm {
    fn drop(&mut self) {
        set_gc();
    }
}

/// Arm the preempt lane from the owning thread (stress mode re-arm; the
/// timer thread writes through its own registered address in preempt.rs).
pub(crate) fn set_preempt() {
    with_flag(|f| f.fetch_or(PREEMPT_LANE, Ordering::Relaxed));
}

/// Consume the preempt lane at poll entry: report whether a timeslice
/// tick was pending and clear it.
pub(crate) fn consume_preempt() -> bool {
    with_flag(|f| {
        if f.load(Ordering::Relaxed) & PREEMPT_LANE_MASK != 0 {
            f.fetch_and(!PREEMPT_LANE_MASK, Ordering::Relaxed);
            true
        } else {
            false
        }
    })
    .unwrap_or(false)
}

/// The poll is about to drain `PENDING_SIGNALS`: clear the signal lane
/// *first*, then drain. A signal landing in between re-sets the lane and
/// its bitmap bit, costing one spurious poll; the reverse order could
/// clear the arming of a bit the drain never saw and lose its wakeup.
pub(crate) fn clear_signal() {
    with_flag(|f| f.fetch_and(!SIGNAL_LANE_MASK, Ordering::Relaxed));
}

/// Arm the signal lane from the signal handler. Async-signal-safe: one
/// relaxed load of the registry and one lock-free `fetch_or`, no locks,
/// no allocation. No-op when no interpreter is registered (the pending
/// bit in `PENDING_SIGNALS` still records the signal).
pub(crate) fn set_signal_from_handler() {
    let addr = SIGNAL_FLAG_ADDR.load(Ordering::Acquire);
    if addr != 0 {
        // SAFETY: non-zero only between a `Codegen`'s registration and
        // its `unregister` in `Codegen::drop`, so the word is live JIT
        // data. (A handler racing the drop itself is a pre-existing
        // teardown hazard the old asm stubs had in a worse form: they
        // *executed from* the freed JIT buffer.)
        unsafe { &*(addr as *const AtomicU32) }.fetch_or(SIGNAL_LANE, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Lane ops on a locally registered word: each lane sets and clears
    /// only its own byte, and the zero test is exactly "any lane set".
    /// (The handler-side path targets the process-global registry, which
    /// other test threads' interpreters also use, so it is exercised via
    /// the word directly here.)
    #[test]
    fn lanes_are_independent() {
        let word = AtomicU32::new(0);
        register(word.as_ptr());
        set_gc();
        set_preempt();
        assert!(gc_requested());
        assert!(consume_preempt());
        // The preempt consume left the GC lane alone.
        assert!(gc_requested());
        assert!(!consume_preempt());
        clear_gc();
        assert!(!gc_requested());
        word.fetch_or(SIGNAL_LANE, Ordering::Relaxed);
        assert_ne!(word.load(Ordering::Relaxed), 0);
        clear_signal();
        assert_eq!(word.load(Ordering::Relaxed), 0);
        unregister(word.as_ptr());
        // Unregistered defensive defaults.
        assert!(gc_requested());
        assert!(!consume_preempt());
    }
}
