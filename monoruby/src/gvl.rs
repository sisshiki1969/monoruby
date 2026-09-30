//! The Global VM Lock.
//!
//! In the 1:1 thread model each Ruby `Thread` is a kernel thread, and
//! the interpreter's shared state (the heap, `Globals`, the JIT, the
//! inline caches) is touched only by the thread that holds this lock.
//! The lock is released at exactly the places a green thread switches
//! today — safepoints and blocking regions — so a thread that does not
//! hold it always has GC-complete frames.
//!
//! Fairness matters more than throughput here: a holder that releases
//! and immediately re-acquires would starve everyone else (the classic
//! GVL convoy), so a release hands the lock *directly* to the longest
//! waiter, FIFO, and a yielding holder joins the back of the queue.
//!
//! A single-threaded program pays nothing: while only one thread is
//! registered, [`Gvl::without`] runs its region inline — no lock
//! traffic at all — and the lock changes hands only once a second
//! thread registers. Until the kernel-thread model lands, the `Vm`'s
//! creating thread is the sole registrant and the only holder.

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

/// One kernel thread's handle on the lock. Created by
/// [`Gvl::register`], holds a parking slot the handoff wakes.
pub(crate) struct GvlThread {
    slot: Arc<Slot>,
}

/// A view of a [`GvlThread`]'s queue position, for the thread that
/// spawned it (the handle itself moves to the kernel thread).
pub(crate) struct GvlWaitProbe {
    slot: Arc<Slot>,
}

impl GvlThread {
    pub(crate) fn wait_probe(&self) -> GvlWaitProbe {
        GvlWaitProbe {
            slot: self.slot.clone(),
        }
    }
}

struct Slot {
    /// `true` once a releasing holder handed the lock to this thread.
    granted: Mutex<bool>,
    wake: Condvar,
}

struct State {
    /// Whether some thread holds the lock.
    held: bool,
    /// Threads waiting for the lock, oldest first.
    waiters: VecDeque<Arc<Slot>>,
    /// Threads registered on this lock (the "live" count that decides
    /// whether releasing is worth anything).
    registered: usize,
}

pub(crate) struct Gvl {
    state: Mutex<State>,
}

/// The GVL's state, held across a `fork(2)` by the forking thread
/// ([`Gvl::prepare_fork`]).
pub(crate) struct GvlForkGuard {
    state: std::sync::MutexGuard<'static, State>,
    /// The forking thread's slot: the child's only thread, and its
    /// holder.
    survivor: Arc<Slot>,
}

impl GvlForkGuard {
    /// The child's side: it has exactly one thread, the forking one,
    /// and that thread holds the lock. Forget every other registrant
    /// and waiter (their kernel threads do not exist here) and whatever
    /// grant was pending on the survivor's slot, then release the state
    /// on the thread that took it.
    pub(crate) fn reset_child(self) {
        let GvlForkGuard {
            mut state,
            survivor,
        } = self;
        state.held = true;
        state.waiters.clear();
        state.registered = 1;
        *survivor.granted.lock().unwrap_or_else(|e| e.into_inner()) = false;
    }
}

impl Gvl {
    /// A lock already held by its creator, the `Vm`'s own thread, whose
    /// handle is returned with it.
    pub(crate) fn new() -> (Self, GvlThread) {
        let gvl = Gvl {
            state: Mutex::new(State {
                held: true,
                waiters: VecDeque::new(),
                registered: 1,
            }),
        };
        let main = GvlThread {
            slot: Arc::new(Slot::new()),
        };
        (gvl, main)
    }

    /// Register a new kernel thread. It does not hold the lock yet;
    /// [`Gvl::acquire`] is its first step.
    pub(crate) fn register(&self) -> GvlThread {
        self.state.lock().unwrap().registered += 1;
        GvlThread {
            slot: Arc::new(Slot::new()),
        }
    }

    /// The thread is done for good. Must not hold the lock.
    pub(crate) fn unregister(&self, _th: GvlThread) {
        self.state.lock().unwrap().registered -= 1;
    }

    /// How many threads share this lock.
    pub(crate) fn registered(&self) -> usize {
        self.state.lock().unwrap().registered
    }

    /// Whether `th` is queued for the lock right now. A spawner uses it
    /// to hand a freshly started thread its first slice: once the new
    /// thread is in the queue, a [`Gvl::yield_now`] reaches it.
    pub(crate) fn is_waiting(&self, probe: &GvlWaitProbe) -> bool {
        self.state
            .lock()
            .unwrap()
            .waiters
            .iter()
            .any(|w| Arc::ptr_eq(w, &probe.slot))
    }

    /// Whether any thread is queued for the lock.
    pub(crate) fn has_waiter(&self) -> bool {
        !self.state.lock().unwrap().waiters.is_empty()
    }

    /// Hold the lock's state across a `fork(2)`, on the forking thread
    /// (part of `crate::fork::prepare`). Another kernel thread may be
    /// inside `acquire` / `release` at the instant of the fork, holding
    /// `state`; the child, where that thread does not exist, would then
    /// block forever on its first `state.lock()`. Holding the state here
    /// makes that instant impossible. The parent drops the guard as soon
    /// as the fork returns; the child spends it on
    /// [`GvlForkGuard::reset_child`].
    pub(crate) fn prepare_fork(&'static self, th: &GvlThread) -> GvlForkGuard {
        GvlForkGuard {
            state: self.state.lock().unwrap_or_else(|e| e.into_inner()),
            survivor: th.slot.clone(),
        }
    }

    /// Take the lock, waiting FIFO behind earlier waiters.
    pub(crate) fn acquire(&self, th: &GvlThread) {
        {
            let mut st = self.state.lock().unwrap();
            if !st.held && st.waiters.is_empty() {
                st.held = true;
                drop(st);
                self.on_acquired(th);
                return;
            }
            *th.slot.granted.lock().unwrap() = false;
            st.waiters.push_back(th.slot.clone());
        }
        let mut granted = th.slot.granted.lock().unwrap();
        while !*granted {
            granted = th.slot.wake.wait(granted).unwrap();
        }
        drop(granted);
        self.on_acquired(th);
    }

    /// Give the lock up. If someone is waiting it is theirs at once
    /// (`held` never drops to `false` in between), so the releasing
    /// thread cannot snatch it back ahead of them.
    pub(crate) fn release(&self, _th: &GvlThread) {
        let mut st = self.state.lock().unwrap();
        debug_assert!(st.held);
        match st.waiters.pop_front() {
            Some(next) => {
                drop(st);
                *next.granted.lock().unwrap() = true;
                next.wake.notify_one();
            }
            None => st.held = false,
        }
    }

    /// Let a waiter run, if there is one: hand the lock to the oldest
    /// waiter and rejoin the queue at the back. With no waiter this is
    /// a single lock-and-look and the caller keeps the lock.
    ///
    /// The handoff and the requeue are one step under the state lock,
    /// not a `release` followed by an `acquire`: in between those the
    /// queue would be empty, and the thread just handed the lock could
    /// reach its own `yield_now` before the yielder was back in line,
    /// find nobody waiting and keep running. `Thread.pass` ping-pong
    /// (and everything built on it, such as the eviction of another
    /// thread's compiled frames while it waits here) relies on the
    /// yielder being the next in line the moment the other side runs.
    pub(crate) fn yield_now(&self, th: &GvlThread) {
        let next = {
            let mut st = self.state.lock().unwrap();
            debug_assert!(st.held);
            let Some(next) = st.waiters.pop_front() else {
                return;
            };
            *th.slot.granted.lock().unwrap() = false;
            st.waiters.push_back(th.slot.clone());
            next
        };
        *next.granted.lock().unwrap() = true;
        next.wake.notify_one();
        let mut granted = th.slot.granted.lock().unwrap();
        while !*granted {
            granted = th.slot.wake.wait(granted).unwrap();
        }
        drop(granted);
        self.on_acquired(th);
    }

    /// Run `f` without the lock: the blocking region. `f` must not
    /// touch the heap, `Globals`, or anything else the lock guards.
    ///
    /// With no other thread registered the region runs inline, so the
    /// single-threaded program never touches the lock.
    #[inline]
    pub(crate) fn without<R>(&self, th: &GvlThread, f: impl FnOnce() -> R) -> R {
        if self.registered() == 1 {
            return f();
        }
        self.release(th);
        let r = f();
        self.acquire(th);
        r
    }

    /// The holder that just left may have written or patched JIT code
    /// (a compile, an entry jump reverted by a basic-op redefinition, a
    /// version word re-stamped by a salvage); it flushed on its side.
    /// This side serializes before it can execute any of it. Every
    /// acquire pays the one instruction rather than tracking which
    /// releases followed a write: the handoff itself is a futex wake,
    /// next to which a `cpuid` / `isb` is noise, and there is no list of
    /// code-writing sites to keep complete.
    fn on_acquired(&self, _th: &GvlThread) {
        serialize_instruction_stream();
    }
}

impl Slot {
    fn new() -> Self {
        Slot {
            granted: Mutex::new(false),
            wake: Condvar::new(),
        }
    }
}

/// Make code written by another thread visible to this one's
/// instruction fetch (the cross-modifying-code protocol: the writer
/// flushes, the executor serializes).
#[inline]
fn serialize_instruction_stream() {
    #[cfg(target_arch = "x86_64")]
    {
        // `cpuid` is used purely as a serializing instruction.
        let _ = core::arch::x86_64::__cpuid(0);
    }
    #[cfg(target_arch = "aarch64")]
    // SAFETY: `isb` has no operands and no preconditions.
    unsafe {
        core::arch::asm!("isb", options(nostack, preserves_flags));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::Ordering;
    use std::sync::atomic::AtomicUsize;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn creator_holds_it_and_a_single_thread_runs_regions_inline() {
        let (gvl, main) = Gvl::new();
        assert_eq!(gvl.registered(), 1);
        // No release happens here: the region runs with `held` still true.
        let r = gvl.without(&main, || gvl.state.lock().unwrap().held);
        assert!(r);
        gvl.yield_now(&main);
        assert!(gvl.state.lock().unwrap().held);
    }

    fn shared() -> (Arc<Gvl>, GvlThread) {
        let (gvl, main) = Gvl::new();
        (Arc::new(gvl), main)
    }

    fn waiters(gvl: &Gvl) -> usize {
        gvl.state.lock().unwrap().waiters.len()
    }

    fn wait_for_waiters(gvl: &Gvl, n: usize) {
        while waiters(gvl) < n {
            thread::sleep(Duration::from_millis(1));
        }
    }

    #[test]
    fn a_blocking_region_hands_the_lock_to_a_waiting_thread() {
        let (gvl, main) = shared();
        let ran_in_region = Arc::new(AtomicUsize::new(0));
        let other = {
            let (gvl, ran) = (gvl.clone(), ran_in_region.clone());
            let th = gvl.register();
            thread::spawn(move || {
                gvl.acquire(&th);
                ran.fetch_add(1, Ordering::SeqCst);
                gvl.release(&th);
                gvl.unregister(th);
            })
        };
        wait_for_waiters(&gvl, 1);
        assert_eq!(ran_in_region.load(Ordering::SeqCst), 0);
        // Two registrants now, so the region really releases.
        gvl.without(&main, || {
            other.join().unwrap();
        });
        assert_eq!(ran_in_region.load(Ordering::SeqCst), 1);
        assert_eq!(gvl.registered(), 1);
        assert!(gvl.state.lock().unwrap().held);
    }

    #[test]
    fn waiters_are_served_in_arrival_order() {
        let (gvl, main) = shared();
        let order = Arc::new(Mutex::new(Vec::new()));
        let mut handles = vec![];
        for i in 0..3 {
            let (g, order) = (gvl.clone(), order.clone());
            let th = g.register();
            handles.push(thread::spawn(move || {
                g.acquire(&th);
                order.lock().unwrap().push(i);
                g.release(&th);
                g.unregister(th);
            }));
            // Each joins the queue before the next is spawned.
            wait_for_waiters(&gvl, i + 1);
        }
        gvl.release(&main);
        for h in handles {
            h.join().unwrap();
        }
        assert_eq!(*order.lock().unwrap(), vec![0, 1, 2]);
        gvl.acquire(&main);
    }

    #[test]
    fn yield_lets_the_waiter_run_and_rejoins_behind_it() {
        let (gvl, main) = shared();
        let order = Arc::new(Mutex::new(Vec::new()));
        let other = {
            let (gvl, order) = (gvl.clone(), order.clone());
            let th = gvl.register();
            thread::spawn(move || {
                gvl.acquire(&th);
                order.lock().unwrap().push("other");
                // The yielder is queued behind us by now; hand it back.
                gvl.release(&th);
                gvl.unregister(th);
            })
        };
        wait_for_waiters(&gvl, 1);
        gvl.yield_now(&main);
        order.lock().unwrap().push("main again");
        other.join().unwrap();
        assert_eq!(*order.lock().unwrap(), vec!["other", "main again"]);
    }

    /// A yielder is back in the queue by the time the thread it handed
    /// the lock to runs, so two threads that only ever `yield_now` to
    /// each other alternate strictly: neither side ever finds the queue
    /// empty and keeps the lock.
    #[test]
    fn a_yielder_is_queued_before_the_next_holder_runs() {
        const ROUNDS: usize = 200;
        let (gvl, main) = shared();
        let other = {
            let gvl = gvl.clone();
            let th = gvl.register();
            thread::spawn(move || {
                gvl.acquire(&th);
                for _ in 0..ROUNDS {
                    assert!(gvl.has_waiter(), "the yielder must already be in line");
                    gvl.yield_now(&th);
                }
                gvl.release(&th);
                gvl.unregister(th);
            })
        };
        wait_for_waiters(&gvl, 1);
        for _ in 0..ROUNDS {
            gvl.yield_now(&main);
            // Handed back by the other side's `yield_now`, so it is the
            // one in line now.
            assert!(gvl.has_waiter());
        }
        gvl.release(&main);
        other.join().unwrap();
        gvl.acquire(&main);
        assert_eq!(gvl.registered(), 1);
    }
}
