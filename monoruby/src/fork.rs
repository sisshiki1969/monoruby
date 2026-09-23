//! What a `fork(2)` has to do about the process's other OS threads.
//!
//! The child of a fork has one thread: the one that called `fork`. Every
//! lock some *other* thread was holding at that instant is, in the
//! child, held by nobody and released by nobody, and the child blocks
//! the first time it takes it. monoruby's own threads are green, so the
//! interpreter thread never contends with itself — but the process has
//! other OS threads (the native offload workers, the preempt timer), and
//! an embedder may run several interpreters on several OS threads over
//! the process-global state (the test harness does, one per test
//! thread). The identifier table is what one of them was holding when
//! a `forking_while_a_thread_keeps_offloading` child hung in
//! `File.open`, in `IdentId::get_id`.
//!
//! This is the `pthread_atfork` prepare / parent / child dance for every
//! process-global lock the interpreter can hold, written out in one
//! place: [`prepare`] takes them all on the forking thread, before the
//! fork, so each is owned in both processes by a thread that exists
//! there; the parent drops the guards; the child spends them on
//! [`ForkGuards::reset_child`], which also forgets the state that
//! belonged to threads it does not have. The lock order is fixed here
//! and never nested elsewhere (no path holds one of these while taking
//! another), so the forking thread cannot deadlock against anyone.
//!
//! Every `fork(2)` in the tree goes through this — `Process._fork`,
//! `Process.daemon` and `Process.spawn`'s exec child alike — so a lock
//! added to the set is held across all of them.
//!
//! One thing the child must *not* do is unlock an inherited `RwLock`.
//! On Linux std's `RwLock` is a futex word and unlocking it in the child
//! is a store plus a wake that finds nobody; on Apple it is the queue
//! lock (`std::sys::sync::rwlock::queue`), whose unlock walks the nodes
//! the parent's waiting threads left on their stacks and unparks each
//! through its `dispatch_semaphore` — and libdispatch traps in the
//! child of a multi-threaded process (`SIGTRAP`, seen on the darwin
//! runner the first time a waiter was queued at the fork). So the
//! `RwLock`s in the set are [`ForkableRwLock`]s: the child moves the
//! data out through the guard it owns, forgets the guard, and installs
//! a fresh lock — the inherited one is leaked, locked, never touched
//! again, as `preempt::ForkState` leaves the timer's mutex. A `Mutex`
//! (the streams', the pool's) is pthread's on Apple and a futex on
//! Linux; its owner unlocking it in the child is an ordinary unlock
//! that wakes nobody, so those guards are simply dropped.

use crate::rvalue::{io, regexp};
use crate::{id_table, native_pool, preempt};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{LockResult, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// A process-global `RwLock` that a forked child can replace instead of
/// unlocking — see the module doc. Reads and writes go through one
/// atomic pointer load; the lock is created on first use with `init`
/// (a CAS race, no `Once`, so a thread mid-initialization at the fork
/// cannot wedge the child either).
pub(crate) struct ForkableRwLock<T: 'static> {
    lock: AtomicPtr<RwLock<T>>,
    init: fn() -> T,
}

impl<T: 'static> ForkableRwLock<T> {
    pub(crate) const fn new(init: fn() -> T) -> Self {
        Self {
            lock: AtomicPtr::new(std::ptr::null_mut()),
            init,
        }
    }

    fn get(&self) -> &RwLock<T> {
        let p = self.lock.load(Ordering::Acquire);
        if !p.is_null() {
            // SAFETY: every pointer stored here is a leaked `Box` that is
            // never freed.
            return unsafe { &*p };
        }
        let fresh = Box::into_raw(Box::new(RwLock::new((self.init)())));
        match self.lock.compare_exchange(
            std::ptr::null_mut(),
            fresh,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            // SAFETY: as above.
            Ok(_) => unsafe { &*fresh },
            Err(winner) => {
                // SAFETY: `fresh` was never published.
                drop(unsafe { Box::from_raw(fresh) });
                // SAFETY: as above.
                unsafe { &*winner }
            }
        }
    }

    pub(crate) fn read(&self) -> LockResult<RwLockReadGuard<'_, T>> {
        self.get().read()
    }

    pub(crate) fn write(&self) -> LockResult<RwLockWriteGuard<'_, T>> {
        self.get().write()
    }

    /// Take the lock for a `fork(2)`, on the forking thread. The parent
    /// drops the guard; the child spends it on [`ForkGuard::reset_child`].
    /// A poisoned lock is taken anyway: the data behind it is what the
    /// child moves out, and leaving the child wedged helps nobody.
    pub(crate) fn prepare_fork(&'static self) -> ForkGuard<T> {
        ForkGuard {
            lock: self,
            guard: self.get().write().unwrap_or_else(|e| e.into_inner()),
        }
    }
}

/// A [`ForkableRwLock`]'s write guard held across a `fork(2)`.
pub(crate) struct ForkGuard<T: 'static> {
    lock: &'static ForkableRwLock<T>,
    guard: RwLockWriteGuard<'static, T>,
}

impl<T: Default + 'static> ForkGuard<T> {
    /// The child's side: carry the data over into a fresh lock and never
    /// touch the inherited one again (it stays locked, and leaks).
    pub(crate) fn reset_child(self) {
        let ForkGuard { lock, mut guard } = self;
        let data = std::mem::take(&mut *guard);
        std::mem::forget(guard);
        let fresh = Box::into_raw(Box::new(RwLock::new(data)));
        lock.lock.store(fresh, Ordering::Release);
    }
}

/// Every guard [`prepare`] took, plus the preempt timer's snapshot. Drop
/// it in the parent as soon as the fork returns; spend it on
/// [`reset_child`](Self::reset_child) in the child. Nothing that takes
/// one of these locks may run in between.
pub(crate) struct ForkGuards {
    pool: native_pool::ForkLocks,
    idents: id_table::ForkLock,
    regexps: regexp::ForkLocks,
    _streams: io::ForkLocks,
    preempt: preempt::ForkState,
}

/// Quiesce the process for a `fork(2)` — see the module doc. Call it
/// after the standard streams are flushed (the flush takes their locks)
/// and immediately before `libc::fork`.
pub(crate) fn prepare() -> ForkGuards {
    // The pool first: its own lock order is pool, orphans, results, and a
    // worker holds nothing else while it holds one of those.
    let pool = native_pool::prepare_fork();
    let idents = id_table::prepare_fork();
    let regexps = regexp::prepare_fork();
    let streams = io::prepare_fork();
    // Not a lock: the timer thread is not quiesced, it just ticks, so
    // the child abandons its lock instead (`preempt::ForkState`).
    let preempt = preempt::prepare_fork();
    ForkGuards {
        pool,
        idents,
        regexps,
        _streams: streams,
        preempt,
    }
}

impl ForkGuards {
    /// The child's side: forget the pool and timer state inherited from
    /// threads the child does not have, replace the `RwLock`s, and
    /// release the mutexes.
    pub(crate) fn reset_child(self) {
        let ForkGuards {
            pool,
            idents,
            regexps,
            _streams,
            preempt,
        } = self;
        pool.reset_child();
        preempt.reset_child();
        idents.reset_child();
        regexps.reset_child();
        // `_streams` unlock here, on the thread that took them.
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id_table::IdentId;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    /// The child's view of a [`ForkableRwLock`]: the data survives,
    /// a fresh lock holds it, and the inherited lock is left exactly as
    /// it was — write-locked, never unlocked (which is what traps on
    /// Apple when a waiter is queued on it).
    #[test]
    fn a_reset_forkable_rwlock_keeps_its_data_and_never_unlocks_the_old_one() {
        static L: ForkableRwLock<Vec<u32>> = ForkableRwLock::new(Vec::new);
        L.write().unwrap().push(7);
        let old = L.get() as *const RwLock<Vec<u32>>;
        L.prepare_fork().reset_child();
        let fresh = L.get() as *const RwLock<Vec<u32>>;
        assert!(!std::ptr::eq(old, fresh), "the child kept the inherited lock");
        assert_eq!(*L.read().unwrap(), vec![7]);
        L.write().unwrap().push(8);
        assert_eq!(*L.read().unwrap(), vec![7, 8]);
        // SAFETY: the leaked lock is never freed.
        let old = unsafe { &*old };
        assert!(old.try_read().is_err(), "the inherited lock was unlocked");
        // The parent's view: dropping the guard unlocks the same lock.
        drop(L.prepare_fork());
        assert!(std::ptr::eq(fresh, L.get()));
        assert_eq!(L.read().unwrap().len(), 2);
    }

    /// The parent's view: dropping what `prepare` took releases every
    /// lock in the set, on the thread that took them. (The child's view,
    /// `reset_child`, is only ever run in a forked child — in this
    /// process it would leave the inherited locks locked under the other
    /// tests' threads — so it is covered by the fork below and by the
    /// `ForkableRwLock` test above.)
    #[test]
    fn dropping_the_guards_releases_every_lock() {
        drop(prepare());
        assert_eq!(IdentId::get_id("fork_reset_probe"), IdentId::get_id("fork_reset_probe"));
        assert!(
            crate::rvalue::RegexpInner::with_option("fork_reset_probe", 0).is_ok(),
            "the regexp caches are still locked"
        );
        io::flush_std_streams();
        drop(prepare());
    }

    /// The hazard itself: another OS thread interning names without
    /// pause while this one forks, over and over, each child interning
    /// one name of its own and exiting. Without [`prepare`] taking the
    /// table's lock, a fork that lands while the churn thread holds it
    /// leaves the child blocked in `get_id` forever; here that is a
    /// child that has not exited within the deadline, which the test
    /// kills and reports rather than hanging the suite (as the real one
    /// did: three `cargo test` runs queued behind one wedged child).
    #[test]
    fn a_forked_child_can_intern_while_another_thread_interns() {
        let stop = Arc::new(AtomicBool::new(false));
        let churn = {
            let stop = stop.clone();
            std::thread::spawn(move || {
                // `get_id_from_string` takes the write lock on every
                // call, hit or miss, so cycling through a few names
                // hammers the lock without growing the table.
                let mut i = 0usize;
                while !stop.load(Ordering::Relaxed) {
                    IdentId::get_id_from_string(format!("fork_churn_{}", i % 64));
                    i = i.wrapping_add(1);
                }
            })
        };
        let deadline = std::time::Duration::from_secs(10);
        for round in 0..200 {
            let guards = prepare();
            // SAFETY: fork(2); the child only interns one name and
            // `_exit`s, the parent waits for it.
            let pid = unsafe { libc::fork() };
            assert!(pid >= 0, "fork failed: {}", std::io::Error::last_os_error());
            if pid == 0 {
                guards.reset_child();
                let _ = IdentId::get_id("fork_child_probe");
                // SAFETY: a forked child of a test process; `_exit` skips
                // the atexit handlers that belong to the parent.
                unsafe { libc::_exit(0) };
            }
            drop(guards);
            let started = std::time::Instant::now();
            let mut status = 0;
            loop {
                // SAFETY: waitpid on the child forked above.
                let r = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) };
                if r == pid {
                    break;
                }
                assert!(r == 0, "waitpid: {}", std::io::Error::last_os_error());
                if started.elapsed() > deadline {
                    // SAFETY: our own child.
                    unsafe {
                        libc::kill(pid, libc::SIGKILL);
                        libc::waitpid(pid, &mut status, 0);
                    }
                    stop.store(true, Ordering::Relaxed);
                    let _ = churn.join();
                    panic!("round {round}: the child never got past get_id (a lock it inherited held)");
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            assert!(
                libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                "round {round}: child status {status:#x}"
            );
        }
        stop.store(true, Ordering::Relaxed);
        churn.join().unwrap();
    }
}
