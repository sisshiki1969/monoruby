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

use crate::rvalue::{io, regexp};
use crate::{id_table, native_pool, preempt};

/// Every guard [`prepare`] took, plus the preempt timer's snapshot. Drop
/// it in the parent as soon as the fork returns; spend it on
/// [`reset_child`](Self::reset_child) in the child. Nothing that takes
/// one of these locks may run in between.
pub(crate) struct ForkGuards {
    pool: native_pool::ForkLocks,
    _idents: id_table::ForkLock,
    _regexps: regexp::ForkLocks,
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
        _idents: idents,
        _regexps: regexps,
        _streams: streams,
        preempt,
    }
}

impl ForkGuards {
    /// The child's side: forget the pool and timer state inherited from
    /// threads the child does not have, and release every lock.
    pub(crate) fn reset_child(self) {
        let ForkGuards {
            pool,
            _idents,
            _regexps,
            _streams,
            preempt,
        } = self;
        pool.reset_child();
        preempt.reset_child();
        // `_idents`, `_regexps`, `_streams` unlock here, on the thread
        // that took them.
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id_table::IdentId;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    /// In-process: after `prepare` and `reset_child` every lock in the
    /// set is free again, on the thread that took them — the child's
    /// view, without a fork.
    #[test]
    fn reset_child_releases_every_lock() {
        let guards = prepare();
        guards.reset_child();
        assert_eq!(IdentId::get_id("fork_reset_probe"), IdentId::get_id("fork_reset_probe"));
        assert!(
            crate::rvalue::RegexpInner::with_option("fork_reset_probe", 0).is_ok(),
            "the regexp caches are still locked"
        );
        io::flush_std_streams();
        // And the parent's view: dropping the guards releases them too.
        drop(prepare());
        let _ = IdentId::get_id("fork_reset_probe");
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
