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
//!
//! The JIT's code pages are the other thing a fork has to think about
//! ([`JitInheritance`]). They are one 2 GiB mapping (two 1 GiB pages,
//! monoasm's `PAGE_SIZE`) of which a process uses a few MiB, and on
//! macOS that mapping is `MAP_JIT` — whose `fork(2)` cost grows with its
//! *virtual* size, touched or not: ~45 ms for the whole region on an
//! Apple M1, against 0.3 ms for a plain anonymous mapping of the same
//! size. ruby-bench's knucleotide forks seven workers per iteration and
//! spent ~250 ms of its ~310 ms there. So the unused tail of each code
//! page is left out of the child (`minherit(VM_INHERIT_NONE)`; Linux's
//! `MADV_DONTFORK` the same way), the parent puts it back as soon as the
//! fork returns, and a child that goes on running Ruby maps a fresh tail
//! at the same address. A child that only `execve`s gets none of the
//! code pages at all.

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
    gvl: crate::gvl::GvlForkGuard,
    pool: native_pool::ForkLocks,
    idents: id_table::ForkLock,
    regexps: regexp::ForkLocks,
    _streams: io::ForkLocks,
    preempt: preempt::ForkState,
    jit: JitInheritance,
}

/// What the child of a `fork(2)` goes on to do, which decides how much
/// of the JIT's code pages it inherits ([`JitInheritance`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ForkKind {
    /// The child keeps running Ruby (`Process._fork`, `Process.daemon`):
    /// it inherits the code written so far and gets a fresh, empty tail.
    Continue,
    /// The child only `execve`s or `_exit`s (`Process.spawn` and
    /// everything built on it): it runs no JIT code, so it inherits none.
    Exec,
}

/// The ranges of the forking interpreter's JIT code pages that were
/// marked not to be inherited for one `fork(2)`. Dropping it (the
/// parent, right after the fork) marks them inheritable again, so a
/// fork that does not come through [`prepare`] — one a C extension
/// makes — still hands its child the whole region.
pub(crate) struct JitInheritance {
    /// `(start, len)`, page-aligned; `len == 0` for an unused slot.
    ranges: [(usize, usize); 2],
    kind: ForkKind,
}

impl JitInheritance {
    fn none(kind: ForkKind) -> Self {
        JitInheritance {
            ranges: [(0, 0); 2],
            kind,
        }
    }

    /// Mark what the child of this fork will not inherit. The extents
    /// come from the forking thread's own interpreter; another
    /// interpreter in the process (the test harness runs one per thread)
    /// keeps its region inherited whole, which costs time but nothing
    /// else, since its threads do not exist in the child.
    fn prepare(kind: ForkKind) -> Self {
        let mut this = Self::none(kind);
        let Some(Some(pages)) = crate::codegen::CODEGEN
            .try_with(|c| c.try_borrow_mut().ok().map(|mut c| c.code_page_extents()))
        else {
            return this;
        };
        let os_page = os_page_size();
        let [(base0, used0), (base1, used1)] = pages;
        // monoasm lays the two code pages out back to back.
        let page_len = base1 - base0;
        let wanted = match kind {
            ForkKind::Exec => [(base0, 2 * page_len), (0, 0)],
            ForkKind::Continue => [
                tail(base0, used0, page_len, os_page),
                tail(base1, used1, page_len, os_page),
            ],
        };
        for (slot, (start, len)) in wanted.into_iter().enumerate() {
            // SAFETY: a range inside this interpreter's own code mapping;
            // only its inheritance attribute changes, never its contents
            // or protection.
            if len > 0 && unsafe { set_inherit(start, len, false) } {
                this.ranges[slot] = (start, len);
            }
        }
        this
    }

    /// The child's side: a child that runs Ruby needs the tails back
    /// before it emits code into them, at the very addresses the code
    /// already written refers to.
    fn reset_child(&self) {
        if self.kind == ForkKind::Exec {
            return;
        }
        for &(start, len) in &self.ranges {
            // SAFETY: `[start, start + len)` is the hole the fork left
            // where the tail was; nothing else in this single-threaded
            // child has mapped anything since.
            if len > 0 && !unsafe { map_tail(start, len) } {
                tail_lost(start, len);
            }
        }
    }
}

impl Drop for JitInheritance {
    fn drop(&mut self) {
        for &(start, len) in &self.ranges {
            if len > 0 {
                // SAFETY: as in `prepare`. A failure leaves the range
                // out of later forks that bypass `prepare`; there is
                // nothing better to do with it here.
                unsafe { set_inherit(start, len, true) };
            }
        }
    }
}

/// The part of the code page at `base` (`page_len` bytes) past the
/// `used` bytes written so far, rounded out to whole OS pages.
fn tail(base: usize, used: usize, page_len: usize, os_page: usize) -> (usize, usize) {
    let start = (base + used + os_page - 1) & !(os_page - 1);
    let end = base + page_len;
    if start < end {
        (start, end - start)
    } else {
        (end, 0)
    }
}

fn os_page_size() -> usize {
    // SAFETY: sysconf has no preconditions.
    let p = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
    if p > 0 { p as usize } else { 4096 }
}

#[cfg(target_os = "macos")]
mod vm_inherit {
    // <mach/vm_inherit.h>; the libc crate does not bind minherit(2).
    pub(super) const VM_INHERIT_COPY: libc::c_int = 1;
    pub(super) const VM_INHERIT_NONE: libc::c_int = 2;
    unsafe extern "C" {
        pub(super) fn minherit(
            addr: *mut libc::c_void,
            len: libc::size_t,
            inherit: libc::c_int,
        ) -> libc::c_int;
    }
}

/// Whether the child of a later `fork(2)` gets `[start, start + len)`.
/// `false` if the platform cannot say.
///
/// # Safety
///
/// The range must be page-aligned and mapped.
unsafe fn set_inherit(start: usize, len: usize, inherit: bool) -> bool {
    #[cfg(target_os = "macos")]
    {
        use vm_inherit::*;
        let how = if inherit {
            VM_INHERIT_COPY
        } else {
            VM_INHERIT_NONE
        };
        // SAFETY: the caller's contract.
        unsafe { minherit(start as *mut libc::c_void, len, how) == 0 }
    }
    #[cfg(target_os = "linux")]
    {
        let how = if inherit {
            libc::MADV_DOFORK
        } else {
            libc::MADV_DONTFORK
        };
        // SAFETY: the caller's contract.
        unsafe { libc::madvise(start as *mut libc::c_void, len, how) == 0 }
    }
    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        let _ = (start, len, inherit);
        false
    }
}

/// Map an empty code-page tail at exactly `start`, with the protection
/// and kind the JIT's code pages have; `false` if the kernel put it
/// anywhere else (it is then unmapped again).
///
/// # Safety
///
/// `[start, start + len)` must be unmapped.
unsafe fn map_tail(start: usize, len: usize) -> bool {
    let prot = libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC;
    // macOS refuses `MAP_FIXED | MAP_JIT`, so the address is a hint
    // there; the hole the fork left is free, and the kernel takes the
    // hint. Linux can be told not to settle for anything else.
    #[cfg(target_os = "macos")]
    let flags = libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_JIT;
    #[cfg(target_os = "linux")]
    let flags =
        libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE | libc::MAP_FIXED_NOREPLACE;
    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    let flags = libc::MAP_PRIVATE | libc::MAP_ANON;
    // SAFETY: an anonymous mapping over a range the caller says is free.
    let p = unsafe { libc::mmap(start as *mut libc::c_void, len, prot, flags, -1, 0) };
    if p == start as *mut libc::c_void {
        return true;
    }
    if p != libc::MAP_FAILED {
        // SAFETY: the mapping just made, elsewhere.
        unsafe { libc::munmap(p, len) };
    }
    false
}

/// The child could not get its code-page tail back. Code already
/// written points into the region, so the JIT cannot move; the child
/// would fault the first time it emitted code past what it inherited.
/// Say so and stop, rather than crash somewhere later.
#[cold]
#[coverage(off)] // needs the kernel to refuse a free address
fn tail_lost(start: usize, len: usize) -> ! {
    eprintln!(
        "monoruby: fork: could not map the JIT code region back at {start:#x} ({len} bytes) in the child: {}",
        std::io::Error::last_os_error()
    );
    // SAFETY: terminating the child, which is all that is left to do.
    unsafe { libc::_exit(1) }
}

/// Quiesce the process for a `fork(2)` — see the module doc. Call it
/// after the standard streams are flushed (the flush takes their locks)
/// and immediately before `libc::fork`.
/// Write this process's coverage profile now. A process that never
/// reaches the profiling runtime's exit hook — one the kernel kills, or
/// a forked child on its way to `execve` — would otherwise leave the
/// lines it ran invisible to coverage. A no-op outside an instrumented
/// build (cargo-llvm-cov sets `--cfg coverage`).
pub(crate) fn flush_coverage_profile() {
    #[cfg(coverage)]
    {
        unsafe extern "C" {
            fn __llvm_profile_write_file() -> libc::c_int;
        }
        // SAFETY: provided by the profiling runtime every instrumented
        // build links; it only writes the profile of this process.
        unsafe {
            __llvm_profile_write_file();
        }
    }
}

pub(crate) fn prepare(kind: ForkKind) -> ForkGuards {
    // The GVL's state: in the 1:1 model another kernel thread may be
    // inside `acquire` / `release` (it holds nothing else there), and
    // the child must not inherit that lock held by a thread it does not
    // have. Taken first, so no thread changes hands from here on.
    let gvl = crate::scheduler::native::gvl_fork_guard();
    // The pool: its own lock order is pool, orphans, results, and a
    // worker holds nothing else while it holds one of those.
    let pool = native_pool::prepare_fork();
    let idents = id_table::prepare_fork();
    let regexps = regexp::prepare_fork();
    let streams = io::prepare_fork();
    // Not a lock: the timer thread is not quiesced, it just ticks, so
    // the child abandons its lock instead (`preempt::ForkState`).
    let preempt = preempt::prepare_fork();
    // Last: nothing above runs JIT code or touches the code pages.
    let jit = JitInheritance::prepare(kind);
    ForkGuards {
        gvl,
        pool,
        idents,
        regexps,
        _streams: streams,
        preempt,
        jit,
    }
}

impl ForkGuards {
    /// The child's side: forget the pool and timer state inherited from
    /// threads the child does not have, replace the `RwLock`s, and
    /// release the mutexes.
    pub(crate) fn reset_child(self) {
        let ForkGuards {
            gvl,
            pool,
            idents,
            regexps,
            _streams,
            preempt,
            jit,
        } = self;
        // First: everything after this may emit code.
        jit.reset_child();
        gvl.reset_child();
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
        let l = fresh_lock();
        l.write().unwrap().push(7);
        let old = l.get() as *const RwLock<Vec<u32>>;
        l.prepare_fork().reset_child();
        let fresh = l.get() as *const RwLock<Vec<u32>>;
        assert!(!std::ptr::eq(old, fresh), "the child kept the inherited lock");
        assert_eq!(*l.read().unwrap(), vec![7]);
        l.write().unwrap().push(8);
        assert_eq!(*l.read().unwrap(), vec![7, 8]);
        // SAFETY: the leaked lock is never freed.
        let old = unsafe { &*old };
        assert!(old.try_read().is_err(), "the inherited lock was unlocked");
        // The parent's view: dropping the guard unlocks the same lock.
        drop(l.prepare_fork());
        assert!(std::ptr::eq(fresh, l.get()));
        assert_eq!(l.read().unwrap().len(), 2);
    }

    /// A lock built at run time (the real ones are `static`s, built in
    /// const evaluation) and leaked, since `prepare_fork` wants `'static`.
    fn fresh_lock() -> &'static ForkableRwLock<Vec<u32>> {
        Box::leak(Box::new(ForkableRwLock::new(Vec::new)))
    }

    /// First use is a CAS race, not a `Once`: every thread that arrives
    /// at a lock nobody has used yet ends up on the same one, and the
    /// losers' boxes are freed, not leaked.
    #[test]
    fn every_thread_racing_a_fresh_lock_gets_the_same_one() {
        for _ in 0..32 {
            let l = fresh_lock();
            let barrier = Arc::new(std::sync::Barrier::new(8));
            let ptrs: Vec<usize> = (0..8)
                .map(|_| {
                    let barrier = barrier.clone();
                    std::thread::spawn(move || {
                        barrier.wait();
                        l.get() as *const RwLock<Vec<u32>> as usize
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|h| h.join().unwrap())
                .collect();
            assert!(ptrs.iter().all(|p| *p == ptrs[0]), "the threads got different locks");
            l.write().unwrap().push(1);
            assert_eq!(l.read().unwrap().len(), 1);
        }
    }

    /// The tail left out of a fork starts at the first whole OS page past
    /// what was written, and is empty when the page is full.
    #[test]
    fn the_tail_starts_past_the_written_bytes() {
        let page = 1 << 30;
        assert_eq!(tail(0x1000_0000, 0, page, 4096), (0x1000_0000, page));
        assert_eq!(tail(0x1000_0000, 1, page, 4096), (0x1000_1000, page - 4096));
        assert_eq!(
            tail(0x1000_0000, 4096, page, 4096),
            (0x1000_1000, page - 4096)
        );
        assert_eq!(
            tail(0x1000_0000, 4097, page, 16384),
            (0x1000_4000, page - 16384)
        );
        assert_eq!(tail(0x1000_0000, page, page, 4096), (0x1000_0000 + page, 0));
    }

    /// The paths of [`JitInheritance`] a forked child never reaches in
    /// a test: an interpreter whose JIT is busy (already borrowed) leaves
    /// its code pages inherited whole, an exec-only child maps nothing
    /// back, and a tail that cannot be placed at its own address is
    /// reported as lost rather than mapped somewhere else.
    #[test]
    fn jit_inheritance_edge_cases() {
        crate::codegen::CODEGEN.with(|c| {
            let _busy = c.borrow_mut();
            let j = JitInheritance::prepare(ForkKind::Continue);
            assert_eq!(j.ranges, [(0, 0); 2]);
        });
        JitInheritance::none(ForkKind::Exec).reset_child();
        let len = os_page_size();
        // SAFETY: an anonymous mapping owned by this test, unmapped below.
        let p = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            )
        };
        assert_ne!(p, libc::MAP_FAILED);
        // SAFETY: the range is mapped, so `map_tail` must refuse it
        // (Linux: MAP_FIXED_NOREPLACE fails; elsewhere the kernel moves
        // the mapping and `map_tail` unmaps its own copy).
        assert!(!unsafe { map_tail(p as usize, len) });
        // SAFETY: the mapping made above.
        unsafe { libc::munmap(p, len) };
    }

    /// The parent's view: dropping what `prepare` took releases every
    /// lock in the set, on the thread that took them. (The child's view,
    /// `reset_child`, is only ever run in a forked child — in this
    /// process it would leave the inherited locks locked under the other
    /// tests' threads — so it is covered by the fork below and by the
    /// `ForkableRwLock` test above.)
    #[test]
    fn dropping_the_guards_releases_every_lock() {
        drop(prepare(ForkKind::Continue));
        assert_eq!(IdentId::get_id("fork_reset_probe"), IdentId::get_id("fork_reset_probe"));
        assert!(
            crate::rvalue::RegexpInner::with_option("fork_reset_probe", 0).is_ok(),
            "the regexp caches are still locked"
        );
        io::flush_std_streams();
        drop(prepare(ForkKind::Continue));
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
        // Flips the flag on the way out, a failed round's panic included,
        // so the churn thread never spins on under the rest of the suite.
        struct StopOnDrop(Arc<AtomicBool>);
        impl Drop for StopOnDrop {
            fn drop(&mut self) {
                self.0.store(true, Ordering::Relaxed);
            }
        }
        let _stop = StopOnDrop(stop.clone());
        let deadline = std::time::Duration::from_secs(10);
        for round in 0..200 {
            let guards = prepare(ForkKind::Continue);
            // SAFETY: fork(2); the child only interns one name and
            // `_exit`s, the parent waits for it.
            let pid = unsafe { libc::fork() };
            assert!(pid >= 0, "fork failed: {}", std::io::Error::last_os_error());
            if pid == 0 {
                guards.reset_child();
                let _ = IdentId::get_id("fork_child_probe");
                // `exit` rather than `_exit`, so the child's coverage
                // profile is flushed (as `fill_closed_std_fds_repairs_closed_stderr`
                // does): `reset_child`'s child-side paths run nowhere else.
                std::process::exit(0);
            }
            drop(guards);
            let status = wait_or_kill(
                pid,
                deadline,
                &format!("round {round}: the child never got past get_id (a lock it inherited held)"),
            );
            assert!(
                libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                "round {round}: child status {status:#x}"
            );
        }
        stop.store(true, Ordering::Relaxed);
        churn.join().unwrap();
    }

    /// Reap `pid` and return its wait status; once `deadline` has passed
    /// without it exiting, kill it, reap it, and panic with `what` — so a
    /// child wedged on an inherited lock fails the test instead of
    /// hanging the suite.
    fn wait_or_kill(pid: libc::pid_t, deadline: std::time::Duration, what: &str) -> i32 {
        let started = std::time::Instant::now();
        let mut status = 0;
        loop {
            // SAFETY: waitpid on a child of ours.
            let r = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) };
            if r == pid {
                return status;
            }
            assert!(r == 0, "waitpid: {}", std::io::Error::last_os_error());
            if started.elapsed() > deadline {
                // SAFETY: our own child.
                unsafe {
                    libc::kill(pid, libc::SIGKILL);
                    libc::waitpid(pid, &mut status, 0);
                }
                panic!("{what}");
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    }

    /// The deadline path of the helper above, on a child that never
    /// exits on its own (a spawned `sleep`, so no line of this binary
    /// runs in a process that is then killed unrecorded).
    #[test]
    fn a_stuck_child_is_killed_and_reported() {
        let child = std::process::Command::new("sleep")
            .arg("1000")
            .spawn()
            .expect("spawn sleep");
        let pid = child.id() as libc::pid_t;
        let r = std::panic::catch_unwind(|| {
            wait_or_kill(pid, std::time::Duration::from_millis(100), "stuck")
        });
        let msg = r.expect_err("the helper let a stuck child pass");
        assert_eq!(msg.downcast_ref::<String>().map(String::as_str), Some("stuck"));
        // Killed and reaped by the helper: nothing is left to wait for.
        // SAFETY: waitpid on a pid the helper has already reaped.
        assert_eq!(unsafe { libc::waitpid(pid, std::ptr::null_mut(), libc::WNOHANG) }, -1);
    }
}
