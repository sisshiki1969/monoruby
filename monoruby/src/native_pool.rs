//! Native worker offload for kernel-blocking syscalls
//! (see doc/threads.md §9).
//!
//! Some syscalls block *in the kernel* with no fd to poll — `flock(2)`
//! waiting for a lock, `open(2)` on a FIFO waiting for its peer. In an
//! M:1 runtime, calling them inline freezes the whole process: no other
//! green thread can run, timers and signals are useless. This module
//! runs such operations on short-lived native OS threads instead:
//!
//! 1. The builtin packages the operation as a [`NativeOp`] — raw fds,
//!    paths and integers only, never heap references — and [`submit`]s
//!    it, getting back a ticket id.
//! 2. A worker thread performs the syscall (retrying `EINTR`) and
//!    stores `(ret, errno)` in the process-global result table, then
//!    writes one byte into the submitting interpreter's completion pipe.
//! 3. The calling green thread parks on the pipe's read end through the
//!    scheduler's ordinary fd poller (`scheduler::wait_fd`), so other
//!    threads keep running — and a parked main thread sleeps in
//!    `poll(2)` rather than spinning. On wake it [`try_take`]s its
//!    result; several waiters share the pipe, so an unsatisfied waiter
//!    just parks again (the retry-loop pattern used everywhere else).
//!
//! A waiter killed or raised into while parked [`discard`]s its ticket:
//! the worker's eventual result is dropped instead of leaking, and the
//! worker itself is left to finish harmlessly (there is no portable way
//! to cancel a blocked syscall).

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Condvar, Mutex, OnceLock};
use std::time::Duration;

/// A kernel-blocking operation. Raw bytes and fds only: workers must
/// never touch the Ruby heap or the interpreter's thread-locals.
pub(crate) enum NativeOp {
    /// `flock(fd, op)` — `op` without `LOCK_NB` (the non-blocking form
    /// is executed inline by the caller).
    Flock { fd: i32, op: i32 },
    /// Blocking `open(2)` — used for FIFOs, whose open blocks until the
    /// peer end appears.
    Open { path: std::ffi::CString, flags: i32, mode: u32 },
    /// `fcntl(fd, cmd, &arg)` for a command that waits in the kernel —
    /// `F_SETLKW` (and its open-file-description twin where the platform
    /// has one), which blocks until the record lock it asks for is
    /// available. `arg` is this job's own copy of the packed
    /// `struct flock`: the waiting commands only read it, so nothing has
    /// to travel back.
    Fcntl { fd: i32, cmd: i32, arg: Vec<u8> },
    /// A foreign call the binding declared blocking (`attach_function ...,
    /// blocking: true`). Unlike the two syscalls above, what runs here is
    /// arbitrary C chosen by the Ruby program, so the raw-data discipline is
    /// enforced on the fiddle side: `FfiWorkerCall` carries only the address
    /// of an immortal descriptor and already-marshalled C arguments, and its
    /// `run` never touches the Ruby heap. `ret` is the raw 64-bit result,
    /// which the waiter boxes; `errno` is unused.
    Ffi(crate::builtins::fiddle::FfiWorkerCall),
    /// Opening or closing a SQLite connection
    /// (`src/builtins/sqlite3.rs`). These touch the filesystem — creating
    /// the file, replaying a WAL, checkpointing on close — so they run
    /// here rather than freezing every green thread, which is what the
    /// Fiddle bridge they replaced did with `blocking: true`. Everything
    /// else (`step`, `prepare`, the column reads) runs inline: it is
    /// short, and the round trip here costs tens of microseconds.
    Sqlite3(crate::builtins::sqlite3::Sqlite3WorkerCall),
}

pub(crate) struct Completion {
    pub ret: i64,
    pub errno: i32,
}

/// Result table, keyed by ticket id. Process-global: workers are plain
/// OS threads with no access to interpreter thread-locals.
fn results() -> &'static Mutex<HashMap<u64, Completion>> {
    static RESULTS: OnceLock<Mutex<HashMap<u64, Completion>>> = OnceLock::new();
    RESULTS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Tickets whose waiter gave up (killed / raised into while parked):
/// the worker's late result is dropped on arrival.
fn orphans() -> &'static Mutex<std::collections::HashSet<u64>> {
    static ORPHANS: OnceLock<Mutex<std::collections::HashSet<u64>>> = OnceLock::new();
    ORPHANS.get_or_init(|| Mutex::new(std::collections::HashSet::new()))
}

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

thread_local! {
    /// Completion pipe for this interpreter OS thread: `(read, write)`.
    /// The read end is what waiters park on; workers write one byte to
    /// the write end per completion. Lazily created; the read end is
    /// non-blocking so `drain` can slurp it dry.
    static PIPE: (i32, i32) = {
        let mut fds = [0i32; 2];
        // SAFETY: plain pipe(2); fds array is properly sized.
        let r = unsafe { libc::pipe(fds.as_mut_ptr()) };
        assert_eq!(r, 0, "native_pool: pipe(2) failed");
        // SAFETY: fds are the freshly created pipe ends.
        unsafe {
            libc::fcntl(fds[0], libc::F_SETFL, libc::O_NONBLOCK);
            libc::fcntl(fds[0], libc::F_SETFD, libc::FD_CLOEXEC);
            libc::fcntl(fds[1], libc::F_SETFD, libc::FD_CLOEXEC);
        }
        (fds[0], fds[1])
    };
}

/// The fd waiters park on (readable when a completion has arrived).
pub(crate) fn wake_fd() -> i32 {
    PIPE.with(|p| p.0)
}

/// Drain the completion pipe (called by a woken waiter before it
/// re-checks its ticket; the pipe is level-triggered, so leftover bytes
/// would make the poller spin).
pub(crate) fn drain() {
    PIPE.with(|p| {
        let mut buf = [0u8; 64];
        // SAFETY: reading our own non-blocking pipe read end.
        while unsafe { libc::read(p.0, buf.as_mut_ptr() as _, buf.len()) } > 0 {}
    });
}

/// One submitted operation, as a worker sees it: the ticket, the work,
/// and the completion pipe of the interpreter thread waiting for it
/// (workers are shared, so the fd travels with the job).
struct Job {
    id: u64,
    op: NativeOp,
    wake: i32,
}

#[derive(Default)]
struct Pool {
    queue: VecDeque<Job>,
    /// Workers parked on the condvar, i.e. available to take a job now.
    idle: usize,
}

fn pool() -> &'static (Mutex<Pool>, Condvar) {
    static POOL: OnceLock<(Mutex<Pool>, Condvar)> = OnceLock::new();
    POOL.get_or_init(|| (Mutex::new(Pool::default()), Condvar::new()))
}

/// How long a worker waits for its next job before exiting. Long enough
/// that a program offloading steadily keeps its workers hot, short
/// enough that a burst does not leave threads parked for the rest of the
/// process.
const WORKER_IDLE_TIMEOUT: Duration = Duration::from_secs(10);

/// Submit `op` to a worker thread; returns the ticket to poll with
/// [`try_take`].
///
/// Reuses a parked worker when there is one — the round trip is then a
/// condvar handoff rather than a `clone(2)`, which is what makes
/// offloading affordable for anything called often. A new worker is
/// started only when every existing one is busy: a worker busy with a
/// kernel-blocking syscall may not come back for minutes, so queueing
/// behind it would reintroduce exactly the stall this module exists to
/// avoid.
pub(crate) fn submit(op: NativeOp) -> u64 {
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let wake = PIPE.with(|p| p.1);
    let (lock, cv) = pool();
    let mut pool = lock.lock().unwrap();
    pool.queue.push_back(Job { id, op, wake });
    if pool.queue.len() > pool.idle {
        std::thread::spawn(worker);
    } else {
        cv.notify_one();
    }
    id
}

/// A pool worker: take jobs until idle for [`WORKER_IDLE_TIMEOUT`].
fn worker() {
    let (lock, cv) = pool();
    let mut pool = lock.lock().unwrap();
    loop {
        let job = loop {
            if let Some(job) = pool.queue.pop_front() {
                break Some(job);
            }
            pool.idle += 1;
            let (guard, timeout) = cv.wait_timeout(pool, WORKER_IDLE_TIMEOUT).unwrap();
            pool = guard;
            pool.idle -= 1;
            if timeout.timed_out() && pool.queue.is_empty() {
                break None;
            }
        };
        let Some(job) = job else { return };
        drop(pool);
        complete(job);
        pool = lock.lock().unwrap();
    }
}

/// Run one job and hand its result to the waiter.
fn complete(job: Job) {
    let (ret, errno) = run_op(&job.op);
    let comp = Completion { ret, errno };
    {
        let mut orphans = orphans().lock().unwrap();
        if orphans.remove(&job.id) {
            // Waiter is gone; drop the result.
            return;
        }
        results().lock().unwrap().insert(job.id, comp);
    }
    // SAFETY: `wake` is the submitting interpreter's pipe write end,
    // which lives for that interpreter thread's lifetime. A failed
    // or short write only delays the waiter until the next poller
    // pass triggered by another completion — never corrupts state.
    unsafe {
        let byte = 1u8;
        let _ = libc::write(job.wake, &byte as *const u8 as _, 1);
    }
}

/// Forget the inherited pool in a freshly forked child.
///
/// Workers do not survive `fork(2)`: the child has only the forking
/// thread. What it does inherit is the bookkeeping — an `idle` count
/// naming workers that no longer exist, and jobs queued for them, which
/// belong to the parent. Left alone, the child's first [`submit`] would
/// see "an idle worker will take it" and wait for a thread that will
/// never run. Called from the child side of every fork that keeps
/// interpreting (`Process._fork`, `Process.daemon`).
pub(crate) fn reset_after_fork() {
    let (lock, _) = pool();
    let mut pool = lock.lock().unwrap();
    pool.queue.clear();
    pool.idle = 0;
}

/// Take the completion for `id`, if the worker has finished.
pub(crate) fn try_take(id: u64) -> Option<Completion> {
    results().lock().unwrap().remove(&id)
}

/// Abandon a ticket whose waiter is unwinding (kill / raise): whichever
/// side arrives second cleans up.
pub(crate) fn discard(id: u64) {
    let mut orphans = orphans().lock().unwrap();
    if results().lock().unwrap().remove(&id).is_none() {
        orphans.insert(id);
    }
}

fn run_op(op: &NativeOp) -> (i64, i32) {
    match op {
        NativeOp::Flock { fd, op } => loop {
            // SAFETY: plain flock(2) on a raw fd owned by the caller's
            // IO object (kept alive by the parked waiter's frame).
            let r = unsafe { libc::flock(*fd, *op) };
            if r == 0 {
                return (0, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            if errno != libc::EINTR {
                return (r as i64, errno);
            }
        },
        NativeOp::Open { path, flags, mode } => loop {
            // SAFETY: NUL-terminated path, plain open(2).
            let r = unsafe { libc::open(path.as_ptr(), *flags, *mode as libc::c_uint) };
            if r >= 0 {
                return (r as i64, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            if errno != libc::EINTR {
                return (-1, errno);
            }
        },
        NativeOp::Fcntl { fd, cmd, arg } => loop {
            // SAFETY: fcntl(2) with a pointer third argument — this job
            // owns `arg` for as long as the worker runs, and the waiter
            // is parked meanwhile.
            let r = unsafe { libc::fcntl(*fd, *cmd, arg.as_ptr()) };
            if r != -1 {
                return (r as i64, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            if errno != libc::EINTR {
                return (-1, errno);
            }
        },
        // No EINTR loop: the foreign function owns its own restart policy,
        // and retrying an arbitrary C call would not generally be safe.
        NativeOp::Ffi(call) => (call.run(), 0),
        // As `Ffi`: raw handles and byte buffers only, and the SQLite
        // build is `SQLITE_THREADSAFE=1`, so a connection opened here is
        // usable from the interpreter thread afterwards.
        NativeOp::Sqlite3(call) => (call.run(), 0),
    }
}

/// Run `op` to completion on a worker while parking the calling green
/// thread on the completion pipe: the standard submit → park → retry
/// loop, shared by every offloaded builtin. On an interrupt delivered
/// while parked (kill / raise) the ticket is discarded and the error
/// propagates.
pub(crate) fn run_blocking(
    vm: &mut crate::executor::Executor,
    globals: &mut crate::globals::Globals,
    op: NativeOp,
) -> crate::executor::Result<Completion> {
    let id = submit(op);
    loop {
        if let Some(comp) = try_take(id) {
            return Ok(comp);
        }
        if let Err(err) = crate::scheduler::wait_fd(vm, globals, wake_fd(), libc::POLLIN, None) {
            discard(id);
            return Err(err);
        }
        drain();
    }
}
