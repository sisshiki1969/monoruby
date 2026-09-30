//! The 1:1 thread model: each Ruby `Thread` runs on a kernel thread of
//! its own, and the threads take turns under the Global VM Lock
//! (`crate::gvl`). Selected with `MONORUBY_THREAD_MODEL=native`; the
//! green scheduler in the parent module stays the default.
//!
//! The registry is the parent module's, unchanged: `Scheduler::threads`,
//! `main`, `current`, the pending reports all live in the `Vm`, and
//! only the GVL holder touches them — so `current` is simply "the
//! holder", written on every acquire. What this module replaces is the
//! *switching*. Where a green thread saves its context and jumps to the
//! scheduler loop, a native thread releases the GVL and blocks in
//! `poll(2)` on its own wake pipe (the [`Parker`]) plus whatever fds it
//! is waiting for; where the scheduler would queue a thread runnable,
//! the waker writes a byte to that pipe. The GVL's FIFO handoff is the
//! ready queue.
//!
//! Every park is at a VM safepoint inside a builtin (a `sleep`, a
//! `join`, an fd wait), exactly where a green thread switched, so a
//! thread that does not hold the lock always has GC-complete frames and
//! the holder's collections can mark every thread through the registry
//! (`Scheduler::mark`: each thread's `Executor`, and the main thread's
//! through `main_exec`, published before main lets go of the lock).
//!
//! Kernel threads serve the spawner's interpreter (`vm::adopt`), block
//! the asynchronous signals so the kernel delivers them to the main
//! thread, and die with their body: `Thread#kill` is still the queued
//! interrupt the parent module delivers at the next delivery point.

use std::cell::RefCell;
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};

use super::*;
use crate::gvl::{Gvl, GvlThread};

/// Whether the 1:1 model is selected (`MONORUBY_THREAD_MODEL=native`).
/// Read once; the model cannot change while threads exist.
pub(crate) fn enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| {
        std::env::var("MONORUBY_THREAD_MODEL").is_ok_and(|v| v == "native")
    })
}

/// Machine stack of a kernel thread: room for the 1 MiB Ruby-frame
/// budget the primary context gets (`MAIN_STACK_SIZE`) and the Rust
/// frames of the builtins beneath it.
const NATIVE_STACK_SIZE: usize = 8 * 1024 * 1024;

/// The unblocking signal: sent with `pthread_kill` to a kernel thread
/// that is inside a GVL-less region (`without_gvl`) when an interrupt
/// is queued for its Ruby thread, so that the syscall it is blocked in
/// returns `EINTR` and the thread comes back under the GVL to take the
/// interrupt. Its handler does nothing; the signal exists to make the
/// kernel return. CRuby's `ubf_pthread` uses the same signal, which is
/// why `SIGVTALRM` is one of the reserved, untrappable ones.
const UBF_SIGNAL: libc::c_int = libc::SIGVTALRM;

extern "C" fn ubf_handler(_: libc::c_int) {}

/// Make sure `ubf_handler` is the process's disposition for
/// `UBF_SIGNAL`, without `SA_RESTART` (the whole point is that the
/// blocked syscall is *not* restarted). Checked, not installed once:
/// the default disposition of `SIGVTALRM` terminates the process, so a
/// disposition reset by anything else must be found before a
/// `pthread_kill`, never after. A Ruby program cannot reset it (the
/// signal is reserved), but a failed in-process `exec` leaves every
/// disposition at its default (`spawn::child_exec`), and foreign code
/// can do as it likes.
fn ensure_ubf_handler() {
    let handler = ubf_handler as *const () as usize as libc::sighandler_t;
    // SAFETY: plain sigaction calls with a handler that lives in the
    // executable for the process lifetime.
    unsafe {
        let mut old: libc::sigaction = std::mem::zeroed();
        if libc::sigaction(UBF_SIGNAL, std::ptr::null(), &mut old) == 0 && old.sa_sigaction == handler {
            return;
        }
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handler;
        sa.sa_flags = 0;
        libc::sigemptyset(&mut sa.sa_mask);
        libc::sigaction(UBF_SIGNAL, &sa, std::ptr::null_mut());
    }
}

/// A thread's wake pipe. The parking thread polls the read end; a waker
/// writes one byte. Both ends are non-blocking: the writer never stalls
/// on a full pipe (one byte is all a park needs), and the parker drains
/// without blocking once it holds the GVL again — no writer can add a
/// byte then, since every waker holds the GVL to write.
#[derive(Debug)]
pub(crate) struct Parker {
    read: i32,
    write: i32,
}

impl Parker {
    pub(crate) fn new() -> Self {
        let mut fds = [0i32; 2];
        // SAFETY: plain pipe(2) into a two-element array; the flags are
        // set on our own freshly created descriptors.
        unsafe {
            if libc::pipe(fds.as_mut_ptr()) != 0 {
                panic!("pipe: {}", std::io::Error::last_os_error());
            }
            for fd in fds {
                libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK);
                libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
            }
        }
        Parker {
            read: fds[0],
            write: fds[1],
        }
    }

    fn fd(&self) -> i32 {
        self.read
    }

    /// Wake the parked owner. A full pipe (`EAGAIN`) already holds a
    /// wake byte, which is all that is needed.
    pub(crate) fn unpark(&self) {
        let byte = 1u8;
        // SAFETY: writing one byte from a live local to our own pipe end.
        unsafe { libc::write(self.write, &byte as *const u8 as _, 1) };
    }

    fn drain(&self) {
        let mut buf = [0u8; 64];
        // SAFETY: reading our own non-blocking pipe read end.
        while unsafe { libc::read(self.read, buf.as_mut_ptr() as _, buf.len()) } > 0 {}
    }
}

impl Drop for Parker {
    fn drop(&mut self) {
        // SAFETY: closing the two ends this Parker created and owns.
        unsafe {
            libc::close(self.read);
            libc::close(self.write);
        }
    }
}

thread_local! {
    /// This kernel thread's handle on the GVL, for a thread spawned by
    /// [`spawn`]. The main thread's handle lives in the `Vm`.
    static GVL_THREAD: RefCell<Option<GvlThread>> = const { RefCell::new(None) };
}

fn with_gvl<R>(f: impl FnOnce(&Gvl, &GvlThread) -> R) -> R {
    let (gvl, main) = crate::vm::vm().gvl();
    GVL_THREAD.with(|t| match &*t.borrow() {
        Some(th) => f(gvl, th),
        None => f(gvl, main),
    })
}

/// Take the GVL for `cur`, which becomes the current thread.
fn acquire(cur: Value) {
    with_gvl(|gvl, th| gvl.acquire(th));
    SCHEDULER.with(|s| s.borrow_mut().current = Some(cur));
}

fn release() {
    with_gvl(|gvl, th| gvl.release(th));
}

/// Let a waiting thread run (if any) and come back as current.
fn yield_now(cur: Value) {
    with_gvl(|gvl, th| gvl.yield_now(th));
    SCHEDULER.with(|s| s.borrow_mut().current = Some(cur));
}

fn current() -> Value {
    SCHEDULER.with(|s| s.borrow().current.unwrap())
}

/// Publish the main thread's root executor for the GC of another
/// thread. Called by main right before it lets go of the GVL — and
/// only then: `ensure_main` is reached from `require` during
/// `Executor::init`, whose executor is moved out afterwards, so an
/// address taken there would go stale.
pub(super) fn publish_main_exec(vm: &mut Executor) {
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.main_exec = Some(root_exec(vm));
        s.in_scheduler = true;
    });
}

/// Write a wake byte to `thread`'s pipe (a no-op before its first park
/// gave it one).
fn unpark(thread: Value) {
    if let Some(p) = &thread.as_thread_inner().parker {
        p.unpark();
    }
}

/// A parked thread becomes runnable: mark it and wake it. Returns
/// whether it was parked.
pub(super) fn wake_parked(mut thread: Value) -> bool {
    let inner = thread.as_thread_inner_mut();
    if matches!(
        inner.state(),
        ThreadState::Sleeping | ThreadState::Joining | ThreadState::IoWaiting
    ) {
        inner.state = ThreadState::Runnable;
        unpark(thread);
        true
    } else {
        false
    }
}

/// An interrupt was queued for `thread`, which is not parked. If it is
/// inside a GVL-less region (`without_gvl`), send the unblocking signal
/// to its kernel thread: the blocked syscall returns `EINTR`, the region
/// ends, and the thread takes the interrupt on its way back under the
/// GVL. Returns whether a signal was sent. A thread that is simply
/// running Ruby needs nothing: it reaches a poll point on its own.
pub(super) fn interrupt_blocking(thread: Value) -> bool {
    match thread.as_thread_inner().blocking_tid {
        Some(tid) => {
            ensure_ubf_handler();
            // SAFETY: `tid` is a live kernel thread of this process — it
            // clears `blocking_tid` (under the GVL, which we hold) before
            // it can exit — and the signal has a no-op handler.
            unsafe { libc::pthread_kill(tid, UBF_SIGNAL) };
            true
        }
        None => false,
    }
}

/// Run `f` with the GVL released, on the calling kernel thread. For a
/// syscall or foreign call that blocks in the kernel with no fd to poll
/// (`flock`, a FIFO `open`, `waitpid`, `getaddrinfo`, a blocking FFI or
/// extension call): the other Ruby threads run meanwhile, as they do
/// while this thread is parked. `f` must touch nothing of the
/// interpreter — no `Value`, no `Globals` — since another thread owns
/// them for the duration; it works on raw fds, integers and its own
/// buffers. The GVL-holding side of the protocol is the same as
/// `park`'s: `resume_exec` is published for `Thread#backtrace`, main's
/// root executor for a collection another thread starts, and
/// `blocking_tid` for an interrupt's unblocking signal. Delivery of a
/// pending interrupt is the caller's job (`deliver_pending_now` after
/// an `EINTR`), as after a park.
pub(super) fn without_gvl<R>(vm: &mut Executor, f: impl FnOnce() -> R) -> R {
    ensure_main(vm);
    let mut cur = current();
    let is_main = SCHEDULER.with(|s| s.borrow().main == Some(cur));
    {
        let inner = cur.as_thread_inner_mut();
        inner.resume_exec = Some(std::ptr::NonNull::from(&mut *vm));
        // SAFETY: pthread_self has no preconditions.
        inner.blocking_tid = Some(unsafe { libc::pthread_self() });
    }
    if is_main {
        publish_main_exec(vm);
    }
    release();
    let r = f();
    acquire(cur);
    let inner = cur.as_thread_inner_mut();
    inner.blocking_tid = None;
    inner.resume_exec = None;
    r
}

/// The 1:1 model's `native_pool::run_blocking`: the operation runs on
/// the calling kernel thread with the GVL released instead of on a pool
/// worker, so there is no completion pipe to park on and no 40 µs
/// round trip. A signal ends the syscall early (`EINTR`) — an
/// interrupt's unblocking signal, or on main a trapped signal — and is
/// handled under the GVL like a park's: the poll point runs, a pending
/// interrupt is delivered, and the syscall is issued again (they are
/// all idempotent waits). A foreign call is never re-issued; an `EINTR`
/// inside it is the callee's own business.
pub(crate) fn run_blocking(
    vm: &mut Executor,
    globals: &mut Globals,
    op: crate::native_pool::NativeOp,
) -> Result<crate::native_pool::Completion> {
    use crate::native_pool::{run_op, Completion};
    ensure_main(vm);
    let cur = current();
    deliver_pending_now(vm, globals, cur, true)?;
    set_park_blocking(cur, true);
    loop {
        let (ret, errno) = without_gvl(vm, || run_op(&op, false));
        if errno == libc::EINTR && op.retries_eintr() {
            if crate::executor::execute_gc(vm, globals).is_none() {
                return Err(vm.take_error());
            }
            deliver_pending_now(vm, globals, cur, true)?;
            continue;
        }
        // An interrupt that arrived meanwhile is delivered before the
        // result is handed back, as CRuby checks interrupts on leaving a
        // blocking region: a foreign call that the unblocking signal cut
        // short (its `EINTR` is inside `ret`) must not run on as if it
        // had completed.
        deliver_pending_now(vm, globals, cur, true)?;
        return Ok(Completion { ret, errno });
    }
}

/// `fd` is about to be closed. A thread parked in `poll(2)` on it would
/// not notice — the kernel keeps polling the file the descriptor
/// referred to — where the green scheduler's poller saw `POLLNVAL` at
/// once. Wake every thread waiting on it; each re-checks its stream and
/// finds it closed (`IOError`, as CRuby's `rb_thread_fd_close`).
pub(super) fn fd_closing(fd: i32) {
    // Reached from `FileDescriptor::drop`, possibly inside a sweep or
    // the interpreter's teardown: touch nothing that is not there.
    let threads = SCHEDULER
        .try_with(|s| s.try_borrow().ok().map(|s| s.threads.clone()))
        .flatten();
    for t in threads.unwrap_or_default() {
        let inner = t.as_thread_inner();
        if inner.state() == ThreadState::IoWaiting && inner.park_fds.contains(&fd) {
            wake_parked(t);
        }
    }
}

/// What the spawner hands the kernel thread. Raw addresses, since the
/// interpreter's types are not `Send`; they are valid for as long as
/// the interpreter runs, and `terminate_all` joins every kernel thread
/// before the interpreter is torn down.
struct Start {
    vm: usize,
    globals: usize,
    poll_flag: usize,
    thread: Value,
    /// Shared with the spawner so that, if the kernel thread cannot be
    /// created, the spawner takes the handle back and unregisters it.
    gvl_thread: Arc<Mutex<Option<GvlThread>>>,
}

// SAFETY: see `Start` — the addresses are dereferenced only under the
// GVL, and `Value` is a plain word.
unsafe impl Send for Start {}

/// Register a freshly created thread object and start its kernel
/// thread, which queues itself on the GVL before this returns — so the
/// `Thread.new` caller's `pass` hands it its first slice, as the green
/// scheduler's eager dispatch does.
pub(super) fn spawn(vm: &mut Executor, globals: &mut Globals, mut thread: Value) -> Result<()> {
    ensure_main(vm);
    ensure_ubf_handler();
    let (gvl_thread, probe) = with_gvl(|gvl, _| {
        let th = gvl.register();
        let probe = th.wait_probe();
        (th, probe)
    });
    thread.as_thread_inner_mut().parker = Some(Parker::new());
    let gvl_thread = Arc::new(Mutex::new(Some(gvl_thread)));
    let start = Start {
        vm: crate::vm::vm() as *const crate::vm::Vm as usize,
        globals: globals as *mut Globals as usize,
        poll_flag: CODEGEN.with(|c| c.borrow().poll_flag_addr()) as usize,
        thread,
        gvl_thread: gvl_thread.clone(),
    };
    let handle = std::thread::Builder::new()
        .name(format!("ruby-thread-{:x}", thread.id()))
        .stack_size(NATIVE_STACK_SIZE)
        .spawn(move || thread_main(start));
    let handle = match handle {
        Ok(h) => h,
        Err(e) => {
            if let Some(th) = gvl_thread.lock().unwrap().take() {
                with_gvl(|gvl, _| gvl.unregister(th));
            }
            return Err(MonorubyErr::threaderr(
                &globals.store,
                format!("can't create Thread: {e}"),
            ));
        }
    };
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.threads.push(thread);
        // Reap the kernel threads of threads that have died since.
        s.native_joins.retain(|(_, h)| !h.is_finished());
        s.native_joins.push((thread.id(), handle));
    });
    // Wait for the new thread to queue on the GVL. It does nothing else
    // before that, so this is a few microseconds.
    while !with_gvl(|gvl, _| gvl.is_waiting(&probe)) {
        std::thread::yield_now();
    }
    notify_thread_count();
    Ok(())
}

/// Block the asynchronous signals on this kernel thread, so the kernel
/// delivers a process-directed signal to the main thread — the only
/// one that runs trap handlers (`signal_delivery_ok`). The synchronous
/// ones (a fault in this thread's own code) must stay deliverable.
fn block_async_signals() {
    // SAFETY: a signal set built and applied on this thread only.
    unsafe {
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut set);
        for sig in [
            libc::SIGSEGV,
            libc::SIGBUS,
            libc::SIGILL,
            libc::SIGFPE,
            libc::SIGTRAP,
            libc::SIGABRT,
            libc::SIGSYS,
            UBF_SIGNAL,
        ] {
            libc::sigdelset(&mut set, sig);
        }
        libc::pthread_sigmask(libc::SIG_BLOCK, &set, std::ptr::null_mut());
    }
}

/// The kernel thread's whole life.
fn thread_main(start: Start) {
    // SAFETY: the spawner's interpreter, alive until every kernel thread
    // has been joined (`terminate_all`).
    let vm = unsafe { &*(start.vm as *const crate::vm::Vm) };
    crate::vm::adopt(vm);
    crate::poll_flag::adopt(start.poll_flag as *mut u32);
    block_async_signals();
    let th = start.gvl_thread.lock().unwrap().take().unwrap();
    GVL_THREAD.with(|t| *t.borrow_mut() = Some(th));
    // SAFETY: as `vm` — and dereferenced only under the GVL.
    let globals = unsafe { &mut *(start.globals as *mut Globals) };
    let thread = start.thread;
    acquire(thread);
    run_body(globals, thread);
    // `thread` may be unrooted from here on (finalization pruned the
    // registry): nothing below touches it.
    release();
    let th = GVL_THREAD.with(|t| t.borrow_mut().take()).unwrap();
    vm.gvl().0.unregister(th);
    crate::vm::unadopt();
}

/// Run the body block on this kernel thread and finalize the thread.
fn run_body(globals: &mut Globals, mut thread: Value) {
    let (handle, proc, args, root_lep) = {
        let inner = thread.as_thread_inner_mut();
        debug_assert_eq!(inner.state(), ThreadState::Created);
        // Killed / raised into before the body ran: dies unstarted, the
        // first queued interrupt deciding how (FIFO, as `dispatch`).
        match inner.pending.pop_front() {
            Some(PendingInterrupt::Kill) => {
                finalize_unstarted(globals, thread, None);
                return;
            }
            Some(PendingInterrupt::Raise(err)) => {
                finalize_unstarted(globals, thread, Some(err));
                return;
            }
            None => {}
        }
        inner.state = ThreadState::Runnable;
        let proc: *const ProcInner = &**inner.proc().unwrap();
        let args = inner.args().to_vec();
        let proc_data = ProcData::from_proc(inner.proc().unwrap());
        let root_lep = proc_data.outer();
        (inner.handle() as *mut Executor, proc, args, root_lep)
    };
    // SAFETY: `handle` and `proc` point into the THREAD RValue and the
    // Proc it holds, both rooted by the registry for the whole run; the
    // RValue arena does not move objects.
    let handle = unsafe { &mut *handle };
    let proc = unsafe { &*proc };
    // This kernel thread's stack is the primary context of the body:
    // the same budget as the process main thread's.
    handle.init_stack_limit(globals);
    // The body block's LEP belongs to the spawning frame; claim it as
    // this context's root svar scope (see `dispatch`).
    handle.enter_root_svar_scope(root_lep);
    let ret = handle.invoke_proc(globals, proc, &args);
    handle.set_terminated();
    let ret = match ret {
        Ok(v) => Some(v),
        Err(err) => {
            handle.set_error(err);
            None
        }
    };
    finalize(globals, thread, ret);
}

/// Park the current thread `cur` — which holds the GVL — until it is
/// woken, one of `fds` is ready, or `deadline` passes. Returns whether a
/// waker moved it out of `state` (as opposed to a timeout or a signal);
/// the caller re-checks its own condition either way.
fn park(
    vm: &mut Executor,
    globals: &mut Globals,
    mut cur: Value,
    state: ThreadState,
    fds: &[(i32, i16)],
    deadline: Option<Instant>,
) -> Result<bool> {
    let indefinite = fds.is_empty() && deadline.is_none();
    let is_main = SCHEDULER.with(|s| s.borrow().main == Some(cur));
    let wake_fd = {
        let inner = cur.as_thread_inner_mut();
        if inner.parker.is_none() {
            inner.parker = Some(Parker::new());
        }
        inner.state = state;
        inner.park_indefinite = indefinite;
        inner.park_fds = fds.iter().map(|(fd, _)| *fd).collect();
        inner.resume_exec = Some(std::ptr::NonNull::from(&mut *vm));
        inner.parker.as_ref().unwrap().fd()
    };
    if is_main {
        publish_main_exec(vm);
    }
    if indefinite && let Some(err) = check_deadlock(cur, is_main) {
        let inner = cur.as_thread_inner_mut();
        inner.state = ThreadState::Runnable;
        inner.park_indefinite = false;
        inner.resume_exec = None;
        return Err(err);
    }
    let mut pfds: Vec<libc::pollfd> = std::iter::once((wake_fd, libc::POLLIN))
        .chain(fds.iter().copied())
        .map(|(fd, events)| libc::pollfd {
            fd,
            events,
            revents: 0,
        })
        .collect();
    let timeout_ms: i32 = match deadline {
        Some(dl) => dl
            .saturating_duration_since(Instant::now())
            .as_millis()
            .min(i32::MAX as u128) as i32,
        None => -1,
    };
    release();
    // SAFETY: pfds is a valid array of pollfd for the duration of the call.
    let ret = unsafe { libc::poll(pfds.as_mut_ptr(), pfds.len() as _, timeout_ms) };
    let err = (ret < 0).then(std::io::Error::last_os_error);
    acquire(cur);
    // Under the GVL again: no waker can write to the pipe now, so a
    // drain leaves it empty for the next park.
    cur.as_thread_inner_mut().parker.as_ref().unwrap().drain();
    let unpark = |mut cur: Value| {
        let inner = cur.as_thread_inner_mut();
        let woken = inner.state != state;
        inner.state = ThreadState::Runnable;
        inner.park_indefinite = false;
        inner.park_fds.clear();
        inner.resume_exec = None;
        woken
    };
    if let Some(err) = err {
        if err.raw_os_error() == Some(libc::EINTR) {
            // A signal: run the poll point — on main, the trap handler
            // (`signal_delivery_ok`). It runs with the park state still
            // in place, so a wakeup it performs on this very thread
            // (`Thread.main.wakeup`, a `Queue#push` to the queue main
            // pops) counts as a wake, as it does in CRuby, where the
            // handler runs inside the sleep loop's interrupt check.
            let r = crate::executor::execute_gc(vm, globals);
            let woken = unpark(cur);
            if r.is_none() {
                return Err(vm.take_error());
            }
            return Ok(woken);
        }
        unpark(cur);
        return Err(MonorubyErr::ioerr(format!("poll failed: {err}")));
    }
    Ok(unpark(cur))
}

/// `cur` is about to park with nothing that could ever wake it but
/// another thread. If every other live thread is in the same position,
/// nothing will: the green scheduler's "No live threads left.
/// Deadlock?" — a fatal error in the main thread. Returned to a main
/// `cur` to raise in place; queued on main (and main woken) otherwise.
fn check_deadlock(cur: Value, is_main: bool) -> Option<MonorubyErr> {
    if ignore_deadlock() {
        return None;
    }
    let all_parked = SCHEDULER.with(|s| {
        s.borrow().threads.iter().all(|t| {
            let inner = t.as_thread_inner();
            *t == cur
                || inner.is_dead()
                || (matches!(inner.state(), ThreadState::Sleeping | ThreadState::Joining)
                    && inner.park_indefinite)
        })
    });
    if !all_parked {
        return None;
    }
    let err = MonorubyErr::fatal("No live threads left. Deadlock?".to_string());
    if is_main {
        return Some(err);
    }
    let mut main = SCHEDULER.with(|s| s.borrow().main.unwrap());
    main.as_thread_inner_mut()
        .pending
        .push_back(PendingInterrupt::Raise(err));
    wake_parked(main);
    None
}

/// `Kernel#sleep` / `Thread.stop`: see the green `sleep`.
pub(super) fn sleep(
    vm: &mut Executor,
    globals: &mut Globals,
    dur: Option<Duration>,
) -> Result<Duration> {
    ensure_main(vm);
    flush_pending_reports(vm, globals);
    let cur = current();
    deliver_pending_now(vm, globals, cur, true)?;
    set_park_blocking(cur, true);
    {
        let mut cur = cur;
        let inner = cur.as_thread_inner_mut();
        if inner.park_permit {
            inner.park_permit = false;
            return Ok(Duration::ZERO);
        }
    }
    let start = Instant::now();
    let deadline = dur.and_then(|d| start.checked_add(d));
    loop {
        let woken = park(vm, globals, cur, ThreadState::Sleeping, &[], deadline)?;
        if woken {
            break;
        }
        if let Some(dl) = deadline
            && Instant::now() >= dl
        {
            break;
        }
        // A signal interrupted the poll: sleep on for the remainder.
    }
    flush_pending_reports(vm, globals);
    deliver_pending_now(vm, globals, cur, true)?;
    Ok(start.elapsed())
}

/// `Thread.pass`: hand the GVL to the longest waiter, if any.
pub(super) fn pass(vm: &mut Executor, globals: &mut Globals) -> Result<()> {
    ensure_main(vm);
    flush_pending_reports(vm, globals);
    let cur = current();
    deliver_pending_now(vm, globals, cur, false)?;
    set_park_blocking(cur, false);
    if is_current_main() {
        publish_main_exec(vm);
    }
    yield_now(cur);
    flush_pending_reports(vm, globals);
    deliver_pending_now(vm, globals, cur, false)
}

/// `Thread#join`: see the green `join`.
pub(super) fn join(
    vm: &mut Executor,
    globals: &mut Globals,
    mut target: Value,
    timeout: Option<Duration>,
) -> Result<bool> {
    ensure_main(vm);
    let cur = current();
    if target == cur {
        return Err(MonorubyErr::threaderr(
            &globals.store,
            "Target thread must not be current thread",
        ));
    }
    let deadline = timeout.map(|d| Instant::now() + d);
    loop {
        flush_pending_reports(vm, globals);
        deliver_pending_now(vm, globals, cur, true)?;
        set_park_blocking(cur, true);
        if target.as_thread_inner().is_dead() {
            return Ok(true);
        }
        if let Some(dl) = deadline
            && Instant::now() >= dl
        {
            return Ok(false);
        }
        target.as_thread_inner_mut().joiners.push(cur);
        let res = park(vm, globals, cur, ThreadState::Joining, &[], deadline);
        // Finalization takes the joiners it wakes; a timeout or a signal
        // leaves ours behind.
        target.as_thread_inner_mut().joiners.retain(|j| *j != cur);
        res?;
    }
}

/// Wait for fd readiness (`wait_fd` / `wait_fds`): one park; the caller
/// re-checks readiness and loops.
pub(super) fn wait_fds(
    vm: &mut Executor,
    globals: &mut Globals,
    fds: &[(i32, i16)],
    deadline: Option<Instant>,
) -> Result<()> {
    ensure_main(vm);
    let cur = current();
    deliver_pending_now(vm, globals, cur, true)?;
    set_park_blocking(cur, true);
    park(vm, globals, cur, ThreadState::IoWaiting, fds, deadline)?;
    deliver_pending_now(vm, globals, cur, true)
}

/// Kill every other live thread at process exit and join their kernel
/// threads, so none outlives the interpreter (see the green
/// `terminate_all`). Bounded, like the green one: a thread that spins
/// in its ensure clause forever is left behind.
pub(super) fn terminate_all(vm: &mut Executor, globals: &mut Globals) {
    ensure_main(vm);
    if !is_current_main() {
        return;
    }
    let targets: Vec<Value> = SCHEDULER.with(|s| {
        let s = s.borrow();
        s.threads
            .iter()
            .copied()
            .filter(|t| Some(*t) != s.main && !t.as_thread_inner().is_dead())
            .collect()
    });
    if !targets.is_empty() {
        for mut t in targets.iter().copied() {
            t.as_thread_inner_mut()
                .pending
                .push_back(PendingInterrupt::Kill);
            if !wake_parked(t) {
                interrupt_blocking(t);
            }
        }
        publish_main_exec(vm);
        for _ in 0..10_000 {
            if targets.iter().all(|t| t.as_thread_inner().is_dead()) {
                break;
            }
            // Hand the lock over; when nobody is queued yet (a woken
            // thread still on its way out of `poll`), give it a moment.
            if !with_gvl(|gvl, _| gvl.has_waiter()) {
                with_gvl(|gvl, th| {
                    gvl.release(th);
                    std::thread::sleep(Duration::from_millis(1));
                    gvl.acquire(th);
                });
                SCHEDULER.with(|s| {
                    let main = s.borrow().main;
                    s.borrow_mut().current = main;
                });
            } else {
                yield_now(current());
            }
        }
    }
    // Reap the kernel threads of every dead thread: after finalization
    // they only release the lock and exit, so the join is short.
    let joins = SCHEDULER.with(|s| std::mem::take(&mut s.borrow_mut().native_joins));
    let mut kept = vec![];
    for (id, h) in joins {
        if thread_alive_by_id(id) {
            kept.push((id, h));
        } else {
            let _ = h.join();
        }
    }
    SCHEDULER.with(|s| s.borrow_mut().native_joins = kept);
    flush_pending_reports(vm, globals);
}

/// The forking thread is the child's only thread — and its main
/// thread. See the green `fork_child_reset_threads`.
pub(super) fn fork_child_reset_threads(cur: Option<Value>) {
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        let threads = s.threads.clone();
        for mut t in threads {
            if Some(t) != cur {
                t.as_thread_inner_mut().mark_dead_for_fork();
            }
        }
        if cur.is_some() {
            s.main = cur;
        }
        // The parent's kernel threads do not exist here; the handles
        // are dropped, never joined.
        s.native_joins.clear();
    });
    if let Some(mut cur) = cur {
        // The wake pipe is shared with the parent (fork copies the fd
        // table): a fresh one, as the native pool's completion pipe.
        let inner = cur.as_thread_inner_mut();
        if inner.parker.is_some() {
            inner.parker = Some(Parker::new());
        }
    }
    with_gvl(|gvl, th| gvl.reset_after_fork(th));
    // A non-main thread that forked is now the main thread, and must
    // receive the signals it used to leave to main.
    if GVL_THREAD.with(|t| t.borrow().is_some()) {
        // SAFETY: unblocking every signal on this (the only) thread.
        unsafe {
            let mut set: libc::sigset_t = std::mem::zeroed();
            libc::sigfillset(&mut set);
            libc::pthread_sigmask(libc::SIG_UNBLOCK, &set, std::ptr::null_mut());
        }
    }
}
