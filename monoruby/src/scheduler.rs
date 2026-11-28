//! The thread registry and the interrupt protocol.
//!
//! Each Ruby `Thread` is a kernel thread of its own, and the threads take
//! turns under the Global VM Lock (`crate::gvl`); `native` starts them,
//! parks them and hands the lock over (see scheduler/native.rs). This
//! module holds what does not depend on how a thread runs: the registry
//! of live threads (a GC root), `Thread.current` / `Thread.main`, the
//! queued asynchronous interrupts (`Thread#raise` / `#kill`) and their
//! `handle_interrupt` masks, and a thread's finalization —
//! `report_on_exception`, joiners, `abort_on_exception`.
//!
//! Only the GVL holder touches any of it, so `SCHEDULER` is a `RefCell`;
//! it is never borrowed across a GVL release, and no Ruby allocation
//! happens while it is borrowed (GC marking re-enters it).

pub(crate) mod native;

use std::time::{Duration, Instant};

use crate::alloc::GC;
use crate::value::rvalue::{InterruptTiming, PendingInterrupt, ThreadInner, ThreadState};
use crate::*;

/// The scheduler, one per interpreter ([`crate::vm::Vm`]).
pub(crate) use crate::vm::SCHEDULER;

pub(crate) struct Scheduler {
    /// All live (non-terminated) thread objects, including main — a GC
    /// root. Dead threads are pruned at finalization (user references
    /// keep them alive from the frames that hold them).
    threads: Vec<Value>,
    /// The thread currently executing. `None` until the main thread
    /// object is lazily created.
    current: Option<Value>,
    main: Option<Value>,
    /// The main thread's root `Executor`, published by main right before
    /// it lets go of the GVL (`native::publish_main_exec`), so that a
    /// collection another thread runs marks main's frames. `None` until
    /// main first releases the lock; dereferenced only while main is not
    /// the running thread.
    main_exec: Option<std::ptr::NonNull<Executor>>,
    /// Pre-rendered report_on_exception texts queued by thread
    /// finalization, delivered through the Ruby-level `$stderr` at the
    /// next safepoint of a thread that is running Ruby
    /// (`flush_pending_reports`).
    pending_reports: Vec<Vec<u8>>,
    /// Scheduler joiners (`ThreadInner::fiber_joiners`) of dead threads
    /// that the exit hook did not answer, as (joining thread, fiber);
    /// answered with the scheduler's `unblock` at the next safepoint
    /// (`flush_pending_reports`), like the reports.
    pending_unblocks: Vec<(Value, Value)>,
    /// Re-entrancy latch for `flush_pending_reports`: the `$stderr.write`
    /// invoke can itself park (full pipe), and the park entry is a flush
    /// point.
    flushing_reports: bool,
    /// The kernel threads, by Ruby thread object id, joined
    /// once their thread is dead (`native::terminate_all`).
    native_joins: Vec<(u64, std::thread::JoinHandle<()>)>,
    /// The threads `terminate_all` is killing, for as long as it waits
    /// for them (`Terminating`). A thread leaves `threads` the moment
    /// it is finalized, and a program rarely holds the object of a
    /// thread it left running (`Thread.new { .. }` with the result
    /// dropped), so nothing else keeps that cell through a collection
    /// one of the *other* dying threads runs in its ensure clause —
    /// while the wait loop on the main thread's Rust stack, which the
    /// collector does not scan, still reads its state (#2027).
    terminating: Vec<Value>,
    /// `Thread.ignore_deadlock` (the truthiness of the value last
    /// assigned, as CRuby stores it). While set, the deadlock detector
    /// is off and a program whose threads all wait forever simply
    /// waits — for a signal handler to wake one of them, the case the
    /// flag exists for.
    ignore_deadlock: bool,
}

impl Scheduler {
    pub(crate) fn new() -> Self {
        Self {
            threads: vec![],
            current: None,
            main: None,
            main_exec: None,
            pending_reports: vec![],
            pending_unblocks: vec![],
            flushing_reports: false,
            native_joins: vec![],
            terminating: vec![],
            ignore_deadlock: false,
        }
    }

    pub(crate) fn mark(&self, alloc: &mut crate::alloc::Allocator<RValue>) {
        for t in &self.threads {
            t.mark(alloc);
        }
        // `current`/`main` and queue members are subsets of `threads`
        // except during pruning, so mark them too for safety.
        if let Some(t) = self.current {
            t.mark(alloc);
        }
        if let Some(t) = self.main {
            t.mark(alloc);
        }
        for (t, f) in &self.pending_unblocks {
            t.mark(alloc);
            f.mark(alloc);
        }
        for t in &self.terminating {
            t.mark(alloc);
        }
        // Main publishes its executor before every release of the GVL,
        // so the pointer is fresh exactly while main is *not* the
        // running thread. While main runs it is the collector's own
        // root, and a pointer published before `Executor::init` moved
        // the executor out would be stale.
        if self.current != self.main
            && let Some(main_exec) = self.main_exec
        {
            // SAFETY: refreshed at every GVL release by main; main's
            // executor is live while another thread runs.
            unsafe { main_exec.as_ref().mark(alloc) };
        }
    }
}

/// Reset all scheduler state. Called when a fresh interpreter starts
/// (`Executor::init`) so state from a previous interpreter on the same
/// OS thread (e.g. the test harness) cannot leak across.
pub(crate) fn reset() {
    SCHEDULER.with(|s| *s.borrow_mut() = Scheduler::new());
}

/// GC root hook (called from `Root::mark`).
pub(crate) fn mark(alloc: &mut crate::alloc::Allocator<RValue>) {
    SCHEDULER.with(|s| s.borrow().mark(alloc));
}

/// Whether a poll point may run signal trap handlers here: only on the
/// main thread — at any of its poll points, including the `EINTR` return
/// of its own park or GVL-less region — which runs the handler on its
/// own kernel stack; any other thread leaves the signal pending for
/// main (the kernel delivers it to main's kernel thread, since the
/// others block it).
pub(crate) fn signal_delivery_ok() -> bool {
    SCHEDULER.with(|s| {
        let s = s.borrow();
        s.current.zip(s.main).is_none_or(|(c, m)| c.id() == m.id())
    })
}

/// Whether a timeslice switch is worth injecting at the current poll
/// point: only when another live thread could actually use the slice.
pub(crate) fn preempt_ok() -> bool {
    SCHEDULER.with(|s| s.borrow().main.is_some()) && has_other_live_threads()
}

/// `Thread.ignore_deadlock`: whether the deadlock detector is off.
pub(crate) fn ignore_deadlock() -> bool {
    SCHEDULER.with(|s| s.borrow().ignore_deadlock)
}

pub(crate) fn set_ignore_deadlock(on: bool) {
    SCHEDULER.with(|s| s.borrow_mut().ignore_deadlock = on);
}

/// Tell the preempt timer how many live threads exist. Computed under
/// the borrow, reported outside it.
fn notify_thread_count() {
    let live = SCHEDULER.with(|s| {
        s.borrow()
            .threads
            .iter()
            .filter(|t| !t.as_thread_inner().is_dead())
            .count()
    });
    crate::preempt::on_thread_count(live);
}

/// Forensics (`MONORUBY_GC_BREAK`): one-line dump of the scheduler state
/// as seen by the GC root scan.
pub(crate) fn dump_state_for_gc() -> String {
    SCHEDULER.with(|s| {
        let s = s.borrow();
        format!(
            "main_exec={:?} main_exec_parent_fiber={:?} current={:016x} main={:016x} threads={}",
            s.main_exec,
            // SAFETY: forensics-only read; main_exec is live while set.
            s.main_exec
                .map(|e| unsafe { e.as_ref().parent_fiber() }),
            s.current.map(|v| v.id()).unwrap_or(0),
            s.main.map(|v| v.id()).unwrap_or(0),
            s.threads.len(),
        ) + &s
            .main
            .map(|m| {
                let inner = m.as_thread_inner();
                format!(
                    " main_state={:?} main_resume_exec={:?} main_resume_parent_fiber={:?}",
                    inner.state,
                    inner.resume_exec,
                    // SAFETY: forensics-only read; resume_exec is live
                    // while the thread is parked.
                    inner
                        .resume_exec
                        .map(|e| unsafe { e.as_ref().parent_fiber() }),
                )
            })
            .unwrap_or_default()
    })
}

/// The `Thread` object for the currently running thread, creating the
/// main thread object lazily on first use.
pub(crate) fn current_thread(vm: &mut Executor) -> Value {
    // `current` is set from the moment the main thread object exists
    // (`ensure_main` sets both), so one look answers nearly every call:
    // this is `Thread.current`, and what `Mutex` and `Monitor` ask on
    // every lock and unlock.
    if let Some(cur) = SCHEDULER.with(|s| s.borrow().current) {
        return cur;
    }
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().current.unwrap())
}

/// Object id of the currently running thread, or `0` before the
/// thread machinery has been touched at all.
///
/// Unlike [`current_thread`] this never creates the main thread
/// object, so it is callable from `Store` and `ClassInfo` — which are
/// handed no `Executor` — on the autoload paths that have to tell the
/// thread running a load from every other one. Those paths only ever
/// compare it against an owner recorded by [`current_thread`], which
/// does create the main object, so the `0` stands for "no thread
/// machinery yet, hence no autoload in flight either" and never
/// collides with a real thread.
pub(crate) fn current_thread_id() -> u64 {
    SCHEDULER.with(|s| s.borrow().current.map(|t| t.id()).unwrap_or(0))
}

/// The main thread's `Thread` object.
pub(crate) fn main_thread(vm: &mut Executor) -> Value {
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().main.unwrap())
}

/// The main thread's root executor, once main has let go of the GVL at
/// least once. Used by `Thread#backtrace` from another thread to read
/// main's frame chain (main is quiescent until it gets the GVL back).
pub(crate) fn main_exec_ptr() -> Option<std::ptr::NonNull<Executor>> {
    SCHEDULER.with(|s| s.borrow().main_exec)
}

/// The executors of every thread that is not the running one and has
/// frames: the context each is suspended in (`resume_exec`, or the
/// published root for a parked main) followed by the fiber contexts it
/// is nested in, root last. Every one is quiescent while the caller
/// runs — a thread does nothing until it gets the GVL — so their frame
/// chains can be read and rewritten in place. What a basic-op
/// redefinition walks to convert the compiled frames of the *other*
/// threads (`Codegen::check_bop_redefine`), the way it converts the
/// redefining thread's.
///
/// A thread that has not started, or is done, has no frames.
pub(crate) fn suspended_executors() -> Vec<std::ptr::NonNull<Executor>> {
    SCHEDULER.with(|s| {
        let s = s.borrow();
        let mut execs = vec![];
        for &t in &s.threads {
            if Some(t) == s.current {
                continue;
            }
            let inner = t.as_thread_inner();
            if matches!(inner.state(), ThreadState::Created | ThreadState::Dead) {
                continue;
            }
            let start = inner
                .resume_exec
                .or_else(|| (Some(t) == s.main).then_some(s.main_exec).flatten());
            let mut cur = start;
            while let Some(exec) = cur {
                execs.push(exec);
                // SAFETY: a suspended executor is live, and the
                // `parent_fiber` links of a nested fiber point at the
                // live executors that resumed it.
                cur = unsafe { exec.as_ref().parent_fiber() };
            }
        }
        execs
    })
}

/// `$?` / `Process.last_status` of the current thread (CRuby keeps the
/// last child status per thread).
pub(crate) fn last_status(vm: &mut Executor) -> Option<Value> {
    let thread = current_thread(vm);
    thread.as_thread_inner().last_status
}

/// Record a reaped child's `Process::Status` as the current thread's
/// `$?` / `Process.last_status`.
pub(crate) fn set_last_status(vm: &mut Executor, status: Value) {
    let mut thread = current_thread(vm);
    thread.as_thread_inner_mut().last_status = Some(status);
}

/// Forget the current thread's `$?` / `Process.last_status` (CRuby's
/// `rb_last_status_clear`): a wait that reaped nothing leaves no status
/// behind, and neither does one that has not finished.
pub(crate) fn clear_last_status(vm: &mut Executor) {
    let mut thread = current_thread(vm);
    thread.as_thread_inner_mut().last_status = None;
}

/// All alive threads (`Thread.list`).
pub(crate) fn thread_list(vm: &mut Executor) -> Vec<Value> {
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().threads.clone())
}

/// After `fork`, only the forking thread survives in the child: mark
/// every other registered thread dead (CRuby's fork semantics — their
/// native contexts belong to the parent).
pub(crate) fn fork_child_reset_threads(vm: &mut Executor) {
    let cur = SCHEDULER.with(|s| s.borrow().current);
    let _ = vm;
    native::fork_child_reset_threads(cur)
}

/// The calling kernel thread's id as the OS reports it (`gettid(2)`;
/// `pthread_threadid_np` on Darwin): what `Thread#native_thread_id`
/// returns.
pub(crate) fn current_kernel_tid() -> i64 {
    #[cfg(target_os = "linux")]
    {
        // SAFETY: gettid(2) takes no arguments and cannot fail.
        unsafe { libc::syscall(libc::SYS_gettid) as i64 }
    }
    #[cfg(target_os = "macos")]
    {
        let mut id: u64 = 0;
        // SAFETY: pthread_self() is a valid thread; `id` is a valid out
        // pointer for the call's duration.
        unsafe { libc::pthread_threadid_np(libc::pthread_self(), &mut id) };
        id as i64
    }
}

/// `Thread#native_thread_id`: the kernel thread `thread` runs on, or
/// `None` once it is dead (and until its body has been dispatched).
pub(crate) fn native_thread_id(thread: Value) -> Option<i64> {
    let inner = thread.as_thread_inner();
    if inner.is_dead() {
        return None;
    }
    inner.native_tid
}

/// Whether a live (not dead) thread with the given object id exists.
/// Used by the `require` load lock to detect a stale loading marker
/// left by a thread that died without unwinding.
pub(crate) fn thread_alive_by_id(id: u64) -> bool {
    SCHEDULER.with(|s| {
        s.borrow()
            .threads
            .iter()
            .any(|t| t.id() == id && !t.as_thread_inner().is_dead())
    })
}

/// Whether any *other* live thread exists (i.e. whether blocking
/// operations must go through the scheduler instead of really blocking).
pub(crate) fn has_other_live_threads() -> bool {
    SCHEDULER.with(|s| {
        let s = s.borrow();
        let cur = s.current;
        s.threads
            .iter()
            .any(|t| Some(*t) != cur && !t.as_thread_inner().is_dead())
    })
}

fn ensure_main(vm: &mut Executor) {
    let need = SCHEDULER.with(|s| s.borrow().main.is_none());
    if need {
        // Allocate before borrowing (allocation can trigger GC, which
        // re-enters the scheduler to mark it).
        let main = Value::new_thread(THREAD_CLASS, ThreadInner::main());
        SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            if s.main.is_none() {
                s.threads.push(main);
                s.main = Some(main);
                s.current = Some(main);
            }
        });
    }
    let _ = vm;
}

fn is_current_main() -> bool {
    SCHEDULER.with(|s| {
        let s = s.borrow();
        s.current.is_some() && s.current == s.main
    })
}

/// Register a freshly created thread object and start its kernel thread.
pub(crate) fn spawn(vm: &mut Executor, globals: &mut Globals, thread: Value) -> Result<()> {
    native::spawn(vm, globals, thread)
}

/// Wake a thread parked in `sleep` (`Thread#wakeup` / `#run`). Returns
/// false if the thread is dead.
///
/// A thread blocked in a C extension call that registered an unblock
/// function (`rb_thread_call_without_gvl`) is woken through it, as
/// CRuby's `rb_thread_wakeup` interrupts a blocking region.
pub(crate) fn wakeup(thread: Value) -> bool {
    if thread.as_thread_inner().blocking_ubf.is_some() && !thread.as_thread_inner().is_dead() {
        unblock(thread);
        return true;
    }
    wakeup_inner(thread, false)
}

/// An extension's unblock function and its argument: CRuby's
/// `rb_unblock_function_t` / `data2` of `rb_thread_call_without_gvl`.
/// `func == None` is `RUBY_UBF_IO`: nothing to call, the blocked
/// syscall is cut short by the unblocking signal instead.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Ubf {
    pub(crate) func: Option<unsafe extern "C" fn(*mut std::ffi::c_void)>,
    pub(crate) arg: usize,
    /// `RUBY_UBF_IO` only: an interrupt has asked for the call to end.
    /// Its signal can land before the call's syscall starts and be
    /// lost, so a joiner keeps re-sending it while this is set and the
    /// thread is still blocked (`native::join`), as CRuby's timer
    /// thread re-runs `ubf_select`.
    pub(crate) requested: bool,
}

/// Interrupt `thread`, which is blocked in an extension call with an
/// unblock function: through `interrupt_blocking` (the unblocking
/// signal, then the function).
fn unblock(thread: Value) {
    if !native::interrupt_blocking(thread) {
        // Not inside the GVL-less region yet: the function alone (it
        // makes the call return whenever it starts).
        run_ubf(thread);
    }
}

/// A signal is pending and the current thread, not main, is running
/// (holding the GVL). If main is blocked in an extension call with an
/// unblock function, run it so main can come back and convert the
/// signal: the kernel's `EINTR` alone is retried by a C loop that
/// waits for its own wakeup (CRuby interrupts main the same way when a
/// signal arrives, `rb_threadptr_interrupt`).
pub(crate) fn unblock_main_for_signal() {
    let main = SCHEDULER.with(|s| s.borrow().main);
    if let Some(main) = main
        && main.as_thread_inner().blocking_ubf.is_some()
    {
        unblock(main);
    }
}

/// Run `thread`'s unblock function, at most once per blocking call
/// (a second call could write to an fd the first one closed, as
/// ruby/spec's own `unblock_gvl_func` does). Called with the GVL held,
/// as CRuby calls it; the function must not touch the interpreter.
pub(crate) fn run_ubf(mut thread: Value) {
    let ubf = &mut thread.as_thread_inner_mut().blocking_ubf;
    match ubf {
        Some(Ubf {
            func: Some(func),
            arg,
            ..
        }) => {
            let (func, arg) = (*func, *arg);
            *ubf = None;
            // SAFETY: the extension's own unblock function with its own
            // argument, registered for exactly this blocking call.
            unsafe { func(arg as *mut std::ffi::c_void) };
        }
        // RUBY_UBF_IO: the signal is the whole unblocking; remember that
        // it was asked for, so it can be re-sent.
        Some(Ubf {
            func: None,
            requested,
            ..
        }) => *requested = true,
        None => {}
    }
}

/// `thread` is blocked in an extension call whose `RUBY_UBF_IO`
/// unblocking was asked for and has not taken effect yet.
pub(crate) fn ubf_io_requested(thread: Value) -> bool {
    matches!(
        thread.as_thread_inner().blocking_ubf,
        Some(Ubf {
            func: None,
            requested: true,
            ..
        })
    )
}

/// Internal wake used by the Ruby-level synchronization primitives
/// (Mutex#unlock, ConditionVariable#signal, the Queue wake helpers) via
/// `Thread#__wakeup_permit`. Unlike the public `Thread#wakeup` — for
/// which CRuby specifies that waking a *running* thread has no effect
/// on its future sleeps — this arms the target's park permit when it is
/// not parked yet: under preemption a waiter can be preempted between
/// registering itself and parking, and the primitive's wake landing in
/// that window would otherwise be lost, leaving the waiter parked
/// forever (see `ThreadInner::park_permit`).
pub(crate) fn wakeup_permit(thread: Value) -> bool {
    wakeup_inner(thread, true)
}

fn wakeup_inner(mut thread: Value, permit: bool) -> bool {
    let inner = thread.as_thread_inner_mut();
    match inner.state() {
        ThreadState::Dead => false,
        ThreadState::Sleeping => {
            native::wake_parked(thread);
            true
        }
        ThreadState::Runnable if permit => {
            inner.park_permit = true;
            true
        }
        // Public wakeup on a running thread, or Created / Joining:
        // nothing to do (CRuby's wakeup only interrupts sleep).
        _ => true,
    }
}

/// `Kernel#sleep` / `Thread.stop` through the scheduler. `dur == None`
/// sleeps until `#wakeup`. Returns the elapsed time.
pub(crate) fn sleep(
    vm: &mut Executor,
    globals: &mut Globals,
    dur: Option<Duration>,
) -> Result<Duration> {
    sleep_woken(vm, globals, dur).map(|(elapsed, _)| elapsed)
}

/// `sleep`, also answering what ended it: `true` for a wake-up
/// (`Thread#wakeup` / `#run`, a `Mutex` or `ConditionVariable` waking its
/// waiter, a park permit left by one that raced the park), `false` for
/// `dur` running out. `Mutex#sleep` returns `nil` for the latter, and
/// `MonitorMixin::ConditionVariable#wait` `false`.
pub(crate) fn sleep_woken(
    vm: &mut Executor,
    globals: &mut Globals,
    dur: Option<Duration>,
) -> Result<(Duration, bool)> {
    native::sleep(vm, globals, dur)
}

/// Roots the threads `terminate_all` is killing (`Scheduler::terminating`)
/// until it is dropped. A stack discipline rather than a plain clear, so
/// a nested entry cannot unroot the outer one's targets.
struct Terminating {
    base: usize,
}

impl Terminating {
    fn root(targets: &[Value]) -> Self {
        SCHEDULER.with(|s| {
            let terminating = &mut s.borrow_mut().terminating;
            let base = terminating.len();
            terminating.extend_from_slice(targets);
            Terminating { base }
        })
    }
}

impl Drop for Terminating {
    fn drop(&mut self) {
        SCHEDULER.with(|s| s.borrow_mut().terminating.truncate(self.base));
    }
}

/// Terminate every live thread except main at process exit, the way
/// CRuby's `rb_thread_terminate_all` does after the `at_exit` handlers:
/// each thread is killed — an unrescuable unwind that runs the ensure
/// clauses of its *current* fiber chain only; suspended fibers are
/// never resumed — and main hands them the GVL until they have all
/// unwound.
/// `handle_interrupt` masks are bypassed: the process is ending, so
/// there is no later delivery point to defer to.
pub(crate) fn terminate_all(vm: &mut Executor, globals: &mut Globals) {
    native::terminate_all(vm, globals)
}

/// `Thread.pass`: hand the GVL to the longest waiter, if any.
pub(crate) fn pass(vm: &mut Executor, globals: &mut Globals) -> Result<()> {
    native::pass(vm, globals)
}

/// `Thread#join` (also backs `#value`): park until `target` terminates
/// or the timeout expires. Returns whether the target is dead.
pub(crate) fn join(
    vm: &mut Executor,
    globals: &mut Globals,
    target: Value,
    timeout: Option<Duration>,
) -> Result<bool> {
    native::join(vm, globals, target, timeout)
}

/// The effective `handle_interrupt` timing for one queued interrupt on
/// a thread. Kill is unmaskable. For a raise, the innermost mask entry
/// whose class covers the exception's class wins; default `Immediate`.
fn interrupt_timing(store: &Store, inner: &ThreadInner, int: &PendingInterrupt) -> InterruptTiming {
    // A `Thread.__uninterruptible` section defers EVERYTHING, kill
    // included: nothing is deliverable and nothing is wake-worthy until
    // the section exits (where the deferred queue fires) — the
    // `mutex_lock_uninterruptible` analogue.
    if inner.uninterruptible > 0 {
        return InterruptTiming::Never;
    }
    let exc_class = match int {
        PendingInterrupt::Kill => return InterruptTiming::Immediate,
        PendingInterrupt::Raise(err) => err.class_id(),
    };
    for frame in inner.masks.iter().rev() {
        for (mask_class, timing) in frame {
            let Some(mask_module) = mask_class.is_class_or_module() else {
                continue;
            };
            // Class-hierarchy match: the exception class or any of its
            // superclasses equals the mask class.
            let mut m = Some(store.get_module(exc_class));
            while let Some(cur) = m {
                if cur.id() == mask_module.id() {
                    return *timing;
                }
                m = cur.superclass();
            }
        }
    }
    InterruptTiming::Immediate
}

/// Index of the first queued interrupt deliverable in this context.
/// FIFO with per-element masking, like CRuby's `pending_interrupt_queue`:
/// masked elements are skipped (they stay queued), the first allowed one
/// is delivered.
fn deliverable_index(store: &Store, thread: Value, at_blocking: bool) -> Option<usize> {
    let inner = thread.as_thread_inner();
    inner
        .pending
        .iter()
        .position(|int| match interrupt_timing(store, inner, int) {
            InterruptTiming::Immediate => true,
            InterruptTiming::OnBlocking => at_blocking,
            InterruptTiming::Never => false,
        })
}

/// Whether any queued interrupt warrants waking a parked target (i.e. is
/// not masked `:never`; a parked thread counts as blocking, so
/// `:on_blocking` qualifies).
fn wake_worthy(store: &Store, thread: Value) -> bool {
    let inner = thread.as_thread_inner();
    inner
        .pending
        .iter()
        .any(|int| interrupt_timing(store, inner, int) != InterruptTiming::Never)
}

/// Remove queue element `idx` and turn it into the error to raise.
fn take_pending_as_error(mut thread: Value, idx: usize) -> Option<MonorubyErr> {
    let inner = thread.as_thread_inner_mut();
    match inner.pending.remove(idx)? {
        PendingInterrupt::Kill => {
            inner.killed = true;
            Some(kill_unwind_error())
        }
        PendingInterrupt::Raise(err) => Some(err),
    }
}

/// Deliver the current thread's first deliverable pending interrupt
/// *now*, per each element's mask timing in this context (`at_blocking`:
/// a real blocking operation vs `Thread.pass` / handle_interrupt
/// boundaries). Called at every park entry and at handle_interrupt
/// entry/exit.
pub(crate) fn deliver_pending_now(
    vm: &mut Executor,
    globals: &mut Globals,
    thread: Value,
    at_blocking: bool,
) -> Result<()> {
    if let Some(idx) = deliverable_index(&globals.store, thread, at_blocking)
        && let Some(err) = take_pending_as_error(thread, idx)
    {
        // Killing the main thread terminates the process.
        let is_main = SCHEDULER.with(|s| s.borrow().main) == Some(thread);
        if is_main && thread.as_thread_inner().killed {
            let mut t = thread;
            t.as_thread_inner_mut().killed = false;
            return Err(MonorubyErr::new(MonorubyErrKind::SystemExit(0), "exit"));
        }
        return Err(finalize_delivered_raise(vm, globals, err));
    }
    Ok(())
}

/// Target-side half of CRuby's `Thread#raise` protocol: delivery runs
/// the queued exception object through `#exception` (no arguments)
/// *in the receiving thread's context* — `rb_make_exception` executes
/// once in the caller (with the message) and once here, so a user
/// override observes both calls, and the delivered object is the
/// second call's result. Errors carry the caller-pinned `cause:`
/// across. Kills and message-only raises (no original object) pass
/// through unchanged.
fn finalize_delivered_raise(
    vm: &mut Executor,
    globals: &mut Globals,
    err: MonorubyErr,
) -> MonorubyErr {
    let Some(obj) = err.original else {
        return err;
    };
    if obj.is_exception().is_none() {
        return err;
    }
    match vm.invoke_method_inner(globals, IdentId::get_id("exception"), obj, &[], None, None) {
        Ok(result) => match result.is_exception() {
            Some(ex) => {
                let mut new_err = MonorubyErr::new_from_exception(ex).with_original(result);
                new_err.explicit_cause = err.explicit_cause;
                new_err
            }
            None => MonorubyErr::typeerr("exception object expected"),
        },
        // A raising `#exception` override: its error is what the
        // target sees.
        Err(e) => e,
    }
}

/// Whether `thread` has a queued pending interrupt (optionally filtered
/// by exception class) — `Thread.pending_interrupt?`.
pub(crate) fn pending_interrupt_p(store: &Store, thread: Value, filter: Option<Module>) -> bool {
    let inner = thread.as_thread_inner();
    let Some(filter) = filter else {
        return !inner.pending.is_empty();
    };
    inner.pending.iter().any(|int| {
        let exc_class = match int {
            PendingInterrupt::Kill => return true,
            PendingInterrupt::Raise(err) => err.class_id(),
        };
        let mut m = Some(store.get_module(exc_class));
        while let Some(cur) = m {
            if cur.id() == filter.id() {
                return true;
            }
            m = cur.superclass();
        }
        false
    })
}

/// Enter / leave a `Thread.__uninterruptible` section on `thread`
/// (a depth counter, so nesting composes).
pub(crate) fn set_uninterruptible(mut thread: Value, on: bool) {
    let inner = thread.as_thread_inner_mut();
    if on {
        inner.uninterruptible += 1;
    } else {
        inner.uninterruptible -= 1;
    }
}

/// Push / pop a `handle_interrupt` mask frame on the current thread.
pub(crate) fn push_interrupt_mask(vm: &mut Executor, mask: Vec<(Value, InterruptTiming)>) {
    let mut cur = current_thread(vm);
    cur.as_thread_inner_mut().masks.push(mask);
}

pub(crate) fn pop_interrupt_mask(vm: &mut Executor) {
    let mut cur = current_thread(vm);
    cur.as_thread_inner_mut().masks.pop();
}

/// Queue an asynchronous interrupt (`Thread#kill` / `#raise`) for
/// `target` and wake it if parked. An interrupt aimed at the *current*
/// thread is returned as `Err` for the caller to raise in place.
pub(crate) fn interrupt(
    vm: &mut Executor,
    globals: &mut Globals,
    mut target: Value,
    int: PendingInterrupt,
) -> Result<()> {
    ensure_main(vm);
    // The kill unwind tag is allocated *before* borrowing the scheduler
    // (allocation can trigger GC, which re-enters it for marking).
    let cur = SCHEDULER.with(|s| s.borrow().current.unwrap());
    if target == cur {
        // Self-targeted interrupts honor the current handle_interrupt
        // mask: only `:immediate` delivers in place (at_blocking =
        // false), everything else stays queued for a later delivery
        // point.
        target.as_thread_inner_mut().pending.push_back(int);
        return deliver_pending_now(vm, globals, target, false);
    }
    if target.as_thread_inner().is_dead() {
        // CRuby: killing a dead thread is a no-op; raising into one is
        // also accepted (the exception is simply lost).
        return Ok(());
    }
    // Append, never overwrite: `t.raise e; t.kill` queues both, and the
    // raise is delivered first (FIFO) — the thread dies *with* the
    // exception, so report_on_exception / #join see it (CRuby).
    target.as_thread_inner_mut().pending.push_back(int);
    // Wake a parked target so delivery happens promptly — unless every
    // queued interrupt is masked `:never`; a parked thread counts as
    // blocking, so `:on_blocking` wakes too.
    if wake_worthy(&globals.store, target) && !native::wake_parked(target) {
        native::interrupt_blocking(target);
    }
    Ok(())
}

/// The unrescuable unwind used to deliver `Thread#kill`: a `Throw` whose
/// tag is a fresh object no `catch` can ever match, so it runs every
/// ensure clause on the way out but passes through every rescue — and is
/// recognized in `finalize` as a clean death.
fn kill_unwind_error() -> MonorubyErr {
    MonorubyErr::throw(Value::object(OBJECT_CLASS), Value::nil())
}

/// Run `f` — a syscall that blocks in the kernel with no fd to poll,
/// such as `waitpid(2)` or `getaddrinfo(3)` — without the GVL, so the
/// other Ruby threads keep running meanwhile. `f` must not touch the
/// interpreter: no `Value`, no
/// `Globals`, only raw fds, integers and its own buffers. An interrupt
/// queued for the calling thread while it is inside `f` makes the
/// syscall return `EINTR` (`native::interrupt_blocking`); the caller's
/// `EINTR` handling — the poll point, then retry — is where it is
/// delivered.
pub(crate) fn without_gvl<R>(vm: &mut Executor, f: impl FnOnce() -> R) -> R {
    native::without_gvl(vm, f)
}

/// Empty the calling thread's signal mask — in a forked child before
/// `execve`, or in the process about to `exec`. Async-signal-safe.
pub(crate) fn reset_child_sigmask() {
    // SAFETY: a signal set built on the stack and applied to this thread.
    unsafe {
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut set);
        libc::pthread_sigmask(libc::SIG_SETMASK, &set, std::ptr::null_mut());
    }
}

/// Park the current thread until `fd` reports one of `events` (or the
/// deadline passes, or another thread wakes/interrupts it). The caller
/// re-checks readiness afterwards — spurious wakeups are fine.
pub(crate) fn wait_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    fd: i32,
    events: i16,
    deadline: Option<Instant>,
) -> Result<()> {
    wait_fds(vm, globals, &[(fd, events)], deadline)
}

/// Multi-fd variant of [`wait_fd`] (backs `IO.select`).
pub(crate) fn wait_fds(
    vm: &mut Executor,
    globals: &mut Globals,
    fds: &[(i32, i16)],
    deadline: Option<Instant>,
) -> Result<()> {
    native::wait_fds(vm, globals, fds, deadline)
}

/// An fd is about to be closed (`FileDescriptor::drop`): a kernel
/// thread blocked in `poll(2)` on it has to be woken.
pub(crate) fn fd_closing(fd: i32) {
    native::fd_closing(fd);
}

/// The thread-root executor of `vm`'s fiber-parent chain.
///
/// GC root marking must always start from the thread-root executor: a
/// fiber's frame chain is reached *from* the frame that holds the Fiber
/// object (the resumer), never the other way round — `Executor::mark`
/// walks only its own cfp chain and does not cross fiber boundaries.
/// `execute_gc` performs this same walk for the triggering executor;
/// capturing `main_exec` from a park that happened *inside* a fiber
/// (e.g. `sleep` in an Enumerator body) without the walk would publish
/// the fiber's executor instead, leaving every frame of the main
/// thread's root chain invisible to a GC another thread runs — a
/// use-after-free of anything referenced only by those frames.
fn root_exec(vm: &mut Executor) -> std::ptr::NonNull<Executor> {
    let mut cur = std::ptr::NonNull::from(&mut *vm);
    // SAFETY: `parent_fiber` links point at live executors while the
    // child fiber is running (the resumer is suspended on its stack).
    while let Some(parent) = unsafe { cur.as_ref().parent_fiber() } {
        cur = parent;
    }
    cur
}

/// A thread killed or raised-into before its body ever ran: mark it
/// dead without running the body (CRuby semantics for an unstarted
/// target), record the raise as its terminating exception, wake joiners.
fn finalize_unstarted(globals: &mut Globals, mut thread: Value, err: Option<MonorubyErr>) {
    {
        let inner = thread.as_thread_inner_mut();
        inner.killed = err.is_none();
        inner.exception = err;
        inner.result = None;
    }
    finalize_common(globals, thread);
}

/// A thread's body finished: record the outcome, wake joiners, prune the
/// registry.
fn finalize(globals: &mut Globals, mut thread: Value, ret: Option<Value>) {
    {
        let inner = thread.as_thread_inner_mut();
        match ret {
            Some(v) => inner.result = Some(v),
            None => {
                let err = inner.take_error();
                if inner.killed && matches!(err.kind(), MonorubyErrKind::Throw(..)) {
                    // The kill unwind arrived at the thread root: a clean
                    // death (status false, join returns, nothing re-raised).
                    inner.result = None;
                } else {
                    // A bare `return` / `break` escaping a thread body has
                    // no enclosing frame on the thread's own stack:
                    // LocalJumpError (CRuby).
                    let err = match err.kind() {
                        MonorubyErrKind::MethodReturn(val, _) => {
                            MonorubyErr::localjumperr_with_val("unexpected return", *val)
                        }
                        MonorubyErrKind::BlockBreak(val, ..) => {
                            MonorubyErr::break_from_proc_closure(*val)
                        }
                        _ => err,
                    };
                    inner.exception = Some(err);
                }
            }
        }
    }
    finalize_common(globals, thread);
}

/// Shared tail of thread termination: mark dead, wake joiners, report an
/// unhandled exception per report_on_exception, prune the registry.
fn finalize_common(globals: &mut Globals, mut thread: Value) {
    // Undelivered interrupts die with the thread (CRuby drops the
    // remaining pending_interrupt_queue too) — e.g. the kill queued
    // behind the raise the thread just died from.
    thread.as_thread_inner_mut().pending.clear();
    // Render the report_on_exception text now (pure Rust — the thread's
    // executor has terminated, so no Ruby runs here). The text is queued
    // and written at the next safepoint of a running thread by
    // `flush_pending_reports`. Rendered before the thread is marked dead
    // so the header shows status "run", like CRuby.
    queue_exception_report(globals, thread);
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        let inner = thread.as_thread_inner_mut();
        inner.state = ThreadState::Dead;
        let joiners = std::mem::take(&mut inner.joiners);
        for j in joiners {
            if j.as_thread_inner().state() == ThreadState::Joining {
                native::wake_parked(j);
            }
        }
        // Scheduler joiners the exit hook did not take (the body never
        // ran, or they registered after it had taken the list): answered
        // at the next safepoint of a running thread — not here, where no
        // Ruby may run.
        let left = std::mem::take(&mut inner.fiber_joiners);
        s.pending_unblocks.extend(left);
        s.threads.retain(|t| *t != thread);
    });
    notify_thread_count();
    // A `SystemExit` that escapes any thread terminates the process: CRuby
    // re-raises it in the main thread (`Kernel#exit` from a thread exits
    // the interpreter once main handles it). The same forwarding applies
    // to *any* terminating exception when the thread — or the `Thread`
    // global default — has `abort_on_exception` set. The exception stays
    // recorded on the dead thread too (`#join` / `#value` re-raise it).
    forward_exception_to_main(globals, thread);
}

/// `Thread#report_on_exception` (instance flag, falling back to the
/// `Thread` class-level default; both default true): render the report
/// for a thread that died with an unhandled exception and queue it for
/// delivery. A kill never reports, and neither does a `SystemExit`
/// (CRuby reports an abort_on_exception forwarding, but never a
/// SystemExit — that is the process exiting, not a crash). Pure Rust —
/// no Ruby is invoked here (see the caller comment).
fn queue_exception_report(globals: &mut Globals, thread: Value) {
    let exc = {
        let inner = thread.as_thread_inner();
        match &inner.exception {
            None => return,
            Some(err) if matches!(err.kind(), MonorubyErrKind::SystemExit(_)) => return,
            Some(err) => {
                // Regular backtrace order (CRuby `rb_error_write` with
                // the non-highlighted, non-reversed layout): the
                // innermost frame heads the message line, the rest
                // follow as `\tfrom` lines.
                // The message line is `#detailed_message`'s, rendered by
                // the thread's exit hook (`render_exception_report`).
                // Its bytes as they are, whatever its encoding (#2246).
                let head = err.get_error_message_bytes(&globals.store);
                let frames: Vec<String> = err
                    .trace
                    .iter()
                    .map(|entry| globals.store.trace_entry_location(entry))
                    .collect();
                match frames.split_first() {
                    None => head,
                    Some((first, rest)) => {
                        let mut text = format!("{first}: ").into_bytes();
                        text.extend_from_slice(&head);
                        for f in rest {
                            text.extend_from_slice(b"\n\tfrom ");
                            text.extend_from_slice(f.as_bytes());
                        }
                        text
                    }
                }
            }
        }
    };
    if !reports_on_exception(globals, thread) {
        return;
    }
    // The header mirrors Thread#inspect
    // (`#<Thread:0xADDR[@name][ file:line] run>`) without calling it —
    // the spec matchers interpolate `t.inspect` taken while the thread
    // was running, so every inspect component (including the spawn
    // location) must round-trip.
    let name = globals
        .store
        .get_ivar(thread, IdentId::get_id("/__name"))
        .and_then(|v| v.is_str().map(|s| format!("@{s}")))
        .unwrap_or_default();
    let location = globals
        .store
        .get_ivar(thread, IdentId::get_id("/__spawn_location"))
        .and_then(|v| v.is_str().map(|s| format!(" {s}")))
        .unwrap_or_default();
    // `object_id << 1`, exactly as the Ruby-side `Thread#to_s` prints
    // the address.
    let mut text = format!(
        "#<Thread:0x{:016x}{}{} run> terminated with exception (report_on_exception is true):\n",
        thread.id() << 1,
        name,
        location,
    )
    .into_bytes();
    text.extend_from_slice(&exc);
    text.push(b'\n');
    SCHEDULER.with(|s| s.borrow_mut().pending_reports.push(text));
}

/// Write queued report_on_exception texts through the Ruby-level
/// `$stderr` — CRuby uses `rb_write_error_str`, which honours a replaced
/// `$stderr`; mspec's `output` matcher relies on exactly that. Called
/// on entry to and exit from `sleep`, `pass` and `join`: the write
/// invokes Ruby and may legitimately park. Errors are swallowed — a report must never
/// alter the program's outcome.
///
/// The scheduler joiners dead threads left unanswered (`pending_unblocks`)
/// are delivered here too, each as the installed scheduler's
/// `unblock(joining thread, fiber)`; without a scheduler there is no one
/// to tell, and they are dropped.
pub(crate) fn flush_pending_reports(vm: &mut Executor, globals: &mut Globals) {
    flush_pending_unblocks(vm, globals);
    loop {
        let Some(text) = SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            if s.flushing_reports || s.pending_reports.is_empty() {
                None
            } else {
                s.flushing_reports = true;
                Some(s.pending_reports.remove(0))
            }
        }) else {
            return;
        };
        let _ = crate::builtins::write_error_str(vm, globals, Value::string_from_vec(text));
        SCHEDULER.with(|s| s.borrow_mut().flushing_reports = false);
    }
}

fn flush_pending_unblocks(vm: &mut Executor, globals: &mut Globals) {
    loop {
        let Some((joiner, fiber)) = SCHEDULER.with(|s| s.borrow_mut().pending_unblocks.pop()) else {
            return;
        };
        let Some(scheduler) = globals.fiber_scheduler else {
            SCHEDULER.with(|s| s.borrow_mut().pending_unblocks.clear());
            return;
        };
        let _ = vm.invoke_method_inner(
            globals,
            IdentId::get_id("unblock"),
            scheduler,
            &[joiner, fiber],
            None,
            None,
        );
    }
}

/// The exit hook of a thread body: runs in the thread's own context right
/// after the body returned — normally or by unwinding — and before the
/// thread is marked terminated, from `native::run_body`. It is CRuby's end
/// of `thread_start_func_2`: the mutexes the thread still holds are
/// released (`Thread#__unlock_on_exit`, which wakes their waiters, a
/// fiber scheduler's through `unblock`), then the fibers that joined this
/// thread through a scheduler are answered with `unblock(joining thread,
/// fiber)` — the joining thread as the blocker, as CRuby passes it.
/// Ruby runs here as it does in the body, so a hook may park.
///
/// `ret` is the body's result word (0 when the body raised), handed back
/// untouched. Nothing roots it yet — the thread records it only once
/// this returns — so it is held on the executor's temp stack while the
/// hooks run Ruby, which may collect. The error the body raised is
/// pending on the executor; it is held aside — rooted, since it may be
/// the only reference to its exception object — while the hooks run and
/// restored after (#1880). An error a hook raises is dropped.
fn thread_body_exit(vm: &mut Executor, globals: &mut Globals, ret: u64) -> u64 {
    vm.with_temp_scope(|vm| {
        if ret != 0 {
            vm.temp_push(Value::from_u64(ret));
        } else {
            render_exception_report(vm, globals);
        }
        vm.with_pending_error_held(|vm| run_thread_exit_hooks(vm, globals));
    });
    ret
}

/// The exception a thread body died of, readied for its
/// `report_on_exception` report: that prints what `#detailed_message`
/// answers (`rb_error_write`), which is Ruby and so has to run here, in
/// the thread's own context — the report itself is put together where no
/// Ruby may run ([`queue_exception_report`]). The pending error becomes
/// its exception object, as it does for the report of the main thread
/// (`Globals::run_with_prelude`). A kill unwind and the non-local exits
/// `finalize` rewords are left as they are.
fn render_exception_report(vm: &mut Executor, globals: &mut Globals) {
    let thread = current_thread(vm);
    let reportable = vm.exception().is_some_and(|err| {
        !matches!(
            err.kind(),
            MonorubyErrKind::SystemExit(_)
                | MonorubyErrKind::MethodReturn(..)
                | MonorubyErrKind::BlockBreak(..)
        )
    });
    if !reportable || thread.as_thread_inner().killed || !reports_on_exception(globals, thread) {
        return;
    }
    let obj = vm.take_ex_obj(globals);
    vm.temp_push(obj);
    MonorubyErr::render_report_messages(vm, globals, obj);
    vm.set_error(MonorubyErr::new_from_exception(obj.is_exception().unwrap()).with_original(obj));
}

/// `Thread#report_on_exception` of *thread*: its own flag, else the
/// `Thread` class-level default, else true.
fn reports_on_exception(globals: &Globals, thread: Value) -> bool {
    let ivar = IdentId::get_id("/__report_on_exception");
    // An ivar explicitly set to nil counts as unset (the Ruby-side
    // accessors treat nil as "fall back to the default").
    globals
        .store
        .get_ivar(thread, ivar)
        .filter(|v| !v.is_nil())
        .map(|v| v.as_bool())
        .or_else(|| {
            globals
                .store
                .get_ivar(globals.store.get_module(THREAD_CLASS).as_val(), ivar)
                .filter(|v| !v.is_nil())
                .map(|v| v.as_bool())
        })
        .unwrap_or(true)
}

fn run_thread_exit_hooks(vm: &mut Executor, globals: &mut Globals) {
    let thread = current_thread(vm);
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("__unlock_on_exit"),
        thread,
        &[],
        None,
        None,
    );
    let joiners = {
        let mut thread = thread;
        std::mem::take(&mut thread.as_thread_inner_mut().fiber_joiners)
    };
    if let Some(scheduler) = globals.fiber_scheduler {
        for (joiner, fiber) in joiners {
            let _ = vm.invoke_method_inner(
                globals,
                IdentId::get_id("unblock"),
                scheduler,
                &[joiner, fiber],
                None,
                None,
            );
        }
    }
}

/// Queue a dead thread's terminating exception as a pending interrupt on
/// the main thread (waking it if parked) when CRuby semantics call for
/// it: always for `SystemExit`, and for any exception when the thread or
/// the `Thread` global default has `abort_on_exception` set.
fn forward_exception_to_main(globals: &mut Globals, thread: Value) -> bool {
    let Some(err) = ({
        let inner = thread.as_thread_inner();
        inner.exception.clone()
    }) else {
        return false;
    };
    let forward = matches!(err.kind(), MonorubyErrKind::SystemExit(_)) || {
        let ivar = IdentId::get_id("/__abort_on_exception");
        let per_thread = globals
            .store
            .get_ivar(thread, ivar)
            .map(|v| v.as_bool())
            .unwrap_or(false);
        let global_default = globals
            .store
            .get_ivar(globals.store.get_module(THREAD_CLASS).as_val(), ivar)
            .map(|v| v.as_bool())
            .unwrap_or(false);
        per_thread || global_default
    };
    if !forward {
        return false;
    }
    let Some(mut main) = SCHEDULER.with(|s| s.borrow().main) else {
        return false;
    };
    if main == thread {
        return false;
    }
    main.as_thread_inner_mut()
        .pending
        .push_back(PendingInterrupt::Raise(err));
    // Wake a parked main so delivery happens promptly (mirrors
    // `interrupt`); delivery itself still honors handle_interrupt masks.
    native::wake_parked(main);
    true
}
