//! Green-thread scheduler.
//!
//! monoruby multiplexes Ruby `Thread`s cooperatively on the one OS
//! thread (M:1). Each green thread owns a dedicated machine stack and an
//! `Executor` (see value/rvalue/thread.rs); context switches reuse the
//! fiber machinery's `rsp_save` swap, with three dedicated stubs
//! (`thread_invoker` / `switch_to_scheduler` / `scheduler_resume` in
//! codegen/arch/*/invoker.rs) that never touch `parent_fiber`, so a
//! thread root keeps `parent_fiber == None` and `Fiber.yield` at a
//! thread root takes the pre-existing error paths unchanged.
//!
//! ## Topology
//!
//! The scheduler loop (`scheduler_run`) only ever runs in the *main*
//! thread's context, event-loop style:
//!
//! - When the **main thread** blocks (`sleep`, `join`, ...), it calls
//!   `scheduler_run` as an ordinary function on its own stack. The loop
//!   dispatches runnable green threads until main is runnable again,
//!   then returns.
//! - When a **green thread** blocks, it records how it must be woken,
//!   then switches to the scheduler context saved in the process-global
//!   `SCHED_RSP` slot — i.e. control returns *out of* the loop's pending
//!   dispatch call, and the loop picks the next runnable thread.
//! - A green thread body that terminates switches back the same way
//!   (the `thread_invoker` epilogue), and the loop finalizes it.
//!
//! Because the loop always saves its context into `SCHED_RSP` right
//! before transferring control (both stubs do this), the slot always
//! holds the correct scheduler context, and only one loop instance can
//! ever be live (green threads never call `scheduler_run`).
//!
//! ## Switching discipline
//!
//! - Context switches happen only at VM safepoints (inside builtins), so
//!   every suspended thread's frames are GC-complete.
//! - `SCHEDULER` is a `RefCell`: it is never borrowed across a context
//!   switch, and no Ruby allocation happens while it is borrowed (GC
//!   marking re-enters it).

use std::cell::RefCell;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

use crate::alloc::GC;
use crate::value::rvalue::{ThreadInner, ThreadState};
use crate::*;

thread_local! {
    pub(crate) static SCHEDULER: RefCell<Scheduler> = RefCell::new(Scheduler::new());
}

/// The scheduler loop's saved machine context. Written by the context
/// switch stubs; read back when a green thread parks or terminates.
/// A single slot suffices because only one scheduler loop can be live
/// (it only runs in the main thread's context).
static mut SCHED_RSP: u64 = 0;

/// Absolute address of `SCHED_RSP`, baked into the context switch stubs.
pub(crate) fn sched_rsp_addr() -> u64 {
    &raw mut SCHED_RSP as u64
}

pub(crate) struct Scheduler {
    /// All live (non-terminated) thread objects, including main — a GC
    /// root. Dead threads are pruned at finalization (user references
    /// keep them alive from the frames that hold them).
    threads: Vec<Value>,
    /// Runnable green threads, FIFO. (The running thread and the main
    /// thread are not queued here.)
    ready: VecDeque<Value>,
    /// Parked-with-deadline threads: `(deadline, thread)`. `None` means
    /// "sleep until woken" (`sleep` without duration / `Thread.stop`).
    /// Kept unsorted (n is small); stale entries — threads woken through
    /// another path first — are skipped on expiry by re-checking state.
    sleepers: Vec<(Option<Instant>, Value)>,
    /// The thread currently executing. `None` until the main thread
    /// object is lazily created.
    current: Option<Value>,
    main: Option<Value>,
    /// The main thread's `Executor`, refreshed at every `scheduler_run`
    /// entry. Only dereferenced (for GC marking) while `in_scheduler` —
    /// i.e. exactly while green threads can be the GC trigger and main's
    /// frames would otherwise be unreachable.
    main_exec: Option<std::ptr::NonNull<Executor>>,
    in_scheduler: bool,
}

impl Scheduler {
    fn new() -> Self {
        Self {
            threads: vec![],
            ready: VecDeque::new(),
            sleepers: vec![],
            current: None,
            main: None,
            main_exec: None,
            in_scheduler: false,
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
        for t in &self.ready {
            t.mark(alloc);
        }
        for (_, t) in &self.sleepers {
            t.mark(alloc);
        }
        if self.in_scheduler
            && let Some(main_exec) = self.main_exec
        {
            // SAFETY: refreshed at scheduler_run entry; the main
            // executor outlives the loop that set it.
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

/// The `Thread` object for the currently running thread, creating the
/// main thread object lazily on first use.
pub(crate) fn current_thread(vm: &mut Executor) -> Value {
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().current.unwrap())
}

/// The main thread's `Thread` object.
pub(crate) fn main_thread(vm: &mut Executor) -> Value {
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().main.unwrap())
}

/// All alive threads (`Thread.list`).
pub(crate) fn thread_list(vm: &mut Executor) -> Vec<Value> {
    ensure_main(vm);
    SCHEDULER.with(|s| s.borrow().threads.clone())
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

/// Register a freshly created thread object and queue it for execution.
pub(crate) fn spawn(vm: &mut Executor, thread: Value) {
    ensure_main(vm);
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.threads.push(thread);
        s.ready.push_back(thread);
    });
}

/// Wake a thread parked in `sleep` (`Thread#wakeup` / `#run`). Returns
/// false if the thread is dead.
pub(crate) fn wakeup(thread: Value) -> bool {
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        let mut t = thread;
        let inner = t.as_thread_inner_mut();
        match inner.state() {
            ThreadState::Dead => false,
            ThreadState::Sleeping => {
                inner.state = ThreadState::Runnable;
                if Some(thread) != s.main {
                    s.ready.push_back(thread);
                }
                true
            }
            // Runnable / Created / Joining: nothing to do (CRuby's
            // wakeup only interrupts sleep).
            _ => true,
        }
    })
}

/// `Kernel#sleep` / `Thread.stop` through the scheduler. `dur == None`
/// sleeps until `#wakeup`. Returns the elapsed time.
pub(crate) fn sleep(
    vm: &mut Executor,
    globals: &mut Globals,
    dur: Option<Duration>,
) -> Result<Duration> {
    ensure_main(vm);
    let start = Instant::now();
    let deadline = dur.map(|d| start + d);
    if is_current_main() {
        SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            let mut main = s.main.unwrap();
            main.as_thread_inner_mut().state = ThreadState::Sleeping;
            s.sleepers.push((deadline, main));
        });
        scheduler_run(vm, globals)?;
    } else {
        let cur = SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            let mut cur = s.current.unwrap();
            cur.as_thread_inner_mut().state = ThreadState::Sleeping;
            s.sleepers.push((deadline, cur));
            cur
        });
        park_switch(vm, cur)?;
    }
    Ok(start.elapsed())
}

/// `Thread.pass`: give every currently runnable thread a chance to run.
pub(crate) fn pass(vm: &mut Executor, globals: &mut Globals) -> Result<()> {
    ensure_main(vm);
    if is_current_main() {
        // Dispatch the threads that are ready *now* (not the ones they
        // enqueue), then come back to main. Like `scheduler_run`, the
        // main thread's frames must stay GC-reachable while a green
        // thread is the one triggering a collection, so publish the main
        // executor for the duration.
        SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            s.main_exec = Some(std::ptr::NonNull::from(&mut *vm));
            s.in_scheduler = true;
        });
        let n = SCHEDULER.with(|s| s.borrow().ready.len());
        let mut result = Ok(());
        for _ in 0..n {
            let t = SCHEDULER.with(|s| s.borrow_mut().ready.pop_front());
            match t {
                Some(t) => {
                    if let Err(err) = dispatch(globals, t) {
                        result = Err(err);
                        break;
                    }
                }
                None => break,
            }
        }
        SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            s.in_scheduler = false;
            let main = s.main.unwrap();
            s.current = Some(main);
        });
        result
    } else {
        let cur = SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            let cur = s.current.unwrap();
            // Stay runnable; go to the back of the queue.
            s.ready.push_back(cur);
            cur
        });
        park_switch(vm, cur)
    }
}

/// `Thread#join` (also backs `#value`): park until `target` terminates
/// or the timeout expires. Returns whether the target is dead.
pub(crate) fn join(
    vm: &mut Executor,
    globals: &mut Globals,
    target: Value,
    timeout: Option<Duration>,
) -> Result<bool> {
    ensure_main(vm);
    let cur = SCHEDULER.with(|s| s.borrow().current.unwrap());
    if target == cur {
        return Err(MonorubyErr::threaderr(
            &globals.store,
            "Target thread must not be current thread",
        ));
    }
    let deadline = timeout.map(|d| Instant::now() + d);
    loop {
        if target.as_thread_inner().is_dead() {
            return Ok(true);
        }
        if let Some(dl) = deadline
            && Instant::now() >= dl
        {
            return Ok(false);
        }
        if is_current_main() {
            SCHEDULER.with(|s| {
                let mut s = s.borrow_mut();
                let mut main = s.main.unwrap();
                main.as_thread_inner_mut().state = ThreadState::Joining;
                let mut target = target;
                target.as_thread_inner_mut().joiners.push(main);
                if deadline.is_some() {
                    s.sleepers.push((deadline, main));
                }
            });
            scheduler_run(vm, globals)?;
        } else {
            SCHEDULER.with(|s| {
                let mut s = s.borrow_mut();
                let mut cur = s.current.unwrap();
                cur.as_thread_inner_mut().state = ThreadState::Joining;
                let mut target = target;
                target.as_thread_inner_mut().joiners.push(cur);
                if deadline.is_some() {
                    s.sleepers.push((deadline, cur));
                }
            });
            park_switch(vm, cur)?;
        }
    }
}

/// Park the current green thread: record the context to resume, then
/// switch to the scheduler. Returns when the scheduler resumes us.
fn park_switch(vm: &mut Executor, mut cur: Value) -> Result<()> {
    cur.as_thread_inner_mut().resume_exec = Some(std::ptr::NonNull::from(&mut *vm));
    let switch = CODEGEN.with(|c| c.borrow().switch_to_scheduler);
    match switch(vm as *mut _, Value::nil()) {
        Some(_) => Ok(()),
        // Reserved for asynchronous interrupt injection (Thread#raise /
        // #kill): a resume that sets an error instead of a value.
        None => Err(vm.take_error()),
    }
}

/// The scheduler loop. Only ever called in the main thread's context,
/// while the main thread is parked; returns when main is runnable again.
fn scheduler_run(vm: &mut Executor, globals: &mut Globals) -> Result<()> {
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.main_exec = Some(std::ptr::NonNull::from(&mut *vm));
        s.in_scheduler = true;
    });
    let result = scheduler_loop(vm, globals);
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.in_scheduler = false;
        let main = s.main.unwrap();
        s.current = Some(main);
    });
    result
}

fn scheduler_loop(vm: &mut Executor, globals: &mut Globals) -> Result<()> {
    loop {
        wake_due_sleepers();
        let (main_ready, next) = SCHEDULER.with(|s| {
            let mut s = s.borrow_mut();
            let main_ready =
                s.main.unwrap().as_thread_inner().state() == ThreadState::Runnable;
            let next = if main_ready { None } else { s.ready.pop_front() };
            (main_ready, next)
        });
        if main_ready {
            return Ok(());
        }
        if let Some(t) = next {
            dispatch(globals, t)?;
            continue;
        }
        // Nothing runnable: sleep until the nearest deadline, or report
        // deadlock when no thread can ever become runnable again.
        match nearest_deadline() {
            Some(deadline) => interruptible_sleep_until(vm, globals, deadline)?,
            None => {
                return Err(MonorubyErr::fatal(
                    "No live threads left. Deadlock?".to_string(),
                ));
            }
        }
    }
}

fn wake_due_sleepers() {
    let now = Instant::now();
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        let mut due = vec![];
        s.sleepers.retain(|(deadline, t)| {
            let mut t = *t;
            let state = t.as_thread_inner().state();
            // Entries whose thread was already woken (or died) through
            // another path are stale — drop them.
            if !matches!(state, ThreadState::Sleeping | ThreadState::Joining) {
                return false;
            }
            match deadline {
                Some(dl) if *dl <= now => {
                    due.push(t);
                    false
                }
                _ => true,
            }
        });
        let main = s.main;
        for mut t in due {
            t.as_thread_inner_mut().state = ThreadState::Runnable;
            if Some(t) != main {
                s.ready.push_back(t);
            }
        }
    });
}

fn nearest_deadline() -> Option<Instant> {
    SCHEDULER.with(|s| {
        s.borrow()
            .sleepers
            .iter()
            .filter_map(|(dl, _)| *dl)
            .min()
    })
}

/// Sleep the whole process until `deadline`, staying signal-responsive:
/// an EINTR runs the VM poll point (converting pending signals / running
/// trap handlers) and the sleep resumes for the remainder.
fn interruptible_sleep_until(
    vm: &mut Executor,
    globals: &mut Globals,
    deadline: Instant,
) -> Result<()> {
    loop {
        if crate::codegen::signal_table::PENDING_SIGNALS.load(std::sync::atomic::Ordering::Relaxed)
            != 0
            && crate::executor::execute_gc(vm, globals).is_none()
        {
            return Err(vm.take_error());
        }
        let now = Instant::now();
        if now >= deadline {
            return Ok(());
        }
        let remaining = deadline - now;
        let ts = libc::timespec {
            tv_sec: remaining.as_secs() as libc::time_t,
            tv_nsec: remaining.subsec_nanos() as libc::c_long,
        };
        // SAFETY: plain POSIX nanosleep on a valid timespec.
        let r = unsafe { libc::nanosleep(&ts, std::ptr::null_mut()) };
        if r == 0 {
            return Ok(());
        }
        if std::io::Error::last_os_error().raw_os_error() != Some(libc::EINTR) {
            return Ok(());
        }
        if crate::executor::execute_gc(vm, globals).is_none() {
            return Err(vm.take_error());
        }
    }
}

/// Run one green thread until it parks or terminates.
fn dispatch(globals: &mut Globals, mut thread: Value) -> Result<()> {
    enum Entry {
        Invoke(ProcData, *const Value, usize, *mut Executor),
        Resume(*mut Executor),
    }
    let entry = SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.current = Some(thread);
        let inner = thread.as_thread_inner_mut();
        if inner.state() == ThreadState::Created {
            inner.state = ThreadState::Runnable;
            inner.initialize_stack();
            let proc_data = ProcData::from_proc(inner.proc().unwrap());
            let args = inner.args();
            let (ptr, len) = (args.as_ptr(), args.len());
            Entry::Invoke(proc_data, ptr, len, inner.handle() as *mut Executor)
        } else {
            inner.state = ThreadState::Runnable;
            Entry::Resume(inner.resume_exec.take().unwrap().as_ptr())
        }
    });
    // The switch itself happens with no scheduler borrow held.
    let ret = match entry {
        Entry::Invoke(proc_data, args, len, handle) => {
            let invoker = CODEGEN.with(|c| c.borrow().thread_invoker);
            // SAFETY: `handle`/`args` point into the THREAD RValue,
            // which is rooted by the scheduler registry for the whole
            // run; the RValue arena does not move objects.
            invoker(
                unsafe { &mut *handle },
                globals,
                &proc_data,
                Value::nil(),
                args,
                len,
            )
        }
        Entry::Resume(exec) => {
            let resume = CODEGEN.with(|c| c.borrow().scheduler_resume);
            resume(exec, Value::nil())
        }
    };
    // Control is back in the scheduler context.
    if thread.as_thread_inner().body_terminated() {
        finalize(globals, thread, ret);
    }
    SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        s.current = s.main;
    });
    Ok(())
}

/// A green thread's body finished: record the outcome, wake joiners,
/// prune the registry.
fn finalize(globals: &mut Globals, mut thread: Value, ret: Option<Value>) {
    let woken = SCHEDULER.with(|s| {
        let mut s = s.borrow_mut();
        let inner = thread.as_thread_inner_mut();
        inner.state = ThreadState::Dead;
        match ret {
            Some(v) => inner.result = Some(v),
            None => {
                let err = inner.take_error();
                // A bare `return` / `break` escaping a thread body has no
                // enclosing frame on the thread's own stack: LocalJumpError
                // (CRuby).
                let err = match err.kind() {
                    MonorubyErrKind::MethodReturn(..) => {
                        MonorubyErr::localjumperr("unexpected return")
                    }
                    MonorubyErrKind::BlockBreak(..) => {
                        MonorubyErr::localjumperr("break from proc-closure")
                    }
                    _ => err,
                };
                inner.exception = Some(err);
            }
        }
        let joiners = std::mem::take(&mut inner.joiners);
        let main = s.main;
        let mut main_woken = false;
        for mut j in joiners {
            let ji = j.as_thread_inner_mut();
            if ji.state() == ThreadState::Joining {
                ji.state = ThreadState::Runnable;
                if Some(j) == main {
                    main_woken = true;
                } else {
                    s.ready.push_back(j);
                }
            }
        }
        s.threads.retain(|t| *t != thread);
        main_woken
    });
    let _ = woken;
    // `Thread#report_on_exception` default: surface a terminating
    // exception on stderr (CRuby prints it when the thread dies).
    let msg = {
        let inner = thread.as_thread_inner();
        inner
            .exception
            .as_ref()
            .map(|err| format!("{} ({})", err.message(), err.class_name(&globals.store)))
    };
    if let Some(msg) = msg {
        eprintln!("warning: thread terminated with exception (report_on_exception is true):");
        eprintln!("{msg}");
    }
}
