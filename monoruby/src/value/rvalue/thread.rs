use super::*;

/// An asynchronous interrupt queued for a thread (`Thread#kill` /
/// `Thread#raise`), delivered by the scheduler when the thread is next
/// resumed (or immediately, if it targets the current thread).
#[derive(Debug)]
pub(crate) enum PendingInterrupt {
    /// `Thread#kill`: unwind the thread's stack running ensure clauses,
    /// uncatchable by `rescue` — delivered as a `Throw` with a fresh
    /// object tag no `catch` can match — then die silently.
    Kill,
    /// `Thread#raise`: deliver the exception at the interrupted point.
    Raise(MonorubyErr),
}

/// `Thread.handle_interrupt` timing for one mask entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InterruptTiming {
    /// Deliver as soon as a delivery point is reached (the default).
    Immediate,
    /// Deliver only at blocking operations (parks) — `Thread.pass` and
    /// `handle_interrupt` boundaries do not count.
    OnBlocking,
    /// Defer while the mask is in effect; the `handle_interrupt` block
    /// exit is the delivery point.
    Never,
}

/// Green-thread control block.
///
/// monoruby threads are cooperative green threads multiplexed on the one
/// OS thread (see scheduler.rs). Each thread owns a dedicated machine
/// stack and an `Executor` — exactly like a `FiberInner` — but is
/// scheduled by the global scheduler instead of an explicit
/// resume/yield parent chain. `parent_fiber` of a thread root is always
/// `None`, so `Fiber.yield` at a thread's root takes the pre-existing
/// "can't yield from main fiber" error paths unchanged.
#[derive(Debug)]
pub struct ThreadInner {
    /// The thread's own VM context. `None` for the main thread, whose
    /// `Executor` is owned by the embedder (marked via the scheduler's
    /// live `main_exec` pointer while green threads run).
    handle: Option<Box<Executor>>,
    /// The body block. `None` for the main thread and for bare
    /// `Thread.allocate` shells (e.g. `Thread::Waiter`).
    proc: Option<Proc>,
    /// Arguments passed to `Thread.new`, delivered to the body block.
    args: Vec<Value>,
    /// Dedicated machine stack, lazily allocated at first activation.
    stack: Option<std::ptr::NonNull<u8>>,
    pub(crate) state: ThreadState,
    /// The executor context the scheduler must resume — the thread root,
    /// or a fiber nested inside the thread that parked on its behalf.
    /// Only valid while parked.
    pub(crate) resume_exec: Option<std::ptr::NonNull<Executor>>,
    /// Body result (threads terminated normally).
    pub(crate) result: Option<Value>,
    /// Terminating exception (re-raised by `#join` / `#value`).
    pub(crate) exception: Option<MonorubyErr>,
    /// Threads parked in `#join` on this thread.
    pub(crate) joiners: Vec<Value>,
    /// Queued asynchronous interrupts (`#kill` / `#raise`), delivered
    /// FIFO at delivery points (CRuby's `pending_interrupt_queue`). A
    /// queue, not a slot: `t.raise e; t.kill` must deliver the raise
    /// first — with a single slot the kill overwrites it and the
    /// exception (and its report_on_exception output) is lost.
    pub(crate) pending: std::collections::VecDeque<PendingInterrupt>,
    /// Set when a kill was delivered: the terminating `Throw` unwind is
    /// then a clean death, not an exception (see scheduler finalize).
    pub(crate) killed: bool,
    /// `Thread.handle_interrupt` mask stack (innermost last). Each frame
    /// is the `{Class => timing}` pairs of one handle_interrupt block.
    pub(crate) masks: Vec<Vec<(Value, InterruptTiming)>>,
    /// Whether the thread's current/last park was a *blocking* operation
    /// (sleep / join / fd wait / queue pop) as opposed to `Thread.pass`.
    /// Consulted when delivering a pending interrupt masked
    /// `:on_blocking`.
    pub(crate) park_blocking: bool,
    /// Park permit: set by `Thread#wakeup` / `#run` when the target is
    /// *running* (not parked); consumed by the next park, which then
    /// returns immediately. Closes the classic lost-wakeup window under
    /// preemption — "enqueue as waiter → (preempted; the waker runs and
    /// wakes a still-running thread) → park forever". The Ruby-level
    /// Mutex / ConditionVariable / Queue in builtins/startup.rb all park
    /// through patterns that need this.
    pub(crate) park_permit: bool,
    /// `$?` / `Process.last_status` for this thread. CRuby keeps the
    /// last child status per thread, so a fresh thread sees `nil` until
    /// it reaps a child of its own.
    pub(crate) last_status: Option<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ThreadState {
    /// Not yet activated (no stack, body not started).
    Created,
    /// Runnable: in the ready queue, or the currently running thread.
    Runnable,
    /// Parked in `Kernel#sleep` / `Thread.stop` (deadline in the
    /// scheduler's sleeper list; `None` deadline = until woken).
    Sleeping,
    /// Parked in `#join` / `#value` waiting on another thread.
    Joining,
    /// Parked waiting for fd readiness (scheduler `io_waiters`).
    IoWaiting,
    /// Terminated (body returned, raised, or was killed).
    Dead,
}

const THREAD_STACK_SIZE: usize = 1024 * 256;

impl Drop for ThreadInner {
    fn drop(&mut self) {
        use std::alloc::*;
        if let Some(stack) = self.stack {
            let layout = Layout::from_size_align(THREAD_STACK_SIZE, 4096).unwrap();
            unsafe {
                libc::mprotect(stack.as_ptr() as _, 4096, libc::PROT_WRITE);
                dealloc(stack.as_ptr(), layout);
            }
            self.stack = None;
        }
    }
}

impl alloc::GC<RValue> for ThreadInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(handle) = &self.handle {
            handle.mark(alloc);
        }
        if let Some(proc) = &self.proc {
            proc.mark(alloc);
        }
        for v in &self.args {
            v.mark(alloc);
        }
        if let Some(v) = self.result {
            v.mark(alloc);
        }
        if let Some(err) = &self.exception {
            err.mark(alloc);
        }
        for v in &self.joiners {
            v.mark(alloc);
        }
        for int in &self.pending {
            if let PendingInterrupt::Raise(err) = int {
                err.mark(alloc);
            }
        }
        for frame in &self.masks {
            for (class, _) in frame {
                class.mark(alloc);
            }
        }
        if let Some(v) = self.last_status {
            v.mark(alloc);
        }
    }
}

impl ThreadInner {
    /// A schedulable green thread with a body.
    pub(crate) fn new(proc: Proc, args: Vec<Value>) -> Self {
        Self {
            handle: Some(Box::new(Executor::default())),
            proc: Some(proc),
            args,
            stack: None,
            state: ThreadState::Created,
            resume_exec: None,
            result: None,
            exception: None,
            joiners: vec![],
            pending: Default::default(),
            killed: false,
            masks: vec![],
            park_blocking: false,
            park_permit: false,
            last_status: None,
        }
    }

    /// The control block representing the main thread. Its `Executor` is
    /// the embedder's; the scheduler marks it through `main_exec`.
    pub(crate) fn main() -> Self {
        Self {
            handle: None,
            proc: None,
            args: vec![],
            stack: None,
            state: ThreadState::Runnable,
            resume_exec: None,
            result: None,
            exception: None,
            joiners: vec![],
            pending: Default::default(),
            killed: false,
            masks: vec![],
            park_blocking: false,
            park_permit: false,
            last_status: None,
        }
    }

    /// An inert shell for `Thread.allocate` (never scheduled). Used by
    /// Ruby-level subclasses with their own life cycle, e.g.
    /// `Thread::Waiter` returned by `Process.detach`.
    pub(crate) fn shell() -> Self {
        Self {
            handle: None,
            proc: None,
            args: vec![],
            stack: None,
            state: ThreadState::Dead,
            resume_exec: None,
            result: None,
            exception: None,
            joiners: vec![],
            pending: Default::default(),
            killed: false,
            masks: vec![],
            park_blocking: false,
            park_permit: false,
            last_status: None,
        }
    }

    //pub(crate) fn is_main(&self) -> bool {
    //    self.handle.is_none() && self.proc.is_none() && self.state != ThreadState::Dead
    //}

    pub(crate) fn state(&self) -> ThreadState {
        self.state
    }

    pub(crate) fn is_dead(&self) -> bool {
        self.state == ThreadState::Dead
    }

    pub(crate) fn proc(&self) -> Option<&Proc> {
        self.proc.as_ref()
    }

    pub(crate) fn args(&self) -> &[Value] {
        &self.args
    }

    pub(crate) fn handle(&mut self) -> &mut Executor {
        self.handle.as_mut().unwrap()
    }

    /// Whether the body has terminated (mirrors `FiberState::Terminated`).
    pub(crate) fn body_terminated(&self) -> bool {
        matches!(
            self.handle.as_deref().map(|h| h.fiber_state()),
            Some(FiberState::Terminated)
        )
    }

    /// Allocate the dedicated stack and point the executor at its top.
    /// Must be called exactly once, at first activation.
    pub(crate) fn initialize_stack(&mut self) {
        use std::alloc::*;
        assert!(self.stack.is_none());
        let layout = Layout::from_size_align(THREAD_STACK_SIZE, 4096).unwrap();
        unsafe {
            let stack_bottom = alloc(layout);
            libc::mprotect(stack_bottom as _, 4096, libc::PROT_NONE);
            let stack_top = stack_bottom.add(THREAD_STACK_SIZE);
            self.stack = Some(std::ptr::NonNull::new(stack_bottom).unwrap());
            let handle = self.handle.as_mut().unwrap();
            handle.save_rsp(stack_top);
            handle.set_stack_limit(stack_top);
        }
    }

    pub(crate) fn take_error(&mut self) -> MonorubyErr {
        self.handle.as_mut().unwrap().take_error()
    }
}
