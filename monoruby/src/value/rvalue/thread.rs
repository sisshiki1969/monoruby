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
    /// A queued asynchronous interrupt (`#kill` / `#raise`), delivered
    /// at the next resume.
    pub(crate) pending: Option<PendingInterrupt>,
    /// Set when a kill was delivered: the terminating `Throw` unwind is
    /// then a clean death, not an exception (see scheduler finalize).
    pub(crate) killed: bool,
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
        if let Some(PendingInterrupt::Raise(err)) = &self.pending {
            err.mark(alloc);
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
            pending: None,
            killed: false,
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
            pending: None,
            killed: false,
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
            pending: None,
            killed: false,
        }
    }

    pub(crate) fn is_main(&self) -> bool {
        self.handle.is_none() && self.proc.is_none() && self.state != ThreadState::Dead
    }

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
