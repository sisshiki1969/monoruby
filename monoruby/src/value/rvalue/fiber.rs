use super::*;

#[monoruby_object]
pub struct Fiber(Value);

impl Fiber {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::FIBER));
        Self(val)
    }

    pub(crate) fn from(proc: Proc) -> Self {
        Fiber(Value::new_fiber(FiberInner::new(proc, 0, None)))
    }
}

/// The executor a Fiber object runs on: fibers created by `Fiber.new` (and
/// the enumerator internals) own theirs; the per-thread *root* Fiber object
/// (`Fiber.current` at a thread's root context) aliases the already-running
/// root executor, which is owned by the embedder (main) or the green-thread
/// control block.
#[derive(Debug)]
pub enum FiberHandle {
    Owned(Box<Executor>),
    Root(std::ptr::NonNull<Executor>),
}

#[derive(Debug)]
pub struct FiberInner {
    handle: FiberHandle,
    proc: Option<Proc>,
    stack: Option<std::ptr::NonNull<u8>>,
    /// Object id of the `Thread` this fiber belongs to (0 = unchecked:
    /// enumerator-internal fibers). Explicit Fiber API calls from another
    /// thread raise FiberError.
    owner_thread: u64,
}

const FIBER_STACK_SIZE: usize = 1024 * 256;

impl Drop for FiberInner {
    fn drop(&mut self) {
        use std::alloc::*;
        if let Some(stack) = self.stack {
            let layout = Layout::from_size_align(FIBER_STACK_SIZE, 4096).unwrap();
            unsafe {
                libc::mprotect(stack.as_ptr() as _, 4096, libc::PROT_WRITE);
                dealloc(stack.as_ptr(), layout);
            }
            self.stack = None;
        }
    }
}

impl alloc::GC<RValue> for FiberInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        // A root handle merely aliases an executor that is marked through
        // its owner (the embedder's main executor / the green thread's
        // control block) — and may already be gone for a dead thread.
        if let FiberHandle::Owned(handle) = &self.handle {
            handle.mark(alloc);
        }
        if let Some(proc) = &self.proc {
            proc.mark(alloc);
        }
    }
}

impl FiberInner {
    pub(crate) fn new(
        proc: Proc,
        owner_thread: u64,
        thread_root: Option<std::ptr::NonNull<Executor>>,
    ) -> Self {
        let mut vm = Executor::default();
        if let Some(root) = thread_root {
            vm.set_thread_root(root);
        }
        let handle = Box::new(vm);
        Self {
            handle: FiberHandle::Owned(handle),
            proc: Some(proc),
            stack: None,
            owner_thread,
        }
    }

    /// A root Fiber object aliasing the given (running) root executor.
    pub(crate) fn root(executor: std::ptr::NonNull<Executor>, owner_thread: u64) -> Self {
        Self {
            handle: FiberHandle::Root(executor),
            proc: None,
            stack: None,
            owner_thread,
        }
    }

    pub(crate) fn is_root(&self) -> bool {
        matches!(self.handle, FiberHandle::Root(_))
    }

    /// Re-anchor a root Fiber object to its executor's *current* address.
    /// `Executor::init` returns the embedder's executor by value, so a root
    /// Fiber materialized during startup (e.g. by a Mutex in the rubygems
    /// shim calling `Fiber.current`) captured a pre-move address; every
    /// `Fiber.current` lookup refreshes it from the live `vm`.
    pub(crate) fn reanchor_root(&mut self, executor: std::ptr::NonNull<Executor>) {
        debug_assert!(self.is_root());
        self.handle = FiberHandle::Root(executor);
    }

    pub(crate) fn owner_thread(&self) -> u64 {
        self.owner_thread
    }

    pub(crate) fn executor(&self) -> &Executor {
        match &self.handle {
            FiberHandle::Owned(handle) => handle,
            // SAFETY: root executors live for their thread's lifetime;
            // cross-thread API calls are rejected before dereferencing.
            FiberHandle::Root(ptr) => unsafe { ptr.as_ref() },
        }
    }

    pub(crate) fn executor_mut(&mut self) -> &mut Executor {
        match &mut self.handle {
            FiberHandle::Owned(handle) => handle,
            // SAFETY: see `executor`.
            FiberHandle::Root(ptr) => unsafe { ptr.as_mut() },
        }
    }

    pub(crate) fn executor_ptr(&mut self) -> *mut Executor {
        self.executor_mut() as *mut _
    }

    pub fn state(&self) -> FiberState {
        self.executor().fiber_state()
    }

    pub fn func_id(&self) -> Option<FuncId> {
        self.proc.as_ref().map(|p| p.func_id())
    }

    pub fn is_terminated(&self) -> bool {
        self.state() == FiberState::Terminated
    }
}

impl Fiber {
    ///
    /// `Fiber#resume` (CRuby `fiber_resume_kw`): validity checks, link
    /// bookkeeping, then a resume-style switch. The switch-back side
    /// (yield or termination) is handled in [`Self::receive_switch_back`].
    ///
    pub fn resume(&mut self, vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
        let arg0 = lfp.arg(0).as_array();
        self.check_resumable(vm)?;
        match self.state() {
            // Pass the arguments individually so block-parameter binding
            // applies (`|a|` receives `:first`, not `[:first]`).
            FiberState::Created => self.invoke_fiber(vm, globals, &arg0),
            FiberState::Suspended => self.resume_fiber(vm, arg0.peel()),
            FiberState::Terminated => unreachable!(),
        }
    }

    /// The CRuby `fiber_resume_kw` precondition ladder, in message order.
    fn check_resumable(&self, vm: &mut Executor) -> Result<()> {
        let target = self.executor() as *const Executor;
        if self.state() == FiberState::Terminated {
            return Err(MonorubyErr::fibererr(
                "attempt to resume a terminated fiber".to_string(),
            ));
        }
        if target == (vm as *const Executor) {
            return Err(MonorubyErr::fibererr(
                "attempt to resume the current fiber".to_string(),
            ));
        }
        if self.executor().parent_fiber().is_some() {
            return Err(MonorubyErr::fibererr(
                "attempt to resume a resumed fiber (double resume)".to_string(),
            ));
        }
        if self.executor().resuming_fiber().is_some() {
            return Err(MonorubyErr::fibererr(
                "attempt to resume a resuming fiber".to_string(),
            ));
        }
        if self.executor().is_transferred() {
            return Err(MonorubyErr::fibererr(
                "cannot resume transferred Fiber".to_string(),
            ));
        }
        Ok(())
    }

    ///
    /// `Fiber#transfer` (CRuby `rb_fiber_transfer_kw`): the target must not
    /// be on a resume chain nor suspended in `Fiber.yield`; a self-transfer
    /// is a no-op that passes its arguments through. Where the target's
    /// body *terminates* is computed here (CRuby's `return_fiber`): its
    /// `prev` if it was resumed, otherwise the deepest fiber on the current
    /// thread root's resuming chain.
    ///
    pub fn transfer(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        args: &[Value],
    ) -> Result<Value> {
        {
            let target = self.executor();
            if target.resuming_fiber().is_some() {
                return Err(MonorubyErr::fibererr(
                    "attempt to transfer to a resuming fiber".to_string(),
                ));
            }
            if target.is_yielding() {
                return Err(MonorubyErr::fibererr(
                    "attempt to transfer to a yielding fiber".to_string(),
                ));
            }
        }
        if self.state() == FiberState::Terminated {
            return Err(MonorubyErr::fibererr("dead fiber called".to_string()));
        }
        self.executor_mut().set_transferred();
        if self.executor_ptr() == (vm as *mut Executor) {
            // Transferring to the currently running fiber: continue in
            // place, the call evaluating to its own arguments.
            return Ok(Self::passing_arg(args));
        }
        // CRuby return_fiber(terminate=true), evaluated eagerly: nothing
        // that runs while the target is on top can change this answer (its
        // own nested switches restore the links before it continues).
        let return_to = match self.executor().parent_fiber() {
            Some(prev) => prev,
            None => {
                let mut cur = vm.thread_root_or_self();
                // SAFETY: resuming-chain members are suspended executors,
                // valid while their fibers are alive.
                while let Some(next) = unsafe { cur.as_ref() }.resuming_fiber() {
                    cur = next;
                }
                cur
            }
        };
        let val = Self::passing_arg(args);
        match self.state() {
            FiberState::Created => {
                self.prepare_activation(vm, return_to, false);
                let res = self.invoke_fiber_inner(vm, globals, args, None, false);
                self.receive_switch_back(vm, res)
            }
            FiberState::Suspended => {
                {
                    let child = self.executor_mut();
                    child.set_return_to(return_to);
                    child.set_yielding(false);
                }
                let invoker = CODEGEN.with(|codegen| codegen.borrow().transfer_fiber);
                let res = invoker(vm as _, self.executor_mut(), val);
                self.receive_switch_back(vm, res)
            }
            FiberState::Terminated => unreachable!(),
        }
    }

    /// The value a switch call evaluates to on the target side: nil for no
    /// args, the argument itself for one, an Array for several.
    fn passing_arg(args: &[Value]) -> Value {
        match args.len() {
            0 => Value::nil(),
            1 => args[0],
            _ => Value::array_from_iter(args.iter().cloned()),
        }
    }

    ///
    /// Inject `err` into this *suspended* fiber, resuming it so its pending
    /// suspension point (a `Fiber.yield`, or the switch primitive of a
    /// `transfer`/`resume` it is parked in) raises the error there —
    /// `Fiber#raise` / `Fiber#kill` delivery. The injected error is placed
    /// in the fiber's own executor and the switch value is the null
    /// sentinel, which every receive path maps to "take the error from my
    /// own executor".
    ///
    pub(crate) fn inject_error(
        &mut self,
        vm: &mut Executor,
        err: MonorubyErr,
        resume_style: bool,
    ) -> Result<Value> {
        // The switch primitives pass the resume value through verbatim as
        // their own return value; `Value` (NonZeroU64) cannot spell the null
        // sentinel, so reinterpret the invoker as taking a raw u64 —
        // identical ABI (`Value` is a transparent u64).
        type RawSwitch = extern "C" fn(*mut Executor, &mut Executor, u64) -> Option<Value>;
        if resume_style {
            // Resume-style (yielding fiber): establish the prev link like a
            // normal resume so an unwind returns to us.
            vm.set_resuming_fiber(std::ptr::NonNull::new(self.executor_ptr()));
            {
                let vm_ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
                let child = self.executor_mut();
                child.set_return_to(vm_ptr);
                child.set_yielding(false);
                child.set_error(err);
            }
            let invoker = CODEGEN.with(|codegen| codegen.borrow().resume_fiber);
            // SAFETY: same ABI, see `RawSwitch`.
            let invoker: RawSwitch = unsafe { std::mem::transmute(invoker) };
            let res = invoker(vm as _, self.executor_mut(), 0);
            self.receive_switch_back(vm, res)
        } else {
            // Transfer-style (fiber suspended in a transfer): same return
            // target computation as a transfer.
            let return_to = match self.executor().parent_fiber() {
                Some(prev) => prev,
                None => {
                    let mut cur = vm.thread_root_or_self();
                    // SAFETY: see `transfer`.
                    while let Some(next) = unsafe { cur.as_ref() }.resuming_fiber() {
                        cur = next;
                    }
                    cur
                }
            };
            {
                let child = self.executor_mut();
                child.set_return_to(return_to);
                child.set_error(err);
            }
            let invoker = CODEGEN.with(|codegen| codegen.borrow().transfer_fiber);
            // SAFETY: same ABI, see `RawSwitch`.
            let invoker: RawSwitch = unsafe { std::mem::transmute(invoker) };
            let res = invoker(vm as _, self.executor_mut(), 0);
            self.receive_switch_back(vm, res)
        }
    }

    pub fn enum_yield_values(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        self_val: Enumerator,
        val: Value,
    ) -> Result<Value> {
        let v = match self.state() {
            FiberState::Created => {
                self.invoke_fiber_with_self(vm, globals, &[], self_val.into())?
            }
            FiberState::Suspended => self.resume_fiber(vm, val)?,
            FiberState::Terminated => {
                return Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ))
            }
        };
        Ok(v)
    }

    pub fn generator_yield_values(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        yielder: Value,
    ) -> Result<Value> {
        match self.state() {
            // First activation: the yielder is the block parameter (`|y|`).
            FiberState::Created => self.invoke_fiber(vm, globals, &[yielder]),
            // Subsequent resumes: this value becomes the return value of the
            // `y.yield(...)` that suspended the generator. For `each`/`to_a`
            // nothing is fed back, so it must be `nil` (CRuby's `Yielder#yield`
            // returns nil) — NOT the yielder itself, which previously leaked a
            // `Yielder` into user code (e.g. `r << y.yield(1)`) and then
            // aborted the process when that value was inspected.
            FiberState::Suspended => self.resume_fiber(vm, Value::nil()),
            FiberState::Terminated => Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            )),
        }
    }

    fn initialize(&mut self) {
        use std::alloc::*;
        let layout = Layout::from_size_align(FIBER_STACK_SIZE, 4096).unwrap();
        unsafe {
            let stack_bottom = alloc(layout);
            libc::mprotect(stack_bottom as _, 4096, libc::PROT_NONE);
            let stack_top = stack_bottom.add(FIBER_STACK_SIZE);
            self.stack = Some(std::ptr::NonNull::new(stack_bottom).unwrap());
            let handle = self.executor_mut();
            handle.save_rsp(stack_top);
            handle.set_stack_limit(stack_top);
        }
    }

    /// Pre-switch bookkeeping shared by first activations: allocate the
    /// stack, record the Fiber identity, set the termination target and
    /// (for resume-style activations) the resume links.
    fn prepare_activation(
        &mut self,
        vm: &mut Executor,
        return_to: std::ptr::NonNull<Executor>,
        resume_style: bool,
    ) {
        assert_eq!(FiberState::Created, self.state());
        let fiber_val = self.0;
        self.initialize();
        if resume_style {
            vm.set_resuming_fiber(std::ptr::NonNull::new(self.executor_ptr()));
        }
        let handle = self.executor_mut();
        handle.set_current_fiber(fiber_val);
        handle.set_return_to(return_to);
    }

    ///
    /// Initialize and invoke the fiber (resume-style: the caller becomes
    /// both `prev` and the termination target).
    ///
    /// - the fiber must be FiberState::Created.
    ///
    pub(super) fn invoke_fiber(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        arg: &[Value],
    ) -> Result<Value> {
        let vm_ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
        self.prepare_activation(vm, vm_ptr, true);
        let res = self.invoke_fiber_inner(vm, globals, arg, None, true);
        self.receive_switch_back(vm, res)
    }

    ///
    /// Initialize and invoke the fiber with *self* (enumerator internals).
    ///
    /// - the fiber must be FiberState::Created.
    ///
    pub(super) fn invoke_fiber_with_self(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        arg: &[Value],
        self_val: Value,
    ) -> Result<Value> {
        let vm_ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
        self.prepare_activation(vm, vm_ptr, true);
        let res = self.invoke_fiber_inner(vm, globals, arg, Some(self_val), true);
        self.receive_switch_back(vm, res)
    }

    fn invoke_fiber_inner(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        arg: &[Value],
        self_val: Option<Value>,
        resume_style: bool,
    ) -> Option<Value> {
        let proc = ProcData::from_proc(self.proc.as_ref().unwrap());
        let invoker = match (self_val.is_some(), resume_style) {
            (true, _) => globals.invokers.fiber_with_self,
            (false, true) => globals.invokers.fiber,
            (false, false) => globals.invokers.fiber_transfer,
        };
        let handle = self.executor_mut();
        invoker(
            vm,
            globals,
            &proc,
            self_val.unwrap_or_default(),
            arg.as_ptr(),
            arg.len(),
            handle,
        )
    }

    pub(super) fn resume_fiber(&mut self, vm: &mut Executor, val: Value) -> Result<Value> {
        self.check_not_across_resume(vm)?;
        vm.set_resuming_fiber(std::ptr::NonNull::new(self.executor_ptr()));
        {
            let vm_ptr = std::ptr::NonNull::new(vm as *mut Executor).unwrap();
            let child = self.executor_mut();
            child.set_return_to(vm_ptr);
            child.set_yielding(false);
        }
        let invoker = CODEGEN.with(|codegen| codegen.borrow().resume_fiber);
        let res = invoker(vm as _, self.executor_mut(), val);
        self.receive_switch_back(vm, res)
    }

    /// Guard the enumerator-internal resume path (which skips
    /// `check_resumable`) against switching into a live executor: resuming
    /// the current context or an executor already on the active chain would
    /// switch the native stack into a live frame (SIGSEGV).
    fn check_not_across_resume(&mut self, vm: &mut Executor) -> Result<()> {
        let target = self.executor_ptr() as *const Executor;
        if target == (vm as *const Executor) {
            return Err(MonorubyErr::fibererr(
                "attempt to resume the current fiber".to_string(),
            ));
        }
        let mut cur = vm.parent_fiber();
        while let Some(p) = cur {
            if (p.as_ptr() as *const Executor) == target {
                return Err(MonorubyErr::fibererr(
                    "attempt to resume a resumed fiber (double resume)".to_string(),
                ));
            }
            cur = unsafe { p.as_ref().parent_fiber() };
        }
        Ok(())
    }

    ///
    /// Post-switch bookkeeping: runs on the *receiving* side of every
    /// switch-back into `vm` (yield-back, termination, or a value passed
    /// back through a transfer).
    ///
    /// - Clears the resume links CRuby's `return_fiber` clears (the child's
    ///   `prev` and our `resuming_fiber`) and marks a yield-suspension.
    /// - Maps the null switch value to "an error was relayed into MY
    ///   executor" (termination relay / raise injection).
    /// - Swallows the kill unwind of a killed child (`Fiber#kill`).
    /// - Delivers a `Fiber#kill` that targeted *us* while we were an active
    ///   ancestor on the resume chain.
    ///
    fn receive_switch_back(&mut self, vm: &mut Executor, res: Option<Value>) -> Result<Value> {
        vm.set_resuming_fiber(None);
        {
            let child = self.executor_mut();
            if child.fiber_state() == FiberState::Suspended {
                // The only way a *resume-style* call returns without the
                // child terminating is a `Fiber.yield`; for a transfer,
                // control legitimately comes back through third parties, in
                // which case the child's links were already maintained at
                // its own switch points and these writes are no-ops.
                if child.parent_fiber().map(|p| p.as_ptr())
                    == Some(vm as *mut Executor)
                {
                    child.set_parent_fiber(None);
                    child.set_yielding(true);
                }
            }
        }
        match res {
            Some(val) => Ok(val),
            None => {
                let killed = self.executor().is_killed();
                if vm.has_error() {
                    let err = vm.take_error();
                    if killed && matches!(err.kind(), MonorubyErrKind::FiberKill) {
                        Ok(Value::nil())
                    } else {
                        Err(err)
                    }
                } else if killed {
                    Ok(Value::nil())
                } else if self.executor().has_error() {
                    // Legacy activation-error path: the error stayed on the
                    // child executor.
                    Err(self.executor_mut().take_error())
                } else {
                    Err(MonorubyErr::fibererr("fiber switch failed".to_string()))
                }
            }
        }
    }
}
