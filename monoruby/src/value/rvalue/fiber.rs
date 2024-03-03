use super::*;

#[monoruby_object]
pub struct Fiber(Value);

impl Fiber {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::FIBER));
        Self(val)
    }

    pub(crate) fn from(proc: Proc) -> Self {
        Fiber(Value::new_fiber(proc))
    }
}

#[derive(Debug)]
pub struct FiberInner {
    handle: Box<Executor>,
    proc: Proc,
    stack: Option<std::ptr::NonNull<u8>>,
}

const FIBER_STACK_SIZE: usize = 8192 * 8;

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
        self.handle.mark(alloc);
        self.proc.mark(alloc);
    }
}

impl FiberInner {
    pub(crate) fn new(proc: Proc) -> Self {
        let vm = Executor::default();
        let handle = Box::new(vm);
        Self {
            handle,
            proc,
            stack: None,
        }
    }

    pub fn state(&self) -> FiberState {
        self.handle.fiber_state()
    }

    pub fn func_id(&self) -> FuncId {
        self.proc.func_id()
    }

    pub fn is_terminated(&self) -> bool {
        self.handle.fiber_state() == FiberState::Terminated
    }
}

impl Fiber {
    pub fn resume(&mut self, vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
        let arg0 = Array::new(lfp.arg(0));
        match self.state() {
            FiberState::Created => self.invoke_fiber(vm, globals, &[lfp.arg(0)]),
            FiberState::Suspended => self.resume_fiber(vm, globals, arg0.peel()),
            FiberState::Terminated => Err(MonorubyErr::fibererr(
                "attempt to resume a terminated fiber".to_string(),
            )),
        }
    }

    pub fn enum_yield_values(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        self_val: Enumerator,
    ) -> Result<Value> {
        let v = match self.state() {
            FiberState::Created => {
                self.invoke_fiber_with_self(vm, globals, &[], self_val.into())?
            }
            FiberState::Suspended => self.resume_fiber(vm, globals, Value::nil())?,
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
            FiberState::Created => self.invoke_fiber(vm, globals, &[yielder]),
            FiberState::Suspended => self.resume_fiber(vm, globals, yielder),
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
            self.handle.save_rsp(stack_top);
        }
    }

    ///
    /// Initialize and invoke the fiber.
    ///
    /// - the fiber must be FiberState::Created.
    ///
    pub(super) fn invoke_fiber(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        arg: &[Value],
    ) -> Result<Value> {
        assert_eq!(FiberState::Created, self.state());
        self.initialize();
        let proc = self.proc;
        let handle = &mut self.handle;
        match (globals.codegen.fiber_invoker)(
            vm,
            globals,
            &proc,
            Value::nil(),
            arg.as_ptr(),
            arg.len(),
            handle,
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    ///
    /// Initialize and invoke the fiber with *self*.
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
        assert_eq!(FiberState::Created, self.state());
        self.initialize();
        let proc = self.proc;
        let handle = &mut self.handle;
        match (globals.codegen.fiber_invoker_with_self)(
            vm,
            globals,
            &proc,
            self_val,
            arg.as_ptr(),
            arg.len(),
            handle,
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(super) fn resume_fiber(
        &mut self,
        vm: &mut Executor,
        globals: &Globals,
        val: Value,
    ) -> Result<Value> {
        match (globals.codegen.resume_fiber)(vm, &mut self.handle as _, val) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    fn take_error(&mut self) -> MonorubyErr {
        self.handle.take_error()
    }
}
