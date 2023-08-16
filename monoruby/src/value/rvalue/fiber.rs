use crate::*;

#[monoruby_object]
pub struct Fiber(Value);

impl Fiber {
    pub(crate) fn new(proc: Proc) -> Self {
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
        //let _ = unsafe { Box::from_raw(self.handle.unwrap().as_ptr()) };
        //self.handle = None;
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
}

impl Fiber {
    pub fn state(&self) -> FiberState {
        self.handle.fiber_state()
    }

    pub fn func_id(&self) -> FuncId {
        self.proc.func_id()
    }

    pub fn resume(&mut self, vm: &mut Executor, globals: &mut Globals, lfp: LFP) -> Result<Value> {
        let len = lfp.arg_len();
        match self.state() {
            FiberState::Created => self.invoke_fiber(vm, globals, lfp.as_arg(), len),
            FiberState::Terminated => Err(MonorubyErr::fibererr(
                "attempt to resume a terminated fiber".to_string(),
            )),
            FiberState::Suspended => {
                let val = if len == 0 {
                    Value::nil()
                } else if len == 1 {
                    lfp.arg(0)
                } else {
                    Value::array_from_iter(lfp.iter())
                };
                self.resume_fiber(vm, globals, val)
            }
        }
    }

    pub fn enum_yield_values(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        self_val: Enumerator,
    ) -> Result<(Array, bool)> {
        let v = match self.state() {
            FiberState::Created => {
                let arg = Arg::from(&Value::nil());
                self.invoke_fiber_with_self(vm, globals, arg, 0, self_val.into())?
            }
            FiberState::Suspended => self.resume_fiber(vm, globals, Value::nil())?,
            FiberState::Terminated => {
                return Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ))
            }
        };
        if self.state() == FiberState::Terminated {
            Ok((Value::array1(v).into(), true))
        } else {
            Ok((v.into(), false))
        }
    }

    pub fn generator_yield_values(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        yielder: Value,
    ) -> Result<(Array, bool)> {
        let v = match self.state() {
            FiberState::Created => {
                let arg = Arg::from(&yielder);
                self.invoke_fiber(vm, globals, arg, 1)?
            }
            FiberState::Suspended => self.resume_fiber(vm, globals, yielder)?,
            FiberState::Terminated => {
                return Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ))
            }
        };
        if self.state() == FiberState::Terminated {
            Ok((Value::array1(v).into(), true))
        } else {
            Ok((v.into(), false))
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
        arg: Arg,
        len: usize,
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
            len,
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
        arg: Arg,
        len: usize,
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
            len,
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
