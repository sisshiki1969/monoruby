use crate::*;

#[derive(Debug)]
pub struct FiberInner {
    handle: Box<Executor>,
    block_data: BlockData,
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
        self.block_data.mark(alloc);
    }
}

impl FiberInner {
    pub(crate) fn new(block_data: BlockData) -> Self {
        let vm = Executor::default();
        let handle = Box::new(vm);
        Self {
            handle,
            block_data,
            stack: None,
        }
    }

    pub fn state(&self) -> FiberState {
        self.handle.fiber_state()
    }

    pub fn func_id(&self) -> FuncId {
        self.block_data.func_id()
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
                self.resume_fiber(vm, val)
            }
        }
    }

    pub fn enum_yield_values(
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
            FiberState::Suspended => self.resume_fiber(vm, yielder)?,
            FiberState::Terminated => {
                return Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ))
            }
        };
        if self.state() == FiberState::Terminated {
            Ok((Array::from(Value::array1(v)), true))
        } else {
            Ok((Array::from(v), false))
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
        match (globals.codegen.fiber_invoker)(
            vm,
            globals,
            &self.block_data,
            &mut self.handle as _,
            arg.as_ptr(),
            len,
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(super) fn resume_fiber(&mut self, vm: &mut Executor, val: Value) -> Result<Value> {
        match resume_fiber(vm, &mut self.handle as _, val) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    fn take_error(&mut self) -> MonorubyErr {
        self.handle.take_error()
    }
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn resume_fiber(vm: *mut Executor, child: &mut Executor, val: Value) -> Option<Value> {
    unsafe {
        std::arch::asm!(
            "push r15",
            "push r14",
            "push r13",
            "push r12",
            "push rbx",
            "push rbp",
            "mov  [rdi + 16], rsp", // [vm.rsp_save] <- rsp
            "mov  rsp, [rsi + 16]", // rsp <- [child_vm.rsp_save]
            "mov  [rsi + 24], rdi", // [child_vm.parent_fiber] <- vm
            "pop  rbp",
            "pop  rbx",
            "pop  r12",
            "pop  r13",
            "pop  r14",
            "pop  r15",
            "mov  rax, rdx",
            "ret",
            options(noreturn)
        );
    }
}
