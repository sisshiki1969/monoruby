use crate::*;

#[derive(Debug)]
pub struct FiberInner {
    handle: std::ptr::NonNull<Executor>,
    block_data: BlockData,
    stack: Option<std::ptr::NonNull<u8>>,
}

const FIBER_STACK_SIZE: usize = 8192 * 8;

impl Drop for FiberInner {
    fn drop(&mut self) {
        use std::alloc::*;
        unsafe { Box::from_raw(self.handle_ptr()) };
        if let Some(stack) = self.stack {
            let layout = Layout::from_size_align(FIBER_STACK_SIZE, 4096).unwrap();
            unsafe {
                dealloc(stack.as_ptr(), layout);
            }
        }
    }
}

impl FiberInner {
    pub fn new(block_data: BlockData) -> Self {
        let vm = Executor::default();
        let handle = std::ptr::NonNull::new(Box::into_raw(Box::new(vm))).unwrap();
        Self {
            handle,
            block_data,
            stack: None,
        }
    }

    pub fn state(&self) -> FiberState {
        unsafe { self.handle.as_ref().fiber_state() }
    }

    pub fn handle_ptr(&self) -> *mut Executor {
        self.handle.as_ptr()
    }

    pub fn handle(&self) -> &Executor {
        unsafe { self.handle.as_ref() }
    }

    pub fn block_data(&self) -> *const BlockData {
        &self.block_data as _
    }

    pub fn func_id(&self) -> FuncId {
        self.block_data.func_id()
    }

    pub fn init(&mut self) {
        use std::alloc::*;
        let layout = Layout::from_size_align(FIBER_STACK_SIZE, 4096).unwrap();
        unsafe {
            let stack_bottom = alloc(layout);
            libc::mprotect(stack_bottom as _, 4096, libc::PROT_NONE);
            let stack_top = stack_bottom.add(FIBER_STACK_SIZE);
            self.stack = Some(std::ptr::NonNull::new(stack_bottom).unwrap());
            self.handle.as_mut().save_rsp(stack_top);
        }
    }

    pub fn take_error(&mut self) -> MonorubyErr {
        unsafe { self.handle.as_mut().take_error() }
    }
}
