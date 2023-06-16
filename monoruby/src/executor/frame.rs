use super::*;

///
/// Control frame pointer.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CFP(std::ptr::NonNull<Option<CFP>>);

impl CFP {
    ///
    /// Create CFP from raw pointer.
    ///
    unsafe fn new(ptr: *mut u8) -> Self {
        CFP(std::ptr::NonNull::new(ptr as *mut Option<CFP>).unwrap())
    }

    ///
    /// Get inner raw pointer.
    ///
    fn get(&self) -> *const Option<CFP> {
        self.0.as_ptr()
    }

    ///
    /// Get a previous control frame of *self*.
    ///
    pub fn prev(&self) -> Option<Self> {
        unsafe { *self.get() }
    }

    ///
    /// Get a return address of *self*.
    ///
    pub unsafe fn return_addr(&self) -> *const usize {
        *(self.get().add(2) as *const *const usize)
    }

    ///
    /// Get base pointer address of *self*.
    ///
    pub unsafe fn bp(&self) -> *const usize {
        self.get().add(BP_PREV_CFP as usize / 8) as _
    }

    ///
    /// Get LFP.
    ///
    pub(super) fn lfp(&self) -> LFP {
        unsafe {
            let bp = self.bp();
            LFP(*bp.sub(BP_LFP as usize / 8) as _)
        }
    }

    ///
    /// Get outermost LFP.
    ///
    pub(super) fn outermost_lfp(&self) -> LFP {
        match self.lfp().outer() {
            Some(dfp) => dfp.outermost().lfp(),
            None => self.lfp(),
        }
    }

    pub(super) fn block_given(&self) -> bool {
        self.outermost_lfp().block().is_some()
    }

    ///
    /// Set LFP.
    ///
    pub unsafe fn set_lfp(&mut self, lfp: LFP) {
        let bp = self.bp() as *mut usize;
        *bp.sub(BP_LFP as usize / 8) = lfp.0 as _;
    }

    ///
    /// Get *FuncId* of a current method / classdef.
    ///
    pub fn method_func_id(&self) -> FuncId {
        self.outermost_lfp().meta().func_id()
    }

    ///
    /// Get *BlockHandler* of a current method / classdef.
    ///
    pub fn get_block(&self) -> Option<BlockHandler> {
        let lfp = self.outermost_lfp();

        lfp.block().map(|bh| match bh.0.try_fixnum() {
            Some(mut i) => {
                let mut cfp = *self;
                loop {
                    if cfp.lfp() == lfp {
                        break;
                    }
                    i += 1;
                    cfp = cfp.prev().unwrap();
                }
                BlockHandler::new(Value::new_integer(i))
            }
            None => bh,
        })
    }

    ///
    /// Get *FuncId* of a current position in the source code.
    ///
    pub fn get_source_pos(&self) -> FuncId {
        let mut cfp = Some(*self);
        while let Some(inner_cfp) = cfp {
            if !inner_cfp.lfp().meta().is_native() {
                return inner_cfp.lfp().meta().func_id();
            }
            cfp = inner_cfp.prev();
        }
        unreachable!("get_source_pos: non-native method not found.")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct LFP(*const u8);

impl std::default::Default for LFP {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

impl std::cmp::PartialEq<CFP> for LFP {
    fn eq(&self, other: &CFP) -> bool {
        self.0 == other.get() as _
    }
}

impl std::cmp::PartialOrd<CFP> for LFP {
    fn partial_cmp(&self, other: &CFP) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&(other.get() as *const u8))
    }
}

impl alloc::GC<RValue> for LFP {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        unsafe {
            let meta = self.meta();
            for r in 0..meta.reg_num() as usize {
                let v = self.register(r);
                v.mark(alloc);
            }
            if let Some(v) = self.block() {
                v.0.mark(alloc)
            };
            if let Some(outer) = self.outer() {
                outer.lfp().mark(alloc);
            }
        }
    }
}

impl LFP {
    ///
    /// Move local frame on the stack to the heap.
    ///
    pub fn move_to_heap(&self) -> Self {
        let len = self.len_in_bytes();
        let v = self.registers().to_vec().into_boxed_slice();
        LFP((Box::into_raw(v) as *mut u64 as usize + len - 8) as _)
    }

    ///
    /// Get CFP.
    ///
    pub unsafe fn cfp(&self) -> CFP {
        CFP::new(self.0.sub(BP_PREV_CFP as usize) as _)
    }

    ///
    /// Get the address of outer.
    ///
    pub fn outer_address(&self) -> DFP {
        DFP::new(unsafe { self.0.sub(LBP_OUTER as usize) } as _)
    }

    ///
    /// Get outer DFP.
    ///
    pub fn outer(&self) -> Option<DFP> {
        self.outer_address().outer()
    }

    ///
    /// Set outer.
    ///
    pub unsafe fn set_outer(&mut self, outer: Option<DFP>) {
        *(self.outer_address().0.as_ptr()) = outer;
    }

    ///
    /// Get Meta.
    ///
    pub(in crate::executor) fn meta(&self) -> Meta {
        Meta::from(unsafe { *(self.0.sub(LBP_META as usize) as *const u64) })
    }

    ///
    /// Get *self*.
    ///
    pub fn self_val(&self) -> Value {
        unsafe { *(self.0.sub(LBP_SELF as usize) as *const _) }
    }

    ///
    /// Get block.
    ///
    pub fn block(&self) -> Option<BlockHandler> {
        unsafe { *(self.0.sub(LBP_BLOCK as usize) as *const _) }
    }

    pub fn expect_block(&self) -> Result<BlockHandler> {
        if let Some(block) = self.block() {
            Ok(block)
        } else {
            Err(MonorubyErr::no_block_given())
        }
    }

    ///
    /// Get a value of register slot *index*.
    ///
    pub unsafe fn register(&self, index: usize) -> Value {
        *(self.0.sub(LBP_SELF as usize + 8 * index) as *const Value)
    }

    ///
    /// Get a value of register slot *index*.
    ///
    pub unsafe fn set_register(&mut self, index: usize, val: Value) {
        *(self.0.sub(LBP_SELF as usize + 8 * index) as *mut Value) = val;
    }

    fn len_in_bytes(&self) -> usize {
        LBP_SELF as usize + 8 * self.meta().reg_num as usize
    }

    fn registers(&self) -> &[u8] {
        let len = self.len_in_bytes();
        unsafe { std::slice::from_raw_parts((self.0 as usize + 8 - len) as *const u8, len) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct DFP(std::ptr::NonNull<Option<DFP>>);

impl DFP {
    fn new(ptr: *mut u8) -> Self {
        DFP(std::ptr::NonNull::new(ptr as *mut Option<DFP>).unwrap())
    }

    fn get(&self) -> *const Option<DFP> {
        self.0.as_ptr()
    }

    ///
    /// Get DFP of an outer frame of *self*.
    ///
    pub fn outer(&self) -> Option<Self> {
        unsafe { *self.get() }
    }

    ///
    /// Get DFP of an outermost frame of *self*.
    ///
    fn outermost(&self) -> DFP {
        let mut dfp = *self;
        while let Some(outer) = dfp.outer() {
            dfp = outer;
        }
        dfp
    }

    ///
    /// Get LFP.
    ///
    pub fn lfp(&self) -> LFP {
        LFP(unsafe { (self.get() as *const u8).add(LBP_OUTER as usize) })
    }
}
