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
    fn as_ptr(&self) -> *const Option<CFP> {
        self.0.as_ptr()
    }

    ///
    /// Get a previous control frame of *self*.
    ///
    pub fn prev(&self) -> Option<Self> {
        unsafe { *self.as_ptr() }
    }

    ///
    /// Get base pointer address of *self*.
    ///
    pub unsafe fn bp(&self) -> *const usize {
        self.as_ptr().add(BP_PREV_CFP as usize / 8) as _
    }

    ///
    /// Get LFP.
    ///
    pub(super) fn lfp(&self) -> LFP {
        unsafe {
            let bp = self.bp();
            LFP::new(*bp.sub(BP_LFP as usize / 8) as _)
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
        *bp.sub(BP_LFP as usize / 8) = lfp.as_ptr() as _;
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
                BlockHandler::new(Value::integer(i))
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
pub struct LFP(std::ptr::NonNull<u8>);

impl std::ops::Deref for LFP {
    type Target = std::ptr::NonNull<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::cmp::PartialEq<CFP> for LFP {
    fn eq(&self, other: &CFP) -> bool {
        self.as_ptr() == other.as_ptr() as _
    }
}

impl std::cmp::PartialOrd<CFP> for LFP {
    fn partial_cmp(&self, other: &CFP) -> Option<std::cmp::Ordering> {
        self.as_ptr().partial_cmp(&(other.as_ptr() as _))
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
    pub unsafe fn new(ptr: *mut u8) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    pub fn sub(self, count: i64) -> *mut u8 {
        unsafe { self.as_ptr().sub(count as usize) }
    }

    ///
    /// Move local frame on the stack to the heap.
    ///
    pub fn move_to_heap(&self) -> Self {
        let len = self.len_in_bytes();
        let v = self.registers().to_vec().into_boxed_slice();
        unsafe { LFP::new((Box::into_raw(v) as *mut u64 as usize + len - 8) as _) }
    }

    ///
    /// Get CFP.
    ///
    pub unsafe fn cfp(&self) -> CFP {
        CFP::new(self.sub(BP_PREV_CFP) as _)
    }

    ///
    /// Get the address of outer.
    ///
    pub fn outer_address(&self) -> DFP {
        unsafe { DFP::new(self.sub(LBP_OUTER) as _) }
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
        Meta::from(unsafe { *(self.sub(LBP_META) as *const u64) })
    }

    ///
    /// Get the length of arguments for a native function.
    ///
    pub fn arg_len(&self) -> usize {
        self.meta().reg_num as usize - 1
    }

    ///
    /// Get *self*.
    ///
    pub fn self_val(&self) -> Value {
        unsafe { *(self.sub(LBP_SELF) as *const _) }
    }

    ///
    /// Get block.
    ///
    pub fn block(&self) -> Option<BlockHandler> {
        unsafe { *(self.sub(LBP_BLOCK) as *const _) }
    }

    pub fn expect_block(&self) -> Result<BlockHandler> {
        if let Some(block) = self.block() {
            Ok(block)
        } else {
            Err(MonorubyErr::no_block_given())
        }
    }

    pub fn expect_no_block(&self) -> Result<()> {
        if self.block().is_none() {
            Ok(())
        } else {
            Err(MonorubyErr::runtimeerr("not supported."))
        }
    }

    ///
    /// Get a value of register slot *index*.
    ///
    pub unsafe fn register(&self, index: usize) -> Value {
        *(self.sub(LBP_SELF + 8 * index as i64) as *const Value)
    }

    ///
    /// Get a value of register slot *index*.
    ///
    pub unsafe fn set_register(&mut self, index: usize, val: Value) {
        *(self.sub(LBP_SELF + 8 * index as i64) as *mut Value) = val;
    }

    fn len_in_bytes(&self) -> usize {
        LBP_SELF as usize + 8 * self.meta().reg_num as usize
    }

    fn registers(&self) -> &[u8] {
        let len = self.len_in_bytes();
        unsafe {
            std::slice::from_raw_parts((self.0.as_ptr() as usize + 8 - len) as *const u8, len)
        }
    }

    // APIs for native methods.

    pub fn to_vec(&self) -> Vec<Value> {
        self.iter().collect()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = Value> + '_ {
        self.iter_inner().rev().cloned()
    }

    pub fn rev(&self) -> impl Iterator<Item = Value> + '_ {
        self.iter_inner().cloned()
    }

    fn iter_inner(&self) -> impl DoubleEndedIterator<Item = &Value> {
        let len = self.arg_len();
        unsafe {
            let data = if len == 0 {
                self.0.as_ptr().sub(LBP_ARG0 as usize)
            } else {
                self.0.as_ptr().sub(LBP_ARG0 as usize + len * 8 - 8)
            };
            std::slice::from_raw_parts(data as *const Value, len).iter()
        }
    }

    pub fn arg(&self, i: usize) -> Value {
        unsafe { *(self.0.as_ptr().sub(LBP_ARG0 as usize + i * 8) as *mut Value) }
    }

    pub fn as_arg(&self) -> Arg {
        unsafe {
            Arg::from(
                (self.0.as_ptr().sub(LBP_ARG0 as usize) as *mut Value)
                    .as_ref()
                    .unwrap(),
            )
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct DFP(std::ptr::NonNull<Option<DFP>>);

impl DFP {
    unsafe fn new(ptr: *mut u8) -> Self {
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
        unsafe { LFP::new((self.get() as *const u8).add(LBP_OUTER as usize) as _) }
    }
}
