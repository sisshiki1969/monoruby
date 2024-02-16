use super::*;

///
/// Control frame pointer.
///
/// CFP points to a control frame which corresponds to an each function call.
/// The control frame contains a CFP of the previous control frame, LFP, and a base pointer.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Cfp(std::ptr::NonNull<Option<Cfp>>);

impl Cfp {
    ///
    /// Create new CFP from a raw pointer.
    ///
    /// ### safety
    /// This function is extremely dangerous. Programmer must ensure that *ptr* is a valid pointer which pointes to a control frame.
    ///
    unsafe fn new(ptr: *mut u8) -> Self {
        Cfp(std::ptr::NonNull::new(ptr as *mut Option<Cfp>).unwrap())
    }

    ///
    /// Get inner raw pointer.
    ///
    fn as_ptr(&self) -> *const Option<Cfp> {
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
    pub(crate) fn lfp(&self) -> Lfp {
        unsafe {
            let bp = self.bp();
            Lfp::new(*bp.sub(BP_LFP as usize / 8) as _)
        }
    }

    ///
    /// Get outermost LFP.
    ///
    pub(crate) fn outermost_lfp(&self) -> Lfp {
        match self.lfp().outer() {
            Some(dfp) => dfp.outermost().0.lfp(),
            None => self.lfp(),
        }
    }

    ///
    /// Get outermost LFP and the depth.
    ///
    pub(crate) fn outermost_lfp_depth(&self) -> (Lfp, usize) {
        match self.lfp().outer() {
            Some(dfp) => {
                let (dfp, depth) = dfp.outermost();
                (dfp.lfp(), depth)
            }
            None => (self.lfp(), 0),
        }
    }

    pub(crate) fn block_given(&self) -> bool {
        self.outermost_lfp().block().is_some()
    }

    ///
    /// Set LFP.
    ///
    pub unsafe fn set_lfp(&mut self, lfp: Lfp) {
        let bp = self.bp() as *mut usize;
        *bp.sub(BP_LFP as usize / 8) = lfp.as_ptr() as _;
    }

    ///
    /// Get *FuncId* of a current method / classdef.
    ///
    pub fn method_func_id(&self) -> FuncId {
        self.outermost_lfp().meta().func_id()
    }

    pub fn method_func_id_depth(&self) -> (FuncId, usize) {
        let (lfp, depth) = self.outermost_lfp_depth();
        (lfp.meta().func_id(), depth)
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

///
/// Local frame pointer.
///
/// the LFP points a local frame which contains self value, local variables, given block, and meta data which correspond to an each function call.
/// Most local frames are on the stack, but some local frames are placed on the heap in such situations like closures.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Lfp(std::ptr::NonNull<u8>);

impl std::ops::Deref for Lfp {
    type Target = std::ptr::NonNull<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::cmp::PartialEq<Cfp> for Lfp {
    fn eq(&self, other: &Cfp) -> bool {
        self.as_ptr() == other.as_ptr() as _
    }
}

impl std::cmp::PartialOrd<Cfp> for Lfp {
    fn partial_cmp(&self, other: &Cfp) -> Option<std::cmp::Ordering> {
        self.as_ptr().partial_cmp(&(other.as_ptr() as _))
    }
}

impl alloc::GC<RValue> for Lfp {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        unsafe {
            let meta = self.meta();
            for r in 0..meta.reg_num() as usize {
                if let Some(v) = self.register(r) {
                    v.mark(alloc);
                }
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

impl Lfp {
    ///
    /// Create LFP from a raw pointer.
    ///
    /// This function is extremely dangerous. Programmer must ensure that *ptr* is a valid pointer which points to a local frame.
    ///
    unsafe fn new(ptr: *mut u8) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    ///
    /// Get CFP.
    ///
    /// ### safety
    ///
    /// *self* must be on the stack. otherwise, panic.
    ///
    fn cfp(&self) -> Cfp {
        assert!(self.on_stack());
        unsafe { Cfp::new(self.sub(BP_PREV_CFP) as _) }
    }

    ///
    /// Set outer.
    ///
    unsafe fn set_outer(&mut self, outer: Option<Dfp>) {
        *(self.outer_address().0.as_ptr()) = outer;
    }

    fn sub(self, count: i64) -> *mut u8 {
        unsafe { self.as_ptr().sub(count as usize) }
    }

    ///
    /// Get the address of outer.
    ///
    fn outer_address(&self) -> Dfp {
        unsafe { Dfp::new(self.sub(LBP_OUTER) as _) }
    }

    fn meta_mut(&mut self) -> &mut Meta {
        unsafe { &mut *(self.sub(LBP_META) as *mut Meta) }
    }

    fn on_stack(&self) -> bool {
        self.meta().on_stack()
    }

    fn frame_bytes(&self) -> usize {
        LBP_SELF as usize + 8 * self.meta().reg_num() as usize
    }

    fn frame_ref(&self) -> &[u8] {
        let len = self.frame_bytes();
        unsafe {
            std::slice::from_raw_parts((self.0.as_ptr() as usize + 8 - len) as *const u8, len)
        }
    }

    ///
    /// Returns an iterator over the arguments of a natve function.
    ///
    /// The iterator yields all values from start to end.
    ///
    /// ### safety
    ///
    /// *self* must be native function. otherwise, panic.
    ///
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

    ///
    /// Move the frame to heap.
    ///
    /// If the frame is already on the heap, do nothing.
    ///
    /// ### args
    /// - *lfp*: the address of the frame to move.
    ///
    /// ### return
    /// - the frame moved to the heap.
    ///
    pub fn move_frame_to_heap(self) -> Self {
        if self.on_stack() {
            unsafe {
                let mut cfp = self.cfp();
                let len = self.frame_bytes();
                let v = self.frame_ref().to_vec().into_boxed_slice();
                let mut heap_lfp = Lfp::new((Box::into_raw(v) as *mut u64 as usize + len - 8) as _);
                heap_lfp.meta_mut().set_on_heap();
                cfp.set_lfp(heap_lfp);
                if let Some(outer) = heap_lfp.outer() {
                    let outer_lfp = outer.lfp();
                    let outer = outer_lfp.move_frame_to_heap().outer_address();
                    heap_lfp.set_outer(Some(outer));
                }
                assert!(!heap_lfp.on_stack());
                heap_lfp
            }
        } else {
            self
        }
    }

    pub fn dummy_heap_frame_with_self(self_val: Value) -> Self {
        unsafe {
            let v = vec![0, 0, self_val.id(), 0, 0, 0, 0, 0, 0].into_boxed_slice();
            let len = v.len() * 8;
            let mut heap_lfp = Lfp::new((Box::into_raw(v) as *mut u64 as usize + len - 8) as _);
            heap_lfp.meta_mut().set_on_heap();
            heap_lfp.meta_mut().set_reg_num(1);
            assert!(!heap_lfp.on_stack());
            heap_lfp
        }
    }

    ///
    /// Get outer DFP.
    ///
    pub fn outer(&self) -> Option<Dfp> {
        self.outer_address().outer()
    }

    ///
    /// Get Meta.
    ///
    pub(crate) fn meta(&self) -> &Meta {
        unsafe { &*(self.sub(LBP_META) as *const Meta) }
    }

    ///
    /// Get block.
    ///
    pub fn block(&self) -> Option<BlockHandler> {
        unsafe { *(self.sub(LBP_BLOCK) as *const _) }
    }

    ///
    /// Set block.
    ///
    pub fn set_block(&self, bh: Option<BlockHandler>) {
        unsafe { *(self.sub(LBP_BLOCK) as *mut _) = bh }
    }

    pub unsafe fn register_ptr(&self, index: usize) -> *mut Option<Value> {
        self.sub(LBP_SELF + 8 * index as i64) as _
    }

    ///
    /// Get a value of a register slot *index*.
    ///
    pub unsafe fn register(&self, index: usize) -> Option<Value> {
        std::ptr::read(self.register_ptr(index))
    }

    /// Get a value of a register slot *index*.
    ///
    pub(crate) unsafe fn get_slot(&self, index: SlotId) -> Option<Value> {
        self.register(index.0 as usize)
    }

    ///
    /// Set a value to a register *index*.
    ///
    pub(crate) unsafe fn set_register(&mut self, index: usize, val: Option<Value>) {
        std::ptr::write(self.register_ptr(index), val);
    }
}

// APIs for native methods.
impl Lfp {
    ///
    /// Get the length of arguments for a native function.
    ///
    /// ### safety
    ///
    /// *self* must be native function. otherwise, panic.
    ///
    pub fn arg_len(&self) -> usize {
        assert!(self.meta().is_native());
        self.meta().reg_num() as usize - 1
    }

    ///
    /// Get *self*.
    ///
    pub fn self_val(&self) -> Value {
        unsafe { *(self.sub(LBP_SELF) as *const _) }
    }

    ///
    /// Get the given block.
    ///
    /// If none, return Err.
    ///
    pub fn expect_block(&self) -> Result<BlockHandler> {
        if let Some(block) = self.block() {
            Ok(block)
        } else {
            Err(MonorubyErr::no_block_given())
        }
    }

    ///
    /// Ensure a block was not given.
    ///
    /// If given, return Err.
    ///
    pub fn expect_no_block(&self) -> Result<()> {
        if self.block().is_none() {
            Ok(())
        } else {
            Err(MonorubyErr::runtimeerr("not supported."))
        }
    }

    pub fn to_vec(&self) -> Vec<Value> {
        self.iter().collect()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = Value> + '_ {
        self.iter_inner().rev().cloned()
    }

    pub fn rev(&self) -> impl Iterator<Item = Value> + '_ {
        self.iter_inner().cloned()
    }

    pub fn slice(&self, start_pos: usize, len: usize) -> impl DoubleEndedIterator<Item = Value> {
        unsafe {
            let ptr = self.register_ptr(start_pos + len);
            std::slice::from_raw_parts(ptr, len)
                .iter()
                .rev()
                .map(|v| v.unwrap())
        }
    }

    pub fn arg(&self, i: usize) -> Value {
        self.try_arg(i).unwrap()
    }

    pub fn try_arg(&self, i: usize) -> Option<Value> {
        let v = unsafe { *((self.0.as_ptr().sub(LBP_ARG0 as usize + i * 8)) as *const u64) };
        if v == 0 {
            None
        } else {
            Some(Value::from(v))
        }
    }
}

///
/// Dynamic frame pointer.
///
/// The DFP points to a DFP in the frame that holds a scope outside the current frame.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Dfp(std::ptr::NonNull<Option<Dfp>>);

impl Dfp {
    unsafe fn new(ptr: *mut u8) -> Self {
        Dfp(std::ptr::NonNull::new(ptr as *mut Option<Dfp>).unwrap())
    }

    fn get(&self) -> *const Option<Dfp> {
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
    fn outermost(&self) -> (Dfp, usize) {
        let mut dfp = *self;
        let mut depth = 0;
        while let Some(outer) = dfp.outer() {
            dfp = outer;
            depth += 1;
        }
        (dfp, depth)
    }

    ///
    /// Get LFP.
    ///
    pub fn lfp(&self) -> Lfp {
        unsafe { Lfp::new((self.get() as *const u8).add(LBP_OUTER as usize) as _) }
    }
}
