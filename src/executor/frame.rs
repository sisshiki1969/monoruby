use super::*;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CFP(*const CFP);

impl std::default::Default for CFP {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

impl CFP {
    ///
    /// Get CFP of previous frame of *self*.
    ///
    pub fn prev(&self) -> Option<Self> {
        unsafe {
            let p = *self.0;
            if p.is_null() {
                None
            } else {
                Some(p)
            }
        }
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    pub unsafe fn return_addr(&self) -> *const usize {
        (*self.0.add(2)).0 as _
    }

    pub unsafe fn bp(&self) -> *const usize {
        self.0.add(BP_PREV_CFP as usize / 8) as _
    }

    ///
    /// Get LFP.
    ///
    pub fn lfp(&self) -> LFP {
        unsafe {
            let bp = self.bp();
            LFP(*bp.sub(BP_LFP as usize / 8) as _)
        }
    }

    ///
    /// Set LFP.
    ///
    pub unsafe fn set_lfp(&mut self, lfp: LFP) {
        let bp = self.bp() as *mut usize;
        *bp.sub(BP_LFP as usize / 8) = lfp.0 as _;
    }

    ///
    /// Get func_id of a current method / classdef.
    ///
    pub fn method_func_id(&self) -> FuncId {
        unsafe {
            let mut lfp = self.lfp();
            while let Some(outer) = lfp.outer() {
                lfp = outer.lfp();
            }
            lfp.meta().func_id()
        }
    }

    ///
    /// Get *BlockHandler* of a current method / classdef.
    ///
    pub fn get_block(&self) -> Option<BlockHandler> {
        unsafe {
            let mut lfp = self.lfp();
            while let Some(outer) = lfp.outer() {
                lfp = outer.lfp();
            }

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
    }

    ///
    /// Get func_id of a current source position.
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
        self.0 == other.0 as _
    }
}

impl std::cmp::PartialOrd<CFP> for LFP {
    fn partial_cmp(&self, other: &CFP) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&(other.0 as *const u8))
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
    pub fn move_to_heap(&self) -> Self {
        let len = self.len_in_bytes();
        let v = self.registers().to_vec().into_boxed_slice();
        LFP((Box::into_raw(v) as *mut u64 as usize + len - 8) as _)
    }

    pub unsafe fn cfp(&self) -> CFP {
        CFP(self.0.sub(BP_PREV_CFP as usize) as _)
    }

    pub unsafe fn outer_address(&self) -> DFP {
        DFP(self.0.sub(LBP_OUTER as usize) as _)
    }

    ///
    /// Get outer.
    ///
    pub unsafe fn outer(&self) -> Option<DFP> {
        self.outer_address().outer()
    }

    ///
    /// Set outer.
    ///
    pub unsafe fn set_outer(&mut self, outer: DFP) {
        *(self.outer_address().0 as *mut DFP) = outer;
    }

    ///
    /// Get Meta.
    ///
    pub(in crate::executor) fn meta(&self) -> Meta {
        Meta::from(unsafe { *(self.0.sub(LBP_META as usize) as *const u64) })
    }

    ///
    /// Get block.
    ///
    pub fn block(&self) -> Option<BlockHandler> {
        unsafe { *(self.0.sub(LBP_BLOCK as usize) as *const _) }
    }

    ///
    /// Get a value of register slot *index*.
    ///
    pub unsafe fn register(&self, index: usize) -> Value {
        *(self.0.sub(LBP_SELF as usize + 8 * index) as *const Value)
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
pub struct DFP(*mut u8);

impl DFP {
    ///
    /// Get CFP of previous frame of *self*.
    ///
    pub fn outer(&self) -> Option<Self> {
        assert!(!self.0.is_null());
        unsafe {
            match *(self.0 as *mut usize) {
                0 => None,
                p => Some(Self(p as _)),
            }
        }
    }

    ///
    /// Get LFP.
    ///
    pub fn lfp(&self) -> LFP {
        assert!(!self.0.is_null());
        LFP(unsafe { self.0.add(LBP_OUTER as usize) })
    }
}
