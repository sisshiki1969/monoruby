use crate::*;
use num::BigInt;

pub type ValueTable = HashMap<IdentId, Value>;

/// Heap-allocated objects.
#[derive(Clone)]
pub struct RValue {
    flags: RVFlag,
    var_table: Option<Box<ValueTable>>,
    pub kind: ObjKind,
}

impl std::fmt::Debug for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl std::fmt::Display for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl PartialEq for RValue {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}
impl GC<RValue> for RValue {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if alloc.gc_check_and_mark(self) {
            return;
        }
        match &self.var_table {
            Some(table) => table.values().for_each(|v| v.mark(alloc)),
            None => {}
        }
    }
}

impl GCBox for RValue {
    fn free(&mut self) {}

    fn next(&self) -> Option<std::ptr::NonNull<RValue>> {
        let next = unsafe { self.flags.next };
        assert!(unsafe { std::mem::transmute::<_, u64>(next) } & 0b1 != 1);
        next
    }

    fn set_next_none(&mut self) {
        self.flags.next = None;
    }

    fn set_next(&mut self, next: *mut RValue) {
        self.flags.next = Some(std::ptr::NonNull::new(next).unwrap());
    }

    fn new_invalid() -> Self {
        RValue {
            flags: RVFlag { next: None },
            kind: ObjKind::Invalid,
            var_table: None,
        }
    }
}

impl RValue {
    pub(crate) fn id(&self) -> u64 {
        self as *const RValue as u64
    }

    pub(crate) fn class(&self) -> u32 {
        self.flags.class()
    }

    pub(crate) fn new_bigint(bigint: BigInt) -> Self {
        RValue {
            flags: RVFlag::new(INTEGER_CLASS),
            kind: ObjKind::Bignum(bigint),
            var_table: None,
        }
    }

    pub(crate) fn new_float(f: f64) -> Self {
        RValue {
            flags: RVFlag::new(FLOAT_CLASS),
            kind: ObjKind::Float(f),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes(bytes: Vec<u8>) -> Self {
        RValue {
            flags: RVFlag::new(STRING_CLASS),
            kind: ObjKind::Bytes(bytes),
            var_table: None,
        }
    }
}

impl RValue {
    /// Pack `self` into `Value`(64-bit data representation).
    ///
    /// This method consumes `self` and allocates it on the heap, returning `Value`,
    /// a wrapped raw pointer.  
    pub(crate) fn pack(self) -> Value {
        let ptr = ALLOC.with(|alloc| alloc.borrow_mut().alloc(self));
        Value::from_ptr(ptr)
    }
}

#[derive(Clone, Copy)]
union RVFlag {
    flag: u64,
    next: Option<std::ptr::NonNull<RValue>>,
}

pub const NIL_CLASS: u32 = 1;
pub const TRUE_CLASS: u32 = 2;
pub const FALSE_CLASS: u32 = 3;
pub const INTEGER_CLASS: u32 = 4;
pub const FLOAT_CLASS: u32 = 5;
pub const STRING_CLASS: u32 = 6;
pub const SYMBOL_CLASS: u32 = 7;

impl RVFlag {
    fn new(class: u32) -> Self {
        RVFlag {
            flag: (class as u64) << 32 | 1,
        }
    }

    fn class(&self) -> u32 {
        let flag = unsafe { self.flag };
        assert!((flag & 0b1) == 1);
        (flag >> 32) as u32
    }
}

#[derive(Debug, Clone)]
pub enum ObjKind {
    Bignum(BigInt),
    Float(f64),
    Bytes(Vec<u8>),
    Invalid,
    Dummy(u64, u64, u64, u64, u64),
}

impl std::fmt::Display for ObjKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ObjKind::Bignum(num) => write!(f, "{}", num),
            ObjKind::Float(num) => write!(f, "{}", num),
            _ => write!(f, "{:?}", self),
        }
    }
}
