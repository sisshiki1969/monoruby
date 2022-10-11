use crate::*;
use num::BigInt;
use smallvec::SmallVec;

const OBJECT_INLINE_IVAR: usize = 5;

/// Heap-allocated objects.
#[derive(Clone)]
#[repr(C)]
pub struct RValue {
    /// flags. 8 bytes
    flags: RVFlag,
    /// instance variable table. 8 bytes
    var_table: Option<Box<Vec<Option<Value>>>>,
    /// object data. 48 bytes.
    pub kind: ObjKind,
}

impl std::fmt::Debug for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
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
        if let Some(v) = &self.var_table {
            v.iter().for_each(|v| {
                if let Some(v) = v {
                    v.mark(alloc)
                }
            });
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

    pub(crate) fn class(&self) -> ClassId {
        self.flags.class()
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        self.flags.change_class(new_class_id);
    }

    pub(crate) fn get_var(&mut self, id: IvarId) -> Value {
        let mut i = id.to_usize();
        if let ObjKind::Object(ary) = self.kind {
            if i < OBJECT_INLINE_IVAR {
                match ary[i] {
                    Some(v) => return v,
                    None => return Value::nil(),
                }
            } else {
                i -= OBJECT_INLINE_IVAR;
            }
        }
        if let Some(v) = &mut self.var_table {
            if v.len() > i {
                match v[i] {
                    Some(v) => return v,
                    None => {}
                }
            }
        }
        Value::nil()
    }

    pub(crate) extern "C" fn get_ivar(base: &mut RValue, id: IvarId) -> Value {
        base.get_var(id)
    }

    pub(crate) fn set_var(&mut self, id: IvarId, val: Value) {
        let mut i = id.to_usize();
        if let ObjKind::Object(ary) = &mut self.kind {
            if i < OBJECT_INLINE_IVAR {
                ary[i] = Some(val);
                return;
            } else {
                i -= OBJECT_INLINE_IVAR;
            }
        }
        match &mut self.var_table {
            Some(v) => {
                if v.len() <= i {
                    v.resize(i + 1, None);
                }
                v[i] = Some(val);
            }
            None => {
                let mut v = vec![None; i + 1];
                v[i] = Some(val);
                self.var_table = Some(Box::new(v));
            }
        }
    }

    pub(crate) extern "C" fn set_ivar(base: &mut RValue, id: IvarId, val: Value) {
        base.set_var(id, val)
    }
}

//
// constructors.
//
impl RValue {
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

    ///
    /// Create new class object with *class_id*.
    ///
    pub(crate) fn new_class(id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(CLASS_CLASS),
            kind: ObjKind::Class(id),
            var_table: None,
        }
    }

    ///
    /// Create new instance object of class *class_id*.
    ///
    pub(crate) fn new_object(class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id),
            kind: ObjKind::Object([None; 5]),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes_from_smallvec(bytes: SmallVec<[u8; 31]>) -> Self {
        RValue {
            flags: RVFlag::new(STRING_CLASS),
            kind: ObjKind::Bytes(bytes),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes(bytes: Vec<u8>) -> Self {
        let v = SmallVec::from_vec(bytes);
        RValue {
            flags: RVFlag::new(STRING_CLASS),
            kind: ObjKind::Bytes(v),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes_from_slice(bytes: &[u8]) -> Self {
        let v = SmallVec::from_slice(bytes);
        RValue {
            flags: RVFlag::new(STRING_CLASS),
            kind: ObjKind::Bytes(v),
            var_table: None,
        }
    }

    pub(crate) fn new_array(v: Vec<Value>) -> Self {
        RValue {
            flags: RVFlag::new(ARRAY_CLASS),
            kind: ObjKind::Array(v),
            var_table: None,
        }
    }

    pub(crate) fn new_array_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id),
            kind: ObjKind::Array(v),
            var_table: None,
        }
    }

    pub(crate) fn new_time(time: TimeInfo) -> Self {
        RValue {
            flags: RVFlag::new(TIME_CLASS),
            kind: ObjKind::Time(time),
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
#[repr(C)]
union RVFlag {
    flag: u64,
    next: Option<std::ptr::NonNull<RValue>>,
}

impl RVFlag {
    fn new(class: ClassId) -> Self {
        let class: u32 = class.into();
        RVFlag {
            flag: (class as u64) << 32 | 1,
        }
    }

    fn class(&self) -> ClassId {
        let flag = unsafe { self.flag };
        assert!((flag & 0b1) == 1);
        ClassId::new((flag >> 32) as u32)
    }

    fn change_class(&mut self, new_class_id: ClassId) {
        let lower_flag = unsafe { self.flag } & 0xffff_ffff;
        let class: u32 = new_class_id.into();
        self.flag = (class as u64) << 32 | lower_flag;
    }
}

#[derive(Debug, Clone)]
pub enum ObjKind {
    Class(ClassId),
    Object([Option<Value>; OBJECT_INLINE_IVAR]),
    Bignum(BigInt),
    Float(f64),
    Bytes(SmallVec<[u8; 31]>),
    Time(TimeInfo),
    Array(Vec<Value>),
    Invalid,
}
