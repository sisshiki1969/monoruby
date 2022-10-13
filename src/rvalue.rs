use crate::*;
use num::BigInt;
use smallvec::SmallVec;
use std::mem::ManuallyDrop;

pub const OBJECT_INLINE_IVAR: usize = 6;

/// Heap-allocated objects.
//#[derive(Clone)]
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
        write!(f, "{:?}", self.kind())
    }
}

impl PartialEq for RValue {
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
            kind: ObjKind::invalid(),
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

    pub(crate) fn kind(&self) -> u8 {
        self.flags.kind()
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        self.flags.change_class(new_class_id);
    }

    pub(crate) fn deep_copy(&self) -> Self {
        RValue {
            flags: self.flags,
            var_table: self.var_table.clone(),
            kind: match self.kind() {
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                ObjKind::CLASS => ObjKind::class(self.as_class()),
                ObjKind::OBJECT => ObjKind::object(),
                ObjKind::BIGNUM => ObjKind::bignum(self.as_bignum().clone()),
                ObjKind::FLOAT => ObjKind {
                    float: self.as_float(),
                },
                ObjKind::BYTES => ObjKind::bytes(self.as_string().clone()),
                ObjKind::TIME => ObjKind::time(self.as_time().clone()),
                ObjKind::ARRAY => ObjKind::array(
                    self.as_array()
                        .iter()
                        .map(|v| Value::deep_copy(*v))
                        .collect(),
                ),
                _ => unreachable!("clone()"),
            },
        }
    }

    pub(crate) fn get_var(&mut self, id: IvarId) -> Value {
        let mut i = id.to_usize();
        if self.kind() == ObjKind::OBJECT {
            if i < OBJECT_INLINE_IVAR {
                match self.as_object()[i] {
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
        if self.kind() == ObjKind::OBJECT {
            if i < OBJECT_INLINE_IVAR {
                self.as_object_mut()[i] = Some(val);
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
            flags: RVFlag::new(INTEGER_CLASS, ObjKind::BIGNUM),
            kind: ObjKind::bignum(bigint),
            var_table: None,
        }
    }

    pub(crate) fn new_float(f: f64) -> Self {
        RValue {
            flags: RVFlag::new(FLOAT_CLASS, ObjKind::FLOAT),
            kind: ObjKind::float(f),
            var_table: None,
        }
    }

    ///
    /// Create new class object with *class_id*.
    ///
    pub(crate) fn new_class(id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(CLASS_CLASS, ObjKind::CLASS),
            kind: ObjKind::class(id),
            var_table: None,
        }
    }

    ///
    /// Create new instance object of class *class_id*.
    ///
    pub(crate) fn new_object(class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id, ObjKind::OBJECT),
            kind: ObjKind::object(),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes_from_smallvec(bytes: InnerVec) -> Self {
        RValue {
            flags: RVFlag::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes(bytes),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes(bytes: Vec<u8>) -> Self {
        let v = SmallVec::from_vec(bytes);
        RValue {
            flags: RVFlag::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes(v),
            var_table: None,
        }
    }

    pub(crate) fn new_bytes_from_slice(bytes: &[u8]) -> Self {
        let v = SmallVec::from_slice(bytes);
        RValue {
            flags: RVFlag::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes(v),
            var_table: None,
        }
    }

    pub(crate) fn new_array(v: Vec<Value>) -> Self {
        RValue {
            flags: RVFlag::new(ARRAY_CLASS, ObjKind::ARRAY),
            kind: ObjKind::array(v),
            var_table: None,
        }
    }

    pub(crate) fn new_array_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id, ObjKind::ARRAY),
            kind: ObjKind::array(v),
            var_table: None,
        }
    }

    pub(crate) fn new_time(time: TimeInfo) -> Self {
        RValue {
            flags: RVFlag::new(TIME_CLASS, ObjKind::TIME),
            kind: ObjKind::time(time),
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
    flag: Flag,
    next: Option<std::ptr::NonNull<RValue>>,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct Flag {
    flag: u16,
    kind: u16,
    class: ClassId,
}

impl RVFlag {
    fn new(class: ClassId, kind: u8) -> Self {
        RVFlag {
            flag: Flag {
                flag: 1,
                kind: kind as u16,
                class,
            },
        }
    }

    fn class(&self) -> ClassId {
        let Flag { flag, class, .. } = unsafe { self.flag };
        assert!((flag & 0b1) == 1);
        class
    }

    fn kind(&self) -> u8 {
        unsafe { self.flag.kind as u8 }
    }

    fn change_class(&mut self, class: ClassId) {
        self.flag.class = class;
    }
}

#[repr(C)]
pub union ObjKind {
    pub invalid: (),
    pub class: ClassId,
    pub object: [Option<Value>; OBJECT_INLINE_IVAR],
    pub bignum: ManuallyDrop<BigInt>,
    pub float: f64,
    pub bytes: ManuallyDrop<InnerVec>,
    pub time: ManuallyDrop<TimeInfo>,
    pub array: ManuallyDrop<Vec<Value>>,
}

impl ObjKind {
    pub const INVALID: u8 = 0;
    pub const CLASS: u8 = 1;
    //pub const MODULE: u8 = 2;
    pub const OBJECT: u8 = 3;
    pub const BIGNUM: u8 = 4;
    pub const FLOAT: u8 = 5;
    pub const BYTES: u8 = 6;
    pub const TIME: u8 = 7;
    pub const ARRAY: u8 = 8;
}

impl ObjKind {
    fn invalid() -> Self {
        Self { invalid: () }
    }

    fn class(class: ClassId) -> Self {
        Self { class }
    }

    fn object() -> Self {
        Self {
            object: [None; OBJECT_INLINE_IVAR],
        }
    }

    fn bignum(b: BigInt) -> Self {
        Self {
            bignum: ManuallyDrop::new(b),
        }
    }

    fn float(float: f64) -> Self {
        Self { float }
    }

    fn bytes(b: InnerVec) -> Self {
        Self {
            bytes: ManuallyDrop::new(b),
        }
    }

    fn array(v: Vec<Value>) -> Self {
        Self {
            array: ManuallyDrop::new(v),
        }
    }

    fn time(info: TimeInfo) -> Self {
        Self {
            time: ManuallyDrop::new(info),
        }
    }
}

impl RValue {
    pub(crate) fn as_object(&self) -> &[Option<value::Value>; OBJECT_INLINE_IVAR] {
        unsafe { &self.kind.object }
    }

    pub(crate) fn as_object_mut(&mut self) -> &mut [Option<value::Value>; OBJECT_INLINE_IVAR] {
        unsafe { &mut self.kind.object }
    }

    pub(crate) fn as_class(&self) -> ClassId {
        unsafe { self.kind.class }
    }

    pub(crate) fn as_float(&self) -> f64 {
        unsafe { self.kind.float }
    }

    pub(crate) fn as_bignum(&self) -> &BigInt {
        unsafe { &*self.kind.bignum }
    }

    pub(crate) fn as_string(&self) -> &InnerVec {
        unsafe { &*self.kind.bytes }
    }

    /*pub(crate) fn as_string_mut(&mut self) -> &mut InnerVec {
        unsafe { &mut *self.kind.bytes }
    }*/

    pub(crate) fn as_array(&self) -> &Vec<Value> {
        unsafe { &*self.kind.array }
    }

    pub(crate) fn as_array_mut(&mut self) -> &mut Vec<Value> {
        unsafe { &mut *self.kind.array }
    }

    pub(crate) fn as_time(&self) -> &TimeInfo {
        unsafe { &*self.kind.time }
    }

    /*pub(crate) fn as_time_mut(&mut self) -> &mut TimeInfo {
        unsafe { &mut *self.kind.time }
    }*/
}
