use crate::*;
use num::BigInt;
use smallvec::SmallVec;
use std::mem::ManuallyDrop;

pub use self::hash::*;

mod hash;

pub const OBJECT_INLINE_IVAR: usize = 6;

/// Heap-allocated objects.
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
        let flag = unsafe { self.flags.flag };
        assert_eq!(1, flag.flag & 0b1);
        write!(
            f,
            "RValue {{ class:{:?} {} {:016x} }}",
            flag.class,
            unsafe {
                match flag.kind {
                    0 => "<INVALID>".to_string(),
                    1 => format!("CLASS({:?})", self.kind.class),
                    2 => format!("MODULE({:?})", self.kind.class),
                    3 => format!("OBJECT({:?})", self.kind.object),
                    4 => format!("BIGNUM({:?})", self.kind.bignum),
                    5 => format!("FLOAT({:?})", self.kind.float),
                    6 => format!(
                        "STRING({:?})",
                        String::from_utf8_lossy(self.kind.string.0.as_ref())
                    ),
                    7 => format!("TIME({:?})", self.kind.time),
                    8 => format!("ARRAY({:?})", self.kind.array),
                    9 => format!("RANGE({:?})", self.kind.range),
                    10 => format!("SPLAT({:?})", self.kind.array),
                    11 => format!("PROC({:?})", self.kind.proc),
                    12 => format!("HASH({:?})", self.kind.hash),
                    _ => unreachable!(),
                }
            },
            self as *const RValue as u64
        )
    }
}

impl PartialEq for RValue {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl std::hash::Hash for RValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self.kind() {
            ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
            ObjKind::BIGNUM => self.as_bignum().hash(state),
            ObjKind::FLOAT => self.as_float().to_bits().hash(state),
            ObjKind::BYTES => self.as_bytes().hash(state),
            ObjKind::ARRAY => self.as_array().hash(state),
            ObjKind::RANGE => self.as_range().hash(state),
            ObjKind::HASH => self.as_hash().hash(state),
            //ObjKind::METHOD => lhs.method().hash(state),
            _ => self.hash(state),
        }
    }
}

impl RValue {
    // This type of equality is used for comparison for keys of Hash.
    pub(crate) fn eql(&self, other: &Self) -> bool {
        match (self.kind(), other.kind()) {
            (ObjKind::OBJECT, ObjKind::OBJECT) => self.id() == other.id(),
            (ObjKind::BIGNUM, ObjKind::BIGNUM) => self.as_bignum() == other.as_bignum(),
            (ObjKind::FLOAT, ObjKind::FLOAT) => self.as_float() == other.as_float(),
            //(ObjKind::COMPLEX, ObjKind::COMPLEX) => {
            //    self.complex().r.eql(&other.complex().r) && self.complex().i.eql(&other.complex().i)
            //}
            (ObjKind::BYTES, ObjKind::BYTES) => self.as_bytes() == other.as_bytes(),
            (ObjKind::ARRAY, ObjKind::ARRAY) => {
                let lhs = self.as_array();
                let rhs = other.as_array();
                if lhs.len() != rhs.len() {
                    return false;
                }
                lhs.iter().zip(rhs.iter()).all(|(a1, a2)| {
                    // Support self-containing arrays.
                    if self.id() == a1.get() && other.id() == a2.get() {
                        true
                    } else if self.id() == a1.get() || other.id() == a2.get() {
                        false
                    } else {
                        a1.eql(a2)
                    }
                })
            }
            (ObjKind::RANGE, ObjKind::RANGE) => self.as_range().eql(other.as_range()),
            (ObjKind::HASH, ObjKind::HASH) => self.as_hash() == other.as_hash(),
            //(ObjKind::METHOD, ObjKind::METHOD) => *self.method() == *other.method(),
            //(ObjKind::UNBOUND_METHOD, ObjKind::UNBOUND_METHOD) => *self.method() == *other.method(),
            (ObjKind::INVALID, _) => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
            (_, ObjKind::INVALID) => panic!("Invalid rvalue. (maybe GC problem) {:?}", other),
            _ => false,
        }
    }
}

impl GC<RValue> for RValue {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if alloc.gc_check_and_mark(self) {
            return;
        }
        if let Some(v) = &self.var_table {
            v.iter().for_each(|v| {
                v.map(|v| {
                    v.mark(alloc);
                });
            });
        }
        match self.kind() {
            ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
            ObjKind::CLASS | ObjKind::MODULE => {}
            ObjKind::OBJECT => {
                self.as_object().iter().for_each(|v| {
                    v.map(|v| {
                        v.mark(alloc);
                    });
                });
            }
            ObjKind::BIGNUM | ObjKind::FLOAT | ObjKind::TIME | ObjKind::BYTES => {}
            ObjKind::ARRAY | ObjKind::SPLAT => {
                self.as_array().iter().for_each(|v| v.mark(alloc));
            }
            ObjKind::RANGE => {
                let range = self.as_range();
                range.start.mark(alloc);
                range.end.mark(alloc);
            }
            ObjKind::HASH => {
                for (k, v) in self.as_hash().iter() {
                    k.mark(alloc);
                    v.mark(alloc);
                }
            }
            _ => unreachable!("mark"),
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

    pub(crate) fn kind(&self) -> u8 {
        self.flags.kind()
    }

    pub(crate) fn get_var(&mut self, id: IvarId) -> Option<Value> {
        let mut i = id.into_usize();
        if self.kind() == ObjKind::OBJECT {
            if i < OBJECT_INLINE_IVAR {
                return self.as_object()[i];
            } else {
                i -= OBJECT_INLINE_IVAR;
            }
        }
        if let Some(v) = &mut self.var_table {
            if v.len() > i {
                return v[i];
            }
        }
        None
    }

    pub(crate) extern "C" fn get_ivar(base: &mut RValue, id: IvarId) -> Value {
        base.get_var(id).unwrap_or_default()
    }

    pub(crate) fn set_var(&mut self, id: IvarId, val: Value) {
        let mut i = id.into_usize();
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

    pub(super) fn change_class(&mut self, new_class_id: ClassId) {
        self.flags.change_class(new_class_id);
    }

    pub(super) fn deep_copy(&self) -> Self {
        RValue {
            flags: self.flags,
            var_table: match &self.var_table {
                Some(box table) => Some(Box::new(
                    table
                        .iter()
                        .map(|v| v.map(|v| Value::deep_copy(v)))
                        .collect(),
                )),
                None => None,
            },
            kind: match self.kind() {
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                ObjKind::CLASS => ObjKind::class(self.as_class()),
                ObjKind::OBJECT => ObjKind::object(),
                ObjKind::BIGNUM => ObjKind::bignum(self.as_bignum().clone()),
                ObjKind::FLOAT => ObjKind {
                    float: self.as_float(),
                },
                ObjKind::BYTES => ObjKind::bytes(self.as_bytes()),
                ObjKind::TIME => ObjKind::time(self.as_time().clone()),
                ObjKind::ARRAY => ObjKind::array(ArrayInner::new(
                    self.as_array()
                        .iter()
                        .map(|v| Value::deep_copy(*v))
                        .collect(),
                )),
                ObjKind::RANGE => {
                    let lhs = self.as_range();
                    ObjKind::range(
                        Value::deep_copy(lhs.start),
                        Value::deep_copy(lhs.end),
                        lhs.exclude_end != 0,
                    )
                }
                ObjKind::HASH => {
                    let mut map = IndexMap::default();
                    let hash = self.as_hash();
                    for (k, v) in hash.iter() {
                        map.insert(HashKey(Value::deep_copy(k)), Value::deep_copy(v));
                    }
                    ObjKind::hash(map)
                }
                _ => unreachable!("clone()"),
            },
        }
    }

    pub(super) fn class(&self) -> ClassId {
        self.flags.class()
    }
}

//
// constructors.
//
impl RValue {
    pub(super) fn new_bigint(bigint: BigInt) -> Self {
        RValue {
            flags: RVFlag::new(INTEGER_CLASS, ObjKind::BIGNUM),
            kind: ObjKind::bignum(bigint),
            var_table: None,
        }
    }

    pub(super) fn new_float(f: f64) -> Self {
        RValue {
            flags: RVFlag::new(FLOAT_CLASS, ObjKind::FLOAT),
            kind: ObjKind::float(f),
            var_table: None,
        }
    }

    ///
    /// Create new class object with *class_id*.
    ///
    pub(super) fn new_class(id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(CLASS_CLASS, ObjKind::CLASS),
            kind: ObjKind::class(id),
            var_table: None,
        }
    }

    ///
    /// Create new instance object of class *class_id*.
    ///
    pub(super) fn new_object(class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id, ObjKind::OBJECT),
            kind: ObjKind::object(),
            var_table: None,
        }
    }

    pub(super) fn new_string(s: String) -> Self {
        Self::new_bytes(s.into_bytes())
    }

    pub(super) fn new_bytes(v: Vec<u8>) -> Self {
        RValue {
            flags: RVFlag::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes_from_vec(v),
            var_table: None,
        }
    }

    pub(super) fn new_bytes_from_slice(slice: &[u8]) -> Self {
        RValue {
            flags: RVFlag::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes(slice),
            var_table: None,
        }
    }

    pub(super) fn new_array(ary: ArrayInner) -> Self {
        RValue {
            flags: RVFlag::new(ARRAY_CLASS, ObjKind::ARRAY),
            kind: ObjKind::array(ary),
            var_table: None,
        }
    }

    pub(super) fn new_array_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id, ObjKind::ARRAY),
            kind: ObjKind::array(ArrayInner::new(v)),
            var_table: None,
        }
    }

    pub(super) fn new_hash(map: IndexMap<HashKey, Value>) -> Self {
        RValue {
            flags: RVFlag::new(HASH_CLASS, ObjKind::HASH),
            kind: ObjKind::hash(map),
            var_table: None,
        }
    }

    pub(super) fn new_hash_with_class(map: IndexMap<HashKey, Value>, class_id: ClassId) -> Self {
        RValue {
            flags: RVFlag::new(class_id, ObjKind::HASH),
            kind: ObjKind::hash(map),
            var_table: None,
        }
    }

    pub(super) fn new_splat(ary: ArrayInner) -> Self {
        RValue {
            flags: RVFlag::new(ARRAY_CLASS, ObjKind::SPLAT),
            kind: ObjKind::array(ary),
            var_table: None,
        }
    }

    pub(super) fn new_time(time: TimeInfo) -> Self {
        RValue {
            flags: RVFlag::new(TIME_CLASS, ObjKind::TIME),
            kind: ObjKind::time(time),
            var_table: None,
        }
    }

    pub(super) fn new_range(start: Value, end: Value, exclude_end: bool) -> Self {
        RValue {
            flags: RVFlag::new(RANGE_CLASS, ObjKind::RANGE),
            kind: ObjKind::range(start, end, exclude_end),
            var_table: None,
        }
    }

    pub(super) fn new_proc(block_data: BlockData) -> Self {
        RValue {
            flags: RVFlag::new(PROC_CLASS, ObjKind::PROC),
            kind: ObjKind::proc(block_data),
            var_table: None,
        }
    }
}

impl RValue {
    pub fn unpack(&self) -> RV {
        match self.kind() {
            ObjKind::BIGNUM => RV::BigInt(self.as_bignum()),
            ObjKind::FLOAT => RV::Float(self.as_float()),
            ObjKind::BYTES => RV::String(self.as_bytes()),
            _ => RV::Object(self),
        }
    }
}

impl RValue {
    /// This function is only used for system assertion.
    pub(crate) fn eq(lhs: &Self, rhs: &Self) -> bool {
        match (lhs.kind(), rhs.kind()) {
            (ObjKind::BIGNUM, ObjKind::BIGNUM) => lhs.as_bignum() == rhs.as_bignum(),
            (ObjKind::FLOAT, ObjKind::FLOAT) => lhs.as_float() == rhs.as_float(),
            (ObjKind::BYTES, ObjKind::BYTES) => lhs.as_bytes() == rhs.as_bytes(),
            (ObjKind::ARRAY, ObjKind::ARRAY) => {
                let lhs = lhs.as_array();
                let rhs = rhs.as_array();
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs.iter())
                        .all(|(lhs, rhs)| Value::eq(*lhs, *rhs))
            }
            (ObjKind::RANGE, ObjKind::RANGE) => lhs.as_range() == rhs.as_range(),
            (ObjKind::HASH, ObjKind::HASH) => {
                let lhs = lhs.as_hash();
                let rhs = rhs.as_hash();
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs.iter())
                        .all(|(lhs, rhs)| Value::eq(lhs.0, rhs.0) && Value::eq(lhs.1, rhs.1))
            }
            _ => false,
        }
    }
}

impl RValue {
    fn as_object(&self) -> &[Option<value::Value>; OBJECT_INLINE_IVAR] {
        unsafe { &self.kind.object }
    }

    fn as_object_mut(&mut self) -> &mut [Option<value::Value>; OBJECT_INLINE_IVAR] {
        unsafe { &mut self.kind.object }
    }

    pub(crate) fn as_class(&self) -> ClassId {
        unsafe { self.kind.class }
    }

    fn as_float(&self) -> f64 {
        unsafe { self.kind.float }
    }

    fn as_bignum(&self) -> &BigInt {
        unsafe { &self.kind.bignum }
    }

    pub(super) fn as_bytes(&self) -> &[u8] {
        unsafe { self.kind.string.0.as_ref() }
    }

    pub(super) fn as_string(&self) -> String {
        unsafe { String::from_utf8_lossy(&self.kind.string.0).to_string() }
    }

    /*pub(crate) fn as_string_mut(&mut self) -> &mut InnerVec {
        unsafe { &mut *self.kind.bytes }
    }*/

    pub(crate) fn as_array(&self) -> &ArrayInner {
        unsafe { &self.kind.array }
    }

    pub(super) fn as_array_mut(&mut self) -> &mut ArrayInner {
        unsafe { &mut self.kind.array }
    }

    pub(super) fn as_range(&self) -> &Range {
        unsafe { &self.kind.range }
    }

    pub(super) fn as_hash(&self) -> &HashInfo {
        unsafe { &self.kind.hash }
    }

    pub(super) fn as_hash_mut(&mut self) -> &mut HashInfo {
        unsafe { &mut self.kind.hash }
    }

    pub(super) fn as_proc(&self) -> &BlockData {
        unsafe { &self.kind.proc }
    }

    pub(crate) fn as_time(&self) -> &TimeInfo {
        unsafe { &self.kind.time }
    }

    /*pub(crate) fn as_time_mut(&mut self) -> &mut TimeInfo {
        unsafe { &mut *self.kind.time }
    }*/
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
    invalid: (),
    class: ClassId,
    object: [Option<Value>; OBJECT_INLINE_IVAR],
    bignum: ManuallyDrop<BigInt>,
    float: f64,
    string: ManuallyDrop<StringInner>,
    time: ManuallyDrop<TimeInfo>,
    array: ManuallyDrop<ArrayInner>,
    range: ManuallyDrop<Range>,
    proc: ManuallyDrop<BlockData>,
    hash: ManuallyDrop<HashInfo>,
}

#[allow(dead_code)]
impl ObjKind {
    pub const INVALID: u8 = 0;
    pub const CLASS: u8 = 1;
    pub const MODULE: u8 = 2;
    pub const OBJECT: u8 = 3;
    pub const BIGNUM: u8 = 4;
    pub const FLOAT: u8 = 5;
    pub const BYTES: u8 = 6;
    pub const TIME: u8 = 7;
    pub const ARRAY: u8 = 8;
    pub const RANGE: u8 = 9;
    pub const SPLAT: u8 = 10;
    pub const PROC: u8 = 11;
    pub const HASH: u8 = 12;
}

#[derive(Clone)]
#[repr(transparent)]
struct StringInner(SmallVec<[u8; STRING_INLINE_CAP]>);

#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct Range {
    pub start: Value,
    pub end: Value,
    pub exclude_end: u32,
}

impl std::hash::Hash for Range {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
        self.exclude_end.hash(state);
    }
}

impl Range {
    pub(crate) fn eql(&self, other: &Self) -> bool {
        self.start.eql(&other.start)
            && self.end.eql(&other.end)
            && self.exclude_end() == other.exclude_end()
    }

    pub fn exclude_end(&self) -> bool {
        self.exclude_end != 0
    }
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

    fn bytes(slice: &[u8]) -> Self {
        Self {
            string: ManuallyDrop::new(StringInner(SmallVec::from_slice(slice))),
        }
    }

    fn bytes_from_vec(vec: Vec<u8>) -> Self {
        Self {
            string: ManuallyDrop::new(StringInner(SmallVec::from_vec(vec))),
        }
    }

    fn array(ary: ArrayInner) -> Self {
        Self {
            array: ManuallyDrop::new(ary),
        }
    }

    fn range(start: Value, end: Value, exclude_end: bool) -> Self {
        Self {
            range: ManuallyDrop::new(Range {
                start,
                end,
                exclude_end: if exclude_end { 1 } else { 0 },
            }),
        }
    }

    fn hash(map: IndexMap<HashKey, Value>) -> Self {
        Self {
            hash: ManuallyDrop::new(HashInfo::new(map)),
        }
    }

    fn time(info: TimeInfo) -> Self {
        Self {
            time: ManuallyDrop::new(info),
        }
    }

    fn proc(block_data: BlockData) -> Self {
        Self {
            proc: ManuallyDrop::new(block_data),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInner(Vec<Value>);

impl std::ops::Deref for ArrayInner {
    type Target = [Value];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ArrayInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ArrayInner {
    pub fn new(v: Vec<Value>) -> Self {
        ArrayInner(v)
    }

    /*pub fn clear(&mut self) {
        self.0.clear();
    }*/

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    /*pub fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.0.truncate(new_len);
    }

    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.0.resize(new_len, value)
    }*/

    pub fn extend(&mut self, iter: impl std::iter::IntoIterator<Item = Value>) {
        self.0.extend(iter);
    }

    pub fn extend_from_slice(&mut self, slice: &[Value]) {
        self.0.extend_from_slice(slice);
    }

    /*pub fn drain(&mut self, range: std::ops::Range<usize>) -> Vec<Value> {
        self.0.drain(range).collect()
    }*/
}

impl ArrayInner {
    pub fn set_index(&mut self, globals: &mut Globals, idx: i64, src: Value) -> Option<Value> {
        if idx >= 0 {
            match self.get_mut(idx as usize) {
                Some(v) => *v = src,
                None => {
                    let idx = idx as usize;
                    self.extend((self.len()..idx).into_iter().map(|_| Value::nil()));
                    self.push(src);
                }
            }
        } else {
            let len = self.len();
            let idx_positive = len as i64 + idx;
            if idx_positive < 0 {
                globals.err_index_too_small(idx, -(len as i64));
                return None;
            } else {
                self[idx_positive as usize] = src;
            }
        };
        return Some(src);
    }

    pub fn get_index(&self, idx: i64) -> Option<Value> {
        return Some(if idx >= 0 {
            self.get(idx as usize).cloned().unwrap_or_default()
        } else {
            let idx = self.len() as i64 + idx;
            if idx < 0 {
                Value::nil()
            } else {
                self.get(idx as usize).cloned().unwrap_or_default()
            }
        });
    }
}
