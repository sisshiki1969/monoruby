use crate::*;
use num::{BigInt, ToPrimitive};
use smallvec::SmallVec;

use crate::alloc::{Allocator, GC};

//const UNINITIALIZED: u64 = 0x24; // 0010_0100
pub const NIL_VALUE: u64 = 0x04; // 0000_0100
pub const FALSE_VALUE: u64 = 0x14; // 0001_0100
const TRUE_VALUE: u64 = 0x1c; // 0001_1100
pub const TAG_SYMBOL: u64 = 0x0c; // 0000_1100

const FLOAT_ZERO: u64 = (0b1000 << 60) | 0b10;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value(std::num::NonZeroU64);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.unpack())
    }
}

impl GC<RValue> for Value {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        match self.try_rvalue() {
            Some(rvalue) => rvalue.mark(alloc),
            None => {}
        }
    }
}

impl Value {
    pub fn eq(lhs: Self, rhs: Self) -> bool {
        if lhs == rhs {
            return true;
        }
        match (lhs.try_rvalue(), rhs.try_rvalue()) {
            (Some(lhs), Some(rhs)) => match (&lhs.kind, &rhs.kind) {
                (ObjKind::Bignum(lhs), ObjKind::Bignum(rhs)) => lhs == rhs,
                (ObjKind::Float(lhs), ObjKind::Float(rhs)) => lhs == rhs,
                (ObjKind::Bytes(lhs), ObjKind::Bytes(rhs)) => lhs == rhs,
                _ => false,
            },
            _ => false,
        }
    }
}

impl Value {
    pub fn from(id: u64) -> Self {
        Value(std::num::NonZeroU64::new(id).unwrap())
    }

    pub(crate) fn from_ptr(ptr: *mut RValue) -> Self {
        Value::from(ptr as u64)
    }

    pub fn class_id(&self) -> ClassId {
        if self.is_fixnum() {
            INTEGER_CLASS
        } else if self.is_flonum() {
            FLOAT_CLASS
        } else if !self.is_packed_value() {
            self.rvalue().class()
        } else if self.is_packed_symbol() {
            SYMBOL_CLASS
        } else {
            match self.0.get() {
                NIL_VALUE => NIL_CLASS,
                TRUE_VALUE => TRUE_CLASS,
                FALSE_VALUE => FALSE_CLASS,
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }

    pub fn change_class(&mut self, new_class_id: ClassId) {
        if !self.is_packed_value() {
            self.rvalue_mut().change_class(new_class_id);
        } else {
            unreachable!("the class of primitive class can not be changed.");
        }
    }

    pub fn get_singleton(self, globals: &mut Globals) -> Value {
        let original_id = self.as_class();
        let singleton = globals.get_singleton_id(original_id);
        globals.get_class_obj(singleton)
    }

    pub fn get_real_class_obj(self, globals: &Globals) -> Value {
        globals.get_real_class_obj(self)
    }

    pub fn to_s(self, globals: &Globals) -> String {
        globals.val_tos(self)
    }

    pub fn to_bytes(self, globals: &Globals) -> Vec<u8> {
        globals.val_tobytes(self)
    }

    pub fn inspect(self, globals: &Globals) -> String {
        globals.val_inspect(self)
    }

    pub(crate) extern "C" fn get_class(val: Value) -> ClassId {
        val.class_id()
    }

    pub(crate) extern "C" fn dup(val: Value) -> Self {
        if val.is_packed_value() {
            val
        } else {
            let rval = val.rvalue().clone();
            rval.pack()
        }
    }

    /*#[inline(always)]
    pub fn from_unchecked(id: u64) -> Self {
        unsafe { Value(std::num::NonZeroU64::new_unchecked(id)) }
    }*/

    pub fn get(&self) -> u64 {
        self.0.get()
    }

    pub const fn nil() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(NIL_VALUE) })
    }

    /*#[inline(always)]
    const fn true_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(TRUE_VALUE) })
    }

    #[inline(always)]
    const fn false_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(FALSE_VALUE) })
    }*/

    pub fn bool(b: bool) -> Self {
        if b {
            Value::from(TRUE_VALUE)
        } else {
            Value::from(FALSE_VALUE)
        }
    }

    pub fn fixnum(num: i64) -> Self {
        Value::from((num << 1) as u64 | 0b1)
    }

    fn is_i63(num: i64) -> bool {
        let top = (num as u64) >> 62 ^ (num as u64) >> 63;
        top & 0b1 == 0
    }

    pub fn int32(num: i32) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn new_integer(num: i64) -> Self {
        if Self::is_i63(num) {
            Value::fixnum(num)
        } else {
            RValue::new_bigint(BigInt::from(num)).pack()
        }
    }

    pub extern "C" fn new_float(num: f64) -> Self {
        if num == 0.0 {
            return Value::from(FLOAT_ZERO);
        }
        let unum = f64::to_bits(num);
        let exp = ((unum >> 60) & 0b111) + 1;
        if (exp & 0b0110) == 0b0100 {
            Value::from(((unum.rotate_left(3)) & !1) | 2)
        } else {
            RValue::new_float(num).pack()
        }
    }

    pub fn new_bigint(bigint: BigInt) -> Self {
        if let Ok(i) = i64::try_from(&bigint) {
            Value::new_integer(i)
        } else {
            RValue::new_bigint(bigint).pack()
        }
    }

    pub fn new_empty_class(id: ClassId) -> Self {
        RValue::new_class(id).pack()
    }

    pub fn new_string(b: Vec<u8>) -> Self {
        RValue::new_bytes(b).pack()
    }

    pub fn new_string_from_smallvec(b: SmallVec<[u8; 31]>) -> Self {
        RValue::new_bytes_from_smallvec(b).pack()
    }

    pub fn new_string_from_slice(b: &[u8]) -> Self {
        RValue::new_bytes_from_slice(b).pack()
    }

    pub fn new_symbol(id: IdentId) -> Self {
        Value::from((id.get() as u64) << 32 | TAG_SYMBOL)
    }

    pub fn new_time(time: TimeInfo) -> Self {
        RValue::new_time(time).pack()
    }

    pub fn unpack(&self) -> RV {
        if let Some(i) = self.try_fixnum() {
            RV::Integer(i)
        } else if let Some(f) = self.try_flonum() {
            RV::Float(f)
        } else if !self.is_packed_value() {
            let rvalue = self.rvalue();
            match &rvalue.kind {
                ObjKind::Bignum(num) => RV::BigInt(num),
                ObjKind::Float(num) => RV::Float(*num),
                ObjKind::Bytes(b) => RV::String(b),
                _ => RV::Object(rvalue),
            }
        } else if self.is_packed_symbol() {
            RV::Symbol(self.as_packed_symbol())
        } else {
            match self.0.get() {
                NIL_VALUE => RV::Nil,
                TRUE_VALUE => RV::Bool(true),
                FALSE_VALUE => RV::Bool(false),
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct F2 {
    f1: f64,
    f2: f64,
}

impl Value {
    /*fn is_nil(&self) -> bool {
        self.0.get() == NIL_VALUE
    }*/

    /*pub fn to_bool(&self) -> bool {
        let v = self.0.get();
        (v | 0x10) != 0x14
    }*/

    pub extern "C" fn val_tof(v: Value) -> f64 {
        match v.unpack() {
            RV::Integer(n) => n.to_f64().unwrap(),
            RV::BigInt(n) => n.to_f64().unwrap(),
            RV::Float(n) => n,
            _ => unimplemented!("to_f is not implemented for {:?}", v),
        }
    }

    pub extern "C" fn binary_flonum(lhs: Value, rhs: Value) -> F2 {
        F2 {
            f1: lhs.as_flonum(),
            f2: rhs.as_flonum(),
        }
    }

    pub fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    pub fn as_fixnnum(&self) -> i64 {
        (self.0.get() as i64) >> 1
    }

    pub fn is_fixnum(&self) -> bool {
        self.0.get() & 0b1 == 1
    }

    pub fn is_flonum(&self) -> bool {
        self.0.get() & 0b11 == 2
    }

    pub fn try_fixnum(&self) -> Option<i64> {
        if self.is_fixnum() {
            Some(self.as_fixnnum())
        } else {
            None
        }
    }

    fn as_flonum(&self) -> f64 {
        let u = self.0.get();
        if u == FLOAT_ZERO {
            return 0.0;
        }
        let bit = 0b10 - ((u >> 63) & 0b1);
        let num = ((u & !3) | bit).rotate_right(3);
        f64::from_bits(num)
    }

    fn try_flonum(&self) -> Option<f64> {
        let u = self.0.get();
        if u & 0b11 == 2 {
            Some(Self::as_flonum(self))
        } else {
            None
        }
    }

    fn is_packed_symbol(&self) -> bool {
        self.get() & 0xff == TAG_SYMBOL
    }

    fn as_packed_symbol(&self) -> IdentId {
        IdentId::from((self.get() >> 32) as u32)
    }

    /// Get reference of RValue from `self`.
    ///
    /// return None if `self` was not a packed value.
    pub(crate) fn try_rvalue(&self) -> Option<&RValue> {
        if self.is_packed_value() {
            None
        } else {
            Some(self.rvalue())
        }
    }

    pub(crate) fn rvalue(&self) -> &RValue {
        unsafe { &*(self.get() as *const RValue) }
    }

    pub(crate) fn rvalue_mut(&self) -> &mut RValue {
        unsafe { &mut *(self.get() as *mut RValue) }
    }

    pub(crate) fn as_class(&self) -> ClassId {
        match self.unpack() {
            RV::Object(rv) => match rv.kind {
                ObjKind::Class(id) => id,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_string(&self) -> &SmallVec<[u8; 31]> {
        match self.unpack() {
            RV::String(b) => b,
            _ => unreachable!(),
        }
    }

    /*#[inline(always)]
    fn is_packed_num(&self) -> bool {
        self.0.get() & 0b11 != 0
    }*/
}

#[derive(Clone, PartialEq)]
pub enum RV<'a> {
    Nil,
    Bool(bool),
    Integer(i64),
    BigInt(&'a BigInt),
    Float(f64),
    Symbol(IdentId),
    String(&'a SmallVec<[u8; 31]>),
    Object(&'a RValue),
}

impl<'a> std::fmt::Debug for RV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV::Nil => write!(f, "nil"),
            RV::Bool(b) => write!(f, "{:?}", b),
            RV::Integer(n) => write!(f, "Integer({})", n),
            RV::BigInt(n) => write!(f, "Bignum({})", n),
            RV::Float(n) => write!(f, "{}", n),
            RV::Symbol(id) => write!(f, "Symbol({})", id.get()),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => write!(f, "\"{}\"", s),
                Err(_) => write!(f, "{:?}", s),
            },
            RV::Object(rvalue) => write!(f, "{:?}", rvalue),
        }
    }
}
