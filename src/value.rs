use crate::*;
use num::{BigInt, ToPrimitive};
use ruruby_parse::{Node, NodeKind};

use crate::alloc::{Allocator, GC};

pub const NIL_VALUE: u64 = 0x04; // 0000_0100
pub const FALSE_VALUE: u64 = 0x14; // 0001_0100
pub const TRUE_VALUE: u64 = 0x1c; // 0001_1100
pub const TAG_SYMBOL: u64 = 0x0c; // 0000_1100

pub const FLOAT_ZERO: u64 = (0b1000 << 60) | 0b10;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value(std::num::NonZeroU64);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.unpack())
    }
}

impl std::default::Default for Value {
    fn default() -> Self {
        Value::nil()
    }
}

impl GC<RValue> for Value {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(rvalue) = self.try_rvalue() {
            rvalue.mark(alloc)
        }
    }
}

impl Value {
    /// This function is only used for system assertion.
    pub(crate) fn eq(lhs: Self, rhs: Self) -> bool {
        if lhs == rhs {
            return true;
        }
        match (lhs.try_rvalue(), rhs.try_rvalue()) {
            (Some(lhs), Some(rhs)) => match (lhs.kind(), rhs.kind()) {
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
                _ => false,
            },
            _ => false,
        }
    }
}

impl Value {
    pub(crate) fn from(id: u64) -> Self {
        Value(std::num::NonZeroU64::new(id).unwrap())
    }

    pub(crate) fn from_ptr(ptr: *mut RValue) -> Self {
        Value::from(ptr as u64)
    }

    pub(crate) fn class_id(&self) -> ClassId {
        if self.is_fixnum() {
            INTEGER_CLASS
        } else if self.is_flonum() {
            FLOAT_CLASS
        } else if let Some(rv) = self.try_rvalue() {
            rv.class()
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

    pub(crate) fn kind(&self) -> Option<u8> {
        self.try_rvalue().map(|rv| rv.kind())
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        if let Some(rv) = self.try_rvalue_mut() {
            rv.change_class(new_class_id);
        } else {
            unreachable!("the class of primitive class can not be changed.");
        }
    }

    pub(crate) fn get_singleton(self, globals: &mut Globals) -> Value {
        let original_id = self.as_class();
        let singleton = globals.get_singleton_id(original_id);
        globals.get_class_obj(singleton)
    }

    pub(crate) fn get_real_class_id(self, globals: &Globals) -> ClassId {
        globals.get_real_class_id(self)
    }

    pub(crate) fn get_real_class_name(self, globals: &Globals) -> String {
        globals.get_real_class_id(self).get_name(globals)
    }

    pub fn to_s(self, globals: &Globals) -> String {
        globals.val_tos(self)
    }

    pub(crate) fn to_bytes(self, globals: &Globals) -> Vec<u8> {
        globals.val_tobytes(self)
    }

    pub fn inspect(self, globals: &Globals) -> String {
        globals.val_inspect(self)
    }

    pub(crate) extern "C" fn get_class(val: Value) -> ClassId {
        val.class_id()
    }

    pub(crate) extern "C" fn deep_copy(val: Value) -> Self {
        if let Some(rv) = val.try_rvalue() {
            rv.deep_copy().pack()
        } else {
            val
        }
    }

    pub(crate) fn get(&self) -> u64 {
        self.0.get()
    }

    pub fn as_bool(&self) -> bool {
        self.get() & !0x10 != NIL_VALUE
    }

    pub const fn nil() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(NIL_VALUE) })
    }

    pub(crate) fn bool(b: bool) -> Self {
        if b {
            Value::from(TRUE_VALUE)
        } else {
            Value::from(FALSE_VALUE)
        }
    }

    pub(crate) fn fixnum(num: i64) -> Self {
        Value::from((num << 1) as u64 | 0b1)
    }

    fn is_i63(num: i64) -> bool {
        let top = (num as u64) >> 62 ^ (num as u64) >> 63;
        top & 0b1 == 0
    }

    pub(crate) fn int32(num: i32) -> Self {
        Value::fixnum(num as i64)
    }

    pub(crate) fn new_integer(num: i64) -> Self {
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

    pub(crate) fn new_bigint(bigint: BigInt) -> Self {
        if let Ok(i) = i64::try_from(&bigint) {
            Value::new_integer(i)
        } else {
            RValue::new_bigint(bigint).pack()
        }
    }

    pub(crate) fn new_empty_class(id: ClassId) -> Self {
        RValue::new_class(id).pack()
    }

    pub(crate) fn new_object(class_id: ClassId) -> Self {
        RValue::new_object(class_id).pack()
    }

    pub(crate) fn new_string(s: String) -> Self {
        RValue::new_string(s).pack()
    }

    pub(crate) fn new_string_from_str(b: &str) -> Self {
        RValue::new_bytes_from_slice(b.as_bytes()).pack()
    }

    pub(crate) fn new_string_from_slice(b: &[u8]) -> Self {
        RValue::new_bytes_from_slice(b).pack()
    }

    pub(crate) fn new_array(ary: ArrayInner) -> Self {
        RValue::new_array(ary).pack()
    }

    pub(crate) fn new_array_from_vec(v: Vec<Value>) -> Self {
        RValue::new_array(ArrayInner::new(v)).pack()
    }

    pub(crate) fn new_array_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue::new_array_with_class(v, class_id).pack()
    }

    pub(crate) fn new_symbol(id: IdentId) -> Self {
        Value::from((id.get() as u64) << 32 | TAG_SYMBOL)
    }

    pub(crate) fn new_range(start: Value, end: Value, exclude_end: bool) -> Self {
        RValue::new_range(start, end, exclude_end).pack()
    }

    pub(crate) fn new_time(time: TimeInfo) -> Self {
        RValue::new_time(time).pack()
    }

    pub(crate) fn unpack(&self) -> RV {
        if let Some(i) = self.try_fixnum() {
            RV::Integer(i)
        } else if let Some(f) = self.try_flonum() {
            RV::Float(f)
        } else if self.get() == 0 {
            RV::None
        } else if let Some(rv) = self.try_rvalue() {
            match rv.kind() {
                ObjKind::BIGNUM => RV::BigInt(rv.as_bignum()),
                ObjKind::FLOAT => RV::Float(rv.as_float()),
                ObjKind::BYTES => RV::String(rv.as_bytes()),
                _ => RV::Object(rv),
            }
        } else if self.is_packed_symbol() {
            RV::Symbol(self.as_packed_symbol())
        } else {
            match self.0.get() {
                NIL_VALUE => RV::Nil,
                TRUE_VALUE => RV::Bool(true),
                FALSE_VALUE => RV::Bool(false),
                _ => unreachable!("Illegal packed value. 0x{:016x}", self.0),
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

#[repr(C)]
pub struct ValTofResult {
    result: f64,
    code: u64,
}

impl Value {
    pub extern "C" fn val_tof(v: Value) -> ValTofResult {
        ValTofResult {
            result: match v.unpack() {
                RV::Integer(n) => n.to_f64().unwrap(),
                RV::BigInt(n) => n.to_f64().unwrap(),
                RV::Float(n) => n,
                _ => {
                    return ValTofResult {
                        result: 0.0,
                        code: 0,
                    }
                }
            },
            code: 1,
        }
    }

    pub(crate) fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    pub(crate) fn as_fixnum(&self) -> i64 {
        (self.0.get() as i64) >> 1
    }

    pub(crate) fn is_fixnum(&self) -> bool {
        self.0.get() & 0b1 == 1
    }

    pub(crate) fn is_flonum(&self) -> bool {
        self.0.get() & 0b11 == 2
    }

    pub(crate) fn try_fixnum(&self) -> Option<i64> {
        if self.is_fixnum() {
            Some(self.as_fixnum())
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

    pub(crate) fn try_flonum(&self) -> Option<f64> {
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
    /// return None if `self` was a packed value.
    pub(crate) fn try_rvalue(&self) -> Option<&RValue> {
        if self.is_packed_value() {
            None
        } else {
            Some(self.rvalue())
        }
    }

    /// Get mutable reference of RValue from `self`.
    ///
    /// return None if `self` was a packed value.
    pub(crate) fn try_rvalue_mut(&mut self) -> Option<&mut RValue> {
        if self.is_packed_value() {
            None
        } else {
            Some(self.rvalue_mut())
        }
    }

    pub(crate) fn rvalue(&self) -> &RValue {
        unsafe { &*(self.get() as *const RValue) }
    }

    pub(crate) fn rvalue_mut(&mut self) -> &mut RValue {
        unsafe { &mut *(self.get() as *mut RValue) }
    }

    /*pub(crate) fn is_symbol(&self) -> Option<IdentId> {
        match self.unpack() {
            RV::Symbol(sym) => Some(sym),
            _ => None,
        }
    }*/

    pub(crate) fn as_array(&self) -> &ArrayInner {
        assert_eq!(ObjKind::ARRAY, self.rvalue().kind());
        self.rvalue().as_array()
    }

    pub(crate) fn as_array_mut(&mut self) -> &mut ArrayInner {
        assert_eq!(ObjKind::ARRAY, self.rvalue().kind());
        self.rvalue_mut().as_array_mut()
    }

    pub(crate) fn is_array(&self) -> Option<&ArrayInner> {
        let rv = self.try_rvalue()?;
        match rv.kind() {
            ObjKind::ARRAY => Some(rv.as_array()),
            _ => None,
        }
    }

    pub(crate) fn is_class(&self) -> Option<ClassId> {
        let rv = self.try_rvalue()?;
        match rv.kind() {
            ObjKind::CLASS => Some(rv.as_class()),
            _ => None,
        }
    }

    pub(crate) fn as_class(&self) -> ClassId {
        assert_eq!(ObjKind::CLASS, self.rvalue().kind());
        self.rvalue().as_class()
    }

    pub(crate) fn expect_class(&self, name: IdentId, globals: &mut Globals) -> Option<ClassId> {
        match self.is_class() {
            Some(class) => Some(class),
            None => {
                globals.err_is_not_class(name);
                None
            }
        }
    }

    pub(crate) fn expect_symbol_or_string(&self, globals: &mut Globals) -> Option<IdentId> {
        match self.unpack() {
            RV::Symbol(sym) => return Some(sym),
            RV::String(s) => {
                let s = String::from_utf8_lossy(s).into_owned();
                return Some(IdentId::get_ident_id_from_string(s));
            }
            _ => {}
        }
        globals.err_is_not_symbol_nor_string(*self);
        None
    }

    pub(crate) fn expect_string(&self, globals: &mut Globals) -> Option<String> {
        match self.unpack() {
            RV::String(s) => {
                let s = String::from_utf8_lossy(s).into_owned();
                return Some(s);
            }
            _ => {}
        }
        globals.err_no_implicit_conversion(*self, STRING_CLASS);
        None
    }

    pub(crate) fn as_bytes(&self) -> &[u8] {
        assert_eq!(ObjKind::BYTES, self.rvalue().kind());
        self.rvalue().as_bytes()
    }

    pub(crate) fn as_string(&self) -> String {
        assert_eq!(ObjKind::BYTES, self.rvalue().kind());
        self.rvalue().as_string()
    }

    pub(crate) fn as_range(&self) -> &Range {
        assert_eq!(ObjKind::RANGE, self.rvalue().kind());
        self.rvalue().as_range()
    }
}

impl Value {
    #[cfg(test)]
    pub(crate) fn from_ast(node: &Node, globals: &mut Globals) -> Value {
        match &node.kind {
            NodeKind::CompStmt(stmts) => {
                assert_eq!(1, stmts.len());
                Self::from_ast(&stmts[0], globals)
            }
            NodeKind::Integer(num) => Value::new_integer(*num),
            NodeKind::Bignum(num) => Value::new_bigint(num.clone()),
            NodeKind::Float(num) => Value::new_float(*num),
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::new_symbol(IdentId::get_ident_id(sym)),
            NodeKind::String(s) => Value::new_string_from_str(s),
            NodeKind::Array(v, ..) => {
                let v = v.iter().map(|node| Self::from_ast(node, globals)).collect();
                Value::new_array_from_vec(v)
            }
            NodeKind::Const {
                toplevel,
                parent,
                prefix,
                name,
            } => {
                assert_eq!(false, *toplevel);
                assert_eq!(None, *parent);
                assert_eq!(0, prefix.len());
                let constant = IdentId::get_ident_id(name);
                globals.get_constant(OBJECT_CLASS, constant).unwrap()
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                ..
            } => {
                let start = Self::from_ast(start, globals);
                let end = Self::from_ast(end, globals);
                Value::new_range(start, end, *exclude_end)
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn from_ast2(node: &Node) -> Value {
        match &node.kind {
            NodeKind::Integer(num) => Value::new_integer(*num),
            NodeKind::Bignum(num) => Value::new_bigint(num.clone()),
            NodeKind::Float(num) => Value::new_float(*num),
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::new_symbol(IdentId::get_ident_id(sym)),
            NodeKind::String(s) => Value::new_string_from_str(s),
            NodeKind::Array(v, true) => {
                let v = v.iter().map(|node| Self::from_ast2(node)).collect();
                Value::new_array_from_vec(v)
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: true,
            } => {
                let start = Self::from_ast2(start);
                let end = Self::from_ast2(end);
                Value::new_range(start, end, *exclude_end)
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum RV<'a> {
    None,
    Nil,
    Bool(bool),
    Integer(i64),
    BigInt(&'a BigInt),
    Float(f64),
    Symbol(IdentId),
    String(&'a [u8]),
    Object(&'a RValue),
}

impl<'a> std::fmt::Debug for RV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV::None => write!(f, "Undef"),
            RV::Nil => write!(f, "nil"),
            RV::Bool(b) => write!(f, "{:?}", b),
            RV::Integer(n) => write!(f, "{}", n),
            RV::BigInt(n) => write!(f, "Bignum({})", n),
            RV::Float(n) => write!(f, "{}", dtoa::Buffer::new().format(*n),),
            RV::Symbol(id) => write!(f, "Symbol({})", id.get()),
            RV::String(s) => match String::from_utf8(s.to_vec()) {
                Ok(s) => write!(f, "\"{}\"", s),
                Err(_) => write!(f, "{:?}", s),
            },
            RV::Object(rvalue) => write!(f, "{:?}", rvalue),
        }
    }
}
