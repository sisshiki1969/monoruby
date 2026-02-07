use num::ToPrimitive;
use onigmo_regex::Captures;
use rubymap::RubyEql;
use std::{
    collections::HashSet,
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Deref,
};

use super::*;
use crate::{
    alloc::{Allocator, GC},
    builtins::TimeInner,
};
use num::{BigInt, FromPrimitive};
use ruruby_parse::{Node, NodeKind};

pub mod numeric;
pub mod rvalue;

pub use numeric::*;
pub use rvalue::*;

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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.unpack())
    }
}

impl std::default::Default for Value {
    fn default() -> Self {
        Value::nil()
    }
}

impl From<Immediate> for Value {
    fn from(imm: Immediate) -> Self {
        Value(imm.0)
    }
}

impl From<RubySymbol> for Value {
    fn from(sym: RubySymbol) -> Self {
        Value(sym.0)
    }
}

impl std::borrow::Borrow<RubySymbol> for Value {
    fn borrow(&self) -> &RubySymbol {
        // SAFETY: If self is a packed value, it is safe to interpret its bits as RubySymbol.
        // If self is not a packed value, this function should not be called.
        assert!(self.is_symbol());
        unsafe { &*(self as *const Value as *const RubySymbol) }
    }
}

impl GC<RValue> for Value {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(rvalue) = self.try_rvalue() {
            rvalue.mark(alloc)
        }
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for Value {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<()> {
        match self.try_rvalue() {
            None => self.0.hash(state),
            // SAFETY: The RValue pointer is valid and the type checking via ty()
            // ensures we're accessing the correct variant of the union.
            Some(lhs) => unsafe {
                match lhs.ty() {
                    //ObjTy::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", lhs),
                    ObjTy::BIGNUM => lhs.as_bignum().hash(state),
                    ObjTy::FLOAT => lhs.as_float().to_bits().hash(state),
                    ObjTy::STRING => lhs.as_rstring().hash(state),
                    ObjTy::ARRAY => lhs.as_array().ruby_hash(state, e, g)?,
                    ObjTy::RANGE => lhs.as_range().ruby_hash(state, e, g)?,
                    ObjTy::HASH => lhs.as_hashmap().ruby_hash(state, e, g)?,
                    //ObjTy::METHOD => lhs.method().hash(state),
                    _ => {
                        e.invoke_method_inner(g, IdentId::HASH, *self, &[], None, None)?
                            .0
                            .hash(state);
                    }
                }
            },
        }
        Ok(())
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for Value {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        if self.id() == other.id() {
            return Ok(true);
        }
        match (self.try_rvalue(), other.try_rvalue()) {
            (None, None) => Ok(self.id() == other.id()),
            (Some(lhs), Some(rhs)) => {
                // SAFETY: Both lhs and rhs are valid RValue pointers. Type checking via ty()
                // ensures we're accessing the correct variants of their unions.
                unsafe {
                    Ok(match (lhs.ty(), rhs.ty()) {
                        (ObjTy::BIGNUM, ObjTy::BIGNUM) => lhs.as_bignum() == rhs.as_bignum(),
                        (ObjTy::FLOAT, ObjTy::FLOAT) => lhs.as_float() == rhs.as_float(),
                        (ObjTy::COMPLEX, ObjTy::COMPLEX) => {
                            lhs.as_complex().eql(rhs.as_complex(), vm, globals)?
                        }
                        (ObjTy::STRING, ObjTy::STRING) => lhs.as_rstring() == rhs.as_rstring(),
                        (ObjTy::ARRAY, ObjTy::ARRAY) => {
                            let lhs = lhs.as_array();
                            let rhs = rhs.as_array();
                            if lhs.len() != rhs.len() {
                                return Ok(false);
                            }
                            for (a1, a2) in lhs.iter().zip(rhs.iter()) {
                                // Support self-containing arrays.
                                if self.id() == a1.id() && other.id() == a2.id() {
                                } else if self.id() == a1.id() || other.id() == a2.id() {
                                    return Ok(false);
                                } else {
                                    if !a1.eql(a2, vm, globals)? {
                                        return Ok(false);
                                    }
                                }
                            }
                            true
                        }
                        (ObjTy::RANGE, ObjTy::RANGE) => {
                            lhs.as_range().eql(rhs.as_range(), vm, globals)?
                        }
                        (ObjTy::HASH, ObjTy::HASH) => {
                            lhs.as_hashmap().eql(rhs.as_hashmap(), vm, globals)?
                        }
                        //(ObjTy::METHOD, ObjTy::METHOD) => *self.method() == *other.method(),
                        //(ObjTy::UNBOUND_METHOD, ObjTy::UNBOUND_METHOD) => *self.method() == *other.method(),
                        //(ObjTy::INVALID, _) => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
                        //(_, ObjTy::INVALID) => panic!("Invalid rvalue. (maybe GC problem) {:?}", other),
                        _ => vm
                            .invoke_method_inner(
                                globals,
                                IdentId::EQL_,
                                *self,
                                &[*other],
                                None,
                                None,
                            )?
                            .as_bool(),
                    })
                }
            }
            _ => Ok(false),
        }
    }
}

impl Value {
    pub fn calculate_hash(self, e: &mut Executor, g: &mut Globals) -> Result<u64> {
        let mut s = std::hash::DefaultHasher::new();
        RubyHash::ruby_hash(&self, &mut s, e, g)?;
        Ok(s.finish())
    }
}

impl Value {
    /// This function is only used for system assertion.
    pub(crate) fn assert_eq(store: &Store, lhs: Self, rhs: Self) {
        if lhs == rhs {
            return;
        }
        match (lhs.try_rvalue(), rhs.try_rvalue()) {
            (Some(lhs), Some(rhs)) => {
                if RValue::eq(store, lhs, rhs) {
                    return;
                }
            }
            _ => {}
        }
        panic!("{} != {}", lhs.inspect(store), rhs.inspect(store))
    }
}

impl Value {
    pub(crate) fn from_u64(id: u64) -> Self {
        Value(std::num::NonZeroU64::new(id).unwrap())
    }

    fn from_ptr(ptr: *mut RValue) -> Self {
        Value::from_u64(ptr as u64)
    }

    pub(crate) fn class(&self) -> ClassId {
        if self.is_fixnum() {
            INTEGER_CLASS
        } else if self.is_flonum() {
            FLOAT_CLASS
        } else if let Some(rv) = self.try_rvalue() {
            rv.class()
        } else if self.is_symbol() {
            SYMBOL_CLASS
        } else {
            match self.0.get() {
                NIL_VALUE => NIL_CLASS,
                TRUE_VALUE => TRUE_CLASS,
                FALSE_VALUE => FALSE_CLASS,
                _ => unreachable!("Illegal packed value. 0x{:016x}", self.0),
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn debug_class(&self) -> Option<ClassId> {
        let class = if self.is_fixnum() {
            INTEGER_CLASS
        } else if self.is_flonum() {
            FLOAT_CLASS
        } else if let Some(rv) = self.try_rvalue() {
            rv.debug_class()?
        } else if self.is_symbol() {
            SYMBOL_CLASS
        } else {
            match self.0.get() {
                NIL_VALUE => NIL_CLASS,
                TRUE_VALUE => TRUE_CLASS,
                FALSE_VALUE => FALSE_CLASS,
                _ => return None,
            }
        };
        Some(class)
    }

    pub(crate) fn ty(&self) -> Option<ObjTy> {
        self.try_rvalue().map(|rv| rv.ty())
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        if let Some(rv) = self.try_rvalue_mut() {
            rv.change_class(new_class_id);
        } else {
            unreachable!("the class of primitive class can not be changed.");
        }
    }

    pub(crate) fn get_singleton(self, store: &mut Store) -> Value {
        if let Some(class) = self.is_class_or_module() {
            store.get_metaclass(class.id())
        } else {
            store.get_singleton(self)
        }
        .as_val()
    }

    ///
    /// Get class object of *self.
    ///
    pub(crate) fn get_class_obj(self, store: &Store) -> Module {
        store[self.class()].get_module()
    }

    pub(crate) fn real_class(self, store: &Store) -> Module {
        self.get_class_obj(store).get_real_class()
    }

    pub(crate) fn get_real_class_name(self, store: &Store) -> String {
        store.get_class_name(self.real_class(store).id())
    }

    pub(crate) fn is_kind_of(self, store: &Store, class: ClassId) -> bool {
        let mut obj_class = Some(self.get_class_obj(store));
        while let Some(obj_class_inner) = obj_class {
            if obj_class_inner.id() == class {
                return true;
            }
            obj_class = obj_class_inner.superclass();
        }
        false
    }

    pub fn is_nil(&self) -> bool {
        self.id() == NIL_VALUE
    }

    pub(crate) fn deep_copy(&self) -> Self {
        if let Some(rv) = self.try_rvalue() {
            rv.deep_copy().pack()
        } else {
            *self
        }
    }

    pub(crate) extern "C" fn value_deep_copy(val: Value) -> Self {
        val.deep_copy()
    }

    pub(crate) fn dup(&self) -> Self {
        if let Some(rv) = self.try_rvalue() {
            rv.dup().pack()
        } else {
            *self
        }
    }

    pub(crate) fn id(&self) -> u64 {
        self.0.get()
    }

    pub fn as_bool(&self) -> bool {
        self.id() & !0x10 != NIL_VALUE
    }

    pub fn set_instance_var(&mut self, store: &mut Store, name: &str, val: Value) -> Result<()> {
        store.set_ivar(*self, IdentId::get_id(name), val)
    }
}

//
// constructors
//
impl Value {
    pub fn nil() -> Self {
        Immediate::nil().into()
    }

    pub fn bool(b: bool) -> Self {
        Immediate::bool(b).into()
    }

    pub fn symbol(id: IdentId) -> Self {
        Immediate::from_u64((id.get() as u64) << 32 | TAG_SYMBOL).into()
    }

    pub fn symbol_from_str(s: &str) -> Self {
        let id = IdentId::get_id(s);
        Value::symbol(id)
    }

    fn fixnum(num: i64) -> Self {
        Immediate::fixnum(num).into()
    }

    pub fn flonum(num: f64) -> Option<Self> {
        Immediate::flonum(num).map(|imm| imm.into())
    }

    fn is_i63(num: i64) -> bool {
        let top = ((num as u64) >> 62) ^ ((num as u64) >> 63);
        top & 0b1 == 0
    }

    pub(crate) fn from_ord(ord: std::cmp::Ordering) -> Self {
        Value::i32(ord as i32)
    }

    pub fn check_fixnum(i: i64) -> Option<Value> {
        if Self::is_i63(i) {
            Some(Value::fixnum(i))
        } else {
            None
        }
    }

    pub fn i32(num: i32) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn integer(num: i64) -> Self {
        if Self::is_i63(num) {
            Value::fixnum(num)
        } else {
            RValue::new_bigint(BigInt::from(num)).pack()
        }
    }

    pub fn integer_from_u64(num: u64) -> Self {
        if let Ok(i) = i64::try_from(num) {
            Value::integer(i)
        } else {
            Value::bigint(BigInt::from(num))
        }
    }

    const INTEGRAL_LIMIT: f64 = 9007199254740992.0;

    pub fn integer_from_f64(f: f64) -> Option<Value> {
        if f.is_nan() || f.is_infinite() {
            return None;
        }

        let f = f.round();
        if f < -Self::INTEGRAL_LIMIT || f > Self::INTEGRAL_LIMIT {
            return Some(Value::bigint(BigInt::from_f64(f)?));
        }

        Some(Value::integer(f as i64))
    }

    pub fn float(num: f64) -> Self {
        if let Some(v) = Self::flonum(num) {
            v
        } else {
            RValue::new_float(num).pack()
        }
    }

    pub extern "C" fn float_heap(num: f64) -> Value {
        RValue::new_float(num).pack()
    }

    pub fn complex(re: impl Into<Real>, im: impl Into<Real>) -> Self {
        let re = re.into();
        let im = im.into();
        RValue::new_complex(re, im).pack()
    }

    pub fn complex_from(complex: num::complex::Complex<Real>) -> Self {
        RValue::new_complex_from(complex).pack()
    }

    pub fn bigint(bigint: BigInt) -> Self {
        if let Ok(i) = i64::try_from(&bigint) {
            Value::integer(i)
        } else {
            RValue::new_bigint(bigint).pack()
        }
    }

    pub(crate) fn class_empty(id: ClassId, superclass: Option<Module>) -> Self {
        RValue::new_class(id, superclass, ModuleType::RealClass).pack()
    }

    pub(crate) fn singleton_class_empty(
        id: ClassId,
        superclass: Option<Module>,
        attach_obj: Value,
    ) -> Self {
        RValue::new_singleton_class(id, superclass, attach_obj).pack()
    }

    pub(crate) fn module_empty(id: ClassId, superclass: Option<Module>) -> Self {
        RValue::new_module(id, superclass).pack()
    }

    pub(crate) fn iclass(id: ClassId, superclass: Option<Module>) -> Self {
        RValue::new_iclass(id, superclass).pack()
    }

    pub fn object(class_id: ClassId) -> Self {
        RValue::new_object(class_id).pack()
    }

    pub fn yielder_object() -> Self {
        // SAFETY: YIELDER is a static variable that is properly initialized before use.
        // Access is synchronized through single-threaded Ruby VM execution.
        Value::object(unsafe { crate::builtins::YIELDER.unwrap().id() })
    }

    pub fn string(s: String) -> Self {
        RValue::new_string(s).pack()
    }

    pub fn bytes(s: Vec<u8>) -> Self {
        RValue::new_bytes(s).pack()
    }

    pub fn string_from_str(s: &str) -> Self {
        RValue::new_string_from_str(s).pack()
    }

    pub fn bytes_from_slice(b: &[u8]) -> Self {
        RValue::new_bytes_from_slice(b).pack()
    }

    pub fn string_from_vec(b: Vec<u8>) -> Self {
        RValue::new_string_from_vec(b).pack()
    }

    pub fn string_from_inner(inner: RStringInner) -> Self {
        RValue::new_string_from_inner(inner).pack()
    }

    pub fn array(ary: ArrayInner) -> Self {
        RValue::new_array(ary).pack()
    }

    pub fn array_with_class(ary: ArrayInner, class_id: ClassId) -> Self {
        RValue::new_array_with_class(ary, class_id).pack()
    }

    pub fn array1(elem: Value) -> Self {
        Value::array(ArrayInner::from(smallvec::smallvec![elem]))
    }

    pub fn array2(v1: Value, v2: Value) -> Self {
        Value::array(ArrayInner::from(smallvec::smallvec![v1, v2]))
    }

    pub fn array_empty() -> Self {
        Value::array(ArrayInner::new())
    }

    pub fn array_empty_with_class(class_id: ClassId) -> Self {
        RValue::new_array_with_class(ArrayInner::new(), class_id).pack()
    }

    pub fn array_with_capacity(len: usize) -> Self {
        Value::array(ArrayInner::with_capacity(len))
    }

    pub fn array_from_vec(v: Vec<Value>) -> Self {
        Value::array(ArrayInner::from_vec(v))
    }

    pub fn array_from_iter(iter: impl Iterator<Item = Value>) -> Self {
        Value::array(ArrayInner::from_iter(iter))
    }

    pub fn array_from_vec_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue::new_array_from_vec_with_class(v, class_id).pack()
    }

    pub fn hash(map: RubyMap<Value, Value>) -> Self {
        RValue::new_hash(map).pack()
    }

    pub fn hash_from_inner(inner: HashmapInner) -> Self {
        RValue::new_hash_from_inner(inner).pack()
    }

    pub fn hash_with_class_and_default_proc(class_id: ClassId, default_proc: Proc) -> Self {
        RValue::new_hash_with_class_and_default_proc(class_id, default_proc).pack()
    }

    pub fn hash_with_class_and_default(class_id: ClassId, default: Value) -> Self {
        RValue::new_hash_with_class_and_default(class_id, default).pack()
    }

    pub fn regexp(regexp: RegexpInner) -> Self {
        RValue::new_regexp(regexp).pack()
    }

    pub(crate) fn new_io_stdin() -> Self {
        RValue::new_io_stdin().pack()
    }

    pub(crate) fn new_io_stdout() -> Self {
        RValue::new_io_stdout().pack()
    }

    pub(crate) fn new_io_stderr() -> Self {
        RValue::new_io_stderr().pack()
    }

    pub(crate) fn new_file(file: std::fs::File, name: String) -> Self {
        RValue::new_file(file, name).pack()
    }

    pub fn range(start: Value, end: Value, exclude_end: bool) -> Self {
        RValue::new_range(start, end, exclude_end).pack()
    }

    pub fn new_exception(err: MonorubyErr) -> Self {
        RValue::new_exception(err).pack()
    }

    pub fn new_exception_from(message: String, class_id: ClassId) -> Self {
        Value::new_exception(MonorubyErr::new(
            MonorubyErrKind::from_class_id(class_id),
            message,
        ))
    }

    pub fn new_time(time: TimeInner) -> Self {
        RValue::new_time(time).pack()
    }

    pub(in crate::value) fn new_proc(block: ProcInner) -> Self {
        RValue::new_proc(block).pack()
    }

    pub fn new_method(receiver: Value, func_id: FuncId, owner: ClassId) -> Self {
        RValue::new_method(receiver, func_id, owner).pack()
    }

    pub fn new_unbound_method(func_id: FuncId, owner: ClassId) -> Self {
        RValue::new_unbound_method(func_id, owner).pack()
    }

    pub(crate) fn new_fiber(proc: Proc) -> Self {
        RValue::new_fiber(proc).pack()
    }

    pub(crate) fn new_enumerator(
        obj: Value,
        method: IdentId,
        proc: Proc,
        args: Vec<Value>,
    ) -> Self {
        RValue::new_enumerator(obj, method, proc, args).pack()
    }

    pub(crate) fn new_generator(proc: Proc) -> Self {
        RValue::new_generator(proc).pack()
    }

    pub(crate) fn new_binding(outer_lfp: Lfp) -> Self {
        RValue::new_binding(outer_lfp).pack()
    }

    pub(crate) fn new_matchdata(captures: Captures, heystack: &str, regex: Regexp) -> Self {
        RValue::new_match_data(captures, heystack, regex).pack()
    }

    pub(crate) fn unpack(&self) -> RV<'_> {
        if let Some(i) = self.try_fixnum() {
            RV::Fixnum(i)
        } else if let Some(f) = self.try_flonum() {
            RV::Float(f)
        } else if self.id() == 0 {
            RV::Invalid
        } else if let Some(rv) = self.try_rvalue() {
            rv.unpack()
        } else if self.is_symbol() {
            RV::Symbol(self.as_symbol())
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

// display

impl Value {
    ///
    /// Inspection for debugging.
    ///
    pub fn debug(&self, store: &Store) -> String {
        match self.unpack() {
            RV::Invalid => "UNDEFINED".to_string(),
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Fixnum(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Complex(_) => self.as_complex().debug(store),
            RV::Symbol(id) => format!(":{id}"),
            RV::String(s) => format!(r#""{}""#, s.inspect()),
            RV::Object(rvalue) => rvalue.debug(store),
        }
    }

    pub fn debug_check(&self, store: &Store) -> Option<String> {
        let s = match self.unpack() {
            RV::Invalid => return None,
            RV::Nil => "nil".to_string(),
            RV::Bool(b) => format!("{:?}", b),
            RV::Fixnum(n) => format!("{}", n),
            RV::BigInt(n) => format!("{}", n),
            RV::Float(f) => dtoa::Buffer::new().format(f).to_string(),
            RV::Complex(_) => self.as_complex().debug(store),
            RV::Symbol(id) => format!(":{id}"),
            RV::String(s) => format!(r#""{}""#, s.inspect()),
            RV::Object(rvalue) => rvalue.debug(store),
        };
        Some(s)
    }

    pub fn debug_tos(&self, store: &Store) -> String {
        let s = match self.unpack() {
            RV::Nil => "".to_string(),
            RV::Symbol(id) => id.to_string(),
            RV::String(s) => s.to_str().unwrap().to_string(),
            _ => self.debug(store),
        };
        s
    }

    pub fn to_s(&self, store: &Store) -> String {
        let s = match self.unpack() {
            RV::Object(rvalue) => rvalue.to_s(store),
            _ => self.debug_tos(store),
        };
        s
    }

    pub fn inspect(&self, store: &Store) -> String {
        let mut set = HashSet::new();
        self.inspect_inner(store, &mut set)
    }

    fn inspect_inner(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        if !set.insert(self.id()) {
            return "...".to_string();
        }
        let s = match self.unpack() {
            RV::Object(rvalue) => rvalue.inspect(store, set),
            _ => self.debug(store),
        };
        s
    }
}

fn search_method(
    vm: &mut Executor,
    globals: &mut Globals,
    methods: &[IdentId],
    recv: Value,
) -> Result<RString> {
    if let Some(s) = recv.is_rstring() {
        return Ok(s);
    }
    for &method in methods {
        if let Some(func_id) = globals.check_method(recv, method) {
            let v = vm.invoke_func_inner(globals, func_id, recv, &[], None, None)?;
            if let Some(s) = v.is_rstring() {
                return Ok(s);
            }
            break;
        }
    }
    Err(MonorubyErr::no_implicit_conversion(
        globals,
        recv,
        STRING_CLASS,
    ))
}

impl Value {
    ///
    /// Check if `self` is a packed value.
    ///
    pub fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    ///
    /// Get a reference to RValue from `self`.
    ///
    /// return None if `self` was a packed value.
    ///
    pub(crate) fn try_rvalue(&self) -> Option<&RValue> {
        if self.is_packed_value() {
            None
        } else {
            // SAFETY: Non-packed values contain a valid pointer to an RValue
            // allocated by the GC allocator. The pointer remains valid as long
            // as the Value is live and not collected.
            Some(unsafe { &*(self.id() as *const RValue) })
        }
    }

    ///
    /// Get a mutable reference to RValue from `self`.
    ///
    /// return None if `self` was a packed value.
    ///
    pub(crate) fn try_rvalue_mut(&mut self) -> Option<&mut RValue> {
        if self.is_packed_value() {
            None
        } else {
            // SAFETY: Non-packed values contain a valid pointer to an RValue
            // allocated by the GC allocator. The pointer remains valid and we have
            // exclusive mutable access through &mut self.
            Some(unsafe { &mut *(self.id() as *mut RValue) })
        }
    }

    ///
    /// Get a reference to RValue from `self`.
    ///
    /// ### Panics
    /// Panics if `self` was a packed value.
    ///
    pub(crate) fn rvalue(&self) -> &RValue {
        self.try_rvalue().unwrap()
    }

    ///
    /// Get a mutable reference to RValue from `self`.
    ///
    /// ### Panics
    /// Panics if `self` was a packed value.
    ///
    pub(crate) fn rvalue_mut(&mut self) -> &mut RValue {
        self.try_rvalue_mut().unwrap()
    }

    ///
    /// Check if `self` is a fixnum.
    ///
    pub(crate) fn is_fixnum(&self) -> bool {
        self.0.get() & 0b1 == 1
    }

    ///
    /// Check if `self` is a fixnum.
    ///
    /// If `self` is a fixnum, return it as i64.
    /// Otherwise, return None.
    ///
    pub fn try_fixnum(&self) -> Option<i64> {
        if self.is_fixnum() {
            Some((self.0.get() as i64) >> 1)
        } else {
            None
        }
    }

    pub(crate) fn try_symbol_or_string(&self) -> Option<IdentId> {
        if let Some(sym) = self.try_symbol() {
            Some(sym)
        } else if let Some(s) = self.is_str() {
            Some(IdentId::get_id(s))
        } else {
            None
        }
    }

    ///
    /// Check if `self` is a flonum.
    ///
    fn is_flonum(&self) -> bool {
        self.0.get() & 0b11 == 2
    }

    ///
    /// Check if `self` is a Float.
    ///
    pub(crate) fn is_float(&self) -> bool {
        if self.is_flonum() {
            true
        } else if let Some(rv) = self.try_rvalue() {
            rv.class() == FLOAT_CLASS
        } else {
            false
        }
    }

    fn try_flonum(&self) -> Option<f64> {
        fn as_flonum(v: &Value) -> f64 {
            let u = v.0.get();
            if u == FLOAT_ZERO {
                return 0.0;
            }
            let bit = 0b10 - ((u >> 63) & 0b1);
            let num = ((u & !3) | bit).rotate_right(3);
            f64::from_bits(num)
        }
        if self.is_flonum() {
            Some(as_flonum(self))
        } else {
            None
        }
    }

    pub fn try_float(&self) -> Option<f64> {
        if let Some(f) = self.try_flonum() {
            return Some(f);
        } else if let Some(rv) = self.try_rvalue() {
            if rv.ty() == ObjTy::FLOAT {
                // SAFETY: The type check ensures this RValue contains a float,
                // so accessing it via as_float() is safe.
                return Some(unsafe { rv.as_float() });
            }
        }
        None
    }

    pub fn try_complex(&self) -> Option<&ComplexInner> {
        if self.ty()? == ObjTy::COMPLEX {
            // SAFETY: The type check ensures this RValue contains a complex number,
            // so accessing it via as_complex() is safe.
            Some(unsafe { self.rvalue().as_complex() })
        } else {
            None
        }
    }

    pub fn as_complex(&self) -> &ComplexInner {
        assert_eq!(Some(ObjTy::COMPLEX), self.ty());
        // SAFETY: The assert ensures this RValue contains a complex number.
        unsafe { self.rvalue().as_complex() }
    }

    fn is_symbol(&self) -> bool {
        self.id() & 0xff == TAG_SYMBOL
    }

    pub fn as_symbol(&self) -> IdentId {
        self.try_symbol().unwrap()
    }

    pub fn try_symbol(&self) -> Option<IdentId> {
        if self.is_symbol() {
            Some(IdentId::from((self.id() >> 32) as u32))
        } else {
            None
        }
    }

    fn as_module_inner(&self) -> &ModuleInner {
        assert!(self.rvalue().ty() == ObjTy::MODULE || self.rvalue().ty() == ObjTy::CLASS);
        // SAFETY: The assert ensures this RValue contains a module or class.
        unsafe { self.rvalue().as_module() }
    }

    fn as_module_inner_mut(&mut self) -> &mut ModuleInner {
        assert!(self.rvalue().ty() == ObjTy::MODULE || self.rvalue().ty() == ObjTy::CLASS);
        // SAFETY: The assert ensures this RValue contains a module or class.
        unsafe { self.rvalue_mut().as_module_mut() }
    }

    pub(crate) fn as_array(self) -> Array {
        assert_eq!(self.ty(), Some(ObjTy::ARRAY));
        Array::new_unchecked(self)
    }

    ///
    /// Get a reference of underlying array from `self`.
    ///
    pub(crate) fn as_array_inner(&self) -> &ArrayInner {
        assert_eq!(ObjTy::ARRAY, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an array.
        unsafe { self.rvalue().as_array() }
    }

    fn as_array_inner_mut(&mut self) -> &mut ArrayInner {
        assert_eq!(ObjTy::ARRAY, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an array.
        unsafe { self.rvalue_mut().as_array_mut() }
    }

    pub(crate) fn try_array_ty(&self) -> Option<Array> {
        let rv = self.try_rvalue()?;
        match rv.ty() {
            ObjTy::ARRAY => Some(self.as_array()),
            _ => None,
        }
    }

    pub(crate) fn is_array_ty(&self) -> bool {
        self.try_array_ty().is_some()
    }

    pub(crate) fn coerce_to_array(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Array> {
        if let Some(ary) = self.try_array_ty() {
            return Ok(ary);
        } else if let Some(fid) = globals.check_method(*self, IdentId::get_id("to_ary")) {
            let v = vm.invoke_func_inner(globals, fid, *self, &[], None, None)?;
            if let Some(ary) = v.try_array_ty() {
                return Ok(ary);
            }
        }
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            *self,
            ARRAY_CLASS,
        ))
    }

    pub(crate) fn as_rstring_inner(&self) -> &RStringInner {
        assert_eq!(ObjTy::STRING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a string.
        unsafe { self.rvalue().as_rstring() }
    }

    pub(crate) fn as_rstring_inner_mut(&mut self) -> &mut RStringInner {
        assert_eq!(ObjTy::STRING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a string.
        unsafe { self.rvalue_mut().as_rstring_mut() }
    }

    ///
    /// Try to convert `self` to i64.
    ///
    /// - if `self` is a Fixnum, return it as i64.
    /// - if `self` is a Float, return it as i64.
    /// - if `self` is a Bignum, return RangeError.
    ///
    pub fn coerce_to_i64(&self, store: &Store) -> Result<i64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i),
            RV::Float(f) => {
                if f.is_nan() || f.is_infinite() || f < (i64::MIN as f64) || (i64::MAX as f64) < f {
                    Err(MonorubyErr::float_out_of_range_of_integer(f))
                } else {
                    Ok(f.trunc() as i64)
                }
            }
            RV::BigInt(_) => Err(MonorubyErr::rangeerr(
                "bignum too big to convert into `long'",
            )),
            _ => Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                INTEGER_CLASS,
            )),
        }
    }

    ///
    /// Try to convert `self` to i64.
    ///
    /// - if `self` is a Fixnum or a Bignum, convert it to f64.
    /// - if `self` is a Float, return it as f64.
    ///
    pub fn coerce_to_f64(&self, store: &Store) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i as f64),
            RV::Float(f) => Ok(f),
            RV::BigInt(b) => {
                if let Some(f) = b.to_f64() {
                    Ok(f)
                } else {
                    Err(MonorubyErr::cant_convert_into_float(store, *self))
                }
            }
            _ => Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                FLOAT_CLASS,
            )),
        }
    }

    ///
    /// Try to convert `f` to Integer.
    ///
    pub fn coerce_f64_to_int(f: f64) -> Result<Value> {
        if f.is_nan() || f.is_infinite() {
            Err(MonorubyErr::float_out_of_range_of_integer(f))
        } else if (i64::MIN as f64) < f && f < (i64::MAX as f64) {
            Ok(Value::integer(f as i64))
        } else if let Some(b) = num::BigInt::from_f64(f) {
            Ok(Value::bigint(b))
        } else {
            Err(MonorubyErr::float_out_of_range_of_integer(f))
        }
    }

    pub(crate) fn is_exception(&self) -> Option<&ExceptionInner> {
        let rv = self.try_rvalue()?;
        // SAFETY: The type check ensures this RValue contains an exception.
        unsafe {
            match rv.ty() {
                ObjTy::EXCEPTION => Some(rv.as_exception()),
                _ => None,
            }
        }
    }

    pub(crate) fn is_exception_mut(&mut self) -> Option<&mut ExceptionInner> {
        let rv = self.try_rvalue_mut()?;
        // SAFETY: The type check ensures this RValue contains an exception.
        unsafe {
            match rv.ty() {
                ObjTy::EXCEPTION => Some(rv.as_exception_mut()),
                _ => None,
            }
        }
    }

    pub(crate) fn try_hash_ty(self) -> Option<Hashmap> {
        match self.try_rvalue()?.ty() {
            ObjTy::HASH => Some(Hashmap::new(self)),
            _ => None,
        }
    }

    pub(crate) fn as_hash(self) -> Hashmap {
        self.try_hash_ty().unwrap()
    }

    pub(crate) fn as_hashmap_inner(&self) -> &HashmapInner {
        assert_eq!(ObjTy::HASH, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a hash.
        unsafe { self.rvalue().as_hashmap() }
    }

    pub(crate) fn as_hashmap_inner_mut(&mut self) -> &mut HashmapInner {
        assert_eq!(ObjTy::HASH, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a hash.
        unsafe { self.rvalue_mut().as_hashmap_mut() }
    }

    pub(crate) fn as_regexp_inner(&self) -> &RegexpInner {
        assert_eq!(ObjTy::REGEXP, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a regexp.
        unsafe { self.rvalue().as_regex() }
    }

    pub(crate) fn as_regexp_inner_mut(&mut self) -> &mut RegexpInner {
        assert_eq!(ObjTy::REGEXP, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a regexp.
        unsafe { self.rvalue_mut().as_regex_mut() }
    }

    pub(crate) fn is_regex(&self) -> Option<Regexp> {
        let rv = self.try_rvalue()?;
        match rv.ty() {
            ObjTy::REGEXP => Some(Regexp::new_unchecked(*self)),
            _ => None,
        }
    }

    pub(crate) fn as_regexp(self) -> Regexp {
        self.is_regex().unwrap()
    }

    pub(crate) fn is_class_or_module(&self) -> Option<Module> {
        match self.try_rvalue()?.ty() {
            ObjTy::CLASS | ObjTy::MODULE => Some(Module::new_unchecked(*self)),
            _ => None,
        }
    }

    pub(crate) fn is_class(&self) -> Option<Module> {
        if self.try_rvalue()?.ty() == ObjTy::CLASS {
            Some(Module::new_unchecked(*self))
        } else {
            None
        }
    }

    pub(crate) fn is_module(&self) -> Option<Module> {
        if self.try_rvalue()?.ty() == ObjTy::MODULE {
            Some(Module::new_unchecked(*self))
        } else {
            None
        }
    }

    pub(crate) fn as_class(&self) -> Module {
        assert!(matches!(
            self.ty(),
            Some(ObjTy::CLASS) | Some(ObjTy::MODULE)
        ));
        Module::new_unchecked(*self)
    }

    pub(crate) fn as_class_id(&self) -> ClassId {
        self.as_class().id()
    }

    pub(crate) fn as_time(&self) -> &TimeInner {
        match self.rvalue().ty() {
            ObjTy::TIME => self.rvalue().as_time(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_time_mut(&mut self) -> &mut TimeInner {
        match self.rvalue().ty() {
            ObjTy::TIME => self.rvalue_mut().as_time_mut(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn is_range(&self) -> Option<&RangeInner> {
        if let Some(rvalue) = self.try_rvalue() {
            // SAFETY: The type check ensures this RValue contains a range.
            unsafe {
                match rvalue.ty() {
                    ObjTy::RANGE => Some(self.rvalue().as_range()),
                    _ => None,
                }
            }
        } else {
            None
        }
    }

    pub(crate) fn is_proc(self) -> Option<Proc> {
        if let Some(rvalue) = self.try_rvalue() {
            match rvalue.ty() {
                ObjTy::PROC => Some(Proc::new(self)),
                _ => None,
            }
        } else {
            None
        }
    }

    pub(crate) fn is_method(&self) -> Option<&MethodInner> {
        if let Some(rvalue) = self.try_rvalue() {
            match rvalue.ty() {
                ObjTy::METHOD => Some(self.rvalue().as_method()),
                _ => None,
            }
        } else {
            None
        }
    }

    pub(crate) fn expect_class_or_module(&self, store: &Store) -> Result<Module> {
        match self.is_class_or_module() {
            Some(class) => Ok(class),
            None => {
                let name = self.inspect(store);
                Err(MonorubyErr::is_not_class_nor_module(name))
            }
        }
    }

    /*pub(crate) fn expect_class_or_module_rescue(&self) -> Result<ClassId> {
        match self.is_class_or_module() {
            Some(class) => Ok(class),
            None => Err(MonorubyErr::is_not_class_nor_module_rescue()),
        }
    }*/

    pub(crate) fn expect_class(&self, globals: &Globals) -> Result<Module> {
        match self.is_class() {
            Some(class) => Ok(class),
            None => {
                let name = self.to_s(&globals.store);
                Err(MonorubyErr::is_not_class(name))
            }
        }
    }

    pub(crate) fn expect_module(&self, globals: &Globals) -> Result<Module> {
        self.is_module().ok_or_else(|| {
            let name = self.to_s(&globals.store);
            MonorubyErr::is_not_class(name)
        })
    }

    #[allow(dead_code)]
    pub(crate) fn expect_symbol(&self, store: &Store) -> Result<IdentId> {
        self.try_symbol()
            .ok_or_else(|| MonorubyErr::is_not_symbol(store, *self))
    }

    pub(crate) fn expect_symbol_or_string(&self, store: &Store) -> Result<IdentId> {
        self.try_symbol_or_string()
            .ok_or_else(|| MonorubyErr::is_not_symbol_nor_string(store, *self))
    }

    pub(crate) fn expect_bytes(&self, store: &Store) -> Result<&[u8]> {
        if let Some(s) = self.is_rstring_inner() {
            Ok(&s)
        } else {
            Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                STRING_CLASS,
            ))
        }
    }

    pub(crate) fn expect_string(&self, store: &Store) -> Result<String> {
        self.expect_str(store).map(|s| s.to_string())
    }

    pub(crate) fn expect_str(&self, store: &Store) -> Result<&str> {
        if let Some(s) = self.is_rstring_inner() {
            s.check_utf8()
        } else {
            Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                STRING_CLASS,
            ))
        }
    }

    pub(crate) fn coerce_to_rstring(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        search_method(vm, globals, &[IdentId::TO_STR], *self)
    }

    pub(crate) fn coerce_to_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<String> {
        Ok(self.coerce_to_rstring(vm, globals)?.to_str()?.to_string())
    }

    pub(crate) fn coerce_to_path_rstring(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        search_method(vm, globals, &[IdentId::TO_STR, IdentId::TO_PATH], *self)
    }

    pub(crate) fn expect_regexp_or_string(&self, store: &Store) -> Result<Regexp> {
        if let Some(re) = self.is_regex() {
            Ok(re)
        } else if let Some(s) = self.is_str() {
            Ok(Regexp::new_unchecked(Value::regexp(
                RegexpInner::with_option(s, 0)?,
            )))
        } else {
            Err(MonorubyErr::is_not_regexp_nor_string(store, *self))
        }
    }

    pub(crate) fn expect_integer(&self, store: &Store) -> Result<i64> {
        if let RV::Fixnum(i) = self.unpack() {
            Ok(i)
        } else {
            Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                INTEGER_CLASS,
            ))
        }
    }

    pub(crate) fn expect_array_ty(&self, store: &Store) -> Result<Array> {
        if let Some(ary) = self.try_array_ty() {
            Ok(ary)
        } else {
            Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                ARRAY_CLASS,
            ))
        }
    }

    pub(crate) fn expect_hash_ty(self, store: &Store) -> Result<Hashmap> {
        if let Some(h) = self.try_hash_ty() {
            Ok(h)
        } else {
            Err(MonorubyErr::no_implicit_conversion(store, self, HASH_CLASS))
        }
    }

    pub(crate) fn try_bytes(&self) -> Option<&RStringInner> {
        if let Some(rv) = self.try_rvalue() {
            // SAFETY: The type check ensures this RValue contains a string.
            unsafe {
                match rv.ty() {
                    ObjTy::STRING => Some(rv.as_rstring()),
                    _ => None,
                }
            }
        } else {
            None
        }
    }

    pub(crate) fn is_rstring_inner(&self) -> Option<&RStringInner> {
        let rv = self.try_rvalue()?;
        // SAFETY: The type check ensures this RValue contains a string.
        unsafe {
            match rv.ty() {
                ObjTy::STRING => Some(rv.as_rstring()),
                _ => None,
            }
        }
    }

    pub(crate) fn is_rstring(&self) -> Option<RString> {
        let rv = self.try_rvalue()?;
        match rv.ty() {
            ObjTy::STRING => Some(RString::new_unchecked(*self)),
            _ => None,
        }
    }

    pub(crate) fn as_str(&self) -> &str {
        assert_eq!(ObjTy::STRING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a string.
        unsafe { self.rvalue().as_str() }
    }

    pub(crate) fn is_str(&self) -> Option<&str> {
        let rv = self.try_rvalue()?;
        // SAFETY: The type check ensures this RValue contains a string.
        unsafe {
            match rv.ty() {
                ObjTy::STRING => Some(rv.as_str()),
                _ => None,
            }
        }
    }

    pub(crate) fn replace_string(&mut self, replace: String) {
        assert_eq!(ObjTy::STRING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a string.
        unsafe { *self.rvalue_mut().as_rstring_mut() = RStringInner::from_string(replace) };
    }

    pub(crate) fn replace_str(&mut self, replace: &str) {
        assert_eq!(ObjTy::STRING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a string.
        unsafe { *self.rvalue_mut().as_rstring_mut() = RStringInner::from_str(replace) };
    }

    pub(crate) fn as_range(&self) -> &RangeInner {
        assert_eq!(ObjTy::RANGE, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a range.
        unsafe { self.rvalue().as_range() }
    }

    #[allow(dead_code)]
    pub(crate) fn as_io_inner(&self) -> &IoInner {
        assert_eq!(ObjTy::IO, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an IO object.
        unsafe { self.rvalue().as_io() }
    }

    pub(crate) fn as_io_inner_mut(&mut self) -> &mut IoInner {
        assert_eq!(ObjTy::IO, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an IO object.
        unsafe { self.rvalue_mut().as_io_mut() }
    }

    pub(crate) fn as_proc_inner(&self) -> &ProcInner {
        assert_eq!(ObjTy::PROC, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a proc.
        unsafe { self.rvalue().as_proc() }
    }

    pub(crate) fn as_proc_inner_mut(&mut self) -> &mut ProcInner {
        assert_eq!(ObjTy::PROC, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a proc.
        unsafe { self.rvalue_mut().as_proc_mut() }
    }

    pub fn as_method(&self) -> &MethodInner {
        assert_eq!(ObjTy::METHOD, self.rvalue().ty());
        self.rvalue().as_method()
    }

    pub fn as_umethod(&self) -> &UMethodInner {
        assert_eq!(ObjTy::UMETHOD, self.rvalue().ty());
        self.rvalue().as_umethod()
    }

    pub fn as_fiber_inner(&self) -> &FiberInner {
        assert_eq!(ObjTy::FIBER, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a fiber.
        unsafe { self.rvalue().as_fiber() }
    }

    pub fn as_fiber_inner_mut(&mut self) -> &mut FiberInner {
        assert_eq!(ObjTy::FIBER, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a fiber.
        unsafe { self.rvalue_mut().as_fiber_mut() }
    }

    pub fn as_enumerator_inner(&self) -> &EnumeratorInner {
        assert_eq!(ObjTy::ENUMERATOR, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an enumerator.
        unsafe { self.rvalue().as_enumerator() }
    }

    pub fn as_enumerator_inner_mut(&mut self) -> &mut EnumeratorInner {
        assert_eq!(ObjTy::ENUMERATOR, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains an enumerator.
        unsafe { self.rvalue_mut().as_enumerator_mut() }
    }

    pub fn as_generator_inner(&self) -> &GeneratorInner {
        assert_eq!(ObjTy::GENERATOR, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a generator.
        unsafe { self.rvalue().as_generator() }
    }

    pub fn as_generator_inner_mut(&mut self) -> &mut GeneratorInner {
        assert_eq!(ObjTy::GENERATOR, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a generator.
        unsafe { self.rvalue_mut().as_generator_mut() }
    }

    pub fn as_binding_inner(&self) -> &BindingInner {
        assert_eq!(ObjTy::BINDING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a binding.
        unsafe { self.rvalue().as_binding() }
    }

    pub fn as_binding_inner_mut(&mut self) -> &mut BindingInner {
        assert_eq!(ObjTy::BINDING, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a binding.
        unsafe { self.rvalue_mut().as_binding_mut() }
    }

    pub fn as_match_data(&self) -> &MatchDataInner {
        assert_eq!(ObjTy::MATCHDATA, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains match data.
        unsafe { self.rvalue().as_match_data() }
    }
}

impl Value {
    pub(crate) fn from_ast(node: &Node, globals: &mut Globals) -> Value {
        let mut vm = Executor::default();
        Self::from_ast_inner(node, &mut vm, globals)
    }

    fn from_ast_inner(node: &Node, vm: &mut Executor, globals: &mut Globals) -> Value {
        use ruruby_parse::NReal;

        match &node.kind {
            NodeKind::CompStmt(stmts) => {
                assert_eq!(1, stmts.len());
                Self::from_ast_inner(&stmts[0], vm, globals)
            }
            NodeKind::Integer(num) => Value::integer(*num),
            NodeKind::Bignum(num) => Value::bigint(num.clone()),
            NodeKind::Float(num) => Value::float(*num),
            NodeKind::Imaginary(r) => match r {
                NReal::Float(f) => Value::complex(0, *f),
                NReal::Integer(i) => Value::complex(0, *i),
                NReal::Bignum(b) => Value::complex(0, b.clone()),
            },
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::symbol_from_str(sym),
            NodeKind::String(s) => Value::string_from_str(s),
            NodeKind::Bytes(b) => Value::bytes_from_slice(b),
            NodeKind::Array(v, ..) => {
                let iter = v.iter().map(|node| Self::from_ast_inner(node, vm, globals));
                Value::array_from_iter(iter)
            }
            NodeKind::Const {
                toplevel,
                parent,
                prefix,
                name,
            } => {
                assert_eq!(false, *toplevel);
                assert_eq!(None, *parent);
                if prefix.len() == 0 {
                    let constant = IdentId::get_id(name);
                    globals
                        .get_constant_noautoload(OBJECT_CLASS, constant)
                        .unwrap()
                } else {
                    let mut module = globals
                        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id(&prefix[0]))
                        .unwrap();
                    for id in &prefix[1..] {
                        module = globals
                            .get_constant_noautoload(
                                module.is_class_or_module().unwrap().id(),
                                IdentId::get_id(id),
                            )
                            .unwrap();
                    }
                    globals
                        .get_constant_noautoload(
                            module.is_class_or_module().unwrap().id(),
                            IdentId::get_id(name),
                        )
                        .unwrap()
                }
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                ..
            } => {
                let start = if let Some(start) = start {
                    Self::from_ast_inner(start, vm, globals)
                } else {
                    Value::nil()
                };
                let end = if let Some(end) = end {
                    Self::from_ast_inner(end, vm, globals)
                } else {
                    Value::nil()
                };
                Value::range(start, end, *exclude_end)
            }
            NodeKind::Hash(v) => {
                let mut map = RubyMap::default();
                for (k, v) in v.iter() {
                    let k = Self::from_ast_inner(k, vm, globals);
                    let v = Self::from_ast_inner(v, vm, globals);
                    map.insert(k, v, vm, globals).unwrap();
                }
                Value::hash(map)
            }
            NodeKind::BinOp(ruruby_parse::BinOp::Add, box lhs, box rhs) => {
                let lhs = Self::from_ast_inner(lhs, vm, globals);
                if let NodeKind::Imaginary(im) = &rhs.kind {
                    Value::complex(Real::try_from(globals, lhs).unwrap(), im.clone())
                } else {
                    unreachable!()
                }
            }
            NodeKind::BinOp(ruruby_parse::BinOp::Sub, box lhs, box rhs) => {
                let lhs = Self::from_ast_inner(lhs, vm, globals);
                if let NodeKind::Imaginary(im) = &rhs.kind {
                    Value::complex(
                        Real::try_from(globals, lhs).unwrap(),
                        -Real::from(im.clone()),
                    )
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!("{:?}", node.kind),
        }
    }

    pub(crate) fn from_const_ast(node: &Node) -> Value {
        use ruruby_parse::NReal;
        match &node.kind {
            NodeKind::Integer(num) => Value::integer(*num),
            NodeKind::Bignum(num) => Value::bigint(num.clone()),
            NodeKind::Float(num) => Value::float(*num),
            NodeKind::Imaginary(r) => match r {
                NReal::Float(f) => Value::complex(0, *f),
                NReal::Integer(i) => Value::complex(0, *i),
                NReal::Bignum(b) => Value::complex(0, b.clone()),
            },
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::symbol_from_str(sym),
            NodeKind::String(s) => Value::string_from_str(s),
            NodeKind::Bytes(b) => Value::bytes_from_slice(b),
            NodeKind::Array(v, ..) => {
                let iter = v.iter().map(|node| Self::from_const_ast(node));
                Value::array_from_iter(iter)
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                ..
            } => {
                let start = if let Some(start) = start {
                    Self::from_const_ast(start)
                } else {
                    Value::nil()
                };
                let end = if let Some(end) = end {
                    Self::from_const_ast(end)
                } else {
                    Value::nil()
                };
                Value::range(start, end, *exclude_end)
            }
            _ => unreachable!("{:?}", node.kind),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Immediate(std::num::NonZeroU64);

impl Deref for Immediate {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Immediate as *const Value) }
    }
}

impl AsRef<Value> for Immediate {
    fn as_ref(&self) -> &Value {
        self.deref()
    }
}

impl From<Value> for Immediate {
    fn from(value: Value) -> Self {
        assert!(value.is_packed_value());
        Self(value.0)
    }
}

impl Debug for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v: Value = (*self).into();
        write!(f, "{:?}", v)
    }
}

impl Immediate {
    fn from_u64(u: u64) -> Immediate {
        Immediate(std::num::NonZeroU64::new(u).unwrap())
    }

    pub fn nil() -> Self {
        Immediate::from_u64(NIL_VALUE)
    }

    pub fn bool(b: bool) -> Self {
        Immediate::from_u64(if b { TRUE_VALUE } else { FALSE_VALUE })
    }

    fn fixnum(num: i64) -> Self {
        Immediate::from_u64((num << 1) as u64 | 0b1)
    }

    pub fn symbol(id: IdentId) -> Self {
        Immediate::from_u64((id.get() as u64) << 32 | TAG_SYMBOL)
    }

    pub fn flonum(num: f64) -> Option<Self> {
        if num == 0.0 {
            return Some(Self::from_u64(FLOAT_ZERO));
        }
        let unum = f64::to_bits(num);
        let exp = ((unum >> 60) & 0b111) + 1;
        if (exp & 0b0110) == 0b0100 {
            Some(Self::from_u64(((unum.rotate_left(3)) & !1) | 2))
        } else {
            None
        }
    }

    pub fn check_fixnum(i: i64) -> Option<Self> {
        if Value::is_i63(i) {
            Some(Immediate::fixnum(i))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct RubySymbol(std::num::NonZeroU64);

impl Deref for RubySymbol {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RubySymbol as *const Value) }
    }
}

impl AsRef<Value> for RubySymbol {
    fn as_ref(&self) -> &Value {
        self.deref()
    }
}

impl RubySymHash for RubySymbol {
    fn ruby_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl RubySymEql for RubySymbol {
    fn eql(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl RubySymbol {
    pub fn new(id: IdentId) -> Self {
        RubySymbol(Value::symbol(id).0)
    }
}

#[derive(Clone, PartialEq)]
pub enum RV<'a> {
    Invalid,
    Nil,
    Bool(bool),
    Fixnum(i64),
    BigInt(&'a BigInt),
    Float(f64),
    Symbol(IdentId),
    Complex(&'a num::complex::Complex<Real>),
    String(&'a RStringInner),
    Object(&'a RValue),
}

impl<'a> std::fmt::Debug for RV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV::Invalid => write!(f, "INVALID"),
            RV::Nil => write!(f, "nil"),
            RV::Bool(b) => write!(f, "{b:?}"),
            RV::Fixnum(n) => write!(f, "{n}"),
            RV::BigInt(n) => write!(f, "Bignum({n})"),
            RV::Float(n) => write!(f, "{}", dtoa::Buffer::new().format(*n),),
            RV::Complex(c) => write!(f, "{:?}", c),
            RV::Symbol(id) => write!(f, ":{}", id),
            RV::String(s) => write!(f, "\"{}\"", s.to_str().unwrap()),
            RV::Object(rvalue) => write!(f, "{rvalue:?}"),
        }
    }
}

impl<'a> std::fmt::Display for RV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV::Invalid => write!(f, "INVALID"),
            RV::Nil => write!(f, "nil"),
            RV::Bool(b) => write!(f, "{b:?}"),
            RV::Fixnum(n) => write!(f, "{n}"),
            RV::BigInt(n) => write!(f, "{n}"),
            RV::Float(n) => write!(f, "{}", dtoa::Buffer::new().format(*n),),
            RV::Complex(c) => write!(f, "{}", c),
            RV::Symbol(id) => write!(f, ":{}", id),
            RV::String(s) => write!(f, "\"{}\"", s.to_str().unwrap()),
            RV::Object(rvalue) => write!(f, "{rvalue:?}"),
        }
    }
}
