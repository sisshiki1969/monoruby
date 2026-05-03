use core::f64;
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

/// Format an f64 value as a String matching CRuby's `Float#to_s` / `Float#inspect` output.
///
/// Uses Rust's `{:?}` formatting (Ryu algorithm, shortest round-trip representation)
/// and then reformats to match CRuby's conventions:
/// - Special values: "NaN", "Infinity", "-Infinity"
/// - Preserves `-0.0`
/// - Scientific notation when decimal exponent >= 15 or <= -5
/// - Scientific exponent always includes sign and at least two digits (`e+02`, `e-05`)
pub fn ruby_float_to_s(f: f64) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        };
    }
    if f == 0.0 {
        return if f.is_sign_negative() {
            "-0.0".to_string()
        } else {
            "0.0".to_string()
        };
    }

    // Rust's Debug format for f64 uses the Ryu algorithm, producing the shortest
    // string that round-trips back to the same f64 value — the same property as
    // CRuby's ruby_dtoa (mode 0).
    let s = format!("{:?}", f);

    let (negative, rest) = if let Some(r) = s.strip_prefix('-') {
        (true, r)
    } else {
        (false, s.as_str())
    };

    // Parse mantissa and optional exponent from Rust's output (e.g., "1.23e-10" or "123.456")
    let (mantissa_str, exp_offset) = if let Some(pos) = rest.find('e') {
        let exp: i32 = rest[pos + 1..].parse().unwrap();
        (&rest[..pos], Some(exp))
    } else {
        (rest, None)
    };

    let dot_pos = mantissa_str.find('.').unwrap_or(mantissa_str.len());
    let all_digits: String = mantissa_str.chars().filter(|&c| c != '.').collect();
    // Strip trailing zeros to get significant digits only (matching ruby_dtoa output)
    let digits = all_digits.trim_end_matches('0');
    let digits = if digits.is_empty() { "0" } else { digits };

    // decpt: position of the decimal point (number of digits before it)
    let decpt = if let Some(exp) = exp_offset {
        exp + dot_pos as i32
    } else {
        dot_pos as i32
    };

    let mut result = String::new();
    if negative {
        result.push('-');
    }

    let ndigits = digits.len() as i32;

    if decpt > 0 && decpt <= 15 {
        // Fixed notation: decimal point is within range
        if decpt >= ndigits {
            // All significant digits before decimal point, pad with zeros
            result.push_str(digits);
            for _ in 0..(decpt - ndigits) {
                result.push('0');
            }
            result.push_str(".0");
        } else {
            // Decimal point falls within the digits
            result.push_str(&digits[..decpt as usize]);
            result.push('.');
            result.push_str(&digits[decpt as usize..]);
        }
    } else if decpt <= 0 && decpt > -4 {
        // Fixed notation with leading zeros: 0.000...digits
        result.push_str("0.");
        for _ in 0..(-decpt) {
            result.push('0');
        }
        result.push_str(digits);
    } else {
        // Scientific notation: d.dddde+dd
        result.push_str(&digits[..1]);
        result.push('.');
        if ndigits > 1 {
            result.push_str(&digits[1..]);
        } else {
            result.push('0');
        }
        let exp = decpt - 1;
        if exp >= 0 {
            result.push_str(&format!("e+{:02}", exp));
        } else {
            result.push_str(&format!("e-{:02}", -exp));
        }
    }

    result
}

/// Integer representation for sprintf (supports both Fixnum and BigInt).
pub(crate) enum IntegerBase {
    Fixnum(i64),
    BigInt(num::BigInt),
}

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

thread_local! {
    static HASH_RECURSION_GUARD: std::cell::RefCell<HashSet<u64>> = std::cell::RefCell::new(HashSet::new());
    static INSPECT_RECURSION_GUARD: std::cell::RefCell<HashSet<u64>> = std::cell::RefCell::new(HashSet::new());
    static COMPARE_RECURSION_GUARD: std::cell::RefCell<HashSet<(u64, u64)>> = std::cell::RefCell::new(HashSet::new());
}

/// Execute `f` with recursion protection for inspect/to_s.
/// If `id` is already in the guard, returns `on_recursive` without calling `f`.
pub(crate) fn exec_recursive<F>(id: u64, f: F, on_recursive: Value) -> Result<Value>
where
    F: FnOnce() -> Result<Value>,
{
    let is_recursive = INSPECT_RECURSION_GUARD.with(|guard| !guard.borrow_mut().insert(id));
    if is_recursive {
        return Ok(on_recursive);
    }
    let result = f();
    INSPECT_RECURSION_GUARD.with(|guard| guard.borrow_mut().remove(&id));
    result
}

thread_local! {
    /// Tracks whether the *outer* recursion-protected hash call has seen any
    /// nested recursion. Used by [`exec_recursive_outer`] to surface inner
    /// cycles to the outermost frame so that two structures with cycles at
    /// different depths still hash to the same sentinel value (matching
    /// CRuby's `rb_exec_recursive_outer`).
    static OUTER_RECURSION_DEPTH: std::cell::RefCell<usize> = const { std::cell::RefCell::new(0) };
    static OUTER_RECURSION_HIT: std::cell::RefCell<bool> = const { std::cell::RefCell::new(false) };
}

/// Execute `f` with **outer**-recursion protection: if any recursion is
/// detected at *any* depth during `f` (including transitively, via the same
/// guard set), the **outermost** call returns `on_recursive` instead of `f`'s
/// result. Equivalent to CRuby's `rb_exec_recursive_outer`.
///
/// This is what `Struct#hash` uses so that two structs that are cyclic at
/// different depths still hash to the same value (the spec requirement that
/// `car.hash == similar_car.hash` when one wraps the other and one is
/// directly self-cyclic).
pub(crate) fn exec_recursive_outer<T, F>(id: u64, f: F, on_recursive: T) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    let is_outer = OUTER_RECURSION_DEPTH.with(|d| {
        let was = *d.borrow() == 0;
        *d.borrow_mut() += 1;
        was
    });
    if is_outer {
        OUTER_RECURSION_HIT.with(|h| *h.borrow_mut() = false);
    }
    let on_stack = HASH_RECURSION_GUARD.with(|g| !g.borrow_mut().insert(id));
    if on_stack {
        // Inner recursion: surface to outer.
        OUTER_RECURSION_HIT.with(|h| *h.borrow_mut() = true);
        OUTER_RECURSION_DEPTH.with(|d| *d.borrow_mut() -= 1);
        return Ok(on_recursive);
    }
    let result = f();
    HASH_RECURSION_GUARD.with(|g| {
        g.borrow_mut().remove(&id);
    });
    OUTER_RECURSION_DEPTH.with(|d| *d.borrow_mut() -= 1);
    if is_outer {
        let hit = OUTER_RECURSION_HIT.with(|h| {
            let v = *h.borrow();
            *h.borrow_mut() = false;
            v
        });
        if hit {
            return Ok(on_recursive);
        }
    }
    result
}

/// Execute `f` with recursion protection for paired operations (==, <=>, eql?).
///
/// Uses a `(lhs_id, rhs_id)` pair to detect when the same pair of containers
/// is being compared recursively. Works for Array, Hash, Set, and any
/// container type.
///
/// If the pair is already being compared, returns `on_recursive` immediately.
pub(crate) fn exec_recursive_paired<F>(
    lhs_id: u64,
    rhs_id: u64,
    f: F,
    on_recursive: Value,
) -> Result<Value>
where
    F: FnOnce() -> Result<Value>,
{
    let pair = (lhs_id, rhs_id);
    let is_recursive = COMPARE_RECURSION_GUARD.with(|guard| !guard.borrow_mut().insert(pair));
    if is_recursive {
        return Ok(on_recursive);
    }
    let result = f();
    COMPARE_RECURSION_GUARD.with(|guard| guard.borrow_mut().remove(&pair));
    result
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
                    ObjTy::ARRAY | ObjTy::HASH => {
                        let id = self.id();
                        let is_recursive =
                            HASH_RECURSION_GUARD.with(|guard| !guard.borrow_mut().insert(id));
                        if is_recursive {
                            0u64.hash(state);
                            return Ok(());
                        }
                        let result = match lhs.ty() {
                            ObjTy::ARRAY => lhs.as_array().ruby_hash(state, e, g),
                            _ => lhs.as_hashmap().ruby_hash(state, e, g),
                        };
                        HASH_RECURSION_GUARD.with(|guard| guard.borrow_mut().remove(&id));
                        result?;
                    }
                    ObjTy::RANGE => lhs.as_range().ruby_hash(state, e, g)?,
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
                            let lhs_ary = lhs.as_array();
                            let rhs_ary = rhs.as_array();
                            if lhs_ary.len() != rhs_ary.len() {
                                return Ok(false);
                            }
                            let l = self.id();
                            let r = other.id();
                            return exec_recursive_paired(
                                l,
                                r,
                                || {
                                    for (a1, a2) in lhs_ary.iter().zip(rhs_ary.iter()) {
                                        if !a1.eql(a2, vm, globals)? {
                                            return Ok(Value::bool(false));
                                        }
                                    }
                                    Ok(Value::bool(true))
                                },
                                Value::bool(true),
                            )
                            .map(|v| v == Value::bool(true));
                        }
                        (ObjTy::RANGE, ObjTy::RANGE) => {
                            lhs.as_range().eql(rhs.as_range(), vm, globals)?
                        }
                        (ObjTy::HASH, ObjTy::HASH) => {
                            let lhs_id = self.id();
                            let rhs_id = other.id();
                            return exec_recursive_paired(
                                lhs_id,
                                rhs_id,
                                || {
                                    let r = lhs.as_hashmap().eql(rhs.as_hashmap(), vm, globals)?;
                                    Ok(Value::bool(r))
                                },
                                Value::bool(true),
                            )
                            .map(|v| v == Value::bool(true));
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

    ///
    /// Class id used for inline-cache and JIT type-prediction purposes.
    ///
    /// Identical to [`Value::class`] except for boolean values: both `true`
    /// and `false` report [`BOOL_CLASS`] so a polymorphic-on-true/false
    /// call site does not deopt on every flip. The returned class id has
    /// no Ruby-visible counterpart — `true.class` and `false.class` still
    /// return `TrueClass` and `FalseClass`.
    ///
    /// Method lookup using [`BOOL_CLASS`] is **not** valid (the class has
    /// no method table); callers must translate it back to `TRUE_CLASS`
    /// (which holds the same `FuncId` as `FALSE_CLASS` for any method
    /// shared between booleans, by construction in `bool_class::init`).
    ///
    pub(crate) fn class_for_ic(&self) -> ClassId {
        let v = self.0.get();
        if v == TRUE_VALUE || v == FALSE_VALUE {
            BOOL_CLASS
        } else {
            self.class()
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

    ///
    /// Returns true if `self` is frozen.
    ///
    /// Packed values (Fixnum, Symbol, true, false, nil) are always frozen.
    ///
    pub(crate) fn is_frozen(&self) -> bool {
        match self.try_rvalue() {
            Some(rv) => rv.is_frozen(),
            None => true, // packed values are always frozen
        }
    }

    ///
    /// Set the frozen flag on `self`.
    ///
    /// Panics if `self` is a packed value (they are always frozen).
    ///
    pub(crate) fn set_frozen(&mut self) {
        self.rvalue_mut().set_frozen()
    }

    ///
    /// Ensure `self` is not frozen. Returns FrozenError if it is.
    ///
    pub(crate) fn ensure_not_frozen(&self, store: &Store) -> Result<()> {
        if self.is_frozen() {
            Err(MonorubyErr::cant_modify_frozen(store, *self))
        } else {
            Ok(())
        }
    }

    ///
    /// Returns true if `self` is a "chilled" String — a String returned by
    /// `Symbol#to_s` that behaves mutable but should emit a one-shot
    /// deprecation warning on first mutation.
    ///
    pub(crate) fn is_chilled(&self) -> bool {
        match self.try_rvalue() {
            Some(rv) => rv.is_chilled(),
            None => false,
        }
    }

    pub(crate) fn set_chilled(&mut self) {
        self.rvalue_mut().set_chilled()
    }

    pub(crate) fn clear_chilled(&mut self) {
        self.rvalue_mut().clear_chilled()
    }

    ///
    /// Ensure `self` (a String) is mutable. If it is "chilled", emit a
    /// one-shot deprecation warning (gated by `Warning[:deprecated]`), clear
    /// the chilled flag, and proceed. If it is truly frozen, raise
    /// `FrozenError`. Otherwise do nothing. Must only be called on String
    /// receivers; behaves like `ensure_not_frozen` for non-chilled values.
    ///
    pub(crate) fn ensure_string_mutable(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        if self.is_chilled() {
            self.clear_chilled();
            emit_chilled_string_mutation_warning(vm, globals, *self)?;
        }
        self.ensure_not_frozen(&globals.store)
    }

    pub(crate) fn change_class(&mut self, new_class_id: ClassId) {
        if let Some(rv) = self.try_rvalue_mut() {
            rv.change_class(new_class_id);
        } else {
            let class = self.class();
            unreachable!("the class of primitive class {class:?} can not be changed.");
        }
    }

    pub(crate) fn get_singleton(self, store: &mut Store) -> Result<Value> {
        if let Some(class) = self.is_class_or_module() {
            Ok(store.get_metaclass(class.id()).as_val())
        } else if self.is_packed_value() {
            match self.0.get() {
                NIL_VALUE => Ok(store[NIL_CLASS].get_module().as_val()),
                TRUE_VALUE => Ok(store[TRUE_CLASS].get_module().as_val()),
                FALSE_VALUE => Ok(store[FALSE_CLASS].get_module().as_val()),
                _ => Err(MonorubyErr::typeerr("can't define singleton")),
            }
        } else {
            Ok(store.get_singleton(self)?.as_val())
        }
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

    pub(crate) fn clone_value(&self) -> Self {
        if let Some(rv) = self.try_rvalue() {
            rv.clone_value().pack()
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

    pub fn is_i63(num: i64) -> bool {
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

    pub fn rational(num: i64, den: i64) -> Self {
        RValue::new_rational(RationalInner::new(num, den)).pack()
    }

    pub fn rational_from_bigint(n: BigInt, d: BigInt) -> Self {
        RValue::new_rational(RationalInner::new_bigint(n, d)).pack()
    }

    pub fn rational_from_inner(inner: RationalInner) -> Self {
        RValue::new_rational(inner).pack()
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

    /// Create a `Struct` subclass instance with `len` slots all set to
    /// nil. The class id determines which `Struct.new(...)`-derived
    /// class this instance belongs to.
    pub fn struct_object(class_id: ClassId, len: usize) -> Self {
        RValue::new_struct_obj(class_id, len).pack()
    }

    pub fn yielder_object() -> Self {
        // SAFETY: YIELDER is a static variable that is properly initialized before use.
        // Access is synchronized through single-threaded Ruby VM execution.
        Value::object(unsafe { crate::builtins::YIELDER.unwrap().id() })
    }

    pub fn string(s: String) -> Self {
        RValue::new_string(s).pack()
    }

    pub fn string_with_class(s: &str, class_id: ClassId) -> Self {
        RValue::new_string_with_class(s, class_id).pack()
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

    ///
    /// Build a String from a source-file byte literal (e.g. `"\xC3\xA9"`).
    ///
    /// The result is tagged as UTF-8, matching CRuby's source-encoding
    /// semantics where string literals inherit the source file encoding
    /// regardless of whether `\xNN` escapes produce valid UTF-8. The bytes
    /// may be invalid UTF-8, in which case `#valid_encoding?` returns
    /// false and byte-level operations still work.
    ///
    pub fn string_from_source_bytes(b: &[u8]) -> Self {
        Self::string_from_inner(RStringInner::from_encoding(b, Encoding::Utf8))
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

    pub fn regexp_with_class(regexp: RegexpInner, class_id: ClassId) -> Self {
        RValue::new_regexp_with_class(regexp, class_id).pack()
    }

    pub(crate) fn new_io(io: IoInner) -> Self {
        RValue::new_io(io).pack()
    }

    pub(crate) fn new_io_with_class(io: IoInner, class_id: ClassId) -> Self {
        RValue::new_io_with_class(io, class_id).pack()
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

    pub fn range_with_class(
        start: Value,
        end: Value,
        exclude_end: bool,
        class_id: ClassId,
    ) -> Self {
        RValue::new_range_with_class(start, end, exclude_end, class_id).pack()
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

    pub fn new_exception_from_with_class(
        message: String,
        err_class_id: ClassId,
        obj_class_id: ClassId,
    ) -> Self {
        let err = MonorubyErr::new(MonorubyErrKind::from_class_id(err_class_id), message);
        RValue::new_exception_with_class(err, obj_class_id).pack()
    }

    pub fn new_time(time: TimeInner) -> Self {
        RValue::new_time(time).pack()
    }

    pub fn new_time_with_class(time: TimeInner, class_id: ClassId) -> Self {
        RValue::new_time_with_class(time, class_id).pack()
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
        size: Option<Value>,
    ) -> Self {
        RValue::new_enumerator(obj, method, proc, args, size).pack()
    }

    pub(crate) fn new_generator(proc: Proc) -> Self {
        RValue::new_generator(proc).pack()
    }

    pub(crate) fn new_binding(outer_lfp: Lfp, pc: Option<BytecodePtr>) -> Self {
        RValue::new_binding(outer_lfp, pc).pack()
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
            RV::Float(f) => ruby_float_to_s(f),
            RV::Complex(_) => self.as_complex().debug(store),
            RV::Rational(r) => r.inspect(),
            RV::Symbol(id) => inspect_symbol(id),
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
            RV::Float(f) => ruby_float_to_s(f),
            RV::Complex(_) => self.as_complex().debug(store),
            RV::Rational(r) => r.inspect(),
            RV::Symbol(id) => inspect_symbol(id),
            RV::String(s) => format!(r#""{}""#, s.inspect()),
            RV::Object(rvalue) => rvalue.debug(store),
        };
        Some(s)
    }

    pub fn debug_tos(&self, store: &Store) -> String {
        let s = match self.unpack() {
            RV::Nil => "".to_string(),
            RV::Symbol(id) => id.to_string(),
            RV::String(s) => String::from_utf8_lossy(s.as_bytes()).into_owned(),
            RV::Complex(_) => self.as_complex().to_s_str(store),
            RV::Rational(r) => r.to_s(),
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

    pub(crate) fn inspect_inner(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        // Only track heap-allocated objects for recursion detection.
        // Packed values (Fixnum, Flonum, Symbol, nil, true, false) cannot
        // form reference cycles.
        if !self.is_packed_value() {
            if !set.insert(self.id()) {
                return match self.ty() {
                    Some(ObjTy::HASH) => {
                        if self.rvalue().class() == SET_CLASS {
                            "Set[...]".to_string()
                        } else {
                            "{...}".to_string()
                        }
                    }
                    Some(ObjTy::ARRAY) => "[...]".to_string(),
                    _ => "...".to_string(),
                };
            }
        }
        let s = match self.unpack() {
            RV::Object(rvalue) => rvalue.inspect(store, set),
            _ => self.debug(store),
        };
        // Remove from set after processing so sibling references to the
        // same object are not falsely detected as recursion.
        if !self.is_packed_value() {
            set.remove(&self.id());
        }
        s
    }
}

///
/// Emit the CRuby-compatible deprecation warning for mutating a chilled
/// string (one returned by `Symbol#to_s`). Gated by `Warning[:deprecated]`;
/// writes to Ruby-level `$stderr` so that mspec's `complain` matcher can
/// capture it. The chilled flag should be cleared on the caller *before*
/// invoking this helper so that failing tests still leave the string in a
/// consistent state.
///
pub(crate) fn emit_chilled_string_mutation_warning(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
) -> Result<()> {
    // Check Warning[:deprecated].
    let warning_val = match globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Warning"))
    {
        Some(v) => v,
        None => return Ok(()),
    };
    let dep_sym = Value::symbol(IdentId::get_id("deprecated"));
    let dep = vm.invoke_method_inner(
        globals,
        IdentId::get_id("[]"),
        warning_val,
        &[dep_sym],
        None,
        None,
    )?;
    if dep.is_nil() || dep == Value::bool(false) {
        return Ok(());
    }

    // The current bytes of a chilled string still carry the original
    // symbol name (monoruby only produces chilled strings from
    // Symbol#to_s, and the flag is cleared on first mutation). Re-intern
    // so the message shows the same form CRuby would have printed.
    let sym = match self_val.is_rstring() {
        Some(s) => {
            let bytes = s.as_bytes();
            match std::str::from_utf8(bytes) {
                Ok(utf8) => IdentId::get_id(utf8),
                Err(_) => IdentId::get_id_from_bytes(bytes.to_vec()),
            }
        }
        None => return Ok(()),
    };
    let msg = format!(
        "warning: string returned by {}.to_s will be frozen in the future\n",
        inspect_symbol(sym)
    );
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or(Value::nil());
    if stderr.is_nil() {
        return Ok(());
    }
    let msg_val = Value::string(msg);
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[msg_val],
        None,
        None,
    )?;
    Ok(())
}

///
/// Emit a `warning: <msg>` line on Ruby-level `$stderr`. Used by
/// builtins that need to surface verbose-only warnings (e.g.
/// `Module#attr` with a boolean argument). The caller is responsible
/// for the `$VERBOSE`/`Warning[:...]` gating; this helper just formats
/// the message and writes it through `$stderr.write` so mspec's
/// `should complain` matcher catches the output.
///
pub(crate) fn emit_verbose_warning(
    vm: &mut Executor,
    globals: &mut Globals,
    msg: &str,
) -> Result<()> {
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or(Value::nil());
    if stderr.is_nil() {
        return Ok(());
    }
    let line = format!("warning: {msg}\n");
    let msg_val = Value::string(line);
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[msg_val],
        None,
        None,
    )?;
    Ok(())
}

///
/// Emit the CRuby-compatible "constant ... is deprecated" warning when
/// reading a constant marked via `Module#deprecate_constant`. Gated by
/// `Warning[:deprecated]`; writes to Ruby-level `$stderr` so mspec's
/// `should complain` matcher captures it. `qualified_name` is the full
/// `OwnerName::ConstantName` path that CRuby includes in the message.
///
pub(crate) fn emit_deprecated_constant_warning(
    vm: &mut Executor,
    globals: &mut Globals,
    qualified_name: &str,
) -> Result<()> {
    // Check Warning[:deprecated]. If the Warning module isn't loaded
    // yet (during bootstrap) we silently skip — CRuby behaves the same.
    let warning_val = match globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Warning"))
    {
        Some(v) => v,
        None => return Ok(()),
    };
    let dep_sym = Value::symbol(IdentId::get_id("deprecated"));
    let dep = vm.invoke_method_inner(
        globals,
        IdentId::get_id("[]"),
        warning_val,
        &[dep_sym],
        None,
        None,
    )?;
    if dep.is_nil() || dep == Value::bool(false) {
        return Ok(());
    }

    let msg = format!("warning: constant {qualified_name} is deprecated\n");
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or(Value::nil());
    if stderr.is_nil() {
        return Ok(());
    }
    let msg_val = Value::string(msg);
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[msg_val],
        None,
        None,
    )?;
    Ok(())
}

///
/// Render a symbol as its Ruby inspect form (`:name` or `:"escaped name"`).
///
/// Simple names — identifiers, operators, `$gv`/`@iv`/`@@cv` — are emitted
/// without quotes. Anything else is wrapped in `:"..."` and escaped through
/// `RStringInner::inspect`. This matches CRuby's `rb_sym_inspect` behavior and
/// is shared between `Symbol#inspect`, `Value::debug`, and the
/// `Symbol#to_s`-chilled-string deprecation warning.
///
pub(crate) fn inspect_symbol(id: IdentId) -> String {
    let ident_name = id.get_ident_name_clone();
    if is_simple_symbol(&ident_name) {
        let mut res = String::from(":");
        match &ident_name {
            IdentName::Utf8(name) => res.push_str(name),
            IdentName::Bytes(bytes) => res.push_str(&String::from_utf8_lossy(bytes)),
        }
        return res;
    }
    let (bytes, enc) = match &ident_name {
        IdentName::Utf8(s) => {
            let enc = if s.is_ascii() {
                Encoding::UsAscii
            } else {
                Encoding::Utf8
            };
            (s.as_bytes(), enc)
        }
        IdentName::Bytes(b) => (b.as_slice(), Encoding::Ascii8),
    };
    let inner = RStringInner::from_encoding(bytes, enc);
    let mut res = String::from(":\"");
    res.push_str(&inner.inspect());
    res.push('"');
    res
}

/// Test whether a symbol name can be rendered without surrounding quotes.
fn is_simple_symbol(name: &IdentName) -> bool {
    let s = match name.as_str() {
        Some(s) => s,
        None => return false,
    };
    if s.is_empty() {
        return false;
    }
    if is_operator_symbol(s) {
        return true;
    }
    if let Some(rest) = s.strip_prefix("@@") {
        return is_plain_identifier(rest);
    }
    if let Some(rest) = s.strip_prefix('@') {
        return is_plain_identifier(rest);
    }
    if let Some(rest) = s.strip_prefix('$') {
        return is_global_var_tail(rest);
    }
    is_method_identifier(s)
}

fn is_operator_symbol(s: &str) -> bool {
    matches!(
        s,
        "|" | "^"
            | "&"
            | "<=>"
            | "=="
            | "==="
            | "=~"
            | ">"
            | ">="
            | "<"
            | "<="
            | "<<"
            | ">>"
            | "+"
            | "-"
            | "*"
            | "/"
            | "%"
            | "**"
            | "~"
            | "+@"
            | "-@"
            | "[]"
            | "[]="
            | "`"
            | "!"
            | "!="
            | "!~"
    )
}

fn is_ident_start_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || (c as u32) >= 0x80
}

fn is_ident_cont_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric() || (c as u32) >= 0x80
}

fn is_plain_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if is_ident_start_char(c) => {}
        _ => return false,
    }
    chars.all(is_ident_cont_char)
}

fn is_method_identifier(s: &str) -> bool {
    let bytes = s.as_bytes();
    let body_end = match bytes.last() {
        Some(b'!') | Some(b'?') | Some(b'=') => bytes.len() - 1,
        _ => bytes.len(),
    };
    if body_end == 0 {
        return false;
    }
    is_plain_identifier(&s[..body_end])
}

fn is_global_var_tail(rest: &str) -> bool {
    if rest.is_empty() {
        return false;
    }
    // $1234: digits only
    if rest.bytes().all(|b| b.is_ascii_digit()) {
        return true;
    }
    // $-X: dash + exactly one identifier-ish char
    if let Some(after_dash) = rest.strip_prefix('-') {
        let mut chars = after_dash.chars();
        if let (Some(c), None) = (chars.next(), chars.next()) {
            return is_ident_cont_char(c);
        }
        return false;
    }
    // Single-character special globals like $~, $*, $$, $?, $!, ...
    if rest.len() == 1 {
        let c = rest.as_bytes()[0];
        if matches!(
            c,
            b'~' | b'*'
                | b'$'
                | b'?'
                | b'!'
                | b'@'
                | b'/'
                | b'\\'
                | b';'
                | b','
                | b'.'
                | b'<'
                | b'>'
                | b':'
                | b'"'
                | b'&'
                | b'\''
                | b'`'
                | b'+'
                | b'='
        ) {
            return true;
        }
    }
    // $name: plain identifier, no !/?/= suffix allowed
    is_plain_identifier(rest)
}

fn coerce_to_rstring_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    methods: &[IdentId],
) -> Result<RString> {
    if let Some(s) = recv.is_rstring() {
        return Ok(s);
    }
    for &method in methods {
        if let Some(func_id) = globals.check_method(recv, method) {
            let result = vm.invoke_func_inner(globals, func_id, recv, &[], None, None)?;
            if let Some(s) = result.is_rstring() {
                return Ok(s);
            }
            return Err(MonorubyErr::cant_convert_error(
                globals, recv, result, "String", method,
            ));
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into String",
        recv.get_real_class_name(&globals.store)
    )))
}

impl Value {
    ///
    /// Check if `self` is a packed value.
    ///
    pub fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    /// Return `Some(Immediate)` if `self` is a packed (immediate) value.
    pub fn is_immediate(&self) -> Option<Immediate> {
        if self.is_packed_value() {
            Some(Immediate(self.0))
        } else {
            None
        }
    }

    ///
    /// Check if `self` is an immediate, Float or Range object.
    ///
    pub fn is_frozen_literal(&self) -> bool {
        self.is_packed_value()
            || self.is_float()
            || self.is_range().is_some()
            || self.class() == COMPLEX_CLASS
        // Bignum is excluded
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

    pub fn is_integer(&self) -> bool {
        match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => true,
            _ => false,
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

    pub fn try_rational(&self) -> Option<&RationalInner> {
        if self.ty()? == ObjTy::RATIONAL {
            // SAFETY: The type check ensures this RValue contains a Rational.
            Some(unsafe { self.rvalue().as_rational() })
        } else {
            None
        }
    }

    pub fn as_rational(&self) -> &RationalInner {
        assert_eq!(Some(ObjTy::RATIONAL), self.ty());
        // SAFETY: The assert ensures this RValue contains a Rational.
        unsafe { self.rvalue().as_rational() }
    }

    // https://github.com/ruby/ruby/blob/3251792f491bd6f8bff71c6fd3352f66ac635902/range.c#L357
    pub fn is_linear(&self) -> bool {
        match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) | RV::Complex(_) => true,
            RV::Object(rv) if rv.ty() == ObjTy::TIME => true,
            _ => false,
        }
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
        } else if let Some(fid) = globals.check_method(*self, IdentId::TO_ARY) {
            let result = vm.invoke_func_inner(globals, fid, *self, &[], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                return Ok(ary);
            }
            return Err(MonorubyErr::cant_convert_error_ary(globals, *self, result));
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
    /// Try conversion from `self` to i64 using `to_int`.
    ///
    /// - if `self` is a Fixnum, return it as i64.
    /// - if `self` is a Bignum, return it as i64 if it fits, otherwise return RangeError.
    /// - if `self` responds to `to_int`, call it and try conversion on the result.
    /// - otherwise, return TypeError.
    ///
    pub(crate) fn coerce_to_int_i64(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<i64> {
        let res = match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => *self,
            _ => self.coerce_to_int(vm, globals)?,
        };
        match res.unpack() {
            RV::Fixnum(i) => return Ok(i),
            RV::BigInt(b) => {
                use num::ToPrimitive;
                return b
                    .to_i64()
                    .ok_or_else(|| MonorubyErr::rangeerr("bignum too big to convert into `long'"));
            }
            _ => unreachable!(),
        };
    }

    /// Coerce to u64 for Array#pack. Extracts the low 64 bits from BigInts
    /// (matching CRuby's truncation semantics) instead of raising RangeError.
    pub(crate) fn coerce_to_pack_u64(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<u64> {
        let res = match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => *self,
            RV::Float(f) => {
                let i = f as i64;
                return Ok(i as u64);
            }
            _ => self.coerce_to_int(vm, globals)?,
        };
        match res.unpack() {
            RV::Fixnum(i) => Ok(i as u64),
            RV::BigInt(b) => {
                // Extract low 64 bits from the BigInt's magnitude,
                // then apply sign (two's complement).
                let (sign, digits) = b.to_u64_digits();
                let abs_low = digits.first().copied().unwrap_or(0);
                Ok(if sign == num::bigint::Sign::Minus {
                    (abs_low as i64).wrapping_neg() as u64
                } else {
                    abs_low
                })
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn coerce_to_int(&self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        // First try direct method lookup (fast path), then fall back to
        // invoke_method_inner which handles method_missing.
        let result = if let Some(func_id) = globals.check_method(*self, IdentId::TO_INT) {
            vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?
        } else {
            // Try via method_missing (for Mock objects etc.)
            match vm.invoke_method_inner(globals, IdentId::TO_INT, *self, &[], None, None) {
                Ok(result) => result,
                Err(_) => {
                    return Err(MonorubyErr::no_implicit_conversion(
                        globals,
                        *self,
                        INTEGER_CLASS,
                    ));
                }
            }
        };
        match result.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => Ok(result),
            _ => Err(MonorubyErr::cant_convert_error_int(globals, *self, result)),
        }
    }

    /// Convert `self` to Integer (Fixnum or BigInt) for sprintf.
    ///
    /// Tries direct conversion first, then to_int, then to_i as fallback.
    /// Accepts Float (truncated), String (parsed).
    /// Raises TypeError if no conversion is possible.
    pub(crate) fn coerce_to_integer(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<IntegerBase> {
        match self.unpack() {
            RV::Fixnum(i) => return Ok(IntegerBase::Fixnum(i)),
            RV::BigInt(b) => return Ok(IntegerBase::BigInt(b.clone())),
            RV::Float(f) => {
                let t = f.trunc();
                return if i64::MIN as f64 <= t && t <= i64::MAX as f64 {
                    Ok(IntegerBase::Fixnum(t as i64))
                } else {
                    use num::FromPrimitive;
                    Ok(IntegerBase::BigInt(
                        num::BigInt::from_f64(t).expect("float is not NaN or infinite"),
                    ))
                };
            }
            RV::String(s) => {
                let s = s.check_utf8()?;
                if let Ok(i) = s.parse::<i64>() {
                    return Ok(IntegerBase::Fixnum(i));
                } else if let Ok(b) = s.parse::<num::BigInt>() {
                    return Ok(IntegerBase::BigInt(b));
                }
            }
            _ => {}
        };
        // Try to_int first, then to_i as fallback (matching CRuby sprintf behavior).
        if let Ok(i) = self.coerce_to_int_i64(vm, globals) {
            return Ok(IntegerBase::Fixnum(i));
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::get_id("to_i")) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            match result.unpack() {
                RV::Fixnum(i) => return Ok(IntegerBase::Fixnum(i)),
                RV::BigInt(b) => return Ok(IntegerBase::BigInt(b.clone())),
                _ => {}
            }
        }
        Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Integer",
            self.get_real_class_name(&globals.store)
        )))
    }

    /// Convert `self` to f64 for sprintf.
    ///
    /// Tries direct conversion first, then to_f.
    /// Accepts Fixnum, Float, String (parsed).
    /// Raises TypeError if no conversion is possible.
    pub(crate) fn coerce_to_float(&self, vm: &mut Executor, globals: &mut Globals) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => return Ok(i as f64),
            RV::Float(f) => return Ok(f),
            RV::String(s) => {
                let s = s.check_utf8()?;
                return s.parse::<f64>().map_err(|_| {
                    MonorubyErr::argumenterr(format!("invalid value for Float(): \"{}\"", s))
                });
            }
            _ => {}
        };
        if let Ok(f) = self.coerce_to_f64(vm, globals) {
            return Ok(f);
        }
        Err(MonorubyErr::cant_convert_into_float(globals, *self))
    }

    ///
    /// Try to convert `self` to i64.
    ///
    /// - if `self` is a Fixnum or a Bignum, convert it to f64.
    /// - if `self` is a Float, return it as f64.
    ///
    /// Convert to f64 from numeric types only (Fixnum/Float/BigInt).
    /// Does NOT call to_f. Use coerce_to_f64() for the full version.
    pub fn coerce_to_f64_no_convert(&self, store: &Store) -> Result<f64> {
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

    /// Convert to f64, calling `to_f` if the value is not a numeric type.
    /// This is the standard coercion method matching CRuby behavior.
    pub(crate) fn coerce_to_f64(&self, vm: &mut Executor, globals: &mut Globals) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i as f64),
            RV::Float(f) => Ok(f),
            RV::BigInt(b) => {
                if let Some(f) = b.to_f64() {
                    Ok(f)
                } else {
                    Err(MonorubyErr::cant_convert_into_float(&globals.store, *self))
                }
            }
            _ => {
                if self.is_str().is_none()
                    && let Some(func_id) = globals.check_method(*self, IdentId::TO_F)
                {
                    let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
                    match result.unpack() {
                        RV::Float(f) => Ok(f),
                        RV::Fixnum(i) => Ok(i as f64),
                        _ => Err(MonorubyErr::cant_convert_error_f(globals, *self, result)),
                    }
                } else {
                    Err(MonorubyErr::no_implicit_conversion(
                        &globals.store,
                        *self,
                        FLOAT_CLASS,
                    ))
                }
            }
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
        self.try_hash_ty().expect(&format!(
            "Value expected to be a hash but was {:?}",
            self.ty()
        ))
    }

    pub(crate) fn as_hashmap_inner(&self) -> &HashmapInner {
        assert_eq!(ObjTy::HASH, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a hash.
        unsafe { self.rvalue().as_hashmap() }
    }

    /// Read-only access to the slot array of a `Struct` subclass
    /// instance. Asserts the receiver is a STRUCT-typed RValue.
    #[allow(dead_code)]
    pub(crate) fn as_struct(&self) -> &StructInner {
        assert_eq!(ObjTy::STRUCT, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue is a Struct instance.
        unsafe { self.rvalue().as_struct_inner() }
    }

    /// Mutable access to the slot array of a `Struct` subclass instance.
    pub(crate) fn as_struct_mut(&mut self) -> &mut StructInner {
        assert_eq!(ObjTy::STRUCT, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue is a Struct instance.
        unsafe { self.rvalue_mut().as_struct_inner_mut() }
    }

    /// Returns Some(&StructInner) if `self` is a Struct instance, else None.
    pub(crate) fn try_struct(&self) -> Option<&StructInner> {
        if let Some(rv) = self.try_rvalue() {
            if rv.ty() == ObjTy::STRUCT {
                // SAFETY: ty() check ensures STRUCT variant.
                return Some(unsafe { rv.as_struct_inner() });
            }
        }
        None
    }

    pub(crate) fn as_hashmap_inner_mut(&mut self) -> &mut HashmapInner {
        assert_eq!(ObjTy::HASH, self.rvalue().ty());
        // SAFETY: The assert ensures this RValue contains a hash.
        unsafe { self.rvalue_mut().as_hashmap_mut() }
    }

    /// Coerce the value to a Hashmap by trying to_hash if necessary.
    pub(crate) fn coerce_to_hash(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Hashmap> {
        if let Some(h) = self.try_hash_ty() {
            return Ok(h);
        }
        let to_hash_id = IdentId::get_id("to_hash");
        if let Some(result) =
            vm.invoke_method_if_exists(globals, to_hash_id, *self, &[], None, None)?
        {
            if let Some(h) = result.try_hash_ty() {
                return Ok(h);
            }
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {} into Hash ({}#to_hash gives {})",
                self.get_real_class_name(globals),
                self.get_real_class_name(globals),
                result.get_real_class_name(globals),
            )));
        }
        Err(MonorubyErr::no_implicit_conversion(
            &globals.store,
            *self,
            HASH_CLASS,
        ))
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

    pub(crate) fn is_umethod(&self) -> Option<&UMethodInner> {
        if let Some(rvalue) = self.try_rvalue() {
            match rvalue.ty() {
                ObjTy::UMETHOD => Some(self.rvalue().as_umethod()),
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

    /// Like [`expect_symbol_or_string`], but additionally tries `#to_str`
    /// coercion for non-Symbol/non-String values, matching CRuby's
    /// `rb_check_id` semantics for `attr_*` and similar APIs.
    ///
    /// - Symbol or String: return the corresponding `IdentId` directly.
    /// - Otherwise: invoke `#to_str` on the receiver. If it returns a
    ///   String, intern that. If it returns anything else (or doesn't
    ///   exist), raise TypeError.
    pub(crate) fn coerce_to_symbol_or_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<IdentId> {
        if let Some(id) = self.try_symbol_or_string() {
            return Ok(id);
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::TO_STR) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            if let Some(s) = result.is_str() {
                return Ok(IdentId::get_id(s));
            }
            // `#to_str` returned a non-String value -> TypeError, matching
            // CRuby's "can't convert X to String (X#to_str gives Y)".
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {} to String ({}#to_str gives {})",
                self.get_real_class_name(&globals.store),
                self.get_real_class_name(&globals.store),
                result.get_real_class_name(&globals.store),
            )));
        }
        Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, *self))
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
        coerce_to_rstring_inner(vm, globals, *self, &[IdentId::TO_STR])
    }

    pub(crate) fn coerce_to_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<String> {
        Ok(self.coerce_to_rstring(vm, globals)?.to_str()?.to_string())
    }

    pub(crate) fn coerce_to_str(&self, vm: &mut Executor, globals: &mut Globals) -> Result<String> {
        self.coerce_to_string(vm, globals)
    }

    /// Call Ruby-level `to_s` (explicit conversion) on the value.
    /// This is used by sprintf %s to match CRuby behavior.
    pub(crate) fn coerce_to_s(&self, vm: &mut Executor, globals: &mut Globals) -> Result<String> {
        if let Some(s) = self.is_str() {
            return Ok(s.to_string());
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::TO_S) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            if let Some(s) = result.is_str() {
                return Ok(s.to_string());
            }
        }
        Ok(self.to_s(&globals.store))
    }

    pub(crate) fn coerce_to_path_rstring(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        coerce_to_rstring_inner(vm, globals, *self, &[IdentId::TO_STR, IdentId::TO_PATH])
    }

    /// Like `expect_regexp_or_string` but tries `to_str` coercion for
    /// non-string/non-regexp values.
    pub(crate) fn coerce_to_regexp_or_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Regexp> {
        if let Some(re) = self.is_regex() {
            Ok(re)
        } else if let Some(s) = self.is_str() {
            Ok(Regexp::new_unchecked(Value::regexp(
                RegexpInner::with_option(s, 0)?,
            )))
        } else {
            // Try to_str coercion
            if let Some(func_id) = globals.check_method(*self, IdentId::TO_STR) {
                let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
                if let Some(s) = result.is_str() {
                    return Ok(Regexp::new_unchecked(Value::regexp(
                        RegexpInner::with_option(s, 0)?,
                    )));
                }
            }
            Err(MonorubyErr::is_not_regexp_nor_string(&globals.store, *self))
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
        // Panics if the string contains invalid UTF-8. Prefer expect_str()
        // for user-facing code paths.
        unsafe {
            self.rvalue()
                .as_str()
                .expect("as_str called on non-UTF-8 string")
        }
    }

    pub(crate) fn is_str(&self) -> Option<&str> {
        let rv = self.try_rvalue()?;
        // SAFETY: The type check ensures this RValue contains a string.
        // Returns None if the value is not a String OR if it contains
        // invalid UTF-8 byte sequences.
        unsafe {
            match rv.ty() {
                ObjTy::STRING => rv.as_str(),
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
                assert_eq!(1, stmts.len(), "multiple statements {stmts:?}");
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
            NodeKind::Rational(n, d) => Value::rational_from_bigint(n.clone(), d.clone()),
            NodeKind::RImaginary(n, d) => {
                let f = n.to_f64().unwrap_or(f64::INFINITY) / d.to_f64().unwrap_or(f64::INFINITY);
                Value::complex(0, f)
            }
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::symbol_from_str(sym),
            NodeKind::String(s) => Value::string_from_str(s),
            NodeKind::Bytes(b) => Value::string_from_source_bytes(b),
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
            NodeKind::Hash(v, _splat) => {
                let mut map = RubyMap::default();
                for (k, v) in v.iter() {
                    let k = Self::from_ast_inner(k, vm, globals);
                    let v = Self::from_ast_inner(v, vm, globals);
                    map.insert(k, v, vm, globals).unwrap();
                }
                Value::hash(map)
            }
            NodeKind::BinOp(ruruby_parse::BinOp::Div, box lhs, box rhs) => {
                // CRuby's `p` outputs rationals as `(num/den)`, which parses as BinOp(Div).
                match (&lhs.kind, &rhs.kind) {
                    (NodeKind::Integer(n), NodeKind::Integer(d)) => Value::rational(*n, *d),
                    (NodeKind::UnOp(ruruby_parse::UnOp::Neg, box inner), NodeKind::Integer(d)) => {
                        if let NodeKind::Integer(n) = &inner.kind {
                            Value::rational(-n, *d)
                        } else {
                            unreachable!("{:?}", node.kind)
                        }
                    }
                    _ => unreachable!("{:?}", node.kind),
                }
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
            NodeKind::Rational(n, d) => Value::rational_from_bigint(n.clone(), d.clone()),
            NodeKind::RImaginary(n, d) => {
                let f = n.to_f64().unwrap_or(f64::INFINITY) / d.to_f64().unwrap_or(f64::INFINITY);
                Value::complex(0, f)
            }
            NodeKind::Bool(b) => Value::bool(*b),
            NodeKind::Nil => Value::nil(),
            NodeKind::Symbol(sym) => Value::symbol_from_str(sym),
            NodeKind::String(s) => Value::string_from_str(s),
            NodeKind::Bytes(b) => Value::string_from_source_bytes(b),
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

    ///
    /// ## Safety: Caller must ensure `u` is not zero.
    ///
    const unsafe fn from_u64_unchecked(u: u64) -> Immediate {
        Immediate(unsafe { std::num::NonZeroU64::new_unchecked(u) })
    }

    pub const fn nil() -> Self {
        unsafe { Immediate::from_u64_unchecked(NIL_VALUE) }
    }

    pub const fn bool(b: bool) -> Self {
        unsafe { Immediate::from_u64_unchecked(if b { TRUE_VALUE } else { FALSE_VALUE }) }
    }

    fn fixnum(num: i64) -> Self {
        unsafe { Immediate::from_u64_unchecked((num << 1) as u64 | 0b1) }
    }

    pub fn symbol(id: IdentId) -> Self {
        unsafe { Immediate::from_u64_unchecked((id.get() as u64) << 32 | TAG_SYMBOL) }
    }

    pub fn flonum(num: f64) -> Option<Self> {
        if num == 0.0 {
            // -0.0 == 0.0 in IEEE 754, but Ruby distinguishes them.
            // Store -0.0 as a heap float to preserve the sign bit.
            if num.is_sign_negative() {
                return None;
            }
            return Some(unsafe { Self::from_u64_unchecked(FLOAT_ZERO) });
        }
        let unum = f64::to_bits(num);
        let exp = ((unum >> 60) & 0b111) + 1;
        if (exp & 0b0110) == 0b0100 {
            Some(unsafe { Self::from_u64_unchecked(((unum.rotate_left(3)) & !1) | 2) })
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

    pub fn try_fixnum(self) -> Option<Fixnum> {
        let i = (*self).try_fixnum()?;
        Some(Fixnum(i))
    }

    pub fn try_flonum(self) -> Option<Flonum> {
        let f = (*self).try_float()?;
        Some(Flonum(f))
    }
}

/// A Value that is guaranteed to be a Fixnum (i63 integer).
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Fixnum(i64);

impl Fixnum {
    pub fn get(&self) -> i64 {
        self.0
    }
}

impl From<Fixnum> for Immediate {
    fn from(f: Fixnum) -> Self {
        Immediate::fixnum(f.0)
    }
}

impl From<Fixnum> for Value {
    fn from(f: Fixnum) -> Self {
        Value::fixnum(f.0)
    }
}

/// A Value that is guaranteed to be a Flonum (inline-encoded f64).
#[derive(Clone, Copy, PartialEq)]
pub struct Flonum(f64);

impl Flonum {
    pub fn get(&self) -> f64 {
        self.0
    }
}

impl From<Flonum> for Immediate {
    fn from(f: Flonum) -> Self {
        Immediate::flonum(f.0).unwrap()
    }
}

impl From<Flonum> for Value {
    fn from(f: Flonum) -> Self {
        Value::float(f.0)
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
    Rational(&'a RationalInner),
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
            RV::Float(n) => write!(f, "{}", ruby_float_to_s(*n)),
            RV::Complex(c) => write!(f, "{:?}", c),
            RV::Rational(r) => write!(f, "{}", r.inspect()),
            RV::Symbol(id) => write!(f, ":{}", id),
            RV::String(s) => write!(f, "\"{}\"", String::from_utf8_lossy(s.as_bytes())),
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
            RV::Float(n) => write!(f, "{}", ruby_float_to_s(*n)),
            RV::Complex(c) => write!(f, "{}", c),
            RV::Rational(r) => write!(f, "{}", r.to_s()),
            RV::Symbol(id) => write!(f, ":{}", id),
            RV::String(s) => write!(f, "\"{}\"", String::from_utf8_lossy(s.as_bytes())),
            RV::Object(rvalue) => write!(f, "{rvalue:?}"),
        }
    }
}
