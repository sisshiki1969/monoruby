use super::*;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                interp: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => match lhs.[<checked_ $op>](rhs){
                        Some(res) => Value::new_integer(res),
                        None => Value::new_bigint(BigInt::from(lhs).$op(BigInt::from(rhs))),
                    }
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::new_bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::new_float(lhs.$op(&(rhs as f64))),

                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::new_bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::new_bigint(lhs.$op(rhs)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => Value::new_float(lhs.$op(rhs.to_f64().unwrap())),

                    (RV::Integer(lhs), RV::Float(rhs)) => Value::new_float((lhs as f64).$op(&rhs)),
                    (RV::BigInt(lhs), RV::Float(rhs)) => Value::new_float(lhs.to_f64().unwrap().$op(&rhs)),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::new_float(lhs.$op(&rhs)),
                    _ => {
                        return interp.invoke_method(globals, $op_str, lhs, &[rhs]);
                    }
                };
                Some(v)
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        binop_values!(($op1, $op_str1));
        binop_values!($(($op2, $op_str2)),+);
    };
}

pub(super) extern "C" fn pow_ii(lhs: i64, rhs: i64) -> Value {
    if let Ok(rhs) = i32::try_from(rhs) {
        if rhs < 0 {
            unimplemented!("a**b: b<0 in not supported yet.")
        }
        let rhs = rhs as u32;
        match lhs.checked_pow(rhs) {
            Some(res) => Value::new_integer(res),
            None => Value::new_bigint(BigInt::from(lhs).pow(rhs)),
        }
    } else {
        Value::new_float(f64::INFINITY)
    }
}

fn pow_ff(lhs: f64, rhs: f64) -> Value {
    Value::new_float(lhs.powf(rhs))
}

pub(super) extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

// TODO: support rhs < 0.
pub(super) extern "C" fn pow_values(
    interp: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => pow_ii(lhs, rhs),
        (RV::Integer(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::new_bigint(BigInt::from(lhs).pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::Integer(lhs), RV::Float(rhs)) => pow_ff(lhs as f64, rhs),
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if let Ok(rhs) = i32::try_from(rhs) {
                let rhs = rhs as u32;
                Value::new_bigint(lhs.pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if let Ok(rhs) = rhs.try_into() {
                Value::new_bigint(lhs.pow(rhs))
            } else {
                Value::new_float(f64::INFINITY)
            }
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => pow_ff(lhs.to_f64().unwrap(), rhs),
        (RV::Float(lhs), RV::Integer(rhs)) => {
            //if let Ok(rhs) = i32::try_from(rhs) {
            pow_ff(lhs, rhs as f64)
            //} else {
            //    Value::new_float(lhs.powf(rhs as f64))
            //}
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => pow_ff(lhs, rhs.to_f64().unwrap()),
        (RV::Float(lhs), RV::Float(rhs)) => pow_ff(lhs, rhs),
        _ => {
            return interp.invoke_method(globals, IdentId::_POW, lhs, &[rhs]);
        }
    };
    Some(v)
}

binop_values!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL),
    (rem, IdentId::_REM)
);

pub(super) extern "C" fn div_values(
    interp: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_integer(lhs.div_floor(rhs))
        }
        (RV::Integer(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(BigInt::from(lhs).div_floor(rhs))
        }
        (RV::Integer(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_float((lhs as f64).div(&rhs))
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(lhs.div_floor(&BigInt::from(rhs)))
        }
        (RV::BigInt(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_bigint(lhs.div_floor(rhs))
        }
        (RV::BigInt(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_float((lhs.to_f64().unwrap()).div(&rhs))
        }
        (RV::Float(lhs), RV::Integer(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_float(lhs.div(&(rhs as f64)))
        }
        (RV::Float(lhs), RV::BigInt(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_float(lhs.div(&rhs.to_f64().unwrap()))
        }
        (RV::Float(lhs), RV::Float(rhs)) => {
            if rhs.is_zero() {
                globals.err_divide_by_zero();
                return None;
            }
            Value::new_float(lhs.div(&rhs))
        }
        _ => {
            return interp.invoke_method(globals, IdentId::_DIV, lhs, &[rhs]);
        }
    };
    Some(v)
}

macro_rules! int_binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                interp: &mut Executor,
                globals: &mut Globals,
                lhs: Value,
                rhs: Value
            ) -> Option<Value> {
                let v = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::new_integer(lhs.$op(&rhs)),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => Value::new_bigint(BigInt::from(lhs).$op(rhs)),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => Value::new_bigint(lhs.$op(BigInt::from(rhs))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::new_bigint(lhs.$op(rhs)),
                    _ => {
                        return interp.invoke_method(globals, $op_str, lhs, &[rhs]);
                    }
                };
                Some(v)
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        int_binop_values!(($op1, $op_str1));
        int_binop_values!($(($op2, $op_str2)),+);
    };
}

int_binop_values!(
    (bitor, IdentId::_BOR),
    (bitand, IdentId::_BAND),
    (bitxor, IdentId::_BXOR)
);

pub(super) extern "C" fn shr_values(
    interp: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                int_shr(lhs, rhs as u64 as u32)
            } else {
                int_shl(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                bigint_shr(lhs, rhs as u64 as u32)
            } else {
                bigint_shl(lhs, -rhs as u64 as u32)
            }
        }
        _ => {
            return interp.invoke_method(globals, IdentId::_SHR, lhs, &[rhs]);
        }
    };
    Some(v)
}

pub(super) extern "C" fn shl_values(
    interp: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                int_shl(lhs, rhs as u64 as u32)
            } else {
                int_shr(lhs, -rhs as u64 as u32)
            }
        }
        (RV::BigInt(lhs), RV::Integer(rhs)) => {
            if rhs >= 0 {
                bigint_shl(lhs, rhs as u64 as u32)
            } else {
                bigint_shr(lhs, -rhs as u64 as u32)
            }
        }
        _ => {
            return interp.invoke_method(globals, IdentId::_SHL, lhs, &[rhs]);
        }
    };
    Some(v)
}

fn int_shr(lhs: i64, rhs: u32) -> Value {
    Value::new_integer(
        lhs.checked_shr(rhs)
            .unwrap_or(if lhs >= 0 { 0 } else { -1 }),
    )
}

fn int_shl(lhs: i64, rhs: u32) -> Value {
    match lhs.checked_shl(rhs) {
        Some(res) => Value::new_integer(res),
        None => bigint_shl(&BigInt::from(lhs), rhs),
    }
}

fn bigint_shr(lhs: &BigInt, rhs: u32) -> Value {
    Value::new_bigint(lhs.shr(rhs))
}

fn bigint_shl(lhs: &BigInt, rhs: u32) -> Value {
    Value::new_bigint(lhs.shl(rhs))
}

macro_rules! cmp_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (lhs, rhs) => unreachable!("{:?} cmp {:?}", lhs, rhs),
                };
                Value::bool(b)
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_values!($op1);
        cmp_values!($($op2),+);
    };
}

cmp_values!(ge, gt, le, lt);

macro_rules! eq_values {
    ($op:ident) => {
        paste! {
            pub(super) extern "C" fn [<cmp_ $op _values>](lhs: Value, rhs: Value) -> Value {
                let b = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => lhs.$op(&rhs),
                    (RV::Integer(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).$op(&rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => (lhs as f64).$op(&rhs),
                    (RV::BigInt(lhs), RV::Integer(rhs)) => lhs.$op(&BigInt::from(rhs)),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.$op(&rhs),
                    (RV::BigInt(lhs), RV::Float(rhs)) => lhs.to_f64().unwrap().$op(&rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => lhs.$op(&(rhs as f64)),
                    (RV::Float(lhs), RV::BigInt(rhs)) => lhs.$op(&(rhs.to_f64().unwrap())),
                    (RV::Float(lhs), RV::Float(rhs)) => lhs.$op(&rhs),
                    (RV::Bool(lhs), RV::Bool(rhs)) => lhs.$op(&rhs),
                    _ => unreachable!("{:?} {:?}", lhs, rhs),
                };
                Value::bool(b)
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        eq_values!($op1);
        eq_values!($($op2),+);
    };
}

eq_values!(eq, ne);

pub(super) extern "C" fn neg_value(
    interp: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
) -> Option<Value> {
    let v = match lhs.unpack() {
        RV::Integer(lhs) => match lhs.checked_neg() {
            Some(lhs) => Value::new_integer(lhs),
            None => Value::new_bigint(-BigInt::from(lhs)),
        },
        RV::Float(lhs) => Value::new_float(-lhs),
        RV::BigInt(lhs) => Value::new_bigint(-lhs),
        _ => {
            return interp.invoke_method(globals, IdentId::_UMINUS, lhs, &[]);
        }
    };
    Some(v)
}

pub extern "C" fn concatenate_string(globals: &Globals, arg: *mut Value, len: usize) -> Value {
    let mut res = String::new();
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        res += &v.to_s(globals);
    }
    Value::new_string(res)
}

pub extern "C" fn expand_array(src: Value, dst: *mut Value, len: usize) -> usize {
    match src.is_array() {
        Some(ary) => {
            if len <= ary.len() {
                for i in 0..len {
                    unsafe { *dst.sub(i) = ary[i] }
                }
                len
            } else {
                for i in 0..ary.len() {
                    unsafe { *dst.sub(i) = ary[i] }
                }
                for i in ary.len()..len {
                    unsafe { *dst.sub(i) = Value::nil() }
                }
                ary.len()
            }
        }
        None => {
            unsafe { *dst = src };
            1
        }
    }
}

pub extern "C" fn make_rest_array(src: *mut Value, len: usize) {
    let mut ary = vec![];
    for i in 0..len {
        unsafe { ary.push(*src.sub(i)) }
    }
    unsafe { *src = Value::new_array_from_vec(ary) };
}

pub extern "C" fn block_expand_array(src: Value, dst: *mut Value, min_len: usize) -> usize {
    let ary = src.as_array();
    let len = ary.len();
    if min_len <= len {
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] }
        }
        len
    } else {
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] }
        }
        for i in len..min_len {
            unsafe { *dst.sub(i) = Value::nil() }
        }
        min_len
    }
}

pub extern "C" fn vm_get_constant(
    interp: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
    _: Option<Value>,
) -> Option<Value> {
    let (cached_version, val) = &globals.func[site_id].cache;
    if *cached_version == const_version {
        return *val;
    };
    let res = interp.find_constant(globals, site_id);
    if res.is_some() {
        globals.func[site_id].cache = (const_version, res)
    }
    res
}

///
/// Get Constant.
///
/// rax: Option<Value>
///
pub extern "C" fn get_constant(
    interp: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
) -> Option<Value> {
    interp.find_constant(globals, site_id)
}

pub extern "C" fn set_constant(
    interp: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
) {
    interp.set_constant(globals, name, val)
}

pub extern "C" fn define_method(
    interp: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) {
    let parent = interp.get_class_context();
    globals.add_method(parent, name, func);
}

pub extern "C" fn _dump_stacktrace(interp: &mut Executor, globals: &mut Globals) {
    let mut cfp = interp.cfp;
    eprintln!("-----begin stacktrace");
    for i in 0..16 {
        eprint!("  [{}]: cfp:{:?} ", i, cfp);
        let ret_addr = cfp.return_addr();
        eprintln!("ret adr: {:?} ", ret_addr);
        let prev_cfp = cfp.next();
        _dump_frame_info(interp, globals, cfp);
        if prev_cfp.is_null() {
            break;
        }
        cfp = prev_cfp;
    }
    eprintln!("-----end stacktrace");
}

fn _dump_frame_info(_interp: &mut Executor, globals: &mut Globals, cfp: CFP) {
    let bp = cfp.bp();
    let meta = Meta::new(unsafe { *bp.sub(OFFSET_META as usize / 8) as u64 });
    let outer = unsafe { *bp.sub(OFFSET_OUTER as usize / 8) };
    let func_id = meta.func_id();
    eprintln!(
        "    name:[{}] outer:0x{:012x} {:?}",
        globals.func[func_id]
            .name()
            .unwrap_or(&"<unnamed>".to_string()),
        outer,
        meta,
    );
    eprint!("    ");
    for r in 0..meta.reg_num() as usize {
        let v = unsafe { Value::from(*bp.sub(OFFSET_SELF as usize / 8 + r) as u64) };
        eprint!(
            "%{}{}:[{}] ",
            r,
            if r == 0 { "(self)" } else { "" },
            globals.val_inspect(v)
        );
    }
    eprintln!();
}
