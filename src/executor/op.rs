use super::*;

use num::{BigInt, Integer, ToPrimitive, Zero};
use paste::paste;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

//
// Generic operations.
//

macro_rules! binop_values {
    (($op:ident, $op_str:expr)) => {
        paste! {
            pub(super) extern "C" fn [<$op _values>](
                interp: &mut Interp,
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

binop_values!(
    (add, IdentId::_ADD),
    (sub, IdentId::_SUB),
    (mul, IdentId::_MUL)
);

/*pub(super) extern "C" fn add_values(
    interp: &mut Interp,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    let v = match (lhs.unpack(), rhs.unpack()) {
        (RV::Integer(lhs), RV::Integer(rhs)) => match lhs.checked_add(rhs) {
            Some(res) => Value::new_integer(res),
            None => Value::new_bigint(BigInt::from(lhs).add(BigInt::from(rhs))),
        },
        (RV::BigInt(lhs), RV::Integer(rhs)) => Value::new_bigint(lhs.add(BigInt::from(rhs))),
        (RV::Float(lhs), RV::Integer(rhs)) => Value::new_float(lhs.add(&(rhs as f64))),

        (RV::Integer(lhs), RV::BigInt(rhs)) => Value::new_bigint(BigInt::from(lhs).add(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => Value::new_bigint(lhs.add(rhs)),
        (RV::Float(lhs), RV::BigInt(rhs)) => Value::new_float(lhs.add(rhs.to_f64().unwrap())),

        (RV::Integer(lhs), RV::Float(rhs)) => Value::new_float((lhs as f64).add(&rhs)),
        (RV::BigInt(lhs), RV::Float(rhs)) => Value::new_float(lhs.to_f64().unwrap().add(&rhs)),
        (RV::Float(lhs), RV::Float(rhs)) => Value::new_float(lhs.add(&rhs)),
        /*(RV::String(lhs), RV::String(rhs)) => Value::new_string_from_smallvec({
            let mut b = lhs.clone();
            b.extend_from_slice(rhs);
            b
        }),*/
        _ => {
            return interp.invoke_method(globals, IdentId::_ADD, lhs, &[rhs]);
        }
    };
    Some(v)
}*/

pub(super) extern "C" fn div_values(
    interp: &mut Interp,
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
                interp: &mut Interp,
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
    interp: &mut Interp,
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
    interp: &mut Interp,
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
    interp: &mut Interp,
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
    let mut res = vec![];
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        res.extend(v.to_s(globals).bytes());
    }
    Value::new_string(res)
}

pub extern "C" fn vm_get_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    let ConstSiteInfo {
        name,
        prefix,
        toplevel,
        cache: (cached_version, val),
    } = globals.func[site_id].clone();
    assert_eq!(0, prefix.len());
    assert!(!toplevel);
    if cached_version == const_version {
        return val;
    };
    let res = match globals.get_constant(name) {
        Some(v) => Some(v),
        None => {
            globals.err_uninitialized_constant(name);
            None
        }
    };
    globals.func[site_id].cache = (const_version, res);
    res
}

pub extern "C" fn get_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    site_id: ConstSiteId,
) -> Option<Value> {
    let ConstSiteInfo { name, .. } = globals.func[site_id].clone();
    let res = match globals.get_constant(name) {
        Some(v) => Some(v),
        None => {
            globals.err_uninitialized_constant(name);
            None
        }
    };
    res
}

pub extern "C" fn set_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
    const_version: &mut usize,
) {
    *const_version += 1;
    if globals.set_constant(name, val).is_some() && globals.warning >= 1 {
        eprintln!(
            "warning: already initialized constant {}",
            globals.get_ident_name(name)
        )
    }
}

pub extern "C" fn _dump_stacktrace(interp: &mut Interp, globals: &mut Globals, mut bp: *const u64) {
    for i in 0..16 {
        eprint!("[{}]:", i);
        let prev_bp = unsafe { *bp as *const u64 };
        let ret_addr = unsafe { *bp.add(1) as *const u64 };
        eprintln!("ret adr: {:?} ", ret_addr);
        let meta = Meta::new(unsafe { *bp.sub(1) });
        let func_id = meta.func_id();
        eprintln!(
            " name:[{}] {:?}",
            globals.func[func_id]
                .name()
                .unwrap_or(&"<unnamed>".to_string()),
            meta,
        );
        let self_val = unsafe { Value::from(*bp.sub(2)) };
        eprint!(" self: {} ", globals.val_inspect(self_val));
        for r in 0..meta.reg_num() as usize {
            let v = unsafe { Value::from(*bp.sub(3 + r)) };
            eprint!("[{}]: {} ", r, globals.val_inspect(v));
        }
        eprintln!();
        if interp.codegen.entry_point_return.as_ptr() as u64 == ret_addr as u64 {
            break;
        }
        bp = prev_bp;
    }
}
