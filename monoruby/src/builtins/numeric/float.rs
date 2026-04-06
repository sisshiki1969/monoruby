use num::{BigInt, FromPrimitive, ToPrimitive};

use super::*;
use crate::executor::Visibility;

//
// Float class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class("Float", FLOAT_CLASS, numeric, OBJECT_CLASS, None);
    globals.set_constant_by_str(FLOAT_CLASS, "NAN", Value::float(f64::NAN));
    globals.set_constant_by_str(FLOAT_CLASS, "INFINITY", Value::float(f64::INFINITY));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX", Value::float(f64::MAX));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN", Value::float(f64::MIN_POSITIVE));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_10_EXP", Value::i32(f64::MAX_10_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MAX_EXP", Value::i32(f64::MAX_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN_10_EXP", Value::i32(f64::MIN_10_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "MIN_EXP", Value::i32(f64::MIN_EXP));
    globals.set_constant_by_str(FLOAT_CLASS, "EPSILON", Value::float(f64::EPSILON));
    globals.set_constant_by_str(FLOAT_CLASS, "DIG", Value::integer(f64::DIGITS as i64));
    globals.set_constant_by_str(FLOAT_CLASS, "MANT_DIG", Value::integer(f64::MANTISSA_DIGITS as i64));
    globals.set_constant_by_str(FLOAT_CLASS, "RADIX", Value::integer(f64::RADIX as i64));
    globals.define_builtin_funcs(FLOAT_CLASS, "to_i", &["to_int"], toi, 0);
    globals.define_basic_op(FLOAT_CLASS, "+", add, 1);
    globals.define_basic_op(FLOAT_CLASS, "-", sub, 1);
    globals.define_basic_op(FLOAT_CLASS, "*", mul, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "/", div, div_ff_f, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "%", rem, rem_ff_f, 1);
    globals.define_builtin_cfunc_ff_f(FLOAT_CLASS, "**", pow, pow_ff_f, 1);
    globals.define_builtin_func(FLOAT_CLASS, "div", div_floor, 1);
    globals.define_builtin_func(FLOAT_CLASS, "modulo", rem, 1);
    globals.define_builtin_func(FLOAT_CLASS, "divmod", divmod, 1);
    globals.define_builtin_func(FLOAT_CLASS, "==", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, "===", eq, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">=", ge, 1);
    globals.define_builtin_func(FLOAT_CLASS, ">", gt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=", le, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<", lt, 1);
    globals.define_builtin_func(FLOAT_CLASS, "!=", ne, 1);
    globals.define_builtin_func(FLOAT_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func_with(FLOAT_CLASS, "floor", floor, 0, 1, false);
    globals.define_builtin_func_with(FLOAT_CLASS, "ceil", ceil, 0, 1, false);
    globals.define_builtin_func_with(FLOAT_CLASS, "truncate", truncate, 0, 1, false);
    globals.define_builtin_func_with_kw(FLOAT_CLASS, "round", round, 0, 1, false, &["half"], false);
    globals.define_builtin_func(FLOAT_CLASS, "finite?", finite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "infinite?", infinite, 0);
    globals.define_builtin_func(FLOAT_CLASS, "nan?", nan, 0);
    globals.define_builtin_funcs(FLOAT_CLASS, "abs", &["magnitude"], abs, 0);
    globals.define_builtin_funcs(FLOAT_CLASS, "angle", &["arg", "phase"], angle, 0);
    globals.define_builtin_func(FLOAT_CLASS, "next_float", next_float, 0);
    globals.define_builtin_func(FLOAT_CLASS, "prev_float", prev_float, 0);
    globals.define_builtin_func(FLOAT_CLASS, "quo", quo, 1);
    globals.define_builtin_func(FLOAT_CLASS, "eql?", float_eql, 0 + 1);
    globals.define_builtin_funcs(FLOAT_CLASS, "to_s", &["inspect"], float_to_s, 0);
    globals.define_builtin_class_func(FLOAT_CLASS, "allocate", super::super::class::undef_allocate, 0);
    // Float.new should raise NoMethodError (not TypeError from allocate)
    let float_meta = globals.store.get_metaclass(FLOAT_CLASS).id();
    globals.add_empty_method(float_meta, IdentId::get_id("new"), Visibility::Undefined);
}

/// Helper to raise FloatDomainError
fn float_domain_error(globals: &Globals, msg: &str) -> MonorubyErr {
    let class_id = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("FloatDomainError"))
        .map(|v| v.as_class_id());
    match class_id {
        Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg),
        None => MonorubyErr::rangeerr(msg),
    }
}

extern "C" fn pow_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.powf(rhs)
}

extern "C" fn div_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.ruby_div(&rhs)
}

extern "C" fn rem_ff_f(lhs: f64, rhs: f64) -> f64 {
    lhs.ruby_mod(&rhs)
}

///
/// ### Float#to_i
///
/// - to_i -> Integer
/// - [NOT SUPPORTED]truncate(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/to_i.html]
#[monoruby_builtin]
fn toi(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Float(f) => {
            if f.is_nan() {
                return Err(float_domain_error(globals, "NaN"));
            }
            if f.is_infinite() {
                return Err(float_domain_error(globals, if f > 0.0 { "Infinity" } else { "-Infinity" }));
            }
            let truncated = f.trunc();
            if (i64::MIN as f64) < truncated && truncated < (i64::MAX as f64) {
                Ok(Value::integer(truncated as i64))
            } else if let Some(b) = num::BigInt::from_f64(truncated) {
                Ok(Value::bigint(b))
            } else {
                Err(float_domain_error(globals, "float out of range of integer"))
            }
        }
        _ => unreachable!(),
    }
}

///
/// ### Float#div
///
/// - div(other) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/div.html]
#[monoruby_builtin]
fn div_floor(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().try_float().unwrap();
    if lhs.is_nan() {
        return Err(float_domain_error(globals, "NaN"));
    }
    if lhs.is_infinite() {
        return Err(float_domain_error(globals, if lhs > 0.0 { "Infinity" } else { "-Infinity" }));
    }
    let rhs = RealKind::expect(globals, lfp.arg(0))?.to_f64();
    if rhs == 0.0 {
        return Err(MonorubyErr::divide_by_zero());
    }
    let div_floor = (lhs / rhs).floor();
    Value::coerce_f64_to_int(div_floor)
}

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.ne_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Float#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let ord = match (lhs.try_float(), rhs.unpack()) {
        (None, _) => unreachable!(),
        (Some(lhs), RV::Fixnum(rhs)) => lhs.partial_cmp(&(rhs as f64)),
        (Some(lhs), RV::BigInt(rhs)) => {
            super::super::op::bigint_cmp_float(rhs, lhs).map(|ord| ord.reverse())
        }
        (Some(lhs), RV::Float(rhs)) => lhs.partial_cmp(&rhs),
        (Some(lhs_f), _) => {
            // Try coerce protocol for non-numeric types
            let coerce_id = IdentId::get_id("coerce");
            if let Some(result) =
                vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None)?
            {
                if let Some(ary) = result.try_array_ty() {
                    if ary.len() == 2 {
                        let cmp_id = IdentId::get_id("<=>");
                        let res = vm.invoke_method_inner(
                            globals, cmp_id, ary[0], &[ary[1]], None, None,
                        )?;
                        return Ok(res);
                    }
                }
                // coerce returned something other than a 2-element array
                return Err(MonorubyErr::typeerr("coerce must return [x, y]"));
            }
            // If self is infinite, check if other responds to infinite?
            if lhs_f.is_infinite() {
                let inf_id = IdentId::get_id("infinite?");
                if let Some(other_inf) =
                    vm.invoke_method_if_exists(globals, inf_id, rhs, &[], None, None)?
                {
                    let self_inf = if lhs_f > 0.0 { 1i64 } else { -1i64 };
                    if let Some(other_inf_i) = other_inf.try_fixnum() {
                        return Ok(Value::from_ord(self_inf.cmp(&other_inf_i)));
                    } else if other_inf.is_nil() {
                        // other is finite — Infinity > finite, -Infinity < finite
                        return Ok(Value::from_ord(self_inf.cmp(&0)));
                    }
                }
            }
            None
        }
    };
    Ok(match ord {
        Some(ord) => Value::from_ord(ord),
        None => Value::nil(),
    })
}

///
/// ### Integer#>=
///
/// - self >= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e=3d.html]
#[monoruby_builtin]
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_ge_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#>
///
/// - self > other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e.html]
#[monoruby_builtin]
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_gt_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#<=
///
/// - self <= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3d.html]
#[monoruby_builtin]
fn le(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_le_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Integer#<
///
/// - self < other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c.html]
#[monoruby_builtin]
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    crate::executor::op::cmp_lt_values(vm, globals, lfp.self_val(), lfp.arg(0))
        .ok_or_else(|| vm.take_error())
}

///
/// ### Float#floor
///
/// - floor([NOT SUPPORTED]ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/floor.html]
#[monoruby_builtin]
fn floor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || f.is_infinite() {
        if ndigits > 0 {
            return Ok(lfp.self_val());
        }
        if f.is_nan() {
            return Err(float_domain_error(globals, "NaN"));
        } else {
            return Err(float_domain_error(globals, if f > 0.0 { "Infinity" } else { "-Infinity" }));
        }
    }
    if ndigits == 0 {
        return Value::coerce_f64_to_int(f.floor());
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).floor() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).floor() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                Ok(v)
            } else {
                Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )))
            }
        } else {
            Err(MonorubyErr::rangeerr("too small to convert to u32"))
        }
    }
}

///
/// ### Float#ceil
///
/// - ceil(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/ceil.html]
#[monoruby_builtin]
fn ceil(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || f.is_infinite() {
        if ndigits > 0 {
            return Ok(lfp.self_val());
        }
        if f.is_nan() {
            return Err(float_domain_error(globals, "NaN"));
        } else {
            return Err(float_domain_error(globals, if f > 0.0 { "Infinity" } else { "-Infinity" }));
        }
    }
    if ndigits == 0 {
        return Value::coerce_f64_to_int(f.ceil());
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).ceil() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).ceil() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                return Ok(v);
            } else {
                return Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )));
            }
        }
        Err(MonorubyErr::rangeerr("too small to convert to u32"))
    }
}

///
/// ### Float#truncate
///
/// - truncate(ndigits = 0) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/truncate.html]
#[monoruby_builtin]
fn truncate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || f.is_infinite() {
        if ndigits > 0 {
            return Ok(lfp.self_val());
        }
        if f.is_nan() {
            return Err(float_domain_error(globals, "NaN"));
        } else {
            return Err(float_domain_error(globals, if f > 0.0 { "Infinity" } else { "-Infinity" }));
        }
    }
    if ndigits == 0 {
        return Value::coerce_f64_to_int(f.trunc());
    }
    if ndigits > 0 {
        if let Ok(ndigits) = u32::try_from(ndigits) {
            let mul = 10f64.powi(ndigits as i32);
            let f = (f * mul).trunc() / mul;
            Ok(Value::float(f))
        } else {
            Err(MonorubyErr::rangeerr("too big to convert to u32"))
        }
    } else {
        if let Ok(neg_ndigits) = u32::try_from(-ndigits) {
            let mul = 10f64.powi(neg_ndigits as i32);
            let f = (f / mul).trunc() * mul;
            if let Some(v) = Value::integer_from_f64(f) {
                return Ok(v);
            } else {
                return Err(MonorubyErr::rangeerr(format!(
                    "[unreachable] invalid f64: {f}"
                )));
            }
        }
        Err(MonorubyErr::rangeerr("too small to convert to u32"))
    }
}

///
/// ### Float#round
///
/// - round(ndigits = 0) -> Integer | Float
/// - round(ndigits = 0, half: :up) -> Integer | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/round.html]
#[monoruby_builtin]
fn round(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    // Parse half: keyword (arg(1) after kw processing)
    let half = if let Some(kw_val) = lfp.try_arg(1) {
        parse_half_mode(kw_val)?
    } else {
        None
    };
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || f.is_infinite() {
        if ndigits > 0 {
            return Ok(lfp.self_val());
        }
        if f.is_nan() {
            return Err(float_domain_error(globals, "NaN"));
        } else {
            return Err(float_domain_error(globals, if f > 0.0 { "Infinity" } else { "-Infinity" }));
        }
    }
    if ndigits == 0 {
        let rounded = round_half(f, half);
        return Value::coerce_f64_to_int(rounded);
    }
    if ndigits > 0 {
        // For very large ndigits, 10^ndigits overflows to infinity.
        // In that case, rounding is a no-op — return self.
        if ndigits > 292 {
            return Ok(Value::float(f));
        }
        let ndigits = ndigits as u32;
        let mul = 10f64.powi(ndigits as i32);
        if mul.is_infinite() {
            return Ok(Value::float(f));
        }
        let scaled = f * mul;
        if scaled.is_infinite() || scaled.is_nan() {
            // f * 10^ndigits overflows — rounding is a no-op
            return Ok(Value::float(f));
        }
        // Check if the scaled value appears to be exactly at a halfway point.
        // If so, verify whether the original f is truly at half by comparing
        // f with the exact halfway value reconstructed from the scaled floor.
        // This avoids precision loss from the f*mul multiplication.
        let effective_half = if scaled == scaled.floor() + 0.5 {
            let half_reconstructed = (scaled.floor() + 0.5) / mul;
            if f != half_reconstructed {
                // f is not truly at the halfway point — scaling introduced
                // the apparent half. Use standard rounding (no half mode).
                None
            } else {
                half // truly at half — use the requested mode
            }
        } else {
            half
        };
        let f = round_half(scaled, effective_half) / mul;
        Ok(Value::float(f))
    } else {
        let neg_ndigits = (-ndigits) as u32;
        if neg_ndigits > 308 {
            return Ok(Value::integer(0));
        }
        // Convert float to BigInt, then do integer-level rounding
        // to avoid precision loss from float division.
        let int_val = match BigInt::from_f64(f.trunc()) {
            Some(v) => v,
            None => return Ok(Value::integer(0)),
        };
        let mul_big = BigInt::from(10u64).pow(neg_ndigits);
        let (quot, rem) = num::integer::div_rem(int_val, mul_big.clone());
        // Determine rounding based on remainder
        let half_mul = &mul_big / 2;
        let abs_rem = if rem < BigInt::ZERO { -&rem } else { rem.clone() };
        let rounded_quot = if abs_rem > half_mul {
            // Not halfway — round away from zero
            if f >= 0.0 { &quot + 1 } else { &quot - 1 }
        } else if abs_rem == half_mul {
            // Exactly halfway — apply half mode, but also check
            // the fractional part of the float
            let frac_part = f - f.trunc();
            if frac_part != 0.0 {
                // There's a fractional part beyond the integer truncation;
                // if positive frac, round up; if negative frac, round down
                if f >= 0.0 { &quot + 1 } else { &quot - 1 }
            } else {
                match half {
                    Some(RoundHalf::Down) => quot.clone(),
                    Some(RoundHalf::Even) => {
                        if &quot % 2 == BigInt::ZERO { quot.clone() } else {
                            if f >= 0.0 { &quot + 1 } else { &quot - 1 }
                        }
                    }
                    _ => {
                        // :up (default) — round away from zero
                        if f >= 0.0 { &quot + 1 } else { &quot - 1 }
                    }
                }
            }
        } else {
            quot.clone()
        };
        let result = rounded_quot * mul_big;
        return Ok(Value::bigint(result));
    }
}

/// Parse the `half:` keyword argument for rounding mode.
pub(super) fn parse_half_mode(v: Value) -> Result<Option<RoundHalf>> {
    if v.is_nil() {
        return Ok(None);
    }
    if let Some(sym) = v.try_symbol() {
        let name = sym.get_name();
        match name.as_str() {
            "up" => Ok(Some(RoundHalf::Up)),
            "down" => Ok(Some(RoundHalf::Down)),
            "even" => Ok(Some(RoundHalf::Even)),
            _ => Err(MonorubyErr::argumenterr(format!(
                "invalid rounding mode: {}",
                name
            ))),
        }
    } else {
        Err(MonorubyErr::argumenterr("invalid rounding mode"))
    }
}

#[derive(Clone, Copy)]
pub(super) enum RoundHalf {
    Up,
    Down,
    Even,
}

/// Round a float value with half-way rounding mode.
/// Carefully detects the exact halfway case to avoid floating-point errors.
fn round_half(f: f64, half: Option<RoundHalf>) -> f64 {
    // Check if f is exactly at a halfway point (N + 0.5 or N - 0.5).
    // Use integer comparison to avoid floating-point subtraction errors.
    let floor = f.floor();
    let ceil = f.ceil();
    let half_val = floor + 0.5;
    if f != half_val {
        // Not exactly at halfway — use standard rounding (ties away from zero).
        return f.round();
    }
    // Exactly halfway (f == floor + 0.5)
    match half {
        Some(RoundHalf::Down) => {
            // :down - round toward zero
            if f > 0.0 { floor } else { ceil }
        }
        Some(RoundHalf::Even) => {
            // :even - round to nearest even
            let f_floor = floor as i64;
            if f_floor % 2 == 0 { floor } else { ceil }
        }
        _ => {
            // :up (default) - round away from zero
            if f > 0.0 { ceil } else { floor }
        }
    }
}

///
/// ### Float#finite?
///
/// - finite? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/finite=3f.html]
#[monoruby_builtin]
fn finite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().try_float().unwrap().is_finite()))
}

///
/// ### Float#finite?
///
/// - infinite? -> 1 | -1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/infinite=3f.html]
#[monoruby_builtin]
fn infinite(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_infinite() {
        Ok(Value::integer(num::signum(f) as i64))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Float#nan?
///
/// - nan? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/nan=3f.html]
#[monoruby_builtin]
fn nan(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().try_float().unwrap().is_nan()))
}

///
/// ### Float#abs
///
/// - abs -> Float
/// - magnitude -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/abs.html]
#[monoruby_builtin]
fn abs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    Ok(Value::float(f.abs()))
}

///
/// ### Float#angle
///
/// - angle -> 0 | Float
/// - arg -> 0 | Float
/// - phase -> 0 | Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Float/i/angle.html]
#[monoruby_builtin]
fn angle(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() {
        Ok(lfp.self_val())
    } else if f.is_sign_negative() {
        Ok(Value::float(std::f64::consts::PI))
    } else {
        Ok(Value::integer(0))
    }
}

/// ### Float#next_float
#[monoruby_builtin]
fn next_float(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || (f.is_infinite() && f.is_sign_positive()) {
        return Ok(Value::float(f));
    }
    let bits = f.to_bits();
    let next_bits = if f == 0.0 { 1u64 } else if f > 0.0 { bits + 1 } else { bits - 1 };
    Ok(Value::float(f64::from_bits(next_bits)))
}

/// ### Float#prev_float
#[monoruby_builtin]
fn prev_float(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = lfp.self_val().try_float().unwrap();
    if f.is_nan() || (f.is_infinite() && f.is_sign_negative()) {
        return Ok(Value::float(f));
    }
    let bits = f.to_bits();
    let prev_bits = if f == 0.0 { (1u64 << 63) | 1u64 } else if f > 0.0 { bits - 1 } else { bits + 1 };
    Ok(Value::float(f64::from_bits(prev_bits)))
}

/// ### Float#quo
/// - quo(other) -> Float
///
/// Returns the quotient of self / other as a Float.
#[monoruby_builtin]
fn quo(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().try_float().unwrap();
    let rhs = lfp.arg(0);
    let rhs_f = match rhs.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap_or(f64::INFINITY),
        _ => {
            let coerce_id = IdentId::get_id("coerce");
            if let Some(result) = vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lfp.self_val()], None, None)? {
                if let Some(ary) = result.try_array_ty() {
                    if ary.len() == 2 {
                        let div_id = IdentId::get_id("/");
                        return vm.invoke_method_inner(globals, div_id, ary[0], &[ary[1]], None, None);
                    }
                }
            }
            return Err(MonorubyErr::cant_convert_into_float(globals, rhs));
        }
    };
    Ok(Value::float(lhs / rhs_f))
}

/// ### Float#eql?
/// - eql?(other) -> bool
///
/// Returns true only if other is a Float with the same value.
#[monoruby_builtin]
fn float_eql(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().try_float().unwrap();
    let rhs = lfp.arg(0);
    let result = match rhs.unpack() {
        RV::Float(r) => lhs == r,
        _ => false,
    };
    Ok(Value::bool(result))
}

/// ### Float#to_s / Float#inspect
///
/// - to_s -> String
/// - inspect -> String
///
/// Returns the string representation of self with US-ASCII encoding.
#[monoruby_builtin]
fn float_to_s(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(f) = lfp.self_val().try_float() {
        let s = crate::ruby_float_to_s(f);
        use crate::value::rvalue::{Encoding, RStringInner};
        Ok(Value::string_from_inner(RStringInner::from_encoding(
            s.as_bytes(),
            Encoding::UsAscii,
        )))
    } else {
        // Fallback for non-float self (e.g., Float.allocate)
        let s = lfp.self_val().to_s(&globals.store);
        Ok(Value::string(s))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn angle() {
        run_test("1.0.angle");
        run_test("(-1.0).angle");
        run_test("0.0.angle");
        run_test("(1.0/0).angle");
        run_test("(-1.0/0).angle");
        run_test_once("Float::NAN.angle.nan?");
        run_test("1.0.arg");
        run_test("(-1.0).arg");
        run_test("1.0.phase");
        run_test("(-1.0).phase");
        // NaN returns self (object identity)
        run_test_once("n = Float::NAN; n.angle.equal?(n)");
        run_test_once("n = Float::NAN; n.arg.equal?(n)");
        run_test_once("n = Float::NAN; n.phase.equal?(n)");
        run_test("1.angle");
        run_test("(-1).angle");
        run_test("0.angle");
    }

    #[test]
    fn float() {
        run_test("4.87.to_i");
        run_test("-2.18.to_i");
        run_test("4.7777.to_f");
        run_test("-725.11.to_f");
        run_test("3.0.div(2)");
        run_test("3.0.div(-2)");
        run_test("(-3.0).div(2)");
        run_test("(-3.0).div(-2)");
        run_test("(-37.044).abs");
        run_test("37.044.magnitude");
    }

    #[test]
    fn round() {
        run_test("1.2.floor");
        run_test("(-1.2).floor");
        run_test("1.2.floor(1)");
        run_test("(-1.2).floor(1)");
        run_test("1.567.floor(2)");
        run_test("1234.567.floor(-2)");
        run_test("11.6.round");
        run_test("11.5.round");
        run_test("11.4.round");
        run_test("(-11.4).round");
        run_test("(-11.5).round");
        run_test("(-11.6).round");
        run_test("(1000 * Math::PI).round(3)");
        run_test("(1000 * Math::PI).round(0)");
        run_test("(1000 * Math::PI).round(-3)");
    }

    #[test]
    fn ceil_truncate_round_large_ndigits() {
        // ndigits > 9 would overflow 10i32.pow() before the fix
        run_test("1.123456789.ceil(15)");
        run_test("1.123456789.floor(15)");
        run_test("1.123456789.truncate(15)");
        run_test("1.123456789.round(15)");
        // Large negative ndigits
        run_test("123456789.0.ceil(-15)");
        run_test("123456789.0.floor(-15)");
        run_test("123456789.0.truncate(-15)");
        run_test("123456789.0.round(-15)");
    }

    #[test]
    fn finite() {
        run_test("(1/0.0).finite?");
        run_test("(-1/0.0).finite?");
        run_test("(Float::NAN).finite?");
        run_test("(5.5).finite?");
        run_test("(1/0.0).infinite?");
        run_test("(-1/0.0).infinite?");
        run_test("(Float::NAN).infinite?");
        run_test("(5.5).infinite?");
        run_test("(1/0.0).nan?");
        run_test("(-1/0.0).nan?");
        run_test("(Float::NAN).nan?");
        run_test("(5.5).nan?");
    }

    #[test]
    fn float_division_by_zero() {
        run_test("(0.0/0).nan?");
        run_test("(0.0/0.0).nan?");
        run_test("1.0/0 == Float::INFINITY");
        run_test("-1.0/0 == -Float::INFINITY");
        run_test("1.0/0.0 == Float::INFINITY");
    }

    #[test]
    fn next_float() {
        run_test("1.0.next_float");
        run_test("0.0.next_float");
        run_test("(-1.0).next_float");
    }

    #[test]
    fn prev_float() {
        run_test("1.0.prev_float");
        run_test("0.0.prev_float");
        run_test("(-1.0).prev_float");
    }

    #[test]
    fn zero_positive_negative() {
        run_test("0.0.zero?");
        run_test("1.0.zero?");
        run_test("(-1.0).zero?");
        run_test("1.0.positive?");
        run_test("(-1.0).positive?");
        run_test("0.0.positive?");
        run_test("1.0.negative?");
        run_test("(-1.0).negative?");
        run_test("0.0.negative?");
    }

    #[test]
    fn float_to_r() {
        run_test_once("1.5.respond_to?(:to_r)");
        run_test_once("1.5.respond_to?(:rationalize)");
    }

    #[test]
    fn float_inspect_to_s() {
        // Special values
        run_test("(1.0/0.0).inspect");
        run_test("(-1.0/0.0).inspect");
        run_test("(0.0/0.0).inspect");
        run_test("(1.0/0.0).to_s");
        run_test("(-1.0/0.0).to_s");
        run_test("(0.0/0.0).to_s");
        // Negative zero
        run_test("(-0.0).inspect");
        run_test("(-0.0).to_s");
        run_test("0.0.inspect");
        // Simple values
        run_test("1.0.inspect");
        run_test("100.0.inspect");
        run_test("(-1.0).inspect");
        // Fixed notation boundaries
        run_test("0.0001.inspect");
        run_test("0.001.inspect");
        run_test("100000000000000.0.inspect");
        // Scientific notation
        run_test("0.00001.inspect");
        run_test("(-0.00001).inspect");
        run_test("1e15.inspect");
        run_test("(-1e15).inspect");
        run_test("1e16.inspect");
        run_test("1e20.inspect");
        // Shortest round-trip representation
        run_test("(1.0/7).inspect");
        run_test("0.21611564636388508.inspect");
        // String interpolation uses to_s
        run_test("\"#{1.0/0.0}\"");
        run_test("\"#{-0.0}\"");
    }

    #[test]
    fn float_cmp_coerce() {
        // Object with coerce method (use run_test_once to avoid JIT deopt issues)
        run_test_once(
            r#"
            class MyNum
              def initialize(n); @n = n; end
              def coerce(other); [other, @n.to_f]; end
            end
            obj = MyNum.new(2)
            1.5 <=> obj
            "#,
        );
        // Object without coerce -> nil
        run_test("1.5 <=> 'a'");
    }

    #[test]
    fn float_domain_errors() {
        // to_i/to_int raise FloatDomainError for NaN/Infinity
        run_test_error("Float::NAN.to_i");
        run_test_error("Float::INFINITY.to_i");
        run_test_error("(-Float::INFINITY).to_i");
        // floor/ceil/truncate raise for NaN/Infinity with ndigits=0
        run_test_error("Float::NAN.floor");
        run_test_error("Float::INFINITY.ceil");
    }

    // Fix 1: Float#round accepts half: keyword argument
    #[test]
    fn round_half_keyword() {
        run_test("2.5.round(half: :up)");
        run_test("2.5.round(half: :down)");
        run_test("2.5.round(half: :even)");
        run_test("3.5.round(half: :up)");
        run_test("3.5.round(half: :down)");
        run_test("3.5.round(half: :even)");
        run_test("(-2.5).round(half: :up)");
        run_test("(-2.5).round(half: :down)");
        run_test("(-2.5).round(half: :even)");
        run_test("2.5.round(half: nil)");
        // half: with ndigits
        run_test("5.55.round(1, half: :up)");
        run_test("5.55.round(1, half: :down)");
        run_test("5.55.round(1, half: :even)");
        run_test("(-5.55).round(1, half: :up)");
        run_test("(-5.55).round(1, half: :down)");
        run_test("(-5.55).round(1, half: :even)");
    }

    // Fix 1: Float#round half: with invalid mode raises ArgumentError
    #[test]
    fn round_half_invalid() {
        run_test_error("2.5.round(half: :invalid)");
    }

    // Fix 2: Float#round algorithm edge cases
    #[test]
    fn round_edge_cases() {
        // Near the limit: -0.49999999999999994 should round to 0, not -1
        run_test("(-0.49999999999999994).round");
        // Large ndigits: should not overflow to Infinity
        run_test("42.0.round(308)");
        run_test("1.0e307.round(2)");
        // Very large ndigits: rounding is a no-op
        run_test_once("0.42.round(2.0**30)");
        run_test_once("0.42.round(2.0**23)");
        // Negative ndigits with big values (BigInt precision)
        run_test_once("(2.5e200).round(-200)");
        run_test_once("(-2.5e200).round(-200)");
        run_test_once("(2.4e200).round(-200)");
        run_test_once("(-2.4e200).round(-200)");
        // Negative ndigits for normal values
        run_test("120.0.round(-1)");
        run_test("123456.78.round(-2)");
        // Precision loss test
        run_test("767573.1875850001.round(5)");
    }

    // Fix 5: Float#<=> with infinite? objects
    #[test]
    fn float_cmp_infinite() {
        run_test_once(
            r#"
            obj = Object.new
            def obj.infinite?; 1; end
            Float::INFINITY <=> obj
            "#,
        );
        run_test_once(
            r#"
            obj = Object.new
            def obj.infinite?; -1; end
            Float::INFINITY <=> obj
            "#,
        );
        run_test_once(
            r#"
            obj = Object.new
            def obj.infinite?; nil; end
            Float::INFINITY <=> obj
            "#,
        );
    }

    // Fix 4: Float#<=> raises TypeError when coerce misbehaves
    #[test]
    fn float_cmp_coerce_typeerror() {
        run_test_error(
            r#"
            class BadCoerce
              def coerce(other); :not_an_array; end
            end
            1.0 <=> BadCoerce.new
            "#,
        );
    }

    // Fix 4: Float comparison operators propagate exceptions from coerce
    #[test]
    fn float_cmp_coerce_exception_propagation() {
        run_test_error(
            r#"
            class RaiseInCoerce
              def coerce(other)
                raise "coerce error"
              end
            end
            1.0 > RaiseInCoerce.new
            "#,
        );
        run_test_error(
            r#"
            class RaiseInCoerce
              def coerce(other)
                raise "coerce error"
              end
            end
            1.0 < RaiseInCoerce.new
            "#,
        );
    }

    // Fix 6: Float#to_s returns US-ASCII encoding
    #[test]
    fn float_to_s_encoding() {
        run_test("1.0.to_s.encoding.to_s");
        run_test("(-0.0).to_s.encoding.to_s");
        run_test("Float::NAN.to_s.encoding.to_s");
        run_test("Float::INFINITY.to_s.encoding.to_s");
        run_test("1.0.inspect.encoding.to_s");
    }

    #[test]
    fn float_quo() {
        run_test("6.0.quo(2)");
        run_test("6.0.quo(2.0)");
    }

    #[test]
    fn float_eql() {
        run_test("1.0.eql?(1.0)");
        run_test("1.0.eql?(1)");
        run_test("1.0.eql?(1.1)");
    }

    #[test]
    fn float_constants() {
        run_test("Float::MIN");
        run_test("Float::DIG");
        run_test("Float::MANT_DIG");
        run_test("Float::RADIX");
    }

    #[test]
    fn float_coerce_string() {
        run_test_once(r#"1.0.coerce("2.5").inspect"#);
        run_test_error(r#"1.0.coerce("not_a_number")"#);
    }

    #[test]
    fn float_allocate_disabled() {
        run_test_error("Float.allocate");
        run_test_error("Float.new");
    }

    #[test]
    fn float_denominator_special() {
        run_test("Float::NAN.denominator");
        run_test("Float::INFINITY.denominator");
        run_test("(-Float::INFINITY).denominator");
        run_test("1.5.denominator");
    }

    #[test]
    fn float_divmod_quotient_type() {
        run_test("3.8.divmod(1)");
        run_test("(-3.8).divmod(1)");
        run_test("3.8.divmod(0.5)");
        run_test("11.5.divmod(3)");
    }

    #[test]
    fn float_eq_coerce_fallback() {
        run_test_once(
            r#"
            class Coercible
              def ==(other); true; end
            end
            1.0 == Coercible.new
            "#,
        );
        run_test_once(
            r#"
            class Coercible
              def ==(other); true; end
            end
            1.0 === Coercible.new
            "#,
        );
    }

    #[test]
    fn float_fdiv() {
        run_test("1.0.fdiv(2)");
        run_test("1.0.fdiv(2.0)");
        run_test("1.0.fdiv(0.5)");
        run_test_error("1.0.fdiv(:foo)");
    }

    #[test]
    fn float_rationalize() {
        run_test("3382729202.92822.rationalize");
        run_test("0.3.rationalize(Rational(1,10))");
        run_test("0.3.rationalize(0.05)");
        run_test("0.3.rationalize(0.001)");
        run_test("(-0.3).rationalize(Rational(1,10))");
        run_test("(-0.3).rationalize(0.05)");
        run_test("(-0.3).rationalize(0.001)");
        run_test("0.0.rationalize");
        run_test_error("Float::NAN.rationalize");
        run_test_error("Float::INFINITY.rationalize");
    }

    #[test]
    fn float_round_precision() {
        run_test("767573.1875850001.round(5)");
        run_test("767573.1875850001.round(5, half: :up)");
        run_test("767573.1875850001.round(5, half: :down)");
        run_test("767573.1875850001.round(5, half: :even)");
        run_test("(-767573.1875850001).round(5, half: :down)");
        run_test("(-767573.1875850001).round(5, half: :even)");
        // Truly at half — half modes should apply
        run_test("767573.187585.round(5, half: :up)");
        run_test("767573.187585.round(5, half: :down)");
        run_test("767573.187585.round(5, half: :even)");
    }
}
