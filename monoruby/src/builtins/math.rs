use super::*;
use num::ToPrimitive;
use std::os::raw::c_int;

unsafe extern "C" {
    #[link_name = "erf"]
    fn c_erf(x: f64) -> f64;
    #[link_name = "erfc"]
    fn c_erfc(x: f64) -> f64;
    #[link_name = "tgamma"]
    fn c_tgamma(x: f64) -> f64;
    #[link_name = "lgamma_r"]
    fn c_lgamma_r(x: f64, signp: *mut c_int) -> f64;
    #[link_name = "ldexp"]
    fn c_ldexp(x: f64, exp: c_int) -> f64;
    #[link_name = "frexp"]
    fn c_frexp(x: f64, exp: *mut c_int) -> f64;
}

//
// Math class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Math").id();
    let standarderr = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("StandardError"))
        .unwrap()
        .as_class();
    globals.define_class("DomainError", standarderr, klass);
    globals.set_constant_by_str(klass, "PI", Value::float(std::f64::consts::PI));
    globals.set_constant_by_str(klass, "E", Value::float(std::f64::consts::E));
    globals.define_builtin_module_inline_func(klass, "sqrt", sqrt, Box::new(math_sqrt), 1);
    globals.define_builtin_module_cfunc_f_f(klass, "cos", cos, extern_cos, 1);
    globals.define_builtin_module_cfunc_f_f(klass, "sin", sin, extern_sin, 1);
    globals.define_builtin_module_func(klass, "tan", tan, 1);
    globals.define_builtin_module_func_with(klass, "log", log, 1, 2, false);
    globals.define_builtin_module_func(klass, "log2", log2, 1);
    globals.define_builtin_module_func(klass, "log10", log10, 1);
    globals.define_builtin_module_func(klass, "exp", exp, 1);
    globals.define_builtin_module_func(klass, "asin", asin, 1);
    globals.define_builtin_module_func(klass, "acos", acos, 1);
    globals.define_builtin_module_func(klass, "atan", atan, 1);
    globals.define_builtin_module_func(klass, "atan2", atan2, 2);
    globals.define_builtin_module_func(klass, "sinh", sinh, 1);
    globals.define_builtin_module_func(klass, "cosh", cosh, 1);
    globals.define_builtin_module_func(klass, "tanh", tanh, 1);
    globals.define_builtin_module_func(klass, "asinh", asinh, 1);
    globals.define_builtin_module_func(klass, "acosh", acosh, 1);
    globals.define_builtin_module_func(klass, "atanh", atanh, 1);
    globals.define_builtin_module_func(klass, "erf", erf, 1);
    globals.define_builtin_module_func(klass, "erfc", erfc, 1);
    globals.define_builtin_module_func(klass, "gamma", gamma, 1);
    globals.define_builtin_module_func(klass, "lgamma", lgamma, 1);
    globals.define_builtin_module_func(klass, "cbrt", cbrt, 1);
    globals.define_builtin_module_func(klass, "hypot", hypot, 2);
    globals.define_builtin_module_func(klass, "ldexp", ldexp, 2);
    globals.define_builtin_module_func(klass, "frexp", frexp, 1);
}

/// Convert a Value to f64, trying `to_f` if the value is not directly numeric.
fn coerce_to_f64(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<f64> {
    match v.unpack() {
        RV::Float(f) => Ok(f),
        RV::Fixnum(i) => Ok(i as f64),
        RV::BigInt(b) => Ok(b.to_f64().unwrap()),
        _ => {
            // Only try to_f for non-String types (CRuby only calls to_f on Numeric subclasses)
            if v.is_str().is_none()
                && let Some(fid) = globals.check_method(v, IdentId::TO_F)
            {
                let result = vm.invoke_func_inner(globals, fid, v, &[], None, None)?;
                match result.unpack() {
                    RV::Float(f) => Ok(f),
                    RV::Fixnum(i) => Ok(i as f64),
                    _ => Err(MonorubyErr::typeerr(format!(
                        "can't convert {} into Float ({}#to_f gives {})",
                        v.get_real_class_name(&globals.store),
                        v.get_real_class_name(&globals.store),
                        result.get_real_class_name(&globals.store),
                    ))),
                }
            } else {
                Err(MonorubyErr::cant_convert_into_float(globals, v))
            }
        }
    }
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
#[monoruby_builtin]
fn sqrt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.sqrt()))
}

/// ### Math.#sin
/// - sin(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sin.html]
#[monoruby_builtin]
fn sin(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.sin()))
}

/// ### Math.#cos
/// - cos(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cos.html]
#[monoruby_builtin]
fn cos(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.cos()))
}

/// ### Math.#tan
/// - tan(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/tan.html]
#[monoruby_builtin]
fn tan(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.tan()))
}

/// ### Math.#log
/// - log(x) -> Float
/// - log(x, base) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/log.html]
#[monoruby_builtin]
fn log(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f.is_sign_negative() {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"log\"",
        ));
    }
    let result = if let Some(base) = lfp.try_arg(1) {
        let b = coerce_to_f64(vm, globals, base)?;
        f.log(b)
    } else {
        f.ln()
    };
    Ok(Value::float(result))
}

/// ### Math.#log2
/// - log2(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/log2.html]
#[monoruby_builtin]
fn log2(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f.is_sign_negative() {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"log2\"",
        ));
    }
    Ok(Value::float(f.log2()))
}

/// Math.#log10
/// - log10(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/log10.html]
#[monoruby_builtin]
fn log10(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f.is_sign_negative() {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - log10",
        ));
    }
    Ok(Value::float(f.log10()))
}

/// ### Math.#exp
/// - exp(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/exp.html]
#[monoruby_builtin]
fn exp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.exp()))
}

/// ### Math.#asin
/// - asin(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/asin.html]
#[monoruby_builtin]
fn asin(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f < -1.0 || f > 1.0 {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"asin\"",
        ));
    }
    Ok(Value::float(f.asin()))
}

/// ### Math.#acos
/// - acos(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/acos.html]
#[monoruby_builtin]
fn acos(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f < -1.0 || f > 1.0 {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"acos\"",
        ));
    }
    Ok(Value::float(f.acos()))
}

/// ### Math.#atan
/// - atan(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/atan.html]
#[monoruby_builtin]
fn atan(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.atan()))
}

/// ### Math.#atan2
/// - atan2(y, x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/atan2.html]
#[monoruby_builtin]
fn atan2(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let y = coerce_to_f64(vm, globals, lfp.arg(0))?;
    let x = coerce_to_f64(vm, globals, lfp.arg(1))?;
    Ok(Value::float(y.atan2(x)))
}

/// ### Math.#sinh
/// - sinh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sinh.html]
#[monoruby_builtin]
fn sinh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.sinh()))
}

/// ### Math.#cosh
/// - cosh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cosh.html]
#[monoruby_builtin]
fn cosh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.cosh()))
}

/// ### Math.#tanh
/// - tanh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/tanh.html]
#[monoruby_builtin]
fn tanh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.tanh()))
}

/// ### Math.#asinh
/// - asinh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/asinh.html]
#[monoruby_builtin]
fn asinh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.asinh()))
}

/// ### Math.#acosh
/// - acosh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/acosh.html]
#[monoruby_builtin]
fn acosh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f < 1.0 {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"acosh\"",
        ));
    }
    Ok(Value::float(f.acosh()))
}

/// ### Math.#atanh
/// - atanh(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/atanh.html]
#[monoruby_builtin]
fn atanh(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f <= -1.0 || f >= 1.0 {
        if f == -1.0 || f == 1.0 {
            // Returns -Infinity or Infinity
            Ok(Value::float(f.atanh()))
        } else {
            return Err(MonorubyErr::rangeerr(
                "Numerical argument is out of domain - \"atanh\"",
            ));
        }
    } else {
        Ok(Value::float(f.atanh()))
    }
}

/// ### Math.#erf
/// - erf(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/erf.html]
#[monoruby_builtin]
fn erf(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    // SAFETY: erf is a standard C math function with no safety concerns.
    Ok(Value::float(unsafe { c_erf(f) }))
}

/// ### Math.#erfc
/// - erfc(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/erfc.html]
#[monoruby_builtin]
fn erfc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    // SAFETY: erfc is a standard C math function with no safety concerns.
    Ok(Value::float(unsafe { c_erfc(f) }))
}

/// ### Math.#gamma
/// - gamma(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/gamma.html]
#[monoruby_builtin]
fn gamma(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    if f < 0.0 && f == f.floor() {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - \"tgamma\"",
        ));
    }
    // SAFETY: tgamma is a standard C math function with no safety concerns.
    Ok(Value::float(unsafe { c_tgamma(f) }))
}

/// ### Math.#lgamma
/// - lgamma(x) -> [Float, Integer]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/lgamma.html]
#[monoruby_builtin]
fn lgamma(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    // SAFETY: lgamma_r is a standard C math function; sign is a valid pointer to a local variable.
    let mut sign: c_int = 0;
    let result = unsafe { c_lgamma_r(f, &mut sign) };
    Ok(Value::array2(
        Value::float(result),
        Value::integer(sign as i64),
    ))
}

/// ### Math.#cbrt
/// - cbrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cbrt.html]
#[monoruby_builtin]
fn cbrt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    Ok(Value::float(f.cbrt()))
}

/// ### Math.#hypot
/// - hypot(x, y) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/hypot.html]
#[monoruby_builtin]
fn hypot(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let x = coerce_to_f64(vm, globals, lfp.arg(0))?;
    let y = coerce_to_f64(vm, globals, lfp.arg(1))?;
    Ok(Value::float(x.hypot(y)))
}

/// ### Math.#ldexp
/// - ldexp(x, exp) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/ldexp.html]
#[monoruby_builtin]
fn ldexp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let x = coerce_to_f64(vm, globals, lfp.arg(0))?;
    let exp = match lfp.arg(1).unpack() {
        RV::Fixnum(i) => i as i32,
        RV::Float(f) => f as i32,
        RV::BigInt(b) => b.to_i32().unwrap_or(i32::MAX),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(globals, lfp.arg(1)));
        }
    };
    // SAFETY: ldexp is a standard C math function with no safety concerns.
    Ok(Value::float(unsafe { c_ldexp(x, exp) }))
}

/// ### Math.#frexp
/// - frexp(x) -> [Float, Integer]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/frexp.html]
#[monoruby_builtin]
fn frexp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = coerce_to_f64(vm, globals, lfp.arg(0))?;
    // SAFETY: frexp is a standard C math function; exp_val is a valid pointer to a local variable.
    let mut exp_val: c_int = 0;
    let frac = unsafe { c_frexp(f, &mut exp_val) };
    Ok(Value::array2(
        Value::float(frac),
        Value::integer(exp_val as i64),
    ))
}

fn math_sqrt(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;
    let fsrc = state.load_xmm(ir, args).enc();
    if let Some(dst) = dst {
        let fret = state.def_F(dst).enc();
        ir.inline(move |r#gen, _, _| {
            monoasm!( &mut r#gen.jit,
                sqrtsd xmm(fret), xmm(fsrc);
            );
        });
    }
    true
}

extern "C" fn extern_cos(f: f64) -> f64 {
    f.cos()
}

extern "C" fn extern_sin(f: f64) -> f64 {
    f.sin()
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn torigonometric() {
        run_test("Math.cos 149");
        run_test("Math.cos -14.97522");
        run_test("Math.sin 149");
        run_test("Math.sin -14.97522");
        run_test("Math.tan 149");
        run_test("Math.tan -14.97522");
    }

    #[test]
    fn inverse_trig() {
        run_test("Math.asin 0.5");
        run_test("Math.acos 0.5");
        run_test("Math.atan 1");
        run_test("Math.atan2(1, 1)");
        run_test("Math.atan2(0, -1)");
    }

    #[test]
    fn hyperbolic() {
        run_test("Math.sinh 1");
        run_test("Math.cosh 1");
        run_test("Math.tanh 1");
        run_test("Math.asinh 1");
        run_test("Math.acosh 2");
        run_test("Math.atanh 0.5");
    }

    #[test]
    fn log() {
        run_test("Math.log10 149");
        run_test("Math.log10 14.9");
        run_test(
            "Math.log10 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
        );
        run_test("Math.log(1)");
        run_test("Math.log(Math::E)");
        run_test("Math.log(10, 10)");
        run_test("Math.log2(1)");
        run_test("Math.log2(8)");
    }

    #[test]
    fn exp_test() {
        run_test("Math.exp(0)");
        run_test("Math.exp(1)");
        run_test("Math.exp(-1)");
    }

    #[test]
    fn error_functions() {
        run_test("Math.erf(0)");
        run_test("Math.erf(1)");
        run_test("Math.erfc(0)");
        run_test("Math.erfc(1)");
    }

    #[test]
    fn gamma_test() {
        run_test("Math.gamma(5)");
        run_test("Math.gamma(1)");
        run_test("Math.lgamma(5)");
        run_test("Math.lgamma(1)");
    }

    #[test]
    fn misc_math() {
        run_test("Math.cbrt(27)");
        run_test("Math.cbrt(-8)");
        run_test("Math.hypot(3, 4)");
        run_test("Math.ldexp(0.5, 2)");
        run_test("Math.frexp(1024)");
    }

    #[test]
    fn sqrt() {
        run_test("Math.sqrt 128");
        run_test("Math.sqrt 2192.56818");
        run_test(
            r#"
        class C
          include Math
          def f
            sqrt 2192.56818
          end
        end
        C.new.f
        "#,
        );
    }

    #[test]
    fn implicit_to_f() {
        run_test(
            r#"
            class MyNum < Numeric
              def to_f
                2.0
              end
            end
            Math.sqrt(MyNum.new)
            "#,
        );
        run_test(
            r#"
            class MyNum2 < Numeric
              def to_f
                1.0
              end
            end
            Math.sin(MyNum2.new)
            "#,
        );
        run_test(
            r#"
            class MyNum3 < Numeric
              def to_f
                0.5
              end
            end
            Math.cos(MyNum3.new)
            "#,
        );
    }
}
