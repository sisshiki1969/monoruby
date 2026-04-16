use num::Zero;

use super::*;

//
// Complex class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class(
        "Complex",
        COMPLEX_CLASS,
        numeric,
        OBJECT_CLASS,
        ObjTy::COMPLEX,
    );
    globals.define_builtin_func(COMPLEX_CLASS, "==", eq, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "!=", ne, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "<=>", cmp, 1);
    globals.define_builtin_class_func_with(COMPLEX_CLASS, "polar", complex_polar, 1, 2, false);
    globals.define_builtin_class_funcs_with(
        COMPLEX_CLASS,
        "rect",
        &["rectangular"],
        complex_rect,
        1,
        2,
        false,
    );
    globals.define_builtin_funcs(COMPLEX_CLASS, "abs", &["magnitude"], abs, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "abs2", abs2, 0);
    globals.define_builtin_funcs(COMPLEX_CLASS, "rect", &["rectangular"], rect, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "real", real, 0);
    globals.define_builtin_funcs(COMPLEX_CLASS, "imaginary", &["imag"], imaginary, 0);
    globals.define_builtin_funcs(COMPLEX_CLASS, "conjugate", &["conj"], conjugate, 0);
    globals.define_builtin_funcs(COMPLEX_CLASS, "angle", &["arg", "phase"], angle, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "polar", polar_instance, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_c", to_c, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "quo", quo, 1);
    globals.define_builtin_func_with(COMPLEX_CLASS, "rationalize", rationalize, 0, 1, false);
    globals.define_builtin_func(COMPLEX_CLASS, "real?", real_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "finite?", finite_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "infinite?", infinite_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "fdiv", fdiv, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "**", pow, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "numerator", numerator, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "denominator", denominator, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "integer?", integer_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "hash", hash, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "marshal_dump", marshal_dump, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_f", to_f, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_r", to_r, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "-@", neg_op, 0);
    globals.store[COMPLEX_CLASS].clear_alloc_func();
    globals.define_builtin_func(COMPLEX_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "coerce", coerce, 1);

    // Complex::I = Complex(0, 1)
    let i_const = Value::complex(Real::from(0), Real::from(1));
    globals.set_constant_by_str(COMPLEX_CLASS, "I", i_const);
}

fn eq_bool(store: &Store, lhs: &ComplexInner, rhs: Value) -> bool {
    match rhs.try_complex() {
        Some(rhs) => lhs == rhs,
        None => {
            let rhs = match Real::try_from(store, rhs) {
                Ok(rhs) => rhs,
                Err(_) => return false,
            };

            lhs == &ComplexInner::new(rhs, Real::zero())
        }
    }
}

///
/// ### Complex#==
///
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    Ok(Value::bool(eq_bool(globals, lhs.as_complex(), lfp.arg(0))))
}

#[monoruby_builtin]
fn ne(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    Ok(Value::bool(!eq_bool(globals, lhs.as_complex(), lfp.arg(0))))
}

///
/// ### Complex.polar
///
/// - polar(r, theta = 0) -> Complex
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/s/polar.html]
#[monoruby_builtin]
fn complex_polar(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let r = RealKind::expect(globals, lfp.arg(0))?.to_f64();
    let theta = if let Some(theta) = lfp.try_arg(1) {
        RealKind::expect(globals, theta)?.to_f64()
    } else {
        RealKind::Float(0.0).to_f64()
    };
    let c = num::complex::Complex::from_polar(r, theta);
    Ok(Value::complex(c.re, c.im))
}

///
/// ### Complex.rect
///
/// - rect(r, i = 0) -> Complex
/// - rectangular(r, i = 0) -> Complex
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/s/rect.html]
#[monoruby_builtin]
fn complex_rect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let r_arg = lfp.arg(0);
    ensure_real_for_rect(vm, globals, r_arg)?;
    let r = Real::try_from(globals, r_arg)?;
    let i = if let Some(i_arg) = lfp.try_arg(1) {
        ensure_real_for_rect(vm, globals, i_arg)?;
        Real::try_from(globals, i_arg)?
    } else {
        Real::from(0)
    };
    Ok(Value::complex(r, i))
}

/// Reject values whose `real?` method returns false (e.g. a Complex-typed
/// Numeric subclass).
fn ensure_real_for_rect(
    vm: &mut Executor,
    globals: &mut Globals,
    value: Value,
) -> Result<()> {
    // Direct numeric types are always real.
    match value.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => return Ok(()),
        _ => {}
    }
    if value.try_rational().is_some() {
        return Ok(());
    }
    // Complex numbers are not accepted as rect parts.
    if value.try_complex().is_some() {
        return Err(MonorubyErr::typeerr("not a real"));
    }
    // Numeric subclass responding to real? — honour that answer.
    let real_q_id = IdentId::get_id("real?");
    if globals.check_method(value, real_q_id).is_some() {
        let v = vm.invoke_method_inner(globals, real_q_id, value, &[], None, None)?;
        if !v.as_bool() {
            return Err(MonorubyErr::typeerr("not a real"));
        }
    }
    Ok(())
}

///
/// ### Complex#abs
///
/// - abs -> Numeric
/// - magnitude -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/abs.html]
#[monoruby_builtin]
fn abs(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let abs = lfp.self_val().as_complex().to_complex_f64().norm();
    Ok(Value::float(abs))
}

///
/// ### Complex#eql?
///
/// - eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/eql=3f.html]
#[monoruby_builtin]
fn eql_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let lhs = self_val.as_complex();
    let rhs = lfp.arg(0);
    if let Some(rhs) = rhs.try_complex() {
        Ok(Value::bool(lhs.eql(rhs, vm, globals)?))
    } else {
        Ok(Value::bool(false))
    }
}

///
/// ### Complex#abs
///
/// - rect -> [Numeric, Numeric]
/// - rectangular -> [Numeric, Numeric]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/rect.html]
#[monoruby_builtin]
fn rect(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let r = self_.as_complex().re();
    let i = self_.as_complex().im();
    Ok(Value::array2(r.get(), i.get()))
}

///
/// ### Complex#coerce
///
/// - coerce(other) -> [Complex, Complex]
///
/// Returns [Complex(other, 0), self].
#[monoruby_builtin]
fn coerce(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    // Already a Complex: return [other, self] unchanged.
    if other.try_complex().is_some() {
        return Ok(Value::array2(other, self_val));
    }
    // Fixnum/BigInt/Float: wrap `other` as Complex(other, 0).
    match other.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => {
            let re = Real::try_from(globals, other)?;
            let wrapped = Value::complex(re, Real::from(0));
            return Ok(Value::array2(wrapped, self_val));
        }
        _ => {}
    }
    // Rational: spec expects `Complex.kind_of?` to be true for the first
    // element, so we convert it (to_f; precision may be lost).
    if let Some(r) = other.try_rational() {
        let wrapped = Value::complex(Real::from(r.to_f()), Real::from(0));
        return Ok(Value::array2(wrapped, self_val));
    }
    // Numeric subclass with real? → true: pass through unchanged.
    if is_real_like_numeric(vm, globals, other)? {
        return Ok(Value::array2(other, self_val));
    }
    Err(MonorubyErr::typeerr(format!(
        "{} can't be coerced into Complex",
        other.get_real_class_name(globals)
    )))
}

///
/// Is `other` a Numeric whose `real?` method returns true? Follows the
/// same protocol CRuby uses to decide whether to treat an unknown Numeric
/// subclass as a real number.
fn is_real_like_numeric(
    vm: &mut Executor,
    globals: &mut Globals,
    other: Value,
) -> Result<bool> {
    // Must be a Numeric subclass.
    let numeric_id = IdentId::get_id("Numeric");
    let numeric = match globals.store.get_constant_noautoload(OBJECT_CLASS, numeric_id) {
        Some(v) => v.as_class_id(),
        None => return Ok(false),
    };
    if !other.is_kind_of(&globals.store, numeric) {
        return Ok(false);
    }
    let real_q_id = IdentId::get_id("real?");
    if globals.check_method(other, real_q_id).is_none() {
        return Ok(false);
    }
    let v = vm.invoke_method_inner(globals, real_q_id, other, &[], None, None)?;
    Ok(v.as_bool())
}

///
/// ### Complex#real
///
#[monoruby_builtin]
fn real(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_complex().re().get())
}

///
/// ### Complex#imaginary
/// ### Complex#imag
///
#[monoruby_builtin]
fn imaginary(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_complex().im().get())
}

///
/// ### Complex#conjugate
/// ### Complex#conj
///
#[monoruby_builtin]
fn conjugate(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    Ok(Value::complex(c.re(), -c.im()))
}

///
/// ### Complex#abs2
///
/// Returns |z|^2 = re^2 + im^2. Preserves the component type (Integer
/// stays Integer) by delegating through Real arithmetic.
#[monoruby_builtin]
fn abs2(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let re_sq = c.re() * c.re();
    let im_sq = c.im() * c.im();
    Ok((re_sq + im_sq).get())
}

///
/// ### Complex#angle
/// ### Complex#arg
/// ### Complex#phase
///
#[monoruby_builtin]
fn angle(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = lfp.self_val().as_complex().to_complex_f64();
    Ok(Value::float(c.im.atan2(c.re)))
}

///
/// ### Complex#real?
///
/// Always returns false for Complex values.
#[monoruby_builtin]
fn real_q(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(false))
}

///
/// ### Complex#finite?
///
#[monoruby_builtin]
fn finite_q(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = lfp.self_val().as_complex().to_complex_f64();
    Ok(Value::bool(c.re.is_finite() && c.im.is_finite()))
}

///
/// ### Complex#infinite?
///
/// Returns 1 if either part is infinite, nil otherwise.
#[monoruby_builtin]
fn infinite_q(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let c = lfp.self_val().as_complex().to_complex_f64();
    if c.re.is_infinite() || c.im.is_infinite() {
        Ok(Value::integer(1))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Complex#<=>
///
/// Returns nil when either has a non-zero imaginary part, or when the
/// argument is not a real Numeric. Otherwise compares the real parts.
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let lhs = self_val.as_complex();
    let other = lfp.arg(0);
    // If self has an imaginary part, no ordering exists.
    if !lhs.im().is_zero() {
        return Ok(Value::nil());
    }
    let lhs_re = lhs.re().get();
    // Complex-like: unwrap to its real part if imaginary is zero.
    if let Some(rhs) = other.try_complex() {
        if !rhs.im().is_zero() {
            return Ok(Value::nil());
        }
        let cmp_id = IdentId::get_id("<=>");
        return vm.invoke_method_inner(globals, cmp_id, lhs_re, &[rhs.re().get()], None, None);
    }
    match other.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => {
            let cmp_id = IdentId::get_id("<=>");
            vm.invoke_method_inner(globals, cmp_id, lhs_re, &[other], None, None)
        }
        _ => Ok(Value::nil()),
    }
}

///
/// ### Complex#fdiv
///
#[monoruby_builtin]
fn fdiv(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let other = lfp.arg(0);
    // fdiv requires a numeric argument.
    match other.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => {}
        _ => {
            return Err(MonorubyErr::typeerr(format!(
                "{} can't be coerced into Float",
                other.get_real_class_name(globals)
            )));
        }
    }
    let fdiv_id = IdentId::get_id("fdiv");
    let re = vm.invoke_method_inner(globals, fdiv_id, c.re().get(), &[other], None, None)?;
    let im = vm.invoke_method_inner(globals, fdiv_id, c.im().get(), &[other], None, None)?;
    let re_f = match re.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        _ => return Err(MonorubyErr::typeerr("fdiv did not return Float")),
    };
    let im_f = match im.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        _ => return Err(MonorubyErr::typeerr("fdiv did not return Float")),
    };
    Ok(Value::complex(Real::from(re_f), Real::from(im_f)))
}

///
/// ### Complex#numerator
///
#[monoruby_builtin]
fn numerator(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let d = complex_denominator(vm, globals, c)?;
    let num_id = IdentId::get_id("numerator");
    let div_id = IdentId::_DIV;
    let mul_id = IdentId::_MUL;
    let den_id = IdentId::get_id("denominator");
    let re_num = vm.invoke_method_inner(globals, num_id, c.re().get(), &[], None, None)?;
    let re_den = vm.invoke_method_inner(globals, den_id, c.re().get(), &[], None, None)?;
    let im_num = vm.invoke_method_inner(globals, num_id, c.im().get(), &[], None, None)?;
    let im_den = vm.invoke_method_inner(globals, den_id, c.im().get(), &[], None, None)?;
    let re_scale = vm.invoke_method_inner(globals, div_id, d, &[re_den], None, None)?;
    let im_scale = vm.invoke_method_inner(globals, div_id, d, &[im_den], None, None)?;
    let re = vm.invoke_method_inner(globals, mul_id, re_num, &[re_scale], None, None)?;
    let im = vm.invoke_method_inner(globals, mul_id, im_num, &[im_scale], None, None)?;
    let re_r = Real::try_from(globals, re)?;
    let im_r = Real::try_from(globals, im)?;
    Ok(Value::complex(re_r, im_r))
}

///
/// ### Complex#denominator
///
#[monoruby_builtin]
fn denominator(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    complex_denominator(vm, globals, c)
}

fn complex_denominator(
    vm: &mut Executor,
    globals: &mut Globals,
    c: &ComplexInner,
) -> Result<Value> {
    let den_id = IdentId::get_id("denominator");
    let re_den = vm.invoke_method_inner(globals, den_id, c.re().get(), &[], None, None)?;
    let im_den = vm.invoke_method_inner(globals, den_id, c.im().get(), &[], None, None)?;
    let lcm_id = IdentId::get_id("lcm");
    vm.invoke_method_inner(globals, lcm_id, re_den, &[im_den], None, None)
}

///
/// ### Complex#integer?
///
/// Always returns false.
#[monoruby_builtin]
fn integer_q(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(false))
}

///
/// ### Complex#hash
///
/// Hash is based on real and imaginary parts' values (stable across instances).
#[monoruby_builtin]
fn hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    c.re().hash(&mut hasher);
    c.im().hash(&mut hasher);
    Ok(Value::integer(hasher.finish() as i64))
}

///
/// ### Complex#marshal_dump
///
/// Returns [real, imaginary].
#[monoruby_builtin]
fn marshal_dump(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    Ok(Value::array2(c.re().get(), c.im().get()))
}

fn complex_to_real(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    coerce_id: IdentId,
    error_label: &str,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    // Imaginary part must be strictly Integer 0, Rational 0, or a custom
    // Numeric subclass whose `==` returns true for 0. Float 0.0 is NOT
    // allowed (CRuby raises RangeError in that case).
    let im = c.im().get();
    let is_exact_zero = match im.unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        RV::Float(_) => false,
        _ => {
            if let Some(r) = im.try_rational() {
                r.is_zero()
            } else {
                let eq_id = IdentId::_EQ;
                let res = vm.invoke_method_inner(globals, eq_id, im, &[Value::integer(0)], None, None)?;
                res.as_bool()
            }
        }
    };
    if !is_exact_zero {
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {} into {}",
            self_val.get_real_class_name(&globals.store),
            error_label
        )));
    }
    // Dispatch to the real part's coercion method.
    vm.invoke_method_inner(globals, coerce_id, c.re().get(), &[], None, None)
}

#[monoruby_builtin]
fn to_f(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    complex_to_real(vm, globals, lfp, IdentId::get_id("to_f"), "Float")
}

#[monoruby_builtin]
fn to_i(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    complex_to_real(vm, globals, lfp, IdentId::get_id("to_i"), "Integer")
}

#[monoruby_builtin]
fn to_r(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Ruby 3.4+ makes to_r more permissive than to_f/to_i: a Float 0.0
    // imaginary part is accepted. Delegate when the imaginary part is
    // *any* kind of zero (Float, Integer, Rational, or `== 0`).
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let im = c.im().get();
    let is_zero = match im.unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        RV::Float(f) => f == 0.0,
        _ => {
            if let Some(r) = im.try_rational() {
                r.is_zero()
            } else {
                let eq_id = IdentId::_EQ;
                let res = vm.invoke_method_inner(globals, eq_id, im, &[Value::integer(0)], None, None)?;
                res.as_bool()
            }
        }
    };
    if !is_zero {
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {} into Rational",
            self_val.get_real_class_name(&globals.store)
        )));
    }
    let to_r_id = IdentId::get_id("to_r");
    vm.invoke_method_inner(globals, to_r_id, c.re().get(), &[], None, None)
}

#[monoruby_builtin]
fn neg_op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let neg_id = IdentId::get_id("-@");
    let re = vm.invoke_method_inner(globals, neg_id, c.re().get(), &[], None, None)?;
    let im = vm.invoke_method_inner(globals, neg_id, c.im().get(), &[], None, None)?;
    let re_r = Real::try_from(globals, re)?;
    let im_r = Real::try_from(globals, im)?;
    Ok(Value::complex(re_r, im_r))
}

///
/// ### Complex#polar (instance)
///
/// Returns [abs, arg].
#[monoruby_builtin]
fn polar_instance(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex().to_complex_f64();
    let r = (c.re * c.re + c.im * c.im).sqrt();
    let theta = c.im.atan2(c.re);
    Ok(Value::array2(Value::float(r), Value::float(theta)))
}

///
/// ### Complex#to_c
///
/// Returns self.
#[monoruby_builtin]
fn to_c(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Complex#quo
///
/// Complex division that preserves Rational parts when dividing by an
/// integer exponent. Delegates to `real.quo(other)` / `imag.quo(other)`.
#[monoruby_builtin]
fn quo(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let other = lfp.arg(0);
    // For Complex/Complex: use normal division.
    if other.try_complex().is_some() {
        let div_id = IdentId::_DIV;
        return vm.invoke_method_inner(globals, div_id, self_val, &[other], None, None);
    }
    let quo_id = IdentId::get_id("quo");
    let re = vm.invoke_method_inner(globals, quo_id, c.re().get(), &[other], None, None)?;
    let im = vm.invoke_method_inner(globals, quo_id, c.im().get(), &[other], None, None)?;
    let re_r = Real::try_from(globals, re)?;
    let im_r = Real::try_from(globals, im)?;
    Ok(Value::complex(re_r, im_r))
}

///
/// ### Complex#rationalize
///
/// Dispatches to the real part's `rationalize` if the imaginary part is 0;
/// otherwise raises RangeError.
#[monoruby_builtin]
fn rationalize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let im = c.im().get();
    // Only Integer 0 or Rational 0 counts as "zero" for rationalize.
    let is_exact_zero = match im.unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => {
            if let Some(r) = im.try_rational() {
                r.is_zero()
            } else {
                false
            }
        }
    };
    if !is_exact_zero {
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {} into Rational",
            self_val.get_real_class_name(&globals.store)
        )));
    }
    let rationalize_id = IdentId::get_id("rationalize");
    // Forward the optional precision argument (if any) to the real part.
    let args: Vec<Value> = lfp.try_arg(0).into_iter().collect();
    vm.invoke_method_inner(globals, rationalize_id, c.re().get(), &args, None, None)
}

///
/// ### Complex#**
///
/// Only supports numeric exponents. Uses polar form for non-integer
/// exponents to avoid recursion into the generic binop path.
#[monoruby_builtin]
fn pow(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let rhs = lfp.arg(0);

    // Integer exponents: repeat multiplication in the native ComplexInner
    // representation so integer-valued parts stay Integer (CRuby compatible).
    if let RV::Fixnum(i) = rhs.unpack() {
        return pow_by_integer_exp(self_val, i);
    }
    if let RV::BigInt(b) = rhs.unpack() {
        use num::ToPrimitive;
        if let Some(i) = b.to_i64() {
            return pow_by_integer_exp(self_val, i);
        }
    }

    // Non-integer exponents: use f64 polar form.
    let base = self_val.as_complex().to_complex_f64();
    let exp = match rhs.unpack() {
        RV::Float(f) => num::complex::Complex::new(f, 0.0),
        _ => {
            if let Some(r) = rhs.try_complex() {
                r.to_complex_f64()
            } else if let Some(r) = rhs.try_rational() {
                num::complex::Complex::new(r.to_f(), 0.0)
            } else {
                // coerce protocol
                let coerce_id = IdentId::get_id("coerce");
                if globals.check_method(rhs, coerce_id).is_none() {
                    return Err(MonorubyErr::typeerr(format!(
                        "{} can't be coerced into Complex",
                        rhs.get_real_class_name(&globals.store)
                    )));
                }
                let result = vm.invoke_method_inner(globals, coerce_id, rhs, &[self_val], None, None)?;
                if let Some(ary) = result.try_array_ty() {
                    if ary.len() == 2 {
                        let pow_id = IdentId::_POW;
                        return vm.invoke_method_inner(globals, pow_id, ary[0], &[ary[1]], None, None);
                    }
                }
                return Err(MonorubyErr::typeerr(format!(
                    "{} can't be coerced into Complex",
                    rhs.get_real_class_name(&globals.store)
                )));
            }
        }
    };
    let result = base.powc(exp);
    Ok(Value::complex(Real::from(result.re), Real::from(result.im)))
}

/// Raise a Complex to an integer exponent by repeated multiplication,
/// preserving the component types (Integer stays Integer).
fn pow_by_integer_exp(self_val: Value, exp: i64) -> Result<Value> {
    if exp == 0 {
        return Ok(Value::complex(Real::from(1), Real::from(0)));
    }
    let base = self_val.as_complex().clone();
    let (mut n, invert) = if exp >= 0 {
        (exp as u64, false)
    } else {
        ((-exp) as u64, true)
    };
    let mut result = num::complex::Complex::new(Real::from(1), Real::from(0));
    let mut factor = (*base).clone();
    while n > 0 {
        if n & 1 == 1 {
            result = result * factor.clone();
        }
        n >>= 1;
        if n > 0 {
            factor = factor.clone() * factor.clone();
        }
    }
    if invert {
        // 1 / result via Complex f64 reciprocal.
        let fr = num::complex::Complex::new(result.re.to_f64(), result.im.to_f64());
        let inv = num::complex::Complex::new(1.0, 0.0) / fr;
        return Ok(Value::complex(Real::from(inv.re), Real::from(inv.im)));
    }
    Ok(Value::complex(result.re, result.im))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn complex() {
        run_test(r#"4000000000000000000000000000000+5000000000000000000000000000000i"#);
        run_test(r#"4.27+1.5i"#);
        run_test(r#"+(4.27+1.5i)"#);
        run_test(r#"-(4.27+1.5i)"#);
        run_test(r#"4-5i"#);
        run_test(r#"4000000000000000000000000000000-5000000000000000000000000000000i"#);
        run_test(r#"4.27-1.5i"#);
        let v = &["4.2-1.5i", "4+5i", "4.0+5.0i", "134", "1.34"];
        run_binop_tests(v, &["+", "-", "*", "==", "!="], v);
        run_binop_tests2(&["4.5-8.0i"], &["/"], &["0.5-2.0i"]);
        //run_test(r#"(47-15i) % (4-5i)"#);
        run_test_error(r#"(4.27-1.5i) + :5"#);
        run_test_error(r#"(4.27-1.5i) - :5"#);
        run_test_error(r#"(4.27-1.5i) * :5"#);
        run_test_error(r#"(4.5-8.0i) / :5"#);

        run_test("Complex.polar(2.0)");
        run_test("Complex.polar(2.0, 0)");
        //run_test("Complex.polar(2, 0)");
        run_test("Complex.rect(1)");
        run_test("Complex.rect(1, 2)");
        run_test("Complex.rectangular(1, 2)");
    }

    #[test]
    fn abs() {
        run_test("Complex(4, 5).abs");
        run_test("Complex(4, 5).magnitude");
    }

    #[test]
    fn rect() {
        run_test("Complex(4, 5).rect");
        run_test("Complex(4.7, 1.5).rect");
    }

    #[test]
    fn eql() {
        run_tests(&[
            // true cases: same type components
            "Complex(1, 2).eql?(Complex(1, 2))",
            "Complex(1.0, 2.0).eql?(Complex(1.0, 2.0))",
            "nil.to_c.eql?(Complex(0, 0))",
            // false cases: different component values
            "Complex(1, 2).eql?(Complex(1, 3))",
            "Complex(1, 2).eql?(Complex(3, 2))",
            // false cases: different component types (Integer vs Float)
            "Complex(1, 2).eql?(Complex(1.0, 2.0))",
            "Complex(1.0, 0.0).eql?(Complex(1, 0))",
            // false cases: non-Complex argument
            "Complex(1, 0).eql?(1)",
            "Complex(1, 0).eql?(1.0)",
            r#"Complex(1, 2).eql?("foo")"#,
            "Complex(1, 2).eql?(nil)",
            "Complex(1, 2).eql?([1, 2])",
        ]);
    }

    #[test]
    fn complex_allocate_disabled() {
        run_test_error("Complex.new(1)");
        run_test_error("Complex.allocate");
    }

    #[test]
    fn complex_coerce() {
        run_test("Complex(8,2).coerce(1.0)");
        run_test("Complex(8,2).coerce(3)");
        run_test("Complex(1,2).coerce(Complex(3,4))");
        run_test_error("Complex(1,2).coerce(:foo)");
    }

    #[test]
    fn float_quo_complex() {
        run_test("74620.09.quo(Complex(8,2))");
        run_test("1.0.fdiv(Complex(8,2))");
    }

    #[test]
    fn complex_accessors() {
        // real, imaginary (alias imag) extract components.
        run_tests(&[
            "Complex(3, 4).real",
            "Complex(3, 4).imaginary",
            "Complex(3, 4).imag",
            "Complex(3.5, -1.25).real",
            "Complex(3.5, -1.25).imaginary",
        ]);
    }

    #[test]
    fn complex_conjugate() {
        run_tests(&[
            "Complex(3, 5).conjugate.to_s",
            "Complex(3, 5).conj.to_s",
            "Complex(3, -5).conjugate.to_s",
            "Complex(0, 0).conj.to_s",
        ]);
    }

    #[test]
    fn complex_abs2() {
        run_tests(&[
            "Complex(3, 4).abs2",
            "Complex(-3, -4).abs2",
            "Complex(0, 0).abs2",
            "Complex(1.5, 2.5).abs2",
        ]);
    }

    #[test]
    fn complex_angle() {
        // Each alias returns the same value.
        run_test("Complex(1, 0).angle");
        run_test("Complex(1, 0).arg");
        run_test("Complex(1, 0).phase");
        run_test("Complex(0, 1).angle");
        run_test("Complex(-1, 0).angle");
    }

    #[test]
    fn complex_i_constant() {
        run_tests(&[
            "Complex::I.real",
            "Complex::I.imaginary",
            "(Complex::I * Complex::I).to_s",
        ]);
    }

    #[test]
    fn complex_real_q_and_integer_q() {
        run_tests(&[
            // Complex#real? always false.
            "Complex(1, 0).real?",
            "Complex(1, 2).real?",
            // Complex#integer? always false.
            "Complex(1, 0).integer?",
            "Complex(1, 2).integer?",
        ]);
    }

    #[test]
    fn complex_finite_infinite() {
        run_tests(&[
            "Complex(1, 2).finite?",
            "Complex(1, 2).infinite?",
            "Complex(Float::INFINITY, 2).finite?",
            "Complex(Float::INFINITY, 2).infinite?",
            "Complex(1, Float::INFINITY).infinite?",
        ]);
    }

    #[test]
    fn complex_cmp() {
        run_tests(&[
            // Real Complex vs real Complex.
            "Complex(5) <=> Complex(2)",
            "Complex(2) <=> Complex(3)",
            "Complex(2) <=> Complex(2)",
            // Real Complex vs real Numeric.
            "Complex(5) <=> 2",
            "Complex(2) <=> 3",
            "Complex(2) <=> 2.0",
            // Imaginary non-zero or non-numeric => nil.
            "(Complex(5, 1) <=> Complex(2)).nil?",
            "(Complex(1) <=> Complex(2, 1)).nil?",
            r#"(Complex(1) <=> "cmp").nil?"#,
        ]);
    }

    #[test]
    fn complex_integer_pow() {
        // Integer exponent preserves integer-valued parts for the common
        // non-negative-exponent cases (regression for the earlier
        // `(3+4i) ** 1 == (3.0000...+4.0i)` bug).
        run_tests(&[
            "(Complex(3, 4) ** 1).to_s",
            "(Complex(3, 4) ** 0).to_s",
            "(Complex(1, 1) ** 4).to_s",
            "(Complex(2, 0) ** 10).to_s",
        ]);
    }

    #[test]
    fn complex_fdiv() {
        run_tests(&[
            "Complex(6, 8).fdiv(2).to_s",
            "Complex(6, 8).fdiv(2.0).to_s",
            "Complex(6, 8).fdiv(-4).to_s",
            // Non-numeric argument raises TypeError.
            r#"begin; Complex(6).fdiv([]); rescue TypeError; :te; end"#,
        ]);
    }

    #[test]
    fn complex_to_f_to_i_to_r() {
        // Integer 0 imaginary is exact-zero; conversions succeed.
        run_tests(&[
            "Complex(5, 0).to_f",
            "Complex(5, 0).to_i",
            "Complex(5, 0).to_r.to_s",
            // Float 0.0 imaginary raises RangeError.
            r#"begin; Complex(5, 0.0).to_f; rescue RangeError; :re; end"#,
            r#"begin; Complex(5, 0.0).to_i; rescue RangeError; :re; end"#,
            r#"begin; Complex(5, 0.0).to_r; rescue RangeError; :re; end"#,
            // Non-zero imaginary also raises.
            r#"begin; Complex(5, 2).to_f; rescue RangeError; :re; end"#,
        ]);
    }

    #[test]
    fn complex_to_c_and_to_s_infinity_nan() {
        run_tests(&[
            // to_c returns self (object identity).
            "Complex(3, 4).to_c.equal?(Complex(3, 4).to_c.to_c)",
            // to_s inserts `*` before `i` for non-finite imaginary parts.
            "Complex(1, Float::INFINITY).to_s",
            "Complex(1, -Float::INFINITY).to_s",
            "Complex(1, Float::NAN).to_s",
        ]);
    }

    #[test]
    fn complex_unary_minus() {
        run_tests(&[
            "(-Complex(3, 4)).to_s",
            "(-Complex(-3, 4)).to_s",
            "(-Complex(0, 0)).to_s",
        ]);
    }

    #[test]
    fn complex_marshal_dump_and_hash() {
        run_tests(&[
            // marshal_dump returns [real, imaginary].
            "Complex(1, 2).send(:marshal_dump)",
            // Equal complexes have equal hashes (stable).
            "Complex(1, 2).hash == Complex(1, 2).hash",
        ]);
    }

    #[test]
    fn complex_negative_positive_undefined() {
        // Complex explicitly undefines negative? and positive?.
        run_tests(&[
            r#"begin; Complex(1).negative?; rescue NoMethodError; :nm; end"#,
            r#"begin; Complex(1).positive?; rescue NoMethodError; :nm; end"#,
            "Complex(1).methods.include?(:negative?)",
            "Complex(1).methods.include?(:positive?)",
        ]);
    }

    #[test]
    fn complex_polar_instance() {
        run_tests(&[
            "Complex(3, 4).polar.size",
            "Complex(3, 4).polar.first",
        ]);
    }

    #[test]
    fn complex_rationalize() {
        run_tests(&[
            // Integer 0 imaginary: delegates to real part.
            "Complex(3, 0).rationalize.to_s",
            "Complex(Rational(1, 2), 0).rationalize.to_s",
            // Argument is forwarded to the real part.
            "Complex(0.3, 0).rationalize(0.1).to_s",
            // Float 0.0 imaginary raises RangeError.
            r#"begin; Complex(3, 0.0).rationalize; rescue RangeError; :re; end"#,
            // Non-zero imaginary also raises.
            r#"begin; Complex(3, 1).rationalize; rescue RangeError; :re; end"#,
        ]);
    }
}
