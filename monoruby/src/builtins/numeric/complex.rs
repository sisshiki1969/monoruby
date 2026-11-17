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
    // `Complex#/` and `Complex#quo` are the same (aliased) method in CRuby
    // (`Complex.instance_method(:quo) == Complex.instance_method(:/)`), so
    // register one implementation under both names.
    globals.define_builtin_funcs(COMPLEX_CLASS, "/", &["quo"], div, 1);
    globals.define_builtin_func_with(COMPLEX_CLASS, "rationalize", rationalize, 0, 1, false);
    globals.define_builtin_func(COMPLEX_CLASS, "real?", real_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "finite?", finite_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "infinite?", infinite_q, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "fdiv", fdiv, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "**", pow, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "numerator", numerator, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "denominator", denominator, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "hash", hash, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "marshal_dump", marshal_dump, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_f", to_f, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_r", to_r, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "-@", neg_op, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "to_s", complex_to_s, 0);
    globals.define_builtin_func(COMPLEX_CLASS, "inspect", complex_inspect, 0);
    globals.store[COMPLEX_CLASS].clear_alloc_func();
    globals.define_builtin_func(COMPLEX_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "coerce", coerce, 1);

    // Complex::I = Complex(0, 1)
    let i_const = Value::complex(Real::from(0), Real::from(1));
    globals.set_constant_by_str(COMPLEX_CLASS, "I", i_const);
}

/// `Complex#==` following CRuby's `nucomp_eqeq_p` protocol:
/// 1. `other` is a Complex          → compare real & imaginary parts.
/// 2. `other` is a real Numeric     → `self.real == other && self.imag.zero?`
///    (the comparison is delegated to `self.real.==`, which may be a
///    user object).
/// 3. otherwise                     → `other == self` (`rb_equal`).
fn complex_eq(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    other: Value,
) -> Result<bool> {
    let lhs = self_val.as_complex();
    let (re, im) = (lhs.re().get(), lhs.im().get());
    if let Some(rhs) = other.try_complex() {
        let (rhs_re, rhs_im) = (rhs.re().get(), rhs.im().get());
        if [re, im, rhs_re, rhs_im].iter().all(|v| is_builtin_real(*v)) {
            return Ok(&*lhs == rhs);
        }
        // A part that is a user-defined Numeric is asked (`f_eqeq_p`).
        return Ok(rb_equal(vm, globals, re, rhs_re)? && rb_equal(vm, globals, im, rhs_im)?);
    }
    if is_real_like_numeric(vm, globals, other)? {
        return Ok(rb_equal(vm, globals, re, other)? && f_zero_p(vm, globals, im)?);
    }
    let r = vm.eq_values_funcall(globals, other, self_val)?;
    Ok(r.as_bool())
}

/// `self` as an error message shows it: through `Complex#to_s`, which
/// asks a part that is a user-defined Numeric for its own `to_s`.
fn message_to_s(vm: &mut Executor, globals: &mut Globals, self_val: Value) -> Result<String> {
    let s = vm.invoke_method_inner(globals, IdentId::TO_S, self_val, &[], None, None)?;
    Ok(match s.is_rstring() {
        Some(s) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
        None => self_val.to_s(&globals.store),
    })
}

/// CRuby's `rb_equal`: the same object is equal without being asked.
fn rb_equal(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<bool> {
    Ok(x.id() == y.id() || vm.eq_values_funcall(globals, x, y)?.as_bool())
}

/// CRuby's `rb_num_coerce_cmp(x, y, <=>)`: `y.coerce(x)` supplies the two
/// operands; nil when `y` cannot coerce.
fn num_coerce_cmp(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<Value> {
    crate::value::coerce::num_coerce_cmp(vm, globals, IdentId::_CMP, x, y)
}

/// Integer, Float or Rational: a part of a Complex whose value is read
/// without calling a method.
fn is_builtin_real(v: Value) -> bool {
    v.try_fixnum().is_some()
        || v.try_float().is_some()
        || matches!(v.ty(), Some(ObjTy::BIGNUM | ObjTy::RATIONAL))
}

/// CRuby's `f_zero_p`: a part that is a user-defined Numeric is asked
/// `== 0`.
fn f_zero_p(vm: &mut Executor, globals: &mut Globals, x: Value) -> Result<bool> {
    Ok(match x.unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        RV::Float(f) => f == 0.0,
        _ => {
            if let Some(r) = x.try_rational() {
                r.is_zero()
            } else {
                rb_equal(vm, globals, x, Value::integer(0))?
            }
        }
    })
}

/// CRuby's `k_exact_zero_p`: zero and not a Float.
fn k_exact_zero_p(vm: &mut Executor, globals: &mut Globals, x: Value) -> Result<bool> {
    Ok(!matches!(x.unpack(), RV::Float(_)) && f_zero_p(vm, globals, x)?)
}

///
/// ### Complex#==
///
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(complex_eq(vm, globals, lfp.self_val(), lfp.arg(0))?))
}

///
/// ### Complex.polar
///
/// - polar(r, theta = 0) -> Complex
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/s/polar.html]
#[monoruby_builtin]
fn complex_polar(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Both arguments accept a real Numeric or a zero-imaginary Complex
    // (CRuby's `nucomp_real_check`).
    let abs = real_component_for_rect(vm, globals, lfp.arg(0))?;
    let arg = if let Some(theta) = lfp.try_arg(1) {
        real_component_for_rect(vm, globals, theta)?
    } else {
        Real::from(0)
    };
    // `f_complex_polar_real`: when either part is zero the magnitude type is
    // preserved (imaginary becomes Float 0.0); otherwise use the float form.
    if abs.is_zero() || arg.is_zero() {
        return Ok(Value::complex(abs, Real::from(0.0)));
    }
    let c = num::complex::Complex::from_polar(abs.to_f64(), arg.to_f64());
    Ok(Value::complex(Real::from(c.re), Real::from(c.im)))
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
    let r = real_component_for_rect(vm, globals, lfp.arg(0))?;
    let i = if let Some(i_arg) = lfp.try_arg(1) {
        real_component_for_rect(vm, globals, i_arg)?
    } else {
        Real::from(0)
    };
    Ok(Value::complex(r, i))
}

/// Extract the real component to store for `Complex.rect`. Accepts a built-in
/// real (Integer/Float/Rational), a Complex whose imaginary part is zero (its
/// real part is used, mirroring CRuby's `nucomp_real_check` + canonicalize),
/// or a Numeric subclass whose `real?` returns true. Anything else raises
/// `TypeError: not a real`.
fn real_component_for_rect(
    vm: &mut Executor,
    globals: &mut Globals,
    value: Value,
) -> Result<Real> {
    match value.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) => return Real::try_from(globals, value),
        _ => {}
    }
    if value.try_rational().is_some() {
        return Real::try_from(globals, value);
    }
    if let Some(c) = value.try_complex() {
        if c.im().is_zero() {
            return Ok(c.re());
        }
        return Err(MonorubyErr::typeerr("not a real"));
    }
    // Numeric subclass responding to real? — honour that answer.
    let real_q_id = IdentId::get_id("real?");
    if globals.check_method(value, real_q_id).is_some() {
        let v = vm.invoke_method_inner(globals, real_q_id, value, &[], None, None)?;
        if v.as_bool() {
            return Real::try_from(globals, value);
        }
    }
    Err(MonorubyErr::typeerr("not a real"))
}

///
/// ### Complex#abs
///
/// - abs -> Numeric
/// - magnitude -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/abs.html]
#[monoruby_builtin]
fn abs(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let (re, im) = (c.re().get(), c.im().get());
    // `rb_complex_abs`: with a zero part the answer is the other part's
    // own `abs` (`Complex(3, 0).abs` is `3`, `Complex(1r/3, 0).abs` is
    // `(1/3)`), a Float only when the zero part was one.
    let (zero, other) = if f_zero_p(vm, globals, re)? {
        (re, im)
    } else if f_zero_p(vm, globals, im)? {
        (im, re)
    } else {
        return Ok(Value::float(c.to_complex_f64().norm()));
    };
    let a = f_abs(vm, globals, other)?;
    if zero.try_float().is_some() && a.try_float().is_none() {
        return vm.invoke_method_inner(globals, IdentId::get_id("to_f"), a, &[], None, None);
    }
    Ok(a)
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
    // Rational: `Complex(other, 0)` with the Rational kept as it is
    // (`nucomp_coerce`), so that `(1r/3) + Complex(1, 2)` stays exact.
    if other.try_rational().is_some() {
        let re = Real::try_from(globals, other)?;
        let wrapped = Value::complex(re, Real::from(0));
        return Ok(Value::array2(wrapped, self_val));
    }
    // Numeric subclass with real? → true: wrap as Complex(other, 0).
    if is_real_like_numeric(vm, globals, other)? {
        let re = Real::try_from(globals, other)?;
        let wrapped = Value::complex(re, Real::from(0));
        return Ok(Value::array2(wrapped, self_val));
    }
    // `Complex#coerce` itself names the operand by class (CRuby's
    // `nucomp_coerce`), unlike the operators' `coerce_failed`.
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
    // `f_real_p`: an Integer, Float or Rational is not asked.
    if is_builtin_real(other) {
        return Ok(true);
    }
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
fn conjugate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    if !is_native_part(c.im().get()) {
        // A Rational imaginary part is negated as a Rational.
        let neg = IdentId::get_id("-@");
        let im = vm.invoke_method_inner(globals, neg, c.im().get(), &[], None, None)?;
        return Ok(Value::complex(c.re(), Real::try_from(globals, im)?));
    }
    Ok(Value::complex(c.re(), -c.im()))
}

///
/// ### Complex#abs2
///
/// Returns |z|^2 = re^2 + im^2. Preserves the component type (Integer
/// stays Integer) by delegating through Real arithmetic.
#[monoruby_builtin]
fn abs2(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    if has_exact_part(c) {
        let (re, im) = (c.re().get(), c.im().get());
        return vm.with_temp_scope(|vm| {
            let re_sq = f_mul(vm, globals, re, re)?;
            vm.temp_push(re_sq);
            let im_sq = f_mul(vm, globals, im, im)?;
            vm.temp_push(im_sq);
            f_add(vm, globals, re_sq, im_sq)
        });
    }
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
    // `nucomp_cmp`.
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if !is_builtin_real(other) && !other.is_kind_of(&globals.store, NUMERIC_CLASS) {
        return num_coerce_cmp(vm, globals, self_val, other);
    }
    let lhs = self_val.as_complex();
    let (lhs_re, lhs_im) = (lhs.re().get(), lhs.im().get());
    // If self has an imaginary part, no ordering exists.
    if !f_zero_p(vm, globals, lhs_im)? {
        return Ok(Value::nil());
    }
    if let Some(rhs) = other.try_complex() {
        let (rhs_re, rhs_im) = (rhs.re().get(), rhs.im().get());
        if !f_zero_p(vm, globals, rhs_im)? {
            return Ok(Value::nil());
        }
        return vm.invoke_method_inner(globals, IdentId::_CMP, lhs_re, &[rhs_re], None, None);
    }
    if is_real_like_numeric(vm, globals, other)? {
        vm.invoke_method_inner(globals, IdentId::_CMP, lhs_re, &[other], None, None)
    } else {
        num_coerce_cmp(vm, globals, lhs_re, other)
    }
}

///
/// ### Complex#fdiv
///
/// CRuby's `f_divide(self, other, f_fdiv, id_fdiv)`: the division of
/// `Complex#/` with every `quo` replaced by `fdiv`.
#[monoruby_builtin]
fn fdiv(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    f_divide(
        vm,
        globals,
        lfp.self_val(),
        lfp.arg(0),
        IdentId::get_id("fdiv"),
    )
}

///
/// ### Complex#numerator
///
#[monoruby_builtin]
fn numerator(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    // Every intermediate can be a heap numeric (Bignum/Rational) that
    // only these Rust locals reference across the following dispatches —
    // keep each rooted for the duration.
    vm.with_temp_scope(|vm| {
        let d = complex_denominator(vm, globals, c)?;
        vm.temp_push(d);
        let num_id = IdentId::get_id("numerator");
        let div_id = IdentId::_DIV;
        let mul_id = IdentId::_MUL;
        let den_id = IdentId::get_id("denominator");
        let re_num = vm.invoke_method_inner(globals, num_id, c.re().get(), &[], None, None)?;
        vm.temp_push(re_num);
        let re_den = vm.invoke_method_inner(globals, den_id, c.re().get(), &[], None, None)?;
        vm.temp_push(re_den);
        let im_num = vm.invoke_method_inner(globals, num_id, c.im().get(), &[], None, None)?;
        vm.temp_push(im_num);
        let im_den = vm.invoke_method_inner(globals, den_id, c.im().get(), &[], None, None)?;
        vm.temp_push(im_den);
        let re_scale = vm.invoke_method_inner(globals, div_id, d, &[re_den], None, None)?;
        vm.temp_push(re_scale);
        let im_scale = vm.invoke_method_inner(globals, div_id, d, &[im_den], None, None)?;
        vm.temp_push(im_scale);
        let re = vm.invoke_method_inner(globals, mul_id, re_num, &[re_scale], None, None)?;
        vm.temp_push(re);
        let im = vm.invoke_method_inner(globals, mul_id, im_num, &[im_scale], None, None)?;
        let re_r = Real::try_from(globals, re)?;
        let im_r = Real::try_from(globals, im)?;
        Ok(Value::complex(re_r, im_r))
    })
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
/// ### Complex#hash
///
/// Hash is based on real and imaginary parts' values (stable across instances).
#[monoruby_builtin]
fn hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let mut hasher = crate::value::seeded_hasher();
    c.re().hash(&mut hasher);
    c.im().hash(&mut hasher);
    Ok(Value::from_hash_digest(hasher.finish()))
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
    if !k_exact_zero_p(vm, globals, im)? {
        let this = message_to_s(vm, globals, self_val)?;
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {this} into {error_label}"
        )));
    }
    // Dispatch to the real part's coercion method.
    vm.invoke_method_inner(globals, coerce_id, c.re().get(), &[], None, None)
}

#[monoruby_builtin]
fn to_f(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    complex_to_real(vm, globals, lfp, IdentId::TO_F, "Float")
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
    if !f_zero_p(vm, globals, im)? {
        let this = message_to_s(vm, globals, self_val)?;
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {this} into Rational"
        )));
    }
    let to_r_id = IdentId::get_id("to_r");
    vm.invoke_method_inner(globals, to_r_id, c.re().get(), &[], None, None)
}

#[monoruby_builtin]
fn neg_op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let c = self_val.as_complex();
    let neg_id = IdentId::_UMINUS;
    // `re` can be a heap numeric only this Rust local references while
    // the second `-@` dispatch runs — root it.
    vm.with_temp_scope(|vm| {
        let re = vm.invoke_method_inner(globals, neg_id, c.re().get(), &[], None, None)?;
        vm.temp_push(re);
        let im = vm.invoke_method_inner(globals, neg_id, c.im().get(), &[], None, None)?;
        let re_r = Real::try_from(globals, re)?;
        let im_r = Real::try_from(globals, im)?;
        Ok(Value::complex(re_r, im_r))
    })
}

/// Format a Complex following CRuby's `nucomp_to_s` / `nucomp_inspect`.
/// `to_s`/`inspect` are dispatched to the (possibly user-defined) real
/// and imaginary parts; the imaginary sign uses signbit semantics (so
/// `-0.0` renders as `-`); and a `*` is inserted before the trailing
/// `i` when the rendered imaginary part does not end in a digit
/// (matching CRuby's `nucomp_to_s`).
fn format_complex(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    inspect: bool,
) -> Result<Value> {
    let c = self_val.as_complex();
    let re = c.re().get();
    let im = c.im().get();
    let method = if inspect {
        IdentId::INSPECT
    } else {
        IdentId::TO_S
    };
    let dispatch = |vm: &mut Executor, globals: &mut Globals, v: Value| -> Result<String> {
        let s = vm.invoke_method_inner(globals, method, v, &[], None, None)?;
        Ok(s.expect_string(&globals.store)?)
    };

    let re_str = dispatch(vm, globals, re)?;

    // Sign: signbit for Float (catches -0.0), else `im < 0`.
    let negative = match im.unpack() {
        RV::Float(f) => f.is_sign_negative() && !f.is_nan(),
        _ => {
            // CRuby's `f_negative_p` drives `imag < 0` and propagates
            // any error the comparison raises.
            let lt = IdentId::get_id("<");
            vm.invoke_method_inner(globals, lt, im, &[Value::integer(0)], None, None)?
                .as_bool()
        }
    };

    // For the negative case CRuby renders `-imag` after emitting the
    // `-` sign, so the magnitude is printed without its own sign.
    let (sign, im_for_fmt) = if negative {
        let neg = vm.invoke_method_inner(globals, IdentId::_UMINUS, im, &[], None, None)?;
        ("-", neg)
    } else {
        ("+", im)
    };
    let im_str = dispatch(vm, globals, im_for_fmt)?;

    let star = match im_str.chars().last() {
        Some(ch) if ch.is_ascii_digit() => "",
        _ => "*",
    };

    let body = format!("{}{}{}{}i", re_str, sign, im_str, star);
    let out = if inspect { format!("({})", body) } else { body };
    Ok(Value::string_usascii_from_str(&out))
}

#[monoruby_builtin]
fn complex_to_s(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    format_complex(vm, globals, lfp.self_val(), false)
}

#[monoruby_builtin]
fn complex_inspect(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    format_complex(vm, globals, lfp.self_val(), true)
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

/// An Integer or a Float: a part `num::complex::Complex<Real>` computes
/// as CRuby does. A Rational (or a user-defined Numeric) part has to go
/// through its own operators, since `RealKind` reads it as a Float.
#[inline(always)]
fn is_native_part(v: Value) -> bool {
    v.is_fixnum() || v.is_flonum() || matches!(v.ty(), Some(ObjTy::BIGNUM | ObjTy::FLOAT))
}

/// Whether a part of `c` is neither an Integer nor a Float. The
/// operators ask this of every Complex operand before they compute in
/// `Real`s, so it is a few tag tests.
#[inline(always)]
pub(crate) fn has_exact_part(c: &num::complex::Complex<Real>) -> bool {
    !(is_native_part(c.re.get()) && is_native_part(c.im.get()))
}

/// `Complex#+` / `#-` / `#*` when a part of an operand is neither an
/// Integer nor a Float ([`has_exact_part`]) and the other operand is a
/// Complex, an Integer or a Float: CRuby's `nucomp_add` / `nucomp_sub` /
/// `rb_complex_mul` over the parts' own operators, so that
/// `Complex(1r/3, 1r/2) + 1` keeps its Rational parts.
pub(crate) fn exact_parts_op(
    vm: &mut Executor,
    globals: &mut Globals,
    op: IdentId,
    lhs: Value,
    rhs: Value,
) -> Result<Value> {
    let parts = |c: &ComplexInner| (c.re().get(), c.im().get());
    let (a, b) = match (lhs.try_complex(), rhs.try_complex()) {
        (Some(l), Some(r)) => (parts(l), parts(r)),
        // A fixnum `+` / `*` a Complex is the Complex's own operator
        // with the operands swapped (`fix_plus`, `fix_mul`).
        (None, Some(r)) if lhs.is_fixnum() && op != IdentId::_SUB => {
            return with_real(vm, globals, op, parts(r), lhs);
        }
        // Any other Integer or Float coerces: `Complex(lhs, 0) op rhs`.
        (None, Some(r)) => ((lhs, Value::integer(0)), parts(r)),
        (Some(l), None) => return with_real(vm, globals, op, parts(l), rhs),
        // Not what the operators hand over: the operator itself answers.
        (None, None) => return vm.invoke_method_inner(globals, op, lhs, &[rhs], None, None),
    };
    vm.with_temp_scope(|vm| {
        let (re, im) = if op == IdentId::_MUL {
            comp_mul(vm, globals, a, b)?
        } else {
            let re = vm.invoke_method_inner(globals, op, a.0, &[b.0], None, None)?;
            vm.temp_push(re);
            let im = vm.invoke_method_inner(globals, op, a.1, &[b.1], None, None)?;
            (re, im)
        };
        Ok(Value::complex(
            Real::try_from(globals, re)?,
            Real::try_from(globals, im)?,
        ))
    })
}

/// A Complex and a real operand: the real part alone for `+` / `-`, each
/// part for `*`.
fn with_real(
    vm: &mut Executor,
    globals: &mut Globals,
    op: IdentId,
    (a_re, a_im): (Value, Value),
    real: Value,
) -> Result<Value> {
    vm.with_temp_scope(|vm| {
        let re = vm.invoke_method_inner(globals, op, a_re, &[real], None, None)?;
        vm.temp_push(re);
        let im = if op == IdentId::_MUL {
            vm.invoke_method_inner(globals, op, a_im, &[real], None, None)?
        } else {
            a_im
        };
        Ok(Value::complex(
            Real::try_from(globals, re)?,
            Real::try_from(globals, im)?,
        ))
    })
}

/// CRuby's `comp_mul`. The results are left on the temp stack of the
/// caller's scope.
fn comp_mul(
    vm: &mut Executor,
    globals: &mut Globals,
    (a_re, a_im): (Value, Value),
    (b_re, b_im): (Value, Value),
) -> Result<(Value, Value)> {
    let arz = f_zero_p(vm, globals, a_re)?;
    let aiz = f_zero_p(vm, globals, a_im)?;
    let brz = f_zero_p(vm, globals, b_re)?;
    let biz = f_zero_p(vm, globals, b_im)?;
    let rr = safe_multiply(vm, globals, a_re, b_re, arz, brz)?;
    vm.temp_push(rr);
    let ii = safe_multiply(vm, globals, a_im, b_im, aiz, biz)?;
    vm.temp_push(ii);
    let re = f_sub(vm, globals, rr, ii)?;
    vm.temp_push(re);
    let ri = safe_multiply(vm, globals, a_re, b_im, arz, biz)?;
    vm.temp_push(ri);
    let ir = safe_multiply(vm, globals, a_im, b_re, aiz, brz)?;
    vm.temp_push(ir);
    let im = f_add(vm, globals, ri, ir)?;
    vm.temp_push(im);
    Ok((re, im))
}

/// CRuby's `safe_multiply`: a non-NaN Float multiplied by a zero counts
/// only for its sign, so that an infinite part times zero is not NaN.
fn safe_multiply(
    vm: &mut Executor,
    globals: &mut Globals,
    a: Value,
    b: Value,
    a_zero: bool,
    b_zero: bool,
) -> Result<Value> {
    let unit = |v: Value| match v.try_float() {
        Some(f) if !f.is_nan() => Some(Value::float(1.0f64.copysign(f))),
        _ => None,
    };
    let a = if !a_zero && b_zero {
        unit(a).unwrap_or(a)
    } else {
        a
    };
    let b = if !b_zero && a_zero {
        unit(b).unwrap_or(b)
    } else {
        b
    };
    f_mul(vm, globals, a, b)
}

fn f_add(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<Value> {
    vm.invoke_method_inner(globals, IdentId::_ADD, x, &[y], None, None)
}

fn f_sub(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<Value> {
    vm.invoke_method_inner(globals, IdentId::_SUB, x, &[y], None, None)
}

fn f_mul(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<Value> {
    vm.invoke_method_inner(globals, IdentId::_MUL, x, &[y], None, None)
}

fn f_abs(vm: &mut Executor, globals: &mut Globals, x: Value) -> Result<Value> {
    let abs_id = IdentId::get_id("abs");
    vm.invoke_method_inner(globals, abs_id, x, &[], None, None)
}

/// `x > y` (CRuby's `f_gt_p`).
fn f_gt(vm: &mut Executor, globals: &mut Globals, x: Value, y: Value) -> Result<bool> {
    let gt_id = IdentId::get_id(">");
    Ok(vm
        .invoke_method_inner(globals, gt_id, x, &[y], None, None)?
        .as_bool())
}

/// CRuby's `rb_rational_canonicalize`: a Rational whose denominator is 1 is
/// reduced to its (Integer) numerator; anything else is returned unchanged.
/// Complex division applies this to each resulting component so that e.g.
/// `Complex(6, 8) / 4` yields `((3/2)+2i)` (the imaginary `(2/1)` becomes `2`).
fn rational_canonicalize(value: Value) -> Value {
    if let Some(r) = value.try_rational() {
        if r.den().is_one() {
            return r.num_as_value();
        }
    }
    value
}

/// Is `other` a real divisor that `Complex#/` divides component-wise?
/// Mirrors CRuby's `k_numeric_p(other) && f_real_p(other)`: built-in Integer /
/// Float / Rational, or a Numeric subclass whose `real?` returns true.
fn is_real_divisor(vm: &mut Executor, globals: &mut Globals, other: Value) -> Result<bool> {
    if matches!(other.unpack(), RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_))
        || other.try_rational().is_some()
    {
        return Ok(true);
    }
    is_real_like_numeric(vm, globals, other)
}

///
/// ### Complex#/
/// ### Complex#quo
///
/// Complex division, following CRuby's `f_divide` (used for both `/` and the
/// aliased `quo`). Integer/Rational components divide exactly (producing
/// Rational parts), while a Float anywhere makes the result Float; the
/// numerically stable Smith's algorithm matches CRuby's component arithmetic
/// and rounding.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/=2f.html]
#[monoruby_builtin]
fn div(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    f_divide(
        vm,
        globals,
        lfp.self_val(),
        lfp.arg(0),
        IdentId::get_id("quo"),
    )
}

/// CRuby's `f_divide`: `Complex#/` (`quo`) and `Complex#fdiv` (`fdiv`)
/// differ only in the method that divides two parts.
fn f_divide(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    other: Value,
    quo: IdentId,
) -> Result<Value> {
    let f_quo = |vm: &mut Executor, globals: &mut Globals, x: Value, y: Value| {
        vm.invoke_method_inner(globals, quo, x, &[y], None, None)
    };
    let (a_re, a_im) = {
        let c = self_val.as_complex();
        (c.re().get(), c.im().get())
    };

    // Complex / Complex: Smith's algorithm using `quo` (`fdiv`) on the
    // components.
    // Every intermediate can be a heap numeric (Rational/Bignum) that
    // only a Rust local references while the next `quo`/`*`/`+` dispatch
    // runs Ruby — root each until the components are extracted.
    if let Some(o) = other.try_complex() {
        let (b_re, b_im) = (o.re().get(), o.im().get());
        let one = Value::integer(1);
        return vm.with_temp_scope(|vm| {
            let abs_re = f_abs(vm, globals, b_re)?;
            vm.temp_push(abs_re);
            let abs_im = f_abs(vm, globals, b_im)?;
            vm.temp_push(abs_im);
            let (mut x, mut y);
            if f_gt(vm, globals, abs_re, abs_im)? {
                // r = b_im / b_re ; n = b_re * (1 + r*r)
                let r = f_quo(vm, globals, b_im, b_re)?;
                vm.temp_push(r);
                let rr = f_mul(vm, globals, r, r)?;
                vm.temp_push(rr);
                let one_plus = f_add(vm, globals, one, rr)?;
                vm.temp_push(one_plus);
                let n = f_mul(vm, globals, b_re, one_plus)?;
                vm.temp_push(n);
                // x = (a_re + a_im*r) / n ; y = (a_im - a_re*r) / n
                let aim_r = f_mul(vm, globals, a_im, r)?;
                vm.temp_push(aim_r);
                let num_x = f_add(vm, globals, a_re, aim_r)?;
                vm.temp_push(num_x);
                let are_r = f_mul(vm, globals, a_re, r)?;
                vm.temp_push(are_r);
                let num_y = f_sub(vm, globals, a_im, are_r)?;
                vm.temp_push(num_y);
                x = f_quo(vm, globals, num_x, n)?;
                vm.temp_push(x);
                y = f_quo(vm, globals, num_y, n)?;
            } else {
                // r = b_re / b_im ; n = b_im * (1 + r*r)
                let r = f_quo(vm, globals, b_re, b_im)?;
                vm.temp_push(r);
                let rr = f_mul(vm, globals, r, r)?;
                vm.temp_push(rr);
                let one_plus = f_add(vm, globals, one, rr)?;
                vm.temp_push(one_plus);
                let n = f_mul(vm, globals, b_im, one_plus)?;
                vm.temp_push(n);
                // x = (a_re*r + a_im) / n ; y = (a_im*r - a_re) / n
                let are_r = f_mul(vm, globals, a_re, r)?;
                vm.temp_push(are_r);
                let num_x = f_add(vm, globals, are_r, a_im)?;
                vm.temp_push(num_x);
                let aim_r = f_mul(vm, globals, a_im, r)?;
                vm.temp_push(aim_r);
                let num_y = f_sub(vm, globals, aim_r, a_re)?;
                vm.temp_push(num_y);
                x = f_quo(vm, globals, num_x, n)?;
                vm.temp_push(x);
                y = f_quo(vm, globals, num_y, n)?;
            }
            x = rational_canonicalize(x);
            y = rational_canonicalize(y);
            let re_r = Real::try_from(globals, x)?;
            let im_r = Real::try_from(globals, y)?;
            Ok(Value::complex(re_r, im_r))
        });
    }

    // Complex / real: divide each component by the real divisor. `x`
    // must survive the second `quo` dispatch — root it.
    if is_real_divisor(vm, globals, other)? {
        return vm.with_temp_scope(|vm| {
            let x = rational_canonicalize(f_quo(vm, globals, a_re, other)?);
            vm.temp_push(x);
            let y = rational_canonicalize(f_quo(vm, globals, a_im, other)?);
            let re_r = Real::try_from(globals, x)?;
            let im_r = Real::try_from(globals, y)?;
            Ok(Value::complex(re_r, im_r))
        });
    }

    // Otherwise fall back to the coerce protocol (`rb_num_coerce_bin`). The
    // re-dispatch uses `quo` / `fdiv` (the id CRuby passes to `f_divide`),
    // so e.g. `coerce` returning `[5, 2]` yields `5.quo(2) == (5/2)`.
    match crate::value::coerce::do_coerce(vm, globals, self_val, other, false)? {
        Some((x, y)) => f_quo(vm, globals, x, y),
        None => Err(MonorubyErr::typeerr(format!(
            "{} can't be coerced into Complex",
            other.coerce_failed_name(&globals.store)
        ))),
    }
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
    // An exact zero: Integer 0, Rational 0, or a user-defined Numeric
    // that answers `== 0` (`k_exact_zero_p`). Float 0.0 is not.
    if !k_exact_zero_p(vm, globals, im)? {
        let this = message_to_s(vm, globals, self_val)?;
        return Err(MonorubyErr::rangeerr(format!(
            "can't convert {this} into Rational"
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
    let mut rhs = lfp.arg(0);
    // `rb_complex_pow`'s canonicalization of the exponent: a Rational
    // with denominator 1 is its numerator, a Complex with an exact zero
    // imaginary part its real part.
    rhs = rational_canonicalize(rhs);
    if let Some(c) = rhs.try_complex()
        && k_exact_zero_p(vm, globals, c.im().get())?
    {
        rhs = c.re().get();
    }

    // Integer exponents: repeat multiplication in the native ComplexInner
    // representation so integer-valued parts stay Integer (CRuby compatible).
    if let RV::Fixnum(i) = rhs.unpack() {
        let c = self_val.as_complex();
        // On an axis or a diagonal (`complex_pow_for_special_angle`).
        let special = {
            let (re, im) = (c.re(), c.im());
            re.is_zero() || im.is_zero() || re == im || re == -im
        };
        if i <= 1 || special || has_exact_part(c) {
            return pow_exact_parts(vm, globals, self_val, i);
        }
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
                return crate::value::coerce::num_coerce_bin(
                    vm,
                    globals,
                    IdentId::_POW,
                    self_val,
                    rhs,
                    "Complex",
                );
            }
        }
    };
    let result = base.powc(exp);
    Ok(Value::complex(Real::from(result.re), Real::from(result.im)))
}

/// Raise a Complex to an integer exponent by repeated multiplication,
/// preserving the component types (Integer stays Integer).
/// `Complex#**` with a fixnum exponent as `rb_complex_pow` computes it,
/// over the parts' own operators: used for a Complex with a Rational (or
/// user-defined) part, and for the exponents the `Real` arithmetic of
/// `pow_by_integer_exp` answers differently (a negative one is exact for
/// Integer parts, `1` keeps the parts as they are).
fn pow_exact_parts(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    exp: i64,
) -> Result<Value> {
    let c = self_val.as_complex();
    let (re, im) = (c.re().get(), c.im().get());
    if exp == 0 {
        return Ok(Value::complex(Real::from(1), Real::from(0)));
    }
    if exp == 1 {
        return Ok(Value::complex(c.re(), c.im()));
    }
    vm.with_temp_scope(|vm| {
        if let Some(v) = pow_for_special_angle(vm, globals, re, im, exp)? {
            return Ok(v);
        }
        // `f_reciprocal` for a negative exponent.
        let (mut xr, mut xi) = if exp < 0 {
            let quo = IdentId::get_id("quo");
            let inv =
                vm.invoke_method_inner(globals, quo, Value::integer(1), &[self_val], None, None)?;
            vm.temp_push(inv);
            match inv.try_complex() {
                Some(c) => (c.re().get(), c.im().get()),
                None => return Err(MonorubyErr::typeerr("not a Complex")),
            }
        } else {
            (re, im)
        };
        let (mut zr, mut zi) = (xr, xi);
        let two = Value::integer(2);
        let mut n = exp.unsigned_abs() - 1;
        while n > 0 {
            while n % 2 == 0 {
                let rr = f_mul(vm, globals, xr, xr)?;
                vm.temp_push(rr);
                let ii = f_mul(vm, globals, xi, xi)?;
                vm.temp_push(ii);
                let tmp = f_sub(vm, globals, rr, ii)?;
                vm.temp_push(tmp);
                let r2 = f_mul(vm, globals, two, xr)?;
                vm.temp_push(r2);
                xi = f_mul(vm, globals, r2, xi)?;
                vm.temp_push(xi);
                xr = tmp;
                n /= 2;
            }
            (zr, zi) = comp_mul(vm, globals, (zr, zi), (xr, xi))?;
            n -= 1;
        }
        Ok(Value::complex(
            Real::try_from(globals, zr)?,
            Real::try_from(globals, zi)?,
        ))
    })
}

/// CRuby's `complex_pow_for_special_angle`: a Complex on an axis or a
/// diagonal raised to an Integer is one real power placed by the angle.
fn pow_for_special_angle(
    vm: &mut Executor,
    globals: &mut Globals,
    re: Value,
    im: Value,
    exp: i64,
) -> Result<Option<Value>> {
    let f_negate = |vm: &mut Executor, globals: &mut Globals, x: Value| {
        vm.invoke_method_inner(globals, IdentId::get_id("-@"), x, &[], None, None)
    };
    let (mut x, mut dir) = if f_zero_p(vm, globals, im)? {
        (re, 0)
    } else if f_zero_p(vm, globals, re)? {
        (im, 2)
    } else if rb_equal(vm, globals, re, im)? {
        (re, 1)
    } else {
        let neg = f_negate(vm, globals, im)?;
        vm.temp_push(neg);
        if !rb_equal(vm, globals, re, neg)? {
            return Ok(None);
        }
        (im, 3)
    };
    let negative = vm
        .invoke_method_inner(globals, IdentId::get_id("negative?"), x, &[], None, None)?
        .as_bool();
    if negative {
        x = f_negate(vm, globals, x)?;
        vm.temp_push(x);
        dir += 4;
    }
    let pow = IdentId::_POW;
    let zx = if dir % 2 == 0 {
        vm.invoke_method_inner(globals, pow, x, &[Value::integer(exp)], None, None)?
    } else {
        let x2 = f_mul(vm, globals, Value::integer(2), x)?;
        vm.temp_push(x2);
        let xx = f_mul(vm, globals, x2, x)?;
        vm.temp_push(xx);
        let half = Value::integer(exp.div_euclid(2));
        let zx = vm.invoke_method_inner(globals, pow, xx, &[half], None, None)?;
        if exp.rem_euclid(2) == 1 {
            vm.temp_push(zx);
            f_mul(vm, globals, zx, x)?
        } else {
            zx
        }
    };
    vm.temp_push(zx);
    const DIRS: [(i8, i8); 8] = [
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
        (0, -1),
        (1, -1),
    ];
    let (dr, di) = DIRS[(dir as i128 * exp as i128).rem_euclid(8) as usize];
    let mut part = |vm: &mut Executor, globals: &mut Globals, d: i8| -> Result<Value> {
        Ok(match d {
            1 => zx,
            -1 => {
                let v = f_negate(vm, globals, zx)?;
                vm.temp_push(v);
                v
            }
            // `zero_for`: a zero of the kind `zx` is.
            _ => {
                if zx.try_float().is_some() {
                    Value::float(0.0)
                } else if zx.try_rational().is_some() {
                    let v = vm.invoke_method_inner(
                        globals,
                        IdentId::get_id("to_r"),
                        Value::integer(0),
                        &[],
                        None,
                        None,
                    )?;
                    vm.temp_push(v);
                    v
                } else {
                    Value::integer(0)
                }
            }
        })
    };
    let zr = part(vm, globals, dr)?;
    let zi = part(vm, globals, di)?;
    Ok(Some(Value::complex(
        Real::try_from(globals, zr)?,
        Real::try_from(globals, zi)?,
    )))
}

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
    fn complex_numeric_protocol() {
        // `+`/`-`/`*` with a Rational operate component-wise (real part for
        // +/-, both parts for *).
        run_tests(&[
            "(Complex(1, 2) + Rational(1, 2)).to_s",
            "(Complex(1, 2) - Rational(1, 2)).to_s",
            "(Complex(1, 2) * Rational(1, 2)).to_s",
            "(Complex(1, 2) + 3).to_s",
            "(Complex(1, 2) * 3).to_s",
        ]);
        // `+`/`-`/`*`/`coerce`/`-@` follow CRuby's `real?` protocol for a
        // custom Numeric, and `polar`/`rectangular` accept zero-imaginary
        // Complex arguments.
        run_test_with_prelude(
            r#"
            [
              Complex(1, 2) + R.new,                       # real? true: real-part op
              Complex(1, 2) - R.new,
              Complex(N.new(1), N.new(2)).send(:-@).to_s,  # -@ dispatched to parts
              (-Complex(N.new(1), N.new(2))).to_s,
              Complex(3, 4).coerce(R.new).map(&:to_s),     # coerce wraps as Complex
              Complex.rectangular(1.0 + 0i, 2 + 0.0i).to_s,
              Complex.polar(2, 0).to_s,                    # zero arg preserves Integer
              Complex.polar(1 + 0.0i).to_s,
            ]
            "#,
            r#"
            class R < Numeric
              def real?; true; end
              def coerce(x); [x, 4]; end
              def to_s; "R"; end
            end
            class N < Numeric
              def initialize(n); @n = n; end
              def -@; @n == 1 ? -1 : -2; end
            end
            "#,
        );
        // real? false → coerce to Complex; non-real argument → TypeError.
        run_tests(&[
            "begin; Complex.rectangular(1.0 + 1i, 2); rescue TypeError; :te; end",
            "begin; Complex.polar(nil); rescue TypeError; :te; end",
        ]);
    }

    #[test]
    fn complex() {
        run_tests(&[
            r#"4000000000000000000000000000000+5000000000000000000000000000000i"#,
            r#"4.27+1.5i"#,
            r#"+(4.27+1.5i)"#,
            r#"-(4.27+1.5i)"#,
            r#"4-5i"#,
            r#"4000000000000000000000000000000-5000000000000000000000000000000i"#,
            r#"4.27-1.5i"#,
        ]);
        let v = &["4.2-1.5i", "4+5i", "4.0+5.0i", "134", "1.34"];
        run_binop_tests(v, &["+", "-", "*", "==", "!="], v);
        run_binop_tests2(&["4.5-8.0i"], &["/"], &["0.5-2.0i"]);
        // Complex#/ (== Complex#quo): exact Rational components when dividing
        // integer/rational Complex values; Float anywhere makes it Float.
        // Regression for #662 (the `/` operator used to truncate via Integer
        // division, giving e.g. `Complex(1,2)/2 #=> (0+1i)`).
        run_tests(&[
            // `/` operator, dot-call, and `send(:/)` all agree.
            "(Complex(1, 2) / 2).to_s",
            "Complex(1, 2)./(2).to_s",
            "Complex(1, 2).send(:/, 2).to_s",
            "Complex(1, 2).quo(2).to_s",
            // `/` and `quo` are the same (aliased) method.
            "Complex.instance_method(:quo) == Complex.instance_method(:/)",
            // Complex / Integer with mixed exact/Float components.
            "(Complex(6, 8) / 4).to_s",
            "(Complex(6, 8) / 2).to_s",
            "(Complex(1.0, 2) / 2).to_s",
            "(Complex(1, 2.0) / 2).to_s",
            "(Complex(1, 2) / 2.0).to_s",
            // Complex / Rational.
            "(Complex(1, 2) / Rational(2, 3)).to_s",
            "(Complex(2, 4) / Rational(2, 1)).to_s",
            // Complex / Complex (exact and Float paths, both Smith's branches).
            "(Complex(7, 3) / Complex(2, 1)).to_s",
            "(Complex(1, 2) / Complex(1, 1)).to_s",
            "(Complex(1, 2) / Complex(0, 2)).to_s",
            "(Complex(4.5, -8.0) / Complex(0.5, -2.0)).to_s",
            // real / Complex via the coerce protocol.
            "(6 / Complex(1, 1)).to_s",
            "(2 / Complex(1, 1)).to_s",
            "(2.0 / Complex(1, 1)).to_s",
            // Division by zero / type errors.
            "begin; Complex(1, 2) / 0; rescue ZeroDivisionError; :zd; end",
            "(Complex(1, 2) / 0.0).to_s",
            "begin; Complex(1, 2) / Complex(0, 0); rescue ZeroDivisionError; :zd; end",
            "begin; Complex(1, 2) / 'x'; rescue TypeError; :te; end",
        ]);
        //run_test(r#"(47-15i) % (4-5i)"#);
        run_test_error(r#"(4.27-1.5i) + :5"#);
        run_test_error(r#"(4.27-1.5i) - :5"#);
        run_test_error(r#"(4.27-1.5i) * :5"#);
        run_test_error(r#"(4.5-8.0i) / :5"#);

        run_tests(&[
            "Complex.polar(2.0)",
            "Complex.polar(2.0, 0)",
            //run_test("Complex.polar(2, 0)");
            "Complex.rect(1)",
            "Complex.rect(1, 2)",
            "Complex.rectangular(1, 2)",
        ]);
    }

    #[test]
    fn abs_2() {
        run_tests(&[
            "Complex(4, 5).abs",
            "Complex(4, 5).magnitude",
            "Complex(4, 5).rect",
            "Complex(4.7, 1.5).rect",
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
    fn float_quo_complex_2() {
        run_tests(&[
            "74620.09.quo(Complex(8,2))",
            "1.0.fdiv(Complex(8,2))",
            // real, imaginary (alias imag) extract components.
            "Complex(3, 4).real",
            "Complex(3, 4).imaginary",
            "Complex(3, 4).imag",
            "Complex(3.5, -1.25).real",
            "Complex(3.5, -1.25).imaginary",
            "Complex(3, 5).conjugate.to_s",
            "Complex(3, 5).conj.to_s",
            "Complex(3, -5).conjugate.to_s",
            "Complex(0, 0).conj.to_s",
            "Complex(3, 4).abs2",
            "Complex(-3, -4).abs2",
            "Complex(0, 0).abs2",
            "Complex(1.5, 2.5).abs2",
            // Each alias returns the same value.
            "Complex(1, 0).angle",
            "Complex(1, 0).arg",
            "Complex(1, 0).phase",
            "Complex(0, 1).angle",
            "Complex(-1, 0).angle",
            "Complex::I.real",
            "Complex::I.imaginary",
            "(Complex::I * Complex::I).to_s",
            // Complex#real? always false.
            "Complex(1, 0).real?",
            "Complex(1, 2).real?",
            // Complex#integer? always false.
            "Complex(1, 0).integer?",
            "Complex(1, 2).integer?",
            "Complex(1, 2).finite?",
            "Complex(1, 2).infinite?",
            "Complex(Float::INFINITY, 2).finite?",
            "Complex(Float::INFINITY, 2).infinite?",
            "Complex(1, Float::INFINITY).infinite?",
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
            // Integer exponent preserves integer-valued parts for the common
            // non-negative-exponent cases (regression for the earlier
            // `(3+4i) ** 1 == (3.0000...+4.0i)` bug).
            "(Complex(3, 4) ** 1).to_s",
            "(Complex(3, 4) ** 0).to_s",
            "(Complex(1, 1) ** 4).to_s",
            "(Complex(2, 0) ** 10).to_s",
            "Complex(6, 8).fdiv(2).to_s",
            "Complex(6, 8).fdiv(2.0).to_s",
            "Complex(6, 8).fdiv(-4).to_s",
            // Non-numeric argument raises TypeError.
            r#"begin; Complex(6).fdiv([]); rescue TypeError; :te; end"#,
            // Integer 0 imaginary is exact-zero; conversions succeed.
            "Complex(5, 0).to_f",
            "Complex(5, 0).to_i",
            "Complex(5, 0).to_r.to_s",
            // Float 0.0 imaginary raises RangeError.
            r#"begin; Complex(5, 0.0).to_f; rescue RangeError; :re; end"#,
            r#"begin; Complex(5, 0.0).to_i; rescue RangeError; :re; end"#,
            r#"begin; Complex(5, 0.0).to_r; rescue RangeError; :re; end"#,
            // Non-zero imaginary also raises.
            r#"begin; Complex(5, 2).to_f; rescue RangeError; :re; end"#,
            // to_c returns self (object identity).
            "Complex(3, 4).to_c.equal?(Complex(3, 4).to_c.to_c)",
            // to_s inserts `*` before `i` for non-finite imaginary parts.
            "Complex(1, Float::INFINITY).to_s",
            "Complex(1, -Float::INFINITY).to_s",
            "Complex(1, Float::NAN).to_s",
            "(-Complex(3, 4)).to_s",
            "(-Complex(-3, 4)).to_s",
            "(-Complex(0, 0)).to_s",
            // marshal_dump returns [real, imaginary].
            "Complex(1, 2).send(:marshal_dump)",
            // Equal complexes have equal hashes (stable).
            "Complex(1, 2).hash == Complex(1, 2).hash",
            // Complex explicitly undefines negative? and positive?.
            r#"begin; Complex(1).negative?; rescue NoMethodError; :nm; end"#,
            r#"begin; Complex(1).positive?; rescue NoMethodError; :nm; end"#,
            "Complex(1).methods.include?(:negative?)",
            "Complex(1).methods.include?(:positive?)",
            "Complex(3, 4).polar.size",
            "Complex(3, 4).polar.first",
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

    #[test]
    fn complex_eq_protocol() {
        run_tests(&[
            // Complex vs Complex.
            "Complex(1, 2) == Complex(1, 2)",
            "Complex(1.0, 2.0) == Complex(1, 2)",
            "Complex(1, 2) == Complex(3, 4)",
            "Complex(0.0/0.0, 0) == Complex(0.0/0.0, 0)",
            // Real Numeric: real == other when imaginary part is zero.
            "Complex(3, 0) == 3",
            "Complex(3.0, 0) == 3",
            "Complex(-3, 0) == -3",
            "Complex(3, 0) == 4",
            "Complex(3, 1) == 3",
            "Complex(3, 0) != 3",
            "Complex(3, 0) != 4",
            // Non-Numeric object: falls back to `other == self`.
            "Complex(3, 0) == 'x'",
            "Complex(3, 0) == nil",
            "(o = Object.new; def o.==(x); true; end; Complex(3, 0) == o)",
            "(o = Object.new; def o.==(x); 42; end; (Complex(3, 0) == o) == true)",
        ]);
    }

    #[test]
    fn complex_to_s_inspect() {
        run_tests(&[
            r#"Complex(1, 2).to_s"#,
            r#"Complex(1, -5).to_s"#,
            r#"Complex(-2.5, -1.5).to_s"#,
            r#"Complex(1, 0.0).to_s"#,
            r#"Complex(1, -0.0).to_s"#,
            r#"Complex(1, Rational(1, 2)).to_s"#,
            r#"Complex(1, Rational(1, 2)).inspect"#,
            r#"Complex(Rational(1, 2), Rational(3, 4)).to_s"#,
            r#"Complex(1, Float::INFINITY).to_s"#,
            r#"Complex(1, -Float::INFINITY).to_s"#,
            r#"Complex(1, Float::NAN).to_s"#,
            r#"Complex(2, 3).inspect"#,
            r#"Complex(0, 5).to_s"#,
        ]);
        // to_s / inspect dispatch to the parts' own #to_s / #inspect,
        // and the `*` separator appears when the rendered imaginary
        // part ends in a non-digit.
        run_test(
            r#"
            class CplxSub < Numeric
              def to_s; "X"; end
              def inspect; "(Y)"; end
              def <(o); false; end
            end
            c = Complex(CplxSub.new, CplxSub.new)
            [c.to_s, c.inspect]
            "#,
        );
    }

    #[test]
    fn complex_numerator_denominator() {
        // `numerator` scales both components onto the common denominator
        // (`denominator` = lcm of the parts'), dispatching `numerator` /
        // `denominator` / `/` / `*` on each part. Every intermediate can be
        // a heap numeric (Rational/Bignum) that only a Rust local holds
        // across the next dispatch, which is what the rooting guards.
        run_tests(&[
            // Integer components: denominator 1, numerator == self.
            "Complex(1, 2).numerator.to_s",
            "Complex(1, 2).denominator",
            // Rational components with different denominators.
            "Complex(Rational(1, 2), Rational(2, 3)).numerator.to_s",
            "Complex(Rational(1, 2), Rational(2, 3)).denominator",
            // Mixed Rational / Integer.
            "Complex(Rational(3, 4), 5).numerator.to_s",
            "Complex(Rational(3, 4), 5).denominator",
            "Complex(7, Rational(5, 6)).numerator.to_s",
            // Negative and zero components.
            "Complex(Rational(-1, 3), Rational(1, 6)).numerator.to_s",
            "Complex(0, Rational(1, 2)).numerator.to_s",
            // Bignum components: the intermediates are heap Integers.
            "Complex(10**30, 3).numerator.to_s",
            "Complex(Rational(10**30, 7), Rational(1, 3)).numerator.to_s",
            "Complex(Rational(10**30, 7), Rational(1, 3)).denominator",
        ]);
    }
}
