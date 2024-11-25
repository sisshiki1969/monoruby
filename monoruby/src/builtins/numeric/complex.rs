use num::Zero;

use super::*;

//
// Complex class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class_by_str("Complex", COMPLEX_CLASS, numeric, OBJECT_CLASS);
    globals.define_builtin_func(COMPLEX_CLASS, "==", eq, 1);
    globals.define_builtin_func(COMPLEX_CLASS, "!=", ne, 1);
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
    globals.define_builtin_funcs(COMPLEX_CLASS, "rect", &["rectangular"], rect, 0);
}

fn eq_bool(lhs: &ComplexInner, rhs: Value) -> bool {
    match rhs.try_complex() {
        Some(rhs) => lhs == rhs,
        None => {
            let rhs = match Real::try_from(rhs) {
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
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val();
    Ok(Value::bool(eq_bool(lhs.as_complex(), lfp.arg(0))))
}

#[monoruby_builtin]
fn ne(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val();
    Ok(Value::bool(!eq_bool(lhs.as_complex(), lfp.arg(0))))
}

///
/// ### Complex.polar
///
/// - polar(r, theta = 0) -> Complex
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/s/polar.html]
#[monoruby_builtin]
fn complex_polar(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let r = RealKind::try_from(lfp.arg(0))?.to_f64();
    let theta = if let Some(theta) = lfp.try_arg(1) {
        RealKind::try_from(theta)?.to_f64()
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
fn complex_rect(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let r = Real::try_from(lfp.arg(0))?;
    let i = if let Some(i) = lfp.try_arg(1) {
        Real::try_from(i)?
    } else {
        Real::from(0)
    };
    Ok(Value::complex(r, i))
}

///
/// ### Complex#abs
///
/// - abs -> Numeric
/// - magnitude -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/abs.html]
#[monoruby_builtin]
fn abs(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let abs = lfp.self_val().as_complex().to_complex_f64().norm();
    Ok(Value::float(abs))
}

///
/// ### Complex#abs
///
/// - rect -> [Numeric, Numeric]
/// - rectangular -> [Numeric, Numeric]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Complex/i/rect.html]
#[monoruby_builtin]
fn rect(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let r = self_.as_complex().re();
    let i = self_.as_complex().im();
    Ok(Value::array2(r.get(), i.get()))
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
}
