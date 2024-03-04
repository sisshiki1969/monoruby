use super::*;

//
// Complex class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Complex", COMPLEX_CLASS);
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

#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn complex() {
        run_test(r#"4000000000000000000000000000000+5000000000000000000000000000000i"#);
        run_test(r#"4.27+1.5i"#);
        run_test(r#"4-5i"#);
        run_test(r#"4000000000000000000000000000000-5000000000000000000000000000000i"#);
        run_test(r#"4.27-1.5i"#);
        run_test(r#"(4.27-1.5i) + (4-5i)"#);
        run_test(r#"(4.27-1.5i) - (4-5i)"#);
        run_test(r#"(4.27-1.5i) * (4-5i)"#);
        run_test(r#"(4.5-8.0i) / (0.5-2.0i)"#);
        //run_test(r#"(47-15i) % (4-5i)"#);

        run_test("Complex.polar(2.0)");
        run_test("Complex.polar(2.0, 0)");
        //run_test("Complex.polar(2, 0)");
        run_test("Complex.rect(1)");
        run_test("Complex.rect(1, 2)");
        run_test("Complex.rectangular(1, 2)");
    }
}
