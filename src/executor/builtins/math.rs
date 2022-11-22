use num::ToPrimitive;

use crate::*;

//
// Math class
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.define_builtin_singleton_func_inlinable(
        class_id,
        "sqrt",
        sqrt,
        1,
        InlineMethod::MathSqrt,
    );
    globals.define_builtin_singleton_func(class_id, "cos", cos, 1);
    globals.define_builtin_singleton_func(class_id, "sin", sin, 1);
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
extern "C" fn sqrt(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            globals.err_cant_conert_into_float(arg0);
            return None;
        }
    };
    Some(Value::new_float(f.sqrt()))
}

/// ### Math.#sin
/// - sin(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sin.html]
extern "C" fn sin(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            globals.err_cant_conert_into_float(arg0);
            return None;
        }
    };
    Some(Value::new_float(f.sin()))
}

/// ### Math.#cos
/// - cos(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cos.html]
extern "C" fn cos(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            globals.err_cant_conert_into_float(arg0);
            return None;
        }
    };
    Some(Value::new_float(f.cos()))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn sqrt() {
        run_test("Math.sqrt 128");
        run_test("Math.sqrt 2192.56818");
    }

    #[test]
    fn torigonometric() {
        run_test("Math.cos 149");
        run_test("Math.cos -14.97522");
        run_test("Math.sin 149");
        run_test("Math.sin -14.97522");
    }
}
