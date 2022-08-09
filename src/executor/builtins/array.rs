use crate::*;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
}

/// ### Array#+
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
extern "C" fn add(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Option<Value> {
    let mut lhs = arg.self_value().as_array().unwrap().clone();
    let rhs = match arg[0].as_array() {
        Some(v) => v,
        None => {
            globals.err_no_implict_conv(arg[0].class_id(), ARRAY_CLASS);
            return None;
        }
    };
    lhs.extend_from_slice(rhs);
    Some(Value::new_array(lhs))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        run_test(r##"[1,2,3] + [4]"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; a"##);
        run_test(r##"a = [1,2,3]; b = [4]; a + b; b"##);
    }
}
