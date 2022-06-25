use crate::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
}

/// String#+
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
extern "C" fn add(
    _vm: &mut Interp,
    _globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut b = arg.self_value().as_string().clone();
    b.extend(arg[0].as_string());
    Some(Value::new_string(b))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        run_test(r##"a = "We will"; a + " " + "rock you." "##);
    }
}
