use crate::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
}

/// ### String#+
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
extern "C" fn add(
    _vm: &mut Interp,
    _globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut b = self_val.as_string().clone();
    b.extend_from_slice(arg[0].as_string());
    Some(Value::new_string_from_smallvec(b))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        run_test(r##"a = "We will"; a + " " + "rock you." "##);
    }
}
