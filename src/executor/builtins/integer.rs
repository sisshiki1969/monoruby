use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
}

// Integer#chr
extern "C" fn chr(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let b = match arg.self_value().as_fixnum() {
        Some(i) => {
            if let Ok(res) = u8::try_from(i) {
                res
            } else {
                unreachable!()
            }
        }
        _ => unreachable!(),
    };
    Value::new_string(vec![b])
}
