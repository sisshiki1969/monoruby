use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
}

// Integer#chr
extern "C" fn chr(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Option<Value> {
    let b = match arg.self_value().as_fixnum() {
        Some(i) => {
            if let Ok(res) = u8::try_from(i) {
                res
            } else {
                globals.set_error_char_out_of_range(arg.self_value());
                return None;
            }
        }
        _ => {
            globals.set_error_char_out_of_range(arg.self_value());
            return None;
        }
    };
    Some(Value::new_string(vec![b]))
}
