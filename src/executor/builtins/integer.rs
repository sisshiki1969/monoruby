use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
extern "C" fn chr(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Option<Value> {
    match arg.self_value().try_fixnum() {
        Some(i) => {
            if let Ok(b) = u8::try_from(i) {
                return Some(Value::new_string_from_slice(&[b]));
            }
        }
        _ => {}
    };
    globals.err_char_out_of_range(arg.self_value());
    return None;
}
