use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
}

/// ### Integer#times
/// - times {|n| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
extern "C" fn times(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _: usize,
) -> Option<Value> {
    let i = match self_val.try_fixnum() {
        Some(i) => i,
        None => unimplemented!(),
    };
    eprint!(
        "{}",
        match arg.block() {
            Some(v) => v.to_s(globals),
            None => "".to_string(),
        }
    );

    return Some(self_val);
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
extern "C" fn chr(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    match self_val.try_fixnum() {
        Some(i) => {
            if let Ok(b) = u8::try_from(i) {
                return Some(Value::new_string_from_slice(&[b]));
            }
        }
        _ => {}
    };
    globals.err_char_out_of_range(self_val);
    return None;
}
