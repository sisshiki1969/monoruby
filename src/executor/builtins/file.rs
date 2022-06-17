use crate::*;

//
// File class
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.define_builtin_singleton_func(class_id, "write", write, 2);
}

/// ### File.write
/// - write(path, string, opt={}) -> Integer
/// - write(path, string, offset=nil, opt={}) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/class/IO.html#S_WRITE]
extern "C" fn write(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.clone()).unwrap(),
        _ => {
            globals.err_no_implict_conv(arg[0].class_id(), STRING_CLASS);
            return None;
        }
    };
    let mut file = File::create(name).unwrap();
    let bytes = arg[1].to_s(globals).into_bytes();
    file.write_all(&bytes).unwrap();
    Some(Value::new_integer(bytes.len() as i64))
}
