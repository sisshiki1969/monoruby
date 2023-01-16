use crate::*;
use std::{fs::File, io::Write};

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
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/write.html]
extern "C" fn write(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.to_vec()).unwrap(),
        _ => {
            globals.err_no_implict_conv(arg[0], STRING_CLASS);
            return None;
        }
    };
    let mut file = File::create(name).unwrap();
    let bytes = arg[1].to_s(globals).into_bytes();
    file.write_all(&bytes).unwrap();
    Some(Value::new_integer(bytes.len() as i64))
}
