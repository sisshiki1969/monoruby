use std::{io::stdout, time::Instant};

use super::*;

//
// Builtin methods.
//

pub fn init_builtins(globals: &mut Globals) {
    globals.define_global_builtin_func("puts", puts, 1);
    globals.define_global_builtin_func("print", print, 1);
    globals.define_global_builtin_func("assert", assert, 2);
    globals.define_global_builtin_func("respond_to?", respond_to, 1);
    globals.define_global_builtin_func("now", now, 0);
    globals.define_global_builtin_func("inspect", inspect, 0);
    globals.define_global_builtin_func("write", write, 2);
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Value);

impl Arg {
    /*pub fn new(ptr: *const Value) -> Self {
        Self(ptr)
    }*/

    pub fn self_value(&self) -> Value {
        unsafe { *self.0.add(1) }
    }
}

impl std::ops::Index<usize> for Arg {
    type Output = Value;
    fn index(&self, index: usize) -> &Value {
        unsafe { &*self.0.sub(index) }
    }
}

extern "C" fn puts(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, len: usize) -> Value {
    for offset in 0..len {
        println!("{}", arg[offset]);
    }
    Value::nil()
}

extern "C" fn print(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, len: usize) -> Value {
    for offset in 0..len {
        match arg[offset].unpack() {
            RV::String(bytes) => {
                std::io::stdout().write(bytes).unwrap();
            }
            _ => print!("{}", arg[offset]),
        };
    }
    stdout().flush().unwrap();
    Value::nil()
}

extern "C" fn assert(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let expected = arg[0];
    let actual = arg[1];
    assert_eq!(expected, actual);
    Value::nil()
}

extern "C" fn respond_to(
    _vm: &mut Interp,
    _globals: &mut Globals,
    _arg: Arg,
    _len: usize,
) -> Value {
    Value::bool(false)
}

extern "C" fn now(_vm: &mut Interp, _globals: &mut Globals, _arg: Arg, _len: usize) -> Value {
    Value::time(Instant::now())
}

extern "C" fn inspect(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    Value::string(format!("{}", arg.self_value()).into_bytes())
}

extern "C" fn write(_vm: &mut Interp, _globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.clone()).unwrap(),
        v => unimplemented!("{}", v),
    };
    let mut file = File::create(name).unwrap();
    let bytes = match arg[1].unpack() {
        RV::String(bytes) => bytes.clone(),
        _ => format!("{}", arg[0]).into_bytes(),
    };
    file.write_all(&bytes).unwrap();
    Value::integer(bytes.len() as i64)
}
