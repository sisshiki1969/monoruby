use std::io::stdout;

use super::*;

//
// Builtin methods.
//

pub fn init_builtins(globals: &mut Globals) {
    globals.add_builtin_func("puts", puts, 1);
    globals.add_builtin_func("print", print, 3);
    globals.add_builtin_func("assert", assert, 2);
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Value);

impl Arg {
    pub fn new(ptr: *const Value) -> Self {
        Self(ptr)
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
        print!("{}", arg[offset]);
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
