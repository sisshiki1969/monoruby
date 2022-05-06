use super::*;

//
// Builtin methods.
//

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

pub extern "C" fn puts(
    _vm: &mut BcCompiler,
    _globals: &mut Globals,
    arg: Arg,
    len: usize,
) -> Value {
    for offset in 0..len {
        println!("{}", arg[offset]);
    }
    Value::nil()
}

pub extern "C" fn assert(
    _vm: &mut BcCompiler,
    _globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Value {
    let expected = arg[0];
    let actual = arg[1];
    assert_eq!(expected, actual);
    Value::nil()
}
