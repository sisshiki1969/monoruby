use super::*;

mod class;
mod integer;
mod object;
mod time;

pub use time::TimeInfo;

//
// Builtin methods.
//

pub fn init_builtins(globals: &mut Globals) {
    object::init(globals);
    integer::init(globals);
    class::init(globals);
    time::init(globals);
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
