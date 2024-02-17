use super::*;

mod array;
mod class;
pub(crate) mod enumerator;
mod exception;
mod fiber;
mod file;
mod float;
mod hash;
mod integer;
mod io;
mod kernel;
mod math;
mod method;
mod module;
mod object;
mod proc;
mod process;
mod random;
mod range;
mod regexp;
mod string;
mod struct_class;
mod symbol;
mod time;

use crate::compiler::jitgen::BBContext;
use compiler::jitgen::asmir::*;
pub use enumerator::YIELDER;
pub use monoasm::*;
pub use monoasm_macro::*;
use monoruby_attr::monoruby_builtin;
pub use time::TimeInner;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    object::init(globals);
    module::init(globals);
    class::init(globals);
    kernel::init(globals);
    exception::init(globals);
    integer::init(globals);
    float::init(globals);
    string::init(globals);
    array::init(globals);
    hash::init(globals);
    regexp::init(globals);
    range::init(globals);
    globals.define_builtin_class_under_obj("NilClass", NIL_CLASS);
    globals.define_builtin_class_under_obj("TrueClass", TRUE_CLASS);
    globals.define_builtin_class_under_obj("FalseClass", FALSE_CLASS);
    proc::init(globals);
    method::init(globals);
    fiber::init(globals);
    enumerator::init(globals);
    time::init(globals);
    io::init(globals);
    struct_class::init(globals);
    file::init(globals);
    math::init(globals);
    process::init(globals);
    random::init(globals);
    symbol::init(globals);
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Option<Value>);

impl std::ops::Index<usize> for Arg {
    type Output = Option<Value>;
    fn index(&self, index: usize) -> &Option<Value> {
        unsafe { &*self.0.sub(index) }
    }
}

impl std::ops::Add<usize> for Arg {
    type Output = Arg;
    fn add(self, rhs: usize) -> Arg {
        Arg(unsafe { self.0.sub(rhs) })
    }
}

/*impl Arg {
    pub fn new(lfp: Lfp) -> Self {
        Self(unsafe { lfp.register_ptr(1) as _ })
    }
}*/
