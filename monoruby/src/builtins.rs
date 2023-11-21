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
mod time;

pub(self) use crate::compiler::jitgen::BBContext;
pub use enumerator::YIELDER;
pub use monoasm::*;
pub use monoasm_macro::*;
pub(self) use monoruby_attr::monoruby_builtin;
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
    globals.define_builtin_class_under_obj("Symbol", SYMBOL_CLASS);
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
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Value);

impl std::ops::Index<usize> for Arg {
    type Output = Value;
    fn index(&self, index: usize) -> &Value {
        unsafe { &*self.0.sub(index) }
    }
}

impl std::ops::Add<usize> for Arg {
    type Output = Arg;
    fn add(self, rhs: usize) -> Arg {
        Arg(unsafe { self.0.sub(rhs) })
    }
}

impl Arg {
    pub fn from(val: &Value) -> Arg {
        Arg(val as _)
    }

    pub fn as_ptr(&self) -> *const Value {
        self.0
    }

    /*pub fn to_vec(&self, len: usize) -> Vec<Value> {
        self.iter(len).collect()
    }*/

    /*pub fn iter(&self, len: usize) -> impl DoubleEndedIterator<Item = Value> + '_ {
        self.iter_inner(len).rev().cloned()
    }*/

    /*fn iter_inner(&self, len: usize) -> impl DoubleEndedIterator<Item = &Value> {
        unsafe {
            let data = if len == 0 {
                self.0
            } else {
                self.0.sub(len - 1)
            };
            std::slice::from_raw_parts(data, len).iter()
        }
    }*/
}
