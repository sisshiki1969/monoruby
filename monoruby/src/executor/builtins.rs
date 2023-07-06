use super::*;

mod array;
mod class;
mod exception;
mod fiber;
mod file;
mod float;
mod hash;
mod integer;
mod io;
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

pub(self) use crate::executor::jitgen::analysis::SlotInfo;
pub(self) use crate::executor::jitgen::BBContext;
pub(self) use monoasm::DestLabel;
pub(self) use monoasm_macro::monoasm;
pub use time::TimeInner;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    object::init(globals, OBJECT_CLASS);
    module::init(globals, MODULE_CLASS);
    class::init(globals, CLASS_CLASS);
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

    assert_eq!(
        PROC_CLASS,
        globals
            .define_builtin_class_under_obj("Proc", PROC_CLASS)
            .id()
    );
    assert_eq!(
        METHOD_CLASS,
        globals
            .define_builtin_class_under_obj("Method", METHOD_CLASS)
            .id()
    );

    proc::init(globals);
    method::init(globals);
    fiber::init(globals);
    time::init(globals);
    io::init(globals);
    struct_class::init(globals);
    file::init(globals);
    math::init(globals);
    process::init(globals);
    random::init(globals);

    let stdin = Value::new_io_stdin();
    globals.set_constant_by_str(OBJECT_CLASS, "STDIN", stdin);
    globals.set_gvar(IdentId::get_id("$stdin"), stdin);

    let stdout = Value::new_io_stdout();
    globals.set_constant_by_str(OBJECT_CLASS, "STDOUT", stdout);
    globals.set_gvar(IdentId::get_id("$stdout"), stdout);
    globals.set_gvar(IdentId::get_id("$>"), stdout);

    let stderr = Value::new_io_stderr();
    globals.set_constant_by_str(OBJECT_CLASS, "STDERR", stderr);
    globals.set_gvar(IdentId::get_id("$stderr"), stderr);
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

impl Arg {
    pub fn to_vec(&self, len: usize) -> Vec<Value> {
        if len == 0 {
            return vec![];
        }
        unsafe {
            let data = self.0.sub(len - 1);
            std::slice::from_raw_parts(data, len)
                .iter()
                .rev()
                .cloned()
                .collect()
        }
    }

    pub fn iter(&self, len: usize) -> impl Iterator<Item = Value> {
        unsafe {
            let data = if len == 0 {
                self.0
            } else {
                self.0.sub(len - 1)
            };
            std::slice::from_raw_parts(data, len).iter().rev().cloned()
        }
    }

    pub fn rev(&self, len: usize) -> impl Iterator<Item = Value> {
        unsafe {
            let data = if len == 0 {
                self.0
            } else {
                self.0.sub(len - 1)
            };
            std::slice::from_raw_parts(data, len).iter().cloned()
        }
    }
}
