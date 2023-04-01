use super::*;

mod array;
mod class;
mod file;
mod float;
mod hash;
mod integer;
mod io;
mod math;
mod module;
mod object;
mod proc;
mod process;
mod random;
mod range;
mod regexp;
mod string;
mod time;

pub use time::TimeInner;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    assert_eq!(
        OBJECT_CLASS,
        globals
            .define_builtin_class_by_str("Object", OBJECT_CLASS, None, OBJECT_CLASS)
            .class_id()
    );
    let module = globals.define_builtin_class_under_obj("Module", MODULE_CLASS);
    assert_eq!(MODULE_CLASS, module.class_id());
    assert_eq!(
        CLASS_CLASS,
        globals
            .define_builtin_class_by_str("Class", CLASS_CLASS, module, OBJECT_CLASS)
            .class_id()
    );
    assert_eq!(
        NIL_CLASS,
        globals
            .define_builtin_class_under_obj("NilClass", NIL_CLASS)
            .class_id()
    );
    assert_eq!(
        TRUE_CLASS,
        globals
            .define_builtin_class_under_obj("TrueClass", TRUE_CLASS)
            .class_id()
    );
    assert_eq!(
        FALSE_CLASS,
        globals
            .define_builtin_class_under_obj("FalseClass", FALSE_CLASS)
            .class_id()
    );
    assert_eq!(
        INTEGER_CLASS,
        globals
            .define_builtin_class_under_obj("Integer", INTEGER_CLASS)
            .class_id()
    );
    assert_eq!(
        FLOAT_CLASS,
        globals
            .define_builtin_class_under_obj("Float", FLOAT_CLASS)
            .class_id()
    );
    assert_eq!(
        STRING_CLASS,
        globals
            .define_builtin_class_under_obj("String", STRING_CLASS)
            .class_id()
    );
    assert_eq!(
        SYMBOL_CLASS,
        globals
            .define_builtin_class_under_obj("Symbol", SYMBOL_CLASS)
            .class_id()
    );
    assert_eq!(
        TIME_CLASS,
        globals
            .define_builtin_class_under_obj("Time", TIME_CLASS)
            .class_id()
    );
    assert_eq!(
        ARRAY_CLASS,
        globals
            .define_builtin_class_under_obj("Array", ARRAY_CLASS)
            .class_id()
    );
    assert_eq!(
        RANGE_CLASS,
        globals
            .define_builtin_class_under_obj("Range", RANGE_CLASS)
            .class_id()
    );
    assert_eq!(
        PROC_CLASS,
        globals
            .define_builtin_class_under_obj("Proc", PROC_CLASS)
            .class_id()
    );
    assert_eq!(
        HASH_CLASS,
        globals
            .define_builtin_class_under_obj("Hash", HASH_CLASS)
            .class_id()
    );
    assert_eq!(
        REGEXP_CLASS,
        globals
            .define_builtin_class_under_obj("Regexp", REGEXP_CLASS)
            .class_id()
    );
    let io_class = globals.define_builtin_class_under_obj("IO", IO_CLASS);
    assert_eq!(IO_CLASS, io_class.class_id());
    let exception_class = globals.define_builtin_class_under_obj("Exception", EXCEPTION_CLASS);
    assert_eq!(EXCEPTION_CLASS, exception_class.class_id());
    let math_class = globals.define_module("Math").class_id();
    let process_class = globals.define_module("Process").class_id();
    let file_class = globals
        .define_class_by_str("File", io_class, OBJECT_CLASS)
        .class_id();
    let random_class = globals
        .define_class_by_str("Random", OBJECT_CLASS.get_obj(globals), OBJECT_CLASS)
        .class_id();

    object::init(globals);
    integer::init(globals);
    float::init(globals);
    module::init(globals);
    class::init(globals);
    string::init(globals);
    array::init(globals);
    hash::init(globals);
    regexp::init(globals);
    range::init(globals);
    proc::init(globals);
    time::init(globals);
    io::init(globals);
    file::init(globals, file_class);
    math::init(globals, math_class);
    process::init(globals, process_class);
    random::init(globals, random_class);

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
