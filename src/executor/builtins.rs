use super::*;

mod array;
mod class;
mod file;
mod float;
mod hash;
mod integer;
mod math;
mod object;
mod proc;
mod range;
mod regexp;
mod string;
mod time;

pub use time::TimeInfo;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    assert_eq!(
        OBJECT_CLASS,
        globals
            .define_builtin_class_by_str("Object", OBJECT_CLASS, None, OBJECT_CLASS)
            .as_class()
            .class_id()
    );
    let module = globals.define_builtin_class_under_obj("Module", MODULE_CLASS);
    assert_eq!(MODULE_CLASS, module.as_class().class_id());
    assert_eq!(
        CLASS_CLASS,
        globals
            .define_builtin_class_by_str("Class", CLASS_CLASS, Module::new(module), OBJECT_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        NIL_CLASS,
        globals
            .define_builtin_class_under_obj("NilClass", NIL_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        TRUE_CLASS,
        globals
            .define_builtin_class_under_obj("TrueClass", TRUE_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        FALSE_CLASS,
        globals
            .define_builtin_class_under_obj("FalseClass", FALSE_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        INTEGER_CLASS,
        globals
            .define_builtin_class_under_obj("Integer", INTEGER_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        FLOAT_CLASS,
        globals
            .define_builtin_class_under_obj("Float", FLOAT_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        STRING_CLASS,
        globals
            .define_builtin_class_under_obj("String", STRING_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        SYMBOL_CLASS,
        globals
            .define_builtin_class_under_obj("Symbol", SYMBOL_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        TIME_CLASS,
        globals
            .define_builtin_class_under_obj("Time", TIME_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        ARRAY_CLASS,
        globals
            .define_builtin_class_under_obj("Array", ARRAY_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        RANGE_CLASS,
        globals
            .define_builtin_class_under_obj("Range", RANGE_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        PROC_CLASS,
        globals
            .define_builtin_class_under_obj("Proc", PROC_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        HASH_CLASS,
        globals
            .define_builtin_class_under_obj("Hash", HASH_CLASS)
            .as_class()
            .class_id()
    );
    assert_eq!(
        REGEXP_CLASS,
        globals
            .define_builtin_class_under_obj("Regexp", REGEXP_CLASS)
            .as_class()
            .class_id()
    );
    let math_class = globals.define_class_under_obj("Math").as_class().class_id();
    let file_class = globals.define_class_under_obj("File").as_class().class_id();

    object::init(globals);
    integer::init(globals);
    float::init(globals);
    class::init(globals);
    string::init(globals);
    array::init(globals);
    hash::init(globals);
    regexp::init(globals);
    range::init(globals);
    proc::init(globals);
    time::init(globals);
    file::init(globals, file_class);
    math::init(globals, math_class);
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
}
