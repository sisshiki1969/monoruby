use super::*;

mod array;
mod class;
mod file;
mod integer;
mod math;
mod object;
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
            .define_builtin_class("Object", OBJECT_CLASS, None, OBJECT_CLASS)
            .as_class()
    );
    assert_eq!(
        CLASS_CLASS,
        globals
            .define_builtin_class_under_obj("Class", CLASS_CLASS)
            .as_class()
    );
    assert_eq!(
        NIL_CLASS,
        globals
            .define_builtin_class_under_obj("NilClass", NIL_CLASS)
            .as_class()
    );
    assert_eq!(
        TRUE_CLASS,
        globals
            .define_builtin_class_under_obj("TrueClass", TRUE_CLASS)
            .as_class()
    );
    assert_eq!(
        FALSE_CLASS,
        globals
            .define_builtin_class_under_obj("FalseClass", FALSE_CLASS)
            .as_class()
    );
    assert_eq!(
        INTEGER_CLASS,
        globals
            .define_builtin_class_under_obj("Integer", INTEGER_CLASS)
            .as_class()
    );
    assert_eq!(
        FLOAT_CLASS,
        globals
            .define_builtin_class_under_obj("Float", FLOAT_CLASS)
            .as_class()
    );
    assert_eq!(
        STRING_CLASS,
        globals
            .define_builtin_class_under_obj("String", STRING_CLASS)
            .as_class()
    );
    assert_eq!(
        SYMBOL_CLASS,
        globals
            .define_builtin_class_under_obj("Symbol", SYMBOL_CLASS)
            .as_class()
    );
    assert_eq!(
        TIME_CLASS,
        globals
            .define_builtin_class_under_obj("Time", TIME_CLASS)
            .as_class()
    );
    assert_eq!(
        ARRAY_CLASS,
        globals
            .define_builtin_class_under_obj("Array", ARRAY_CLASS)
            .as_class()
    );
    let math_class = globals.define_class_under_obj("Math").as_class();
    let file_class = globals.define_class_under_obj("File").as_class();

    object::init(globals);
    integer::init(globals);
    class::init(globals);
    string::init(globals);
    array::init(globals);
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
