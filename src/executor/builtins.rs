use super::*;

mod array;
mod class;
mod file;
mod integer;
mod object;
mod string;
mod time;

pub use time::TimeInfo;

//
// Builtin methods.
//

pub fn init_builtins(globals: &mut Globals) {
    assert_eq!(
        OBJECT_CLASS,
        globals
            .define_class("Object", None, OBJECT_CLASS)
            .as_class()
    );
    assert_eq!(
        CLASS_CLASS,
        globals.define_class_under_obj("Class").as_class()
    );
    assert_eq!(
        NIL_CLASS,
        globals.define_class_under_obj("NilClass").as_class()
    );
    assert_eq!(
        TRUE_CLASS,
        globals.define_class_under_obj("TrueClass").as_class()
    );
    assert_eq!(
        FALSE_CLASS,
        globals.define_class_under_obj("FalseClass").as_class()
    );
    assert_eq!(
        INTEGER_CLASS,
        globals.define_class_under_obj("Integer").as_class()
    );
    assert_eq!(
        FLOAT_CLASS,
        globals.define_class_under_obj("Float").as_class()
    );
    assert_eq!(
        STRING_CLASS,
        globals.define_class_under_obj("String").as_class()
    );
    assert_eq!(
        SYMBOL_CLASS,
        globals.define_class_under_obj("Symbol").as_class()
    );
    assert_eq!(
        TIME_CLASS,
        globals.define_class_under_obj("Time").as_class()
    );
    assert_eq!(
        ARRAY_CLASS,
        globals.define_class_under_obj("Array").as_class()
    );
    //globals.define_class_under_obj("Process");
    let file_class = globals.define_class_under_obj("File").as_class();

    object::init(globals);
    integer::init(globals);
    class::init(globals);
    string::init(globals);
    array::init(globals);
    time::init(globals);
    file::init(globals, file_class);
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Value);

impl Arg {
    pub fn as_ptr(&self) -> *const Value {
        self.0
    }

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
