use crate::*;

//
// Exception class
//

pub(super) fn init(globals: &mut Globals) {
    let exception_class = EXCEPTION_CLASS.get_obj(globals);
    let standarderr = globals.define_class_by_str("StandardError", exception_class, OBJECT_CLASS);
    globals.define_class_by_str("NoMemoryError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SecurityError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SignalException", standarderr, OBJECT_CLASS);

    let scripterr = globals.define_class_by_str("ScriptError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("LoadError", scripterr, OBJECT_CLASS);

    globals.define_class_by_str("ArgumentError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("EncodingError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("FiberError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("IOError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("IndexError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("LocalJumpError", standarderr, OBJECT_CLASS);
    let math_class = globals
        .get_constant(OBJECT_CLASS, IdentId::get_id("Math"))
        .unwrap()
        .as_class_id();
    globals.define_class_by_str("DomainError", standarderr, math_class);
    globals.define_class_by_str("NameError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("RangeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("RegexpError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("RuntimeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("SystemCallError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("TypeError", standarderr, OBJECT_CLASS);
    globals.define_class_by_str("ZeroDivisionError", standarderr, OBJECT_CLASS);
}
