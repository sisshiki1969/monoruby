use super::*;

//
// Complex class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Complex", COMPLEX_CLASS);
}
