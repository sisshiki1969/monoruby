use crate::*;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
}

extern "C" fn superclass(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Value {
    let class_id = arg.self_value().as_class();
    match globals.class[class_id].super_class() {
        None => Value::nil(),
        Some(super_id) => globals.get_class_obj(super_id),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_class() {
        run_test("Time.superclass");
    }
}
