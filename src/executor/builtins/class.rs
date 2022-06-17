use crate::*;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/class/Class.html#I_SUPERCLASS]
extern "C" fn superclass(
    _vm: &mut Interp,
    globals: &mut Globals,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class_id = arg.self_value().as_class();
    let res = match class_id.super_class(globals) {
        None => Value::nil(),
        Some(super_id) => super_id.get_obj(globals),
    };
    Some(res)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_class() {
        run_test("Time.superclass");
    }
}
