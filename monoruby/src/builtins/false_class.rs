use super::*;

//
// FalseClass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("FalseClass", FALSE_CLASS, None);
    globals.store[FALSE_CLASS].clear_alloc_func();
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn to_s() {
        run_test(r##"false.to_s"##);
        run_test(r##"false.to_s.class"##);
        run_test(r##"false.to_s == "false""##);
        run_test(r##"false.to_s.equal?(false.to_s)"##);
    }

    #[test]
    fn inspect() {
        run_test(r##"false.inspect"##);
    }

    #[test]
    fn class_name() {
        run_test(r##"false.class"##);
        run_test(r##"false.is_a?(FalseClass)"##);
    }

    #[test]
    fn xor() {
        run_tests(&[
            r##"false ^ true"##,
            r##"false ^ false"##,
            r##"false ^ nil"##,
            r##"false ^ 100"##,
            r##"false ^ //"##,
            r##"false ^ "100""##,
            r##"false.^ true"##,
            r##"false.^ false"##,
            r##"false.^ nil"##,
            r##"false.^ 100"##,
            r##"false.^ //"##,
            r##"false.^ "100""##,
        ]);
    }

    #[test]
    fn and() {
        run_tests(&[
            r##"false & true"##,
            r##"false & false"##,
            r##"false & nil"##,
            r##"false & 100"##,
            r##"false & //"##,
            r##"false & "100""##,
            r##"false.& true"##,
            r##"false.& false"##,
            r##"false.& nil"##,
            r##"false.& 100"##,
            r##"false.& //"##,
            r##"false.& "100""##,
        ]);
    }

    #[test]
    fn or() {
        run_tests(&[
            r##"false | true"##,
            r##"false | false"##,
            r##"false | nil"##,
            r##"false | 100"##,
            r##"false | //"##,
            r##"false | "100""##,
            r##"false.| true"##,
            r##"false.| false"##,
            r##"false.| nil"##,
            r##"false.| 100"##,
            r##"false.| //"##,
            r##"false.| "100""##,
        ]);
    }

    #[test]
    fn equality() {
        run_test(r##"false == false"##);
        run_test(r##"false == true"##);
        run_test(r##"false == nil"##);
        run_test(r##"false != true"##);
        run_test(r##"false.equal?(false)"##);
    }
}
