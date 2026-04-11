use super::*;

//
// Trueclass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("TrueClass", TRUE_CLASS, None);
    globals.define_builtin_class_func(TRUE_CLASS, "allocate", super::class::undef_allocate, 0);
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn to_s() {
        run_test(r##"true.to_s"##);
        run_test(r##"true.to_s.class"##);
        run_test(r##"true.to_s == "true""##);
        run_test(r##"true.to_s.equal?(true.to_s)"##);
    }

    #[test]
    fn inspect() {
        run_test(r##"true.inspect"##);
    }

    #[test]
    fn class_name() {
        run_test(r##"true.class"##);
        run_test(r##"true.is_a?(TrueClass)"##);
    }

    #[test]
    fn xor() {
        run_tests(&[
            r##"true ^ true"##,
            r##"true ^ false"##,
            r##"true ^ nil"##,
            r##"true ^ 100"##,
            r##"true ^ //"##,
            r##"true ^ "100""##,
            r##"true.^ true"##,
            r##"true.^ false"##,
            r##"true.^ nil"##,
            r##"true.^ 100"##,
            r##"true.^ //"##,
            r##"true.^ "100""##,
        ]);
    }

    #[test]
    fn and() {
        run_tests(&[
            r##"true & true"##,
            r##"true & false"##,
            r##"true & nil"##,
            r##"true & 100"##,
            r##"true & //"##,
            r##"true & "100""##,
            r##"true.& true"##,
            r##"true.& false"##,
            r##"true.& nil"##,
            r##"true.& 100"##,
            r##"true.& //"##,
            r##"true.& "100""##,
        ]);
    }

    #[test]
    fn or() {
        run_tests(&[
            r##"true | true"##,
            r##"true | false"##,
            r##"true | nil"##,
            r##"true | 100"##,
            r##"true | //"##,
            r##"true | "100""##,
            r##"true.| true"##,
            r##"true.| false"##,
            r##"true.| nil"##,
            r##"true.| 100"##,
            r##"true.| //"##,
            r##"true.| "100""##,
        ]);
    }

    #[test]
    fn equality() {
        run_test(r##"true == true"##);
        run_test(r##"true == false"##);
        run_test(r##"true == nil"##);
        run_test(r##"true != false"##);
        run_test(r##"true.equal?(true)"##);
    }
}
