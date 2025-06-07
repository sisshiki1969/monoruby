use super::*;

//
// Trueclass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("TrueClass", TRUE_CLASS, None);
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn xor() {
        run_test(r##"true ^ true"##);
        run_test(r##"true ^ false"##);
        run_test(r##"true ^ nil"##);
        run_test(r##"true ^ 100"##);
        run_test(r##"true ^ //"##);
        run_test(r##"true ^ "100""##);

        run_test(r##"true.^ true"##);
        run_test(r##"true.^ false"##);
        run_test(r##"true.^ nil"##);
        run_test(r##"true.^ 100"##);
        run_test(r##"true.^ //"##);
        run_test(r##"true.^ "100""##);
    }

    #[test]
    fn and() {
        run_test(r##"true & true"##);
        run_test(r##"true & false"##);
        run_test(r##"true & nil"##);
        run_test(r##"true & 100"##);
        run_test(r##"true & //"##);
        run_test(r##"true & "100""##);

        run_test(r##"true.& true"##);
        run_test(r##"true.& false"##);
        run_test(r##"true.& nil"##);
        run_test(r##"true.& 100"##);
        run_test(r##"true.& //"##);
        run_test(r##"true.& "100""##);
    }

    #[test]
    fn or() {
        run_test(r##"true | true"##);
        run_test(r##"true | false"##);
        run_test(r##"true | nil"##);
        run_test(r##"true | 100"##);
        run_test(r##"true | //"##);
        run_test(r##"true | "100""##);

        run_test(r##"true.| true"##);
        run_test(r##"true.| false"##);
        run_test(r##"true.| nil"##);
        run_test(r##"true.| 100"##);
        run_test(r##"true.| //"##);
        run_test(r##"true.| "100""##);
    }
}
