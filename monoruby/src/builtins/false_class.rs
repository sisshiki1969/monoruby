use super::*;

//
// FalseClass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("FalseClass", FALSE_CLASS, None);
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn xor() {
        run_test(r##"false ^ true"##);
        run_test(r##"false ^ false"##);
        run_test(r##"false ^ nil"##);
        run_test(r##"false ^ 100"##);
        run_test(r##"false ^ //"##);
        run_test(r##"false ^ "100""##);

        run_test(r##"false.^ true"##);
        run_test(r##"false.^ false"##);
        run_test(r##"false.^ nil"##);
        run_test(r##"false.^ 100"##);
        run_test(r##"false.^ //"##);
        run_test(r##"false.^ "100""##);
    }

    #[test]
    fn and() {
        run_test(r##"false & true"##);
        run_test(r##"false & false"##);
        run_test(r##"false & nil"##);
        run_test(r##"false & 100"##);
        run_test(r##"false & //"##);
        run_test(r##"false & "100""##);

        run_test(r##"false.& true"##);
        run_test(r##"false.& false"##);
        run_test(r##"false.& nil"##);
        run_test(r##"false.& 100"##);
        run_test(r##"false.& //"##);
        run_test(r##"false.& "100""##);
    }

    #[test]
    fn or() {
        run_test(r##"false | true"##);
        run_test(r##"false | false"##);
        run_test(r##"false | nil"##);
        run_test(r##"false | 100"##);
        run_test(r##"false | //"##);
        run_test(r##"false | "100""##);

        run_test(r##"false.| true"##);
        run_test(r##"false.| false"##);
        run_test(r##"false.| nil"##);
        run_test(r##"false.| 100"##);
        run_test(r##"false.| //"##);
        run_test(r##"false.| "100""##);
    }
}
