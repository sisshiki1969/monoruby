use super::*;

//
// Symbol class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("NilClass", NIL_CLASS, None);
    globals.define_builtin_class_func(NIL_CLASS, "allocate", super::class::undef_allocate, 0);
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn xor() {
        run_test(r##"nil ^ true"##);
        run_test(r##"nil ^ false"##);
        run_test(r##"nil ^ nil"##);
        run_test(r##"nil ^ 100"##);
        run_test(r##"nil ^ //"##);
        run_test(r##"nil ^ "100""##);

        run_test(r##"nil.^ true"##);
        run_test(r##"nil.^ false"##);
        run_test(r##"nil.^ nil"##);
        run_test(r##"nil.^ 100"##);
        run_test(r##"nil.^ //"##);
        run_test(r##"nil.^ "100""##);
    }

    #[test]
    fn and() {
        run_test(r##"nil & true"##);
        run_test(r##"nil & false"##);
        run_test(r##"nil & nil"##);
        run_test(r##"nil & 100"##);
        run_test(r##"nil & //"##);
        run_test(r##"nil & "100""##);

        run_test(r##"nil.& true"##);
        run_test(r##"nil.& false"##);
        run_test(r##"nil.& nil"##);
        run_test(r##"nil.& 100"##);
        run_test(r##"nil.& //"##);
        run_test(r##"nil.& "100""##);
    }

    #[test]
    fn or() {
        run_test(r##"nil | true"##);
        run_test(r##"nil | false"##);
        run_test(r##"nil | nil"##);
        run_test(r##"nil | 100"##);
        run_test(r##"nil | //"##);
        run_test(r##"nil | "100""##);

        run_test(r##"nil.| true"##);
        run_test(r##"nil.| false"##);
        run_test(r##"nil.| nil"##);
        run_test(r##"nil.| 100"##);
        run_test(r##"nil.| //"##);
        run_test(r##"nil.| "100""##);
    }
}
