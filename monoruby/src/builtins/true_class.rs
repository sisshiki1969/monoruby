use super::*;

//
// Trueclass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("TrueClass", TRUE_CLASS, None);
    globals.store[TRUE_CLASS].clear_alloc_func();
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn to_s() {
        run_tests(&[
            r##"true.to_s"##,
            r##"true.to_s.class"##,
            r##"true.to_s == "true""##,
            r##"true.to_s.equal?(true.to_s)"##,
            r##"true.inspect"##,
            r##"true.class"##,
            r##"true.is_a?(TrueClass)"##,
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
            r##"true == true"##,
            r##"true == false"##,
            r##"true == nil"##,
            r##"true != false"##,
            r##"true.equal?(true)"##,
        ]);
    }
}
