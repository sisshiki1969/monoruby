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
        run_tests(&[
            r##"false.to_s"##,
            r##"false.to_s.class"##,
            r##"false.to_s == "false""##,
            r##"false.to_s.equal?(false.to_s)"##,
            r##"false.inspect"##,
            r##"false.class"##,
            r##"false.is_a?(FalseClass)"##,
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
            r##"false == false"##,
            r##"false == true"##,
            r##"false == nil"##,
            r##"false != true"##,
            r##"false.equal?(false)"##,
        ]);
    }
}
