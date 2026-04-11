use super::*;

//
// NilClass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("NilClass", NIL_CLASS, None);
    globals.define_builtin_class_func(NIL_CLASS, "allocate", super::class::undef_allocate, 0);
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn to_s() {
        run_test(r##"nil.to_s"##);
        run_test(r##"nil.to_s.class"##);
        run_test(r##"nil.to_s == """##);
        run_test(r##"nil.to_s.equal?(nil.to_s)"##);
    }

    #[test]
    fn inspect() {
        run_test(r##"nil.inspect"##);
    }

    #[test]
    fn class_name() {
        run_test(r##"nil.class"##);
        run_test(r##"nil.is_a?(NilClass)"##);
    }

    #[test]
    fn to_a() {
        run_test(r##"nil.to_a"##);
        run_test(r##"nil.to_a.class"##);
        run_test(r##"nil.to_a == []"##);
        run_test(r##"nil.to_a.equal?(nil.to_a)"##);
    }

    #[test]
    fn match_op() {
        run_test(r##"nil =~ /foo/"##);
        run_test(r##"nil =~ "foo""##);
        run_test(r##"nil =~ 42"##);
        run_test(r##"nil =~ nil"##);
    }

    #[test]
    fn to_c() {
        run_test(r##"nil.to_c"##);
        run_test(r##"nil.to_c.class"##);
        run_test(r##"nil.to_c == Complex(0, 0)"##);
    }

    #[test]
    fn to_i() {
        run_test(r##"nil.to_i"##);
    }

    #[test]
    fn nil_p() {
        run_test(r##"nil.nil?"##);
        run_test(r##"nil == nil"##);
        run_test(r##"nil.equal?(nil)"##);
    }

    #[test]
    fn singleton_method_def() {
        run_test(
            r##"
            def nil.foo; 1; end
            nil.foo
            "##,
        );
        run_test(
            r##"
            def true.bar; 2; end
            true.bar
            "##,
        );
        run_test(
            r##"
            def false.baz; 3; end
            false.baz
            "##,
        );
        run_test(
            r##"
            obj = Object.new
            def (obj).qux; 4; end
            obj.qux
            "##,
        );
    }

    #[test]
    fn xor() {
        run_tests(&[
            r##"nil ^ true"##,
            r##"nil ^ false"##,
            r##"nil ^ nil"##,
            r##"nil ^ 100"##,
            r##"nil ^ //"##,
            r##"nil ^ "100""##,
            r##"nil.^ true"##,
            r##"nil.^ false"##,
            r##"nil.^ nil"##,
            r##"nil.^ 100"##,
            r##"nil.^ //"##,
            r##"nil.^ "100""##,
        ]);
    }

    #[test]
    fn and() {
        run_tests(&[
            r##"nil & true"##,
            r##"nil & false"##,
            r##"nil & nil"##,
            r##"nil & 100"##,
            r##"nil & //"##,
            r##"nil & "100""##,
            r##"nil.& true"##,
            r##"nil.& false"##,
            r##"nil.& nil"##,
            r##"nil.& 100"##,
            r##"nil.& //"##,
            r##"nil.& "100""##,
        ]);
    }

    #[test]
    fn or() {
        run_tests(&[
            r##"nil | true"##,
            r##"nil | false"##,
            r##"nil | nil"##,
            r##"nil | 100"##,
            r##"nil | //"##,
            r##"nil | "100""##,
            r##"nil.| true"##,
            r##"nil.| false"##,
            r##"nil.| nil"##,
            r##"nil.| 100"##,
            r##"nil.| //"##,
            r##"nil.| "100""##,
        ]);
    }
}
