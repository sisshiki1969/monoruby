use super::*;

//
// NilClass
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("NilClass", NIL_CLASS, None);
    globals.store[NIL_CLASS].clear_alloc_func();
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn to_s() {
        run_tests(&[
            r##"nil.to_s"##,
            r##"nil.to_s.class"##,
            r##"nil.to_s == """##,
            r##"nil.to_s.equal?(nil.to_s)"##,
            r##"nil.inspect"##,
            r##"nil.class"##,
            r##"nil.is_a?(NilClass)"##,
            r##"nil.to_a"##,
            r##"nil.to_a.class"##,
            r##"nil.to_a == []"##,
            r##"nil.to_a.equal?(nil.to_a)"##,
            r##"nil =~ /foo/"##,
            r##"nil =~ "foo""##,
            r##"nil =~ 42"##,
            r##"nil =~ nil"##,
            r##"nil.to_c"##,
            r##"nil.to_c.class"##,
            r##"nil.to_c == Complex(0, 0)"##,
            r##"nil.to_i"##,
            r##"nil.nil?"##,
            r##"nil == nil"##,
            r##"nil.equal?(nil)"##,
        ]);
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
