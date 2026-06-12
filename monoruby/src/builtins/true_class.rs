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
    fn bool_receiver_ic_divergent_methods() {
        // #713: a single call site dispatching on both `true` and `false`
        // may only share the BOOL_CLASS-unified inline cache when
        // TrueClass and FalseClass resolve the method to the same
        // FuncId. Divergent methods (inspect/to_s, user-defined) must
        // run each receiver's own method; unified ones (`!`) keep the
        // fast path. 50 iterations cover the JIT tier.
        run_test_once(
            r##"
        res = []
        50.times do |i|
          v = (i % 2 == 0)
          res << v.inspect << v.to_s << v.!
        end
        res.uniq
        "##,
        );
        run_test_once(
            r##"
        class TrueClass; def label713; "yes"; end; end
        class FalseClass; def label713; "no"; end; end
        res = []
        50.times { |i| res << (i % 2 == 0).label713 }
        res.uniq
        "##,
        );
        // Super dispatch on a bool receiver (a callsite without a name)
        // tags the cache with the real class unconditionally. `super`
        // has no super-method here, so both engines raise NoMethodError.
        run_test_once(
            r##"
        class TrueClass; def label_t713; super rescue "T"; end; end
        class FalseClass; def label_f713; super rescue "F"; end; end
        res = []
        50.times { |i| v = (i % 2 == 0); res << (v ? v.label_t713 : v.label_f713) }
        res.uniq
        "##,
        );
        // A method present on only one of the two classes must not leak
        // to the other through the unified cache.
        run_test_once(
            r##"
        class TrueClass; def only_true713; 1; end; end
        res = []
        50.times do |i|
          begin
            res << (i % 2 == 0).only_true713
          rescue NoMethodError
            res << :nme
          end
        end
        res.uniq
        "##,
        );
    }

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

    #[test]
    fn alias_identities() {
        // ruby/spec core/true/inspect_spec.rb: inspect aliases to_s on TrueClass.
        // (Unlike false/nil, true's ^ and | genuinely differ, so they are NOT
        // aliases — assert that too.)
        run_tests(&[
            r##"true.method(:inspect) == true.method(:to_s)"##,
            r##"true.method(:^) == true.method(:|)"##,
        ]);
    }
}
