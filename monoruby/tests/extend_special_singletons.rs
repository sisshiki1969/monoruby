extern crate monoruby;
use monoruby::tests::*;

// Issue #1943: `nil`, `true` and `false` are frozen, but their singleton
// class is NilClass / TrueClass / FalseClass, and CRuby's
// `rb_extend_object` includes the module there. An Integer, a Symbol or a
// Float has no singleton class at all, which is a TypeError, not the
// FrozenError of a frozen receiver.

#[test]
fn extend_nil_true_false() {
    run_test_once(
        r##"
        module ExtM; def ext_foo = :foo; end
        module ExtM2
          def self.extended(o) = ($ext ||= []) << o
          def ext_foo2 = :foo2
        end
        res = []
        [nil, true, false].each do |v|
          res << [v.extend(ExtM).equal?(v), v.ext_foo, v.extend(ExtM2).ext_foo2,
                  v.singleton_class, v.singleton_class.include?(ExtM),
                  v.class.ancestors.first(3), v.frozen?]
        end
        res << $ext
        ExtM.send(:extend_object, nil)
        res << nil.dup.ext_foo
        res
        "##,
    );
}

#[test]
fn singleton_methods_on_nil_true_false() {
    // `nil.define_singleton_method` used to panic.
    run_test_once(
        r##"
        res = []
        nil.define_singleton_method(:dsm_nil) { 3 }
        true.define_singleton_method(:dsm_true) { 4 }
        def false.dsm_false = 5
        res << nil.dsm_nil << true.dsm_true << false.dsm_false
        res << NilClass.instance_methods(false).include?(:dsm_nil)
        res << nil.singleton_methods.include?(:dsm_nil)
        res
        "##,
    );
}

#[test]
fn extend_without_a_singleton_class() {
    run_test_once(
        r##"
        module ExtM; def ext_foo = :foo; end
        t = ->(&b) { b.call rescue [$!.class, $!.message] }
        [
          t.() { 1.extend(ExtM) },
          t.() { :a.extend(ExtM) },
          t.() { 1.5.extend(ExtM) },
          t.() { (2**70).extend(ExtM) },
          t.() { 1e300.extend(ExtM) },
          t.() { ExtM.send(:extend_object, 1) },
          t.() { 1.define_singleton_method(:x) { } },
          t.() { :a.singleton_class },
          t.() { 1.extend(Class) },
          t.() { nil.extend(1) },
          t.() { nil.extend },
          # Frozen receivers that do have one.
          t.() { [1].freeze.extend(ExtM) },
          t.() { (1..2).extend(ExtM) },
          t.() { Rational(1, 2).extend(ExtM) },
          t.() { "xyz".dup.freeze.extend(ExtM) },
          t.() { s = "xyz".dup.freeze; def s.x = 1 },
          t.() { "xyz".dup.freeze.define_singleton_method(:x) { 1 } },
          t.() { "xyz".dup.freeze.singleton_class.frozen? },
          t.() { "xyz".dup.extend(ExtM).ext_foo },
        ]
        "##,
    );
}

#[test]
fn pooled_strings_have_no_singleton_class() {
    // A deduplicated String (`RSTRING_FSTR`) is shared by everything
    // that names the same content: TypeError, where any other frozen
    // String is a FrozenError (or gets a frozen singleton class).
    run_test_once(
        r##"
        module ExtM; def ext_foo = :foo; end
        t = ->(&b) { b.call rescue [$!.class, $!.message] }
        [
          t.() { "lit".freeze.extend(ExtM) },
          t.() { (-"string").singleton_class },
          t.() { (-("str" + "ing2")).singleton_class },
          t.() { s = -"lit2"; def s.x = 1 },
          t.() { s = -"lit3"; class << s; end },
          t.() { (-"lit4").define_singleton_method(:x) { 1 } },
          t.() { ExtM.send(:extend_object, -"lit5") },
        ]
        "##,
    );
}
