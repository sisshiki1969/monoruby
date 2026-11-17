extern crate monoruby;
use monoruby::tests::*;

// Issue #1994: `attr_*` / `define_method` read the default visibility
// of the frame that calls them only when that frame is the body of the
// receiver itself (CRuby's `rb_vm_cref_in_context(klass, klass)`):
// `K.send(:attr_accessor, :x)` from the top level, from a method, or
// from another class's `private` section defines public methods.
#[test]
fn attr_and_define_method_take_the_scope_visibility_of_their_receiver_only() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        class Foo; end
        class Bar; end
        Foo.send(:attr_reader, :r); Foo.attr_writer :w; Foo.send(:define_method, :dm) { }
        rowp Foo.public_instance_methods(false).sort, Foo.private_instance_methods(false).sort
        def helper(k) = k.send(:attr_accessor, :h)
        helper(Foo); rowp Foo.public_method_defined?(:h)
        class Bar
          private
          Foo.send(:attr_accessor, :from_bar_private)
          attr_accessor :own
        end
        rowp Foo.public_method_defined?(:from_bar_private), Bar.private_method_defined?(:own)
        Foo.class_eval { attr_accessor :ce }
        rowp Foo.public_method_defined?(:ce)
        Foo.class_eval { private; attr_accessor :ce2 }
        rowp Foo.private_method_defined?(:ce2)
        class Baz
          def self.mk; attr_accessor :z; define_method(:dm2) {}; end
          def self.mk2; class_eval { private; attr_accessor :z2; define_method(:dm3) {} }; end
          private
          mk
          mk2
          attr_reader :initialize
        end
        rowp Baz.public_method_defined?(:z), Baz.public_method_defined?(:dm2), Baz.private_method_defined?(:z2), Baz.private_method_defined?(:dm3)
        rowp Baz.private_instance_methods(false).sort
        class Qux
          protected
          attr_accessor :pr
          Object.send(:attr_accessor, :on_object_from_qux)
        end
        rowp Qux.protected_method_defined?(:pr), Object.public_method_defined?(:on_object_from_qux)
        Object.send(:attr_accessor, :on_object_top)
        rowp Object.public_method_defined?(:on_object_top)
        private
        Foo.send(:attr_accessor, :after_top_private)
        rowp Foo.public_method_defined?(:after_top_private)
        $o
        "##,
    );
}
