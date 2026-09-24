extern crate monoruby;
use monoruby::tests::*;

// The method lists — `Module#instance_methods` and its public / private
// / protected kin, `Kernel#methods` and its kin — take a class's whole
// ancestry, `Object`, `Kernel` and `BasicObject` included, as CRuby's
// do (#1646); a module's list stays the module and what it includes.
// monoruby's own `__helper` methods are left out of every list.

/// What a plain class inherits from Object / Kernel / BasicObject.
#[test]
fn a_class_lists_what_it_inherits_from_object() {
    run_test_once(
        r##"
        class K; def mine; end; protected def prot; end; private def priv; end; end
        module M; def m; end; end
        class L; include M; end
        [K.instance_methods.include?(:dup), K.instance_methods.include?(:frozen?), K.instance_methods.include?(:instance_eval),
         K.public_instance_methods.include?(:dup), K.public_instance_methods.include?(:prot),
         K.private_instance_methods.include?(:puts), K.private_instance_methods.include?(:priv),
         K.protected_instance_methods, K.instance_methods(false).sort,
         K.new.methods.include?(:dup), K.new.public_methods.include?(:itself), K.new.private_methods.include?(:puts),
         K.new.protected_methods, Class.new.instance_methods.size == Object.instance_methods.size,
         (K.instance_methods - Object.instance_methods).sort, L.instance_methods.include?(:m), L.instance_methods.include?(:dup),
         String.instance_methods.include?(:dup), Class.new(BasicObject).instance_methods.sort,
         M.instance_methods, Comparable.instance_methods.sort, Kernel.instance_methods.include?(:dup),
         Kernel.instance_methods.include?(:instance_eval), Enumerable.instance_methods.include?(:dup)]
        "##,
    );
}

/// monoruby's internal helpers (`__assert`, `__warn_deprecated`,
/// `___dlopen`, …) are no method CRuby lists; `__send__`, `__id__` and
/// friends are.
#[test]
fn internal_helpers_are_not_listed() {
    run_test_once(
        r##"
        class K; def __mine; end; end
        [K.instance_methods.grep(/\A__/).sort, K.private_instance_methods.grep(/\A__/).sort,
         Object.new.methods.grep(/\A__/).sort, Object.new.private_methods.grep(/\A__/).sort,
         String.instance_methods.grep(/\A__/).sort, Kernel.private_instance_methods.grep(/\A__/).sort,
         BasicObject.instance_methods.sort, 1.respond_to?(:display), Kernel.instance_method(:display).owner]
        "##,
    );
}

/// OpenStruct makes its `!`-aliases from `instance_methods`, so every
/// member it defines needs `define_singleton_method!` among them.
#[test]
fn openstruct_defines_its_members() {
    run_test_once(
        r##"
        require "rubygems"
        require "ostruct"
        o = OpenStruct.new(a: 1)
        o.foo = 2
        o[:bar] = 3
        r = [o.to_h, o.foo, o.bar, o.respond_to?(:foo), o.dig(:a), o.each_pair.to_a]
        o.delete_field(:foo)
        r + [o.to_h, o.foo, OpenStruct.new(x: 1) == OpenStruct.new(x: 1)]
        "##,
    );
}

#[test]
fn display_writes_to_the_port() {
    run_test_once(
        r##"
        require "stringio"
        io = StringIO.new
        [1.display(io), "ab".display(io), [1, :s].display(io), io.string]
        "##,
    );
}
