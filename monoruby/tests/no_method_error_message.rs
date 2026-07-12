//! Differential coverage for `NoMethodError#message`'s receiver
//! rendering, which calls the receiver's (or its class's) Ruby-level
//! `#name` — a class with an overridden `def self.name` shows that name.
//! Each snippet is compared against the reference CRuby.

use monoruby::tests::*;

#[test]
fn message_uses_ruby_name_for_named_class_and_module() {
    run_tests(&[
        r#"(k = Class.new { def self.name; "MyClass"; end }; begin; k.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(m = Module.new { def self.name; "MyModule"; end }; begin; m.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(k = Class.new { def self.name; "MyClass"; end }; begin; k.new.bar; rescue NoMethodError => e; e.message; end)"#,
    ]);
}

#[test]
fn message_receiver_special_forms() {
    run_tests(&[
        r#"(begin; nil.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(begin; true.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(begin; false.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(begin; 3.foo; rescue NoMethodError => e; e.message; end)"#,
    ]);
}

#[test]
fn message_does_not_call_inspect() {
    // Building the message must go through #name, never the receiver's
    // #inspect.
    run_test_once(
        r#"
        called = false
        klass = Class.new do
          define_method(:inspect) { called = true; "X" }
        end
        begin
          klass.new.bar
        rescue NoMethodError => e
          e.message
        end
        called
        "#,
    );
}

#[test]
fn private_and_protected_messages_unchanged() {
    run_tests(&[
        r#"(class SpecPrivC; private def priv; end; end; begin; SpecPrivC.new.priv; rescue NoMethodError => e; e.message; end)"#,
        r#"(class SpecProtC; protected def prot; end; end; begin; SpecProtC.new.prot; rescue NoMethodError => e; e.message; end)"#,
    ]);
}
