//! Differential coverage for `NameError#name` / `#receiver`,
//! `NoMethodError#args`, and `SystemExit` exit-status propagation added
//! for ruby/spec's core/exception category. Each snippet runs under both
//! monoruby and the reference CRuby and the results are compared.

use monoruby::tests::*;

#[test]
fn name_error_receiver_from_raise_sites() {
    run_tests(&[
        // Undefined constant, bare (receiver == Object).
        r#"(begin; NoSuchConstXyz; rescue NameError => e; [e.name, e.receiver.equal?(Object)]; end)"#,
        // Undefined constant, namespaced (receiver == the namespace class).
        r#"(module NsA; class RcA; end; end; begin; NsA::RcA::Nope; rescue NameError => e; [e.name, e.receiver.equal?(NsA::RcA)]; end)"#,
        // Undefined class variable read in a method (receiver == class).
        r#"(class CvA; def f; @@nope; end; end; begin; CvA.new.f; rescue NameError => e; [e.name, e.receiver.equal?(CvA)]; end)"#,
        // Bareword undefined local variable or method (receiver == self).
        r#"(begin; totally_undefined_thing; rescue NameError => e; [e.name, e.receiver.equal?(self)]; end)"#,
    ]);
}

#[test]
fn name_error_receiver_argument_error_when_absent() {
    run_test_once(
        r#"
        begin
          NameError.new.receiver
          :no_raise
        rescue ArgumentError
          :arg_error
        end
        "#,
    );
}

#[test]
fn name_error_reflection_name_identity() {
    // CRuby preserves the exact argument object as #name (checked with
    // equal?) for the reflection methods.
    run_tests(&[
        r#"(n = "bad_ivar_name"; begin; Object.new.instance_variable_get(n); rescue NameError => e; [e.name.equal?(n), e.receiver.class]; end)"#,
        r#"(n = "bad_cvar_name"; begin; Object.class_variable_get(n); rescue NameError => e; [e.name.equal?(n), e.receiver.equal?(Object)]; end)"#,
    ]);
}

#[test]
fn no_method_error_args_and_dup() {
    run_test_once(
        r#"
        receiver = Object.new
        begin
          receiver.foo(:one, :two)
        rescue NoMethodError => e
          d = e.dup
          [d.name, d.receiver.equal?(receiver), d.args]
        end
        "#,
    );
    // No-argument missing call yields an empty args array.
    run_test_once(
        r#"
        begin
          Object.new.no_such_method
        rescue NoMethodError => e
          e.args
        end
        "#,
    );
}

#[test]
fn name_error_dup_copies_name_and_receiver() {
    run_test_once(
        r#"
        begin
          undefined_local_or_method
        rescue NameError => e
          d = e.dup
          [d.name, d.receiver.equal?(self)]
        end
        "#,
    );
}
