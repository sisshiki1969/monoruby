//! Differential coverage for `Exception#backtrace` / `#backtrace_locations`
//! / `#set_backtrace`: the raise-time stack is captured in full (even when
//! the exception is raised and rescued within the same method), frame
//! labels use fully-qualified class names, and the returned array is
//! memoized. Compared against the reference CRuby.

use monoruby::tests::*;

#[test]
fn backtrace_includes_callers_when_rescued_in_same_method() {
    run_test_once(
        r#"
        def inner
          begin
            raise "x"
          rescue => e
            return e.backtrace.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') }
          end
        end
        def outer; inner; end
        outer
        "#,
    );
}

#[test]
fn backtrace_propagated_full_chain() {
    run_test_once(
        r#"
        def a; raise "x"; end
        def b; a; end
        def c; b; end
        begin
          c
        rescue => e
          e.backtrace.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') }
        end
        "#,
    );
}

#[test]
fn backtrace_uses_qualified_class_name() {
    run_test_once(
        r#"
        module NsX
          class Cx
            def self.boom; raise "x"; end
          end
        end
        begin
          NsX::Cx.boom
        rescue => e
          e.backtrace.first.sub(/\A.*:\d+:in /, '')
        end
        "#,
    );
}

#[test]
fn backtrace_array_is_memoized_and_mutable() {
    run_test_once(
        r#"
        begin
          raise "x"
        rescue => e
          e.backtrace.unshift("first")
          [e.backtrace[0], e.backtrace.equal?(e.backtrace), e.dup.backtrace.equal?(e.backtrace)]
        end
        "#,
    );
}

#[test]
fn set_backtrace_strings_leaves_locations_nil() {
    run_test_once(
        r#"
        err = RuntimeError.new
        before = [err.backtrace, err.backtrace_locations]
        err.set_backtrace(["a.rb:1:in 'x'"])
        [before, err.backtrace, err.backtrace_locations]
        "#,
    );
}

#[test]
fn backtrace_of_unraised_exception_is_nil() {
    run_tests(&[
        r#"Exception.new.backtrace"#,
        r#"Exception.new.backtrace_locations"#,
        r#"RuntimeError.new("x").backtrace"#,
    ]);
}
