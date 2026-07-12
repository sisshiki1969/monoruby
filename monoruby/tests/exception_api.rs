//! Differential coverage for the exception APIs added for ruby/spec's
//! core/exception category: everything here runs the same snippet under
//! monoruby and the reference CRuby and compares results.

use monoruby::tests::*;

#[test]
fn full_message_format() {
    run_test_once(
        r#"
        e = RuntimeError.new("Some runtime error")
        e.set_backtrace(["a.rb:1", "b.rb:2"])
        [
          e.full_message(highlight: false, order: :top),
          e.full_message(highlight: false, order: :bottom),
          e.full_message(highlight: true, order: :top),
          e.full_message(highlight: true, order: :bottom),
        ]
        "#,
    );
}

#[test]
fn full_message_multiline_and_empty() {
    run_test_once(
        r#"
        res = []
        begin
          raise "first line\nsecond line\nthird line"
        rescue => e
          res << e.full_message(highlight: true, order: :top).lines[1..]
        end
        e = RuntimeError.new("")
        e.set_backtrace(["a.rb:1"])
        res << e.full_message(highlight: false)
        res << e.full_message(highlight: true)
        # Caller-derived location differs between harnesses; compare the tail.
        res << StandardError.new("").full_message(highlight: false).lines.first.end_with?(": StandardError\n")
        res
        "#,
    );
}

#[test]
fn full_message_cause_chain_and_detailed_message() {
    run_test_once(
        r#"
        res = []
        begin
          begin
            raise "the cause"
          rescue
            raise "main exception"
          end
        rescue => e
          fm = e.full_message(highlight: false)
          res << fm.include?("main exception") << fm.include?("the cause")
        end
        e = RuntimeError.new("new error")
        def e.detailed_message(**opts) = "DETAILED #{opts[:foo]}"
        # The first line's location is caller-derived (file paths differ
        # between the two harnesses), so compare only the tail.
        res << e.full_message(highlight: false, foo: 1).lines.first.end_with?(": DETAILED 1\n")
        res << RuntimeError.new("x\ny").detailed_message(highlight: true)
        res
        "#,
    );
}

#[test]
fn exception_inspect_and_nil_message() {
    run_tests(&[
        r#"RuntimeError.new("oops").inspect"#,
        r#"RuntimeError.new(nil).message"#,
        r#"RuntimeError.new(nil).inspect"#,
        r#"(o = Object.new; def o.to_s; "custom"; end; RuntimeError.new(o).message)"#,
    ]);
}

#[test]
fn local_jump_error_metadata() {
    run_test_once(
        r#"
        def m
          proc { return 42 }
        end
        begin
          m.call
        rescue LocalJumpError => e
          [e.class, e.exit_value, e.reason]
        end
        "#,
    );
}

#[test]
fn stop_iteration_result() {
    run_test_once(
        r#"
        o = Object.new
        def o.each
          yield :a
          yield :b
          :method_returned
        end
        e = o.to_enum
        e.next; e.next
        begin
          e.next
        rescue StopIteration => x
          [x.class, x.result]
        end
        "#,
    );
}

#[test]
fn uncaught_throw_error_tag() {
    run_test_once(
        r#"
        begin
          throw :abc
        rescue UncaughtThrowError => e
          [e.class, e.tag, e.message]
        end
        "#,
    );
}

#[test]
fn syntax_error_path() {
    run_tests(&[
        r#"(begin; eval("1+", binding, "speccing.rb"); rescue SyntaxError => e; e.path; end)"#,
        r#"SyntaxError.new.path"#,
    ]);
}

#[test]
fn name_error_and_no_method_error_constructors() {
    run_tests(&[
        r#"(e = NameError.new("msg", :sym, receiver: :recv); [e.message, e.name, e.receiver])"#,
        r#"(e = NoMethodError.new("msg", :m, [1, 2], receiver: :r); [e.message, e.name, e.args, e.receiver])"#,
        r#"NoMethodError.new("msg").args"#,
    ]);
}

#[test]
fn no_method_error_receiver_descriptions() {
    run_tests(&[
        r#"(begin; nil.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(begin; true.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(begin; 3.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(module SpecMod; end; begin; SpecMod.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(SpecKlass = Class.new; begin; SpecKlass.foo; rescue NoMethodError => e; e.message; end)"#,
        r#"(class SpecPriv; private def p2; end; end; begin; SpecPriv.new.p2; rescue NoMethodError => e; e.message; end)"#,
    ]);
}

#[test]
fn system_call_error_construction() {
    run_tests(&[
        r#"SystemCallError.new(Errno::EINVAL::Errno).class"#,
        r#"SystemCallError.new(Errno::EINVAL::Errno).message"#,
        r#"SystemCallError.new("custom message", Errno::EINVAL::Errno).message"#,
        r#"SystemCallError.new("custom message", Errno::EINVAL::Errno, "location").message"#,
        r#"(e = SystemCallError.new(99999); [e.class, e.message, e.errno])"#,
        r#"SystemCallError.new(nil, Errno::EINVAL::Errno).message"#,
        r#"SystemCallError.new("a message").message"#,
        r#"SystemCallError.new("foo", 2.9).errno"#,
        r#"(begin; SystemCallError.new(:foo, 1); rescue TypeError => e; e.message; end)"#,
        r#"(begin; SystemCallError.new("m", "x"); rescue TypeError => e; e.message; end)"#,
        r#"(begin; SystemCallError.new("m", Complex(1, 2)); rescue RangeError => e; e.message; end)"#,
        r#"Errno::ENOENT.new.message"#,
        r#"Errno::ENOENT.new("custom message", "location").message"#,
        r#"Errno::EINVAL.new.errno"#,
        r#"(SubEnoent = Class.new(Errno::ENOENT); SubEnoent.new("custom").message)"#,
        r#"SystemCallError.instance_method(:initialize).arity"#,
    ]);
}

#[test]
fn signal_exception_and_interrupt() {
    run_tests(&[
        r#"(e = SignalException.new(:TERM); [e.signo, e.signm, e.message])"#,
        r#"(e = SignalException.new("SIGTERM"); [e.signo, e.signm])"#,
        r#"(e = SignalException.new(15); [e.signo, e.signm])"#,
        r#"(e = Interrupt.new; [e.signo, e.message])"#,
        r#"(begin; SignalException.new(:NOSUCH); rescue ArgumentError => e; :err; end)"#,
        r#"ClosedQueueError.superclass"#,
    ]);
}
