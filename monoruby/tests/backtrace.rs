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

/// An argument that cannot be bound (a wrong arity, a missing or
/// unknown keyword) is raised from the callee's frame, which CRuby
/// reports at the line of its `def` / block rather than as `<internal>`
/// (#1870) — whichever way the callee was reached.
#[test]
fn argument_error_frame_is_the_callee_definition() {
    run_test_once(
        r#"
        def m(a)
        end
        def k(a:, b: 1)
        end
        def o(a, b = 1,
              c = 2)
        end
        class C
          def im(x) = x
          define_method(:dm) { |x| x }
        end
        def show
          yield
        rescue => e
          # Only the callee's own frame: the frames between it and the
          # caller differ for `send` / `Method#call` (#1894).
          loc = e.backtrace_locations[0]
          [e.message, e.backtrace[0].sub(/\A.*:(\d+:in )/, '\1'), loc.lineno, loc.label]
        end
        l = ->(a) {}
        pr = proc { |a:| }
        [
          show { m },
          show { m(1, 2) },
          show { k },
          show { k(a: 1, c: 2) },
          show { o },
          show { C.new.im },
          show { C.new.dm },
          show { send(:m) },
          show { method(:m).call },
          show { l.call },
          show { pr.call },
        ]
        "#,
    );
}

/// The same, once the caller is JIT-compiled: the direct call, and the
/// inlined `send` / `Method#call` fast paths that check a simple
/// callee's arity in the caller's code.
#[test]
fn argument_error_frame_is_the_callee_definition_jit() {
    // `run_test_once`: the loops warm the JIT themselves, and the
    // repeated copies `run_test` makes would move the line numbers.
    run_test_once(
        r#"
        def m(a)
        end
        class C
          def im(x) = x
        end
        def direct = m
        def snd(o, n) = o.send(n)
        def mcall(mm) = mm.call
        r = []
        mm = method(:m)
        c = C.new
        [
          -> { direct },
          -> { snd(self, :m) },
          -> { snd(c, :im) },
          -> { mcall(mm) },
          -> { c.im(1, 2) },
        ].each do |f|
          200.times do
            f.call
          rescue ArgumentError => e
            r << [e.message, e.backtrace[0].sub(/\A.*:(\d+:in )/, '\1')]
          end
        end
        r.tally
        "#,
    );
}
