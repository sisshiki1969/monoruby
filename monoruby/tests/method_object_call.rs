//! `Method#call` reaches the bound method through the JIT's inline path
//! rather than the builtin, so each shape the inline path must still get
//! right gets a case here.
extern crate monoruby;
use monoruby::tests::*;

/// The ordinary shape, hot enough to be JIT-compiled: a fixed-arity
/// method called through a `Method` held in a local.
#[test]
fn a_hot_method_object_call() {
    run_test(
        r#"
        class C
          def g(x) = x ^ x >> 16
        end
        m = C.new.method(:g)
        a = 0
        i = 0
        while i < 30000
          a = m.call(i)
          i += 1
        end
        a
        "#,
    );
}

/// Every arity the inline argument copy has to handle, including zero,
/// plus the `[]` and `===` spellings that share the entry.
#[test]
fn arities_and_aliases() {
    run_test(
        r#"
        class C
          def none = 42
          def one(a) = a * 2
          def three(a, b, c) = a + b + c
        end
        o = C.new
        res = []
        i = 0
        while i < 3000
          res = [o.method(:none).call, o.method(:one).call(5),
                 o.method(:three).call(1, 2, 3),
                 o.method(:one)[7], o.method(:one).===(9)]
          i += 1
        end
        res
        "#,
    );
}

/// The receiver the callee sees is the one the `Method` is bound to, not
/// the `Method` object: the inline path writes `SELF` from the
/// `MethodInner`, so a method reading `self` would catch a slip here.
#[test]
fn the_bound_receiver_reaches_self() {
    run_test(
        r#"
        class C
          def initialize(n) = @n = n
          def get = @n
        end
        ms = [C.new(1).method(:get), C.new(2).method(:get)]
        res = []
        i = 0
        while i < 3000
          res = [ms[0].call, ms[1].call]
          i += 1
        end
        res
        "#,
    );
}

/// A `method_missing` proxy `Method` (from `respond_to_missing?`): the
/// inline path cannot express its dispatch and has to leave for the
/// runtime helper, which prepends the target name.
#[test]
fn a_method_missing_proxy() {
    run_test(
        r#"
        class P
          def respond_to_missing?(n, p) = n == :zap
          def method_missing(n, *a) = [n, a]
        end
        m = P.new.method(:zap)
        res = nil
        i = 0
        while i < 3000
          res = m.call(1, 2)
          i += 1
        end
        res
        "#,
    );
}

/// A call site alternating between a proxy `Method` and an ordinary one,
/// so the inline path and its bail-out both run at the same site.
#[test]
fn a_proxy_and_an_ordinary_method_at_one_site() {
    run_test(
        r#"
        class P
          def respond_to_missing?(n, p) = n == :zap
          def method_missing(n, *a) = [n, a]
          def real(x) = x + 1
        end
        o = P.new
        ms = [o.method(:zap), o.method(:real)]
        res = []
        i = 0
        while i < 6000
          res << ms[i % 2].call(i % 3)
          res.shift if res.size > 2
          i += 1
        end
        res
        "#,
    );
}

/// Wrong arity raises from the inline path's own check, with CRuby's
/// message.
#[test]
fn a_wrong_number_of_arguments() {
    run_test(
        r#"
        class C
          def one(a) = a
        end
        m = C.new.method(:one)
        res = nil
        i = 0
        while i < 3000
          begin
            m.call(1, 2)
          rescue ArgumentError => e
            res = e.message
          end
          i += 1
        end
        res
        "#,
    );
}

/// A block passed through `Method#call` reaches the callee's `yield`.
#[test]
fn a_block_travels_through_the_call() {
    run_test(
        r#"
        class C
          def each2
            yield 1
            yield 2
          end
        end
        m = C.new.method(:each2)
        acc = []
        i = 0
        while i < 3000
          acc = []
          m.call { |v| acc << v }
          i += 1
        end
        acc
        "#,
    );
}

/// A callee that is not a simple fixed-arity iseq (optional, rest and
/// keyword parameters) takes the generic argument marshaller instead of
/// the inline copy.
#[test]
fn a_callee_that_is_not_simple() {
    run_test(
        r#"
        class C
          def opt(a, b = 10) = a + b
          def rest(*a) = a
          def kw(a, k: 5) = a + k
        end
        o = C.new
        res = []
        i = 0
        while i < 3000
          res = [o.method(:opt).call(1), o.method(:opt).call(1, 2),
                 o.method(:rest).call(1, 2, 3), o.method(:kw).call(1)]
          i += 1
        end
        res
        "#,
    );
}

/// Floats crossing the call: the inline path saves and restores the
/// float registers around it, so a loop keeping floats live is the check.
#[test]
fn floats_live_across_the_call() {
    run_test(
        r#"
        class C
          def f(x) = x * 2.0
        end
        m = C.new.method(:f)
        z = 1.0
        w = 0.5
        i = 0
        while i < 30000
          z = z * 1.0000001 + m.call(w)
          w = z * 0.25
          i += 1
        end
        [z.round(6), w.round(6)]
        "#,
    );
}

/// `Method#call` on a builtin (a C function, not an iseq): the callee is
/// not a simple iseq, so this is the other side of the `not_simple`
/// branch.
#[test]
fn a_builtin_behind_a_method_object() {
    run_test(
        r#"
        m = "hello".method(:upcase)
        n = 10.method(:+)
        res = []
        i = 0
        while i < 3000
          res = [m.call, n.call(5)]
          i += 1
        end
        res
        "#,
    );
}

/// The same arity message from the inlined `send`, which shares the
/// argument copy this path was modeled on.
#[test]
fn a_wrong_number_of_arguments_through_send() {
    run_test(
        r#"
        class C
          def one(a) = a
        end
        o = C.new
        res = nil
        i = 0
        while i < 3000
          begin
            o.send(:one, 1, 2)
          rescue ArgumentError => e
            res = e.message
          end
          i += 1
        end
        res
        "#,
    );
}
