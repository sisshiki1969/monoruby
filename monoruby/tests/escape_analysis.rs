//! Allocation elision for Array literals used as a receiver
//! (`doc/escape_analysis.md`).
//!
//! The JIT does not allocate `[a, b]` in `[a, b].max`: the literal is an
//! elided object in the abstract state until a use either consumes it
//! (the callee's receiver escape summary says how) or could observe it,
//! at which point it is allocated after all. These tests pin both halves:
//! the consuming uses compute what the builtins compute, and every way the
//! object can escape — a use the summary does not cover, an element slot
//! overwritten before the use, a block boundary, a side exit, a
//! redefinition of the callee — still sees a real, fresh Array.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn element_ops_on_slot_literals() {
    run_test(
        r#"
        def f(a, b)
          [[a, b].max, [a, b].min, [b, a].max, [a, b].size, [a, b].length,
           [a, b].empty?, [].empty?, [].size, [].max, [a, b].first, [a, b].last,
           [].first, [].last, [a].first, [a, b, a + b].last]
        end
        r = nil
        20.times { |i| r = f(i, 2.5) }
        r + f(1, "x".size)
        "#,
    );
}

#[test]
fn element_ops_result_unused_and_in_conditions() {
    run_test(
        r#"
        def f(a, b)
          [a, b].max
          [a, b].first
          n = 0
          n += 1 if [a, b].empty?
          n += 10 unless [a, b].empty?
          n += 100 if [a, b].first == a
          n += [a, b].max if a > 1
          n
        end
        r = []
        20.times { |i| r << f(i, 3) }
        r
        "#,
    );
}

#[test]
fn transparent_callees_on_literals_of_immediates() {
    run_test(
        r#"
        def f(x)
          [[:a, :b].include?(x), [1, 2, 3].include?(x), [1, 2, 3].sum,
           [1, 2, 3].sum { |v| v * 2 }, [1, 2, 3].count { |v| v > 1 },
           [3, 1, 2].sort, [1, 2, 3].join("-"), [1, 2, 3].minmax,
           [1, 2].take(5), [1, 2, 3].drop(1), [1, 2] + [3], [1, 2, 2] - [2],
           [1, 2, nil].compact, [1, 1, 2].uniq, [1, 2].reverse,
           [1, 2] & [2], [1] | [2], [1, 2].hash == [1, 2].hash,
           [1, nil].any?, [1, nil].all?, [1.5, :s, nil, true].inspect,
           [1, 2, 3].pack("C*"), [1, 2].max, [1, 2].first, [].size]
        end
        r = nil
        20.times { |i| r = f(i % 4) }
        r + f(:a)
        "#,
    );
}

#[test]
fn transparent_results_do_not_share_the_template() {
    // Every transparent callee that returns an Array returns a new one:
    // mutating it must not change what the literal evaluates to next time.
    run_test(
        r#"
        def f
          a = [3, 1, 2].sort; a << 9
          b = [1, 2].take(5); b << 9
          c = [1, 2] + [3]; c << 9
          d = [1, 2, 2].uniq; d << 9
          e = [1, nil].compact; e << 9
          [[3, 1, 2].sort, [1, 2].take(5), [1, 2] + [3], [1, 2, 2].uniq, [1, nil].compact, a, b]
        end
        r = nil
        20.times { r = f }
        r
        "#,
    );
}

#[test]
fn escaping_uses_see_a_real_array() {
    run_test(
        r#"
        def g(a) = a
        def f(a, b)
          x = [a, b]
          y = [a, b].map { |v| v * 2 }
          z = [a, b].tap { |t| t << 3 }
          w = g([a, b])
          v = [a, b].include?(a + 1)
          u = [a, b][1]
          t = [a, b].frozen?
          s = [[a, b].max, [a, b]]
          q = [1, 2].each { }
          q << 5
          [x, y, z, w, v, u, t, s, q, [1, 2]]
        end
        r = nil
        20.times { |i| r = f(i, 7) }
        r
        "#,
    );
}

#[test]
fn literal_returned_and_across_blocks() {
    run_test(
        r#"
        def mk(a, b) = [a, b]
        def mk2 = [1, 2]
        def f(a, b, c)
          x = c ? [a, b] : [b, a]
          y = ([a, b] if c)
          x << 0
          [x, y, mk(a, b), mk2.push(3), mk2]
        end
        r = []
        20.times { |i| r << f(i, 1, i.even?) }
        r.last(3)
        "#,
    );
}

#[test]
fn redefinition_after_compile_allocates_again() {
    // The consuming lowerings are guarded by the class version: once
    // `Array#max` / `#include?` are redefined, the call must see a real,
    // fresh Array each time — which the redefinitions here return and
    // mutate.
    run_test_once(
        r#"
        def f(i)
          v = [1, 2].include?(1)
          v << 3 if v.is_a?(Array)
          w = [i, 2].max
          w << 4 if w.is_a?(Array)
          [v, w]
        end
        res = []
        40.times do |i|
          if i == 30
            class Array
              def include?(x) = self
              def max = self
            end
          end
          res << f(i)
        end
        res.last(12) + [[1, 2].include?(1), [5, 6].max]
        "#,
    );
}

#[test]
fn side_exit_with_an_elided_literal_pending() {
    // A type guard between the literal and its use deopts with the literal
    // still elided; the side exit has to allocate it.
    run_test_once(
        r#"
        def f(a, x)
          [a, 1].max + [:p, :q].include?(x).to_s.size
        end
        res = []
        30.times { |i| res << f(i, :p) }
        res << f(2.5, :q)
        res << f("z".size, nil)
        res
        "#,
    );
}

#[test]
fn incomparable_elements_raise() {
    run_test_error(
        r#"
        def f(a, b) = [a, b].max
        20.times { f(1, 2) }
        f(1, "a")
        "#,
    );
}

#[test]
fn refined_callee_is_not_elided() {
    // A refinement in scope resolves the call elsewhere; the receiver
    // escape summary of the builtin no longer applies.
    run_test_once(
        r#"
        module R
          refine Array do
            def max = self
            def include?(x) = self
          end
        end
        using R
        def f(a, b)
          x = [a, b].max
          x << 0
          y = [1, 2].include?(1)
          y << 3
          [x, y, [1, 2]]
        end
        r = nil
        30.times { |i| r = f(i, 1) }
        r
        "#,
    );
}

// ---------------------------------------------------------------------------
// Elided objects inside specialized callees (`doc/escape_analysis.md` §6):
// a `*rest` parameter whose Array is never built, and an elided literal
// handed over as an argument. Each callee is called enough to specialize.
// ---------------------------------------------------------------------------

#[test]
fn rest_parameter_elided_in_specialized_callee() {
    run_test(
        r#"
        def f(*args)
          [args.size, args.length, args.empty?, args.first, args.last, args.max, args.min]
        end
        def g(a, *rest, k: 1)
          [a, rest.size, rest.first, rest.last, k]
        end
        def h(a, b = 2, *rest)
          [a, b, rest.size, rest.max]
        end
        res = []
        30.times do |i|
          res << f(i, 2, 3) << f(i) << f << g(i, 5, 6) << g(i, k: 2) << h(i) << h(i, 3) << h(i, 3, 9, 1)
        end
        res.last(8)
        "#,
    );
}

#[test]
fn rest_read_again_after_an_elements_op_and_across_branches() {
    // `args.size` consumes the elements, not the object: the parameter is
    // read again, in another basic block, and at the end of the method.
    run_test(
        r#"
        def f(*args)
          if args.size > 2
            args.first + args.last
          else
            args.max
          end
        end
        def g(*args)
          n = args.size
          y = helper(n)
          args.first + y
        end
        def helper(x) = x + 1
        def h(*args)
          args.size
          args
        end
        res = []
        30.times { |i| res << f(i, 1, 2) << f(i, 100) << g(i, 2, 3) << h(i, 7) }
        res.last(4)
        "#,
    );
}

#[test]
fn rest_escaping_uses_see_a_real_array() {
    // A use the summary does not cover (`each`, a local, an ivar, `<<`,
    // `[]`, a non-forwarding splat) materializes the rest Array inside the
    // callee, and the result is a fresh, correct Array every call.
    run_test(
        r#"
        def each_sum(*args)
          s = 0
          args.each { |x| s += x }
          s
        end
        def to_local(*args)
          x = args
          x << 1
          x
        end
        def to_ivar(*args)
          @a = args
          @a.size
        end
        def index(*args) = args[1]
        def splat(*args) = tgt(*args)
        def tgt(a, b, c = 9) = a + b + c
        def ret(*args) = args
        res = []
        30.times do |i|
          res << each_sum(i, 1) << to_local(i, 2) << to_ivar(i, 2, 3) << index(i, 8) << splat(i, 2) << splat(i, 2, 3)
          r = ret(i, 1)
          r << 5
          res << r
        end
        res.last(7)
        "#,
    );
}

#[test]
fn rest_forwarded_through_a_trampoline() {
    // `def f(...) = g(...)`: the rest window is routed straight into `g`'s
    // frame, with and without literal keywords.
    run_test(
        r#"
        def f(...) = g(...)
        def g(a, b = 2, k: 3) = a + b + k
        def h(...) = i(...)
        def i(*rest) = rest.sum
        res = []
        30.times { |n| res << f(n) << f(n, 1) << f(n, k: 5) << f(n, 1, k: 7) << h(n, 1, 2) << h }
        res.last(6)
        "#,
    );
}

#[test]
fn elided_literal_argument_into_specialized_callees() {
    // `f([a, b])` hands the literal over elided: a frameless callee (reads
    // it through its window), a framed one (through its saved frame
    // pointer, forced by the optional parameter), and a literal of
    // immediates kept as a template for a transparent callee.
    run_test(
        r#"
        def frameless(a) = a.max + a.size
        def framed(a, b = 1) = a.max + a.size + b
        def first_last(a, b = 0) = a.first + a.last + b
        def inc(a, x) = a.include?(x)
        def two(a, b) = a.size + b.size
        res = []
        30.times do |i|
          res << frameless([i, 3]) << framed([i, 3]) << framed([i, 3], 2) << first_last([i, 4, 5])
          res << inc([:x, :y], :y) << inc([:x, :y], i) << two([i], [i, i, i])
        end
        res.last(7)
        "#,
    );
}

#[test]
fn elided_argument_escaping_in_the_callee() {
    // The callee stores, returns, copies or iterates the argument: it is
    // materialized there, and the caller never sees a shared object.
    run_test(
        r#"
        def keep(a) = (@k = a)
        def ret(a) = a
        def copy(a)
          b = a
          b.size
        end
        def each(a)
          s = 0
          a.each { |x| s += x }
          s
        end
        def sizes(a, b = 0) = a.size + b
        res = []
        30.times do |i|
          r = ret([i, 1])
          r << 2
          k = keep([i, 3])
          k << 4
          res << r << k << @k << copy([i, 5]) << each([i, 6]) << sizes([i, [i, 7].max])
        end
        res.last(6)
        "#,
    );
}

#[test]
fn elided_argument_with_a_block_or_splat_is_materialized() {
    run_test(
        r#"
        def with_block(a)
          a.map { |x| yield x }
        end
        def with_rest(*a) = a.size
        def two(a, b) = a.size + b.size
        res = []
        30.times do |i|
          res << with_block([i, 1]) { |x| x + 1 } << with_rest(*[i, 2]) << two(*[[i], [i, 3]])
        end
        res.last(3)
        "#,
    );
}

#[test]
fn deopt_inside_a_callee_with_an_elided_rest_pending() {
    // A class-version guard fails inside `f` while its rest is still
    // elided: the side exit builds the Array from the caller's window and
    // the interpreter resumes with it. Then the same for an elided literal
    // handed over as an argument.
    run_test_once(
        r#"
        def helper(x) = x + 1
        def f(*args)
          y = helper(args.size)
          args.first + args.last + y
        end
        def g(a, b = 1)
          y = helper(b)
          a.max + a.size + y
        end
        res = []
        30.times { |i| res << f(i, 2) << g([i, 9]) }
        def helper(x) = x + 100
        res << f(1, 2) << g([3, 4])
        30.times { |i| res << f(i, 2) << g([i, 9]) }
        res.last(4)
        "#,
    );
}

#[test]
fn exception_and_gc_inside_a_callee_with_an_elided_rest() {
    run_test_once(
        r#"
        def f(*args)
          raise ArgumentError, "too many" if args.size > 3
          args.first
        end
        def g(*args)
          GC.start
          args.first + args.last
        end
        def h(*args)
          args.max
        rescue => e
          e.class
        end
        res = []
        30.times { |i| res << f(i, 2) << g(i, 1) << h(i, 2) }
        res << (begin f(1, 2, 3, 4) rescue $!.message end)
        res << h(1, "a")
        res.last(5)
        "#,
    );
}

#[test]
fn binding_in_the_callee_sees_the_rest() {
    run_test(
        r#"
        def f(*args)
          b = binding
          b.local_variable_get(:args).size + args.size
        end
        def g(*args)
          eval("args").size
        end
        res = []
        30.times { |i| res << f(i, 2) << g(i) }
        res.last(2)
        "#,
    );
}

#[test]
fn rest_in_a_loop_and_a_recursive_callee() {
    run_test(
        r#"
        def f(*args)
          s = 0
          i = 0
          while i < args.size
            s += args.first
            i += 1
          end
          s
        end
        def rec(n, *args)
          return args.size if n == 0
          rec(n - 1, *args) + args.first
        end
        res = []
        30.times { |i| res << f(i, 2, 3) << rec(3, i, 1) }
        res.last(2)
        "#,
    );
}

#[test]
fn side_exits_under_a_routed_forward() {
    // `def f(...) = g(...)` with the rest routed straight into `g`'s frame
    // (never built): a class-version guard failing inside `g`, an
    // exception raised there, a `binding` taken there, and the forwarded
    // `initialize` of `Class#new` all see the real arguments.
    run_test_once(
        r##"
        def helper(x) = x + 1
        def g(a, b = 1) = a + b + helper(a)
        def f(...) = g(...)
        def r(a, b)
          raise(ArgumentError, "#{a}-#{b}") if b > 25
          a
        end
        def fr(...) = r(...)
        def bd(a, b = 7) = binding.local_variable_get(:b) + a
        def fb(...) = bd(...)
        class Pt
          attr_reader :x, :y
          def initialize(x, y = 0) = (@x, @y = x, y)
        end
        res = []
        30.times do |i|
          res << f(i, 2) << f(i) << fb(i) << fb(i, 2) << Pt.new(i, 1).y << Pt.new(i).x
          begin
            fr(i, i)
          rescue ArgumentError => e
            res << e.message
          end
        end
        def helper(x) = x + 100
        res << f(1, 2)
        res.last(10)
        "##,
    );
}

#[test]
fn chain_deopt_under_a_pending_literal_argument() {
    // A literal of immediates handed over elided into `t`'s parameter,
    // still pending while `t` is suspended in a framed callee that deopts:
    // the chain conversion of `t`'s frame copies the template into the
    // slot.
    run_test_once(
        r#"
        def helper(x) = x.to_s.size + x
        def t(a, b)
          y = helper(b)
          a.max + y
        end
        def w(i) = t([1, 2], i)
        res = []
        30.times { |i| res << w(i) }
        res << w(1.5)
        30.times { |i| res << w(i) }
        res.last(3)
        "#,
    );
}
