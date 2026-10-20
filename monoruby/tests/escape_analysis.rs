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

#[test]
fn rest_forwarded_to_a_polymorphic_target() {
    // `def f(...) = @o.m(...)` where the forward's receiver is one of
    // several classes: the site is compiled as a class dispatch, which
    // takes no elided object, so the trampoline's rest is materialized
    // before the split (rubocop's `Node#each_descendant(...)` forwards
    // this way).
    run_test(
        r#"
        class A; def m(a, b = 1) = a + b; end
        class B; def m(*r) = r.sum * 2; end
        class C; def m(a, b = 0, k: 0) = a - b - k; end
        class W
          def initialize(o) = @o = o
          def f(...) = @o.m(...)
        end
        ws = [W.new(A.new), W.new(B.new), W.new(C.new)]
        res = []
        60.times { |i| w = ws[i % 3]; res << w.f(i, 2) << w.f(i) }
        res.last(6)
        "#,
    );
}

#[test]
fn elided_argument_live_across_a_dispatch_that_branches() {
    // A literal handed over into a callee whose first use of it is behind
    // a polymorphic call feeding an `if`: the dispatch emits the branch
    // per arm, so the object has to be performed before the split, not
    // left for the branch target to see one copy of and the fall-through
    // another (psych's `ToRuby#register(node, [])`).
    run_test(
        r#"
        class N; def initialize(a) = @a = a; def anchor = @a; end
        class M; def initialize(a) = @a = a; def anchor = @a && @a.to_s; end
        class R
          def initialize = @st = {}
          def register(node, object)
            @st[node.anchor] = object if node.anchor
            object
          end
          def reg_empty(node) = register(node, [])
          def st = @st
        end
        r = R.new
        nodes = [N.new(1), M.new(2), N.new(nil), M.new(nil)]
        res = []
        40.times { |i| res << r.reg_empty(nodes[i % 4]).size }
        [res.sum, r.st]
        "#,
    );
    // The same with a comparison dispatch (`Integer` / `Float` operands).
    run_test(
        r#"
        def g(x, lst)
          lst.push(x) if x > 0.5
          lst.size
        end
        def h(x) = g(x, [1])
        res = 0
        40.times { |i| res += h(i.odd? ? i : i * 0.3) }
        res
        "#,
    );
}

// ---- Hash literals (`doc/escape_analysis.md` §7) ----

#[test]
fn hash_template_folds_and_transparent_callees() {
    // A Hash literal of immediates is a template: a lookup by a constant
    // key it holds is folded, `size` / `empty?` / `key?` by a constant key
    // are constants, and every other use that lets no receiver escape runs
    // on the template itself.
    run_test(
        r#"
        def f(m, x)
          [{a: 1, b: 2}[:a], {a: 1, b: 2}[:zz], {a: 1, b: 2}.fetch(:b),
           {a: 1, b: 2}.fetch(:zz, x), {a: 1}.fetch(:a, x), {a: 1, b: 2}.key?(:a),
           {a: 1}.key?(:zz), {a: 1, b: 2}.size, {a: 1, b: 2, a: 3}.length,
           {a: 1}.empty?, {}.empty?, {}.size, {1 => :one, 2.5 => :f}[1],
           {1 => :one, 2.5 => :f}[2.5], {nil => 0, true => 1}[true],
           {get: 1, head: 2}[m], {get: 1, head: 2}.key?(m), {get: 1, head: 2}.fetch(m, 0),
           {get: 1, head: 2}.include?(m), {get: 1, head: 2}.has_key?(:get),
           {get: 1, head: 2}.member?(:post), {a: 1, b: 2}.dig(m),
           {a: 1, b: 2}.values_at(m, :a), {a: 1, b: 2}.merge({c: 3}),
           {a: 1, b: 2}.keys, {a: 1, b: 2}.values, {a: 1, b: 2}.invert,
           {b: 1, a: 2}.sort, {a: 1, b: 2}.inspect, {a: 1}.to_s, {a: 1} == {a: 1},
           {a: 1}.eql?({a: 1}), {a: 1}.hash == {a: 1}.hash, {a: 1, b: 2}.key(2),
           {a: 1, b: 2}.assoc(:b), {a: 1, b: 2}.rassoc(1), {a: 1, b: 2}.any?,
           {}.any?, {a: 1, b: 2}.all?, {a: 1, b: 2}.count, {a: 1, b: nil}.compact,
           {a: 1, b: 2}.slice(:a), {a: 1, b: 2}.except(:a), {a: 1, b: 2}.fetch_values(:a),
           {a: 1, b: 2}.has_value?(x), {a: 1, b: 2}.value?(2), {a: {b: 2}}.dig(:a, :b),
           {a: 1, b: 2}.flatten, {a: 1} < {a: 1, b: 2}, {a: 1, b: 2} >= {a: 1}]
        end
        r = nil
        20.times { |i| r = f(i.odd? ? :get : :head, i) }
        r + f(:post, 1) + f(:a, 2)
        "#,
    );
}

#[test]
fn hash_template_results_do_not_share_the_template() {
    // What a transparent callee returns is fresh, and a use the summary
    // does not cover (a mutation, an identity, an iteration, a local) sees
    // a fresh copy of the literal every time — the template itself is
    // never mutated.
    run_test(
        r#"
        def f(i)
          a = {a: 1, b: 2}.merge({c: i})
          a[:d] = i
          b = {a: 1, b: 2}.keys
          b << :z
          c = {a: 1, b: 2}
          c[:x] = i
          d = {a: 1, b: 2}.delete(:a)
          e = {a: 1}.merge!({b: i})
          g = {a: 1}.equal?({a: 1})
          h = {a: 1, b: 2}.each { |k, v| }
          j = {a: 1, b: 2}.to_a
          k = {a: 1}.freeze.frozen?
          l = {a: 1, b: 2}.map { |k, v| v }
          m = {a: 1, b: 2}.select { |k, v| v > 1 }
          n = {a: 1}.each_with_object([]) { |kv, acc| acc << kv }
          [a, b, c, d, e, g, h, j, k, l, m, n, {a: 1, b: 2}.size]
        end
        r = nil
        20.times { |i| r = f(i) }
        r
        "#,
    );
}

#[test]
fn hash_template_with_strings_or_heap_values_is_copied() {
    // Keys or values that are not immediates (a String key, an Array
    // value) take the literal off the template path: a mutation through
    // the value must not reach the next evaluation.
    run_test(
        r#"
        def f(i)
          v = {a: [1, 2]}[:a]
          v << i
          w = {"k" => 1, :s => 2}.size
          x = {"k" => i}["k"]
          y = {a: "str"}.fetch(:a)
          y << "!"
          [v, w, x, y]
        end
        r = nil
        20.times { |i| r = f(i) }
        r
        "#,
    );
}

#[test]
fn hash_redefinition_after_compile_allocates_again() {
    // The folds and the template hand-over are guarded by the class
    // version: once `Hash#[]` / `#key?` / `#size` are redefined, each call
    // sees a real, fresh Hash — which the redefinitions return and mutate.
    run_test_once(
        r#"
        def f(i)
          v = {a: 1, b: 2}[:a]
          v[:n] = i if v.is_a?(Hash)
          w = {a: 1}.key?(:a)
          w[:m] = i if w.is_a?(Hash)
          x = {a: 1, b: 2}.size
          [v, w, x]
        end
        res = []
        40.times do |i|
          if i == 30
            class Hash
              def [](k) = self
              def key?(k) = self
              def size = 99
            end
          end
          res << f(i)
        end
        res.last(12) + [{a: 1}[:a], {b: 1}.key?(:b)]
        "#,
    );
}

#[test]
fn side_exit_with_an_elided_hash_pending() {
    // A type guard between the literal and its use deopts with the
    // template still elided; the side exit has to copy it.
    run_test_once(
        r#"
        def f(a, m)
          {a: 1, b: 2}[:a] + a * 2 + {get: 1, head: 2}.fetch(m, 0)
        end
        res = []
        30.times { |i| res << f(i, :get) }
        res << f(2.5, :head)
        res << f("z".size, :post)
        res
        "#,
    );
}

#[test]
fn hash_template_handed_into_a_specialized_callee() {
    // A template passed as an argument stays elided in the callee, whose
    // folds and transparent uses apply there too.
    run_test(
        r#"
        def lookup(h, k) = h[k]
        def has(h) = h.key?(:x)
        def sz(h) = h.size + (h.empty? ? 10 : 0)
        def esc(h) = h
        def f(i)
          [lookup({x: 1, y: 2}, :y), lookup({x: 1, y: 2}, i.odd? ? :x : :zz),
           has({x: 1}), has({z: 1}), sz({x: 1, y: 2}), sz({}), esc({x: i}).size]
        end
        r = nil
        20.times { |i| r = f(i) }
        r
        "#,
    );
}

// ---- `**kwrest` handed over (`doc/escape_analysis.md` §7) ----

#[test]
fn kwrest_elided_in_specialized_callee() {
    // The literal keywords a specialized callee declares no parameter for
    // stay in the caller's kw window: the callee's `**kwrest` is an elided
    // object, and a lookup by a constant key reads the caller's slot.
    run_test(
        r#"
        def f(a, **opts) = a + opts[:x] + opts.fetch(:y, 10) + (opts.key?(:z) ? 100 : 0) + opts.size * 1000
        def g(**opts) = opts.empty? ? -1 : opts.fetch(:k)
        def h(a, x: 1, **opts) = a + x + opts[:z]
        def w(i)
          [f(i, x: 1), f(i, x: 2, y: 3), f(i, z: 0, x: 4), f(i, y: 1, x: 2, z: 3),
           g, g(k: i), h(i, z: 5), h(i, x: 7, z: 5), f(i, x: i, y: i * 2.5)]
        end
        r = nil
        20.times { |i| r = w(i) }
        r + w(1.5)
        "#,
    );
}

#[test]
fn kwrest_escaping_uses_see_a_real_hash() {
    // Every use the folds do not cover materializes the Hash — once — and
    // the caller's window is left alone.
    run_test(
        r#"
        def ret(**opts) = opts
        def merge(**opts) = opts.merge(y: 2)
        def each(**opts) = opts.map { |k, v| [k, v] }
        def pass(**opts) = inner(opts)
        def inner(h) = h[:x]
        def splat(**opts) = inner2(**opts)
        def inner2(x: 0, **rest) = x + rest.size
        def fold_then_escape(**opts)
          v = opts[:x]
          [v, opts]
        end
        def mutate(**opts)
          opts[:added] = 1
          opts
        end
        def var_key(k, **opts) = opts[k]
        def w(i)
          [ret(x: i), ret, merge(x: i), each(x: i, z: 2), pass(x: i), splat(x: i, q: 1),
           fold_then_escape(x: i, y: 1), mutate(x: i), var_key(:a, a: i, b: 2),
           var_key(:c, a: i, b: 2)]
        end
        r = nil
        20.times { |i| r = w(i) }
        r
        "#,
    );
}

#[test]
fn kwrest_duplicate_keys_and_heap_values() {
    // A key written twice holds the later value; heap values survive a
    // GC run while the Hash is still elided (they are in the caller's
    // spilled window).
    run_test(
        r#"
        def f(**opts)
          GC.start
          opts[:x] + opts.fetch(:y).size
        end
        def w(i) = f(x: i, y: "a" * i, x: i + 1)
        r = nil
        20.times { |i| r = w(i) }
        r
        "#,
    );
}

#[test]
fn deopt_inside_a_callee_with_an_elided_kwrest_pending() {
    // A class-version guard fails inside `f` while its `**kwrest` is still
    // elided: the side exit rebuilds the Hash from the caller's window and
    // the interpreter resumes with it. Then a chain deopt: `f` suspended in
    // a framed callee that deopts, its `**kwrest` pending.
    run_test_once(
        r#"
        def helper(x) = x + 1
        def f(a, **opts)
          y = helper(a)
          opts[:x] + opts.size + y
        end
        res = []
        30.times { |i| res << f(i, x: 2) }
        def helper(x) = x + 100
        res << f(1, x: 2)
        30.times { |i| res << f(i, x: 2, y: 1) }
        res.last(4)
        "#,
    );
    run_test_once(
        r#"
        def helper(x) = x.to_s.size + x
        def t(a, **opts)
          y = helper(a)
          opts[:x] + y
        end
        def w(i) = t(i, x: 3)
        res = []
        30.times { |i| res << w(i) }
        res << w(1.5)
        30.times { |i| res << w(i) }
        res.last(3)
        "#,
    );
}

#[test]
fn kwrest_with_a_splat_or_block_is_built() {
    // A `**hash` splat at the call site and a forwarding trampoline (whose
    // keywords K1 routes) take the ordinary path; a callee that captures
    // its frame (`binding`, `eval`) materializes the object before the
    // capturing call, the `*rest` kind included.
    run_test(
        r#"
        def f(**opts) = opts[:x].to_s + opts.size.to_s
        def cap(**opts) = binding.local_variable_get(:opts)[:x]
        def cap_rest(*args) = binding.local_variable_get(:args).size
        def ev(**opts) = eval("opts")[:x]
        def blk(**opts) = [1].map { |v| v + opts[:x] }
        def fwd(...) = tgt(...)
        def tgt(a, x: 0, **rest) = a + x + rest.size
        def w(i)
          h = {x: i, q: 1}
          [f(**h), f(x: i, **h), cap(x: i), cap_rest(i, 2), ev(x: i), blk(x: i),
           fwd(i, x: 1, z: 2), fwd(i)]
        end
        r = nil
        20.times { |i| r = w(i) }
        r
        "#,
    );
}

#[test]
fn range_element_ops_on_dynamic_endpoints() {
    // `lo..hi` with Fixnum (or nil) endpoints is elided; the receiver
    // escape summaries of `Range#include?` and friends answer from the
    // endpoints in place, and a non-numeric argument or an endpoint the
    // method rejects goes through the real method on a Range built after
    // all.
    run_test(
        r#"
        def cover(a, b, x)
          [(a..b).include?(x), (a...b).include?(x), (a..b).cover?(x), (a..b).member?(x),
           (a..b) === x, (a..nil).include?(x), (nil..b).include?(x), (a..b).include?(x.to_f),
           (a...b).include?(x + 0.5), (a..b).include?("s"), (a..b).include?(nil),
           (a..b).include?(2**70), (a..).cover?(x), (..b).cover?(x)]
        end
        def ops(a, b)
          [(a..b).size, (a...b).size, (a..b).count, (a...b).count, (a..b).sum, (a...b).sum,
           (a..b).min, (a..b).max, (a...b).max, (a...b).min, (a..b).first, (a..b).last,
           (a..b).begin, (a...b).end, (a...b).exclude_end?, (a..b).exclude_end?,
           (a..nil).size, (a..nil).count, (nil..b).count, (a..nil).min, (nil..b).max,
           (a..b) == (a..b), (a..b).hash == (a...b).hash]
        end
        def walks(a, b)
          [(a..b).to_a, (a..b).minmax, (a..b).map { |v| v * 2 }, (a..b).reject(&:even?),
           (a..b).all? { |v| v > 0 }]
        end
        res = []
        30.times do |i|
          res << cover(1, 5, i) << ops(i, i + 3) << ops(5, 2) << ops(i, i) << walks(i, i + 3)
        end
        res << cover(1, 5, 2.5) << ops(-(2**62), 2**62 - 1) << ops(0, 2**40) << walks(5, 2)
        res
        "#,
    );
}

#[test]
fn range_endpoint_errors_and_fallbacks() {
    // A nil endpoint the method rejects raises from the real method.
    run_test(
        r#"
        def probe(a)
          r = []
          [-> { (nil..a).first }, -> { (a..nil).last }, -> { (nil..a).size },
           -> { (nil..a).min }, -> { (a..nil).max }, -> { (nil...a).max },
           -> { (nil..a).sum }].each do |f|
            r << begin
              f.call
            rescue => e
              e.class
            end
          end
          r
        end
        res = []
        25.times { |i| res << probe(i) }
        res
        "#,
    );
}

#[test]
fn range_literal_templates_and_case_when() {
    // A `lo..hi` literal with immediate endpoints is a template; the
    // endpoint compare of `case … when lo..hi` is inline, and the
    // argument-less ops are folded at compile time.
    run_test(
        r#"
        def lit(x)
          r = case x
              when 1..5 then :low
              when 6...10 then :mid
              when 10.. then :high
              when ..-5 then :verylow
              else :neg
              end
          [r, (1..3).include?(x), (1..3).size, (1...3).sum, (1..3).min, (1..3).max,
           (1...1).min, (1..3).first, (1..3).last, (1..3).begin, (1...3).end,
           (1...3).exclude_end?, (1..3).to_a, (1..3).count, (..3).count, (1..).size,
           (1.0..3.0).include?(x), ("a".."c").include?("b"), (1..3) === x, (1..3) === "x",
           (1..3) == (1..3), (1..3).eql?(1...3), (1..3).hash == (1..3).hash,
           (1..3).include?(x.to_f), (1..10).sum, (1..(2**62)).sum, (3..1).sum, (3..1).max,
           (1..3).to_set.size, (1..5).overlap?(4..9), (1..100).bsearch { |v| v >= x.to_i },
           (:a..:c).include?(:b), (nil..nil).include?(x.to_i)]
        end
        res = []
        30.times { |i| res << lit(i - 8) }
        res << lit(2.5) << lit("s") << lit(nil)
        res
        "#,
    );
}

#[test]
fn range_escaping_uses_see_a_real_range() {
    // Every use the summaries do not cover — a local, an argument, `each`
    // (which returns self), `step`, identity — sees a real Range.
    run_test(
        r#"
        def f(a, b, x)
          r = a..b
          s = (a...b)
          t = r.each { |v| }
          u = (a..b).step(2).to_a
          v = (a..b).each_with_index.to_a
          [r, s, t.equal?(r), u, v, (a..b).frozen?, (a..b).equal?(a..b), (a..b).dup,
           (a..b).instance_of?(Range), (a..b).to_s, (a..b).inspect, [a..b].first,
           (a..b).tap { |z| }, g(a..b), (a..b).then { |z| z.size }]
        end
        def g(r) = r
        res = []
        25.times { |i| res << f(i, i + 2, i) }
        res
        "#,
    );
}

#[test]
fn range_endpoint_slot_overwritten_before_the_use() {
    // An endpoint local written while the Range is elided, and an
    // endpoint temporary clobbered by a later push: the object is built
    // before the write, with the old endpoint.
    run_test(
        r#"
        def f(a, b, x)
          r = (a..b).include?(x)
          b = 100
          s = (a..b).include?(x)
          y = (a...b.size).sum
          z = (a..b.to_s.size).include?(x)
          w = (a..b).include?(x.to_s.size)
          [r, s, y, z, w, b]
        end
        res = []
        25.times { |i| res << f(i, i + 5, i + 1) }
        res
        "#,
    );
}

#[test]
fn range_redefinition_after_compile_allocates_again() {
    // The consuming lowerings are guarded by the class version: once
    // `Range#include?` / `#size` are redefined, the call must see a real,
    // fresh Range each time.
    run_test_once(
        r#"
        def f(a, b, x)
          v = (a..b).include?(x)
          w = (a...b).size
          u = case x when a..b then :in else :out end
          [v, w, u]
        end
        res = []
        40.times do |i|
          if i == 30
            class Range
              def include?(x) = self
              def size = self
              def ===(x) = self
            end
          end
          res << f(1, i, 3)
        end
        res.last(12) + [(1..2).include?(1), (5..6).size]
        "#,
    );
}

#[test]
fn range_cover_with_redefined_integer_cmp() {
    // The inline Fixnum compare stands in for `Integer#<=>`; once that is
    // redefined, the real method answers.
    run_test_once(
        r#"
        def f(a, b, x) = [(a..b).include?(x), (1..5) === x, (a..b).cover?(x), (a..b).min]
        res = []
        40.times do |i|
          if i == 30
            class Integer
              def <=>(o) = 0
            end
          end
          res << f(1, 5, i)
        end
        res.last(12)
        "#,
    );
}

#[test]
fn range_template_handed_into_a_specialized_callee() {
    // A literal Range passed as an argument stays elided in the callee,
    // where its endpoints are constants; a use that needs the object
    // builds it there.
    run_test(
        r#"
        def g(r, x) = [r.include?(x), r.size, r.first, r.min, r.sum, r === x]
        def h(r, x) = [r.include?(x), r.include?(x.to_s), r]
        def f(x)
          [g(1..5, x), g(1...5, x), h(2..4, x)]
        end
        res = []
        30.times { |i| res << f(i) }
        res << f(2.5) << f("q")
        res
        "#,
    );
}

#[test]
fn range_with_non_fixnum_endpoints_is_built() {
    // Only Fixnum / nil endpoints are elided: a Float, Bignum, String or
    // unknown-class endpoint builds the Range as before (and `lo..hi`
    // with `hi < lo` is still a Range).
    run_test(
        r#"
        def f(a, b, x)
          [(a.to_f..b).include?(x), (a..b.to_f).include?(x), ((2**70)..(2**71)).include?(2**70 + 1),
           (a.to_s..b.to_s).include?(x.to_s), (a..b).include?(x), (b..a).to_a, (b...a).size,
           (a..b).step(2).size]
        end
        res = []
        25.times { |i| res << f(i, i + 4, i + 2) }
        res
        "#,
    );
}

#[test]
fn range_element_ops_result_unused_and_in_conditions() {
    run_test(
        r#"
        def f(a, b, x)
          (a..b).include?(x)
          (a..b).size
          n = 0
          n += 1 if (a..b).include?(x)
          n += 10 unless (a...b).cover?(x)
          n += 100 if (a..b).size > 3
          n += (a..b).sum if x > 1
          n += 1000 if (1..3) === x
          n
        end
        res = []
        25.times { |i| res << f(i, i + 3, i + 1) }
        res
        "#,
    );
}

#[test]
fn elided_parameter_carried_through_a_merge() {
    // The elided object in a parameter slot survives a forward merge when
    // every path brings it: an optional parameter's prologue (`b = 0`),
    // an optional keyword next to `**kwrest`, a guard clause, and a
    // `?:` before the use. A loop head is not such a merge: the object is
    // built before the loop.
    run_test(
        r#"
        def opt(a, b = 0) = a.max + b
        def kwo(x: 1, **opts) = opts[:y].to_s + x.to_s + opts.size.to_s
        def guard(*a)
          return 0 if a.empty?
          a.max
        end
        def after_merge(*a, flag)
          n = flag ? 1 : 2
          a.max + n
        end
        def in_loop(*a)
          s = 0
          3.times { |i| s += i }
          a.max + s
        end
        def chain(*a, flag)
          return a.size if flag.nil?
          n = flag ? 10 : 20
          m = n > 15 ? 1 : 2
          a.first + a.last + m
        end
        res = []
        30.times do |i|
          res << opt([i, 2]) << opt([i, 2], 7) << kwo(x: 2, y: i) << kwo(y: i)
          res << guard(i, 2) << guard << after_merge(i, 4, true) << after_merge(i, 4, false)
          res << in_loop(i, 5) << chain(i, 1, true) << chain(i, 1, false) << chain(i, 1, nil)
        end
        res.last(12)
        "#,
    );
}

#[test]
fn elided_parameter_materialized_on_one_path_of_a_merge() {
    // One path escapes the object (reads the parameter itself, hands it
    // on, stores it) and the other consumes it: the merge sees a real
    // object on the first path's bridge and none on the other, and every
    // use after the merge is right on both.
    run_test(
        r#"
        def one_path(*a)
          if a.size > 1
            a
          else
            a.max
          end
        end
        def stores(*a, flag)
          @keep = a if flag
          a.max
        end
        def hands_on(*a, flag)
          x = flag ? a.dup.size : 0
          a.max + x
        end
        def kw_one(x: 1, **opts)
          h = x > 1 ? opts : {}
          [opts[:y], h.size]
        end
        res = []
        30.times do |i|
          res << one_path(i, 2) << one_path(i) << stores(i, 2, true) << stores(i, 2, false)
          res << hands_on(i, 3, true) << hands_on(i, 3, false) << kw_one(x: 2, y: i) << kw_one(y: i)
        end
        res << @keep
        res.last(9)
        "#,
    );
}

#[test]
fn side_exit_after_a_carried_merge() {
    // A type guard fails after the merge the elided parameter was carried
    // through: the side exit allocates it, and a chain deopt in a callee
    // called after the merge finds it pending.
    run_test_once(
        r#"
        def helper(x) = x + 1
        def f(*a, flag)
          n = flag ? 1 : 2
          y = helper(n)
          a.max + y
        end
        def g(x, b = 0)
          n = b + 1
          x.max + n
        end
        def w(i, flag) = g([i, 1], flag ? 2 : 3)
        res = []
        30.times { |i| res << f(i, 2, true) << w(i, true) }
        res << f(2.5, 1, false) << f("ab".size, 1, true)
        res << w(1.5, false) << w("x".size, true)
        def helper(x) = x + 100
        res << f(3, 4, true)
        30.times { |i| res << f(i, 2, false) }
        res.last(8)
        "#,
    );
}

#[test]
fn range_fallbacks_from_method_bodies() {
    // Dynamic endpoints inside method bodies (where they are proven
    // Fixnum): the ops the summaries do not plan (`first(n)`, `max(n)`),
    // the errors the helpers raise through the real method, and the
    // spans that overflow i64 at compile time and in the helper.
    run_test(
        r#"
        def firsts(a, b) = [(a..b).first(2), (a..b).max(2), (a...b).min(2), (a..b).last(2)]
        def huge(a, b) = [(a..b).size, (a...b).count, (a..b).sum, (a..b).min, (a..b).max]
        def huge_lit = [(-4611686018427387904..4611686018427387903).size,
                        (0..4611686018427387903).sum, (0...4611686018427387903).count]
        res = []
        25.times do |i|
          res << firsts(i + 1, 100) << huge(-4611686018427387904 + i, 4611686018427387903) << huge_lit
        end
        res.last(3)
        "#,
    );
}

#[test]
fn case_when_on_array_and_hash_literals_and_a_private_teq() {
    // `when [1, 2]` / `when {a: 1}` dispatch `===` (an alias of `==`,
    // transparent) on a literal of immediates: the template is the
    // receiver, nothing is allocated. A private `Range#===` is still
    // reachable from case/when (funcall semantics) but not from a plain
    // `(a..b) === x`, which raises.
    run_test(
        r#"
        def lit(x)
          case x
          when [1, 2] then :arr
          when {a: 1} then :hash
          when 1..5 then :range
          else :none
          end
        end
        res = []
        30.times { |i| res << lit([1, 2]) << lit({a: 1}) << lit(i) << lit(:s) }
        res.last(4)
        "#,
    );
    run_test(
        r#"
        class Range
          private :===
        end
        def kase(a, b, x)
          case x
          when a..b then :in
          else :out
          end
        end
        def plain(a, b, x) = (a..b) === x
        res = []
        30.times do |i|
          res << kase(1, 5, i)
          res << begin
            plain(1, 5, i)
          rescue NoMethodError => e
            e.class
          end
        end
        res.last(4)
        "#,
    );
}

#[test]
fn constant_range_literal_is_a_template() {
    // `lo..hi` with literal endpoints is a frozen literal: the template is
    // answered from inline (case/when, `include?`, the folds) or handed
    // into a specialized callee, and a use that needs the object gets the
    // literal itself — the same object every time, never a copy.
    run_test(
        r#"
        def inc(r, x) = r.include?(x)
        def folds(r) = [r.size, r.sum, r.min, r.max, r.first, r.last, r.begin, r.end, r.exclude_end?]
        def endless(r) = [r.size, r.min, r.first, r.begin, r.end, r.exclude_end?]
        def keep(r) = r
        def uses(x)
          [(1..3).frozen?, (1..3).step(2).to_a, (1...9).each_slice(4).to_a, (1..3).equal?(keep(1..3)),
           (1..5).include?(x), (1..5) === x, (1..5).first(2), (-4611686018427387904..4611686018427387903).size]
        end
        res = []
        30.times do |i|
          res << inc(1..5, i) << inc(1...5, i) << inc(1..5, i.to_f) << folds(1...4) << endless(1..nil)
          res << keep(1..3).equal?(keep(1..3)) << uses(i)
        end
        res.last(7)
        "#,
    );
}

#[test]
fn range_endpoint_copies_and_helper_errors() {
    // `first` / `last` / `begin` / `end` on dynamic endpoints copy the
    // endpoint slot, and a nil endpoint the fold cannot answer raises
    // from the real method, called by the helper on a Range built then.
    // (A parameter is proven Fixnum only in a callee specialized for a
    // call site with a literal argument, or by arithmetic on it — `a - 1`
    // — so the shapes here use one or the other.)
    run_test(
        r#"
        def fl(a, b) = [(a..b).first, (a..b).last, (a..b).begin, (a...b).end, (a..b).exclude_end?,
                        (a..nil).first, (nil..b).last, (nil..b).end]
        def sz_nil(a) = (nil..a - 1).size
        def min_nil(a) = (nil..a - 1).min
        def max_nil(a) = (a + 1..nil).max
        def max_nil_excl(a) = (nil...a - 1).max
        def first_nil(a) = (nil..a - 1).first
        def last_nil(a) = (a + 1..nil).last
        def sum_nil(a) = (nil..a - 1).sum
        res = []
        25.times do |i|
          res << fl(i + 1, 100)
          res << (sz_nil(i) rescue $!.class) << (min_nil(i) rescue $!.class) << (max_nil(i) rescue $!.class)
          res << (max_nil_excl(i) rescue $!.class) << (first_nil(i) rescue $!.class)
          res << (last_nil(i) rescue $!.class) << (sum_nil(i) rescue $!.class)
        end
        res.last(8)
        "#,
    );
}

#[test]
fn elided_object_returned_from_a_callee() {
    // A specialized callee's plain `return` of an elided Array / template /
    // Range hands the object over to the call site elided
    // (`doc/escape_analysis.md` §9): the caller's consuming use computes
    // what the builtin computes, with the elements copied into the
    // caller's slots (a self-call's result slot is its first argument
    // slot, so the copies overlap the callee's arguments).
    run_test(
        r#"
        def pair(a, b) = [a, b]
        def rest(*a) = a
        def tmpl = [1, 2, 3]
        def rng(a, b) = (a..b)
        def xrng(a, b) = (a...b)
        def crng = (1..5)
        def with_ivar(a, b)
          @n = (@n || 0) + 1
          [b, a]
        end
        def f(x, y)
          [pair(x, y).max, pair(y, x).min, pair(x, y).first, pair(x, y).last, pair(x, y).size,
           rest(x, y).min, rest(x, y, 7).max, rest.size, rest(x).first, rest(x, y).empty?,
           tmpl.size, tmpl.first, tmpl.last, tmpl.max, tmpl.include?(x),
           rng(x, x + 10).include?(5), rng(1, x + 10).cover?(7), xrng(x, x + 2).include?(x + 2),
           rng(x, x + 3).size, rng(x, x + 3).sum, rng(x + 1, x + 3).min, rng(x, x + 3).last,
           crng.cover?(x), crng.size, with_ivar(x, y).max, with_ivar(x, y).first]
        end
        @n = 0
        r = nil
        20.times { |i| r = f(i, 3) }
        [r, f(2, 1), f("ab".size, 10), @n]
        "#,
    );
}

#[test]
fn returned_object_destructured() {
    // `a, b = f(x, y)` expands an Array returned elided straight into
    // its targets, which overlap the element slots (the expansion's
    // temporaries sit right above the result): fewer elements than
    // targets gives `nil`, more are dropped, and a template's constants
    // and a `*rest` handed back are expanded the same way.
    run_test(
        r#"
        def pair(a, b) = [a, b]
        def rest(*a) = a
        def tmpl = [1, 2, 3]
        def two(x, y)
          a, b = pair(x, y)
          a + b
        end
        def two_rev(x, y)
          a, b = pair(y, x)
          [a, b]
        end
        def three(x, y)
          a, b, c = rest(x, y)
          [a, b, c, c.nil?]
        end
        def one(x, y)
          a, = pair(x, y)
          a
        end
        def lit
          a, b, c = tmpl
          a + b * c
        end
        def shorter(x)
          a, b = rest(x, x + 1, x + 2)
          [a, b]
        end
        def nested(x, y)
          a, b = pair(x, y)
          c, d = pair(b, a)
          [a, b, c, d]
        end
        res = []
        20.times { |i| res << two(i, 2) << two_rev(i, 2) << three(i, 3) << one(i, 4) << lit << shorter(i) << nested(i, 5) }
        res.last(7)
        "#,
    );
}

#[test]
fn return_paths_agree_or_materialize() {
    // Every return path must hand over the same object for the call site
    // to take it elided: two literals of the same length agree (whatever
    // their elements), a path returning an element (not an object) or a
    // literal of another length makes every path allocate, and an
    // elided object the caller itself goes on to return reaches the
    // caller's caller as a real Array.
    run_test(
        r#"
        def two_paths(a, b, flag)
          if flag
            [a, b]
          else
            [b, a]
          end
        end
        def mixed(a, b, flag)
          return a if flag
          [a, b]
        end
        def lengths(a, b, flag)
          flag ? [a, b] : [a, b, a + b]
        end
        def early(a, b)
          return [b, a] if a > b
          [a, b]
        end
        def f(x, y)
          [two_paths(x, y, x > 5).max, two_paths(x, y, x > 5).first,
           mixed(x, y, x > 5), mixed(x, y, x <= 5), mixed(x, y, false).max,
           lengths(x, y, x > 5).size, lengths(x, y, x <= 5).last,
           early(x, y).first, early(y, x).last]
        end
        def up(x, y) = two_paths(x, y, true)
        def up2(x, y)
          r = up(x, y)
          r << 1
          r
        end
        res = []
        20.times { |i| res << f(i, 7) << up2(i, 2) }
        res.last(2)
        "#,
    );
}

#[test]
fn returned_object_not_carried() {
    // The hand-over is declined where the callee's frame may be observed
    // or the object has escaped before the `return`: a callee with an
    // exception handler, one passing a block, one that captures its
    // binding, a `**kwrest` returned, an object stored before being
    // returned, and a `return` from inside a block. Each still sees a
    // real, fresh Array.
    run_test(
        r#"
        def with_rescue(a, b)
          [a, b]
        rescue
          nil
        end
        def with_block(a, b) = [a, b].tap { |x| x }
        def with_binding(a, b)
          c = [a, b]
          binding
          c
        end
        def kwr(**o) = o
        def stored(a, b)
          @keep = [a, b]
          @keep
        end
        def from_block(a, b)
          [1].each { |_| return [a, b] }
        end
        def f(x, y)
          r1 = with_rescue(x, y); r1 << x
          r2 = with_block(x, y); r2 << y
          r3 = with_binding(x, y); r3 << 1
          r5 = stored(x, y); r5 << 2
          r6 = from_block(x, y); r6 << 3
          [r1, r2, r3, kwr(a: x)[:a], kwr(a: x).size, r5, r6, @keep]
        end
        res = []
        20.times { |i| res << f(i, 2) }
        res.last
        "#,
    );
}

#[test]
fn errors_and_heap_elements_through_an_elided_return() {
    // The consuming use after the hand-over can raise (`max` over
    // incomparable elements) and can see heap elements; a returned
    // object whose consuming use is a type-guarded op after a deopt
    // still computes the right value.
    run_test_once(
        r#"
        def pair(a, b) = [a, b]
        def mx(x, y) = pair(x, y).max
        def sum(x, y)
          a, b = pair(x, y)
          a + b
        end
        res = []
        20.times { |i| res << mx(i, 2) << mx("a#{i}", "b") << sum(i, 1) }
        res << (mx(1, "x") rescue $!.class) << sum(1.5, 2) << sum("a", "b") << mx(2, 1)
        def pair(a, b) = [b, a, a]
        res << mx(3, 9) << sum(3, 9)
        res.last(7)
        "#,
    );
}

#[test]
fn elided_return_into_a_polymorphic_call_site() {
    // The call site dispatches on the receiver's class: each arm's callee
    // may hand its object back elided, and the arms' merge takes the
    // destination as a value, so the object is allocated at the end of
    // the arm (whether the use after the merge consumes it or not).
    run_test(
        r#"
        class Base
          def pair(v) = [v, v + 1]
          def m(v) = pair(v).max
        end
        class A < Base; end
        class B < Base
          def pair(v) = [v + 2, v]
        end
        class C < Base; end
        def f(o, i)
          a, b = o.pair(i)
          [o.pair(i).max, o.m(i), a + b, o.pair(i)]
        end
        objs = [A.new, B.new, C.new]
        res = []
        30.times { |i| res << f(objs[i % 3], i) }
        res.last(3)
        "#,
    );
}

#[test]
fn discarded_return_never_built() {
    // A call site that discards the result: the specialized callee's
    // `Ret` drops the object instead of building it, while everything
    // the callee did on the way still happens.
    run_test(
        r#"
        class C
          attr_reader :log
          def initialize = @log = []
          def pair(a, b)
            @log << a
            [a, b]
          end
          def rest(*a) = a
          def tmpl = [1, 2, 3]
          def rng(a, b) = (a..b)
          def mixed(a, b, flag)
            return a if flag
            [a, b]
          end
          def nested(a, b) = pair(a, b)
          def setter_like(a, b)
            @last = pair(b, a)
            pair(a, b)
          end
          def f(x, y)
            pair(x, y)
            rest(x, y)
            tmpl
            rng(x + 1, x + 10)
            mixed(x, y, x.odd?)
            nested(x, y)
            setter_like(x, y)
            x + y
          end
        end
        c = C.new
        res = []
        20.times { |i| res << c.f(i, i * 2) }
        [res, c.log.size, c.log.last(3), c.instance_variable_get(:@last)]
        "#,
    );
    // A callee the gate declines (a rescue, a block at the call site) or
    // whose object escapes before the return allocates as before.
    run_test(
        r#"
        def guarded(a, b)
          [a, b]
        rescue
          nil
        end
        def with_block(a, b)
          yield
          [a, b]
        end
        def kept(a, b)
          $kept = [a, b]
          $kept
        end
        def f(x, y)
          guarded(x, y)
          with_block(x, y) { }
          kept(x, y)
          $kept.sum
        end
        res = []
        20.times { |i| res << f(i, i + 1) }
        res
        "#,
    );
}

#[test]
fn local_holds_an_elided_object_across_uses_and_calls() {
    // `x = [a, b]`: the object lives in the local, its elements in the
    // local's scratch window, through several uses and across calls the
    // compiler knows cannot see the frame.
    run_test(
        r#"
        def g(i) = i + 1
        def f(a, b)
          x = [a, b]
          n = x.max
          n += g(a)
          n += x.first + x.last
          n += x.size
          r = (a..b)
          n += g(b)
          n += 1 if r.include?(a + 1)
          n += r.size
          h = {k: 1, v: 2}
          n += g(n)
          n += h[:k] + h.size
          e = []
          n += g(1)
          n + e.size
        end
        res = []
        20.times { |i| res << f(i, i + 3) }
        res << f(1, 1.5.floor)
        res
        "#,
    );
}

#[test]
fn local_object_moved_and_reassigned() {
    // `y = x` with `x` dead moves the object; `y = x` with `x` still read
    // (an alias) builds it, and both see the same Array. A local
    // reassigned with another literal reuses its window.
    run_test(
        r#"
        def f(a, b)
          t = [a, b]
          x = t
          n = x.max
          x = [b, a, n]
          n += x.last
          y = x
          y << 1
          n += x.size + y.size
          r = (a...b)
          s = r
          n + (s.include?(a) ? 10 : 0)
        end
        res = []
        20.times { |i| res << f(i, i + 2) }
        res
        "#,
    );
}

#[test]
fn local_object_escaping_uses_see_a_real_object() {
    // Every store that lets a local's object out of the frame builds it
    // first: an ivar, a gvar, a constant, a class variable, an index
    // store, an argument to a native callee, a return from the root.
    run_test(
        r#"
        class C
          @@cv = nil
          def f(a, b)
            x = [a, b]
            @iv = x
            n = x.max
            y = [b, a]
            $gv = y
            z = [a, a]
            @@cv = z
            w = [b, b]
            w[0] = n
            u = [a, n]
            Kernel.send(:format, "%d", u.size)
            v = (a..b)
            $rg = v
            [n, @iv.sum, $gv.sum, @@cv.sum, w.sum, u.sum, $rg.sum]
          end
        end
        res = []
        c = C.new
        20.times { |i| res << c.f(i, i + 3) }
        res
        "#,
    );
}

#[test]
fn local_object_materialized_before_a_capture() {
    // A block literal, a `binding`, an `eval` and a generic `send` can
    // all reach the frame's locals, so an object a local holds is built
    // before any of them, and what they see is the real Array.
    run_test(
        r#"
        def f(a, b)
          x = [a, b]
          n = x.max
          s = 0
          2.times { |i| s += x.size + i }
          y = [b, a]
          bd = binding
          n += bd.local_variable_get(:y).first
          z = [a, a, b]
          n += eval("z.size")
          w = [b, b]
          bd2 = send(:binding)
          n += bd2.local_variable_get(:w).sum
          v = (a..b)
          3.times { |i| s += v.include?(i) ? 1 : 0 }
          n + s + y.max
        end
        res = []
        20.times { |i| res << f(i, i + 2) }
        res
        "#,
    );
}

#[test]
fn local_object_around_loops_and_branches() {
    // Built before a loop head; kept through a forward merge; a literal
    // inside a loop body is consumed in the body.
    run_test(
        r#"
        def f(a, b)
          x = [a, b]
          i = 0
          while i < 3
            i += 1
          end
          n = x.max + i
          y = a > 3 ? [a, 1] : [b, 2]
          n += y.first
          z = [a, b]
          n += z.size if a > 1
          n += z.max
          i = 0
          while i < 4
            p = [i, a]
            n += p.max
            i += 1
          end
          r = (a..b)
          n += r.sum if b > a
          n
        end
        res = []
        20.times { |i| res << f(i, i + 3) }
        res
        "#,
    );
}

#[test]
fn side_exit_and_chain_deopt_with_a_local_object_pending() {
    // A type guard deopts while a local still holds its object (the
    // side exit allocates it); a basic-op redefinition inside a callee
    // converts the suspended caller frame by chain, which has to build
    // the object from the frame's own scratch slots.
    run_test_once(
        r#"
        def f(a, c)
          x = [a, 1]
          r = (a..a + 2)
          d = c + 1
          x.max + d + (r.include?(d) ? 1 : 0)
        end
        res = []
        30.times { |i| res << f(i, i) }
        res << f(2, 2.5)
        res << f("z".size, 1)
        res
        "#,
    );
    run_test_once(
        r#"
        $redef = proc do
          class Integer
            def +(o) = 1000
          end
        end
        def g(i)
          $redef.call if i == 25
          i
        end
        def f(a)
          x = [a, 7]
          r = (a..a + 2)
          g(a)
          x.max + r.sum
        end
        res = []
        40.times { |i| res << f(i) }
        res.last(20)
        "#,
    );
}

#[test]
fn local_object_in_blocks_and_returned_from_a_callee() {
    // A block's own local holds an object; a specialized callee returns
    // the object its local holds, elided, into the call site.
    run_test(
        r#"
        def pair(a, b)
          x = [a, b]
          x
        end
        def rng(a, b)
          r = (a..b)
          r
        end
        def f(a, b)
          s = 0
          3.times { |i| x = [i, a]; s += x.max }
          s += pair(a, b).max
          c, d = pair(b, a)
          s += c - d
          s += rng(a, b).size
          s
        end
        res = []
        20.times { |i| res << f(i, i + 2) }
        res
        "#,
    );
}

#[test]
fn local_object_with_a_handler_or_a_frameless_body() {
    // A body that rescues keeps allocating (the handler could read any
    // local); the result is the same either way.
    run_test(
        r#"
        def f(a, b)
          x = [a, b]
          y = x.max
          raise "boom" if a == 5
          y + x.size
        rescue
          x.sum
        end
        def small(a, b)
          x = [a, b]
          x.max
        end
        res = []
        20.times { |i| res << f(i, i + 1) << small(i, 3) }
        res
        "#,
    );
}

#[test]
fn local_object_written_by_a_block_or_replaced() {
    // A block literal that assigns the local replaces the object behind
    // the compiler's back: it is real by the time the block runs. A local
    // redefined without being read drops its elided object instead.
    run_test_once(
        r##"
        def each3
          yield 1; yield 2; yield 3
        end
        def f(n)
          x = [n, n + 1]
          each3 { |i| x = [i, i] if i == 2 }
          y = 1..n
          y = [n, n, n]
          [x.sum, y.size]
        end
        def g(n)
          r = n..n + 9
          r = [n]
          s = [n, n + 1]
          s = (s.first..s.last)
          [r.size, s.size]
        end
        res = []
        40.times { |i| res << f(i) << g(i) }
        res
        "##,
    );
}

#[test]
fn local_object_with_a_rescuing_block() {
    // A block literal with a `rescue` of its own: its handler blocks are
    // not decoded, so the local counts as touched and the object is real
    // before the call.
    run_test_once(
        r##"
        def each3
          yield 1; yield 2; yield 3
        end
        def f(n)
          x = [n, n + 1]
          s = 0
          each3 { |i| begin; s += Integer("#{i}"); rescue ArgumentError; s += 100; end }
          s + x.max
        end
        res = []
        40.times { |i| res << f(i) }
        res
        "##,
    );
}

#[test]
fn local_object_with_a_lambda_or_an_unresolved_call_in_a_block() {
    // A lambda literal inside a block (its body is not reachable from the
    // frame), and a block whose call the compiler cannot resolve (never
    // executed, so its cache is empty): either makes every local count as
    // touched by the block, so the object is real before the call.
    run_test_once(
        r##"
        def each3
          yield 1; yield 2; yield 3
        end
        def g(i) = i
        def lam(n)
          x = [n, n + 1]
          s = 0
          each3 { |i| l = -> { i * 2 }; s += l.call }
          s + x.max
        end
        def unresolved(n)
          x = [n, n + 1]
          s = 0
          each3 { |i| s += i; g(i) if s > 1000 }
          s + x.max
        end
        res = []
        40.times { |i| res << lam(i) << unresolved(i) }
        res
        "##,
    );
}

#[test]
fn receiver_moved_to_a_temporary_while_the_argument_is_computed() {
    // `r.include?(a + 1)`: the receiver is copied into a temporary and the
    // argument computed after it. The object survives the operator (which
    // writes a plain destination) and is consumed by the call.
    run_test_once(
        r##"
        def f(a, b)
          a + b
          r = (a..b)
          x = [a, b]
          n = 0
          n += 1 if r.include?(a + 1)
          n += 2 if x.include?(b - 1)
          [r.include?(a + 1), r.cover?(b - 1), x.include?(a * 2), x.max + (a - 1), n, r.size, x.size]
        end
        res = []
        40.times { |i| res << f(i, i + 3) }
        res
        "##,
    );
}

#[test]
fn local_object_carried_through_a_loop_head() {
    // An object a local holds stays elided across a loop head when the
    // loop neither writes the local nor captures it: its element
    // operations in the body fold every iteration, and nothing is built.
    run_test(
        r#"
        def rng(lo, hi, n)
          r = (lo..hi)
          i = 0
          c = 0
          while i < n
            c += 1 if r.include?(i)
            c += 2 if r.cover?(i + 1)
            c += 4 if r === i - 1
            i += 1
          end
          c + r.size + r.first + r.last
        end
        def ary(a, b, n)
          x = [a, b, 7]
          i = 0
          c = 0
          while i < n
            c += 1 if x.include?(i)
            c += x.size + x.first
            i += 1
          end
          c + x.max
        end
        def nested(lo, hi, n)
          r = (lo..hi)
          i = 0
          c = 0
          while i < n
            j = 0
            while j < 6
              c += 1 if r.include?(i + j)
              j += 1
            end
            i += 1
          end
          c
        end
        def exits(lo, hi, n)
          r = (lo..hi)
          i = 0
          c = 0
          while true
            i += 1
            next if i == 2
            break if i > n
            c += 1 if r.include?(i)
          end
          c + (r.include?(n) ? 10 : 0)
        end
        def rest(*args)
          s = 0
          i = 0
          while i < args.size
            s += args.first + args.last
            i += 1
          end
          s
        end
        res = []
        30.times do |i|
          res << rng(i, i + 5, 12) << ary(i, i + 1, 10) << nested(2, i + 3, 5)
          res << exits(1, i, 8) << rest(i, 2, 3)
        end
        res.last(10)
        "#,
    );
}

#[test]
fn local_object_not_carried_through_a_loop_head() {
    // The loop writes the local (the object is dropped or rebuilt), or
    // reads it in a way that is not folded (an Array index), or a block
    // in the body reads it (a capture point): the object is built on the
    // loop's entries, as before, and the result is unchanged.
    run_test(
        r#"
        def rewritten(lo, n)
          r = (lo..lo + 3)
          i = 0
          c = 0
          while i < n
            c += 1 if r.include?(i)
            r = (i..i + 3) if i % 4 == 0
            i += 1
          end
          c + r.first
        end
        def endpoint_written(lo, n)
          hi = lo + 3
          r = (lo..hi)
          i = 0
          c = 0
          while i < n
            c += 1 if r.include?(i)
            hi += 1
            i += 1
          end
          c + hi + r.last
        end
        def indexed(a, b, n)
          x = [a, b, 9]
          i = 0
          c = 0
          while i < n
            c += x[i % 3]
            c += 1 if x.include?(i)
            i += 1
          end
          c
        end
        def captured(a, b, n)
          x = [a, b]
          i = 0
          c = 0
          while i < n
            [1].each { |k| c += x.max + k }
            c += 1 if x.include?(i)
            i += 1
          end
          c
        end
        def untouched(a, b, n)
          x = [a, b]
          i = 0
          c = 0
          while i < n
            [1].each { |k| c += k }
            c += 1 if x.include?(i)
            i += 1
          end
          c
        end
        def escaped(a, b, n)
          x = [a, b]
          i = 0
          c = 0
          while i < n
            c += 1 if x.include?(i)
            $last = x if i == 3
            i += 1
          end
          c + $last.size
        end
        res = []
        30.times do |i|
          res << rewritten(i, 9) << endpoint_written(i, 6) << indexed(i, 2, 7)
          res << captured(i, 1, 4) << untouched(i, 1, 4) << escaped(i, 4, 6)
        end
        res.last(12)
        "#,
    );
}

#[test]
fn side_exits_inside_a_loop_with_a_local_object_pending() {
    // A type guard deopts in the loop body while the local's object is
    // still elided across the head (the side exit builds it from the
    // scratch window); a basic-op redefinition in a callee called from
    // the body converts the frame by chain.
    run_test_once(
        r#"
        def f(lo, hi, xs)
          r = (lo..hi)
          i = 0
          c = 0
          while i < xs.size
            v = xs[i]
            c += v + 1
            c += 10 if r.include?(i)
            i += 1
          end
          c + r.size
        end
        res = []
        30.times { |i| res << f(i, i + 4, [1, 2, 3, 4, 5, 6]) }
        res << f(1, 3, [1, 2, 3.5, 4, 5, 6])
        res << f(1, 3, [1, 2, "ab".size, 4, 5, 6])
        res
        "#,
    );
    run_test_once(
        r#"
        $redef = proc do
          class Integer
            def +(o) = 1000
          end
        end
        def g(i)
          $redef.call if i == 125
          i
        end
        def f(lo, k)
          r = (lo..lo + 3)
          x = [lo, 7]
          i = 0
          c = 0
          while i < 5
            g(k + i)
            c += 1 if r.include?(i) || x.include?(i)
            i += 1
          end
          c + x.max + r.sum
        end
        res = []
        40.times { |i| res << f(i, i * 5) }
        res.last(20)
        "#,
    );
}

#[test]
fn local_object_passed_in_its_own_slot_and_read_again() {
    // A local's object passed directly as an argument (`get(x)`: the
    // local is the argument slot) is handed over only when the local is
    // dead afterwards; read again, it is materialized for the call.
    run_test(
        r#"
        def get(a) = a[0]
        def sum(a, b) = a.size + b
        def drive(k)
          x = [k, 2, 3]
          s = get(x)
          s += x.max
          y = (k..k + 2)
          t = sum(x, 1) + y.size
          s + t + x.first + y.last
        end
        res = []
        40.times { |i| res << drive(i) }
        res.last(5)
        "#,
    );
}

#[test]
fn array_index_on_elided_literals() {
    // `[]` by an Integer on an elided Array reads the element's slot: a
    // constant index (negative counting from the end, `nil` out of
    // range) at compile time, a Fixnum index in place with the bounds
    // check; a non-Integer index, a slice, and a Bignum build the Array.
    run_test(
        r#"
        def f(a, b, i)
          x = [a, b, a + b]
          [x[0], x[1], x[2], x[3], x[-1], x[-3], x[-4], x[i], x[-i], x[i + 5],
           x[i - 5], x[1..], x[0, 2], x[1.0], x.size]
        end
        def g(a, b, i) = [a, b][i]
        def h(a, i)
          x = [a, 1, 2]
          x[i] + x[i - 1].to_i + x[-i].to_i
        end
        r = []
        30.times { |i| r << f(i, 2, i % 4) << g(i, 3, i % 3) << h(i, i % 3) }
        r.last(8) + [g(1, 2, -1), h(5, 1)]
        "#,
    );
}

#[test]
fn array_index_on_handed_over_objects() {
    // `[]` on an elided Array in a parameter — a `*rest` and a literal
    // passed as an argument — reads the caller's slots through the base,
    // whether the callee is framed or frameless.
    run_test(
        r#"
        def rest(*args)
          args[0].to_i + args[-1].to_i + args[args.size - 1].to_i + args[10].to_i + args[1].to_i
        end
        def take(a, i) = a[i]
        def take2(a) = a[1] + a[-1]
        def framed(a, i)
          yield
          a[i] + a[0]
        end
        r = []
        30.times do |i|
          r << rest(i, 2, 3) << rest << rest(i) << take([i, 2], i % 3) << take2([i, 5, 6])
          r << framed([i, 7], i % 2) { }
        end
        r.last(12)
        "#,
    );
}

#[test]
fn array_index_in_a_loop_keeps_the_local_object() {
    // A loop body reading a local's Array by index does not build it: the
    // index read is a consuming use, so the object is carried through the
    // loop head (§10) and every iteration reads a slot.
    run_test(
        r#"
        def f(a, b, n)
          res = [a, b, a + b]
          i = 0
          s = 0
          while i < n
            s += res[i % 3] + res[0] + res[-1] + res[i - 7].to_i
            i += 1
          end
          s + res.size
        end
        def g(n)
          tbl = [3, 1, 4, 1, 5]
          i = 0
          s = 0
          while i < n
            s += tbl[i % 5] * i
            i += 1
          end
          s
        end
        r = []
        30.times { |i| r << f(i, 2, 10) << g(i) }
        r.last(6) + [f(1.5, 2, 10)]
        "#,
    );
}

#[test]
fn array_index_deopt_on_a_non_fixnum_index() {
    // The in-place read guards the index Fixnum: an index of another
    // class after the compile deopts with the object built by the side
    // exit, and the read is the generic one.
    run_test(
        r#"
        def f(a, i)
          x = [a, a + 1, a + 2]
          x[i]
        end
        r = []
        30.times { |i| r << f(i, i % 3) }
        r << f(1, 1.0) << f(1, nil.to_i) << f(1, -1) << f(1, 1..2)
        r.last(8)
        "#,
    );
}

#[test]
fn range_literal_handed_to_a_non_consuming_callee() {
    // A constant `lo..hi` literal passed where nothing consumes it elided
    // (`Array#[]=`, `Array#[]`, a Hash key) is loaded as the literal
    // itself, the one object every evaluation of the site yields.
    run_test(
        r#"
        def f(q, p)
          q[0..-1] = p
          h = {}
          h[1..3] = q[1..2]
          [q, h, (1..3).equal?(h.keys[0])]
        end
        r = []
        30.times { |i| r << f([0, 1, 2], [i, i + 1, i + 2]) }
        r.last(2)
        "#,
    );
}

#[test]
fn expand_of_an_array_the_method_returns() {
    // The value of a multiple assignment is its right-hand side, so an
    // `ExpandArray` that ends a method reads the Array the method then
    // returns: it may not consume the elided literal (this panicked in
    // `LinkMode::as_return`). The statement form still expands it elided.
    run_test_with_prelude(
        r#"
        r = []
        30.times { |i| r << [f1(i, 2), f2(i, 3), f3(i, 4), f4(i, 5), o.set(i, 6)] }
        r.last(2)
        "#,
        r#"
        def f1(a, b)
          x, y, = a, b
        end
        def f2(a, b)
          @a, @b = [a, b]
        end
        def f3(a, b)
          @a, @b, = a, b
        end
        def f4(a, b)
          x, y, = a, b
          x + y
        end
        class C
          def set(a, b)
            @a, @b, = a, b
          end
        end
        def o = (@o ||= C.new)
        "#,
    );
}
