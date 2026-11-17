//! Refinements across an inlining boundary (#1973).
//!
//! A refinement binds lexically: the method a call site resolves to is a
//! function of where the site was *written*, not of where its body ends
//! up running. The JIT moves bodies around — a callee is compiled into
//! its caller's unit, a small one without a frame of its own, a block
//! into the Ruby-written `Array#each` that yields to it — so each inlined
//! frame resolves under its own iseq's set, and a helper called from a
//! frameless body is told whose scope it answers for.

extern crate monoruby;
use monoruby::tests::*;

/// The issue's script: methods written before `using` see no refinement,
/// wherever they are called from, once the calling block is compiled.
#[test]
fn callee_outside_using_inlined_into_refined_scope() {
    run_test_once(
        r##"
        class Foo1973; def foo = :plain; end
        module R1973
          refine Integer do
            def +(o) = :refined_plus
            def <(o) = :refined_lt
            def !=(o) = :refined_ne
          end
          refine Foo1973 do
            def foo = :refined_foo
          end
        end
        def out_plus1973(x, y) = x + y
        def out_lt1973(x, y) = x < y
        def out_ne1973(x, y) = x != y
        def out_foo1973(x) = x.foo
        using R1973
        f = Foo1973.new
        res = []
        40.times do
          res << [out_plus1973(1, 2), out_lt1973(1, 2), out_ne1973(1, 2), out_foo1973(f)]
        end
        res.uniq
        "##,
    );
}

/// The other direction: methods written under `using`, called from a
/// compiled loop that activated nothing, keep their refinements.
#[test]
fn callee_under_using_inlined_into_plain_scope() {
    run_test_once(
        r##"
        class FooRev1973; def foo = :plain; def ==(o) = :plain_eq; end
        module RRev1973
          refine Integer do
            def +(o) = :refined_plus
            def <(o) = :refined_lt
            def !=(o) = :refined_ne
            def ==(o) = :refined_eq
          end
          refine FooRev1973 do
            def foo = :refined_foo
            def ==(o) = :refined_foo_eq
          end
        end
        module InRev1973
          using RRev1973
          def self.plus(x, y) = x + y
          def self.lt(x, y) = x < y
          def self.ne(x, y) = x != y
          def self.eq(x, y) = x == y
          def self.foo(x) = x.foo
        end
        f = FooRev1973.new
        res = []
        40.times do
          m = InRev1973
          res << [m.plus(1, 2), m.lt(1, 2), m.ne(1, 2), m.eq(1, 2), m.foo(f), m.eq(f, f), m.ne(f, f)]
        end
        res.uniq
        "##,
    );
}

/// Every kind of site, on both sides of a `using`, called from a method
/// that activated nothing and from a module body that did: operators,
/// indexing, unary minus, interpolation, ordinary calls, `super` from a
/// refined method, `send` / `respond_to?`, `&:sym`, blocks, `yield`, and
/// calls that cross the boundary twice.
#[test]
fn every_site_resolves_where_it_was_written() {
    run_test_once(
        r##"
        class FooAll1973
          def foo = :plain
          def bar = :plain_bar
          def to_s = "plain_to_s"
          attr_accessor :v
        end
        class SubAll1973 < FooAll1973; end
        module RAll1973
          refine Integer do
            def +(o) = :r_plus
            def -(o) = :r_minus
            def *(o) = :r_mul
            def <(o) = :r_lt
            def ==(o) = :r_eq
            def !=(o) = :r_ne
            def [](i) = :r_idx
            def -@ = :r_neg
            def to_s = "r_to_s"
            def succ = :r_succ
            def zero? = :r_zero
          end
          refine Float do
            def +(o) = :r_fplus
            def <(o) = :r_flt
          end
          refine Array do
            def [](i) = :r_aidx
            def size = :r_size
            def <<(o) = :r_shl
            def first = :r_first
          end
          refine String do
            def +(o) = :r_splus
            def ==(o) = :r_seq
            def size = :r_ssize
            def to_sym = :r_to_sym
          end
          refine Symbol do
            def to_proc = proc { |x| :r_to_proc }
            def ==(o) = :r_symeq
          end
          refine NilClass do
            def ==(o) = :r_nileq
            def nil? = :r_nilp
          end
          refine FooAll1973 do
            def foo = :r_foo
            def bar = [:r_bar, super]
            def to_s = "r_foo_to_s"
            def v = :r_v
          end
          refine Hash do
            def [](k) = :r_hidx
          end
        end

        module OutAll1973
          def self.plus(x, y) = x + y
          def self.minus(x, y) = x - y
          def self.mul(x, y) = x * y
          def self.lt(x, y) = x < y
          def self.eq(x, y) = x == y
          def self.ne(x, y) = x != y
          def self.idx(x, y) = x[y]
          def self.neg(x) = -x
          def self.interp(x) = "<#{x}>"
          def self.succ(x) = x.succ
          def self.zero(x) = x.zero?
          def self.size(x) = x.size
          def self.shl(x, y) = x << y
          def self.first(x) = x.first
          def self.to_sym(x) = x.to_sym
          def self.amp(x) = x.map(&:foo)
          def self.nilp(x) = x.nil?
          def self.foo(x) = x.foo
          def self.bar(x) = x.bar
          def self.v(x) = x.v
          def self.send_(x) = x.send(:foo)
          def self.resp(x) = x.respond_to?(:foo)
          def self.blk(x) = [x].map { |e| e.foo }
          def self.blk2(x, y) = [x].each { |e| return e + y }
          def self.yielder(x) = yield(x)
          def self.nested(x, y) = plus(x, y)
          def self.nested2(x) = foo(x)
          def self.ival(o) = o.instance_eval { foo }
          def self.loop_(n) = (i = 0; s = 0; while i < n; s += i; i += 1; end; s)
        end

        module InAll1973
          using RAll1973
          def self.plus(x, y) = x + y
          def self.minus(x, y) = x - y
          def self.mul(x, y) = x * y
          def self.lt(x, y) = x < y
          def self.eq(x, y) = x == y
          def self.ne(x, y) = x != y
          def self.idx(x, y) = x[y]
          def self.neg(x) = -x
          def self.interp(x) = "<#{x}>"
          def self.succ(x) = x.succ
          def self.zero(x) = x.zero?
          def self.size(x) = x.size
          def self.shl(x, y) = x << y
          def self.first(x) = x.first
          def self.to_sym(x) = x.to_sym
          def self.amp(x) = x.map(&:foo)
          def self.nilp(x) = x.nil?
          def self.foo(x) = x.foo
          def self.bar(x) = x.bar
          def self.v(x) = x.v
          def self.send_(x) = x.send(:foo)
          def self.resp(x) = x.respond_to?(:foo)
          def self.blk(x) = [x].map { |e| e.foo }
          def self.blk2(x, y) = [x].each { |e| return e + y }
          def self.yielder(x) = yield(x)
          def self.nested(x, y) = OutAll1973.plus(x, y)
          def self.nested2(x) = OutAll1973.foo(x)
          def self.nested3(x, y) = OutAll1973.yielder(x) { |e| e + y }
          def self.nested4(x, y) = OutAll1973.yielder(x) { |e| OutAll1973.plus(e, y) }
          def self.ival(o) = o.instance_eval { foo }
          def self.loop_(n) = (i = 0; s = 0; while i < n; s += i; i += 1; end; s) rescue :err
        end

        module AllSites1973
          def self.all(m, f)
            [m.plus(1, 2), m.plus(1.5, 2.5), m.plus("a", "b"), m.minus(3, 1), m.mul(2, 3),
             m.lt(1, 2), m.lt(1.5, 2.0), m.eq(1, 1), m.eq("a", "a"), m.eq(:a, :a),
             m.eq(nil, nil), m.eq(f, f), m.ne(1, 1), m.ne(f, f), m.ne(nil, 1),
             m.idx([1, 2], 0), m.idx(5, 0), m.idx({a: 1}, :a), m.neg(1), m.interp(1),
             m.interp(f), m.succ(1), m.zero(0), m.size([1]), m.size("ab"), m.shl([], 1),
             m.first([1]), m.to_sym("a"), m.amp([f]), m.nilp(nil), m.foo(f), m.bar(f),
             m.v(f), m.send_(f), m.resp(f), m.blk(f), m.blk2(1, 2), m.yielder(1) { |e| e + 1 },
             m.nested(1, 2), m.nested2(f), m.ival(f), m.loop_(3)]
          end

          # A scope that activated nothing.
          def self.plain
            f = SubAll1973.new
            res = []
            50.times do
              r = [all(OutAll1973, f), all(InAll1973, f),
                   InAll1973.nested3(1, 2), InAll1973.nested4(1, 2)]
              res.push(r) unless res.include?(r)
            end
            res
          end
        end

        # A scope that activated the refinements: the block below is
        # written under them, so only its own sites are refined.
        module Refined1973
          using RAll1973
          F = SubAll1973.new
          RES = {}
          50.times do
            r = [AllSites1973.all(OutAll1973, F), AllSites1973.all(InAll1973, F),
                 InAll1973.nested3(1, 2), InAll1973.nested4(1, 2),
                 OutAll1973.yielder(1) { |e| e + 1 }, InAll1973.yielder(1) { |e| e + 1 }]
            RES.store(r.inspect, r)
          end
        end

        [AllSites1973.plain, Refined1973::RES.values]
        "##,
    );
}

/// `==` / `!=` on a site that sees several classes is a helper call, and
/// in a body compiled without a frame the helper has no frame of the
/// body's to ask which refinements are active. It answers for the scope
/// the body was written in, on either side of a `using`.
#[test]
fn polymorphic_eq_in_frameless_callee_across_using() {
    run_test_once(
        r##"
        class FooPoly1973; end
        class BarPoly1973; end
        module RPoly1973
          refine FooPoly1973 do
            def ==(o) = :refined_eq
            def !=(o) = :refined_ne
          end
        end
        def out_eq_poly1973(x, y) = x == y
        def out_ne_poly1973(x, y) = x != y
        module InPoly1973
          using RPoly1973
          def self.eq(x, y) = x == y
          def self.ne(x, y) = x != y
        end
        module Poly1973
          def self.round(vals, i)
            v = vals[i % vals.size]
            [v.class, InPoly1973.eq(v, v), InPoly1973.ne(v, v),
             out_eq_poly1973(v, v), out_ne_poly1973(v, v)]
          end

          def self.plain
            vals = [nil, :a, 1, "s", FooPoly1973.new, BarPoly1973.new, 1.5]
            res = []
            70.times do |i|
              v = vals[i % vals.size]
              r = [v.class, InPoly1973.eq(v, v), InPoly1973.ne(v, v),
                   out_eq_poly1973(v, v), out_ne_poly1973(v, v)]
              res.push(r) unless res.include?(r)
            end
            res
          end

          def self.plain_nested
            vals = [nil, :a, 1, "s", FooPoly1973.new, BarPoly1973.new, 1.5]
            res = []
            70.times do |i|
              r = round(vals, i)
              res.push(r) unless res.include?(r)
            end
            res
          end
        end
        module RefinedPoly1973
          using RPoly1973
          VALS = [nil, :a, 1, "s", FooPoly1973.new, BarPoly1973.new, 1.5]
          RES = {}
          70.times do |i|
            v = VALS[i % VALS.size]
            r = [v.class, InPoly1973.eq(v, v), InPoly1973.ne(v, v),
                 out_eq_poly1973(v, v), out_ne_poly1973(v, v)]
            RES.store(r.inspect, r)
          end
        end
        [Poly1973.plain, Poly1973.plain_nested, RefinedPoly1973::RES.values]
        "##,
    );
}

/// The shape that went wrong: a method written under `using`, compiled
/// frameless into a block of a method that activated nothing. The helper
/// asked the block's frame and found `BasicObject#==`.
#[test]
fn polymorphic_eq_under_using_in_frameless_callee_of_plain_block() {
    run_test_once(
        r##"
        class FooWarm1973; end
        class BarWarm1973; end
        module RWarm1973
          refine FooWarm1973 do
            def ==(o) = :refined_eq
            def !=(o) = :refined_ne
          end
        end
        def out_eq_warm1973(x, y) = x == y
        def out_ne_warm1973(x, y) = x != y
        module InWarm1973
          using RWarm1973
          def self.eq(x, y) = x == y
          def self.ne(x, y) = x != y
        end
        def warm1973
          f = FooWarm1973.new
          vals = [nil, :a, 1, "s", f, BarWarm1973.new, 1.5]
          res = []
          120.times do |i|
            v = vals[i % vals.size]
            res << [v.class, InWarm1973.eq(v, v), InWarm1973.ne(v, v),
                    out_eq_warm1973(v, v), out_ne_warm1973(v, v)]
          end
          res.uniq
        end
        warm1973
        "##,
    );
}

/// The same helper inside a block the Ruby-written core yields to
/// (`Array#each`, `Integer#times`) and inside a frameless callee: the
/// core stands in for CRuby's C and sees no redefinition of `==`, the
/// user's block compiled into it does.
#[test]
fn polymorphic_eq_in_block_run_inside_the_core_sees_a_redefinition() {
    run_test_once(
        r##"
        class Core1973
          attr_reader :bad
          def initialize = @bad = nil
          def reset = @bad = nil
          def m(vals, w)
            vals.each { |x| @bad = x if (x == w) == true }
          end
          def t(a, b)
            3.times { @bad = a if (a == b) == true }
          end
          def eq(a, b) = a == b
          def u(a, b)
            @bad = a if eq(a, b) == true
          end
        end
        c = Core1973.new
        vals = [nil, :a, 1, 2, :b, nil, :c]
        300.times do
          c.m(vals, 5); c.t(:a, nil); c.t(nil, 1); c.t(1, :a)
          c.u(nil, 1); c.u(:a, 1); c.u(1, :a)
        end
        class String
          def ==(o) = :redef_str_eq
        end
        class Float
          def ==(o) = :redef_flo_eq
        end
        res = []
        c.reset; c.m(["a"], "a"); res << c.bad
        c.reset; c.m([1.5], 1.5); res << c.bad
        c.reset; c.t("a", "a"); res << c.bad
        c.reset; c.t(1.5, 1.5); res << c.bad
        c.reset; c.u("a", "a"); res << c.bad
        c.reset; c.u(1.5, 1.5); res << c.bad
        res
        "##,
    );
}

/// A block written under `using` and run inside the core keeps its
/// refinements; the same block written outside has none.
#[test]
fn polymorphic_eq_in_block_run_inside_the_core_under_using() {
    run_test_once(
        r##"
        class FooBlk1973; end
        class BarBlk1973; end
        module RBlk1973
          refine FooBlk1973 do
            def ==(o) = :r_eq
            def !=(o) = :r_ne
          end
          refine Integer do
            def ==(o) = :r_ieq
          end
        end
        class InBlk1973
          using RBlk1973
          attr_reader :got
          def initialize = @got = []
          def m(vals, w) = vals.each { |x| @r = (x == w); @got << @r unless @got.include?(@r) }
          def n(vals, w) = vals.each { |x| @r = (x != w); @got << @r unless @got.include?(@r) }
          def t(a, b) = 2.times { @r = (a == b); @got << @r unless @got.include?(@r) }
        end
        class OutBlk1973
          attr_reader :got
          def initialize = @got = []
          def m(vals, w) = vals.each { |x| @r = (x == w); @got << @r unless @got.include?(@r) }
          def n(vals, w) = vals.each { |x| @r = (x != w); @got << @r unless @got.include?(@r) }
          def t(a, b) = 2.times { @r = (a == b); @got << @r unless @got.include?(@r) }
        end
        f = FooBlk1973.new
        vals = [nil, :a, BarBlk1973.new, f, "s", 1.5, 2]
        res = []
        [InBlk1973, OutBlk1973].each do |k|
          c = k.new
          200.times { c.m(vals, f); c.t(f, f); c.t(nil, f); c.t(:a, f) }
          res << c.got.map(&:to_s)
          c = k.new
          200.times { c.n(vals, f) }
          res << c.got.map(&:to_s)
        end
        res
        "##,
    );
}

/// A private or protected `BasicObject#==` at such a site is not the
/// helper's to decide without a frame: the protected check reads `self`.
#[test]
fn polymorphic_eq_in_block_run_inside_the_core_honours_visibility() {
    run_test_once(
        r##"
        class Priv1973; private :==; end
        class Prot1973
          protected :==
          def cmp(vals, w) = vals.each { |x| @r = ((x == w) rescue $!.class) }
          def r = @r
        end
        class Vis1973
          attr_reader :got
          def initialize = @got = []
          def m(vals, w)
            vals.each do |x|
              @r = ((x == w) rescue $!.class)
              @got << @r unless @got.include?(@r)
            end
          end
        end
        v = Vis1973.new
        plain = [nil, :a, Object.new, 1]
        100.times { v.m(plain, 1) }
        pr = Priv1973.new
        pt = Prot1973.new
        v.m([pr, pt, nil], pr)
        v.m([pt], pt)
        100.times { pt.cmp(plain, 1) }
        pt.cmp([pt], pt)
        [v.got, pt.r]
        "##,
    );
}
