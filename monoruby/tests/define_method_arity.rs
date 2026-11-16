extern crate monoruby;
use monoruby::tests::*;

// A method made by `define_method` is entered at its body, which is still
// a block, but binds its arguments as a method: strict arity, no spreading
// of a lone Array over several parameters. The VM calls it through the
// entry (`FuncKind::Proc`), which is method-style; compiled code called
// the body directly and bound the arguments by the body's rules, so once a
// caller was compiled, surplus arguments were dropped, missing ones became
// `nil` and `o.n([1, 2])` bound `a = 1, b = 2` (#1961). Each case below is
// called often enough to be compiled, and has to answer the same from the
// first call to the last.

#[test]
fn define_method_arity_in_compiled_callers() {
    run_test_once(
        r##"
        class DmArity
          define_method(:m) { |a, **kw| [a, kw] }
          define_method(:n) { |a, b = :b| [a, b] }
          define_method(:q) { |a| a }
          define_method(:r) { |a, b| [a, b] }
          define_method(:s) { |a, *r| [a, r] }
          define_method(:z) { 1 }
          define_method(:k) { |a, k: 1| [a, k] }
          define_method(:l, &lambda { |a| a })
          define_method(:pr, &proc { |a, b| [a, b] })
        end
        o = DmArity.new
        def dm_g1(o) = o.m(1, 2)
        def dm_g2(o) = o.n(1, 2, 3)
        def dm_g3(o) = o.q(1, 2)
        def dm_g4(o) = o.q
        def dm_g5(o) = o.r([1, 2])
        def dm_g6(o) = o.n([1, 2])
        def dm_g7(o) = o.s
        def dm_g8(o) = o.z(1)
        def dm_g9(o) = o.k(1, z: 2)
        def dm_g10(o) = o.k(1, 2)
        def dm_g11(o) = o.l(1, 2)
        def dm_g12(o) = o.pr([1, 2])
        def dm_g13(o) = o.pr(1)
        def dm_g14(o, a) = o.r(*a)
        def dm_g15(o, a) = o.q(*a)
        def dm_g16(o) = o.q(1) { }
        def dm_g17(o) = o.q(1, 2) { }
        def dm_ok1(o) = o.n(1)
        def dm_ok2(o) = o.r(1, 2)
        def dm_ok3(o) = o.k(1, k: 3)
        def dm_ok4(o) = o.s(1, 2, 3)
        def dm_t = (yield rescue :"#{$!.class}")
        r = []
        60.times do
          r << [
            dm_t { dm_g1(o) }, dm_t { dm_g2(o) }, dm_t { dm_g3(o) }, dm_t { dm_g4(o) },
            dm_t { dm_g5(o) }, dm_t { dm_g6(o) }, dm_t { dm_g7(o) }, dm_t { dm_g8(o) },
            dm_t { dm_g9(o) }, dm_t { dm_g10(o) }, dm_t { dm_g11(o) }, dm_t { dm_g12(o) },
            dm_t { dm_g13(o) }, dm_t { dm_g14(o, [[1, 2]]) }, dm_t { dm_g14(o, [1]) },
            dm_t { dm_g15(o, [1, 2]) }, dm_t { dm_g16(o) }, dm_t { dm_g17(o) },
            dm_t { dm_ok1(o) }, dm_t { dm_ok2(o) }, dm_t { dm_ok3(o) }, dm_t { dm_ok4(o) },
          ]
        end
        r.tally.to_a
        "##,
    );
}

// The same through a forward: `...` and `*r, **k, &b` hand the callee a
// rest Array and a keyword Hash, bound by the forwarding fast paths.
#[test]
fn define_method_arity_through_forwarding() {
    run_test_once(
        r##"
        class DmFwd
          define_method(:m) { |a, **kw| [a, kw] }
          define_method(:n) { |a, b = :b| [a, b] }
          define_method(:r) { |a, b| [a, b] }
        end
        o = DmFwd.new
        def dm_fw(o, ...) = o.m(...)
        def dm_fn(o, ...) = o.n(...)
        def dm_fr(o, ...) = o.r(...)
        def dm_gw(o, *r, **k, &b) = o.n(*r, **k, &b)
        def dm_t = (yield rescue :"#{$!.class}")
        r = []
        60.times do
          r << [
            dm_t { dm_fw(o, 1, 2) }, dm_t { dm_fw(o, 1, x: 2) }, dm_t { dm_fw(o) },
            dm_t { dm_fn(o, :lead, 1, k: 7) }, dm_t { dm_fn(o) }, dm_t { dm_fn(o, [1, 2]) },
            dm_t { dm_fn(o, 1, 2) }, dm_t { dm_fr(o, [1, 2]) }, dm_t { dm_fr(o, 1, 2) },
            dm_t { dm_gw(o, :lead, 1, k: 7) }, dm_t { dm_gw(o, 1, 2, 3) }, dm_t { dm_gw(o, 1) },
            dm_t { dm_gw(o) },
          ]
        end
        r.tally.to_a
        "##,
    );
}

// A site that sees several receiver classes dispatches on the class and
// calls each `define_method` body from its own arm.
#[test]
fn define_method_arity_polymorphic_site() {
    run_test_once(
        r##"
        class DmPoly
          define_method(:q) { |a| a }
          define_method(:n) { |a, b = :b| [a, b] }
        end
        class DmPoly2 < DmPoly; end
        class DmPoly3 < DmPoly
          define_method(:q) { |a| -a }
          define_method(:n) { |a, b = :c| [b, a] }
        end
        os = [DmPoly.new, DmPoly2.new, DmPoly3.new]
        def dm_p1(o) = o.q(1, 2)
        def dm_p2(o) = o.q
        def dm_p3(o) = o.n([1, 2])
        def dm_t = (yield rescue :"#{$!.class}")
        r = []
        120.times do |i|
          x = os[i % 3]
          r << [dm_t { dm_p1(x) }, dm_t { dm_p2(x) }, dm_t { x.q(5) }, dm_t { dm_p3(x) }]
        end
        r.tally.to_a
        "##,
    );
}

// `main.define_method` is `Module#define_method` on Object (CRuby's
// `top_define_method`): the same strict method, public. It used to keep the
// block's loose binding — and a compiled `o.pr([1, 2])` then took the
// in-line Array spread written for `yield`, which fills the parameters
// but not `self`, so the body ran with whatever the stack held there.
#[test]
fn toplevel_define_method_is_a_method() {
    run_test_once(
        r##"
        define_method(:dm_top_q) { |a| a }
        define_method(:dm_top_l, &lambda { |a| a })
        define_method(:dm_top_pr, &proc { |a, b| [a, b, self.class] })
        define_method(:dm_top_ret) { |a| return a * 2; 0 }
        o = Object.new
        def dm_tg1(o) = o.dm_top_q
        def dm_tg2(o) = o.dm_top_l(1, 2)
        def dm_tg3(o) = o.dm_top_pr([1, 2])
        def dm_tg4(o) = o.dm_top_pr(1, 2)
        def dm_tg5(o) = o.dm_top_ret(4)
        def dm_t = (yield rescue :"#{$!.class}")
        r = []
        60.times do
          r << [dm_t { dm_tg1(o) }, dm_t { dm_tg2(o) }, dm_t { dm_tg3(o) }, dm_t { dm_tg4(o) }, dm_t { dm_tg5(o) }]
        end
        [
          r.tally.to_a,
          Object.public_method_defined?(:dm_top_q),
          o.method(:dm_top_q).arity,
          o.method(:dm_top_pr).arity,
          (define_method(:dm_top_none) rescue $!.class),
        ]
        "##,
    );
}
