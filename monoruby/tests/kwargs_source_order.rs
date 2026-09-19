extern crate monoruby;
use monoruby::tests::*;

// A call site's keywords are applied in the order they were written
// (#1407). Literal `k: v` pairs and `**hash` splats live in two separate
// containers at a call site, and reading one after the other made the
// first container win a duplicated key — so the `f(**defaults, key:
// override)` idiom silently dropped the override — and put the keys of a
// `**kwrest` hash in the wrong order.

/// The issue's own tables: a key on both sides, and the key order a
/// `**kwrest` sees for distinct keys.
#[test]
fn a_duplicated_key_takes_its_last_value() {
    run_test_once(
        r##"
        def ks(**kw) = kw
        def dec(a:) = a
        def deck(a:, **kw) = [a, kw]
        h1 = {x: 1}
        h2 = {z: 3}
        res = []
        # A key written on both sides: the later one wins.
        res << ks(**{a: 1}, a: 2)
        res << dec(**{a: 1}, a: 2)
        res << ks(a: 2, **{a: 1})
        res << dec(a: 2, **{a: 1})
        res << ks(**{a: 1}, **{a: 9})
        res << deck(**{a: 1, b: 9}, a: 2)
        res << deck(a: 2, **{a: 1, b: 9})
        # A literal key repeated across a splat: still the last one.
        res << ks(a: 1, **{a: 9}, a: 2)
        res << ks(a: 1, **{a: 9}, a: 2, **{a: 8})
        res << dec(a: 1, **{a: 9}, a: 2)
        # Distinct keys: the hash keeps source order.
        res << ks(b: 2, **{a: 1})
        res << ks(**{a: 1}, b: 2)
        res << ks(**{a: 1}, b: 2, **{c: 3})
        res << ks(a: 1, **{b: 2}, c: 3)
        res << ks(**h1, y: 2, **h2)
        # A key written twice with no splat at the site is folded at
        # compile time, so it takes the *later* pair's place; with a
        # splat anywhere the merge happens at run time and the key keeps
        # the first place it was given.
        res << ks(a: 1, b: 2, a: 3)
        res << ks(a: 1, b: 2, a: 3, **{})
        res << ks(a: 1, b: 2, **{}, a: 3)
        res << ks(a: 1, b: 2, **{c: 3}, a: 4)
        res << ks(**{c: 9}, a: 1, b: 2, a: 3)
        res << ks(b: 2, a: 1, **{a: 3})
        # Non-Symbol keys ride along in their place.
        res << ks(**{"s" => 3}, b: 2)
        res << ks(**{a: 1}, b: 2, "s" => 3)
        # `**nil` and `**{}` contribute nothing.
        res << ks(**{a: 1}, **{})
        res << ks(**{a: 1}, **nil)
        res
        "##,
    );
}

/// The keywords a callee with no keyword parameters receives as a
/// trailing Hash, and the ones `method_missing` is handed, are built
/// from the same sources and order.
#[test]
fn a_trailing_hash_and_method_missing_see_the_same_order() {
    run_test_once(
        r##"
        def pos(h) = h
        class M
          def method_missing(name, *a, **kw) = [name, a, kw]
        end
        h = {a: 1}
        res = []
        res << pos(**h, b: 2)
        res << pos(b: 2, **h)
        res << pos(a: 1, **{b: 2}, c: 3)
        res << pos(**{a: 1}, a: 2)
        res << M.new.foo(**h, b: 2)
        res << M.new.foo(1, **h, b: 2, **{c: 3})
        res << M.new.foo(**{a: 1}, a: 2)
        res << M.new.foo(a: 1, b: 2, a: 3)
        res << M.new.foo(a: 1, b: 2, a: 3, **{})
        res << pos(a: 1, b: 2, a: 3)
        res << pos(a: 1, b: 2, a: 3, **{})
        # A Proc / block callee binds them the same way.
        pr = ->(**kw) { kw }
        bl = proc { |**kw| kw }
        res << pr.call(**h, a: 2)
        res << bl.call(**h, b: 2)
        res << pr.(**{a: 1}, b: 2, **{c: 3})
        # `super` passes them on unchanged.
        cls = Class.new do
          def m(**kw) = kw
        end
        sub = Class.new(cls) do
          def m(**kw) = super(**{a: 1}, b: 2, **kw)
        end
        res << sub.new.m
        res << sub.new.m(a: 9)
        res
        "##,
    );
}

/// Keyword operands are *evaluated* left to right, splats included, and
/// an unknown-keyword error names the keys in that same order.
#[test]
fn keyword_operands_are_evaluated_and_reported_left_to_right() {
    run_test_once(
        r##"
        def ks(**kw) = kw
        def one(x:) = x
        $ord = []
        def side(n, h = {}) = ($ord << n; h)
        res = []
        ks(**side(1, {a: 1}), b: (($ord << 2); 9), **side(3, {c: 3}))
        res << $ord
        $ord = []
        ks(a: (($ord << 1); 9), **side(2))
        res << $ord
        $ord = []
        ks(**side(1), **side(2), c: (($ord << 3); 0))
        res << $ord
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res << t.() { one(a: 1, **{b: 2}, x: 0) }
        res << t.() { one(**{b: 2}, a: 1, x: 0) }
        res << t.() { one(**{b: 2, c: 3}, x: 0) }
        res << t.() { one(**{"s" => 1}, a: 2, x: 0) }
        # An unknown keyword is named once however often it was written.
        res << t.() { one(a: 1, a: 2, x: 0) }
        res << t.() { one(a: 1, a: 2, **{}, x: 0) }
        res << t.() { one(a: 1, b: 2, **{}, a: 3, x: 0) }
        res << t.() { one(a: 1, b: 2, a: 3, x: 0) }
        res << t.() { one(**{a: 1, b: 2}, a: 3, x: 0) }
        res
        "##,
    );
}

/// The same shapes once the JIT has compiled the caller, and through a
/// `def f(...)` forwarding trampoline.
#[test]
fn source_order_survives_jit_compilation_and_forwarding() {
    run_test(
        r##"
        def ks(**kw) = kw
        def dec(a:) = a
        def deck(a:, **kw) = [a, kw]
        def pos(h) = h
        def fwd(...) = ks(...)
        h = {a: 1}
        res = []
        res << ks(**h, a: 2)
        res << dec(**h, a: 2)
        res << deck(**{a: 1, b: 9}, a: 2)
        res << ks(**h, b: 2, **{c: 3})
        res << pos(**h, b: 2)
        res << fwd(**h, b: 2)
        res << fwd(a: 1, a: 2)
        res << ks(a: 1, a: 2)
        res << ks(a: 1, b: 2, a: 3)
        res << ks(a: 1, b: 2, a: 3, **{})
        res << deck(a: 1, b: 2, a: 3)
        res << ks(a: 1, **{a: 9}, a: 2)
        res
        "##,
    );
}
