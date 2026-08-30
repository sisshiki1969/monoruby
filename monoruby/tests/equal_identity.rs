//! Container equality starts from identity, as CRuby's `rb_ary_equal`,
//! `rb_hash_equal`, `rb_struct_equal` and `Set#==` all do: an object
//! equals itself before anything inside it is looked at, and a value
//! equals itself before its `==` is asked.
//!
//! Not only a shortcut. Without it a Hash holding a `NaN` — or anything
//! whose `==` answers false — was not equal to itself.

extern crate monoruby;
use monoruby::tests::*;

/// Every container answers `true` for itself, whatever it holds.
#[test]
fn a_container_equals_itself() {
    run_test(
        r##"
        require "set"
        class Never
          def ==(o) = false
          def eql?(o) = false
        end
        n = Never.new
        nan = Float::NAN
        ary = [nan, n]
        hash = { a: nan, b: n }
        set = Set.new([1, 2])
        st = Struct.new(:x, :y).new(nan, n)
        [ary == ary, ary.eql?(ary), ary === ary,
         hash == hash, hash.eql?(hash), hash === hash,
         set == set, st == st, st.eql?(st),
         [] == [], {} == {}, Set.new == Set.new]
        "##,
    );
}

/// A value equals itself inside two *different* containers too — the
/// per-element step, which Array had and Hash did not.
#[test]
fn a_value_equals_itself_across_containers() {
    run_test(
        r##"
        class Never
          def ==(o) = false
          def eql?(o) = false
        end
        n = Never.new
        nan = Float::NAN
        [[nan] == [nan], [n] == [n],
         { x: nan } == { x: nan }, { x: n } == { x: n },
         { x: nan }.eql?({ x: nan }), { x: n }.eql?({ x: n }),
         [[nan]] == [[nan]], { a: { b: nan } } == { a: { b: nan } }]
        "##,
    );
}

/// Everything the identity step must *not* swallow: containers that
/// really differ, and the class / flag mismatches each `==` checks.
#[test]
fn unequal_containers_stay_unequal() {
    run_test(
        r##"
        require "set"
        S1 = Struct.new(:x)
        S2 = Struct.new(:x)
        h = { a: 1 }
        [[1, 2] == [1, 3], [1] == [1, 2], [1] == 1, [1] == nil,
         { a: 1 } == { a: 2 }, { a: 1 } == { b: 1 }, { a: 1 } == { a: 1, b: 2 },
         { a: 1 } == 1, { a: 1 } == nil,
         h == h.dup, h.dup == h.dup,
         h == {}.compare_by_identity.merge(a: 1),
         Set.new([1]) == Set.new([1]), Set.new([1]) == Set.new([2]),
         Set.new([1]) == [1],
         S1.new(1) == S1.new(1), S1.new(1) == S2.new(1), S1.new(1) == S1.new(2),
         [1].eql?([1.0]), ({ a: 1 }).eql?({ a: 1.0 })]
        "##,
    );
}

/// A container that holds itself: the identity step must not hide the
/// recursion guard that was already there.
#[test]
fn self_referential_containers() {
    run_test(
        r##"
        a = []
        a << a
        b = []
        b << b
        h = {}
        h[:x] = h
        g = {}
        g[:x] = g
        [a == a, a == b, a == a.dup, a.eql?(a),
         h == h, h == g, h.eql?(h), a.inspect, h.inspect]
        "##,
    );
}
