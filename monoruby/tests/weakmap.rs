extern crate monoruby;
use monoruby::tests::*;

// `ObjectSpace::WeakMap` and the `WeakRef` on top of it. The storage is
// `ObjTy::WEAKMAP` (src/value/rvalue/weakmap.rs), which the collector
// never traces and whose pairs it breaks between the mark and the
// sweep. Every case is compared against CRuby, which is the only way to
// pin "weak" — a strong map passes every test that does not collect.

/// The map API, with nothing collected yet.
#[test]
fn weakmap_basics() {
    run_test_once(
        r##"
        m = ObjectSpace::WeakMap.new
        k1, k2, v1, v2 = Object.new, Object.new, "one", "two"
        res = [m.size, m[k1]]
        # The assignment answers nil, not the value.
        res << m.[]=(k1, v1)
        m[k2] = v2
        res << [m.size, m.length, m[k1], m[k2]]
        res << [m.key?(k1), m.include?(k1), m.member?(k1), m.key?(Object.new)]
        # Storing under a key already present replaces its value rather
        # than adding a pair.
        m[k1] = "one again"
        res << [m[k1], m.size]
        # Keys are compared by identity, never by ==/eql?: two equal
        # but distinct strings are two pairs.
        a, b = "same", "same"
        m[a] = "A"
        m[b] = "B"
        res << [m[a], m[b], m.size]
        res << m.delete(k1)
        res << [m.delete(k1), m.size, m.key?(k1)]
        res << m.keys.size
        res << m.values.sort_by { |v| v.to_s }
        n = 0
        m.each { |k, v| n += 1 }
        m.each_pair { |k, v| n += 1 }
        m.each_key { |k| n += 1 }
        m.each_value { |v| n += 1 }
        res << n
        res << m.class.name
        res
        "##,
    );
}

/// The point of the thing: a pair whose key or whose value is collected
/// is gone from the map.
#[test]
fn weakmap_drops_collected_pairs() {
    run_test_once(
        r##"
        m = ObjectSpace::WeakMap.new
        kept_k, kept_v = Object.new, Object.new
        m[kept_k] = kept_v
        # `hold` keeps every half alive while the pairs are counted, so
        # `before` says how many pairs went in rather than how soon the
        # collector got to them — under gc-stress it runs at every
        # allocation, and the garbage would already be gone by here.
        hold = []
        # Both halves garbage.
        def both(m, hold) = 200.times { k, v = Object.new, Object.new; hold << k << v; m[k] = v }
        # Only the value garbage; the key is held.
        keys = []
        def values_only(m, keys, hold) = 200.times { k = Object.new; keys << k; v = Object.new; hold << v; m[k] = v }
        # Only the key garbage; the value is held.
        vals = []
        def keys_only(m, vals, hold) = 200.times { v = Object.new; vals << v; k = Object.new; hold << k; m[k] = v }
        both(m, hold)
        values_only(m, keys, hold)
        keys_only(m, vals, hold)
        before = m.size
        hold.clear
        3.times { GC.start }
        [before, m.size, m[kept_k].equal?(kept_v), keys.size, vals.size]
        "##,
    );
}

/// An immediate is accepted and reads back, but has no cell for the
/// collector to watch: it is simply always live, so a pair with an
/// immediate half lives exactly as long as its *other* half does. The
/// values here are therefore held, which is what makes the pairs
/// outlive a collection — leaving them to the map alone would only be
/// testing how soon the collector runs.
#[test]
fn weakmap_immediate_keys_and_values() {
    run_test_once(
        r##"
        m = ObjectSpace::WeakMap.new
        imm = [1, :sym, nil, true, false, 2.5]
        strs = imm.map { |v| "keyed by #{v.inspect}" }
        imm.each_with_index { |v, i| m[v] = strs[i] }
        held = Object.new
        m[held] = 42
        res = [imm.map { |v| m[v] }, m[held], m.size]
        3.times { GC.start }
        res << imm.map { |v| m[v] }
        res << [m[held], m.key?(held), m.size]
        res
        "##,
    );
}

/// `WeakRef` delegates while its referent lives and raises once it does
/// not. This is CRuby's own weakref.rb running on the map above.
#[test]
fn weakref_is_weak() {
    run_test_once(
        r##"
        require "weakref"
        res = []
        strong = "a string"
        r = WeakRef.new(strong)
        res << [r.weakref_alive?, r.upcase, r.length, r.is_a?(String)]
        # Referents that are collected.
        refs = []
        def mk(refs) = 200.times { refs << WeakRef.new(Object.new) }
        mk(refs)
        3.times { GC.start }
        res << refs.count { |x| x.weakref_alive? }
        res << (begin
                  refs.first.to_s
                rescue WeakRef::RefError => e
                  [e.class.name, e.message]
                end)
        # The one still referenced is untouched.
        res << [r.weakref_alive?, r.upcase, strong]
        res
        "##,
    );
}

/// The collector must not keep a weak map's halves alive, but it must
/// keep the map itself usable across collections — including one that
/// happens while the map is being filled.
#[test]
fn weakmap_survives_collection_while_filling() {
    run_test_once(
        r##"
        m = ObjectSpace::WeakMap.new
        kept = []
        500.times do |i|
          k = Object.new
          kept << k if i % 100 == 0
          m[k] = "value #{i}"
          GC.start if i % 150 == 0
        end
        GC.start
        # Only the keys still held can still be found, and their values
        # are strings the map itself is not keeping alive — so the
        # surviving count is what CRuby's is.
        [kept.size, kept.count { |k| m.key?(k) }, m.size <= 500]
        "##,
    );
}

/// A weak map that is itself collected takes its pairs with it, and the
/// collector forgets it — the registry of live maps must not outlive
/// the cells it points at.
#[test]
fn weakmap_itself_can_be_collected() {
    run_test_once(
        r##"
        kept = ObjectSpace::WeakMap.new
        # Both halves held, so the pair's survival turns on the map
        # rather than on when its key or value is collected.
        anchor, value = Object.new, Object.new
        kept[anchor] = value
        # Hundreds of maps, each with pairs, all dropped.
        def churn = 300.times { m = ObjectSpace::WeakMap.new; m[Object.new] = Object.new; nil }
        churn
        3.times { GC.start }
        # The surviving map is untouched and still usable.
        held_key, held_value = Object.new, Object.new
        kept[held_key] = held_value
        [kept[anchor].equal?(value), kept[held_key].equal?(held_value), kept.size, kept.class.name]
        "##,
    );
}
