extern crate monoruby;
use monoruby::tests::*;

// String keys in a boxed (eql?-keyed) Hash probe through the vm-free
// prehashed fast path (`string_digest` / `string_key_eq` in
// rvalue/hash.rs): a byte-content digest plus identity-then-byte
// equality, exercised here against CRuby for every probe shape —
// lookup, insert-overwrite, membership, and the slow-path operations
// (delete, iteration) that must observe the same buckets.

#[test]
fn string_key_basic_roundtrip() {
    run_test(
        r##"
        h = {}
        h["alpha"] = 1
        h["beta"] = 2
        h["alpha" + ""] = 3
        [h, h["alpha"], h["beta"], h["gamma"], h.key?("beta"), h.key?("nope"), h.size]
        "##,
    );
}

#[test]
fn string_key_mixed_with_other_types() {
    // Packed keys keep their own prehashed path; a String probe must
    // never match a packed or non-String heap key, and vice versa.
    run_test(
        r##"
        m = { 1 => :one, "one" => :str, :one => :sym, 1.5 => :flt, [1] => :ary, "1" => :dig }
        [m["one"], m[1], m[:one], m[1.5], m[[1]], m["1"], m.key?("one"), m.key?("two"), m.size]
        "##,
    );
}

#[test]
fn string_key_subclass_and_frozen() {
    run_test(
        r##"
        class MyStr < String; end
        h = { "alpha" => 1 }
        s = MyStr.new("alpha")
        h[s] = 9
        h["beta".freeze] = 42
        [h["alpha"], h[s], h["beta"], h.size, h.keys.map(&:class).map(&:to_s)]
        "##,
    );
}

#[test]
fn string_key_indexed_map() {
    // Past the linear (ar_table) bound: the probe goes through the
    // index table with the caller-computed digest.
    run_test(
        r##"
        h = {}
        200.times { |i| h["key#{i}"] = i }
        r = [h["key0"], h["key123"], h["key199"], h["key200"], h.size]
        100.times { |i| h["key#{i}"] = -i }
        r << h["key50"] << h["key150"] << h.size
        r << h.key?("key77") << h.key?("key777")
        r
        "##,
    );
}

#[test]
fn string_key_delete_and_tombstones() {
    // Deletes leave tombstones only while an iteration is live; a
    // String probe must skip dead (None) entries either way.
    run_test(
        r##"
        h = {}
        20.times { |i| h["k#{i}"] = i }
        h.delete("k3")
        r = [h["k3"], h.key?("k3"), h.size]
        h.each_key { |k| h.delete(k) if k == "k7" || k == "k11" }
        r << h["k7"] << h["k11"] << h["k12"] << h.size
        h["k7"] = :again
        r << h["k7"] << h.size
        r
        "##,
    );
}

#[test]
fn string_key_mutation_and_rehash() {
    // Mutating a live key strands its insert-time bucket — Hash#rehash
    // restores it. The prehashed probe computes the digest from the
    // probe's current bytes exactly like the general path.
    run_test(
        r##"
        k = "mut"
        h = { k => 1, "other" => 2 }
        k << "ated"
        r = [h["mutated"], h["mut"]]
        h.rehash
        r << h["mutated"] << h.size
        r
        "##,
    );
}

#[test]
fn string_key_encodings() {
    // eql? for strings is byte equality; 7-bit content in different
    // (comparable) encodings is the same key.
    run_test(
        r##"
        a = "abc"
        b = "abc".dup.force_encoding("ASCII-8BIT")
        h = { a => 1 }
        r = [h[b], h.key?(b)]
        u = "こんにちは"
        h[u] = :utf8
        r << h["こんにちは"] << h.size
        r
        "##,
    );
}

#[test]
fn string_key_compare_by_identity() {
    // compare_by_identity keys by object id — the byte-content fast
    // path must not apply there.
    run_test(
        r##"
        h = {}.compare_by_identity
        a = "dup"
        h[a] = 1
        h["dup"] = 2
        [h.size, h[a], h.keys.map(&:to_s).sort]
        "##,
    );
}

#[test]
fn string_key_default_and_fetch() {
    run_test(
        r##"
        h = Hash.new { |hash, k| hash[k] = "made-#{k}" }
        h["x"] = 1
        [h["x"], h["y"], h.fetch("x"), h.fetch("z", :dflt), h.size]
        "##,
    );
}
