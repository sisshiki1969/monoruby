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

// Small Hash literals with frozen String keys stay in the inline
// representation (see `is_inline_key` in rvalue/hash.rs); the inline
// scan compares String keys by byte content, so every probe shape that
// the boxed path handles must behave identically before and after the
// literal is promoted.

#[test]
fn inline_string_key_literal_lookup() {
    run_test(
        r##"
        h = {"content-type" => "text/plain", "b" => 2}
        k = "content" + "-type"
        r = [h["content-type"], h[k], h[k.dup], h["zz"], h.key?("b"), h.keys.map(&:frozen?), h.size]
        h["c"] = 3
        r << [h["c"], h.size, h.keys]
        h["d"] = 4
        r << [h["d"], h["content-type"], h.size, h.keys]
        r
        "##,
    );
}

#[test]
fn inline_string_key_duplicate_and_identity() {
    run_test(
        r##"
        d = {"a" => 1, "a" + "" => 2}
        i = {"x" => 1}.compare_by_identity
        m = +"mut"
        mh = {}
        mh[m] = 1
        m << "!"
        [d, d.size, i["x"], i[i.keys[0]], i.compare_by_identity?,
         mh["mut"], mh["mut!"], mh.keys[0].frozen?, mh.keys[0].equal?(m)]
        "##,
    );
}

#[test]
fn inline_string_key_equality_with_boxed() {
    run_test(
        r##"
        big = {"a" => 1}
        %w[b c d e].each { |x| big[x] = 1 }
        %w[b c d e].each { |x| big.delete(x) }
        [{"a" => 1} == {"a" => 1}, {"a" => 1}.eql?({"a" => 1}), {"a" => 1}.hash == {"a" => 1}.hash,
         big == {"a" => 1}, {"a" => 1} == big, big.hash == {"a" => 1}.hash]
        "##,
    );
}

#[test]
fn inline_string_key_encoding_and_mixed() {
    run_test(
        r##"
        bin = "a".b
        e = {"k" => 1, :k => 2, 3 => 4}
        s = {"k" => 1}
        r = [{"a" => 1}[bin], {bin => 1}["a"], {"あ" => 1}["あ".encode("EUC-JP")],
             e["k"], e[:k], e[3], e.to_a, s.delete("k"), s, s.delete("k")]
        begin
          e.each { |kk, v| e["new"] = 1 }
        rescue => ex
          r << ex.class
        end
        r << (e.rehash == e)
        r
        "##,
    );
}

#[test]
fn inline_string_key_ignores_redefined_string_hash() {
    // Hash keys use the built-in byte comparison even when String#hash /
    // String#eql? are redefined.
    run_test_once(
        r##"
        class String; def hash = 0; def eql?(o) = true; end
        [{"p" => 1}["q"], {"p" => 1}.key?("q"), {"p" => 1}["p"]]
        "##,
    );
}

/// A String **subclass** key is compared by `eql?`, not by bytes.
///
/// CRuby's `rb_any_cmp` gates its String short-circuit on
/// `RBASIC(a)->klass == rb_cString` for both operands, so a subclass falls
/// through to `rb_eql` and a redefined `eql?` decides. Its `any_hash` has
/// no such gate — a subclass still hashes by content — so the two keys
/// land in the same bucket and the redefinition is what turns the lookup
/// into a miss, rather than the bucket choice.
///
/// Both halves are checked here, and in both representations: `OnlyEql`
/// misses (`eql?` said no) while `OnlyHash` and `Plain` hit (the bytes
/// chose the bucket, and the default `eql?` agreed) — which is also why
/// `h["q"]` finds the subclass-keyed entry in every case.
#[test]
fn string_subclass_key_dispatches_eql() {
    run_test_once(
        r##"
        class OnlyHash < String; def hash; object_id; end; end
        class OnlyEql  < String; def eql?(o); false; end; end
        class Plain    < String; end
        class Both     < String; def hash; 1; end; def eql?(o); true; end; end
        res = []
        [OnlyHash, OnlyEql, Plain, Both].each do |c|
          # 1 pair exercises the inline representation, 18 the boxed map
          [1, 18].each do |n|
            h = {}
            (1...n).each { |i| h["pad#{i}"] = i }
            h[c.new("q")] = :hit
            res << [c.to_s, n, h[c.new("q")], h["q"], h.key?(c.new("q")), h.size]
          end
        end
        # the other direction: a plain String key, probed with a subclass
        [1, 18].each do |n|
          h = {}
          (1...n).each { |i| h["pad#{i}"] = i }
          h["q"] = :plain
          res << ["stored-plain", n, h[Plain.new("q")], h[OnlyEql.new("q")]]
        end
        h = {"q" => 1}
        res << ["delete", h.delete(Plain.new("q")), h.size]
        res
        "##,
    );
}

/// Ruby key identity is hash equality AND `eql?`, in that order — `eql?`
/// may only decide between keys whose hashes already match. A subclass
/// with an over-broad `eql?` (always true) and its own `#hash` must not
/// match a plain-String entry in either representation: the boxed map's
/// indexed probe used to consult `eql?` on a mere control-byte (7-bit)
/// hash collision, matching a stranger's entry on ~1/70 hash seeds
/// (`string_subclass_key_dispatches_eql`'s flake), and the inline scan
/// consulted `eql?` without hashing at all, matching deterministically.
#[test]
fn eql_needs_a_full_hash_match_first() {
    run_test_once(
        r##"
        class Sticky < String
          def hash; 1; end
          def eql?(o); true; end
        end
        res = []
        # Inline representation (single pair, then a few).
        h1 = { "pad1" => 1 }
        res << h1[Sticky.new("q")] << h1.key?(Sticky.new("q"))
        h3 = { "pad1" => 1, "pad2" => 2, "pad3" => 3 }
        res << h3[Sticky.new("q")]
        # Boxed representation.
        hb = {}
        (1...18).each { |i| hb["pad#{i}"] = i }
        res << hb[Sticky.new("q")] << hb.key?(Sticky.new("q"))
        # The subclass still finds itself: same #hash, eql? true.
        hb[Sticky.new("s")] = :self
        res << hb[Sticky.new("t")] << hb.size
        # And deleting through the over-broad eql? only takes the
        # hash-matching entry with it.
        res << hb.delete(Sticky.new("u")) << hb.size
        res << hb.delete("pad9") << hb.size
        res
        "##,
    );
}

// A Hash literal that is *not* constant (some value is an expression)
// is built pair by pair, and its String literal keys are emitted as
// the interned frozen String — the same object the constant-literal
// template holds — instead of a mutable literal that the insert then
// dups and freezes again (`push_hash_key` in bytecodegen/expression.rs).
// CRuby compiles such keys as frozen fstrings, so identity, frozenness
// and lookup behaviour are compared against it for every shape: the
// short path, the chunked path (> 256 pairs) and the `**splat` path.

#[test]
fn dynamic_literal_string_keys_are_frozen_and_shared() {
    run_test(
        r##"
        x = 1
        res = []
        a = { "alpha" => x, "beta" => x + 1 }
        b = { "alpha" => x * 3 }
        res << a.keys.map(&:frozen?) << a.keys[0].equal?(b.keys[0])
        # The same literal evaluated twice hands out the same key object.
        ks = 2.times.map { |i| { "k" => i }.keys[0] }
        res << ks[0].equal?(ks[1]) << ks[0].frozen?
        # Lookup with a fresh (mutable, unfrozen) probe still works, and a
        # frozen literal key from a constant literal is the same object.
        res << a["alpha" + ""] << a.key?("beta".dup) << a.keys[1].equal?({ "beta" => 2 }.keys[1])
        # Mutating a key through `keys` raises, as it does in CRuby.
        begin
          a.keys[0] << "!"
        rescue => e
          res << e.class
        end
        res << a
        res
        "##,
    );
}

#[test]
fn dynamic_literal_string_keys_chunked_and_splat() {
    run_test_once(
        r##"
        x = 7
        # Chunked construction (> 256 pairs), one dynamic value forces the
        # pair-by-pair path.
        src = (0...300).map { |i| "\"k#{i}\" => #{i == 150 ? 'x' : i}" }.join(", ")
        h = eval("{ #{src} }")
        res = [h.size, h["k150"], h["k299"], h.keys.all?(&:frozen?), h.keys[0].equal?({ "k0" => x }.keys[0])]
        # `**splat` interleaved with String keys.
        extra = { "mid" => :m, "a" => :over }
        s = { "a" => x, **extra, "z" => x + 1 }
        res << s << s.keys.map(&:frozen?) << s.keys[0].equal?({ "a" => 0 }.keys[0])
        res
        "##,
    );
}

#[test]
fn dynamic_literal_string_keys_encodings_and_escapes() {
    run_test_once(
        r##"
        x = 1
        h = { "café" => x, "\xff\xfe".b => x + 1, "tab\tnl\n" => x + 2, 'single' => x + 3 }
        res = [h.keys.map { |k| k.encoding.name }, h.keys.map(&:frozen?), h["café"], h["\xff\xfe".b], h["tab\tnl\n"], h["single"]]
        res << h.keys[0].equal?({ "café" => 0 }.keys[0])
        # A non-literal BINARY key is dup'd and frozen on insert like any
        # other String key (literal form and `[]=` alike).
        k = "\xff\xfe".b
        g = { k => 1 }
        g["\xfe".b + "\xff".b] = 2
        res << g.keys.map(&:frozen?) << k.frozen? << g.keys[0].equal?(k) << g[k]
        res
        "##,
    );
}

#[test]
fn dynamic_literal_string_keys_ignore_redefined_string_hash() {
    // `String#hash` / `eql?` are never consulted for a String literal
    // key, before and after this change alike (a frozen literal is
    // still a plain String).
    run_test_once(
        r##"
        class String
          def hash; 42; end
          def eql?(o); false; end
        end
        x = 1
        h = { "a" => x, "b" => x + 1, "c" => x + 2, "d" => x + 3, "e" => x + 4 }
        [h["a"], h["e"], h["a".dup], h.key?("c"), h.size, h.keys.map(&:frozen?)]
        "##,
    );
}
