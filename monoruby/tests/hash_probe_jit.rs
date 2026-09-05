extern crate monoruby;
use monoruby::tests::*;

// `Hash#[]` with the probe emitted as machine code (`AsmInst::HashProbe`):
// for a boxed map and a key that is either one of the bits-hashed
// immediates (Symbol, nil, true, false) or a String of class String
// itself, the JIT probes the map in line — the entries directly for a map
// of at most eight entries, the hashbrown indices table (control groups,
// triangular probe) past that — comparing digests first, then the key
// (its bits, or identity-then-bytes for a String), and reads the value out
// of the entry on a hit. Everything else is the builtin call, as before.
// These pin the answers against CRuby for every shape the probe can meet,
// including the ones it must hand back.

/// The probe's home ground: a boxed (4+ pairs), linear (≤ 8) map with
/// Symbol keys — hits, a miss, and the default value / default proc a miss
/// must still reach through the builtin.
#[test]
fn hash_probe_symbol_keys_linear() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        h = {a: 1, b: 2, c: 3, d: 4, e: 5}
        res << run(h, :a) << run(h, :e) << run(h, :zz)
        hd = Hash.new(:dflt); hd[:a] = 1; hd[:b] = 2; hd[:c] = 3; hd[:d] = 4
        res << run(hd, :a) << run(hd, :nope)
        hp = Hash.new { |hh, kk| "proc:#{kk}" }; hp[:a] = 1; hp[:b] = 2; hp[:c] = 3; hp[:d] = 4
        res << run(hp, :a) << run(hp, :q)
        res
        "##,
    );
}

/// The other bits-hashed immediates as keys.
#[test]
fn hash_probe_nil_true_false_keys() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        hn = {nil => :n, true => :t, false => :f, a: 1}
        [run(hn, nil), run(hn, true), run(hn, false), run(hn, :a), run(hn, :b)]
        "##,
    );
}

/// A deleted entry leaves a tombstone — `None` in the key slot, the zero
/// word — which the scan must step over, never match, and never confuse
/// with its neighbours.
#[test]
fn hash_probe_tombstones() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        ht = {a: 1, b: 2, c: 3, d: 4, e: 5}
        ht.delete(:c)
        res = [run(ht, :c), run(ht, :d), run(ht, :e), run(ht, :a)]
        ht.delete(:a); ht.delete(:e)
        res << run(ht, :a) << run(ht, :b) << run(ht, :d) << run(ht, :e)
        res
        "##,
    );
}

/// Shapes the probe hands to the builtin rather than answering itself —
/// and must hand over as a *call*, never an exit, since a deopt would not
/// recompile and the site would then exit on every lookup:
/// a map past the linear size (indexed), the inline representation
/// (≤ 3 pairs, no digests), and an identity-keyed map.
#[test]
fn hash_probe_hands_other_shapes_to_the_builtin() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        hb = {}; (1..20).each { |i| hb[:"k#{i}"] = i }
        res << run(hb, :k1) << run(hb, :k20) << run(hb, :k99)
        hi = {a: 1, b: 2}
        res << run(hi, :a) << run(hi, :b) << run(hi, :c)
        hc = {a: 1, b: 2, c: 3, d: 4}.compare_by_identity
        res << run(hc, :a) << run(hc, :x)
        res
        "##,
    );
}

/// The key's class is this site's inline-cache assumption and is guarded;
/// a key of another class at the same site must exit cleanly and still
/// answer. And a map that grows across the linear / indexed boundary
/// while the site is hot must keep answering on both sides of it.
#[test]
fn hash_probe_class_guard_and_growth() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        hm = {a: 1, b: 2, c: 3, d: 4, 7 => :seven, "s" => :str}
        res << run(hm, :a) << run(hm, 7) << run(hm, "s") << run(hm, :a)
        hg = {}
        (1..12).each { |i| hg[:"g#{i}"] = i; res << run(hg, :"g#{i}", 40) }
        res << run(hg, :g1) << run(hg, :g12) << run(hg, :none)
        res
        "##,
    );
}

/// A frozen Hash and a Hash mutated between lookups: the probe reads the
/// live entries every time and caches nothing, so an insert, an overwrite
/// and a delete between two lookups at the same site are all observed.
#[test]
fn hash_probe_reads_live_state() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        h = {a: 1, b: 2, c: 3, d: 4}
        res = [run(h, :a)]
        h[:a] = 10;    res << run(h, :a)
        h[:e] = 5;     res << run(h, :e)
        h.delete(:b);  res << run(h, :b) << run(h, :c)
        f = {a: 1, b: 2, c: 3, d: 4}.freeze
        res << run(f, :a) << run(f, :zz)
        res
        "##,
    );
}

/// String keys, both regimes: a linear map and an 18-entry indexed one (the
/// erubi shape). A hit through identity (the frozen literal is the stored
/// object) and through bytes (a fresh String of the same content), a miss
/// answered in line, and the default value / default proc a miss must still
/// reach through the builtin.
#[test]
fn hash_probe_string_keys() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        hs = {"a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5}
        res << run(hs, "a") << run(hs, "e") << run(hs, "zz") << run(hs, "a".dup)
        hb = {}
        18.times { |i| hb["key#{i}"] = i }
        res << run(hb, "key0") << run(hb, "key17") << run(hb, "key9") << run(hb, "nope")
        fk = "key3".freeze
        hb[fk] = :frozen
        res << run(hb, fk) << run(hb, "key3") << run(hb, "key3".dup)
        hd = Hash.new(:d); 12.times { |i| hd["d#{i}"] = i }
        res << run(hd, "d3") << run(hd, "x")
        hp = Hash.new { |h, k| "p:#{k}" }; 12.times { |i| hp["p#{i}"] = i }
        res << run(hp, "p3") << run(hp, "x")
        hl = Hash.new(:d); hl["a"] = 1; hl["b"] = 2; hl["c"] = 3; hl["d"] = 4
        res << run(hl, "a") << run(hl, "x")
        res
        "##,
    );
}

/// The indexed regime for the bits-compared keys, including a table big
/// enough that the probe sequence must step past full control groups, and
/// entries deleted after the table was built.
#[test]
fn hash_probe_indexed_symbol_keys() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        hsym = {}; 100.times { |i| hsym[:"s#{i}"] = i }
        res << run(hsym, :s0) << run(hsym, :s99) << run(hsym, :s50) << run(hsym, :none)
        hsym.delete(:s50); hsym.delete(:s0)
        res << run(hsym, :s50) << run(hsym, :s0) << run(hsym, :s51) << run(hsym, :s99)
        hmix = {nil => :n, true => :t, false => :f}; 20.times { |i| hmix[:"m#{i}"] = i }
        res << run(hmix, nil) << run(hmix, true) << run(hmix, false) << run(hmix, :m7)
        big = {}; 3000.times { |i| big[:"b#{i}"] = i }
        hit = 0; miss = 0
        3000.times { |i| hit += 1 if run(big, :"b#{i}", 2) == i; miss += 1 if run(big, :"nb#{i}", 2).nil? }
        res << hit << miss
        res
        "##,
    );
}

/// String keys in the indexed regime with tombstones (the entries are not
/// compacted on delete; the indices table no longer reaches the dead
/// entry), and a table walked past full groups.
#[test]
fn hash_probe_indexed_string_keys_tombstones() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        ht = {}; 20.times { |i| ht["t#{i}"] = i }
        ht.delete("t5"); ht.delete("t19"); ht.delete("t0")
        res << run(ht, "t5") << run(ht, "t19") << run(ht, "t0") << run(ht, "t6") << run(ht, "t18")
        ht["t5"] = :back
        res << run(ht, "t5")
        big = {}; 3000.times { |i| big["b#{i}"] = i }
        hit = 0; miss = 0
        3000.times { |i| hit += 1 if run(big, "b#{i}", 2) == i; miss += 1 if run(big, "nb#{i}", 2).nil? }
        res << hit << miss
        res
        "##,
    );
}

/// What a String key must *not* be probed as: a String subclass key (its
/// `eql?` decides, and the site's class guard keeps a subclass instance off
/// the byte comparison), and an identity-keyed map, whose digests are of
/// the key's identity — a content digest would not find the stored object
/// itself. Both hand over to the builtin.
#[test]
fn hash_probe_string_keys_that_are_not_bytes_compared() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        class Sub < String; def eql?(o) = false; def hash = "q".hash; end
        hq = {}; hq[Sub.new("q")] = 1; hq["r"] = 2; hq["s"] = 3; hq["t"] = 4
        res << run(hq, "q") << run(hq, Sub.new("q")) << run(hq, "r")
        hc = {}.compare_by_identity
        k = "x"; hc[k] = 1; hc["y"] = 2; hc["z"] = 3; hc["w"] = 4; hc["v"] = 5
        res << run(hc, k) << run(hc, "x")
        big = {}.compare_by_identity
        keys = (0..20).map { |i| "b#{i}" }
        keys.each_with_index { |kk, i| big[kk] = i }
        res << run(big, keys[3]) << run(big, "b3")
        res
        "##,
    );
}

/// A String-keyed map growing across the linear / indexed boundary while
/// the site is hot, and a site that sees String keys first and Symbol keys
/// after (the class guard must exit cleanly in that order too).
#[test]
fn hash_probe_string_growth_and_key_class_change() {
    run_test(
        r##"
        def run(h, k, n = 300)
          r = nil; i = 0
          while i < n; r = h[k]; i += 1; end
          r
        end
        res = []
        hg = {}
        (1..12).each { |i| hg["g#{i}"] = i; res << run(hg, "g#{i}", 40) }
        res << run(hg, "g1") << run(hg, "g12") << run(hg, "none")
        hm = {"s" => :str, :a => 1, 7 => :seven, "t" => :u, "v" => :w}
        res << run(hm, "s") << run(hm, :a) << run(hm, 7) << run(hm, "t") << run(hm, :a)
        res
        "##,
    );
}
