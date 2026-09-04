extern crate monoruby;
use monoruby::tests::*;

// `Hash#[]` with the probe emitted as machine code (`AsmInst::HashProbePacked`):
// for a boxed map of at most eight entries and a key whose digest is its
// bits through the mixer (Symbol, nil, true, false), the JIT scans the
// entries in line — digest first, then the key's bits — and reads the
// value out of the entry on a hit. Everything else is the builtin call, as
// before. These pin the answers against CRuby for every shape the probe
// can meet, including the ones it must hand back.

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
