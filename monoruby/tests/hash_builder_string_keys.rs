//! Every method that builds a Hash stores an unfrozen String key as a
//! frozen copy, as `Hash#[]=` does (#2023).
//!
//! CRuby funnels them through `rb_hash_aset`: `Array#to_h`, `Hash[]`,
//! `group_by`, `invert`, `merge`, Marshal, and Set, whose table is a
//! Hash. `tally` is the exception (`rb_enum_tally_up` stores with
//! `st_update`, and keeps the key it was given), and so is a
//! `compare_by_identity` Hash, which a copy would defeat.

use monoruby::tests::*;

const HELPER: &str = r####"
def t; yield.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
def k(h) = (key = h.keys[0]; [key.frozen?, key.equal?($s)])
$s = +"str"
require "set"
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// `[frozen?, same object?]` of the key each builder stored.
#[test]
fn hash_builders() {
    probe(&[
        r####"k([[$s, 1]].to_h)"####,
        r####"k(Hash[[[$s, 1]]])"####,
        r####"k(Hash[$s, 1])"####,
        r####"k([$s].group_by { |x| x })"####,
        r####"k({1 => $s}.invert)"####,
        r####"k([$s].zip([1]).to_h)"####,
        r####"k([$s].to_h { |x| [x, 1] })"####,
        r####"k([$s].each_with_index.to_h)"####,
        r####"k({}.merge!({}.compare_by_identity.tap { |h| h[$s] = 1 }))"####,
        r####"k({}.merge({}.compare_by_identity.tap { |h| h[$s] = 1 }))"####,
        r####"k({}.update({}.compare_by_identity.tap { |h| h[$s] = 1 }) { |*| 1 })"####,
        r####"k([$s].tally)"####,
        r####"k([$s].each.tally)"####,
        r####"k([$s].tally_by { |x| x })"####,
        r####"k([$s].each_with_object({}) { |x, h| h[x] = 1 })"####,
        r####"k({}.replace({}.compare_by_identity.tap { |h| h[$s] = 1 }))"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.to_h)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.dup)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.select { true })"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.to_a.to_h)"####,
        r####"k([$s].chunk_while { true }.to_a.group_by { |x| x[0] })"####,
        r####"k([$s].each_slice(1).to_h { |x| [x[0], 1] })"####,
        r####"k({a: 1}.to_h { |a, b| [$s, 1] })"####,
        r####"k({a: 1}.transform_keys { $s })"####,
        r####"k({a: 1}.transform_keys(a: $s))"####,
        r####"h = {a: 1}; h.transform_keys! { $s }; k(h)"####,
        r####"k({a: 1}.map { [$s, 1] }.to_h)"####,
        r####"k([$s].partition { true }[0].group_by(&:itself))"####,
        r####"k([[$s, 1]].each_entry.to_h)"####,
        r####"k(Hash.new.tap { |h| h.store($s, 1) })"####,
        r####"k([$s].index_by { |x| x } ) rescue "n/a""####,
        r####"k({1 => $s}.each_with_object({}) { |(a, b), h| h[b] = a })"####,
    ]);
}

/// Through Enumerator, Lazy, Range, Struct and identity Hashes.
#[test]
fn more_hash_builders() {
    probe(&[
        r####"k(ENV.to_h.class == Hash ? {$s => 1} : nil)"####,
        r####"k([$s].to_set.group_by { |x| x })"####,
        r####"k([$s].uniq.to_h { [_1, 1] })"####,
        r####"k([$s].min_by(1) { 1 }.group_by { _1 })"####,
        r####"k([[$s, 1]].each_with_index.map { |a, i| a }.to_h)"####,
        r####"k(Struct.new(:a).new($s).each_pair.to_h { |a, b| [b, a] })"####,
        r####"k({1 => $s}.filter_map { |a, b| [b, a] }.to_h)"####,
        r####"k(Hash[{}.compare_by_identity.tap { |h| h[$s] = 1 }])"####,
        r####"k([$s].each_with_index.group_by { |x, i| x })"####,
        r####"k([$s].lazy.group_by { |x| x })"####,
        r####"k([$s].tally({}))"####,
        r####"k([$s].each_entry.tally)"####,
        r####"k([$s].lazy.tally)"####,
        r####"k([$s].partition { true }.then { |a, b| a.zip([1]).to_h })"####,
        r####"k((1..1).to_h { [$s, 1] })"####,
        r####"k((1..1).group_by { $s })"####,
        r####"k({a: 1}.group_by { $s })"####,
        r####"k([[$s, 1]].each.to_h)"####,
        r####"k([$s].each.to_h { [_1, 1] })"####,
        r####"k([$s].each_with_index.to_h { [_1, _2] })"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.merge({}))"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.reject { false })"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.filter_map { |a, b| [a, b] }.to_h)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.transform_values { 1 })"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.slice($s))"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.except(1))"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.compact)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.invert.invert)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.sort.to_h)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.min_by(1) { 1 }.to_h)"####,
        r####"k({}.compare_by_identity.tap { |h| h[$s] = 1 }.each_with_object({}) { |(a, b), h| h.update(a => b) })"####,
    ]);
}

/// A Set freezes a copy of a String element the same way.
#[test]
fn set_elements() {
    probe(&[
        r####"e = Set[$s].to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set.new([$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set.new.add($s).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set.new << $s).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = [$s].to_set.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set.new | [$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set.new.merge([$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"s = Set.new; s.add?($s); e = s.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set.new.compare_by_identity.add($s).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set[1] ^ [$s]).to_a[-1]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[1].map! { $s }.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set[] + [$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[1].collect! { $s }.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].dup.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[[$s]].flatten.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[1].replace([$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].select { true }.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].group_by { 1 }[1].to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].classify { 1 }[1].to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].partition { true }[0][0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set[$s] & [$s]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[1].tap { |s| s.add($s) }.to_a[1]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[[$s]].flatten!.to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = (Set[$s, 1] - [1]).to_a[0]; [e.frozen?, e.equal?($s)]"####,
        r####"e = Set[$s].divide { 1 }.to_a[0].to_a[0]; [e.frozen?, e.equal?($s)]"####,
    ]);
}

/// The stored key is a copy: changing the String afterwards does not
/// reach the Hash.
#[test]
fn mutating_the_original_key() {
    probe(&[r####"s = +"k"; h = [[s, 1]].to_h; s << "x"; [h, h["k"], h["kx"]]"####]);
}

/// `hash_aset_str`: the frozen copy is made for a new key only. A store
/// to a key the Hash already has keeps the stored key object and leaves
/// the String it was given alone, whatever the representation (inline
/// pairs, the linear table, the indexed one), and is the one store an
/// iteration allows.
#[test]
fn an_existing_key_is_kept() {
    run_test(
        r####"
        r = []
        [1, 3, 4, 9, 40].each do |n|
          src = (0...n).map { |i| "k#{i}" }
          h = {}
          src.each { |s| h[s] = 1 }
          r << [n, h.keys.all?(&:frozen?), src.none?(&:frozen?), h.keys.zip(src).none? { |a, b| a.equal?(b) }]
          keys = h.keys
          again = (0...n).map { |i| "k#{i}" }
          again.each { |s| h[s] = 2 }
          r << [h.keys.zip(keys).all? { |a, b| a.equal?(b) }, again.none?(&:frozen?), h.values.uniq, h.size]
          src.each { |s| s << "x" }
          r << [h["k0"], h.key?("k0x"), h.keys.first]
          h.each { |k, _| h[k.dup] = 3 }
          r << h.values.uniq
          r << (begin; h.each { h["new#{n}".dup] = 1 }; :no; rescue => e; [e.class, e.message]; end)
          r << [h.size, h.key?("new#{n}")]
          h.store("k0".dup, 9)
          r << [h["k0"], h.keys.zip(keys).all? { |a, b| a.equal?(b) }]
        end
        r
        "####,
    );
}

/// The same store from compiled code (`Hash#[]=` inlined into a method
/// and a loop): new keys are copied, repeated ones are not.
#[test]
fn an_existing_key_is_kept_compiled() {
    run_test(
        r####"
        def put(h, k, v) = h[k] = v
        def count(words)
          c = Hash.new(0)
          words.each { |w| c[w] += 1 }
          c
        end
        h = {}
        srcs = []
        300.times { |i| s = "k#{i % 7}"; srcs << s; put(h, s, i) }
        keys = h.keys
        300.times { |i| put(h, "k#{i % 7}", -i) }
        words = (0...300).map { |i| "w#{i % 11}" }
        c = count(words)
        [h, h.keys.all?(&:frozen?), srcs.none?(&:frozen?),
         h.keys.zip(keys).all? { |a, b| a.equal?(b) },
         h.keys.none? { |k| srcs.any? { |s| s.equal?(k) } },
         c, c.keys.all?(&:frozen?), words.none?(&:frozen?)]
        "####,
    );
}

/// Which key object a store keeps, by the kind of String: a frozen one
/// is stored as it is, an identity Hash keeps every String it is given,
/// a String with a singleton class or instance variables is copied
/// without them, an instance of a subclass is stored as it is, and two
/// Hashes share the copy of a bare String (its fstring).
#[test]
fn key_kinds() {
    run_test(
        r####"
        r = []
        fk = "frz".dup.freeze
        h = {}
        h[fk] = 1
        r << h.keys[0].equal?(fk)
        h["frz".dup] = 2
        r << [h.keys[0].equal?(fk), h[fk]]
        ih = {}.compare_by_identity
        s = "id".dup
        ih[s] = 1
        ih[s] = 2
        t = "id".dup
        ih[t] = 3
        r << [ih.size, ih.keys[0].equal?(s), ih.keys[0].frozen?, ih.keys[1].equal?(t)]
        sc = "sing".dup
        def sc.foo = 1
        h = {}
        h[sc] = 1
        r << [h.keys[0].frozen?, h.keys[0].equal?(sc), h.keys[0].respond_to?(:foo), sc.frozen?]
        h[sc] = 2
        r << [h.size, h["sing"]]
        iv = "ivar".dup
        iv.instance_variable_set(:@a, 1)
        h[iv] = 1
        h[iv] = 5
        r << [h.keys[1].frozen?, h.keys[1].equal?(iv), h.keys[1].instance_variable_get(:@a), h.size, h["ivar"]]
        class MyS2023 < String; end
        ms = MyS2023.new("sub")
        h[ms] = 1
        h[MyS2023.new("sub")] = 2
        h["sub"] = 3
        r << [h.keys[2].equal?(ms), h.keys[2].frozen?, h.keys[2].class, h.size, h.values.last(2)]
        a = {}; b = {}
        a["shared-key".dup] = 1
        b["shared-key".dup] = 1
        r << [a.keys[0].equal?(b.keys[0]), a.keys[0].equal?(-"shared-key".dup)]
        c = Hash.new { |hh, k| hh[k] = [] }
        %w[a b a].map(&:dup).each_with_index { |w, i| c[w] << i }
        r << [c, c.keys.all?(&:frozen?)]
        bk = "\xff\xfe".b
        bh = {}; bh[bk] = 1; bh["\xff\xfe".b] = 2
        r << [bh.size, bh.keys[0].frozen?, bh.keys[0].encoding.name, bh.values]
        eh = {}; eh["é".dup] = 1; eh["é".encode("ISO-8859-1")] = 2; eh["é".dup] = 3
        r << [eh.size, eh.values, eh.keys.map { |k| k.encoding.name }]
        r
        "####,
    );
}

/// The builders with a key given more than once: one frozen key per
/// content, the Strings they were given untouched.
#[test]
fn builders_with_repeated_keys() {
    run_test(
        r####"
        require "set"
        r = []
        src = %w[x y x z y x].map(&:dup)
        prs = src.each_with_index.to_a
        th = prs.to_h
        r << [th, th.keys.all?(&:frozen?), src.none?(&:frozen?), th.keys[0].equal?(src[0])]
        th2 = prs.to_h { |k, v| [k, v * 2] }
        r << [th2, th2.keys.all?(&:frozen?)]
        g = src.group_by { |x| x }
        r << [g.keys, g.keys.all?(&:frozen?), g.values.map(&:size)]
        hh = Hash[prs]
        r << [hh, hh.keys.all?(&:frozen?)]
        hh2 = Hash[*prs.flatten]
        r << [hh2, hh2.keys.all?(&:frozen?)]
        hh3 = Hash[{}.compare_by_identity.tap { |q| src.each_with_index { |k, i| q[k] = i } }]
        r << [hh3, hh3.keys.all?(&:frozen?), hh3.compare_by_identity?]
        inv = { 1 => "p".dup, 2 => "q".dup, 3 => "p".dup }.invert
        r << [inv, inv.keys.all?(&:frozen?)]
        m = { "a".dup => 1 }.merge({ "a" => 2, "b" => 3 }) { |k, x, y| x + y }
        r << [m, m.keys.all?(&:frozen?)]
        st = Set.new(src)
        r << [st.to_a, st.to_a.all?(&:frozen?), st.size]
        st2 = Set.new; src.each { |x| st2 << x; st2.add(x.dup) }
        r << [st2.to_a, st2.size]
        k1 = "dyn".dup
        lit = { k1 => 1, "dyn".dup => 2, "#{k1}2" => 3, k1 => 4 }
        r << [lit, lit.keys.all?(&:frozen?), k1.frozen?]
        u = {}; u.update({}.compare_by_identity.tap { |q| q["u".dup] = 1; q["u".dup] = 2 })
        r << [u, u.keys.all?(&:frozen?)]
        r << src.none?(&:frozen?)
        r
        "####,
    );
}
