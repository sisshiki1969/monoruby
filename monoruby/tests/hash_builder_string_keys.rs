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
