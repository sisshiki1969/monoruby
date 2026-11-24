extern crate monoruby;
use monoruby::tests::*;

// A Hash changed while one of its own methods walks it (#2155): by the
// block, or by a key's `#hash` / `#eql?` / `#==` / `#inspect`. CRuby's
// `rb_hash_foreach` reads the table again at every step, so a pair
// deleted ahead of the walk is not visited, one that is still there is,
// and adding a key raises. Each case is `[label, answer or error]`.

const PRELUDE: &str = r##"
def t = (yield rescue "#{$!.class}: #{$!.message}")
# A value whose #hash and #== delete :b from `h`.
class D
  attr_accessor :h, :n
  def initialize(n) = @n = n
  def hash = (h&.delete(:b); n.hash)
  def eql?(o) = n == o.n
  def ==(o) = (h&.delete(:b); D === o && n == o.n)
  def inspect = "D#{n}"
end
# A value whose #inspect deletes :b from `h`.
class I
  attr_accessor :h
  def inspect = (h&.delete(:b); "I")
end
# A value that runs `act` from #hash, #eql?, #== and #to_s.
class V
  attr_accessor :h, :n, :act
  def initialize(n, &act) = (@n = n; @act = act)
  def hash = (act&.call; n.hash)
  def eql?(o) = (act&.call; n == o.n)
  def ==(o) = (act&.call; V === o && n == o.n)
  def inspect = "V#{n}"
  def to_s = (act&.call; "V#{n}")
end
def kw(**o) = o
def big(n = 20) = (1..n).to_h { |i| [i, i * 10] }
"##;

fn check(cases: &str) {
    // As a String: the answers hold objects with an `inspect` of their
    // own, which the harness could not read back from CRuby's output.
    run_test_once(&format!("{PRELUDE}\n[\n{cases}\n].inspect"));
}

// `each` and the Enumerable methods built on it: a block that deletes
// from, clears or adds to the Hash it is walking.
#[test]
fn hash_walk_enumerable() {
    check(
        r##"
        ["each clear", t { h = {a: 1, b: 2}; r = []; h.each { |k, v| r << k; h.clear }; [r, h] }],
        ["to_a clear in each", t { h = {a: 1, b: 2, c: 3}; r = []; h.each { |k, v| r << k; h.delete(:b) }; r }],
        ["any? del", t { h = {a: 1, b: 2, c: 3}; r = []; h.any? { |k, v| r << k; h.delete(:b); false }; r }],
        ["select del", t { h = {a: 1, b: 2, c: 3}; r = []; h.select { |k, v| r << k; h.delete(:b); true } }],
        ["reject del", t { h = {a: 1, b: 2, c: 3}; r = []; h.reject { |k, v| r << k; h.delete(:b); false } }],
        ["min_by del", t { h = {a: 1, b: 2, c: 3}; r = []; h.min_by { |k, v| r << k; h.delete(:b); v }; r }],
        ["sum del", t { h = {a: 1, b: 2, c: 3}; r = []; h.sum { |k, v| r << k; h.delete(:b); v } }],
        ["to_h del", t { h = {a: 1, b: 2, c: 3}; h.to_h { |k, v| h.delete(:b); [k, v] } }],
        ["filter_map del", t { h = {a: 1, b: 2, c: 3}; h.filter_map { |k, v| h.delete(:b); k } }],
        ["find del", t { h = {a: 1, b: 2, c: 3}; r = []; h.find { |k, v| r << k; h.delete(:b); false }; r }],
        ["each_with_index del", t { h = {a: 1, b: 2, c: 3}; r = []; h.each_with_index { |(k, v), i| r << k; h.delete(:b) }; r }],
        ["group_by del", t { h = {a: 1, b: 2, c: 3}; h.group_by { |k, v| h.delete(:b); v.odd? } }],
        ["values_at", t { h = {a: 1}; h.values_at(:a, :b) }],
        ["compact del", t { h = {a: 1, b: nil, c: 3}; h.compact }],
        ["sort_by del", t { h = {a: 1, b: 2, c: 3}; h.sort_by { |k, v| h.delete(:b); -v } }],
        ["count del", t { h = {a: 1, b: 2, c: 3}; h.count { |k, v| h.delete(:b); true } }],
        ["each_entry", t { h = {a: 1, b: 2, c: 3}; r = []; h.each_entry { |e| r << e; h.delete(:b) }; r }],
        ["flat_map del", t { h = {a: 1, b: 2, c: 3}; h.flat_map { |k, v| h.delete(:b); [k] } }],
        ["reduce del", t { h = {a: 1, b: 2, c: 3}; h.reduce(0) { |s, (k, v)| h.delete(:b); s + v } }],
        ["partition del", t { h = {a: 1, b: 2, c: 3}; h.partition { |k, v| h.delete(:b); v.odd? } }],
        ["each_slice del", t { h = {a: 1, b: 2, c: 3}; r = []; h.each_slice(1) { |e| r << e; h.delete(:b) }; r }],
        ["zip del", t { h = {a: 1, b: 2, c: 3}; h.zip([1,2,3]) }],
        ["transform_values del", t { h = {a: 1, b: 2, c: 3}; h.transform_values { |v| h.delete(:b); v } }],
        ["filter_map2", t { h = {a: 1, b: 2, c: 3}; h.each_key.map { |k| h.delete(:b); k } }],
        ["big each clear add", t { h = big; r = []; (h.each { |k, v| r << k; h.clear; h[:z] = 1 } rescue r << $!.message); [r, h] }],
        ["small each del add", t { h = {a: 1, b: 2}; r = []; (h.each { |k, v| r << k; h.delete(:b); h[:z] = 1 } rescue r << $!.message); [r, h] }],
        ["each shift", t { h = {a: 1, b: 2, c: 3}; r = []; h.each { |k, v| r << k; h.shift }; [r, h] }],
        ["each update existing", t { h = {a: 1, b: 2, c: 3}; r = []; h.each { |k, v| r << [k, v]; h[:c] = 9 }; [r, h] }],
        ["each del all", t { h = {a: 1, b: 2, c: 3}; r = []; h.each { |k, v| r << k; h.delete(:a); h.delete(:b); h.delete(:c) }; [r, h, h.empty?] }],
        ["each_key del", t { h = {a: 1, b: 2, c: 3}; r = []; h.each_key { |k| r << k; h.delete(:b) }; r }],
        ["each_value del", t { h = {a: 1, b: 2, c: 3}; r = []; h.each_value { |v| r << v; h.delete(:b) }; r }],
        ["map del", t { h = {a: 1, b: 2, c: 3}; h.map { |k, v| h.delete(:b); k } }],
        ["transform_values! del", t { h = {a: 1, b: 2, c: 3}; h.transform_values! { |v| h.delete(:b); v * 2 }; h }],
        ["transform_keys! del", t { h = {a: 1, b: 2, c: 3}; (h.transform_keys! { |k| h.delete(:b); k.to_s } rescue $!.message); h }],
        ["filter_map clear", t { h = {a: 1, b: 2, c: 3}; h.filter_map { |k, v| h.clear; k } }],
        ["any? clear", t { h = {a: 1, b: 2, c: 3}; r = []; h.any? { |k, v| r << k; h.clear; false }; r }],
        ["all? clear", t { h = {a: 1, b: 2, c: 3}; r = []; h.all? { |k, v| r << k; h.clear; true }; r }],
        ["count clear", t { h = {a: 1, b: 2, c: 3}; h.count { |k, v| h.clear; true } }],
        ["find_all clear", t { h = {a: 1, b: 2, c: 3}; h.find_all { |k, v| h.clear; true } }],
        ["select clear", t { h = {a: 1, b: 2, c: 3}; h.select { |k, v| h.clear; true } }],
        ["reject clear", t { h = {a: 1, b: 2, c: 3}; h.reject { |k, v| h.clear; false } }],
        ["min_by clear", t { h = {a: 1, b: 2, c: 3}; h.min_by { |k, v| h.clear; v } }],
        ["sum clear", t { h = {a: 1, b: 2, c: 3}; h.sum { |k, v| h.clear; v } }],
        ["to_h clear", t { h = {a: 1, b: 2, c: 3}; h.to_h { |k, v| h.clear; [k, v] } }],
        ["each_with_object clear", t { h = {a: 1, b: 2, c: 3}; h.each_with_object([]) { |(k, v), a| a << k; h.clear } }],
        ["group_by clear", t { h = {a: 1, b: 2, c: 3}; h.group_by { |k, v| h.clear; v.odd? } }],
        ["partition clear", t { h = {a: 1, b: 2, c: 3}; h.partition { |k, v| h.clear; v.odd? } }],
        ["compact! in each", t { h = {a: 1, b: nil, c: 3}; r = []; h.each { |k, v| r << k; h.compact! }; [r, h] }],
        ["delete in each iter_lev nested raise", t { h = {a: 1, b: 2}; begin; h.each { h.each { raise "x" } }; rescue; end; h[:z] = 1; h }],
        ["gc keys", t { h = {}; 50.times { |i| h["k#{i}"] = "v#{i}" }; h.delete_if { |k, v| GC.start if k == "k10"; k.size == 2 }; [h.size, h["k10"], h.keys.last] }],
        "##,
    );
}

// `delete_if` / `keep_if` / `select!` / `reject!` delete each pair as it is
// reached, so the block sees the Hash shrink, a freeze in the block
// raises at the next deletion, and `select!` / `reject!` answer `self`
// whenever the size changed.
#[test]
fn hash_walk_delete_where() {
    check(
        r##"
        ["delete_if size", t { h = {a: 1, b: 2, c: 3}; r = []; h.delete_if { |k, v| r << h.size; true }; [r, h] }],
        ["delete_if freeze", t { h = {a: 1, b: 2, c: 3}; h.delete_if { |k, v| h.freeze; true }; h }],
        ["keep_if freeze", t { h = {a: 1, b: 2, c: 3}; h.keep_if { |k, v| h.freeze; false }; h }],
        ["select! freeze", t { h = {a: 1, b: 2, c: 3}; h.select! { |k, v| h.freeze; false }; h }],
        ["reject! freeze", t { h = {a: 1, b: 2, c: 3}; h.reject! { |k, v| h.freeze; true }; h }],
        ["select! blockdel ret", t { h = {a: 1, b: 2, c: 3}; r = h.select! { |k, v| h.delete(:c); true }; [r, h] }],
        ["reject! blockdel ret", t { h = {a: 1, b: 2, c: 3}; r = h.reject! { |k, v| h.delete(:c); false }; [r, h] }],
        ["select! add", t { h = {a: 1, b: 2, c: 3}; h.select! { |k, v| h[:z] = 1; true }; h }],
        ["select! clear", t { h = {a: 1, b: 2, c: 3}; r = []; x = h.select! { |k, v| r << k; h.clear; true }; [r, x, h] }],
        ["delete_if break", t { h = {a: 1, b: 2, c: 3}; h.delete_if { |k, v| break if k == :b; true }; h[:z] = 1; h }],
        ["delete_if raise", t { h = {a: 1, b: 2, c: 3}; (h.delete_if { |k, v| raise "x" if k == :b; true } rescue nil); h[:z] = 1; h }],
        ["delete_if nested del", t { h = {a: 1, b: 2, c: 3}; r = []; h.delete_if { |k, v| r << k; h.delete(:c); false }; [r, h] }],
        ["big delete_if", t { h = big; h.delete_if { |k, v| k.even? }; h.keys }],
        ["big delete_if del later", t { h = big; r = []; h.delete_if { |k, v| r << k; h.delete(k + 1); false }; [r, h.keys] }],
        ["big select! clear", t { h = big; r = []; x = h.select! { |k, v| r << k; h.clear; true }; [r, x, h] }],
        ["delete_if shift", t { h = {a: 1, b: 2, c: 3}; r = []; h.delete_if { |k, v| r << k; h.shift; false }; [r, h] }],
        ["delete_if after reuse", t { h = {a: 1, b: 2, c: 3}; h.delete_if { |k, v| k == :b }; h[:b] = 5; h[:d] = 6; [h, h.keys, h.size] }],
        ["big delete_if after reuse", t { h = big; h.delete_if { |k, v| k > 3 }; h[100] = 5; [h, h.size, h.first, h.to_a.last] }],
        ["reject! none", t { h = {a: 1}; h.reject! { false } }],
        ["select! none", t { h = {a: 1}; h.select! { true } }],
        ["keep_if ret", t { h = {a: 1}; h.keep_if { true }.equal?(h) }],
        ["delete_if enum", t { h = {a: 1, b: 2}; e = h.delete_if; [e.class, e.size, e.each { |k, v| v == 1 }, h] }],
        ["select! enum", t { h = {a: 1, b: 2}; e = h.select!; [e.size, e.each { |k, v| v == 1 }, h] }],
        ["frozen delete_if", t { h = {a: 1}.freeze; h.delete_if { false } }],
        ["frozen empty reject!", t { h = {}.freeze; h.reject! { false } }],
        ["filter! alias", t { h = {a: 1, b: 2}; [h.filter! { |k, v| v > 1 }, h] }],
        ["delete_if throw", t { h = {a: 1, b: 2, c: 3}; catch(:x) { h.delete_if { |k, v| throw :x if k == :b; true } }; h[:z] = 1; h }],
        "##,
    );
}

// `merge` / `merge!` / `update` walk the argument while the block (or a
// key's `#hash`) runs.
#[test]
fn hash_walk_merge() {
    check(
        r##"
        ["merge! self", t { h = {a: 1, b: 2}; h.merge!(h) { |k, a, b| a + b }; h }],
        ["merge! add other", t { a = {a: 1}; b = {a: 2, b: 3}; a.merge!(b) { |k, x, y| b[:zz] = 1; x + y }; [a, b] }],
        ["merge add other noblk", t { a = {a: 1}; b = {a: 2, b: 3}; a.merge!(b); [a, b] }],
        ["merge! del later", t { a = {"x" => 1, "y" => 2, "z" => 3}; b = {"x" => 10, "y" => 20, "z" => 30}; a.merge!(b) { |k, x, y| b.delete("z"); x + y }; [a, b] }],
        ["merge del", t { a = {"x" => 1, "y" => 2, "z" => 3}; b = {"x" => 10, "y" => 20, "z" => 30}; c = a.merge(b) { |k, x, y| b.delete("y"); x + y }; [a, b, c] }],
        ["merge multi", t { {a: 1}.merge({a: 2, b: 3}, {b: 4, c: 5}) { |k, x, y| x + y } }],
        ["merge! multi", t { h = {a: 1}; h.merge!({a: 2, b: 3}, {b: 4, c: 5}) { |k, x, y| x + y }; h }],
        ["merge! self noblk", t { h = {a: 1, b: 2}; h.merge!(h); h }],
        ["merge! self blk add", t { h = {a: 1, b: 2}; h.merge!(h) { |k, a, b| h[:z] = 1; a + b }; h }],
        ["merge cbi", t { a = {}.compare_by_identity; a["x"] = 1; b = {"x" => 2}; c = a.merge(b); [c.size, c.compare_by_identity?] }],
        ["merge default", t { a = Hash.new(5); a[:a] = 1; b = a.merge({b: 2}); [b, b.default, b[:zz]] }],
        ["update other del self", t { a = {a: 1, b: 2}; b = {a: 10, b: 20, c: 30}; a.update(b) { |k, x, y| a.delete(:b); x + y }; [a, b] }],
        "##,
    );
}

// `invert`, `key`, `rassoc` and `assoc` run `#==` / `#hash` per pair.
#[test]
fn hash_walk_search() {
    check(
        r##"
        ["invert", t { h = {a: 1, b: 2, c: 3}; h.invert }],
        ["invert del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h.invert, h] }],
        ["key del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h.key(3), h] }],
        ["rassoc del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h.rassoc(2), h] }],
        ["assoc del", t { v = D.new(1); h = {v => 1, :b => 2, :c => 3}; v.h = h; [h.assoc(:c), h.assoc(:b), h] }],
        ["invert big", t { big(12).invert.size }],
        ["invert collide", t { {a: 1, b: 1}.invert }],
        ["assoc", t { h = {1 => :a, 1.0 => :b}; [h.assoc(1.0), h.assoc(1), h.assoc(2)] }],
        ["assoc cbi", t { h = {}.compare_by_identity; s = "x"; h[s] = 1; [h.assoc("x"), h.assoc(s)] }],
        ["assoc clear", t { v = V.new(1); h = {v => 1, :b => 2}; w = V.new(1); w.act = -> { h.clear }; [h.assoc(w), h] }],
        ["rassoc clear", t { v = V.new(1); h = {a: 0, b: v, c: 3}; w = V.new(1); v.act = -> { h.clear }; [h.rassoc(w), h] }],
        ["key clear", t { v = V.new(1); h = {a: 0, b: v, c: 3}; w = V.new(1); v.act = -> { h.clear }; [h.key(w), h] }],
        ["key/rassoc", t { h = {a: 1, b: 2.0}; [h.key(2), h.rassoc(2), h.key(9), h.rassoc(9)] }],
        "##,
    );
}

// `==`, `eql?`, `hash` and the subset operators run `#==` / `#hash` on
// the values per pair.
#[test]
fn hash_walk_compare() {
    check(
        r##"
        ["== del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h == {a: D.new(1), b: 2, c: 3}, h] }],
        ["== del2", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h == {a: D.new(1), c: 3, d: 4}, h] }],
        ["eql del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h.eql?({a: D.new(1), b: 2, c: 3}), h] }],
        ["hash del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h.hash.class, h] }],
        ["<= del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [h <= {a: D.new(1), b: 2, c: 3, d: 4}, h] }],
        [">= del", t { v = D.new(1); h = {a: v, b: 2, c: 3}; v.h = h; [{a: D.new(1), b: 2, c: 3, d: 4} >= h, h] }],
        ["== order", t { {a: 1, b: 2} == {b: 2, a: 1} }],
        ["== nested", t { {a: {b: [1, {c: 2}]}} == {a: {b: [1, {c: 2}]}} }],
        ["== recursive", t { a = {}; a[:a] = a; b = {}; b[:a] = b; a == b }],
        ["eql float", t { [{a: 1}.eql?({a: 1.0}), {a: 1} == {a: 1.0}] }],
        ["hash eq", t { [{a: 1, b: 2}.hash == {b: 2, a: 1}.hash, {a: 1}.hash == {a: 2}.hash] }],
        ["hash recursive", t { a = {}; a[:a] = a; a.hash.class }],
        ["hash cbi", t { a = {}.compare_by_identity; a["x"] = 1; b = {}.compare_by_identity; b["x"] = 1; [a == b, a.hash == b.hash] }],
        ["<= etc", t { a = {a: 1}; b = {a: 1, b: 2}; [a <= b, a < b, b >= a, b > a, a >= b, a < a, a <= a] }],
        ["<= value clear", t { v = V.new(1); a = {a: v, b: 2}; b = {a: V.new(1), b: 2, c: 3}; v.act = -> { a.clear }; [(a <= b), a] }],
        ["== value clear", t { v = V.new(1); a = {a: v, b: 2}; b = {a: V.new(1), b: 2}; v.act = -> { a.clear }; [(a == b), a] }],
        ["== value add", t { v = V.new(1); a = {a: v, b: 2}; b = {a: V.new(1), b: 2}; v.act = -> { a[:z] = 1 }; [(a == b rescue $!.message), a.size] }],
        ["hash value add", t { v = V.new(1); a = {a: v, b: 2}; v.act = -> { a[:z] = 1 }; [(a.hash.class rescue $!.message), a.size] }],
        "##,
    );
}

// `inspect` runs `#inspect` of every key and value.
#[test]
fn hash_walk_inspect() {
    check(
        r##"
        ["inspect del", t { i = I.new; h = {a: i, b: 2, c: 3}; i.h = h; [h.inspect, h] }],
        ["inspect add", t { i = I.new; h = {a: i, b: 2, c: 3}; i.h = nil; def i.inspect = (@hh[:z] = 1; "I"); i.instance_variable_set(:@hh, h); h.inspect }],
        ["inspect clear", t { i = Object.new; h = {a: i, b: 2}; i.define_singleton_method(:inspect) { h.clear; "I" }; [h.inspect, h] }],
        ["inspect key del", t { i = Object.new; h = {i => 1, :b => 2, :c => 3}; i.define_singleton_method(:inspect) { h.delete(:b); "I" }; [h.inspect, h.size] }],
        ["to_s del", t { i = Object.new; h = {a: i, b: 2}; i.define_singleton_method(:inspect) { h.delete(:b); "I" }; h.to_s }],
        ["inspect recursive", t { h = {a: 1}; h[:b] = h; h.inspect }],
        "##,
    );
}

// `Hash[hash]` copies the table: no key is hashed again, and the
// default, the default proc and a frozen state stay behind.
#[test]
fn hash_walk_hash_bracket() {
    check(
        r##"
        ["Hash[] del", t { v = D.new(1); h = {v => 1, :b => 2, :c => 3}; v.h = h; [Hash[h], h] }],
        ["Hash[] hash", t { h = {a: 1, b: 2}; g = Hash[h]; g[:c] = 3; [h, g, g.equal?(h)] }],
        ["Hash[] default", t { h = Hash.new(7); h[:a] = 1; g = Hash[h]; [g, g.default, g[:zz]] }],
        ["Hash[] defproc", t { h = Hash.new { 7 }; h[:a] = 1; g = Hash[h]; [g, g.default_proc, g[:zz]] }],
        ["Hash[] cbi", t { h = {}.compare_by_identity; h["a"] = 1; g = Hash[h]; [g, g.compare_by_identity?] }],
        ["Hash[] frozen", t { h = {a: 1}.freeze; g = Hash[h]; g[:b] = 2; g }],
        ["Hash[] subclass", t { k = Class.new(Hash); g = k[{a: 1}]; [g, g.class == k] }],
        ["Hash[] subclass src", t { k = Class.new(Hash); s = k.new; s[:a] = 1; g = Hash[s]; [g, g.class] }],
        ["Hash[] to_hash", t { o = Object.new; def o.to_hash = {a: 1}; Hash[o] }],
        ["Hash[] big", t { h = big; g = Hash[h]; g.delete(1); [h.size, g.size] }],
        ["Hash[] pairs", t { [Hash[[[1, 2], [3]]], (Hash[[[1, 2, 3]]] rescue $!.message), Hash[1, 2, 3, 4], (Hash[1] rescue $!.message)] }],
        ["Hash[] during iteration", t { h = {a: 1, b: 2}; r = nil; h.each { r = Hash[h]; r[:z] = 1 }; [h, r] }],
        ["Hash[] deleted", t { h = {a: 1, b: 2, c: 3}; h.delete(:b); g = Hash[h]; g[:d] = 4; [g, g.keys] }],
        "##,
    );
}

// `clear` during a walk empties the Hash in place; `rehash`, `replace`
// and `compare_by_identity` raise then, each with its own message.
#[test]
fn hash_walk_clear_rehash_replace() {
    check(
        r##"
        ["each rehash", t { h = {a: 1, b: 2}; h.each { h.rehash } }],
        ["each cbi", t { h = {a: 1, b: 2}; h.each { h.compare_by_identity } }],
        ["each replace", t { h = {a: 1, b: 2}; r = []; h.each { |k, v| r << k; h.replace({x: 1, y: 2, z: 3}) }; [r, h] }],
        ["each replace same keys", t { h = {a: 1, b: 2}; r = []; h.each { |k, v| r << [k, v]; h.replace({a: 5, b: 6}) }; [r, h] }],
        ["clear in each then use", t { h = {a: 1, b: 2, c: 3}; h.each { h.clear }; h[:x] = 1; [h, h.size, h.keys, h.to_a] }],
        ["clear in nested each", t { h = {a: 1, b: 2, c: 3}; r = []; h.each { |k, _| h.each { |j, _| r << [k, j]; h.clear } }; h[:x] = 1; [r, h] }],
        ["big clear in each then use", t { h = big; r = []; h.each { |k, _| r << k; h.clear }; h[:x] = 1; [r, h, h.size] }],
        ["clear frozen in each", t { h = {a: 1}; h.each { h.freeze; h.clear } }],
        ["rehash basic", t { a = [1]; h = {a => 1}; a << 2; r = [h[a]]; h.rehash; r << h[a]; r << h.rehash.equal?(h); r }],
        ["rehash default", t { h = Hash.new(5); a = [1]; h[a] = 1; a << 2; h.rehash; [h[a], h[:zz], h.default] }],
        ["rehash defproc", t { h = Hash.new { |_, k| k }; a = [1]; h[a] = 1; a << 2; h.rehash; [h[a], h[:zz]] }],
        ["rehash cbi", t { h = {}.compare_by_identity; a = [1]; h[a] = 1; a << 2; h.rehash; [h[a], h.compare_by_identity?] }],
        ["rehash merge dups", t { a = [1]; b = [2]; h = {a => 1, b => 2}; b[0] = 1; h.rehash; h }],
        ["rehash frozen", t { {a: 1}.freeze.rehash }],
        ["rehash frozen iter", t { h = {a: 1}; h.each { h.freeze; h.rehash } }],
        ["rehash raise", t { v = V.new(1); h = {:a => 1, v => 2}; v.act = -> { raise "boom" }; r = (h.rehash rescue $!.message); v.act = nil; [r, h.size, h[:a]] }],
        ["rehash key del", t { v = V.new(1); h = {:a => 1, v => 2, :c => 3}; v.act = -> { h.delete(:c) }; r = (h.rehash rescue $!.message); v.act = nil; [r.class, h.size] }],
        ["rehash big", t { h = big; h.rehash; [h.size, h[20]] }],
        ["replace self iter", t { h = {a: 1}; h.each { h.replace(h) }; h }],
        ["replace frozen iter", t { h = {a: 1}; h.each { h.freeze; h.replace({}) } }],
        ["replace default", t { h = Hash.new(5); h.replace(Hash.new(7)); h.default }],
        ["cbi twice iter", t { h = {}.compare_by_identity; h[:a] = 1; h.each { h.compare_by_identity }; h.compare_by_identity? }],
        ["cbi frozen", t { {}.freeze.compare_by_identity }],
        ["cbi frozen already", t { h = {}.compare_by_identity.freeze; h.compare_by_identity.equal?(h) }],
        "##,
    );
}

// `**hash` walks the Hash being splatted.
#[test]
fn hash_walk_kwsplat() {
    check(
        r##"
        ["kwsplat sym del", t { h = {a: 1, b: 2}; kw(**h) }],
        "##,
    );
}

// The three examples of #2155. In the last one `merge` used to go on
// reading the argument's storage after the key's `#hash` had cleared it,
// and answered pairs of whatever was allocated there next.
#[test]
fn hash_walk_issue_2155() {
    run_test_once(
        r##"
        res = []
        h = {a: 1, b: 2, c: 3}
        r = []
        h.select! { |k, v| r << k; h.delete(:b) if k == :a; true }
        res << r << h

        a = {"x" => 1, "y" => 2, "z" => 3}
        b = {"x" => 10, "y" => 20, "z" => 30}
        a.merge!(b) { |k, x, y| b.delete(k); x + y }
        res << a << b

        class K
          attr_accessor :o
          def hash = (o&.clear; self.o = nil; $junk = Array.new(2000) { {"a#{_1}" => _1, "b" => 1, "c" => 2, "d" => 3} }; 1)
          def eql?(x) = equal?(x)
          def inspect = "K"
        end
        k = K.new
        other = {k => 1, :a => 2, :b => 3, :c => 4}
        k.o = other
        res << {}.merge(other).to_a
        res.inspect
        "##,
    );
}
