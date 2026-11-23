//! A Hash literal whose key runs Ruby code keeps what it has stored so
//! far reachable (#2153).
//!
//! `runtime::gen_hash` fills a map that lives on the Rust stack, where
//! the collector does not look. The literal's own keys and values are in
//! the frame's slots, but the frozen copy stored for an unfrozen String
//! key is held by the map alone, so a collection inside a later key's
//! `#hash` / `#eql?` used to free it. From the first key that may
//! dispatch, the literal is now built as a Hash on the temp stack.
//!
//! Each case runs warm (VM, then JIT) and is compared with CRuby,
//! together with the number of `#hash` and `#eql?` calls it made.

use monoruby::tests::*;

const PRELUDE: &str = r####"
class GcKey
  attr_reader :i
  def initialize(i) = @i = i
  def hash = ($hashed += 1; GC.start; $junk = Array.new(500) { |j| "junk#{j}" }; @i.hash)
  def eql?(o) = ($eqled += 1; GC.start; o.is_a?(GcKey) && o.i == @i)
  def inspect = "K#{@i}"
end
class GcStr < String
  def hash = ($hashed += 1; GC.start; $junk = Array.new(500) { |j| "junk#{j}" }; super)
  def eql?(o) = ($eqled += 1; GC.start; super)
end
$hashed = $eqled = 0
def chk(h) = h.map { |k, v| [k.inspect, k.frozen?, v] }
def counts = ((r = [$hashed, $eqled]); $hashed = $eqled = 0; r)
"####;

fn check(defs: &str, code: &str) {
    run_test_with_prelude(&format!("[{code}, counts]"), &format!("{PRELUDE}\n{defs}"));
}

/// The issue's shape: unfrozen String keys ahead of a key whose `#hash`
/// collects. Three pairs stay in the inline form until the first such
/// key, seven are pre-sized and boxed from the start.
#[test]
fn string_copies_survive_a_collecting_hash() {
    check(
        r####"def lit(i) = {"key-#{i}" => "a" * 3, GcKey.new(1) => "b" * 3, "other-#{i}" => "c" * 3}"####,
        "chk(lit(7))",
    );
    check(
        r####"def lit(i) = {"key-#{i}" => "a" * 3, GcKey.new(1) => "b" * 3, "other-#{i}" => "c" * 3, GcKey.new(2) => "d" * 3, "z#{i}" => i, GcKey.new(3) => [i], "y#{i}" => i.to_s}"####,
        "chk(lit(7))",
    );
}

/// Later pairs overwrite earlier ones through a Ruby `eql?`, and the
/// stored key stays the first one.
#[test]
fn duplicate_keys() {
    check(
        r####"def lit(i) = {"k#{i}" => 1, GcKey.new(1) => 2, "k#{i}" => 3, GcKey.new(1) => 4, "m#{i}" => 5}"####,
        "chk(lit(7))",
    );
}

/// A String with a class of its own hashes by its bytes but is compared
/// through its own `eql?`.
#[test]
fn string_subclass_keys() {
    check(
        r####"def lit(i) = {"k#{i}" => 1, GcStr.new("k#{i}") => 2, "l#{i}" => 3, GcStr.new("l#{i}") => 4, "k#{i}" => 5}"####,
        "chk(lit(7))",
    );
}

/// `**h` in the middle: the pairs before it are one `gen_hash`, the ones
/// after it go in through `hash_insert`.
#[test]
fn double_splat() {
    check(
        r####"def lit(i) = (hh = {"s#{i}" => 0, GcKey.new(5) => 1}; {"k#{i}" => 1, GcKey.new(1) => 2, **hh, "m#{i}" => 5, GcKey.new(5) => 9})"####,
        "chk(lit(7))",
    );
}

/// More pairs than one chunk holds (`LITERAL_CHUNK_LEN`).
#[test]
fn chunked_literal() {
    check(
        r####"
src = +"def lit(i) = {"
300.times { |n| src << (n % 50 == 7 ? "GcKey.new(#{n}) => #{n}, " : "\"k#{n}-\#{i}\" => #{n}, ") }
eval(src << "}")
def sum(i) = (h = lit(i); [h.size, h.keys.all?(&:frozen?), h.keys.map(&:inspect).join.size, h.values.sum])
"####,
        "sum(7)",
    );
}

/// Heap keys that hash and compare natively take the rooted path too.
#[test]
fn native_heap_keys() {
    check(
        r####"def lit(i) = (z = -0.0; {"k#{i}" => 1, [i, "x#{i}"] => 2, 2**70 + i => 3, 1e300 * (i + 1) => 4, z => 5, 0.0 => 6, (i..i + 1) => 7, "t#{i}" => 8, 2**70 + i => 9})"####,
        "chk(lit(7))",
    );
    check(
        r####"def lit(a, b, i) = {a => i, :s => 1, b => 2, "x#{i}" => 3, a => 4}"####,
        "(o = Object.new; h = lit(o, [o], 7); [h.size, h[o], h[[o]], h.keys[3].frozen?])",
    );
}

/// A `#hash` that raises leaves nothing behind: the next literal is
/// built as usual.
#[test]
fn hash_raises_midway() {
    check(
        r####"
def lit(i)
  o = Object.new
  def o.hash = (GC.start; raise "boom")
  {"k#{i}" => 1, o => 2, "m#{i}" => 3}
rescue => e
  [e.message, chk({"k#{i}" => 1, GcKey.new(i) => 2, "n#{i}" => 3})]
end
"####,
        "lit(7)",
    );
}

/// Literals of packed and String keys only never leave the fast path;
/// they must still store what `Hash#[]=` stores.
#[test]
fn native_keys_only() {
    check(
        r####"def lit(i, s) = {a: i, "b" => i, s => i, "c#{i}" => i, 1 => i, 1.5 => i, nil => i, true => i, s => 0, 0.0 => 1, :a.itself => 2}"####,
        r####"(s = +"s"; h = lit(7, s); [chk(h), h.keys[2].equal?(s), h.size])"####,
    );
    check(
        r####"def lit(i, s) = {s => i, "c#{i}" => 1, s => 2}"####,
        r####"(s = +"s"; h = lit(7, s); [chk(h), h.keys[0].equal?(s), h.size])"####,
    );
}
