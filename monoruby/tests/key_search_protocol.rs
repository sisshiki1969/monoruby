extern crate monoruby;
use monoruby::tests::*;

// What a search asks of the elements it walks.
//
// - #2206: `rb_equal` — an element that *is* the argument is found
//   however its `==` answers, and `Hash#assoc` looks the key up by its
//   hash value before it scans.
// - #2207: a `Set` hashes the element even when it is empty.
// - #2213: `Array#<=>` and `Array#uniq` read the length and the element
//   afresh each round, so an element's own method popping the Array is
//   not walked past its new end.

// As a String: an answer may hold an object whose `inspect` the harness
// could not read back from CRuby's output.
fn check_once(code: &str) {
    run_test_once(&format!("{code}.inspect"));
}

// A `==` that answers `false` for its own receiver, and one that raises:
// both are found by identity alone wherever CRuby's `rb_equal` is used.
const NEVER_EQ: &str = r##"
require "set"
class NeverEq
  def ==(o) = false
  def eql?(o) = equal?(o)
  def hash = 7
  def inspect = "NeverEq"
end
class RaiseEq
  def ==(o) = raise("== ran")
  def eql?(o) = equal?(o)
  def hash = 7
  def inspect = "RaiseEq"
end
"##;

#[test]
fn search_finds_the_identical_element() {
    check_once(&format!(
        r##"{NEVER_EQ}
        [NeverEq.new, RaiseEq.new, Float::NAN].map do |o|
          r = []
          r << ({{o => 1}}.assoc(o) ? :pair : nil)
          big = {{}}; 12.times {{ |i| big["k#{{i}}"] = i }}; big[o] = 1
          r << (big.assoc(o) ? :pair : nil)
          r << ({{1 => o}}.rassoc(o) ? :pair : nil)
          r << {{1 => o}}.key(o) << {{1 => o}}.value?(o) << {{1 => o}}.has_value?(o)
          r << [o].each.include?(o) << [o].each.member?(o) << [o].each.find_index(o)
          r << [o].each_entry.include?(o) << (1..1).map {{ o }}.each.include?(o)
          r << [o].include?(o) << [o].index(o) << [o].count(o)
          r << [[o, 1]].assoc(o).nil? << [[1, o]].rassoc(o).nil?
          r << {{o => 1}}.key?(o) << Set[o].include?(o)
          r
        end
      "##
    ));
}

// The identity step is the *only* thing that is skipped: a `==` between
// different objects still runs, with the operand order CRuby uses — the
// argument is the receiver for `Hash#assoc` / `rassoc`, the stored one
// for `Hash#key` / `value?` and `Enumerable#include?` / `find_index`.
#[test]
fn search_asks_the_receiver_cruby_asks() {
    check_once(&format!(
        r##"{NEVER_EQ}
        def pair(a, b) = [(a.call rescue $!.message), (b.call rescue $!.message)]
        big = -> {{ h = {{}}; 12.times {{ |i| h["k#{{i}}"] = i }}; h }}
        r = []
        r << pair(-> {{ h = big.(); h[RaiseEq.new] = 1; h.assoc(NeverEq.new) }},
                  -> {{ h = big.(); h[NeverEq.new] = 1; h.assoc(RaiseEq.new) }})
        r << pair(-> {{ {{RaiseEq.new => 1}}.assoc(NeverEq.new) }},
                  -> {{ {{NeverEq.new => 1}}.assoc(RaiseEq.new) }})
        r << pair(-> {{ {{1 => RaiseEq.new}}.rassoc(NeverEq.new) }},
                  -> {{ {{1 => NeverEq.new}}.rassoc(RaiseEq.new) }})
        r << pair(-> {{ {{1 => RaiseEq.new}}.key(NeverEq.new) }},
                  -> {{ {{1 => NeverEq.new}}.key(RaiseEq.new) }})
        r << pair(-> {{ {{1 => RaiseEq.new}}.value?(NeverEq.new) }},
                  -> {{ {{1 => NeverEq.new}}.value?(RaiseEq.new) }})
        r << pair(-> {{ [RaiseEq.new].each.include?(NeverEq.new) }},
                  -> {{ [NeverEq.new].each.include?(RaiseEq.new) }})
        r << pair(-> {{ [RaiseEq.new].each.find_index(NeverEq.new) }},
                  -> {{ [NeverEq.new].each.find_index(RaiseEq.new) }})
        r
      "##
    ));
}

// `rb_hash_assoc`: past the size an ar table holds, the key is looked up
// by its hash value first — one `#hash` on the argument, the pairs filed
// under it compared with `==` — and the pair answered is then
// `[argument, stored value]`. A smaller Hash, and an identity one of any
// size, only scan.
#[test]
fn hash_assoc_looks_the_key_up_by_its_hash() {
    check_once(
        r##"
        $h = $e = $q = 0
        class C
          def initialize(n = 0) = @n = n
          def inspect = "C#{@n}"
          def hash = ($h += 1; 1)
          def eql?(o) = ($e += 1; equal?(o))
          def ==(o) = ($q += 1; C === o)
        end
        def counted
          $h = $e = $q = 0
          r = yield
          [r, $h, $e, $q]
        end
        r = []
        [3, 8, 9, 12].each do |n|
          build = -> { h = {}; n.times { |i| h["k#{i}"] = i }; h }
          a = C.new(1)
          h = build.(); h[a] = :va
          r << counted { x = h.assoc(C.new(2)); [x, x && x[0].equal?(a)] }
          r << counted { x = h.assoc(a); [x, x && x[0].equal?(a)] }
          h2 = build.(); h2[a] = :va; h2.compare_by_identity
          r << counted { h2.assoc(a) }
          r << counted { build.().assoc(C.new(3)) }
        end
        # The `#hash` of the argument is sent, and what it raises comes out.
        bad = Class.new { def hash = raise("no hash"); def ==(o) = false }.new
        big = {}; 12.times { |i| big[i] = i }
        r << (begin; big.assoc(bad); rescue => e; e.message; end)
        r << (begin; {1 => 2}.assoc(bad); rescue => e; e.message; end)
        nonint = Class.new { def hash = "x"; def ==(o) = false }.new
        r << (begin; big.assoc(nonint); rescue => e; e.class; end)
        # An empty Hash asks nothing at all.
        c = 0; k = Class.new { define_method(:hash) { c += 1; 1 } }.new
        r << [{}.assoc(k), c]
        # A `==` that deletes the pair it accepts: the value answered is
        # the one the probe read.
        del = Class.new do
          def initialize(h) = @h = h
          def hash = 1
          def ==(o) = (@h.delete(o); true)
          def inspect = "Del"
        end
        big = {}; 12.times { |i| big[i] = i }
        stored = Class.new { def hash = 1; def eql?(o) = equal?(o) }.new
        big[stored] = :sv
        r << big.assoc(del.new(big)).last << big.size
        r
      "##,
    );
}

// #2207: a Set's table hashes the element whatever the Set holds, so an
// empty Set's membership test sends `#hash` — where an empty Hash's
// lookup sends nothing. An identity Set hashes nothing either way.
#[test]
fn empty_set_membership_hashes_the_element() {
    check_once(
        r##"
        require "set"
        class Bad; def hash = raise("boom"); end
        class Cnt; def hash = ($c += 1; 1); def eql?(o) = equal?(o); end
        b = Bad.new
        r = []
        [-> { Set.new.include?(b) }, -> { Set.new.member?(b) },
         -> { Set.new === b }, -> { Set.new.disjoint?([b]) },
         -> { Set.new.intersect?([b]) }, -> { Class.new(Set).new.include?(b) },
         -> { Set.new(nil).include?(b) }, -> { Set.new([1]).include?(b) },
         -> { Set.new.compare_by_identity.include?(b) },
         -> { Set.new.disjoint?(Set.new([1])) },
         -> { Set.new.intersect?(Set.new([1])) },
         -> { Set.new.disjoint?([]) },
         -> { Set.new.delete?(b) }, -> { Set.new.add?(b) },
         -> { {}.key?(b) }, -> { {}.delete(b) },
         -> { Set.new.include?(1) }, -> { Set.new.disjoint?([1]) },
         -> { Set.new.intersect?([1]) }].each do |f|
          r << (begin; f.call; rescue => e; e.message; end)
        end
        $c = 0
        s = Set.new
        r << [s.include?(Cnt.new), s === Cnt.new, s.disjoint?([Cnt.new]),
              s.intersect?([Cnt.new])]
        r << $c
        $c = 0
        r << [Set.new.compare_by_identity.include?(Cnt.new), $c]
        r
      "##,
    );
}

// #2213: `Array#<=>` compares the pairs below the shorter length as it
// was on entry, reading each element afresh — an element popped by a
// `<=>` run meanwhile is `nil` — and the lengths as they are by the end
// decide when no pair did.
#[test]
fn array_cmp_reads_each_element_afresh() {
    check_once(
        r##"
        class Shrink
          def initialize(a) = @a = a
          def <=>(o) = (@a.pop(@a.size - 1) if @a.size > 1; 0)
          def inspect = "Shrink"
        end
        class Grow
          def initialize(a) = @a = a
          def <=>(o) = (@a << 1; 0)
          def inspect = "Grow"
        end
        class Clear
          def initialize(a) = @a = a
          def <=>(o) = (@a.clear; 0)
          def inspect = "Clear"
        end
        r = []
        a = []; a << Shrink.new(a); 5.times { a << 1 }
        r << (a <=> Array.new(6) { 2 }) << a.size
        b = []; b << Grow.new(b)
        r << (b <=> [1]) << (b <=> [1, 0]) << b.size
        other = [1, 2, 3]
        c = [Clear.new(other), 5, 6]
        r << (c <=> other)
        other2 = [1, 2, 3]
        r << ([Clear.new(other2), nil, nil] <=> other2)
        # The ordinary answers, and the recursion guard.
        r << ([1, 2] <=> [1, 3]) << ([1, 2] <=> [1]) << ([] <=> [1])
        r << ([1] <=> [1]) << ([1] <=> 1) << ([1] <=> ["a"])
        d = []; d << d << 2; e = []; e << e << 1 << 0
        r << (d <=> e)
        f = []; f << f; g = []; g << g << 1
        r << (f <=> g) << (g <=> f)
        same = [Class.new { def <=>(o) = 1 }.new]
        r << (same <=> same)
        r << ([1] <=> Class.new { def to_ary = [2] }.new)
        r << ([1] <=> Class.new { def to_ary = nil }.new)
        r << (begin; [1] <=> Class.new { def to_ary = 1 }.new; rescue => e; e.class; end)
        # What an element's `<=>` raises comes out.
        bad = Class.new { def <=>(o) = raise("no order") }.new
        r << (begin; [1, bad] <=> [1, 2]; rescue => e; e.message; end)
        r
      "##,
    );
}

// #2213: `Array#uniq` / `uniq!` hash the element the Array holds *now* —
// one popped by an earlier element's `#hash` is never hashed — and an
// Array of one element is uniq'd without hashing anything at all.
#[test]
fn array_uniq_reads_each_element_afresh() {
    check_once(
        r##"
        $log = []
        class Pop
          def initialize(a) = @a = a
          def shrink = (@a.pop(@a.size - 1) if @a.size > 1)
          def hash = (shrink; 1)
          def eql?(o) = (shrink; true)
          def inspect = "Pop"
        end
        class Logged
          def hash = ($log << :h; 2)
          def eql?(o) = ($log << :e; true)
          def inspect = "Logged"
        end
        mk = -> { a = []; a << Pop.new(a); 5.times { a << Logged.new }; a }
        r = []
        $log.clear; r << mk.().uniq.size << $log.tally
        $log.clear; a = mk.(); r << a.uniq!.nil? << a.size << $log.tally
        $log.clear; r << mk.().uniq { |x| x }.size << $log.tally
        $log.clear; a = mk.(); r << a.uniq! { |x| x }.nil? << a.size << $log.tally
        # Nothing to compare with: neither `#hash` nor the block is called.
        class Bad; def hash = raise("boom"); end
        r << [Bad.new].uniq.size << [Bad.new].uniq!
        c = 0; [1].uniq { c += 1 }; [1].uniq! { c += 1 }; r << c
        r << (begin; [].freeze.uniq!; rescue => e; e.class; end)
        # An element whose `#hash` grows or freezes the receiver.
        class Grow
          def initialize(a) = @a = a
          def hash = (@a << @a.size if @a.size < 10; 5)
          def eql?(o) = false
          def inspect = "Grow"
        end
        g = []; g << Grow.new(g) << 1
        r << g.uniq.size << g.size
        g2 = []; g2 << Grow.new(g2) << 1 << 1
        g2.uniq!; r << g2.size
        class Freeze
          def initialize(a) = @a = a
          def hash = (@a.freeze; 5)
          def inspect = "Freeze"
        end
        f = []; f << Freeze.new(f) << 1 << 1
        r << (begin; f.uniq!; rescue => e; e.class; end)
        f2 = []; f2 << Freeze.new(f2) << 1 << 2
        r << f2.uniq! << f2.size
        # The ordinary answers: order, the stored object, recursion.
        s = ["a", "b", "a"]
        u = s.uniq; r << u << u[0].equal?(s[0]) << u[0].frozen?
        r << [1, 1.0, 1r, "1", :a, nil, nil, [1], [1], {a: 1}, {a: 1}, 2**70, 2**70].uniq
        rec = [1, 2]; rec << rec << rec
        r << rec.uniq.size << rec.uniq!.equal?(rec) << rec.size
        t = [1, 2, 1, 3, 2]; r << t.uniq! << t
        n = [1, 2, 3]; r << n.uniq! << n
        r << [1, 2, 3, 4].uniq { |x| x % 2 }
        r << ((1..20).to_a + (1..20).to_a).uniq.size
        r
      "##,
    );
}

// `Array#|` over the size at which it hashes rather than scans: the same
// re-read, and the elements' methods see the result being built.
#[test]
fn array_union_reads_each_element_afresh() {
    check_once(
        r##"
        class Pop
          def initialize(a) = @a = a
          def hash = (@a.pop(@a.size - 1) if @a.size > 1; 1)
          def eql?(o) = true
          def inspect = "Pop"
        end
        r = []
        a = []; a << Pop.new(a); 20.times { |i| a << i }
        r << (a | [:x]).size << a.size
        b = (1..20).to_a
        r << (b | b).size << (b | [21]).size << ([1, 2] | [2, 3])
        r << ([1, 2] | [2, 3] | [3, 4]) << (b | b | b).size
        r << ([1, 1, 2] | []) << ([] | [1, 1, 2])
        r
      "##,
    );
}
