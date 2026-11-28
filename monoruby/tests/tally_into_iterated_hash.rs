extern crate monoruby;
use monoruby::tests::*;

// `tally(hash)` stores through CRuby's `rb_hash_stlike_update`, which does
// not look at the iteration level, so it may add a key to a Hash that is
// being walked (#2271), where `h[k] = v` raises. What the walk then does
// with the new pair is CRuby's: a small Hash (an `ar_table`) is walked up
// to the bound it had when the walk began, and raises once it has outgrown
// that size; a large or identity-keyed one (an `st_table`) is walked up to
// its current bound, so the new pair is reached.

#[test]
fn tally_adds_to_a_hash_being_walked() {
    run_test(
        r##"
        res = []
        t = ->(&b) { res << begin; b.call; rescue => e; "#{e.class}: #{e.message}"; end }
        # The issue's example: the new pairs are not walked.
        t.() { h = {1 => 1, 2 => 1}; n = 0; h.each { |k, v| n += 1; [k + 10].tally(h) }; [h, n] }
        t.() { h = {1 => 1, 2 => 1}; n = 0; h.each_pair { |k, v| n += 1; [k + 10].tally(h) }; [h, n] }
        t.() { h = {1 => 1, 2 => 1}; n = 0; h.each_key { |k| n += 1; [k + 10].tally(h) }; [h, n] }
        t.() { h = {1 => 1, 2 => 1}; n = 0; h.each_value { |v| n += 1; [n + 10].tally(h) }; [h, n] }
        # Up to eight pairs a small Hash stays small.
        t.() { h = {1 => 1, 2 => 1, 3 => 1, 4 => 1}; n = 0; h.each { |k, v| n += 1; [k + 10].tally(h) }; [h, n] }
        # Outgrowing it raises. (CRuby raises right after the block that
        # made it outgrow, monoruby once the walk reaches its bound.)
        t.() { h = (1..5).to_h { [_1, 1] }; h.each { |k, v| [k + 10].tally(h) }; h }
        t.() { h = (1..8).to_h { [_1, 1] }; n = 0; begin; h.each { |k, v| n += 1; [k + 10].tally(h) if n == 1 }; rescue => e; end; [e&.message, h.size] }
        t.() { h = (1..8).to_h { [_1, 1] }; h.each_key { |k| [k + 10].tally(h) if k == 8 }; h }
        t.() { h = (1..8).to_h { [_1, 1] }; h.delete_if { |k, v| [k + 10].tally(h) if k == 8; false }; h }
        # A large Hash's walk reaches the new pairs.
        t.() { h = (1..9).to_h { [_1, 1] }; n = 0; h.each { |k, v| n += 1; [k + 100].tally(h) if k < 3 }; [h.keys, n] }
        t.() { h = (1..9).to_h { [_1, 1] }; n = 0; h.each_key { |k| n += 1; [k + 100].tally(h) if k < 3 }; [h.keys, n] }
        t.() { h = (1..9).to_h { [_1, 1] }; n = 0; h.each { |k, v| n += 1; [k + 100].tally(h) if k < 50 }; [h.size, n] }
        # An identity-keyed one too, however small.
        t.() { h = {}.compare_by_identity; h[1] = 1; n = 0; h.each { |k, v| n += 1; [k + 1].tally(h) if k < 5 }; [h, n] }
        # Nested walks, and an existing key.
        t.() { h = {1 => 1}; n = 0; h.each { h.each { n += 1; [n + 10].tally(h) } }; [h, n] }
        t.() { h = {1 => 1}; h.each { [1].tally(h) }; h }
        # String keys go in as they are.
        t.() { h = {"a" => 1}; h.each { |k, v| [k + "b"].tally(h) }; [h, h.keys.map(&:frozen?)] }
        # `h[k] = v` still raises.
        t.() { h = {1 => 1}; h.each { h[2] = 1 }; h }
        # The walks written in Rust.
        t.() { h = {1 => 1, 2 => 2}; h.delete_if { |k, v| [k + 10].tally(h); false }; h }
        t.() { h = {1 => 1, 2 => 2}; n = 0; h.any? { |k, v| n += 1; [k + 10].tally(h); false }; [h, n] }
        t.() { h = {1 => 1, 2 => 2}; r = h.count { |k, v| [k + 10].tally(h); true }; [h, r] }
        t.() { h = (1..9).to_h { [_1, 1] }; r = h.count { |k, v| [k + 100].tally(h) if k < 3; true }; [h.keys, r] }
        t.() { h = {1 => 1, 2 => 2}; r = h.map { |k, v| [k + 10].tally(h); k }; [h, r] }
        t.() { h = {1 => 1}; h.transform_values! { |v| [99].tally(h); v + 1 }; h }
        t.() { h = {1 => 1}; r = h.to_h { |k, v| [99].tally(h); [k, v] }; [h, r] }
        t.() { h = {1 => 1, 2 => 2}; r = h.select { |k, v| [k + 100].tally(h); true }; [r, h] }
        # An external enumerator.
        t.() { h = {1 => 1, 2 => 1}; e = h.each; e.next; [5].tally(h); [h, e.next] }
        res
        "##,
    );
}

#[test]
fn tally_appends_behind_tombstones() {
    // A delete in the walk leaves a tombstone where the pair stood; the
    // pair `tally` appends goes after it, and once the walk is over the
    // Hash is whole: every key found, the deleted ones gone.
    run_test(
        r##"
        res = []
        class K
          def initialize(v) = @v = v
          attr_reader :v
          def hash = @v.hash
          def eql?(o) = o.is_a?(K) && o.v == @v
          def inspect = "K#{@v}"
        end
        h = {1 => 1, 2 => 1, 3 => 1}; n = 0
        h.each { |k, v| n += 1; h.delete(3) if k == 1; [k + 10].tally(h) }
        res << [h.dup, n, h.size, [1, 2, 3, 11, 12].map { h[_1] }]
        h[4] = 1; res << [h, h.size]
        h = (1..12).to_h { [_1, 1] }; n = 0
        h.each { |k, v| n += 1; h.delete(k); [k + 100].tally(h) if k < 3 }
        res << [h.dup, n, h.size, [1, 101, 102].map { h[_1] }]
        h[1] = 2; res << [h, h.size]
        h = {K.new(1) => 1, K.new(2) => 1}; n = 0
        h.each { |k, v| n += 1; h.delete(k) if k.v == 1; [K.new(k.v + 10)].tally(h) }
        res << [h, n, [1, 2, 11, 12].map { h[K.new(_1)] }]
        h = (1..10).to_h { [K.new(_1), 1] }; n = 0
        h.each { |k, v| n += 1; h.delete(k) if k.v.odd?; [K.new(k.v + 100)].tally(h) if k.v < 4 }
        res << [h, n, h.size, [1, 2, 101, 102, 103].map { h[K.new(_1)] }]
        h = {:a => 1, :b => 2}; s = +"x"
        h.each { |k, v| h.delete(:b); [s, s].tally(h) }
        res << [h, h["x"], h.keys[1].equal?(s)]
        res.inspect
        "##,
    );
}
