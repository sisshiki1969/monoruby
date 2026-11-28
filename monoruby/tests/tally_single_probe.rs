extern crate monoruby;
use monoruby::tests::*;

// `tally` counts an element with CRuby's `tally_up`, one
// `rb_hash_stlike_update`: the element's `#hash` is sent once whether it
// is new to the Hash or already counted (#2265), and `eql?` is asked as
// often as one probe asks it.

#[test]
fn tally_hashes_each_element_once() {
    run_test_once(
        r##"
        class K
          def hash = ($n += 1; 1)
        end
        k = K.new
        res = []
        $n = 0; [k, k, k].tally; res << $n
        $n = 0; [k, k, k].tally({}); res << $n
        $n = 0; [k, k, k].each.tally; res << $n
        res
        "##,
    );
}

#[test]
fn tally_counts_with_one_probe() {
    run_test_once(
        r##"
        res = []
        class K
          def initialize(v) = @v = v
          attr_reader :v
          def hash = ($n += 1; @v.hash)
          def eql?(o) = ($e += 1; o.is_a?(K) && o.v == @v)
          def inspect = "K#{@v}"
        end
        t = ->(&b) { $n = 0; $e = 0; r = begin; b.call; rescue Exception => e; "#{e.class}: #{e.message}"; end; res << [r, $n, $e] }
        t.() { [K.new(1), K.new(1), K.new(2), K.new(1)].tally }
        t.() { [K.new(1), K.new(1)].tally({K.new(1) => 5}) }
        t.() { [K.new(1), K.new(1)].each.tally }
        # Past the inline representation's size, into the boxed map.
        t.() { (1..8).map { K.new(_1 % 3) }.tally }
        t.() { (1..20).map { K.new(_1 % 7) }.tally.values }
        t.() { h = {}.compare_by_identity; k = K.new(1); [k, k, K.new(1)].tally(h).values }
        t.() { [K.new(1)].tally({}.freeze) }
        # A count that is not an Integer, a Bignum one, the last Fixnum.
        t.() { [K.new(1)].tally({K.new(1) => "x"}) }
        t.() { [K.new(1)].tally({K.new(1) => 2**64}) }
        t.() { [K.new(1)].tally({K.new(1) => 2**62 - 1}) }
        t.() { [1].tally({1 => 2**62 - 1}) }
        # The key is stored as it is: no frozen copy of a String.
        t.() { s = +"a"; h = [s, "a", s].tally; [h, h.keys[0].equal?(s), h.keys[0].frozen?] }
        # An existing key is updated during iteration.
        t.() { h = {1 => 1, 2 => 1}; h.each { [1].tally(h) }; h }
        t.() { h = {K.new(1) => 1}; h.each { [K.new(1)].tally(h) }; h }
        t.() { [1, 1.0, 1r, nil, nil, :a, :a, 2**70, 2**70].tally }
        t.() { (1..30).map { _1 % 4 }.tally }
        # A `#hash` that stores into the Hash being counted into.
        class M
          def initialize(h) = @h = h
          def hash = ($n += 1; @h[:x] = 1 if @h; 7)
          def eql?(o) = equal?(o)
          def inspect = "M"
        end
        t.() { h = {}; m = M.new(h); [m, m].tally(h) }
        t.() { h = {}; m = M.new(nil); [m, *(1..5), m].tally(h).size }
        res.inspect
        "##,
    );
}
