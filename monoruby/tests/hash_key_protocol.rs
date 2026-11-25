extern crate monoruby;
use monoruby::tests::*;

// What a Hash asks of a key: `#hash` once, then `eql?` of the keys filed
// under that number.
//
// - #2204: an object whose `#hash` answers another object's hash value
//   meets that object as a key, whatever its type.
// - #2199: `#hash` and `eql?` are Ruby code, and may change the Hash the
//   probe is in.

// As a String: the answers hold objects with an `inspect` of their own,
// which the harness could not read back from CRuby's output.
fn check_once(code: &str) {
    run_test_once(&format!("{code}.inspect"));
}

const WRAPPER: &str = r##"
require "set"
# A key that stands in for the value it wraps.
class W
  def initialize(v) = @v = v
  attr_reader :v
  def hash = @v.hash
  def eql?(o) = @v.eql?(o.is_a?(W) ? o.v : o)
  def inspect = "W(#{@v.inspect})"
end
S3 = Struct.new(:a, :b)
VALS = [1, 2**70, -(2**70), 2**62, -2**62, 2**64, 1.5, 3.7, 0.0, -0.0, 1.0e300,
        :sym, nil, true, false, "str", "", [1, 2], [], [[1], "a"],
        {a: 1}, {}, {a: 1, b: 2}, 1..2, 1...2, ("a".."c"),
        1r, Rational(3, 4), Complex(1, 2), S3.new(1, 2), Object, Comparable]
"##;

// A wrapper found by, and finding, the value it wraps: lookups, stores
// and removals, in the inline and in the boxed representation.
#[test]
fn hash_key_delegated_hash() {
    check_once(&format!(
        r##"{WRAPPER}
        VALS.map do |v|
          w = W.new(v)
          r = []
          h = {{v => :x}}
          r << h[w] << h.key?(w) << h.fetch(w, :none)
          h2 = {{w => :y}}
          r << h2[v] << h2.key?(v)
          h3 = {{v => 1}}; h3[w] = 2
          r << h3.size << h3[v]
          h4 = {{v => 1, 2 => 3, 4 => 5, 6 => 7, 8 => 9}}
          r << h4[w] << h4.delete(w) << h4.size
          h5 = {{}}; 20.times {{ |i| h5[i * 3 + 100] = i }}; h5[v] = :big
          r << h5[w] << h5.key?(w); h5[w] = :upd; r << h5[v] << h5.size
          r << h5.delete(w) << h5.size
          s = Set[v]; r << s.include?(w); s << w; r << s.size
          r << (v.hash == w.hash)
          r << [v].hash.eql?([w].hash) << {{v => 1}}.hash.eql?({{w => 1}}.hash)
          r << [v, w].uniq.size << ([v] & [w]).size << ([v] | [w]).size << ([v] - [w]).size
          r << [v, w].group_by {{ _1 }}.size << [v, w].tally.size
          [v, r]
        end
        "##
    ));
}

// The same through compiled code: `h[k]`, `h[k] = v` and `key?` of a key
// that has to be asked.
#[test]
fn hash_key_delegated_hash_jit() {
    run_test_with_prelude(
        r##"
        r = []
        KEYS.each do |v, w|
          h = {v => 1}
          r << h[w] << h.key?(w)
          h[w] = 2
          r << h.size << h[v]
          big = BIG.dup
          big[v] = 3
          r << big[w]
          big[w] = 4
          r << big[v] << big.size << big.delete(w) << big.size
        end
        r
        "##,
        r##"
        class W
          def initialize(v) = @v = v
          attr_reader :v
          def hash = @v.hash
          def eql?(o) = @v.eql?(o.is_a?(W) ? o.v : o)
        end
        KEYS = [1, :a, "s", 2**70, 1.5, 1.0e300, [1, 2], {a: 1}, nil].map { |v| [v, W.new(v)] }
        BIG = (0...20).to_h { |i| [i * 3 + 100, i] }
        "##,
    );
}

// Delegators, the objects this is for.
#[test]
fn hash_key_delegator() {
    run_test_once(
        r##"
        require "delegate"
        r = []
        sd = SimpleDelegator.new("str")
        h = {"str" => 1}
        r << h[sd] << h.key?(sd) << ({sd => 1}["str"])
        r << ({42 => :a}[SimpleDelegator.new(42)])
        r << ({[1, 2] => :a}[SimpleDelegator.new([1, 2])])
        r << ({sym: :a}[SimpleDelegator.new(:sym)])
        r << ({2**70 => :a}[SimpleDelegator.new(2**70)])
        r << ({1.5 => :a}[SimpleDelegator.new(1.5)])
        r
        "##,
    );
}

// Who is asked, and what counts as equal.
#[test]
fn hash_key_eql_receiver() {
    run_test_once(
        r##"
        class W
          def initialize(v) = @v = v
          attr_reader :v
          def hash = @v.hash
          def eql?(o) = @v.eql?(o.is_a?(W) ? o.v : o)
        end
        # Inherits Kernel#hash, with an eql? of its own.
        class O
          def eql?(o) = true
        end
        r = []
        o = O.new
        h = {W.new(o) => 1}
        r << h[o]
        h = {o => 1}
        r << h[W.new(o)] << h[O.new]
        # A Hash key is the same key in any order of its pairs.
        r << ({ {a: 1, b: 2} => 1 }[{b: 2, a: 1}])
        r << ({a: 1, b: 2}.hash == {b: 2, a: 1}.hash)
        r << ([{a: 1, b: 2}].hash == [{b: 2, a: 1}].hash)
        # A String subclass hashes by its bytes; its eql? is sent.
        class Sub < String; def eql?(o) = false; end
        h = {}; h[Sub.new("q")] = 1
        r << h[Sub.new("q")] << h["q"]
        class Sub2 < String; def hash = 0; end
        h = {Sub2.new("q") => 1}
        r << h["q"] << h[Sub2.new("q")]
        # Hash values are Integers, equal for eql? objects.
        r << [1, :a, "s", 1.5, nil, [1], {a: 1}, 2**70, Object.new, 1.0e300].all? { |x| x.hash.is_a?(Integer) }
        r << (1.hash == 1.0.hash) << ("a".hash == "a".dup.hash) << ((2**70).hash == (2**70).hash)
        r << (0.0.hash == -0.0.hash) << (1.0e300.hash == 1.0e300.hash) << ([1, "a"].hash == [1, "a"].hash)
        r << {0.0 => 1}[-0.0] << {-0.0 => 1}[0.0]
        r
        "##,
    );
}

// What `#hash` may answer, and errors raised from the two methods.
#[test]
fn hash_key_hash_answers() {
    run_test_once(
        r##"
        r = []
        class TI; def hash = 2**70; def eql?(o) = o.is_a?(TI); end
        h = {TI.new => 1}; r << h[TI.new]
        class TF; def hash = Struct.new(:to_int).new(7); def eql?(o) = o.is_a?(TF); end
        h = {TF.new => 1}; r << h[TF.new]
        class TN; def hash = nil; end
        begin; {TN.new => 1}; rescue => e; r << e.class.to_s << e.message; end
        class TE; def hash = raise("boom"); end
        h = {1 => 2}
        begin; h[TE.new] = 1; rescue => e; r << e.message; end
        begin; h[TE.new]; rescue => e; r << e.message; end
        begin; h.delete(TE.new); rescue => e; r << e.message; end
        r << h
        class EE; def hash = 1.hash; def eql?(o) = raise("eboom"); end
        begin; h[EE.new]; rescue => e; r << e.message; end
        begin; h[EE.new] = 3; rescue => e; r << e.message; end
        begin; h.delete(EE.new); rescue => e; r << e.message; end
        r << h
        r
        "##,
    );
}

const MUTATING: &str = r##"
require "set"
# A key that runs `hact` from #hash and `act` from eql?. Every C has the
# one hash value, so each probe asks the keys already there.
class C
  attr_accessor :act, :hact, :n
  def initialize(n = 0) = @n = n
  def hash = (hact&.call; 1)
  def eql?(o) = (act&.call; equal?(o))
  def inspect = "C#{@n}"
end
def t = (yield rescue "#{$!.class}: #{$!.message}")
"##;

// #2199: a store, a lookup or a removal whose key changes the Hash from
// its `#hash` or its `eql?`. The store that cleared the Hash was left in
// storage the Hash had given back.
#[test]
fn hash_key_methods_change_the_hash() {
    check_once(&format!(
        r##"{MUTATING}
        MUT = {{
          clear:   ->(h, a) {{ h.clear }},
          add40:   ->(h, a) {{ 40.times {{ |i| h[i] = i }} }},
          delete:  ->(h, a) {{ h.delete(a) }},
          delete_other: ->(h, a) {{ h.delete(:x) }},
          rehash:  ->(h, a) {{ h.rehash }},
          replace: ->(h, a) {{ h.replace({{z: 1}}) }},
          shift:   ->(h, a) {{ h.shift }},
          store:   ->(h, a) {{ h[:y] = 1 }},
        }}
        OPS = {{
          store:   ->(h, b) {{ h[b] = 2 }},
          aref:    ->(h, b) {{ h[b] }},
          key?:    ->(h, b) {{ h.key?(b) }},
          delete:  ->(h, b) {{ h.delete(b) }},
          fetch:   ->(h, b) {{ h.fetch(b, :none) }},
          dig:     ->(h, b) {{ h.dig(b) }},
          values_at: ->(h, b) {{ h.values_at(b) }},
          update:  ->(h, b) {{ h.update(b => 4).size }},
        }}
        res = []
        [:act, :hact].each do |hook|
          [0, 3, 12].each do |pre|
            MUT.each do |mn, m|
              OPS.each do |on, o|
                res << [hook, pre, mn, on, t do
                  h = {{}}
                  pre.times {{ |i| h["k#{{i}}"] = i }}
                  h[:x] = 0
                  a = C.new(1); b = C.new(2)
                  h[a] = 1
                  fired = false
                  b.send("#{{hook}}=", -> {{ next if fired; fired = true; m.(h, a) }})
                  r = o.(h, b)
                  b.act = b.hact = nil
                  [r, h.size, h.key?(a), h.key?(b), h.keys.last(2)]
                end]
              end
            end
          end
        end
        res
        "##
    ));
}

// The same for a Set.
#[test]
fn set_member_methods_change_the_set() {
    check_once(&format!(
        r##"{MUTATING}
        res = []
        [:act, :hact].each do |hook|
          {{ clear: ->(s, a) {{ s.clear }}, add40: ->(s, a) {{ 40.times {{ |i| s << i }} }}, delete: ->(s, a) {{ s.delete(a) }} }}.each do |mn, m|
            {{ add: ->(s, b) {{ s << b; 0 }}, include: ->(s, b) {{ s.include?(b) }}, delete: ->(s, b) {{ s.delete(b); 0 }}, add?: ->(s, b) {{ s.add?(b) ? 1 : 0 }} }}.each do |on, o|
              res << [hook, mn, on, t do
                s = Set.new; a = C.new(1); b = C.new(2)
                s << a
                fired = false
                b.send("#{{hook}}=", -> {{ next if fired; fired = true; m.(s, a) }})
                r = o.(s, b)
                b.act = b.hact = nil
                [r, s.size, s.include?(a), s.include?(b)]
              end]
            end
          end
        end
        res
        "##
    ));
}

// An Array key's elements are asked too, and their methods may resize the
// Array being hashed or compared: nothing of it is held over the call.
// (`Array#hash` walked the elements an element's `#hash` had popped, and
// `Array#eql?` / `==` indexed past the end of a shrunken Array.)
#[test]
fn array_key_elements_resize_the_array() {
    check_once(
        r##"
        $log = []
        class Q
          def hash = ($log << :h; 2)
          def eql?(o) = ($log << :e; true)
          def ==(o) = ($log << :q; true)
        end
        # An element whose methods resize the Array it is in.
        class P
          def initialize(a, how, on) = (@a = a; @how = how; @on = on)
          def resize(m)
            return unless @on == m
            case @how
            when :pop then @a.pop(@a.size - 1) if @a.size > 1
            when :clear then @a.clear
            when :grow then 40.times { @a << Q.new } if @a.size < 40
            end
          end
          def hash = (resize(:hash); 1)
          def eql?(o) = (resize(:eql?); true)
          def ==(o) = (resize(:==); true)
        end
        res = []
        t = ->(&b) { $log.clear; r = b.call; [r, $log.tally] }
        [:pop, :clear, :grow].each do |how|
          mk = ->(on) { a = []; a << P.new(a, how, on); 5.times { a << Q.new }; a }
          res << t.() { mk.(:hash).hash.class }
          res << t.() { h = {}; h[mk.(:hash)] = 1; h.size }
          res << t.() { mk.(:eql?).eql?(Array.new(6) { Object.new }) }
          res << t.() { Array.new(6) { Q.new }.eql?(mk.(:nothing)) }
          res << t.() { mk.(:==) == Array.new(6) { Object.new } }
          res << t.() { a = mk.(:eql?); b = mk.(:eql?); h = {a => 1}; [h[b], a.size, b.size] }
        end
        res
        "##,
    );
}

// The crash of #2199 as reported, and a store or a removal while an
// external enumerator holds a traversal open.
#[test]
fn hash_key_eql_edge_cases() {
    check_once(&format!(
        r##"{MUTATING}
        res = []
        res << t do
          h = {{}}; a = C.new(1); b = C.new(2)
          h[a] = 1
          b.act = -> {{ h.clear }}
          h[b] = 2
          h.size
        end
        res << t do
          h = {{a: 1, b: 2}}; e = h.each; e.next
          k = C.new(1)
          (h[k] = 1 rescue $!.message)
        end
        res << t do
          h = {{a: 1, b: 2}}; k = C.new(1); h[k] = 0
          e = h.each; e.next
          h[k] = 5
          [h.delete(k), h.size, h]
        end
        # A removal hashes its key even when the Hash is empty; a lookup
        # does not.
        class Bad; def hash = raise("boom"); end
        res << t {{ {{}}.delete(Bad.new) }} << t {{ Hash.new(5).delete(Bad.new) }}
        res << t {{ {{}}[Bad.new] }} << t {{ Hash.new(5)[Bad.new] }}
        res << t {{ Set.new.delete(Bad.new).size }}
        res
        "##
    ));
}

// An `eql?` that answers true and deletes the key it has just accepted.
// CRuby goes on with the entry it deleted (a lookup answers the dead
// entry's value, a store writes into it and is lost, a removal counts the
// entry out twice); here the key is simply no longer there.
#[test]
fn hash_key_eql_deletes_the_key_it_accepts() {
    let v = run_test_no_result_check(
        r##"
        class A
          attr_accessor :act
          def hash = 1
          def eql?(o) = (act&.call; true)
        end
        res = []
        [0, 12].each do |pre|
          mk = -> do
            h = {}; pre.times { |i| h[i] = i }
            a = A.new; b = A.new
            h[a] = 1
            b.act = -> { b.act = nil; h.delete(a) }
            [h, a, b]
          end
          h, a, b = mk.()
          res << (h[b] == nil && h.size == pre)
          h, a, b = mk.()
          h[b] = 2
          res << (h.size == pre + 1 && h[b] == 2 && h.keys.last.equal?(b))
          h, a, b = mk.()
          res << (h.delete(b) == nil && h.size == pre)
          h, a, b = mk.()
          res << (h.key?(b) == false && h.size == pre)
        end
        res.all?
        "##,
    );
    assert!(v.as_bool());
}
