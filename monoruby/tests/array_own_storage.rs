//! The Array methods written in Ruby (builtins/array.rb) read the
//! receiver's own storage, as CRuby's C does with `RARRAY_LEN` /
//! `RARRAY_AREF`: `size`, `length`, `[]` and `[]=` redefined by a
//! subclass, or on `Array` itself, are not consulted (issue #2178).

extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r##"
    class Lying < Array
      def size = 1
      def length = 1
      def [](*) = :x
      def []=(*)
        raise "[]= called"
      end
    end
    class OwnEach < Array
      def each = yield(:e)
    end
    def try
      yield
    rescue StandardError => e
      [e.class, e.message]
    end
"##;

#[test]
fn iterators_ignore_overridden_size_and_index() {
    run_test_with_prelude(
        r##"
        a = Lying.new([1, 2, 3])
        r = []
        a.each { |x| r << x }
        a.reverse_each { |x| r << x }
        a.each_with_index { |x, i| r << [x, i] }
        a.each_index { |i| r << i }
        [
          r,
          a.each.size, a.reverse_each.size, a.each_index.size, a.map.size,
          a.map!.size, a.select.size, a.reject.size, a.cycle(2).size,
          # Enumerable's in CRuby, whose Enumerator size does ask `size`.
          a.each_with_index.size, a.partition.size, a.flat_map.size,
          a.filter_map.size,
          a.each.to_a, a.reverse_each.to_a, a.each_index.to_a,
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn collectors_ignore_overridden_size_and_index() {
    run_test_with_prelude(
        r##"
        a = Lying.new([1, 2, 3])
        b = Lying.new([1, 2, 3])
        b.map! { |x| x * 2 }
        [
          a.map { |x| x + 1 },
          a.collect { |x| x },
          b.to_a,
          a.select { |x| x > 1 },
          a.filter { true },
          a.reject { |x| x > 1 },
          a.partition { |x| x.odd? },
          a.flat_map { |x| [x, x] },
          a.filter_map { |x| x * 2 if x.odd? },
          a.tally,
          a.drop_while { |x| x < 2 },
          a.drop_while { |x| x < 2 }.class,
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn searches_ignore_overridden_size_and_index() {
    run_test_with_prelude(
        r##"
        a = Lying.new([1, 2, 3, 4])
        [
          a.bsearch { |x| x >= 3 },
          a.bsearch { |x| 3 <=> x },
          a.bsearch_index { |x| x >= 3 },
          a.bsearch_index { |x| 9 <=> x },
          a.rindex(3),
          a.rindex { |x| x < 3 },
          a.at(1), a.at(-1), a.at(9),
          a.dig(2),
          Lying.new([[1, [2]]]).dig(0, 1, 0),
          a.values_at(0, 2, 1..2, -1, 7, 2.., ..0),
          a.fetch_values(0, 3),
          a.fetch_values(0, 9, -9) { |i| i * 2 },
          Lying.new([[1, 2], [3, 4]]).assoc(3),
          Lying.new([[1, 2], [3, 4]]).rassoc(4),
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn combinatorics_ignore_overridden_size_and_index() {
    run_test_with_prelude(
        r##"
        a = Lying.new([1, 2, 3])
        r = []
        a.cycle(2) { |x| r << x }
        [
          r,
          a.combination(2).to_a, a.combination(2).size,
          a.permutation(2).to_a, a.permutation.to_a.size,
          a.permutation.size, a.permutation(2).size,
          a.repeated_permutation(2).to_a, a.repeated_permutation(2).size,
          a.repeated_combination(2).to_a, a.repeated_combination(2).size,
        ]
        "##,
        PRELUDE,
    );
}

/// Enumerable's methods reach the elements through `Array#each`.
#[test]
fn enumerable_over_each_ignores_overridden_index() {
    run_test_with_prelude(
        r##"
        a = Class.new(Array) { def [](*) = :x }.new([1, 2, 3])
        [
          a.each_slice(2).to_a,
          a.each_with_object([]) { |x, m| m << x },
          a.min_by { |x| -x },
          a.take_while { |x| x < 3 },
          a.lazy.map { |x| x * 2 }.to_a,
          a.each_entry.to_a,
        ]
        "##,
        PRELUDE,
    );
}

/// `cycle`, `assoc`, `rassoc` and `combination` are Array's own in
/// CRuby and walk the storage: an overridden `each` is not called.
#[test]
fn own_methods_ignore_overridden_each() {
    run_test_with_prelude(
        r##"
        b = OwnEach.new([1, 2, 3])
        r = []
        b.cycle(1) { |x| r << x }
        [
          r,
          OwnEach.new([[1, 2], [3, 4]]).assoc(3),
          OwnEach.new([[1, 2], [3, 4]]).rassoc(4),
          b.combination(1).to_a,
          b.repeated_combination(1).to_a,
        ]
        "##,
        PRELUDE,
    );
}

/// `size` and `[]` redefined on Array itself, while a compiled `each`
/// is in use, and put back.
#[test]
fn iterators_ignore_size_and_index_redefined_on_array() {
    run_test_with_prelude(
        r##"
        a = [1, 2, 3]
        before = walk(a)
        class Array
          alias_method :__saved_size, :size
          alias_method :__saved_aref, :[]
          def size = 1
          def [](*) = :x
        end
        during = begin
          walk(a)
        ensure
          class Array
            alias_method :size, :__saved_size
            alias_method :[], :__saved_aref
          end
        end
        [before, during, walk(a)]
        "##,
        r##"
        def walk(a)
          r = 0
          a.each { |x| r += x }
          a.reverse_each { |x| r += x * 10 }
          a.each_with_index { |x, i| r += x * i * 100 }
          a.map { |x| r += x * 1000; x }
          a.select { |x| r += x * 10000; true }
          r
        end
        50.times { walk([1, 2, 3]) }
        "##,
    );
}

/// The length is read again as the walk goes: a block that shrinks the
/// receiver ends it (or, for `reverse_each`, moves it down).
#[test]
fn block_shrinking_the_receiver() {
    run_test(
        r##"
        res = []
        a = [1, 2, 3, 4]; r = []
        a.reverse_each { |x| r << x; a.clear if x == 3 }
        res << r
        a = [1, 2, 3, 4, 5, 6]; r = []
        a.reverse_each { |x| r << x; a.pop; a.pop }
        res << r
        a = [1, 2, 3]; r = []
        a.reverse_each { |x| r << x; a << 9 if r.size < 3 }
        res << r
        a = [1, 2, 3, 4]
        res << a.map { |x| a.pop; x }
        a = [1, 2, 3, 4]
        res << a.map { |x| a << 0 if x == 1; a.pop if x == 2; x }
        a = [1, 2, 3, 4]
        a.map! { |x| a.pop; x * 2 }
        res << a
        a = [1, 2, 3]; r = []
        a.cycle { |x| r << x; a.clear if r.size == 5 }
        res << r
        a = [1, 2, 3]; r = []
        a.cycle(2) { |x| r << x; a.pop if r.size == 1 }
        res << r
        res << [1, 2].cycle(-1) { raise } << [].cycle { raise } << [1].cycle(0) { raise }
        res
        "##,
    );
}

/// `at` and `dig` take what `NUM2LONG` takes: a Range is not an index.
#[test]
fn at_and_dig_convert_the_index_to_an_integer() {
    run_test_with_prelude(
        r##"
        o = Object.new
        def o.to_int = 2
        a = [1, 2, 3]
        [
          a.at(1.9), a.at(o), a.at(-1), a.at(2**62), a.dig(1.9), a.dig(o),
          try { a.at(0..1) }, try { a.at("1") }, try { a.at(nil) },
          try { a.at(2**70) }, try { a.dig(0..1) }, try { a.dig(:a) },
          try { a.values_at("1") }, try { a.values_at(2**70) },
          a.values_at(1.9, o),
        ]
        "##,
        PRELUDE,
    );
}

/// One compiled `at` site, Integers first and then everything else:
/// the inline read is guarded on the index being a Fixnum.
#[test]
fn compiled_at_with_changing_index_classes() {
    run_test_with_prelude(
        r##"
        a = [10, 20, 30]
        o = Object.new
        def o.to_int = 1
        r = []
        40.times { |i| r << get(a, i % 4) }
        [1.5, o, -1, 2**70, 0..1, nil, 2, "x"].each { |i| r << try { get(a, i) } }
        40.times { |i| r << get(a, i % 4) }
        r
        "##,
        &format!("{PRELUDE}\n def get(a, i) = a.at(i)\n"),
    );
}

/// The primitives are private: nothing outside builtins/array.rb sees
/// them, and they are not part of what an Array responds to.
#[test]
fn primitives_are_private() {
    run_test(
        r##"
        a = [1, 2, 3]
        [:__size, :__aref, :__aset].map do |m|
          [a.respond_to?(m), a.public_methods.include?(m), Array.public_method_defined?(m)]
        end
        "##,
    );
}
