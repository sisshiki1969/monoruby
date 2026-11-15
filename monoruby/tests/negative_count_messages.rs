extern crate monoruby;
use monoruby::tests::*;

// Issue #1922: the ArgumentError raised for a negative count carried
// monoruby's own wording in `Array#first` / `last` / `take` / `drop`,
// `Range#first` and `Enumerable#first`. CRuby has three: the Array ones
// say "negative array size" (`ary_take_first_or_last_n`), `take` / `drop`
// and `Enumerable#first` "attempt to take / drop negative size", and
// `Range#first` "negative array size (or size too big)" (`rb_ary_new_capa`
// reached through `range_first`).

const MSG: &str = r##"
    def msg
      yield
    rescue ArgumentError, RangeError => e
      "#{e.class}: #{e.message}"
    end
"##;

#[test]
fn negative_count_array() {
    run_test_with_prelude(
        r##"
        %i[first last pop shift take drop sample].map do |m|
          [m, msg { [1, 2, 3].send(m, -1) }, msg { [1, 2, 3].send(m, -1.9) }]
        end
        "##,
        MSG,
    );
}

#[test]
fn negative_count_range_first() {
    run_test_with_prelude(
        r##"
        [1..3, 1.., "a".."c", 1.0..3.0, 1...1].map do |r|
          msg { r.first(-1) }
        end + [msg { (1..3).last(-1) }]
        "##,
        MSG,
    );
}

#[test]
fn negative_count_enumerable_first() {
    run_test_with_prelude(
        r##"
        o = Object.new
        o.extend(Enumerable)
        def o.each
          yield 1
          yield 2
        end
        [
          o,
          [1].each_slice(1),
          [1, 2].each,
          { a: 1 },
          Struct.new(:a, :b).new(1, 2),
          Set[1, 2],
          [1, 2].lazy,
          Enumerator.produce(1) { |x| x + 1 },
          1.step(10, 3),
          (1..10).step(3),
          1.0.step(10.0, 3.0),
        ].map { |e| [msg { e.first(-1) }, msg { e.first(-2**64) }] }
        "##,
        MSG,
    );
}

#[test]
fn negative_count_arithmetic_sequence_last() {
    run_test_with_prelude(
        r##"
        [1.step(10, 3), (1..10).step(3), 1.0.step(10.0, 3.0)].map do |e|
          msg { e.last(-1) }
        end
        "##,
        MSG,
    );
}
