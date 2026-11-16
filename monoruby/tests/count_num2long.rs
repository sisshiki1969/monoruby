extern crate monoruby;
use monoruby::tests::*;

// Issue #1930: a C method reads a count with NUM2LONG before it looks at
// the sign, so a Bignum — negative or not — is a RangeError, a Float
// truncates, and `nil` has a message of its own.

const PRELUDE: &str = r##"
    class ToI; def initialize(v) = @v = v; def to_int = @v; end
    def t = (yield rescue [$!.class, $!.message])
"##;

fn each_count(body: &str) {
    run_test_once(&format!(
        r##"
        {PRELUDE}
        e = [1, 2, 3].each_entry
        counts = [-(2**64), 2**64, nil, true, "1", 1.9, -0.5, 1e30, -1e30,
                  Float::INFINITY, Float::NAN, ToI.new(2), ToI.new("x"),
                  ToI.new(2**64), ToI.new(1.5), Rational(5, 2), -1, 0, :a]
        counts.map do |v|
          [{body}]
        end
        "##
    ));
}

#[test]
fn enumerable_counts() {
    each_count(
        r##"
        t { e.take(v) }, t { e.drop(v) }, t { e.first(v) },
        t { e.each_slice(v).to_a }, t { e.each_cons(v).to_a },
        t { e.min(v) }, t { e.max(v) },
        t { e.min_by(v) { |x| x } }, t { e.max_by(v) { |x| -x } },
        t { {a: 1}.take(v) }, t { {a: 1}.min(v) }, t { Set[1, 2].take(v) },
        t { Struct.new(:a).new(1).take(v) }
        "##,
    );
}

#[test]
fn cycle_counts() {
    // The Enumerator comes first: the count is converted when it is
    // iterated or sized.
    each_count(
        r##"
        t { e.cycle(v).first(4) }, t { e.cycle(v).size },
        t { r = []; e.cycle(v) { |x| r << x; break if r.size > 3 }; r },
        t { e.cycle(v).class },
        t { [1, 2].cycle(v).first(3) }, t { [1, 2].cycle(v).size },
        t { r = []; [1, 2].cycle(v) { |x| r << x; break if r.size > 3 }; r },
        t { [].cycle(v) { } }, t { [].cycle(v).size }, t { [1, 2].cycle(v).class },
        t { (1..3).cycle(v).first(2) }
        "##,
    );
}

#[test]
fn array_combination_counts() {
    each_count(
        r##"
        t { [1, 2, 3].combination(v).to_a }, t { [1, 2, 3].combination(v).size },
        t { [1, 2, 3].permutation(v).first(2) },
        t { v.nil? ? nil : [1, 2, 3].permutation(v).size },
        t { [1, 2].repeated_permutation(v).first(3) },
        t { [1, 2].repeated_permutation(v).size },
        t { [1, 2].repeated_combination(v).first(3) },
        t { [1, 2].repeated_combination(v).size },
        t { [1, 2, 3].take(v) }, t { [1, 2, 3].drop(v) }, t { [1, 2, 3].first(v) },
        t { [1, 2, 3].last(v) }, t { [1, 2, 3].min(v) }, t { [1, 2, 3].max(v) },
        t { [1, 2, 3].pop(v) }, t { [1, 2, 3].shift(v) }
        "##,
    );
}

#[test]
fn lazy_and_range_counts() {
    each_count(
        r##"
        t { e.lazy.take(v).to_a }, t { e.lazy.drop(v).to_a }, t { e.lazy.first(v) },
        t { e.lazy.each_slice(v).to_a }, t { e.lazy.each_cons(v).to_a },
        t { v.nil? ? nil : (1..3).min(v) }, t { v.nil? ? nil : (1..3).max(v) },
        t { (1..3).min(v) { |a, b| a <=> b } }, t { (1..3).max(v) { |a, b| a <=> b } },
        t { (1..3).take(v) }, t { (1..3).drop(v) }, t { (1..3).first(v) },
        t { (1..3).last(v) }, t { (1..3).each_slice(v).to_a },
        t { (1..3).min_by(v) { |x| x } },
        t { ("a".."c").first(v) }, t { (1..).first(v) }, t { (1.0..3.0).first(v) },
        t { 1.step(10, 2).first(v) }, t { 1.step(10, 2).last(v) },
        t { 1.step(10, 2).take(v) }
        "##,
    );
}

#[test]
fn range_first_iterates_even_for_zero() {
    // `range_first` runs `#each` whatever the count, so a range that
    // cannot be iterated is a TypeError for `first(0)` too.
    run_test_once(
        r##"
        def t = (yield rescue [$!.class, $!.message])
        [
          t { (1.0..3.0).first(0) }, t { (1.0..3.0).first(1) }, t { (1.0..3.0).first },
          t { (1.0..3.0).first(-1) }, t { (1.0..3.0).first("a") },
          t { (Rational(1)..Rational(3)).first(0) },
          t { (Time.at(0)..Time.at(1)).first(0) },
          t { ("a".."c").first(0) }, t { (1..).first(0) }, t { (1..3).first(0) },
          t { (nil..1).first(0) }, t { (1.0..3.0).take(0) },
        ]
        "##,
    );
}

#[test]
fn integer_digits_radix() {
    run_test_once(&format!(
        r##"
        {PRELUDE}
        [-1, -(2**64), 2**64, 0, 1, 1.5, -0.5, nil, "2", ToI.new(2), ToI.new("x"),
         Float::INFINITY, 1e30].map do |v|
          [t {{ 12.digits(v) }}, t {{ (2**70).digits(v).size }}, t {{ -12.digits(v) }}]
        end
        "##
    ));
}

#[test]
fn random_bytes_size() {
    run_test_once(&format!(
        r##"
        {PRELUDE}
        [-1, -0.5, -1.5, 2.5, 0, 3, -(2**64), 2**64, nil, "1", ToI.new(2),
         Float::NAN, 1e30, 2**63, -(2**63), 1.0e19, -1.0e19].map do |v|
          [t {{ Random.bytes(v).size }}, t {{ Random.new(1).bytes(v).size }},
           t {{ Random.urandom(v).size }}]
        end
        "##
    ));
}

#[test]
fn io_read_lengths() {
    run_test_once(&format!(
        r##"
        {PRELUDE}
        big = -(2**64)
        path = "/tmp/monoruby_io_read_lengths.txt"
        File.write(path, "[package]\nname = 1\n")
        f = File.open(path)
        res = [
          t {{ f.readpartial(-1) }}, t {{ f.readpartial(big) }},
          t {{ f.sysread(-1) }}, t {{ f.sysread(big) }},
          t {{ f.read(-1) }}, t {{ f.read_nonblock(-1) }},
          t {{ f.pread(-1, 0) }}, t {{ f.pread(big, 0) }}, t {{ f.pread(2**64, 0) }},
          t {{ f.pread(2**63, 0) }}, t {{ f.pread(-(2**63) - 1, 0) }},
          t {{ f.pread(-(2**63), 0) }},
          t {{ f.pread(1, big) }}, t {{ f.pread(0, 0) }},
          t {{ f.each_line(-0.5).first }}, t {{ f.each_line(0).first }},
          t {{ f.each_line(0.5).first }}, t {{ f.each_line(0.5).class }},
          t {{ f.each_line(big).first.class }}, t {{ f.each_line("\n", 0.2).first }},
          t {{ f.rewind; f.each_line(1.5).first.size }},
          t {{ f.rewind; f.each_line(-1.5).first.size > 1 }},
          t {{ f.rewind; f.each_line(nil, 2).first.size }},
        ]
        f.close
        File.delete(path)
        res
        "##
    ));
}

#[test]
fn integer_pow_with_a_bignum_modulus() {
    run_test(
        r##"
        def t = (yield rescue [$!.class, $!.message])
        big = -(2**64)
        pbig = 2**64
        [
          2.pow(3, big), 2.pow(3, pbig), 2.pow(3, -5), 2.pow(3, 5),
          2.pow(pbig, pbig + 1), 3.pow(2**70, big), (-7).pow(3, big - 1),
          (2**70).pow(3, big), (2**70).pow(3, -5), (-(2**70)).pow(5, 7),
          7.pow(0, 1), 7.pow(0, -1), 7.pow(5, 1), 0.pow(0, 5), 0.pow(3, pbig),
          12345678901234567890.pow(98765432109876543210, 1000000007),
          t { 2.pow(3, 0) }, t { 2.pow(-3, 5) }, t { 2.pow(-3, 1.5) },
          t { 2.pow(3, 1.5) }, t { 2.pow(1.5, 3) }, t { 2.pow(big, 5) },
          t { 2.pow(-1, 0) },
        ]
        "##,
    );
}

#[test]
fn to_int_answering_a_non_integer() {
    // `rb_to_integer`'s message: "to Integer", the argument's class, and
    // what its `to_int` gave.
    run_test_once(&format!(
        r##"
        {PRELUDE}
        class N; def to_int = nil; end
        s = Struct.new(:a).new(1)
        [t {{ Integer.sqrt(ToI.new("x")) }}, t {{ Integer.sqrt(N.new) }},
         t {{ Integer.try_convert(ToI.new("x")) }}, Integer.try_convert(N.new),
         t {{ s.deconstruct_keys([ToI.new("x")]) }},
         t {{ 12.digits(ToI.new("x")) }}, t {{ 12.round(ToI.new(1.5)) }},
         t {{ [1, 2].first(ToI.new(nil)) }}]
        "##
    ));
}

#[test]
fn hash_min_by_with_a_count() {
    run_test(
        r##"
        h = { a: 3, b: 1, c: 2 }
        [h.min_by(2) { |k, v| v }, h.min_by { |k, v| v }, h.min_by(0) { |k, v| v },
         h.min_by(nil) { |k, v| v }, h.min_by(2).size, h.min_by.size,
         h.min_by(2, &:last), ({}).min_by { 1 }, ({}).min_by(2) { 1 },
         (h.min_by(-1) { 1 } rescue [$!.class, $!.message]),
         (h.min_by(-(2**64)) { 1 } rescue [$!.class, $!.message]),
         (h.min_by(1, 2) { 1 } rescue $!.class)]
        "##,
    );
}

#[test]
fn counts_at_the_edge_of_a_long() {
    // 2**62 … 2**63 - 1 are Bignums here and longs in C.
    run_test_once(&format!(
        r##"
        {PRELUDE}
        e = [1, 2, 3].each_entry
        [2**62, 2**63 - 1, 2**63, -(2**62) - 1, -(2**63), -(2**63) - 1,
         ToI.new(2**63 - 1), ToI.new(2**63), 4611686018427387904.0].map do |n|
          [t {{ e.each_slice(n).to_a }}, t {{ e.drop(n) }}, t {{ e.each_cons(n).to_a }},
           t {{ e.lazy.take(n).to_a }}]
        end
        "##
    ));
}

#[test]
fn line_reader_limits() {
    // `extract_getline_args`: a lone argument with #to_str is the
    // separator, a nil limit is no limit, anything else is NUM2LONG; a
    // zero limit is rejected once the stream is known to be open.
    run_test_once(&format!(
        r##"
        {PRELUDE}
        class ToS; def to_str = " "; end
        path = "/tmp/monoruby_line_reader_limits.txt"
        File.write(path, "a b\nc d\n")
        f = -> {{ File.open(path) }}
        closed = f.(); closed.close
        res = [
          f.().each_line(ToS.new).to_a, f.().each_line(2.9).to_a,
          f.().each_line(ToI.new(3)).to_a, f.().each_line(" ", ToI.new(1)).to_a,
          f.().each_line(" ", nil).to_a, f.().each_line(nil, nil).to_a,
          f.().each_line(2, chomp: true).to_a, f.().each_line(ToI.new(2)).first(2),
          t {{ f.().each_line(0).to_a }}, t {{ f.().each_line(0.5).to_a }},
          t {{ f.().each_line(ToI.new(0)).to_a }}, t {{ f.().each_line(nil, 0).to_a }},
          t {{ f.().each_line(-(2**64)).to_a }}, t {{ f.().each_line(:a).to_a }},
          t {{ f.().each_line(" ", "x").to_a }},
          t {{ closed.each_line(0) {{ }} }}, t {{ closed.each_line(:a) {{ }} }},
          f.().gets(" ", nil), f.().gets(nil, nil), f.().gets(1.9), f.().gets(-1.5),
          f.().gets(ToI.new(2)), f.().readlines(" ", nil), f.().readline(2.5),
          t {{ f.().gets(-(2**64)) }}, t {{ f.().gets(" ", :a) }},
          t {{ f.().readlines(1e30) }}, t {{ f.().readlines(0.9) }},
          IO.readlines(path, " ", nil), File.foreach(path, nil, nil).to_a,
          t {{ IO.readlines(path, 0.5) }}, t {{ File.foreach(path, 0.5).to_a }},
          File.readlines(path, " ", nil), File.readlines(path, ToS.new),
          File.readlines(path, 2.9), File.readlines(path, " ", -1),
          t {{ File.readlines(path, 0) }}, t {{ File.readlines(path, " ", 0.5) }},
          t {{ File.readlines(path, -(2**64)) }},
          t {{ File.readlines("/nonexistent", :a) }},
        ]
        File.delete(path)
        res
        "##
    ));
}
