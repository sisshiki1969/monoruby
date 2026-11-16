extern crate monoruby;
use monoruby::tests::*;

// Issue #1956: the counts and indices #1930 left — `Range#min` / `#max`
// with a block, `Array#permutation`, the size limits of `min(n)` /
// `take(n)`, a Struct index, `Integer#round`'s keywords and
// `File.readlines`.

const T: &str = r##"
    def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
    class X; def to_int = "x"; end
    class Y; def to_int = 0; end
"##;

fn t(body: &str) {
    run_test_once(&format!("{T}\n[{body}]"));
}

#[test]
fn range_min_max_with_an_explicit_nil() {
    // An explicit `nil` count is not "no count" once there is a block or
    // the range cannot answer from its ends.
    t(r##"
        t { (1..3).min }, t { (1..3).max }, t { (1..3).min(2) }, t { (1..3).max(2) },
        t { (1..3).min(nil) }, t { (1..3).max(nil) },
        t { (1..3).min(nil) { |a, b| a <=> b } }, t { (1..3).max(nil) { |a, b| a <=> b } },
        t { (1..3).min(2) { |a, b| b <=> a } }, t { (1..3).max(2) { |a, b| b <=> a } },
        t { (1..3).min(-1) }, t { (1..3).max(-1) },
        t { (1..3).min(-1) { |a, b| a <=> b } }, t { (1..3).max(-1) { |a, b| a <=> b } },
        t { ("a"..."d").max(nil) }, t { ("a"..."d").max }, t { ("a"..."d").max(2) },
        t { ("a".."d").max(nil) }, t { (1.0..3.0).max(nil) }, t { (1.0..3.0).min(nil) },
        t { (1...3).max(nil) }, t { (1..).min(nil) }, t { (..3).max(nil) },
        t { (1..3).min(1.9) }, t { (1..3).max("1") }
    "##);
}

#[test]
fn min_max_size_limits() {
    // `rb_nmin_run`: "too big size" above LONG_MAX / 4 (/ 8 with `_by`),
    // then the buffer's own "array size too big".
    t(r##"
        t { (1..3).min(2**62) }, t { (1..3).max(2**62) },
        t { (1..3).min(2**62) { |a, b| a <=> b } }, t { (1..3).max(2**62) { |a, b| a <=> b } },
        t { (1..3).min(2**60) }, t { (1..3).max(2**60) },
        t { [1, 2, 3].min(nil) }, t { [1, 2, 3].max(nil) },
        t { [1, 2, 3].each_entry.min(nil) }, t { [1, 2, 3].each_entry.max(nil) },
        t { [1, 2, 3].min(2**62) }, t { [1, 2, 3].max(2**62) },
        t { [1, 2, 3].min(2**61) }, t { [1, 2, 3].min(2**61 - 1) }, t { [1, 2, 3].min(2**58) },
        t { [1, 2, 3].each_entry.min(2**62) }, t { [1, 2, 3].each_entry.max(2**62) },
        t { [1, 2, 3].each_entry.min_by(2**62) { |x| x } },
        t { [1, 2, 3].each_entry.max_by(2**60) { |x| x } },
        t { [1, 2, 3].min_by(2**57 + 1) { |x| x } },
        t { {a: 1}.min_by(2**62) { |k, v| v } }, t { {a: 1}.min(2**62) }
    "##);
}

#[test]
fn take_and_first_size_limits() {
    t(r##"
        t { [1, 2, 3].take(2**62) }, t { [1, 2, 3].first(2**62) },
        t { (1..3).take(2**62) }, t { (1..3).first(2**62) },
        t { {a: 1}.take(2**62) }, t { {a: 1}.first(2**62) },
        t { [1, 2, 3].each_entry.take(2**62) }, t { [1, 2, 3].each_entry.first(2**62) },
        t { [1, 2, 3].each_entry.take(2**60) }, t { [1, 2, 3].each_entry.first(2**60) },
        t { [1, 2, 3].each_entry.take(0) }, t { [1, 2, 3].each_entry.first(2) },
        t { [1, 2, 3].lazy.take(2**62).to_a }
    "##);
}

#[test]
fn permutation_with_an_explicit_nil() {
    t(r##"
        t { [1, 2, 3].permutation(nil).size }, t { [1, 2, 3].permutation(nil).to_a.size },
        t { [1, 2, 3].permutation.size }, t { [1, 2, 3].permutation(2).size },
        t { [1, 2, 3].permutation(nil).first }, t { [1, 2, 3].permutation(nil).inspect },
        t { [1, 2, 3].permutation.inspect }, t { [1, 2, 3].permutation(nil) { } },
        t { [1, 2, 3].permutation(X.new).size }
    "##);
}

#[test]
fn struct_index_conversion() {
    // `rb_struct_pos`: NUM2LONG for anything that is not a Symbol or a
    // String.
    t(r##"
        (S = Struct.new(:a, :b); s = S.new(1, 2); nil),
        t { s[5] }, t { s[-5] }, t { s[5] = 1 }, t { s[-5] = 1 },
        t { s[1.9] }, t { s[-1.2] }, t { s[Y.new] }, t { s[nil] }, t { s[Object.new] }.sub(/0x\h+/, "0x"),
        t { s[2**64] }, t { s[X.new] }, t { s[1e30] },
        t { s[1.9] = 7 }, t { s[Y.new] = 8 }, t { s[nil] = 1 }, t { s[X.new] = 1 }, s.to_a,
        t { s.dig(1.9) }, t { s.dig(Y.new) }, t { s.dig(nil) }, t { s.dig(X.new) },
        t { s.dig(9) }, t { s.dig(:zz) }, t { s.dig("a") },
        t { s.values_at(1.9, Y.new) }, t { s.values_at(nil) }, t { s.values_at(X.new) },
        t { s.values_at(9) }, t { s.values_at(-9) }, t { s.values_at(0..5) },
        t { s[:zz] }, t { s["zz"] }, t { s[:zz] = 1 },
        (f = S.new(1, 2).freeze; nil),
        t { f[0] = 1 }, t { f[:a] = 1 }, t { f[:zz] = 1 }, t { f[9] = 1 }, t { f[X.new] = 1 },
        (e = (s[:zz] rescue $!); [e.name, e.receiver.equal?(s)]),
        (e = (s["zz"] rescue $!); [e.name, e.receiver.equal?(s)])
    "##);
}

#[test]
fn integer_round_keywords() {
    // `int_round`: without digits the receiver is returned before any
    // keyword is looked at; with them, `half:` is checked even where the
    // result does not depend on it.
    t(r##"
        t { 12.round(half: :even, ndigits: 1) }, t { 12.round(half: :bogus) },
        t { 12.round(foo: 1) }, t { 12.round(1, foo: 1) }, t { 12.round(1, half: :bogus) },
        t { 12.round(-1, half: :bogus) }, t { 12.round(-1, foo: 1) },
        t { 15.round(-1, half: :even, foo: 1) }, t { 15.round(-1, half: "EVEN") },
        t { 15.round(-1, half: 1) }, t { 15.round(-1, half: nil) },
        t { 15.round(-1, half: "Up") }, t { 25.round(-1, half: :DOWN) },
        t { (2**70).round(foo: 1) }, t { (2**70).round(-1, foo: 1) },
        t { 12.floor(foo: 1) }, t { 12.ceil(half: :up) }, t { 12.truncate(half: :up) },
        t { 12.round(**{}) }, t { 12.round("1") }, t { 12.round(nil) }, t { 12.round(1.9) },
        t { 12.round(2**31) }, t { 12.round(-2**31 - 1) }, t { 12.round(2**30) }, t { 12.round(2**64) },
        t { 1.5.round(foo: 1) }, t { 1.5.round(half: :even, ndigits: 1) }, t { 1.5.round(half: :bogus) },
        t { 2.5.round(half: "EVEN") }, t { 2.5.round(half: 1) }, t { 2.5r.round(half: "down") },
        t { 1.5r.round(foo: 1) }, t { 1.5r.round(half: :bogus) }
    "##);
}

#[test]
fn file_readlines_is_io_readlines() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        path = "/tmp/monoruby_readlines_#{$$}_#{rand(1_000_000)}.txt"
        File.write(path, "a b\nc d\n\n\ne\n")
        res = [
          File.readlines(path), File.readlines(path, chomp: true),
          File.readlines(path, " "), File.readlines(path, 3),
          File.readlines(path, ""), File.readlines(path, "", chomp: true),
          File.readlines(path, nil), File.readlines(path, " ", 2),
          File.readlines(path, nil, 3),
          t { File.readlines(path, 0) },
          t { File.readlines(path, mode: "rb", encoding: "ASCII-8BIT").map { |l| l.encoding.to_s }.uniq },
          t { File.readlines }, t { File.readlines(path, {}) },
          File.method(:readlines).owner.inspect,
          File.open(path) { |f| f.readlines(nil, 3) },
        ]
        File.delete(path)
        res
        "##,
    );
}
