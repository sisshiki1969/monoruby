//! Two groups of argument handling that an in-process run can check
//! against the oracle.
//!
//! - Reading lines from a stream whose encoding is not
//!   ASCII-compatible (#2220): the default separator is the encoding's
//!   own line feed (`"\n\0"` in UTF-16LE, not the byte `"\n"`), a
//!   separator is searched for at a character boundary, and one in
//!   another encoding is converted or refused the way CRuby's
//!   `rb_io_getline` prepares it.
//! - `nil`, a Float or an object where CRuby converts an argument with
//!   `NUM2LONG` / `StringValue` / a keyword check (#2240):
//!   `String#bytesplice`, `#unicode_normalize`, `Range#sum`,
//!   `Float#rationalize`, `Hash.new(capacity:)`, `String.new(capacity:)`,
//!   `ObjectSpace.each_object` / `count_objects`, `Time.at`, `Dir.glob`,
//!   `MatchData#[]` and `format` with a Complex.

use monoruby::tests::*;

/// `"a\n" "b\n"` in UTF-16LE, then CR LF and a paragraph break.
#[test]
fn lines_of_a_utf16_stream() {
    run_test_once(
        r####"
path = "/tmp/mr_wide_lines_#{$$}.txt"
File.binwrite(path, "a\x00\n\x00b\x00\n\x00")
res = []
t = ->(n, &b) { res << (begin; [n, b.call].inspect; rescue Exception => e; [n, e.class, e.message].inspect; end) }
t.(1) { File.open(path, "rb:utf-16le") { |f| f.readlines.map(&:b) } }
t.(2) { File.open(path, "rb:utf-16le") { |f| f.each_line.count } }
t.(3) { File.open(path, "rb:utf-16le") { |f| [f.readlines.size, f.lineno] } }
t.(4) { File.open(path, "rb:utf-16le") { |f| f.readlines("\x00") } }
t.(5) { File.open(path, "rb:utf-16le:utf-8") { |f| f.readlines } }
t.(6) { File.open(path, "rb:utf-16le") { |f| [f.gets, f.gets, f.gets, f.gets].map { _1&.b } } }
t.(7) { File.open(path, "rb:utf-16le") { |f| f.readlines(chomp: true).map(&:b) } }
t.(8) { File.open(path, "rb:utf-16le") { |f| f.readlines("\n") } }
t.(9) { File.open(path, "rb:utf-16le") { |f| f.readlines("\n".encode("utf-16le")).map(&:b) } }
t.(10) { File.open(path, "rb:utf-16le") { |f| f.readlines("b".encode("utf-16le")).map(&:b) } }
t.(11) { File.open(path, "rb:utf-16le") { |f| f.readlines(nil).map(&:b) } }
t.(12) { File.open(path, "rb:utf-16le") { |f| f.readlines("").map(&:b) } }
t.(13) { File.open(path, "rb:utf-16le") { |f| f.readlines(3).map(&:b) } }
t.(14) { File.open(path, "rb:utf-16le") { |f| f.readlines("\n".encode("utf-16be")).map(&:b) } }
t.(15) { File.readlines(path, mode: "rb:utf-16le").map(&:b) }
t.(16) { File.foreach(path, mode: "rb:utf-16le").map(&:b) }
t.(17) { File.readlines(path, "\x00", mode: "rb:utf-16le").map(&:b) }
t.(18) { File.readlines(path, chomp: true, mode: "rb:utf-16le").map(&:b) }
t.(19) { File.open(path, "rb:utf-16le") { |f| f.each_line("\x00").to_a } }
t.(20) { File.open(path, "rb:utf-16le") { |f| f.gets("\x00") } }
t.(21) { File.open(path, "rb:utf-16le") { |f| f.readline("\x00") } }
t.(22) { File.open(path, "r:utf-8:utf-16le") { |f| f.readlines.map(&:b) } }
t.(23) { File.open(path, "r:utf-8:utf-16le") { |f| f.readlines("\n") } }
t.(24) { File.open(path, "r:utf-8") { |f| f.readlines("\n".encode("utf-16le")) } }
t.(25) { File.open(path, "r:utf-8") { |f| f.readlines("あ".encode("euc-jp")) } }
t.(26) { File.open(path, "r:utf-8") { |f| f.readlines("a".encode("euc-jp")).size } }
t.(27) { File.open(path, "r:euc-jp") { |f| f.readlines("あ").size } }
t.(28) { File.open(path, "rb") { |f| f.readlines("あ").size } }
t.(29) { File.open(path, "r:utf-8") { |f| f.readlines("あ".b).size } }
t.(30) { File.open(path, "r:utf-8") { |f| f.readlines("\xff".b).size } }
t.(31) { File.open(path, "r:us-ascii") { |f| f.readlines("あ").size } }
t.(32) { File.open(path, "rb:utf-16le") { |f| f.readlines("", chomp: true).map(&:b) } }
t.(33) { File.open(path, "rb:utf-16le") { |f| f.readlines("a".b) } }
t.(34) { File.open(path, "rb:utf-16le") { |f| f.readlines("a".force_encoding("us-ascii")) } }
t.(35) { File.open(path, "rb:utf-32le") { |f| f.readlines.map(&:b) } }
t.(36) { File.open(path, "rb:utf-16be") { |f| f.readlines.map(&:b) } }
t.(37) { $/ = "b"; File.open(path, "rb:utf-16le") { |f| f.readlines.map(&:b) } }
$/ = "\n"
t.(38) { File.open(path, "rb:utf-16le") { |f| f.readlines.map(&:b) } }
t.(39) { File.open(path, "rb:utf-16le") { |f| f.gets(nil, 3).b } }
t.(40) { File.open(path, "rb:utf-16le") { |f| f.gets(3).b } }
t.(41) { File.open(path, "rb:utf-16le") { |f| f.gets(1).b } }
File.binwrite(path, "a\x00\r\x00\n\x00b\x00\n\x00\n\x00c\x00")
t.(42) { File.open(path, "rb:utf-16le") { |f| f.readlines(chomp: true).map(&:b) } }
t.(43) { File.open(path, "rb:utf-16le") { |f| f.readlines("").map(&:b) } }
t.(44) { File.open(path, "rb:utf-16le") { |f| f.readlines("", chomp: true).map(&:b) } }
t.(45) { File.open(path, "rb:utf-16le") { |f| f.each_line(chomp: true).map(&:b) } }
t.(46) { File.open(path, "r:utf-16le") { |f| f.readlines(chomp: true).map(&:b) } }
t.(47) { File.open(path, "rb:utf-16le") { |f| f.readline(chomp: true).b } }
File.binwrite(path, "\n\x0a\x00\x0a\n\x00")
t.(49) { File.open(path, "rb:utf-16le") { |f| f.readlines.map(&:b) } }
File.delete(path)
res
"####,
    );
}

/// UTF-16BE, and a separator given in another encoding than the
/// stream's (converted when the stream transcodes, refused when the
/// two cannot be compared).
#[test]
fn separators_in_another_encoding() {
    run_test_once(
        r####"
path = "/tmp/mr_wide_lines_b_#{$$}.txt"
res = []
t = ->(n, &b) { res << (begin; [n, b.call].inspect; rescue Exception => e; [n, e.class, e.message].inspect; end) }
File.binwrite(path, "\x00a\x00\r\x00\n\x00b\x00\n")
t.(1) { File.open(path, "rb:utf-16be") { |f| f.readlines(chomp: true).map(&:b) } }
t.(2) { File.open(path, "rb:utf-16be:utf-8") { |f| f.readlines(chomp: true).map(&:b) } }
t.(3) { File.open(path, "rb:utf-16be") { |f| f.readlines.map(&:encoding) } }
t.(4) { File.open(path, "rb:utf-16be") { |f| f.each_line.map(&:b) } }
t.(5) { File.open(path, "rb:utf-16be") { |f| f.each_line("b".encode("utf-16be"), chomp: true).map(&:b) } }
t.(6) { File.open(path, "rb:utf-16be") { |f| [f.readline.b, f.lineno, $., f.eof?] } }
t.(7) { File.open(path, "rb:utf-16be") { |f| f.gets; $_.b } }
t.(8) { IO.readlines(path, mode: "rb:utf-16be", chomp: true).map(&:b) }
t.(9) { IO.foreach(path, "b".encode("utf-16be"), mode: "rb:utf-16be").map(&:b) }
t.(10) { IO.foreach(path, "b", mode: "rb:utf-16be").map(&:b) }
t.(11) { File.open(path, "rb:utf-16be:utf-8") { |f| f.readlines("b") } }
t.(12) { File.open(path, "rb:utf-16be:utf-8") { |f| f.readlines("b".encode("utf-16be")) } }
t.(13) { File.open(path, "rb:utf-16be:utf-8") { |f| f.readlines("b".encode("euc-jp")) } }
File.binwrite(path, "\xa4\xa2x\xa4\xa4y\n")
t.(14) { File.open(path, "r:euc-jp:utf-8") { |f| f.readlines("い") } }
t.(15) { File.open(path, "r:euc-jp:utf-8") { |f| f.readlines("い", chomp: true) } }
t.(16) { File.open(path, "r:euc-jp:utf-8") { |f| f.readlines("い".encode("euc-jp")) } }
t.(17) { File.open(path, "r:euc-jp") { |f| f.readlines("い".encode("euc-jp")).map(&:b) } }
t.(18) { File.open(path, "r:euc-jp") { |f| f.readlines("x").map(&:b) } }
t.(19) { File.open(path, "r:euc-jp:utf-16le") { |f| f.readlines("x".encode("utf-16le")).map(&:b) } }
t.(20) { File.open(path, "r:euc-jp:utf-16le") { |f| f.readlines.map(&:b) } }
t.(21) { File.open(path, "r:euc-jp:utf-16le") { |f| f.readlines(chomp: true).map(&:b) } }
t.(22) { File.open(path, "r:euc-jp:utf-16le") { |f| f.readlines("x") } }
$/ = "\n"
File.delete(path)
res
"####,
    );
}

const CONVERSIONS: &str = r####"
exprs = ['"abc".bytesplice(nil, 1, "x")', '"abc".bytesplice(1.9, 1, "x")', '"abc".unicode_normalize(nil)',
 '(1..3).sum(nil)', '1.5.rationalize(nil)', 'Hash.new(capacity: nil)', 'Hash.new(capacity: "a")',
 'String.new(capacity: nil)', 'ObjectSpace.each_object(nil) {}.class', 'ObjectSpace.count_objects(nil)',
 'Time.at(0, 0, nil)', 'Dir.glob("/nonexistent*", flags: nil)', '"abc".match(/(b)(c)/)[1.9..]',
 '"%f" % 1i',
 # extras
 '"abc".bytesplice("a", 1, "x")', '"abc".bytesplice(0..1, "xyz", 1..2)', '"abc".bytesplice(0..1, "xyz", 1)', '"abc".bytesplice(0..1, "xyz", nil)',
 '"abc".bytesplice(0, 1, "xyz", 1, 1)', '"abc".bytesplice(0, 1, "xyz", nil, 1)', '"abc".bytesplice(0, 1, "xyz", 1.5, 1)', '"abc".bytesplice(1.5, "x")', '"abc".bytesplice(nil, "x")', '"abc".bytesplice(0.5, 1.5, "xyz", 1, 1)',
 '"abc".bytesplice(0, nil, "x")', '"abc".bytesplice(0, 1.9, "x")', '"abc".bytesplice(0, 1, "xyz", 1, nil)',
 '"abc".unicode_normalize(1)', '"abc".unicode_normalize("nfc")', '"abc".unicode_normalize(:foo)', '"abc".unicode_normalize!(nil)', '"abc".unicode_normalized?(nil)', '"abc".unicode_normalize("x")',
 '(1..3).sum(nil) {|x| x}', '[1,2].sum(nil)', '(1..3).sum("a")', '(1..3).sum(1.5)', '(1..3).sum(:a)', '(1.0..3.0).sum(nil)', '(1..3).each_slice(2).sum(nil)', '(1...1).sum(nil)', '(1..3).sum(Object.new)', '(1..3).sum([])', '("a".."c").sum(nil)', '(1..3).sum(1r)', '(1..3).sum(1i)', '(1..2**70).sum(nil)',
 '1.5.rationalize(nil)', '1.5.rationalize("a")', '1.5.rationalize(0.1)', '1.5.rationalize(1r/100)', '1.rationalize(nil)', '1.5r.rationalize(nil)', '1.5.rationalize(Object.new)', '0.3.rationalize(-0.01)', 'nil.rationalize(nil)', '1i.rationalize(nil)', '1.5.to_r.rationalize("a")',
 'Hash.new(capacity: 1.5)', 'Hash.new(capacity: -1)', 'Hash.new(capacity: 2**70)', 'Hash.new(1, capacity: nil)', 'Hash.new(capacity: Object.new)', 'Hash.new(capacity: 3)',
 'String.new(capacity: "a")', 'String.new(capacity: 1.5)', 'String.new(capacity: -1)', 'String.new(capacity: 2**70)', 'String.new("a", capacity: nil)', 'String.new(capacity: Object.new)', 'String.new(encoding: nil)', 'String.new(encoding: nil, capacity: nil)',
 'ObjectSpace.each_object(1) {}.class', 'ObjectSpace.each_object("a") {}.class', 'ObjectSpace.each_object(nil).class', 'ObjectSpace.each_object(Comparable) {}.class',
 'ObjectSpace.count_objects(1)', 'ObjectSpace.count_objects("a")', 'ObjectSpace.count_objects({}).class', 'ObjectSpace.count_objects([])',
 'Time.at(0, 0, :foo)', 'Time.at(0, 0, "ms")', 'Time.at(0, 0, 1)', 'Time.at(0, 0, :ms).to_i', 'Time.at(0, 500, :millisecond).usec', 'Time.at(0, 0, Object)',
 'Dir.glob("/nonexistent*", flags: "a")', 'Dir.glob("/nonexistent*", flags: 1.5)', 'Dir.glob("/nonexistent*", nil)', 'Dir.glob("/nonexistent*", 1.5)', 'Dir.glob("/nonexistent*", flags: Object.new)', 'Dir["/nonexistent*", flags: nil]',  'Dir.glob("/nonexistent*", base: nil, flags: 0)', 'Dir.glob("/nonexistent*", sort: nil)',
 '"abc".match(/(b)(c)/)[1.9..2.9]', '"abc".match(/(b)(c)/)[..1.9]', '"abc".match(/(b)(c)/)[nil..]', '"abc".match(/(b)(c)/)[1.9]', '"abc".match(/(b)(c)/)["a".."b"]', '"abc".match(/(b)(c)/)[1.9, 1]', '"abc".match(/(b)(c)/)[1, 1.9]', '"abc".match(/(b)(c)/)[(1.9)...(2.9)]', '"abc".match(/(b)(c)/)[-1.9..]', '"abc".match(/(b)(c)/).values_at(1.9..)', '"abc".match(/(b)(c)/).values_at(1.9)',
 '"%f" % 1i', '"%d" % 1i', '"%f" % Complex(1,0)', '"%d" % Complex(1,0)', '"%e" % 1i', '"%g" % 1i', '"%a" % 1i', '"%s" % 1i', '"%x" % 1i', '"%f" % Complex(1.5,2)', '"%f" % Complex(1,0.0)', 'Float(1i)', 'Integer(1i)', '1i.to_f', 'Float(Complex(1,0))',
]
exprs.map do |e|
  r = begin; "ok #{eval(e).inspect}"; rescue Exception => x; "#{x.class}: #{x.message}"; end
  "#{e}  ==>  #{r}"
end
"####;

#[test]
fn nil_and_float_where_an_integer_or_string_is_converted() {
    run_test_once(CONVERSIONS);
}

/// The same calls once they are compiled: the conversions are in the
/// builtin, not in a slow path only the interpreter takes.
#[test]
fn conversions_under_the_jit() {
    run_test(
        r####"
        x = ->(&b) { begin; b.call; rescue => e; "#{e.class}: #{e.message}"; end }
        k = Class.new(String)
        o = Object.new
        def o.to_str = "Q"
        [
          x.() { String.new(capacity: nil) }, x.() { String.new("a", capacity: nil) },
          x.() { String.new(encoding: nil) }, x.() { String.new(encoding: nil, capacity: nil) },
          x.() { k.new(capacity: nil).class.superclass.to_s }, x.() { k.new(encoding: nil) },
          x.() { k.new("a", encoding: nil, capacity: nil) },
          x.() { "abc".send(:initialize, encoding: nil).encoding.to_s },
          x.() { String.new(capacity: 1.5) }, x.() { String.new(capacity: -1) },
          x.() { "abc".bytesplice(0, 1, nil) }, x.() { "abc".bytesplice(0..1, :a) },
          x.() { "abc".bytesplice(0, 1, o) }, x.() { "abc".bytesplice(nil, 1, "x") },
          x.() { "abc".bytesplice(1.9, 1, "x") }, x.() { "abc".unicode_normalize(nil) },
          x.() { "a".unicode_normalize(:foo) }, x.() { (1..3).sum(nil) }, x.() { (1..3).sum(1.5) },
          x.() { 1.5.rationalize(nil) }, x.() { Hash.new(capacity: nil) },
          x.() { Hash.new(capacity: "a") }, x.() { Time.at(0, 0, nil) },
          x.() { "abc".match(/(b)(c)/)[1.9..] }, x.() { "%f" % 1i },
        ]
        "####,
    );
}
