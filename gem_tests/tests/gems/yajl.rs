extern crate monoruby;
use monoruby::tests::*;

// The yajl-ruby gem over monoruby's stand-in for yajl.so (gem/yajl/yajl.rb).
// Every case is compared against the host CRuby, which runs the gem's real
// C extension over the YAJL library — so these pin the stand-in to what
// yajl actually does, error messages included.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and yajl-ruby is an ordinary gem.

/// What `Yajl::Parser.parse` builds: objects, arrays and scalars,
/// `symbolize_keys`, an empty or blank input (nil), an unterminated
/// object (the part built so far — the extension ignores the status of
/// its final flush), an IO source, big integers and float overflow, UTF-8
/// results, the chunked form with `on_parse_complete`, and `check_utf8:
/// false` letting invalid bytes through.
#[test]
fn yajl_parse_values() {
    run_test_once(
        r##"
        require "rubygems"
        require "yajl"
        require "stringio"
        res = []
        res << Yajl::Parser.parse('{"a":1,"b":[1,2.5,"x",null,true,false,-0,1e3]}')
        res << Yajl::Parser.parse('{"a":{"b":1}}', symbolize_keys: true)
        res << Yajl::Parser.parse('{"a":{"b":1}}', symbolize_names: true)
        res << [Yajl::Parser.parse("3"), Yajl::Parser.parse('"x"'), Yajl::Parser.parse("true"), Yajl::Parser.parse("null")]
        res << [Yajl::Parser.parse(""), Yajl::Parser.parse("  \n")]
        res << [Yajl::Parser.parse("{"), Yajl::Parser.parse('{"a":[1,'), Yajl::Parser.parse('[1,{"a"')]
        res << Yajl::Parser.parse(StringIO.new('[1,2]'))
        r = []
        res << [Yajl::Parser.parse(StringIO.new('{"k":"v"}' * 3 + "  "), {}, 4) { |o| r << o }, r]
        res << Yajl::Parser.parse('[12345678901234567890, 1e400, -0.0, 1.5e3, 0.1]')
        res << Yajl::Parser.parse('["é😀", "\\u00e9", "\\ud83d\\ude00", "a\\/b\\n\\t\\"\\\\"]').map { |s| [s.bytes, s.encoding.name] }
        p1 = Yajl::Parser.new
        r = []
        p1.on_parse_complete = ->(o) { r << o }
        p1 << '{"a"'
        p1 << ':1}{"b"'
        p1 << ':2} [3'
        p1 << ', 4] 5 '
        res << r
        p1 << '"s"'
        res << r
        p1 = Yajl::Parser.new
        res << p1.parse('{"a":1}')
        res << Yajl::Parser.parse("[\"\xff\"]", check_utf8: false).first.bytes
        res << Yajl::Parser.parse('["a"]').first.encoding.name
        res << Yajl::Parser.parse("/* c */ [1, // line\n 2]")
        res << [Yajl::MAX_DEPTH, Yajl::Parser.parse("[" * 130 + "]" * 130).flatten]
        r = []
        Yajl::Parser.parse('{"a":1} {"a":2} 3') { |o| r << o }
        res << r
        r = []
        Yajl::Parser.new.parse("[1] [2]", 10) { |o| r << o }
        res << r
        res << [Yajl.load('{"a":1}'), Yajl.load('[1]', symbolize_keys: true)]
        res
        "##,
    );
}

/// The parser's errors, with yajl's rendering: the error line, the
/// input around the failure padded to column 41, the arrow.
#[test]
fn yajl_parse_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "yajl"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { Yajl::Parser.parse("{\"a\":1}\n{\"a\":2}") }
        res << t.() { Yajl::Parser.parse("1 2") }
        res << t.() { Yajl::Parser.new << '{"a":1}' }
        res << t.() { Yajl::Parser.new.parse_chunk(nil) }
        res << t.() { Yajl::Parser.parse(:sym) }
        res << t.() { Yajl::Parser.parse('{"a":}') }
        res << t.() { Yajl::Parser.parse('{"a":1} xx') }
        res << t.() { Yajl::Parser.parse('[NaN]') }
        res << t.() { Yajl::Parser.parse('[tru]') }
        res << t.() { Yajl::Parser.parse('{1:2}') }
        res << t.() { Yajl::Parser.parse('{"a" 1}') }
        res << t.() { Yajl::Parser.parse('{"a":1 "b":2}') }
        res << t.() { Yajl::Parser.parse('[1 2]') }
        res << t.() { Yajl::Parser.parse('["a\\x"]') }
        res << t.() { Yajl::Parser.parse('["\\u12g4"]') }
        res << t.() { Yajl::Parser.parse("[\"a\tb\"]") }
        res << t.() { Yajl::Parser.parse('[-]') }
        res << t.() { Yajl::Parser.parse('[1.]') }
        res << t.() { Yajl::Parser.parse('[1e]') }
        res << t.() { Yajl::Parser.parse('[1] // c', allow_comments: false) }
        res << t.() { Yajl::Parser.parse('[1] /x') }
        res << t.() { Yajl::Parser.parse('{"key":"value","other":"value","third":"value","fourth":"value",}') }
        res << t.() { Yajl::Parser.parse("{\"a\":\n1,\n}") }
        res << t.() { Yajl::Parser.parse("[1]", "x") }
        res << t.() { Yajl::Parser.new(1) }
        res
        "##,
    );
}

/// `Yajl::Encoder`: compact and pretty layout (default and custom
/// indent, empty containers), the terminator, `html_safe` and
/// `entities`, an IO or block target, floats and big integers through
/// `to_s`, `Time` and other objects through `to_s`, `to_json` when
/// defined, non-string keys, escapes, `on_progress`, the `Yajl.dump` /
/// `Yajl.load` helpers, and `enable_json_gem_compatability`.
#[test]
fn yajl_encode() {
    run_test_once(
        r##"
        require "rubygems"
        require "yajl"
        require "stringio"
        res = []
        res << Yajl::Encoder.encode({"a" => 1, :b => [1, 2.5, nil, true, "x\"y\n"], "c" => {"d" => :e}})
        res << Yajl::Encoder.encode({"a" => [1, {"b" => 2}], "c" => {}, "d" => []}, pretty: true)
        res << Yajl::Encoder.encode({"a" => [1]}, pretty: true, indent: "\t")
        res << Yajl::Encoder.encode([[]], pretty: true)
        res << [Yajl::Encoder.encode([1], terminator: "\n"), Yajl::Encoder.encode([1], terminator: nil)]
        res << Yajl::Encoder.encode(["</script> & é / <"], html_safe: true).bytes
        res << Yajl::Encoder.encode(["<>&'\"/  "], entities: true).bytes
        io = StringIO.new
        r = Yajl::Encoder.encode([1], io)
        res << [r, io.string]
        r = []
        x = Yajl::Encoder.encode([1, 2], &->(chunk) { r << chunk })
        res << [x, r]
        r = []
        Yajl::Encoder.new(terminator: "\n").encode([1]) { |chunk| r << chunk }
        res << r
        res << Yajl::Encoder.encode([1.0, 1e20, 1.5e-7, 100.0, 12345678912345.678, 2**70, -5, 3r])
        res << Yajl::Encoder.encode([Time.at(0).utc, :sym, Object.new.class])
        o = Object.new
        def o.to_json(*); '"custom"'; end
        res << Yajl::Encoder.encode([o])
        res << Yajl::Encoder.encode({1 => 2, nil => 3, :s => 4, 1.5 => 5})
        res << Yajl::Encoder.encode(["a/b", "\x01\x1f", "\t\r\b\f", "\u{1F600}"]).bytes
        res << Yajl::Encoder.encode(["\xff".b]).bytes
        res << Yajl::Encoder.encode(["é"]).encoding.name
        e = Yajl::Encoder.new
        r = []
        e.on_progress = ->(c) { r << c }
        x = e.encode([1])
        res << [x, r]
        e = Yajl::Encoder.new(pretty: true)
        res << [e.encode([1]), e.encode({"a" => 1})]
        res << [Yajl.dump({"a" => 1}), Yajl.dump([1], pretty: true)]
        io = StringIO.new
        Yajl.dump([1], io)
        res << io.string
        big = ["x" * 5000] * 3
        r = []
        Yajl::Encoder.new.encode(big) { |chunk| r << chunk.bytesize }
        res << r
        res << (Yajl::Encoder.encode(Array.new(200) { [] }.inject { |a, b| [a] }).size)
        Yajl::Encoder.enable_json_gem_compatability
        res << [[1, {"a" => nil}].to_json, "x".to_json, nil.to_json, 1.5.to_json, true.to_json, 7.to_json, {"k" => [1]}.to_json(Yajl::Encoder.new(pretty: true))]
        res
        "##,
    );
}

/// What the encoder refuses: NaN and the infinities, nesting past
/// `MAX_DEPTH`, a `to_json` answering something other than a String, and
/// a non-Hash option. (A non-String `indent:` is not here: the extension
/// crashes on it.)
#[test]
fn yajl_encode_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "yajl"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { Yajl::Encoder.encode([Float::NAN]) }
        res << t.() { Yajl::Encoder.encode([Float::INFINITY]) }
        res << t.() { Yajl::Encoder.encode([-Float::INFINITY]) }
        res << t.() { Yajl::Encoder.encode(Array.new(300) { [] }.inject { |a, b| [a] }) }
        o = Object.new
        def o.to_json(*); 1; end
        res << t.() { Yajl::Encoder.encode([o]) }
        res << t.() { Yajl::Encoder.new(1) }
        res
        "##,
    );
}
