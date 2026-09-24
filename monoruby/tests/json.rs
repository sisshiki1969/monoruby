extern crate monoruby;
use monoruby::tests::*;

// The json gem as CRuby 4.0.6 ships it (json 2.18.0, #1623): its Ruby
// half vendored as it is, and its two C extensions ported —
// `JSON::Ext::ParserConfig` / `JSON::Ext::Parser.parse` from parser.c,
// `JSON::Ext::Generator::State` and the `GeneratorMethods` from
// generator.c. The parser refuses what CRuby's refuses, with CRuby's
// messages at CRuby's line and column; the generator goes through a
// user's `to_json` / `to_s`, keeps a Bignum a number and writes a
// Float as `fpconv_dtoa` does.

const HELPER: &str = r#"
  require "json"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class, e.message.gsub(/0x\h+/, "0x"), (e.is_a?(JSON::ParserError) ? [e.line, e.column] : nil)]; end }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// Leading zeros, trailing input, bad escapes, raw control characters,
/// a lone surrogate: `JSON::ParserError`, the message naming a fragment
/// of the input and where it is, which the exception also carries as
/// `line` / `column`. Comments are whitespace.
#[test]
fn the_parser_refuses_what_cruby_refuses() {
    run(r##"
        srcs = ["01", "-01", "-", "1.", "1.e5", "1e", "1e+", ".5", "[1]x", "[1]\n\n  x", '["\x"]', "[\"a\nb\"]", "[\"a\tb\"]",
                "nul", "tru", "fals", "x", "[1,", "[1 2]", '{"a" 1}', '{1:2}', '{"a":1 "b":2}', '{"a":1,}', '[1,]', '{"a":1,2}',
                "\xEF\xBB\xBF[1]", '["\ud83c"]', '["\ud83c\u0041"]', '["\u12"]', '["\u12G4"]', '["abc\u12"]', '["ab\qcd"]',
                "", "   ", "/* x */ [1] // y", "[1] /* x", "[1] /", "[1] /x", "[NaN]", "[Infinity]", "[-Infinity]", "[-Inf]",
                "\"abc", "[\"abc\\", "[1]\u0000", "\u0000[1]", "[\n1,\n2\n,\n]", "{\n  \"a\": [\n    1,\n    x\n  ]\n}",
                "\"é\" x", "[\"aé\" x", "[\"ab\\\n\"]", "[1] é", "[1] xé"]
        srcs.map { |src| [src, t.() { JSON.parse(src) }] }
    "##);
}

/// An integer of any size is an Integer; a decimal is a Float, or what
/// `decimal_class` makes of its text.
#[test]
fn numbers() {
    run(r##"
        res = []
        res << t.() { JSON.parse("[123456789012345678901234567890, -123456789012345678901234567890, 99999999999999999, 999999999999999999, 1.5, -0.0, 1E3, 1e-3, 12345678901234567890.5, 1e400, -1e400]") }
        res << t.() { JSON.parse("[1.5, 2]", decimal_class: Rational) }
        require "rubygems"
        res << t.() { require "bigdecimal"; v = JSON.parse('{"foo": 9.01234567890123456789}', decimal_class: BigDecimal)["foo"]; [v.class, v.to_s] }
        res << t.() { require "bigdecimal"; [BigDecimal.respond_to?(:new), (BigDecimal.new("1") rescue $!.class)] }
        res
    "##);
}

/// The parser's options, each as parser.c reads it.
#[test]
fn parser_options() {
    run(r##"
        res = []
        res << t.() { JSON.parse("[NaN, Infinity, -Infinity]", allow_nan: true).map(&:to_s) }
        res << t.() { JSON.parse("[1,2,]", allow_trailing_comma: true) }
        res << t.() { JSON.parse('{"a":1,}', allow_trailing_comma: true) }
        res << t.() { JSON.parse("[\"a\tb\"]", allow_control_characters: true) }
        res << t.() { JSON.parse("[\"a\\\tb\\nc\"]", allow_control_characters: true) }
        res << t.() { JSON.parse('{"a":{"b":1}}', symbolize_names: true) }
        res << t.() { r = JSON.parse('{"a":["x",{"b":"y"}]}', freeze: true); [r.frozen?, r["a"].frozen?, r["a"][0].frozen?, r.keys[0].frozen?, r["a"][1].frozen?, r["a"][0].equal?(-"x")] }
        res << t.() { k = JSON.parse('{"key":1}').keys[0]; [k.frozen?, k.equal?(-"key")] }
        res << t.() { l = []; JSON.parse('{"a":[1,"x"]}', on_load: ->(v) { l << v; v }); l }
        res << t.() { JSON.parse('[[[1]]]', max_nesting: 2) }
        res << t.() { JSON.parse('[[[1]]]', max_nesting: 3) }
        res << t.() { JSON.parse('[[[1]]]', max_nesting: false) }
        res << t.() { JSON.parse('{"a":1,"a":2}') }
        res << t.() { JSON.parse('{"a":1,"a":2}', allow_duplicate_key: true) }
        res << t.() { JSON.parse("{\n\"a\":1,\n\"a\":2}", allow_duplicate_key: false) }
        res << t.() { JSON.parse('{"a":1,"a":2}', symbolize_names: true, allow_duplicate_key: false) }
        res << t.() { JSON.parse('{"a":1}', object_class: Struct.new(:a) { def []=(k, v); self.a = v; end }).to_a }
        res << t.() { c = Class.new(Array); JSON.parse('[1,[2]]', array_class: c).map { |x| x.class == c } }
        res << t.() { JSON.parse('{"a":1}', symbolize_names: true, create_additions: true) }
        res << t.() { JSON.parse!("[NaN, " + "[" * 120 + "1" + "]" * 120 + "]").flatten.map(&:to_s) }
        res << t.() { JSON.load("[1] x") }
        res << t.() { JSON.load("") }
        res << t.() { JSON.load(nil) }
        res << t.() { JSON.load('[1,{"a":2}]', ->(x) { x }) }
        res << t.() { JSON.parse(nil) }
        res << t.() { JSON.parse(Class.new { def to_str = "[2]" }.new) }
        res << t.() { JSON::Ext::ParserConfig.new(symbolize_names: true).parse('{"a":1}') }
        res << t.() { JSON::Ext::ParserConfig.new(1) }
        res << t.() { JSON::Ext::ParserConfig.allocate.parse("[[1]]") }
        res << t.() { JSON::Parser.new('{"a":1}', symbolize_names: true).then { |pa| [pa.source, pa.parse] } }
        res << t.() { JSON::Coder.new(symbolize_names: true).load('{"a":1}') }
        res
    "##);
}

/// `JSON.generate` without options goes through a user's `to_json`,
/// through `to_s` for an object without one, and writes a Bignum as a
/// number; `to_json` is handed the State at its depth.
#[test]
fn the_generator_calls_to_json_and_to_s() {
    run(r##"
        class Foo; def to_json(*) = '"foo"'; end
        class Bar; def to_s = "bar"; end
        class Baz; def to_json(state) = [state.class.name, state.depth, state.indent].to_json; end
        class Bad; def to_json(*) = 1; end
        S = Struct.new(:a, :b)
        res = []
        res << t.() { JSON.generate([Foo.new, Bar.new, 2**70, -2**70, :sym, nil, true, false, 1, -1]) }
        res << t.() { JSON.generate(Foo.new) }
        res << t.() { JSON.generate(Bar.new) }
        res << t.() { JSON.generate([[Baz.new]]) }
        res << t.() { JSON.pretty_generate([[Baz.new]]) }
        res << t.() { JSON.generate([Bad.new]) }
        res << t.() { JSON.generate([S.new(1, "x")]) }
        res << t.() { [1.to_json, (2**70).to_json, 1.5.to_json, "x".to_json, nil.to_json, true.to_json, false.to_json, :a.to_json, [1].to_json, {"a" => 1}.to_json, Bar.new.to_json] }
        res << t.() { [[1, [2]].to_json(indent: "  ", array_nl: "\n"), {"a" => 1}.to_json(JSON::State.new(space: " "))] }
        res << t.() { JSON.generate([JSON::Fragment.new('{"raw": 1}')]) }
        res << t.() { JSON::Coder.new { |o| o.to_s }.dump([1, Bar.new, :s]) }
        res << t.() { JSON::Coder.new.dump([1, Bar.new]) }
        res << t.() { [JSON[[1, 2]], JSON['[1, 2]'], JSON(Bar.new)] }
        res
    "##);
}

/// A Float the way json's `fpconv_dtoa` writes it — Grisu2 digits,
/// `1e+20`, `0.00001` — and NaN or an infinity only under `allow_nan:`.
#[test]
fn floats_as_fpconv_writes_them() {
    run(r##"
        res = []
        res << [1.0, 1e20, 1e15, 1e14, 123456.789, 1e-5, 1e-6, 1e-7, 0.1, -0.0, 5e-324, 1.7976931348623157e308, 12345678901234567.0,
                0.000123456789, 1.0e-10, 123e-20, 2.0**62, 3.14159, 100.0, 1e16, 9007199254740993.0, 2.2250738585072014e-308, 1.1, 1/3.0].map { |f| JSON.generate(f) }
        res << t.() { JSON.generate([Float::NAN]) }
        res << t.() { JSON.generate([-Float::INFINITY]) }
        res << t.() { JSON.generate([Float::NAN, Float::INFINITY, -Float::INFINITY], allow_nan: true) }
        res << t.() { JSON.dump([Float::NAN]) }
        res
    "##);
}

/// Nesting, key types, the escaping modes, the layout options,
/// `strict:` / `as_json:` and the encoding of what is written.
#[test]
fn generator_options_and_errors() {
    run(r##"
        class Bar; def to_s = "bar"; end
        res = []
        res << t.() { a = []; a << a; JSON.generate(a) }
        res << t.() { h = {}; h["h"] = h; JSON.generate(h) }
        res << t.() { JSON.generate([[[1]]], max_nesting: 2) }
        res << t.() { JSON.generate([[[1]]], max_nesting: 3) }
        res << t.() { JSON.dump([[[1]]], 2) }
        res << t.() { JSON.dump([1, {"a" => nil}]) }
        res << t.() { require "stringio"; io = StringIO.new; r = JSON.dump({"a" => 1}, io); [r.equal?(io), io.string] }
        res << t.() { JSON.generate({1 => 2, nil => 3, :s => 4, 1.5 => 5}) }
        res << t.() { JSON.generate({"a" => 1, a: 2}) }
        res << t.() { JSON.generate({"a" => 1, a: 2}, allow_duplicate_key: false) }
        res << t.() { JSON.generate({"a" => 1, b: 2}, allow_duplicate_key: false) }
        res << t.() { JSON.generate("\u2028/é𝄞<\"\\\x01\x1f", script_safe: true) }
        res << t.() { JSON.generate("\u2028/é𝄞<\"\\\x01\x1f", ascii_only: true) }
        res << t.() { JSON.generate("\u2028/é𝄞", ascii_only: true, script_safe: true) }
        res << t.() { JSON.generate([1, {"a" => [2, {}], "b" => []}], indent: "\t", array_nl: "\n", object_nl: "\n", space: " ", space_before: " ") }
        res << t.() { JSON.pretty_generate({"a" => [1, 2, {"b" => nil}], "c" => {}}) }
        res << t.() { JSON.pretty_generate([]) }
        res << t.() { JSON.generate([1, Object.new], strict: true) }
        res << t.() { JSON.generate([1, :sym], strict: true) }
        res << t.() { JSON.generate({Object.new => 1}, strict: true) }
        res << t.() { JSON.generate([1, Bar.new, Float::NAN], strict: true, as_json: ->(o, is_key) { o.is_a?(Float) ? "nan" : o.to_s }) }
        res << t.() { JSON.generate({Bar.new => 1}, strict: true, as_json: ->(o, is_key) { [o.to_s, is_key].join(":") }) }
        res << t.() { JSON.generate({1 => 1}, strict: true, as_json: ->(o, is_key) { o }) }
        res << t.() { JSON.generate(["\xFF".b]) }
        res << t.() { JSON.generate(["\u3042".b]).bytes }
        res << t.() { JSON.generate(["\u3042".encode("EUC-JP")]).bytes }
        res << t.() { JSON.generate(["a\xFF"]) }
        res << t.() { e = (JSON.generate({foo: "\x82\xAC\xEF".b}) rescue $!); [e.class, e.cause.class, e.invalid_object] }
        res << t.() { class MyStr < String; end; h = {MyStr.new("k") => MyStr.new("v")}; [h.keys[0].class, JSON.generate(h)] }
        res << t.() { class MyStr2 < String; def to_s = "custom"; def to_json(*) = '"J"'; end; JSON.generate({MyStr2.new("k") => MyStr2.new("v")}) }
        res << t.() { JSON.generate({"a" => 1}, JSON::State.new(object_nl: "\n")) }
        res << t.() { JSON.generate(1 << 64, {}) }
        res
    "##);
}

/// `JSON::State`: its accessors, `to_h`, `configure`, what it refuses,
/// and a frozen one generating.
#[test]
fn the_state() {
    run(r##"
        res = []
        res << t.() { JSON::State.new(indent: "  ", max_nesting: 5, allow_nan: true, ascii_only: true, depth: 2, script_safe: true, strict: true, buffer_initial_length: 10).to_h }
        res << t.() { s = JSON::State.new; [s.indent, s.space, s.space_before, s.object_nl, s.array_nl, s.as_json, s.max_nesting, s.depth, s.buffer_initial_length, s.allow_nan?, s.ascii_only?, s.script_safe?, s.strict?, s.check_circular?, s.instance_variables, s.indent.frozen?] }
        res << t.() { s = JSON::State.new; s.indent = "x"; s.max_nesting = false; s.depth = 3; s.buffer_initial_length = -1; s.escape_slash = true; [s.indent, s.max_nesting, s.depth, s.buffer_initial_length, s.script_safe, s.check_circular?] }
        res << t.() { s = JSON::State.new; s.configure(indent: "  ", foo: 1); [s.indent, s.to_h.key?(:foo)] }
        res << t.() { JSON::State.new(indent: 1) }
        res << t.() { JSON::State.new.as_json = nil }
        res << t.() { JSON::State.new.buffer_initial_length = "x" }
        res << t.() { JSON::State.new.freeze.indent = "x" }
        res << t.() { JSON::State.new(indent: "  ").freeze.generate([1]) }
        res << t.() { s = JSON::State.new(indent: "  "); d = s.dup; d.indent = ""; [s.indent, d.indent] }
        res << t.() { [JSON::State.from_state(nil).class.name, JSON::State.from_state({indent: "  "}).indent] }
        res << t.() { JSON::State.generate([1, 2], {space: " "}, nil) }
        res << t.() { JSON::State.equal?(JSON::Ext::Generator::State) }
        res
    "##);
}

// ---------------------------------------------------------------------------
// From the former unit tests in `src/builtins/json.rs`.
// ---------------------------------------------------------------------------

#[test]
fn json_generate_writes_every_string_as_utf8() {
    // CRuby's generator: a UTF-8 string must be well-formed, a
    // BINARY string with well-formed UTF-8 bytes is taken for UTF-8,
    // everything else is transcoded and the transcoding's failure
    // is the GeneratorError's message; `invalid_object` names the
    // string, wherever it sat.
    run_test_once(
        r##"
        require "json"
        e = ->(&b) { begin; b.call; rescue JSON::GeneratorError => x; [x.class, x.message, x.invalid_object, x.detailed_message]; end }
        h = ->(s) { s.unpack1("H*") }
        [e.() { JSON.generate(["a\xFFb"]) },
         e.() { JSON.generate(["a\xE3\x81"]) },
         e.() { JSON.generate({"k" => ["a\xFFb"]}) },
         e.() { JSON.generate({"\xFF" => 1}) },
         e.() { JSON.generate("a\xFFb") },
         [h.(JSON.generate(["abc".b])), JSON.generate(["abc".b]).encoding.to_s],
         h.(JSON.generate(["\xE3\x81\x82".b])),
         e.() { JSON.generate(["\xFF".b]) },
         e.() { JSON.generate(["a\x80".force_encoding("US-ASCII")]) },
         h.(JSON.generate(["\u3042".encode("EUC-JP")])),
         h.(JSON.generate(["\u3042".encode("Shift_JIS")])),
         h.(JSON.generate(["\u3042".encode("UTF-16LE")])),
         h.(JSON.generate(["\u3042".encode("ISO-2022-JP")])),
         h.(JSON.generate({"\u3042".encode("EUC-JP") => 1})),
         e.() { JSON.generate(["a\xA4".force_encoding("EUC-JP")]) },
         e.() { JSON.generate(["a".encode("UTF-16LE").byteslice(0, 1)]) }.first(2),
         e.() { JSON.generate(["abc".force_encoding("UTF-7")]) }.first(2),
         e.() { JSON.dump(["a\xFFb"]) },
         JSON.generate(["\u3042"]).bytes]
        "##,
    );
}

#[test]
fn json_parse_reads_the_source_as_utf8() {
    // `convert_encoding`: a UTF-8 source is read as it is, ill-formed
    // bytes and all; a BINARY source is taken for UTF-8; any other
    // encoding is transcoded first, and its failure is an
    // `Encoding::*Error`. Every string in the result is UTF-8, and a
    // key is frozen.
    run_test_once(
        r##"
        require "json"
        e = ->(&b) { begin; b.call; rescue => x; [x.class, x.message]; end }
        d = ->(v) { v.map { |s| [s.unpack1("H*"), s.encoding.to_s, s.valid_encoding?] } }
        [d.(JSON.parse("[\"a\xFFb\"]")),
         d.(JSON.parse("[\"a\xE3\x81\"]")),
         d.(JSON.parse("[\"abc\"]".b)),
         d.(JSON.parse("[\"\xE3\x81\x82\"]".b)),
         d.(JSON.parse("[\"\xFF\"]".b)),
         d.(JSON.parse("[\"\u3042\"]".encode("EUC-JP"))),
         d.(JSON.parse("[\"\u3042\"]".encode("Shift_JIS"))),
         d.(JSON.parse("[\"\u3042\"]".encode("UTF-16LE"))),
         d.(JSON.parse("[\"abc\"]".force_encoding("US-ASCII"))),
         e.() { JSON.parse("[\"a\x80\"]".force_encoding("US-ASCII")) },
         e.() { JSON.parse("[\"\xA4\"]".force_encoding("EUC-JP")) },
         e.() { JSON.parse("[\"a\"]".encode("UTF-16LE").byteslice(0, 7)) },
         e.() { JSON.parse("[1]".force_encoding("UTF-7")) },
         e.() { JSON.parse("[\"\\ud83c\"]") }.first,
         d.(JSON.parse("[\"\\ud83c\\udf63\"]")),
         JSON.parse("{\"a\":1}").keys.map { |k| [k.frozen?, k.encoding.to_s] },
         d.(JSON.parse("{\"\u3042\":1}".encode("EUC-JP")).keys),
         e.() { JSON.parse("x") }.first,
         e.() { JSON.parse(nil) },
         e.() { JSON.parse(Object.new) }.first,
         JSON.parse(Class.new { def to_str = "[2]" }.new)]
        "##,
    );
}

#[test]
fn json_parse_scalars() {
    run_tests(&[
        r#"require "json"; JSON.parse("null")"#,
        r#"require "json"; JSON.parse("true")"#,
        r#"require "json"; JSON.parse("false")"#,
        r#"require "json"; JSON.parse("42")"#,
        r#"require "json"; JSON.parse("-7")"#,
        r#"require "json"; JSON.parse("0")"#,
        r#"require "json"; JSON.parse("3.14")"#,
        r#"require "json"; JSON.parse("-0.5")"#,
        r#"require "json"; JSON.parse("1e10")"#,
        r#"require "json"; JSON.parse("2.5E-3")"#,
        r#"require "json"; JSON.parse("1e+2")"#,
        r#"require "json"; JSON.parse("\"hello\"")"#,
        r#"require "json"; JSON.parse("\"\"")"#,
    ]);
}

#[test]
fn json_parse_string_escapes() {
    run_tests(&[
        r#"require "json"; JSON.parse("\"line1\\nline2\"")"#,
        r#"require "json"; JSON.parse("\"tab\\there\"")"#,
        r#"require "json"; JSON.parse("\"back\\\\slash\"")"#,
        r#"require "json"; JSON.parse("\"quote\\\"here\"")"#,
        r#"require "json"; JSON.parse("\"slash\\/ok\"")"#,
        r#"require "json"; JSON.parse("\"bs\\b\"")"#,
        r#"require "json"; JSON.parse("\"ff\\f\"")"#,
        r#"require "json"; JSON.parse("\"cr\\r\"")"#,
        r#"require "json"; JSON.parse("\"\\u0041\"") == "A""#,
        r#"require "json"; JSON.parse("\"\\u00e9\"") == "é""#,
        r#"require "json"; JSON.parse("\"\\uD83D\\uDE00\"") == "😀""#,
    ]);
}

#[test]
fn json_parse_and_generate_repeatedly() {
    // Under the JIT: the parser's value stack and the generator's
    // buffers across many calls.
    run_test(
        r#"
        require "json"
        data = JSON.parse('{"name": "Nokogiri (鋸)", "a":[1,{"b":true}],"c":null, "d": 1.5e3}')
        s = "tab\there\nnewline\r\nand\\backslash\"quote"
        [data, JSON.generate(data), JSON.parse(JSON.generate(s)) == s, JSON.pretty_generate(data)]
        "#,
    );
}

#[test]
fn json_generate_collections() {
    run_tests(&[
        r#"require "json"; JSON.generate([])"#,
        r#"require "json"; JSON.generate([1, 2, 3])"#,
        r#"require "json"; JSON.generate([[1], [2]])"#,
        r#"require "json"; JSON.generate({})"#,
        r#"require "json"; JSON.generate({"a" => 1})"#,
        r#"require "json"; JSON.generate({"a" => [true, false, nil]})"#,
        r#"require "json"; JSON.generate({"a" => {"b" => 1}})"#,
        r#"require "json"; JSON.generate("line\n\ttab\"q\\b\b\f\r")"#,
    ]);
}
