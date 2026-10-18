# Run on monoruby (the gem's own C extensions, parser.so and generator.so,
# built through the C API layer) and on the host CRuby by
# tests/capi_gems.rs; both must print the same.
gem "json", ENV.fetch("CAPI_GEM_VERSION")
require "json"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\A(parser|generator)\z/).sort.inspect
p JSON::VERSION, JSON::Parser.name, JSON::State.name
def try
  yield
rescue => e
  [e.class, e.message]
end
doc = { "a" => [1, 2.5, "x", nil, true, false], "b" => { "c" => "あ\n\t\"\\/", "d" => -0.0, "e" => 12345678901234567890, "f" => 1e-7, "g" => 1.0e20 }, "s" => :sym, 3 => "int key" }
s = JSON.generate(doc)
puts s
puts JSON.pretty_generate(doc)
puts JSON.generate(doc, indent: "\t", space: " ", object_nl: "\n", array_nl: "\n")
p JSON.parse(s)
p JSON.parse(s, symbolize_names: true)
p JSON.parse('{"a": 1}', freeze: true).frozen?, JSON.parse('"x"'), JSON.parse("[]"), JSON.parse("  12  "), JSON.parse("null"), JSON.parse("-0.0"), JSON.parse("1E2"), JSON.parse('"\\u3042\\ud83d\\ude00"')
p JSON.parse('{"a":{"b":[{"c":[]}]}}', object_class: Hash, array_class: Array), JSON.parse("[1.5]", decimal_class: Float)
p try { JSON.parse("") }, try { JSON.parse("[1,]") }, try { JSON.parse('{"a" 1}') }, try { JSON.parse("[" * 101 + "]" * 101) }, try { JSON.parse("NaN") }, try { JSON.parse("[1] x") }
p JSON.parse("NaN", allow_nan: true).nan?, JSON.parse("[1,]", allow_trailing_comma: true), JSON.parse("[1, // c\n 2]", allow_control_characters: false) rescue p $!.class
p try { JSON.generate(Float::NAN) }, JSON.generate(Float::INFINITY, allow_nan: true), try { JSON.generate("\xff") }, JSON.generate("\xff".b, strict: false) rescue p $!.class
p JSON.generate("</script> ", script_safe: true), JSON.generate("é😀", ascii_only: true), JSON.generate("a\u0000b")
p try { JSON.generate(Object.new, strict: true) }, JSON.generate(Object.new.tap { |o| def o.to_s = "obj" }).start_with?('"obj'), JSON.generate(1..3), JSON.generate(Struct.new(:x).new(1))
p [1, "two", :three, nil, true, 2.5, { k: [1] }].map(&:to_json)
p JSON.dump({ "a" => nil }), JSON.load('{"a": null}'), JSON.load(nil), JSON.load("")
st = JSON::State.new(indent: "  ", max_nesting: 2)
p st.indent, st.max_nesting, st.to_h.keys.sort.first(6), try { st.generate([[[1]]]) }, st.generate([[1]])
p JSON.generate([JSON::Fragment.new("{\"raw\": 1}")]) if defined?(JSON::Fragment)
coder = JSON::Coder.new(symbolize_names: true) { |o| o.to_s }
p coder.dump({ a: Object.new.tap { |o| def o.to_s = "coded" } }), coder.load('{"x": [1]}')
class WithToJson; def to_json(*a) = { "custom" => true }.to_json(*a); end
p JSON.generate([WithToJson.new]), JSON.pretty_generate([WithToJson.new, { "n" => {} }, []])
big = { "list" => (1..200).map { |i| { "i" => i, "f" => i / 3.0, "s" => "v#{i}" } } }
p JSON.parse(JSON.generate(big)) == big, JSON.generate(big).bytesize
p JSON.parse('{"a":1,"a":2}'), try { JSON.parse('{"a":1,"a":2}', allow_duplicate_key: false) }
p JSON.generate("x" => 1.0 / 3), JSON.generate(0.1 + 0.2), JSON.generate(100.0), JSON.generate(1e100), JSON.generate(-1.5e-9), JSON.generate(123456789.123456789)
