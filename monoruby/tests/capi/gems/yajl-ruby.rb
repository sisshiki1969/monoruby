# Run on monoruby (the gem installed through its C API layer) and on the
# host CRuby by tests/capi_gems.rs; both must print the same.
gem "yajl-ruby", ENV.fetch("CAPI_GEM_VERSION")
require "yajl"
require "stringio"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\Ayajl\z/).sort.inspect
def try
  yield
rescue => e
  [e.class, e.message]
end
src = '{"a": [1, 2.5, "x", null, true, false], "b": {"c": "あ\n", "d": -0.0, "e": 12345678901234567890}}'
p Yajl::Parser.parse(src)
p Yajl::Parser.parse(src, symbolize_keys: true)
p Yajl::Parser.new(symbolize_keys: true).parse(src).keys
p Yajl::Parser.parse(StringIO.new(src))["b"]
p Yajl::Parser.parse("[1, 2] [3]") rescue p $!.class
docs = []
parser = Yajl::Parser.new
parser.on_parse_complete = lambda { |obj| docs << obj }
parser << '{"x": 1}'
parser << '{"y": [2'
parser << ', 3]}'
p docs
p try { Yajl::Parser.parse('{"a": }') }, try { Yajl::Parser.parse("[1, 2") }, try { Yajl::Parser.parse(nil) }
p Yajl::Parser.parse('{"a": 1, "a": 2}'), Yajl::Parser.parse('"😀"'), Yajl::Parser.parse("1e3"), Yajl::Parser.parse("-7")
obj = {"a" => [1, 2.5, "x", nil, true, false], "b" => {"c" => "あ\n\"\\", "d" => 1.0e-5, :sym => :val}, 7 => 7}
p Yajl::Encoder.encode(obj)
puts Yajl::Encoder.encode(obj, pretty: true)
puts Yajl::Encoder.encode(obj, pretty: true, indent: "\t")
p Yajl::Encoder.new.encode([1, {"a" => nil}]), Yajl::Encoder.new(terminator: "\n").encode([1]), Yajl::Encoder.encode("str"), Yajl::Encoder.encode(nil)
io = StringIO.new
Yajl::Encoder.encode({"k" => [1, 2]}, io)
p io.string
chunks = []
Yajl::Encoder.encode({"k" => [1, 2]}) { |chunk| chunks << chunk }
p chunks
p Yajl.dump([1, "two"]), Yajl.load('{"x": [1]}'), Yajl::Encoder.encode(2**64), Yajl::Encoder.encode(1.5e300), Yajl::Encoder.encode(Object.new.then { |o| def o.to_json(*) = '{"custom":1}'; o }.class.name)
p try { Yajl::Encoder.encode(Float::NAN) }, try { Yajl::Encoder.encode(Float::INFINITY) }, try { Yajl::Encoder.encode(["\xff".b]) }
p Yajl::Encoder.encode(Time.at(0).utc), Yajl::Encoder.encode(Class), Yajl::Encoder.encode(1..2)
Yajl::Encoder.enable_json_gem_compatability
p [1, "a", nil, true, 2.5, {"k" => [1]}].to_json, 1.to_json, "x".to_json, nil.to_json, {}.to_json
p Yajl::ParseError.ancestors.include?(StandardError), Yajl::EncodeError.superclass, Yajl::Parser.instance_methods(false).sort
