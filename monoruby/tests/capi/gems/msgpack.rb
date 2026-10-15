# Run on monoruby (the gem installed through its C API layer) and on the
# host CRuby by tests/capi_gems.rs; both must print the same.
gem "msgpack", ENV.fetch("CAPI_GEM_VERSION")
require "msgpack"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\A(bcrypt_ext|msgpack|parser|generator)\z/).sort.inspect
def show(label, v)
  packed = v.to_msgpack
  back = MessagePack.unpack(packed)
  puts "#{label}: #{packed.unpack1("H*")} #{back.inspect} #{back == v} #{packed.encoding}"
end
show "nil", nil
show "true", true
show "false", false
[0, 1, -1, 127, 128, 255, 256, 65535, 65536, 2**31 - 1, 2**31, 2**32, 2**62 - 1, 2**62, 2**63 - 1, -2**63, -2**31, -32, -33, -128, -129, 2**63, 2**64 - 1].each { |i| show "int #{i}", i }
begin; (2**64).to_msgpack; rescue => e; p e.class, e.message; end
begin; (-2**63 - 1).to_msgpack; rescue => e; p e.class, e.message; end
show "float", 1.5
show "float big", 1e300
show "float neg zero", -0.0
show "str", "hello"
show "str utf8", "日本語"
show "str bin", "\xff\x00\x01".b
show "str long", "x" * 300
show "sym", :sym
show "array", [1, [2, [3, nil]], "a"]
show "hash", { "a" => 1, "b" => [true, false], 3 => { "x" => nil } }
show "nested", (1..20).map { |i| { i.to_s => [i] * 3 } }
h = MessagePack.unpack({ "a" => 1 }.to_msgpack, symbolize_keys: true)
p h
p MessagePack.unpack("\x81\xa1a\x01".b, freeze: true).frozen?
begin; MessagePack.unpack("\xc1".b); rescue => e; p e.class, e.message; end
begin; MessagePack.unpack("\x92\x01".b); rescue => e; p e.class, e.message; end
begin; MessagePack.unpack("\x01\x02".b); rescue => e; p e.class, e.message; end
# Packer / Unpacker API
pk = MessagePack::Packer.new
pk.write_array_header(3).write(1).write("two").write_nil
pk.write_map_header(1).write("k").write_float32(1.5)
pk.write_bin("\x00\xff".b).write_ext(5, "payload")
p pk.to_s.unpack1("H*"), pk.size, pk.empty?
uk = MessagePack::Unpacker.new
uk.feed(pk.to_s)
uk = MessagePack::Unpacker.new(allow_unknown_ext: true)
uk.feed(pk.to_s)
uk.each { |o| p o }
p uk.read_array_header rescue p $!.class
# streaming
uk2 = MessagePack::Unpacker.new
data = [1, "a", [2]].map(&:to_msgpack).join
data.each_char { |c| uk2.feed_each(c) { |o| p [:stream, o] } }
# Buffer
b = MessagePack::Buffer.new
b << "abc"
b.write("def")
p b.size, b.read(2), b.read_all, b.empty?
b.write("xyz" * 100)
p b.skip(5), b.read(4), b.to_s.size, b.to_a.map(&:size)
# ext types via Factory
Point = Struct.new(:x, :y)
f = MessagePack::Factory.new
f.register_type(1, Point, packer: ->(pt) { [pt.x, pt.y].pack("l>l>") }, unpacker: ->(s) { Point.new(*s.unpack("l>l>")) })
f.register_type(2, Symbol)
f.register_type(-1, Time, packer: MessagePack::Time::Packer, unpacker: MessagePack::Time::Unpacker)
pt = Point.new(3, -4)
packed = f.dump(pt)
p packed.unpack1("H*"), f.load(packed)
p f.load(f.dump(:sym)), f.dump(:sym).unpack1("H*")
t = Time.at(1_700_000_000, 123456789, :nsec)
p f.load(f.dump(t)) == t, f.dump(t).unpack1("H*")
p f.registered_types.map { |x| [x[:type], x[:class].to_s] }
p f.packer.registered_types.size, f.unpacker.registered_types.size
# bigint ext
f2 = MessagePack::Factory.new
f2.register_type(0x01, Integer, packer: ->(i) { i.to_s }, unpacker: ->(s) { s.to_i }, oversized_integer_extension: true)
p f2.load(f2.dump(2**70)), f2.dump(2**70).unpack1("H*"), f2.dump(5).unpack1("H*")
p f2.load(f2.dump(-2**70))
# recursive ext
f3 = MessagePack::Factory.new
f3.register_type(3, Array, packer: ->(a, pk) { pk.write(a.size); a.each { |e| pk.write(e) } }, unpacker: ->(uk) { n = uk.read; n.times.map { uk.read } }, recursive: true)
p f3.load(f3.dump([1, [2, 3]]))
# ExtensionValue
ev = MessagePack::ExtensionValue.new(9, "raw")
p MessagePack.unpack(ev.to_msgpack, allow_unknown_ext: true), ev.to_msgpack.unpack1("H*")
# pool
pool = f.pool(2)
p pool.load(pool.dump([1, pt]))
# to_msgpack with packer arg
class Custom; def to_msgpack(pk = nil); pk ? pk.write("custom") : "custom".to_msgpack; end; end
p MessagePack.pack([Custom.new]).unpack1("H*")
# packer to IO
require "stringio"
io = StringIO.new
pk2 = MessagePack::Packer.new(io)
pk2.write({ "io" => [1, 2] }).flush
p io.string.unpack1("H*")
uk3 = MessagePack::Unpacker.new(StringIO.new(io.string))
p uk3.read
p MessagePack::VERSION.class
p MessagePack.unpack(MessagePack.pack("x" * 40000)).size
p MessagePack.unpack(MessagePack.pack((1..70000).to_a)).size
p MessagePack.unpack(MessagePack.pack(Hash[(1..70000).map { |i| [i, i] }])).size
