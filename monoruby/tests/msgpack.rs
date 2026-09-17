extern crate monoruby;
use monoruby::tests::*;

// The msgpack gem over monoruby's stand-in for msgpack.so
// (gem/msgpack/msgpack.rb). Every case is compared against the host CRuby,
// which runs the gem's real C extension — so these pin the stand-in to the
// extension's wire format, argument handling and error texts.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and msgpack is an ordinary gem.

/// The packer's encodings: the integer families by range, float64, str /
/// bin by the String's encoding (ASCII-8BIT is bin, other non-UTF-8
/// encodings are transcoded), fixarray / array16 and fixmap / map16
/// headers, the fixext / ext8 sizes, compatibility mode (no bin, no str8).
#[test]
fn msgpack_pack_wire_format() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        res = []
        res << MessagePack.pack([nil, true, false, 1, -1, 127, 128, 255, 256, 65535, 65536,
                                 2**32-1, 2**32, 2**64-1, -32, -33, -128, -129, -32768, -32769,
                                 -2**31, -2**31-1, -2**63, 1.5, "abc", :sym, "x".b,
                                 {"a" => 1, b: [2]}]).bytes
        res << [MessagePack.pack("a" * 31).bytes[0, 2], MessagePack.pack("a" * 32).bytes[0, 2],
                MessagePack.pack("a" * 256).bytes[0, 3], MessagePack.pack("a" * 65536).bytes[0, 5]]
        res << [MessagePack.pack(("a" * 255).b).bytes[0, 2], MessagePack.pack(("a" * 256).b).bytes[0, 3]]
        res << [MessagePack.pack("a" * 32, compatibility_mode: true).bytes[0, 3],
                MessagePack.pack("x".b, compatibility_mode: true).bytes]
        res << [MessagePack.pack("abc".encode("Shift_JIS")).bytes,
                MessagePack.pack("\x82\xa0".dup.force_encoding("Shift_JIS")).bytes,
                MessagePack.pack("é".encode("ISO-8859-1")).bytes, MessagePack.pack("é").bytes]
        res << [MessagePack.pack([0] * 16).bytes[0, 3], MessagePack.pack([0] * 65536).bytes[0, 5],
                MessagePack.pack((0...16).to_h { |i| [i, i] }).bytes[0, 3]]
        res << MessagePack::Packer.new.write_float32(1.5).to_s.bytes
        res << MessagePack.pack(MessagePack::ExtensionValue.new(5, "abcd")).bytes
        res << [1, 2, 3, 4, 8, 16, 17, 256].map { |n| MessagePack.pack(MessagePack::ExtensionValue.new(-1, "a" * n)).bytes[0, 3] }
        res << [1.to_msgpack.bytes, "a".to_msgpack.bytes, nil.to_msgpack.bytes, [1].to_msgpack.bytes,
                {}.to_msgpack.bytes, :s.to_msgpack.bytes, 1.5.to_msgpack.bytes, true.to_msgpack.bytes]
        o = Object.new
        def o.to_msgpack(pk); pk.write("custom"); end
        res << MessagePack.pack([o]).bytes
        res
        "##,
    );
}

/// What the packer refuses: integers beyond 64 bits, an extension type
/// outside a signed byte, the typed `write_*` methods, an object without
/// `to_msgpack`.
#[test]
fn msgpack_pack_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { MessagePack.pack(2**64) }
        res << t.() { MessagePack.pack(-2**63 - 1) }
        res << t.() { MessagePack::Packer.new.write_ext(200, "a") }
        res << t.() { MessagePack::Packer.new.write_hash([]) }
        res << t.() { MessagePack::Packer.new.write_string(:a) }
        res << t.() { MessagePack.pack(Object.new) }.first
        res
        "##,
    );
}

/// Unpacked values: every integer family back to Integer, str as UTF-8
/// and bin as ASCII-8BIT, float32, `symbolize_keys`, `freeze` (every
/// produced object, strings deduplicated), map keys frozen either way,
/// an IO source, and the 128-level nesting limit.
#[test]
fn msgpack_unpack_values() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        require "stringio"
        res = []
        res << MessagePack.unpack(MessagePack.pack([nil, true, false, 1, -1, 300, 70000, 2**40, -300,
                                                    -70000, -2**40, 1.5, "abc", "x".b, {"a" => [1]}]))
        res << MessagePack.unpack(MessagePack.pack(["abc", "x".b])).map { |s| s.encoding.name }
        res << MessagePack.unpack("\xca\x3f\xc0\x00\x00".b)
        res << MessagePack.unpack(MessagePack.pack({"a" => {"b" => 1}, 1 => 2}), symbolize_keys: true)
        v = MessagePack.unpack(MessagePack.pack({"a" => ["x"]}), freeze: true)
        res << [v.frozen?, v["a"].frozen?, v["a"][0].frozen?, v.keys[0].frozen?]
        h = MessagePack.unpack(MessagePack.pack({"a" => "b"}))
        res << [h.keys[0].frozen?, h["a"].frozen?]
        res << MessagePack.load(StringIO.new(MessagePack.pack([1, 2])))
        res << MessagePack.unpack(MessagePack.pack([1, 2]), symbolize_keys: true)
        res << (begin; MessagePack.unpack("\x91" * 200 + "\x01"); rescue => e; [e.class, e.message]; end)
        res << MessagePack::Timestamp.from_msgpack_ext(MessagePack::Timestamp.to_msgpack_ext(1, 2)).then { |ts| [ts.sec, ts.nsec] }
        res
        "##,
    );
}

/// The unpacker's errors: an unassigned format byte, running out of
/// bytes, bytes left after `full_unpack`, an unregistered extension type
/// with and without `allow_unknown_ext`, a header read on the wrong type.
#[test]
fn msgpack_unpack_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { MessagePack.unpack("\xc1".b) }
        res << t.() { MessagePack.unpack("\x92\x01".b) }
        res << t.() { MessagePack.unpack("\x01\x02".b) }
        res << t.() { MessagePack.unpack("\xd4\x05\x01".b) }
        res << MessagePack.unpack("\xd4\x05\x01".b, allow_unknown_ext: true).to_a
        res << t.() { u = MessagePack::Unpacker.new; u.feed("\x01".b); u.read_array_header }
        res << t.() { u = MessagePack::Unpacker.new; u.feed("\x01\x02\x03".b); u.full_unpack }
        res << t.() { MessagePack::Unpacker.new.read }
        res << [MessagePack::UnexpectedTypeError.ancestors.include?(MessagePack::TypeError),
                MessagePack::MalformedFormatError.superclass, MessagePack::StackError.superclass]
        res
        "##,
    );
}

/// The unpacker as a stream: an object split across two `feed_each`
/// calls, `each` stopping quietly at the buffer's end and at an IO's end
/// of file, the header readers, `skip_nil` (which does not consume the
/// nil it saw — the extension keeps it as the pending head byte), `skip`,
/// `reset`, and the enumerator forms.
#[test]
fn msgpack_unpacker_streaming() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        require "stringio"
        res = []
        u = MessagePack::Unpacker.new
        r = []
        u.feed_each("\x92\x01".b) { |o| r << o }
        u.feed_each("\x02\xc0".b) { |o| r << o }
        res << r
        res << MessagePack::Unpacker.new.each.class
        u = MessagePack::Unpacker.new(StringIO.new(MessagePack.pack(1) + MessagePack.pack("a")))
        r = []
        u.each { |o| r << o }
        res << r
        res << (begin; MessagePack::Unpacker.new(StringIO.new("")).read; rescue => e; [e.class, e.message]; end)
        u = MessagePack::Unpacker.new
        u.feed(MessagePack.pack([1, 2]) + MessagePack.pack({"a" => 1}))
        res << [u.read_array_header, u.read, u.read, u.read_map_header, u.read, u.read]
        u = MessagePack::Unpacker.new
        u.feed("\xc0\x01".b)
        res << [u.skip_nil, u.skip_nil, u.read, u.read]
        u = MessagePack::Unpacker.new
        u.feed("\x01\x02".b)
        res << [u.skip, u.read]
        u = MessagePack::Unpacker.new
        u.feed("\x92".b)
        u.reset
        u.feed("\x01".b)
        res << u.read
        res << [(MessagePack::Unpacker.new(nil, 1) rescue $!.message), (MessagePack::Unpacker.new(1, 2, 3) rescue $!.message),
                MessagePack::Unpacker.new(nil).class, MessagePack::Unpacker.new(symbolize_keys: true).symbolize_keys?,
                MessagePack::Unpacker.new.freeze?, MessagePack::Unpacker.new(allow_unknown_ext: true).allow_unknown_ext?]
        u = MessagePack::Unpacker.new
        u.register_type(1) { |d| d.upcase }
        u.feed("\xd4\x01a".b)
        res << [u.registered_types.map { |h| [h[:type], h[:class]] }, u.type_registered?(1), u.read]
        res
        "##,
    );
}

/// The packer object: its own type registry, `write_extension`, the
/// buffer views (`to_a`, `size`, `empty?`, `full_pack`), an IO target,
/// options without an IO, `write_to`, and headers written by hand.
#[test]
fn msgpack_packer_api() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        require "stringio"
        res = []
        pk = MessagePack::Packer.new
        pk.register_type(1, Symbol) { |s| s.to_s }
        pk.write(:ab)
        res << [pk.to_s.bytes, pk.registered_types.map { |h| [h[:type], h[:class]] }, pk.type_registered?(Symbol), pk.type_registered?(1)]
        res << MessagePack::Packer.new.write_extension(MessagePack::ExtensionValue.new(1, "ab")).to_s.bytes
        pk = MessagePack::Packer.new
        pk.write(1)
        res << [pk.to_a, pk.size, pk.empty?, pk.full_pack, pk.size, pk.empty?]
        io = StringIO.new
        pk = MessagePack::Packer.new(io)
        pk.write([1])
        r = pk.full_pack
        res << [r, io.string.bytes]
        res << MessagePack::Packer.new(compatibility_mode: true).compatibility_mode?
        io = StringIO.new
        pk = MessagePack::Packer.new
        pk.write(1)
        res << [pk.write_to(io), io.string.bytes, pk.size]
        res << MessagePack::Packer.new.write_array_header(2).write(1).write_map_header(1).write("a").write(nil).to_s.bytes
        res << [MessagePack::Packer.new.write_nil.write_true.write_false.write_int(5).write_float(1.0).write_symbol(:a).write_string("b").write_bin("c").write_array([]).write_hash({}).write_bin_header(1).to_s.bytes]
        res
        "##,
    );
}

/// `MessagePack::Buffer`: reads that stop at the data's end versus
/// `read_all` / `skip_all` that insist, the IO-backed refills (and the
/// IO's own EOFError coming through), `flush`, the return values, the
/// argument checks.
#[test]
fn msgpack_buffer() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        require "stringio"
        res = []
        b = MessagePack::Buffer.new
        b << "abc"
        res << [b.size, b.read(1), b.read_all(1), b.read, b.read, b.empty?, b.read(0), b.read_all(0)]
        b = MessagePack::Buffer.new
        b << "ab"
        res << (begin; b.read_all(3); rescue => e; [e.class, e.message]; end)
        b = MessagePack::Buffer.new
        b << "abcd"
        res << [b.skip(2), b.skip(5), b.skip(1)]
        b = MessagePack::Buffer.new
        b << "ab"
        res << [b.skip_all(1).class, (b.skip_all(5) rescue $!.message)]
        b = MessagePack::Buffer.new
        res << [b.write("abc"), (b << "d").class, b.to_s, b.to_str, b.to_a]
        io = StringIO.new("hello")
        b = MessagePack::Buffer.new(io)
        res << [b.read(2), b.read, b.read(1), b.io.equal?(io)]
        io = StringIO.new("hello")
        b = MessagePack::Buffer.new(io)
        res << [b.read_all(2), (b.read_all(10) rescue $!.message)]
        io = StringIO.new
        b = MessagePack::Buffer.new(io)
        b << "x"
        b.flush
        res << [io.string, b.size]
        res << [(MessagePack::Buffer.new(1, 2) rescue $!.message), (MessagePack::Buffer.new(1, 2, 3) rescue $!.message)]
        b = MessagePack::Buffer.new
        b << "x"
        res << [b.clear, b.size]
        res
        "##,
    );
}

/// `MessagePack::Factory`: registered types flow into its packers and
/// unpackers, the Time and Bigint extensions from the gem's Ruby half, a
/// recursive type whose payload is MessagePack itself, `freeze` and
/// `dup`, the argument checks, a subclass and an included module found
/// through the ancestor scan (a plain String never is), symbol
/// optimisation, and `pool`.
#[test]
fn msgpack_factory() {
    run_test_once(
        r##"
        require "rubygems"
        require "msgpack"
        require "msgpack/bigint"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        f = MessagePack::Factory.new
        f.register_type(1, Symbol)
        pk = f.packer
        pk.write(:a)
        res << [pk.to_s.bytes, f.unpacker.feed(pk.to_s).read, f.registered_types.map { |h| [h[:type], h[:class]] }]
        f = MessagePack::Factory.new
        f.register_type(-1, Time, packer: MessagePack::Time::Packer, unpacker: MessagePack::Time::Unpacker)
        tm = Time.at(1700000000, 123456789, :nsec)
        res << [f.dump(tm).bytes, f.load(f.dump(tm)) == tm]
        f = MessagePack::Factory.new
        f.register_type(0x01, Integer, packer: MessagePack::Bigint.method(:to_msgpack_ext),
                        unpacker: MessagePack::Bigint.method(:from_msgpack_ext), oversized_integer_extension: true)
        res << [f.dump(2**70).bytes, f.load(f.dump(2**70)), f.dump(2**64 - 1).bytes, f.dump(-2**63).bytes, f.dump(-2**63 + 1).bytes[0]]
        res << t.() { MessagePack::Factory.new.register_type(1, String, oversized_integer_extension: true) }
        f = MessagePack::Factory.new
        pt = Struct.new(:x, :y)
        f.register_type(2, pt, packer: ->(o, pk) { pk.write(o.x); pk.write(o.y) }, unpacker: ->(u) { pt.new(u.read, u.read) }, recursive: true)
        d = f.dump([pt.new(1, "a")])
        res << [d.bytes, f.load(d).map(&:to_a)]
        res << t.() { f = MessagePack::Factory.new.freeze; f.register_type(1, Symbol) }
        f = MessagePack::Factory.new
        f.register_type(1, Symbol)
        g = f.dup
        g.register_type(2, Time, packer: ->(t) { "" }, unpacker: ->(d) { nil })
        res << [f.registered_types.size, g.registered_types.size, f.frozen?, f.freeze.frozen?]
        res << t.() { MessagePack::Factory.new.register_type(300, Symbol) }
        res << t.() { MessagePack::Factory.new(1) }
        f = MessagePack::Factory.new
        k = Class.new(String)
        f.register_type(9, k, packer: ->(s) { "sub:" + s }, unpacker: ->(d) { d })
        res << [f.dump(k.new("x")).bytes, f.dump("x").bytes]
        f = MessagePack::Factory.new
        m = Module.new
        c = Class.new { include m; def to_s; "obj"; end }
        f.register_type(3, m, packer: ->(o) { o.to_s }, unpacker: ->(d) { d })
        res << f.dump(c.new).bytes
        f = MessagePack::Factory.new
        f.register_type(0, Symbol, optimized_symbols_parsing: true)
        res << [f.dump(:abc).bytes, f.load(f.dump(:abc))]
        f = MessagePack::Factory.new
        pool = f.pool(2)
        res << [pool.dump([1]).bytes, pool.load(pool.dump({"a" => 1}))]
        res << [MessagePack::ExtensionValue.name, MessagePack::ExtensionValue.new(1, "a").to_a]
        res
        "##,
    );
}
