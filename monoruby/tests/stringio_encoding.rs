extern crate monoruby;
use monoruby::tests::*;

// StringIO encoding behaviour the pure-Ruby stdlib/stringio.rb used to get
// wrong: `set_encoding` / `binmode` retag the backing string (chunky_png
// builds every PNG through `StringIO.new.set_encoding("ASCII-8BIT")`), a
// length-limited `read` answers ASCII-8BIT, and `write` copies bytes across
// encodings instead of raising CompatibilityError.

#[test]
fn stringio_set_encoding_and_binary_reads() {
    run_test_once(
        r#"
        require "stringio"
        r = []
        io = StringIO.new
        io.set_encoding("ASCII-8BIT")
        io << "\x89PNG\r\n\x1a\n".b
        io << "IHDR"
        io << [13].pack("N")
        r << io.string.encoding.name << io.external_encoding.name << io.string.bytesize
        blob = io.string
        rd = StringIO.new(blob, "rb")
        sig = rd.read(8)
        r << sig.encoding.name << (sig == "\x89PNG\r\n\x1a\n".b) << rd.read(4).bytes << rd.read(4).bytes << rd.read.encoding.name << rd.read(1)
        rd.rewind
        r << (rd.read(8) == "\x89PNG\r\n\x1a\n".b)
        b = StringIO.new(+"abc")
        b.binmode
        r << b.string.encoding.name << b.external_encoding.name
        w = StringIO.new("".b)
        w.write("café")
        w.write("\xff\xfe".b)
        r << w.string.encoding.name << w.string.bytes
        t = StringIO.new(+"")
        t.write("café")
        t.write("x".b)
        r << t.string.encoding.name << t.string.bytes
        f = StringIO.new("frozen".freeze)
        f.set_encoding(Encoding::BINARY)
        r << f.string.encoding.name << f.external_encoding.name
        r
        "#,
    );
}
