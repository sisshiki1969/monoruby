extern crate monoruby;
use monoruby::tests::*;

// gzip framing in stdlib/zlib.rb: `Zlib.gzip` / `Zlib.gunzip`, the
// GzipReader / GzipWriter objects (header fields, trailer checks, the
// line-oriented reader API) and the `window_bits` framing selection of
// the streaming Deflate / Inflate objects. `real` is a stream produced by
// CRuby's zlib at level 9 (fixed-Huffman blocks), the rest round-trips
// monoruby's own stored-block output.

#[test]
fn gzip_round_trips_and_reader_writer_api() {
    run_test_once(
        r#"
require "zlib"
require "stringio"
require "tmpdir"
r = []
text = "hello gzip\nsecond line\nthird\n" * 3
gz = Zlib.gzip(text)
r << gz.byteslice(0, 4).bytes
r << (Zlib.gunzip(gz) == text)
# a real gzip stream (produced by CRuby/zlib at level 9): "abcabcabc\n"
real = ["1f8b0800eaaca06a02034b4c4a4e0423aec421cd0200b7cbc922c8000000"].pack("H*")
r << Zlib.gunzip(real)
r << Zlib::GzipReader.new(StringIO.new(real)).read
sio = StringIO.new("".b)
w = Zlib::GzipWriter.new(sio)
w.mtime = Time.at(1_000_000)
w.orig_name = "a.txt"
w.write("line1\n"); w << "line2\n"; w.puts("line3"); w.print("x"); w.putc(0x41)
w.close
data = sio.string
r << Zlib.gunzip(data)
rd = Zlib::GzipReader.new(StringIO.new(data))
r << rd.orig_name << rd.mtime.to_i << rd.os_code << rd.gets << rd.lineno << rd.gets.chomp << rd.eof?
r << rd.read(3) << rd.read << rd.eof? << rd.read(1) << rd.read
rd.rewind
r << rd.readlines << rd.unused
rd.close
r << rd.closed?
Dir.mktmpdir do |d|
  path = File.join(d, "t.gz")
  Zlib::GzipWriter.open(path) { |g| g.write("file contents\n" * 2) }
  r << Zlib::GzipReader.open(path) { |g| g.read }
  r << Zlib::GzipReader.open(path, &:each_line).to_a rescue r << $!.class
  File.open(path, "rb") { |f| r << Zlib::GzipReader.wrap(f) { |g| g.each_line.map(&:chomp) } }
  r << Zlib::GzipReader.zcat(File.open(path, "rb"))
end
r << (begin; Zlib.gunzip("not gzip"); rescue Zlib::GzipFile::Error => e; e.message; end)
bad = gz.dup; bad.setbyte(bad.bytesize - 5, bad.getbyte(bad.bytesize - 5) ^ 1)
r << (begin; Zlib.gunzip(bad); rescue Zlib::GzipFile::CRCError => e; e.message; end)
r << (begin; Zlib.gunzip(gz.byteslice(0, gz.bytesize - 3)); rescue Zlib::GzipFile::Error => e; [e.class, e.message]; end)
i = Zlib::Inflate.new(Zlib::MAX_WBITS + 32); i << gz; r << (i.finish == text)
i = Zlib::Inflate.new(-Zlib::MAX_WBITS); i << Zlib::Deflate.new(6, -Zlib::MAX_WBITS).deflate(text, Zlib::FINISH); r << (i.finish == text)
d = Zlib::Deflate.new(6, Zlib::MAX_WBITS + 16); d << text; r << (Zlib.gunzip(d.finish) == text)
r
        "#,
    );
}
