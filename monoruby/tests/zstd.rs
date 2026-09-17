extern crate monoruby;
use monoruby::tests::*;

// The zstd-ruby gem over monoruby's stand-in for zstdruby.so
// (gem/zstd-ruby/zstdruby.rb + src/builtins/zstd.rs, the bundled libzstd).
// Every case is compared against the host CRuby, which runs the gem's real
// C extension over the same libzstd version — so the compressed bytes are
// expected to be identical, not just to round-trip.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and zstd-ruby is an ordinary gem.

/// One-shot `Zstd.compress` / `decompress`: the libzstd version number
/// (the gem's own `Zstd::VERSION` is whatever gem is installed, so it is
/// not compared), the bytes (a frame with the content size, so
/// `decompress` sizes its buffer from the header), the
/// levels (positional or `level:`), an empty input, the result
/// encodings, a dictionary as bytes / `CDict` / `DDict` (identical output
/// either way), the deprecated `*_using_dict` forms, a frame without a
/// content size (streamed out), and a larger input.
#[test]
fn zstd_one_shot() {
    run_test_once(
        r##"
        require "rubygems"
        require "zstd-ruby"
        data = ("hello zstd world " * 200).b
        dict = ("the quick brown fox jumps over the lazy dog " * 50).b
        res = []
        res << Zstd.zstd_version
        c = Zstd.compress(data)
        res << [c.bytesize, c.bytes, c.encoding.name]
        res << [1, 3, 9, 19, -5].map { |l| Zstd.compress(data, level: l).bytes }
        res << Zstd.compress(data, 1) == Zstd.compress(data, level: 1)
        res << Zstd.compress("").bytes
        res << [Zstd.decompress(c) == data, Zstd.decompress(Zstd.compress("abc")).encoding.name]
        c = Zstd.compress(data, dict: dict)
        res << [c.bytes, Zstd.decompress(c, dict: dict) == data]
        cd = Zstd::CDict.new(dict)
        dd = Zstd::DDict.new(dict)
        res << [Zstd.compress(data, dict: cd) == c, Zstd.decompress(c, dict: dd) == data]
        cd = Zstd::CDict.new(dict, 9)
        res << Zstd.compress(data, dict: cd) == Zstd.compress(data, dict: dict, level: 9)
        c = Zstd.compress_using_dict(data, dict)
        res << [c == Zstd.compress(data, dict: dict), Zstd.decompress_using_dict(c, dict) == data]
        s = Zstd::StreamingCompress.new
        c = s.compress(data) + s.finish
        res << [c.bytes, Zstd.decompress(c) == data]
        big = (0...100_000).map { |i| (i * 7919 % 251).chr }.join.b
        res << [Zstd.compress(big).bytesize, Zstd.decompress(Zstd.compress(big)) == big]
        res
        "##,
    );
}

/// What the module refuses: input that is not a zstd frame, a truncated
/// frame, non-String arguments, unknown keywords, a `dict:` of the wrong
/// kind, duplicating a dictionary, a dictionary-id mismatch, a bad stream,
/// a skippable-frame magic variant out of range, and `decompress` on a
/// skippable frame.
#[test]
fn zstd_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "zstd-ruby"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        data = ("hello zstd world " * 200).b
        dict = ("the quick brown fox jumps over the lazy dog " * 50).b
        res = []
        res << t.() { Zstd.decompress("garbage!") }
        res << t.() { Zstd.decompress(Zstd.compress(data)[0, 20]) }
        res << t.() { Zstd.decompress(1) }
        res << t.() { Zstd.compress(nil) }
        res << t.() { Zstd.compress("x", foo: 1) }
        res << t.() { Zstd.decompress("x", level: 1) }
        res << t.() { Zstd.compress("x", dict: 1) }
        res << t.() { Zstd.decompress("x", dict: 1) }
        res << [t.() { Zstd::CDict.new(dict).dup }, t.() { Zstd::DDict.new(dict).clone }]
        res << t.() { Zstd.decompress_using_dict(Zstd.compress(data), dict) }
        res << t.() { Zstd::StreamingDecompress.new.decompress("nonsense!!") }
        res << t.() { Zstd.write_skippable_frame("", "m", magic_variant: 16) }
        f = Zstd.write_skippable_frame("", "meta") + Zstd.compress(data)
        res << [Zstd.read_skippable_frame(f), t.() { Zstd.decompress(f) }]
        res << t.() { Zstd::StreamingCompress.new(dict: 1) }
        res << t.() { Zstd::StreamingDecompress.new(foo: 1) }
        # a frame header without a content size, then a reserved block type:
        # the streamed-out path of decompress hits the corruption
        corrupt = "\x28\xb5\x2f\xfd\x00\x58\x07\x00\x00".b
        res << t.() { Zstd.decompress(corrupt) }
        res << t.() { Zstd::StreamingDecompress.new.decompress(corrupt) }
        res
        "##,
    );
}

/// The `String.__zstd_*` builtins' own guards, which the Ruby half never
/// trips (it checks the dictionary's class and owns every handle): a
/// handle of the wrong kind, a closed or negative handle, an end directive
/// out of range, and freeing twice. monoruby only — CRuby has no such
/// methods — so the script checks its own expectations.
#[test]
fn zstd_builtin_guards() {
    run_test_no_result_check(
        r##"
        require "rubygems"
        require "zstd-ruby"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        dict = ("the quick brown fox jumps over the lazy dog " * 50).b
        cd = String.__zstd_cdict_new(dict, nil)
        dd = String.__zstd_ddict_new(dict)
        got = []
        got << [String.__zstd_dict_id(cd), String.__zstd_dict_id(dd)]
        got << t.() { String.__zstd_dict_id(-1) }
        got << t.() { String.__zstd_dict_id(999_999) }
        got << t.() { String.__zstd_compress("x", 3, -1) }
        got << t.() { String.__zstd_compress("x", 3, dd) }
        got << t.() { String.__zstd_decompress(Zstd.compress("x"), cd) }
        cs = String.__zstd_cstream_new(3, cd)
        got << t.() { String.__zstd_cstream_run(cs, "", 3) }
        got << t.() { String.__zstd_cstream_run(999_999, "", 0) }
        got << t.() { String.__zstd_dstream_run(-1, "") }
        ds = String.__zstd_dstream_new(dd)
        got << t.() { String.__zstd_cstream_run(ds, "x", 0) }
        got << t.() { String.__zstd_dstream_run(cs, "x") }
        got << [String.__zstd_dict_free(-1), String.__zstd_stream_free(-1),
                String.__zstd_dict_free(999_999), String.__zstd_stream_free(999_999)]
        got << String.__zstd_dstream_run(ds, Zstd.compress("abc"))
        String.__zstd_stream_free(cs)
        String.__zstd_stream_free(ds)
        got << t.() { String.__zstd_cstream_run(cs, "x", 0) }
        got << t.() { String.__zstd_dstream_run(ds, "x") }
        String.__zstd_dict_free(cd)
        String.__zstd_dict_free(dd)
        got << t.() { String.__zstd_decompress(Zstd.compress("x"), dd) }
        got << t.() { String.__zstd_cstream_new(3, cd) }
        expected = [
          [0, 0],
          [ArgumentError, "closed handle"],
          [ArgumentError, "closed dictionary"],
          [ArgumentError, "closed dictionary"],
          [RuntimeError, "ZSTD_CCtx_refCDict failed"],
          [RuntimeError, "ZSTD_DCtx_refDDict failed"],
          [ArgumentError, "invalid end directive"],
          [ArgumentError, "closed stream"],
          [ArgumentError, "closed handle"],
          [RuntimeError, "compress error error code: No error detected"],
          [RuntimeError, "decompress error error code: No error detected"],
          [nil, nil, nil, nil],
          "abc",
          [ArgumentError, "closed stream"],
          [ArgumentError, "closed stream"],
          [ArgumentError, "closed dictionary"],
          [ArgumentError, "closed dictionary"],
        ]
        got.zip(expected).each_with_index do |(g, e), i|
          raise "case #{i}: #{g.inspect} != #{e.inspect}" unless g == e
        end
        true
        "##,
    );
}

/// The streaming classes: `compress` across chunks and `finish`, `write`
/// (bytes taken, output held for `flush` / `finish`), the IO-style
/// writers, the end-directive constants, dictionaries, decompression in
/// pieces, the gem's `StreamWriter` / `StreamReader`, and the
/// skippable-frame helpers.
#[test]
fn zstd_streaming() {
    run_test_once(
        r##"
        require "rubygems"
        require "zstd-ruby"
        require "stringio"
        data = ("hello zstd world " * 200).b
        dict = ("the quick brown fox jumps over the lazy dog " * 50).b
        res = []
        s = Zstd::StreamingCompress.new
        a = s.compress(data[0, 1000])
        b = s.compress(data[1000..])
        c = s.finish
        res << [a.bytesize, b.bytesize, c.bytes, Zstd.decompress(a + b + c) == data]
        s = Zstd::StreamingCompress.new(level: 5)
        n = s.write("abc", "def")
        f = s.flush
        e = s.finish
        res << [n, f.bytes, e.bytes, Zstd.decompress(f + e), (s << "x").class, s.print("y"), s.puts("z"), s.printf("%d", 1)]
        s = Zstd::StreamingCompress.new
        s << "one"
        s.print("two", "three")
        s.puts("four", ["five"])
        s.printf("%03d", 6)
        res << Zstd.decompress(s.finish)
        res << [Zstd::StreamingCompress::CONTINUE, Zstd::StreamingCompress::FLUSH, Zstd::StreamingCompress.const_get(:END)]
        s = Zstd::StreamingCompress.new(dict: dict)
        c = s.compress(data) + s.finish
        d = Zstd::StreamingDecompress.new(dict: dict)
        res << [c.bytes, d.decompress(c) == data]
        c = Zstd.compress(data)
        d = Zstd::StreamingDecompress.new
        res << (d.decompress(c[0, 30]) + d.decompress(c[30..])) == data
        res << Zstd::StreamingCompress.new(1).class
        io = StringIO.new
        w = Zstd::StreamWriter.new(io)
        w.write("hello ")
        w.write("world")
        w.finish
        io.rewind
        r = Zstd::StreamReader.new(io)
        res << r.read(1000)
        f = Zstd.write_skippable_frame("", "meta")
        res << [f.bytes, Zstd.read_skippable_frame(f), Zstd.read_skippable_frame(Zstd.compress("x")),
                Zstd.write_skippable_frame("ignored", "m", magic_variant: 3).bytes, Zstd.read_skippable_frame("")]
        res
        "##,
    );
}
