extern crate monoruby;
use monoruby::tests::*;

// `Zlib::Deflate` / `Zlib::Inflate` run on the bundled zlib (libz-sys), so
// the compressed bytes must be exactly what CRuby's zlib.so produces for the
// same level / strategy / window (the oracle holds CRuby's answer). The
// streams are summarised as [bytesize, crc32, leading bytes] to keep the
// snapshot small; a mismatch in any of them means a different bit stream.

#[test]
fn deflate_bytes_match_zlib_for_every_level_and_strategy() {
    ensure_extension("zlib_native");
    run_test_once(
        r#"
        require "zlib"
        # Deterministic, mildly compressible input: prose with repeats plus a
        # pseudo-random tail (an LCG, so both sides agree byte for byte).
        prose = ("The quick brown fox jumps over the lazy dog. " * 40) +
                ("Pack my box with five dozen liquor jugs. " * 25) +
                (1..300).map { |i| "line #{i}: value=#{i * 7919 % 1000}\n" }.join
        x = 12345
        noise = Array.new(4000) { x = (x * 1103515245 + 12345) & 0x7fffffff; (x >> 16) & 0xff }.pack("C*")
        data = (prose + noise + prose).b
        sig = ->(z) { [z.bytesize, Zlib.crc32(z), z.byteslice(0, 12).unpack("C*")] }
        r = {}
        (0..9).each { |lv| r["level#{lv}"] = sig.(Zlib::Deflate.deflate(data, lv)) }
        r["default"] = sig.(Zlib::Deflate.deflate(data))
        [Zlib::FILTERED, Zlib::HUFFMAN_ONLY, Zlib::RLE, Zlib::FIXED].each do |st|
          d = Zlib::Deflate.new(6, Zlib::MAX_WBITS, Zlib::DEF_MEM_LEVEL, st)
          r["strategy#{st}"] = sig.(d.deflate(data, Zlib::FINISH))
          d.close
        end
        # Window / memory variants and the raw / gzip wrappers.
        r["w12m6"] = sig.(Zlib::Deflate.new(9, 12, 6).deflate(data, Zlib::FINISH))
        r["raw"] = sig.(Zlib::Deflate.new(6, -Zlib::MAX_WBITS).deflate(data, Zlib::FINISH))
        r["gzip"] = sig.(Zlib::Deflate.new(6, Zlib::MAX_WBITS + 16).deflate(data, Zlib::FINISH))
        # Chunked feeding with NO_FLUSH yields the same stream as one shot.
        d = Zlib::Deflate.new(9, Zlib::MAX_WBITS, 6)
        chunked = "".b
        data.bytes.each_slice(1000) { |c| chunked << d.deflate(c.pack("C*")) }
        chunked << d.finish
        d.close
        r["chunked==oneshot"] = chunked == Zlib::Deflate.deflate(data, 9)
        # SYNC_FLUSH / FULL_FLUSH mid-stream.
        d = Zlib::Deflate.new(6)
        s = d.deflate(data.byteslice(0, 3000), Zlib::SYNC_FLUSH)
        s << d.deflate(data.byteslice(3000, 3000), Zlib::FULL_FLUSH)
        s << d.deflate(data.byteslice(6000..), Zlib::FINISH)
        r["flushes"] = sig.(s)
        r["flushes_roundtrip"] = Zlib::Inflate.inflate(s) == data
        # Every stream inflates back to the input.
        r["roundtrip"] = (0..9).all? { |lv| Zlib::Inflate.inflate(Zlib::Deflate.deflate(data, lv)) == data }
        r["raw_roundtrip"] = Zlib::Inflate.new(-Zlib::MAX_WBITS).inflate(Zlib::Deflate.new(6, -Zlib::MAX_WBITS).deflate(data, Zlib::FINISH)) == data
        r["auto_roundtrip"] = Zlib::Inflate.new(Zlib::MAX_WBITS + 32).inflate(Zlib::Deflate.new(6, Zlib::MAX_WBITS + 16).deflate(data, Zlib::FINISH)) == data
        r
        "#,
    );
}

#[test]
fn inflate_streaming_and_errors() {
    ensure_extension("zlib_native");
    run_test_once(
        r#"
        require "zlib"
        data = ("abc" * 5000 + "xyz" * 100).b
        z = Zlib::Deflate.deflate(data, 9)
        r = {}
        i = Zlib::Inflate.new
        out = "".b
        z.bytes.each_slice(700) { |c| out << i.inflate(c.pack("C*")) }
        out << i.finish
        r["chunked"] = [out == data, i.finished?, i.total_in, i.total_out]
        i.close
        r["closed"] = [i.closed?, (begin; i.inflate("x"); rescue Zlib::Error => e; e.message; end)]
        i = Zlib::Inflate.new
        i << z.byteslice(0, 40)
        r["partial_finish"] = (begin; i.finish; rescue Zlib::BufError => e; e.message; end)
        r["trailing"] = (begin; j = Zlib::Inflate.new; o = j.inflate(z + "junk"); [o == data, j.finished?, j.total_in == z.bytesize]; end)
        r["corrupt"] = (begin; Zlib::Inflate.inflate("\x78\x9c\xff\xff\xff\xff".b); rescue Zlib::DataError => e; e.class; end)
        r["bad_header"] = (begin; Zlib::Inflate.inflate("not zlib at all".b); rescue Zlib::DataError => e; e.message; end)
        r["dict"] = (begin
          dict = "abc" * 10
          d = Zlib::Deflate.new(6); d.set_dictionary(dict)
          zd = d.deflate("abcabcabcabc", Zlib::FINISH)
          j = Zlib::Inflate.new
          begin
            j.inflate(zd)
          rescue Zlib::NeedDict
            j.set_dictionary(dict)
            j.inflate("")
          end
        end)
        r["bad_level"] = (begin; Zlib::Deflate.new(10); rescue Zlib::StreamError => e; e.message; end)
        r["adler"] = (d = Zlib::Deflate.new; d.deflate(data, Zlib::FINISH); [d.adler == Zlib.adler32(data), d.total_in, d.finished?])
        r["module_fns"] = Zlib.inflate(Zlib.deflate(data, 1)) == data
        r
        "#,
    );
}

#[test]
fn deflate_params_reset_dictionary_and_stream_state() {
    // The remaining `__zstream_*` entry points: `params` mid-stream (the
    // bytes flushed by the old settings, then the rest), `reset` reusing a
    // Deflate and an Inflate, `set_dictionary` on the inflate side after
    // NeedDict, and the counters / flags around them.
    ensure_extension("zlib_native");
    run_test_once(
        r#"
        require "zlib"
        data = (("the quick brown fox jumps over the lazy dog. " * 40) + ("0123456789" * 200) + ("\x00\x01\x02\x03" * 500)).b
        sig = ->(s) { [s.bytesize, Zlib.crc32(s)] }
        r = {}
        # (`params` right after `Deflate#deflate` raises StreamError in CRuby
        # and on a fresh stream crashes it, so feed with `<<` first.)
        d = Zlib::Deflate.new(1)
        d << data.byteslice(0, 4000)
        d.params(9, Zlib::FILTERED)
        d << data.byteslice(4000..)
        z = d.finish
        r["params"] = [sig.(z), Zlib::Inflate.inflate(z) == data, d.total_in, d.total_out == z.bytesize, d.finished?]
        r["params_closed"] = (begin; d.close; d.params(1, Zlib::DEFAULT_STRATEGY); rescue Zlib::Error => e; e.message; end)
        d = Zlib::Deflate.new(6)
        first = d.deflate(data, Zlib::FINISH)
        d.reset
        second = d.deflate(data, Zlib::FINISH)
        r["deflate_reset"] = [first == second, first == Zlib::Deflate.deflate(data, 6), d.total_in]
        i = Zlib::Inflate.new
        one = i.inflate(first)
        i.reset
        two = i.inflate(second)
        r["inflate_reset"] = [one == data, two == data, i.finished?, i.total_out]
        dict = "the quick brown fox jumps over the lazy dog. ".b
        d = Zlib::Deflate.new(6)
        d.set_dictionary(dict)
        zd = d.deflate(data.byteslice(0, 2000), Zlib::FINISH)
        i = Zlib::Inflate.new
        out = begin
          i.inflate(zd)
        rescue Zlib::NeedDict
          i.set_dictionary(dict)
          i.inflate("")
        end
        r["inflate_dict"] = [out == data.byteslice(0, 2000), i.finished?, (begin; Zlib::Inflate.new.tap { |j| j.inflate(zd) rescue j.set_dictionary("wrong dictionary") }; :ok; rescue Zlib::DataError => e; e.message; end)]
        d = Zlib::Deflate.new
        d << data.byteslice(0, 100)
        r["state"] = [d.data_type.is_a?(Integer), d.avail_in, d.avail_out.is_a?(Integer), d.finished?, d.flush(Zlib::SYNC_FLUSH).bytesize > 0, d.flush_next_out.class, d.flush_next_in.class]
        d.finish
        r["after_finish"] = [d.finished?, d.total_in, d.closed?]
        r["version"] = [Zlib.zlib_version.is_a?(String), Zlib::VERSION.is_a?(String)]
        r["crc"] = [Zlib.crc32, Zlib.crc32("abc"), Zlib.crc32("c", Zlib.crc32("ab")), Zlib.adler32, Zlib.adler32("abc"), Zlib.adler32("c", Zlib.adler32("ab")), Zlib.crc32("abc", 0), Zlib.crc_table.size]
        r
        "#,
    );
}

// The checks that lived beside the native half while it was in the core
// (`src/builtins/zlib.rs`): the checksum argument semantics, stored-block
// deflate, and inflating streams a real zlib produced. `run_tests` spawns
// the host CRuby for each expression.

#[test]
fn zlib_checksums() {
    ensure_extension("zlib_native");
    run_tests(&[
        r#"require "zlib"; [Zlib.crc32, Zlib.adler32, Zlib.crc32(nil), Zlib.crc32(nil, 5), Zlib.adler32(nil, 5), Zlib.crc32("", 5), Zlib.adler32("", 5)]"#,
        r#"require "zlib"; [Zlib.crc32("abc"), Zlib.adler32("abc"), Zlib.crc32("abc", 2**32 - 1), Zlib.crc32("abc", 2**32), Zlib.crc32("abc", 2**40 + 7), Zlib.crc32("abc", -1), Zlib.crc32("abc", 1.5)]"#,
        r#"require "zlib"; [Zlib.crc32("あ"), Zlib.adler32("あ"), Zlib.crc32("abc", Zlib.crc32("IDAT"))]"#,
        r#"require "zlib"; o = Object.new; def o.to_str; "abc"; end; [Zlib.crc32(o), Zlib.adler32(o)]"#,
        r#"require "zlib"; s = ("x" * 7000) + (0..255).map(&:chr).join; [Zlib.crc32(s), Zlib.adler32(s), Zlib.crc32(s, Zlib.crc32(s))]"#,
        // `crc32_combine(_, crc2, 0)` only with `crc2 == 0`: zlib < 1.2.12
        // short-circuits a zero `len2` to `crc1` where 1.2.12+ still XORs
        // `crc2` in, so any other seed pair depends on the host's zlib.
        r#"require "zlib"; a = "abc" * 10; b = "defg" * 500; [Zlib.crc32_combine(Zlib.crc32(a), Zlib.crc32(b), b.bytesize) == Zlib.crc32(a + b), Zlib.adler32_combine(Zlib.adler32(a), Zlib.adler32(b), b.bytesize) == Zlib.adler32(a + b), Zlib.crc32_combine(7, 0, 0), Zlib.adler32_combine(7, 9, 0)]"#,
    ]);
    // The stub follows zlib 1.2.12+ (`crc1 ^ crc2` even for a zero
    // `len2`); pin that without consulting the host's CRuby, whose
    // linked zlib may be older and answer `crc1`.
    assert!(run_test_no_result_check(r#"require "zlib"; Zlib.crc32_combine(7, 9, 0) == 14"#).as_bool());
    run_test_error(r#"require "zlib"; Zlib.crc32("abc", "1")"#);
    run_test_error(r#"require "zlib"; Zlib.crc32(123)"#);
    run_test_error(r#"require "zlib"; Zlib.adler32(:abc)"#);
}

#[test]
fn zlib_deflate_stored() {
    ensure_extension("zlib_native");
    // Up to one stored block the NO_COMPRESSION output is
    // byte-identical to CRuby's. Past that the split point moved
    // between zlib 1.3 and 1.3.1 (`deflate_stored` keeps a few more
    // bytes back), and which one the host CRuby links varies, so from
    // 65530 bytes on only the framing, the trailer and the round trip
    // are compared. The other levels differ in the header's FLEVEL
    // bits alone, which is all a stored stream can carry of them.
    run_tests(&[
        r#"require "zlib"; [0, 1, 5, 100, 65529].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.bytesize, d.encoding.name, d[0, 7].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
        r#"require "zlib"; [65530, 65531, 65532, 70000, 200000].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.encoding.name, d[0, 2].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
        r#"require "zlib"; s = (0..255).map(&:chr).join * 3; d = Zlib::Deflate.deflate(s, Zlib::NO_COMPRESSION); [d == Zlib::Deflate.deflate(s, 0), Zlib::Inflate.inflate(d) == s.b, Zlib::Inflate.inflate(d).encoding.name]"#,
        r#"require "zlib"; [-1, 0, 1, 2, 5, 6, 7, 9, nil].map { |l| d = l.nil? ? Zlib::Deflate.deflate("abc") : Zlib::Deflate.deflate("abc", l); [d[0, 2].unpack("C*"), Zlib::Inflate.inflate(d)] }"#,
        r#"require "zlib"; d = Zlib::Deflate.new(Zlib::NO_COMPRESSION); d << "abc"; r = [d.finished?, d.total_in]; d << "def"; out = d.finish; r << d.finished? << d.total_out; d.close; r << d.closed?; [out.unpack("C*"), r]"#,
        r#"require "zlib"; d = Zlib::Deflate.new(0); out = d.deflate("hello", Zlib::FINISH); [out.unpack("C*"), Zlib::Inflate.inflate(out)]"#,
        r#"require "zlib"; o = Object.new; def o.to_str; "abc"; end; Zlib::Inflate.inflate(Zlib::Deflate.deflate(o, 0))"#,
    ]);
    run_test_error(r#"require "zlib"; Zlib::Deflate.deflate("abc", 10)"#);
    run_test_error(r#"require "zlib"; Zlib::Deflate.deflate("abc", -2)"#);
    run_test_error(r#"require "zlib"; Zlib::Deflate.deflate(nil)"#);
    run_test_error(r#"require "zlib"; Zlib::Deflate.deflate(123)"#);
    // A closed stream answers nothing but `closed?`.
    run_test_error(r#"require "zlib"; d = Zlib::Deflate.new; d.close; d.finished?"#);
    run_test_error(r#"require "zlib"; d = Zlib::Deflate.new; d.close; d << "x""#);
}

#[test]
fn zlib_inflate() {
    ensure_extension("zlib_native");
    // Streams a real zlib produced: a fixed-Huffman block, a
    // dynamic-Huffman block, and stored blocks with a multi-block
    // split. Each is decoded and compared with its plaintext.
    run_tests(&[
        r#"require "zlib"; Zlib::Inflate.inflate([120, 156, 203, 72, 205, 201, 201, 87, 200, 64, 39, 117, 20, 202, 243, 139, 114, 82, 20, 1, 184, 181, 11, 70].pack("C*"))"#,
        r#"require "zlib"; text = (1..20).map { |i| "line #{i}: #{i * i} #{(i * 7919) % 1000}\n" }.join; d = [120, 218, 45, 207, 203, 13, 67, 49, 8, 68, 209, 253, 171, 98, 74, 240, 240, 179, 113, 63, 89, 68, 122, 74, 255, 203, 96, 153, 229, 69, 8, 29, 222, 239, 239, 3, 110, 16, 201, 124, 222, 83, 178, 97, 88, 186, 110, 233, 70, 98, 250, 188, 101, 181, 25, 136, 25, 55, 125, 67, 28, 158, 126, 51, 54, 52, 224, 180, 155, 179, 14, 37, 76, 245, 230, 218, 8, 131, 186, 220, 204, 141, 69, 200, 228, 77, 142, 58, 61, 6, 152, 163, 7, 71, 37, 172, 97, 187, 88, 48, 154, 65, 90, 70, 61, 152, 68, 90, 227, 120, 116, 25, 88, 209, 60, 30, 95, 1, 231, 106, 32, 227, 128, 3, 115, 52, 145, 101, 148, 149, 8, 105, 36, 75, 169, 98, 112, 107, 38, 243, 60, 69, 88, 52, 84, 10, 106, 5, 213, 53, 158, 63, 235, 56, 74, 2].pack("C*"); [Zlib::Inflate.inflate(d) == text, Zlib::Inflate.inflate(d).encoding.name]"#,
        r#"require "zlib"; s = ("ab" * 40000) + "\x00\xff".b * 10; d = Zlib::Deflate.deflate(s, 0); i = Zlib::Inflate.new; i << d[0, 1000]; i << d[1000..]; out = i.finish; i.close; [out == s.b, out.bytesize, i.closed?]"#,
        r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x27".b)"#,
        r#"require "zlib"; [Zlib::Inflate.inflate("\x78\x01\x03\x00\x00\x00\x00\x01".b), Zlib::Inflate.inflate(Zlib::Deflate.deflate("", 0))]"#,
    ]);
    // Bad header, bad Adler-32, truncated stream, preset dictionary.
    run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("garbage")"#);
    run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x28".b)"#);
    run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffab".b)"#);
    run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x20\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x27".b)"#);
    run_test_error(r#"require "zlib"; Zlib::Inflate.inflate(nil)"#);
}
