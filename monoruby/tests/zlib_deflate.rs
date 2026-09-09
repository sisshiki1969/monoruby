extern crate monoruby;
use monoruby::tests::*;

// `Zlib::Deflate` / `Zlib::Inflate` run on the bundled zlib (libz-sys), so
// the compressed bytes must be exactly what CRuby's zlib.so produces for the
// same level / strategy / window (the oracle holds CRuby's answer). The
// streams are summarised as [bytesize, crc32, leading bytes] to keep the
// snapshot small; a mismatch in any of them means a different bit stream.

#[test]
fn deflate_bytes_match_zlib_for_every_level_and_strategy() {
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
