use super::*;
use std::sync::LazyLock;

//
// Zlib checksum backend.
//
// `Zlib` itself is the pure-Ruby stub in `stdlib/zlib.rb` (monoruby cannot
// load zlib.so). Its `Zlib.crc32` / `Zlib.adler32` used to be Ruby loops
// over `each_byte` — 50 ns a byte, against the 0.3 ns of zlib's table
// walk — and chunky_png runs a CRC over every 170 KB IDAT it writes. The
// stub keeps the argument semantics (`nil`, `to_str`, the 32-bit mask on
// the seed) and hands the byte walk to these two helpers, the same split
// as `String.__digest` for `Digest`.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__crc32", crc32, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__adler32", adler32, 2);
}

/// String.__crc32(data, crc) -> Integer
///
/// zlib's `crc32(crc, data)`: `data` must be a String and `crc` an
/// Integer already reduced to 32 bits.
#[monoruby_builtin]
fn crc32(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data_v = lfp.arg(0);
    let data = data_v.expect_bytes(&globals.store)?;
    let seed = lfp.arg(1).expect_integer(&globals.store)? as u32;
    Ok(Value::integer(crc32_update(seed, data) as i64))
}

/// String.__adler32(data, adler) -> Integer
///
/// zlib's `adler32(adler, data)`, same contract as `__crc32`.
#[monoruby_builtin]
fn adler32(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data_v = lfp.arg(0);
    let data = data_v.expect_bytes(&globals.store)?;
    let seed = lfp.arg(1).expect_integer(&globals.store)? as u32;
    Ok(Value::integer(adler32_update(seed, data) as i64))
}

/// Slicing-by-8 tables for the reflected CRC-32 (polynomial 0xEDB88320):
/// `TABLES[k][b]` is the CRC contribution of byte `b` sitting `k` bytes
/// before the end of an 8-byte word.
static CRC_TABLES: LazyLock<[[u32; 256]; 8]> = LazyLock::new(|| {
    let mut t = [[0u32; 256]; 8];
    for i in 0..256u32 {
        let mut c = i;
        for _ in 0..8 {
            c = if c & 1 == 1 { 0xEDB8_8320 ^ (c >> 1) } else { c >> 1 };
        }
        t[0][i as usize] = c;
    }
    for k in 1..8 {
        for i in 0..256 {
            let prev = t[k - 1][i];
            t[k][i] = t[0][(prev & 0xff) as usize] ^ (prev >> 8);
        }
    }
    t
});

/// `crc32(crc, buf, len)`: continue the CRC-32 `crc` over `data`.
pub(crate) fn crc32_update(crc: u32, data: &[u8]) -> u32 {
    let t = &*CRC_TABLES;
    let mut crc = !crc;
    let mut chunks = data.chunks_exact(8);
    for c in &mut chunks {
        let lo = u32::from_le_bytes([c[0], c[1], c[2], c[3]]) ^ crc;
        let hi = u32::from_le_bytes([c[4], c[5], c[6], c[7]]);
        crc = t[7][(lo & 0xff) as usize]
            ^ t[6][((lo >> 8) & 0xff) as usize]
            ^ t[5][((lo >> 16) & 0xff) as usize]
            ^ t[4][(lo >> 24) as usize]
            ^ t[3][(hi & 0xff) as usize]
            ^ t[2][((hi >> 8) & 0xff) as usize]
            ^ t[1][((hi >> 16) & 0xff) as usize]
            ^ t[0][(hi >> 24) as usize];
    }
    for &b in chunks.remainder() {
        crc = t[0][((crc ^ b as u32) & 0xff) as usize] ^ (crc >> 8);
    }
    !crc
}

/// `adler32(adler, buf, len)`: continue the Adler-32 `adler` over `data`.
pub(crate) fn adler32_update(adler: u32, data: &[u8]) -> u32 {
    const BASE: u32 = 65521;
    // The largest run of bytes whose sums stay inside a u32 (zlib's NMAX).
    const NMAX: usize = 5552;
    let mut a = adler & 0xffff;
    let mut b = (adler >> 16) & 0xffff;
    for run in data.chunks(NMAX) {
        for &byte in run {
            a += byte as u32;
            b += a;
        }
        a %= BASE;
        b %= BASE;
    }
    (b << 16) | a
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    fn crc32_naive(crc: u32, data: &[u8]) -> u32 {
        let mut c = !crc;
        for &b in data {
            c ^= b as u32;
            for _ in 0..8 {
                c = if c & 1 == 1 { 0xEDB8_8320 ^ (c >> 1) } else { c >> 1 };
            }
        }
        !c
    }

    fn adler32_naive(adler: u32, data: &[u8]) -> u32 {
        let mut a = (adler & 0xffff) as u64;
        let mut b = ((adler >> 16) & 0xffff) as u64;
        for &byte in data {
            a = (a + byte as u64) % 65521;
            b = (b + a) % 65521;
        }
        ((b as u32) << 16) | a as u32
    }

    #[test]
    fn checksums_match_the_reference_values() {
        assert_eq!(crc32_update(0, b"123456789"), 0xCBF4_3926);
        assert_eq!(crc32_update(0, b""), 0);
        assert_eq!(adler32_update(1, b"Wikipedia"), 0x11E6_0398);
        assert_eq!(adler32_update(1, b""), 1);
    }

    #[test]
    fn sliced_crc_and_chunked_adler_agree_with_the_byte_loops() {
        // Every remainder length around the 8-byte slice, a seed, and a
        // run long enough to cross the Adler NMAX boundary.
        let data: Vec<u8> = (0..20_000u32)
            .map(|i| (i.wrapping_mul(2654435761) >> 13) as u8)
            .collect();
        for len in (0..40).chain([5551, 5552, 5553, 11104, 20_000]) {
            let d = &data[..len];
            assert_eq!(crc32_update(0, d), crc32_naive(0, d), "crc len {len}");
            assert_eq!(crc32_update(0xDEAD_BEEF, d), crc32_naive(0xDEAD_BEEF, d), "crc seed len {len}");
            assert_eq!(adler32_update(1, d), adler32_naive(1, d), "adler len {len}");
            assert_eq!(adler32_update(0x1234_5678, d), adler32_naive(0x1234_5678, d), "adler seed len {len}");
        }
        // Splitting a run continues the checksum exactly.
        let (l, r) = data.split_at(777);
        assert_eq!(crc32_update(crc32_update(0, l), r), crc32_update(0, &data));
        assert_eq!(adler32_update(adler32_update(1, l), r), adler32_update(1, &data));
    }

    #[test]
    fn zlib_checksums() {
        run_tests(&[
            r#"require "zlib"; [Zlib.crc32, Zlib.adler32, Zlib.crc32(nil), Zlib.crc32(nil, 5), Zlib.adler32(nil, 5), Zlib.crc32("", 5), Zlib.adler32("", 5)]"#,
            r#"require "zlib"; [Zlib.crc32("abc"), Zlib.adler32("abc"), Zlib.crc32("abc", 2**32 - 1), Zlib.crc32("abc", 2**32), Zlib.crc32("abc", 2**40 + 7), Zlib.crc32("abc", -1), Zlib.crc32("abc", 1.5)]"#,
            r#"require "zlib"; [Zlib.crc32("あ"), Zlib.adler32("あ"), Zlib.crc32("abc", Zlib.crc32("IDAT"))]"#,
            r#"require "zlib"; o = Object.new; def o.to_str; "abc"; end; [Zlib.crc32(o), Zlib.adler32(o)]"#,
            r#"require "zlib"; s = ("x" * 7000) + (0..255).map(&:chr).join; [Zlib.crc32(s), Zlib.adler32(s), Zlib.crc32(s, Zlib.crc32(s))]"#,
            r#"require "zlib"; a = "abc" * 10; b = "defg" * 500; [Zlib.crc32_combine(Zlib.crc32(a), Zlib.crc32(b), b.bytesize) == Zlib.crc32(a + b), Zlib.adler32_combine(Zlib.adler32(a), Zlib.adler32(b), b.bytesize) == Zlib.adler32(a + b), Zlib.crc32_combine(7, 9, 0), Zlib.adler32_combine(7, 9, 0)]"#,
        ]);
        run_test_error(r#"require "zlib"; Zlib.crc32("abc", "1")"#);
        run_test_error(r#"require "zlib"; Zlib.crc32(123)"#);
        run_test_error(r#"require "zlib"; Zlib.adler32(:abc)"#);
    }

    #[test]
    fn zlib_deflate_stored() {
        // Up to one stored block the NO_COMPRESSION output is
        // byte-identical to CRuby's. Past that zlib splits where its
        // caller's output buffer happens to end, so only the framing,
        // the trailer and the round trip are compared. The other levels
        // differ in the header's FLEVEL bits alone, which is all a
        // stored stream can carry of them.
        run_tests(&[
            r#"require "zlib"; [0, 1, 5, 100, 65530].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.bytesize, d.encoding.name, d[0, 7].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
            r#"require "zlib"; [65531, 65532, 70000, 200000].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.encoding.name, d[0, 3].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
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
}
