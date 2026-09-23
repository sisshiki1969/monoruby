extern crate monoruby;
use monoruby::tests::*;

// The encodings #1530 found without a converter, or with one that
// stopped short: IBM037 (EBCDIC through ISO-8859-1), the transcoders
// CRuby spells in upper case, and stateless-ISO-2022-JP-KDDI /
// ISO-2022-JP-KDDI — CP51932's cells behind a 0x92 lead with the KDDI
// emoji in rows F5..FB, converted to and from UTF8-KDDI. CP51932 gained
// its NEC row 13 and NEC-selected IBM extension rows on the way.

/// IBM037 is a permutation of Latin-1: every byte reads, every Latin-1
/// character writes, and anything above U+00FF is refused at the
/// ISO-8859-1 hop.
#[test]
fn ibm037_is_latin1_permuted() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { "ab".encode("IBM037") }
        res << t.() { "é".encode("IBM037") }
        res << t.() { "あ".encode("IBM037") }
        res << t.() { "aあb".encode("IBM037", undef: :replace) }
        res << t.() { "\xc1".force_encoding("IBM037").encode("UTF-8") }
        res << t.() { "\xff".force_encoding("IBM037").encode("UTF-8") }
        res << t.() { (0..255).map { |b| [b].pack("C").force_encoding("IBM037").encode("UTF-8").ord } }
        res << t.() { "\xe9".force_encoding("ISO-8859-1").encode("IBM037") }
        res << t.() { "あ".encode("Shift_JIS").encode("IBM037") }
        res << t.() { "ab".encode("UTF-16LE").encode("IBM037") }
        res << t.() { "\x81".force_encoding("IBM037").encode("UTF-16LE") }
        res << t.() { "a\nb".encode("IBM037", crlf_newline: true) }
        res << t.() { "\xff\x00".force_encoding("IBM037").valid_encoding? }
        res << t.() { Encoding::Converter.new("UTF-8", "IBM037").convpath.map { |p| p.map(&:to_s) } }
        res << t.() { Encoding::Converter.new("IBM037", "UTF-8").convpath.map { |p| p.map(&:to_s) } }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "IBM037"); [ec.convert("ab"), ec.finish].map(&:bytes) }
        res << t.() { ec = Encoding::Converter.new("IBM037", "UTF-8"); [ec.convert("\x81\xC1".b), ec.finish] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "IBM037"); [ec.primitive_convert("あ".dup, "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("IBM037", "EUC-JP"); [ec.primitive_convert("\x81".b, "".dup), ec.primitive_errinfo] }
        res
        "##,
    );
}

/// The Windows and Mac code pages' transcoders carry the name in upper
/// case, and a conversion that ends at one is written out in full.
#[test]
fn transcoders_spelled_in_upper_case() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        %w[Windows-874 Windows-1252 macRoman KOI8-R IBM437 Windows-31J].each do |e|
          res << t.() { "あ".encode(e) }
          res << t.() { "あ".encode("EUC-JP").encode(e) }
          res << t.() { ec = Encoding::Converter.new("UTF-8", e); [ec.primitive_convert("あ".dup, "".dup), ec.primitive_errinfo] }
        end
        res << t.() { "\xa1".force_encoding("Windows-874").encode("UTF-8") }
        res << t.() { "\xdb".force_encoding("Windows-874").encode("UTF-8") }
        res << t.() { "\xdb".force_encoding("Windows-874").encode("Shift_JIS") }
        res << t.() { ec = Encoding::Converter.new("Windows-874", "UTF-8"); [ec.primitive_convert("\xdb".b, "".dup), ec.primitive_errinfo] }
        res
        "##,
    );
}

/// The KDDI emoji: each has a cell in the stateless encoding and a
/// private-use character in UTF8-KDDI, and reaches Unicode, the other
/// carriers and the ordinary encodings through it.
#[test]
fn kddi_emoji_have_their_cells() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        sk = "stateless-ISO-2022-JP-KDDI"
        res = []
        sun = "\u{E488}"
        %w[ISO-2022-JP-KDDI stateless-ISO-2022-JP-KDDI UTF8-KDDI SJIS-KDDI].each do |e|
          res << t.() { sun.encode(e) }
          res << t.() { ("あ" + sun + "a").encode(e) }
        end
        res << t.() { "☀".encode(sk) }
        res << t.() { "☀".encode("ISO-2022-JP-KDDI") }
        res << t.() { "☀".encode("UTF-16BE").encode(sk) }
        res << t.() { sun.force_encoding("UTF8-KDDI").encode(sk) }
        res << t.() { "\u{E63E}".force_encoding("UTF8-DoCoMo").encode(sk) }
        res << t.() { "\xf6\x60".force_encoding("SJIS-KDDI").encode("ISO-2022-JP-KDDI") }
        res << t.() { "\u{E63E}".encode("ISO-2022-JP-KDDI") }
        res << t.() { "\e$B\x75\x41\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8").codepoints }
        res << t.() { "\e$B\x24\x22\x75\x41\e(Ba".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8").codepoints }
        res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode("UTF8-KDDI").codepoints }
        res << t.() { "\x92\xf5\xa1".force_encoding(sk).encode("UTF8-KDDI").codepoints }
        res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode("UTF8-DoCoMo").codepoints }
        res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode("SJIS-KDDI") }
        res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode("SJIS-DoCoMo") }
        res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode("UTF-16BE") }
        %w[EUC-JP Shift_JIS ISO-2022-JP stateless-ISO-2022-JP CP50220 Windows-874 IBM037].each do |e|
          res << t.() { "\x92\xf5\xc1".force_encoding(sk).encode(e) }
          res << t.() { ec = Encoding::Converter.new(sk, e); [ec.primitive_convert("\x92\xf5\xc1".b, "".dup), ec.primitive_errinfo] }
        end
        res << t.() { "\x92\xf5\xc1\x92\xa4\xa2".force_encoding(sk).encode("EUC-JP", undef: :replace) }
        res << t.() { "\e$B\x75\x41\e(B".force_encoding("ISO-2022-JP-KDDI").encode("EUC-JP") }
        res << t.() { "\x92\xf9\xa1".force_encoding(sk).encode("UTF-8") }
        res << t.() { "\x92\xf9\xa1\x92\xa4\xa2".force_encoding(sk).encode("UTF-8", undef: :replace) }
        res << t.() { "\x92\xf9\xa1".force_encoding(sk).encode("UTF-16BE") }
        res << t.() { "\e$B\x79\x21\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.primitive_convert("\x92\xf9\xa1".b, "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); [ec.primitive_convert("\e$B\x79\x21\e(B".b, "".dup), ec.primitive_errinfo] }
        res << t.() { "\x92\xf9\xa1".force_encoding(sk).encode("UTF8-KDDI").encode(sk) }
        res << t.() { Encoding::Converter.search_convpath("UTF-8", sk).map { |p| p.map(&:to_s) } }
        res << t.() { Encoding::Converter.search_convpath(sk, "Shift_JIS").map { |p| p.map(&:to_s) } }
        res << t.() { Encoding::Converter.new(sk, "ISO-2022-JP").convpath.map { |p| p.map(&:to_s) } }
        res << t.() { Encoding::Converter.new("ISO-2022-JP", "ISO-2022-JP-KDDI").convpath.map { |p| p.map(&:to_s) } }
        res << t.() { Encoding::Converter.new("CP50220", sk).convpath.map { |p| p.map(&:to_s) } }
        res
        "##,
    );
}

/// The rest of stateless-ISO-2022-JP-KDDI is CP51932 behind the lead:
/// NEC row 13 and the IBM extension rows read and write, the cells
/// CP51932 holds twice go into the NEC one, and what CP51932 spells
/// outside the two-byte plane — half-width kana, JIS X 0212 — has no
/// cell, reported as the UTF-8 bytes against the hop from UTF8-KDDI.
#[test]
fn stateless_kddi_is_cp51932_behind_a_lead() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        sk = "stateless-ISO-2022-JP-KDDI"
        res = []
        %w[〜 ～ ¬ ① 髙 ∥ ‖ ￢ ￤ 　 ¢ £ ¥ 纊 × ≒ Ⅰ ⅰ ㈱ ｱ 丂].each do |c|
          res << t.() { c.encode(sk) }
          res << t.() { c.encode("ISO-2022-JP-KDDI") }
        end
        res << t.() { "\x92\xad\xa1\x92\xfc\xe2\x92\xfc\xfb\x92\xa1\xc1\x92\xf9\xa1".force_encoding(sk).encode("UTF8-KDDI").codepoints }
        res << t.() { (0xa1..0xfe).map { |c| [0x92, 0xad, c].pack("C*").force_encoding(sk).encode("UTF-8") rescue nil }.compact.join }
        res << t.() { (0xf4..0xfe).map { |c| [0x92, 0xfb, c].pack("C*").force_encoding(sk).encode("UTF8-KDDI").codepoints } }
        res << t.() { "\x92\xfc\xa1".force_encoding(sk).encode("UTF-8") }
        res << t.() { "\e$B\x2d\x21\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { "\x92\xa2\xe2".force_encoding(sk).encode("UTF-8").encode(sk) }
        res << t.() { "\x92\xa2\xe2".force_encoding(sk).encode("ISO-2022-JP-KDDI") }
        res << t.() { "\e$B\x22\x62\e(B".force_encoding("ISO-2022-JP-KDDI").encode(sk) }
        res << t.() { ec = Encoding::Converter.new("UTF-8", sk); [ec.primitive_convert("ｱ".dup, "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP-KDDI"); [ec.primitive_convert("ｱ".dup, "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF-16LE", sk); [ec.primitive_convert("ｱ".encode("UTF-16LE"), "".dup), ec.primitive_errinfo] }
        res << t.() { "aｱb".encode(sk, undef: :replace) }
        res << t.() { "aｱb".encode(sk, undef: :replace, replace: "あ") }
        res << t.() { "ｱあ".encode("ISO-2022-JP-KDDI", undef: :replace) }
        res << t.() { "ｱあ".encode("Shift_JIS").encode(sk, undef: :replace) }
        res << t.() { "\x8e\xb1".force_encoding("EUC-JP").encode(sk) }
        res << t.() { "\x8f\xb0\xa1".force_encoding("EUC-JP").encode(sk) }
        res << t.() { "\xa4\xa2".force_encoding("EUC-JP").encode(sk) }
        res << t.() { "\x92\xa4\xa2".force_encoding("stateless-ISO-2022-JP").encode(sk) }
        res << t.() { "ｱ".encode("CP50221").encode(sk) }
        res << t.() { "\x8e\xb1".force_encoding("CP51932").encode(sk) }
        res << t.() { "ｱ".encode("Shift_JIS").encode(sk) }
        res << t.() { "a\nb".encode(sk, crlf_newline: true) }
        res << t.() { "a\r\nb".force_encoding(sk).encode("UTF-8", universal_newline: true) }
        res << t.() { "<&\"".encode(sk, xml: :attr) }
        res
        "##,
    );
}

/// The transcoder is a table: a cell no character lives in is a
/// malformed sequence, cut where a cell could no longer begin, where
/// the encoding's own walk still calls it valid.
#[test]
fn stateless_kddi_malformed_runs() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        sk = "stateless-ISO-2022-JP-KDDI"
        res = []
        ["\x92\xa9\xa1", "\x92\xa4\xf4", "\x92\x21", "\x92\xa4\x21", "\xa4\xa2", "\x92\x8f\xb0", "\x92\xfd\xa1"].each do |s|
          res << t.() { s.dup.force_encoding(sk).encode("UTF-8") }
          res << t.() { s.dup.force_encoding(sk).encode("Shift_JIS") }
          res << t.() { s.dup.force_encoding(sk).valid_encoding? }
          res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.primitive_convert(s.b, "".dup), ec.primitive_errinfo] }
        end
        res << t.() { "\x92\x21\x92\xa4\xa2".force_encoding(sk).encode("UTF-8", invalid: :replace) }
        res << t.() { "\x92\xa9\xa1\x92\xa4\xa2".force_encoding(sk).encode("UTF-8", invalid: :replace) }
        res << t.() { "\x92\xa4\xf4\x92\xa4\xa2".force_encoding(sk).encode("Shift_JIS", invalid: :replace) }
        res << t.() { "\x92\xa4\xf4\x92\xa4\xa2".force_encoding(sk).encode("UTF-8", invalid: :replace, replace: "X") }
        res << t.() { "\x92\x21\x92\xa4\xa2".force_encoding(sk).scrub("?").bytes }
        res << t.() { "\x92\xf5\xc1a".force_encoding(sk).chars.map(&:bytes) }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.primitive_convert("\x92".b, "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.primitive_convert("\x92\xa4".b, "".dup, nil, nil, partial_input: true), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.convert("\x92\xa4".b), (ec.finish rescue $!.message)] }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); ec.convert("\x92\xa4\xf4".b) }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); d = "".dup; s = "\x92\xa4\xa2\x92\xa4\xf4\x92\xa4\xa2".b; r = ec.primitive_convert(s, d); [r, d, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); d = "".dup; s = "\x92\xa4\xa2\x92\xa9\xa1\x92\xa4\xa2".b; r = ec.primitive_convert(s, d); [r, d, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("stateless-ISO-2022-JP", "UTF-8"); d = "".dup; s = "\x92\xa4\xa2\x92\x21\x92\xa4\xa2".b; r = ec.primitive_convert(s, d); [r, d, s.bytes, ec.primitive_errinfo] }
        res << t.() { "\e$B\x24\x22\x24\x74\x24\x22\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { "\e$B\x29\x21\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { "\e$B\x29\x21\e(B".force_encoding("ISO-2022-JP").encode("UTF-8") }
        res << t.() { "\e(I\x31\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { "\e$(D\x30\x21\e(B".force_encoding("ISO-2022-JP-KDDI").encode("UTF-8") }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); ec.convert("\e$B\x24\x74".b) }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); d = "".dup; s = "\e$B\x24\x22\x24\x74\x24\x22\e(B".b; r = ec.primitive_convert(s, d); [r, d, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); d = "".dup; s = "\e$B\x29\x21\e(B".b; r = ec.primitive_convert(s, d); [r, d, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", sk); d = "".dup; s = "\e$B\x24\x22\x24\x74\x24\x22\e(B".b; r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res
        "##,
    );
}

/// Streams: a destination cap is filled to the byte with the rest of
/// the character held, chunks split inside a cell are read as the cell,
/// and ISO-2022-JP-KDDI to or from its stateless form is the escape
/// rewrite alone.
#[test]
fn kddi_streams_cap_and_rewrite() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        sk = "stateless-ISO-2022-JP-KDDI"
        cap = ->(from, to, s, n) { ec = Encoding::Converter.new(from, to); d = "".dup; s = s.dup; r = ec.primitive_convert(s, d, nil, n); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res = []
        res << t.() { cap.("UTF-8", sk, "あ\u{E488}い", 4) }
        res << t.() { cap.("UTF-8", sk, "あ\u{E488}い", 3) }
        res << t.() { cap.("UTF8-KDDI", sk, "あ\u{E488}い".force_encoding("UTF8-KDDI"), 4) }
        res << t.() { cap.(sk, "UTF-8", "\x92\xa4\xa2\x92\xf5\xc1\x92\xa4\xa4".b, 4) }
        res << t.() { cap.(sk, "UTF8-KDDI", "\x92\xa4\xa2\x92\xf5\xc1\x92\xa4\xa4".b, 4) }
        res << t.() { cap.(sk, "EUC-JP", "\x92\xa4\xa2\x92\xa4\xa4".b, 3) }
        res << t.() { cap.("EUC-JP", sk, "\xa4\xa2\xa4\xa4".b, 4) }
        res << t.() { cap.("UTF-8", "ISO-2022-JP-KDDI", "あ\u{E488}い", 6) }
        res << t.() { cap.(sk, "ISO-2022-JP-KDDI", "\x92\xa4\xa2\x92\xf5\xc1".b, 6) }
        res << t.() { cap.("ISO-2022-JP-KDDI", sk, "\e$B\x24\x22\x75\x41\e(B".b, 4) }
        res << t.() { cap.("UTF8-KDDI", "UTF-8", "あ\u{E488}い".force_encoding("UTF8-KDDI"), 4) }
        res << t.() { cap.("UTF8-KDDI", "UTF-8", "あ\u{E488}い".force_encoding("UTF8-KDDI"), 3) }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); [ec.convert("\e$B\x75".b), ec.convert("\x41\e(B".b), ec.finish] }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP-KDDI", "UTF-8"); [ec.convert("\e$B\x24".b), ec.convert("\x22\e(B".b), ec.finish] }
        res << t.() { ec = Encoding::Converter.new(sk, "UTF-8"); [ec.convert("\x92\xf5".b), ec.convert("\xc1\x92".b), ec.convert("\xa4\xa2".b), ec.finish] }
        res << t.() { "\e$B\x24\x22\x75\x41\e(B".force_encoding("ISO-2022-JP-KDDI").encode(sk) }
        res << t.() { "\x92\xa4\xa2\x92\xf5\xc1".force_encoding(sk).encode("ISO-2022-JP-KDDI") }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP-KDDI"); [ec.convert("あ"), ec.convert("\u{E488}"), ec.convert("a"), ec.finish].map(&:bytes) }
        res
        "##,
    );
}

/// CP51932 is Windows-31J in EUC form: NEC row 13 and the NEC-selected
/// IBM extension rows read and write, and the seven cells JIS and
/// Windows read differently are spelled the Windows way — which
/// CP50220 and CP50221 inherit.
#[test]
fn cp51932_has_its_extension_rows() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        %w[① 髙 ￢ ～ ∥ 〜 ¬ ⅰ Ⅰ ㈱ ≒ ￤ ¢ − ‖ — ―].each do |c|
          res << t.() { c.encode("CP51932") }
          res << t.() { c.encode("EUC-JP") }
          res << t.() { c.encode("CP50220") }
        end
        %w[ad\ a1 fc\ e2 fc\ fb a1\ c1 a1\ bd a1\ dd f9\ a1 fa\ a1 fc\ fc f5\ a1 a9\ a1 a2\ af ad\ fe].each do |h|
          res << t.() { h.split.map { |x| x.hex }.pack("C*").force_encoding("CP51932").encode("UTF-8") }
          res << t.() { h.split.map { |x| x.hex }.pack("C*").force_encoding("CP51932").valid_encoding? }
        end
        res << t.() { (0xa1..0xfe).select { |r| (0xa1..0xfe).any? { |c| [r, c].pack("C*").force_encoding("CP51932").encode("UTF-8") rescue false } } }
        res << t.() { [0xad, 0xf9, 0xfa, 0xfb, 0xfc].map { |r| (0xa1..0xfe).count { |c| [r, c].pack("C*").force_encoding("CP51932").encode("UTF-8") rescue false } } }
        res << t.() { "\e$B\x2d\x21\x7c\x62\e(B".force_encoding("CP50221").encode("UTF-8") }
        res << t.() { "\xad\xa1\xfc\xe2".force_encoding("CP51932").encode("Shift_JIS") }
        res << t.() { "\xad\xa1\xfc\xe2".force_encoding("CP51932").encode("Windows-31J") }
        res << t.() { "\x87\x40\xfb\xfc".force_encoding("Windows-31J").encode("CP51932") }
        res << t.() { "\xad\xa1".force_encoding("CP51932").encode("EUC-JP") }
        res
        "##,
    );
}

/// The xml decorator escapes characters, so a source that is not UTF-8
/// is read by its own conversion first; and a carrier's stream reports
/// the Unicode meaning of what the far end refused, or the emoji itself
/// when it has none.
#[test]
fn xml_reads_the_source_and_carrier_streams_name_the_unicode() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        sk = "stateless-ISO-2022-JP-KDDI"
        res = []
        res << t.() { "<\xa4\xa2".force_encoding("EUC-JP").encode("UTF-8", xml: :text) }
        res << t.() { "<\xa4\xa2&".force_encoding("EUC-JP").encode("Shift_JIS", xml: :attr) }
        res << t.() { "<\x92\xa4\xa2".force_encoding(sk).encode("ISO-2022-JP", xml: :text) }
        res << t.() { "<\x82\xa0".force_encoding("Windows-31J").encode("US-ASCII", xml: :text) }
        res << t.() { "a\xff".force_encoding("EUC-JP").encode("UTF-8", xml: :text) }
        res << t.() { "a\xff".force_encoding("EUC-JP").encode("UTF-8", xml: :text, invalid: :replace) }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "EUC-JP"); [ec.primitive_convert("\u{E488}".force_encoding("UTF8-KDDI"), "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "EUC-JP"); ec.convert("\u{E488}".force_encoding("UTF8-KDDI")) }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "UTF-8"); [ec.primitive_convert("\u{E5CD}".force_encoding("UTF8-KDDI"), "".dup), ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "UTF-8"); ec.convert("\u{E5CD}".force_encoding("UTF8-KDDI")) }
        res << t.() { "\u{E488}".force_encoding("UTF8-KDDI").encode("EUC-JP") }
        res << t.() { "\xf6\x60".force_encoding("SJIS-KDDI").encode("EUC-JP") }
        res << t.() { "\u{E63E}".force_encoding("UTF8-DoCoMo").encode("Shift_JIS") }
        res << t.() { ec = Encoding::Converter.new("SJIS-KDDI", "EUC-JP"); [ec.primitive_convert("\xf6\x60".b, "".dup), ec.primitive_errinfo] }
        res
        "##,
    );
}
