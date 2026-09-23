extern crate monoruby;
use monoruby::tests::*;

// The converters #1520 found missing: three DOS code pages CRuby has
// single-byte tables for, Big5-UAO and GB12345 (their cell tables ride
// with #1500's), and CP50220 / CP50221 — Windows' readings of
// ISO-2022-JP, which CRuby writes from CP51932 and which read Shift
// Out / Shift In, `ESC ( I` and the 8-bit bytes as JIS X 0201 kana.

/// CP850, IBM737 and IBM775: every high byte, in and out.
#[test]
fn the_three_dos_code_pages_convert_through_their_tables() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        %w[CP850 IBM850 IBM737 IBM775].each do |e|
          res << t.() { "ab".encode(e) }
          res << t.() { "あ".encode(e) }
          res << t.() { "é".encode(e) }
          res << t.() { "Β".encode(e) }
          res << t.() { "あ".encode(e, undef: :replace) }
          res << t.() { (0x80..0xff).map { |b| b.chr }.join.force_encoding(e).encode("UTF-8") }
          res << t.() { (0x80..0xff).map { |b| b.chr }.join.force_encoding(e).encode("UTF-8").encode(e) }
          res << t.() { Encoding::Converter.new("UTF-8", e).convpath.map { |x| x.map(&:to_s) } }
          res << t.() { "é".encode("Shift_JIS").encode(e) }
        end
        res
        "##,
    );
}

/// Big5-UAO and GB12345 read and write through their tables.
#[test]
fn big5_uao_and_gb12345_have_converters() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        %w[Big5-UAO GB12345].each do |e|
          res << t.() { "ab".encode(e) }
          res << t.() { "あ".encode(e) }
          res << t.() { "é".encode(e) }
          res << t.() { "一龘國圖".encode(e) }
          res << t.() { "あé".encode(e).encode("UTF-8") }
          res << t.() { "\x81".force_encoding(e).encode("UTF-8") }
          res << t.() { "\x81\x81".force_encoding(e).encode("UTF-8") }
          res << t.() { "\xa1\xa1".force_encoding(e).encode("UTF-8") }
          res << t.() { "\xfe\xfe".force_encoding(e).encode("UTF-8") }
          res << t.() { Encoding::Converter.new("UTF-8", e).convpath.map { |x| x.map(&:to_s) } }
          res << t.() { Encoding::Converter.new(e, "UTF-8").convert("\xa4\xa2".b).bytes }
          res << t.() { "\u{1F600}".encode(e) }
        end
        res << t.() { "\x81\x40\xa1\x40\xc8\x40\xf9\xd5".force_encoding("Big5-UAO").encode("UTF-8") }
        res << t.() { "\xb0\xa1\xd7\xfe\xa1\xa1".force_encoding("GB12345").encode("UTF-8") }
        res
        "##,
    );
}

/// CP50220 and CP50221: the way out, kana folded or under `ESC ( I`.
#[test]
fn cp5022x_write_kana_as_windows_does() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        %w[CP50220 CP50221].each do |e|
          res << t.() { "あ".encode(e) }
          res << t.() { "ｱｲ".encode(e) }
          res << t.() { "ｶﾞ".encode(e) }
          res << t.() { "ﾊﾟ".encode(e) }
          res << t.() { "ｳﾞ".encode(e) }
          res << t.() { "ﾞﾟﾞ".encode(e) }
          res << t.() { "aｱあb".encode(e) }
          res << t.() { "ｱaｱ".encode(e) }
          res << t.() { "¥".encode(e) }
          res << t.() { "‾".encode(e) }
          res << t.() { "丂".encode(e) }
          res << t.() { "\u{E63E}".encode(e) }
          res << t.() { "\x82\xa0".force_encoding("Shift_JIS").encode(e) }
          res << t.() { "\xa4\xa2".force_encoding("EUC-JP").encode(e) }
          res << t.() { "\xa4\xa2".force_encoding("CP51932").encode(e) }
          res << t.() { "あ".encode("UTF-16LE").encode(e) }
          res << t.() { "丂".encode("Shift_JIS").encode(e) rescue "sjis" }
          res << t.() { Encoding::Converter.new("UTF-8", e).convpath.map { |x| x.map(&:to_s) } }
          res << t.() { Encoding::Converter.new("Shift_JIS", e).convpath.map { |x| x.map(&:to_s) } }
          res << t.() { Encoding::Converter.new("EUC-JP", e).convpath.map { |x| x.map(&:to_s) } }
          res << t.() { Encoding::Converter.new(e, "UTF-8").convpath.map { |x| x.map(&:to_s) } }
          res << t.() { Encoding::Converter.new(e, "Shift_JIS").convpath.map { |x| x.map(&:to_s) } }
          # The full fold: every half-width kana, alone and with each mark.
          res << t.() { (0xFF61..0xFF9F).map { |c| [c].pack("U") }.join.encode(e) }
          res << t.() { (0xFF61..0xFF9D).map { |c| [c].pack("U") + "ﾞ" }.join.encode(e) }
          res << t.() { (0xFF61..0xFF9D).map { |c| [c].pack("U") + "ﾟ" }.join.encode(e) }
          ec = Encoding::Converter.new("UTF-8", e)
          res << t.() { [ec.convert("あ"), ec.convert("ｱ"), ec.convert("a"), ec.finish].map(&:bytes) }
          ec = Encoding::Converter.new("UTF-8", e)
          res << t.() { [ec.primitive_convert("あ¥".dup, +""), ec.primitive_errinfo, ec.last_error.message] }
        end
        res
        "##,
    );
}

/// CP50220 and CP50221: the way in, three spellings of kana included,
/// and what the parser refuses.
#[test]
fn cp5022x_read_kana_three_ways() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        %w[CP50220 CP50221].each do |e|
          [
            "\e$B\x24\x22\e(B", "\e$@\x24\x22\e(B", "\e(J\x5c\x7e\e(B",
            "a\x0e\x31\x32\x0fb", "a\e(I\x31\e(Bb", "\xb1", "\e(I\xb1\e(B",
            "\e$B\x24\x22\x0e\x31\x0f\x24\x22\e(B", "\e$B\x24\x22\e(I\x31\e$B\x24\x22\e(B",
            "\e(I\x31\x24\x22", "\x0e\x31", "\e(I", "\e$B\x24\x22",
            "\e(I\x80\e(B", "\e(I\x20", "\e(I\x60", "\x0e\x60\x0f", "\xe0",
            "\e$B\x24\x22\xb1\x24\x22\e(B", "\e$B\x24", "\e$(D\x30\x21\e(B", "\e$A\x30\x21\e(B",
            "\e$B\x24\x22\x0a\x24\x22\e(B", "\e(", "\e",
          ].each do |s|
            res << [s.bytes, s.force_encoding(e).valid_encoding?, t.() { s.force_encoding(e).encode("UTF-8") },
                    t.() { s.force_encoding(e).encode("UTF-8", invalid: :replace) },
                    t.() { s.force_encoding(e).encode("Shift_JIS") }]
          end
          ec = Encoding::Converter.new(e, "UTF-8")
          res << t.() { [ec.convert("\e$B\x24"), ec.convert("\x22"), ec.convert("\e(B"), ec.finish] }
          ec = Encoding::Converter.new(e, "UTF-8")
          res << t.() { [ec.convert("\e(I\x31"), ec.convert("\x32\x0e\x33"), ec.finish] }
          ec = Encoding::Converter.new(e, "UTF-8")
          res << t.() { [ec.primitive_convert("\e$B\x24\x22\e(I\x80".dup, +""), ec.primitive_errinfo] }
        end
        res
        "##,
    );
}

/// ISO-2022-JP proper: Shift Out / Shift In are not its bytes, a cell
/// split across two `#convert` calls is still a cell, and the message
/// for a character it cannot take names the whole chain.
#[test]
fn iso_2022_jp_keeps_its_designation_across_calls() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { "a\x0e\x31\x0fb".force_encoding("ISO-2022-JP").encode("UTF-8") }
        res << t.() { "\x0f".force_encoding("ISO-2022-JP").encode("UTF-8", invalid: :replace) }
        ec = Encoding::Converter.new("ISO-2022-JP", "UTF-8")
        res << t.() { [ec.convert("\e$B\x24"), ec.convert("\x22"), ec.convert("\e(B"), ec.finish] }
        ec = Encoding::Converter.new("ISO-2022-JP", "UTF-8")
        res << t.() { [ec.convert("\e$B\x24\x22\x24"), ec.convert("\x22"), ec.finish] }
        res << t.() { "ｱ".encode("ISO-2022-JP") }
        res << t.() { "髙".encode("ISO-2022-JP") }
        res << t.() { "丂".encode("ISO-2022-JP") }
        res << t.() { "髙".encode("Shift_JIS").encode("ISO-2022-JP") rescue "sjis" }
        res << t.() { "\e$B\x2d\x21\e(B".force_encoding("ISO-2022-JP").encode("UTF-8") }
        res << t.() { "\e$B\x2d\x21\e(B".force_encoding("ISO-2022-JP").encode("IBM437") }
        ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP")
        res << t.() { [ec.primitive_convert("あｱ".dup, +""), ec.primitive_errinfo, ec.last_error.message] }
        ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP")
        res << t.() { [ec.primitive_convert("あ髙".dup, +""), ec.primitive_errinfo, ec.last_error.message] }
        ec = Encoding::Converter.new("ISO-2022-JP", "UTF-8")
        res << t.() { [ec.primitive_convert("\e$B\x2d\x21".dup, +""), ec.primitive_errinfo, ec.last_error.message] }
        res
        "##,
    );
}
