extern crate monoruby;
use monoruby::tests::*;

// `String#encode` out of the Big5 family (#1500): CRuby's transcoders
// cut their input wider than the encoding's own walk — every byte from
// 0x81 to 0xFE leads a cell — so a well-formed cell the table has no
// character for is an *undefined* conversion (not covered by
// `invalid: :replace`, covered by `undef: :replace`), where the same
// bytes are `valid_encoding? == false`; and Big5-HKSCS reads its rows
// below 0xA1 that Big5 leaves empty.

/// Every byte pattern of the sweep the issue was measured with, for
/// each member of the family: validity, the plain conversion, and the
/// two replacement modes.
#[test]
fn big5_cells_without_a_character_are_undefined_not_invalid() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        pats = [[0x80],[0x81],[0x8e],[0x8f],[0xa0],[0xa1],[0xfd],[0xfe],[0xff],
                [0x81,0x20],[0x81,0x30],[0x81,0x40],[0x81,0x7f],[0x81,0x80],[0x81,0xa1],[0x81,0xff],
                [0x8e,0xa1],[0x8f,0xa1],[0x8f,0xa1,0xa1],[0xa1,0xa1],[0xa1,0x40],[0xfe,0xfe],[0xff,0xff],
                [0xf9,0xd5],[0xf9,0xd6],[0xc8,0x40],[0xa1,0x7f],[0xa3,0xe1],[0xc6,0xa1],[0xc7,0xfe],[0x87,0x40],[0x61,0x81]]
        res = []
        %w[Big5 Big5-HKSCS CP950 CP951].each do |enc|
          pats.each do |bs|
            s = bs.pack('C*').force_encoding(enc)
            res << [enc, bs, s.valid_encoding?, t.() { s.encode("UTF-8") },
                    t.() { s.encode("UTF-8", invalid: :replace) },
                    t.() { s.encode("UTF-8", undef: :replace) },
                    t.() { s.encode("UTF-8", invalid: :replace, undef: :replace) }]
          end
        end
        res
        "##,
    );
}

/// The way in: what the family writes for characters that live in
/// HKSCS's extra rows, in Big5's own cells, and nowhere.
#[test]
fn big5_family_writes_only_the_cells_its_table_has() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; [v.bytes, v.encoding.to_s]; rescue => e; [e.class, e.message]; end }
        res = []
        chars = ["繧", "\u{28625}", "€", "屋", "龘", "é", "あ", "①", "", " "]
        %w[Big5 Big5-HKSCS CP950 CP951].each do |enc|
          chars.each do |c|
            res << [enc, c.ord, t.() { c.encode(enc) }, t.() { c.encode(enc, undef: :replace) }]
          end
        end
        res
        "##,
    );
}

/// `Encoding::Converter` reads the same cells the same way, and
/// reports the offending bytes as CRuby does.
#[test]
fn converter_reads_big5_cells_like_encode() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        [["Big5", "\x81\x40"], ["Big5", "\x81\x20"], ["Big5", "\x8e\xa1"], ["Big5-HKSCS", "\x8e\xa1"], ["Big5-HKSCS", "\x81\x40"], ["Big5-HKSCS", "\x8f\xa1\xa1"], ["CP950", "\x81\x40"]].each do |enc, bytes|
          ec = Encoding::Converter.new(enc, "UTF-8")
          res << [enc, bytes.bytes, t.() { ec.convert(bytes.b) }, ec.primitive_errinfo, t.() { ec.finish }]
          ec = Encoding::Converter.new(enc, "UTF-8")
          d = +""
          res << [ec.primitive_convert(bytes.b, d), d.bytes, ec.primitive_errinfo]
        end
        res
        "##,
    );
}
