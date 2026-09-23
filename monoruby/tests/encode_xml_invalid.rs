extern crate monoruby;
use monoruby::tests::*;

// `String#encode(xml:)` is an output decorator on the ordinary
// conversion in CRuby, so `invalid:` acts underneath it: a bad byte
// becomes the destination's replacement, or the error the conversion
// owes, and never a `&#xFFFD;` reference. Between one encoding and
// itself no transcoder runs and the byte passes through (#1615).

#[test]
fn the_xml_decorator_leaves_invalid_bytes_to_the_conversion() {
    run_test_once(
        r##"
        # The bytes and the tag. CRuby marks the pass-through result
        # valid whatever its bytes (the converter's coderange rule),
        # where monoruby classifies them, so `valid_encoding?` is not
        # compared — as in tests/gsub_wide_replacement.rs.
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        s = "a\xff<b>".dup.force_encoding("UTF-8")
        %w[UTF-8 EUC-JP UTF-16LE US-ASCII Windows-31J ISO-8859-1].each do |d|
          res << t.() { s.encode(d, invalid: :replace, xml: :text) }
          res << t.() { s.encode(d, invalid: :replace, xml: :attr) }
          res << t.() { s.encode(d, xml: :text) }
          res << t.() { s.encode(d, invalid: :replace, replace: "X", xml: :text) }
          res << t.() { "a\xffé<b>".dup.force_encoding("UTF-8").encode(d, invalid: :replace, undef: :replace, xml: :text) }
          res << t.() { "aé<b>".encode(d, undef: :replace, xml: :text) }
          res << t.() { "aé<b>".encode(d, xml: :text) }
          res << t.() { "a\"&\u{1F600}".encode(d, xml: :attr) }
        end
        e = "a\xff<\xa4\xa2>".dup.force_encoding("EUC-JP")
        %w[EUC-JP UTF-8 Shift_JIS].each do |d|
          res << t.() { e.encode(d, invalid: :replace, xml: :text) }
          res << t.() { e.encode(d, xml: :text) }
        end
        res << t.() { "a<\xe3\x81".dup.force_encoding("UTF-8").encode("EUC-JP", xml: :text) }
        res << t.() { "a<\xe3\x81".dup.force_encoding("UTF-8").encode("EUC-JP", invalid: :replace, xml: :text) }
        res << t.() { "a<\xe3\x81".dup.force_encoding("UTF-8").encode("UTF-8", invalid: :replace, xml: :text) }
        res << t.() { "a\xff<".b.encode("UTF-8", invalid: :replace, xml: :text) }
        res << t.() { "a\xff<".b.encode("UTF-8", xml: :text) }
        res << t.() { "a\xff<".b.encode("UTF-8", undef: :replace, xml: :text) }
        res << t.() { "a<".encode("UTF-16LE").encode("UTF-8", xml: :text) }
        res << t.() { "a<".encode("UTF-16LE").b[0..2].force_encoding("UTF-16LE").encode("UTF-8", invalid: :replace, xml: :text) }
        res << t.() { "a<".encode("UTF-16LE").b[0..2].force_encoding("UTF-16LE").encode("UTF-8", xml: :text) }
        res << t.() { "<\xa4\xa2".force_encoding("EUC-JP").encode("UTF-8", xml: :text) }
        res << t.() { "a\x80<".dup.force_encoding("UTF-8").encode("UTF-8", xml: :attr) }
        res
        "##,
    );
}
