extern crate monoruby;
use monoruby::tests::*;

// Three edges of `Encoding::Converter` measured in #1584, #1591 and
// #1592: the hop an error names for the code pages CRuby's transcoder
// spells in upper case and for IBM037's two real hops, Windows-1258
// having no converter at all, and a US-ASCII destination holding a
// chunk that ends inside a character.

/// `U+4E00 to WINDOWS-1252 in conversion from UTF-8 to WINDOWS-1252`,
/// `U+4E00 to ISO-8859-1 in conversion from UTF-8 to ISO-8859-1 to
/// IBM037`, and the plain form everywhere else — in the message and in
/// `primitive_errinfo`'s destination slot alike (#1584).
#[test]
fn the_hop_an_undefined_conversion_names() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        %w[Windows-1250 Windows-1251 Windows-1252 Windows-1253 Windows-1254 Windows-1255 Windows-1256 Windows-1257
           CP1250 CP1257 Windows-874 CP874 macCroatian macCyrillic macGreek macIceland macRoman macRomania macTurkish macUkraine
           IBM037 ebcdic-cp-us IBM437 CP852 ISO-8859-1 TIS-620 US-ASCII KOI8-R IBM866 ISO-2022-JP stateless-ISO-2022-JP EUC-JP].each do |d|
          res << t.() { "\u{4E00}".encode(d) }
          res << t.() { "\u{4E00}".encode("EUC-JP").encode(d) }
          res << t.() { ec = Encoding::Converter.new("UTF-8", d); ec.primitive_convert("a\x80z".dup.force_encoding("UTF-8"), +""); ec.primitive_errinfo }
          res << t.() { ec = Encoding::Converter.new("UTF-8", d); ec.primitive_convert("a\u{4E00}z".dup, +""); ec.primitive_errinfo }
        end
        res << t.() { "a\x80z".force_encoding("UTF-8").encode("IBM037") }
        res << t.() { "a\x80z".force_encoding("UTF-8").encode("IBM037", invalid: :replace) }
        res << t.() { "aあz".encode("IBM037", undef: :replace, replace: "é") }
        res << t.() { "\x00a\xd8\x00".force_encoding("UTF-16BE").encode("IBM037") }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "IBM037"); d = +""; s = "a\x80z".b.force_encoding("UTF-8"); r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "IBM037"); [ec.convert("a\xC3").bytes, ec.convert("\xA9b").bytes, ec.finish.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "IBM037"); d = +""; s = "abcé".dup; r = ec.primitive_convert(s, d, nil, 2); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { "a\r\nb".force_encoding("IBM037").encode("UTF-8", universal_newline: true) }
        res << t.() { "<é".encode("IBM037", xml: :text) }
        res << t.() { "<あ".encode("IBM037", xml: :text) }
        res << t.() { "\xff".force_encoding("UTF8-KDDI").encode("stateless-ISO-2022-JP-KDDI") }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "stateless-ISO-2022-JP-KDDI"); d = +""; s = "a\xffb".b.force_encoding("UTF8-KDDI"); r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF8-KDDI", "stateless-ISO-2022-JP-KDDI"); [ec.convert("a\xE3".b.force_encoding("UTF8-KDDI")).bytes, ec.convert("\x81\x82".b.force_encoding("UTF8-KDDI")).bytes, ec.finish.bytes] }
        res
        "##,
    );
}

/// Windows-1258 (CP1258) is an encoding with no transcoder: every
/// conversion but 7-bit text is `ConverterNotFoundError`, and the
/// string's own operations are untouched (#1591).
#[test]
fn windows_1258_has_no_converter() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        %w[Windows-1258 CP1258].each do |e|
          res << t.() { "é".encode(e) }
          res << t.() { "abc".encode(e) }
          res << t.() { Encoding::Converter.new("UTF-8", e).to_s }
          res << t.() { Encoding::Converter.new(e, "UTF-8").to_s }
          res << t.() { Encoding::Converter.search_convpath("UTF-8", e) }
          res << t.() { "\xe9".force_encoding(e).encode("UTF-8") }
          res << t.() { "abc".force_encoding(e).encode("UTF-8") }
          res << t.() { "\xe9".b.encode(e) }
          res << t.() { "a\nb".encode(e, crlf_newline: true) }
          res << t.() { "ab".encode("UTF-16LE").encode(e) }
          res << t.() { "ab".force_encoding(e).encode("UTF-16LE") }
          res << t.() { "a<".encode(e, xml: :text) }
          res << t.() { "\xe9".force_encoding(e).valid_encoding? }
          res << t.() { "\xe9".force_encoding(e).encode(e) }
          res << t.() { "\xe9".force_encoding(e).scrub }
          res << t.() { "\xe9".force_encoding(e).encode(e, invalid: :replace) }
          res << t.() { Encoding.find(e).name }
        end
        %w[Windows-1250 Windows-1257 Windows-874].each { |e| res << t.() { "é".encode(e) } }
        res
        "##,
    );
}
