extern crate monoruby;
use monoruby::tests::*;

// `code converter not found (...)` as CRuby's `rb_econv_open_exc`
// spells it (#1614): both names of the pair, in the positions they were
// given in, and the decorators the options asked for — which means
// reading those options the way `econv_opts` does, before either name.

/// The pair, whichever side failed to resolve, with the receiver's own
/// encoding standing in for an omitted source.
#[test]
fn the_message_names_the_pair() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        [
          t.() { "abc".encode("bogus") },
          t.() { "abc".encode("UTF-8", "bogus") },
          t.() { Encoding::Converter.new("bogus", "UTF-8") },
          t.() { Encoding::Converter.new("UTF-8", "bogus") },
          t.() { "abc".encode("internal") },
          t.() { "abc".encode("bogus1", "bogus2") },
          t.() { Encoding::Converter.new("bogus1", "bogus2") },
          t.() { "abc".encode("") },
          t.() { "abc".encode("UTF-8", "") },
          t.() { "abc".force_encoding("EUC-JP").encode("bogus") },
          t.() { "abc".encode("bogus", Encoding::EUC_JP) },
          t.() { "abc".encode!("bogus") },
          t.() { "abc".encode("utf-7") },
          t.() { Encoding::Converter.new("Bogus", "euc-jp") },
          t.() { "x".encode("ISO-2022-JP", "bogus") },
          t.() { Encoding::Converter.search_convpath("bogus", "EUC-JP") },
        ]
        "##,
    );
}

/// The decorators follow the pair, in CRuby's fixed order, however
/// they were asked for: a `*_newline:` flag, `newline:`, `xml:`, or a
/// converter's Integer mask.
#[test]
fn the_message_names_the_decorators() {
    run_test_once(
        r##"
        C = Encoding::Converter
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        [
          t.() { "abc".encode("bogus", universal_newline: true) },
          t.() { "abc".encode("bogus", crlf_newline: true) },
          t.() { "abc".encode("bogus", xml: :text) },
          t.() { "abc".encode("bogus", xml: :attr, cr_newline: true) },
          t.() { "abc".encode("UTF-8", "bogus", xml: :text) },
          t.() { "abc".encode("bogus", newline: :crlf) },
          t.() { "abc".encode("bogus", newline: :universal) },
          t.() { "abc".encode("bogus", newline: :cr) },
          t.() { "abc".encode("bogus", newline: :lf) },
          t.() { "abc".encode("bogus", lf_newline: true) },
          t.() { "abc".encode("bogus", crlf_newline: true, newline: :cr) },
          t.() { "abc".encode("bogus", universal_newline: true, xml: :text, crlf_newline: true) },
          t.() { "abc".encode("bogus", invalid: :replace, replace: "x", crlf_newline: true) },
          t.() { "abc".encode("bogus", "bogus2", universal_newline: true) },
          t.() { "abc".encode!("bogus", xml: :text) },
          t.() { "abc".encode("UTF-7", crlf_newline: true) },
          t.() { "abc".encode("UTF-7", xml: :text) },
          t.() { C.new("bogus", "UTF-8", C::CRLF_NEWLINE_DECORATOR) },
          t.() { C.new("bogus", "UTF-8", C::LF_NEWLINE_DECORATOR) },
          t.() { C.new("bogus", "UTF-8", C::XML_ATTR_CONTENT_DECORATOR | C::XML_ATTR_QUOTE_DECORATOR | C::CR_NEWLINE_DECORATOR) },
          t.() { C.new("bogus", "UTF-8", newline: :universal) },
          t.() { C.new("bogus", "UTF-8", xml: :text) },
          t.() { C.new("bogus", "UTF-8", universal_newline: true, crlf_newline: true) },
          t.() { C.new("UTF-8", "bogus", xml: :attr) },
          t.() { C.new("UTF-8", "UTF-8", xml: :text) },
          t.() { C.new("UTF-8", "UTF-8", newline: :crlf) },
          t.() { C.new("UTF-8", "UTF-7", universal_newline: true) },
          t.() { C.new("UTF-8", "UTF-7", newline: :crlf) },
          t.() { C.search_convpath("bogus", "EUC-JP", universal_newline: true) },
          t.() { C.search_convpath("UTF-8", "bogus", newline: :cr) },
        ]
        "##,
    );
}

/// Decorators that cannot be stacked have no converter. `String#encode`
/// between one encoding and itself opens a decorator-only converter,
/// whose refusal names no encodings.
#[test]
fn decorators_that_cannot_be_stacked() {
    run_test_once(
        r##"
        C = Encoding::Converter
        s = "a\nb\r\nc\rd"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        [
          t.() { s.encode("EUC-JP", universal_newline: true, crlf_newline: true) },
          t.() { s.encode("UTF-8", "UTF-8", crlf_newline: true, cr_newline: true) },
          t.() { s.encode(crlf_newline: true, lf_newline: true) },
          t.() { s.encode("UTF-8", universal_newline: true, crlf_newline: true) },
          t.() { C.new("UTF-8", "EUC-JP", universal_newline: true, crlf_newline: true) },
          t.() { C.new("UTF-8", "EUC-JP", C::UNIVERSAL_NEWLINE_DECORATOR | C::CRLF_NEWLINE_DECORATOR) },
          t.() { C.new("UTF-8", "EUC-JP", C::XML_TEXT_DECORATOR | C::XML_ATTR_CONTENT_DECORATOR) },
          # `newline:` overrides the flags, so this is one decorator.
          t.() { s.encode("EUC-JP", newline: :crlf, universal_newline: true) },
        ]
        "##,
    );
}

/// The options are read before either encoding, and a value they do
/// not know is an ArgumentError — naming a Symbol, and nothing else.
#[test]
fn an_unknown_option_value_comes_first() {
    run_test_once(
        r##"
        C = Encoding::Converter
        s = "abc"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        [
          t.() { s.encode("bogus", xml: :foo) },
          t.() { s.encode("bogus", newline: :foo) },
          t.() { s.encode("UTF-8", newline: "crlf") },
          t.() { s.encode("UTF-8", newline: 1) },
          t.() { s.encode("UTF-8", xml: "text") },
          t.() { s.encode("UTF-8", xml: 1) },
          t.() { s.encode("UTF-8", invalid: :foo) },
          t.() { s.encode("UTF-8", undef: :foo) },
          t.() { s.encode("UTF-8", invalid: "replace") },
          t.() { s.encode("UTF-8", xml: nil, newline: nil, invalid: nil) },
          t.() { C.new("bogus", "UTF-8", xml: :foo) },
          t.() { C.new("UTF-8", "EUC-JP", newline: :foo) },
          t.() { C.new("bogus", "EUC-JP", invalid: :foo) },
          t.() { C.search_convpath("bogus", "EUC-JP", newline: :foo) },
        ]
        "##,
    );
}

/// `newline:` and `lf_newline:` convert, as the `*_newline:` flags
/// already did.
#[test]
fn newline_option_and_lf_newline() {
    run_test_once(
        r##"
        s = "a\nb\r\nc\rd"
        [
          s.encode("UTF-8", lf_newline: true),
          s.encode("UTF-8", newline: :lf),
          s.encode("UTF-8", newline: :crlf),
          s.encode("UTF-8", newline: :cr),
          s.encode("UTF-8", newline: :universal),
          s.encode("UTF-16LE", newline: :crlf).encode("UTF-8"),
          s.encode("UTF-16LE", "UTF-8", newline: :lf).encode("UTF-8"),
          s.encode("UTF-16LE", "UTF-8", lf_newline: true).encode("UTF-8"),
          s.encode("UTF-8", crlf_newline: true, newline: :cr),
          s.encode("UTF-8", newline: :cr, crlf_newline: true),
          s.encode("UTF-8", universal_newline: false, crlf_newline: nil),
          s.encode(newline: :crlf),
          s.encode("EUC-JP", "UTF-8", newline: :crlf),
          s.dup.encode!("UTF-8", newline: :crlf),
          Encoding::Converter::LF_NEWLINE_DECORATOR,
          Encoding::Converter.search_convpath("UTF-8", "EUC-JP", newline: :crlf)
            .map { |hop| hop.is_a?(Array) ? hop.map(&:to_s) : hop },
        ]
        "##,
    );
}

/// A `#to_str` argument that names no encoding is converted twice —
/// once to look the name up, once more to report it — and one that
/// resolves only once, as in CRuby's `enc_arg`.
#[test]
fn an_unresolved_to_str_argument_is_read_twice() {
    run_test_once(
        r##"
        class S
          def initialize(s) = (@s = s; @n = 0)
          def to_str = (@n += 1; @s)
          attr_reader :n
        end
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        s = S.new("bogus"); d = S.new("utf-8")
        res << t.() { "abc".encode(s, d) } << s.n << d.n
        s = S.new("bogus"); d = S.new("utf-8")
        res << t.() { Encoding::Converter.new(d, s) } << s.n << d.n
        s = S.new("euc-jp"); d = S.new("utf-8")
        res << "abc".encode(s, d).encoding.to_s << s.n << d.n
        res << t.() { "abc".encode(Object.new) }
        res << t.() { "abc".encode("bogus\0") }
        res
        "##,
    );
}
