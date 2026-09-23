extern crate monoruby;
use monoruby::tests::*;

// `partial_input: true` on `Encoding::Converter#primitive_convert`
// (#1589): a chunk the call brought itself still answers
// `:source_buffer_empty` once it has been read, since more is coming;
// `:finished` is the end of the stream, after which nothing more is
// converted; and `Converter.new` on one encoding names the decorator
// it was refused with.

/// `[:source_buffer_empty, [97]]` for `"a"` with `partial_input:`,
/// then `:finished` for the chunk that ends the stream, and
/// `:finished` again — with nothing written — for anything after it.
#[test]
fn partial_input_never_finishes() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        res = []
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r = ec.primitive_convert("a".dup, d, nil, nil, partial_input: true); [r, d.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r1 = ec.primitive_convert("a".dup, d, nil, nil, partial_input: true); r2 = ec.primitive_convert("b".dup, d); r3 = ec.primitive_convert("c".dup, d); [r1, r2, r3, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r = ec.primitive_convert(nil, d, nil, nil, partial_input: true); [r, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r = ec.primitive_convert("".dup, d, nil, nil, partial_input: true); [r, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r1 = ec.primitive_convert("a".dup, d); r2 = ec.primitive_convert("b".dup, d, nil, nil, partial_input: true); [r1, r2, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "UTF-16LE"); d = "".b; r = ec.primitive_convert("a".dup, d, nil, nil, partial_input: true); [r, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP"); d = "".b; r = ec.primitive_convert("あ".dup, d, nil, nil, partial_input: true); r2 = ec.primitive_convert("".dup, d); [r, r2, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; r = ec.primitive_convert("ab".dup, d, nil, 1, partial_input: true); [r, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "US-ASCII"); d = "".b; r = ec.primitive_convert("aé".dup, d, nil, nil, partial_input: true); [r, d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-8", "EUC-JP"); d = "".b; rs = []; rs << ec.primitive_convert("a".dup, d, nil, nil, partial_input: true); rs << ec.primitive_convert("b".dup, d, nil, nil, partial_input: true); rs << ec.primitive_convert(nil, d); rs << ec.primitive_convert("c".dup, d); rs << ec.primitive_convert(nil, d); [rs, d.bytes] }
        res
        "##,
    );
}

/// `code converter not found (UTF-8 to UTF-8 with universal_newline)`:
/// the decorator is named, as it is for a pair no converter serves.
#[test]
fn same_encoding_names_the_decorator() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        res = []
        res << t.() { Encoding::Converter.new("UTF-8", "UTF-8", universal_newline: true) }
        res << t.() { Encoding::Converter.new("UTF-8", "UTF-8", crlf_newline: true) }
        res << t.() { Encoding::Converter.new("UTF-8", "UTF-8") }
        res << t.() { Encoding::Converter.new("EUC-JP", "EUC-JP", Encoding::Converter::UNIVERSAL_NEWLINE_DECORATOR) }
        res << t.() { Encoding::Converter.new("UTF-8", "UTF8-MAC").convpath.map { |a, b| [a.to_s, b.to_s] } }
        res
        "##,
    );
}
