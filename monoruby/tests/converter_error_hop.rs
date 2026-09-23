extern crate monoruby;
use monoruby::tests::*;

// The hop an error names (#1618): `primitive_errinfo[1, 2]`, the
// exception's `#source_encoding_name` / `#destination_encoding_name`
// and its message all name the transcoder that refused the character
// — `["UTF-8", "EUC-JP"]` for `U+20AC` on the way to ISO-2022-JP,
// `["EUC-JP", "stateless-ISO-2022-JP"]` for `U+00E9`, which EUC-JP
// holds in JIS X 0212 and stateless does not. And `"\x80"` is not an
// EUC-JP byte: an EUC-JP source into the stateless family is read as
// EUC-JP, so it is `:invalid_byte_sequence`, not a missing cell.

/// `[errinfo, [class, message, [source name, destination name]]]` for
/// each pair, through the converter and through `String#encode`.
#[test]
fn errinfo_and_the_exception_name_the_hop_that_refused() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        info = ->(from, to, s) { ec = Encoding::Converter.new(from, to); r = ec.primitive_convert(s.dup.force_encoding(from), +""); [r, ec.primitive_errinfo] }
        msg = ->(from, to, s) { begin; s.dup.force_encoding(from).encode(to); "ok"; rescue => e; [e.class.to_s, e.message, (e.respond_to?(:source_encoding_name) ? [e.source_encoding_name, e.destination_encoding_name] : nil)]; end }
        res = []
        [["UTF-8", "ISO-2022-JP", "€"], ["UTF-8", "ISO-2022-JP", "é"], ["UTF-8", "stateless-ISO-2022-JP", "€"], ["UTF-8", "stateless-ISO-2022-JP", "é"],
         ["Shift_JIS", "ISO-2022-JP", "\xB1".b], ["CP949", "ISO-2022-JP", "\xB0\xA1".b], ["CP949", "stateless-ISO-2022-JP", "\xB0\xA1".b],
         ["EUC-JP", "ISO-2022-JP", "A\x80B".b], ["EUC-JP", "stateless-ISO-2022-JP", "A\x80B".b], ["EUC-JP", "ISO-2022-JP", "\x8e\xb1".b], ["EUC-JP", "ISO-2022-JP", "\x8f\xb0\xa1".b],
         ["UTF-8", "CP50220", "€"], ["UTF-8", "CP50220", "é"], ["UTF-8", "ISO-2022-JP-KDDI", "€"], ["UTF-8", "ISO-2022-JP-KDDI", "é"], ["UTF-8", "ISO-2022-JP-KDDI", "ｱ"],
         ["UTF-16LE", "ISO-2022-JP", "é".encode("UTF-16LE").b], ["UTF-16LE", "ISO-2022-JP", "€".encode("UTF-16LE").b], ["ISO-8859-1", "ISO-2022-JP", "\xe9".b], ["ISO-8859-1", "EUC-JP", "\xe9".b],
         ["UTF-8", "IBM037", "é"], ["UTF-8", "IBM037", "€"], ["Windows-31J", "ISO-2022-JP", "\x87\x40".b], ["EUC-JP", "US-ASCII", "\xa4\xa2".b], ["UTF-8", "EUC-JP", "¥"],
         ["UTF-8", "stateless-ISO-2022-JP", "A\x80B".b], ["Shift_JIS", "stateless-ISO-2022-JP", "A\x80B".b], ["UTF-8", "ISO-2022-JP", "A\xe3".b],
        ].each do |from, to, s|
          res << [from, to, s.bytes, t.() { [info.(from, to, s), msg.(from, to, s)] }]
        end
        res
        "##,
    );
}

/// The names on the exception itself, from `#convert` and from
/// `String#encode`, and `#source_encoding` / `#destination_encoding`
/// resolved from them.
#[test]
fn the_exception_carries_the_hop() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        res = []
        res << t.() { ec = Encoding::Converter.new("UTF-8", "stateless-ISO-2022-JP"); begin; ec.convert("€"); rescue => e; [e.class.to_s, e.message, e.source_encoding_name, e.destination_encoding_name, e.source_encoding.to_s, e.destination_encoding.to_s, ec.last_error.class.to_s]; end }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "stateless-ISO-2022-JP"); begin; ec.convert("A\x80B".b.force_encoding("EUC-JP")); rescue => e; [e.class.to_s, e.message, e.source_encoding_name, e.destination_encoding_name, e.error_bytes.bytes, e.readagain_bytes]; end }
        res << t.() { begin; "é".encode("ISO-2022-JP"); rescue => e; [e.source_encoding.to_s, e.destination_encoding.to_s]; end }
        res << t.() { begin; "€".encode("UTF-16LE").encode("ISO-2022-JP"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.error_char]; end }
        res << t.() { begin; "一".encode("Windows-1252"); rescue => e; [e.source_encoding_name, e.destination_encoding_name]; end }
        res << t.() { begin; "一".encode("IBM037"); rescue => e; [e.source_encoding_name, e.destination_encoding_name]; end }
        res << t.() { begin; "\x81\xca".b.force_encoding("Shift_JIS").encode("EUC-JP"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.message]; end }
        res << t.() { begin; "\x87\x40".b.force_encoding("Windows-31J").encode("EUC-JP"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.message]; end }
        res << t.() { begin; "\x80".b.force_encoding("EUC-JP").encode("UTF-8"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.message]; end }
        res << t.() { begin; "\x80".b.force_encoding("EUC-JP").encode("Shift_JIS"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.message]; end }
        res << t.() { begin; "\xa4\xa2".b.force_encoding("EUC-JP").encode("US-ASCII"); rescue => e; [e.source_encoding_name, e.destination_encoding_name, e.error_char]; end }
        res
        "##,
    );
}

/// An EUC-JP source read as EUC-JP on the way into the stateless
/// family: `"\x80"` is invalid input, replaced under `invalid:
/// :replace`, a chunk ending in half a cell is incomplete — and a
/// cell with no stateless counterpart is read out of `src` with the
/// undefined-conversion report, as every other path already did.
#[test]
fn an_eucjp_source_is_read_as_eucjp() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        res = []
        res << t.() { "A\x80B".b.force_encoding("EUC-JP").encode("stateless-ISO-2022-JP", invalid: :replace) }
        res << t.() { "A\x80B".b.force_encoding("EUC-JP").encode("ISO-2022-JP", invalid: :replace) }
        res << t.() { "A\x80B".b.force_encoding("EUC-JP").encode("ISO-2022-JP") }
        res << t.() { "A\xa4".b.force_encoding("EUC-JP").encode("stateless-ISO-2022-JP") }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "stateless-ISO-2022-JP", invalid: :replace); s = "A\x80B".b.force_encoding("EUC-JP"); d = +""; r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "ISO-2022-JP", invalid: :replace); [ec.convert("A\x80B".b.force_encoding("EUC-JP")).bytes, ec.finish.bytes] }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "stateless-ISO-2022-JP"); s = "A\xa4".b.force_encoding("EUC-JP"); d = +""; r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "stateless-ISO-2022-JP"); s = "A\xa4".b.force_encoding("EUC-JP"); d = +""; r = ec.primitive_convert(s, d, nil, nil, partial_input: true); r2 = ec.primitive_convert("\xa2".b.force_encoding("EUC-JP"), d); [r, r2, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("EUC-JP", "ISO-2022-JP"); s = "A\x80B".b.force_encoding("EUC-JP"); d = +""; r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        [["EUC-JP", "A\x8f\xa2\xafB".b], ["EUC-JP", "A\x8e\xb1B".b], ["Shift_JIS", "A\xb1B".b], ["ISO-8859-1", "A\xe9B".b], ["GB18030", "A\xb0\xa1B".b], ["UTF-8", "AéB"], ["UTF-8", "A€B"], ["CP949", "A\xb0\xa1B".b]].each do |from, bytes|
          %w[stateless-ISO-2022-JP ISO-2022-JP].each do |to|
            res << t.() { ec = Encoding::Converter.new(from, to); s = bytes.dup.force_encoding(from); d = +""; r = ec.primitive_convert(s, d); r2 = ec.primitive_convert(s, d); [from, to, r, d.bytes, s.bytes, ec.primitive_errinfo, r2] }
          end
        end
        res
        "##,
    );
}
