extern crate monoruby;
use monoruby::tests::*;

// `Encoding::Converter#primitive_convert` takes the malformed run out
// of `src` (#1617): the bytes in `primitive_errinfo[3]` and the
// read-again bytes in `[4]` have been read, so the documented retry
// loop makes progress. A `UTF8-MAC` source keeps the cluster before
// the run in its buffer and writes it on the next call.

/// Two calls on each malformed source, against three destinations:
/// `[result, dst, src, errinfo]` after each.
#[test]
fn the_run_and_its_readagain_bytes_leave_src() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        srcs = {
          "UTF8-MAC" => "A\x80B".b,
          "UTF8-MAC 2" => "A\xe3\x81B".b,
          "UTF-16BE" => "\x00A\xd8\x00\x00B".b,
          "UTF-32LE" => "A\x00\x00\x00\xff\xff\xff\xffB\x00\x00\x00".b,
          "CESU-8" => "A\xed\xa0\x80B".b,
          "CESU-8 pair" => "A\xed\xa0\x80\xed\xb0\x80B".b,
          "stateless-ISO-2022-JP" => "A\x92\x21B".b,
          "EUC-JP" => "A\x80B".b,
          "GB18030" => "A\x81\x20B".b,
          "Shift_JIS" => "A\x81\x20B".b,
          "UTF-8" => "A\x80B".b,
        }
        res = []
        srcs.each do |name, bytes|
          enc = name.split(" ").first
          %w[UTF-8 EUC-JP UTF-16LE].each do |d|
            res << t.() {
              ec = Encoding::Converter.new(enc, d); s = bytes.dup.force_encoding(enc); dst = +""
              r1 = ec.primitive_convert(s, dst); e1 = ec.primitive_errinfo; s1 = s.bytes; d1 = dst.bytes
              r2 = ec.primitive_convert(s, dst); e2 = ec.primitive_errinfo; s2 = s.bytes; d2 = dst.bytes
              [name, d, [r1, d1, s1, e1], [r2, d2, s2, e2]]
            }
          end
        end
        res
        "##,
    );
}

/// `#putback` hands the read-again byte back, the held `UTF8-MAC`
/// cluster comes out with the next chunk (`#convert` and `#finish`
/// included), and a cap among the good bytes comes first.
#[test]
fn putback_and_the_held_cluster() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        res = []
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); r = []; begin; ec.convert("A\x80B".b.force_encoding("UTF8-MAC")); rescue => e; r << e.class.to_s; end; r << ec.primitive_errinfo; r << ec.convert("C").bytes; r << ec.finish.bytes; r }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); r = []; begin; ec.convert("A\xe3\x81B".b.force_encoding("UTF8-MAC")); rescue => e; r << e.class.to_s; end; r << ec.primitive_errinfo; r << ec.convert("C").bytes; r << ec.finish.bytes; r }
        res << t.() { ec = Encoding::Converter.new("UTF-16BE", "UTF-8"); s = "\x00A\xd8\x00\x00B".b.force_encoding("UTF-16BE"); d = +""; r = ec.primitive_convert(s, d); pb = ec.putback; [r, d.bytes, s.bytes, pb.bytes, pb.encoding.to_s, ec.putback.bytes] }
        res << t.() { ec = Encoding::Converter.new("CESU-8", "UTF-8"); s = "A\xed\xa0\x80B".b.force_encoding("CESU-8"); d = +""; r = ec.primitive_convert(s, d); pb = ec.putback; [r, d.bytes, s.bytes, pb.bytes, ec.primitive_convert(s, d), d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); s = "A\xe3\x81B".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d); pb = ec.putback; [r, d.bytes, s.bytes, pb.bytes, ec.primitive_convert(s, d), d.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); s = "AB\x80C".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d, nil, 1); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); s = "AB\x80C".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d, nil, 2); [r, d.bytes, s.bytes, ec.primitive_errinfo] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8", invalid: :replace); s = "A\x80B".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d); [r, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "EUC-JP"); s = "A\xe3\x81B".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d); s << "\xe3\x81\x82".b; r2 = ec.primitive_convert(s, d); [r, r2, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF8-MAC", "UTF-8"); s = "A\x80B".b.force_encoding("UTF8-MAC"); d = +""; r = ec.primitive_convert(s, d, nil, nil, partial_input: true); r2 = ec.primitive_convert(s, d, nil, nil, partial_input: true); r3 = ec.primitive_convert("", d); [r, r2, r3, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("UTF-16BE", "UTF-8"); s = "\x00A\xd8\x00\x00B".b.force_encoding("UTF-16BE"); d = +""; out = []; 5.times { r = ec.primitive_convert(s, d); out << [r, d.bytes, s.bytes]; break if r == :finished }; out }
        res
        "##,
    );
}
