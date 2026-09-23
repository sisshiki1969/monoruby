extern crate monoruby;
use monoruby::tests::*;

// `destination_bytesize` on an ISO-2022-JP source (#1619): the source
// is read unit by unit, an escape sequence being a unit that writes
// nothing. Into stateless-ISO-2022-JP — one transcoder — the call
// stops once the destination is full and an escape has been read,
// even at the end of the input; a character that does not fit is read
// whole, written as far as it fits and held. Into a destination behind
// the pivot the character the cap stopped on is taken out of `src`
// with the escape in front of it, in source bytes.

/// `[result, bytes written, bytes consumed, dst, errinfo[0]]` for five
/// inputs into four destinations at caps `0..=5`.
#[test]
fn the_cap_is_met_unit_by_unit() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        cap = ->(from, to, bytes, n) { ec = Encoding::Converter.new(from, to); s = bytes.dup.force_encoding(from); d = +""; r = ec.primitive_convert(s, d, nil, n); [r, d.bytes.size, bytes.bytesize - s.bytesize, d.bytes, ec.primitive_errinfo[0]] }
        inputs = { "abc" => "ABC".b, "a-jis-b" => "A\e$B0l\e(BB".b, "jis2" => "\e$B0l0m".b, "jis-close" => "\e$B0l\e(B".b, "a-jis" => "A\e$B0l".b }
        res = []
        inputs.each do |name, bytes|
          %w[stateless-ISO-2022-JP UTF-8 EUC-JP Shift_JIS].each do |to|
            (0..5).each do |n|
              res << [name, to, n, t.() { cap.("ISO-2022-JP", to, bytes, n) }]
            end
          end
        end
        %w[CP50220 CP50221].each do |d|
          ["A\e$B0l\e(BB".b, "\e$B0l\e(B".b, "\x0e\x31\x0fA".b].each do |b|
            (0..5).each do |n|
              res << t.() { ec = Encoding::Converter.new(d, "CP51932"); s = b.dup.force_encoding(d); dd = +""; r = ec.primitive_convert(s, dd, nil, n); [d, b.bytes, n, r, dd.bytes, s.bytes] }
            end
          end
        end
        res
        "##,
    );
}

/// The documented loop — call until `:finished` — writes the same
/// bytes in the same number of calls, and an escape past the point a
/// call stopped at is the next call's to read.
#[test]
fn the_streaming_loop_agrees_call_for_call() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class.to_s, e.message]; end }
        inputs = { "abc" => "ABC".b, "a-jis-b" => "A\e$B0l\e(BB".b, "jis2" => "\e$B0l0m".b, "jis-close" => "\e$B0l\e(B".b, "a-jis" => "A\e$B0l".b, "abc-jis" => "abc\e$B0l".b, "jis2-xyz" => "\e$B0l0m\e(Bxyz".b }
        res = []
        %w[stateless-ISO-2022-JP UTF-8 EUC-JP].each do |to|
          inputs.each do |name, bytes|
            (1..4).each do |n|
              res << t.() { ec = Encoding::Converter.new("ISO-2022-JP", to); s = bytes.dup.force_encoding("ISO-2022-JP"); d = +""; calls = 0; loop { r = ec.primitive_convert(s, d, nil, n); calls += 1; break if r == :finished || calls > 30 }; [to, name, n, d.bytes, calls] }
            end
          end
        end
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP", "stateless-ISO-2022-JP"); s = "\e$B0l\e(B".b.force_encoding("ISO-2022-JP"); d = +""; r = ec.primitive_convert(s, d, nil, 3, partial_input: true); r2 = ec.primitive_convert(s, d, nil, 3, partial_input: true); r3 = ec.primitive_convert("", d); [r, r2, r3, d.bytes, s.bytes] }
        res << t.() { ec = Encoding::Converter.new("ISO-2022-JP", "UTF-8"); s = "A\e$B0l\e(BB".b.force_encoding("ISO-2022-JP"); d = +""; out = []; 8.times { r = ec.primitive_convert(s, d, nil, 1); out << [r, d.bytes, s.bytes]; break if r == :finished }; out }
        res
        "##,
    );
}
