extern crate monoruby;
use monoruby::tests::*;

// Issue #1978: `String#scan` with a String pattern on a UTF-16 / UTF-32
// receiver searches character-aligned, as `rb_pat_search` does for a
// literal pattern, instead of compiling a Regexp from it.

#[test]
fn scan_string_pattern_on_wide_receivers() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message.b.inspect}"; end
        %w[UTF-16LE UTF-16BE UTF-32LE UTF-32BE].map do |e|
          s = "aéb\u{1F600}éa".encode(e)
          hi = "é".encode(e)
          [e, t { s.scan(hi).map(&:bytes) }, t { s.scan("b".encode(e)).size }, t { s.scan("".encode(e)).size },
           t { s.scan("\u{1F600}".encode(e)).map { |x| [x.encoding.to_s, x.bytesize] } },
           t { r = []; s.scan(hi) { |m| r << [m.bytes, $~.begin(0), $~.pre_match.bytesize] }; r },
           t { s.scan(hi); [$~[0].bytes, $~.begin(0), $~.end(0), $~.post_match.bytesize, $~[0].encoding.to_s] },
           t { s.scan("z".encode(e)); $~ },
           t { s.scan("b") }, t { s.scan("é") }, t { s.scan(/b/) },
           t { s.scan(Regexp.new("b".encode(e))).size },
           t { s.scan("\x00".force_encoding(e)).size }, t { s.scan("a\x00".b) },
           # A needle that only lines up between two characters is no match.
           t { "\u6162\u6263".encode(e).scan("\u6262".encode(e)).size }]
        end
        "##,
    );
}
