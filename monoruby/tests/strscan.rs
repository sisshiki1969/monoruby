extern crate monoruby;
use monoruby::tests::*;

// monoruby's StringScanner is implemented in Ruby (stdlib/strscan.rb) on
// top of the `String#__strscan_match` zero-copy primitive; CRuby's is a C
// extension. Comparing outputs exercises the whole scanning surface.

#[test]
fn strscan_basic_scanning() {
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("This is an example string")
        r = []
        r << s.eos?
        r << s.scan(/\w+/) << s.scan(/\w+/) << s.scan(/\s+/) << s.scan(/\s+/)
        r << s.scan(/\w+/) << s.skip(/\s+/) << s.scan(/\w+/)
        r << s.match?(/\s+ex/) << s.check(/\s+\w+/) << s.pos
        r << s.scan_until(/str/) << s.pos << s.matched << s.pre_match << s.post_match
        r << s.skip_until(/in/) << s.exist?(/g/) << s.check_until(/g/) << s.pos
        r << s.scan(/g/) << s.eos? << s.scan(//)
        r
        "#,
    );
}

#[test]
fn strscan_string_patterns_and_pos() {
    run_test_once(
        r#"
        require "strscan"
        r = []
        s = StringScanner.new("abc")
        # String patterns are literal bytes, not regexps.
        r << s.scan("a") << s.scan(".") << s.scan("bc") << s.eos?
        s2 = StringScanner.new("a.c")
        r << s2.scan(".") << s2.skip("a.") << s2.scan(".")
        s3 = StringScanner.new("foo bar")
        s3.pos = 4
        r << s3.scan(/bar/) << s3.pos << s3.eos?
        s3.pos = 0
        r << s3.rest << s3.rest_size
        r
        "#,
    );
}

#[test]
fn strscan_anchor_semantics() {
    // CRuby's default (fixed_anchor: false) anchors \A and ^ at the scan
    // position, because the engine only sees the rest of the string.
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("This is a test")
        r = []
        r << s.scan(/\w+/) << s.scan(/^\d/) << s.scan(/^\s/)
        s.reset
        r << s.scan(/\w+/) << s.scan(/\A\d/) << s.scan(/\A\s/)
        s.reset
        r << s.scan(/\w+/) << s.scan(/( is not|\A is a)/)
        s2 = StringScanner.new("line1\nline2")
        s2.scan_until(/\n/)
        r << s2.scan(/^line2/)
        r
        "#,
    );
}

#[test]
fn strscan_match_data_accessors() {
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("Fri Dec 12 1975 14:39")
        r = []
        r << s.scan(/(\w+) (\w+) (\d+) /)
        r << s[0] << s[1] << s[2] << s[3] << s.size << s.captures
        r << s.values_at(0, 2) << s.matched << s.matched? << s.matched_size
        r << s.pre_match << s.post_match << s.pos
        s.unscan
        r << s.pos << s.scan(/\w+/)
        r
        "#,
    );
}

#[test]
fn strscan_repeated_scan_hot_loop() {
    // The pattern caches and the zero-copy fast path must return fresh,
    // correct results across many scans (JIT-warmed).
    run_test(
        r#"
        res = []
        require "strscan"
        20.times do
            s = StringScanner.new("a1 b2 c3 " * 10)
            toks = []
            until s.eos?
                toks << (s.scan(/[a-z]/) || s.scan(/\d/) || s.scan(/\s+/) && "_")
            end
            res << toks.join
        end
        res
        "#,
    );
}

#[test]
fn strscan_register_accessors() {
    // The register (spans) representation must answer every accessor —
    // named groups, negative indices, getch registers, pre/post_match in
    // the regular-expression sense — identically to CRuby's C strscan.
    run_test_once(
        r#"
        require "strscan"
        r = []
        s = StringScanner.new("Fri Dec 12 1975 14:39")
        r << s.scan(/(\w+)(?<mon>\s\w+)?/) << s[0] << s[1] << s[:mon] << s["mon"] << s[-1] << s[9]
        r << s.size << s.captures << s.matched << s.matched_size << s.pre_match << s.post_match
        begin; s[:nope]; rescue IndexError => e; r << e.class.to_s; end
        r << s.check_until(/\d+/) << s.pre_match << s.post_match << s.matched << s.pos
        r << s.scan_until(/12/) << s.pre_match << s.post_match << s.pos
        r << s.getch << s.matched << s[0] << s.matched_size
        r << s.skip(/\s*(19)(75)\s*/) << s[1] << s[2] << s.captures << s.matched
        s2 = StringScanner.new("a b")
        r << s2.exist?(/b/) << s2.pos << s2.skip_until(/b/) << s2.pos
        s3 = StringScanner.new("test")
        r << s3.scan(/(t)(e)(x)?(s)/) << s3[3] << s3.captures << s3.values_at(0, 4, -1, 3)
        r << s3.unscan.pos
        r
        "#,
    );
}

#[test]
fn string_match_position_boundaries() {
    // String#match clamps past-the-end positions to the end (a zero-width
    // pattern still matches there); String#match? rejects them instead.
    // Negative positions count characters from the end for both.
    run_test_once(
        r#"
        r = []
        [0, 1, 2, 3, 9, -1, -2, -3].each do |i|
            m = "ab".match(/x?/, i)
            r << (m ? [m[0], m.begin(0)] : nil)
        end
        r << "ab".match(/b/, 3) << "ab".match(/b/, -1)&.[](0)
        r << "ab".match?(/x?/, 2) << "ab".match?(/x?/, 3)
        r << "ab".match?(/b/, -1) << "ab".match?(/b/, -9)
        r
        "#,
    );
}

#[test]
fn strscan_literal_string_patterns() {
    // String patterns are literal bytes for the anchored family and a
    // literal byte search for the `_until` family; a name lookup on a
    // String-pattern match has no groups to find.
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("a.b.c a+b")
        r = []
        r << s.scan(".") << s.scan("a") << s.scan(".") << s.skip("b.") << s.pos
        r << s.matched << s.matched_size << s.pre_match << s.post_match << s[0] << s[1]
        r << s.check_until("+") << s.scan_until("+") << s.pos << s.matched << s.pre_match
        r << s.exist?("b") << s.skip_until("b") << s.eos? << s.scan_until("zz") << s.matched?
        s.reset
        r << s.scan_until("") << s.pos << s.scan("") << s.check("a.b")
        r << (begin; s.scan("a"); s["x"]; rescue IndexError => e; e.class; end)
        r
        "#,
    );
}

#[test]
fn strscan_utf8_subjects_in_place() {
    // Non-ASCII UTF-8 subjects are matched in place at a byte position;
    // the registers stay byte offsets, so every accessor agrees with
    // CRuby's byte-based scanner.
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("日本語 text ünïcode 123")
        r = []
        r << s.scan(/\S+/) << s.pos << s.matched_size << s.charpos
        r << s.skip(/\s+/) << s.scan(/(t)(e)(x)(t)?/) << s[2] << s.captures << s.pos
        r << s.scan_until(/ü/) << s.pos << s.pre_match << s.post_match
        r << s.check_until(/\d+/) << s.skip_until(/(\d)(\d)/) << s[1] << s[2] << s.rest
        r << s.scan(/\d/) << s.eos? << s.getch
        s.pos = 3
        r << s.scan(/本/) << s.scan(/語/) << s.pos
        s.reset
        r << s.getch << s.pos << s.matched << s.matched_size << s.getch << s.unscan.pos
        r << s.get_byte << s.pos << s.peek(2) << s.rest_size
        r
        "#,
    );
}

#[test]
fn strscan_register_state_across_calls() {
    // A failed match clears the registers; getch / get_byte record the
    // char as the whole match; unscan restores the previous position.
    run_test_once(
        r#"
        require "strscan"
        s = StringScanner.new("ab12")
        r = []
        r << s.scan(/(a)(b)/) << s.size << s[-1] << s[2] << s[3] << s.values_at(0, 1, 2)
        r << s.scan(/x/) << s.matched? << s.matched << s.size << s.captures << s[0]
        r << s.getch << s.matched << s.matched_size << s.pre_match << s.post_match << s.size
        r << s.get_byte << s.matched << s.unscan.pos << s.matched?
        r << (begin; s.unscan; rescue => e; e.class.to_s; end)
        r << s.scan(/\d+/) << s.unscan.pos << s.scan(/(\d)(\d)/) << s.captures << s.eos?
        r
        "#,
    );
}

#[test]
fn strscan_fallback_subjects_and_pattern_types() {
    // A BINARY subject with 8-bit content is matched in place on its raw
    // bytes under Onigmo's ASCII codec (String patterns stay literal
    // bytes); a pattern that is neither raises the conversion TypeError
    // CRuby reports.
    run_test_once(
        r#"
        require "strscan"
        r = []
        b = StringScanner.new("ab\xFFcd ef".b)
        r << b.scan(/ab/) << b.pos << b.matched << b.scan(/x/) << b.check_until(/c/) << b.scan_until(/c/) << b.pos
        r << b.matched << b.pre_match.bytesize << b.post_match << b.scan("d") << b.skip(/\s/) << b.scan(/(e)(f)/) << b[2] << b.eos?
        r << b.string.encoding.to_s
        s = StringScanner.new("abc")
        s.pos = 3
        r << s.scan("") << s.scan("a") << s.scan_until("a")
        r << (begin; s.scan(1); rescue TypeError => e; e.message; end)
        r << (begin; s.scan_until(:a); rescue TypeError => e; e.message; end)
        r
        "#,
    );
}

/// A BINARY subject with 8-bit content, scanned the way ruby-bench's
/// pure-Ruby JSON parser does it (`force_encoding(ASCII_8BIT)` on UTF-8
/// text, `/n` patterns with byte-range classes and a capture group):
/// the registers are raw byte offsets, so `[]` / `matched` / `pos` are
/// the subject's own bytes, and the JIT-compiled scan loop agrees.
#[test]
fn strscan_binary_subject_with_multibyte_content() {
    run_test(
        r#"
        require "strscan"
        src = "{\"name\": \"K\xC3\xA4rnten\", \"n\": 12, \"s\": \"v\xE2\x82\xACx\"}".b
        r = []
        3.times do
          s = StringScanner.new(src)
          r << s.skip("{") << s.skip(/\s*/)
          while (m = s.scan(/"((?:[^\x0-\x1f"\\]|\\[\x20-\xff])*)"/n))
            key = s[1]
            r << [m.bytesize, key, key.encoding.to_s, s.pos, s.matched.bytesize, s.pre_match.bytesize]
            s.skip(/\s*:\s*/)
            if s.scan(/"((?:[^\x0-\x1f"\\]|\\[\x20-\xff])*)"/n)
              r << s[1] << s[1].bytesize << s[0].bytesize
            else
              r << s.scan(/-?\d+/) << s.matched_size
            end
            s.skip(/\s*/)
            break unless s.skip(",")
            s.skip(/\s*/)
          end
          r << s.skip("}") << s.eos? << s.rest
        end
        t = StringScanner.new("x\xFFy z".b)
        r << t.scan(/./n) << t.getch << t.pos << t.check(/y/) << t.scan_until(/z/) << t.pre_match << t.eos?
        e = StringScanner.new("\xA4\xA2\xA4\xA4 x".force_encoding("EUC-JP"))
        # (bytesizes, not the strings: the oracle cannot round-trip the `p`
        # output of an EUC-JP string through the test harness)
        r << e.scan(/./)&.bytesize << e.pos << e.scan(/\S+/)&.bytesize << e.pos << e.skip(/ /) << e.scan(/x/)
        r
        "#,
    );
}

/// `String#match` / `#match?` / `#=~` and `Regexp#=~` on a BINARY subject
/// with 8-bit content: matched on the raw bytes, so the MatchData's
/// strings keep the subject's bytes and encoding, byte offsets are the
/// subject's own, positions count one char per byte, and a UTF-8 regexp
/// pinned by a non-ASCII char is refused as in CRuby.
#[test]
fn string_match_binary_subjects() {
    run_test(
        r#"
        t = ->(&blk) { begin; blk.call; rescue => e; e.class; end }
        b = "ab\xC3\xA4cd\xE2\x82\xAC ef".b
        r = []
        r << b.match(/cd/n).then { |m| [m[0], m.pre_match, m.post_match, m.begin(0), m.end(0), m.byteoffset(0), m.string.encoding.to_s, m[0].encoding.to_s] }
        r << b.match(/[\x80-\xff]+/n, 3)&.byteoffset(0) << b.match(/e/, -3)&.begin(0) << b.match(/x?/, 100)&.begin(0) << b.match(/a/, -100)
        r << b.match?(/e/) << b.match?(/e/, 9) << b.match?(/e/, 10) << b.match?(/e/, 100) << b.match?(/[\x80-\xff]/n)
        r << (b =~ /cd/) << (b =~ /ef/) << (b =~ /zz/) << ($~ && $~[0]) << ($~ && $~.pre_match.bytesize)
        r << (/ef/ =~ b) << (/\xE2/n =~ b) << (/zz/ =~ b) << $~ << ($~ && $~.byteoffset(0)) << (/ef/.match(b).begin(0))
        u = Regexp.new("\u00e4")
        r << t.call { b.match(u) } << t.call { b =~ u } << t.call { b.match?(u) } << t.call { u.match(b) } << t.call { "abc".b.match(u) }
        r << b.match(/(\w)(\d)?/n).then { |m| [m[1], m[2], m.captures, m.values_at(0, 2)] }
        r << b.match(/cd/n) { |m| m[0] + "!" }
        k = "K\xC3\xA4rnten".b
        r << k.match(/[^\x0-\x1f"]+/n)[0].bytesize << k.match(/rn/)[0].encoding.to_s << k.match(/[\x80-\xff]+/n).byteoffset(0)
        r
        "#,
    );
}
