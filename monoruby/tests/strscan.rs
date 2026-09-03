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
    // A byte-oriented subject with 8-bit content cannot be viewed in
    // place, so Regexp patterns take the MatchData fallback (String
    // patterns stay literal bytes); a pattern that is neither raises the
    // conversion TypeError CRuby reports.
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
