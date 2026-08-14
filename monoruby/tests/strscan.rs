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
