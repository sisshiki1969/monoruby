extern crate monoruby;
use monoruby::tests::*;

// A 7-bit pattern whose encoding is not pinned is matched against a 7-bit
// subject with its US-ASCII compile (`RegexpInner::engine_for`, CRuby's
// `rb_reg_prepare_enc`), and against anything else with the UTF-8 one.
// Every shape below runs both engines on the same regexp object — first
// a 7-bit subject, then a multibyte one, then 7-bit again — and is
// compared with CRuby: positions, captures, `$~`, case folding, the
// patterns that must stay on the UTF-8 engine (`\p{…}`, a non-ASCII
// source, `\u` escapes, pinned encodings), the probe limit, and the
// walking primitives (`gsub`, `scan`, `split`, `StringScanner`).

#[test]
fn ascii_engine_same_answers_as_utf8_engine() {
    run_test(
        r##"
        re = /\b([a-z]+)-(\d+)\b/i
        ascii = "Item-12, ITEM-7 and thing-003"
        multi = "Item-12, ITÉM-7 und dinge-003 — ok"
        r = []
        [ascii, multi, ascii].each do |s|
          r << (s =~ re) << $~[0] << $~[1] << $~[2] << $~.begin(2) << re.match(s, 8)&.[](0)
          r << s.scan(re) << s.gsub(re) { "<#{$1}:#{$2}>" } << s.sub(re, '\2=\1')
          r << s.index(re, 3) << s.rindex(re) << s.match?(re, 9) << s.split(re) << s[re, 1]
        end
        r
        "##,
    );
}

#[test]
fn ascii_engine_case_folding_and_classes() {
    run_test(
        r##"
        r = []
        ["k", "K", "s", "SS", "ss", "ﬀ", "ß", "K"].each do |s|
          r << [s =~ /k/i, s =~ /ss/i, s =~ /ß/i, s =~ /ff/i, s.match?(/\w/), s.match?(/[[:alpha:]]/),
                s.match?(/\p{Alpha}/), s.match?(/[a-z]/i), s.match?(/\d|\s/), s =~ /\A.\z/]
        end
        r << ("hello world".gsub(/o/i, "0")) << ("HeLLo".scan(/l/i)) << ("abc" =~ /(?i)B/)
        r << ("x-y_z".split(/[-_]/)) << ("a1b22c333".scan(/\d+/)) << ("tab\there" =~ /\s/)
        r << ("line1\nline2".scan(/^l\w+$/)) << ("ab\ncd" =~ /b.c/m) << ("ab\ncd" =~ /b.c/)
        r
        "##,
    );
}

#[test]
fn utf8_engine_keeps_patterns_it_must() {
    run_test(
        r##"
        r = []
        # \p{…} pins the regexp to UTF-8 in CRuby too; \u escapes, a
        # non-ASCII source and the n/u modifiers likewise stay off the
        # US-ASCII compile.
        [/\p{Hiragana}/, /\P{Alpha}/, /Ab/, /\u{41 42}/, /Aé/, /é/, /abc/u, /a.c/n, /[^\x00-\x7f]/].each do |re|
          r << [re.fixed_encoding?, "ABC ab" =~ re, "ひらがな AB" =~ re, "AéB" =~ re, "ÉAB".unicode_normalize(:nfd) =~ re]
        end
        r << ("abc" =~ /\x41/) << ("A" =~ /\x41/) << ("café" =~ /f./)
        r
        "##,
    );
}

#[test]
fn ascii_engine_long_subjects_and_positions() {
    run_test(
        r##"
        # Past the probe limit the UTF-8 engine is used unless the caller
        # knows the code range; both must agree.
        short = "a" * 100 + "needle" + "b" * 100
        long = "a" * 5000 + "needle" + "b" * 5000
        re = /NEEDLE|b{3}\z/i
        r = [short =~ re, long =~ re, long.index(re, 5006), long.rindex(re), long.match(re, 4000)&.begin(0)]
        r << long.gsub(/a{1000}/) { |m| m.size.to_s } << long.scan(/b{2000}/).map(&:size)
        r << (long + "é").index(re, 10) << (long + "é").scan(/é|needle/)
        r << short.match?(/\Aa+needle/) << long.match?(/\Aa+needle/) << long.match?(/z/)
        r
        "##,
    );
}

#[test]
fn ascii_engine_string_scanner() {
    run_test(
        r##"
        require "strscan"
        r = []
        ["key = value; k2 = 12 # c", "clé = valeur; k2 = 12 # ç"].each do |src|
          ss = StringScanner.new(src)
          until ss.eos?
            ss.skip(/\s+/)
            r << (ss.scan(/[a-z]\w*/i) || ss.scan(/\d+/) || ss.scan(/[=;#]/) || ss.getch)
            r << ss.pos
          end
          r << ss.check_until(/z/) << ss.scan_until(/\z/)
        end
        r
        "##,
    );
}

#[test]
fn ascii_engine_shares_binary_subject_compile() {
    run_test(
        r##"
        # A BINARY subject with 8-bit content also uses the US-ASCII
        # compile of the pattern; the two uses of one regexp must not
        # disturb each other.
        re = /a(.)c/i
        bin = "xAbC\xffa\xfec".b
        r = [bin =~ re, $~[1].bytes, "AbC" =~ re, $~[1], bin.scan(re).map { |x| x[0].bytes }, "ABC abc" =~ re]
        r << ("abc".force_encoding("US-ASCII") =~ re) << ("abc".force_encoding("ISO-8859-1") =~ re)
        r << ("a\xe9c".force_encoding("ISO-8859-1") =~ re) << $~[1].bytes
        r
        "##,
    );
}
