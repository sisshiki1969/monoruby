extern crate monoruby;
use monoruby::tests::*;

// Issue #1975: CESU-8 is a Unicode encoding with a codec of its own
// (CRuby's `enc/cesu_8.c`, `OnigmoEncoding::CESU_8`): a surrogate pair
// is one six-byte character standing for the astral scalar, a Regexp
// walks those characters, and case mapping is the Unicode map.

#[test]
fn cesu8_issue_probe() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        c = "aé\u{1F600}b\u{1F600}c".encode("CESU-8")
        e = "\u{1F600}".encode("CESU-8")
        show = ->(m) { m && [m[0].bytes, m[0].encoding.to_s, m.pre_match.bytesize, m.offset(0)] }
        [
          t { [c =~ /b/, show.($~)] },
          t { show.(c.match(/./, 2)) },
          t { c.start_with?(/a/) },
          t { u = c.dup; u[/b/] = "x"; u.bytesize },
          t { c.index(/c/) },
          t { c.scan(/./).size },
          t { "éi".encode("CESU-8").upcase.bytes },
          t { "I".encode("CESU-8").downcase(:turkic).bytes }
        ]
        "##,
    );
}

#[test]
fn cesu8_regexp_walks_characters() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        c = "aé\u{1F600}b\u{1F600}c".encode("CESU-8")
        e = "\u{1F600}".encode("CESU-8")
        show = ->(m) { m && [m[0].bytes, m[0].encoding.to_s, m.pre_match.bytesize, m.offset(0)] }
        [
          t { c.scan(/./).map(&:bytesize) },
          t { c.scan(/\p{Emoji}/).map(&:bytes) },
          t { c.gsub(/./) { |x| x.bytesize.to_s }.bytes },
          t { c.sub(/b/, e).bytesize },
          t { c.split(/b/).map(&:bytesize) },
          t { c.rindex(/./) },
          t { c.byteindex(/c/) },
          t { c[/.b/].bytes },
          t { c.slice(/(.)(b)/, 1).bytes },
          t { c.partition(/b/).map(&:bytesize) },
          t { c.rpartition(/./).map(&:bytesize) },
          t { c.match?(/\A.{6}\z/) },
          t { c =~ /é/ },
          t { c =~ Regexp.new("é".encode("CESU-8")) },
          t { c =~ Regexp.new(e) },
          t { c =~ Regexp.new(e + "+") },
          t { r = Regexp.new("[" + e + "]b"); [r.encoding.to_s, r.fixed_encoding?, c =~ r, $~ && $~[0].bytesize] },
          t { c.each_grapheme_cluster.to_a.size },
          t { c.tr("é", "e").bytes.size },
          t { c =~ /B/i },
          t { "É".encode("CESU-8") =~ Regexp.new("é".encode("CESU-8"), Regexp::IGNORECASE) },
          t { c.end_with?("c".encode("CESU-8")) },
          t { c.sub(/(?<x>b)/) { $~[:x].encoding.to_s } .bytesize },
          t { c.match(/b/).post_match.bytes },
          t { c.match(/b/).byteoffset(0) },
          t { c.unicode_normalize rescue $!.class },
          t { "\xED\xA0\xBD".force_encoding("CESU-8") =~ /./ },
          t { c.chars.size },
          t { c =~ /\u{1F600}/ },
          t { c.match(/[[:alpha:]]+/)[0].bytes },
          t { c.match(/\w+/)[0].bytes },
          t { c.scan(/\X/).size },
          t { c.gsub(/b/, "é".encode("CESU-8")).bytesize },
          t { c.gsub(/b/, "é").bytesize },
          t { c.gsub("b", "x").bytesize },
          t { c.index(e, 3) },
          t { c.squeeze.bytesize },
          t { c.delete("b").bytesize }
        ]
        "##,
    );
}

#[test]
fn cesu8_case_mapping_is_unicode() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        c = "aé\u{1F600}b\u{1F600}c".encode("CESU-8")
        e = "\u{1F600}".encode("CESU-8")
        show = ->(m) { m && [m[0].bytes, m[0].encoding.to_s, m.pre_match.bytesize, m.offset(0)] }
        [
          t { c.upcase.bytes == "AÉ\u{1F600}B\u{1F600}C".encode("CESU-8").bytes },
          t { "\u{10428}".encode("CESU-8").upcase.bytes },
          t { "\u{10400}".encode("CESU-8").downcase.bytes },
          t { "ǆ".encode("CESU-8").capitalize.bytes },
          t { "aÉ".encode("CESU-8").swapcase.bytes },
          t { "ß".encode("CESU-8").upcase.bytes },
          t { "ß".encode("CESU-8").downcase(:fold).bytes },
          t { "É".encode("CESU-8").casecmp?("é".encode("CESU-8")) },
          t { "É".encode("CESU-8").casecmp("é".encode("CESU-8")) },
          t { "é".encode("CESU-8").upcase(:ascii).bytes },
          t { x = "é".encode("CESU-8"); [x.upcase!, x.bytes, x.upcase!] },
          t { "abc".encode("CESU-8").upcase.then { |x| [x, x.encoding.to_s] } },
          t { "aé".encode("CESU-8").to_sym.upcase.to_s.bytes },
          t { "\xED\xA0\xBD".force_encoding("CESU-8").upcase.bytes }
        ]
        "##,
    );
}
