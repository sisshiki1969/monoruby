extern crate monoruby;
use monoruby::tests::*;

// Issue #1959: a Regexp matched against a string in one of the encodings
// CRuby defines as a replica of another (the IBM / Mac code pages of
// ASCII-8BIT, TIS-620 and Windows-874 of ISO-8859-11, Windows-1255 / 1256
// of ISO-8859-8 / -6). They have Onigmo's codec of the encoding they
// replicate; without it the match ran over a UTF-8 image of the bytes.

#[test]
fn replica_encodings_match_on_their_own_bytes() {
    run_test_once(
        r##"
        show = ->(m) { m && [m[0].bytes, m[0].encoding.to_s, m.pre_match.bytes, m.post_match.bytes,
                             m.pre_match.encoding.to_s, m.string.bytes, m.byteoffset(0), m.offset(0),
                             m.captures.map { |c| c && c.bytes }] }
        t = ->(&b) { $~ = nil; b.call rescue [$!.class, $!.message] }
        %w[IBM437 IBM852 IBM866 CP850 macRoman macCyrillic TIS-620 Windows-874 Windows-1255 Windows-1256
           Windows-1258 GB1988].map do |e|
          s = "a\xE9b\xE9c".dup.force_encoding(e)
          hi = "\xE9".dup.force_encoding(e)
          [e,
           t.() { [s =~ /b(.)/, show.($~)] },
           t.() { [/b/ =~ s, show.($~)] },
           t.() { [/b/ === s, show.($~)] },
           t.() { show.(s.match(/b/)) },
           t.() { show.(s.match(/./, 2)) },
           t.() { show.(/(.)c/.match(s)) },
           t.() { [s.match?(/\xE9b/n), $~] },
           t.() { [s.start_with?(/a./), show.($~)] },
           t.() { [s.to_sym.start_with?(/a/), show.($~)] },
           t.() { [s.to_sym =~ /b/, /b/.match(s.to_sym).begin(0), /c/.match?(s.to_sym)] },
           t.() { u = s.dup; u[/b/] = "x"; [u.bytes, show.($~)] },
           t.() { u = s.dup; [u.slice!(/b./).bytes, u.bytes, show.($~)] },
           t.() { [s.index(/c/), s.rindex(/./), s.byteindex(/b/), show.($~)] },
           t.() { [s.scan(/./).map(&:bytes), s.split(/b/).map(&:bytes), s.sub(/./) { $&.bytes.inspect }.bytes] },
           t.() { [s.gsub(/[^a-c]/, "_"), s.partition(/b/).map(&:bytes), s[/.c/].bytes, s[/(.)c/, 1].encoding.to_s] },
           # a pattern with the encoding's own high bytes
           t.() { r = Regexp.new(hi); [r.encoding.to_s, s =~ r, show.($~), s.index(hi), s.match?(hi), s.scan(hi).size] },
           t.() { [s.sub(hi, "x").bytes, s.tr(hi, "y").bytes, s.count(hi), s.delete(hi).bytes] }]
        end
        "##,
    );
}

#[test]
fn string_index_assignment_and_slice_bang_by_string() {
    // `s[str] = x` and `s.slice!(str)` search the receiver's own bytes,
    // aligned to its characters — an 8-bit receiver used to be refused
    // as invalid UTF-8 — and `slice!` answers a copy of the argument.
    run_test_once(
        r##"
        t = ->(&b) { b.call rescue [$!.class, $!.message] }
        res = %w[IBM437 ISO-8859-1 ASCII-8BIT Windows-1252 EUC-JP Shift_JIS UTF8-MAC].map do |e|
          s = "a\xE9\xA1b\xE9\xA1c".dup.force_encoding(e)
          hi = "\xE9\xA1".dup.force_encoding(e)
          [e,
           t.() { u = s.dup; u[hi] = "x"; [u.bytes, u.encoding.to_s] },
           t.() { u = s.dup; u["b"] = hi; u.bytes },
           t.() { u = s.dup; u["zz"] = "x"; u.bytes },
           t.() { u = s.dup; r = u.slice!(hi); [r.bytes, r.encoding.to_s, u.bytes] },
           t.() { u = s.dup; r = u.slice!("b".encode("US-ASCII")); [r, r.encoding.to_s, u.bytes] },
           t.() { u = s.dup; [u.slice!("zz"), u.bytes] },
           t.() { u = s.dup.freeze; u.slice!("b") },
           t.() { u = s.dup; u["b", 1] = "x" }]
        end
        # a Shift_JIS trail byte is not the ASCII letter it looks like
        sj = "\x83\x41A".dup.force_encoding("Shift_JIS")
        res << t.() { u = sj.dup; u["A"] = "z"; u.bytes } << t.() { u = sj.dup; [u.slice!("A"), u.bytes] }
        res
        "##,
    );
}

#[test]
fn slice_bang_by_regexp_group() {
    // By number or by name; a group that took no part in the match
    // removes nothing and answers "", and `$~` is over the receiver in
    // its own encoding whatever view the match walked.
    run_test_once(
        r##"
        t = ->(&b) { b.call rescue [$!.class, $!.message] }
        s = ->(e = "UTF-8") { "aébcabc".dup.force_encoding(e) }
        [
          t.() { u = s.(); [u.slice!(/é(?<x>b)/, "x"), u, $~.to_a] },
          t.() { u = s.(); [u.slice!(/(?<x>b)|(?<x>c)/, :x), u] },
          t.() { u = s.(); [u.slice!(/(?<x>a)(?<x>é)/, "x"), u] },
          t.() { u = s.(); u.slice!(/b/, "nope") },
          t.() { u = s.(); u.slice!(/b/, :nope) },
          t.() { u = s.(); [u.slice!(/a(z)?/, 1), u.slice!(/a(?<q>z)?/, :q), u.slice!(/a(z)?/, -1), u] },
          t.() { u = s.(); [u.slice!(/a(z)?/, 2), u.slice!(/a(z)?/, -3), u.slice!(/a(z)?/, -2), u] },
          t.() { u = s.(); [u.slice!(/z/), $~, u] },
          t.() { u = s.().freeze; u.slice!(/b/) },
          t.() { u = s.("UTF8-MAC"); r = u.slice!(/b(.)/); [r, r.encoding.to_s, u, $~[0].encoding.to_s, $~.pre_match.encoding.to_s, $1.encoding.to_s, $~.string.encoding.to_s] },
          t.() { u = s.("IBM437"); r = u.slice!(/b(.)/, 1); [r.bytes, r.encoding.to_s, u.bytes, $~.byteoffset(0)] },
          t.() { u = s.("IBM437"); r = u.slice!(/a(z)?/, 1); [r, r.encoding.to_s, u.bytes] },
        ]
        "##,
    );
}

#[test]
fn regexp_match_positions_are_characters() {
    // `Regexp#=~` and `Regexp#~` answered a byte offset for a non-ASCII
    // UTF-8 subject; `Regexp#match(str, pos)` clamps a `pos` past the end
    // to the end rather than answering nil (or matching from 0); and a
    // Symbol is matched as its name in its own encoding.
    run_test_once(
        r##"
        s = "aéb"
        mac = s.dup.force_encoding("UTF8-MAC")
        $_ = "ééb"
        [
          /b/ =~ s, /b/.send(:=~, s), s =~ /b/, /b/ =~ mac, mac =~ /b/, /b/ =~ :"aéb",
          /b/.match(:"aéb").begin(0), ~/b/,
          /x?/.match("abc", 9).to_a, /x?/.match("abc", 3).to_a, /x?/.match("abc", 9).begin(0),
          /c/.match("abc", 9), /./.match("abc", -1).to_a, /./.match("abc", -9), /x?/.match(s, 9).begin(0),
          "abc".match(/x?/, 9).to_a, "abc".match(/x?/, 3).to_a,
          /b/.match("a\xE9b".dup.force_encoding("IBM437").to_sym).pre_match.then { |s| [s.bytes, s.encoding.to_s] },
          (/\xE9/n.match("a\xE9b".dup.force_encoding("IBM437")) rescue [$!.class, $!.message]),
        ]
        "##,
    );
}
