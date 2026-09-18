extern crate monoruby;
use monoruby::tests::*;

// Regexp operations over a byte-oriented subject with 8-bit content
// (#1377): BINARY, ISO-8859-1, Shift_JIS. The iterating and slicing
// paths (`scan`, `split`, `sub` / `gsub` in every form, `slice` / `[]`,
// `[]=`, `slice!`, `index` / `rindex`, `partition`, `start_with?`) walk
// the subject's raw bytes under Onigmo's native codec, so a `/n` pattern
// matches the one byte 0xFF (not the two bytes of its UTF-8 surrogate),
// every span is a byte range of the subject itself, and `$~` holds the
// subject's own bytes and encoding.

/// The issue's examples: split and sub used to abort, scan / gsub /
/// sub matched each high byte twice as U+FFFD.
#[test]
fn binary_subject_scan_split_sub_gsub() {
    run_test_once(
        r##"
        s = "a\xffb".b
        res = []
        res << s.split(/[^a-z]/n)
        res << s.split(/[^a-z]/)
        res << s.sub(/[^a-z]/n) { |m| m.bytes.inspect }
        res << s.scan(/[^a-z]/n).map(&:bytes)
        res << s.scan(/./n).map(&:bytes)
        res << s.scan(/./).map(&:bytes)
        res << s.gsub(/[^a-z]/n) { |m| m.bytes.inspect }
        res << s.gsub(/[^a-z]/n, "\xff".b => "F")
        res << s.sub(/[^a-z]/n, "X")
        res << s.gsub(/[^a-z]/n, "X")
        res << [0xc3, 0xbf].pack("C*").scan(/[^a-z]/n).map(&:bytes)
        res << s.match(/[^a-z]/n)[0].bytes
        res << (s =~ /[^a-z]/n)
        res << s.index(/[^a-z]/n)
        res << s.rindex(/[^a-z]/n)
        t = "a\xffb\xfec".b
        res << t.split(/[^a-z]/n)
        res << t.split(/[^a-z]/n, 2)
        res << t.split(/[^a-z]/n, -1)
        res << t.split(/([^a-z])/n).map(&:bytes)
        res << t.partition(/[^a-z]/n).map(&:bytes)
        res << t.rpartition(/[^a-z]/n).map(&:bytes)
        res << t.scan(/[^a-z]/n).map(&:bytes)
        res << t.gsub(/[^a-z]/n) { |m| "<#{m.bytes.join(",")}>" }
        res << t.gsub(/[^a-z]/n, "\xff".b => "F", "\xfe".b => "E")
        res << t.gsub(/[^a-z]/n, "-")
        res << t.gsub(/[^a-z]/n).to_a.map(&:bytes)
        res << s.split(/\xff/n).map(&:bytes)
        res << s.split("").map(&:bytes)
        res << s.split(//).map(&:bytes)
        res << "\xff\xfeab".b.scan(/../n).map(&:bytes)
        res << "\xff\xfeab".b.scan(/(.)(.)/n).map { |a| a.map(&:bytes) }
        res << "a\xff\xffb".b.gsub(/\xff+/n, "!")
        res << "a\xff\xffb".b.scan(/\xff*/n).map(&:bytes)
        res << "a\xff\xffb".b.gsub(/\xff*/n, "-")
        res << "a\xff\xffb".b.split(/\xff*/n).map(&:bytes)
        res
        "##,
    );
}

/// What the block, the Hash and `$~` see: the subject's own bytes and
/// encoding, with pre- and post-match cut at the right byte.
#[test]
fn binary_subject_match_data_and_chunks() {
    run_test_once(
        r##"
        s = "a\xffb".b
        res = []
        res << s.gsub(/[^a-z]/n) { |m| m.encoding.to_s }
        res << s.scan(/[^a-z]/n).map(&:encoding).map(&:to_s)
        res << s.gsub(/[^a-z]/n) { $~[0].bytes.inspect }
        res << s.gsub(/[^a-z]/n) { $`.bytes.inspect + $'.bytes.inspect }
        res << s.sub(/([^a-z])/n) { $1.bytes.inspect }
        s.scan(/[^a-z]/n); res << [$~[0].bytes, $~.pre_match.bytes, $~.post_match.bytes, $~.begin(0), $~.byteoffset(0)]
        s.gsub(/b/n, "c"); res << [$~[0], $~.pre_match.bytes, $~.begin(0)]
        s.split(/\xff/n); res << $~
        s.index(/b/n); res << [$~[0], $~.begin(0)]
        s.rindex(/[^a-z]/n); res << [$~[0].bytes, $~.begin(0), $~.pre_match.encoding.to_s]
        s[/[^a-z]/n]; res << [$~[0].bytes, $~.end(0)]
        res << s.sub(/(?<x>[^a-z])/n, "[\\k<x>]").bytes
        res << s.sub(/([^a-z])/n, "[\\1]").bytes
        res << s.sub(/([^a-z])/n, "[\\0]").bytes
        res << s.sub(/b/n, "[\\`]").bytes
        res << s.sub(/a/n, "[\\']").bytes
        res << s.gsub(/[^a-z]/n, "<\\&>").bytes
        res << s.gsub(/[^a-z]/n, "\\\\").bytes
        res << s.gsub(/\xff/n, "é".b).bytes
        res << s.gsub(/\xff/n) { "é".b }.bytes
        res << s.gsub(/\xff/n, "\xff".b => "é".b).bytes
        res << s.gsub(/\xff/n, "é").then { |r| [r.bytes, r.encoding.to_s] }
        res << s.gsub(/b/, "c").then { |r| [r.bytes, r.encoding.to_s] }
        res << (begin; s.gsub(/b/) { "é" }; rescue => e; [e.class, e.message]; end)
        res << s.gsub(/\xff/n) { "é" }.then { |r| [r.bytes, r.encoding.to_s] }
        res << (begin; s.gsub(/a/, "é"); rescue => e; [e.class, e.message]; end)
        res << s.gsub(/[^a-z]/n, "-" => "x").bytes
        h = Hash.new { |hash, k| hash[k] = "<#{k.bytes.join}>" }
        res << s.gsub(/[^a-z]/n, h).then { |r| [r.bytes, r.encoding.to_s] }
        res << s.gsub(/[^a-z]/n, "\xff".b => "é").then { |r| [r.bytes, r.encoding.to_s] }
        res
        "##,
    );
}

/// The slicing and searching forms.
#[test]
fn binary_subject_slice_index_assign_start_with() {
    run_test_once(
        r##"
        s = "a\xffb".b
        res = []
        res << s.slice(/[^a-z]/n).bytes
        res << s[/[^a-z]/n].bytes
        res << s[/([^a-z])(b)/n, 2]
        res << s[/(?<x>[^a-z])/n, "x"].bytes
        res << s[/(?<x>[^a-z])/n, :x].bytes
        res << s[/[^a-z]/n, 1]
        res << s[/z/n]
        res << s[/[^a-z]/n].encoding.to_s
        t = s.dup; t[/[^a-z]/n] = "Y"; res << [t.bytes, t.encoding.to_s]
        t = s.dup; t[/([^a-z])(b)/n, 2] = "Z"; res << t.bytes
        t = s.dup; res << (begin; t[/z/n] = "Y"; rescue => e; [e.class, e.message]; end)
        t = s.dup; res << [t.slice!(/[^a-z]/n).bytes, t.bytes, t.encoding.to_s]
        t = s.dup; res << [t.slice!(/([^a-z])(b)/n, 2), t.bytes]
        t = s.dup; res << [t.slice!(/z/n), t.bytes]
        res << s.start_with?(/a\xff/n)
        res << s.start_with?(/\xff/n)
        res << s.start_with?(/a/n)
        s.start_with?(/a\xff/n); res << $~[0].bytes
        res << s.index(/b/n, 1)
        res << s.index(/[^a-z]/n, 2)
        res << s.index(/$/n, 3)
        res << "a\xffb\xffc".b.rindex(/\xff/n)
        res << "a\xffb\xffc".b.rindex(/\xff/n, 2)
        res << "a\xffb\xffc".b.rindex(/\xff/n, 1)
        res << "a\xffb\xffc".b.rindex(/z/n)
        res << "a\xffb\xffc".b.rindex(//n)
        res << "a\xffb\xffc".b.index(//n, 5)
        res << s.match?(/\xffb/n)
        res << s.each_grapheme_cluster.map(&:bytes)
        res
        "##,
    );
}

/// Other byte-oriented encodings with a native codec: a Latin-1 subject
/// is walked byte by byte, a Shift_JIS one by its double-byte
/// characters.
#[test]
fn latin1_and_shift_jis_subjects() {
    run_test_once(
        r##"
        res = []
        l = "h\xE9llo w\xF6rld".dup.force_encoding("ISO-8859-1")
        res << l.scan(/\w+/).map(&:bytes)
        res << l.split(/\s/).map(&:bytes)
        res << l.gsub(/[^a-z ]/) { |m| "<%02x>" % m.ord }
        res << (begin; l.gsub(/[\x80-\xff]/n) { |m| m }; rescue => e; [e.class, e.message]; end)
        res << l.sub(/l+/, "L").bytes
        res << l.gsub(/h/) { |m| m.upcase }.bytes
        res << l[/w.r/].bytes
        res << l.index(/w/)
        res << l.rindex(/l/)
        res << l.partition(/ /).map(&:encoding).map(&:to_s)
        res << (begin; l.start_with?(/h\xe9/n); rescue => e; [e.class, e.message]; end)
        res << l.start_with?(/h./)
        sj = "\x82\xa0a\x82\xa2b".dup.force_encoding("Shift_JIS")
        res << sj.scan(/./).map(&:bytes)
        res << sj.split(/a/).map(&:bytes)
        res << sj.sub(/a/, "-").bytes
        res << sj.gsub(/[ab]/) { |m| m.upcase }.bytes
        res << sj.gsub(/./) { |m| m.bytesize.to_s }
        res << sj.index(/b/)
        res << sj.rindex(/a/)
        res << sj[/a./].bytes
        res << sj.split(//).map(&:bytes)
        res << sj.split("").map(&:bytes)
        res << sj.split("", 2).map(&:bytes)
        res << sj.scan(/(.)(.)/).map { |a| a.map(&:bytesize) }
        res
        "##,
    );
}
