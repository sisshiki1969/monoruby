extern crate monoruby;
use monoruby::tests::*;

// String-pattern search in `String#index` / `#rindex` / `#sub` / `#gsub`
// / `#count` / `#delete` / `#squeeze` runs on memmem / memchr fast paths
// (builtins/string.rs) instead of the regex engine or a per-call
// character-boundary table. Every shape is compared against CRuby:
// positions and anchors, ASCII and multibyte receivers, needles that
// straddle character boundaries, `$~` after `sub` / `gsub`, and the
// backslash-expanding replacement that must keep the slow path.

#[test]
fn index_rindex_string_pattern_ascii() {
    run_test(
        r##"
        s = "hello world, hello ruby"
        long = "x" * 300 + "needle" + "y" * 10
        [
          s.index("hello"), s.index("hello", 1), s.index("hello", -10), s.index("hello", 100),
          s.index("o w"), s.index("ruby"), s.index("rubyx"), s.index("", 5), s.index("", 23), s.index("", 24),
          s.index("l", 3), s.index("l", -1), s.index("world", -100),
          s.rindex("hello"), s.rindex("hello", 12), s.rindex("hello", 13), s.rindex("hello", 0), s.rindex("hello", -12),
          s.rindex("ruby"), s.rindex("zzz"), s.rindex("", 3), s.rindex("", 100), s.rindex("y", -1),
          long.index("needle"), long.rindex("needle"), long.index("needle", 301), long.index("xx", 298),
          "abcabc".rindex("bc"), "abcabc".rindex("bc", 3), "abcabc".rindex("bc", 2), "abcabc".index("abcabcd"),
        ]
        "##,
    );
}

#[test]
fn index_rindex_string_pattern_multibyte() {
    run_test(
        r##"
        s = "aあbいcあb"
        r = [
          s.index("b"), s.index("b", 3), s.index("あ"), s.index("あ", 2), s.index("あb"), s.index("いc", 2),
          s.index("c", -3), s.index("あ", 6), s.index("", 7), s.index("", 8),
          s.rindex("あ"), s.rindex("あ", 4), s.rindex("あ", 3), s.rindex("b"), s.rindex("b", 5), s.rindex("x"),
          "日本語のテキスト日本".index("日本", 1), "日本語のテキスト日本".rindex("日本"),
          ("x" * 200 + "あ" * 50 + "z").index("z"), ("x" * 200 + "あ" * 50 + "z").rindex("あ"),
        ]
        # A needle that only occurs inside a multibyte character's bytes
        # must not match at a non-boundary.
        begin
          r << "ああ".index("\x81".b)
        rescue => e
          r << e.class
        end
        r << "ああ".index("\x81".force_encoding("UTF-8")) rescue r << $!.class
        r
        "##,
    );
}

#[test]
fn sub_gsub_string_pattern_and_backref() {
    run_test(
        r##"
        s = "hello world"
        r = []
        r << s.sub("o", "0") << s.gsub("o", "0") << s.sub("z", "0") << s.gsub("z", "0")
        r << s.sub("", "X") << s.gsub("", "-") << s.sub("hello world", "") << s.gsub("l", "")
        r << s.sub("l", '[\0]') << s.gsub("l", '<\&>') << s.sub("l", '\\\\') << s.sub("l", '\1') << s.sub("wor", '\`|\'')
        r << "héllo wörld".sub("l", "L") << "héllo wörld".gsub("ö", "o") << "héllo".gsub("é", "e").encoding.name
        r << ("ab" * 100).gsub("b", "cc").size << ("ab" * 100).sub("ab", "").size
        t = "hello".dup
        r << t.sub!("z", "y") << t.sub!("l", "L") << t << t.gsub!("l", "L") << t << t.gsub!("q", "")
        r << "hello".sub("l", "L").frozen? << "hello".sub("l", "L").encoding.name
        begin
          "frozen".freeze.sub!("f", "F")
        rescue => e
          r << e.class
        end
        r
        "##,
    );
}

#[test]
fn sub_gsub_string_pattern_sets_backref() {
    run_test_once(
        r##"
        s = "hello world"
        r = []
        s.sub("o", "0"); r << [$~.class, $~[0], $~.pre_match, $~.post_match, $~.begin(0), $~.end(0), $~.to_a]
        s.sub("zz", "0"); r << $~
        s.gsub("o", "0"); r << [$~[0], $~.begin(0), $~.pre_match]
        s.gsub("zz", "0"); r << $~
        "héllo".sub("l", "L"); r << [$~.begin(0), $~.pre_match]
        t = "abc".dup; t.sub!("b", "B"); r << [$~[0], t]
        t.gsub!("q", "Q"); r << $~
        "he[[o".gsub("[", "]"); r << ($~.regexp == /\[/) << $~.regexp.source
        "a.b".sub(".", "-"); r << $~.regexp.source << $~[0]
        # `$~` stays a per-frame variable.
        def probe; "xyz".sub("y", "Y"); $~[0]; end
        "outer".sub("t", "T"); r << probe << $~[0]
        r
        "##,
    );
}

#[test]
fn count_delete_squeeze_single_char() {
    run_test(
        r##"
        s = "hello world, hello ruby"
        m = "héllo wörld ll"
        [
          s.count("l"), s.count("z"), s.count(" "), s.count("-"), s.count("^"), s.count("\\"), s.count("l", "lo"),
          m.count("l"), m.count("é"), m.count("ö"), "a-b-c".count("-"), "a^b".count("^"), "a\\b".count("\\"),
          s.delete("l"), s.delete("z"), m.delete("l"), m.delete("é"), "a-b-c".delete("-"), "a^b".delete("^"),
          s.squeeze("l"), "helllo".squeeze("l"), "helllo".squeeze("z"), "héélllo".squeeze("l"), "héélllo".squeeze("é"), "aaa".squeeze("a"),
          s.dup.delete!("l"), s.dup.delete!("z"), "helllo".dup.squeeze!("l"), "hello".dup.squeeze!("z"),
          ("ab" * 500).count("b"), ("ab" * 500).delete("a").size,
        ]
        "##,
    );
}

#[test]
fn index_rindex_string_pattern_non_utf8_receivers() {
    // Receivers the memmem fast path cannot index directly: multibyte
    // non-UTF-8 encodings walk `iter_char_bytes` up to each hit (a hit
    // inside a character — Shift_JIS `表` ends in the byte of `\` — is
    // skipped), broken UTF-8 takes the same walk, and `rindex` on them
    // tabulates the boundaries up to the anchor.
    run_test(
        r##"
        r = []
        e = "aあいうbい".encode("EUC-JP")
        i = "い".encode("EUC-JP")
        r << e.index(i) << e.index(i, 3) << e.index("z".encode("EUC-JP")) << e.index("b".encode("EUC-JP"), 6) << e.index("b".encode("EUC-JP"), 7)
        r << e.rindex(i) << e.rindex(i, 2) << e.rindex(i, 5) << e.rindex("z".encode("EUC-JP")) << e.rindex("a".encode("EUC-JP"), 0) << e.rindex("う".encode("EUC-JP"), 100)
        s = "表\\示".encode("Shift_JIS")
        bs = "\\".encode("Shift_JIS")
        r << s.index(bs) << s.rindex(bs) << s.index("示".encode("Shift_JIS")) << s.rindex("表".encode("Shift_JIS"), 0) << s.index("表".encode("Shift_JIS"), 1)
        # The rightmost byte-level candidate sits inside `表`; the search
        # must step back to the standalone `\`.
        t = "a\\表\\表".encode("Shift_JIS")
        r << "\\表".encode("Shift_JIS").rindex(bs) << t.rindex(bs) << t.rindex(bs, 2) << "表".encode("Shift_JIS").rindex(bs)
        b = "ab\xffcd\xffab".b.force_encoding("UTF-8")
        r << b.valid_encoding? << b.index("ab") << b.index("ab", 1) << b.index("zz") << b.index("cd", 100)
        r << b.rindex("ab") << b.rindex("ab", 5) << b.rindex("zz") << b.rindex("b", 0)
        r << "aあb".index("b", 3) << "aあb".index("z") << "aあb".index("あ", 1) << "aあb".rindex("b", 1)
        # ASCII-only receiver in a non-UTF-8 encoding: byte == char index.
        a = "abcabc".encode("Shift_JIS")
        bc = "bc".encode("Shift_JIS")
        r << a.index(bc) << a.index(bc, 2) << a.index(bc, 6) << a.rindex(bc) << a.rindex(bc, 3) << a.rindex(bc, 0) << a.rindex("zz".encode("Shift_JIS"))
        r
        "##,
    );
}

#[test]
fn string_pattern_fast_path_fallbacks() {
    // Shapes the fast paths hand back to the general code: a
    // non-UTF-8 receiver for `count` / `delete` / `squeeze` / `sub`, a
    // `to_str` replacement, a non-String pattern, and the type errors.
    run_test(
        r##"
        r = []
        s = "表\\示".encode("Shift_JIS")
        bs = "\\".encode("Shift_JIS")
        e = "aあいうbい".encode("EUC-JP")
        r << s.count(bs) << (s.delete(bs) == s) << e.squeeze("a".encode("EUC-JP")).bytesize
        r << s.sub("示".encode("Shift_JIS"), "X".encode("Shift_JIS")).encoding.name
        class ToStr; def to_str; "R"; end; end
        r << "hello".sub("l", ToStr.new) << "hello".gsub("l", ToStr.new) << "hello".sub(/l/, "L") << "hello".gsub(/l/, "L")
        begin; "hello".sub("l", 1); rescue TypeError => ex; r << ex.class; end
        begin; "hello".count(1); rescue TypeError => ex; r << ex.class; end
        r
        "##,
    );
}

#[test]
fn string_pattern_backref_survives_gc() {
    // The `$~` of a substring search holds the pattern String; it must
    // stay alive across a collection and still serve `$~.regexp`.
    run_test_once(
        r##"
        r = []
        "hello world".sub("wor", "WOR"); GC.start; r << $~.regexp.source << $~[0] << $~.pre_match
        "hello world".gsub("o", "0"); GC.start; r << $~.regexp.source << $~.begin(0)
        r
        "##,
    );
}
