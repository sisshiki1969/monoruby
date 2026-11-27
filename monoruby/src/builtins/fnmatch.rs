//! CRuby-compatible `fnmatch(3)`-style pattern matching, shared by
//! `File.fnmatch` and `Dir.glob`.
//!
//! Semantics follow CRuby's `file.c:fnmatch` (verified against CRuby
//! 4.0.2), notably:
//!
//! - The leading-period guard: `*` / `?` / `[...]` never match a `.` at
//!   the start of the string — and, with `FNM_PATHNAME`, at the start of
//!   any path segment — unless `FNM_DOTMATCH` is given. A literal `.`
//!   in the pattern always matches.
//! - With `FNM_PATHNAME` the pattern and string are matched segment by
//!   segment (wildcards never cross `/`), and a `**` *segment followed
//!   by more segments* matches zero or more intermediate segments
//!   ("globstar"); a trailing `**` segment behaves like `*`. Skipped
//!   segments are still subject to the period guard.
//! - Without `FNM_PATHNAME`, wildcards match `/` freely and the period
//!   guard applies only at the very start of the string.
//! - `\\` escapes the next character (making it literal) unless
//!   `FNM_NOESCAPE` is given, in which case `\\` is an ordinary literal
//!   character while `*` / `?` / `[` keep their special meaning.
//! - Bracket expressions support `!` / `^` negation and `-` ranges.
//!   There is no POSIX "first `]` is literal" rule: `[]` right at the
//!   start is an (unmatchable) empty class — `File.fnmatch("[]]", "]")`
//!   is `false` in CRuby. An unterminated `[` matches nothing at all
//!   (not even a literal `[`).
//! - `FNM_CASEFOLD` compares ASCII letters case-insensitively, and
//!   nothing else (CRuby's `downcase` is `ISUPPER` / `TOLOWER`).
//! - Both sides are bytes, read as characters of one encoding (the
//!   pattern's for `Dir.glob`, the two strings' compatible one for
//!   `File.fnmatch`), so a name in another encoding still matches: a
//!   byte that starts no character is a character of its own, as
//!   `rb_enc_mbclen` steps over it.
//! - `FNM_EXTGLOB` enables `{a,b}` brace alternation (expanded up
//!   front, then each expansion is matched).

pub(crate) const FNM_NOESCAPE: u32 = 0x01;
pub(crate) const FNM_PATHNAME: u32 = 0x02;
pub(crate) const FNM_DOTMATCH: u32 = 0x04;
pub(crate) const FNM_CASEFOLD: u32 = 0x08;
pub(crate) const FNM_EXTGLOB: u32 = 0x10;

use crate::value::Encoding;

/// One character of a pattern or a name: its code point
/// (`rb_enc_codepoint`, so ASCII is itself in every encoding this is
/// used with), or [`BAD_BYTE`] plus the byte for one that starts no
/// character.
type Unit = u64;

const BAD_BYTE: Unit = 1 << 32;

const fn ascii(c: u8) -> Unit {
    c as Unit
}

/// The characters of `bytes` in `enc`.
fn units(bytes: &[u8], enc: Encoding) -> Vec<Unit> {
    // Nearly every name is ASCII: one unit a byte, nothing to decode.
    if bytes.is_ascii() && enc.is_ascii_compatible() {
        return bytes.iter().map(|&b| b as Unit).collect();
    }
    let mut out = Vec::with_capacity(bytes.len());
    let mut pos = 0;
    while pos < bytes.len() {
        let width = match crate::value::rvalue::precise_mbclen(enc, bytes, pos) {
            crate::value::rvalue::PreciseLen::Char(n) => n,
            _ => 0,
        };
        match crate::value::rvalue::enc_codepoint(enc, &bytes[pos..pos + width]) {
            Some(cp) if width > 0 => {
                out.push(cp as Unit);
                pos += width;
            }
            _ => {
                out.push(BAD_BYTE | bytes[pos] as Unit);
                pos += 1;
            }
        }
    }
    out
}

/// Full `File.fnmatch` entry point.
pub(crate) fn fnmatch(pattern: &[u8], string: &[u8], enc: Encoding, flags: u32) -> bool {
    if flags & FNM_EXTGLOB != 0 {
        expand_braces(pattern, enc, flags & FNM_NOESCAPE != 0)
            .iter()
            .any(|pat| fnmatch_no_extglob(pat, string, enc, flags))
    } else {
        fnmatch_no_extglob(pattern, string, enc, flags)
    }
}

fn fnmatch_no_extglob(pattern: &[u8], string: &[u8], enc: Encoding, flags: u32) -> bool {
    if flags & FNM_PATHNAME != 0 {
        let pats: Vec<&[u8]> = pattern.split(|&b| b == b'/').collect();
        let strs: Vec<&[u8]> = string.split(|&b| b == b'/').collect();
        match_segments(&pats, &strs, enc, flags)
    } else {
        match_units(&units(pattern, enc), &units(string, enc), flags, true)
    }
}

/// Segment-wise matching for `FNM_PATHNAME`.
fn match_segments(pats: &[&[u8]], strs: &[&[u8]], enc: Encoding, flags: u32) -> bool {
    let Some((first, rest)) = pats.split_first() else {
        return strs.is_empty();
    };
    // A `**` segment *followed by more segments* is a globstar; a
    // trailing `**` degrades to `*` (handled by the generic branch —
    // `match_segment` treats consecutive stars as one).
    if *first == b"**" && !rest.is_empty() {
        let mut i = 0;
        loop {
            if match_segments(rest, &strs[i..], enc, flags) {
                return true;
            }
            if i >= strs.len() {
                return false;
            }
            // Skipped intermediate segments are subject to the period
            // guard: `**/*` does not descend through `.hidden/`.
            if strs[i].starts_with(b".") && flags & FNM_DOTMATCH == 0 {
                return false;
            }
            i += 1;
        }
    }
    let Some((s_first, s_rest)) = strs.split_first() else {
        return false;
    };
    match_segment(first, s_first, enc, flags) && match_segments(rest, s_rest, enc, flags)
}

/// Match one path segment (no `/` on either side).
pub(crate) fn match_segment(pattern: &[u8], name: &[u8], enc: Encoding, flags: u32) -> bool {
    match_units(&units(pattern, enc), &units(name, enc), flags, true)
}

/// Whether a segment pattern contains an unescaped metacharacter.
pub(crate) fn has_meta(pattern: &[u8], enc: Encoding) -> bool {
    let pat = units(pattern, enc);
    let mut i = 0;
    while i < pat.len() {
        match pat[i] {
            c if c == ascii(b'\\') => i += 1,
            c if c == ascii(b'*') || c == ascii(b'?') || c == ascii(b'[') => return true,
            _ => {}
        }
        i += 1;
    }
    false
}

fn fold(c: Unit, casefold: bool) -> Unit {
    if casefold && (ascii(b'A')..=ascii(b'Z')).contains(&c) {
        c + 0x20
    } else {
        c
    }
}

/// The backtracking core. `guard_start`: whether the leading-period
/// guard applies at index 0 of `s`.
fn match_units(pat: &[Unit], s: &[Unit], flags: u32, guard_start: bool) -> bool {
    let noescape = flags & FNM_NOESCAPE != 0;
    let casefold = flags & FNM_CASEFOLD != 0;
    let dotmatch = flags & FNM_DOTMATCH != 0;
    // Is `s[i]` protected by the leading-period guard?
    let guarded = |si: usize| -> bool {
        !dotmatch && guard_start && si == 0 && s.first() == Some(&ascii(b'.'))
    };

    let mut pi = 0;
    let mut si = 0;
    // Backtrack state for the most recent `*`.
    let mut star: Option<(usize, usize)> = None; // (pat index after *, str index)

    loop {
        if si >= s.len() {
            // String consumed: the rest of the pattern must be all `*`s.
            while pi < pat.len() && pat[pi] == ascii(b'*') {
                pi += 1;
            }
            if pi == pat.len() {
                return true;
            }
            // Backtracking cannot help — a star can only *consume more*.
            return false;
        }
        let mut advanced = false;
        if pi < pat.len() {
            match pat[pi] {
                c if c == ascii(b'*') => {
                    // Collapse consecutive stars.
                    while pi + 1 < pat.len() && pat[pi + 1] == ascii(b'*') {
                        pi += 1;
                    }
                    star = Some((pi + 1, si));
                    pi += 1;
                    continue;
                }
                c if c == ascii(b'?') => {
                    if !guarded(si) {
                        pi += 1;
                        si += 1;
                        advanced = true;
                    }
                }
                c if c == ascii(b'[') => {
                    // An unterminated bracket matches nothing at all —
                    // not even a literal `[` (`fnmatch("abc[de",
                    // "abc[de")` is false in CRuby) — so `None` simply
                    // falls through to the backtracking below.
                    if !guarded(si)
                        && let Some((matched, plen)) =
                            match_bracket(&pat[pi..], s[si], noescape, casefold)
                        && matched
                    {
                        pi += plen;
                        si += 1;
                        advanced = true;
                    }
                }
                mut c => {
                    if c == ascii(b'\\') && !noescape {
                        // Escape: next pattern char is literal (a
                        // dangling trailing `\` stays a literal `\`).
                        if pi + 1 < pat.len() {
                            pi += 1;
                            c = pat[pi];
                        }
                    }
                    if fold(c, casefold) == fold(s[si], casefold) {
                        pi += 1;
                        si += 1;
                        advanced = true;
                    }
                }
            }
        }
        if advanced {
            continue;
        }
        // Mismatch: backtrack to the last star, extending its span by
        // one character — unless that character is guard-protected.
        match star {
            Some((star_pi, star_si)) if star_si < s.len() && !guarded(star_si) => {
                star = Some((star_pi, star_si + 1));
                pi = star_pi;
                si = star_si + 1;
            }
            _ => return false,
        }
    }
}

/// Parse and match a bracket expression starting at `pat[0] == '['`
/// against `c`. Returns `(matched, consumed_pattern_chars)`, or `None`
/// when the bracket has no closing `]` (caller treats `[` literally).
fn match_bracket(pat: &[Unit], c: Unit, noescape: bool, casefold: bool) -> Option<(bool, usize)> {
    debug_assert_eq!(pat.first(), Some(&ascii(b'[')));
    let mut i = 1;
    let negate = matches!(pat.get(i), Some(&c) if c == ascii(b'!') || c == ascii(b'^'));
    if negate {
        i += 1;
    }
    let c = fold(c, casefold);
    let mut matched = false;
    // One item: an (optionally escaped) character, possibly the low end
    // of a `-` range. No POSIX first-`]`-literal rule (CRuby).
    let read_char = |i: &mut usize| -> Option<Unit> {
        let mut ch = *pat.get(*i)?;
        if ch == ascii(b']') {
            return None;
        }
        if ch == ascii(b'\\') && !noescape {
            if let Some(&next) = pat.get(*i + 1) {
                *i += 1;
                ch = next;
            }
        }
        *i += 1;
        Some(ch)
    };
    loop {
        match pat.get(i) {
            None => return None, // unterminated
            Some(&b) if b == ascii(b']') => return Some((matched ^ negate, i + 1)),
            _ => {}
        }
        let lo = read_char(&mut i)?;
        // Range? (`x-y` with a real upper bound; a trailing `x-]` is
        // the literal characters `x` and `-`.)
        if pat.get(i) == Some(&ascii(b'-')) && pat.get(i + 1).is_some_and(|&n| n != ascii(b']')) {
            i += 1; // consume '-'
            let Some(hi) = read_char(&mut i) else {
                return None;
            };
            let (lo, hi) = (fold(lo, casefold), fold(hi, casefold));
            if lo <= c && c <= hi {
                matched = true;
            }
        } else if fold(lo, casefold) == c {
            matched = true;
        }
    }
}

/// Expand `{a,b}` brace alternations (used by `FNM_EXTGLOB` and by
/// `Dir.glob`, where braces are always active). Expansion is
/// depth-first and keeps the source order: the first alternative of
/// the left-most group comes first. Escaped braces/commas (`\{`,
/// `\}`, `\,`) are literal unless `noescape`. Unmatched braces are
/// returned verbatim. The pattern is walked by character of `enc`, so
/// a byte of a multibyte character is never taken for a brace.
pub(crate) fn expand_braces(pattern: &[u8], enc: Encoding, noescape: bool) -> Vec<Vec<u8>> {
    if !pattern.contains(&b'{') {
        return vec![pattern.to_vec()];
    }
    let chars = char_slices(pattern, enc);
    expand_brace_chars(&chars, noescape)
        .into_iter()
        .map(|cs| cs.concat())
        .collect()
}

/// `bytes` cut into its characters in `enc` (a byte that starts none is
/// one of its own).
fn char_slices(bytes: &[u8], enc: Encoding) -> Vec<&[u8]> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut pos = 0;
    while pos < bytes.len() {
        let width = match crate::value::rvalue::precise_mbclen(enc, bytes, pos) {
            crate::value::rvalue::PreciseLen::Char(n) => n,
            _ => 1,
        };
        out.push(&bytes[pos..pos + width]);
        pos += width;
    }
    out
}

fn expand_brace_chars<'a>(chars: &[&'a [u8]], noescape: bool) -> Vec<Vec<&'a [u8]>> {
    // Find the first top-level complete `{...}` group.
    let mut depth = 0usize;
    let mut open = None;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == b"\\" && !noescape && i + 1 < chars.len() {
            i += 2;
            continue;
        }
        match c {
            b"{" => {
                if depth == 0 {
                    open = Some(i);
                }
                depth += 1;
            }
            b"}" => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    if let Some(start) = open {
                        let prefix = &chars[..start];
                        let suffix = &chars[i + 1..];
                        let inside = &chars[start + 1..i];
                        let mut results = vec![];
                        for alt in split_top_level_commas(inside, noescape) {
                            let expanded: Vec<&[u8]> = [prefix, alt.as_slice(), suffix].concat();
                            results.extend(expand_brace_chars(&expanded, noescape));
                        }
                        return results;
                    }
                    break;
                }
            }
            _ => {}
        }
        i += 1;
    }
    vec![chars.to_vec()]
}

/// Split brace-group contents at top-level commas.
fn split_top_level_commas<'a>(chars: &[&'a [u8]], noescape: bool) -> Vec<Vec<&'a [u8]>> {
    let mut out = vec![];
    let mut cur = vec![];
    let mut depth = 0usize;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == b"\\" && !noescape && i + 1 < chars.len() {
            cur.push(c);
            cur.push(chars[i + 1]);
            i += 2;
            continue;
        }
        match c {
            b"{" => {
                depth += 1;
                cur.push(c);
            }
            b"}" => {
                depth = depth.saturating_sub(1);
                cur.push(c);
            }
            b"," if depth == 0 => {
                out.push(std::mem::take(&mut cur));
            }
            _ => cur.push(c),
        }
        i += 1;
    }
    out.push(cur);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Ground truth captured from CRuby 4.0.2 (`File.fnmatch`).
    #[test]
    fn cruby_ground_truth() {
        #[rustfmt::skip]
        let cases: &[(&str, &str, u32, bool)] = &[
            ("cat", "cat", 0, true), ("cat", "category", 0, false),
            ("c?t", "cat", 0, true), ("c??t", "cat", 0, false),
            ("c*", "cats", 0, true),
            ("*", ".profile", 0, false), ("*", ".profile", 4, true),
            ("*", "dave/.profile", 0, true),
            ("*/*", "dave/.profile", 0, true),
            ("*/*", "dave/.profile", 2, false),
            ("*/*", "dave/.profile", 6, true),
            ("*", "dave/.profile", 2, false),
            ("**/*", "a/b/c", 2, true), ("**/*", "a", 2, true),
            ("**/*", ".a", 2, false),
            ("**/*", "a/.b/c", 2, false), ("**/*", "a/.b/c", 6, true),
            ("**/foo", "a/b/c/foo", 2, true), ("**/foo", "foo", 2, true),
            ("**", "a/b", 2, false), ("**", "a", 2, true),
            ("?", "/", 0, true), ("?", "/", 2, false),
            ("*", "/", 0, true), ("*", "/", 2, false),
            ("[/]", "/", 0, true), ("[/]", "/", 2, false),
            ("[a-/]", "-", 2, false),
            ("[a-z]", "c", 0, true), ("[a-z]", "C", 0, false),
            ("[a-z]", "C", 8, true), ("CAt", "cat", 8, true),
            ("[^a-z]", "1", 0, true), ("[!a-z]", "1", 0, true),
            ("[]]", "]", 0, false), ("[]a]", "a", 0, false),
            ("\\?", "?", 0, true), ("\\?", "a", 0, false),
            ("\\*", "*", 0, true), ("\\a", "a", 0, true),
            ("\\a", "\\a", 0, false), ("\\a", "\\a", 1, true),
            ("\\*", "\\ab", 1, true), ("\\*", "*", 1, false),
            ("[\\?]", "?", 0, true),
            ("c\\at", "cat", 0, true), ("c\\at", "c\\at", 1, true),
            ("./*", "./foo", 2, true), ("*", "./foo", 2, false),
            ("./*", "foo", 2, false),
            ("{a,b}", "a", 16, true), ("{a,b}", "c", 16, false),
            ("{a,b}", "a", 0, false), ("{a,b}", "{a,b}", 0, true),
            ("a{b,c{d,e}}", "acd", 16, true),
            ("*.[ch]", "main.c", 0, true), ("*.[ch]pp", "main.cpp", 0, true),
            ("a?b", "a/b", 0, true), ("a?b", "a/b", 2, false),
            ("*a*", ".ab", 0, false), ("?a", ".a", 0, false),
            ("[.]a", ".a", 0, false), ("[.]a", ".a", 4, true),
            ("**/", "a/", 2, true),
            ("a**b", "aXb", 0, true), ("a**b", "a/b", 0, true),
            ("a**b", "a/b", 2, false),
            ("**", ".a", 2, false), ("**", ".a/b", 2, false),
            // Unterminated brackets match nothing (not even a literal `[`).
            ("abc[de", "abcd", 0, false), ("abc[de", "abc[de", 0, false),
            ("[[]a-z", "[a-z", 0, true),
            // Multibyte paths (char-based matching).
            ("こ*は.txt", "こんにちは.txt", 0, true),
            ("?ん*", "こんにちは", 0, true),
        ];
        for &(pat, s, flags, expected) in cases {
            assert_eq!(
                fnmatch(pat.as_bytes(), s.as_bytes(), Encoding::UTF8, flags),
                expected,
                "fnmatch({pat:?}, {s:?}, {flags}) should be {expected}"
            );
        }
    }

    #[test]
    fn brace_expansion_order() {
        let expand = |p: &str| -> Vec<String> {
            expand_braces(p.as_bytes(), Encoding::UTF8, false)
                .into_iter()
                .map(|b| String::from_utf8(b).unwrap())
                .collect()
        };
        assert_eq!(expand("a{b,c}{d,e}"), vec!["abd", "abe", "acd", "ace"]);
        assert_eq!(expand("{,x,y/z}.rb"), vec![".rb", "x.rb", "y/z.rb"]);
        assert_eq!(expand("a{b"), vec!["a{b"]);
        assert_eq!(expand("\\{a,b}"), vec!["\\{a,b}"]);
    }

    /// Ground truth captured from CRuby 4.0.9: names that are not text
    /// of the pattern's encoding, and patterns in other encodings
    /// (#2243).
    #[test]
    fn bytes_and_encodings() {
        let euc = Encoding::EUC_JP;
        let bin = Encoding::Ascii8;
        let utf8 = Encoding::UTF8;
        #[rustfmt::skip]
        let cases: &[(&[u8], &[u8], Encoding, bool)] = &[
            (b"\xA4\xA2*", b"\xA4\xA2.txt", bin, true),
            (b"?.txt", b"\xA4\xA2.txt", utf8, false),
            (b"??.txt", b"\xA4\xA2.txt", utf8, true),
            (b"?.txt", b"\xA4\xA2.txt", euc, true),
            (b"?.txt", b"\xA4\xA2.txt", bin, false),
            (b"?.txt", "あ.txt".as_bytes(), utf8, true),
            (b"???.txt", "あ.txt".as_bytes(), bin, true),
            (b"*", b"\xFF", bin, true),
            ("[あ-う].txt".as_bytes(), "い.txt".as_bytes(), utf8, true),
            ("Ä".as_bytes(), "ä".as_bytes(), utf8, false),
        ];
        for &(pat, s, enc, expected) in cases {
            assert_eq!(
                fnmatch(pat, s, enc, 0),
                expected,
                "fnmatch({pat:?}, {s:?}, {enc:?})"
            );
        }
        assert!(fnmatch(b"a", b"A", utf8, FNM_CASEFOLD));
        assert!(!fnmatch("Ä".as_bytes(), "ä".as_bytes(), utf8, FNM_CASEFOLD));
    }
}
