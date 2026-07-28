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
//! - `FNM_CASEFOLD` compares characters case-insensitively (simple
//!   one-to-one folding).
//! - `FNM_EXTGLOB` enables `{a,b}` brace alternation (expanded up
//!   front, then each expansion is matched).

pub(crate) const FNM_NOESCAPE: u32 = 0x01;
pub(crate) const FNM_PATHNAME: u32 = 0x02;
pub(crate) const FNM_DOTMATCH: u32 = 0x04;
pub(crate) const FNM_CASEFOLD: u32 = 0x08;
pub(crate) const FNM_EXTGLOB: u32 = 0x10;

/// Full `File.fnmatch` entry point.
pub(crate) fn fnmatch(pattern: &str, string: &str, flags: u32) -> bool {
    if flags & FNM_EXTGLOB != 0 {
        expand_braces(pattern, flags & FNM_NOESCAPE != 0)
            .iter()
            .any(|pat| fnmatch_no_extglob(pat, string, flags))
    } else {
        fnmatch_no_extglob(pattern, string, flags)
    }
}

fn fnmatch_no_extglob(pattern: &str, string: &str, flags: u32) -> bool {
    if flags & FNM_PATHNAME != 0 {
        let pats: Vec<&str> = pattern.split('/').collect();
        let strs: Vec<&str> = string.split('/').collect();
        match_segments(&pats, &strs, flags)
    } else {
        let pat: Vec<char> = pattern.chars().collect();
        let s: Vec<char> = string.chars().collect();
        match_segment_chars(&pat, &s, flags, true)
    }
}

/// Segment-wise matching for `FNM_PATHNAME`.
fn match_segments(pats: &[&str], strs: &[&str], flags: u32) -> bool {
    let Some((first, rest)) = pats.split_first() else {
        return strs.is_empty();
    };
    // A `**` segment *followed by more segments* is a globstar; a
    // trailing `**` degrades to `*` (handled by the generic branch —
    // `match_segment` treats consecutive stars as one).
    if *first == "**" && !rest.is_empty() {
        let mut i = 0;
        loop {
            if match_segments(rest, &strs[i..], flags) {
                return true;
            }
            if i >= strs.len() {
                return false;
            }
            // Skipped intermediate segments are subject to the period
            // guard: `**/*` does not descend through `.hidden/`.
            if strs[i].starts_with('.') && flags & FNM_DOTMATCH == 0 {
                return false;
            }
            i += 1;
        }
    }
    let Some((s_first, s_rest)) = strs.split_first() else {
        return false;
    };
    match_segment(first, s_first, flags) && match_segments(rest, s_rest, flags)
}

/// Match one path segment (no `/` on either side).
pub(crate) fn match_segment(pattern: &str, name: &str, flags: u32) -> bool {
    let pat: Vec<char> = pattern.chars().collect();
    let s: Vec<char> = name.chars().collect();
    match_segment_chars(&pat, &s, flags, true)
}

fn fold(c: char, casefold: bool) -> char {
    if casefold {
        c.to_lowercase().next().unwrap_or(c)
    } else {
        c
    }
}

/// The backtracking core. `guard_start`: whether the leading-period
/// guard applies at index 0 of `s`.
fn match_segment_chars(pat: &[char], s: &[char], flags: u32, guard_start: bool) -> bool {
    let noescape = flags & FNM_NOESCAPE != 0;
    let casefold = flags & FNM_CASEFOLD != 0;
    let dotmatch = flags & FNM_DOTMATCH != 0;
    // Is `s[i]` protected by the leading-period guard?
    let guarded = |si: usize| -> bool {
        !dotmatch && guard_start && si == 0 && s.first() == Some(&'.')
    };

    let mut pi = 0;
    let mut si = 0;
    // Backtrack state for the most recent `*`.
    let mut star: Option<(usize, usize)> = None; // (pat index after *, str index)

    loop {
        if si >= s.len() {
            // String consumed: the rest of the pattern must be all `*`s.
            while pi < pat.len() && pat[pi] == '*' {
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
                '*' => {
                    // Collapse consecutive stars.
                    while pi + 1 < pat.len() && pat[pi + 1] == '*' {
                        pi += 1;
                    }
                    star = Some((pi + 1, si));
                    pi += 1;
                    continue;
                }
                '?' => {
                    if !guarded(si) {
                        pi += 1;
                        si += 1;
                        advanced = true;
                    }
                }
                '[' => {
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
                    if c == '\\' && !noescape {
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
fn match_bracket(pat: &[char], c: char, noescape: bool, casefold: bool) -> Option<(bool, usize)> {
    debug_assert_eq!(pat.first(), Some(&'['));
    let mut i = 1;
    let negate = matches!(pat.get(i), Some('!') | Some('^'));
    if negate {
        i += 1;
    }
    let c = fold(c, casefold);
    let mut matched = false;
    // One item: an (optionally escaped) character, possibly the low end
    // of a `-` range. No POSIX first-`]`-literal rule (CRuby).
    let read_char = |i: &mut usize| -> Option<char> {
        let mut ch = *pat.get(*i)?;
        if ch == ']' {
            return None;
        }
        if ch == '\\' && !noescape {
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
            Some(']') => return Some((matched ^ negate, i + 1)),
            _ => {}
        }
        let lo = read_char(&mut i)?;
        // Range? (`x-y` with a real upper bound; a trailing `x-]` is
        // the literal characters `x` and `-`.)
        if pat.get(i) == Some(&'-') && pat.get(i + 1).is_some_and(|&n| n != ']') {
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
/// returned verbatim.
pub(crate) fn expand_braces(pattern: &str, noescape: bool) -> Vec<String> {
    let chars: Vec<char> = pattern.chars().collect();
    // Find the first top-level complete `{...}` group.
    let mut depth = 0usize;
    let mut open = None;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && !noescape && i + 1 < chars.len() {
            i += 2;
            continue;
        }
        match c {
            '{' => {
                if depth == 0 {
                    open = Some(i);
                }
                depth += 1;
            }
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    if let Some(start) = open {
                        let prefix: String = chars[..start].iter().collect();
                        let suffix: String = chars[i + 1..].iter().collect();
                        let inside = &chars[start + 1..i];
                        let mut results = vec![];
                        for alt in split_top_level_commas(inside, noescape) {
                            let expanded = format!("{}{}{}", prefix, alt, suffix);
                            results.extend(expand_braces(&expanded, noescape));
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
    vec![pattern.to_string()]
}

/// Split brace-group contents at top-level commas.
fn split_top_level_commas(chars: &[char], noescape: bool) -> Vec<String> {
    let mut out = vec![];
    let mut cur = String::new();
    let mut depth = 0usize;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && !noescape && i + 1 < chars.len() {
            cur.push(c);
            cur.push(chars[i + 1]);
            i += 2;
            continue;
        }
        match c {
            '{' => {
                depth += 1;
                cur.push(c);
            }
            '}' => {
                depth = depth.saturating_sub(1);
                cur.push(c);
            }
            ',' if depth == 0 => {
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
                fnmatch(pat, s, flags),
                expected,
                "fnmatch({pat:?}, {s:?}, {flags}) should be {expected}"
            );
        }
    }

    #[test]
    fn brace_expansion_order() {
        assert_eq!(
            expand_braces("a{b,c}{d,e}", false),
            vec!["abd", "abe", "acd", "ace"]
        );
        assert_eq!(expand_braces("{,x,y/z}.rb", false), vec![".rb", "x.rb", "y/z.rb"]);
        assert_eq!(expand_braces("a{b", false), vec!["a{b"]);
        assert_eq!(expand_braces("\\{a,b}", false), vec!["\\{a,b}"]);
    }
}
