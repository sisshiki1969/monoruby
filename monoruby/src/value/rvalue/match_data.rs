use super::*;
use smallvec::SmallVec;

/// Byte span of one capture group, `(start, end)`.
///
/// `NO_MATCH` (the all-ones span) marks a group that did not
/// participate in the match. Real spans never reach `u32::MAX`
/// because match subjects are limited to < 4 GiB (asserted at
/// construction).
type Span = (u32, u32);
const NO_MATCH: Span = (u32::MAX, u32::MAX);

#[inline]
fn encode_span(m: Option<(usize, usize)>) -> Span {
    match m {
        Some((start, end)) => (start as u32, end as u32),
        None => NO_MATCH,
    }
}

#[inline]
fn decode_span(span: Span) -> Option<(usize, usize)> {
    if span == NO_MATCH {
        None
    } else {
        Some((span.0 as usize, span.1 as usize))
    }
}

///
/// MatchData payload.
///
/// `$~` is materialized on every successful regexp match, so this
/// struct is sized to keep that as cheap as possible while fitting
/// RValue's 48-byte payload: 8 (regex) + 16 (`Box<str>`) + 24
/// (SmallVec) = 48. The inline capacity of 2 covers the most common
/// match shapes (whole match only, or one capture group) without a
/// positions heap allocation.
///
#[derive(Debug, Clone)]
#[repr(C)]
pub struct MatchDataInner {
    regex: Option<Regexp>,
    heystack: Box<str>,
    matches: SmallVec<[Span; 2]>,
}

impl GC<RValue> for MatchDataInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(re) = &self.regex {
            re.mark(alloc);
        }
    }
}

impl MatchDataInner {
    pub fn new(heystack: &str, matches: Vec<Option<(usize, usize)>>) -> Self {
        assert!(
            heystack.len() < u32::MAX as usize,
            "match subject longer than 4GiB is not supported"
        );
        MatchDataInner {
            regex: None,
            heystack: Box::from(heystack),
            matches: matches.into_iter().map(encode_span).collect(),
        }
    }

    /// Build directly from onigmo `Captures` without intermediate
    /// position vectors (hot path: `$~` save on every match).
    pub fn from_captures(captures: &Captures, heystack: &str) -> Self {
        assert!(
            heystack.len() < u32::MAX as usize,
            "match subject longer than 4GiB is not supported"
        );
        let matches = captures
            .iter()
            .map(|m| encode_span(m.as_ref().map(|m| (m.start(), m.end()))))
            .collect();
        MatchDataInner {
            regex: None,
            heystack: Box::from(heystack),
            matches,
        }
    }

    /// Builder helper: attach a `Regexp` to a freshly-constructed
    /// `MatchDataInner`. Used when reconstructing a `MatchData`
    /// from `Executor`'s special-variable stash so named-capture
    /// lookup (`md["name"]` / `md[:name]`) finds the regex's
    /// group table.
    pub fn with_regex(mut self, regex: Regexp) -> Self {
        self.regex = Some(regex);
        self
    }

    pub fn from_capture(captures: Captures, heystack: &str, regex: Regexp) -> Self {
        Self::from_captures(&captures, heystack).with_regex(regex)
    }

    pub fn regexp(&self) -> Option<Regexp> {
        self.regex
    }

    pub fn string(&self) -> &str {
        &self.heystack
    }

    pub fn pos(&self, pos: usize) -> Option<(usize, usize)> {
        decode_span(self.matches[pos])
    }

    /// Matched bytes for capture `pos`. Slices the haystack on
    /// **byte** boundaries (always safe) — required for binary
    /// (`/n`) regexps whose onigmo byte offsets land mid-UTF-8 of
    /// the underlying Rust `String`. Returning `&str` here would
    /// panic in `core::str::slice_error_fail` on a non-char boundary
    /// (which is a `panic_nounwind` ⇒ process abort across the
    /// `#[monoruby_builtin]` extern "C" boundary).
    pub fn at(&self, pos: usize) -> Option<&[u8]> {
        self.pos(pos)
            .map(|(start, end)| &self.heystack.as_bytes()[start..end])
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn captures(&self) -> impl Iterator<Item = Option<&[u8]>> {
        self.matches
            .iter()
            .map(|m| decode_span(*m).map(|(start, end)| &self.heystack.as_bytes()[start..end]))
    }

    pub fn to_s(&self) -> String {
        String::from_utf8_lossy(self.at(0).unwrap()).into_owned()
    }

    pub fn inspect(&self) -> String {
        let mut s = format!(
            "#<MatchData \"{}\"",
            String::from_utf8_lossy(self.at(0).unwrap())
        );
        let names = self
            .regexp()
            .and_then(|r| r.capture_names().ok())
            .unwrap_or_default();
        for i in 1..self.len() {
            if names.is_empty() {
                s.push_str(&format!(" {i}:"));
            } else {
                s.push_str(&format!(" {}:", names[i - 1]));
            }
            if let Some(capture) = self.at(i) {
                s.push_str(&format!("\"{}\"", String::from_utf8_lossy(capture)));
            } else {
                s.push_str(&format!("nil"));
            }
        }
        s.push_str(">");
        s
    }
}
