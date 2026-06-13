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
/// RValue's 48-byte payload: 8 (regex) + 8 (`Value`) + 24 (SmallVec)
/// = 40. The inline capacity of 2 covers the most common match shapes
/// (whole match only, or one capture group) without a positions heap
/// allocation.
///
/// `heystack` is a *snapshot* String taken at match time — a shared
/// (copy-on-write) view when the subject's Value was known to the
/// matcher (see `Executor::resolve_haystack`), an owned copy
/// otherwise — so later mutation of the subject never changes this
/// MatchData (CRuby behaviour: `MatchData#string` is frozen). Its
/// content is guaranteed valid UTF-8: every constructor takes the
/// haystack as `&str`.
///
#[derive(Debug, Clone)]
#[repr(C)]
pub struct MatchDataInner {
    regex: Option<Regexp>,
    heystack: Value,
    matches: SmallVec<[Span; 2]>,
}

impl GC<RValue> for MatchDataInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        if let Some(re) = &self.regex {
            re.mark(alloc);
        }
        self.heystack.mark(alloc);
    }
}

impl MatchDataInner {
    /// Snapshot the haystack as a String `Value`. When `resolved` says
    /// `s` borrows from a known String Value's buffer (verified by
    /// pointer containment in `Executor::resolve_haystack`), the
    /// snapshot is a zero-copy shared substring (the subject becomes a
    /// CoW sharer); otherwise the bytes are copied, exactly as the old
    /// `Box<str>` field did.
    fn snapshot(s: &str, resolved: Option<(Value, usize)>) -> Value {
        match resolved {
            Some((val, offset)) => string_substring(val, offset, offset + s.len()),
            None => Value::string_from_str(s),
        }
    }

    /// Build directly from onigmo `Captures` without intermediate
    /// position vectors (hot path: `$~` save on every match).
    /// `resolved` (from `Executor::resolve_haystack`) enables the
    /// zero-copy haystack snapshot.
    pub fn from_captures_snap(
        captures: &Captures,
        heystack: &str,
        resolved: Option<(Value, usize)>,
    ) -> Self {
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
            heystack: Self::snapshot(heystack, resolved),
            matches,
        }
    }

    /// `from_captures_snap` without a haystack Value (always copies).
    pub fn from_captures(captures: &Captures, heystack: &str) -> Self {
        Self::from_captures_snap(captures, heystack, None)
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

    pub fn from_capture_snap(
        captures: Captures,
        heystack: &str,
        resolved: Option<(Value, usize)>,
        regex: Regexp,
    ) -> Self {
        Self::from_captures_snap(&captures, heystack, resolved).with_regex(regex)
    }

    pub fn regexp(&self) -> Option<Regexp> {
        self.regex
    }

    fn heystack_bytes(&self) -> &[u8] {
        self.heystack.as_rstring_inner().as_bytes()
    }

    pub fn string(&self) -> &str {
        // SAFETY: every constructor receives the haystack as `&str`
        // and the snapshot preserves those exact bytes (shared view or
        // copy), so the content is valid UTF-8.
        unsafe { std::str::from_utf8_unchecked(self.heystack_bytes()) }
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
            .map(|(start, end)| &self.heystack_bytes()[start..end])
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn captures(&self) -> impl Iterator<Item = Option<&[u8]>> {
        self.matches
            .iter()
            .map(|m| decode_span(*m).map(|(start, end)| &self.heystack_bytes()[start..end]))
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
