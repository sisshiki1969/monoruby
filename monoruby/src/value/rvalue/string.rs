use super::*;

/// Longest encoding name `Encoding::try_from_str` can recognise
/// (`WINDOWS_31J` and friends are far shorter; the cap only has to be an
/// upper bound for the stack-buffer fast path).
const MAX_ENC_NAME: usize = 32;

use smallvec::SmallVec;
use std::cell::Cell;
use std::cmp::Ordering;

pub mod pack;
mod printable;

/// The widest an EUC-JP character gets (onigenc's `mbmaxlen`).
pub(crate) const EUCJP_MAX_LEN: usize = 3;

/// The widest a Shift_JIS character gets.
pub(crate) const SJIS_MAX_LEN: usize = 2;

/// Classify the EUC-JP sequence starting at `bytes[pos]`.
///
/// The lead byte fixes the width — ASCII (1); `0x8E` + 1 (2, JIS X
/// 0201 katakana); `0x8F` + 2 (3, JIS X 0212); `0xA1..=0xFE` + 1 (2,
/// JIS X 0208) — and every byte after it must be `0xA1..=0xFE`.
/// `0x80..=0x8D`, `0x90..=0xA0` and `0xFF` lead nothing.
///
/// Read off onigenc's own answers: `0x8E`'s second byte is the full
/// `0xA1..=0xFE`, not just the `0xA1..=0xDF` the kana block occupies,
/// so `"\x8E\xFC"` is a valid two-byte character to CRuby.
pub(crate) fn eucjp_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    let len = match lead {
        0x00..=0x7f => return PreciseLen::Char(1),
        0x8e | 0xa1..=0xfe => 2,
        0x8f => 3,
        _ => return PreciseLen::Invalid,
    };
    for i in 1..len {
        match bytes.get(pos + i) {
            None => return PreciseLen::NeedMore,
            Some(0xa1..=0xfe) => {}
            Some(_) => return PreciseLen::Invalid,
        }
    }
    PreciseLen::Char(len)
}

/// Classify the Shift_JIS / CP932 sequence starting at `bytes[pos]`.
///
/// ASCII and the `0xA1..=0xDF` half-width kana stand alone; a
/// `0x81..=0x9F | 0xE0..=0xFC` lead takes a `0x40..=0x7E | 0x80..=0xFC`
/// trail. `0x80`, `0xA0` and `0xFD..=0xFF` lead nothing.
pub(crate) fn sjis_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f | 0xa1..=0xdf => PreciseLen::Char(1),
        0x81..=0x9f | 0xe0..=0xfc => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0x40..=0x7e | 0x80..=0xfc) => PreciseLen::Char(2),
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Classify the EUC-KR sequence starting at `bytes[pos]`.
///
/// A 94×94 double-byte set over ASCII: `0xA1..=0xFE` leads and takes a
/// trail from the same range. GB2312 and GB12345 have exactly this
/// shape, and so does EUC-TW's two-byte half (#1473).
/// Classify the stateless-ISO-2022-JP sequence starting at `bytes[pos]`.
///
/// ISO-2022-JP's repertoire with the escape sequences taken out: ASCII,
/// plus a two-byte form with a lead in `0x81..=0x8F` and a trail in
/// `0xA0..=0xFF`. Despite sitting with the EUC-JP family in CRuby's
/// naming, it shares none of EUC-JP's byte structure — the two disagree
/// on 10182 of the one- and two-byte sequences — so it gets a walk of
/// its own rather than riding EUC-JP's the way `eucJP-ms` and `CP51932`
/// legitimately do (#1562).
pub(crate) fn stateless_iso2022jp_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f => PreciseLen::Char(1),
        // The shift-free form of ISO-2022-JP: a character set is named
        // by a lead byte rather than by an escape sequence still in
        // effect, and the bytes after it are that set's cell, as
        // EUC-JP writes it. `0x81..=0x8f` names a single-byte set,
        // `0x90..=0x99` a two-byte one — so those sequences are three
        // bytes long, which the walk read as a stray byte followed by
        // a two-byte one (#1600).
        //
        // `0x90` and `0x92` are JIS X 0208, 1978 and 1983, mapping to
        // the same cells. The rest of the range the walk takes
        // converts nowhere, the way CP949's and the Big5 family's
        // walks reach past their transcoders.
        0x81..=0x99 => {
            let trail = if lead >= 0x90 { 2 } else { 1 };
            for i in 1..=trail {
                match bytes.get(pos + i) {
                    None => return PreciseLen::NeedMore,
                    Some(0xa0..=0xff) => {}
                    Some(_) => return PreciseLen::Invalid,
                }
            }
            PreciseLen::Char(1 + trail)
        }
        _ => PreciseLen::Invalid,
    }
}

pub(crate) fn euckr_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f => PreciseLen::Char(1),
        0xa1..=0xfe => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0xa1..=0xfe) => PreciseLen::Char(2),
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Classify the EUC-TW sequence starting at `bytes[pos]`.
///
/// [`euckr_precise_len`]'s shape plus the four-byte plane form: `0x8E`,
/// a plane byte `0xA1..=0xB0`, then two bytes from `0xA1..=0xFE`
/// (#1473).
pub(crate) fn euctw_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    if bytes.get(pos) != Some(&0x8e) {
        return euckr_precise_len(bytes, pos);
    }
    for (i, range) in [0xa1..=0xb0u8, 0xa1..=0xfe, 0xa1..=0xfe].into_iter().enumerate() {
        match bytes.get(pos + 1 + i) {
            None => return PreciseLen::NeedMore,
            Some(b) if range.contains(b) => {}
            Some(_) => return PreciseLen::Invalid,
        }
    }
    PreciseLen::Char(4)
}

/// Classify the CP949 (Unified Hangul Code) sequence starting at
/// `bytes[pos]`.
///
/// EUC-KR's superset: `0x80` stands alone, `0x81..=0xFE` leads, and the
/// trail widens to the three ASCII-overlapping runs Microsoft added
/// (#1473).
pub(crate) fn cp949_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x80 => PreciseLen::Char(1),
        0x81..=0xfe => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0x41..=0x5a | 0x61..=0x7a | 0x81..=0xfe) => PreciseLen::Char(2),
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Classify the Big5 sequence starting at `bytes[pos]`.
///
/// `0xA1..=0xFE` leads and takes a `0x40..=0x7E | 0xA1..=0xFE` trail.
/// Big5-HKSCS, Big5-UAO and CP950 extend the *characters*, not the byte
/// structure, so they walk the same way (#1473).
pub(crate) fn big5_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f => PreciseLen::Char(1),
        0xa1..=0xfe => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0x40..=0x7e | 0xa1..=0xfe) => PreciseLen::Char(2),
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Classify the GBK sequence starting at `bytes[pos]`.
///
/// `0x80` stands alone, `0x81..=0xFE` leads, and the trail is
/// `0x40..=0x7E | 0x80..=0xFE` — everything but `0x7F` and the ASCII
/// controls (#1473).
pub(crate) fn gbk_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x80 => PreciseLen::Char(1),
        0x81..=0xfe => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0x40..=0x7e | 0x80..=0xfe) => PreciseLen::Char(2),
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Classify the GB18030 sequence starting at `bytes[pos]`.
///
/// GBK's two-byte form, minus GBK's lone `0x80`, plus the four-byte
/// form: the *second* byte decides, since a digit `0x30..=0x39` cannot
/// be a two-byte trail. The remaining two are `0x81..=0xFE` and another
/// digit (#1473).
pub(crate) fn gb18030_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f => PreciseLen::Char(1),
        0x81..=0xfe => match bytes.get(pos + 1) {
            None => PreciseLen::NeedMore,
            Some(0x40..=0x7e | 0x80..=0xfe) => PreciseLen::Char(2),
            Some(0x30..=0x39) => {
                for (i, range) in [0x81..=0xfeu8, 0x30..=0x39].into_iter().enumerate() {
                    match bytes.get(pos + 2 + i) {
                        None => return PreciseLen::NeedMore,
                        Some(b) if range.contains(b) => {}
                        Some(_) => return PreciseLen::Invalid,
                    }
                }
                PreciseLen::Char(4)
            }
            Some(_) => PreciseLen::Invalid,
        },
        _ => PreciseLen::Invalid,
    }
}

/// Width (in bytes) of the *complete* EUC-JP character starting at
/// `b[0]`, or `None` if none starts there — a well-formed prefix that
/// merely ran out of bytes counts as none.
pub(crate) fn eucjp_char_width(b: &[u8]) -> Option<usize> {
    match eucjp_precise_len(b, 0) {
        PreciseLen::Char(n) => Some(n),
        _ => None,
    }
}

/// Width of the *complete* Shift_JIS / CP932 character starting at
/// `b[0]`, or `None`.
pub(crate) fn sjis_char_width(b: &[u8]) -> Option<usize> {
    match sjis_precise_len(b, 0) {
        PreciseLen::Char(n) => Some(n),
        _ => None,
    }
}

#[monoruby_object]
pub struct RString(Value);

impl RString {
    pub fn bytes(v: Vec<u8>) -> Self {
        Self(Value::bytes(v))
    }
}

/// Iterator yielding one character's worth of bytes per call,
/// honouring the declared encoding. For UTF-8, walks valid UTF-8
/// scalars; broken byte sequences advance one byte at a time so the
/// iterator always terminates. Non-UTF-8 encodings use the
/// fixed-code-unit width (1 for Ascii8/UsAscii/Iso8859, 2 for
/// UTF-16, 4 for UTF-32). The multibyte ASCII-compatible families
/// (EUC-JP, Shift_JIS, Emacs-Mule) go through their own
/// `precise_mbclen`, and a sequence that is not a complete character
/// there advances one byte, as it does in UTF-8.
pub struct CharByteIter<'a> {
    bytes: &'a [u8],
    pos: usize,
    encoding: Encoding,
}

/// The byte width of the character of `encoding` starting at
/// `bytes[pos]` (1 for a byte past the end or a malformed lead), and
/// the number of characters in `bytes` — the two boundary questions the
/// byte-offset pattern walkers (`Subject`) ask of a non-UTF-8 subject.
pub(crate) fn char_width_at(encoding: Encoding, bytes: &[u8], pos: usize) -> usize {
    CharByteIter { bytes, pos, encoding }.next().map_or(1, |c| c.len())
}

pub(crate) fn char_count(encoding: Encoding, bytes: &[u8]) -> usize {
    CharByteIter { bytes, pos: 0, encoding }.count()
}

impl<'a> Iterator for CharByteIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<&'a [u8]> {
        if self.pos >= self.bytes.len() {
            return None;
        }
        let width = match self.encoding {
            // A valid Emacs-Mule sequence is one character; a byte
            // that starts none stands on its own, the way a stray
            // byte does in every other encoding here.
            Encoding::NamedByte(EMACS_MULE) => {
                emacs_mule_char_len(self.bytes, self.pos).unwrap_or(1)
            }
            // EUC-JP / Shift_JIS, likewise: the lead byte alone does
            // not settle the width, because a lead whose continuation
            // is not a continuation leads nothing and stands on its
            // own. Taking the width off the lead swallowed the next
            // byte instead (`"\x8E "` counted 1 character, CRuby 2).
            Encoding::EucJp(_) => match eucjp_precise_len(self.bytes, self.pos) {
                PreciseLen::Char(n) => n,
                _ => 1,
            },
            Encoding::Sjis(_) => match sjis_precise_len(self.bytes, self.pos) {
                PreciseLen::Char(n) => n,
                _ => 1,
            },
            // The CJK double-byte sets, the same way (#1473).
            Encoding::NamedByte(_) if mbc_walker(self.encoding).is_some() => {
                let (_, precise) = mbc_walker(self.encoding).expect("just checked");
                match precise(self.bytes, self.pos) {
                    PreciseLen::Char(n) => n,
                    _ => 1,
                }
            }
            Encoding::Ascii8
            | Encoding::UsAscii
            | Encoding::Iso8859(_)
            | Encoding::Other(_)
            | Encoding::NamedByte(_)
            // ISO-2022-JP groups bytes 1-at-a-time at the codepoint
            // iterator level — the actual char-vs-ESC-sequence
            // chunking happens further up via `encoding_rs`. This
            // matches the behaviour of `String#bytes.length` ==
            // `String#bytesize` for stateful encodings.
            | Encoding::Iso2022Jp => 1,
            // A well-formed surrogate pair is one character, four
            // bytes wide; a lone surrogate (or a trailing odd byte)
            // stands on its own, as CRuby's UTF-16 walker has it.
            Encoding::Utf16Le | Encoding::Utf16Be => {
                let rest = self.bytes.len() - self.pos;
                let unit = |i: usize| {
                    let (a, b) = (self.bytes[i], self.bytes[i + 1]);
                    if self.encoding == Encoding::Utf16Be {
                        ((a as u32) << 8) | b as u32
                    } else {
                        ((b as u32) << 8) | a as u32
                    }
                };
                if rest >= 4
                    && (0xD800..0xDC00).contains(&unit(self.pos))
                    && (0xDC00..0xE000).contains(&unit(self.pos + 2))
                {
                    4
                } else {
                    2
                }
            }
            Encoding::Utf32Le | Encoding::Utf32Be => 4,
            Encoding::Utf8(_) => {
                let b = self.bytes[self.pos];
                if b < 0x80 {
                    1
                } else if b < 0xC0 {
                    1 // continuation byte at scalar boundary => broken; consume one
                } else {
                    let needed = if b < 0xE0 {
                        2
                    } else if b < 0xF0 {
                        3
                    } else {
                        4
                    };
                    let end = (self.pos + needed).min(self.bytes.len());
                    // Validate the candidate scalar; on failure, fall
                    // back to one byte (CRuby's "adds 1 for every
                    // invalid byte in UTF-8" rule).
                    if end - self.pos == needed
                        && std::str::from_utf8(&self.bytes[self.pos..end]).is_ok()
                    {
                        needed
                    } else {
                        1
                    }
                }
            }
        };
        let end = (self.pos + width).min(self.bytes.len());
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Some(slice)
    }
}

/// Cached classification of an `RStringInner`'s byte content relative
/// to its declared `encoding`. CRuby's `enum ruby_coderange_type`
/// equivalent — keeps `valid_encoding?` / `ascii_only?` /
/// compatibility checks O(1) after the first walk.
/// `repr(u8)` with fixed discriminants because the JIT's inline
/// `String#setbyte` pokes the cached classification directly (see
/// `STRING_CR_OFFSET` / `emit_string_setbyte`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CodeRange {
    /// Not yet computed.
    Unknown = 0,
    /// Every byte is < 0x80; safe under any ASCII-compatible encoding.
    SevenBit = 1,
    /// Encoding-valid (and contains at least one non-ASCII codepoint).
    Valid = 2,
    /// Contains a byte sequence invalid in the declared encoding.
    Broken = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Encoding {
    /// Binary / ASCII-8BIT.
    Ascii8,
    /// UTF-8, and the encodings whose bytes are UTF-8 but whose
    /// *content* is normalised differently — currently `UTF8-MAC`,
    /// Apple's HFS+ decomposed form.
    ///
    /// The payload indexes [`UTF8_VARIANTS`]. Storage, iteration and
    /// validity are UTF-8's for every variant; only the name and the
    /// normalisation a conversion applies differ (#1562).
    Utf8(u8),
    /// US-ASCII (a 7-bit subset of UTF-8 for storage purposes).
    UsAscii,
    /// UTF-16 little-endian.
    Utf16Le,
    /// UTF-16 big-endian.
    Utf16Be,
    /// UTF-32 little-endian.
    Utf32Le,
    /// UTF-32 big-endian.
    Utf32Be,
    /// ISO-8859-N for N in 1..=16 (excluding 12). One byte per char,
    /// every byte is "valid" in the encoding.
    Iso8859(u8),
    /// EUC-JP and the encodings that share its byte structure exactly
    /// and differ only in vendor mapping tables monoruby does not
    /// carry: `eucJP-ms`, `CP51932`, `stateless-ISO-2022-JP`,
    /// `EUC-JIS-2004` and `stateless-ISO-2022-JP-KDDI`. ASCII-compatible
    /// multibyte.
    ///
    /// The payload indexes [`EUC_JP_VARIANTS`] and decides the name
    /// only — as `Sjis`'s does for Windows-31J and MacJapanese. Without
    /// it these five collapsed onto EUC-JP, so `force_encoding` handed
    /// back an encoding the caller had not asked for while
    /// `Encoding.find` answered correctly (#1562).
    EucJp(u8),
    /// Shift_JIS / Windows-31J / CP932 (we treat these as one
    /// implementation, but the canonical name is preserved). The
    /// `u8` distinguishes the alias for `name()`/`==`.
    Sjis(u8),
    /// ISO-2022-JP. Stateful 7-bit encoding using ESC sequences
    /// (`ESC ( B`, `ESC $ B`, `ESC ( J`) to switch between
    /// ASCII / JIS X 0208 / JIS X 0201 character sets. Not
    /// ASCII-compatible — a literal `0x42` byte may decode as
    /// either `'B'` or part of a JIS X 0208 codepoint depending
    /// on the surrounding ESC state.
    Iso2022Jp,
    /// A byte-oriented encoding monoruby has no native codec for but
    /// must still name-preserve and treat as ASCII-incompatible
    /// (CRuby's stateful / dummy encodings: UTF-7, CP50220/1,
    /// ISO-2022-JP-2/KDDI, the BOM-form UTF-16/UTF-32, IBM037, …).
    /// The payload indexes [`OTHER_ENC_NAMES`] (kept `u8` so the enum
    /// stays small — `RValue` must remain exactly 64 bytes).
    /// Transcoding and per-char iteration behave like ASCII-8BIT (raw
    /// bytes); only `#name`, `#inspect` and ASCII-incompatibility
    /// differ.
    Other(u8),
    /// An ASCII-*compatible* byte-oriented national encoding monoruby
    /// has no native codec for (Big5, GBK, GB18030, EUC-KR, the
    /// Windows-125x / IBM / KOI8 / TIS-620 family, …). Unlike
    /// [`Encoding::Other`] these keep bytes < 0x80 as ASCII, so they
    /// behave like `ASCII-8BIT` for storage/iteration but preserve
    /// their declared name (so `# encoding: big5` makes
    /// `__ENCODING__.name == "Big5"`). The payload indexes
    /// [`NAMED_BYTE_ENCODINGS`].
    NamedByte(u8),
}

/// Canonical names for [`Encoding::Other`] variants (stateful /
/// dummy byte encodings monoruby has no native codec for). The
/// index is the `Encoding::Other` payload.
/// `(display name, `Encoding::<CONST>` suffix)` for the [`Encoding::EucJp`]
/// family. Index 0 is canonical EUC-JP; the rest ride its character walk
/// and differ only in the name they report.
/// `(display name, `Encoding::<CONST>` suffix)` for the
/// [`Encoding::Utf8`] family. Index 0 is canonical UTF-8; the rest hold
/// the same bytes under a different normalisation, so they share every
/// UTF-8 path and differ only in name and in what a conversion does.
pub(crate) const UTF8_VARIANTS: &[(&str, &str)] = &[
    ("UTF-8", "UTF_8"),
    // Apple's HFS+ form: canonically decomposed, minus the codepoints
    // it leaves alone (see `UTF8_MAC_NO_DECOMPOSE`).
    ("UTF8-MAC", "UTF8_MAC"),
    // The carrier sets: UTF-8's bytes, with a block of characters
    // spelled as one Japanese carrier's private-use emoji (#1573).
    ("UTF8-DoCoMo", "UTF8_DOCOMO"),
    ("UTF8-KDDI", "UTF8_KDDI"),
    ("UTF8-SoftBank", "UTF8_SOFTBANK"),
];

/// The three carrier members of [`UTF8_VARIANTS`].
pub(crate) const UTF8_DOCOMO: u8 = utf8_variant_index("UTF8_DOCOMO");
pub(crate) const UTF8_KDDI: u8 = utf8_variant_index("UTF8_KDDI");
pub(crate) const UTF8_SOFTBANK: u8 = utf8_variant_index("UTF8_SOFTBANK");

/// Index of `UTF8-MAC` in [`UTF8_VARIANTS`].
pub(crate) const UTF8_MAC: u8 = utf8_variant_index("UTF8_MAC");

/// Look up a [`UTF8_VARIANTS`] index by its constant suffix.
pub(crate) const fn utf8_variant_index(konst: &str) -> u8 {
    let mut i = 0;
    while i < UTF8_VARIANTS.len() {
        if const_str_eq(UTF8_VARIANTS[i].1.as_bytes(), konst.as_bytes()) {
            return i as u8;
        }
        i += 1;
    }
    panic!("UTF8_VARIANTS has no such constant suffix")
}

/// The `Encoding::<CONST>` suffix for an [`Encoding::Utf8`] payload.
pub(crate) fn utf8_const_name(index: u8) -> &'static str {
    UTF8_VARIANTS[index as usize].1
}

/// The codepoints `UTF8-MAC` leaves alone where NFD would decompose
/// them.
///
/// Apple's HFS+ form is canonical decomposition with a fixed set of
/// exceptions — the composition exclusions, the singleton mappings
/// (`2126` OHM, `212A` KELVIN, `212B` ANGSTROM, `2329` / `232A`), and
/// the CJK compatibility ideographs. Enumerating CRuby over all
/// 0x110000 codepoints says the two agree exactly otherwise: nothing
/// decomposes differently, and nothing decomposes here that NFD leaves
/// alone. So the whole of the difference is these 1128 codepoints,
/// derived from CRuby rather than transcribed (#1562).
///
/// Sorted, so a lookup is a binary search.
const UTF8_MAC_NO_DECOMPOSE: &[(u32, u32)] = &[
    (0x0390, 0x0390),
    (0x03B0, 0x03B0),
    (0x1B06, 0x1B06),
    (0x1B08, 0x1B08),
    (0x1B0A, 0x1B0A),
    (0x1B0C, 0x1B0C),
    (0x1B0E, 0x1B0E),
    (0x1B12, 0x1B12),
    (0x1B3B, 0x1B3B),
    (0x1B3D, 0x1B3D),
    (0x1B40, 0x1B41),
    (0x1B43, 0x1B43),
    (0x1F71, 0x1F71),
    (0x1F73, 0x1F73),
    (0x1F75, 0x1F75),
    (0x1F77, 0x1F77),
    (0x1F79, 0x1F79),
    (0x1F7B, 0x1F7B),
    (0x1F7D, 0x1F7D),
    (0x1FBB, 0x1FBB),
    (0x1FC9, 0x1FC9),
    (0x1FCB, 0x1FCB),
    (0x1FDB, 0x1FDB),
    (0x1FEB, 0x1FEB),
    (0x1FEE, 0x1FEE),
    (0x1FF9, 0x1FF9),
    (0x1FFB, 0x1FFB),
    (0x2000, 0x2001),
    (0x2126, 0x2126),
    (0x212A, 0x212B),
    (0x219A, 0x219B),
    (0x21AE, 0x21AE),
    (0x21CD, 0x21CF),
    (0x2204, 0x2204),
    (0x2209, 0x2209),
    (0x220C, 0x220C),
    (0x2224, 0x2224),
    (0x2226, 0x2226),
    (0x2241, 0x2241),
    (0x2244, 0x2244),
    (0x2247, 0x2247),
    (0x2249, 0x2249),
    (0x2260, 0x2260),
    (0x2262, 0x2262),
    (0x226D, 0x2271),
    (0x2274, 0x2275),
    (0x2278, 0x2279),
    (0x2280, 0x2281),
    (0x2284, 0x2285),
    (0x2288, 0x2289),
    (0x22AC, 0x22AF),
    (0x22E0, 0x22E3),
    (0x22EA, 0x22ED),
    (0x2329, 0x232A),
    (0x2ADC, 0x2ADC),
    (0xF900, 0xFA0D),
    (0xFA10, 0xFA10),
    (0xFA12, 0xFA12),
    (0xFA15, 0xFA1E),
    (0xFA20, 0xFA20),
    (0xFA22, 0xFA22),
    (0xFA25, 0xFA26),
    (0xFA2A, 0xFA6D),
    (0xFA70, 0xFAD9),
    (0x105C9, 0x105C9),
    (0x105E4, 0x105E4),
    (0x1109A, 0x1109A),
    (0x1109C, 0x1109C),
    (0x110AB, 0x110AB),
    (0x1112E, 0x1112F),
    (0x1134B, 0x1134C),
    (0x11383, 0x11383),
    (0x11385, 0x11385),
    (0x1138E, 0x1138E),
    (0x11391, 0x11391),
    (0x113C5, 0x113C5),
    (0x113C7, 0x113C8),
    (0x114BB, 0x114BC),
    (0x114BE, 0x114BE),
    (0x115BA, 0x115BB),
    (0x11938, 0x11938),
    (0x16121, 0x16128),
    (0x16D68, 0x16D6A),
    (0x1D15E, 0x1D164),
    (0x1D1BB, 0x1D1C0),
    (0x2F800, 0x2FA1D),
];

/// Whether `c` is one of the codepoints [`UTF8_MAC_NO_DECOMPOSE`] names.
fn utf8_mac_keeps_composed(c: char) -> bool {
    let cp = c as u32;
    UTF8_MAC_NO_DECOMPOSE
        .binary_search_by(|&(lo, hi)| {
            if cp < lo {
                std::cmp::Ordering::Greater
            } else if cp > hi {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        })
        .is_ok()
}

/// Put a decomposed sequence into canonical order: within each run of
/// combining marks, sort by canonical combining class, stably.
///
/// UAX #15's canonical ordering algorithm. `unicode-normalization` does
/// this inside `nfd()`, which is no use here because the decomposition
/// has to skip [`UTF8_MAC_NO_DECOMPOSE`] first.
fn canonical_order(chars: &mut [char]) {
    use unicode_normalization::char::canonical_combining_class as ccc;
    let mut i = 0;
    while i < chars.len() {
        if ccc(chars[i]) == 0 {
            i += 1;
            continue;
        }
        let start = i;
        while i < chars.len() && ccc(chars[i]) != 0 {
            i += 1;
        }
        chars[start..i].sort_by_key(|&c| ccc(c));
    }
}

/// `UTF-8` -> `UTF8-MAC`: canonical decomposition, skipping the
/// codepoints Apple's form keeps composed.
pub(crate) fn utf8_to_mac(s: &str) -> String {
    use unicode_normalization::char::decompose_canonical;
    let mut out: Vec<char> = Vec::with_capacity(s.len());
    for c in s.chars() {
        if utf8_mac_keeps_composed(c) {
            out.push(c);
        } else {
            decompose_canonical(c, |d| out.push(d));
        }
    }
    canonical_order(&mut out);
    out.into_iter().collect()
}

/// The byte ranges of `s`'s canonical-composition clusters: each is a
/// starter (canonical combining class 0) together with the marks that
/// follow it.
///
/// Neither the canonical reordering nor the composition below reaches
/// across a starter, so a cluster converts to and from `UTF8-MAC` on
/// its own and the pieces concatenate. That is what lets a streamed
/// conversion hold the trailing cluster back until it knows whether
/// the next chunk opens with a mark that composes onto it (#1576).
pub(crate) fn mac_clusters(s: &str) -> Vec<std::ops::Range<usize>> {
    use unicode_normalization::char::canonical_combining_class as ccc;
    let mut out: Vec<std::ops::Range<usize>> = vec![];
    for (at, c) in s.char_indices() {
        let end = at + c.len_utf8();
        match out.last_mut() {
            Some(last) if ccc(c) != 0 => last.end = end,
            _ => out.push(at..end),
        }
    }
    out
}

/// `UTF8-MAC` -> `UTF-8`: the same restricted decomposition, then
/// canonical composition.
///
/// Not `nfc()`: that would decompose `212B` ANGSTROM to `0041 030A` and
/// compose that to `00C5`, where CRuby hands back the `212B` it was
/// given. Composing *after* the restricted decomposition is what keeps
/// the two apart.
pub(crate) fn mac_to_utf8(s: &str) -> String {
    use unicode_normalization::char::{canonical_combining_class as ccc, compose};
    let decomposed: Vec<char> = utf8_to_mac(s).chars().collect();
    let mut out: Vec<char> = Vec::with_capacity(decomposed.len());
    // UAX #15's canonical composition: each character composes onto the
    // last starter unless something blocks it, and a character is
    // blocked when the mark before it has a class at least its own.
    let mut starter: Option<usize> = None;
    let mut last_class: Option<u8> = None;
    for c in decomposed {
        let c_class = ccc(c);
        if let Some(si) = starter
            && last_class.is_none_or(|prev| prev < c_class)
            && let Some(composed) = compose(out[si], c)
        {
            out[si] = composed;
            continue;
        }
        if c_class == 0 {
            starter = Some(out.len());
            last_class = None;
        } else {
            last_class = Some(c_class);
        }
        out.push(c);
    }
    out.into_iter().collect()
}


/// Where a stateless-ISO-2022-JP sequence ends according to the
/// *transcoder*, which is narrower than the encoding object's walk:
/// only `0x90` and `0x92` name a set it can convert, and their cells
/// are `0xA1..=0xFE` rather than `0xA0..=0xFF`. The two questions are
/// separate, as they are for CP949 and the Big5 family, and this is
/// the one that decides a malformed run (#1600).
pub(crate) fn stateless_iso2022jp_transcode_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    match lead {
        0x00..=0x7f => PreciseLen::Char(1),
        0x90 | 0x92 => {
            for i in 1..=2 {
                match bytes.get(pos + i) {
                    None => return PreciseLen::NeedMore,
                    Some(0xa1..=0xfe) => {}
                    Some(_) => return PreciseLen::Invalid,
                }
            }
            PreciseLen::Char(3)
        }
        _ => PreciseLen::Invalid,
    }
}

/// What stopped an ISO-2022-JP parse: where, the bytes CRuby reports
/// as the malformed run, the ones it reads again, and whether the
/// input simply ended in the middle.
pub(crate) struct Iso2022JpStop {
    pub at: usize,
    pub error: Vec<u8>,
    pub again: Vec<u8>,
    pub incomplete: bool,
}

/// The stateless-ISO-2022-JP bytes the ISO-2022-JP `bytes` stand for.
///
/// The two are the same repertoire written two ways: ISO-2022-JP
/// names the character set with an escape sequence that stays in
/// effect, stateless names it with a lead byte on every character.
/// `ESC $ @` and `ESC $ B` are JIS X 0208 1978 and 1983, which are
/// stateless's `0x90` and `0x92`; `ESC ( B` and `ESC ( J` are ASCII
/// and JIS X 0201 Roman, and CRuby reads both as plain ASCII bytes
/// (#1609).
pub(crate) fn iso2022jp_to_stateless(
    bytes: &[u8],
) -> std::result::Result<Vec<u8>, Iso2022JpStop> {
    iso2022jp_to_stateless_from(bytes, None).map(|(out, _)| out)
}

/// The same, starting in the designation `start` left in effect and
/// reporting the one this chunk leaves — which is what a converter
/// needs, the escape staying in effect across `#convert` calls.
pub(crate) fn iso2022jp_to_stateless_from(
    bytes: &[u8],
    start: Option<u8>,
) -> std::result::Result<(Vec<u8>, Option<u8>), Iso2022JpStop> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut lead: Option<u8> = start;
    let mut pos = 0;
    while pos < bytes.len() {
        if bytes[pos] == 0x1b {
            let rest = &bytes[pos..];
            match (rest.get(1), rest.get(2)) {
                (Some(b'('), Some(b'B' | b'J')) => lead = None,
                (Some(b'$'), Some(b'@')) => lead = Some(0x90),
                (Some(b'$'), Some(b'B')) => lead = Some(0x92),
                // An escape the encoding does not have: the prefix is
                // the run and the byte that disproved it is read again.
                (Some(&b1), Some(&b2)) => {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: vec![0x1b, b1],
                        again: vec![b2],
                        incomplete: false,
                    });
                }
                _ => {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: rest.to_vec(),
                        again: vec![],
                        incomplete: true,
                    });
                }
            }
            pos += 3;
            continue;
        }
        match lead {
            None => {
                if bytes[pos] >= 0x80 {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: vec![bytes[pos]],
                        again: vec![],
                        incomplete: false,
                    });
                }
                out.push(bytes[pos]);
                pos += 1;
            }
            Some(l) => {
                let b1 = bytes[pos];
                if !(0x21..=0x7e).contains(&b1) {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: vec![b1],
                        again: vec![],
                        incomplete: false,
                    });
                }
                let Some(&b2) = bytes.get(pos + 1) else {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: vec![b1],
                        again: vec![],
                        incomplete: true,
                    });
                };
                if !(0x21..=0x7e).contains(&b2) {
                    return Err(Iso2022JpStop {
                        at: pos,
                        error: vec![b1],
                        again: vec![b2],
                        incomplete: false,
                    });
                }
                out.extend_from_slice(&[l, b1 + 0x80, b2 + 0x80]);
                pos += 2;
            }
        }
    }
    Ok((out, lead))
}

/// The ISO-2022-JP bytes for the stateless-ISO-2022-JP `bytes`, or
/// `Err(offset)` at the first sequence ISO-2022-JP cannot hold — the
/// single-byte sets `0x81..=0x8F` name and the two-byte ones other
/// than JIS X 0208.
pub(crate) fn stateless_to_iso2022jp(bytes: &[u8]) -> std::result::Result<Vec<u8>, usize> {
    stateless_to_iso2022jp_from(bytes, None, true).map(|(out, _)| out)
}

/// The same, starting in the designation `start` and closing back to
/// ASCII only when `close` — a converter emits that last escape from
/// `#finish`, not from every `#convert` (#1609).
pub(crate) fn stateless_to_iso2022jp_from(
    bytes: &[u8],
    start: Option<u8>,
    close: bool,
) -> std::result::Result<(Vec<u8>, Option<u8>), usize> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut lead: Option<u8> = start;
    let mut pos = 0;
    while pos < bytes.len() {
        match bytes[pos] {
            b @ 0x00..=0x7f => {
                if lead.take().is_some() {
                    out.extend_from_slice(b"\x1b(B");
                }
                out.push(b);
                pos += 1;
            }
            l @ (0x90 | 0x92)
                if matches!(bytes.get(pos + 1), Some(0xa1..=0xfe))
                    && matches!(bytes.get(pos + 2), Some(0xa1..=0xfe)) =>
            {
                if lead != Some(l) {
                    out.extend_from_slice(if l == 0x90 { b"\x1b$@" } else { b"\x1b$B" });
                    lead = Some(l);
                }
                out.push(bytes[pos + 1] - 0x80);
                out.push(bytes[pos + 2] - 0x80);
                pos += 3;
            }
            _ => return Err(pos),
        }
    }
    // A run of cells is closed off, so the bytes end in ASCII.
    if close && lead.take().is_some() {
        out.extend_from_slice(b"\x1b(B");
    }
    Ok((out, lead))
}

/// The EUC-JP bytes the stateless-ISO-2022-JP `bytes` stand for, or
/// `Err(offset)` at the first sequence that is not stateless's.
///
/// stateless-ISO-2022-JP is ISO-2022-JP with the character set named
/// by a lead byte instead of by an escape sequence still in effect,
/// and the cell after it written exactly as EUC-JP writes it. So the
/// conversion out of it is this rewrite followed by EUC-JP's own — the
/// chain CRuby names in its errors, `stateless-ISO-2022-JP to EUC-JP
/// to UTF-8` (#1600).
///
/// `0x90` and `0x92` are JIS X 0208, 1978 and 1983; CRuby maps them to
/// the same cells. The rest of the `0x90..=0x99` range the walk
/// accepts converts nowhere, so it is malformed *here* even though
/// `valid_encoding?` is true for it — the same split the Big5 family
/// and CP949 have.
pub(crate) fn stateless_iso2022jp_to_eucjp(bytes: &[u8]) -> std::result::Result<Vec<u8>, usize> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut pos = 0;
    while pos < bytes.len() {
        match bytes[pos] {
            b @ 0x00..=0x7f => {
                out.push(b);
                pos += 1;
            }
            0x90 | 0x92
                if matches!(bytes.get(pos + 1), Some(0xa1..=0xfe))
                    && matches!(bytes.get(pos + 2), Some(0xa1..=0xfe)) =>
            {
                out.extend_from_slice(&bytes[pos + 1..pos + 3]);
                pos += 3;
            }
            _ => return Err(pos),
        }
    }
    Ok(out)
}

/// The stateless-ISO-2022-JP bytes for the EUC-JP `bytes`, or
/// `Err(offset)` at the first character stateless has no cell for.
///
/// Only JIS X 0208 and ASCII are in it: EUC-JP's half-width katakana
/// (`0x8E` + one byte) and JIS X 0212 (`0x8F` + a cell) have nowhere
/// to go, which is what makes them an undefined conversion — the same
/// two EUC-JP forms ISO-2022-JP itself cannot spell (#1600).
pub(crate) fn eucjp_to_stateless_iso2022jp(bytes: &[u8]) -> std::result::Result<Vec<u8>, usize> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut pos = 0;
    while pos < bytes.len() {
        match bytes[pos] {
            b @ 0x00..=0x7f => {
                out.push(b);
                pos += 1;
            }
            0xa1..=0xfe if matches!(bytes.get(pos + 1), Some(0xa1..=0xfe)) => {
                out.push(0x92);
                out.extend_from_slice(&bytes[pos..pos + 2]);
                pos += 2;
            }
            _ => return Err(pos),
        }
    }
    Ok(out)
}

/// CESU-8's bytes for `s`.
///
/// Below `U+10000` the bytes are UTF-8's; above it the character is its
/// UTF-16 surrogate pair, and each half is written as if it were an
/// ordinary three-byte character. So a conversion into CESU-8 is UTF-8
/// plus this rewrite, exactly as one into `UTF8-MAC` is UTF-8 plus a
/// normalisation (#1562).
pub(crate) fn utf8_to_cesu8(s: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len());
    let mut buf = [0u8; 4];
    for c in s.chars() {
        let cp = c as u32;
        if cp < 0x10000 {
            out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        } else {
            let c = cp - 0x10000;
            for half in [0xd800 + (c >> 10), 0xdc00 + (c & 0x3ff)] {
                out.push(0xe0 | (half >> 12) as u8);
                out.push(0x80 | ((half >> 6) & 0x3f) as u8);
                out.push(0x80 | (half & 0x3f) as u8);
            }
        }
    }
    out
}

/// The UTF-8 bytes of the CESU-8 `bytes`, or `None` if they are not
/// well-formed CESU-8 (the caller reports the offending sequence).
pub(crate) fn cesu8_to_utf8(bytes: &[u8]) -> Option<String> {
    let mut out = String::with_capacity(bytes.len());
    let mut pos = 0;
    while pos < bytes.len() {
        let PreciseLen::Char(n) = cesu8_precise_len(bytes, pos) else {
            return None;
        };
        if n == 6 {
            let half = |i: usize| -> u32 {
                (((bytes[i] & 0x0f) as u32) << 12)
                    | (((bytes[i + 1] & 0x3f) as u32) << 6)
                    | ((bytes[i + 2] & 0x3f) as u32)
            };
            let cp = 0x10000 + ((half(pos) - 0xd800) << 10) + (half(pos + 3) - 0xdc00);
            // The walk proved both halves are surrogates in range, so
            // the pair names a character of the supplementary planes.
            out.push(char::from_u32(cp)?);
        } else {
            // Below `U+10000` and not a surrogate: the same bytes are
            // this character's UTF-8, so they go out as they came in.
            out.push_str(std::str::from_utf8(&bytes[pos..pos + n]).ok()?);
        }
        pos += n;
    }
    Some(out)
}


pub(crate) const SJIS_VARIANTS: &[(&str, &str)] = &[
    ("Shift_JIS", "SHIFT_JIS"),
    ("Windows-31J", "WINDOWS_31J"),
    // Rides the Shift_JIS walk and has no converter of its own in
    // CRuby either, so only its name is kept apart (#1471).
    ("MacJapanese", "MACJAPANESE"),
    // The carrier sets: Windows-31J's cells, with a block of them
    // reading as one Japanese carrier's emoji (#1573).
    ("SJIS-DoCoMo", "SJIS_DOCOMO"),
    ("SJIS-KDDI", "SJIS_KDDI"),
    ("SJIS-SoftBank", "SJIS_SOFTBANK"),
];

/// Index of `Windows-31J` in [`SJIS_VARIANTS`] — the member whose
/// cells the carriers are a difference from.
pub(crate) const WINDOWS_31J: u8 = sjis_variant_index("WINDOWS_31J");
/// Index of `MacJapanese`, which has no converter at all.
pub(crate) const MACJAPANESE: u8 = sjis_variant_index("MACJAPANESE");
/// The three carrier members, in [`SJIS_VARIANTS`] order.
pub(crate) const SJIS_DOCOMO: u8 = sjis_variant_index("SJIS_DOCOMO");
pub(crate) const SJIS_KDDI: u8 = sjis_variant_index("SJIS_KDDI");
pub(crate) const SJIS_SOFTBANK: u8 = sjis_variant_index("SJIS_SOFTBANK");

/// Look up a [`SJIS_VARIANTS`] index by its constant suffix.
pub(crate) const fn sjis_variant_index(konst: &str) -> u8 {
    let mut i = 0;
    while i < SJIS_VARIANTS.len() {
        if const_str_eq(SJIS_VARIANTS[i].1.as_bytes(), konst.as_bytes()) {
            return i as u8;
        }
        i += 1;
    }
    panic!("SJIS_VARIANTS has no such constant suffix")
}

pub(crate) const EUC_JP_VARIANTS: &[(&str, &str)] = &[
    ("EUC-JP", "EUC_JP"),
    ("eucJP-ms", "EUCJP_MS"),
    ("CP51932", "CP51932"),
    ("EUC-JIS-2004", "EUC_JIS_2004"),
];

/// Look up an [`EUC_JP_VARIANTS`] index by its constant suffix.
pub(crate) const fn euc_jp_variant_index(konst: &str) -> u8 {
    let mut i = 0;
    while i < EUC_JP_VARIANTS.len() {
        if const_str_eq(EUC_JP_VARIANTS[i].1.as_bytes(), konst.as_bytes()) {
            return i as u8;
        }
        i += 1;
    }
    panic!("EUC_JP_VARIANTS has no such constant suffix")
}

const fn const_str_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}

pub(crate) const OTHER_ENC_NAMES: &[&str] = &[
    "UTF-7",
    "CP50220",
    "CP50221",
    "UTF-16",
    "UTF-32",
    // EBCDIC, and the two stateful ISO-2022-JP variants. All three are
    // dummy in CRuby, so raw bytes with a preserved name is the whole
    // of what they need (#1555).
    "IBM037",
    "ISO-2022-JP-2",
    "ISO-2022-JP-KDDI",
];

/// `(display name, `Encoding::<CONST>` suffix)` for
/// [`Encoding::NamedByte`] variants — ASCII-compatible byte
/// encodings monoruby name-preserves but has no native codec for.
/// The index is the `Encoding::NamedByte` payload. The constant
/// suffix must match a registered `Encoding::*` constant so
/// `__ENCODING__` can load it.
pub(crate) const NAMED_BYTE_ENCODINGS: &[(&str, &str)] = &[
    ("Big5", "Big5"),
    ("Big5-HKSCS", "Big5_HKSCS"),
    ("Big5-UAO", "Big5_UAO"),
    ("GBK", "GBK"),
    ("GB2312", "GB2312"),
    ("GB18030", "GB18030"),
    ("GB12345", "GB12345"),
    ("EUC-KR", "EUC_KR"),
    ("EUC-TW", "EUC_TW"),
    ("CP949", "CP949"),
    ("TIS-620", "TIS_620"),
    // Single-byte national sets with no codec, like the rest of this
    // table: the Arabic and Thai DOS pages, and the ISO-646 Chinese
    // variant (#1555).
    ("IBM720", "IBM720"),
    ("Windows-874", "Windows_874"),
    ("GB1988", "GB1988"),
    // Big5 variants: same byte structure as their base (checked over
    // every one- and two-byte sequence), so they ride its walk and
    // keep their own names.
    ("CP950", "CP950"),
    ("CP951", "CP951"),
    // ISO-2022-JP's repertoire without the escapes. CRuby names them
    // with the EUC-JP family, but their byte structure is their own
    // (#1562).
    ("stateless-ISO-2022-JP", "STATELESS_ISO_2022_JP"),
    ("stateless-ISO-2022-JP-KDDI", "STATELESS_ISO_2022_JP_KDDI"),
    ("KOI8-R", "KOI8_R"),
    ("KOI8-U", "KOI8_U"),
    ("Windows-1250", "Windows_1250"),
    ("Windows-1251", "Windows_1251"),
    ("Windows-1252", "Windows_1252"),
    ("Windows-1253", "Windows_1253"),
    ("Windows-1254", "Windows_1254"),
    ("Windows-1255", "Windows_1255"),
    ("Windows-1256", "Windows_1256"),
    ("Windows-1257", "Windows_1257"),
    ("Windows-1258", "Windows_1258"),
    ("IBM437", "IBM437"),
    ("IBM737", "IBM737"),
    ("IBM775", "IBM775"),
    // CRuby's canonical name for code page 850 is `CP850`; the
    // constant stays `Encoding::IBM850` (with `Encoding::CP850` beside
    // it) as it does for the rest of the family (#1520).
    ("CP850", "IBM850"),
    ("IBM852", "IBM852"),
    ("IBM855", "IBM855"),
    // CP852 / CP855 are *separate* encodings from IBM852 / IBM855 in
    // CRuby, each with a single name — not the two-name pairs the rest
    // of the family forms. They were an alias of the IBM ones here,
    // which is a different encoding to answer with (#1555).
    ("CP852", "CP852"),
    ("CP855", "CP855"),
    ("IBM857", "IBM857"),
    ("IBM860", "IBM860"),
    ("IBM861", "IBM861"),
    ("IBM862", "IBM862"),
    ("IBM863", "IBM863"),
    ("IBM864", "IBM864"),
    ("IBM865", "IBM865"),
    ("IBM866", "IBM866"),
    ("IBM869", "IBM869"),
    // Not a national codepage, but the same shape: ASCII-compatible,
    // name-preserved, no codec (so `Encoding::Converter.new` refuses
    // it while 7-bit content still converts).
    ("Emacs-Mule", "Emacs_Mule"),
    // The Mac OS script encodings. Eight of them have a table of their
    // own ([`single_byte_table`](crate::builtins::encoding)); CRuby has
    // no converter at all for `macCentEuro` and `macThai`, so those two
    // are name-only, which is what this variant is for. CRuby spells
    // the family with a lowercase `mac` — `MacJapanese` is the one
    // exception, and it is a Shift_JIS variant rather than one of these.
    ("macRoman", "MacRoman"),
    ("macCyrillic", "MacCyrillic"),
    ("macCentEuro", "MacCentEuro"),
    ("macCroatian", "MacCroatian"),
    ("macGreek", "MacGreek"),
    ("macIceland", "MacIceland"),
    ("macRomania", "MacRomania"),
    ("macThai", "MacThai"),
    ("macTurkish", "MacTurkish"),
    ("macUkraine", "MacUkraine"),
    // Not a national byte encoding at all: CESU-8 is here because what
    // it needs from this table is a name of its own and a byte walk,
    // and those are what the table carries. Its codec is a wrapper
    // around UTF-8's, in `transcode_bytes_with_opts` (#1562).
    ("CESU-8", "CESU_8"),
];

/// Index of `Emacs-Mule` in [`NAMED_BYTE_ENCODINGS`]. It is the one
/// entry monoruby validates rather than passing through as raw bytes,
/// so `classify` and the character walk single it out by index rather
/// than by name.
///
/// Found at compile time: written out as a number, inserting an entry
/// anywhere above it in the table silently re-pointed it at whatever
/// moved into the slot (#1555).
pub(crate) const EMACS_MULE: u8 = named_byte_index_const("Emacs_Mule");

/// Index of `CESU-8` in [`NAMED_BYTE_ENCODINGS`], found the same way
/// and for the same reason.
pub(crate) const CESU_8: u8 = named_byte_index_const("CESU_8");

/// [`named_byte_index`] for a `const` context. Panics — at compile
/// time — on a constant suffix the table does not carry.
const fn named_byte_index_const(konst: &str) -> u8 {
    let mut i = 0;
    while i < NAMED_BYTE_ENCODINGS.len() {
        if const_str_eq(NAMED_BYTE_ENCODINGS[i].1.as_bytes(), konst.as_bytes()) {
            return i as u8;
        }
        i += 1;
    }
    panic!("NAMED_BYTE_ENCODINGS has no such constant suffix")
}

/// What the multibyte sequence at a given offset is — CRuby's
/// `rb_enc_precise_mbclen` three-way answer. Shared by every encoding
/// monoruby walks itself (Emacs-Mule, EUC-JP, Shift_JIS), because the
/// three answers are what `#length`, `#chars` and `#scrub` each need to
/// tell apart: a width to advance by, a tail that is still only a
/// prefix, and a byte no character can start at.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum PreciseLen {
    /// A complete character, this many bytes wide.
    Char(usize),
    /// A well-formed prefix: it would still become a character with
    /// more bytes after it.
    NeedMore,
    /// No character can start here.
    Invalid,
}

/// The widest an Emacs-Mule character gets (CRuby's `mbmaxlen`).
const EMACS_MULE_MAX_LEN: usize = 4;

/// The widest a CESU-8 character gets: the six bytes of a surrogate
/// pair.
pub(crate) const CESU8_MAX_LEN: usize = 6;

/// Classify the CESU-8 sequence starting at `bytes[pos]`.
///
/// CESU-8 is UTF-8 with the astral plane spelled the way UTF-16 does:
/// a character above `U+FFFF` is its surrogate pair, and each half of
/// that pair is encoded as if it were an ordinary three-byte
/// character. So the two differences from UTF-8 are that a four-byte
/// sequence is invalid (`F0..FF` starts nothing) and that
/// `ED A0..AF xx` is not a character on its own but the first half of
/// a six-byte one, which must be followed by `ED B0..BF xx`.
///
/// A lone surrogate of either half is therefore invalid, where plain
/// UTF-8 rejects both halves outright and CESU-8 rejects only the
/// unpaired ones. Read off CRuby's own validator over every one- and
/// two-byte sequence and every shape of the six-byte form.
pub(crate) fn cesu8_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    // The second byte's range is what the lead narrows: `E0` may not
    // encode a value below `U+0800`, and `ED` splits into the plain
    // `D000..D7FF` block and the surrogate halves.
    let (len, second) = match lead {
        0x00..=0x7f => return PreciseLen::Char(1),
        0xc2..=0xdf => (2, 0x80..=0xbf),
        0xe0 => (3, 0xa0..=0xbf),
        0xe1..=0xec | 0xee..=0xef => (3, 0x80..=0xbf),
        0xed => match bytes.get(pos + 1) {
            None => return PreciseLen::NeedMore,
            // A high surrogate: the six-byte form, whose second half
            // has to be a low one.
            Some(0xa0..=0xaf) => return cesu8_surrogate_pair(bytes, pos),
            Some(0x80..=0x9f) => (3, 0x80..=0x9f),
            Some(_) => return PreciseLen::Invalid,
        },
        _ => return PreciseLen::Invalid,
    };
    for (i, range) in std::iter::once(second)
        .chain(std::iter::repeat(0x80..=0xbf))
        .take(len - 1)
        .enumerate()
    {
        match bytes.get(pos + 1 + i) {
            None => return PreciseLen::NeedMore,
            Some(b) if range.contains(b) => {}
            Some(_) => return PreciseLen::Invalid,
        }
    }
    PreciseLen::Char(len)
}

/// The six-byte form, given that `bytes[pos..pos + 2]` is `ED A0..AF`.
fn cesu8_surrogate_pair(bytes: &[u8], pos: usize) -> PreciseLen {
    for (i, range) in [0x80..=0xbf, 0xed..=0xed, 0xb0..=0xbf, 0x80..=0xbf]
        .into_iter()
        .enumerate()
    {
        match bytes.get(pos + 2 + i) {
            None => return PreciseLen::NeedMore,
            Some(b) if range.contains(b) => {}
            Some(_) => return PreciseLen::Invalid,
        }
    }
    PreciseLen::Char(6)
}

/// Classify the Emacs-Mule sequence starting at `bytes[pos]`.
///
/// The lead byte fixes both the width and the range the *second* byte
/// must fall in — that byte is a charset id, and the private charsets
/// are what narrow it — and every byte after that must be
/// `0xA0..=0xFF`. Read off CRuby's own validator (`enc/emacs_mule.c`)
/// and checked against it over the whole lead/continuation space.
pub(crate) fn emacs_mule_precise_len(bytes: &[u8], pos: usize) -> PreciseLen {
    let Some(&lead) = bytes.get(pos) else {
        return PreciseLen::NeedMore;
    };
    let (len, second) = match lead {
        0x00..=0x7f => return PreciseLen::Char(1),
        0x81..=0x8f => (2, 0xa0..=0xff),
        0x90..=0x99 => (3, 0xa0..=0xff),
        0x9a..=0x9b => (3, 0xe0..=0xef),
        0x9c => (4, 0xf0..=0xf4),
        0x9d => (4, 0xf5..=0xfe),
        // 0x80, and everything from 0x9E up, lead nothing.
        _ => return PreciseLen::Invalid,
    };
    let avail = bytes.len() - pos;
    for i in 1..len {
        if i >= avail {
            return PreciseLen::NeedMore;
        }
        let b = bytes[pos + i];
        let ok = if i == 1 { second.contains(&b) } else { b >= 0xa0 };
        if !ok {
            return PreciseLen::Invalid;
        }
    }
    PreciseLen::Char(len)
}

/// The length of the complete Emacs-Mule character starting at
/// `bytes[pos]`, or `None` when none starts there — a prefix that
/// merely ran out of bytes counts as none.
pub(crate) fn emacs_mule_char_len(bytes: &[u8], pos: usize) -> Option<usize> {
    match emacs_mule_precise_len(bytes, pos) {
        PreciseLen::Char(n) => Some(n),
        _ => None,
    }
}

/// How long the ill-formed subpart starting at `bytes[pos]` is, given
/// that no character starts there.
///
/// CRuby's subparts are not one per byte: a run that is a well-formed
/// *prefix* of a character is one subpart, so Emacs-Mule `90 A0 20`
/// gives one replacement and then keeps the `0x20`. `enc_str_scrub`
/// finds the run by shortening the window until the prefix would only
/// need more bytes; this is that walk. An encoding whose `mbmaxlen` is
/// 2 can never have such a run, so it always answers 1.
fn ill_formed_run(
    bytes: &[u8],
    pos: usize,
    max_len: usize,
    precise: fn(&[u8], usize) -> PreciseLen,
) -> usize {
    let mut clen = max_len.min(bytes.len() - pos);
    if clen <= 2 {
        return 1;
    }
    clen -= 1;
    while clen > 1 && precise(&bytes[..pos + clen], pos) != PreciseLen::NeedMore {
        clen -= 1;
    }
    clen
}

/// One piece of a [`walk_mbc`] walk.
pub(crate) enum MbcPiece<'a> {
    /// A complete character.
    Char(&'a [u8]),
    /// One ill-formed subpart, whole.
    Bad(&'a [u8]),
}

/// Walk `bytes` as `precise` sees them, handing each complete
/// character and each ill-formed subpart to `on`.
///
/// The shared half of `String#scrub` and its block form: both need the
/// same notion of where one subpart ends and the next begins, and they
/// differ only in what they put in its place. A prefix at the very end
/// is one subpart covering the whole tail, and ends the walk.
pub(crate) fn walk_mbc(
    bytes: &[u8],
    max_len: usize,
    precise: fn(&[u8], usize) -> PreciseLen,
    on: impl FnMut(MbcPiece<'_>) -> Result<()>,
) -> Result<()> {
    walk_mbc_with(bytes, max_len, precise, IllFormed::Run, on)
}

/// How much of the buffer one ill-formed piece covers.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum IllFormed {
    /// A *run*: the bytes that start no character, and a truncated
    /// character's whole tail, are each one piece. `#scrub` replaces
    /// them once however many bytes they have.
    Run,
    /// One byte, then look again. `rb_str_inspect` advances
    /// `mbminlen` — one byte in every encoding walked here — on any
    /// byte that starts no character, so GB18030's `81 30` is a bad
    /// `\x81` followed by the *printable* `0` rather than two escapes,
    /// and EUC-TW's `8E A1 A1 41` is a bad `\x8E` and then a whole
    /// character (#1473).
    Byte,
}

/// [`walk_mbc`], with the caller's reading of an ill-formed piece.
pub(crate) fn walk_mbc_with(
    bytes: &[u8],
    max_len: usize,
    precise: fn(&[u8], usize) -> PreciseLen,
    ill_formed: IllFormed,
    mut on: impl FnMut(MbcPiece<'_>) -> Result<()>,
) -> Result<()> {
    let mut pos = 0;
    while pos < bytes.len() {
        match precise(bytes, pos) {
            PreciseLen::Char(n) => {
                on(MbcPiece::Char(&bytes[pos..pos + n]))?;
                pos += n;
            }
            _ if ill_formed == IllFormed::Byte => {
                on(MbcPiece::Bad(&bytes[pos..pos + 1]))?;
                pos += 1;
            }
            PreciseLen::NeedMore => {
                on(MbcPiece::Bad(&bytes[pos..]))?;
                return Ok(());
            }
            PreciseLen::Invalid => {
                let clen = ill_formed_run(bytes, pos, max_len, precise);
                on(MbcPiece::Bad(&bytes[pos..pos + clen]))?;
                pos += clen;
            }
        }
    }
    Ok(())
}

/// Copy `bytes`, putting `repl` in place of every ill-formed subpart.
pub(crate) fn scrub_mbc(
    bytes: &[u8],
    repl: &[u8],
    max_len: usize,
    precise: fn(&[u8], usize) -> PreciseLen,
) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    let _ = walk_mbc(bytes, max_len, precise, |piece| {
        match piece {
            MbcPiece::Char(c) => out.extend_from_slice(c),
            MbcPiece::Bad(_) => out.extend_from_slice(repl),
        }
        Ok(())
    });
    out
}

/// The `(mbmaxlen, precise_mbclen)` pair for an encoding monoruby walks
/// itself, or `None` for one it leaves to `encoding_rs` or to a
/// fixed-width rule.
pub(crate) fn mbc_walker(enc: Encoding) -> Option<(usize, fn(&[u8], usize) -> PreciseLen)> {
    match enc {
        Encoding::EucJp(_) => Some((EUCJP_MAX_LEN, eucjp_precise_len)),
        Encoding::Sjis(_) => Some((SJIS_MAX_LEN, sjis_precise_len)),
        Encoding::NamedByte(EMACS_MULE) => Some((EMACS_MULE_MAX_LEN, emacs_mule_precise_len)),
        // The CJK double-byte sets. Without a walk here nothing can
        // tell a character from a stray byte in them, so
        // `valid_encoding?` answered `true` for any bytes at all and
        // `#inspect` split characters down the middle (#1473).
        Encoding::NamedByte(i) => match named_byte_const_name(i) {
            "EUC_KR" | "GB2312" | "GB12345" => Some((2, euckr_precise_len)),
            "EUC_TW" => Some((4, euctw_precise_len)),
            "CP949" => Some((2, cp949_precise_len)),
            "Big5" | "Big5_HKSCS" | "Big5_UAO" | "CP950" | "CP951" => {
                Some((2, big5_precise_len))
            }
            "STATELESS_ISO_2022_JP" | "STATELESS_ISO_2022_JP_KDDI" => {
                Some((3, stateless_iso2022jp_precise_len))
            }
            "CESU_8" => Some((CESU8_MAX_LEN, cesu8_precise_len)),
            "GBK" => Some((2, gbk_precise_len)),
            "GB18030" => Some((4, gb18030_precise_len)),
            _ => None,
        },
        _ => None,
    }
}

/// Look up a [`Encoding::NamedByte`] index by its normalized
/// (uppercased, `-`/`.`→`_`) constant suffix.
pub(crate) fn named_byte_index(normalized_const: &str) -> Option<u8> {
    NAMED_BYTE_ENCODINGS
        .iter()
        .position(|(_, konst)| konst.eq_ignore_ascii_case(normalized_const))
        .map(|i| i as u8)
}

/// The `Encoding::<CONST>` suffix for a [`Encoding::NamedByte`] payload.
pub(crate) fn named_byte_const_name(index: u8) -> &'static str {
    NAMED_BYTE_ENCODINGS[index as usize].1
}

/// The `Encoding::<CONST>` suffix for an [`Encoding::EucJp`] payload.
pub(crate) fn euc_jp_const_name(index: u8) -> &'static str {
    EUC_JP_VARIANTS[index as usize].1
}

/// The `Encoding::<CONST>` suffix for an [`Encoding::Sjis`] payload.
pub(crate) fn sjis_const_name(index: u8) -> &'static str {
    SJIS_VARIANTS[index as usize].1
}

impl Encoding {
    /// True if the encoding is a strict superset of US-ASCII for
    /// 7-bit bytes — every byte 0..0x80 represents the same ASCII
    /// character regardless of the encoding. UTF-16 / UTF-32 are
    /// not (their code units are 2 / 4 bytes).
    pub fn is_ascii_compatible(self) -> bool {
        !matches!(
            self,
            Self::Utf16Le
                | Self::Utf16Be
                | Self::Utf32Le
                | Self::Utf32Be
                | Self::Iso2022Jp
                | Self::Other(_)
        )
    }

    /// True if the encoding pairs with US-ASCII / UTF-8 such that
    /// existing UTF-8-internal pipelines (`expect_str`,
    /// `String::chars`, regex calls) can run without modification.
    pub fn is_utf8_compatible(self) -> bool {
        matches!(self, Encoding::Utf8(_) | Encoding::UsAscii)
    }

    /// True if `self` and `other` are two *different* members of the
    /// [`UTF8_VARIANTS`] family.
    ///
    /// They hold the same bytes, so every read path treats them alike
    /// and [`Self::is_utf8_compatible`] answers `true` for both — but
    /// CRuby calls them incompatible encodings, and mixing them in one
    /// string raises. The pairwise checks that exempt a UTF-8 /
    /// US-ASCII pair have to exclude this one (#1562).
    pub fn is_distinct_utf8_variant(self, other: Self) -> bool {
        matches!((self, other), (Encoding::Utf8(a), Encoding::Utf8(b)) if a != b)
    }

    /// True if monoruby has no native byte→character decoder for
    /// this encoding and the bytes are stored opaquely. Used to
    /// route operations down the binary-style path.
    pub fn is_dummy(self) -> bool {
        !matches!(self, Encoding::Ascii8 | Encoding::Utf8(_) | Encoding::UsAscii)
    }

    /// True for the byte-oriented encodings whose 8-bit content runs
    /// through the byte↔U+00XX surrogate space
    /// ([`RStringInner::regex_view`]) rather than direct UTF-8 reads.
    pub fn is_byte_oriented(self) -> bool {
        matches!(
            self,
            Encoding::Ascii8
                | Encoding::Iso8859(_)
                | Encoding::EucJp(_)
                | Encoding::Sjis(_)
                | Encoding::Iso2022Jp
                | Encoding::Other(_)
                | Encoding::NamedByte(_)
        )
    }

    /// Canonical CRuby-facing name. Matches the constant-name suffix
    /// after `Encoding::`, except `_` is rendered as `-` for the
    /// hyphenated forms users see in `Encoding#to_s`.
    pub fn name(self) -> &'static str {
        match self {
            Encoding::Other(i) => OTHER_ENC_NAMES[i as usize],
            Encoding::NamedByte(i) => NAMED_BYTE_ENCODINGS[i as usize].0,
            Encoding::Ascii8 => "ASCII-8BIT",
            Encoding::Utf8(i) => UTF8_VARIANTS[i as usize].0,
            Encoding::UsAscii => "US-ASCII",
            Encoding::Utf16Le => "UTF-16LE",
            Encoding::Utf16Be => "UTF-16BE",
            Encoding::Utf32Le => "UTF-32LE",
            Encoding::Utf32Be => "UTF-32BE",
            Encoding::Iso8859(1) => "ISO-8859-1",
            Encoding::Iso8859(2) => "ISO-8859-2",
            Encoding::Iso8859(3) => "ISO-8859-3",
            Encoding::Iso8859(4) => "ISO-8859-4",
            Encoding::Iso8859(5) => "ISO-8859-5",
            Encoding::Iso8859(6) => "ISO-8859-6",
            Encoding::Iso8859(7) => "ISO-8859-7",
            Encoding::Iso8859(8) => "ISO-8859-8",
            Encoding::Iso8859(9) => "ISO-8859-9",
            Encoding::Iso8859(10) => "ISO-8859-10",
            Encoding::Iso8859(11) => "ISO-8859-11",
            Encoding::Iso8859(13) => "ISO-8859-13",
            Encoding::Iso8859(14) => "ISO-8859-14",
            Encoding::Iso8859(15) => "ISO-8859-15",
            Encoding::Iso8859(16) => "ISO-8859-16",
            Encoding::Iso8859(_) => "ISO-8859-1",
            Encoding::EucJp(i) => EUC_JP_VARIANTS[i as usize].0,
            Encoding::Sjis(i) => SJIS_VARIANTS[i as usize].0,
            Encoding::Iso2022Jp => "ISO-2022-JP",
        }
    }

    /// Inspect form used by `Encoding#inspect` /
    /// `RStringInner::str_encoding`'s i-var label.
    pub fn inspect_label(self) -> String {
        match self {
            Encoding::Ascii8 => "BINARY (ASCII-8BIT)".to_string(),
            other => other.name().to_string(),
        }
    }

    /// Walk `bytes` and classify them into a `CodeRange` for *this*
    /// encoding. Used to populate the `cr` cache lazily.
    pub fn classify(self, bytes: &[u8]) -> CodeRange {
        if bytes.is_empty() {
            return CodeRange::SevenBit;
        }
        // Quick path: bytes that are all ASCII are SevenBit under any
        // ASCII-compatible encoding (UTF-8/US-ASCII/EUC-JP/SJIS/
        // ISO-8859-*). UTF-16/32 don't qualify even for "abc" (since
        // their code units are 2/4 bytes), so skip the fast path.
        if self.is_ascii_compatible() && bytes.iter().all(|b| *b < 0x80) {
            return CodeRange::SevenBit;
        }
        match self {
            Encoding::UsAscii => {
                // US-ASCII permits *only* 7-bit bytes; the SevenBit
                // fast path above handles the all-ASCII case, so
                // anything reaching here has at least one high byte
                // and is therefore Broken.
                CodeRange::Broken
            }
            Encoding::Utf8(_) => match std::str::from_utf8(bytes) {
                Ok(_) => CodeRange::Valid,
                Err(_) => CodeRange::Broken,
            },
            Encoding::Ascii8 => CodeRange::Valid, // every byte is "valid"
            // Emacs-Mule has no codec either, but it does have a
            // shape, and CRuby reports a byte that does not fit it as
            // broken — which is what lets `String#scrub` and
            // `#encode(invalid: :replace)` see anything to replace.
            Encoding::NamedByte(EMACS_MULE) => {
                let mut pos = 0;
                while pos < bytes.len() {
                    match emacs_mule_char_len(bytes, pos) {
                        Some(n) => pos += n,
                        None => return CodeRange::Broken,
                    }
                }
                CodeRange::Valid
            }
            // The CJK double-byte sets have no codec here either, but
            // they do have a shape, and CRuby reports bytes that do not
            // fit it as broken — which is what `#valid_encoding?`,
            // `#scrub` and `invalid: :replace` all key off (#1473).
            Encoding::NamedByte(_) if mbc_walker(self).is_some() => {
                let (_, precise) = mbc_walker(self).expect("just checked");
                let mut pos = 0;
                while pos < bytes.len() {
                    match precise(bytes, pos) {
                        PreciseLen::Char(n) => pos += n,
                        _ => return CodeRange::Broken,
                    }
                }
                CodeRange::Valid
            }
            // No native codec and no shape: raw bytes, every sequence
            // "valid". ISO-2022-JP belongs here with the other dummy
            // encodings (UTF-7, CP50220/1, …): it is stateful, so Ruby
            // gives it no character decoder at all and every byte
            // string labelled with it is valid. Decoding it to look
            // for truncated escapes, as this used to, made the same
            // bytes Broken here and Valid as CP50220 — its own variant
            // — and `valid_encoding?` answer `false` where CRuby says
            // `true` (#1554).
            Encoding::Other(_) | Encoding::NamedByte(_) | Encoding::Iso2022Jp => CodeRange::Valid,
            Encoding::Iso8859(_) => CodeRange::Valid, // every byte 0..256 represents a glyph
            // For encodings we don't decode natively, treat any
            // sequence as Valid unless its byte count contradicts
            // the code-unit width.
            // The surrogate rules are part of the encoding, not of
            // Unicode alone: a lone half is Broken in UTF-16, and a
            // UTF-32 unit must be a scalar value.
            Encoding::Utf16Le | Encoding::Utf16Be => {
                if bytes.len() % 2 != 0 {
                    return CodeRange::Broken;
                }
                let be = self == Encoding::Utf16Be;
                let unit = |i: usize| {
                    let (hi, lo) = if be {
                        (bytes[i], bytes[i + 1])
                    } else {
                        (bytes[i + 1], bytes[i])
                    };
                    ((hi as u32) << 8) | lo as u32
                };
                let mut i = 0;
                while i < bytes.len() {
                    let u = unit(i);
                    if (0xD800..0xDC00).contains(&u) {
                        // A high surrogate needs a low one after it.
                        if i + 4 > bytes.len() || !(0xDC00..0xE000).contains(&unit(i + 2)) {
                            return CodeRange::Broken;
                        }
                        i += 4;
                    } else if (0xDC00..0xE000).contains(&u) {
                        // A low surrogate on its own.
                        return CodeRange::Broken;
                    } else {
                        i += 2;
                    }
                }
                CodeRange::Valid
            }
            Encoding::Utf32Le | Encoding::Utf32Be => {
                if bytes.len() % 4 != 0 {
                    return CodeRange::Broken;
                }
                let be = self == Encoding::Utf32Be;
                for unit in bytes.chunks_exact(4) {
                    let u = if be {
                        u32::from_be_bytes([unit[0], unit[1], unit[2], unit[3]])
                    } else {
                        u32::from_le_bytes([unit[0], unit[1], unit[2], unit[3]])
                    };
                    if u > 0x10FFFF || (0xD800..0xE000).contains(&u) {
                        return CodeRange::Broken;
                    }
                }
                CodeRange::Valid
            }
            Encoding::EucJp(_) | Encoding::Sjis(_) => {
                let char_w = if matches!(self, Encoding::EucJp(_)) {
                    eucjp_char_width
                } else {
                    sjis_char_width
                };
                let mut i = 0;
                while i < bytes.len() {
                    match char_w(&bytes[i..]) {
                        Some(w) => i += w,
                        None => return CodeRange::Broken,
                    }
                }
                CodeRange::Valid
            }
        }
    }

    /// Best-effort encoding-name parser. Recognises every name in
    /// `init_encoding`'s constant table; unknown names raise
    /// `ArgumentError` matching CRuby.
    pub fn try_from_str(s: &str) -> Result<Self> {
        // CRuby resolves a name through `StringValueCStr`, so an
        // embedded NUL is rejected before any table is consulted, with
        // a message of its own.
        if s.as_bytes().contains(&0) {
            return Err(MonorubyErr::argumenterr(
                "invalid encoding name (NUL byte)",
            ));
        }
        // Normalize: uppercase, replace '-' / '.' with '_'.
        //
        // Every name we recognise is ASCII and short, so normalise into a
        // stack buffer. The obvious
        // `s.to_uppercase().replace('-', "_").replace('.', "_")` allocates
        // three times per call, and this runs on every `IO#getc` /
        // `IO#gets` (three times each, resolving the stream's encodings).
        let mut buf = [0u8; MAX_ENC_NAME];
        let normalized: &str = if s.len() <= MAX_ENC_NAME && s.is_ascii() {
            for (i, b) in s.as_bytes().iter().enumerate() {
                buf[i] = match b {
                    b'-' | b'.' => b'_',
                    b => b.to_ascii_uppercase(),
                };
            }
            // SAFETY: the input was ASCII and every replacement is ASCII.
            unsafe { std::str::from_utf8_unchecked(&buf[..s.len()]) }
        } else {
            // Only reachable for names no encoding has; fall through to
            // the error arm below without a fast path.
            return Err(MonorubyErr::argumenterr(format!(
                "unknown encoding name - {s}"
            )));
        };
        match normalized {
            "UTF_8" | "UTF8" | "CP65001" => Ok(Encoding::UTF8),
            // Apple's HFS+ form. Its bytes are UTF-8, so it shares
            // every storage and iteration path; what differs is the
            // name and the decomposition a conversion applies (#1562).
            "UTF8_MAC" | "UTF_8_MAC" | "UTF_8_HFS" => Ok(Encoding::Utf8(UTF8_MAC)),
            // UTF-8's bytes throughout; a conversion spells a block of
            // characters as the carrier's own emoji instead (#1573).
            "UTF8_DOCOMO" => Ok(Encoding::Utf8(UTF8_DOCOMO)),
            "UTF8_KDDI" => Ok(Encoding::Utf8(UTF8_KDDI)),
            "UTF8_SOFTBANK" => Ok(Encoding::Utf8(UTF8_SOFTBANK)),
            // CESU-8 is not a UTF-8 variant: a four-byte sequence is
            // invalid in it and a surrogate pair is one character, so
            // it has a walk of its own ([`cesu8_precise_len`]) and
            // sits with the encodings that carry their own name.
            "CESU_8" | "CESU8" => Ok(Encoding::NamedByte(CESU_8)),

            // ASCII-incompatible stateful / dummy byte encodings with
            // no native codec: name-preserved, `#inspect` escapes
            // every byte, symbols are quoted (CRuby semantics).
            "UTF_7" | "CP65000" => Ok(Encoding::Other(0)),
            "CP50220" => Ok(Encoding::Other(1)),
            "CP50221" => Ok(Encoding::Other(2)),
            "ASCII_8BIT" | "BINARY" => Ok(Encoding::Ascii8),
            "US_ASCII" | "ASCII" | "ANSI_X3_4_1968" | "646" => Ok(Encoding::UsAscii),
            "LOCALE" | "EXTERNAL" | "FILESYSTEM" => Ok(Encoding::UTF8),

            // Bare `UTF-16` / `UTF-32` are CRuby's BOM-based *dummy*
            // encodings, distinct from the real `UTF-16LE` / … codecs:
            // ASCII-incompatible, byte-oriented, name-preserved.
            "UTF_16" => Ok(Encoding::Other(3)),
            "UTF_32" => Ok(Encoding::Other(4)),
            "IBM037" | "EBCDIC_CP_US" => Ok(Encoding::Other(5)),
            "ISO_2022_JP_2" | "ISO2022_JP2" => Ok(Encoding::Other(6)),
            "ISO_2022_JP_KDDI" => Ok(Encoding::Other(7)),
            "UTF_16LE" => Ok(Encoding::Utf16Le),
            "UTF_16BE" | "UCS_2BE" => Ok(Encoding::Utf16Be),
            "UTF_32LE" | "UCS_4LE" => Ok(Encoding::Utf32Le),
            "UTF_32BE" | "UCS_4BE" => Ok(Encoding::Utf32Be),

            "ISO_8859_1" | "ISO8859_1" | "LATIN1" => Ok(Encoding::Iso8859(1)),
            "ISO_8859_2" | "ISO8859_2" | "LATIN2" => Ok(Encoding::Iso8859(2)),
            "ISO_8859_3" | "ISO8859_3" | "LATIN3" => Ok(Encoding::Iso8859(3)),
            "ISO_8859_4" | "ISO8859_4" | "LATIN4" => Ok(Encoding::Iso8859(4)),
            "ISO_8859_5" | "ISO8859_5" => Ok(Encoding::Iso8859(5)),
            "ISO_8859_6" | "ISO8859_6" => Ok(Encoding::Iso8859(6)),
            "ISO_8859_7" | "ISO8859_7" => Ok(Encoding::Iso8859(7)),
            "ISO_8859_8" | "ISO8859_8" => Ok(Encoding::Iso8859(8)),
            "ISO_8859_9" | "ISO8859_9" | "LATIN5" => Ok(Encoding::Iso8859(9)),
            "ISO_8859_10" | "ISO8859_10" | "LATIN6" => Ok(Encoding::Iso8859(10)),
            "ISO_8859_11" | "ISO8859_11" => Ok(Encoding::Iso8859(11)),
            "ISO_8859_13" | "ISO8859_13" | "LATIN7" => Ok(Encoding::Iso8859(13)),
            "ISO_8859_14" | "ISO8859_14" | "LATIN8" => Ok(Encoding::Iso8859(14)),
            "ISO_8859_15" | "ISO8859_15" | "LATIN9" => Ok(Encoding::Iso8859(15)),
            "ISO_8859_16" | "ISO8859_16" | "LATIN10" => Ok(Encoding::Iso8859(16)),

            "EUC_JP" | "EUCJP" => Ok(Encoding::EUC_JP),
            // The rest of the family: one codec, their own names. They
            // used to answer as EUC-JP, so a string asked to be
            // `CP51932` came back labelled `EUC-JP` while
            // `Encoding.find` said otherwise (#1562).
            "EUCJP_MS" | "EUCJP_WIN" | "EUC_JP_MS" | "EUC_JP_WIN" => {
                Ok(Encoding::EucJp(euc_jp_variant_index("EUCJP_MS")))
            }
            "CP51932" => Ok(Encoding::EucJp(euc_jp_variant_index("CP51932"))),
            "EUC_JIS_2004" | "EUC_JISX0213" => {
                Ok(Encoding::EucJp(euc_jp_variant_index("EUC_JIS_2004")))
            }
            "STATELESS_ISO_2022_JP" => Ok(Encoding::NamedByte(
                named_byte_index("STATELESS_ISO_2022_JP").unwrap(),
            )),
            "STATELESS_ISO_2022_JP_KDDI" => Ok(Encoding::NamedByte(
                named_byte_index("STATELESS_ISO_2022_JP_KDDI").unwrap(),
            )),
            // Only ISO-2022-JP's own two names. `ISO-2022-JP-2` and
            // `ISO-2022-JP-KDDI` are encodings of their own in CRuby,
            // and answering them with this one relabelled the string
            // as something the caller did not ask for — while
            // `Encoding.find`, which reads the registry rather than
            // this table, rejected the same names outright (#1554).
            // `ISO-2022-JP-2004` is not a Ruby encoding at all.
            // Registering the real ones is #1555.
            "ISO_2022_JP" | "ISO2022_JP" => Ok(Encoding::Iso2022Jp),
            "SHIFT_JIS" => Ok(Encoding::Sjis(sjis_variant_index("SHIFT_JIS"))),
            // MacJapanese is a Shift_JIS variant. monoruby runs it on
            // the Shift_JIS codec and keeps only its name apart, as it
            // does for Windows-31J (#1471).
            "MACJAPANESE" | "MACJAPAN" => Ok(Encoding::Sjis(MACJAPANESE)),
            // The carrier sets ride Windows-31J's walk and differ from
            // it only in what a conversion does (#1573).
            "SJIS_DOCOMO" => Ok(Encoding::Sjis(SJIS_DOCOMO)),
            "SJIS_KDDI" => Ok(Encoding::Sjis(SJIS_KDDI)),
            "SJIS_SOFTBANK" => Ok(Encoding::Sjis(SJIS_SOFTBANK)),
            // CRuby's "SJIS" is an alias of Windows-31J, not of Shift_JIS.
            "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" | "PCK" | "SJIS" => {
                Ok(Encoding::Sjis(WINDOWS_31J))
            }

            // ASCII-compatible national byte encodings without a native
            // codec: bytes are stored raw (like ASCII-8BIT) but the
            // declared name is preserved via `Encoding::NamedByte`, so
            // `# encoding: big5` reports `__ENCODING__.name == "Big5"`.
            "BIG5" => Ok(Encoding::NamedByte(named_byte_index("Big5").unwrap())),
            // `:2008` is the year of the revision, and part of the
            // name CRuby answers to (#1520).
            "BIG5_HKSCS" | "BIG5HKSCS" | "BIG5_HKSCS:2008" => {
                Ok(Encoding::NamedByte(named_byte_index("Big5_HKSCS").unwrap()))
            }
            // CP950 / CP951 are encodings of their own rather than
            // aliases of the two above, which is how they were read
            // here (#1555).
            "CP950" => Ok(Encoding::NamedByte(named_byte_index("CP950").unwrap())),
            "CP951" => Ok(Encoding::NamedByte(named_byte_index("CP951").unwrap())),
            "BIG5_UAO" => Ok(Encoding::NamedByte(named_byte_index("Big5_UAO").unwrap())),
            "GBK" | "CP936" => Ok(Encoding::NamedByte(named_byte_index("GBK").unwrap())),
            "GB2312" | "EUC_CN" | "EUCCN" => {
                Ok(Encoding::NamedByte(named_byte_index("GB2312").unwrap()))
            }
            "GB18030" => Ok(Encoding::NamedByte(named_byte_index("GB18030").unwrap())),
            "GB12345" => Ok(Encoding::NamedByte(named_byte_index("GB12345").unwrap())),
            "EUC_KR" | "EUCKR" => Ok(Encoding::NamedByte(named_byte_index("EUC_KR").unwrap())),
            // CP949 is EUC-KR's superset, and a *different* encoding to
            // CRuby: `Encoding::CP949 != Encoding::EUC_KR`, and the name
            // it keeps is the one a conversion error reports (#1471).
            "CP949" => Ok(Encoding::NamedByte(named_byte_index("CP949").unwrap())),
            "EUC_TW" | "EUCTW" => Ok(Encoding::NamedByte(named_byte_index("EUC_TW").unwrap())),
            "TIS_620" | "TIS620" => Ok(Encoding::NamedByte(named_byte_index("TIS_620").unwrap())),
            "IBM720" | "CP720" => Ok(Encoding::NamedByte(named_byte_index("IBM720").unwrap())),
            "WINDOWS_874" | "CP874" => {
                Ok(Encoding::NamedByte(named_byte_index("Windows_874").unwrap()))
            }
            "GB1988" => Ok(Encoding::NamedByte(named_byte_index("GB1988").unwrap())),
            "KOI8_R" | "CP878" => Ok(Encoding::NamedByte(named_byte_index("KOI8_R").unwrap())),
            "KOI8_U" => Ok(Encoding::NamedByte(named_byte_index("KOI8_U").unwrap())),
            "WINDOWS_1250" | "CP1250" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1250").unwrap(),
            )),
            "WINDOWS_1251" | "CP1251" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1251").unwrap(),
            )),
            "WINDOWS_1252" | "CP1252" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1252").unwrap(),
            )),
            "WINDOWS_1253" | "CP1253" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1253").unwrap(),
            )),
            "WINDOWS_1254" | "CP1254" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1254").unwrap(),
            )),
            "WINDOWS_1255" | "CP1255" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1255").unwrap(),
            )),
            "WINDOWS_1256" | "CP1256" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1256").unwrap(),
            )),
            "WINDOWS_1257" | "CP1257" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1257").unwrap(),
            )),
            "WINDOWS_1258" | "CP1258" => Ok(Encoding::NamedByte(
                named_byte_index("Windows_1258").unwrap(),
            )),
            "IBM437" | "CP437" => Ok(Encoding::NamedByte(named_byte_index("IBM437").unwrap())),
            "IBM737" | "CP737" => Ok(Encoding::NamedByte(named_byte_index("IBM737").unwrap())),
            "IBM775" | "CP775" => Ok(Encoding::NamedByte(named_byte_index("IBM775").unwrap())),
            "IBM850" | "CP850" => Ok(Encoding::NamedByte(named_byte_index("IBM850").unwrap())),
            "IBM852" => Ok(Encoding::NamedByte(named_byte_index("IBM852").unwrap())),
            "IBM855" => Ok(Encoding::NamedByte(named_byte_index("IBM855").unwrap())),
            "CP852" => Ok(Encoding::NamedByte(named_byte_index("CP852").unwrap())),
            "CP855" => Ok(Encoding::NamedByte(named_byte_index("CP855").unwrap())),
            "IBM857" | "CP857" => Ok(Encoding::NamedByte(named_byte_index("IBM857").unwrap())),
            "IBM860" | "CP860" => Ok(Encoding::NamedByte(named_byte_index("IBM860").unwrap())),
            "IBM861" | "CP861" => Ok(Encoding::NamedByte(named_byte_index("IBM861").unwrap())),
            "IBM862" | "CP862" => Ok(Encoding::NamedByte(named_byte_index("IBM862").unwrap())),
            "IBM863" | "CP863" => Ok(Encoding::NamedByte(named_byte_index("IBM863").unwrap())),
            "IBM864" | "CP864" => Ok(Encoding::NamedByte(named_byte_index("IBM864").unwrap())),
            "IBM865" | "CP865" => Ok(Encoding::NamedByte(named_byte_index("IBM865").unwrap())),
            "IBM866" | "CP866" => Ok(Encoding::NamedByte(named_byte_index("IBM866").unwrap())),
            "IBM869" | "CP869" => Ok(Encoding::NamedByte(named_byte_index("IBM869").unwrap())),

            "EMACS_MULE" => Ok(Encoding::NamedByte(named_byte_index("Emacs_Mule").unwrap())),

            // The Mac OS script encodings keep their own names, and
            // the eight with a table convert through it (#1471).
            "MACROMAN" => Ok(Encoding::NamedByte(named_byte_index("MacRoman").unwrap())),
            "MACCYRILLIC" => Ok(Encoding::NamedByte(
                named_byte_index("MacCyrillic").unwrap(),
            )),
            "MACCENTEURO" => Ok(Encoding::NamedByte(
                named_byte_index("MacCentEuro").unwrap(),
            )),
            "MACCROATIAN" => Ok(Encoding::NamedByte(
                named_byte_index("MacCroatian").unwrap(),
            )),
            "MACGREEK" => Ok(Encoding::NamedByte(named_byte_index("MacGreek").unwrap())),
            "MACICELAND" => Ok(Encoding::NamedByte(
                named_byte_index("MacIceland").unwrap(),
            )),
            "MACROMANIA" => Ok(Encoding::NamedByte(
                named_byte_index("MacRomania").unwrap(),
            )),
            "MACTHAI" => Ok(Encoding::NamedByte(named_byte_index("MacThai").unwrap())),
            "MACTURKISH" => Ok(Encoding::NamedByte(
                named_byte_index("MacTurkish").unwrap(),
            )),
            "MACUKRAINE" => Ok(Encoding::NamedByte(
                named_byte_index("MacUkraine").unwrap(),
            )),

            _ => Err(MonorubyErr::argumenterr(format!(
                "unknown encoding name - {s}"
            ))),
        }
    }

    /// CRuby's `Encoding.compatible?(a, b)` algorithm for two
    /// strings (ignoring Symbols / nil / Regexp inputs handled by
    /// the caller). Returns the result encoding, or `None` if the
    /// pair is incompatible.
    pub fn compatible(
        a_enc: Encoding,
        a_cr: CodeRange,
        b_enc: Encoding,
        b_cr: CodeRange,
    ) -> Option<Encoding> {
        if a_enc == b_enc {
            return Some(a_enc);
        }
        // CRuby's `rb_enc_compatible` order:
        //
        // 1. Both sides ASCII-compatible AND both 7-bit → the *first*
        //    encoding (left side wins) — `compatible?("abc",
        //    "def".encode("US-ASCII")) == Encoding::UTF_8`.
        // 2. Both ASCII-compatible, exactly one 7-bit → the
        //    non-7-bit side (whoever has actual non-ASCII content
        //    keeps its encoding).
        // 3. Otherwise → incompatible.
        let a_ascii = a_enc.is_ascii_compatible();
        let b_ascii = b_enc.is_ascii_compatible();
        let a_seven = matches!(a_cr, CodeRange::SevenBit);
        let b_seven = matches!(b_cr, CodeRange::SevenBit);
        if a_ascii && b_ascii {
            if a_seven && b_seven {
                return Some(a_enc);
            }
            if a_seven {
                return Some(b_enc);
            }
            if b_seven {
                return Some(a_enc);
            }
        }
        None
    }
}

/// Tag stored in the `capacity` slot of a *shared* `StringContent`.
///
/// A real `SmallVec` capacity can never be `isize::MAX` (Rust
/// allocations are bounded by `isize::MAX` bytes), so the value is an
/// unambiguous discriminant. It is deliberately `isize::MAX` rather
/// than `usize::MAX`: the JIT's inline `String#bytesize` / `#getbyte`
/// select inline-vs-heap storage with a *signed* `capa > INLINE_CAP`
/// compare (`cmovgt` / `csel gt`), so the tag must stay positive to
/// route shared strings onto the heap path (where the shared `ptr` /
/// `len` overlay the spilled SmallVec's fields — see `SharedContent`).
pub(crate) const STRING_SHARED_TAG: usize = isize::MAX as usize;

/// Payload of a shared (zero-copy substring) `StringContent`. Field
/// order is layout-critical: it overlays the vendored `SmallVec`
/// (whose layout the JIT already depends on via `smallvec::OFFSET_*`):
///
/// | offset | `SmallVec` (spilled)  | `SharedContent` |
/// |--------|-----------------------|-----------------|
/// | 0      | capacity              | tag (= `STRING_SHARED_TAG`) |
/// | 8      | heap ptr              | ptr             |
/// | 16     | heap len              | len             |
/// | 24     | (inline tail)         | root            |
///
/// Keeping `ptr`/`len` on the spilled heap-ptr/len offsets means the
/// JIT's read-only inline string ops (`bytesize`, `getbyte`) work on
/// shared strings without modification; only mutating inline ops
/// (`setbyte`) need a shared check (they deopt).
#[repr(C)]
#[derive(Clone, Copy)]
struct SharedContent {
    tag: usize,
    /// Start of this string's view, pointing into `root`'s heap buffer.
    ptr: *const u8,
    /// Byte length of the view.
    len: usize,
    /// The hidden, frozen String that owns the heap buffer. Kept alive
    /// by the GC via `RValue::mark` on every sharer.
    root: Value,
}

/// The owned byte buffer of a String: inline up to `STRING_INLINE_CAP`
/// bytes, spilled to the heap beyond. Builders that know their result
/// lands in a String assemble it in one of these and hand it over with
/// `RStringInner::from_buf_cr`, so short results never allocate.
pub(crate) type StringBuf = SmallVec<[u8; STRING_INLINE_CAP]>;

/// Byte storage of a Ruby String: either an owned buffer (the plain
/// `SmallVec`, inline ≤ `STRING_INLINE_CAP` bytes or spilled to the
/// heap) or a zero-copy view into a frozen root's buffer. The active
/// variant is discriminated by the first `usize` (the SmallVec
/// `capacity` slot): `STRING_SHARED_TAG` means shared.
#[repr(C)]
union StringContent {
    owned: ManuallyDrop<SmallVec<[u8; STRING_INLINE_CAP]>>,
    shared: SharedContent,
}

impl StringContent {
    #[inline]
    fn is_shared(&self) -> bool {
        // SAFETY: both variants start with a usize (SmallVec's
        // `capacity` / SharedContent's `tag`), so reading it through
        // either field is always valid.
        unsafe { self.shared.tag == STRING_SHARED_TAG }
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        unsafe {
            if self.is_shared() {
                // SAFETY: `ptr`/`len` describe a live sub-range of the
                // root's heap buffer; the root is kept alive by the GC
                // as long as this sharer is, and its buffer is frozen
                // (never reallocated).
                std::slice::from_raw_parts(self.shared.ptr, self.shared.len)
            } else {
                &self.owned
            }
        }
    }

    #[inline]
    fn from_owned(owned: SmallVec<[u8; STRING_INLINE_CAP]>) -> Self {
        StringContent {
            owned: ManuallyDrop::new(owned),
        }
    }
}

///
/// Ruby-level String.
///
/// This struct is used to represent a Ruby-level String.
/// `content` is an opaque byte buffer; the declared `ty` (encoding)
/// is informational only — invalid byte sequences for the declared
/// encoding are tolerated (e.g. `"\xff".force_encoding("UTF-8")`).
/// `cr` caches the result of walking `content` against `ty` so that
/// `valid_encoding?` / `ascii_only?` / encoding-compatibility checks
/// don't re-scan on every call.
///
/// `content` must stay the first field (the JIT's inline string ops
/// address it at `RVALUE_OFFSET_KIND + smallvec::OFFSET_*`).
///
#[repr(C)]
pub struct RStringInner {
    content: StringContent,
    ty: Encoding,
    cr: Cell<CodeRange>,
}

impl Drop for RStringInner {
    fn drop(&mut self) {
        if !self.content.is_shared() {
            // SAFETY: discriminated by the tag — `owned` is the live
            // variant here, and it is dropped exactly once.
            unsafe { ManuallyDrop::drop(&mut self.content.owned) }
        }
        // Shared: nothing to drop — the root's buffer is owned by the
        // root RValue and freed when the GC collects it.
    }
}

impl Clone for RStringInner {
    fn clone(&self) -> Self {
        let content = if self.content.is_shared() {
            // Cloning a sharer just adds another sharer of the same
            // root (O(1)); copy-on-write protects all of them.
            // SAFETY: tag-discriminated; `shared` is the live variant.
            StringContent {
                shared: unsafe { self.content.shared },
            }
        } else {
            // SAFETY: tag-discriminated; `owned` is the live variant.
            StringContent::from_owned(unsafe { (*self.content.owned).clone() })
        };
        RStringInner {
            content,
            ty: self.ty,
            cr: self.cr.clone(),
        }
    }
}

impl std::fmt::Debug for RStringInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RStringInner")
            .field("content", &self.as_bytes())
            .field("shared", &self.content.is_shared())
            .field("ty", &self.ty)
            .field("cr", &self.cr)
            .finish()
    }
}

impl Eq for RStringInner {}

/// Forward surrogate map for [`RStringInner::regex_view`]: every
/// byte becomes the Unicode scalar of the same value (i.e. the
/// Latin-1 → UTF-8 transcode). One input byte ↔ one output `char`.
pub fn map_bytes_to_utf8(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| b as char).collect()
}

/// Byte offset of the cached `CodeRange` (`cr`) from the head of an
/// `RValue` holding a String, for the JIT's inline `String#setbyte`.
/// `Cell<CodeRange>` has the same layout as `CodeRange` (`repr(u8)`).
pub const STRING_CR_OFFSET: usize =
    super::RVALUE_OFFSET_KIND + std::mem::offset_of!(RStringInner, cr);

/// Byte offset of the encoding tag (`ty`) from the head of an `RValue`
/// holding a String, for the JIT's inline `String#<<`. `Encoding` is
/// `repr(u8)`, so its first byte is the discriminant (`Ascii8` == 0)
/// and the byte after it is [`Encoding::payload`] — the variant index
/// of the variants that carry one.
pub const STRING_TY_OFFSET: usize =
    super::RVALUE_OFFSET_KIND + std::mem::offset_of!(RStringInner, ty);

/// Byte offset of the encoding's payload — the variant index of an
/// `Encoding` variant that carries one, padding for one that does not.
///
/// The JIT's inline string literal writes it, and the inline
/// `String#<<` reads it to tell UTF-8 from `UTF8-MAC`: those two share
/// a discriminant, so within `Utf8` the tag byte alone is not the
/// encoding (#1562).
pub const STRING_TY_PAYLOAD_OFFSET: usize = STRING_TY_OFFSET + 1;

/// The one discriminant `STRING_TY_MAX_INLINE_SHL` admits that carries
/// a payload byte, so the inline `String#<<` has to read
/// [`STRING_TY_PAYLOAD_OFFSET`] before calling two equal tags an equal
/// encoding.
pub const STRING_TY_PAYLOAD_TAG: u8 = Encoding::UTF8.tag();

/// The largest encoding tag the JIT's inline `String#<<` may append a
/// raw byte into: `Ascii8` (0), `Utf8` (1), `UsAscii` (2). In these
/// three a 7-bit codepoint *is* its byte; in UTF-16 / UTF-32 it is two
/// or four, and the encodings past `UsAscii` have multibyte sequences
/// the fast path does not know how to build, so they take the helper.
/// The `Utf8` tag covers every [`UTF8_VARIANTS`] entry, which is what
/// this path wants: they hold UTF-8 bytes, so a 7-bit byte is that
/// character in all of them.
/// (The high-byte case narrows further to `Ascii8` — see
/// `emit_string_shl`.)
pub const STRING_TY_MAX_INLINE_SHL: u8 = Encoding::UsAscii.tag();

impl Encoding {
    /// Canonical UTF-8. The other [`UTF8_VARIANTS`] hold the same
    /// bytes under a different normalisation.
    pub const UTF8: Self = Encoding::Utf8(0);

    /// Canonical EUC-JP. The other [`EUC_JP_VARIANTS`] share its codec
    /// and differ only in the name they report.
    pub const EUC_JP: Self = Encoding::EucJp(0);

    /// The `repr(u8)` discriminant, as the JIT reads it out of
    /// [`STRING_TY_OFFSET`].
    pub const fn tag(self) -> u8 {
        // SAFETY: `Encoding` is `#[repr(u8)]`, so its first byte is the
        // discriminant and reading it through a pointer cast is the
        // documented way to obtain one from a value with fields.
        unsafe { *(&self as *const Self as *const u8) }
    }

    /// The variant index the discriminant is followed by, or `0` for a
    /// variant that carries none.
    ///
    /// The second half of what [`STRING_TY_OFFSET`] addresses: a
    /// payload-carrying variant's index sits one byte past its
    /// discriminant, so writing a whole `Encoding` from the JIT is the
    /// two bytes together. A payload-free variant's second byte is
    /// padding, and `0` is what the JIT stores there — reading it back
    /// is never a payload, so any value would do.
    pub const fn payload(self) -> u8 {
        match self {
            Encoding::Utf8(i)
            | Encoding::Iso8859(i)
            | Encoding::EucJp(i)
            | Encoding::Sjis(i)
            | Encoding::Other(i)
            | Encoding::NamedByte(i) => i,
            _ => 0,
        }
    }
}

impl std::ops::Deref for RStringInner {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        self.content.as_slice()
    }
}

/// CRuby's `rb_str_eql`: the same bytes *and* encodings that can be
/// compared. This is `String#==` / `#eql?`, and it is also what a Hash
/// or Set asks of a String key — `RubyEql for Value` answers a pair of
/// plain Strings from here.
///
/// It used to compare bytes alone, which the Ruby-level `==` then
/// corrected with its own encoding check. The container had no such
/// correction, so `{"あ" => 1}["あ".b]` found the entry and
/// `[a, b].uniq` collapsed the pair, for every encoding rather than
/// only the UTF-8 family (#1569).
impl std::cmp::PartialEq for RStringInner {
    fn eq(&self, other: &Self) -> bool {
        // Same bytes, and encodings that can be compared. Equal tags are
        // the common case by far (every key of a Hash built from one
        // source string), so answer them here rather than through
        // `compatible_encoding`'s general negotiation.
        self.as_bytes() == other.as_bytes()
            && (self.ty == other.ty || self.compatible_encoding(other).is_some())
    }
}

/// The digest [`PartialEq`] obliges: equal strings must hash alike.
///
/// CRuby's `rb_str_hash` xors the encoding index into the byte digest,
/// except for ASCII-only content, where it uses 0. So two 7-bit
/// strings hash alike whatever their encodings — which is what makes
/// `"abc" == "abc".b` usable as one Hash key — and two others only
/// when their encodings match, which is exactly when `eq` above can
/// call them equal with the same bytes.
impl std::hash::Hash for RStringInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_bytes().hash(state);
        if !self.is_ascii_only() {
            self.ty.tag().hash(state);
            self.ty.payload().hash(state);
        }
    }
}

impl std::cmp::PartialOrd<Self> for RStringInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let lhs = self.as_bytes();
        let rhs = other.as_bytes();
        let ord = if lhs.len() >= rhs.len() {
            for (r, l) in rhs.iter().zip(lhs.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => {}
                    ord => return Some(ord),
                }
            }
            if lhs.len() == rhs.len() {
                Ordering::Equal
            } else {
                Ordering::Greater
            }
        } else {
            for (l, r) in lhs.iter().zip(rhs.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => {}
                    ord => return Some(ord),
                }
            }
            Ordering::Less
        };
        Some(ord)
    }
}

impl std::cmp::Ord for RStringInner {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl RStringInner {
    pub fn to_str(&self) -> Result<std::borrow::Cow<'_, str>> {
        if self.ty.is_utf8_compatible() {
            match std::str::from_utf8(self) {
                Ok(s) => Ok(std::borrow::Cow::Borrowed(s)),
                Err(err) => Err(MonorubyErr::runtimeerr(format!(
                    "invalid byte sequence: {} {}",
                    err,
                    String::from_utf8_lossy(self.as_bytes())
                ))),
            }
        } else {
            // Binary / non-UTF-8 encodings (Ascii8, UTF-16/32,
            // ISO-8859, EUC-JP, Shift_JIS, …): render byte-wise
            // with `\xHH` for high bytes.
            let mut res = String::new();
            for c in self.as_bytes() {
                if c.is_ascii() {
                    res.push(*c as char);
                } else {
                    res += &format!(r#"\x{:0>2X}"#, c);
                }
            }
            Ok(std::borrow::Cow::Owned(res))
        }
    }

    pub fn dump(&self) -> String {
        if self.ty.is_utf8_compatible() {
            let mut res = String::with_capacity(self.len());
            utf8_dump_with_lookahead(&mut res, self.as_bytes());
            res
        } else {
            let mut res = String::with_capacity(self.len());
            let bytes = self.as_bytes();
            for (i, c) in bytes.iter().enumerate() {
                let next = bytes.get(i + 1).copied().unwrap_or(0);
                if *c == b'#' && matches!(next, b'$' | b'@' | b'{') {
                    res.push_str("\\#");
                } else {
                    ascii_escape(&mut res, *c);
                }
            }
            res
        }
    }

    pub fn inspect(&self) -> String {
        match self.ty {
            Encoding::Utf8(_) => {
                let mut res = String::with_capacity(self.len());
                utf8_inspect_with_lookahead(&mut res, self.as_bytes(), true);
                res
            }
            // UTF-16 / UTF-32 (not ASCII-compatible, Unicode): decode
            // to scalar values, then render ASCII chars with the
            // normal escape rules and every non-ASCII codepoint as
            // `\uXXXX` / `\u{XXXXX}` (CRuby never shows them literally
            // because the result encoding differs from the string's).
            Encoding::Utf16Le | Encoding::Utf16Be | Encoding::Utf32Le | Encoding::Utf32Be => {
                let chars = decode_unicode_units(self.as_bytes(), self.ty);
                let mut res = String::with_capacity(self.len());
                for (idx, &c) in chars.iter().enumerate() {
                    let next = chars.get(idx + 1).copied().unwrap_or('\0');
                    unicode_inspect_char(&mut res, c, next);
                }
                res
            }
            // ASCII-compatible, non-Unicode multibyte (EUC-JP,
            // Shift_JIS, Emacs-Mule): CRuby's `rb_str_inspect` picks
            // its escape by whether the bytes form a *character*.
            //
            // - ASCII characters use the normal rules;
            // - a **valid** character with no rendering is written
            //   whole, in one `\x{HH..}` — but a one-byte character
            //   (Shift_JIS's halfwidth katakana, say) is just `\xHH`,
            //   since there is nothing to group;
            // - a byte that starts no character is written on its own,
            //   `\xHH`, one per byte, and a printable byte after the
            //   ill-formed run stays outside the escape.
            //
            // That last rule is why this walks with `walk_mbc` rather
            // than `iter_char_bytes`: the walker separates complete
            // characters from ill-formed subparts, which is exactly the
            // distinction the two escapes encode.
            ty if mbc_walker(ty).is_some() => {
                let (max_len, precise) = mbc_walker(ty).unwrap();
                let bytes = self.as_bytes();
                let mut res = String::with_capacity(self.len());
                let mut pos = 0;
                let _ = walk_mbc_with(bytes, max_len, precise, IllFormed::Byte, |piece| {
                    let len = match piece {
                        MbcPiece::Char(cb) | MbcPiece::Bad(cb) => cb.len(),
                    };
                    let next = bytes.get(pos + 1).copied();
                    pos += len;
                    match piece {
                        MbcPiece::Char(cb) if cb.len() == 1 => {
                            ascii_escape_with_next(&mut res, cb[0], next)
                        }
                        MbcPiece::Char(cb) => {
                            res.push_str("\\x{");
                            for b in cb {
                                res.push_str(&format!("{b:0>2X}"));
                            }
                            res.push('}');
                        }
                        MbcPiece::Bad(cb) => {
                            for b in cb {
                                res.push_str(&format!("\\x{b:0>2X}"));
                            }
                        }
                    }
                    Ok(())
                });
                res
            }
            // ASCII-incompatible stateful / no-codec byte encodings
            // (ISO-2022-JP, UTF-7, CP50220/1, …): CRuby escapes *every*
            // byte as `\xHH`, even printable ASCII — except the control
            // characters that have a named escape, which keep it
            // (`\e\x24\x42` for an ESC $ B sequence, not `\x1B\x24\x42`).
            Encoding::Iso2022Jp | Encoding::Other(_) => {
                let mut res = String::with_capacity(self.len() * 4);
                for b in self.as_bytes() {
                    match b {
                        0x07 => res.push_str("\\a"),
                        0x08 => res.push_str("\\b"),
                        0x09 => res.push_str("\\t"),
                        0x0a => res.push_str("\\n"),
                        0x0b => res.push_str("\\v"),
                        0x0c => res.push_str("\\f"),
                        0x0d => res.push_str("\\r"),
                        0x1b => res.push_str("\\e"),
                        _ => res.push_str(&format!("\\x{b:0>2X}")),
                    }
                }
                res
            }
            // US-ASCII, ASCII-8BIT, ISO-8859-N: pure per-byte —
            // ASCII rules for < 0x80, `\xHH` otherwise.
            _ => {
                let bytes = self.as_bytes();
                let mut res = String::with_capacity(self.len());
                for (i, c) in bytes.iter().enumerate() {
                    ascii_escape_with_next(&mut res, *c, bytes.get(i + 1).copied());
                }
                res
            }
        }
    }

    pub fn valid(&self) -> bool {
        self.is_valid_encoding()
    }
}

/// True iff byte position `pos` falls on a UTF-8 character boundary
/// inside `bytes`. Assumes the surrounding bytes are valid UTF-8 — under
/// that invariant, a non-continuation byte (top two bits != 10) is
/// always the start of a new codepoint.
#[inline]
fn is_utf8_char_boundary(bytes: &[u8], pos: usize) -> bool {
    pos == 0 || pos == bytes.len() || (bytes[pos] & 0xC0) != 0x80
}

fn utf8_escape(s: &mut String, ch: char) {
    let cp = ch as u32;
    if cp < 0x80 {
        // ASCII bytes are escaped per the standard `\\xNN` /
        // named-escape rules. Letting `ascii_escape` handle this
        // keeps `dump` and `inspect` aligned for plain ASCII.
        ascii_escape(s, ch as u8);
    } else if cp <= 0xFFFF {
        // BMP non-ASCII: 4-digit `\uNNNN` form.
        s.push_str(&format!("\\u{:0>4X}", cp));
    } else {
        // Supplementary planes: brace form, variable width.
        s.push_str(&format!("\\u{{{:X}}}", cp));
    }
}

/// Render a single character for `String#inspect`. `next_ch` is the
/// following character (or `'\0'` if there is none); used to escape
/// `#` when followed by `$`, `@`, or `{`. `is_utf8` switches the
/// escape style for ASCII control characters: UTF-8 strings use the
/// `\uNNNN` form, while US-ASCII / binary strings use `\xNN`.
fn utf8_inspect_with_next(s: &mut String, ch: char, next_ch: char, is_utf8: bool) {
    if ch.is_ascii() {
        let b = ch as u8;
        match b {
            b'"' => s.push_str("\\\""),
            b'\\' => s.push_str("\\\\"),
            b'\t' => s.push_str("\\t"),
            b'\n' => s.push_str("\\n"),
            b'\r' => s.push_str("\\r"),
            b'\x0c' => s.push_str("\\f"),
            b'\x08' => s.push_str("\\b"),
            b'\x07' => s.push_str("\\a"),
            b'\x1b' => s.push_str("\\e"),
            b'\x0b' => s.push_str("\\v"),
            b'#' if matches!(next_ch, '$' | '@' | '{') => s.push_str("\\#"),
            c if c.is_ascii_graphic() || c == b' ' => s.push(c as char),
            _ => {
                if is_utf8 {
                    s.push_str(&format!("\\u{:0>4X}", b));
                } else {
                    s.push_str(&format!("\\x{:0>2X}", b));
                }
            }
        }
    } else if printable::is_printable(ch) {
        s.push(ch);
    } else {
        let cp = ch as u32;
        // CRuby prefers the 4-digit `\uNNNN` form for BMP codepoints
        // and only falls back to `\u{N}` once the codepoint exceeds
        // 0xFFFF (`"\u{1F600}".inspect` → `"\\u{1F600}"`).
        if cp <= 0xFFFF {
            s.push_str(&format!("\\u{:0>4X}", cp));
        } else {
            s.push_str(&format!("\\u{{{:X}}}", cp));
        }
    }
}

/// Decode UTF-16/UTF-32 (LE/BE) bytes into scalar values for
/// `#inspect`. Mirrors the transcoder's hand-rolled codecs; invalid
/// units degrade to U+FFFD (CRuby would emit `\xHH` for genuinely
/// broken units, but the inspect specs only exercise valid input —
/// and U+FFFD still renders as `�`, matching CRuby's output for
/// already-U+FFFD content).
fn decode_unicode_units(bytes: &[u8], enc: Encoding) -> Vec<char> {
    let mut out = Vec::new();
    match enc {
        Encoding::Utf32Le | Encoding::Utf32Be => {
            let be = matches!(enc, Encoding::Utf32Be);
            for c in bytes.chunks(4) {
                if c.len() < 4 {
                    out.push('\u{FFFD}');
                    continue;
                }
                let v = if be {
                    u32::from_be_bytes([c[0], c[1], c[2], c[3]])
                } else {
                    u32::from_le_bytes([c[0], c[1], c[2], c[3]])
                };
                out.push(char::from_u32(v).unwrap_or('\u{FFFD}'));
            }
        }
        _ => {
            let be = matches!(enc, Encoding::Utf16Be);
            let units: Vec<u16> = bytes
                .chunks(2)
                .map(|c| {
                    if c.len() < 2 {
                        0xFFFDu16
                    } else if be {
                        u16::from_be_bytes([c[0], c[1]])
                    } else {
                        u16::from_le_bytes([c[0], c[1]])
                    }
                })
                .collect();
            let mut i = 0;
            while i < units.len() {
                let u = units[i];
                if (0xD800..=0xDBFF).contains(&u)
                    && i + 1 < units.len()
                    && (0xDC00..=0xDFFF).contains(&units[i + 1])
                {
                    let hi = (u as u32 - 0xD800) << 10;
                    let lo = units[i + 1] as u32 - 0xDC00;
                    out.push(char::from_u32(0x10000 + hi + lo).unwrap_or('\u{FFFD}'));
                    i += 2;
                } else {
                    out.push(char::from_u32(u as u32).unwrap_or('\u{FFFD}'));
                    i += 1;
                }
            }
        }
    }
    out
}

/// `#inspect` for a single char of a Unicode but non-ASCII-compatible
/// encoding (UTF-16/UTF-32): ASCII chars use the normal escape rules;
/// every non-ASCII codepoint is `\uXXXX` / `\u{XXXXX}` (never shown
/// literally, because the result encoding differs from the string's).
fn unicode_inspect_char(s: &mut String, ch: char, next_ch: char) {
    if ch.is_ascii() {
        utf8_inspect_with_next(s, ch, next_ch, true);
    } else {
        let cp = ch as u32;
        if cp <= 0xFFFF {
            s.push_str(&format!("\\u{:0>4X}", cp));
        } else {
            s.push_str(&format!("\\u{{{:X}}}", cp));
        }
    }
}

/// Like `utf8_escape_bytes(..., utf8_inspect)` but threads the next
/// character through so `utf8_inspect_with_next` can decide whether to
/// escape `#` as `\#` (when followed by `$`, `@`, `{`).
fn utf8_inspect_with_lookahead(res: &mut String, bytes: &[u8], is_utf8: bool) {
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(valid) => {
                let chars: Vec<char> = valid.chars().collect();
                for (idx, &c) in chars.iter().enumerate() {
                    let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                    utf8_inspect_with_next(res, c, next_ch, is_utf8);
                }
                break;
            }
            Err(e) => {
                let valid_up_to = e.valid_up_to();
                if valid_up_to > 0 {
                    let valid = std::str::from_utf8(&bytes[i..i + valid_up_to]).unwrap_or("");
                    let chars: Vec<char> = valid.chars().collect();
                    for (idx, &c) in chars.iter().enumerate() {
                        let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                        utf8_inspect_with_next(res, c, next_ch, is_utf8);
                    }
                    i += valid_up_to;
                }
                let bad_len = e.error_len().unwrap_or(bytes.len() - i);
                for &b in &bytes[i..i + bad_len] {
                    res.push_str(&format!("\\x{:0>2X}", b));
                }
                i += bad_len;
            }
        }
    }
}

/// [`ascii_escape`] with the one-byte lookahead CRuby's `rb_str_inspect`
/// applies in *every* ASCII-compatible encoding: a `#` immediately
/// before `$`, `@` or `{` is written `\#`, so the inspected form does
/// not read back as an interpolation. (`#dump` does the same, and
/// already did so here for every encoding.)
fn ascii_escape_with_next(s: &mut String, ch: u8, next: Option<u8>) {
    if ch == b'#' && matches!(next, Some(b'$') | Some(b'@') | Some(b'{')) {
        s.push_str("\\#");
        return;
    }
    ascii_escape(s, ch);
}

fn ascii_escape(s: &mut String, ch: u8) {
    let str = match ch {
        b'"' => "\\\"",
        b'\\' => "\\\\",
        c if c.is_ascii_graphic() => {
            s.push(c as char);
            return;
        }
        b' ' => " ",
        b'\t' => "\\t",
        b'\x0b' => "\\v",
        b'\n' => "\\n",
        b'\r' => "\\r",
        b'\x0c' => "\\f",
        b'\x08' => "\\b",
        b'\x07' => "\\a",
        b'\x1b' => "\\e",
        _ => {
            s.push_str(&format!("\\x{:0>2X}", ch));
            return;
        }
    };
    s.push_str(str);
}

/// `String#dump` with one-character lookahead so `#` can be escaped
/// to `\#` when the next char is `$` / `@` / `{` (CRuby keeps the
/// dumped form parseable as a Ruby literal — those trigrams would
/// otherwise re-trigger interpolation when parsed back).
fn utf8_dump_with_lookahead(res: &mut String, bytes: &[u8]) {
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(valid) => {
                let chars: Vec<char> = valid.chars().collect();
                for (idx, &c) in chars.iter().enumerate() {
                    let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                    utf8_dump_one(res, c, next_ch);
                }
                break;
            }
            Err(err) => {
                let valid_up_to = err.valid_up_to();
                if valid_up_to > 0 {
                    let valid_str =
                        unsafe { std::str::from_utf8_unchecked(&bytes[i..i + valid_up_to]) };
                    let chars: Vec<char> = valid_str.chars().collect();
                    for (idx, &c) in chars.iter().enumerate() {
                        let next_ch = chars.get(idx + 1).copied().unwrap_or('\0');
                        utf8_dump_one(res, c, next_ch);
                    }
                }
                let error_len = err.error_len().unwrap_or(bytes.len() - i - valid_up_to);
                for b in &bytes[i + valid_up_to..i + valid_up_to + error_len] {
                    res.push_str(&format!("\\x{:0>2X}", b));
                }
                i += valid_up_to + error_len;
            }
        }
    }
}

fn utf8_dump_one(s: &mut String, ch: char, next_ch: char) {
    if ch == '#' && matches!(next_ch, '$' | '@' | '{') {
        s.push_str("\\#");
        return;
    }
    utf8_escape(s, ch);
}

fn scrub_utf8(bytes: &[u8], repl: &[u8], out: &mut Vec<u8>) {
    // Walk the byte stream, replacing each "maximal subpart" of an
    // ill-formed sequence with a single replacement (matches the
    // Unicode/CRuby definition of `scrub`).
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(rest) => {
                out.extend_from_slice(rest.as_bytes());
                return;
            }
            Err(e) => {
                let valid_up_to = e.valid_up_to();
                if valid_up_to > 0 {
                    out.extend_from_slice(&bytes[i..i + valid_up_to]);
                    i += valid_up_to;
                }
                let bad_len = e.error_len().unwrap_or(bytes.len() - i);
                out.extend_from_slice(repl);
                i += bad_len;
            }
        }
    }
}

impl RStringInner {
    /// Low-level constructor. Callers pass the `cr` they want to
    /// record — either `Unknown` (defer classification to the first
    /// query) or a value computed up front (caller has already done
    /// the scan). Keeping `cr` explicit at this layer means every
    /// public constructor states its scanning behaviour in one place.
    fn from(content: SmallVec<[u8; STRING_INLINE_CAP]>, ty: Encoding, cr: CodeRange) -> Self {
        RStringInner {
            content: StringContent::from_owned(content),
            ty,
            cr: Cell::new(cr),
        }
    }

    /// Construct a zero-copy view of `len` bytes at `ptr` inside
    /// `root`'s heap buffer. `root` must be a frozen String whose
    /// content is owned and spilled (so the buffer address is stable
    /// for the root's lifetime).
    fn from_shared(root: Value, ptr: *const u8, len: usize, ty: Encoding, cr: CodeRange) -> Self {
        RStringInner {
            content: StringContent {
                shared: SharedContent {
                    tag: STRING_SHARED_TAG,
                    ptr,
                    len,
                    root,
                },
            },
            ty,
            cr: Cell::new(cr),
        }
    }

    /// Whether this string is a zero-copy view into a shared root's
    /// buffer.
    #[inline]
    pub(crate) fn is_shared(&self) -> bool {
        self.content.is_shared()
    }

    /// The hidden root whose buffer this string shares, if any. Used
    /// by the GC to keep the buffer owner alive.
    #[inline]
    pub(crate) fn shared_root(&self) -> Option<Value> {
        if self.content.is_shared() {
            // SAFETY: tag-discriminated.
            Some(unsafe { self.content.shared.root })
        } else {
            None
        }
    }

    /// Mutation funnel: every write to the byte buffer goes through
    /// here. A shared string is first detached from its root
    /// (copy-on-write); an owned string is returned as-is.
    #[inline]
    fn owned_mut(&mut self) -> &mut SmallVec<[u8; STRING_INLINE_CAP]> {
        if self.content.is_shared() {
            self.uniquify();
        }
        // SAFETY: owned is the live variant (just uniquified if needed).
        unsafe { &mut self.content.owned }
    }

    /// Grow the owned buffer so it can hold `cap` bytes without
    /// reallocating — `String.new(capacity:)`'s allocation hint. A
    /// shared view is detached first: a sharer has no buffer of its own
    /// to grow. Nothing observable changes, so the cached code range
    /// stays valid.
    ///
    /// Fallible on purpose. `capacity: 2 ** 62` is a legal Ruby
    /// expression that CRuby answers with `NoMemoryError`; an
    /// infallible `reserve` would abort the process instead.
    pub(crate) fn try_reserve_capacity(&mut self, cap: usize) -> bool {
        let buf = self.owned_mut();
        let additional = cap.saturating_sub(buf.len());
        if additional == 0 {
            return true;
        }
        // Ask the `MONORUBY_MALLOC_HARD_LIMIT` guard first: it aborts the
        // process rather than failing the allocation, so `try_reserve`
        // would never get to report a `capacity: 2 ** 62`.
        if crate::alloc::would_exceed_malloc_hard_limit(additional) {
            return false;
        }
        buf.try_reserve(additional).is_ok()
    }

    /// The bytes, for overwriting in place at the same length (a shared
    /// view is detached first). The cached code range is dropped, since
    /// the caller may write anything.
    pub(crate) fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.cr.set(CodeRange::Unknown);
        self.owned_mut().as_mut_slice()
    }

    /// Detach a shared string from its root in place — `uniquify` for
    /// callers outside this module (the JIT's inline `String#<<` calls
    /// this through `runtime::str_detach` and retries its append on the
    /// now-owned buffer instead of deopting). A no-op on an owned string.
    pub(crate) fn detach(&mut self) {
        self.uniquify();
    }

    /// Detach a shared string from its root by copying the viewed
    /// bytes into a fresh owned buffer (the "write" half of
    /// copy-on-write).
    fn uniquify(&mut self) {
        if self.content.is_shared() {
            let owned = SmallVec::from_slice(self.content.as_slice());
            // Plain assignment: the old value is shared, so RStringInner's
            // Drop has nothing to release for it.
            self.content = StringContent::from_owned(owned);
        }
    }

    pub fn encoding(&self) -> Encoding {
        self.ty
    }

    /// Set the declared encoding tag. Resets the cached code range
    /// since the same bytes may now classify differently
    /// (`"abc".force_encoding("UTF-16LE")` was SevenBit under
    /// UTF-8 but has odd byte count under UTF-16LE).
    pub fn set_encoding(&mut self, ty: Encoding) {
        // Re-tagging with the same encoding changes nothing the code
        // range describes; keep it cached. (Dropping it made every
        // `sub` / `gsub` / `force_encoding` result an *uncached* piece,
        // which the append fast path then folded into an Unknown
        // receiver — see `emit_string_shl`.)
        if self.ty != ty {
            self.ty = ty;
            self.cr.set(CodeRange::Unknown);
        }
    }

    /// Returns the cached code range, computing it on first call.
    pub fn code_range(&self) -> CodeRange {
        match self.cr.get() {
            CodeRange::Unknown => {
                let cr = self.ty.classify(self.as_bytes());
                self.cr.set(cr);
                cr
            }
            other => other,
        }
    }

    pub fn is_ascii_only(&self) -> bool {
        matches!(self.code_range(), CodeRange::SevenBit)
    }

    pub fn is_valid_encoding(&self) -> bool {
        matches!(self.code_range(), CodeRange::SevenBit | CodeRange::Valid)
    }

    /// Number of characters under the declared encoding. Always
    /// succeeds — broken byte sequences fall back to byte counting
    /// (matches CRuby's `String#length` returning byte length on
    /// invalid UTF-8).
    pub fn char_length(&self) -> usize {
        match self.ty {
            // Fixed 1-byte-per-char.
            // Walked encodings count characters, not bytes: Emacs-Mule
            // and the CJK double-byte sets (#1473).
            Encoding::NamedByte(_) if mbc_walker(self.ty).is_some() => match self.code_range() {
                CodeRange::SevenBit => self.len(),
                _ => self.iter_char_bytes().count(),
            },
            Encoding::Ascii8
            | Encoding::UsAscii
            | Encoding::Iso8859(_)
            | Encoding::Other(_)
            | Encoding::NamedByte(_)
            // ISO-2022-JP counts bytes, as CRuby does for a dummy
            // encoding and as this string's own character iterator
            // already does (`fixed_char_width` is 1 for it). Decoding
            // it here made `length` 2 where `chars.size` was 10.
            | Encoding::Iso2022Jp => self.len(),
            // UTF-16 / UTF-32 with one extra unit per broken trailing
            // byte. CRuby's `String#length` for these reports the
            // number of *complete* code units plus one per stray byte
            // ("adds 1 (and not 2) for a incomplete surrogate in
            // UTF-16").
            // A surrogate pair counts once, so the units have to be
            // walked; a stray trailing byte still adds one.
            Encoding::Utf16Le | Encoding::Utf16Be => self.iter_char_bytes().count(),
            Encoding::Utf32Le | Encoding::Utf32Be => {
                let len = self.len();
                len / 4 + (len % 4 > 0) as usize
            }
            // UTF-8: count valid scalars and count each invalid byte
            // individually (CRuby's "adds 1 for every invalid byte
            // in UTF-8" rule). Use the cached cr instead of re-running
            // from_utf8 -- a SevenBit string can answer in O(1).
            Encoding::Utf8(_) => match self.code_range() {
                CodeRange::SevenBit => self.len(),
                CodeRange::Valid => self
                    .as_bytes()
                    .iter()
                    .filter(|&&b| (b & 0xC0) != 0x80)
                    .count(),
                CodeRange::Broken => self.iter_char_bytes().count(),
                // code_range() always populates `cr` to a concrete
                // variant before returning; Unknown is unreachable.
                CodeRange::Unknown => unreachable!(),
            },
            // EUC-JP / Shift_JIS: native stateless multibyte decode.
            // ASCII-only content is 1 byte/char, so the cached
            // SevenBit range answers in O(1); otherwise walk the
            // encoding-aware character iterator.
            Encoding::EucJp(_) | Encoding::Sjis(_) => match self.code_range() {
                CodeRange::SevenBit => self.len(),
                _ => self.iter_char_bytes().count(),
            },
        }
    }

    /// Iterate the string's characters as byte-slice views into
    /// `self`. The yielded slices reference `self.content`. Used by
    /// `String#chars` / `#each_char` / `#slice` to be encoding-aware
    /// without converting through a `&str`.
    pub fn iter_char_bytes(&self) -> CharByteIter<'_> {
        CharByteIter {
            bytes: self.as_bytes(),
            pos: 0,
            encoding: self.ty,
        }
    }

    /// Codepoint-reversed copy of `self`. The declared encoding and
    /// the cached code range are both preserved: reversing UTF-8
    /// codepoints yields valid UTF-8, ASCII-only buffers stay
    /// ASCII-only, and a broken UTF-8 input stays broken because
    /// `iter_char_bytes` walks invalid bytes one-at-a-time (the
    /// same "each invalid byte is its own char" rule CRuby's
    /// `rb_str_reverse` applies).
    pub fn reverse(&self) -> Self {
        let enc = self.ty;
        let bytes = self.as_bytes();
        let cr = self.code_range();

        // Fast path — pure byte reversal — when every char occupies
        // a single byte: SevenBit (ASCII-only under any encoding) or
        // genuine single-byte encodings regardless of content.
        let byte_reversible = matches!(cr, CodeRange::SevenBit)
            || matches!(
                enc,
                Encoding::Ascii8 | Encoding::UsAscii | Encoding::Iso8859(_)
            );
        if byte_reversible {
            let mut buf: SmallVec<[u8; STRING_INLINE_CAP]> = SmallVec::with_capacity(bytes.len());
            buf.extend(bytes.iter().rev().copied());
            return RStringInner::from(buf, enc, cr);
        }

        // Encoding-aware path. Walk the source forward via
        // `iter_char_bytes` and copy each char's bytes into the
        // output starting from the tail. This costs one allocation
        // and one forward pass — no intermediate Vec of slices.
        let n = bytes.len();
        let mut buf: SmallVec<[u8; STRING_INLINE_CAP]> = SmallVec::with_capacity(n);
        // SAFETY: capacity is exactly `n` and the loop below writes
        // every byte before the value is observed (each
        // `iter_char_bytes` step consumes a disjoint slice covering
        // the whole buffer in aggregate).
        unsafe {
            buf.set_len(n);
            let dst = buf.as_mut_ptr();
            let mut write = n;
            for slice in self.iter_char_bytes() {
                write -= slice.len();
                std::ptr::copy_nonoverlapping(slice.as_ptr(), dst.add(write), slice.len());
            }
            debug_assert_eq!(write, 0);
        }
        RStringInner::from(buf, enc, cr)
    }

    /// Negotiate the result encoding for an operation that combines
    /// `self` and `other` (string concatenation, replacement, …).
    /// Returns `None` if the two encodings are not compatible —
    /// callers should raise `Encoding::CompatibilityError`.
    pub fn compatible_encoding(&self, other: &Self) -> Option<Encoding> {
        // Equal encodings always compatible (even dummy / non-ASCII-
        // compatible — `"".force_encoding("UTF-7") +
        // "".force_encoding("UTF-7")` keeps UTF-7).
        if self.encoding() == other.encoding() {
            return Some(self.encoding());
        }
        let self_empty = self.is_empty();
        let other_empty = other.is_empty();
        // Both empty: left wins unconditionally (CRuby preserves
        // the receiver's declared encoding when both sides carry
        // no bytes — even when the encodings are non-ASCII-compat
        // or dummy, e.g. `"".encode("UTF-16BE") << "".encode("US-ASCII")`
        // ⇒ UTF-16BE).
        if self_empty && other_empty {
            return Some(self.encoding());
        }
        // One side empty (E), other non-empty (N) — CRuby's
        // `rb_enc_compatible` rule:
        //   - If N is 7-bit ASCII-only AND E.encoding is
        //     ASCII-compatible → the *left*-most encoding wins.
        //   - Otherwise → N's encoding wins (covers both "N has
        //     non-ASCII content" and "E is non-ASCII-compat").
        // The "left wins on 7-bit" branch matches behaviour like
        // `"".encode("UTF-8") << "abc".encode("US-ASCII")` ⇒ UTF-8
        // and its mirror `"abc".encode("US-ASCII") << "".encode("UTF-8")`
        // ⇒ US-ASCII.
        if self_empty {
            if self.encoding().is_ascii_compatible()
                && matches!(other.code_range(), CodeRange::SevenBit)
            {
                return Some(self.encoding());
            }
            return Some(other.encoding());
        }
        if other_empty {
            if other.encoding().is_ascii_compatible()
                && matches!(self.code_range(), CodeRange::SevenBit)
            {
                return Some(self.encoding());
            }
            return Some(self.encoding());
        }
        // Both non-empty: route through the encoding-only rule.
        Encoding::compatible(
            self.encoding(),
            self.code_range(),
            other.encoding(),
            other.code_range(),
        )
    }

    /// True if the `&str`-based pattern/transform machinery must go
    /// through the byte↔`U+00XX` surrogate mapping ([`Self::regex_view`])
    /// instead of reading the bytes as UTF-8 directly: the declared
    /// encoding is a byte-oriented one (not UTF-8/US-ASCII, not
    /// UTF-16/32) and the content has non-ASCII bytes. ASCII-only
    /// content needs no mapping — it is its own UTF-8 image.
    ///
    /// Note this is keyed on the *declared* encoding, not on whether
    /// the bytes happen to be valid UTF-8: ISO-8859-1 `"\xC3\xA9"`
    /// is two characters (`Ã©`), and reading it as UTF-8 (`é`) would
    /// give regex `.`/char-offset semantics of the wrong encoding.
    pub fn needs_byte_mapping(&self) -> bool {
        self.ty.is_byte_oriented() && !self.is_ascii_only()
    }

    /// A guaranteed-valid-UTF-8 view of `self` for the `&str`-based
    /// pattern/transform machinery (regex matching, `tr`, `succ`,
    /// `split`, …).
    ///
    /// - UTF-8 / US-ASCII / ASCII-only content: the direct byte view
    ///   (borrowed), erroring like [`Self::check_utf8`] on broken
    ///   UTF-8 — matching CRuby's `ArgumentError` for regex
    ///   operations on invalid strings.
    /// - Byte-oriented encodings with 8-bit content: an owned
    ///   surrogate image where every byte `b` becomes the scalar
    ///   `U+00bb` ([`map_bytes_to_utf8`]). One original byte ↔ one
    ///   `char` in the view, so *char* offsets/counts in the view
    ///   equal *byte* (= character, for single-byte encodings)
    ///   offsets in the original. Results computed in view space are
    ///   decoded back with [`Self::from_mapped_utf8`].
    ///
    /// For multibyte byte-oriented encodings (EUC-JP / Shift_JIS)
    /// this is a byte-wise approximation of CRuby's per-encoding
    /// character walking; ASCII-pattern matching still lands on the
    /// right bytes (previously these raised outright).
    pub fn regex_view(&self) -> Result<std::borrow::Cow<'_, str>> {
        if self.needs_byte_mapping() {
            Ok(std::borrow::Cow::Owned(map_bytes_to_utf8(self.as_bytes())))
        } else {
            self.check_utf8().map(std::borrow::Cow::Borrowed)
        }
    }

    /// Decode a string built in [`Self::regex_view`]'s surrogate
    /// space back to raw bytes tagged `enc`: every scalar `≤ U+00FF`
    /// becomes the single byte of the same value. Scalars above
    /// `U+00FF` can only appear if a replacement smuggled in
    /// characters the byte encoding cannot hold (the compatibility
    /// checks normally raise first); those keep their UTF-8 bytes as
    /// a best effort rather than panicking.
    pub fn from_mapped_utf8(s: &str, enc: Encoding) -> Self {
        let mut out: SmallVec<[u8; STRING_INLINE_CAP]> = SmallVec::with_capacity(s.len());
        for c in s.chars() {
            let cp = c as u32;
            if cp <= 0xFF {
                out.push(cp as u8);
            } else {
                let mut buf = [0u8; 4];
                out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            }
        }
        RStringInner::from(out, enc, CodeRange::Unknown)
    }

    pub fn check_utf8(&self) -> Result<&str> {
        // Skip the O(n) re-validation when we already know the answer:
        //
        //   - SevenBit: every byte is < 0x80, which is a strict subset
        //     of valid UTF-8, regardless of the declared encoding.
        //   - Utf8 + Valid: classify ran from_utf8 and accepted the
        //     bytes. Other encodings can also classify as Valid (e.g.
        //     EUC-JP / ASCII-8BIT) without being valid UTF-8, so we
        //     still re-check those.
        let cr = self.code_range();
        if matches!(cr, CodeRange::SevenBit)
            || (matches!(self.ty, Encoding::Utf8(_)) && matches!(cr, CodeRange::Valid))
        {
            // SAFETY: see above.
            return Ok(unsafe { std::str::from_utf8_unchecked(self.as_bytes()) });
        }
        match std::str::from_utf8(self) {
            Ok(s) => Ok(s),
            Err(_) => Err(MonorubyErr::argumenterr("invalid byte sequence in UTF-8")),
        }
    }

    /// Returns a copy of `self` with each invalid byte sequence replaced
    /// by `repl` (default `"\u{FFFD}"` for Unicode-aware encodings, `"?"`
    /// for ASCII-only encodings).
    pub fn scrub(&self, repl: &RStringInner) -> Result<RStringInner> {
        let bytes = self.as_bytes();
        let enc = self.encoding();
        if self.is_valid_encoding() {
            // Already valid — clone the receiver so the result inherits
            // its cached cr instead of reverting to Unknown. Same alloc
            // cost as `from_encoding(bytes, enc)` (both copy `bytes`),
            // but preserves the SevenBit/Valid classification the caller
            // had already paid for.
            return Ok(self.clone());
        }
        let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
        match enc {
            Encoding::Utf8(_) => scrub_utf8(bytes, repl.as_bytes(), &mut out),
            Encoding::UsAscii => {
                for &b in bytes {
                    if b < 0x80 {
                        out.push(b);
                    } else {
                        out.extend_from_slice(repl.as_bytes());
                    }
                }
            }
            // Emacs-Mule, EUC-JP and Shift_JIS share one walk: an
            // ill-formed subpart is a *run*, not a byte, so a
            // truncated EUC-JP tail (`8F A1`) is one replacement and
            // not two. Shift_JIS never has such a run — its widest
            // character is two bytes — but it costs nothing to say so
            // once.
            _ if mbc_walker(enc).is_some() => {
                let (max_len, precise) = mbc_walker(enc).unwrap();
                out = scrub_mbc(bytes, repl.as_bytes(), max_len, precise);
            }
            // UTF-16/32: replace each ill-formed coding unit (a lone
            // surrogate half, a non-scalar UTF-32 unit) and the odd
            // tail, unit by unit, as `rb_enc_str_scrub` does.
            Encoding::Utf16Le | Encoding::Utf16Be => {
                let be = enc == Encoding::Utf16Be;
                let unit = |i: usize| {
                    let (hi, lo) = if be {
                        (bytes[i], bytes[i + 1])
                    } else {
                        (bytes[i + 1], bytes[i])
                    };
                    ((hi as u32) << 8) | lo as u32
                };
                let mut i = 0;
                while i + 2 <= bytes.len() {
                    let u = unit(i);
                    let width = if (0xD800..0xDC00).contains(&u) {
                        if i + 4 <= bytes.len() && (0xDC00..0xE000).contains(&unit(i + 2)) {
                            4
                        } else {
                            0
                        }
                    } else if (0xDC00..0xE000).contains(&u) {
                        0
                    } else {
                        2
                    };
                    if width == 0 {
                        out.extend_from_slice(repl.as_bytes());
                        i += 2;
                    } else {
                        out.extend_from_slice(&bytes[i..i + width]);
                        i += width;
                    }
                }
                if i < bytes.len() {
                    out.extend_from_slice(repl.as_bytes());
                }
            }
            Encoding::Utf32Le | Encoding::Utf32Be => {
                let be = enc == Encoding::Utf32Be;
                let mut i = 0;
                while i + 4 <= bytes.len() {
                    let u = if be {
                        u32::from_be_bytes([bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]])
                    } else {
                        u32::from_le_bytes([bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]])
                    };
                    if u > 0x10FFFF || (0xD800..0xE000).contains(&u) {
                        out.extend_from_slice(repl.as_bytes());
                    } else {
                        out.extend_from_slice(&bytes[i..i + 4]);
                    }
                    i += 4;
                }
                if i < bytes.len() {
                    out.extend_from_slice(repl.as_bytes());
                }
            }
            _ => out.extend_from_slice(bytes),
        }
        // Scrub by definition produces a fully valid byte sequence under
        // `enc`, so we eagerly classify the result. Skipping this leaves
        // cr=Unknown and the next operation that touches cr would walk
        // the whole buffer again.
        Ok(RStringInner::from_encoding_scanned(&out, enc))
    }

    /// O(1): build a UTF-8 string from `&str` without scanning.
    /// `&str` guarantees valid UTF-8 bytes, but the resulting string
    /// starts with `cr = Unknown` — the first operation that needs
    /// the code range (e.g. `is_ascii_only?`, `check_utf8`,
    /// encoding compatibility) will lazy-classify. Use this for
    /// ephemeral strings where the cr may never be queried; pay the
    /// scan only when something needs it.
    pub fn from_str(s: &str) -> Self {
        RStringInner::from(
            SmallVec::from_slice(s.as_bytes()),
            Encoding::UTF8,
            CodeRange::Unknown,
        )
    }

    /// O(N): build a UTF-8 string and pre-scan it so `cr` is set to
    /// `SevenBit` (all bytes < 0x80) or `Valid`. Use for long-lived
    /// strings whose `cr` will be queried often or that are templates
    /// for many `deep_copy` clones — paying the scan once up front
    /// amortises across every later cr-needing operation.
    pub fn from_str_scanned(s: &str) -> Self {
        let cr = if s.is_ascii() {
            CodeRange::SevenBit
        } else {
            CodeRange::Valid
        };
        RStringInner::from(SmallVec::from_slice(s.as_bytes()), Encoding::UTF8, cr)
    }

    /// O(1) variant of `from_str_scanned` that consumes a `String`.
    /// See `from_str` for the distinction between scanning and lazy
    /// variants.
    pub fn from_string(s: String) -> Self {
        RStringInner::from(
            SmallVec::from_vec(s.into_bytes()),
            Encoding::UTF8,
            CodeRange::Unknown,
        )
    }

    /// O(N) variant of `from_string` that pre-scans for SevenBit.
    pub fn from_string_scanned(s: String) -> Self {
        let cr = if s.is_ascii() {
            CodeRange::SevenBit
        } else {
            CodeRange::Valid
        };
        RStringInner::from(SmallVec::from_vec(s.into_bytes()), Encoding::UTF8, cr)
    }

    pub fn bytes(slice: &[u8]) -> Self {
        RStringInner::from(
            SmallVec::from_slice(slice),
            Encoding::Ascii8,
            CodeRange::Unknown,
        )
    }

    pub fn bytes_from_vec(vec: Vec<u8>) -> Self {
        RStringInner::from(
            SmallVec::from_vec(vec),
            Encoding::Ascii8,
            CodeRange::Unknown,
        )
    }

    /// O(1): build a string with the given encoding without
    /// scanning. `cr = Unknown`; lazy-classify on first need.
    pub fn from_encoding(slice: &[u8], encoding: Encoding) -> Self {
        RStringInner::from(SmallVec::from_slice(slice), encoding, CodeRange::Unknown)
    }

    /// O(1): take ownership of an already-built byte buffer with a
    /// pre-computed `cr` (the caller tracked it while building — see
    /// `Array#join`).
    pub(crate) fn from_vec_cr(bytes: Vec<u8>, encoding: Encoding, cr: CodeRange) -> Self {
        RStringInner::from(SmallVec::from_vec(bytes), encoding, cr)
    }

    /// O(1): take ownership of a buffer that was built directly in the
    /// string's own representation — a short result never touched the
    /// heap, a long one is adopted without a copy (see
    /// `concatenate_string_inner`). `cr` was tracked by the builder.
    pub(crate) fn from_buf_cr(bytes: StringBuf, encoding: Encoding, cr: CodeRange) -> Self {
        RStringInner::from(bytes, encoding, cr)
    }

    /// O(N): build a string with the given encoding and pre-classify
    /// `cr` via `Encoding::classify`. Useful for source-byte
    /// literals (`"\xFF"`) and other long-lived inputs where the
    /// classification will outlive many clones.
    pub fn from_encoding_scanned(slice: &[u8], encoding: Encoding) -> Self {
        RStringInner::from(
            SmallVec::from_slice(slice),
            encoding,
            encoding.classify(slice),
        )
    }

    /// O(1): build a string whose bytes the caller guarantees are
    /// all 7-bit ASCII (< 0x80), recording `cr = SevenBit` directly
    /// and skipping the classification scan. The caller is
    /// responsible for the invariant — in debug builds this is
    /// checked. Used by byte-level fast paths (charset ops, case
    /// mapping, etc.) whose output is provably ASCII whenever every
    /// input was.
    pub fn from_ascii_bytes(bytes: SmallVec<[u8; STRING_INLINE_CAP]>, encoding: Encoding) -> Self {
        debug_assert!(
            bytes.iter().all(|b| *b < 0x80),
            "from_ascii_bytes: caller passed a byte >= 0x80"
        );
        debug_assert!(
            encoding.is_ascii_compatible(),
            "from_ascii_bytes: encoding must be ASCII-compatible"
        );
        RStringInner::from(bytes, encoding, CodeRange::SevenBit)
    }

    /// Build a substring of `parent` covering the byte range
    /// `start..end`, inheriting the parent's encoding and propagating
    /// the cached code range when the parent's classification implies
    /// the child's. Avoids the O(N) re-classification that
    /// `String#match` and friends would otherwise pay on every
    /// byteslice of an already-classified haystack.
    pub fn from_substring(parent: &RStringInner, start: usize, end: usize) -> Self {
        RStringInner::from(
            SmallVec::from_slice(&parent.as_bytes()[start..end]),
            parent.encoding(),
            Self::propagated_cr(parent, start, end),
        )
    }

    /// Owned, with the byte buffer spilled to the heap (so its address
    /// is stable and shareable).
    ///
    /// The bytes, encoding tag and code range of a literal template the
    /// JIT may instantiate inline: owned content the copy can hold in
    /// its own inline buffer, so there is no heap buffer to clone and
    /// nothing for [`share_string_buffer`] to convert.
    ///
    /// The encoding goes out whole: the JIT writes both its
    /// discriminant and its [`Encoding::payload`], so a variant
    /// carrying an index reproduces exactly (#1562).
    ///
    pub(crate) fn inline_copyable(&self) -> Option<(Vec<u8>, Encoding, u8)> {
        if self.content.is_shared() || self.owned_spilled() {
            return None;
        }
        Some((
            self.content.as_slice().to_vec(),
            self.ty,
            self.cr.get() as u8,
        ))
    }

    fn owned_spilled(&self) -> bool {
        // SAFETY: tag-discriminated; `owned` is the live variant.
        !self.content.is_shared() && unsafe { self.content.owned.spilled() }
    }

    /// Determine the child code range for a `start..end` byte-range
    /// view of `parent`. Returns `Unknown` when the parent's state
    /// doesn't imply the child's (e.g. UTF-8 + Valid where the cut
    /// could land in the middle of a multi-byte sequence, or
    /// encodings without a native decoder).
    fn propagated_cr(parent: &RStringInner, start: usize, end: usize) -> CodeRange {
        // Empty slice: trivially SevenBit (matches `classify` for
        // every encoding).
        if start == end {
            return CodeRange::SevenBit;
        }
        match parent.code_range() {
            // SevenBit: every byte is < 0x80, so any byte-range view
            // is still SevenBit, regardless of encoding or character
            // boundaries.
            CodeRange::SevenBit => CodeRange::SevenBit,
            CodeRange::Valid => match parent.encoding() {
                // Single-byte encodings: every byte position is a
                // character boundary; Valid trivially propagates.
                Encoding::Ascii8
                | Encoding::Iso8859(_)
                | Encoding::Other(_)
                | Encoding::NamedByte(_) => CodeRange::Valid,
                // UTF-8: the cut points must land on character
                // boundaries (a non-continuation byte or one-past-the-
                // end). When both endpoints align, the byte sequence
                // between them is still valid UTF-8.
                Encoding::Utf8(_) => {
                    if Self::is_utf8_char_boundary(parent, start)
                        && Self::is_utf8_char_boundary(parent, end)
                    {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                // UTF-16/UTF-32: code-unit width is fixed at 2/4
                // bytes. A byte slice that starts and ends on a code-
                // unit boundary is still well-formed under the same
                // "byte count parity" rule that `classify` uses.
                Encoding::Utf16Le | Encoding::Utf16Be => {
                    if start % 2 == 0 && end % 2 == 0 {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                Encoding::Utf32Le | Encoding::Utf32Be => {
                    if start % 4 == 0 && end % 4 == 0 {
                        CodeRange::Valid
                    } else {
                        CodeRange::Unknown
                    }
                }
                // No native multibyte decoder; defer to lazy
                // re-classify on first use. ISO-2022-JP joins this
                // bucket because the cut points might land inside
                // an ESC sequence — a sub-range that's syntactically
                // separate from the parent's escape state and would
                // need re-decoding to classify.
                Encoding::UsAscii | Encoding::EucJp(_) | Encoding::Sjis(_) | Encoding::Iso2022Jp => {
                    CodeRange::Unknown
                }
            },
            // Broken parents are never safe to propagate — a sub-
            // range could be Valid (if the broken bytes are outside
            // it) or still Broken.
            CodeRange::Broken | CodeRange::Unknown => CodeRange::Unknown,
        }
    }

    /// Whether byte index `i` is a UTF-8 character boundary. `0` and
    /// `content.len()` are always boundaries; otherwise the byte at
    /// `i` must not be a continuation byte (top two bits != `10`).
    fn is_utf8_char_boundary(parent: &RStringInner, i: usize) -> bool {
        i == 0 || i == parent.len() || (parent.as_bytes()[i] & 0xC0) != 0x80
    }

    /// O(N): build a string from arbitrary bytes with auto-detected
    /// encoding (UTF-8 if valid, else ASCII-8BIT) and pre-classified
    /// `cr`. Always scans — encoding detection requires walking the
    /// bytes, so we fold the SevenBit/Valid classification into the
    /// same pass.
    pub fn from_vec_scanned(vec: Vec<u8>) -> Self {
        let (enc, cr) = if vec.iter().all(|&b| b < 0x80) {
            (Encoding::UTF8, CodeRange::SevenBit)
        } else if std::str::from_utf8(&vec).is_ok() {
            (Encoding::UTF8, CodeRange::Valid)
        } else {
            // Ascii8 — every byte is "valid" under the binary tag.
            (Encoding::Ascii8, CodeRange::Valid)
        };
        RStringInner::from(SmallVec::from_vec(vec), enc, cr)
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.content.as_slice()
    }

    /// Make the buffer C-string compatible without changing the visible
    /// Ruby content: ensure capacity ≥ `len + 1` and write a NUL byte at
    /// `content[len]` (in spare capacity, *not* part of `as_bytes`).
    /// Returns a raw pointer to the start of the buffer.
    ///
    /// Used by the Fiddle / FFI bridge so a Ruby String passed where
    /// C expects `char*` / `void*` can be:
    ///   * read by `strlen`-style C code (the trailing NUL stops the scan
    ///     at `len`), and
    ///   * written into by `memcpy`-style C code (writes within the first
    ///     `len` bytes are visible to subsequent Ruby reads of the same
    ///     String, because we expose the actual backing buffer instead of
    ///     a copy).
    ///
    /// SAFETY: The pointer is valid until the next mutation of `self`
    /// (which may reallocate). `as_bytes`, `len`, `hash`, and equality
    /// of `self` are unchanged — the NUL lives in spare capacity.
    pub fn nul_terminated_buf_ptr(&mut self) -> *mut u8 {
        // C callees may write through the returned pointer, so a shared
        // string must be detached from its root first (copy-on-write).
        let buf = self.owned_mut();
        let len = buf.len();
        if buf.capacity() <= len {
            buf.reserve(1);
        }
        // SAFETY: capacity > len after the reserve above, so the byte at
        // offset `len` is owned spare capacity that we can write to.
        unsafe { buf.as_mut_ptr().add(len).write(0) };
        buf.as_mut_ptr()
    }

    pub fn set_byte(&mut self, index: usize, byte: u8) {
        self.owned_mut()[index] = byte;
        // Phase 1 lets a UTF-8-tagged buffer hold invalid bytes,
        // so we no longer downgrade the encoding tag here.
        //
        // Most setbyte calls poke ASCII bytes into already-SevenBit
        // strings; we can preserve the cached classification for that
        // common case to avoid an O(N) re-classify on the next
        // `valid_encoding?` / encoding-compat check. Anything else
        // falls back to lazy re-classify.
        let new_cr = match (self.cr.get(), byte) {
            (CodeRange::SevenBit, b) if b < 0x80 => CodeRange::SevenBit,
            _ => CodeRange::Unknown,
        };
        self.cr.set(new_cr);
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is out of range.
    ///
    pub fn conv_char_index(&self, char_pos: i64) -> Option<usize> {
        let len = self.char_length();
        if char_pos >= 0 {
            if char_pos <= len as i64 {
                Some(char_pos as usize)
            } else {
                None
            }
        } else {
            match len as i64 + char_pos {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
    }

    ///
    /// Convert `char_pos` to the true position in char of the string `self`.
    ///
    /// Return None if `i` is negative.
    ///
    pub fn conv_char_index2(&self, char_pos: i64) -> Option<usize> {
        let len = self.char_length();
        if char_pos >= 0 {
            Some(char_pos as usize)
        } else {
            match len as i64 + char_pos {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
    }

    pub fn byte_to_char_index(&self, byte_pos: usize) -> Result<usize> {
        for (i, (pos, _)) in self.check_utf8()?.char_indices().enumerate() {
            match pos {
                pos if pos == byte_pos => {
                    return Ok(i);
                }
                pos if pos > byte_pos => {
                    return Err(MonorubyErr::runtimeerr(format!(
                        "invalid byte position: {byte_pos}"
                    )));
                }
                _ => {}
            }
        }
        Err(MonorubyErr::runtimeerr(format!(
            "invalid byte position: {byte_pos}"
        )))
    }

    pub fn get_range(&self, index: usize, len: usize) -> std::ops::Range<usize> {
        // Walk the encoding-aware char iterator, accumulating the
        // byte offsets of chars `index` through `index+len`. Falls
        // back to byte indexing for fixed-1-byte encodings (which
        // is what `iter_char_bytes` would do anyway, but we
        // shortcut it for performance).
        let unit = match self.ty {
            // Emacs-Mule is a `NamedByte` encoding but not a byte-wide
            // one: it has a validator and a multibyte shape, so it has
            // to walk the iterator like EUC-JP does. Without this,
            // `s[1]` handed back the lead byte alone while `s.chars[1]`
            // gave the whole character.
            // Emacs-Mule and the CJK double-byte sets, likewise: a
            // `NamedByte` encoding with a walk is not byte-wide, and
            // indexing it by bytes handed back a lead byte alone where
            // `#chars` gave the whole character (#1473).
            Encoding::NamedByte(_) if mbc_walker(self.ty).is_some() => None,
            Encoding::Ascii8
            | Encoding::UsAscii
            | Encoding::Iso8859(_)
            | Encoding::Other(_)
            | Encoding::NamedByte(_) => Some(1),
            Encoding::Utf16Le | Encoding::Utf16Be => Some(2),
            Encoding::Utf32Le | Encoding::Utf32Be => Some(4),
            // ISO-2022-JP still iterates byte-wise (stateful decode
            // deferred), so the fixed-1-byte shortcut matches its
            // iterator.
            Encoding::Iso2022Jp => Some(1),
            // ASCII-only UTF-8: every character is a single byte, so
            // char index == byte index — take the O(1) offset path
            // instead of walking the scalar iterator. (Must precede the
            // generic UTF-8 arm below.)
            Encoding::Utf8(_) if matches!(self.code_range(), CodeRange::SevenBit) => Some(1),
            // EUC-JP / Shift_JIS are variable-width: walk the
            // (now encoding-aware) char iterator so `String#[]` /
            // `#slice` index by characters, not bytes.
            Encoding::EucJp(_) | Encoding::Sjis(_) | Encoding::Utf8(_) => None,

        };
        if let Some(u) = unit {
            let total = self.len();
            let start_byte = (index * u).min(total);
            let end_byte = ((index + len) * u).min(total);
            return start_byte..end_byte;
        }
        // UTF-8 path: walk per scalar (or per broken byte) so
        // `String#[char_index]` indexes by *characters*, not bytes.
        let mut start_byte: Option<usize> = None;
        let mut end_byte = self.len();
        let mut byte_pos = 0usize;
        for (i, c) in self.iter_char_bytes().enumerate() {
            if i == index {
                start_byte = Some(byte_pos);
                if len == 0 {
                    end_byte = byte_pos;
                    break;
                }
            }
            byte_pos += c.len();
            if start_byte.is_some() && i + 1 == index + len {
                end_byte = byte_pos;
                break;
            }
        }
        match start_byte {
            // `index` past the end but exactly equal to char count
            // is the "append point" — empty range at end.
            None if index == self.char_length() => self.len()..self.len(),
            None => 0..0,
            Some(s) => s..end_byte,
        }
    }

    /// Append `other`'s bytes to `self`, raising
    /// `Encoding::CompatibilityError` (rather than `RuntimeError`)
    /// on incompatible encodings. Used by call sites that have
    /// access to `Store` — preferred for new code; the older
    /// `extend()` is kept for the call sites that don't.
    pub fn extend(&mut self, other: &Self, store: &Store) -> Result<()> {
        if other.is_empty() {
            return Ok(());
        }
        let result_enc = self
            .compatible_encoding(other)
            .ok_or_else(|| MonorubyErr::incompatible_encoding(store, self.ty, other.ty))?;
        // Snapshot (possibly cached) classifications BEFORE extending the
        // buffer and fold them into a combined cr in O(1). Previously every
        // append wiped self.cr to Unknown, which forced the *next*
        // extend's compatible_encoding() call to re-classify the
        // entire growing buffer -- turning N appends from O(N) into O(N²).
        // (See String#<< micro-bench: a 100k-iter `s << "abcde"` loop took
        // ~5.7s before this change vs 5.6ms in CRuby.)
        // Classify the (short, new) piece rather than reading its raw
        // `cr`: on the common equal-encoding path `compatible_encoding`
        // never touched it, and folding an Unknown piece would degrade
        // `self.cr` to Unknown — forcing a later full re-scan of the
        // whole accumulated buffer (or an O(len) `size` on it). The
        // piece scan is amortized O(total appended bytes), and its
        // result is cached in the piece itself.
        let new_cr = match (self.cr.get(), other.code_range()) {
            (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
            (CodeRange::SevenBit | CodeRange::Valid, CodeRange::SevenBit | CodeRange::Valid) => {
                // compatible_encoding already verified the encodings are
                // compatible (equal, or the non-winning side is 7-bit),
                // so two well-formed pieces concatenate to a well-formed
                // whole. (Multi-byte boundaries can only collide at the
                // junction if one side was already Broken.)
                CodeRange::Valid
            }
            // The accumulated side is Broken/never-computed, or the
            // piece is Broken -- fall back to lazy re-classify.
            _ => CodeRange::Unknown,
        };
        self.owned_mut().extend_from_slice(other.as_bytes());
        self.ty = result_enc;
        self.cr.set(new_cr);
        Ok(())
    }

    pub fn extend_from_slice_checked(&mut self, slice: &[u8]) -> Result<()> {
        if self.ty.is_utf8_compatible() && std::str::from_utf8(slice).is_err() {
            return Err(MonorubyErr::runtimeerr(format!(
                "invalid byte sequence: {:?}",
                slice
            )));
        }
        // We just verified `slice` validates under self.ty when
        // utf8-compatible, so its cr is at worst Valid; if every byte is
        // < 0x80 it's still SevenBit. Combine with self.cr in O(1) so
        // repeated callers (e.g. String#<< with single-char Integer args
        // or per-codepoint encode loops) don't degenerate to O(N²) when
        // self.code_range() is later queried.
        let slice_cr = if slice.iter().all(|&b| b < 0x80) {
            CodeRange::SevenBit
        } else if self.ty.is_utf8_compatible() {
            CodeRange::Valid
        } else {
            CodeRange::Unknown
        };
        let new_cr = match (self.cr.get(), slice_cr) {
            (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
            (CodeRange::SevenBit | CodeRange::Valid, CodeRange::SevenBit | CodeRange::Valid) => {
                CodeRange::Valid
            }
            _ => CodeRange::Unknown,
        };
        self.owned_mut().extend_from_slice(slice);
        self.cr.set(new_cr);
        Ok(())
    }

    /// Append raw bytes without any encoding validation. Used by
    /// `String#append_as_bytes` where deliberately producing
    /// "broken" sequences in the receiver's encoding is permitted.
    pub fn extend_from_slice_no_validate(&mut self, slice: &[u8]) {
        self.owned_mut().extend_from_slice(slice);
        self.cr.set(CodeRange::Unknown);
    }

    /// Append raw bytes without validation, merging the cached code range
    /// in O(`slice`): the slice is classified standalone under the
    /// receiver's encoding and combined with the cached cr, so hot append
    /// loops (e.g. `Array#pack` with `buffer:`) stay linear and a later
    /// `code_range()` query doesn't rescan the whole string. Unlike
    /// `extend_from_slice_checked`, bytes invalid in the receiver's
    /// encoding are permitted — the cr just degrades to Unknown (a later
    /// query classifies to Broken), matching pack's semantics of writing
    /// arbitrary binary into a text-tagged buffer.
    pub fn extend_from_slice_merge_cr(&mut self, slice: &[u8]) {
        if slice.is_empty() {
            return;
        }
        let slice_cr = if self.ty.is_ascii_compatible() && slice.iter().all(|&b| b < 0x80) {
            CodeRange::SevenBit
        } else {
            self.ty.classify(slice)
        };
        // Both sides classifying SevenBit/Valid standalone makes the
        // concatenation Valid: a SevenBit/Valid receiver ends on a
        // complete character, and the appended part is a well-formed
        // sequence from its first byte. Anything else (either side
        // Broken/Unknown) is left Unknown for lazy reclassification.
        let new_cr = match (self.cr.get(), slice_cr) {
            (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
            (CodeRange::SevenBit | CodeRange::Valid, CodeRange::SevenBit | CodeRange::Valid) => {
                CodeRange::Valid
            }
            _ => CodeRange::Unknown,
        };
        self.owned_mut().extend_from_slice(slice);
        self.cr.set(new_cr);
    }

    pub fn repeat(&self, len: usize) -> RStringInner {
        // Repetition of a classified buffer trivially preserves cr:
        // SevenBit/Valid stay so under concatenation of identical
        // valid runs, and Broken stays Broken. The empty case
        // (`len == 0`) collapses to SevenBit, matching `classify(b"")`.
        let cr = if len == 0 {
            CodeRange::SevenBit
        } else {
            self.cr.get()
        };
        RStringInner::from(SmallVec::from_vec(self.as_bytes().repeat(len)), self.ty, cr)
    }

    /// Apply a set of non-overlapping replacements to the haystack
    /// `bytes` (in encoding `given_enc`; `given_7bit` says it is
    /// ASCII-only) in a single forward pass, producing a fresh
    /// `RStringInner`. `replacements` must be sorted by start position
    /// and non-overlapping — `gsub`/`sub` collect them that way.
    ///
    /// This is O(`given` + Σ replacement) instead of the
    /// O(`given` · matches) you get from applying each replacement with
    /// an individual buffer-shifting `bytesplice_with` (which `copy_within`s
    /// the tail every time — quadratic in the match count). Each
    /// replacement is still encoding-compatibility-checked, raising
    /// `Encoding::CompatibilityError` where the pieces cannot share an
    /// encoding. The result's code range is lazy.
    pub fn splice_all(
        store: &Store,
        bytes: &[u8],
        given_enc: Encoding,
        given_7bit: bool,
        replacements: &[(std::ops::Range<usize>, RStringInner)],
    ) -> Result<RStringInner> {
        // Final length is non-negative (ranges are within `given` and
        // non-overlapping), but the running sum can dip, so compute in
        // signed space.
        let cap = (bytes.len() as i64
            + replacements
                .iter()
                .map(|(r, rep)| rep.len() as i64 - (r.end - r.start) as i64)
                .sum::<i64>())
        .max(0) as usize;
        let mut buf: SmallVec<[u8; STRING_INLINE_CAP]> = SmallVec::with_capacity(cap);
        let mut last = 0usize;
        // The result's encoding, as CRuby's `rb_enc_cr_str_buf_cat`
        // settles it while appending piece by piece (a stretch of the
        // haystack, a replacement): 7-bit pieces never change it; the
        // first piece with non-ASCII content decides it — so
        // `"a-b".gsub(/-/, "\xff".b)` is BINARY, while `"aéb"` stays
        // UTF-8 whatever 7-bit replacement it takes — and a later
        // non-ASCII piece in another encoding cannot fit
        // (`Encoding::CompatibilityError`).
        let mut enc = given_enc;
        let mut seven_bit = true;
        let mut append = |piece: &[u8], piece_enc: Encoding, piece_7bit: bool| -> Result<()> {
            if !piece_7bit {
                if seven_bit {
                    enc = piece_enc;
                    seven_bit = false;
                } else if enc != piece_enc {
                    return Err(MonorubyErr::incompatible_encoding(store, enc, piece_enc));
                }
            }
            buf.extend_from_slice(piece);
            Ok(())
        };
        for (r, rep) in replacements {
            // A replacement in an ASCII-incompatible encoding (UTF-16,
            // ...) never fits an ASCII-compatible haystack, 7-bit or not,
            // unless it is empty.
            let rep_enc = rep.encoding();
            if !rep.is_empty()
                && rep_enc != given_enc
                && !(rep_enc.is_ascii_compatible() && given_enc.is_ascii_compatible())
            {
                return Err(MonorubyErr::incompatible_encoding(store, given_enc, rep_enc));
            }
            let stretch = &bytes[last..r.start];
            append(stretch, given_enc, given_7bit || stretch.is_ascii())?;
            append(rep.as_bytes(), rep_enc, rep.is_ascii_only())?;
            last = r.end;
        }
        let tail = &bytes[last..];
        append(tail, given_enc, given_7bit || tail.is_ascii())?;
        Ok(RStringInner::from(buf, enc, CodeRange::Unknown))
    }

    /// Mutate `self.content` in place: replace bytes `start..end` with
    /// `replacement`. Encoding tag and `cr` are *not* touched — the
    /// caller is responsible for re-classifying or otherwise updating
    /// them. The shared low-level shuffling used by `bytesplice_with`.
    fn splice_bytes(&mut self, start: usize, end: usize, replacement: &[u8]) {
        let buf = self.owned_mut();
        debug_assert!(start <= end);
        debug_assert!(end <= buf.len());
        let len = end - start;
        let new_len = buf.len() - len + replacement.len();
        if replacement.len() > len {
            // Need to grow: extend first, then shift the tail right.
            let extra = replacement.len() - len;
            buf.resize(new_len, 0);
            buf.copy_within(end..new_len - extra, end + extra);
        } else if replacement.len() < len {
            // Shrink: shift the tail left, then truncate.
            let shrink = len - replacement.len();
            let old_len = buf.len();
            buf.copy_within(end..old_len, end - shrink);
            buf.truncate(new_len);
        }
        buf[start..start + replacement.len()].copy_from_slice(replacement);
    }

    /// Replace byte range `start..start+len` with the bytes of
    /// `replacement`, taking the replacement's encoding and cached
    /// code range into account. Returns
    /// `Encoding::CompatibilityError` (rather than ignoring the
    /// mismatch the way `bytesplice(&[u8])` does) when the receiver
    /// and `replacement` carry incompatible encodings.
    ///
    /// The cached `code_range()` of `replacement` lets us short-cut
    /// the post-mutation classification: when both sides are
    /// SevenBit/Valid and the splice falls on UTF-8 character
    /// boundaries we set the result cr in O(1) without re-walking
    /// the whole buffer. A broken result keeps the encoding
    /// `compatible_encoding` negotiated, as CRuby's does, and is simply
    /// classified Broken.
    pub fn bytesplice_with(
        &mut self,
        start: usize,
        len: usize,
        replacement: &RStringInner,
        store: &Store,
    ) -> Result<()> {
        let result_enc = self
            .compatible_encoding(replacement)
            .ok_or_else(|| MonorubyErr::incompatible_encoding(store, self.ty, replacement.ty))?;

        let end = start + len;
        let prev_cr = self.cr.get();
        let prev_ty = self.ty;
        let repl_cr = replacement.code_range();
        let repl_bytes = replacement.as_bytes();

        // Test BEFORE we mutate: if the receiver is valid UTF-8 and
        // both splice endpoints fall on character boundaries, the
        // prefix-free property of UTF-8 lets the receiver-side stay
        // valid as long as the replacement is itself valid (which
        // its cached cr tells us in O(1)).
        let utf8_boundaries_ok = matches!(prev_ty, Encoding::Utf8(_))
            && matches!(prev_cr, CodeRange::Valid | CodeRange::SevenBit)
            && is_utf8_char_boundary(self.as_bytes(), start)
            && is_utf8_char_boundary(self.as_bytes(), end);

        self.splice_bytes(start, end, repl_bytes);
        self.ty = result_enc;

        // Fast path 1: SevenBit + ASCII-compatible encoding +
        // ASCII-only replacement (its cr already says SevenBit).
        if matches!(prev_cr, CodeRange::SevenBit)
            && matches!(repl_cr, CodeRange::SevenBit)
            && result_enc.is_ascii_compatible()
        {
            self.cr.set(CodeRange::SevenBit);
            return Ok(());
        }
        // Fast path 2: receiver and replacement are both well-formed
        // UTF-8 (or pure ASCII) and the splice respects character
        // boundaries — the result is well-formed UTF-8 too.
        if utf8_boundaries_ok && matches!(repl_cr, CodeRange::SevenBit | CodeRange::Valid) {
            self.cr.set(CodeRange::Valid);
            return Ok(());
        }
        // Slow path: re-classify the whole buffer. Caching the
        // result keeps a chain of in-place splices O(N) rather than
        // O(N²).
        // The encoding is whatever `compatible_encoding` negotiated, and a
        // broken result does not change it: CRuby leaves the string under
        // its own tag and lets `valid_encoding?` report false. Splicing
        // ASCII into an already-broken UTF-8 string keeps it UTF-8 there,
        // where re-tagging it ASCII-8BIT would also claim it valid.
        self.cr.set(self.ty.classify(self.as_bytes()));
        Ok(())
    }

    /// Build an empty `RStringInner` tagged with the given encoding
    /// and pre-allocated for `cap` bytes. Empty content is always
    /// SevenBit (the empty byte sequence is trivially ASCII), so
    /// callers can append into the result without paying for an
    /// initial classification.
    pub fn with_encoding_capacity(encoding: Encoding, cap: usize) -> Self {
        RStringInner::from(SmallVec::with_capacity(cap), encoding, CodeRange::SevenBit)
    }

    pub fn first_code(&self) -> Result<u32> {
        if self.len() == 0 {
            return Err(MonorubyErr::argumenterr("empty string"));
        }
        let bytes = self.as_bytes();
        let broken = || {
            MonorubyErr::argumenterr(format!("invalid byte sequence in {}", self.ty.name()))
        };
        // CRuby's `rb_enc_codepoint_len` looks at the *first* character
        // only: `"a\xff"` still ords to 97, while a receiver whose
        // first character is broken for its declared encoding raises —
        // including `"\u00a9"` bytes tagged US-ASCII, which are perfectly
        // good UTF-8 and still not US-ASCII.
        let ord = match self.ty {
            Encoding::UsAscii => {
                if bytes[0] >= 0x80 {
                    return Err(broken());
                }
                bytes[0] as u32
            }
            Encoding::Utf8(_) => {
                let head = &bytes[..bytes.len().min(4)];
                match std::str::from_utf8(head) {
                    Ok(s) => s.chars().next().unwrap() as u32,
                    // A trailing truncation is the next character's, not
                    // the first one's: decode the valid prefix instead.
                    Err(e) if e.valid_up_to() > 0 => {
                        // SAFETY: `valid_up_to` bounds a valid UTF-8 prefix.
                        unsafe { std::str::from_utf8_unchecked(&head[..e.valid_up_to()]) }
                            .chars()
                            .next()
                            .unwrap() as u32
                    }
                    Err(_) => return Err(broken()),
                }
            }
            _ => {
                let first = self.iter_char_bytes().next().unwrap_or(&[]);
                char_bytes_code(self.ty, first)
            }
        };
        Ok(ord)
    }
}

/// The code point one character's bytes stand for in `enc`. The
/// fixed-width UTF forms are decoded (a `"\n"` in UTF-32BE is four
/// bytes, and its ordinal is still 10); every other non-UTF-8 encoding
/// reports the leading byte, which is what CRuby answers for
/// ASCII-8BIT and the conservative answer for the dummy encodings
/// monoruby does not decode.
pub fn char_bytes_code(enc: Encoding, bytes: &[u8]) -> u32 {
    let unit16 = |hi: u8, lo: u8| ((hi as u32) << 8) | lo as u32;
    match enc {
        Encoding::Utf16Be | Encoding::Utf16Le if bytes.len() >= 2 => {
            let be = enc == Encoding::Utf16Be;
            let first = if be {
                unit16(bytes[0], bytes[1])
            } else {
                unit16(bytes[1], bytes[0])
            };
            if bytes.len() >= 4 && (0xD800..0xDC00).contains(&first) {
                let second = if be {
                    unit16(bytes[2], bytes[3])
                } else {
                    unit16(bytes[3], bytes[2])
                };
                if (0xDC00..0xE000).contains(&second) {
                    return 0x10000 + ((first - 0xD800) << 10) + (second - 0xDC00);
                }
            }
            first
        }
        Encoding::Utf32Be if bytes.len() >= 4 => {
            u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
        }
        Encoding::Utf32Le if bytes.len() >= 4 => {
            u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
        }
        _ => bytes.first().copied().unwrap_or(0) as u32,
    }
}

///
/// Substring `start..end` (byte offsets) of the String `parent`,
/// returned as a new String `Value`.
///
/// When the slice is long enough and the parent's buffer lives on the
/// heap, the result is a zero-copy *shared* view (CRuby-style shared
/// substring): the parent's buffer is moved to a hidden frozen root
/// (or the parent itself serves as the root when it is frozen, or is
/// already a sharer), and both strings become copy-on-write views of
/// it. Otherwise this is an ordinary O(len) copy.
///
pub(crate) fn string_substring(mut parent: Value, start: usize, end: usize) -> Value {
    let inner = parent.as_rstring_inner();
    debug_assert!(start <= end && end <= inner.len());
    let len = end - start;
    let ty = inner.encoding();
    let cr = RStringInner::propagated_cr(inner, start, end);
    // Share only when the copy would have to heap-allocate anyway
    // (an inline copy is at most STRING_INLINE_CAP bytes and cheaper
    // than a root allocation + GC edge).
    if len > STRING_INLINE_CAP && (inner.is_shared() || inner.owned_spilled()) {
        let (root, base) = ensure_shared_root(&mut parent);
        // SAFETY: `base..base+parent.len()` is a live range of the
        // root's heap buffer and `start..end` is within it.
        let ptr = unsafe { base.add(start) };
        Value::string_from_inner(RStringInner::from_shared(root, ptr, len, ty, cr))
    } else {
        Value::string_from_inner(RStringInner::from(
            SmallVec::from_slice(&parent.as_rstring_inner().as_bytes()[start..end]),
            ty,
            cr,
        ))
    }
}

///
/// Return a frozen String holding a stable snapshot of `receiver`'s
/// current bytes — a buffer that never changes, so callers can borrow
/// its `&str` and slice zero-copy [`string_substring`] views from it
/// while user code (e.g. a block) may concurrently mutate `receiver`.
///
/// * frozen receiver        → itself (already immutable).
/// * shared / heap-spilled  → a frozen shared root of its buffer
///   (`receiver` becomes a CoW sharer of the same root).
/// * small inline receiver  → a frozen clone (cheap; avoids promoting
///   the receiver to a sharer for a buffer that views would copy anyway).
pub(crate) fn string_snapshot(mut receiver: Value) -> Value {
    if receiver.is_frozen() {
        return receiver;
    }
    let shareable = {
        let inner = receiver.as_rstring_inner();
        inner.is_shared() || inner.owned_spilled()
    };
    if shareable {
        let (root, base) = ensure_shared_root(&mut receiver);
        let view_len = receiver.as_rstring_inner().len();
        let root_inner = root.as_rstring_inner();
        if base == root_inner.as_ptr() && view_len == root_inner.len() {
            root
        } else {
            // The receiver is a sharer viewing a sub-range of the
            // root's buffer (e.g. a CoW substring handed out by a
            // MatchData accessor). The snapshot must present exactly
            // that window — returning the bare root would hand callers
            // the whole original buffer.
            let start = base as usize - root_inner.as_ptr() as usize;
            let mut snap = string_substring(root, start, start + view_len);
            snap.set_frozen();
            snap
        }
    } else {
        let mut dup = Value::string_from_inner(receiver.as_rstring_inner().clone());
        dup.set_frozen();
        dup
    }
}

/// CRuby's `str_mod_check`: raise `RuntimeError: "string modified"` when
/// the receiver's byte length changed during a block iteration
/// (`String#scan` / `#gsub` with a block or hash). Length is the only
/// signal that matches CRuby here — monoruby reallocates the buffer on
/// in-place same-length edits (copy-on-write detach), so a pointer
/// comparison would over-trigger on mutations CRuby treats as in place.
pub(crate) fn check_string_not_modified(recv: Value, expected_len: usize) -> Result<()> {
    if recv.as_rstring_inner().len() != expected_len {
        Err(MonorubyErr::runtimeerr("string modified"))
    } else {
        Ok(())
    }
}

///
/// Make `parent`'s byte buffer shareable and return `(root, base)`:
/// the (frozen, hidden) String owning the buffer and the address of
/// `parent`'s first byte within it.
///
/// * already a sharer  → its existing root.
/// * frozen            → `parent` is its own root (the buffer can
///   never be reallocated; chilled strings are mutable-with-warning
///   and do NOT qualify).
/// * mutable + spilled → move the buffer into a fresh hidden frozen
///   root and turn `parent` into the first sharer; later mutations of
///   `parent` copy-on-write via `owned_mut`.
///
/// The caller must guarantee `parent` is a spilled-or-shared String.
///
/// Convert a heap-spilled string into a copy-on-write sharer of a
/// hidden frozen root (a no-op for inline-stored content, an existing
/// sharer, or a frozen string), so subsequent `clone`s of its inner are
/// O(1) view copies. Used on string-literal templates: every
/// per-execution `deep_copy` then shares the template's buffer — and
/// its cached code range — instead of copying the bytes. Mutating any
/// copy un-shares it through the ordinary CoW machinery.
pub(crate) fn share_string_buffer(v: &mut Value) {
    let Some(inner) = v.is_rstring_inner() else {
        return;
    };
    if inner.content.is_shared() || v.is_frozen() || inner.owned_spilled() {
        let _ = ensure_shared_root(v);
    }
}

fn ensure_shared_root(parent: &mut Value) -> (Value, *const u8) {
    {
        let inner = parent.as_rstring_inner();
        if inner.content.is_shared() {
            // SAFETY: tag-discriminated.
            let sc = unsafe { inner.content.shared };
            return (sc.root, sc.ptr);
        }
        if parent.is_frozen() {
            return (*parent, inner.as_ptr());
        }
    }
    // Move the heap buffer out of `parent` into a hidden root.
    // `parent` is left briefly as a valid empty string: if allocating
    // the root triggers GC, every object is in a consistent state (the
    // buffer itself is owned by the local `buf`, unreachable by GC and
    // therefore safe).
    let (buf, ty, cr) = {
        let inner = parent.as_rstring_inner_mut();
        debug_assert!(inner.owned_spilled());
        // SAFETY: tag-discriminated; `owned` is the live variant.
        let buf = std::mem::take(unsafe { &mut *inner.content.owned });
        (buf, inner.ty, inner.cr.get())
    };
    let mut root = Value::string_from_inner(RStringInner::from(buf, ty, cr));
    root.set_frozen();
    let root_inner = root.as_rstring_inner();
    let (ptr, len) = (root_inner.as_ptr(), root_inner.len());
    // Overwriting the (empty, heap-free) owned content with the shared
    // variant leaks nothing: an un-spilled SmallVec owns no allocation.
    parent.as_rstring_inner_mut().content = StringContent {
        shared: SharedContent {
            tag: STRING_SHARED_TAG,
            ptr,
            len,
            root,
        },
    };
    // `parent` now holds an edge to the freshly allocated (young) `root`.
    // If `parent` is already old, this is an un-barriered old→young store:
    // a later minor GC would reclaim `root` while `parent` still views its
    // buffer, and a subsequent mark of `parent` would walk the freed root
    // ("Dead object"). Record the edge — this (and a sharer's clone) is the
    // only path that gives a String an outgoing reference, mirrored by
    // `young_child_exists`/`is_promotable`.
    parent.write_barrier(root);
    (root, ptr)
}

#[cfg(test)]
mod encoding_tests {
    use super::*;

    /// The JIT's inline `String#<<` compares the encoding tag against a
    /// numeric bound, so the three encodings a raw byte append is sound
    /// in must stay the *first* three variants, in this order. Adding a
    /// variant among them without moving the bound would let a UTF-16
    /// receiver take the byte path.
    #[test]
    fn inline_shl_encoding_tags_are_pinned() {
        assert_eq!(0, Encoding::Ascii8.tag());
        assert_eq!(1, Encoding::UTF8.tag());
        assert_eq!(2, Encoding::UsAscii.tag());
        assert_eq!(STRING_TY_MAX_INLINE_SHL, Encoding::UsAscii.tag());
        // Everything the fast path must refuse sorts above the bound.
        for enc in [
            Encoding::Utf16Le,
            Encoding::Utf16Be,
            Encoding::Utf32Le,
            Encoding::Utf32Be,
            Encoding::Iso8859(1),
            Encoding::EUC_JP,
            Encoding::Sjis(0),
            Encoding::Iso2022Jp,
            Encoding::Other(0),
            Encoding::NamedByte(0),
        ] {
            assert!(enc.tag() > STRING_TY_MAX_INLINE_SHL, "{enc:?}");
        }
    }

    #[test]
    fn cesu8_round_trips_every_codepoint() {
        // Read off CRuby over the whole codepoint space when the walk
        // was written; this keeps the two halves consistent with each
        // other, which is what a conversion round trip needs.
        let mut checked = 0;
        for cp in 0..=0x10FFFFu32 {
            let Some(c) = char::from_u32(cp) else { continue };
            let s = c.to_string();
            let bytes = utf8_to_cesu8(&s);
            // A supplementary character is six bytes and one character;
            // everything below `U+10000` is UTF-8's own bytes.
            if cp >= 0x10000 {
                assert_eq!(bytes.len(), 6, "U+{cp:04X}");
            } else {
                assert_eq!(bytes.as_slice(), s.as_bytes(), "U+{cp:04X}");
            }
            assert_eq!(cesu8_precise_len(&bytes, 0), PreciseLen::Char(bytes.len()));
            assert_eq!(cesu8_to_utf8(&bytes).as_deref(), Some(s.as_str()), "U+{cp:04X}");
            checked += 1;
        }
        assert!(checked > 1_000_000, "checked {checked}");
    }

    #[test]
    fn cesu8_refuses_what_utf8_spells_differently() {
        // The two differences from UTF-8, both ways round.
        let emoji = "\u{1F600}";
        assert_eq!(
            utf8_to_cesu8(emoji),
            vec![0xed, 0xa0, 0xbd, 0xed, 0xb8, 0x80]
        );
        // Its four-byte UTF-8 form starts no character here.
        assert_eq!(cesu8_precise_len(emoji.as_bytes(), 0), PreciseLen::Invalid);
        assert_eq!(cesu8_to_utf8(emoji.as_bytes()), None);
        // A lone surrogate half is a prefix, not a character...
        assert_eq!(cesu8_precise_len(&[0xed, 0xa0, 0xbd], 0), PreciseLen::NeedMore);
        // ...and a low half on its own starts nothing.
        assert_eq!(cesu8_precise_len(&[0xed, 0xb0, 0x80], 0), PreciseLen::Invalid);
        // `U+D7FF` sits just below the surrogates and is an ordinary
        // three-byte character.
        assert_eq!(cesu8_precise_len(&[0xed, 0x9f, 0xbf], 0), PreciseLen::Char(3));
        // The ill-formed run the walk reports is the well-formed
        // prefix, which is what CRuby's error messages quote.
        let runs = |bytes: &[u8]| {
            let mut out: Vec<Vec<u8>> = vec![];
            let _ = walk_mbc(bytes, CESU8_MAX_LEN, cesu8_precise_len, |piece| {
                if let MbcPiece::Bad(b) = piece {
                    out.push(b.to_vec());
                }
                Ok(())
            });
            out
        };
        assert_eq!(
            runs(&[0x41, 0xf0, 0x9f, 0x98, 0x80, 0x42]),
            vec![vec![0xf0], vec![0x9f], vec![0x98], vec![0x80]]
        );
        assert_eq!(runs(&[0x41, 0xed, 0xa0, 0xbd, 0x42]), vec![vec![0xed, 0xa0, 0xbd]]);
        assert_eq!(
            runs(&[0x41, 0xed, 0xa0, 0xbd, 0xed, 0x9f, 0xbf, 0x42]),
            vec![vec![0xed, 0xa0, 0xbd, 0xed], vec![0x9f], vec![0xbf]]
        );
    }

    #[test]
    fn utf8_mac_agrees_with_cruby_everywhere() {
        // The table was derived from CRuby; this re-derives CRuby's
        // answer for all 0x110000 codepoints and compares, so a change
        // to `unicode-normalization`'s tables cannot drift silently.
        let script = r#"
            require "json"
            out = {}
            (0..0x10FFFF).each do |cp|
              next if (0xD800..0xDFFF).cover?(cp)
              s = begin; [cp].pack("U"); rescue; next; end
              t = begin; s.encode("UTF8-MAC").force_encoding("UTF-8"); rescue; next; end
              out[cp] = t.codepoints if t.codepoints != [cp]
            end
            print JSON.dump(out)
        "#;
        let Ok(out) = std::process::Command::new("ruby").arg("-e").arg(script).output() else {
            eprintln!("no ruby on PATH; skipping");
            return;
        };
        if !out.status.success() {
            eprintln!("ruby failed; skipping");
            return;
        }
        let json = String::from_utf8(out.stdout).unwrap();
        // {"cp":[a,b,..],...} — parsed by hand rather than pulling in
        // serde for one test.
        let mut expected: std::collections::HashMap<u32, String> =
            std::collections::HashMap::new();
        for entry in json.trim_matches(|c| c == '{' || c == '}').split("],") {
            let Some((k, v)) = entry.split_once(":[") else { continue };
            let cp: u32 = k.trim_matches('"').parse().unwrap();
            let s: String = v
                .trim_end_matches(']')
                .split(',')
                .map(|n| char::from_u32(n.trim().parse().unwrap()).unwrap())
                .collect();
            expected.insert(cp, s);
        }
        let mut checked = 0;
        for cp in 0..=0x10FFFFu32 {
            let Some(c) = char::from_u32(cp) else { continue };
            let s = c.to_string();
            let want = expected.get(&cp).cloned().unwrap_or_else(|| s.clone());
            assert_eq!(utf8_to_mac(&s), want, "U+{cp:04X}");
            checked += 1;
        }
        assert!(checked > 1_000_000, "checked {checked}");
    }

    #[test]
    fn utf8_mac_round_trips_every_codepoint() {
        // Checked against CRuby over all 0x110000 codepoints when the
        // table was derived; this keeps the two halves consistent with
        // each other, which is what a conversion round trip needs.
        for cp in 0..=0x10FFFFu32 {
            let Some(c) = char::from_u32(cp) else { continue };
            let s = c.to_string();
            let mac = utf8_to_mac(&s);
            // Decomposing an already-decomposed string is a no-op.
            assert_eq!(utf8_to_mac(&mac), mac, "U+{cp:04X}");
            // And composing it back returns the composed form, which
            // for anything the table keeps composed is the input.
            if utf8_mac_keeps_composed(c) {
                assert_eq!(mac, s, "U+{cp:04X} must not decompose");
                assert_eq!(mac_to_utf8(&mac), s, "U+{cp:04X}");
            }
        }
    }

    #[test]
    fn utf8_mac_matches_the_documented_cases() {
        // The four shapes that separate this from plain NFD/NFC.
        let ga = "\u{304C}";
        assert_eq!(utf8_to_mac(ga), "\u{304B}\u{3099}");
        assert_eq!(mac_to_utf8("\u{304B}\u{3099}"), ga);
        // A singleton the table keeps: ANGSTROM stays put, where NFC
        // would answer U+00C5.
        assert_eq!(utf8_to_mac("\u{212B}"), "\u{212B}");
        assert_eq!(mac_to_utf8("\u{212B}"), "\u{212B}");
        // ...while the decomposed form of the same letter composes.
        assert_eq!(mac_to_utf8("A\u{030A}"), "\u{00C5}");
        // A CJK compatibility ideograph is left alone both ways.
        assert_eq!(utf8_to_mac("\u{F900}"), "\u{F900}");
        assert_eq!(mac_to_utf8("\u{F900}"), "\u{F900}");
        // A compatibility ligature is not a canonical decomposition, so
        // neither direction touches it.
        assert_eq!(utf8_to_mac("\u{FB01}"), "\u{FB01}");
        // Two marks on one base come back in canonical order.
        assert_eq!(utf8_to_mac("\u{1E69}"), "s\u{0323}\u{0307}");
        assert_eq!(mac_to_utf8("s\u{0307}\u{0323}"), "\u{1E69}");
    }

    /// The JIT addresses an `RStringInner`'s encoding as two bytes —
    /// the discriminant and [`Encoding::payload`] — and its code range
    /// as the byte after them. Nothing in the language pins that
    /// layout, so pin it here: a `cr` that moved onto the payload byte
    /// would have the inline string literal write the code range and
    /// then stamp the encoding index over it.
    #[test]
    fn the_encoding_payload_byte_sits_between_the_tag_and_the_code_range() {
        assert_eq!(STRING_TY_PAYLOAD_OFFSET, STRING_TY_OFFSET + 1);
        assert_eq!(STRING_CR_OFFSET, STRING_TY_PAYLOAD_OFFSET + 1);
        assert_eq!(std::mem::size_of::<Encoding>(), 2);
        assert_eq!(std::mem::align_of::<Encoding>(), 1);
    }

    /// `Encoding::payload` has to name every variant that carries an
    /// index: one left out would read back as `0` and silently become
    /// the canonical member of its family.
    #[test]
    fn every_payload_carrying_variant_reports_its_index() {
        for enc in [
            Encoding::Utf8(1),
            Encoding::Iso8859(9),
            Encoding::EucJp(2),
            Encoding::Sjis(1),
            Encoding::Other(3),
            Encoding::NamedByte(CESU_8),
        ] {
            // SAFETY: `Encoding` is `#[repr(u8)]` with a `u8` field, so
            // the second byte of a payload-carrying variant is that
            // field — which is the byte the JIT reads.
            let byte = unsafe { *(&enc as *const Encoding as *const u8).add(1) };
            assert_eq!(enc.payload(), byte, "{enc:?}");
            assert_ne!(enc.payload(), 0, "{enc:?}");
        }
        for enc in [
            Encoding::Ascii8,
            Encoding::UsAscii,
            Encoding::Utf16Le,
            Encoding::Utf16Be,
            Encoding::Utf32Le,
            Encoding::Utf32Be,
            Encoding::Iso2022Jp,
        ] {
            assert_eq!(enc.payload(), 0, "{enc:?}");
        }
    }

    /// The inline `String#<<` reads one tag byte and only then the
    /// payload, so exactly one discriminant it admits may carry one.
    /// Insert a payload-carrying variant above `Utf32Be` and the fast
    /// path would call two different encodings equal.
    #[test]
    fn utf8_is_the_only_payload_carrying_tag_the_inline_shl_admits() {
        assert_eq!(STRING_TY_PAYLOAD_TAG, Encoding::UTF8.tag());
        for enc in [
            Encoding::Ascii8,
            Encoding::UsAscii,
            Encoding::Utf16Le,
            Encoding::Utf16Be,
            Encoding::Utf32Le,
            Encoding::Utf32Be,
        ] {
            assert!(enc.tag() <= 6, "{enc:?}");
            assert_ne!(enc.tag(), STRING_TY_PAYLOAD_TAG, "{enc:?}");
        }
        assert!(Encoding::UTF8.tag() <= 6);
        // Everything that carries an index and is not `Utf8` sits past
        // the range, so the fast path never sees it.
        for enc in [
            Encoding::Iso8859(1),
            Encoding::EUC_JP,
            Encoding::Sjis(0),
            Encoding::Iso2022Jp,
            Encoding::Other(0),
            Encoding::NamedByte(0),
        ] {
            assert!(enc.tag() > 6, "{enc:?}");
        }
    }

    #[test]
    fn named_byte_index_const_agrees_with_the_table() {
        // `EMACS_MULE` is the one index `NAMED_BYTE_ENCODINGS` is read
        // by number rather than by name, and it used to be written out
        // as `37` — inserting an entry above it re-pointed it at
        // `IBM861` and broke both encodings' validation. Its doc
        // comment credited `emacs_mule_index_is_pinned` with keeping
        // the two in step; that test did not exist. This is it, and it
        // runs the `const fn` at run time so the walk it does at
        // compile time is exercised as well.
        assert_eq!(
            NAMED_BYTE_ENCODINGS[EMACS_MULE as usize],
            ("Emacs-Mule", "Emacs_Mule")
        );
        for (index, (_, konst)) in NAMED_BYTE_ENCODINGS.iter().enumerate() {
            assert_eq!(named_byte_index_const(konst), index as u8, "{konst}");
            assert_eq!(named_byte_index(konst), Some(index as u8), "{konst}");
        }
        // The two ways the byte compare can reject a candidate: a
        // different length, and a same-length mismatch.
        assert_eq!(named_byte_index_const("Big5"), 0);
        assert!(named_byte_index("Big5X").is_none());
        assert!(named_byte_index("Big4").is_none());
    }

    #[test]
    #[should_panic(expected = "NAMED_BYTE_ENCODINGS has no such constant suffix")]
    fn named_byte_index_const_rejects_an_unknown_suffix() {
        // The compile-time failure this gives for a typo in a `const`
        // position, seen from run time.
        let _ = named_byte_index_const("NoSuchEncoding");
    }

    #[test]
    fn try_from_str_normalises_separators_and_aliases() {
        // The normalisation runs in a fixed stack buffer, so the edges of
        // that buffer matter: an empty name, a name exactly at the cap, a
        // longer one, and a non-ASCII one must all still be rejected
        // rather than truncated into a match.
        assert!(Encoding::try_from_str("").is_err());
        assert!(Encoding::try_from_str(&"A".repeat(MAX_ENC_NAME)).is_err());
        assert!(Encoding::try_from_str(&"A".repeat(MAX_ENC_NAME + 1)).is_err());
        assert!(Encoding::try_from_str("UTF-8\u{3042}").is_err());
        // `.` normalises like `-` (CRuby accepts `UTF.8`).
        assert_eq!(Encoding::try_from_str("UTF.8").unwrap(), Encoding::UTF8);
        // The longest name we recognise still fits the buffer.
        assert_eq!(
            Encoding::try_from_str("ANSI_X3.4-1968").unwrap(),
            Encoding::UsAscii
        );
        assert_eq!(Encoding::try_from_str("UTF-8").unwrap(), Encoding::UTF8);
        assert_eq!(Encoding::try_from_str("utf-8").unwrap(), Encoding::UTF8);
        assert_eq!(Encoding::try_from_str("UTF8").unwrap(), Encoding::UTF8);
        assert_eq!(Encoding::try_from_str("BINARY").unwrap(), Encoding::Ascii8);
        assert_eq!(
            Encoding::try_from_str("ASCII-8BIT").unwrap(),
            Encoding::Ascii8
        );
        assert_eq!(
            Encoding::try_from_str("US-ASCII").unwrap(),
            Encoding::UsAscii
        );
        // ISO-8859-N round-trips through `_` and `-` separators.
        assert_eq!(
            Encoding::try_from_str("ISO-8859-1").unwrap(),
            Encoding::Iso8859(1)
        );
        assert_eq!(
            Encoding::try_from_str("ISO_8859_15").unwrap(),
            Encoding::Iso8859(15)
        );
        assert_eq!(
            Encoding::try_from_str("LATIN1").unwrap(),
            Encoding::Iso8859(1)
        );
        // UTF-16 / UTF-32 family.
        assert_eq!(
            Encoding::try_from_str("UTF-16LE").unwrap(),
            Encoding::Utf16Le
        );
        assert_eq!(
            Encoding::try_from_str("UTF-16BE").unwrap(),
            Encoding::Utf16Be
        );
        // Bare `UTF-16` / `UTF-32` are CRuby's BOM-based *dummy*
        // encodings — distinct ASCII-incompatible `Other` variants,
        // not the real LE codecs.
        assert_eq!(
            Encoding::try_from_str("UTF-16").unwrap(),
            Encoding::Other(3)
        );
        assert_eq!(
            Encoding::try_from_str("UTF-32").unwrap(),
            Encoding::Other(4)
        );
        // Japanese.
        assert_eq!(Encoding::try_from_str("EUC-JP").unwrap(), Encoding::EUC_JP);
        assert_eq!(
            Encoding::try_from_str("Shift_JIS").unwrap(),
            Encoding::Sjis(0)
        );
        assert_eq!(
            Encoding::try_from_str("Windows-31J").unwrap(),
            Encoding::Sjis(1)
        );
        assert_eq!(Encoding::try_from_str("CP932").unwrap(), Encoding::Sjis(1));
        // Pseudo-encoding names map to UTF-8.
        assert_eq!(Encoding::try_from_str("LOCALE").unwrap(), Encoding::UTF8);
        // Unknown name → ArgumentError.
        assert!(Encoding::try_from_str("Bogus-1").is_err());
    }

    #[test]
    fn name_round_trips_through_try_from_str() {
        for enc in [
            Encoding::Ascii8,
            Encoding::UTF8,
            Encoding::UsAscii,
            Encoding::Utf16Le,
            Encoding::Utf16Be,
            Encoding::Utf32Le,
            Encoding::Utf32Be,
            Encoding::Iso8859(1),
            Encoding::Iso8859(5),
            Encoding::Iso8859(15),
            Encoding::EUC_JP,
            Encoding::Sjis(0),
            Encoding::Sjis(1),
        ] {
            assert_eq!(Encoding::try_from_str(enc.name()).unwrap(), enc);
        }
    }

    #[test]
    fn ascii_compatible_flags() {
        assert!(Encoding::UTF8.is_ascii_compatible());
        assert!(Encoding::UsAscii.is_ascii_compatible());
        assert!(Encoding::Ascii8.is_ascii_compatible());
        assert!(Encoding::Iso8859(1).is_ascii_compatible());
        assert!(Encoding::EUC_JP.is_ascii_compatible());
        assert!(Encoding::Sjis(0).is_ascii_compatible());
        assert!(!Encoding::Utf16Le.is_ascii_compatible());
        assert!(!Encoding::Utf16Be.is_ascii_compatible());
        assert!(!Encoding::Utf32Le.is_ascii_compatible());
        assert!(!Encoding::Utf32Be.is_ascii_compatible());
    }

    #[test]
    fn dummy_flags_cover_non_native_decoders() {
        assert!(!Encoding::UTF8.is_dummy());
        assert!(!Encoding::UsAscii.is_dummy());
        assert!(!Encoding::Ascii8.is_dummy());
        assert!(Encoding::Utf16Le.is_dummy());
        assert!(Encoding::Iso8859(1).is_dummy());
        assert!(Encoding::EUC_JP.is_dummy());
    }

    /// `emacs_mule_precise_len`'s three answers, including the two
    /// only `emacs_mule_scrub`'s window-shortening ever asks for: a
    /// well-formed prefix, and an offset with nothing at it.
    #[test]
    fn emacs_mule_precise_len_answers() {
        use PreciseLen::*;
        // Nothing there at all, and a prefix that ran out of bytes.
        assert_eq!(emacs_mule_precise_len(&[], 0), NeedMore);
        assert_eq!(emacs_mule_precise_len(&[0x61], 1), NeedMore);
        assert_eq!(emacs_mule_precise_len(&[0x90, 0xa0], 0), NeedMore);
        assert_eq!(emacs_mule_precise_len(&[0x9c, 0xf0, 0xa0], 0), NeedMore);
        // One of each width.
        assert_eq!(emacs_mule_precise_len(b"a", 0), Char(1));
        assert_eq!(emacs_mule_precise_len(&[0x81, 0xa0], 0), Char(2));
        assert_eq!(emacs_mule_precise_len(&[0x90, 0xa0, 0xa0], 0), Char(3));
        assert_eq!(emacs_mule_precise_len(&[0x9a, 0xe0, 0xa0], 0), Char(3));
        assert_eq!(emacs_mule_precise_len(&[0x9c, 0xf0, 0xa0, 0xa0], 0), Char(4));
        assert_eq!(emacs_mule_precise_len(&[0x9d, 0xf5, 0xa0, 0xa0], 0), Char(4));
        // A lead that leads nothing, a charset id out of range, and a
        // continuation that is not one.
        assert_eq!(emacs_mule_precise_len(&[0x80, 0xa0], 0), Invalid);
        assert_eq!(emacs_mule_precise_len(&[0x9e, 0xa0], 0), Invalid);
        assert_eq!(emacs_mule_precise_len(&[0x9a, 0xa0, 0xa0], 0), Invalid);
        assert_eq!(emacs_mule_precise_len(&[0x9d, 0xf0, 0xa0, 0xa0], 0), Invalid);
        assert_eq!(emacs_mule_precise_len(&[0x90, 0xa0, 0x20], 0), Invalid);
    }

    /// `eucjp_precise_len` / `sjis_precise_len`, read off onigenc's own
    /// answers over the whole lead/continuation space.
    #[test]
    fn eucjp_sjis_precise_len_answers() {
        use PreciseLen::*;
        // EUC-JP: nothing there, and each width's truncated prefix.
        assert_eq!(eucjp_precise_len(&[], 0), NeedMore);
        assert_eq!(eucjp_precise_len(&[0xa1], 0), NeedMore);
        assert_eq!(eucjp_precise_len(&[0x8e], 0), NeedMore);
        assert_eq!(eucjp_precise_len(&[0x8f, 0xa1], 0), NeedMore);
        // One of each width.
        assert_eq!(eucjp_precise_len(b"a", 0), Char(1));
        assert_eq!(eucjp_precise_len(&[0xa1, 0xa1], 0), Char(2));
        assert_eq!(eucjp_precise_len(&[0xfe, 0xfe], 0), Char(2));
        assert_eq!(eucjp_precise_len(&[0x8f, 0xa1, 0xa1], 0), Char(3));
        // 0x8E takes the whole A1..FE, not just the kana block: onigenc
        // validates the shape, not the JIS X 0201 table.
        assert_eq!(eucjp_precise_len(&[0x8e, 0xa1], 0), Char(2));
        assert_eq!(eucjp_precise_len(&[0x8e, 0xfe], 0), Char(2));
        // Leads that lead nothing, and continuations that are not one.
        for lead in [0x80u8, 0x8d, 0x90, 0xa0, 0xff] {
            assert_eq!(eucjp_precise_len(&[lead, 0xa1], 0), Invalid, "{lead:#04x}");
        }
        assert_eq!(eucjp_precise_len(&[0x8e, 0x20], 0), Invalid);
        assert_eq!(eucjp_precise_len(&[0xa1, 0xa0], 0), Invalid);
        assert_eq!(eucjp_precise_len(&[0x8f, 0xa1, 0x20], 0), Invalid);

        // Shift_JIS: the single-byte ranges, the double-byte pair, and
        // the four bytes that lead nothing.
        assert_eq!(sjis_precise_len(&[], 0), NeedMore);
        assert_eq!(sjis_precise_len(&[0x81], 0), NeedMore);
        assert_eq!(sjis_precise_len(b"a", 0), Char(1));
        assert_eq!(sjis_precise_len(&[0xa1], 0), Char(1));
        assert_eq!(sjis_precise_len(&[0xdf], 0), Char(1));
        assert_eq!(sjis_precise_len(&[0x81, 0x40], 0), Char(2));
        assert_eq!(sjis_precise_len(&[0x9f, 0x7e], 0), Char(2));
        assert_eq!(sjis_precise_len(&[0xe0, 0x80], 0), Char(2));
        assert_eq!(sjis_precise_len(&[0xfc, 0xfc], 0), Char(2));
        for lead in [0x80u8, 0xa0, 0xfd, 0xff] {
            assert_eq!(sjis_precise_len(&[lead, 0x40], 0), Invalid, "{lead:#04x}");
        }
        assert_eq!(sjis_precise_len(&[0x81, 0x3f], 0), Invalid);
        assert_eq!(sjis_precise_len(&[0x81, 0x7f], 0), Invalid);
        assert_eq!(sjis_precise_len(&[0x81, 0xfd], 0), Invalid);
    }

    #[test]
    fn classify_seven_bit_short_circuit() {
        // SevenBit fast path applies to every ASCII-compatible enc
        // when all bytes are < 0x80.
        for enc in [
            Encoding::UTF8,
            Encoding::UsAscii,
            Encoding::Ascii8,
            Encoding::Iso8859(1),
            Encoding::EUC_JP,
            Encoding::Sjis(0),
        ] {
            assert_eq!(enc.classify(b"abc"), CodeRange::SevenBit, "{:?}", enc);
        }
        // UTF-16/32 don't qualify even for 7-bit-only payloads.
        assert_ne!(Encoding::Utf16Le.classify(b"ab"), CodeRange::SevenBit);
        assert_ne!(Encoding::Utf32Le.classify(b"abcd"), CodeRange::SevenBit);
        // Empty → SevenBit by definition.
        assert_eq!(Encoding::UTF8.classify(b""), CodeRange::SevenBit);
        assert_eq!(Encoding::Utf16Le.classify(b""), CodeRange::SevenBit);
    }

    #[test]
    fn classify_us_ascii_rejects_high_bytes() {
        // High byte under US-ASCII is Broken (Phase 1's stricter
        // semantics — was Valid pre-refactor).
        assert_eq!(Encoding::UsAscii.classify(b"\xff"), CodeRange::Broken);
        assert_eq!(Encoding::UsAscii.classify(b"a\xffz"), CodeRange::Broken);
    }

    #[test]
    fn classify_utf8_validity() {
        assert_eq!(Encoding::UTF8.classify("é".as_bytes()), CodeRange::Valid);
        assert_eq!(Encoding::UTF8.classify(&[0xff]), CodeRange::Broken);
        // Truncated 2-byte scalar.
        assert_eq!(Encoding::UTF8.classify(&[0xC3]), CodeRange::Broken);
    }

    #[test]
    fn classify_utf16_byte_count_parity() {
        // UTF-16 needs even byte count to validate.
        assert_eq!(Encoding::Utf16Le.classify(&[0x61, 0x00]), CodeRange::Valid);
        assert_eq!(
            Encoding::Utf16Le.classify(&[0x61, 0x00, 0x62]),
            CodeRange::Broken
        );
        assert_eq!(Encoding::Utf16Be.classify(&[0x00, 0x61]), CodeRange::Valid);
    }

    #[test]
    fn classify_utf32_byte_count_parity() {
        assert_eq!(
            Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00, 0x00]),
            CodeRange::Valid
        );
        assert_eq!(
            Encoding::Utf32Le.classify(&[0x61, 0x00, 0x00]),
            CodeRange::Broken
        );
    }

    #[test]
    fn cr_cache_is_lazy_and_then_stable() {
        // First read computes; subsequent reads return the same
        // value without re-walking. We can't directly observe
        // "didn't re-walk" in a unit test, but we can pin the
        // computed value.
        let s = RStringInner::from_str("abc");
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert!(s.is_ascii_only());
        assert!(s.is_valid_encoding());

        let bad = RStringInner::from_encoding(b"\xff", Encoding::UTF8);
        assert_eq!(bad.code_range(), CodeRange::Broken);
        assert!(!bad.is_ascii_only());
        assert!(!bad.is_valid_encoding());
    }

    #[test]
    fn set_encoding_invalidates_cache() {
        let mut s = RStringInner::from_str("abc");
        // Prime the cache (SevenBit under UTF-8).
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        // Switch to UTF-16LE — 3 bytes is now a broken code-unit
        // sequence. The cache must clear so the next read returns
        // Broken.
        s.set_encoding(Encoding::Utf16Le);
        assert_eq!(s.code_range(), CodeRange::Broken);
    }

    #[test]
    fn from_str_does_not_scan_until_queried() {
        // The no-scan constructor should leave cr at Unknown so
        // ephemeral strings that never need cr pay nothing. Once
        // queried, lazy classify computes and caches.
        let s = RStringInner::from_str("abc");
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_str("あいう");
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::Valid);
    }

    #[test]
    fn from_str_scanned_sets_cr_eagerly() {
        // The scanning constructor records cr at construction time
        // so subsequent queries are O(1) and `deep_copy` clones
        // inherit the classification without rescanning.
        let s = RStringInner::from_str_scanned("abc");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_str_scanned("あいう");
        assert_eq!(s.cr.get(), CodeRange::Valid);

        // Empty -> classify returns SevenBit; eager constructor
        // mirrors that.
        let s = RStringInner::from_str_scanned("");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
    }

    #[test]
    fn from_string_does_not_scan_until_queried() {
        let s = RStringInner::from_string("abc".to_string());
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    #[test]
    fn from_string_scanned_sets_cr_eagerly() {
        let s = RStringInner::from_string_scanned("abc".to_string());
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        let s = RStringInner::from_string_scanned("カナ".to_string());
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn from_encoding_does_not_scan_until_queried() {
        let s = RStringInner::from_encoding(b"abc", Encoding::UTF8);
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    #[test]
    fn from_encoding_scanned_sets_cr_eagerly() {
        // Source-byte literal that's all ASCII -> SevenBit.
        let s = RStringInner::from_encoding_scanned(b"abc", Encoding::UTF8);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        // Source-byte literal with valid UTF-8 multi-byte content.
        let s = RStringInner::from_encoding_scanned("é".as_bytes(), Encoding::UTF8);
        assert_eq!(s.cr.get(), CodeRange::Valid);

        // Source-byte literal with broken UTF-8 (e.g. `"\xff"`).
        let s = RStringInner::from_encoding_scanned(b"\xff", Encoding::UTF8);
        assert_eq!(s.cr.get(), CodeRange::Broken);

        // Ascii8 + high bytes: trivially Valid (no cut into char
        // sequences).
        let s = RStringInner::from_encoding_scanned(b"\xff\xfe", Encoding::Ascii8);
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn propagated_cr_empty_range_is_seven_bit() {
        // Any empty byte range matches the way `Encoding::classify`
        // treats `b""` — SevenBit by definition, regardless of the
        // parent's encoding or cr.
        let parent = RStringInner::from_encoding(b"\xff", Encoding::Ascii8);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 0),
            CodeRange::SevenBit
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 1, 1),
            CodeRange::SevenBit
        );
    }

    #[test]
    fn propagated_cr_valid_single_byte_encodings_propagate_unconditionally() {
        // ASCII-8BIT: every byte position is a character boundary, so
        // a Valid parent always yields a Valid child for any non-empty
        // sub-range.
        let parent = RStringInner::from_encoding(&[0xff, 0x80, 0x81], Encoding::Ascii8);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 3), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 1, 2), CodeRange::Valid);

        // ISO-8859-1: same — every byte represents a glyph, so any
        // cut respects character boundaries.
        let parent = RStringInner::from_encoding(&[0xc3, 0xa9, 0xfe], Encoding::Iso8859(1));
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 3), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 1, 3), CodeRange::Valid);
    }

    #[test]
    fn propagated_cr_valid_utf8_only_propagates_on_char_boundaries() {
        // "abcあdef" — 9 bytes; the あ scalar occupies offsets 3..6
        // (E3 81 82). Cuts that land on offsets 0/3/6/9 are character
        // boundaries; offsets 4/5/7/8 land on continuation bytes and
        // must NOT inherit Valid.
        let parent = RStringInner::from_encoding("abcあdef".as_bytes(), Encoding::UTF8);
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 3), (0, 6), (3, 6), (6, 9), (3, 9), (0, 9)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid,
                "{start}..{end} should propagate Valid"
            );
        }

        for &(start, end) in &[(0, 4), (0, 5), (3, 5), (4, 6), (4, 9), (5, 9)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown,
                "{start}..{end} crosses a UTF-8 continuation byte"
            );
        }
    }

    #[test]
    fn propagated_cr_valid_utf16_requires_even_byte_endpoints() {
        // "abc" encoded as UTF-16LE: 6 bytes, code-unit width 2.
        let parent =
            RStringInner::from_encoding(&[0x61, 0x00, 0x62, 0x00, 0x63, 0x00], Encoding::Utf16Le);
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 2), (0, 4), (0, 6), (2, 6), (4, 6)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid,
                "{start}..{end} should propagate Valid"
            );
        }

        for &(start, end) in &[(0, 1), (0, 3), (0, 5), (1, 4), (3, 6)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown,
                "{start}..{end} would split a UTF-16 code unit"
            );
        }

        // BE side: same parity rule.
        let parent = RStringInner::from_encoding(&[0x00, 0x61, 0x00, 0x62], Encoding::Utf16Be);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(RStringInner::propagated_cr(&parent, 0, 4), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 1, 4),
            CodeRange::Unknown
        );
    }

    #[test]
    fn propagated_cr_valid_utf32_requires_endpoints_aligned_to_four() {
        // "ab" encoded as UTF-32LE: 8 bytes, code-unit width 4.
        let parent = RStringInner::from_encoding(
            &[0x61, 0x00, 0x00, 0x00, 0x62, 0x00, 0x00, 0x00],
            Encoding::Utf32Le,
        );
        assert_eq!(parent.code_range(), CodeRange::Valid);

        for &(start, end) in &[(0, 4), (0, 8), (4, 8)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Valid
            );
        }

        for &(start, end) in &[(0, 2), (0, 5), (1, 4), (2, 8)] {
            assert_eq!(
                RStringInner::propagated_cr(&parent, start, end),
                CodeRange::Unknown
            );
        }
    }

    #[test]
    fn propagated_cr_valid_no_native_decoder_falls_back_to_unknown() {
        // EUC-JP / Shift_JIS: variable-width but monoruby has no
        // native decoder, so we can't tell whether `start`/`end`
        // land on a character boundary. The conservative choice is
        // Unknown so the next operation lazy-classifies.
        let parent = RStringInner::from_encoding(&[0xc6, 0xfc, 0xcb, 0xdc], Encoding::EUC_JP);
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 2),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 4),
            CodeRange::Unknown
        );

        let parent = RStringInner::from_encoding(&[0x82, 0xa0, 0x82, 0xa2], Encoding::Sjis(0));
        assert_eq!(parent.code_range(), CodeRange::Valid);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 1),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 4),
            CodeRange::Unknown
        );
    }

    #[test]
    fn propagated_cr_broken_or_unknown_parent_never_propagates() {
        // A Broken parent could have either Broken or Valid sub-
        // ranges depending on which bytes the slice covers, so we
        // don't propagate.
        let parent = RStringInner::from_encoding(&[0x61, 0xff, 0x62], Encoding::UTF8);
        assert_eq!(parent.code_range(), CodeRange::Broken);
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 1),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 0, 3),
            CodeRange::Unknown
        );
        assert_eq!(
            RStringInner::propagated_cr(&parent, 2, 3),
            CodeRange::Unknown
        );
    }

    #[test]
    fn from_vec_scanned_classifies_eagerly() {
        // All-ASCII bytes: tagged UTF-8 and pre-classified as
        // SevenBit, skipping the `from_utf8` rerun the lazy path
        // would do on first use.
        let s = RStringInner::from_vec_scanned(b"abc".to_vec());
        assert_eq!(s.encoding(), Encoding::UTF8);
        assert_eq!(s.code_range(), CodeRange::SevenBit);

        // Non-ASCII but valid UTF-8: tagged UTF-8 and pre-classified
        // Valid (the from_utf8 check landed on Ok).
        let s = RStringInner::from_vec_scanned("あいう".as_bytes().to_vec());
        assert_eq!(s.encoding(), Encoding::UTF8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Two-byte UTF-8 scalar: still Valid + UTF-8.
        let s = RStringInner::from_vec_scanned("é".as_bytes().to_vec());
        assert_eq!(s.encoding(), Encoding::UTF8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Invalid UTF-8 falls back to ASCII-8BIT, which classifies
        // every non-empty byte sequence as Valid.
        let s = RStringInner::from_vec_scanned(vec![0xff, 0xfe, 0x80]);
        assert_eq!(s.encoding(), Encoding::Ascii8);
        assert_eq!(s.code_range(), CodeRange::Valid);

        // Empty input: SevenBit by definition under any encoding.
        let s = RStringInner::from_vec_scanned(vec![]);
        assert_eq!(s.encoding(), Encoding::UTF8);
        assert_eq!(s.code_range(), CodeRange::SevenBit);
    }

    /// A `Hash`/`Eq` pair must agree: whatever `eq` calls equal has to
    /// hash alike, or a lookup misses a key the container holds.
    #[test]
    fn equal_strings_hash_alike_and_unequal_ones_are_told_apart() {
        use std::hash::{BuildHasher, RandomState};
        let s = RandomState::new();
        let mk = |bytes: &[u8], enc: Encoding| RStringInner::from_encoding_scanned(bytes, enc);
        let same = |a: &RStringInner, b: &RStringInner| {
            assert_eq!(a, b, "{a:?} vs {b:?}");
            assert_eq!(s.hash_one(a), s.hash_one(b), "{a:?} vs {b:?}");
        };
        let apart = |a: &RStringInner, b: &RStringInner| {
            assert_ne!(a, b, "{a:?} vs {b:?}");
            assert_ne!(s.hash_one(a), s.hash_one(b), "{a:?} vs {b:?}");
        };
        // 7-bit content is one key whatever the encoding carries.
        let ascii = [
            Encoding::UTF8,
            Encoding::Ascii8,
            Encoding::UsAscii,
            Encoding::EUC_JP,
            Encoding::Utf8(UTF8_MAC),
        ];
        for enc in ascii {
            same(&mk(b"abc", Encoding::UTF8), &mk(b"abc", enc));
        }
        // ...and so is the empty string, even where the encodings
        // could not be negotiated with any content in them.
        for enc in [Encoding::Utf16Be, Encoding::Iso2022Jp, Encoding::Ascii8] {
            same(&mk(b"", Encoding::UTF8), &mk(b"", enc));
        }
        // 8-bit content is a key per encoding.
        let hi = "\u{3042}".as_bytes();
        let utf8 = mk(hi, Encoding::UTF8);
        for enc in [
            Encoding::Ascii8,
            Encoding::EUC_JP,
            Encoding::Sjis(0),
            Encoding::Utf8(UTF8_MAC),
            Encoding::NamedByte(CESU_8),
        ] {
            apart(&utf8, &mk(hi, enc));
        }
        // Different bytes stay different whatever the encodings.
        apart(&utf8, &mk("\u{3044}".as_bytes(), Encoding::UTF8));
    }

    /// The bytes alone never decide: a `PartialEq` that read them and
    /// nothing else made every pair above one key (#1569).
    #[test]
    fn the_encoding_is_part_of_a_strings_identity() {
        let hi = "\u{3042}".as_bytes();
        assert_eq!(
            RStringInner::from_encoding_scanned(hi, Encoding::UTF8).as_bytes(),
            RStringInner::from_encoding_scanned(hi, Encoding::Ascii8).as_bytes(),
        );
        assert_ne!(
            RStringInner::from_encoding_scanned(hi, Encoding::UTF8),
            RStringInner::from_encoding_scanned(hi, Encoding::Ascii8),
        );
    }

    #[test]
    fn encoding_compatible_same_encoding() {
        // `Encoding::compatible` (the bare classifier used by
        // `Encoding.compatible?`) — same encoding always returns
        // that encoding regardless of CR.
        assert_eq!(
            Encoding::compatible(
                Encoding::UTF8,
                CodeRange::Valid,
                Encoding::UTF8,
                CodeRange::Broken
            ),
            Some(Encoding::UTF8)
        );
    }

    #[test]
    fn encoding_compatible_seven_bit_left_wins() {
        // Both ASCII-compatible AND both 7-bit → left wins.
        assert_eq!(
            Encoding::compatible(
                Encoding::UTF8,
                CodeRange::SevenBit,
                Encoding::UsAscii,
                CodeRange::SevenBit,
            ),
            Some(Encoding::UTF8)
        );
        assert_eq!(
            Encoding::compatible(
                Encoding::UsAscii,
                CodeRange::SevenBit,
                Encoding::UTF8,
                CodeRange::SevenBit,
            ),
            Some(Encoding::UsAscii)
        );
    }

    #[test]
    fn encoding_compatible_seven_bit_defers_to_other() {
        // Exactly one side 7-bit → the non-7-bit side keeps its
        // encoding.
        assert_eq!(
            Encoding::compatible(
                Encoding::UTF8,
                CodeRange::SevenBit,
                Encoding::Iso8859(1),
                CodeRange::Valid,
            ),
            Some(Encoding::Iso8859(1))
        );
        assert_eq!(
            Encoding::compatible(
                Encoding::Iso8859(1),
                CodeRange::Valid,
                Encoding::UTF8,
                CodeRange::SevenBit,
            ),
            Some(Encoding::Iso8859(1))
        );
    }

    #[test]
    fn encoding_compatible_incompatible_pairs() {
        // Two non-7-bit, distinct ASCII-compatible encodings → None.
        assert_eq!(
            Encoding::compatible(
                Encoding::UTF8,
                CodeRange::Valid,
                Encoding::Iso8859(1),
                CodeRange::Valid,
            ),
            None
        );
        // UTF-16 vs anything → None (UTF-16 isn't ASCII-compatible).
        assert_eq!(
            Encoding::compatible(
                Encoding::Utf16Le,
                CodeRange::Valid,
                Encoding::UTF8,
                CodeRange::SevenBit,
            ),
            None
        );
    }

    // ---------- Non-UTF-8 / error-path coverage ----------

    #[test]
    fn to_str_errors_on_invalid_utf8_in_utf8_string() {
        // UTF-8-tagged bytes containing an invalid sequence: `to_str`
        // returns Err with the "invalid byte sequence: ..." prefix.
        let s = RStringInner::from_encoding(b"abc\xFFdef", Encoding::UTF8);
        let err = s.to_str().unwrap_err();
        let msg = err.message();
        assert!(
            msg.starts_with("invalid byte sequence:"),
            "unexpected message: {msg}"
        );
    }

    #[test]
    fn to_str_escapes_high_bytes_for_binary_encoding() {
        // Non-UTF-8 encodings always succeed; high bytes come back as
        // `\xHH` literal in the resulting string.
        let s = RStringInner::from_encoding(b"a\xFFb", Encoding::Ascii8);
        assert_eq!(s.to_str().unwrap().as_ref(), r"a\xFFb");

        let utf16 = RStringInner::from_encoding(&[0x00, 0xD8], Encoding::Utf16Le);
        // 0x00 stays ASCII-printable; 0xD8 escapes.
        assert_eq!(utf16.to_str().unwrap().as_ref(), "\x00\\xD8");
    }

    #[test]
    fn byte_to_char_index_errors_on_invalid_utf8_bytes() {
        // `byte_to_char_index` walks `check_utf8()`, which fails on
        // any byte sequence that isn't valid UTF-8 — here a Shift_JIS
        // string whose high bytes are not valid UTF-8 starters.
        let s = RStringInner::from_encoding(b"\x82\xa0", Encoding::Sjis(0));
        assert!(s.byte_to_char_index(0).is_err());

        // Same for an explicitly-broken UTF-8 string.
        let s = RStringInner::from_encoding(b"abc\xFF", Encoding::UTF8);
        assert!(s.byte_to_char_index(3).is_err());
    }

    #[test]
    fn byte_to_char_index_errors_off_char_boundary() {
        // UTF-8 valid string ("aあb"). `あ` occupies bytes 1..4. A
        // byte position pointing into the middle of `あ` is rejected.
        let s = RStringInner::from_str("aあb");
        assert_eq!(s.byte_to_char_index(0).unwrap(), 0);
        assert_eq!(s.byte_to_char_index(1).unwrap(), 1);
        assert_eq!(s.byte_to_char_index(4).unwrap(), 2);
        assert!(s.byte_to_char_index(2).is_err());
        assert!(s.byte_to_char_index(3).is_err());
        // Out-of-range byte position also errors.
        assert!(s.byte_to_char_index(99).is_err());
    }

    #[test]
    fn dump_non_utf8_compatible_routes_through_ascii_escape() {
        // Non-UTF-8-compatible encodings (UTF-16/32, ISO-8859,
        // EUC-JP, Shift_JIS) take the byte-wise branch in `dump`.
        // Each byte goes through `ascii_escape` (high bytes become
        // `\xHH`).
        let s = RStringInner::from_encoding(b"a\x80\xFFb", Encoding::Ascii8);
        assert_eq!(s.dump(), "a\\x80\\xFFb");

        let s = RStringInner::from_encoding(&[0x00, 0xD8, 0x00, 0x00], Encoding::Utf16Le);
        assert_eq!(s.dump(), "\\x00\\xD8\\x00\\x00");

        // The `#`-trigram lookahead also fires on the byte-wise path.
        let s = RStringInner::from_encoding(b"a#$b", Encoding::Ascii8);
        assert_eq!(s.dump(), "a\\#$b");
        let s = RStringInner::from_encoding(b"#@x", Encoding::Ascii8);
        assert_eq!(s.dump(), "\\#@x");
        let s = RStringInner::from_encoding(b"#{}", Encoding::Ascii8);
        assert_eq!(s.dump(), "\\#{}");
        // Lone `#` (not followed by a trigram char) is left alone.
        let s = RStringInner::from_encoding(b"#abc", Encoding::Ascii8);
        assert_eq!(s.dump(), "#abc");
    }

    #[test]
    fn utf8_dump_with_lookahead_handles_invalid_utf8() {
        // Invalid bytes within a UTF-8-tagged buffer fall into the
        // `Err(_)` arm of `from_utf8` and emit `\xHH` per byte.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, b"a\xFFb");
        assert_eq!(out, "a\\xFFb");

        // Multiple invalid bytes in a row → one `\xHH` each.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, &[0xC0, 0xC1, 0xF5]);
        assert_eq!(out, "\\xC0\\xC1\\xF5");

        // Mixed valid + invalid: walk-and-resume keeps the rest of
        // the input. `#$` inside the valid prefix is escaped.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, b"#$\xFFhi");
        assert_eq!(out, "\\#$\\xFFhi");

        // Truncated multibyte at the end (the `error_len() == None`
        // arm): the leading bytes surface as `\xHH`.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, &[b'a', 0xE3, 0x81]);
        assert_eq!(out, "a\\xE3\\x81");

        // BMP non-ASCII still passes through `utf8_dump_one` →
        // `utf8_escape`, so it lands on `\uNNNN` (confirms the
        // lookahead-driven path doesn't bypass the codepoint-form
        // rule for valid runs).
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, "\u{0080}".as_bytes());
        assert_eq!(out, "\\u0080");

        // Supplementary-plane char → brace form.
        let mut out = String::new();
        utf8_dump_with_lookahead(&mut out, "\u{1F600}".as_bytes());
        assert_eq!(out, "\\u{1F600}");
    }

    #[test]
    fn with_encoding_capacity_starts_seven_bit() {
        // The empty byte sequence is trivially ASCII, so a freshly
        // allocated empty buffer should advertise SevenBit
        // regardless of the declared encoding.
        let s = RStringInner::with_encoding_capacity(Encoding::Sjis(0), 16);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::Sjis(0));
        assert_eq!(s.as_bytes().len(), 0);

        let s = RStringInner::with_encoding_capacity(Encoding::Utf16Le, 0);
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::Utf16Le);
    }

    #[test]
    fn extend_from_slice_merge_cr_merges_in_o1_queries() {
        // SevenBit + SevenBit stays SevenBit.
        let mut s = RStringInner::from_str_scanned("abc");
        s.extend_from_slice_merge_cr(b"def");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);

        // SevenBit + valid multi-byte UTF-8 → Valid.
        s.extend_from_slice_merge_cr("あ".as_bytes());
        assert_eq!(s.cr.get(), CodeRange::Valid);

        // Appending bytes invalid in the receiver's encoding degrades to
        // Unknown, and a later query classifies the truth (Broken).
        s.extend_from_slice_merge_cr(b"\xff");
        assert_eq!(s.cr.get(), CodeRange::Unknown);
        assert_eq!(s.code_range(), CodeRange::Broken);

        // Ascii8 receiver: high bytes are Valid, so the merge keeps an
        // O(1)-queryable cr.
        let mut b = RStringInner::from_encoding_scanned(b"ab", Encoding::Ascii8);
        b.extend_from_slice_merge_cr(b"\xff\xfe");
        assert_eq!(b.cr.get(), CodeRange::Valid);

        // Empty append is a no-op.
        let mut e = RStringInner::from_str_scanned("x");
        e.extend_from_slice_merge_cr(b"");
        assert_eq!(e.cr.get(), CodeRange::SevenBit);
    }

    #[test]
    fn bytesplice_with_seven_bit_combines_to_seven_bit() {
        let globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("abcdef");
        let repl = RStringInner::from_str_scanned("XY");
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(repl.code_range(), CodeRange::SevenBit);

        s.bytesplice_with(2, 2, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), b"abXYef");
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
        assert_eq!(s.encoding(), Encoding::UTF8);
    }

    #[test]
    fn bytesplice_with_seven_bit_plus_valid_is_valid() {
        let globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("abcdef");
        let repl = RStringInner::from_str_scanned("あ"); // 3 bytes, Valid
        assert_eq!(s.code_range(), CodeRange::SevenBit);
        assert_eq!(repl.code_range(), CodeRange::Valid);

        s.bytesplice_with(2, 2, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), "abあef".as_bytes());
        // SevenBit + Valid on aligned UTF-8 boundaries → Valid in O(1).
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn bytesplice_with_valid_plus_valid_keeps_valid() {
        let globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("xあy");
        let repl = RStringInner::from_str_scanned("いう");
        assert_eq!(s.code_range(), CodeRange::Valid);
        assert_eq!(repl.code_range(), CodeRange::Valid);

        // Replace the middle 3 bytes ("あ") with the 6-byte replacement.
        s.bytesplice_with(1, 3, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), "xいうy".as_bytes());
        assert_eq!(s.cr.get(), CodeRange::Valid);
    }

    #[test]
    fn bytesplice_with_us_ascii_and_utf8_combine() {
        // Pure-ASCII Utf8 and UsAscii are compatible; CRuby keeps
        // the receiver's encoding when both sides are SevenBit. The
        // important guarantees here are (a) the call succeeds and
        // (b) cr stays SevenBit, since both sides are pure ASCII
        // and the result encoding is ASCII-compatible.
        let globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("hi");
        s.set_encoding(Encoding::UsAscii);
        let repl = RStringInner::from_str_scanned("X");
        assert_eq!(s.encoding(), Encoding::UsAscii);
        assert_eq!(repl.encoding(), Encoding::UTF8);

        s.bytesplice_with(0, 1, &repl, &globals.store).unwrap();
        assert_eq!(s.as_bytes(), b"Xi");
        assert!(s.encoding().is_ascii_compatible());
        assert_eq!(s.cr.get(), CodeRange::SevenBit);
    }

    #[test]
    fn bytesplice_with_incompatible_encodings_errors() {
        // Sjis with a non-ASCII byte cannot be combined with a
        // non-ASCII UTF-8 replacement.
        let globals = Globals::new_test();
        let mut s = RStringInner::from_encoding_scanned(b"\x82\xa0", Encoding::Sjis(0)); // "あ" in Sjis
        let repl = RStringInner::from_str_scanned("え");
        assert!(s.bytesplice_with(0, 0, &repl, &globals.store).is_err());
    }

    #[test]
    fn bytesplice_with_breaks_utf8_boundary_stays_utf8() {
        // Splicing into the middle of a multi-byte UTF-8 character
        // produces broken bytes. The string keeps its UTF-8 tag and is
        // classified Broken: re-tagging it ASCII-8BIT would also declare
        // it valid, and CRuby reports `valid_encoding?` false instead.
        // `String#bytesplice` itself never gets here, rejecting a
        // non-boundary offset with IndexError as CRuby does; this is the
        // helper that `index_assign` also splices through.
        let globals = Globals::new_test();
        let mut s = RStringInner::from_str_scanned("あ"); // 3 bytes
        let repl = RStringInner::from_str_scanned("X");

        // Replace byte 1 (middle of "あ") — boundary check fails.
        s.bytesplice_with(1, 0, &repl, &globals.store).unwrap();
        assert_eq!(s.encoding(), Encoding::UTF8);
        assert_eq!(s.cr.get(), CodeRange::Broken);
    }
}

#[cfg(test)]
mod shared_string_tests {
    use super::*;

    /// The shared overlay is only sound if `SharedContent`'s fields sit
    /// exactly on the vendored SmallVec's capacity / heap-ptr / heap-len
    /// offsets (the JIT's inline `bytesize`/`getbyte` read shared strings
    /// through those offsets). Verify behaviourally against a spilled
    /// SmallVec rather than trusting the (non-repr(C)) SmallVec layout.
    #[test]
    fn shared_overlay_matches_spilled_smallvec_layout() {
        assert_eq!(
            std::mem::offset_of!(SharedContent, tag),
            smallvec::OFFSET_CAPA
        );
        assert_eq!(
            std::mem::offset_of!(SharedContent, ptr),
            smallvec::OFFSET_HEAP_PTR
        );
        assert_eq!(
            std::mem::offset_of!(SharedContent, len),
            smallvec::OFFSET_HEAP_LEN
        );
        assert_eq!(
            std::mem::size_of::<StringContent>(),
            std::mem::size_of::<SmallVec<[u8; STRING_INLINE_CAP]>>()
        );

        // A spilled owned buffer read through the `shared` overlay must
        // expose its heap ptr / len on the same offsets.
        let bytes: Vec<u8> = (0..100u8).collect();
        let inner = RStringInner::from(
            SmallVec::from_slice(&bytes),
            Encoding::Ascii8,
            CodeRange::Valid,
        );
        assert!(inner.owned_spilled());
        let (ptr, len) = unsafe { (inner.content.shared.ptr, inner.content.shared.len) };
        assert_eq!(ptr, inner.as_ptr());
        assert_eq!(len, inner.len());
        // ... and a real capacity can never collide with the tag.
        assert_ne!(unsafe { inner.content.shared.tag }, STRING_SHARED_TAG);
    }

    #[test]
    fn substring_shares_and_copy_on_write_isolates() {
        let _globals = Globals::new_test();
        let src: String = ('a'..='z').cycle().take(200).collect();
        let mut parent = Value::string(src.clone());
        assert!(parent.as_rstring_inner().owned_spilled());

        // Long suffix: shared, zero-copy.
        let mut child = string_substring(parent, 50, 200);
        assert!(child.as_rstring_inner().is_shared());
        // Parent has been converted into a sharer of the same hidden root.
        assert!(parent.as_rstring_inner().is_shared());
        let root = child.as_rstring_inner().shared_root().unwrap();
        assert_eq!(parent.as_rstring_inner().shared_root(), Some(root));
        assert!(root.is_frozen());
        assert_eq!(
            child.as_rstring_inner().as_bytes(),
            &src.as_bytes()[50..200]
        );
        assert_eq!(parent.as_rstring_inner().as_bytes(), src.as_bytes());
        // Both views alias the root's buffer (no copy happened).
        assert_eq!(
            unsafe { parent.as_rstring_inner().as_ptr().add(50) },
            child.as_rstring_inner().as_ptr()
        );

        // Mutating the parent copies it out; the child is unaffected.
        parent.as_rstring_inner_mut().set_byte(50, b'!');
        assert!(!parent.as_rstring_inner().is_shared());
        assert_eq!(parent.as_rstring_inner().as_bytes()[50], b'!');
        assert_eq!(child.as_rstring_inner().as_bytes()[0], src.as_bytes()[50]);

        // Mutating the child copies it out; the root stays intact.
        child.as_rstring_inner_mut().set_byte(0, b'?');
        assert!(!child.as_rstring_inner().is_shared());
        assert_eq!(root.as_rstring_inner().as_bytes(), src.as_bytes());

        // Short slices stay plain owned copies.
        let small = string_substring(parent, 0, 10);
        assert!(!small.as_rstring_inner().is_shared());

        // A substring of a sharer shares the same root (no chains).
        let mut parent2 = Value::string(src.clone());
        let c1 = string_substring(parent2, 10, 190);
        let c2 = string_substring(c1, 5, 120);
        assert_eq!(
            c2.as_rstring_inner().shared_root(),
            c1.as_rstring_inner().shared_root()
        );
        assert_eq!(c2.as_rstring_inner().as_bytes(), &src.as_bytes()[15..130]);
        let _ = &mut parent2;
    }

    #[test]
    fn frozen_parent_is_its_own_root() {
        let _globals = Globals::new_test();
        let src: String = ('0'..='9').cycle().take(120).collect();
        let mut parent = Value::string(src.clone());
        parent.set_frozen();
        let child = string_substring(parent, 0, 100);
        assert!(child.as_rstring_inner().is_shared());
        // No hidden root was created: the frozen parent serves directly.
        assert_eq!(child.as_rstring_inner().shared_root(), Some(parent));
        assert!(!parent.as_rstring_inner().is_shared());
        assert_eq!(child.as_rstring_inner().as_bytes(), &src.as_bytes()[0..100]);
    }

    #[test]
    fn clone_of_sharer_is_cheap_and_isolated() {
        let _globals = Globals::new_test();
        let src = "x".repeat(150);
        let parent = Value::string(src.clone());
        let child = string_substring(parent, 0, 150);
        assert!(child.as_rstring_inner().is_shared());
        let cloned = child.as_rstring_inner().clone();
        assert!(cloned.is_shared());
        assert_eq!(cloned.as_ptr(), child.as_rstring_inner().as_ptr());
        assert_eq!(cloned.as_bytes(), src.as_bytes());
    }
}
