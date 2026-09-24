//! Converting a string's bytes from one encoding to another.
//!
//! The codecs, the CRuby tables they are corrected by, the pivot through
//! UTF-8, the streamed converter behind `Encoding::Converter`, and the
//! error messages each of them raises with. The entry point for a
//! whole string is `transcode_bytes_with_opts` in the parent module;
//! `builtins::encoding` reads the Ruby-level arguments and calls in here.
//!
//! The data tables are generated from CRuby by `bin/gen-*` into the
//! child modules: `carrier` (the carrier-emoji encodings), `cjk` (the
//! double-byte CJK encodings), `cp51932` and `kddi`.

use super::*;

pub(crate) mod carrier;
mod cjk;
mod cp51932;
mod kddi;
#[cfg(test)]
mod tests;

use carrier::carrier_utf8_form;

pub(crate) fn encoding_to_rs(
    enc: crate::value::Encoding,
) -> Option<&'static encoding_rs::Encoding> {
    use crate::value::Encoding as E;
    let label: &[u8] = match enc {
        E::Utf8(_) => b"utf-8",
        E::Utf16Le => b"utf-16le",
        E::Utf16Be => b"utf-16be",
        E::Iso8859(n) => match n {
            1 => b"iso-8859-1",
            2 => b"iso-8859-2",
            3 => b"iso-8859-3",
            4 => b"iso-8859-4",
            5 => b"iso-8859-5",
            6 => b"iso-8859-6",
            7 => b"iso-8859-7",
            8 => b"iso-8859-8",
            10 => b"iso-8859-10",
            13 => b"iso-8859-13",
            14 => b"iso-8859-14",
            15 => b"iso-8859-15",
            16 => b"iso-8859-16",
            _ => return None,
        },
        E::EucJp(_) => b"euc-jp",
        // MacJapanese runs on the Shift_JIS character walk but has no
        // converter of its own in CRuby, which answers
        // `ConverterNotFoundError` for anything but 7-bit text — so it
        // gets no codec here either (#1471).
        E::Sjis(2) => return None,
        E::Sjis(_) => b"shift_jis",
        // ISO-2022-JP is not `encoding_rs`'s: WHATWG's folds half-width
        // katakana into their full-width cells and reaches rows CRuby
        // has no room for (#1612), and going through Unicode at all
        // loses the cells Unicode has no character for (#1609). It
        // converts through stateless-ISO-2022-JP and EUC-JP instead,
        // which is the chain CRuby's own `convpath` reports.
        E::Iso2022Jp => return None,
        // Named byte-oriented encodings with an encoding_rs codec.
        // WHATWG deviations accepted here (they only widen coverage):
        // "big5" is the HKSCS-extended table, "euc-kr" is windows-949
        // (a CP949 superset of EUC-KR), and "windows-874" is a
        // TIS-620 superset.
        E::NamedByte(_) => match enc.name() {
            "Windows-1250" => b"windows-1250",
            "Windows-1251" => b"windows-1251",
            "Windows-1252" => b"windows-1252",
            "Windows-1253" => b"windows-1253",
            "Windows-1254" => b"windows-1254",
            "Windows-1255" => b"windows-1255",
            "Windows-1256" => b"windows-1256",
            "Windows-1257" => b"windows-1257",
            // Windows-1258 is an encoding CRuby ships no transcoder
            // for, so it answers `ConverterNotFoundError` for
            // anything but 7-bit text — and so gets no codec here
            // either, as MacJapanese does not (#1591).
            "Windows-1258" => return None,
            "KOI8-R" => b"koi8-r",
            "KOI8-U" => b"koi8-u",
            "IBM866" => b"ibm866",
            // CP950 and CP951 are Big5's table under Microsoft's
            // names, and CRuby converts them exactly as it does Big5
            // (#1567). The walk knows them already — #1563 gave them
            // `big5_precise_len`.
            // Big5-UAO and GB12345 read through the same codecs as
            // their neighbours, corrected cell by cell by the tables
            // `bin/gen-cjk-tables` reads off CRuby (#1520).
            "Big5" | "Big5-HKSCS" | "Big5-UAO" | "CP950" | "CP951" => b"big5",
            "GBK" | "GB2312" | "GB12345" => b"gbk",
            "GB18030" => b"gb18030",
            "EUC-KR" | "CP949" => b"euc-kr",
            // EUC-TW and the DOS codepages other than IBM866 have no
            // encoding_rs codec; IBM437 is served by the in-tree
            // single-byte table instead.
            _ => return None,
        },
        // Handled by callers as fast paths / no native codec.
        E::Utf32Le | E::Utf32Be | E::Ascii8 | E::UsAscii | E::Other(_) => return None,
    };
    encoding_rs::Encoding::for_label(label)
}

/// ASCII-8BIT read *as a source*: its 7-bit half is ASCII and every
/// byte above it stands for no character at all, which is the shape
/// of a single-byte table with an empty high half. Kept apart from
/// [`single_byte_table`], which also answers for destinations and for
/// the ctype tables, where BINARY is not a character encoding at all
/// (#1596).
pub(crate) fn source_byte_table(
    enc: crate::value::Encoding,
) -> Option<&'static [Option<char>; 128]> {
    const BINARY: [Option<char>; 128] = [None; 128];
    if enc == crate::value::Encoding::Ascii8 {
        return Some(&BINARY);
    }
    single_byte_table(enc)
}

/// High-half (0x80..=0xFF) Unicode mapping for the single-byte
/// encodings monoruby transcodes with an in-tree table rather than
/// through `encoding_rs`. `None` is a cell the encoding assigns no
/// character to. Bytes < 0x80 are ASCII in all of these.
pub(crate) fn single_byte_table(
    enc: crate::value::Encoding,
) -> Option<&'static [Option<char>; 128]> {
    /// CP850 (DOS Latin-1), IBM737 (DOS Greek) and IBM775 (DOS
    /// Baltic): CRuby's single-byte tables, which `encoding_rs` has no
    /// codec for (#1520). Read off CRuby 4.0.6, every cell assigned.
    const CP850: [Option<char>; 128] = [
        Some('\u{C7}'),
        Some('\u{FC}'),
        Some('\u{E9}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E0}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{E8}'),
        Some('\u{EF}'),
        Some('\u{EE}'),
        Some('\u{EC}'),
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C9}'),
        Some('\u{E6}'),
        Some('\u{C6}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F2}'),
        Some('\u{FB}'),
        Some('\u{F9}'),
        Some('\u{FF}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{F8}'),
        Some('\u{A3}'),
        Some('\u{D8}'),
        Some('\u{D7}'),
        Some('\u{192}'),
        Some('\u{E1}'),
        Some('\u{ED}'),
        Some('\u{F3}'),
        Some('\u{FA}'),
        Some('\u{F1}'),
        Some('\u{D1}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{BF}'),
        Some('\u{AE}'),
        Some('\u{AC}'),
        Some('\u{BD}'),
        Some('\u{BC}'),
        Some('\u{A1}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{C1}'),
        Some('\u{C2}'),
        Some('\u{C0}'),
        Some('\u{A9}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{A2}'),
        Some('\u{A5}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{E3}'),
        Some('\u{C3}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{A4}'),
        Some('\u{F0}'),
        Some('\u{D0}'),
        Some('\u{CA}'),
        Some('\u{CB}'),
        Some('\u{C8}'),
        Some('\u{131}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{A6}'),
        Some('\u{CC}'),
        Some('\u{2580}'),
        Some('\u{D3}'),
        Some('\u{DF}'),
        Some('\u{D4}'),
        Some('\u{D2}'),
        Some('\u{F5}'),
        Some('\u{D5}'),
        Some('\u{B5}'),
        Some('\u{FE}'),
        Some('\u{DE}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        Some('\u{FD}'),
        Some('\u{DD}'),
        Some('\u{AF}'),
        Some('\u{B4}'),
        Some('\u{AD}'),
        Some('\u{B1}'),
        Some('\u{2017}'),
        Some('\u{BE}'),
        Some('\u{B6}'),
        Some('\u{A7}'),
        Some('\u{F7}'),
        Some('\u{B8}'),
        Some('\u{B0}'),
        Some('\u{A8}'),
        Some('\u{B7}'),
        Some('\u{B9}'),
        Some('\u{B3}'),
        Some('\u{B2}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];
    const IBM737: [Option<char>; 128] = [
        Some('\u{391}'),
        Some('\u{392}'),
        Some('\u{393}'),
        Some('\u{394}'),
        Some('\u{395}'),
        Some('\u{396}'),
        Some('\u{397}'),
        Some('\u{398}'),
        Some('\u{399}'),
        Some('\u{39A}'),
        Some('\u{39B}'),
        Some('\u{39C}'),
        Some('\u{39D}'),
        Some('\u{39E}'),
        Some('\u{39F}'),
        Some('\u{3A0}'),
        Some('\u{3A1}'),
        Some('\u{3A3}'),
        Some('\u{3A4}'),
        Some('\u{3A5}'),
        Some('\u{3A6}'),
        Some('\u{3A7}'),
        Some('\u{3A8}'),
        Some('\u{3A9}'),
        Some('\u{3B1}'),
        Some('\u{3B2}'),
        Some('\u{3B3}'),
        Some('\u{3B4}'),
        Some('\u{3B5}'),
        Some('\u{3B6}'),
        Some('\u{3B7}'),
        Some('\u{3B8}'),
        Some('\u{3B9}'),
        Some('\u{3BA}'),
        Some('\u{3BB}'),
        Some('\u{3BC}'),
        Some('\u{3BD}'),
        Some('\u{3BE}'),
        Some('\u{3BF}'),
        Some('\u{3C0}'),
        Some('\u{3C1}'),
        Some('\u{3C3}'),
        Some('\u{3C2}'),
        Some('\u{3C4}'),
        Some('\u{3C5}'),
        Some('\u{3C6}'),
        Some('\u{3C7}'),
        Some('\u{3C8}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{2561}'),
        Some('\u{2562}'),
        Some('\u{2556}'),
        Some('\u{2555}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{255C}'),
        Some('\u{255B}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{255E}'),
        Some('\u{255F}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{2567}'),
        Some('\u{2568}'),
        Some('\u{2564}'),
        Some('\u{2565}'),
        Some('\u{2559}'),
        Some('\u{2558}'),
        Some('\u{2552}'),
        Some('\u{2553}'),
        Some('\u{256B}'),
        Some('\u{256A}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{258C}'),
        Some('\u{2590}'),
        Some('\u{2580}'),
        Some('\u{3C9}'),
        Some('\u{3AC}'),
        Some('\u{3AD}'),
        Some('\u{3AE}'),
        Some('\u{3CA}'),
        Some('\u{3AF}'),
        Some('\u{3CC}'),
        Some('\u{3CD}'),
        Some('\u{3CB}'),
        Some('\u{3CE}'),
        Some('\u{386}'),
        Some('\u{388}'),
        Some('\u{389}'),
        Some('\u{38A}'),
        Some('\u{38C}'),
        Some('\u{38E}'),
        Some('\u{38F}'),
        Some('\u{B1}'),
        Some('\u{2265}'),
        Some('\u{2264}'),
        Some('\u{3AA}'),
        Some('\u{3AB}'),
        Some('\u{F7}'),
        Some('\u{2248}'),
        Some('\u{B0}'),
        Some('\u{2219}'),
        Some('\u{B7}'),
        Some('\u{221A}'),
        Some('\u{207F}'),
        Some('\u{B2}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];
    const IBM775: [Option<char>; 128] = [
        Some('\u{106}'),
        Some('\u{FC}'),
        Some('\u{E9}'),
        Some('\u{101}'),
        Some('\u{E4}'),
        Some('\u{123}'),
        Some('\u{E5}'),
        Some('\u{107}'),
        Some('\u{142}'),
        Some('\u{113}'),
        Some('\u{156}'),
        Some('\u{157}'),
        Some('\u{12B}'),
        Some('\u{179}'),
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C9}'),
        Some('\u{E6}'),
        Some('\u{C6}'),
        Some('\u{14D}'),
        Some('\u{F6}'),
        Some('\u{122}'),
        Some('\u{A2}'),
        Some('\u{15A}'),
        Some('\u{15B}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{F8}'),
        Some('\u{A3}'),
        Some('\u{D8}'),
        Some('\u{D7}'),
        Some('\u{A4}'),
        Some('\u{100}'),
        Some('\u{12A}'),
        Some('\u{F3}'),
        Some('\u{17B}'),
        Some('\u{17C}'),
        Some('\u{17A}'),
        Some('\u{201D}'),
        Some('\u{A6}'),
        Some('\u{A9}'),
        Some('\u{AE}'),
        Some('\u{AC}'),
        Some('\u{BD}'),
        Some('\u{BC}'),
        Some('\u{141}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{104}'),
        Some('\u{10C}'),
        Some('\u{118}'),
        Some('\u{116}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{12E}'),
        Some('\u{160}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{172}'),
        Some('\u{16A}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{17D}'),
        Some('\u{105}'),
        Some('\u{10D}'),
        Some('\u{119}'),
        Some('\u{117}'),
        Some('\u{12F}'),
        Some('\u{161}'),
        Some('\u{173}'),
        Some('\u{16B}'),
        Some('\u{17E}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{258C}'),
        Some('\u{2590}'),
        Some('\u{2580}'),
        Some('\u{D3}'),
        Some('\u{DF}'),
        Some('\u{14C}'),
        Some('\u{143}'),
        Some('\u{F5}'),
        Some('\u{D5}'),
        Some('\u{B5}'),
        Some('\u{144}'),
        Some('\u{136}'),
        Some('\u{137}'),
        Some('\u{13B}'),
        Some('\u{13C}'),
        Some('\u{146}'),
        Some('\u{112}'),
        Some('\u{145}'),
        Some('\u{2019}'),
        Some('\u{AD}'),
        Some('\u{B1}'),
        Some('\u{201C}'),
        Some('\u{BE}'),
        Some('\u{B6}'),
        Some('\u{A7}'),
        Some('\u{F7}'),
        Some('\u{201E}'),
        Some('\u{B0}'),
        Some('\u{2219}'),
        Some('\u{B7}'),
        Some('\u{B9}'),
        Some('\u{B3}'),
        Some('\u{B2}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];
    /// IBM437 (the original IBM PC / DOS codepage).
    const IBM437: [Option<char>; 128] = [
        Some('Ç'),
        Some('ü'),
        Some('é'),
        Some('â'),
        Some('ä'),
        Some('à'),
        Some('å'),
        Some('ç'),
        Some('ê'),
        Some('ë'),
        Some('è'),
        Some('ï'),
        Some('î'),
        Some('ì'),
        Some('Ä'),
        Some('Å'), //
        Some('É'),
        Some('æ'),
        Some('Æ'),
        Some('ô'),
        Some('ö'),
        Some('ò'),
        Some('û'),
        Some('ù'),
        Some('ÿ'),
        Some('Ö'),
        Some('Ü'),
        Some('¢'),
        Some('£'),
        Some('¥'),
        Some('₧'),
        Some('ƒ'), //
        Some('á'),
        Some('í'),
        Some('ó'),
        Some('ú'),
        Some('ñ'),
        Some('Ñ'),
        Some('ª'),
        Some('º'),
        Some('¿'),
        Some('⌐'),
        Some('¬'),
        Some('½'),
        Some('¼'),
        Some('¡'),
        Some('«'),
        Some('»'), //
        Some('░'),
        Some('▒'),
        Some('▓'),
        Some('│'),
        Some('┤'),
        Some('╡'),
        Some('╢'),
        Some('╖'),
        Some('╕'),
        Some('╣'),
        Some('║'),
        Some('╗'),
        Some('╝'),
        Some('╜'),
        Some('╛'),
        Some('┐'), //
        Some('└'),
        Some('┴'),
        Some('┬'),
        Some('├'),
        Some('─'),
        Some('┼'),
        Some('╞'),
        Some('╟'),
        Some('╚'),
        Some('╔'),
        Some('╩'),
        Some('╦'),
        Some('╠'),
        Some('═'),
        Some('╬'),
        Some('╧'), //
        Some('╨'),
        Some('╤'),
        Some('╥'),
        Some('╙'),
        Some('╘'),
        Some('╒'),
        Some('╓'),
        Some('╫'),
        Some('╪'),
        Some('┘'),
        Some('┌'),
        Some('█'),
        Some('▄'),
        Some('▌'),
        Some('▐'),
        Some('▀'), //
        Some('α'),
        Some('ß'),
        Some('Γ'),
        Some('π'),
        Some('Σ'),
        Some('σ'),
        Some('µ'),
        Some('τ'),
        Some('Φ'),
        Some('Θ'),
        Some('Ω'),
        Some('δ'),
        Some('∞'),
        Some('φ'),
        Some('ε'),
        Some('∩'), //
        Some('≡'),
        Some('±'),
        Some('≥'),
        Some('≤'),
        Some('⌠'),
        Some('⌡'),
        Some('÷'),
        Some('≈'),
        Some('°'),
        Some('∙'),
        Some('·'),
        Some('√'),
        Some('ⁿ'),
        Some('²'),
        Some('■'),
        Some('\u{A0}'),
    ];
    /// ISO-8859-1 (Latin-1) *is* `U+0000..=U+00FF`, C1 controls
    /// included, so its high half is the identity map. `encoding_rs`
    /// has no codec for it: the WHATWG `iso-8859-1` label resolves to
    /// windows-1252, which puts printable characters in the C1 range
    /// (#1508).
    const ISO8859_1: [Option<char>; 128] = {
        let mut t = [None; 128];
        let mut i = 0;
        while i < 128 {
            t[i] = char::from_u32(0x80 + i as u32);
            i += 1;
        }
        t
    };

    /// ISO-8859-9 (Latin-5, Turkish) is Latin-1 with six Icelandic
    /// letters swapped for Turkish ones. The WHATWG `iso-8859-9` label
    /// is windows-1254, which also replaces the whole C1 range.
    const ISO8859_9: [Option<char>; 128] = {
        let mut t = ISO8859_1;
        t[0xD0 - 0x80] = Some('\u{11E}'); // LATIN CAPITAL LETTER G WITH BREVE
        t[0xDD - 0x80] = Some('\u{130}'); // LATIN CAPITAL LETTER I WITH DOT ABOVE
        t[0xDE - 0x80] = Some('\u{15E}'); // LATIN CAPITAL LETTER S WITH CEDILLA
        t[0xF0 - 0x80] = Some('\u{11F}'); // LATIN SMALL LETTER G WITH BREVE
        t[0xFD - 0x80] = Some('\u{131}'); // LATIN SMALL LETTER DOTLESS I
        t[0xFE - 0x80] = Some('\u{15F}'); // LATIN SMALL LETTER S WITH CEDILLA
        t
    };

    /// ISO-8859-11 (Thai): TIS-620 with NBSP, so `0x80..=0xA0` stay C1
    /// and NBSP while `0xA1..=0xFB` are Thai — with eight cells
    /// (`0xDB..=0xDE`, `0xFC..=0xFF`) that are assigned no character at
    /// all, and which CRuby refuses as an undefined conversion rather
    /// than as invalid bytes. The WHATWG `iso-8859-11` label is
    /// windows-874, which fills five of the C1 slots in.
    const ISO8859_11: [Option<char>; 128] = [
        Some('\u{80}'),
        Some('\u{81}'),
        Some('\u{82}'),
        Some('\u{83}'),
        Some('\u{84}'),
        Some('\u{85}'),
        Some('\u{86}'),
        Some('\u{87}'),
        Some('\u{88}'),
        Some('\u{89}'),
        Some('\u{8A}'),
        Some('\u{8B}'),
        Some('\u{8C}'),
        Some('\u{8D}'),
        Some('\u{8E}'),
        Some('\u{8F}'),
        Some('\u{90}'),
        Some('\u{91}'),
        Some('\u{92}'),
        Some('\u{93}'),
        Some('\u{94}'),
        Some('\u{95}'),
        Some('\u{96}'),
        Some('\u{97}'),
        Some('\u{98}'),
        Some('\u{99}'),
        Some('\u{9A}'),
        Some('\u{9B}'),
        Some('\u{9C}'),
        Some('\u{9D}'),
        Some('\u{9E}'),
        Some('\u{9F}'),
        Some('\u{A0}'),
        Some('\u{E01}'),
        Some('\u{E02}'),
        Some('\u{E03}'),
        Some('\u{E04}'),
        Some('\u{E05}'),
        Some('\u{E06}'),
        Some('\u{E07}'),
        Some('\u{E08}'),
        Some('\u{E09}'),
        Some('\u{E0A}'),
        Some('\u{E0B}'),
        Some('\u{E0C}'),
        Some('\u{E0D}'),
        Some('\u{E0E}'),
        Some('\u{E0F}'),
        Some('\u{E10}'),
        Some('\u{E11}'),
        Some('\u{E12}'),
        Some('\u{E13}'),
        Some('\u{E14}'),
        Some('\u{E15}'),
        Some('\u{E16}'),
        Some('\u{E17}'),
        Some('\u{E18}'),
        Some('\u{E19}'),
        Some('\u{E1A}'),
        Some('\u{E1B}'),
        Some('\u{E1C}'),
        Some('\u{E1D}'),
        Some('\u{E1E}'),
        Some('\u{E1F}'),
        Some('\u{E20}'),
        Some('\u{E21}'),
        Some('\u{E22}'),
        Some('\u{E23}'),
        Some('\u{E24}'),
        Some('\u{E25}'),
        Some('\u{E26}'),
        Some('\u{E27}'),
        Some('\u{E28}'),
        Some('\u{E29}'),
        Some('\u{E2A}'),
        Some('\u{E2B}'),
        Some('\u{E2C}'),
        Some('\u{E2D}'),
        Some('\u{E2E}'),
        Some('\u{E2F}'),
        Some('\u{E30}'),
        Some('\u{E31}'),
        Some('\u{E32}'),
        Some('\u{E33}'),
        Some('\u{E34}'),
        Some('\u{E35}'),
        Some('\u{E36}'),
        Some('\u{E37}'),
        Some('\u{E38}'),
        Some('\u{E39}'),
        Some('\u{E3A}'),
        None,
        None,
        None,
        None,
        Some('\u{E3F}'),
        Some('\u{E40}'),
        Some('\u{E41}'),
        Some('\u{E42}'),
        Some('\u{E43}'),
        Some('\u{E44}'),
        Some('\u{E45}'),
        Some('\u{E46}'),
        Some('\u{E47}'),
        Some('\u{E48}'),
        Some('\u{E49}'),
        Some('\u{E4A}'),
        Some('\u{E4B}'),
        Some('\u{E4C}'),
        Some('\u{E4D}'),
        Some('\u{E4E}'),
        Some('\u{E4F}'),
        Some('\u{E50}'),
        Some('\u{E51}'),
        Some('\u{E52}'),
        Some('\u{E53}'),
        Some('\u{E54}'),
        Some('\u{E55}'),
        Some('\u{E56}'),
        Some('\u{E57}'),
        Some('\u{E58}'),
        Some('\u{E59}'),
        Some('\u{E5A}'),
        Some('\u{E5B}'),
        None,
        None,
        None,
        None,
    ];

    /// TIS-620 is the Thai standard ISO-8859-11 adds to: the same
    /// Thai half, with `0x80..=0xA0` assigned nothing at all — no C1
    /// controls and no NBSP. `encoding_rs` has no codec for it; the
    /// WHATWG `windows-874` label it used to borrow is Microsoft's
    /// extension, which fills ten of those cells in and disagrees
    /// with CRuby in all 33 (#1580).
    const TIS620: [Option<char>; 128] = {
        let mut t = ISO8859_11;
        let mut i = 0;
        while i <= 0x20 {
            t[i] = None;
            i += 1;
        }
        t
    };

    /// Windows-874 is ISO-8859-11's Thai half with Microsoft's C1
    /// row: the range is otherwise unassigned, and the eight cells
    /// ISO-8859-11 leaves empty stay empty. `encoding_rs`'s
    /// `windows-874` is WHATWG's, which differs from CRuby's in 23
    /// cells, so this is a table rather than that codec (#1567).
    const WINDOWS874: [Option<char>; 128] = {
        let mut t = ISO8859_11;
        let mut i = 0;
        while i < 0x20 {
            t[i] = None;
            i += 1;
        }
        t[0x00] = Some('\u{20AC}'); // EURO SIGN
        t[0x05] = Some('\u{2026}'); // HORIZONTAL ELLIPSIS
        t[0x11] = Some('\u{2018}');
        t[0x12] = Some('\u{2019}');
        t[0x13] = Some('\u{201C}');
        t[0x14] = Some('\u{201D}');
        t[0x15] = Some('\u{2022}');
        t[0x16] = Some('\u{2013}');
        t[0x17] = Some('\u{2014}');
        t
    };

    /// The three DOS code pages CRuby converts and `encoding_rs` has
    /// no codec for, read off CRuby byte by byte (#1567). IBM720
    /// (Arabic) leaves eight cells unassigned; the other two fill all
    /// 128. `CP852` / `IBM852` and `CP855` / `IBM855` are separate
    /// `Encoding` objects in CRuby with identical tables, so each
    /// pair shares one here.
    const IBM720: [Option<char>; 128] = [
        None,
        None,
        Some('\u{E9}'),
        Some('\u{E2}'),
        None,
        Some('\u{E0}'),
        None,
        Some('\u{E7}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{E8}'),
        Some('\u{EF}'),
        Some('\u{EE}'),
        None,
        None,
        None,
        None,
        Some('\u{651}'),
        Some('\u{652}'),
        Some('\u{F4}'),
        Some('\u{A4}'),
        Some('\u{640}'),
        Some('\u{FB}'),
        Some('\u{F9}'),
        Some('\u{621}'),
        Some('\u{622}'),
        Some('\u{623}'),
        Some('\u{624}'),
        Some('\u{A3}'),
        Some('\u{625}'),
        Some('\u{626}'),
        Some('\u{627}'),
        Some('\u{628}'),
        Some('\u{629}'),
        Some('\u{62A}'),
        Some('\u{62B}'),
        Some('\u{62C}'),
        Some('\u{62D}'),
        Some('\u{62E}'),
        Some('\u{62F}'),
        Some('\u{630}'),
        Some('\u{631}'),
        Some('\u{632}'),
        Some('\u{633}'),
        Some('\u{634}'),
        Some('\u{635}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{2561}'),
        Some('\u{2562}'),
        Some('\u{2556}'),
        Some('\u{2555}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{255C}'),
        Some('\u{255B}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{255E}'),
        Some('\u{255F}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{2567}'),
        Some('\u{2568}'),
        Some('\u{2564}'),
        Some('\u{2565}'),
        Some('\u{2559}'),
        Some('\u{2558}'),
        Some('\u{2552}'),
        Some('\u{2553}'),
        Some('\u{256B}'),
        Some('\u{256A}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{258C}'),
        Some('\u{2590}'),
        Some('\u{2580}'),
        Some('\u{636}'),
        Some('\u{637}'),
        Some('\u{638}'),
        Some('\u{639}'),
        Some('\u{63A}'),
        Some('\u{641}'),
        Some('\u{B5}'),
        Some('\u{642}'),
        Some('\u{643}'),
        Some('\u{644}'),
        Some('\u{645}'),
        Some('\u{646}'),
        Some('\u{647}'),
        Some('\u{648}'),
        Some('\u{649}'),
        Some('\u{64A}'),
        Some('\u{2261}'),
        Some('\u{64B}'),
        Some('\u{64C}'),
        Some('\u{64D}'),
        Some('\u{64E}'),
        Some('\u{64F}'),
        Some('\u{650}'),
        Some('\u{2248}'),
        Some('\u{B0}'),
        Some('\u{2219}'),
        Some('\u{B7}'),
        Some('\u{221A}'),
        Some('\u{207F}'),
        Some('\u{B2}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];
    const CP852: [Option<char>; 128] = [
        Some('\u{C7}'),
        Some('\u{FC}'),
        Some('\u{E9}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{16F}'),
        Some('\u{107}'),
        Some('\u{E7}'),
        Some('\u{142}'),
        Some('\u{EB}'),
        Some('\u{150}'),
        Some('\u{151}'),
        Some('\u{EE}'),
        Some('\u{179}'),
        Some('\u{C4}'),
        Some('\u{106}'),
        Some('\u{C9}'),
        Some('\u{139}'),
        Some('\u{13A}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{13D}'),
        Some('\u{13E}'),
        Some('\u{15A}'),
        Some('\u{15B}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{164}'),
        Some('\u{165}'),
        Some('\u{141}'),
        Some('\u{D7}'),
        Some('\u{10D}'),
        Some('\u{E1}'),
        Some('\u{ED}'),
        Some('\u{F3}'),
        Some('\u{FA}'),
        Some('\u{104}'),
        Some('\u{105}'),
        Some('\u{17D}'),
        Some('\u{17E}'),
        Some('\u{118}'),
        Some('\u{119}'),
        Some('\u{AC}'),
        Some('\u{17A}'),
        Some('\u{10C}'),
        Some('\u{15F}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{C1}'),
        Some('\u{C2}'),
        Some('\u{11A}'),
        Some('\u{15E}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{17B}'),
        Some('\u{17C}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{102}'),
        Some('\u{103}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{A4}'),
        Some('\u{111}'),
        Some('\u{110}'),
        Some('\u{10E}'),
        Some('\u{CB}'),
        Some('\u{10F}'),
        Some('\u{147}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{11B}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{162}'),
        Some('\u{16E}'),
        Some('\u{2580}'),
        Some('\u{D3}'),
        Some('\u{DF}'),
        Some('\u{D4}'),
        Some('\u{143}'),
        Some('\u{144}'),
        Some('\u{148}'),
        Some('\u{160}'),
        Some('\u{161}'),
        Some('\u{154}'),
        Some('\u{DA}'),
        Some('\u{155}'),
        Some('\u{170}'),
        Some('\u{FD}'),
        Some('\u{DD}'),
        Some('\u{163}'),
        Some('\u{B4}'),
        Some('\u{AD}'),
        Some('\u{2DD}'),
        Some('\u{2DB}'),
        Some('\u{2C7}'),
        Some('\u{2D8}'),
        Some('\u{A7}'),
        Some('\u{F7}'),
        Some('\u{B8}'),
        Some('\u{B0}'),
        Some('\u{A8}'),
        Some('\u{2D9}'),
        Some('\u{171}'),
        Some('\u{158}'),
        Some('\u{159}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];
    const CP855: [Option<char>; 128] = [
        Some('\u{452}'),
        Some('\u{402}'),
        Some('\u{453}'),
        Some('\u{403}'),
        Some('\u{451}'),
        Some('\u{401}'),
        Some('\u{454}'),
        Some('\u{404}'),
        Some('\u{455}'),
        Some('\u{405}'),
        Some('\u{456}'),
        Some('\u{406}'),
        Some('\u{457}'),
        Some('\u{407}'),
        Some('\u{458}'),
        Some('\u{408}'),
        Some('\u{459}'),
        Some('\u{409}'),
        Some('\u{45A}'),
        Some('\u{40A}'),
        Some('\u{45B}'),
        Some('\u{40B}'),
        Some('\u{45C}'),
        Some('\u{40C}'),
        Some('\u{45E}'),
        Some('\u{40E}'),
        Some('\u{45F}'),
        Some('\u{40F}'),
        Some('\u{44E}'),
        Some('\u{42E}'),
        Some('\u{44A}'),
        Some('\u{42A}'),
        Some('\u{430}'),
        Some('\u{410}'),
        Some('\u{431}'),
        Some('\u{411}'),
        Some('\u{446}'),
        Some('\u{426}'),
        Some('\u{434}'),
        Some('\u{414}'),
        Some('\u{435}'),
        Some('\u{415}'),
        Some('\u{444}'),
        Some('\u{424}'),
        Some('\u{433}'),
        Some('\u{413}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2591}'),
        Some('\u{2592}'),
        Some('\u{2593}'),
        Some('\u{2502}'),
        Some('\u{2524}'),
        Some('\u{445}'),
        Some('\u{425}'),
        Some('\u{438}'),
        Some('\u{418}'),
        Some('\u{2563}'),
        Some('\u{2551}'),
        Some('\u{2557}'),
        Some('\u{255D}'),
        Some('\u{439}'),
        Some('\u{419}'),
        Some('\u{2510}'),
        Some('\u{2514}'),
        Some('\u{2534}'),
        Some('\u{252C}'),
        Some('\u{251C}'),
        Some('\u{2500}'),
        Some('\u{253C}'),
        Some('\u{43A}'),
        Some('\u{41A}'),
        Some('\u{255A}'),
        Some('\u{2554}'),
        Some('\u{2569}'),
        Some('\u{2566}'),
        Some('\u{2560}'),
        Some('\u{2550}'),
        Some('\u{256C}'),
        Some('\u{A4}'),
        Some('\u{43B}'),
        Some('\u{41B}'),
        Some('\u{43C}'),
        Some('\u{41C}'),
        Some('\u{43D}'),
        Some('\u{41D}'),
        Some('\u{43E}'),
        Some('\u{41E}'),
        Some('\u{43F}'),
        Some('\u{2518}'),
        Some('\u{250C}'),
        Some('\u{2588}'),
        Some('\u{2584}'),
        Some('\u{41F}'),
        Some('\u{44F}'),
        Some('\u{2580}'),
        Some('\u{42F}'),
        Some('\u{440}'),
        Some('\u{420}'),
        Some('\u{441}'),
        Some('\u{421}'),
        Some('\u{442}'),
        Some('\u{422}'),
        Some('\u{443}'),
        Some('\u{423}'),
        Some('\u{436}'),
        Some('\u{416}'),
        Some('\u{432}'),
        Some('\u{412}'),
        Some('\u{44C}'),
        Some('\u{42C}'),
        Some('\u{2116}'),
        Some('\u{AD}'),
        Some('\u{44B}'),
        Some('\u{42B}'),
        Some('\u{437}'),
        Some('\u{417}'),
        Some('\u{448}'),
        Some('\u{428}'),
        Some('\u{44D}'),
        Some('\u{42D}'),
        Some('\u{449}'),
        Some('\u{429}'),
        Some('\u{447}'),
        Some('\u{427}'),
        Some('\u{A7}'),
        Some('\u{25A0}'),
        Some('\u{A0}'),
    ];

    /// macRoman (Mac OS Roman), and the seven tables below it, as
    /// CRuby's own converters have them — read off byte by byte. The
    /// Apple-logo cell (`0xF0` in most of the family) maps to no
    /// character, which CRuby reports as an undefined conversion.
    const MACROMAN: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C7}'),
        Some('\u{C9}'),
        Some('\u{D1}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{E1}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E3}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{ED}'),
        Some('\u{EC}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{F1}'),
        Some('\u{F3}'),
        Some('\u{F2}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F5}'),
        Some('\u{FA}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{B4}'),
        Some('\u{A8}'),
        Some('\u{2260}'),
        Some('\u{C6}'),
        Some('\u{D8}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{A5}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{2211}'),
        Some('\u{220F}'),
        Some('\u{3C0}'),
        Some('\u{222B}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{2126}'),
        Some('\u{E6}'),
        Some('\u{F8}'),
        Some('\u{BF}'),
        Some('\u{A1}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{C0}'),
        Some('\u{C3}'),
        Some('\u{D5}'),
        Some('\u{152}'),
        Some('\u{153}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{25CA}'),
        Some('\u{FF}'),
        Some('\u{178}'),
        Some('\u{2044}'),
        Some('\u{A4}'),
        Some('\u{2039}'),
        Some('\u{203A}'),
        Some('\u{FB01}'),
        Some('\u{FB02}'),
        Some('\u{2021}'),
        Some('\u{B7}'),
        Some('\u{201A}'),
        Some('\u{201E}'),
        Some('\u{2030}'),
        Some('\u{C2}'),
        Some('\u{CA}'),
        Some('\u{C1}'),
        Some('\u{CB}'),
        Some('\u{C8}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{CC}'),
        Some('\u{D3}'),
        Some('\u{D4}'),
        None,
        Some('\u{D2}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        Some('\u{131}'),
        Some('\u{2C6}'),
        Some('\u{2DC}'),
        Some('\u{AF}'),
        Some('\u{2D8}'),
        Some('\u{2D9}'),
        Some('\u{2DA}'),
        Some('\u{B8}'),
        Some('\u{2DD}'),
        Some('\u{2DB}'),
        Some('\u{2C7}'),
    ];
    /// macCyrillic.
    const MACCYRILLIC: [Option<char>; 128] = [
        Some('\u{410}'),
        Some('\u{411}'),
        Some('\u{412}'),
        Some('\u{413}'),
        Some('\u{414}'),
        Some('\u{415}'),
        Some('\u{416}'),
        Some('\u{417}'),
        Some('\u{418}'),
        Some('\u{419}'),
        Some('\u{41A}'),
        Some('\u{41B}'),
        Some('\u{41C}'),
        Some('\u{41D}'),
        Some('\u{41E}'),
        Some('\u{41F}'),
        Some('\u{420}'),
        Some('\u{421}'),
        Some('\u{422}'),
        Some('\u{423}'),
        Some('\u{424}'),
        Some('\u{425}'),
        Some('\u{426}'),
        Some('\u{427}'),
        Some('\u{428}'),
        Some('\u{429}'),
        Some('\u{42A}'),
        Some('\u{42B}'),
        Some('\u{42C}'),
        Some('\u{42D}'),
        Some('\u{42E}'),
        Some('\u{42F}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{406}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{402}'),
        Some('\u{452}'),
        Some('\u{2260}'),
        Some('\u{403}'),
        Some('\u{453}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{456}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{408}'),
        Some('\u{404}'),
        Some('\u{454}'),
        Some('\u{407}'),
        Some('\u{457}'),
        Some('\u{409}'),
        Some('\u{459}'),
        Some('\u{40A}'),
        Some('\u{45A}'),
        Some('\u{458}'),
        Some('\u{405}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{40B}'),
        Some('\u{45B}'),
        Some('\u{40C}'),
        Some('\u{45C}'),
        Some('\u{455}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{201E}'),
        Some('\u{40E}'),
        Some('\u{45E}'),
        Some('\u{40F}'),
        Some('\u{45F}'),
        Some('\u{2116}'),
        Some('\u{401}'),
        Some('\u{451}'),
        Some('\u{44F}'),
        Some('\u{430}'),
        Some('\u{431}'),
        Some('\u{432}'),
        Some('\u{433}'),
        Some('\u{434}'),
        Some('\u{435}'),
        Some('\u{436}'),
        Some('\u{437}'),
        Some('\u{438}'),
        Some('\u{439}'),
        Some('\u{43A}'),
        Some('\u{43B}'),
        Some('\u{43C}'),
        Some('\u{43D}'),
        Some('\u{43E}'),
        Some('\u{43F}'),
        Some('\u{440}'),
        Some('\u{441}'),
        Some('\u{442}'),
        Some('\u{443}'),
        Some('\u{444}'),
        Some('\u{445}'),
        Some('\u{446}'),
        Some('\u{447}'),
        Some('\u{448}'),
        Some('\u{449}'),
        Some('\u{44A}'),
        Some('\u{44B}'),
        Some('\u{44C}'),
        Some('\u{44D}'),
        Some('\u{44E}'),
        Some('\u{A4}'),
    ];
    /// macCroatian.
    const MACCROATIAN: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C7}'),
        Some('\u{C9}'),
        Some('\u{D1}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{E1}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E3}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{ED}'),
        Some('\u{EC}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{F1}'),
        Some('\u{F3}'),
        Some('\u{F2}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F5}'),
        Some('\u{FA}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{160}'),
        Some('\u{2122}'),
        Some('\u{B4}'),
        Some('\u{A8}'),
        Some('\u{2260}'),
        Some('\u{17D}'),
        Some('\u{D8}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{2206}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{2211}'),
        Some('\u{220F}'),
        Some('\u{161}'),
        Some('\u{222B}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{2126}'),
        Some('\u{17E}'),
        Some('\u{F8}'),
        Some('\u{BF}'),
        Some('\u{A1}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{106}'),
        Some('\u{AB}'),
        Some('\u{10C}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{C0}'),
        Some('\u{C3}'),
        Some('\u{D5}'),
        Some('\u{152}'),
        Some('\u{153}'),
        Some('\u{110}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{25CA}'),
        None,
        Some('\u{A9}'),
        Some('\u{2044}'),
        Some('\u{A4}'),
        Some('\u{2039}'),
        Some('\u{203A}'),
        Some('\u{C6}'),
        Some('\u{BB}'),
        Some('\u{2013}'),
        Some('\u{B7}'),
        Some('\u{201A}'),
        Some('\u{201E}'),
        Some('\u{2030}'),
        Some('\u{C2}'),
        Some('\u{107}'),
        Some('\u{C1}'),
        Some('\u{10D}'),
        Some('\u{C8}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{CC}'),
        Some('\u{D3}'),
        Some('\u{D4}'),
        Some('\u{111}'),
        Some('\u{D2}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        Some('\u{131}'),
        Some('\u{2C6}'),
        Some('\u{2DC}'),
        Some('\u{AF}'),
        Some('\u{3C0}'),
        Some('\u{CB}'),
        Some('\u{2DA}'),
        Some('\u{B8}'),
        Some('\u{CA}'),
        Some('\u{E6}'),
        Some('\u{2C7}'),
    ];
    /// macGreek.
    const MACGREEK: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{B9}'),
        Some('\u{B2}'),
        Some('\u{C9}'),
        Some('\u{B3}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{385}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{384}'),
        Some('\u{A8}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{A3}'),
        Some('\u{2122}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{2022}'),
        Some('\u{BD}'),
        Some('\u{2030}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{A6}'),
        Some('\u{AD}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{2020}'),
        Some('\u{393}'),
        Some('\u{394}'),
        Some('\u{398}'),
        Some('\u{39B}'),
        Some('\u{39E}'),
        Some('\u{3A0}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{3A3}'),
        Some('\u{3AA}'),
        Some('\u{A7}'),
        Some('\u{2260}'),
        Some('\u{B0}'),
        Some('\u{387}'),
        Some('\u{391}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{A5}'),
        Some('\u{392}'),
        Some('\u{395}'),
        Some('\u{396}'),
        Some('\u{397}'),
        Some('\u{399}'),
        Some('\u{39A}'),
        Some('\u{39C}'),
        Some('\u{3A6}'),
        Some('\u{3AB}'),
        Some('\u{3A8}'),
        Some('\u{3A9}'),
        Some('\u{3AC}'),
        Some('\u{39D}'),
        Some('\u{AC}'),
        Some('\u{39F}'),
        Some('\u{3A1}'),
        Some('\u{2248}'),
        Some('\u{3A4}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{3A5}'),
        Some('\u{3A7}'),
        Some('\u{386}'),
        Some('\u{388}'),
        Some('\u{153}'),
        Some('\u{2013}'),
        Some('\u{2015}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{389}'),
        Some('\u{38A}'),
        Some('\u{38C}'),
        Some('\u{38E}'),
        Some('\u{3AD}'),
        Some('\u{3AE}'),
        Some('\u{3AF}'),
        Some('\u{3CC}'),
        Some('\u{38F}'),
        Some('\u{3CD}'),
        Some('\u{3B1}'),
        Some('\u{3B2}'),
        Some('\u{3C8}'),
        Some('\u{3B4}'),
        Some('\u{3B5}'),
        Some('\u{3C6}'),
        Some('\u{3B3}'),
        Some('\u{3B7}'),
        Some('\u{3B9}'),
        Some('\u{3BE}'),
        Some('\u{3BA}'),
        Some('\u{3BB}'),
        Some('\u{3BC}'),
        Some('\u{3BD}'),
        Some('\u{3BF}'),
        Some('\u{3C0}'),
        Some('\u{3CE}'),
        Some('\u{3C1}'),
        Some('\u{3C3}'),
        Some('\u{3C4}'),
        Some('\u{3B8}'),
        Some('\u{3C9}'),
        Some('\u{3C2}'),
        Some('\u{3C7}'),
        Some('\u{3C5}'),
        Some('\u{3B6}'),
        Some('\u{3CA}'),
        Some('\u{3CB}'),
        Some('\u{390}'),
        Some('\u{3B0}'),
        None,
    ];
    /// macIceland.
    const MACICELAND: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C7}'),
        Some('\u{C9}'),
        Some('\u{D1}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{E1}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E3}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{ED}'),
        Some('\u{EC}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{F1}'),
        Some('\u{F3}'),
        Some('\u{F2}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F5}'),
        Some('\u{FA}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{DD}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{B4}'),
        Some('\u{A8}'),
        Some('\u{2260}'),
        Some('\u{C6}'),
        Some('\u{D8}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{A5}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{2211}'),
        Some('\u{220F}'),
        Some('\u{3C0}'),
        Some('\u{222B}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{2126}'),
        Some('\u{E6}'),
        Some('\u{F8}'),
        Some('\u{BF}'),
        Some('\u{A1}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{C0}'),
        Some('\u{C3}'),
        Some('\u{D5}'),
        Some('\u{152}'),
        Some('\u{153}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{25CA}'),
        Some('\u{FF}'),
        Some('\u{178}'),
        Some('\u{2044}'),
        Some('\u{A4}'),
        Some('\u{D0}'),
        Some('\u{F0}'),
        Some('\u{DE}'),
        Some('\u{FE}'),
        Some('\u{FD}'),
        Some('\u{B7}'),
        Some('\u{201A}'),
        Some('\u{201E}'),
        Some('\u{2030}'),
        Some('\u{C2}'),
        Some('\u{CA}'),
        Some('\u{C1}'),
        Some('\u{CB}'),
        Some('\u{C8}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{CC}'),
        Some('\u{D3}'),
        Some('\u{D4}'),
        None,
        Some('\u{D2}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        Some('\u{131}'),
        Some('\u{2C6}'),
        Some('\u{2DC}'),
        Some('\u{AF}'),
        Some('\u{2D8}'),
        Some('\u{2D9}'),
        Some('\u{2DA}'),
        Some('\u{B8}'),
        Some('\u{2DD}'),
        Some('\u{2DB}'),
        Some('\u{2C7}'),
    ];
    /// macRomania.
    const MACROMANIA: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C7}'),
        Some('\u{C9}'),
        Some('\u{D1}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{E1}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E3}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{ED}'),
        Some('\u{EC}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{F1}'),
        Some('\u{F3}'),
        Some('\u{F2}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F5}'),
        Some('\u{FA}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{B4}'),
        Some('\u{A8}'),
        Some('\u{2260}'),
        Some('\u{102}'),
        Some('\u{15E}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{A5}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{2211}'),
        Some('\u{220F}'),
        Some('\u{3C0}'),
        Some('\u{222B}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{2126}'),
        Some('\u{103}'),
        Some('\u{15F}'),
        Some('\u{BF}'),
        Some('\u{A1}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{C0}'),
        Some('\u{C3}'),
        Some('\u{D5}'),
        Some('\u{152}'),
        Some('\u{153}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{25CA}'),
        Some('\u{FF}'),
        Some('\u{178}'),
        Some('\u{2044}'),
        Some('\u{A4}'),
        Some('\u{2039}'),
        Some('\u{203A}'),
        Some('\u{162}'),
        Some('\u{163}'),
        Some('\u{2021}'),
        Some('\u{B7}'),
        Some('\u{201A}'),
        Some('\u{201E}'),
        Some('\u{2030}'),
        Some('\u{C2}'),
        Some('\u{CA}'),
        Some('\u{C1}'),
        Some('\u{CB}'),
        Some('\u{C8}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{CC}'),
        Some('\u{D3}'),
        Some('\u{D4}'),
        None,
        Some('\u{D2}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        Some('\u{131}'),
        Some('\u{2C6}'),
        Some('\u{2DC}'),
        Some('\u{AF}'),
        Some('\u{2D8}'),
        Some('\u{2D9}'),
        Some('\u{2DA}'),
        Some('\u{B8}'),
        Some('\u{2DD}'),
        Some('\u{2DB}'),
        Some('\u{2C7}'),
    ];
    /// macTurkish.
    const MACTURKISH: [Option<char>; 128] = [
        Some('\u{C4}'),
        Some('\u{C5}'),
        Some('\u{C7}'),
        Some('\u{C9}'),
        Some('\u{D1}'),
        Some('\u{D6}'),
        Some('\u{DC}'),
        Some('\u{E1}'),
        Some('\u{E0}'),
        Some('\u{E2}'),
        Some('\u{E4}'),
        Some('\u{E3}'),
        Some('\u{E5}'),
        Some('\u{E7}'),
        Some('\u{E9}'),
        Some('\u{E8}'),
        Some('\u{EA}'),
        Some('\u{EB}'),
        Some('\u{ED}'),
        Some('\u{EC}'),
        Some('\u{EE}'),
        Some('\u{EF}'),
        Some('\u{F1}'),
        Some('\u{F3}'),
        Some('\u{F2}'),
        Some('\u{F4}'),
        Some('\u{F6}'),
        Some('\u{F5}'),
        Some('\u{FA}'),
        Some('\u{F9}'),
        Some('\u{FB}'),
        Some('\u{FC}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{A2}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{DF}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{B4}'),
        Some('\u{A8}'),
        Some('\u{2260}'),
        Some('\u{C6}'),
        Some('\u{D8}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{A5}'),
        Some('\u{B5}'),
        Some('\u{2202}'),
        Some('\u{2211}'),
        Some('\u{220F}'),
        Some('\u{3C0}'),
        Some('\u{222B}'),
        Some('\u{AA}'),
        Some('\u{BA}'),
        Some('\u{2126}'),
        Some('\u{E6}'),
        Some('\u{F8}'),
        Some('\u{BF}'),
        Some('\u{A1}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{C0}'),
        Some('\u{C3}'),
        Some('\u{D5}'),
        Some('\u{152}'),
        Some('\u{153}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{25CA}'),
        Some('\u{FF}'),
        Some('\u{178}'),
        Some('\u{11E}'),
        Some('\u{11F}'),
        Some('\u{130}'),
        Some('\u{131}'),
        Some('\u{15E}'),
        Some('\u{15F}'),
        Some('\u{2021}'),
        Some('\u{B7}'),
        Some('\u{201A}'),
        Some('\u{201E}'),
        Some('\u{2030}'),
        Some('\u{C2}'),
        Some('\u{CA}'),
        Some('\u{C1}'),
        Some('\u{CB}'),
        Some('\u{C8}'),
        Some('\u{CD}'),
        Some('\u{CE}'),
        Some('\u{CF}'),
        Some('\u{CC}'),
        Some('\u{D3}'),
        Some('\u{D4}'),
        None,
        Some('\u{D2}'),
        Some('\u{DA}'),
        Some('\u{DB}'),
        Some('\u{D9}'),
        None,
        Some('\u{2C6}'),
        Some('\u{2DC}'),
        Some('\u{AF}'),
        Some('\u{2D8}'),
        Some('\u{2D9}'),
        Some('\u{2DA}'),
        Some('\u{B8}'),
        Some('\u{2DD}'),
        Some('\u{2DB}'),
        Some('\u{2C7}'),
    ];
    /// macUkraine.
    const MACUKRAINE: [Option<char>; 128] = [
        Some('\u{410}'),
        Some('\u{411}'),
        Some('\u{412}'),
        Some('\u{413}'),
        Some('\u{414}'),
        Some('\u{415}'),
        Some('\u{416}'),
        Some('\u{417}'),
        Some('\u{418}'),
        Some('\u{419}'),
        Some('\u{41A}'),
        Some('\u{41B}'),
        Some('\u{41C}'),
        Some('\u{41D}'),
        Some('\u{41E}'),
        Some('\u{41F}'),
        Some('\u{420}'),
        Some('\u{421}'),
        Some('\u{422}'),
        Some('\u{423}'),
        Some('\u{424}'),
        Some('\u{425}'),
        Some('\u{426}'),
        Some('\u{427}'),
        Some('\u{428}'),
        Some('\u{429}'),
        Some('\u{42A}'),
        Some('\u{42B}'),
        Some('\u{42C}'),
        Some('\u{42D}'),
        Some('\u{42E}'),
        Some('\u{42F}'),
        Some('\u{2020}'),
        Some('\u{B0}'),
        Some('\u{490}'),
        Some('\u{A3}'),
        Some('\u{A7}'),
        Some('\u{2022}'),
        Some('\u{B6}'),
        Some('\u{406}'),
        Some('\u{AE}'),
        Some('\u{A9}'),
        Some('\u{2122}'),
        Some('\u{402}'),
        Some('\u{452}'),
        Some('\u{2260}'),
        Some('\u{403}'),
        Some('\u{453}'),
        Some('\u{221E}'),
        Some('\u{B1}'),
        Some('\u{2264}'),
        Some('\u{2265}'),
        Some('\u{456}'),
        Some('\u{B5}'),
        Some('\u{491}'),
        Some('\u{408}'),
        Some('\u{404}'),
        Some('\u{454}'),
        Some('\u{407}'),
        Some('\u{457}'),
        Some('\u{409}'),
        Some('\u{459}'),
        Some('\u{40A}'),
        Some('\u{45A}'),
        Some('\u{458}'),
        Some('\u{405}'),
        Some('\u{AC}'),
        Some('\u{221A}'),
        Some('\u{192}'),
        Some('\u{2248}'),
        Some('\u{2206}'),
        Some('\u{AB}'),
        Some('\u{BB}'),
        Some('\u{2026}'),
        Some('\u{A0}'),
        Some('\u{40B}'),
        Some('\u{45B}'),
        Some('\u{40C}'),
        Some('\u{45C}'),
        Some('\u{455}'),
        Some('\u{2013}'),
        Some('\u{2014}'),
        Some('\u{201C}'),
        Some('\u{201D}'),
        Some('\u{2018}'),
        Some('\u{2019}'),
        Some('\u{F7}'),
        Some('\u{201E}'),
        Some('\u{40E}'),
        Some('\u{45E}'),
        Some('\u{40F}'),
        Some('\u{45F}'),
        Some('\u{2116}'),
        Some('\u{401}'),
        Some('\u{451}'),
        Some('\u{44F}'),
        Some('\u{430}'),
        Some('\u{431}'),
        Some('\u{432}'),
        Some('\u{433}'),
        Some('\u{434}'),
        Some('\u{435}'),
        Some('\u{436}'),
        Some('\u{437}'),
        Some('\u{438}'),
        Some('\u{439}'),
        Some('\u{43A}'),
        Some('\u{43B}'),
        Some('\u{43C}'),
        Some('\u{43D}'),
        Some('\u{43E}'),
        Some('\u{43F}'),
        Some('\u{440}'),
        Some('\u{441}'),
        Some('\u{442}'),
        Some('\u{443}'),
        Some('\u{444}'),
        Some('\u{445}'),
        Some('\u{446}'),
        Some('\u{447}'),
        Some('\u{448}'),
        Some('\u{449}'),
        Some('\u{44A}'),
        Some('\u{44B}'),
        Some('\u{44C}'),
        Some('\u{44D}'),
        Some('\u{44E}'),
        Some('\u{A4}'),
    ];

    use crate::value::Encoding as E;
    match enc {
        E::Iso8859(1) => Some(&ISO8859_1),
        E::Iso8859(9) => Some(&ISO8859_9),
        E::Iso8859(11) => Some(&ISO8859_11),
        E::NamedByte(_) => match enc.name() {
            "IBM437" => Some(&IBM437),
            "IBM720" => Some(&IBM720),
            "CP850" => Some(&CP850),
            "IBM737" => Some(&IBM737),
            "IBM775" => Some(&IBM775),
            // The three Thai encodings differ only in that row:
            // TIS-620 assigns none of it, ISO-8859-11 adds the C1
            // controls and NBSP, Windows-874 adds NBSP and ten
            // punctuation cells.
            "TIS-620" => Some(&TIS620),
            "Windows-874" => Some(&WINDOWS874),
            "CP852" | "IBM852" => Some(&CP852),
            "CP855" | "IBM855" => Some(&CP855),
            "macRoman" => Some(&MACROMAN),
            "macCyrillic" => Some(&MACCYRILLIC),
            "macCroatian" => Some(&MACCROATIAN),
            "macGreek" => Some(&MACGREEK),
            "macIceland" => Some(&MACICELAND),
            "macRomania" => Some(&MACROMANIA),
            "macTurkish" => Some(&MACTURKISH),
            "macUkraine" => Some(&MACUKRAINE),
            // `macCentEuro` and `macThai` have no table: CRuby ships no
            // converter for either, so a conversion is a
            // `ConverterNotFoundError` rather than a mapping (#1471).
            _ => None,
        },
        _ => None,
    }
}

/// Unicode → JIS X 0212 (the three-byte `0x8F` plane of EUC-JP).
///
/// Built from `encoding_rs`'s own decoder rather than a table in the
/// tree: every `8F xx yy` in the 94×94 space is decoded once, and the
/// cells that come back as a single character give the mapping. Where
/// two cells decode to the same character the lower one wins, which is
/// the order CRuby's table has them in.
fn jisx0212_reverse() -> &'static std::collections::HashMap<char, [u8; 3]> {
    static MAP: std::sync::OnceLock<std::collections::HashMap<char, [u8; 3]>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| {
        let mut m = std::collections::HashMap::new();
        for b2 in 0xa1u8..=0xfe {
            for b3 in 0xa1u8..=0xfe {
                let seq = [0x8fu8, b2, b3];
                // Through `eucjp_decode`, not `encoding_rs` directly:
                // the map has to be keyed by the character CRuby says
                // the cell holds. `8F A2 B7` is the one that moves —
                // it leaves as `U+FF5E` and arrives as `U+007E` — and
                // that is exactly the entry that must not exist, since
                // `U+FF5E` has no EUC-JP form in CRuby at all.
                let d = jp_decode(&EUCJP_FIXUP, &seq, None);
                if d.had_invalid || d.unmapped.is_some() {
                    continue;
                }
                let decoded = d.text;
                let mut chars = decoded.chars();
                if let (Some(c), None) = (chars.next(), chars.next()) {
                    m.entry(c).or_insert(seq);
                }
            }
        }
        m
    })
}

/// Where CRuby's Japanese codecs differ from the WHATWG ones
/// `encoding_rs` implements. Two kinds of difference, in both
/// directions:
///
/// - **the duplicate mappings.** Seven glyphs have two Unicode homes
///   each (EM DASH / HORIZONTAL BAR, WAVE DASH / FULLWIDTH TILDE, …)
///   and the two standards picked different ones. CRuby takes the
///   plain half, WHATWG the fullwidth half.
/// - **the extension rows.** CRuby's `EUC-JP` and `Shift_JIS` tables
///   have nothing in the NEC and IBM rows; WHATWG's fill them in.
///
/// `Windows-31J` is the encoding that genuinely *has* those rows and
/// the fullwidth readings, so there CRuby and WHATWG already agree —
/// it carries an almost-empty fixup rather than none at all, because
/// one character still has to be refused.
///
/// Every entry was read off a full round trip of the encoding's cell
/// space against CRuby 4.0.6 (17735 cells for EUC-JP, 11343 for
/// Shift_JIS), not transcribed from a table.
#[derive(Clone, Copy)]
pub(crate) struct JpFixup {
    /// The encoding these corrections are for.
    enc: crate::value::Encoding,
    /// The WHATWG codec this one corrects.
    rs: &'static encoding_rs::Encoding,
    /// The encoding's `precise_mbclen` (#1444), which is what cuts the
    /// buffer into cells.
    precise: fn(&[u8], usize) -> PreciseLen,
    /// Whether the encoding has a second plane to fall back on when
    /// `encoding_rs` will not write a character at all — EUC-JP's JIS
    /// X 0212, reached through [`jisx0212_reverse`].
    second_plane: bool,
    /// A complete cell → the character CRuby's table holds.
    decode: &'static [(&'static [u8], char)],
    /// A character → the cell CRuby writes for it. **Not** the mirror
    /// of `decode`: CRuby's tables are many-to-one on the way in, so
    /// `U+2014` and `U+2015` both land on the same cell. Only the
    /// characters `encoding_rs` gets wrong are listed.
    encode: &'static [(char, &'static [u8])],
    /// Characters CRuby's table has no cell for at all — the other
    /// half of each duplicate pair. `encoding_rs` writes each one into
    /// the cell that now reads back as its twin, so they have to be
    /// refused explicitly.
    reject: &'static [char],
    /// Lead-byte ranges whose rows CRuby's table does not have.
    dead_rows: &'static [(u8, u8)],
    /// Whether this encoding carries Windows-31J's user-defined area,
    /// whose way back is [`windows31j_pua_cell`] rather than a table.
    pua: bool,
}

/// Windows-31J's user-defined area: rows `F0`..`F9` run consecutively
/// through `U+E000..U+E757`, 188 cells to a row (`40..7E` then
/// `80..FC`). `encoding_rs` decodes them and refuses to encode them
/// back, so all 1880 are a one-way trip; the mapping is arithmetic
/// rather than a table, so this is the whole of the way back (#1462).
///
/// Rows `FA`..`FC` are *not* this area — they are the NEC-selected IBM
/// extensions, real characters that `encoding_rs` already writes.
fn windows31j_pua_cell(c: char) -> Option<[u8; 2]> {
    const ROWS: u32 = 10;
    const CELLS_PER_ROW: u32 = 188;
    let idx = (c as u32).checked_sub(0xE000)?;
    if idx >= ROWS * CELLS_PER_ROW {
        return None;
    }
    let hi = 0xF0 + (idx / CELLS_PER_ROW) as u8;
    let col = (idx % CELLS_PER_ROW) as u8;
    // The trail byte skips `0x7F`, as every Shift_JIS cell does.
    let lo = if col < 0x7f - 0x40 {
        0x40 + col
    } else {
        0x80 + (col - (0x7f - 0x40))
    };
    Some([hi, lo])
}

static EUCJP_FIXUP: JpFixup = JpFixup {
    enc: crate::value::Encoding::EUC_JP,
    rs: encoding_rs::EUC_JP,
    precise: eucjp_precise_len,
    second_plane: true,
    decode: &[
        (&[0x8f, 0xa2, 0xb7], '\u{007e}'), // TILDE, not FULLWIDTH TILDE
        (&[0xa1, 0xbd], '\u{2014}'),       // EM DASH, not HORIZONTAL BAR
        (&[0xa1, 0xc1], '\u{301c}'),       // WAVE DASH, not FULLWIDTH TILDE
        (&[0xa1, 0xc2], '\u{2016}'),       // DOUBLE VERTICAL LINE, not PARALLEL TO
        (&[0xa1, 0xdd], '\u{2212}'),       // MINUS SIGN, not FULLWIDTH HYPHEN-MINUS
        (&[0xa1, 0xf1], '\u{00a2}'),       // CENT SIGN, not FULLWIDTH CENT SIGN
        (&[0xa1, 0xf2], '\u{00a3}'),       // POUND SIGN, not FULLWIDTH POUND SIGN
        (&[0xa2, 0xcc], '\u{00ac}'),       // NOT SIGN, not FULLWIDTH NOT SIGN
    ],
    // `U+2212` already reaches `A1 DD` through `encoding_rs`, so it is
    // not repeated here.
    encode: &[
        ('\u{00a2}', &[0xa1, 0xf1]),
        ('\u{00a3}', &[0xa1, 0xf2]),
        ('\u{00ac}', &[0xa2, 0xcc]),
        ('\u{2014}', &[0xa1, 0xbd]),
        ('\u{2016}', &[0xa1, 0xc2]),
        ('\u{301c}', &[0xa1, 0xc1]),
    ],
    // `encoding_rs` writes U+00A5 as `5C` and U+203E as `7E` — JIS
    // X 0201's readings of those two ASCII positions. CRuby's table
    // has no cell for either character, so both are refused (and
    // `5C` / `7E` read back as backslash and tilde, not as these).
    reject: &[
        '\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}', '\u{a5}',
        '\u{203e}',
    ],
    // WHATWG fills `A9..AD` and `F9..FC`; CRuby maps nothing in either
    // range, 457 cells in all.
    dead_rows: &[(0xa9, 0xaf), (0xf5, 0xfe)],
    pua: false,
};

static SJIS_FIXUP: JpFixup = JpFixup {
    enc: crate::value::Encoding::Sjis(0),
    rs: encoding_rs::SHIFT_JIS,
    precise: sjis_precise_len,
    second_plane: false,
    decode: &[
        (&[0x81, 0x5c], '\u{2014}'),
        (&[0x81, 0x60], '\u{301c}'),
        (&[0x81, 0x61], '\u{2016}'),
        (&[0x81, 0x7c], '\u{2212}'),
        (&[0x81, 0x91], '\u{00a2}'),
        (&[0x81, 0x92], '\u{00a3}'),
        (&[0x81, 0xca], '\u{00ac}'),
    ],
    // `U+2212` already reaches `81 7C`, as in EUC-JP.
    encode: &[
        ('\u{00a2}', &[0x81, 0x91]),
        ('\u{00a3}', &[0x81, 0x92]),
        ('\u{00ac}', &[0x81, 0xca]),
        ('\u{2014}', &[0x81, 0x5c]),
        ('\u{2016}', &[0x81, 0x61]),
        ('\u{301c}', &[0x81, 0x60]),
    ],
    // U+00A5 and U+203E as above, plus U+0080: `encoding_rs` carries
    // it through as the byte `80`, which Shift_JIS does not have a
    // character at (#1500 is the same byte on the way in).
    reject: &[
        '\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}', '\u{80}', '\u{a5}',
        '\u{203e}',
    ],
    // Row 13 (`87`), the NEC-selected IBM rows (`ED`/`EE`) and the
    // user-defined + IBM rows (`F0`..`FC`) — 2725 cells.
    dead_rows: &[(0x87, 0x87), (0xed, 0xee), (0xf0, 0xfc)],
    pua: false,
};

/// Windows-31J agrees with WHATWG everywhere except one character:
/// `encoding_rs` writes `U+2212` to `81 7C`, which in this encoding is
/// `U+FF0D`'s cell and nothing else's.
static WINDOWS31J_FIXUP: JpFixup = JpFixup {
    enc: crate::value::Encoding::Sjis(crate::value::WINDOWS_31J),
    rs: encoding_rs::SHIFT_JIS,
    precise: sjis_precise_len,
    second_plane: false,
    decode: &[],
    encode: &[],
    // U+0080 / U+00A5 / U+203E as for Shift_JIS above.
    reject: &['\u{2212}', '\u{80}', '\u{a5}', '\u{203e}'],
    dead_rows: &[],
    // Windows-31J alone carries the user-defined area; plain Shift_JIS
    // has nothing in those rows and refuses them in both directions.
    pua: true,
};

/// EUC-JP and Shift_JIS spell the same JIS X 0208 plane, so CRuby
/// converts between them cell to cell and never asks which Unicode
/// character is involved — `Encoding::Converter#convpath` reports no
/// pivot for the pair. The mapping is arithmetic rather than a table,
/// and it reaches the 1957 cells per direction whose Unicode home
/// CRuby's own tables do not have, which a pivot cannot follow
/// (#1460). Windows-31J is *not* in this: CRuby pivots that pair, and
/// `EUC-JP → Windows-31J` of a row-13 cell is an undefined conversion
/// where `EUC-JP → Shift_JIS` of the same is `87 40`.
pub(crate) fn jis_direct_from_euc(
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> Option<bool> {
    use crate::value::Encoding as E;
    match (src_enc, dst_enc) {
        // `Sjis(0)` is Shift_JIS proper; the other payloads are
        // Windows-31J / CP932 and MacJapanese, which CRuby pivots — as
        // it pivots EUC-JP's own variants, CP51932 and eucJP-ms, whose
        // extension rows Shift_JIS has no cells for (#1530).
        (E::EucJp(_), E::Sjis(0)) if src_enc == E::EUC_JP => Some(true),
        (E::Sjis(0), E::EucJp(_)) if dst_enc == E::EUC_JP => Some(false),
        _ => None,
    }
}

/// What one character costs on the way in, and what it spells on the
/// way out — or which way it is refused.
pub(crate) enum JisCell {
    /// Bytes to write, and how many source bytes they came from.
    Cell(Vec<u8>, usize),
    /// Well-formed for the source, with no home in the destination:
    /// EUC-JP's JIS X 0212 plane, or a Shift_JIS cell outside the
    /// 94×94 grid.
    Undefined(usize),
    /// Not well-formed for the source encoding at all.
    Invalid(usize),
}

/// One character of a direct EUC-JP ↔ Shift_JIS conversion.
pub(crate) fn jis_direct_one(bytes: &[u8], from_euc: bool) -> JisCell {
    let b0 = bytes[0];
    if b0 < 0x80 {
        return JisCell::Cell(vec![b0], 1);
    }
    if from_euc {
        // `8E` prefixes a half-width katakana, which Shift_JIS spells
        // as the single byte on its own.
        if b0 == 0x8E {
            return match bytes.get(1) {
                Some(&k) if (0xA1..=0xDF).contains(&k) => JisCell::Cell(vec![k], 2),
                Some(_) => JisCell::Invalid(2),
                None => JisCell::Invalid(1),
            };
        }
        // `8F` prefixes the JIS X 0212 plane, which Shift_JIS has no
        // room for at all.
        if b0 == 0x8F {
            return match (bytes.get(1), bytes.get(2)) {
                (Some(&b1), Some(&b2))
                    if (0xA1..=0xFE).contains(&b1) && (0xA1..=0xFE).contains(&b2) =>
                {
                    JisCell::Undefined(3)
                }
                (Some(_), Some(_)) => JisCell::Invalid(3),
                _ => JisCell::Invalid(bytes.len().min(2)),
            };
        }
        if !(0xA1..=0xFE).contains(&b0) {
            return JisCell::Invalid(1);
        }
        let Some(&b1) = bytes.get(1) else {
            return JisCell::Invalid(1);
        };
        if !(0xA1..=0xFE).contains(&b1) {
            return JisCell::Invalid(2);
        }
        // Row and cell are 1..=94 either side; only the spelling of
        // the pair differs.
        let (row, cell) = ((b0 - 0xA0) as u32, (b1 - 0xA0) as u32);
        let s1 = if row <= 62 {
            (row + 257) / 2
        } else {
            (row + 385) / 2
        };
        let s2 = if row % 2 == 1 {
            cell + 63 + u32::from(cell >= 64)
        } else {
            cell + 158
        };
        JisCell::Cell(vec![s1 as u8, s2 as u8], 2)
    } else {
        // Shift_JIS spells half-width katakana in one byte.
        if (0xA1..=0xDF).contains(&b0) {
            return JisCell::Cell(vec![0x8E, b0], 1);
        }
        let lead = match b0 {
            0x81..=0x9F => 2 * b0 as u32 - 257,
            0xE0..=0xFC => 2 * b0 as u32 - 385,
            _ => return JisCell::Invalid(1),
        };
        let Some(&b1) = bytes.get(1) else {
            return JisCell::Invalid(1);
        };
        if !(0x40..=0xFC).contains(&b1) || b1 == 0x7F {
            return JisCell::Invalid(if (0x40..=0xFC).contains(&b1) { 2 } else { 1 });
        }
        // The trail byte says which of the two rows this lead covers.
        let (row, cell) = if b1 <= 0x9E {
            (lead, b1 as u32 - 63 - u32::from(b1 >= 0x80))
        } else {
            (lead + 1, b1 as u32 - 158)
        };
        // Shift_JIS reaches past the 94×94 grid (the IBM extension
        // rows); EUC-JP's two-byte form does not.
        if !(1..=94).contains(&row) || !(1..=94).contains(&cell) {
            return JisCell::Undefined(2);
        }
        JisCell::Cell(vec![(row + 0xA0) as u8, (cell + 0xA0) as u8], 2)
    }
}

/// A destination whose characters go through CRuby's tables one at a
/// time rather than the codec in bulk — the Japanese pair since
/// #1445, and the CJK table encodings since #1544. The streaming
/// branch below is the same either way; only this differs.
enum DstEncoder {
    Jp(&'static JpFixup),
    Table(&'static cjk::CellTable, &'static encoding_rs::Encoding),
}

impl DstEncoder {
    /// The bytes for *c*, or the character the destination has no
    /// cell for.
    fn one(&self, c: char) -> std::result::Result<Vec<u8>, char> {
        let mut buf = [0u8; 4];
        match self {
            DstEncoder::Jp(fx) => jp_encode(fx, c.encode_utf8(&mut buf)),
            DstEncoder::Table(tab, rs) => table_cell_encode(tab, rs, c).ok_or(c),
        }
    }
}

fn dst_encoder(dst_enc: crate::value::Encoding) -> Option<DstEncoder> {
    if let Some(fx) = jp_fixup(dst_enc) {
        return Some(DstEncoder::Jp(fx));
    }
    let tab = cell_table(dst_enc)?;
    Some(DstEncoder::Table(tab, encoding_to_rs(dst_enc)?))
}

/// What CRuby writes for *c* in a table encoding: its own cell where
/// the codec's differs, the codec's where the table holds it, and
/// nothing where it does not (#1544).
pub(crate) fn table_cell_encode(
    tab: &cjk::CellTable,
    dst_rs: &'static encoding_rs::Encoding,
    c: char,
) -> Option<Vec<u8>> {
    if tab.refuses(c) {
        return None;
    }
    if let Some(b) = tab.write(c) {
        return Some(b);
    }
    let mut buf = [0u8; 4];
    let (bytes, _, err) = dst_rs.encode(c.encode_utf8(&mut buf));
    if err {
        return None;
    }
    // The codec's own repertoire is wider, so a cell it writes has to
    // be one CRuby's table holds. A single byte is not a cell — GBK
    // spells the euro sign as `80` — and neither is ASCII.
    match cell_key(&bytes) {
        Some(cell) if !tab.holds(cell) => None,
        _ => Some(bytes.into_owned()),
    }
}

/// CRuby's own table for a CJK encoding whose codec here is a
/// different one: `encoding_rs`'s `gb2312` is GBK and its `big5`
/// reaches rows CRuby has nothing in, so the codec reads and writes
/// cells the encoding does not have (#1544). The tables are generated
/// from CRuby by `bin/gen-cjk-tables`.
pub(crate) fn cell_table(enc: crate::value::Encoding) -> Option<&'static cjk::CellTable> {
    use crate::value::Encoding as E;
    // The generator diffs CRuby against the codec, so it needs a build
    // that answers from the codec alone.
    if cfg!(feature = "no-cjk-tables") {
        return None;
    }
    match enc {
        E::NamedByte(i) => match crate::value::named_byte_const_name(i) {
            "GB2312" => Some(&cjk::GB2312),
            "GBK" => Some(&cjk::GBK),
            "Big5" => Some(&cjk::BIG5),
            // The other two Big5s read through the same `big5` codec:
            // HKSCS differs from WHATWG's HKSCS-2008 in a few cells
            // and UAO in thousands, and both have rows below `0xA1`
            // that Big5 does not (#1500, #1520).
            "Big5_HKSCS" => Some(&cjk::BIG5_HKSCS),
            "Big5_UAO" => Some(&cjk::BIG5_UAO),
            // GB2312's grid with the traditional forms in it, read
            // through `gbk` as GB2312 is (#1520).
            "GB12345" => Some(&cjk::GB12345),
            // Microsoft's Big5 rather than CRuby's: thousands of
            // extra cells and a best-fit encoder, so they carry
            // tables of their own (#1567).
            "CP950" => Some(&cjk::CP950),
            "CP951" => Some(&cjk::CP951),
            // No grid of its own: GB18030 reads every cell CRuby
            // does, and only writes a handful differently.
            "GB18030" => Some(&cjk::GB18030),
            _ => None,
        },
        _ => None,
    }
}

/// A two-byte cell as the tables key them; anything else is not one.
fn cell_key(piece: &[u8]) -> Option<u16> {
    match piece {
        [b1, b2] => Some(((*b1 as u16) << 8) | *b2 as u16),
        _ => None,
    }
}

/// CP51932 is Windows-31J's table in EUC form: EUC-JP's two-byte
/// plane with NEC row 13 and the NEC-selected IBM extension rows
/// (`0xAD`, `0xF9..=0xFC`) on top, the seven cells JIS and Windows
/// read differently spelled the Windows way (`A1 C1` is U+FF5E, not
/// U+301C), and no JIS X 0212 plane: CRuby's transcoder neither
/// writes the three-byte `0x8F` form nor reads it, so a character
/// that lives there only is undefined into it, and the byte is
/// malformed out of it (#1520, #1530). The tables are read off CRuby
/// by `bin/gen-cp51932-table`.
static CP51932_FIXUP: std::sync::LazyLock<JpFixup> = std::sync::LazyLock::new(|| JpFixup {
    enc: cp51932_enc(),
    precise: crate::value::rvalue::cp51932_transcoder_len,
    second_plane: false,
    decode: &cp51932::CP51932_DECODE,
    encode: &cp51932::CP51932_ENCODE,
    reject: &cp51932::CP51932_REJECT,
    // WHATWG fills `A9..AC` and `AE..AF` too; CRuby maps nothing in
    // either, nor in `F5..F8` and `FD..FE`.
    dead_rows: &[(0xa9, 0xac), (0xae, 0xaf), (0xf5, 0xf8), (0xfd, 0xfe)],
    ..EUCJP_FIXUP
});

pub(crate) fn jp_fixup(enc: crate::value::Encoding) -> Option<&'static JpFixup> {
    use crate::value::Encoding as E;
    match enc {
        E::EucJp(i) if i == crate::value::euc_jp_variant_index("CP51932") => Some(&CP51932_FIXUP),
        E::EucJp(_) => Some(&EUCJP_FIXUP),
        E::Sjis(0) => Some(&SJIS_FIXUP),
        // MacJapanese has no converter in CRuby at all, so it gets no
        // fixup — and no transcoding path — here either (#1471).
        E::Sjis(2) => None,
        E::Sjis(_) => Some(&WINDOWS31J_FIXUP),
        _ => None,
    }
}

/// Whether `encoding_rs` put this cell in a row the encoding has.
fn jp_cell_is_live(fx: &JpFixup, cell: &[u8]) -> bool {
    cell.first().is_none_or(|lead| {
        !fx.dead_rows
            .iter()
            .any(|&(lo, hi)| (lo..=hi).contains(lead))
    })
}

/// CRuby's answer for one complete cell, or `None` where it agrees
/// with `encoding_rs`.
fn jp_decode_override(fx: &JpFixup, cell: &[u8]) -> Option<char> {
    // Every table is sorted (`fixup_tables_are_sorted` checks), and
    // CP51932's runs to hundreds of cells.
    fx.decode
        .binary_search_by(|(seq, _)| (*seq).cmp(cell))
        .ok()
        .map(|i| fx.decode[i].1)
}

/// The cell CRuby writes `c` into where `encoding_rs` would not.
fn jp_encode_override(fx: &JpFixup, c: char) -> Option<&'static [u8]> {
    fx.encode
        .binary_search_by(|(k, _)| k.cmp(&c))
        .ok()
        .map(|i| fx.encode[i].1)
}

/// Whether this buffer holds anything [`jp_decode`] must treat
/// differently from `encoding_rs` — an allocation-free walk, so the
/// common case pays one extra scan and nothing else.
fn jp_decode_needs_fixup(fx: &JpFixup, bytes: &[u8]) -> bool {
    let mut pos = 0;
    while pos < bytes.len() {
        match (fx.precise)(bytes, pos) {
            PreciseLen::Char(n) => {
                let cell = &bytes[pos..pos + n];
                if !jp_cell_is_live(fx, cell) || jp_decode_override(fx, cell).is_some() {
                    return true;
                }
                pos += n;
            }
            // Not a character: `encoding_rs` decides what to do with
            // it, exactly as it does today.
            _ => pos += 1,
        }
    }
    false
}

/// What [`jp_decode`] found.
pub(crate) struct JpDecoded<'a> {
    pub(crate) text: std::borrow::Cow<'a, str>,
    /// An ill-formed byte sequence was seen (`invalid:` territory).
    pub(crate) had_invalid: bool,
    /// The first cell that is **well formed** but has no character in
    /// CRuby's table — an extension row. That is an
    /// `UndefinedConversionError`, not an invalid sequence:
    /// `"\xF9\xA1".force_encoding("EUC-JP").valid_encoding?` is
    /// `true`, and `invalid: :replace` does not suppress it. `None`
    /// when the caller asked for those to be replaced instead.
    pub(crate) unmapped: Option<Vec<u8>>,
    /// Where `unmapped`'s cell starts in the input. The streaming
    /// path needs it to say how much it consumed and to emit what
    /// converted before it (#1461); the one-shot path only reports.
    pub(crate) unmapped_at: Option<usize>,
    /// Where the first ill-formed piece starts, for the same reason.
    pub(crate) invalid_at: Option<usize>,
}

/// The `Encoding` a fixup belongs to, for the character walk its
/// `precise` came from.
fn jp_enc_of(fx: &JpFixup) -> crate::value::Encoding {
    fx.enc
}

/// Decode the way CRuby does, by wrapping `encoding_rs`'s WHATWG codec
/// rather than replacing it.
///
/// Everything that is not a duplicate-mapping cell or an extension row
/// is handed to `encoding_rs` in runs, so its answers — including how
/// it groups an invalid sequence into replacement characters — are
/// untouched. Both encodings are stateless and the runs are cut at
/// complete cells, so decoding in pieces gives the same result as
/// decoding the whole buffer.
///
/// `undef` is the replacement text for an unmapped cell when the
/// caller passed `undef: :replace`; without it the first such cell is
/// reported back so the caller can raise.
pub(crate) fn jp_decode<'a>(fx: &JpFixup, bytes: &'a [u8], undef: Option<&str>) -> JpDecoded<'a> {
    cell_decode(jp_enc_of(fx), fx.rs, Some(fx), bytes, undef)
}

/// Decode through `encoding_rs`, letting the encoding's own character
/// walk decide what is well formed and its own table decide what has
/// a character.
///
/// `encoding_rs` carries WHATWG's tables, which differ from CRuby's in
/// two ways that both surface here. They read bytes CRuby's encodings
/// do not have — Shift_JIS's `0x80` as `U+0080`, EUC-KR's `B0 41`
/// through `windows-949`, GB2312's through `gbk` — and the walk, which
/// is what `#valid_encoding?` and `#scrub` answer from, is what says
/// so (#1473, #1500). And they read *cells* CRuby has no character
/// for, which is an **undefined** conversion rather than an invalid
/// sequence, and so is not covered by `invalid: :replace`.
///
/// A buffer that is well formed, touches no fixup cell and decodes
/// cleanly is handed to `encoding_rs` whole, so this costs one
/// classify on the common path. Everything else is walked piece by
/// piece, complete cells going to the codec in runs — exact, because
/// these encodings are stateless and the runs are cut at cell
/// boundaries.
///
/// `fx` carries the per-cell corrections where the encoding has any
/// (EUC-JP / Shift_JIS); `undef` is the replacement text for a cell
/// with no character when the caller passed `undef: :replace`, and
/// without it the first such cell is reported back so the caller can
/// raise.
pub(crate) fn cell_decode<'a>(
    enc: crate::value::Encoding,
    rs: &'static encoding_rs::Encoding,
    fx: Option<&JpFixup>,
    bytes: &'a [u8],
    undef: Option<&str>,
) -> JpDecoded<'a> {
    use crate::value::rvalue::MbcPiece;
    let plain = |bytes: &'a [u8]| {
        let (text, had_invalid) = rs.decode_without_bom_handling(bytes);
        JpDecoded {
            text,
            had_invalid,
            unmapped: None,
            unmapped_at: None,
            invalid_at: None,
        }
    };
    let Some((max_len, precise)) = conversion_walker(enc) else {
        return plain(bytes);
    };
    // A table encoding reads every cell through its own table, so the
    // buffer only skips the careful path when it holds no cell at all.
    let needs_fixup = fx.is_some_and(|fx| jp_decode_needs_fixup(fx, bytes))
        || (cell_table(enc).is_some() && bytes.iter().any(|&b| b >= 0x80));
    let broken = matches!(enc.classify(bytes), crate::value::CodeRange::Broken);
    if !needs_fixup && !broken {
        let d = plain(bytes);
        // No error means no cell the codec could not read, and the
        // walk already accepted every one of them.
        if !d.had_invalid {
            return d;
        }
    }
    let base = bytes.as_ptr() as usize;
    let mut pieces: Vec<(usize, usize, bool)> = Vec::new();
    let _ = crate::value::rvalue::walk_mbc(bytes, max_len, precise, |piece| {
        let (b, ok) = match piece {
            MbcPiece::Char(b) => (b, true),
            MbcPiece::Bad(b) => (b, false),
        };
        pieces.push((b.as_ptr() as usize - base, b.len(), ok));
        Ok(())
    });
    let mut out = String::with_capacity(bytes.len());
    let mut had_invalid = false;
    let mut unmapped: Option<Vec<u8>> = None;
    let mut unmapped_at: Option<usize> = None;
    let mut invalid_at: Option<usize> = None;
    let mut run: Option<std::ops::Range<usize>> = None;
    // A run of plain cells, decoded together; if the codec stumbles
    // anywhere in it the cells are taken one at a time, so the one it
    // cannot read can be named.
    macro_rules! flush {
        () => {
            if let Some(r) = run.take() {
                let (s, e) = rs.decode_without_bom_handling(&bytes[r.clone()]);
                if !e {
                    out.push_str(&s);
                } else {
                    let mut p = r.start;
                    while p < r.end {
                        let PreciseLen::Char(n) = precise(bytes, p) else {
                            break;
                        };
                        let cell = &bytes[p..p + n];
                        let (s, e) = rs.decode_without_bom_handling(cell);
                        if e {
                            // CP949's `0x80` is the one byte in the
                            // family whose character table and
                            // transcoder disagree in CRuby: it is
                            // `valid_encoding?`-valid and still an
                            // *invalid byte sequence* to convert, where
                            // every other unreadable cell here is an
                            // undefined conversion.
                            if cell == [0x80] && enc.name() == "CP949" {
                                out.push('\u{FFFD}');
                                had_invalid = true;
                                invalid_at.get_or_insert(p);
                                p += n;
                                continue;
                            }
                            match undef {
                                Some(repl) => out.push_str(repl),
                                None => {
                                    if unmapped.is_none() {
                                        unmapped = Some(cell.to_vec());
                                        unmapped_at = Some(p);
                                    }
                                }
                            }
                        } else {
                            out.push_str(&s);
                        }
                        p += n;
                    }
                }
            }
        };
    }
    for (start, len, ok) in pieces {
        let piece = &bytes[start..start + len];
        if !ok {
            flush!();
            // One replacement character per ill-formed piece, which is
            // how `encoding_rs` groups them too.
            out.push('\u{FFFD}');
            had_invalid = true;
            invalid_at.get_or_insert(start);
            continue;
        }
        if let Some(fx) = fx {
            if let Some(c) = jp_decode_override(fx, piece) {
                flush!();
                out.push(c);
                continue;
            }
            if !jp_cell_is_live(fx, piece) {
                flush!();
                match undef {
                    Some(repl) => out.push_str(repl),
                    None => {
                        if unmapped.is_none() {
                            unmapped = Some(piece.to_vec());
                            unmapped_at = Some(start);
                        }
                    }
                }
                continue;
            }
        }
        if let Some(tab) = cell_table(enc)
            && let Some(cell) = cell_key(piece)
        {
            if let Some(c) = tab.read(cell) {
                flush!();
                out.push(c);
                continue;
            }
            if !tab.holds(cell) {
                // A cell CRuby's table does not have: well formed for
                // the walk, and no character.
                flush!();
                match undef {
                    Some(repl) => out.push_str(repl),
                    None => {
                        if unmapped.is_none() {
                            unmapped = Some(piece.to_vec());
                            unmapped_at = Some(start);
                        }
                    }
                }
                continue;
            }
        }
        match &mut run {
            Some(r) => r.end = start + len,
            None => run = Some(start..start + len),
        }
    }
    flush!();
    JpDecoded {
        text: std::borrow::Cow::Owned(out),
        had_invalid,
        unmapped,
        unmapped_at,
        invalid_at,
    }
}

/// The conversion chain CRuby names in a pivoted error message: every
/// non-UTF-8 source reaches the pivot as "<src> to UTF-8", except
/// ISO-2022-JP, whose transcoder goes the long way round.
pub(crate) fn pivot_chain(src_enc: crate::value::Encoding) -> String {
    if let Some(chain) = jis_family_chain(src_enc, crate::value::Encoding::UTF8) {
        chain_names(&chain)
    } else if let Some(utf8_form) = carrier_utf8_form(src_enc) {
        // A `UTF8-*` carrier reads straight into UTF-8; an `SJIS-*`
        // one goes through its vendor's `UTF8-*` first (#1530).
        if utf8_form == src_enc {
            format!("{} to UTF-8", src_enc.name())
        } else {
            format!("{} to {} to UTF-8", src_enc.name(), utf8_form.name())
        }
    } else if src_enc == crate::value::Encoding::EUC_JP {
        "EUC-JP to UTF-8".to_string()
    } else {
        format!("{} to UTF-8", src_enc.name())
    }
}

/// `opts` with the source an error message should name pinned to
/// `src` — the wrapper's own encoding rather than the intermediate it
/// rewrote its bytes into (#1609).
pub(crate) fn reporting_as(opts: &TranscodeOpts, src: crate::value::Encoding) -> TranscodeOpts {
    let mut o = opts.clone();
    o.report_src = Some(o.report_src.unwrap_or(src));
    o
}

/// How many of `stateless`'s bytes CRuby has read when an
/// ISO-2022-JP destination of `max` bytes runs out: it stops on the
/// character whose output does not fit, *having read it*, so the
/// answer is the shortest prefix whose ISO-2022-JP form is longer
/// than the cap. The whole input when none is — the overflow is then
/// inside the closing escape, which no character owns (#1609).
fn jis_read_through(
    write: fn(&[u8], Option<u8>, bool) -> std::result::Result<(Vec<u8>, Option<u8>), usize>,
    stateless: &[u8],
    state: Option<u8>,
    max: usize,
) -> usize {
    (1..=stateless.len())
        .find(|&n| write(&stateless[..n], state, false).is_ok_and(|(out, _)| out.len() > max))
        .unwrap_or(stateless.len())
}

/// stateless-ISO-2022-JP, the hop ISO-2022-JP and EUC-JP meet at.
fn stateless_enc() -> crate::value::Encoding {
    crate::value::Encoding::NamedByte(
        crate::value::named_byte_index("STATELESS_ISO_2022_JP").unwrap_or(0),
    )
}

/// stateless-ISO-2022-JP-KDDI, the hop ISO-2022-JP-KDDI is written
/// from.
pub(crate) fn stateless_kddi_enc() -> crate::value::Encoding {
    crate::value::Encoding::NamedByte(
        crate::value::named_byte_index("STATELESS_ISO_2022_JP_KDDI").unwrap_or(0),
    )
}

/// UTF8-KDDI, which stateless-ISO-2022-JP-KDDI is written from in
/// CRuby's `convpath`.
pub(crate) fn utf8_kddi_enc() -> crate::value::Encoding {
    crate::value::Encoding::Utf8(crate::value::UTF8_KDDI)
}

/// CP51932, the EUC-JP variant CP50220 and CP50221 are written from.
fn cp51932_enc() -> crate::value::Encoding {
    crate::value::Encoding::EucJp(crate::value::euc_jp_variant_index("CP51932"))
}

fn eucjp_to_cp50220_from(
    bytes: &[u8],
    start: Option<u8>,
    close: bool,
) -> std::result::Result<(Vec<u8>, Option<u8>), usize> {
    crate::value::eucjp_to_cp5022x_from(bytes, start, close, true)
}

fn eucjp_to_cp50221_from(
    bytes: &[u8],
    start: Option<u8>,
    close: bool,
) -> std::result::Result<(Vec<u8>, Option<u8>), usize> {
    crate::value::eucjp_to_cp5022x_from(bytes, start, close, false)
}

/// stateless-ISO-2022-JP-KDDI: CP51932's two-byte cells behind a
/// `0x92` lead — as stateless-ISO-2022-JP's are EUC-JP's — with the
/// KDDI emoji in rows `0xF5..=0xFB`, over the IBM extension cells
/// there, which still write. CRuby converts it to and from UTF8-KDDI,
/// where the emoji are private-use characters, and its transcoder is
/// a byte table: a cell no character lives in is a *malformed*
/// sequence rather than an undefined one, and the run reported is
/// the longest prefix a cell could still begin with — `"\x92\xA4"`
/// followed by `"\xF4"` (#1530).
///
/// The character in a cell: an emoji first, else CP51932's.
fn kddi_cell_char(cell: [u8; 2]) -> Option<char> {
    let key = u16::from_be_bytes(cell);
    let emoji = &kddi::KDDI_ISO2022_DECODE;
    if let Ok(i) = emoji.binary_search_by(|(k, _)| k.cmp(&key)) {
        return char::from_u32(emoji[i].1);
    }
    let d = jp_decode(&CP51932_FIXUP, &cell, None);
    if d.had_invalid || d.unmapped.is_some() {
        return None;
    }
    let mut it = d.text.chars();
    let c = it.next()?;
    it.next().is_none().then_some(c)
}

/// The cell a UTF8-KDDI character is written into, if any: the emoji
/// table for a private-use character, CP51932's plane for the rest.
pub(crate) fn kddi_char_cell(c: char) -> Option<[u8; 2]> {
    let emoji = &kddi::KDDI_ISO2022_ENCODE;
    if let Ok(i) = emoji.binary_search_by(|(k, _)| k.cmp(&(c as u32))) {
        return Some(emoji[i].1.to_be_bytes());
    }
    // The duplicates JIS X 0208 and NEC row 13 both hold go into the
    // NEC cell here, the JIS one in CP51932.
    let prefer = &kddi::KDDI_ISO2022_PREFER;
    if let Ok(i) = prefer.binary_search_by(|(k, _)| k.cmp(&(c as u32))) {
        return Some(prefer[i].1.to_be_bytes());
    }
    let mut buf = [0u8; 4];
    match jp_encode(&CP51932_FIXUP, c.encode_utf8(&mut buf))
        .ok()?
        .as_slice()
    {
        [b1 @ 0xa1..=0xfe, b2 @ 0xa1..=0xfe] => Some([*b1, *b2]),
        _ => None,
    }
}

/// Whether `enc` is ISO-2022-JP-KDDI, the wrapper around this encoding.
pub(crate) fn kddi_wrapper(enc: crate::value::Encoding) -> bool {
    jis_wrapper(enc).is_some_and(|w| w.inner == stateless_kddi_enc())
}

/// The rows a cell can lie in: CP51932's, and the emoji rows.
fn kddi_row_exists(row: u8) -> bool {
    matches!(row, 0xa1..=0xa8 | 0xad | 0xb0..=0xfc)
}

/// The walk CRuby's stateless-ISO-2022-JP-KDDI transcoder reads
/// with: ASCII, or `0x92` and a cell a character lives in. A lead
/// with a row but no cell yet is `NeedMore`, which is what makes the
/// malformed run `"\x92\xA4"` rather than `"\x92"`.
fn kddi_transcode_len(bytes: &[u8], pos: usize) -> PreciseLen {
    match bytes.get(pos) {
        None => PreciseLen::NeedMore,
        Some(0x00..=0x7f) => PreciseLen::Char(1),
        Some(0x92) => match (bytes.get(pos + 1), bytes.get(pos + 2)) {
            (None, _) => PreciseLen::NeedMore,
            (Some(&row), _) if !kddi_row_exists(row) => PreciseLen::Invalid,
            (Some(_), None) => PreciseLen::NeedMore,
            (Some(&row), Some(&col)) if kddi_cell_char([row, col]).is_some() => PreciseLen::Char(3),
            _ => PreciseLen::Invalid,
        },
        Some(_) => PreciseLen::Invalid,
    }
}

/// The UTF8-KDDI of a stateless-ISO-2022-JP-KDDI buffer's well-formed
/// prefix, each unit's width on both sides, and where that prefix
/// ends.
pub(crate) fn kddi_read(bytes: &[u8]) -> (Vec<u8>, Vec<(usize, usize)>, usize) {
    let mut out = Vec::with_capacity(bytes.len());
    let mut units = Vec::new();
    let mut at = 0;
    loop {
        match kddi_transcode_len(bytes, at) {
            PreciseLen::Char(1) => {
                out.push(bytes[at]);
                units.push((1, 1));
                at += 1;
            }
            PreciseLen::Char(n) => {
                let c = kddi_cell_char([bytes[at + 1], bytes[at + 2]])
                    .expect("the walk only accepts a cell with a character");
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                out.extend_from_slice(s.as_bytes());
                units.push((n, s.len()));
                at += n;
            }
            _ => break,
        }
    }
    (out, units, at)
}

/// How many source bytes `out_consumed` bytes of the UTF8-KDDI stand
/// for — whole units only, as [`stateless_len_for_eucjp`] counts.
fn kddi_src_len(units: &[(usize, usize)], out_consumed: usize) -> usize {
    let (mut s_at, mut o_at) = (0, 0);
    for &(s, o) in units {
        if o_at + o > out_consumed {
            break;
        }
        o_at += o;
        s_at += s;
    }
    s_at
}

/// The outcome for the malformed run at `good`: consumed along with
/// the bytes read to disprove it, which are held for `#putback`, and
/// named against the hop into UTF8-KDDI whatever the conversion's
/// source is called — ISO-2022-JP-KDDI's wrapper hands its cells over
/// as this encoding's, and CRuby's errinfo says so.
fn kddi_bad_source(
    src_bytes: &[u8],
    good: usize,
    partial_input: bool,
) -> (StreamConvertResult, usize, ErrMeta) {
    let (kind, mut meta) =
        bad_source_outcome(stateless_kddi_enc(), &src_bytes[good..], !partial_input);
    meta.stage = Some((
        "stateless-ISO-2022-JP-KDDI".to_string(),
        "UTF8-KDDI".to_string(),
    ));
    let through = through_bad_run(good, &kind, &meta, src_bytes.len());
    (kind, through, meta)
}

/// Where a malformed run at `at` leaves the source: consumed, along
/// with the bytes read to disprove it — those are held for `#putback`
/// rather than left in `src` — where a pending one stays for the next
/// call to finish.
fn through_bad_run(at: usize, kind: &StreamConvertResult, meta: &ErrMeta, len: usize) -> usize {
    if matches!(kind, StreamConvertResult::InvalidByteSequence) {
        (at + meta.error_bytes.len() + meta.readagain_bytes.len()).min(len)
    } else {
        at
    }
}

/// `"\xEF\xBD\xB1" to stateless-ISO-2022-JP-KDDI in conversion from
/// UTF-8 to UTF8-KDDI to stateless-ISO-2022-JP-KDDI`: the message for
/// a character UTF8-KDDI holds and stateless-ISO-2022-JP-KDDI has no
/// cell for. The hop is a byte table, so CRuby quotes the character's
/// bytes rather than naming its codepoint.
pub(crate) fn kddi_undefined_message(
    c: char,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let chain = jis_family_chain(src_enc, dst_enc)
        .unwrap_or_else(|| vec![src_enc, utf8_kddi_enc(), stateless_kddi_enc()]);
    let mut buf = [0u8; 4];
    format!(
        "{} to stateless-ISO-2022-JP-KDDI in conversion from {}",
        quote_error_bytes(c.encode_utf8(&mut buf).as_bytes()),
        chain_names(&chain)
    )
}

/// IBM037 (EBCDIC, US / Canada): the Latin-1 character each byte
/// stands for. A permutation of `U+0000..=U+00FF`, so the way back is
/// its inverse. Read off CRuby 4.0.6.
static IBM037_TO_LATIN1: [u8; 256] = [
    0x00, 0x01, 0x02, 0x03, 0x9c, 0x09, 0x86, 0x7f, 0x97, 0x8d, 0x8e, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x9d, 0x85, 0x08, 0x87, 0x18, 0x19, 0x92, 0x8f, 0x1c, 0x1d, 0x1e, 0x1f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x0a, 0x17, 0x1b, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x05, 0x06, 0x07,
    0x90, 0x91, 0x16, 0x93, 0x94, 0x95, 0x96, 0x04, 0x98, 0x99, 0x9a, 0x9b, 0x14, 0x15, 0x9e, 0x1a,
    0x20, 0xa0, 0xe2, 0xe4, 0xe0, 0xe1, 0xe3, 0xe5, 0xe7, 0xf1, 0xa2, 0x2e, 0x3c, 0x28, 0x2b, 0x7c,
    0x26, 0xe9, 0xea, 0xeb, 0xe8, 0xed, 0xee, 0xef, 0xec, 0xdf, 0x21, 0x24, 0x2a, 0x29, 0x3b, 0xac,
    0x2d, 0x2f, 0xc2, 0xc4, 0xc0, 0xc1, 0xc3, 0xc5, 0xc7, 0xd1, 0xa6, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,
    0xf8, 0xc9, 0xca, 0xcb, 0xc8, 0xcd, 0xce, 0xcf, 0xcc, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,
    0xd8, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0xab, 0xbb, 0xf0, 0xfd, 0xfe, 0xb1,
    0xb0, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0xaa, 0xba, 0xe6, 0xb8, 0xc6, 0xa4,
    0xb5, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0xa1, 0xbf, 0xd0, 0xdd, 0xde, 0xae,
    0x5e, 0xa3, 0xa5, 0xb7, 0xa9, 0xa7, 0xb6, 0xbc, 0xbd, 0xbe, 0x5b, 0x5d, 0xaf, 0xa8, 0xb4, 0xd7,
    0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0xad, 0xf4, 0xf6, 0xf2, 0xf3, 0xf5,
    0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0xb9, 0xfb, 0xfc, 0xf9, 0xfa, 0xff,
    0x5c, 0xf7, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0xb2, 0xd4, 0xd6, 0xd2, 0xd3, 0xd5,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0xb3, 0xdb, 0xdc, 0xd9, 0xda, 0x9f,
];

/// IBM037, the one EBCDIC code page CRuby names: a dummy, not
/// ASCII-compatible, converted through ISO-8859-1 — `convpath` is
/// `UTF-8 → ISO-8859-1 → IBM037` — with a table that is a permutation
/// of Latin-1, so every byte reads and every Latin-1 character writes,
/// and anything above U+00FF is refused at the ISO-8859-1 hop (#1530).
pub(crate) fn is_ibm037(enc: crate::value::Encoding) -> bool {
    matches!(enc, crate::value::Encoding::Other(_)) && enc.name() == "IBM037"
}

fn ibm037_enc() -> crate::value::Encoding {
    crate::value::Encoding::Other(5)
}

/// The IBM037 byte for a Latin-1 character.
fn latin1_to_ibm037(c: char) -> Option<u8> {
    let cp = c as u32;
    if cp > 0xff {
        return None;
    }
    IBM037_TO_LATIN1
        .iter()
        .position(|&l| l as u32 == cp)
        .map(|i| i as u8)
}

/// ISO-8859-1 bytes as IBM037's: the table's inverse, byte for byte.
pub(crate) fn latin1_bytes_to_ibm037(bytes: &[u8]) -> Vec<u8> {
    bytes
        .iter()
        .map(|&b| latin1_to_ibm037(b as char).unwrap_or(b))
        .collect()
}

/// IBM037 bytes as the UTF-8 of the Latin-1 characters they stand for.
pub(crate) fn ibm037_to_utf8(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|&b| IBM037_TO_LATIN1[b as usize] as char)
        .collect()
}

/// `U+3042 to ISO-8859-1 in conversion from UTF-8 to ISO-8859-1 to
/// IBM037`: the hop that gives up is the one into Latin-1.
fn ibm037_undefined_message(c: char, src_enc: crate::value::Encoding) -> String {
    let chain =
        jis_family_chain(src_enc, ibm037_enc()).unwrap_or_else(|| vec![src_enc, ibm037_enc()]);
    format!(
        "U+{:04X} to ISO-8859-1 in conversion from {}",
        c as u32,
        chain_names(&chain)
    )
}

/// A stateful encoding that is escape sequences around the cells of a
/// stateless one: ISO-2022-JP around stateless-ISO-2022-JP,
/// ISO-2022-JP-KDDI around stateless-ISO-2022-JP-KDDI, and CP50220 /
/// CP50221 around CP51932 — which is the hop CRuby's `convpath` names
/// for each. The conversion in and out is that rewrite, with the
/// designation in effect carried from one chunk to the next, and the
/// rest is `inner`'s own conversion (#1609, #1520, #1530).
#[derive(Clone, Copy)]
pub(crate) struct JisWrapper {
    pub(crate) inner: crate::value::Encoding,
    /// The escapes read: the stateless bytes, and the designation left
    /// in effect.
    pub(crate) read: fn(
        &[u8],
        Option<u8>,
    )
        -> std::result::Result<(Vec<u8>, Option<u8>), crate::value::Iso2022JpStop>,
    /// The escapes written, closing back to ASCII when asked.
    pub(crate) write:
        fn(&[u8], Option<u8>, bool) -> std::result::Result<(Vec<u8>, Option<u8>), usize>,
}

pub(crate) fn jis_wrapper(enc: crate::value::Encoding) -> Option<JisWrapper> {
    use crate::value::Encoding as E;
    match enc {
        E::Iso2022Jp => Some(JisWrapper {
            inner: stateless_enc(),
            read: crate::value::iso2022jp_to_stateless_from,
            write: crate::value::stateless_to_iso2022jp_from,
        }),
        E::Other(_) => match enc.name() {
            "ISO-2022-JP-KDDI" => Some(JisWrapper {
                inner: stateless_kddi_enc(),
                read: crate::value::iso2022jp_to_stateless_from,
                write: crate::value::stateless_to_iso2022jp_from,
            }),
            "CP50220" => Some(JisWrapper {
                inner: cp51932_enc(),
                read: crate::value::cp5022x_to_eucjp_from,
                write: eucjp_to_cp50220_from,
            }),
            "CP50221" => Some(JisWrapper {
                inner: cp51932_enc(),
                read: crate::value::cp5022x_to_eucjp_from,
                write: eucjp_to_cp50221_from,
            }),
            _ => None,
        },
        _ => None,
    }
}

/// The name CRuby's transcoder for a single-byte table carries, which
/// is the encoding's name in upper case for the Windows code pages and
/// the Mac ones — `WINDOWS-874`, `MACROMAN` — and the name itself
/// everywhere else. It is what `primitive_errinfo` names, and a
/// conversion whose end is spelled differently from the encoding
/// asked for is written out in full: `U+3042 to WINDOWS-874 in
/// conversion from UTF-8 to WINDOWS-874` (#1530).
pub(crate) fn transcoder_spelling(name: &str) -> String {
    if name.starts_with("mac") || (name.starts_with("Windows-") && name != "Windows-31J") {
        name.to_uppercase()
    } else {
        name.to_string()
    }
}

/// `"\xEE\x97\x8D" from UTF8-KDDI to UTF-8`, `"\xEE\x97\x8D" to UTF-8 in
/// conversion from SJIS-KDDI to UTF8-KDDI to UTF-8 to EUC-JP`: a
/// carrier emoji with no Unicode meaning is refused on the way out of
/// the vendor's `UTF8-*` encoding, and quoted, since that hop is a
/// table (#1530).
pub(crate) fn carrier_no_unicode_message(
    pua: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let quoted = quote_error_bytes(pua);
    if dst_enc == crate::value::Encoding::UTF8 && carrier_utf8_form(src_enc) == Some(src_enc) {
        format!("{quoted} from {} to UTF-8", src_enc.name())
    } else if dst_enc == crate::value::Encoding::UTF8 {
        format!(
            "{quoted} to UTF-8 in conversion from {}",
            pivot_chain(src_enc)
        )
    } else {
        format!(
            "{quoted} to UTF-8 in conversion from {} to {}",
            pivot_chain(src_enc),
            transcoder_spelling(dst_enc.name())
        )
    }
}

/// The `UTF8-*` spelling of a carrier's character, when the character
/// is one the carrier holds: the bytes the hop out of that encoding
/// quotes when it has no Unicode for it.
pub(crate) fn carrier_pua_bytes(
    own: &[u8],
    src_enc: crate::value::Encoding,
    store: &Store,
) -> Option<Vec<u8>> {
    let utf8_form = carrier_utf8_form(src_enc)?;
    if utf8_form == src_enc {
        return Some(own.to_vec());
    }
    transcode_bytes_with_opts(own, src_enc, utf8_form, &TranscodeOpts::default(), store).ok()
}

/// The hops of a chain, as an error message spells them.
pub(crate) fn chain_names(chain: &[crate::value::Encoding]) -> String {
    (0..chain.len())
        .map(|i| chain_hop_name(chain, i))
        .collect::<Vec<_>>()
        .join(" to ")
}

/// The name of hop `i` of a chain: the encoding's, except that CRuby's
/// transcoder *out of* CP50220 / CP50221 calls its destination
/// `cp51932`, in lower case, where the one into them says `CP51932`.
fn chain_hop_name(chain: &[crate::value::Encoding], i: usize) -> String {
    use crate::value::Encoding as E;
    if i == 1 && chain[1] == cp51932_enc() && matches!(chain[0], E::Other(1) | E::Other(2)) {
        return "cp51932".to_string();
    }
    transcoder_spelling(chain[i].name())
}

/// The one character `bytes` are the UTF-8 of, if they are.
fn single_utf8_char(bytes: &[u8]) -> Option<char> {
    let s = std::str::from_utf8(bytes).ok()?;
    let mut it = s.chars();
    let c = it.next()?;
    it.next().is_none().then_some(c)
}

/// The message and stage names for a character a wrapper's inner
/// conversion refused: CRuby names the hop that gave up and spells
/// the whole chain — `U+00A5 to CP51932 in conversion from UTF-8 to
/// CP51932 to CP50220`, `U+9AD9 to EUC-JP in conversion from UTF-8 to
/// EUC-JP to stateless-ISO-2022-JP to ISO-2022-JP` — and a cell
/// stateless-ISO-2022-JP itself has no room for is quoted as the
/// bytes EUC-JP wrote (#1609, #1520).
pub(crate) fn wrapper_dst_undefined(
    meta: &mut ErrMeta,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    w: JisWrapper,
) {
    use crate::value::Encoding as E;
    if w.inner == stateless_kddi_enc() {
        // The hop into stateless-ISO-2022-JP-KDDI is a byte table from
        // UTF8-KDDI, and its refusal is worded by the inner stream
        // against the shorter chain: only the chain grows (#1530).
        if meta
            .stage
            .as_ref()
            .is_some_and(|(_, d)| d == "stateless-ISO-2022-JP-KDDI")
            && let Some(c) = single_utf8_char(&meta.error_bytes)
        {
            meta.message = Some(kddi_undefined_message(c, src_enc, dst_enc));
        }
        return;
    }
    let chain = jis_family_chain(src_enc, dst_enc).unwrap_or_else(|| vec![src_enc, dst_enc]);
    let names = chain_names(&chain);
    let (stage, message) = match single_utf8_char(&meta.error_bytes) {
        Some(c) if !meta.decode_stage => {
            // Refused on the way into the inner encoding's own pivot.
            let hop_dst = if stateless_iso2022jp(w.inner).is_some() {
                E::EUC_JP
            } else {
                w.inner
            };
            let hop_src = chain
                .iter()
                .position(|&e| e == hop_dst)
                .and_then(|i| i.checked_sub(1))
                .map_or(E::UTF8, |i| chain[i]);
            (
                (hop_src.name().to_string(), hop_dst.name().to_string()),
                format!(
                    "U+{:04X} to {} in conversion from {names}",
                    c as u32,
                    hop_dst.name()
                ),
            )
        }
        _ => (
            (E::EUC_JP.name().to_string(), w.inner.name().to_string()),
            format!(
                "{} to {} in conversion from {names}",
                quote_error_bytes(&meta.error_bytes),
                w.inner.name()
            ),
        ),
    };
    meta.stage = Some(stage);
    meta.message = Some(message);
}

/// The same for a cell a wrapper's *source* read that the far end has
/// no character for: the hop that gave up is the line's gateway into
/// UTF-8, and the chain is spelled whole.
fn wrapper_src_undefined(
    meta: &mut ErrMeta,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) {
    use crate::value::Encoding as E;
    let Some(chain) = jis_family_chain(src_enc, dst_enc) else {
        return;
    };
    let Some(at) = chain.iter().position(|&e| e == E::UTF8) else {
        return;
    };
    if at == 0 {
        return;
    }
    meta.stage = Some((chain[at - 1].name().to_string(), "UTF-8".to_string()));
    meta.message = Some(format!(
        "{} to UTF-8 in conversion from {}",
        quote_error_bytes(&meta.error_bytes),
        chain_names(&chain)
    ));
}

/// `Some(enc)` when `enc` is one of the stateless-ISO-2022-JP pair.
pub(crate) fn stateless_iso2022jp(enc: crate::value::Encoding) -> Option<crate::value::Encoding> {
    matches!(enc, crate::value::Encoding::NamedByte(i)
    if matches!(
        crate::value::named_byte_const_name(i),
        "STATELESS_ISO_2022_JP" | "STATELESS_ISO_2022_JP_KDDI"
    ))
    .then_some(enc)
}

/// The same for a character that never reached EUC-JP: the hop that
/// gave up is `… → EUC-JP`, and the chain carries the UTF-8 pivot
/// when the source needs one.
pub(crate) fn undefined_before_eucjp_message(
    c: char,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let chain = if src_enc.is_utf8_compatible() {
        src_enc.name().to_string()
    } else {
        pivot_chain(src_enc)
    };
    format!(
        "U+{:04X} to EUC-JP in conversion from {chain} to EUC-JP to {}",
        c as u32,
        dst_enc.name()
    )
}

/// Encode `s` as `enc`, correcting `encoding_rs` the same way
/// [`jp_decode`] does and, for EUC-JP, reaching into JIS X 0212 for
/// what it will not write at all. Returns `Err(c)` on the first
/// character the encoding has no cell for.
///
/// WHATWG's EUC-JP encoder emits JIS X 0208 only, the three-byte
/// `0x8F` forms being decode-only there — so `"ü".encode("EUC-JP")` is
/// `8F AB E4` in CRuby and was an `UndefinedConversionError` here. An
/// answer landing in a dead row counts as no answer, and JIS X 0208
/// proper still wins over 0212 wherever both hold a character.
pub(crate) fn jp_encode(fx: &JpFixup, s: &str) -> std::result::Result<Vec<u8>, char> {
    let rs = fx.rs;
    // Whether this character is one the fixup tables move — the cheap
    // test that keeps the fast path.
    let is_fixed_up = |c: char| {
        c > '\u{7f}'
            && (fx.reject.contains(&c)
                || jp_encode_override(fx, c).is_some()
                || (fx.pua && windows31j_pua_cell(c).is_some()))
    };
    let (bytes, _, had_err) = rs.encode(s);
    if !had_err && jp_live_throughout(fx, &bytes) && !s.chars().any(is_fixed_up) {
        return Ok(bytes.into_owned());
    }
    let mut out = Vec::with_capacity(bytes.len());
    let mut buf = [0u8; 4];
    for c in s.chars() {
        if fx.reject.contains(&c) {
            return Err(c);
        }
        if let Some(seq) = jp_encode_override(fx, c) {
            out.extend_from_slice(seq);
            continue;
        }
        if fx.pua
            && let Some(cell) = windows31j_pua_cell(c)
        {
            out.extend_from_slice(&cell);
            continue;
        }
        let (chunk, _, err) = rs.encode(c.encode_utf8(&mut buf));
        if !err && jp_cell_is_live(fx, &chunk) {
            out.extend_from_slice(&chunk);
        } else if fx.second_plane
            && let Some(seq) = jisx0212_reverse().get(&c)
        {
            out.extend_from_slice(seq);
        } else {
            return Err(c);
        }
    }
    Ok(out)
}

/// Whether a whole encoded buffer avoids the dead rows — the cheap
/// check that lets the common case skip the per-character walk in
/// [`jp_encode`]. Those bytes are only extension rows in *lead*
/// position (a trailing byte covers them too), so the buffer is walked
/// rather than scanned.
fn jp_live_throughout(fx: &JpFixup, bytes: &[u8]) -> bool {
    if fx.dead_rows.is_empty() {
        return true;
    }
    let mut pos = 0;
    while pos < bytes.len() {
        match (fx.precise)(bytes, pos) {
            PreciseLen::Char(n) => {
                if !jp_cell_is_live(fx, &bytes[pos..pos + n]) {
                    return false;
                }
                pos += n;
            }
            _ => pos += 1,
        }
    }
    true
}

/// Decode a single-byte-table encoding into a Rust `String`. Every
/// byte maps (the tables are total), so this cannot fail.
/// Decode through a single-byte table. `Err(b)` is a byte the encoding
/// assigns no character to — CRuby calls that an *undefined
/// conversion*, not an invalid byte, since the byte is a perfectly good
/// character of the source that Unicode has nowhere to put.
pub(crate) fn table_decode(
    bytes: &[u8],
    table: &[Option<char>; 128],
) -> std::result::Result<String, (usize, u8)> {
    let mut out = String::with_capacity(bytes.len());
    for (i, &b) in bytes.iter().enumerate() {
        if b < 0x80 {
            out.push(b as char);
        } else {
            out.push(table[(b - 0x80) as usize].ok_or((i, b))?);
        }
    }
    Ok(out)
}

/// [`table_decode`] with `undef: :replace`: an unassigned byte becomes
/// the replacement rather than an error.
pub(crate) fn table_decode_lossy(
    bytes: &[u8],
    table: &[Option<char>; 128],
    replace: &str,
) -> String {
    let mut out = String::with_capacity(bytes.len());
    for &b in bytes {
        if b < 0x80 {
            out.push(b as char);
        } else if let Some(c) = table[(b - 0x80) as usize] {
            out.push(c);
        } else {
            out.push_str(replace);
        }
    }
    out
}

/// Encode `s` through a single-byte table. Returns `Err(c)` on the
/// first character the encoding cannot represent.
pub(crate) fn table_encode(
    s: &str,
    table: &[Option<char>; 128],
) -> std::result::Result<Vec<u8>, char> {
    let mut out = Vec::with_capacity(s.len());
    for c in s.chars() {
        if c.is_ascii() {
            out.push(c as u8);
        } else if let Some(pos) = table.iter().position(|&t| t == Some(c)) {
            out.push(0x80 + pos as u8);
        } else {
            return Err(c);
        }
    }
    Ok(out)
}

/// Transcode `src_bytes` from `src_enc` to `dst_enc`. Returns
/// the new byte buffer, or an `Encoding::*Error` on a problem.
///
/// Strategy:
/// 1. Same encoding → identity copy.
/// 2. ASCII-only content + ASCII-compatible target → identity
///    copy (this also covers `BINARY → UTF-8` for 7-bit input
///    and `UsAscii → X` paths that `encoding_rs` mishandles).
/// 3. Otherwise route through `encoding_rs::Encoding`:
///    decode src → UTF-8, encode UTF-8 → dst. Errors at either
///    stage map to `InvalidByteSequenceError` /
///    `UndefinedConversionError`.
/// 4. Encoding pairs `encoding_rs` can't represent (UTF-32 in
///    either slot, UsAscii against a non-ASCII destination)
///    raise `ConverterNotFoundError`.
/// Subset of `String#encode`'s options that affect transcoding.
/// `invalid: :replace` turns ill-formed source bytes into the
/// `replace` string (instead of raising `InvalidByteSequenceError`).
/// `undef: :replace` does the same for code points that aren't
/// representable in the destination (instead of
/// `UndefinedConversionError`). `replace` defaults to `"?"` (or
/// `"�"` for UTF-aware destinations) per CRuby.
#[derive(Default, Clone, Debug)]
pub(crate) struct TranscodeOpts {
    pub invalid_replace: bool,
    pub undef_replace: bool,
    pub replace: Option<String>,
    /// The encoding the `replace:` string was given in. The scrub
    /// path needs it: there the replacement has to be compatible
    /// with the *receiver*, which is `rb_enc_check`'s rule and not
    /// the converter's (#1599).
    pub replace_enc: Option<crate::value::Encoding>,
    /// The encoding an error message should name as the source, when
    /// the conversion running is one hop of a longer chain: the
    /// wrappers (CESU-8, `UTF8-MAC`, stateless-ISO-2022-JP,
    /// ISO-2022-JP) rewrite their bytes and hand the rest to the
    /// ordinary pipeline, which would otherwise name the intermediate
    /// (#1609).
    pub report_src: Option<crate::value::Encoding>,
    /// The ISO-2022-JP designation in effect when this chunk starts,
    /// and whether its closing escape is owed yet. A converter keeps
    /// the escape across `#convert` calls and writes the closing one
    /// from `#finish`; a one-shot conversion is one whole chunk, so it
    /// starts in ASCII and closes (#1609).
    pub iso_state: Option<u8>,
    /// Newline decorators: `universal_newline:` normalizes CRLF / CR
    /// to LF on the decode side; `crlf_newline:` / `cr_newline:`
    /// rewrite LF on the encode side, and `lf_newline:` rewrites CRLF
    /// and CR to LF there.
    pub universal_newline: bool,
    pub crlf_newline: bool,
    pub cr_newline: bool,
    pub lf_newline: bool,
}

impl TranscodeOpts {
    pub(crate) fn has_newline(&self) -> bool {
        self.universal_newline || self.crlf_newline || self.cr_newline || self.lf_newline
    }

    /// Apply the newline decorators to decoded text.
    pub(crate) fn apply_newline(&self, s: &str) -> String {
        let mut out = s.to_string();
        if self.universal_newline || self.lf_newline {
            out = out.replace("\r\n", "\n").replace('\r', "\n");
        }
        if self.crlf_newline {
            out = out.replace('\n', "\r\n");
        } else if self.cr_newline {
            out = out.replace('\n', "\r");
        }
        out
    }
}

/// Whether an encoding spells `U+FFFD`, and so replaces with it rather
/// than with `"?"`.
///
/// CRuby keeps a table of the names that do and asks it about the
/// encoding the replacement is *inserted in* — the destination for an
/// ordinary converter. The table is UTF-8, the UTF-16 and UTF-32 forms
/// and the `UCS-*` aliases of those, and nothing else: read off
/// `Encoding::Converter#replacement` for all 175 names CRuby lists,
/// and confirmed from the other side by `"\xf0".force_encoding(
/// "CESU-8").encode("CESU-8", invalid: :replace)`, which is `"?"`
/// although a conversion *into* CESU-8 replaces with `U+FFFD` — see
/// `inserted_replacement` for why those two differ (#1571).
fn replaces_with_u_fffd(enc: crate::value::Encoding) -> bool {
    use crate::value::Encoding as E;
    match enc {
        E::UTF8 | E::Utf16Le | E::Utf16Be | E::Utf32Le | E::Utf32Be => true,
        // The endianness-less dummies write a BOM and then the
        // big-endian form, so they answer as that form does.
        E::Other(_) => dummy_wide_target(enc).is_some(),
        _ => false,
    }
}

/// The encoding a converter inserts its replacement in, for a given
/// pair.
///
/// `rb_econv_encoding_to_insert_output` asks the *last* transcoder in
/// the chain, and answers with that transcoder's **source** encoding
/// when it is an `asciicompat_encoder`. Of everything monoruby
/// converts, `from_UTF8_MAC` is the only one — so a `UTF8-MAC` source
/// with the pivot as its destination is the single conversion whose
/// replacement is the source's rather than the destination's (#1577).
pub(crate) fn insert_encoding(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
) -> crate::value::Encoding {
    if src == crate::value::Encoding::Utf8(crate::value::UTF8_MAC)
        && dst == crate::value::Encoding::UTF8
    {
        src
    } else {
        dst
    }
}

/// The replacement a *converter* inserts, asked of the encoding it
/// inserts into.
///
/// Not the same question as [`replaces_with_u_fffd`], which is an
/// encoding's *own* replacement — the one `String#scrub` and a
/// same-encoding `invalid: :replace` use. CESU-8 is where the two
/// part: a converter inserting into it writes `U+FFFD`, while
/// scrubbing a CESU-8 string writes `"?"`. `UTF8-MAC` is the mirror
/// image: inserting into it is `"?"`. Neither answer is derivable from
/// anything monoruby holds about the encoding; both are read off CRuby
/// (#1571, #1577).
pub(crate) fn inserted_replacement(insert_enc: crate::value::Encoding) -> &'static str {
    if replaces_with_u_fffd(insert_enc)
        || insert_enc == crate::value::Encoding::NamedByte(crate::value::CESU_8)
    {
        "\u{FFFD}"
    } else {
        "?"
    }
}

/// The endianness-less dummy `UTF-16` / `UTF-32` as a *decode* source:
/// the concrete encoding its BOM names, and the bytes after it.
///
/// CRuby reads the BOM and consumes it. Without one the source is
/// ill-formed — there is nothing to say which end the code units start
/// at — so this answers `None` and the caller reports it, naming the
/// first code unit as the dummy encoding's own (#1576).
pub(crate) fn dummy_wide_source(
    enc: crate::value::Encoding,
    bytes: &[u8],
) -> Option<(crate::value::Encoding, &[u8])> {
    use crate::value::Encoding as E;
    let wide = dummy_wide_target(enc)?;
    match wide {
        E::Utf16Be => match bytes {
            [0xFE, 0xFF, rest @ ..] => Some((E::Utf16Be, rest)),
            [0xFF, 0xFE, rest @ ..] => Some((E::Utf16Le, rest)),
            _ => None,
        },
        _ => match bytes {
            [0x00, 0x00, 0xFE, 0xFF, rest @ ..] => Some((E::Utf32Be, rest)),
            [0xFF, 0xFE, 0x00, 0x00, rest @ ..] => Some((E::Utf32Le, rest)),
            _ => None,
        },
    }
}

/// The endianness-less dummy `UTF-16` / `UTF-32` as an encode target:
/// returns the big-endian concrete encoding CRuby writes after a BOM.
pub(crate) fn dummy_wide_target(enc: crate::value::Encoding) -> Option<crate::value::Encoding> {
    if let crate::value::Encoding::Other(_) = enc {
        match enc.name() {
            "UTF-16" => Some(crate::value::Encoding::Utf16Be),
            "UTF-32" => Some(crate::value::Encoding::Utf32Be),
            _ => None,
        }
    } else {
        None
    }
}

impl TranscodeOpts {
    pub(crate) fn replace_str(&self, dst_enc: crate::value::Encoding) -> &str {
        if let Some(s) = &self.replace {
            return s;
        }
        // CRuby: default replacement is "�" for the UTF
        // destinations and "?" otherwise — see `replaces_with_u_fffd`
        // for which names those are, since `UTF8-MAC` is not one of
        // them and CESU-8 is.
        if replaces_with_u_fffd(dst_enc) {
            "\u{FFFD}"
        } else {
            "?"
        }
    }
}

/// Decode UTF-16 (`be` = big-endian) into a Rust `String`. Invalid
/// units (odd trailing byte, unpaired/garbled surrogate) become
/// U+FFFD; the bool is `true` if any were seen.
fn decode_utf16_bytes(bytes: &[u8], be: bool) -> (String, bool) {
    let mut out = String::with_capacity(bytes.len() / 2);
    let mut had_err = false;
    let units: Vec<u16> = bytes
        .chunks(2)
        .map(|c| {
            if c.len() < 2 {
                had_err = true;
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
        if (0xD800..=0xDBFF).contains(&u) {
            if i + 1 < units.len() && (0xDC00..=0xDFFF).contains(&units[i + 1]) {
                let hi = (u as u32 - 0xD800) << 10;
                let lo = units[i + 1] as u32 - 0xDC00;
                let c = char::from_u32(0x10000 + hi + lo).unwrap_or('\u{FFFD}');
                out.push(c);
                i += 2;
                continue;
            }
            had_err = true;
            out.push('\u{FFFD}');
            i += 1;
        } else if (0xDC00..=0xDFFF).contains(&u) {
            had_err = true;
            out.push('\u{FFFD}');
            i += 1;
        } else {
            out.push(char::from_u32(u as u32).unwrap_or('\u{FFFD}'));
            i += 1;
        }
    }
    (out, had_err)
}

/// Decode UTF-32 (`be` = big-endian) into a Rust `String`. Invalid
/// units (short trailing group, value > U+10FFFF, surrogate range)
/// become U+FFFD; the bool is `true` if any were seen.
fn decode_utf32_bytes(bytes: &[u8], be: bool) -> (String, bool) {
    let mut out = String::with_capacity(bytes.len() / 4);
    let mut had_err = false;
    for c in bytes.chunks(4) {
        if c.len() < 4 {
            had_err = true;
            out.push('\u{FFFD}');
            continue;
        }
        let v = if be {
            u32::from_be_bytes([c[0], c[1], c[2], c[3]])
        } else {
            u32::from_le_bytes([c[0], c[1], c[2], c[3]])
        };
        match char::from_u32(v) {
            Some(ch) => out.push(ch),
            None => {
                had_err = true;
                out.push('\u{FFFD}');
            }
        }
    }
    (out, had_err)
}

/// Encode `s` as UTF-16 (`be` = big-endian) bytes.
fn encode_utf16_bytes(s: &str, be: bool) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len() * 2);
    let mut buf = [0u16; 2];
    for ch in s.chars() {
        for u in ch.encode_utf16(&mut buf).iter() {
            if be {
                out.extend_from_slice(&u.to_be_bytes());
            } else {
                out.extend_from_slice(&u.to_le_bytes());
            }
        }
    }
    out
}

/// Encode `s` as UTF-32 (`be` = big-endian) bytes.
fn encode_utf32_bytes(s: &str, be: bool) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len() * 4);
    for ch in s.chars() {
        let v = ch as u32;
        if be {
            out.extend_from_slice(&v.to_be_bytes());
        } else {
            out.extend_from_slice(&v.to_le_bytes());
        }
    }
    out
}

/// `true` for the UTF-16/UTF-32 (LE/BE) encodings handled by the
/// hand-rolled codecs above (encoding_rs cannot encode these, and has
/// no UTF-32 at all).
pub(crate) fn is_utf16_or_32(enc: crate::value::Encoding) -> bool {
    use crate::value::Encoding as E;
    matches!(enc, E::Utf16Le | E::Utf16Be | E::Utf32Le | E::Utf32Be)
}

pub(crate) fn decode_utf16_32(bytes: &[u8], enc: crate::value::Encoding) -> (String, bool) {
    use crate::value::Encoding as E;
    match enc {
        E::Utf16Le => decode_utf16_bytes(bytes, false),
        E::Utf16Be => decode_utf16_bytes(bytes, true),
        E::Utf32Le => decode_utf32_bytes(bytes, false),
        E::Utf32Be => decode_utf32_bytes(bytes, true),
        _ => unreachable!(),
    }
}

pub(crate) fn encode_utf16_32(s: &str, enc: crate::value::Encoding) -> Vec<u8> {
    use crate::value::Encoding as E;
    match enc {
        E::Utf16Le => encode_utf16_bytes(s, false),
        E::Utf16Be => encode_utf16_bytes(s, true),
        E::Utf32Le => encode_utf32_bytes(s, false),
        E::Utf32Be => encode_utf32_bytes(s, true),
        _ => unreachable!(),
    }
}

/// Transcode an environment string to `Encoding.default_internal`
/// (`env_enc_str_new`). Plain conversion with no `invalid:` / `undef:`
/// handling — CRuby raises on a byte it cannot map, and so does this.
pub(crate) fn transcode_for_env(
    store: &Store,
    bytes: &[u8],
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
) -> Result<Vec<u8>> {
    transcode_bytes_with_opts(bytes, src, dst, &TranscodeOpts::default(), store)
}

#[derive(Clone, Copy)]
pub(crate) enum XmlMode {
    Attr,
    Text,
}

/// The encoding `Encoding::Converter#replacement` hands its answer
/// back in: the one CRuby inserts substituted output in, which is the
/// *input* of the last step of the conversion rather than the
/// destination. For a single-step conversion the two are the same, so
/// this is the destination for nearly everything; the wide encodings
/// are written from UTF-8 by a step of their own, and keep it.
///
/// ISO-2022-JP is the other multi-step destination: it is written
/// from stateless-ISO-2022-JP by a step of its own, so that is where
/// its replacement lives (#1600, which gave monoruby the converter
/// that makes this spellable).
fn replacement_encoding(dst: crate::value::Encoding) -> crate::value::Encoding {
    if is_utf16_or_32(dst) {
        crate::value::Encoding::UTF8
    } else if let Some(w) = jis_wrapper(dst) {
        w.inner
    } else {
        dst
    }
}

/// The replacement as [`replacement_encoding`] spells it, which is
/// what `Encoding::Converter#replacement` hands back (#1583).
///
/// `in_dst` is the replacement already converted to the destination,
/// which is how the caller established that the destination can
/// spell it — so those are exactly the bytes wanted, except for the
/// encodings that keep theirs in UTF-8, where the text already is.
/// Nothing is converted twice and nothing here can fail.
pub(crate) fn replacement_in(
    s: &str,
    in_dst: &[u8],
    dst: crate::value::Encoding,
    store: &Store,
) -> crate::value::RStringInner {
    let enc = replacement_encoding(dst);
    // The destination's own bytes when the last step writes straight
    // into it; UTF-8's when it is the wide encodings' pivot, where the
    // text already is. stateless-ISO-2022-JP is neither, so that one
    // hop is converted here — the destination can spell the
    // replacement, so stateless can too, ISO-2022-JP being written
    // from it (#1600).
    let converted;
    let bytes = if enc == dst {
        in_dst
    } else if enc == crate::value::Encoding::UTF8 {
        s.as_bytes()
    } else {
        converted = transcode_bytes_with_opts(
            s.as_bytes(),
            crate::value::Encoding::UTF8,
            enc,
            &TranscodeOpts::default(),
            store,
        )
        .unwrap_or_else(|_| s.as_bytes().to_vec());
        &converted
    };
    crate::value::RStringInner::from_encoding_scanned(bytes, enc)
}

/// The read-again bytes an `:invalid_byte_sequence` outcome left in
/// the converter, taken out of it: they are the head of whatever the
/// next call converts, as CRuby keeps them at the head of its input
/// buffer — unless `#putback` handed them back to the caller first.
/// Some pairs carry state across calls, which a single-shot
/// transcode has no way to keep: the endianness-less dummies carry
/// a BOM, read once on the source side and written once on the
/// destination side, and a `UTF8-MAC` source holds its trailing
/// cluster back. ISO-2022-JP carries its designation across calls,
/// and an escape sequence split between two of them is held rather
/// than substituted — which the single-shot transcoder, seeing one
/// whole input, cannot know (#1576, #1609).
pub(crate) fn converter_is_stateful(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
) -> bool {
    dummy_wide_target(src).is_some()
        || dummy_wide_target(dst).is_some()
        || src == crate::value::Encoding::Utf8(crate::value::UTF8_MAC)
        || jis_wrapper(src).is_some()
        || jis_wrapper(dst).is_some()
}

/// The bytes at the end of `input` that begin a character the next
/// chunk may finish — what a converter holds back rather than
/// converts, in replacement mode as on every other path (#1592).
pub(crate) fn incomplete_tail_len(src: crate::value::Encoding, input: &[u8]) -> usize {
    use crate::value::Encoding as E;
    if is_utf16_or_32(src) {
        // A partial coding unit, and for UTF-16 a high surrogate
        // waiting for its pair.
        let unit = if matches!(src, E::Utf16Le | E::Utf16Be) {
            2
        } else {
            4
        };
        let mut tail = input.len() % unit;
        if unit == 2 && input.len() - tail >= 2 {
            let last = &input[input.len() - tail - 2..input.len() - tail];
            let u = if src == E::Utf16Be {
                u16::from_be_bytes([last[0], last[1]])
            } else {
                u16::from_le_bytes([last[0], last[1]])
            };
            if (0xd800..0xdc00).contains(&u) {
                tail += 2;
            }
        }
        return tail;
    }
    if src.is_utf8_compatible() {
        let mut at = 0;
        loop {
            match std::str::from_utf8(&input[at..]) {
                Ok(_) => return 0,
                Err(e) => match e.error_len() {
                    None => return input.len() - (at + e.valid_up_to()),
                    Some(n) => at += e.valid_up_to() + n,
                },
            }
        }
    }
    let Some((_, precise)) = conversion_walker(src).or_else(|| crate::value::mbc_walker(src))
    else {
        return 0;
    };
    let mut at = 0;
    while at < input.len() {
        match precise(input, at) {
            PreciseLen::Char(n) if n > 0 => at += n,
            PreciseLen::NeedMore => return input.len() - at,
            _ => at += 1,
        }
    }
    0
}

// CRuby's `ECONV_*` bits, as `Encoding::Converter`'s constants spell
// them. The decorator bits travel in a converter's flags as they are;
// `INVALID_REPLACE` / `UNDEF_REPLACE` are only what [`econv_opts`]
// reads from an option Hash.
pub(crate) const ECONV_INVALID_REPLACE: i64 = 0x0000_0002;
pub(crate) const ECONV_UNDEF_REPLACE: i64 = 0x0000_0020;
pub(crate) const ECONV_UNIVERSAL_NEWLINE: i64 = 0x0000_0100;
pub(crate) const ECONV_CRLF_NEWLINE: i64 = 0x0000_1000;
pub(crate) const ECONV_CR_NEWLINE: i64 = 0x0000_2000;
pub(crate) const ECONV_LF_NEWLINE: i64 = 0x0000_4000;
const ECONV_NEWLINE_MASK: i64 = 0x0000_7f00;
pub(crate) const ECONV_XML_TEXT: i64 = 0x0000_8000;
pub(crate) const ECONV_XML_ATTR_CONTENT: i64 = 0x0001_0000;
pub(crate) const ECONV_XML_ATTR_QUOTE: i64 = 0x0010_0000;
pub(crate) const ECONV_DECORATORS: i64 =
    ECONV_NEWLINE_MASK | ECONV_XML_TEXT | ECONV_XML_ATTR_CONTENT | ECONV_XML_ATTR_QUOTE;

/// Whether `flags` asks for decorators no converter can stack: two
/// newline decorators, or `xml_text` with `xml_attr_content`
/// (`decorator_names` in transcode.c refuses both).
pub(crate) fn econv_conflict(flags: i64) -> bool {
    (flags & ECONV_NEWLINE_MASK).count_ones() > 1
        || (flags & ECONV_XML_TEXT != 0 && flags & ECONV_XML_ATTR_CONTENT != 0)
}

/// The `ECONV_*` decorator bits `opts` and `xml` stand for.
fn decorator_flags(opts: &TranscodeOpts, xml: Option<XmlMode>) -> i64 {
    let mut flags = 0;
    for (on, bit) in [
        (opts.universal_newline, ECONV_UNIVERSAL_NEWLINE),
        (opts.crlf_newline, ECONV_CRLF_NEWLINE),
        (opts.cr_newline, ECONV_CR_NEWLINE),
        (opts.lf_newline, ECONV_LF_NEWLINE),
    ] {
        if on {
            flags |= bit;
        }
    }
    match xml {
        Some(XmlMode::Text) => flags | ECONV_XML_TEXT,
        Some(XmlMode::Attr) => flags | ECONV_XML_ATTR_CONTENT | ECONV_XML_ATTR_QUOTE,
        None => flags,
    }
}

/// `opts` with the newline decorators `flags` asks for.
pub(crate) fn with_newline_flags(mut opts: TranscodeOpts, flags: i64) -> TranscodeOpts {
    opts.universal_newline = flags & ECONV_UNIVERSAL_NEWLINE != 0;
    opts.crlf_newline = flags & ECONV_CRLF_NEWLINE != 0;
    opts.cr_newline = flags & ECONV_CR_NEWLINE != 0;
    opts.lf_newline = flags & ECONV_LF_NEWLINE != 0;
    opts
}

/// `code converter not found (UTF-8 to bogus with crlf_newline)`, as
/// `rb_econv_open_exc` spells it (`econv_description`): the two names
/// as given, a side that is empty left out, then the decorators in
/// CRuby's fixed order — and `no-conversion` when there is nothing to
/// name at all.
pub(crate) fn converter_not_found_named(
    store: &Store,
    sname: &str,
    dname: &str,
    flags: i64,
) -> MonorubyErr {
    let mut desc = match (sname.is_empty(), dname.is_empty()) {
        (true, true) => String::new(),
        (true, false) => dname.to_string(),
        (false, true) => sname.to_string(),
        (false, false) => format!("{sname} to {dname}"),
    };
    let decorators: Vec<&str> = [
        (ECONV_UNIVERSAL_NEWLINE, "universal_newline"),
        (ECONV_CRLF_NEWLINE, "crlf_newline"),
        (ECONV_CR_NEWLINE, "cr_newline"),
        (ECONV_LF_NEWLINE, "lf_newline"),
        (ECONV_XML_TEXT, "xml_text"),
        (ECONV_XML_ATTR_CONTENT, "xml_attr_content"),
        (ECONV_XML_ATTR_QUOTE, "xml_attr_quote"),
    ]
    .into_iter()
    .filter(|(bit, _)| flags & bit != 0)
    .map(|(_, name)| name)
    .collect();
    if !decorators.is_empty() {
        if !desc.is_empty() {
            desc.push_str(" with ");
        }
        desc.push_str(&decorators.join(","));
    }
    if desc.is_empty() {
        desc.push_str("no-conversion");
    }
    MonorubyErr::converter_not_found_error(store, format!("code converter not found ({desc})"))
}

/// Refuse a `replace:` string the destination cannot spell (#1566).
///
/// CRuby converts the replacement into the destination when it opens
/// the converter, and a failure there is reported as a converter that
/// does not exist rather than as a character with no cell — so the
/// error names the pair and not the character, and arrives before any
/// of the input is looked at. Dropping the replacement instead, as
/// this did, turned a substitution into silence: the character with
/// no cell went missing and so did the thing meant to stand for it.
pub(crate) fn validate_replacement(
    opts: &TranscodeOpts,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    xml: Option<XmlMode>,
    store: &Store,
) -> Result<Option<Vec<u8>>> {
    let Some(repl) = opts.replace.as_deref() else {
        return Ok(None);
    };
    if let Ok(bytes) = transcode_bytes_with_opts(
        repl.as_bytes(),
        crate::value::Encoding::UTF8,
        dst_enc,
        &TranscodeOpts::default(),
        store,
    ) {
        return Ok(Some(bytes));
    }
    Err(converter_not_found(store, src_enc, dst_enc, opts, xml))
}

/// `code converter not found (UTF-8 to MacJapanese with crlf_newline)`:
/// a pair with no transcoder, spelled with the decorators asked for.
pub(crate) fn converter_not_found(
    store: &Store,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    xml: Option<XmlMode>,
) -> MonorubyErr {
    converter_not_found_named(
        store,
        src_enc.name(),
        dst_enc.name(),
        decorator_flags(opts, xml),
    )
}

/// Whether monoruby can convert to and from `enc`.
///
/// `Encoding::Converter` converts a chunk at a time, so this is not
/// quite the same question `String#encode` asks: the pivot wrappers
/// need an arm in `stream_convert` and the endianness-less dummies
/// need their BOM remembered across calls. Both are there now, so the
/// two entry points agree on the whole set (#1576).
pub(crate) fn has_codec(enc: crate::value::Encoding) -> bool {
    use crate::value::Encoding as E;
    encoding_to_rs(enc).is_some()
        || single_byte_table(enc).is_some()
        || is_utf16_or_32(enc)
        || dummy_wide_target(enc).is_some()
        || enc == E::NamedByte(crate::value::CESU_8)
        || stateless_iso2022jp(enc).is_some()
        || jis_wrapper(enc).is_some()
        || is_ibm037(enc)
        || matches!(enc, E::Ascii8 | E::UsAscii)
}

/// Validate that `(src, dst)` is a transcoder monoruby can run.
/// Raises `Encoding::ConverterNotFoundError` for anything
/// [`has_codec`] does not cover. Identical encodings are always
/// allowed.
/// The part of `rb_econv_open`'s answer that is settled before any byte
/// is read: a dummy encoding with no codec (UTF-7) on either side has no
/// converter, whatever the input. The rest of the transcoders' coverage
/// is decided by the transcoding itself, which reports its own
/// `ConverterNotFoundError` where it has nothing for a pair.
pub(crate) fn refuse_pair_without_converter(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    ecflags: i64,
    store: &Store,
) -> Result<()> {
    if src == dst {
        return Ok(());
    }
    let missing = |e: crate::value::Encoding| is_cruby_dummy_name(e.name()) && !has_codec(e);
    if missing(src) || missing(dst) {
        return Err(converter_not_found_named(
            store,
            src.name(),
            dst.name(),
            ecflags & ECONV_DECORATORS,
        ));
    }
    Ok(())
}

pub(crate) fn validate_converter_pair(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    ecflags: i64,
    store: &Store,
) -> Result<()> {
    if src == dst {
        return Ok(());
    }
    let src_supported = has_codec(src);
    let dst_supported = has_codec(dst);
    if !src_supported || !dst_supported {
        return Err(converter_not_found_named(
            store,
            src.name(),
            dst.name(),
            ecflags & ECONV_DECORATORS,
        ));
    }
    Ok(())
}

/// Outcome of `stream_convert`. Maps 1:1 to the Symbol returned
/// from `Encoding::Converter#primitive_convert`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StreamConvertResult {
    /// All input consumed, output written; no more conversion to do.
    Finished,
    /// Filled the destination buffer up to its max bytesize before
    /// finishing the input. The remaining input is still in `src`.
    DestinationBufferFull,
    /// Encountered bytes that aren't valid in the source encoding.
    /// The malformed run has been removed from `src`.
    InvalidByteSequence,
    /// Encountered a code point that the destination encoding can't
    /// represent. The triggering byte sequence has been removed from
    /// `src`.
    UndefinedConversion,
    /// `partial_input: false` and the input ended mid-multi-byte-
    /// sequence. The trailing partial bytes have been removed from
    /// `src`.
    IncompleteInput,
    /// `partial_input: true` and the input ended on a sequence
    /// boundary (or mid-sequence — the decoder is happy to keep
    /// waiting for more). All consumed bytes removed from `src`.
    SourceBufferEmpty,
}

impl StreamConvertResult {
    pub(crate) fn symbol_name(self) -> &'static str {
        match self {
            Self::Finished => "finished",
            Self::DestinationBufferFull => "destination_buffer_full",
            Self::InvalidByteSequence => "invalid_byte_sequence",
            Self::UndefinedConversion => "undefined_conversion",
            Self::IncompleteInput => "incomplete_input",
            Self::SourceBufferEmpty => "source_buffer_empty",
        }
    }
}

/// Streamed transcoder for `Encoding::Converter#primitive_convert`.
/// Decodes `src_bytes` from `src_enc` to UTF-8 in chunks, then
/// encodes UTF-8 to `dst_enc`, stopping early on error / output-
/// buffer-full / partial-input. Returns:
///
/// - `result` — what stopped us (see `StreamConvertResult`).
/// - `src_consumed` — number of bytes from the front of `src_bytes`
///   the caller should drain. Includes any malformed / unmappable
///   bytes that triggered an error symbol so they don't get
///   re-fed on the next call.
/// - `out_bytes` — bytes to append to the destination buffer at
///   `dst_offset`.
/// Byte-level detail of a conversion error, for `primitive_errinfo`,
/// `putback` and `last_error`.
#[derive(Debug, Clone, Default)]
pub(crate) struct ErrMeta {
    /// The message and the errinfo stage names a wrapper settled for an
    /// error its inner conversion reported, when the inner encoding's
    /// own wording would name the wrong hop (#1520).
    pub(crate) message: Option<String>,
    pub(crate) stage: Option<(String, String)>,
    /// The bytes that are definitely part of the erroneous sequence.
    pub(crate) error_bytes: Vec<u8>,
    /// Bytes consumed while detecting the error that should be
    /// re-examined (CRuby's read-again bytes).
    pub(crate) readagain_bytes: Vec<u8>,
    /// `:destination_buffer_full` only: source bytes *past* the
    /// returned consumed count that CRuby additionally reports as
    /// consumed. Its transcoder reads the character it could not
    /// fit and buffers that character's output, so a destination
    /// capped at 2 bytes takes `"\u3042a"` out of `"\u3042abcd"`
    /// while writing only the first character (#1511). We have no
    /// output buffer, so the caller re-converts those bytes next
    /// call instead — but it must still take them out of the
    /// user's `src` to match what CRuby leaves there.
    pub(crate) dst_full_extra: usize,
    /// `:destination_buffer_full` only: the output bytes of that
    /// character which did not fit. CRuby fills the destination to
    /// the byte and holds the rest for the next call, so a cap of 1
    /// takes `"\u3042"` apart into `A4` and `A2` (#1532). When this
    /// is set the character is *not* re-converted next call — these
    /// bytes are written instead.
    pub(crate) dst_full_out: Vec<u8>,
    /// The ISO-2022-JP designation this chunk leaves in effect, for
    /// the next one to carry on from (#1609).
    pub(crate) iso_state_out: Option<u8>,
    /// Source bytes read *before* an error and still held by the
    /// decoder, not written: a `UTF8-MAC` source keeps its last
    /// cluster back for the composition the next chunk may complete,
    /// and a malformed run after it does not flush it. They have been
    /// taken out of the caller's source, so the converter re-reads
    /// them ahead of everything else next call (#1617).
    pub(crate) hold_src: Vec<u8>,
    /// `:undefined_conversion` only: the character had no mapping in
    /// the *decode* half (a source byte with no Unicode meaning),
    /// not the encode half. The two stages are reported differently
    /// — `error_bytes` are raw source bytes rather than the UTF-8
    /// pivot, and the stage pair is `[source, "UTF-8"]` (#1511).
    pub(crate) decode_stage: bool,
}

/// How many bytes of `src_bytes` decode to the first `pivot_bytes`
/// bytes of the UTF-8 pivot. Used where a conversion fails partway
/// through the *encode* half and only the source up to there counts as
/// consumed.
///
/// A prefix of the source decodes to a prefix of the pivot, and longer
/// never decodes to shorter, so the shortest source prefix that reaches
/// `pivot_bytes` is a binary search — a handful of decodes rather than
/// one per character.
fn pivot_prefix_consumed(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    pivot_bytes: usize,
    opts: &TranscodeOpts,
    store: &Store,
) -> usize {
    pivot_prefix_consumed_in(
        src_bytes,
        src_enc,
        crate::value::Encoding::UTF8,
        pivot_bytes,
        opts,
        store,
    )
}

/// The same question against a pivot other than UTF-8 —
/// stateless-ISO-2022-JP's is EUC-JP (#1600).
fn pivot_prefix_consumed_in(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    pivot_enc: crate::value::Encoding,
    pivot_bytes: usize,
    opts: &TranscodeOpts,
    store: &Store,
) -> usize {
    if pivot_bytes == 0 {
        return 0;
    }
    if src_enc == pivot_enc {
        return pivot_bytes.min(src_bytes.len());
    }
    // The shortest prefix that reaches the offset is the one CRuby
    // read: it stops on the character whose output it could not fit
    // and reads no further, so bytes that follow and write nothing —
    // an ISO-2022-JP escape sequence — stay in `src` (#1609).
    let decoded_len = |n: usize| -> usize {
        let (_, _, out, _) =
            stream_convert(&src_bytes[..n], src_enc, pivot_enc, None, true, opts, store);
        out.len()
    };
    let (mut lo, mut hi) = (0usize, src_bytes.len());
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if decoded_len(mid) < pivot_bytes {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    lo
}

/// The outcome to report for a source that does not decode, as the
/// destinations `encoding_rs` has no encoder for (`US-ASCII`, BINARY)
/// need it: they walk characters rather than bytes, so the bytes that
/// failed have to be found again. [`first_bad_sequence`] is the same
/// walk `String#encode`'s error uses, and answers all three fields at
/// once — CRuby reports `"\xC2"` on a truncated tail as
/// `:incomplete_input`, and `"\xFF"` as `:invalid_byte_sequence`.
fn bad_source_outcome(
    src_enc: crate::value::Encoding,
    src_bytes: &[u8],
    last: bool,
) -> (StreamConvertResult, ErrMeta) {
    let Some((error_bytes, readagain_bytes, incomplete)) = first_bad_sequence(src_enc, src_bytes)
    else {
        return (StreamConvertResult::InvalidByteSequence, ErrMeta::default());
    };
    let kind = if !incomplete {
        StreamConvertResult::InvalidByteSequence
    } else if last {
        StreamConvertResult::IncompleteInput
    } else {
        StreamConvertResult::SourceBufferEmpty
    };
    (
        kind,
        ErrMeta {
            error_bytes,
            readagain_bytes,
            ..ErrMeta::default()
        },
    )
}

/// The byte offset of the first UTF-16 / UTF-32 unit `decode_utf16_32`
/// cannot read — a lone surrogate, an out-of-range scalar, or a group
/// the input ended in the middle of. `None` when the whole input reads.
fn utf16_32_first_error(bytes: &[u8], enc: crate::value::Encoding) -> Option<usize> {
    use crate::value::Encoding as E;
    let be = matches!(enc, E::Utf16Be | E::Utf32Be);
    if matches!(enc, E::Utf32Le | E::Utf32Be) {
        let mut i = 0;
        while i + 4 <= bytes.len() {
            let g = [bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]];
            let cp = if be {
                u32::from_be_bytes(g)
            } else {
                u32::from_le_bytes(g)
            };
            if char::from_u32(cp).is_none() {
                return Some(i);
            }
            i += 4;
        }
        return (i < bytes.len()).then_some(i);
    }
    let unit = |i: usize| {
        let (a, b) = (bytes[i], bytes[i + 1]);
        if be {
            u16::from_be_bytes([a, b])
        } else {
            u16::from_le_bytes([a, b])
        }
    };
    let mut i = 0;
    while i + 2 <= bytes.len() {
        let u = unit(i);
        if (0xD800..=0xDBFF).contains(&u) {
            if i + 4 <= bytes.len() && (0xDC00..=0xDFFF).contains(&unit(i + 2)) {
                i += 4;
                continue;
            }
            return Some(i);
        }
        if (0xDC00..=0xDFFF).contains(&u) {
            return Some(i);
        }
        i += 2;
    }
    (i < bytes.len()).then_some(i)
}

/// Map a byte offset in the composed pivot back to `head`'s own bytes.
///
/// Clusters compose independently of one another, so their lengths add
/// up on both sides and the walk is linear.
fn mac_src_offset_for_pivot(head: &str, pivot_upto: usize) -> usize {
    let mut src_at = 0;
    let mut piv_at = 0;
    for r in crate::value::mac_clusters(head) {
        let piece = crate::value::mac_to_utf8(&head[r.clone()]);
        if piv_at + piece.len() > pivot_upto {
            break;
        }
        piv_at += piece.len();
        src_at = r.end;
    }
    src_at
}

/// The end of the cluster a pivot offset falls in, as a `(pivot,
/// source)` pair. Clusters compose independently of one another, so
/// this is the same walk [`mac_src_offset_for_pivot`] makes, stopping
/// one cluster later.
fn mac_cluster_end_for_pivot(head: &str, pivot_at: usize) -> (usize, usize) {
    let mut piv = 0;
    let mut src = 0;
    for r in crate::value::mac_clusters(head) {
        let piece = crate::value::mac_to_utf8(&head[r.clone()]);
        piv += piece.len();
        src = r.end;
        if piv >= pivot_at {
            break;
        }
    }
    (piv, src)
}

/// How much of `bytes` a `UTF8-MAC` source is holding back: its
/// trailing cluster, when the whole of `bytes` is one. Any other
/// source holds nothing.
pub(crate) fn mac_held_len(src_enc: crate::value::Encoding, bytes: &[u8]) -> usize {
    if src_enc != crate::value::Encoding::Utf8(crate::value::UTF8_MAC) {
        return 0;
    }
    let Ok(s) = std::str::from_utf8(bytes) else {
        return 0;
    };
    crate::value::mac_clusters(s)
        .last()
        .filter(|r| {
            s[r.start..r.end]
                .chars()
                .next()
                .is_some_and(|c| (c as u32) < 0x10000)
        })
        .map_or(0, |r| s.len() - r.start)
}

/// How much of `bytes` is well-formed CESU-8.
fn cesu8_good_prefix(bytes: &[u8]) -> usize {
    let mut at = 0;
    while at < bytes.len() {
        match crate::value::cesu8_precise_len(bytes, at) {
            crate::value::PreciseLen::Char(n) => at += n,
            _ => break,
        }
    }
    at
}

/// Where a bad byte in a `UTF8-MAC` source lands relative to the
/// cluster the decoder is holding.
///
/// `from_UTF8_MAC` keeps the cluster it is composing to itself and
/// hands it on only once the next starter arrives. With the pivot as
/// the destination that transcoder is the whole conversion, so
/// CRuby's `rb_econv_insert_output` writes the replacement into the
/// very buffer the cluster is flushed to and it lands *after* it.
/// With any other destination there is a second transcoder and the
/// replacement goes into *its* output, so it comes out first — and
/// the cluster, still upstream and still open, goes on collecting
/// marks that arrive after the bad byte (#1577).
pub(crate) fn mac_replacement_leads(dst_enc: crate::value::Encoding) -> bool {
    dst_enc != crate::value::Encoding::UTF8
}

/// A broken `UTF8-MAC` source under `invalid: :replace`, composed into
/// the pivot with every ill-formed subpart replaced.
pub(crate) struct MacReplaced {
    /// The composed pivot, replacements and all.
    pub(crate) pivot: String,
    /// How much of the source it accounts for. With more input still
    /// to come the open cluster stays behind for the next chunk — and
    /// so does anything the replacement has yet to be positioned
    /// against.
    cut: usize,
    /// `(pivot offset, source offset)` wherever the two are in step:
    /// every point at which the decoder had nothing held. A
    /// conversion that stops early maps back through these.
    marks: Vec<(usize, usize)>,
}

impl MacReplaced {
    /// The source offset a pivot offset stands for, rounded down to
    /// the nearest point the two agree on.
    fn src_for_pivot(&self, pivot_at: usize) -> usize {
        self.marks
            .iter()
            .rev()
            .find(|(p, _)| *p <= pivot_at)
            .map_or(0, |(_, s)| *s)
    }

    /// The first point at or after `pivot_at` the two agree on.
    fn next_mark(&self, pivot_at: usize) -> Option<(usize, usize)> {
        self.marks.iter().copied().find(|(p, _)| *p >= pivot_at)
    }
}

/// Compose a broken `UTF8-MAC` source, replacing each ill-formed
/// subpart the way CRuby's converter does.
///
/// The walk is CRuby's transcoder rather than monoruby's usual
/// decode-then-scrub: the cluster being composed is held until a
/// starter arrives, an ill-formed subpart does not close it, and the
/// replacement is written where the held cluster is not yet — which is
/// the whole of what #1577 reported.
pub(crate) fn mac_replace_pivot(
    src_bytes: &[u8],
    replace: &str,
    leads: bool,
    partial_input: bool,
) -> MacReplaced {
    use unicode_normalization::char::canonical_combining_class as ccc;
    let mut pivot = String::new();
    let mut marks = vec![(0usize, 0usize)];
    // The cluster being composed. Empty means nothing is held, and
    // every point at which that is so is a point the pivot and the
    // source agree on.
    let mut held = String::new();
    let mut at = 0usize;
    loop {
        let rest = &src_bytes[at..];
        let good = match std::str::from_utf8(rest) {
            Ok(_) => rest.len(),
            Err(e) => e.valid_up_to(),
        };
        let run = std::str::from_utf8(&rest[..good]).expect("valid up to here");
        for (off, c) in run.char_indices() {
            if ccc(c) != 0 && !held.is_empty() {
                held.push(c);
                continue;
            }
            if !held.is_empty() {
                pivot.push_str(&crate::value::mac_to_utf8(&held));
                held.clear();
                marks.push((pivot.len(), at + off));
            }
            // Only a character a mark can attach to is worth holding,
            // and in CRuby's table that is the Basic Multilingual
            // Plane — the same cut `mac_source_stream` makes at a
            // chunk end. An astral character goes straight out, so a
            // bad byte after it comes out behind it.
            if (c as u32) >= 0x10000 {
                pivot.push(c);
                marks.push((pivot.len(), at + off + c.len_utf8()));
                continue;
            }
            held.push(c);
        }
        at += good;
        if at == src_bytes.len() {
            break;
        }
        // One maximal ill-formed subpart. A truncated character at the
        // very end of a chunk is not one yet — more of it may still
        // come — so it goes back with whatever else is open.
        let err = std::str::from_utf8(&src_bytes[at..]).expect_err("stopped short of the end");
        let bad = match err.error_len() {
            Some(n) => n,
            None if partial_input => break,
            None => src_bytes.len() - at,
        };
        if !leads {
            // The cluster goes out first, so the replacement follows
            // it and the composition starts again after it.
            if !held.is_empty() {
                pivot.push_str(&crate::value::mac_to_utf8(&held));
                held.clear();
            }
        }
        pivot.push_str(replace);
        at += bad;
        if held.is_empty() {
            marks.push((pivot.len(), at));
        }
    }
    let cut = if partial_input {
        // Only what the source and the pivot agree on goes out. A
        // replacement written past the last of those marks is waiting
        // on the cluster it precedes, and the bytes behind it are
        // handed back — so it must not be converted here as well, or
        // the chunk that settles it writes it a second time.
        let (pivot_end, src_end) = *marks.last().expect("seeded above");
        pivot.truncate(pivot_end);
        src_end
    } else {
        if !held.is_empty() {
            pivot.push_str(&crate::value::mac_to_utf8(&held));
        }
        marks.push((pivot.len(), src_bytes.len()));
        src_bytes.len()
    };
    MacReplaced { pivot, cut, marks }
}

/// A `UTF8-MAC` source, a chunk at a time: compose out of Apple's
/// decomposed form and hand the result to the ordinary pipeline.
///
/// The trailing cluster is held back while more input may come — the
/// next chunk can open with a mark that composes onto it. CRuby holds
/// one back the same way, which is why `"abc"` comes back as `"ab"`
/// and then, at `#finish`, `"c"` (#1576).
fn mac_source_stream(
    src_bytes: &[u8],
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let src_enc = E::Utf8(crate::value::UTF8_MAC);
    // Broken UTF-8 is not this wrapper's to report: convert what
    // composes and let `bad_source_outcome` name the rest, as the
    // pipeline would.
    let good = match std::str::from_utf8(src_bytes) {
        Ok(_) => src_bytes.len(),
        Err(e) => e.valid_up_to(),
    };
    if good < src_bytes.len() {
        if opts.invalid_replace {
            return mac_replace_stream(
                src_bytes,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        // The well-formed prefix converts exactly as it would on its
        // own — hold-back included, whatever `partial_input` says: the
        // decoder keeps its last cluster back for a composition that
        // may still come, and a malformed run is not the end of the
        // input. CRuby writes nothing of `"A\x80B"` on the call that
        // reports the `\x80`; the `A` comes out with the `B` (#1617).
        let (kind_before, consumed, out, meta_before) = mac_source_stream(
            &src_bytes[..good],
            dst_enc,
            max_dst_bytes,
            true,
            opts,
            store,
        );
        // The destination filling up among the good bytes comes first.
        if matches!(kind_before, StreamConvertResult::DestinationBufferFull) {
            return (kind_before, consumed, out, meta_before);
        }
        let (kind, mut meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
        if !matches!(kind, StreamConvertResult::InvalidByteSequence) {
            // A chunk that merely ends inside a character: nothing
            // has been disproved, and the cluster in front of the
            // tail waits with it for the next chunk.
            return (kind, consumed, out, meta);
        }
        // The held cluster has been read: it leaves the caller's
        // source with the run and its read-again byte, and the
        // converter reads it again ahead of them next call.
        meta.hold_src = src_bytes[consumed..good].to_vec();
        let through = through_bad_run(good, &kind, &meta, src_bytes.len());
        return (kind, through, out, meta);
    }
    let s = std::str::from_utf8(src_bytes).expect("checked just above");
    // Only a character a mark can attach to is worth keeping, and in
    // CRuby's table that is the Basic Multilingual Plane: an astral
    // character at the end of a chunk goes out with the rest of it.
    let held = crate::value::mac_clusters(s)
        .last()
        .filter(|r| {
            s[r.start..r.end]
                .chars()
                .next()
                .is_some_and(|c| (c as u32) < 0x10000)
        })
        .map(|r| r.start);
    let cut = if partial_input {
        held.unwrap_or(s.len())
    } else {
        s.len()
    };
    let head = &s[..cut];
    let composed = crate::value::mac_to_utf8(head);
    let (result, pivot_consumed, out, mut meta) = stream_convert(
        composed.as_bytes(),
        E::UTF8,
        dst_enc,
        max_dst_bytes,
        false,
        opts,
        store,
    );
    let consumed = if pivot_consumed == composed.len() {
        cut
    } else {
        mac_src_offset_for_pivot(head, pivot_consumed)
    };
    if matches!(result, StreamConvertResult::DestinationBufferFull) {
        // The cap stopped inside a cluster, and a cluster's source
        // cannot be cut there — it composes as one piece. Take the
        // whole of it and hold the rest of its output for the next
        // call, the way a character split across two calls is held
        // (#1532, #1577).
        //
        // This runs whether or not the pipeline read ahead of the cap
        // itself: a character *inside* a cluster has no source bytes
        // of its own, so a stop after one leaves `dst_full_extra` at
        // zero while `consumed` rounds back to the cluster's start.
        // Saying nothing there held the character's output and
        // converted it again next call, and `"a\u0301b\u0302c"`
        // through a one-byte destination never got past the `b`.
        let over = pivot_consumed + meta.dst_full_extra;
        let (piv_end, src_end) = mac_cluster_end_for_pivot(head, over);
        if piv_end > over {
            let (_, _, rest, _) = stream_convert(
                &composed.as_bytes()[over..piv_end],
                E::UTF8,
                dst_enc,
                None,
                false,
                opts,
                store,
            );
            meta.dst_full_out.extend_from_slice(&rest);
        }
        meta.dst_full_extra = src_end.saturating_sub(consumed);
    }
    // What was held back is not the end of the input.
    let result = if cut < s.len() && matches!(result, StreamConvertResult::Finished) {
        StreamConvertResult::SourceBufferEmpty
    } else {
        result
    };
    (result, consumed, out, meta)
}

/// A broken `UTF8-MAC` source under `invalid: :replace`, a chunk at a
/// time.
///
/// The composed pivot carries the replacements, so the rest of the
/// pipeline converts them along with everything else; what the chunk
/// leaves open — the cluster still being composed, and a replacement
/// that has yet to be positioned against it — goes back to the caller
/// as unconsumed source, to be read again with the chunk that settles
/// it. CRuby keeps that state inside the converter instead, so a chunk
/// boundary can fall in a different place; the bytes either side of it
/// add up to the same conversion (#1577).
fn mac_replace_stream(
    src_bytes: &[u8],
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let mac = E::Utf8(crate::value::UTF8_MAC);
    let leads = mac_replacement_leads(dst_enc);
    let replace = opts
        .replace
        .clone()
        .unwrap_or_else(|| inserted_replacement(insert_encoding(mac, dst_enc)).to_string());
    let composed = mac_replace_pivot(src_bytes, &replace, leads, partial_input);
    let (result, pivot_consumed, out, mut meta) = stream_convert(
        composed.pivot.as_bytes(),
        E::UTF8,
        dst_enc,
        max_dst_bytes,
        false,
        opts,
        store,
    );
    // Back to source bytes. Everything written stops at a point the
    // two agree on, so that half is a lookup.
    let consumed = composed.src_for_pivot(pivot_consumed);
    // What a capped destination read past it is a different question:
    // the source of a piece cannot be cut in the middle, since the
    // replacement comes out *before* the cluster it precedes and a
    // prefix of the output is not a prefix of the source. So take the
    // whole piece — convert the rest of it and hold that as output,
    // the way a character split across two calls is held (#1532).
    let over = pivot_consumed + meta.dst_full_extra;
    let (end_pivot, end_src) = composed
        .next_mark(over)
        .unwrap_or((composed.pivot.len(), composed.cut));
    if end_pivot > over {
        let (_, _, rest, _) = stream_convert(
            &composed.pivot.as_bytes()[over..end_pivot],
            E::UTF8,
            dst_enc,
            None,
            false,
            opts,
            store,
        );
        meta.dst_full_out.extend_from_slice(&rest);
    }
    meta.dst_full_extra = end_src.saturating_sub(consumed);
    // What was left open is not the end of the input.
    let result =
        if composed.cut < src_bytes.len() && matches!(result, StreamConvertResult::Finished) {
            StreamConvertResult::SourceBufferEmpty
        } else {
            result
        };
    (result, consumed, out, meta)
}

/// A CESU-8 source, a chunk at a time: spell the surrogate pairs back
/// as the characters they name and hand the result to the ordinary
/// pipeline. Nothing is held back — CESU-8 is UTF-8's structure, so a
/// complete character here stays one (#1576).
fn cesu_source_stream(
    src_bytes: &[u8],
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let src_enc = E::NamedByte(crate::value::CESU_8);
    let scrubbed;
    let mut bytes = src_bytes;
    if opts.invalid_replace {
        // The replacement is the destination's and goes into the
        // pivot as text, so the pipeline converts it along with
        // everything else.
        scrubbed = crate::value::scrub_mbc(
            src_bytes,
            opts.replace_str(dst_enc).as_bytes(),
            crate::value::CESU8_MAX_LEN,
            crate::value::cesu8_precise_len,
        );
        bytes = &scrubbed;
    }
    let Some(pivot) = crate::value::cesu8_to_utf8(bytes) else {
        let good = cesu8_good_prefix(bytes);
        let (kind_before, consumed, out, meta_before) = cesu_source_stream(
            &bytes[..good],
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
        // The destination filling up among the good bytes comes first.
        if matches!(kind_before, StreamConvertResult::DestinationBufferFull) {
            return (kind_before, consumed, out, meta_before);
        }
        let (kind, meta) = bad_source_outcome(src_enc, bytes, !partial_input);
        // The run is consumed along with the byte read to disprove
        // it, which is held for `#putback` (#1617).
        let through = through_bad_run(good, &kind, &meta, bytes.len());
        return (kind, through, out, meta);
    };
    let (result, pivot_consumed, out, mut meta) = stream_convert(
        pivot.as_bytes(),
        E::UTF8,
        dst_enc,
        max_dst_bytes,
        partial_input,
        opts,
        store,
    );
    // Back to source bytes: a character below `U+10000` is spelled
    // exactly as UTF-8 spells it, and one above is its six-byte pair.
    let unit_len = |c: char| {
        if (c as u32) < 0x10000 {
            c.len_utf8()
        } else {
            crate::value::CESU8_MAX_LEN
        }
    };
    let consumed = if pivot_consumed == pivot.len() {
        src_bytes.len()
    } else {
        pivot[..pivot_consumed]
            .chars()
            .map(unit_len)
            .sum::<usize>()
            .min(src_bytes.len())
    };
    if meta.dst_full_extra > 0 {
        let end = (pivot_consumed + meta.dst_full_extra).min(pivot.len());
        meta.dst_full_extra = pivot[pivot_consumed..end].chars().map(unit_len).sum();
    }
    (result, consumed, out, meta)
}

/// A destination whose bytes are a rewrite of the UTF-8 pivot:
/// `UTF8-MAC`'s decomposition, CESU-8's surrogate spelling. The
/// pipeline converts into the pivot and the rewrite runs on the way
/// out (#1576).
/// How many stateless-ISO-2022-JP bytes `eucjp_consumed` bytes of the
/// EUC-JP rewrite came from: ASCII is one byte on both sides, a JIS X
/// 0208 cell is two in EUC-JP and three here.
fn stateless_len_for_eucjp(stateless: &[u8], eucjp_consumed: usize) -> usize {
    let (mut s_at, mut e_at) = (0, 0);
    while s_at < stateless.len() && e_at < eucjp_consumed {
        let n = if stateless[s_at] < 0x80 { 1 } else { 3 };
        e_at += if n == 1 { 1 } else { 2 };
        if e_at > eucjp_consumed {
            break;
        }
        s_at += n;
    }
    s_at
}

/// The longest prefix of `bytes` the stateless transcoder reads whole.
pub(crate) fn stateless_good_prefix(bytes: &[u8]) -> usize {
    use crate::value::PreciseLen as P;
    let mut at = 0;
    while at < bytes.len() {
        match crate::value::stateless_iso2022jp_transcode_len(bytes, at) {
            P::Char(n) if n > 0 => at += n,
            _ => break,
        }
    }
    at
}

/// A stateless-ISO-2022-JP source: rewritten to EUC-JP, then EUC-JP's
/// own conversion. Consumed counts come back through
/// [`stateless_len_for_eucjp`] (#1600).
fn stateless_source_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    let bytes = src_bytes;
    let good = stateless_good_prefix(bytes);
    let eucjp =
        crate::value::stateless_iso2022jp_to_eucjp(&bytes[..good]).unwrap_or_else(|_| Vec::new());
    // EUC-JP is the one destination this *is* the conversion for: the
    // two encodings hold the same cells, so a cell with no character
    // crosses like any other and no codec is asked (#1600).
    if dst_enc == crate::value::Encoding::EUC_JP {
        let fits = max_dst_bytes.map_or(eucjp.len(), |m| m.min(eucjp.len()));
        if fits < eucjp.len() {
            // CRuby fills the destination to the byte and holds the
            // rest of that character for the next call, taking the
            // whole of it out of `src` — so the cap is read through
            // the first character whose output runs past it (#1532).
            let (mut s_at, mut e_at) = (0, 0);
            while s_at < bytes.len() {
                let n = if bytes[s_at] < 0x80 { 1 } else { 3 };
                let out_n = if n == 1 { 1 } else { 2 };
                s_at += n;
                e_at += out_n;
                if e_at > fits {
                    break;
                }
            }
            return (
                StreamConvertResult::DestinationBufferFull,
                s_at.min(bytes.len()),
                eucjp[..fits].to_vec(),
                ErrMeta {
                    dst_full_out: eucjp[fits..e_at.min(eucjp.len())].to_vec(),
                    ..ErrMeta::default()
                },
            );
        }
        let consumed = stateless_len_for_eucjp(bytes, fits);
        if good < bytes.len() {
            let (kind, meta) = bad_source_outcome(src_enc, &bytes[good..], !partial_input);
            let through = through_bad_run(consumed, &kind, &meta, bytes.len());
            return (kind, through, eucjp, meta);
        }
        let kind = if partial_input {
            StreamConvertResult::SourceBufferEmpty
        } else {
            StreamConvertResult::Finished
        };
        return (kind, consumed, eucjp, ErrMeta::default());
    }
    let (result, eucjp_consumed, out, meta) = stream_convert(
        &eucjp,
        crate::value::Encoding::EUC_JP,
        dst_enc,
        max_dst_bytes,
        // More stateless bytes may follow the good prefix, and the
        // EUC-JP half must not call the rewrite's end the input's.
        partial_input || good < bytes.len(),
        opts,
        store,
    );
    let consumed = stateless_len_for_eucjp(bytes, eucjp_consumed);
    if good < bytes.len() && consumed == good {
        // Everything the rewrite could take converted; what stopped it
        // is the source's own malformed run — consumed, with the bytes
        // read to disprove it held for `#putback` (#1530).
        let (kind, meta) = bad_source_outcome(src_enc, &bytes[good..], !partial_input);
        let through = through_bad_run(consumed, &kind, &meta, bytes.len());
        return (kind, through, out, meta);
    }
    (result, consumed, out, meta)
}

/// A stateless-ISO-2022-JP destination: the ordinary conversion into
/// EUC-JP, then the rewrite. The two EUC-JP forms stateless has no
/// cell for — half-width katakana and JIS X 0212 — are an undefined
/// conversion against that hop (#1600).
/// An IBM037 source, streamed: every byte is one Latin-1 character,
/// so the chunk converts whole through UTF-8 and only the counts have
/// to be translated back to bytes of the source.
fn ibm037_source_stream(
    src_bytes: &[u8],
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let src_enc = ibm037_enc();
    let text = ibm037_to_utf8(src_bytes);
    let (kind, consumed, out, mut meta) = stream_convert(
        text.as_bytes(),
        E::UTF8,
        dst_enc,
        max_dst_bytes,
        partial_input,
        &reporting_as(opts, src_enc),
        store,
    );
    // A UTF-8 offset into `text` is a character count, which is the
    // byte count in the source.
    let chars_before = |at: usize| text[..at.min(text.len())].chars().count();
    let src_consumed = chars_before(consumed);
    meta.dst_full_extra = chars_before(consumed + meta.dst_full_extra) - src_consumed;
    if matches!(kind, StreamConvertResult::UndefinedConversion) && !meta.decode_stage {
        if let Some(c) = single_utf8_char(&meta.error_bytes)
            && let Some(chain) = jis_family_chain(src_enc, dst_enc)
        {
            meta.message = Some(format!(
                "U+{:04X} to {} in conversion from {}",
                c as u32,
                dst_enc.name(),
                chain_names(&chain)
            ));
        }
    }
    (kind, src_consumed, out, meta)
}

/// An IBM037 destination, streamed: ISO-8859-1's own stream does
/// the reading, the cap and the refusals, and each byte it writes is
/// one of the table's. A character it has no cell for is the
/// undefined conversion into ISO-8859-1 that CRuby reports, spelled
/// against the whole chain (#1530, #1584).
fn ibm037_dest_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let (kind, consumed, out, mut meta) = stream_convert(
        src_bytes,
        src_enc,
        E::Iso8859(1),
        max_dst_bytes,
        partial_input,
        opts,
        store,
    );
    let out = latin1_bytes_to_ibm037(&out);
    meta.dst_full_out = latin1_bytes_to_ibm037(&meta.dst_full_out);
    if matches!(kind, StreamConvertResult::UndefinedConversion)
        && !meta.decode_stage
        && meta.message.is_none()
        && let Some(c) = single_utf8_char(&meta.error_bytes)
    {
        meta.stage = Some(("UTF-8".to_string(), "ISO-8859-1".to_string()));
        meta.message = Some(ibm037_undefined_message(
            c,
            opts.report_src.unwrap_or(src_enc),
        ));
    }
    (kind, consumed, out, meta)
}

/// A stateless-ISO-2022-JP-KDDI source, streamed: the well-formed
/// prefix read into UTF8-KDDI, then UTF8-KDDI's own conversion, with
/// the counts translated back through the units read (#1530).
fn kddi_source_stream(
    src_bytes: &[u8],
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let src_enc = stateless_kddi_enc();
    let kddi = utf8_kddi_enc();
    let (text, units, good) = kddi_read(src_bytes);
    let named = opts.report_src.unwrap_or(src_enc);
    if dst_enc == kddi {
        // The one destination this *is* the conversion for.
        let fits = max_dst_bytes.map_or(text.len(), |m| m.min(text.len()));
        if fits < text.len() {
            // Filled to the byte, the rest of that character held for
            // the next call and the whole of it taken out of `src`.
            let (mut s_at, mut o_at) = (0, 0);
            for &(s, o) in &units {
                s_at += s;
                o_at += o;
                if o_at > fits {
                    break;
                }
            }
            return (
                StreamConvertResult::DestinationBufferFull,
                s_at.min(src_bytes.len()),
                text[..fits].to_vec(),
                ErrMeta {
                    dst_full_out: text[fits..o_at.min(text.len())].to_vec(),
                    ..ErrMeta::default()
                },
            );
        }
        if good < src_bytes.len() {
            let (kind, through, meta) = kddi_bad_source(src_bytes, good, partial_input);
            return (kind, through, text, meta);
        }
        let kind = if partial_input {
            StreamConvertResult::SourceBufferEmpty
        } else {
            StreamConvertResult::Finished
        };
        return (kind, good, text, ErrMeta::default());
    }
    let (result, kddi_consumed, out, mut meta) = stream_convert(
        &text,
        kddi,
        dst_enc,
        max_dst_bytes,
        // More bytes may follow the good prefix, and the UTF8-KDDI
        // half must not call the rewrite's end the input's.
        partial_input || good < src_bytes.len(),
        opts,
        store,
    );
    let consumed = kddi_src_len(&units, kddi_consumed);
    if good < src_bytes.len() && consumed == good {
        let (kind, through, meta) = kddi_bad_source(src_bytes, good, partial_input);
        return (kind, through, out, meta);
    }
    if matches!(result, StreamConvertResult::UndefinedConversion)
        && let Some(c) = single_utf8_char(&meta.error_bytes)
    {
        let chain =
            jis_family_chain(named, dst_enc).unwrap_or_else(|| vec![named, kddi, E::UTF8, dst_enc]);
        let names = chain_names(&chain);
        // What the carrier's stream refused is its own character: an
        // emoji that has no Unicode meaning, or one whose meaning the
        // far end had no cell for. UTF8-KDDI's own conversion tells
        // the two apart, and the second is reported as the Unicode.
        let unicode = if ('\u{e000}'..='\u{f8ff}').contains(&c) {
            transcode_bytes_with_opts(
                &meta.error_bytes,
                kddi,
                E::UTF8,
                &TranscodeOpts::default(),
                store,
            )
            .ok()
            .and_then(|b| single_utf8_char(&b))
        } else {
            Some(c)
        };
        match unicode {
            None => {
                meta.stage = Some(("UTF8-KDDI".to_string(), "UTF-8".to_string()));
                meta.message = Some(format!(
                    "{} to UTF-8 in conversion from {names}",
                    quote_error_bytes(&meta.error_bytes)
                ));
            }
            Some(u) => {
                match meta.message.take() {
                    // Worded by a wrapper further down the line against
                    // its own chain, which starts at UTF8-KDDI.
                    Some(m) => {
                        let upto = chain.iter().position(|&e| e == kddi).unwrap_or(0);
                        meta.message = Some(
                            m.replacen(
                                "in conversion from UTF8-KDDI to",
                                &format!("in conversion from {} to", chain_names(&chain[..=upto])),
                                1,
                            )
                            .replacen(
                                &format!("U+{:04X}", c as u32),
                                &format!("U+{:04X}", u as u32),
                                1,
                            ),
                        );
                    }
                    None => {
                        // The hop that gave up is the far end's own
                        // way in: EUC-JP for the stateless-ISO-2022-JP
                        // pair, a wrapper's inner encoding, ISO-8859-1
                        // for IBM037, the destination itself elsewhere.
                        let hop = if let Some(w) = jis_wrapper(dst_enc) {
                            if stateless_iso2022jp(w.inner).is_some() {
                                Some(E::EUC_JP)
                            } else {
                                Some(w.inner)
                            }
                        } else if stateless_iso2022jp(dst_enc).is_some() {
                            Some(E::EUC_JP)
                        } else if is_ibm037(dst_enc) {
                            Some(E::Iso8859(1))
                        } else {
                            None
                        };
                        let hop_dst = match hop {
                            Some(h) => {
                                meta.stage = Some(("UTF-8".to_string(), h.name().to_string()));
                                h.name().to_string()
                            }
                            None => error_stage_names(kddi, dst_enc, false).1,
                        };
                        meta.message = Some(format!(
                            "U+{:04X} to {hop_dst} in conversion from {names}",
                            u as u32
                        ));
                    }
                }
                let mut buf = [0u8; 4];
                meta.error_bytes = u.encode_utf8(&mut buf).as_bytes().to_vec();
            }
        }
    }
    (result, consumed, out, meta)
}

/// A stateless-ISO-2022-JP-KDDI destination, streamed: the source
/// reaches UTF8-KDDI by its own stream, and each character then
/// writes its cell — or is the undefined conversion out of UTF8-KDDI
/// that CRuby reports (#1530).
fn kddi_dest_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    let dst_enc = stateless_kddi_enc();
    let kddi = utf8_kddi_enc();
    let mut inner = opts.clone();
    inner.universal_newline = false;
    inner.crlf_newline = false;
    inner.cr_newline = false;
    inner.lf_newline = false;
    let (kind, consumed, utf8, meta) = if src_enc == kddi {
        // Its own bytes, once they are read as characters: a
        // malformed run is the source's to report, and a chunk that
        // ends inside a character keeps that tail for the next one.
        let good =
            std::str::from_utf8(src_bytes).map_or_else(|e| e.valid_up_to(), |_| src_bytes.len());
        if good < src_bytes.len() {
            let (kind, meta) = bad_source_outcome(kddi, &src_bytes[good..], !partial_input);
            let through = if matches!(kind, StreamConvertResult::SourceBufferEmpty) {
                good
            } else {
                through_bad_run(good, &kind, &meta, src_bytes.len())
            };
            (kind, through, src_bytes[..good].to_vec(), meta)
        } else {
            let k = if partial_input {
                StreamConvertResult::SourceBufferEmpty
            } else {
                StreamConvertResult::Finished
            };
            (k, src_bytes.len(), src_bytes.to_vec(), ErrMeta::default())
        }
    } else {
        stream_convert(src_bytes, src_enc, kddi, None, partial_input, &inner, store)
    };
    let text = String::from_utf8_lossy(&utf8).into_owned();
    let text = if opts.has_newline() {
        opts.apply_newline(&text)
    } else {
        text
    };
    // Where a character of `text` came from in the source, for the
    // counts a stop has to report.
    let source_through = |utf8_at: usize| -> usize {
        pivot_prefix_consumed_in(
            src_bytes,
            src_enc,
            kddi,
            utf8_at.min(utf8.len()),
            &inner,
            store,
        )
    };
    let spell = |c: char, out: &mut Vec<u8>| -> bool {
        if c.is_ascii() {
            out.push(c as u8);
            return true;
        }
        match kddi_char_cell(c) {
            Some([b1, b2]) => {
                out.extend_from_slice(&[0x92, b1, b2]);
                true
            }
            None => false,
        }
    };
    let mut out: Vec<u8> = Vec::with_capacity(text.len());
    let mut at = 0usize;
    for c in text.chars() {
        let mut unit: Vec<u8> = Vec::with_capacity(3);
        if !spell(c, &mut unit) {
            if opts.undef_replace {
                for r in opts.replace_str(dst_enc).chars() {
                    spell(r, &mut out);
                }
                at += c.len_utf8();
                continue;
            }
            let mut buf = [0u8; 4];
            return (
                StreamConvertResult::UndefinedConversion,
                source_through(at + c.len_utf8()),
                out,
                ErrMeta {
                    error_bytes: c.encode_utf8(&mut buf).as_bytes().to_vec(),
                    decode_stage: false,
                    stage: Some((
                        "UTF8-KDDI".to_string(),
                        "stateless-ISO-2022-JP-KDDI".to_string(),
                    )),
                    message: Some(kddi_undefined_message(
                        c,
                        opts.report_src.unwrap_or(src_enc),
                        dst_enc,
                    )),
                    ..ErrMeta::default()
                },
            );
        }
        if let Some(max) = max_dst_bytes
            && out.len() + unit.len() > max
        {
            // Filled to the byte, the rest of the cell held for the
            // next call, the character read.
            let fits = max - out.len();
            let written_through = source_through(at);
            let through_tried = source_through(at + c.len_utf8());
            out.extend_from_slice(&unit[..fits]);
            return (
                StreamConvertResult::DestinationBufferFull,
                written_through,
                out,
                ErrMeta {
                    dst_full_extra: through_tried.saturating_sub(written_through),
                    dst_full_out: unit[fits..].to_vec(),
                    ..ErrMeta::default()
                },
            );
        }
        out.extend_from_slice(&unit);
        at += c.len_utf8();
    }
    (kind, consumed, out, meta)
}

fn stateless_dest_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    // An EUC-JP source needs no conversion to reach the pivot: the
    // two encodings hold the same cells, so the rewrite below is the
    // whole of it and a cell with no character crosses like any other
    // (#1600).
    let (result, consumed, eucjp, mut meta) = if src_enc == crate::value::Encoding::EUC_JP {
        let kind = if partial_input {
            StreamConvertResult::SourceBufferEmpty
        } else {
            StreamConvertResult::Finished
        };
        (
            kind,
            src_bytes.len(),
            src_bytes.to_vec(),
            ErrMeta::default(),
        )
    } else {
        stream_convert(
            src_bytes,
            src_enc,
            crate::value::Encoding::EUC_JP,
            None,
            partial_input,
            opts,
            store,
        )
    };
    // An error on the way *into* EUC-JP is that hop's: `errinfo` names
    // `["UTF-8", "EUC-JP"]` for `U+20AC`, not the rewrite after it
    // (#1618). The message already says as much.
    if matches!(
        result,
        StreamConvertResult::InvalidByteSequence
            | StreamConvertResult::UndefinedConversion
            | StreamConvertResult::IncompleteInput
    ) && meta.stage.is_none()
    {
        let decode_stage =
            !matches!(result, StreamConvertResult::UndefinedConversion) || meta.decode_stage;
        meta.stage = Some(error_stage_names(
            src_enc,
            crate::value::Encoding::EUC_JP,
            decode_stage,
        ));
        if !decode_stage
            && meta.message.is_none()
            && let Some(c) = single_utf8_char(&meta.error_bytes)
        {
            meta.message = Some(undefined_before_eucjp_message(
                c,
                opts.report_src.unwrap_or(src_enc),
                dst_enc,
            ));
        }
    }
    let mut out = Vec::with_capacity(eucjp.len());
    let mut at = 0;
    while at < eucjp.len() {
        let (unit, n): (Vec<u8>, usize) = match eucjp[at] {
            b @ 0x00..=0x7f => (vec![b], 1),
            0xa1..=0xfe if matches!(eucjp.get(at + 1), Some(0xa1..=0xfe)) => {
                (vec![0x92, eucjp[at], eucjp[at + 1]], 2)
            }
            _ => match crate::value::eucjp_char_width(&eucjp[at..]) {
                None => {
                    // Not a cell at all. Only an EUC-JP source can
                    // put one here — the transcoders write none —
                    // and it is malformed input, `"\x80" on EUC-JP`,
                    // not a character the rewrite has no cell for
                    // (#1618).
                    let (kind, bad) = bad_source_outcome(
                        crate::value::Encoding::EUC_JP,
                        &eucjp[at..],
                        !partial_input,
                    );
                    if opts.invalid_replace
                        && !matches!(kind, StreamConvertResult::SourceBufferEmpty)
                    {
                        let skip = bad.error_bytes.len().max(1);
                        (opts.replace_str(dst_enc).as_bytes().to_vec(), skip)
                    } else {
                        let through = through_bad_run(at, &kind, &bad, eucjp.len());
                        return (
                            kind,
                            pivot_prefix_consumed_in(
                                src_bytes,
                                src_enc,
                                crate::value::Encoding::EUC_JP,
                                through,
                                opts,
                                store,
                            ),
                            out,
                            bad,
                        );
                    }
                }
                Some(cell_len) => {
                    let cell_len = cell_len.max(1);
                    // The character with no cell has been read: it is
                    // the error's bytes, and leaves the caller's
                    // source with them (#1617).
                    let through = (at + cell_len).min(eucjp.len());
                    return (
                        StreamConvertResult::UndefinedConversion,
                        pivot_prefix_consumed_in(
                            src_bytes,
                            src_enc,
                            crate::value::Encoding::EUC_JP,
                            through,
                            opts,
                            store,
                        ),
                        out,
                        ErrMeta {
                            error_bytes: eucjp[at..(at + cell_len).min(eucjp.len())].to_vec(),
                            decode_stage: false,
                            ..ErrMeta::default()
                        },
                    );
                }
            },
        };
        if let Some(max) = max_dst_bytes
            && out.len() + unit.len() > max
        {
            let fits = max - out.len();
            let written_through = pivot_prefix_consumed_in(
                src_bytes,
                src_enc,
                crate::value::Encoding::EUC_JP,
                at,
                opts,
                store,
            );
            let through_tried = pivot_prefix_consumed_in(
                src_bytes,
                src_enc,
                crate::value::Encoding::EUC_JP,
                at + n,
                opts,
                store,
            );
            out.extend_from_slice(&unit[..fits]);
            return (
                StreamConvertResult::DestinationBufferFull,
                written_through,
                out,
                ErrMeta {
                    dst_full_extra: through_tried.saturating_sub(written_through),
                    dst_full_out: unit[fits..].to_vec(),
                    ..ErrMeta::default()
                },
            );
        }
        out.extend_from_slice(&unit);
        at += n;
    }
    let _ = dst_enc;
    (result, consumed, out, meta)
}

fn pivot_rewrite_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    let (result, consumed, pivot, meta) = stream_convert(
        src_bytes,
        src_enc,
        E::UTF8,
        None,
        partial_input,
        opts,
        store,
    );
    let text = String::from_utf8_lossy(&pivot);
    // The unit the rewrite runs on. For `UTF8-MAC` it is the whole
    // cluster, since canonical reordering can move a mark past the
    // one before it; CESU-8 rewrites character by character.
    let pieces: Vec<(usize, usize, Vec<u8>)> = if dst_enc == E::Utf8(crate::value::UTF8_MAC) {
        crate::value::mac_clusters(&text)
            .into_iter()
            .map(|r| {
                (
                    r.start,
                    r.end,
                    crate::value::utf8_to_mac(&text[r.clone()]).into_bytes(),
                )
            })
            .collect()
    } else {
        text.char_indices()
            .map(|(at, c)| {
                let mut buf = [0u8; 4];
                (
                    at,
                    at + c.len_utf8(),
                    crate::value::utf8_to_cesu8(c.encode_utf8(&mut buf)),
                )
            })
            .collect()
    };
    let mut out = Vec::with_capacity(pivot.len());
    for (start, end, unit) in pieces {
        if let Some(max) = max_dst_bytes
            && out.len() + unit.len() > max
        {
            // Fill to the byte and hold the rest of this unit for the
            // next call, as the UTF-16 destination does (#1532).
            let fits = max - out.len();
            let written_through = pivot_prefix_consumed(src_bytes, src_enc, start, opts, store);
            let through_tried = pivot_prefix_consumed(src_bytes, src_enc, end, opts, store);
            out.extend_from_slice(&unit[..fits]);
            return (
                StreamConvertResult::DestinationBufferFull,
                written_through,
                out,
                ErrMeta {
                    dst_full_extra: through_tried.saturating_sub(written_through),
                    dst_full_out: unit[fits..].to_vec(),
                    ..ErrMeta::default()
                },
            );
        }
        out.extend_from_slice(&unit);
    }
    (result, consumed, out, meta)
}

/// A chunk through a carrier set.
///
/// The carriers hold no state across calls, so the only question a
/// chunk raises is where it stops being well-formed for its base: the
/// prefix that is converts, and the rest is reported the way the base
/// would report it. Everything else is the one-shot conversion, which
/// is where the tables live (#1573).
#[allow(clippy::too_many_arguments)]
fn carrier_stream(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    // Only the destination is a carrier: the source is an ordinary
    // encoding, so the pipeline reads it into the pivot and the table
    // runs on the way out — the shape `pivot_rewrite_stream` has.
    if carrier_vendor(src_enc).is_none() {
        let (result, consumed, pivot, meta) = stream_convert(
            src_bytes,
            src_enc,
            crate::value::Encoding::UTF8,
            None,
            partial_input,
            opts,
            store,
        );
        let out = match transcode_bytes_with_opts(
            &pivot,
            crate::value::Encoding::UTF8,
            dst_enc,
            opts,
            store,
        ) {
            Ok(out) => out,
            Err(_) => {
                let text = String::from_utf8_lossy(&pivot);
                let mut written: Vec<u8> = vec![];
                let mut at = 0;
                for c in text.chars() {
                    match transcode_bytes_with_opts(
                        c.to_string().as_bytes(),
                        crate::value::Encoding::UTF8,
                        dst_enc,
                        opts,
                        store,
                    ) {
                        Ok(piece) => {
                            written.extend_from_slice(&piece);
                            at += c.len_utf8();
                        }
                        Err(_) => {
                            return (
                                StreamConvertResult::UndefinedConversion,
                                pivot_prefix_consumed(src_bytes, src_enc, at, opts, store),
                                written,
                                ErrMeta {
                                    error_bytes: c.to_string().into_bytes(),
                                    readagain_bytes: vec![],
                                    ..ErrMeta::default()
                                },
                            );
                        }
                    }
                }
                written
            }
        };
        return (result, consumed, out, meta);
    }
    // How much of the source reads as whole characters of its own
    // encoding. A carrier's walk is its base's.
    let good = match conversion_walker(src_enc) {
        Some((_, precise)) => {
            let mut at = 0;
            while at < src_bytes.len() {
                match precise(src_bytes, at) {
                    crate::value::PreciseLen::Char(n) if n > 0 => at += n,
                    _ => break,
                }
            }
            at
        }
        None => match std::str::from_utf8(src_bytes) {
            Ok(_) => src_bytes.len(),
            Err(e) => e.valid_up_to(),
        },
    };
    let head = &src_bytes[..good];
    let out = match transcode_bytes_with_opts(head, src_enc, dst_enc, opts, store) {
        Ok(out) => out,
        Err(_) => {
            // The conversion refused a character the carrier does not
            // hold. Convert what precedes it and report it the way the
            // pipeline does, by converting one character at a time up
            // to the first refusal.
            let mut written: Vec<u8> = vec![];
            let mut at = 0;
            while at < good {
                let n = match conversion_walker(src_enc) {
                    Some((_, precise)) => match precise(src_bytes, at) {
                        crate::value::PreciseLen::Char(n) if n > 0 => n,
                        _ => break,
                    },
                    None => match std::str::from_utf8(&src_bytes[at..]) {
                        Ok(rest) => rest.chars().next().map_or(1, |c| c.len_utf8()),
                        Err(_) => break,
                    },
                };
                match transcode_bytes_with_opts(
                    &src_bytes[at..at + n],
                    src_enc,
                    dst_enc,
                    opts,
                    store,
                ) {
                    Ok(piece) => {
                        written.extend_from_slice(&piece);
                        at += n;
                    }
                    Err(_) => break,
                }
            }
            let bad = &src_bytes[at..(at + 4).min(src_bytes.len())];
            let own = carrier_error_char(bad, src_enc);
            // The far end refused the character's Unicode meaning,
            // when it has one; a carrier emoji with none is refused
            // at the carrier's own hop into UTF-8 (#1530).
            let unicode = if dst_enc == crate::value::Encoding::UTF8 {
                None
            } else {
                transcode_bytes_with_opts(
                    &own,
                    src_enc,
                    crate::value::Encoding::UTF8,
                    &TranscodeOpts::default(),
                    store,
                )
                .ok()
                .filter(|b| single_utf8_char(b).is_some())
            };
            let (error_bytes, stage, message) =
                match (unicode, carrier_pua_bytes(&own, src_enc, store)) {
                    (Some(u), _) => (u, None, None),
                    (None, Some(pua)) => {
                        let named = opts.report_src.unwrap_or(src_enc);
                        (
                            pua.clone(),
                            Some((
                                carrier_utf8_form(src_enc)
                                    .map_or(src_enc, |e| e)
                                    .name()
                                    .to_string(),
                                "UTF-8".to_string(),
                            )),
                            Some(carrier_no_unicode_message(&pua, named, dst_enc)),
                        )
                    }
                    // A cell the carrier holds nothing in at all.
                    (None, None) => (own, None, None),
                };
            return (
                StreamConvertResult::UndefinedConversion,
                at,
                written,
                ErrMeta {
                    error_bytes,
                    readagain_bytes: vec![],
                    stage,
                    message,
                    ..ErrMeta::default()
                },
            );
        }
    };
    if good < src_bytes.len() {
        let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
        return (kind, good, out, meta);
    }
    if let Some(max) = max_dst_bytes
        && out.len() > max
    {
        // The cap cuts the output; hold the rest for the next call.
        // CRuby stops on the character whose output does not fit,
        // having read it, and reads no further (#1530).
        let fits = max;
        let held = out[fits..].to_vec();
        let consumed =
            pivot_prefix_consumed_in(src_bytes, src_enc, dst_enc, fits + 1, opts, store).min(good);
        return (
            StreamConvertResult::DestinationBufferFull,
            consumed,
            out[..fits].to_vec(),
            ErrMeta {
                dst_full_out: held,
                ..ErrMeta::default()
            },
        );
    }
    (StreamConvertResult::Finished, good, out, ErrMeta::default())
}

/// The character a carrier refused, in the UTF-8 the error machinery
/// stores it as.
fn carrier_error_char(bytes: &[u8], src_enc: crate::value::Encoding) -> Vec<u8> {
    let base = carrier_base(src_enc).unwrap_or(src_enc);
    if base == crate::value::Encoding::UTF8 {
        return match std::str::from_utf8(bytes) {
            Ok(s) => s.chars().next().map(|c| c.to_string().into_bytes()),
            Err(e) => std::str::from_utf8(&bytes[..e.valid_up_to()])
                .ok()
                .and_then(|s| s.chars().next())
                .map(|c| c.to_string().into_bytes()),
        }
        .unwrap_or_else(|| bytes.to_vec());
    }
    let n = match crate::value::sjis_precise_len(bytes, 0) {
        crate::value::PreciseLen::Char(n) => n,
        _ => return bytes.to_vec(),
    };
    bytes[..n.min(bytes.len())].to_vec()
}

pub(crate) fn stream_convert(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
    store: &Store,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    // stateless-ISO-2022-JP-KDDI is read into and written from
    // UTF8-KDDI, which is itself a carrier, so it goes ahead of the
    // carrier dispatch below (#1530).
    // Its own wrapper is the exception: ISO-2022-JP-KDDI to and from
    // it is the escape rewrite alone, which the wrapper blocks do.
    if src_enc != dst_enc {
        if src_enc == stateless_kddi_enc() && !kddi_wrapper(dst_enc) {
            return kddi_source_stream(
                src_bytes,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if dst_enc == stateless_kddi_enc() && !kddi_wrapper(src_enc) {
            return kddi_dest_stream(
                src_bytes,
                src_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
    }
    // The pivot wrappers #1562 gave `String#encode` — `UTF8-MAC`'s
    // normalisation, CESU-8's surrogate spelling — are conversions in
    // their own right, so a stream through one is the ordinary
    // pipeline with the rewrite on the side that needs it. They come
    // before the identity fast path below, which would otherwise hand
    // the bytes through unconverted (#1576).
    // A carrier set on either side is the same story: its base's
    // structure, with a table saying which characters it spells
    // differently. It is stateless — no BOM, nothing held back — so a
    // chunk converts on its own, once its well-formed prefix is
    // settled (#1573).
    if carrier_vendor(src_enc).is_some() || carrier_vendor(dst_enc).is_some() {
        return carrier_stream(
            src_bytes,
            src_enc,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
    }
    let mac = E::Utf8(crate::value::UTF8_MAC);
    let cesu = E::NamedByte(crate::value::CESU_8);
    if src_enc != dst_enc {
        if is_ibm037(src_enc) {
            return ibm037_source_stream(
                src_bytes,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if is_ibm037(dst_enc) {
            return ibm037_dest_stream(
                src_bytes,
                src_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if src_enc == mac {
            return mac_source_stream(
                src_bytes,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if src_enc == cesu {
            return cesu_source_stream(
                src_bytes,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        // ISO-2022-JP rides stateless-ISO-2022-JP, which rides EUC-JP.
        // The designation is the only thing that survives a chunk, and
        // `opts` / `meta` carry it in and out (#1609).
        if let Some(w) = jis_wrapper(src_enc) {
            let (stateless, left) = match (w.read)(src_bytes, opts.iso_state) {
                Ok(v) => v,
                // With `invalid: :replace` a malformed run is
                // substituted here, but an *incomplete* one is
                // only malformed once the input has ended — until
                // then it is held for the next chunk (#1609).
                Err(ref stop) if opts.invalid_replace && (!stop.incomplete || !partial_input) => {
                    let out = transcode_bytes_with_opts(src_bytes, src_enc, dst_enc, opts, store)
                        .unwrap_or_default();
                    let kind = if partial_input {
                        StreamConvertResult::SourceBufferEmpty
                    } else {
                        StreamConvertResult::Finished
                    };
                    return (kind, src_bytes.len(), out, ErrMeta::default());
                }
                Err(stop) => {
                    let (head, head_left) =
                        (w.read)(&src_bytes[..stop.at], opts.iso_state).unwrap_or_default();
                    let (kind, consumed, out, mut meta) =
                        stream_convert(&head, w.inner, dst_enc, max_dst_bytes, true, opts, store);
                    // The destination filling up happens first.
                    // CRuby reads the source in order, so a cap
                    // that stops among the bytes *before* the
                    // malformed run reports itself and leaves the
                    // run for the next call to trip over (#1609).
                    if matches!(kind, StreamConvertResult::DestinationBufferFull) {
                        let through = if consumed == head.len() {
                            stop.at
                        } else {
                            pivot_prefix_consumed_in(
                                src_bytes, src_enc, w.inner, consumed, opts, store,
                            )
                        };
                        meta.iso_state_out = (w.read)(&src_bytes[..through], opts.iso_state)
                            .map(|(_, left)| left)
                            .unwrap_or(opts.iso_state);
                        return (kind, through, out, meta);
                    }
                    let kind = if stop.incomplete && partial_input {
                        StreamConvertResult::SourceBufferEmpty
                    } else if stop.incomplete {
                        StreamConvertResult::IncompleteInput
                    } else {
                        StreamConvertResult::InvalidByteSequence
                    };
                    meta.error_bytes = stop.error.clone();
                    meta.readagain_bytes = stop.again.clone();
                    // The designation the chunk left in effect is
                    // the one its next chunk resumes in — a cell
                    // split across two calls is read as a cell.
                    meta.iso_state_out = head_left;
                    let through = if matches!(kind, StreamConvertResult::InvalidByteSequence) {
                        (stop.at + stop.error.len() + stop.again.len()).min(src_bytes.len())
                    } else {
                        stop.at
                    };
                    return (kind, through, out, meta);
                }
            };
            // stateless-ISO-2022-JP *is* what the rewrite produced, so
            // that destination needs no second conversion — and the
            // cap applies to these bytes directly.
            if dst_enc == w.inner {
                let kind = if partial_input {
                    StreamConvertResult::SourceBufferEmpty
                } else {
                    StreamConvertResult::Finished
                };
                if let Some(max) = max_dst_bytes
                    && max <= stateless.len()
                {
                    // The cap is met unit by unit, the way CRuby's
                    // one-transcoder path reads the source (#1619):
                    // an escape sequence is read and writes nothing,
                    // and if the destination is full once it has been
                    // read the call stops there — even at the end of
                    // the input, so `"\e$B0l\e(B"` into three bytes
                    // is `:destination_buffer_full` with everything
                    // read and nothing held. A character that fits is
                    // written; one that does not is read whole, the
                    // bytes that fit are written and the rest held
                    // for the next call (#1532) — with no room at all
                    // included, so a cap of 0 reads the first
                    // character and holds it.
                    let mut out: Vec<u8> = Vec::with_capacity(max);
                    let mut pos = 0;
                    let mut state = opts.iso_state;
                    while pos < src_bytes.len() {
                        // The shortest prefix the reader accepts is
                        // the next unit: one character, or one
                        // escape sequence.
                        let Some((n, unit, next)) =
                            (1..=4.min(src_bytes.len() - pos)).find_map(|n| {
                                (w.read)(&src_bytes[pos..pos + n], state)
                                    .ok()
                                    .map(|(unit, next)| (n, unit, next))
                            })
                        else {
                            break;
                        };
                        pos += n;
                        state = next;
                        if unit.is_empty() {
                            if out.len() >= max {
                                return (
                                    StreamConvertResult::DestinationBufferFull,
                                    pos,
                                    out,
                                    ErrMeta {
                                        iso_state_out: state,
                                        ..ErrMeta::default()
                                    },
                                );
                            }
                            continue;
                        }
                        if out.len() + unit.len() > max {
                            let fits = max - out.len();
                            out.extend_from_slice(&unit[..fits]);
                            return (
                                StreamConvertResult::DestinationBufferFull,
                                pos,
                                out,
                                ErrMeta {
                                    dst_full_out: unit[fits..].to_vec(),
                                    iso_state_out: state,
                                    ..ErrMeta::default()
                                },
                            );
                        }
                        out.extend_from_slice(&unit);
                    }
                    return (
                        kind,
                        src_bytes.len(),
                        out,
                        ErrMeta {
                            iso_state_out: state,
                            ..ErrMeta::default()
                        },
                    );
                }
                return (
                    kind,
                    src_bytes.len(),
                    stateless,
                    ErrMeta {
                        iso_state_out: left,
                        ..ErrMeta::default()
                    },
                );
            }
            let (kind, stateless_consumed, out, mut meta) = stream_convert(
                &stateless,
                w.inner,
                dst_enc,
                max_dst_bytes,
                partial_input,
                &reporting_as(opts, src_enc),
                store,
            );
            meta.iso_state_out = left;
            if matches!(kind, StreamConvertResult::UndefinedConversion) && meta.decode_stage {
                wrapper_src_undefined(&mut meta, src_enc, dst_enc);
            }
            // A cap stops part way through the rewrite, so the source
            // bytes it read are the ones that produced the stateless
            // prefix — escape sequences included, since they are read
            // whole and write nothing. Only a run that *ended* takes
            // the whole source: one that stopped — on an error or on a
            // full destination — read no further than the character it
            // choked on, so a trailing escape sequence is still the
            // caller's (#1609).
            let ended = matches!(
                kind,
                StreamConvertResult::Finished | StreamConvertResult::SourceBufferEmpty
            );
            let consumed = if ended && stateless_consumed == stateless.len() {
                src_bytes.len()
            } else {
                pivot_prefix_consumed_in(
                    src_bytes,
                    src_enc,
                    w.inner,
                    stateless_consumed,
                    opts,
                    store,
                )
            };
            // The character a cap read and could not write is counted
            // in stateless bytes by the inner stream; the caller takes
            // *source* bytes out of `src`, and an escape sequence in
            // front of the character is read with it. Counting three
            // for `"\e$B"` + a cell left `"(B"` in the source as text
            // (#1619).
            if meta.dst_full_extra > 0 {
                let through = pivot_prefix_consumed_in(
                    src_bytes,
                    src_enc,
                    w.inner,
                    stateless_consumed + meta.dst_full_extra,
                    opts,
                    store,
                );
                meta.dst_full_extra = through.saturating_sub(consumed);
            }
            // A call that stopped short leaves the designation in
            // effect *there*, not the one the whole chunk ended in:
            // the escapes past that point are still the caller's, and
            // the next call reads them itself.
            let read_to = consumed + meta.dst_full_extra;
            if read_to < src_bytes.len() {
                meta.iso_state_out = (w.read)(&src_bytes[..read_to], opts.iso_state)
                    .map(|(_, l)| l)
                    .unwrap_or(opts.iso_state);
            }
            return (kind, consumed, out, meta);
        }
        if let Some(w) = jis_wrapper(dst_enc) {
            let (kind, consumed, stateless, mut meta) = if src_enc == w.inner {
                let k = if partial_input {
                    StreamConvertResult::SourceBufferEmpty
                } else {
                    StreamConvertResult::Finished
                };
                (k, src_bytes.len(), src_bytes.to_vec(), ErrMeta::default())
            } else {
                stream_convert(
                    src_bytes,
                    src_enc,
                    w.inner,
                    None,
                    partial_input,
                    opts,
                    store,
                )
            };
            if matches!(kind, StreamConvertResult::UndefinedConversion) {
                wrapper_dst_undefined(&mut meta, src_enc, dst_enc, w);
            }
            // The closing escape belongs to the end of the input, so a
            // chunk that may be followed by more does not write it —
            // and neither does one that stopped on an error, which is
            // not the end of anything (#1609).
            let ended = matches!(
                kind,
                StreamConvertResult::Finished | StreamConvertResult::SourceBufferEmpty
            );
            return match (w.write)(&stateless, opts.iso_state, ended && !partial_input) {
                Ok((out, left)) => {
                    // The cap counts ISO-2022-JP bytes and CRuby
                    // fills the destination to the byte — a partial
                    // escape sequence included — holding the rest for
                    // the next call rather than leaving it in `src`
                    // (#1532).
                    if let Some(max) = max_dst_bytes
                        && out.len() > max
                    {
                        let read = jis_read_through(w.write, &stateless, opts.iso_state, max);
                        // A run that stopped on an error consumed the
                        // malformed bytes too, and those are past the
                        // character the cap stopped on.
                        let consumed = if read == stateless.len() && ended {
                            consumed
                        } else if src_enc == w.inner {
                            read.min(src_bytes.len())
                        } else {
                            pivot_prefix_consumed_in(src_bytes, src_enc, w.inner, read, opts, store)
                        };
                        return (
                            StreamConvertResult::DestinationBufferFull,
                            consumed,
                            out[..max].to_vec(),
                            ErrMeta {
                                dst_full_out: out[max..].to_vec(),
                                iso_state_out: left,
                                ..ErrMeta::default()
                            },
                        );
                    }
                    // A destination filled to the last byte is
                    // still full when the stream ends in ASCII: the
                    // encoder's closing reset writes nothing then,
                    // and it cannot know that without room to try, so
                    // CRuby asks for one more call. A stream ending
                    // in JIS wrote its `ESC ( B` and is done (#1609).
                    let closed_empty = (w.write)(&stateless, opts.iso_state, false)
                        .is_ok_and(|(_, l)| l.is_none());
                    if max_dst_bytes == Some(out.len())
                        && !partial_input
                        && closed_empty
                        && matches!(kind, StreamConvertResult::Finished)
                    {
                        return (
                            StreamConvertResult::DestinationBufferFull,
                            consumed,
                            out,
                            ErrMeta {
                                iso_state_out: left,
                                ..ErrMeta::default()
                            },
                        );
                    }
                    meta.iso_state_out = left;
                    (kind, consumed, out, meta)
                }
                Err(at) => {
                    // The run before the malformed one was converted,
                    // and CRuby writes it: the error stops the
                    // conversion, it does not undo what came before
                    // (#1609).
                    let (mut out, left) =
                        (w.write)(&stateless[..at], opts.iso_state, false).unwrap_or_default();
                    // Reaching here means the *stateless* bytes are
                    // malformed, and those are either `src_bytes`
                    // verbatim or the output of a conversion, which is
                    // well-formed by construction — so the source is
                    // stateless-ISO-2022-JP itself and its offsets are
                    // the ones below. (`w.inner` is the exact
                    // encoding, not the pair: a KDDI-stateless source
                    // is *converted* to it and takes the Ok arm.)
                    debug_assert_eq!(src_enc, w.inner);
                    let (k, mut m) = bad_source_outcome(w.inner, &stateless[at..], !partial_input);
                    m.iso_state_out = left;
                    if let Some(max) = max_dst_bytes
                        && out.len() > max
                    {
                        let rest = out.split_off(max);
                        let read = jis_read_through(w.write, &stateless[..at], opts.iso_state, max);
                        let consumed = read.min(src_bytes.len());
                        return (
                            StreamConvertResult::DestinationBufferFull,
                            consumed,
                            out,
                            ErrMeta {
                                dst_full_out: rest,
                                iso_state_out: left,
                                ..ErrMeta::default()
                            },
                        );
                    }
                    let through = at + m.error_bytes.len() + m.readagain_bytes.len();
                    (k, through.min(src_bytes.len()), out, m)
                }
            };
        }
        if let Some(stateless) = stateless_iso2022jp(src_enc) {
            return stateless_source_stream(
                src_bytes,
                stateless,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if let Some(stateless) = stateless_iso2022jp(dst_enc) {
            return stateless_dest_stream(
                src_bytes,
                src_enc,
                stateless,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
        if dst_enc == mac || dst_enc == cesu {
            return pivot_rewrite_stream(
                src_bytes,
                src_enc,
                dst_enc,
                max_dst_bytes,
                partial_input,
                opts,
                store,
            );
        }
    }
    // Identity-copy fast path: only safe when the *source* is
    // known-valid for its declared encoding. Same-encoding pairs
    // (UTF-8 → UTF_8_MAC alias, UTF-8 → UTF-8, …) still need
    // validation because the spec requires
    // `:invalid_byte_sequence` reporting on bad input. Restrict
    // the fast path to the all-ASCII / ASCII-compatible case
    // where every byte is unambiguously valid.
    // The same rule for the streaming half: the bytes before the
    // offending one convert, and that byte alone is the malformed
    // run — nothing is read again after it, since it can never begin
    // a sequence (#1596).
    if src_enc == E::UsAscii
        && src_enc != dst_enc
        && !opts.invalid_replace
        && let Some(at) = src_bytes.iter().position(|&b| b >= 0x80)
    {
        let (kind, consumed, out, meta) = stream_convert(
            &src_bytes[..at],
            src_enc,
            dst_enc,
            max_dst_bytes,
            true,
            opts,
            store,
        );
        if !matches!(
            kind,
            StreamConvertResult::Finished | StreamConvertResult::SourceBufferEmpty
        ) {
            return (kind, consumed, out, meta);
        }
        let (kind, meta) = bad_source_outcome(src_enc, &src_bytes[at..], !partial_input);
        // The malformed run is *consumed*: `primitive_convert` leaves
        // only what follows it in `src`, since the bytes are readable
        // from `#primitive_errinfo` (and `#putback`) instead. There is
        // no pending-run case to hold bytes back for and none to read
        // again: a byte at or above 0x80 can never begin a US-ASCII
        // sequence, so the run is this one byte and it is complete
        // however the source is split.
        debug_assert!(matches!(kind, StreamConvertResult::InvalidByteSequence));
        debug_assert!(meta.readagain_bytes.is_empty());
        let through_error = (consumed + meta.error_bytes.len()).min(src_bytes.len());
        return (kind, through_error, out, meta);
    }
    let all_ascii = src_bytes.iter().all(|&b| b < 0x80);
    if all_ascii && src_enc.is_ascii_compatible() && dst_enc.is_ascii_compatible() {
        let limit = max_dst_bytes
            .unwrap_or(src_bytes.len())
            .min(src_bytes.len());
        let (result, meta) = if limit < src_bytes.len() {
            (
                StreamConvertResult::DestinationBufferFull,
                // One byte per character here, so the character the
                // cap cut off is exactly one more source byte, and
                // its output is that same byte (#1532).
                ErrMeta {
                    dst_full_extra: 1,
                    dst_full_out: vec![src_bytes[limit]],
                    ..ErrMeta::default()
                },
            )
        } else {
            (StreamConvertResult::Finished, ErrMeta::default())
        };
        return (result, limit, src_bytes[..limit].to_vec(), meta);
    }
    // A dummy `UTF-16` / `UTF-32` source whose byte order nothing has
    // settled: its BOM opens the stream and says which end the code
    // units start at, and a stream that does not open with one is
    // ill-formed. A converter settles this once and passes the
    // concrete encoding in; this is the one-shot answer (#1576).
    if dummy_wide_target(src_enc).is_some() {
        let Some((wide, rest)) = dummy_wide_source(src_enc, src_bytes) else {
            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
            return (kind, 0, vec![], meta);
        };
        let eaten = src_bytes.len() - rest.len();
        let (result, consumed, out, meta) = stream_convert(
            rest,
            wide,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
        return (result, consumed + eaten, out, meta);
    }
    // UTF-16 / UTF-32 on either side. `encoding_rs` has no UTF-32 at
    // all and no UTF-16 *encoder*, so the pivot is built and consumed
    // here with the same helpers `String#encode` uses (#1509).
    if is_utf16_or_32(src_enc) {
        let (pivot, decode_err) = decode_utf16_32(src_bytes, src_enc);
        if decode_err && !opts.invalid_replace {
            // The units before the bad one still convert: CRuby writes
            // them to the destination and *then* reports the error.
            let at = utf16_32_first_error(src_bytes, src_enc).unwrap_or(0);
            let (_, _, out, _) = stream_convert(
                &src_bytes[..at],
                src_enc,
                dst_enc,
                max_dst_bytes,
                false,
                opts,
                store,
            );
            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
            // The run is consumed along with the unit read to
            // disprove it, which is held for `#putback` (#1617).
            let through = through_bad_run(at, &kind, &meta, src_bytes.len());
            return (kind, through, out, meta);
        }
        let (result, pivot_consumed, out, meta) = stream_convert(
            pivot.as_bytes(),
            E::UTF8,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
        // Back to source bytes: two per UTF-16 code unit (so four for a
        // surrogate pair), four per UTF-32 character.
        let wide = matches!(src_enc, E::Utf32Le | E::Utf32Be);
        let unit_len = |c: char| if wide { 4 } else { c.len_utf16() * 2 };
        let consumed = pivot[..pivot_consumed].chars().map(unit_len).sum();
        let mut meta = meta;
        if meta.dst_full_extra > 0 {
            let end = (pivot_consumed + meta.dst_full_extra).min(pivot.len());
            meta.dst_full_extra = pivot[pivot_consumed..end].chars().map(unit_len).sum();
        }
        return (result, consumed, out, meta);
    }
    if is_utf16_or_32(dst_enc) {
        let (result, consumed, pivot, meta) = stream_convert(
            src_bytes,
            src_enc,
            E::UTF8,
            None,
            partial_input,
            opts,
            store,
        );
        // Whatever decoded before the decode half gave up still has to
        // come out; the encode half itself cannot fail, every scalar
        // having a UTF-16 and a UTF-32 form.
        let text = String::from_utf8_lossy(&pivot);
        let mut out = Vec::with_capacity(text.len() * 2);
        for (at, c) in text.char_indices() {
            let mut buf = [0u8; 4];
            let unit = encode_utf16_32(c.encode_utf8(&mut buf), dst_enc);
            if let Some(max) = max_dst_bytes
                && out.len() + unit.len() > max
            {
                // Fill to the byte and hold the rest of this unit
                // for the next call (#1532).
                let fits = max - out.len();
                let written_through = pivot_prefix_consumed(src_bytes, src_enc, at, opts, store);
                let through_tried =
                    pivot_prefix_consumed(src_bytes, src_enc, at + c.len_utf8(), opts, store);
                out.extend_from_slice(&unit[..fits]);
                return (
                    StreamConvertResult::DestinationBufferFull,
                    written_through,
                    out,
                    ErrMeta {
                        dst_full_extra: through_tried.saturating_sub(written_through),
                        dst_full_out: unit[fits..].to_vec(),
                        ..ErrMeta::default()
                    },
                );
            }
            out.extend_from_slice(&unit);
        }
        return (result, consumed, out, meta);
    }
    // EUC-JP ↔ Shift_JIS converts cell to cell with no pivot at all,
    // so it is settled before either half below gets a say (#1460).
    if let Some(from_euc) = jis_direct_from_euc(src_enc, dst_enc) {
        let mut out: Vec<u8> = Vec::with_capacity(src_bytes.len());
        let mut at = 0;
        while at < src_bytes.len() {
            let (bytes, n, refusal) = match jis_direct_one(&src_bytes[at..], from_euc) {
                JisCell::Cell(b, n) => (b, n, None),
                JisCell::Undefined(n) if opts.undef_replace => {
                    (opts.replace_str(dst_enc).as_bytes().to_vec(), n, None)
                }
                JisCell::Invalid(n) if opts.invalid_replace => {
                    (opts.replace_str(dst_enc).as_bytes().to_vec(), n, None)
                }
                JisCell::Undefined(n) => (vec![], n, Some(false)),
                JisCell::Invalid(n) => (vec![], n, Some(true)),
            };
            if let Some(is_invalid) = refusal {
                let cell = &src_bytes[at..(at + n).min(src_bytes.len())];
                if is_invalid {
                    // An incomplete tail is the next call's to finish,
                    // exactly as it is for the pivoted paths.
                    let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
                    let through = if matches!(kind, StreamConvertResult::InvalidByteSequence) {
                        (at + meta.error_bytes.len() + meta.readagain_bytes.len())
                            .min(src_bytes.len())
                    } else {
                        at
                    };
                    return (kind, through, out, meta);
                }
                return (
                    StreamConvertResult::UndefinedConversion,
                    at + n,
                    out,
                    ErrMeta {
                        error_bytes: cell.to_vec(),
                        readagain_bytes: vec![],
                        // Named against the source encoding, since
                        // there is no pivot in between.
                        decode_stage: true,
                        ..ErrMeta::default()
                    },
                );
            }
            if let Some(max) = max_dst_bytes
                && out.len() + bytes.len() > max
            {
                // Fill to the byte and hold the rest, as every other
                // destination does (#1532).
                let fits = max - out.len();
                out.extend_from_slice(&bytes[..fits]);
                return (
                    StreamConvertResult::DestinationBufferFull,
                    at + n,
                    out,
                    ErrMeta {
                        dst_full_out: bytes[fits..].to_vec(),
                        ..ErrMeta::default()
                    },
                );
            }
            out.extend_from_slice(&bytes);
            at += n;
        }
        let result = if partial_input {
            StreamConvertResult::SourceBufferEmpty
        } else {
            StreamConvertResult::Finished
        };
        return (result, src_bytes.len(), out, ErrMeta::default());
    }
    // A source whose walk is the authority reads through that walk
    // rather than the codec. `encoding_rs` carries WHATWG's tables,
    // which for the Japanese pair disagree with CRuby's on the
    // duplicate-mapping cells and read extension rows CRuby has no
    // character for (#1461), and for `euc-kr` and `gb2312` are
    // Windows-949 and GBK — so the codec read cells those encodings
    // do not have at all, and the converter decoded characters
    // `String#encode` refused (#1558). The one-shot path has gone
    // through `cell_decode` since #1445; this is the same buffer
    // through the same walk, which hands a clean one to the codec
    // whole and so costs one walk on the common path.
    // A table encoding reads through its table for the same reason,
    // even where its walk is not what decides a malformed run: the
    // two questions are separate, and `bad_source_outcome` below
    // still answers the second one (#1544).
    // CP949 is a third reason, and neither of the first two: its walk
    // is not the authority for a malformed run and it has no
    // corrected table, but `encoding_rs`'s `euc-kr` still reads 5,380
    // cells CRuby's transcoder reports as an *undefined conversion*.
    // Without the per-cell read the codec's U+FFFD made those a
    // malformed sequence, so the converter disagreed with
    // `String#encode`, which has gone through `cell_decode`
    // unconditionally since #1445 (#1565).
    if (walk_reports_runs(src_enc)
        || cell_table(src_enc).is_some()
        || cell_decode_reads_cells(src_enc))
        && let Some(src_rs_in) = encoding_to_rs(src_enc)
    {
        let repl = opts.undef_replace.then(|| opts.replace_str(dst_enc));
        let d = cell_decode(
            src_enc,
            src_rs_in,
            jp_fixup(src_enc),
            src_bytes,
            repl.as_deref(),
        );
        // Where the decode half first has something to say: a cell the
        // buffer ended in the middle of, one that is ill-formed, or a
        // well-formed one CRuby's table has no character for.
        let decode_stop = match (
            d.had_invalid.then_some(d.invalid_at).flatten(),
            d.unmapped_at,
        ) {
            (Some(i), Some(u)) => Some(i.min(u)),
            (Some(i), None) => Some(i),
            (None, u) => u,
        }
        .filter(|at| !opts.invalid_replace || d.unmapped_at == Some(*at));
        if let Some(stop) = decode_stop {
            // Convert the part before it on its own. The encode half
            // gets first refusal: a character with no cell in the
            // destination, or a destination that fills up, happens
            // *earlier* in the stream than whatever stopped the
            // decoder, and that is what CRuby reports.
            let head = cell_decode(
                src_enc,
                src_rs_in,
                jp_fixup(src_enc),
                &src_bytes[..stop],
                repl.as_deref(),
            );
            let (res, pivot_consumed, out, meta) = stream_convert(
                head.text.as_bytes(),
                E::UTF8,
                dst_enc,
                max_dst_bytes,
                true,
                opts,
                store,
            );
            // The head is complete text by construction, so a clean
            // one answers `Finished`, or `SourceBufferEmpty` for the
            // `partial_input` this call passes — neither is the encode
            // half objecting.
            if !matches!(
                res,
                StreamConvertResult::Finished | StreamConvertResult::SourceBufferEmpty
            ) {
                let consumed = if pivot_consumed == head.text.len() {
                    stop
                } else {
                    pivot_prefix_consumed(src_bytes, src_enc, pivot_consumed, opts, store)
                };
                return (res, consumed, out, meta);
            }
            if d.unmapped_at == Some(stop) {
                // A well-formed cell with no character is an
                // *undefined* conversion reported against the source
                // encoding — the decode half is the one that gave up.
                let cell = d.unmapped.unwrap_or_default();
                return (
                    StreamConvertResult::UndefinedConversion,
                    stop + cell.len(),
                    out,
                    ErrMeta {
                        error_bytes: cell,
                        readagain_bytes: vec![],
                        decode_stage: true,
                        ..ErrMeta::default()
                    },
                );
            }
            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
            // A malformed run is consumed, along with the bytes read
            // to disprove it — those are held for `#putback` rather
            // than left in `src`. A *pending* one is not: a cell split
            // across two calls has to stay for the next one to finish.
            let through_error = if matches!(kind, StreamConvertResult::InvalidByteSequence) {
                (stop + meta.error_bytes.len() + meta.readagain_bytes.len()).min(src_bytes.len())
            } else {
                stop
            };
            return (kind, through_error, out, meta);
        }
        let (result, pivot_consumed, out, meta) = stream_convert(
            d.text.as_bytes(),
            E::UTF8,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
        // The whole pivot converting is the common case and needs no
        // mapping back; anything short of it is a capped destination or
        // an encode-half error, where the source offset is the pivot
        // prefix re-decoded.
        let consumed = if pivot_consumed == d.text.len() {
            src_bytes.len()
        } else {
            pivot_prefix_consumed(src_bytes, src_enc, pivot_consumed, opts, store)
        };
        let mut meta = meta;
        if meta.dst_full_extra > 0 {
            let end = (pivot_consumed + meta.dst_full_extra).min(d.text.len());
            meta.dst_full_extra = pivot_prefix_consumed(src_bytes, src_enc, end, opts, store)
                .saturating_sub(consumed);
        }
        return (result, consumed, out, meta);
    }
    // The same three on the way *out*. `encoding_rs`'s encoders write
    // the duplicate-mapping characters into cells that read back as
    // their twins, have no EUC-JP JIS X 0212 plane at all, and will
    // not write Windows-31J's user-defined area — all of which
    // `jp_encode` settles. `String#encode` has used it since #1445;
    // the streaming path had the raw codec (#1461).
    if let Some(enc1) = dst_encoder(dst_enc) {
        // Decode uncapped: the cap is felt in destination bytes, and
        // these encodings write one or two of them per character.
        let (result, consumed, pivot, meta) = stream_convert(
            src_bytes,
            src_enc,
            E::UTF8,
            None,
            partial_input,
            opts,
            store,
        );
        let text = String::from_utf8_lossy(&pivot);
        let mut out: Vec<u8> = Vec::with_capacity(text.len());
        for (at, c) in text.char_indices() {
            // Where this character's bytes start, so the destination
            // cap can cut back to a whole one.
            let before = out.len();
            match enc1.one(c) {
                Ok(bytes) => out.extend_from_slice(&bytes),
                Err(_) if opts.undef_replace => {
                    for r in opts.replace_str(dst_enc).chars() {
                        out.extend_from_slice(&enc1.one(r).unwrap_or_default());
                    }
                }
                Err(bad) => {
                    // Everything up to and including the character
                    // with no cell is consumed; what follows is the
                    // caller's to retry.
                    let upto = at + c.len_utf8();
                    return (
                        StreamConvertResult::UndefinedConversion,
                        pivot_prefix_consumed(src_bytes, src_enc, upto, opts, store),
                        out,
                        ErrMeta {
                            error_bytes: bad.to_string().into_bytes(),
                            readagain_bytes: vec![],
                            ..ErrMeta::default()
                        },
                    );
                }
            }
            if let Some(max) = max_dst_bytes
                && out.len() > max
            {
                // Only the characters that fit are converted; the one
                // the cap cut off is what CRuby reads ahead and
                // buffers the output of (#1511). It fills the
                // destination to the byte, so the cut is at `max` and
                // the rest of this character rides the next call
                // (#1532) — `before` is where it starts.
                let leftover = out[max.max(before)..].to_vec();
                out.truncate(max);
                let written_through = pivot_prefix_consumed(src_bytes, src_enc, at, opts, store);
                let through_tried =
                    pivot_prefix_consumed(src_bytes, src_enc, at + c.len_utf8(), opts, store);
                return (
                    StreamConvertResult::DestinationBufferFull,
                    written_through,
                    out,
                    ErrMeta {
                        dst_full_extra: through_tried.saturating_sub(written_through),
                        dst_full_out: leftover,
                        ..ErrMeta::default()
                    },
                );
            }
        }
        // The pivot encoded cleanly, so the decode half's verdict — a
        // clean finish or the error it stopped on — is the answer.
        return (result, consumed, out, meta);
    }
    // A single-byte table encoding on either side: `encoding_rs` has
    // no codec for these, or resolves their label to a Windows code
    // page that is not the same encoding (#1508). They are stateless
    // and one byte per character, so the pivot is built here and the
    // rest of the pair goes through the ordinary path.
    if let Some(table) = source_byte_table(src_enc) {
        let pivot = if opts.undef_replace {
            table_decode_lossy(src_bytes, table, &opts.replace_str(dst_enc))
        } else {
            match table_decode(src_bytes, table) {
                Ok(p) => p,
                Err((at, b)) => {
                    // Everything up to and including the unassigned
                    // byte is consumed — one byte is one character
                    // here — and what converted before it still comes
                    // out, as CRuby's incremental transcoder has it.
                    let (_, _, out, _) = stream_convert(
                        &src_bytes[..at],
                        src_enc,
                        dst_enc,
                        max_dst_bytes,
                        false,
                        opts,
                        store,
                    );
                    return (
                        StreamConvertResult::UndefinedConversion,
                        at + 1,
                        out,
                        ErrMeta {
                            error_bytes: vec![b],
                            readagain_bytes: vec![],
                            // The byte has no Unicode meaning at all:
                            // the *decode* half is what gave up, so
                            // the error is reported against the
                            // source encoding, not the pivot (#1511).
                            decode_stage: true,
                            ..ErrMeta::default()
                        },
                    );
                }
            }
        };
        let (result, pivot_consumed, out, meta) = stream_convert(
            pivot.as_bytes(),
            E::UTF8,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
            store,
        );
        // One source byte per pivot character, so the count converts
        // back by counting characters rather than bytes.
        let consumed = pivot[..pivot_consumed].chars().count();
        let mut meta = meta;
        if meta.dst_full_extra > 0 {
            let end = (pivot_consumed + meta.dst_full_extra).min(pivot.len());
            meta.dst_full_extra = pivot[pivot_consumed..end].chars().count();
        }
        return (result, consumed, out, meta);
    }
    if let Some(table) = single_byte_table(dst_enc) {
        // Decode with the ordinary path, then map the pivot through the
        // table: one destination byte per character, so a destination
        // cap falls on a character boundary by construction.
        let (result, consumed, pivot, meta) = stream_convert(
            src_bytes,
            src_enc,
            E::UTF8,
            None,
            partial_input,
            opts,
            store,
        );
        // Whatever decoded before the decode half gave up still has to
        // come out: `#primitive_convert` appends it to the destination
        // and *then* reports the error.
        let text = String::from_utf8_lossy(&pivot);
        let mut out = Vec::with_capacity(text.len());
        for (at, c) in text.char_indices() {
            let mut buf = [0u8; 4];
            match table_encode(c.encode_utf8(&mut buf), table) {
                Ok(bytes) => out.extend_from_slice(&bytes),
                Err(_) if opts.undef_replace => {
                    let repl = opts.replace_str(dst_enc);
                    match table_encode(&repl, table) {
                        Ok(bytes) => out.extend_from_slice(&bytes),
                        Err(_) => out.push(b'?'),
                    }
                }
                Err(c) => {
                    // Everything up to and including the offending
                    // character is consumed — CRuby leaves only what
                    // follows it in `src`. Re-decoding with the pivot
                    // capped there is how many source bytes that is,
                    // which the ordinary path already counts for a
                    // capped destination.
                    let upto = at + c.len_utf8();
                    return (
                        StreamConvertResult::UndefinedConversion,
                        pivot_prefix_consumed(src_bytes, src_enc, upto, opts, store),
                        out,
                        ErrMeta {
                            error_bytes: c.to_string().into_bytes(),
                            readagain_bytes: vec![],
                            ..ErrMeta::default()
                        },
                    );
                }
            }
            if let Some(max) = max_dst_bytes
                && out.len() > max
            {
                // `consumed` is the *decode* half's count — the whole
                // source, since the pivot was built uncapped. Only the
                // characters that fit are converted; the one the cap
                // cut off is what CRuby reads ahead and buffers
                // (#1511). One byte per character here, so filling to
                // the byte (#1532) never splits one.
                let leftover = out[max..].to_vec();
                out.truncate(max);
                let written_through = pivot_prefix_consumed(src_bytes, src_enc, at, opts, store);
                let through_tried =
                    pivot_prefix_consumed(src_bytes, src_enc, at + c.len_utf8(), opts, store);
                return (
                    StreamConvertResult::DestinationBufferFull,
                    written_through,
                    out,
                    ErrMeta {
                        dst_full_extra: through_tried.saturating_sub(written_through),
                        dst_full_out: leftover,
                        ..ErrMeta::default()
                    },
                );
            }
        }
        // The pivot mapped cleanly, so the decode half's verdict — a
        // clean finish or the error it stopped on — is the answer.
        return (result, consumed, out, meta);
    }
    // US-ASCII / ASCII-8BIT destination: `encoding_rs` has no
    // encoder for these. Decode the source to UTF-8 *without*
    // replacement (so a malformed source byte stays an error
    // rather than a U+FFFD that would masquerade as an undefined
    // conversion), then every character must be ASCII. A non-ASCII
    // char is `:undefined_conversion` (or the configured
    // replacement under `undef: :replace`); a malformed source
    // byte is `:invalid_byte_sequence` (or the replacement under
    // `invalid: :replace`). The decoder yields the valid prefix
    // *before* each malformed run, so a non-ASCII char that occurs
    // positionally before a later bad byte naturally wins —
    // matching CRuby's incremental ordering.
    if encoding_to_rs(dst_enc).is_none() && matches!(dst_enc, E::UsAscii | E::Ascii8) {
        let repl = opts.replace_str(dst_enc);
        let mut out: Vec<u8> = Vec::with_capacity(src_bytes.len());
        // How far into the UTF-8 pivot the characters written so far
        // reach — a destination cap has to be reported in *source*
        // bytes, and that is the offset to translate back (#1511).
        let mut pivot_at = 0usize;
        // Closure-free helper macro: append `ch`, honouring undef
        // replacement and the destination cap.
        macro_rules! push_char {
            ($ch:expr) => {{
                let ch = $ch;
                if ch.is_ascii() {
                    out.push(ch as u8);
                } else if opts.undef_replace {
                    out.extend_from_slice(repl.as_bytes());
                } else {
                    // Everything after the character that has no
                    // ASCII form stays in `src` for the next call —
                    // consuming the whole source would silently drop
                    // it (#1511).
                    return (
                        StreamConvertResult::UndefinedConversion,
                        pivot_prefix_consumed(
                            src_bytes,
                            src_enc,
                            pivot_at + ch.len_utf8(),
                            opts,
                            store,
                        ),
                        out,
                        ErrMeta {
                            error_bytes: ch.to_string().into_bytes(),
                            readagain_bytes: vec![],
                            ..ErrMeta::default()
                        },
                    );
                }
                if let Some(max) = max_dst_bytes
                    && out.len() > max
                {
                    // Only what fit is converted; the character the
                    // cap cut off is the one CRuby reads ahead and
                    // buffers the output of.
                    out.truncate(max);
                    let written_through =
                        pivot_prefix_consumed(src_bytes, src_enc, pivot_at, opts, store);
                    let through_tried = pivot_prefix_consumed(
                        src_bytes,
                        src_enc,
                        pivot_at + ch.len_utf8(),
                        opts,
                        store,
                    );
                    return (
                        StreamConvertResult::DestinationBufferFull,
                        written_through,
                        out,
                        ErrMeta {
                            dst_full_extra: through_tried.saturating_sub(written_through),
                            ..ErrMeta::default()
                        },
                    );
                }
                pivot_at += ch.len_utf8();
            }};
        }
        if let Some(src_rs) = encoding_to_rs(src_enc) {
            use encoding_rs::DecoderResult;
            let mut decoder = src_rs.new_decoder_without_bom_handling();
            let last = !partial_input;
            let mut rest = src_bytes;
            loop {
                let mut buf = vec![0u8; rest.len() + 16];
                let (res, read, written) =
                    decoder.decode_to_utf8_without_replacement(rest, &mut buf, last);
                for ch in std::str::from_utf8(&buf[..written]).unwrap_or("").chars() {
                    push_char!(ch);
                }
                match res {
                    DecoderResult::InputEmpty => {
                        // A chunk that ends inside a character keeps
                        // that tail for the next one, as every other
                        // destination does: it comes back unconsumed
                        // and the converter holds it (#1592).
                        if !last
                            && let Some((tail, _, true)) = first_bad_sequence(src_enc, src_bytes)
                        {
                            return (
                                StreamConvertResult::SourceBufferEmpty,
                                src_bytes.len() - tail.len().min(src_bytes.len()),
                                out,
                                ErrMeta::default(),
                            );
                        }
                        break;
                    }
                    DecoderResult::Malformed(..) => {
                        if !opts.invalid_replace {
                            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, last);
                            // Consumed through the malformed run; what
                            // follows it is the caller's to retry.
                            let through_error = src_bytes.len() - rest.len() + read;
                            return (kind, through_error, out, meta);
                        }
                        out.extend_from_slice(repl.as_bytes());
                        rest = &rest[read..];
                    }
                    DecoderResult::OutputFull => {
                        rest = &rest[read..];
                    }
                }
            }
        } else {
            match std::str::from_utf8(src_bytes) {
                Ok(s) => {
                    for ch in s.chars() {
                        push_char!(ch);
                    }
                }
                // The same tail, held for the next chunk (#1592).
                Err(e) if partial_input && e.error_len().is_none() => {
                    for ch in std::str::from_utf8(&src_bytes[..e.valid_up_to()])
                        .unwrap_or("")
                        .chars()
                    {
                        push_char!(ch);
                    }
                    return (
                        StreamConvertResult::SourceBufferEmpty,
                        e.valid_up_to(),
                        out,
                        ErrMeta::default(),
                    );
                }
                Err(_) if opts.invalid_replace => {
                    for ch in String::from_utf8_lossy(src_bytes).chars() {
                        push_char!(ch);
                    }
                }
                Err(e) => {
                    let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
                    let through_error = e.valid_up_to()
                        + e.error_len().unwrap_or(src_bytes.len() - e.valid_up_to());
                    return (kind, through_error, out, meta);
                }
            }
        }
        return (
            StreamConvertResult::Finished,
            src_bytes.len(),
            out,
            ErrMeta::default(),
        );
    }
    // Resolve the encoding_rs encoders. The Converter constructor
    // already validated this pair, so the lookups should succeed —
    // bail with `Finished` for any unsupported pair to be safe.
    let (Some(src_rs), Some(dst_rs)) = (encoding_to_rs(src_enc), encoding_to_rs(dst_enc)) else {
        // BINARY → ascii-compat with non-ASCII bytes → undefined
        // (matches `transcode_bytes_with_opts`'s behaviour).
        if src_enc == E::Ascii8 && dst_enc.is_ascii_compatible() && !all_ascii {
            return (
                StreamConvertResult::UndefinedConversion,
                1,
                Vec::new(),
                ErrMeta::default(),
            );
        }
        // Anything else falls through as a no-op pass.
        return (
            StreamConvertResult::Finished,
            src_bytes.len(),
            src_bytes.to_vec(),
            ErrMeta::default(),
        );
    };

    use encoding_rs::DecoderResult;
    use encoding_rs::EncoderResult;

    let mut decoder = src_rs.new_decoder_without_bom_handling();
    let mut encoder = dst_rs.new_encoder();

    // UTF-8 intermediate buffer. encoding_rs's
    // `decode_to_utf8_without_replacement` returns
    // `DecoderResult::Malformed` on bad input rather than substituting
    // a replacement char (which is what we want for the symbol-based
    // outcome to be useful).
    let mut utf8_buf = vec![0u8; src_bytes.len() + 16];
    let last = !partial_input;
    let (decode_result, src_read, utf8_written) =
        decoder.decode_to_utf8_without_replacement(src_bytes, &mut utf8_buf, last);

    // Encode whatever UTF-8 we got so far into the destination.
    // When `max_dst_bytes` is `Some`, size the buffer to exactly
    // that number of bytes — encoding_rs's encoder respects the
    // buffer length and returns `EncoderResult::OutputFull` when
    // it can't fit the next codepoint, which is what triggers
    // `:destination_buffer_full`. When unbounded, allocate
    // generously so the encoder runs to completion in one go.
    let max_out = max_dst_bytes.unwrap_or(utf8_written * 4 + 16);
    let mut out_buf = vec![0u8; max_out];
    let utf8_str = std::str::from_utf8(&utf8_buf[..utf8_written]).unwrap_or("");
    // encoding_rs's encoders stop short of a destination they could
    // still partly fill — an EUC-JP encoder with one byte free stops
    // rather than write the ASCII character that would fit. Feeding
    // the remainder back in until it stops making progress is what
    // makes an exactly-sized destination `:finished` rather than
    // `:destination_buffer_full` (#1511).
    let mut utf8_read = 0usize;
    let mut out_written = 0usize;
    // `undef: :replace` is settled here rather than in the arms
    // below: the encoder has to carry on past the character it has no
    // cell for, and there may be several. The branches that map the
    // pivot themselves substitute character by character; this one
    // writes the replacement through the same encoder and re-enters
    // (#1542).
    let undef_repl = opts.undef_replace.then(|| opts.replace_str(dst_enc));
    let mut encode_result = loop {
        let (res, read, written) = encoder.encode_from_utf8_without_replacement(
            &utf8_str[utf8_read..],
            &mut out_buf[out_written..],
            last,
        );
        utf8_read += read;
        out_written += written;
        if matches!(res, EncoderResult::OutputFull) && written > 0 && out_written < max_out {
            continue;
        }
        // A destination too full to hold the next character hides
        // whether it has a cell for it at all. With a replacement to
        // fall back on, ask and write that instead — it is usually
        // shorter, so it may fit where the character did not.
        if matches!(res, EncoderResult::OutputFull)
            && let Some(repl) = &undef_repl
            && let Some(c) = utf8_str[utf8_read..].chars().next()
            && dst_rs_unmappable(dst_rs, dst_enc, c)
        {
            let (repl_res, _, repl_written) = encoder.encode_from_utf8_without_replacement(
                repl,
                &mut out_buf[out_written..],
                false,
            );
            if matches!(repl_res, EncoderResult::OutputFull) {
                break EncoderResult::OutputFull;
            }
            out_written += repl_written;
            utf8_read += c.len_utf8();
            continue;
        }
        // `encode_from_utf8_without_replacement` has already consumed
        // the unmappable character, so the next round starts after it.
        if matches!(res, EncoderResult::Unmappable(_))
            && let Some(repl) = &undef_repl
        {
            let (repl_res, _, repl_written) = encoder.encode_from_utf8_without_replacement(
                repl,
                &mut out_buf[out_written..],
                false,
            );
            out_written += repl_written;
            // No room for the replacement is a full destination, and
            // the character it stands for has been read.
            if matches!(repl_res, EncoderResult::OutputFull) {
                break EncoderResult::OutputFull;
            }
            continue;
        }
        break res;
    };
    out_buf.truncate(out_written);

    // The codec is wider than the destination for `euc-kr`
    // (Windows-949) and `gb2312` (GBK), so it can write cells the
    // destination itself calls invalid — bytes monoruby would then
    // refuse to read back. CRuby never writes what it cannot read, in
    // any encoding, so such a cell counts as no cell at all (#1544).
    // Checking costs one walk of the output; only an output that
    // fails it is redone character by character.
    if !dst_can_hold(dst_enc, &out_buf) {
        let mut out: Vec<u8> = Vec::with_capacity(out_buf.len());
        for (at, c) in utf8_str.char_indices() {
            let bytes = match dst_rs_encode_one(dst_rs, dst_enc, c) {
                Some(b) => b,
                None => match &undef_repl {
                    Some(repl) => dst_rs.encode(repl).0.into_owned(),
                    None => {
                        let through =
                            src_offset_for_utf8_prefix(src_rs, src_bytes, at + c.len_utf8())
                                .unwrap_or(src_read);
                        return (
                            StreamConvertResult::UndefinedConversion,
                            through,
                            out,
                            ErrMeta {
                                error_bytes: c.to_string().into_bytes(),
                                readagain_bytes: vec![],
                                ..ErrMeta::default()
                            },
                        );
                    }
                },
            };
            if let Some(max) = max_dst_bytes
                && out.len() + bytes.len() > max
            {
                // Fill to the byte and hold the rest, as every other
                // destination does (#1532).
                let fits = max - out.len();
                let written_through =
                    src_offset_for_utf8_prefix(src_rs, src_bytes, at).unwrap_or(0);
                let through_tried =
                    src_offset_for_utf8_prefix(src_rs, src_bytes, at + c.len_utf8())
                        .unwrap_or(written_through);
                out.extend_from_slice(&bytes[..fits]);
                return (
                    StreamConvertResult::DestinationBufferFull,
                    through_tried,
                    out,
                    ErrMeta {
                        dst_full_out: bytes[fits..].to_vec(),
                        ..ErrMeta::default()
                    },
                );
            }
            out.extend_from_slice(&bytes);
        }
        // The whole pivot encoded after all, so the decode half's
        // verdict is the answer.
        out_buf = out;
        encode_result = EncoderResult::InputEmpty;
    }

    match (decode_result, encode_result) {
        // Unmappable codepoint on the encode side wins regardless of
        // decoder state — that's the next thing the caller would hit.
        // We compute the *src* offset that corresponds to "the
        // unmappable codepoint and everything before it" so the
        // caller can leave the bytes after that error in `src` for
        // the next call (matching CRuby's behaviour). For UTF-8 src
        // this is identity (`utf8_read + unmappable.len_utf8()`);
        // for other source encodings the decoder consumes a
        // variable number of bytes per codepoint, so we re-feed
        // src incrementally to a probe decoder until it produces
        // exactly the matching utf8 prefix.
        (_, EncoderResult::Unmappable(c)) => {
            // encoding_rs's `utf8_read` already advances *past* the
            // unmappable codepoint (the encoder consumes the bytes
            // and then reports the char it couldn't map). So
            // `utf8_read` itself is the offset of the first UTF-8
            // byte the user-visible "leftover" should start at.
            let src_through_error = src_offset_for_utf8_prefix(src_rs, src_bytes, utf8_read);
            (
                StreamConvertResult::UndefinedConversion,
                src_through_error.unwrap_or(src_read),
                out_buf,
                ErrMeta {
                    // The erroneous bytes are reported in the stage
                    // source encoding — the UTF-8 pivot.
                    error_bytes: c.to_string().into_bytes(),
                    readagain_bytes: vec![],
                    ..ErrMeta::default()
                },
            )
        }
        // Output buffer hit its cap. Caller should grow it and call
        // again on the remaining `src`.
        (_, EncoderResult::OutputFull) => {
            // Only the source the encoder actually got through is
            // converted — the decoder may have read all of `src`
            // while the encoder stopped part-way through the pivot,
            // so `src_read` would lose everything after the cap
            // (#1511). `utf8_read` is the pivot prefix the encoder
            // wrote; map it back to source bytes.
            let written_through =
                src_offset_for_utf8_prefix(src_rs, src_bytes, utf8_read).unwrap_or(src_read);
            // CRuby reads one character further than it writes — the
            // one it tried to write, whose output it buffers — so
            // `"あabcd"` capped at 2 leaves `"bcd"` in `src`, not
            // `"abcd"`.
            let tried = utf8_str[utf8_read..].chars().next();
            let through_tried = tried
                .and_then(|c| {
                    src_offset_for_utf8_prefix(src_rs, src_bytes, utf8_read + c.len_utf8())
                })
                .unwrap_or(written_through);
            // A full destination is not why the encoder stopped if the
            // character it stopped at has no cell there at all: CRuby
            // reports that undefined conversion in this same call, not
            // in the next one (#1533). The branches that map the pivot
            // themselves already decide it in this order; only the
            // codec pair had to be asked, which is one character
            // through a throwaway encoder on the cap path alone.
            if !opts.undef_replace
                && let Some(c) = tried
                && dst_rs_unmappable(dst_rs, dst_enc, c)
            {
                return (
                    StreamConvertResult::UndefinedConversion,
                    through_tried,
                    out_buf,
                    ErrMeta {
                        error_bytes: c.to_string().into_bytes(),
                        readagain_bytes: vec![],
                        ..ErrMeta::default()
                    },
                );
            }
            // `encoding_rs` will not write a character it cannot fit
            // whole, and wants headroom besides — a two-byte character
            // and a two-byte destination leave it writing *nothing*,
            // so the cap path made no progress at all and a caller
            // looping to `:finished` never got there. Fill the
            // destination here instead, character by character through
            // a throwaway encoder, to exactly the byte CRuby fills it
            // to; then read one character further, as CRuby does, and
            // hold its output for the next call (#1532).
            let mut out_buf = out_buf;
            let mut leftover: Vec<u8> = vec![];
            let mut at = utf8_read;
            if let Some(max) = max_dst_bytes {
                while out_buf.len() < max {
                    let Some(c) = utf8_str[at..].chars().next() else {
                        break;
                    };
                    let Some(bytes) = dst_rs_encode_one(dst_rs, dst_enc, c) else {
                        break;
                    };
                    let fits = (max - out_buf.len()).min(bytes.len());
                    out_buf.extend_from_slice(&bytes[..fits]);
                    at += c.len_utf8();
                    if fits < bytes.len() {
                        leftover = bytes[fits..].to_vec();
                        break;
                    }
                }
                // Nothing was split, so the character CRuby reads
                // ahead is the next one whole.
                if leftover.is_empty()
                    && let Some(c) = utf8_str[at..].chars().next()
                    && let Some(bytes) = dst_rs_encode_one(dst_rs, dst_enc, c)
                {
                    leftover = bytes;
                    at += c.len_utf8();
                }
            }
            // The whole pivot fits after all: `encoding_rs` simply
            // would not write into a buffer this tight. The cap was
            // never binding, so this is an ordinary finish — convert
            // again without one and let the decode half answer.
            if leftover.is_empty() && at == utf8_str.len() && max_dst_bytes.is_some() {
                return stream_convert(
                    src_bytes,
                    src_enc,
                    dst_enc,
                    None,
                    partial_input,
                    opts,
                    store,
                );
            }
            // Everything up to `at` is converted — written or held —
            // so it is consumed outright and `dst_full_extra`, which
            // exists to re-convert what was only read, has nothing to
            // say.
            let consumed = if at == utf8_read {
                written_through
            } else {
                src_offset_for_utf8_prefix(src_rs, src_bytes, at).unwrap_or(through_tried)
            };
            (
                StreamConvertResult::DestinationBufferFull,
                consumed,
                out_buf,
                ErrMeta {
                    dst_full_extra: through_tried.saturating_sub(consumed.max(written_through)),
                    dst_full_out: leftover,
                    ..ErrMeta::default()
                },
            )
        }
        // Decoder hit an invalid sequence in the middle of the input.
        // `bad_len` is the length of the malformed run; the decoder
        // already advanced past it as part of `src_read`.
        (DecoderResult::Malformed(mal_len, extra), EncoderResult::InputEmpty) => {
            // `Malformed(m, e)`: the erroneous run is the `m` bytes
            // ending `e` bytes before the decoder's read position; the
            // trailing `e` bytes were consumed to detect the error and
            // are CRuby's read-again bytes.
            let mal_len = mal_len as usize;
            let extra = extra as usize;
            let err_start = src_read.saturating_sub(extra + mal_len);
            let mut meta = ErrMeta {
                error_bytes: src_bytes[err_start..src_read - extra].to_vec(),
                readagain_bytes: src_bytes[src_read - extra..src_read].to_vec(),
                ..ErrMeta::default()
            };
            // encoding_rs sometimes folds the disproving byte into the
            // malformed run itself (EUC-JP reports `\xA1\xFF` as one
            // 2-byte run). CRuby splits such a run at the longest
            // prefix that is still a *pending* sequence — the bytes it
            // was waiting on become the error, the byte that disproved
            // them becomes read-again.
            // UTF-16/32 are read a coding unit at a time, so a split
            // may only fall on a unit boundary.
            let unit = if src_rs.name().starts_with("UTF-16") {
                2
            } else {
                1
            };
            let mut consumed = src_read;
            // Distinguish "incomplete tail" from "junk in middle":
            // if there are bytes after the malformed run, it's junk;
            // otherwise the partial_input flag picks between
            // incomplete (`last=true`) and source_buffer_empty
            // (`last=false`, but we asked for last=true so this branch
            // means truly invalid-at-end).
            let kind = if src_read < src_bytes.len() {
                StreamConvertResult::InvalidByteSequence
            } else if !probe_incomplete_tail(src_rs, src_bytes, src_read) {
                // A byte that can never start a character is wrong now,
                // not merely unfinished: promising more input does not
                // rescue a lone `\xFF` on UTF-8, so `#convert` raises
                // instead of buffering it.
                StreamConvertResult::InvalidByteSequence
            } else if !last {
                StreamConvertResult::SourceBufferEmpty
            } else {
                StreamConvertResult::IncompleteInput
            };
            // Split the run into the prefix that failed and the unit
            // that disproved it — but only when something *did*
            // disprove it. An incomplete tail has nothing after it, so
            // CRuby reports the whole run as `error_bytes` with no
            // read-again (`incomplete "\xE3\x81" on UTF-8`, not
            // `"\xE3" followed by "\x81"`).
            if matches!(kind, StreamConvertResult::InvalidByteSequence)
                && meta.readagain_bytes.is_empty()
                && meta.error_bytes.len() > unit
            {
                let run = std::mem::take(&mut meta.error_bytes);
                let pending_prefix = |k: usize| {
                    let mut probe = src_rs.new_decoder_without_bom_handling();
                    let mut probe_dst = vec![0u8; run.len() * 4 + 16];
                    let (r, read, written) =
                        probe.decode_to_utf8_without_replacement(&run[..k], &mut probe_dst, false);
                    // Still waiting for more input, having produced
                    // nothing: a genuine incomplete prefix.
                    matches!(r, DecoderResult::InputEmpty) && read == k && written == 0
                };
                // A run that is *whole* still a pending prefix —
                // `\xE3\x81` before an `x` — was disproved by what
                // follows it, not by anything inside: it is reported
                // whole, with the disproving unit pulled in below
                // (#1592).
                let pending_len = if pending_prefix(run.len()) {
                    None
                } else {
                    (unit..run.len())
                        .rev()
                        .step_by(unit)
                        .find(|k| pending_prefix(*k))
                };
                match pending_len {
                    Some(k) => {
                        meta.readagain_bytes = run[k..].to_vec();
                        meta.error_bytes = run[..k].to_vec();
                    }
                    None => meta.error_bytes = run,
                }
            }
            // encoding_rs stops *before* the byte(s) that disproved
            // the sequence; CRuby reads one more coding unit and
            // reports it as the read-again bytes (`"\xF1" followed by
            // "a" on UTF-8`). Pull that unit in ourselves — but only
            // when the malformed run is a plausible prefix of a longer
            // sequence (a fresh decoder fed just those bytes is still
            // waiting for more). An inherently invalid byte like a
            // lone `\x80` on UTF-8 needs no disproving byte, and CRuby
            // leaves the following bytes in src with empty read-again.
            let is_plausible_prefix = || {
                let mut probe = src_rs.new_decoder_without_bom_handling();
                let mut probe_dst = vec![0u8; meta.error_bytes.len() * 4 + 16];
                let (r, read, _) = probe.decode_to_utf8_without_replacement(
                    &meta.error_bytes,
                    &mut probe_dst,
                    false,
                );
                matches!(r, DecoderResult::InputEmpty) && read == meta.error_bytes.len()
            };
            if matches!(kind, StreamConvertResult::InvalidByteSequence)
                && meta.readagain_bytes.is_empty()
                && consumed < src_bytes.len()
                && is_plausible_prefix()
            {
                let take = unit.min(src_bytes.len() - consumed);
                meta.readagain_bytes = src_bytes[consumed..consumed + take].to_vec();
                consumed += take;
            }
            (kind, consumed, out_buf, meta)
        }
        // Decoder finished cleanly. With `last=false` (partial_input
        // mode) report source_buffer_empty so the caller knows
        // they may feed more; otherwise it's a clean finish.
        (DecoderResult::InputEmpty, EncoderResult::InputEmpty) => {
            if !last {
                // The decoder may be holding a trailing incomplete
                // sequence in its internal state, which we do not keep
                // across calls. Re-probe with `last = true` to find
                // where that tail starts so the caller can buffer it.
                let mut consumed = src_read;
                let mut probe = src_rs.new_decoder_without_bom_handling();
                let mut probe_dst = vec![0u8; src_bytes.len() * 4 + 16];
                let (probe_res, probe_read, _) =
                    probe.decode_to_utf8_without_replacement(src_bytes, &mut probe_dst, true);
                if let DecoderResult::Malformed(m2, e2) = probe_res
                    && probe_read == src_bytes.len()
                {
                    consumed = probe_read.saturating_sub(m2 as usize + e2 as usize);
                }
                (
                    StreamConvertResult::SourceBufferEmpty,
                    consumed,
                    out_buf,
                    ErrMeta::default(),
                )
            } else {
                (
                    StreamConvertResult::Finished,
                    src_read,
                    out_buf,
                    ErrMeta::default(),
                )
            }
        }
        // Decoder ran out of room in the UTF-8 staging buffer. Our
        // staging buffer is sized to `src_bytes.len() + 16`, which is
        // enough for any encoding_rs source / dest pair, so this
        // shouldn't fire — treat it as DestinationBufferFull defensively.
        (DecoderResult::OutputFull, _) => (
            StreamConvertResult::DestinationBufferFull,
            src_read,
            out_buf,
            ErrMeta::default(),
        ),
    }
}

/// Did `src_bytes[..end]` cut off mid-multi-byte-sequence (i.e.
/// would the same bytes have decoded cleanly if we'd told the
/// decoder there's more input coming)? Used to differentiate
/// `:incomplete_input` from `:invalid_byte_sequence` when the
/// terminal decode result is `Malformed` and there's nothing
/// after it.
/// Walk a fresh decoder over `src_bytes` byte-by-byte and stop
/// the moment it has produced exactly `target_utf8_len` UTF-8
/// bytes; return the corresponding src position. Used to map
/// the encoder's "unmappable codepoint" position back into
/// source-encoding bytes so the caller can leave the post-error
/// tail in the user's `src` buffer.
///
/// Returns `None` for inputs the decoder can't satisfy (target
/// not exactly hit before src is exhausted) so the caller can
/// fall back to a coarser strategy.
fn src_offset_for_utf8_prefix(
    src_rs: &'static encoding_rs::Encoding,
    src_bytes: &[u8],
    target_utf8_len: usize,
) -> Option<usize> {
    if target_utf8_len == 0 {
        return Some(0);
    }
    let mut probe = src_rs.new_decoder_without_bom_handling();
    let mut utf8_buf = vec![0u8; target_utf8_len + 16];
    let mut utf8_so_far = 0usize;
    let mut src_pos = 0usize;
    while src_pos < src_bytes.len() && utf8_so_far < target_utf8_len {
        let chunk_end = (src_pos + 1).min(src_bytes.len());
        let (_, src_read, utf8_n) = probe.decode_to_utf8_without_replacement(
            &src_bytes[src_pos..chunk_end],
            &mut utf8_buf[utf8_so_far..],
            false,
        );
        src_pos += src_read;
        utf8_so_far += utf8_n;
        if utf8_so_far >= target_utf8_len {
            return Some(src_pos);
        }
    }
    if utf8_so_far == target_utf8_len {
        Some(src_pos)
    } else {
        None
    }
}

fn probe_incomplete_tail(
    src_rs: &'static encoding_rs::Encoding,
    src_bytes: &[u8],
    end: usize,
) -> bool {
    if end == 0 {
        return false;
    }
    let mut probe = src_rs.new_decoder_without_bom_handling();
    let mut probe_dst = vec![0u8; src_bytes.len() * 4 + 16];
    // Re-feed the bytes up to and including the malformed run with
    // `last=false`. A decoder told more input may follow keeps a
    // well-formed *prefix* pending instead of rejecting it, so both
    // calls answering `InputEmpty` — the second flushing what is held —
    // means the tail is incomplete rather than wrong. The first call's
    // verdict is the one that matters: a byte that can never start a
    // character is rejected there and then, however much more input is
    // promised.
    let (fed, _, _) =
        probe.decode_to_utf8_without_replacement(&src_bytes[..end], &mut probe_dst, false);
    if !matches!(fed, encoding_rs::DecoderResult::InputEmpty) {
        return false;
    }
    let (probe_result, _, _) = probe.decode_to_utf8_without_replacement(&[], &mut probe_dst, false);
    matches!(probe_result, encoding_rs::DecoderResult::InputEmpty)
}

/// Quote a byte run CRuby-style for conversion-error messages:
/// printable ASCII stays literal, everything else becomes `\xHH`.
pub(crate) fn quote_error_bytes(bytes: &[u8]) -> String {
    let mut out = String::from("\"");
    for &b in bytes {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            0x20..=0x7e => out.push(b as char),
            // The mnemonics `String#inspect` uses, which CRuby's
            // message builders go through (#1607). `#` is not among
            // them: it is escaped only before `{`, `$` or `@`, and a
            // quoted run cannot hold two printable bytes — every
            // encoding that reaches here begins a sequence at or
            // above 0x80.
            0x07 => out.push_str("\\a"),
            0x08 => out.push_str("\\b"),
            0x09 => out.push_str("\\t"),
            0x0a => out.push_str("\\n"),
            0x0b => out.push_str("\\v"),
            0x0c => out.push_str("\\f"),
            0x0d => out.push_str("\\r"),
            0x1b => out.push_str("\\e"),
            _ => out.push_str(&format!("\\x{b:02X}")),
        }
    }
    out.push('"');
    out
}

/// The first sequence in `bytes` that `enc` cannot read, in the three
/// pieces `Encoding::InvalidByteSequenceError` exposes:
/// `(error_bytes, readagain_bytes, incomplete)`.
///
/// CRuby's transcoder reports the longest *valid prefix* it had
/// consumed as `error_bytes`, the byte(s) it read past that prefix and
/// will re-read as `readagain_bytes`, and sets `incomplete_input?` when
/// the prefix was well formed and the input simply ran out:
///
/// ```text
/// "\xFF"            on UTF-8       →  ("\xFF",     "",     false)
/// "\xED\xA0\x80"    on UTF-8       →  ("\xED",     "\xA0", false)
/// "abc\xC2"         on UTF-8       →  ("\xC2",     "",     true)
/// "\x81\x20"        on Shift_JIS   →  ("\x81",     " ",    false)
/// ```
///
/// `None` when the walk finds nothing — the caller then falls back to
/// naming no bytes, rather than inventing some.
pub(crate) fn first_bad_sequence(
    enc: crate::value::Encoding,
    bytes: &[u8],
) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    use crate::value::Encoding as E;
    match enc {
        E::UsAscii => bytes
            .iter()
            .position(|b| *b >= 0x80)
            .map(|i| (vec![bytes[i]], vec![], false)),
        // A BOM-less endianness-less dummy: the whole source is
        // ill-formed, and CRuby names its first code unit (#1576).
        _ if dummy_wide_target(enc).is_some() => {
            let unit = if dummy_wide_target(enc) == Some(E::Utf16Be) {
                2
            } else {
                4
            };
            Some(if bytes.len() < unit {
                (bytes.to_vec(), vec![], true)
            } else {
                (bytes[..unit].to_vec(), vec![], false)
            })
        }
        // Stateful: only a parse from the start knows which character
        // set is in effect, so the run comes from the parser (#1609).
        _ if jis_wrapper(enc).is_some() => match (jis_wrapper(enc).unwrap().read)(bytes, None) {
            Ok(_) => None,
            Err(stop) => Some((stop.error, stop.again, stop.incomplete)),
        },
        // Its transcoder is narrower than its walk, and the transcoder
        // is what decides a run (#1600).
        _ if enc == stateless_kddi_enc() => first_bad_via_precise(kddi_transcode_len, bytes),
        _ if stateless_iso2022jp(enc).is_some() => {
            first_bad_via_precise(crate::value::stateless_iso2022jp_transcode_len, bytes)
        }
        E::Utf16Le | E::Utf16Be => first_bad_utf16(bytes, enc == E::Utf16Be),
        E::Utf32Le | E::Utf32Be => first_bad_utf32(bytes, enc == E::Utf32Be),
        // The table encodings are total over the byte range, and BINARY
        // is a byte bucket: no sequence in either is ill-formed, and
        // neither has a decoder to ask.
        _ if single_byte_table(enc).is_some() => None,
        // An encoding whose walk agrees with CRuby's transcoder
        // answers from that walk rather than from `encoding_rs`,
        // whose idea of how far a malformed run reaches is not
        // CRuby's: a byte that can never begin a sequence took the
        // byte after it along (#1546).
        _ if walk_reports_runs(enc) => first_bad_via_walk(enc, bytes),
        _ => first_bad_via_rs(encoding_to_rs(enc)?, bytes),
    }
}

/// Whether `enc` is one of the Big5 family, whose transcoders in
/// CRuby cut their input differently from the encoding's own walk.
fn is_big5_family(enc: crate::value::Encoding) -> bool {
    matches!(enc, crate::value::Encoding::NamedByte(i)
    if matches!(
        crate::value::named_byte_const_name(i),
        "Big5" | "Big5_HKSCS" | "Big5_UAO" | "CP950" | "CP951"
    ))
}

/// The walk a *conversion* cuts `enc`'s bytes with: the encoding's own
/// (`mbc_walker`, what `#valid_encoding?` answers from) for every
/// encoding but the Big5 family, whose transcoders in CRuby read every
/// byte from `0x81` to `0xFE` as a lead where the encoding object
/// starts at `0xA1`. So `"\x81\x40"` is `valid_encoding? == false`
/// in Big5 and still a well-formed cell to the converter — one the
/// table has no character for, which is an undefined conversion, not
/// a malformed sequence — and Big5-HKSCS reads its rows below `0xA1`
/// (#1500).
pub(crate) fn conversion_walker(
    enc: crate::value::Encoding,
) -> Option<(usize, fn(&[u8], usize) -> crate::value::PreciseLen)> {
    if is_big5_family(enc) {
        return Some((2, crate::value::rvalue::big5_transcoder_len));
    }
    if enc == cp51932_enc() {
        return Some((2, crate::value::rvalue::cp51932_transcoder_len));
    }
    crate::value::mbc_walker(enc)
}

/// Whether `enc`'s walk can be asked where a malformed run reaches.
///
/// It can when the walk and the converter agree about which sequences
/// exist — which for the Big5 family means [`conversion_walker`]'s
/// walk, the transcoder's own shape. CP949 is the one where they do
/// not: its runs stay with the codec.
fn walk_reports_runs(enc: crate::value::Encoding) -> bool {
    use crate::value::Encoding as E;
    match enc {
        E::EucJp(_) | E::Sjis(_) => true,
        E::NamedByte(i) => matches!(
            crate::value::named_byte_const_name(i),
            // CESU-8's walk is the converter — the conversion out of
            // it *is* that walk — so the two cannot disagree (#1562).
            "EUC_KR"
                | "GB2312"
                | "GB12345"
                | "EUC_TW"
                | "GBK"
                | "GB18030"
                | "CESU_8"
                | "Big5"
                | "Big5_HKSCS"
                | "Big5_UAO"
                | "CP950"
                | "CP951"
        ),
        _ => false,
    }
}

/// An encoding whose cells the converter must read one at a time even
/// though neither of the other two reasons applies.
///
/// CP949 is the only one. Its walk agrees with CRuby's transcoder
/// about where a cell ends, but `encoding_rs`'s `euc-kr` reads cells
/// CRuby has no character for; taking them one at a time is what
/// tells "this cell exists and maps to nothing" (an undefined
/// conversion) from "these bytes are malformed" (#1565).
///
/// The Big5 family reads through its tables instead, cut by the
/// transcoder's own walk (`conversion_walker`).
fn cell_decode_reads_cells(enc: crate::value::Encoding) -> bool {
    matches!(enc, crate::value::Encoding::NamedByte(i)
        if crate::value::named_byte_const_name(i) == "CP949")
}

/// Where the first ill-formed sequence in *bytes* is, according to
/// the encoding's own walk.
///
/// CRuby reports a malformed run as the longest prefix that is still
/// a *pending* sequence, with the byte that disproved it read again
/// rather than swallowed — `"\xA1A"` in EUC-JP is `"\xA1"` followed
/// by `"A"`. A byte that can never begin a sequence has no pending
/// prefix at all, so the run is that byte alone and nothing is read
/// again (#1546).
fn first_bad_via_walk(
    enc: crate::value::Encoding,
    bytes: &[u8],
) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    let (_, precise) = conversion_walker(enc)?;
    first_bad_via_precise(precise, bytes)
}

/// The same, driven by a given step function rather than by the
/// encoding's own walk — which is what stateless-ISO-2022-JP needs,
/// its transcoder being narrower than its walk (#1600).
fn first_bad_via_precise(
    precise: fn(&[u8], usize) -> crate::value::PreciseLen,
    bytes: &[u8],
) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    use crate::value::PreciseLen as P;
    let mut at = 0;
    while at < bytes.len() {
        match precise(bytes, at) {
            P::Char(n) if n > 0 => at += n,
            // The buffer ended in the middle of a sequence.
            P::NeedMore => return Some((bytes[at..].to_vec(), vec![], true)),
            _ => {
                let rest = &bytes[at..];
                let mut pending = 0;
                for n in 1..rest.len() {
                    if matches!(precise(&rest[..n], 0), P::NeedMore) {
                        pending = n;
                    } else {
                        break;
                    }
                }
                return Some(if pending == 0 {
                    (vec![rest[0]], vec![], false)
                } else {
                    (rest[..pending].to_vec(), vec![rest[pending]], false)
                });
            }
        }
    }
    None
}

/// Whether *bytes* are something `enc` can actually hold — every
/// sequence in them one its own walk accepts.
///
/// `encoding_rs`'s tables are wider than CRuby's for two of these:
/// its `euc-kr` is Windows-949 and its `gb2312` is GBK, so the codec
/// writes cells the destination itself calls invalid, and monoruby
/// wrote bytes it would then refuse to read back. CRuby never does
/// that in any encoding — measured over every BMP scalar in twelve
/// CJK encodings — so the codec's answer is taken only when the
/// destination can hold it (#1544).
pub(crate) fn dst_can_hold(enc: crate::value::Encoding, bytes: &[u8]) -> bool {
    // `bin/gen-cjk-tables` wants the codec's own answer, corrected by
    // neither this rule nor the tables it is generating.
    if cfg!(feature = "no-cjk-tables") {
        return true;
    }
    let Some((_, precise)) = conversion_walker(enc) else {
        return true;
    };
    let mut at = 0;
    while at < bytes.len() {
        match precise(bytes, at) {
            crate::value::PreciseLen::Char(n) => at += n,
            _ => return false,
        }
    }
    true
}

/// What `dst_rs` writes for *c*, or `None` if it has no cell for it.
/// Asked with a throwaway encoder, for the character a full
/// destination stopped the real one from writing (#1532).
fn dst_rs_encode_one(
    dst_rs: &'static encoding_rs::Encoding,
    dst_enc: crate::value::Encoding,
    c: char,
) -> Option<Vec<u8>> {
    let mut probe = dst_rs.new_encoder();
    let mut src = [0u8; 4];
    let mut out = [0u8; 16];
    let (res, _, written) =
        probe.encode_from_utf8_without_replacement(c.encode_utf8(&mut src), &mut out, false);
    match res {
        // A cell the destination cannot hold is no cell at all
        // (#1544), so the paths that ask what a character writes —
        // the cap fill and the replacement — refuse it with the ones
        // the codec has no mapping for.
        encoding_rs::EncoderResult::InputEmpty if dst_can_hold(dst_enc, &out[..written]) => {
            Some(out[..written].to_vec())
        }
        _ => None,
    }
}

/// Whether `dst_rs` has no cell for *c* — asked with a throwaway
/// encoder, since a destination that is also full hides the answer
/// (#1533).
fn dst_rs_unmappable(
    dst_rs: &'static encoding_rs::Encoding,
    dst_enc: crate::value::Encoding,
    c: char,
) -> bool {
    dst_rs_encode_one(dst_rs, dst_enc, c).is_none()
}

/// [`first_bad_sequence`] for the encodings `encoding_rs` decodes.
///
/// `DecoderResult::Malformed` gives the offending run directly, but not
/// CRuby's distinction between a *valid prefix* that ran out of input
/// (incomplete) or was broken by the next byte (which is re-read), and
/// a run that could never have started a character. Feeding the run
/// back as a non-final chunk settles it: a decoder told more input may
/// follow keeps a valid prefix pending instead of rejecting it.
fn first_bad_via_rs(
    rs: &'static encoding_rs::Encoding,
    bytes: &[u8],
) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    let (start, err_len) = rs_first_malformed(rs, bytes, true)?;
    // The run `encoding_rs` reports is not always CRuby's split: for a
    // DBCS lead with a bad trail it covers both bytes where CRuby keeps
    // the lead as the prefix, and for GB18030's four-byte form it stops
    // one byte short of it. Grow the prefix a byte at a time instead and
    // let the decoder say where it stops being one. Everything before
    // `start` decoded cleanly, so scanning from there needs no state.
    let tail = &bytes[start..];
    let mut prefix = 0;
    while prefix < tail.len() && rs_first_malformed(rs, &tail[..prefix + 1], false).is_none() {
        prefix += 1;
    }
    if prefix == 0 {
        // Not a prefix of anything: the run is the whole error.
        return Some((tail[..err_len].to_vec(), vec![], false));
    }
    match tail.get(prefix) {
        // CRuby re-reads exactly the one byte that broke the prefix.
        Some(b) => Some((tail[..prefix].to_vec(), vec![*b], false)),
        None => Some((tail.to_vec(), vec![], true)),
    }
}

/// The offset and length of the first `Malformed` run the decoder
/// reports, or `None` if the whole input decodes.
fn rs_first_malformed(
    rs: &'static encoding_rs::Encoding,
    bytes: &[u8],
    last: bool,
) -> Option<(usize, usize)> {
    let mut decoder = rs.new_decoder_without_bom_handling();
    let mut sink = [0u8; 1024];
    let mut pos = 0usize;
    loop {
        let (result, read, _) =
            decoder.decode_to_utf8_without_replacement(&bytes[pos..], &mut sink, last);
        match result {
            encoding_rs::DecoderResult::InputEmpty => return None,
            encoding_rs::DecoderResult::OutputFull => {
                // The sink filled up before the input ran out; keep going
                // from where the decoder stopped reading.
                if read == 0 {
                    return None;
                }
                pos += read;
            }
            encoding_rs::DecoderResult::Malformed(err_len, unread) => {
                // `unread` counts bytes consumed past the run that the
                // decoder will hand back; the run itself ends before them.
                let end = (pos + read).checked_sub(unread as usize)?;
                let err_len = err_len as usize;
                return Some((end.checked_sub(err_len)?, err_len));
            }
        }
    }
}

/// [`first_bad_sequence`] for UTF-16: a trailing odd byte and a high
/// surrogate at the very end are *incomplete*, a high surrogate that is
/// not followed by a low one re-reads as much of the next unit as the
/// byte order made it read, and a lone low surrogate is simply invalid.
fn first_bad_utf16(bytes: &[u8], be: bool) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    let unit = |i: usize| {
        let (a, b) = (bytes[i], bytes[i + 1]);
        if be {
            u16::from_be_bytes([a, b])
        } else {
            u16::from_le_bytes([a, b])
        }
    };
    let mut i = 0;
    while i + 2 <= bytes.len() {
        let u = unit(i);
        if (0xD800..=0xDBFF).contains(&u) {
            if i + 4 <= bytes.len() && (0xDC00..=0xDFFF).contains(&unit(i + 2)) {
                i += 4;
                continue;
            }
            // A high surrogate the input does not complete. How much
            // of what follows CRuby re-reads depends on the byte order,
            // because that is how much of the next unit it had to read
            // to learn it was not a low surrogate: big-endian, the
            // *first* byte decides, so one byte is re-read; little-
            // endian, the *second* one does, so the whole unit is. A
            // little-endian high surrogate with a single byte after it
            // is therefore not a completed error at all — it is a
            // three-byte prefix still waiting for its fourth byte.
            //
            // Big-endian, that first byte decides only when it rules a
            // low surrogate out: `D8 3D DE` is a pair three quarters
            // written, not a malformed one, and a stream fed by the
            // byte is exactly how it arrives (#1576).
            let err = bytes[i..i + 2].to_vec();
            return Some(if be {
                match bytes.get(i + 2) {
                    Some(b) if (0xDC..=0xDF).contains(b) => (bytes[i..].to_vec(), vec![], true),
                    Some(b) => (err, vec![*b], false),
                    None => (err, vec![], true),
                }
            } else if i + 4 <= bytes.len() {
                (err, bytes[i + 2..i + 4].to_vec(), false)
            } else if i + 3 == bytes.len() {
                (bytes[i..].to_vec(), vec![], true)
            } else {
                (err, vec![], true)
            });
        }
        if (0xDC00..=0xDFFF).contains(&u) {
            return Some((bytes[i..i + 2].to_vec(), vec![], false));
        }
        i += 2;
    }
    // A single trailing byte: incomplete unless no second byte could
    // complete it — a big-endian 0xDC..0xDF is a *low* surrogate, which
    // nothing that follows can rescue. Little-endian, the stray byte is
    // the low half and the high half is still free, so it always can be.
    (i < bytes.len()).then(|| {
        let ok = !be || !(0xDC..=0xDF).contains(&bytes[i]);
        (bytes[i..].to_vec(), vec![], ok)
    })
}

/// [`first_bad_sequence`] for UTF-32: a surrogate or an out-of-range
/// scalar is invalid, and a short trailing group is *incomplete* only
/// when some completion of it would still be a scalar — CRuby calls
/// `"\x00\x10"` on UTF-32BE incomplete but `"\x00\x11"` invalid,
/// since every codepoint above U+10FFFF is out of range already.
fn first_bad_utf32(bytes: &[u8], be: bool) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    let mut i = 0;
    while i + 4 <= bytes.len() {
        let g = [bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]];
        let cp = if be {
            u32::from_be_bytes(g)
        } else {
            u32::from_le_bytes(g)
        };
        if char::from_u32(cp).is_none() {
            return Some((g.to_vec(), vec![], false));
        }
        i += 4;
    }
    (i < bytes.len()).then(|| {
        let rest = &bytes[i..];
        (rest.to_vec(), vec![], utf32_prefix_completable(rest, be))
    })
}

/// Whether the 1..3 bytes of a truncated UTF-32 group could still be
/// the start of a scalar. The free bytes are the *low* ones big-endian
/// and the *high* ones little-endian, so the two orders constrain
/// completely differently.
fn utf32_prefix_completable(rest: &[u8], be: bool) -> bool {
    if be {
        // Fixed high bytes: the first must be 0 and the second at most
        // 0x10, and a third byte of 0xD8..0xDF under a zero second byte
        // pins the whole remaining range inside the surrogates.
        match rest {
            [0] => true,
            [0, b1] => *b1 <= 0x10,
            [0, b1, b2] => *b1 <= 0x10 && !(*b1 == 0 && (0xD8..=0xDF).contains(b2)),
            _ => false,
        }
    } else {
        // Fixed low bytes: with one or two of them the high half is
        // still free enough to land outside the surrogates, and with
        // three the last byte can only be 0.
        match rest {
            [_] | [_, _] => true,
            _ => rest
                .first_chunk::<3>()
                .and_then(|[b0, b1, b2]| {
                    char::from_u32(*b0 as u32 | (*b1 as u32) << 8 | (*b2 as u32) << 16)
                })
                .is_some(),
        }
    }
}

/// CRuby's `InvalidByteSequenceError` message: it names the offending
/// bytes and the encoding they were read as, and never the destination
/// — the bytes were already ill-formed as *source*, before any
/// conversion was attempted. Same three shapes as the converter's
/// [`conversion_error_message`].
fn invalid_byte_sequence_message(
    src_enc: crate::value::Encoding,
    err: &[u8],
    again: &[u8],
    incomplete: bool,
) -> String {
    let name = src_enc.name();
    if incomplete {
        format!("incomplete {} on {name}", quote_error_bytes(err))
    } else if again.is_empty() {
        format!("{} on {name}", quote_error_bytes(err))
    } else {
        format!(
            "{} followed by {} on {name}",
            quote_error_bytes(err),
            quote_error_bytes(again)
        )
    }
}

/// The `Encoding::InvalidByteSequenceError` for `src_bytes` read as
/// `src_enc` on the way to `dst_enc`, with the five fields the
/// exception exposes riding along as a payload (see
/// [`MonorubyErr::invalid_byte_sequence_error_detail`]).
///
/// When the walk cannot name a sequence — an encoding with no decoder
/// here, or a decoder that disagrees with the one that raised — the
/// message keeps the old wording rather than quoting bytes it guessed.
pub(crate) fn invalid_byte_sequence(
    store: &Store,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    src_bytes: &[u8],
) -> MonorubyErr {
    // Every encoding that can reach here has a walker; if one ever does
    // not, the whole input is the most the message can honestly name.
    let (err, again, incomplete) = first_bad_sequence(src_enc, src_bytes)
        .unwrap_or_else(|| (src_bytes.to_vec(), vec![], false));
    let msg = invalid_byte_sequence_message(src_enc, &err, &again, incomplete);
    // The failing hop is source → pivot, so its destination is the
    // pivot unless the source already sits at it — `"\x80"` on EUC-JP
    // reports UTF-8 as its destination however far the conversion was
    // headed, where a broken UTF-8 source reports the real one.
    let (stage_src, stage_dst) = error_stage_names(src_enc, dst_enc, true);
    let detail = Value::array_from_vec(vec![
        Value::string_from_str(&stage_src),
        Value::string_from_str(&stage_dst),
        binary_string(&err),
        if again.is_empty() {
            Value::nil()
        } else {
            binary_string(&again)
        },
        Value::bool(incomplete),
    ]);
    MonorubyErr::invalid_byte_sequence_error_detail(store, msg, detail)
}

/// The (stage-source, stage-destination) encoding names reported for a
/// conversion error: decode-stage errors fail into the UTF-8 pivot
/// unless the source already is UTF-8-compatible; encode-stage errors
/// fail out of the pivot.
/// Whether `enc` *is* the UTF-8 pivot the conversion chain runs
/// through, so an error in it names one hop rather than two.
///
/// Plain UTF-8 is, and US-ASCII is read as it without converting.
/// `UTF8-MAC` is not, although it holds UTF-8's bytes: getting out of
/// Apple's decomposed form is a conversion, and CRuby names it. That
/// is why `is_utf8_compatible` — which answers "do these bytes read
/// as UTF-8" — is the wrong question here (#1576).
pub(crate) fn is_the_utf8_pivot(enc: crate::value::Encoding) -> bool {
    // US-ASCII is *not* the pivot: CRuby still builds a US-ASCII →
    // UTF-8 step in front of every other hop, so a byte the source
    // encoding has no character for is reported against UTF-8 as the
    // destination whatever the conversion's real destination is
    // (#1596).
    enc == crate::value::Encoding::UTF8
}

pub(crate) fn error_stage_names(
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    decode_stage: bool,
) -> (String, String) {
    let (a, b) = error_stage_encodings(src_enc, dst_enc, decode_stage);
    (transcoder_spelling(&a), transcoder_spelling(&b))
}

fn error_stage_encodings(
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    decode_stage: bool,
) -> (String, String) {
    // A pair with no pivot names itself at both ends, whichever half
    // gave up — there is no UTF-8 hop in the conversion to blame
    // (#1460).
    if jis_direct_from_euc(src_enc, dst_enc).is_some() {
        return (src_enc.name().to_string(), dst_enc.name().to_string());
    }
    if decode_stage {
        // The hop that reads the source is the conversion path's
        // first, and inside the JIS family that is a neighbour on the
        // line rather than the pivot: ISO-2022-JP hands its bytes to
        // stateless-ISO-2022-JP, which hands them to EUC-JP (#1609).
        if let Some(chain) = jis_family_chain(src_enc, dst_enc) {
            return (chain_hop_name(&chain, 0), chain_hop_name(&chain, 1));
        }
        let stage_dst = if is_the_utf8_pivot(src_enc) {
            dst_enc.name().to_string()
        } else {
            "UTF-8".to_string()
        };
        (src_enc.name().to_string(), stage_dst)
    } else {
        // The hop that writes the destination reads the destination's
        // own pivot, which is EUC-JP for stateless-ISO-2022-JP and
        // UTF-8 for everything else (#1600).
        let pivot = if dst_enc == stateless_kddi_enc() {
            utf8_kddi_enc()
        } else if stateless_iso2022jp(dst_enc).is_some() {
            crate::value::Encoding::EUC_JP
        } else {
            crate::value::Encoding::UTF8
        };
        let stage_src = if src_enc == pivot
            || is_the_utf8_pivot(src_enc) && pivot == crate::value::Encoding::UTF8
        {
            src_enc.name().to_string()
        } else {
            pivot.name().to_string()
        };
        (stage_src, dst_enc.name().to_string())
    }
}

/// A BINARY-tagged String value from raw bytes.
pub(crate) fn binary_string(bytes: &[u8]) -> Value {
    let mut s =
        crate::value::RStringInner::from_encoding_scanned(bytes, crate::value::Encoding::Ascii8);
    s.set_encoding(crate::value::Encoding::Ascii8);
    Value::string_from_inner(s)
}

/// The hops of a conversion between a member of one of the JIS lines
/// and anything else, as `convpath` reports them.
///
/// The family runs in lines: `ISO-2022-JP — stateless-ISO-2022-JP —
/// EUC-JP — Shift_JIS`, with UTF-8 hanging off EUC-JP's end;
/// `ISO-2022-JP-KDDI — stateless-ISO-2022-JP-KDDI — UTF8-KDDI —
/// UTF-8`; and `CP50220 — CP51932 — UTF-8` (CP50221 likewise). A
/// conversion walks its line, so the cells never become Unicode
/// unless the other end is outside the family (#1609, #1520, #1530).
pub(crate) fn jis_family_chain(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
) -> Option<Vec<crate::value::Encoding>> {
    use crate::value::Encoding as E;
    if src == dst {
        return None;
    }
    let jis = [E::Iso2022Jp, stateless_enc(), E::EUC_JP, E::Sjis(0)];
    let kddi = [
        crate::value::Encoding::Other(7),
        stateless_kddi_enc(),
        utf8_kddi_enc(),
        E::UTF8,
    ];
    let cp50220 = [E::Other(1), cp51932_enc(), E::UTF8];
    let cp50221 = [E::Other(2), cp51932_enc(), E::UTF8];
    let ebcdic = [ibm037_enc(), E::Iso8859(1), E::UTF8];
    // Each line, the index of its gateway to the pivot (EUC-JP has a
    // hop to UTF-8; the other lines end at UTF-8 itself), and how many
    // of its members make a pair this function's business — the
    // stateful and stateless spellings, not the ordinary encodings a
    // line passes through.
    let lines: [(&[E], usize, usize); 5] = [
        (&jis, 2, 2),
        (&kddi, 3, 2),
        (&cp50220, 2, 1),
        (&cp50221, 2, 1),
        (&ebcdic, 2, 1),
    ];
    let (line, gateway) = lines
        .iter()
        .find(|(l, _, own)| l[..*own].contains(&src) || l[..*own].contains(&dst))
        .map(|(l, g, _)| (*l, *g))?;
    let rank = |e: crate::value::Encoding| line.iter().position(|&x| x == e);
    let mut hops: Vec<crate::value::Encoding> = Vec::new();
    match (rank(src), rank(dst)) {
        (Some(a), Some(b)) => {
            let mut i = a;
            hops.push(line[i]);
            while i != b {
                i = if b > i { i + 1 } else { i - 1 };
                hops.push(line[i]);
            }
        }
        // Into the line from outside: reach its gateway first, which
        // is the only member with a hop to the pivot — by the source's
        // own line when it is on one (#1530).
        (None, Some(b)) => {
            match jis_family_chain(src, E::UTF8) {
                Some(pre) => hops.extend(pre),
                None => {
                    hops.push(src);
                    if src != E::UTF8 {
                        hops.push(E::UTF8);
                    }
                }
            }
            let mut i = gateway;
            hops.push(line[i]);
            while i != b {
                i -= 1;
                hops.push(line[i]);
            }
        }
        (Some(a), None) => {
            let mut i = a;
            hops.push(line[i]);
            while i != gateway {
                i += 1;
                hops.push(line[i]);
            }
            match jis_family_chain(E::UTF8, dst) {
                Some(post) => hops.extend(post),
                None => {
                    if dst != E::UTF8 {
                        hops.push(E::UTF8);
                    }
                    hops.push(dst);
                }
            }
        }
        (None, None) => return None,
    }
    hops.dedup();
    (hops.len() >= 2).then_some(hops)
}

/// Encoding names that CRuby flags as "dummy" — registered but not
/// natively decoded. monoruby's broader `Encoding::is_dummy` covers
/// "we don't decode", which is too eager (ISO-8859 / EUC-JP / SJIS
/// have CRuby decoders even if monoruby doesn't). For
/// `Encoding#dummy?` and `Encoding#inspect` we use this narrower
/// match to match CRuby observed behaviour.
pub(crate) fn is_cruby_dummy_name(name: &str) -> bool {
    let normalized = name.to_uppercase().replace('-', "_");
    // CRuby's actual dummy-encoding set (Emacs-Mule / CESU-8 /
    // stateless-ISO-2022-JP are *not* dummy in CRuby — they are
    // ASCII-compatible).
    matches!(
        normalized.as_str(),
        "UTF_7"
            | "UTF_16"
            | "UTF_32"
            | "CP50220"
            | "CP50221"
            | "ISO_2022_JP"
            | "ISO_2022_JP_2"
            | "ISO_2022_JP_KDDI"
            | "IBM037"
    )
}
