use super::*;

//
// Encoding class and encoding-related String methods
//

pub(super) fn encoding_class(globals: &Globals) -> ClassId {
    globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
        .unwrap()
        .as_class_id()
}

/// Map a constant-style encoding name (`SHIFT_JIS`, `EUC_JP`,
/// `Windows_1252`, …) to the canonical CRuby name (`Shift_JIS`,
/// `EUC-JP`, `Windows-1252`). Most encodings just translate
/// underscores to hyphens; a handful (Shift_JIS, eucJP-ms, …) need
/// explicit overrides because CRuby uses mixed-case or keeps
/// underscores.
pub(super) fn canonical_encoding_name(name: &str) -> &'static str {
    match name {
        // Underscore-preserving / mixed-case names CRuby exposes.
        "SHIFT_JIS" | "Shift_JIS" => "Shift_JIS",
        "EUCJP_MS" => "eucJP-ms",
        // Defaults: replace `_` with `-`. The `match` returns
        // `&'static str`, but the input is also `&'static str` from
        // the call site (a literal name in the constant table). The
        // wildcard arm uses a `Box::leak` trick at startup time —
        // the encoding table is initialized once.
        _ => Box::leak(name.replace('_', "-").into_boxed_str()),
    }
}

pub(super) fn init_encoding(globals: &mut Globals) {
    let enc = globals.define_class_under_obj("Encoding");
    let val = Value::object(enc.id());
    globals
        .store
        .set_ivar(
            val,
            IdentId::_NAME,
            Value::string_from_str("#<Encoding:BINARY (ASCII-8BIT)>"),
        )
        .unwrap();
    globals
        .store
        .set_ivar(
            val,
            IdentId::_ENCODING,
            Value::string_from_str("ASCII-8BIT"),
        )
        .unwrap();
    globals.set_constant(enc.id(), IdentId::ASCII_8BIT, val);
    globals.set_constant_by_str(enc.id(), "BINARY", val);
    // Add encoding constants (placeholder objects for compatibility).
    // monoruby does not actually support these encodings natively, but the
    // constants must exist so that code like `str.encoding == Encoding::UTF_16LE`
    // can evaluate to false.
    for name in [
        "UTF_8",
        "UTF_16LE",
        "UTF_16BE",
        "UTF_16",
        "UTF_32LE",
        "UTF_32BE",
        "UTF_32",
        "US_ASCII",
        "ISO_8859_1",
        "ISO_8859_2",
        "ISO_8859_3",
        "ISO_8859_4",
        "ISO_8859_5",
        "ISO_8859_6",
        "ISO_8859_7",
        "ISO_8859_8",
        "ISO_8859_9",
        "ISO_8859_10",
        "ISO_8859_11",
        "ISO_8859_13",
        "ISO_8859_14",
        "ISO_8859_15",
        "ISO_8859_16",
        "Shift_JIS",
        "SHIFT_JIS",
        "EUC_JP",
        "ISO_2022_JP",
        "Windows_1250",
        "Windows_1251",
        "Windows_1252",
        "Windows_1253",
        "Windows_1254",
        "Windows_1255",
        "Windows_1256",
        "Windows_1257",
        "Windows_1258",
        "Windows_31J",
        "IBM437",
        "IBM737",
        "IBM775",
        "IBM850",
        "IBM852",
        "IBM855",
        "IBM857",
        "IBM860",
        "IBM861",
        "IBM862",
        "IBM863",
        "IBM864",
        "IBM865",
        "IBM866",
        "IBM869",
        "KOI8_R",
        "KOI8_U",
        "GB2312",
        "GBK",
        "GB18030",
        "Big5",
        "EUC_KR",
        "EUC_TW",
        "CP949",
        "TIS_620",
        "MACJAPANESE",
        "EUCJP_MS",
        "CP51932",
        "STATELESS_ISO_2022_JP",
        "CESU_8",
        // Additional encodings exercised by ruby/spec. Aliases that
        // share an *object* with an existing constant (BINARY ↔
        // ASCII-8BIT, ASCII ↔ US-ASCII, CP65001 ↔ UTF-8) are handled
        // separately below so the constants compare equal under
        // object-identity `==`.
        "UTF_7",
        "CP50220",
        "CP50221",
        "Emacs_Mule",
        "Big5_HKSCS",
        "Big5_UAO",
        "GB12345",
        "MacCyrillic",
        "MacGreek",
        "MacIceland",
        "MacRoman",
        "MacRomania",
        "MacThai",
        "MacTurkish",
        "MacUkraine",
        // `UTF8_MAC` (and the legacy `UTF_8_MAC` spelling) is
        // CRuby's HFS+/macOS-NFD UTF-8 variant. We expose both
        // constant identifiers so spec setup resolves; the
        // underlying transcoder treats them as a UTF-8 alias.
        "UTF8_MAC",
        "UTF_8_MAC",
        // `CESU-8` is a UTF-8 variant used by some legacy systems
        // (encodes supplementary chars as surrogate pairs in
        // UTF-8). We don't transcode it specially; the constant
        // exists so `Encoding::CESU_8` resolves.
        "CESU_8",
    ] {
        let canonical = canonical_encoding_name(name);
        // If a constant with the same canonical name has already been
        // registered (for example, `Shift_JIS` registered before
        // `SHIFT_JIS` — both canonicalise to "Shift_JIS"), reuse its
        // Value so `Encoding::SHIFT_JIS.equal?(Encoding::Shift_JIS)`
        // is `true`.
        let val = if let Some(existing) = globals
            .store
            .get_constant_noautoload(enc.id(), IdentId::get_id(canonical))
        {
            existing
        } else {
            let val = Value::object(enc.id());
            globals
                .store
                .set_ivar(
                    val,
                    IdentId::_NAME,
                    Value::string_from_str(&format!("#<Encoding:{}>", canonical)),
                )
                .unwrap();
            globals
                .store
                .set_ivar(
                    val,
                    IdentId::_ENCODING,
                    Value::string_from_str(canonical),
                )
                .unwrap();
            val
        };
        globals.set_constant_by_str(enc.id(), name, val);
        // Also expose the canonical-cased constant if the input name
        // differs (e.g. registering `SHIFT_JIS` should make
        // `Encoding::Shift_JIS` resolve to the same Value too). Skip
        // when the canonical-cased constant has already been seen so
        // we don't trip the "already initialized" warning.
        if canonical != name
            && globals
                .store
                .get_constant_noautoload(enc.id(), IdentId::get_id(canonical))
                .is_none()
        {
            globals.set_constant_by_str(enc.id(), canonical, val);
        }
    }

    // Aliases that share their underlying Value with another
    // constant — `Encoding::ASCII == Encoding::US_ASCII` and
    // `Encoding::CP65001 == Encoding::UTF_8` per CRuby. (BINARY ↔
    // ASCII-8BIT is already wired above.)
    if let Some(us_ascii) =
        globals.store.get_constant_noautoload(enc.id(), IdentId::get_id("US_ASCII"))
    {
        globals.set_constant_by_str(enc.id(), "ASCII", us_ascii);
    }
    if let Some(utf8) = globals.store.get_constant_noautoload(enc.id(), IdentId::UTF_8) {
        globals.set_constant_by_str(enc.id(), "CP65001", utf8);
    }

    // Encoding::CompatibilityError < EncodingError < StandardError.
    // The fourth `define_class` argument is the *lexical parent* —
    // pass `enc.id()` so `Module#name` walks back through `Encoding`
    // and renders `"Encoding::CompatibilityError"`. (Passing
    // `OBJECT_CLASS` would still register the class as the
    // `Encoding::CompatibilityError` *constant*, but its `parent`
    // field would point at `Object`, and `set_constant` only
    // re-parents anonymous / non-permanent classes — so the bare
    // leaf `"CompatibilityError"` would leak through.)
    let enc_error_val = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("EncodingError"))
        .unwrap();
    let enc_error_module = enc_error_val.expect_class(globals).unwrap();
    let compat_error = globals.define_class("CompatibilityError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "CompatibilityError", compat_error.get());
    // Encoding::ConverterNotFoundError < EncodingError. Stubbed so
    // specs that reference the constant (e.g.
    // `String#encode` expectations) don't fail with NameError before
    // we get to the actual encode behaviour.
    let conv_not_found =
        globals.define_class("ConverterNotFoundError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "ConverterNotFoundError", conv_not_found.get());
    // Encoding::UndefinedConversionError < EncodingError. Same
    // motivation — referenced by `String#encode` specs.
    let undef_conv =
        globals.define_class("UndefinedConversionError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "UndefinedConversionError", undef_conv.get());
    // Encoding::InvalidByteSequenceError < EncodingError. Same
    // motivation.
    let invalid_byte =
        globals.define_class("InvalidByteSequenceError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "InvalidByteSequenceError", invalid_byte.get());

    // Instance accessors on the encoding-error subclasses, exposed
    // by parsing the canonical `"U+XXXX from SRC to DST"` /
    // `"\"\\xXX\" on SRC"` message strings monoruby formats in
    // `transcode_bytes_with_opts`. Threading the structured data
    // through the `MonorubyErr` → exception materialisation
    // pipeline would need a new `MonorubyErrKind` variant; the
    // message-parse approach gets the same observable behaviour
    // for the cases the spec exercises without that surgery.
    globals.define_builtin_func(
        undef_conv.id(),
        "source_encoding_name",
        enc_err_source_encoding_name,
        0,
    );
    globals.define_builtin_func(
        undef_conv.id(),
        "destination_encoding_name",
        enc_err_destination_encoding_name,
        0,
    );
    globals.define_builtin_func(
        undef_conv.id(),
        "source_encoding",
        enc_err_source_encoding,
        0,
    );
    globals.define_builtin_func(
        undef_conv.id(),
        "destination_encoding",
        enc_err_destination_encoding,
        0,
    );
    globals.define_builtin_func(undef_conv.id(), "error_char", enc_err_error_char, 0);
    globals.define_builtin_func(
        invalid_byte.id(),
        "source_encoding_name",
        enc_err_source_encoding_name,
        0,
    );
    globals.define_builtin_func(
        invalid_byte.id(),
        "destination_encoding_name",
        enc_err_destination_encoding_name,
        0,
    );
    globals.define_builtin_func(
        invalid_byte.id(),
        "source_encoding",
        enc_err_source_encoding,
        0,
    );
    globals.define_builtin_func(
        invalid_byte.id(),
        "destination_encoding",
        enc_err_destination_encoding,
        0,
    );
    globals.define_builtin_func(
        invalid_byte.id(),
        "incomplete_input?",
        enc_err_incomplete_input_p,
        0,
    );

    // Encoding class methods
    globals.define_builtin_class_func(enc.id(), "default_external", enc_default_external, 0);
    globals.define_builtin_class_func(enc.id(), "default_external=", enc_set_default_external, 1);
    globals.define_builtin_class_func(enc.id(), "default_internal", enc_default_internal, 0);
    globals.define_builtin_class_func(enc.id(), "default_internal=", enc_set_default_internal, 1);

    // Encoding::Converter — minimal stub. `Encoding::Converter.new(src,
    // dst)` validates that monoruby can transcode the pair (mostly
    // used in spec setup expectations). The runtime `convert` /
    // `primitive_convert` API is not exposed yet.
    // Inherit from `Object` explicitly so `Encoding::Converter#class`
    // and the Kernel methods inherited via Object (`is_a?`,
    // `inspect`, …) work on Converter instances. Passing `None`
    // produces a class with no superclass, which monoruby renders
    // as an empty string for `Converter.superclass` (CRuby shows
    // `Object`).
    let object_class = globals.store.object_class();
    let converter = globals.define_class("Converter", object_class, enc.id());
    globals.set_constant_by_str(enc.id(), "Converter", converter.get());
    // Mirror CRuby's Encoding::Converter::* flag constants. monoruby
    // doesn't implement the underlying behaviour (decorators / output
    // pacing) — exposing the integers is enough for spec setup like
    // `Encoding::Converter.new(src, dst, INVALID_REPLACE | UNDEF_REPLACE)`
    // not to NameError on constant lookup.
    for (name, val) in [
        ("INVALID_MASK", 0x0000_000fi64),
        ("INVALID_REPLACE", 0x0000_0002),
        ("UNDEF_MASK", 0x0000_00f0),
        ("UNDEF_REPLACE", 0x0000_0020),
        ("UNDEF_HEX_CHARREF", 0x0000_0030),
        ("PARTIAL_INPUT", 0x0002_0000),
        ("AFTER_OUTPUT", 0x0004_0000),
        ("UNIVERSAL_NEWLINE_DECORATOR", 0x0000_0100),
        ("CRLF_NEWLINE_DECORATOR", 0x0000_1000),
        ("CR_NEWLINE_DECORATOR", 0x0000_2000),
        ("XML_TEXT_DECORATOR", 0x0000_8000),
        ("XML_ATTR_CONTENT_DECORATOR", 0x0001_0000),
        ("XML_ATTR_QUOTE_DECORATOR", 0x0010_0000),
    ] {
        globals.set_constant_by_str(converter.id(), name, Value::integer(val));
    }
    // 2..3 positional args: (src, dst, opts?). `opts` accepts an
    // Integer flag mask or an option Hash; both are tolerated and
    // ignored beyond construction-time validation.
    globals.define_builtin_class_func_with(converter.id(), "new", converter_new, 2, 3, false);
    globals.define_builtin_class_func(
        converter.id(),
        "asciicompat_encoding",
        converter_asciicompat_encoding,
        1,
    );
    globals.define_builtin_class_func(
        converter.id(),
        "search_convpath",
        converter_search_convpath,
        2,
    );
    globals.define_builtin_func(converter.id(), "source_encoding", converter_source_encoding, 0);
    globals.define_builtin_func(
        converter.id(),
        "destination_encoding",
        converter_destination_encoding,
        0,
    );
    globals.define_builtin_func(converter.id(), "replacement", converter_replacement, 0);
    globals.define_builtin_func(converter.id(), "replacement=", converter_replacement_set, 1);
    globals.define_builtin_func(converter.id(), "convert", converter_convert, 1);
    globals.define_builtin_func(converter.id(), "finish", converter_finish, 0);
    globals.define_builtin_func(converter.id(), "inspect", converter_inspect, 0);
    // Streaming-API stubs. monoruby's transcoder is single-shot
    // (`convert` runs the whole input at once and either succeeds
    // or raises), so the chunked / partial-output variants
    // (`primitive_convert`, `last_error`, `primitive_errinfo`)
    // can't faithfully report mid-stream state. We expose them
    // anyway so spec setup like
    // `ec.primitive_errinfo[0]` doesn't NoMethodError before the
    // assertion we actually care about runs.
    globals.define_builtin_func(
        converter.id(),
        "primitive_errinfo",
        converter_primitive_errinfo,
        0,
    );
    globals.define_builtin_func(converter.id(), "last_error", converter_last_error, 0);
    globals.define_builtin_func(converter.id(), "putback", converter_putback, 0);
    globals.define_builtin_func(converter.id(), "==", converter_eq, 1);
    globals.define_builtin_class_func(enc.id(), "list", enc_list, 0);
    globals.define_builtin_class_func(enc.id(), "find", enc_find, 1);
    globals.define_builtin_class_func(enc.id(), "locale_charmap", enc_locale_charmap, 0);
    globals.define_builtin_class_func(enc.id(), "aliases", enc_aliases, 0);
    globals.define_builtin_class_func(enc.id(), "name_list", enc_name_list, 0);
    globals.define_builtin_class_func(enc.id(), "compatible?", enc_compatible, 2);
    globals.define_builtin_func(enc.id(), "names", enc_names, 0);
    globals.define_builtin_func(enc.id(), "to_s", enc_to_s, 0);
    globals.define_builtin_func(enc.id(), "inspect", enc_inspect, 0);
    globals.define_builtin_func(enc.id(), "name", enc_to_s, 0);
    globals.define_builtin_func(enc.id(), "ascii_compatible?", enc_ascii_compatible_p, 0);
    globals.define_builtin_func(enc.id(), "dummy?", enc_dummy_p, 0);
}

// -------------------------------------------------------
// Transcoding (String#encode, String#encode!, Encoding::Converter)
// -------------------------------------------------------

/// Map a monoruby `Encoding` to the corresponding
/// `encoding_rs::Encoding`. Returns `None` for encodings
/// `encoding_rs` doesn't support (UTF-32, UsAscii — see notes
/// below) so the caller can decide whether to fast-path them or
/// raise `Encoding::ConverterNotFoundError`.
///
/// - `UsAscii`: encoding_rs maps the "us-ascii" label to
///   `windows-1252`, which differs in the 0x80..0x9F range. We
///   intentionally return `None` and let `transcode_bytes`
///   handle UsAscii specially (only valid for 7-bit content,
///   in which case the bytes pass through unchanged).
/// - `Ascii8` (BINARY): no transcoding semantics — bytes pass
///   through unchanged for ASCII-only content; otherwise
///   `encode` raises `UndefinedConversionError` per CRuby.
/// - UTF-32: encoding_rs does not support it; we raise
///   `ConverterNotFoundError`.
fn encoding_to_rs(enc: crate::value::Encoding) -> Option<&'static encoding_rs::Encoding> {
    use crate::value::Encoding as E;
    let label: &[u8] = match enc {
        E::Utf8 => b"utf-8",
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
        E::EucJp => b"euc-jp",
        E::Sjis(_) => b"shift_jis",
        E::Iso2022Jp => b"iso-2022-jp",
        // Handled by callers as fast paths.
        E::Utf32Le | E::Utf32Be | E::Ascii8 | E::UsAscii => return None,
    };
    encoding_rs::Encoding::for_label(label)
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
pub(super) struct TranscodeOpts {
    pub invalid_replace: bool,
    pub undef_replace: bool,
    pub replace: Option<String>,
}

impl TranscodeOpts {
    fn replace_str(&self, dst_enc: crate::value::Encoding) -> String {
        if let Some(s) = &self.replace {
            return s.clone();
        }
        // CRuby: default replacement is "�" for UTF
        // destinations and "?" otherwise.
        match dst_enc {
            crate::value::Encoding::Utf8
            | crate::value::Encoding::Utf16Le
            | crate::value::Encoding::Utf16Be
            | crate::value::Encoding::Utf32Le
            | crate::value::Encoding::Utf32Be => "\u{FFFD}".to_string(),
            _ => "?".to_string(),
        }
    }
}

pub(super) fn transcode_bytes(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    store: &Store,
) -> Result<Vec<u8>> {
    transcode_bytes_with_opts(src_bytes, src_enc, dst_enc, &TranscodeOpts::default(), store)
}

pub(super) fn transcode_bytes_with_opts(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<Vec<u8>> {
    use crate::value::Encoding as E;
    if src_enc == dst_enc {
        return Ok(src_bytes.to_vec());
    }
    // Fast path: 7-bit content + ascii-compatible source AND
    // destination copies through unchanged. Covers both the
    // "BINARY ASCII → UTF-8" and "UsAscii → X" cases. Required
    // because encoding_rs's `us-ascii` → `windows-1252` mapping
    // isn't what CRuby does, and BINARY isn't a real encoding_rs
    // encoding. We require the *source* to also be
    // ASCII-compatible — `Iso2022Jp` looks like 7-bit input on
    // the wire (it's a 7-bit encoding), but its bytes carry ESC
    // sequences that change interpretation, so the identity copy
    // would silently retag escape codes as ASCII characters.
    let all_ascii = src_bytes.iter().all(|&b| b < 0x80);
    if all_ascii && src_enc.is_ascii_compatible() && dst_enc.is_ascii_compatible() {
        return Ok(src_bytes.to_vec());
    }
    // BINARY → ascii-compat with non-ASCII bytes is "undef":
    // there's no Unicode for "byte 0x82 in BINARY".
    if src_enc == E::Ascii8 && dst_enc.is_ascii_compatible() {
        return Err(MonorubyErr::undefined_conversion_error(
            store,
            format!(
                "\"\\x{:02X}\" from ASCII-8BIT to {}",
                src_bytes
                    .iter()
                    .copied()
                    .find(|&b| b >= 0x80)
                    .unwrap_or(0),
                dst_enc.name()
            ),
        ));
    }
    // Anything → BINARY is just a tag change (BINARY accepts any
    // bytes). If the source isn't already valid as some recognised
    // encoding, we still let it through — CRuby tolerates this.
    if dst_enc == E::Ascii8 {
        // For multibyte sources we'd ideally decode to UTF-8 then
        // hand back the UTF-8 bytes (CRuby converts "あ" in EUC-JP
        // to UTF-8 BINARY, which contains the UTF-8 byte sequence).
        // Implement that path via encoding_rs.
        if let Some(src_rs) = encoding_to_rs(src_enc) {
            let (decoded, decode_err) = src_rs.decode_without_bom_handling(src_bytes);
            if decode_err {
                return Err(MonorubyErr::invalid_byte_sequence_error(
                    store,
                    format!(
                        "invalid byte sequence on {} → ASCII-8BIT",
                        src_enc.name()
                    ),
                ));
            }
            return Ok(decoded.into_owned().into_bytes());
        }
        // Source not representable; just copy bytes.
        return Ok(src_bytes.to_vec());
    }
    // Decode the source through encoding_rs (UsAscii is decoded as
    // UTF-8 since 7-bit ASCII bytes are identical in both — the
    // all_ascii fast-path above already covered the ASCII-only
    // case, so here we know src has a non-ASCII byte; UsAscii src
    // with non-ASCII content is invalid by definition).
    let src_rs = match encoding_to_rs(src_enc) {
        Some(s) => s,
        None if src_enc == E::UsAscii => {
            return Err(MonorubyErr::invalid_byte_sequence_error(
                store,
                format!("invalid byte sequence on US-ASCII"),
            ));
        }
        None => {
            return Err(MonorubyErr::converter_not_found_error(
                store,
                format!(
                    "code converter not found ({} to {})",
                    src_enc.name(),
                    dst_enc.name()
                ),
            ));
        }
    };
    let (decoded, decode_err) = src_rs.decode_without_bom_handling(src_bytes);
    if decode_err && !opts.invalid_replace {
        return Err(MonorubyErr::invalid_byte_sequence_error(
            store,
            format!(
                "invalid byte sequence on {} ({} → {})",
                src_enc.name(),
                src_enc.name(),
                dst_enc.name()
            ),
        ));
    }
    // UsAscii destination: only ASCII characters are representable.
    // Non-ASCII content raises UndefinedConversionError unless
    // `undef: :replace` was given (in which case we substitute).
    if dst_enc == E::UsAscii {
        if !decoded.chars().all(|c| c.is_ascii()) {
            if !opts.undef_replace {
                let bad = decoded.chars().find(|c| !c.is_ascii()).unwrap();
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    format!(
                        "U+{:04X} from {} to US-ASCII",
                        bad as u32,
                        src_enc.name()
                    ),
                ));
            }
            let replace = opts.replace_str(dst_enc);
            let mut out = String::with_capacity(decoded.len());
            for c in decoded.chars() {
                if c.is_ascii() {
                    out.push(c);
                } else {
                    out.push_str(&replace);
                }
            }
            return Ok(out.into_bytes());
        }
        return Ok(decoded.into_owned().into_bytes());
    }
    let dst_rs = match encoding_to_rs(dst_enc) {
        Some(d) => d,
        None => {
            return Err(MonorubyErr::converter_not_found_error(
                store,
                format!(
                    "code converter not found ({} to {})",
                    src_enc.name(),
                    dst_enc.name()
                ),
            ));
        }
    };
    let (encoded, _, encode_err) = dst_rs.encode(&decoded);
    if encode_err {
        if !opts.undef_replace {
            return Err(MonorubyErr::undefined_conversion_error(
                store,
                format!(
                    "U+{:04X} from {} to {}",
                    decoded
                        .chars()
                        .find(|c| !c.is_ascii())
                        .map(|c| c as u32)
                        .unwrap_or(0),
                    src_enc.name(),
                    dst_enc.name()
                ),
            ));
        }
        // `undef: :replace`: walk character by character and substitute
        // anything the destination encoder can't represent. This is
        // O(N*M) but only runs in the slow / replace path.
        let replace = opts.replace_str(dst_enc);
        let mut out: Vec<u8> = Vec::with_capacity(encoded.len());
        for c in decoded.chars() {
            let mut buf = [0u8; 4];
            let s = c.encode_utf8(&mut buf);
            let (chunk, _, ce) = dst_rs.encode(s);
            if ce {
                // Substitute. For the replacement we ALSO need to
                // encode it through the destination encoder so that
                // non-UTF dst encodings get the right bytes.
                let (rchunk, _, _) = dst_rs.encode(&replace);
                out.extend_from_slice(&rchunk);
            } else {
                out.extend_from_slice(&chunk);
            }
        }
        return Ok(out);
    }
    Ok(encoded.into_owned())
}

// -------------------------------------------------------
// String instance methods related to encoding
// -------------------------------------------------------

/// Resolve an encoding argument (String or Encoding object) to a validated
/// constant name via `enc_name_to_const`.  Returns the constant name on
/// success or an ArgumentError on unknown encoding.
fn resolve_enc_arg(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<&'static str> {
    let name = if let Some(s) = arg.is_str() {
        s.to_string()
    } else if arg.class() == encoding_class(globals) {
        let s = globals.store.get_ivar(arg, IdentId::_ENCODING).unwrap();
        s.as_str().to_string()
    } else {
        let s = arg.coerce_to_string(vm, globals)?;
        s
    };
    enc_name_to_const(&name).ok_or_else(|| {
        // CRuby raises `Encoding::ConverterNotFoundError` (not
        // ArgumentError) for `String#encode("xyz")` when the
        // label is unknown. The encoding-search path below the
        // call (`Encoding.find`) does still raise ArgumentError;
        // the wrapper at `encode_resolve_enc_arg` lifts it to
        // `ConverterNotFoundError` for the encode path.
        MonorubyErr::argumenterr(format!("unknown encoding name - {}", name))
    })
}

/// `resolve_enc_arg` variant that lifts the unknown-encoding
/// `ArgumentError` to `Encoding::ConverterNotFoundError`,
/// matching CRuby's `String#encode` semantics.
fn encode_resolve_enc_arg(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<&'static str> {
    resolve_enc_arg(vm, globals, arg).map_err(|e| {
        // Only translate the unknown-encoding-name case; other
        // argument errors (TypeError on a non-coercible argument,
        // etc.) pass through unchanged.
        let msg = e.message().to_string();
        if msg.starts_with("unknown encoding name") {
            let label = msg
                .trim_start_matches("unknown encoding name - ")
                .to_string();
            MonorubyErr::converter_not_found_error(
                &globals.store,
                format!("code converter not found for {}", label),
            )
        } else {
            e
        }
    })
}

/// Implement the `xml:` keyword of `String#encode` directly here
/// (was previously a Ruby-level shadow in `monoruby/builtins/
/// string.rb`).
///
/// - `xml: :attr` — escape `&`, `<`, `>`, `"` and wrap the result in
///   `"`s for use as an HTML attribute value.
/// - `xml: :text` — escape `&`, `<`, `>`.
/// - any other value — `ArgumentError`.
///
/// Returns `Some(Value)` when the `xml:` key is present (the caller
/// should short-circuit). Returns `None` when no `xml:` option was
/// supplied — the regular transcoding path runs.
fn handle_xml_option(globals: &mut Globals, lfp: Lfp) -> Result<Option<Value>> {
    let opts_val = match get_options_hash_value(lfp) {
        Some(v) => v,
        None => return Ok(None),
    };
    // Look up `xml:` via the Hash without going through `[]` — we
    // don't want any Hash#default to fire here. Use the inner map's
    // `get_no_default` if available; otherwise just iterate keys.
    let xml_sym = Value::symbol_from_str("xml");
    let opts = opts_val.try_hash_ty().unwrap();
    let v = match find_hash_value_for_symbol(&opts, "xml") {
        Some(v) => v,
        None => return Ok(None),
    };
    let _ = xml_sym; // silence warning when not used by the alternative path
    if v.is_nil() {
        // `xml:` explicitly nil — fall through to transcoding.
        return Ok(None);
    }
    let sym_attr = Value::symbol_from_str("attr");
    let sym_text = Value::symbol_from_str("text");
    let mode = if v == sym_attr {
        XmlMode::Attr
    } else if v == sym_text {
        XmlMode::Text
    } else {
        return Err(MonorubyErr::argumenterr(format!(
            "unexpected value for xml option: {}",
            v.inspect(&globals.store)
        )));
    };
    let bytes = lfp.self_val().as_rstring_inner().as_bytes().to_vec();
    let s = String::from_utf8_lossy(&bytes);
    let mut out = String::with_capacity(s.len() + 2);
    if matches!(mode, XmlMode::Attr) {
        out.push('"');
    }
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' if matches!(mode, XmlMode::Attr) => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    if matches!(mode, XmlMode::Attr) {
        out.push('"');
    }
    Ok(Some(Value::string_from_inner(
        crate::value::RStringInner::from_string_scanned(out),
    )))
}

#[derive(Clone, Copy)]
enum XmlMode {
    Attr,
    Text,
}

/// Look up a value by symbol-named key in a Hash, without
/// triggering `Hash#default` / `default_proc`. Iterates the
/// hash keys and matches by symbol name.
fn find_hash_value_for_symbol(hash: &crate::value::Hashmap, name: &str) -> Option<Value> {
    for (k, v) in hash.iter() {
        if let Some(sym) = k.try_symbol() {
            if sym.get_name() == name {
                return Some(v);
            }
        }
        if let Some(s) = k.is_str() {
            if s == name {
                return Some(v);
            }
        }
    }
    None
}

/// Pull the keyword-argument hash Value off `lfp` (the trailing
/// `**opts` of `String#encode` / `#encode!`). Returns `None` when
/// the call site didn't pass any kwargs.
fn get_options_hash_value(lfp: Lfp) -> Option<Value> {
    // For builtins registered via `define_builtin_func_with_kw` with
    // `accept_double_splat=true`, the kwarg hash sits at the trailing
    // positional slot. encode has at most 2 positional args, so the
    // hash is at index <= 2.
    for i in (0..3).rev() {
        if let Some(v) = lfp.try_arg(i) {
            if v.try_hash_ty().is_some() {
                return Some(v);
            }
        }
    }
    None
}

/// Map a canonical encoding name (the constant-table key) back
/// to the matching `Encoding` value.
fn encoding_from_canonical_name(name: &str) -> Option<crate::value::Encoding> {
    crate::value::Encoding::try_from_str(name).ok()
}

/// Resolve the destination encoding for `encode` from arg0.
fn resolve_dst_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<crate::value::Encoding> {
    let name = encode_resolve_enc_arg(vm, globals, arg)?;
    encoding_from_canonical_name(name).ok_or_else(|| {
        MonorubyErr::converter_not_found_error(
            &globals.store,
            format!("code converter not found ({})", name),
        )
    })
}

/// Look up the current `Encoding.default_internal` (set via
/// `Encoding.default_internal=`). Returns `None` when unset
/// (matches CRuby's "no transcoding" default).
fn current_default_internal(globals: &mut Globals) -> Option<crate::value::Encoding> {
    let v = globals.get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))?;
    if v.is_nil() {
        return None;
    }
    let enc_str = globals.store.get_ivar(v, IdentId::_ENCODING)?;
    let name = enc_str.as_str();
    encoding_from_canonical_name(&name)
}

/// Compute the (src_enc, dst_enc) pair for an `encode` call.
/// Returns `None` for `dst_enc` when no transcoding should
/// happen (no args + no default_internal — CRuby returns a
/// copy unchanged).
fn resolve_encode_pair(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    self_enc: crate::value::Encoding,
) -> Result<(crate::value::Encoding, Option<crate::value::Encoding>)> {
    let dst = if let Some(arg0) = lfp.try_arg(0) {
        Some(resolve_dst_encoding(vm, globals, arg0)?)
    } else {
        current_default_internal(globals)
    };
    let src = if let Some(arg1) = lfp.try_arg(1) {
        resolve_dst_encoding(vm, globals, arg1)?
    } else {
        self_enc
    };
    Ok((src, dst))
}

///
/// ### String#encode
///
/// - encode(encoding, **opts) -> String
/// - encode(dst_encoding, src_encoding, **opts) -> String
/// - encode(**opts) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encode.html]
#[monoruby_builtin]
pub(super) fn encode(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(v) = handle_xml_option(globals, lfp)? {
        return Ok(v);
    }
    let self_val = lfp.self_val();
    let self_enc = self_val.as_rstring_inner().encoding();
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc)?;
    let dst_enc = match dst_enc_opt {
        Some(e) => e,
        None => return Ok(self_val.dup()),
    };
    let opts = parse_transcode_opts(lfp);
    let bytes = transcode_bytes_with_opts(
        self_val.as_rstring_inner().as_bytes(),
        src_enc,
        dst_enc,
        &opts,
        &globals.store,
    )?;
    Ok(Value::string_from_inner(
        crate::value::RStringInner::from_encoding_scanned(&bytes, dst_enc),
    ))
}

///
/// ### String#encode!
///
/// - encode!(encoding, **opts) -> self
/// - encode!(dst_encoding, src_encoding, **opts) -> self
/// - encode!(**opts) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encode=21.html]
#[monoruby_builtin]
pub(super) fn encode_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.ensure_string_mutable(vm, globals)?;
    if let Some(v) = handle_xml_option(globals, lfp)? {
        // CRuby's `encode!` just `replace`s self with the encoded
        // form when xml is given.
        if let Some(inner) = v.is_rstring_inner() {
            self_val.replace_with_inner(inner.clone());
        }
        return Ok(self_val);
    }
    let self_enc = self_val.as_rstring_inner().encoding();
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc)?;
    let dst_enc = match dst_enc_opt {
        Some(e) => e,
        None => return Ok(self_val),
    };
    let opts = parse_transcode_opts(lfp);
    let bytes = transcode_bytes_with_opts(
        self_val.as_rstring_inner().as_bytes(),
        src_enc,
        dst_enc,
        &opts,
        &globals.store,
    )?;
    self_val.replace_with_inner(crate::value::RStringInner::from_encoding_scanned(
        &bytes, dst_enc,
    ));
    Ok(self_val)
}

/// Parse `String#encode`'s keyword options (`invalid:`, `undef:`,
/// `replace:`) into a `TranscodeOpts`. Unknown keys are ignored
/// (CRuby silently does the same) — `xml:` is handled separately.
fn parse_transcode_opts(lfp: Lfp) -> TranscodeOpts {
    let opts_val = match get_options_hash_value(lfp) {
        Some(v) => v,
        None => return TranscodeOpts::default(),
    };
    let hash = opts_val.try_hash_ty().unwrap();
    let mut out = TranscodeOpts::default();
    if let Some(v) = find_hash_value_for_symbol(&hash, "invalid") {
        if let Some(sym) = v.try_symbol() {
            if sym.get_name() == "replace" {
                out.invalid_replace = true;
            }
        }
    }
    if let Some(v) = find_hash_value_for_symbol(&hash, "undef") {
        if let Some(sym) = v.try_symbol() {
            if sym.get_name() == "replace" {
                out.undef_replace = true;
            }
        }
    }
    if let Some(v) = find_hash_value_for_symbol(&hash, "replace") {
        if let Some(s) = v.is_str() {
            out.replace = Some(s.to_string());
            // Specifying `replace:` implicitly enables both modes
            // per CRuby (you can't supply a replacement without
            // wanting to replace).
            out.invalid_replace = true;
            out.undef_replace = true;
        }
    }
    out
}

///
/// ### String#encoding
///
/// - encoding -> Encoding
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encoding.html]
#[monoruby_builtin]
pub(super) fn str_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    // Check for overridden encoding label (set by Integer#chr for mock encodings)
    let enc_override_id = IdentId::get_id("/encoding_override");
    if let Some(enc_obj) = globals.store.get_ivar(self_, enc_override_id) {
        return Ok(enc_obj);
    }
    let enc = self_.as_rstring_inner().encoding();
    let enc_class = vm
        .get_constant_checked(globals, OBJECT_CLASS, IdentId::ENCODING)?
        .expect_class(globals)?
        .id();
    let const_name = encoding_constant_name(enc);
    let res = vm.get_constant_checked(globals, enc_class, IdentId::get_id(const_name))?;
    Ok(res)
}

/// Map an `Encoding` to the corresponding `Encoding::<NAME>` Ruby
/// constant name registered by `init_encoding`.
pub(super) fn encoding_constant_name(enc: Encoding) -> &'static str {
    match enc {
        Encoding::Ascii8 => "ASCII_8BIT",
        Encoding::Utf8 => "UTF_8",
        Encoding::UsAscii => "US_ASCII",
        Encoding::Utf16Le => "UTF_16LE",
        Encoding::Utf16Be => "UTF_16BE",
        Encoding::Utf32Le => "UTF_32LE",
        Encoding::Utf32Be => "UTF_32BE",
        Encoding::Iso8859(1) => "ISO_8859_1",
        Encoding::Iso8859(2) => "ISO_8859_2",
        Encoding::Iso8859(3) => "ISO_8859_3",
        Encoding::Iso8859(4) => "ISO_8859_4",
        Encoding::Iso8859(5) => "ISO_8859_5",
        Encoding::Iso8859(6) => "ISO_8859_6",
        Encoding::Iso8859(7) => "ISO_8859_7",
        Encoding::Iso8859(8) => "ISO_8859_8",
        Encoding::Iso8859(9) => "ISO_8859_9",
        Encoding::Iso8859(10) => "ISO_8859_10",
        Encoding::Iso8859(11) => "ISO_8859_11",
        Encoding::Iso8859(13) => "ISO_8859_13",
        Encoding::Iso8859(14) => "ISO_8859_14",
        Encoding::Iso8859(15) => "ISO_8859_15",
        Encoding::Iso8859(16) => "ISO_8859_16",
        Encoding::Iso8859(_) => "ISO_8859_1",
        Encoding::EucJp => "EUC_JP",
        Encoding::Sjis(0) => "SHIFT_JIS",
        Encoding::Sjis(_) => "Windows_31J",
        Encoding::Iso2022Jp => "ISO_2022_JP",
    }
}

///
/// ### String#b
///
/// - b -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/b.html]
#[monoruby_builtin]
pub(super) fn b(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut res = lfp.self_val().dup();
    res.as_rstring_inner_mut().set_encoding(Encoding::Ascii8);
    Ok(res)
}

///
/// ### String#force_encoding
///
/// - force_encoding(encoding) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/force_encoding.html]
#[monoruby_builtin]
pub(super) fn force_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_string_mutable(vm, globals)?;
    let arg0 = lfp.arg(0);
    let enc = if let Some(s) = arg0.is_str() {
        Encoding::try_from_str(s)?
    } else if arg0.class() == encoding_class(globals) {
        let s = globals.store.get_ivar(arg0, IdentId::_ENCODING).unwrap();
        Encoding::try_from_str(s.as_str())?
    } else {
        // Try to_str coercion
        let s = arg0.coerce_to_string(vm, globals)?;
        Encoding::try_from_str(&s)?
    };
    lfp.self_val().as_rstring_inner_mut().set_encoding(enc);
    Ok(lfp.self_val())
}

///
/// ### String#valid_encoding?
///
/// - valid_encoding? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/valid_encoding=3f.html]
#[monoruby_builtin]
pub(super) fn valid_encoding(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_rstring_inner().valid()))
}

///
/// ### String#ascii_only?
///
/// - ascii_only? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/ascii_only=3f.html]
#[monoruby_builtin]
pub(super) fn ascii_only(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // Use the cr-cached `is_ascii_only`, not the slice's `is_ascii`,
    // so that repeated calls on a long-but-already-classified string
    // are O(1) instead of O(n) per call.
    Ok(Value::bool(
        lfp.self_val().as_rstring_inner().is_ascii_only(),
    ))
}

// -------------------------------------------------------
// Encoding class methods
// -------------------------------------------------------

///
/// ### Encoding.default_external
/// - default_external -> Encoding
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/default_external.html]
#[monoruby_builtin]
fn enc_default_external(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let enc_class = encoding_class(globals);
    let utf8 = globals
        .store
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
        .unwrap_or(Value::nil());
    Ok(utf8)
}

///
/// ### Encoding.default_external=
/// - default_external = enc -> enc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/default_external=3d.html]
#[monoruby_builtin]
fn enc_set_default_external(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // Stub: accept the argument but do nothing
    Ok(lfp.arg(0))
}

///
/// ### Encoding.default_internal
/// - default_internal -> Encoding | nil
///
/// Returns the current default-internal encoding (set via
/// `Encoding.default_internal=`), or `nil` if unset.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/default_internal.html]
#[monoruby_builtin]
fn enc_default_internal(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(globals
        .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
        .unwrap_or(Value::nil()))
}

///
/// Names used as ivar keys on a `Encoding::Converter` instance to
/// stash its source / destination encoding and current
/// replacement string. `/`-prefixed keys are a monoruby convention
/// for "internal" ivars that user code can't accidentally read or
/// write.
const CONVERTER_SRC_IVAR: &str = "/converter_src";
const CONVERTER_DST_IVAR: &str = "/converter_dst";
const CONVERTER_REPLACE_IVAR: &str = "/converter_replace";
const CONVERTER_FINISHED_IVAR: &str = "/converter_finished";

/// Validate that `(src, dst)` is a transcoder monoruby can run.
/// Raises `Encoding::ConverterNotFoundError` for anything
/// `encoding_rs` doesn't cover (UTF-32, dummy CRuby encodings).
/// Identical encodings are always allowed.
fn validate_converter_pair(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    store: &Store,
) -> Result<()> {
    if src == dst {
        return Ok(());
    }
    let src_supported = encoding_to_rs(src).is_some()
        || matches!(
            src,
            crate::value::Encoding::Ascii8 | crate::value::Encoding::UsAscii
        );
    let dst_supported = encoding_to_rs(dst).is_some()
        || matches!(
            dst,
            crate::value::Encoding::Ascii8 | crate::value::Encoding::UsAscii
        );
    if !src_supported || !dst_supported {
        return Err(MonorubyErr::converter_not_found_error(
            store,
            format!(
                "code converter not found ({} to {})",
                src.name(),
                dst.name()
            ),
        ));
    }
    Ok(())
}

/// Look up the `Encoding::<NAME>` constant whose `_ENCODING` ivar
/// holds `enc.name()`. Used by accessor methods that return an
/// `Encoding` object.
fn encoding_value(globals: &Globals, enc: crate::value::Encoding) -> Value {
    let enc_class = encoding_class(globals);
    let const_name = encoding_constant_name(enc);
    globals
        .store
        .get_constant_noautoload(enc_class, IdentId::get_id(const_name))
        .unwrap_or(Value::nil())
}

/// Pull the source encoding out of a `Encoding::Converter`
/// instance's stashed ivar. Returns `Encoding::Ascii8` as a
/// best-effort fallback if the ivar is missing or unparseable —
/// `converter_new` always sets it, so the fallback shouldn't
/// fire in practice.
fn converter_get_src(globals: &Globals, recv: Value) -> crate::value::Encoding {
    globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_SRC_IVAR))
        .and_then(|v| v.is_str().map(|s| s.to_string()))
        .and_then(|s| crate::value::Encoding::try_from_str(&s).ok())
        .unwrap_or(crate::value::Encoding::Ascii8)
}

fn converter_get_dst(globals: &Globals, recv: Value) -> crate::value::Encoding {
    globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_DST_IVAR))
        .and_then(|v| v.is_str().map(|s| s.to_string()))
        .and_then(|s| crate::value::Encoding::try_from_str(&s).ok())
        .unwrap_or(crate::value::Encoding::Ascii8)
}

/// Default replacement string used by `Encoding::Converter#replacement`
/// when none has been set explicitly. CRuby uses `"�"` (encoded
/// in the destination encoding) when the dest is UTF-8 / UTF-16 /
/// UTF-32, and `"?"` (US-ASCII) for everything else.
fn converter_default_replacement(dst: crate::value::Encoding) -> Value {
    match dst {
        crate::value::Encoding::Utf8 => {
            // U+FFFD as UTF-8 bytes.
            let mut s = crate::value::RStringInner::from_string_scanned("\u{FFFD}".to_string());
            s.set_encoding(crate::value::Encoding::Utf8);
            Value::string_from_inner(s)
        }
        crate::value::Encoding::Utf16Be | crate::value::Encoding::Utf16Le => {
            // Encode U+FFFD via encoding_rs.
            let label = match dst {
                crate::value::Encoding::Utf16Be => b"utf-16be" as &[u8],
                _ => b"utf-16le",
            };
            let enc_rs = encoding_rs::Encoding::for_label(label).unwrap();
            let (bytes, _, _) = enc_rs.encode("\u{FFFD}");
            let mut s = crate::value::RStringInner::from_encoding(&bytes, dst);
            s.set_encoding(dst);
            Value::string_from_inner(s)
        }
        _ => {
            // Plain `?` tagged as US-ASCII.
            let mut s = crate::value::RStringInner::from_string_scanned("?".to_string());
            s.set_encoding(crate::value::Encoding::UsAscii);
            Value::string_from_inner(s)
        }
    }
}

///
/// ### Encoding::Converter.new
/// - Encoding::Converter.new(src, dst) -> Encoding::Converter
/// - Encoding::Converter.new(src, dst, opts) -> Encoding::Converter
///
/// Validates the (src, dst) pair (raising
/// `Encoding::ConverterNotFoundError` if unsupported) and stashes
/// the encoding tags as instance variables so the accessor and
/// `convert` / `finish` methods can read them back. The third
/// `opts` argument (Integer flag mask or option Hash) is accepted
/// but ignored — only the basic `convert`/`finish` flow is
/// implemented.
///
#[monoruby_builtin]
fn converter_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let src = resolve_dst_encoding(vm, globals, lfp.arg(0))?;
    let dst = resolve_dst_encoding(vm, globals, lfp.arg(1))?;
    validate_converter_pair(src, dst, &globals.store)?;
    let class = lfp.self_val();
    let obj = Value::object(class.as_class_id());
    // Stash the encodings as their canonical *names* so
    // `encoding_value` and `try_from_str` can round-trip without
    // a separate Encoding-to-id table.
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id(CONVERTER_SRC_IVAR),
        Value::string_from_str(src.name()),
    );
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id(CONVERTER_DST_IVAR),
        Value::string_from_str(dst.name()),
    );
    Ok(obj)
}

///
/// ### Encoding::Converter#source_encoding
/// - source_encoding -> Encoding
///
#[monoruby_builtin]
fn converter_source_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let src = converter_get_src(globals, lfp.self_val());
    Ok(encoding_value(globals, src))
}

///
/// ### Encoding::Converter#destination_encoding
/// - destination_encoding -> Encoding
///
#[monoruby_builtin]
fn converter_destination_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let dst = converter_get_dst(globals, lfp.self_val());
    Ok(encoding_value(globals, dst))
}

///
/// ### Encoding::Converter#replacement
/// - replacement -> String
///
/// Returns the configured replacement string (or the destination
/// encoding's default — `"�"` for UTF-* destinations, `"?"`
/// for everything else).
///
#[monoruby_builtin]
fn converter_replacement(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    if let Some(v) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_REPLACE_IVAR))
    {
        return Ok(v);
    }
    let dst = converter_get_dst(globals, recv);
    Ok(converter_default_replacement(dst))
}

///
/// ### Encoding::Converter#replacement=
/// - replacement = str -> str
///
/// Sets the replacement string. Must be a `String`; the bytes are
/// validated by transcoding through the destination encoder so
/// that calling `replacement = "..."` with characters
/// unrepresentable in the destination raises
/// `Encoding::UndefinedConversionError` instead of silently
/// stashing an unusable replacement.
///
#[monoruby_builtin]
fn converter_replacement_set(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let s = arg
        .is_str()
        .ok_or_else(|| MonorubyErr::typeerr(format!("no implicit conversion into String")))?;
    // Validate that the new replacement is encodable in dst.
    let recv = lfp.self_val();
    let dst = converter_get_dst(globals, recv);
    if let Some(dst_rs) = encoding_to_rs(dst) {
        let (_bytes, _, encode_err) = dst_rs.encode(s);
        if encode_err {
            return Err(MonorubyErr::undefined_conversion_error(
                &globals.store,
                format!(
                    "U+{:04X} from UTF-8 to {}",
                    s.chars()
                        .find(|c| !c.is_ascii())
                        .map(|c| c as u32)
                        .unwrap_or(0),
                    dst.name()
                ),
            ));
        }
    }
    // Tag the stored value with the destination's encoding so a
    // subsequent `replacement` call returns it correctly classified.
    let mut inner = crate::value::RStringInner::from_string_scanned(s.to_string());
    inner.set_encoding(dst);
    let val = Value::string_from_inner(inner);
    let _ = globals
        .store
        .set_ivar(recv, IdentId::get_id(CONVERTER_REPLACE_IVAR), val);
    Ok(arg)
}

///
/// ### Encoding::Converter#convert
/// - convert(string) -> String
///
/// Transcodes `string` from the converter's source encoding to its
/// destination encoding. The argument is reinterpreted under the
/// configured source encoding (CRuby's behaviour — its `encoding`
/// tag is ignored), and the result is tagged with the destination
/// encoding. Raises
/// `Encoding::InvalidByteSequenceError` /
/// `Encoding::UndefinedConversionError` on bad / unmappable input,
/// and `ArgumentError` after `#finish` has been called on this
/// converter.
///
#[monoruby_builtin]
fn converter_convert(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    if globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_FINISHED_IVAR))
        .is_some()
    {
        return Err(MonorubyErr::argumenterr(
            "convert called after finish".to_string(),
        ));
    }
    let arg = lfp.arg(0);
    let bytes = arg
        .is_rstring_inner()
        .ok_or_else(|| MonorubyErr::typeerr("expected String".to_string()))?
        .as_bytes()
        .to_vec();
    let src = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    // Plumb the configured replacement through the transcoder so a
    // user-set `replacement = "..."` is honoured by `convert`.
    let mut opts = TranscodeOpts::default();
    if let Some(v) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_REPLACE_IVAR))
        && let Some(s) = v.is_str()
    {
        opts.replace = Some(s.to_string());
    }
    let out = transcode_bytes_with_opts(&bytes, src, dst, &opts, &globals.store)?;
    Ok(Value::string_from_inner(
        crate::value::RStringInner::from_encoding_scanned(&out, dst),
    ))
}

///
/// ### Encoding::Converter#finish
/// - finish -> String
///
/// Marks the converter as drained and returns the trailing bytes
/// (always empty in monoruby — none of the `encoding_rs`
/// transcoders are stateful). Subsequent `convert` calls raise
/// `ArgumentError`.
///
#[monoruby_builtin]
fn converter_finish(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    let _ = globals.store.set_ivar(
        recv,
        IdentId::get_id(CONVERTER_FINISHED_IVAR),
        Value::bool(true),
    );
    let dst = converter_get_dst(globals, recv);
    Ok(Value::string_from_inner(
        crate::value::RStringInner::from_encoding_scanned(b"", dst),
    ))
}

///
/// ### Encoding::Converter#inspect
/// - inspect -> String
///
/// CRuby renders Converters as `#<Encoding::Converter: SRC to DST>`.
///
#[monoruby_builtin]
fn converter_inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    let src = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    Ok(Value::string(format!(
        "#<Encoding::Converter: {} to {}>",
        src.name(),
        dst.name()
    )))
}

///
/// ### Encoding::Converter#primitive_errinfo
/// - primitive_errinfo -> [Symbol, String, String, String, String]
///
/// CRuby returns the last `primitive_convert` status as a 5-tuple
/// `[result, src_enc, dst_enc, error_bytes, readagain_bytes]`.
/// monoruby's `convert` doesn't surface mid-stream state, so we
/// report `[:source_buffer_empty, "", "", "", ""]` — the value
/// CRuby returns when there's nothing pending.
///
#[monoruby_builtin]
fn converter_primitive_errinfo(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::array_from_iter(
        [
            Value::symbol_from_str("source_buffer_empty"),
            Value::string_from_str(""),
            Value::string_from_str(""),
            Value::string_from_str(""),
            Value::string_from_str(""),
        ]
        .into_iter(),
    ))
}

///
/// ### Encoding::Converter#last_error
/// - last_error -> Exception | nil
///
/// CRuby returns the most recent `Encoding::*Error` raised by
/// `primitive_convert`. monoruby's `convert` raises immediately
/// on errors instead of stashing them, so there's never a stored
/// last-error to report — return `nil`.
///
#[monoruby_builtin]
fn converter_last_error(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### Encoding::Converter#putback
/// - putback -> String
/// - putback(max_numbytes) -> String
///
/// CRuby returns the bytes left in the converter's input buffer
/// after a `primitive_convert` step. monoruby's `convert` is
/// single-shot, so the buffer is always drained — return an
/// empty BINARY string to match CRuby's "nothing left" answer.
///
#[monoruby_builtin]
fn converter_putback(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut s = crate::value::RStringInner::from_string_scanned(String::new());
    s.set_encoding(crate::value::Encoding::Ascii8);
    Ok(Value::string_from_inner(s))
}

///
/// ### Encoding::Converter#==
/// - == other -> bool
///
/// Two converters compare equal iff they share source and
/// destination encodings.
///
#[monoruby_builtin]
fn converter_eq(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    if rhs.class() != lhs.class() {
        return Ok(Value::bool(false));
    }
    let same = converter_get_src(globals, lhs) == converter_get_src(globals, rhs)
        && converter_get_dst(globals, lhs) == converter_get_dst(globals, rhs);
    Ok(Value::bool(same))
}

///
/// ### Encoding::Converter.asciicompat_encoding
/// - asciicompat_encoding(enc) -> Encoding | nil
///
/// Maps a non-ASCII-compatible / dummy encoding to its
/// ASCII-compatible counterpart (the encoding CRuby would internally
/// pivot through). Returns `nil` for encodings that are already
/// ASCII-compatible. monoruby covers the spec-exercised cases —
/// UTF-16/32 → UTF-8, ISO-2022-JP → stateless-ISO-2022-JP — and
/// returns `nil` for everything else (matching CRuby's default for
/// unknown / ASCII-compat inputs).
///
#[monoruby_builtin]
fn converter_asciicompat_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    // Accept Encoding objects as well as String / Symbol names. We
    // need the canonical name so we can dispatch on it.
    let enc_name: String = if arg.class() == encoding_class(globals) {
        globals
            .store
            .get_ivar(arg, IdentId::_ENCODING)
            .and_then(|v| v.is_str().map(|s| s.to_string()))
            .unwrap_or_default()
    } else {
        arg.coerce_to_string(vm, globals)?
    };
    let target = match enc_name.as_str() {
        "UTF-16BE" | "UTF-16LE" | "UTF-32BE" | "UTF-32LE" | "UTF-16" | "UTF-32" => Some("UTF_8"),
        "ISO-2022-JP" => Some("STATELESS_ISO_2022_JP"),
        _ => None,
    };
    let Some(const_name) = target else {
        return Ok(Value::nil());
    };
    let enc_class = encoding_class(globals);
    Ok(globals
        .store
        .get_constant_noautoload(enc_class, IdentId::get_id(const_name))
        .unwrap_or(Value::nil()))
}

///
/// ### Encoding::Converter.search_convpath
/// - search_convpath(src, dst) -> Array
///
/// Returns the list of encoding pairs the converter would walk
/// through. monoruby's transcoder is direct (decode src → UTF-8 →
/// encode dst, all in a single step), so for any supported pair
/// we report `[[src, dst]]`. Pairs `Encoding::Converter.new`
/// would reject raise `ConverterNotFoundError`, mirroring CRuby.
///
#[monoruby_builtin]
fn converter_search_convpath(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let src = resolve_dst_encoding(vm, globals, lfp.arg(0))?;
    let dst = resolve_dst_encoding(vm, globals, lfp.arg(1))?;
    validate_converter_pair(src, dst, &globals.store)?;
    let pair = Value::array2(encoding_value(globals, src), encoding_value(globals, dst));
    Ok(Value::array1(pair))
}

///
/// ### Encoding.default_internal=
/// - default_internal = enc -> enc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/default_internal=3d.html]
#[monoruby_builtin]
fn enc_set_default_internal(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = lfp.arg(0);
    // If a string is given, convert to an Encoding object via Encoding.find
    let enc_val = if val.is_nil() {
        Value::nil()
    } else if val.is_str().is_some() {
        let find_id = IdentId::get_id("find");
        let enc_class_val = lfp.self_val(); // Encoding class object
        vm.invoke_method_inner(globals, find_id, enc_class_val, &[val], None, None)?
    } else {
        val
    };
    globals.set_gvar(IdentId::get_id("$DEFAULT_INTERNAL"), enc_val);
    Ok(enc_val)
}

///
/// ### Encoding.locale_charmap
/// - locale_charmap -> String
///
/// Returns the locale's character map name. CRuby reads this from
/// the C locale via `nl_langinfo(CODESET)`. monoruby is locale-
/// agnostic so we return `"UTF-8"` — the de-facto default on
/// modern Linux desktops and the value `Encoding.find("locale")`
/// also resolves to.
///
#[monoruby_builtin]
fn enc_locale_charmap(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::string_from_str("UTF-8"))
}

// -------------------------------------------------------
// Instance-method helpers on Encoding::*Error subclasses
// -------------------------------------------------------

/// Read the exception's message via the standard `Exception#message`
/// pipeline. Errors raised by `transcode_bytes_with_opts` have
/// canonical message shapes that the `enc_err_*` accessors below
/// pattern-match against.
fn enc_err_message(globals: &Globals, exc: Value) -> Option<String> {
    exc.is_exception()
        .map(|inner| inner.message().to_string())
        .or_else(|| {
            globals
                .store
                .get_ivar(exc, IdentId::get_id("/message"))
                .and_then(|v| v.is_str().map(|s| s.to_string()))
        })
}

/// Extract `(src_enc_name, dst_enc_name)` from the canonical
/// monoruby-format error messages:
///
/// - `"U+XXXX from SRC to DST"` (UndefinedConversionError)
/// - `"\"\\xXX\" from SRC to DST"`
///   (BINARY → ASCII-compat dest UndefinedConversionError)
/// - `"invalid byte sequence on SRC (SRC → DST)"`
///   (InvalidByteSequenceError)
fn parse_enc_err_pair(msg: &str) -> Option<(String, String)> {
    if let Some(rest) = msg.split_once(" from ").map(|(_, b)| b)
        && let Some((src, rest)) = rest.split_once(" to ")
    {
        return Some((src.trim().to_string(), rest.trim().to_string()));
    }
    if let Some(open) = msg.find('(')
        && let Some(close) = msg.find(')')
        && open < close
    {
        let inner = &msg[open + 1..close];
        if let Some((src, dst)) = inner.split_once(" → ") {
            return Some((src.trim().to_string(), dst.trim().to_string()));
        }
        if let Some((src, dst)) = inner.split_once(" -> ") {
            return Some((src.trim().to_string(), dst.trim().to_string()));
        }
    }
    None
}

#[monoruby_builtin]
fn enc_err_source_encoding_name(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(msg) = enc_err_message(globals, lfp.self_val())
        && let Some((src, _)) = parse_enc_err_pair(&msg)
    {
        return Ok(Value::string(src));
    }
    Ok(Value::string_from_str(""))
}

#[monoruby_builtin]
fn enc_err_destination_encoding_name(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(msg) = enc_err_message(globals, lfp.self_val())
        && let Some((_, dst)) = parse_enc_err_pair(&msg)
    {
        return Ok(Value::string(dst));
    }
    Ok(Value::string_from_str(""))
}

#[monoruby_builtin]
fn enc_err_source_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(msg) = enc_err_message(globals, lfp.self_val())
        && let Some((src, _)) = parse_enc_err_pair(&msg)
        && let Ok(enc) = crate::value::Encoding::try_from_str(&src)
    {
        return Ok(encoding_value(globals, enc));
    }
    Ok(Value::nil())
}

#[monoruby_builtin]
fn enc_err_destination_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(msg) = enc_err_message(globals, lfp.self_val())
        && let Some((_, dst)) = parse_enc_err_pair(&msg)
        && let Ok(enc) = crate::value::Encoding::try_from_str(&dst)
    {
        return Ok(encoding_value(globals, enc));
    }
    Ok(Value::nil())
}

/// `Encoding::UndefinedConversionError#error_char` — the source
/// character that couldn't be represented in the destination.
/// Parses the leading `U+XXXX ` prefix monoruby formats into
/// `transcode_bytes_with_opts`'s message.
#[monoruby_builtin]
fn enc_err_error_char(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let Some(msg) = enc_err_message(globals, lfp.self_val()) else {
        return Ok(Value::string_from_str(""));
    };
    if let Some(rest) = msg.strip_prefix("U+") {
        let hex_end = rest.find(|c: char| !c.is_ascii_hexdigit()).unwrap_or(rest.len());
        let hex = &rest[..hex_end];
        if let Ok(cp) = u32::from_str_radix(hex, 16)
            && let Some(c) = char::from_u32(cp)
        {
            return Ok(Value::string(c.to_string()));
        }
    }
    Ok(Value::string_from_str(""))
}

/// `Encoding::InvalidByteSequenceError#incomplete_input?` —
/// whether the bad bytes were a (partial) leading prefix of a
/// valid sequence rather than an outright invalid byte. monoruby's
/// transcoder doesn't track partial-vs-complete; report `false`
/// to match the more conservative answer.
#[monoruby_builtin]
fn enc_err_incomplete_input_p(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(false))
}

///
/// ### Encoding.list
/// - list -> [Encoding]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/list.html]
#[monoruby_builtin]
fn enc_list(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let enc_class = encoding_class(globals);
    let utf8 = globals
        .store
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
        .unwrap_or(Value::nil());
    let ascii = globals
        .store
        .get_constant_noautoload(enc_class, IdentId::ASCII_8BIT)
        .unwrap_or(Value::nil());
    Ok(Value::array2(utf8, ascii))
}

///
/// ### Encoding.find
/// - find(name) -> Encoding
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/find.html]
#[monoruby_builtin]
fn enc_find(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let enc_class = encoding_class(globals);
    // CRuby's `Encoding.find` accepts either a String name (subject
    // to `to_str` coercion) *or* an existing `Encoding` object,
    // returning it unchanged. Without this short-circuit a value of
    // class `Encoding` would fail `coerce_to_string`'s TypeError.
    if arg0.class() == enc_class {
        return Ok(arg0);
    }
    let name = arg0.coerce_to_string(vm, globals)?;
    // Resolve special names first
    let const_name = enc_name_to_const(&name);
    let result = if let Some(c) = const_name {
        globals
            .store
            .get_constant_noautoload(enc_class, IdentId::get_id(c))
    } else {
        None
    };
    match result {
        Some(v) => Ok(v),
        None => Err(MonorubyErr::argumenterr(format!(
            "unknown encoding name - {}",
            name
        ))),
    }
}

/// Map an encoding name (as given by the user) to the Encoding constant name.
/// Returns None if the name is not recognized.
fn enc_name_to_const(name: &str) -> Option<&'static str> {
    // Normalize: uppercase, replace '-' with '_'
    let normalized = name.to_uppercase().replace('-', "_");
    match normalized.as_str() {
        // Special pseudo-encoding names
        "LOCALE" | "EXTERNAL" | "FILESYSTEM" => Some("UTF_8"),

        // UTF-8 (and aliases sharing the constant — `Encoding::CP65001`
        // is an alias of `Encoding::UTF_8`).
        "UTF_8" | "UTF8" | "CP65001" => Some("UTF_8"),

        // UTF-7 (dummy)
        "UTF_7" => Some("UTF_7"),

        // Emacs-Mule and other dummy ISO-2022-JP variants
        "EMACS_MULE" => Some("Emacs_Mule"),
        "CP50220" => Some("CP50220"),
        "CP50221" => Some("CP50221"),

        // ASCII-8BIT / BINARY
        "ASCII_8BIT" | "BINARY" => Some("ASCII_8BIT"),

        // US-ASCII
        "US_ASCII" | "ASCII" | "ANSI_X3.4_1968" | "646" => Some("US_ASCII"),

        // UTF-16
        "UTF_16" => Some("UTF_16"),
        "UTF_16BE" => Some("UTF_16BE"),
        "UTF_16LE" => Some("UTF_16LE"),

        // UTF-32
        "UTF_32" => Some("UTF_32"),
        "UTF_32BE" => Some("UTF_32BE"),
        "UTF_32LE" => Some("UTF_32LE"),

        // ISO-8859 family
        "ISO_8859_1" | "ISO8859_1" | "LATIN1" => Some("ISO_8859_1"),
        "ISO_8859_2" | "ISO8859_2" | "LATIN2" => Some("ISO_8859_2"),
        "ISO_8859_3" | "ISO8859_3" | "LATIN3" => Some("ISO_8859_3"),
        "ISO_8859_4" | "ISO8859_4" | "LATIN4" => Some("ISO_8859_4"),
        "ISO_8859_5" | "ISO8859_5" => Some("ISO_8859_5"),
        "ISO_8859_6" | "ISO8859_6" => Some("ISO_8859_6"),
        "ISO_8859_7" | "ISO8859_7" => Some("ISO_8859_7"),
        "ISO_8859_8" | "ISO8859_8" => Some("ISO_8859_8"),
        "ISO_8859_9" | "ISO8859_9" | "LATIN5" => Some("ISO_8859_9"),
        "ISO_8859_10" | "ISO8859_10" | "LATIN6" => Some("ISO_8859_10"),
        "ISO_8859_11" | "ISO8859_11" => Some("ISO_8859_11"),
        "ISO_8859_13" | "ISO8859_13" | "LATIN7" => Some("ISO_8859_13"),
        "ISO_8859_14" | "ISO8859_14" | "LATIN8" => Some("ISO_8859_14"),
        "ISO_8859_15" | "ISO8859_15" | "LATIN9" => Some("ISO_8859_15"),
        "ISO_8859_16" | "ISO8859_16" | "LATIN10" => Some("ISO_8859_16"),

        // Japanese encodings
        "EUC_JP" | "EUCJP" => Some("EUC_JP"),
        "SHIFT_JIS" | "SJIS" => Some("Shift_JIS"),
        "ISO_2022_JP" | "ISO2022_JP" => Some("ISO_2022_JP"),
        "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" => Some("Windows_31J"),
        "MACJAPANESE" | "MACJAPAN" => Some("MACJAPANESE"),
        // `eucJP-ms` is the canonical CRuby spelling; the lower-case
        // and partly-hyphenated user inputs (`euc-jp-ms`,
        // `eucjp-ms`, `EUCJP-MS`) all normalise to `EUC_JP_MS` /
        // `EUCJP_MS` here, so cover both.
        "EUCJP_MS" | "EUCJP_WIN" | "EUC_JP_MS" | "EUC_JP_WIN" => Some("EUCJP_MS"),
        "CP51932" => Some("CP51932"),
        "STATELESS_ISO_2022_JP" => Some("STATELESS_ISO_2022_JP"),
        // `UTF8-MAC` is CRuby's identifier for HFS+ NFD-normalised
        // UTF-8 (used on macOS filesystems); we don't actually do
        // the NFD trick but the constant has to exist for spec
        // setup like `Encoding::UTF8_MAC` to resolve.
        "UTF8_MAC" | "UTF_8_MAC" => Some("UTF8_MAC"),
        "CESU_8" | "CESU8" => Some("CESU_8"),

        // Windows code pages
        "WINDOWS_1250" | "CP1250" => Some("Windows_1250"),
        "WINDOWS_1251" | "CP1251" => Some("Windows_1251"),
        "WINDOWS_1252" | "CP1252" => Some("Windows_1252"),
        "WINDOWS_1253" | "CP1253" => Some("Windows_1253"),
        "WINDOWS_1254" | "CP1254" => Some("Windows_1254"),
        "WINDOWS_1255" | "CP1255" => Some("Windows_1255"),
        "WINDOWS_1256" | "CP1256" => Some("Windows_1256"),
        "WINDOWS_1257" | "CP1257" => Some("Windows_1257"),
        "WINDOWS_1258" | "CP1258" => Some("Windows_1258"),

        // IBM code pages
        "IBM437" | "CP437" => Some("IBM437"),
        "IBM737" | "CP737" => Some("IBM737"),
        "IBM775" | "CP775" => Some("IBM775"),
        "IBM850" | "CP850" => Some("IBM850"),
        "IBM852" | "CP852" => Some("IBM852"),
        "IBM855" | "CP855" => Some("IBM855"),
        "IBM857" | "CP857" => Some("IBM857"),
        "IBM860" | "CP860" => Some("IBM860"),
        "IBM861" | "CP861" => Some("IBM861"),
        "IBM862" | "CP862" => Some("IBM862"),
        "IBM863" | "CP863" => Some("IBM863"),
        "IBM864" | "CP864" => Some("IBM864"),
        "IBM865" | "CP865" => Some("IBM865"),
        "IBM866" | "CP866" => Some("IBM866"),
        "IBM869" | "CP869" => Some("IBM869"),

        // KOI8
        "KOI8_R" => Some("KOI8_R"),
        "KOI8_U" => Some("KOI8_U"),

        // Chinese encodings
        "GB2312" | "EUC_CN" => Some("GB2312"),
        "GBK" | "CP936" => Some("GBK"),
        "GB18030" => Some("GB18030"),
        "BIG5" | "BIG5_HKSCS" => Some("Big5"),

        // Korean encodings
        "EUC_KR" | "EUCKR" => Some("EUC_KR"),
        "CP949" => Some("CP949"),

        // Other
        "EUC_TW" | "EUCTW" => Some("EUC_TW"),
        "TIS_620" | "TIS620" => Some("TIS_620"),

        // Mac-family encoding aliases. CRuby exposes these with
        // the `Mac…` mixed-case prefix; the user-facing string
        // names go through this lookup with hyphens already
        // collapsed to underscores by the `normalized` step
        // above, so a single uppercase arm covers all spellings.
        "MACCYRILLIC" => Some("MacCyrillic"),
        "MACGREEK" => Some("MacGreek"),
        "MACICELAND" => Some("MacIceland"),
        "MACROMAN" => Some("MacRoman"),
        "MACROMANIA" => Some("MacRomania"),
        "MACTHAI" => Some("MacThai"),
        "MACTURKISH" => Some("MacTurkish"),
        "MACUKRAINE" => Some("MacUkraine"),

        _ => None,
    }
}

///
/// ### Encoding.aliases
/// - aliases -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/aliases.html]
#[monoruby_builtin]
fn enc_aliases(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut map = RubyMap::default();
    let aliases: &[(&str, &str)] = &[
        ("BINARY", "ASCII-8BIT"),
        ("ASCII", "US-ASCII"),
        ("ANSI_X3.4-1968", "US-ASCII"),
        ("646", "US-ASCII"),
        ("UTF8", "UTF-8"),
        ("CP65001", "UTF-8"),
        ("locale", "UTF-8"),
        ("external", "UTF-8"),
        ("filesystem", "UTF-8"),
        ("CP932", "Windows-31J"),
        ("csWindows31J", "Windows-31J"),
        ("SJIS", "Shift_JIS"),
        ("eucJP", "EUC-JP"),
        ("CP51932", "CP51932"),
        ("eucjp-ms", "eucJP-ms"),
        ("euc-jp-ms", "eucJP-ms"),
        ("EUC-CN", "GB2312"),
        ("CP936", "GBK"),
        ("CP949", "CP949"),
        ("CP1250", "Windows-1250"),
        ("CP1251", "Windows-1251"),
        ("CP1252", "Windows-1252"),
        ("CP1253", "Windows-1253"),
        ("CP1254", "Windows-1254"),
        ("CP1255", "Windows-1255"),
        ("CP1256", "Windows-1256"),
        ("CP1257", "Windows-1257"),
        ("CP1258", "Windows-1258"),
    ];
    for (alias, name) in aliases {
        map.insert(
            Value::string_from_str(alias),
            Value::string_from_str(name),
            vm,
            globals,
        )?;
    }
    Ok(Value::hash(map))
}

/// The static set of canonical encoding names plus their aliases.
/// Used by `Encoding.name_list` and `Encoding#names`. Each tuple is
/// `(canonical, &[aliases])`.
const ENCODING_NAMES: &[(&str, &[&str])] = &[
    ("ASCII-8BIT", &["BINARY"]),
    ("UTF-8", &["CP65001", "locale", "external", "filesystem"]),
    ("US-ASCII", &["ASCII", "ANSI_X3.4-1968", "646"]),
    ("UTF-16BE", &[]),
    ("UTF-16LE", &[]),
    ("UTF-16", &[]),
    ("UTF-32BE", &[]),
    ("UTF-32LE", &[]),
    ("UTF-32", &[]),
    ("ISO-8859-1", &[]),
    ("ISO-8859-2", &[]),
    ("ISO-8859-3", &[]),
    ("ISO-8859-4", &[]),
    ("ISO-8859-5", &[]),
    ("ISO-8859-6", &[]),
    ("ISO-8859-7", &[]),
    ("ISO-8859-8", &[]),
    ("ISO-8859-9", &[]),
    ("ISO-8859-10", &[]),
    ("ISO-8859-11", &[]),
    ("ISO-8859-13", &[]),
    ("ISO-8859-14", &[]),
    ("ISO-8859-15", &[]),
    ("ISO-8859-16", &[]),
    ("Shift_JIS", &["SJIS"]),
    ("Windows-31J", &["CP932", "csWindows31J"]),
    ("EUC-JP", &["eucJP"]),
    ("ISO-2022-JP", &[]),
    ("Windows-1250", &["CP1250"]),
    ("Windows-1251", &["CP1251"]),
    ("Windows-1252", &["CP1252"]),
    ("Windows-1253", &["CP1253"]),
    ("Windows-1254", &["CP1254"]),
    ("Windows-1255", &["CP1255"]),
    ("Windows-1256", &["CP1256"]),
    ("Windows-1257", &["CP1257"]),
    ("Windows-1258", &["CP1258"]),
    ("KOI8-R", &[]),
    ("KOI8-U", &[]),
    ("GB2312", &["EUC-CN"]),
    ("GBK", &["CP936"]),
    ("GB18030", &[]),
    ("Big5", &[]),
    ("EUC-KR", &[]),
    ("EUC-TW", &[]),
    ("CP949", &[]),
    ("TIS-620", &[]),
    ("MacJapanese", &[]),
    ("eucJP-ms", &["eucjp-ms", "euc-jp-ms"]),
    ("CP51932", &[]),
    ("stateless-ISO-2022-JP", &[]),
    ("CESU-8", &[]),
    ("UTF-7", &[]),
    ("Emacs-Mule", &[]),
    ("CP50220", &[]),
    ("CP50221", &[]),
];

///
/// ### Encoding.name_list
/// - name_list -> [String]
///
/// Returns the list of all encoding names plus all aliases.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/name_list.html]
#[monoruby_builtin]
fn enc_name_list(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut names: Vec<Value> = Vec::new();
    for (canonical, aliases) in ENCODING_NAMES {
        names.push(Value::string_from_str(canonical));
        for alias in *aliases {
            names.push(Value::string_from_str(alias));
        }
    }
    Ok(Value::array_from_iter(names.into_iter()))
}

///
/// ### Encoding#names
/// - names -> [String]
///
/// Returns the list of all canonical names and aliases that refer to
/// this encoding.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/i/names.html]
#[monoruby_builtin]
fn enc_names(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let canonical = match globals.store.get_ivar(self_, IdentId::_ENCODING) {
        Some(v) => v.as_str().to_string(),
        None => return Ok(Value::array_empty()),
    };
    // Find the matching row by canonical name and collect every
    // (canonical, alias) that points here.
    let mut names: Vec<Value> = vec![Value::string_from_str(&canonical)];
    for (c, aliases) in ENCODING_NAMES {
        if c.eq_ignore_ascii_case(&canonical) {
            for alias in *aliases {
                names.push(Value::string_from_str(alias));
            }
            break;
        }
    }
    Ok(Value::array_from_iter(names.into_iter()))
}

///
/// ### Encoding.compatible?
/// - compatible?(obj1, obj2) -> Encoding | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/compatible=3f.html]
#[monoruby_builtin]
fn enc_compatible(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let result = if let (Some(a), Some(b)) =
        (lfp.arg(0).is_rstring_inner(), lfp.arg(1).is_rstring_inner())
    {
        // String / String: route through the inner-aware
        // `compatible_encoding` so the empty-side rules
        // (`compatible?("", x)` adopts `x`'s encoding even when
        // `x.encoding` isn't ASCII-compatible) are honoured.
        // `Encoding::compatible` alone takes only the
        // `(encoding, code_range)` pair and can't distinguish
        // empty from "ASCII-only but non-empty".
        a.compatible_encoding(b)
    } else {
        let lhs = resolve_compat_arg(globals, lfp.arg(0));
        let rhs = resolve_compat_arg(globals, lfp.arg(1));
        let (a_enc, a_cr) = match lhs {
            Some(p) => p,
            None => return Ok(Value::nil()),
        };
        let (b_enc, b_cr) = match rhs {
            Some(p) => p,
            None => return Ok(Value::nil()),
        };
        Encoding::compatible(a_enc, a_cr, b_enc, b_cr)
    };
    match result {
        Some(enc) => {
            let const_name = encoding_constant_name(enc);
            let enc_class = encoding_class(globals);
            Ok(globals
                .store
                .get_constant_noautoload(enc_class, IdentId::get_id(const_name))
                .unwrap_or(Value::nil()))
        }
        None => Ok(Value::nil()),
    }
}

/// Resolve a value (`String` or `Encoding` object) to the
/// `(encoding, code_range)` pair `Encoding::compatible` consumes.
/// Other types return `None`, which the caller treats as
/// "incompatible".
fn resolve_compat_arg(globals: &Globals, v: Value) -> Option<(Encoding, CodeRange)> {
    if let Some(s) = v.is_rstring_inner() {
        return Some((s.encoding(), s.code_range()));
    }
    if v.class() == encoding_class(globals) {
        let name = globals.store.get_ivar(v, IdentId::_ENCODING)?;
        let s = name.is_str()?;
        let enc = Encoding::try_from_str(s).ok()?;
        // Pure Encoding object — pretend the byte stream is empty,
        // so the code range is SevenBit (compatible with anything
        // ASCII-compatible).
        return Some((enc, CodeRange::SevenBit));
    }
    None
}

///
/// ### Encoding#to_s / Encoding#name
/// - to_s -> String
/// - name -> String
///
#[monoruby_builtin]
fn enc_to_s(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    match globals.store.get_ivar(self_, IdentId::_ENCODING) {
        Some(v) => Ok(v),
        None => Ok(Value::string_from_str("UTF-8")),
    }
}

///
/// ### Encoding#inspect
/// - inspect -> String
///
#[monoruby_builtin]
fn enc_inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    // `#<Encoding:NAME>` form for non-dummy encodings, with a
    // `(dummy)` suffix for the encodings CRuby flags as dummy.
    // Ruby 3.4+ renders ASCII-8BIT as `BINARY (ASCII-8BIT)` since
    // BINARY became the canonical name.
    let name = match globals.store.get_ivar(self_, IdentId::_ENCODING) {
        Some(v) => v.as_str().to_string(),
        None => "UTF-8".to_string(),
    };
    if name == "ASCII-8BIT" {
        return Ok(Value::string_from_str("#<Encoding:BINARY (ASCII-8BIT)>"));
    }
    let suffix = if is_cruby_dummy_name(&name) { " (dummy)" } else { "" };
    Ok(Value::string(format!("#<Encoding:{name}{suffix}>")))
}

/// Encoding names that CRuby flags as "dummy" — registered but not
/// natively decoded. monoruby's broader `Encoding::is_dummy` covers
/// "we don't decode", which is too eager (ISO-8859 / EUC-JP / SJIS
/// have CRuby decoders even if monoruby doesn't). For
/// `Encoding#dummy?` and `Encoding#inspect` we use this narrower
/// match to match CRuby observed behaviour.
fn is_cruby_dummy_name(name: &str) -> bool {
    let normalized = name.to_uppercase().replace('-', "_");
    matches!(
        normalized.as_str(),
        "UTF_7"
            | "UTF_16"
            | "UTF_32"
            | "EMACS_MULE"
            | "CP50220"
            | "CP50221"
            | "ISO_2022_JP"
            | "STATELESS_ISO_2022_JP"
            | "CESU_8"
    )
}

///
/// ### Encoding#ascii_compatible?
/// - ascii_compatible? -> bool
///
/// Returns true for encodings whose encoded forms are a superset of ASCII.
///
#[monoruby_builtin]
fn enc_ascii_compatible_p(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let name = globals
        .store
        .get_ivar(self_, IdentId::_ENCODING)
        .and_then(|v| v.is_str().map(|s| s.to_string()))
        .unwrap_or_default();
    Ok(Value::bool(is_ascii_compatible_encoding(&name)))
}

///
/// ### Encoding#dummy?
/// - dummy? -> bool
///
#[monoruby_builtin]
fn enc_dummy_p(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let name = globals
        .store
        .get_ivar(self_, IdentId::_ENCODING)
        .and_then(|v| v.is_str().map(|s| s.to_string()))
        .unwrap_or_default();
    Ok(Value::bool(is_dummy_encoding(&name)))
}

fn is_ascii_compatible_encoding(name: &str) -> bool {
    !matches!(
        name,
        "UTF-16"
            | "UTF-32"
            | "UTF-16BE"
            | "UTF-16LE"
            | "UTF-32BE"
            | "UTF-32LE"
            | "ISO-2022-JP"
            | "STATELESS-ISO-2022-JP"
            | "CP50220"
            | "CP50221"
            | "UTF-7"
    )
}

fn is_dummy_encoding(name: &str) -> bool {
    // Delegate to the canonical helper used by `Encoding#inspect`
    // so the two views agree.
    is_cruby_dummy_name(name)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn force_encoding() {
        run_test(r#""Ruby".force_encoding("ASCII-8BIT")"#);
        run_test(r#""Ruby".force_encoding("UTF-8")"#);
        run_test(r#""Ruby".force_encoding(Encoding::UTF_8)"#);
        run_test(r#""Ruby".force_encoding(Encoding::ASCII_8BIT)"#);
        run_test_error(r#""Ruby".force_encoding(:ASCII)"#);
    }

    #[test]
    fn force_encoding_preserves_dummy_names() {
        // The new (Phase 1) encoding tags round-trip through
        // `force_encoding`/`#encoding`.
        run_test(r#""abc".force_encoding("UTF-16LE").encoding == Encoding::UTF_16LE"#);
        run_test(r#""abc".force_encoding("UTF-16BE").encoding == Encoding::UTF_16BE"#);
        run_test(r#""abc".force_encoding("ISO-8859-1").encoding == Encoding::ISO_8859_1"#);
        run_test(r#""abc".force_encoding("ISO-8859-15").encoding == Encoding::ISO_8859_15"#);
        run_test(r#""abc".force_encoding("EUC-JP").encoding == Encoding::EUC_JP"#);
        run_test(r#""abc".force_encoding("Windows-31J").encoding == Encoding::Windows_31J"#);
    }

    #[test]
    fn ascii_only() {
        run_test(r#"'abc123'.ascii_only?"#);
        run_test(r#"''.ascii_only?"#);
        run_test(r#"'日本語'.ascii_only?"#);
        run_test(r#"'日本語abc123'.ascii_only?"#);
    }

    #[test]
    fn valid_encoding_for_broken_utf8() {
        // `force_encoding("UTF-8")` on bytes that aren't valid UTF-8
        // can now be observed via `valid_encoding?`. Previously the
        // tag silently asserted validity.
        run_test(r#"[0xff].pack("C").force_encoding("UTF-8").valid_encoding?"#);
        run_test(r#""abc".force_encoding("UTF-8").valid_encoding?"#);
        // UTF-16 needs an even byte count to validate as a code-unit
        // sequence.
        run_test(r#""abc".force_encoding("UTF-16LE").valid_encoding?"#);
        run_test(r#""ab".force_encoding("UTF-16LE").valid_encoding?"#);
    }

    #[test]
    fn encoding_compatible_basic() {
        run_test(r#"Encoding.compatible?("abc", "def".encode("US-ASCII")) == Encoding::US_ASCII"#);
        // 7-bit US-ASCII is compatible with any ASCII-compatible
        // encoding; result encoding is the non-7-bit side.
        run_test(
            r#"Encoding.compatible?("abc".force_encoding("US-ASCII"), "\xff") == Encoding::ASCII_8BIT"#,
        );
        // Two distinct non-ASCII-only encodings → nil.
        run_test(
            r#"Encoding.compatible?("\xff".force_encoding("UTF-8"), "\xff".force_encoding("ASCII-8BIT"))"#,
        );
    }

    #[test]
    fn encoding_compatible_left_wins_for_two_seven_bit() {
        // Both ASCII-compatible AND both 7-bit → the *first*
        // (left-side) encoding wins, matching CRuby's
        // `rb_enc_compatible`.
        run_test(
            r#"
              utf8 = "abc".force_encoding("UTF-8")
              ascii = "def".force_encoding("US-ASCII")
              left = Encoding.compatible?(utf8, ascii) == Encoding::UTF_8
              right = Encoding.compatible?(ascii, utf8) == Encoding::US_ASCII
              [left, right]
            "#,
        );
    }

    #[test]
    fn encoding_compatible_iso_and_ascii() {
        // 7-bit ASCII-compatible string + ISO-8859 with non-ASCII
        // → ISO-8859 wins.
        run_test(
            r#"
              ascii = "abc"
              iso = "\xff".force_encoding("ISO-8859-1")
              Encoding.compatible?(ascii, iso) == Encoding::ISO_8859_1
            "#,
        );
    }

    #[test]
    fn encoding_compatible_self_with_self() {
        // Same encoding always compatible, returns that encoding.
        run_test(r#"Encoding.compatible?("abc", "def") == Encoding::UTF_8"#);
        run_test(
            r#"Encoding.compatible?("\xff".force_encoding("UTF-8"), "\xfe".force_encoding("UTF-8")) == Encoding::UTF_8"#,
        );
    }

    #[test]
    fn encoding_compatible_returns_nil_for_unsupported_inputs() {
        // `nil` / Integer arguments are not strings or Encoding
        // objects, so the helper returns `nil`. (CRuby additionally
        // accepts Symbols / Regexps / IOs; Phase 1 keeps the
        // Symbol/Regexp/IO paths unimplemented and they fall
        // through to nil too.)
        run_test(r#"Encoding.compatible?("abc", nil).nil?"#);
        run_test(r#"Encoding.compatible?("abc", 42).nil?"#);
    }

    #[test]
    fn force_encoding_round_trips_iso8859_variants() {
        // Each ISO-8859-N variant has its own constant and
        // `force_encoding` round-trips through `#encoding`.
        for n in [1, 2, 5, 9, 13, 15, 16] {
            let src = format!(
                r#""abc".force_encoding("ISO-8859-{n}").encoding == Encoding::ISO_8859_{n}"#
            );
            run_test(&src);
        }
    }

    #[test]
    fn force_encoding_round_trips_utf32() {
        run_test(r#""abcd".force_encoding("UTF-32LE").encoding == Encoding::UTF_32LE"#);
        run_test(r#""abcd".force_encoding("UTF-32BE").encoding == Encoding::UTF_32BE"#);
    }

    #[test]
    fn force_encoding_via_encoding_object() {
        // Accept an `Encoding` object as the argument, not just a
        // String.
        run_test(
            r#""Ruby".force_encoding(Encoding::ISO_8859_1).encoding == Encoding::ISO_8859_1"#,
        );
        run_test(
            r#""Ruby".force_encoding(Encoding::UTF_16LE).encoding == Encoding::UTF_16LE"#,
        );
    }

    #[test]
    fn force_encoding_unknown_name_raises() {
        run_test_error(r#""abc".force_encoding("Bogus-7")"#);
        run_test_error(r#""abc".force_encoding("UTF-99")"#);
    }

    #[test]
    fn encoding_methods_for_dummy_variants() {
        // `Encoding#name` reports the canonical CRuby name for
        // each variant.
        run_test(r#"Encoding::UTF_16LE.name"#);
        run_test(r#"Encoding::ISO_8859_5.name"#);
        run_test(r#"Encoding::EUC_JP.name"#);
        run_test(r#"Encoding::Windows_31J.name"#);
        // `ascii_compatible?` is false for the UTF-16/32 family,
        // true for the ASCII-compatible ones.
        run_test(r#"Encoding::UTF_16LE.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_32BE.ascii_compatible?"#);
        run_test(r#"Encoding::ISO_8859_1.ascii_compatible?"#);
        run_test(r#"Encoding::EUC_JP.ascii_compatible?"#);
    }

    #[test]
    fn ascii_only_after_force_encoding_to_us_ascii() {
        // After `force_encoding("US-ASCII")`, a high byte makes the
        // string non-ASCII-only AND invalid.
        run_test(
            r#"
              s = "\xff".force_encoding("US-ASCII")
              [s.ascii_only?, s.valid_encoding?]
            "#,
        );
    }

    #[test]
    fn cr_cache_invalidates_on_set_byte() {
        // Mutating a single byte via `setbyte` flips the cached
        // classification — `valid_encoding?` should reflect the new
        // bytes on the next call (the test would expose the cache
        // not being cleared).
        run_test(
            r#"
              s = "abc"
              before = s.valid_encoding?
              s.setbyte(0, 0xff)
              [before, s.valid_encoding?]
            "#,
        );
    }

    #[test]
    fn cr_cache_invalidates_on_concat() {
        // Two broken halves can combine into a valid scalar.
        // CRuby reports the resulting string as `valid_encoding?
        // == true`, which only works if the cache is cleared on
        // every `<<` / `+`.
        run_test(
            r#"
              a = [0xC3].pack("C").force_encoding("UTF-8")
              b = [0xA9].pack("C").force_encoding("UTF-8")
              [a.valid_encoding?, b.valid_encoding?, (a + b).valid_encoding?]
            "#,
        );
    }

    #[test]
    fn cr_cache_invalidates_on_force_encoding() {
        // The same bytes can be SevenBit under one encoding and
        // Broken under another — the cache must clear when the
        // tag changes.
        run_test(
            r#"
              s = "abc"
              first = s.valid_encoding?
              s.force_encoding("UTF-16LE")
              [first, s.valid_encoding?]
            "#,
        );
    }

    #[test]
    fn marshal_round_trips_dummy_encoded_bytes() {
        // Marshal-dumping a string whose declared encoding is a
        // dummy (UTF-16LE here) writes the bytes opaquely; the
        // round-trip recovers the bytes (the encoding tag isn't
        // preserved because monoruby doesn't currently emit a
        // `:encoding` ivar for non-UTF-8 strings).
        run_test(
            r#"
              s = "abcd".force_encoding("UTF-16LE")
              loaded = Marshal.load(Marshal.dump(s))
              [loaded.bytes, loaded.bytesize]
            "#,
        );
    }

    #[test]
    fn string_concat_raises_compat_error() {
        // Two distinct broken sides cannot be concatenated.
        run_test_error(
            r#""\xff".force_encoding("UTF-8") + "\xff".force_encoding("ASCII-8BIT")"#,
        );
        run_test_error(
            r#"
              s = "\xff".force_encoding("UTF-8")
              s << "\xff".force_encoding("ASCII-8BIT")
            "#,
        );
    }

    #[test]
    fn string_concat_empty_adopts_other_encoding() {
        // Empty side adopts the other side's encoding (matters for
        // non-ASCII-compatible encodings).
        run_test(
            r#"("".force_encoding("UTF-16LE") + "abc").encoding == Encoding::UTF_8"#,
        );
        run_test(
            r#"
              s = "".force_encoding("UTF-16LE")
              s << "abc"
              s.encoding == Encoding::UTF_8
            "#,
        );
    }

    #[test]
    fn gsub_raises_compat_error_on_replacement() {
        // Receiver is UTF-8 with non-ASCII content, replacement is
        // an ASCII-8BIT broken byte → CompatibilityError.
        run_test_error(
            r#""é".gsub(/é/, "\xff".force_encoding("ASCII-8BIT"))"#,
        );
    }

    #[test]
    fn index_assign_raises_compat_error_on_replacement() {
        run_test_error(
            r#"
              s = "é"
              s[0] = "\xff".force_encoding("ASCII-8BIT")
            "#,
        );
    }

    #[test]
    fn length_for_dummy_encodings() {
        // UTF-16: count code units (surrogate pairs count as 2).
        run_test(r#""ab".force_encoding("UTF-16LE").length"#);
        run_test(r#""abc".force_encoding("UTF-16LE").length"#); // 1.5 + 1 (broken trailing)
        // UTF-32: 1 char per 4 bytes.
        run_test(r#""abcd".force_encoding("UTF-32LE").length"#);
        // ISO-8859-N: 1 byte per char.
        run_test(r#""\xff\xfe".force_encoding("ISO-8859-1").length"#);
    }

    #[test]
    fn length_for_broken_utf8() {
        // Broken UTF-8: each invalid byte counts as one char.
        run_test(r#""\xF4\x90\x80\x80".length"#);
        run_test(r#""a\xF4\x90\x80\x80b".length"#);
        run_test(r#""é\xF4\x90\x80\x80è".length"#);
    }

    #[test]
    fn chars_preserves_encoding() {
        // Each yielded character carries the source encoding.
        run_test(
            r#""abc".force_encoding("ASCII-8BIT").chars.all? { |c| c.encoding == Encoding::ASCII_8BIT }"#,
        );
        run_test(
            r#""ab".force_encoding("ISO-8859-1").chars.map(&:encoding) == [Encoding::ISO_8859_1, Encoding::ISO_8859_1]"#,
        );
    }

    #[test]
    fn encoding_default_external() {
        run_test_no_result_check(
            r#"
            enc = Encoding.default_external
            raise "should be Encoding" unless enc.is_a?(Encoding)
            "#,
        );
    }

    #[test]
    fn encoding_default_internal() {
        run_test(r#"Encoding.default_internal"#);
        // setter and getter round-trip
        run_test(r#"
            Encoding.default_internal = Encoding::UTF_8
            res = Encoding.default_internal == Encoding::UTF_8
            Encoding.default_internal = nil
            res
        "#);
        run_test(r#"
            Encoding.default_internal = nil
            Encoding.default_internal
        "#);
        // string argument to setter
        run_test(r#"
            Encoding.default_internal = "UTF-8"
            res = Encoding.default_internal == Encoding::UTF_8
            Encoding.default_internal = nil
            res
        "#);
    }

    #[test]
    fn encoding_list() {
        run_test(
            r#"
            list = Encoding.list
            list.is_a?(Array)
            "#,
        );
    }

    #[test]
    fn encoding_find() {
        run_test_no_result_check(
            r#"
            raise unless Encoding.find("UTF-8").is_a?(Encoding)
            raise unless Encoding.find("ASCII-8BIT").is_a?(Encoding)
            raise unless Encoding.find("US-ASCII").is_a?(Encoding)
            raise unless Encoding.find("BINARY").is_a?(Encoding)
            raise unless Encoding.find("ASCII").is_a?(Encoding)
            raise unless Encoding.find("locale").is_a?(Encoding)
            raise unless Encoding.find("Shift_JIS").is_a?(Encoding)
            raise unless Encoding.find("ISO-8859-1").is_a?(Encoding)
            raise unless Encoding.find("EUC-JP").is_a?(Encoding)
            "#,
        );
    }

    #[test]
    fn encoding_aliases() {
        run_test(
            r#"
            Encoding.aliases.is_a?(Hash)
            "#,
        );
    }

    #[test]
    fn string_encode() {
        run_test(r#""hello".encode("UTF-8")"#);
        run_test(r#""hello".encode("US-ASCII")"#);
        run_test(r#""hello".encode("UTF-8").encoding.name"#);
    }

    #[test]
    fn string_encode_bang() {
        run_test(r#"s = "hello"; s.encode!("UTF-8"); s.encoding.name"#);
    }

    #[test]
    fn string_encode_changes_encoding_tag() {
        // PR #361: `String#encode` (Ruby override + Rust encode) updates
        // the encoding tag of the result instead of returning self verbatim.
        run_tests(&[
            r#""hello".encode("US-ASCII").encoding.name"#,
            r#""hello".encode("ASCII-8BIT").encoding.name"#,
            r#""hello".encode(Encoding::US_ASCII).encoding.name"#,
            // Original is untouched; encode returns a copy.
            r#"s = "hello"; s.encode("US-ASCII"); s.encoding.name"#,
        ]);
    }

    #[test]
    fn encoding_compatible() {
        run_test(
            r#"
            Encoding.compatible?("a", "b").nil?.!
            "#,
        );
    }

    #[test]
    fn warning_module() {
        // Warning[] returns category status
        run_test("Warning[:deprecated]");
        run_test("Warning[:experimental]");
        run_test("Warning[:performance]");
        // Warning[]= sets category
        run_test(r#"
            old = Warning[:deprecated]
            Warning[:deprecated] = false
            res = Warning[:deprecated]
            Warning[:deprecated] = old
            res
        "#);
        // Invalid category raises ArgumentError
        run_test_error("Warning[:nonexistent]");
    }

    #[test]
    fn compatibility_error_class() {
        // Encoding::CompatibilityError exists and inherits from EncodingError
        run_test("Encoding::CompatibilityError.is_a?(Class)");
        run_test("Encoding::CompatibilityError < EncodingError");
    }

    #[test]
    fn encoding_ascii_compatible_p() {
        // ASCII-compatible encodings
        run_test(r#"Encoding::UTF_8.ascii_compatible?"#);
        run_test(r#"Encoding::US_ASCII.ascii_compatible?"#);
        run_test(r#"Encoding::ASCII_8BIT.ascii_compatible?"#);
        run_test(r#"Encoding::ISO_8859_1.ascii_compatible?"#);
        run_test(r#"Encoding::Shift_JIS.ascii_compatible?"#);
        run_test(r#"Encoding::EUC_JP.ascii_compatible?"#);
        // Non-ASCII-compatible encodings
        run_test(r#"Encoding::UTF_16.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_16BE.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_16LE.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_32.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_32BE.ascii_compatible?"#);
        run_test(r#"Encoding::UTF_32LE.ascii_compatible?"#);
    }

    #[test]
    fn encoding_dummy_p() {
        // Non-dummy encodings
        run_test(r#"Encoding::UTF_8.dummy?"#);
        run_test(r#"Encoding::US_ASCII.dummy?"#);
        run_test(r#"Encoding::ASCII_8BIT.dummy?"#);
        run_test(r#"Encoding::UTF_16BE.dummy?"#);
        run_test(r#"Encoding::UTF_16LE.dummy?"#);
        run_test(r#"Encoding::Shift_JIS.dummy?"#);
        // Dummy encodings (stateful / no-BOM UTF-16/32)
        run_test(r#"Encoding::UTF_16.dummy?"#);
        run_test(r#"Encoding::UTF_32.dummy?"#);
        run_test(r#"Encoding::ISO_2022_JP.dummy?"#);
    }

    #[test]
    fn encoding_extra_dummy_constants() {
        // Defined and reachable as Encoding objects (CRuby uses
        // mixed-case canonical identifiers for the dummy / mac-family
        // names — this is the literal constant identifier).
        run_test(r#"Encoding::UTF_7.is_a?(Encoding)"#);
        run_test(r#"Encoding::Emacs_Mule.is_a?(Encoding)"#);
        run_test(r#"Encoding::CP50220.is_a?(Encoding)"#);
        run_test(r#"Encoding::CP50221.is_a?(Encoding)"#);
        // Their `dummy?` is true (CRuby-strict set).
        run_test(r#"Encoding::UTF_7.dummy?"#);
        run_test(r#"Encoding::CP50220.dummy?"#);
        // Mac-family aliases reachable, dummy? false.
        run_test(r#"Encoding::MacCyrillic.is_a?(Encoding)"#);
        run_test(r#"Encoding::MacGreek.is_a?(Encoding)"#);
        run_test(r#"Encoding::MacRoman.is_a?(Encoding)"#);
        run_test(r#"Encoding::MacTurkish.dummy?"#);
        run_test(r#"Encoding::Big5_HKSCS.is_a?(Encoding)"#);
    }

    #[test]
    fn encoding_object_identity_aliases() {
        // ASCII <-> US_ASCII share the same Value (object identity).
        run_test(r#"Encoding::ASCII.equal?(Encoding::US_ASCII)"#);
        run_test(r#"Encoding::ASCII == Encoding::US_ASCII"#);
        // CP65001 <-> UTF_8 share the same Value.
        run_test(r#"Encoding::CP65001.equal?(Encoding::UTF_8)"#);
        run_test(r#"Encoding::CP65001 == Encoding::UTF_8"#);
        // BINARY <-> ASCII_8BIT (existing behaviour, asserted for parity).
        run_test(r#"Encoding::BINARY.equal?(Encoding::ASCII_8BIT)"#);
    }

    #[test]
    fn encoding_find_accepts_encoding_object() {
        // Pre-existing: Encoding.find with a String name.
        run_test(r#"Encoding.find("UTF-8").is_a?(Encoding)"#);
        // New: Encoding.find with an Encoding object returns it as-is.
        run_test(r#"Encoding.find(Encoding::UTF_8).equal?(Encoding::UTF_8)"#);
        run_test(r#"Encoding.find(Encoding::ASCII_8BIT).equal?(Encoding::ASCII_8BIT)"#);
    }

    #[test]
    fn encoding_name_list_class_method() {
        run_test(r#"Encoding.name_list.is_a?(Array)"#);
        run_test(r#"Encoding.name_list.all? { |n| n.is_a?(String) }"#);
        run_test(r#"Encoding.name_list.include?("UTF-8")"#);
        run_test(r#"Encoding.name_list.include?("ASCII-8BIT")"#);
        // Aliases listed alongside canonical names.
        run_test(r#"Encoding.name_list.include?("BINARY")"#);
        run_test(r#"Encoding.name_list.include?("CP65001")"#);
    }

    #[test]
    fn encoding_names_instance_method() {
        run_test(r#"Encoding::UTF_8.names.is_a?(Array)"#);
        run_test(r#"Encoding::UTF_8.names.include?("UTF-8")"#);
        run_test(r#"Encoding::UTF_8.names.include?("CP65001")"#);
        run_test(r#"Encoding::ASCII_8BIT.names.include?("ASCII-8BIT")"#);
        run_test(r#"Encoding::ASCII_8BIT.names.include?("BINARY")"#);
        run_test(r#"Encoding::US_ASCII.names.include?("US-ASCII")"#);
        run_test(r#"Encoding::US_ASCII.names.include?("ASCII")"#);
    }

    #[test]
    fn encoding_inspect_form() {
        // ASCII-8BIT renders as the 3.4+ canonical `BINARY (ASCII-8BIT)` form.
        run_test(r#"Encoding::ASCII_8BIT.inspect"#);
        // Plain non-dummy encoding: `#<Encoding:NAME>`.
        run_test(r#"Encoding::UTF_8.inspect"#);
        run_test(r#"Encoding::US_ASCII.inspect"#);
        // Dummy encoding: `(dummy)` suffix.
        run_test(r#"Encoding::UTF_7.inspect"#);
        run_test(r#"Encoding::ISO_2022_JP.inspect"#);
        run_test(r#"Encoding::UTF_16.inspect"#);
    }

    #[test]
    fn force_encoding_accepts_dummy_aliases() {
        // The new dummy / Mac-family aliases are accepted without
        // raising. monoruby internally normalises the encoding tag to
        // ASCII-8BIT (it doesn't transcode), so we just assert that
        // the call returns a String.
        run_test_no_result_check(
            r#"
            raise unless "abc".force_encoding("UTF-7").is_a?(String)
            raise unless "abc".force_encoding("Emacs-Mule").is_a?(String)
            raise unless "abc".force_encoding("CP50220").is_a?(String)
            raise unless "abc".force_encoding("MacCyrillic").is_a?(String)
            "#,
        );
    }
}
