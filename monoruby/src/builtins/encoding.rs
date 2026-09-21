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
/// The `Encoding::<NAME>` constants an encoding *name* contributes, per
/// CRuby's `set_encoding_const`:
///
/// - a name starting with a digit (`"646"`) contributes none;
/// - a name that already spells a constant (leading uppercase, then
///   only alphanumerics and underscores) is registered as written;
/// - otherwise every non-alphanumeric character becomes `_` and a
///   leading lowercase letter is upcased — that spelling is registered
///   when the name carries an uppercase letter anywhere
///   (`"eucJP-ms"` → `EucJP_ms`, but `"euc-jp-ms"` → nothing here);
/// - and a name carrying a lowercase letter anywhere also gets the
///   all-uppercase spelling (`"Windows-31J"` → `WINDOWS_31J`).
///
/// Verified name for name against CRuby 4.0: the set below is exactly
/// `Encoding.constants` for every name in `Encoding.name_list`.
fn encoding_const_names(name: &str) -> Vec<String> {
    let mut out: Vec<String> = vec![];
    let Some(first) = name.chars().next() else {
        return out;
    };
    if first.is_ascii_digit() {
        return out;
    }
    let has_upper = name.bytes().any(|b| b.is_ascii_uppercase());
    let has_lower = name.bytes().any(|b| b.is_ascii_lowercase());
    if first.is_ascii_uppercase()
        && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
    {
        out.push(name.to_string());
    }
    let mut sanitized: String = name
        .bytes()
        .map(|b| if b.is_ascii_alphanumeric() { b as char } else { '_' })
        .collect();
    // SAFETY of the index: `sanitized` is ASCII by construction and
    // `name` is non-empty, so byte 0 is a whole character.
    sanitized.replace_range(0..1, &sanitized[0..1].to_ascii_uppercase());
    if has_upper && !out.contains(&sanitized) {
        out.push(sanitized.clone());
    }
    if has_lower {
        let upper = sanitized.to_ascii_uppercase();
        if !out.contains(&upper) {
            out.push(upper);
        }
    }
    out
}

pub(super) fn canonical_encoding_name(name: &str) -> &'static str {
    match name {
        // Underscore-preserving / mixed-case names CRuby exposes.
        "SHIFT_JIS" | "Shift_JIS" => "Shift_JIS",
        "EUCJP_MS" => "eucJP-ms",
        // CRuby spells the Mac OS script encodings with a lowercase
        // `mac` — everywhere but `MacJapanese`, which keeps the capital
        // (#1471). The constant is `Encoding::MacRoman` either way.
        "MacRoman" => "macRoman",
        "MacCyrillic" => "macCyrillic",
        "MacCentEuro" => "macCentEuro",
        "MacCroatian" => "macCroatian",
        "MacGreek" => "macGreek",
        "MacIceland" => "macIceland",
        "MacRomania" => "macRomania",
        "MacThai" => "macThai",
        "MacTurkish" => "macTurkish",
        "MacUkraine" => "macUkraine",
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
            Value::string_usascii_from_str("#<Encoding:BINARY (ASCII-8BIT)>"),
        )
        .unwrap();
    globals
        .store
        .set_ivar(
            val,
            IdentId::_ENCODING,
            Value::string_usascii_from_str("ASCII-8BIT"),
        )
        .unwrap();
    globals.register_encoding_object(val, Encoding::Ascii8);
    globals.set_constant(enc.id(), IdentId::ASCII_8BIT, val);
    globals.set_constant_by_str(enc.id(), "BINARY", val);
    // `(canonical display name, encoding object)` for every constant
    // registered below, so the CRuby-spelling pass afterwards can find
    // each encoding's object without re-deriving it.
    let mut registered: Vec<(&'static str, Value)> = vec![];
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
        "MacJapanese",
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
        "MacCentEuro",
        "MacCroatian",
        "MacGreek",
        "MacIceland",
        "MacRoman",
        "MacRomania",
        "MacThai",
        "MacTurkish",
        "MacUkraine",
        // `UTF8_MAC` is CRuby's HFS+/macOS-NFD UTF-8 variant. The
        // legacy `UTF_8_MAC` spelling is wired as an alias of this
        // single object below (not a distinct encoding).
        "UTF8_MAC",
        // `CESU-8` is a UTF-8 variant used by some legacy systems
        // (encodes supplementary chars as surrogate pairs in
        // UTF-8). We don't transcode it specially; the constant
        // exists so `Encoding::CESU_8` resolves.
        "CESU_8",
    ] {
        let canonical: &'static str = canonical_encoding_name(name);
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
                    Value::string_usascii_from_str(&format!("#<Encoding:{}>", canonical)),
                )
                .unwrap();
            globals
                .store
                .set_ivar(val, IdentId::_ENCODING, Value::string_usascii_from_str(canonical))
                .unwrap();
            // The object's `Encoding`, recorded once so
            // `String#force_encoding(Encoding::X)` never re-parses the
            // name. A name monoruby has no `Encoding` for keeps only the
            // name path.
            if let Ok(e) = Encoding::try_from_str(canonical) {
                globals.register_encoding_object(val, e);
            }
            val
        };
        globals.set_constant_by_str(enc.id(), name, val);
        registered.push((canonical, val));
    }

    // CRuby's `set_encoding_const`: every canonical name and every
    // alias contributes its constant spellings, so `Encoding::EUCJP`,
    // `Encoding::SJIS`, `Encoding::ISO8859_9` and
    // `Encoding::WINDOWS_1252` all resolve. A display name is not a
    // constant name — the loop above registered "ISO-8859-1" verbatim,
    // which no Ruby program can even write.
    for (canonical, val) in registered {
        let aliases = ENCODING_NAMES
            .iter()
            .find(|(c, _)| *c == canonical)
            .map(|(_, a)| *a)
            .unwrap_or(&[]);
        for name in std::iter::once(canonical).chain(aliases.iter().copied()) {
            // The three run-time aliases name whatever the locale and
            // `default_external` currently are, so they are not
            // constants in CRuby either.
            if DYNAMIC_ALIASES.contains(&name) {
                continue;
            }
            for c in encoding_const_names(name) {
                if globals
                    .store
                    .get_constant_noautoload(enc.id(), IdentId::get_id(&c))
                    .is_none()
                {
                    globals.set_constant_by_str(enc.id(), &c, val);
                }
            }
        }
    }

    // Aliases that share their underlying Value with another
    // constant — `Encoding::ASCII == Encoding::US_ASCII` and
    // `Encoding::CP65001 == Encoding::UTF_8` per CRuby. (BINARY ↔
    // ASCII-8BIT is already wired above.)
    if let Some(us_ascii) = globals
        .store
        .get_constant_noautoload(enc.id(), IdentId::get_id("US_ASCII"))
    {
        globals.set_constant_by_str(enc.id(), "ASCII", us_ascii);
    }
    if let Some(utf8) = globals
        .store
        .get_constant_noautoload(enc.id(), IdentId::UTF_8)
    {
        globals.set_constant_by_str(enc.id(), "CP65001", utf8);
    }
    // `UTF_8_MAC` is CRuby's legacy spelling of `UTF8_MAC` — the same
    // encoding, not a distinct one. Share the object so `Encoding.list`
    // lists it once and `Encoding.find` round-trips its name.
    if let Some(utf8_mac) = globals
        .store
        .get_constant_noautoload(enc.id(), IdentId::get_id("UTF8_MAC"))
    {
        globals.set_constant_by_str(enc.id(), "UTF_8_MAC", utf8_mac);
    }
    // CP-numbered alias constants sharing their object with the
    // canonical encoding, per CRuby (`Encoding::CP1251 ==
    // Encoding::Windows_1251`, `Encoding::CP437 == Encoding::IBM437`,
    // `Encoding::CP936 == Encoding::GBK`).
    for (alias, canonical) in [
        ("CP1250", "Windows_1250"),
        ("CP1251", "Windows_1251"),
        ("CP1252", "Windows_1252"),
        ("CP1253", "Windows_1253"),
        ("CP1254", "Windows_1254"),
        ("CP1255", "Windows_1255"),
        ("CP1256", "Windows_1256"),
        ("CP1257", "Windows_1257"),
        ("CP1258", "Windows_1258"),
        ("CP437", "IBM437"),
        ("CP737", "IBM737"),
        ("CP775", "IBM775"),
        ("CP850", "IBM850"),
        ("CP852", "IBM852"),
        ("CP855", "IBM855"),
        ("CP857", "IBM857"),
        ("CP860", "IBM860"),
        ("CP861", "IBM861"),
        ("CP862", "IBM862"),
        ("CP863", "IBM863"),
        ("CP864", "IBM864"),
        ("CP865", "IBM865"),
        ("CP866", "IBM866"),
        ("CP869", "IBM869"),
        ("CP936", "GBK"),
    ] {
        if let Some(val) = globals
            .store
            .get_constant_noautoload(enc.id(), IdentId::get_id(canonical))
        {
            globals.set_constant_by_str(enc.id(), alias, val);
        }
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
    let conv_not_found = globals.define_class("ConverterNotFoundError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "ConverterNotFoundError", conv_not_found.get());
    // Encoding::UndefinedConversionError < EncodingError. Same
    // motivation — referenced by `String#encode` specs.
    let undef_conv = globals.define_class("UndefinedConversionError", enc_error_module, enc.id());
    globals.set_constant_by_str(enc.id(), "UndefinedConversionError", undef_conv.get());
    // Encoding::InvalidByteSequenceError < EncodingError. Same
    // motivation.
    let invalid_byte = globals.define_class("InvalidByteSequenceError", enc_error_module, enc.id());
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
    globals.define_builtin_func(invalid_byte.id(), "error_bytes", enc_err_error_bytes, 0);
    globals.define_builtin_func(
        invalid_byte.id(),
        "readagain_bytes",
        enc_err_readagain_bytes,
        0,
    );
    globals.define_builtin_func(undef_conv.id(), "error_bytes", enc_err_error_bytes, 0);

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
    // kw_rest=true so `Converter.new(src, dst, replace: …)` and
    // `**opts` deliver the options as a trailing Hash (read via
    // `get_options_hash_value`, like `String#encode`).
    globals.define_builtin_class_func_with_kw(
        converter.id(),
        "new",
        converter_new,
        2,
        3,
        false,
        &[],
        true,
    );
    globals.define_builtin_class_func(
        converter.id(),
        "asciicompat_encoding",
        converter_asciicompat_encoding,
        1,
    );
    globals.define_builtin_class_func_with_kw(
        converter.id(),
        "search_convpath",
        converter_search_convpath,
        2,
        3,
        false,
        &[],
        true,
    );
    globals.define_builtin_func(converter.id(), "convpath", converter_convpath, 0);
    globals.define_builtin_func(
        converter.id(),
        "source_encoding",
        converter_source_encoding,
        0,
    );
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
    globals.define_builtin_func_with(converter.id(), "putback", converter_putback, 0, 1, false);
    globals.define_builtin_func(converter.id(), "==", converter_eq, 1);
    // `primitive_convert(src, dst, dst_offset = 0, dst_bytesize = nil, opts = {})`
    // — chunked variant of `convert` that mutates `src` and `dst`
    // in place and returns a Symbol describing the outcome.
    globals.define_builtin_func_with_kw(
        converter.id(),
        "primitive_convert",
        converter_primitive_convert,
        2,
        4,
        false,
        &[],
        true,
    );
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
/// UTF-7, the one encoding CRuby names but ships no converter for in
/// either direction — `"ab".encode("UTF-7")` is a
/// `ConverterNotFoundError` where every other dummy encoding converts
/// its 7-bit content (#1471).
pub(super) fn is_utf7(enc: crate::value::Encoding) -> bool {
    matches!(enc, crate::value::Encoding::Other(0))
}

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
        // MacJapanese runs on the Shift_JIS character walk but has no
        // converter of its own in CRuby, which answers
        // `ConverterNotFoundError` for anything but 7-bit text — so it
        // gets no codec here either (#1471).
        E::Sjis(2) => return None,
        E::Sjis(_) => b"shift_jis",
        E::Iso2022Jp => b"iso-2022-jp",
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
            "Windows-1258" => b"windows-1258",
            "KOI8-R" => b"koi8-r",
            "KOI8-U" => b"koi8-u",
            "IBM866" => b"ibm866",
            "Big5" | "Big5-HKSCS" => b"big5",
            "GBK" | "GB2312" => b"gbk",
            "GB18030" => b"gb18030",
            "EUC-KR" | "CP949" => b"euc-kr",
            "TIS-620" => b"windows-874",
            // Big5-UAO / EUC-TW / GB12345 and the DOS codepages other
            // than IBM866 have no encoding_rs codec; IBM437 is served
            // by the in-tree single-byte table instead.
            _ => return None,
        },
        // Handled by callers as fast paths / no native codec.
        E::Utf32Le | E::Utf32Be | E::Ascii8 | E::UsAscii | E::Other(_) => return None,
    };
    encoding_rs::Encoding::for_label(label)
}

/// The character a high-half byte stands for in a single-byte
/// encoding: the in-tree table when there is one, otherwise the codec.
/// `None` when the byte is not a character there (or the encoding is
/// not single-byte), which is what Onigmo's ctype tables are built on.
pub(super) fn single_byte_char(enc: crate::value::Encoding, b: u8) -> Option<char> {
    if let Some(table) = single_byte_table(enc) {
        return table[(b & 0x7f) as usize];
    }
    if let crate::value::Encoding::Iso8859(n) = enc {
        // The ISO-8859 family keeps C1 controls at 0x80..=0x9F.
        // `encoding_rs` is WHATWG's, where `iso-8859-1` / `-9` / `-11`
        // are aliases of the Windows code pages, which put characters
        // there instead.
        if (0x80..=0x9f).contains(&b) {
            return None;
        }
        // Latin-1 *is* U+0000..U+00FF, so it needs no codec — and the
        // parts `encoding_rs` has none for (ISO-8859-9) differ from it
        // only by swapping letters for letters, which leaves the
        // letter / non-letter split this is asked for unchanged.
        if n == 1 || encoding_to_rs(enc).is_none() {
            return Some(char::from(b));
        }
    }
    let rs = encoding_to_rs(enc)?;
    let buf = [b];
    let (decoded, had_err) = rs.decode_without_bom_handling(&buf);
    if had_err {
        return None;
    }
    let mut chars = decoded.chars();
    let c = chars.next()?;
    if chars.next().is_some() || c == '\u{FFFD}' {
        return None;
    }
    Some(c)
}

/// High-half (0x80..=0xFF) Unicode mapping for the single-byte
/// encodings monoruby transcodes with an in-tree table rather than
/// through `encoding_rs`. `None` is a cell the encoding assigns no
/// character to. Bytes < 0x80 are ASCII in all of these.
pub(super) fn single_byte_table(enc: crate::value::Encoding) -> Option<&'static [Option<char>; 128]> {
    /// IBM437 (the original IBM PC / DOS codepage).
    const IBM437: [Option<char>; 128] = [
        Some('Ç'), Some('ü'), Some('é'), Some('â'), Some('ä'), Some('à'), Some('å'), Some('ç'), Some('ê'), Some('ë'), Some('è'), Some('ï'), Some('î'), Some('ì'), Some('Ä'), Some('Å'), //
        Some('É'), Some('æ'), Some('Æ'), Some('ô'), Some('ö'), Some('ò'), Some('û'), Some('ù'), Some('ÿ'), Some('Ö'), Some('Ü'), Some('¢'), Some('£'), Some('¥'), Some('₧'), Some('ƒ'), //
        Some('á'), Some('í'), Some('ó'), Some('ú'), Some('ñ'), Some('Ñ'), Some('ª'), Some('º'), Some('¿'), Some('⌐'), Some('¬'), Some('½'), Some('¼'), Some('¡'), Some('«'), Some('»'), //
        Some('░'), Some('▒'), Some('▓'), Some('│'), Some('┤'), Some('╡'), Some('╢'), Some('╖'), Some('╕'), Some('╣'), Some('║'), Some('╗'), Some('╝'), Some('╜'), Some('╛'), Some('┐'), //
        Some('└'), Some('┴'), Some('┬'), Some('├'), Some('─'), Some('┼'), Some('╞'), Some('╟'), Some('╚'), Some('╔'), Some('╩'), Some('╦'), Some('╠'), Some('═'), Some('╬'), Some('╧'), //
        Some('╨'), Some('╤'), Some('╥'), Some('╙'), Some('╘'), Some('╒'), Some('╓'), Some('╫'), Some('╪'), Some('┘'), Some('┌'), Some('█'), Some('▄'), Some('▌'), Some('▐'), Some('▀'), //
        Some('α'), Some('ß'), Some('Γ'), Some('π'), Some('Σ'), Some('σ'), Some('µ'), Some('τ'), Some('Φ'), Some('Θ'), Some('Ω'), Some('δ'), Some('∞'), Some('φ'), Some('ε'), Some('∩'), //
        Some('≡'), Some('±'), Some('≥'), Some('≤'), Some('⌠'), Some('⌡'), Some('÷'), Some('≈'), Some('°'), Some('∙'), Some('·'), Some('√'), Some('ⁿ'), Some('²'), Some('■'),
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
        Some('\u{80}'), Some('\u{81}'), Some('\u{82}'), Some('\u{83}'), Some('\u{84}'), Some('\u{85}'), Some('\u{86}'), Some('\u{87}'),
        Some('\u{88}'), Some('\u{89}'), Some('\u{8A}'), Some('\u{8B}'), Some('\u{8C}'), Some('\u{8D}'), Some('\u{8E}'), Some('\u{8F}'),
        Some('\u{90}'), Some('\u{91}'), Some('\u{92}'), Some('\u{93}'), Some('\u{94}'), Some('\u{95}'), Some('\u{96}'), Some('\u{97}'),
        Some('\u{98}'), Some('\u{99}'), Some('\u{9A}'), Some('\u{9B}'), Some('\u{9C}'), Some('\u{9D}'), Some('\u{9E}'), Some('\u{9F}'),
        Some('\u{A0}'), Some('\u{E01}'), Some('\u{E02}'), Some('\u{E03}'), Some('\u{E04}'), Some('\u{E05}'), Some('\u{E06}'), Some('\u{E07}'),
        Some('\u{E08}'), Some('\u{E09}'), Some('\u{E0A}'), Some('\u{E0B}'), Some('\u{E0C}'), Some('\u{E0D}'), Some('\u{E0E}'), Some('\u{E0F}'),
        Some('\u{E10}'), Some('\u{E11}'), Some('\u{E12}'), Some('\u{E13}'), Some('\u{E14}'), Some('\u{E15}'), Some('\u{E16}'), Some('\u{E17}'),
        Some('\u{E18}'), Some('\u{E19}'), Some('\u{E1A}'), Some('\u{E1B}'), Some('\u{E1C}'), Some('\u{E1D}'), Some('\u{E1E}'), Some('\u{E1F}'),
        Some('\u{E20}'), Some('\u{E21}'), Some('\u{E22}'), Some('\u{E23}'), Some('\u{E24}'), Some('\u{E25}'), Some('\u{E26}'), Some('\u{E27}'),
        Some('\u{E28}'), Some('\u{E29}'), Some('\u{E2A}'), Some('\u{E2B}'), Some('\u{E2C}'), Some('\u{E2D}'), Some('\u{E2E}'), Some('\u{E2F}'),
        Some('\u{E30}'), Some('\u{E31}'), Some('\u{E32}'), Some('\u{E33}'), Some('\u{E34}'), Some('\u{E35}'), Some('\u{E36}'), Some('\u{E37}'),
        Some('\u{E38}'), Some('\u{E39}'), Some('\u{E3A}'), None, None, None, None, Some('\u{E3F}'),
        Some('\u{E40}'), Some('\u{E41}'), Some('\u{E42}'), Some('\u{E43}'), Some('\u{E44}'), Some('\u{E45}'), Some('\u{E46}'), Some('\u{E47}'),
        Some('\u{E48}'), Some('\u{E49}'), Some('\u{E4A}'), Some('\u{E4B}'), Some('\u{E4C}'), Some('\u{E4D}'), Some('\u{E4E}'), Some('\u{E4F}'),
        Some('\u{E50}'), Some('\u{E51}'), Some('\u{E52}'), Some('\u{E53}'), Some('\u{E54}'), Some('\u{E55}'), Some('\u{E56}'), Some('\u{E57}'),
        Some('\u{E58}'), Some('\u{E59}'), Some('\u{E5A}'), Some('\u{E5B}'), None, None, None, None,
    ];

    /// macRoman (Mac OS Roman), and the seven tables below it, as
    /// CRuby's own converters have them — read off byte by byte. The
    /// Apple-logo cell (`0xF0` in most of the family) maps to no
    /// character, which CRuby reports as an undefined conversion.
    const MACROMAN: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{C5}'), Some('\u{C7}'), Some('\u{C9}'), Some('\u{D1}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{E1}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E3}'), Some('\u{E5}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{ED}'), Some('\u{EC}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{F1}'), Some('\u{F3}'),
        Some('\u{F2}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F5}'), Some('\u{FA}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{B4}'), Some('\u{A8}'), Some('\u{2260}'), Some('\u{C6}'), Some('\u{D8}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{A5}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{2211}'),
        Some('\u{220F}'), Some('\u{3C0}'), Some('\u{222B}'), Some('\u{AA}'), Some('\u{BA}'), Some('\u{2126}'), Some('\u{E6}'), Some('\u{F8}'),
        Some('\u{BF}'), Some('\u{A1}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{C0}'), Some('\u{C3}'), Some('\u{D5}'), Some('\u{152}'), Some('\u{153}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{25CA}'),
        Some('\u{FF}'), Some('\u{178}'), Some('\u{2044}'), Some('\u{A4}'), Some('\u{2039}'), Some('\u{203A}'), Some('\u{FB01}'), Some('\u{FB02}'),
        Some('\u{2021}'), Some('\u{B7}'), Some('\u{201A}'), Some('\u{201E}'), Some('\u{2030}'), Some('\u{C2}'), Some('\u{CA}'), Some('\u{C1}'),
        Some('\u{CB}'), Some('\u{C8}'), Some('\u{CD}'), Some('\u{CE}'), Some('\u{CF}'), Some('\u{CC}'), Some('\u{D3}'), Some('\u{D4}'),
        None, Some('\u{D2}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), Some('\u{131}'), Some('\u{2C6}'), Some('\u{2DC}'),
        Some('\u{AF}'), Some('\u{2D8}'), Some('\u{2D9}'), Some('\u{2DA}'), Some('\u{B8}'), Some('\u{2DD}'), Some('\u{2DB}'), Some('\u{2C7}'),
    ];
    /// macCyrillic.
    const MACCYRILLIC: [Option<char>; 128] = [
        Some('\u{410}'), Some('\u{411}'), Some('\u{412}'), Some('\u{413}'), Some('\u{414}'), Some('\u{415}'), Some('\u{416}'), Some('\u{417}'),
        Some('\u{418}'), Some('\u{419}'), Some('\u{41A}'), Some('\u{41B}'), Some('\u{41C}'), Some('\u{41D}'), Some('\u{41E}'), Some('\u{41F}'),
        Some('\u{420}'), Some('\u{421}'), Some('\u{422}'), Some('\u{423}'), Some('\u{424}'), Some('\u{425}'), Some('\u{426}'), Some('\u{427}'),
        Some('\u{428}'), Some('\u{429}'), Some('\u{42A}'), Some('\u{42B}'), Some('\u{42C}'), Some('\u{42D}'), Some('\u{42E}'), Some('\u{42F}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{406}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{402}'), Some('\u{452}'), Some('\u{2260}'), Some('\u{403}'), Some('\u{453}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{456}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{408}'),
        Some('\u{404}'), Some('\u{454}'), Some('\u{407}'), Some('\u{457}'), Some('\u{409}'), Some('\u{459}'), Some('\u{40A}'), Some('\u{45A}'),
        Some('\u{458}'), Some('\u{405}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{40B}'), Some('\u{45B}'), Some('\u{40C}'), Some('\u{45C}'), Some('\u{455}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{201E}'),
        Some('\u{40E}'), Some('\u{45E}'), Some('\u{40F}'), Some('\u{45F}'), Some('\u{2116}'), Some('\u{401}'), Some('\u{451}'), Some('\u{44F}'),
        Some('\u{430}'), Some('\u{431}'), Some('\u{432}'), Some('\u{433}'), Some('\u{434}'), Some('\u{435}'), Some('\u{436}'), Some('\u{437}'),
        Some('\u{438}'), Some('\u{439}'), Some('\u{43A}'), Some('\u{43B}'), Some('\u{43C}'), Some('\u{43D}'), Some('\u{43E}'), Some('\u{43F}'),
        Some('\u{440}'), Some('\u{441}'), Some('\u{442}'), Some('\u{443}'), Some('\u{444}'), Some('\u{445}'), Some('\u{446}'), Some('\u{447}'),
        Some('\u{448}'), Some('\u{449}'), Some('\u{44A}'), Some('\u{44B}'), Some('\u{44C}'), Some('\u{44D}'), Some('\u{44E}'), Some('\u{A4}'),
    ];
    /// macCroatian.
    const MACCROATIAN: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{C5}'), Some('\u{C7}'), Some('\u{C9}'), Some('\u{D1}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{E1}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E3}'), Some('\u{E5}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{ED}'), Some('\u{EC}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{F1}'), Some('\u{F3}'),
        Some('\u{F2}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F5}'), Some('\u{FA}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{160}'), Some('\u{2122}'), Some('\u{B4}'), Some('\u{A8}'), Some('\u{2260}'), Some('\u{17D}'), Some('\u{D8}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{2206}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{2211}'),
        Some('\u{220F}'), Some('\u{161}'), Some('\u{222B}'), Some('\u{AA}'), Some('\u{BA}'), Some('\u{2126}'), Some('\u{17E}'), Some('\u{F8}'),
        Some('\u{BF}'), Some('\u{A1}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{106}'), Some('\u{AB}'),
        Some('\u{10C}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{C0}'), Some('\u{C3}'), Some('\u{D5}'), Some('\u{152}'), Some('\u{153}'),
        Some('\u{110}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{25CA}'),
        None, Some('\u{A9}'), Some('\u{2044}'), Some('\u{A4}'), Some('\u{2039}'), Some('\u{203A}'), Some('\u{C6}'), Some('\u{BB}'),
        Some('\u{2013}'), Some('\u{B7}'), Some('\u{201A}'), Some('\u{201E}'), Some('\u{2030}'), Some('\u{C2}'), Some('\u{107}'), Some('\u{C1}'),
        Some('\u{10D}'), Some('\u{C8}'), Some('\u{CD}'), Some('\u{CE}'), Some('\u{CF}'), Some('\u{CC}'), Some('\u{D3}'), Some('\u{D4}'),
        Some('\u{111}'), Some('\u{D2}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), Some('\u{131}'), Some('\u{2C6}'), Some('\u{2DC}'),
        Some('\u{AF}'), Some('\u{3C0}'), Some('\u{CB}'), Some('\u{2DA}'), Some('\u{B8}'), Some('\u{CA}'), Some('\u{E6}'), Some('\u{2C7}'),
    ];
    /// macGreek.
    const MACGREEK: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{B9}'), Some('\u{B2}'), Some('\u{C9}'), Some('\u{B3}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{385}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{384}'), Some('\u{A8}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{A3}'), Some('\u{2122}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{2022}'), Some('\u{BD}'),
        Some('\u{2030}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{A6}'), Some('\u{AD}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{2020}'), Some('\u{393}'), Some('\u{394}'), Some('\u{398}'), Some('\u{39B}'), Some('\u{39E}'), Some('\u{3A0}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{3A3}'), Some('\u{3AA}'), Some('\u{A7}'), Some('\u{2260}'), Some('\u{B0}'), Some('\u{387}'),
        Some('\u{391}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{A5}'), Some('\u{392}'), Some('\u{395}'), Some('\u{396}'),
        Some('\u{397}'), Some('\u{399}'), Some('\u{39A}'), Some('\u{39C}'), Some('\u{3A6}'), Some('\u{3AB}'), Some('\u{3A8}'), Some('\u{3A9}'),
        Some('\u{3AC}'), Some('\u{39D}'), Some('\u{AC}'), Some('\u{39F}'), Some('\u{3A1}'), Some('\u{2248}'), Some('\u{3A4}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{3A5}'), Some('\u{3A7}'), Some('\u{386}'), Some('\u{388}'), Some('\u{153}'),
        Some('\u{2013}'), Some('\u{2015}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{389}'),
        Some('\u{38A}'), Some('\u{38C}'), Some('\u{38E}'), Some('\u{3AD}'), Some('\u{3AE}'), Some('\u{3AF}'), Some('\u{3CC}'), Some('\u{38F}'),
        Some('\u{3CD}'), Some('\u{3B1}'), Some('\u{3B2}'), Some('\u{3C8}'), Some('\u{3B4}'), Some('\u{3B5}'), Some('\u{3C6}'), Some('\u{3B3}'),
        Some('\u{3B7}'), Some('\u{3B9}'), Some('\u{3BE}'), Some('\u{3BA}'), Some('\u{3BB}'), Some('\u{3BC}'), Some('\u{3BD}'), Some('\u{3BF}'),
        Some('\u{3C0}'), Some('\u{3CE}'), Some('\u{3C1}'), Some('\u{3C3}'), Some('\u{3C4}'), Some('\u{3B8}'), Some('\u{3C9}'), Some('\u{3C2}'),
        Some('\u{3C7}'), Some('\u{3C5}'), Some('\u{3B6}'), Some('\u{3CA}'), Some('\u{3CB}'), Some('\u{390}'), Some('\u{3B0}'), None,
    ];
    /// macIceland.
    const MACICELAND: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{C5}'), Some('\u{C7}'), Some('\u{C9}'), Some('\u{D1}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{E1}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E3}'), Some('\u{E5}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{ED}'), Some('\u{EC}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{F1}'), Some('\u{F3}'),
        Some('\u{F2}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F5}'), Some('\u{FA}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{DD}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{B4}'), Some('\u{A8}'), Some('\u{2260}'), Some('\u{C6}'), Some('\u{D8}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{A5}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{2211}'),
        Some('\u{220F}'), Some('\u{3C0}'), Some('\u{222B}'), Some('\u{AA}'), Some('\u{BA}'), Some('\u{2126}'), Some('\u{E6}'), Some('\u{F8}'),
        Some('\u{BF}'), Some('\u{A1}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{C0}'), Some('\u{C3}'), Some('\u{D5}'), Some('\u{152}'), Some('\u{153}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{25CA}'),
        Some('\u{FF}'), Some('\u{178}'), Some('\u{2044}'), Some('\u{A4}'), Some('\u{D0}'), Some('\u{F0}'), Some('\u{DE}'), Some('\u{FE}'),
        Some('\u{FD}'), Some('\u{B7}'), Some('\u{201A}'), Some('\u{201E}'), Some('\u{2030}'), Some('\u{C2}'), Some('\u{CA}'), Some('\u{C1}'),
        Some('\u{CB}'), Some('\u{C8}'), Some('\u{CD}'), Some('\u{CE}'), Some('\u{CF}'), Some('\u{CC}'), Some('\u{D3}'), Some('\u{D4}'),
        None, Some('\u{D2}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), Some('\u{131}'), Some('\u{2C6}'), Some('\u{2DC}'),
        Some('\u{AF}'), Some('\u{2D8}'), Some('\u{2D9}'), Some('\u{2DA}'), Some('\u{B8}'), Some('\u{2DD}'), Some('\u{2DB}'), Some('\u{2C7}'),
    ];
    /// macRomania.
    const MACROMANIA: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{C5}'), Some('\u{C7}'), Some('\u{C9}'), Some('\u{D1}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{E1}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E3}'), Some('\u{E5}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{ED}'), Some('\u{EC}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{F1}'), Some('\u{F3}'),
        Some('\u{F2}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F5}'), Some('\u{FA}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{B4}'), Some('\u{A8}'), Some('\u{2260}'), Some('\u{102}'), Some('\u{15E}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{A5}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{2211}'),
        Some('\u{220F}'), Some('\u{3C0}'), Some('\u{222B}'), Some('\u{AA}'), Some('\u{BA}'), Some('\u{2126}'), Some('\u{103}'), Some('\u{15F}'),
        Some('\u{BF}'), Some('\u{A1}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{C0}'), Some('\u{C3}'), Some('\u{D5}'), Some('\u{152}'), Some('\u{153}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{25CA}'),
        Some('\u{FF}'), Some('\u{178}'), Some('\u{2044}'), Some('\u{A4}'), Some('\u{2039}'), Some('\u{203A}'), Some('\u{162}'), Some('\u{163}'),
        Some('\u{2021}'), Some('\u{B7}'), Some('\u{201A}'), Some('\u{201E}'), Some('\u{2030}'), Some('\u{C2}'), Some('\u{CA}'), Some('\u{C1}'),
        Some('\u{CB}'), Some('\u{C8}'), Some('\u{CD}'), Some('\u{CE}'), Some('\u{CF}'), Some('\u{CC}'), Some('\u{D3}'), Some('\u{D4}'),
        None, Some('\u{D2}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), Some('\u{131}'), Some('\u{2C6}'), Some('\u{2DC}'),
        Some('\u{AF}'), Some('\u{2D8}'), Some('\u{2D9}'), Some('\u{2DA}'), Some('\u{B8}'), Some('\u{2DD}'), Some('\u{2DB}'), Some('\u{2C7}'),
    ];
    /// macTurkish.
    const MACTURKISH: [Option<char>; 128] = [
        Some('\u{C4}'), Some('\u{C5}'), Some('\u{C7}'), Some('\u{C9}'), Some('\u{D1}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{E1}'),
        Some('\u{E0}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E3}'), Some('\u{E5}'), Some('\u{E7}'), Some('\u{E9}'), Some('\u{E8}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{ED}'), Some('\u{EC}'), Some('\u{EE}'), Some('\u{EF}'), Some('\u{F1}'), Some('\u{F3}'),
        Some('\u{F2}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F5}'), Some('\u{FA}'), Some('\u{F9}'), Some('\u{FB}'), Some('\u{FC}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{A2}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{DF}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{B4}'), Some('\u{A8}'), Some('\u{2260}'), Some('\u{C6}'), Some('\u{D8}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{A5}'), Some('\u{B5}'), Some('\u{2202}'), Some('\u{2211}'),
        Some('\u{220F}'), Some('\u{3C0}'), Some('\u{222B}'), Some('\u{AA}'), Some('\u{BA}'), Some('\u{2126}'), Some('\u{E6}'), Some('\u{F8}'),
        Some('\u{BF}'), Some('\u{A1}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{C0}'), Some('\u{C3}'), Some('\u{D5}'), Some('\u{152}'), Some('\u{153}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{25CA}'),
        Some('\u{FF}'), Some('\u{178}'), Some('\u{11E}'), Some('\u{11F}'), Some('\u{130}'), Some('\u{131}'), Some('\u{15E}'), Some('\u{15F}'),
        Some('\u{2021}'), Some('\u{B7}'), Some('\u{201A}'), Some('\u{201E}'), Some('\u{2030}'), Some('\u{C2}'), Some('\u{CA}'), Some('\u{C1}'),
        Some('\u{CB}'), Some('\u{C8}'), Some('\u{CD}'), Some('\u{CE}'), Some('\u{CF}'), Some('\u{CC}'), Some('\u{D3}'), Some('\u{D4}'),
        None, Some('\u{D2}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), None, Some('\u{2C6}'), Some('\u{2DC}'),
        Some('\u{AF}'), Some('\u{2D8}'), Some('\u{2D9}'), Some('\u{2DA}'), Some('\u{B8}'), Some('\u{2DD}'), Some('\u{2DB}'), Some('\u{2C7}'),
    ];
    /// macUkraine.
    const MACUKRAINE: [Option<char>; 128] = [
        Some('\u{410}'), Some('\u{411}'), Some('\u{412}'), Some('\u{413}'), Some('\u{414}'), Some('\u{415}'), Some('\u{416}'), Some('\u{417}'),
        Some('\u{418}'), Some('\u{419}'), Some('\u{41A}'), Some('\u{41B}'), Some('\u{41C}'), Some('\u{41D}'), Some('\u{41E}'), Some('\u{41F}'),
        Some('\u{420}'), Some('\u{421}'), Some('\u{422}'), Some('\u{423}'), Some('\u{424}'), Some('\u{425}'), Some('\u{426}'), Some('\u{427}'),
        Some('\u{428}'), Some('\u{429}'), Some('\u{42A}'), Some('\u{42B}'), Some('\u{42C}'), Some('\u{42D}'), Some('\u{42E}'), Some('\u{42F}'),
        Some('\u{2020}'), Some('\u{B0}'), Some('\u{490}'), Some('\u{A3}'), Some('\u{A7}'), Some('\u{2022}'), Some('\u{B6}'), Some('\u{406}'),
        Some('\u{AE}'), Some('\u{A9}'), Some('\u{2122}'), Some('\u{402}'), Some('\u{452}'), Some('\u{2260}'), Some('\u{403}'), Some('\u{453}'),
        Some('\u{221E}'), Some('\u{B1}'), Some('\u{2264}'), Some('\u{2265}'), Some('\u{456}'), Some('\u{B5}'), Some('\u{491}'), Some('\u{408}'),
        Some('\u{404}'), Some('\u{454}'), Some('\u{407}'), Some('\u{457}'), Some('\u{409}'), Some('\u{459}'), Some('\u{40A}'), Some('\u{45A}'),
        Some('\u{458}'), Some('\u{405}'), Some('\u{AC}'), Some('\u{221A}'), Some('\u{192}'), Some('\u{2248}'), Some('\u{2206}'), Some('\u{AB}'),
        Some('\u{BB}'), Some('\u{2026}'), Some('\u{A0}'), Some('\u{40B}'), Some('\u{45B}'), Some('\u{40C}'), Some('\u{45C}'), Some('\u{455}'),
        Some('\u{2013}'), Some('\u{2014}'), Some('\u{201C}'), Some('\u{201D}'), Some('\u{2018}'), Some('\u{2019}'), Some('\u{F7}'), Some('\u{201E}'),
        Some('\u{40E}'), Some('\u{45E}'), Some('\u{40F}'), Some('\u{45F}'), Some('\u{2116}'), Some('\u{401}'), Some('\u{451}'), Some('\u{44F}'),
        Some('\u{430}'), Some('\u{431}'), Some('\u{432}'), Some('\u{433}'), Some('\u{434}'), Some('\u{435}'), Some('\u{436}'), Some('\u{437}'),
        Some('\u{438}'), Some('\u{439}'), Some('\u{43A}'), Some('\u{43B}'), Some('\u{43C}'), Some('\u{43D}'), Some('\u{43E}'), Some('\u{43F}'),
        Some('\u{440}'), Some('\u{441}'), Some('\u{442}'), Some('\u{443}'), Some('\u{444}'), Some('\u{445}'), Some('\u{446}'), Some('\u{447}'),
        Some('\u{448}'), Some('\u{449}'), Some('\u{44A}'), Some('\u{44B}'), Some('\u{44C}'), Some('\u{44D}'), Some('\u{44E}'), Some('\u{A4}'),
    ];

    use crate::value::Encoding as E;
    match enc {
        E::Iso8859(1) => Some(&ISO8859_1),
        E::Iso8859(9) => Some(&ISO8859_9),
        E::Iso8859(11) => Some(&ISO8859_11),
        E::NamedByte(_) => match enc.name() {
            "IBM437" => Some(&IBM437),
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
struct JpFixup {
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
}

static EUCJP_FIXUP: JpFixup = JpFixup {
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
    reject: &['\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}'],
    // WHATWG fills `A9..AD` and `F9..FC`; CRuby maps nothing in either
    // range, 457 cells in all.
    dead_rows: &[(0xa9, 0xaf), (0xf5, 0xfe)],
};

static SJIS_FIXUP: JpFixup = JpFixup {
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
    reject: &['\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}'],
    // Row 13 (`87`), the NEC-selected IBM rows (`ED`/`EE`) and the
    // user-defined + IBM rows (`F0`..`FC`) — 2725 cells.
    dead_rows: &[(0x87, 0x87), (0xed, 0xee), (0xf0, 0xfc)],
};

/// Windows-31J agrees with WHATWG everywhere except one character:
/// `encoding_rs` writes `U+2212` to `81 7C`, which in this encoding is
/// `U+FF0D`'s cell and nothing else's.
static WINDOWS31J_FIXUP: JpFixup = JpFixup {
    rs: encoding_rs::SHIFT_JIS,
    precise: sjis_precise_len,
    second_plane: false,
    decode: &[],
    encode: &[],
    reject: &['\u{2212}'],
    dead_rows: &[],
};

/// The table corrections for an encoding, or `None` for one
/// `encoding_rs` already answers CRuby's way.
fn jp_fixup(enc: crate::value::Encoding) -> Option<&'static JpFixup> {
    use crate::value::Encoding as E;
    match enc {
        E::EucJp => Some(&EUCJP_FIXUP),
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
        !fx.dead_rows.iter().any(|&(lo, hi)| (lo..=hi).contains(lead))
    })
}

/// CRuby's answer for one complete cell, or `None` where it agrees
/// with `encoding_rs`.
fn jp_decode_override(fx: &JpFixup, cell: &[u8]) -> Option<char> {
    fx.decode.iter().find(|(seq, _)| *seq == cell).map(|(_, c)| *c)
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
struct JpDecoded<'a> {
    text: std::borrow::Cow<'a, str>,
    /// An ill-formed byte sequence was seen (`invalid:` territory).
    had_invalid: bool,
    /// The first cell that is **well formed** but has no character in
    /// CRuby's table — an extension row. That is an
    /// `UndefinedConversionError`, not an invalid sequence:
    /// `"\xF9\xA1".force_encoding("EUC-JP").valid_encoding?` is
    /// `true`, and `invalid: :replace` does not suppress it. `None`
    /// when the caller asked for those to be replaced instead.
    unmapped: Option<Vec<u8>>,
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
fn jp_decode<'a>(fx: &JpFixup, bytes: &'a [u8], undef: Option<&str>) -> JpDecoded<'a> {
    let rs = fx.rs;
    if !jp_decode_needs_fixup(fx, bytes) {
        let (text, had_invalid) = rs.decode_without_bom_handling(bytes);
        return JpDecoded { text, had_invalid, unmapped: None };
    }
    let flush = |out: &mut String, had_err: &mut bool, run: &[u8]| {
        if run.is_empty() {
            return;
        }
        let (s, e) = rs.decode_without_bom_handling(run);
        out.push_str(&s);
        *had_err |= e;
    };
    let mut out = String::with_capacity(bytes.len());
    let mut had_invalid = false;
    let mut unmapped = None;
    let mut pos = 0;
    let mut pending = 0;
    while pos < bytes.len() {
        let PreciseLen::Char(n) = (fx.precise)(bytes, pos) else {
            pos += 1;
            continue;
        };
        let cell = &bytes[pos..pos + n];
        if let Some(c) = jp_decode_override(fx, cell) {
            flush(&mut out, &mut had_invalid, &bytes[pending..pos]);
            out.push(c);
            pending = pos + n;
        } else if !jp_cell_is_live(fx, cell) {
            flush(&mut out, &mut had_invalid, &bytes[pending..pos]);
            match undef {
                Some(repl) => out.push_str(repl),
                None => {
                    if unmapped.is_none() {
                        unmapped = Some(cell.to_vec());
                    }
                }
            }
            pending = pos + n;
        }
        pos += n;
    }
    flush(&mut out, &mut had_invalid, &bytes[pending..]);
    JpDecoded { text: std::borrow::Cow::Owned(out), had_invalid, unmapped }
}

/// CRuby's `UndefinedConversionError` message for a source cell with
/// no character, spelling out the UTF-8 pivot for a non-UTF-8
/// destination exactly as the BINARY path above does.
fn undefined_cell_message(
    cell: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let escaped: String = cell.iter().map(|b| format!("\\x{b:02X}")).collect();
    if dst_enc == crate::value::Encoding::Utf8 {
        format!("\"{escaped}\" from {} to UTF-8", src_enc.name())
    } else {
        format!(
            "\"{escaped}\" to UTF-8 in conversion from {} to UTF-8 to {}",
            src_enc.name(),
            dst_enc.name()
        )
    }
}

/// The conversion chain CRuby names in a pivoted error message: every
/// non-UTF-8 source reaches the pivot as "<src> to UTF-8", except
/// ISO-2022-JP, whose transcoder goes the long way round.
fn pivot_chain(src_enc: crate::value::Encoding) -> String {
    if src_enc == crate::value::Encoding::Iso2022Jp {
        "ISO-2022-JP to stateless-ISO-2022-JP to EUC-JP to UTF-8".to_string()
    } else {
        format!("{} to UTF-8", src_enc.name())
    }
}

/// CRuby's `UndefinedConversionError` message for a character the
/// destination has no cell for.
///
/// A UTF-8(-compatible) source converts in one hop and is reported as
/// `U+3042 from UTF-8 to EUC-JP`; anything else runs through the UTF-8
/// pivot, and CRuby then spells the whole chain out —
/// `U+3042 to IBM437 in conversion from EUC-JP to UTF-8 to IBM437`.
/// [`undefined_char_message`] for a *byte* the source encoding assigns
/// a character to that Unicode has nowhere to put — ISO-8859-11's eight
/// unassigned Thai cells are the only ones here. CRuby quotes the byte
/// where it would otherwise name a codepoint.
fn undefined_byte_message(
    b: u8,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let quoted = quote_error_bytes(&[b]);
    if dst_enc == crate::value::Encoding::Utf8 {
        format!("{quoted} from {} to {}", src_enc.name(), dst_enc.name())
    } else {
        format!(
            "{quoted} to UTF-8 in conversion from {} to UTF-8 to {}",
            src_enc.name(),
            dst_enc.name()
        )
    }
}

fn undefined_char_message(
    c: char,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    if src_enc.is_utf8_compatible() {
        format!(
            "U+{:04X} from {} to {}",
            c as u32,
            src_enc.name(),
            dst_enc.name()
        )
    } else {
        format!(
            "U+{:04X} to {} in conversion from {} to {}",
            c as u32,
            dst_enc.name(),
            pivot_chain(src_enc),
            dst_enc.name()
        )
    }
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
fn jp_encode(fx: &JpFixup, s: &str) -> std::result::Result<Vec<u8>, char> {
    let rs = fx.rs;
    // Whether this character is one the fixup tables move — the cheap
    // test that keeps the fast path.
    let is_fixed_up = |c: char| {
        c > '\u{7f}' && (fx.reject.contains(&c) || fx.encode.iter().any(|(k, _)| *k == c))
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
        if let Some((_, seq)) = fx.encode.iter().find(|(k, _)| *k == c) {
            out.extend_from_slice(seq);
            continue;
        }
        let (chunk, _, err) = rs.encode(c.encode_utf8(&mut buf));
        if !err && jp_cell_is_live(fx, &chunk) {
            out.extend_from_slice(&chunk);
        } else if fx.second_plane && let Some(seq) = jisx0212_reverse().get(&c) {
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
fn table_decode(
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
fn table_decode_lossy(bytes: &[u8], table: &[Option<char>; 128], replace: &str) -> String {
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
fn table_encode(s: &str, table: &[Option<char>; 128]) -> std::result::Result<Vec<u8>, char> {
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
pub(super) struct TranscodeOpts {
    pub invalid_replace: bool,
    pub undef_replace: bool,
    pub replace: Option<String>,
    /// Newline decorators: `universal_newline:` normalizes CRLF / CR
    /// to LF on the decode side; `crlf_newline:` / `cr_newline:`
    /// rewrite LF on the encode side.
    pub universal_newline: bool,
    pub crlf_newline: bool,
    pub cr_newline: bool,
}

impl TranscodeOpts {
    fn has_newline(&self) -> bool {
        self.universal_newline || self.crlf_newline || self.cr_newline
    }

    /// Apply the newline decorators to decoded text.
    fn apply_newline(&self, s: &str) -> String {
        let mut out = s.to_string();
        if self.universal_newline {
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

/// The endianness-less dummy `UTF-16` / `UTF-32` as an encode target:
/// returns the big-endian concrete encoding CRuby writes after a BOM.
fn dummy_wide_target(enc: crate::value::Encoding) -> Option<crate::value::Encoding> {
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

/// Byte-level [`TranscodeOpts::apply_newline`], sound for any
/// ASCII-compatible encoding (no multibyte sequence contains the 0x0A /
/// 0x0D bytes in those encodings).
fn apply_newline_bytes(bytes: &[u8], opts: &TranscodeOpts) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if opts.universal_newline && b == b'\r' {
            // CRLF or bare CR → LF.
            if bytes.get(i + 1) == Some(&b'\n') {
                i += 1;
            }
            out.push(b'\n');
        } else if b == b'\n' && opts.crlf_newline {
            out.extend_from_slice(b"\r\n");
        } else if b == b'\n' && opts.cr_newline {
            out.push(b'\r');
        } else {
            out.push(b);
        }
        i += 1;
    }
    out
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
fn is_utf16_or_32(enc: crate::value::Encoding) -> bool {
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
pub(super) fn transcode_for_env(
    store: &Store,
    bytes: &[u8],
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
) -> Result<Vec<u8>> {
    transcode_bytes_with_opts(bytes, src, dst, &TranscodeOpts::default(), store)
}

pub(super) fn transcode_bytes_with_opts(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<Vec<u8>> {
    use crate::value::Encoding as E;
    // `invalid: :replace` has work to do even when the encodings match,
    // so a broken string with a usable codec skips the identity path
    // and goes through decode / re-encode to be scrubbed.
    // …and only when nothing else is being asked for. A newline
    // decorator on a same-encoding `encode` is the whole conversion —
    // no transcoder runs, so there is nothing for `invalid:` to act in
    // and CRuby hands the bytes through untouched:
    //
    //     "a\x80\r\nb".encode(invalid: :replace, universal_newline: true)
    //     # the 0x80 survives; only the CRLF becomes LF
    let scrub_in_place = src_enc == dst_enc
        && opts.invalid_replace
        && !opts.has_newline()
        && matches!(
            RStringInner::from_encoding_scanned(src_bytes, src_enc).code_range(),
            crate::value::CodeRange::Broken
        )
        && (encoding_to_rs(src_enc).is_some()
            || is_utf16_or_32(src_enc)
            || single_byte_table(src_enc).is_some());
    // The encodings monoruby walks itself (Emacs-Mule, EUC-JP,
    // Shift_JIS) scrub through that walk, not through a codec. For
    // Emacs-Mule there is no codec to use; for the other two there is,
    // and using it was wrong — `encoding_rs`'s EUC-JP and Shift_JIS are
    // WHATWG's, so a decode / re-encode round trip rewrote cells CRuby
    // leaves alone (`FC A1` came back as the JIS X 0212 `8F E3 A6`) and
    // silently accepted bytes onigenc calls broken (Shift_JIS `0x80`).
    // A same-encoding `invalid: :replace` *is* `String#scrub` in CRuby,
    // so it has to be the same walk here too.
    if src_enc == dst_enc
        && opts.invalid_replace
        && !opts.has_newline()
        && let Some((max_len, precise)) = crate::value::mbc_walker(src_enc)
    {
        let replace = opts.replace_str(dst_enc);
        return Ok(crate::value::scrub_mbc(
            src_bytes,
            replace.as_bytes(),
            max_len,
            precise,
        ));
    }
    if src_enc == dst_enc && !scrub_in_place {
        // The newline decorators still apply to a same-encoding
        // "conversion" (`"a\n".encode("UTF-8", crlf_newline: true)`).
        if opts.has_newline() && src_enc.is_ascii_compatible() {
            return Ok(apply_newline_bytes(src_bytes, opts));
        }
        if !opts.has_newline() {
            return Ok(src_bytes.to_vec());
        }
        // Non-ASCII-compatible encodings fall through to the decode /
        // re-encode pipeline so the decorators run on real characters.
    }
    // Fast path: 7-bit content + an ascii-compatible source copies
    // through unchanged into any byte-oriented destination. Covers the
    // "BINARY ASCII → UTF-8" and "UsAscii → X" cases (encoding_rs's
    // `us-ascii` → `windows-1252` mapping isn't what CRuby does, and
    // BINARY isn't a real encoding_rs encoding) and the dummy
    // destinations CRuby has no generic converter for (Emacs-Mule,
    // UTF-7, …): 7-bit content is valid in all of them, so `encode`
    // succeeds even though `Encoding::Converter.new` would raise.
    // We require the *source* to be ASCII-compatible — `Iso2022Jp`
    // looks like 7-bit input on the wire (it's a 7-bit encoding), but
    // its bytes carry ESC sequences that change interpretation, so the
    // identity copy would silently retag escape codes as ASCII
    // characters. UTF-7 is excluded on the other side for the same
    // reason: it re-spells `+` as `+-`, so 7-bit text is not a copy
    // there, and CRuby — which ships no UTF-7 converter at all —
    // answers `ConverterNotFoundError` even for `"ab"` (#1471).
    let all_ascii = src_bytes.iter().all(|&b| b < 0x80);
    if all_ascii
        && src_enc.is_ascii_compatible()
        && !is_utf16_or_32(dst_enc)
        && !is_utf7(dst_enc)
        && dummy_wide_target(dst_enc).is_none()
    {
        if opts.has_newline() {
            return Ok(apply_newline_bytes(src_bytes, opts));
        }
        return Ok(src_bytes.to_vec());
    }
    // 7-bit content from any ASCII-compatible source (incl. BINARY,
    // which `encoding_rs` doesn't model) widens cleanly into the
    // non-ASCII-compatible UTF-16/UTF-32 targets — every byte is a
    // valid one-codepoint character. `"def".b.encode("utf-32le")`
    // works in CRuby; without this it raised ConverterNotFound.
    if all_ascii && src_enc.is_ascii_compatible() && is_utf16_or_32(dst_enc) {
        let s = std::str::from_utf8(src_bytes).expect("bytes < 0x80 are valid UTF-8");
        if opts.has_newline() {
            return Ok(encode_utf16_32(&opts.apply_newline(s), dst_enc));
        }
        return Ok(encode_utf16_32(s, dst_enc));
    }
    // BINARY → ascii-compat with non-ASCII bytes is "undef":
    // there's no Unicode for "byte 0x82 in BINARY". A codec-less
    // destination (Emacs-Mule, Big5-UAO, …) is "converter not found"
    // instead — there is no transcoder to be undefined *in*.
    if src_enc == E::Ascii8 && dst_enc.is_ascii_compatible() {
        if matches!(dst_enc, E::NamedByte(_))
            && encoding_to_rs(dst_enc).is_none()
            && single_byte_table(dst_enc).is_none()
        {
            return Err(MonorubyErr::converter_not_found_error(
                store,
                format!(
                    "code converter not found ({} to {})",
                    src_enc.name(),
                    dst_enc.name()
                ),
            ));
        }
        return Err(MonorubyErr::undefined_conversion_error(
            store,
            format!(
                "\"\\x{:02X}\" from ASCII-8BIT to {}",
                src_bytes.iter().copied().find(|&b| b >= 0x80).unwrap_or(0),
                dst_enc.name()
            ),
        ));
    }
    // → BINARY. ASCII-8BIT is a byte bucket, not a character encoding:
    // CRuby has no conversion *to* it from any character above U+007F,
    // so every one of them is an `UndefinedConversionError` (the
    // all-ASCII fast path above already handled the content that does
    // convert). `String#b` and `#force_encoding("BINARY")` are the
    // reinterpret-these-bytes operations; `#encode("BINARY")` asks for
    // a conversion, and handing back the decoded pivot's UTF-8 bytes
    // would silently rewrite the caller's EUC-JP bytes as UTF-8.
    if dst_enc == E::Ascii8 {
        // Decode to the UTF-8 pivot first: the error names a character
        // (`U+3042`), and `undef: :replace` substitutes per character.
        let decoded: String = if is_utf16_or_32(src_enc) {
            let (decoded, decode_err) = decode_utf16_32(src_bytes, src_enc);
            if decode_err && !opts.invalid_replace {
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
            }
            decoded
        } else if let Some(table) = single_byte_table(src_enc) {
            if opts.undef_replace {
                table_decode_lossy(src_bytes, table, &opts.replace_str(dst_enc))
            } else {
                table_decode(src_bytes, table).map_err(|(_, b)| {
                    MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_byte_message(b, src_enc, dst_enc),
                    )
                })?
            }
        } else if let Some(src_rs) = encoding_to_rs(src_enc) {
            let (decoded, decode_err) = if let Some(fx) = jp_fixup(src_enc) {
                let d = jp_decode(fx, src_bytes, None);
                if let Some(cell) = d.unmapped {
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_cell_message(&cell, src_enc, dst_enc),
                    ));
                }
                (d.text, d.had_invalid)
            } else {
                src_rs.decode_without_bom_handling(src_bytes)
            };
            if decode_err && !opts.invalid_replace {
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
            }
            decoded.into_owned()
        } else {
            // No decoder for the source: nothing to say about which
            // character is undefined, so the bytes go through as they
            // always did.
            return Ok(src_bytes.to_vec());
        };
        // `invalid: :replace` substitutes at the invalid bytes, which
        // `encoding_rs` has already turned into U+FFFD — and U+FFFD is
        // itself undefined in BINARY, so it has to go now rather than
        // resurface below as an undefined conversion.
        let decoded = if decoded.contains('\u{FFFD}') && opts.invalid_replace {
            decoded.replace('\u{FFFD}', &opts.replace_str(dst_enc))
        } else {
            decoded
        };
        let decoded = if opts.has_newline() {
            opts.apply_newline(&decoded)
        } else {
            decoded
        };
        if let Some(bad) = decoded.chars().find(|c| !c.is_ascii()) {
            if !opts.undef_replace {
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_char_message(bad, src_enc, dst_enc),
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
        return Ok(decoded.into_bytes());
    }
    // Decode the source through encoding_rs (UsAscii is decoded as
    // UTF-8 since 7-bit ASCII bytes are identical in both — the
    // all_ascii fast-path above already covered the ASCII-only
    // case, so here we know src has a non-ASCII byte; UsAscii src
    // with non-ASCII content is invalid by definition).
    let (decoded, decode_err): (std::borrow::Cow<str>, bool) = if is_utf16_or_32(src_enc) {
        let (s, e) = decode_utf16_32(src_bytes, src_enc);
        (std::borrow::Cow::Owned(s), e)
    } else if let Some(table) = single_byte_table(src_enc) {
        // A table encoding has a character for every byte it assigns
        // one to, so no byte sequence in it is *invalid* — but a cell
        // the encoding leaves unassigned has no character to convert,
        // which is an undefined conversion.
        let decoded = if opts.undef_replace {
            table_decode_lossy(src_bytes, table, &opts.replace_str(dst_enc))
        } else {
            table_decode(src_bytes, table).map_err(|(_, b)| {
                MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_byte_message(b, src_enc, dst_enc),
                )
            })?
        };
        (std::borrow::Cow::Owned(decoded), false)
    } else {
        let src_rs = match encoding_to_rs(src_enc) {
            Some(s) => s,
            None if src_enc == E::UsAscii => {
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
            }
            None if src_enc == E::Ascii8 => {
                // BINARY with an 8-bit byte has no defined conversion
                // to a real codec: CRuby reports the first offending
                // byte as an UndefinedConversionError, spelling out the
                // UTF-8 pivot for non-UTF-8 destinations.
                let bad = src_bytes.iter().copied().find(|b| *b >= 0x80).unwrap_or(0);
                let msg = if dst_enc == E::Utf8 {
                    format!("\"\\x{bad:02X}\" from ASCII-8BIT to UTF-8")
                } else {
                    format!(
                        "\"\\x{bad:02X}\" to UTF-8 in conversion from ASCII-8BIT to UTF-8 to {}",
                        dst_enc.name()
                    )
                };
                return Err(MonorubyErr::undefined_conversion_error(store, msg));
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
        // EUC-JP goes through our own wrapper: `encoding_rs`'s is
        // WHATWG's, which disagrees with CRuby on eight cells and
        // accepts the NEC/IBM rows CRuby has no table for.
        if let Some(fx) = jp_fixup(src_enc) {
            let repl = opts.undef_replace.then(|| opts.replace_str(dst_enc));
            let d = jp_decode(fx, src_bytes, repl.as_deref());
            if let Some(cell) = d.unmapped {
                // A well-formed cell with no character: an undefined
                // conversion, which `invalid: :replace` does not cover.
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_cell_message(&cell, src_enc, dst_enc),
                ));
            }
            (d.text, d.had_invalid)
        } else {
            src_rs.decode_without_bom_handling(src_bytes)
        }
    };
    if decode_err && !opts.invalid_replace {
        return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
    }
    // `invalid: :replace`: the bytes `encoding_rs` turned into U+FFFD
    // are replaced with the *destination's* replacement string here.
    // Carrying U+FFFD into the encode half would make the invalid
    // sequence come back out as an *undefined* conversion (CRuby
    // substitutes at the point of the invalid bytes instead).
    let decoded: std::borrow::Cow<str> = if decode_err && opts.invalid_replace {
        let replace = opts.replace_str(dst_enc);
        if replace == "\u{FFFD}" {
            decoded
        } else {
            std::borrow::Cow::Owned(decoded.replace('\u{FFFD}', &replace))
        }
    } else {
        decoded
    };
    // Newline decorators run on the decoded text, between the decode
    // and encode halves.
    let decoded: std::borrow::Cow<str> = if opts.has_newline() {
        std::borrow::Cow::Owned(opts.apply_newline(&decoded))
    } else {
        decoded
    };
    // The endianness-less dummy UTF-16 / UTF-32: CRuby's encoder emits a
    // BOM followed by the big-endian form.
    if let Some(wide) = dummy_wide_target(dst_enc) {
        let mut out: Vec<u8> = match wide {
            E::Utf16Be => vec![0xFE, 0xFF],
            _ => vec![0x00, 0x00, 0xFE, 0xFF],
        };
        out.extend(encode_utf16_32(&decoded, wide));
        return Ok(out);
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
                    undefined_char_message(bad, src_enc, dst_enc),
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
    // UTF-16/UTF-32 destination: every Unicode scalar value is
    // representable, so there is no undefined-conversion case.
    if is_utf16_or_32(dst_enc) {
        return Ok(encode_utf16_32(&decoded, dst_enc));
    }
    // Single-byte-table destination (IBM437 &c.): encode via the
    // reverse table, honoring `undef: :replace`.
    if let Some(table) = single_byte_table(dst_enc) {
        return match table_encode(&decoded, table) {
            Ok(v) => Ok(v),
            Err(bad) if !opts.undef_replace => Err(MonorubyErr::undefined_conversion_error(
                store,
                undefined_char_message(bad, src_enc, dst_enc),
            )),
            Err(_) => {
                let replace = opts.replace_str(dst_enc);
                let mut out = Vec::with_capacity(decoded.len());
                for c in decoded.chars() {
                    let mut buf = [0u8; 4];
                    match table_encode(c.encode_utf8(&mut buf), table) {
                        Ok(v) => out.extend_from_slice(&v),
                        Err(_) => match table_encode(&replace, table) {
                            Ok(r) => out.extend_from_slice(&r),
                            Err(_) => out.push(b'?'),
                        },
                    }
                }
                Ok(out)
            }
        };
    }
    // EUC-JP has a second plane `encoding_rs` will not write, and
    // both Japanese codecs need the table corrections; go through the
    // encoder that knows about them.
    if let Some(fx) = jp_fixup(dst_enc) {
        match jp_encode(fx, &decoded) {
            Ok(v) => return Ok(v),
            Err(bad) if !opts.undef_replace => {
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_char_message(bad, src_enc, dst_enc),
                ));
            }
            Err(_) => {
                let replace = opts.replace_str(dst_enc);
                let mut out: Vec<u8> = Vec::with_capacity(decoded.len());
                let mut buf = [0u8; 4];
                for c in decoded.chars() {
                    match jp_encode(fx, c.encode_utf8(&mut buf)) {
                        Ok(v) => out.extend_from_slice(&v),
                        Err(_) => {
                            out.extend_from_slice(&jp_encode(fx, &replace).unwrap_or_default())
                        }
                    }
                }
                return Ok(out);
            }
        }
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
            // The character the destination cannot write, not merely
            // the first non-ASCII one: a String can hold plenty of
            // non-ASCII the encoder is perfectly happy with.
            let mut buf = [0u8; 4];
            let bad = decoded
                .chars()
                .find(|c| dst_rs.encode(c.encode_utf8(&mut buf)).2)
                .unwrap_or('\0');
            return Err(MonorubyErr::undefined_conversion_error(
                store,
                undefined_char_message(bad, src_enc, dst_enc),
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
fn handle_xml_option(
    globals: &mut Globals,
    lfp: Lfp,
    dst_enc: crate::value::Encoding,
) -> Result<Option<Value>> {
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
    let plain = TranscodeOpts::default();
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' if matches!(mode, XmlMode::Attr) => out.push_str("&quot;"),
            // A character the destination cannot represent becomes an
            // upper-case hexadecimal numeric character reference — the
            // whole point of the xml decorator.
            _ if !c.is_ascii()
                && transcode_bytes_with_opts(
                    c.to_string().as_bytes(),
                    crate::value::Encoding::Utf8,
                    dst_enc,
                    &plain,
                    &globals.store,
                )
                .is_err() =>
            {
                out.push_str(&format!("&#x{:X};", c as u32));
            }
            _ => out.push(c),
        }
    }
    if matches!(mode, XmlMode::Attr) {
        out.push('"');
    }
    let encoded = transcode_bytes_with_opts(
        out.as_bytes(),
        crate::value::Encoding::Utf8,
        dst_enc,
        &plain,
        &globals.store,
    )?;
    Ok(Some(Value::string_from_inner(
        crate::value::RStringInner::from_encoding_scanned(&encoded, dst_enc),
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
    let self_val = lfp.self_val();
    let self_enc = self_val.as_rstring_inner().encoding();
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc)?;
    // With no destination (and no `default_internal`) the conversion is
    // to the receiver's own encoding — the options still apply, so this
    // is not a no-op: `"a\n".encode(crlf_newline: true)` converts.
    let dst_enc = dst_enc_opt.unwrap_or(self_enc);
    if let Some(v) = handle_xml_option(globals, lfp, dst_enc)? {
        return Ok(v);
    }
    let opts = parse_transcode_opts(lfp);
    let fallback = parse_fallback_opt(lfp);
    let src_bytes = self_val.as_rstring_inner().as_bytes().to_vec();
    let bytes =
        transcode_with_fallback(vm, globals, &src_bytes, src_enc, dst_enc, &opts, fallback)?;
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
    let self_enc = self_val.as_rstring_inner().encoding();
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc)?;
    let dst_enc = dst_enc_opt.unwrap_or(self_enc);
    if let Some(v) = handle_xml_option(globals, lfp, dst_enc)? {
        // CRuby's `encode!` just `replace`s self with the encoded
        // form when xml is given.
        if let Some(inner) = v.is_rstring_inner() {
            self_val.replace_with_inner(inner.clone());
        }
        return Ok(self_val);
    }
    let opts = parse_transcode_opts(lfp);
    let fallback = parse_fallback_opt(lfp);
    let src_bytes = self_val.as_rstring_inner().as_bytes().to_vec();
    let bytes =
        transcode_with_fallback(vm, globals, &src_bytes, src_enc, dst_enc, &opts, fallback)?;
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
            // CRuby's `econv_opts`: a bare `replace:` implies
            // `undef: :replace` — but only when `invalid: :replace`
            // was *not* given, so `invalid: :replace, replace: ""`
            // still raises on an unconvertible character.
            if !out.invalid_replace {
                out.undef_replace = true;
            }
        }
    }
    for (key, flag) in [
        ("universal_newline", 0usize),
        ("crlf_newline", 1),
        ("cr_newline", 2),
    ] {
        if let Some(v) = find_hash_value_for_symbol(&hash, key)
            && v.as_bool()
        {
            match flag {
                0 => out.universal_newline = true,
                1 => out.crlf_newline = true,
                _ => out.cr_newline = true,
            }
        }
    }
    out
}

/// Pull the `fallback:` option value off `String#encode`'s kwargs (the
/// conversion-time callback consulted for undefined characters). Kept
/// out of [`TranscodeOpts`] because invoking it needs the VM.
fn parse_fallback_opt(lfp: Lfp) -> Option<Value> {
    let opts_val = get_options_hash_value(lfp)?;
    let hash = opts_val.try_hash_ty()?;
    let v = find_hash_value_for_symbol(&hash, "fallback")?;
    if v.is_nil() { None } else { Some(v) }
}

/// Whether `err` is an `Encoding::UndefinedConversionError`.
fn is_undefined_conversion_error(store: &Store, err: &MonorubyErr) -> bool {
    let crate::MonorubyErrKind::Other(cid) = err.kind() else {
        return false;
    };
    let Some(enc_const) = store.get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING) else {
        return false;
    };
    store
        .get_constant_noautoload(enc_const.as_class_id(), IdentId::get_id("UndefinedConversionError"))
        .map(|c| c.as_class_id())
        == Some(*cid)
}

/// Transcode with `String#encode`'s `fallback:` semantics: run the
/// plain conversion first, and when it fails with an
/// `UndefinedConversionError`, retry character by character consulting
/// the fallback object — anything responding to `[]` (Hash, Proc,
/// Method, an object with `#[]`) — for each unrepresentable character.
/// `nil` from the fallback re-raises the original error; a non-String
/// result goes through `#to_str` (never `#to_s`); a replacement the
/// destination cannot encode raises `ArgumentError: too big fallback
/// string`, all per CRuby.
fn transcode_with_fallback(
    vm: &mut Executor,
    globals: &mut Globals,
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    fallback: Option<Value>,
) -> Result<Vec<u8>> {
    let first = transcode_bytes_with_opts(src_bytes, src_enc, dst_enc, opts, &globals.store);
    let (err, fallback) = match (first, fallback) {
        (Ok(b), _) => return Ok(b),
        (Err(e), None) => return Err(e),
        (Err(e), Some(f)) => (e, f),
    };
    if !is_undefined_conversion_error(&globals.store, &err) {
        return Err(err);
    }
    let index_id = IdentId::get_id("[]");
    if globals.check_method(fallback, index_id).is_none() {
        return Err(err);
    }
    // Decode the whole source to characters first (honouring
    // `invalid:` and the newline decorators), then encode one
    // character at a time.
    let decode_opts = TranscodeOpts {
        invalid_replace: opts.invalid_replace,
        replace: opts.replace.clone(),
        ..Default::default()
    };
    let decoded_bytes = transcode_bytes_with_opts(
        src_bytes,
        src_enc,
        crate::value::Encoding::Utf8,
        &decode_opts,
        &globals.store,
    )?;
    let mut decoded = String::from_utf8_lossy(&decoded_bytes).into_owned();
    if opts.has_newline() {
        decoded = opts.apply_newline(&decoded);
    }
    let mut out: Vec<u8> = Vec::with_capacity(decoded.len());
    for c in decoded.chars() {
        let mut buf = [0u8; 4];
        let cs = c.encode_utf8(&mut buf);
        match transcode_bytes_with_opts(
            cs.as_bytes(),
            crate::value::Encoding::Utf8,
            dst_enc,
            &TranscodeOpts::default(),
            &globals.store,
        ) {
            Ok(b) => out.extend_from_slice(&b),
            Err(cerr) => {
                if !is_undefined_conversion_error(&globals.store, &cerr) {
                    return Err(cerr);
                }
                let rep =
                    vm.invoke_method_inner(globals, index_id, fallback, &[Value::string_from_str(cs)], None, None)?;
                if rep.is_nil() {
                    return Err(cerr);
                }
                let rep_str = if rep.is_str().is_some() {
                    rep
                } else if globals.check_method(rep, IdentId::TO_STR).is_some() {
                    let converted = vm.invoke_method_inner(globals, IdentId::TO_STR, rep, &[], None, None)?;
                    if converted.is_str().is_none() {
                        return Err(MonorubyErr::cant_convert_error(
                            globals,
                            rep,
                            converted,
                            "String",
                            IdentId::TO_STR,
                        ));
                    }
                    converted
                } else {
                    return Err(MonorubyErr::typeerr(format!(
                        "no implicit conversion of {} into String",
                        rep.builtin_class_name(globals)
                    )));
                };
                let inner = rep_str.as_rstring_inner();
                let rep_bytes = transcode_bytes_with_opts(
                    inner.as_bytes(),
                    inner.encoding(),
                    dst_enc,
                    &TranscodeOpts::default(),
                    &globals.store,
                )
                .map_err(|_| MonorubyErr::argumenterr("too big fallback string"))?;
                out.extend_from_slice(&rep_bytes);
            }
        }
    }
    Ok(out)
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
    if let Some(enc_obj) = globals.store.get_ivar(self_, IdentId::_ENCODING_OVERRIDE) {
        return Ok(enc_obj);
    }
    let enc = self_.as_rstring_inner().encoding();
    // The `Encoding::<NAME>` object for `enc` is the same every time;
    // resolve it through the constant table once and answer from the
    // memo after that (a call used to intern the constant's name and
    // walk two constant lookups).
    if let Some(obj) = globals.cached_encoding_object(enc) {
        return Ok(obj);
    }
    let enc_class = vm
        .get_constant_checked(globals, OBJECT_CLASS, IdentId::ENCODING)?
        .expect_class(globals)?
        .id();
    let const_name = encoding_constant_name(enc);
    let res = vm.get_constant_checked(globals, enc_class, IdentId::get_id(const_name))?;
    globals.cache_encoding_object(enc, res);
    Ok(res)
}

/// Map an `Encoding` to the corresponding `Encoding::<NAME>` Ruby
/// constant name registered by `init_encoding`.
pub(crate) fn encoding_constant_name(enc: Encoding) -> &'static str {
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
        Encoding::Sjis(2) => "MacJapanese",
        Encoding::Sjis(_) => "Windows_31J",
        Encoding::Iso2022Jp => "ISO_2022_JP",
        // Name-preserving byte encodings: map the canonical display
        // name back to its registered `Encoding::<CONST>` identifier.
        Encoding::Other(i) => match i {
            0 => "UTF_7",
            1 => "CP50220",
            2 => "CP50221",
            3 => "UTF_16",
            4 => "UTF_32",
            _ => "ASCII_8BIT",
        },
        Encoding::NamedByte(i) => crate::value::named_byte_const_name(i),
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
    let enc = value_to_encoding(vm, globals, lfp.arg(0))?;
    lfp.self_val().as_rstring_inner_mut().set_encoding(enc);
    Ok(lfp.self_val())
}

/// The four names that do not name a fixed encoding: `"internal"`,
/// `"external"`, `"locale"` and `"filesystem"` resolve against the
/// running interpreter's defaults. `"internal"` with no default
/// internal encoding set is BINARY (CRuby's `rb_to_encoding` falls
/// back to ASCII-8BIT there, unlike `Encoding.find`, which answers
/// nil). Returns `None` for every other name.
fn special_encoding_name(globals: &mut Globals, name: &str) -> Option<Encoding> {
    let lowered = name.to_ascii_lowercase();
    let value = match lowered.as_str() {
        "internal" => {
            let internal = globals
                .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
                .filter(|v| !v.is_nil());
            match internal {
                Some(v) => v,
                None => return Some(Encoding::Ascii8),
            }
        }
        "external" | "filesystem" => globals
            .get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
            .filter(|v| !v.is_nil())
            .unwrap_or_else(|| Value::nil()),
        "locale" => locale_encoding_value(globals),
        _ => return None,
    };
    globals.encoding_of_object(value).or(Some(Encoding::Utf8))
}

/// Resolve an encoding operand — an `Encoding` object, a String name, or
/// anything `#to_str`-coercible — to a monoruby `Encoding` (the argument
/// convention shared by `String#force_encoding` and
/// `String.new(encoding:)`).
pub(super) fn value_to_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    arg0: Value,
) -> Result<Encoding> {
    if let Some(s) = arg0.is_str() {
        if let Some(enc) = special_encoding_name(globals, s) {
            return Ok(enc);
        }
        Encoding::try_from_str(s)
    } else if let Some(enc) = globals.encoding_of_object(arg0) {
        // An `Encoding::<NAME>` constant object: its `Encoding` was
        // recorded at init, so no name is read or parsed.
        Ok(enc)
    } else if arg0.class() == encoding_class(globals) {
        let s = globals.store.get_ivar(arg0, IdentId::_ENCODING).unwrap();
        Encoding::try_from_str(s.as_str())
    } else {
        // Try to_str coercion
        let s = arg0.coerce_to_string(vm, globals)?;
        Encoding::try_from_str(&s)
    }
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
    //
    // A string in an encoding that is not ASCII-compatible is never
    // ascii_only?, whatever its bytes say — an empty UTF-16 string
    // answers `false`, because `\0` there is not the ASCII NUL
    // (CRuby's `rb_enc_str_asciionly_p` asks the encoding first).
    let self_ = lfp.self_val();
    let inner = self_.as_rstring_inner();
    Ok(Value::bool(
        inner.encoding().is_ascii_compatible() && inner.is_ascii_only(),
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
    if let Some(v) = globals.get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL")) {
        if !v.is_nil() {
            return Ok(v);
        }
    }
    let enc_class = encoding_class(globals);
    let utf8 = globals
        .store
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
        .unwrap_or(Value::nil());
    Ok(utf8)
}

///
/// The `Encoding` object an `Encoding.default_external=` /
/// `default_internal=` argument names.
///
/// An `Encoding` is taken as it is; anything else must be a String or
/// convertible to one with `#to_str`, and is then resolved through
/// `Encoding.find`. No `#to_str`, or one that answers a non-String, is
/// the `TypeError` CRuby raises — the previous code stored any such
/// object verbatim, so `Encoding.default_internal` could answer
/// something that is not an encoding at all.
///
fn resolve_default_encoding_arg(
    vm: &mut Executor,
    globals: &mut Globals,
    enc_class: Value,
    val: Value,
) -> Result<Value> {
    let enc_class_id = enc_class.expect_class_or_module(&globals.store)?.id();
    if val.class() == enc_class_id {
        return Ok(val);
    }
    let name = if val.is_str().is_some() {
        val
    } else {
        let converted = match globals.check_method(val, IdentId::TO_STR) {
            Some(fid) => vm.invoke_func_inner(globals, fid, val, &[], None, None)?,
            None => {
                return Err(MonorubyErr::no_implicit_conversion(
                    &globals.store,
                    val,
                    STRING_CLASS,
                ));
            }
        };
        if converted.is_str().is_none() {
            // A `#to_str` that ran and answered the wrong type is
            // CRuby's "can't convert X to String (X#to_str gives Y)",
            // not the "no implicit conversion" of a missing one.
            return Err(MonorubyErr::cant_convert_error(
                &globals.store,
                val,
                converted,
                "String",
                IdentId::TO_STR,
            ));
        }
        converted
    };
    let find_id = IdentId::get_id("find");
    vm.invoke_method_inner(globals, find_id, enc_class, &[name], None, None)
}

///
/// ### Encoding.default_external=
/// - default_external = enc -> enc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/default_external=3d.html]
#[monoruby_builtin]
fn enc_set_default_external(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let val = lfp.arg(0);
    if val.is_nil() {
        return Err(MonorubyErr::argumenterr(
            "default external can not be nil",
        ));
    }
    let enc_val = resolve_default_encoding_arg(vm, globals, lfp.self_val(), val)?;
    globals.set_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"), enc_val);
    refresh_inspect_escape(globals);
    Ok(enc_val)
}

/// The encoding `#inspect` renders into, when that is not UTF-8.
///
/// CRuby escapes every character its *result encoding* cannot show and
/// tags the rendering with that encoding — `rb_str_inspect`'s `resenc`,
/// which is `Encoding.default_internal` when one is set and
/// `default_external` otherwise, with US-ASCII standing in for an
/// encoding that is not ASCII-compatible. monoruby's strings are UTF-8,
/// so that comes down to: escape unless the result encoding is UTF-8,
/// which with the locale-derived default is the difference between
/// `p "い"` under a `C` locale and under a UTF-8 one.
fn inspect_escape_encoding(globals: &mut Globals) -> Option<Encoding> {
    let resenc = globals
        .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
        .filter(|v| !v.is_nil())
        .or_else(|| {
            globals
                .get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
                .filter(|v| !v.is_nil())
        })?;
    let enc = globals.encoding_of_object(resenc)?;
    match enc {
        Encoding::Utf8 => None,
        e if !e.is_ascii_compatible() => Some(Encoding::UsAscii),
        e => Some(e),
    }
}

/// Wrap an `#inspect` rendering in the string CRuby would hand back:
/// escaped to `\uXXXX` and tagged with the result encoding when that
/// cannot show non-ASCII ([`inspect_escape_encoding`]), the UTF-8 text
/// itself otherwise.
pub(crate) fn inspect_result(globals: &mut Globals, s: String) -> Value {
    if globals.store.inspect_escape() {
        // Tagged with the result encoding even when the rendering came
        // out ASCII anyway: CRuby associates `resenc` with what
        // `rb_str_inspect` built, so `"abc".inspect.encoding` is
        // US-ASCII under a `C` locale, EUC-JP under `-E EUC-JP`, and
        // UTF-8 under a UTF-8 locale.
        Value::string_from_inner(RStringInner::from_encoding(
            crate::value::escape_nonascii_to_u(&s).as_bytes(),
            globals.store.inspect_escape_encoding(),
        ))
    } else {
        Value::string(s)
    }
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
/// Most recent `primitive_convert` outcome as the
/// `[result, src_enc_name, dst_enc_name, error_bytes, readagain_bytes]`
/// 5-tuple `Encoding::Converter#primitive_errinfo` returns.
const CONVERTER_ERRINFO_IVAR: &str = "/converter_errinfo";
/// Source bytes that the previous `primitive_convert` call accepted
/// but couldn't produce output for (typically because the
/// destination buffer hit its `dst_bytesize` cap mid-conversion).
/// Prepended to the next call's source bytes so streaming works
/// across multiple calls. Stored as a binary String value.
const CONVERTER_PENDING_IVAR: &str = "/converter_pending";
/// Read-again bytes buffered by an `:invalid_byte_sequence` outcome,
/// returned (and drained) by `Encoding::Converter#putback`.
const CONVERTER_READAGAIN_IVAR: &str = "/converter_readagain";
/// Structured data of the last conversion error (for `#last_error` /
/// the error-object attribute readers): `[kind, msg, error_bytes,
/// readagain_bytes, stage_src, stage_dst]`, or absent/nil when the
/// last outcome was not an error.
const CONVERTER_LAST_ERROR_IVAR: &str = "/converter_last_error";
/// Conversion flags configured at construction (`invalid: :replace`
/// / `undef: :replace` kwargs, or the `INVALID_REPLACE` /
/// `UNDEF_REPLACE` Integer-flag bits). Stored as a Fixnum:
/// bit0 = invalid→replace, bit1 = undef→replace.
const CONVERTER_FLAGS_IVAR: &str = "/converter_flags";
const CONVERTER_FLAG_INVALID_REPLACE: i64 = 0b01;
const CONVERTER_FLAG_UNDEF_REPLACE: i64 = 0b10;

/// Build the `TranscodeOpts` for a Converter instance from its
/// stored replacement string + conversion flags. Shared by
/// `#convert` and `#primitive_convert` so both honour
/// `invalid:`/`undef: :replace` and a user-set `#replacement=`.
fn converter_transcode_opts(globals: &Globals, recv: Value) -> TranscodeOpts {
    let mut opts = TranscodeOpts::default();
    if let Some(v) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_REPLACE_IVAR))
        && let Some(s) = v.is_str()
    {
        opts.replace = Some(s.to_string());
    }
    if let Some(v) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_FLAGS_IVAR))
        && let Some(n) = v.try_fixnum()
    {
        opts.invalid_replace = n & CONVERTER_FLAG_INVALID_REPLACE != 0;
        opts.undef_replace = n & CONVERTER_FLAG_UNDEF_REPLACE != 0;
        // The newline-decorator bits are kept in the flags ivar for
        // `#convpath`; wiring them into the transcode itself arrives
        // with the TranscodeOpts decorator support (PR #1018).
    }
    opts
}

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
        || single_byte_table(src).is_some()
        || is_utf16_or_32(src)
        || matches!(
            src,
            crate::value::Encoding::Ascii8 | crate::value::Encoding::UsAscii
        );
    let dst_supported = encoding_to_rs(dst).is_some()
        || single_byte_table(dst).is_some()
        || is_utf16_or_32(dst)
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
    // Resolve to canonical constant names *once* (so a `#to_str`
    // mock argument is converted exactly once, per spec).
    let src_name = encode_resolve_enc_arg(vm, globals, lfp.arg(0))?;
    let dst_name = encode_resolve_enc_arg(vm, globals, lfp.arg(1))?;
    let src = encoding_from_canonical_name(src_name).ok_or_else(|| {
        MonorubyErr::converter_not_found_error(
            &globals.store,
            format!("code converter not found ({})", src_name),
        )
    })?;
    let dst = encoding_from_canonical_name(dst_name).ok_or_else(|| {
        MonorubyErr::converter_not_found_error(
            &globals.store,
            format!("code converter not found ({})", dst_name),
        )
    })?;
    // CRuby raises `Encoding::ConverterNotFoundError` for identical
    // source/destination encodings — there is no "X to X" transcoder.
    // Compare the resolved canonical *names*, not the internal
    // `Encoding`: monoruby folds aliases like `UTF8-MAC` onto
    // `Utf8`, but CRuby treats them as distinct and DOES build a
    // converter (`Converter.new(UTF_8, UTF8_MAC)` is valid).
    if src_name == dst_name {
        return Err(MonorubyErr::converter_not_found_error(
            &globals.store,
            format!("code converter not found ({} to {})", src.name(), dst.name()),
        ));
    }
    validate_converter_pair(src, dst, &globals.store)?;
    // Options Hash (`replace:` kwargs / `**opts` / trailing Hash).
    // With `kw_rest=true` the collected kwargs Hash is delivered in
    // the slot after the positional args (index 3 here); a literal
    // positional Hash lands at index 2. An Integer 3rd arg is a
    // flags bitmask and carries no `replace:`. The encoding args at
    // 0/1 are never Hashes, so scanning 2..=3 is unambiguous.
    let opts_hash = (2..=3)
        .filter_map(|i| lfp.try_arg(i))
        .find_map(|v| v.try_hash_ty());
    // Conversion flags: either an Integer 3rd arg (the
    // `INVALID_REPLACE` / `UNDEF_REPLACE` bitmask) or
    // `invalid:`/`undef: :replace` kwargs.
    let mut flags: i64 = 0;
    let mut replace_inner: Option<crate::value::RStringInner> = None;
    if let Some(n) = lfp.try_arg(2).and_then(|v| v.try_fixnum()) {
        if n & 0x0000_0002 != 0 {
            flags |= CONVERTER_FLAG_INVALID_REPLACE;
        }
        if n & 0x0000_0020 != 0 {
            flags |= CONVERTER_FLAG_UNDEF_REPLACE;
        }
        // Newline decorator bits pass through verbatim.
        flags |= n & (0x0000_0100 | 0x0000_1000 | 0x0000_2000);
    }
    if let Some(hash) = opts_hash {
        {
            if let Some(v) = find_hash_value_for_symbol(&hash, "invalid")
                && v.try_symbol().map(|s| s.get_name() == "replace") == Some(true)
            {
                flags |= CONVERTER_FLAG_INVALID_REPLACE;
            }
            if let Some(v) = find_hash_value_for_symbol(&hash, "undef")
                && v.try_symbol().map(|s| s.get_name() == "replace") == Some(true)
            {
                flags |= CONVERTER_FLAG_UNDEF_REPLACE;
            }
            for (key, bit) in [
                ("universal_newline", 0x0000_0100i64),
                ("crlf_newline", 0x0000_1000),
                ("cr_newline", 0x0000_2000),
            ] {
                if let Some(v) = find_hash_value_for_symbol(&hash, key)
                    && v.as_bool()
                {
                    flags |= bit;
                }
            }
            if let Some(rep) = find_hash_value_for_symbol(&hash, "replace") {
                // `replace: nil` → keep the destination's default
                // replacement (handled lazily by `#replacement`).
                if !rep.is_nil() {
                    // CRuby coerces via `#to_str`; a non-String
                    // return or an object without `#to_str`
                    // (true/false/Integer) raises TypeError. This
                    // re-enters Ruby, so it runs BEFORE the converter
                    // object exists — holding the fresh object in a
                    // Rust local across `#to_str` left it invisible
                    // to the GC (caught by true `gc-stress`).
                    let s = if let Some(st) = rep.is_str() {
                        st.to_string()
                    } else {
                        rep.coerce_to_string(vm, globals)?
                    };
                    let mut inner = crate::value::RStringInner::from_string_scanned(s);
                    inner.set_encoding(dst);
                    replace_inner = Some(inner);
                }
            }
        }
    }
    let class = lfp.self_val();
    let obj = Value::object(class.as_class_id());
    // Stash the encodings as their canonical *names* so
    // `encoding_value` and `try_from_str` can round-trip without
    // a separate Encoding-to-id table. `set_ivar` never re-enters
    // Ruby, so `obj` needs no temp-stack rooting here.
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
    if let Some(inner) = replace_inner {
        let _ = globals.store.set_ivar(
            obj,
            IdentId::get_id(CONVERTER_REPLACE_IVAR),
            Value::string_from_inner(inner),
        );
    }
    if flags != 0 {
        let _ = globals.store.set_ivar(
            obj,
            IdentId::get_id(CONVERTER_FLAGS_IVAR),
            Value::integer(flags),
        );
    }
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
    } else if dst == crate::value::Encoding::UsAscii && !s.is_ascii() {
        // `encoding_rs` has no US-ASCII encoder; a non-ASCII
        // replacement is unrepresentable there (CRuby raises).
        return Err(MonorubyErr::undefined_conversion_error(
            &globals.store,
            format!(
                "U+{:04X} from UTF-8 to US-ASCII",
                s.chars().find(|c| !c.is_ascii()).map(|c| c as u32).unwrap_or(0)
            ),
        ));
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
    // Honour the configured replacement + `invalid:`/`undef:
    // :replace` flags (a user-set `#replacement=` too).
    let opts = converter_transcode_opts(globals, recv);
    if opts.invalid_replace || opts.undef_replace {
        // Replacement mode cannot error on content — the single-shot
        // transcoder suffices.
        let out = transcode_bytes_with_opts(&bytes, src, dst, &opts, &globals.store)?;
        let meta = ErrMeta::default();
        store_conversion_outcome(
            globals,
            recv,
            StreamConvertResult::SourceBufferEmpty,
            &meta,
            src,
            dst,
        );
        return Ok(Value::string_from_inner(
            crate::value::RStringInner::from_encoding_scanned(&out, dst),
        ));
    }
    // Streamed conversion: prepend bytes buffered by a previous
    // partial call, convert with `partial_input` semantics (a
    // trailing incomplete character is buffered, not an error), and
    // raise — leaving `primitive_errinfo` / `last_error` /
    // `putback` observable — on invalid / undefined input.
    let pending_id = IdentId::get_id(CONVERTER_PENDING_IVAR);
    let mut input: Vec<u8> = globals
        .store
        .get_ivar(recv, pending_id)
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    input.extend_from_slice(&bytes);
    let (result, consumed, out, meta) = stream_convert(&input, src, dst, None, true, &opts);
    match result {
        StreamConvertResult::Finished
        | StreamConvertResult::SourceBufferEmpty
        | StreamConvertResult::DestinationBufferFull => {
            let tail = &input[consumed..];
            let pending_val = if tail.is_empty() {
                Value::nil()
            } else {
                binary_string(tail)
            };
            let _ = globals.store.set_ivar(recv, pending_id, pending_val);
            store_conversion_outcome(
                globals,
                recv,
                StreamConvertResult::SourceBufferEmpty,
                &ErrMeta::default(),
                src,
                dst,
            );
        }
        _ => {
            let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
            let msg = store_conversion_outcome(globals, recv, result, &meta, src, dst)
                .unwrap_or_default();
            return Err(converter_last_error_raise(globals, recv, result, msg));
        }
    }
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
    let src_enc = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    // Input buffered by a partial `#convert` that never completed is an
    // incomplete-input error at finish time (CRuby).
    let pending_id = IdentId::get_id(CONVERTER_PENDING_IVAR);
    let pending: Vec<u8> = globals
        .store
        .get_ivar(recv, pending_id)
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    if !pending.is_empty() {
        let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
        let meta = ErrMeta {
            error_bytes: pending,
            readagain_bytes: vec![],
        };
        let msg = store_conversion_outcome(
            globals,
            recv,
            StreamConvertResult::IncompleteInput,
            &meta,
            src_enc,
            dst,
        )
        .unwrap_or_default();
        return Err(converter_last_error_raise(
            globals,
            recv,
            StreamConvertResult::IncompleteInput,
            msg,
        ));
    }
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

/// Outcome of `stream_convert`. Maps 1:1 to the Symbol returned
/// from `Encoding::Converter#primitive_convert`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StreamConvertResult {
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
    fn symbol_name(self) -> &'static str {
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
struct ErrMeta {
    /// The bytes that are definitely part of the erroneous sequence.
    error_bytes: Vec<u8>,
    /// Bytes consumed while detecting the error that should be
    /// re-examined (CRuby's read-again bytes).
    readagain_bytes: Vec<u8>,
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
) -> usize {
    if pivot_bytes == 0 {
        return 0;
    }
    let decoded_len = |n: usize| -> usize {
        let (_, _, out, _) = stream_convert(
            &src_bytes[..n],
            src_enc,
            crate::value::Encoding::Utf8,
            None,
            true,
            opts,
        );
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
    let Some((error_bytes, readagain_bytes, incomplete)) =
        first_bad_sequence(src_enc, src_bytes)
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

fn stream_convert(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    max_dst_bytes: Option<usize>,
    partial_input: bool,
    opts: &TranscodeOpts,
) -> (StreamConvertResult, usize, Vec<u8>, ErrMeta) {
    use crate::value::Encoding as E;
    // Identity-copy fast path: only safe when the *source* is
    // known-valid for its declared encoding. Same-encoding pairs
    // (UTF-8 → UTF_8_MAC alias, UTF-8 → UTF-8, …) still need
    // validation because the spec requires
    // `:invalid_byte_sequence` reporting on bad input. Restrict
    // the fast path to the all-ASCII / ASCII-compatible case
    // where every byte is unambiguously valid.
    let all_ascii = src_bytes.iter().all(|&b| b < 0x80);
    if all_ascii && src_enc.is_ascii_compatible() && dst_enc.is_ascii_compatible() {
        let limit = max_dst_bytes.unwrap_or(src_bytes.len()).min(src_bytes.len());
        let result = if limit < src_bytes.len() {
            StreamConvertResult::DestinationBufferFull
        } else {
            StreamConvertResult::Finished
        };
        return (result, limit, src_bytes[..limit].to_vec(), ErrMeta::default());
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
            let (_, _, out, _) =
                stream_convert(&src_bytes[..at], src_enc, dst_enc, max_dst_bytes, false, opts);
            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
            return (kind, at, out, meta);
        }
        let (result, pivot_consumed, out, meta) = stream_convert(
            pivot.as_bytes(),
            E::Utf8,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
        );
        // Back to source bytes: two per UTF-16 code unit (so four for a
        // surrogate pair), four per UTF-32 character.
        let wide = matches!(src_enc, E::Utf32Le | E::Utf32Be);
        let consumed = pivot[..pivot_consumed]
            .chars()
            .map(|c| if wide { 4 } else { c.len_utf16() * 2 })
            .sum();
        return (result, consumed, out, meta);
    }
    if is_utf16_or_32(dst_enc) {
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, E::Utf8, None, partial_input, opts);
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
                return (
                    StreamConvertResult::DestinationBufferFull,
                    pivot_prefix_consumed(src_bytes, src_enc, at, opts),
                    out,
                    ErrMeta::default(),
                );
            }
            out.extend_from_slice(&unit);
        }
        return (result, consumed, out, meta);
    }
    // A single-byte table encoding on either side: `encoding_rs` has
    // no codec for these, or resolves their label to a Windows code
    // page that is not the same encoding (#1508). They are stateless
    // and one byte per character, so the pivot is built here and the
    // rest of the pair goes through the ordinary path.
    if let Some(table) = single_byte_table(src_enc) {
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
                    let (_, _, out, _) =
                        stream_convert(&src_bytes[..at], src_enc, dst_enc, max_dst_bytes, false, opts);
                    return (
                        StreamConvertResult::UndefinedConversion,
                        at + 1,
                        out,
                        ErrMeta {
                            error_bytes: vec![b],
                            readagain_bytes: vec![],
                        },
                    );
                }
            }
        };
        let (result, pivot_consumed, out, meta) = stream_convert(
            pivot.as_bytes(),
            E::Utf8,
            dst_enc,
            max_dst_bytes,
            partial_input,
            opts,
        );
        // One source byte per pivot character, so the count converts
        // back by counting characters rather than bytes.
        let consumed = pivot[..pivot_consumed].chars().count();
        return (result, consumed, out, meta);
    }
    if let Some(table) = single_byte_table(dst_enc) {
        // Decode with the ordinary path, then map the pivot through the
        // table: one destination byte per character, so a destination
        // cap falls on a character boundary by construction.
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, E::Utf8, None, partial_input, opts);
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
                        pivot_prefix_consumed(src_bytes, src_enc, upto, opts),
                        out,
                        ErrMeta {
                            error_bytes: c.to_string().into_bytes(),
                            readagain_bytes: vec![],
                        },
                    );
                }
            }
            if let Some(max) = max_dst_bytes
                && out.len() > max
            {
                out.truncate(max);
                return (StreamConvertResult::DestinationBufferFull, consumed, out, ErrMeta::default());
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
                    return (
                        StreamConvertResult::UndefinedConversion,
                        src_bytes.len(),
                        out,
                        ErrMeta {
                            error_bytes: ch.to_string().into_bytes(),
                            readagain_bytes: vec![],
                        },
                    );
                }
                if let Some(max) = max_dst_bytes
                    && out.len() > max
                {
                    out.truncate(max);
                    return (StreamConvertResult::DestinationBufferFull, src_bytes.len(), out, ErrMeta::default());
                }
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
                    DecoderResult::InputEmpty => break,
                    DecoderResult::Malformed(..) => {
                        if !opts.invalid_replace {
                            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, last);
                            return (kind, src_bytes.len(), out, meta);
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
                Err(_) if opts.invalid_replace => {
                    for ch in String::from_utf8_lossy(src_bytes).chars() {
                        push_char!(ch);
                    }
                }
                Err(_) => {
                    let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
                    return (kind, src_bytes.len(), out, meta);
                }
            }
        }
        return (StreamConvertResult::Finished, src_bytes.len(), out, ErrMeta::default());
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

    let mut decoder = src_rs.new_decoder_without_bom_handling();
    let mut encoder = dst_rs.new_encoder();

    // UTF-8 intermediate buffer. encoding_rs's
    // `decode_to_utf8_without_replacement` returns
    // `DecoderResult::Malformed` on bad input rather than substituting
    // a replacement char (which is what we want for the symbol-based
    // outcome to be useful).
    let mut utf8_buf = vec![0u8; src_bytes.len() + 16];
    let last = !partial_input;
    let (decode_result, src_read, utf8_written) = decoder
        .decode_to_utf8_without_replacement(src_bytes, &mut utf8_buf, last);

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
    let (encode_result, utf8_read, out_written) = encoder
        .encode_from_utf8_without_replacement(utf8_str, &mut out_buf, last);
    out_buf.truncate(out_written);

    use encoding_rs::DecoderResult;
    use encoding_rs::EncoderResult;
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
            let src_through_error =
                src_offset_for_utf8_prefix(src_rs, src_bytes, utf8_read);
            (
                StreamConvertResult::UndefinedConversion,
                src_through_error.unwrap_or(src_read),
                out_buf,
                ErrMeta {
                    // The erroneous bytes are reported in the stage
                    // source encoding — the UTF-8 pivot.
                    error_bytes: c.to_string().into_bytes(),
                    readagain_bytes: vec![],
                },
            )
        }
        // Output buffer hit its cap. Caller should grow it and call
        // again on the remaining `src`.
        (_, EncoderResult::OutputFull) => {
            // We may have consumed *all* src bytes already (the encoder
            // ran out before the decoder did). Report dst-full so the
            // caller knows.
            (
                StreamConvertResult::DestinationBufferFull,
                src_read,
                out_buf,
                ErrMeta::default(),
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
                let pending_len = (unit..run.len()).rev().step_by(unit).find(|k| {
                    let mut probe = src_rs.new_decoder_without_bom_handling();
                    let mut probe_dst = vec![0u8; run.len() * 4 + 16];
                    let (r, read, written) = probe.decode_to_utf8_without_replacement(
                        &run[..*k],
                        &mut probe_dst,
                        false,
                    );
                    // Still waiting for more input, having produced
                    // nothing: a genuine incomplete prefix.
                    matches!(r, DecoderResult::InputEmpty) && read == *k && written == 0
                });
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
                (StreamConvertResult::Finished, src_read, out_buf, ErrMeta::default())
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
fn quote_error_bytes(bytes: &[u8]) -> String {
    let mut out = String::from("\"");
    for &b in bytes {
        if (0x20..0x7f).contains(&b) && b != b'"' && b != b'\\' {
            out.push(b as char);
        } else {
            out.push_str(&format!("\\x{:02X}", b));
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
fn first_bad_sequence(enc: crate::value::Encoding, bytes: &[u8]) -> Option<(Vec<u8>, Vec<u8>, bool)> {
    use crate::value::Encoding as E;
    match enc {
        E::UsAscii => bytes
            .iter()
            .position(|b| *b >= 0x80)
            .map(|i| (vec![bytes[i]], vec![], false)),
        E::Utf16Le | E::Utf16Be => first_bad_utf16(bytes, enc == E::Utf16Be),
        E::Utf32Le | E::Utf32Be => first_bad_utf32(bytes, enc == E::Utf32Be),
        // The table encodings are total over the byte range, and BINARY
        // is a byte bucket: no sequence in either is ill-formed, and
        // neither has a decoder to ask.
        _ if single_byte_table(enc).is_some() => None,
        _ => first_bad_via_rs(encoding_to_rs(enc)?, bytes),
    }
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
            let err = bytes[i..i + 2].to_vec();
            return Some(if be {
                match bytes.get(i + 2) {
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
fn invalid_byte_sequence(
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
    let detail = Value::array_from_vec(vec![
        Value::string_from_str(&src_enc.name()),
        Value::string_from_str(&dst_enc.name()),
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
fn error_stage_names(
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    decode_stage: bool,
) -> (String, String) {
    if decode_stage {
        let stage_dst = if src_enc.is_utf8_compatible() {
            dst_enc.name().to_string()
        } else {
            "UTF-8".to_string()
        };
        (src_enc.name().to_string(), stage_dst)
    } else {
        let stage_src = if src_enc.is_utf8_compatible() {
            src_enc.name().to_string()
        } else {
            "UTF-8".to_string()
        };
        (stage_src, dst_enc.name().to_string())
    }
}

/// Build the CRuby-compatible message for a conversion error.
fn conversion_error_message(
    result: StreamConvertResult,
    meta: &ErrMeta,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let (stage_src, stage_dst) = match result {
        StreamConvertResult::UndefinedConversion => error_stage_names(src_enc, dst_enc, false),
        _ => error_stage_names(src_enc, dst_enc, true),
    };
    match result {
        StreamConvertResult::UndefinedConversion => {
            let cp = std::str::from_utf8(&meta.error_bytes)
                .ok()
                .and_then(|s| s.chars().next())
                .map(|c| c as u32)
                .unwrap_or(0);
            if src_enc.is_utf8_compatible() {
                format!("U+{:04X} from {} to {}", cp, stage_src, stage_dst)
            } else {
                // Two-hop path: show the whole conversion chain.
                format!(
                    "U+{:04X} to {} in conversion from {} to UTF-8 to {}",
                    cp,
                    stage_dst,
                    src_enc.name(),
                    dst_enc.name()
                )
            }
        }
        StreamConvertResult::IncompleteInput => {
            format!(
                "incomplete {} on {}",
                quote_error_bytes(&meta.error_bytes),
                stage_src
            )
        }
        _ => {
            if meta.readagain_bytes.is_empty() {
                // CRuby names the bytes and the encoding, with no
                // leading phrase — the same text `String#encode` builds
                // in `invalid_byte_sequence_message`.
                format!("{} on {}", quote_error_bytes(&meta.error_bytes), stage_src)
            } else {
                format!(
                    "{} followed by {} on {}",
                    quote_error_bytes(&meta.error_bytes),
                    quote_error_bytes(&meta.readagain_bytes),
                    stage_src
                )
            }
        }
    }
}

/// A BINARY-tagged String value from raw bytes.
fn binary_string(bytes: &[u8]) -> Value {
    let mut s = crate::value::RStringInner::from_encoding_scanned(bytes, crate::value::Encoding::Ascii8);
    s.set_encoding(crate::value::Encoding::Ascii8);
    Value::string_from_inner(s)
}

/// Record the outcome of a conversion step on the converter object:
/// the `primitive_errinfo` tuple, the read-again buffer, and the
/// structured last-error data. Returns the error message when the
/// outcome was an error (for the raising callers).
fn store_conversion_outcome(
    globals: &mut Globals,
    recv: Value,
    result: StreamConvertResult,
    meta: &ErrMeta,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> Option<String> {
    let is_error = matches!(
        result,
        StreamConvertResult::InvalidByteSequence
            | StreamConvertResult::UndefinedConversion
            | StreamConvertResult::IncompleteInput
    );
    let errinfo = if is_error {
        let decode_stage = !matches!(result, StreamConvertResult::UndefinedConversion);
        let (stage_src, stage_dst) = error_stage_names(src_enc, dst_enc, decode_stage);
        Value::array_from_iter(
            [
                Value::symbol_from_str(result.symbol_name()),
                Value::string(stage_src),
                Value::string(stage_dst),
                binary_string(&meta.error_bytes),
                binary_string(&meta.readagain_bytes),
            ]
            .into_iter(),
        )
    } else {
        Value::array_from_iter(
            [
                Value::symbol_from_str(result.symbol_name()),
                Value::nil(),
                Value::nil(),
                Value::nil(),
                Value::nil(),
            ]
            .into_iter(),
        )
    };
    let _ = globals
        .store
        .set_ivar(recv, IdentId::get_id(CONVERTER_ERRINFO_IVAR), errinfo);
    // Read-again buffer, for `#putback`.
    let ra = if matches!(result, StreamConvertResult::InvalidByteSequence) {
        binary_string(&meta.readagain_bytes)
    } else {
        Value::nil()
    };
    let _ = globals
        .store
        .set_ivar(recv, IdentId::get_id(CONVERTER_READAGAIN_IVAR), ra);
    // Structured last-error data, for `#last_error`.
    if is_error {
        let msg = conversion_error_message(result, meta, src_enc, dst_enc);
        let decode_stage = !matches!(result, StreamConvertResult::UndefinedConversion);
        let (stage_src, stage_dst) = error_stage_names(src_enc, dst_enc, decode_stage);
        let data = Value::array_from_iter(
            [
                Value::symbol_from_str(result.symbol_name()),
                Value::string(msg.clone()),
                binary_string(&meta.error_bytes),
                binary_string(&meta.readagain_bytes),
                Value::string(stage_src),
                Value::string(stage_dst),
            ]
            .into_iter(),
        );
        let _ = globals
            .store
            .set_ivar(recv, IdentId::get_id(CONVERTER_LAST_ERROR_IVAR), data);
        Some(msg)
    } else {
        let _ = globals
            .store
            .set_ivar(recv, IdentId::get_id(CONVERTER_LAST_ERROR_IVAR), Value::nil());
        None
    }
}

/// Materialise the stored last-error data into a fresh exception
/// object with the attribute ivars set.
fn build_last_error_object(globals: &mut Globals, recv: Value) -> Value {
    let Some(data) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_LAST_ERROR_IVAR))
        .filter(|v| !v.is_nil())
    else {
        return Value::nil();
    };
    let ary = data.as_array();
    let kind = ary[0];
    let msg = ary[1].is_str().map(|s| s.to_string()).unwrap_or_default();
    let class_name = if kind == Value::symbol_from_str("undefined_conversion") {
        "UndefinedConversionError"
    } else {
        "InvalidByteSequenceError"
    };
    let Some(enc_const) = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
    else {
        return Value::nil();
    };
    let Some(cls) = globals
        .store
        .get_constant_noautoload(enc_const.as_class_id(), IdentId::get_id(class_name))
    else {
        return Value::nil();
    };
    let obj = Value::new_exception_from(msg, cls.as_class_id());
    let _ = globals.store.set_ivar(obj, IdentId::get_id("@error_bytes"), ary[2]);
    // CRuby reports "no read-again bytes" as nil, not as an empty
    // String (the `primitive_errinfo` tuple still says "").
    let readagain = match ary[3].is_rstring_inner() {
        Some(s) if s.as_bytes().is_empty() => Value::nil(),
        _ => ary[3],
    };
    let _ = globals
        .store
        .set_ivar(obj, IdentId::get_id("@readagain_bytes"), readagain);
    let incomplete = kind == Value::symbol_from_str("incomplete_input");
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id("@incomplete_input"),
        Value::bool(incomplete),
    );
    let _ = globals
        .store
        .set_ivar(obj, IdentId::get_id("@source_encoding_name"), ary[4]);
    let _ = globals
        .store
        .set_ivar(obj, IdentId::get_id("@destination_encoding_name"), ary[5]);
    // `UndefinedConversionError#error_char`: the offending character,
    // whose bytes `stream_convert` stored UTF-8-encoded (the stage
    // source encoding for every path that can reach an undef error).
    if kind == Value::symbol_from_str("undefined_conversion")
        && let Some(bytes) = ary[2].is_rstring_inner().map(|s| s.as_bytes().to_vec())
        && let Ok(text) = String::from_utf8(bytes)
    {
        let _ = globals
            .store
            .set_ivar(obj, IdentId::get_id("@error_char"), Value::string(text));
    }
    obj
}

/// Raise the converter's stored last error as a real exception object,
/// so `#error_bytes`, `#readagain_bytes`, `#incomplete_input?` and the
/// encoding accessors are populated — CRuby raises the very object
/// `#last_error` hands back afterwards. Falls back to a plain
/// message-only error if the last-error slot could not be materialised.
fn converter_last_error_raise(
    globals: &mut Globals,
    recv: Value,
    result: StreamConvertResult,
    msg: String,
) -> MonorubyErr {
    let obj = build_last_error_object(globals, recv);
    if let Some(inner) = obj.is_exception() {
        return MonorubyErr::new_from_exception(inner).with_original(obj);
    }
    if matches!(result, StreamConvertResult::UndefinedConversion) {
        MonorubyErr::undefined_conversion_error(&globals.store, msg)
    } else {
        MonorubyErr::invalid_byte_sequence_error(&globals.store, msg)
    }
}

///
/// ### Encoding::Converter#primitive_convert
/// - primitive_convert(src, dst) -> Symbol
/// - primitive_convert(src, dst, dst_offset) -> Symbol
/// - primitive_convert(src, dst, dst_offset, dst_bytesize) -> Symbol
/// - primitive_convert(src, dst, dst_offset, dst_bytesize, opts) -> Symbol
///
/// Streamed sibling of `Encoding::Converter#convert`. Returns one
/// of `:finished`, `:source_buffer_empty`, `:destination_buffer_full`,
/// `:invalid_byte_sequence`, `:undefined_conversion`,
/// `:incomplete_input`. Mutates `src` (drains consumed bytes) and
/// `dst` (writes converted bytes starting at `dst_offset`, capped at
/// `dst_bytesize`). The `opts` hash supports `partial_input:` and
/// `after_output:` (the latter is accepted but ignored — monoruby's
/// stream loop runs to completion in one call rather than yielding
/// after each character).
///
#[monoruby_builtin]
fn converter_primitive_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    let mut src_arg = lfp.arg(0);
    let mut dst_arg = lfp.arg(1);
    let dst_offset_arg = lfp.try_arg(2);
    let dst_bytesize_arg = lfp.try_arg(3);
    let opts_arg = lfp.try_arg(4);

    // dst must be a writable String — frozen / chilled strings are
    // rejected before we look at the rest of the args.
    if dst_arg.is_rstring_inner().is_none() {
        return Err(MonorubyErr::typeerr(
            "no implicit conversion of nil into String".to_string(),
        ));
    }
    dst_arg.ensure_string_mutable(vm, globals)?;

    // Source is allowed to be nil (CRuby treats it as an empty
    // string, which means the call is just a "drain whatever the
    // converter has buffered" with nothing more to push). We don't
    // hold per-call decoder state across `primitive_convert`
    // invocations, so nil + empty src + last=true just yields
    // `:finished` with no output.
    // `is_rstring_inner()` (not `is_str()`) — the source may carry
    // bytes that aren't valid UTF-8 (e.g. partial EUC-JP / SJIS
    // codepoints). `is_str()` would return `None` for those and
    // the spec's `partial_input` tests would TypeError.
    let new_src_bytes: Vec<u8> = if src_arg.is_nil() {
        Vec::new()
    } else {
        src_arg
            .is_rstring_inner()
            .ok_or_else(|| MonorubyErr::typeerr("expected String".to_string()))?
            .as_bytes()
            .to_vec()
    };
    if !src_arg.is_nil() {
        src_arg.ensure_string_mutable(vm, globals)?;
    }
    // Pending bytes from the previous `primitive_convert` that the
    // dst-bytesize cap held back. Prepend them to the new src so
    // multi-call streaming with `dst_bytesize` works (the spec
    // test "uses the destination byte offset" hits this path).
    let pending: Vec<u8> = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_PENDING_IVAR))
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    let mut src_bytes = pending;
    src_bytes.extend_from_slice(&new_src_bytes);

    // Existing dst content (we'll truncate to `dst_offset` and
    // append the new bytes).
    let dst_initial: Vec<u8> = dst_arg
        .is_rstring_inner()
        .map(|s| s.as_bytes().to_vec())
        .unwrap_or_default();

    // Resolve dst_offset. nil → end-of-buffer (append). Default
    // when the arg is omitted is also "end-of-buffer" per the spec
    // (`primitive_convert(src, dst)` writes at the *end*; the
    // mocking-test "uses the destination byte offset" overrides
    // explicitly).
    let dst_offset = match dst_offset_arg {
        None => dst_initial.len(),
        Some(v) if v.is_nil() => dst_initial.len(),
        Some(v) => match v.coerce_to_integer(vm, globals)? {
            crate::value::IntegerBase::Fixnum(n) if n >= 0 => n as usize,
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "negative or too-large integer".to_string(),
                ));
            }
        },
    };
    if dst_offset > dst_initial.len() {
        return Err(MonorubyErr::argumenterr(
            "destination byte offset is greater than bytesize".to_string(),
        ));
    }

    // Resolve dst_bytesize. nil → unlimited.
    let max_dst_bytes = match dst_bytesize_arg {
        None => None,
        Some(v) if v.is_nil() => None,
        Some(v) => Some(match v.coerce_to_integer(vm, globals)? {
            crate::value::IntegerBase::Fixnum(n) if n >= 0 => n as usize,
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "negative or too-large integer".to_string(),
                ));
            }
        }),
    };

    // Resolve `partial_input:` from the options hash. Default false.
    let partial_input = match opts_arg {
        Some(v) if !v.is_nil() => v
            .try_hash_ty()
            .and_then(|h| find_hash_value_for_symbol(&h, "partial_input"))
            .map(|v| !v.is_nil() && v != Value::bool(false))
            .unwrap_or(false),
        _ => false,
    };

    let src_enc = converter_get_src(globals, recv);
    let dst_enc = converter_get_dst(globals, recv);

    let conv_opts = converter_transcode_opts(globals, recv);
    let (result, src_consumed, out_bytes, meta) = stream_convert(
        &src_bytes,
        src_enc,
        dst_enc,
        max_dst_bytes,
        partial_input,
        &conv_opts,
    );

    // After-call mutation rules (CRuby observed behaviour):
    //
    //   - `:invalid_byte_sequence` / `:undefined_conversion`:
    //     bytes after the error point stay in the user's
    //     `src_arg`. The bytes up to and including the error are
    //     consumed (and visible via `primitive_errinfo`); they're
    //     also the offset of the user's "putback" via
    //     `Encoding::Converter#putback`, but we don't yet support
    //     that. Don't promote anything to the pending buffer —
    //     only clean transient buffer-full / partial-input
    //     interruptions are buffered.
    //   - everything else (`:finished`, `:source_buffer_empty`,
    //     `:destination_buffer_full`, `:incomplete_input`):
    //     `src_arg` is cleared completely, and any tail
    //     (`src_bytes[src_consumed..]`) goes into the per-
    //     converter pending buffer for the next call to consume.
    let leave_remaining_in_src = matches!(
        result,
        StreamConvertResult::InvalidByteSequence
            | StreamConvertResult::UndefinedConversion
    );
    let pending_id = IdentId::get_id(CONVERTER_PENDING_IVAR);
    if leave_remaining_in_src {
        // Put bytes after the error back into `src_arg`. Pending
        // buffer is dropped — the converter has nothing it
        // could write next call without more user input.
        let leftover: Vec<u8> = src_bytes[src_consumed..].to_vec();
        if !src_arg.is_nil() {
            let mut new_src =
                crate::value::RStringInner::from_encoding_scanned(&leftover, src_enc);
            new_src.set_encoding(src_enc);
            src_arg.replace_with_inner(new_src);
        }
        let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
    } else {
        // Clear src and stash the unconverted tail (if any) in
        // pending for the next call.
        let pending_after: Vec<u8> = src_bytes[src_consumed..].to_vec();
        if !src_arg.is_nil() {
            let cleared = crate::value::RStringInner::from_encoding_scanned(b"", src_enc);
            src_arg.replace_with_inner(cleared);
        }
        if pending_after.is_empty() {
            let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
        } else {
            let mut pending_inner =
                crate::value::RStringInner::from_encoding_scanned(&pending_after, src_enc);
            pending_inner.set_encoding(crate::value::Encoding::Ascii8);
            let _ = globals
                .store
                .set_ivar(recv, pending_id, Value::string_from_inner(pending_inner));
        }
    }

    // Mutate dst: truncate to dst_offset, append converted bytes,
    // tag with destination encoding (CRuby always re-tags dst on
    // a primitive_convert call, success or failure).
    let mut new_dst_bytes = dst_initial[..dst_offset].to_vec();
    new_dst_bytes.extend_from_slice(&out_bytes);
    let new_dst = crate::value::RStringInner::from_encoding_scanned(&new_dst_bytes, dst_enc);
    dst_arg.replace_with_inner(new_dst);

    // Record errinfo / read-again / last-error state.
    store_conversion_outcome(globals, recv, result, &meta, src_enc, dst_enc);

    Ok(Value::symbol_from_str(result.symbol_name()))
}

///
/// ### Encoding::Converter#primitive_errinfo
/// - primitive_errinfo -> [Symbol, String, String, String, String]
///
/// CRuby returns the last `primitive_convert` status as a 5-tuple
/// `[result, src_enc, dst_enc, error_bytes, readagain_bytes]`.
/// `primitive_convert` stashes the latest result here so callers
/// can read the 5-tuple back out:
///   `[result, src_enc_name, dst_enc_name, error_bytes, readagain_bytes]`
/// We default to `[:source_buffer_empty, "", "", "", ""]` (CRuby's
/// "nothing pending" form) when no `primitive_convert` has run yet.
///
#[monoruby_builtin]
fn converter_primitive_errinfo(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    if let Some(v) = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_ERRINFO_IVAR))
    {
        return Ok(v);
    }
    // No `primitive_convert` has run yet — CRuby's "nothing
    // pending" form is `[:source_buffer_empty, nil, nil, nil, nil]`.
    Ok(Value::array_from_iter(
        [
            Value::symbol_from_str("source_buffer_empty"),
            Value::nil(),
            Value::nil(),
            Value::nil(),
            Value::nil(),
        ]
        .into_iter(),
    ))
}

///
/// ### Encoding::Converter#last_error
/// - last_error -> Exception | nil
///
/// Returns the most recent conversion error as a fresh exception
/// object (with `error_bytes` / `readagain_bytes` /
/// `incomplete_input?` readable), or `nil` when the last outcome was
/// not an error.
///
#[monoruby_builtin]
fn converter_last_error(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(build_last_error_object(globals, lfp.self_val()))
}

///
/// ### Encoding::Converter#putback
/// - putback -> String
/// - putback(max_numbytes) -> String
///
/// Returns (and drains) the read-again bytes buffered by an
/// `:invalid_byte_sequence` outcome, tagged with the converter's
/// source encoding. With an integer argument, at most that many
/// bytes are returned.
///
#[monoruby_builtin]
fn converter_putback(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    let ra_id = IdentId::get_id(CONVERTER_READAGAIN_IVAR);
    let buffered: Vec<u8> = globals
        .store
        .get_ivar(recv, ra_id)
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    let take = match lfp.try_arg(0) {
        Some(v) if !v.is_nil() => {
            (v.coerce_to_int_i64(vm, globals)?.max(0) as usize).min(buffered.len())
        }
        _ => buffered.len(),
    };
    let (out, rest) = buffered.split_at(take);
    let rest_val = if rest.is_empty() {
        Value::nil()
    } else {
        binary_string(rest)
    };
    let _ = globals.store.set_ivar(recv, ra_id, rest_val);
    let src_enc = converter_get_src(globals, recv);
    let mut s = crate::value::RStringInner::from_encoding_scanned(out, src_enc);
    s.set_encoding(src_enc);
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
    // The optional third argument / kwargs carry decorator options
    // (the kwargs hash may land in either trailing slot).
    let crlf = (2..=3)
        .filter_map(|i| lfp.try_arg(i))
        .find_map(|v| v.try_hash_ty())
        .and_then(|h| find_hash_value_for_symbol(&h, "crlf_newline"))
        .map(|v| v.as_bool())
        .unwrap_or(false);
    Ok(build_convpath(globals, src, dst, crlf))
}

/// The conversion path CRuby reports: encodings convert directly to /
/// from UTF-8; everything else pivots through it. Decorators append
/// their name as a trailing String element.
fn build_convpath(
    globals: &mut Globals,
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    crlf: bool,
) -> Value {
    use crate::value::Encoding as E;
    let mut elems: Vec<Value> = vec![];
    if src == E::Utf8 || dst == E::Utf8 || src == dst {
        elems.push(Value::array2(
            encoding_value(globals, src),
            encoding_value(globals, dst),
        ));
    } else {
        elems.push(Value::array2(
            encoding_value(globals, src),
            encoding_value(globals, E::Utf8),
        ));
        elems.push(Value::array2(
            encoding_value(globals, E::Utf8),
            encoding_value(globals, dst),
        ));
    }
    if crlf {
        elems.push(Value::string_from_str("crlf_newline"));
    }
    Value::array_from_vec(elems)
}

///
/// ### Encoding::Converter#convpath
/// - convpath -> Array
///
#[monoruby_builtin]
fn converter_convpath(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    let src = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    let crlf = globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_FLAGS_IVAR))
        .and_then(|v| v.try_fixnum())
        .map(|n| n & 0x1000 != 0)
        .unwrap_or(false);
    Ok(build_convpath(globals, src, dst, crlf))
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
    let enc_val = if val.is_nil() {
        Value::nil()
    } else {
        resolve_default_encoding_arg(vm, globals, lfp.self_val(), val)?
    };
    globals.set_gvar(IdentId::get_id("$DEFAULT_INTERNAL"), enc_val);
    refresh_inspect_escape(globals);
    Ok(enc_val)
}

/// The locale's character map — `nl_langinfo(CODESET)` under the
/// locale the environment selects, which is exactly what CRuby reports.
///
/// Derived once, because CRuby reads the codeset when it calls
/// `setlocale` at startup and never looks at `ENV` again — ruby/spec
/// checks that assigning `ENV['LC_ALL']` in a running process does
/// *not* move `Encoding.locale_charmap`. [`Globals::new`] forces that
/// one derivation at startup, before any thread or extension could be
/// running.
///
/// The system is asked rather than the variables parsed, because the
/// two disagree: a `LANG` naming a locale the machine does not have
/// generated (`en_US.UTF-8` on a container carrying only `C.UTF-8`)
/// leaves `setlocale` on `C`, and CRuby then reports `ANSI_X3.4-1968`
/// where reading the name alone says `UTF-8`. The codeset's spelling is
/// the platform's too — glibc's `ANSI_X3.4-1968` is macOS's `US-ASCII`,
/// and both name US-ASCII to [`Encoding.find`](enc_find).
pub(super) fn locale_charmap_str() -> &'static str {
    static CHARMAP: std::sync::OnceLock<String> = std::sync::OnceLock::new();
    CHARMAP.get_or_init(system_locale_charmap).as_str()
}

/// `nl_langinfo(CODESET)` for the environment's locale, without
/// touching the process's own: `newlocale` builds a throwaway
/// `LC_CTYPE` locale from the environment and `nl_langinfo_l` reads
/// the codeset out of it. CRuby installs that locale process-wide
/// (`setlocale(LC_CTYPE, "")`), which we deliberately do not — the
/// answer is the same, and the C libraries linked into the extensions
/// (libxml2, SQLite) keep the `C` locale they have always had.
///
/// A locale the system cannot provide makes `newlocale` fail, which is
/// the `setlocale` failure CRuby falls back to `C` from, so we ask for
/// `C` in turn.
#[cfg(unix)]
fn system_locale_charmap() -> String {
    // `nl_langinfo_l` is POSIX.1-2008 and present on both platforms,
    // but the `libc` crate declares it only for Linux.
    unsafe extern "C" {
        fn nl_langinfo_l(item: libc::nl_item, locale: libc::locale_t) -> *mut std::ffi::c_char;
    }

    fn codeset_of(name: &std::ffi::CStr) -> Option<String> {
        // SAFETY: `newlocale` allocates a locale object we own and
        // never install, so nothing else in the process can observe or
        // free it; the `nl_langinfo_l` result points into that object
        // and is copied before `freelocale` invalidates it.
        unsafe {
            let loc = libc::newlocale(libc::LC_CTYPE_MASK, name.as_ptr(), std::ptr::null_mut());
            if loc.is_null() {
                return None;
            }
            let codeset = nl_langinfo_l(libc::CODESET, loc);
            let s = if codeset.is_null() {
                None
            } else {
                Some(
                    std::ffi::CStr::from_ptr(codeset)
                        .to_string_lossy()
                        .into_owned(),
                )
            };
            libc::freelocale(loc);
            s.filter(|s| !s.is_empty())
        }
    }

    codeset_of(c"")
        .or_else(|| codeset_of(c"C"))
        .unwrap_or_else(|| "ANSI_X3.4-1968".to_string())
}

/// Without `nl_langinfo`, read the codeset out of the variables
/// glibc would have consulted: `LC_ALL` > `LC_CTYPE` > `LANG`, an empty
/// variable counting as unset, the codeset being the part after `.`
/// with any `@modifier` stripped, and no locale at all (or `C` /
/// `POSIX`) meaning `ANSI_X3.4-1968`.
#[cfg(not(unix))]
fn system_locale_charmap() -> String {
    let locale = ["LC_ALL", "LC_CTYPE", "LANG"]
        .iter()
        .find_map(|key| std::env::var(key).ok().filter(|v| !v.is_empty()))
        .unwrap_or_default();
    let locale = locale.split('@').next().unwrap_or("");
    match locale.split_once('.') {
        Some((_, codeset)) => {
            let normalized = codeset.replace(['-', '_'], "").to_ascii_lowercase();
            if normalized == "utf8" {
                "UTF-8".to_string()
            } else if normalized == "ascii" || normalized == "usascii" {
                "ANSI_X3.4-1968".to_string()
            } else {
                codeset.to_string()
            }
        }
        None if locale.is_empty() || locale == "C" || locale == "POSIX" => {
            "ANSI_X3.4-1968".to_string()
        }
        None => "UTF-8".to_string(),
    }
}

/// The canonical name of the encoding the locale charmap selects, or
/// `None` when it names none monoruby knows. This is the source
/// encoding CRuby gives a `-e` script: a `C` locale makes
/// `ruby -e 'p "\u3044"'` an `invalid multibyte character`, where a
/// UTF-8 one runs it. (`-E` does not move it — that sets the external
/// encoding, not the source's — but `-K` does, and a magic comment in
/// the script wins over both.)
pub fn locale_source_encoding_name() -> Option<&'static str> {
    crate::value::Encoding::try_from_str(locale_charmap_str())
        .ok()
        .map(|enc| enc.name())
}

/// The encoding `Encoding.find("locale")` answers with: the locale
/// charmap's encoding when we recognise it, else UTF-8.
fn locale_encoding_value(globals: &Globals) -> Value {
    find_encoding_object(globals, locale_charmap_str()).unwrap_or_else(|| {
        let enc_class = encoding_class(globals);
        globals
            .store
            .get_constant_noautoload(enc_class, IdentId::UTF_8)
            .unwrap_or(Value::nil())
    })
}

/// Seed `Encoding.default_external` with the locale's encoding, as
/// CRuby does at startup: `rb_enc_set_default_external` is handed
/// `rb_locale_encoding()`, so a `C` / `POSIX` locale (or none) starts
/// the process on US-ASCII rather than UTF-8. `-E` / `-K` overwrite it
/// from the CLI prelude, and `Encoding.default_external=` from Ruby;
/// nothing can unset it again (`= nil` is an `ArgumentError`), so every
/// later read finds an encoding here and the readers' own UTF-8
/// fallbacks only cover a `Globals` this never ran on.
pub(crate) fn init_default_external(globals: &mut Globals) {
    let v = locale_encoding_value(globals);
    globals.set_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"), v);
    // `Encoding.find("filesystem")` needs nothing of its own: it is an
    // alias of `default_external`, which the reference CRuby confirms
    // on both CI platforms — under no locale it answers US-ASCII on
    // macOS just as it does on Linux, so the `__APPLE__` arm of
    // `rb_filesystem_encindex` is not what this Ruby ends up on.
    refresh_inspect_escape(globals);
}

/// Recompute [`Store::inspect_escape`] from the two gvars that decide
/// it. Called from every place either can move: startup, `-E` / `-K`
/// (through `Encoding.default_external=`), `default_internal=`, and the
/// test pin.
fn refresh_inspect_escape(globals: &mut Globals) {
    let enc = inspect_escape_encoding(globals);
    globals.store.set_inspect_escape(enc);
}

/// Pin `Encoding.default_external` to UTF-8 whatever the locale says.
/// Used by [`Globals::new_test`], whose differential partner is spawned
/// with `-E UTF-8` for the same reason: `cargo nextest` runs with no
/// `LANG`, and a locale-derived US-ASCII would have both sides escaping
/// every non-ASCII `#inspect` — reproducible, but not what the tests
/// are about, and it would re-record the whole snapshot oracle. The
/// locale path is covered by `tests/encoding_locale.rs`, which spawns
/// the binary with the environment it wants.
pub(crate) fn set_default_external_utf8(globals: &mut Globals) {
    let enc_class = encoding_class(globals);
    if let Some(utf8) = globals
        .store
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
    {
        globals.set_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"), utf8);
    }
    refresh_inspect_escape(globals);
}

///
/// ### Encoding.locale_charmap
/// - locale_charmap -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/locale_charmap.html]
#[monoruby_builtin]
fn enc_locale_charmap(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::string_from_str(locale_charmap_str()))
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
    if let Some(v) = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@source_encoding_name"))
        .filter(|v| !v.is_nil())
    {
        return Ok(v);
    }
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
    if let Some(v) = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@destination_encoding_name"))
        .filter(|v| !v.is_nil())
    {
        return Ok(v);
    }
    if let Some(msg) = enc_err_message(globals, lfp.self_val())
        && let Some((_, dst)) = parse_enc_err_pair(&msg)
    {
        return Ok(Value::string(dst));
    }
    Ok(Value::string_from_str(""))
}

/// The `Encoding` object named by one of the `@*_encoding_name` ivars
/// a converter-raised error carries, if it is set and resolvable.
fn enc_err_named_encoding(globals: &Globals, exc: Value, ivar: &str) -> Option<Value> {
    let name = globals
        .store
        .get_ivar(exc, IdentId::get_id(ivar))
        .and_then(|v| v.is_str().map(|s| s.to_string()))?;
    find_encoding_object(globals, &name)
}

#[monoruby_builtin]
fn enc_err_source_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(v) = enc_err_named_encoding(globals, lfp.self_val(), "@source_encoding_name") {
        return Ok(v);
    }
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
    if let Some(v) = enc_err_named_encoding(globals, lfp.self_val(), "@destination_encoding_name") {
        return Ok(v);
    }
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
    if let Some(v) = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@error_char"))
        .filter(|v| !v.is_nil())
    {
        return Ok(v);
    }
    let Some(msg) = enc_err_message(globals, lfp.self_val()) else {
        return Ok(Value::string_from_str(""));
    };
    if let Some(rest) = msg.strip_prefix("U+") {
        let hex_end = rest
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap_or(rest.len());
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
/// whether the error was caused by input ending mid-sequence
/// (a partial leading prefix of a valid sequence) rather than an
/// outright invalid byte.
#[monoruby_builtin]
fn enc_err_incomplete_input_p(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // Converter-produced errors carry the flag as an ivar. Errors
    // raised from `String#encode` &c. are plain messages; classify
    // from the transcoder-generated message shape (CRuby answers
    // false there — string input is always "complete"). A freshly
    // `.new`ed error, whose message matches neither shape, answers
    // nil like CRuby.
    if let Some(v) = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@incomplete_input"))
    {
        return Ok(v);
    }
    if let Some(msg) = enc_err_message(globals, lfp.self_val()) {
        if msg.starts_with("incomplete \"") {
            return Ok(Value::bool(true));
        }
        if msg.contains("invalid byte sequence") || msg.contains("\" followed by \"") {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::nil())
}

///
/// ### Encoding::InvalidByteSequenceError#error_bytes
/// - error_bytes -> String | nil
///
#[monoruby_builtin]
fn enc_err_error_bytes(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@error_bytes"))
        .unwrap_or(Value::nil()))
}

///
/// ### Encoding::InvalidByteSequenceError#readagain_bytes
/// - readagain_bytes -> String | nil
///
#[monoruby_builtin]
fn enc_err_readagain_bytes(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@readagain_bytes"))
        .unwrap_or(Value::nil()))
}

///
/// ### Encoding.list
/// - list -> [Encoding]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Encoding/s/list.html]
#[monoruby_builtin]
fn enc_list(_vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let enc_class = encoding_class(globals);
    // Every `Encoding::*` constant whose value is an `Encoding`
    // instance, listed once per distinct encoding object (aliases such
    // as `BINARY`/`ASCII-8BIT` share an object, so they collapse to a
    // single entry). The error subclasses (`CompatibilityError`, …) are
    // `Class` values and are filtered out by the class check.
    let names = globals.store.get_constant_names(enc_class);
    let mut seen: Vec<u64> = Vec::new();
    let mut out: Vec<(String, Value)> = Vec::new();
    for name in names {
        if let Some(v) = globals.store.get_constant_noautoload(enc_class, name) {
            if v.class() == enc_class {
                let id = v.id();
                if !seen.contains(&id) {
                    seen.push(id);
                    let ename = globals
                        .store
                        .get_ivar(v, IdentId::_ENCODING)
                        .and_then(|s| s.is_str().map(|s| s.to_string()))
                        .unwrap_or_default();
                    out.push((ename, v));
                }
            }
        }
    }
    // `constants` is backed by a `HashMap`; sort by canonical name for
    // a stable, reproducible order.
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(Value::array_from_vec(out.into_iter().map(|(_, v)| v).collect()))
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
    // Special names resolved at query time: the filesystem/locale
    // encodings follow `default_external`, and "internal" may be nil.
    // CRuby resolves these the same way whatever the case, since the
    // whole name lookup is case-insensitive.
    match name.to_ascii_lowercase().as_str() {
        // The locale encoding follows the locale charmap, which CRuby
        // reads from the environment at startup; the other two follow
        // `default_external`.
        "locale" => return Ok(locale_encoding_value(globals)),
        "external" | "filesystem" => {
            let ext = globals
                .get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
                .filter(|v| !v.is_nil())
                .unwrap_or_else(|| {
                    globals
                        .store
                        .get_constant_noautoload(enc_class, IdentId::UTF_8)
                        .unwrap_or(Value::nil())
                });
            return Ok(ext);
        }
        "internal" => {
            let int = globals
                .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
                .unwrap_or(Value::nil());
            return Ok(int);
        }
        _ => {}
    }
    // `rb_to_encoding` goes through `StringValueCStr`, so an embedded
    // NUL is its own error rather than an unknown name.
    if name.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr(
            "invalid encoding name (NUL byte)",
        ));
    }
    match find_encoding_object(globals, &name) {
        Some(v) => Ok(v),
        None => Err(MonorubyErr::argumenterr(format!(
            "unknown encoding name - {}",
            name
        ))),
    }
}

/// Resolve an encoding *name* to its registered `Encoding` object,
/// preserving object identity (so e.g. `IBM866` stays `IBM866` rather
/// than collapsing to `ASCII-8BIT` the way the `Encoding` enum does).
/// Mirrors `Encoding.find` without the `to_str`/error handling.
pub(super) fn find_encoding_object(globals: &Globals, name: &str) -> Option<Value> {
    let enc_class = encoding_class(globals);
    // The alias table names a constant directly, which answers the
    // common names ("UTF-8", "ASCII-8BIT", …) without touching the rest
    // of the table. It is only trusted here when the constant it names
    // really does carry this canonical name — the table is
    // hand-maintained and would otherwise mis-resolve a name like
    // "Big5-HKSCS" to a prefix match — so anything it gets wrong falls
    // through to the scan below, which is what decides.
    if let Some(v) = enc_name_to_const(name)
        .and_then(|c| globals.store.get_constant_noautoload(enc_class, IdentId::get_id(c)))
        && v.class() == enc_class
        && encoding_object_name_is(globals, v, name)
    {
        return Some(v);
    }
    // An exact (separator/case-insensitive) match against the canonical
    // name of every registered encoding, so `Encoding.find(e.name)`
    // round-trips for *every* encoding in `Encoding.list`.
    for cname in globals.store.get_constant_names(enc_class) {
        if let Some(v) = globals.store.get_constant_noautoload(enc_class, cname)
            && v.class() == enc_class
            && encoding_object_name_is(globals, v, name)
        {
            return Some(v);
        }
    }
    // Last, the alias / pseudo-name table without the canonical-name
    // check — this is what resolves the names no `Encoding` carries
    // (LOCALE, UTF8, …).
    enc_name_to_const(name)
        .and_then(|c| globals.store.get_constant_noautoload(enc_class, IdentId::get_id(c)))
}

/// `v`'s canonical name (its `_ENCODING` ivar) is `name`, compared the
/// way `Encoding.find` compares names.
fn encoding_object_name_is(globals: &Globals, v: Value, name: &str) -> bool {
    match globals.store.get_ivar(v, IdentId::_ENCODING) {
        Some(ev) => match ev.is_str() {
            Some(es) => enc_name_eq(es, name),
            None => false,
        },
        None => false,
    }
}

/// Two encoding names are the same name when they agree ignoring case
/// and the `-` / `_` separators ("utf8" == "UTF-8").
///
/// Encoding names are ASCII, so this compares ASCII case and allocates
/// nothing — where the previous `to_uppercase().replace(…)` built two
/// `String`s for *every candidate* of *every* lookup (`Encoding.find`
/// scans ~107 encodings, and the mail benchmark calls it 80 times per
/// message: 17,000 temporary strings a message).
fn enc_name_eq(a: &str, b: &str) -> bool {
    let sep = |c: &u8| *c != b'-' && *c != b'_';
    let mut a = a.bytes().filter(sep);
    let mut b = b.bytes().filter(sep);
    loop {
        match (a.next(), b.next()) {
            (None, None) => return true,
            (Some(x), Some(y)) if x.eq_ignore_ascii_case(&y) => {}
            _ => return false,
        }
    }
}

/// The canonical name string carried by an `Encoding` object's
/// `_ENCODING` ivar (e.g. `"IBM866"`, `"UTF-8"`).
pub(super) fn encoding_object_name(globals: &Globals, v: Value) -> Option<String> {
    globals
        .store
        .get_ivar(v, IdentId::_ENCODING)
        .and_then(|ev| ev.is_str().map(|s| s.to_string()))
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
        "UTF_7" | "CP65000" => Some("UTF_7"),

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
        "UTF_16BE" | "UCS_2BE" => Some("UTF_16BE"),
        "UTF_16LE" => Some("UTF_16LE"),

        // UTF-32
        "UTF_32" => Some("UTF_32"),
        "UTF_32BE" | "UCS_4BE" => Some("UTF_32BE"),
        "UTF_32LE" | "UCS_4LE" => Some("UTF_32LE"),

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
        "SHIFT_JIS" => Some("Shift_JIS"),
        "ISO_2022_JP" | "ISO2022_JP" => Some("ISO_2022_JP"),
        "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "WINDOWS31J" | "PCK" | "SJIS" => {
            Some("Windows_31J")
        }
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
        "KOI8_R" | "CP878" => Some("KOI8_R"),
        "KOI8_U" => Some("KOI8_U"),

        // Chinese encodings
        "GB2312" | "EUC_CN" | "EUCCN" => Some("GB2312"),
        "GBK" | "CP936" => Some("GBK"),
        "GB18030" => Some("GB18030"),
        "GB12345" => Some("GB12345"),
        "BIG5" => Some("Big5"),
        "BIG5_HKSCS" => Some("Big5_HKSCS"),
        "BIG5_UAO" => Some("Big5_UAO"),

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
        "MACCENTEURO" => Some("MacCentEuro"),
        "MACCROATIAN" => Some("MacCroatian"),
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
    // Derived from `ENCODING_NAMES`, the single source of truth shared with
    // `Encoding.name_list` and `Encoding#names`, so the three stay mutually
    // consistent (ruby/spec checks that every alias key appears in
    // `name_list`, and that `#names` lists every alias pointing at it).
    for (canonical, aliases) in ENCODING_NAMES {
        for alias in *aliases {
            map.insert(
                Value::string_from_str(alias),
                Value::string_from_str(canonical),
                vm,
                globals,
            )?;
        }
    }
    for (alias, canonical) in dynamic_encoding_aliases(globals) {
        map.insert(
            Value::string_from_str(alias),
            Value::string_from_str(&canonical),
            vm,
            globals,
        )?;
    }
    Ok(Value::hash(map))
}

/// The three alias names whose target is decided at run time:
/// `"locale"` follows the locale charmap, `"external"` and
/// `"filesystem"` follow `Encoding.default_external`. Returns
/// `(alias, canonical name)` pairs so `Encoding.aliases`,
/// `Encoding#names` and `Encoding.find` all agree.
const DYNAMIC_ALIASES: &[&str] = &["locale", "external", "filesystem"];

fn dynamic_encoding_aliases(globals: &mut Globals) -> Vec<(&'static str, String)> {
    fn canonical_of(globals: &Globals, v: Value) -> Option<String> {
        globals
            .store
            .get_ivar(v, IdentId::_ENCODING)
            .and_then(|ev| ev.is_str().map(|s| s.to_string()))
    }
    let external_val = globals
        .get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
        .filter(|v| !v.is_nil());
    let locale_val = locale_encoding_value(globals);
    let locale = canonical_of(globals, locale_val);
    let external = external_val
        .and_then(|v| canonical_of(globals, v))
        .unwrap_or_else(|| "UTF-8".to_string());
    DYNAMIC_ALIASES
        .iter()
        .map(|alias| {
            let target = match *alias {
                "locale" => locale.clone().unwrap_or_else(|| external.clone()),
                _ => external.clone(),
            };
            (*alias, target)
        })
        .collect()
}

/// The static set of canonical encoding names plus their aliases.
/// Used by `Encoding.name_list` and `Encoding#names`. Each tuple is
/// `(canonical, &[aliases])`.
const ENCODING_NAMES: &[(&str, &[&str])] = &[
    ("ASCII-8BIT", &["BINARY"]),
    ("UTF-8", &["CP65001"]),
    ("US-ASCII", &["ASCII", "ANSI_X3.4-1968", "646"]),
    ("UTF-16BE", &["UCS-2BE"]),
    ("UTF-16LE", &[]),
    ("UTF-16", &[]),
    ("UTF-32BE", &["UCS-4BE"]),
    ("UTF-32LE", &["UCS-4LE"]),
    ("UTF-32", &[]),
    ("ISO-8859-1", &["ISO8859-1"]),
    ("ISO-8859-2", &["ISO8859-2"]),
    ("ISO-8859-3", &["ISO8859-3"]),
    ("ISO-8859-4", &["ISO8859-4"]),
    ("ISO-8859-5", &["ISO8859-5"]),
    ("ISO-8859-6", &["ISO8859-6"]),
    ("ISO-8859-7", &["ISO8859-7"]),
    ("ISO-8859-8", &["ISO8859-8"]),
    ("ISO-8859-9", &["ISO8859-9"]),
    ("ISO-8859-10", &["ISO8859-10"]),
    ("ISO-8859-11", &["ISO8859-11"]),
    ("ISO-8859-13", &["ISO8859-13"]),
    ("ISO-8859-14", &["ISO8859-14"]),
    ("ISO-8859-15", &["ISO8859-15"]),
    ("ISO-8859-16", &["ISO8859-16"]),
    ("Shift_JIS", &[]),
    ("Windows-31J", &["CP932", "csWindows31J", "SJIS", "PCK"]),
    ("EUC-JP", &["eucJP"]),
    ("ISO-2022-JP", &["ISO2022-JP"]),
    ("Windows-1250", &["CP1250"]),
    ("Windows-1251", &["CP1251"]),
    ("Windows-1252", &["CP1252"]),
    ("Windows-1253", &["CP1253"]),
    ("Windows-1254", &["CP1254"]),
    ("Windows-1255", &["CP1255"]),
    ("Windows-1256", &["CP1256"]),
    ("Windows-1257", &["CP1257"]),
    ("Windows-1258", &["CP1258"]),
    ("KOI8-R", &["CP878"]),
    ("KOI8-U", &[]),
    ("GB2312", &["EUC-CN", "eucCN"]),
    ("GBK", &["CP936"]),
    ("GB18030", &[]),
    ("Big5", &[]),
    ("EUC-KR", &["eucKR"]),
    ("EUC-TW", &["eucTW"]),
    ("CP949", &[]),
    ("TIS-620", &[]),
    ("MacJapanese", &["MacJapan"]),
    ("macRoman", &[]),
    ("macCyrillic", &[]),
    ("macCentEuro", &[]),
    ("macCroatian", &[]),
    ("macGreek", &[]),
    ("macIceland", &[]),
    ("macRomania", &[]),
    ("macThai", &[]),
    ("macTurkish", &[]),
    ("macUkraine", &[]),
    ("eucJP-ms", &["eucjp-ms", "euc-jp-ms"]),
    ("CP51932", &[]),
    ("stateless-ISO-2022-JP", &[]),
    ("CESU-8", &[]),
    ("UTF-7", &["CP65000"]),
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
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut seen: Vec<String> = Vec::new();
    let mut names: Vec<Value> = Vec::new();
    let add = |names: &mut Vec<Value>, seen: &mut Vec<String>, s: &str| {
        if !seen.iter().any(|x| x == s) {
            seen.push(s.to_string());
            names.push(Value::string_from_str(s));
        }
    };
    for (canonical, aliases) in ENCODING_NAMES {
        add(&mut names, &mut seen, canonical);
        for alias in *aliases {
            add(&mut names, &mut seen, alias);
        }
    }
    for alias in DYNAMIC_ALIASES {
        add(&mut names, &mut seen, alias);
    }
    // Also include the canonical name of every encoding exposed by
    // `Encoding.list` so `name_list` is a superset of it (spec:
    // "name_list includes all non-dummy encodings").
    let enc_class = encoding_class(globals);
    for cname in globals.store.get_constant_names(enc_class) {
        if let Some(v) = globals.store.get_constant_noautoload(enc_class, cname)
            && v.class() == enc_class
            && let Some(es) = globals
                .store
                .get_ivar(v, IdentId::_ENCODING)
                .and_then(|ev| ev.is_str().map(|s| s.to_string()))
        {
            add(&mut names, &mut seen, &es);
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
    let mut names: Vec<Value> = vec![Value::string_usascii_from_str(&canonical)];
    for (c, aliases) in ENCODING_NAMES {
        if c.eq_ignore_ascii_case(&canonical) {
            for alias in *aliases {
                names.push(Value::string_usascii_from_str(alias));
            }
            break;
        }
    }
    for (alias, target) in dynamic_encoding_aliases(globals) {
        if target.eq_ignore_ascii_case(&canonical) {
            names.push(Value::string_from_str(alias));
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
    let a = lfp.arg(0);
    let b = lfp.arg(1);
    let result = compute_encoding_compatibility(globals, a, b);
    match result {
        Some(enc) => Ok(encoding_value_for(globals, enc)),
        None => Ok(Value::nil()),
    }
}

/// Resolve `Encoding.compatible?(a, b)` to either an `Encoding` or
/// `None` (meaning the spec returns `nil`). Mirrors CRuby's
/// `rb_enc_compatible` (string/symbol/regexp) plus the
/// `rb_enc_check`-style Encoding/Encoding pair handling — this is
/// the truth table that `core/encoding/compatible_spec.rb` exercises.
fn compute_encoding_compatibility(
    globals: &Globals,
    a: Value,
    b: Value,
) -> Option<Encoding> {
    let enc_class = encoding_class(globals);
    // `Encoding` × `Encoding` follows a different rule set: only the
    // "second is US-ASCII" exception, no ASCII-only-content
    // accommodation (since Encoding objects carry no bytes). Dummy
    // encodings are rejected outright (except when both sides are
    // the same dummy — `compatible?(UTF_7, UTF_7) == UTF_7`).
    if a.class() == enc_class && b.class() == enc_class {
        let a_enc = pure_encoding_value(globals, a)?;
        let b_enc = pure_encoding_value(globals, b)?;
        return compatible_encoding_pair(a_enc, b_enc);
    }
    // String / Symbol / Regexp / (bare) Encoding: CRuby's
    // `rb_enc_compatible`. Anything else (Object, nil, …) → nil.
    let lhs = encoded_operand(globals, a)?;
    let rhs = encoded_operand(globals, b)?;
    rb_enc_compatible(&lhs, &rhs)
}

/// An encoding-bearing operand for `Encoding.compatible?`. `is_string`
/// distinguishes a real `String` (whose emptiness / coderange feed the
/// extra `rb_enc_compatible` rules) from `Symbol`/`Regexp`/bare
/// `Encoding`, which CRuby treats as "objects whose encoding is the
/// same as the contents" (the non-String branches).
struct EncodedOperand {
    bytes: Vec<u8>,
    encoding: Encoding,
    is_string: bool,
}

impl EncodedOperand {
    /// CRuby `rb_enc_str_asciionly_p`: ASCII-compatible encoding AND
    /// 7-bit content. Non-ASCII-compatible encodings (UTF-16/32, …)
    /// are never "ASCII only" even for `"abc"`.
    fn ascii_only(&self) -> bool {
        self.encoding.is_ascii_compatible()
            && matches!(
                RStringInner::from_encoding_scanned(&self.bytes, self.encoding).code_range(),
                crate::value::CodeRange::SevenBit
            )
    }

    fn code_range(&self) -> crate::value::CodeRange {
        RStringInner::from_encoding_scanned(&self.bytes, self.encoding).code_range()
    }
}

/// CRuby's `rb_enc_compatible(str1, str2)` (encoding.c). Returns the
/// negotiated encoding, or `None` for incompatible (the caller maps
/// that to `nil`). This is the exact algorithm `compatible_spec.rb`
/// verifies — keep it byte-for-byte faithful.
fn rb_enc_compatible(a: &EncodedOperand, b: &EncodedOperand) -> Option<Encoding> {
    use crate::value::CodeRange;
    let enc1 = a.encoding;
    let enc2 = b.encoding;
    if enc1 == enc2 {
        return Some(enc1);
    }
    // An empty *String* second operand yields to the first
    // unconditionally (even when enc1 is dummy / non-ASCII-compat).
    if b.is_string && b.bytes.is_empty() {
        return Some(enc1);
    }
    // An empty *String* first operand: the second wins, unless the
    // first's encoding is ASCII-compatible and the second is ASCII
    // only (then the first's encoding is kept).
    if a.is_string && a.bytes.is_empty() {
        return Some(if enc1.is_ascii_compatible() && b.ascii_only() {
            enc1
        } else {
            enc2
        });
    }
    if !enc1.is_ascii_compatible() || !enc2.is_ascii_compatible() {
        return None;
    }
    // "objects whose encoding is the same as the contents": a
    // non-String US-ASCII operand defers to the other side.
    if !b.is_string && enc2 == Encoding::UsAscii {
        return Some(enc1);
    }
    if !a.is_string && enc1 == Encoding::UsAscii {
        return Some(enc2);
    }
    // Orient so the (only / first) String is the left operand; the
    // coderange negotiation below is written from that viewpoint.
    // `enc1` / `enc2` deliberately stay bound to the *original*
    // operands: CRuby's swap in `enc_compatible_latter` permutes the
    // encoding indices twice, so they come out unchanged while the
    // string values are exchanged. `Encoding.compatible?(regexp, str)`
    // relies on that asymmetry (it answers with the String's encoding,
    // while the reversed call answers with the Regexp's).
    let (s1, s2) = if !a.is_string { (b, a) } else { (a, b) };
    let cr1 = s1.code_range();
    if s2.is_string {
        let cr2 = s2.code_range();
        if cr1 != cr2 {
            if cr1 == CodeRange::SevenBit {
                return Some(enc2);
            }
            if cr2 == CodeRange::SevenBit {
                return Some(enc1);
            }
        }
        if cr2 == CodeRange::SevenBit {
            return Some(enc1);
        }
    }
    if cr1 == CodeRange::SevenBit {
        return Some(enc2);
    }
    None
}

fn encoded_operand(globals: &Globals, v: Value) -> Option<EncodedOperand> {
    if let Some(s) = v.is_rstring_inner() {
        return Some(EncodedOperand {
            bytes: s.as_bytes().to_vec(),
            encoding: s.encoding(),
            is_string: true,
        });
    }
    if let Some(sym) = v.try_symbol() {
        let ident = sym.get_ident_name_clone();
        let default_enc = match &ident {
            crate::id_table::IdentName::Utf8(s) => {
                if s.is_ascii() {
                    Encoding::UsAscii
                } else {
                    Encoding::Utf8
                }
            }
            crate::id_table::IdentName::Bytes(_) => Encoding::Ascii8,
        };
        return Some(EncodedOperand {
            bytes: ident.as_bytes().to_vec(),
            // Preserve the symbol's recorded source encoding (set by
            // the per-(bytes, encoding) interner). Without this, a
            // `force_encoding("euc-jp").to_sym` symbol would report
            // ASCII-8BIT and mis-negotiate against ASCII-only sides.
            encoding: sym.symbol_encoding().unwrap_or(default_enc),
            is_string: false,
        });
    }
    if let Some(re) = v.is_regex() {
        // NOTE: monoruby stores the regexp source as a Rust `String`,
        // so a non-UTF-8 source (`Regexp.new("\xa4\xa2".b)`) is lossy
        // before it reaches here — those `compatible_spec.rb` cases
        // stay failing pending lossless regexp-source storage. The
        // ASCII / UTF-8 cases are exact.
        let src = re.as_str().as_bytes().to_vec();
        return Some(EncodedOperand {
            bytes: src,
            encoding: re.declared_encoding(),
            is_string: false,
        });
    }
    if v.class() == encoding_class(globals) {
        // Bare `Encoding` object — a non-String operand whose
        // "contents" are empty in that encoding.
        if let Some(enc) = pure_encoding_value(globals, v) {
            return Some(EncodedOperand {
                bytes: Vec::new(),
                encoding: enc,
                is_string: false,
            });
        }
    }
    None
}

fn pure_encoding_value(globals: &Globals, v: Value) -> Option<Encoding> {
    let name = globals.store.get_ivar(v, IdentId::_ENCODING)?;
    let s = name.is_str()?;
    Encoding::try_from_str(s).ok()
}

/// CRuby's `Encoding × Encoding` compatibility rule:
///   - identical encoding → that encoding (even if dummy);
///   - either side dummy → nil;
///   - second is US-ASCII (and first isn't) → first;
///   - otherwise (two different non-US-ASCII encodings) → nil.
///
/// Used both by the bare-`Encoding` case in
/// `Encoding.compatible?(enc1, enc2)` and by various
/// `Encoding::CompatibilityError`-raising sites that work with
/// abstract encoding pairs.
pub(crate) fn compatible_encoding_pair(a: Encoding, b: Encoding) -> Option<Encoding> {
    if a == b {
        return Some(a);
    }
    if is_cruby_dummy(a) || is_cruby_dummy(b) {
        return None;
    }
    if b == Encoding::UsAscii {
        return Some(a);
    }
    None
}

/// True for the encodings CRuby flags as "dummy" (no decoder
/// available). Narrower than `Encoding::is_dummy` — the latter
/// covers any encoding monoruby doesn't decode natively, which is
/// too eager (ISO-8859 / EUC-JP / SJIS have decoders even if
/// monoruby doesn't use them in compat checks).
pub(crate) fn is_cruby_dummy(enc: Encoding) -> bool {
    matches!(enc, Encoding::Iso2022Jp)
        || is_cruby_dummy_name(enc.name())
}

/// Materialise an `Encoding` Value (the `Encoding::NAME` constant)
/// from an internal `Encoding`. Falls back to `nil` if the constant
/// isn't registered (shouldn't happen in practice — every encoding
/// monoruby tracks has a corresponding `Encoding::*` constant).
fn encoding_value_for(globals: &Globals, enc: Encoding) -> Value {
    let const_name = encoding_constant_name(enc);
    let enc_class = encoding_class(globals);
    globals
        .store
        .get_constant_noautoload(enc_class, IdentId::get_id(const_name))
        .unwrap_or(Value::nil())
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
        None => Ok(Value::string_usascii_from_str("UTF-8")),
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
        return Ok(Value::string_usascii_from_str("#<Encoding:BINARY (ASCII-8BIT)>"));
    }
    let suffix = if is_cruby_dummy_name(&name) {
        " (dummy)"
    } else {
        ""
    };
    Ok(Value::string_usascii(format!("#<Encoding:{name}{suffix}>")))
}

/// Encoding names that CRuby flags as "dummy" — registered but not
/// natively decoded. monoruby's broader `Encoding::is_dummy` covers
/// "we don't decode", which is too eager (ISO-8859 / EUC-JP / SJIS
/// have CRuby decoders even if monoruby doesn't). For
/// `Encoding#dummy?` and `Encoding#inspect` we use this narrower
/// match to match CRuby observed behaviour.
fn is_cruby_dummy_name(name: &str) -> bool {
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
    #[test]
    fn encode_fallback_option() {
        // fallback: consulted per undefined character — Hash (with
        // default / default_proc), proc, #[] object; #to_str on the
        // result; nil / missing key re-raises; unencodable replacement
        // is "too big fallback string".
        crate::tests::run_test_once(
            r##"(r=[]; r << "B�".encode(Encoding::US_ASCII, fallback: { "�" => "bar" }); r << "B�".encode(Encoding::US_ASCII, fallback: proc { |c| c.bytes.inspect }); h={}; h.default="dbar"; r << "B�".encode(Encoding::US_ASCII, fallback: h); o=Object.new; def o.to_str; "tbar"; end; r << "B�".encode(Encoding::US_ASCII, fallback: { "�" => o }); r << (begin; "B�".encode(Encoding::US_ASCII, fallback: { "x" => "y" }); rescue => e; e.class; end); r << (begin; "B�".encode(Encoding::US_ASCII, fallback: { "�" => "￮" }); rescue => e; [e.class, e.message]; end); r << (begin; "B�".encode(Encoding::US_ASCII, fallback: { "�" => Object.new }); rescue => e; e.class; end); r << (begin; "B�".encode(Encoding::US_ASCII, fallback: Object.new); rescue => e; e.class; end); r)"##,
        );
    }

    #[test]
        fn encode_binary_source_undefined_conversion() {
        // BINARY with an 8-bit byte -> real codec: CRuby's
        // UndefinedConversionError, direct and pivot message forms.
        run_test_once(
            r#"(a = (begin; "\xC3\xA9".b.encode("UTF-8"); rescue => e; [e.class.to_s, e.message]; end); b = (begin; "\xC3\xA9".b.encode("UTF-16BE"); rescue => e; [e.class.to_s, e.message]; end); [a, b])"#,
        );
    }

    #[test]
    fn converter_streaming_state() {
        // primitive_convert error reporting: errinfo tuples, read-again
        // buffering (putback), last_error objects with byte attributes.
        crate::tests::run_test_once(
            r##"(r=[]; ec=Encoding::Converter.new("utf-8","iso-8859-1"); r << ec.primitive_errinfo << ec.last_error; r << ec.primitive_convert(+"\xf1abcd", +""); r << ec.primitive_errinfo; e=ec.last_error; r << e.class << [e.error_bytes, e.readagain_bytes, e.incomplete_input?]; r << ec.primitive_convert("ok".dup.force_encoding("utf-8"), +"") << ec.primitive_errinfo << ec.last_error; ec2=Encoding::Converter.new("EUC-JP","ISO-8859-1"); s=+"abc\xa1def"; d=+""; r << ec2.primitive_convert(s,d,nil,10) << [s,d] << ec2.putback << ec2.putback; s=ec2.putback+s; r << ec2.primitive_convert(s,d,nil,10) << [s,d]; ec3=Encoding::Converter.new("utf-16le","iso-8859-1"); s3=+"\x00\xd8\x61\x00"; r << ec3.primitive_convert(s3,+"") << ec3.primitive_errinfo.map{|x| x.is_a?(String) ? x.bytes : x} << ec3.putback(2).bytes; ec4=Encoding::Converter.new("EUC-JP","ISO-8859-1"); r << ec4.primitive_convert(+"\xa4",+"",nil,10) << ec4.primitive_errinfo << ec4.last_error.class << ec4.last_error.incomplete_input?; r)"##,
        );
    }

    #[test]
    fn encode_to_binary_undefined_conversion() {
        // ASCII-8BIT is a byte bucket, not a character encoding: CRuby
        // has no conversion to it from any character above U+007F, so
        // every source encoding raises UndefinedConversionError — in the
        // direct form from UTF-8 and through the spelled-out UTF-8 pivot
        // from anything else. ASCII-only content still converts, a
        // BINARY receiver is the identity, and `undef: :replace`
        // substitutes.
        run_test_once(
            r#"(f=->(&b){ begin; v=b.call; [v.bytes, v.encoding.name]; rescue => e; [e.class.to_s, e.message]; end }; [
              f.call { "\u3042".encode("ASCII-8BIT") },
              f.call { "caf\u00e9".encode("BINARY") },
              f.call { "\u3042".encode("EUC-JP").encode("ASCII-8BIT") },
              f.call { "\u3042".encode("Shift_JIS").encode("ASCII-8BIT") },
              f.call { "\u00e9".encode("ISO-8859-1").encode("ASCII-8BIT") },
              f.call { "\u3042".encode("UTF-16LE").encode("ASCII-8BIT") },
              f.call { "\u3042".encode("UTF-32BE").encode("ASCII-8BIT") },
              f.call { "abc".encode("ASCII-8BIT") },
              f.call { "abc".encode("US-ASCII").encode("ASCII-8BIT") },
              f.call { "ab".encode("UTF-16LE").encode("ASCII-8BIT") },
              f.call { "\xff\xfe".b.encode("ASCII-8BIT") },
              f.call { "\u3042".encode("ASCII-8BIT", undef: :replace) },
              f.call { "caf\u00e9\u3042".encode("BINARY", undef: :replace) },
              f.call { "\u3042".encode("EUC-JP").encode("ASCII-8BIT", undef: :replace) },
              f.call { "\u3042".encode("ASCII-8BIT", undef: :replace, replace: "!") },
              f.call { "\u3042".encode("ASCII-8BIT", undef: :replace, xml: :text) },
              f.call { "\xff".dup.force_encoding("UTF-8").encode("ASCII-8BIT", invalid: :replace) },
            ])"#,
        );
    }

    #[test]
    fn encode_undefined_conversion_pivot_message() {
        // The `U+XXXX from <src> to <dst>` form is CRuby's only for a
        // UTF-8(-compatible) source; every other source converts through
        // the UTF-8 pivot and the message names the whole chain — with
        // ISO-2022-JP taking the long way round through EUC-JP.
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue => e; e.message; end }; [
              f.call { "\u3042".encode("EUC-JP").encode("US-ASCII") },
              f.call { "\u00e9".encode("ISO-8859-1").encode("US-ASCII") },
              f.call { "\u3042".encode("UTF-16LE").encode("US-ASCII") },
              f.call { "\u3042".encode("EUC-JP").encode("IBM437") },
              f.call { "\u3042".encode("ISO-2022-JP").encode("IBM437") },
              f.call { "\u3042".encode("ISO-2022-JP").encode("US-ASCII") },
              f.call { "\u20ac".encode("UTF-8").encode("EUC-JP") },
              f.call { "\u20ac".encode("UTF-16LE").encode("EUC-JP") },
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_error_names_the_bytes() {
        // CRuby's `InvalidByteSequenceError` names the offending
        // sequence and the encoding it was read as, never the
        // destination — the bytes were already ill-formed as *source*.
        // Three shapes: a plain run, a valid prefix broken by the next
        // byte (which is re-read), and a valid prefix the input ran out
        // on (`incomplete_input?`).
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue Encoding::InvalidByteSequenceError => e;
                 [e.message, e.error_bytes, e.readagain_bytes, e.incomplete_input?,
                  e.source_encoding_name, e.destination_encoding_name,
                  e.source_encoding&.name, e.destination_encoding&.name]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              f.call { g.call("\xff", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xff", "UTF-8").encode("US-ASCII") },
              f.call { g.call("\xff", "UTF-8").encode("ASCII-8BIT") },
              f.call { g.call("\x82", "EUC-JP").encode("UTF-8") },
              f.call { g.call("\xff", "US-ASCII").encode("UTF-8") },
              f.call { g.call("abc\xff", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xc0\x80", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xf5\x80\x80\x80", "UTF-8").encode("EUC-JP") },
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_prefix_and_readagain() {
        // The prefix/read-again split, and the incomplete cases: a
        // truncated multibyte character at the end of the input, and
        // the same character broken by a byte that cannot continue it.
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue Encoding::InvalidByteSequenceError => e;
                 [e.message, e.error_bytes, e.readagain_bytes, e.incomplete_input?]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              f.call { g.call("abc\xc2", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xe3\x81", "UTF-8").encode("Shift_JIS") },
              f.call { g.call("\xf0\x90", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xed\xa0\x80", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xe3\x81\x20", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xf0\x90\x80\x20", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\x81", "Shift_JIS").encode("UTF-8") },
              f.call { g.call("\x81\x20", "Shift_JIS").encode("UTF-8") },
              f.call { g.call("\x81\x40\x81", "Shift_JIS").encode("UTF-8") },
              f.call { g.call("\xa1", "EUC-JP").encode("Shift_JIS") },
              f.call { g.call("\xa1\x20", "EUC-JP").encode("UTF-8") },
              f.call { g.call("\x8f\xa1", "EUC-JP").encode("UTF-8") },
              f.call { g.call("\x8f\xa1\x20", "EUC-JP").encode("UTF-8") },
              f.call { g.call("\x8f\x20", "EUC-JP").encode("UTF-8") },
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_utf16_and_utf32() {
        // UTF-16 and UTF-32 are walked here rather than by
        // `encoding_rs`, and their truncated-group rules are their own:
        // a big-endian low surrogate can never be completed, and a
        // short UTF-32 group is incomplete only while some completion
        // would still be a scalar (`"\x00\x10"` yes, `"\x00\x11"` no).
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue Encoding::InvalidByteSequenceError => e;
                 [e.message, e.error_bytes, e.readagain_bytes, e.incomplete_input?]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              f.call { g.call("\x00\x41\x00", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xd8\x00", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xdc\x00", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xd8\x00\x00\x41", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xd8\x00\x41", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xdc", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xd8", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\x41\x00\x00", "UTF-16LE").encode("UTF-8") },
              f.call { g.call("\x00\x00\x00", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x11", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x10", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x00\xd8", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\xff", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x11\x00\x00", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x00\xd8\x00", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x00\x00\x11", "UTF-32LE").encode("UTF-8") },
              f.call { g.call("\x00\x00\x10", "UTF-32LE").encode("UTF-8") },
              f.call { g.call("\x00\xd8\x00", "UTF-32LE").encode("UTF-8") },
              f.call { g.call("\xff\xff", "UTF-32LE").encode("UTF-8") },
            ])"#,
        );
    }

    #[test]
    fn converter_reads_no_bom() {
        // `Encoding::Converter` built its decoder with `encoding_rs`'s
        // BOM-sniffing constructor where `String#encode` did not, so it
        // swallowed a leading BOM and held a leading `\xFF` back as half
        // of a UTF-16LE one (#1499). CRuby's converter strips nothing.
        run_test_once(
            r#"(t=->(&b){ begin; b.call; rescue => e; [e.class.to_s, e.message]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              t.call { Encoding::Converter.new("UTF-8", "EUC-JP").convert(g.call("\xEF\xBB\xBFa", "UTF-8")) },
              t.call { Encoding::Converter.new("UTF-16LE", "UTF-8").convert(g.call("\xFF\xFEa\x00", "UTF-16LE")).bytes },
              t.call { Encoding::Converter.new("UTF-8", "EUC-JP").convert(g.call("\xff", "UTF-8")) },
              t.call { Encoding::Converter.new("UTF-8", "EUC-JP").convert(g.call("a\xffb", "UTF-8")) },
              t.call { c = Encoding::Converter.new("UTF-8", "EUC-JP")
                       c.convert(g.call("\xff", "UTF-8")) rescue nil
                       c.finish },
            ])"#,
        );
    }

    #[test]
    fn converter_error_detail_matches_the_one_shot_path() {
        // The converter's `primitive_errinfo` now says what
        // `String#encode`'s error says: the same bytes, the same
        // incomplete-vs-invalid split, and no `invalid byte sequence`
        // preamble on the message (#1499). A destination `encoding_rs`
        // has no encoder for (`US-ASCII`, BINARY) walks characters, so
        // it has to find the failing bytes again — it reported none.
        run_test_once(
            r#"(p=->(src, dst, s){ c = Encoding::Converter.new(src, dst); d = "".dup
                 r = c.primitive_convert(s.dup.force_encoding(src), d)
                 [r, d.bytes, c.primitive_errinfo] }; [
              p.call("UTF-8", "EUC-JP", "\xff"),
              p.call("UTF-8", "EUC-JP", "a\xffb"),
              p.call("UTF-8", "EUC-JP", "\xff\xfe"),
              p.call("UTF-8", "EUC-JP", "\xc2"),
              p.call("UTF-8", "EUC-JP", "\xe3\x81"),
              p.call("UTF-8", "US-ASCII", "\xff"),
              p.call("UTF-8", "US-ASCII", "a\xffb"),
              p.call("UTF-8", "US-ASCII", "\xc2"),
              p.call("UTF-8", "US-ASCII", "\xe3\x81"),
              p.call("UTF-8", "US-ASCII", "\xa1\xa1"),
              p.call("UTF-8", "ASCII-8BIT", "\xff"),
              p.call("Shift_JIS", "UTF-8", "\x81"),
              p.call("EUC-JP", "UTF-8", "\x82"),
            ])"#,
        );
    }

    #[test]
    fn converter_still_converts() {
        // The decoder swap is an error-path and BOM change only.
        run_test_once(
            r#"(p=->(src, dst, s){ Encoding::Converter.new(src, dst)
                 .convert(s.dup.force_encoding(src)).bytes }; [
              p.call("UTF-8", "EUC-JP", "a\xe3\x81\x82b"),
              p.call("EUC-JP", "UTF-8", "a\xa4\xa2b"),
              p.call("Shift_JIS", "UTF-8", "a\x82\xa0b"),
              p.call("UTF-8", "Shift_JIS", "a\xe3\x81\x82b"),
              p.call("UTF-8", "US-ASCII", "abc"),
              "a\xffb".dup.force_encoding("UTF-8").encode("EUC-JP", invalid: :replace).bytes,
              "a\xffb".dup.force_encoding("UTF-8").encode("US-ASCII", invalid: :replace, undef: :replace).bytes,
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_past_the_first_block() {
        // The walk streams: an input whose decoded prefix overruns the
        // sink several times still names the sequence that follows it,
        // and so does a destination of BINARY, which decodes through the
        // UTF-8 pivot on its own path.
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue Encoding::InvalidByteSequenceError => e;
                 [e.message, e.error_bytes, e.readagain_bytes, e.incomplete_input?]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              f.call { g.call("a" * 3000 + "\xff", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xe3\x81\x82" * 1000 + "\xff", "UTF-8").encode("EUC-JP") },
              f.call { g.call("\xd8\x00", "UTF-16BE").encode("ASCII-8BIT") },
              f.call { g.call("\xdc\x00", "UTF-16BE").encode("ASCII-8BIT") },
              f.call { g.call("\x00\x11\x00\x00", "UTF-32BE").encode("ASCII-8BIT") },
              f.call { g.call("\x81", "Shift_JIS").encode("ASCII-8BIT") },
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_after_a_well_formed_run() {
        // The walk has to get *past* what decodes before it reaches what
        // does not — a surrogate pair in UTF-16, a whole group in
        // UTF-32 — rather than stopping at the first wide character.
        run_test_once(
            r#"(f=->(&b){ begin; b.call; rescue Encoding::InvalidByteSequenceError => e;
                 [e.message, e.error_bytes, e.readagain_bytes, e.incomplete_input?]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              f.call { g.call("\xd8\x00\xdc\x00\xdc\x00", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\xd8\x00\xdc\x00\x41", "UTF-16BE").encode("UTF-8") },
              f.call { g.call("\x00\x00\x11\x00", "UTF-32LE").encode("UTF-8") },
              f.call { g.call("A\x00\x00\x00\x00\x00\x11\x00", "UTF-32LE").encode("UTF-8") },
              f.call { g.call("\x00\x00\x00A\x00\x11\x00\x00", "UTF-32BE").encode("UTF-8") },
              f.call { g.call("\x41", "UTF-32LE").encode("UTF-8") },
            ])"#,
        );
    }

    #[test]
    fn invalid_byte_sequence_replace_still_replaces() {
        // Naming the bytes is an error-path change only: `invalid:
        // :replace` still substitutes rather than raising, and the
        // converter's own messages are untouched.
        run_test_once(
            r#"(g=->(s, enc){ s.dup.force_encoding(enc) }; [
              g.call("a\xffb", "UTF-8").encode("EUC-JP", invalid: :replace).bytes,
              g.call("a\xffb", "UTF-8").encode("UTF-8", invalid: :replace, replace: "?"),
              g.call("\x82", "EUC-JP").encode("UTF-8", invalid: :replace, replace: "!"),
              g.call("\xe3\x81", "UTF-8").encode("UTF-8", invalid: :replace, replace: "?"),
              g.call("\x81", "Shift_JIS").encode("UTF-8", invalid: :replace, replace: "?"),
              g.call("a\xffb", "UTF-8").scrub("*"),
              g.call("\xd8\x00", "UTF-16BE").encode("UTF-8", invalid: :replace, replace: "?"),
            ])"#,
        );
    }

    #[test]
    fn encode_newline_decorators() {
        crate::tests::run_test_once(
            r##"(a="a\nb\nc".encode("UTF-8", crlf_newline: true); b="a\nb".encode("UTF-8", cr_newline: true); c="a\r\nb\rc\n".encode("UTF-8", universal_newline: true); d="a\nb".encode(Encoding::US_ASCII, crlf_newline: true); e2="a\nb".encode("UTF-16LE", crlf_newline: true).bytes; [a,b,c,d,e2])"##,
        );
    }

    #[test]
    fn encode_seven_bit_and_dummy_wide() {
        // 7-bit content converts into codec-less encodings; 8-bit
        // raises ConverterNotFoundError; the dummy UTF-16 target gets a
        // BOM + big-endian body and #lines does not split it; dummy
        // UTF-7 #lines raises.
        crate::tests::run_test_once(
            r##"(a="\x79".dup.force_encoding(Encoding::BINARY).encode(Encoding::Emacs_Mule); b=(begin; [0x80].pack("C").force_encoding(Encoding::BINARY).encode(Encoding::Emacs_Mule); rescue => e; e.class; end); c=(begin; Encoding::Converter.new(Encoding::Emacs_Mule, Encoding::BINARY); rescue => e; e.class; end); s="a\nb".encode(Encoding::UTF_16); d=s.bytes; e2=s.encoding.name; f=s.lines.map(&:bytes); g="\x00\n\n\x00".dup.force_encoding(Encoding::UTF_16); h=(g.lines == [g]); i=(begin; "a\nb".dup.force_encoding(Encoding::UTF_7).lines; rescue => e; e.class; end); [a,b,c,d,e2,f,h,i])"##,
        );
    }

    #[test]
    fn converter_convert_finish_and_convpath() {
        // #convert buffers partial input and raises with observable
        // state; #finish flags buffered incompletes; convpath /
        // search_convpath report the UTF-8 pivot and decorators.
        crate::tests::run_test_once(
            r##"(t=lambda{|&b| begin; b.call; rescue Exception => e; [e.class, e.message]; end}; r=[]; ec=Encoding::Converter.new("utf-8","iso-8859-1"); r << t.call{ec.convert("\xf1abcd")} << ec.primitive_errinfo << ec.last_error.class; ec2=Encoding::Converter.new("iso-8859-1","Big5"); r << t.call{ec2.convert("\xE9")}[0] << ec2.last_error.message.include?("from ISO-8859-1 to UTF-8 to Big5"); ec3=Encoding::Converter.new("EUC-JP","ISO-8859-1"); r << ec3.convert("\xa4") << t.call{ec3.finish} << ec3.primitive_errinfo; r << Encoding::Converter.new("ASCII","UTF-8").convpath.map{|p| p.map(&:name)} << Encoding::Converter.new("ascii","Big5").convpath.map{|p| p.map(&:name)}; r << Encoding::Converter.new("iso-8859-1","EUC-JP",crlf_newline: true).convpath.last; r << Encoding::Converter.search_convpath("ISO-8859-1","EUC-JP",crlf_newline: true).last; r << Encoding::InvalidByteSequenceError.new.incomplete_input?; r)"##,
        );
    }

    #[test]
    fn iso8859_tables_are_the_iso_ones() {
        // `encoding_rs` resolves `iso-8859-1` / `-9` / `-11` the WHATWG
        // way, onto windows-1252 / -1254 / -874. CRuby uses the real
        // ISO tables, so all three needed one of their own (#1508).
        run_test_once(
            r#"(f=->(enc){ (0x80..0xff).map { |b|
                 s = [b].pack("C*").dup.force_encoding(enc)
                 begin; s.encode("UTF-8").ord; rescue => e; e.class.to_s; end } }; [
              f.call("ISO-8859-1"), f.call("ISO-8859-9"), f.call("ISO-8859-11"),
            ])"#,
        );
    }

    #[test]
    fn iso8859_round_trips_and_refuses_what_it_has_no_cell_for() {
        // The encode direction reads the same tables backwards, and
        // ISO-8859-11's eight unassigned cells are an *undefined
        // conversion* — the byte is a character of the source that
        // Unicode has nowhere to put — not an invalid byte.
        run_test_once(
            r#"(t=->(&b){ begin; b.call; rescue => e; [e.class.to_s, e.message]; end }
               g=->(s, enc){ s.dup.force_encoding(enc) }; [
              t.call { g.call("\xDB", "ISO-8859-11").encode("UTF-8") },
              t.call { g.call("\xDB", "ISO-8859-11").encode("EUC-JP") },
              t.call { g.call("a\xDBb", "ISO-8859-11").encode("UTF-8", undef: :replace).bytes },
              t.call { g.call("a\xDBb", "ISO-8859-11").encode("UTF-8", undef: :replace, replace: "?") },
              t.call { g.call("\xDB", "ISO-8859-11").valid_encoding? },
              t.call { "\u{11E}".encode("ISO-8859-9").bytes },
              t.call { "\u{E01}".encode("ISO-8859-11").bytes },
              t.call { "\u{FF}".encode("ISO-8859-1").bytes },
              t.call { "\u{201A}".encode("ISO-8859-1") },
              t.call { "\u{100}".encode("ISO-8859-1") },
              t.call { (0x80..0xff).map { |b|
                         s = [b].pack("C*").dup.force_encoding("ISO-8859-1")
                         s.encode("UTF-8").encode("ISO-8859-1").bytes[0] } },
            ])"#,
        );
    }

    #[test]
    fn iso8859_converter_matches_the_one_shot_path() {
        // `Encoding::Converter` went straight to `encoding_rs`, so it
        // kept the Windows readings after `String#encode` stopped using
        // them. It consults the same tables now, in both directions,
        // and still reports what it consumed the way CRuby does.
        run_test_once(
            r#"(t=->(&b){ begin; b.call; rescue => e; [e.class.to_s, e.message]; end }
               c=->(src, dst, s){ Encoding::Converter.new(src, dst)
                 .convert(s.dup.force_encoding(src)).bytes }; [
              t.call { c.call("ISO-8859-1", "UTF-8", "\x82\x80\xff") },
              t.call { c.call("ISO-8859-9", "UTF-8", "\xD0\xDD\xFE") },
              t.call { c.call("ISO-8859-11", "UTF-8", "\xA1\xFB") },
              t.call { c.call("UTF-8", "ISO-8859-1", "\u{FF}") },
              t.call { c.call("UTF-8", "ISO-8859-9", "\u{11E}") },
              t.call { c.call("ISO-8859-1", "ISO-8859-9", "\xD0") },
              # The bytes after an undefined character stay in `src`,
              # and what converted before it still reaches `dst`.
              t.call { ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
                       s = "\u{9878}abcd".dup; d = "".dup
                       [ec.primitive_convert(s, d), s, d, ec.primitive_errinfo] },
              # A source byte the encoding assigns nothing to: the
              # classification and what it consumes are CRuby's; the
              # message the converter builds for it is #1511.
              t.call { ec = Encoding::Converter.new("ISO-8859-11", "UTF-8")
                       s = "a\xDBb".dup.force_encoding("ISO-8859-11"); d = "".dup
                       [ec.primitive_convert(s, d), s.bytes, d.bytes] },
            ])"#,
        );
    }

    #[test]
    fn encode_named_byte_encodings() {
        // String#encode routes the named byte encodings through the
        // transcoder: Windows-125x / KOI8-R / IBM866 via encoding_rs,
        // IBM437 via the in-tree table. Round-trips compare bytes.
        run_test_once(
            r##"(a="é".encode("Windows-1250").bytes; b="é".encode("Windows-1250").encoding.name; c="Ω≈±".encode("IBM437").bytes; d="Ω≈±".encode("IBM437").encode("UTF-8"); e2="Привет".encode("KOI8-R").bytes; f="Привет".encode("KOI8-R").encode("UTF-8"); g="Тест".encode("IBM866").bytes; h="абв".encode("Windows-1251").encode("UTF-8"); [a,b,c,d,e2,f,g,h])"##,
        );
        run_test_once(
            r##"(a=(begin; "→".encode("IBM437"); rescue Encoding::UndefinedConversionError => e; e.class; end); b="a→b".encode("IBM437", undef: :replace); [a, b.bytes])"##,
        );
    }

    #[test]
    fn encode_named_byte_constants_and_converter() {
        // CP-numbered constants alias the canonical objects; the
        // Converter handles named byte encodings; ASCII-only content
        // passes through dummy encodings (Big5-UAO) unchanged.
        run_test_once(
            r##"(a=(Encoding::CP1251 == Encoding::Windows_1251); b=(Encoding::CP437 == Encoding::IBM437); c=Encoding::CP1250.name; ec=Encoding::Converter.new("UTF-8", "Windows-1251"); d=ec.convert("да").bytes; e2=ec.destination_encoding.name; f="abc".dup.force_encoding("Big5-UAO").encode("UTF-8"); [a,b,c,d,e2,f])"##,
        );
    }
    use crate::tests::*;

    #[test]
    fn encoding_object_round_trip_without_names() {
        // `force_encoding(Encoding::X)` reads the object's recorded
        // `Encoding` and `String#encoding` answers from a memo, so every
        // registered constant — including the shared-object aliases and
        // the name-only encodings — has to round-trip to the same
        // object CRuby returns, and the name path must be untouched.
        run_tests(&[
            r#"s = +"abc"; [Encoding::UTF_8, Encoding::ASCII_8BIT, Encoding::BINARY, Encoding::US_ASCII, Encoding::ASCII, Encoding::CP65001, Encoding::UTF_16LE, Encoding::UTF_16BE, Encoding::UTF_32LE, Encoding::Shift_JIS, Encoding::SHIFT_JIS, Encoding::Windows_31J, Encoding::EUC_JP, Encoding::ISO_8859_1, Encoding::ISO_8859_15, Encoding::Big5, Encoding::Windows_1252, Encoding::IBM437, Encoding::CP437, Encoding::UTF_7, Encoding::ISO_2022_JP].map { |e| s.force_encoding(e); [s.encoding.name, s.encoding.equal?(e)] }"#,
            r#"s = +"abc"; ["UTF-8", "ASCII-8BIT", "BINARY", "US-ASCII", "utf-8", "Shift_JIS", "Windows-31J", "Big5", "UTF-16LE"].map { |n| s.force_encoding(n); [s.encoding.name, s.encoding.equal?(Encoding.find(n))] }"#,
            r#"o = Object.new; def o.to_str; "US-ASCII"; end; s = +"abc"; s.force_encoding(o); [s.encoding.name, s.encoding.equal?(Encoding::US_ASCII)]"#,
            r#"a = "x".encoding; b = "y".encoding; c = "z".b.encoding; [a.equal?(b), a.equal?(Encoding::UTF_8), c.equal?(Encoding::BINARY), c.equal?(Encoding::ASCII_8BIT), "w".force_encoding("US-ASCII").encoding.equal?(Encoding::US_ASCII)]"#,
            r#"s = +"\xe3\x81\x82"; r = []; [Encoding::BINARY, Encoding::UTF_8, Encoding::Shift_JIS, Encoding::UTF_8].each { |e| s.force_encoding(e); r << [s.encoding.name, s.valid_encoding?, s.length] }; r"#,
            // `Integer#chr`'s mock-encoding override still takes
            // precedence over the string's real encoding.
            r#"c = 0x80.chr(Encoding::Windows_1252); [c.encoding.name, c.bytes, c.encoding.equal?(Encoding::Windows_1252)]"#,
        ]);
        run_test_error(r#""Ruby".force_encoding(Encoding)"#);
        run_test_error(r#""Ruby".force_encoding(Object.new)"#);
        run_test_error(r#""Ruby".force_encoding(nil)"#);
    }

    #[test]
    fn force_encoding() {
        run_tests(&[
            r#""Ruby".force_encoding("ASCII-8BIT")"#,
            r#""Ruby".force_encoding("UTF-8")"#,
            r#""Ruby".force_encoding(Encoding::UTF_8)"#,
            r#""Ruby".force_encoding(Encoding::ASCII_8BIT)"#,
        ]);
        run_test_error(r#""Ruby".force_encoding(:ASCII)"#);
    }

    #[test]
    fn encoding_misc_group_1() {
        // The new (Phase 1) encoding tags round-trip through
        // `force_encoding`/`#encoding`.
        run_tests(&[
            r#""abc".force_encoding("UTF-16LE").encoding == Encoding::UTF_16LE"#,
            r#""abc".force_encoding("UTF-16BE").encoding == Encoding::UTF_16BE"#,
            r#""abc".force_encoding("ISO-8859-1").encoding == Encoding::ISO_8859_1"#,
            r#""abc".force_encoding("ISO-8859-15").encoding == Encoding::ISO_8859_15"#,
            r#""abc".force_encoding("EUC-JP").encoding == Encoding::EUC_JP"#,
            r#""abc".force_encoding("Windows-31J").encoding == Encoding::Windows_31J"#,
            r#"'abc123'.ascii_only?"#,
            r#"''.ascii_only?"#,
            r#"'日本語'.ascii_only?"#,
            r#"'日本語abc123'.ascii_only?"#,
            // `force_encoding("UTF-8")` on bytes that aren't valid UTF-8
            // can now be observed via `valid_encoding?`. Previously the
            // tag silently asserted validity.
            r#"[0xff].pack("C").force_encoding("UTF-8").valid_encoding?"#,
            r#""abc".force_encoding("UTF-8").valid_encoding?"#,
            // UTF-16 needs an even byte count to validate as a code-unit
            // sequence.
            r#""abc".force_encoding("UTF-16LE").valid_encoding?"#,
            r#""ab".force_encoding("UTF-16LE").valid_encoding?"#,
            r#"Encoding.compatible?("abc", "def".encode("US-ASCII")) == Encoding::US_ASCII"#,
            // 7-bit US-ASCII is compatible with any ASCII-compatible
            // encoding; result encoding is the non-7-bit side.
            r#"Encoding.compatible?("abc".force_encoding("US-ASCII"), "\xff") == Encoding::ASCII_8BIT"#,
            // Two distinct non-ASCII-only encodings → nil.
            r#"Encoding.compatible?("\xff".force_encoding("UTF-8"), "\xff".force_encoding("ASCII-8BIT"))"#,
            // Both ASCII-compatible AND both 7-bit → the *first*
            // (left-side) encoding wins, matching CRuby's
            // `rb_enc_compatible`.
            r#"
              utf8 = "abc".force_encoding("UTF-8")
              ascii = "def".force_encoding("US-ASCII")
              left = Encoding.compatible?(utf8, ascii) == Encoding::UTF_8
              right = Encoding.compatible?(ascii, utf8) == Encoding::US_ASCII
              [left, right]
            "#,
            // 7-bit ASCII-compatible string + ISO-8859 with non-ASCII
            // → ISO-8859 wins.
            r#"
              ascii = "abc"
              iso = "\xff".force_encoding("ISO-8859-1")
              Encoding.compatible?(ascii, iso) == Encoding::ISO_8859_1
            "#,
            // Same encoding always compatible, returns that encoding.
            r#"Encoding.compatible?("abc", "def") == Encoding::UTF_8"#,
            r#"Encoding.compatible?("\xff".force_encoding("UTF-8"), "\xfe".force_encoding("UTF-8")) == Encoding::UTF_8"#,
            // `nil` / Integer arguments are not strings or Encoding
            // objects, so the helper returns `nil`. (CRuby additionally
            // accepts Symbols / Regexps / IOs; Phase 1 keeps the
            // Symbol/Regexp/IO paths unimplemented and they fall
            // through to nil too.)
            r#"Encoding.compatible?("abc", nil).nil?"#,
            r#"Encoding.compatible?("abc", 42).nil?"#,
        ]);
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
        run_tests(&[
            r#""abcd".force_encoding("UTF-32LE").encoding == Encoding::UTF_32LE"#,
            r#""abcd".force_encoding("UTF-32BE").encoding == Encoding::UTF_32BE"#,
            // Accept an `Encoding` object as the argument, not just a
            // String.
            r#""Ruby".force_encoding(Encoding::ISO_8859_1).encoding == Encoding::ISO_8859_1"#,
            r#""Ruby".force_encoding(Encoding::UTF_16LE).encoding == Encoding::UTF_16LE"#,
        ]);
    }

    #[test]
    fn force_encoding_unknown_name_raises() {
        run_test_error(r#""abc".force_encoding("Bogus-7")"#);
        run_test_error(r#""abc".force_encoding("UTF-99")"#);
    }

    #[test]
    fn encoding_methods_group_1() {
        // `Encoding#name` reports the canonical CRuby name for
        // each variant.
        run_tests(&[
            r#"Encoding::UTF_16LE.name"#,
            r#"Encoding::ISO_8859_5.name"#,
            r#"Encoding::EUC_JP.name"#,
            r#"Encoding::Windows_31J.name"#,
            // `ascii_compatible?` is false for the UTF-16/32 family,
            // true for the ASCII-compatible ones.
            r#"Encoding::UTF_16LE.ascii_compatible?"#,
            r#"Encoding::UTF_32BE.ascii_compatible?"#,
            r#"Encoding::ISO_8859_1.ascii_compatible?"#,
            r#"Encoding::EUC_JP.ascii_compatible?"#,
            // After `force_encoding("US-ASCII")`, a high byte makes the
            // string non-ASCII-only AND invalid.
            r#"
              s = "\xff".force_encoding("US-ASCII")
              [s.ascii_only?, s.valid_encoding?]
            "#,
            // Mutating a single byte via `setbyte` flips the cached
            // classification — `valid_encoding?` should reflect the new
            // bytes on the next call (the test would expose the cache
            // not being cleared).
            r#"
              s = "abc"
              before = s.valid_encoding?
              s.setbyte(0, 0xff)
              [before, s.valid_encoding?]
            "#,
            // Two broken halves can combine into a valid scalar.
            // CRuby reports the resulting string as `valid_encoding?
            // == true`, which only works if the cache is cleared on
            // every `<<` / `+`.
            r#"
              a = [0xC3].pack("C").force_encoding("UTF-8")
              b = [0xA9].pack("C").force_encoding("UTF-8")
              [a.valid_encoding?, b.valid_encoding?, (a + b).valid_encoding?]
            "#,
            // The same bytes can be SevenBit under one encoding and
            // Broken under another — the cache must clear when the
            // tag changes.
            r#"
              s = "abc"
              first = s.valid_encoding?
              s.force_encoding("UTF-16LE")
              [first, s.valid_encoding?]
            "#,
            // Marshal-dumping a string whose declared encoding is a
            // dummy (UTF-16LE here) writes the bytes opaquely; the
            // round-trip recovers the bytes (the encoding tag isn't
            // preserved because monoruby doesn't currently emit a
            // `:encoding` ivar for non-UTF-8 strings).
            r#"
              s = "abcd".force_encoding("UTF-16LE")
              loaded = Marshal.load(Marshal.dump(s))
              [loaded.bytes, loaded.bytesize]
            "#,
        ]);
    }

    #[test]
    fn string_concat_raises_compat_error() {
        // Two distinct broken sides cannot be concatenated.
        run_test_error(r#""\xff".force_encoding("UTF-8") + "\xff".force_encoding("ASCII-8BIT")"#);
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
        run_tests(&[
            r#"("".force_encoding("UTF-16LE") + "abc").encoding == Encoding::UTF_8"#,
            r#"
              s = "".force_encoding("UTF-16LE")
              s << "abc"
              s.encoding == Encoding::UTF_8
            "#,
        ]);
    }

    #[test]
    fn gsub_raises_compat_error_on_replacement() {
        // Receiver is UTF-8 with non-ASCII content that survives the
        // replace, replacement is an ASCII-8BIT byte → CompatibilityError.
        // (When the receiver's only non-ASCII text is what gets
        // replaced, CRuby lets the result take the replacement's
        // encoding instead: `"é".gsub(/é/, "\xff".b)` is "\xFF" in
        // BINARY — see tests/gsub_binary_replacement.rs.)
        run_test_error(r#""éa".gsub(/a/, "\xff".force_encoding("ASCII-8BIT"))"#);
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
        run_tests(&[
            // UTF-16: count code units (surrogate pairs count as 2).
            r#""ab".force_encoding("UTF-16LE").length"#,
            r#""abc".force_encoding("UTF-16LE").length"#, // 1.5 + 1 (broken trailing)
            // UTF-32: 1 char per 4 bytes.
            r#""abcd".force_encoding("UTF-32LE").length"#,
            // ISO-8859-N: 1 byte per char.
            r#""\xff\xfe".force_encoding("ISO-8859-1").length"#,
            // Broken UTF-8: each invalid byte counts as one char.
            r#""\xF4\x90\x80\x80".length"#,
            r#""a\xF4\x90\x80\x80b".length"#,
            r#""é\xF4\x90\x80\x80è".length"#,
            // Each yielded character carries the source encoding.
            r#""abc".force_encoding("ASCII-8BIT").chars.all? { |c| c.encoding == Encoding::ASCII_8BIT }"#,
            r#""ab".force_encoding("ISO-8859-1").chars.map(&:encoding) == [Encoding::ISO_8859_1, Encoding::ISO_8859_1]"#,
        ]);
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
    fn inspect_under_default_external() {
        // `Encoding.default_external` is now stateful (get/set via a
        // gvar). Container #inspect escapes non-ASCII as \uXXXX and
        // tags the result US-ASCII unless default_external is UTF-8.
        run_tests(&[
            // round-trips get/set
            r#"o = Encoding.default_external
               Encoding.default_external = Encoding::US_ASCII
               r = Encoding.default_external.name
               Encoding.default_external = o
               r"#,
            // non-UTF-8 default_external => escaped + US-ASCII result
            r#"o = Encoding.default_external
               Encoding.default_external = Encoding::US_ASCII
               r = [{ "あ": 1 }.inspect, ["café"].inspect,
                    [1, 2].inspect.encoding.name,
                    { a: 1 }.inspect]
               Encoding.default_external = o
               r"#,
            r#"o = Encoding.default_external
               Encoding.default_external = Encoding.find('UTF-32')
               r = [["jp".encode("EUC-JP"), "utf8"].inspect,
                    ["jp".encode("EUC-JP"), "utf8"].inspect.encoding.name,
                    { あ: 1 }.to_s]
               Encoding.default_external = o
               r"#,
            // UTF-8 default_external => unchanged (bare, UTF-8 result)
            r#"o = Encoding.default_external
               Encoding.default_external = Encoding::UTF_8
               r = [{ あ: 1 }.inspect, ["café"].inspect,
                    ["café"].inspect.encoding.name]
               Encoding.default_external = o
               r"#,
        ]);
    }

    #[test]
    fn encoding_default_internal() {
        run_tests(&[
            r#"Encoding.default_internal"#,
            // setter and getter round-trip
            r#"
            Encoding.default_internal = Encoding::UTF_8
            res = Encoding.default_internal == Encoding::UTF_8
            Encoding.default_internal = nil
            res
        "#,
            r#"
            Encoding.default_internal = nil
            Encoding.default_internal
        "#,
            // string argument to setter
            r#"
            Encoding.default_internal = "UTF-8"
            res = Encoding.default_internal == Encoding::UTF_8
            Encoding.default_internal = nil
            res
        "#,
            r#"
            list = Encoding.list
            list.is_a?(Array)
            "#,
        ]);
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
        run_tests(&[
            r#"
            Encoding.aliases.is_a?(Hash)
            "#,
            r#""hello".encode("UTF-8")"#,
            r#""hello".encode("US-ASCII")"#,
            r#""hello".encode("UTF-8").encoding.name"#,
            r#"s = "hello"; s.encode!("UTF-8"); s.encoding.name"#,
            // PR #361: `String#encode` (Ruby override + Rust encode) updates
            // the encoding tag of the result instead of returning self verbatim.
            r#""hello".encode("US-ASCII").encoding.name"#,
            r#""hello".encode("ASCII-8BIT").encoding.name"#,
            r#""hello".encode(Encoding::US_ASCII).encoding.name"#,
            // Original is untouched; encode returns a copy.
            r#"s = "hello"; s.encode("US-ASCII"); s.encoding.name"#,
            r#"
            Encoding.compatible?("a", "b").nil?.!
            "#,
        ]);
    }

    #[test]
    fn warning_module() {
        // Warning[] returns category status
        run_tests(&[
            "Warning[:deprecated]",
            "Warning[:experimental]",
            "Warning[:performance]",
            // Warning[]= sets category
            r#"
            old = Warning[:deprecated]
            Warning[:deprecated] = false
            res = Warning[:deprecated]
            Warning[:deprecated] = old
            res
        "#,
        ]);
        // Invalid category raises ArgumentError
        run_test_error("Warning[:nonexistent]");
    }

    #[test]
    fn encoding_constants_group_1() {
        run_tests(&[
            // Encoding::CompatibilityError exists and inherits from EncodingError
            "Encoding::CompatibilityError.is_a?(Class)",
            "Encoding::CompatibilityError < EncodingError",
            // ASCII-compatible encodings
            r#"Encoding::UTF_8.ascii_compatible?"#,
            r#"Encoding::US_ASCII.ascii_compatible?"#,
            r#"Encoding::ASCII_8BIT.ascii_compatible?"#,
            r#"Encoding::ISO_8859_1.ascii_compatible?"#,
            r#"Encoding::Shift_JIS.ascii_compatible?"#,
            r#"Encoding::EUC_JP.ascii_compatible?"#,
            // Non-ASCII-compatible encodings
            r#"Encoding::UTF_16.ascii_compatible?"#,
            r#"Encoding::UTF_16BE.ascii_compatible?"#,
            r#"Encoding::UTF_16LE.ascii_compatible?"#,
            r#"Encoding::UTF_32.ascii_compatible?"#,
            r#"Encoding::UTF_32BE.ascii_compatible?"#,
            r#"Encoding::UTF_32LE.ascii_compatible?"#,
            // Non-dummy encodings
            r#"Encoding::UTF_8.dummy?"#,
            r#"Encoding::US_ASCII.dummy?"#,
            r#"Encoding::ASCII_8BIT.dummy?"#,
            r#"Encoding::UTF_16BE.dummy?"#,
            r#"Encoding::UTF_16LE.dummy?"#,
            r#"Encoding::Shift_JIS.dummy?"#,
            // Dummy encodings (stateful / no-BOM UTF-16/32)
            r#"Encoding::UTF_16.dummy?"#,
            r#"Encoding::UTF_32.dummy?"#,
            r#"Encoding::ISO_2022_JP.dummy?"#,
            // Defined and reachable as Encoding objects (CRuby uses
            // mixed-case canonical identifiers for the dummy / mac-family
            // names — this is the literal constant identifier).
            r#"Encoding::UTF_7.is_a?(Encoding)"#,
            r#"Encoding::Emacs_Mule.is_a?(Encoding)"#,
            r#"Encoding::CP50220.is_a?(Encoding)"#,
            r#"Encoding::CP50221.is_a?(Encoding)"#,
            // Their `dummy?` is true (CRuby-strict set).
            r#"Encoding::UTF_7.dummy?"#,
            r#"Encoding::CP50220.dummy?"#,
            // Mac-family aliases reachable, dummy? false.
            r#"Encoding::MacCyrillic.is_a?(Encoding)"#,
            r#"Encoding::MacGreek.is_a?(Encoding)"#,
            r#"Encoding::MacRoman.is_a?(Encoding)"#,
            r#"Encoding::MacTurkish.dummy?"#,
            r#"Encoding::Big5_HKSCS.is_a?(Encoding)"#,
            // ASCII <-> US_ASCII share the same Value (object identity).
            r#"Encoding::ASCII.equal?(Encoding::US_ASCII)"#,
            r#"Encoding::ASCII == Encoding::US_ASCII"#,
            // CP65001 <-> UTF_8 share the same Value.
            r#"Encoding::CP65001.equal?(Encoding::UTF_8)"#,
            r#"Encoding::CP65001 == Encoding::UTF_8"#,
            // BINARY <-> ASCII_8BIT (existing behaviour, asserted for parity).
            r#"Encoding::BINARY.equal?(Encoding::ASCII_8BIT)"#,
            // Pre-existing: Encoding.find with a String name.
            r#"Encoding.find("UTF-8").is_a?(Encoding)"#,
            // New: Encoding.find with an Encoding object returns it as-is.
            r#"Encoding.find(Encoding::UTF_8).equal?(Encoding::UTF_8)"#,
            r#"Encoding.find(Encoding::ASCII_8BIT).equal?(Encoding::ASCII_8BIT)"#,
            r#"Encoding.name_list.is_a?(Array)"#,
            r#"Encoding.name_list.all? { |n| n.is_a?(String) }"#,
            r#"Encoding.name_list.include?("UTF-8")"#,
            r#"Encoding.name_list.include?("ASCII-8BIT")"#,
            // Aliases listed alongside canonical names.
            r#"Encoding.name_list.include?("BINARY")"#,
            r#"Encoding.name_list.include?("CP65001")"#,
            r#"Encoding::UTF_8.names.is_a?(Array)"#,
            r#"Encoding::UTF_8.names.include?("UTF-8")"#,
            r#"Encoding::UTF_8.names.include?("CP65001")"#,
            r#"Encoding::ASCII_8BIT.names.include?("ASCII-8BIT")"#,
            r#"Encoding::ASCII_8BIT.names.include?("BINARY")"#,
            r#"Encoding::US_ASCII.names.include?("US-ASCII")"#,
            r#"Encoding::US_ASCII.names.include?("ASCII")"#,
            // ASCII-8BIT renders as the 3.4+ canonical `BINARY (ASCII-8BIT)` form.
            r#"Encoding::ASCII_8BIT.inspect"#,
            // Plain non-dummy encoding: `#<Encoding:NAME>`.
            r#"Encoding::UTF_8.inspect"#,
            r#"Encoding::US_ASCII.inspect"#,
            // Dummy encoding: `(dummy)` suffix.
            r#"Encoding::UTF_7.inspect"#,
            r#"Encoding::ISO_2022_JP.inspect"#,
            r#"Encoding::UTF_16.inspect"#,
        ]);
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

    #[test]
    fn converter_group_1() {
        run_tests(&[
            // src/dst encodings round-trip through the stashed ivars, and
            // a basic ASCII-only convert returns the input bytes tagged
            // with the destination encoding.
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").source_encoding == Encoding::UTF_8"#,
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").destination_encoding == Encoding::Shift_JIS"#,
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").convert("hello")"#,
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").convert("hello").encoding == Encoding::Shift_JIS"#,
            // CRuby formats Converters as `#<Encoding::Converter: SRC to DST>`.
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").inspect"#,
            r#"Encoding::Converter.new("Shift_JIS", "UTF-8").inspect"#,
            // After `#finish`, subsequent `#convert` raises ArgumentError.
            // `finish` itself returns an empty string in the dst encoding.
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").finish"#,
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").finish.encoding == Encoding::Shift_JIS"#,
            r#"
              ec = Encoding::Converter.new("UTF-8", "Shift_JIS")
              ec.finish
              begin
                ec.convert("x")
                :no_raise
              rescue ArgumentError
                :ok
              end
            "#,
            // Default replacement: "?" tagged US-ASCII for non-UTF dst,
            // "�" tagged UTF-8 for UTF-8 dst.
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").replacement"#,
            r#"Encoding::Converter.new("UTF-8", "Shift_JIS").replacement.encoding == Encoding::US_ASCII"#,
            r#"Encoding::Converter.new("Shift_JIS", "UTF-8").replacement"#,
            r#"Encoding::Converter.new("Shift_JIS", "UTF-8").replacement.encoding == Encoding::UTF_8"#,
            // Setter validates encodability against the destination.
            r#"
              ec = Encoding::Converter.new("UTF-8", "Shift_JIS")
              ec.replacement = "?"
              ec.replacement
            "#,
            // Unencodable replacement raises Encoding::UndefinedConversionError.
            // ISO-8859-1 has an `encoding_rs` entry, so the encodability
            // check actually runs (US-ASCII would skip it because monoruby
            // routes UsAscii through a fast path that bypasses
            // `encoding_to_rs`).
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              begin
                ec.replacement = "日"
                :no_raise
              rescue Encoding::UndefinedConversionError
                :ok
              end
            "#,
            // Two Converters compare equal iff they share src and dst.
            r#"
              a = Encoding::Converter.new("UTF-8", "Shift_JIS")
              b = Encoding::Converter.new("UTF-8", "Shift_JIS")
              a == b
            "#,
            r#"
              a = Encoding::Converter.new("UTF-8", "Shift_JIS")
              b = Encoding::Converter.new("Shift_JIS", "UTF-8")
              a == b
            "#,
        ]);
    }

    #[test]
    fn converter_utf16_32_pairs_supported() {
        // UTF-16/UTF-32 (LE/BE) now have hand-rolled codecs, so
        // `Encoding::Converter.new` accepts these pairs (it used to
        // raise ConverterNotFoundError for UTF-32).
        run_tests(&[
            r#"Encoding::Converter.new("UTF-8", "UTF-32BE").class.name"#,
            r#"Encoding::Converter.new("UTF-32LE", "UTF-8").class.name"#,
            r#"Encoding::Converter.new("UTF-16BE", "UTF-32LE").class.name"#,
        ]);
    }

    #[test]
    fn converter_utf16_32_transcodes_both_ways() {
        // The UTF-16/UTF-32 codecs are reached through a UTF-8 pivot,
        // so both a wide source and a wide destination convert, and a
        // wide pair converts through the middle.
        run_tests(&[
            r#"Encoding::Converter.new("UTF-8", "UTF-16LE").convert("a\u3042").bytes"#,
            r#"Encoding::Converter.new("UTF-8", "UTF-32BE").convert("a\u3042").bytes"#,
            r#"Encoding::Converter.new("UTF-16LE", "UTF-8").convert("a\u3042".encode("UTF-16LE")).bytes"#,
            r#"Encoding::Converter.new("UTF-16BE", "UTF-32LE").convert("a\u3042".encode("UTF-16BE")).bytes"#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-16BE")
              s = +"abc"
              d = +""
              r = c.primitive_convert(s, d)
              [r, s, d.bytes]
            "#,
            // A destination cap that cuts the output mid-way stops with
            // `:destination_buffer_full`, and a cap that cannot hold a
            // whole code unit writes none of it.
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-16LE")
              s = +"abc"
              d = +""
              [c.primitive_convert(s, d, nil, 2), d.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-32LE")
              s = +"abc"
              d = +""
              c.primitive_convert(s, d, nil, 3)
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-16LE")
              s = +"\u3042b"
              d = +""
              [c.primitive_convert(s, d, nil, 2), d.bytes]
            "#,
            r#""a\u3042".encode("UTF-32LE").encode("UTF-8").bytes"#,
            // `invalid: :replace` on a wide source replaces the bad
            // unit and converts the rest, in either byte order.
            r#""a\x00\x00\xd8\x62\x00".b.force_encoding("UTF-16LE").encode("UTF-8", invalid: :replace).bytes"#,
            r#""\x00\x61\xd8\x00\x00\x62".b.force_encoding("UTF-16BE").encode("UTF-8", invalid: :replace).bytes"#,
        ]);
    }

    #[test]
    fn converter_utf16_32_source_error_positions() {
        // Where a wide source first stops decoding: a UTF-32 group that
        // is a surrogate or out of range, a group the input ends in the
        // middle of, a lone low surrogate, an odd trailing byte — and,
        // for each, that a well-formed surrogate *pair* before it is
        // stepped over rather than mistaken for the error. Everything
        // decoded before the bad unit still reaches the destination.
        run_test_once(
            r#"
              def t(s, enc)
                c = Encoding::Converter.new(enc, "UTF-8")
                src = +s.b
                d = +""
                r = c.primitive_convert(src, d)
                e = c.primitive_errinfo
                [r, d.bytes, e[3]&.bytes, e[4]&.bytes]
              end
              [
                t("\x61\x00\x00\x00\x00\xd8\x00\x00", "UTF-32LE"),
                t("\x61\x00\x00\x00\x00\x00\x11\x00", "UTF-32LE"),
                t("\x61\x00\x00\x00\x62\x00", "UTF-32LE"),
                t("\x00\xd8\x00\xdc\x00\xdc\x61\x00", "UTF-16LE"),
                t("\x61\x00\x00\xdc", "UTF-16LE"),
                t("\x61\x00\x62", "UTF-16LE"),
                t("\x00\xd8\x00\xdc\x61\x00", "UTF-16LE"),
                t("\x00\x00\x00\x61\x00\x00\xd8\x00", "UTF-32BE"),
                t("\x00\x61\xdc\x00", "UTF-16BE"),
              ]
            "#,
        );
    }

    #[test]
    fn converter_utf16_readagain_is_byte_order_dependent() {
        // A high surrogate that no low surrogate completes: how much of
        // the following unit CRuby re-reads is how much of it the byte
        // order made it read before it knew. Little-endian the second
        // byte decides, so the whole unit is put back — and a high
        // surrogate with a single byte after it is an *incomplete*
        // three-byte prefix, not a finished error. Big-endian the first
        // byte decides, so one byte suffices.
        run_tests(&[
            r#"
              c = Encoding::Converter.new("UTF-16LE", "ISO-8859-1")
              s = +"a\x00\x00\xd8\x62\x00".b
              d = +""
              r = c.primitive_convert(s, d)
              [r, d.bytes, c.primitive_errinfo[3].bytes, c.primitive_errinfo[4].bytes, c.putback.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-16BE", "ISO-8859-1")
              s = +"\x00\x61\xd8\x00\x00\x62".b
              d = +""
              r = c.primitive_convert(s, d)
              [r, d.bytes, c.primitive_errinfo[3].bytes, c.primitive_errinfo[4].bytes, c.putback.bytes]
            "#,
            r#"
              begin
                "\x00\xd8\x61".b.force_encoding("UTF-16LE").encode("UTF-8")
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes.bytes, e.readagain_bytes&.bytes, e.incomplete_input?]
              end
            "#,
            r#"
              begin
                "\xd8\x00\x00".b.force_encoding("UTF-16BE").encode("UTF-8")
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes.bytes, e.readagain_bytes&.bytes, e.incomplete_input?]
              end
            "#,
            // A high surrogate with nothing at all after it is an
            // incomplete pair whichever way round the units are.
            r#"
              begin
                "\x00\xd8".b.force_encoding("UTF-16LE").encode("UTF-8")
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes.bytes, e.readagain_bytes&.bytes, e.incomplete_input?]
              end
            "#,
        ]);
    }

    #[test]
    fn converter_group_2() {
        run_tests(&[
            // The 3rd arg (Integer flag mask) is tolerated; constructor
            // succeeds.
            r#"
              flags = Encoding::Converter::INVALID_REPLACE | Encoding::Converter::UNDEF_REPLACE
              Encoding::Converter.new("UTF-8", "Shift_JIS", flags).is_a?(Encoding::Converter)
            "#,
            // The Encoding::Converter::* flag constants exist as Integers
            // (the values themselves don't have to match CRuby — only
            // that they're defined and integer-typed so spec setup
            // like `INVALID_REPLACE | UNDEF_REPLACE` works).
            r#"Encoding::Converter::INVALID_REPLACE.is_a?(Integer)"#,
            r#"Encoding::Converter::UNDEF_REPLACE.is_a?(Integer)"#,
            r#"Encoding::Converter::UNDEF_HEX_CHARREF.is_a?(Integer)"#,
            r#"Encoding::Converter::PARTIAL_INPUT.is_a?(Integer)"#,
            r#"Encoding::Converter::AFTER_OUTPUT.is_a?(Integer)"#,
            r#"Encoding::Converter::UNIVERSAL_NEWLINE_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::CRLF_NEWLINE_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::CR_NEWLINE_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::XML_TEXT_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::XML_ATTR_CONTENT_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::XML_ATTR_QUOTE_DECORATOR.is_a?(Integer)"#,
            r#"Encoding::Converter::INVALID_MASK.is_a?(Integer)"#,
            r#"Encoding::Converter::UNDEF_MASK.is_a?(Integer)"#,
            // UTF-16/32 → UTF-8, ISO-2022-JP → STATELESS_ISO_2022_JP,
            // ASCII-compatible inputs → nil. Accepts both Encoding
            // objects and string names.
            r#"Encoding::Converter.asciicompat_encoding(Encoding::UTF_16BE) == Encoding::UTF_8"#,
            r#"Encoding::Converter.asciicompat_encoding(Encoding::UTF_16LE) == Encoding::UTF_8"#,
            r#"Encoding::Converter.asciicompat_encoding("UTF-16LE") == Encoding::UTF_8"#,
            r#"Encoding::Converter.asciicompat_encoding(Encoding::UTF_8)"#,
            r#"Encoding::Converter.asciicompat_encoding("Shift_JIS")"#,
        ]);
    }

    #[test]
    fn converter_search_convpath() {
        // Single-step path for any directly supported pair. The
        // returned array round-trips Encoding objects, which aren't
        // re-parseable Ruby literals — observe its shape via
        // accessors that yield comparable scalars.
        run_tests(&[
            r#"Encoding::Converter.search_convpath("UTF-8", "Shift_JIS").length"#,
            r#"Encoding::Converter.search_convpath("UTF-8", "Shift_JIS")[0].length"#,
            r#"Encoding::Converter.search_convpath("UTF-8", "Shift_JIS")[0][0] == Encoding::UTF_8"#,
            r#"Encoding::Converter.search_convpath("UTF-8", "Shift_JIS")[0][1] == Encoding::Shift_JIS"#,
        ]);
        // UTF-32 is now a supported direct pair (hand-rolled codec).
        run_tests(&[
            r#"Encoding::Converter.search_convpath("UTF-8", "UTF-32BE").length"#,
            r#"Encoding::Converter.search_convpath("UTF-8", "UTF-32BE")[0][1] == Encoding::UTF_32BE"#,
        ]);
    }

    #[test]
    fn converter_streaming_stubs_callable() {
        // The streaming-API stubs (`primitive_errinfo`, `last_error`,
        // `putback`) exist and return non-erroring values so spec setup
        // that touches them doesn't NoMethodError. Their *exact* return
        // values diverge from CRuby (monoruby is single-shot, CRuby
        // tracks mid-stream state), so we only assert callability and
        // shape, not output equality.
        run_test_no_result_check(
            r#"
              ec = Encoding::Converter.new("UTF-8", "Shift_JIS")
              raise unless ec.last_error.nil?
              raise unless ec.putback.is_a?(String)
              info = ec.primitive_errinfo
              raise unless info.is_a?(Array)
              raise unless info.length == 5
              raise unless info[0] == :source_buffer_empty
            "#,
        );
    }

    #[test]
    fn encoding_undefined_conversion_error_metadata() {
        // `Encoding::UndefinedConversionError` exposes parsed
        // source/destination encoding info on instances. monoruby
        // recovers these by pattern-matching its canonical error
        // message; the message text itself diverges from CRuby's, so
        // we assert structurally rather than via `run_test`.
        run_test_no_result_check(
            r#"
              begin
                "日本".encode("US-ASCII")
              rescue Encoding::UndefinedConversionError => e
                raise unless e.respond_to?(:source_encoding_name)
                raise unless e.respond_to?(:destination_encoding_name)
                raise unless e.respond_to?(:source_encoding)
                raise unless e.respond_to?(:destination_encoding)
                raise unless e.respond_to?(:error_char)
                raise unless e.source_encoding_name == "UTF-8"
                raise unless e.destination_encoding_name == "US-ASCII"
                raise unless e.source_encoding == Encoding::UTF_8
                raise unless e.destination_encoding == Encoding::US_ASCII
                raise unless e.error_char == "日"
              end
            "#,
        );
    }

    #[test]
    fn encoding_invalid_byte_sequence_error_metadata() {
        // `Encoding::InvalidByteSequenceError` exposes the parsed
        // source/destination encoding pair and `incomplete_input?`
        // (always `false` in monoruby — the transcoder doesn't
        // distinguish partial-vs-invalid).
        run_test_no_result_check(
            r#"
              begin
                "\xff".force_encoding("UTF-8").encode("Shift_JIS")
              rescue Encoding::InvalidByteSequenceError => e
                raise unless e.respond_to?(:source_encoding_name)
                raise unless e.respond_to?(:destination_encoding_name)
                raise unless e.respond_to?(:source_encoding)
                raise unless e.respond_to?(:destination_encoding)
                raise unless e.respond_to?(:incomplete_input?)
                raise unless e.source_encoding == Encoding::UTF_8
                raise unless e.destination_encoding == Encoding::Shift_JIS
                raise unless e.incomplete_input? == false
              end
            "#,
        );
    }

    #[test]
    fn encoding_compatible_empty_string_adopts_other_encoding() {
        // The PR routes the String/String case through the inner-aware
        // `compatible_encoding` so the empty-side rule for
        // ASCII-incompatible encodings is honoured. Pre-PR, the
        // legacy `Encoding::compatible(SevenBit, SevenBit, ...)` path
        // returned `nil` for any pair whose encodings weren't *both*
        // ASCII-compatible — these cases now agree with CRuby:
        // empty UTF-16LE + non-empty UTF-8 → UTF-8 (non-empty side
        // wins), and the symmetric arrangement.
        run_tests(&[
            r#"Encoding.compatible?("".force_encoding("UTF-16LE"), "abc") == Encoding::UTF_8"#,
            r#"Encoding.compatible?("abc", "".force_encoding("UTF-16LE")) == Encoding::UTF_8"#,
            // Both empty UTF-16LE → UTF-16LE (empty/empty rule).
            r#"Encoding.compatible?("".force_encoding("UTF-16LE"), "".force_encoding("UTF-16LE")) == Encoding::UTF_16LE"#,
            // Two non-empty UTF-16LE strings stay UTF-16LE.
            r#"Encoding.compatible?("abc".force_encoding("UTF-16LE"), "def".force_encoding("UTF-16LE")) == Encoding::UTF_16LE"#,
            // New constant aliases added by the PR — `Encoding::UTF8_MAC`,
            // `Encoding::UTF_8_MAC`, `Encoding::CESU_8` — exist as
            // Encoding instances.
            r#"Encoding::UTF8_MAC.is_a?(Encoding)"#,
            r#"Encoding::UTF_8_MAC.is_a?(Encoding)"#,
            r#"Encoding::CESU_8.is_a?(Encoding)"#,
        ]);
    }

    #[test]
    fn encoding_locale_charmap() {
        // `Encoding.locale_charmap` follows the locale environment, so
        // assert only the properties that hold on every CI host: it is
        // a String, it names an encoding `Encoding.find` knows, and it
        // is the encoding `Encoding.find("locale")` answers with.
        run_test_no_result_check(
            r#"
              raise unless Encoding.locale_charmap.is_a?(String)
              raise unless Encoding.find(Encoding.locale_charmap) == Encoding.find("locale")
              raise unless Encoding.aliases["locale"] == Encoding.find("locale").name
            "#,
        );
    }

    #[test]
    fn encode_replacement_options() {
        run_tests(&[
            // `replace:` alone implies `undef: :replace` ...
            r#""test\u0100".encode(Encoding::Windows_1252, replace: "?")"#,
            // ... but not when `invalid: :replace` was given explicitly.
            r#"begin
                 "test\u0100".encode(Encoding::Windows_1252, invalid: :replace, replace: "")
               rescue => e
                 e.class.to_s
               end"#,
            // ... and it never turns invalid bytes into replacements.
            r#"begin
                 "ab\xFFc".dup.force_encoding("utf-8").encode(Encoding::ISO_8859_1, replace: "!")
               rescue => e
                 e.class.to_s
               end"#,
            // An invalid byte is replaced with the *destination's*
            // replacement, not with U+FFFD (which would then be an
            // undefined conversion).
            r#""ab\xFFc".dup.force_encoding("utf-8").encode(Encoding::ISO_8859_1, invalid: :replace)"#,
            r#""ab\xFFc".dup.force_encoding("utf-8").encode(Encoding::ISO_8859_1, invalid: :replace, replace: "!")"#,
            // Same encoding in and out still scrubs.
            r#""ab\xFFc".dup.force_encoding("utf-8").encode("utf-8", invalid: :replace).bytes"#,
            r#""\u3042?\u3042".encode(Encoding::EUC_JP, undef: :replace).bytes"#,
        ]);
    }

    #[test]
    fn encode_decorators_without_a_destination() {
        run_tests(&[
            // No destination encoding: the options still apply.
            r#"["\r\nfoo", "\rfoo", "\nfoo"].map { |s| s.encode(cr_newline: true) }"#,
            r#"["\r\nfoo", "\rfoo", "\nfoo"].map { |s| s.encode(crlf_newline: true) }"#,
            r#"["\r\nfoo", "\rfoo", "\nfoo"].map { |s| s.encode(universal_newline: true) }"#,
            // The xml decorator writes numeric character references for
            // characters the destination cannot hold, and tags the
            // result with the destination encoding.
            r#""\u00FCrst".encode(Encoding::US_ASCII, xml: :text)"#,
            r#""\u00FCrst".encode(Encoding::US_ASCII, xml: :attr)"#,
            r#""\u00FCrst".encode(Encoding::US_ASCII, xml: :text).encoding.to_s"#,
            r#""& < > \"".encode("UTF-8", xml: :attr)"#,
            r#""& < > \"".encode("UTF-8", xml: :text)"#,
        ]);
    }

    #[test]
    fn encoding_alias_tables_are_consistent() {
        // `Encoding.aliases`, `Encoding.name_list` and `Encoding#names`
        // are all derived from one table, so ruby/spec's
        // self-consistency checks hold by construction.
        run_test_no_result_check(
            r##"
              list = Encoding.name_list
              Encoding.aliases.each do |a, canonical|
                raise "#{a} missing from name_list" unless list.include?(a)
                raise "#{a} does not resolve" unless Encoding.find(a) == Encoding.find(canonical)
                raise "#{a} missing from names" unless Encoding.find(a).names.include?(a)
              end
              raise unless Encoding::ASCII_8BIT.names.first == "ASCII-8BIT"
              raise unless Encoding::ASCII_8BIT.names.include?("BINARY")
              raise unless Encoding.find("LOCALE") == Encoding.find("locale")
              raise unless Encoding.find("EXTERNAL") == Encoding.find("external")
              raise unless Encoding.find("Internal") == Encoding.find("internal")
            "##,
        );
    }

    #[test]
    fn encoding_constants_use_crubys_spellings() {
        // A display name is not a constant name: `Encoding::ISO-8859-1`
        // is unwritable. `set_encoding_const`'s rule turns every name
        // and alias into the constant(s) CRuby registers for it.
        run_test_no_result_check(
            r##"
              pairs = {
                "ISO8859_9" => "ISO-8859-9", "ISO_8859_9" => "ISO-8859-9",
                "ISO8859_1" => "ISO-8859-1",
                "EUCJP" => "EUC-JP", "EUC_JP" => "EUC-JP",
                "SJIS" => "Windows-31J", "SHIFT_JIS" => "Shift_JIS",
                "PCK" => "Windows-31J", "CP932" => "Windows-31J",
                "WINDOWS_1252" => "Windows-1252", "Windows_1252" => "Windows-1252",
                "CP1252" => "Windows-1252",
                "BIG5" => "Big5", "UCS_2BE" => "UTF-16BE", "UCS_4LE" => "UTF-32LE",
                "CP65000" => "UTF-7", "CP878" => "KOI8-R", "EUCCN" => "GB2312",
                "ANSI_X3_4_1968" => "US-ASCII", "ASCII" => "US-ASCII",
                "UTF_8" => "UTF-8", "CP65001" => "UTF-8",
                "EMACS_MULE" => "Emacs-Mule", "Emacs_Mule" => "Emacs-Mule",
              }
              pairs.each do |const, name|
                e = Encoding.const_get(const)
                raise "#{const} => #{e.name}, want #{name}" unless e.name == name
              end
              # No constant may carry a character Ruby cannot write.
              bad = Encoding.constants.map(&:to_s).grep(/[^A-Za-z0-9_]/)
              raise "unwritable constants: #{bad.inspect}" unless bad.empty?
              # A name starting with a digit contributes no constant.
              raise unless Encoding.constants.map(&:to_s).none? { |c| c =~ /\A[0-9]/ }
            "##,
        );
    }

    #[test]
    fn converter_error_carries_its_attributes() {
        // The object a Converter raises is the one `#last_error`
        // returns, so every accessor is populated — including the
        // *stage* encodings of a multi-step conversion path.
        run_tests(&[
            r##"
              ec = Encoding::Converter.new("utf-8", "iso-8859-1")
              begin
                ec.convert("\xf1abcd")
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes, e.error_bytes.encoding.to_s,
                 e.readagain_bytes, e.readagain_bytes.encoding.to_s,
                 e.incomplete_input?,
                 e.source_encoding.to_s, e.destination_encoding.to_s,
                 e.source_encoding_name, e.destination_encoding_name]
              end
            "##,
            // EUC-JP → ISO-8859-1 goes through the UTF-8 pivot, so the
            // decode error reports EUC-JP → UTF-8, and the malformed
            // run splits into the pending lead byte and the byte that
            // disproved it.
            r##"
              ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
              begin
                ec.convert("abc\xA1\xFFdef")
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes, e.readagain_bytes,
                 e.source_encoding.to_s, e.destination_encoding.to_s]
              end
            "##,
            r##"
              ec = Encoding::Converter.new("utf-8", "ascii")
              begin
                ec.convert("\u{8765}")
              rescue Encoding::UndefinedConversionError => e
                [e.error_char, e.error_char.encoding.to_s,
                 e.source_encoding.to_s, e.destination_encoding.to_s,
                 e.source_encoding_name, e.destination_encoding_name]
              end
            "##,
            r##"
              ec = Encoding::Converter.new("ISO-8859-1", "EUC-JP")
              begin
                ec.convert("\xA0")
              rescue Encoding::UndefinedConversionError => e
                [e.error_char.bytes, e.source_encoding.to_s, e.destination_encoding.to_s]
              end
            "##,
            // A converter left holding an incomplete tail raises at
            // `#finish`, with the same attributes.
            r##"
              ec = Encoding::Converter.new("utf-8", "iso-8859-1")
              ec.convert("ab\xE3")
              begin
                ec.finish
              rescue Encoding::InvalidByteSequenceError => e
                [e.error_bytes, e.readagain_bytes, e.incomplete_input?,
                 e.source_encoding.to_s]
              end
            "##,
        ]);
    }

    #[test]
    fn encoding_compatible_regexp_string_is_asymmetric() {
        // CRuby's `enc_compatible_latter` swaps the two *values* but
        // leaves the encodings bound to the original operands, so the
        // answer depends on which side the Regexp is.
        run_tests(&[
            r#"
              r = Regexp.new("\xa4\xa2".dup.force_encoding("euc-jp"))
              Encoding.compatible?(r, "hello".dup.force_encoding("utf-8")).name
            "#,
            r#"
              r = Regexp.new("\xa4\xa2".dup.force_encoding("euc-jp"))
              Encoding.compatible?("hello".dup.force_encoding("utf-8"), r).name
            "#,
            r#"
              r = Regexp.new("\xa4\xa2".dup.force_encoding("euc-jp"))
              Encoding.compatible?(r, "hello".dup.force_encoding("euc-jp")).name
            "#,
            r#"Encoding.compatible?(/abc/, "abc".dup.force_encoding("us-ascii")).name"#,
        ]);
    }

    #[test]
    fn primitive_convert_group_1() {
        run_tests(&[
            // ASCII round-trip through the all-ASCII fast path: returns
            // `:finished`, clears `src`, writes the converted bytes to `dst`,
            // and tags `dst` with the destination encoding.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              src = "hello"
              dst = ""
              [ec.primitive_convert(src, dst), src, dst, dst.encoding == Encoding::ISO_8859_1]
            "#,
            // `primitive_convert(nil, "")` is a no-op that returns `:finished`.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert(nil, "")
            "#,
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("", "")
            "#,
        ]);
    }

    #[test]
    fn primitive_convert_frozen_dst_raises() {
        run_test_error(
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("", "".freeze)
            "#,
        );
    }

    #[test]
    fn primitive_convert_dst_offset_group() {
        run_tests(&[
            // Default `dst_offset` (omitted or nil) is "end of dst" so the
            // converter appends rather than overwrites.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "aa"
              ec.primitive_convert("bc", dst)
              dst
            "#,
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "aa"
              ec.primitive_convert("bc", dst, nil)
              dst
            "#,
            // An explicit `dst_offset` truncates `dst` to that prefix and
            // appends the converted bytes there.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "abc"
              ec.primitive_convert("XY", dst, 1)
              dst
            "#,
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "abc"
              ec.primitive_convert("XY", dst, 0)
              dst
            "#,
            // Non-Integer `dst_offset` is coerced via `to_int`.
            r#"
              klass = Class.new do
                def initialize(n); @n = n; end
                def to_int; @n; end
              end
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "   "
              result = ec.primitive_convert("abc", dst, klass.new(2))
              [result, dst]
            "#,
        ]);
    }

    #[test]
    fn primitive_convert_dst_offset_out_of_range_raises() {
        // `dst_offset > dst.bytesize` raises ArgumentError.
        run_test_error(
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("", "am", 3)
            "#,
        );
        // Offsets within range (0..bytesize) succeed.
        run_test(
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("", "am", 0)
              ec.primitive_convert("", "am", 1)
              ec.primitive_convert("", "am", 2)
            "#,
        );
    }

    #[test]
    fn primitive_convert_group_2() {
        run_tests(&[
            // `dst_bytesize` caps how many bytes can be written and yields
            // `:destination_buffer_full` when the source needs more room.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = ""
              result = ec.primitive_convert("glark", dst, nil, 1)
              [result, dst.bytesize, dst]
            "#,
            // Non-Integer `dst_bytesize` is coerced via `to_int`.
            r#"
              klass = Class.new do
                def initialize(n); @n = n; end
                def to_int; @n; end
              end
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "   "
              result = ec.primitive_convert("abc", dst, 0, klass.new(2))
              [result, dst]
            "#,
            // Nil `dst_bytesize` means unlimited; the full source is written.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = ""
              ec.primitive_convert("glark", dst, nil, nil)
              dst.bytesize
            "#,
            // The cross-call pending buffer carries unwritten source bytes
            // forward when `dst_bytesize` caps output. The progression
            // matches the spec's "uses the destination byte offset" test:
            // dst stays "aa", then becomes "aab", then "aabbb".
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dest = "aa"
              s1 = ec.primitive_convert("b", dest, nil, 0); after1 = dest.dup
              s2 = ec.primitive_convert("b", dest, nil, 1); after2 = dest.dup
              s3 = ec.primitive_convert("b", dest, nil, 2); after3 = dest.dup
              [s1, after1, s2, after2, s3, after3]
            "#,
            // A character unrepresentable in the destination yields
            // `:undefined_conversion` and bytes after the offending
            // codepoint stay in `src`.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              s = "\u{9878}abcd"
              dst = ""
              result = ec.primitive_convert(s, dst)
              [result, s]
            "#,
            // Even when the call returns `:undefined_conversion`, dst is
            // re-tagged with the destination encoding (CRuby contract).
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              dst = "".force_encoding("UTF-8")
              ec.primitive_convert("\u{9878}", dst)
              dst.encoding == Encoding::ISO_8859_1
            "#,
            // Stray UTF-8 continuation bytes trigger
            // `:invalid_byte_sequence` and the bytes after the malformed
            // run stay in `src`.
            r#"
              ec = Encoding::Converter.new(Encoding::UTF_8, Encoding::UTF_8_MAC)
              s = "\xC3\xA1\x80\x80\xC3\xA1".force_encoding("UTF-8")
              dest = "".force_encoding("UTF-8")
              result = ec.primitive_convert(s, dest)
              [result, s.bytes]
            "#,
            // Repeated calls peel one malformed byte off the front each
            // call until `src` is empty.
            r#"
              ec = Encoding::Converter.new(Encoding::UTF_8, Encoding::UTF_8_MAC)
              s = "\x80\x80\x80"
              dest = "".force_encoding(Encoding::UTF_8_MAC)
              ec.primitive_convert(s, dest); a = s.bytes
              ec.primitive_convert(s, dest); b = s.bytes
              ec.primitive_convert(s, dest); c = s.bytes
              [a, b, c]
            "#,
            // EUC-JP `\xa4` is an incomplete lead byte; with
            // `partial_input: true` the call returns `:source_buffer_empty`
            // (the decoder is willing to wait for more input).
            r#"
              ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
              s = "\xa4"
              result = ec.primitive_convert(s, "", nil, nil, partial_input: true)
              [result, s]
            "#,
            // Same lead byte without `partial_input` yields
            // `:incomplete_input` (the decoder was told this is the
            // final input).
            r#"
              ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
              s = "\xa4"
              result = ec.primitive_convert(s, "", nil, nil, partial_input: false)
              [result, s]
            "#,
            // `after_output:` is recognised (read off the opts hash) and
            // does not raise even though monoruby doesn't suspend on it.
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("", "", nil, nil, after_output: true)
            "#,
            // `:destination_buffer_full` clears `src` (the leftover goes
            // into the converter's pending buffer, not the user's `src`).
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-2022-JP")
              s = "\u{9999}"
              destination_bytesize = s.bytesize - 1
              result = ec.primitive_convert(s, "", 0, destination_bytesize)
              [result, s]
            "#,
        ]);
    }

    #[test]
    fn primitive_errinfo_reflects_last_primitive_convert() {
        // CRuby's `primitive_errinfo` for a non-error result
        // (`:source_buffer_empty` / `:finished` /
        // `:destination_buffer_full`) is `[state, nil, nil, nil,
        // nil]` — only the state symbol; the four trailing fields
        // are nil. (Verified byte-exact vs `LANG=C.UTF-8 ruby`.)
        run_test_no_result_check(
            r#"
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              ec.primitive_convert("hello", "")
              info = ec.primitive_errinfo
              raise unless info == [:finished, nil, nil, nil, nil]
            "#,
        );
    }

    // ----- E1: Encoding.compatible? matrix -----

    #[test]
    fn compatible_group_1() {
        run_tests(&[
            // Two strings same encoding → that encoding.
            r#"Encoding.compatible?("abc", "def").to_s"#,
            // ASCII-only UTF-8 vs ASCII-only US-ASCII → US-ASCII (right's
            // when right has US-ASCII, both 7-bit).
            r#"
              a = "abc".encode("us-ascii")
              b = "def".encode("us-ascii")
              Encoding.compatible?(a, b).to_s
            "#,
            // UTF-8 7-bit + Shift_JIS 7-bit → first wins (UTF-8).
            r#"
              a = "abc".dup.force_encoding("UTF-8")
              b = "def".dup.force_encoding("Shift_JIS")
              Encoding.compatible?(a, b).to_s
            "#,
            // Both empty: left wins unconditionally.
            r#"
              a = "".dup.force_encoding("UTF-8")
              b = "".dup.force_encoding("US-ASCII")
              Encoding.compatible?(a, b).to_s
            "#,
            r#"
              a = "".dup.force_encoding("UTF-16BE")
              b = "".dup.force_encoding("US-ASCII")
              Encoding.compatible?(a, b).to_s
            "#,
            // One empty, other 7-bit ASCII: left wins.
            r#"
              a = "".dup.force_encoding("UTF-8")
              b = "abc".dup.force_encoding("US-ASCII")
              Encoding.compatible?(a, b).to_s
            "#,
            // Empty + non-ASCII content: non-empty side's encoding wins.
            r#"
              a = "".dup.force_encoding("UTF-8")
              b = "café"
              Encoding.compatible?(a, b).to_s
            "#,
            // Encoding × Encoding pair: same encoding round-trips.
            r#"Encoding.compatible?(Encoding::UTF_8, Encoding::UTF_8).to_s"#,
            // Encoding × Encoding: second is US-ASCII → first wins.
            r#"Encoding.compatible?(Encoding::UTF_8, Encoding::US_ASCII).to_s"#,
            // Encoding × Encoding: two distinct non-US-ASCII → nil.
            r#"Encoding.compatible?(Encoding::UTF_8, Encoding::EUC_JP).inspect"#,
            // Non-encoding-aware values yield nil per spec.
            r#"Encoding.compatible?(Object.new, "abc").inspect"#,
            r#"Encoding.compatible?(nil, nil).inspect"#,
            r#"Encoding.compatible?("abc", Object.new).inspect"#,
            r#"Encoding.compatible?(:sym, Object.new).inspect"#,
            // Symbol pairs work like Strings of the symbol's name.
            r#"Encoding.compatible?(:abc, :def).to_s"#,
            // Regexp pairs use the declared encoding of the source.
            r#"Encoding.compatible?(/abc/, /def/).to_s"#,
            r#"Encoding.compatible?("abc", /def/).to_s"#,
        ]);
    }

    #[test]
    fn other_dummy_encodings_name_preserved() {
        // UTF-7 / CP50220 / CP50221: name-preserved, ASCII-
        // incompatible — `#inspect` escapes every byte, symbols are
        // quoted, and the encoding round-trips through force_encoding.
        run_tests(&[
            r#""abcd".dup.force_encoding("UTF-7").encoding.name"#,
            r#""abcd".dup.force_encoding("UTF-7").inspect"#,
            r#""abcd".dup.force_encoding("CP50220").inspect"#,
            r#""abcd".dup.force_encoding("CP50221").encoding.name"#,
            r#""a\"b".dup.force_encoding("UTF-7").inspect"#,
            r#"Encoding.find("UTF-7").name"#,
            r#"Encoding.find("CP50220").name"#,
            r#""abc".dup.force_encoding("UTF-7").to_sym.inspect"#,
            r#""abc".dup.force_encoding("UTF-7").to_sym.encoding.name"#,
            // ascii_compatible? false => Encoding.compatible? rules
            r#"Encoding.compatible?(Encoding::UTF_7, Encoding::UTF_7) == Encoding::UTF_7"#,
            r#"Encoding.compatible?("x".dup.force_encoding("UTF-7"), "y").inspect"#,
            // Emacs-Mule / CESU-8 stay ASCII-compatible (unchanged)
            r#""abcd".dup.force_encoding("Emacs-Mule").inspect"#,
            r#""abcd".dup.force_encoding("CESU-8").inspect"#,
        ]);
    }

    #[test]
    fn dummy_encoding_classification() {
        run_tests(&[
            // CRuby dummy set: dummy => not ASCII-compatible; the
            // ASCII-only-content `to_sym` is byte-escaped + quoted.
            r#"Encoding.list.select(&:dummy?).all? { |e| !e.ascii_compatible? }"#,
            r#"Encoding.list.select(&:dummy?).map { |e|
                 "abcd".dup.force_encoding(e).to_sym.inspect
               }.uniq"#,
            // Emacs-Mule / CESU-8 / stateless-ISO-2022-JP are NOT dummy
            // and ARE ASCII-compatible (CRuby).
            r#"[Encoding::CESU_8.dummy?, Encoding::Emacs_Mule.dummy?,
                Encoding::STATELESS_ISO_2022_JP.dummy?]"#,
            r#"[Encoding::CESU_8.ascii_compatible?,
                Encoding::Emacs_Mule.ascii_compatible?,
                Encoding::STATELESS_ISO_2022_JP.ascii_compatible?]"#,
            r#""abcd".dup.force_encoding("Emacs-Mule").to_sym.inspect"#,
            r#""abcd".dup.force_encoding("CESU-8").to_sym.inspect"#,
            // bare UTF-16/UTF-32 are dummy (distinct from the LE codecs)
            r#"[Encoding::UTF_16.dummy?, Encoding::UTF_16LE.dummy?]"#,
            r#""abcd".dup.force_encoding("UTF-16").to_sym.inspect"#,
            r#""abcd".dup.force_encoding("UTF-32").to_sym.inspect"#,
            r#"Encoding.find("UTF-16").name"#,
            r#""x".encode("UTF-16LE").bytes.size"#,
        ]);
    }

    #[test]
    fn mac_family_tables_match_cruby() {
        // The eight Mac OS script encodings CRuby ships a converter
        // for, pinned cell by cell: each table was read off CRuby byte
        // by byte, and this digest changes if any one of the 128 cells
        // moves (#1471).
        run_test_once(
            r#"
              digest = lambda do |cps|
                cps.each_with_index.reduce(0) { |a, (c, i)| (a * 131 + (c + 2) * (i + 1)) % 1_000_000_007 }
              end
              %w[macRoman macCyrillic macCroatian macGreek macIceland macRomania macTurkish macUkraine].map do |n|
                cps = (0x80..0xff).map { |b| ([b].pack("C").force_encoding(n).encode("UTF-8").ord rescue -1) }
                [n, digest.call(cps)]
              end
            "#,
        );
        // The readable half of the same pin: é and ß are in every one
        // of them, Ω only in macGreek, and the Apple-logo cell
        // (`0xF0`, `0xD8` in macCroatian) is assigned no character at
        // all — an undefined conversion, not an invalid byte.
        run_test_once(
            r#"
              [
                %w[é Ω ß].map { |c| %w[macRoman macGreek macTurkish macCroatian].map { |n| (c.encode(n).bytes rescue $!.class.name.sub("Encoding::", "")) } },
                %w[macRoman macGreek macTurkish macCroatian macIceland macRomania].map { |n| (["\xf0".b.force_encoding(n).encode("UTF-8").bytes] rescue $!.class.name.sub("Encoding::", "")) },
                (["\xd8".b.force_encoding("macCroatian").encode("UTF-8").bytes] rescue $!.class.name.sub("Encoding::", "")),
              ]
            "#,
        );
    }

    #[test]
    fn mac_family_and_cp949_keep_their_own_names() {
        // Each of these used to answer some other encoding's name:
        // `CP949` was stored as `EUC-KR`, `MacJapanese` as
        // `Windows-31J`, and the rest of the Mac family as
        // `ASCII-8BIT`. CRuby spells the family with a lowercase
        // `mac`, `MacJapanese` excepted (#1471).
        run_test_once(
            r#"
              names = %w[macRoman macCyrillic macCentEuro macCroatian macGreek macIceland
                         macRomania macThai macTurkish macUkraine MacJapanese CP949]
              [
                names.map { |n| e = Encoding.find(n); [e.name, e.names, e.dummy?, e.ascii_compatible?] },
                names.map { |n| "ab".encode(n).encoding.name },
                [Encoding::MacRoman.name, Encoding::MacCentEuro.name, Encoding::MacJapanese.name],
                [Encoding::CP949 == Encoding::EUC_KR, Encoding::CP949.name, Encoding::EUC_KR.name],
                (begin; "ab".dup.force_encoding("UHC"); rescue => e; e.class.name; end),
              ]
            "#,
        );
    }

    #[test]
    fn encodings_with_no_converter_refuse_to_convert() {
        // CRuby ships no converter for `macCentEuro`, `macThai` or
        // `MacJapanese`, so they carry 7-bit text and nothing else —
        // and none at all for UTF-7, which refuses even `"ab"` where
        // every other dummy encoding copies it through (#1471).
        run_test_once(
            r#"
              names = %w[macCentEuro macThai MacJapanese UTF-7]
              [
                names.map { |n| ("ab".encode(n).bytes rescue $!.class.name.sub("Encoding::", "")) },
                names.map { |n| ("あ".encode(n).bytes rescue $!.class.name.sub("Encoding::", "")) },
                names.map { |n| (Encoding::Converter.new("UTF-8", n) && "ok" rescue $!.class.name.sub("Encoding::", "")) },
                ("ab".dup.force_encoding("UTF-7").encode("UTF-8") rescue $!.class.name.sub("Encoding::", "")),
              ]
            "#,
        );
    }

    #[test]
    fn encoding_list_completeness() {
        run_tests(&[
            // `Encoding.list` is a non-trivial Array of Encoding
            // instances, each listed once.
            r#"Encoding.list.class.name"#,
            r#"Encoding.list.all? { |e| e.is_a?(Encoding) }"#,
            r#"Encoding.list.map(&:name) == Encoding.list.map(&:name).uniq"#,
            r#"Encoding.list.include?(Encoding::UTF_8)"#,
            r#"Encoding.list.include?(Encoding::CESU_8)"#,
            r#"Encoding.list.include?(Encoding.default_external)"#,
            // Dummy / non-ASCII-compatible selections are non-empty.
            r#"Encoding.list.any?(&:dummy?)"#,
            r#"Encoding.list.reject(&:ascii_compatible?).any?"#,
            // `Encoding.find(e.name)` round-trips for every encoding.
            r#"Encoding.list.all? { |e| Encoding.find(e.name).equal?(e) }"#,
            r#"Encoding.find("Big5-HKSCS").name"#,
            r#"Encoding.find("UTF-8-MAC").equal?(Encoding::UTF8_MAC)"#,
            r#"Encoding::UTF_8_MAC.equal?(Encoding::UTF8_MAC)"#,
            // `name_list` is a superset of every listed encoding name.
            r#"Encoding.list.all? { |e| Encoding.name_list.include?(e.name) }"#,
        ]);
    }

    /// Emacs-Mule has no codec, but it does have a shape, and CRuby
    /// reports a byte that does not fit it as broken — which is what
    /// gives `#scrub` and `#encode(invalid: :replace)` something to
    /// replace (#1424).
    #[test]
    fn emacs_mule_validity() {
        let mut v: Vec<String> = vec![];
        for seq in [
            // The lead byte fixes the width and the range its second
            // byte must fall in.
            "0x80",                        // leads nothing
            "0x81, 0xA0",                  // 2 bytes
            "0x81, 0x20",                  // …with a second byte that is not one
            "0x81",                        // …truncated
            "0x90, 0xA0, 0xA0",            // 3 bytes
            "0x90, 0xA0",                  // …truncated: one subpart, not two
            "0x90, 0xA0, 0x20, 0xA0",      // …and the 0x20 survives
            "0x9A, 0xE0, 0xA0",            // a private charset
            "0x9A, 0xA0, 0xA0",            // …whose id is out of range
            "0x9C, 0xF0, 0xA0, 0xA0",      // 4 bytes
            "0x9C, 0xFF, 0xA0, 0xA0",
            "0x9D, 0xF5, 0xA0, 0xA0",
            "0x9D, 0xF0, 0xA0, 0xA0",      // 0x9C's range, not 0x9D's
            "0x9E, 0xA0",                  // leads nothing either
            "0x61, 0xFF, 0x62",
            "0x61, 0x90, 0xA0, 0xA0, 0x62",
        ] {
            let build = format!(r#"s = [{seq}].pack("C*").force_encoding("Emacs-Mule")"#);
            for read in [
                // A newline decorator is the whole conversion when the
                // encodings match: no transcoder runs, so `invalid:`
                // has nothing to act in and the bytes pass through.
                "s.encode(invalid: :replace, universal_newline: true).bytes",
                "s.encode(invalid: :replace, crlf_newline: true).bytes",
                "s.valid_encoding?",
                "s.length",
                "s.chars.map(&:bytes)",
                "s.scrub.bytes",
                r#"s.scrub("!").bytes"#,
                "s.encode(invalid: :replace).bytes",
                r#"s.encode(invalid: :replace, replace: "!").bytes"#,
                "s.reverse.bytes",
            ] {
                v.push(format!("{build}; {read}"));
            }
        }
        let refs: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
        run_tests(&refs);
    }

    /// The whole-buffer scan that lets `jp_encode` skip the
    /// per-character walk. The byte ranges it looks for are also
    /// *trailing* bytes, so it has to walk rather than scan — which is
    /// the part worth testing directly, since `encoding_rs`'s own
    /// output never contains the three-byte form.
    #[test]
    fn eucjp_row_scan() {
        use crate::value::Encoding as E;
        let live =
            |bytes: &[u8]| super::jp_live_throughout(super::jp_fixup(E::EucJp).unwrap(), bytes);
        // Plain ASCII, JIS X 0208, half-width katakana, JIS X 0212.
        assert!(live(b"abc"));
        assert!(live(&[0xa6, 0xd0]));
        assert!(live(&[0x8e, 0xb1]));
        assert!(live(&[0x8f, 0xab, 0xe4]));
        assert!(live(&[0x61, 0x8f, 0xab, 0xe4, 0x62]));
        // An extension row in lead position.
        assert!(!live(&[0xf9, 0xa1]));
        assert!(!live(&[0xad, 0xe2]));
        assert!(!live(&[0xa6, 0xd0, 0xf9, 0xa1]));
        // …and the same bytes as *trailing* bytes, which are fine.
        assert!(live(&[0xa1, 0xf9]));
        assert!(live(&[0x8f, 0xf9, 0xad]));
        // Shift_JIS has its own dead rows, and Windows-31J has none.
        let sjis_live =
            |bytes: &[u8]| super::jp_live_throughout(super::jp_fixup(E::Sjis(0)).unwrap(), bytes);
        assert!(sjis_live(&[0x82, 0xa0]));
        assert!(!sjis_live(&[0x87, 0x40]));
        assert!(!sjis_live(&[0xed, 0x40]));
        assert!(!sjis_live(&[0xfa, 0x40]));
        // `87` / `ED` / `FA` as *trailing* bytes are ordinary.
        assert!(sjis_live(&[0x82, 0x87]));
        assert!(sjis_live(&[0x82, 0xed]));
        assert!(super::jp_live_throughout(
            super::jp_fixup(E::Sjis(1)).unwrap(),
            &[0x87, 0x40]
        ));
    }

    /// The duplicate mappings and the extension rows (#1445).
    ///
    /// `encoding_rs`'s EUC-JP and Shift_JIS are WHATWG's, which
    /// disagree with CRuby's on seven cells each — the same glyph with
    /// two Unicode homes — and carry NEC/IBM extension rows CRuby's
    /// tables do not have. `Windows-31J` is the encoding that really
    /// has both, and must not move.
    #[test]
    fn jp_duplicate_mappings_and_extension_rows() {
        run_test_once(
            r#"
            res = []
            dec = ->(enc, bs) {
              bs.pack("C*").force_encoding(enc).encode("UTF-8").codepoints.map { |c| "U+%04X" % c } rescue $!.class.to_s.split("::").last
            }
            enc = ->(e, h) {
              [h.to_i(16)].pack("U").encode(e).bytes.map { |b| "%02X" % b }.join rescue $!.class.to_s.split("::").last
            }
            pairs = %w[2014 2015 301C FF5E 2016 2225 2212 FF0D 00A2 FFE0 00A3 FFE1 00AC FFE2]
            # EUC-JP reads CRuby's half of each pair …
            res << [[0xA1,0xBD],[0xA1,0xC1],[0xA1,0xC2],[0xA1,0xDD],
                    [0xA1,0xF1],[0xA1,0xF2],[0xA2,0xCC],[0x8F,0xA2,0xB7]].map { |bs| dec.("EUC-JP", bs) }
            # … and writes many-to-one, refusing the other half.
            res << pairs.map { |h| enc.("EUC-JP", h) }
            # Shift_JIS has the same pairs in its own cells.
            res << [[0x81,0x5C],[0x81,0x60],[0x81,0x61],[0x81,0x7C],
                    [0x81,0x91],[0x81,0x92],[0x81,0xCA]].map { |bs| dec.("Shift_JIS", bs) }
            res << pairs.map { |h| enc.("Shift_JIS", h) }
            # Windows-31J keeps the fullwidth readings and the rows.
            res << [[0x81,0x5C],[0x81,0x60],[0x87,0x40],[0xED,0x40],[0xFA,0x40]].map { |bs| dec.("Windows-31J", bs) }
            res << %w[2014 2015 FF5E 2212 FF0D].map { |h| enc.("Windows-31J", h) }
            # An extension row is well-formed bytes with no character:
            # an undefined conversion, which `invalid: :replace` does
            # not cover and `undef: :replace` does.
            s = [0xF9, 0xA1].pack("C*").force_encoding("EUC-JP")
            res << s.valid_encoding?
            res << (begin; s.encode("UTF-8"); rescue => e; [e.class.to_s.split("::").last, e.message]; end)
            res << s.encode("UTF-8", undef: :replace).codepoints.map { |c| "U+%04X" % c }
            res << s.encode("UTF-8", undef: :replace, replace: "?")
            res << (begin; s.encode("UTF-8", invalid: :replace); rescue => e; e.class.to_s.split("::").last; end)
            t = [0x87, 0x40].pack("C*").force_encoding("Shift_JIS")
            res << t.valid_encoding?
            res << (begin; t.encode("UTF-8"); rescue => e; e.class.to_s.split("::").last; end)
            res << t.encode("UTF-8", undef: :replace, replace: "?")
            # JIS X 0212 is still reachable (#1424 is unchanged).
            res << "\u00FC".encode("EUC-JP").bytes
            # A corrected cell with ordinary text on either side: the
            # runs around it still go through `encoding_rs`, which is
            # the whole point of correcting it rather than replacing
            # the codec.
            res << [0x41, 0xA1, 0xBD, 0x42].pack("C*").force_encoding("EUC-JP")
                     .encode("UTF-8").codepoints.map { |c| "U+%04X" % c }
            res << [0xA6, 0xD0, 0xA1, 0xC1, 0xA6, 0xD0].pack("C*").force_encoding("EUC-JP")
                     .encode("UTF-8").codepoints.map { |c| "U+%04X" % c }
            res << [0x82, 0xA0, 0x81, 0x5C, 0x82, 0xA2].pack("C*").force_encoding("Shift_JIS")
                     .encode("UTF-8").codepoints.map { |c| "U+%04X" % c }
            res << "日本語—テスト".encode("EUC-JP").encode("UTF-8")
            res << "日本語—テスト".encode("Shift_JIS").encode("UTF-8")
            # …and with an ill-formed byte in the same string, which
            # `encoding_rs` still gets to group its own way.
            res << [0x41, 0x80, 0xA1, 0xBD].pack("C*").force_encoding("EUC-JP")
                     .encode("UTF-8", invalid: :replace).codepoints.map { |c| "U+%04X" % c }
            res << [0xA1, 0xBD, 0x80, 0x41].pack("C*").force_encoding("EUC-JP")
                     .encode("UTF-8", invalid: :replace).codepoints.map { |c| "U+%04X" % c }
            # The undefined-conversion message names the pivot when the
            # destination is not UTF-8, and BINARY takes its own path.
            u = [0xF9, 0xA1].pack("C*").force_encoding("EUC-JP")
            res << %w[ISO-8859-1 UTF-16LE US-ASCII IBM437].map { |d|
              begin; u.encode(d); rescue => e; [e.class.to_s.split("::").last, e.message]; end
            }
            res << (begin; u.encode("ASCII-8BIT"); rescue => e; e.class.to_s.split("::").last; end)
            res
            "#,
        );
    }

    /// EUC-JP's second plane (#1424). `encoding_rs`'s encoder is
    /// WHATWG's — JIS X 0208 only, plus NEC/IBM extension rows CRuby's
    /// EUC-JP does not have — so characters that live in JIS X 0212
    /// had no EUC-JP form at all.
    #[test]
    fn eucjp_jisx0212_plane() {
        run_tests(&[
            // The issue's own case: π is in 0208, ü and é are in 0212.
            r#""ü".encode("EUC-JP").bytes"#,
            r#""é".encode("EUC-JP").bytes"#,
            r#""π".encode("EUC-JP").bytes"#,
            r#""あ".encode("euc-jp", "ibm437").bytes"#,
            r#""aπüé漢".encode("EUC-JP").bytes"#,
            // …and the round trip.
            r#""ü".encode("EUC-JP").encode("UTF-8")"#,
            r#"[0x8F, 0xAB, 0xE4].pack("C*").force_encoding("EUC-JP").encode("UTF-8")"#,
            // A character CRuby's EUC-JP does not have is still
            // undefined, and the error names *it* rather than the
            // first non-ASCII character in the string.
            r#"begin; "aπ€".encode("EUC-JP"); rescue => e; [e.class.to_s, e.message]; end"#,
            r#""aπ€".encode("EUC-JP", undef: :replace).bytes"#,
            // U+FF02 is only in the NEC/IBM rows `encoding_rs` carries
            // and CRuby's EUC-JP does not, so it has no form here
            // either; U+4E28 is in those rows *and* in 0212, and takes
            // the 0212 form.
            r#"begin; "＂".encode("EUC-JP"); rescue => e; e.class.to_s; end"#,
            r#""丨".encode("EUC-JP").bytes"#,
            r#""纊".encode("EUC-JP").bytes"#,
        ]);
    }

    /// The strings CRuby *names* things with are built out of US-ASCII
    /// when their content is ASCII, which nearly all of them are. The
    /// text is identical either way — the tag only shows when the name
    /// meets another string, and then it decides: a US-ASCII name
    /// yields, where a UTF-8 one wins over a US-ASCII receiver (#1476).
    #[test]
    fn a_name_is_us_ascii_when_its_bytes_are() {
        run_test_once(
            r##"
            def e(x) = [x, x.encoding.name]
            [
              e(Encoding::UTF_8.name), e(Encoding::UTF_8.to_s), e(Encoding::UTF_8.inspect),
              e(Encoding::UTF_8.names.first), e(Encoding::ASCII_8BIT.name),
              e(Encoding::ASCII_8BIT.inspect), e(Encoding::EUC_JP.name),
              e(String.to_s), e(String.name), e(String.inspect), e(Comparable.to_s),
              e(String.singleton_class.to_s), e(Encoding::UTF_8.class.name),
              e(Rational(1, 2).to_s), e(Rational(1, 2).inspect),
              e(Complex(1, 2).to_s), e(Complex(1, 2).inspect),
              e(true.to_s), e(false.to_s), e(nil.to_s),
              e(true.inspect), e(false.inspect), e(nil.inspect),
              e((1..2).to_s), e(/ab/.to_s), e(/ab/.source),
              e(Exception.new.message), e(Exception.new.to_s), e(ArgumentError.new.message),
              e(Encoding::CompatibilityError.new.message),
              ("x".encode("EUC-JP") + String.name).encoding.name,
              (String.name + "x".encode("EUC-JP")).encoding.name,
              ("".b << Encoding::UTF_8.name).encoding.name,
              # …and a name that is not ASCII keeps its own encoding.
              (class Aあ; end
               [Aあ.name, Aあ.name.encoding.name,
                Aあ.to_s.encoding.name, Aあ.inspect.encoding.name]),
            ]
            "##,
        );
    }
}
