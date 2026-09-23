use super::encoding_carrier::{
    carrier_utf8_form,
    CarrierPair, SjisCarrier, Utf8Carrier, carrier_base, carrier_pair, carrier_route,
    carrier_vendor, sjis_carrier, utf8_carrier,
};
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

/// `v` is one of the `Encoding` singletons. Compared by the real class:
/// an Encoding can be given a singleton class, and `v.class()` would
/// then be that.
pub(super) fn is_encoding_object(globals: &Globals, v: Value) -> bool {
    !v.is_packed_value() && v.real_class(&globals.store).id() == encoding_class(globals)
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
        // CRuby's canonical name for code page 850 is `CP850`, with
        // `IBM850` as the alias — the other way round from the rest of
        // the family — and the stateless ISO-2022-JP variant is
        // lower-case (#1520).
        "IBM850" => "CP850",
        "STATELESS_ISO_2022_JP" => "stateless-ISO-2022-JP",
        "EUC_JIS_2004" => "EUC-JIS-2004",
        "STATELESS_ISO_2022_JP_KDDI" => "stateless-ISO-2022-JP-KDDI",
        // The constant is `Encoding::EBCDIC_CP_US`, the name `IBM037`
        // (#1555).
        "EBCDIC_CP_US" => "IBM037",
        "Windows_874" => "Windows-874",
        // The carrier sets, whose names carry the vendor's own
        // capitalisation (#1573).
        "SJIS_DOCOMO" => "SJIS-DoCoMo",
        "SJIS_KDDI" => "SJIS-KDDI",
        "SJIS_SOFTBANK" => "SJIS-SoftBank",
        "UTF8_DOCOMO" => "UTF8-DoCoMo",
        "UTF8_KDDI" => "UTF8-KDDI",
        "UTF8_SOFTBANK" => "UTF8-SoftBank",
        "ISO_2022_JP_2" => "ISO-2022-JP-2",
        "ISO_2022_JP_KDDI" => "ISO-2022-JP-KDDI",
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
        // Registered in #1555: the Arabic / Thai DOS pages, the ISO-646
        // Chinese variant, the two DOS pages CRuby keeps apart from
        // their IBM namesakes, the Big5 variants, EBCDIC, and the two
        // stateful ISO-2022-JP variants.
        "IBM720",
        "Windows_874",
        "GB1988",
        "CP852",
        "CP855",
        "CP950",
        "CP951",
        "EBCDIC_CP_US",
        "ISO_2022_JP_2",
        "ISO_2022_JP_KDDI",
        "MacJapanese",
        "EUCJP_MS",
        "CP51932",
        "STATELESS_ISO_2022_JP",
        // The last two of the EUC-JP family, which needed the variant
        // index to keep a name of their own (#1562).
        "EUC_JIS_2004",
        "STATELESS_ISO_2022_JP_KDDI",
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
        // `CESU-8`: UTF-8 with the supplementary planes spelled as
        // surrogate pairs. It has a byte walk and a codec of its own
        // (#1562).
        "CESU_8",
        // The six carrier-emoji sets: their bases' bytes throughout,
        // with a block of characters spelled as one Japanese
        // carrier's emoji (#1573).
        "SJIS_DOCOMO",
        "SJIS_KDDI",
        "SJIS_SOFTBANK",
        "UTF8_DOCOMO",
        "UTF8_KDDI",
        "UTF8_SOFTBANK",
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
    // Marshal: an Encoding dumps as its name ('u' payload) and loads by
    // looking that name up again — `_load` itself hands the string
    // back, and Marshal (`finish_user_marshal`) resolves it, as CRuby's
    // compat loader does.
    globals.define_builtin_func_with(enc.id(), "_dump", enc_dump, 0, 1, false);
    globals.define_builtin_class_func(enc.id(), "_load", enc_load, 1);
    // Every Encoding is a frozen singleton: there is no allocator, so
    // `allocate`, `dup` and `clone` are TypeErrors (Kernel's copy
    // methods refuse an object whose class has none) and `new` is not
    // a method at all.
    globals.store[enc.id()].clear_alloc_func();
    let meta = globals.store.get_metaclass(enc.id()).id();
    globals
        .store
        .add_empty_method(meta, IdentId::NEW, Visibility::Undefined);
    globals.set_constant_by_str(enc.id(), "UNICODE_VERSION", frozen_usascii("17.0.0"));
    // Freeze the objects and the names they hand out (`name` / `to_s`
    // answer the `_ENCODING` string itself), now that every constant
    // is registered.
    for cname in globals.store.get_constant_names(enc.id()) {
        if let Some(mut v) = globals.store.get_constant_noautoload(enc.id(), cname)
            && v.class() == enc.id()
            && !v.is_frozen()
        {
            for ivar in [IdentId::_NAME, IdentId::_ENCODING] {
                if let Some(mut s) = globals.store.get_ivar(v, ivar) {
                    s.set_frozen();
                }
            }
            // CRuby's objects carry their name as `@name`, and it is
            // the one instance variable they show.
            if let Some(name) = globals.store.get_ivar(v, IdentId::_ENCODING) {
                globals.store.set_ivar(v, IdentId::get_id("@name"), name).unwrap();
            }
            v.set_frozen();
        }
    }
}

/// A frozen US-ASCII string, which is what every name an Encoding
/// hands out is.
fn frozen_usascii(s: &str) -> Value {
    let mut v = Value::string_usascii_from_str(s);
    v.set_frozen();
    v
}

///
/// ### Encoding#_dump
/// - _dump(limit = nil) -> String
///
/// The encoding's name; Marshal resolves it back to the singleton.
#[monoruby_builtin]
fn enc_dump(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(globals
        .store
        .get_ivar(lfp.self_val(), IdentId::_ENCODING)
        .unwrap_or_else(|| frozen_usascii("UTF-8")))
}

///
/// ### Encoding._load
/// - _load(str) -> str
///
/// Answers its argument unchanged: it is Marshal that turns the name
/// into the encoding (`finish_user_marshal`), as CRuby's compat loader
/// does after `_load`.
#[monoruby_builtin]
fn enc_load(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.arg(0))
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

/// ASCII-8BIT read *as a source*: its 7-bit half is ASCII and every
/// byte above it stands for no character at all, which is the shape
/// of a single-byte table with an empty high half. Kept apart from
/// [`single_byte_table`], which also answers for destinations and for
/// the ctype tables, where BINARY is not a character encoding at all
/// (#1596).
fn source_byte_table(
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
pub(super) fn single_byte_table(enc: crate::value::Encoding) -> Option<&'static [Option<char>; 128]> {
    /// CP850 (DOS Latin-1), IBM737 (DOS Greek) and IBM775 (DOS
    /// Baltic): CRuby's single-byte tables, which `encoding_rs` has no
    /// codec for (#1520). Read off CRuby 4.0.6, every cell assigned.
    const CP850: [Option<char>; 128] = [
        Some('\u{C7}'), Some('\u{FC}'), Some('\u{E9}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{E0}'), Some('\u{E5}'), Some('\u{E7}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{E8}'), Some('\u{EF}'), Some('\u{EE}'), Some('\u{EC}'), Some('\u{C4}'), Some('\u{C5}'),
        Some('\u{C9}'), Some('\u{E6}'), Some('\u{C6}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{F2}'), Some('\u{FB}'), Some('\u{F9}'),
        Some('\u{FF}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{F8}'), Some('\u{A3}'), Some('\u{D8}'), Some('\u{D7}'), Some('\u{192}'),
        Some('\u{E1}'), Some('\u{ED}'), Some('\u{F3}'), Some('\u{FA}'), Some('\u{F1}'), Some('\u{D1}'), Some('\u{AA}'), Some('\u{BA}'),
        Some('\u{BF}'), Some('\u{AE}'), Some('\u{AC}'), Some('\u{BD}'), Some('\u{BC}'), Some('\u{A1}'), Some('\u{AB}'), Some('\u{BB}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{C1}'), Some('\u{C2}'), Some('\u{C0}'),
        Some('\u{A9}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{A2}'), Some('\u{A5}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{E3}'), Some('\u{C3}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{A4}'),
        Some('\u{F0}'), Some('\u{D0}'), Some('\u{CA}'), Some('\u{CB}'), Some('\u{C8}'), Some('\u{131}'), Some('\u{CD}'), Some('\u{CE}'),
        Some('\u{CF}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{A6}'), Some('\u{CC}'), Some('\u{2580}'),
        Some('\u{D3}'), Some('\u{DF}'), Some('\u{D4}'), Some('\u{D2}'), Some('\u{F5}'), Some('\u{D5}'), Some('\u{B5}'), Some('\u{FE}'),
        Some('\u{DE}'), Some('\u{DA}'), Some('\u{DB}'), Some('\u{D9}'), Some('\u{FD}'), Some('\u{DD}'), Some('\u{AF}'), Some('\u{B4}'),
        Some('\u{AD}'), Some('\u{B1}'), Some('\u{2017}'), Some('\u{BE}'), Some('\u{B6}'), Some('\u{A7}'), Some('\u{F7}'), Some('\u{B8}'),
        Some('\u{B0}'), Some('\u{A8}'), Some('\u{B7}'), Some('\u{B9}'), Some('\u{B3}'), Some('\u{B2}'), Some('\u{25A0}'), Some('\u{A0}'),
    ];
    const IBM737: [Option<char>; 128] = [
        Some('\u{391}'), Some('\u{392}'), Some('\u{393}'), Some('\u{394}'), Some('\u{395}'), Some('\u{396}'), Some('\u{397}'), Some('\u{398}'),
        Some('\u{399}'), Some('\u{39A}'), Some('\u{39B}'), Some('\u{39C}'), Some('\u{39D}'), Some('\u{39E}'), Some('\u{39F}'), Some('\u{3A0}'),
        Some('\u{3A1}'), Some('\u{3A3}'), Some('\u{3A4}'), Some('\u{3A5}'), Some('\u{3A6}'), Some('\u{3A7}'), Some('\u{3A8}'), Some('\u{3A9}'),
        Some('\u{3B1}'), Some('\u{3B2}'), Some('\u{3B3}'), Some('\u{3B4}'), Some('\u{3B5}'), Some('\u{3B6}'), Some('\u{3B7}'), Some('\u{3B8}'),
        Some('\u{3B9}'), Some('\u{3BA}'), Some('\u{3BB}'), Some('\u{3BC}'), Some('\u{3BD}'), Some('\u{3BE}'), Some('\u{3BF}'), Some('\u{3C0}'),
        Some('\u{3C1}'), Some('\u{3C3}'), Some('\u{3C2}'), Some('\u{3C4}'), Some('\u{3C5}'), Some('\u{3C6}'), Some('\u{3C7}'), Some('\u{3C8}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{2561}'), Some('\u{2562}'), Some('\u{2556}'),
        Some('\u{2555}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{255C}'), Some('\u{255B}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{255E}'), Some('\u{255F}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{2567}'),
        Some('\u{2568}'), Some('\u{2564}'), Some('\u{2565}'), Some('\u{2559}'), Some('\u{2558}'), Some('\u{2552}'), Some('\u{2553}'), Some('\u{256B}'),
        Some('\u{256A}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{258C}'), Some('\u{2590}'), Some('\u{2580}'),
        Some('\u{3C9}'), Some('\u{3AC}'), Some('\u{3AD}'), Some('\u{3AE}'), Some('\u{3CA}'), Some('\u{3AF}'), Some('\u{3CC}'), Some('\u{3CD}'),
        Some('\u{3CB}'), Some('\u{3CE}'), Some('\u{386}'), Some('\u{388}'), Some('\u{389}'), Some('\u{38A}'), Some('\u{38C}'), Some('\u{38E}'),
        Some('\u{38F}'), Some('\u{B1}'), Some('\u{2265}'), Some('\u{2264}'), Some('\u{3AA}'), Some('\u{3AB}'), Some('\u{F7}'), Some('\u{2248}'),
        Some('\u{B0}'), Some('\u{2219}'), Some('\u{B7}'), Some('\u{221A}'), Some('\u{207F}'), Some('\u{B2}'), Some('\u{25A0}'), Some('\u{A0}'),
    ];
    const IBM775: [Option<char>; 128] = [
        Some('\u{106}'), Some('\u{FC}'), Some('\u{E9}'), Some('\u{101}'), Some('\u{E4}'), Some('\u{123}'), Some('\u{E5}'), Some('\u{107}'),
        Some('\u{142}'), Some('\u{113}'), Some('\u{156}'), Some('\u{157}'), Some('\u{12B}'), Some('\u{179}'), Some('\u{C4}'), Some('\u{C5}'),
        Some('\u{C9}'), Some('\u{E6}'), Some('\u{C6}'), Some('\u{14D}'), Some('\u{F6}'), Some('\u{122}'), Some('\u{A2}'), Some('\u{15A}'),
        Some('\u{15B}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{F8}'), Some('\u{A3}'), Some('\u{D8}'), Some('\u{D7}'), Some('\u{A4}'),
        Some('\u{100}'), Some('\u{12A}'), Some('\u{F3}'), Some('\u{17B}'), Some('\u{17C}'), Some('\u{17A}'), Some('\u{201D}'), Some('\u{A6}'),
        Some('\u{A9}'), Some('\u{AE}'), Some('\u{AC}'), Some('\u{BD}'), Some('\u{BC}'), Some('\u{141}'), Some('\u{AB}'), Some('\u{BB}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{104}'), Some('\u{10C}'), Some('\u{118}'),
        Some('\u{116}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{12E}'), Some('\u{160}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{172}'), Some('\u{16A}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{17D}'),
        Some('\u{105}'), Some('\u{10D}'), Some('\u{119}'), Some('\u{117}'), Some('\u{12F}'), Some('\u{161}'), Some('\u{173}'), Some('\u{16B}'),
        Some('\u{17E}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{258C}'), Some('\u{2590}'), Some('\u{2580}'),
        Some('\u{D3}'), Some('\u{DF}'), Some('\u{14C}'), Some('\u{143}'), Some('\u{F5}'), Some('\u{D5}'), Some('\u{B5}'), Some('\u{144}'),
        Some('\u{136}'), Some('\u{137}'), Some('\u{13B}'), Some('\u{13C}'), Some('\u{146}'), Some('\u{112}'), Some('\u{145}'), Some('\u{2019}'),
        Some('\u{AD}'), Some('\u{B1}'), Some('\u{201C}'), Some('\u{BE}'), Some('\u{B6}'), Some('\u{A7}'), Some('\u{F7}'), Some('\u{201E}'),
        Some('\u{B0}'), Some('\u{2219}'), Some('\u{B7}'), Some('\u{B9}'), Some('\u{B3}'), Some('\u{B2}'), Some('\u{25A0}'), Some('\u{A0}'),
    ];
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
        None, None, Some('\u{E9}'), Some('\u{E2}'), None, Some('\u{E0}'), None, Some('\u{E7}'),
        Some('\u{EA}'), Some('\u{EB}'), Some('\u{E8}'), Some('\u{EF}'), Some('\u{EE}'), None, None, None,
        None, Some('\u{651}'), Some('\u{652}'), Some('\u{F4}'), Some('\u{A4}'), Some('\u{640}'), Some('\u{FB}'), Some('\u{F9}'),
        Some('\u{621}'), Some('\u{622}'), Some('\u{623}'), Some('\u{624}'), Some('\u{A3}'), Some('\u{625}'), Some('\u{626}'), Some('\u{627}'),
        Some('\u{628}'), Some('\u{629}'), Some('\u{62A}'), Some('\u{62B}'), Some('\u{62C}'), Some('\u{62D}'), Some('\u{62E}'), Some('\u{62F}'),
        Some('\u{630}'), Some('\u{631}'), Some('\u{632}'), Some('\u{633}'), Some('\u{634}'), Some('\u{635}'), Some('\u{AB}'), Some('\u{BB}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{2561}'), Some('\u{2562}'), Some('\u{2556}'),
        Some('\u{2555}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{255C}'), Some('\u{255B}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{255E}'), Some('\u{255F}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{2567}'),
        Some('\u{2568}'), Some('\u{2564}'), Some('\u{2565}'), Some('\u{2559}'), Some('\u{2558}'), Some('\u{2552}'), Some('\u{2553}'), Some('\u{256B}'),
        Some('\u{256A}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{258C}'), Some('\u{2590}'), Some('\u{2580}'),
        Some('\u{636}'), Some('\u{637}'), Some('\u{638}'), Some('\u{639}'), Some('\u{63A}'), Some('\u{641}'), Some('\u{B5}'), Some('\u{642}'),
        Some('\u{643}'), Some('\u{644}'), Some('\u{645}'), Some('\u{646}'), Some('\u{647}'), Some('\u{648}'), Some('\u{649}'), Some('\u{64A}'),
        Some('\u{2261}'), Some('\u{64B}'), Some('\u{64C}'), Some('\u{64D}'), Some('\u{64E}'), Some('\u{64F}'), Some('\u{650}'), Some('\u{2248}'),
        Some('\u{B0}'), Some('\u{2219}'), Some('\u{B7}'), Some('\u{221A}'), Some('\u{207F}'), Some('\u{B2}'), Some('\u{25A0}'), Some('\u{A0}'),
    ];
    const CP852: [Option<char>; 128] = [
        Some('\u{C7}'), Some('\u{FC}'), Some('\u{E9}'), Some('\u{E2}'), Some('\u{E4}'), Some('\u{16F}'), Some('\u{107}'), Some('\u{E7}'),
        Some('\u{142}'), Some('\u{EB}'), Some('\u{150}'), Some('\u{151}'), Some('\u{EE}'), Some('\u{179}'), Some('\u{C4}'), Some('\u{106}'),
        Some('\u{C9}'), Some('\u{139}'), Some('\u{13A}'), Some('\u{F4}'), Some('\u{F6}'), Some('\u{13D}'), Some('\u{13E}'), Some('\u{15A}'),
        Some('\u{15B}'), Some('\u{D6}'), Some('\u{DC}'), Some('\u{164}'), Some('\u{165}'), Some('\u{141}'), Some('\u{D7}'), Some('\u{10D}'),
        Some('\u{E1}'), Some('\u{ED}'), Some('\u{F3}'), Some('\u{FA}'), Some('\u{104}'), Some('\u{105}'), Some('\u{17D}'), Some('\u{17E}'),
        Some('\u{118}'), Some('\u{119}'), Some('\u{AC}'), Some('\u{17A}'), Some('\u{10C}'), Some('\u{15F}'), Some('\u{AB}'), Some('\u{BB}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{C1}'), Some('\u{C2}'), Some('\u{11A}'),
        Some('\u{15E}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{17B}'), Some('\u{17C}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{102}'), Some('\u{103}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{A4}'),
        Some('\u{111}'), Some('\u{110}'), Some('\u{10E}'), Some('\u{CB}'), Some('\u{10F}'), Some('\u{147}'), Some('\u{CD}'), Some('\u{CE}'),
        Some('\u{11B}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{162}'), Some('\u{16E}'), Some('\u{2580}'),
        Some('\u{D3}'), Some('\u{DF}'), Some('\u{D4}'), Some('\u{143}'), Some('\u{144}'), Some('\u{148}'), Some('\u{160}'), Some('\u{161}'),
        Some('\u{154}'), Some('\u{DA}'), Some('\u{155}'), Some('\u{170}'), Some('\u{FD}'), Some('\u{DD}'), Some('\u{163}'), Some('\u{B4}'),
        Some('\u{AD}'), Some('\u{2DD}'), Some('\u{2DB}'), Some('\u{2C7}'), Some('\u{2D8}'), Some('\u{A7}'), Some('\u{F7}'), Some('\u{B8}'),
        Some('\u{B0}'), Some('\u{A8}'), Some('\u{2D9}'), Some('\u{171}'), Some('\u{158}'), Some('\u{159}'), Some('\u{25A0}'), Some('\u{A0}'),
    ];
    const CP855: [Option<char>; 128] = [
        Some('\u{452}'), Some('\u{402}'), Some('\u{453}'), Some('\u{403}'), Some('\u{451}'), Some('\u{401}'), Some('\u{454}'), Some('\u{404}'),
        Some('\u{455}'), Some('\u{405}'), Some('\u{456}'), Some('\u{406}'), Some('\u{457}'), Some('\u{407}'), Some('\u{458}'), Some('\u{408}'),
        Some('\u{459}'), Some('\u{409}'), Some('\u{45A}'), Some('\u{40A}'), Some('\u{45B}'), Some('\u{40B}'), Some('\u{45C}'), Some('\u{40C}'),
        Some('\u{45E}'), Some('\u{40E}'), Some('\u{45F}'), Some('\u{40F}'), Some('\u{44E}'), Some('\u{42E}'), Some('\u{44A}'), Some('\u{42A}'),
        Some('\u{430}'), Some('\u{410}'), Some('\u{431}'), Some('\u{411}'), Some('\u{446}'), Some('\u{426}'), Some('\u{434}'), Some('\u{414}'),
        Some('\u{435}'), Some('\u{415}'), Some('\u{444}'), Some('\u{424}'), Some('\u{433}'), Some('\u{413}'), Some('\u{AB}'), Some('\u{BB}'),
        Some('\u{2591}'), Some('\u{2592}'), Some('\u{2593}'), Some('\u{2502}'), Some('\u{2524}'), Some('\u{445}'), Some('\u{425}'), Some('\u{438}'),
        Some('\u{418}'), Some('\u{2563}'), Some('\u{2551}'), Some('\u{2557}'), Some('\u{255D}'), Some('\u{439}'), Some('\u{419}'), Some('\u{2510}'),
        Some('\u{2514}'), Some('\u{2534}'), Some('\u{252C}'), Some('\u{251C}'), Some('\u{2500}'), Some('\u{253C}'), Some('\u{43A}'), Some('\u{41A}'),
        Some('\u{255A}'), Some('\u{2554}'), Some('\u{2569}'), Some('\u{2566}'), Some('\u{2560}'), Some('\u{2550}'), Some('\u{256C}'), Some('\u{A4}'),
        Some('\u{43B}'), Some('\u{41B}'), Some('\u{43C}'), Some('\u{41C}'), Some('\u{43D}'), Some('\u{41D}'), Some('\u{43E}'), Some('\u{41E}'),
        Some('\u{43F}'), Some('\u{2518}'), Some('\u{250C}'), Some('\u{2588}'), Some('\u{2584}'), Some('\u{41F}'), Some('\u{44F}'), Some('\u{2580}'),
        Some('\u{42F}'), Some('\u{440}'), Some('\u{420}'), Some('\u{441}'), Some('\u{421}'), Some('\u{442}'), Some('\u{422}'), Some('\u{443}'),
        Some('\u{423}'), Some('\u{436}'), Some('\u{416}'), Some('\u{432}'), Some('\u{412}'), Some('\u{44C}'), Some('\u{42C}'), Some('\u{2116}'),
        Some('\u{AD}'), Some('\u{44B}'), Some('\u{42B}'), Some('\u{437}'), Some('\u{417}'), Some('\u{448}'), Some('\u{428}'), Some('\u{44D}'),
        Some('\u{42D}'), Some('\u{449}'), Some('\u{429}'), Some('\u{447}'), Some('\u{427}'), Some('\u{A7}'), Some('\u{25A0}'), Some('\u{A0}'),
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
struct JpFixup {
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
        '\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}',
        '\u{a5}', '\u{203e}',
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
        '\u{2225}', '\u{ff0d}', '\u{ff5e}', '\u{ffe0}', '\u{ffe1}', '\u{ffe2}',
        '\u{80}', '\u{a5}', '\u{203e}',
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

/// The table corrections for an encoding, or `None` for one
/// `encoding_rs` already answers CRuby's way.
/// Walks a whole buffer through [`jis_direct_one`], honouring
/// `invalid:` / `undef: :replace`. Stops at the first refusal it
/// cannot replace, returning what converted, where it stopped, and
/// whether the character was ill-formed or merely homeless.
fn jis_direct_all(
    bytes: &[u8],
    from_euc: bool,
    opts: &TranscodeOpts,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> (Vec<u8>, Option<(usize, usize, bool)>) {
    let mut out = Vec::with_capacity(bytes.len());
    let mut at = 0;
    while at < bytes.len() {
        match jis_direct_one(&bytes[at..], from_euc) {
            JisCell::Cell(b, n) => {
                out.extend_from_slice(&b);
                at += n;
            }
            JisCell::Undefined(n) if opts.undef_replace => {
                out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                at += n;
            }
            JisCell::Invalid(n) if opts.invalid_replace => {
                out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                // The bytes read to disprove the sequence are read
                // again rather than swallowed with it, so the walk
                // decides how far the malformed run reaches — the
                // same answer the raising path gets.
                at += first_bad_sequence(src_enc, &bytes[at..])
                    .map(|(err, _, _)| err.len().max(1))
                    .unwrap_or(n);
            }
            JisCell::Undefined(n) => return (out, Some((at, n, false))),
            JisCell::Invalid(n) => return (out, Some((at, n, true))),
        }
    }
    (out, None)
}

/// EUC-JP and Shift_JIS spell the same JIS X 0208 plane, so CRuby
/// converts between them cell to cell and never asks which Unicode
/// character is involved — `Encoding::Converter#convpath` reports no
/// pivot for the pair. The mapping is arithmetic rather than a table,
/// and it reaches the 1957 cells per direction whose Unicode home
/// CRuby's own tables do not have, which a pivot cannot follow
/// (#1460). Windows-31J is *not* in this: CRuby pivots that pair, and
/// `EUC-JP → Windows-31J` of a row-13 cell is an undefined conversion
/// where `EUC-JP → Shift_JIS` of the same is `87 40`.
fn jis_direct_from_euc(src_enc: crate::value::Encoding, dst_enc: crate::value::Encoding) -> Option<bool> {
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
enum JisCell {
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
fn jis_direct_one(bytes: &[u8], from_euc: bool) -> JisCell {
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
        let s1 = if row <= 62 { (row + 257) / 2 } else { (row + 385) / 2 };
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
    Table(
        &'static encoding_cjk::CellTable,
        &'static encoding_rs::Encoding,
    ),
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
fn table_cell_encode(
    tab: &encoding_cjk::CellTable,
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
fn cell_table(enc: crate::value::Encoding) -> Option<&'static super::encoding_cjk::CellTable> {
    use crate::value::Encoding as E;
    // The generator diffs CRuby against the codec, so it needs a build
    // that answers from the codec alone.
    if cfg!(feature = "no-cjk-tables") {
        return None;
    }
    match enc {
        E::NamedByte(i) => match crate::value::named_byte_const_name(i) {
            "GB2312" => Some(&super::encoding_cjk::GB2312),
            "GBK" => Some(&super::encoding_cjk::GBK),
            "Big5" => Some(&super::encoding_cjk::BIG5),
            // The other two Big5s read through the same `big5` codec:
            // HKSCS differs from WHATWG's HKSCS-2008 in a few cells
            // and UAO in thousands, and both have rows below `0xA1`
            // that Big5 does not (#1500, #1520).
            "Big5_HKSCS" => Some(&super::encoding_cjk::BIG5_HKSCS),
            "Big5_UAO" => Some(&super::encoding_cjk::BIG5_UAO),
            // GB2312's grid with the traditional forms in it, read
            // through `gbk` as GB2312 is (#1520).
            "GB12345" => Some(&super::encoding_cjk::GB12345),
            // Microsoft's Big5 rather than CRuby's: thousands of
            // extra cells and a best-fit encoder, so they carry
            // tables of their own (#1567).
            "CP950" => Some(&super::encoding_cjk::CP950),
            "CP951" => Some(&super::encoding_cjk::CP951),
            // No grid of its own: GB18030 reads every cell CRuby
            // does, and only writes a handful differently.
            "GB18030" => Some(&super::encoding_cjk::GB18030),
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
    decode: &super::encoding_cp51932::CP51932_DECODE,
    encode: &super::encoding_cp51932::CP51932_ENCODE,
    reject: &super::encoding_cp51932::CP51932_REJECT,
    // WHATWG fills `A9..AC` and `AE..AF` too; CRuby maps nothing in
    // either, nor in `F5..F8` and `FD..FE`.
    dead_rows: &[(0xa9, 0xac), (0xae, 0xaf), (0xf5, 0xf8), (0xfd, 0xfe)],
    ..EUCJP_FIXUP
});

fn jp_fixup(enc: crate::value::Encoding) -> Option<&'static JpFixup> {
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
        !fx.dead_rows.iter().any(|&(lo, hi)| (lo..=hi).contains(lead))
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
    /// Where `unmapped`'s cell starts in the input. The streaming
    /// path needs it to say how much it consumed and to emit what
    /// converted before it (#1461); the one-shot path only reports.
    unmapped_at: Option<usize>,
    /// Where the first ill-formed piece starts, for the same reason.
    invalid_at: Option<usize>,
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
fn jp_decode<'a>(fx: &JpFixup, bytes: &'a [u8], undef: Option<&str>) -> JpDecoded<'a> {
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
fn cell_decode<'a>(
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

/// CRuby's `UndefinedConversionError` message for a source cell with
/// no character, spelling out the UTF-8 pivot for a non-UTF-8
/// destination exactly as the BINARY path above does.
fn undefined_cell_message(
    cell: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    // The same rendering every other quoted run gets: a printable
    // trail byte is printed, not escaped (#1607).
    let quoted = quote_error_bytes(cell);
    // A source on one of the JIS lines reaches UTF-8 through its
    // line, and CRuby names every hop of it (#1609).
    if let Some(chain) = jis_family_chain(src_enc, dst_enc)
        && chain.contains(&crate::value::Encoding::UTF8)
    {
        return format!("{quoted} to UTF-8 in conversion from {}", chain_names(&chain));
    }
    if dst_enc == crate::value::Encoding::UTF8 {
        format!("{quoted} from {} to UTF-8", src_enc.name())
    } else {
        format!(
            "{quoted} to UTF-8 in conversion from {} to UTF-8 to {}",
            src_enc.name(),
            dst_enc.name()
        )
    }
}

/// The conversion chain CRuby names in a pivoted error message: every
/// non-UTF-8 source reaches the pivot as "<src> to UTF-8", except
/// ISO-2022-JP, whose transcoder goes the long way round.
fn pivot_chain(src_enc: crate::value::Encoding) -> String {
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
/// A source cell with no character, at `at`, is reported only when
/// everything before it converts: a character the destination has no
/// cell for, earlier in the string, is what CRuby reports — the
/// encode hop gets first refusal, as it does in the stream (#1611).
fn encode_hop_first(
    src_bytes: &[u8],
    at: usize,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<()> {
    if at == 0 || at > src_bytes.len() {
        return Ok(());
    }
    transcode_bytes_with_opts(&src_bytes[..at], src_enc, dst_enc, opts, store).map(|_| ())
}

fn undefined_byte_message(
    b: u8,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let quoted = quote_error_bytes(&[b]);
    // A source whose transcoder is spelled differently from the
    // encoding is written out in full, one hop or not — with the
    // encoding's own spelling in the chain (#1530).
    if transcoder_spelling(src_enc.name()) != src_enc.name() {
        return if dst_enc == crate::value::Encoding::UTF8 {
            format!("{quoted} to UTF-8 in conversion from {} to UTF-8", src_enc.name())
        } else {
            format!(
                "{quoted} to UTF-8 in conversion from {} to UTF-8 to {}",
                src_enc.name(),
                dst_enc.name()
            )
        };
    }
    if dst_enc == crate::value::Encoding::UTF8 {
        format!("{quoted} from {} to {}", src_enc.name(), dst_enc.name())
    } else {
        format!(
            "{quoted} to UTF-8 in conversion from {} to UTF-8 to {}",
            src_enc.name(),
            dst_enc.name()
        )
    }
}

/// `opts` with the source an error message should name pinned to
/// `src` — the wrapper's own encoding rather than the intermediate it
/// rewrote its bytes into (#1609).
fn reporting_as(opts: &TranscodeOpts, src: crate::value::Encoding) -> TranscodeOpts {
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
fn stateless_kddi_enc() -> crate::value::Encoding {
    crate::value::Encoding::NamedByte(
        crate::value::named_byte_index("STATELESS_ISO_2022_JP_KDDI").unwrap_or(0),
    )
}

/// UTF8-KDDI, which stateless-ISO-2022-JP-KDDI is written from in
/// CRuby's `convpath`.
fn utf8_kddi_enc() -> crate::value::Encoding {
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
    let emoji = &super::encoding_kddi::KDDI_ISO2022_DECODE;
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
fn kddi_char_cell(c: char) -> Option<[u8; 2]> {
    let emoji = &super::encoding_kddi::KDDI_ISO2022_ENCODE;
    if let Ok(i) = emoji.binary_search_by(|(k, _)| k.cmp(&(c as u32))) {
        return Some(emoji[i].1.to_be_bytes());
    }
    // The duplicates JIS X 0208 and NEC row 13 both hold go into the
    // NEC cell here, the JIS one in CP51932.
    let prefer = &super::encoding_kddi::KDDI_ISO2022_PREFER;
    if let Ok(i) = prefer.binary_search_by(|(k, _)| k.cmp(&(c as u32))) {
        return Some(prefer[i].1.to_be_bytes());
    }
    let mut buf = [0u8; 4];
    match jp_encode(&CP51932_FIXUP, c.encode_utf8(&mut buf)).ok()?.as_slice() {
        [b1 @ 0xa1..=0xfe, b2 @ 0xa1..=0xfe] => Some([*b1, *b2]),
        _ => None,
    }
}

/// Whether `enc` is ISO-2022-JP-KDDI, the wrapper around this encoding.
fn kddi_wrapper(enc: crate::value::Encoding) -> bool {
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
            (Some(&row), Some(&col)) if kddi_cell_char([row, col]).is_some() => {
                PreciseLen::Char(3)
            }
            _ => PreciseLen::Invalid,
        },
        Some(_) => PreciseLen::Invalid,
    }
}

/// The UTF8-KDDI of a stateless-ISO-2022-JP-KDDI buffer's well-formed
/// prefix, each unit's width on both sides, and where that prefix
/// ends.
fn kddi_read(bytes: &[u8]) -> (Vec<u8>, Vec<(usize, usize)>, usize) {
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
    meta.stage = Some(("stateless-ISO-2022-JP-KDDI".to_string(), "UTF8-KDDI".to_string()));
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
fn kddi_undefined_message(
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
fn is_ibm037(enc: crate::value::Encoding) -> bool {
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
fn latin1_bytes_to_ibm037(bytes: &[u8]) -> Vec<u8> {
    bytes
        .iter()
        .map(|&b| latin1_to_ibm037(b as char).unwrap_or(b))
        .collect()
}

/// IBM037 bytes as the UTF-8 of the Latin-1 characters they stand for.
fn ibm037_to_utf8(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|&b| IBM037_TO_LATIN1[b as usize] as char)
        .collect()
}

/// `U+3042 to ISO-8859-1 in conversion from UTF-8 to ISO-8859-1 to
/// IBM037`: the hop that gives up is the one into Latin-1.
fn ibm037_undefined_message(c: char, src_enc: crate::value::Encoding) -> String {
    let chain = jis_family_chain(src_enc, ibm037_enc()).unwrap_or_else(|| vec![src_enc, ibm037_enc()]);
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
struct JisWrapper {
    inner: crate::value::Encoding,
    /// The escapes read: the stateless bytes, and the designation left
    /// in effect.
    read: fn(&[u8], Option<u8>) -> std::result::Result<(Vec<u8>, Option<u8>), crate::value::Iso2022JpStop>,
    /// The escapes written, closing back to ASCII when asked.
    write: fn(&[u8], Option<u8>, bool) -> std::result::Result<(Vec<u8>, Option<u8>), usize>,
}

fn jis_wrapper(enc: crate::value::Encoding) -> Option<JisWrapper> {
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
fn transcoder_spelling(name: &str) -> String {
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
fn carrier_no_unicode_message(
    pua: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let quoted = quote_error_bytes(pua);
    if dst_enc == crate::value::Encoding::UTF8 && carrier_utf8_form(src_enc) == Some(src_enc) {
        format!("{quoted} from {} to UTF-8", src_enc.name())
    } else if dst_enc == crate::value::Encoding::UTF8 {
        format!("{quoted} to UTF-8 in conversion from {}", pivot_chain(src_enc))
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
fn carrier_pua_bytes(own: &[u8], src_enc: crate::value::Encoding, store: &Store) -> Option<Vec<u8>> {
    let utf8_form = carrier_utf8_form(src_enc)?;
    if utf8_form == src_enc {
        return Some(own.to_vec());
    }
    transcode_bytes_with_opts(own, src_enc, utf8_form, &TranscodeOpts::default(), store).ok()
}

/// The hops of a chain, as an error message spells them.
fn chain_names(chain: &[crate::value::Encoding]) -> String {
    (0..chain.len()).map(|i| chain_hop_name(chain, i)).collect::<Vec<_>>().join(" to ")
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
fn wrapper_dst_undefined(
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
        if meta.stage.as_ref().is_some_and(|(_, d)| d == "stateless-ISO-2022-JP-KDDI")
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
                format!("U+{:04X} to {} in conversion from {names}", c as u32, hop_dst.name()),
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

/// [`wrapper_dst_undefined`] for the single-shot transcoder: the
/// inner conversion's error, re-worded from a streamed run of the
/// same bytes when it is a character the inner encoding refused.
fn wrapper_dst_error(
    e: MonorubyErr,
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    w: JisWrapper,
    opts: &TranscodeOpts,
    store: &Store,
) -> MonorubyErr {
    let (kind, _, _, mut meta) = stream_convert(src_bytes, src_enc, w.inner, None, false, opts, store);
    if !matches!(kind, StreamConvertResult::UndefinedConversion) {
        return e;
    }
    wrapper_dst_undefined(&mut meta, src_enc, dst_enc, w);
    match meta.message {
        Some(msg) => MonorubyErr::undefined_conversion_error(store, msg),
        None => e,
    }
}

/// How much of `inner` bytes read as whole sequences of their own —
/// stateless-ISO-2022-JP by its transcoder's walk, an EUC-JP variant
/// by its own.
fn inner_good_prefix(inner: crate::value::Encoding, bytes: &[u8]) -> usize {
    if inner == stateless_kddi_enc() {
        return kddi_read(bytes).2;
    }
    if stateless_iso2022jp(inner).is_some() {
        return stateless_good_prefix(bytes);
    }
    let Some((_, precise)) = conversion_walker(inner) else {
        return bytes.len();
    };
    let mut at = 0;
    while at < bytes.len() {
        match precise(bytes, at) {
            crate::value::PreciseLen::Char(n) if n > 0 => at += n,
            _ => break,
        }
    }
    at
}

/// `Some(enc)` when `enc` is one of the stateless-ISO-2022-JP pair.
fn stateless_iso2022jp(enc: crate::value::Encoding) -> Option<crate::value::Encoding> {
    matches!(enc, crate::value::Encoding::NamedByte(i)
        if matches!(
            crate::value::named_byte_const_name(i),
            "STATELESS_ISO_2022_JP" | "STATELESS_ISO_2022_JP_KDDI"
        ))
    .then_some(enc)
}

/// The first character of `src_bytes` that EUC-JP has no cell for, so
/// the chain message can name it. `None` when the source cannot even
/// be read, in which case the inner error stands as it is.
fn first_char_eucjp_refuses(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    store: &Store,
) -> Option<char> {
    let utf8 = if src_enc == crate::value::Encoding::UTF8 {
        src_bytes.to_vec()
    } else {
        transcode_bytes_with_opts(
            src_bytes,
            src_enc,
            crate::value::Encoding::UTF8,
            &TranscodeOpts::default(),
            store,
        )
        .ok()?
    };
    let mut buf = [0u8; 4];
    String::from_utf8(utf8).ok()?.chars().find(|c| {
        transcode_bytes_with_opts(
            c.encode_utf8(&mut buf).as_bytes(),
            crate::value::Encoding::UTF8,
            crate::value::Encoding::EUC_JP,
            &TranscodeOpts::default(),
            store,
        )
        .is_err()
    })
}

/// A character that reached EUC-JP and has no stateless-ISO-2022-JP
/// cell: half-width katakana (`0x8E` + a byte) or JIS X 0212 (`0x8F`
/// + a cell), the two forms ISO-2022-JP cannot spell either.
///
/// The failing hop is EUC-JP → stateless, so CRuby quotes the *EUC-JP
/// bytes* rather than naming a codepoint, and spells the chain unless
/// the source was EUC-JP itself — in which case there is only the one
/// hop to name (#1600).
fn undefined_stateless_cell_message(
    eucjp_cell: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    let bytes = quote_error_bytes(eucjp_cell);
    if src_enc == crate::value::Encoding::EUC_JP {
        format!("{bytes} from EUC-JP to {}", dst_enc.name())
    } else {
        format!(
            "{bytes} to {} in conversion from {} to EUC-JP to {}",
            dst_enc.name(),
            src_enc.name(),
            dst_enc.name()
        )
    }
}

/// The same for a character that never reached EUC-JP: the hop that
/// gave up is `… → EUC-JP`, and the chain carries the UTF-8 pivot
/// when the source needs one.
fn undefined_before_eucjp_message(
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

fn undefined_char_message(
    c: char,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> String {
    // A destination whose transcoder is spelled differently from the
    // encoding asked for is written out in full, one hop or not.
    let spelled = transcoder_spelling(dst_enc.name());
    if spelled != dst_enc.name() {
        let from = if src_enc == crate::value::Encoding::UTF8 {
            src_enc.name().to_string()
        } else {
            pivot_chain(src_enc)
        };
        return format!("U+{:04X} to {spelled} in conversion from {from} to {spelled}", c as u32);
    }
    // The short form is for a conversion that really is one hop, so
    // the source has to *be* the pivot — `UTF8-MAC` is UTF-8
    // compatible and still a step of its own (#1609).
    if src_enc == crate::value::Encoding::UTF8 || src_enc == crate::value::Encoding::UsAscii {
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
        if fx.pua && let Some(cell) = windows31j_pua_cell(c) {
            out.extend_from_slice(&cell);
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

/// A US-ASCII source decoded with `invalid: :replace`.
///
/// US-ASCII has no `encoding_rs` codec here, no single-byte table and
/// no `mbc_walker`, so none of the three paths that honour `invalid:`
/// covered it and a byte above 0x7F raised whatever the caller had
/// asked for (#1570). Its walk needs none of them: every byte below
/// 0x80 is its own character and every byte at or above it is one
/// ill-formed byte.
fn usascii_decode_lossy(src_bytes: &[u8], repl: &str) -> String {
    let mut out = String::with_capacity(src_bytes.len());
    for &b in src_bytes {
        if b < 0x80 {
            out.push(b as char);
        } else {
            out.push_str(repl);
        }
    }
    out
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
fn insert_encoding(
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
fn inserted_replacement(insert_enc: crate::value::Encoding) -> &'static str {
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
fn dummy_wide_source(
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
        // CRuby: default replacement is "�" for the UTF
        // destinations and "?" otherwise — see `replaces_with_u_fffd`
        // for which names those are, since `UTF8-MAC` is not one of
        // them and CESU-8 is.
        match dst_enc {
            _ if replaces_with_u_fffd(dst_enc) => "\u{FFFD}".to_string(),
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

/// `src_bytes` as UTF-8, on the way to a destination whose codec is
/// UTF-8's with a rewrite after it (`UTF8-MAC`, CESU-8).
///
/// The pivot conversion is the ordinary pipeline with UTF-8 as its
/// destination; what this adds is `dst_enc`'s share of the *reporting*.
/// A source that is already UTF-8 converts by doing nothing, so nothing
/// would otherwise notice that its bytes are broken — CRuby raises
/// there, naming the destination the caller asked for. And an
/// `UndefinedConversionError` out of the pivot is a two-hop failure to
/// CRuby, which spells both hops.
/// One hop of a carrier-to-carrier conversion: read the bytes in the
/// hop's source base, rewrite what its table names, write them in the
/// destination base. `src_enc` and `dst_enc` are the conversion's own
/// ends, which is what an error message names (#1573).
#[allow(clippy::too_many_arguments)]
fn carrier_hop(
    bytes: &[u8],
    from: crate::value::Encoding,
    to: crate::value::Encoding,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<Vec<u8>> {
    let from_base = carrier_base(from).expect("a carrier has a base");
    let to_base = carrier_base(to).expect("a carrier has a base");
    let text = if from_base == crate::value::Encoding::UTF8 {
        std::str::from_utf8(bytes)
            .map_err(|_| invalid_byte_sequence(store, src_enc, dst_enc, bytes))?
            .to_string()
    } else {
        let pivot =
            transcode_bytes_with_opts(bytes, from_base, crate::value::Encoding::UTF8, &TranscodeOpts::default(), store)?;
        String::from_utf8(pivot).map_err(|_| invalid_byte_sequence(store, src_enc, dst_enc, bytes))?
    };
    // Crossing a vendor's two forms, only its own emoji go by the
    // table; everything else is Unicode's to carry, and the base
    // tables have things to say about it — the `SJIS-*` encoders
    // prefer the NEC-selected row for the 383 characters Windows-31J
    // has two cells for. So that hop is the ordinary conversion with
    // the table standing in for the emoji Unicode cannot hold.
    if carrier_vendor(from) == carrier_vendor(to) {
        let table = carrier_pair(from, to);
        let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
        for c in text.chars() {
            let piece = match table.and_then(|t| t.maps(&[c])) {
                Some(Some(spelled)) => {
                    let s: String = spelled.into_iter().collect();
                    transcode_bytes_with_opts(s.as_bytes(), crate::value::Encoding::UTF8, to_base, opts, store)?
                }
                Some(None) => {
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_char_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                    ));
                }
                // `text` is already Unicode, so what is left is the
                // ordinary conversion into the destination form —
                // which is where its own table runs.
                None => transcode_bytes_with_opts(
                    c.to_string().as_bytes(),
                    crate::value::Encoding::UTF8,
                    to,
                    opts,
                    store,
                )
                .or_else(|e| {
                    if opts.undef_replace {
                        Ok(opts.replace_str(dst_enc).into_bytes())
                    } else {
                        Err(e)
                    }
                })?,
            };
            out.extend_from_slice(&piece);
        }
        return Ok(out);
    }
    let spelled = match carrier_pair(from, to) {
        None => text,
        Some(table) => match carrier_to_carrier(&text, table) {
            Ok(spelled) => spelled,
            Err(c) if opts.undef_replace => {
                let replace = opts.replace_str(dst_enc);
                let mut out = String::with_capacity(text.len());
                for c in text.chars() {
                    match carrier_to_carrier(&c.to_string(), table) {
                        Ok(piece) => out.push_str(&piece),
                        Err(_) => out.push_str(&replace),
                    }
                }
                let _ = c;
                out
            }
            Err(c) => {
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_char_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                ));
            }
        },
    };
    if to_base == crate::value::Encoding::UTF8 {
        Ok(spelled.into_bytes())
    } else {
        transcode_bytes_with_opts(spelled.as_bytes(), crate::value::Encoding::UTF8, to_base, opts, store)
    }
}

/// An `SJIS-*` carrier source read as Unicode.
///
/// The walk is Windows-31J's, and each two-byte cell is asked of the
/// carrier's table first: it reads a block of them as its own emoji
/// and holds no character at all for another block, both of which
/// Windows-31J reads as private-use characters. Everything else is
/// Windows-31J's own reading. `Err(Some(cell))` is a cell the carrier
/// does not hold; `Err(None)` is a byte sequence the walk itself
/// rejects, which the caller reports as it reports any other (#1573).
fn carrier_sjis_to_unicode(
    bytes: &[u8],
    table: &'static SjisCarrier,
    store: &Store,
) -> std::result::Result<String, Option<u16>> {
    let base = crate::value::Encoding::Sjis(crate::value::WINDOWS_31J);
    let mut out = String::with_capacity(bytes.len());
    let mut at = 0;
    while at < bytes.len() {
        let b = bytes[at];
        if b < 0x80 {
            out.push(b as char);
            at += 1;
            continue;
        }
        let width = match crate::value::sjis_precise_len(bytes, at) {
            crate::value::PreciseLen::Char(n) => n,
            _ => return Err(None),
        };
        if width == 2 {
            let cell = ((bytes[at] as u16) << 8) | bytes[at + 1] as u16;
            if table.unreadable(cell) {
                return Err(Some(cell));
            }
            if let Some(cs) = table.reads(cell) {
                out.extend(cs);
                at += 2;
                continue;
            }
        }
        let piece = transcode_bytes_with_opts(
            &bytes[at..at + width],
            base,
            crate::value::Encoding::UTF8,
            &TranscodeOpts::default(),
            store,
        )
        .map_err(|_| Some(((bytes[at] as u16) << 8) | *bytes.get(at + 1).unwrap_or(&0) as u16))?;
        let Ok(text) = String::from_utf8(piece) else {
            return Err(None);
        };
        out.push_str(&text);
        at += width;
    }
    Ok(out)
}

/// Unicode written as an `SJIS-*` carrier: the characters its table
/// names go into the carrier's own cells — its emoji, and the
/// NEC-selected row it prefers where Windows-31J has two cells for one
/// character — and the rest are Windows-31J's to write (#1573).
fn unicode_to_carrier_sjis(
    s: &str,
    table: &'static SjisCarrier,
    store: &Store,
) -> std::result::Result<Vec<u8>, char> {
    let base = crate::value::Encoding::Sjis(crate::value::WINDOWS_31J);
    let mut out: Vec<u8> = Vec::with_capacity(s.len());
    let mut buf = [0u8; 4];
    for c in s.chars() {
        if table.refuses(c) {
            return Err(c);
        }
        if let Some(bytes) = table.writes(c) {
            out.extend_from_slice(&bytes);
            continue;
        }
        let piece = transcode_bytes_with_opts(
            c.encode_utf8(&mut buf).as_bytes(),
            crate::value::Encoding::UTF8,
            base,
            &TranscodeOpts::default(),
            store,
        )
        .map_err(|_| c)?;
        out.extend_from_slice(&piece);
    }
    Ok(out)
}

/// One carrier's text rewritten as another's.
///
/// A conversion with a carrier at each end is a transcoder of its own
/// in CRuby, not a round trip through the pivot: a carrier's emoji
/// converts to the other's even where it has no Unicode meaning at all
/// and so could not have gone through one. Everything the table has no
/// say in keeps its own spelling (#1573).
fn carrier_to_carrier(s: &str, table: &'static CarrierPair) -> std::result::Result<String, char> {
    let cs: Vec<char> = s.chars().collect();
    let mut out = String::with_capacity(s.len());
    let mut at = 0;
    while at < cs.len() {
        // The longer key wins: two of one carrier's characters can be
        // one of another's.
        let mut took = None;
        for n in (1..=CarrierPair::LOOKAHEAD.min(cs.len() - at)).rev() {
            if let Some(answer) = table.maps(&cs[at..at + n]) {
                took = Some((n, answer));
                break;
            }
        }
        match took {
            Some((_, None)) => return Err(cs[at]),
            Some((n, Some(spelled))) => {
                out.extend(spelled);
                at += n;
            }
            None => {
                out.push(cs[at]);
                at += 1;
            }
        }
    }
    Ok(out)
}

/// A `UTF8-*` carrier source read as Unicode: the carrier's own
/// characters mean what its table says, and the rest mean what UTF-8
/// means. `Err` names the first character the carrier holds no meaning
/// for (#1573).
fn carrier_utf8_to_unicode(s: &str, table: &'static Utf8Carrier) -> std::result::Result<String, char> {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if table.unreadable(c) {
            return Err(c);
        }
        match table.reads(c) {
            Some(cs) => out.extend(cs),
            None => out.push(c),
        }
    }
    Ok(out)
}

/// Unicode written as a `UTF8-*` carrier: the characters its table
/// names become the carrier's own emoji, the rest keep their UTF-8
/// spelling. `Err` names the first character the carrier cannot spell.
fn unicode_to_carrier_utf8(s: &str, table: &'static Utf8Carrier) -> std::result::Result<String, char> {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if table.refuses(c) {
            return Err(c);
        }
        match table.writes(c) {
            Some(cs) => out.extend(cs),
            None => out.push(c),
        }
    }
    Ok(out)
}



fn to_pivot_for(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<String> {
    // The inner conversion runs to UTF-8, so left alone it resolves
    // `invalid:` / `undef:`'s default replacement against the *pivot*.
    // That is right for one of these two destinations and not the
    // other, so pin it here rather than letting the recursion decide
    // (#1571).
    let pinned;
    let opts = if opts.replace.is_none() && (opts.invalid_replace || opts.undef_replace) {
        pinned = TranscodeOpts {
            replace: Some(inserted_replacement(dst_enc).to_string()),
            ..opts.clone()
        };
        &pinned
    } else {
        opts
    };
    let utf8 = transcode_bytes_with_opts(
        src_bytes,
        src_enc,
        crate::value::Encoding::UTF8,
        opts,
        store,
    )
    .map_err(|e| name_pivot_destination(e, src_enc, dst_enc))?;
    match String::from_utf8(utf8) {
        Ok(s) => Ok(s),
        Err(e) => Err(invalid_byte_sequence(
            store,
            src_enc,
            dst_enc,
            e.as_bytes(),
        )),
    }
}

/// Re-spell a pivot conversion's error for the destination the caller
/// actually named.
///
/// The inner conversion ran to UTF-8, so an undefined source byte was
/// reported as `"\xFF" from ASCII-8BIT to UTF-8` — the one-hop form.
/// With a destination past the pivot the failure is the first of two
/// hops, and CRuby names them both. Everything else (an invalid byte
/// sequence, which names only the *source*) already reads correctly.
fn name_pivot_destination(
    err: MonorubyErr,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
) -> MonorubyErr {
    let one_hop = format!(" from {} to UTF-8", src_enc.name());
    let Some(quoted) = err.message().strip_suffix(&one_hop) else {
        return err;
    };
    let msg = format!(
        "{quoted} to UTF-8 in conversion from {} to UTF-8 to {}",
        src_enc.name(),
        dst_enc.name()
    );
    let mut err = err;
    err.set_msg(msg);
    err
}

pub(super) fn transcode_bytes_with_opts(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    store: &Store,
) -> Result<Vec<u8>> {
    use crate::value::Encoding as E;
    // A replacement the destination cannot spell is refused here,
    // before any of the input is read, because that is where CRuby
    // opens the converter (#1566).
    if opens_a_converter(src_bytes, src_enc, dst_enc, opts, false) {
        // A pair with no transcoder has nothing to open, decorators
        // or not: 7-bit text passes through the fast path below only
        // when nothing is asked of a converter, as MacJapanese and
        // Windows-1258 have it in CRuby (#1591).
        if !has_codec(src_enc) || !has_codec(dst_enc) {
            return Err(converter_not_found(store, src_enc, dst_enc, opts, None));
        }
        validate_replacement(opts, src_enc, dst_enc, None, store)?;
    }
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
    // A same-encoding `invalid: :replace` is `String#scrub`, and only
    // a receiver with something to scrub consults the replacement at
    // all (#1599).
    let scrubbing = src_enc == dst_enc
        && opts.invalid_replace
        && !opts.has_newline()
        && matches!(
            RStringInner::from_encoding_scanned(src_bytes, src_enc).code_range(),
            crate::value::CodeRange::Broken
        );
    let scrub_in_place = scrubbing
        && (encoding_to_rs(src_enc).is_some()
            || is_utf16_or_32(src_enc)
            || single_byte_table(src_enc).is_some()
            // US-ASCII has neither, but the decode below scrubs it
            // by hand, so a broken one must not take the identity
            // path that copies the offending byte (#1570).
            || src_enc == E::UsAscii);
    // The encodings monoruby walks itself (Emacs-Mule, EUC-JP,
    // Shift_JIS) scrub through that walk, not through a codec. For
    // Emacs-Mule there is no codec to use; for the other two there is,
    // and using it was wrong — `encoding_rs`'s EUC-JP and Shift_JIS are
    // WHATWG's, so a decode / re-encode round trip rewrote cells CRuby
    // leaves alone (`FC A1` came back as the JIS X 0212 `8F E3 A6`) and
    // silently accepted bytes onigenc calls broken (Shift_JIS `0x80`).
    // A same-encoding `invalid: :replace` *is* `String#scrub` in CRuby,
    // so it has to be the same walk here too.
    // The replacement has to suit the *receiver* — `rb_enc_check`'s
    // rule, not the converter's. Both scrubbing routes substitute, so
    // the question is asked once, here, and the walk below reuses the
    // bytes it answers with (#1599).
    let scrub_bytes = if scrubbing {
        Some(scrub_replacement_bytes(opts, src_enc, store)?)
    } else {
        None
    };
    if src_enc == dst_enc
        && opts.invalid_replace
        && !opts.has_newline()
        && let Some((max_len, precise)) = crate::value::mbc_walker(src_enc)
    {
        let Some(replace) = scrub_bytes else {
            // Nothing to scrub: the walk would change nothing.
            return Ok(src_bytes.to_vec());
        };
        return Ok(crate::value::scrub_mbc(
            src_bytes,
            &replace,
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
    // `UTF8-MAC` holds UTF-8 bytes in Apple's HFS+ decomposed form, so
    // a conversion to or from it is a normalisation wrapped around the
    // ordinary pipeline rather than a codec of its own: take the source
    // out of that form first, put the destination into it last (#1562).
    // Broken input falls through, so it is the pipeline that reports
    // it, with the message it already gets right.
    if src_enc != dst_enc {
        let mac = crate::value::Encoding::Utf8(crate::value::UTF8_MAC);
        if src_enc == mac {
            if let Ok(s) = std::str::from_utf8(src_bytes) {
                let composed = crate::value::mac_to_utf8(s);
                return transcode_bytes_with_opts(
                    composed.as_bytes(),
                    crate::value::Encoding::UTF8,
                    dst_enc,
                    &reporting_as(opts, src_enc),
                    store,
                );
            }
            if opts.invalid_replace {
                // Broken input still composes: CRuby's transcoder goes
                // on holding the cluster it was building, so the
                // replacement lands where the cluster is not yet and
                // the marks after the bad byte still join it (#1577).
                let leads = mac_replacement_leads(dst_enc);
                let replace = opts.replace.clone().unwrap_or_else(|| {
                    inserted_replacement(insert_encoding(mac, dst_enc)).to_string()
                });
                let composed = mac_replace_pivot(src_bytes, &replace, leads, false);
                return transcode_bytes_with_opts(
                    composed.pivot.as_bytes(),
                    crate::value::Encoding::UTF8,
                    dst_enc,
                    &reporting_as(opts, src_enc),
                    store,
                );
            }
            // Without it the pipeline below reports the bad byte, with
            // the message it already gets right.
        }
        if dst_enc == mac {
            let utf8 = to_pivot_for(src_bytes, src_enc, dst_enc, opts, store)?;
            return Ok(crate::value::utf8_to_mac(&utf8).into_bytes());
        }
        // stateless-ISO-2022-JP-KDDI rides UTF8-KDDI, not EUC-JP: the
        // emoji rows have no EUC-JP form, and the cells the two share
        // are CP51932's (#1530).
        if src_enc == stateless_kddi_enc() && !kddi_wrapper(dst_enc) {
            let kddi = utf8_kddi_enc();
            let (text, _, good) = kddi_read(src_bytes);
            if good < src_bytes.len() {
                if opts.invalid_replace {
                    // As stateless-ISO-2022-JP below: each good run
                    // converts whole and the destination's
                    // replacement goes between them.
                    let mut out = transcode_bytes_with_opts(
                        &src_bytes[..good],
                        src_enc,
                        dst_enc,
                        opts,
                        store,
                    )?;
                    out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                    let run = first_bad_sequence(src_enc, &src_bytes[good..])
                        .map(|(e, _, _)| e.len().max(1))
                        .unwrap_or(1);
                    let rest = transcode_bytes_with_opts(
                        &src_bytes[(good + run).min(src_bytes.len())..],
                        src_enc,
                        dst_enc,
                        opts,
                        store,
                    )?;
                    out.extend_from_slice(&rest);
                    return Ok(out);
                }
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
            }
            if dst_enc == kddi {
                if opts.has_newline() {
                    return Ok(apply_newline_bytes(&text, opts));
                }
                return Ok(text);
            }
            return transcode_bytes_with_opts(&text, kddi, dst_enc, opts, store).map_err(|e| {
                // A refusal further down the line is named against
                // the whole chain, which the stream knows how to say.
                let (kind, _, _, meta) =
                    stream_convert(src_bytes, src_enc, dst_enc, None, false, opts, store);
                match (kind, meta.message) {
                    (StreamConvertResult::UndefinedConversion, Some(msg)) => {
                        MonorubyErr::undefined_conversion_error(store, msg)
                    }
                    _ => e,
                }
            });
        }
        if dst_enc == stateless_kddi_enc() && !kddi_wrapper(src_enc) {
            let kddi = utf8_kddi_enc();
            let text = transcode_bytes_with_opts(src_bytes, src_enc, kddi, opts, store)?;
            let Ok(text) = std::str::from_utf8(&text) else {
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
            };
            let mut out = Vec::with_capacity(text.len());
            for c in text.chars() {
                if c.is_ascii() {
                    out.push(c as u8);
                    continue;
                }
                match kddi_char_cell(c) {
                    Some([b1, b2]) => out.extend_from_slice(&[0x92, b1, b2]),
                    None if opts.undef_replace => {
                        for r in opts.replace_str(dst_enc).chars() {
                            if r.is_ascii() {
                                out.push(r as u8);
                            } else if let Some([b1, b2]) = kddi_char_cell(r) {
                                out.extend_from_slice(&[0x92, b1, b2]);
                            }
                        }
                    }
                    None => {
                        return Err(MonorubyErr::undefined_conversion_error(
                            store,
                            kddi_undefined_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                        ));
                    }
                }
            }
            return Ok(out);
        }
        // A carrier at each end is CRuby's own transcoder, not a round
        // trip through the pivot: a carrier's emoji converts to the
        // other's even where it has no Unicode meaning at all, and one
        // vendor's two encodings hold the same emoji in different
        // bases. `carrier_route` is the `convpath` CRuby reports for
        // the pair, and each hop is one table read in the two sides'
        // base readings (#1573).
        if carrier_vendor(src_enc).is_some() && carrier_vendor(dst_enc).is_some() {
            let mut bytes = src_bytes.to_vec();
            for (from, to) in carrier_route(src_enc, dst_enc) {
                bytes = carrier_hop(&bytes, from, to, src_enc, dst_enc, opts, store)?;
            }
            return Ok(bytes);
        }
        // The six carrier sets ride it too. Each is its base with a
        // block of characters spelled as one Japanese carrier's emoji,
        // so one side of the conversion is that table and the other is
        // the ordinary pipeline. `UTF8-*` reads and writes characters;
        // `SJIS-*` reads *cells*, because Windows-31J's table is
        // many-to-one and `SJIS-SoftBank` tells two of its cells apart
        // where Windows-31J does not (#1573).
        if let crate::value::Encoding::Utf8(i) = src_enc
            && let Some(table) = utf8_carrier(i)
            && let Ok(text) = std::str::from_utf8(src_bytes)
            // ISO-2022-JP-KDDI is written from UTF8-KDDI itself, with
            // no Unicode in between (#1530).
            && !kddi_wrapper(dst_enc)
        {
            let unicode = match carrier_utf8_to_unicode(text, table) {
                Ok(unicode) => unicode,
                // A carrier character with no Unicode meaning at all
                // is an undefined conversion, and `undef: :replace`
                // stands the destination's replacement in for it like
                // any other.
                Err(_) if opts.undef_replace => {
                    let replace = opts.replace_str(dst_enc);
                    let mut out = String::with_capacity(text.len());
                    for c in text.chars() {
                        match carrier_utf8_to_unicode(&c.to_string(), table) {
                            Ok(piece) => out.push_str(&piece),
                            Err(_) => out.push_str(&replace),
                        }
                    }
                    out
                }
                Err(c) => {
                    let mut buf = [0u8; 4];
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        carrier_no_unicode_message(
                            c.encode_utf8(&mut buf).as_bytes(),
                            opts.report_src.unwrap_or(src_enc),
                            dst_enc,
                        ),
                    ));
                }
            };
            return transcode_bytes_with_opts(
                unicode.as_bytes(),
                crate::value::Encoding::UTF8,
                dst_enc,
                &reporting_as(opts, src_enc),
                store,
            );
        }
        if let crate::value::Encoding::Utf8(i) = dst_enc
            && let Some(table) = utf8_carrier(i)
            && !kddi_wrapper(src_enc)
        {
            let utf8 = to_pivot_for(src_bytes, src_enc, dst_enc, opts, store)?;
            return match unicode_to_carrier_utf8(&utf8, table) {
                Ok(text) => Ok(text.into_bytes()),
                Err(c) if opts.undef_replace => {
                    let replace = opts.replace_str(dst_enc);
                    let mut out = String::with_capacity(utf8.len());
                    for c in utf8.chars() {
                        match unicode_to_carrier_utf8(&c.to_string(), table) {
                            Ok(piece) => out.push_str(&piece),
                            Err(_) => out.push_str(&replace),
                        }
                    }
                    let _ = c;
                    Ok(out.into_bytes())
                }
                Err(c) => Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_char_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                )),
            };
        }
        if let crate::value::Encoding::Sjis(i) = src_enc
            && let Some(table) = sjis_carrier(i)
        {
            // The same for a cell the carrier holds no character for.
            let replaced;
            let attempt = match carrier_sjis_to_unicode(src_bytes, table, store) {
                Err(Some(_)) if opts.undef_replace => {
                    let replace = opts.replace_str(dst_enc);
                    let mut out = String::with_capacity(src_bytes.len());
                    let mut at = 0;
                    while at < src_bytes.len() {
                        let n = match crate::value::sjis_precise_len(src_bytes, at) {
                            crate::value::PreciseLen::Char(n) if n > 0 => n,
                            _ => break,
                        };
                        match carrier_sjis_to_unicode(&src_bytes[at..at + n], table, store) {
                            Ok(piece) => out.push_str(&piece),
                            Err(_) => out.push_str(&replace),
                        }
                        at += n;
                    }
                    replaced = out;
                    Ok(replaced)
                }
                other => other,
            };
            match attempt {
                Ok(unicode) => {
                    return transcode_bytes_with_opts(
                        unicode.as_bytes(),
                        crate::value::Encoding::UTF8,
                        dst_enc,
                        &reporting_as(opts, src_enc),
                        store,
                    );
                }
                Err(Some(cell)) => {
                    let bytes = [(cell >> 8) as u8, cell as u8];
                    if let Some(pua) = carrier_pua_bytes(&bytes, src_enc, store) {
                        return Err(MonorubyErr::undefined_conversion_error(
                            store,
                            carrier_no_unicode_message(&pua, src_enc, dst_enc),
                        ));
                    }
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        format!(
                            "{} to UTF-8 in conversion from {} to UTF-8 to {}",
                            quote_error_bytes(&bytes),
                            src_enc.name(),
                            dst_enc.name()
                        ),
                    ));
                }
                // The walk itself refused the bytes: the pipeline
                // reports that, with the message it already gets right.
                Err(None) => {}
            }
        }
        if let crate::value::Encoding::Sjis(i) = dst_enc
            && let Some(table) = sjis_carrier(i)
        {
            let utf8 = to_pivot_for(src_bytes, src_enc, dst_enc, opts, store)?;
            return match unicode_to_carrier_sjis(&utf8, table, store) {
                Ok(bytes) => Ok(bytes),
                Err(c) if opts.undef_replace => {
                    let replace = opts.replace_str(dst_enc);
                    let mut out: Vec<u8> = Vec::with_capacity(utf8.len());
                    for c in utf8.chars() {
                        match unicode_to_carrier_sjis(&c.to_string(), table, store) {
                            Ok(piece) => out.extend_from_slice(&piece),
                            Err(_) => out.extend_from_slice(replace.as_bytes()),
                        }
                    }
                    let _ = c;
                    Ok(out)
                }
                Err(c) => Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_char_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                )),
            };
        }
        // CESU-8 rides the same wrapper: it is UTF-8 with the
        // supplementary planes spelled as surrogate pairs, so one side
        // of the conversion is the rewrite and the other is the
        // ordinary pipeline (#1562).
        let cesu = crate::value::Encoding::NamedByte(crate::value::CESU_8);
        if src_enc == cesu {
            let scrubbed;
            let mut bytes = src_bytes;
            if opts.invalid_replace {
                // The replacement is the *destination's* — an EUC-JP
                // destination takes `"?"`, not `U+FFFD` — and it goes
                // into the pivot as text, so the pipeline converts it
                // along with everything else.
                scrubbed = crate::value::scrub_mbc(
                    src_bytes,
                    opts.replace_str(dst_enc).as_bytes(),
                    crate::value::CESU8_MAX_LEN,
                    crate::value::cesu8_precise_len,
                );
                bytes = &scrubbed;
            }
            let Some(s) = crate::value::cesu8_to_utf8(bytes) else {
                return Err(invalid_byte_sequence(store, src_enc, dst_enc, bytes));
            };
            return transcode_bytes_with_opts(
                s.as_bytes(),
                crate::value::Encoding::UTF8,
                dst_enc,
                &reporting_as(opts, src_enc),
                store,
            );
        }
        if dst_enc == cesu {
            let utf8 = to_pivot_for(src_bytes, src_enc, dst_enc, opts, store)?;
            return Ok(crate::value::utf8_to_cesu8(&utf8));
        }
        // stateless-ISO-2022-JP rides it too, but its other side is
        // EUC-JP rather than UTF-8: it is ISO-2022-JP with the
        // character set named by a lead byte instead of by an escape
        // sequence, and the cell after it written as EUC-JP writes it.
        // So a conversion is that rewrite plus EUC-JP's own, which is
        // the chain CRuby's errors name (#1600).
        // ISO-2022-JP rides stateless-ISO-2022-JP, which rides EUC-JP:
        // the three are the same repertoire written three ways, and
        // CRuby's `convpath` spells exactly that chain (#1609).
        if let Some(w) = jis_wrapper(src_enc) {
            let stateless = match (w.read)(src_bytes, None).map(|(o, _)| o) {
                Ok(b) => b,
                Err(_) if opts.invalid_replace => {
                    // Each good run converts whole and the
                    // destination's replacement goes between them.
                    // Parsing resumes in the designation that was in
                    // effect, so the byte after a substituted one is
                    // still read as part of its character set rather
                    // than as ASCII.
                    let mut out: Vec<u8> = Vec::new();
                    let mut at = 0;
                    let mut state = None;
                    while at <= src_bytes.len() {
                        match (w.read)(&src_bytes[at..], state) {
                            Ok((piece, _)) => {
                                out.extend_from_slice(&transcode_bytes_with_opts(
                                    &piece,
                                    w.inner,
                                    dst_enc,
                                    &reporting_as(opts, src_enc),
                                    store,
                                )?);
                                break;
                            }
                            Err(stop) => {
                                let (piece, left) = (w.read)(
                                    &src_bytes[at..at + stop.at],
                                    state,
                                )
                                .unwrap_or_default();
                                out.extend_from_slice(&transcode_bytes_with_opts(
                                    &piece,
                                    w.inner,
                                    dst_enc,
                                    &reporting_as(opts, src_enc),
                                    store,
                                )?);
                                out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                                state = left;
                                let step = (stop.at + stop.error.len().max(1)).max(1);
                                at += step;
                                if at > src_bytes.len() {
                                    break;
                                }
                            }
                        }
                    }
                    return Ok(out);
                }
                Err(_) => {
                    return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
                }
            };
            if dst_enc == w.inner {
                return Ok(stateless);
            }
            return transcode_bytes_with_opts(
                &stateless,
                w.inner,
                dst_enc,
                &reporting_as(opts, src_enc),
                store,
            );
        }
        if let Some(w) = jis_wrapper(dst_enc) {
            let stateless = if src_enc == w.inner {
                src_bytes.to_vec()
            } else {
                transcode_bytes_with_opts(src_bytes, src_enc, w.inner, opts, store)
                    .map_err(|e| wrapper_dst_error(e, src_bytes, src_enc, dst_enc, w, opts, store))?
            };
            if opts.invalid_replace
                && (w.write)(&stateless, None, true).map(|(o, _)| o).is_err()
            {
                // The malformed run is the stateless source's, so it
                // is settled at that hop: each good run converts whole
                // and the replacement goes between them.
                let mut out: Vec<u8> = Vec::new();
                let mut at = 0;
                while at < stateless.len() {
                    let good = inner_good_prefix(w.inner, &stateless[at..]);
                    if good > 0 {
                        let (piece, _) = (w.write)(
                            &stateless[at..at + good],
                            None,
                            true,
                        )
                        .unwrap_or_default();
                        out.extend_from_slice(&piece);
                    }
                    at += good;
                    if at >= stateless.len() {
                        break;
                    }
                    out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                    let run = first_bad_sequence(w.inner, &stateless[at..])
                        .map(|(e, _, _)| e.len().max(1))
                        .unwrap_or(1);
                    at += run;
                }
                return Ok(out);
            }
            return match (w.write)(&stateless, None, true).map(|(o, _)| o) {
                Ok(out) => Ok(out),
                // ISO-2022-JP takes exactly what stateless's
                // transcoder takes, so there is no well-formed
                // sequence without a home here — an `Err` is a
                // malformed stateless run, which only a
                // stateless *source* can hand over.
                Err(_) => Err(invalid_byte_sequence(
                    store,
                    w.inner,
                    dst_enc,
                    &stateless,
                )),
            };
        }
        if let Some(stateless) = stateless_iso2022jp(src_enc) {
            let Ok(eucjp) = crate::value::stateless_iso2022jp_to_eucjp(src_bytes) else {
                if opts.invalid_replace {
                    // The replacement is the *destination's*, and a
                    // malformed run is settled at this hop, so each
                    // good run converts whole and the replacement
                    // goes straight into the output between them —
                    // it need not be spellable in EUC-JP, which the
                    // pivot would have required.
                    let good = stateless_good_prefix(src_bytes);
                    let mut out = transcode_bytes_with_opts(
                        &src_bytes[..good],
                        stateless,
                        dst_enc,
                        opts,
                        store,
                    )?;
                    out.extend_from_slice(opts.replace_str(dst_enc).as_bytes());
                    let run = first_bad_sequence(stateless, &src_bytes[good..])
                        .map(|(e, _, _)| e.len().max(1))
                        .unwrap_or(1);
                    let rest = transcode_bytes_with_opts(
                        &src_bytes[(good + run).min(src_bytes.len())..],
                        stateless,
                        dst_enc,
                        opts,
                        store,
                    )?;
                    out.extend_from_slice(&rest);
                    return Ok(out);
                }
                return Err(invalid_byte_sequence(store, stateless, dst_enc, src_bytes));
            };
            if dst_enc == crate::value::Encoding::EUC_JP {
                return Ok(eucjp);
            }
            return transcode_bytes_with_opts(
                &eucjp,
                crate::value::Encoding::EUC_JP,
                dst_enc,
                &reporting_as(opts, stateless),
                store,
            );
        }
        if let Some(stateless) = stateless_iso2022jp(dst_enc) {
            let eucjp = if src_enc == crate::value::Encoding::EUC_JP {
                src_bytes.to_vec()
            } else {
                transcode_bytes_with_opts(
                    src_bytes,
                    src_enc,
                    crate::value::Encoding::EUC_JP,
                    opts,
                    store,
                )
                // The hop that gave up is `… → EUC-JP`, but it is one
                // step of a longer conversion, and CRuby names the
                // whole of it.
                .map_err(|e| {
                    match first_char_eucjp_refuses(src_bytes, src_enc, store) {
                        Some(c) => MonorubyErr::undefined_conversion_error(
                            store,
                            undefined_before_eucjp_message(c, src_enc, stateless),
                        ),
                        None => e,
                    }
                })?
            };
            return match crate::value::eucjp_to_stateless_iso2022jp(&eucjp) {
                Ok(out) => Ok(out),
                // What is left in EUC-JP and not in stateless is
                // half-width katakana and JIS X 0212.
                Err(at) => {
                    let n = crate::value::eucjp_char_width(&eucjp[at..])
                        .unwrap_or(1)
                        .max(1);
                    let cell = &eucjp[at..(at + n).min(eucjp.len())];
                    if opts.undef_replace {
                        let mut out =
                            crate::value::eucjp_to_stateless_iso2022jp(&eucjp[..at])
                                .unwrap_or_default();
                        out.extend_from_slice(opts.replace_str(stateless).as_bytes());
                        let rest = transcode_bytes_with_opts(
                            &eucjp[at + cell.len()..],
                            crate::value::Encoding::EUC_JP,
                            stateless,
                            opts,
                            store,
                        )?;
                        out.extend_from_slice(&rest);
                        return Ok(out);
                    }
                    Err(MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_stateless_cell_message(cell, src_enc, stateless),
                    ))
                }
            };
        }
    }
    // The endianness-less dummies read the other way too: the BOM
    // names the endianness and is consumed, and without one there is
    // nothing to say which end the code units start at, so the source
    // is ill-formed (#1576). `dst_enc` may be the dummy as well — the
    // encode half below writes its own BOM.
    if dummy_wide_target(src_enc).is_some() {
        let Some((wide, rest)) = dummy_wide_source(src_enc, src_bytes) else {
            return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
        };
        return transcode_bytes_with_opts(rest, wide, dst_enc, opts, store);
    }
    // EUC-JP ↔ Shift_JIS needs no pivot: CRuby maps the shared JIS
    // X 0208 plane cell to cell, which reaches the cells its own
    // tables have no Unicode home for (#1460).
    if let Some(from_euc) = jis_direct_from_euc(src_enc, dst_enc)
        && !opts.has_newline()
    {
        let (out, stop) = jis_direct_all(src_bytes, from_euc, opts, src_enc, dst_enc);
        return match stop {
            None => Ok(out),
            Some((at, _n, true)) => Err(invalid_byte_sequence(
                store,
                src_enc,
                dst_enc,
                &src_bytes[at..],
            )),
            Some((at, n, false)) => {
                // Named against the two encodings themselves — this
                // conversion has no pivot to name.
                let cell = &src_bytes[at..(at + n).min(src_bytes.len())];
                Err(MonorubyErr::undefined_conversion_error(
                    store,
                    format!(
                        "{} from {} to {}",
                        quote_error_bytes(cell),
                        src_enc.name(),
                        dst_enc.name()
                    ),
                ))
            }
        };
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
    // A byte of 0x80 or above is not US-ASCII at all, so it is a
    // malformed sequence rather than a character with no cell — and
    // the fast paths below would otherwise hand it through (#1596).
    // A conversion to its own encoding runs no converter, so CRuby
    // leaves those bytes alone and so does this.
    if src_enc == E::UsAscii
        && src_enc != dst_enc
        && !opts.invalid_replace
        && src_bytes.iter().any(|&b| b >= 0x80)
    {
        return Err(invalid_byte_sequence(store, src_enc, dst_enc, src_bytes));
    }
    // IBM037 is a permutation of Latin-1 with nothing in ASCII's
    // place, so it takes no fast path in either direction: the way in
    // is the table and then UTF-8's own conversion, the way out is
    // UTF-8's own conversion and then the table (#1530).
    if is_ibm037(src_enc) {
        let text = ibm037_to_utf8(src_bytes);
        return transcode_bytes_with_opts(
            text.as_bytes(),
            E::UTF8,
            dst_enc,
            &reporting_as(opts, src_enc),
            store,
        );
    }
    if is_ibm037(dst_enc) {
        // ISO-8859-1's own conversion does the reading, the
        // decorators and the refusals — a malformed source byte is
        // its to report, as CRuby's `convpath` says — and the table
        // is a byte permutation on top (#1584).
        let latin1 = transcode_bytes_with_opts(src_bytes, src_enc, E::Iso8859(1), opts, store)
            .map_err(|e| {
                let (kind, _, _, meta) =
                    stream_convert(src_bytes, src_enc, dst_enc, None, false, opts, store);
                match (kind, meta.message) {
                    (StreamConvertResult::UndefinedConversion, Some(msg)) => {
                        MonorubyErr::undefined_conversion_error(store, msg)
                    }
                    _ => e,
                }
            })?;
        return Ok(latin1_bytes_to_ibm037(&latin1));
    }
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
        // Everything else falls through to the source-table path,
        // which reports the offending byte and honours
        // `undef: :replace` the way every other byte-per-character
        // source does (#1596).
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
        } else if let Some(table) = source_byte_table(src_enc) {
            if opts.undef_replace {
                table_decode_lossy(src_bytes, table, &opts.replace_str(dst_enc))
            } else {
                match table_decode(src_bytes, table) {
                    Ok(s) => s,
                    Err((at, b)) => {
                        encode_hop_first(src_bytes, at, src_enc, dst_enc, opts, store)?;
                        return Err(MonorubyErr::undefined_conversion_error(
                            store,
                            undefined_byte_message(b, src_enc, dst_enc),
                        ));
                    }
                }
            }
        } else if let Some(src_rs) = encoding_to_rs(src_enc) {
            let (decoded, decode_err) = if let Some(fx) = jp_fixup(src_enc) {
                let d = jp_decode(fx, src_bytes, None);
                if let Some(cell) = d.unmapped {
                    encode_hop_first(src_bytes, d.unmapped_at.unwrap_or(0), src_enc, dst_enc, opts, store)?;
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
        } else if src_enc == E::UsAscii && opts.invalid_replace {
            usascii_decode_lossy(src_bytes, &opts.replace_str(dst_enc))
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
                    undefined_char_message(bad, opts.report_src.unwrap_or(src_enc), dst_enc),
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
    } else if let Some(table) = source_byte_table(src_enc) {
        // A table encoding has a character for every byte it assigns
        // one to, so no byte sequence in it is *invalid* — but a cell
        // the encoding leaves unassigned has no character to convert,
        // which is an undefined conversion.
        let decoded = if opts.undef_replace {
            table_decode_lossy(src_bytes, table, &opts.replace_str(dst_enc))
        } else {
            match table_decode(src_bytes, table) {
                Ok(s) => s,
                Err((at, b)) => {
                    encode_hop_first(src_bytes, at, src_enc, dst_enc, opts, store)?;
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_byte_message(b, src_enc, dst_enc),
                    ));
                }
            }
        };
        (std::borrow::Cow::Owned(decoded), false)
    } else if src_enc == E::UsAscii && opts.invalid_replace {
        let out = usascii_decode_lossy(src_bytes, &opts.replace_str(dst_enc));
        (std::borrow::Cow::Owned(out), false)
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
                let msg = if dst_enc == E::UTF8 {
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
                encode_hop_first(src_bytes, d.unmapped_at.unwrap_or(0), src_enc, dst_enc, opts, store)?;
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_cell_message(&cell, opts.report_src.unwrap_or(src_enc), dst_enc),
                ));
            }
            (d.text, d.had_invalid)
        } else {
            let repl = opts.undef_replace.then(|| opts.replace_str(dst_enc));
            let d = cell_decode(src_enc, src_rs, None, src_bytes, repl.as_deref());
            // A malformed run *before* the cell is what CRuby reports,
            // unless it is being substituted: `"\x8A\x8F\xA1"` in Big5
            // is `"\x8A"` followed by `"\x8F"`, and the cell `8F A1`
            // after it, which the table has no character for, comes
            // second.
            let invalid_first = !opts.invalid_replace
                && d.had_invalid
                && d.invalid_at
                    .zip(d.unmapped_at)
                    .is_some_and(|(invalid, unmapped)| invalid < unmapped);
            if let Some(cell) = d.unmapped
                && !invalid_first
            {
                encode_hop_first(src_bytes, d.unmapped_at.unwrap_or(0), src_enc, dst_enc, opts, store)?;
                return Err(MonorubyErr::undefined_conversion_error(
                    store,
                    undefined_cell_message(&cell, opts.report_src.unwrap_or(src_enc), dst_enc),
                ));
            }
            (d.text, d.had_invalid)
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
                    undefined_char_message(bad, opts.report_src.unwrap_or(src_enc), dst_enc),
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
                undefined_char_message(bad, opts.report_src.unwrap_or(src_enc), dst_enc),
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
    // The CJK tables are the same story: the codec writes cells the
    // destination does not have and reads others differently, so the
    // characters go through CRuby's table one at a time (#1544).
    if let Some(tab) = cell_table(dst_enc)
        && let Some(dst_rs) = encoding_to_rs(dst_enc)
    {
        let mut out: Vec<u8> = Vec::with_capacity(decoded.len());
        for c in decoded.chars() {
            match table_cell_encode(tab, dst_rs, c) {
                Some(b) => out.extend_from_slice(&b),
                None if opts.undef_replace => {
                    let replace = opts.replace_str(dst_enc);
                    for r in replace.chars() {
                        if let Some(b) = table_cell_encode(tab, dst_rs, r) {
                            out.extend_from_slice(&b);
                        }
                    }
                }
                None => {
                    return Err(MonorubyErr::undefined_conversion_error(
                        store,
                        undefined_char_message(c, opts.report_src.unwrap_or(src_enc), dst_enc),
                    ));
                }
            }
        }
        return Ok(out);
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
                    undefined_char_message(bad, opts.report_src.unwrap_or(src_enc), dst_enc),
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
    // A cell the codec wrote that the destination cannot hold counts
    // as no cell at all, so the whole output is walked before it is
    // handed back — one linear pass, and only for the encodings that
    // have a walk (#1544).
    let unholdable = !encode_err && !dst_can_hold(dst_enc, &encoded);
    if encode_err || unholdable {
        if !opts.undef_replace {
            // The character the destination cannot write, not merely
            // the first non-ASCII one: a String can hold plenty of
            // non-ASCII the encoder is perfectly happy with.
            let mut buf = [0u8; 4];
            let bad = decoded
                .chars()
                .find(|c| {
                    let (b, _, ce) = dst_rs.encode(c.encode_utf8(&mut buf));
                    ce || !dst_can_hold(dst_enc, &b)
                })
                .unwrap_or('\0');
            return Err(MonorubyErr::undefined_conversion_error(
                store,
                undefined_char_message(bad, opts.report_src.unwrap_or(src_enc), dst_enc),
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
            if ce || !dst_can_hold(dst_enc, &chunk) {
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
    } else if is_encoding_object(globals, arg) {
        let s = globals.store.get_ivar(arg, IdentId::_ENCODING).unwrap();
        s.as_str().to_string()
    } else {
        let s = arg.coerce_to_string(vm, globals)?;
        s
    };
    // A name that stands for a setting names the encoding that setting
    // currently holds; the table below only knows fixed names. CRuby
    // reaches the same place from the other side — the four are
    // ordinary aliases in its encoding table, re-pointed as the
    // settings move — so `"x".encode("locale")` converts to whatever
    // `Encoding.find("locale")` answers, and an unset `"internal"`,
    // whose alias was never registered, is a converter that does not
    // exist rather than a conversion to BINARY (#1575).
    let name = dynamic_alias_name(globals, &name).unwrap_or(name);
    // The converters read a label through `StringValueCStr`: a NUL is
    // reported as such, and as an ArgumentError even on the paths that
    // lift an unknown name to `ConverterNotFoundError`.
    if name.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    known_encoding_name(globals, &name)
        .and_then(|canonical| enc_name_to_const(&canonical))
        .ok_or_else(|| {
        // CRuby raises `Encoding::ConverterNotFoundError` (not
        // ArgumentError) for `String#encode("xyz")` when the
        // label is unknown. The encoding-search path below the
        // call (`Encoding.find`) does still raise ArgumentError;
        // the wrapper at `encode_resolve_enc_arg` lifts it to
        // `ConverterNotFoundError` for the encode path.
        MonorubyErr::argumenterr(format!("unknown encoding name - {}", name))
    })
}

/// The canonical name [`dynamic_alias_object`] resolves `name` to, for
/// the callers that work in names rather than in `Encoding` objects.
/// `None` when `name` is not one of the four, and also when it is
/// `"internal"` with no `default_internal` — the name stands unresolved
/// then, and fails the lookup it is handed to.
fn dynamic_alias_name(globals: &Globals, name: &str) -> Option<String> {
    let v = dynamic_alias_object(globals, name)?;
    encoding_object_name(globals, v)
}

/// `resolve_enc_arg` variant that lifts the unknown-encoding
/// `ArgumentError` to `Encoding::ConverterNotFoundError`,
/// matching CRuby's `String#encode` semantics.
fn resolve_enc_label(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<std::result::Result<&'static str, String>> {
    match resolve_enc_arg(vm, globals, arg) {
        Ok(c) => Ok(Ok(c)),
        // Only the unknown-encoding-name case is a label; other
        // argument errors (TypeError on a non-coercible argument,
        // etc.) pass through unchanged.
        Err(e) if e.message().starts_with("unknown encoding name - ") => Ok(Err(e
            .message()
            .trim_start_matches("unknown encoding name - ")
            .to_string())),
        Err(e) => Err(e),
    }
}

/// The source of a converter request: an argument, or the receiver's
/// own encoding when `String#encode` was given only a destination.
enum ConverterSrc {
    Given(Value),
    Receiver(crate::value::Encoding),
}

/// Resolve a converter request's two encodings together, so that a
/// name that is no encoding's is reported the way CRuby's
/// `rb_econv_open_exc` reports it: `code converter not found (SRC to
/// DST)`, each side spelled by its canonical name when it resolved
/// and as given when it did not, and a side that is empty left out.
/// `String#encode` reads its destination before its source, as CRuby
/// does; `Encoding::Converter` reads them in argument order.
fn resolve_converter_pair(
    vm: &mut Executor,
    globals: &mut Globals,
    src: ConverterSrc,
    dst: Value,
    dst_first: bool,
) -> Result<(crate::value::Encoding, crate::value::Encoding)> {
    let resolve_src = |vm: &mut Executor, globals: &mut Globals| match src {
        ConverterSrc::Given(v) => resolve_enc_label(vm, globals, v),
        ConverterSrc::Receiver(e) => Ok(Ok(encoding_const_name(e))),
    };
    let (src_label, dst_label) = if dst_first {
        let d = resolve_enc_label(vm, globals, dst)?;
        (resolve_src(vm, globals)?, d)
    } else {
        let s = resolve_src(vm, globals)?;
        (s, resolve_enc_label(vm, globals, dst)?)
    };
    let display = |label: &std::result::Result<&'static str, String>| match label {
        Ok(c) => canonical_encoding_name(c).to_string(),
        Err(given) => given.clone(),
    };
    let not_found = |store: &Store| {
        let (s, d) = (display(&src_label), display(&dst_label));
        let desc = if s.is_empty() {
            d
        } else if d.is_empty() {
            s
        } else {
            format!("{} to {}", s, d)
        };
        MonorubyErr::converter_not_found_error(
            store,
            format!("code converter not found ({})", desc),
        )
    };
    match (&src_label, &dst_label) {
        (Ok(s), Ok(d)) => match (
            encoding_from_canonical_name(s),
            encoding_from_canonical_name(d),
        ) {
            (Some(s), Some(d)) => Ok((s, d)),
            _ => Err(not_found(&globals.store)),
        },
        _ => Err(not_found(&globals.store)),
    }
}

/// The constant name (`enc_name_to_const`'s answer) of an `Encoding`,
/// so the receiver's encoding can stand in for a source label.
fn encoding_const_name(enc: crate::value::Encoding) -> &'static str {
    enc_name_to_const(enc.name()).unwrap_or("UTF_8")
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
    // The decorator is work of its own, so a converter is opened even
    // for text that would otherwise pass straight through — and its
    // `replace:` is checked with it (#1566).
    let src_enc = lfp.self_val().as_rstring_inner().encoding();
    let opts = parse_transcode_opts(lfp, &globals.store);
    validate_replacement(&opts, src_enc, dst_enc, Some(mode), &globals.store)?;
    if src_enc != dst_enc && (!has_codec(src_enc) || !has_codec(dst_enc)) {
        return Err(converter_not_found(&globals.store, src_enc, dst_enc, &opts, Some(mode)));
    }
    // In CRuby `xml:` is an output decorator on the ordinary
    // converter, not a path of its own: `invalid:` acts in the
    // transcoder underneath, and the decorator only ever sees what
    // the conversion produced. So the source is read by its own
    // conversion first, with the same `invalid:` handling every other
    // `encode` has — the destination's replacement, or the error —
    // and `undef:` set aside: the decorator's numeric reference is
    // what an undefined character becomes, and a source byte with no
    // Unicode meaning at all stays the error it is (#1615).
    let plain = TranscodeOpts::default();
    let mut reading = opts.clone();
    reading.undef_replace = false;
    if reading.invalid_replace && reading.replace.is_none() {
        reading.replace = Some(opts.replace_str(dst_enc));
    }
    let decoded;
    let s = if src_enc == dst_enc && src_enc.is_ascii_compatible() {
        // No transcoder runs between one encoding and itself, so the
        // bytes — a malformed one included — pass through the
        // decorator untouched. Escaping is byte-wise here.
        let mut out = Vec::with_capacity(bytes.len() + 2);
        if matches!(mode, XmlMode::Attr) {
            out.push(b'"');
        }
        for &b in &bytes {
            match b {
                b'&' => out.extend_from_slice(b"&amp;"),
                b'<' => out.extend_from_slice(b"&lt;"),
                b'>' => out.extend_from_slice(b"&gt;"),
                b'"' if matches!(mode, XmlMode::Attr) => out.extend_from_slice(b"&quot;"),
                _ => out.push(b),
            }
        }
        if matches!(mode, XmlMode::Attr) {
            out.push(b'"');
        }
        return Ok(Some(Value::string_from_inner(
            crate::value::RStringInner::from_encoding_scanned(&out, dst_enc),
        )));
    } else if src_enc == crate::value::Encoding::UTF8 {
        match std::str::from_utf8(&bytes) {
            Ok(_) => String::from_utf8_lossy(&bytes),
            Err(_) if reading.invalid_replace => {
                decoded = transcode_bytes_with_opts(&bytes, src_enc, src_enc, &reading, &globals.store)?;
                String::from_utf8_lossy(&decoded)
            }
            Err(_) => return Err(invalid_byte_sequence(&globals.store, src_enc, dst_enc, &bytes)),
        }
    } else {
        decoded = transcode_bytes_with_opts(
            &bytes,
            src_enc,
            crate::value::Encoding::UTF8,
            &reading,
            &globals.store,
        )?;
        String::from_utf8_lossy(&decoded)
    };
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
            // A character the destination cannot represent becomes an
            // upper-case hexadecimal numeric character reference — the
            // whole point of the xml decorator.
            _ if !c.is_ascii()
                && transcode_bytes_with_opts(
                    c.to_string().as_bytes(),
                    crate::value::Encoding::UTF8,
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
        crate::value::Encoding::UTF8,
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


/// Look up the current `Encoding.default_internal` (set via
/// `Encoding.default_internal=`). Returns `None` when unset
/// (matches CRuby's "no transcoding" default).
pub(super) fn current_default_internal(globals: &mut Globals) -> Option<crate::value::Encoding> {
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
    let Some(arg0) = lfp.try_arg(0) else {
        return Ok((self_enc, current_default_internal(globals)));
    };
    let src = match lfp.try_arg(1) {
        Some(arg1) => ConverterSrc::Given(arg1),
        None => ConverterSrc::Receiver(self_enc),
    };
    let (src, dst) = resolve_converter_pair(vm, globals, src, arg0, true)?;
    Ok((src, Some(dst)))
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
    // `rb_econv_open` comes first: a pair no converter serves is refused
    // before the input is looked at, so a UTF-7 source (or a broken one
    // bound for UTF-7) is `ConverterNotFoundError`, never a complaint
    // about its bytes.
    refuse_pair_without_converter(src_enc, dst_enc, &globals.store)?;
    if let Some(v) = handle_xml_option(globals, lfp, dst_enc)? {
        return Ok(v);
    }
    let opts = parse_transcode_opts(lfp, &globals.store);
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
    refuse_pair_without_converter(src_enc, dst_enc, &globals.store)?;
    if let Some(v) = handle_xml_option(globals, lfp, dst_enc)? {
        // CRuby's `encode!` just `replace`s self with the encoded
        // form when xml is given.
        if let Some(inner) = v.is_rstring_inner() {
            self_val.replace_with_inner(inner.clone());
        }
        return Ok(self_val);
    }
    let opts = parse_transcode_opts(lfp, &globals.store);
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
/// The `replace:` option as characters, whatever encoding it was
/// given in.
///
/// CRuby converts the replacement into the destination as soon as it
/// has one, so a replacement written in the source's encoding — or in
/// the destination's — is as good as one written in UTF-8. Reading it
/// through `is_str`, which answers only for valid UTF-8, dropped
/// those silently and left the default `"?"` in their place (#1583).
/// The pipeline here carries the replacement as text and converts it
/// with everything else, so this is that conversion one step earlier.
fn replacement_text(v: Value, store: &Store) -> Option<String> {
    let inner = v.is_rstring_inner()?;
    let bytes = inner.as_bytes();
    if let Ok(s) = std::str::from_utf8(bytes)
        && (bytes.is_ascii() || inner.encoding().is_utf8_compatible())
    {
        return Some(s.to_string());
    }
    let utf8 = transcode_bytes_with_opts(
        bytes,
        inner.encoding(),
        crate::value::Encoding::UTF8,
        &TranscodeOpts::default(),
        store,
    )
    .ok()?;
    String::from_utf8(utf8).ok()
}

/// The bytes a same-encoding `invalid: :replace` substitutes, which
/// is `String#scrub`'s replacement and follows its rule: it has to be
/// *compatible with the receiver*, not spellable in some destination,
/// and what goes into the string is its own bytes (#1599).
///
/// An ASCII-only replacement suits any ASCII-compatible receiver, and
/// one already in the receiver's encoding suits it too; anything else
/// is what `rb_enc_check` refuses.
fn scrub_replacement_bytes(
    opts: &TranscodeOpts,
    enc: crate::value::Encoding,
    store: &Store,
) -> Result<Vec<u8>> {
    let text = opts.replace_str(enc);
    let Some(repl_enc) = opts.replace_enc else {
        // The default replacement is the encoding's own.
        return Ok(text.into_bytes());
    };
    if repl_enc != enc && !(text.is_ascii() && enc.is_ascii_compatible()) {
        return Err(MonorubyErr::incompatible_encoding(store, enc, repl_enc));
    }
    transcode_bytes_with_opts(
        text.as_bytes(),
        crate::value::Encoding::UTF8,
        enc,
        &TranscodeOpts::default(),
        store,
    )
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
fn replacement_in(
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
        converted =
            transcode_bytes_with_opts(s.as_bytes(), crate::value::Encoding::UTF8, enc, &TranscodeOpts::default(), store)
                .unwrap_or_else(|_| s.as_bytes().to_vec());
        &converted
    };
    crate::value::RStringInner::from_encoding_scanned(bytes, enc)
}

fn parse_transcode_opts(lfp: Lfp, store: &Store) -> TranscodeOpts {
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
        if let Some(s) = replacement_text(v, store) {
            out.replace = Some(s);
            out.replace_enc = v.is_rstring_inner().map(|i| i.encoding());
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
        crate::value::Encoding::UTF8,
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
            crate::value::Encoding::UTF8,
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
        Encoding::Utf8(i) => crate::value::utf8_const_name(i),
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
        Encoding::EucJp(i) => crate::value::euc_jp_const_name(i),
        Encoding::Sjis(i) => crate::value::sjis_const_name(i),
        Encoding::Iso2022Jp => "ISO_2022_JP",
        // Name-preserving byte encodings: map the canonical display
        // name back to its registered `Encoding::<CONST>` identifier.
        Encoding::Other(i) => match i {
            0 => "UTF_7",
            1 => "CP50220",
            2 => "CP50221",
            3 => "UTF_16",
            4 => "UTF_32",
            5 => "EBCDIC_CP_US",
            6 => "ISO_2022_JP_2",
            7 => "ISO_2022_JP_KDDI",
            // Every index in `OTHER_ENC_NAMES` needs an arm above: the
            // fallback renders the encoding as ASCII-8BIT, which is
            // how an addition to that table goes wrong quietly.
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
    match dynamic_alias_object(globals, name) {
        Some(value) => globals.encoding_of_object(value).or(Some(Encoding::UTF8)),
        // The one place the four are not resolved alike: an unset
        // `"internal"` names no encoding, and `rb_to_encoding` answers
        // BINARY for it where `Encoding.find` answers nil and the
        // converters call the name unknown.
        None if name.eq_ignore_ascii_case("internal") => Some(Encoding::Ascii8),
        None => None,
    }
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
        // `rb_to_encoding` reads the name through `StringValueCStr`, so
        // an embedded NUL is its own error rather than an unknown name.
        if s.as_bytes().contains(&0) {
            return Err(MonorubyErr::argumenterr(
                "invalid encoding name (NUL byte)",
            ));
        }
        // `try_from_str` folds separators and case, so it is the name's
        // presence in `Encoding.name_list` that decides whether the
        // name is one at all ("utf8" is not).
        let canonical = known_encoding_name(globals, s).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("unknown encoding name - {}", s))
        })?;
        Encoding::try_from_str(&canonical)
    } else if let Some(enc) = globals.encoding_of_object(arg0) {
        // An `Encoding::<NAME>` constant object: its `Encoding` was
        // recorded at init, so no name is read or parsed.
        Ok(enc)
    } else if is_encoding_object(globals, arg0) {
        let s = globals.store.get_ivar(arg0, IdentId::_ENCODING).unwrap();
        Encoding::try_from_str(s.as_str())
    } else {
        // Try to_str coercion
        let s = arg0.coerce_to_string(vm, globals)?;
        if let Some(enc) = special_encoding_name(globals, &s) {
            return Ok(enc);
        }
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
    enc_class.expect_class_or_module(&globals.store)?;
    if is_encoding_object(globals, val) {
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
/// The encoding an `#inspect` answer will be read in: what
/// `Encoding.default_internal` or `.default_external` says, UTF-8 when
/// neither does. CRuby's `rb_reg_desc` compares a pattern against it
/// to decide what to escape (#1516).
pub(crate) fn inspect_result_encoding(globals: &mut Globals) -> Encoding {
    inspect_escape_encoding(globals).unwrap_or(Encoding::UTF8)
}

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
        Encoding::Utf8(_) => None,
        e if !e.is_ascii_compatible() => Some(Encoding::UsAscii),
        e => Some(e),
    }
}

/// [`inspect_result`] for a rendering CRuby builds with `rb_sprintf`
/// instead of a string buffer of its own: the `#<…>` form an object
/// falls back on when it has nothing better to say about itself.
/// `rb_sprintf` starts its buffer with *no* encoding, so a result that
/// came out all-ASCII is tagged ASCII-8BIT rather than the result
/// encoding, and one carrying a non-ASCII name or message keeps that
/// text's own encoding (#1494).
///
/// The escaping is still `inspect_result`'s: CRuby escapes the values
/// such a rendering embeds, through `rb_inspect`, and monoruby escapes
/// the rendering as a whole, which for an embedded value comes to the
/// same text.
pub(crate) fn sprintf_result(globals: &mut Globals, s: String) -> Value {
    let text = if globals.store.inspect_escape() && !s.is_ascii() {
        crate::value::escape_nonascii_to_u(&s)
    } else {
        s
    };
    Value::string_sprintf(text)
}

/// A value's `#inspect` as it goes *into* a `#<…>` rendering. CRuby
/// reaches it through `rb_inspect`, which escapes to `\uXXXX` when the
/// result encoding cannot show the answer — the rendering around it is
/// not escaped, only what it embeds (#1494).
pub(crate) fn inspect_embedded(globals: &Globals, s: String) -> String {
    if globals.store.inspect_escape() && !s.is_ascii() {
        crate::value::escape_nonascii_to_u(&s)
    } else {
        s
    }
}

/// [`inspect_result`] where only a `#<…>` rendering is one of
/// `rb_sprintf`'s: the generic `#inspect`, which sees every kind of
/// object and so meets both kinds of rendering.
pub(crate) fn inspect_or_sprintf_result(globals: &mut Globals, s: String) -> Value {
    if s.starts_with("#<") {
        sprintf_result(globals, s)
    } else {
        inspect_result(globals, s)
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
/// Output bytes a `dst_bytesize` cap held back mid-character, for the
/// next call to write before it converts anything new (#1532).
const CONVERTER_PENDING_OUT_IVAR: &str = "/converter_pending_out";
/// Read-again bytes buffered by an `:invalid_byte_sequence` outcome,
/// returned (and drained) by `Encoding::Converter#putback`.
const CONVERTER_READAGAIN_IVAR: &str = "/converter_readagain";
/// Structured data of the last conversion error (for `#last_error` /
/// the error-object attribute readers): `[kind, msg, error_bytes,
/// readagain_bytes, stage_src, stage_dst]`, or absent/nil when the
/// last outcome was not an error.
const CONVERTER_LAST_ERROR_IVAR: &str = "/converter_last_error";

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
fn converter_is_stateful(src: crate::value::Encoding, dst: crate::value::Encoding) -> bool {
    dummy_wide_target(src).is_some()
        || dummy_wide_target(dst).is_some()
        || src == crate::value::Encoding::Utf8(crate::value::UTF8_MAC)
        || jis_wrapper(src).is_some()
        || jis_wrapper(dst).is_some()
}

/// The bytes at the end of `input` that begin a character the next
/// chunk may finish — what a converter holds back rather than
/// converts, in replacement mode as on every other path (#1592).
fn incomplete_tail_len(src: crate::value::Encoding, input: &[u8]) -> usize {
    use crate::value::Encoding as E;
    if is_utf16_or_32(src) {
        // A partial coding unit, and for UTF-16 a high surrogate
        // waiting for its pair.
        let unit = if matches!(src, E::Utf16Le | E::Utf16Be) { 2 } else { 4 };
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
    let Some((_, precise)) = conversion_walker(src).or_else(|| crate::value::mbc_walker(src)) else {
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

fn converter_take_readagain(globals: &mut Globals, recv: Value) -> Vec<u8> {
    let ra_id = IdentId::get_id(CONVERTER_READAGAIN_IVAR);
    let bytes = globals
        .store
        .get_ivar(recv, ra_id)
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    if !bytes.is_empty() {
        let _ = globals.store.set_ivar(recv, ra_id, Value::nil());
    }
    bytes
}
/// Conversion flags configured at construction (`invalid: :replace`
/// / `undef: :replace` kwargs, or the `INVALID_REPLACE` /
/// `UNDEF_REPLACE` Integer-flag bits). Stored as a Fixnum:
/// bit0 = invalid→replace, bit1 = undef→replace.
const CONVERTER_FLAGS_IVAR: &str = "/converter_flags";

/// The concrete endianness the source stream's BOM named, once one
/// has been seen. The dummy `UTF-16` / `UTF-32` carry it in the first
/// chunk only, so the chunks after it have to be told (#1576).
const CONVERTER_SRC_BOM_IVAR: &str = "/converter_src_bom";

/// Set once the destination's BOM has been written. The dummies write
/// one, and only ahead of the first character they emit (#1576).
const CONVERTER_DST_BOM_IVAR: &str = "/converter_dst_bom";
/// The ISO-2022-JP designation in effect, as a small integer: the
/// escape stays in effect across `#convert` calls, and the closing
/// one is written from `#finish` (#1609).
const CONVERTER_ISO_STATE_IVAR: &str = "/converter_iso_state";
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
        // Stored as the destination spells it (#1583); the pipeline
        // wants it as characters, and reads it back the same way it
        // reads the option.
        && let Some(s) = replacement_text(v, &globals.store)
    {
        opts.replace = Some(s);
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

/// Whether a conversion opens a converter at all, which is when
/// CRuby validates the `replace:` string (#1566). `String#encode`
/// hands the bytes straight back when the source and destination
/// agree, and when both are ASCII-compatible and the source is
/// 7-bit — and a replacement it never had to look at goes
/// unexamined. A decorator is work of its own, so it opens one
/// whatever the source looks like.
fn opens_a_converter(
    src_bytes: &[u8],
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    xml: bool,
) -> bool {
    if src_enc == dst_enc {
        return false;
    }
    if opts.has_newline() || xml {
        return true;
    }
    !(src_enc.is_ascii_compatible() && dst_enc.is_ascii_compatible() && src_bytes.is_ascii())
}

/// The decorators CRuby names after the encodings when it describes a
/// converter, in its order.
fn decorator_names(opts: &TranscodeOpts, xml: Option<XmlMode>) -> Vec<&'static str> {
    let mut out = vec![];
    if opts.universal_newline {
        out.push("universal_newline");
    }
    if opts.crlf_newline {
        out.push("crlf_newline");
    }
    if opts.cr_newline {
        out.push("cr_newline");
    }
    match xml {
        Some(XmlMode::Text) => out.push("xml_text"),
        Some(XmlMode::Attr) => {
            out.push("xml_attr_content");
            out.push("xml_attr_quote");
        }
        None => {}
    }
    out
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
fn validate_replacement(
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
fn converter_not_found(
    store: &Store,
    src_enc: crate::value::Encoding,
    dst_enc: crate::value::Encoding,
    opts: &TranscodeOpts,
    xml: Option<XmlMode>,
) -> MonorubyErr {
    let decorators = decorator_names(opts, xml);
    let with = if decorators.is_empty() {
        String::new()
    } else {
        format!(" with {}", decorators.join(","))
    };
    MonorubyErr::converter_not_found_error(
        store,
        format!(
            "code converter not found ({} to {}{})",
            src_enc.name(),
            dst_enc.name(),
            with
        ),
    )
}

/// Whether monoruby can convert to and from `enc`.
///
/// `Encoding::Converter` converts a chunk at a time, so this is not
/// quite the same question `String#encode` asks: the pivot wrappers
/// need an arm in `stream_convert` and the endianness-less dummies
/// need their BOM remembered across calls. Both are there now, so the
/// two entry points agree on the whole set (#1576).
fn has_codec(enc: crate::value::Encoding) -> bool {
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
fn refuse_pair_without_converter(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    store: &Store,
) -> Result<()> {
    if src == dst {
        return Ok(());
    }
    let missing = |e: crate::value::Encoding| is_cruby_dummy_name(e.name()) && !has_codec(e);
    if missing(src) || missing(dst) {
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

fn validate_converter_pair(
    src: crate::value::Encoding,
    dst: crate::value::Encoding,
    decorators: &TranscodeOpts,
    store: &Store,
) -> Result<()> {
    if src == dst {
        return Ok(());
    }
    let src_supported = has_codec(src);
    let dst_supported = has_codec(dst);
    if !src_supported || !dst_supported {
        return Err(converter_not_found(store, src, dst, decorators, None));
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


/// The encoding a chunk of this converter's source is to be read as.
///
/// The dummy `UTF-16` / `UTF-32` carry their BOM in the first chunk
/// only. That chunk stays the dummy — `stream_convert` reads the BOM
/// off it and converts the rest — and the byte order it named is
/// remembered here, so every chunk after it is read as the concrete
/// encoding (#1576). Any other source is already settled and answers
/// itself.
fn converter_resolve_src_bom(
    globals: &mut Globals,
    recv: Value,
    src_enc: crate::value::Encoding,
    input: &[u8],
) -> crate::value::Encoding {
    if dummy_wide_target(src_enc).is_none() {
        return src_enc;
    }
    let id = IdentId::get_id(CONVERTER_SRC_BOM_IVAR);
    if let Some(enc) = globals
        .store
        .get_ivar(recv, id)
        .and_then(|v| v.is_str().map(|s| s.to_string()))
        .and_then(|s| crate::value::Encoding::try_from_str(&s).ok())
    {
        return enc;
    }
    if let Some((enc, _)) = dummy_wide_source(src_enc, input) {
        let _ = globals
            .store
            .set_ivar(recv, id, Value::string_from_str(enc.name()));
    }
    // Either way this chunk is the one the BOM is in, or the one that
    // shows there is none; the dummy is what reads it.
    src_enc
}

/// The destination's own BOM. The endianness-less dummies write one
/// ahead of the first character they emit; everything else writes
/// none (#1576).
fn converter_dst_bom(dst_enc: crate::value::Encoding) -> Vec<u8> {
    match dummy_wide_target(dst_enc) {
        Some(crate::value::Encoding::Utf16Be) => vec![0xFE, 0xFF],
        Some(_) => vec![0x00, 0x00, 0xFE, 0xFF],
        None => vec![],
    }
}

/// The ISO-2022-JP designation this converter is in the middle of.
fn converter_iso_state(globals: &Globals, recv: Value) -> Option<u8> {
    globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_ISO_STATE_IVAR))
        .and_then(|v| v.try_fixnum())
        .map(|n| n as u8)
}

fn converter_set_iso_state(globals: &mut Globals, recv: Value, state: Option<u8>) {
    let _ = globals.store.set_ivar(
        recv,
        IdentId::get_id(CONVERTER_ISO_STATE_IVAR),
        match state {
            Some(b) => Value::integer(b as i64),
            None => Value::nil(),
        },
    );
}

/// Whether this converter still owes its destination a BOM.
fn converter_dst_bom_owed(globals: &Globals, recv: Value, dst_enc: crate::value::Encoding) -> bool {
    dummy_wide_target(dst_enc).is_some()
        && globals
            .store
            .get_ivar(recv, IdentId::get_id(CONVERTER_DST_BOM_IVAR))
            .is_none()
}

/// The BOM to put in front of `out`, marking it written. A call that
/// emits nothing writes no BOM either, which is how CRuby answers an
/// empty chunk and a `#finish` that had nothing held.
fn converter_take_dst_bom(
    globals: &mut Globals,
    recv: Value,
    dst_enc: crate::value::Encoding,
) -> Vec<u8> {
    if !converter_dst_bom_owed(globals, recv, dst_enc) {
        return vec![];
    }
    let _ = globals.store.set_ivar(
        recv,
        IdentId::get_id(CONVERTER_DST_BOM_IVAR),
        Value::bool(true),
    );
    converter_dst_bom(dst_enc)
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
/// when none has been set explicitly.
///
/// CRuby keeps the replacement in the encoding it will be *inserted*
/// in, not in the destination, and for every destination that takes
/// `U+FFFD` at all that encoding is UTF-8 — so `Converter.new("UTF-8",
/// "UTF-16BE").replacement` is the three UTF-8 bytes tagged UTF-8, not
/// the two UTF-16BE ones. Everything else takes `"?"` as US-ASCII.
/// See `inserted_replacement` for which encodings take which, and
/// `insert_encoding` for which encoding is asked (#1571, #1577).
fn converter_default_replacement(insert_enc: crate::value::Encoding) -> Value {
    let text = inserted_replacement(insert_enc);
    let enc = if text == "?" {
        crate::value::Encoding::UsAscii
    } else {
        crate::value::Encoding::UTF8
    };
    let mut s = crate::value::RStringInner::from_string_scanned(text.to_string());
    s.set_encoding(enc);
    Value::string_from_inner(s)
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
    let (src, dst) = resolve_converter_pair(
        vm,
        globals,
        ConverterSrc::Given(lfp.arg(0)),
        lfp.arg(1),
        false,
    )?;
    // The decorators the message names, read ahead of the options
    // proper: `code converter not found (UTF-8 to Windows-1258 with
    // crlf_newline)` (#1591).
    let mut decorators = TranscodeOpts::default();
    if let Some(n) = lfp.try_arg(2).and_then(|v| v.try_fixnum()) {
        decorators.universal_newline = n & 0x0000_0100 != 0;
        decorators.crlf_newline = n & 0x0000_1000 != 0;
        decorators.cr_newline = n & 0x0000_2000 != 0;
    }
    if let Some(hash) = (2..=3)
        .filter_map(|i| lfp.try_arg(i))
        .find_map(|v| v.try_hash_ty())
    {
        let on = |key: &str| find_hash_value_for_symbol(&hash, key).is_some_and(|v| v.as_bool());
        decorators.universal_newline |= on("universal_newline");
        decorators.crlf_newline |= on("crlf_newline");
        decorators.cr_newline |= on("cr_newline");
    }
    // CRuby raises `Encoding::ConverterNotFoundError` for identical
    // source/destination encodings — there is no "X to X" transcoder,
    // and a decorator does not make one: `(UTF-8 to UTF-8 with
    // universal_newline)` is refused with the decorator named (#1589).
    // Compare the resolved canonical *names*, not the internal
    // `Encoding`: monoruby folds aliases like `UTF8-MAC` onto
    // `Utf8`, but CRuby treats them as distinct and DOES build a
    // converter (`Converter.new(UTF_8, UTF8_MAC)` is valid).
    if src.name() == dst.name() {
        return Err(converter_not_found(&globals.store, src, dst, &decorators, None));
    }
    validate_converter_pair(src, dst, &decorators, &globals.store)?;
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
                    let s = match replacement_text(rep, &globals.store) {
                        Some(s) => s,
                        // Not a String at all: CRuby coerces via
                        // `#to_str`, which re-enters Ruby.
                        None => {
                            let coerced = rep.coerce_to_string(vm, globals)?;
                            coerced
                        }
                    };
                    // A converter is always opened here, so the
                    // replacement is checked against the destination
                    // now rather than at the first substitution
                    // (#1566). The flags carry the decorators the
                    // message names.
                    let decorators = TranscodeOpts {
                        universal_newline: flags & 0x0000_0100 != 0,
                        crlf_newline: flags & 0x0000_1000 != 0,
                        cr_newline: flags & 0x0000_2000 != 0,
                        replace: Some(s.clone()),
                        ..Default::default()
                    };
                    // The check converts the replacement into the
                    // destination, which is also what `#replacement`
                    // hands back — so the bytes are kept rather than
                    // tagging the UTF-8 ones with the destination's
                    // name (#1583).
                    let in_dst =
                        validate_replacement(&decorators, src, dst, None, &globals.store)?
                            .unwrap_or_default();
                    replace_inner = Some(replacement_in(&s, &in_dst, dst, &globals.store));
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
    let src = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    Ok(converter_default_replacement(insert_encoding(src, dst)))
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
    let recv = lfp.self_val();
    let dst = converter_get_dst(globals, recv);
    let s = replacement_text(arg, &globals.store)
        .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion into String".to_string()))?;
    let s = s.as_str();
    // The new replacement has to be one the destination can spell.
    // Asking the pipeline rather than the codec is what makes this
    // agree with `#encode` — the codec's repertoire is wider than
    // several of these encodings (#1544) — and CRuby names neither
    // the character nor the pair here (#1566).
    let Ok(in_dst) = transcode_bytes_with_opts(
        s.as_bytes(),
        crate::value::Encoding::UTF8,
        dst,
        &TranscodeOpts::default(),
        &globals.store,
    ) else {
        return Err(MonorubyErr::undefined_conversion_error(
            &globals.store,
            "replacement character setup failed".to_string(),
        ));
    };
    // Those are the bytes `#replacement` hands back (#1583).
    let val = Value::string_from_inner(replacement_in(s, &in_dst, dst, &globals.store));
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
    // The argument is converted before the converter's state is
    // looked at, as CRuby's `StringValue` is reached first: a
    // finished converter handed a non-String still answers the
    // `TypeError` (#1537).
    let arg = lfp.arg(0);
    let bytes = arg
        .is_rstring_inner()
        .ok_or_else(|| {
            MonorubyErr::no_implicit_conversion(&globals.store, arg, STRING_CLASS)
        })?
        .as_bytes()
        .to_vec();
    if globals
        .store
        .get_ivar(recv, IdentId::get_id(CONVERTER_FINISHED_IVAR))
        .is_some()
    {
        return Err(MonorubyErr::argumenterr(
            "converter already finished".to_string(),
        ));
    }
    let src = converter_get_src(globals, recv);
    let dst = converter_get_dst(globals, recv);
    // Honour the configured replacement + `invalid:`/`undef:
    // :replace` flags (a user-set `#replacement=` too).
    let opts = converter_transcode_opts(globals, recv);
    // Some pairs carry state across calls, which a single-shot
    // transcode has no way to keep: the endianness-less dummies carry
    // a BOM, read once on the source side and written once on the
    // destination side, and a `UTF8-MAC` source holds its trailing
    // cluster back. Those go the streamed way whatever the flags say
    // (#1576).
    let stateful = converter_is_stateful(src, dst);
    let pending_id = IdentId::get_id(CONVERTER_PENDING_IVAR);
    if (opts.invalid_replace || opts.undef_replace) && !stateful {
        // Replacement mode cannot error on content — the single-shot
        // transcoder suffices — but a chunk that ends inside a
        // character is still held for the next one to finish, as it
        // is on every other path (#1592).
        let mut input: Vec<u8> = converter_take_readagain(globals, recv);
        input.extend(
            globals
                .store
                .get_ivar(recv, pending_id)
                .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
                .unwrap_or_default(),
        );
        input.extend_from_slice(&bytes);
        let tail = incomplete_tail_len(src, &input);
        let head = &input[..input.len() - tail];
        let out = transcode_bytes_with_opts(head, src, dst, &opts, &globals.store)?;
        if tail == 0 {
            let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
        } else {
            let mut held =
                crate::value::RStringInner::from_encoding_scanned(&input[input.len() - tail..], src);
            held.set_encoding(crate::value::Encoding::Ascii8);
            let _ = globals.store.set_ivar(recv, pending_id, Value::string_from_inner(held));
        }
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
    let mut input: Vec<u8> = converter_take_readagain(globals, recv);
    input.extend(
        globals
            .store
            .get_ivar(recv, pending_id)
            .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
            .unwrap_or_default(),
    );
    input.extend_from_slice(&bytes);
    // A dummy `UTF-16` / `UTF-32` source is read in whatever
    // endianness its BOM named; until one has arrived whole there is
    // nothing to read the chunk as (#1576).
    // The encodings the conversion runs in. They differ from the
    // converter's own only for the dummies, which stay the names every
    // error message uses — CRuby reports "from UTF-16", not from
    // whichever end its BOM turned out to name.
    let src_stream = converter_resolve_src_bom(globals, recv, src, &input);
    // The destination's: a dummy writes its BOM here, once, and then
    // the big-endian form CRuby writes.
    let dst_stream = dummy_wide_target(dst).unwrap_or(dst);
    let mut opts = opts;
    opts.iso_state = converter_iso_state(globals, recv);
    let (result, consumed, out, meta) =
        stream_convert(&input, src_stream, dst_stream, None, true, &opts, &globals.store);
    converter_set_iso_state(globals, recv, meta.iso_state_out);
    let out = if out.is_empty() {
        out
    } else {
        let mut bom = converter_take_dst_bom(globals, recv, dst);
        bom.extend_from_slice(&out);
        bom
    };
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
/// Marks the converter as drained and returns the trailing bytes:
/// empty for the `encoding_rs` transcoders, which are stateless, and
/// whatever a `UTF8-MAC` source held back for the composition that
/// never came (#1576). Subsequent `convert` calls raise
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
    let mut pending: Vec<u8> = converter_take_readagain(globals, recv);
    pending.extend(
        globals
            .store
            .get_ivar(recv, pending_id)
            .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
            .unwrap_or_default(),
    );
    let src_stream = converter_resolve_src_bom(globals, recv, src_enc, &pending);
    let dst_stream = dummy_wide_target(dst).unwrap_or(dst);
    let mut out: Vec<u8> = vec![];
    if !pending.is_empty() {
        let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
        // The end of the input settles what a chunk could not: a
        // cluster a `UTF8-MAC` source was holding in case a mark
        // followed it now converts on its own — and can turn out to
        // have no cell in the destination, which is that error and not
        // an incomplete one. Only a tail that is still half a
        // character is incomplete input.
        let mut opts = converter_transcode_opts(globals, recv);
        opts.iso_state = converter_iso_state(globals, recv);
        let (result, consumed, flushed, meta) = if opts.invalid_replace
            && !converter_is_stateful(src_stream, dst_stream)
        {
            // In replacement mode what is still half a character is
            // replaced, not reported (#1592).
            let out = transcode_bytes_with_opts(&pending, src_stream, dst_stream, &opts, &globals.store)?;
            (StreamConvertResult::Finished, pending.len(), out, ErrMeta::default())
        } else {
            stream_convert(&pending, src_stream, dst_stream, None, false, &opts, &globals.store)
        };
        converter_set_iso_state(globals, recv, meta.iso_state_out);
        if matches!(result, StreamConvertResult::Finished) && consumed == pending.len() {
            out = flushed;
        } else {
            let (kind, meta) = match result {
                StreamConvertResult::UndefinedConversion
                | StreamConvertResult::InvalidByteSequence => (result, meta),
                // The flush names the bytes that are still half a
                // character, which is what CRuby reports — and not the
                // same thing as the whole pending buffer, since a
                // `UTF8-MAC` source can have a settled cluster held in
                // front of them. Fall back to the buffer for a flush
                // that named nothing.
                _ if !meta.error_bytes.is_empty() => {
                    (StreamConvertResult::IncompleteInput, meta)
                }
                _ => (
                    StreamConvertResult::IncompleteInput,
                    ErrMeta {
                        error_bytes: pending,
                        readagain_bytes: vec![],
                        ..ErrMeta::default()
                    },
                ),
            };
            let msg = store_conversion_outcome(globals, recv, kind, &meta, src_enc, dst)
                .unwrap_or_default();
            return Err(converter_last_error_raise(globals, recv, kind, msg));
        }
    }
    // The end of the input is where ISO-2022-JP designates ASCII
    // again, and CRuby writes that escape from `#finish` whether or
    // not anything was held back (#1609).
    if jis_wrapper(dst).is_some()
        && converter_iso_state(globals, recv).is_some()
    {
        out.extend_from_slice(b"\x1b(B");
        converter_set_iso_state(globals, recv, None);
    }
    if !out.is_empty() {
        let mut bom = converter_take_dst_bom(globals, recv, dst);
        bom.extend_from_slice(&out);
        out = bom;
    }
    // The end of the stream is an outcome of its own: `primitive_errinfo`
    // answers `:finished` after it, not the error a previous call
    // reported and already consumed.
    store_conversion_outcome(
        globals,
        recv,
        StreamConvertResult::Finished,
        &ErrMeta::default(),
        src_enc,
        dst,
    );
    Ok(Value::string_from_inner(
        crate::value::RStringInner::from_encoding_scanned(&out, dst),
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
    Ok(Value::string_sprintf(format!(
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
    /// The message and the errinfo stage names a wrapper settled for an
    /// error its inner conversion reported, when the inner encoding's
    /// own wording would name the wrong hop (#1520).
    message: Option<String>,
    stage: Option<(String, String)>,
    /// The bytes that are definitely part of the erroneous sequence.
    error_bytes: Vec<u8>,
    /// Bytes consumed while detecting the error that should be
    /// re-examined (CRuby's read-again bytes).
    readagain_bytes: Vec<u8>,
    /// `:destination_buffer_full` only: source bytes *past* the
    /// returned consumed count that CRuby additionally reports as
    /// consumed. Its transcoder reads the character it could not
    /// fit and buffers that character's output, so a destination
    /// capped at 2 bytes takes `"\u3042a"` out of `"\u3042abcd"`
    /// while writing only the first character (#1511). We have no
    /// output buffer, so the caller re-converts those bytes next
    /// call instead — but it must still take them out of the
    /// user's `src` to match what CRuby leaves there.
    dst_full_extra: usize,
    /// `:destination_buffer_full` only: the output bytes of that
    /// character which did not fit. CRuby fills the destination to
    /// the byte and holds the rest for the next call, so a cap of 1
    /// takes `"\u3042"` apart into `A4` and `A2` (#1532). When this
    /// is set the character is *not* re-converted next call — these
    /// bytes are written instead.
    dst_full_out: Vec<u8>,
    /// The ISO-2022-JP designation this chunk leaves in effect, for
    /// the next one to carry on from (#1609).
    iso_state_out: Option<u8>,
    /// `:undefined_conversion` only: the character had no mapping in
    /// the *decode* half (a source byte with no Unicode meaning),
    /// not the encode half. The two stages are reported differently
    /// — `error_bytes` are raw source bytes rather than the UTF-8
    /// pivot, and the stage pair is `[source, "UTF-8"]` (#1511).
    decode_stage: bool,
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
        let (_, _, out, _) = stream_convert(
            &src_bytes[..n],
            src_enc,
            pivot_enc,
            None,
            true,
            opts,
            store,
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
fn mac_held_len(src_enc: crate::value::Encoding, bytes: &[u8]) -> usize {
    if src_enc != crate::value::Encoding::Utf8(crate::value::UTF8_MAC) {
        return 0;
    }
    let Ok(s) = std::str::from_utf8(bytes) else {
        return 0;
    };
    crate::value::mac_clusters(s)
        .last()
        .filter(|r| s[r.start..r.end].chars().next().is_some_and(|c| (c as u32) < 0x10000))
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
fn mac_replacement_leads(dst_enc: crate::value::Encoding) -> bool {
    dst_enc != crate::value::Encoding::UTF8
}

/// A broken `UTF8-MAC` source under `invalid: :replace`, composed into
/// the pivot with every ill-formed subpart replaced.
struct MacReplaced {
    /// The composed pivot, replacements and all.
    pivot: String,
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
fn mac_replace_pivot(
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
        // own — hold-back included, or a chunk that ends mid-character
        // would settle the cluster before it and lose the composition
        // the next chunk was going to complete.
        let (_, consumed, out, _) =
            mac_source_stream(&src_bytes[..good], dst_enc, max_dst_bytes, partial_input, opts, store);
        let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
        return (kind, consumed, out, meta);
    }
    let s = std::str::from_utf8(src_bytes).expect("checked just above");
    // Only a character a mark can attach to is worth keeping, and in
    // CRuby's table that is the Basic Multilingual Plane: an astral
    // character at the end of a chunk goes out with the rest of it.
    let held = crate::value::mac_clusters(s)
        .last()
        .filter(|r| s[r.start..r.end].chars().next().is_some_and(|c| (c as u32) < 0x10000))
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
    let result = if composed.cut < src_bytes.len() && matches!(result, StreamConvertResult::Finished)
    {
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
        let (_, consumed, out, _) =
            cesu_source_stream(&bytes[..good], dst_enc, max_dst_bytes, partial_input, opts, store);
        let (kind, meta) = bad_source_outcome(src_enc, bytes, !partial_input);
        return (kind, consumed, out, meta);
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
fn stateless_good_prefix(bytes: &[u8]) -> usize {
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
    let eucjp = crate::value::stateless_iso2022jp_to_eucjp(&bytes[..good])
        .unwrap_or_else(|_| Vec::new());
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
    let (kind, consumed, out, mut meta) =
        stream_convert(src_bytes, src_enc, E::Iso8859(1), max_dst_bytes, partial_input, opts, store);
    let out = latin1_bytes_to_ibm037(&out);
    meta.dst_full_out = latin1_bytes_to_ibm037(&meta.dst_full_out);
    if matches!(kind, StreamConvertResult::UndefinedConversion)
        && !meta.decode_stage
        && meta.message.is_none()
        && let Some(c) = single_utf8_char(&meta.error_bytes)
    {
        meta.stage = Some(("UTF-8".to_string(), "ISO-8859-1".to_string()));
        meta.message = Some(ibm037_undefined_message(c, opts.report_src.unwrap_or(src_enc)));
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
        let chain = jis_family_chain(named, dst_enc)
            .unwrap_or_else(|| vec![named, kddi, E::UTF8, dst_enc]);
        let names = chain_names(&chain);
        // What the carrier's stream refused is its own character: an
        // emoji that has no Unicode meaning, or one whose meaning the
        // far end had no cell for. UTF8-KDDI's own conversion tells
        // the two apart, and the second is reported as the Unicode.
        let unicode = if ('\u{e000}'..='\u{f8ff}').contains(&c) {
            transcode_bytes_with_opts(&meta.error_bytes, kddi, E::UTF8, &TranscodeOpts::default(), store)
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
    let (kind, consumed, utf8, meta) = if src_enc == kddi {
        // Its own bytes, once they are read as characters: a
        // malformed run is the source's to report, and a chunk that
        // ends inside a character keeps that tail for the next one.
        let good = std::str::from_utf8(src_bytes).map_or_else(|e| e.valid_up_to(), |_| src_bytes.len());
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
        pivot_prefix_consumed_in(src_bytes, src_enc, kddi, utf8_at.min(utf8.len()), &inner, store)
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
                    stage: Some(("UTF8-KDDI".to_string(), "stateless-ISO-2022-JP-KDDI".to_string())),
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
    let (result, consumed, eucjp, meta) = if src_enc == crate::value::Encoding::EUC_JP {
        let kind = if partial_input {
            StreamConvertResult::SourceBufferEmpty
        } else {
            StreamConvertResult::Finished
        };
        (kind, src_bytes.len(), src_bytes.to_vec(), ErrMeta::default())
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
    let mut out = Vec::with_capacity(eucjp.len());
    let mut at = 0;
    while at < eucjp.len() {
        let (unit, n): (Vec<u8>, usize) = match eucjp[at] {
            b @ 0x00..=0x7f => (vec![b], 1),
            0xa1..=0xfe if matches!(eucjp.get(at + 1), Some(0xa1..=0xfe)) => {
                (vec![0x92, eucjp[at], eucjp[at + 1]], 2)
            }
            _ => {
                let cell_len = crate::value::eucjp_char_width(&eucjp[at..]).unwrap_or(1).max(1);
                return (
                    StreamConvertResult::UndefinedConversion,
                    pivot_prefix_consumed_in(src_bytes, src_enc, crate::value::Encoding::EUC_JP, at, opts, store),
                    out,
                    ErrMeta {
                        error_bytes: eucjp[at..(at + cell_len).min(eucjp.len())].to_vec(),
                        decode_stage: false,
                        ..ErrMeta::default()
                    },
                );
            }
        };
        if let Some(max) = max_dst_bytes
            && out.len() + unit.len() > max
        {
            let fits = max - out.len();
            let written_through = pivot_prefix_consumed_in(src_bytes, src_enc, crate::value::Encoding::EUC_JP, at, opts, store);
            let through_tried =
                pivot_prefix_consumed_in(
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
    let (result, consumed, pivot, meta) =
        stream_convert(src_bytes, src_enc, E::UTF8, None, partial_input, opts, store);
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
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, crate::value::Encoding::UTF8, None, partial_input, opts, store);
        let out = match transcode_bytes_with_opts(&pivot, crate::value::Encoding::UTF8, dst_enc, opts, store) {
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
                match transcode_bytes_with_opts(&src_bytes[at..at + n], src_enc, dst_enc, opts, store)
                {
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
            let (error_bytes, stage, message) = match (unicode, carrier_pua_bytes(&own, src_enc, store)) {
                (Some(u), _) => (u, None, None),
                (None, Some(pua)) => {
                    let named = opts.report_src.unwrap_or(src_enc);
                    (
                        pua.clone(),
                        Some((
                            carrier_utf8_form(src_enc).map_or(src_enc, |e| e).name().to_string(),
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

fn stream_convert(
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
            return kddi_source_stream(src_bytes, dst_enc, max_dst_bytes, partial_input, opts, store);
        }
        if dst_enc == stateless_kddi_enc() && !kddi_wrapper(src_enc) {
            return kddi_dest_stream(src_bytes, src_enc, max_dst_bytes, partial_input, opts, store);
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
            return ibm037_source_stream(src_bytes, dst_enc, max_dst_bytes, partial_input, opts, store);
        }
        if is_ibm037(dst_enc) {
            return ibm037_dest_stream(src_bytes, src_enc, max_dst_bytes, partial_input, opts, store);
        }
        if src_enc == mac {
            return mac_source_stream(src_bytes, dst_enc, max_dst_bytes, partial_input, opts, store);
        }
        if src_enc == cesu {
            return cesu_source_stream(src_bytes, dst_enc, max_dst_bytes, partial_input, opts, store);
        }
        // ISO-2022-JP rides stateless-ISO-2022-JP, which rides EUC-JP.
        // The designation is the only thing that survives a chunk, and
        // `opts` / `meta` carry it in and out (#1609).
        if let Some(w) = jis_wrapper(src_enc) {
            let (stateless, left) =
                match (w.read)(src_bytes, opts.iso_state) {
                    Ok(v) => v,
                    // With `invalid: :replace` a malformed run is
                    // substituted here, but an *incomplete* one is
                    // only malformed once the input has ended — until
                    // then it is held for the next chunk (#1609).
                    Err(ref stop)
                        if opts.invalid_replace && (!stop.incomplete || !partial_input) =>
                    {
                        let out = transcode_bytes_with_opts(
                            src_bytes,
                            src_enc,
                            dst_enc,
                            opts,
                            store,
                        )
                        .unwrap_or_default();
                        let kind = if partial_input {
                            StreamConvertResult::SourceBufferEmpty
                        } else {
                            StreamConvertResult::Finished
                        };
                        return (kind, src_bytes.len(), out, ErrMeta::default());
                    }
                    Err(stop) => {
                        let (head, head_left) = (w.read)(
                            &src_bytes[..stop.at],
                            opts.iso_state,
                        )
                        .unwrap_or_default();
                        let (kind, consumed, out, mut meta) = stream_convert(
                            &head,
                            w.inner,
                            dst_enc,
                            max_dst_bytes,
                            true,
                            opts,
                            store,
                        );
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
                                    src_bytes,
                                    src_enc,
                                    w.inner,
                                    consumed,
                                    opts,
                                    store,
                                )
                            };
                            meta.iso_state_out = (w.read)(
                                &src_bytes[..through],
                                opts.iso_state,
                            )
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
                let fits = max_dst_bytes.map_or(stateless.len(), |m| m.min(stateless.len()));
                if fits < stateless.len() {
                    // The cap cuts a character in half. CRuby writes
                    // the bytes that fit and holds the rest of that
                    // character for the next call, taking the whole
                    // of it out of `src` — dropping them here cost
                    // the streaming loop its output (#1532).
                    let mut end = 0;
                    while end < stateless.len() {
                        end += if stateless[end] < 0x80 { 1 } else { 3 };
                        if end > fits {
                            break;
                        }
                    }
                    let end = end.min(stateless.len());
                    let consumed = pivot_prefix_consumed_in(
                        src_bytes,
                        src_enc,
                        w.inner,
                        end,
                        opts,
                        store,
                    );
                    return (
                        StreamConvertResult::DestinationBufferFull,
                        consumed,
                        stateless[..fits].to_vec(),
                        ErrMeta {
                            dst_full_out: stateless[fits..end].to_vec(),
                            iso_state_out: (w.read)(
                                &src_bytes[..consumed],
                                opts.iso_state,
                            )
                            .map(|(_, l)| l)
                            .unwrap_or(opts.iso_state),
                            ..ErrMeta::default()
                        },
                    );
                }
                let kind = if partial_input {
                    StreamConvertResult::SourceBufferEmpty
                } else {
                    StreamConvertResult::Finished
                };
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
            return match (w.write)(
                &stateless,
                opts.iso_state,
                ended && !partial_input,
            ) {
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
                            pivot_prefix_consumed_in(
                                src_bytes,
                                src_enc,
                                w.inner,
                                read,
                                opts,
                                store,
                            )
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
                    let closed_empty = (w.write)(
                        &stateless,
                        opts.iso_state,
                        false,
                    )
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
                    let (mut out, left) = (w.write)(
                        &stateless[..at],
                        opts.iso_state,
                        false,
                    )
                    .unwrap_or_default();
                    // Reaching here means the *stateless* bytes are
                    // malformed, and those are either `src_bytes`
                    // verbatim or the output of a conversion, which is
                    // well-formed by construction — so the source is
                    // stateless-ISO-2022-JP itself and its offsets are
                    // the ones below. (`w.inner` is the exact
                    // encoding, not the pair: a KDDI-stateless source
                    // is *converted* to it and takes the Ok arm.)
                    debug_assert_eq!(src_enc, w.inner);
                    let (k, mut m) =
                        bad_source_outcome(w.inner, &stateless[at..], !partial_input);
                    m.iso_state_out = left;
                    if let Some(max) = max_dst_bytes
                        && out.len() > max
                    {
                        let rest = out.split_off(max);
                        let read =
                            jis_read_through(w.write, &stateless[..at], opts.iso_state, max);
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
        let (kind, consumed, out, meta) =
            stream_convert(&src_bytes[..at], src_enc, dst_enc, max_dst_bytes, true, opts, store);
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
        let limit = max_dst_bytes.unwrap_or(src_bytes.len()).min(src_bytes.len());
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
        let (result, consumed, out, meta) =
            stream_convert(
        rest, wide, dst_enc, max_dst_bytes, partial_input,
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
            let (_, _, out, _) =
                stream_convert(
        &src_bytes[..at], src_enc, dst_enc, max_dst_bytes, false,
        opts,
        store,
    );
            let (kind, meta) = bad_source_outcome(src_enc, src_bytes, !partial_input);
            return (kind, at, out, meta);
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
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, E::UTF8, None, partial_input, opts, store);
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
                    (opts.replace_str(dst_enc).into_bytes(), n, None)
                }
                JisCell::Invalid(n) if opts.invalid_replace => {
                    (opts.replace_str(dst_enc).into_bytes(), n, None)
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
        let d = cell_decode(src_enc, src_rs_in, jp_fixup(src_enc), src_bytes, repl.as_deref());
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
            meta.dst_full_extra =
                pivot_prefix_consumed(src_bytes, src_enc, end, opts, store).saturating_sub(consumed);
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
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, E::UTF8, None, partial_input, opts, store);
        let text = String::from_utf8_lossy(&pivot);
        let mut out: Vec<u8> = Vec::with_capacity(text.len());
        let mut buf = [0u8; 4];
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
                    let (_, _, out, _) =
                        stream_convert(
        &src_bytes[..at], src_enc, dst_enc, max_dst_bytes, false,
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
        let (result, consumed, pivot, meta) =
            stream_convert(src_bytes, src_enc, E::UTF8, None, partial_input, opts, store);
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
                        pivot_prefix_consumed(src_bytes, src_enc, pivot_at + ch.len_utf8(), opts, store),
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
                    let through_tried =
                        pivot_prefix_consumed(src_bytes, src_enc, pivot_at + ch.len_utf8(), opts, store);
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
                    return (StreamConvertResult::SourceBufferEmpty, e.valid_up_to(), out, ErrMeta::default());
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
        src_bytes, src_enc, dst_enc, None, partial_input,
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
                    let (r, read, written) = probe.decode_to_utf8_without_replacement(
                        &run[..k],
                        &mut probe_dst,
                        false,
                    );
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
                    (unit..run.len()).rev().step_by(unit).find(|k| pending_prefix(*k))
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
fn first_bad_sequence(enc: crate::value::Encoding, bytes: &[u8]) -> Option<(Vec<u8>, Vec<u8>, bool)> {
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
fn conversion_walker(
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
            "EUC_KR" | "GB2312" | "GB12345" | "EUC_TW" | "GBK" | "GB18030" | "CESU_8"
                | "Big5" | "Big5_HKSCS" | "Big5_UAO" | "CP950" | "CP951"
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
fn dst_can_hold(enc: crate::value::Encoding, bytes: &[u8]) -> bool {
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
        encoding_rs::EncoderResult::InputEmpty
            if dst_can_hold(dst_enc, &out[..written]) =>
        {
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
fn is_the_utf8_pivot(enc: crate::value::Encoding) -> bool {
    // US-ASCII is *not* the pivot: CRuby still builds a US-ASCII →
    // UTF-8 step in front of every other hop, so a byte the source
    // encoding has no character for is reported against UTF-8 as the
    // destination whatever the conversion's real destination is
    // (#1596).
    enc == crate::value::Encoding::UTF8
}

fn error_stage_names(
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
        let stage_src = if src_enc == pivot || is_the_utf8_pivot(src_enc) && pivot == crate::value::Encoding::UTF8 {
            src_enc.name().to_string()
        } else {
            pivot.name().to_string()
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
    if let Some(message) = &meta.message {
        return message.clone();
    }
    let (stage_src, stage_dst) = match (&meta.stage, result) {
        // The stream named the hop that gave up: a wrapper's inner
        // encoding refusing what the wrapper read (#1530).
        (Some(stage), _) => stage.clone(),
        (None, StreamConvertResult::UndefinedConversion) if !meta.decode_stage => {
            error_stage_names(src_enc, dst_enc, false)
        }
        _ => error_stage_names(src_enc, dst_enc, true),
    };
    match result {
        StreamConvertResult::UndefinedConversion if meta.decode_stage => {
            // A source byte with no Unicode meaning: there is no
            // codepoint to name, so CRuby dumps the bytes the way an
            // invalid sequence is dumped, and the failing hop is
            // source → pivot.
            let bytes = quote_error_bytes(&meta.error_bytes);
            if transcoder_spelling(src_enc.name()) != src_enc.name() {
                if dst_enc == crate::value::Encoding::UTF8 {
                    format!("{bytes} to UTF-8 in conversion from {} to UTF-8", src_enc.name())
                } else {
                    format!(
                        "{bytes} to UTF-8 in conversion from {} to UTF-8 to {}",
                        src_enc.name(),
                        dst_enc.name()
                    )
                }
            } else if stage_dst == dst_enc.name() {
                format!("{} from {} to {}", bytes, stage_src, stage_dst)
            } else {
                format!(
                    "{} to {} in conversion from {} to UTF-8 to {}",
                    bytes,
                    stage_dst,
                    src_enc.name(),
                    dst_enc.name()
                )
            }
        }
        StreamConvertResult::UndefinedConversion => {
            let cp = std::str::from_utf8(&meta.error_bytes)
                .ok()
                .and_then(|s| s.chars().next())
                .map(|c| c as u32)
                .unwrap_or(0);
            if transcoder_spelling(dst_enc.name()) != dst_enc.name() {
                let from = if is_the_utf8_pivot(src_enc) {
                    src_enc.name().to_string()
                } else {
                    format!("{} to UTF-8", src_enc.name())
                };
                format!("U+{:04X} to {stage_dst} in conversion from {from} to {stage_dst}", cp)
            } else if is_the_utf8_pivot(src_enc) {
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
        let decode_stage =
            !matches!(result, StreamConvertResult::UndefinedConversion) || meta.decode_stage;
        let (stage_src, stage_dst) = meta
            .stage
            .clone()
            .unwrap_or_else(|| error_stage_names(src_enc, dst_enc, decode_stage));
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
        let decode_stage =
            !matches!(result, StreamConvertResult::UndefinedConversion) || meta.decode_stage;
        let (stage_src, stage_dst) = meta
            .stage
            .clone()
            .unwrap_or_else(|| error_stage_names(src_enc, dst_enc, decode_stage));
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
        return Err(MonorubyErr::no_implicit_conversion(
            &globals.store,
            dst_arg,
            STRING_CLASS,
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
            .ok_or_else(|| {
                MonorubyErr::no_implicit_conversion(&globals.store, src_arg, STRING_CLASS)
            })?
            .as_bytes()
            .to_vec()
    };
    if !src_arg.is_nil() {
        src_arg.ensure_string_mutable(vm, globals)?;
    }
    // Once the input has ended the converter is done (see below):
    // a later call reads nothing, so it takes nothing out of the
    // converter either.
    let finished_id = IdentId::get_id(CONVERTER_FINISHED_IVAR);
    let already_finished = globals.store.get_ivar(recv, finished_id).is_some();
    // The read-again bytes of the last `:invalid_byte_sequence` come
    // first — CRuby keeps them at the head of its input buffer, so
    // `"\xf1abcd"` into ISO-8859-1 stops at `\xF1` with `"a"` read
    // again, and the next call writes `"abcd"` — then the bytes a
    // dst-bytesize cap held back last call, then the new source, so
    // multi-call streaming with `dst_bytesize` works (the spec test
    // "uses the destination byte offset" hits this path).
    let mut src_bytes = if already_finished {
        vec![]
    } else {
        converter_take_readagain(globals, recv)
    };
    src_bytes.extend(
        globals
            .store
            .get_ivar(recv, IdentId::get_id(CONVERTER_PENDING_IVAR))
            .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
            .unwrap_or_default(),
    );
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
    // What is left in the caller's source keeps the tag the caller
    // gave it: the converter reads the bytes as its own source
    // encoding, but never re-tags the String (#1424).
    let src_tag = match src_arg.is_rstring_inner() {
        Some(s) => s.encoding(),
        None => src_enc,
    };
    // The endianness-less dummies carry a BOM the stream shows once:
    // read off the source here and remembered, written to the
    // destination ahead of the first character it emits (#1576).
    let src_stream = converter_resolve_src_bom(globals, recv, src_enc, &src_bytes);
    let dst_stream = dummy_wide_target(dst_enc).unwrap_or(dst_enc);

    // Output the last call's cap held back mid-character goes out
    // first, before anything new is converted, and counts against
    // this call's cap (#1532). If it fills the destination on its
    // own the call stops there having read nothing — and the stream
    // is not over, so the end-of-input check below is skipped with
    // it.
    let pending_out_id = IdentId::get_id(CONVERTER_PENDING_OUT_IVAR);
    let pending_out: Vec<u8> = globals
        .store
        .get_ivar(recv, pending_out_id)
        .and_then(|v| v.is_rstring_inner().map(|s| s.as_bytes().to_vec()))
        .unwrap_or_default();
    let (held_out, still_held) = match max_dst_bytes {
        Some(max) if pending_out.len() > max => {
            (pending_out[..max].to_vec(), pending_out[max..].to_vec())
        }
        _ => (pending_out, vec![]),
    };
    if !still_held.is_empty() {
        let _ = globals
            .store
            .set_ivar(recv, pending_out_id, Value::bytes_from_slice(&still_held));
        let mut new_dst_bytes = dst_initial[..dst_offset].to_vec();
        new_dst_bytes.extend_from_slice(&held_out);
        let new_dst = crate::value::RStringInner::from_encoding_scanned(&new_dst_bytes, dst_enc);
        dst_arg.replace_with_inner(new_dst);
        let result = StreamConvertResult::DestinationBufferFull;
        store_conversion_outcome(globals, recv, result, &ErrMeta::default(), src_enc, dst_enc);
        return Ok(Value::symbol_from_str(result.symbol_name()));
    }
    let _ = globals.store.set_ivar(recv, pending_out_id, Value::nil());
    // Whatever it took is off this call's allowance.
    let max_dst_bytes = max_dst_bytes.map(|m| m - held_out.len());

    // A call that brings no source of its own says the input has
    // ended, unless `partial_input:` says more is merely not here
    // yet — `nil` and `""` alike, which is how CRuby's
    // `rb_econv_convert` reads an empty final chunk. Once that has
    // happened the converter is done: every later call answers
    // `:finished` having converted nothing, and `#convert` raises
    // (#1537). `#finish` sets the same flag.
    // The *caller's* source, not the pending buffer: bytes held back
    // by an earlier `dst_bytesize` cap are still converted by the
    // call that ends the stream.
    let no_more_input = src_arg.is_nil() || new_src_bytes.is_empty();

    let mut conv_opts = converter_transcode_opts(globals, recv);
    conv_opts.iso_state = converter_iso_state(globals, recv);
    let (result, src_consumed, out_bytes, meta) = if already_finished {
        (
            StreamConvertResult::Finished,
            0,
            Vec::new(),
            ErrMeta::default(),
        )
    } else {
        // The destination's BOM takes its bytes out of this call's
        // allowance before anything is converted into it.
        let bom_owed = if converter_dst_bom_owed(globals, recv, dst_enc) {
            converter_dst_bom(dst_enc).len()
        } else {
            0
        };
        stream_convert(
            &src_bytes,
            src_stream,
            dst_stream,
            max_dst_bytes.map(|m| m.saturating_sub(bom_owed)),
            partial_input,
            &conv_opts,
            &globals.store,
        )
    };
    if !already_finished {
        converter_set_iso_state(globals, recv, meta.iso_state_out);
    }
    // The BOM goes in front of the first output there is, and is
    // itself output: a cap too small to hold it writes what fits and
    // holds the rest for the next call, the way a character's bytes
    // are held (#1532). Without that a cap of three bytes against a
    // four-byte BOM wrote nothing at all, and a caller looping to
    // `:finished` never got there (#1576).
    let mut bom_overflow: Vec<u8> = vec![];
    // A cap that left no room for a character left none for the BOM
    // either, and that call is still the one the BOM belongs to.
    let emits = !out_bytes.is_empty()
        || matches!(result, StreamConvertResult::DestinationBufferFull);
    let out_bytes = if !emits {
        out_bytes
    } else {
        let mut with_bom = converter_take_dst_bom(globals, recv, dst_enc);
        with_bom.extend_from_slice(&out_bytes);
        with_bom
    };
    let out_bytes = match max_dst_bytes {
        Some(max) if out_bytes.len() > max => {
            bom_overflow = out_bytes[max..].to_vec();
            out_bytes[..max].to_vec()
        }
        _ => out_bytes,
    };
    // What the call reports, and whether it closes the stream.
    //
    // `partial_input: true` never finishes: the chunk is converted
    // whole, but more may follow, so a run that reached the end of it
    // is `:source_buffer_empty` — with input of its own as much as
    // without (#1589). An error it ran into is still the answer.
    //
    // Without it the call is the last: the converter is done once
    // it has read everything it was given — whether the caller said
    // so with an empty source or the run simply finished — and every
    // later call answers `:finished` having converted nothing.
    let result = if already_finished {
        result
    } else if partial_input {
        match result {
            StreamConvertResult::Finished => StreamConvertResult::SourceBufferEmpty,
            other => other,
        }
    } else {
        if no_more_input || matches!(result, StreamConvertResult::Finished) {
            let _ = globals.store.set_ivar(recv, finished_id, Value::bool(true));
        }
        result
    };

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
    //   - `:destination_buffer_full`: the bytes the destination
    //     had no room for stay in `src_arg` too. CRuby reads one
    //     character further than it writes and buffers *that
    //     character's output*; we have no output buffer, so that
    //     one character's source bytes go to the pending buffer
    //     and are re-converted next call, which leaves `src_arg`
    //     holding exactly what CRuby leaves there (#1511).
    //   - everything else (`:finished`, `:source_buffer_empty`,
    //     `:incomplete_input`): `src_arg` is cleared completely,
    //     and any tail (`src_bytes[src_consumed..]`) goes into the
    //     per-converter pending buffer for the next call.
    let leave_remaining_in_src = matches!(
        result,
        StreamConvertResult::InvalidByteSequence
            | StreamConvertResult::UndefinedConversion
            | StreamConvertResult::DestinationBufferFull
    );
    let pending_id = IdentId::get_id(CONVERTER_PENDING_IVAR);
    if already_finished {
        // Neither `src` nor the pending buffer is touched — this call
        // read nothing.
    } else if leave_remaining_in_src {
        // How much of the source this call took but did not write.
        // Only a capped destination has any (an error result stops
        // before the offending character rather than reading past
        // it), and it must be held for the next call or its
        // conversion is lost.
        // A `UTF8-MAC` source holds its trailing cluster for the
        // composition the next chunk may complete. Those bytes have
        // been read, so they belong with what this call buffers and
        // not in `src` — CRuby leaves nothing of them there either
        // (#1576).
        let read_ahead = if matches!(result, StreamConvertResult::DestinationBufferFull) {
            (meta.dst_full_extra + mac_held_len(src_stream, &src_bytes[src_consumed..]))
                .min(src_bytes.len() - src_consumed)
        } else {
            0
        };
        // The character the cap stopped at has been converted and
        // what did not fit is held as *output*, so its source must
        // not also be held as input or it converts twice (#1532).
        let converted_ahead = !meta.dst_full_out.is_empty();
        if converted_ahead || !bom_overflow.is_empty() {
            let mut held = bom_overflow.clone();
            held.extend_from_slice(&meta.dst_full_out);
            let _ = globals
                .store
                .set_ivar(recv, pending_out_id, Value::bytes_from_slice(&held));
        }
        // With no `src` to leave them in (the caller passed `nil`)
        // everything a capped destination held back has to be
        // buffered instead, or it is lost. An error result buffers
        // nothing either way: the converter cannot make progress on
        // those bytes without the caller's intervention.
        let split = if !src_arg.is_nil() {
            src_consumed + read_ahead
        } else if matches!(result, StreamConvertResult::DestinationBufferFull) {
            src_bytes.len()
        } else {
            src_consumed
        };
        // Its output is held instead, so the bytes of the character
        // already converted are not buffered again — but only those.
        // The cluster a `UTF8-MAC` source is holding for a composition
        // is read ahead *without* being converted, so it belongs in
        // the buffer: skipping the whole of `split` here dropped it,
        // and `"abcd"` in `UTF8-MAC` came back through a two-byte
        // destination as `"abd"` (#1577).
        let converted_ahead_len = if converted_ahead {
            meta.dst_full_extra.min(src_bytes.len() - src_consumed)
        } else {
            0
        };
        let buffered_from = (src_consumed + converted_ahead_len).min(split);
        let buffered: Vec<u8> = src_bytes[buffered_from..split].to_vec();
        // Put bytes after the error back into `src_arg`. Pending
        // buffer otherwise drops — the converter has nothing it
        // could write next call without more user input.
        let leftover: Vec<u8> = src_bytes[split..].to_vec();
        if !src_arg.is_nil() {
            let new_src = crate::value::RStringInner::from_encoding_scanned(&leftover, src_tag);
            src_arg.replace_with_inner(new_src);
        }
        if buffered.is_empty() {
            let _ = globals.store.set_ivar(recv, pending_id, Value::nil());
        } else {
            let mut pending_inner =
                crate::value::RStringInner::from_encoding_scanned(&buffered, src_enc);
            pending_inner.set_encoding(crate::value::Encoding::Ascii8);
            let _ = globals
                .store
                .set_ivar(recv, pending_id, Value::string_from_inner(pending_inner));
        }
    } else {
        // Clear src and stash the unconverted tail (if any) in
        // pending for the next call. An `:incomplete_input` answer
        // has *reported* that tail — it is the error's bytes, and
        // CRuby drops them from its buffer with the report — so
        // nothing is held: the next `#convert` starts afresh rather
        // than gluing them onto its argument.
        let pending_after: Vec<u8> = if matches!(result, StreamConvertResult::IncompleteInput) {
            vec![]
        } else {
            src_bytes[src_consumed..].to_vec()
        };
        if !src_arg.is_nil() {
            let cleared = crate::value::RStringInner::from_encoding_scanned(b"", src_tag);
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
    new_dst_bytes.extend_from_slice(&held_out);
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
    let enc_name: String = if is_encoding_object(globals, arg) {
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
    let (src, dst) = resolve_converter_pair(
        vm,
        globals,
        ConverterSrc::Given(lfp.arg(0)),
        lfp.arg(1),
        false,
    )?;
    validate_converter_pair(src, dst, &TranscodeOpts::default(), &globals.store)?;
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

/// The hops of a conversion between a member of one of the JIS lines
/// and anything else, as `convpath` reports them.
///
/// The family runs in lines: `ISO-2022-JP — stateless-ISO-2022-JP —
/// EUC-JP — Shift_JIS`, with UTF-8 hanging off EUC-JP's end;
/// `ISO-2022-JP-KDDI — stateless-ISO-2022-JP-KDDI — UTF8-KDDI —
/// UTF-8`; and `CP50220 — CP51932 — UTF-8` (CP50221 likewise). A
/// conversion walks its line, so the cells never become Unicode
/// unless the other end is outside the family (#1609, #1520, #1530).
fn jis_family_chain(
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
    // The JIS family reaches Unicode through each other, not through
    // the pivot: ISO-2022-JP writes stateless-ISO-2022-JP, which
    // writes EUC-JP, which writes Shift_JIS — and only EUC-JP has a
    // hop to UTF-8 at all (#1609).
    if let Some(chain) = jis_family_chain(src, dst) {
        for pair in chain.windows(2) {
            elems.push(Value::array2(
                encoding_value(globals, pair[0]),
                encoding_value(globals, pair[1]),
            ));
        }
    } else if src == E::UTF8
        || dst == E::UTF8
        || src == dst
        // EUC-JP ↔ Shift_JIS is one step: CRuby has a converter that
        // maps the shared JIS X 0208 plane directly (#1460).
        || jis_direct_from_euc(src, dst).is_some()
    {
        elems.push(Value::array2(
            encoding_value(globals, src),
            encoding_value(globals, dst),
        ));
    } else {
        elems.push(Value::array2(
            encoding_value(globals, src),
            encoding_value(globals, E::UTF8),
        ));
        elems.push(Value::array2(
            encoding_value(globals, E::UTF8),
            encoding_value(globals, dst),
        ));
    }
    if crlf {
        // The decorator rewrites LF, so it has to run while the text
        // is still ASCII-compatible. CRuby puts it *before* the hop
        // that writes an ASCII-incompatible destination — the escape
        // sequences of ISO-2022-JP, the code units of UTF-16 — and
        // after everything otherwise (`rb_econv_decorate_at_last`).
        let at = if dst.is_ascii_compatible() {
            elems.len()
        } else {
            elems.len() - 1
        };
        elems.insert(at, Value::string_from_str("crlf_newline"));
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
        // The pivot form names three encodings — `from A to UTF-8 to
        // D` — and the hop that failed is the first one, which is
        // what CRuby's accessors answer. Taking the whole remainder
        // made the destination read "UTF-8 to D".
        let dst = rest.split(" to ").next().unwrap_or(rest);
        return Some((src.trim().to_string(), dst.trim().to_string()));
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
            if is_encoding_object(globals, v) {
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
    // CRuby's `Encoding.find` accepts either a String name (subject
    // to `to_str` coercion) *or* an existing `Encoding` object,
    // returning it unchanged. Without this short-circuit a value of
    // class `Encoding` would fail `coerce_to_string`'s TypeError.
    if is_encoding_object(globals, arg0) {
        return Ok(arg0);
    }
    let name = arg0.coerce_to_string(vm, globals)?;
    // Special names resolved at query time: the locale encoding follows
    // the locale charmap, the filesystem/external ones follow
    // `default_external`, and "internal" may name nothing. The lookup
    // below answers all four the same way; the one thing `Encoding.find`
    // does differently is report an unset `"internal"` as nil rather
    // than as an unknown name.
    if name.eq_ignore_ascii_case("internal") {
        return Ok(dynamic_alias_object(globals, &name).unwrap_or_else(Value::nil));
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

/// The `Encoding` object one of the four run-time alias names stands
/// for, or `None` for any other name.
///
/// CRuby registers these in the encoding table with
/// `enc_alias_internal` and re-points them whenever the setting behind
/// them moves: `"locale"` at startup from the locale charmap,
/// `"filesystem"` and `"external"` from `Encoding.default_external`,
/// `"internal"` only while a `default_internal` is set. That last one
/// is why `"internal"` can name nothing at all — with no default
/// internal encoding the alias was never registered, so the name is
/// simply unknown, which is what `Encoding.find` reports as nil and
/// what makes `"x".encode("internal")` a missing converter rather than
/// a conversion to BINARY.
fn dynamic_alias_object(globals: &Globals, name: &str) -> Option<Value> {
    // The names are ASCII and the comparison is case-insensitive, as
    // every encoding-name lookup is; unlike the rest of them these are
    // whole names, so the `-` / `_` folding does not apply.
    let gvar = |n: &str| {
        globals
            .get_gvar(IdentId::get_id(n))
            .filter(|v| !v.is_nil())
    };
    if name.eq_ignore_ascii_case("locale") {
        Some(locale_encoding_value(globals))
    } else if name.eq_ignore_ascii_case("external") || name.eq_ignore_ascii_case("filesystem") {
        // `$DEFAULT_EXTERNAL` is seeded at startup and can never be
        // unset again, so the fallback only covers a `Globals` that
        // `init_default_external` never ran on.
        Some(gvar("$DEFAULT_EXTERNAL").unwrap_or_else(|| {
            let enc_class = encoding_class(globals);
            globals
                .store
                .get_constant_noautoload(enc_class, IdentId::UTF_8)
                .unwrap_or(Value::nil())
        }))
    } else if name.eq_ignore_ascii_case("internal") {
        gvar("$DEFAULT_INTERNAL")
    } else {
        None
    }
}

/// Resolve an encoding *name* to its registered `Encoding` object,
/// preserving object identity (so e.g. `IBM866` stays `IBM866` rather
/// than collapsing to `ASCII-8BIT` the way the `Encoding` enum does).
/// Mirrors `Encoding.find` without the `to_str`/error handling.
pub(super) fn find_encoding_object(globals: &Globals, name: &str) -> Option<Value> {
    // The four names that stand for a setting rather than for an
    // encoding are answered from that setting, before any table is
    // consulted. They have to be: a fixed answer here is what made
    // `String#encode("locale")` and `File.open(f, "r:locale")` reach
    // UTF-8 whatever the locale actually was (#1575).
    if let Some(v) = dynamic_alias_object(globals, name) {
        return Some(v);
    }
    let enc_class = encoding_class(globals);
    // A name is one of the names `Encoding.name_list` lists, compared
    // without regard to case and to nothing else: "utf8" and "UTF_8"
    // are unknown names, "eucJP" is an alias. The alias resolves to
    // its canonical name, and the canonical name to its object.
    let canonical = known_encoding_name(globals, name)?;
    // The alias table names a constant directly, which answers the
    // common names ("UTF-8", "ASCII-8BIT", …) without touching the rest
    // of the table. It is only trusted here when the constant it names
    // really does carry this canonical name — the table is
    // hand-maintained and would otherwise mis-resolve a name like
    // "Big5-HKSCS" to a prefix match — so anything it gets wrong falls
    // through to the scan below, which is what decides.
    if let Some(v) = enc_name_to_const(&canonical)
        .and_then(|c| globals.store.get_constant_noautoload(enc_class, IdentId::get_id(c)))
        && is_encoding_object(globals, v)
        && encoding_object_name_is(globals, v, &canonical)
    {
        return Some(v);
    }
    // The canonical name of every registered encoding, so
    // `Encoding.find(e.name)` round-trips for *every* encoding in
    // `Encoding.list`.
    for cname in globals.store.get_constant_names(enc_class) {
        if let Some(v) = globals.store.get_constant_noautoload(enc_class, cname)
            && is_encoding_object(globals, v)
            && encoding_object_name_is(globals, v, &canonical)
        {
            return Some(v);
        }
    }
    None
}

/// The canonical name `name` stands for, when `name` is one of the
/// names `Encoding.name_list` lists — a canonical name or an alias in
/// `ENCODING_NAMES`, or the canonical name of a registered encoding
/// the table does not carry — compared ASCII-case-insensitively and
/// otherwise exactly, as CRuby's encoding table does. The four
/// run-time aliases are not answered here.
fn known_encoding_name(globals: &Globals, name: &str) -> Option<String> {
    if name.is_empty() || !name.is_ascii() {
        return None;
    }
    for (canonical, aliases) in ENCODING_NAMES {
        if canonical.eq_ignore_ascii_case(name)
            || aliases.iter().any(|a| a.eq_ignore_ascii_case(name))
        {
            return Some(canonical.to_string());
        }
    }
    let enc_class = encoding_class(globals);
    for cname in globals.store.get_constant_names(enc_class) {
        if let Some(v) = globals.store.get_constant_noautoload(enc_class, cname)
            && is_encoding_object(globals, v)
            && let Some(es) = globals
                .store
                .get_ivar(v, IdentId::_ENCODING)
                .and_then(|ev| ev.is_str().map(|s| s.to_string()))
            && es.eq_ignore_ascii_case(name)
        {
            return Some(es);
        }
    }
    None
}

/// `v`'s canonical name (its `_ENCODING` ivar) is `name`, compared the
/// way `Encoding.find` compares names: ASCII case does not count,
/// everything else does.
fn encoding_object_name_is(globals: &Globals, v: Value, name: &str) -> bool {
    match globals.store.get_ivar(v, IdentId::_ENCODING) {
        Some(ev) => match ev.is_str() {
            Some(es) => es.eq_ignore_ascii_case(name),
            None => false,
        },
        None => false,
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
        // `"LOCALE"`, `"EXTERNAL"`, `"FILESYSTEM"` and `"INTERNAL"` are
        // deliberately absent: they name whatever the interpreter's
        // settings currently hold, which this table cannot know.
        // `dynamic_alias_object` answers them from that state before
        // any caller reaches here (#1575).

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
        "EUC_JIS_2004" | "EUC_JISX0213" => Some("EUC_JIS_2004"),
        "STATELESS_ISO_2022_JP_KDDI" => Some("STATELESS_ISO_2022_JP_KDDI"),
        // `UTF8-MAC` is CRuby's identifier for HFS+ NFD-normalised
        // UTF-8 (used on macOS filesystems); a conversion to or from
        // it applies that normalisation (#1562).
        "UTF8_MAC" | "UTF_8_MAC" | "UTF_8_HFS" => Some("UTF8_MAC"),

        // The carrier sets (#1573).
        "UTF8_DOCOMO" => Some("UTF8_DOCOMO"),
        "UTF8_KDDI" => Some("UTF8_KDDI"),
        "UTF8_SOFTBANK" => Some("UTF8_SOFTBANK"),
        "SJIS_DOCOMO" => Some("SJIS_DOCOMO"),
        "SJIS_KDDI" => Some("SJIS_KDDI"),
        "SJIS_SOFTBANK" => Some("SJIS_SOFTBANK"),
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
        "IBM852" => Some("IBM852"),
        "IBM855" => Some("IBM855"),
        // CP852 / CP855 are encodings of their own, not aliases of the
        // IBM pages above (#1555).
        "CP852" => Some("CP852"),
        "CP855" => Some("CP855"),
        "IBM720" | "CP720" => Some("IBM720"),
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
        "BIG5_HKSCS" | "BIG5_HKSCS:2008" => Some("Big5_HKSCS"),
        "CP950" => Some("CP950"),
        "CP951" => Some("CP951"),
        "WINDOWS_874" | "CP874" => Some("Windows_874"),
        "GB1988" => Some("GB1988"),
        "IBM037" | "EBCDIC_CP_US" => Some("EBCDIC_CP_US"),
        "ISO_2022_JP_2" | "ISO2022_JP2" => Some("ISO_2022_JP_2"),
        "ISO_2022_JP_KDDI" => Some("ISO_2022_JP_KDDI"),
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
                frozen_usascii(alias),
                frozen_usascii(canonical),
                vm,
                globals,
            )?;
        }
    }
    for (alias, canonical) in dynamic_encoding_aliases(globals) {
        map.insert(
            frozen_usascii(alias),
            frozen_usascii(&canonical),
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
const DYNAMIC_ALIASES: &[&str] = &["locale", "external", "filesystem", "internal"];

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
    // `"internal"` follows `Encoding.default_internal`, which is
    // *unset* by default — so unlike the other three it usually names
    // no encoding at all, and appears in `Encoding.name_list` without
    // appearing in any encoding's `#names` (#1520).
    let internal = globals
        .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
        .filter(|v| !v.is_nil())
        .and_then(|v| canonical_of(globals, v));
    DYNAMIC_ALIASES
        .iter()
        .filter_map(|alias| {
            let target = match *alias {
                "locale" => locale.clone().unwrap_or_else(|| external.clone()),
                "internal" => internal.clone()?,
                _ => external.clone(),
            };
            Some((*alias, target))
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
    // The two stateful ISO-2022-JP variants, and EBCDIC. All dummy in
    // CRuby, so raw bytes with a preserved name is all they need
    // (#1555).
    ("ISO-2022-JP-2", &["ISO2022-JP2"]),
    ("ISO-2022-JP-KDDI", &[]),
    ("IBM037", &["ebcdic-cp-us"]),
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
    // The DOS code pages: CRuby names each one both ways (#1520).
    ("IBM437", &["CP437"]),
    ("IBM737", &["CP737"]),
    ("IBM775", &["CP775"]),
    ("CP850", &["IBM850"]),
    ("IBM852", &[]),
    ("IBM855", &[]),
    // CP852 / CP855 are encodings of their own in CRuby, each with a
    // single name — not aliases of the IBM ones, which is how they
    // resolved here (#1555).
    ("CP852", &[]),
    ("CP855", &[]),
    ("IBM720", &["CP720"]),
    ("Windows-874", &["CP874"]),
    ("GB1988", &[]),
    // Big5 variants of their own rather than aliases of Big5 /
    // Big5-HKSCS.
    ("CP950", &[]),
    ("CP951", &[]),
    ("IBM857", &["CP857"]),
    ("IBM860", &["CP860"]),
    ("IBM861", &["CP861"]),
    ("IBM862", &["CP862"]),
    ("IBM863", &["CP863"]),
    ("IBM864", &["CP864"]),
    ("IBM865", &["CP865"]),
    ("IBM866", &["CP866"]),
    ("IBM869", &["CP869"]),
    ("Big5-HKSCS", &["Big5-HKSCS:2008"]),
    ("UTF8-MAC", &["UTF-8-MAC", "UTF-8-HFS"]),
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
    ("UTF8-DoCoMo", &[]),
    ("SJIS-DoCoMo", &[]),
    ("UTF8-KDDI", &[]),
    ("SJIS-KDDI", &[]),
    ("UTF8-SoftBank", &[]),
    ("SJIS-SoftBank", &[]),
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
    ("eucJP-ms", &["euc-jp-ms"]),
    ("CP51932", &[]),
    ("stateless-ISO-2022-JP", &[]),
    ("EUC-JIS-2004", &["EUC-JISX0213"]),
    ("stateless-ISO-2022-JP-KDDI", &[]),
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
            names.push(frozen_usascii(s));
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
            && is_encoding_object(globals, v)
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
    let mut names: Vec<Value> = vec![frozen_usascii(&canonical)];
    for (c, aliases) in ENCODING_NAMES {
        if c.eq_ignore_ascii_case(&canonical) {
            for alias in *aliases {
                names.push(frozen_usascii(alias));
            }
            break;
        }
    }
    for (alias, target) in dynamic_encoding_aliases(globals) {
        if target.eq_ignore_ascii_case(&canonical) {
            names.push(frozen_usascii(alias));
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
    // `Encoding` × `Encoding` follows a different rule set: only the
    // "second is US-ASCII" exception, no ASCII-only-content
    // accommodation (since Encoding objects carry no bytes). Dummy
    // encodings are rejected outright (except when both sides are
    // the same dummy — `compatible?(UTF_7, UTF_7) == UTF_7`).
    if is_encoding_object(globals, a) && is_encoding_object(globals, b) {
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
                    Encoding::UTF8
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
    if is_encoding_object(globals, v) {
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
    // `enc_compatible_latter` with no strings to look into: both
    // sides have to be ASCII-compatible (which no dummy is), and then
    // US-ASCII on either side yields the other — "objects whose
    // encoding is the same as their contents".
    if is_cruby_dummy(a) || is_cruby_dummy(b) {
        return None;
    }
    if !a.is_ascii_compatible() || !b.is_ascii_compatible() {
        return None;
    }
    if b == Encoding::UsAscii {
        return Some(a);
    }
    if a == Encoding::UsAscii {
        return Some(b);
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
pub(super) fn is_cruby_dummy_name(name: &str) -> bool {
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
            | "ISO-2022-JP-2"
            | "ISO-2022-JP-KDDI"
            | "CP50220"
            | "CP50221"
            | "UTF-7"
            // EBCDIC does not even agree with ASCII on the letters.
            | "IBM037"
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
    fn newly_registered_encodings_match_cruby() {
        // Ten names that raised `unknown encoding name` — or, for
        // CP852 / CP855 / CP950 / CP951, resolved to a *relative* —
        // until #1555. Each is byte-structurally identical to an
        // encoding already here (checked over every one- and two-byte
        // sequence), so they ride its walk and keep their own name.
        crate::tests::run_test_once(
            r##"
            names = %w[CP852 CP855 CP950 CP951 GB1988 IBM037 IBM720
                       ISO-2022-JP-2 ISO-2022-JP-KDDI Windows-874
                       CP720 CP874 ebcdic-cp-us ISO2022-JP2
                       IBM852 IBM855 Big5 Big5-HKSCS]
            sample = ["\x41\x42", "\xE6\x9D\x94", "\xA4\xA2", "\x81\xA0",
                      "\xFF", "\xA1\x40", "\x0a"]
            names.map { |n|
              e = Encoding.find(n)
              [n, e.name, e.names.sort, e.dummy?, e.ascii_compatible?,
               sample.map { |s|
                 t = s.b.dup.force_encoding(e)
                 [t.valid_encoding?, t.length]
               }]
            }
            "##,
        );
    }

    #[test]
    fn the_euc_jp_family_keeps_its_own_names() {
        // `eucJP-ms`, `CP51932` and `EUC-JIS-2004` share EUC-JP's byte
        // structure exactly — checked over every one- and two-byte
        // sequence against CRuby — and differ only in vendor mapping
        // tables. They used to collapse onto EUC-JP, so a string asked
        // to be `CP51932` came back labelled `EUC-JP` while
        // `Encoding.find` answered correctly (#1562).
        crate::tests::run_test_once(
            r##"
            sample = ["\x41\x42", "\xA4\xA2", "\x8E\xB1", "\x8F\xA1\xA1",
                      "\xE6\x9D\x94", "\xFF", "\xA1", "\x81\xA0"]
            %w[EUC-JP eucJP eucJP-ms euc-jp-ms CP51932
               EUC-JIS-2004 EUC-JISX0213
               stateless-ISO-2022-JP stateless-ISO-2022-JP-KDDI].map { |n|
              e = Encoding.find(n)
              [n, e.name, e.names.sort, e.dummy?, e.ascii_compatible?,
               "abc".b.dup.force_encoding(n).encoding.name,
               sample.map { |s|
                 t = s.b.dup.force_encoding(e)
                 [t.valid_encoding?, t.length]
               }]
            }
            "##,
        );
    }

    #[test]
    fn stateless_iso_2022_jp_is_not_an_euc_jp_variant() {
        // CRuby names it with the EUC-JP family and monoruby read it
        // as one, but it is ISO-2022-JP's repertoire without the
        // escapes: ASCII plus a lead in 0x81..0x8F and a trail in
        // 0xA0..0xFF. The two disagree on 10182 of the one- and
        // two-byte sequences, so it has a walk of its own (#1562).
        crate::tests::run_test_once(
            r##"
            st = Encoding.find("stateless-ISO-2022-JP")
            eu = Encoding.find("EUC-JP")
            seqs = (0..255).flat_map { |x| [[x].pack("C"), [x, 0xA0].pack("C2"),
                                            [x, 0x41].pack("C2"), [x, 0xFF].pack("C2")] }
            agree = seqs.count { |s|
              a = s.dup.force_encoding(st); b = s.dup.force_encoding(eu)
              a.valid_encoding? == b.valid_encoding? && a.length == b.length
            }
            [seqs.size, agree,
             seqs.count { |s| s.dup.force_encoding(st).valid_encoding? }]
            "##,
        );
        // A `0x90..=0x99` lead names a two-byte set, so the sequence
        // is three bytes long; the walk read it as a stray byte and a
        // two-byte one (#1600).
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            r = []
            [[0x92, 0xB0, 0xEC], [0x90, 0xB0, 0xEC], [0x91, 0xB0, 0xEC],
             [0x99, 0xB0, 0xEC], [0x81, 0xA1], [0x81, 0x41], [0x8E, 0xA1],
             [0x92, 0xB0], [0x92], [0x80], [0xA4, 0xA2],
             [0x41, 0x92, 0xB0, 0xEC, 0x42]].each do |bytes|
              s = bytes.pack("C*").force_encoding(st)
              r << [bytes.map { |b| "%02X" % b }.join, s.valid_encoding?, s.chars.size,
                    s.chars.map { |c| c.bytesize }]
            end
            r
            "##,
        );
    }

    #[test]
    fn the_jis_family_converts_cell_to_cell_through_iso_2022_jp() {
        // The family is a line — ISO-2022-JP — stateless-ISO-2022-JP —
        // EUC-JP — Shift_JIS — with UTF-8 hanging off EUC-JP's end, so
        // a conversion inside it never asks Unicode. ISO-2022-JP was
        // outside that line, going through `encoding_rs`'s WHATWG
        // table instead: cells Unicode has no character for could not
        // reach it (#1609) and 343 characters CRuby refuses could
        // (#1612).
        crate::tests::run_test_once(
            r##"
            [["EUC-JP","ISO-2022-JP"],["ISO-2022-JP","EUC-JP"],["Shift_JIS","ISO-2022-JP"],
             ["ISO-2022-JP","Shift_JIS"],["EUC-JP","Shift_JIS"],["UTF-8","ISO-2022-JP"],
             ["ISO-2022-JP","UTF-8"],["stateless-ISO-2022-JP","EUC-JP"],
             ["ISO-2022-JP","stateless-ISO-2022-JP"],["stateless-ISO-2022-JP","ISO-2022-JP"]].map do |a, b|
              begin
                Encoding::Converter.new(a, b).convpath.map { |x| x.is_a?(Array) ? x.map(&:to_s) : x.to_s }
              rescue
                $!.class.to_s
              end
            end
            "##,
        );
        // A cell with no character crosses like any other, and the
        // duplicate-mapping rows keep JIS's reading rather than
        // Windows's.
        crate::tests::run_test_once(
            r##"
            [[0xA2,0xAF],[0xA1,0xBD],[0xA1,0xC1],[0xA1,0xF1],[0xA2,0xCC],[0xB0,0xEC],[0xFE,0xFE]].map do |l, t|
              e = [l, t].pack("C*").force_encoding("EUC-JP")
              [("%02X%02X" % [l, t]),
               begin; e.dup.encode("ISO-2022-JP").bytes; rescue; $!.class.to_s; end,
               begin; e.dup.encode("Shift_JIS").encode("ISO-2022-JP").bytes; rescue; $!.class.to_s; end,
               begin
                 Encoding::Converter.new("EUC-JP", "ISO-2022-JP").convert(e.dup).bytes
               rescue
                 $!.class.to_s
               end]
            end
            "##,
        );
        // Half-width katakana and JIS X 0212 have no ISO-2022-JP cell,
        // and no longer quietly become full-width ones.
        crate::tests::run_test_once(
            r##"
            r = []
            ["\u{FF71}", "\u{FF61}", "\u{2116}", "\u{00A5}", "\u{203E}"].each do |c|
              r << [c.codepoints, begin; c.encode("ISO-2022-JP").bytes; rescue; $!.class.to_s; end]
            end
            r << ("\u{4E00}\u{3042}".encode("ISO-2022-JP").bytes)
            r
            "##,
        );
        // The escape grammar, and the designation surviving a chunk:
        // it stays in effect across `#convert`, and the closing escape
        // comes from `#finish`.
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            r = []
            [[0x1B,0x24,0x42,0x30,0x6C,0x1B,0x28,0x42], [0x1B,0x24,0x42,0x30,0x6C],
             [0x1B,0x24,0x40,0x30,0x6C,0x1B,0x28,0x42], [0x1B,0x28,0x4A,0x5C,0x1B,0x28,0x42],
             [0x1B,0x24,0x42,0x7E,0x7E,0x1B,0x28,0x42], [0x1B,0x24,0x42,0x20,0x21],
             [0x1B,0x24,0x42,0x21,0x7F], [0x1B,0x24,0x49,0x21], [0x1B], [0x1B,0x24], [0x80]].each do |bytes|
              s = bytes.pack("C*").force_encoding(e)
              r << [bytes.map { |b| "%02X" % b }.join,
                    begin; s.encode("stateless-ISO-2022-JP").bytes; rescue; [$!.class.to_s, $!.message]; end,
                    begin; s.encode("UTF-8").codepoints; rescue; $!.class.to_s; end]
            end
            ec = Encoding::Converter.new("UTF-8", e)
            r << "\u{4E00}\u{3042}".each_char.map { |c| ec.convert(c).bytes }
            r << ec.finish.bytes
            ec2 = Encoding::Converter.new(e, "UTF-8")
            d = +""
            r << [ec2.primitive_convert([0x1B,0x24].pack("C*").dup.force_encoding(e), d, nil, nil, partial_input: true), d.bytes]
            r << [ec2.primitive_convert([0x42,0x30,0x6C].pack("C*").dup.force_encoding(e), d, nil, nil, partial_input: true), d.bytes]
            r << [ec2.primitive_convert("".dup.force_encoding(e), d), d.bytes]
            r
            "##,
        );
    }

    #[test]
    fn iso_2022_jp_stops_where_the_destination_fills_not_where_the_error_is() {
        // The source is read in order, so a cap that stops among the
        // bytes *before* a malformed run reports the full destination
        // and leaves the run for the next call to trip over (#1609).
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            [[0x41,0x80,0x42], [0x1B,0x24,0x42,0x30,0x6C,0x80], [0x1B,0x24,0x49,0x21]].map do |bytes|
              [bytes.map { |b| "%02X" % b }.join,
               (0..4).map do |cap|
                 ec = Encoding::Converter.new(e, "UTF-8")
                 d = +""; s = bytes.pack("C*").force_encoding(e)
                 r = ec.primitive_convert(s, d, nil, cap)
                 [r, d.bytes, s.bytes,
                  ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
               end]
            end
            "##,
        );
        // Stopping means stopping: an escape sequence that follows the
        // character the destination could not hold writes nothing, but
        // it is never read, so it stays in `src`.
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            r = []
            { "cell+esc+ascii" => [0x1B,0x24,0x42,0x30,0x6C,0x1B,0x28,0x42,0x41,0x42],
              "cell+esc"       => [0x1B,0x24,0x42,0x30,0x6C,0x1B,0x28,0x42],
              "ascii+esc+cell" => [0x41,0x1B,0x24,0x42,0x30,0x6C] }.each do |name, bytes|
              (0..7).each do |cap|
                ec = Encoding::Converter.new(e, "UTF-8")
                s = bytes.pack("C*").force_encoding(e); d = +""
                r << [name, cap, ec.primitive_convert(s, d, nil, cap), d.bytes, s.bytes]
              end
              ec = Encoding::Converter.new(e, "US-ASCII")
              s = bytes.pack("C*").force_encoding(e); d = +""
              r << [name, ec.primitive_convert(s, d), d.bytes, s.bytes,
                    ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            r
            "##,
        );
    }

    #[test]
    fn an_iso_2022_jp_destination_reads_through_the_character_that_did_not_fit() {
        // A cap stops the encoder on the character whose escape
        // sequence and cell do not both fit; that character is read,
        // the bytes after it are not, and the output already written
        // is held for the next call rather than left in `src` (#1609).
        crate::tests::run_test_once(
            r##"
            s = "stateless-ISO-2022-JP"
            [[0x41,0x92,0xA4,0xA2,0x42], [0x92,0xA4,0xA2,0x92,0xA4,0xA4],
             [0x41,0x80,0x42], [0x41,0x92,0xA4,0xA2,0x80,0x42],
             [0x41,0x91,0xA4,0xA2,0x42]].map do |bytes|
              [bytes.map { |b| "%02X" % b }.join,
               (0..11).map do |cap|
                 ec = Encoding::Converter.new(s, "ISO-2022-JP")
                 src = bytes.pack("C*").force_encoding(s); d = +""
                 [cap, ec.primitive_convert(src, d, nil, cap), d.bytes, src.bytes]
               end]
            end
            "##,
        );
        // A destination filled to the last byte is still full when
        // the stream ends in ASCII — the closing reset has nowhere to
        // run — and finished when the closing escape was what filled
        // it.
        crate::tests::run_test_once(
            r##"
            [["UTF-8", "EUC-JP", "ab"], ["UTF-8", "ISO-2022-JP", "ab"],
             ["stateless-ISO-2022-JP", "ISO-2022-JP", "ab"],
             ["UTF-8", "UTF-16BE", "ab"]].map do |s, d, str|
              bytes = str.dup.force_encoding(s)
              full = Encoding::Converter.new(s, d).convert(bytes.dup).bytesize
              [s, d, [full - 1, full, full + 1].map do |cap|
                ec = Encoding::Converter.new(s, d); src = bytes.dup; dst = +""
                [cap, ec.primitive_convert(src, dst, nil, cap), dst.bytes, src.bytes]
              end]
            end
            "##,
        );
    }

    #[test]
    fn a_source_bound_for_iso_2022_jp_maps_a_cap_back_through_the_line() {
        // The cap counts ISO-2022-JP bytes, so a source that is not
        // already stateless-ISO-2022-JP has to map the offset back
        // along its own hop to say what it read — with or without a
        // malformed run waiting past it (#1609).
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            [["EUC-JP", [0x41,0xA4,0xA2,0x42]], ["Shift_JIS", [0x41,0x82,0xA0,0x42]],
             ["UTF-8", [0x41,0xE3,0x81,0x82,0x42]], ["CP949", [0x41,0x42]],
             ["Shift_JIS", [0x41,0x82,0xA0,0x80,0x42]],
             ["UTF-8", [0x41,0xE3,0x81,0x82,0x80,0x42]]].map do |src, bytes|
              [src, bytes.map { |b| "%02X" % b }.join,
               (0..12).map do |cap|
                 ec = Encoding::Converter.new(src, e)
                 s = bytes.pack("C*").force_encoding(src); d = +""
                 [cap, ec.primitive_convert(s, d, nil, cap), d.bytes, s.bytes,
                  ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
               end]
            end
            "##,
        );
        // An ISO-2022-JP source that stops mid-escape is incomplete,
        // not malformed, and says so whatever the destination.
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            r = []
            [[0x41,0x1B], [0x41,0x1B,0x24], [0x1B,0x24,0x42,0x30]].each do |bytes|
              [nil, 2].each do |cap|
                ["UTF-8", "stateless-ISO-2022-JP"].each do |d|
                  ec = Encoding::Converter.new(e, d)
                  s = bytes.pack("C*").force_encoding(e); dst = +""
                  r << [bytes.map { |b| "%02X" % b }.join, cap, d,
                        ec.primitive_convert(s, dst, nil, cap), dst.bytes, s.bytes,
                        ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
                end
                ec = Encoding::Converter.new(e, "UTF-8")
                s = bytes.pack("C*").force_encoding(e); dst = +""
                r << [bytes.map { |b| "%02X" % b }.join, cap, "partial",
                      ec.primitive_convert(s, dst, nil, cap, partial_input: true),
                      dst.bytes, s.bytes]
              end
            end
            r
            "##,
        );
    }

    #[test]
    fn an_error_writing_iso_2022_jp_does_not_close_the_designation() {
        // The closing `ESC ( B` belongs to the end of the stream, and
        // an error is not one: writing it there put three bytes into
        // the destination that CRuby does not (#1609).
        crate::tests::run_test_once(
            r##"
            [["Shift_JIS", [0x41,0x82,0xA0,0x80,0x42]], ["UTF-8", [0x41,0xE3,0x81,0x82,0x80,0x42]],
             ["stateless-ISO-2022-JP", [0x41,0x92,0xA4,0xA2,0x80,0x42]]].map do |src, bytes|
              ec = Encoding::Converter.new(src, "ISO-2022-JP")
              s = bytes.pack("C*").force_encoding(src); d = +""
              [src, ec.primitive_convert(s, d), d.bytes, s.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
    }

    #[test]
    fn a_capped_stateless_destination_holds_the_character_it_cut() {
        // Whatever the cap, a loop that keeps calling until it is
        // finished writes the same bytes: the half of a character
        // that did not fit is held for the next call, not dropped
        // (#1532).
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"; s = "stateless-ISO-2022-JP"
            [[0x41,0x42,0x43], [0x41,0x1B,0x24,0x42,0x30,0x6C,0x1B,0x28,0x42,0x42],
             [0x1B,0x24,0x42,0x30,0x6C,0x30,0x6D], [0x1B,0x24,0x42,0x30,0x6C,0x1B,0x28,0x42]].map do |bytes|
              [bytes.map { |b| "%02X" % b }.join,
               (1..4).map do |cap|
                 ec = Encoding::Converter.new(e, s)
                 src = bytes.pack("C*").force_encoding(e); out = +""; n = 0
                 loop do
                   d = +""
                   r = ec.primitive_convert(src, d, nil, cap)
                   out << d; n += 1
                   break if r == :finished || n > 50
                   break unless r == :destination_buffer_full
                 end
                 [cap, out.bytes]
               end]
            end
            "##,
        );
    }

    #[test]
    fn iso_2022_jp_names_the_hop_that_read_the_source() {
        // The escape sequences come off first, so a malformed run is
        // reported against `ISO-2022-JP → stateless-ISO-2022-JP` —
        // never against the conversion's real destination (#1609).
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            ["UTF-8", "EUC-JP", "Shift_JIS", "stateless-ISO-2022-JP", "UTF-16BE",
             "ISO-8859-1", "CP949"].map do |d|
              [[0x41,0x80,0x42], [0x1B,0x24,0x49,0x21,0x21]].map do |bytes|
                ec = Encoding::Converter.new(e, d)
                s = bytes.pack("C*").force_encoding(e); dst = +""
                [d, ec.primitive_convert(s, dst), dst.bytes, s.bytes,
                 ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
              end
            end
            "##,
        );
        // The same rule read from the other end: the hop that reads a
        // source *bound* for the family is its own first hop, which is
        // EUC-JP for everything that reaches ISO-2022-JP through it.
        // What was converted before the malformed run is written, not
        // thrown away with it.
        crate::tests::run_test_once(
            r##"
            [["UTF-8", [0x41,0x80,0x42]], ["Shift_JIS", [0x41,0x80,0x42]],
             ["stateless-ISO-2022-JP", [0x41,0x80,0x42]],
             ["stateless-ISO-2022-JP", [0x41,0x91,0xA4,0xA2,0x42]],
             ["CP949", [0x41,0x80,0x42]]].map do |src, bytes|
              (["ISO-2022-JP", "stateless-ISO-2022-JP"] - [src]).map do |d|
                ec = Encoding::Converter.new(src, d)
                s = bytes.pack("C*").force_encoding(src); dst = +""
                [src, d, ec.primitive_convert(s, dst), dst.bytes, s.bytes,
                 ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
              end
            end
            "##,
        );
    }

    #[test]
    fn a_newline_decorator_runs_before_an_ascii_incompatible_encoder() {
        // The decorator rewrites LF, so CRuby puts it before the hop
        // that writes ISO-2022-JP's escape sequences or UTF-16's code
        // units, and after everything otherwise (#1609).
        crate::tests::run_test_once(
            r##"
            ["ISO-2022-JP", "UTF-16BE", "UTF-16LE", "UTF-32BE", "UTF-32LE", "EUC-JP",
             "Shift_JIS", "Windows-31J", "stateless-ISO-2022-JP"].map do |d|
              [d, Encoding::Converter.new("UTF-8", d, crlf_newline: true).convpath
                    .map { |x| x.is_a?(Array) ? x.map(&:to_s) : x.to_s },
                  Encoding::Converter.search_convpath("ISO-2022-JP", "EUC-JP", crlf_newline: true)
                    .map { |x| x.is_a?(Array) ? x.map(&:to_s) : x.to_s }]
            end
            "##,
        );
    }

    #[test]
    fn iso_2022_jp_substitutes_a_malformed_run_in_the_set_it_was_in() {
        // `invalid: :replace` on a stateful source has to resume in
        // the designation that was in effect: the byte after a
        // substituted one is still that character set's, not ASCII
        // (#1609).
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            [[0x41,0x80,0x42], [0x1B,0x24,0x42,0x30,0x6C,0x80,0x1B,0x28,0x42],
             [0x1B,0x24,0x49,0x21,0x1B,0x28,0x42], [0x1B,0x24,0x42,0x20,0x21,0x1B,0x28,0x42],
             [0x1B,0x24,0x42,0x21,0x7F,0x1B,0x28,0x42], [0x41,0x1B,0x24,0x42,0x30,0x6C]].map do |bytes|
              s = bytes.pack("C*").force_encoding(e)
              [bytes.map { |b| "%02X" % b }.join,
               ["UTF-8", "EUC-JP", "stateless-ISO-2022-JP"].map do |d|
                 [begin; s.dup.encode(d, invalid: :replace).bytes; rescue; $!.class.to_s; end,
                  begin
                    Encoding::Converter.new(e, d, invalid: :replace).convert(s.dup).bytes
                  rescue
                    $!.class.to_s
                  end]
               end]
            end
            "##,
        );
        // An *incomplete* escape is only malformed once the input has
        // ended: `#convert` holds it and `#finish` substitutes it, so
        // the converter cannot take the single-shot shortcut that
        // sees one whole input.
        crate::tests::run_test_once(
            r##"
            e = "ISO-2022-JP"
            r = []
            [[0x1B], [0x1B, 0x24], [0x41, 0x1B, 0x24]].each do |bytes|
              s = bytes.pack("C*").force_encoding(e)
              ec = Encoding::Converter.new(e, "UTF-8", invalid: :replace)
              r << [bytes.map { |b| "%02X" % b }.join, ec.convert(s.dup).bytes, ec.finish.bytes]
              ec2 = Encoding::Converter.new(e, "UTF-8", invalid: :replace)
              d = +""
              r << [ec2.primitive_convert(s.dup, d), d.bytes]
            end
            r
            "##,
        );
        // A malformed run in a stateless source bound for ISO-2022-JP
        // is settled at that hop too.
        crate::tests::run_test_once(
            r##"
            [[0x41,0x81,0xA1,0x42], [0x41,0x8E,0xA1]].map do |bytes|
              s = bytes.pack("C*").force_encoding("stateless-ISO-2022-JP")
              [bytes.map { |b| "%02X" % b }.join,
               begin; s.dup.encode("ISO-2022-JP").bytes; rescue; [$!.class.to_s, $!.message]; end,
               begin; s.dup.encode("ISO-2022-JP", invalid: :replace).bytes; rescue; $!.class.to_s; end]
            end
            "##,
        );
    }

    #[test]
    fn stateless_iso_2022_jp_converts_through_euc_jp() {
        // It is ISO-2022-JP with the character set named by a lead
        // byte instead of by an escape sequence still in effect, and
        // the cell after it written as EUC-JP writes it. `0x90` and
        // `0x92` are JIS X 0208, 1978 and 1983, mapping to the same
        // cells; encoding always writes `0x92` (#1600).
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            ["\u{4E00}", "\u{3042}", "\u{FF21}", "\u{2015}", "?", "A"].map do |c|
              to = begin; c.encode(st).bytes; rescue; [$!.class.to_s, $!.message]; end
              # Both JIS X 0208 leads read back to the same character.
              read = [0x92, 0x90].map do |lead|
                begin
                  e = c.encode("EUC-JP")
                  bytes = e.bytesize == 1 ? e.b : ([lead].pack("C") + e).b
                  bytes.force_encoding(st).encode("UTF-8").codepoints
                rescue
                  $!.class.to_s
                end
              end
              [c.codepoints, to, read, read[0] == read[1]]
            end
            "##,
        );
        // Half-width katakana and JIS X 0212 reach EUC-JP and stop
        // there, so the error quotes the EUC-JP bytes and names that
        // hop; a character EUC-JP itself has no cell for stops one
        // hop earlier and is named by codepoint. Both spell the chain
        // unless the source was EUC-JP.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            [["UTF-8", "\u{00A5}"], ["UTF-8", "\u{FF71}"], ["UTF-8", "\u{02D8}"],
             ["UTF-8", "\u{1F600}"], ["Shift_JIS", "\u{FF71}"], ["EUC-JP", "\u{FF71}"],
             ["EUC-JP", "\u{02D8}"], ["Windows-31J", "\u{2460}"],
             ["UTF-16BE", "\u{00A5}"], ["ISO-8859-1", "\u{00A5}"]].map do |se, c|
              src = begin; c.encode(se); rescue; nil; end
              next [se, c.codepoints, "SRC-UNENCODABLE"] if src.nil?
              [se, c.codepoints,
               begin; src.encode(st).bytes; rescue; [$!.class.to_s, $!.message]; end,
               begin; src.dup.encode(st, undef: :replace).bytes; rescue; $!.class.to_s; end]
            end
            "##,
        );
        // The malformed run is the transcoder's, not the walk's:
        // `0x91` is a lead the walk accepts and the transcoder does
        // not, so it is one byte on its own, while `0x92` holds what
        // it has read and reads the disproving byte again.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            [[0xA4,0xA2],[0x91],[0x91,0xB0],[0x91,0xB0,0xEC],[0x92],[0x92,0xB0],
             [0x92,0x41],[0x92,0xB0,0x41],[0x92,0xA0,0xA1],[0x92,0xA1,0xA0],
             [0x80],[0x8E,0xA1],[0x81,0xA1],[0xFF],[0x99,0xB0,0xEC],
             [0x41,0xA4,0xA2,0x42]].map do |bytes|
              s = bytes.pack("C*").force_encoding(st)
              ec = Encoding::Converter.new(st, "UTF-8")
              d = +""
              res = ec.primitive_convert(s.dup, d)
              [bytes.map { |b| "%02X" % b }.join, res, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x },
               begin; s.encode("UTF-8"); "ok"; rescue; [$!.class.to_s, $!.message]; end]
            end
            "##,
        );
        // EUC-JP is the one partner this *is* the conversion for —
        // the two hold the same cells, so one with no character
        // crosses like any other and no codec is asked.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            [[0xA2,0xAF],[0xB0,0xEC],[0xA1,0xBD],[0xFE,0xFE]].map do |l, t|
              s = [0x92, l, t].pack("C*").force_encoding(st)
              e = [l, t].pack("C*").force_encoding("EUC-JP")
              [("%02X%02X" % [l, t]),
               begin; s.dup.encode("EUC-JP").bytes; rescue; $!.class.to_s; end,
               begin; Encoding::Converter.new(st, "EUC-JP").convert(s.dup).bytes; rescue; $!.class.to_s; end,
               begin; e.dup.encode(st).bytes; rescue; $!.class.to_s; end,
               begin; Encoding::Converter.new("EUC-JP", st).convert(e.dup).bytes; rescue; $!.class.to_s; end]
            end
            "##,
        );
        // The converter *into* it, which the one-shot path does not
        // reach: a destination cap holds what will not fit and takes
        // exactly the source bytes that were read, and a cell with no
        // stateless home is an undefined conversion named against the
        // EUC-JP hop.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            r = []
            ["UTF-8", "EUC-JP", "Shift_JIS"].each do |src|
              s = "a\u{4E00}b\u{3042}".encode(src)
              ec = Encoding::Converter.new(src, st)
              r << [src, ec.convert(s.dup).bytes, ec.finish.bytes]
              (1..6).each do |cap|
                e2 = Encoding::Converter.new(src, st)
                d = +""
                t = s.dup
                r << [src, cap, e2.primitive_convert(t, d, nil, cap), d.bytes, t.bytes]
              end
            end
            [[0x8E, 0xB1], [0x8F, 0xA2, 0xAF]].each do |bytes|
              s = bytes.pack("C*").force_encoding("EUC-JP")
              ec = Encoding::Converter.new("EUC-JP", st)
              d = +""
              r << [bytes.map { |x| "%02X" % x }.join, ec.primitive_convert(s.dup, d), d.bytes,
                    ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
              r << begin
                Encoding::Converter.new("EUC-JP", st, undef: :replace).convert(s.dup).bytes
              rescue
                $!.class.to_s
              end
            end
            r
            "##,
        );
        // The converter *from* it: a destination cap fills to the byte
        // and holds the rest of that character, taking the whole of it
        // out of `src`, and a malformed run still stops the stream
        // where the transcoder says.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            good = [0x41, 0x92, 0xB0, 0xEC, 0x42].pack("C*").force_encoding(st)
            r = []
            (0..6).each do |cap|
              ec = Encoding::Converter.new(st, "EUC-JP")
              d = +""
              t = good.dup
              r << [cap, ec.primitive_convert(t, d, nil, cap), d.bytes, t.bytes]
            end
            [[0x41, 0x80], [0x41, 0x92], [0x41, 0x92, 0xB0], [0x92, 0xA0, 0xA1]].each do |bytes|
              s = bytes.pack("C*").force_encoding(st)
              ec = Encoding::Converter.new(st, "EUC-JP")
              d = +""
              r << [bytes.map { |b| "%02X" % b }.join, ec.primitive_convert(s.dup, d), d.bytes,
                    ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            ["UTF-8", "EUC-JP", "Shift_JIS"].each do |d|
              bad = [0x41, 0x92, 0xB0, 0xEC, 0x80, 0x42].pack("C*").force_encoding(st)
              r << [d, begin
                Encoding::Converter.new(st, d, invalid: :replace).convert(bad.dup).bytes
              rescue
                $!.class.to_s
              end]
            end
            r
            "##,
        );
        // A malformed run in a stateless source is settled at that
        // hop, so `invalid: :replace` puts the *destination's*
        // replacement between the runs either side — it never has to
        // be spellable in EUC-JP, which routing it through the pivot
        // would have required.
        crate::tests::run_test_once(
            r##"
            st = "stateless-ISO-2022-JP"
            s = [0x41, 0x92, 0xB0, 0xEC, 0x80, 0x42].pack("C*").force_encoding(st)
            [begin; s.dup.encode("UTF-8", invalid: :replace).codepoints; rescue; $!.class.to_s; end,
             begin
               Encoding::Converter.new(st, "UTF-8", invalid: :replace).convert(s.dup).codepoints
             rescue
               $!.class.to_s
             end,
             begin; s.dup.encode("EUC-JP", invalid: :replace).bytes; rescue; $!.class.to_s; end,
             begin; s.dup.encode("UTF-8"); "ok"; rescue; [$!.class.to_s, $!.message]; end]
            "##,
        );
        // With a converter to it, ISO-2022-JP's replacement lands
        // where CRuby puts it: the input of the conversion's last
        // step, which for that destination is stateless-ISO-2022-JP
        // rather than ISO-2022-JP itself (#1583 left it with the
        // destination because there was nowhere else to put it).
        crate::tests::run_test_once(
            r##"
            c = Encoding::Converter.new("UTF-8", "ISO-2022-JP")
            r = [[c.replacement.bytes, c.replacement.encoding.name]]
            c.replacement = "\u{4E00}"
            r << [c.replacement.bytes, c.replacement.encoding.name]
            c.replacement = "?"
            r << [c.replacement.bytes, c.replacement.encoding.name]
            r << Encoding::Converter.asciicompat_encoding("ISO-2022-JP").name
            r
            "##,
        );
    }

    #[test]
    fn the_euc_jp_family_converts_under_its_own_name() {
        // Naming an encoding monoruby only approximates is safe here
        // because the cases it cannot do *raise*: every byte it
        // produces is CRuby's, and the vendor-extension characters
        // CRuby maps through the per-vendor tables (U+FF5E, U+2460,
        // U+3231) are an honest UndefinedConversionError rather than
        // EUC-JP's bytes under another name. That is what the UTF-8
        // family fails — it silently yields plain UTF-8 — and why
        // those names are still not preserved (#1562).
        crate::tests::run_test_once(
            r##"
            ["A", "\uFFE5", "\u3042", "\u4E2D"].map { |ch|
              [ch] + %w[EUC-JP eucJP-ms CP51932 EUC-JIS-2004].map { |n|
                t = ch.encode(n)
                [t.encoding.name, t.bytes.map { |b| "%02X" % b }.join]
              }
            }
            "##,
        );
    }

    #[test]
    fn iso_2022_jp_is_a_dummy_encoding() {
        // Stateful, so Ruby gives it no character decoder: every byte
        // string labelled with it is valid, and `length` counts bytes.
        // monoruby used to decode it through `encoding_rs`, which made
        // the same bytes Broken here and Valid as CP50220 — its own
        // variant — and `length` 2 where `chars.size` was 10 (#1554).
        crate::tests::run_test_once(
            r##"
            s = "\xE6\x9D\x94".b
            j = "\e$B4A;z\e(B".b
            %w[ISO-2022-JP CP50220 CP50221 UTF-7].map { |e|
              t = s.dup.force_encoding(e)
              [e, t.valid_encoding?, t.length, t.chars.size]
            } + %w[ISO-2022-JP CP50220].map { |e|
              t = j.dup.force_encoding(e)
              [e, t.valid_encoding?, t.length, t.chars.size]
            }
            "##,
        );
    }

    #[test]
    fn force_encoding_never_answers_a_name_with_another_encoding() {
        // The two resolvers have to agree: `Encoding.find` reads the
        // registry, `force_encoding` the enum's own alias table, and
        // the table used to map `ISO-2022-JP-2` / `-KDDI` / `-2004`
        // onto plain ISO-2022-JP. So a name `Encoding.find` rejected
        // silently relabelled the string as an encoding the caller
        // never asked for (#1554). Asserting they agree rather than
        // what they answer keeps this about that bug — which names
        // are *registered* is #1555.
        crate::tests::run_test_once(
            r##"
            # The carrier-emoji encodings are the ones left out: their
            # conversion is a vendor table monoruby does not carry, so
            # naming them would silently hand back plain UTF-8 or
            # Shift_JIS (#1562).
            names = %w[ISO-2022-JP ISO2022-JP ISO-2022-JP-2 ISO-2022-JP-KDDI
                       ISO-2022-JP-2004 CP50220 CP50221 UTF-7
                       UTF-8 Big5-HKSCS Shift_JIS Windows-31J MacJapanese
                       EUC-JP eucJP eucJP-ms euc-jp-ms CP51932
                       stateless-ISO-2022-JP stateless-ISO-2022-JP-KDDI
                       EUC-JIS-2004 EUC-JISX0213
                       UTF8-MAC UTF8_MAC UTF-8-MAC UTF-8-HFS
                       CESU-8 CESU8 NOPE]
            names.map { |n|
              found = (begin; Encoding.find(n).name; rescue ArgumentError; nil; end)
              forced = (begin; "abc".b.force_encoding(n).encoding.name; rescue ArgumentError; nil; end)
              [n, found == forced]
            }
            "##,
        );
    }

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
    fn a_converter_streams_the_pivot_wrappers() {
        // CESU-8 and `UTF8-MAC` are the ordinary pipeline with a
        // rewrite on one side, so a converter runs them a chunk at a
        // time. Before #1576 the CESU-8 pair was refused outright and
        // the `UTF8-MAC` one was built and then handed its bytes
        // through unconverted.
        run_tests(&[
            r#"Encoding::Converter.new("UTF-8", "CESU-8").convert("aあ\u{1F600}").bytes"#,
            r#"Encoding::Converter.new("CESU-8", "UTF-8").convert("aあ\u{1F600}".encode("CESU-8")).bytes"#,
            r#"Encoding::Converter.new("UTF-8", "UTF8-MAC").convert("が").bytes"#,
            r#"Encoding::Converter.new("CESU-8", "EUC-JP").convert("あ".encode("CESU-8")).bytes"#,
            r#"
              c = Encoding::Converter.new("UTF-8", "CESU-8")
              [c.convert("a").bytes, c.convert("\u{1F600}").bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF8-MAC")
              [c.convert("a").bytes, c.convert("が").bytes, c.finish.bytes]
            "#,
            r#"Encoding::Converter.new("UTF-8", "CESU-8").convert("a").encoding.to_s"#,
        ]);
    }

    #[test]
    fn a_utf8_mac_source_holds_its_trailing_cluster() {
        // The next chunk may open with a mark that composes onto the
        // character before it, so a streamed `UTF8-MAC` source keeps
        // the last one back until `#finish` says no more is coming.
        // That is why even `"abc"` comes out as `"ab"` and then `"c"`,
        // and why a mark split across two chunks still composes
        // (#1576).
        run_tests(&[
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              [c.convert("abc".b.force_encoding("UTF8-MAC")).bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              [c.convert("あか".b.force_encoding("UTF8-MAC")).bytes,
               c.convert("゙い".b.force_encoding("UTF8-MAC")).bytes,
               c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              [c.convert("が".b.force_encoding("UTF8-MAC")).bytes, c.finish.bytes]
            "#,
            // Half a character at the end is held for a different
            // reason, and `#finish` is where it becomes an error.
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              c.convert("a\xE3\x81".b.force_encoding("UTF8-MAC")).bytes
              begin; c.finish; rescue => e; [e.class.name, e.message]; end
            "#,
        ]);
    }

    #[test]
    fn a_converter_writes_the_dummies_bom_once() {
        // `UTF-16` and `UTF-32` name no byte order of their own, so
        // CRuby's encoder writes a BOM and then the big-endian form.
        // A stream writes that BOM ahead of the first character it
        // emits and never again — so a call that emits nothing, and
        // a `#finish` with nothing held, write none at all (#1576).
        run_tests(&[
            r#"Encoding::Converter.new("UTF-8", "UTF-16").convert("aあ").bytes"#,
            r#"Encoding::Converter.new("UTF-8", "UTF-32").convert("aあ").bytes"#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-16")
              [c.convert("a").bytes, c.convert("あ").bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-32")
              [c.convert("").bytes, c.convert("a").bytes, c.finish.bytes]
            "#,
            r#"Encoding::Converter.new("UTF-8", "UTF-16").finish.bytes"#,
            r#"Encoding::Converter.new("UTF-8", "UTF-16").convert("a").encoding.to_s"#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-16")
              s = +"ab"
              d = +""
              [c.primitive_convert(s, d), d.bytes]
            "#,
        ]);
    }

    #[test]
    fn a_converter_reads_the_dummies_bom_once() {
        // The other way round: the BOM opens the stream, names the
        // byte order, and is consumed — the chunks after it carry
        // none, so the converter has to remember what it said. A
        // stream that does not open with one at all is ill-formed
        // (#1576).
        run_tests(&[
            r#"Encoding::Converter.new("UTF-16", "UTF-8").convert("aあ".encode("UTF-16")).bytes"#,
            r#"Encoding::Converter.new("UTF-32", "UTF-8").convert("aあ".encode("UTF-32")).bytes"#,
            r#"
              c = Encoding::Converter.new("UTF-16", "UTF-8")
              [c.convert("\xFE".b.force_encoding("UTF-16")).bytes,
               c.convert("\xFF\x30\x42".b.force_encoding("UTF-16")).bytes,
               c.convert("\x30\x44".b.force_encoding("UTF-16")).bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-16", "UTF-8")
              [c.convert("\xFF\xFE\x42\x30".b.force_encoding("UTF-16")).bytes,
               c.convert("\x44\x30".b.force_encoding("UTF-16")).bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-16", "UTF-8")
              begin
                c.convert("\x30\x42\x30\x44".b.force_encoding("UTF-16"))
              rescue => e
                [e.class.name, e.message]
              end
            "#,
        ]);
    }

    #[test]
    fn half_a_surrogate_pair_is_incomplete_not_malformed() {
        // A big-endian high surrogate followed by the first byte of
        // what could still be its partner is three quarters of a pair,
        // not a malformed unit — and a stream fed by the byte is
        // exactly how it arrives. The byte that *cannot* open a low
        // surrogate is still the malformed one, read again (#1576).
        run_tests(&[
            r#"
              c = Encoding::Converter.new("UTF-16BE", "UTF-8")
              s = "\u{1F600}".encode("UTF-16BE")
              [s.bytes.map { |b| c.convert(b.chr.force_encoding("UTF-16BE")).bytes }, c.finish.bytes]
            "#,
            r#"
              begin
                "\xD8\x3D\x00\x61".b.force_encoding("UTF-16BE").encode("UTF-8")
              rescue => e
                [e.class.name, e.message]
              end
            "#,
        ]);
    }

    #[test]
    fn what_a_utf8_mac_source_holds_back_and_what_it_does_not() {
        // Only a character a mark can attach to is worth keeping: in
        // CRuby's table that is the Basic Multilingual Plane, so an
        // astral character at the end of a chunk goes out with the
        // rest of it. And a chunk that ends mid-character must not
        // settle the cluster in front of it either, or the
        // composition the next chunk was going to complete is lost
        // (#1576).
        run_tests(&[
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              [c.convert("a\u{1F600}".b.force_encoding("UTF8-MAC")).bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              [c.convert("a漢".b.force_encoding("UTF8-MAC")).bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "UTF-8")
              s = "が".b.force_encoding("UTF8-MAC")
              [s.bytes.each_slice(2).map { |g| c.convert(g.pack("C*").force_encoding("UTF8-MAC")).bytes },
               c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "EUC-JP")
              s = "が".b.force_encoding("UTF8-MAC")
              [s.bytes.each_slice(2).map { |g| c.convert(g.pack("C*").force_encoding("UTF8-MAC")).bytes },
               c.finish.bytes]
            "#,
        ]);
    }

    #[test]
    fn a_streamed_error_names_the_hops_the_conversion_took() {
        // `UTF8-MAC` holds UTF-8's bytes, so `is_utf8_compatible`
        // called it the pivot and the message lost a hop; a dummy
        // resolved to the end its BOM named and the message reported
        // that instead of the encoding the converter was built with
        // (#1576).
        run_tests(&[
            r#"
              begin
                Encoding::Converter.new("UTF8-MAC", "US-ASCII").convert("あ".encode("UTF8-MAC"))
              rescue => e
                [e.class.name, e.message]
              end
            "#,
            r#"
              begin
                Encoding::Converter.new("UTF-16", "EUC-JP").convert("\u{1F600}".encode("UTF-16"))
              rescue => e
                [e.class.name, e.message]
              end
            "#,
            r#"
              begin
                Encoding::Converter.new("CESU-8", "US-ASCII").convert("あ".encode("CESU-8"))
              rescue => e
                [e.class.name, e.message]
              end
            "#,
        ]);
    }

    #[test]
    fn a_capped_destination_stops_where_it_can_through_the_wrappers() {
        // The rewrite runs on whole units, so a cap that cuts one
        // fills the destination to the byte and holds the rest for the
        // next call — and the BOM is output like any other: a cap too
        // small for it writes what fits rather than nothing at all,
        // which is what a caller looping to `:finished` needs (#1576).
        run_tests(&[
            r#"
              [["UTF-8","CESU-8"],["CESU-8","UTF-8"],["UTF-8","UTF8-MAC"],["UTF8-MAC","UTF-8"]].map do |s, d|
                c = Encoding::Converter.new(s, d)
                src = "aあb".encode(s).dup
                dst = +""
                [c.primitive_convert(src, dst, nil, 3), dst.bytes, src.bytes]
              end
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF-32")
              src = +"aあb"
              dst = +""
              [c.primitive_convert(src, dst, nil, 3), dst.bytes, src.bytes]
            "#,
        ]);
    }

    #[test]
    fn a_wrapper_source_reports_and_replaces_what_it_cannot_read() {
        // CESU-8's walk is its converter, so a lone surrogate half is
        // a malformed sequence there and `invalid: :replace` scrubs it
        // with the destination's replacement. A `UTF8-MAC` or dummy
        // source in replacement mode cannot take the single-shot path
        // the other encodings do — it has a BOM or a held cluster to
        // remember — so it streams, and still holds one back (#1576).
        run_tests(&[
            r#"
              begin
                Encoding::Converter.new("CESU-8", "UTF-8").convert("a\xED\xA0\x80z".b.force_encoding("CESU-8"))
              rescue => e
                [e.class.name, e.message]
              end
            "#,
            r#"
              Encoding::Converter.new("CESU-8", "UTF-8", invalid: :replace)
                .convert("a\xED\xA0\x80z".b.force_encoding("CESU-8")).bytes
            "#,
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "EUC-JP", undef: :replace)
              [c.convert("a\u{1F600}b".encode("UTF8-MAC")).bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-16", "EUC-JP", undef: :replace)
              [c.convert("a\u{1F600}b".encode("UTF-16")).bytes, c.finish.bytes]
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "CESU-8", undef: :replace)
              [c.convert("a\u{1F600}b").bytes, c.finish.bytes]
            "#,
        ]);
    }

    #[test]
    fn a_wrapper_conversion_is_observable_the_way_the_others_are() {
        // What `#finish` raises when the cluster it was holding turns
        // out to have no cell, and that a wrapper source reports its
        // error through `primitive_errinfo` / `putback` / `last_error`
        // as every other source does (#1576).
        run_tests(&[
            r#"
              c = Encoding::Converter.new("UTF8-MAC", "US-ASCII")
              a = c.convert("aあ".encode("UTF8-MAC")).bytes
              b = (begin; c.finish; rescue => e; [e.class.name, e.message]; end)
              [a, b]
            "#,
            r#"
              c = Encoding::Converter.new("CESU-8", "UTF-8")
              src = "a\xED\xA0\x80z".b.force_encoding("CESU-8")
              dst = +""
              c.primitive_convert(src.dup, dst)
              e = c.primitive_errinfo
              [dst.bytes, e[0], e[2], e[3], e[4], c.putback.bytes, c.last_error.class.name]
            "#,
            // A dummy source a chunk at a time through the other
            // entry point: the BOM arrives in the first and the
            // second is read in the byte order it named.
            r#"
              c = Encoding::Converter.new("UTF-16", "UTF-8")
              s = "aあ".encode("UTF-16")
              d1 = +""; d2 = +""
              c.primitive_convert(s.byteslice(0, 4).dup, d1, nil, nil, partial_input: true)
              c.primitive_convert(s.byteslice(4, 2).dup, d2, nil, nil, partial_input: true)
              [d1.bytes, d2.bytes]
            "#,
            // And that the pairs describe themselves like any other.
            r#"
              [["UTF-8","CESU-8"],["UTF-8","UTF-16"],["CESU-8","UTF-8"],["UTF8-MAC","UTF-8"]].map do |s, d|
                Encoding::Converter.new(s, d).convpath.map { |x| x.is_a?(Array) ? x.map(&:name) : x }
              end
            "#,
            r#"%w[CESU-8 UTF-16 UTF-32 UTF8-MAC].map { |e| Encoding::Converter.search_convpath("UTF-8", e).size }"#,
        ]);
    }

    #[test]
    fn the_carrier_sets_are_their_bases_with_a_vendors_emoji() {
        // Storage, iteration and validity are the base's throughout —
        // `UTF8-DoCoMo` is UTF-8's bytes, `SJIS-DoCoMo` is
        // Windows-31J's cells — and only a conversion tells them
        // apart (#1573).
        run_tests(&[
            r#"
              %w[UTF8-DoCoMo UTF8-KDDI UTF8-SoftBank SJIS-DoCoMo SJIS-KDDI SJIS-SoftBank].map do |n|
                e = Encoding.find(n)
                [e.name, e.dummy?, e.ascii_compatible?, e.names,
                 "abc".dup.force_encoding(n).valid_encoding?]
              end
            "#,
            r#"
              %w[UTF8-DoCoMo UTF8-KDDI UTF8-SoftBank SJIS-DoCoMo SJIS-KDDI SJIS-SoftBank].map do |n|
                ["☀".encode(n).bytes, "☀".encode(n).encode("UTF-8").bytes]
              end
            "#,
            // The cells Windows-31J holds and a carrier does not.
            r#"
              c = [0xF8, 0xA0].pack("C*")
              [c.dup.force_encoding("Windows-31J").encode("UTF-8").unpack("U*"),
               c.dup.force_encoding("SJIS-DoCoMo").encode("UTF-8").unpack("U*")]
            "#,
            r#"Encoding.name_list.sort == Encoding.name_list.sort.uniq"#,
        ]);
    }

    #[test]
    fn one_carriers_emoji_converts_to_another_without_a_unicode_meaning() {
        // A conversion with a carrier at each end is CRuby's own
        // transcoder, not a round trip through the pivot: DoCoMo's
        // emoji for `U+1F600` means nothing in Unicode — converting it
        // to UTF-8 raises — and still spells itself in KDDI's and
        // SoftBank's alphabets (#1573).
        run_tests(&[
            r#"
              d = "\u{1F600}".encode("UTF8-DoCoMo")
              [d.unpack("U*"),
               (begin; d.encode("UTF-8"); rescue => e; e.class.name; end),
               d.encode("UTF8-KDDI").unpack("U*"),
               d.encode("SJIS-SoftBank").bytes,
               d.encode("UTF-8", undef: :replace).bytes]
            "#,
            // And the same through a converter, for the pairs that
            // cross a base as well as a vendor.
            r#"
              [["SJIS-DoCoMo","UTF8-KDDI"],["SJIS-KDDI","SJIS-SoftBank"],["UTF8-SoftBank","SJIS-DoCoMo"]].map do |s, d|
                src = "a☀b".encode(s)
                [src.encode(d).bytes, Encoding::Converter.new(s, d).convert(src.dup).bytes]
              end
            "#,
            r#"
              c = Encoding::Converter.new("UTF-8", "UTF8-DoCoMo")
              [c.convert("a☀").bytes, c.convert("☁b").bytes, c.finish.bytes]
            "#,
            // `undef: :replace` stands in for an emoji the other
            // carrier has not got, as it does for any other character.
            r#"
              %w[UTF-8 Windows-31J EUC-JP US-ASCII].map do |d|
                "\u{1F600}".encode("UTF8-DoCoMo").encode(d, undef: :replace).bytes
              end
            "#,
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
    fn a_slice_without_a_high_byte_is_ascii_only() {
        // The parent being Valid does not make an all-ASCII slice of
        // it non-ASCII: `"abc日本xyz"[1]` is `"b"`, and `"b"` is
        // compatible with anything.
        crate::tests::run_test_once(
            r##"
            s = "abc日本xyz"; t = "\xff abc".b; l = "caf\xE9 abc".force_encoding("ISO-8859-1")
            u16 = "a".encode("UTF-16LE")
            [s[1].ascii_only?, s[1,2].ascii_only?, s[-2..].ascii_only?, s.scan(/./).map(&:ascii_only?),
             s.partition("b").map(&:ascii_only?), s.byteslice(1,2).ascii_only?, s[3].ascii_only?, s[3..].ascii_only?,
             t[1..].ascii_only?, t[0].ascii_only?, l[4..].ascii_only?, l[0,4].ascii_only?,
             (s[1] + "\xff".b).encoding.to_s, s[1].encoding.to_s, s.split("日").map(&:ascii_only?),
             s.chars.map(&:ascii_only?), "abc".force_encoding("UTF-7")[1].ascii_only?, u16[0].ascii_only?]
            "##,
        );
    }

    #[test]
    fn an_encoding_name_is_one_the_name_list_lists() {
        // Case does not count; separators and everything else do, so
        // "utf8" and "UTF_8" are unknown where "eucJP" and "utf-8" are
        // aliases — through `Encoding.find`, `force_encoding`,
        // `String.new`, `encode` and `Encoding::Converter` alike.
        crate::tests::run_test_once(
            r##"
            names = %w[utf8 UTF_8 utf-8 Utf-8 latin1 koi8r iso2022jp ASCII8BIT ascii-8bit shift-jis SHIFT_JIS
                       eucjp EUC_JP cp932 SJIS BINARY external ISO8859_1 ISO_8859_1 iso8859-1 646 UTF8-MAC
                       UTF-8-MAC UTF_8_MAC utf_16le UTF16LE euc-jp-ms EUCJP_MS macjapan sjis-docomo SJIS_DOCOMO
                       UTF.8 Big5-UAO gb12345]
            r = names.map { |n| (Encoding.find(n).name rescue [$!.class, $!.message]) }
            r << names.map { |n| ("x".dup.force_encoding(n).encoding.name rescue [$!.class, $!.message]) }
            r << (String.new(encoding: "utf8") rescue [$!.class, $!.message])
            r << ("x".encode("utf8") rescue [$!.class, $!.message])
            r << ("x".encode("utf-8", "utf8") rescue [$!.class, $!.message])
            r << ("x".encode("utf8", Encoding::EUC_JP) rescue [$!.class, $!.message])
            r << ("x".encode("") rescue [$!.class, $!.message])
            r << (Encoding::Converter.new("utf8", "UTF-8") rescue [$!.class, $!.message])
            r << (Encoding::Converter.new("UTF-8", "utf8") rescue [$!.class, $!.message])
            r << (Encoding::Converter.search_convpath("utf8", "UTF-8") rescue [$!.class, $!.message])
            r << (Encoding.default_internal = "utf8") rescue r << [$!.class, $!.message]
            r << (File.open("/dev/null", "r:utf8") { |f| f.external_encoding.name } rescue [$!.class, $!.message])
            r << (Encoding.find(" utf-8") rescue [$!.class, $!.message])
            r << (Encoding.find("") rescue [$!.class, $!.message])
            r << ("x".encode("a\0b") rescue [$!.class, $!.message])
            r << (Encoding::Converter.new("UTF-8", "a\0b") rescue [$!.class, $!.message])
            r << (String.new(encoding: "a\0b") rescue [$!.class, $!.message])
            r
            "##,
        );
    }

    #[test]
    fn an_encoding_is_a_frozen_singleton() {
        crate::tests::run_test_once(
            r##"
            e = Encoding::UTF_8
            r = [e.frozen?, e.name.frozen?, e.to_s.frozen?, e.name.encoding.to_s, e.names.frozen?,
                 e.names.map(&:frozen?).uniq, e.names.map { |n| n.encoding.to_s }.uniq,
                 Encoding.name_list.frozen?, Encoding.name_list.map(&:frozen?).uniq,
                 Encoding.name_list.map { |n| n.encoding.to_s }.uniq,
                 Encoding.aliases.frozen?, Encoding.aliases.keys.map(&:frozen?).uniq,
                 Encoding.aliases.values.map(&:frozen?).uniq, Encoding.list.map(&:frozen?).uniq]
            r << (Encoding.new rescue [$!.class, $!.message])
            r << (Encoding.allocate rescue [$!.class, $!.message])
            r << (e.dup rescue [$!.class, $!.message])
            r << (e.clone rescue [$!.class, $!.message])
            r << (e.clone(freeze: false) rescue [$!.class, $!.message])
            r << (e.instance_variable_set(:@x, 1) rescue [$!.class, $!.message])
            r << e.instance_variables << e.instance_variable_get(:@name)
            r << Encoding.respond_to?(:new) << Encoding.respond_to?(:allocate)
            r << [Encoding::UNICODE_VERSION, Encoding::UNICODE_VERSION.frozen?, Encoding::UNICODE_VERSION.encoding.to_s]
            r << Encoding.instance_methods(false).sort << Encoding.singleton_methods.sort
            # A singleton class does not stop the object being an Encoding.
            r << e.singleton_class.class << Encoding.find("utf-8").equal?(e) << "x".force_encoding(e).encoding.equal?(e)
            r << Encoding.compatible?(e, Encoding::US_ASCII).equal?(e)
            r
            "##,
        );
    }

    #[test]
    fn an_encoding_marshals_by_name() {
        crate::tests::run_test_once(
            r##"
            e = Encoding::UTF_8
            r = [e._dump(-1), e._dump(-1).encoding.to_s, e._dump(-1).frozen?, e._dump, Encoding._load("UTF-8"), Encoding._load(3)]
            r << (e._dump(1, 2) rescue [$!.class, $!.message])
            r << Marshal.dump(e).bytes
            r << Marshal.load(Marshal.dump(e)).equal?(e)
            r << Marshal.load(Marshal.dump([e, Encoding::BINARY, Encoding::ISO_2022_JP, Encoding::Windows_31J])).map(&:name)
            r << Marshal.load(Marshal.dump({e => "x"})).map { |k, v| [k.name, v] }
            r << (Marshal.load("\x04\x08Iu:\x0dEncoding\x09nope\x06:\x06EF") rescue [$!.class, $!.message])
            r << Marshal.load("\x04\x08u:\x0dEncoding\x0aUTF-8").name
            r << Marshal.load("\x04\x08Iu:\x0dEncoding\x0aeucjp\x06:\x06ET").name
            r
            "##,
        );
    }

    #[test]
    fn two_encodings_are_compatible_the_way_enc_compatible_latter_says() {
        // With no strings to look into: both ASCII-compatible (no
        // dummy is), then US-ASCII on either side yields the other.
        crate::tests::run_test_once(
            r##"
            [[Encoding::US_ASCII, Encoding::BINARY], [Encoding::US_ASCII, Encoding::UTF_8],
             [Encoding::UTF_16LE, Encoding::US_ASCII], [Encoding::UTF_8, Encoding::US_ASCII],
             [Encoding::EUC_JP, Encoding::Shift_JIS], [Encoding::UTF_7, Encoding::UTF_7],
             [Encoding::UTF_7, Encoding::US_ASCII], [Encoding::ISO_2022_JP, Encoding::US_ASCII],
             [Encoding::BINARY, Encoding::US_ASCII], [Encoding::UTF_8, Encoding::UTF_16LE]].map { |a, b| Encoding.compatible?(a, b)&.to_s }
            "##,
        );
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

    /// The converter's buffers across calls (#1424): the caller's
    /// source keeps its own tag when what is left of it is written
    /// back; the read-again bytes of an `:invalid_byte_sequence` are
    /// the head of the next call's input (unless `#putback` took
    /// them); a non-partial `:incomplete_input` has reported its bytes
    /// and holds nothing for the next `#convert`; and `#finish`
    /// records `:finished` rather than the error a previous call
    /// already consumed.
    /// [`jp_decode_override`] and [`jp_encode_override`] binary-search
    /// the fixup tables, so every one of them has to be sorted — the
    /// hand-written ones as much as the generated CP51932 tables.
    #[test]
    fn fixup_tables_are_sorted() {
        for fx in [
            &super::EUCJP_FIXUP,
            &super::SJIS_FIXUP,
            &super::WINDOWS31J_FIXUP,
            &*super::CP51932_FIXUP,
        ] {
            assert!(fx.decode.windows(2).all(|w| w[0].0 < w[1].0), "{} decode", fx.enc.name());
            assert!(fx.encode.windows(2).all(|w| w[0].0 < w[1].0), "{} encode", fx.enc.name());
        }
        let emoji = &super::super::encoding_kddi::KDDI_ISO2022_ENCODE;
        assert!(emoji.windows(2).all(|w| w[0].0 < w[1].0));
        let cells = &super::super::encoding_kddi::KDDI_ISO2022_DECODE;
        assert!(cells.windows(2).all(|w| w[0].0 < w[1].0));
    }

    #[test]
    fn converter_buffers_across_calls() {
        run_test_once(
            r##"
            t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
            res = []
            ec = Encoding::Converter.new(Encoding::UTF_8, Encoding::UTF_8_MAC)
            s = "\x80\x80\x80".b
            dest = "".b.force_encoding(Encoding::UTF_8_MAC)
            res << ec.primitive_convert(s, dest) << s.encoding.to_s << s.bytes << (s == "\x80\x80".b)
            res << ec.primitive_convert(s, dest) << s.bytes
            res << ec.primitive_convert(s, dest) << s.bytes << dest.bytes
            ec = Encoding::Converter.new("utf-8", "iso-8859-1")
            s = "\xf1abcd".b
            d = "".b
            res << ec.primitive_convert(s, d) << s.encoding.to_s << s << ec.primitive_errinfo
            res << ec.primitive_convert(s, d) << s << d
            ec = Encoding::Converter.new("utf-8", "iso-8859-1")
            res << t.() { ec.convert("\xf1abcd") } << ec.putback << t.() { ec.convert("") } << t.() { ec.finish }
            ec = Encoding::Converter.new("utf-8", "iso-8859-1")
            res << t.() { ec.convert("\xf1abcd") } << t.() { ec.finish }
            ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
            res << ec.primitive_convert(+"\xA1", +'') << ec.primitive_errinfo
            res << t.() { ec.convert("\xA1") } << ec.primitive_errinfo
            res << t.() { ec.finish } << ec.primitive_errinfo
            ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
            res << t.() { ec.convert("\xA1") } << t.() { ec.convert("\xA1") } << t.() { ec.finish } << ec.primitive_errinfo
            ec = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
            res << t.() { ec.primitive_convert(+"\xA1\xA1", +'') } << t.() { ec.finish } << ec.primitive_errinfo
            ec = Encoding::Converter.new("EUC-JP", "UTF-8")
            res << ec.primitive_convert(+"\xA1", +'', nil, nil, partial_input: true) << ec.primitive_convert(+"\xA1", +'') << t.() { ec.finish }
            res
            "##,
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
    fn windows31j_user_defined_area_round_trips() {
        // Rows `F0`..`F9` are Windows-31J's user-defined area, mapped
        // to the Unicode private-use area. `encoding_rs` decodes them
        // and refuses to encode them back, so all 1880 cells were a
        // one-way trip; the mapping is arithmetic, and this walks
        // every cell of it (#1462).
        run_test_once(
            r#"
              cells = []
              (0xF0..0xF9).each do |hi|
                ((0x40..0x7E).to_a + (0x80..0xFC).to_a).each { |lo| cells << [hi, lo] }
              end
              bad = cells.reject do |hi, lo|
                b = [hi, lo].pack("C*").dup.force_encoding("Windows-31J")
                b.encode("UTF-8").encode("Windows-31J").bytes == [hi, lo]
              end
              [
                [cells.size, bad.size,
                 cells.map { |hi, lo| [hi, lo].pack("C*").dup.force_encoding("Windows-31J").encode("UTF-8").ord }.minmax],
                # The edges of the area, and the character just past it.
                [0xE000, 0xE757, 0xE758, 0xDFFF].map { |cp|
                  ([cp].pack("U").encode("Windows-31J").bytes rescue $!.class.name.sub("Encoding::", "")) },
                # Plain Shift_JIS has nothing in those rows, either way.
                [0xE000, 0xE757].map { |cp|
                  ([cp].pack("U").encode("Shift_JIS").bytes rescue $!.class.name.sub("Encoding::", "")) },
                # A mixed string takes the per-character walk; an
                # ordinary one still takes the whole-buffer fast path.
                ["あ\u{E000}い\u{E757}う".encode("Windows-31J").bytes,
                 "あ\u{E000}い".encode("Windows-31J").encode("UTF-8") == "あ\u{E000}い",
                 "あいう".encode("Windows-31J").bytes],
                # Rows `FA`..`FC` are the NEC-selected IBM extensions,
                # not this area, and were already written.
                [0x2170, 0x9ED1].map { |cp| [cp].pack("U").encode("Windows-31J").bytes },
              ]
            "#,
        );
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
    fn code_pages_answer_to_both_their_names() {
        // CRuby names each DOS code page twice, `IBMnnn` and `CPnnn`,
        // and monoruby had only the constant spellings — the names
        // were missing from `Encoding.aliases`, `#names` and
        // `name_list`, and `Encoding.find("CP437")` did not resolve.
        // Code page 850 is the one the family names the other way
        // round: `CP850` is canonical there, `IBM850` the alias
        // (#1520).
        run_test_once(
            r#"
              names = %w[CP437 CP737 CP775 CP850 IBM850 CP857 CP860 CP861 CP862 CP863
                         CP864 CP865 CP866 CP869 Big5-HKSCS:2008 UTF-8-MAC UTF-8-HFS]
              [
                names.map { |n| [n, Encoding.find(n).name] },
                names.map { |n| "ab".dup.force_encoding(n).encoding.name },
                [Encoding::CP850.name, Encoding::IBM850.name,
                 Encoding::CP850.equal?(Encoding::IBM850),
                 Encoding::CP437.equal?(Encoding::IBM437)],
                [Encoding::IBM437.names, Encoding::CP850.names,
                 Encoding.find("Big5-HKSCS").names, Encoding.find("UTF8-MAC").names],
                # `eucJP-ms` has two names, not three.
                Encoding.find("eucJP-ms").names,
              ]
            "#,
        );
    }

    #[test]
    fn internal_names_an_encoding_only_once_it_is_set() {
        // `"internal"` follows `Encoding.default_internal`, which is
        // unset by default — so unlike `locale` / `external` /
        // `filesystem` it usually names no encoding at all, and is in
        // `name_list` without being in any encoding's `#names`
        // (#1520).
        run_test_once(
            r#"
              before = [Encoding.name_list.include?("internal"),
                        Encoding.find("internal"),
                        Encoding::UTF_8.names.include?("internal")]
              Encoding.default_internal = Encoding::EUC_JP
              after = [Encoding.find("internal").name,
                       Encoding.find("EUC-JP").names.include?("internal"),
                       Encoding::UTF_8.names.include?("internal")]
              Encoding.default_internal = nil
              [before, after, Encoding.find("internal")]
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
            |bytes: &[u8]| super::jp_live_throughout(super::jp_fixup(E::EUC_JP).unwrap(), bytes);
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

    #[test]
    fn converter_destination_buffer_full_leaves_the_rest_in_src() {
        // A capped destination consumes only what it converted. CRuby
        // reads one character further than it writes and buffers that
        // character's output, so `src` keeps everything from the
        // character after the one that did not fit — and the next
        // call still produces the whole conversion (#1511).
        crate::tests::run_test_once(
            r##"
            [0, 2, 5].map do |cap|
              ec = Encoding::Converter.new("UTF-8", "EUC-JP")
              s = "あabcd".dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              mid = [s.dup, d.bytes]
              second = ec.primitive_convert(s, d, nil, 100)
              [first, mid, second, s, d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_destination_that_fits_exactly_is_finished() {
        // The encoder stopping with room still free is not a full
        // destination: a cap that the conversion fits into exactly
        // finishes in one call (#1511).
        crate::tests::run_test_once(
            r##"
            [["UTF-8", "EUC-JP", "あいu", 5],
             ["UTF-8", "EUC-JP", "あ", 2],
             ["UTF-8", "UTF-16BE", "ab", 4]].map do |src, dst, text, cap|
              ec = Encoding::Converter.new(src, dst)
              s = text.encode(src).dup
              d = "".dup
              [ec.primitive_convert(s, d, nil, cap), s.bytes, d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_undefined_conversion_in_the_decode_half() {
        // A source byte with no Unicode meaning fails on the way *in*:
        // the stage pair is [source, "UTF-8"], the error bytes are raw
        // source bytes, and the message quotes them rather than naming
        // a codepoint (#1511).
        crate::tests::run_test_once(
            r##"
            [["ISO-8859-11", "UTF-8"], ["ISO-8859-11", "EUC-JP"],
             ["ISO-8859-11", "ISO-8859-1"]].map do |src, dst|
              ec = Encoding::Converter.new(src, dst)
              s = "a\xDBb".dup.force_encoding(src)
              d = "".dup
              r = ec.primitive_convert(s, d)
              [r, s.bytes, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x },
               ec.last_error.message]
            end
            "##,
        );
    }

    #[test]
    fn converter_applies_the_japanese_table_corrections() {
        // `String#encode` has gone through CRuby's own tables since
        // #1445; `Encoding::Converter` had the raw WHATWG codec, so
        // the same bytes converted two ways depending on the API
        // (#1461). The three shapes that differ: a duplicate-mapping
        // cell, an extension row CRuby has no character for, and a
        // cell its *walk* accepts that the codec cannot read.
        crate::tests::run_test_once(
            r##"
            [["EUC-JP", "\xA1\xBD"], ["EUC-JP", "\xF9\xA1"], ["EUC-JP", "\x8F\xA1\xA1"],
             ["Shift_JIS", "\x81\x5C"], ["Shift_JIS", "\x87\x40"], ["Shift_JIS", "\x81\xAD"],
             ["Windows-31J", "\x81\x5C"], ["Windows-31J", "\x87\x40"],
             ["Windows-31J", "\x81\xAD"]].map do |enc, bytes|
              s = bytes.dup.force_encoding(enc)
              one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
              cv = (Encoding::Converter.new(enc, "UTF-8").convert(bytes.dup).codepoints rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        // The same on the way out: the duplicate characters, EUC-JP's
        // JIS X 0212 plane, Windows-31J's user-defined area, and the
        // three ASCII-position characters WHATWG writes and CRuby has
        // no cell for.
        crate::tests::run_test_once(
            r##"
            [["EUC-JP", 0x2014], ["EUC-JP", 0x2015], ["EUC-JP", 0x9299],
             ["Shift_JIS", 0x2212], ["Shift_JIS", 0xFF0D],
             ["Windows-31J", 0xE000], ["Windows-31J", 0xE757],
             ["EUC-JP", 0xA5], ["Shift_JIS", 0x203E], ["Windows-31J", 0x80]].map do |enc, cp|
              s = [cp].pack("U")
              one = (s.encode(enc).bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("UTF-8", enc).convert(s.dup).bytes rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
    }

    #[test]
    fn converter_japanese_cells_across_calls() {
        // A cell split between two `primitive_convert`s has to stay
        // pending rather than be consumed as a malformed run (#1461).
        crate::tests::run_test_once(
            r##"
            ["\xA1\xBD\xA1\xC1", "\x8F\xA1\xA1"].flat_map do |bytes|
              (1...bytes.bytesize).map do |cut|
                ec = Encoding::Converter.new("EUC-JP", "UTF-8")
                d = "".dup
                s1 = bytes[0, cut].dup.force_encoding("EUC-JP")
                r1 = ec.primitive_convert(s1, d, nil, nil, partial_input: true)
                s2 = bytes[cut..].dup.force_encoding("EUC-JP")
                r2 = ec.primitive_convert(s2, d)
                [r1, r2, s1.bytes, s2.bytes, d.bytes]
              end
            end
            "##,
        );
        // An undefined cell reached after a well-formed one, with the
        // errinfo that names it against the *source* encoding — the
        // decode half is what gave up.
        crate::tests::run_test_once(
            r##"
            ["\x82\xA0\x81\xAD", "\x87\x40\x81\xAD", "A\x81\xAD", "\x81\xAD\x82\xA0"].map do |b|
              ec = Encoding::Converter.new("Windows-31J", "UTF-8")
              s = b.dup.force_encoding("Windows-31J")
              d = "".dup
              [ec.primitive_convert(s, d), s.bytes, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
        // A malformed run is consumed together with the bytes read to
        // disprove it (they are held for `#putback`); an incomplete
        // tail is not.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("EUC-JP", "US-ASCII")
            s = "ab\xFF\xFEcd".dup.force_encoding("EUC-JP")
            d = "".dup
            r = [ec.primitive_convert(s, d), s.bytes, d.bytes]
            ec2 = Encoding::Converter.new("EUC-JP", "ISO-8859-1")
            s2 = "abc\xa1def".dup
            d2 = "".dup
            r << ec2.primitive_convert(s2, d2, nil, 10) << [s2, d2] << ec2.putback
            "##,
        );
    }

    #[test]
    fn the_converter_reads_the_same_cells_the_one_shot_path_does() {
        // `encoding_rs`'s `euc-kr` is Windows-949 and its `gb2312` is
        // GBK, so the converter's decode half read cells those
        // encodings do not have — characters `String#encode` refused
        // (#1558).
        crate::tests::run_test_once(
            r##"
            [["EUC-KR", 0x81, 0x41], ["EUC-KR", 0xA1, 0x41], ["EUC-KR", 0xB0, 0xA1],
             ["GB2312", 0xA1, 0x40], ["GB2312", 0xB0, 0xA1],
             ["CP949", 0x81, 0x41], ["Shift_JIS", 0x82, 0xA0]].map do |enc, b1, b2|
              s = [b1, b2].pack("C*").force_encoding(enc)
              one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
              cv = (Encoding::Converter.new(enc, "UTF-8").convert(s.dup).codepoints rescue $!.class.to_s)
              [one, cv, one == cv, s.valid_encoding?]
            end
            "##,
        );
        // Through `primitive_convert`, where the cell decides how much
        // of `src` is taken and what the errinfo names.
        crate::tests::run_test_once(
            r##"
            [["EUC-KR", 0x81], ["EUC-KR", 0xA1], ["GB2312", 0xA1]].map do |enc, b|
              ec = Encoding::Converter.new(enc, "UTF-8")
              s = ([b, 0x41, 0x42].pack("C*")).force_encoding(enc)
              d = "".dup
              [ec.primitive_convert(s, d), s.bytes, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
        // `invalid: :replace` substitutes the cell rather than
        // decoding it, through both APIs.
        crate::tests::run_test_once(
            r##"
            ["EUC-KR", "GB2312"].map do |enc|
              s = ([0x81, 0x41, 0x42].pack("C*")).force_encoding(enc)
              [s.encode("UTF-8", invalid: :replace).bytes,
               Encoding::Converter.new(enc, "UTF-8", invalid: :replace).convert(s.dup).bytes]
            end
            "##,
        );
    }

    #[test]
    fn a_malformed_run_stops_where_the_encoding_says_it_does() {
        // CRuby reports the run as the longest prefix that is still a
        // *pending* sequence, with the byte that disproved it read
        // again rather than swallowed. A byte that can never begin a
        // sequence has no pending prefix, so the run is that byte
        // alone — monoruby took the next byte along with it (#1546).
        crate::tests::run_test_once(
            r##"
            [["Shift_JIS", 0x80], ["Windows-31J", 0x80], ["GB18030", 0x80],
             ["EUC-KR", 0x81], ["EUC-KR", 0xA0], ["EUC-JP", 0xA1],
             ["EUC-KR", 0xA1], ["Shift_JIS", 0x81]].flat_map do |enc, b|
              [[], [0x41], [0xA1], [0xFF]].map do |tail|
                s = ([b] + tail).pack("C*").force_encoding(enc)
                begin
                  s.encode("UTF-8")
                  "ok"
                rescue Encoding::InvalidByteSequenceError => e
                  [e.error_bytes.bytes, (e.readagain_bytes || "").bytes,
                   e.incomplete_input?, e.message]
                rescue => e
                  e.class.to_s
                end
              end
            end
            "##,
        );
        // The same through the converter, where the run decides how
        // much of `src` is consumed. (EUC-KR is left out: its
        // converter still *decodes* cells the encoding does not have,
        // which is a separate leak on the other side of the same
        // codec.)
        crate::tests::run_test_once(
            r##"
            [["Shift_JIS", 0x80], ["Windows-31J", 0x80], ["EUC-JP", 0xA1]].map do |enc, b|
              ec = Encoding::Converter.new(enc, "UTF-8")
              s = ([b, 0x41, 0x42].pack("C*")).force_encoding(enc)
              d = "".dup
              [ec.primitive_convert(s, d), s.bytes, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
        // `invalid: :replace` substitutes the run and re-reads the
        // byte that disproved it, so a swallowed byte would go
        // missing from the output.
        crate::tests::run_test_once(
            r##"
            [["Shift_JIS", 0x80], ["EUC-KR", 0x81], ["EUC-KR", 0xA1],
             ["EUC-JP", 0xA1], ["GB18030", 0x80]].map do |enc, b|
              ([b, 0x41, 0x42].pack("C*")).force_encoding(enc)
                .encode("UTF-8", invalid: :replace).bytes
            end
            "##,
        );
        // Big5 and CP949 keep the codec's answer: CRuby's transcoder
        // there reads leads its own encoding object does not, so the
        // walk is not the authority. These are the runs it gets right
        // and the walk would not — the cells where the two disagree
        // outright are #1500's, not this.
        crate::tests::run_test_once(
            r##"
            [["Big5", 0x8A], ["Big5-HKSCS", 0x86], ["CP949", 0x80]].map do |enc, b|
              s = ([b, 0x8F, 0xA1].pack("C*")).force_encoding(enc)
              [s.valid_encoding?,
               begin
                 s.encode("UTF-8"); "ok"
               rescue Encoding::InvalidByteSequenceError => e
                 [e.error_bytes.bytes, (e.readagain_bytes || "").bytes]
               rescue => e
                 e.class.to_s
               end]
            end
            "##,
        );
    }

    #[test]
    fn a_quoted_byte_run_is_spelled_the_way_inspect_spells_it() {
        // Every conversion error quotes the offending bytes, and CRuby
        // renders that run the way `String#inspect` renders a byte:
        // printable ASCII literally, eight control characters as their
        // mnemonics, everything else as `\xNN`. There were two
        // renderers here and neither matched — one escaped `"`, `\`
        // and the control characters, the other escaped *everything*
        // (#1607).
        crate::tests::run_test_once(
            r##"
            # The readagain byte is one byte, so this isolates the
            # per-byte rule over the whole range.
            (0x00..0xFF).map do |b|
              s = [0xA1, b].pack("C*").force_encoding("EUC-JP")
              m = begin; s.encode("UTF-8"); nil; rescue; $!.message; end
              m && m[/followed by "((?:[^"\\]|\\.)*)"/, 1]
            end.compact
            "##,
        );
        // The other builder is the one-shot `UndefinedConversionError`
        // for a well-formed cell with no character, where the trail
        // byte reaches into the printable range.
        crate::tests::run_test_once(
            r##"
            [[0xC9, 0x41], [0xC9, 0x22], [0xC9, 0x5C], [0xC9, 0x7E], [0xC9, 0x20],
             [0xFE, 0x41], [0xA2, 0xE8]].map do |l, t|
              s = [l, t].pack("C*").force_encoding("CP949")
              begin
                s.encode("UTF-8"); "ok"
              rescue
                [$!.class.to_s, $!.message]
              end
            end
            "##,
        );
        // A multi-byte run keeps the same rule, and an incomplete one
        // is quoted whole.
        crate::tests::run_test_once(
            r##"
            r = []
            [[0xE3, 0x81], [0xE3, 0x81, 0x41], [0xF0, 0x9F, 0x98], [0xC2]].each do |bytes|
              s = bytes.pack("C*").force_encoding("UTF-8")
              r << begin; s.encode("EUC-JP"); "ok"; rescue; $!.message; end
            end
            [[0xA1, 0x07], [0xA1, 0x1B], [0xA1, 0x7F], [0xA1, 0x00], [0xA1, 0x20]].each do |bytes|
              s = bytes.pack("C*").force_encoding("EUC-JP")
              r << begin
                Encoding::Converter.new("EUC-JP", "UTF-8").convert(s.dup); "ok"
              rescue
                $!.message
              end
            end
            r
            "##,
        );
    }

    #[test]
    fn a_cp949_cell_with_no_character_is_an_undefined_conversion_in_both_apis() {
        // `encoding_rs`'s `euc-kr` reads cells CRuby's CP949
        // transcoder has no character for. The one-shot path takes
        // them one at a time and calls that an undefined conversion;
        // the converter kept the codec's whole-buffer answer, where
        // the unreadable cell is U+FFFD and so a malformed sequence.
        // 5,380 cells of the grid disagreed that way (#1565).
        //
        // The messages are compared by class only: `String#encode`
        // still spells the cell's printable trail byte as `\xNN`
        // where CRuby prints it (#1607).
        crate::tests::run_test_once(
            r##"
            [[0xA2, 0xE8], [0xC9, 0x41], [0xFE, 0x41], [0xA5, 0x41], [0xAF, 0xFE]].map do |l, t|
              s = [l, t].pack("C*").force_encoding("CP949")
              one = begin; s.dup.encode("UTF-8"); "ok"; rescue; $!.class.to_s; end
              cv = begin
                Encoding::Converter.new("CP949", "UTF-8").convert(s.dup); "ok"
              rescue
                $!.class.to_s
              end
              ["%02X%02X" % [l, t], one, cv, one == cv]
            end
            "##,
        );
        // The cells that do have a character still read, and the two
        // bytes CP949 treats specially keep their own answers: `0x80`
        // is `valid_encoding?`-valid and still a malformed sequence to
        // convert, and a lead with no trail is an incomplete run.
        crate::tests::run_test_once(
            r##"
            r = []
            r << [0xB0, 0xA1].pack("C*").force_encoding("CP949").encode("UTF-8").codepoints
            r << Encoding::Converter.new("CP949", "UTF-8")
                   .convert([0xB0, 0xA1].pack("C*").force_encoding("CP949")).codepoints
            [[0x80], [0x80, 0x41], [0xA2], [0xA2, 0xE8, 0x41]].each do |bytes|
              s = bytes.pack("C*").force_encoding("CP949")
              one = begin; s.dup.encode("UTF-8"); "ok"; rescue; $!.class.to_s; end
              cv = begin
                Encoding::Converter.new("CP949", "UTF-8").convert(s.dup); "ok"
              rescue
                $!.class.to_s
              end
              r << [bytes.map { |b| "%02X" % b }.join, s.valid_encoding?, one, cv]
            end
            r
            "##,
        );
        // What converted before the cell still comes out, the cell is
        // the whole run, and `#primitive_errinfo` names it.
        crate::tests::run_test_once(
            r##"
            ["UTF-8", "EUC-KR", "UTF-16BE"].map do |d|
              ec = Encoding::Converter.new("CP949", d)
              dst = +""
              s = ([0x41, 0xA2, 0xE8, 0x42].pack("C*")).force_encoding("CP949")
              res = ec.primitive_convert(s, dst)
              [d, res, dst.bytes, s.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
        // `undef: :replace` covers it and `invalid: :replace` does
        // not — which is the whole point of the distinction, and was
        // the other way round through the converter.
        crate::tests::run_test_once(
            r##"
            s = ([0x41, 0xA2, 0xE8, 0x42].pack("C*")).force_encoding("CP949")
            [begin; s.dup.encode("UTF-8", undef: :replace).bytes; rescue; $!.class.to_s; end,
             begin; s.dup.encode("UTF-8", invalid: :replace).bytes; rescue; $!.class.to_s; end,
             begin
               Encoding::Converter.new("CP949", "UTF-8", undef: :replace).convert(s.dup).bytes
             rescue
               $!.class.to_s
             end,
             begin
               Encoding::Converter.new("CP949", "UTF-8", invalid: :replace).convert(s.dup).bytes
             rescue
               $!.class.to_s
             end]
            "##,
        );
    }

    #[test]
    fn a_replacement_the_destination_cannot_spell_is_refused() {
        // CRuby converts the `replace:` string into the destination
        // when it opens the converter, and a failure there is a
        // converter that does not exist rather than a character with
        // no cell. Dropping it instead turned a substitution into
        // silence — the character went missing and so did the thing
        // meant to stand for it (#1566).
        crate::tests::run_test_once(
            r##"
            bad = [0x1D11E].pack("U")
            r = []
            r << ("\u{1F600}".encode("Big5", undef: :replace, replace: bad) rescue [$!.class.to_s, $!.message])
            r << ("\u{4E00}".encode("Big5", undef: :replace, replace: bad) rescue [$!.class.to_s, $!.message])
            r << ("\u{4E00}".encode("Big5", invalid: :replace, replace: bad) rescue [$!.class.to_s, $!.message])
            r << (Encoding::Converter.new("UTF-8", "Big5", undef: :replace, replace: bad) rescue [$!.class.to_s, $!.message])
            r << (Encoding::Converter.new("UTF-8", "Big5", replace: bad) rescue [$!.class.to_s, $!.message])
            r
            "##,
        );
        // Where CRuby opens no converter it never looks at the
        // replacement: the same string and the same options go
        // through when the source needs no conversion.
        crate::tests::run_test_once(
            r##"
            bad = [0x1D11E].pack("U")
            r = []
            r << ("abc".encode("Big5", undef: :replace, replace: bad) rescue $!.class.to_s)
            r << ("abc".encode("Big5", invalid: :replace, replace: bad) rescue $!.class.to_s)
            r << ("".encode("Big5", undef: :replace, replace: bad) rescue $!.class.to_s)
            r << ("\u{1F600}".encode("UTF-8", undef: :replace, replace: bad) rescue $!.class.to_s)
            r << ("abc".dup.force_encoding("Big5").encode("Big5", undef: :replace,
                    replace: bad, universal_newline: true) rescue $!.class.to_s)
            r
            "##,
        );
        // A decorator is work of its own, so it opens a converter for
        // text that would otherwise pass straight through — and the
        // error names the decorators after the pair.
        crate::tests::run_test_once(
            r##"
            bad = [0x1D11E].pack("U")
            [[{ undef: :replace, universal_newline: true }, "a\nb"],
             [{ undef: :replace, crlf_newline: true }, "a\nb"],
             [{ undef: :replace, cr_newline: true }, "a\nb"],
             [{ xml: :text }, "abc"],
             [{ xml: :attr }, "abc"]].map do |o, s|
              (s.encode("Big5", **o, replace: bad) rescue [$!.class.to_s, $!.message])
            end
            "##,
        );
        // The same question asked after the fact, where CRuby names
        // neither the character nor the pair. The destinations whose
        // repertoire is narrower than their codec's are included, so
        // the check is the pipeline's and not `encoding_rs`'s (#1544).
        crate::tests::run_test_once(
            r##"
            [["Big5", 0x1D11E], ["US-ASCII", 0x4E00], ["GB2312", 0x20AC],
             ["Big5", 0x4E00], ["GB2312", 0x4E00]].map do |enc, cp|
              c = Encoding::Converter.new("UTF-8", enc)
              (c.replacement = [cp].pack("U")) rescue [$!.class.to_s, $!.message]
            end
            "##,
        );
    }

    #[test]
    fn the_replacement_is_kept_as_the_destination_spells_it() {
        // It was kept as UTF-8 text with the destination's name
        // stuck on it, so what `#replacement` handed back was a
        // string its own tag called impossible (#1583).
        crate::tests::run_test_once(
            r##"
            ["Big5", "EUC-JP", "ISO-8859-1", "UTF-16BE", "US-ASCII"].map do |d|
              c = Encoding::Converter.new("UTF-8", d)
              begin
                c.replacement = [0x4E00].pack("U")
                [d, c.replacement.bytes, c.replacement.encoding.name,
                 c.replacement.valid_encoding?]
              rescue
                [d, $!.class.to_s]
              end
            end
            "##,
        );
        // And a replacement written in some other encoding was
        // dropped on the way in — `is_str` answers only for valid
        // UTF-8 — so the default took its place, or the setter
        // refused a String outright.
        crate::tests::run_test_once(
            r##"
            r = []
            euc = [0x4E00].pack("U").encode("EUC-JP")
            big5 = [0x4E00].pack("U").encode("Big5")
            r << ([0x1F600].pack("U").encode("Big5", undef: :replace, replace: euc).bytes)
            r << (Encoding::Converter.new("UTF-8", "Big5", undef: :replace, replace: euc)
                    .convert([0x1F600].pack("U")).bytes)
            r << (begin
                    c = Encoding::Converter.new("UTF-8", "Big5"); c.replacement = euc
                    c.replacement.bytes
                  rescue; [$!.class.to_s] end)
            r << (begin
                    c = Encoding::Converter.new("UTF-8", "Big5"); c.replacement = big5
                    c.replacement.bytes
                  rescue; [$!.class.to_s] end)
            # one the destination cannot spell, whatever it is written in
            r << ([0x1F600].pack("U").encode("Big5", undef: :replace,
                    replace: [0x3042].pack("U").encode("EUC-JP")) rescue [$!.class.to_s, $!.message])
            r
            "##,
        );
        // The default is untouched, and the wide destinations keep
        // theirs in UTF-8 — CRuby inserts substituted output in the
        // input of the conversion's last step, not the destination.
        crate::tests::run_test_once(
            r##"
            ["Big5", "UTF-16BE", "UTF-16LE", "UTF-32BE", "ISO-8859-1"].map do |d|
              c = Encoding::Converter.new("UTF-8", d)
              [d, c.replacement.bytes, c.replacement.encoding.name]
            end
            "##,
        );
    }

    #[test]
    fn a_scrubbed_string_takes_a_replacement_its_own_encoding_admits() {
        // A same-encoding `invalid: :replace` is `String#scrub`, and
        // scrub's replacement has to suit the *receiver* — the same
        // encoding, or nothing above 0x7F — which is `rb_enc_check`'s
        // rule and not the converter's (#1599). What goes into the
        // string is the replacement's own bytes.
        crate::tests::run_test_once(
            r##"
            b5 = [0x81].pack("C").force_encoding("Big5")
            r = []
            [[0x1D11E, "UTF-8"], [0x4E00, "UTF-8"], [0x4E00, "Big5"],
             [0x3F, "UTF-8"], [0x3F, "ASCII-8BIT"], [0x3F, "EUC-JP"]].each do |cp, e|
              repl = [cp].pack("U").encode(e)
              enc = (b5.encode("Big5", invalid: :replace, replace: repl).bytes rescue [$!.class.to_s, $!.message])
              scr = (b5.scrub(repl).bytes rescue [$!.class.to_s, $!.message])
              r << [e, cp, enc, scr, enc == scr]
            end
            r << (b5.scrub("\xFF".b).bytes rescue [$!.class.to_s, $!.message])
            r << (b5.scrub.bytes)
            r
            "##,
        );
        // Only a receiver with something to scrub consults it, so one
        // with nothing to scrub takes a replacement it would
        // otherwise refuse — and BINARY, where no byte is ever
        // ill-formed, never asks at all.
        crate::tests::run_test_once(
            r##"
            bad = [0x1D11E].pack("U")
            r = []
            ok = "ok".dup.force_encoding("Big5")
            r << (ok.scrub(bad).bytes rescue [$!.class.to_s])
            r << (ok.encode("Big5", invalid: :replace, replace: bad).bytes rescue [$!.class.to_s])
            r << ("\x80".b.scrub([0x3042].pack("U")).bytes rescue [$!.class.to_s])
            r << ("\x80".b.scrub("?").bytes rescue [$!.class.to_s])
            r
            "##,
        );
        // Every encoding monoruby scrubs, through all three of the
        // methods that do it.
        crate::tests::run_test_once(
            r##"
            bad = { "UTF-8" => [0x80], "Big5" => [0x81], "EUC-JP" => [0xA1, 0xFF],
                    "Shift_JIS" => [0x80], "EUC-KR" => [0xFF], "US-ASCII" => [0x80] }
            bad.map do |e, bytes|
              s = ("ok" + bytes.pack("C*") + "z").dup.force_encoding(e)
              repl = [0x4E00].pack("U")
              a = (s.scrub(repl).bytes rescue [$!.class.to_s, $!.message])
              b = (s.encode(e, invalid: :replace, replace: repl).bytes rescue [$!.class.to_s, $!.message])
              c = (begin; t = s.dup; t.scrub!(repl); t.bytes; rescue; [$!.class.to_s, $!.message] end)
              [e, a, a == b, a == c, s.scrub.bytes]
            end
            "##,
        );
        // Not a String at all is the same question asked earlier: a
        // valid receiver takes it without a glance, an ill-formed one
        // refuses it the way any String argument is refused.
        crate::tests::run_test_once(
            r##"
            good = "abc"
            bad  = "a\x80b".dup.force_encoding("UTF-8")
            [123, :sym, nil, [], 1.5].map do |a|
              [a.class.to_s,
               (good.dup.scrub(a).bytes rescue [$!.class.to_s, $!.message]),
               (bad.dup.scrub(a).bytes rescue [$!.class.to_s, $!.message]),
               (begin; t = bad.dup; t.scrub!(a); t.bytes; rescue; [$!.class.to_s, $!.message] end)]
            end
            "##,
        );
    }

    #[test]
    fn a_byte_no_source_encoding_has_a_character_for_is_reported() {
        // A byte of 0x80 or above is not US-ASCII at all, so it is a
        // malformed sequence. `String#encode` said so; the converter
        // handed the byte through instead, tagging the result with a
        // destination whose own rules it breaks (#1596).
        crate::tests::run_test_once(
            r##"
            s = [0x80].pack("C").force_encoding("US-ASCII")
            ["UTF-8", "Big5", "ASCII-8BIT", "UTF-16BE"].map do |d|
              one = (s.encode(d).bytes rescue [$!.class.to_s, $!.message])
              cv = (Encoding::Converter.new("US-ASCII", d).convert(s.dup).bytes rescue [$!.class.to_s, $!.message])
              [d, one, cv, one == cv]
            end
            "##,
        );
        // What converted before it still comes out, the byte itself
        // is the whole run, and nothing is read again after it.
        crate::tests::run_test_once(
            r##"
            r = []
            ec = Encoding::Converter.new("US-ASCII", "UTF-8")
            d = +""
            s = "a\x80bc".dup.force_encoding("US-ASCII")
            r << [ec.primitive_convert(s, d), d.bytes,
                  ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            r << ("a\x80b".dup.force_encoding("US-ASCII").encode("UTF-8", invalid: :replace).codepoints)
            r << ([0x80].pack("C").force_encoding("US-ASCII").encode("US-ASCII").bytes)
            r
            "##,
        );
        // The run is *consumed*: only what follows it stays in `src`,
        // since the bytes come back out of `#primitive_errinfo`. And
        // US-ASCII is not itself the pivot — CRuby puts a US-ASCII →
        // UTF-8 step in front of every other hop, so UTF-8 is the
        // destination it reports whatever the real one is.
        crate::tests::run_test_once(
            r##"
            ["UTF-8", "UTF-16BE", "Big5", "EUC-JP", "ISO-8859-1", "ASCII-8BIT"].map do |d|
              ec = Encoding::Converter.new("US-ASCII", d)
              dst = +""
              s = "abc\x80z".dup.force_encoding("US-ASCII")
              res = ec.primitive_convert(s, dst)
              [d, res, dst.bytes, s.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
        // A destination cap that fills before the offending byte is
        // reached is the ordinary `:destination_buffer_full`, and the
        // byte is still there for the next call to trip over.
        crate::tests::run_test_once(
            r##"
            r = []
            ["UTF-8", "UTF-16BE"].each do |d|
              (1..4).each do |cap|
                ec = Encoding::Converter.new("US-ASCII", d)
                dst = +""
                s = "abc\x80z".dup.force_encoding("US-ASCII")
                res = ec.primitive_convert(s, dst, nil, cap)
                r << [d, cap, res, dst.bytes, s.bytes,
                      ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
              end
            end
            r
            "##,
        );
        // BINARY is the other source with no character for a high
        // byte, and there it is an undefined conversion rather than a
        // malformed one. `undef: :replace` went unhonoured, and the
        // error named U+0000 where CRuby quotes the byte and spells
        // the pivot it failed at.
        crate::tests::run_test_once(
            r##"
            b = "a\x80b".b
            r = []
            ["UTF-8", "Big5", "EUC-JP", "UTF-16BE"].each do |d|
              one = (b.dup.encode(d).bytes rescue [$!.class.to_s, $!.message])
              cv = (Encoding::Converter.new("ASCII-8BIT", d).convert(b.dup).bytes rescue [$!.class.to_s, $!.message])
              r << [d, one, cv, one == cv]
              r << [d, b.dup.encode(d, undef: :replace).bytes,
                    Encoding::Converter.new("ASCII-8BIT", d, undef: :replace).convert(b.dup).bytes]
            end
            ec = Encoding::Converter.new("ASCII-8BIT", "Big5")
            dd = +""
            r << [ec.primitive_convert(b.dup, dd), dd.bytes,
                  ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            r << ("\x80".b.encode("ASCII-8BIT").bytes)
            r
            "##,
        );
    }

    #[test]
    fn the_three_thai_encodings_differ_only_in_the_c1_row() {
        // TIS-620 borrowed `encoding_rs`'s `windows-874`, which is
        // Microsoft's extension of it: ten of the cells TIS-620
        // assigns nothing to are filled in there, and CRuby disagreed
        // with monoruby in all 33 of `0x80..=0xA0` (#1580).
        crate::tests::run_test_once(
            r##"
            ["TIS-620", "ISO-8859-11", "Windows-874"].map do |e|
              row = [0x80, 0x85, 0x91, 0x9F, 0xA0, 0xA1, 0xDA, 0xDB, 0xFB, 0xFF].map do |b|
                s = [b].pack("C").force_encoding(e)
                one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
                cv = (Encoding::Converter.new(e, "UTF-8").convert(s.dup).codepoints rescue $!.class.to_s)
                [one, one == cv]
              end
              [e, row]
            end
            "##,
        );
        // And the way out, including the characters Microsoft's
        // extension has and the standard does not.
        crate::tests::run_test_once(
            r##"
            ["TIS-620", "ISO-8859-11", "Windows-874"].map do |e|
              n = (0x20..0x3100).count { |cp| ([cp].pack("U").encode(e) rescue nil) }
              row = [0x20AC, 0x2018, 0x00A0, 0x0E01, 0x0E5B, 0x0085].map do |cp|
                ([cp].pack("U").encode(e).bytes rescue $!.class.to_s)
              end
              [e, n, row]
            end
            "##,
        );
    }

    #[test]
    fn the_encodings_that_had_no_converter_have_one() {
        // #1563 gave these `Encoding` objects; CRuby transcodes them
        // and monoruby answered `ConverterNotFoundError` in both
        // directions (#1567). The three DOS code pages and
        // Windows-874 have no `encoding_rs` codec — its `windows-874`
        // is WHATWG's, which differs from CRuby's in 23 cells — so
        // they read from in-tree tables.
        crate::tests::run_test_once(
            r##"
            [["IBM720", 0x82], ["IBM720", 0x80], ["Windows-874", 0xA1],
             ["Windows-874", 0x80], ["Windows-874", 0x81], ["Windows-874", 0xDB],
             ["CP852", 0x80], ["IBM852", 0xB5], ["CP855", 0x80], ["IBM855", 0xFD],
             ["CP874", 0x91], ["CP720", 0x91]].map do |enc, b|
              s = [b].pack("C").force_encoding(enc)
              one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
              cv = (Encoding::Converter.new(enc, "UTF-8").convert(s.dup).codepoints rescue $!.class.to_s)
              [enc, one, cv, one == cv]
            end
            "##,
        );
        // The way out, and the characters these code pages have no
        // cell for.
        crate::tests::run_test_once(
            r##"
            [["IBM720", 0x644], ["IBM720", 0x4E00], ["Windows-874", 0xE01],
             ["Windows-874", 0x20AC], ["Windows-874", 0x4E00], ["CP852", 0x104],
             ["CP855", 0x426], ["IBM852", 0x4E00]].map do |enc, cp|
              s = [cp].pack("U")
              one = (s.encode(enc).bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("UTF-8", enc).convert(s.dup).bytes rescue $!.class.to_s)
              [enc, one, cv, one == cv]
            end
            "##,
        );
        // CP950 and CP951 are Microsoft's Big5, not CRuby's: they
        // fill thousands of cells Big5 leaves empty — with private
        // use characters — and their encoders carry best-fit
        // mappings, which are the single-byte cells here.
        crate::tests::run_test_once(
            r##"
            r = []
            r << [[0xA4, 0x40], [0xC6, 0xA1], [0xF9, 0xD6]].map do |b1, b2|
              [b1, b2].pack("C*").force_encoding("CP950").encode("UTF-8").codepoints
            end
            r << [0x4E00, 0xA1, 0xFF0C, 0x2550].map do |cp|
              [cp].pack("U").encode("CP950").bytes
            end
            r << [0x4E00, 0xA1, 0x2550].map do |cp|
              [cp].pack("U").encode("CP951").bytes
            end
            # what each of the three reaches, counted
            r << ["Big5", "CP950", "CP951"].map do |e|
              n = (0x80..0x9FFF).count { |cp| ([cp].pack("U").encode(e) rescue nil) }
              [e, n]
            end
            r
            "##,
        );
    }

    #[test]
    fn the_cjk_encodings_use_crubys_tables() {
        // `encoding_rs` carries WHATWG's tables: its `gb2312` is GBK,
        // and its `big5` reaches rows CRuby has nothing in. So cells
        // the encoding does not have read as characters, and
        // characters it cannot spell were written (#1544).
        crate::tests::run_test_once(
            r##"
            [["GB2312", 0xA2, 0xA1], ["GB2312", 0xA4, 0xA1], ["GB2312", 0xD7, 0xFA],
             ["GB2312", 0xF8, 0xA1], ["GBK", 0xA1, 0xA1], ["GBK", 0xA2, 0xA1],
             ["GBK", 0xD7, 0xFA], ["GBK", 0xFE, 0x50], ["Big5", 0xA3, 0xC0],
             ["Big5", 0xC6, 0xA1], ["Big5", 0xC7, 0x40], ["Big5", 0xFA, 0x40],
             ["Big5", 0xF9, 0xD6]].map do |enc, b1, b2|
              s = [b1, b2].pack("C*").force_encoding(enc)
              one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
              cv = (Encoding::Converter.new(enc, "UTF-8").convert(s.dup).codepoints rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        // The way out, including the characters whose cell CRuby has
        // and the codec spells differently.
        crate::tests::run_test_once(
            r##"
            [["GB2312", 0x4E00], ["GB2312", 0x554A], ["GB2312", 0x20AC], ["GB2312", 0x0251],
             ["GBK", 0x4E00], ["GBK", 0x20AC], ["GBK", 0x0251], ["GBK", 0xE7C7],
             ["Big5", 0x4E00], ["Big5", 0xF6B1], ["Big5", 0x00A8], ["Big5", 0x2554],
             ["GB18030", 0xFE17], ["GB18030", 0x4E00], ["GB18030", 0x1D11E]].map do |enc, cp|
              s = [cp].pack("U")
              one = (s.encode(enc).bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("UTF-8", enc).convert(s.dup).bytes rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        // A character the codec itself cannot spell, which is a
        // different refusal from a cell CRuby's table lacks: both are
        // an undefined conversion, and `undef: :replace` substitutes
        // for either.
        crate::tests::run_test_once(
            r##"
            r = []
            [["GB2312", 0x1D11E], ["GBK", 0x1D11E], ["Big5", 0x1F600],
             ["GB18030", 0x1D11E]].each do |enc, cp|
              s = [cp].pack("U")
              one = (s.encode(enc).bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("UTF-8", enc).convert(s.dup).bytes rescue $!.class.to_s)
              r << [enc, one, cv, one == cv]
            end
            r << ("A\u{1F600}B".encode("Big5", undef: :replace).bytes)
            r << ("A\u{1D11E}B".encode("GB2312", undef: :replace).bytes)
            r << (Encoding::Converter.new("UTF-8", "GB2312", undef: :replace)
                    .convert("A\u{1D11E}B".dup).bytes)
            r
            "##,
        );
        // GB18030 restricts no cell — it has one for everything — but
        // it still reads a handful differently: CRuby keeps them in the
        // private use area where `encoding_rs` gives the character.
        crate::tests::run_test_once(
            r##"
            [[0xA3, 0xA0], [0xA6, 0xD9], [0xA6, 0xEC], [0xA8, 0xBC], [0xFE, 0x59],
             [0xA1, 0xA1], [0xD2, 0xBB]].map do |b1, b2|
              s = [b1, b2].pack("C*").force_encoding("GB18030")
              one = (s.encode("UTF-8").codepoints rescue $!.class.to_s)
              cv = (Encoding::Converter.new("GB18030", "UTF-8").convert(s.dup).codepoints rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        // What the tables reach, counted: the codec's repertoire is
        // wider than the encoding's in every one of these.
        crate::tests::run_test_once(
            r##"
            ["GB2312", "GBK", "Big5", "EUC-KR", "CP949"].map do |e|
              n = (0x80..0x9FFF).count { |cp| ([cp].pack("U").encode(e) rescue nil) }
              m = (0xA1..0xFE).sum do |b1|
                (0xA1..0xFE).count do |b2|
                  ([b1, b2].pack("C*").force_encoding(e).encode("UTF-8") rescue nil)
                end
              end
              [e, n, m]
            end
            "##,
        );
        // A cell the table does not have is an undefined conversion,
        // and `undef: :replace` substitutes for a character it cannot
        // spell — through both APIs, capped and not.
        crate::tests::run_test_once(
            r##"
            r = []
            r << ([0xA2, 0xA1].pack("C*").force_encoding("GB2312")
                    .encode("UTF-8", undef: :replace).bytes)
            r << ("一ɑ啊".encode("GB2312", undef: :replace).bytes)
            r << (Encoding::Converter.new("UTF-8", "GB2312", undef: :replace)
                    .convert("一ɑ啊".dup).bytes)
            ec = Encoding::Converter.new("UTF-8", "Big5")
            s = "一¨丁".dup
            d = "".dup
            r << [ec.primitive_convert(s, d), s.bytes, d.bytes]
            e2 = Encoding::Converter.new("UTF-8", "GBK")
            s2 = "一丁".dup
            d2 = "".dup
            r << [e2.primitive_convert(s2, d2, nil, 3), d2.bytes, s2.bytes]
            "##,
        );
    }

    #[test]
    fn an_encoder_never_writes_what_the_encoding_calls_invalid() {
        // `encoding_rs`'s `euc-kr` is Windows-949 and its `gb2312` is
        // GBK, so the codec wrote cells the destination itself calls
        // invalid — bytes monoruby would then refuse to read back.
        // CRuby never writes what it cannot read, in any encoding
        // (#1544).
        crate::tests::run_test_once(
            r##"
            ["EUC-KR", "GB2312", "CP949", "GBK", "Big5", "EUC-JP", "Shift_JIS"].map do |e|
              bad = 0
              (0x20..0x2FFF).each do |cp|
                s = [cp].pack("U")
                b = (s.encode(e) rescue next)
                bad += 1 unless b.dup.force_encoding(e).valid_encoding?
              end
              [e, bad]
            end
            "##,
        );
        // The round trip that used to break: written, then refused.
        crate::tests::run_test_once(
            r##"
            [["EUC-KR", 0xC12A], ["EUC-KR", 0xAC02], ["GB2312", 0x02CA],
             ["CP949", 0xC12A], ["CP949", 0xAC02]].map do |enc, cp|
              s = [cp].pack("U")
              out = (s.encode(enc).bytes rescue $!.class.to_s)
              back = if out.is_a?(Array)
                (out.pack("C*").force_encoding(enc).encode("UTF-8").codepoints rescue $!.class.to_s)
              end
              [out, back]
            end
            "##,
        );
        // `#encode` and `Encoding::Converter` agree on all of it, and
        // `undef: :replace` substitutes rather than writing the cell.
        crate::tests::run_test_once(
            r##"
            ["EUC-KR", "GB2312"].flat_map do |e|
              [0xC12A, 0xAC02, 0x02CA, 0xAC00].map do |cp|
                s = [cp].pack("U")
                one = (s.encode(e).bytes rescue $!.class.to_s)
                cv = (Encoding::Converter.new("UTF-8", e).convert(s.dup).bytes rescue $!.class.to_s)
                repl = s.encode(e, undef: :replace).bytes
                [one, cv, one == cv, repl]
              end
            end
            "##,
        );
        // A capped destination asks a second path what a character
        // writes, and it has to refuse the unholdable cell too — the
        // first byte of one leaked into the destination otherwise,
        // and the call answered `:destination_buffer_full` where the
        // character had no cell at all.
        crate::tests::run_test_once(
            r##"
            u = [0xC12A].pack("U")
            r = []
            r << Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
                   .convert("\u{AC00}#{u}\u{AC01}".dup).bytes
            r << "\u{AC00}#{u}\u{AC01}".encode("EUC-KR", undef: :replace).bytes
            ec = Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
            s = "\u{AC00}#{u}\u{AC01}".dup
            d = "".dup
            steps = []
            6.times do
              x = ec.primitive_convert(s, d, nil, 3)
              steps << [x, d.bytes.dup]
              break if x == :finished
            end
            r << steps
            e2 = Encoding::Converter.new("UTF-8", "EUC-KR")
            s2 = "\u{AC00}#{u}\u{AC01}".dup
            d2 = "".dup
            r << [e2.primitive_convert(s2, d2, nil, 3), d2.bytes, s2.bytes]
            "##,
        );
        // The streaming path writes the cell before it can be asked
        // about, so an output carrying one is redone character by
        // character. Uncapped it substitutes and finishes; capped so
        // that the redone output runs out of room, it fills to the
        // byte and holds the rest like any other destination.
        crate::tests::run_test_once(
            r##"
            u = [0xC12A].pack("U")
            ec = Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
            s = "\u{AC00}#{u}\u{AC01}".dup
            d = "".dup
            r = [ec.primitive_convert(s, d), s.bytes, d.bytes]
            [[4, "\u{AC00}#{u}\u{AC01}"], [6, "#{u}#{u}\u{AC01}"]].each do |cap, text|
              e2 = Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
              s2 = text.dup
              d2 = "".dup
              steps = []
              8.times do
                x = e2.primitive_convert(s2, d2, nil, cap)
                steps << [x, d2.bytes.dup]
                break if x == :finished
              end
              r << [cap, steps]
            end
            r
            "##,
        );
        // Through the streaming API too, with a capped destination.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "EUC-KR")
            s = ("\u{AC00}" + [0xC12A].pack("U") + "\u{AC01}").dup
            d = "".dup
            r = [ec.primitive_convert(s, d), s.bytes, d.bytes,
                 ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            e2 = Encoding::Converter.new("UTF-8", "EUC-KR")
            s2 = "\u{AC00}\u{AC01}".dup
            d2 = "".dup
            r << e2.primitive_convert(s2, d2, nil, 3) << d2.bytes << s2.bytes
            "##,
        );
    }

    #[test]
    fn euc_jp_and_shift_jis_convert_without_a_pivot() {
        // The two spell the same JIS X 0208 plane, so CRuby maps them
        // cell to cell and never asks which Unicode character is
        // involved — which reaches the cells its own tables have no
        // Unicode home for (#1460).
        crate::tests::run_test_once(
            r##"
            [[0xA2, 0xAF], [0xF6, 0xAF], [0xAD, 0xA1], [0xA1, 0xA1], [0xF4, 0xA6],
             [0xA4, 0xA2], [0xFE, 0xFE]].map do |b1, b2|
              s = [b1, b2].pack("C*").force_encoding("EUC-JP")
              one = (s.encode("Shift_JIS").bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("EUC-JP", "Shift_JIS").convert(s.dup).bytes rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        crate::tests::run_test_once(
            r##"
            [[0x81, 0xAD], [0x87, 0x40], [0xEB, 0xAD], [0x81, 0x40], [0xF0, 0x40],
             [0xFA, 0x40], [0xED, 0x40]].map do |b1, b2|
              s = [b1, b2].pack("C*").force_encoding("Shift_JIS")
              one = (s.encode("EUC-JP").bytes rescue $!.class.to_s)
              cv = (Encoding::Converter.new("Shift_JIS", "EUC-JP").convert(s.dup).bytes rescue $!.class.to_s)
              [one, cv, one == cv]
            end
            "##,
        );
        // Half-width katakana changes shape, the JIS X 0212 plane has
        // nowhere to go, and ASCII rides through.
        crate::tests::run_test_once(
            r##"
            r = []
            r << ("x" + [0x8E, 0xB1].pack("C*")).force_encoding("EUC-JP").encode("Shift_JIS").bytes
            r << ("x" + [0xB1].pack("C*")).force_encoding("Shift_JIS").encode("EUC-JP").bytes
            r << ([0x8F, 0xA1, 0xA1].pack("C*").force_encoding("EUC-JP").encode("Shift_JIS") rescue
                  [$!.class.to_s, $!.message])
            r << ([0x8F, 0xA1, 0xA1].pack("C*").force_encoding("EUC-JP")
                    .encode("Shift_JIS", undef: :replace).bytes)
            r << ([0xF0, 0x40].pack("C*").force_encoding("Shift_JIS").encode("EUC-JP") rescue
                  [$!.class.to_s, $!.message])
            r << ([0xA1, 0x41].pack("C*").force_encoding("EUC-JP")
                    .encode("Shift_JIS", invalid: :replace).bytes)
            "##,
        );
        // Errors name the two encodings themselves: this conversion
        // has no UTF-8 hop to blame, and `convpath` says so.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("EUC-JP", "Shift_JIS")
            d = "".dup
            s = [0x8F, 0xA1, 0xA1].pack("C*").force_encoding("EUC-JP")
            r = [ec.primitive_convert(s, d),
                 ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            r << Encoding::Converter.new("EUC-JP", "Shift_JIS").convpath.map { |x| x.map(&:name) }
            r << Encoding::Converter.new("EUC-JP", "Windows-31J").convpath.map { |x| x.map(&:name) }
            "##,
        );
        // Windows-31J is not in it — CRuby pivots that pair, so the
        // row-13 cell that Shift_JIS reaches is undefined there.
        crate::tests::run_test_once(
            r##"
            [["EUC-JP", "Windows-31J"], ["Windows-31J", "EUC-JP"],
             ["Shift_JIS", "Windows-31J"]].flat_map do |a, b|
              [[0xA1, 0xA1], [0xAD, 0xA1], [0xF6, 0xAF]].map do |b1, b2|
                s = [b1, b2].pack("C*").force_encoding(a)
                (s.encode(b).bytes rescue $!.class.to_s)
              end
            end
            "##,
        );
        // Malformed input on the direct path: which bytes the run
        // covers, and which are read again rather than swallowed with
        // it. `\x80` and `\xFD` are left out — those never begin a
        // Shift_JIS sequence, and the run they report is #1546, which
        // the pivoted paths get wrong in the same way.
        crate::tests::run_test_once(
            r##"
            [[0xA1, 0x41], [0xA1, 0x20], [0xA1, 0xFF], [0x8E, 0x41], [0x8E, 0xFF],
             [0x8F, 0xA1], [0x8F, 0x41], [0xA1]].map do |b|
              s = b.pack("C*").force_encoding("EUC-JP")
              [(s.encode("Shift_JIS") rescue [$!.class.to_s, $!.message]),
               s.encode("Shift_JIS", invalid: :replace, undef: :replace).bytes]
            end
            "##,
        );
        crate::tests::run_test_once(
            r##"
            [[0x81, 0x20], [0x81, 0x7F], [0x81, 0xFD], [0x81]].map do |b|
              s = b.pack("C*").force_encoding("Shift_JIS")
              [(s.encode("EUC-JP") rescue [$!.class.to_s, $!.message]),
               s.encode("EUC-JP", invalid: :replace, undef: :replace).bytes]
            end
            "##,
        );
        // The same through the converter, where an incomplete tail is
        // the next call's to finish rather than an error.
        crate::tests::run_test_once(
            r##"
            [[0xA1, 0x41], [0x8F, 0xA1], [0xA1]].map do |b|
              ec = Encoding::Converter.new("EUC-JP", "Shift_JIS")
              s = b.pack("C*").force_encoding("EUC-JP")
              d = "".dup
              r = [ec.primitive_convert(s, d, nil, nil, partial_input: true), s.bytes, d.bytes]
              ec2 = Encoding::Converter.new("EUC-JP", "Shift_JIS")
              s2 = b.pack("C*").force_encoding("EUC-JP")
              d2 = "".dup
              r << ec2.primitive_convert(s2, d2) << s2.bytes << d2.bytes
              r << ec2.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }
            end
            "##,
        );
        // A capped destination and a split call behave as everywhere
        // else.
        crate::tests::run_test_once(
            r##"
            r = []
            [0, 1, 2, 3].each do |cap|
              ec = Encoding::Converter.new("EUC-JP", "Shift_JIS")
              s = [0xF6, 0xAF].pack("C*").force_encoding("EUC-JP")
              d = "".dup
              steps = []
              8.times do
                x = ec.primitive_convert(s, d, nil, cap)
                steps << [x, d.bytes.dup]
                break if x == :finished
              end
              r << [cap, steps]
            end
            ec2 = Encoding::Converter.new("EUC-JP", "Shift_JIS")
            d2 = "".dup
            a = ec2.primitive_convert([0xF6].pack("C*").force_encoding("EUC-JP"), d2,
                                      nil, nil, partial_input: true)
            b = ec2.primitive_convert([0xAF].pack("C*").force_encoding("EUC-JP"), d2)
            r << [a, b, d2.bytes]
            "##,
        );
    }

    #[test]
    fn converter_fills_a_capped_destination_to_the_byte() {
        // CRuby fills the destination byte by byte and holds the rest
        // of that character's output for the next call, so a cap of 1
        // takes a two-byte character apart (#1532).
        crate::tests::run_test_once(
            r##"
            ["EUC-JP", "UTF-16BE", "UTF-32BE", "GBK"].flat_map do |dst|
              [1, 2, 3].map do |cap|
                ec = Encoding::Converter.new("UTF-8", dst)
                s = "あい".dup
                d = "".dup
                steps = []
                12.times do
                  r = ec.primitive_convert(s, d, nil, cap)
                  steps << [r, d.bytes.dup, s.bytesize]
                  break if r == :finished
                end
                [dst, cap, steps]
              end
            end
            "##,
        );
        // The character the cap stopped at is converted once: its
        // output is held, not its source re-read. A destination whose
        // encoder will not write into a buffer that tight made no
        // progress at all before this, so a caller looping to
        // `:finished` never got there.
        crate::tests::run_test_once(
            r##"
            ["GBK", "EUC-KR", "Shift_JIS", "EUC-JP"].map do |dst|
              ec = Encoding::Converter.new("UTF-8", dst)
              s = "éé".dup
              d = "".dup
              steps = []
              12.times do
                r = (ec.primitive_convert(s, d, nil, 2) rescue break steps << $!.class.to_s)
                steps << [r, d.bytesize, s.bytesize]
                break if r == :finished
              end
              [dst, steps, d.bytes]
            end
            "##,
        );
        // A cap that is not actually binding is an ordinary finish,
        // whatever the codec says about headroom.
        crate::tests::run_test_once(
            r##"
            ["GBK", "EUC-JP", "UTF-16BE"].flat_map do |dst|
              [4, 5, 8].map do |cap|
                ec = Encoding::Converter.new("UTF-8", dst)
                s = "éé".dup
                d = "".dup
                [dst, cap, ec.primitive_convert(s, d, nil, cap), d.bytesize, s.bytesize]
              end
            end
            "##,
        );
        // The held output goes out before anything new is converted,
        // and counts against the next call's cap — including when the
        // caller changes it.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "UTF-32BE")
            s = "ab".dup
            d = "".dup
            r = []
            r << ec.primitive_convert(s, d, nil, 3) << d.bytes.dup
            r << ec.primitive_convert(s, d, nil, 1) << d.bytes.dup
            r << ec.primitive_convert(s, d, nil, 100) << d.bytes.dup
            r << s.bytesize
            "##,
        );
    }

    #[test]
    fn converter_undef_replace_through_the_codec_pair() {
        // The branches that map the pivot themselves substitute
        // character by character; the codec pair returned the
        // undefined conversion instead, so `#primitive_convert`
        // raised where `#convert` replaced (#1542).
        crate::tests::run_test_once(
            r##"
            ["EUC-KR", "Big5"].flat_map do |dst|
              ["aéb", "éé", "\u{1F600}x"].map do |t|
                ec = Encoding::Converter.new("UTF-8", dst, undef: :replace)
                s = t.dup
                d = "".dup
                [ec.primitive_convert(s, d), s.bytes, d.bytes]
              end
            end
            "##,
        );
        // A replacement of the caller's choosing, and one that has to
        // fit where the character it stands for did not.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
            ec.replacement = "!"
            s = "aéb".dup
            d = "".dup
            r = [ec.primitive_convert(s, d), s.bytes, d.bytes]
            r << (0..4).map do |cap|
              e2 = Encoding::Converter.new("UTF-8", "EUC-KR", undef: :replace)
              ss = "aéb".dup
              dd = "".dup
              [cap, e2.primitive_convert(ss, dd, nil, cap), ss.bytes, dd.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_reports_an_unconvertible_character_over_a_full_destination() {
        // A full destination is not why the encoder stopped if the
        // character it stopped at has no cell there at all — CRuby
        // reports that undefined conversion in the same call (#1533).
        // EUC-KR and Big5 reach it through the codec pair, where the
        // cap hides the answer; the branches that map the pivot
        // themselves already decided it in this order.
        crate::tests::run_test_once(
            r##"
            [["EUC-KR", "aéb"], ["Big5", "aéb"],
             ["Shift_JIS", "aéb"], ["US-ASCII", "aあb"],
             ["ISO-8859-1", "aあb"]].flat_map do |dst, text|
              [0, 1, 2].map do |cap|
                ec = Encoding::Converter.new("UTF-8", dst)
                s = text.dup
                d = "".dup
                [cap, ec.primitive_convert(s, d, nil, cap), s.bytes, d.bytes,
                 ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
              end
            end
            "##,
        );
        // With a replacement the character does have a cell, so the
        // cap is the answer again. Shift_JIS stands in for EUC-KR
        // here: the codec-pair path drops `undef: :replace` entirely,
        // which is #1542, not this.
        crate::tests::run_test_once(
            r##"
            [0, 1, 2, 3].map do |cap|
              ec = Encoding::Converter.new("UTF-8", "Shift_JIS", undef: :replace)
              s = "aéb".dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              [first, s.bytes, d.bytes, ec.primitive_convert(s, d, nil, 100), d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_destination_cap_through_a_pivot_encoding() {
        // A UTF-16 / UTF-32 source is decoded to a UTF-8 pivot before
        // anything is converted, so the destination cap is felt in
        // pivot bytes and has to be mapped back to source units —
        // two per UTF-16 code unit, four per UTF-32 character (#1511).
        crate::tests::run_test_once(
            r##"
            [["UTF-16BE", 3], ["UTF-16BE", 5], ["UTF-32LE", 2], ["UTF-32LE", 6]].map do |src, cap|
              ec = Encoding::Converter.new(src, "EUC-JP")
              s = "あabc".encode(src).dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              mid = [s.bytes, d.bytes]
              [first, mid, ec.primitive_convert(s, d, nil, 200), s.bytes, d.bytes]
            end
            "##,
        );
        // A UTF-16 / UTF-32 *destination* is built here rather than by
        // a codec, and counts its cap in destination bytes. CRuby
        // fills the last bytes of the buffer with the front of the
        // character that did not fit (#1532), so only the drained end
        // state is compared.
        crate::tests::run_test_once(
            r##"
            [["UTF-16LE", 3], ["UTF-32BE", 6]].map do |dst, cap|
              ec = Encoding::Converter.new("UTF-8", dst)
              s = "あabc".dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              [first, ec.primitive_convert(s, d, nil, 200), s.bytes, d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_single_byte_source_cap() {
        // A single-byte-table *source* builds the pivot itself and
        // hands it to the ordinary path, so a cap comes back measured
        // in pivot characters and has to be counted back to the one
        // source byte each of them came from (#1511).
        //
        // Cap 2 is left out: it falls inside `à`'s two UTF-8 bytes,
        // where CRuby writes the first of them and we stop on the
        // character boundary (#1532).
        crate::tests::run_test_once(
            r##"
            [1, 3, 4].map do |cap|
              ec = Encoding::Converter.new("ISO-8859-1", "UTF-8")
              s = "aàbéc".encode("ISO-8859-1").dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              mid = [s.bytes, d.bytes]
              [first, mid, ec.primitive_convert(s, d, nil, 200), s.bytes, d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_capped_destination_with_no_source_buffers_everything() {
        // A `nil` source has nowhere to leave what the cap held back,
        // so all of it goes to the converter's pending buffer instead
        // of being dropped (#1511).
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "EUC-JP")
            s = "あいう".dup
            d = "".dup
            first = ec.primitive_convert(s, d, nil, 2)
            second = ec.primitive_convert(nil, d, nil, 3)
            [first, second, s.bytes, d.bytes]
            "##,
        );
    }

    #[test]
    fn converter_single_byte_destination_cap() {
        // A single-byte-table destination maps the pivot itself, one
        // byte per character, and has to report the cap in *source*
        // bytes — which a non-ASCII source does not count the same way
        // (#1511).
        crate::tests::run_test_once(
            r##"
            [1, 2, 3].map do |cap|
              ec = Encoding::Converter.new("UTF-8", "ISO-8859-1")
              s = "àéü".dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              mid = [s.bytes, d.bytes]
              [first, mid, ec.primitive_convert(s, d, nil, 200), s.bytes, d.bytes]
            end
            "##,
        );
    }

    #[test]
    fn converter_ascii_destination_with_a_wide_source() {
        // The US-ASCII / BINARY destination walks characters itself,
        // so both the cap and an unconvertible character have to be
        // translated back through the pivot to source bytes — which a
        // multi-byte source does not count one-for-one (#1511).
        crate::tests::run_test_once(
            r##"
            [["EUC-JP", "US-ASCII", 1, "abあc"],
             ["EUC-JP", "US-ASCII", nil, "abあc"],
             ["Shift_JIS", "BINARY", 1, "abあc"],
             ["ISO-8859-1", "US-ASCII", 1, "aébc"],
             ["ISO-8859-1", "US-ASCII", nil, "aébc"]].map do |src, dst, cap, text|
              ec = Encoding::Converter.new(src, dst)
              s = text.encode(src).dup
              d = "".dup
              first = ec.primitive_convert(s, d, nil, cap)
              [first, s.bytes, d.bytes,
               ec.primitive_errinfo.map { |x| x.is_a?(String) ? x.bytes : x }]
            end
            "##,
        );
    }

    #[test]
    fn converter_stream_ends_on_an_empty_source() {
        // A `primitive_convert` that brings no source of its own says
        // the input has ended — `nil` and `""` alike. Every later call
        // answers `:finished` having converted nothing, and leaves
        // `src` alone (#1537).
        crate::tests::run_test_once(
            r##"
            [nil, ""].map do |empty|
              ec = Encoding::Converter.new("UTF-8", "EUC-JP")
              d = "".dup
              a = ec.primitive_convert(empty && empty.dup, d)
              s = "ab".dup
              b = ec.primitive_convert(s, d)
              [a, b, s.bytes, d.bytes,
               ec.primitive_errinfo, ec.last_error,
               (ec.convert("cd".dup) rescue [$!.class.to_s, $!.message])]
            end
            "##,
        );
        // `partial_input:` says more is merely not here yet: the
        // stream stays open, and the answer is `:source_buffer_empty`.
        crate::tests::run_test_once(
            r##"
            [nil, ""].map do |empty|
              ec = Encoding::Converter.new("UTF-8", "EUC-JP")
              d = "".dup
              a = ec.primitive_convert(empty && empty.dup, d, nil, nil, partial_input: true)
              b = ec.primitive_convert(empty && empty.dup, d, nil, nil, partial_input: true)
              s = "ab".dup
              c = ec.primitive_convert(s, d)
              [a, b, c, s.bytes, d.bytes]
            end
            "##,
        );
        // Whatever an earlier destination cap held back is still
        // converted by the call that ends the stream — it is the
        // *caller's* source that is empty, not the converter.
        crate::tests::run_test_once(
            r##"
            r = []
            [false, true].each do |partial|
              ec = Encoding::Converter.new("UTF-8", "EUC-JP")
              d = "".dup
              a = ec.primitive_convert("あab".dup, d, nil, 2)
              b = ec.primitive_convert("".dup, d, nil, 100, partial_input: partial)
              s = "zz".dup
              c = ec.primitive_convert(s, d, nil, 100)
              r << [a, b, c, s.bytes, d.bytes]
            end
            r
            "##,
        );
        // `#finish` ends it the same way, and the destination is still
        // truncated to `dst_offset` and re-tagged by a call that comes
        // after the end.
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "EUC-JP")
            r = [ec.finish.bytes, ec.finish.bytes]
            s = "ab".dup
            r << ec.primitive_convert(s, "".dup) << s.bytes
            ec2 = Encoding::Converter.new("UTF-8", "EUC-JP")
            ec2.primitive_convert(nil, "".dup)
            d = "xyz".dup.force_encoding("UTF-8")
            r << ec2.primitive_convert("ab".dup, d, 1) << d.bytes << d.encoding.name
            r << (ec2.convert("ab".dup) rescue [$!.class.to_s, $!.message])
            "##,
        );
    }

    #[test]
    fn converter_argument_errors_name_the_class() {
        // CRuby's `TypeError` names the class it could not convert;
        // ours said "expected String", and the destination's said
        // "nil" whatever was passed (#1537, found alongside).
        crate::tests::run_test_once(
            r##"
            ec = Encoding::Converter.new("UTF-8", "EUC-JP")
            [42, :ab, [1], nil].map do |bad|
              [(ec.primitive_convert(bad, "".dup) rescue [$!.class.to_s, $!.message]),
               (ec.primitive_convert("a".dup, bad) rescue [$!.class.to_s, $!.message]),
               (ec.convert(bad) rescue [$!.class.to_s, $!.message])]
            end
            "##,
        );
    }

    #[test]
    fn converter_ascii_destination_leaves_the_rest_in_src() {
        // The US-ASCII / BINARY destination has no codec of its own,
        // and used to report the whole source as consumed however far
        // it actually got — losing everything after an error or a
        // destination cap (#1511).
        crate::tests::run_test_once(
            r##"
            r = []
            ["US-ASCII", "BINARY"].each do |dst|
              ec = Encoding::Converter.new("UTF-8", dst)
              s = "aéb".dup
              d = "".dup
              r << [ec.primitive_convert(s, d), s.bytes, d.bytes]
              ec = Encoding::Converter.new("UTF-8", dst)
              s = "ab\xFF\xFEcd".dup
              d = "".dup
              r << [ec.primitive_convert(s, d), s.bytes, d.bytes]
              ec = Encoding::Converter.new("UTF-8", dst)
              s = "glark".dup
              d = "".dup
              r << [ec.primitive_convert(s, d, nil, 1), s.dup, d.dup]
              r << [ec.primitive_convert(s, d, nil, 100), s.dup, d.dup]
            end
            r
            "##,
        );
    }

    #[test]
    fn utf8_mac_converts_rather_than_renames() {
        // `UTF8-MAC` used to be an alias of UTF-8, so a conversion
        // into it handed back the composed bytes it was given. It is
        // now Apple's HFS+ form: canonically decomposed, minus the
        // codepoints the table keeps (#1562).
        crate::tests::run_test_once(
            r##"
            r = []
            r << "が".encode("UTF8-MAC").bytes
            r << "が".encode("UTF8-MAC").encoding.name
            r << "が".dup.force_encoding("UTF8-MAC").encode("UTF-8").bytes
            # The singletons the table keeps composed, where NFC/NFD
            # would move them.
            r << ["Å", "Ω", "豈"].map { |c| c.encode("UTF8-MAC").bytes }
            # Two marks on one base come back in canonical order.
            r << "ṩ".encode("UTF8-MAC").codepoints
            # Through a third encoding: the pivot is UTF-8 both ways.
            r << "が".encode("UTF8-MAC").encode("EUC-JP").bytes
            r << "が".encode("EUC-JP").encode("UTF8-MAC").bytes
            r
            "##,
        );
    }

    #[test]
    fn utf8_mac_is_its_own_encoding_not_an_alias_of_utf8() {
        // Its bytes are UTF-8's, so every read path treats it alike —
        // but CRuby calls the two incompatible encodings, and mixing
        // them raises wherever it would for any other pair (#1562).
        crate::tests::run_test_once(
            r##"
            m = "abcé".dup.force_encoding("UTF8-MAC")
            u = "é"
            r = [m.length, m.chars.size, m.valid_encoding?, m.ascii_only?,
                 m.upcase, m.upcase.encoding.name, m.succ, m[3].encoding.name,
                 m.dup.concat(0x1E69).bytes, Marshal.dump(m).bytes,
                 Marshal.load(Marshal.dump(m)).encoding.name]
            # `sub` / `gsub` with a replacement of the other variant
            # is left out: their `&str` path settles the encoding per
            # piece and does not yet tell the two apart (#1562).
            [lambda { m =~ /#{u}/ }, lambda { m.match(/#{u}/) },
             lambda { m.match?(/#{u}/) }, lambda { m.split(/#{u}/) },
             lambda { m.sub(u, "z") }, lambda { m.gsub(u, "z") },
             lambda { m.end_with?(u) }, lambda { m.index(u) },
             lambda { m + u }, lambda { m.unicode_normalize(:nfc) },
             lambda { [m, u].join }, lambda { m.center(8, u) },
             # ...while an ASCII-only argument stays compatible.
             lambda { m.end_with?("cé".dup.force_encoding("UTF8-MAC")) },
             lambda { m.sub("a", "z") }].each do |f|
              r << (begin; f.call; rescue => e; e.class.to_s; end)
            end
            r
            "##,
        );
    }

    #[test]
    fn a_bom_dummy_source_reads_the_bom_it_was_written_with() {
        // `UTF-16` and `UTF-32` worked as a destination and not as a
        // source, so a round trip through either raised
        // `ConverterNotFoundError` — `dummy_wide_target` had no
        // matching source half (#1585).
        crate::tests::run_test_once(
            r##"
            s = "aあb"
            %w[UTF-16 UTF-32].map do |e|
              enc = s.encode(e)
              wide = e.sub("UTF", "UTF")
              le = (e == "UTF-16" ? [0xFF,0xFE] : [0xFF,0xFE,0,0]).pack("C*") +
                   s.encode(wide + "LE").b
              le.force_encoding(e)
              nb = s.encode(e + "BE").b.dup.force_encoding(e)
              [enc.b.bytes,
               # the BOM picks the endianness and is consumed...
               (begin; enc.encode("UTF-8").codepoints; rescue => x; [x.class.to_s, x.message]; end),
               (begin; le.encode("UTF-8").codepoints; rescue => x; [x.class.to_s, x.message]; end),
               # ...and without one the source is ill-formed, named by
               # its first code unit as the dummy encoding's own.
               (begin; nb.encode("UTF-8").codepoints; rescue => x; [x.class.to_s, x.message]; end),
               # a truncated first unit is incomplete rather than invalid
               (begin
                  enc.b.bytes.first(1).pack("C*").dup.force_encoding(e).encode("UTF-8")
                rescue => x
                  [x.class.to_s, x.message, x.incomplete_input?]
                end),
               # and it still converts onward past the pivot
               (begin; enc.encode("EUC-JP").bytes; rescue => x; x.class.to_s; end)]
            end
            "##,
        );
    }

    #[test]
    fn invalid_replace_is_honoured_for_a_us_ascii_source() {
        // US-ASCII has no `encoding_rs` codec, no single-byte table and
        // no `mbc_walker`, so none of the three paths that honour
        // `invalid:` covered it: it raised whatever the caller asked
        // for, and the destinations with no codec of their own copied
        // the offending byte through (#1570).
        crate::tests::run_test_once(
            r##"
            s = "a\x80b".dup.force_encoding("US-ASCII")
            r = [s.valid_encoding?, s.scrub("?")]
            ["UTF-8", "EUC-JP", "US-ASCII", "ASCII-8BIT", "UTF-16BE"].each do |d|
              r << (begin; s.encode(d, invalid: :replace).bytes; rescue => e; e.class.to_s; end)
            end
            r << s.encode("UTF-8", invalid: :replace, replace: "!").bytes
            # ...while a source that is merely ill-formed still raises
            # without the option, with the message it already had.
            r << (begin; s.encode("UTF-8"); rescue => e; [e.class.to_s, e.message]; end)
            r
            "##,
        );
    }

    #[test]
    fn the_replacement_follows_the_encoding_it_is_inserted_in() {
        // `U+FFFD` or `"?"` is decided by a table of encoding *names*,
        // asked about the encoding the last transcoder inserts into.
        // Resolving it against the UTF-8 pivot rather than the
        // destination gave `UTF8-MAC` the `U+FFFD` that only UTF-8 and
        // the wide forms take, and the endianness-less dummies took
        // `"?"` where their big-endian form takes `U+FFFD` (#1571).
        crate::tests::run_test_once(
            r##"
            s = "a\xffb".dup.force_encoding("UTF-8")
            dsts = %w[UTF-8 UTF8-MAC CESU-8 EUC-JP Shift_JIS US-ASCII ASCII-8BIT
                      UTF-16BE UTF-16LE UTF-32BE UTF-32LE UTF-16 UTF-32
                      UCS-2BE UCS-4BE ISO-8859-1 Windows-31J]
            r = dsts.map do |d|
              [d, (begin; s.encode(d, invalid: :replace).b.bytes; rescue => e; e.class.to_s; end)]
            end
            # `Converter#replacement` keeps the same string, and CRuby
            # holds it in the encoding it is inserted in rather than in
            # the destination — UTF-8 even for a UTF-16BE destination.
            # CESU-8 and the two BOM dummies are left out:
            # `Encoding::Converter` does not take them as destinations
            # yet although `String#encode` does, which is its own gap.
            r << (dsts - %w[CESU-8 UTF-16 UTF-32]).map do |d|
              [d, (begin
                     ec = Encoding::Converter.new("UTF-8", d)
                     [ec.replacement.b.bytes, ec.replacement.encoding.name]
                   rescue => e
                     e.class.to_s
                   end)]
            end
            # The same-encoding scrub asks the source's own name, so
            # CESU-8 takes "?" although a conversion *into* it does not.
            r << "\xf0".dup.force_encoding("CESU-8").encode("CESU-8", invalid: :replace).b.bytes
            r << "\xff".dup.force_encoding("UTF-8").encode("UTF-8", invalid: :replace).b.bytes
            r
            "##,
        );
    }

    #[test]
    fn a_conversion_past_the_pivot_names_the_destination_it_was_given() {
        // `UTF8-MAC` and CESU-8 convert through UTF-8, so a source
        // that cannot reach the pivot fails on the first of two hops —
        // which CRuby spells out, naming the destination the caller
        // asked for. A source that *is* UTF-8 reaches the pivot by
        // doing nothing, so only this notices that its bytes are
        // broken (#1562).
        //
        // `#destination_encoding_name` is the failing hop's, which is
        // the pivot: reading the whole tail of the message answered
        // "UTF-8 to UTF8-MAC" — a pre-existing misparse, wrong the
        // same way for `"\xC3\xA9".b.encode("UTF-16BE")`.
        crate::tests::run_test_once(
            r##"
            [["\xff".b, "ASCII-8BIT"], ["\xff".dup.force_encoding("UTF-8"), "UTF-8"],
             ["\x80\x40".dup.force_encoding("EUC-JP"), "EUC-JP"]].map do |s, n|
              ["UTF8-MAC", "CESU-8", "UTF-8", "UTF-16BE"].map do |d|
                begin
                  [n, d, s.encode(d).bytes]
                rescue => e
                  [n, d, e.class.to_s, e.message,
                   e.destination_encoding_name, e.source_encoding_name]
                end
              end
            end
            "##,
        );
    }

    #[test]
    fn cesu8_spells_the_astral_planes_as_surrogate_pairs() {
        // UTF-8's own four-byte sequence is invalid here and a pair of
        // three-byte surrogate halves is one character, so CESU-8 needs
        // a byte walk and a codec of its own rather than UTF-8's
        // (#1562).
        crate::tests::run_test_once(
            r##"
            r = []
            r << "\u{1F600}".encode("CESU-8").bytes
            r << ["\u{1F600}".encode("CESU-8").length,
                  "\u{1F600}".encode("CESU-8").bytesize,
                  "\u{1F600}".encode("CESU-8").encoding.name]
            r << "\u{1F600}".encode("CESU-8").encode("UTF-8").bytes
            # Below U+10000 the bytes are UTF-8's, unchanged.
            r << "abcéあ".encode("CESU-8").bytes
            # Through a third encoding.
            r << "あ".encode("CESU-8").encode("EUC-JP").bytes
            r << "あ".encode("EUC-JP").encode("CESU-8").bytes
            r << (begin; "\u{1F600}".encode("CESU-8").encode("EUC-JP"); rescue => e; e.class.to_s; end)
            r
            "##,
        );
    }

    #[test]
    fn cesu8_reports_its_own_ill_formed_runs() {
        // The run a message quotes is the well-formed prefix, and the
        // byte after it is handed back to be read again — except when
        // the lead byte started nothing at all (#1562).
        crate::tests::run_test_once(
            r##"
            [[0x41,0xF0,0x9F,0x98,0x80,0x42],
             [0x41,0xED,0xA0,0xBD,0x42],
             [0x41,0xED,0xA0,0xBD],
             [0x41,0xED,0xA0],
             [0x41,0xED,0xB0,0x80,0x42],
             [0x41,0x80,0x42],
             [0x41,0xE3,0x81],
             [0x41,0xE3,0x81,0x42],
             [0x41,0xC2],
             [0x41,0xED,0xA0,0xBD,0xED,0x9F,0xBF,0x42]].map do |b|
              s = b.pack("C*").dup.force_encoding("CESU-8")
              begin
                [s.valid_encoding?, s.scrub("?"), s.encode("UTF-8").bytes]
              rescue Encoding::InvalidByteSequenceError => e
                [s.valid_encoding?, s.scrub("?"), e.message,
                 e.error_bytes.bytes, e.readagain_bytes&.bytes, e.incomplete_input?]
              end
            end
            "##,
        );
    }

    #[test]
    fn the_names_that_stand_for_a_setting_name_what_it_holds() {
        // `"locale"`, `"external"` and `"filesystem"` are references to
        // an encoding, not encodings: CRuby keeps them in its encoding
        // table as aliases it re-points whenever the setting behind
        // them moves. monoruby had them in a table that answered UTF-8
        // outright, so every resolver but `Encoding.find` and
        // `force_encoding` disagreed with the interpreter's own
        // setting. Asserted against `Encoding.find` rather than against
        // a name, so the answer does not depend on the locale the test
        // runs under (#1575).
        run_test_once(
            r#"
              names = %w[locale external filesystem LOCALE External]
              [
                names.map { |n| "abc".encode(n).encoding == Encoding.find(n) },
                names.map { |n| "abc".encode("UTF-8", n).encoding.name },
                names.map { |n| Encoding::Converter.new("UTF-16BE", n).destination_encoding == Encoding.find(n) },
                names.map { |n| "abc".dup.force_encoding(n).encoding == Encoding.find(n) },
                (require "stringio"
                 io = StringIO.new
                 io.set_encoding("locale")
                 io.external_encoding == Encoding.find("locale")),
                # The names resolve through `#to_str` coercion too,
                # which reaches the resolver by a different route than
                # a String argument does.
                (o = Object.new
                 def o.to_str = "locale"
                 ["abc".dup.force_encoding(o).encoding == Encoding.find("locale"),
                  String.new("abc", encoding: o).encoding == Encoding.find("locale")]),
                # `"internal"` is the one of the four that can name
                # nothing at all: with no `default_internal` CRuby never
                # registered the alias, so the converters call the name
                # unknown where `rb_to_encoding` still falls back to
                # BINARY.
                [Encoding.find("internal"),
                 ("abc".encode("internal") rescue $!.class.name),
                 (Encoding::Converter.new("UTF-8", "internal") rescue $!.class.name),
                 "abc".dup.force_encoding("internal").encoding.name],
              ]
            "#,
        );
    }

    #[test]
    fn a_conversion_to_the_default_external_encoding_can_fail() {
        // The row that bites in #1575: under a US-ASCII default
        // external, `"\u00e9".encode("locale")` is a conversion CRuby
        // refuses because the target cannot hold the character, and it
        // silently succeeded — returning the string labelled UTF-8.
        // The settings are assigned here rather than read, so this
        // holds whatever locale the test runs under.
        run_test_once(
            r#"
              Encoding.default_external = Encoding::US_ASCII
              a = [
                %w[external filesystem].map { |n| "abc".encode(n).encoding.name },
                %w[external filesystem].map { |n| ("\u00e9".encode(n) rescue $!.message) },
                %w[external filesystem].map { |n| "abc".dup.force_encoding(n).encoding.name },
              ]
              Encoding.default_external = Encoding::EUC_JP
              b = [
                %w[external filesystem].map { |n| "\u3042".encode(n).bytes },
                Encoding::Converter.new("UTF-8", "external").destination_encoding.name,
              ]
              # `"internal"` resolves once there is a `default_internal`
              # for it to name — CRuby registers the alias on the
              # assignment.
              Encoding.default_internal = Encoding::EUC_JP
              c = [
                "abc".encode("internal").encoding.name,
                Encoding::Converter.new("UTF-8", "internal").destination_encoding.name,
                Encoding.find("internal").name,
              ]
              Encoding.default_internal = nil
              [a, b, c]
            "#,
        );
    }

    #[test]
    fn a_broken_utf8_mac_source_replaces_where_its_decoder_is() {
        // `from_UTF8_MAC` keeps the cluster it is composing to itself
        // until the next starter arrives, so a bad byte reaches the
        // output before it — which is why every destination past the
        // pivot sees the replacement one character early, and the
        // pivot itself, the one destination that conversion reaches in
        // a single transcoder, does not. The character follows the
        // same rule from the other side: that single hop inserts into
        // `UTF8-MAC`, which CRuby does not spell `U+FFFD` for, so it
        // is `"?"` where the two-hop conversions answer for their own
        // destination (#1577).
        run_test_once(
            r#"
              def mac(s) = s.dup.force_encoding("UTF8-MAC")
              [
                %w[UTF-8 CESU-8 EUC-JP Shift_JIS US-ASCII ISO-8859-1 UTF-16BE ASCII-8BIT].map { |d|
                  mac("a\xffb").encode(d, invalid: :replace).bytes },
                # One character back, not to the front of the string.
                ["abc\xffd", "ab\xffcd", "\xffab", "ab\xff", "a\xff\xffb", "a\xc3"].map { |b|
                  [mac(b).encode("EUC-JP", invalid: :replace).bytes,
                   mac(b).encode("UTF-8", invalid: :replace).bytes] },
                # The held cluster is still open, so a mark after the
                # bad byte joins it where a second transcoder follows,
                # and does not where the pivot is the destination.
                ["a\u0301\xffb", "a\xff\u0301b", "a\u0301b\xff"].map { |b|
                  [mac(b).encode("EUC-JP", invalid: :replace).bytes,
                   mac(b).encode("UTF-8", invalid: :replace).bytes] },
                # `undef:` is the other half of the conversion, and is
                # reported where it happens.
                [mac("a\u00e9b").encode("US-ASCII", undef: :replace).bytes,
                 mac("a\xff\u00e9b").encode("US-ASCII", invalid: :replace, undef: :replace).bytes,
                 mac("a\xffb").encode("EUC-JP", invalid: :replace, replace: "!").bytes],
                # Nothing is held after a character no mark can attach
                # to — CRuby's table is the Basic Multilingual Plane —
                # so a bad byte after an astral character comes out
                # behind it.
                ["\u{1F600}\xffb", "a\u{1F600}\xffb", "\u{1F600}\u0301\xffb"].map { |b|
                  [mac(b).encode("UTF-16BE", invalid: :replace).bytes,
                   mac(b).encode("UTF-8", invalid: :replace).bytes] },
                # An empty replacement drops the bad byte and nothing
                # else, at either end of the rule.
                [mac("a\xffb").encode("EUC-JP", invalid: :replace, replace: "").bytes,
                 mac("a\xffb").encode("UTF-8", invalid: :replace, replace: "").bytes,
                 mac("\xff\xff\xff").encode("EUC-JP", invalid: :replace).bytes,
                 mac("").encode("EUC-JP", invalid: :replace).bytes],
              ]
            "#,
        );
    }

    #[test]
    fn a_converter_inserts_in_the_encoding_its_last_step_writes() {
        // `Encoding::Converter#replacement` asks
        // `rb_econv_encoding_to_insert_output`, which answers with the
        // last transcoder's *source* encoding when that transcoder is
        // an `asciicompat_encoder` — `from_UTF8_MAC` is the only one
        // monoruby converts through. CESU-8 is the other half of the
        // same question from the destination side: a converter
        // inserting into it writes `U+FFFD`, although scrubbing a
        // CESU-8 string writes `"?"` (#1571, #1577).
        run_test_once(
            r#"
              [
                [["UTF8-MAC", "UTF-8"], ["UTF8-MAC", "EUC-JP"], ["UTF8-MAC", "UTF-16BE"],
                 ["UTF8-MAC", "CESU-8"], ["UTF-8", "CESU-8"], ["EUC-JP", "CESU-8"],
                 ["UTF-8", "UTF8-MAC"], ["UTF-8", "UTF-16BE"]].map { |a, b|
                  r = Encoding::Converter.new(a, b).replacement
                  [r.bytes, r.encoding.name] },
                # Scrubbing answers for the string's own encoding, and
                # CESU-8's own replacement is still "?".
                ["\xf0".dup.force_encoding("CESU-8").encode("CESU-8", invalid: :replace).bytes,
                 "\xf0".dup.force_encoding("CESU-8").scrub.bytes],
              ]
            "#,
        );
    }

    #[test]
    fn a_streamed_broken_utf8_mac_source_adds_up_to_the_one_shot_one() {
        // The converter used to refuse the input outright — `invalid:
        // :replace` never reached a `UTF8-MAC` source at all. It
        // replaces now, and what it holds back between calls is
        // source rather than CRuby's converted output, so a chunk
        // boundary can fall in a different place; the bytes either
        // side of it are the same conversion (#1577).
        run_test_once(
            r#"
              def mac(s) = s.dup.force_encoding("UTF8-MAC")
              # In bytes, so that an empty chunk's answer cannot bring
              # an encoding of its own to the join.
              def drive(dst, *chunks)
                c = Encoding::Converter.new("UTF8-MAC", dst, invalid: :replace)
                chunks.flat_map { |b| c.convert(mac(b)).bytes } + c.finish.bytes
              end
              [
                [drive("EUC-JP", "a\xffb"), drive("EUC-JP", "a\xff", "b"),
                 drive("EUC-JP", "a", "\xffb"), drive("EUC-JP", "a", "\xff", "b"),
                 mac("a\xffb").encode("EUC-JP", invalid: :replace).bytes],
                [drive("UTF-8", "ab\xffcd"), drive("UTF-8", "ab", "\xff", "cd"),
                 mac("ab\xffcd").encode("UTF-8", invalid: :replace).bytes],
                [drive("UTF-16BE", "a\xffb"),
                 mac("a\xffb").encode("UTF-16BE", invalid: :replace).bytes],
                # A mark in the next chunk still joins the cluster the
                # bad byte interrupted.
                [drive("EUC-JP", "a\xff", "\u0301b"),
                 mac("a\xff\u0301b").encode("EUC-JP", invalid: :replace).bytes],
                [drive("UTF-16BE", "\u{1F600}\xffb"), drive("UTF-16BE", "\u{1F600}", "\xffb"),
                 mac("\u{1F600}\xffb").encode("UTF-16BE", invalid: :replace).bytes],
                # A truncated character at a chunk end waits for the
                # rest of itself; at the end of the stream it is
                # ill-formed like any other.
                [drive("EUC-JP", "ab\xc3"), drive("EUC-JP", "ab\xc3", "\x81"),
                 mac("ab\xc3").encode("EUC-JP", invalid: :replace).bytes],
              ]
            "#,
        );
    }

    #[test]
    fn a_capped_destination_keeps_what_a_utf8_mac_source_held_back() {
        // The cluster a `UTF8-MAC` source holds for a composition is
        // read ahead without being converted, so it has to be
        // buffered; it was being skipped along with the character
        // whose output the cap had already taken, and `"abcd"` came
        // back through a two-byte destination as `"abd"` (#1577).
        run_test_once(
            r#"
              def drain(src_enc, bytes, dst_enc, cap, rounds = 8)
                c = Encoding::Converter.new(src_enc, dst_enc, invalid: :replace)
                src = bytes.dup.force_encoding(src_enc)
                dst = String.new(encoding: dst_enc)
                out = []
                rounds.times { c.primitive_convert(src, dst, 0, cap); out << dst.bytes.dup; dst.clear }
                out.flatten
              end
              # A cap can stop inside a composed cluster, whose source
              # cannot be cut there — one cluster is one piece of the
              # pivot. `"a\u0301b\u0302c"` through a one-byte
              # destination used to hold the `b`'s output and convert
              # it again next call, and never got past it.
              nfd = "a\u0301b\u0302c".encode("UTF8-MAC")
              [
                [drain("UTF8-MAC", "abcd", "UTF-16BE", 2),
                 "abcd".encode("UTF-16BE").bytes],
                (1..4).map { |cap|
                  [drain("UTF8-MAC", nfd, "UTF-8", cap, 14),
                   drain("UTF8-MAC", nfd, "UTF-16BE", cap, 14)] },
                [nfd.encode("UTF-8").bytes, nfd.encode("UTF-16BE").bytes],
                [drain("UTF8-MAC", "ab\xffcd", "UTF-16BE", 2),
                 "ab\xffcd".dup.force_encoding("UTF8-MAC").encode("UTF-16BE", invalid: :replace).bytes],
                [drain("UTF-8", "abcd", "UTF-16BE", 3),
                 "abcd".encode("UTF-16BE").bytes],
              ]
            "#,
        );
    }
}
