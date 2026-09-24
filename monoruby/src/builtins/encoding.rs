use super::*;
use crate::value::transcode::*;
use crate::value::transcode_bytes_with_opts;

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
    if first.is_ascii_uppercase() && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        out.push(name.to_string());
    }
    let mut sanitized: String = name
        .bytes()
        .map(|b| {
            if b.is_ascii_alphanumeric() {
                b as char
            } else {
                '_'
            }
        })
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
                .set_ivar(
                    val,
                    IdentId::_ENCODING,
                    Value::string_usascii_from_str(canonical),
                )
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
    // Its state is a payload of its own (`ConverterInner`), not
    // instance variables: the pair, the flags and every buffer the
    // stream carries between calls.
    let converter = globals.store.define_class_with_instance_ty(
        "Converter",
        object_class,
        enc.id(),
        ObjTy::CONVERTER,
    );
    globals.store[converter.id()].set_alloc_func(converter_alloc);
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
        ("UNIVERSAL_NEWLINE_DECORATOR", ECONV_UNIVERSAL_NEWLINE),
        ("CRLF_NEWLINE_DECORATOR", ECONV_CRLF_NEWLINE),
        ("CR_NEWLINE_DECORATOR", ECONV_CR_NEWLINE),
        ("LF_NEWLINE_DECORATOR", ECONV_LF_NEWLINE),
        ("XML_TEXT_DECORATOR", ECONV_XML_TEXT),
        ("XML_ATTR_CONTENT_DECORATOR", ECONV_XML_ATTR_CONTENT),
        ("XML_ATTR_QUOTE_DECORATOR", ECONV_XML_ATTR_QUOTE),
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
                globals
                    .store
                    .set_ivar(v, IdentId::get_id("@name"), name)
                    .unwrap();
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

// -------------------------------------------------------
// String instance methods related to encoding
// -------------------------------------------------------

/// Resolve an encoding argument (String or Encoding object) to a validated
/// constant name via `enc_name_to_const`.  Returns the constant name on
/// success or an ArgumentError on unknown encoding.
fn resolve_enc_arg(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<&'static str> {
    let coerced = arg.is_str().is_none() && !is_encoding_object(globals, arg);
    let name = if let Some(s) = arg.is_str() {
        s.to_string()
    } else if !coerced {
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
    if let Some(c) =
        known_encoding_name(globals, &name).and_then(|canonical| enc_name_to_const(&canonical))
    {
        return Ok(c);
    }
    // A name that resolves to nothing is read a second time: CRuby's
    // `enc_arg` asks `rb_to_encoding_index` first and, when that
    // fails, takes the label with `StringValueCStr`, so a `#to_str`
    // argument is converted once more — and the second answer is the
    // one the message names.
    let name = if coerced {
        arg.coerce_to_string(vm, globals)?
    } else {
        name
    };
    if name.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    // CRuby raises `Encoding::ConverterNotFoundError` (not
    // ArgumentError) for `String#encode("xyz")` when the label is
    // unknown. The encoding-search path below the call
    // (`Encoding.find`) does still raise ArgumentError; the wrapper at
    // `resolve_enc_label` lifts it to `ConverterNotFoundError` for the
    // converter paths.
    Err(MonorubyErr::argumenterr(format!(
        "unknown encoding name - {}",
        name
    )))
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
/// DST with DECORATORS)`, each side spelled by its canonical name when
/// it resolved and as given when it did not, a side that is empty left
/// out, and `ecflags` (what [`econv_opts`] read) naming the
/// decorators.
///
/// Decorators that cannot be stacked are refused here as well, once
/// both sides have resolved. `for_encode` is `String#encode`'s way of
/// asking, which differs twice from `Encoding::Converter`'s: it reads
/// its destination before its source, and when the two are the same
/// encoding it opens a decorator-only converter, whose refusal names
/// no encodings (`code converter not found (crlf_newline,cr_newline)`).
fn resolve_converter_pair(
    vm: &mut Executor,
    globals: &mut Globals,
    src: ConverterSrc,
    dst: Value,
    for_encode: bool,
    ecflags: i64,
) -> Result<(crate::value::Encoding, crate::value::Encoding)> {
    let resolve_src = |vm: &mut Executor, globals: &mut Globals| match src {
        ConverterSrc::Given(v) => resolve_enc_label(vm, globals, v),
        ConverterSrc::Receiver(e) => Ok(Ok(encoding_const_name(e))),
    };
    let (src_label, dst_label) = if for_encode {
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
    let (sname, dname) = (display(&src_label), display(&dst_label));
    let not_found = |store: &Store| converter_not_found_named(store, &sname, &dname, ecflags);
    let (Ok(s), Ok(d)) = (&src_label, &dst_label) else {
        return Err(not_found(&globals.store));
    };
    let (Some(src), Some(dst)) = (
        encoding_from_canonical_name(s),
        encoding_from_canonical_name(d),
    ) else {
        return Err(not_found(&globals.store));
    };
    if econv_conflict(ecflags) {
        return Err(if for_encode && sname.eq_ignore_ascii_case(&dname) {
            converter_not_found_named(&globals.store, "", "", ecflags)
        } else {
            not_found(&globals.store)
        });
    }
    Ok((src, dst))
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
        return Err(converter_not_found(
            &globals.store,
            src_enc,
            dst_enc,
            &opts,
            Some(mode),
        ));
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
        reading.replace = Some(opts.replace_str(dst_enc).to_string());
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
                decoded =
                    transcode_bytes_with_opts(&bytes, src_enc, src_enc, &reading, &globals.store)?;
                String::from_utf8_lossy(&decoded)
            }
            Err(_) => {
                return Err(invalid_byte_sequence(
                    &globals.store,
                    src_enc,
                    dst_enc,
                    &bytes,
                ));
            }
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
    ecflags: i64,
) -> Result<(crate::value::Encoding, Option<crate::value::Encoding>)> {
    let Some(arg0) = lfp.try_arg(0).filter(|v| v.try_hash_ty().is_none()) else {
        let dst = current_default_internal(globals);
        if econv_conflict(ecflags) {
            // Only the decorators are asked for, and they cannot be
            // stacked (`"a".encode(crlf_newline: true, cr_newline: true)`).
            let dname = dst.map_or(self_enc.name(), |d| d.name());
            let (s, d) = if dname.eq_ignore_ascii_case(self_enc.name()) {
                ("", "")
            } else {
                (self_enc.name(), dname)
            };
            return Err(converter_not_found_named(&globals.store, s, d, ecflags));
        }
        return Ok((self_enc, dst));
    };
    let src = match lfp.try_arg(1) {
        Some(arg1) => ConverterSrc::Given(arg1),
        None => ConverterSrc::Receiver(self_enc),
    };
    let (src, dst) = resolve_converter_pair(vm, globals, src, arg0, true, ecflags)?;
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
    // The options come first, as in `str_transcode`: a bad one is
    // reported ahead of a bad encoding name, and the decorators they
    // ask for are named when a converter cannot be found.
    let ecflags = econv_opts(get_options_hash_value(lfp).and_then(|v| v.try_hash_ty()))?;
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc, ecflags)?;
    // With no destination (and no `default_internal`) the conversion is
    // to the receiver's own encoding — the options still apply, so this
    // is not a no-op: `"a\n".encode(crlf_newline: true)` converts.
    let dst_enc = dst_enc_opt.unwrap_or(self_enc);
    // `rb_econv_open` comes first: a pair no converter serves is refused
    // before the input is looked at, so a UTF-7 source (or a broken one
    // bound for UTF-7) is `ConverterNotFoundError`, never a complaint
    // about its bytes.
    refuse_pair_without_converter(src_enc, dst_enc, ecflags, &globals.store)?;
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
    let ecflags = econv_opts(get_options_hash_value(lfp).and_then(|v| v.try_hash_ty()))?;
    let (src_enc, dst_enc_opt) = resolve_encode_pair(vm, globals, lfp, self_enc, ecflags)?;
    let dst_enc = dst_enc_opt.unwrap_or(self_enc);
    refuse_pair_without_converter(src_enc, dst_enc, ecflags, &globals.store)?;
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
    replacement_text_bytes(inner.as_bytes(), inner.encoding(), store)
}

/// As `replacement_text`, for a replacement already taken apart into
/// its bytes and their encoding — which is how a converter keeps it.
fn replacement_text_bytes(
    bytes: &[u8],
    enc: crate::value::Encoding,
    store: &Store,
) -> Option<String> {
    if let Ok(s) = std::str::from_utf8(bytes)
        && (bytes.is_ascii() || enc.is_utf8_compatible())
    {
        return Some(s.to_string());
    }
    let utf8 = transcode_bytes_with_opts(
        bytes,
        enc,
        crate::value::Encoding::UTF8,
        &TranscodeOpts::default(),
        store,
    )
    .ok()?;
    String::from_utf8(utf8).ok()
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
    // The callers have run `econv_opts` over this Hash already and
    // refused what it refuses, so its answer is only read here.
    let flags = econv_opts(Some(hash)).unwrap_or(0);
    with_newline_flags(out, flags)
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
        .get_constant_noautoload(
            enc_const.as_class_id(),
            IdentId::get_id("UndefinedConversionError"),
        )
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
                let rep = vm.invoke_method_inner(
                    globals,
                    index_id,
                    fallback,
                    &[Value::string_from_str(cs)],
                    None,
                    None,
                )?;
                if rep.is_nil() {
                    return Err(cerr);
                }
                let rep_str = if rep.is_str().is_some() {
                    rep
                } else if globals.check_method(rep, IdentId::TO_STR).is_some() {
                    let converted =
                        vm.invoke_method_inner(globals, IdentId::TO_STR, rep, &[], None, None)?;
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
            return Err(MonorubyErr::argumenterr("invalid encoding name (NUL byte)"));
        }
        // `try_from_str` folds separators and case, so it is the name's
        // presence in `Encoding.name_list` that decides whether the
        // name is one at all ("utf8" is not).
        let canonical = known_encoding_name(globals, s)
            .ok_or_else(|| MonorubyErr::argumenterr(format!("unknown encoding name - {}", s)))?;
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
        return Err(MonorubyErr::argumenterr("default external can not be nil"));
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

/// `rb_inspect`: `obj.inspect`, as `rb_obj_as_string` turns its answer
/// into a String, and escaped by [`str_escape`] when the result
/// encoding — `default_internal`, else `default_external` — cannot show
/// it: when that encoding is ASCII-incompatible and the text is not
/// ASCII, or when the text is in another encoding and is not ASCII.
/// Otherwise the very string `#inspect` answered comes back, encoding
/// and all (so a UTF-8 answer passes untouched under a UTF-8 locale,
/// broken bytes included).
///
/// Unlike [`inspect_embedded`], this dispatches `#inspect`, so a
/// user-defined one is what gets embedded.
pub(crate) fn rb_inspect(vm: &mut Executor, globals: &mut Globals, obj: Value) -> Result<Value> {
    let res = vm.invoke_method_inner(globals, IdentId::INSPECT, obj, &[], None, None)?;
    let res = rb_obj_as_string(vm, globals, res)?;
    let inner = res.as_rstring_inner();
    let enc = inner.encoding();
    let ascii_only = enc.is_ascii_compatible() && inner.as_bytes().is_ascii();
    let resenc = default_internal_or_external(globals).unwrap_or(Encoding::UTF8);
    let escape = !ascii_only && (!resenc.is_ascii_compatible() || enc != resenc);
    if !escape {
        return Ok(res);
    }
    Ok(Value::string_from_inner(RStringInner::from_encoding(
        str_escape(inner).as_bytes(),
        Encoding::UsAscii,
    )))
}

/// `rb_obj_as_string`: a String as it is, anything else through its
/// `#to_s` — and, when that is no String either, `rb_any_to_s`'s
/// `#<Class:0x…>`.
fn rb_obj_as_string(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Value> {
    if v.is_rstring_inner().is_some() {
        return Ok(v);
    }
    let s = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
    if s.is_rstring_inner().is_some() {
        return Ok(s);
    }
    let class_name = v.get_real_class_name(&globals.store);
    Ok(Value::string(format!(
        "#<{}:0x{:016x}>",
        class_name,
        v.id()
    )))
}

/// `default_internal`, else `default_external`: the encoding
/// `rb_inspect` measures a rendering against.
fn default_internal_or_external(globals: &mut Globals) -> Option<Encoding> {
    let resenc = globals
        .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
        .filter(|v| !v.is_nil())
        .or_else(|| {
            globals
                .get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
                .filter(|v| !v.is_nil())
        })?;
    globals.encoding_of_object(resenc)
}

/// `rb_str_escape`: `s` as ASCII text. A character that is printable
/// ASCII in an ASCII-compatible encoding stays; `\n`, `\t` and the
/// other C escapes are spelled so, and DEL is `\c?`; any other
/// character is `\uXXXX` /
/// `\u{XXXXX}` in a Unicode encoding (printable ASCII staying itself
/// there too) and `\xXX` / `\x{XXXX}` elsewhere; and a byte that
/// starts no character is `\xXX`. Unlike `String#inspect` it adds no
/// quotes and leaves `"`, `\` and `#` alone.
pub(crate) fn str_escape(s: &RStringInner) -> String {
    use crate::value::Encoding as E;
    let enc = s.encoding();
    let bytes = s.as_bytes();
    let unicode = matches!(
        enc,
        E::Utf8(_) | E::Utf16Le | E::Utf16Be | E::Utf32Le | E::Utf32Be
    ) || enc == E::NamedByte(crate::value::CESU_8);
    let asciicompat = enc.is_ascii_compatible();
    let printable = |c: u32| (0x20..0x7f).contains(&c);
    let mut out = String::new();
    let mut p = 0;
    while p < bytes.len() {
        let n = match crate::value::precise_mbclen(enc, bytes, p) {
            PreciseLen::Char(n) => n,
            _ => {
                out.push_str(&format!("\\x{:02X}", bytes[p]));
                p += 1;
                continue;
            }
        };
        let Some(c) = crate::value::enc_codepoint(enc, &bytes[p..p + n]) else {
            for b in &bytes[p..p + n] {
                out.push_str(&format!("\\x{:02X}", b));
            }
            p += n;
            continue;
        };
        p += n;
        let named = match c {
            0x0a => Some('n'),
            0x0d => Some('r'),
            0x09 => Some('t'),
            0x0c => Some('f'),
            0x0b => Some('v'),
            0x08 => Some('b'),
            0x07 => Some('a'),
            0x1b => Some('e'),
            _ => None,
        };
        if let Some(cc) = named {
            out.push('\\');
            out.push(cc);
        } else if c == 0x7f {
            out.push_str("\\c?");
        } else if asciicompat && printable(c) {
            out.push(c as u8 as char);
        } else if unicode {
            if c < 0x7f && printable(c) {
                out.push(c as u8 as char);
            } else if c < 0x10000 {
                out.push_str(&format!("\\u{:04X}", c));
            } else {
                out.push_str(&format!("\\u{{{:X}}}", c));
            }
        } else if c < 0x100 {
            out.push_str(&format!("\\x{:02X}", c));
        } else {
            out.push_str(&format!("\\x{{{:X}}}", c));
        }
    }
    out
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

/// The allocator for `Encoding::Converter` and its subclasses.
///
/// `Converter.new` builds the payload from a resolved pair, so this
/// only ever runs for a bare `.allocate`. The pair it hands back is
/// the one every reader fell back to while the state lived in
/// instance variables.
extern "C" fn converter_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_converter(
        class_id,
        ConverterInner::new(
            crate::value::Encoding::Ascii8,
            crate::value::Encoding::Ascii8,
        ),
    )
}

/// Build the `TranscodeOpts` for a Converter instance from its
/// stored replacement string + conversion flags. Shared by
/// `#convert` and `#primitive_convert` so both honour
/// `invalid:`/`undef: :replace` and a user-set `#replacement=`.
fn converter_transcode_opts(store: &Store, conv: &mut ConverterInner) -> TranscodeOpts {
    let mut opts = TranscodeOpts::default();
    if let Some((bytes, enc)) = conv.replacement()
        // Stored as the destination spells it (#1583); the pipeline
        // wants it as characters, and reads it back the same way it
        // reads the option.
        && let Some(s) = replacement_text_bytes(bytes, enc, store)
    {
        opts.replace = Some(s);
    }
    opts.invalid_replace = conv.invalid_replace();
    opts.undef_replace = conv.undef_replace();
    // The newline-decorator bits are kept in `flags` for `#convpath`;
    // wiring them into the transcode itself arrives with the
    // TranscodeOpts decorator support (PR #1018).
    opts
}

/// Read a conversion option Hash the way CRuby's `econv_opts` does, and
/// answer the `ECONV_*` bits it asks for — or the `ArgumentError` it
/// raises for a value it does not know. `String#encode`,
/// `Encoding::Converter.new` and `.search_convpath` all read it before
/// they look at either encoding, so a bad option is reported ahead of
/// a bad name.
///
/// `newline:` names one decorator and overrides the `*_newline:`
/// flags; without it, each of those flags that is true adds its
/// decorator. More than one of them is not refused here —
/// [`econv_conflict`] refuses it when a converter is opened, as
/// `rb_econv_open` does.
fn econv_opts(hash: Option<crate::value::Hashmap>) -> Result<i64> {
    let Some(hash) = hash else {
        return Ok(0);
    };
    let get = |key: &str| find_hash_value_for_symbol(&hash, key).filter(|v| !v.is_nil());
    let is_sym = |v: Value, name: &str| v.try_symbol().is_some_and(|s| s.get_name() == name);
    // `unexpected value for xml option: foo` names a Symbol it does
    // not know, and names nothing for any other value.
    let unexpected = |option: &str, v: Value| match v.try_symbol() {
        Some(sym) => MonorubyErr::argumenterr(format!(
            "unexpected value for {option} option: {}",
            sym.get_name()
        )),
        None => MonorubyErr::argumenterr(format!("unexpected value for {option} option")),
    };
    let mut flags = 0;
    if let Some(v) = get("invalid") {
        if !is_sym(v, "replace") {
            return Err(MonorubyErr::argumenterr(
                "unknown value for invalid character option",
            ));
        }
        flags |= ECONV_INVALID_REPLACE;
    }
    if let Some(v) = get("undef") {
        if !is_sym(v, "replace") {
            return Err(MonorubyErr::argumenterr(
                "unknown value for undefined character option",
            ));
        }
        flags |= ECONV_UNDEF_REPLACE;
    }
    if get("replace").is_some() && flags & ECONV_INVALID_REPLACE == 0 {
        flags |= ECONV_UNDEF_REPLACE;
    }
    if let Some(v) = get("xml") {
        if is_sym(v, "text") {
            flags |= ECONV_XML_TEXT;
        } else if is_sym(v, "attr") {
            flags |= ECONV_XML_ATTR_CONTENT | ECONV_XML_ATTR_QUOTE;
        } else {
            return Err(unexpected("xml", v));
        }
    }
    if let Some(v) = get("newline") {
        flags |= [
            ("universal", ECONV_UNIVERSAL_NEWLINE),
            ("crlf", ECONV_CRLF_NEWLINE),
            ("cr", ECONV_CR_NEWLINE),
            ("lf", ECONV_LF_NEWLINE),
        ]
        .into_iter()
        .find(|(name, _)| is_sym(v, name))
        .map(|(_, bit)| bit)
        .ok_or_else(|| unexpected("newline", v))?;
    } else {
        for (key, bit) in [
            ("universal_newline", ECONV_UNIVERSAL_NEWLINE),
            ("crlf_newline", ECONV_CRLF_NEWLINE),
            ("cr_newline", ECONV_CR_NEWLINE),
            ("lf_newline", ECONV_LF_NEWLINE),
        ] {
            if find_hash_value_for_symbol(&hash, key).is_some_and(|v| v.as_bool()) {
                flags |= bit;
            }
        }
    }
    Ok(flags)
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
    conv: &mut ConverterInner,
    src_enc: crate::value::Encoding,
    input: &[u8],
) -> crate::value::Encoding {
    if dummy_wide_target(src_enc).is_none() {
        return src_enc;
    }
    if let Some(enc) = conv.src_bom() {
        return enc;
    }
    if let Some((enc, _)) = dummy_wide_source(src_enc, input) {
        conv.set_src_bom(enc);
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

/// Whether this converter still owes its destination a BOM.
fn converter_dst_bom_owed(conv: &ConverterInner, dst_enc: crate::value::Encoding) -> bool {
    dummy_wide_target(dst_enc).is_some() && !conv.dst_bom_written()
}

/// The BOM to put in front of `out`, marking it written. A call that
/// emits nothing writes no BOM either, which is how CRuby answers an
/// empty chunk and a `#finish` that had nothing held.
fn converter_take_dst_bom(conv: &mut ConverterInner, dst_enc: crate::value::Encoding) -> Vec<u8> {
    if !converter_dst_bom_owed(conv, dst_enc) {
        return vec![];
    }
    conv.set_dst_bom_written();
    converter_dst_bom(dst_enc)
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
    // The flags come first, as in `econv_args`: an Integer mask as it
    // is, or an option Hash read by `econv_opts` — so a bad option is
    // reported ahead of a bad name, and the decorators they ask for are
    // named when a converter cannot be found: `code converter not found
    // (UTF-8 to Windows-1258 with crlf_newline)` (#1591).
    let ecflags = match lfp.try_arg(2).and_then(|v| v.try_fixnum()) {
        Some(n) => n,
        None => econv_opts(
            (2..=3)
                .filter_map(|i| lfp.try_arg(i))
                .find_map(|v| v.try_hash_ty()),
        )?,
    };
    let decorators = ecflags & ECONV_DECORATORS;
    // Resolve to canonical constant names *once* (so a `#to_str`
    // mock argument is converted exactly once, per spec).
    let (src, dst) = resolve_converter_pair(
        vm,
        globals,
        ConverterSrc::Given(lfp.arg(0)),
        lfp.arg(1),
        false,
        decorators,
    )?;
    // CRuby raises `Encoding::ConverterNotFoundError` for identical
    // source/destination encodings — there is no "X to X" transcoder,
    // and a decorator does not make one: `(UTF-8 to UTF-8 with
    // universal_newline)` is refused with the decorator named (#1589).
    // Compare the resolved canonical *names*, not the internal
    // `Encoding`: monoruby folds aliases like `UTF8-MAC` onto
    // `Utf8`, but CRuby treats them as distinct and DOES build a
    // converter (`Converter.new(UTF_8, UTF8_MAC)` is valid).
    if src.name() == dst.name() {
        return Err(converter_not_found_named(
            &globals.store,
            src.name(),
            dst.name(),
            decorators,
        ));
    }
    validate_converter_pair(src, dst, decorators, &globals.store)?;
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
    let mut replace_inner: Option<crate::value::RStringInner> = None;
    let mut flags = ConverterFlag::default();
    if let Some(n) = lfp.try_arg(2).and_then(|v| v.try_fixnum()) {
        if n & ECONV_INVALID_REPLACE != 0 {
            flags.set_invalid_replace();
        }
        if n & ECONV_UNDEF_REPLACE != 0 {
            flags.set_undef_replace();
        }
    }
    // The decorator bits pass through verbatim, whichever way they
    // were asked for.
    flags.set_raw_flag(decorators);
    if let Some(hash) = opts_hash {
        {
            if let Some(v) = find_hash_value_for_symbol(&hash, "invalid")
                && v.try_symbol().map(|s| s.get_name() == "replace") == Some(true)
            {
                flags.set_invalid_replace();
            }
            if let Some(v) = find_hash_value_for_symbol(&hash, "undef")
                && v.try_symbol().map(|s| s.get_name() == "replace") == Some(true)
            {
                flags.set_undef_replace();
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
                    let decorators = with_newline_flags(
                        TranscodeOpts {
                            replace: Some(s.clone()),
                            ..Default::default()
                        },
                        flags.0,
                    );
                    // The check converts the replacement into the
                    // destination, which is also what `#replacement`
                    // hands back — so the bytes are kept rather than
                    // tagging the UTF-8 ones with the destination's
                    // name (#1583).
                    let in_dst = validate_replacement(&decorators, src, dst, None, &globals.store)?
                        .unwrap_or_default();
                    replace_inner = Some(replacement_in(&s, &in_dst, dst, &globals.store));
                }
            }
        }
    }
    let class = lfp.self_val();
    // The payload is built whole and handed to the allocation, so
    // there is no window in which a half-filled converter is visible
    // — and nothing allocates after it.

    let mut conv = ConverterInner::new(src, dst);
    conv.set_flags(flags);
    if let Some(inner) = replace_inner {
        conv.set_replacement(inner.as_bytes().to_vec(), inner.encoding());
    }
    Ok(Value::new_converter(class.as_class_id(), conv))
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
    let conv = Converter::new(lfp.self_val());
    Ok(encoding_value(globals, conv.src()))
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
    let conv = Converter::new(lfp.self_val());
    Ok(encoding_value(globals, conv.dst()))
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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let conv = Converter::new(lfp.self_val());
    if let Some((bytes, enc)) = conv.replacement() {
        return Ok(Value::string_from_inner(
            crate::value::RStringInner::from_encoding_scanned(bytes, enc),
        ));
    }
    Ok(converter_default_replacement(insert_encoding(
        conv.src(),
        conv.dst(),
    )))
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
    let mut conv = Converter::new(lfp.self_val());
    let dst = conv.dst();
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
    let inner = replacement_in(s, &in_dst, dst, &globals.store);
    conv.set_replacement(inner.as_bytes().to_vec(), inner.encoding());
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut conv = Converter::new(lfp.self_val());
    // The argument is converted before the converter's state is
    // looked at, as CRuby's `StringValue` is reached first: a
    // finished converter handed a non-String still answers the
    // `TypeError` (#1537).
    let arg = lfp.arg(0);
    let bytes = arg
        .is_rstring_inner()
        .ok_or_else(|| MonorubyErr::no_implicit_conversion(&globals.store, arg, STRING_CLASS))?
        .as_bytes()
        .to_vec();
    if conv.finished() {
        return Err(MonorubyErr::argumenterr(
            "converter already finished".to_string(),
        ));
    }
    let src = conv.src();
    let dst = conv.dst();
    // Honour the configured replacement + `invalid:`/`undef:
    // :replace` flags (a user-set `#replacement=` too).
    let opts = converter_transcode_opts(&globals.store, &mut conv);
    // Some pairs carry state across calls, which a single-shot
    // transcode has no way to keep: the endianness-less dummies carry
    // a BOM, read once on the source side and written once on the
    // destination side, and a `UTF8-MAC` source holds its trailing
    // cluster back. Those go the streamed way whatever the flags say
    // (#1576).
    let stateful = converter_is_stateful(src, dst);
    if (opts.invalid_replace || opts.undef_replace) && !stateful {
        // Replacement mode cannot error on content — the single-shot
        // transcoder suffices — but a chunk that ends inside a
        // character is still held for the next one to finish, as it
        // is on every other path (#1592).
        let mut input: Vec<u8> = conv.pending().to_vec();
        input.extend(conv.take_readagain());
        input.extend_from_slice(&bytes);
        let tail = incomplete_tail_len(src, &input);
        let head = &input[..input.len() - tail];
        let out = transcode_bytes_with_opts(head, src, dst, &opts, &globals.store)?;
        conv.set_pending(input[input.len() - tail..].to_vec());
        let meta = ErrMeta::default();
        store_conversion_outcome(
            &mut conv,
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
    // The bytes an error left the decoder holding come before its
    // read-again bytes (#1617); on every other path one of the two
    // is empty.
    let mut input: Vec<u8> = conv.pending().to_vec();
    input.extend(conv.take_readagain());
    input.extend_from_slice(&bytes);
    // A dummy `UTF-16` / `UTF-32` source is read in whatever
    // endianness its BOM named; until one has arrived whole there is
    // nothing to read the chunk as (#1576).
    // The encodings the conversion runs in. They differ from the
    // converter's own only for the dummies, which stay the names every
    // error message uses — CRuby reports "from UTF-16", not from
    // whichever end its BOM turned out to name.
    let src_stream = converter_resolve_src_bom(&mut conv, src, &input);
    // The destination's: a dummy writes its BOM here, once, and then
    // the big-endian form CRuby writes.
    let dst_stream = dummy_wide_target(dst).unwrap_or(dst);
    let mut opts = opts;
    opts.iso_state = conv.iso_state();
    let (result, consumed, out, meta) = stream_convert(
        &input,
        src_stream,
        dst_stream,
        None,
        true,
        &opts,
        &globals.store,
    );
    conv.set_iso_state(meta.iso_state_out);
    let out = if out.is_empty() {
        out
    } else {
        let mut bom = converter_take_dst_bom(&mut conv, dst);
        bom.extend_from_slice(&out);
        bom
    };
    match result {
        StreamConvertResult::Finished
        | StreamConvertResult::SourceBufferEmpty
        | StreamConvertResult::DestinationBufferFull => {
            conv.set_pending(input[consumed..].to_vec());
            store_conversion_outcome(
                &mut conv,
                StreamConvertResult::SourceBufferEmpty,
                &ErrMeta::default(),
                src,
                dst,
            );
        }
        _ => {
            // What the decoder was holding when it stopped is still
            // its: the next call reads it first (#1617).
            conv.set_pending(meta.hold_src.clone());
            let msg =
                store_conversion_outcome(&mut conv, result, &meta, src, dst).unwrap_or_default();
            return Err(converter_last_error_raise(vm, globals, conv, result, msg));
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut conv = Converter::new(lfp.self_val());
    conv.set_finished();
    let src_enc = conv.src();
    let dst = conv.dst();
    // Input buffered by a partial `#convert` that never completed is an
    // incomplete-input error at finish time (CRuby).
    // What an error left the decoder holding is read ahead of its
    // read-again bytes (#1617).
    let mut pending: Vec<u8> = conv.pending().to_vec();
    pending.extend(conv.take_readagain());
    let src_stream = converter_resolve_src_bom(&mut conv, src_enc, &pending);
    let dst_stream = dummy_wide_target(dst).unwrap_or(dst);
    let mut out: Vec<u8> = vec![];
    if !pending.is_empty() {
        conv.set_pending(vec![]);
        // The end of the input settles what a chunk could not: a
        // cluster a `UTF8-MAC` source was holding in case a mark
        // followed it now converts on its own — and can turn out to
        // have no cell in the destination, which is that error and not
        // an incomplete one. Only a tail that is still half a
        // character is incomplete input.
        let mut opts = converter_transcode_opts(&globals.store, &mut conv);
        opts.iso_state = conv.iso_state();
        let (result, consumed, flushed, meta) = if opts.invalid_replace
            && !converter_is_stateful(src_stream, dst_stream)
        {
            // In replacement mode what is still half a character is
            // replaced, not reported (#1592).
            let out =
                transcode_bytes_with_opts(&pending, src_stream, dst_stream, &opts, &globals.store)?;
            (
                StreamConvertResult::Finished,
                pending.len(),
                out,
                ErrMeta::default(),
            )
        } else {
            stream_convert(
                &pending,
                src_stream,
                dst_stream,
                None,
                false,
                &opts,
                &globals.store,
            )
        };
        conv.set_iso_state(meta.iso_state_out);
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
                _ if !meta.error_bytes.is_empty() => (StreamConvertResult::IncompleteInput, meta),
                _ => (
                    StreamConvertResult::IncompleteInput,
                    ErrMeta {
                        error_bytes: pending,
                        readagain_bytes: vec![],
                        ..ErrMeta::default()
                    },
                ),
            };
            let msg =
                store_conversion_outcome(&mut conv, kind, &meta, src_enc, dst).unwrap_or_default();
            return Err(converter_last_error_raise(vm, globals, conv, kind, msg));
        }
    }
    // The end of the input is where ISO-2022-JP designates ASCII
    // again, and CRuby writes that escape from `#finish` whether or
    // not anything was held back (#1609).
    if jis_wrapper(dst).is_some() && conv.iso_state().is_some() {
        out.extend_from_slice(b"\x1b(B");
        conv.set_iso_state(None);
    }
    if !out.is_empty() {
        let mut bom = converter_take_dst_bom(&mut conv, dst);
        bom.extend_from_slice(&out);
        out = bom;
    }
    // The end of the stream is an outcome of its own: `primitive_errinfo`
    // answers `:finished` after it, not the error a previous call
    // reported and already consumed.
    store_conversion_outcome(
        &mut conv,
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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let conv = Converter::new(lfp.self_val());
    let src = conv.src();
    let dst = conv.dst();
    Ok(Value::string_sprintf(format!(
        "#<Encoding::Converter: {} to {}>",
        src.name(),
        dst.name()
    )))
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
                    format!(
                        "{bytes} to UTF-8 in conversion from {} to UTF-8",
                        src_enc.name()
                    )
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
                format!(
                    "U+{:04X} to {stage_dst} in conversion from {from} to {stage_dst}",
                    cp
                )
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

/// Record the outcome of a conversion step on the converter object:
/// the `primitive_errinfo` tuple, the read-again buffer, and the
/// structured last-error data. Returns the error message when the
/// outcome was an error (for the raising callers).
fn store_conversion_outcome(
    recv: &mut ConverterInner,
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
    let error = is_error.then(|| {
        let decode_stage =
            !matches!(result, StreamConvertResult::UndefinedConversion) || meta.decode_stage;
        let (stage_src, stage_dst) = meta
            .stage
            .clone()
            .unwrap_or_else(|| error_stage_names(src_enc, dst_enc, decode_stage));
        ConverterError {
            message: conversion_error_message(result, meta, src_enc, dst_enc),
            stage_src,
            stage_dst,
            error_bytes: meta.error_bytes.clone(),
            readagain_bytes: meta.readagain_bytes.clone(),
        }
    });
    let msg = error.as_ref().map(|e| e.message.clone());
    // Read-again buffer, for `#putback`.
    recv.set_readagain(
        if matches!(result, StreamConvertResult::InvalidByteSequence) {
            meta.readagain_bytes.clone()
        } else {
            vec![]
        },
    );
    recv.set_outcome(ConverterOutcome { result, error });
    msg
}

/// Materialise the stored last-error data into a fresh exception
/// object with the attribute ivars set.
fn build_last_error_object(vm: &mut Executor, globals: &mut Globals, recv: Converter) -> Value {
    let Some(err) = recv
        .outcome()
        .and_then(|o| o.error.as_ref().map(|e| (o.result, e.clone())))
    else {
        return Value::nil();
    };
    let (kind, err) = err;
    let msg = err.message;
    let class_name = if matches!(kind, StreamConvertResult::UndefinedConversion) {
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
    // Every attribute below is a freshly built String, so the object
    // has to be rooted across them: it is reachable from nothing else
    // until it is returned.
    let temp = vm.temp_len();
    vm.temp_push(obj);
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id("@error_bytes"),
        binary_string(&err.error_bytes),
    );
    // CRuby reports "no read-again bytes" as nil, not as an empty
    // String (the `primitive_errinfo` tuple still says "").
    let readagain = if err.readagain_bytes.is_empty() {
        Value::nil()
    } else {
        binary_string(&err.readagain_bytes)
    };
    let _ = globals
        .store
        .set_ivar(obj, IdentId::get_id("@readagain_bytes"), readagain);
    let incomplete = matches!(kind, StreamConvertResult::IncompleteInput);
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id("@incomplete_input"),
        Value::bool(incomplete),
    );
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id("@source_encoding_name"),
        Value::string(err.stage_src),
    );
    let _ = globals.store.set_ivar(
        obj,
        IdentId::get_id("@destination_encoding_name"),
        Value::string(err.stage_dst),
    );
    // `UndefinedConversionError#error_char`: the offending character,
    // whose bytes `stream_convert` stored UTF-8-encoded (the stage
    // source encoding for every path that can reach an undef error).
    if matches!(kind, StreamConvertResult::UndefinedConversion)
        && let Ok(text) = String::from_utf8(err.error_bytes)
    {
        let _ = globals
            .store
            .set_ivar(obj, IdentId::get_id("@error_char"), Value::string(text));
    }
    vm.temp_clear(temp);
    obj
}

/// Raise the converter's stored last error as a real exception object,
/// so `#error_bytes`, `#readagain_bytes`, `#incomplete_input?` and the
/// encoding accessors are populated — CRuby raises the very object
/// `#last_error` hands back afterwards. Falls back to a plain
/// message-only error if the last-error slot could not be materialised.
fn converter_last_error_raise(
    vm: &mut Executor,
    globals: &mut Globals,
    conv: Converter,
    result: StreamConvertResult,
    msg: String,
) -> MonorubyErr {
    let obj = build_last_error_object(vm, globals, conv);
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
    let mut conv = Converter::new(lfp.self_val());
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
    let already_finished = conv.finished();
    // The read-again bytes of the last `:invalid_byte_sequence` come
    // first — CRuby keeps them at the head of its input buffer, so
    // `"\xf1abcd"` into ISO-8859-1 stops at `\xF1` with `"a"` read
    // again, and the next call writes `"abcd"` — then the bytes a
    // dst-bytesize cap held back last call, then the new source, so
    // multi-call streaming with `dst_bytesize` works (the spec test
    // "uses the destination byte offset" hits this path).
    // Ahead of them both is what the decoder was holding when the
    // error stopped it — a `UTF8-MAC` cluster read before the run,
    // buffered without being written (#1617). Only an error leaves
    // read-again bytes, and only an error buffers such a hold, so
    // the two never meet on any other path.
    let mut src_bytes: Vec<u8> = if already_finished {
        vec![]
    } else {
        conv.pending().to_vec()
    };
    if !already_finished {
        src_bytes.extend(conv.take_readagain());
    }
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

    let src_enc = conv.src();
    let dst_enc = conv.dst();
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
    let src_stream = converter_resolve_src_bom(&mut conv, src_enc, &src_bytes);
    let dst_stream = dummy_wide_target(dst_enc).unwrap_or(dst_enc);

    // Output the last call's cap held back mid-character goes out
    // first, before anything new is converted, and counts against
    // this call's cap (#1532). If it fills the destination on its
    // own the call stops there having read nothing — and the stream
    // is not over, so the end-of-input check below is skipped with
    // it.
    let pending_out: Vec<u8> = conv.pending_out().to_vec();
    let (held_out, still_held) = match max_dst_bytes {
        Some(max) if pending_out.len() > max => {
            (pending_out[..max].to_vec(), pending_out[max..].to_vec())
        }
        _ => (pending_out, vec![]),
    };
    if !still_held.is_empty() {
        conv.set_pending_out(still_held);
        let mut new_dst_bytes = dst_initial[..dst_offset].to_vec();
        new_dst_bytes.extend_from_slice(&held_out);
        let new_dst = crate::value::RStringInner::from_encoding_scanned(&new_dst_bytes, dst_enc);
        dst_arg.replace_with_inner(new_dst);
        let result = StreamConvertResult::DestinationBufferFull;
        store_conversion_outcome(&mut conv, result, &ErrMeta::default(), src_enc, dst_enc);
        return Ok(Value::symbol_from_str(result.symbol_name()));
    }
    conv.set_pending_out(vec![]);
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

    let mut conv_opts = converter_transcode_opts(&globals.store, &mut conv);
    conv_opts.iso_state = conv.iso_state();
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
        let bom_owed = if converter_dst_bom_owed(&mut conv, dst_enc) {
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
        conv.set_iso_state(meta.iso_state_out);
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
    let emits =
        !out_bytes.is_empty() || matches!(result, StreamConvertResult::DestinationBufferFull);
    let out_bytes = if !emits {
        out_bytes
    } else {
        let mut with_bom = converter_take_dst_bom(&mut conv, dst_enc);
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
            conv.set_finished();
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
            conv.set_pending_out(held);
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
        // What the decoder read and still holds goes first: it was
        // taken out of `src` before the error and is read again
        // ahead of the read-again bytes (#1617).
        let mut buffered: Vec<u8> = meta.hold_src.clone();
        buffered.extend_from_slice(&src_bytes[buffered_from..split]);
        // Put bytes after the error back into `src_arg`. Pending
        // buffer otherwise drops — the converter has nothing it
        // could write next call without more user input.
        let leftover: Vec<u8> = src_bytes[split..].to_vec();
        if !src_arg.is_nil() {
            let new_src = crate::value::RStringInner::from_encoding_scanned(&leftover, src_tag);
            src_arg.replace_with_inner(new_src);
        }
        conv.set_pending(buffered);
    } else {
        // Clear src and stash the unconverted tail (if any) in
        // pending for the next call. An `:incomplete_input` answer
        // has *reported* that tail — it is the error's bytes, and
        // CRuby drops them from its buffer with the report — so
        // nothing is held: the next `#convert` starts afresh rather
        // than gluing them onto its argument.
        let mut pending_after: Vec<u8> = meta.hold_src.clone();
        if !matches!(result, StreamConvertResult::IncompleteInput) {
            pending_after.extend_from_slice(&src_bytes[src_consumed..]);
        }
        if !src_arg.is_nil() {
            let cleared = crate::value::RStringInner::from_encoding_scanned(b"", src_tag);
            src_arg.replace_with_inner(cleared);
        }
        conv.set_pending(pending_after);
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
    store_conversion_outcome(&mut conv, result, &meta, src_enc, dst_enc);

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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let recv = lfp.self_val();
    // No `primitive_convert` has run yet — CRuby's "nothing
    // pending" form is `[:source_buffer_empty, nil, nil, nil, nil]`.
    let Some(outcome) = recv.as_converter_inner().outcome() else {
        return Ok(Value::array_from_iter(
            [
                Value::symbol_from_str("source_buffer_empty"),
                Value::nil(),
                Value::nil(),
                Value::nil(),
                Value::nil(),
            ]
            .into_iter(),
        ));
    };
    let result = Value::symbol_from_str(outcome.result.symbol_name());
    let tuple = match &outcome.error {
        Some(e) => [
            result,
            Value::string(e.stage_src.clone()),
            Value::string(e.stage_dst.clone()),
            binary_string(&e.error_bytes),
            binary_string(&e.readagain_bytes),
        ],
        None => [
            result,
            Value::nil(),
            Value::nil(),
            Value::nil(),
            Value::nil(),
        ],
    };
    Ok(Value::array_from_iter(tuple.into_iter()))
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(build_last_error_object(
        vm,
        globals,
        Converter::new(lfp.self_val()),
    ))
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
    let mut conv = Converter::new(lfp.self_val());
    let buffered: Vec<u8> = conv.readagain().to_vec();
    let take = match lfp.try_arg(0) {
        Some(v) if !v.is_nil() => {
            (v.coerce_to_int_i64(vm, globals)?.max(0) as usize).min(buffered.len())
        }
        _ => buffered.len(),
    };
    let (out, rest) = buffered.split_at(take);
    conv.set_readagain(rest.to_vec());
    let src_enc = conv.src();
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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let lhs = Converter::new(lfp.self_val());
    let rhs = Converter::new(lfp.arg(0));
    if rhs.as_val().class() != lhs.as_val().class() {
        return Ok(Value::bool(false));
    }
    let same = lhs.src() == rhs.src() && lhs.dst() == rhs.dst();
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
    // The optional third argument / kwargs carry decorator options
    // (the kwargs hash may land in either trailing slot); they are
    // read before the encodings, as in `econv_args`.
    let ecflags = econv_opts(
        (2..=3)
            .filter_map(|i| lfp.try_arg(i))
            .find_map(|v| v.try_hash_ty()),
    )?;
    let decorators = ecflags & ECONV_DECORATORS;
    let (src, dst) = resolve_converter_pair(
        vm,
        globals,
        ConverterSrc::Given(lfp.arg(0)),
        lfp.arg(1),
        false,
        decorators,
    )?;
    validate_converter_pair(src, dst, decorators, &globals.store)?;
    let crlf = ecflags & ECONV_CRLF_NEWLINE != 0;
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
    let conv = recv.as_converter_inner();
    let (src, dst) = (conv.src(), conv.dst());
    let crlf = conv.flags().0 & 0x1000 != 0;
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
    // `U+AC00 to EUC-JP in conversion from CP949 to UTF-8 to EUC-JP to
    // stateless-ISO-2022-JP`: the hop that gave up is the one whose
    // destination the message names, and its source is the hop before
    // it on the chain — `["UTF-8", "EUC-JP"]`, not the chain's first
    // pair (#1618).
    if let Some((head, chain)) = msg.split_once(" in conversion from ")
        && let Some((_, hop_dst)) = head.rsplit_once(" to ")
    {
        let hops: Vec<&str> = chain.split(" to ").map(str::trim).collect();
        if let Some(i) = hops.iter().skip(1).position(|h| *h == hop_dst.trim()) {
            return Some((hops[i].to_string(), hop_dst.trim().to_string()));
        }
    }
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
        .get_ivar(
            lfp.self_val(),
            IdentId::get_id("@destination_encoding_name"),
        )
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
    Ok(Value::array_from_vec(
        out.into_iter().map(|(_, v)| v).collect(),
    ))
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
        return Err(MonorubyErr::argumenterr("invalid encoding name (NUL byte)"));
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
    let gvar = |n: &str| globals.get_gvar(IdentId::get_id(n)).filter(|v| !v.is_nil());
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
    if let Some(v) = enc_name_to_const(&canonical).and_then(|c| {
        globals
            .store
            .get_constant_noautoload(enc_class, IdentId::get_id(c))
    }) && is_encoding_object(globals, v)
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
fn compute_encoding_compatibility(globals: &Globals, a: Value, b: Value) -> Option<Encoding> {
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
    matches!(enc, Encoding::Iso2022Jp) || is_cruby_dummy_name(enc.name())
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
        return Ok(Value::string_usascii_from_str(
            "#<Encoding:BINARY (ASCII-8BIT)>",
        ));
    }
    let suffix = if is_cruby_dummy_name(&name) {
        " (dummy)"
    } else {
        ""
    };
    Ok(Value::string_usascii(format!("#<Encoding:{name}{suffix}>")))
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
            "0x80",                   // leads nothing
            "0x81, 0xA0",             // 2 bytes
            "0x81, 0x20",             // …with a second byte that is not one
            "0x81",                   // …truncated
            "0x90, 0xA0, 0xA0",       // 3 bytes
            "0x90, 0xA0",             // …truncated: one subpart, not two
            "0x90, 0xA0, 0x20, 0xA0", // …and the 0x20 survives
            "0x9A, 0xE0, 0xA0",       // a private charset
            "0x9A, 0xA0, 0xA0",       // …whose id is out of range
            "0x9C, 0xF0, 0xA0, 0xA0", // 4 bytes
            "0x9C, 0xFF, 0xA0, 0xA0",
            "0x9D, 0xF5, 0xA0, 0xA0",
            "0x9D, 0xF0, 0xA0, 0xA0", // 0x9C's range, not 0x9D's
            "0x9E, 0xA0",             // leads nothing either
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
