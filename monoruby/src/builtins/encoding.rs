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
    ] {
        let val = Value::object(enc.id());
        globals
            .store
            .set_ivar(
                val,
                IdentId::_NAME,
                Value::string_from_str(&format!("#<Encoding:{}>", name.replace('_', "-"))),
            )
            .unwrap();
        globals
            .store
            .set_ivar(
                val,
                IdentId::_ENCODING,
                Value::string_from_str(&name.replace('_', "-")),
            )
            .unwrap();
        globals.set_constant_by_str(enc.id(), name, val);
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
    if let Some(utf8) = globals.store.get_constant_noautoload(enc.id(), IdentId::get_id("UTF_8")) {
        globals.set_constant_by_str(enc.id(), "CP65001", utf8);
    }

    // Encoding::CompatibilityError < EncodingError < StandardError
    let enc_error_val = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("EncodingError"))
        .unwrap();
    let enc_error_module = enc_error_val.expect_class(globals).unwrap();
    let compat_error = globals.define_class("CompatibilityError", enc_error_module, OBJECT_CLASS);
    globals.set_constant_by_str(enc.id(), "CompatibilityError", compat_error.get());

    // Encoding class methods
    globals.define_builtin_class_func(enc.id(), "default_external", enc_default_external, 0);
    globals.define_builtin_class_func(enc.id(), "default_external=", enc_set_default_external, 1);
    globals.define_builtin_class_func(enc.id(), "default_internal=", enc_set_default_internal, 1);
    globals.define_builtin_class_func(enc.id(), "list", enc_list, 0);
    globals.define_builtin_class_func(enc.id(), "find", enc_find, 1);
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
    enc_name_to_const(&name)
        .ok_or_else(|| MonorubyErr::argumenterr(format!("unknown encoding name - {}", name)))
}

///
/// ### String#encode
///
/// - encode(encoding, **opts) -> String
/// - encode(dst_encoding, src_encoding, **opts) -> String
/// - encode(**opts) -> String
///
/// Stub: updates the encoding tag of the result but does not transcode bytes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encode.html]
#[monoruby_builtin]
pub(super) fn encode(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // Validate encoding arguments and resolve the destination encoding.
    let dst = if let Some(arg0) = lfp.try_arg(0) {
        Some(resolve_enc_arg(vm, globals, arg0)?)
    } else {
        None
    };
    if let Some(arg1) = lfp.try_arg(1) {
        resolve_enc_arg(vm, globals, arg1)?;
    }
    let mut result = lfp.self_val().dup();
    if let Some(dst) = dst {
        let enc = Encoding::try_from_str(dst)?;
        result.as_rstring_inner_mut().set_encoding(enc);
    }
    Ok(result)
}

///
/// ### String#encode!
///
/// - encode!(encoding, **opts) -> self
/// - encode!(dst_encoding, src_encoding, **opts) -> self
/// - encode!(**opts) -> self
///
/// Stub: updates the encoding tag of self but does not transcode bytes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encode=21.html]
#[monoruby_builtin]
pub(super) fn encode_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_string_mutable(vm, globals)?;
    let dst = if let Some(arg0) = lfp.try_arg(0) {
        Some(resolve_enc_arg(vm, globals, arg0)?)
    } else {
        None
    };
    if let Some(arg1) = lfp.try_arg(1) {
        resolve_enc_arg(vm, globals, arg1)?;
    }
    if let Some(dst) = dst {
        let enc = Encoding::try_from_str(dst)?;
        lfp.self_val().as_rstring_inner_mut().set_encoding(enc);
    }
    Ok(lfp.self_val())
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
        .get_constant_checked(globals, OBJECT_CLASS, IdentId::get_id("Encoding"))?
        .expect_class(globals)?
        .id();
    let const_name = encoding_constant_name(enc);
    let res = vm.get_constant_checked(globals, enc_class, IdentId::get_id(const_name))?;
    Ok(res)
}

/// Map an `Encoding` to the corresponding `Encoding::<NAME>` Ruby
/// constant name registered by `init_encoding`.
fn encoding_constant_name(enc: Encoding) -> &'static str {
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
    Ok(Value::bool(lfp.self_val().as_rstring_inner().is_ascii()))
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
        .get_constant_noautoload(enc_class, IdentId::get_id("UTF_8"))
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
        .get_constant_noautoload(enc_class, IdentId::get_id("UTF_8"))
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
        "EUCJP_MS" | "EUCJP_WIN" => Some("EUCJP_MS"),
        "CP51932" => Some("CP51932"),
        "STATELESS_ISO_2022_JP" => Some("STATELESS_ISO_2022_JP"),

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
    // Resolve each argument to an (Encoding, CodeRange) pair. String
    // arguments use their actual byte content for code-range
    // classification; Encoding objects (or non-strings) report
    // SevenBit as a placeholder since they have no bytes —
    // matching CRuby's "two Encoding objects compatible iff equal".
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
    match Encoding::compatible(a_enc, a_cr, b_enc, b_cr) {
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
