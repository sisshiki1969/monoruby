use super::*;

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Regexp", REGEXP_CLASS, ObjTy::REGEXP);
    globals.define_builtin_class_funcs_with_kw(
        REGEXP_CLASS,
        "new",
        &["compile"],
        regexp_new,
        1,
        2,
        false,
        &[TIMEOUT_KW],
        false,
    );
    globals.define_builtin_class_funcs(REGEXP_CLASS, "escape", &["quote"], regexp_escape, 1);
    globals.define_builtin_class_func_rest(REGEXP_CLASS, "union", regexp_union);
    globals.define_builtin_class_func_with(
        REGEXP_CLASS,
        "last_match",
        regexp_last_match,
        0,
        1,
        false,
    );
    globals.define_builtin_class_func(REGEXP_CLASS, "try_convert", regexp_try_convert, 1);
    globals.define_builtin_class_func_with(
        REGEXP_CLASS,
        "linear_time?",
        regexp_linear_time_p,
        1,
        2,
        false,
    );
    // Regexp::TimeoutError < RegexpError — raised when a match runs past
    // `Regexp.timeout` / `Regexp.new(timeout:)`. Registered here rather
    // than with the other exception classes because `exception::init`
    // runs before `Regexp` itself exists, and this one is named under
    // it (#1423).
    let regexp_error = globals.store.get_module(REGEX_ERROR_CLASS);
    let timeout_error = globals.store.define_builtin_class(
        "TimeoutError",
        REGEX_TIMEOUT_ERROR_CLASS,
        Some(regexp_error),
        REGEXP_CLASS,
        ObjTy::EXCEPTION,
    );
    globals.store.get_metaclass(REGEX_TIMEOUT_ERROR_CLASS);
    // Naming it under Regexp sets the class's *name*; the constant that
    // `Regexp::TimeoutError` resolves has to be set as well.
    globals.set_constant_by_str(REGEXP_CLASS, "TimeoutError", timeout_error.get());
    globals.define_builtin_class_func(REGEXP_CLASS, "timeout", regexp_timeout_get, 0);
    globals.define_builtin_class_func(REGEXP_CLASS, "timeout=", regexp_timeout_set, 1);
    globals.define_builtin_func(REGEXP_CLASS, "=~", regexp_match, 1);
    globals.define_builtin_func(REGEXP_CLASS, "~", regexp_tilde, 0);
    globals.define_builtin_func(REGEXP_CLASS, "===", teq, 1);
    globals.define_builtin_funcs(REGEXP_CLASS, "==", &["eql?"], regexp_eq, 1);
    globals.define_builtin_func(REGEXP_CLASS, "hash", regexp_hash, 0);
    globals.define_builtin_func(REGEXP_CLASS, "source", source, 0);
    globals.define_builtin_func(REGEXP_CLASS, "options", options, 0);
    globals.define_builtin_func(REGEXP_CLASS, "casefold?", casefold_p, 0);
    globals.define_builtin_func(REGEXP_CLASS, "encoding", encoding, 0);
    globals.define_builtin_func(REGEXP_CLASS, "fixed_encoding?", fixed_encoding_p, 0);
    globals.define_builtin_func(REGEXP_CLASS, "named_captures", named_captures, 0);
    globals.define_builtin_func_with(REGEXP_CLASS, "match?", match_, 1, 2, false);
    globals.define_builtin_func_with(REGEXP_CLASS, "match", rmatch, 1, 2, false);
    globals.define_builtin_func(REGEXP_CLASS, "names", names, 0);
    globals.define_builtin_func(REGEXP_CLASS, "timeout", regexp_inst_timeout, 0);
    // `Regexp#initialize` is a private method that always raises:
    // - `FrozenError` if the receiver is frozen (literals are frozen);
    // - `TypeError` otherwise (CRuby treats every monoruby Regexp as
    //   "already initialized" since `Regexp.new` is the sole entry
    //   point and produces a fully-built instance).
    let init_id = globals.define_private_builtin_func_with_kw(
        REGEXP_CLASS,
        "initialize",
        regexp_initialize,
        1,
        2,
        false,
        &[TIMEOUT_KW],
        false,
    );
    let _ = init_id;
    globals.store[REGEXP_CLASS].set_alloc_func(regexp_alloc_func);
}

///
/// ### Regexp#names
///
/// - names -> [String]
///
/// Returns the names of named captures declared in the pattern.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/names.html]
#[monoruby_builtin]
fn names(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let re = self_.as_regexp_inner();
    let raw = re.capture_names().unwrap_or_default();
    let mut unique: Vec<String> = Vec::with_capacity(raw.len());
    for n in raw {
        if !unique.iter().any(|u| *u == n) {
            unique.push(n);
        }
    }
    // A name is spelled in the pattern's encoding: in a UTF-16 regexp
    // it is the UTF-16 bytes, which CRuby hands back as they are.
    let enc = re.declared_encoding();
    Ok(Value::array_from_iter(unique.iter().map(|n| {
        if enc.is_wide() {
            Value::string_from_inner(RStringInner::from_encoding(n.as_bytes(), enc))
        } else {
            Value::string_from_str(n)
        }
    })))
}

// Class methods

/// Allocator for `Regexp` and its subclasses. An empty pattern with no
/// options cannot fail to compile, so unwrap is safe. The result is
/// flagged "uninitialized" so that `#match`/`#=~`/`#match?` raise
/// `TypeError` on the bare `Regexp.allocate` form (matches CRuby).
pub(crate) extern "C" fn regexp_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    let mut regexp = RegexpInner::with_option("", 0).expect("empty regexp compile cannot fail");
    regexp.mark_uninitialized();
    Value::regexp_with_class(regexp, class_id)
}

/// Private `Regexp#initialize`. On a freshly-`allocate`d (uninitialized)
/// receiver — the path taken by `SubclassOfRegexp.new` and an explicit
/// `super` from an overridden `#initialize` — it compiles the source and
/// fills in the instance. Re-initialising an already-built regexp is
/// rejected: `FrozenError` for a frozen literal, otherwise `TypeError`
/// "already initialized regexp" (matches CRuby).
#[monoruby_builtin]
fn regexp_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    if self_.is_frozen() {
        return Err(MonorubyErr::frozenerr("can't modify frozen Regexp"));
    }
    if self_.as_regexp_inner().initialized() {
        return Err(MonorubyErr::typeerr("already initialized regexp"));
    }
    // Root the receiver and the source/option args across the allocating
    // build below: this runs from `super` in an overridden `#initialize`,
    // whose frame slots aren't otherwise reachable by the GC here.
    let temp = vm.temp_len();
    vm.temp_push(self_);
    vm.temp_push(lfp.arg(0));
    if let Some(a1) = lfp.try_arg(1) {
        vm.temp_push(a1);
    }
    // The `timeout:` keyword is read (and rejected) before the source
    // is compiled, as CRuby does.
    let nanos = timeout_arg(vm, globals, lfp.try_arg(2).unwrap_or_default())?;
    let inner = build_regexp_inner(vm, globals, lfp)?;
    vm.flush_compile_warnings(globals);
    *self_.as_regexp_inner_mut() = inner;
    store_regexp_timeout(globals, self_, nanos)?;
    vm.temp_clear(temp);
    Ok(Value::nil())
}

///
/// ### Regexp.new
/// - new(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
/// - compile(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    // The `timeout:` keyword is read (and rejected) before the source is
    // compiled, as CRuby does.
    let nanos = timeout_arg(vm, globals, lfp.try_arg(2).unwrap_or_default())?;
    if class_id == REGEXP_CLASS {
        // Fast path for the base class: build directly.
        let inner = build_regexp_inner(vm, globals, lfp)?;
        // Surface Onigmo compile-time diagnostics right away, like
        // CRuby's rb_warn during Regexp.new.
        vm.flush_compile_warnings(globals);
        let re = Value::regexp(inner);
        store_regexp_timeout(globals, re, nanos)?;
        return Ok(re);
    }
    // A subclass: allocate an instance of it, then initialize. Root the
    // fresh instance across the construction below, which allocates (and
    // may GC) before the instance is reachable from the Ruby stack.
    let mut obj = regexp_alloc_func(class_id, globals);
    let temp = vm.temp_len();
    vm.temp_push(obj);
    let init_fid = vm.find_method(globals, obj, IdentId::INITIALIZE, true)?;
    if globals.store[init_fid].is_iseq().is_some() {
        // The subclass overrides `#initialize` (a Ruby method): run it so
        // its `super` reaches `Regexp#initialize` and its own body (e.g.
        // `@args = args`) executes.
        let args: Vec<Value> = match lfp.try_arg(1) {
            Some(opt) => vec![lfp.arg(0), opt],
            None => vec![lfp.arg(0)],
        };
        let kw = match lfp.try_arg(2).filter(|v| !v.is_nil()) {
            Some(t) => {
                let mut map = RubyMap::default();
                map.insert(Value::symbol(IdentId::get_id(TIMEOUT_KW)), t, vm, globals)?;
                Some(Hashmap::new(Value::hash(map)))
            }
            None => None,
        };
        vm.invoke_func_inner(globals, init_fid, obj, &args, None, kw)?;
    } else {
        // Inherited `Regexp#initialize`: build the data directly into the
        // freshly-allocated instance.
        let inner = build_regexp_inner(vm, globals, lfp)?;
        *obj.as_regexp_inner_mut() = inner;
        store_regexp_timeout(globals, obj, nanos)?;
    }
    vm.temp_clear(temp);
    Ok(obj)
}

/// The flag letters CRuby's `rb_reg_desc` appends to a rendered
/// source, in its order (`/\x81/mix`). `n` is left out: the only
/// caller skips a `NOENCODING` source entirely.
fn option_letters(option: u32) -> String {
    let mut res = String::new();
    if option & onigmo_regex::ONIG_OPTION_MULTILINE != 0 {
        res.push('m');
    }
    if option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0 {
        res.push('i');
    }
    if option & onigmo_regex::ONIG_OPTION_EXTEND != 0 {
        res.push('x');
    }
    res
}

/// CRuby's `rb_reg_preprocess` walks the source in the encoding it is
/// tagged with and refuses it as soon as the bytes spell no character
/// there: `Regexp.new("a\xA4".force_encoding("EUC-JP"))` raises
/// `RegexpError: invalid multibyte character: /a\xA4/`, whatever
/// Onigmo would later have said about the truncated lead byte.
///
/// Two sources are exempt. A BINARY one has a character per byte, so
/// it can never be broken; and a `NOENCODING` (`/…/n`) one is read as
/// bytes rather than in its tag's encoding — CRuby refuses that with a
/// different complaint ("/.../n has a non escaped non ASCII character
/// in non ASCII-8BIT script"), which monoruby does not raise yet, so
/// leave that case as it stands instead of answering it with this one.
pub(super) fn check_regexp_source_valid(source: &RStringInner, option: u32) -> Result<()> {
    if option & RegexpInner::NOENCODING != 0
        || source.encoding() == crate::value::Encoding::Ascii8
        || source.is_valid_encoding()
    {
        return Ok(());
    }
    Err(MonorubyErr::regexerr(format!(
        "invalid multibyte character: /{}/{}",
        super::string::regexp_source_desc(source),
        option_letters(option)
    )))
}

/// [`check_regexp_source_valid`] for a caller that holds the source as
/// loose bytes rather than as a `String` object — `Marshal.load`,
/// which rebuilds a Regexp straight from a `/` payload.
pub(super) fn check_regexp_source_bytes_valid(
    bytes: &[u8],
    encoding: crate::value::Encoding,
    option: u32,
) -> Result<()> {
    check_regexp_source_valid(&RStringInner::from_encoding(bytes, encoding), option)
}

/// A regexp source as an error message renders it: the message is
/// UTF-8, so a UTF-8 source's characters show as themselves and any
/// other encoding's are escaped by value (`/é/`, `/\x{A4A2}/`).
fn regexp_source_desc_in_message(bytes: &[u8], enc: crate::value::Encoding) -> String {
    String::from_utf8_lossy(&super::string::regexp_source_desc_bytes(
        bytes,
        enc,
        Some(crate::value::Encoding::UTF8),
    ))
    .into_owned()
}

/// `unescape_nonascii`'s reading of the `\xHH` escapes in a regexp
/// source: an escaped byte above `0x7F` has to spell, together with the
/// `\xHH` escapes right after it, one character of `enc` — "too short
/// escaped multibyte character" when the escapes run out first,
/// "invalid multibyte escape" when they spell none — and when it does,
/// the regexp is pinned to `enc` (`Ok(true)`). BINARY and US-ASCII
/// sources are exempt: their escaped bytes are bytes, and UTF-16 /
/// UTF-32 sources are pinned already, and spell their escapes in code
/// units this walk does not read.
pub(crate) fn check_regexp_hex_escapes(bytes: &[u8], enc: crate::value::Encoding) -> Result<bool> {
    use crate::value::Encoding;
    if matches!(enc, Encoding::Ascii8 | Encoding::UsAscii) || enc.is_wide() {
        return Ok(false);
    }
    let refuse = |what: &str| {
        MonorubyErr::regexerr(format!("{what}: /{}/", regexp_source_desc_in_message(bytes, enc)))
    };
    // A `\xH` / `\xHH` at `i` (the `x`): its byte and the index after
    // the digits.
    let hex_at = |i: usize| -> Option<(u8, usize)> {
        let d = |j: usize| bytes.get(j).and_then(|b| (*b as char).to_digit(16));
        let first = d(i + 1)?;
        match d(i + 2) {
            Some(second) => Some(((first * 16 + second) as u8, i + 3)),
            None => Some((first as u8, i + 2)),
        }
    };
    let mut pinned = false;
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] != b'\\' {
            i += 1;
            continue;
        }
        if bytes[i + 1] != b'x' {
            // `\\`, `\n`, `\u`, …: the escaped character is skipped
            // whole, so a `\\` is never read as the start of an escape.
            i += 2;
            continue;
        }
        let Some((byte, next)) = hex_at(i + 1) else {
            // No digits: the engine's "invalid hex escape".
            i += 2;
            continue;
        };
        i = next;
        if byte < 0x80 {
            continue;
        }
        let mut ch = vec![byte];
        loop {
            match crate::value::precise_mbclen(enc, &ch, 0) {
                PreciseLen::Char(_) => {
                    pinned = true;
                    break;
                }
                PreciseLen::Invalid => return Err(refuse("invalid multibyte escape")),
                PreciseLen::NeedMore => {
                    // The rest of the character has to be escaped too.
                    if bytes.get(i) == Some(&b'\\')
                        && bytes.get(i + 1) == Some(&b'x')
                        && let Some((b, next)) = hex_at(i + 1)
                    {
                        ch.push(b);
                        i = next;
                    } else {
                        return Err(refuse("too short escaped multibyte character"));
                    }
                }
            }
        }
    }
    Ok(pinned)
}

/// Parse `Regexp.new`/`#initialize` arguments (source + optional option)
/// into a fully-built `RegexpInner`.
fn build_regexp_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<RegexpInner> {
    let arg0 = lfp.arg(0);
    // When given an existing Regexp, carry over both source and
    // options so `Regexp.new(/abc/i) == /abc/i`. CRuby's `rb_reg_init`
    // copies the source verbatim and inherits the options unless the
    // caller passes an explicit second argument. We also forward the
    // existing regex's declared encoding so the new copy renders
    // the same `Regexp#encoding` even though we re-parse the source.
    // When the source is a Regexp, also forward the kcode bits
    // derived from its declared encoding so that `Regexp.new(/abc/u)`
    // ends up with `encoding == UTF-8` and `fixed_encoding? == true`.
    // `raw_option()` doesn't carry KCODE/NOENCODING/FIXEDENCODING
    // bits (those live in `RegexpInner`, not in Onigmo's option word).
    // The flags a Regexp source carries over. Settle them before the
    // source string is taken: CRuby's `rb_reg_initialize_m` reads the
    // options first, and the broken-source check below renders the
    // offending pattern with them (`/\x81/i`).
    let (default_option, default_kcode) = if let Some(re) = arg0.is_regex() {
        let kc = if re.fixed_encoding() {
            kcode_from_encoding(re.declared_encoding())
        } else {
            None
        };
        let mut opt = re.raw_option();
        if let Some(bits) = kc {
            opt |= bits;
        }
        (Some(opt), kc)
    } else {
        (None, None)
    };
    let option_provided = lfp.try_arg(1).is_some_and(|v| !v.is_nil());
    if arg0.is_regex().is_some() && option_provided {
        // CRuby emits "warning: flags ignored" (without raising) when
        // a Regexp arg is combined with explicit options. The original
        // regex's flags are preserved.
        warn_flags_ignored(vm, globals);
    }
    let option = if let Some(option) = lfp.try_arg(1) {
        if option.is_nil() {
            default_option.unwrap_or(onigmo_regex::ONIG_OPTION_NONE)
        } else if arg0.is_regex().is_some() {
            // Flags ignored; use the source regex's options unchanged.
            default_option.unwrap_or(onigmo_regex::ONIG_OPTION_NONE)
        } else if let Some(option) = option.try_fixnum() {
            option as i32 as u32
        } else if let Some(s) = option.is_str() {
            parse_option_string(s)?
        } else if option == Value::bool(false) {
            onigmo_regex::ONIG_OPTION_NONE
        } else if option == Value::bool(true) {
            onigmo_regex::ONIG_OPTION_IGNORECASE
        } else {
            warn_unexpected_regexp_option(vm, globals, option);
            onigmo_regex::ONIG_OPTION_IGNORECASE
        }
    } else {
        default_option.unwrap_or(onigmo_regex::ONIG_OPTION_NONE)
    };
    if arg0.is_regex().is_none() {
        return regexp_inner_from_string(vm, globals, arg0, option, default_kcode);
    }
    let re = arg0.is_regex().unwrap();
    let (string, source_encoding, source_bytes) = (
        re.as_str().to_string(),
        Some(re.declared_encoding()),
        // Preserve the original Regexp's source verbatim.
        Some(re.source_bytes().to_vec()),
    );
    regexp_inner_from_parts(string, source_encoding, source_bytes, option, default_kcode)
}

/// `Regexp.new(string, option)` — and `get_pat`, which is how a String
/// handed to `String#match` / `#index` / `#byteindex` becomes a
/// pattern: the string is compiled as a regexp source in its own
/// encoding, whatever that encoding is.
pub(crate) fn regexp_inner_from_string(
    vm: &mut Executor,
    globals: &mut Globals,
    arg0: Value,
    option: u32,
    default_kcode: Option<u32>,
) -> Result<RegexpInner> {
    // The matching engine needs a UTF-8 view, so escape non-UTF-8
    // bytes (`coerce_to_string`), but keep the *raw* bytes for
    // `Regexp#source` so a Shift_JIS/EUC-JP/binary source survives.
    let (raw, enc) = match arg0.is_rstring_inner() {
        Some(r) => {
            check_regexp_source_valid(&r, option)?;
            // `/…/n` reads its source as bytes, so a non-ASCII character
            // written raw in any source but a BINARY one — and every
            // character of a UTF-16 / UTF-32 source — is refused
            // (`unescape_nonascii`); the `\xHH` form is how such a byte
            // is spelled there.
            if option & RegexpInner::NOENCODING != 0
                && r.encoding() != crate::value::Encoding::Ascii8
                && (!r.encoding().is_ascii_compatible() || !r.is_ascii_only())
            {
                return Err(MonorubyErr::regexerr(format!(
                    "/.../n has a non escaped non ASCII character in non ASCII-8BIT script: /{}/",
                    regexp_source_desc_in_message(r.as_bytes(), r.encoding())
                )));
            }
            (Some(r.as_bytes().to_vec()), r.encoding())
        }
        None => (None, crate::value::Encoding::UTF8),
    };
    let s = arg0.coerce_to_string(vm, globals)?;
    regexp_inner_from_parts(s, Some(enc), raw, option, default_kcode)
}

/// The reading of a regexp source shared by `Regexp.new`, the literal
/// (`const_regexp`) and the interpolated literal (`concatenate_regexp`):
/// `rb_reg_initialize` plus `rb_reg_preprocess`. `string` is the
/// source as UTF-8 text (the engine's view when the pattern compiles
/// under UTF-8), `source_bytes` the bytes as written, in
/// `source_encoding`; an `e` / `s` / `u` modifier (`default_kcode` or the
/// `KCODE_*` bits of `option`) re-tags them first, as
/// `reg_fragment_setenc` does.
pub(crate) fn regexp_inner_from_parts(
    string: String,
    source_encoding: Option<crate::value::Encoding>,
    source_bytes: Option<Vec<u8>>,
    mut option: u32,
    default_kcode: Option<u32>,
) -> Result<RegexpInner> {
    // The encoding the source is read in: the modifier's when there
    // is one, else the string's own.
    let kcode_bits = option & RegexpInner::KCODE_MASK;
    let effective_kcode = if kcode_bits != 0 { Some(kcode_bits) } else { default_kcode };
    let source_encoding = match effective_kcode {
        Some(k) if k & RegexpInner::KCODE_UTF8 != 0 => Some(crate::value::Encoding::UTF8),
        Some(k) if k & RegexpInner::KCODE_EUCJP != 0 => Some(crate::value::Encoding::EUC_JP),
        Some(k) if k & RegexpInner::KCODE_SJIS != 0 => Some(crate::value::Encoding::Sjis(1)),
        _ => source_encoding,
    };
    // A `\xHH` escape spelling a character of that encoding pins the
    // regexp to it, and one spelling none is refused here, before
    // Onigmo (whose own reading of the bytes is worded differently).
    if option & RegexpInner::NOENCODING == 0
        && let (Some(enc), Some(bytes)) = (source_encoding, source_bytes.as_deref())
        && check_regexp_hex_escapes(bytes, enc)?
    {
        option |= RegexpInner::FIXEDENCODING;
    }
    // A BINARY source carrying high bytes — raw, or as `\xHH` escapes
    // (`Regexp.new("[\xC2-\xDF]".b)`, what `Regexp.union` of `/…/n`
    // regexps hands back) — is matched byte-wise like `/…/n`; under the
    // UTF-8 codec such an escape is "too short multibyte code string".
    let binary_source = source_encoding == Some(crate::value::Encoding::Ascii8)
        && source_bytes.as_ref().is_some_and(|b| {
            b.iter().any(|&c| c >= 0x80) || RegexpInner::has_non_ascii_hex_escape(b)
        });
    // A source in a native ASCII-compatible codec (EUC-JP, Shift_JIS,
    // ISO-8859-*) carrying non-ASCII bytes has to be compiled under
    // *that* codec: the escaped UTF-8 view hands Onigmo a `\xA4\xEC`
    // pair, which is "too short multibyte code string" under UTF-8.
    let native_source = if option & RegexpInner::NOENCODING == 0 && !binary_source {
        source_encoding
            .filter(|e| {
                !matches!(
                    e,
                    crate::value::Encoding::Utf8(_)
                        | crate::value::Encoding::UsAscii
                        | crate::value::Encoding::Ascii8
                )
            })
            .filter(|_| {
                source_bytes.as_ref().is_some_and(|b| {
                    b.iter().any(|&c| c >= 0x80) || RegexpInner::has_non_ascii_hex_escape(b)
                })
            })
            .and_then(RegexpInner::onigmo_encoding_for)
    } else {
        None
    };
    // A UTF-16 / UTF-32 source is compiled under its own codec whatever
    // it contains: even its ASCII characters are not the bytes the
    // UTF-8 engine reads.
    let wide_source = if option & RegexpInner::NOENCODING == 0 {
        source_encoding
            .filter(|e| e.is_wide())
            .and_then(RegexpInner::onigmo_encoding_for)
    } else {
        None
    };
    let encoding = if option & RegexpInner::NOENCODING != 0 || binary_source {
        onigmo_regex::OnigmoEncoding::ASCII
    } else {
        native_source
            .or(wide_source)
            .unwrap_or(onigmo_regex::OnigmoEncoding::UTF8)
    };
    // Pull the kcode bit out of the option mask before passing to
    // onigmo (which doesn't understand the modifier letters).
    let kcode_bits = option & RegexpInner::KCODE_MASK;
    let kcode = if kcode_bits != 0 {
        Some(kcode_bits)
    } else {
        default_kcode
    };
    let regexp = RegexpInner::with_option_kcode_source(
        string,
        option,
        encoding,
        kcode,
        source_encoding,
        source_bytes,
    )?;
    Ok(regexp)
}

/// Map a Ruby-visible encoding to the matching KCODE_* bit, mirroring
/// what `n`/`u`/`e`/`s` modifiers would set. Used when reconstructing
/// a Regexp from another Regexp so the new one's `encoding`/
/// `fixed_encoding?` mirrors the original.
fn kcode_from_encoding(enc: crate::value::Encoding) -> Option<u32> {
    use crate::value::Encoding;
    match enc {
        Encoding::Utf8(_) => Some(RegexpInner::KCODE_UTF8),
        Encoding::EucJp(_) => Some(RegexpInner::KCODE_EUCJP),
        Encoding::Sjis(_) => Some(RegexpInner::KCODE_SJIS),
        _ => None,
    }
}

/// Emit CRuby's "flags ignored" warning to `$stderr`, used when
/// `Regexp.new(/.../, opt)` is called with a Regexp source — the
/// options in the second arg are discarded and the source regex's
/// flags are preserved.
fn warn_flags_ignored(vm: &mut Executor, globals: &mut Globals) {
    let stderr_id = IdentId::get_id("$stderr");
    let stderr = match globals.get_gvar(stderr_id) {
        Some(v) => v,
        None => return,
    };
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[Value::string(
            "warning: flags ignored\n".to_string(),
        )],
        None,
        None,
    );
}

/// Parse a Ruby flag string like "im" into an Onigmo options bitmap.
/// Only `i`/`m`/`x` are accepted; the kcode letters
/// (`n`/`u`/`e`/`s`) are *literal-only* flags in CRuby — passing
/// them via `Regexp.new("...", "u")` raises
/// `ArgumentError: unknown regexp option: u`. Anything else also
/// raises `ArgumentError`.
fn parse_option_string(s: &str) -> Result<u32> {
    // First pass: scan for any non-`i`/`m`/`x` char. If we hit
    // one, CRuby reports the *whole* string in the error message
    // (`Regexp.new("...", "mjx")` → "unknown regexp option: mjx",
    // not just "j") — we replicate that here.
    if !s.chars().all(|c| matches!(c, 'i' | 'm' | 'x')) {
        return Err(MonorubyErr::argumenterr(format!(
            "unknown regexp option: {s}"
        )));
    }
    let mut opt = onigmo_regex::ONIG_OPTION_NONE;
    for c in s.chars() {
        match c {
            'i' => opt |= onigmo_regex::ONIG_OPTION_IGNORECASE,
            'm' => opt |= onigmo_regex::ONIG_OPTION_MULTILINE,
            'x' => opt |= onigmo_regex::ONIG_OPTION_EXTEND,
            _ => unreachable!(),
        }
    }
    Ok(opt)
}

/// Emit CRuby's "expected true or false as ignorecase" warning to
/// `$stderr`, used when `Regexp.new`'s second argument is a non-
/// Integer / non-String / non-nil / non-bool value. We don't need to
/// invoke `Kernel#warn` here — `complain` matchers in mspec read
/// `$stderr.write` output, which is what the spec checks.
fn warn_unexpected_regexp_option(vm: &mut Executor, globals: &mut Globals, option: Value) {
    let stderr_id = IdentId::get_id("$stderr");
    let stderr = match globals.get_gvar(stderr_id) {
        Some(v) => v,
        None => return,
    };
    let msg = format!(
        "warning: expected true or false as ignorecase: {}\n",
        option.inspect(&globals.store)
    );
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[Value::string(msg)],
        None,
        None,
    );
}

///
/// ### Regexp.escape
/// - escape(string) -> String
/// - quote(string) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/escape.html]
#[monoruby_builtin]
fn regexp_escape(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg0 = lfp.arg(0);
    // CRuby accepts both String and Symbol; for a Symbol we use its
    // textual form. Other types still go through `to_str`. Work on
    // raw bytes so "broken" strings (invalid byte sequences for
    // their encoding) escape byte-wise instead of raising.
    let (bytes, src_enc) = if let Some(r) = arg0.is_rstring_inner() {
        (r.as_bytes().to_vec(), r.encoding())
    } else if let Some(sym) = arg0.try_symbol() {
        (
            sym.to_string().into_bytes(),
            crate::value::Encoding::UTF8,
        )
    } else {
        (
            arg0.coerce_to_str(vm, globals)?.into_bytes(),
            crate::value::Encoding::UTF8,
        )
    };
    let escaped = RegexpInner::escape_in(&bytes, src_enc);
    // CRuby tags the result US-ASCII when it is 7-bit in an
    // ASCII-compatible encoding (otherwise it keeps the source's — a
    // UTF-16 string is never 7-bit, whatever it spells). The escape
    // itself only adds ASCII metacharacters, so the result is 7-bit iff
    // the input is.
    let result_enc = if src_enc.is_ascii_compatible() && escaped.is_ascii() {
        crate::value::Encoding::UsAscii
    } else {
        src_enc
    };
    Ok(Value::string_from_inner(
        crate::value::rvalue::RStringInner::from_encoding_scanned(
            &escaped,
            result_enc,
        ),
    ))
}

///
/// ### Regexp.union
/// - union(*pattern) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/union.html]
#[monoruby_builtin]
fn regexp_union(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut rest = lfp.arg(0).as_array();
    if rest.is_empty() {
        return Ok(Value::regexp(RegexpInner::with_option("(?!)", 0)?));
    }
    if rest.len() == 1 {
        let arg = rest[0];
        if let Some(arr) = arg.try_array_ty() {
            rest = arr;
        } else {
            if let Some(re) = arg.is_regex() {
                return Ok(re.into());
            }
            if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
                let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
                if let Some(re) = result.is_regex() {
                    return Ok(re.into());
                }
            }
            let s = format_union_member(vm, globals, arg)?;
            // Single non-Regexp arg: pin the encoding to the
            // arg's source encoding (CRuby behaviour), rendering
            // through the same compat-check pipeline so a
            // non-ASCII-compatible source produces a
            // non-ASCII-compatible regex.
            let combined = UnionEnc::Free.combine(union_arg_encoding(globals, arg), &globals.store)?;
            return Ok(Value::regexp(union_inner_with_encoding(s, combined)?));
        }
    }
    let mut parts = Vec::with_capacity(rest.len());
    let mut combined: UnionEnc = UnionEnc::Free;
    let mut all_ascii_only = true;
    for arg in rest.iter() {
        let enc = union_arg_encoding(globals, *arg);
        if !enc.ascii_only {
            all_ascii_only = false;
        }
        combined = combined.combine(enc, &globals.store)?;
        parts.push(format_union_member(vm, globals, *arg)?);
    }
    // CRuby's `Regexp.union` downgrades the result to US-ASCII
    // when *every* arg was 7-bit ASCII content, even if
    // individual args were declared as a pinned encoding via a
    // `/.../e`-style modifier. Honour that downgrade unless an
    // ASCII-incompatible encoding is in play (UTF-16, UTF-32),
    // which we always pin.
    let resolved_combined = if all_ascii_only
        && !matches!(combined, UnionEnc::Pinned(e) if !e.is_ascii_compatible())
    {
        UnionEnc::Free
    } else {
        combined
    };
    // The `|` between members is a character of the result's encoding
    // (`rb_str_buf_cat_ascii`): two bytes in UTF-16.
    let bar = crate::value::enc_mbcput(resolved_combined.resolved(), b'|' as u32)
        .unwrap_or_else(|_| vec![b'|']);
    let s = parts.join(&bar[..]);
    Ok(Value::regexp(union_inner_with_encoding(s, resolved_combined)?))
}

/// Tracks the running encoding state of a `Regexp.union` build:
///   - `Free`: nothing requires a particular encoding yet (only
///     ASCII-only / not-fixed args seen).
///   - `Pinned(enc)`: at least one arg pinned the result to a
///     specific encoding; subsequent fixed args must agree.
#[derive(Clone, Copy)]
enum UnionEnc {
    Free,
    Pinned(crate::value::Encoding),
}

/// Encoding info extracted from a single `Regexp.union` argument.
/// `fixed` is true when the arg's encoding is non-negotiable (a
/// Regexp with `fixed_encoding?` true, or a String with non-ASCII
/// content / a non-ASCII-compatible source encoding).
struct ArgEnc {
    encoding: crate::value::Encoding,
    fixed: bool,
    /// Whether the arg's *content* is all 7-bit ASCII. Tracked
    /// separately from `fixed` because CRuby's `Regexp.union`
    /// downgrades a string of all-ASCII-only args to `US-ASCII`
    /// even when individual args were pinned by an
    /// `e`/`u`/`s`/`n` modifier.
    ascii_only: bool,
}

impl UnionEnc {
    fn combine(self, arg: ArgEnc, store: &Store) -> Result<Self> {
        let _ = store;
        // An ASCII-incompatible encoding (UTF-16LE/BE, UTF-32LE/BE,
        // …) can't share a regex with anything else: even pure-
        // ASCII strings/Regexps live in a 1-byte-per-char world
        // that an ASCII-incompatible encoding doesn't speak.
        // CRuby raises ArgumentError for any mix.
        let prev_pinned = matches!(self, UnionEnc::Pinned(prev) if !prev.is_ascii_compatible());
        let arg_ascii_incompat = !arg.encoding.is_ascii_compatible();
        if prev_pinned && arg_ascii_incompat {
            // both sides have an explicit non-ASCII-compat encoding —
            // they must match exactly, otherwise raise.
            if let UnionEnc::Pinned(prev) = self
                && prev != arg.encoding
            {
                return Err(MonorubyErr::argumenterr(format!(
                    "incompatible encodings: {} and {}",
                    prev.name(),
                    arg.encoding.name()
                )));
            }
        } else if prev_pinned {
            // Previous side is non-ASCII-compat; arg is anything
            // else (even pure-ASCII). CRuby still rejects. When the
            // other side is pure 7-bit ASCII, CRuby phrases it as
            // "ASCII incompatible encoding: <enc>"; otherwise it names
            // both encodings.
            if let UnionEnc::Pinned(prev) = self {
                if arg.ascii_only {
                    return Err(MonorubyErr::argumenterr(format!(
                        "ASCII incompatible encoding: {}",
                        prev.name()
                    )));
                }
                return Err(MonorubyErr::argumenterr(format!(
                    "incompatible encodings: {} and {}",
                    prev.name(),
                    arg.encoding.name()
                )));
            }
        } else if arg_ascii_incompat {
            // Arg is non-ASCII-compat. Pin to it — but a previous
            // ASCII-only value already in `self` (Free or pinned-
            // ASCII) is incompatible with it.
            if !matches!(self, UnionEnc::Free) {
                if let UnionEnc::Pinned(prev) = self {
                    return Err(MonorubyErr::argumenterr(format!(
                        "incompatible encodings: {} and {}",
                        prev.name(),
                        arg.encoding.name()
                    )));
                }
            }
            return Ok(UnionEnc::Pinned(arg.encoding));
        }
        if !arg.fixed {
            return Ok(self);
        }
        match self {
            UnionEnc::Free => Ok(UnionEnc::Pinned(arg.encoding)),
            UnionEnc::Pinned(prev) if prev == arg.encoding => Ok(self),
            UnionEnc::Pinned(prev) => Err(MonorubyErr::argumenterr(format!(
                "incompatible encodings: {} and {}",
                prev.name(),
                arg.encoding.name()
            ))),
        }
    }

    fn resolved(self) -> crate::value::Encoding {
        match self {
            UnionEnc::Free => crate::value::Encoding::UsAscii,
            UnionEnc::Pinned(e) => e,
        }
    }
}

/// Read encoding info off a `Regexp.union` argument: Regexp uses
/// its declared encoding + `fixed_encoding?`, String uses the
/// content's actual encoding + "non-ASCII content / non-ASCII-
/// compat source" as the fixed bit. Other types fall back to a
/// non-fixed US-ASCII bucket.
fn union_arg_encoding(globals: &Globals, arg: Value) -> ArgEnc {
    if let Some(re) = arg.is_regex() {
        // A BINARY (`/.../n` with high `\xHH` escapes) regexp carries
        // non-ASCII content even though its source *text* is 7-bit, so
        // it must not be treated as ASCII-only (which would let
        // `Regexp.union` downgrade the result to US-ASCII).
        let ascii_only = re.declared_encoding() != crate::value::Encoding::Ascii8
            && re.source_bytes().iter().all(|&b| b < 0x80);
        return ArgEnc {
            encoding: re.declared_encoding(),
            fixed: re.fixed_encoding(),
            ascii_only,
        };
    }
    if let Some(s) = arg.is_rstring_inner() {
        let enc = s.encoding();
        let has_non_ascii = s.as_bytes().iter().any(|&b| b >= 0x80);
        let fixed = has_non_ascii || !enc.is_ascii_compatible();
        return ArgEnc {
            encoding: enc,
            fixed,
            ascii_only: !has_non_ascii && enc.is_ascii_compatible(),
        };
    }
    let _ = globals;
    ArgEnc {
        encoding: crate::value::Encoding::UsAscii,
        fixed: false,
        ascii_only: true,
    }
}

/// Build the resulting `RegexpInner` for `Regexp.union`, picking
/// the matching engine (`OnigmoEncoding::ASCII` for BINARY,
/// `OnigmoEncoding::UTF8` for everything else) and faking the
/// kcode bit so the declared encoding round-trips through
/// `Regexp#encoding`.
fn union_inner_with_encoding(
    pattern: Vec<u8>,
    union_enc: UnionEnc,
) -> Result<RegexpInner> {
    use crate::value::Encoding;
    let enc = union_enc.resolved();
    let pinned = matches!(union_enc, UnionEnc::Pinned(_));
    // For pinned encodings without a kcode equivalent (UTF-16LE,
    // UTF-32, ISO-8859-*, …) we can't piggy-back on the
    // `KCODE_*` bits — set `FIXEDENCODING` instead so the source-
    // encoding fallback branch in `resolve_declared_encoding`
    // honours the encoding we computed.
    // `FIXEDENCODING`, never a `KCODE_*` bit: the result is pinned to
    // the encoding a member had — Shift_JIS stays Shift_JIS where the
    // `s` modifier would have made it Windows-31J — and a BINARY member
    // pins BINARY without the `n` modifier having been written (CRuby's
    // union of a `/…/n` member carries no `n` of its own, #1516).
    let (option, kcode) = if pinned || matches!(enc, Encoding::Ascii8) {
        (RegexpInner::FIXEDENCODING, None)
    } else {
        (0u32, None)
    };
    // `Regexp.union` ends in `rb_reg_new_str`, so the joined source is
    // preprocessed like any other and a member that is broken in its
    // own encoding is refused there — after the per-member encoding
    // combine above, which is what raises for two *different* broken
    // encodings. The source CRuby renders is the join, not the member
    // (`Regexp.union("x", eucbad)` is `/x|a\xA4/`), which is exactly
    // what `pattern` now holds.
    check_regexp_source_bytes_valid(&pattern, enc, option)?;
    // From here the join is read exactly as `Regexp.new` reads a String
    // in `enc` — the engine (a Shift_JIS join compiles under Shift_JIS,
    // a UTF-16 one under UTF-16), the declared encoding and the `\xHH`
    // escapes are all the shared builder's.
    let text = String::from_utf8_lossy(&pattern).into_owned();
    regexp_inner_from_parts(text, Some(enc), Some(pattern), option, kcode)
}

/// Render a single `Regexp.union` argument into its embedded form.
/// Strings and Symbols are passed through `Regexp.escape`; Regexps
/// use their `to_s` group form so flags are preserved. Falls back to
/// `to_regexp` then `to_str` for general objects, matching CRuby.
fn format_union_member(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<Vec<u8>> {
    // Bytes, not a Rust `String`: a member whose source is not UTF-8
    // has to reach the union's own source as the bytes that went in,
    // or the union neither renders nor *matches* what it was built
    // from (#1516).
    //
    // `is_rstring_inner()` (not `is_str()`) — a String tagged as
    // an ASCII-incompatible encoding (UTF-16LE etc.) or one
    // carrying invalid UTF-8 bytes shows up here, and `is_str()`
    // would reject it for not being valid UTF-8.
    if let Some(s) = arg.is_rstring_inner() {
        return Ok(RegexpInner::escape_in(s.as_bytes(), s.encoding()));
    }
    if let Some(re) = arg.is_regex() {
        return Ok(re.tos_bytes());
    }
    if let Some(sym) = arg.try_symbol() {
        return Ok(RegexpInner::escape(sym.get_name().as_str()).into_bytes());
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(re) = result.is_regex() {
            return Ok(re.tos_bytes());
        }
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::TO_STR) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(s) = result.is_rstring_inner() {
            return Ok(RegexpInner::escape_in(s.as_bytes(), s.encoding()));
        }
    }
    let class = arg.builtin_class_name(&globals.store);
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {class} into String"
    )))
}

///
/// ### Regexp.last_match
/// - last_match -> MatchData
/// - last_match(nth) -> String | nil           (Integer nth)
/// - last_match(name) -> String | nil          (Symbol / String name)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
#[monoruby_builtin]
fn regexp_last_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let Some(arg0) = lfp.try_arg(0) else {
        return Ok(vm.get_last_matchdata());
    };
    // CRuby short-circuits to `nil` when there is no last match,
    // even for arguments that would otherwise raise (e.g. an
    // arbitrary `Object` that doesn't respond to `to_int`). Test
    // the no-match case before dispatching so we don't surface
    // a coercion `TypeError` that wouldn't fire on CRuby.
    let md = vm.get_last_matchdata();
    if md.is_nil() {
        return Ok(Value::nil());
    }
    // Symbol / String name → look up as a named capture in the
    // most recent MatchData. CRuby raises `IndexError` on a
    // missing name *only* when there is a current match;
    // when the last match is `nil` we just propagate `nil`.
    let is_name = arg0.try_symbol().is_some() || arg0.is_str().is_some();
    if is_name {
        // Dispatch to `MatchData#[]` so the name-lookup logic
        // (and the IndexError on missing name) lives in one
        // place rather than duplicating it here.
        return vm.invoke_method_inner(
            globals,
            IdentId::get_id("[]"),
            md,
            &[arg0],
            None,
            None,
        );
    }
    // Integer / `to_int`-coercible arg: treat as an Nth-capture
    // index. CRuby silently returns `nil` when the index is out of
    // range; `get_special_matches` already does that.
    let nth = arg0.coerce_to_int_i64(vm, globals)?;
    Ok(vm.get_special_matches(nth).unwrap_or_default())
}

/// Mask of Onigmo options that participate in `Regexp#==` / `#eql?` /
/// `#hash`. Only the `m`/`i`/`x` flags belong here; encoding bits
/// participate via the separately-tracked `declared_encoding` field
/// so that, e.g., `/abc/u == /abc/n` is `false` while `// == //n`
/// is `true` (empty source ⇒ both resolve to US-ASCII).
const REGEXP_EQ_OPTION_MASK: u32 = onigmo_regex::ONIG_OPTION_MULTILINE
    | onigmo_regex::ONIG_OPTION_IGNORECASE
    | onigmo_regex::ONIG_OPTION_EXTEND;

///
/// ### Regexp#==, Regexp#eql?
/// - self == other -> bool
/// - self.eql?(other) -> bool
///
/// True when both are regexps with the same source pattern, the
/// same `m`/`i`/`x` options, and the same declared encoding.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d.html]
#[monoruby_builtin]
fn regexp_eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_regexp_inner();
    if !lhs.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    let rhs = match lfp.arg(0).is_regex() {
        Some(r) => r,
        None => return Ok(Value::bool(false)),
    };
    if !rhs.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    let same_source = lhs.source_bytes() == rhs.source_bytes();
    let same_options =
        (lhs.raw_option() & REGEXP_EQ_OPTION_MASK) == (rhs.raw_option() & REGEXP_EQ_OPTION_MASK);
    let same_encoding = lhs.declared_encoding() == rhs.declared_encoding();
    Ok(Value::bool(same_source && same_options && same_encoding))
}

/// ### Regexp#hash
/// Returns a hash code based on the source pattern and the `m`/`i`/`x`
/// options. CRuby intentionally hashes only `source` + onigmo options
/// (encoding flags don't participate), so `/abc/u.hash == /abc/n.hash`
/// even though `/abc/u != /abc/n` — collisions are fine for hash
/// semantics.
#[monoruby_builtin]
fn regexp_hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let self_ = lfp.self_val();
    let re = self_.as_regexp_inner();
    let mut h = crate::value::seeded_hasher();
    re.source_bytes().hash(&mut h);
    (re.raw_option() & REGEXP_EQ_OPTION_MASK).hash(&mut h);
    Ok(Value::from_hash_digest(h.finish()))
}

/// ### Regexp#===
/// - self === string -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let arg0 = lfp.arg(0);
    // CRuby returns false (without matching) for nil / Regexp
    // and other non-string-like args. For string-like args
    // (responds to `to_str`), it coerces and matches.
    let subject = if arg0.is_rstring().is_some() {
        arg0
    } else if arg0.try_symbol().is_some() {
        symbol_as_string(arg0)
    } else if let Some(func_id) = globals.check_method(arg0, IdentId::TO_STR) {
        // `to_str` coercion path: call the method, then accept the
        // result iff it's a String. CRuby raises TypeError on a
        // non-String return value (`can't convert X to String
        // (X#to_str gives Y)`); we mirror that.
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        if result.is_str().is_none() {
            let class = arg0.get_real_class_name(&globals.store);
            let res_class = result.get_real_class_name(&globals.store);
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {class} to String ({class}#to_str gives {res_class})"
            )));
        }
        result
    } else {
        return Ok(Value::bool(false));
    };
    check_subject_match_encoding(&globals.store, &regex, subject)?;
    warn_binary_regexp_match(vm, globals, &regex, subject);
    // A subject Onigmo walks natively is matched on its own bytes (see
    // `Regexp#match`).
    if let Some(rs) = subject.is_rstring()
        && rs.code_range() != CodeRange::SevenBit
        && let Some(native_enc) = RegexpInner::native_codec_for(&subject.as_rstring_inner())
    {
        vm.set_match_regex(self_);
        let bytes = subject.as_rstring_inner().as_bytes();
        let hit = regex
            .captures_bytes_from_pos(bytes, subject, native_enc, 0, vm)?
            .is_some();
        return Ok(Value::bool(hit));
    }
    // Stash a UTF-8-valid String subject so the MatchData snapshot is
    // zero-copy and carries the subject's encoding into $&/$1..$N;
    // non-UTF-8 subjects fall back to the owned lossy copy as before.
    let given_owned;
    let given: &str = match subject.is_rstring() {
        Some(rs) if std::str::from_utf8(rs.as_bytes()).is_ok() => {
            vm.set_match_haystack(subject);
            // SAFETY: just validated as UTF-8.
            unsafe { std::str::from_utf8_unchecked(subject.as_rstring_inner().as_bytes()) }
        }
        _ => {
            given_owned =
                String::from_utf8_lossy(subject.as_rstring_inner().as_bytes()).into_owned();
            &given_owned
        }
    };
    vm.set_match_regex(self_);
    let res = Value::bool(regex.find_one(vm, given)?.is_some());
    Ok(res)
}

/// CRuby's `rb_reg_prepare_enc`: verify that `regex` can be matched
/// against a subject of encoding `str_enc` (with `str_ascii_only`
/// telling whether its content is entirely 7-bit), raising
/// `Encoding::CompatibilityError` when it cannot. No-op for
/// Symbol subjects and for compatible String/Regexp encoding pairs.
///
/// The rule:
///   - An **ASCII-incompatible** subject (UTF-16/32) is matchable only
///     by a regexp of the *same* encoding.
///   - A **fixed-encoding** regexp on an ASCII-compatible subject must
///     share the subject's encoding, unless the regexp's own encoding is
///     ASCII-compatible *and* the subject is entirely 7-bit.
///   - Otherwise (a non-fixed, ASCII-compatible regexp on an
///     ASCII-compatible subject) any content is fine.
pub(crate) fn check_match_encoding(
    store: &Store,
    regex: &RegexpInner,
    subject: &RStringInner,
) -> Result<()> {
    // `rb_reg_prepare_enc` refuses a broken subject before it looks at
    // the encodings at all, naming the subject's own encoding — so an
    // odd-length UTF-16LE string is "invalid byte sequence in UTF-16LE"
    // and never "incompatible encoding regexp match", and a US-ASCII
    // string with a high byte names US-ASCII, not the UTF-8 its view
    // is read through.
    if !subject.is_valid_encoding() {
        return Err(MonorubyErr::argumenterr(format!(
            "invalid byte sequence in {}",
            subject.encoding().name()
        )));
    }
    let str_enc = subject.encoding();
    let str_ascii_only = subject.is_ascii_only();
    let reg_enc = regex.declared_encoding();
    if !str_enc.is_ascii_compatible() {
        if reg_enc != str_enc {
            return Err(regexp_encoding_mismatch(store, reg_enc, str_enc));
        }
    } else if regex.fixed_encoding()
        && reg_enc != str_enc
        && (!reg_enc.is_ascii_compatible() || !str_ascii_only)
    {
        return Err(regexp_encoding_mismatch(store, reg_enc, str_enc));
    }
    Ok(())
}

/// `Encoding::CompatibilityError` for a regexp/subject encoding clash,
/// worded as CRuby's `reg_enc_error`.
fn regexp_encoding_mismatch(
    store: &Store,
    reg_enc: crate::value::Encoding,
    str_enc: crate::value::Encoding,
) -> MonorubyErr {
    MonorubyErr::encoding_compatibility_error_with_store(
        store,
        format!(
            "incompatible encoding regexp match ({} regexp with {} string)",
            reg_enc.inspect_label(),
            str_enc.inspect_label()
        ),
    )
}

/// Run [`check_match_encoding`] for a subject `Value` — only Strings
/// carry an encoding that can clash; Symbols are always compatible.
fn check_subject_match_encoding(
    store: &Store,
    regex: &RegexpInner,
    subject: Value,
) -> Result<()> {
    if subject.is_rstring().is_some() {
        check_match_encoding(store, regex, &subject.as_rstring_inner())?;
    } else if let Some(sym) = subject.try_symbol() {
        // `rb_reg_match` reads a Symbol as its String (US-ASCII when
        // 7-bit, UTF-8 otherwise), so a regexp that cannot match such a
        // string cannot match the Symbol either.
        let name = sym.get_name();
        let enc = if name.is_ascii() {
            crate::value::Encoding::UsAscii
        } else {
            crate::value::Encoding::UTF8
        };
        let inner = RStringInner::from_encoding(name.as_bytes(), enc);
        check_match_encoding(store, regex, &inner)?;
    }
    Ok(())
}

/// CRuby's `historical binary regexp match` warning (`rb_reg_prepare_enc`):
/// a `/n` (NOENCODING) regexp matched against a String that carries
/// non-ASCII content in an encoding other than ASCII-8BIT relies on
/// byte-wise matching that predates encoding-aware regexps. Warn at the
/// default level, once per match, like CRuby. No-op for a Symbol subject
/// or an ASCII-only / already-BINARY String.
fn warn_binary_regexp_match(
    vm: &mut Executor,
    globals: &mut Globals,
    regex: &RegexpInner,
    subject: Value,
) {
    if regex.option() & RegexpInner::NOENCODING == 0 || subject.is_rstring().is_none() {
        return;
    }
    let inner = subject.as_rstring_inner();
    let enc = inner.encoding();
    if enc != crate::value::Encoding::Ascii8 && !inner.is_ascii_only() {
        let _ = vm.ruby_warn(
            globals,
            &format!(
                "warning: historical binary regexp match /.../n against {} string",
                enc.name()
            ),
        );
    }
}

///
/// ### Regexp#=~
/// - self =~ string -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=7e.html]
#[monoruby_builtin]
fn regexp_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if lfp.arg(0).is_nil() {
        vm.clear_capture_special_variables();
        return Ok(Value::nil());
    }
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    if !regex.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    // Borrow the subject's bytes directly (and stash the Value for the
    // zero-copy MatchData snapshot, which also propagates the subject's
    // encoding to $&/$`/$'/$1..$N) when it is a UTF-8-valid String;
    // Symbols and non-UTF-8 subjects fall back to the owned conversion.
    let arg0 = lfp.arg(0);
    check_subject_match_encoding(&globals.store, &regex, arg0)?;
    warn_binary_regexp_match(vm, globals, &regex, arg0);
    // A subject in a non-UTF-8 encoding Onigmo has a native codec for
    // is matched on its raw bytes (see `Regexp#match`); the result is
    // the *character* index of the match start.
    if let Some(rs) = arg0.is_rstring()
        && rs.code_range() != CodeRange::SevenBit
        && let Some(native_enc) = RegexpInner::onigmo_encoding_for(rs.encoding())
    {
        vm.set_match_regex(self_);
        let bytes = arg0.as_rstring_inner().as_bytes();
        let res = match regex.captures_bytes_from_pos(bytes, arg0, native_enc, 0, vm)? {
            Some(captures) => {
                let start = captures.pos(0).map_or(0, |(s, _)| s);
                Value::integer(char_index_of_byte(arg0.as_rstring_inner(), start) as i64)
            }
            None => Value::nil(),
        };
        return Ok(res);
    }
    let arg0 = symbol_as_string(arg0);
    let given_owned;
    let given: &str = match arg0.is_rstring() {
        Some(rs) if std::str::from_utf8(rs.as_bytes()).is_ok() => {
            vm.set_match_haystack(arg0);
            // SAFETY: just validated as UTF-8.
            unsafe { std::str::from_utf8_unchecked(arg0.as_rstring_inner().as_bytes()) }
        }
        _ => {
            given_owned = subject_to_string(globals, arg0)?;
            &given_owned
        }
    };
    vm.set_match_regex(self_);
    let res = match regex.find_one(vm, given)? {
        Some(mat) => Value::integer(mat.start as i64),
        None => Value::nil(),
    };
    Ok(res)
}

/// The character index of byte offset `byte` in `s`, walking the
/// string's own encoding (one char per byte for single-byte encodings).
pub(super) fn char_index_of_byte(s: &crate::value::rvalue::RStringInner, byte: usize) -> usize {
    let mut chars = 0;
    let mut off = 0;
    for c in s.iter_char_bytes() {
        if off >= byte {
            break;
        }
        off += c.len();
        chars += 1;
    }
    chars
}

/// The byte offset of character index `cp` in `s`, walking the string's
/// own encoding; clamps to the end for a position past it.
pub(super) fn byte_offset_of_char(s: &crate::value::rvalue::RStringInner, cp: usize) -> usize {
    s.iter_char_bytes().take(cp).map(|c| c.len()).sum()
}

///
/// Convert `i` to the position of the char in the string with `len` chars.
///
/// Return None if `i` is out of range.
///
fn conv_index(i: i64, len: usize) -> Option<usize> {
    if i >= 0 {
        if i <= len as i64 {
            Some(i as usize)
        } else {
            None
        }
    } else {
        match len as i64 + i {
            n if n < 0 => None,
            n => Some(n as usize),
        }
    }
}

///
/// ### Regexp#source
/// - source -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/source.html]
#[monoruby_builtin]
fn source(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let re = self_.is_regex().unwrap();
    // CRuby's `Regexp#source.encoding`: a regexp whose encoding is pinned
    // (the `u`/`e`/`s` modifiers, a non-ASCII `\u{}` escape, or non-ASCII
    // source bytes) carries that encoding; an unpinned, 7-bit-only source
    // is US-ASCII.
    let enc = if re.fixed_encoding() {
        re.declared_encoding()
    } else {
        crate::value::Encoding::UsAscii
    };
    Ok(Value::string_from_inner(
        crate::value::rvalue::RStringInner::from_encoding_scanned(re.source_bytes(), enc),
    ))
}

///
/// ### Regexp#options
/// - options -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/options.html]
#[monoruby_builtin]
fn options(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regexp = self_.is_regex().unwrap();
    if !regexp.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    Ok(Value::integer(regexp.option() as i64))
}

///
/// ### Regexp#match?
/// - match?(str, pos = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/match=3f.html]
#[monoruby_builtin]
fn match_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    if !regex.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    let arg0 = lfp.arg(0);
    if arg0.is_nil() {
        return Ok(Value::bool(false));
    }
    // A subject String that is invalid in its own encoding can't be
    // matched — CRuby raises ArgumentError before scanning (as `match`
    // does here; this path used to read such a subject lossily instead).
    if let Some(inner) = arg0.is_rstring_inner()
        && !inner.is_valid_encoding()
    {
        return Err(MonorubyErr::argumenterr(format!(
            "invalid byte sequence in {}",
            inner.encoding().name()
        )));
    }
    check_subject_match_encoding(&globals.store, &regex, arg0)?;
    warn_binary_regexp_match(vm, globals, &regex, arg0);
    // A subject in a non-UTF-8 encoding Onigmo has a native codec for is
    // matched on its raw bytes, as `match` / `=~` do: read lossily, a
    // BINARY "\xC3" became U+FFFD and `/\xC3/n.match?` answered false.
    if let Some(rs) = arg0.is_rstring()
        && rs.code_range() != CodeRange::SevenBit
        && let Some(native_enc) = RegexpInner::onigmo_encoding_for(rs.encoding())
    {
        let byte_pos = if let Some(pos) = lfp.try_arg(1) {
            match conv_index(pos.coerce_to_int_i64(vm, globals)?, rs.char_length()) {
                Some(cp) => rs.iter_char_bytes().take(cp).map(|c| c.len()).sum(),
                None => return Ok(Value::bool(false)),
            }
        } else {
            0
        };
        let bytes = arg0.as_rstring_inner().as_bytes();
        return Ok(Value::bool(regex.match_pred_bytes(bytes, native_enc, byte_pos)?));
    }
    // Borrow a valid-UTF-8 String subject in place, as `match` / `=~` do;
    // anything else goes through `subject_to_string`. This used to read the
    // subject as an identifier, interning every string ever asked about —
    // on railsbench the request id and the session cookie, forever.
    let given_owned: String;
    let given: &str = match arg0.is_rstring() {
        Some(rs) if std::str::from_utf8(rs.as_bytes()).is_ok() => {
            // SAFETY: just validated as UTF-8.
            unsafe { std::str::from_utf8_unchecked(arg0.as_rstring_inner().as_bytes()) }
        }
        _ => {
            given_owned = subject_to_string(globals, arg0)?;
            &given_owned
        }
    };
    let char_pos = if let Some(pos) = lfp.try_arg(1) {
        match conv_index(pos.coerce_to_int_i64(vm, globals)?, given.chars().count()) {
            Some(pos) => pos,
            None => return Ok(Value::bool(false)),
        }
    } else {
        0
    };
    // CRuby's `Regexp#match?` is documented as "doesn't update
    // `$~` and friends". Route through the predicate helper so we
    // skip both the special-var save (`save_capture_special_variables`)
    // and the per-vm match-regex stash that `set_match_regex`
    // would otherwise leave behind for `Regexp.last_match`.
    let _ = vm;
    Ok(Value::bool(RegexpInner::match_pred(&regex, given, char_pos)?))
}

/// The subject of a match as an owned UTF-8 String, for the operands the
/// zero-copy path cannot borrow: a Symbol's name, or a String that is not
/// valid UTF-8, read lossily as before. Never interns — the subject is
/// arbitrary data, and a symbol is never collected.
/// `rb_reg_match`'s reading of a Symbol subject: the String it names,
/// US-ASCII when 7-bit and UTF-8 otherwise — which is what the
/// MatchData's strings then carry. Anything else is handed back as it
/// is.
fn symbol_as_string(subject: Value) -> Value {
    let Some(sym) = subject.try_symbol() else {
        return subject;
    };
    let name = sym.get_name();
    let enc = if name.is_ascii() {
        crate::value::Encoding::UsAscii
    } else {
        crate::value::Encoding::UTF8
    };
    Value::string_from_inner(RStringInner::from_encoding(name.as_bytes(), enc))
}

fn subject_to_string(store: &Store, subject: Value) -> Result<String> {
    if let Some(sym) = subject.try_symbol() {
        return Ok(sym.get_name());
    }
    if let Some(inner) = subject.is_rstring_inner() {
        return Ok(String::from_utf8_lossy(inner.as_bytes()).into_owned());
    }
    Err(MonorubyErr::is_not_symbol_nor_string(store, subject))
}

///
/// ### Regexp#match
///
/// - match(str, pos = 0) -> MatchData | nil
/// - [NOT SUPPORTED] match(str, pos = 0) {|m| ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/match=3f.html]
#[monoruby_builtin]
fn rmatch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.as_regexp();
    if !regex.initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    let arg0 = lfp.arg(0);
    // CRuby's `Regexp#match(nil)` clears `$~` and returns nil.
    if arg0.is_nil() {
        vm.clear_capture_special_variables();
        return Ok(Value::nil());
    }
    // A subject String that is invalid in its own encoding can't be
    // matched — CRuby raises ArgumentError before scanning.
    if arg0.is_rstring().is_some() {
        let inner = arg0.as_rstring_inner();
        if !inner.is_valid_encoding() {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid byte sequence in {}",
                inner.encoding().name()
            )));
        }
    }
    check_subject_match_encoding(&globals.store, &regex, arg0)?;
    warn_binary_regexp_match(vm, globals, &regex, arg0);
    // Subject declared in a non-UTF-8 encoding Onigmo has a native
    // codec for: compile the source under that codec and match the
    // raw bytes, so char boundaries follow the subject's *declared*
    // encoding (e.g. `/./s` on Windows-31J "\xC3\xA9" matches one
    // byte, even though those bytes happen to be valid UTF-8 too).
    // Pure 7-bit content decodes identically either way; keep it on
    // the fast zero-copy UTF-8 path.
    if let Some(rs) = arg0.is_rstring()
        && rs.code_range() != CodeRange::SevenBit
        && let Some(native_enc) = RegexpInner::onigmo_encoding_for(rs.encoding())
    {
        let byte_pos = if let Some(pos) = lfp.try_arg(1) {
            match conv_index(pos.coerce_to_int_i64(vm, globals)?, rs.char_length()) {
                // Convert char index -> byte offset by walking char
                // boundaries under the native encoding.
                Some(cp) => rs.iter_char_bytes().take(cp).map(|c| c.len()).sum(),
                None => return Ok(Value::nil()),
            }
        } else {
            0
        };
        vm.set_match_regex(self_);
        let bytes = arg0.as_rstring_inner().as_bytes();
        let md = if let Some(captures) =
            regex.captures_bytes_from_pos(bytes, arg0, native_enc, byte_pos, vm)?
        {
            Value::new_matchdata_bytes(&captures, arg0, regex)
        } else {
            Value::nil()
        };
        if !md.is_nil() {
            vm.set_backref(md);
        }
        if let Some(bh) = lfp.block() {
            if md.is_nil() {
                return Ok(Value::nil());
            }
            return vm.invoke_block_once(globals, bh, &[md]);
        }
        return Ok(md);
    }
    // Borrow the subject's bytes directly (and stash the Value for
    // the zero-copy MatchData snapshot) when it is a UTF-8 String;
    // Symbols and non-UTF-8 subjects fall back to the owned
    // conversion as before.
    let arg0 = symbol_as_string(arg0);
    let heystack_owned;
    let heystack: &str = match arg0.is_rstring() {
        Some(rs) if std::str::from_utf8(rs.as_bytes()).is_ok() => {
            vm.set_match_haystack(arg0);
            // SAFETY: just validated as UTF-8.
            unsafe { std::str::from_utf8_unchecked(arg0.as_rstring_inner().as_bytes()) }
        }
        _ => {
            heystack_owned = subject_to_string(globals, arg0)?;
            &heystack_owned
        }
    };
    let char_pos = if let Some(pos) = lfp.try_arg(1) {
        match conv_index(
            pos.coerce_to_int_i64(vm, globals)?,
            heystack.chars().count(),
        ) {
            Some(pos) => pos,
            // Out-of-range position ⇒ no match. CRuby returns
            // `nil` here (we used to return `false`, which mixed
            // poorly with downstream `nil`-checks).
            None => return Ok(Value::nil()),
        }
    } else {
        0
    };
    let byte_pos = match heystack.char_indices().nth(char_pos) {
        Some((pos, _)) => pos,
        None => 0, //return Ok(Value::bool(false)),
    };
    vm.set_match_regex(self_);
    let md = if let Some(captures) = regex.captures_from_pos(heystack, byte_pos, vm)? {
        // `captures_from_pos` has just saved this match as `$~` (with
        // the Regexp attached through the stash above), and that object
        // *is* the result — `regexp.match(s).equal?($~)` holds in CRuby
        // — so hand it back rather than building a second MatchData
        // with a second haystack view. A builtin always runs inside a
        // Ruby frame, so the svar container the save went to exists.
        let _ = captures;
        vm.current_match_data()
            .expect("`$~` was saved by captures_from_pos")
    } else {
        Value::nil()
    };
    // `Regexp#match(str) { |m| … }` block form: yield the
    // MatchData (or skip the block when there's no match) and
    // return whatever the block returned. CRuby calls the block
    // with `nil` when the match fails — but it returns `nil` to
    // the caller without yielding, so a missing `m` doesn't
    // surface inside the block.
    if let Some(bh) = lfp.block() {
        if md.is_nil() {
            return Ok(Value::nil());
        }
        return vm.invoke_block_once(globals, bh, &[md]);
    }
    Ok(md)
}

///
/// ### Regexp.try_convert
/// - try_convert(obj) -> Regexp | nil
///
/// Returns the argument if it is already a Regexp; otherwise calls
/// `to_regexp` and returns the result if it's a Regexp. Returns nil
/// when no conversion is possible. Raises TypeError if `to_regexp`
/// is defined but returns a non-Regexp.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/try_convert.html]
#[monoruby_builtin]
fn regexp_try_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_regex().is_some() {
        return Ok(arg);
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if result.is_nil() {
            return Ok(Value::nil());
        }
        if result.is_regex().is_some() {
            return Ok(result);
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} to Regexp ({}#to_regexp gives {})",
            arg.get_real_class_name(&globals.store),
            arg.get_real_class_name(&globals.store),
            result.get_real_class_name(&globals.store),
        )));
    }
    Ok(Value::nil())
}

///
/// ### Regexp.linear_time?
/// - linear_time?(re) -> bool
/// - linear_time?(string, options=0) -> bool
///
/// Whether a match of the pattern is bounded by the engine's match
/// cache, and so runs in time linear in the subject length however the
/// subject is chosen.
///
/// The answer comes from Onigmo, which walks the compiled program for
/// the constructs the cache cannot memoize across — back-references,
/// subexpression calls (`\g<...>`), the absent operator, a capture group
/// inside a look-around that compiles to a push, and nested repeats. It
/// is not a syntactic test on the source: `/.(?=(a))/` is false while
/// `/.(?<=(a))/` is true, which no reading of the source alone gives.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/linear_time=3f.html]
#[monoruby_builtin]
fn regexp_linear_time_p(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    // If a Regexp is given, options on the second arg are ignored by
    // CRuby (with a warning).
    if let Some(re) = arg.is_regex() {
        if lfp.try_arg(1).is_some() {
            warn_flags_ignored(vm, globals);
        }
        return Ok(Value::bool(re.is_linear_time()));
    }
    // Otherwise, compile the source to validate the pattern. The
    // option arg may be Integer/String/nil/Boolean — we only need
    // it for error parity, so pass nil-equivalent through unchanged.
    let s = arg.coerce_to_string(vm, globals)?;
    let opt = if let Some(o) = lfp.try_arg(1) {
        if o.is_nil() {
            0
        } else if let Some(i) = o.try_fixnum() {
            i as u32
        } else if let Some(s) = o.is_str() {
            parse_option_string(s.as_ref())?
        } else if o == Value::bool(true) {
            onigmo_regex::ONIG_OPTION_IGNORECASE
        } else {
            0
        }
    } else {
        0
    };
    let re = RegexpInner::with_option(s, opt)?;
    Ok(Value::bool(re.is_linear_time()))
}

/// The `timeout:` keyword of `Regexp.new` / `Regexp#initialize`.
const TIMEOUT_KW: &str = "timeout";

// A regexp's own timeout lives on its `RegexpInner`, and the global one
// in `REGEXP_GLOBAL_TIMEOUT` beside it: the matcher is what consults
// them, and the matcher only ever holds a `RegexpInner`. Both are
// enforced now — the vendored Onigmo calls an embedder hook from inside
// its match loop (`CHECK_INTERRUPT_IN_MATCH_AT`, which a non-Ruby build
// used to compile away) and `onigmo-regex` turns that into a per-thread
// deadline (#1423).
use crate::value::rvalue::regexp::REGEXP_GLOBAL_TIMEOUT;

/// A timeout in seconds as the `u64` of nanoseconds CRuby keeps, which
/// is what makes its accessors quantize the way they do: a value below
/// one nanosecond becomes 0, which *is* the unset marker, so it reads
/// back as `nil`, and one past `u64::MAX` nanoseconds saturates at
/// about 18446744073.7 seconds. Rust's `as` does both on its own.
///
/// A NaN is the one value CRuby has no answer for: converting it to an
/// unsigned integer is undefined in C, and the platforms disagree —
/// glibc lands on `i64::MAX` (so `Regexp.timeout` reads back
/// 9223372036.854776) where macOS lands on zero (so it reads back
/// `nil`). Rust's saturating cast gives zero, which is the answer that
/// at least means something: no timeout.
fn timeout_nanos(sec: f64) -> u64 {
    (sec * 1_000_000_000f64) as u64
}

/// The Ruby value a stored nanosecond count reads back as.
fn timeout_value(nanos: u64) -> Value {
    if nanos == 0 {
        Value::nil()
    } else {
        Value::float(nanos as f64 / 1_000_000_000f64)
    }
}

/// Coerce and validate a timeout argument: `nil` clears, anything else
/// is coerced to a Float and must be positive. CRuby spells the
/// rejected value with `#to_s`, not `#inspect`.
fn timeout_arg(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<u64> {
    if arg.is_nil() {
        return Ok(0);
    }
    let sec = arg.coerce_to_f64(vm, globals)?;
    if sec <= 0.0 {
        return Err(MonorubyErr::argumenterr(format!(
            "invalid timeout: {}",
            arg.to_s(&globals.store)
        )));
    }
    Ok(timeout_nanos(sec))
}

/// Record a `Regexp.new` `timeout:` on the freshly built regexp. A
/// timeout that quantized to nothing is no timeout, so nothing is
/// stored and `#timeout` answers nil — as it does in CRuby.
fn store_regexp_timeout(_globals: &mut Globals, mut regexp: Value, nanos: u64) -> Result<()> {
    if nanos != 0 {
        regexp.as_regexp_inner_mut().set_timeout_nanos(nanos);
    }
    Ok(())
}

///
/// ### Regexp#timeout
/// - timeout -> Float | nil
///
/// The timeout this regexp was built with (`Regexp.new(src, timeout:)`),
/// or `nil` when it has none of its own. It does *not* fall back to the
/// global `Regexp.timeout`, which is consulted separately at match time.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/timeout.html]
#[monoruby_builtin]
fn regexp_inst_timeout(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if !lfp.self_val().as_regexp_inner().initialized() {
        return Err(MonorubyErr::typeerr("uninitialized Regexp"));
    }
    Ok(timeout_value(
        lfp.self_val().as_regexp_inner().timeout_nanos(),
    ))
}

///
/// ### Regexp.timeout
/// - timeout -> Float | nil
///
/// Returns the global timeout (a Float), or `nil` when unset.
#[monoruby_builtin]
fn regexp_timeout_get(
    _: &mut Executor,
    _: &mut Globals,
    _: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(timeout_value(REGEXP_GLOBAL_TIMEOUT.with(|t| t.get())))
}

///
/// ### Regexp.timeout=
/// - timeout=(sec) -> sec
///
/// Stores the global timeout (`nil` clears it); zero or negative raises
/// `ArgumentError` and leaves the old value standing. Answers the
/// argument, not the stored value.
#[monoruby_builtin]
fn regexp_timeout_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let nanos = timeout_arg(vm, globals, arg)?;
    REGEXP_GLOBAL_TIMEOUT.with(|t| t.set(nanos));
    Ok(arg)
}

///
/// ### Regexp#~
/// - ~ self -> Integer | nil
///
/// Sugar for `self =~ $_`.
#[monoruby_builtin]
fn regexp_tilde(
    vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `$_` is frame-local; read it from the calling scope's LEP
    // (the `~` builtin's own native frame is skipped).
    let target = vm.get_last_read_line();
    if target.is_nil() {
        vm.clear_capture_special_variables();
        return Ok(Value::nil());
    }
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let s = match target.is_str() {
        Some(s) => s.to_string(),
        None => return Ok(Value::nil()),
    };
    let res = match regex.find_one(vm, &s)? {
        Some(mat) => Value::integer(mat.start as i64),
        None => Value::nil(),
    };
    Ok(res)
}

///
/// ### Regexp#casefold?
/// - casefold? -> bool
///
/// True if the IGNORECASE flag is set.
#[monoruby_builtin]
fn casefold_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let on = regex.option() & onigmo_regex::ONIG_OPTION_IGNORECASE != 0;
    Ok(Value::bool(on))
}

///
/// ### Regexp#encoding
/// - encoding -> Encoding
///
/// Returns the source encoding of the Regexp. monoruby tracks two
/// classes (UTF-8 and ASCII); Onigmo's `ASCII` mode corresponds to
/// either US-ASCII (when the source has no non-ASCII bytes) or
/// ASCII-8BIT (when it does).
#[monoruby_builtin]
fn encoding(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let enc = regexp_encoding_value(globals, &regex);
    Ok(enc)
}

///
/// ### Regexp#fixed_encoding?
/// - fixed_encoding? -> bool
///
/// True if the regex has a fixed encoding (FIXEDENCODING option set,
/// or the source contains non-ASCII bytes that pin the encoding).
#[monoruby_builtin]
fn fixed_encoding_p(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    Ok(Value::bool(regex.fixed_encoding()))
}

///
/// ### Regexp#named_captures
/// - named_captures -> Hash
///
/// Returns a Hash mapping each capture group name to an Array of
/// the indexes that name refers to (capture-name aliasing in Onigmo).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/named_captures.html]
#[monoruby_builtin]
fn named_captures(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.as_regexp_inner();
    let raw = regex.capture_names().unwrap_or_default();
    let mut map = RubyMap::default();
    let mut seen: Vec<String> = Vec::with_capacity(raw.len());
    for name in raw {
        if seen.iter().any(|n| *n == name) {
            continue;
        }
        let members = regex.get_group_members(&name);
        let arr = Value::array_from_iter(members.iter().map(|i| Value::integer(*i as i64)));
        let key = Value::string_from_str(&name);
        map.insert(key, arr, vm, globals)?;
        seen.push(name);
    }
    Ok(Value::hash(map))
}

/// Resolve the `Encoding::<NAME>` Value for a `RegexpInner`.
/// Reads the declared (CRuby-visible) encoding `RegexpInner` set
/// at construction time — this honours `n`/`u`/`e`/`s` modifiers
/// and the source-string's own encoding tag, which the older
/// "infer from `OnigmoEncoding` + content scan" path couldn't
/// distinguish.
fn regexp_encoding_value(globals: &Globals, regex: &RegexpInner) -> Value {
    let enc_class = encoding::encoding_class(globals);
    // An allocated-but-uninitialized Regexp (`Regexp.allocate`) has no
    // source yet; CRuby reports its encoding as BINARY (ASCII-8BIT).
    let encoding = if regex.initialized() {
        regex.declared_encoding()
    } else {
        crate::value::Encoding::Ascii8
    };
    let const_name = encoding::encoding_constant_name(encoding);
    globals
        .store
        .get_constant_noautoload(enc_class, IdentId::get_id(const_name))
        .unwrap_or(Value::nil())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn a_broken_pattern_is_refused_before_the_receiver() {
        // CRuby turns a String pattern into a Regexp *before* it
        // negotiates the two encodings, so a pattern broken in its own
        // encoding is refused first. `#split` walks the separator's
        // own characters first for the same reason and had the two
        // checks the other way round; `#sub`, `#gsub` and `#scan`
        // already run in that order. `#index` and `#partition`
        // genuinely do check compatibility first, and stay as they are
        // — the order is per method, which is what this pins (#1522).
        run_test_once(
            r##"
              def sj(s) = s.dup.force_encoding("Shift_JIS")
              [
                (("あ".sub(sj("\x81"), "z")) rescue "#{$!.class}: #{$!.message}"),
                (("あ".gsub(sj("\x81"), "z")) rescue "#{$!.class}: #{$!.message}"),
                (("あ".scan(sj("\x81"))) rescue "#{$!.class}: #{$!.message}"),
                (("あ".split(sj("\x81"))) rescue "#{$!.class}: #{$!.message}"),
                (("あ".index(sj("\x81"))) rescue "#{$!.class}: #{$!.message}"),
                (("あ".partition(sj("\x81"))) rescue "#{$!.class}: #{$!.message}"),
                ((sj("\x82\xa0").sub(sj("\x81"), "z")) rescue "#{$!.class}: #{$!.message}"),
                ((sj("\x82\xa0").index(sj("\x81"))) rescue "#{$!.class}: #{$!.message}"),
                (("あ".split(sj("\x82\xa0"))) rescue "#{$!.class}: #{$!.message}"),
              ]
            "##,
        );
    }

    #[test]
    fn a_broken_source_names_the_character_not_the_byte() {
        // `rb_reg_initialize` refuses a source broken in its own
        // encoding with one message whatever the encoding — which
        // `Regexp.new` now gives; this pins it over the shapes the
        // constructor can be reached with, since the check sits on one
        // path and every kind of broken source has to reach it.
        run_test_once(
            r##"
              def sj(s) = s.dup.force_encoding("Shift_JIS")
              def ej(s) = s.dup.force_encoding("EUC-JP")
              [
                (Regexp.new("\xff".dup.force_encoding("UTF-8")) rescue "#{$!.class}: #{$!.message}"),
                (Regexp.new("\xff".dup.force_encoding("US-ASCII")) rescue "#{$!.class}: #{$!.message}"),
                (Regexp.new(ej("\xa4")) rescue "#{$!.class}: #{$!.message}"),
                (Regexp.new(ej("a\xa4b")) rescue "#{$!.class}: #{$!.message}"),
                (Regexp.new(ej("(\xa4")) rescue "#{$!.class}: #{$!.message}"),
                (Regexp.new(sj("\x81")) rescue "#{$!.class}: #{$!.message}"),
                # A BINARY source is never broken, and a valid one of
                # any encoding still compiles.
                Regexp.new("\xff".dup.force_encoding("ASCII-8BIT")).inspect,
                Regexp.new(ej("\xa4\xa2")).inspect,
                /\xff/n.inspect,
              ]
            "##,
        );
        // A surrogate is in the BMP range but names no character, so it
        // is refused the way a codepoint past `U+10FFFF` is — Onigmo
        // accepts the four digits, so the check cannot be left to it.
        run_test_once(
            r##"
              %w[\ud800 \udfff ퟿ ].map { |t|
                (Regexp.new(t).inspect rescue "#{$!.class}: #{$!.message}") } +
              ['\u{d800}', '\u{dfff}', '\u{d7ff}', '\u{110000}', '\u{10ffff}', '\u{d800 41}'].map { |t|
                (Regexp.new(t).inspect rescue "#{$!.class}: #{$!.message}") }
            "##,
        );
    }

    #[test]
    fn match_returns_the_saved_backref() {
        // `String#match` / `Regexp#match` hand back the very MatchData
        // they saved as `$~` (no second object), block form included.
        run_test(
            r##"
            r = []
            m = "abc".match(/b/); r << m.equal?($~) << m[0] << m.pre_match << m.post_match
            m = /(b)(c)/.match("abc"); r << m.equal?($~) << m[2] << $2
            m = "abc".match(/b/) { |x| x }; r << m.equal?($~)
            m = "abc".match(/z/); r << m << $~
            m = "xabc".match(/b/, 1); r << m.equal?($~) << m.begin(0)
            m = /(?<n>c)/.match("abc"); r << m[:n] << $~[:n] << m.regexp.source
            m = "abc".match("b"); r << m.equal?($~) << m[0]
            r
            "##,
        );
    }

    #[test]
    fn gsub_block_without_a_match_probes_first() {
        // With no match the block never runs, so `gsub` skips the frozen
        // snapshot and returns a plain copy; `$~` is cleared, the copy is
        // unfrozen and independent, and a receiver mutated inside the
        // block on a *matching* call is still detected.
        run_test(
            r##"
            r = []
            s = +"example.com"
            t = s.gsub(/[^a-z.]/) { |c| "%20" }
            r << t << t.equal?(s) << t.frozen? << $~
            t << "x"; r << s
            u = "a b c".gsub(/ /) { |c| $~[0] == " " ? "%20" : "?" }
            r << u << $~.nil?
            v = +"a b"
            begin
              v.gsub(/ /) { |c| v << "!" ; "-" }
              r << "no error"
            rescue RuntimeError => e
              r << e.message
            end
            r << "".gsub(/x/) { "y" } << "".gsub(//) { "y" } << "ab".gsub(//) { "-" }
            w = "あ い".gsub(/ /) { "_" }; r << w << w.encoding.name
            r
            "##,
        );
    }
    #[test]
    fn regex() {
        run_tests(&[
            r##"/abcde/xmi.to_s"##,
            r##"/abcde/xmi.inspect"##,
            r##"/abcde/.to_s"##,
            r##"/abcde/.inspect"##,
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match[0], Regexp.last_match[1], Regexp.last_match[2], Regexp.last_match[3]]
        "#,
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3), Regexp.last_match(-1), Regexp.last_match(-2), Regexp.last_match(-10)]
        "#,
            r#"
          /(.)(.)/ =~ "abcde"
          [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3)]
            "#,
            // Regexp.last_match returns nil when the last match failed
            r#"
          /NOMATCH/ =~ "abc"
          Regexp.last_match
            "#,
            // Regexp.last_match returns nil when no match has occurred yet
            r#"
          /DONTMATCH/.match("")
          Regexp.last_match
            "#,
            // $~ reads match data after a regex match
            r#"
          "abc" =~ /(a)(b)/
          $~[0]
            "#,
            // $~ = nil clears match data
            r#"
          "abc" =~ /(a)/
          $~ = nil
          $~
            "#,
            // $~ is nil initially after a failed match
            r#"
          /NOMATCH/ =~ "abc"
          $~
            "#,
            r#"
          /(.)(.)/ =~ "abcde"
          #assert $', Regexp.post_match
          [$', $&, $1, $2, $3]
            "#,
            r#"
          /(.)(.)/ =~ :abcde
          [$', $&, $1, $2, $3]
            "#,
            r#"
          /(.)(.)/ =~ nil
          [$', $&, $1, $2, $3]
            "#,
            r##"Regexp.union(/g/i, "a(b)[c]d", /bbbb/x, /cccc/m).to_s"##,
            r##"/foo/.options"##,
            r##"/foo/i.options"##,
            r##"/foo/m.options"##,
            r##"/foo/x.options"##,
            r##"/foo/mix.options"##,
            r#""abcdefg".gsub(/def/, "!!")"#,
            r#""2.5".gsub(".", ",")"#,
            r#""xbbgz-xbbbvzbbc".gsub(/(b+.z)(..)/) { $2 + $1.upcase }"#,
            r#"
            res = /(aa).*(bb)/ === "andaadefbbje"
            [res, $&, $1, $2]
        "#,
            r#"
            res = /(aa).*(bb)/ === :andaadefbbje
            [res, $&, $1, $2]
        "#,
            r#"
            a = "HELLO"
            case a
            when /\A[a-z]*\z/
                "Lower case"
            when /\A[A-Z]*\z/
                "Upper case"
            else
                "Mixed case"
            end
        "#,
            r#""aaazzz" =~ /\172+/"#,
            r#"/foo/ =~ "foo""#,
            r#"/foo/ =~ "afoo""#,
            r#"/foo/ =~ "bar""#,
            r#"
            i = 123
            /ab#{i}cd/ =~ "ab123cd"
        "#,
            r#"
        a = /Ruby\Z/
        ["Ruby" =~ /Ruby\Z/, "Rubys" =~ /Ruby\Z/]
        "#,
        ]);
    }

    #[test]
    fn regexp_error1() {
        run_test_error(r#"/+/"#);
    }

    #[test]
    fn regexp_error2() {
        run_test_error(r#"Regexp.new("+")"#);
    }

    #[test]
    fn query_apis_do_not_intern_their_string() {
        // A symbol is never collected, so `Regexp#match?` reading its subject
        // as an identifier (and `autoload?`, a named-capture reference
        // reading their argument that way) grew the symbol table on every
        // distinct string ever asked about — on railsbench, the request id
        // and the session cookie of every request. CRuby's `rb_check_id`
        // looks up without interning; so do these now. (The symbol table
        // is process-wide and other tests intern in parallel, so each
        // snippet counts its own prefix rather than the table's size.)
        run_tests(&[
            r#"r = /\A[\w\-]{1,255}\z/; 3000.times { |i| r.match?("qmatch_#{i}") }; Symbol.all_symbols.count { |s| s.start_with?("qmatch_") }"#,
            r#"r = /x/; 3000.times { |i| "qeq_#{i}" =~ r; r.match("qeq2_#{i}"); r =~ "qeq3_#{i}" }; Symbol.all_symbols.count { |s| s.start_with?("qeq") }"#,
            r#"3000.times { |i| Object.autoload?("Qauto#{i}"); autoload?("Qauto2#{i}") }; Symbol.all_symbols.count { |s| s.start_with?("Qauto") }"#,
            r#"m = /(?<name>a)/.match("a"); 3000.times { |i| (m["qgrp_#{i}"] rescue nil); (m.begin("qgrp_#{i}") rescue nil); (m.values_at("qgrp_#{i}") rescue nil) }; Symbol.all_symbols.count { |s| s.start_with?("qgrp_") }"#,
            // Symbol subjects and names still work, and non-UTF-8 subjects
            // are read as before.
            r#"[/x/.match?(:sym_x), /y/.match?(:sym_x), (/a/.match?(1) rescue $!.class), /a/ =~ :xa, /b/.match(:abc)&.begin(0)]"#,
            r#"[/a/.match?("\xE9a".b), /a/.match?("\x82\xa0a".dup.force_encoding("Shift_JIS")), (/a/.match?("\xff".dup.force_encoding("UTF-8")) rescue $!.class)]"#,
            r#"m = /(?<name>a)/.match("a"); [m[:name], m["name"], (m["nope"] rescue $!.class), m.begin("name"), m.end(:name), m.values_at("name"), (m.begin("nope") rescue $!.class)]"#,
            r#"m = /(?<name>a)/.match("a"); [m.match("name"), m.match(:name), (m.match("nope") rescue $!.class), m.match_length("name"), m.match_length(:name), (m.match_length("nope") rescue $!.class)]"#,
            r#"[Object.autoload?("NoSuchConstZz"), Object.autoload?(:NoSuchConstZz2), autoload?("NoSuchConstZz3")]"#,
            // A name that is not UTF-8 is looked up among the raw-bytes
            // symbols: absent, then present once `to_sym` made one.
            r#"o = Object.new; b = "\xff\xfe".b; sj = "\x82\xa0".dup.force_encoding("Shift_JIS"); r1 = [o.respond_to?(b), Object.autoload?(b), autoload?(sj), (/(?<name>a)/.match("a")[b] rescue $!.class)]; b.to_sym; sj.to_sym; r1 + [o.respond_to?(b), o.respond_to?(sj, true), Object.autoload?(b), autoload?(sj)]"#,
        ]);
    }

    #[test]
    fn respond_to_with_a_string_no_symbol_exists_for() {
        // CRuby's `respond_to?` has two shapes of the `respond_to_missing?`
        // call: a name that is a symbol goes `(Symbol, bool)` and the answer
        // is truthified; a String no symbol existed for is interned and
        // passed with the second argument *as given* (nil when omitted),
        // and the override's answer comes back as is.
        run_tests(&[
            r#"o = Object.new; [o.respond_to?("to_s"), o.respond_to?("no_such_zz"), o.respond_to?(:to_s), o.respond_to?("no_such_zz", true), (o.respond_to?(1) rescue $!.class)]"#,
            // (A fresh name each time: once interned, the name takes the
            // symbol shape on the next call — in CRuby too.)
            r#"class RtmQ; def respond_to_missing?(n, p) = [n.class, n.to_s.start_with?("never_defined_zz"), p]; end; $rtmq = ($rtmq || 0) + 1; q = RtmQ.new; [q.respond_to?("never_defined_zz_a#{$rtmq}"), q.respond_to?("never_defined_zz_b#{$rtmq}", 7), q.respond_to?(:never_defined_zz_2), q.respond_to?(:never_defined_zz_2, 7), q.respond_to?("to_s")]"#,
            r#"class RtmN; def respond_to_missing?(n, p) = nil; end; class RtmT; def respond_to_missing?(n, p) = 42; end; [RtmN.new.respond_to?("never_defined_zz_3"), RtmN.new.respond_to?(:never_defined_zz_4), RtmT.new.respond_to?("never_defined_zz_5"), RtmT.new.respond_to?(:never_defined_zz_6)]"#,
            // An exception from the override propagates in both shapes.
            r#"class RtmE; def respond_to_missing?(n, p) = raise(ArgumentError, "rtm #{n.class}"); end; e = RtmE.new; $rtme = ($rtme || 0) + 1; [(e.respond_to?(:never_defined_zz_7) rescue $!.message), (e.respond_to?("never_defined_zz_e#{$rtme}") rescue $!.message), (e.respond_to?("never_defined_zz_e#{$rtme}b", true) rescue $!.class)]"#,
        ]);
        // CRuby interns the name in that second shape too (`rb_to_symbol`);
        // monoruby does not when `respond_to_missing?` is the default or
        // answers a literal, since such a body never reads it. CRuby's
        // count is collector-dependent, so this side is checked alone.
        let res = run_test_no_result_check(
            r#"
            class RtmC; def respond_to_missing?(n, p) = false; end
            o = Object.new; c = RtmC.new
            3000.times { |i| o.respond_to?("qrtm_#{i}"); o.respond_to?("qrtm_#{i}", true); c.respond_to?("qrtm_#{i}") }
            Symbol.all_symbols.count { |s| s.start_with?("qrtm_") }
            "#,
        );
        assert_eq!(res.try_fixnum(), Some(0));
    }

    #[test]
    fn match_() {
        run_tests(&[
            r#"
        res = [/R.../.match?("-Ruby")]
        for i in -6...6
            res << /R.../.match?("-Ruby", 0)
        end
        res
        "#,
            r#"
        sep_pat = /#{Regexp.quote File::SEPARATOR}/
        /\A#{sep_pat}?\z/.match?("")
        "#,
            r#"/foo/.match?(nil)"#,
            r#"/foo/.match?(nil, 0)"#,
            r#"//.match?(nil)"#,
            r##"/(.).(.)/.match("foobar", 3).captures"##,
            r##"/(.).(.)/.match("foobar", -3).captures"##,
            // No named captures → empty array
            r##"/(foo)(bar)/.names"##,
            // Single named capture
            r##"/(?<a>foo)/.names"##,
            // Multiple named captures preserve declaration order
            r##"/(?<a>foo)(?<b>bar)/.names"##,
            // Mixed named + unnamed
            r##"/(?<a>foo)(bar)(?<c>baz)/.names"##,
            // Duplicate names appear once each in declaration order
            r##"/(?<a>foo)(?<a>bar)/.names"##,
            // Same source + same options.
            r#"/abc/ == /abc/"#,
            r#"/abc/.eql?(/abc/)"#,
            r#"/abc/i == /abc/i"#,
            r#"/abc/i == /abc/"#,
            r#"/abc/.hash == /abc/.hash"#,
            r#"/abc/i.hash == /abc/i.hash"#,
            // Different source.
            r#"/abc/ == /abd/"#,
            // /n flag (NOENCODING) doesn't change == for ASCII source.
            r#"// == //n"#,
            r#"//.hash == //n.hash"#,
            // Non-Regexp argument is never equal.
            r#"/abc/ == "abc""#,
            // Empty args → /(?!)/ (never matches).
            r#"Regexp.union == /(?!)/"#,
            // Single Regexp arg returned as-is (preserves identity via ==).
            r#"Regexp.union(/foo/) == /foo/"#,
            // Symbol accepted.
            r#"Regexp.union(:foo) == /foo/"#,
            // Strings escaped.
            r#"Regexp.union("n", ".") == /n|\./"#,
            // Single Array arg flattened.
            r#"Regexp.union(["+", "-"]) == /\+|\-/"#,
            r#"Regexp.union(["skiing", "sledding"]) == /skiing|sledding/"#,
            // `Regexp.new(/abc/i)` keeps both source and options.
            r#"Regexp.new(/abc/i) == /abc/i"#,
            r#"Regexp.new(/^hi{2,3}fo.o$/) == /^hi{2,3}fo.o$/"#,
            // `i`, `m`, `x` accepted as a flag string.
            r#"Regexp.new("Hi", "i") == /Hi/i"#,
            r#"Regexp.new("Hi", "im") == /Hi/im"#,
            r#"(Regexp.new("Hi", "im").options & Regexp::IGNORECASE) != 0"#,
            // The `x` (EXTENDED) flag in the string form sets EXTENDED
            // and ignores whitespace / `#`-comments inside the pattern.
            r#"Regexp.new("Hi", "x") == /Hi/x"#,
            r#"(Regexp.new("Hi", "x").options & Regexp::EXTENDED) != 0"#,
            // EXTENDED skips inline whitespace, so a pattern with spaces
            // still matches a no-space subject (returns the match position).
            r#"Regexp.new("a b c", "x") =~ "abc""#,
            // Combined with other flags.
            r#"Regexp.new("Hi", "ix") == /Hi/ix"#,
            r#"Regexp.new("Hi", "mix") == /Hi/mix"#,
        ]);
    }

    #[test]
    fn match_named_capture_locals() {
        // `/literal/ =~ str` with named captures binds each capture to a
        // local variable named after it (the value, or `nil` when that
        // group did not participate in the match). The snippets that
        // inspect `local_variables` are wrapped in a method so the scope
        // is isolated from the test harness's own top-level locals.
        run_test(
            r#"
            def m
              /(?<matched>foo)(?<unmatched>bar)?/ =~ "foofoo"
              [local_variables, matched, unmatched]
            end
            m
        "#,
        );
        // No match: every named-capture local becomes `nil`, but is
        // still declared.
        run_test(
            r#"
            def m
              /(?<a>foo)/ =~ "nope"
              [local_variables, a]
            end
            m
        "#,
        );
        // The whole expression evaluates to the `=~` result (match
        // position or `nil`), independent of any `nil` capture value.
        run_test(r#"/(?<z>foo)/ =~ "foofoo""#);
        run_test(r#"/(?<z>foo)(?<opt>bar)?/ =~ "foofoo""#);
        run_test(r#"/(?<z>foo)/ =~ "nope""#);
        // A capture matching an existing local in an enclosing scope
        // assigns to that outer local rather than a fresh block-local.
        run_test(
            r#"
            def m
              a = 42
              1.times { /(?<a>foo)/ =~ "foofoo" }
              a
            end
            m
        "#,
        );
        // Regexp on the *right*, a regexp *variable*, and an explicit
        // method call all bind no locals.
        run_test(
            r#"
            def m
              "foofoo" =~ /(?<matched>foo)/
              local_variables
            end
            m
        "#,
        );
        run_test(
            r#"
            def m
              re = /(?<matched>foo)/
              re =~ "foofoo"
              local_variables
            end
            m
        "#,
        );
        run_test(
            r#"
            def m
              /(?<matched>foo)/.=~("foofoo")
              local_variables
            end
            m
        "#,
        );
        // Used as a condition: a match is truthy, a no-match falsy.
        run_test(r#"if /(?<w>foo)/ =~ "foofoo" then "hit" else "miss" end"#);
        run_test(r#"if /(?<w>foo)/ =~ "nope" then "hit" else "miss" end"#);
    }

    #[test]
    fn regexp_new_invalid_flag_string_raises() {
        // `e` is *not* accepted in the string-form flag arg.
        run_test_error(r#"Regexp.new("Hi", "e")"#);
        run_test_error(r#"Regexp.new("Hi", "z")"#);
    }

    #[test]
    fn regexp_unterminated_unicode_property_raises() {
        // `\p{name}` / `\P{name}` must be closed; an unterminated one is a
        // RegexpError (Onigmo would otherwise accept it silently).
        run_test_error(r#"Regexp.new('\p{')"#);
        run_test_error(r#"Regexp.new('\p{Wor')"#);
        run_test_error(r#"Regexp.new('\P{')"#);
        // A closed property still compiles and matches.
        run_test(r#"[/\p{Word}/.match("a").to_a, /\p{Alpha}+/.match("abc").to_a, /\p{^L}/.match("1").to_a]"#);
    }

    #[test]
    fn regexp_new_unexpected_option_warns_and_treats_as_truthy() {
        // A non-Integer / non-String / non-bool / non-nil second
        // argument writes a `warning: expected true or false as
        // ignorecase: <inspect>` line to `$stderr` and treats the
        // argument as truthy → IGNORECASE.
        //
        // The assertion compares the resulting Regexp to `/Hi/i` so
        // the truthy-coercion is observable; the warning text itself
        // ends up on the test's stderr stream and is verified
        // indirectly by the spec battery.
        run_tests(&[
            r#"Regexp.new("Hi", Object.new) == /Hi/i"#,
            r#"Regexp.new("Hi", []) == /Hi/i"#,
            r#"Regexp.new("Hi", :sym) == /Hi/i"#,
            // Even an Object that masquerades as falsey via `to_s` still
            // triggers the warning + IGNORECASE path.
            r#"
              o = Object.new
              def o.inspect; "fake"; end
              Regexp.new("Hi", o) == /Hi/i
            "#,
            // `/` not already escaped is escaped in inspect.
            r#"Regexp.new("/foo/bar").inspect"#,
            r#"Regexp.new("//").inspect"#,
            // Already-escaped `\/` is not double-escaped.
            r#"/\/foo\/bar/.inspect"#,
            // `n` flag (NOENCODING) appears in inspect output.
            r#"//n.inspect"#,
            r#"//nixm.inspect"#,
            r#"Regexp::IGNORECASE"#,
            r#"Regexp::EXTENDED"#,
            r#"Regexp::MULTILINE"#,
            r#"Regexp::FIXEDENCODING"#,
            r#"Regexp::NOENCODING"#,
        ]);
    }

    #[test]
    fn regexp_try_convert() {
        run_tests(&[
            // Already a Regexp -> returned as-is.
            r#"Regexp.try_convert(/abc/) == /abc/"#,
            // Non-Regexp without #to_regexp -> nil.
            r#"Regexp.try_convert("abc")"#,
            r#"Regexp.try_convert(nil)"#,
            r#"Regexp.try_convert(123)"#,
            // Object that defines #to_regexp -> returns the Regexp.
            r#"
            o = Object.new
            def o.to_regexp; /xyz/; end
            Regexp.try_convert(o) == /xyz/
            "#,
        ]);
        // #to_regexp returning a non-Regexp -> TypeError.
        run_test_error(
            r#"
            o = Object.new
            def o.to_regexp; "not a regexp"; end
            Regexp.try_convert(o)
            "#,
        );
    }

    #[test]
    fn regexp_linear_time_p() {
        // The answer comes from the engine walking the compiled
        // program, so it is about what the match cache can memoize
        // across. Back-references, subexpression calls, the absent
        // operator and a capture inside a look-around that compiles to
        // a push all defeat it; look-behind, atomic groups and
        // possessive quantifiers do not.
        run_tests(&[
            r#"Regexp.linear_time?(/abc/)"#,
            r#"Regexp.linear_time?("abc")"#,
            r#"Regexp.linear_time?("abc", Regexp::IGNORECASE)"#,
            r#"Regexp.linear_time?(/(a)\1/)"#,
            r#"Regexp.linear_time?("(a)\\1")"#,
            r#"Regexp.linear_time?(/a*(?:(?=a*)a)*b/)"#,
            r#"Regexp.linear_time?(/a*(?:(?<=a)a*)*b/)"#,
            r#"Regexp.linear_time?(/.(?<=(a))/)"#,
            r#"Regexp.linear_time?(/(?<a>a){0}\g<a>/)"#,
            r#"Regexp.linear_time?(/[\x80-\xff]/n)"#,
            // A capture inside a look-*ahead* is not linear-time, while
            // the same capture inside a look-behind (above) is. No
            // reading of the source alone separates those two.
            r#"Regexp.linear_time?(/.(?=(a))/)"#,
            // The absent operator.
            r#"Regexp.linear_time?(/(?~abc)/)"#,
            r#"Regexp.linear_time?(/x(?~y)z/)"#,
            // A named back-reference.
            r#"Regexp.linear_time?(/(?<x>a)\k<x>/)"#,
            // The patterns the cache exists for.
            r#"Regexp.linear_time?(/^(a*)*$/)"#,
            r#"Regexp.linear_time?(/^(a|a)*$/)"#,
            r#"Regexp.linear_time?(/(x+x+)+y/)"#,
            r#"Regexp.linear_time?(/(?>a*)*b/)"#,
            r#"Regexp.linear_time?(/^(([a-z])+.)+[A-Z]([a-z])+$/)"#,
            // Nested repeats. A small bounded repeat is expanded by
            // Onigmo rather than compiled to OP_REPEAT, so it stays
            // linear; past that threshold the nesting is what the cache
            // cannot handle.
            r#"Regexp.linear_time?(/(?:a{1,2}){1,3}/)"#,
            r#"Regexp.linear_time?(/a{100,200}/)"#,
            r#"Regexp.linear_time?(/(?:a{10,20})+/)"#,
            r#"Regexp.linear_time?(/(?:a{20,30}){20,30}/)"#,
            r#"Regexp.linear_time?(/(a{100,200})*/)"#,
        ]);
        // Flags are ignored (with a warning) for a Regexp argument.
        run_test_no_result_check(
            r#"Regexp.linear_time?(/a/, Regexp::IGNORECASE)"#,
        );
    }

    #[test]
    fn linear_time_patterns_do_not_run_away() {
        // Each of these used to backtrack exponentially: the match is
        // one `onig_search` call, so before the engine memoized there
        // was nothing that could interrupt it and a 40-character
        // subject did not finish. `Regexp.linear_time?` says true for
        // all of them, and this is that promise being kept.
        run_tests(&[
            r#"/^(a*)*$/ =~ ("a" * 40 + "b")"#,
            r#"/^(a*)*$/.match("a" * 40).to_a"#,
            r#"/^(a|a)*$/ =~ ("a" * 40 + "b")"#,
            r#"/^(a|aa)*$/.match("a" * 40).to_a"#,
            r#"/(x+x+)+y/ =~ ("x" * 30)"#,
            r#"/(x+x+)+y/.match("x" * 30 + "y").to_a"#,
            r#"/^(([a-z])+.)+[A-Z]([a-z])+$/ =~ ("a" * 30)"#,
            r#"/^(?:(?=a*)a)*$/.match("a" * 40).to_a"#,
            r#"/^(?>a*)*$/.match("a" * 40).to_a"#,
            r#"/^(a*)+$/.match("a" * 40).to_a"#,
            r#"/^(a?)*$/.match("a" * 40).to_a"#,
            // Bigger than anything that could finish by luck.
            r#"/^(a*)*$/ =~ ("a" * 400 + "b")"#,
            r#"("a" * 200).scan(/^(a*)*$/).size"#,
        ]);
    }

    #[test]
    fn a_non_linear_pattern_still_needs_its_timeout() {
        // The flip side: a back-reference is what the cache cannot key
        // on, so `/(a+)+\1b/` still runs away and is still the timeout's
        // to stop. `linear_time?` says so up front.
        run_test_once(
            r#"
            [Regexp.linear_time?(/(a+)+\1b/),
             begin
               Regexp.timeout = 0.05
               /(a+)+\1b/ =~ ("a" * 40 + "c")
             rescue Regexp::TimeoutError => e
               e.message
             ensure
               Regexp.timeout = nil
             end]
            "#,
        );
    }

    #[test]
    fn regexp_options_uninitialized() {
        // `Regexp.allocate` is uninitialized -> `#options` is a TypeError.
        run_test_error(r#"Regexp.allocate.options"#);
    }

    /// The per-regexp `timeout:` and `Regexp#timeout` beside the global
    /// pair, and the validation both share.
    ///
    /// None of it is enforced during a match — that needs an interrupt
    /// hook inside Onigmo's own loop, which `onigmo-regex` does not
    /// expose (#1423) — but the accessors answer what CRuby's do,
    /// quantization included: CRuby keeps the value as a `uint64` of
    /// nanoseconds, so anything under a nanosecond reads back as nil and
    /// anything enormous saturates. A NaN is deliberately not pinned —
    /// the C conversion is undefined and glibc and macOS disagree; see
    /// `timeout_nanos`.
    #[test]
    fn regexp_per_regexp_timeout() {
        run_tests(&[
            // A regexp's own timeout, and the literals and copies that
            // have none. `Regexp.new(re)` does *not* inherit it.
            r#"Regexp.new("abc", timeout: 3).timeout"#,
            r#"Regexp.new("abc").timeout"#,
            r#"/abc/.timeout"#,
            r#"src = Regexp.new("abc", timeout: 7)
               [src.timeout, Regexp.new(src).timeout, Regexp.new(src, timeout: 2).timeout]"#,
            // The global is separate: an instance never falls back to it.
            r#"Regexp.timeout = 5
               r = [Regexp.new("a").timeout, Regexp.new("a", timeout: 2).timeout, Regexp.timeout]
               Regexp.timeout = nil
               r"#,
            // The keyword is a keyword, not the options argument: it
            // used to be read as `ignorecase` and warn about it.
            r#"r = Regexp.new("abc", timeout: 3); [r.source, r.options, r.inspect, (r =~ "ABC")]"#,
            r#"Regexp.new("a", Regexp::IGNORECASE, timeout: 2).options"#,
            r#"Regexp.new(/abc/i, timeout: 1).options"#,
            // Coercion and quantization.
            r#"[Regexp.new("a", timeout: nil).timeout,
                Regexp.new("a", timeout: 1).timeout,
                Regexp.new("a", timeout: Rational(1, 2)).timeout,
                Regexp.new("a", timeout: 1e-12).timeout,
                Regexp.new("a", timeout: 10**30).timeout,
                Regexp.new("a", timeout: Float::INFINITY).timeout]"#,
            // A subclass, both with and without its own `#initialize`.
            r#"class RTa < Regexp; end
               [RTa.new("a", timeout: 4).timeout, RTa.new("a").timeout]"#,
            r#"class RTb < Regexp
                 def initialize(src, opt = nil, timeout: nil) = super
               end
               [RTb.new("a", timeout: 6).timeout, RTb.new("a").timeout]"#,
            // It is not a user ivar, and a dump does not carry it.
            r#"r = Regexp.new("a", timeout: 3)
               [r.instance_variables, Marshal.load(Marshal.dump(r)).timeout]"#,
        ]);
        // Zero and negative are rejected, by both the keyword and the
        // global setter, and the global keeps its old value.
        for v in ["0", "-1", "0.0", "-0.0", "Rational(-1, 2)"] {
            run_test_error(&format!(r#"Regexp.new("a", timeout: {v})"#));
            run_test_error(&format!(r#"Regexp.timeout = {v}"#));
        }
        run_test_error(r#"Regexp.new("a", timeout: "x")"#);
        // …and `Regexp.allocate` has no timeout to answer.
        run_test_error(r#"Regexp.allocate.timeout"#);
    }

    #[test]
    fn regexp_timeout_accessors() {
        run_tests(&[
            // `Regexp.timeout` round-trips the global value (a Float),
            // `nil` clears it. (Reset first so the value is deterministic
            // across the harness's repeated in-process evaluations.)
            r#"Regexp.timeout = nil; Regexp.timeout"#,
            r#"(Regexp.timeout = 1.0)"#,
            r#"Regexp.timeout = 3; r = Regexp.timeout; Regexp.timeout = nil; [r, Regexp.timeout]"#,
            r#"
            $_ = "input data"
            ~ /at/
            "#,
            r#"
            $_ = "input data"
            ~ /missing/
            "#,
            // No `$_` -> nil
            r#"
            $_ = nil
            ~ /at/
            "#,
            r#"/abc/.casefold?"#,
            r#"/abc/i.casefold?"#,
            r#"/abc/m.casefold?"#,
            // Pure-ASCII source -> US-ASCII regardless of `n` flag.
            r#"/abc/.encoding.name"#,
            r#"/abc/n.encoding.name"#,
            // Non-ASCII source -> UTF-8.
            r#"/©/.encoding.name"#,
            // Pure-ASCII source isn't fixed-encoding.
            r#"/abc/.fixed_encoding?"#,
            // Non-ASCII source pins the encoding.
            r#"/©/.fixed_encoding?"#,
            // `n` flag with pure-ASCII source -> not fixed.
            r#"/abc/n.fixed_encoding?"#,
            r#"/(?<a>foo)(?<b>bar)/.named_captures"#,
            r#"/foo/.named_captures"#,
            // Duplicate name keeps both indexes under one key.
            r#"/(?<x>a)(?<x>b)/.named_captures"#,
            r#"Regexp.escape(:"a.b")"#,
            r#"Regexp.quote(:"a.b")"#,
            r#"Regexp.escape("a.b")"#,
            // ASCII-only result tags as US-ASCII regardless of source.
            r#"Regexp.escape("abc").encoding.to_s"#,
            r#"Regexp.escape("a.b*c+").encoding.to_s"#,
            r#"Regexp.escape("hello world").encoding.to_s"#,
            // Non-ASCII content keeps the source's UTF-8 tag.
            r#"Regexp.escape("café").encoding.to_s"#,
            r#"Regexp.escape("日本語").encoding.to_s"#,
            // Symbol / String key: dispatches to MatchData#[].
            r#"
            /(?<word>\w+)/ =~ "hello"
            [Regexp.last_match(:word), Regexp.last_match("word"),
             Regexp.last_match(0), Regexp.last_match(1)]
            "#,
            // Symbol with no match propagates nil.
            r#"
            /(?<word>\w+)/ =~ ""
            Regexp.last_match(:word).inspect
            "#,
            // Match success: block runs, return value comes from block.
            r#"/(\d+)/.match("hello 42") { |m| m[1].to_i + 1 }"#,
            // Match failure: block does NOT run, returns nil.
            r#"/(\d+)/.match("hello") { |m| "should not run" }.inspect"#,
            // nil arg: returns nil without raising.
            r#"/abc/.match(nil).inspect"#,
            // Regexp.compile/new should preserve encoding+fixed_encoding
            // from the source Regexp argument.
            r#"Regexp.compile(/abc/u).encoding.to_s"#,
            r#"Regexp.new(/abc/u).encoding.to_s"#,
            r#"Regexp.compile(/abc/u).fixed_encoding?"#,
            r#"Regexp.compile(/abc/).encoding.to_s"#,
            r#"Regexp.compile(/abc/).fixed_encoding?"#,
            // n flag preserves NOENCODING.
            r#"Regexp.compile(/abc/n).encoding.to_s"#,
            r#"Regexp.compile(/abc/n).fixed_encoding?"#,
            // \u escape with non-ASCII codepoint pins to UTF-8.
            r#"/\u{1234}/.encoding.to_s"#,
            r#"/\u{1234}/.fixed_encoding?"#,
            r#"/é/.encoding.to_s"#,
            r#"/é/.fixed_encoding?"#,
            // \u escape with ASCII codepoint stays US-ASCII (not pinned).
            r#"/A/.encoding.to_s"#,
            r#"/A/.fixed_encoding?"#,
            r#"/\u{41}/.encoding.to_s"#,
            r#"/\u{41}/.fixed_encoding?"#,
            // ----- Phase D follow-ups -----
            // Once `Regexp.last_match` is `nil`, *any* arg short-circuits
            // to `nil` (CRuby semantics). Including args that would
            // normally raise from `to_int` coercion.
            r#"
              /foo/ =~ "TEST123"
              [Regexp.last_match(:test),
               Regexp.last_match(1),
               Regexp.last_match(Object.new),
               Regexp.last_match("test")]
            "#,
        ]);
    }

    #[test]
    fn regexp_initialize_is_private_and_raises() {
        // The instance method exists, is private, and always raises.
        run_test(r#"Regexp.private_instance_methods.include?(:initialize)"#);
        run_test_error(r#"//.send(:initialize, "")"#);
        run_test_error(r#"Regexp.new("").send(:initialize, "")"#);
        run_test_error(r#"Class.new(Regexp).new("").send(:initialize, "")"#);
    }

    #[test]
    fn regexp_literal_is_frozen() {
        run_tests(&[
            r#"//.frozen?"#,
            r#"/abc/.frozen?"#,
            // `Regexp.new(...)` instances are not auto-frozen.
            r#"Regexp.new("abc").frozen?"#,
            // CRuby escapes ` ` / `\t`/`\n`/`\r`/`\f`/`\v` so the result
            // round-trips through the `x` modifier.
            r#"Regexp.escape("a b")"#,
            r#"Regexp.escape("a\tb\nc")"#,
            // Mixed meta + whitespace.
            r#"Regexp.escape("\\*?{}.+^$[]()- \t\n\r")"#,
        ]);
    }

    #[test]
    fn regexp_teq_uses_to_str() {
        run_tests(&[
            // `Regexp#===` coerces string-like objects via `#to_str`.
            r#"
              c = Class.new { def to_str; "abc"; end }
              /abc/ === c.new
            "#,
            // `nil` and `Regexp` args still return false.
            r#"/abc/ === nil"#,
            r#"/abc/ === /abc/"#,
        ]);
        // `to_str` returning a non-String raises TypeError, matching CRuby.
        run_test_error(
            r#"
              c = Class.new { def to_str; 42; end }
              /abc/ === c.new
            "#,
        );
    }

    #[test]
    fn regexp_match_uninitialized_raises_typeerror() {
        run_test_error(r#"Regexp.allocate.match("foo")"#);
        run_test_error(r#"Regexp.allocate.match?("foo")"#);
        run_test_error(r#"Regexp.allocate =~ "foo""#);
    }

    #[test]
    fn regexp_match_pred_does_not_set_special_vars() {
        run_tests(&[
            // `Regexp#match?` is documented as "doesn't update `$~` and
            // friends". The previous match's `$~` should survive a
            // subsequent `match?` call.
            r#"
              "abc" =~ /(a)/
              before = $~[0]
              /x/.match?("xyz")
              [before, $~[0]]
            "#,
            // Different declared encoding ⇒ not equal.
            r#"/abc/u == /abc/n"#,
            // Same kcode ⇒ equal.
            r#"/abc/u == /abc/u"#,
            r#"/abc/n == /abc/n"#,
            // Empty / pure-ASCII source: the `n` modifier doesn't shift
            // the resolved encoding away from US-ASCII, so // == //n.
            r#"// == //n"#,
            r#"//n == //"#,
            // ----- Phase D: extended coverage -----
            // Positive / negative / out-of-range Integer indices off
            // a match — including the `$~`-style negatives that wrap
            // from the end of the captures list.
            r#"
              /(\w)(\w)(\w)/ =~ "abcdef"
              [Regexp.last_match(0),
               Regexp.last_match(1),
               Regexp.last_match(2),
               Regexp.last_match(3),
               Regexp.last_match(4),
               Regexp.last_match(-1),
               Regexp.last_match(-3),
               Regexp.last_match(-4),
               Regexp.last_match(-100)]
            "#,
            // When two capture groups share a name, `MatchData#[]` (and
            // therefore `Regexp.last_match(:name)`) returns the *last
            // participating* group. CRuby's named-group lookup is
            // last-wins on collisions.
            r#"
              /(?<x>a)(?<x>b)/ =~ "ab"
              [Regexp.last_match(:x), Regexp.last_match("x")]
            "#,
            // A failed `=~` clears `$~` to nil; subsequent
            // `Regexp.last_match` reads as `nil` regardless of arg type.
            r#"
              /(\w+)/ =~ "abc"
              before = Regexp.last_match(1)
              /xyz/ =~ "abc"
              [before, Regexp.last_match, Regexp.last_match(0), Regexp.last_match(:nope)]
            "#,
            // The block's return value becomes the `match { ... }` value.
            r#"/(\d+)/.match("count: 42") { |m| m[1].to_i * 2 }"#,
            // Returning an arbitrary object works.
            r#"/(\w)/.match("z") { |m| [m[0], 99, :sym] }"#,
            // The match object is the block's only argument.
            r#"/(.)/.match("a") { |m| m.class.name }"#,
            // Match starting at a positive char position.
            r#"/(.).(.)/.match("foobar", 3).captures"#,
            // Negative position counts from the end.
            r#"/(.).(.)/.match("foobar", -3).captures"#,
            // Position past end of string returns nil.
            r#"/x/.match("abc", 100)"#,
            // `match?` accepts the same position arg as `match`.
            r#"/foo/.match?("xfooy", 1)"#,
            r#"/foo/.match?("xfooy", -3)"#,
            // `\Az/.match?("", 0)` finds the zero-width match at the end.
            r#"/\Az/.match?("", 0)"#,
            // `\z/` on a non-empty string with `match?(s, len)` finds
            // the end-of-string match.
            r#"/\z/.match?("abc", 3)"#,
            // `Regexp.compile(re, nil)` is the same as `Regexp.compile(re)`
            // — no warning, options preserved.
            r#"Regexp.compile(/abc/i, nil) == /abc/i"#,
            r#"Regexp.compile(/abc/u, nil).encoding.name"#,
            r#"Regexp.compile(/abc/u, nil).fixed_encoding?"#,
            // `Regexp.new(/.../e)` keeps the EUC-JP declared encoding;
            // `/.../s` keeps Windows-31J. `Regexp.compile` is an alias
            // for `Regexp.new`.
            r#"Regexp.new(/abc/e).encoding.name"#,
            r#"Regexp.new(/abc/s).encoding.name"#,
            r#"Regexp.compile(/abc/e).fixed_encoding?"#,
            r#"Regexp.compile(/abc/s).fixed_encoding?"#,
            // `\u` escapes inside a character class still pin encoding.
            r#"/[\u{1234}]/.encoding.name"#,
            r#"/[\u{1234}]/.fixed_encoding?"#,
            // Range with `\u{...}` endpoints.
            r#"/[\u{20}-\u{1234}]/.fixed_encoding?"#,
            // `i`/`m`/`x` modifiers combine with `\u`-pinning without
            // disturbing the encoding result.
            r#"/\u{1234}/i.encoding.name"#,
            r#"/\u{1234}/i.fixed_encoding?"#,
            r#"/\u{1234}/m.encoding.name"#,
            r#"/\u{1234}/x.encoding.name"#,
            // `n` modifier overrides the `\u` codepoint check (would
            // also be a SyntaxError if the source actually contained
            // non-ASCII bytes; here the source is pure-ASCII).
            // Empty input round-trips US-ASCII.
            r#"Regexp.escape("").encoding.name"#,
            r#"Regexp.escape("")"#,
            // `Regexp.escape` of meta-only ASCII is US-ASCII.
            r#"Regexp.escape(".+*?").encoding.name"#,
            // Symbol path produces US-ASCII too (when ASCII-only).
            r#"Regexp.escape(:abc).encoding.name"#,
            r#"Regexp.escape(:abc)"#,
            // The escaped form recompiles into a regex that matches the
            // original input verbatim — the whole point of `Regexp.escape`.
            r#"
              s = "a.b*c+ d?"
              Regexp.new(Regexp.escape(s)) =~ s
            "#,
            // With whitespace + the `x` modifier (which would otherwise
            // skip whitespace), the escaped form still matches.
            r#"
              s = "a b\tc"
              Regexp.new(Regexp.escape(s), Regexp::EXTENDED) =~ s
            "#,
            // The `initialize` method only raises on a *previously
            // initialized* receiver. `Regexp.new` itself goes through
            // the C-side construction and never invokes `#initialize`,
            // so the public surface stays usable.
            r#"Regexp.new("abc").source"#,
            r#"Regexp.new("abc").options"#,
            r#"Regexp.new(/abc/i).options & Regexp::IGNORECASE != 0"#,
        ]);
    }

    #[test]
    fn regexp_allocate_observable_metadata() {
        run_tests(&[
            // `Regexp.allocate` produces a real Regexp instance (so
            // `#class`, `#frozen?`, etc. work) but methods that read
            // the source raise `TypeError`. CRuby behaves the same way.
            r#"Regexp.allocate.class.name"#,
            r#"Regexp.allocate.frozen?"#,
        ]);
        // Source-reading method `#==` raises TypeError.
        run_test_error(r#"Regexp.allocate == /abc/"#);
        run_test_error(r#"Regexp.allocate == Regexp.allocate"#);
    }

    #[test]
    fn regexp_hash_ignores_encoding() {
        run_tests(&[
            // CRuby's `#hash` is keyed on source + onigmo options only;
            // the declared encoding does not participate. Two regexps
            // can hash equal yet `==` false (hash collisions are fine).
            r#"/abc/.hash == /abc/.hash"#,
            r#"/abc/u.hash == /abc/u.hash"#,
            r#"/abc/u.hash == /abc/n.hash"#,
            // Different `mix` bits change the hash.
            r#"/abc/.hash == /abc/i.hash"#,
            // Plain happy paths exercising the String / Symbol fast paths
            // alongside the new `#to_str` fallback.
            r#"/abc/ === "xabcx""#,
            r#"/abc/ === :abc"#,
            r#"/abc/ === "no""#,
            // Even when `match?` is called *with* a previous successful
            // match in `$~`, `$~` is unchanged regardless of the
            // `match?` result. Both true and false paths share this
            // contract in CRuby.
            r#"
              "abc" =~ /(b)/
              kept = $~[0]
              ok = /b/.match?("xyzb")
              fail = /q/.match?("xyz")
              [kept, $~[0], ok, fail]
            "#,
            // A whole-pattern-spanning `(?flags:...)` / `(?:...)` wrapper
            // folds into the outer option block (CRuby `rb_reg_to_s`).
            r#"/(?i:nothing outside this group)/.to_s"#,
            r#"/(?i:.)/.to_s"#,
            r#"/(?mmmmix-miiiix:)/.to_s"#,
            r#"/(?:.)/.to_s"#,
            // No folding when the group does not span the whole pattern.
            r#"/(?ix:foo)(?m:bar)/.to_s"#,
            r#"/(?ix:foo)bar/m.to_s"#,
            r#"/whatever(?:0d)/ix.to_s"#,
            r#"/(?=5)/.to_s"#,
            r#"/(?!5)/.to_s"#,
            // Plain option rendering still correct.
            r#"/abc/mxi.to_s"#,
            r#"/abc/i.to_s"#,
            r#"/abc/.to_s"#,
            r#"/(a)(b)/.to_s"#,
            // Char class containing parens must not confuse the scan.
            r#"/(?i:[()])/.to_s"#,
        ]);
    }

    #[test]
    fn regexp_escape_quote() {
        run_tests(&[
            // Metacharacters escaped; whitespace controls rewritten.
            r#"Regexp.escape("a b.c*[d]{e}(f)|g-h^i$j+k?l#m")"#,
            r#"Regexp.escape("\n\t\r\f\v")"#,
            // Symbol argument.
            r#"Regexp.quote(:"a.b")"#,
            // ASCII-only result is tagged US-ASCII.
            r#"Regexp.escape("abc").encoding.name"#,
        ]);
        // Broken (invalid-UTF-8) strings escape byte-wise instead of
        // raising — the invalid byte passes through verbatim.
        run_test(
            r##"
            s = "a\xffb".force_encoding("UTF-8")
            e = Regexp.escape(s)
            [e.bytes, e.encoding.name]
            "##,
        );
        // A non-String/Symbol arg is coerced via #to_str.
        run_test(
            r##"
            o = Object.new
            def o.to_str; "a.b"; end
            Regexp.escape(o)
            "##,
        );
        // An arg that can't be coerced ⇒ TypeError.
        run_test_error(r#"Regexp.escape(5)"#);
    }

    #[test]
    fn regexp_options_fixedencoding() {
        // The `u`/`e`/`s` encoding modifiers pin the encoding, so
        // `#options` exposes Regexp::FIXEDENCODING; a plain regexp does not.
        run_test(
            "[/abc/u, /abc/e, /abc/s, /abc/].map { |r| (r.options & Regexp::FIXEDENCODING) != 0 }",
        );
    }

    #[test]
    fn regexp_source_bytes_preserved() {
        // A non-UTF-8 (Shift_JIS) source survives in #source / #encoding.
        run_test(
            r#"s = "\x82\xa0".dup.force_encoding(Encoding::Shift_JIS); r = Regexp.new(s);
               [r.encoding.to_s, r.source.encoding.to_s, r.source.bytes]"#,
        );
        // `\u{}` source is kept as written (not expanded), distinct from
        // its decoded form.
        run_test(r#"[/\u{61}/.source, /\u{61}/.inspect, (/\u{61}/ == /a/)]"#);
        // A Regexp argument preserves the original source verbatim.
        run_test(r#"Regexp.new(/\u{61}/).source"#);
    }

    #[test]
    fn regexp_subclass_new() {
        // A subclass with an overridden #initialize: the override runs
        // (its `super` builds the regexp, its body sets @args), and the
        // result is an instance of the subclass.
        run_test(
            r#"class ReSubA < Regexp; def initialize(*a); super; @a = a; end; attr_reader :a; end
               r = ReSubA.new("hi"); [r.is_a?(ReSubA), r.a.first, r.source, r.match("xhiy")[0]]"#,
        );
        // A subclass without an overridden #initialize.
        run_test(r#"class ReSubB < Regexp; end; r = ReSubB.new("hi"); [r.is_a?(ReSubB), r.source]"#);
        // Re-initializing an already-built regexp still raises.
        run_test(r#"(Regexp.new("x").send(:initialize, "y"); nil) rescue $!.class"#);
    }

    #[test]
    fn regexp_interpolation_encoding() {
        // An interpolated non-ASCII String upgrades the regexp's encoding.
        run_test(r#"s = "文字化け".encode("euc-jp"); /#{s}/.encoding.to_s"#);
        // A 7-bit interpolated String stays US-ASCII even if tagged EUC-JP.
        run_test(r#"a = "abc".encode("euc-jp"); /#{a}/.encoding.to_s"#);
        run_test(r#"/#{"あ"}/.encoding.to_s"#);
        // Interpolation still matches.
        run_test(r#"x = "wor"; "hello world" =~ /#{x}ld/"#);
    }

    #[test]
    fn regexp_match_invalid_encoding() {
        // Matching a subject that is invalid in its own encoding raises
        // ArgumentError (CRuby), instead of mis-matching or erroring.
        run_test(
            r#"x = [150].pack("C").force_encoding("utf-8");
               (/(.).(.)/.match("ab #{x} cd", 1); nil) rescue [$!.class, $!.message]"#,
        );
        run_test(
            r#"x = [150].pack("C").force_encoding("utf-8");
               (/(.).(.)/.match("ab #{x} cd", -1); nil) rescue [$!.class, $!.message]"#,
        );
        // A valid (binary) subject still matches.
        run_test(r#"/a/.match("xay".b).nil?"#);
    }

    #[test]
    fn regexp_binary_match_warning() {
        // A `/n` (NOENCODING) regexp matched against a non-ASCII String in
        // an encoding other than ASCII-8BIT warns "historical binary regexp
        // match" across match / =~; ASCII-only, BINARY, or non-/n cases are
        // silent. Capture $stderr and assert on what was written.
        run_test_once(
            r#"
            def cap
              saved = $stderr
              buf = +""
              io = Object.new
              io.define_singleton_method(:write) { |*a| a.each { |x| buf << x.to_s } }
              $stderr = io
              yield
              buf
            ensure
              $stderr = saved
            end
            u = "\303\251".dup.force_encoding("utf-8")
            res = []
            res << cap { /./n.match(u) }.scan(/historical binary regexp match/)
            res << cap { /./n =~ u }.scan(/historical binary regexp match/)
            res << cap { /./n.match("abc") }.empty?
            res << cap { /./n.match("\303\251".b) }.empty?
            res << cap { /./.match(u) }.empty?
            res
            "#,
        );
    }

    #[test]
    fn regexp_match_encoding_compatibility() {
        // CRuby `rb_reg_prepare_enc`: a regexp/subject encoding clash
        // raises Encoding::CompatibilityError across `match` / `match?` /
        // `=~` / `===`.
        // (1) ASCII-incompatible subject (UTF-16LE) vs an ASCII regexp.
        run_test(
            r#"(/\A[[:space:]]*\z/.match(" ".encode("UTF-16LE")); nil) rescue $!.class.name"#,
        );
        run_test(
            r#"(/\A[[:space:]]*\z/.match?(" ".encode("UTF-16LE")); nil) rescue $!.class.name"#,
        );
        run_test(r#"(/\A[[:space:]]*\z/ =~ " ".encode("UTF-16LE"); nil) rescue $!.class.name"#);
        run_test(r#"(/x/ === "y".encode("UTF-16LE"); nil) rescue $!.class.name"#);
        // (2) A fixed-encoding regexp whose encoding differs from the
        // (ASCII-compatible) subject's.
        run_test(
            r#"(Regexp.new("".dup.force_encoding("UTF-16LE"), Regexp::FIXEDENCODING) =~ " ".encode("UTF-8"); nil) rescue $!.class.name"#,
        );
        // (3) A fixed US-ASCII regexp vs a UTF-8 subject with non-ASCII
        // content.
        run_test(
            r#"(Regexp.new("".dup.force_encoding("US-ASCII"), Regexp::FIXEDENCODING) =~ "\303\251".dup.force_encoding("UTF-8"); nil) rescue $!.class.name"#,
        );
        // Compatible pairs (ASCII subject, or same encoding) still match.
        run_test(r#"[ /abc/ =~ "xabc", /\d+/.match("a12b")[0], ("héllo" =~ /é/) ]"#);
        // A subject broken in its own encoding is still ArgumentError,
        // not CompatibilityError.
        run_test(
            r#"("\x80".dup.force_encoding("UTF-8") =~ /./; nil) rescue [$!.class.name, $!.message]"#,
        );
    }

    #[test]
    fn regexp_encoding_binary_noencoding() {
        // `/.../n` with a high `\xHH` escape is BINARY; pure-ASCII /n is
        // US-ASCII.
        run_test(r#"[/\xc2\xa1/n.encoding.to_s, /abc/n.encoding.to_s, /\x7f/n.encoding.to_s]"#);
        run_test(
            r#"Regexp.new('([\x00-\xFF])', Regexp::IGNORECASE | Regexp::NOENCODING).encoding.to_s"#,
        );
        // union keeps BINARY when a part is BINARY-with-non-ASCII.
        run_test(r#"Regexp.union(/abc/, /[\x00-\x7f]/n, /[\x80-\xBF]/n).encoding.to_s"#);
    }

    #[test]
    fn regexp_encoding_specifier_last_wins() {
        // Several encoding specifiers on one literal: the last in source
        // order wins (`/foo/ensuens` selects `s`), for the declared
        // encoding and for `==`.
        run_test(
            r#"[ /foo/ensuensuens.encoding.to_s,
                /foo/ensuensuens == /foo/s,
                /foo/sn.encoding.to_s,
                /foo/ns.encoding.to_s ]"#,
        );
        // ...and through interpolation (the same "last wins" decode).
        run_test(r#"x = "o"; [ /fo#{x}/ensuens.encoding.to_s, /fo#{x}/su.encoding.to_s ]"#);
    }

    #[test]
    fn a_cjk_subject_is_matched_by_character_under_its_own_codec() {
        // Onigmo has a codec for every CJK code page monoruby walks
        // (`onigmo_encoding_for`), so a subject in one is matched on its
        // own bytes: `.` takes a whole character, offsets are character
        // offsets, and the chunks cut out carry the subject's encoding.
        // These used to be matched byte by byte through the UTF-8 view.
        run_test_once(
            r##"
            def e(s, enc) = s.dup.force_encoding(enc)
            def w(v) = v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v.is_a?(Array) ? v.map { |i| w(i) } : v.is_a?(MatchData) ? w(v.to_a) : v
            x = ->(&b) { begin; w(b.call); rescue => err; [err.class, err.message]; end }
            ss = [e("\xC7\xD1\xB1\xB9\xBE\xEEabc", "EUC-KR"), e("\xA4\xA4\xA4\xE5abc", "Big5"), e("\xD6\xD0\xCE\xC4abc", "GBK"),
                  e("\xD6\xD0\xCE\xC4abc\x94\x32\xBE\x34", "GB18030"), e("\x8C\x63abc", "CP949"), e("\xC4\xA1\x8E\xA2\xA1\xA1abc", "EUC-TW"),
                  e("\x92\xA4\xA2abc", "Emacs-Mule"), e("\xA4\xA4abc", "Big5-HKSCS"), e("\xC7\xD1abc", "GB2312"), e("\xA4\xA4abc", "CP950")]
            ss.map { |s|
              [x.() { s =~ /a/ }, x.() { s.match(/(.)/) }, x.() { s.scan(/./) }, x.() { s.index(/b/) }, x.() { s[/./] }, x.() { s.start_with?(/a/) },
               x.() { s.sub(/./, "X") }, x.() { s.gsub(/./, "X") }, x.() { s.gsub(/(.)/) { $1 * 2 } }, x.() { s =~ /\w/ }, x.() { s =~ /[a-z]+/; $~.byteoffset(0) },
               x.() { s.byteindex(/a/) }, x.() { s.byterindex(/./) }, x.() { s.byteindex(/./, 1) }, x.() { s.byterindex(/./, 1) }, x.() { s.split(/b/) },
               x.() { s.partition(/a/) }, x.() { s.match?(/c\z/) }, x.() { s =~ /./u }, x.() { s.rindex(/./) }, x.() { /a/ === s }]
            }
            "##,
        );
    }

    #[test]
    fn the_subject_is_checked_as_rb_reg_prepare_enc_does() {
        // A broken subject is refused first, in its own encoding's name
        // (an odd-length UTF-16LE string is "invalid byte sequence in
        // UTF-16LE", a US-ASCII string with a high byte names US-ASCII);
        // only then are the encodings compared, so a UTF-16 subject meets
        // an ASCII regexp as an encoding clash rather than as bytes that
        // are no UTF-8 — for every regexp operation, `scan` and `split`
        // included. A Symbol is checked as the String it names.
        run_test_once(
            r##"
            def e(s, enc) = s.dup.force_encoding(enc)
            def w(v) = v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v.is_a?(Array) ? v.map { |i| w(i) } : v.is_a?(MatchData) ? w(v.to_a) : v
            x = ->(&b) { begin; w(b.call); rescue => err; [err.class, err.message]; end }
            u16 = "ab日".encode("UTF-16LE"); odd = e("a\x00b", "UTF-16LE"); usb = e("ab\x80c", "US-ASCII"); krb = e("\xC7abc", "EUC-KR"); b5 = e("\xA4abc", "Big5")
            [u16, "ab日".encode("UTF-32BE"), odd, usb, krb, b5, "ab\xFFc"].map { |s|
              [x.() { s =~ /a/ }, x.() { s.scan(/./) }, x.() { s.split(/b/) }, x.() { s.index(/b/) }, x.() { s.rindex(/b/) }, x.() { s.sub(/./, "X") },
               x.() { s.gsub(/a/, "a" => "b") }, x.() { s.gsub(/(.)/) { $1 } }, x.() { s.match?(/a/) }, x.() { s.start_with?(/a/) }, x.() { s.index("b") },
               x.() { /a/ === s }, x.() { s =~ /日/ }, x.() { s.partition(/a/) }, x.() { s[/a/] }, x.() { s.byteindex(/a/) }]
            } + [x.() { /a/.match(:abc) }, x.() { Regexp.new("a".encode("UTF-16LE")).match(:abc) }, x.() { Regexp.new("a".encode("UTF-16LE")) === "abc" },
                 x.() { Regexp.new("a".encode("UTF-16LE")) =~ "abc" }, x.() { u16 =~ "a".encode("UTF-16LE") }]
            "##,
        );
    }

    #[test]
    fn a_utf16_pattern_is_compiled_and_matched_in_its_own_encoding() {
        // `Regexp.new` of a UTF-16 / UTF-32 String compiles its bytes
        // under that codec; the regexp is pinned to it, and matches only
        // subjects in it. `Regexp.escape` and `Regexp.union` spell their
        // metacharacters and the `|` as characters of the encoding, a
        // replacement's `\2\1` are read as code units, `#to_s` wraps in
        // the encoding, and `#inspect` is the US-ASCII rendering CRuby
        // writes (an ASCII character's own bytes, NUL units included).
        run_test_once(
            r##"
            def e(s, enc) = s.dup.force_encoding(enc)
            def w(v)
              case v
              when String then [v.bytes, v.encoding.to_s]
              when Array then v.map { |i| w(i) }
              when MatchData then w(v.to_a)
              when Regexp then [v.source.bytes, v.encoding.to_s, v.fixed_encoding?, v.options, v.inspect.bytes, v.inspect.encoding.to_s, v.to_s.bytes, v.to_s.encoding.to_s]
              else v
              end
            end
            x = ->(&b) { begin; w(b.call); rescue => err; [err.class, err.message.ascii_only? ? err.message : err.message.encoding.to_s]; end }
            u = ->(s, enc = "UTF-16LE") { s.encode(enc) }
            s = u.("ab日cab")
            re = Regexp.new(u.("(日)(.)"))
            [x.() { re }, x.() { re.match(s) }, x.() { m = re.match(s); [m.begin(0), m.end(0), m.byteoffset(0), m.pre_match, m.post_match] },
             x.() { Regexp.new(u.("c")) =~ s }, x.() { s =~ Regexp.new(u.("A"), "i") }, x.() { s[Regexp.new(u.("日."))] }, x.() { s[Regexp.new(u.("(日)(.)")), 2] },
             x.() { s.index(Regexp.new(u.("a")), 1) }, x.() { s.rindex(Regexp.new(u.("a"))) }, x.() { s.scan(Regexp.new(u.("."))) }, x.() { s.scan(Regexp.new(u.("(a)(.)"))) },
             x.() { s.gsub(Regexp.new(u.("a")), u.("X")) }, x.() { s.gsub(Regexp.new(u.("(a)(b)")), u.("<\\2\\1>")) }, x.() { s.gsub(Regexp.new(u.("a"))) { |m| m + m } },
             x.() { s.gsub(Regexp.new(u.("a")), u.("a") => u.("Z")) }, x.() { s.sub(Regexp.new(u.("b"))) { $~.begin(0).to_s.encode("UTF-16LE") } },
             x.() { s.split(Regexp.new(u.("b"))) }, x.() { s.split(Regexp.new(u.("(b)"))) }, x.() { s.split(u.("b")) }, x.() { s.split(u.("b"), 2) }, x.() { u.("a b  c").split(u.(" ")) },
             x.() { s.start_with?(Regexp.new(u.("ab"))) }, x.() { s.partition(Regexp.new(u.("日"))) }, x.() { s.rpartition(Regexp.new(u.("a"))) }, x.() { s.match(u.("日.")) }, x.() { s.match?(u.("日.")) },
             x.() { s.byteindex(Regexp.new(u.("c"))) }, x.() { s.byteindex(Regexp.new(u.("a")), 2) }, x.() { s.byteindex(Regexp.new(u.("a")), 1) }, x.() { s.byterindex(Regexp.new(u.("a"))) },
             x.() { Regexp.new(u.("日", "UTF-16BE")).match(u.("ab日", "UTF-16BE")) }, x.() { Regexp.new(u.("(日)", "UTF-32LE")).match(u.("ab日", "UTF-32LE")) },
             x.() { Regexp.new(u.("日", "UTF-32BE")).match(u.("ab日", "UTF-32BE")).byteoffset(0) },
             x.() { Regexp.new(u.("a")).match(u.("ab", "UTF-16BE")) }, x.() { Regexp.new(u.("a")).match("abc") }, x.() { /a/ =~ s }, x.() { /日/ =~ s },
             x.() { Regexp.escape(u.("a[b].c\n")) }, x.() { Regexp.escape(u.("日.")) }, x.() { Regexp.union(u.("a[b"), u.("c.")) }, x.() { Regexp.union(u.("a[b"), u.("日")) =~ s },
             x.() { Regexp.union(u.("a"), "b") }, x.() { [Regexp.new(u.("a")) == Regexp.new(u.("a")), Regexp.new(u.("a")) == /a/, Regexp.new(u.("a")).hash == Regexp.new(u.("a")).hash] },
             x.() { Regexp.new(u.("(?<n>a)")).names }, x.() { Regexp.new(Regexp.new(u.("a"))) }, x.() { Regexp.new(u.("a")).match(e("a\x00b", "UTF-16LE")) },
             x.() { Regexp.new(u.("")).match(s) }, x.() { Regexp.new(u.("a\x01\t/\\/b")) }, x.() { Regexp.new(u.("(b)")).match(s); [$~[1], $1, $`, $'] },
             x.() { Regexp.new(u.("a"), "mix").options }]
            "##,
        );
    }

    #[test]
    fn a_string_pattern_is_a_regexp_source_in_its_own_encoding() {
        // `get_pat`: a String handed where a pattern is wanted is
        // compiled as a regexp source in its own encoding — an EUC-KR or
        // Shift_JIS one too — and `Regexp.escape` walks the string's
        // characters, so a Shift_JIS trail byte that happens to be `[`
        // is not escaped and a metacharacter in UTF-16 takes a two-byte
        // backslash.
        run_test_once(
            r##"
            def w(v) = v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v.is_a?(Array) ? v.map { |i| w(i) } : v.is_a?(MatchData) ? w(v.to_a) : v.is_a?(Regexp) ? [v.source.bytes, v.encoding.to_s, v.fixed_encoding?] : v
            x = ->(&b) { begin; w(b.call); rescue => err; [err.class, err.message]; end }
            kr = "한국어abc".encode("EUC-KR"); sj = "ソ[ト".encode("Shift_JIS")
            [x.() { kr.match("국.".encode("EUC-KR")) }, x.() { kr.match?("국.".encode("EUC-KR")) }, x.() { kr =~ "국".encode("EUC-KR") }, x.() { kr.split("국".encode("EUC-KR")) },
             x.() { kr.sub("국".encode("EUC-KR"), "<\\0>".encode("EUC-KR")) }, x.() { kr.index("어".encode("EUC-KR")) }, x.() { kr.match("국") },
             x.() { Regexp.escape(sj) }, x.() { Regexp.escape("한[".encode("EUC-KR")) }, x.() { Regexp.escape("a[".force_encoding("US-ASCII")) }, x.() { Regexp.escape("a[\xFF".b) },
             x.() { Regexp.escape("a[".encode("EUC-KR")) }, x.() { sj.match(Regexp.escape("[ト".encode("Shift_JIS"))) }, x.() { Regexp.new(Regexp.escape(sj)) },
             x.() { Regexp.new(Regexp.escape("한[".encode("EUC-KR"))).match("x한[".encode("EUC-KR")) }, x.() { Regexp.union(sj, "x".encode("Shift_JIS")) }, x.() { Regexp.union(kr, "日") }]
            "##,
        );
    }

    #[test]
    fn a_hex_escape_pins_or_refuses_the_regexp_s_encoding() {
        // `unescape_nonascii`: an escaped byte above 0x7F has to spell,
        // with the `\xHH` right after it, one character of the source's
        // encoding (the modifier's, under `/e` / `/s` / `/u`) — else "too
        // short escaped multibyte character" when the escapes run out
        // and "invalid multibyte escape" when they spell none — and then
        // pins the regexp to that encoding. BINARY and US-ASCII sources
        // are exempt, as is `/n`. The same reading applies to a literal,
        // where a refusal is a SyntaxError (#1622).
        run_test_once(
            r##"
            def e(s, enc) = s.dup.force_encoding(enc)
            def w(v) = v.is_a?(Regexp) ? [v.source.bytes, v.encoding.to_s, v.fixed_encoding?, v.options] : v
            x = ->(&b) { begin; w(b.call); rescue SyntaxError => err; [err.class, err.message[/(invalid|too short)[^\n]*/]]; rescue => err; [err.class, err.message]; end }
            [x.() { Regexp.new("\\xff") }, x.() { Regexp.new("\\x80") }, x.() { Regexp.new("\\xe3\\x81\\x82") }, x.() { Regexp.new("\\xe3\\x81\\x82") =~ "xあ" }, x.() { Regexp.new("\\xe3") },
             x.() { Regexp.new("\\xe3\\x81x") }, x.() { Regexp.new(e("\\xC6\\xFC", "EUC-JP")) }, x.() { Regexp.new(e("\\xC6", "EUC-JP")) }, x.() { Regexp.new(e("\\xC6a", "EUC-JP")) },
             x.() { Regexp.new(e("\\xff", "US-ASCII")) }, x.() { Regexp.new("\\xff".b) }, x.() { Regexp.new("\\xff", Regexp::NOENCODING) }, x.() { Regexp.new("[\\xe3\\x81\\x82]") },
             x.() { Regexp.new("[\\xff]") }, x.() { Regexp.new("\\\\\\xff") }, x.() { Regexp.new(e("\\xe3\\x81\\x82", "EUC-JP")) }, x.() { Regexp.new(e("\\x83\\x67", "Shift_JIS")) },
             x.() { Regexp.new(e("\\x83g", "Shift_JIS")) }, x.() { Regexp.new("é", Regexp::NOENCODING) }, x.() { Regexp.new(e("\xA4\xA2", "EUC-JP"), Regexp::NOENCODING) },
             x.() { Regexp.new("\\xe9", Regexp::NOENCODING) }, x.() { Regexp.new("\\x41") }, x.() { Regexp.new("\\u3042\\xe3\\x81\\x82") },
             x.() { eval('/\xe3\x81\x82/') }, x.() { eval('/\xff/') }, x.() { eval('/\xff/n') }, x.() { eval('/\xe3/') }, x.() { eval('/\xC6\xFC/e') }, x.() { eval('/\xC6/e') },
             x.() { eval('/\x80/u') }, x.() { eval('/\x41/') }, x.() { eval('/\xe3\x81\x82#{1}/') }, x.() { eval('/\xff#{1}/') }, x.() { eval('/\xC6\xFC#{1}/e') }]
            "##,
        );
    }

    #[test]
    fn a_regexp_literal_in_a_non_utf8_source_reads_the_file_s_encoding() {
        // `# encoding: EUC-JP` reaches regexp literals as it does string
        // literals: the bytes as written are the pattern, in EUC-JP,
        // pinned when they are not ASCII; `\xHH` escapes are read in
        // EUC-JP; and the literal used to be a FatalError (#1622).
        run_test_once(
            r##"
            require "tmpdir"
            Dir.mktmpdir do |d|
              path = File.join(d, "e.rb")
              File.binwrite(path, "# encoding: EUC-JP\n$r = [/\xC6\xFC/.encoding, /\xC6\xFC/.fixed_encoding?, \"\xC6\xFC\" =~ /\xC6\xFC/, /\\xC6\\xFC/.encoding, /\\xC6\\xFC/e.encoding, /a/e.encoding, /a/s.encoding, /abc/.encoding, /abc/.fixed_encoding?, /\xC6\xFC/.source.bytes, /\#{1}\xC6\xFC/.encoding, /\xC6\xFC/ =~ \"x\xC6\xFC\", \"\xC6\xFC\".match(/(.)/)[1].bytes, /\xC6\xFC/.inspect.bytes, /\xC6\xFC/.to_s.bytes]\n")
              load path
              $r.map { |v| v.is_a?(Encoding) ? v.to_s : v }
            end
            "##,
        );
    }

    #[test]
    fn regexp_uninitialized_encoding_is_binary() {
        // `Regexp.allocate` has no source yet; its encoding is BINARY.
        run_test(r#"Regexp.allocate.encoding.to_s"#);
    }

    #[test]
    fn regexp_source_broken_in_its_own_encoding() {
        // `rb_reg_preprocess` walks the source in the encoding it is
        // tagged with and refuses it as soon as the bytes spell no
        // character there — the whole family is "invalid multibyte
        // character", not whatever Onigmo would have said about the
        // truncated lead byte ("too short multibyte code string").
        run_tests(&[
            r##"(Regexp.new("a\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("\x81".dup.force_encoding("Shift_JIS")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("a\xff".dup.force_encoding("UTF-8")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("x\xe3\x81y".dup.force_encoding("UTF-8")); nil) rescue [$!.class, $!.message]"##,
            // A valid character before the broken bytes renders as a
            // character, the broken bytes byte by byte.
            r##"(Regexp.new("あ\xff".dup.force_encoding("UTF-8")); nil) rescue [$!.class, $!.message]"##,
            // The flags the pattern was given are part of the
            // rendering, in `rb_reg_desc`'s m-i-x order.
            r##"(Regexp.new("\x81".dup.force_encoding("Shift_JIS"), Regexp::IGNORECASE); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("\x81".dup.force_encoding("Shift_JIS"), Regexp::IGNORECASE | Regexp::MULTILINE | Regexp::EXTENDED); nil) rescue [$!.class, $!.message]"##,
            // `Regexp.escape` does not make a broken source whole.
            r##"(Regexp.new(Regexp.escape("a\xa4".dup.force_encoding("EUC-JP"))); nil) rescue [$!.class, $!.message]"##,
            // BINARY has a character per byte, so it is never broken.
            r##"Regexp.new("a\xa4".dup.force_encoding("BINARY")).source.bytes.inspect"##,
            // A source that *is* valid in its own encoding still compiles.
            r##"Regexp.new("\x82\xa0".dup.force_encoding("Shift_JIS")).source.bytes.inspect"##,
            r##"Regexp.new("\xa4\xa2".dup.force_encoding("EUC-JP")).source.bytes.inspect"##,
            r##"Regexp.new("あ").source"##,
        ]);
    }

    #[test]
    fn regexp_escaped_byte_too_short_for_its_codec() {
        // The other half of a truncated multibyte character: written as
        // a `\xHH` escape rather than raw, it survives preprocessing
        // and Onigmo is the one that finds it. CRuby renames the
        // engine's "too short multibyte code string" to "too short
        // escaped multibyte character" there — a different complaint
        // from the raw-byte one above, and the one that actually
        // reaches a caller.
        run_tests(&[
            r##"(Regexp.new("\\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("a\\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.new("\\x81".dup.force_encoding("Shift_JIS")); nil) rescue [$!.class, $!.message]"##,
            // A complete escaped character is fine.
            r##"Regexp.new("\\xa4\\xa2".dup.force_encoding("EUC-JP")).source.bytes.inspect"##,
        ]);
    }

    #[test]
    fn marshalled_regexp_with_a_broken_source_is_refused() {
        // A `/` payload carries whatever bytes the dump held, so it can
        // hand the engine a source `Regexp.new` would have refused.
        // CRuby preprocesses it like any other and raises rather than
        // building a Regexp nothing can match.
        run_tests(&[
            r##"d = Marshal.dump(Regexp.new("\xa4\xa2".dup.force_encoding("EUC-JP")));
                broken = d.sub("\xa4\xa2".dup.force_encoding("BINARY"), "a\xa4".dup.force_encoding("BINARY"));
                (Marshal.load(broken); nil) rescue [$!.class, $!.message]"##,
            // A sound one still round-trips.
            r##"d = Marshal.dump(Regexp.new("\xa4\xa2".dup.force_encoding("EUC-JP")));
                re = Marshal.load(d); [re.source.bytes, re.encoding.to_s]"##,
        ]);
    }

    #[test]
    fn regexp_union_member_broken_in_its_own_encoding() {
        // `Regexp.union` ends in `rb_reg_new_str`, so a member that is
        // broken in its own encoding is refused like any other source —
        // and the pattern the error renders is the *join*, not the
        // offending member.
        run_tests(&[
            r##"(Regexp.union("a\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.union("x", "a\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.union("a\xa4".dup.force_encoding("EUC-JP"), "x"); nil) rescue [$!.class, $!.message]"##,
            r##"(Regexp.union(["x", "a\xa4".dup.force_encoding("EUC-JP")]); nil) rescue [$!.class, $!.message]"##,
            // The per-member encoding combine still runs first, so two
            // members in *different* encodings are an ArgumentError
            // about the pair rather than about either one's bytes.
            r##"(Regexp.union("\x81".dup.force_encoding("Shift_JIS"), "a\xa4".dup.force_encoding("EUC-JP")); nil) rescue [$!.class, $!.message]"##,
            // BINARY has a character per byte, so it is never broken.
            r##"Regexp.union("a\xa4".dup.force_encoding("BINARY")).source.bytes.inspect"##,
        ]);
    }

    #[test]
    fn regexp_timeout_raises_timeout_error() {
        // `(a+)+\1b` is the shape that still backtracks exponentially in
        // CRuby: its backreference is what keeps the linear-time
        // memoization (`Regexp.linear_time?`) from applying, so both
        // engines really run the match and both have to be cut short by
        // the deadline. A pattern CRuby memoizes — `^(a*)*$` — would
        // return instantly there and prove nothing.
        run_tests(&[
            r##"Regexp::TimeoutError.superclass.to_s"##,
            r##"Regexp::TimeoutError.ancestors[0, 3].inspect"##,
            // The regexp's own timeout.
            r##"re = Regexp.new('(a+)+\1b', timeout: 0.05)
                (re =~ ("a" * 40 + "c"); "no raise") rescue [$!.class, $!.message]"##,
            // The global one.
            r##"Regexp.timeout = 0.05
                r = ((/(a+)+\1b/ =~ ("a" * 40 + "c")); "no raise") rescue [$!.class, $!.message]
                Regexp.timeout = nil
                r"##,
            // It is a RegexpError, so the older rescue still catches it.
            r##"Regexp.timeout = 0.05
                r = ((/(a+)+\1b/ =~ ("a" * 40 + "c")); "no raise") rescue (RegexpError === $! ? $!.class.to_s : "not a RegexpError")
                Regexp.timeout = nil
                r"##,
            // The regexp's own value wins over a global that would not
            // have fired.
            r##"Regexp.timeout = 50
                re = Regexp.new('(a+)+\1b', timeout: 0.05)
                r = ((re =~ ("a" * 40 + "c")); "no raise") rescue $!.class.to_s
                Regexp.timeout = nil
                r"##,
            // The String methods run the same matcher, so they are bound too.
            r##"Regexp.timeout = 0.05
                r = (("a" * 40 + "c").match?(/(a+)+\1b/); "no raise") rescue $!.class.to_s
                Regexp.timeout = nil
                r"##,
            r##"Regexp.timeout = 0.05
                r = (("a" * 40 + "c").sub(/(a+)+\1b/, "x"); "no raise") rescue $!.class.to_s
                Regexp.timeout = nil
                r"##,
            r##"Regexp.timeout = 0.05
                r = (("a" * 40 + "c").scan(/(a+)+\1b/); "no raise") rescue $!.class.to_s
                Regexp.timeout = nil
                r"##,
            // With no timeout in force nothing is bounded, and a match
            // that raised does not leave a deadline behind.
            r##"[/\d+/ =~ "abc 123", "hello".sub(/l+/, "L")].inspect"##,
            r##"Regexp.timeout = 0.05
                (/(a+)+\1b/ =~ ("a" * 40 + "c")) rescue nil
                Regexp.timeout = nil
                /\d+/ =~ "abc 123""##,
            // The accessors still read back as they did.
            r##"[Regexp.new("a", timeout: 3).timeout, Regexp.new("a").timeout].inspect"##,
        ]);
    }

    #[test]
    fn regexp_union_ascii_incompatible() {
        // A single ASCII-incompatible (UTF-16) arg pins the result.
        run_test(r#"Regexp.union("a".encode("UTF-16LE")).encoding.to_s"#);
        run_test(r#"Regexp.new("a".encode("UTF-16LE")).encoding.to_s"#);
        // ASCII-incompatible + ASCII-only -> "ASCII incompatible encoding".
        run_test(
            r#"(Regexp.union("a".encode("UTF-16LE"), "b".encode("UTF-8")); nil) rescue [$!.class, $!.message]"#,
        );
        run_test(
            r#"(Regexp.union(Regexp.new("a".encode("UTF-16LE")), Regexp.new("b".encode("UTF-8"))); nil) rescue [$!.class, $!.message]"#,
        );
        // Two conflicting ASCII-incompatible encodings -> names both.
        run_test(
            r#"(Regexp.union(Regexp.new("a".encode("UTF-16LE")), Regexp.new("b".encode("UTF-16BE"))); nil) rescue [$!.class, $!.message]"#,
        );
        // ASCII-incompatible + non-ASCII content in a different encoding.
        run_test(
            r#"(Regexp.union("a".encode("UTF-16LE"), "©".encode("ISO-8859-1")); nil) rescue [$!.class, $!.message]"#,
        );
    }

    #[test]
    fn regexp_source_encoding() {
        // A 7-bit, unpinned source is US-ASCII; a pinned encoding (u/e/s
        // modifier, non-ASCII `\u{}` escape, non-ASCII source) carries
        // that encoding.
        run_test(
            r#"[ /abc/, /abc/u, /abc/e, /\u{61}/, /\u{3042}/,
                Regexp.new("abc"), Regexp.new("\u{ff}"), Regexp.new("ほげ") ]
              .map { |r| r.source.encoding.to_s }"#,
        );
    }

    #[test]
    fn regexp_interpolation_encoding_modifiers() {
        // An interpolated regexp with an `n`/`u`/`e`/`s` encoding modifier
        // must not feed the letter into an Onigmo group option (`(?n)` —
        // which used to raise "undefined group option"); the modifier fixes
        // the declared encoding while the interpolated bytes form the source.
        run_test(
            r#"x = "a"
               [ /#{x}/n, /#{x}/u, /#{x}/e, /#{x}/s, /#{x}/ ]
                 .map { |r| r.encoding.to_s }"#,
        );
        // `i`/`m`/`x` still lower to an inline `(?imx)` group and the
        // interpolated fragments still form the source verbatim.
        run_test(
            r#"x = "b"
               r = /a#{x}c/imx
               [r.source, (r =~ "xABCx"), r.match("aBc")[0]]"#,
        );
        // A modifier combined with `i`, and with a leading empty fragment.
        run_test(r#"x = "Z"; (/#{x}/i =~ "qzq")"#);
    }

    #[test]
    fn regexp_nested_quantifier_not_reduced() {
        // Bug #17341 semantics (ported into our Onigmo fork): a+?*
        // behaves as (a+?)*; the match consumes all input.
        run_test(r#"eval("/a+?*/").match("aa")[0]"#);
        run_test(r#"eval("/a+?*/").match("")[0]"#);
        run_test(r#"eval("/a+?+/").match("aa")[0]"#);
        // The reduction-with-warning case still matches like CRuby.
        run_test(r#"eval("/foo(A{0,1}+)Abar/").match("fooAAAbar").to_a"#);
    }

    #[test]
    fn regexp_word_class_join_control() {
        // CRuby >= 3.4: [[:word:]] / \p{Word} include Join_Control
        // (U+200C/200D) per UTS #18; \w stays ASCII-only.
        run_test(r#"["‌" =~ /[[:word:]]/, "‍" =~ /[[:word:]]/, "‌" =~ /\w/]"#);
    }

    #[test]
    fn regexp_match_native_encoding() {
        // Windows-31J subject: "\xC3\xA9" is TWO single-byte chars in
        // Windows-31J (0xC3 is not a lead byte) even though the same
        // bytes are one char ("é") as UTF-8. `/./s` must match one
        // byte, and the capture must carry the subject's encoding —
        // exercises the native-encoding byte-match path
        // (`captures_bytes_from_pos` / `from_captures_bytes`).
        run_test(
            r#"m = /./s.match("\303\251".dup.force_encoding(Encoding::Windows_31J))
               [m[0].bytes, m[0].encoding.to_s, m.pre_match.bytes, m.post_match.bytes]"#,
        );
        // Interpolated form (spec: "with interpolation" / "and /o").
        run_test(
            r#"m = /#{/./}/s.match("\303\251".dup.force_encoding(Encoding::Windows_31J))
               m.to_a.map(&:bytes)"#,
        );
        // EUC-JP: 0xA4A2 ("あ") is one 2-byte char; `/./e` matches both bytes.
        run_test(
            r#"m = /./e.match("\244\242".dup.force_encoding(Encoding::EUC_JP))
               [m[0].bytes, m[0].encoding.to_s]"#,
        );
        // ISO-8859-1: every byte is one char.
        run_test(
            r#"s = "\351x".dup.force_encoding(Encoding::ISO_8859_1)
               m = Regexp.new(".".dup.force_encoding(Encoding::ISO_8859_1)).match(s)
               [m[0].bytes, m[0].encoding.to_s, m.post_match.bytes]"#,
        );
        // Capture groups + named backrefs still work on the native path.
        run_test(
            r#"s = "\303\251\303".dup.force_encoding(Encoding::Windows_31J)
               m = /(.)(.)/s.match(s)
               [m[1].bytes, m[2].bytes, m.to_a.size, $~[0].bytes, $1.bytes]"#,
        );
        // `pos` argument: char offset converts to byte offset under the
        // subject's encoding.
        run_test(
            r#"s = "\303\251\303".dup.force_encoding(Encoding::Windows_31J)
               m = /./s.match(s, 2)
               [m[0].bytes, m.begin(0)]"#,
        );
        // No match on the native path clears $~ and returns nil.
        run_test(
            r#"s = "\303".dup.force_encoding(Encoding::Windows_31J)
               [/z/s.match(s), $~]"#,
        );
        // Pure 7-bit subject stays equivalent regardless of path.
        run_test(
            r#"m = /b./s.match("abc".dup.force_encoding(Encoding::Windows_31J))
               [m[0], m[0].encoding.to_s]"#,
        );
        // MatchData#string snapshot preserves the original bytes/encoding.
        run_test(
            r#"s = "\303\251".dup.force_encoding(Encoding::Windows_31J)
               m = /./s.match(s)
               [m.string.bytes, m.string.encoding.to_s, m.string.frozen?]"#,
        );
    }

    #[test]
    fn regexp_match_iso8859_variants() {
        // Every ISO-8859-N Onigmo codec: single high byte is one char,
        // so `/./` (compiled in the subject's encoding) matches exactly
        // that byte. Exercises each arm of onigmo_encoding_for.
        run_test(
            r#"%w[ISO-8859-1 ISO-8859-2 ISO-8859-3 ISO-8859-4 ISO-8859-5
                 ISO-8859-6 ISO-8859-7 ISO-8859-8 ISO-8859-9 ISO-8859-10
                 ISO-8859-11 ISO-8859-13 ISO-8859-14 ISO-8859-15 ISO-8859-16]
                .map do |name|
                  enc = Encoding.find(name)
                  s = "\341z".dup.force_encoding(enc)
                  m = Regexp.new(".".dup.force_encoding(enc)).match(s)
                  [name, m[0].bytes, m[0].encoding == enc]
                end"#,
        );
        // Single-byte NamedByte encodings mapped to native Onigmo
        // codecs (KOI8 / Windows-125x family).
        run_test(
            r#"%w[KOI8-R KOI8-U Windows-1250 Windows-1251 Windows-1252
                 Windows-1253 Windows-1254 Windows-1257].map do |name|
                  enc = Encoding.find(name)
                  s = "\341z".dup.force_encoding(enc)
                  m = Regexp.new(".".dup.force_encoding(enc)).match(s)
                  [name, m[0].bytes, m[0].encoding == enc]
                end"#,
        );
    }
}
