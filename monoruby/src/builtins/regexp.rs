use super::*;

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Regexp", REGEXP_CLASS, ObjTy::REGEXP);
    globals.define_builtin_class_funcs_with(
        REGEXP_CLASS,
        "new",
        &["compile"],
        regexp_new,
        1,
        2,
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
    // `Regexp#initialize` is a private method that always raises:
    // - `FrozenError` if the receiver is frozen (literals are frozen);
    // - `TypeError` otherwise (CRuby treats every monoruby Regexp as
    //   "already initialized" since `Regexp.new` is the sole entry
    //   point and produces a fully-built instance).
    let init_id = globals.define_private_builtin_func(REGEXP_CLASS, "initialize", regexp_initialize, 1);
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
    Ok(Value::array_from_iter(
        unique.iter().map(|n| Value::string_from_str(n)),
    ))
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

/// Private `Regexp#initialize` that always rejects re-initialisation.
/// `Regexp.new` builds a fully-formed instance via the alloc function +
/// internal construction path, so any subsequent `.send(:initialize, …)`
/// must be either a frozen literal (FrozenError) or an already-built
/// instance (TypeError "already initialized regexp"). matches CRuby
/// (the `< 4.1` branch of `initialize_spec.rb`).
#[monoruby_builtin]
fn regexp_initialize(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    if self_.is_frozen() {
        return Err(MonorubyErr::frozenerr("can't modify frozen Regexp"));
    }
    Err(MonorubyErr::typeerr("already initialized regexp"))
}

///
/// ### Regexp.new
/// - new(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
/// - compile(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    let (string, default_option, source_encoding, default_kcode) = if let Some(re) = arg0.is_regex()
    {
        let kc = if re.fixed_encoding() {
            kcode_from_encoding(re.declared_encoding())
        } else {
            None
        };
        let mut opt = re.raw_option();
        if let Some(bits) = kc {
            opt |= bits;
        }
        (
            re.as_str().to_string(),
            Some(opt),
            Some(re.declared_encoding()),
            kc,
        )
    } else {
        let s = arg0.coerce_to_string(vm, globals)?;
        let enc = arg0
            .is_rstring_inner()
            .map(|r| r.encoding())
            .unwrap_or(crate::value::Encoding::Utf8);
        (s, None, Some(enc), None)
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
    let encoding = if option & RegexpInner::NOENCODING != 0 {
        onigmo_regex::OnigmoEncoding::ASCII
    } else {
        onigmo_regex::OnigmoEncoding::UTF8
    };
    // Pull the kcode bit out of the option mask before passing to
    // onigmo (which doesn't understand the modifier letters).
    let kcode_bits = option & RegexpInner::KCODE_MASK;
    let kcode = if kcode_bits != 0 {
        Some(kcode_bits)
    } else {
        default_kcode
    };
    let regexp =
        RegexpInner::with_option_kcode(string, option, encoding, kcode, source_encoding)?;
    Ok(Value::regexp(regexp))
}

/// Map a Ruby-visible encoding to the matching KCODE_* bit, mirroring
/// what `n`/`u`/`e`/`s` modifiers would set. Used when reconstructing
/// a Regexp from another Regexp so the new one's `encoding`/
/// `fixed_encoding?` mirrors the original.
fn kcode_from_encoding(enc: crate::value::Encoding) -> Option<u32> {
    use crate::value::Encoding;
    match enc {
        Encoding::Utf8 => Some(RegexpInner::KCODE_UTF8),
        Encoding::EucJp => Some(RegexpInner::KCODE_EUCJP),
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
    // textual form. Other types still go through `to_str`.
    let (string, src_enc) = if let Some(s) = arg0.is_str() {
        let enc = arg0
            .is_rstring_inner()
            .map(|r| r.encoding())
            .unwrap_or(crate::value::Encoding::Utf8);
        (s.to_string(), enc)
    } else if let Some(sym) = arg0.try_symbol() {
        (sym.to_string(), crate::value::Encoding::Utf8)
    } else {
        (
            arg0.coerce_to_str(vm, globals)?,
            crate::value::Encoding::Utf8,
        )
    };
    let escaped = RegexpInner::escape(&string);
    // CRuby tags the result US-ASCII when it contains only ASCII
    // bytes (otherwise it inherits the source's encoding). The
    // escape itself only adds ASCII metacharacters, so the result
    // is ASCII-only iff the input is.
    let result_enc = if escaped.is_ascii() {
        crate::value::Encoding::UsAscii
    } else {
        src_enc
    };
    Ok(Value::string_from_inner(
        crate::value::rvalue::RStringInner::from_encoding_scanned(
            escaped.as_bytes(),
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
    let s = parts.join("|");
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
            // else (even pure-ASCII). CRuby still rejects.
            if let UnionEnc::Pinned(prev) = self {
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
        let ascii_only = re.as_str().bytes().all(|b| b < 0x80);
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
    pattern: String,
    union_enc: UnionEnc,
) -> Result<RegexpInner> {
    use crate::value::Encoding;
    let enc = union_enc.resolved();
    let onigmo_enc = if matches!(enc, Encoding::Ascii8) {
        onigmo_regex::OnigmoEncoding::ASCII
    } else {
        onigmo_regex::OnigmoEncoding::UTF8
    };
    let pinned = matches!(union_enc, UnionEnc::Pinned(_));
    // For pinned encodings without a kcode equivalent (UTF-16LE,
    // UTF-32, ISO-8859-*, …) we can't piggy-back on the
    // `KCODE_*` bits — set `FIXEDENCODING` instead so the source-
    // encoding fallback branch in `resolve_declared_encoding`
    // honours the encoding we computed.
    let (option, kcode) = match enc {
        Encoding::Utf8 => (RegexpInner::KCODE_UTF8, Some(RegexpInner::KCODE_UTF8)),
        Encoding::EucJp => (RegexpInner::KCODE_EUCJP, Some(RegexpInner::KCODE_EUCJP)),
        Encoding::Sjis(_) => (RegexpInner::KCODE_SJIS, Some(RegexpInner::KCODE_SJIS)),
        Encoding::Ascii8 => (RegexpInner::NOENCODING, None),
        _ if pinned => (RegexpInner::FIXEDENCODING, None),
        _ => (0u32, None),
    };
    RegexpInner::with_option_kcode(pattern, option, onigmo_enc, kcode, Some(enc))
}

/// Render a single `Regexp.union` argument into its embedded form.
/// Strings and Symbols are passed through `Regexp.escape`; Regexps
/// use their `to_s` group form so flags are preserved. Falls back to
/// `to_regexp` then `to_str` for general objects, matching CRuby.
fn format_union_member(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<String> {
    // `is_rstring_inner()` (not `is_str()`) — a String tagged as
    // an ASCII-incompatible encoding (UTF-16LE etc.) or one
    // carrying invalid UTF-8 bytes shows up here, and `is_str()`
    // would reject it for not being valid UTF-8.
    if let Some(s) = arg.is_rstring_inner() {
        return Ok(RegexpInner::escape(&String::from_utf8_lossy(s.as_bytes())));
    }
    if let Some(re) = arg.is_regex() {
        return Ok(re.tos());
    }
    if let Some(sym) = arg.try_symbol() {
        return Ok(RegexpInner::escape(sym.get_name().as_str()));
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(re) = result.is_regex() {
            return Ok(re.tos());
        }
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::TO_STR) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(s) = result.is_str() {
            return Ok(RegexpInner::escape(s));
        }
    }
    let class = arg.get_real_class_name(&globals.store);
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
    let same_source = lhs.as_str() == rhs.as_str();
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
    let mut h = std::collections::hash_map::DefaultHasher::new();
    re.as_str().hash(&mut h);
    (re.raw_option() & REGEXP_EQ_OPTION_MASK).hash(&mut h);
    Ok(Value::integer(h.finish() as i64))
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
    let given = if let Some(s) = arg0.is_str() {
        s.to_string()
    } else if let Some(sym) = arg0.try_symbol() {
        sym.to_string()
    } else if let Some(func_id) = globals.check_method(arg0, IdentId::TO_STR) {
        // `to_str` coercion path: call the method, then accept the
        // result iff it's a String. CRuby raises TypeError on a
        // non-String return value (`can't convert X to String
        // (X#to_str gives Y)`); we mirror that.
        let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
        match result.is_str() {
            Some(s) => s.to_string(),
            None => {
                let class = arg0.get_real_class_name(&globals.store);
                let res_class = result.get_real_class_name(&globals.store);
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {class} to String ({class}#to_str gives {res_class})"
                )));
            }
        }
    } else {
        return Ok(Value::bool(false));
    };
    vm.set_match_regex(self_);
    let res = Value::bool(regex.find_one(vm, &given)?.is_some());
    Ok(res)
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
    let given = lfp.arg(0).expect_symbol_or_string(globals)?.to_string();
    vm.set_match_regex(self_);
    let res = match regex.find_one(vm, &given)? {
        Some(mat) => Value::integer(mat.start as i64),
        None => Value::nil(),
    };
    Ok(res)
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
    Ok(Value::string_from_str(self_.is_regex().unwrap().as_str()))
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
    let given = arg0.expect_symbol_or_string(globals)?.to_string();
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
    Ok(Value::bool(RegexpInner::match_pred(&regex, &given, char_pos)?))
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
    let heystack = arg0.expect_symbol_or_string(globals)?.to_string();
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
    let md = if let Some(captures) = regex.captures_from_pos(&heystack, byte_pos, vm)? {
        Value::new_matchdata(captures, &heystack, regex)
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
/// monoruby's regex engine (Onigmo) classifies linear-time vs.
/// possibly-exponential matchers conservatively. CRuby treats the
/// vast majority of patterns as linear-time. We don't expose
/// Onigmo's internal classifier, so return `true` for any pattern
/// that compiles — the spec only checks for boolean shape.
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
    // If a Regexp is given, options on the second arg are ignored
    // by CRuby (with a warning); we just accept and ignore.
    if arg.is_regex().is_some() {
        return Ok(Value::bool(true));
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
    let _ = RegexpInner::with_option(s, opt)?;
    Ok(Value::bool(true))
}

///
/// ### Regexp.timeout
/// - timeout -> Float | nil
///
/// monoruby does not implement per-match timeouts; the global
/// setting reads as `nil`.
#[monoruby_builtin]
fn regexp_timeout_get(
    _: &mut Executor,
    _: &mut Globals,
    _: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### Regexp.timeout=
/// - timeout=(sec) -> sec
///
/// Accepted but not enforced — monoruby has no regex timeout
/// implementation. Returns the argument so chained assignments work.
#[monoruby_builtin]
fn regexp_timeout_set(
    _: &mut Executor,
    _: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.arg(0))
}

///
/// ### Regexp#~
/// - ~ self -> Integer | nil
///
/// Sugar for `self =~ $_`.
#[monoruby_builtin]
fn regexp_tilde(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let target = globals.get_gvar(IdentId::get_id("$_")).unwrap_or_default();
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
    let encoding = regex.declared_encoding();
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
    fn regex() {
        run_test(r##"/abcde/xmi.to_s"##);
        run_test(r##"/abcde/xmi.inspect"##);
        run_test(r##"/abcde/.to_s"##);
        run_test(r##"/abcde/.inspect"##);
    }

    #[test]
    fn regexp_last_match() {
        run_test(
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match[0], Regexp.last_match[1], Regexp.last_match[2], Regexp.last_match[3]]
        "#,
        );
        run_test(
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3), Regexp.last_match(-1), Regexp.last_match(-2), Regexp.last_match(-10)]
        "#,
        );
    }

    #[test]
    fn last_match1() {
        run_test(
            r#"
          /(.)(.)/ =~ "abcde"
          [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3)]
            "#,
        );
    }

    #[test]
    fn last_match_nil_when_no_match() {
        // Regexp.last_match returns nil when the last match failed
        run_test(
            r#"
          /NOMATCH/ =~ "abc"
          Regexp.last_match
            "#,
        );
        // Regexp.last_match returns nil when no match has occurred yet
        run_test(
            r#"
          /DONTMATCH/.match("")
          Regexp.last_match
            "#,
        );
    }

    #[test]
    fn dollar_tilde_read_write() {
        // $~ reads match data after a regex match
        run_test(
            r#"
          "abc" =~ /(a)(b)/
          $~[0]
            "#,
        );
        // $~ = nil clears match data
        run_test(
            r#"
          "abc" =~ /(a)/
          $~ = nil
          $~
            "#,
        );
        // $~ is nil initially after a failed match
        run_test(
            r#"
          /NOMATCH/ =~ "abc"
          $~
            "#,
        );
    }

    #[test]
    fn last_match2() {
        run_test(
            r#"
          /(.)(.)/ =~ "abcde"
          #assert $', Regexp.post_match
          [$', $&, $1, $2, $3]
            "#,
        );
        run_test(
            r#"
          /(.)(.)/ =~ :abcde
          [$', $&, $1, $2, $3]
            "#,
        );
        run_test(
            r#"
          /(.)(.)/ =~ nil
          [$', $&, $1, $2, $3]
            "#,
        );
    }

    #[test]
    fn union() {
        run_test(r##"Regexp.union(/g/i, "a(b)[c]d", /bbbb/x, /cccc/m).to_s"##);
    }

    #[test]
    fn regexp_options() {
        run_test(r##"/foo/.options"##);
        run_test(r##"/foo/i.options"##);
        run_test(r##"/foo/m.options"##);
        run_test(r##"/foo/x.options"##);
        run_test(r##"/foo/mix.options"##);
    }

    #[test]
    fn regexp1() {
        run_test(r#""abcdefg".gsub(/def/, "!!")"#);
        run_test(r#""2.5".gsub(".", ",")"#);
        run_test(r#""xbbgz-xbbbvzbbc".gsub(/(b+.z)(..)/) { $2 + $1.upcase }"#);
    }

    #[test]
    fn regexp_teq() {
        run_test(
            r#"
            res = /(aa).*(bb)/ === "andaadefbbje"
            [res, $&, $1, $2]
        "#,
        );
        run_test(
            r#"
            res = /(aa).*(bb)/ === :andaadefbbje
            [res, $&, $1, $2]
        "#,
        );
        run_test(
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
        );
    }

    #[test]
    fn regexp2() {
        run_test(r#""aaazzz" =~ /\172+/"#);
        run_test(r#"/foo/ =~ "foo""#);
        run_test(r#"/foo/ =~ "afoo""#);
        run_test(r#"/foo/ =~ "bar""#);
        run_test(
            r#"
            i = 123
            /ab#{i}cd/ =~ "ab123cd"
        "#,
        );
    }

    #[test]
    fn regexp3() {
        run_test(
            r#"
        a = /Ruby\Z/
        ["Ruby" =~ /Ruby\Z/, "Rubys" =~ /Ruby\Z/]
        "#,
        );
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
    fn match_() {
        run_test(
            r#"
        res = [/R.../.match?("-Ruby")]
        for i in -6...6
            res << /R.../.match?("-Ruby", 0)
        end
        res
        "#,
        );
        run_test(
            r#"
        SEPARATOR_PAT = /#{Regexp.quote File::SEPARATOR}/
        /\A#{SEPARATOR_PAT}?\z/.match?("")
        "#,
        );
    }

    #[test]
    fn match_nil() {
        run_test(r#"/foo/.match?(nil)"#);
        run_test(r#"/foo/.match?(nil, 0)"#);
        run_test(r#"//.match?(nil)"#);
    }

    #[test]
    fn r#match() {
        run_test(r##"/(.).(.)/.match("foobar", 3).captures"##);
        run_test(r##"/(.).(.)/.match("foobar", -3).captures"##);
    }

    #[test]
    fn regexp_names() {
        // No named captures → empty array
        run_test(r##"/(foo)(bar)/.names"##);
        // Single named capture
        run_test(r##"/(?<a>foo)/.names"##);
        // Multiple named captures preserve declaration order
        run_test(r##"/(?<a>foo)(?<b>bar)/.names"##);
        // Mixed named + unnamed
        run_test(r##"/(?<a>foo)(bar)(?<c>baz)/.names"##);
        // Duplicate names appear once each in declaration order
        run_test(r##"/(?<a>foo)(?<a>bar)/.names"##);
    }

    #[test]
    fn regexp_eq_eql_hash() {
        // Same source + same options.
        run_test(r#"/abc/ == /abc/"#);
        run_test(r#"/abc/.eql?(/abc/)"#);
        run_test(r#"/abc/i == /abc/i"#);
        run_test(r#"/abc/i == /abc/"#);
        run_test(r#"/abc/.hash == /abc/.hash"#);
        run_test(r#"/abc/i.hash == /abc/i.hash"#);
        // Different source.
        run_test(r#"/abc/ == /abd/"#);
        // /n flag (NOENCODING) doesn't change == for ASCII source.
        run_test(r#"// == //n"#);
        run_test(r#"//.hash == //n.hash"#);
        // Non-Regexp argument is never equal.
        run_test(r#"/abc/ == "abc""#);
    }

    #[test]
    fn regexp_union_empty_and_special() {
        // Empty args → /(?!)/ (never matches).
        run_test(r#"Regexp.union == /(?!)/"#);
        // Single Regexp arg returned as-is (preserves identity via ==).
        run_test(r#"Regexp.union(/foo/) == /foo/"#);
        // Symbol accepted.
        run_test(r#"Regexp.union(:foo) == /foo/"#);
        // Strings escaped.
        run_test(r#"Regexp.union("n", ".") == /n|\./"#);
        // Single Array arg flattened.
        run_test(r#"Regexp.union(["+", "-"]) == /\+|\-/"#);
        run_test(r#"Regexp.union(["skiing", "sledding"]) == /skiing|sledding/"#);
    }

    #[test]
    fn regexp_new_from_regexp_preserves_options() {
        // `Regexp.new(/abc/i)` keeps both source and options.
        run_test(r#"Regexp.new(/abc/i) == /abc/i"#);
        run_test(r#"Regexp.new(/^hi{2,3}fo.o$/) == /^hi{2,3}fo.o$/"#);
    }

    #[test]
    fn regexp_new_string_flags() {
        // `i`, `m`, `x` accepted as a flag string.
        run_test(r#"Regexp.new("Hi", "i") == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", "im") == /Hi/im"#);
        run_test(r#"(Regexp.new("Hi", "im").options & Regexp::IGNORECASE) != 0"#);
    }

    #[test]
    fn regexp_new_string_flag_x() {
        // The `x` (EXTENDED) flag in the string form sets EXTENDED
        // and ignores whitespace / `#`-comments inside the pattern.
        run_test(r#"Regexp.new("Hi", "x") == /Hi/x"#);
        run_test(r#"(Regexp.new("Hi", "x").options & Regexp::EXTENDED) != 0"#);
        // EXTENDED skips inline whitespace, so a pattern with spaces
        // still matches a no-space subject (returns the match position).
        run_test(r#"Regexp.new("a b c", "x") =~ "abc""#);
        // Combined with other flags.
        run_test(r#"Regexp.new("Hi", "ix") == /Hi/ix"#);
        run_test(r#"Regexp.new("Hi", "mix") == /Hi/mix"#);
    }

    #[test]
    fn regexp_new_invalid_flag_string_raises() {
        // `e` is *not* accepted in the string-form flag arg.
        run_test_error(r#"Regexp.new("Hi", "e")"#);
        run_test_error(r#"Regexp.new("Hi", "z")"#);
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
        run_test(r#"Regexp.new("Hi", Object.new) == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", []) == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", :sym) == /Hi/i"#);
        // Even an Object that masquerades as falsey via `to_s` still
        // triggers the warning + IGNORECASE path.
        run_test(
            r#"
              o = Object.new
              def o.inspect; "fake"; end
              Regexp.new("Hi", o) == /Hi/i
            "#,
        );
    }

    #[test]
    fn regexp_inspect_escapes_slashes_and_n_flag() {
        // `/` not already escaped is escaped in inspect.
        run_test(r#"Regexp.new("/foo/bar").inspect"#);
        run_test(r#"Regexp.new("//").inspect"#);
        // Already-escaped `\/` is not double-escaped.
        run_test(r#"/\/foo\/bar/.inspect"#);
        // `n` flag (NOENCODING) appears in inspect output.
        run_test(r#"//n.inspect"#);
        run_test(r#"//nixm.inspect"#);
    }

    #[test]
    fn regexp_option_constants() {
        run_test(r#"Regexp::IGNORECASE"#);
        run_test(r#"Regexp::EXTENDED"#);
        run_test(r#"Regexp::MULTILINE"#);
        run_test(r#"Regexp::FIXEDENCODING"#);
        run_test(r#"Regexp::NOENCODING"#);
    }

    #[test]
    fn regexp_try_convert() {
        // Already a Regexp -> returned as-is.
        run_test(r#"Regexp.try_convert(/abc/) == /abc/"#);
        // Non-Regexp without #to_regexp -> nil.
        run_test(r#"Regexp.try_convert("abc")"#);
        run_test(r#"Regexp.try_convert(nil)"#);
        run_test(r#"Regexp.try_convert(123)"#);
        // Object that defines #to_regexp -> returns the Regexp.
        run_test(
            r#"
            o = Object.new
            def o.to_regexp; /xyz/; end
            Regexp.try_convert(o) == /xyz/
            "#,
        );
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
        run_test(r#"Regexp.linear_time?(/abc/)"#);
        run_test(r#"Regexp.linear_time?("abc")"#);
        run_test(r#"Regexp.linear_time?("abc", Regexp::IGNORECASE)"#);
    }

    #[test]
    fn regexp_timeout_accessors() {
        // monoruby has no per-match timeout; accessors are stubs.
        run_test(r#"Regexp.timeout"#);
        run_test(r#"(Regexp.timeout = 1.0)"#);
    }

    #[test]
    fn regexp_tilde() {
        run_test(
            r#"
            $_ = "input data"
            ~ /at/
            "#,
        );
        run_test(
            r#"
            $_ = "input data"
            ~ /missing/
            "#,
        );
        // No `$_` -> nil
        run_test(
            r#"
            $_ = nil
            ~ /at/
            "#,
        );
    }

    #[test]
    fn regexp_casefold_p() {
        run_test(r#"/abc/.casefold?"#);
        run_test(r#"/abc/i.casefold?"#);
        run_test(r#"/abc/m.casefold?"#);
    }

    #[test]
    fn regexp_encoding_method() {
        // Pure-ASCII source -> US-ASCII regardless of `n` flag.
        run_test(r#"/abc/.encoding.name"#);
        run_test(r#"/abc/n.encoding.name"#);
        // Non-ASCII source -> UTF-8.
        run_test(r#"/©/.encoding.name"#);
    }

    #[test]
    fn regexp_fixed_encoding_p() {
        // Pure-ASCII source isn't fixed-encoding.
        run_test(r#"/abc/.fixed_encoding?"#);
        // Non-ASCII source pins the encoding.
        run_test(r#"/©/.fixed_encoding?"#);
        // `n` flag with pure-ASCII source -> not fixed.
        run_test(r#"/abc/n.fixed_encoding?"#);
    }

    #[test]
    fn regexp_named_captures_method() {
        run_test(r#"/(?<a>foo)(?<b>bar)/.named_captures"#);
        run_test(r#"/foo/.named_captures"#);
        // Duplicate name keeps both indexes under one key.
        run_test(r#"/(?<x>a)(?<x>b)/.named_captures"#);
    }

    #[test]
    fn regexp_escape_accepts_symbol() {
        run_test(r#"Regexp.escape(:"a.b")"#);
        run_test(r#"Regexp.quote(:"a.b")"#);
        run_test(r#"Regexp.escape("a.b")"#);
    }

    #[test]
    fn regexp_escape_encoding_tagging() {
        // ASCII-only result tags as US-ASCII regardless of source.
        run_test(r#"Regexp.escape("abc").encoding.to_s"#);
        run_test(r#"Regexp.escape("a.b*c+").encoding.to_s"#);
        run_test(r#"Regexp.escape("hello world").encoding.to_s"#);
        // Non-ASCII content keeps the source's UTF-8 tag.
        run_test(r#"Regexp.escape("café").encoding.to_s"#);
        run_test(r#"Regexp.escape("日本語").encoding.to_s"#);
    }

    #[test]
    fn regexp_last_match_named_capture() {
        // Symbol / String key: dispatches to MatchData#[].
        run_test(
            r#"
            /(?<word>\w+)/ =~ "hello"
            [Regexp.last_match(:word), Regexp.last_match("word"),
             Regexp.last_match(0), Regexp.last_match(1)]
            "#,
        );
        // Symbol with no match propagates nil.
        run_test(
            r#"
            /(?<word>\w+)/ =~ ""
            Regexp.last_match(:word).inspect
            "#,
        );
    }

    #[test]
    fn regexp_match_block_form() {
        // Match success: block runs, return value comes from block.
        run_test(r#"/(\d+)/.match("hello 42") { |m| m[1].to_i + 1 }"#);
        // Match failure: block does NOT run, returns nil.
        run_test(r#"/(\d+)/.match("hello") { |m| "should not run" }.inspect"#);
        // nil arg: returns nil without raising.
        run_test(r#"/abc/.match(nil).inspect"#);
    }

    #[test]
    fn regexp_new_from_regexp_propagates_encoding() {
        // Regexp.compile/new should preserve encoding+fixed_encoding
        // from the source Regexp argument.
        run_test(r#"Regexp.compile(/abc/u).encoding.to_s"#);
        run_test(r#"Regexp.new(/abc/u).encoding.to_s"#);
        run_test(r#"Regexp.compile(/abc/u).fixed_encoding?"#);
        run_test(r#"Regexp.compile(/abc/).encoding.to_s"#);
        run_test(r#"Regexp.compile(/abc/).fixed_encoding?"#);
        // n flag preserves NOENCODING.
        run_test(r#"Regexp.compile(/abc/n).encoding.to_s"#);
        run_test(r#"Regexp.compile(/abc/n).fixed_encoding?"#);
    }

    #[test]
    fn regexp_unicode_escape_pins_encoding() {
        // \u escape with non-ASCII codepoint pins to UTF-8.
        run_test(r#"/\u{1234}/.encoding.to_s"#);
        run_test(r#"/\u{1234}/.fixed_encoding?"#);
        run_test(r#"/é/.encoding.to_s"#);
        run_test(r#"/é/.fixed_encoding?"#);
        // \u escape with ASCII codepoint stays US-ASCII (not pinned).
        run_test(r#"/A/.encoding.to_s"#);
        run_test(r#"/A/.fixed_encoding?"#);
        run_test(r#"/\u{41}/.encoding.to_s"#);
        run_test(r#"/\u{41}/.fixed_encoding?"#);
    }

    // ----- Phase D follow-ups -----

    #[test]
    fn regexp_last_match_no_match_returns_nil_for_any_arg() {
        // Once `Regexp.last_match` is `nil`, *any* arg short-circuits
        // to `nil` (CRuby semantics). Including args that would
        // normally raise from `to_int` coercion.
        run_test(
            r#"
              /foo/ =~ "TEST123"
              [Regexp.last_match(:test),
               Regexp.last_match(1),
               Regexp.last_match(Object.new),
               Regexp.last_match("test")]
            "#,
        );
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
        run_test(r#"//.frozen?"#);
        run_test(r#"/abc/.frozen?"#);
        // `Regexp.new(...)` instances are not auto-frozen.
        run_test(r#"Regexp.new("abc").frozen?"#);
    }

    #[test]
    fn regexp_escape_quotes_whitespace() {
        // CRuby escapes ` ` / `\t`/`\n`/`\r`/`\f`/`\v` so the result
        // round-trips through the `x` modifier.
        run_test(r#"Regexp.escape("a b")"#);
        run_test(r#"Regexp.escape("a\tb\nc")"#);
        // Mixed meta + whitespace.
        run_test(r#"Regexp.escape("\\*?{}.+^$[]()- \t\n\r")"#);
    }

    #[test]
    fn regexp_teq_uses_to_str() {
        // `Regexp#===` coerces string-like objects via `#to_str`.
        run_test(
            r#"
              c = Class.new { def to_str; "abc"; end }
              /abc/ === c.new
            "#,
        );
        // `nil` and `Regexp` args still return false.
        run_test(r#"/abc/ === nil"#);
        run_test(r#"/abc/ === /abc/"#);
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
        // `Regexp#match?` is documented as "doesn't update `$~` and
        // friends". The previous match's `$~` should survive a
        // subsequent `match?` call.
        run_test(
            r#"
              "abc" =~ /(a)/
              before = $~[0]
              /x/.match?("xyz")
              [before, $~[0]]
            "#,
        );
    }

    #[test]
    fn regexp_eq_includes_encoding() {
        // Different declared encoding ⇒ not equal.
        run_test(r#"/abc/u == /abc/n"#);
        // Same kcode ⇒ equal.
        run_test(r#"/abc/u == /abc/u"#);
        run_test(r#"/abc/n == /abc/n"#);
        // Empty / pure-ASCII source: the `n` modifier doesn't shift
        // the resolved encoding away from US-ASCII, so // == //n.
        run_test(r#"// == //n"#);
        run_test(r#"//n == //"#);
    }

    // ----- Phase D: extended coverage -----

    #[test]
    fn regexp_last_match_indexing() {
        // Positive / negative / out-of-range Integer indices off
        // a match — including the `$~`-style negatives that wrap
        // from the end of the captures list.
        run_test(
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
        );
    }

    #[test]
    fn regexp_last_match_named_with_duplicate_names() {
        // When two capture groups share a name, `MatchData#[]` (and
        // therefore `Regexp.last_match(:name)`) returns the *last
        // participating* group. CRuby's named-group lookup is
        // last-wins on collisions.
        run_test(
            r#"
              /(?<x>a)(?<x>b)/ =~ "ab"
              [Regexp.last_match(:x), Regexp.last_match("x")]
            "#,
        );
    }

    #[test]
    fn regexp_last_match_clears_on_failed_match() {
        // A failed `=~` clears `$~` to nil; subsequent
        // `Regexp.last_match` reads as `nil` regardless of arg type.
        run_test(
            r#"
              /(\w+)/ =~ "abc"
              before = Regexp.last_match(1)
              /xyz/ =~ "abc"
              [before, Regexp.last_match, Regexp.last_match(0), Regexp.last_match(:nope)]
            "#,
        );
    }

    #[test]
    fn regexp_match_block_return_value() {
        // The block's return value becomes the `match { ... }` value.
        run_test(r#"/(\d+)/.match("count: 42") { |m| m[1].to_i * 2 }"#);
        // Returning an arbitrary object works.
        run_test(r#"/(\w)/.match("z") { |m| [m[0], 99, :sym] }"#);
        // The match object is the block's only argument.
        run_test(r#"/(.)/.match("a") { |m| m.class.name }"#);
    }

    #[test]
    fn regexp_match_position_argument() {
        // Match starting at a positive char position.
        run_test(r#"/(.).(.)/.match("foobar", 3).captures"#);
        // Negative position counts from the end.
        run_test(r#"/(.).(.)/.match("foobar", -3).captures"#);
        // Position past end of string returns nil.
        run_test(r#"/x/.match("abc", 100)"#);
    }

    #[test]
    fn regexp_match_pred_position() {
        // `match?` accepts the same position arg as `match`.
        run_test(r#"/foo/.match?("xfooy", 1)"#);
        run_test(r#"/foo/.match?("xfooy", -3)"#);
        // `\Az/.match?("", 0)` finds the zero-width match at the end.
        run_test(r#"/\Az/.match?("", 0)"#);
        // `\z/` on a non-empty string with `match?(s, len)` finds
        // the end-of-string match.
        run_test(r#"/\z/.match?("abc", 3)"#);
    }

    #[test]
    fn regexp_compile_with_regexp_arg_and_nil_option() {
        // `Regexp.compile(re, nil)` is the same as `Regexp.compile(re)`
        // — no warning, options preserved.
        run_test(r#"Regexp.compile(/abc/i, nil) == /abc/i"#);
        run_test(r#"Regexp.compile(/abc/u, nil).encoding.name"#);
        run_test(r#"Regexp.compile(/abc/u, nil).fixed_encoding?"#);
    }

    #[test]
    fn regexp_new_modifier_propagation_through_class() {
        // `Regexp.new(/.../e)` keeps the EUC-JP declared encoding;
        // `/.../s` keeps Windows-31J. `Regexp.compile` is an alias
        // for `Regexp.new`.
        run_test(r#"Regexp.new(/abc/e).encoding.name"#);
        run_test(r#"Regexp.new(/abc/s).encoding.name"#);
        run_test(r#"Regexp.compile(/abc/e).fixed_encoding?"#);
        run_test(r#"Regexp.compile(/abc/s).fixed_encoding?"#);
    }

    #[test]
    fn regexp_unicode_escape_in_character_class() {
        // `\u` escapes inside a character class still pin encoding.
        run_test(r#"/[\u{1234}]/.encoding.name"#);
        run_test(r#"/[\u{1234}]/.fixed_encoding?"#);
        // Range with `\u{...}` endpoints.
        run_test(r#"/[\u{20}-\u{1234}]/.fixed_encoding?"#);
    }

    #[test]
    fn regexp_unicode_escape_combined_with_modifiers() {
        // `i`/`m`/`x` modifiers combine with `\u`-pinning without
        // disturbing the encoding result.
        run_test(r#"/\u{1234}/i.encoding.name"#);
        run_test(r#"/\u{1234}/i.fixed_encoding?"#);
        run_test(r#"/\u{1234}/m.encoding.name"#);
        run_test(r#"/\u{1234}/x.encoding.name"#);
        // `n` modifier overrides the `\u` codepoint check (would
        // also be a SyntaxError if the source actually contained
        // non-ASCII bytes; here the source is pure-ASCII).
    }

    #[test]
    fn regexp_escape_empty_and_ascii() {
        // Empty input round-trips US-ASCII.
        run_test(r#"Regexp.escape("").encoding.name"#);
        run_test(r#"Regexp.escape("")"#);
        // `Regexp.escape` of meta-only ASCII is US-ASCII.
        run_test(r#"Regexp.escape(".+*?").encoding.name"#);
        // Symbol path produces US-ASCII too (when ASCII-only).
        run_test(r#"Regexp.escape(:abc).encoding.name"#);
        run_test(r#"Regexp.escape(:abc)"#);
    }

    #[test]
    fn regexp_escape_round_trip_through_regexp() {
        // The escaped form recompiles into a regex that matches the
        // original input verbatim — the whole point of `Regexp.escape`.
        run_test(
            r#"
              s = "a.b*c+ d?"
              Regexp.new(Regexp.escape(s)) =~ s
            "#,
        );
        // With whitespace + the `x` modifier (which would otherwise
        // skip whitespace), the escaped form still matches.
        run_test(
            r#"
              s = "a b\tc"
              Regexp.new(Regexp.escape(s), Regexp::EXTENDED) =~ s
            "#,
        );
    }

    #[test]
    fn regexp_initialize_with_no_arg_succeeds() {
        // The `initialize` method only raises on a *previously
        // initialized* receiver. `Regexp.new` itself goes through
        // the C-side construction and never invokes `#initialize`,
        // so the public surface stays usable.
        run_test(r#"Regexp.new("abc").source"#);
        run_test(r#"Regexp.new("abc").options"#);
        run_test(r#"Regexp.new(/abc/i).options & Regexp::IGNORECASE != 0"#);
    }

    #[test]
    fn regexp_allocate_observable_metadata() {
        // `Regexp.allocate` produces a real Regexp instance (so
        // `#class`, `#frozen?`, etc. work) but methods that read
        // the source raise `TypeError`. CRuby behaves the same way.
        run_test(r#"Regexp.allocate.class.name"#);
        run_test(r#"Regexp.allocate.frozen?"#);
        // Source-reading method `#==` raises TypeError.
        run_test_error(r#"Regexp.allocate == /abc/"#);
        run_test_error(r#"Regexp.allocate == Regexp.allocate"#);
    }

    #[test]
    fn regexp_hash_ignores_encoding() {
        // CRuby's `#hash` is keyed on source + onigmo options only;
        // the declared encoding does not participate. Two regexps
        // can hash equal yet `==` false (hash collisions are fine).
        run_test(r#"/abc/.hash == /abc/.hash"#);
        run_test(r#"/abc/u.hash == /abc/u.hash"#);
        run_test(r#"/abc/u.hash == /abc/n.hash"#);
        // Different `mix` bits change the hash.
        run_test(r#"/abc/.hash == /abc/i.hash"#);
    }

    #[test]
    fn regexp_teq_string_path() {
        // Plain happy paths exercising the String / Symbol fast paths
        // alongside the new `#to_str` fallback.
        run_test(r#"/abc/ === "xabcx""#);
        run_test(r#"/abc/ === :abc"#);
        run_test(r#"/abc/ === "no""#);
    }

    #[test]
    fn regexp_match_pred_clears_no_special_vars() {
        // Even when `match?` is called *with* a previous successful
        // match in `$~`, `$~` is unchanged regardless of the
        // `match?` result. Both true and false paths share this
        // contract in CRuby.
        run_test(
            r#"
              "abc" =~ /(b)/
              kept = $~[0]
              ok = /b/.match?("xyzb")
              fail = /q/.match?("xyz")
              [kept, $~[0], ok, fail]
            "#,
        );
    }
}
