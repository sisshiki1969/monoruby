use super::*;

//
// MatchData class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("MatchData", MATCHDATA_CLASS, ObjTy::MATCHDATA);
    globals.store[MATCHDATA_CLASS].clear_alloc_func();
    // CRuby explicitly undefines `MatchData.allocate` (via
    // `rb_undef_method` on the singleton class) so calling it raises
    // `NoMethodError` rather than the default `TypeError: allocator
    // undefined`. Mirror that by defining a class method that raises
    // NoMethodError directly.
    globals.define_builtin_class_func(MATCHDATA_CLASS, "allocate", allocate_undefined, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "captures", captures, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "to_a", to_a, 0);
    globals.define_builtin_func_with(MATCHDATA_CLASS, "[]", index, 1, 2, false);
    globals.define_builtin_func(MATCHDATA_CLASS, "begin", match_begin, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "end", match_end, 1);
    globals.define_builtin_func_with(MATCHDATA_CLASS, "named_captures", named_captures, 0, 1, false);
    globals.define_builtin_funcs(MATCHDATA_CLASS, "size", &["length"], size, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "regexp", regexp_, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "string", string_, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "pre_match", pre_match, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "post_match", post_match, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "offset", offset, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "names", names, 0);
    globals.define_builtin_func_rest(MATCHDATA_CLASS, "values_at", values_at);
    globals.define_builtin_func(MATCHDATA_CLASS, "deconstruct", deconstruct, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "match", match_, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "match_length", match_length, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "bytebegin", bytebegin, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "byteend", byteend, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "byteoffset", byteoffset, 1);
    globals.define_builtin_func_with(
        MATCHDATA_CLASS,
        "deconstruct_keys",
        deconstruct_keys_md,
        1,
        1,
        false,
    );
    globals.define_builtin_funcs(MATCHDATA_CLASS, "==", &["eql?"], eq, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "hash", hash, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "to_s", to_s, 0);
}

///
/// ### MatchData#to_s
///
/// - to_s -> String
///
/// The whole match, in the subject's encoding (`rb_reg_last_match`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/to_s.html]
#[monoruby_builtin]
fn to_s(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp
        .self_val()
        .as_match_data()
        .at_value(0)
        .unwrap_or_else(|| Value::string_from_str("")))
}

///
/// ### MatchData#inspect
///
/// - inspect -> String
///
/// CRuby's `match_inspect` (re.c). The rendering is built in a BINARY
/// buffer: a group's name is copied into it byte for byte
/// (`rb_str_buf_cat`), in whatever encoding the pattern is, and only the
/// captures go through `rb_str_inspect` and `rb_str_buf_append`'s
/// encoding negotiation. So `/(?<é>x)/.match("x").inspect` is a BINARY
/// string holding the name's two bytes, the rendering takes a
/// capture's encoding only when the capture shows non-ASCII, and a
/// non-ASCII capture after a non-ASCII name is
/// `Encoding::CompatibilityError`, as it is there (#1907).
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/inspect.html]
#[monoruby_builtin]
fn inspect(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    // A substring-search match nobody has asked the Regexp of has none
    // in CRuby, and `match_inspect` shows it another way: the matched
    // text as it is, through `rb_sprintf`.
    if m.is_substring_match() {
        let mut buf = crate::builtins::encoding::InspectBuf::new("#<MatchData: ");
        if let Some(matched) = m.at_value(0) {
            let matched = matched.as_rstring_inner();
            if matched.encoding().is_ascii_compatible() {
                buf.append(&globals.store, matched)?;
            } else {
                // `rb_sprintf` reads a UTF-16 / UTF-32 string as the C
                // string its bytes make.
                if matched.as_bytes().contains(&0) {
                    return Err(MonorubyErr::argumenterr("string contains null byte"));
                }
                buf.cat_bytes(matched.as_bytes());
            }
        }
        buf.cat(">");
        return Ok(buf.into_value());
    }
    let re = m.regexp();
    let names: &[Vec<u8>] = match &re {
        Some(re) => re.capture_names(),
        None => &[],
    };
    let mut buf = crate::builtins::encoding::InspectBuf::new("#<MatchData");
    for i in 0..m.len() {
        buf.cat(" ");
        if i > 0 {
            // An unnamed group has an empty entry, and is shown by number.
            match names.get(i - 1).filter(|name| !name.is_empty()) {
                Some(name) => buf.cat_bytes(name),
                None => buf.cat(&i.to_string()),
            }
            buf.cat(":");
        }
        match m.at_value(i) {
            Some(capture) => {
                let piece =
                    crate::builtins::encoding::str_inspect(globals, capture.as_rstring_inner());
                buf.append(&globals.store, piece.as_rstring_inner())?;
            }
            None => buf.cat("nil"),
        }
    }
    buf.cat(">");
    Ok(buf.into_value())
}

///
/// ### MatchData#== / #eql?
///
/// Two MatchData are equal iff their target strings, patterns, and all
/// match positions are equal.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let other = lfp.arg(0);
    if !matches!(other.unpack(), RV::Object(rv) if rv.ty() == ObjTy::MATCHDATA) {
        return Ok(Value::bool(false));
    }
    if self_.as_match_data().string_value().as_rstring_inner()
        != other.as_match_data().string_value().as_rstring_inner()
    {
        return Ok(Value::bool(false));
    }
    // CRuby compares `match_regexp` of both sides, which builds and
    // keeps the Regexp of a substring-search match.
    let re_eq = match (
        match_regexp(vm, globals, self_)?,
        match_regexp(vm, globals, other)?,
    ) {
        (Some(x), Some(y)) => super::regexp::reg_equal(&x, &y),
        (None, None) => true,
        _ => false,
    };
    let a = self_.as_match_data();
    let b = other.as_match_data();
    let pos_eq = a.len() == b.len() && (0..a.len()).all(|i| a.pos(i) == b.pos(i));
    Ok(Value::bool(re_eq && pos_eq))
}

///
/// ### MatchData#hash
///
/// CRuby's `match_hash`: the target string, the pattern (`match_regexp`,
/// so a substring-search match gets its Regexp here) and every match
/// position — what `==` compares.
#[monoruby_builtin]
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let self_ = lfp.self_val();
    let re = match_regexp(vm, globals, self_)?;
    let m = self_.as_match_data();
    let mut h = crate::value::seeded_hasher();
    m.string_value().as_rstring_inner().hash(&mut h);
    if let Some(re) = re {
        super::regexp::reg_hash(&re, &mut h);
    }
    m.len().hash(&mut h);
    for i in 0..m.len() {
        m.pos(i).hash(&mut h);
    }
    Ok(Value::from_hash_digest(h.finish()))
}

/// Stand-in for the undefined `MatchData.allocate`. Raises NoMethodError
/// to match CRuby (where the method is `rb_undef_method`-removed).
#[monoruby_builtin]
fn allocate_undefined(
    _: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Err(MonorubyErr::method_not_found_for_class(
        &globals.store,
        IdentId::ALLOCATE,
        MATCHDATA_CLASS,
    ))
}

/// A capture-name argument (Symbol or String) as the string the regexp's
/// group names are compared against. A String is read as it is: it used to
/// be interned into a symbol first, which for a name the regexp does not
/// have meant one more symbol for every wrong name ever asked about.
fn group_name_of(arg: Value) -> Option<Vec<u8>> {
    if let Some(sym) = arg.try_symbol() {
        return Some(sym.with_ident_name(|name, _| name.as_bytes().to_vec()));
    }
    let inner = arg.is_rstring_inner()?;
    Some(inner.as_bytes().to_vec())
}

/// `IndexError` for a group name the pattern does not have.
fn undefined_group_name(name: &[u8]) -> MonorubyErr {
    MonorubyErr::indexerr(format!(
        "undefined group name reference: {}",
        String::from_utf8_lossy(name)
    ))
}

/// Resolve a `begin`/`end`/`offset`/`bytebegin`/`byteend`/`byteoffset`
/// argument to a non-negative capture index. Accepts Integer / String /
/// Symbol; for the latter two, looks up the named capture and returns
/// the *last* group index sharing that name (matching CRuby's "farthest
/// match" semantics for duplicated capture names). Out-of-range
/// integers and unknown names raise IndexError.
fn resolve_capture_index(
    vm: &mut Executor,
    globals: &mut Globals,
    m: &crate::value::rvalue::MatchDataInner,
    arg: Value,
) -> Result<usize> {
    if let Some(group_name) = group_name_of(arg) {
        if let Some(i) = m
            .regexp()
            .and_then(|r| r.get_group_members(&group_name).last().copied())
        {
            return Ok(i as usize);
        }
        return Err(undefined_group_name(&group_name));
    }
    let idx = arg.coerce_to_int_i64(vm, globals)?;
    let len = m.len() as i64;
    // `begin`/`end`/`offset`/`byte*` reject negative indexes outright
    // (no wrap-around), matching CRuby's `match_array_subseq` rules.
    if idx < 0 || idx >= len {
        return Err(MonorubyErr::indexerr(format!(
            "index {idx} out of matches"
        )));
    }
    Ok(idx as usize)
}

///
/// ### MatchData#deconstruct_keys
///
/// - deconstruct_keys(keys) -> Hash
///
/// Returns a hash of named captures for pattern matching. With `nil`,
/// returns every named capture. With an Array of Symbols, CRuby's
/// all-or-nothing rule applies: if any requested key isn't a named
/// capture, returns `{}`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/deconstruct_keys.html]
#[monoruby_builtin]
fn deconstruct_keys_md(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let Some(re) = m.regexp() else {
        return Ok(Value::hash(RubyMap::default()));
    };
    let names = re.unique_capture_names();
    let val_for = |name: &[u8]| -> Value {
        re.get_group_members(name)
            .last()
            .and_then(|&i| m.at_value(i as usize))
            .unwrap_or_default()
    };
    let mut map = RubyMap::default();
    if arg.is_nil() {
        for &name in &names {
            let key = re.name_symbol(name);
            map.insert(key, val_for(name), vm, globals)?;
        }
    } else if arg.is_array_ty() {
        let arr = arg.as_array();
        // CRuby: more requested keys than named captures -> `{}`.
        if arr.len() > names.len() {
            return Ok(Value::hash(RubyMap::default()));
        }
        for key in arr.iter() {
            let Some(sym) = key.try_symbol() else {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong argument type {} (expected Symbol)",
                    key.builtin_class_name(globals)
                )));
            };
            let name = sym.with_ident_name(|name, _| name.as_bytes().to_vec());
            // Stop at the first key with no corresponding named
            // capture, returning what was collected so far.
            if !names.contains(&name.as_slice()) {
                break;
            }
            map.insert(*key, val_for(&name), vm, globals)?;
        }
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Array)",
            arg.builtin_class_name(globals)
        )));
    }
    Ok(Value::hash(map))
}

///
/// ### MatchData#bytebegin
///
/// - bytebegin(n) -> Integer | nil
///
/// Byte-based offset of the start of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/bytebegin.html]
#[monoruby_builtin]
fn bytebegin(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((start, _)) => Ok(Value::integer(start as i64)),
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#byteend
///
/// - byteend(n) -> Integer | nil
///
/// Byte-based offset just past the end of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/byteend.html]
#[monoruby_builtin]
fn byteend(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((_, end)) => Ok(Value::integer(end as i64)),
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#byteoffset
///
/// - byteoffset(n) -> [Integer, Integer] | [nil, nil]
///
/// Byte-based `[begin, end]` offsets of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/byteoffset.html]
#[monoruby_builtin]
fn byteoffset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((s, e)) => Ok(Value::array_from_vec(vec![
            Value::integer(s as i64),
            Value::integer(e as i64),
        ])),
        None => Ok(Value::array_from_vec(vec![Value::nil(), Value::nil()])),
    }
}

///
/// ### MatchData#size
///
/// - size -> Integer
/// - length -> Integer
///
/// Number of captures including the whole match (index 0).
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/length.html]
#[monoruby_builtin]
fn size(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_match_data().len() as i64))
}

///
/// ### MatchData#regexp
///
/// - regexp -> Regexp | nil
///
/// Returns the Regexp that produced this MatchData.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/regexp.html]
#[monoruby_builtin]
fn regexp_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match match_regexp(vm, globals, lfp.self_val())? {
        Some(r) => Ok(r.as_val()),
        None => Ok(Value::nil()),
    }
}

/// CRuby's `match_regexp`: the Regexp of `md`. A substring-search match
/// has none until this is called; it is then built from the matched
/// text (`rb_reg_regcomp(rb_reg_quote(match[0]))`, in the text's own
/// encoding) and kept, so every later call answers the same object.
fn match_regexp(vm: &mut Executor, globals: &mut Globals, mut md: Value) -> Result<Option<Regexp>> {
    let m = md.as_match_data();
    if !m.is_substring_match() {
        return Ok(m.regexp());
    }
    let Some(matched) = m.at(0) else {
        return Ok(None);
    };
    let enc = m.string_value().as_rstring_inner().encoding();
    let escaped = RegexpInner::escape_in(matched, enc);
    // `rb_reg_quote` tags a 7-bit result US-ASCII.
    let enc = if enc.is_ascii_compatible() && escaped.is_ascii() {
        crate::value::Encoding::UsAscii
    } else {
        enc
    };
    let source = Value::string_from_inner(RStringInner::from_encoding(&escaped, enc));
    let inner = crate::builtins::regexp::regexp_inner_from_string(vm, globals, source, 0, None)?;
    let re = Value::regexp(inner).is_regex().unwrap();
    md.as_match_data_mut().set_regexp(re);
    Ok(Some(re))
}

///
/// ### MatchData#string
///
/// - string -> String
///
/// Returns the source string against which the match was performed.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/string.html]
#[monoruby_builtin]
fn string_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // CRuby returns a single frozen copy of the subject string, the same
    // object on every call. Our MatchData already holds an immutable
    // snapshot of the haystack, so freeze it in place and return it.
    let mut s = lfp.self_val().as_match_data().string_value();
    s.set_frozen();
    Ok(s)
}

///
/// ### MatchData#pre_match
///
/// - pre_match -> String
///
/// Returns the portion of the source string before the match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/pre_match.html]
#[monoruby_builtin]
fn pre_match(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // The result carries the source string's encoding (CRuby semantics);
    // `pre_match_value` returns a CoW substring of the haystack snapshot.
    Ok(lfp.self_val().as_match_data().pre_match_value())
}

///
/// ### MatchData#post_match
///
/// - post_match -> String
///
/// Returns the portion of the source string after the match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/post_match.html]
#[monoruby_builtin]
fn post_match(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // The result carries the source string's encoding (CRuby semantics).
    Ok(lfp.self_val().as_match_data().post_match_value())
}

///
/// ### MatchData#offset
///
/// - offset(n) -> [Integer, Integer] | [nil, nil]
///
/// Character-based `[begin, end]` offsets of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/offset.html]
#[monoruby_builtin]
fn offset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((start, end)) => {
            let s = m.char_count_upto(start) as i64;
            let e = m.char_count_upto(end) as i64;
            Ok(Value::array_from_vec(vec![Value::integer(s), Value::integer(e)]))
        }
        None => Ok(Value::array_from_vec(vec![Value::nil(), Value::nil()])),
    }
}

///
/// ### MatchData#names
///
/// - names -> [String]
///
/// Returns the names of named captures in the original Regexp.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/names.html]
#[monoruby_builtin]
fn names(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    // A name shared by several groups appears only once (== Regexp#names).
    Ok(match m.regexp() {
        Some(re) => Value::array_from_iter(
            re.unique_capture_names()
                .into_iter()
                .map(|n| re.name_string(n)),
        ),
        None => Value::array_empty(),
    })
}

/// The value of named capture `name`: the *last* group sharing that
/// name whose capture actually participated in the match (CRuby
/// "latest matched capture"); `None` if the name never matched.
fn last_matched_named(
    m: &crate::value::rvalue::MatchDataInner,
    capture_names: &[Vec<u8>],
    name: &[u8],
) -> Option<usize> {
    let mut result = None;
    for (i, n) in capture_names.iter().enumerate() {
        if n.as_slice() == name && m.at(i + 1).is_some() {
            result = Some(i + 1);
        }
    }
    result
}

///
/// ### MatchData#values_at
///
/// - values_at(*indices) -> [String | nil]
///
/// Returns captures at the given integer indices (nil for out-of-range).
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/values_at.html]
#[monoruby_builtin]
fn values_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let args = lfp.arg(0).as_array();
    let mut res = vec![];
    for a in args.iter() {
        // Range argument: append the slice (out-of-range range
        // raises RangeError).
        if let Some(range) = a.is_range() {
            let len = m.len() as i64;
            let (start, slice_len) = match range_to_slice(range, len) {
                Some(v) => v,
                None => {
                    let sep = if range.exclude_end() { "..." } else { ".." };
                    return Err(MonorubyErr::rangeerr(format!(
                        "{}{sep}{} out of range",
                        range.start().to_s(&globals.store),
                        range.end().to_s(&globals.store),
                    )));
                }
            };
            let s = start as usize;
            let e = (s + slice_len as usize).min(m.len());
            for i in s..e {
                res.push(
                    m.at_value(i)
                        .unwrap_or_default(),
                );
            }
            // Pad with nil for positions past the end.
            let requested = (start + slice_len) as usize;
            for _ in e..requested {
                res.push(Value::nil());
            }
            continue;
        }
        // Symbol/String: named-capture lookup.
        if let Some(group_name) = group_name_of(*a) {
            let i = m
                .regexp()
                .and_then(|r| r.get_group_members(&group_name).last().copied())
                .map(|i| i as usize)
                .ok_or_else(|| undefined_group_name(&group_name))?;
            res.push(
                m.at_value(i)
                    .unwrap_or_else(Value::nil),
            );
            continue;
        }
        let idx = a.coerce_to_int_i64(vm, globals)?;
        let idx = if idx >= 0 {
            idx as usize
        } else {
            let v = idx + m.len() as i64;
            if v < 0 {
                res.push(Value::nil());
                continue;
            }
            v as usize
        };
        if idx >= m.len() {
            res.push(Value::nil());
        } else {
            res.push(
                m.at_value(idx)
                    .unwrap_or_default(),
            );
        }
    }
    Ok(Value::array_from_vec(res))
}

///
/// ### MatchData#deconstruct
///
/// - deconstruct -> [String | nil]
///
/// Returns the captures (without the whole-match at index 0) for array
/// pattern matching.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/deconstruct.html]
#[monoruby_builtin]
fn deconstruct(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::array_from_iter(
        lfp.self_val()
            .as_match_data()
            .captures_values()
            .skip(1)
            .map(|s| s.unwrap_or_default()),
    ))
}

///
/// ### MatchData#match
///
/// - match(n) -> String | nil
/// - match(name) -> String | nil
///
/// Returns the matched string for the given index or named capture.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/match.html]
#[monoruby_builtin]
fn match_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let arg = lfp.arg(0);
    if let Some(idx) = arg.try_fixnum() {
        let idx = if idx >= 0 {
            idx as usize
        } else {
            let v = idx + m.len() as i64;
            if v < 0 { return Ok(Value::nil()); }
            v as usize
        };
        if idx >= m.len() {
            return Ok(Value::nil());
        }
        Ok(m.at_value(idx)
            .unwrap_or_default())
    } else if let Some(sym) = group_name_of(arg) {
        if let Some(i) = m
            .regexp()
            .map(|r| r.get_group_members(&sym))
            .and_then(|g| g.last().copied())
        {
            Ok(m.at_value(i as usize)
                .unwrap_or_default())
        } else {
            Err(undefined_group_name(&sym))
        }
    } else {
        Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Integer",
            arg.builtin_class_name(globals)
        )))
    }
}

///
/// ### MatchData#match_length
///
/// - match_length(n) -> Integer | nil
/// - match_length(name) -> Integer | nil
///
/// Character length of the nth / named match, or nil if the group
/// didn't match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/match_length.html]
#[monoruby_builtin]
fn match_length(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let arg = lfp.arg(0);
    let idx = if let Some(idx) = arg.try_fixnum() {
        if idx >= 0 {
            idx as usize
        } else {
            let v = idx + m.len() as i64;
            if v < 0 { return Ok(Value::nil()); }
            v as usize
        }
    } else if let Some(sym) = group_name_of(arg) {
        if let Some(i) = m
            .regexp()
            .map(|r| r.get_group_members(&sym))
            .and_then(|g| g.last().copied())
        {
            i as usize
        } else {
            return Err(undefined_group_name(&sym));
        }
    } else {
        let _ = (vm, globals);
        return Err(MonorubyErr::typeerr("no implicit conversion into Integer"));
    };
    if idx >= m.len() {
        return Ok(Value::nil());
    }
    match m.at(idx) {
        Some(s) => Ok(Value::integer(
            String::from_utf8_lossy(s).chars().count() as i64,
        )),
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#captures
///
/// - captures -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/captures.html]
#[monoruby_builtin]
fn captures(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::array_from_iter(
        lfp.self_val()
            .as_match_data()
            .captures_values()
            .skip(1)
            .map(|s| s.unwrap_or_default()),
    ))
}

/// ### MatchData#to_a
///
/// - to_a -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/to_a.html]
#[monoruby_builtin]
fn to_a(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::array_from_iter(
        lfp.self_val()
            .as_match_data()
            .captures_values()
            .map(|s| s.unwrap_or_default()),
    ))
}

///
/// ### MatchData#[]
///
/// - self[n] -> String | nil
/// - self[name] -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/=5b=5d.html]
#[monoruby_builtin]
fn index(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    // `[start, length]` two-argument form returns a slice as Array.
    if let Some(arg1) = lfp.try_arg(1) {
        let start = lfp.arg(0).try_fixnum().ok_or_else(|| {
            MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into Integer",
                lfp.arg(0).builtin_class_name(globals)
            ))
        })?;
        let len = arg1.try_fixnum().ok_or_else(|| {
            MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into Integer",
                arg1.builtin_class_name(globals)
            ))
        })?;
        return Ok(slice_match_data(&m, start, len));
    }
    if let Some(idx) = lfp.arg(0).try_fixnum() {
        let idx = if idx >= 0 {
            idx as usize
        } else {
            let idx = idx + m.len() as i64;
            if idx < 0 {
                return Ok(Value::nil());
            }
            idx as usize
        };
        if idx >= m.len() {
            return Ok(Value::nil());
        }
        Ok(m.at_value(idx as usize)
            .unwrap_or_default())
    } else if let Some(range) = lfp.arg(0).is_range() {
        // `[start..end]` / `[start...]` / `[..end]` slicing.
        let len = m.len() as i64;
        let (start, slice_len) = match range_to_slice(range, len) {
            Some(v) => v,
            None => return Ok(Value::nil()),
        };
        Ok(slice_match_data(&m, start, slice_len))
    } else if let Some(sym) = lfp.arg(0).try_symbol()
        && let Some(r) = m.regexp()
        && let Some(members) =
            Some(sym.with_ident_name(|name, _| r.get_group_members(name.as_bytes())))
        && !members.is_empty()
    {
        // `m[:name]`, the common form: look the name up in place rather
        // than copying it out of the identifier table first.
        Ok(last_matched_member(&m, members))
    } else if let Some(sym) = group_name_of(lfp.arg(0)) {
        let members = m
            .regexp()
            .map(|r| r.get_group_members(&sym))
            .unwrap_or_default();
        if members.is_empty() {
            return Err(undefined_group_name(&sym));
        }
        Ok(last_matched_member(&m, members))
    } else {
        Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Integer",
            lfp.arg(0).builtin_class_name(globals)
        )))
    }
}

/// Among groups sharing a name, the last one that actually matched (CRuby
/// semantics); nil if none matched.
fn last_matched_member(m: &crate::value::rvalue::MatchDataInner, members: Vec<i32>) -> Value {
    let mut result = Value::nil();
    for i in members {
        if let Some(s) = m.at_value(i as usize) {
            result = s;
        }
    }
    result
}

/// Slice `[start, length]` semantics over a MatchData. `start` may be
/// negative (counts from end). Returns `nil` for out-of-range start.
/// Out-of-range tail is silently truncated.
fn slice_match_data(
    m: &crate::value::rvalue::MatchDataInner,
    start: i64,
    length: i64,
) -> Value {
    let len = m.len() as i64;
    let s = if start < 0 { start + len } else { start };
    if s < 0 || s > len || length < 0 {
        return Value::nil();
    }
    let s = s as usize;
    let e = (s + length as usize).min(m.len());
    let mut out = Vec::with_capacity(e - s);
    for i in s..e {
        out.push(
            m.at_value(i)
                .unwrap_or_default(),
        );
    }
    Value::array_from_vec(out)
}

/// Convert a `Range` of integers (possibly with nil endpoints for
/// beginningless / endless) into a `(start, length)` pair suitable
/// for `slice_match_data`. Returns `None` when the start exceeds the
/// match-array length (CRuby returns `nil` in that case).
fn range_to_slice(
    range: &crate::value::rvalue::RangeInner,
    len: i64,
) -> Option<(i64, i64)> {
    let start_v = range.start();
    let end_v = range.end();
    let start = if start_v.is_nil() {
        0
    } else {
        let v = start_v.try_fixnum()?;
        if v < 0 { v + len } else { v }
    };
    if start < 0 || start > len {
        return None;
    }
    let end = if end_v.is_nil() {
        len
    } else {
        let v = end_v.try_fixnum()?;
        let v = if v < 0 { v + len } else { v };
        if range.exclude_end() { v } else { v + 1 }
    };
    let length = (end.max(start) - start).max(0);
    Some((start, length))
}

///
/// ### MatchData#begin
///
/// - begin(n) -> Integer | nil
///
/// Returns the offset of the start of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/begin.html]
#[monoruby_builtin]
fn match_begin(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((start, _)) => {
            let char_offset = m.char_count_upto(start);
            Ok(Value::integer(char_offset as i64))
        }
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#end
///
/// - end(n) -> Integer | nil
///
/// Returns the offset of the character just past the end of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/end.html]
#[monoruby_builtin]
fn match_end(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = resolve_capture_index(vm, globals, &m, lfp.arg(0))?;
    match m.pos(idx) {
        Some((_, end_pos)) => {
            let char_offset = m.char_count_upto(end_pos);
            Ok(Value::integer(char_offset as i64))
        }
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#named_captures
///
/// - named_captures -> Hash
///
/// Returns a Hash of named captures.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/named_captures.html]
#[monoruby_builtin]
fn named_captures(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let Some(re) = m.regexp() else {
        return Ok(Value::hash(RubyMap::default()));
    };
    let names = re.capture_names();
    // `named_captures(symbolize_names: true)` -> Symbol keys.
    let symbolize = lfp
        .try_arg(0)
        .and_then(|h| h.try_hash_ty())
        .and_then(|h| h.get(Value::symbol_from_str("symbolize_names"), vm, globals).ok().flatten())
        .map(|v| v.as_bool())
        .unwrap_or(false);
    let mut map = RubyMap::default();
    for name in re.unique_capture_names() {
        let key = if symbolize {
            re.name_symbol(name)
        } else {
            re.name_string(name)
        };
        let val = last_matched_named(&m, names, name)
            .and_then(|i| m.at_value(i))
            .unwrap_or_default();
        map.insert(key, val, vm, globals)?;
    }
    Ok(Value::hash(map))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // The haystack is snapshotted at match time (zero-copy shared
    // substring + CoW): mutating the subject afterwards must not
    // change the MatchData or `$~`.
    #[test]
    fn match_data_haystack_snapshot() {
        run_tests(&[
            r##"
        s = "hello world foo bar baz " * 10
        m = s.match(/world (\w+)/)
        before = [m[0], m[1], m.pre_match.bytesize, m.string.dup]
        s << "tail"
        s.setbyte(6, 33)
        after = [m[0], m[1], m.pre_match.bytesize, m.string]
        [before == after, m.string.include?("world"), s.getbyte(6)]
        "##,
            r##"
        big = "x" * 100 + "needle" + "y" * 100
        rest = big.byteslice(50..)
        mt = rest.match(/needle/)
        big.setbyte(150, 33)
        [mt[0], mt.pre_match.bytesize, mt.post_match.bytesize]
        "##,
            // Subjects that die before the MatchData does (the snapshot
            // must keep the haystack alive through GC).
            r##"
        mds = 20.times.map { |i| ("p#{i} " + "x" * 64 + " needle#{i}").match(/needle\d+/) }
        GC.start
        mds.each_with_index.map { |m, i| m[0] == "needle#{i}" && m.string.start_with?("p#{i}") }.all?
        "##,
        ]);
    }

    /// The probes of the `match_data_inspect_*` tests: `#inspect` of
    /// each match under `Encoding.default_external = enc`, as bytes
    /// and encoding, or the error it raises.
    fn inspect_probe(enc: &str, matches: &str) -> String {
        format!(
            r##"
        $VERBOSE = nil
        sj = ->(s) {{ s.dup.force_encoding("Windows-31J") }}
        eu = ->(s) {{ s.dup.force_encoding("EUC-JP") }}
        l1 = ->(s) {{ s.dup.force_encoding("ISO-8859-1") }}
        ext = Encoding.default_external
        Encoding.default_external = "{enc}"
        res = [{matches}].map do |m|
          begin
            s = m.call.inspect
            [s.b, s.encoding.to_s]
          rescue => e
            [e.class.to_s, e.message]
          end
        end
        Encoding.default_external = ext
        res
        "##
        )
    }

    /// The matches every result encoding is probed with (#1907): group
    /// names are copied byte for byte, captures are `String#inspect`ed.
    const INSPECT_MATCHES: &str = r##"
          -> { /(?<a>x)/.match("x") },
          -> { /x/.match("x") },
          -> { /é/.match("é") },
          -> { /(x)(y)?(z)?/.match("x") },
          -> { /(é)(y)?/.match("é") },
          -> { /(?<é>x)/.match("x") },
          -> { /(?<é>é)/.match("é") },
          -> { /(?<é>é)?x/.match("x") },
          -> { /(?<é>x)(?<ü>y)/.match("xy") },
          -> { /(?<a>é)/.match("é") },
          -> { /(?<é>x)é/.match("xé") },
          # A capture outside the whole match: the name has made the
          # BINARY buffer non-ASCII by the time a non-ASCII capture comes.
          -> { /(?=(?<é>é))/.match("é") },
          -> { /(?=(?<a>é))/.match("é") },
          -> { /(?=(?<é>x)(?<ü>é))/.match("xé") },
          -> { /(?<é>foo)(?<b>bar)(?<é>BAZ)?/.match("foobar") },
          -> { Regexp.new(sj["(?<n\x87]>x)"]).match("x") },
          -> { Regexp.new(sj["(?<n\x87]>x)\x87]"]).match(sj["x\x87]"]) },
          -> { Regexp.new(sj["(?<n\x87]>\x87])"]).match(sj["\x87]"]) },
          -> { Regexp.new(sj["(?<n>\x87])"]).match(sj["\x87]"]) },
          -> { Regexp.new(eu["(?<\xA4\xA2>x)"]).match("x") },
          -> { Regexp.new(eu["(?<\xA4\xA2>\xA4\xA2)"]).match(eu["\xA4\xA2"]) },
          -> { Regexp.new(eu["(?<n>\xA4\xA2)x"]).match(eu["\xA4\xA2x"]) },
          -> { Regexp.new(l1["(?<n\xE9>\xE9)"]).match(l1["\xE9"]) },
          -> { Regexp.new(l1["(?<n\xE9>x)"]).match("x") },
          -> { /(?<a>.)/n.match("\xff".b) },
          -> { /(?<a>.)(.)?/m.match("\n") },
          -> { /(?<a>.)/.match("\"") },
          -> { "x" =~ /(?<é>x)/; $~ },
          -> { r = nil; "xéy".gsub(/(?<é>é)/) { r = $~; "" }; r },
    "##;

    #[test]
    fn match_data_inspect_utf8() {
        run_test(&inspect_probe("UTF-8", INSPECT_MATCHES));
    }

    #[test]
    fn match_data_inspect_us_ascii() {
        run_test(&inspect_probe("US-ASCII", INSPECT_MATCHES));
    }

    #[test]
    fn match_data_inspect_binary() {
        run_test(&inspect_probe("ASCII-8BIT", INSPECT_MATCHES));
    }

    #[test]
    fn match_data_inspect_windows_31j() {
        run_test(&inspect_probe("Windows-31J", INSPECT_MATCHES));
    }

    #[test]
    fn match_data_inspect_euc_jp() {
        run_test(&inspect_probe("EUC-JP", INSPECT_MATCHES));
    }

    #[test]
    fn match_data_inspect_iso_8859_1() {
        run_test(&inspect_probe("ISO-8859-1", INSPECT_MATCHES));
    }

    /// `rb_inspect` escapes the BINARY rendering where it is embedded,
    /// and the captures are not `String#inspect` dispatched.
    #[test]
    fn match_data_inspect_embedded() {
        run_test(
            r##"
        $VERBOSE = nil
        ext = Encoding.default_external
        res = ["UTF-8", "US-ASCII", "ASCII-8BIT"].map do |enc|
          Encoding.default_external = enc
          m = /(?<é>x)/.match("x")
          [[m], ["é", m], {a: m}, "#{m.inspect}!"].map { |o|
            s = o.is_a?(String) ? o : o.inspect
            [s.b, s.encoding.to_s]
          }
        end
        Encoding.default_external = ext
        class String
          alias_method :__inspect_1907, :inspect
          def inspect = "REDEFINED"
        end
        r = /(?<a>x)/.match("x").inspect
        class String
          alias_method :inspect, :__inspect_1907
        end
        res << r
        "##,
        );
    }

    /// A match found by a substring search has no Regexp until one is
    /// asked for (`#regexp`, `#==`), and inspects in a form of its own.
    #[test]
    fn match_data_inspect_substring_match() {
        run_test(&inspect_probe(
            "UTF-8",
            r##"
          -> { "abc".sub("b", "x"); $~ },
          -> { "aéc".sub("é", "x"); $~ },
          -> { "abc".gsub("b", "x"); $~ },
          -> { "a.c".sub(".", "x"); $~ },
          -> { sj["a\x87]c"].sub(sj["\x87]"], "x"); $~ },
          -> { s = +"abc"; s.sub!("b", "x"); $~ },
          -> { "abc".sub("b", "x"); m = $~; m.regexp; m },
          -> { "aéc".sub("é", "x"); m = $~; m.regexp; m },
          -> { "abc".sub("b", "x"); m = $~; "abc".sub("b", "y"); m == $~; m },
          -> { "abc".sub("b", "x"); m = $~; "abd".sub("b", "y"); m == $~; m },
          -> { "abc".sub("b", "x"); m = $~; m.names; m.named_captures; m[0]; m.to_a; m },
            "##,
        ));
        run_test(
            r##"
        "abc".sub("b", "x")
        m = $~
        [m.regexp.equal?(m.regexp), m.regexp.source, m == m, m.inspect, m.to_s.encoding.to_s]
        "##,
        );
        // `rb_sprintf` takes a UTF-16 string for the C string its bytes
        // make.
        run_test(
            r##"
        u = ->(s) { s.encode("UTF-16LE") }
        [u["abc"], u["a\u3042\u3043c"]].zip([u["b"], u["\u3042\u3043"]]).map do |s, pat|
          s.sub(pat, u["x"])
          begin
            i = $~.inspect
            [i.b, i.encoding.to_s]
          rescue => e
            [e.class.to_s, e.message]
          end
        end
        "##,
        );
    }

    /// `MatchData#to_s` is in the subject's encoding.
    #[test]
    fn match_data_to_s_encoding() {
        run_test(
            r##"
        [
          /(?<a>x)/.match("x").to_s,
          /é/.match("é").to_s,
          /./n.match("\xff".b).to_s,
          /x/.match("x".encode("Windows-31J")).to_s,
          /x/.match("x".force_encoding("US-ASCII")).to_s,
        ].map { |s| [s.b, s.encoding.to_s] }
        "##,
        );
    }

    /// `Kernel#p` shows a rendering through `rb_inspect`: one in another
    /// encoding than the result encoding is escaped, a BINARY one byte by
    /// byte.
    #[test]
    fn match_data_inspect_p() {
        run_test(
            r##"
        $VERBOSE = nil
        ext = Encoding.default_external
        bin = Object.new
        def bin.inspect = "caf\xC3\xA9".b
        broken = Object.new
        def broken.inspect = "\xFF\u3042"
        sjis = Object.new
        def sjis.inspect = "\x87]".dup.force_encoding("Windows-31J")
        res = ["UTF-8", "US-ASCII", "ASCII-8BIT", "Windows-31J"].map do |enc|
          Encoding.default_external = enc
          r, w = IO.pipe
          $stdout = w
          begin
            p /(?<é>x)/.match("x"), /(?<a>é)/.match("é"), /é/.match("é")
            p bin, broken, sjis, "é", :é, nil
          ensure
            $stdout = STDOUT
          end
          w.close
          r.read.b
        end
        Encoding.default_external = ext
        res
        "##,
        );
    }

    #[test]
    fn match_data() {
        run_tests(&[
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").to_s"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").inspect"##,
            r##"/(?<a>foo)(?<b>bar)(?<c>BAZ)?/.match("foobarbaz").inspect"##,
            r##"/(?<a>foo)(?<b>bar)(BAZ)?/.match("foobarbaz").inspect"##,
            r##"/(?<a>foo)(?<b>bar)(?<a>BAZ)?/.match("foobarbaz").inspect"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").captures"##,
        ]);
    }

    #[test]
    fn match_data_binary_n_byte_safe() {
        // `/n` regex matches per *byte*. On a haystack whose bytes
        // are not valid UTF-8 char boundaries (e.g. a single high
        // byte under ASCII-8BIT), `MatchDataInner::at` previously
        // sliced `&str` and aborted (`panic_nounwind` across the
        // builtin's extern "C" boundary). The byte-slice path must
        // return the matched bytes intact.
        run_tests(&[
            r##"/./n.match("\xC3\xA9".dup.force_encoding(Encoding::ASCII_8BIT)).to_a"##,
            r##"/./n.match("\xC3\xA9".dup.force_encoding(Encoding::ASCII_8BIT))[0]"##,
            r##"/./n.match("\xC3\xA9".dup.force_encoding(Encoding::ASCII_8BIT)).pre_match"##,
            r##"/./n.match("\xC3\xA9".dup.force_encoding(Encoding::ASCII_8BIT)).post_match"##,
            // Sanity: ASCII haystack still works through the same path.
            r##"/(\w)(\w)/.match("abc").to_a"##,
            r##"/(\w)(\w)/.match("abc")[0..]"##,
            r##"/(\w)(\w)/.match("abc").pre_match + /(\w)/.match("abc").post_match"##,
        ]);
    }

    #[test]
    fn match_data_deconstruct_keys() {
        run_test(
            r##"
            m = /(?<f>foo)(?<b>bar)(?<c>baz)/.match("foobarbaz")
            [m.deconstruct_keys(nil), m.deconstruct_keys([:f]),
             m.deconstruct_keys([]), m.deconstruct_keys([:f, :a, :b]),
             m.deconstruct_keys([:f, :b, :x])]
            "##,
        );
        // No named captures -> empty hash.
        run_test(r##"/(foo)(bar)/.match("foobar").deconstruct_keys(nil)"##);
        run_test_error(r##"/(?<f>x)/.match("x").deconstruct_keys(1)"##);
        run_test_error(r##"/(?<f>x)/.match("x").deconstruct_keys("asd")"##);
        // Non-Symbol key with array length == named-capture count -> TypeError.
        run_test_error(r##"/(?<f>x)(?<g>y)/.match("xy").deconstruct_keys(['s', :g])"##);
        // Array longer than named captures -> {} (early return, no type check).
        run_test(r##"/(?<f>x)/.match("x").deconstruct_keys(['s', :f])"##);
    }

    #[test]
    fn match_data_dup_names_eq() {
        run_tests(&[
            r##"/(?<hay>hay)(?<dot>\.)(?<hay>hay)/.match("hay.hay").names"##,
            r##"/\A(?<a>.)(?<b>.)(?<b>.)(?<a>.)?\z/.match("012").named_captures"##,
            r##"/(?<a>.)(?<b>.)?/.match("0").named_captures(symbolize_names: true)"##,
            r##"/(?<a>.)(?<b>.)?/.match("02").named_captures(symbolize_names: false)"##,
            r##"/(?<f>foo)|(?<f>bar)/.match("bar")[:f]"##,
            r##"/(?<f>foo)|(?<f>bar)/.match("foo")[:f]"##,
            r##"(/(?<a>x)/.match("x")["nope"] rescue $!.class)"##,
            r##"/(a)(b)/.match("ab") == /(a)(b)/.match("ab")"##,
            r##"/(a)(b)/.match("ab").eql?(/(a)(b)/.match("ab"))"##,
            r##"/(a)(b)/.match("ab") == /(a)(b)/.match("xab")"##,
            r##"/(a)/.match("a") == "a""##,
        ]);
    }

    #[test]
    fn match_data_index() {
        run_tests(&[
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-100]"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-1]"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[0]"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[100]"##,
            r##"/(foo)(?<abc>bar)(BAZ)?/.match("foobarbaz")["abc"]"##,
            r##"/(foo)(?<abc>xxx)?(BAZ)?/.match("foobarbaz")["abc"]"##,
        ]);
        run_test_error(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")["abc"]"##);
    }

    #[test]
    fn match_data_begin_end_named_via_last_match_size_regexp_pre_post() {
        run_tests(&[
            r##"/(foo)(bar)/.match("foobar").begin(0)"##,
            r##"/(foo)(bar)/.match("foobar").begin(1)"##,
            r##"/(foo)(bar)/.match("foobar").begin(2)"##,
            r##"/(foo)(bar)/.match("foobar").end(0)"##,
            r##"/(foo)(bar)/.match("foobar").end(1)"##,
            r##"/(foo)(bar)/.match("foobar").end(2)"##,
            // nil group
            r##"/(foo)(bar)(BAZ)?/.match("foobar").begin(3)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobar").end(3)"##,
            r##"/(?<a>foo)(?<b>bar)/.match("foobar").named_captures"##,
            r#"
            "foobar" =~ /(foo)(bar)/
            m = Regexp.last_match
            [m.class.to_s, m[0], m[1], m[2], m.begin(0), m.end(0), m.begin(1), m.end(2)]
            "#,
            r#"
            "hello world" =~ /(\w+)\s(\w+)/
            [Regexp.last_match.begin(1), Regexp.last_match.end(2)]
            "#,
            r#"
            "abc".scan(/(b)/) { }
            [Regexp.last_match.class.to_s, Regexp.last_match[0], Regexp.last_match[1]]
            "#,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").size"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").length"##,
            r##"/(foo)(bar)/.match("foobarbaz").regexp.source"##,
            r##"/(foo)(bar)/.match("foobarbaz").string"##,
            r##"/bar/.match("foobarbaz").pre_match"##,
            r##"/bar/.match("foobarbaz").post_match"##,
            r##"/^foo/.match("foobar").pre_match"##,
            r##"/baz$/.match("foobaz").post_match"##,
        ]);
    }

    #[test]
    fn match_data_offset() {
        run_tests(&[
            r##"/(foo)(bar)/.match("foobar").offset(0)"##,
            r##"/(foo)(bar)/.match("foobar").offset(1)"##,
            r##"/(foo)(bar)/.match("foobar").offset(2)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobar").offset(3)"##,
        ]);
        run_test_error(r##"/(foo)(bar)/.match("foobar").offset(10)"##);
    }

    #[test]
    fn match_data_names_values_at_deconstruct_match_byte_offsets() {
        run_tests(&[
            r##"/(?<a>foo)(?<b>bar)/.match("foobar").names"##,
            r##"/(foo)(bar)/.match("foobar").names"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz").values_at(0,1,2,3)"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz").values_at(-1,-2,-100,100)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").deconstruct"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").match(0)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").match(3)"##,
            r##"/(foo)(bar)/.match("foobar").match_length(0)"##,
            r##"/(foo)(bar)/.match("foobar").match_length(1)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobar").match_length(3)"##,
            r##"/(foo)(bar)/.match("foobar").bytebegin(0)"##,
            r##"/(foo)(bar)/.match("foobar").byteend(1)"##,
            r##"/(foo)(bar)/.match("foobar").byteoffset(2)"##,
            r##"/(foo)(bar)(BAZ)?/.match("foobar").byteoffset(3)"##,
            // multibyte: UTF-8, byte offsets differ from char offsets
            r##"/い/.match("あぃい").byteoffset(0)"##,
        ]);
    }

    #[test]
    fn match_data_begin_end_out_of_range() {
        // Index out of range -> IndexError
        run_test_error(r##"/(foo)(bar)/.match("foobar").begin(5)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").end(5)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").begin(-1)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").end(-1)"##);
    }

    #[test]
    fn match_data_allocate_undefined() {
        // CRuby `rb_undef_method`s allocate -> NoMethodError, not the
        // default "allocator undefined" TypeError.
        run_test_error(r##"MatchData.allocate"##);
    }

    #[test]
    fn match_data_index_with_start_length_and_range() {
        run_tests(&[
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[1, 2]"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[0, 4]"##,
            // Out-of-range start -> nil.
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[10, 1]"##,
            // Negative start counts from end.
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[-2, 2]"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[0..2]"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[1..]"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[..2]"##,
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[1...3]"##,
            // Out-of-range start -> nil.
            r##"/(foo)(bar)/.match("foobar")[10..20]"##,
        ]);
    }

    #[test]
    fn match_data_values_at_with_range_and_names() {
        run_tests(&[
            r##"/(?<x>.)(?<y>.)(?<z>.)/.match("abc").values_at(0, 1..2)"##,
            r##"/(?<x>.)(?<y>.)(?<z>.)/.match("abc").values_at(:x, :z)"##,
            // Range past the end pads with nil.
            r##"/(.)(.)(\d+)(\d)/.match("THX1138: The Movie").values_at(0..5)"##,
        ]);
        // Out-of-range Range -> RangeError with CRuby-format message.
        run_test_error(
            r##"/(.)(.)(\d+)(\d)/.match("THX1138: The Movie").values_at(-6..3)"##,
        );
    }

    #[test]
    fn match_data_position_methods_named_arg() {
        run_tests(&[
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").begin(:f)"##,
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").end("b")"##,
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").bytebegin(:b)"##,
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").byteend("f")"##,
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").byteoffset(:b)"##,
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").offset(:b)"##,
        ]);
        // Unknown name -> IndexError
        run_test_error(
            r##"/(?<f>foo)/.match("foo").begin(:nope)"##,
        );
    }

    #[test]
    fn match_data_position_methods_negative_arg() {
        // Negative integer arg -> IndexError (no wrap-around).
        run_test_error(r##"/(foo)/.match("foo").begin(-1)"##);
        run_test_error(r##"/(foo)/.match("foo").byteoffset(-1)"##);
        run_test_error(r##"/(foo)/.match("foo").offset(-1)"##);
    }

    #[test]
    fn match_data_string_frozen() {
        // MatchData#string is a single frozen object, the same on every call.
        run_test(r##"md = /(.)(\d+)/.match("THX1138"); [md.string, md.string.frozen?, md.string.equal?(md.string)]"##);
    }

    #[test]
    fn match_data_regexp_after_gsub_string() {
        // gsub(String) attaches the (escaped) Regexp to `$~`.
        run_test(r#"'he[[o'.gsub('[', ']'); $~.regexp == /\[/"#);
    }

    #[test]
    fn match_data_result_encoding() {
        run_tests(&[
            // Capture / pre_match / post_match strings inherit the subject's
            // encoding (not a fixed UTF-8/ASCII-8BIT auto-detection).
            r#"s = "abc".dup.force_encoding(Encoding::EUC_JP); \
               m = s.match(/b/); [m.pre_match.encoding.to_s, m.post_match.encoding.to_s]"#,
            r#"s = "abc".dup.force_encoding(Encoding::ISO_8859_1); \
               s.match(/a/).pre_match.encoding.to_s"#,
            r#"md = "haystack".dup.force_encoding("euc-jp").match(/(?<t>t(?<a>ack))/u); \
               [md[:t].encoding.to_s, md[1].encoding.to_s]"#,
            r#"s = "abc".dup.force_encoding(Encoding::EUC_JP); \
               s.match(/(b)(c)/).captures.map { |x| x.encoding.to_s }"#,
        ]);
    }

    #[test]
    fn backref_shared_with_lambda() {
        // A match performed before a lambda is visible as `$~` inside it
        // (lambdas share their defining method frame's special variables).
        run_test(r##"/(?<t>\w+)/ =~ "TEST"; l = -> { Regexp.last_match(:t) }; l.call"##);
        run_test(r##"def m; /(\w)(\w)/ =~ "xy"; -> { $~[0] }.call; end; m"##);
        run_test(r##"/(\w+)/ =~ "abc"; [1].each { }; $1"##);
    }

    /// A group name is looked up and handed out by its bytes, in the
    /// pattern's encoding (#1902): a Windows-31J `(?<n\x87]>…)` is
    /// reachable by its symbol and string, and every listing of the
    /// names gives it back in Windows-31J. ASCII names of a US-ASCII
    /// pattern are US-ASCII; `Regexp#named_captures` keys are BINARY.
    #[test]
    fn match_data_non_utf8_group_names() {
        run_test_once(
            r##"
            src = <<~'R'.gsub("NAME", "n\x87]").force_encoding("Windows-31J")
              m = 'abbc'.match(/(?<NAME>b+)(?<x>c)/)
              r = [$~[:NAME], $~.names, Regexp.last_match(:NAME), m["NAME"], m.begin(:NAME), m.byteoffset("NAME"),
                   m.values_at(:NAME), m.named_captures, m.named_captures(symbolize_names: true),
                   m.deconstruct_keys(nil), m.deconstruct_keys([:NAME]), m.regexp.names, m.regexp.named_captures,
                   'abbc'[/(?<NAME>b+)/, :NAME], 'abbc'[/(?<NAME>b+)/, "NAME"], 'abbc'.sub(/(?<NAME>b+)/, '<\k<NAME>>'),
                   (m[:zz] rescue $!.class)]
              r << (/(?<NAME>b+)/ =~ 'abbc' && NAME)
              case m
              in {NAME: String => v} then r << v
              end
              r
            R
            show = ->(v) { v.is_a?(Array) || v.is_a?(Hash) ? v.to_a.flatten.map(&show) : (v.respond_to?(:encoding) ? [v.class, v.encoding.to_s, v.to_s.bytes] : v) }
            show.(eval(src))
            "##,
        );
        run_test_once(
            r##"
            m = /(?<a>x)(?<a>y)?(?<b>z)/.match("xz")
            u = Regexp.new("(?<ab>x)".encode("UTF-16LE"))
            r = [m.names, m.named_captures, m.regexp.names, m.regexp.named_captures, m[:a],
                 /(?<a>x)/n.names, Regexp.new("(?<a>x)".encode("EUC-JP")).names, u.names, u.named_captures,
                 "xyz".sub(/(?<a>y)/, '[\k<a>]')]
            show = ->(v) { v.is_a?(Array) || v.is_a?(Hash) ? v.to_a.flatten.map(&show) : (v.respond_to?(:encoding) ? [v.class, v.encoding.to_s, v.to_s.bytes] : v) }
            show.(r)
            "##,
        );
    }

    /// Every String method that matches a Regexp leaves that Regexp on
    /// `$~` (#1946): `$~.regexp`, the group names and `$~[:name]` work
    /// after `sub`, `scan`, `index`, `[]`, `partition`, … as they do
    /// after `=~`.
    #[test]
    fn backref_carries_the_regexp() {
        run_test(
            r##"
        re = /(?<n>b)(c)?/
        [
          -> { "abc" =~ re },
          -> { "abc".match(re) },
          -> { "abc".match("b") },
          -> { "abc".sub(re, "x") },
          -> { (+"abc").sub!(re, "x") },
          -> { "abc".sub(re) { "x" } },
          -> { "abc".sub(re, "b" => "x") },
          -> { "abcabc".gsub(re, "x") },
          -> { (+"abcabc").gsub!(re, "x") },
          -> { "abcabc".gsub(re) { "x" } },
          -> { "abcabc".gsub(re, "b" => "x") },
          -> { "abcabc".scan(re) },
          -> { "abcabc".scan(re) { } },
          -> { "abc".index(re) },
          -> { "abcabc".index(re, 2) },
          -> { "abcabc".rindex(re) },
          -> { "abcabc".rindex(re, 3) },
          -> { "abcabc".byteindex(re, 2) },
          -> { "abcabc".byterindex(re, 3) },
          -> { "abc"[re] },
          -> { "abc"[re, 1] },
          -> { "abc"[re, "n"] },
          -> { "abc".slice(re) },
          -> { (+"abc").slice!(re) },
          -> { (+"abc").slice!(re, 1) },
          -> { s = +"abc"; s[re] = "x" },
          -> { s = +"abc"; s[re, 1] = "x" },
          -> { "abc".partition(re) },
          -> { "abcabc".rpartition(re) },
          -> { "bc".start_with?(re) },
          -> { $_ = "abc"; ~re },
          -> { :abc[re] },
          -> { :abc =~ re },
          -> { :abc.match(re) },
          -> { :bc.start_with?(re) },
          -> { re =~ "abc" },
          -> { re === "abc" },
          -> { re.match("abcabc", 2) },
          -> { case "abc" when re then 1 end },
        ].map do |f|
          f.call
          m = $~
          [m.regexp.equal?(re) || m.regexp.source, m.names, (m[:n] rescue $!.class.to_s), m.inspect]
        end
        "##,
        );
        // Inside the block, and for a subject Onigmo walks on its own
        // bytes or that is matched through a surrogate image.
        run_test(
            r##"
        re = /(?<n>b)/
        r = []
        "abcabc".gsub(re) { r << $~[:n]; "" }
        "abcabc".scan(re) { r << $~[:n] << $~.regexp.source }
        "abc".sub(re) { r << $~.names; "" }
        [
          "\x82\xA0b\x82\xA0b".dup.force_encoding("Windows-31J"),
          "\xA4\xA2b\xA4\xA2b".dup.force_encoding("EUC-JP"),
          "a\xffb\xffb".b,
          "a\xE9b\xE9b".dup.force_encoding("ISO-8859-1"),
        ].each do |s|
          [
            -> { s =~ re }, -> { s.sub(re, "x") }, -> { s.gsub(re, "x") }, -> { s.gsub(re) { "" } },
            -> { s.scan(re) }, -> { s.scan(re) { } }, -> { s.index(re) }, -> { s.rindex(re) },
            -> { s.byteindex(re) }, -> { s.byterindex(re) }, -> { s[re] }, -> { s.dup.slice!(re) },
            -> { s.partition(re) }, -> { s.rpartition(re) }, -> { s.split(re, 2) }, -> { re.match(s) },
          ].each do |f|
            f.call
            m = $~
            r << [m.regexp.equal?(re), m[:n], m.pre_match.bytes, m.pre_match.encoding.to_s, m.byteoffset(0)]
          end
        end
        r
        "##,
        );
    }

    /// A String pattern is a substring search, and its `$~` is the
    /// match CRuby's `rb_backref_set_string` records: no Regexp until
    /// one is asked for, the `#<MatchData: b>` form, and offsets and
    /// `pre_match` over the receiver's own bytes whatever its encoding
    /// (not over the image its bytes are matched through).
    #[test]
    fn backref_of_a_string_pattern() {
        run_test(
            r##"
        r = []
        probe = ->(m) {
          return nil unless m
          i = (m.inspect.b rescue $!.message)
          re = m.regexp
          [i, m.inspect.b, re.source.bytes, re.encoding.to_s, re.options, m.names, m.to_a.map(&:bytes),
           m.pre_match.bytes, m.post_match.bytes, m.pre_match.encoding.to_s, m.offset(0), m.byteoffset(0),
           m.string.bytes, m.string.encoding.to_s]
        }
        [
          ["abcab", "b"],
          ["a.*b.*", ".*"],
          ["aあbあ", "あ"],
          ["abcab".dup.force_encoding("US-ASCII"), "b"],
          ["a\xffb\xfe\xffc".b, "\xff".b],
          ["a\xE9b\xE8\xE9c".dup.force_encoding("ISO-8859-1"), "\xE9".dup.force_encoding("ISO-8859-1")],
          ["\x82\xA0b\x82\xA2\x82\xA0c".dup.force_encoding("Windows-31J"), "\x82\xA0".dup.force_encoding("Windows-31J")],
          ["\xA4\xA2b\xA4\xA4\xA4\xA2c".dup.force_encoding("EUC-JP"), "\xA4\xA2".dup.force_encoding("EUC-JP")],
          ["abcab".encode("UTF-16LE"), "b".encode("UTF-16LE")],
          ["aあbあ".encode("UTF-32BE"), "あ".encode("UTF-32BE")],
        ].each do |s, pat|
          m = nil
          s.sub(pat) { m = $~; "" };            r << probe.(m)
          s.sub(pat, {});                       r << probe.($~)
          s.sub(pat, "");                       r << probe.($~)
          s.dup.sub!(pat, "");                  r << probe.($~)
          s.gsub(pat) { m = $~; "" };           r << probe.(m) << probe.($~)
          s.gsub(pat, {});                      r << probe.($~)
          s.gsub(pat, "");                      r << probe.($~)
          s.dup.gsub!(pat) { "" };              r << probe.($~)
          s.scan(pat);                          r << probe.($~)
          s.scan(pat) { m = $~ };               r << probe.(m) << probe.($~)
          r << probe.(s.match(pat))
        end
        r
        "##,
        );
        // `==` and `hash` go through the Regexp the match stands for.
        run_test(
            r##"
        a = b = c = nil
        "abc".sub("b") { a = $~; "" }
        "abc".sub("b") { b = $~; "" }
        "abd".sub("b") { c = $~; "" }
        d = "abc".match(/b/)
        e = "abc".match(/b/i)
        h = { a => 1 }
        [a == b, a.eql?(b), a.hash == b.hash, a == c, a == d, a.hash == d.hash, d == e, h[b], h[d], h[c],
         a.regexp.equal?(a.regexp), a.dup.regexp.source, a.inspect]
        "##,
        );
    }

    /// `start_with?` matches at the start only, so a match further on
    /// leaves `$~` nil; a wide String separator is walked by a Regexp
    /// `split` builds for itself, which is not the caller's match.
    #[test]
    fn backref_start_with_and_split() {
        run_test(
            r##"
        r = []
        "abc" =~ /a/
        r << "abc".start_with?(/b/) << $~.inspect
        r << "abc".start_with?(/(?<n>a)b/) << $~.inspect << $~[:n]
        r << :abc.start_with?(/b/) << $~.inspect
        # A Symbol whose name is walked on its own bytes: `$~` is over a
        # String made for it.
        sym = "a\xE9b".dup.force_encoding("Windows-1252").to_sym
        r << sym.start_with?(/a./) << $~[0].bytes << $~.post_match.bytes << $~[0].encoding.to_s << $~.regexp.source
        "abc" =~ /a/
        u = ->(s) { s.encode("UTF-16LE") }
        r << u["abcab"].split(u["b"]).size << $~.inspect
        r << "abcab".split("b").size << $~.inspect
        r << "abcab".split(/(?<n>b)/, 2).size << $~.inspect << $~[:n]
        r << "abcab".split(/b/).size << $~.inspect
        r
        "##,
        );
    }
}
