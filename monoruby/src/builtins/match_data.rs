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
    globals.define_builtin_func_with(MATCHDATA_CLASS, "deconstruct_keys", deconstruct_keys_md, 1, 1, false);
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
        IdentId::get_id("allocate"),
        MATCHDATA_CLASS,
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
    if let Some(name) = arg.try_symbol_or_string() {
        let group_name = name.to_string();
        if let Some(i) = m
            .regexp()
            .and_then(|r| r.capture_names().ok())
            .and_then(|names| {
                let mut last: Option<usize> = None;
                for (i, n) in names.iter().enumerate() {
                    if *n == group_name {
                        last = Some(i + 1);
                    }
                }
                last
            })
        {
            return Ok(i);
        }
        return Err(MonorubyErr::indexerr(format!(
            "undefined group name reference: {group_name}"
        )));
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
    let names = m
        .regexp()
        .and_then(|r| r.capture_names().ok())
        .unwrap_or_default();
    let val_for = |name: &str| -> Value {
        let mut last: Option<usize> = None;
        for (i, n) in names.iter().enumerate() {
            if n == name {
                last = Some(i);
            }
        }
        match last.and_then(|i| m.at(i + 1)) {
            Some(s) => Value::string_from_str(s),
            None => Value::nil(),
        }
    };
    let mut map = RubyMap::default();
    if arg.is_nil() {
        for name in &names {
            let key = Value::symbol_from_str(name);
            map.insert(key, val_for(name), vm, globals)?;
        }
    } else if arg.is_array_ty() {
        let arr = arg.as_array();
        // CRuby: if any requested symbol isn't a named capture, return `{}`.
        for key in arr.iter() {
            let Some(sym) = key.try_symbol() else {
                return Ok(Value::hash(RubyMap::default()));
            };
            let name = sym.get_name();
            if !names.iter().any(|n| *n == name) {
                return Ok(Value::hash(RubyMap::default()));
            }
            map.insert(*key, val_for(&name), vm, globals)?;
        }
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Array or nil)",
            arg.get_real_class_name(globals)
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
fn regexp_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().as_match_data().regexp() {
        Some(r) => Ok(r.as_val()),
        None => Ok(Value::nil()),
    }
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
    Ok(Value::string_from_str(lfp.self_val().as_match_data().string()))
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
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    match m.pos(0) {
        Some((start, _)) => Ok(Value::string_from_str(&m.string()[..start])),
        None => Ok(Value::string_from_str("")),
    }
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
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    match m.pos(0) {
        Some((_, end)) => Ok(Value::string_from_str(&m.string()[end..])),
        None => Ok(Value::string_from_str("")),
    }
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
            let s = m.string()[..start].chars().count() as i64;
            let e = m.string()[..end].chars().count() as i64;
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
    let names = m.regexp().and_then(|r| r.capture_names().ok()).unwrap_or_default();
    Ok(Value::array_from_iter(
        names.iter().map(|n| Value::string_from_str(n)),
    ))
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
                res.push(m.at(i).map(Value::string_from_str).unwrap_or_default());
            }
            // Pad with nil for positions past the end.
            let requested = (start + slice_len) as usize;
            for _ in e..requested {
                res.push(Value::nil());
            }
            continue;
        }
        // Symbol/String: named-capture lookup.
        if let Some(name) = a.try_symbol_or_string() {
            let group_name = name.to_string();
            let i = m
                .regexp()
                .and_then(|r| r.capture_names().ok())
                .and_then(|names| {
                    let mut last: Option<usize> = None;
                    for (i, n) in names.iter().enumerate() {
                        if *n == group_name {
                            last = Some(i + 1);
                        }
                    }
                    last
                })
                .ok_or_else(|| {
                    MonorubyErr::indexerr(format!(
                        "undefined group name reference: {group_name}"
                    ))
                })?;
            res.push(
                m.at(i)
                    .map(Value::string_from_str)
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
            res.push(m.at(idx).map(Value::string_from_str).unwrap_or_default());
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
        lfp.self_val().as_match_data().captures().skip(1).map(|s| {
            s.map(Value::string_from_str).unwrap_or_default()
        }),
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
        Ok(m.at(idx).map(Value::string_from_str).unwrap_or_default())
    } else if let Some(sym) = arg.try_symbol_or_string() {
        if let Some(i) = m
            .regexp()
            .map(|r| r.get_group_members(&format!("{sym}")))
            .and_then(|g| g.last().copied())
        {
            Ok(m.at(i as usize).map(Value::string_from_str).unwrap_or_default())
        } else {
            Err(MonorubyErr::indexerr(format!(
                "undefined group name reference: {sym}"
            )))
        }
    } else {
        Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Integer",
            arg.get_real_class_name(globals)
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
    } else if let Some(sym) = arg.try_symbol_or_string() {
        if let Some(i) = m
            .regexp()
            .map(|r| r.get_group_members(&format!("{sym}")))
            .and_then(|g| g.last().copied())
        {
            i as usize
        } else {
            return Err(MonorubyErr::indexerr(format!(
                "undefined group name reference: {sym}"
            )));
        }
    } else {
        let _ = (vm, globals);
        return Err(MonorubyErr::typeerr("no implicit conversion into Integer"));
    };
    if idx >= m.len() {
        return Ok(Value::nil());
    }
    match m.at(idx) {
        Some(s) => Ok(Value::integer(s.chars().count() as i64)),
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
        lfp.self_val().as_match_data().captures().skip(1).map(|s| {
            if let Some(s) = s {
                Value::string_from_str(s)
            } else {
                Value::nil()
            }
        }),
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
        lfp.self_val().as_match_data().captures().map(|s| {
            if let Some(s) = s {
                Value::string_from_str(s)
            } else {
                Value::nil()
            }
        }),
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
                lfp.arg(0).get_real_class_name(globals)
            ))
        })?;
        let len = arg1.try_fixnum().ok_or_else(|| {
            MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into Integer",
                arg1.get_real_class_name(globals)
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
        Ok(m.at(idx as usize)
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default())
    } else if let Some(range) = lfp.arg(0).is_range() {
        // `[start..end]` / `[start...]` / `[..end]` slicing.
        let len = m.len() as i64;
        let (start, slice_len) = match range_to_slice(range, len) {
            Some(v) => v,
            None => return Ok(Value::nil()),
        };
        Ok(slice_match_data(&m, start, slice_len))
    } else if let Some(sym) = lfp.arg(0).try_symbol_or_string() {
        if let Some(i) = m
            .regexp()
            .map(|r| r.get_group_members(&format!("{sym}")))
            .and_then(|g| g.last().copied())
        {
            if let Some(s) = m.at(i as usize) {
                Ok(Value::string_from_str(s))
            } else {
                Ok(Value::nil())
            }
        } else {
            Err(MonorubyErr::indexerr(format!(
                "undefined group name reference: {sym}"
            )))
        }
    } else {
        Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Integer",
            lfp.arg(0).get_real_class_name(globals)
        )))
    }
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
            m.at(i)
                .map(|v| Value::string_from_str(v))
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
            let char_offset = m.string()[..start].chars().count();
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
            let char_offset = m.string()[..end_pos].chars().count();
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
    let names = m
        .regexp()
        .and_then(|r| r.capture_names().ok())
        .unwrap_or_default();
    let mut map = RubyMap::default();
    for (i, name) in names.iter().enumerate() {
        let key = Value::string_from_str(name);
        let val = m.at(i + 1)
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default();
        map.insert(key, val, vm, globals)?;
    }
    Ok(Value::hash(map))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn match_data() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").to_s"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(?<c>BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(?<a>BAZ)?/.match("foobarbaz").inspect"##);

        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").captures"##);
    }

    #[test]
    fn match_data_index() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-100]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-1]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[0]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[100]"##);
        run_test(r##"/(foo)(?<abc>bar)(BAZ)?/.match("foobarbaz")["abc"]"##);
        run_test(r##"/(foo)(?<abc>xxx)?(BAZ)?/.match("foobarbaz")["abc"]"##);
        run_test_error(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")["abc"]"##);
    }

    #[test]
    fn match_data_begin_end() {
        run_test(r##"/(foo)(bar)/.match("foobar").begin(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").begin(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").begin(2)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(2)"##);
        // nil group
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").begin(3)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").end(3)"##);
    }

    #[test]
    fn match_data_named_captures() {
        run_test(r##"/(?<a>foo)(?<b>bar)/.match("foobar").named_captures"##);
    }

    #[test]
    fn match_data_via_last_match() {
        run_test(
            r#"
            "foobar" =~ /(foo)(bar)/
            m = Regexp.last_match
            [m.class.to_s, m[0], m[1], m[2], m.begin(0), m.end(0), m.begin(1), m.end(2)]
            "#,
        );
        run_test(
            r#"
            "hello world" =~ /(\w+)\s(\w+)/
            [Regexp.last_match.begin(1), Regexp.last_match.end(2)]
            "#,
        );
        run_test(
            r#"
            "abc".scan(/(b)/) { }
            [Regexp.last_match.class.to_s, Regexp.last_match[0], Regexp.last_match[1]]
            "#,
        );
    }

    #[test]
    fn match_data_size_length() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").size"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").length"##);
    }

    #[test]
    fn match_data_regexp_string() {
        run_test(r##"/(foo)(bar)/.match("foobarbaz").regexp.source"##);
        run_test(r##"/(foo)(bar)/.match("foobarbaz").string"##);
    }

    #[test]
    fn match_data_pre_post_match() {
        run_test(r##"/bar/.match("foobarbaz").pre_match"##);
        run_test(r##"/bar/.match("foobarbaz").post_match"##);
        run_test(r##"/^foo/.match("foobar").pre_match"##);
        run_test(r##"/baz$/.match("foobaz").post_match"##);
    }

    #[test]
    fn match_data_offset() {
        run_test(r##"/(foo)(bar)/.match("foobar").offset(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").offset(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").offset(2)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").offset(3)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").offset(10)"##);
    }

    #[test]
    fn match_data_names() {
        run_test(r##"/(?<a>foo)(?<b>bar)/.match("foobar").names"##);
        run_test(r##"/(foo)(bar)/.match("foobar").names"##);
    }

    #[test]
    fn match_data_values_at() {
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz").values_at(0,1,2,3)"##);
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz").values_at(-1,-2,-100,100)"##);
    }

    #[test]
    fn match_data_deconstruct() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").deconstruct"##);
    }

    #[test]
    fn match_data_match_and_match_length() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").match(0)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").match(3)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").match_length(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").match_length(1)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").match_length(3)"##);
    }

    #[test]
    fn match_data_byte_offsets() {
        run_test(r##"/(foo)(bar)/.match("foobar").bytebegin(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").byteend(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").byteoffset(2)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").byteoffset(3)"##);
        // multibyte: UTF-8, byte offsets differ from char offsets
        run_test(r##"/い/.match("あぃい").byteoffset(0)"##);
    }

    #[test]
    fn match_data_deconstruct_keys() {
        run_test(
            r##"/(?<a>foo)(?<b>bar)/.match("foobar").deconstruct_keys(nil)"##,
        );
        run_test(
            r##"/(?<a>foo)(?<b>bar)/.match("foobar").deconstruct_keys([:a])"##,
        );
        run_test(
            r##"/(?<a>foo)(?<b>bar)/.match("foobar").deconstruct_keys([:a, :missing, :b])"##,
        );
        // No named captures → empty hash
        run_test(r##"/(foo)(bar)/.match("foobar").deconstruct_keys(nil)"##);
        run_test_error(r##"/(?<a>x)/.match("x").deconstruct_keys(1)"##);
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
    fn match_data_index_with_start_length() {
        run_test(
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[1, 2]"##,
        );
        run_test(
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[0, 4]"##,
        );
        // Out-of-range start -> nil.
        run_test(
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[10, 1]"##,
        );
        // Negative start counts from end.
        run_test(
            r##"/(foo)(bar)(baz)/.match("foobarbaz")[-2, 2]"##,
        );
    }

    #[test]
    fn match_data_index_with_range() {
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz")[0..2]"##);
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz")[1..]"##);
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz")[..2]"##);
        run_test(r##"/(foo)(bar)(baz)/.match("foobarbaz")[1...3]"##);
        // Out-of-range start -> nil.
        run_test(r##"/(foo)(bar)/.match("foobar")[10..20]"##);
    }

    #[test]
    fn match_data_values_at_with_range_and_names() {
        run_test(
            r##"/(?<x>.)(?<y>.)(?<z>.)/.match("abc").values_at(0, 1..2)"##,
        );
        run_test(
            r##"/(?<x>.)(?<y>.)(?<z>.)/.match("abc").values_at(:x, :z)"##,
        );
        // Range past the end pads with nil.
        run_test(
            r##"/(.)(.)(\d+)(\d)/.match("THX1138: The Movie").values_at(0..5)"##,
        );
        // Out-of-range Range -> RangeError with CRuby-format message.
        run_test_error(
            r##"/(.)(.)(\d+)(\d)/.match("THX1138: The Movie").values_at(-6..3)"##,
        );
    }

    #[test]
    fn match_data_position_methods_named_arg() {
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").begin(:f)"##,
        );
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").end("b")"##,
        );
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").bytebegin(:b)"##,
        );
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").byteend("f")"##,
        );
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").byteoffset(:b)"##,
        );
        run_test(
            r##"/(?<f>foo)(?<b>bar)/.match("foobar").offset(:b)"##,
        );
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
}
