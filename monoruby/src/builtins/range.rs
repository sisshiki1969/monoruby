use super::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Range", RANGE_CLASS, ObjTy::RANGE);
    globals.define_builtin_class_func_with(RANGE_CLASS, "new", range_new, 2, 3, false);
    globals.store[RANGE_CLASS].set_alloc_func(range_alloc_func);
    globals.define_builtin_inline_func(RANGE_CLASS, "begin", begin, Box::new(range_begin), 0);
    globals.define_builtin_func_with(RANGE_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_inline_func(RANGE_CLASS, "end", end, Box::new(range_end), 0);
    globals.define_builtin_func_with(RANGE_CLASS, "last", last, 0, 1, false);
    globals.define_builtin_inline_func(
        RANGE_CLASS,
        "exclude_end?",
        exclude_end,
        Box::new(range_exclude_end),
        0,
    );
    globals.define_builtin_func(RANGE_CLASS, "each", each, 0);
    //globals.define_builtin_func(RANGE_CLASS, "reject", reject, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "include?", &["member?"], include_, 1);
    globals.define_builtin_func(RANGE_CLASS, "===", teq, 1);
    globals.define_builtin_func(RANGE_CLASS, "all?", all_, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "collect", &["map"], map, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "collect_concat", &["flat_map"], flat_map, 0);
    globals.define_builtin_funcs(RANGE_CLASS, "entries", &["to_a"], toa, 0);
    globals.define_builtin_func(RANGE_CLASS, "min", min, 0);
    globals.define_builtin_func(RANGE_CLASS, "max", max, 0);
    globals.define_builtin_func(RANGE_CLASS, "count", count, 0);
    globals.define_builtin_func(RANGE_CLASS, "minmax", minmax, 0);
}

///
/// ### Range.new
///
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
#[monoruby_builtin]
fn range_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let exclude_end = lfp.try_arg(2).map(|v| v.as_bool()).unwrap_or(false);
    globals.generate_range(lfp.arg(0), lfp.arg(1), exclude_end)
}

/// Allocator for `Range` and its subclasses.
pub(crate) extern "C" fn range_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::range_with_class(Value::nil(), Value::nil(), false, class_id)
}

///
/// ### Range#begin
///
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_range().start())
}

fn range_begin(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        if let Some(start) = range.start().is_immediate() {
            state.def_C(dst, start);
            return true;
        }
    }
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, [rdi + (crate::rvalue::RANGE_START_OFFSET as i32)];
        }
    });

    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

///
/// Range#end
///
/// - end -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/end.html]
#[monoruby_builtin]
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_range().end())
}

fn range_end(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        if let Some(end) = range.end().is_immediate() {
            state.def_C(dst, end);
            return true;
        }
    }
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, [rdi + (crate::rvalue::RANGE_END_OFFSET as i32)];
        }
    });

    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

///
/// ### Range#first
///
/// - first -> object
/// - first(n) -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/first.html]
#[monoruby_builtin]
fn first(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some(n_val) = lfp.try_arg(0) {
        let n = n_val.coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = n as usize;
        if let Some((start, end)) = range.try_fixnum() {
            let vec: Vec<Value> = (start..end).take(n).map(Value::integer).collect();
            Ok(Value::array_from_vec(vec))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        Ok(range.start())
    }
}

///
/// ### Range#last
///
/// - last -> object
/// - last(n) -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/last.html]
#[monoruby_builtin]
fn last(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some(n_val) = lfp.try_arg(0) {
        let n = n_val.coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = n as usize;
        if let Some((start, end)) = range.try_fixnum() {
            let len = (end - start).max(0) as usize;
            let skip = if n >= len { 0 } else { len - n };
            let vec: Vec<Value> = (start..end).skip(skip).map(Value::integer).collect();
            Ok(Value::array_from_vec(vec))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        Ok(range.end())
    }
}

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_range().exclude_end()))
}

fn range_exclude_end(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        let exclude_end = range.exclude_end();
        state.def_C(dst, Immediate::bool(exclude_end));
        return true;
    }
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movl rax, [rdi + (crate::rvalue::RANGE_EXCLUDE_END_OFFSET as i32)];
            shlq rax, 3;
            orq  rax, (FALSE_VALUE);
        }
    });

    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

///
/// ### Range#each
///
/// - each {|item| .... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some((start, end)) = range.try_fixnum() {
        let iter = (start..end).map(Value::integer);
        vm.invoke_block_iter1(globals, bh, iter)?;
        Ok(self_)
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

/*
///
/// ### Enumerable#reject
///
/// - reject {|item| ... } -> [object]
/// - reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        let mut elem = range.start;
        let end = range.end;
        while !vm
            .invoke_method_inner(globals, IdentId::_EQ, elem, &[end], None)?
            .as_bool()
        {
            if !vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                res.push(elem);
            };
            elem = vm.invoke_method_inner(globals, IdentId::get_id("succ"), elem, &[], None)?;
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("reject"), lfp.self_val(), vec![])
    }
}*/

/// Shared implementation of the Range cover/include logic.
///
/// Mirrors CRuby's `range_include_internal`:
/// - Numeric endpoints (Fixnum, Float, BigInt) and nil-bounded numeric ranges
///   are handled with comparison-based cover check (r_cover_p equivalent).
/// - Both-nil ranges return `true` for numeric values.
/// - String ranges use lexicographic comparison.
/// - Beginless/endless ranges with non-numeric, non-nil endpoints raise TypeError.
/// - All other combinations return `false`.
fn range_include_impl(start: Value, end: Value, val: Value, excl: bool) -> Result<bool> {
    /// Convert a Value to f64 if it is numeric; return None otherwise.
    #[inline]
    fn to_f64(v: Value) -> Option<f64> {
        match v.unpack() {
            RV::Fixnum(n) => Some(n as f64),
            RV::Float(f) => Some(f),
            RV::BigInt(b) => b.to_f64(),
            _ => None,
        }
    }

    /// Check `lower <= val` (or lower is None ≡ -∞).
    #[inline]
    fn above(lower: Option<f64>, val: f64) -> bool {
        lower.map_or(true, |lo| lo <= val)
    }

    /// Check `val <= upper` (or upper is None ≡ +∞), respecting exclusivity.
    #[inline]
    fn below(val: f64, upper: Option<f64>, excl: bool) -> bool {
        upper.map_or(true, |hi| if excl { val < hi } else { val <= hi })
    }

    match (start.unpack(), end.unpack()) {
        // ── Both endpoints are Fixnum ───────────────────────────────────────
        (RV::Fixnum(s), RV::Fixnum(e)) => Ok(match val.unpack() {
            RV::Fixnum(v) => s <= v && if excl { v < e } else { v <= e },
            RV::BigInt(v) => {
                let s = num::BigInt::from(s);
                let e = num::BigInt::from(e);
                &s <= v && if excl { v < &e } else { v <= &e }
            }
            RV::Float(v) => above(Some(s as f64), v) && below(v, Some(e as f64), excl),
            _ => false,
        }),
        (RV::BigInt(s), RV::Fixnum(e)) => Ok(match val.unpack() {
            RV::Fixnum(v) => {
                let e = num::BigInt::from(e);
                let v = num::BigInt::from(v);
                s <= &v && if excl { v < e } else { v <= e }
            }
            RV::BigInt(v) => {
                let e = num::BigInt::from(e);
                &s <= &v && if excl { v < &e } else { v <= &e }
            }
            RV::Float(v) => {
                let s = s.to_f64().unwrap();
                let e = e as f64;
                above(Some(s), v) && below(v, Some(e), excl)
            }
            _ => false,
        }),
        (RV::Fixnum(s), RV::BigInt(e)) => Ok(match val.unpack() {
            RV::Fixnum(v) => {
                let s = num::BigInt::from(s);
                let v = num::BigInt::from(v);
                s <= v && if excl { &v < e } else { &v <= e }
            }
            RV::BigInt(v) => {
                let s = num::BigInt::from(s);
                &s <= v && if excl { v < &e } else { v <= &e }
            }
            RV::Float(v) => {
                let s = s as f64;
                let e = e.to_f64().unwrap();
                above(Some(s), v) && below(v, Some(e), excl)
            }
            _ => false,
        }),
        (RV::BigInt(s), RV::BigInt(e)) => Ok(match val.unpack() {
            RV::Fixnum(v) => {
                let v = num::BigInt::from(v);
                s <= &v && if excl { &v < e } else { &v <= e }
            }
            RV::BigInt(v) => s <= &v && if excl { v < e } else { v <= e },
            RV::Float(v) => {
                let s = s.to_f64().unwrap();
                let e = e.to_f64().unwrap();
                above(Some(s), v) && below(v, Some(e), excl)
            }
            _ => false,
        }),

        // ── Both endpoints are Float ────────────────────────────────────────
        (RV::Float(s), RV::Float(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v) && below(v, Some(e), excl))
        }

        // ── Mixed Fixnum / Float ────────────────────────────────────────────
        (RV::Fixnum(s), RV::Float(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s as f64), v) && below(v, Some(e), excl))
        }
        (RV::BigInt(s), RV::Float(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s.to_f64().unwrap()), v) && below(v, Some(e), excl))
        }
        (RV::Float(s), RV::Fixnum(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v) && below(v, Some(e as f64), excl))
        }
        (RV::Float(s), RV::BigInt(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v) && below(v, Some(e.to_f64().unwrap()), excl))
        }

        // ── Beginless numeric (nil..end) ────────────────────────────────────
        (RV::Nil, RV::Fixnum(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(below(v, Some(e as f64), excl))
        }
        (RV::Nil, RV::BigInt(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(below(v, Some(e.to_f64().unwrap()), excl))
        }
        (RV::Nil, RV::Float(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(below(v, Some(e), excl))
        }

        // ── Endless numeric (beg..nil) ──────────────────────────────────────
        (RV::Fixnum(s), RV::Nil) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s as f64), v))
        }
        (RV::BigInt(s), RV::Nil) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s.to_f64().unwrap()), v))
        }
        (RV::Float(s), RV::Nil) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v))
        }

        // ── Both nil (beginless-endless range) ──────────────────────────────
        // linear objects (Fixnum, Float, BigInt) are always included.
        (RV::Nil, RV::Nil) if val.is_linear() => Ok(true),

        // ── Both endpoints are String ────────────────────────────────────────
        // Use lexicographic comparison (matches CRuby for single-char strings;
        // for multi-byte strings this is equivalent to Range#cover?).
        (RV::String(s), RV::String(e)) => Ok(match val.unpack() {
            RV::String(v) => s <= v && if excl { v < e } else { v <= e },
            _ => false,
        }),

        // ── Beginless/endless with non-numeric, non-nil endpoint ────────────
        // CRuby raises TypeError here.
        (RV::Nil, _) | (_, RV::Nil) => Err(MonorubyErr::typeerr(
            "cannot determine inclusion in beginless/endless ranges",
        )),

        // ── All other types ─────────────────────────────────────────────────
        // CRuby falls through to Enumerable#include? (iterates the range).
        // We return false as a conservative fallback.
        _ => Ok(false),
    }
}

///
/// ### Range#include?
///
/// - include?(obj) -> bool
/// - member?(obj) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/include=3f.html]
#[monoruby_builtin]
fn include_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let b = range_include_impl(range.start(), range.end(), lfp.arg(0), range.exclude_end())?;
    Ok(Value::bool(b))
}

///
/// ### Range#===
///
/// - self === obj -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let b = range_include_impl(range.start(), range.end(), lfp.arg(0), range.exclude_end())?;
    Ok(Value::bool(b))
}

///
/// ### Range#all
///
/// - all? -> bool
/// - all? {|item| ... } -> bool
/// - all?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/all=3f.html]
#[monoruby_builtin]
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let self_ = lfp.self_val();
        let range = self_.as_range();
        if let Some((start, end)) = range.try_fixnum() {
            let iter = (start..end).map(Value::integer);
            let data = vm.get_block_data(globals, bh)?;
            for val in iter {
                if !vm.invoke_block(globals, &data, &[val])?.as_bool() {
                    return Ok(Value::bool(false));
                };
            }
            Ok(Value::bool(true))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        Ok(Value::bool(true))
    }
}

///
/// ### Enumerable#map
///
/// - [NOT SUPPORTED]collect -> Enumerator
/// - [NOT SUPPORTED]map -> Enumerator
/// - collect {|item| ... } -> [object]
/// - map {|item| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some((start, end)) = range.try_fixnum() {
        if end <= start {
            return Ok(Value::array_from_vec(vec![]));
        }

        let iter = (start..end).map(Value::integer);
        vm.invoke_block_map1(globals, bh, iter, (end - start).unsigned_abs() as usize)
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

/// ### Enumerable#collect_concat
///
/// - flat_map {| obj | block } -> Array
/// - collect_concat {| obj | block } -> Array
/// - [NOT SUPPORTED] flat_map -> Enumerator
/// - [NOT SUPPORTED] collect_concat -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect_concat.html]
#[monoruby_builtin]
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some((start, end)) = range.try_fixnum() {
        if end <= start {
            return Ok(Value::array_from_vec(vec![]));
        }

        let iter = (start..end).map(Value::integer);
        vm.invoke_block_flat_map1(globals, bh, iter, (end - start).unsigned_abs() as usize)
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

///
/// ### Range#entries
///
/// - to_a -> Array
/// - entries -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/entries.html]
#[monoruby_builtin]
fn toa(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some((start, end)) = range.try_fixnum() {
        let vec = (start..end).map(Value::integer).collect();
        Ok(Value::array_from_vec(vec))
    } else if let Some(start) = range.start().is_str()
        && let Some(end) = range.end().is_str()
    {
        let mut start = start.to_string();
        let end = end.to_string();
        let mut v = vec![];
        while start < end {
            v.push(Value::string_from_str(&start));
            start = builtins::string::str_next(&start);
        }
        if !range.exclude_end() && start == end {
            v.push(Value::string_from_str(&start));
        }
        Ok(Value::array_from_vec(v))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
    }
}

///
/// ### Range#min
///
/// - min -> object | nil
/// - min {|a, b| ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/min.html]
#[monoruby_builtin]
fn min(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let start = range.start();
    let end = range.end();

    if let Some(bh) = lfp.block() {
        // With block: raise RangeError for endless ranges, then delegate to iteration
        if end.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the minimum of endless range",
            ));
        }
        if start.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the minimum of beginless range",
            ));
        }
        // Check for empty range first
        let cmp = vm.compare_values_inner(globals, start, end)?;
        match cmp {
            Some(std::cmp::Ordering::Greater) => return Ok(Value::nil()),
            Some(std::cmp::Ordering::Equal) if range.exclude_end() => return Ok(Value::nil()),
            _ => {}
        }
        // Iterate the range and find min using the block
        if let Some((s, e)) = range.try_fixnum() {
            let data = vm.get_block_data(globals, bh)?;
            let mut min_val = Value::integer(s);
            for i in (s + 1)..e {
                let val = Value::integer(i);
                let cmp = vm.invoke_block(globals, &data, &[val, min_val])?;
                // CRuby calls < on the block result
                let is_less = vm
                    .invoke_method_inner(
                        globals,
                        IdentId::_LT,
                        cmp,
                        &[Value::integer(0)],
                        None,
                        None,
                    )?
                    .as_bool();
                if is_less {
                    min_val = val;
                }
            }
            Ok(min_val)
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        // No block
        if start.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the minimum of beginless range",
            ));
        }
        // Check for empty range
        if !end.is_nil() {
            let cmp = vm.compare_values_inner(globals, start, end)?;
            match cmp {
                Some(std::cmp::Ordering::Greater) => return Ok(Value::nil()),
                Some(std::cmp::Ordering::Equal) => {
                    if range.exclude_end() {
                        return Ok(Value::nil());
                    }
                }
                _ => {}
            }
        }
        Ok(start)
    }
}

///
/// ### Range#max
///
/// - max -> object | nil
/// - max {|a, b| ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/max.html]
#[monoruby_builtin]
fn max(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let start = range.start();
    let end = range.end();

    if let Some(bh) = lfp.block() {
        // With block: raise RangeError for endless/beginless ranges, then iterate
        if end.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the maximum of endless range",
            ));
        }
        if start.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the maximum of beginless range",
            ));
        }
        // Check for empty range first
        let cmp = vm.compare_values_inner(globals, start, end)?;
        match cmp {
            Some(std::cmp::Ordering::Greater) => return Ok(Value::nil()),
            Some(std::cmp::Ordering::Equal) if range.exclude_end() => return Ok(Value::nil()),
            _ => {}
        }
        if let Some((s, e)) = range.try_fixnum() {
            let data = vm.get_block_data(globals, bh)?;
            let mut max_val = Value::integer(s);
            for i in (s + 1)..e {
                let val = Value::integer(i);
                let cmp = vm.invoke_block(globals, &data, &[val, max_val])?;
                // CRuby calls > on the block result
                let is_greater = vm
                    .invoke_method_inner(
                        globals,
                        IdentId::_GT,
                        cmp,
                        &[Value::integer(0)],
                        None,
                        None,
                    )?
                    .as_bool();
                if is_greater {
                    max_val = val;
                }
            }
            Ok(max_val)
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        // No block
        if end.is_nil() {
            return Err(MonorubyErr::rangeerr(
                "cannot get the maximum of endless range",
            ));
        }

        // Check for empty range
        if !start.is_nil() {
            let cmp = vm.compare_values_inner(globals, start, end)?;
            match cmp {
                Some(std::cmp::Ordering::Greater) => return Ok(Value::nil()),
                Some(std::cmp::Ordering::Equal) => {
                    if range.exclude_end() {
                        return Ok(Value::nil());
                    }
                    return Ok(end);
                }
                _ => {}
            }
        }

        if range.exclude_end() {
            // For exclusive ranges, need integer end or string end
            if let Some(i) = end.try_fixnum() {
                // For beginless exclusive integer ranges, raise TypeError
                if start.is_nil() {
                    return Err(MonorubyErr::typeerr(
                        "cannot exclude end value with non Integer begin value",
                    ));
                }
                return Ok(Value::integer(i - 1));
            } else if let RV::BigInt(b) = end.unpack() {
                return Ok(Value::bigint(b - 1));
            } else if let Some(end_str) = end.is_str() {
                // For exclusive string ranges, iterate to find the predecessor
                if let Some(start_str) = start.is_str() {
                    let mut current = start_str.to_string();
                    let mut prev = current.clone();
                    while current != end_str {
                        prev = current.clone();
                        current = builtins::string::str_next(&current);
                    }
                    return Ok(Value::string_from_str(&prev));
                }
                return Err(MonorubyErr::typeerr("cannot exclude non Integer end value"));
            } else {
                return Err(MonorubyErr::typeerr("cannot exclude non Integer end value"));
            }
        }

        Ok(end)
    }
}

///
/// ### Range#count
///
/// - count -> Integer | Float::INFINITY
/// - count(item) -> Integer
/// - count {|item| ... } -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/count.html]
#[monoruby_builtin]
fn count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let start = range.start();
    let end = range.end();

    if lfp.block().is_some() {
        // With block: delegate to Enumerable iteration
        if let Some((s, e)) = range.try_fixnum() {
            let bh = lfp.expect_block()?;
            let data = vm.get_block_data(globals, bh)?;
            let mut count: i64 = 0;
            for i in s..e {
                let val = Value::integer(i);
                if vm.invoke_block(globals, &data, &[val])?.as_bool() {
                    count += 1;
                }
            }
            Ok(Value::integer(count))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        // No block, no args
        if start.is_nil() || end.is_nil() {
            return Ok(Value::float(f64::INFINITY));
        }
        // For integer ranges, calculate directly
        if let Some((s, e)) = range.try_fixnum() {
            let count = if e > s { e - s } else { 0 };
            Ok(Value::integer(count))
        } else {
            // For non-integer finite ranges, fall back to iteration
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    }
}

///
/// ### Range#minmax
///
/// - minmax -> [object, object]
/// - minmax {|a, b| ... } -> [object, object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/minmax.html]
#[monoruby_builtin]
fn minmax(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    let start = range.start();
    let end = range.end();

    if start.is_nil() {
        return Err(MonorubyErr::rangeerr(
            "cannot get the minimum of beginless range",
        ));
    }
    if end.is_nil() {
        return Err(MonorubyErr::rangeerr(
            "cannot get the maximum of endless range",
        ));
    }

    if let Some(bh) = lfp.block() {
        // With block: iterate
        if let Some((s, e)) = range.try_fixnum() {
            if e <= s {
                return Ok(Value::array_from_vec(vec![Value::nil(), Value::nil()]));
            }
            let data = vm.get_block_data(globals, bh)?;
            let mut min_val = Value::integer(s);
            let mut max_val = Value::integer(s);
            for i in (s + 1)..e {
                let val = Value::integer(i);
                let cmp_min = vm.invoke_block(globals, &data, &[val, min_val])?;
                let is_less = vm
                    .invoke_method_inner(
                        globals,
                        IdentId::_LT,
                        cmp_min,
                        &[Value::integer(0)],
                        None,
                        None,
                    )?
                    .as_bool();
                if is_less {
                    min_val = val;
                }
                let cmp_max = vm.invoke_block(globals, &data, &[val, max_val])?;
                let is_greater = vm
                    .invoke_method_inner(
                        globals,
                        IdentId::_GT,
                        cmp_max,
                        &[Value::integer(0)],
                        None,
                        None,
                    )?
                    .as_bool();
                if is_greater {
                    max_val = val;
                }
            }
            Ok(Value::array_from_vec(vec![min_val, max_val]))
        } else {
            Err(MonorubyErr::runtimeerr("not supported"))
        }
    } else {
        // No block: compute min and max directly
        // Check for empty range
        let cmp = vm.compare_values_inner(globals, start, end)?;
        match cmp {
            Some(std::cmp::Ordering::Greater) => {
                return Ok(Value::array_from_vec(vec![Value::nil(), Value::nil()]));
            }
            Some(std::cmp::Ordering::Equal) => {
                if range.exclude_end() {
                    return Ok(Value::array_from_vec(vec![Value::nil(), Value::nil()]));
                }
                return Ok(Value::array_from_vec(vec![start, end]));
            }
            _ => {}
        }

        // Get max value
        let max_val = if range.exclude_end() {
            if let Some(i) = end.try_fixnum() {
                Value::integer(i - 1)
            } else if let RV::BigInt(b) = end.unpack() {
                Value::bigint(b - 1)
            } else if let Some(end_str) = end.is_str() {
                if let Some(start_str) = start.is_str() {
                    let mut current = start_str.to_string();
                    let mut prev = current.clone();
                    while current != end_str {
                        prev = current.clone();
                        current = builtins::string::str_next(&current);
                    }
                    Value::string_from_str(&prev)
                } else {
                    return Err(MonorubyErr::typeerr("cannot exclude non Integer end value"));
                }
            } else {
                return Err(MonorubyErr::typeerr("cannot exclude non Integer end value"));
            }
        } else {
            end
        };

        Ok(Value::array_from_vec(vec![start, max_val]))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn range() {
        run_test(
            r##"
          r = Range.new(3,10)
          [r.begin, r.end, r.exclude_end?]
        "##,
        );
        run_test("(1..5).exclude_end?");
        run_test("(1...5).exclude_end?");
        run_test("(1...5).to_a");
        run_test("Range.new(1, 5, true).to_a");
        run_test("(1..10).first(3)");
        run_test("(1..10).last(3)");
        run_test("(1..5).first");
        run_test("(1..5).last");
    }

    #[test]
    fn each() {
        run_test(
            r#"
        a = 0
        (1...5).each do |x|
            a += x
        end
        a
        "#,
        );
        run_test(
            r#"
        a = 0
        (1..5).each do |x|
            a += x
        end
        a
        "#,
        );
        run_test(
            r#"
        a = 0
        (10..5).each do |x|
            a += x
        end
        a
        "#,
        );
        run_test(
            r#"
        a = ''
        ('a'...'z').each do |x|
            a += x
        end
        a
        "#,
        );
    }

    #[test]
    fn map() {
        run_test(
            r#"
        (1...5).map do |x|
            x * 100
        end
        "#,
        );
        run_test(
            r#"
        (1..5).map do |x|
            x + 100
        end
        "#,
        );
        run_test(
            r#"
        (1..1).map do |x|
            x + 100
        end
        "#,
        );
        run_test(
            r#"
        (5..1).map do |x|
            x + 100
        end
        "#,
        );
    }

    #[test]
    fn flat_map() {
        run_test(
            r#"
        (1...5).flat_map do |x|
            [x] * x
        end
        "#,
        );
        run_test(
            r#"
        (1..5).flat_map do |x|
            [x] * x
        end
        "#,
        );
    }

    #[test]
    fn include() {
        let mut test = vec![];
        let nums = [
            "nil",
            "-1000000000000000000000000000000000000000000000000000",
            "-10000",
            "-10000.0",
            "0",
            "10000.0",
            "10000",
            "1000000000000000000000000000000000000000000000000000",
        ];
        for lhs in nums {
            for rhs in nums {
                for val in nums {
                    if lhs == "nil" && rhs == "nil" && val == "nil" {
                        continue;
                    }
                    test.extend_from_slice(&[
                        format!("(({lhs})..({rhs})).include?({val})"),
                        format!("(({lhs})...({rhs})).include?({val})"),
                    ]);
                }
            }
        }
        run_tests(&test);

        run_tests(&[
            // Beginless ranges (nil..end)
            r#"(nil..5).include?(3)"#,
            r#"(nil..5).include?(5)"#,
            r#"(nil..5).include?(6)"#,
            r#"(nil...5).include?(5)"#,
            r#"(nil..5.0).include?(3.0)"#,
            r#"(nil..5).include?(:a)"#,
            // exclude-end beginless
            r#"(nil...5).include?(4)"#,
            r#"(nil...5.0).include?(5.0)"#,
            // beginless with float endpoint and integer value
            r#"(nil..5.0).include?(3)"#,
            r#"(nil..5.0).include?(5)"#,
            // beginless via ===
            r#"(nil..5) === 3"#,
            r#"(nil..5) === 6"#,
            r#"(nil...5) === 5"#,
            // Endless ranges (beg..nil)
            r#"(1..nil).include?(3)"#,
            r#"(1..nil).include?(0)"#,
            r#"(1.0..nil).include?(3.0)"#,
            r#"(1..nil).include?(:a)"#,
            // endless with float endpoint and integer value
            r#"(1.0..nil).include?(2)"#,
            r#"(1.0..nil).include?(0)"#,
            // endless via ===
            r#"(1..nil) === 3"#,
            r#"(1..nil) === 0"#,
            r#"(1.0..nil) === 3.0"#,
            // Both-nil ranges (nil..nil) — numeric values are always included
            r#"(nil..nil).include?(0)"#,
            r#"(nil..nil).include?(42)"#,
            r#"(nil..nil).include?(3.14)"#,
            // (nil..nil).include?(:a) raises TypeError in CRuby; monoruby returns false
            // (nil..nil) === :a  returns true in CRuby (cover? semantics); monoruby returns false
            // These divergences are not tested here.
            r#"(nil..nil) === 0"#,
            // String ranges
            r#"("a".."z").include?("a")"#,
            r#"("a".."z").include?("m")"#,
            r#"("a".."z").include?("z")"#,
            r#"("a"..."z").include?("z")"#,
            r#"("a".."z").include?("A")"#,
            r#"("a".."z").include?(1)"#,
            // string ranges via ===
            r#"("a".."z") === "a""#,
            r#"("a".."z") === "m""#,
            r#"("a".."z") === "z""#,
            r#"("a"..."z") === "z""#,
            r#"("a".."z") === 1"#,
            // member? is an alias for include?
            r#"(1..5).member?(3)"#,
            r#"(1..5).member?(6)"#,
            r#"("a".."z").member?("m")"#,
            r#"(nil..5).member?(3)"#,
            r#"(1..nil).member?(3)"#,
        ]);
    }

    /// Test that beginless/endless ranges with non-numeric, non-nil endpoints raise TypeError.
    /// Uses run_test_error which does not require an external CRuby binary.
    #[test]
    fn include_type_errors() {
        // Beginless range with string endpoint
        run_test_error(r#"(nil.."z").include?(1)"#);
        run_test_error(r#"(nil.."z").include?(1.0)"#);
        // Endless range with string endpoint
        run_test_error(r#"("a"..nil).include?(1)"#);
        run_test_error(r#"("a"..nil).include?(1.0)"#);
        run_test_error(r#"(nil..nil).include?(nil)"#);
    }

    #[test]
    fn all() {
        run_test(
            r#"
        (1...5).all? do |x|
            x > 0
        end
        "#,
        );
        run_test(
            r#"
        (1...5).all? do |x|
            x != 5
        end
        "#,
        );
    }

    #[test]
    fn bsearch() {
        run_test(
            r##"
        res = []
        ary = [0, 4, 7, 10, 12]
        res << (0...ary.size).bsearch {|i| ary[i] >= 4 } # => 1
        res << (0...ary.size).bsearch {|i| ary[i] >= 6 } # => 2
        res << (0...ary.size).bsearch {|i| ary[i] >= 8 } # => 3
        res << (0...ary.size).bsearch {|i| ary[i] >= 100 } # => nil

        ary = [0, 100, 100, 100, 200]
        res << (0..4).bsearch {|i| 100 - ary[i] } # => 1, 2 or 3
        res << (0..4).bsearch {|i| 300 - ary[i] } # => nil
        res << (0..4).bsearch {|i|  50 - ary[i] } # => nil

        res << (10..4).bsearch {|i| 100 - ary[i] }
        res << (10..4).bsearch {|i| 300 - ary[i] }
        res << (10..4).bsearch {|i|  50 - ary[i] }
        
        res
        "##,
        );
    }

    #[test]
    fn reject() {
        run_test(
            r##"
        (1..6).reject {|i| i % 2 == 0 }
        "##,
        );
        run_test(
            r##"
        (1...6).reject {|i| i % 2 == 0 }
        "##,
        );
        run_test(
            r##"
        (10..6).reject {|i| i % 2 == 0 }
        "##,
        );
    }

    #[test]
    fn min() {
        run_test("(1..5).min");
        run_test("(1...5).min");
        run_test("(5..1).min");
        run_test("(1..1).min");
        run_test("(1...1).min");
        run_test("('a'..'z').min");
    }

    #[test]
    fn min_with_block() {
        run_test("(1..5).min {|a, b| b <=> a }");
        run_test("(1...5).min {|a, b| b <=> a }");
    }

    #[test]
    fn max() {
        run_test("(1..5).max");
        run_test("(1...5).max");
        run_test("(5..1).max");
        run_test("(1..1).max");
        run_test("(1...1).max");
        run_test("('a'..'z').max");
    }

    #[test]
    fn max_with_block() {
        run_test("(1..5).max {|a, b| b <=> a }");
        run_test("(1...5).max {|a, b| b <=> a }");
    }

    #[test]
    fn count() {
        run_test("(1..5).count");
        run_test("(1...5).count");
        run_test("(5..1).count");
        run_test("(1..1).count");
        run_test("(1...1).count");
    }

    #[test]
    fn minmax() {
        run_test("(1..5).minmax");
        run_test("(1...5).minmax");
        run_test("(5..1).minmax");
        run_test("(1..1).minmax");
        run_test("(1...1).minmax");
        run_test("('a'..'z').minmax");
    }

    #[test]
    fn minmax_with_block() {
        run_test("(1..5).minmax {|a, b| b <=> a }");
    }

    #[test]
    fn min_errors() {
        // beginless range
        run_test_error("(..5).min");
        // beginless range with block
        run_test_error("(..5).min {|a, b| a <=> b }");
        // endless range with block
        run_test_error("(1..).min {|a, b| a <=> b }");
    }

    #[test]
    fn min_endless() {
        // endless range without block returns start
        run_test("(1..).min");
    }

    #[test]
    fn max_errors() {
        // endless range
        run_test_error("(1..).max");
        // beginless range with block
        run_test_error("(..5).max {|a, b| a <=> b }");
        // exclusive range with non-integer end
        run_test_error("(1.0...3.0).max");
    }

    #[test]
    fn max_beginless() {
        // beginless range without block returns end
        run_test("(..5).max");
    }

    #[test]
    fn count_endless() {
        // endless/beginless ranges return Float::INFINITY
        // CRuby would hang on these, so we just verify no crash
        run_test_no_result_check("(1..).count");
        run_test_no_result_check("(..5).count");
    }

    #[test]
    fn minmax_errors() {
        // beginless range
        run_test_error("(..5).minmax");
        // endless range
        run_test_error("(1..).minmax");
        // exclusive range with non-integer end
        run_test_error("(1.0...3.0).minmax");
    }

    #[test]
    fn cover() {
        run_tests(&[
            "(1..10).cover?(5)",
            "(1..10).cover?(0)",
            "(1..10).cover?(11)",
            "(1...10).cover?(10)",
            "(1...10).cover?(9)",
            "(1..10).cover?(1..5)",
            "(1..10).cover?(0..5)",
            "(1..10).cover?(5..15)",
            "('a'..'z').cover?('m')",
        ]);
    }
}
