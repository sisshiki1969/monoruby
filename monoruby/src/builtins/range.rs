use super::*;

//
// Range class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Range", RANGE_CLASS, ObjTy::RANGE);
    globals.define_builtin_class_func_with(RANGE_CLASS, "new", range_new, 2, 2, false);
    globals.define_builtin_inline_funcs(
        RANGE_CLASS,
        "begin",
        &["first"],
        begin,
        Box::new(range_begin),
        0,
    );
    globals.define_builtin_inline_funcs(RANGE_CLASS, "end", &["last"], end, Box::new(range_end), 0);
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
}

///
/// ### Range.new
///
/// - new(first, last, exclude_end = false) -> Range
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/s/new.html]
#[monoruby_builtin]
fn range_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    globals.generate_range(lfp.arg(0), lfp.arg(1), false)
}

///
/// ### Range#begin
///
/// - begin -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().as_range().start())
}

fn range_begin(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        let start = range.start();
        if start.is_frozen_literal() {
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
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val().as_range().end())
}

fn range_end(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        let end = range.end();
        if end.is_frozen_literal() {
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

/// Range#exclude_end?
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Range/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_range().exclude_end()))
}

fn range_exclude_end(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    if let Some(range) = state.is_range_literal(callsite.recv) {
        let exclude_end = range.exclude_end();
        state.def_C(dst, Value::bool(exclude_end));
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
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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

        // ── Both endpoints are Float ────────────────────────────────────────
        (RV::Float(s), RV::Float(e)) => Ok(match val.unpack() {
            RV::Fixnum(v) => above(Some(s), v as f64) && below(v as f64, Some(e), excl),
            RV::BigInt(v) => {
                let v = v.to_f64().unwrap_or(f64::NAN);
                above(Some(s), v) && below(v, Some(e), excl)
            }
            RV::Float(v) => above(Some(s), v) && below(v, Some(e), excl),
            _ => false,
        }),

        // ── Mixed Fixnum / Float ────────────────────────────────────────────
        (RV::Fixnum(s), RV::Float(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s as f64), v) && below(v, Some(e), excl))
        }
        (RV::Float(s), RV::Fixnum(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v) && below(v, Some(e as f64), excl))
        }

        // ── Beginless numeric (nil..end) ────────────────────────────────────
        (RV::Nil, RV::Fixnum(e)) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(below(v, Some(e as f64), excl))
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
        (RV::Float(s), RV::Nil) => {
            let Some(v) = to_f64(val) else {
                return Ok(false);
            };
            Ok(above(Some(s), v))
        }

        // ── Both nil (beginless-endless range) ──────────────────────────────
        // linear objects (Fixnum, Float, BigInt) are always included.
        (RV::Nil, RV::Nil) => Ok(matches!(
            val.unpack(),
            RV::Fixnum(_) | RV::Float(_) | RV::BigInt(_)
        )),

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
fn include_(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn teq(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn toa(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let range = self_.as_range();
    if let Some((start, end)) = range.try_fixnum() {
        let vec = (start..end).map(Value::integer).collect();
        Ok(Value::array_from_vec(vec))
    } else if let Some(start) = range.start().is_str()
        && let Some(end) = range.end().is_str()
    {
        let mut start = start.to_string();
        let mut v = vec![];
        while start != end {
            v.push(Value::string_from_str(&start));
            start = builtins::string::str_next(&start);
        }
        if !range.exclude_end() {
            v.push(Value::string_from_str(&start));
        }
        Ok(Value::array_from_vec(v))
    } else {
        Err(MonorubyErr::runtimeerr("not supported"))
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
        run_test(r#"(1...5).include?(-1)"#);
        run_test(r#"(1...5).include?(-1.7)"#);
        run_test(r#"(1...5).include?(1)"#);
        run_test(r#"(1...5).include?(0.9)"#);
        run_test(r#"(1...5).include?(1.1)"#);
        run_test(r#"(1...5).include?(3)"#);
        run_test(r#"(1...5).include?(3.9)"#);
        run_test(r#"(1...5).include?(5)"#);
        run_test(r#"(1...5).include?(5.0)"#);
        run_test(r#"(1...5).include?(:a)"#);

        run_test(r#"(1..5).include?(-1)"#);
        run_test(r#"(1..5).include?(-1.7)"#);
        run_test(r#"(1..5).include?(1)"#);
        run_test(r#"(1..5).include?(0.9)"#);
        run_test(r#"(1..5).include?(1.1)"#);
        run_test(r#"(1..5).include?(3)"#);
        run_test(r#"(1..5).include?(3.9)"#);
        run_test(r#"(1..5).include?(5)"#);
        run_test(r#"(1..5).include?(5.0)"#);
        run_test(r#"(1..5).include?(:a)"#);

        run_test(r#"(1...5) === (-1)"#);
        run_test(r#"(1...5) === (-1.7)"#);
        run_test(r#"(1...5) === (1)"#);
        run_test(r#"(1...5) === (0.9)"#);
        run_test(r#"(1...5) === (1.1)"#);
        run_test(r#"(1...5) === (3)"#);
        run_test(r#"(1...5) === (3.9)"#);
        run_test(r#"(1...5) === (5)"#);
        run_test(r#"(1...5) === (5.0)"#);
        run_test(r#"(1...5) === (:a)"#);

        run_test(r#"(1..5).=== (-1)"#);
        run_test(r#"(1..5).=== (-1.7)"#);
        run_test(r#"(1..5).=== (1)"#);
        run_test(r#"(1..5).=== (0.9)"#);
        run_test(r#"(1..5).=== (1.1)"#);
        run_test(r#"(1..5).=== (3)"#);
        run_test(r#"(1..5).=== (3.9)"#);
        run_test(r#"(1..5).=== (5)"#);
        run_test(r#"(1..5).=== (5.0)"#);
        run_test(r#"(1..5).=== (:a)"#);

        // Beginless ranges (nil..end)
        run_test(r#"(nil..5).include?(3)"#);
        run_test(r#"(nil..5).include?(5)"#);
        run_test(r#"(nil..5).include?(6)"#);
        run_test(r#"(nil...5).include?(5)"#);
        run_test(r#"(nil..5.0).include?(3.0)"#);
        run_test(r#"(nil..5).include?(:a)"#);

        // Endless ranges (beg..nil)
        run_test(r#"(1..nil).include?(3)"#);
        run_test(r#"(1..nil).include?(0)"#);
        run_test(r#"(1.0..nil).include?(3.0)"#);
        run_test(r#"(1..nil).include?(:a)"#);

        // String ranges
        run_test(r#"("a".."z").include?("a")"#);
        run_test(r#"("a".."z").include?("m")"#);
        run_test(r#"("a".."z").include?("z")"#);
        run_test(r#"("a"..."z").include?("z")"#);
        run_test(r#"("a".."z").include?("A")"#);
        run_test(r#"("a".."z").include?(1)"#);

        // member? is an alias for include?
        run_test(r#"(1..5).member?(3)"#);
        run_test(r#"("a".."z").member?("m")"#);
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
}
