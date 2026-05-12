use super::*;

use num::{BigInt, ToPrimitive, Zero};

//
// Enumerator::ArithmeticSequence — native (begin, end, step, exclude_end?)
// holder returned by `Numeric#step`, `Range#step`, and `Range#%` when
// invoked without a block. Storage lives in `ObjTy::ARITHMETIC_SEQUENCE`
// (`ArithmeticSequenceInner`). Every behavioural method below operates
// on those fields directly: no Ruby wrappers in between.
//
// Type dispatch (each / size / first / last / `[]`): Fixnum and Float
// fast paths inline; user-defined Numerics fall through to
// `vm.invoke_method_inner` (CRuby-shape generic path).
//

pub(super) fn init(globals: &mut Globals) {
    let object_class = globals.object_class();
    globals.define_builtin_class(
        "ArithmeticSequence",
        ARITHMETIC_SEQUENCE_CLASS,
        object_class,
        ENUMERATOR_CLASS,
        ObjTy::ARITHMETIC_SEQUENCE,
    );
    globals.define_builtin_class_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "__build",
        arithmetic_sequence_build,
        4,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "begin",
        begin,
        Box::new(as_begin_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "end",
        end,
        Box::new(as_end_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "step",
        step,
        Box::new(as_step_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "exclude_end?",
        exclude_end,
        Box::new(as_exclude_end_inline),
        0,
    );
    // `each` is intentionally NOT registered here — it's defined in
    // `builtins/arithmetic_sequence.rb` (Ruby) so monoruby's JIT can
    // inline the block dispatch directly into the loop. See benchmark
    // discussion in PR comments.
    globals.define_builtin_funcs(ARITHMETIC_SEQUENCE_CLASS, "size", &["length"], size, 0);
    globals.define_builtin_func_with(ARITHMETIC_SEQUENCE_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_func_with(ARITHMETIC_SEQUENCE_CLASS, "last", last, 0, 1, false);
    globals.define_builtin_func(ARITHMETIC_SEQUENCE_CLASS, "[]", index_op, 1);
}

///
/// ### Enumerator::ArithmeticSequence.__build
///
/// monoruby-internal allocator used by `Numeric#step` / `Range#step` /
/// `Range#%`. Stamps the native (begin, end, step, exclude_end?) tuple
/// directly into a fresh `RValue` — bypasses `Enumerator#new`'s block
/// requirement.
#[monoruby_builtin]
fn arithmetic_sequence_build(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let begin = lfp.arg(0);
    let end = lfp.arg(1);
    let step = lfp.arg(2);
    let exclude_end = lfp.arg(3).as_bool();
    Ok(Value::arithmetic_sequence(begin, end, step, exclude_end))
}

///
/// ### Enumerator::ArithmeticSequence#begin
///
/// - begin -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/begin.html]
#[monoruby_builtin]
fn begin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_arithmetic_sequence_inner().begin())
}

fn as_begin_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    inline_field_load(state, ir, store, callid, crate::rvalue::AS_BEGIN_OFFSET)
}

///
/// ### Enumerator::ArithmeticSequence#end
///
/// - end -> Numeric | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/end.html]
#[monoruby_builtin]
fn end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_arithmetic_sequence_inner().end())
}

fn as_end_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    inline_field_load(state, ir, store, callid, crate::rvalue::AS_END_OFFSET)
}

///
/// ### Enumerator::ArithmeticSequence#step
///
/// - step -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/step.html]
#[monoruby_builtin]
fn step(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val().as_arithmetic_sequence_inner().step())
}

fn as_step_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    inline_field_load(state, ir, store, callid, crate::rvalue::AS_STEP_OFFSET)
}

///
/// ### Enumerator::ArithmeticSequence#exclude_end?
///
/// - exclude_end? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/exclude_end=3f.html]
#[monoruby_builtin]
fn exclude_end(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(
        lfp.self_val().as_arithmetic_sequence_inner().exclude_end(),
    ))
}

fn as_exclude_end_inline(
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
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _, _| {
        monoasm! { &mut r#gen.jit,
            movl rax, [rdi + (crate::rvalue::AS_EXCLUDE_END_OFFSET as i32)];
            shlq rax, 3;
            orq  rax, (FALSE_VALUE);
        }
    });
    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

/// Shared inliner for the Fixnum/Value field readers
/// (`begin` / `end` / `step`). Loads a 64-bit `Value` from the given
/// offset within the receiver's `RValue`. AS has no source-level literal
/// form, so unlike Range we don't fold against a literal here.
fn inline_field_load(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    store: &Store,
    callid: CallSiteId,
    offset: usize,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, [rdi + (offset as i32)];
        }
    });
    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

// ─── Type-dispatched helpers ────────────────────────────────────────

/// True iff `v` is numerically zero (Fixnum/BigInt/Float).
fn value_is_zero(v: Value) -> bool {
    match v.unpack() {
        RV::Fixnum(0) => true,
        RV::Float(f) => f == 0.0,
        RV::BigInt(b) => b.is_zero(),
        _ => false,
    }
}

/// Check `v.is_a?(Numeric)` natively first; method dispatch fallback for
/// user-defined classes.
fn value_is_numeric(globals: &Globals, v: Value) -> bool {
    match v.unpack() {
        RV::Fixnum(_) | RV::BigInt(_) | RV::Float(_) | RV::Complex(_) => true,
        _ => v.is_kind_of(&globals.store, NUMERIC_CLASS),
    }
}

/// Best-effort conversion to f64. None for non-numeric / Complex.
fn to_f64(v: Value) -> Option<f64> {
    match v.unpack() {
        RV::Fixnum(i) => Some(i as f64),
        RV::Float(f) => Some(f),
        RV::BigInt(b) => b.to_f64(),
        _ => None,
    }
}

/// Result of counting terms in a bounded AS.
#[derive(Debug)]
enum Count {
    /// Concrete count, fits in i64 (used as Array size + index domain).
    Finite(i64),
    /// `e == ±Infinity` ⇒ unbounded in step's direction.
    Infinite,
}

/// Compute `(b + i * s)` natively for the index `i` (signed). Promotes
/// to BigInt on Fixnum overflow.
fn term_at(b: Value, i: i64, s: Value) -> Option<Value> {
    match (b.unpack(), s.unpack()) {
        (RV::Fixnum(b_i), RV::Fixnum(s_i)) => {
            let prod = (i as i128).checked_mul(s_i as i128)?;
            let sum = (b_i as i128).checked_add(prod)?;
            Some(if let Ok(v) = i64::try_from(sum) {
                Value::integer(v)
            } else {
                Value::bigint(BigInt::from(sum))
            })
        }
        // Float-involved: arithmetic in f64.
        _ => {
            let b_f = to_f64(b)?;
            let s_f = to_f64(s)?;
            Some(Value::float(b_f + (i as f64) * s_f))
        }
    }
}

/// Compute term at index `i` using Ruby's `+` / `*` for non-trivial
/// numeric types (BigInt with i64-overflowing products, Rational,
/// user-defined Numerics). Slower than `term_at` but type-correct.
fn term_at_generic(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    i: i64,
    s: Value,
) -> Result<Value> {
    let i_val = Value::integer(i);
    let prod = vm.invoke_method_inner(globals, IdentId::_MUL, i_val, &[s], None, None)?;
    vm.invoke_method_inner(globals, IdentId::_ADD, b, &[prod], None, None)
}

/// Closed-form count formula. Caller must have validated:
///   * step is Numeric and non-zero
///   * begin is non-nil
/// `e` is the end (may be `Float::INFINITY` / `-Float::INFINITY`).
fn finite_count(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    e: Value,
    s: Value,
    excl: bool,
) -> Result<Count> {
    // ── All-Fixnum fast path ────────────────────────────────────
    if let (Some(b_i), Some(e_i), Some(s_i)) = (b.try_fixnum(), e.try_fixnum(), s.try_fixnum()) {
        return Ok(fixnum_count(b_i, e_i, s_i, excl));
    }

    // ── Float-involved fast path ────────────────────────────────
    // (Catches the e == Float::INFINITY case before generic dispatch.)
    if let (Some(b_f), Some(e_f), Some(s_f)) = (to_f64(b), to_f64(e), to_f64(s)) {
        return Ok(float_count(b_f, e_f, s_f, excl));
    }

    // ── Generic slow path (BigInt overflow, Rational, etc.) ────
    generic_count(vm, globals, b, e, s, excl)
}

fn fixnum_count(b: i64, e: i64, s: i64, excl: bool) -> Count {
    let diff = match (e as i128).checked_sub(b as i128) {
        Some(d) => d,
        None => return Count::Finite(0), // shouldn't happen with i64 inputs
    };
    if (s > 0 && diff < 0) || (s < 0 && diff > 0) {
        return Count::Finite(0);
    }
    let s128 = s as i128;
    let mut n_int = diff / s128; // same sign ⇒ trunc == floor
    if excl {
        // last_val = b + n_int * s
        let last_val = b as i128 + n_int * s128;
        let overshoot = if s > 0 {
            last_val >= e as i128
        } else {
            last_val <= e as i128
        };
        if overshoot {
            n_int -= 1;
        }
    }
    if n_int < 0 {
        return Count::Finite(0);
    }
    // n_int + 1 is the count; clamp to i64 max (unrealistically large).
    let count = n_int.saturating_add(1);
    if count > i64::MAX as i128 {
        Count::Finite(i64::MAX)
    } else {
        Count::Finite(count as i64)
    }
}

fn float_count(b: f64, e: f64, s: f64, excl: bool) -> Count {
    if s.is_infinite() {
        let ok = if s > 0.0 {
            if excl { b < e } else { b <= e }
        } else if excl {
            b > e
        } else {
            b >= e
        };
        return Count::Finite(if ok { 1 } else { 0 });
    }
    let diff = e - b;
    if (s > 0.0 && diff < 0.0) || (s < 0.0 && diff > 0.0) {
        return Count::Finite(0);
    }
    let nf = diff / s;
    if nf.is_nan() || nf < 0.0 {
        return Count::Finite(0);
    }
    if nf.is_infinite() {
        return Count::Infinite;
    }
    let mut n_int = nf.floor() as i64;
    if excl {
        let last_v = b + (n_int as f64) * s;
        let overshoot = if s > 0.0 { last_v >= e } else { last_v <= e };
        if overshoot {
            n_int -= 1;
        }
    }
    if n_int < 0 {
        return Count::Finite(0);
    }
    Count::Finite(n_int.saturating_add(1))
}

/// Generic slow path — dispatches arithmetic through Ruby methods.
fn generic_count(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    e: Value,
    s: Value,
    excl: bool,
) -> Result<Count> {
    let zero = Value::integer(0);
    // diff = e - b
    let diff = vm.invoke_method_inner(globals, IdentId::_SUB, e, &[b], None, None)?;
    let s_pos = vm
        .invoke_method_inner(globals, IdentId::_GT, s, &[zero], None, None)?
        .as_bool();
    let diff_neg = vm
        .invoke_method_inner(globals, IdentId::_LT, diff, &[zero], None, None)?
        .as_bool();
    let diff_pos = vm
        .invoke_method_inner(globals, IdentId::_GT, diff, &[zero], None, None)?
        .as_bool();
    if (s_pos && diff_neg) || (!s_pos && diff_pos) {
        return Ok(Count::Finite(0));
    }
    // nf = diff.to_f / s.to_f
    let diff_f = invoke_to_f(vm, globals, diff)?;
    let s_f = invoke_to_f(vm, globals, s)?;
    let nf = diff_f / s_f;
    if nf.is_nan() || nf < 0.0 {
        return Ok(Count::Finite(0));
    }
    if nf.is_infinite() {
        return Ok(Count::Infinite);
    }
    let mut n_int = nf.floor() as i64;
    if excl {
        let last_v = term_at_generic(vm, globals, b, n_int, s)?;
        let cmp_op = if s_pos { IdentId::_GE } else { IdentId::_LE };
        let overshoot = vm
            .invoke_method_inner(globals, cmp_op, last_v, &[e], None, None)?
            .as_bool();
        if overshoot {
            n_int -= 1;
        }
    }
    if n_int < 0 {
        return Ok(Count::Finite(0));
    }
    Ok(Count::Finite(n_int.saturating_add(1)))
}

fn invoke_to_f(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<f64> {
    let res = vm.invoke_method_inner(globals, IdentId::get_id("to_f"), v, &[], None, None)?;
    res.try_float()
        .ok_or_else(|| MonorubyErr::typeerr("to_f did not return a Float"))
}

// ─── size ──────────────────────────────────────────────────────────

///
/// ### Enumerator::ArithmeticSequence#size
///
/// - size -> Integer | Float::INFINITY
/// - length -> Integer | Float::INFINITY
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/size.html]
#[monoruby_builtin]
fn size(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (b, e, s, excl) = unpack_as(lfp.self_val());
    if !value_is_numeric(globals, s) {
        return Err(MonorubyErr::argumenterr("step must be numeric"));
    }
    if value_is_zero(s) {
        return Err(MonorubyErr::argumenterr("step can't be 0"));
    }
    if b.is_nil() || e.is_nil() {
        return Ok(Value::float(f64::INFINITY));
    }
    match finite_count(vm, globals, b, e, s, excl)? {
        Count::Finite(n) => Ok(Value::integer(n)),
        Count::Infinite => Ok(Value::float(f64::INFINITY)),
    }
}

/// Extract the native (begin, end, step, exclude_end?) tuple. Owning
/// the receiver via `let` is necessary because `as_arithmetic_sequence_inner`
/// borrows from the RValue — pulling fields out by value here lets
/// callers freely take `&mut` to vm/globals afterwards.
fn unpack_as(recv: Value) -> (Value, Value, Value, bool) {
    let inner = recv.as_arithmetic_sequence_inner();
    (
        inner.begin(),
        inner.end(),
        inner.step(),
        inner.exclude_end(),
    )
}

// ─── each (Ruby) ───────────────────────────────────────────────────
//
// `each` is registered in `builtins/arithmetic_sequence.rb` rather
// than here — benchmarks showed the JIT'd Ruby loop is on par with
// the Rust + `invoke_block` path. The native implementation below is
// kept for reference / future use.

/*///
/// ### Enumerator::ArithmeticSequence#each
///
/// - each {|elem| ... } -> self
/// - each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/each.html]
#[allow(dead_code)]
#[coverage(off)]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let recv = lfp.self_val();
    let inner = recv.as_arithmetic_sequence_inner();
    let b = inner.begin();
    let e = inner.end();
    let s = inner.step();
    let excl = inner.exclude_end();

    let Some(bh) = lfp.block() else {
        return vm.generate_enumerator(IdentId::EACH, recv, vec![], pc);
    };

    if value_is_zero(s) {
        return Err(MonorubyErr::typeerr("step can't be 0"));
    }
    if b.is_nil() {
        return Err(MonorubyErr::argumenterr(
            "#each for beginless arithmetic sequences is meaningless",
        ));
    }

    let data = vm.get_block_data(globals, bh)?;

    // Fixnum fast path: b, s both Fixnum AND end is nil or Fixnum.
    if let (Some(b_i), Some(s_i)) = (b.try_fixnum(), s.try_fixnum()) {
        if e.is_nil() {
            return fixnum_each_endless(vm, globals, &data, b_i, s_i, recv);
        }
        if let Some(e_i) = e.try_fixnum() {
            return fixnum_each_finite(vm, globals, &data, b_i, e_i, s_i, excl, recv);
        }
    }

    // Float fast path.
    if let (Some(b_f), Some(s_f)) = (to_f64(b), to_f64(s))
        && (e.is_nil() || to_f64(e).is_some())
    {
        let e_opt = if e.is_nil() { None } else { to_f64(e) };
        return float_each(vm, globals, &data, b_f, e_opt, s_f, excl, recv);
    }

    generic_each(vm, globals, &data, b, e, s, excl, recv)
}

#[allow(dead_code)]
fn fixnum_each_finite(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    b: i64,
    e: i64,
    s: i64,
    excl: bool,
    recv: Value,
) -> Result<Value> {
    let Count::Finite(count) = fixnum_count(b, e, s, excl) else {
        unreachable!()
    };
    let mut cur = b;
    let mut remaining = count;
    while remaining > 0 {
        vm.invoke_block(globals, data, &[Value::integer(cur)])?;
        remaining -= 1;
        if remaining == 0 {
            break;
        }
        match cur.checked_add(s) {
            Some(n) => cur = n,
            None => break,
        }
    }
    Ok(recv)
}*/

/*#[allow(dead_code)]
fn fixnum_each_endless(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    b: i64,
    s: i64,
    recv: Value,
) -> Result<Value> {
    let mut cur = b;
    loop {
        vm.invoke_block(globals, data, &[Value::integer(cur)])?;
        match cur.checked_add(s) {
            Some(n) => cur = n,
            None => {
                // Promote to BigInt and continue via generic path.
                let cur_v = Value::bigint(BigInt::from(cur) + BigInt::from(s));
                let s_v = Value::integer(s);
                return generic_each_endless(vm, globals, data, cur_v, s_v, recv);
            }
        }
    }
}*/

/*#[allow(dead_code)]
fn float_each(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    b: f64,
    e: Option<f64>,
    s: f64,
    excl: bool,
    recv: Value,
) -> Result<Value> {
    if s == 0.0 {
        return Err(MonorubyErr::typeerr("step can't be 0"));
    }
    let endless_loop = |vm: &mut Executor, globals: &mut Globals| -> Result<()> {
        let mut i: i64 = 0;
        loop {
            vm.invoke_block(globals, data, &[Value::float(b + (i as f64) * s)])?;
            i += 1;
        }
    };
    match e {
        None => {
            endless_loop(vm, globals)?;
            Ok(recv)
        }
        Some(e) => {
            let cnt = float_count(b, e, s, excl);
            match cnt {
                Count::Finite(n) => {
                    let mut i: i64 = 0;
                    while i < n {
                        vm.invoke_block(globals, data, &[Value::float(b + (i as f64) * s)])?;
                        i += 1;
                    }
                    Ok(recv)
                }
                Count::Infinite => {
                    endless_loop(vm, globals)?;
                    Ok(recv)
                }
            }
        }
    }
}*/

/*#[allow(dead_code)]
fn generic_each(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    b: Value,
    e: Value,
    s: Value,
    excl: bool,
    recv: Value,
) -> Result<Value> {
    if e.is_nil() {
        return generic_each_endless(vm, globals, data, b, s, recv);
    }
    let s_pos = vm
        .invoke_method_inner(globals, IdentId::_GT, s, &[Value::integer(0)], None, None)?
        .as_bool();
    let cmp_op = if s_pos {
        if excl { IdentId::_LT } else { IdentId::_LE }
    } else if excl {
        IdentId::_GT
    } else {
        IdentId::_GE
    };
    let mut cur = b;
    loop {
        let ok = vm
            .invoke_method_inner(globals, cmp_op, cur, &[e], None, None)?
            .as_bool();
        if !ok {
            break;
        }
        vm.invoke_block(globals, data, &[cur])?;
        cur = vm.invoke_method_inner(globals, IdentId::_ADD, cur, &[s], None, None)?;
    }
    Ok(recv)
}*/

/*#[allow(dead_code)]
fn generic_each_endless(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    b: Value,
    s: Value,
    _recv: Value,
) -> Result<Value> {
    let mut cur = b;
    loop {
        vm.invoke_block(globals, data, &[cur])?;
        cur = vm.invoke_method_inner(globals, IdentId::_ADD, cur, &[s], None, None)?;
    }
}*/

///
/// ### Enumerator::ArithmeticSequence#first
///
/// - first -> Numeric | nil
/// - first(n) -> [Numeric]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/first.html]
#[monoruby_builtin]
fn first(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (b, e, s, excl) = unpack_as(lfp.self_val());
    let arg = lfp.try_arg(0);
    let Some(n_arg) = arg else {
        // No arg: just return begin (matches Enumerator#first for AS).
        return Ok(b);
    };
    let n = n_arg.coerce_to_int_i64(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("negative array size"));
    }
    if n == 0 {
        return Ok(Value::array_empty());
    }
    if value_is_zero(s) {
        return Err(MonorubyErr::typeerr("step can't be 0"));
    }
    if b.is_nil() {
        return Err(MonorubyErr::argumenterr(
            "#each for beginless arithmetic sequences is meaningless",
        ));
    }
    let take = if e.is_nil() {
        n
    } else {
        match finite_count(vm, globals, b, e, s, excl)? {
            Count::Infinite => n,
            Count::Finite(c) => std::cmp::min(n, c),
        }
    };
    build_terms(vm, globals, b, s, 0, take)
}

// ─── last ──────────────────────────────────────────────────────────

///
/// ### Enumerator::ArithmeticSequence#last
///
/// - last -> Numeric | nil
/// - last(n) -> [Numeric]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/last.html]
#[monoruby_builtin]
fn last(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (b, e, s, excl) = unpack_as(lfp.self_val());
    if value_is_zero(s) {
        return Err(MonorubyErr::typeerr("step can't be 0"));
    }
    if e.is_nil() {
        return Err(MonorubyErr::rangeerr(
            "cannot get the last element of endless arithmetic sequence",
        ));
    }
    if b.is_nil() {
        // CRuby raises TypeError from internal `nil - end` arithmetic.
        return Err(MonorubyErr::typeerr("nil can't be coerced into Integer"));
    }
    let count = match finite_count(vm, globals, b, e, s, excl)? {
        Count::Finite(c) => c,
        Count::Infinite => {
            return Err(MonorubyErr::rangeerr(
                "cannot get the last element of endless arithmetic sequence",
            ));
        }
    };
    let arg = lfp.try_arg(0);
    match arg {
        None => {
            if count == 0 {
                return Ok(Value::nil());
            }
            term_or_generic(vm, globals, b, count - 1, s)
        }
        Some(n_arg) => {
            let n = n_arg.coerce_to_int_i64(vm, globals)?;
            if n < 0 {
                return Err(MonorubyErr::argumenterr("negative array size"));
            }
            if count == 0 || n == 0 {
                return Ok(Value::array_empty());
            }
            let n = std::cmp::min(n, count);
            let base = count - n;
            build_terms(vm, globals, b, s, base, n)
        }
    }
}

/// Build an Array of `n` terms starting at index `start_idx`: term[i]
/// = b + (start_idx + i) * s for i in 0..n.
fn build_terms(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    s: Value,
    start_idx: i64,
    n: i64,
) -> Result<Value> {
    let mut vec = Vec::with_capacity(n as usize);
    for i in 0..n {
        vec.push(term_or_generic(vm, globals, b, start_idx + i, s)?);
    }
    Ok(Value::array_from_vec(vec))
}

/// `b + idx * s`: native fast path for Fixnum/Float; generic
/// method-dispatch for everything else.
fn term_or_generic(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    idx: i64,
    s: Value,
) -> Result<Value> {
    if let Some(v) = term_at(b, idx, s) {
        return Ok(v);
    }
    term_at_generic(vm, globals, b, idx, s)
}

// ─── [] (Array stride-aware index) ─────────────────────────────────

///
/// ### Enumerator::ArithmeticSequence#[]
///
/// - self[ary] -> Array
///
/// `aseq[ary]` extracts the slice of `ary` whose indices are the terms
/// of this sequence. Mirrors CRuby's `rb_ary_subseq_step`: integer
/// indices only (the caller is expected to be passing an AS built for
/// slicing — `Numeric#step` / `Range#step` / `Range#%`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/=5b=5d.html]
#[monoruby_builtin]
fn index_op(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (raw_b, raw_e, s_val, excl) = unpack_as(lfp.self_val());

    let arr_v = lfp.arg(0);
    let arr = arr_v.as_array();
    let len = arr.len() as i64;

    // `s == 0` ⇒ empty result (matches existing Ruby behaviour).
    if value_is_zero(s_val) {
        return Ok(Value::array_empty());
    }
    let s = s_val
        .try_fixnum()
        .ok_or_else(|| MonorubyErr::typeerr("step must be an Integer for Array indexing"))?;

    // `raw_b` and `raw_e` must be Integer or nil. Anything else is a
    // misuse — match CRuby's strict integer-indexing assumption.
    fn coerce_idx(v: Value) -> Option<Option<i64>> {
        if v.is_nil() {
            Some(None)
        } else {
            v.try_fixnum().map(Some)
        }
    }
    let rb = coerce_idx(raw_b)
        .ok_or_else(|| MonorubyErr::typeerr("begin must be an Integer for Array indexing"))?;
    let re = coerce_idx(raw_e)
        .ok_or_else(|| MonorubyErr::typeerr("end must be an Integer for Array indexing"))?;

    if s > 0 {
        // ── positive step ────────────────────────────────────────
        let mut b = rb.unwrap_or(0);
        if b < 0 {
            b += len;
        }
        if let Some(orig_b) = rb {
            let orig_b_after = if orig_b < 0 { orig_b + len } else { orig_b };
            if orig_b_after > len {
                if s > 1 {
                    return Err(MonorubyErr::rangeerr(format_oob(raw_b, raw_e, s_val)));
                }
                return Ok(Value::nil());
            }
        }
        if b == len {
            return Ok(Value::array_empty());
        }
        if b < 0 {
            return Ok(Value::nil());
        }

        let (e, end_is_term) = match re {
            None => (len - 1, false),
            Some(orig_e) => {
                let mut e_res = orig_e;
                if e_res < 0 {
                    e_res += len;
                }
                let end_is_term = !excl && b >= 0 && (e_res - b).rem_euclid(s) == 0;
                if s > 1 && end_is_term && e_res >= len {
                    return Err(MonorubyErr::rangeerr(format_oob(raw_b, raw_e, s_val)));
                }
                let e = if excl { e_res - 1 } else { e_res };
                (e, end_is_term)
            }
        };
        let _ = end_is_term; // kept for parity with the Ruby comments
        let e = if e >= len { len - 1 } else { e };
        if e < b {
            return Ok(Value::array_empty());
        }
        // Closed-form capacity: (e - b) / s + 1 terms.
        let cap = ((e - b) / s) as usize + 1;
        let mut result = Vec::with_capacity(cap);
        let slice: &[Value] = &arr;
        let mut i = b as usize;
        let e = e as usize;
        let step = s as usize;
        while i <= e {
            // SAFETY: invariant `0 <= b <= e <= len - 1` was established
            // above (raw_b/raw_e normalised against `len`, e clamped to
            // `len-1`), so `i` stays inside the slice for every push.
            result.push(unsafe { *slice.get_unchecked(i) });
            i += step;
        }
        Ok(Value::array_from_vec(result))
    } else {
        // ── negative step ────────────────────────────────────────
        let b = match rb {
            None => len - 1,
            Some(orig_b) => {
                let mut b = orig_b;
                if b < 0 {
                    b += len;
                }
                if b > len - 1 {
                    let diff = b - (len - 1);
                    if s.abs() > 1 && diff > s.abs() {
                        return Err(MonorubyErr::rangeerr(format_oob(raw_b, raw_e, s_val)));
                    }
                    b = len - 1;
                }
                if b < 0 {
                    return Ok(Value::nil());
                }
                b
            }
        };
        // Special-case: empty array, b stays at -1 above — handled
        // below via the b < e check.

        let mut e = match re {
            None => 0,
            Some(orig_e) => {
                let mut e_res = orig_e;
                if e_res < 0 {
                    e_res += len;
                }
                if excl { e_res + 1 } else { e_res }
            }
        };
        if e < 0 {
            e = 0;
        }
        if b < e {
            return Ok(Value::array_empty());
        }
        // Closed-form capacity: (b - e) / |s| + 1 terms.
        let cap = ((b - e) / s.unsigned_abs() as i64) as usize + 1;
        let mut result = Vec::with_capacity(cap);
        let slice: &[Value] = &arr;
        let mut i = b;
        while i >= e {
            // SAFETY: `b` is clamped to `len - 1` above and never goes
            // below `e >= 0` in this branch.
            result.push(unsafe { *slice.get_unchecked(i as usize) });
            i += s; // s is negative
            if i < 0 {
                break;
            }
        }
        Ok(Value::array_from_vec(result))
    }
}

/// Mirrors the Ruby format `((b..e).step(s)) out of range` — empty
/// string for nil endpoints (matching AS#inspect), `to_s` for integers.
fn format_oob(b: Value, e: Value, s: Value) -> String {
    fn render(v: Value) -> String {
        if v.is_nil() {
            String::new()
        } else if let Some(i) = v.try_fixnum() {
            i.to_string()
        } else {
            // Non-integer step path is rejected upstream; this branch
            // is defensive — show the type to aid debugging.
            "?".to_string()
        }
    }
    format!(
        "(({}..{}).step({})) out of range",
        render(b),
        render(e),
        render(s)
    )
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // `generic_count` is reached only when the all-Fixnum and the
    // all-`to_f64` fast paths both fall through, i.e. at least one of
    // (b, e, s) is a Rational / Complex / user-defined Numeric. A
    // Rational step is the simplest reproducer. Each `.size` call below
    // routes through `size` → `finite_count` → `generic_count`.
    #[test]
    fn arithmetic_sequence_generic_count_via_size() {
        run_tests(&[
            // Normal positive-step path: s_pos && !diff_neg, non-excl,
            // floor(diff/s) terms + 1.
            "(1..10).step(Rational(3,2)).size",
            "(1..10).step(Rational(1,3)).size",
            "(1..10).step(Rational(1,1)).size",
            // Excl with last-term-on-boundary → overshoot adjustment
            // decrements n_int (10 == e ⇒ drop).
            "(1...10).step(Rational(3,2)).size",
            "(1...10).step(Rational(1,3)).size",
            // Excl without overshoot — last computed term sits strictly
            // below e, so n_int stays.
            "(1...11).step(Rational(3,2)).size",
            // Negative-step normal path (!s_pos && !diff_pos).
            "(10..1).step(Rational(-3,2)).size",
            "(10...1).step(Rational(-3,2)).size",
            // Sign-mismatch early-returns: (s_pos && diff_neg) and
            // (!s_pos && diff_pos) both ⇒ Finite(0).
            "(1..10).step(Rational(-3,2)).size",
            "(10..1).step(Rational(3,2)).size",
            // Same-point endpoints: diff == 0. Inclusive ⇒ 1 term;
            // exclusive ⇒ overshoot drives n_int below zero ⇒ 0.
            "(5..5).step(Rational(3,2)).size",
            "(5...5).step(Rational(3,2)).size",
            // Range#% delegates to the same AS machinery.
            "(1..10).%(Rational(3,2)).size",
            // Numeric#step path (b is Fixnum, s is Rational).
            "1.step(10, Rational(3,2)).size",
            "10.step(1, Rational(-3,2)).size",
        ]);
    }
}
