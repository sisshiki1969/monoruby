use super::*;
use jitgen::{AbstractState, JitContext};

use num::{BigInt, Zero};

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
    // A subclass of Enumerator, as in CRuby, with a payload of its own:
    // the Enumerator builtins that need an Enumerator's payload take it
    // from the one the sequence keeps behind it (`enumerator.rs`,
    // `Backing`).
    let enumerator_class = globals.store[ENUMERATOR_CLASS].get_module();
    globals.define_builtin_class(
        "ArithmeticSequence",
        ARITHMETIC_SEQUENCE_CLASS,
        enumerator_class,
        ENUMERATOR_CLASS,
        ObjTy::ARITHMETIC_SEQUENCE,
    );
    // CRuby: `Enumerator::ArithmeticSequence.allocate` raises
    // `TypeError: allocator undefined for ...` and `.new` raises
    // `NoMethodError`. Without this the default object allocator
    // produces an `ObjTy::OBJECT` instance whose class is
    // ArithmeticSequence; any subsequent ArithmeticSequence builtin
    // (e.g. `inspect`) then hits `as_arithmetic_sequence_inner`'s
    // type assertion and aborts the process (non-unwinding builtin).
    globals.store[ARITHMETIC_SEQUENCE_CLASS].clear_alloc_func();
    let meta = globals.store.get_metaclass(ARITHMETIC_SEQUENCE_CLASS).id();
    // Insert the undef tombstone directly: `Class#new` is now the Ruby
    // trampoline defined later by startup.rb, so at builtin-init time
    // there is no `new` to look up — but the tombstone must still
    // shadow it once defined.
    globals
        .store
        .add_empty_method(meta, IdentId::NEW, Visibility::Undefined);
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
        inline_gen2!(as_begin_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "end",
        end,
        inline_gen2!(as_end_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "step",
        step,
        inline_gen2!(as_step_inline),
        0,
    );
    globals.define_builtin_inline_func(
        ARITHMETIC_SEQUENCE_CLASS,
        "exclude_end?",
        exclude_end,
        inline_gen2!(as_exclude_end_inline),
        0,
    );
    // `each` is intentionally NOT registered here — it's defined in
    // `builtins/arithmetic_sequence.rb` (Ruby) so monoruby's JIT can
    // inline the block dispatch directly into the loop. See benchmark
    // discussion in PR comments.
    globals.define_builtin_func(ARITHMETIC_SEQUENCE_CLASS, "size", size, 0);
    globals.define_builtin_func(ARITHMETIC_SEQUENCE_CLASS, "__float_step", float_step, 0);
    globals.define_builtin_func_with(ARITHMETIC_SEQUENCE_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_func_with(ARITHMETIC_SEQUENCE_CLASS, "last", last, 0, 1, false);
    // `Array#[]` with a sequence as its index. Not a method of the class in
    // CRuby, hence the internal name.
    globals.define_builtin_func(ARITHMETIC_SEQUENCE_CLASS, "__slice_array", index_op, 1);
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
    _: Option<CachedClass>,
    _: Option<CachedClass>,
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
    _: Option<CachedClass>,
    _: Option<CachedClass>,
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
    _: Option<CachedClass>,
    _: Option<CachedClass>,
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
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    // Pure-LIR bool-field read (no arch-specific closure), as `range_exclude_end`.
    ir.bool_field_to_reg(GP::Rax, GP::Rdi, crate::rvalue::AS_EXCLUDE_END_OFFSET as i32);
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
    // Pure-LIR field read (no arch-specific closure): the receiver is in Rdi,
    // load `[Rdi + offset]` into Rax. Lowers to `AsmInst::LoadFieldToReg` →
    // `LInst::Load { Field }`, identical on both arches (cf. `range_begin`).
    ir.load_field_to_reg(GP::Rax, GP::Rdi, offset as i32);
    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

// ─── Type-dispatched helpers ────────────────────────────────────────
//
// The iteration rules are CRuby's (`enumerator.c`): a sequence is a
// *Float* sequence as soon as any of begin, end and step is a Float —
// its terms are then `i * step + begin` in C doubles, counted by
// `ruby_float_step_size` — and otherwise it is walked with the numbers'
// own `+`, `-`, `*` and `div`, whatever they are (Integer, Rational, a
// user's Numeric). All-Fixnum sequences take a native shortcut that
// gives the same answers.

/// True iff `v` is numerically zero (Fixnum/BigInt/Float).
fn value_is_zero(v: Value) -> bool {
    match v.unpack() {
        RV::Fixnum(0) => true,
        RV::Float(f) => f == 0.0,
        RV::BigInt(b) => b.is_zero(),
        _ => false,
    }
}

/// CRuby's `ruby_float_step_size`: how many terms `beg, beg + unit, …`
/// has up to `end`, as a C double (it can be infinite, and NaN for NaN
/// operands). The count is rounded by an error bound proportional to
/// the magnitudes involved, so that `1.0.step(1.3, 0.1)` has the four
/// terms a reader counts although `0.3 / 0.1` is `2.9999999999999996`.
fn float_step_size(beg: f64, end: f64, unit: f64, excl: bool) -> f64 {
    if unit == 0.0 {
        return f64::INFINITY;
    }
    if unit.is_infinite() {
        let one = if unit > 0.0 { beg <= end } else { beg >= end };
        return if one { 1.0 } else { 0.0 };
    }
    let mut n = (end - beg) / unit;
    let err = ((beg.abs() + end.abs() + (end - beg).abs()) / unit.abs() * f64::EPSILON).min(0.5);
    if excl {
        if n <= 0.0 {
            return 0.0;
        }
        n = if n < 1.0 { 0.0 } else { (n - err).floor() };
        let d = (n + 1.0) * unit + beg;
        if (beg < end && d < end) || (beg > end && d > end) {
            n += 1.0;
        }
    } else {
        if n < 0.0 {
            return 0.0;
        }
        n = (n + err).floor();
        let d = (n + 1.0) * unit + beg;
        if (beg < end && d <= end) || (beg > end && d >= end) {
            n += 1.0;
        }
    }
    n + 1.0
}

/// A Float sequence, as CRuby's `ruby_float_step` sets it up.
struct FloatSeq {
    beg: f64,
    end: f64,
    unit: f64,
    /// [`float_step_size`] of the three.
    len: f64,
}

impl FloatSeq {
    /// The `i`-th term. The last one is clamped to the end: `i * unit +
    /// beg` can land a rounding error beyond it.
    fn term(&self, i: i64) -> f64 {
        let d = (i as f64) * self.unit + self.beg;
        if self.unit >= 0.0 {
            if self.end < d { self.end } else { d }
        } else if d < self.end {
            self.end
        } else {
            d
        }
    }

    /// The length as `#size` answers it: `Infinity`, or an Integer.
    fn len_value(&self) -> Result<Value> {
        if self.len.is_infinite() {
            Ok(Value::float(self.len))
        } else {
            // NaN is a FloatDomainError here, as `rb_dbl2big` makes it.
            Value::coerce_f64_to_int(self.len)
        }
    }
}

/// The sequence as a Float one, if it is one: any of begin, end and
/// step a Float. An endless sequence runs to the infinity its step
/// points at.
fn float_seq(
    vm: &mut Executor,
    globals: &mut Globals,
    b: Value,
    e: Value,
    s: Value,
    excl: bool,
) -> Result<Option<FloatSeq>> {
    if !(b.is_float() || e.is_float() || s.is_float()) {
        return Ok(None);
    }
    let unit = thread::num2dbl(vm, globals, s)?;
    let beg = thread::num2dbl(vm, globals, b)?;
    let end = if e.is_nil() {
        if unit < 0.0 {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }
    } else {
        thread::num2dbl(vm, globals, e)?
    };
    let len = float_step_size(beg, end, unit, excl);
    Ok(Some(FloatSeq {
        beg,
        end,
        unit,
        len,
    }))
}

fn call(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    op: IdentId,
    arg: Value,
) -> Result<Value> {
    vm.invoke_method_inner(globals, op, recv, &[arg], None, None)
}

/// CRuby's `rb_int_plus` / `rb_int_minus` / `rb_int_mul` for a receiver
/// that need not be an Integer: an Integer's own operator, and for
/// anything else `rb_num_coerce_bin` — the *argument* coerces the
/// receiver, and the operator is applied to what comes back.
///
/// `arith_seq_size`, the tail of `arith_seq_last` and the endless loop
/// of `arith_seq_each` are written with these, where the rest of the
/// file uses the generic `num_plus` & co. ([`call`]). The difference
/// shows with a Rational step, which an Integer argument coerces to a
/// Float: `(1..2).step(1r/3).last(2)` is `[1.6666666666666667, 2.0]` in
/// CRuby although `#last` is `(2/1)`.
fn int_op(
    vm: &mut Executor,
    globals: &mut Globals,
    x: Value,
    op: IdentId,
    y: Value,
) -> Result<Value> {
    if x.is_integer() {
        return call(vm, globals, x, op, y);
    }
    let pair = call(vm, globals, y, IdentId::get_id("coerce"), x)?;
    match pair.try_array_ty() {
        Some(pair) if pair.len() == 2 => call(vm, globals, pair[0], op, pair[1]),
        _ => Err(MonorubyErr::typeerr("coerce must return [x, y]")),
    }
}

/// CRuby's `num_idiv` (and `rb_int_idiv`, which differs only for a
/// receiver that has no `div` worth calling): the floored quotient, as
/// an Integer wherever the operands' own division makes that possible.
fn num_idiv(vm: &mut Executor, globals: &mut Globals, a: Value, b: Value) -> Result<Value> {
    let q = if a.is_float() || a.class() == RATIONAL_CLASS {
        call(vm, globals, a, IdentId::_DIV, b)?
    } else {
        call(vm, globals, a, IdentId::get_id("div"), b)?
    };
    if q.is_integer() {
        Ok(q)
    } else {
        vm.invoke_method_inner(globals, IdentId::get_id("floor"), q, &[], None, None)
    }
}

/// CRuby's `rb_num_negative_int_p`.
fn negative_p(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<bool> {
    Ok(match v.unpack() {
        RV::Fixnum(i) => i < 0,
        RV::BigInt(b) => b.sign() == num::bigint::Sign::Minus,
        _ => call(vm, globals, v, IdentId::_LT, Value::integer(0))?.as_bool(),
    })
}

/// Where a sequence that is not a Float one stops: `len_1` is the index
/// of the last term (negative for an empty sequence — there is no
/// `Tail` then), `adjusted` says the end was excluded and exactly hit,
/// `last` is the last term.
struct Tail {
    len_1: Value,
    last: Value,
    adjusted: bool,
}

impl Tail {
    /// As `arith_seq_last` and `arith_seq_each` compute it, with the
    /// operands' own arithmetic (`num_minus`, `num_idiv`, …).
    fn new(
        vm: &mut Executor,
        globals: &mut Globals,
        b: Value,
        e: Value,
        s: Value,
        excl: bool,
    ) -> Result<Option<Self>> {
        let diff = call(vm, globals, e, IdentId::_SUB, b)?;
        let len_1 = num_idiv(vm, globals, diff, s)?;
        if negative_p(vm, globals, len_1)? {
            return Ok(None);
        }
        let prod = call(vm, globals, s, IdentId::_MUL, len_1)?;
        let mut last = call(vm, globals, b, IdentId::_ADD, prod)?;
        let adjusted = excl && call(vm, globals, last, IdentId::_EQ, e)?.as_bool();
        if adjusted {
            last = call(vm, globals, last, IdentId::_SUB, s)?;
        }
        Ok(Some(Tail {
            len_1,
            last,
            adjusted,
        }))
    }

    /// As `arith_seq_size` computes it, with [`int_op`].
    fn for_size(
        vm: &mut Executor,
        globals: &mut Globals,
        b: Value,
        e: Value,
        s: Value,
        excl: bool,
    ) -> Result<Option<Self>> {
        let diff = int_op(vm, globals, e, IdentId::_SUB, b)?;
        let len_1 = num_idiv(vm, globals, diff, s)?;
        if negative_p(vm, globals, len_1)? {
            return Ok(None);
        }
        let prod = int_op(vm, globals, s, IdentId::_MUL, len_1)?;
        let last = int_op(vm, globals, b, IdentId::_ADD, prod)?;
        let adjusted = excl && call(vm, globals, last, IdentId::_EQ, e)?.as_bool();
        Ok(Some(Tail {
            len_1,
            last,
            adjusted,
        }))
    }

    /// The number of terms.
    fn len(&self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if self.adjusted {
            Ok(self.len_1)
        } else {
            call(vm, globals, self.len_1, IdentId::_ADD, Value::integer(1))
        }
    }
}

/// The number of terms of an all-Fixnum sequence.
fn fixnum_count(b: i64, e: i64, s: i64, excl: bool) -> i64 {
    let diff = e as i128 - b as i128;
    if s == 0 || (s > 0 && diff < 0) || (s < 0 && diff > 0) {
        return 0;
    }
    let s128 = s as i128;
    let mut n_int = diff / s128; // same sign ⇒ trunc == floor
    if excl && b as i128 + n_int * s128 == e as i128 {
        n_int -= 1;
    }
    // n_int + 1 is the count; clamp to i64 max (unrealistically large).
    (n_int + 1).clamp(0, i64::MAX as i128) as i64
}

/// The `i`-th term of an all-Fixnum sequence, an Integer of whatever
/// size it takes.
fn fixnum_term(b: i64, i: i64, s: i64) -> Value {
    let v = b as i128 + (i as i128) * (s as i128);
    match i64::try_from(v) {
        Ok(v) => Value::integer(v),
        Err(_) => Value::bigint(BigInt::from(v)),
    }
}

fn fixnum_terms(b: i64, s: i64, start: i64, n: i64) -> Value {
    Value::array_from_iter((0..n).map(|i| fixnum_term(b, start + i, s)))
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
    if let Some(fs) = float_seq(vm, globals, b, e, s, excl)? {
        return fs.len_value();
    }
    if e.is_nil() || value_is_zero(s) {
        return Ok(Value::float(f64::INFINITY));
    }
    if let (Some(b), Some(e), Some(s)) = (b.try_fixnum(), e.try_fixnum(), s.try_fixnum()) {
        return Ok(Value::integer(fixnum_count(b, e, s, excl)));
    }
    match Tail::for_size(vm, globals, b, e, s, excl)? {
        Some(tail) => tail.len(vm, globals),
        None => Ok(Value::integer(0)),
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
// the Rust + `invoke_block` path. It asks `__float_step` what a Float
// sequence is made of, so that the counting stays in one place.

///
/// ### Enumerator::ArithmeticSequence#__float_step
///
/// - __float_step -> [beg, unit, end, len] | nil
///
/// For a Float sequence — CRuby's `ruby_float_step` — the three
/// operands as Floats and the number of terms (an Integer, or
/// `Infinity`); nil for any other sequence, and for a Complex step,
/// which `arith_seq_each` keeps out of the Float loop.
#[monoruby_builtin]
fn float_step(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (b, e, s, excl) = unpack_as(lfp.self_val());
    if s.class() == COMPLEX_CLASS {
        return Ok(Value::nil());
    }
    let Some(fs) = float_seq(vm, globals, b, e, s, excl)? else {
        return Ok(Value::nil());
    };
    // A NaN length is a loop that never runs.
    let len = if fs.len.is_nan() {
        Value::integer(0)
    } else {
        fs.len_value()?
    };
    Ok(Value::array_from_vec(vec![
        Value::float(fs.beg),
        Value::float(fs.unit),
        Value::float(fs.end),
        len,
    ]))
}

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
    let Some(n_arg) = lfp.try_arg(0) else {
        // The begin as it is — `(1..3.0).step.first` is `1` although the
        // terms are Floats — unless the sequence is empty.
        if b.is_nil() {
            return Ok(Value::nil());
        }
        if !e.is_nil() {
            let zero = Value::integer(0);
            let empty = if call(vm, globals, s, IdentId::_GT, zero)?.as_bool() {
                call(vm, globals, b, IdentId::_GT, e)?.as_bool()
            } else if call(vm, globals, s, IdentId::_LT, zero)?.as_bool() {
                call(vm, globals, b, IdentId::_LT, e)?.as_bool()
            } else {
                false
            };
            if empty {
                return Ok(Value::nil());
            }
        }
        return Ok(b);
    };
    let mut n = n_arg.num2long(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("attempt to take negative size"));
    }
    if n == 0 {
        return Ok(Value::array_empty());
    }
    if let (Some(b), Some(s)) = (b.try_fixnum(), s.try_fixnum()) {
        if e.is_nil() {
            return Ok(fixnum_terms(b, s, 0, n));
        }
        if let Some(e) = e.try_fixnum() {
            return Ok(fixnum_terms(b, s, 0, n.min(fixnum_count(b, e, s, excl))));
        }
    }
    if let Some(fs) = float_seq(vm, globals, b, e, s, excl)? {
        if (n as f64) > fs.len {
            n = fs.len as i64;
        }
        if fs.unit.is_infinite() {
            // `i * unit + beg` would be NaN: the one term is the begin.
            let one = (fs.len > 0.0).then(|| Value::float(fs.beg));
            return Ok(Value::array_from_iter(one.into_iter()));
        }
        return Ok(Value::array_from_iter(
            (0..n).map(|i| Value::float(fs.term(i))),
        ));
    }
    // What is left is `Enumerator#first(n)` over `#each`: the terms by
    // repeated addition, which for an endless sequence is `rb_int_plus`.
    let endless = e.is_nil();
    if !endless && !value_is_zero(s) {
        let len = match Tail::new(vm, globals, b, e, s, excl)? {
            Some(tail) => tail.len(vm, globals)?,
            None => return Ok(Value::array_empty()),
        };
        if let Some(len) = len.try_fixnum() {
            n = n.min(len);
        }
    }
    let mut terms = Vec::with_capacity(n as usize);
    let mut c = b;
    for i in 0..n {
        if i != 0 {
            c = if endless {
                int_op(vm, globals, c, IdentId::_ADD, s)?
            } else {
                call(vm, globals, c, IdentId::_ADD, s)?
            };
        }
        terms.push(c);
    }
    Ok(Value::array_from_vec(terms))
}

// ─── last ──────────────────────────────────────────────────────────

///
/// ### Enumerator::ArithmeticSequence#last
///
/// - last -> Numeric | nil
/// - last(n) -> [Numeric]
///
/// The one place a Float sequence is *not* walked in C doubles: CRuby's
/// `arith_seq_last` computes with the operands' own arithmetic for
/// every kind of sequence, so `(1..3.0).step(1).last` is the Integer
/// `3` although `to_a.last` is `3.0`, and `last(n)` counts back from
/// the last term by repeated addition.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/last.html]
#[monoruby_builtin]
fn last(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (b, e, s, excl) = unpack_as(lfp.self_val());
    if e.is_nil() {
        return Err(MonorubyErr::rangeerr(
            "cannot get the last element of endless arithmetic sequence",
        ));
    }
    let arg = lfp.try_arg(0);
    let empty = |arg: Option<Value>| {
        if arg.is_some() {
            Value::array_empty()
        } else {
            Value::nil()
        }
    };
    let fixnums = match (b.try_fixnum(), e.try_fixnum(), s.try_fixnum()) {
        (Some(b), Some(e), Some(s)) if s != 0 => Some((b, e, s)),
        _ => None,
    };
    // The number of terms and the last of them.
    let (len, last) = if let Some((b, e, s)) = fixnums {
        let count = fixnum_count(b, e, s, excl);
        if count == 0 {
            return Ok(empty(arg));
        }
        (Value::integer(count), fixnum_term(b, count - 1, s))
    } else {
        let Some(tail) = Tail::new(vm, globals, b, e, s, excl)? else {
            return Ok(empty(arg));
        };
        (tail.len(vm, globals)?, tail.last)
    };
    let Some(n_arg) = arg else {
        return Ok(last);
    };
    // The count is clamped to the length *before* NUM2LONG, so one
    // beyond a `long` takes everything rather than raising.
    let mut nv = if n_arg.is_integer() {
        n_arg
    } else {
        n_arg.coerce_to_int(vm, globals)?
    };
    if call(vm, globals, nv, IdentId::_GT, len)?.as_bool() {
        nv = len;
    }
    let n = nv.num2long(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("negative array size"));
    }
    if let Some((b, e, s)) = fixnums {
        return Ok(fixnum_terms(b, s, fixnum_count(b, e, s, excl) - n, n));
    }
    let back = int_op(vm, globals, s, IdentId::_MUL, nv)?;
    let mut c = int_op(vm, globals, last, IdentId::_SUB, back)?;
    let mut terms = Vec::with_capacity(n as usize);
    for _ in 0..n {
        c = int_op(vm, globals, c, IdentId::_ADD, s)?;
        terms.push(c);
    }
    Ok(Value::array_from_vec(terms))
}

// ─── [] (Array stride-aware index) ─────────────────────────────────

///
/// ### Enumerator::ArithmeticSequence#__slice_array
///
/// - __slice_array(ary) -> Array
///
/// `aseq.__slice_array(ary)` extracts the slice of `ary` whose indices are the terms
/// of this sequence. Mirrors CRuby's `rb_ary_subseq_step`: integer
/// indices only (the caller is expected to be passing an AS built for
/// slicing — `Numeric#step` / `Range#step` / `Range#%`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aArithmeticSequence/i/=5b=5d.html]
#[monoruby_builtin]
fn index_op(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (raw_b, raw_e, s_val, raw_excl) = unpack_as(lfp.self_val());

    let arr_v = lfp.arg(0);
    let arr = arr_v.as_array();
    let alen = arr.len() as i64;

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
    let mut b = coerce_idx(raw_b)
        .ok_or_else(|| MonorubyErr::typeerr("begin must be an Integer for Array indexing"))?;
    let mut e = coerce_idx(raw_e)
        .ok_or_else(|| MonorubyErr::typeerr("end must be an Integer for Array indexing"))?;
    let mut excl = raw_excl;

    // Mirror CRuby's `rb_arithmetic_sequence_beg_len_step` (enumerator.c)
    // followed by `rb_range_component_beg_len` (range.c) and
    // `rb_ary_subseq_step` (array.c). The combined effect:
    //   * Negative step is folded into a forward (begin, end) span by
    //     bumping `end` by +1 when `exclude_end?` and swapping endpoints.
    //   * `|step| > 1` engages the strict err=1 checks: out-of-range
    //     raises rather than clamps.
    //   * `|step| == 1` uses CRuby's err=0 clamping (begin past len ⇒
    //     return nil; end past len ⇒ silently clamp to len).

    // ── Step 1: fold negative step into a forward span. ──
    if s < 0 {
        if excl && e.is_some() {
            e = Some(e.unwrap() + 1);
            excl = false;
        }
        std::mem::swap(&mut b, &mut e);
    }

    // ── Step 2: rb_range_component_beg_len. ──
    let mut beg = b.unwrap_or(0);
    let mut end = e.unwrap_or(-1);
    if e.is_none() {
        excl = false;
    }
    let strict = s.unsigned_abs() > 1;
    let oob = || MonorubyErr::rangeerr(format_oob(raw_b, raw_e, s_val, raw_excl));

    if beg < 0 {
        beg += alen;
        if beg < 0 {
            // CRuby: range_component returns Qnil here. The caller
            // (`rb_arithmetic_sequence_beg_len_step`) then raises iff
            // strict; otherwise the Qnil bubbles up to `[]` as nil.
            return if strict {
                Err(oob())
            } else {
                Ok(Value::nil())
            };
        }
    }
    if end < 0 {
        end += alen;
    }
    if !excl {
        end += 1;
    }

    let mut range_len = if strict {
        // err=1: no clamp of end against alen. Out-of-range raises.
        if beg > alen {
            return Err(oob());
        }
        let len_unclamped = end - beg;
        if len_unclamped > alen {
            return Err(oob());
        }
        len_unclamped.max(0)
    } else {
        // err=0: clamp end past alen, return nil for beg past alen.
        if beg > alen {
            return Ok(Value::nil());
        }
        if end > alen {
            end = alen;
        }
        (end - beg).max(0)
    };

    // ── Step 3: rb_ary_subseq_step. ──
    if alen < range_len || alen < beg + range_len {
        range_len = alen - beg;
    }
    if range_len == 0 {
        return Ok(Value::array_empty());
    }

    let slice: &[Value] = &arr;
    if s == 1 {
        return Ok(Value::array_from_vec(
            slice[beg as usize..(beg + range_len) as usize].to_vec(),
        ));
    }
    if s == -1 {
        return Ok(Value::array_from_vec(
            slice[beg as usize..(beg + range_len) as usize]
                .iter()
                .rev()
                .copied()
                .collect(),
        ));
    }

    // |s| > 1: stride through the (beg, range_len) span. Matches
    // CRuby's `ary_make_partial_step`: forward step starts at `beg`,
    // negative step starts at the far end and walks backward.
    let s_eff = if s < 0 && s < -range_len { -range_len } else { s };
    let ustep = s_eff.unsigned_abs() as i64;
    let n = (range_len + ustep - 1) / ustep;
    let mut result = Vec::with_capacity(n as usize);
    let mut j = beg + if s_eff > 0 { 0 } else { range_len - 1 };
    for _ in 0..n {
        // SAFETY: `0 <= beg`, `beg + range_len <= alen`, and `j` walks
        // strictly inside `[beg, beg + range_len)` for every push (n
        // is derived from `range_len / |s_eff|` above).
        result.push(unsafe { *slice.get_unchecked(j as usize) });
        j += s_eff;
    }
    Ok(Value::array_from_vec(result))
}

/// Mirrors the Ruby format `((b..e).step(s)) out of range` — empty
/// string for nil endpoints (matching AS#inspect), `to_s` for integers,
/// `...` rendered when the underlying AS has `exclude_end? == true`.
fn format_oob(b: Value, e: Value, s: Value, excl: bool) -> String {
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
        "(({}{}{}).step({})) out of range",
        render(b),
        if excl { "..." } else { ".." },
        render(e),
        render(s)
    )
}

#[cfg(test)]
mod tests {
    // Coverage for the two error-prone helpers that aren't directly
    // exposed: `Tail::for_size` (the Rational / user-Numeric path of
    // `size`) and `format_oob` (the error builder dispatched from
    // `index_op`).
    //
    // `Tail::for_size` is reached only when the sequence is neither
    // all-Fixnum nor a Float one — i.e. at least one of (b, e, s) is a
    // Rational / Bignum / user-defined Numeric. Rational steps below
    // drive `.size` through it.
    //
    // `format_oob` fires from `index_op` whenever the underlying
    // `rb_arithmetic_sequence_beg_len_step` (folded into `index_op`)
    // sees `|step| > 1` AND either `begin > len` or `(end_excl_adj
    // - begin) > len` after CRuby's range normalisation. Each error
    // case uses `rescue $!.message` so the rendered string is compared
    // against CRuby, and is paired with the in-range neighbour that
    // escapes the check.
    use crate::tests::*;

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

    #[test]
    fn arithmetic_sequence_index_op_format_oob() {
        run_tests(&[
            // ── positive step, begin past len ──────────────────────
            // Endless `(7..).step(2)`: raw_e is nil ⇒ rendered as
            // empty between `..` and `).step`.
            "arr = [10, 20, 30, 40, 50]; arr[(7..).step(2)] rescue $!.message",
            // Boundary just past len, same path.
            "arr = [10, 20, 30, 40, 50]; arr[(6..).step(2)] rescue $!.message",
            // Begin past len but s == 1 ⇒ no error, returns nil
            // (the err=0 clamping path).
            "arr = [10, 20, 30, 40, 50]; arr[(7..).step(1)]",
            "arr = [10, 20, 30, 40, 50]; arr[(6..).step(1)]",
            // Begin == len ⇒ empty result (not an error).
            "arr = [10, 20, 30, 40, 50]; arr[(5..).step(2)]",

            // ── positive step, end past len (issue #492 cat. 1) ────
            // `(0..N).step(2)` for any N >= len raises, regardless of
            // alignment. CRuby's check is `(end + 1 - begin) > alen`
            // for inclusive, `(end - begin) > alen` for exclusive.
            "arr = [10, 20, 30, 40, 50]; arr[(0..5).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(0..6).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(0..7).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(0...6).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(0...10).step(2)] rescue $!.message",
            // End exactly at len-1 ⇒ no error.
            "arr = [10, 20, 30, 40, 50]; arr[(0..4).step(2)]",
            // End < len, no error (issue #492 cat. 2: previously
            // rejected because `e_res == len` was misread as oob).
            "arr = [10, 20, 30, 40, 50]; arr[(0..3).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(1..5).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(2..5).step(3)]",
            "arr = [10, 20, 30, 40, 50]; arr[(3..5).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(2..6).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(3..7).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(4..8).step(2)]",

            // ── negative step (issue #492 cat. 3) ──────────────────
            // The reversal in CRuby folds `(b..e).step(-s)` into a
            // forward `(e..b).step(-s)` after bumping `e` for excl;
            // strict checks then catch any case where the span exceeds
            // the array bounds.
            "arr = [10, 20, 30, 40, 50]; arr[(5..0).step(-2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(6..0).step(-2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(6...0).step(-3)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(10..0).step(-3)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(10..0).step(-2)] rescue $!.message",
            // |s| == 1 escapes the strict check ⇒ clamped, no error.
            "arr = [10, 20, 30, 40, 50]; arr[(10..0).step(-1)]",
            "arr = [10, 20, 30, 40, 50]; arr[(5..0).step(-1)]",

            // ── negative end with negative step (issue #492 cat. 4) ─
            // After reversal: e=-1 with excl becomes 0; the forward
            // span [0, b+1) drives the slice. `(5...-1).step(-1)`
            // walks the whole array in reverse.
            "arr = [10, 20, 30, 40, 50]; arr[(5...-1).step(-1)]",
            "arr = [10, 20, 30, 40, 50]; arr[(5...-1).step(-2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(5..-1).step(-2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(4..-1).step(-2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(4..0).step(-2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(5...0).step(-2)]",

            // ── format_oob renders `...` for excl (issue #492 cat. 5) ─
            "arr = [10, 20, 30, 40, 50]; arr[Range.new(6, 5, true).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[Range.new(10, 0, true).step(-2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(0...6).step(3)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(7...).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(...6).step(2)] rescue $!.message",

            // ── beginless / endless with valid bounds ──────────────
            "arr = [10, 20, 30, 40, 50]; arr[(...5).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(...-1).step(2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(..0).step(-2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(..5).step(-2)]",
            "arr = [10, 20, 30, 40, 50]; arr[(4..).step(2)]",

            // ── negative begin pre-normalised below zero (err=0 vs
            //    err=1 branches both exercised) ──────────────────────
            "arr = [10, 20, 30, 40, 50]; arr[(-100..5).step(1)]",
            "arr = [10, 20, 30, 40, 50]; arr[(-100..5).step(2)] rescue $!.message",
            "arr = [10, 20, 30, 40, 50]; arr[(-100..-90).step(2)] rescue $!.message",
        ]);
    }
}
