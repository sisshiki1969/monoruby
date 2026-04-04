use super::*;
use jitgen::JitContext;
use num::{BigInt, ToPrimitive, Zero};
use std::ops::{BitAnd, BitOr, BitXor};

//
// Integer class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class("Integer", INTEGER_CLASS, numeric, OBJECT_CLASS, None);
    globals.define_builtin_func_with(INTEGER_CLASS, "chr", chr, 0, 1, false);
    //globals.define_builtin_inline_func(INTEGER_CLASS, "succ", succ, Box::new(integer_succ), 0);
    //globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    //globals.define_builtin_func_with(INTEGER_CLASS, "step", step, 1, 2, false);
    globals.define_builtin_func(INTEGER_CLASS, "upto", upto, 1);
    globals.define_builtin_func(INTEGER_CLASS, "downto", downto, 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "to_f", to_f, Box::new(integer_tof), 0);
    globals.define_builtin_func(INTEGER_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(INTEGER_CLASS, "to_int", to_i, 0);

    globals.define_basic_op(INTEGER_CLASS, "+", add, 1);
    globals.define_basic_op(INTEGER_CLASS, "-", sub, 1);
    globals.define_basic_op(INTEGER_CLASS, "*", mul, 1);
    globals.define_basic_op(INTEGER_CLASS, "/", div, 1);
    globals.define_basic_op(INTEGER_CLASS, "%", rem, 1);
    globals.define_basic_op(INTEGER_CLASS, "&", bitand, 1);
    globals.define_basic_op(INTEGER_CLASS, "|", bitor, 1);
    globals.define_basic_op(INTEGER_CLASS, "^", bitxor, 1);
    globals.define_builtin_func(INTEGER_CLASS, "divmod", divmod, 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, ">>", shr, Box::new(integer_shr), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "<<", shl, Box::new(integer_shl), 1);
    globals.define_builtin_func(INTEGER_CLASS, "==", eq, 1);
    globals.define_builtin_func(INTEGER_CLASS, "===", eq, 1);
    globals.define_builtin_func(INTEGER_CLASS, ">=", ge, 1);
    globals.define_builtin_func(INTEGER_CLASS, ">", gt, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<=", le, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<", lt, 1);
    globals.define_builtin_func(INTEGER_CLASS, "!=", ne, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func_with(INTEGER_CLASS, "[]", index, 1, 2, false);
    globals.define_builtin_func(INTEGER_CLASS, "even?", even_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "odd?", odd_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "nonzero?", nonzero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "zero?", zero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "size", size, 0);
    globals.define_builtin_func(INTEGER_CLASS, "bit_length", bit_length, 0);
    globals.define_builtin_func_with(INTEGER_CLASS, "to_s", to_s, 0, 1, false);
    globals.define_builtin_func_with(INTEGER_CLASS, "inspect", to_s, 0, 1, false);
    globals.define_builtin_func(INTEGER_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func(INTEGER_CLASS, "abs", abs, 0);
    globals.define_builtin_func(INTEGER_CLASS, "magnitude", abs, 0);
}

/*///
/// ### Integer#times
///
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
#[monoruby_builtin]
fn times(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => vm.invoke_block_iter1(globals, bh, (0..i).map(Value::integer))?,
        RV::BigInt(_) => unimplemented!(),
        _ => unreachable!(),
    };

    Ok(lfp.self_val())
}*/

struct PosStep {
    cur: i64,
    limit: i64,
    step: i64, // must be > 0
}

impl Iterator for PosStep {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur > self.limit {
            None
        } else {
            let v = Value::integer(self.cur);
            self.cur += self.step;
            Some(v)
        }
    }
}

struct NegStep {
    cur: i64,
    limit: i64,
    step: i64, // must be < 0
}

impl Iterator for NegStep {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        if self.limit > self.cur {
            None
        } else {
            let v = Value::integer(self.cur);
            self.cur += self.step;
            Some(v)
        }
    }
}

/*///
/// ### Integer#step
///
/// - step(limit, step = 1) {|n| ... } -> self
/// - step(limit, step = 1) -> Enumerator
/// - [NOT SUPPORTED] step(limit, step = 1) -> Enumerator::ArithmeticSequence
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/step.html]
#[monoruby_builtin]
fn step(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("step");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = lfp.arg(0).coerce_to_int(vm, globals)?;
    let step = if let Some(arg1) = lfp.try_arg(1) {
        let step = arg1.coerce_to_int(vm, globals)?;
        if step == 0 {
            return Err(MonorubyErr::argumenterr("Step can not be 0."));
        }
        step
    } else {
        1
    };

    if step > 0 {
        let iter = PosStep { cur, step, limit };
        vm.invoke_block_iter1(globals, bh, iter)?;
    } else {
        let iter = NegStep { cur, step, limit };
        vm.invoke_block_iter1(globals, bh, iter)?;
    }
    Ok(lfp.self_val())
}*/

///
/// ### Integer#upto
///
/// - upto(max) {|n| ... } -> Integer
/// - upto(max) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/upto.html]
#[monoruby_builtin]
fn upto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("upto");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = match lfp.arg(0).coerce_to_int_i64(vm, globals) {
        Ok(v) => v,
        Err(_) => return Err(MonorubyErr::argumenterr(format!(
            "bad value for range"
        ))),
    };
    if cur > limit {
        return Ok(lfp.self_val());
    }

    let iter = PosStep {
        cur,
        limit,
        step: 1,
    };
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Integer#downto
///
/// - downto(min) {|n| ... } -> self
/// - downto(min) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/downto.html]
#[monoruby_builtin]
fn downto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("downto");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = match lfp.arg(0).coerce_to_int_i64(vm, globals) {
        Ok(v) => v,
        Err(_) => return Err(MonorubyErr::argumenterr(format!(
            "bad value for range"
        ))),
    };
    if cur < limit {
        return Ok(lfp.self_val());
    }

    let iter = NegStep {
        cur,
        limit,
        step: -1,
    };
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
#[monoruby_builtin]
fn chr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use crate::value::rvalue::Encoding;
    let encoding_arg = lfp.try_arg(0);

    // Parse encoding if provided
    let encoding = if let Some(enc_val) = encoding_arg {
        if let Some(s) = enc_val.is_str() {
            Some(Encoding::try_from_str(s)?)
        } else {
            // Check if it's an Encoding object
            let enc_class_id = globals
                .store
                .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
                .map(|v| v.as_class_id());
            if enc_class_id == Some(enc_val.class()) {
                let s = globals.store.get_ivar(enc_val, IdentId::_ENCODING).unwrap();
                Some(Encoding::try_from_str(s.as_str())?)
            } else {
                // Try to_str coercion
                let s = enc_val.coerce_to_string(vm, globals)?;
                Some(Encoding::try_from_str(&s)?)
            }
        }
    } else {
        None
    };

    let i = match lfp.self_val().try_fixnum() {
        Some(i) => i,
        None => {
            // Handle BigInt case - always out of range
            return Err(MonorubyErr::rangeerr("bignum out of char range"));
        }
    };

    match encoding {
        Some(Encoding::Utf8) | Some(Encoding::UsAscii) => {
            // UTF-8 encoding: support full Unicode codepoint range
            if i < 0 || i > 0x10FFFF {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            match char::from_u32(i as u32) {
                Some(c) => {
                    let mut buf = [0u8; 4];
                    let s = c.encode_utf8(&mut buf);
                    Ok(Value::string_from_str(s))
                }
                None => {
                    // Invalid codepoint (e.g., surrogates 0xD800-0xDFFF)
                    Err(MonorubyErr::rangeerr(format!(
                        "invalid codepoint 0x{:X} in UTF-8",
                        i
                    )))
                }
            }
        }
        Some(Encoding::Ascii8) | None => {
            // ASCII-8BIT/BINARY encoding or no encoding specified: 0-255 only
            if let Ok(b) = u8::try_from(i) {
                if encoding.is_none() && b <= 0x7f {
                    // No encoding specified, 0-127: return US-ASCII (mapped to UTF-8)
                    return Ok(Value::string_from_str(std::str::from_utf8(&[b]).unwrap()));
                }
                return Ok(Value::bytes_from_slice(&[b]));
            }
            Err(MonorubyErr::char_out_of_range(
                &globals.store,
                lfp.self_val(),
            ))
        }
    }
}

// Integer#succ is defined in Ruby (integer.rb).
// The Rust implementation and JIT inline specialization are commented out
// because the Ruby definition takes precedence at runtime.
/*
#[monoruby_builtin]
fn succ(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match lfp.self_val().unpack() {
        RV::Fixnum(i) => i
            .checked_add(1)
            .map(Value::integer)
            .unwrap_or_else(|| Value::bigint(BigInt::from(i) + 1)),
        RV::BigInt(b) => Value::bigint(b + 1),
        _ => unimplemented!(),
    })
}

fn integer_succ(
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
    let CallSiteInfo { dst, recv, .. } = *callsite;
    if let Some(i) = state.is_fixnum_literal(recv)
        && Value::is_i63(i.get() + 1)
    {
        state.def_C(dst, Immediate::check_fixnum(i.get() + 1).unwrap());
        return true;
    }

    let deopt = ir.new_deopt(state);
    state.load(ir, recv, GP::Rdi);
    ir.inline(move |r#gen, _, labels| {
        let deopt = &labels[deopt];
        monoasm! { &mut r#gen.jit,
            addq  rdi, 2;
            jo    deopt;
        }
    });
    if let Some(dst) = dst {
        state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    }
    true
}
*/

#[monoruby_builtin]
fn to_f(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Ok(Value::float(f))
}

fn integer_tof(
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
    let CallSiteInfo { dst, recv, .. } = *callsite;
    if let Some(ret) = dst {
        let fret = state.def_F(ret).enc();
        state.load(ir, recv, GP::Rdi);
        ir.inline(move |r#gen, _, _| {
            monoasm! { &mut r#gen.jit,
                sarq  rdi, 1;
                cvtsi2sdq xmm(fret), rdi;
            }
        });
    }
    true
}

///
/// ### Integer#to_i
///
/// - to_i -> self
/// - to_int -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_i.html]
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

/// ### Integer#eql?
///
/// - eql?(other) -> bool
///
/// Returns true if other is an Integer with the same value.
#[monoruby_builtin]
fn eql_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    if lhs.id() == rhs.id() {
        return Ok(Value::bool(true));
    }
    let res = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(a), RV::Fixnum(b)) => a == b,
        (RV::BigInt(a), RV::BigInt(b)) => a == b,
        (RV::Fixnum(_), RV::BigInt(_)) | (RV::BigInt(_), RV::Fixnum(_)) => false,
        _ => false,
    };
    Ok(Value::bool(res))
}

// Bitwise operations.

macro_rules! binop {
    ($op:ident, $op_str:expr) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
                let lhs = lfp.self_val();
                let rhs = lfp.arg(0);
                match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => Ok(Value::integer(lhs.$op(rhs))),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(BigInt::from(lhs).$op(rhs))),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Ok(Value::bigint(lhs.$op(BigInt::from(rhs)))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(lhs.$op(rhs))),
                    _ => {
                        // CRuby-compatible: call coerce, check result is Integer
                        let op_id = IdentId::get_id($op_str);
                        match crate::executor::op::binary_ops::try_coerce_and_apply_bit(vm, globals, op_id, lhs, rhs) {
                            Some(v) => Ok(v),
                            None => Err(vm.take_error()),
                        }
                    }
                }
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        binop!($op1, $op_str1);
        binop!($(($op2, $op_str2)),+);
    };
    (($op1:ident, $op_str1:expr)) => {
        binop!($op1, $op_str1);
    };
}

binop!((bitand, "&"), (bitor, "|"), (bitxor, "^"));

// Compare operations.

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let b = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(l), RV::Fixnum(r)) => l == r,
        (RV::Fixnum(l), RV::BigInt(r)) => BigInt::from(l) == *r,
        (RV::Fixnum(l), RV::Float(r)) => (l as f64) == r,
        (RV::BigInt(l), RV::Fixnum(r)) => *l == BigInt::from(r),
        (RV::BigInt(l), RV::BigInt(r)) => l == r,
        (RV::BigInt(l), RV::Float(r)) => l.to_f64().unwrap() == r,
        _ => {
            // Reverse dispatch: try rhs == lhs
            let eq_id = IdentId::get_id("==");
            let result = vm.invoke_method_inner(globals, eq_id, rhs, &[lhs], None, None)?;
            return Ok(Value::bool(result.as_bool()));
        }
    };
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.ne_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let ord = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.cmp(&rhs),
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).cmp(rhs),
        (RV::Fixnum(lhs), RV::Float(rhs)) => match (lhs as f64).partial_cmp(&rhs) {
            Some(ord) => ord,
            None => {
                return Ok(Value::nil());
            }
        },
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.cmp(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.cmp(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => match lhs.to_f64().unwrap().partial_cmp(&rhs) {
            Some(ord) => ord,
            None => {
                return Ok(Value::nil());
            }
        },
        _ => {
            // Try coerce protocol: call rhs.coerce(lhs), propagate exceptions
            let coerce_id = IdentId::get_id("coerce");
            match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                Ok(Some(result)) => {
                    if let Some(ary) = result.try_array_ty() {
                        if ary.len() == 2 {
                            let cmp_id = IdentId::get_id("<=>");
                            return vm.invoke_method_inner(globals, cmp_id, ary[0], &[ary[1]], None, None);
                        }
                    }
                    return Ok(Value::nil());
                }
                Ok(None) => return Ok(Value::nil()),
                Err(e) => return Err(e), // Propagate exception from coerce
            }
        }
    };
    Ok(Value::from_ord(ord))
}

macro_rules! cmpop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
                let lhs = lfp.self_val();
                let rhs = lfp.arg(0);
                match crate::executor::op::[<cmp_ $op _values>](vm, globals, lhs, rhs) {
                    Some(res) => Ok(res),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmpop!($op1);
        cmpop!($($op2),+);
    };
}

cmpop!(ge, gt, le, lt);

///
/// ### Integer#>>
///
/// - self >> bits -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e=3e.html]
#[monoruby_builtin]
fn shr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::shr_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_shr(
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
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    state.load(ir, recv, GP::Rdi);
    if let Some(rhs) = state.is_u8_literal(args) {
        ir.inline(move |r#gen, _, _| r#gen.gen_shr_imm(rhs));
    } else {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels| r#gen.gen_shr(&labels[deopt]));
    }
    state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    true
}

///
/// ### Integer#<<
///
/// - self << bits -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::shl_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_shl(
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
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    state.load(ir, recv, GP::Rdi);
    if let Some(rhs) = state.is_u8_literal(args)
        && rhs < 64
    {
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels| r#gen.gen_shl_rhs_imm(rhs, &labels[deopt]));
    } else if let Some(lhs) = state.is_fixnum_literal(recv) {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels| r#gen.gen_shl_lhs_imm(lhs.get(), &labels[deopt]));
    } else {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels| r#gen.gen_shl(&labels[deopt]));
    }
    state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    true
}

///
/// ### Integer#[]
///
/// self[nth] -> Integer
/// self[nth, len] -> Integer
/// self[range] -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(len_val) = lfp.try_arg(1) {
        // self[nth, len] form
        let nth = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        let len = len_val.coerce_to_int_i64(vm, globals)?;
        if len < 0 {
            // Negative length: ignore length, extract all bits from nth onward
            // Equivalent to n >> nth (or n << |nth| for negative nth)
            return match self_val.unpack() {
                RV::Fixnum(base) => {
                    if nth >= 0 {
                        if nth >= 64 {
                            Ok(Value::integer(if base < 0 { -1 } else { 0 }))
                        } else {
                            Ok(Value::integer(base >> nth))
                        }
                    } else {
                        Ok(Value::bigint(BigInt::from(base) << (-nth) as usize))
                    }
                }
                RV::BigInt(base) => {
                    if nth >= 0 {
                        Ok(Value::bigint(base >> nth as usize))
                    } else {
                        Ok(Value::bigint(base << (-nth) as usize))
                    }
                }
                _ => unreachable!(),
            };
        }
        match self_val.unpack() {
            RV::Fixnum(base) => {
                if nth < 0 {
                    // Negative nth: shift left by |nth|, then mask
                    let shifted = BigInt::from(base) << (-nth) as usize;
                    let mask = if len >= 64 {
                        (BigInt::from(1) << len as usize) - 1
                    } else {
                        BigInt::from((1i64 << len) - 1)
                    };
                    return Ok(Value::bigint(shifted & mask));
                }
                let shifted = if nth >= 64 {
                    if base < 0 { -1i64 } else { 0i64 }
                } else {
                    base >> nth
                };
                let mask = if len >= 64 { -1i64 } else { (1i64 << len) - 1 };
                Ok(Value::integer(shifted & mask))
            }
            RV::BigInt(base) => {
                let shifted = if nth >= 0 {
                    base >> nth as usize
                } else {
                    base << (-nth) as usize
                };
                // Limit mask size to avoid OOM on huge len values.
                // If len exceeds the bit length of shifted, masking is a no-op for non-negative values.
                let bits = shifted.bits() as i64;
                if len > bits + 1 {
                    // For non-negative shifted, all bits fit within len, so no masking needed.
                    // For negative shifted, extracting len bits gives the two's complement representation.
                    if shifted >= BigInt::ZERO {
                        Ok(Value::bigint(shifted))
                    } else {
                        let mask = (BigInt::from(1) << len as usize) - 1;
                        Ok(Value::bigint(shifted & mask))
                    }
                } else {
                    let mask = (BigInt::from(1) << len as usize) - 1;
                    Ok(Value::bigint(shifted & mask))
                }
            }
            _ => unreachable!(),
        }
    } else {
        let arg0 = lfp.arg(0);
        // Try to_int coercion for non-integer, non-bigint arguments
        let index = match arg0.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => arg0,
            _ => {
                if arg0.is_range().is_some() {
                    arg0
                } else {
                    // Coerce to Integer via to_int; allow BigInt results
                    arg0.coerce_to_int(vm, globals)?
                }
            }
        };
        op::integer_index1(vm, globals, self_val, index)
    }
}

///
/// ### Integer#even?
///
/// - even? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/even=3f.html]
#[monoruby_builtin]
fn even_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i % 2 == 0,
        RV::BigInt(b) => (b % 2u32).is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

///
/// ### Integer#odd?
///
/// - odd? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/odd=3f.html]
#[monoruby_builtin]
fn odd_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i % 2 != 0,
        RV::BigInt(b) => !(b % 2u32).is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

///
/// ### Numeric#nonzero?
///
/// - nonzero? -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/nonzero=3f.html]
#[monoruby_builtin]
fn nonzero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if match lfp.self_val().unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => unreachable!(),
    } {
        Ok(Value::nil())
    } else {
        Ok(lfp.self_val())
    }
}

///
/// ### Numeric#zero?
///
/// - zero? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/zero=3f.html]
#[monoruby_builtin]
fn zero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

///
/// ### Integer#abs
///
/// - abs -> Integer
///
/// Returns the absolute value of the integer.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/abs.html]
#[monoruby_builtin]
fn abs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match lfp.self_val().unpack() {
        RV::Fixnum(i) => match i.checked_neg() {
            Some(neg) if i < 0 => Value::integer(neg),
            None if i < 0 => Value::bigint(-BigInt::from(i)),
            _ => lfp.self_val(),
        },
        RV::BigInt(b) => {
            if b < &BigInt::ZERO {
                Value::bigint(-b)
            } else {
                lfp.self_val()
            }
        }
        _ => unreachable!(),
    })
}

///
/// ### Integer#size
///
/// - size -> Integer
///
/// Returns the number of bytes in the machine representation of the integer.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/size.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Fixnum(_) => Ok(Value::integer(std::mem::size_of::<i64>() as i64)),
        RV::BigInt(b) => {
            // CRuby returns the number of bytes for the unsigned magnitude
            let bits = b.bits();
            let bytes = ((bits + 7) / 8) as i64;
            // Minimum 1 byte for zero
            Ok(Value::integer(bytes.max(1)))
        }
        _ => unreachable!(),
    }
}

///
/// ### Integer#bit_length
///
/// - bit_length -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/bit_length.html]
#[monoruby_builtin]
fn bit_length(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => {
            let n = if i < 0 { !(i as u64) } else { i as u64 };
            Ok(Value::integer(64 - n.leading_zeros() as i64))
        }
        RV::BigInt(b) => {
            use num::traits::sign::Signed;
            let bits = if b.is_negative() {
                // For negative numbers, bit_length = bits(~n) = bits(-n - 1)
                let complement: BigInt = -(b) - 1;
                complement.bits()
            } else {
                b.bits()
            };
            Ok(Value::integer(bits as i64))
        }
        _ => unreachable!(),
    }
}

///
/// ### Integer#to_s
///
/// - to_s(base = 10) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_s.html]
#[monoruby_builtin]
fn to_s(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let base = if let Some(b) = lfp.try_arg(0) {
        let b = b.coerce_to_int_i64(vm, globals)?;
        if !(2..=36).contains(&b) {
            return Err(MonorubyErr::argumenterr(format!("invalid radix {}", b)));
        }
        b as u32
    } else {
        10
    };
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => {
            if base == 10 {
                Ok(Value::string(format!("{}", i)))
            } else {
                let negative = i < 0;
                let abs = if negative {
                    (i as i128).unsigned_abs()
                } else {
                    i as u128
                };
                let s = format_integer_base(abs, base);
                if negative {
                    Ok(Value::string(format!("-{}", s)))
                } else {
                    Ok(Value::string(s))
                }
            }
        }
        RV::BigInt(b) => {
            if base == 10 {
                Ok(Value::string(format!("{}", b)))
            } else {
                use num::traits::sign::Signed;
                let negative = b.is_negative();
                let abs = if negative { -b } else { b.clone() };
                let s = format_bigint_base(&abs, base);
                if negative {
                    Ok(Value::string(format!("-{}", s)))
                } else {
                    Ok(Value::string(s))
                }
            }
        }
        _ => unreachable!(),
    }
}

fn format_integer_base(mut n: u128, base: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let base = base as u128;
    let mut buf = Vec::new();
    while n > 0 {
        buf.push(DIGITS[(n % base) as usize]);
        n /= base;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

fn format_bigint_base(n: &BigInt, base: u32) -> String {
    use num::Zero;
    if n.is_zero() {
        return "0".to_string();
    }
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let base_big = BigInt::from(base);
    let mut buf = Vec::new();
    let mut val = n.clone();
    while !val.is_zero() {
        let rem = &val % &base_big;
        buf.push(DIGITS[rem.to_usize().unwrap()]);
        val /= &base_big;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn times() {
        run_test(
            r##"
        a = 0
        5.times do |z|
          b = 0
          7.times do |y|
            10.times do |x|
              a += x
              b += x + y
            end
            a += y
          end
          a += z
          a -= b
        end
        a
        "##,
        );
    }

    #[test]
    fn step() {
        run_test(
            r##"
        a = 0
        x = 0.step(10, 2) do |z|
          b = 0
          3.step(12, 3) do |y|
            5.step(25, 5) do |x|
              a += x
              b += x + y
            end
            a += y
          end
          a += z
          a -= b
        end
        [a, x]
        "##,
        );
    }

    #[test]
    fn upto() {
        run_test(
            r##"
        a = 0
        x = 0.upto(10) do |z|
          b = 0
          13.step(12) do |y|
            15.step(25) do |x|
              a += x
              b += x + y
            end
            a += y
          end
          a += z
          a -= b
        end
        [a, x]
        "##,
        );
        run_test(
            r##"
        res = 0
        10.upto(8) do |z|
          res += z
        end
        res
        "##,
        );
    }

    #[test]
    fn downto() {
        run_test(
            r##"
        a = 0
        x = 10.downto(0) do |z|
          b = 0
          13.step(12) do |y|
            15.step(25) do |x|
              a += x
              b += x + y
            end
            a += y
          end
          a += z
          a -= b
        end
        [a, x]
        "##,
        );
        run_test(
            r##"
        res = 0
        8.downto(10) do |z|
          res += z
        end
        res
        "##,
        );
    }

    #[test]
    fn succ() {
        run_test("253.succ.succ.succ");
        run_test_with_prelude("$a.succ.succ.succ", "$a = 100");
    }

    #[test]
    fn to_f() {
        run_test("253.to_f");
        run_test("-25253.to_f");
        run_test("0.to_f");
    }

    #[test]
    fn to_i() {
        run_test("253.to_i");
        run_test("-25253.to_i");
        run_test(
            "8364942539529902342420345356709767546464574458647864843346254643534645647575786.to_i",
        );
    }

    #[test]
    fn non_zero() {
        run_test("253.nonzero?");
        run_test("0.nonzero?");
        run_test("253.zero?");
        run_test("0.zero?");
    }

    #[test]
    fn index() {
        run_test("999999999999999999999999999999999[-100]");
        run_test("999999999999999999999999999999999[0]");
        run_test("999999999999999999999999999999999[47]");
        run_test("999999999999999999999999999999999[48]");
        run_test("999999999999999999999999999999999[82]");
        run_test("999999999999999999999999999999999[85]");
        run_test("999999999999999999999999999999999[86]");
        run_test("999999999999999999999999999999999[999999999999999999999999999999999]");
        run_test("27[-2]");
        run_test("27[0]");
        run_test("27[2]");
        run_test("27[200]");
        run_test("27[2000000000000000000000000000000000000000000]");
        // Integer#[Range]
        run_test("0b11110000[4..6]");
        run_test("0b11110000[4...7]");
        run_test("0b11110000[-4..6]");
        run_test("0b11110000[-4..-6]");
        run_test("0b11110000[4..-6]");
        run_test("0b11110000[6..4]");
        run_test("0b11110000[7...4]");
        run_test("0b11110000[6..-4]");
        run_test("0b11110000[-6..-4]");
        run_test("0b11110000[-6..4]");
        run_test("0xFF[0..3]");
        run_test("0xFF[4..7]");
        run_test("255[0..7]");
        run_test("0b11110000[0..3]");
        run_test("123[0..0]");
        run_test("123[0...0]");
        run_test("999999999999999999999999999999999[0..31]");
        run_test("999999999999999999999999999999999[-10..31]");
        run_test("999999999999999999999999999999999[-10..-31]");
        run_test("999999999999999999999999999999999[0..-31]");
        run_test("999999999999999999999999999999999[32..63]");
        run_test("999999999999999999999999999999999[63..32]");
        run_test("999999999999999999999999999999999[63..-32]");
        run_test("999999999999999999999999999999999[-63..-32]");
        run_test("999999999999999999999999999999999[-63..32]");
        run_test("999999999999999999999999999999999[64..95]");
        // Negative self (Fixnum)
        run_test("-1[0..7]");
        run_test("-1[0..63]");
        run_test("-1[0...64]");
        run_test("-1[4..7]");
        run_test("-1[-4..7]");
        run_test("-1[-4..-6]");
        run_test("-1[4..-6]");
        run_test("-256[0..7]");
        run_test("-256[8..15]");
        run_test("-256[-4..11]");
        run_test("-127[0..7]");
        run_test("-127[4..11]");
        run_test("-127[0..0]");
        run_test("-127[0...0]");
        run_test("-2[0..63]");
        run_test("-2[63..32]");
        // Negative self (BigInt)
        run_test("-999999999999999999999999999999999[0..31]");
        run_test("-999999999999999999999999999999999[32..63]");
        run_test("-999999999999999999999999999999999[64..95]");
        run_test("-999999999999999999999999999999999[-10..31]");
        run_test("-999999999999999999999999999999999[-10..-31]");
        run_test("-999999999999999999999999999999999[63..32]");
        run_test("-999999999999999999999999999999999[-63..32]");
    }

    #[test]
    fn even_() {
        run_test("100.even?");
        run_test("-100.even?");
        run_test(
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?",
        );
        run_test(
            "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?",
        );
        run_test("100.odd?");
        run_test("-100.odd?");
        run_test(
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?",
        );
        run_test(
            "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?",
        );
    }

    #[test]
    fn cmp() {
        run_test("100.send(:==, 100)");
        run_test("100.send(:!=, 100)");
        run_test("100.send(:>=, 100)");
        run_test("100.send(:<=, 100)");

        run_test("100.==(100)");
        run_test("100.==(50)");
        run_test("100.==(100.0)");
        run_test(r#"100.==("100")"#);
        run_test("100.!=(100)");
        run_test("100.!=(50)");
        run_test("100.!=(100.0)");
        run_test(r#"100.!=("100")"#);
    }

    #[test]
    fn digits() {
        run_test("100.digits");
        run_test("100.digits(10)");
        run_test("100.digits(16)");
        run_test("100.digits(16.5)");
        run_test("class C; def to_int; 10; end; end; 100.digits(C.new)");

        run_test_error("(-100).digits(16)");
        run_test_error("-100.digits(16)");
        run_test_error("100.digits(-16)");
        run_test_error("100.digits(0)");
        run_test_error("100.digits(1)");
    }

    #[test]
    fn integer_abs() {
        run_test("42.abs");
        run_test("(-42).abs");
        run_test("0.abs");
        run_test("42.magnitude");
        run_test("(-42).magnitude");
    }

    #[test]
    fn integer_negative() {
        run_test("42.negative?");
        run_test("(-42).negative?");
        run_test("0.negative?");
        run_test("42.positive?");
        run_test("(-42).positive?");
        run_test("0.positive?");
    }

    #[test]
    fn integer_integer() {
        run_test("42.integer?");
        run_test("0.integer?");
    }

    #[test]
    fn integer_ord() {
        run_test("42.ord");
        run_test("0.ord");
    }

    #[test]
    fn integer_ceil() {
        run_test("42.ceil");
        run_test("42.ceil(0)");
        run_test("42.ceil(-1)");
        run_test("45.ceil(-1)");
        run_test("(-42).ceil(-1)");
        run_test("123.ceil(-2)");
    }

    #[test]
    fn integer_round() {
        run_test("42.round");
        run_test("42.round(0)");
        run_test("42.round(-1)");
        run_test("45.round(-1)");
        run_test("(-42).round(-1)");
        run_test("15.round(-1)");
        run_test("25.round(-1)");
        run_test("35.round(-1)");
    }

    #[test]
    fn integer_truncate() {
        run_test("42.truncate");
        run_test("42.truncate(0)");
        run_test("42.truncate(-1)");
        run_test("(-42).truncate(-1)");
        run_test("123.truncate(-2)");
    }

    #[test]
    fn integer_next_pred() {
        run_test("42.next");
        run_test("(-1).next");
        run_test("42.pred");
        run_test("0.pred");
    }

    #[test]
    fn integer_remainder() {
        run_test("5.remainder(3)");
        run_test("(-5).remainder(3)");
        run_test("5.remainder(-3)");
        run_test("(-5).remainder(-3)");
    }

    #[test]
    fn integer_fdiv() {
        run_test("1.fdiv(2)");
        run_test("(-1).fdiv(2)");
        run_test("1.fdiv(2.0)");
    }

    #[test]
    fn integer_gcd_lcm() {
        run_test("12.gcd(8)");
        run_test("12.gcd(-8)");
        run_test("(-12).gcd(8)");
        run_test("0.gcd(5)");
        run_test("5.gcd(0)");
        run_test("12.lcm(8)");
        run_test("12.lcm(-8)");
        run_test("0.lcm(5)");
        run_test("12.gcdlcm(8)");
    }

    #[test]
    fn integer_pow() {
        run_test("2.pow(10)");
        run_test("2.pow(10, 1000)");
        run_test("2.pow(0)");
    }

    #[test]
    fn integer_allbits() {
        run_test("0b1010.allbits?(0b1010)");
        run_test("0b1010.allbits?(0b1000)");
        run_test("0b1010.allbits?(0b1011)");
        run_test("0b1010.anybits?(0b0001)");
        run_test("0b1010.anybits?(0b1001)");
        run_test("0b1010.nobits?(0b0101)");
        run_test("0b1010.nobits?(0b0001)");
    }

    #[test]
    fn integer_bit_length() {
        run_test("0.bit_length");
        run_test("1.bit_length");
        run_test("255.bit_length");
        run_test("256.bit_length");
        run_test("(-1).bit_length");
        run_test("(-256).bit_length");
        run_test("(-257).bit_length");
        run_test("(2**100).bit_length");
        run_test("(-(2**100)).bit_length");
    }

    #[test]
    fn integer_coerce() {
        run_test("1.coerce(2)");
        run_test("1.coerce(2.5)");
    }

    #[test]
    fn integer_numerator_denominator() {
        run_test("42.numerator");
        run_test("42.denominator");
        run_test("0.numerator");
        run_test("0.denominator");
    }

    #[test]
    fn integer_sqrt() {
        run_test("Integer.sqrt(0)");
        run_test("Integer.sqrt(1)");
        run_test("Integer.sqrt(4)");
        run_test("Integer.sqrt(9)");
        run_test("Integer.sqrt(10)");
        run_test("Integer.sqrt(100)");
    }

    #[test]
    fn integer_try_convert() {
        run_test("Integer.try_convert(1)");
        run_test("Integer.try_convert(nil)");
        run_test(r#"Integer.try_convert("1")"#);
    }

    #[test]
    fn positive_negative() {
        run_test("42.positive?");
        run_test("(-42).positive?");
        run_test("0.positive?");
        run_test("42.negative?");
        run_test("(-42).negative?");
        run_test("0.negative?");
    }

    #[test]
    fn bop_redefinition() {
        // Verify that redefining Integer#+ does not crash the JIT.
        // Uses run_test_no_result_check because BOP redefinition
        // affects global state and the result may differ from CRuby.
        run_test_no_result_check(
            r##"
            res = []
            res << (1 + 2)
            class Integer
              def +(other)
                self - other
              end
            end
            res << (1 + 2)
            class Integer
              remove_method :+
            end
            res << (1 + 2)
            res
            "##,
        );
    }

    #[test]
    fn integer_to_r() {
        run_test_once("3.respond_to?(:to_r)");
        run_test_once("3.respond_to?(:rationalize)");
    }

    #[test]
    fn bitwise_type_error() {
        // Non-integer arguments raise TypeError
        run_test_error("5 | 'a'");
        run_test_error("5 & 'a'");
        run_test_error("5 ^ 'a'");
        run_test_error("5 | 1.5");
        run_test_error("5 & nil");
    }

    #[test]
    fn to_s_with_base() {
        run_test("255.to_s(16)");
        run_test("(-255).to_s(16)");
        run_test("10.to_s(2)");
        run_test("0.to_s(16)");
        run_test("123.to_s");
        run_test("255.to_s(36)");
    }

    #[test]
    fn chr_with_encoding() {
        run_test("65.chr");
        run_test("97.chr");
    }

    #[test]
    fn index_with_length() {
        run_test("42[0, 3]");
        run_test("0b11010[1, 3]");
        run_test("255[4, 4]");
    }

    #[test]
    fn integer_eql() {
        run_test("(2**64).eql?(2**64)");
        run_test("1.eql?(1)");
        run_test("1.eql?(1.0)");
        run_test("(2**64).eql?(2**64 + 1)");
    }

    #[test]
    fn pow_bignum_exponent() {
        run_test("1 ** (2**100)");
        run_test("(-1) ** (2**100)");
        run_test("(-1) ** (2**100 + 1)");
        run_test_error("2 ** (2**100)");
    }

    #[test]
    fn shift_to_int() {
        run_test_once(
            r#"
            class Foo; def to_int; 2; end; end
            [8 << Foo.new, 8 >> Foo.new]
        "#,
        );
    }

    #[test]
    fn bitwise_float_typeerror() {
        run_test_error("1 & 3.0");
        run_test_error("1 | 3.0");
        run_test_error("1 ^ 3.0");
    }

    #[test]
    fn fdiv_fixnum() {
        run_test("100.fdiv(3)");
        run_test("10.fdiv(2)");
    }

    #[test]
    fn divmod_nan() {
        run_test_error("1.divmod(Float::NAN)");
    }

    #[test]
    fn div_coerce() {
        run_test("10 / 3.0");
        run_test("10 / 2.5");
    }

    #[test]
    fn modulo_float_zero() {
        run_test_error("10 % 0.0");
    }

    #[test]
    fn eq_reverse_dispatch() {
        run_test_once(
            r#"
            class Foo; def ==(other); other == 42; end; end
            [42 == Foo.new, 42 === Foo.new, (2**64) == Foo.new, 42 == 99]
            "#,
        );
    }

    #[test]
    fn integer_dup() {
        run_test("42.dup");
        run_test("42.dup.equal?(42)");
        run_test_once("a = 2**100; a.dup.equal?(a)");
    }

    #[test]
    fn integer_round_errors() {
        run_test_error("42.round(Float::INFINITY)");
        run_test_error("42.round(Float::NAN)");
        run_test_error("42.round('string')");
    }

    #[test]
    fn integer_floor_errors() {
        run_test_error("42.floor(Float::INFINITY)");
        run_test_error("42.floor(Float::NAN)");
        run_test_error("42.floor('string')");
    }

    #[test]
    fn integer_ceil_errors() {
        run_test_error("42.ceil(Float::INFINITY)");
        run_test_error("42.ceil(Float::NAN)");
        run_test_error("42.ceil('string')");
    }

    #[test]
    fn integer_truncate_errors() {
        run_test_error("42.truncate(Float::INFINITY)");
        run_test_error("42.truncate(Float::NAN)");
        run_test_error("42.truncate('string')");
    }

    #[test]
    fn integer_round_float_ndigits() {
        run_test("42.round(1.5)");
        run_test("42.round(-1.5)");
    }

    #[test]
    fn integer_coerce_extended() {
        run_test_error(r#"1.coerce("")"#);
        run_test_once(
            r#"
            class Foo; def to_f; 99.5; end; end
            1.coerce(Foo.new)
        "#,
        );
    }

    #[test]
    fn integer_chr_utf8() {
        run_test(r#"256.chr("UTF-8")"#);
        run_test(r#"900.chr("UTF-8").bytes"#);
        run_test(r#"128.chr("UTF-8").bytes"#);
        run_test_error(r#"(-1).chr"#);
    }

    #[test]
    fn integer_index_negative() {
        run_test("0b11010[-2, 3]");
        run_test("0b11010[1, 3]");
    }

    #[test]
    fn integer_index_endless_range() {
        run_test("0b11010[1..]");
        run_test("255[4..]");
        run_test("255[70..]");
        run_test("255[-2..]");
        run_test("123456789123456789123456789123456789123456789[70..]");
        run_test("123456789123456789123456789123456789123456789[-3..]");

        run_test_error("255[Float::NAN..]");
        run_test_error("255[Float::INFINITY..]");
        run_test_error("255[Float::-INFINITY..]");

        run_test_error("255[..4]");
        run_test_error("255[..70]");
        run_test("255[..-2]");
        run_test_error("123456789123456789123456789123456789123456789[..70]");
        run_test("123456789123456789123456789123456789123456789[..-3]");
    }

    #[test]
    fn integer_round_half() {
        run_test("25.round(-1, half: :up)");
        run_test("25.round(-1, half: :down)");
        run_test("25.round(-1, half: :even)");
        run_test("15.round(-1, half: :even)");
    }

    #[test]
    fn integer_allbits_typeerror() {
        run_test_error(r#"42.allbits?("foo")"#);
        run_test_error(r#"42.anybits?("foo")"#);
        run_test_error(r#"42.nobits?("foo")"#);
    }

    #[test]
    fn integer_abs_bigint() {
        run_test("(-(2**64)).abs");
        run_test("(2**64).abs");
        run_test("(-42).abs");
    }

    #[test]
    fn integer_neg_boundary() {
        run_test("-(2**62)");
    }

    #[test]
    fn integer_div() {
        run_test("7.div(3)");
        run_test("(-7).div(3)");
        run_test("7.div(-3)");
        run_test("7.div(3.0)");
        run_test("5.div(Rational(3))");
        run_test_error("7.div(0)");
        run_test_error("7.div(0.0)");
    }

    #[test]
    fn integer_ceildiv() {
        run_test("7.ceildiv(3)");
        run_test("(-7).ceildiv(3)");
        run_test("7.ceildiv(-3)");
        run_test_error("7.ceildiv(0)");
    }

    #[test]
    fn pow_zero_neg() {
        run_test_error("0 ** -1");
        run_test_error("0 ** -2");
    }

    #[test]
    fn integer_size_bigint() {
        run_test("(2**64).size");
        run_test("(2**63).size");
    }

    #[test]
    fn integer_cmp_coerce() {
        run_test_once(r#"
            class Foo
              def coerce(other)
                [other.to_f, 42.0]
              end
            end
            [1 <=> Foo.new, 100 <=> Foo.new]
        "#);
    }

    #[test]
    fn downto_upto_bad_arg() {
        run_test_error(r#"1.upto("foo") {}"#);
        run_test_error(r#"1.downto("foo") {}"#);
    }

    #[test]
    fn fdiv_large_bigint() {
        run_test_once("(10**200).fdiv(10**199)");
        run_test_once("(10**200).fdiv(3 * 10**199)");
    }

    #[test]
    fn integer_sqrt_newton() {
        run_test("Integer.sqrt(0)");
        run_test("Integer.sqrt(1)");
        run_test("Integer.sqrt(100)");
        run_test_error("Integer.sqrt(-1)");
        run_test_error(r#"Integer.sqrt("foo")"#);
    }

    #[test]
    fn round_negative_precision() {
        run_test("249.round(-2)");
        run_test("(-249).round(-2)");
        run_test("150.round(-2)");
    }

    #[test]
    fn floor_bigint_neg_precision() {
        run_test("(-130).floor(-1)");
        run_test("(-131).floor(-1)");
    }

    #[test]
    fn module_include_q() {
        run_test_once("Integer.include?(Comparable)");
        run_test_once("Integer.include?(Enumerable)");
    }

    #[test]
    fn rational_pow_zero_base() {
        run_test_error("Rational(0, 1) ** -1");
        run_test_error("Rational(0, 1) ** Rational(-1, 1)");
    }
}
