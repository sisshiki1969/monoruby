use super::*;
use jitgen::JitContext;
use num::{BigInt, ToPrimitive, Zero};
use std::ops::{BitAnd, BitOr, BitXor};

//
// Integer class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class("Integer", INTEGER_CLASS, numeric, OBJECT_CLASS, None);
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_inline_func(INTEGER_CLASS, "succ", succ, Box::new(integer_succ), 0);
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
    globals.define_builtin_func(INTEGER_CLASS, "[]", index, 1);
    globals.define_builtin_func(INTEGER_CLASS, "even?", even_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "odd?", odd_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "nonzero?", nonzero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "zero?", zero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "size", size, 0);
    globals.define_builtin_func(INTEGER_CLASS, "bit_length", bit_length, 0);
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
    let limit = lfp.arg(0).coerce_to_i64(globals)?;
    let step = if let Some(arg1) = lfp.try_arg(1) {
        let step = arg1.coerce_to_i64(globals)?;
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
    let limit = lfp.arg(0).coerce_to_i64(globals)?;
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
    let limit = lfp.arg(0).coerce_to_i64(globals)?;
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
/// - [NOT SUPPORTED] chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
#[monoruby_builtin]
fn chr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(i) = lfp.self_val().try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Ok(Value::bytes_from_slice(&[b]));
        }
    };
    Err(MonorubyErr::char_out_of_range(
        &globals.store,
        lfp.self_val(),
    ))
}

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
                        // Try coerce protocol
                        let coerce_id = IdentId::get_id("coerce");
                        if let Some(result) = vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None)? {
                            if let Some(ary) = result.try_array_ty() {
                                if ary.len() == 2 {
                                    let op_id = IdentId::get_id($op_str);
                                    return vm.invoke_method_inner(globals, op_id, ary[0], &[ary[1]], None, None);
                                }
                            }
                            return Err(MonorubyErr::typeerr("coerce must return [x, y]".to_string()));
                        }
                        lfp.arg(0).coerce_to_i64(globals)?;
                        unreachable!();
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
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
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
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
            return Ok(Value::nil());
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
/// NOT SUPPORTED: self[nth, len] -> Integer
/// NOT SUPPORTED: self[range] -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=5b=5d.html]
#[monoruby_builtin]
fn index(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    op::integer_index1(globals, self_val, lfp.arg(0))
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
fn nonzero_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
            let bytes = b.to_signed_bytes_le().len();
            // Round up to the nearest machine word size
            let word_size = std::mem::size_of::<i64>();
            let size = ((bytes + word_size - 1) / word_size) * word_size;
            Ok(Value::integer(size as i64))
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
}
