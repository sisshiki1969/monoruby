use super::*;
use num::{BigInt, ToPrimitive, Zero};
use std::ops::{BitAnd, BitOr, BitXor};

//
// Integer class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class_by_str("Integer", INTEGER_CLASS, numeric, OBJECT_CLASS);
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    globals.define_builtin_func_with(INTEGER_CLASS, "step", step, 1, 2, false);
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
}

///
/// ### Integer#times
///
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
#[monoruby_builtin]
fn times(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => vm.invoke_block_iter1(globals, bh, (0..i).map(Value::integer))?,
        RV::BigInt(_) => unimplemented!(),
        _ => unreachable!(),
    };

    Ok(lfp.self_val())
}

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

///
/// ### Integer#step
///
/// - step(limit, step = 1) {|n| ... } -> self
/// - step(limit, step = 1) -> Enumerator
/// - [NOT SUPPORTED] step(limit, step = 1) -> Enumerator::ArithmeticSequence
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/step.html]
#[monoruby_builtin]
fn step(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("step");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer()?;
    let limit = lfp.arg(0).coerce_to_i64()?;
    let step = if let Some(arg1) = lfp.try_arg(1) {
        let step = arg1.coerce_to_i64()?;
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
}

///
/// ### Integer#upto
///
/// - upto(max) {|n| ... } -> Integer
/// - upto(max) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/upto.html]
#[monoruby_builtin]
fn upto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("upto");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer()?;
    let limit = lfp.arg(0).coerce_to_i64()?;
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
fn downto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("downto");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer()?;
    let limit = lfp.arg(0).coerce_to_i64()?;
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
fn chr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(i) = lfp.self_val().try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Ok(Value::bytes_from_slice(&[b]));
        }
    };
    Err(MonorubyErr::char_out_of_range(globals, lfp.self_val()))
}

#[monoruby_builtin]
fn to_f(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let f = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Ok(Value::float(f))
}

fn integer_tof(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    _pc: BytecodePtr,
) {
    let CallSiteInfo { dst, .. } = store[callid];
    if let Some(ret) = dst {
        let fret = ir.xmm_write_enc(bb, ret);
        ir.inline(move |gen, _| {
            monoasm! { &mut gen.jit,
                sarq  rdi, 1;
                cvtsi2sdq xmm(fret), rdi;
            }
        });
    }
}

///
/// ### Integer#to_i
///
/// - to_i -> self
/// - to_int -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_i.html]
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}

// Bitwise operations.

macro_rules! binop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
                let lhs = lfp.self_val();
                let rhs = lfp.arg(0);
                match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => Ok(Value::integer(lhs.$op(rhs))),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(BigInt::from(lhs).$op(rhs))),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Ok(Value::bigint(lhs.$op(BigInt::from(rhs)))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(lhs.$op(rhs))),
                    _ => {
                        lfp.arg(0).coerce_to_i64()?;
                        unreachable!();
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop!($op1);
        binop!($($op2),+);
    };
}

binop!(bitand, bitor, bitxor);

// Compare operations.

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = vm.eq_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn shr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    super::op::shr_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_shr(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BytecodePtr,
) {
    let CallSiteInfo { dst, args, .. } = store[callid];
    let deopt = ir.new_deopt(bb, pc);
    if let Some(rhs) = bb.is_u8_literal(args) {
        ir.inline(move |gen, _| gen.gen_shr_imm(rhs));
    } else {
        bb.fetch_guard_fixnum(ir, args, GP::Rsi, deopt);
        ir.inline(move |gen, labels| gen.gen_shr(labels[deopt]));
    }
    bb.reg2acc_fixnum(ir, GP::Rdi, dst);
}

///
/// ### Integer#<<
///
/// - self << bits -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    super::op::shl_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_shl(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BytecodePtr,
) {
    let CallSiteInfo { dst, args, .. } = store[callid];
    let deopt = ir.new_deopt(bb, pc);
    if let Some(rhs) = bb.is_u8_literal(args)
        && rhs < 64
    {
        ir.inline(move |gen, labels| gen.gen_shl_imm(rhs, labels[deopt]));
    } else {
        bb.fetch_guard_fixnum(ir, args, GP::Rsi, deopt);
        ir.inline(move |gen, labels| gen.gen_shl(labels[deopt]));
    }
    bb.reg2acc_fixnum(ir, GP::Rdi, dst);
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
fn index(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    op::integer_index1(self_val, lfp.arg(0))
}

///
/// ### Integer#even?
///
/// - even? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/even=3f.html]
#[monoruby_builtin]
fn even_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn odd_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn nonzero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn zero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
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
    }

    #[test]
    fn even_() {
        run_test("100.even?");
        run_test("-100.even?");
        run_test("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?");
        run_test("-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?");
        run_test("100.odd?");
        run_test("-100.odd?");
        run_test("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?");
        run_test("-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?");
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
}
