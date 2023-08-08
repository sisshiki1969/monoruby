use super::*;
use num::{ToPrimitive, Zero};

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Integer", INTEGER_CLASS);
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    globals.define_builtin_func(INTEGER_CLASS, "step", step, -1);
    globals.define_builtin_inline_func(
        INTEGER_CLASS,
        "to_f",
        to_f,
        0,
        integer_tof,
        analysis_integer_tof,
    );
    globals.define_builtin_func(INTEGER_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(INTEGER_CLASS, "to_int", to_i, 0);
    globals.define_builtin_func(INTEGER_CLASS, "+", add, 1);
    globals.define_builtin_func(INTEGER_CLASS, "[]", index, 1);
    globals.define_builtin_func(INTEGER_CLASS, "even?", even_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "odd?", odd_, 0);
}

///
/// ### Integer#times
///
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
#[monoruby_builtin]
fn times(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
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
fn step(vm: &mut Executor, globals: &mut Globals, lfp: LFP, args: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("step");
            let e = vm.generate_enumerator(globals, id)?;
            /*let en: Enumerator = e.into();
            eprintln!(
                "step {:016x} self: {} obj: {}",
                e.get(),
                globals.to_s(en.proc.self_val()),
                globals.to_s(en.obj)
            );*/
            return Ok(e);
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().as_fixnum();
    let limit = args[0].coerce_to_i64(globals)?;
    let step = if len == 2 {
        let step = args[1].coerce_to_i64(globals)?;
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

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
#[monoruby_builtin]
fn chr(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    if let Some(i) = lfp.self_val().try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Ok(Value::string_from_slice(&[b]));
        }
    };
    Err(MonorubyErr::char_out_of_range(globals, lfp.self_val()))
}

#[monoruby_builtin]
fn to_f(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let f = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Ok(Value::float(f))
}

///
/// ### Integer#to_i
///
/// - to_i -> self
/// - to_int -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_i.html]
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Integer#+
///
/// - self + other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2b.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 1)?;
    match super::op::add_values(vm, globals, lfp.self_val(), arg[0]) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
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
fn index(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let self_val = lfp.self_val();
    op::integer_index1(globals, self_val, arg[0])
}

fn integer_tof(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    pc: BcPc,
    deopt: DestLabel,
) {
    let MethodInfo { recv, ret, .. } = method_info;
    gen.load_rdi(*recv);
    if !recv.is_zero() {
        gen.guard_class(pc.class_version().0, deopt);
    }
    let fret = ctx.xmm_write_enc(*ret);
    monoasm!( &mut gen.jit,
        sarq  rdi, 1;
        cvtsi2sdq xmm(fret), rdi;
    );
}

fn analysis_integer_tof(info: &mut SlotInfo, method_info: &MethodInfo) {
    info.use_non_float(method_info.recv);
    info.def_as(method_info.ret, true);
}

///
/// ### Integer#even?
///
/// - even? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/even=3f.html]
#[monoruby_builtin]
fn even_(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 0)?;
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
fn odd_(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i % 2 != 0,
        RV::BigInt(b) => !(b % 2u32).is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
}
