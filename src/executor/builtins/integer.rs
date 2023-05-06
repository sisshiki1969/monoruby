use num::ToPrimitive;

use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    globals.define_builtin_func(INTEGER_CLASS, "step", step, -1);
    globals.define_builtin_func_inlinable(INTEGER_CLASS, "to_f", to_f, 0, InlineMethod::IntegerTof);
    globals.define_builtin_func(INTEGER_CLASS, "to_i", to_i, 0);
    globals.define_builtin_func(INTEGER_CLASS, "to_int", to_i, 0);
    globals.define_builtin_func(INTEGER_CLASS, "+", add, 1);
}

/// ### Integer#times
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
fn times(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    let count = match lfp.self_val().try_fixnum() {
        Some(i) => i,
        None => unimplemented!(),
    };
    if let Some(b) = lfp.block() {
        let iter = (0..count).map(|i| Value::new_integer(i));
        vm.invoke_block_iter1(globals, b, iter)?;
    } else {
        unimplemented!("needs block.")
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
            let v = Value::new_integer(self.cur);
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
            let v = Value::new_integer(self.cur);
            self.cur += self.step;
            Some(v)
        }
    }
}

/// ### Integer#step
/// - step(limit, step = 1) {|n| ... } -> self[permalink][rdoc][edit]
/// [NOT SUPPORTED]- step(limit, step = 1) -> Enumerator
/// [NOT SUPPORTED]- step(limit, step = 1) -> Enumerator::ArithmeticSequence
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/step.html]
fn step(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    args: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 1..=2)?;
    let block = match lfp.block() {
        None => {
            /*let id = IdentId::get_ident_id("step");
            let val = vm.create_enumerator(id, self_val, args.into(vm))?;
            return Ok(val);*/
            unimplemented!()
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().as_fixnum();
    let limit = args[0].coerce_to_fixnum(globals)?;
    let step = if len == 2 {
        let step = args[1].coerce_to_fixnum(globals)?;
        if step == 0 {
            return Err(MonorubyErr::argumenterr("Step can not be 0.".to_string()));
        }
        step
    } else {
        1
    };

    if step > 0 {
        let iter = PosStep { cur, step, limit };
        vm.invoke_block_iter1(globals, block, iter)?;
    } else {
        let iter = NegStep { cur, step, limit };
        vm.invoke_block_iter1(globals, block, iter)?;
    }
    Ok(lfp.self_val())
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
fn chr(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    if let Some(i) = lfp.self_val().try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Ok(Value::new_string_from_slice(&[b]));
        }
    };
    Err(MonorubyErr::char_out_of_range(globals, lfp.self_val()))
}

fn to_f(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let f = match lfp.self_val().unpack() {
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Ok(Value::new_float(f))
}

///
/// ### Integer#to_i
///
/// - to_i -> self
/// - to_int -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_i.html]
fn to_i(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Integer#+
///
/// - self + other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2b.html]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    Executor::check_number_of_arguments(len, 1..=1)?;
    match super::op::add_values(vm, globals, lfp.self_val(), arg[0]) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error().unwrap();
            Err(err)
        }
    }
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
}
