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
}

/// ### Integer#times
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
extern "C" fn times(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _: Arg,
    _: usize,
    block: Option<BlockHandler>,
) -> Option<Value> {
    let count = match self_val.try_fixnum() {
        Some(i) => i,
        None => unimplemented!(),
    };
    if let Some(block) = block {
        let data = vm.get_block_data(globals, block);
        for i in 0..count {
            vm.invoke_block(globals, data.clone(), &[Value::new_integer(i)])?;
        }
    } else {
        unimplemented!("needs block.")
    };

    Some(self_val)
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
extern "C" fn step(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    args: Arg,
    len: usize,
    block: Option<BlockHandler>,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 1..=2)?;
    let block = match block {
        None => {
            /*let id = IdentId::get_ident_id("step");
            let val = vm.create_enumerator(id, self_val, args.into(vm))?;
            return Ok(val);*/
            unimplemented!()
        }
        Some(block) => block,
    };
    let cur = self_val.as_fixnum();
    let limit = args[0].coerce_to_fixnum(globals)?;
    let step = if len == 2 {
        let step = args[1].coerce_to_fixnum(globals)?;
        if step == 0 {
            globals.err_argument("Step can not be 0.");
            return None;
        }
        step
    } else {
        1
    };

    let data = vm.get_block_data(globals, block);
    if step > 0 {
        let iter = PosStep { cur, step, limit };
        for i in iter {
            vm.invoke_block(globals, data.clone(), &[i])?;
        }
    } else {
        let iter = NegStep { cur, step, limit };
        for i in iter {
            vm.invoke_block(globals, data.clone(), &[i])?;
        }
    }
    Some(self_val)
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
extern "C" fn chr(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    if let Some(i) = self_val.try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Some(Value::new_string_from_slice(&[b]));
        }
    };
    globals.err_char_out_of_range(self_val);
    None
}

extern "C" fn to_f(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let f = match self_val.unpack() {
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Some(Value::new_float(f))
}

///
/// ### Integer#to_i
///
/// - to_i -> self
/// - to_int -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_i.html]
extern "C" fn to_i(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    Some(self_val)
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
