use num::ToPrimitive;

use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    globals.define_builtin_func(INTEGER_CLASS, "to_f", tof, 0);
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
    block: Option<Value>,
) -> Option<Value> {
    /*let mut bp: u64;
    unsafe {
        std::arch::asm!(
            "mov {bp}, rbp",
            bp = out(reg) bp,
        );
    }
    dbg!(bp as *const u8);*/
    let count = match self_val.try_fixnum() {
        Some(i) => i,
        None => unimplemented!(),
    };
    if let Some(block) = block {
        for i in 0..count {
            vm.invoke_block(globals, block, self_val, &[Value::new_integer(i)])?;
        }
    } else {
        unimplemented!("needs block.")
    };

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
    _: Option<Value>,
) -> Option<Value> {
    if let Some(i) = self_val.try_fixnum() {
        if let Ok(b) = u8::try_from(i) {
            return Some(Value::new_string_from_slice(&[b]));
        }
    };
    globals.err_char_out_of_range(self_val);
    None
}

extern "C" fn tof(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let f = match self_val.unpack() {
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Some(Value::new_float(f))
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
}
