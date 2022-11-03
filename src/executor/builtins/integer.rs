use crate::*;

//
// Integer class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(INTEGER_CLASS, "chr", chr, 0);
    globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
}

/// ### Integer#times
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
extern "C" fn times(
    vm: &mut Interp,
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

    return Some(self_val);
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
extern "C" fn chr(
    _vm: &mut Interp,
    globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    match self_val.try_fixnum() {
        Some(i) => {
            if let Ok(b) = u8::try_from(i) {
                return Some(Value::new_string_from_slice(&[b]));
            }
        }
        _ => {}
    };
    globals.err_char_out_of_range(self_val);
    return None;
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn times() {
        run_test_no_result_check(
            r##"
        a = 100
        4.times do
          puts a
          #__dump
        end
        "##,
        );
    }
}
