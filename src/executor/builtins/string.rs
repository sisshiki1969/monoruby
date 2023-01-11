use num::BigInt;

use crate::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
    globals.define_builtin_func(STRING_CLASS, "%", rem, 1);
    globals.define_builtin_func(STRING_CLASS, "gsub", gsub, -1);
    globals.define_builtin_func(STRING_CLASS, "gsub!", gsub_, -1);
    globals.define_builtin_func(STRING_CLASS, "sub", sub, -1);
    globals.define_builtin_func(STRING_CLASS, "sub!", sub_, -1);
}

/// ### String#+
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
extern "C" fn add(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let mut b = self_val.as_bytes().to_vec();
    b.extend_from_slice(arg[0].as_bytes());
    Some(Value::new_string_from_slice(&b))
}

fn expect_char(globals: &mut Globals, chars: &mut std::str::Chars) -> Option<char> {
    let ch = match chars.next() {
        Some(ch) => ch,
        None => {
            globals.err_argument("Invalid termination of format string");
            return None;
        }
    };
    Some(ch)
}

enum Integer {
    Fixnum(i64),
    BigInt(BigInt),
}

fn coerce_to_integer(globals: &mut Globals, val: Value) -> Option<Integer> {
    match val.unpack() {
        RV::Integer(i) => return Some(Integer::Fixnum(i)),
        RV::String(s) => {
            if let Ok(s) = String::from_utf8(s.to_vec()) {
                match s.parse::<i64>() {
                    Ok(i) => return Some(Integer::Fixnum(i)),
                    Err(_) => {
                        if let Ok(b) = s.parse::<BigInt>() {
                            return Some(Integer::BigInt(b));
                        }
                    }
                }
            }
        }
        _ => {}
    }
    let s = val.to_s(globals);
    globals.err_argument(&format!("invalid value for Integer(): {}", s));
    None
}

fn coerce_to_float(globals: &mut Globals, val: Value) -> Option<f64> {
    match val.unpack() {
        RV::Integer(i) => Some(i as f64),
        RV::Float(f) => Some(f),
        _ => {
            let s = val.to_s(globals);
            globals.err_argument(&format!("invalid value for Float(): {}", s));
            None
        }
    }
}

fn coerce_to_char(globals: &mut Globals, val: Value) -> Option<char> {
    match val.unpack() {
        RV::Integer(i) => {
            if let Ok(u) = u32::try_from(i) {
                if let Some(c) = char::from_u32(u) {
                    return Some(c);
                }
            }
            globals.err_argument("invalid character");
        }
        RV::Float(f) => {
            let f = f.trunc();
            if 0.0 <= f && f <= u32::MAX as f64 {
                if let Some(c) = char::from_u32(f as u32) {
                    return Some(c);
                }
            }
            globals.err_argument("invalid character");
        }
        RV::String(s) => match String::from_utf8(s.to_vec()) {
            Ok(s) => {
                if s.chars().count() != 1 {
                    globals.err_argument("%c requires a character");
                } else {
                    return Some(s.chars().next().unwrap());
                }
            }
            _ => {
                globals.err_argument("%c requires a character");
            }
        },
        _ => {
            globals.err_argument("invalid character");
        }
    };
    None
}

macro_rules! next_char {
    ($ch:ident, $chars:ident) => {
        $ch = match $chars.next() {
            Some(c) => c,
            None => break,
        };
    };
}

/// ### String#%
/// - self % args -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=25.html]
extern "C" fn rem(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _block: Option<Value>,
) -> Option<Value> {
    let arguments = match arg[0].is_array() {
        Some(ary) => ary.to_vec(),
        None => vec![arg[0]],
    };
    let mut arg_no = 0;
    let mut format_str = String::new();
    let self_str = self_val.as_string();
    let mut chars = self_str.chars();
    let mut ch = match chars.next() {
        Some(ch) => ch,
        None => {
            return Some(Value::new_string(String::new()));
        }
    };
    loop {
        if ch != '%' {
            format_str.push(ch);
            next_char!(ch, chars);
            continue;
        }
        match chars.next() {
            Some(c) if c == '%' => {
                format_str.push('%');
                next_char!(ch, chars);
                continue;
            }
            Some(c) => ch = c,
            None => {
                globals.err_argument("incomplete format specifier; use %% (double %) instead");
                return None;
            }
        };
        let mut zero_flag = false;
        // Zero-fill
        if ch == '0' {
            zero_flag = true;
            ch = expect_char(globals, &mut chars)?;
        }
        // Width
        let mut width = 0usize;
        while ('0'..='9').contains(&ch) {
            width = width * 10 + ch as usize - '0' as usize;
            ch = expect_char(globals, &mut chars)?;
        }
        // Precision
        let mut precision = 0usize;
        if ch == '.' {
            ch = expect_char(globals, &mut chars)?;
            while ('0'..='9').contains(&ch) {
                precision = precision * 10 + ch as usize - '0' as usize;
                ch = expect_char(globals, &mut chars)?;
            }
        } else {
            precision = 6;
        };
        if arguments.len() <= arg_no {
            globals.err_argument("too few arguments");
            return None;
        };
        // Specifier
        let val = arguments[arg_no];
        arg_no += 1;
        let format = match ch {
            'c' => {
                let ch = coerce_to_char(globals, val)?;
                format!("{}", ch)
            }
            's' => val.to_s(globals),
            'd' | 'i' => {
                let val = coerce_to_integer(globals, val)?;
                if zero_flag {
                    match val {
                        Integer::Fixnum(val) => format!("{:0w$.p$}", val, w = width, p = precision),
                        Integer::BigInt(val) => format!("{:0w$.p$}", val, w = width, p = precision),
                    }
                } else {
                    match val {
                        Integer::Fixnum(val) => format!("{:w$.p$}", val, w = width, p = precision),
                        Integer::BigInt(val) => format!("{:w$.p$}", val, w = width, p = precision),
                    }
                }
            }
            'b' => {
                let val = coerce_to_integer(globals, val)?;
                if zero_flag {
                    match val {
                        Integer::Fixnum(val) => format!("{:0w$b}", val, w = width),
                        Integer::BigInt(val) => format!("{:0w$b}", val, w = width),
                    }
                } else {
                    match val {
                        Integer::Fixnum(val) => format!("{:w$b}", val, w = width),
                        Integer::BigInt(val) => format!("{:w$b}", val, w = width),
                    }
                }
            }
            'x' => {
                let val = coerce_to_integer(globals, val)?;
                if zero_flag {
                    match val {
                        Integer::Fixnum(val) => format!("{:0w$x}", val, w = width),
                        Integer::BigInt(val) => format!("{:0w$x}", val, w = width),
                    }
                } else {
                    match val {
                        Integer::Fixnum(val) => format!("{:w$x}", val, w = width),
                        Integer::BigInt(val) => format!("{:w$x}", val, w = width),
                    }
                }
            }
            'X' => {
                let val = coerce_to_integer(globals, val)?;
                if zero_flag {
                    match val {
                        Integer::Fixnum(val) => format!("{:0w$X}", val, w = width),
                        Integer::BigInt(val) => format!("{:0w$X}", val, w = width),
                    }
                } else {
                    match val {
                        Integer::Fixnum(val) => format!("{:w$X}", val, w = width),
                        Integer::BigInt(val) => format!("{:w$X}", val, w = width),
                    }
                }
            }
            'f' => {
                let f = coerce_to_float(globals, val)?;
                if zero_flag {
                    format!("{:0w$.p$}", f, w = width, p = precision)
                } else {
                    format!("{:w$.p$}", f, w = width, p = precision)
                }
            }
            'e' => {
                let f = coerce_to_float(globals, val)?;
                if zero_flag {
                    format!("{:0w$.p$e}", f, w = width, p = precision)
                } else {
                    format!("{:w$.p$e}", f, w = width, p = precision)
                }
            }
            'E' => {
                let f = coerce_to_float(globals, val)?;
                if zero_flag {
                    format!("{:0w$.p$E}", f, w = width, p = precision)
                } else {
                    format!("{:w$.p$E}", f, w = width, p = precision)
                }
            }
            _ => {
                globals.err_argument(&format!("malformed format string - %{}", ch));
                return None;
            }
        };
        format_str += &format;
        next_char!(ch, chars);
    }

    let res = Value::new_string(format_str);
    Some(res)
}

/// ### String#sub
/// - sub(pattern, replace) -> String
/// - sub(pattern) {|matched| .... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub.html]
extern "C" fn sub(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<Value> {
    let (res, _) = sub_main(vm, globals, self_val, arg, len, block)?;
    Some(Value::new_string(res))
}

/// ### String#sub
/// - sub!(pattern, replace) -> self | nil
/// - sub!(pattern) {|matched| .... } -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub=21.html]
extern "C" fn sub_(
    vm: &mut Executor,
    globals: &mut Globals,
    mut self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<Value> {
    let (res, changed) = sub_main(vm, globals, self_val, arg, len, block)?;
    self_val.replace_string(res);
    let res = if changed { self_val } else { Value::nil() };
    Some(res)
}

fn sub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<(String, bool)> {
    match block {
        None => {
            if len != 2 {
                globals.err_wrong_number_of_arguments_range(len, 2..=2);
                return None;
            }
            let given = self_val.expect_string(globals)?;
            let replace = arg[1].expect_string(globals)?;
            RegexpInfo::replace_one(vm, globals, arg[0], &given, &replace)
        }
        Some(block) => {
            if len != 1 {
                globals.err_wrong_number_of_arguments_range(len, 1..=1);
                return None;
            }
            let given = self_val.expect_string(globals)?;
            RegexpInfo::replace_one_block(vm, globals, arg[0], &given, block)
        }
    }
}

/// ### String#gsub
/// - gsub(pattern, replace) -> String
/// - gsub(pattern) {|matched| .... } -> String
/// - gsub(pattern) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/gsub.html]
extern "C" fn gsub(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<Value> {
    let (res, _) = gsub_main(vm, globals, self_val, arg, len, block)?;
    Some(Value::new_string(res))
}

/// ### String#gsub!
/// - gsub!(pattern, replace) -> self | nil
/// - gsub!(pattern) {|matched| .... } -> self | nil
/// - gsub!(pattern) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/gsub=21.html]
extern "C" fn gsub_(
    vm: &mut Executor,
    globals: &mut Globals,
    mut self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<Value> {
    let (res, changed) = gsub_main(vm, globals, self_val, arg, len, block)?;
    self_val.replace_string(res);
    let res = if changed { self_val } else { Value::nil() };
    Some(res)
}

fn gsub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    args: Arg,
    len: usize,
    block: Option<Value>,
) -> Option<(String, bool)> {
    match block {
        None => {
            if len != 2 {
                globals.err_wrong_number_of_arguments_range(len, 2..=2);
                return None;
            }
            let given = self_val.expect_string(globals)?;
            let replace = args[1].expect_string(globals)?;
            RegexpInfo::replace_all(vm, globals, args[0], &given, &replace)
        }
        Some(block) => {
            if len != 1 {
                globals.err_wrong_number_of_arguments_range(len, 1..=1);
                return None;
            }
            let given = self_val.expect_string(globals)?;
            RegexpInfo::replace_all_block(vm, globals, args[0], &given, block)
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn string_add() {
        run_test(r##"a = "We will"; a + " " + "rock you." "##);
    }

    #[test]
    fn string_format() {
        run_test2(r###""-%d-" % 12"###);
        run_test2(r###""-%4d-" % 12"###);
        run_test2(r###""-%04d-" % 12"###);
        run_test2(r###""-%x-" % 12"###);
        run_test2(r###""-%4x-" % 12"###);
        run_test2(r###""-%04x-" % 12"###);
        run_test2(r###""-%X-" % 12"###);
        run_test2(r###""-%4X-" % 12"###);
        run_test2(r###""-%04X-" % 12"###);
        run_test2(r###""-%b-" % 9"###);
        run_test2(r###""-%6b-" % 9"###);
        run_test2(r###""-%06b-" % 9"###);
        run_test2(r###""%40i" % "1257765464656546546546546546546546546""###);
        run_test2(r###""%40b" % "1257765464656546546546546546546546546""###);
        run_test2(r###""%08.5f" % 12.575824562"###);
        run_test2(r###""%08.3f" % 12.57"###);
        run_test2(r###""%08.f" % 12.57"###);
        run_test2(r###""%.2f" % 1.345"###);
        run_test2(r###""%3.4f" % 1.34578"###);
        run_test2(r###""%3f" % 1.34578785885"###);
        run_test2(r###""%15.1e" % 12785.34578e-127"###);
        run_test2(r###""%15.1E" % 12785.34578e-127"###);
        run_test2(r###""%c %c %c" % [46, 52.0, "r"]"###);
    }

    #[test]
    fn gsub() {
        run_test(r##""abcdefgdef".gsub(/def/, "!!")"##);
        run_test(r##""2.5".gsub(".", ",")"##);
        run_test(
            r##"
        "abcdefgdddefjklefl".gsub(/d*ef/) {
            |matched| "+" + matched + "+"
        }
        "##,
        );
        run_test(
            r##"
        s = "abcdefghdefr"
        s.gsub!(/def/, "!!")
        s
        "##,
        );
        run_test(
            r##"
        s = "2.5.3..75841."
        s.gsub!(".", ",")
        s
        "##,
        );
        run_test(
            r##"
        s = "abcdefghdefr"
        s.gsub!(/def1/, "!!")
        "##,
        );
    }

    #[test]
    fn sub() {
        run_test(r##""abcdefgdef".sub(/def/, "!!")"##);
        run_test(r##""2.5".sub(".", ",")"##);
        run_test(
            r##"
        "abcdefgdddefjklefl".sub(/d*ef/) {
            |matched| "+" + matched + "+"
        }
        "##,
        );
        run_test(
            r##"
        s = "abcdefghdefr"
        s.sub!(/def/, "!!")
        "##,
        );
        run_test(
            r##"
        s = "2.5.3..75841."
        s.sub!(".", ",")
        "##,
        );
        run_test(
            r##"
        s = "abcdefghdefr"
        s.sub!(/def1/, "!!")
        "##,
        );
    }
}
