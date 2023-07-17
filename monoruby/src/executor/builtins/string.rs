use num::{BigInt, Num};

use crate::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("String", STRING_CLASS);
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
    globals.define_builtin_func(STRING_CLASS, "*", mul, 1);
    globals.define_builtin_func(STRING_CLASS, "==", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "===", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "!=", ne, 1);
    globals.define_builtin_func(STRING_CLASS, "%", rem, 1);
    globals.define_builtin_func(STRING_CLASS, "=~", match_, 1);
    globals.define_builtin_func(STRING_CLASS, "[]", index, -1);
    globals.define_builtin_func(STRING_CLASS, "start_with?", start_with, 1);
    globals.define_builtin_func(STRING_CLASS, "end_with?", end_with, 1);
    globals.define_builtin_func(STRING_CLASS, "split", split, -1);
    globals.define_builtin_func(STRING_CLASS, "gsub", gsub, -1);
    globals.define_builtin_func(STRING_CLASS, "gsub!", gsub_, -1);
    globals.define_builtin_func(STRING_CLASS, "sub", sub, -1);
    globals.define_builtin_func(STRING_CLASS, "sub!", sub_, -1);
    globals.define_builtin_func(STRING_CLASS, "match", string_match, -1);
    globals.define_builtin_func(STRING_CLASS, "to_s", tos, 0);
    globals.define_builtin_func(STRING_CLASS, "length", length, 0);
    globals.define_builtin_func(STRING_CLASS, "size", length, 0);
    globals.define_builtin_func(STRING_CLASS, "ljust", ljust, -1);
    globals.define_builtin_func(STRING_CLASS, "rjust", rjust, -1);
    globals.define_builtin_func(STRING_CLASS, "lines", lines, -0);
    globals.define_builtin_func(STRING_CLASS, "empty?", empty, 0);
    globals.define_builtin_func(STRING_CLASS, "to_i", to_i, -1);
    globals.define_builtin_func(STRING_CLASS, "intern", to_sym, 0);
    globals.define_builtin_func(STRING_CLASS, "to_sym", to_sym, 0);
    globals.define_builtin_func(STRING_CLASS, "upcase", upcase, 0);
    globals.define_builtin_func(STRING_CLASS, "tr", tr, 2);
}

///
/// ### String#+
///
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
#[monoruby_builtin]
fn add(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut b = StringInner::from_slice(lfp.self_val().as_bytes());
    b.extend_from_slice(arg[0].as_bytes());
    Ok(Value::string_from_inner(b))
}

///
/// ### String#*
///
/// - self * times -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2a.html]
#[monoruby_builtin]
fn mul(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut lhs = StringInner::from_slice(lfp.self_val().as_bytes());
    let count = match arg[0].coerce_to_fixnum(globals)? {
        i if i < 0 => return Err(MonorubyErr::negative_argument()),
        i => i as usize,
    };

    let res = Value::string_from_vec(lhs.repeat(count));
    Ok(res)
}

///
/// ### String#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let b = match arg[0].is_string() {
        Some(rhs) => rhs == lhs,
        None => false,
    };
    Ok(Value::bool(b))
}

///
/// ### String#!=
///
/// - self != other -> bool
///
/// []
#[monoruby_builtin]
fn ne(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let b = match arg[0].is_string() {
        Some(rhs) => rhs == lhs,
        None => false,
    };
    Ok(Value::bool(!b))
}

fn expect_char(chars: &mut std::str::Chars) -> Result<char> {
    let ch = match chars.next() {
        Some(ch) => ch,
        None => {
            return Err(MonorubyErr::argumenterr(
                "Invalid termination of format string".to_string(),
            ));
        }
    };
    Ok(ch)
}

enum Integer {
    Fixnum(i64),
    BigInt(BigInt),
}

fn coerce_to_integer(globals: &mut Globals, val: Value) -> Result<Integer> {
    match val.unpack() {
        RV::Integer(i) => return Ok(Integer::Fixnum(i)),
        RV::String(s) => {
            if let Ok(s) = String::from_utf8(s.to_vec()) {
                match s.parse::<i64>() {
                    Ok(i) => return Ok(Integer::Fixnum(i)),
                    Err(_) => {
                        if let Ok(b) = s.parse::<BigInt>() {
                            return Ok(Integer::BigInt(b));
                        }
                    }
                }
            }
        }
        _ => {}
    }
    let s = globals.to_s(val);
    Err(MonorubyErr::argumenterr(format!(
        "invalid value for Integer(): {}",
        s
    )))
}

fn coerce_to_float(globals: &mut Globals, val: Value) -> Result<f64> {
    match val.unpack() {
        RV::Integer(i) => Ok(i as f64),
        RV::Float(f) => Ok(f),
        _ => {
            let s = globals.to_s(val);
            Err(MonorubyErr::argumenterr(format!(
                "invalid value for Float(): {}",
                s
            )))
        }
    }
}

fn coerce_to_char(val: Value) -> Result<char> {
    match val.unpack() {
        RV::Integer(i) => {
            if let Ok(u) = u32::try_from(i) {
                if let Some(c) = char::from_u32(u) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character".to_string()))
        }
        RV::Float(f) => {
            let f = f.trunc();
            if 0.0 <= f && f <= u32::MAX as f64 {
                if let Some(c) = char::from_u32(f as u32) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character".to_string()))
        }
        RV::String(s) => match String::from_utf8(s.to_vec()) {
            Ok(s) => {
                if s.chars().count() != 1 {
                    Err(MonorubyErr::argumenterr(
                        "%c requires a character".to_string(),
                    ))
                } else {
                    Ok(s.chars().next().unwrap())
                }
            }
            _ => Err(MonorubyErr::argumenterr(
                "%c requires a character".to_string(),
            )),
        },
        _ => Err(MonorubyErr::argumenterr("invalid character".to_string())),
    }
}

macro_rules! next_char {
    ($ch:ident, $chars:ident) => {
        $ch = match $chars.next() {
            Some(c) => c,
            None => break,
        };
    };
}

///
/// ### String#%
///
/// - self % args -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=25.html]
#[monoruby_builtin]
fn rem(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arguments = match arg[0].is_array() {
        Some(ary) => ary.to_vec(),
        None => vec![arg[0]],
    };
    let mut arg_no = 0;
    let mut format_str = String::new();
    let self_ = lfp.self_val();
    let self_str = self_.as_str();
    let mut chars = self_str.as_ref().chars();
    let mut ch = match chars.next() {
        Some(ch) => ch,
        None => {
            return Ok(Value::string(String::new()));
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
                return Err(MonorubyErr::argumenterr(
                    "incomplete format specifier; use %% (double %) instead".to_string(),
                ));
            }
        };
        let mut zero_flag = false;
        // Zero-fill
        if ch == '0' {
            zero_flag = true;
            ch = expect_char(&mut chars)?;
        }
        // Width
        let mut width = 0usize;
        while ch.is_ascii_digit() {
            width = width * 10 + ch as usize - '0' as usize;
            ch = expect_char(&mut chars)?;
        }
        // Precision
        let mut precision = 0usize;
        if ch == '.' {
            ch = expect_char(&mut chars)?;
            while ch.is_ascii_digit() {
                precision = precision * 10 + ch as usize - '0' as usize;
                ch = expect_char(&mut chars)?;
            }
        } else {
            precision = 6;
        };
        if arguments.len() <= arg_no {
            return Err(MonorubyErr::argumenterr("too few arguments".to_string()));
        };
        // Specifier
        let val = arguments[arg_no];
        arg_no += 1;
        let format = match ch {
            'c' => {
                let ch = coerce_to_char(val)?;
                format!("{}", ch)
            }
            's' => globals.to_s(val),
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
                return Err(MonorubyErr::argumenterr(format!(
                    "malformed format string - %{}",
                    ch
                )))
            }
        };
        format_str += &format;
        next_char!(ch, chars);
    }

    let res = Value::string(format_str);
    Ok(res)
}

///
/// ### String#=~
///
/// - self =~ other -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3d=7e.html]
#[monoruby_builtin]
fn match_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let given = self_val.as_str();
    let regex = &arg[0].expect_regexp_or_string(globals)?;
    let res = match RegexpInner::find_one(vm, regex, &given)? {
        Some(mat) => Value::integer(mat.start() as i64),
        None => Value::nil(),
    };
    Ok(res)
}

///
/// ### String#[]
/// - self[nth] -> String | nil
/// - self[nth, len] -> String | nil
/// [NOT SUPPORTED] - self[substr] -> String | nil
/// - self[regexp, nth = 0] -> String
/// [NOT SUPPORTED] - self[regexp, nth = 0] -> String
/// - self[range] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=5b=5d.html]
#[monoruby_builtin]
fn index(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    fn conv_index(i: i64, len: usize) -> Option<usize> {
        if i >= 0 {
            if i <= len as i64 {
                Some(i as usize)
            } else {
                None
            }
        } else {
            match len as i64 + i {
                n if n < 0 => None,
                n => Some(n as usize),
            }
        }
    }
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let self_ = lfp.self_val();
    let lhs = self_.expect_string(globals)?;
    if let Some(i) = arg[0].try_fixnum() {
        let index = match conv_index(i, lhs.chars().count()) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if len == 2 {
            let len = match arg[1].coerce_to_fixnum(globals)? {
                0 => return Ok(Value::string_from_str("")),
                i if i < 0 => return Ok(Value::nil()),
                i => i as usize,
            };
            let ch: String = lhs.chars().skip(index).take(len).collect();
            Ok(Value::string_from_vec(ch.into_bytes()))
        } else {
            let len = 1usize;
            let ch: String = lhs.chars().skip(index).take(len).collect();
            if ch.len() != 0 {
                Ok(Value::string_from_vec(ch.into_bytes()))
            } else {
                Ok(Value::nil())
            }
        }
    } else if let Some(info) = arg[0].is_range() {
        let len = lhs.chars().count();
        // TODO: exclude?
        let (start, end) = match (info.start.try_fixnum(), info.end.try_fixnum()) {
            (Some(start), Some(end)) => match (conv_index(start, len), conv_index(end, len)) {
                (Some(start), Some(end)) if start > end => return Ok(Value::string_from_str("")),
                (Some(start), Some(end)) => (start, end),
                _ => return Ok(Value::nil()),
            },
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "Index must be Integer.".to_string(),
                ));
            }
        };
        let s: String = lhs.chars().skip(start).take(end - start + 1).collect();
        Ok(Value::string(s))
    } else if let Some(info) = arg[0].is_regex() {
        let nth = if len == 1 {
            0
        } else {
            arg[1].coerce_to_fixnum(globals)?
        };
        match info.captures(&lhs) {
            Ok(None) => return Ok(Value::nil()),
            Ok(Some(captures)) => {
                vm.save_captures(&captures, &lhs);
                let len = captures.len() as i64;
                if nth == 0 {
                    Ok(Value::string_from_str(captures.get(0).unwrap().as_str()))
                } else if nth > 0 {
                    match captures.get(nth as usize) {
                        Some(m) => Ok(Value::string_from_str(m.as_str())),
                        None => Ok(Value::nil()),
                    }
                } else {
                    match len + nth {
                        i if i > 0 => Ok(Value::string_from_str(
                            captures.get(i as usize).unwrap().as_str(),
                        )),
                        _ => Ok(Value::nil()),
                    }
                }
            }
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    } else {
        Err(MonorubyErr::argumenterr("Bad type for index.".to_string()))
    }
}

///
/// ### String#start_with?
/// - start_with?([NOT SUPPORTED]*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/start_with=3f.html]
#[monoruby_builtin]
fn start_with(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let string = lfp.self_val().expect_string(globals)?;
    let arg0 = arg[0];
    let arg = arg0.expect_string(globals)?;
    let res = string.starts_with(&arg);
    Ok(Value::bool(res))
}

///
/// ### String#end_with?
/// - end_with?([NOT SUPPORTED]*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/end_with=3f.html]
#[monoruby_builtin]
fn end_with(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let string = lfp.self_val().expect_string(globals)?;
    let arg0 = arg[0];
    let arg = arg0.expect_string(globals)?;
    let res = string.ends_with(&arg);
    Ok(Value::bool(res))
}

///
/// ### String#split
/// - split(sep = $;, limit = 0) -> [String]
/// - split(sep = $;, limit = 0) {|s| ... } -> self
///
/// TODO: support nil and ' ' for sep.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/split.html]
#[monoruby_builtin]
fn split(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let self_ = lfp.self_val();
    let string = self_.expect_string(globals)?;
    let arg0 = arg[0];
    if let Some(sep) = arg0.is_string() {
        let lim = if len > 1 {
            arg[1].coerce_to_fixnum(globals)?
        } else {
            0
        };
        let v: Vec<Value> = if sep == " " {
            if lim < 0 {
                let end_with = string.ends_with(|c: char| c.is_ascii_whitespace());
                let mut v: Vec<_> = string
                    .split_ascii_whitespace()
                    .map(Value::string_from_str)
                    .collect();
                if end_with {
                    v.push(Value::string_from_str(""))
                }
                v
            } else if lim == 0 {
                let mut vec: Vec<&str> = string.split_ascii_whitespace().collect();
                while let Some(s) = vec.last() {
                    if s.is_empty() {
                        vec.pop().unwrap();
                    } else {
                        break;
                    }
                }
                vec.into_iter().map(Value::string_from_str).collect()
            } else {
                string
                    .trim_start()
                    .splitn(lim as usize, |c: char| c.is_ascii_whitespace())
                    .map(|s| s.trim_start())
                    .map(Value::string_from_str)
                    .collect()
            }
        } else if lim < 0 {
            string.split(&sep).map(Value::string_from_str).collect()
        } else if lim == 0 {
            let mut vec: Vec<&str> = string.split(&sep).collect();
            while let Some(s) = vec.last() {
                if s.is_empty() {
                    vec.pop().unwrap();
                } else {
                    break;
                }
            }
            vec.into_iter().map(Value::string_from_str).collect()
        } else {
            string
                .splitn(lim as usize, &sep)
                .map(Value::string_from_str)
                .collect()
        };
        match lfp.block() {
            Some(_) => {
                let t = vm.temp_append(v.clone());
                vm.invoke_block_iter1(globals, v.into_iter())?;
                vm.temp_clear(t);
                Ok(lfp.self_val())
            }
            None => Ok(Value::array_from_vec(v)),
        }
    } else if let Some(re) = arg0.is_regex() {
        let lim = if len > 1 {
            arg[1].coerce_to_fixnum(globals)?
        } else {
            0
        };
        let all_cap = re.captures_iter(&string);
        let mut cursor = 0usize;
        let mut res = vec![];
        let mut count = 0;
        'l: for c in all_cap {
            let c = c.unwrap();
            let mut iter = c.iter();
            let m = iter.next().unwrap().unwrap();
            count += 1;
            if count == lim {
                break 'l;
            } else {
                res.push(&string[cursor..m.start()]);
            }
            while let Some(m) = iter.next() {
                count += 1;
                if count == lim {
                    cursor = m.unwrap().start();
                    break 'l;
                } else {
                    res.push(m.unwrap().as_str())
                }
            }
            cursor = m.end();
        }
        if cursor <= string.len() {
            res.push(&string[cursor..]);
        }
        // if lim == 0, remove all empty strings from a tail.
        if lim == 0 {
            while let Some(s) = res.last() {
                if s.is_empty() {
                    res.pop().unwrap();
                } else {
                    break;
                }
            }
        }
        let iter = res.into_iter().map(|s| Value::string_from_str(s));
        match lfp.block() {
            Some(_) => {
                vm.invoke_block_iter1(globals, iter)?;
                Ok(lfp.self_val())
            }
            None => Ok(Value::array_from_iter(iter)),
        }
    } else {
        Err(MonorubyErr::is_not_regexp_nor_string(globals, arg0))
    }
}

///
/// ### String#sub
///
/// - sub(pattern, replace) -> String
/// - sub(pattern) {|matched| .... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let (res, _) = sub_main(vm, globals, lfp.self_val(), arg, len, lfp.block())?;
    Ok(Value::string(res))
}

///
/// ### String#sub
///
/// - sub!(pattern, replace) -> self | nil
/// - sub!(pattern) {|matched| .... } -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub=21.html]
#[monoruby_builtin]
fn sub_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let (res, changed) = sub_main(vm, globals, self_, arg, len, lfp.block())?;
    self_.replace_string(res);
    let res = if changed { self_ } else { Value::nil() };
    Ok(res)
}

fn sub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    block: Option<BlockHandler>,
) -> Result<(String, bool)> {
    match block {
        None => {
            MonorubyErr::check_number_of_arguments(len, 2)?;
            let given = self_val.expect_string(globals)?;
            let replace = arg[1].expect_string(globals)?;
            RegexpInner::replace_one(vm, globals, arg[0], &given, &replace)
        }
        Some(_) => {
            MonorubyErr::check_number_of_arguments(len, 1)?;
            let given = self_val.expect_string(globals)?;
            RegexpInner::replace_one_block(vm, globals, arg[0], &given)
        }
    }
}

///
/// ### String#gsub
///
/// - gsub(pattern, replace) -> String
/// - gsub(pattern) {|matched| .... } -> String
/// - gsub(pattern) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/gsub.html]
#[monoruby_builtin]
fn gsub(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let (res, _) = gsub_main(vm, globals, lfp.self_val(), arg, len, lfp.block())?;
    Ok(Value::string(res))
}

///
/// ### String#gsub!
///
/// - gsub!(pattern, replace) -> self | nil
/// - gsub!(pattern) {|matched| .... } -> self | nil
/// - gsub!(pattern) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/gsub=21.html]
#[monoruby_builtin]
fn gsub_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let (res, changed) = gsub_main(vm, globals, self_, arg, len, lfp.block())?;
    self_.replace_string(res);
    let res = if changed { self_ } else { Value::nil() };
    Ok(res)
}

fn gsub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    args: Arg,
    len: usize,
    block: Option<BlockHandler>,
) -> Result<(String, bool)> {
    match block {
        None => {
            MonorubyErr::check_number_of_arguments(len, 2)?;
            let given = self_val.expect_string(globals)?;
            let replace = args[1].expect_string(globals)?;
            RegexpInner::replace_all(vm, globals, args[0], &given, &replace)
        }
        Some(_) => {
            MonorubyErr::check_number_of_arguments(len, 1)?;
            let given = self_val.expect_string(globals)?;
            RegexpInner::replace_all_block(vm, globals, args[0], &given)
        }
    }
}

///
/// ### String#match
///
/// - match(regexp, pos = 0) -> MatchData | nil
/// - match(regexp, pos = 0) {|m| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/match.html]
#[monoruby_builtin]
fn string_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let pos = match len {
        1 => 0usize,
        2 => match arg[1].coerce_to_fixnum(globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        },
        _ => unreachable!(),
    };
    let self_ = lfp.self_val();
    let given = self_.expect_string(globals)?;
    let re = arg[0].expect_regexp_or_string(globals)?;

    RegexpInner::match_one(vm, globals, &re, &given, lfp.block(), pos)
}

/// ### String#to_s
#[monoruby_builtin]
fn tos(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    Ok(lfp.self_val())
}

///
/// ### String#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/length.html]
#[monoruby_builtin]
fn length(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let length = lfp.self_val().as_str().chars().count();
    Ok(Value::integer(length as i64))
}

fn gen_pad(padding: &str, len: usize) -> String {
    let pad_len = padding.chars().count();
    let pad_repeat = padding.repeat(len / pad_len);
    let pad_end: String = padding.chars().take(len % pad_len).collect();
    format!("{}{}", pad_repeat, pad_end)
}

///
/// ### String#ljust
///
/// - ljust(width, padding = ' ') -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/ljust.html]
#[monoruby_builtin]
fn ljust(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let padding = if len == 2 {
        let arg = arg[1];
        arg.expect_string(globals)?
    } else {
        " ".to_string()
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = arg[0].coerce_to_fixnum(globals)?;
    let str_len = lhs.chars().count();
    if width <= 0 || width as usize <= str_len {
        return Ok(Value::string(lhs.to_string()));
    }
    let tail = width as usize - str_len;
    Ok(Value::string(format!("{}{}", lhs, gen_pad(&padding, tail))))
}

///
/// ### String#rjust
///
/// - rjust(width, padding = ' ') -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/rjust.html]
#[monoruby_builtin]
fn rjust(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let padding = if len == 2 {
        let arg = arg[1];
        arg.expect_string(globals)?
    } else {
        " ".to_string()
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = arg[0].coerce_to_fixnum(globals)?;
    let str_len = lhs.chars().count();
    if width <= 0 || width as usize <= str_len {
        return Ok(Value::string(lhs.to_string()));
    }
    let tail = width as usize - str_len;
    Ok(Value::string(format!("{}{}", gen_pad(&padding, tail), lhs)))
}

///
/// ### String#lines
/// - lines(rs = $/, chomp: false) -> [String]
/// [NOT SUPPORTED] - lines(rs = $/, chomp: false) {|line| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/lines.html]
#[monoruby_builtin]
fn lines(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    if lfp.block().is_some() {
        return Err(MonorubyErr::runtimeerr(
            "block is not supported.".to_string(),
        ));
    }
    let receiver = lfp.self_val();
    let string = receiver.expect_string(globals)?;
    let ary = string
        .split_inclusive('\n')
        .map(|line| Value::string_from_str(line));
    Ok(Value::array_from_iter(ary))
}

///
/// ### String#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/empty=3f.html]
#[monoruby_builtin]
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg, _: usize) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_bytes().is_empty()))
}

///
/// ### String#to_i
///
/// - to_i(base = 10) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_i.html]
#[monoruby_builtin]
fn to_i(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let self_ = lfp.self_val();
    let s = self_.as_str();
    let radix = if len == 0 {
        10
    } else {
        match arg[0].expect_integer(globals)? {
            n if n < 2 || 36 < n => {
                return Err(MonorubyErr::argumenterr(format!("invalid radix {n}")));
            }
            n => n as u32,
        }
    };
    let num = if let Ok(num) = i64::from_str_radix(&s, radix) {
        Value::integer(num)
    } else if let Ok(b) = BigInt::from_str_radix(&s, radix) {
        Value::bigint(b)
    } else {
        Value::i32(0)
    };
    Ok(num)
}

///
/// ### String#intern
///
/// - intern -> Symbol
/// - to_sym -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/intern.html]
#[monoruby_builtin]
fn to_sym(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let self_val = lfp.self_val();
    let sym = Value::symbol(IdentId::get_id(self_val.as_str().as_ref()));
    Ok(sym)
}

///
/// ### String#upcase
///
/// - upcase([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase.html]
#[monoruby_builtin]
fn upcase(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let self_val = lfp.self_val();
    let s = self_val.as_str().as_ref().to_uppercase();
    Ok(Value::string_from_vec(s.into_bytes()))
}

///
/// ### String#tr
///
/// - tr(pattern, replace) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/tr.html]
#[monoruby_builtin]
fn tr(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 2)?;
    let rec = lfp.self_val().expect_string(globals)?;
    let from = arg[0].expect_string(globals)?;
    let to = arg[1].expect_string(globals)?;
    assert_eq!(1, from.chars().count());
    assert_eq!(1, to.chars().count());
    let res = rec.replace(&from, &to);
    Ok(Value::string(res))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn string_empty() {
        run_test(r##""文字列".empty?"##);
        run_test(r##""".empty?"##);
    }

    #[test]
    fn string_add() {
        run_test(r##"a = "We will"; a + " " + "rock you." "##);
    }

    #[test]
    fn string_mul() {
        run_test(r##""abcde" * 5"##);
        run_test(r##""機動戦士" * 3"##);
        run_test_error(r##""機動戦士" * -1"##);
    }

    #[test]
    fn string_eq() {
        run_test(r##""abcde" == "abcde""##);
        run_test(r##""機動戦士GUNDOM" == "機動戦士GUNDOM""##);
        run_test(r##""機動戦士GUNDOM" == "機動戦士GUNDAM""##);
        run_test(r##""機動戦士GUNDOM" == :abs"##);
    }

    #[test]
    fn string_index() {
        run_test(r##"'bar'[2]"##);
        run_test(r##"'bar'[2] == ?r"##);
        run_test(r##"'bar'[-1]"##);
        run_test(r##"'bar'[3]"##);
        run_test(r##"'bar'[-4]"##);
        run_test_with_prelude(
            r##"[str0[2,1], str0[2,0], str0[2,100], str0[-1,1], str0[-1,2], str0[3,1], str0[4,1], str0[-4,1]]"##,
            r#"str0 = "bar""#,
        );
        run_test(r##""foobar"[/bar/]"##);
        run_test(r##""def getcnt(line)"[ /def\s+(\w+)/, 1 ]"##);
        run_test(
            r##"['abcd'[2..1], 'abcd'[2..2], 'abcd'[2..3], 'abcd'[2..4], 'abcd'[2..-1], 'abcd'[3..-1]]"##,
        );
        run_test(
            r##"['abcd'[1..2], 'abcd'[2..2], 'abcd'[3..2], 'abcd'[4..2], 'abcd'[5..2], 'abcd'[-3..2], 'abcd'[-4..2], 'abcd'[-5..2]]"##,
        );
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
        run_test_error(
            r##"
        "abcdefgdef".gsub(/def/, 3)
        "##,
        );
        run_test_error(
            r##"
        "abcdefgdef".gsub(/def/, "!!", 1)
        "##,
        );
        run_test_error(
            r##"
        "abcdefgdddefjklefl".gsub(/d*ef/, 10) {
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

    #[test]
    fn set_match() {
        run_test(
            r##"
        "abcdefgdddefjklefl".match(/d*ef/) {
            |matched| matched.to_s
        }
        "##,
        );
        run_test(r##"'hoge hige hege bar'.match('h.ge', 0)[0]"##);
        run_test(r##"'hoge hige hege bar'.match('h.ge', 1)[0]"##);
        run_test(r##"'hoge 髭男 hege bar'.match('髭.', 5)[0]"##);
        run_test(r##"'髭女 髭男 髭面 髭剃'.match('髭.', 5)[0]"##);
    }

    #[test]
    fn length() {
        run_test(r##""本日は快晴なり".length"##);
    }

    #[test]
    fn ljust() {
        run_test(r##""戦闘妖精".ljust 11"##);
        run_test(r##""戦闘妖精".ljust 11,"$""##);
        run_test(r##""戦闘妖精".ljust 11,"123""##);
        run_test_error(r##""戦闘妖精".ljust"##);
        run_test_error(r##""戦闘妖精".ljust 8, """##);
    }

    #[test]
    fn rjust() {
        run_test(r##""戦闘妖精".rjust 11"##);
        run_test(r##""戦闘妖精".rjust 11,"$""##);
        run_test(r##""戦闘妖精".rjust 11,"123""##);
        run_test_error(r##""戦闘妖精".rjust"##);
        run_test_error(r##""戦闘妖精".rjust 8, """##);
    }

    #[test]
    fn with() {
        run_test(r##""string".start_with?("str")"##);
        run_test(r##""string".start_with?("ing")"##);
        run_test(r##""string".end_with?("str")"##);
        run_test(r##""string".end_with?("ing")"##);
    }

    #[test]
    fn lines() {
        run_test(r##""aa\nbb\ncc\n".lines"##);
        run_test(r##""hello\nworld\n".lines"##);
    }

    #[test]
    fn split() {
        run_test(r##""   a \t  b \n  c".split(/\s+/)"##);
        //run_test(r##""   a \t  b \n  c".split(nil)"##);
        run_test(r##""   a \t  b \n  c  ".split(' ')"##);
        run_test(r##""   a \t  b \n  c  ".split(' ', -1)"##);
        run_test(r##""   a \t  b \n  c ".split(' ', -1)"##);
        run_test(r##""   a \t  b \n  c".split(' ', -1)"##);
        run_test(r##""   a \t  b \n  c  ".split(' ', 0)"##);
        run_test(r##""   a \t  b \n  c  ".split(' ', 2)"##);
        //run_test(r##""   a \t  b \n  c".split"##);
        run_test(r##"'1-10,20'.split(/([-,])/)"##);
        run_test(r##"'hi there'.split(/\s*/).join(':')"##);
        run_test(r##"'hi there'.split(//).join(':')"##);
        run_test(r##""a,b,c,,,".split(/,/, 0)"##);
        run_test(r##""a,b,c,,,".split(/,/)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 1)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 2)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 3)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 4)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 5)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 6)"##);
        run_test(r##""a,b,c,d,e".split(/,/, 7)"##);
        run_test(r##""a,b,c,d,e".split(/,/, -1)"##);
    }

    #[test]
    fn to_i() {
        run_test(r"'42581'.to_i");
        run_test(r"'4a5f1'.to_i(16)");
        run_test(r"'4258159248352010254587519982001542568633842205196875555'.to_i");
        run_test(r"'42581592483edrcs0254587519982001ipgomrn568633842205196875555'.to_i(36)");
        run_test_error(r"'42581'.to_i(-10)");
        run_test_error(r"'42581'.to_i(100)");
    }

    #[test]
    fn to_sym() {
        run_test(r"'RubyAndRust'.to_sym");
        run_test(r"'Jane12345'.intern");
    }

    #[test]
    fn upcase() {
        run_test(r"'AkrFju35]['.upcase");
    }
}
