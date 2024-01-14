use std::borrow::Cow;

use num::{BigInt, Num};

use super::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("String", STRING_CLASS);
    globals.define_builtin_func(STRING_CLASS, "+", add);
    globals.define_builtin_func(STRING_CLASS, "*", mul);
    globals.define_builtin_func(STRING_CLASS, "==", eq);
    globals.define_builtin_func(STRING_CLASS, "===", eq);
    globals.define_builtin_func(STRING_CLASS, "<=>", cmp);
    globals.define_builtin_func(STRING_CLASS, "!=", ne);
    globals.define_builtin_func(STRING_CLASS, "%", rem);
    globals.define_builtin_func(STRING_CLASS, "=~", match_);
    globals.define_builtin_func(STRING_CLASS, "[]", index);
    globals.define_builtin_func(STRING_CLASS, "start_with?", start_with);
    globals.define_builtin_func(STRING_CLASS, "end_with?", end_with);
    globals.define_builtin_func(STRING_CLASS, "split", split);
    globals.define_builtin_func(STRING_CLASS, "chomp", chomp);
    globals.define_builtin_func(STRING_CLASS, "gsub", gsub);
    globals.define_builtin_func(STRING_CLASS, "gsub!", gsub_);
    globals.define_builtin_func(STRING_CLASS, "sub", sub);
    globals.define_builtin_func(STRING_CLASS, "sub!", sub_);
    globals.define_builtin_func(STRING_CLASS, "scan", scan);
    globals.define_builtin_func(STRING_CLASS, "match", string_match);
    globals.define_builtin_func(STRING_CLASS, "to_s", tos);
    globals.define_builtin_func(STRING_CLASS, "length", length);
    globals.define_builtin_func(STRING_CLASS, "size", length);
    globals.define_builtin_func(STRING_CLASS, "ljust", ljust);
    globals.define_builtin_func(STRING_CLASS, "rjust", rjust);
    globals.define_builtin_func(STRING_CLASS, "lines", lines);
    globals.define_builtin_func(STRING_CLASS, "bytes", bytes);
    globals.define_builtin_func(STRING_CLASS, "each_line", each_line);
    globals.define_builtin_func(STRING_CLASS, "empty?", empty);
    globals.define_builtin_func(STRING_CLASS, "to_i", to_i);
    globals.define_builtin_func(STRING_CLASS, "intern", to_sym);
    globals.define_builtin_func(STRING_CLASS, "to_sym", to_sym);
    globals.define_builtin_func(STRING_CLASS, "upcase", upcase);
    globals.define_builtin_func(STRING_CLASS, "downcase", downcase);
    globals.define_builtin_func(STRING_CLASS, "tr", tr);
    globals.define_builtin_func(STRING_CLASS, "sum", sum);
}

///
/// ### String#+
///
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
#[monoruby_builtin]
fn add(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let mut b = StringInner::from_slice(lfp.self_val().as_bytes());
    b.extend_from_slice(lfp.arg(0).as_bytes());
    Ok(Value::string_from_inner(b))
}

///
/// ### String#*
///
/// - self * times -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2a.html]
#[monoruby_builtin]
fn mul(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let mut lhs = StringInner::from_slice(lfp.self_val().as_bytes());
    let count = match lfp.arg(0).coerce_to_i64(globals)? {
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
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let b = match lfp.arg(0).is_str() {
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
fn ne(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let b = match lfp.arg(0).is_str() {
        Some(rhs) => rhs == lhs,
        None => false,
    };
    Ok(Value::bool(!b))
}

///
/// ### String#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let self_ = lfp.self_val();
    let lhs = self_.as_bytes();
    let b = match lfp.arg(0).is_bytes() {
        Some(rhs) => lhs.string_cmp(rhs),
        None => return Ok(Value::nil()),
    };
    Ok(Value::from_ord(b))
}

fn expect_char(chars: &mut std::str::Chars) -> Result<char> {
    let ch = match chars.next() {
        Some(ch) => ch,
        None => {
            return Err(MonorubyErr::argumenterr(
                "Invalid termination of format string",
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
        RV::Fixnum(i) => return Ok(Integer::Fixnum(i)),
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
        RV::Fixnum(i) => Ok(i as f64),
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
        RV::Fixnum(i) => {
            if let Ok(u) = u32::try_from(i) {
                if let Some(c) = char::from_u32(u) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::Float(f) => {
            let f = f.trunc();
            if 0.0 <= f && f <= u32::MAX as f64 {
                if let Some(c) = char::from_u32(f as u32) {
                    return Ok(c);
                }
            }
            Err(MonorubyErr::argumenterr("invalid character"))
        }
        RV::String(s) => match String::from_utf8(s.to_vec()) {
            Ok(s) => {
                if s.chars().count() != 1 {
                    Err(MonorubyErr::argumenterr("%c requires a character"))
                } else {
                    Ok(s.chars().next().unwrap())
                }
            }
            _ => Err(MonorubyErr::argumenterr("%c requires a character")),
        },
        _ => Err(MonorubyErr::argumenterr("invalid character")),
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
fn rem(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let arguments = match lfp.arg(0).is_array() {
        Some(ary) => ary.to_vec(),
        None => vec![lfp.arg(0)],
    };
    let mut arg_no = 0;
    let mut format_str = String::new();
    let self_ = lfp.self_val();
    let self_str = self_.as_str();
    let mut chars = self_str.chars();
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
                    "incomplete format specifier; use %% (double %) instead",
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
            return Err(MonorubyErr::argumenterr("too few arguments"));
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
fn match_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let self_val = lfp.self_val();
    let given = self_val.as_str();
    let regex = &lfp.arg(0).expect_regexp_or_string(globals)?;
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
fn index(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
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
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(1..=2)?;
    let self_ = lfp.self_val();
    let lhs = self_.expect_string(globals)?;
    if let Some(i) = lfp.arg(0).try_fixnum() {
        let index = match conv_index(i, lhs.chars().count()) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if len == 2 {
            let len = match lfp.arg(1).coerce_to_i64(globals)? {
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
    } else if let Some(info) = lfp.arg(0).is_range() {
        let len = lhs.chars().count();
        // TODO: exclude?
        let (start, end) = match (info.start.try_fixnum(), info.end.try_fixnum()) {
            (Some(start), Some(end)) => match (conv_index(start, len), conv_index(end, len)) {
                (Some(start), Some(end)) if start > end => return Ok(Value::string_from_str("")),
                (Some(start), Some(end)) => (start, end),
                _ => return Ok(Value::nil()),
            },
            _ => {
                return Err(MonorubyErr::argumenterr("Index must be Integer."));
            }
        };
        let s: String = lhs.chars().skip(start).take(end - start + 1).collect();
        Ok(Value::string(s))
    } else if let Some(info) = lfp.arg(0).is_regex() {
        let nth = if len == 1 {
            0
        } else {
            lfp.arg(1).coerce_to_i64(globals)?
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
        Err(MonorubyErr::argumenterr("Bad type for index."))
    }
}

///
/// ### String#start_with?
/// - start_with?([NOT SUPPORTED]*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/start_with=3f.html]
#[monoruby_builtin]
fn start_with(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let string = lfp.self_val().expect_string(globals)?;
    let arg0 = lfp.arg(0);
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
fn end_with(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let string = lfp.self_val().expect_string(globals)?;
    let arg0 = lfp.arg(0);
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
fn split(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(1..=2)?;
    let self_ = lfp.self_val();
    let string = self_.expect_string(globals)?;
    //let arg0 = lfp.arg(0);
    if let Some(sep) = lfp.arg(0).is_str() {
        let lim = if len > 1 {
            lfp.arg(1).coerce_to_i64(globals)?
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
            string.split(&*sep).map(Value::string_from_str).collect()
        } else if lim == 0 {
            let mut vec: Vec<&str> = string.split(&*sep).collect();
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
                .splitn(lim as usize, &*sep)
                .map(Value::string_from_str)
                .collect()
        };
        match lfp.block() {
            Some(bh) => {
                let t = vm.temp_append(v.clone());
                vm.invoke_block_iter1(globals, bh, v.into_iter())?;
                vm.temp_clear(t);
                Ok(lfp.self_val())
            }
            None => Ok(Value::array_from_vec(v)),
        }
    } else if let Some(re) = lfp.arg(0).is_regex() {
        let lim = if len > 1 {
            lfp.arg(1).coerce_to_i64(globals)?
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
                if cursor != 0 || cursor != m.start() || m.range().len() != 0 {
                    res.push(&string[cursor..m.start()]);
                }
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
            Some(bh) => {
                vm.invoke_block_iter1(globals, bh, iter)?;
                Ok(lfp.self_val())
            }
            None => Ok(Value::array_from_iter(iter)),
        }
    } else {
        Err(MonorubyErr::is_not_regexp_nor_string(globals, lfp.arg(0)))
    }
}

///
/// ### String#chomp
/// - chomp(rs = $/) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/chomp.html]
#[monoruby_builtin]
fn chomp(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments_range(0..=1)?;
    let rs = if lfp.arg_len() == 0 {
        "\n".to_string()
    } else {
        let arg = lfp.arg(0);
        if arg.is_nil() {
            return Ok(lfp.self_val());
        }
        arg.expect_string(globals)?
    };

    let self_ = lfp.self_val().expect_string(globals)?;
    let res = if rs.is_empty() {
        let mut s = self_.as_str();
        let mut len = s.len();
        loop {
            s = s.trim_end_matches("\r\n");
            if s.ends_with('\n') {
                s = &s[0..s.len() - 1];
            }
            if len == s.len() {
                break;
            }
            len = s.len();
        }
        s.to_string()
    } else if rs == "\n" {
        self_.trim_end_matches(&['\n', '\r']).to_string()
    } else {
        self_.trim_end_matches(&rs).to_string()
    };
    Ok(Value::string(res))
}

///
/// ### String#sub
///
/// - sub(pattern, replace) -> String
/// - sub(pattern) {|matched| .... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let (res, _) = sub_main(vm, globals, lfp.self_val(), lfp)?;
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
fn sub_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let (res, changed) = sub_main(vm, globals, self_, lfp)?;
    self_.replace_string(res);
    let res = if changed { self_ } else { Value::nil() };
    Ok(res)
}

fn sub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    lfp: LFP,
) -> Result<(String, bool)> {
    match lfp.block() {
        None => {
            lfp.check_number_of_arguments(2)?;
            let given = self_val.expect_string(globals)?;
            let replace = lfp.arg(1).expect_string(globals)?;
            RegexpInner::replace_one(vm, globals, lfp.arg(0), &given, &replace)
        }
        Some(bh) => {
            lfp.check_number_of_arguments(1)?;
            let given = self_val.expect_string(globals)?;
            RegexpInner::replace_one_block(vm, globals, lfp.arg(0), &given, bh)
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
fn gsub(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let (res, _) = gsub_main(vm, globals, lfp.self_val(), lfp)?;
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
fn gsub_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let (res, changed) = gsub_main(vm, globals, self_, lfp)?;
    self_.replace_string(res);
    let res = if changed { self_ } else { Value::nil() };
    Ok(res)
}

fn gsub_main(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    lfp: LFP,
) -> Result<(String, bool)> {
    match lfp.block() {
        None => {
            lfp.check_number_of_arguments(2)?;
            let given = self_val.expect_string(globals)?;
            let replace = lfp.arg(1).expect_string(globals)?;
            RegexpInner::replace_all(vm, globals, lfp.arg(0), &given, &replace)
        }
        Some(bh) => {
            lfp.check_number_of_arguments(1)?;
            let given = self_val.expect_string(globals)?;
            RegexpInner::replace_all_block(vm, globals, lfp.arg(0), &given, bh)
        }
    }
}

///
/// ### String#scan
///
/// - scan(pattern) -> [String] | [[String]]
/// - scan(pattern) {|s| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/scan.html]
#[monoruby_builtin]
fn scan(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let given = lfp.self_val().expect_string(globals)?;
    let vec = if let Some(s) = lfp.arg(0).is_str() {
        let re = RegexpInner::from_escaped(globals, &s)?;
        RegexpInner::find_all(vm, &re, &given)?
    } else if let Some(re) = lfp.arg(0).is_regex() {
        RegexpInner::find_all(vm, re, &given)?
    } else {
        return Err(MonorubyErr::argumenterr(
            "1st arg must be RegExp or String.",
        ));
    };
    match lfp.block() {
        None => Ok(Value::array_from_vec(vec)),
        Some(block) => {
            let temp_len = vm.temp_extend_form_slice(&vec);
            let res = scan_inner(vm, globals, block, vec);
            vm.temp_clear(temp_len);
            res?;
            Ok(lfp.self_val())
        }
    }
}

fn scan_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    block: BlockHandler,
    vec: Vec<Value>,
) -> Result<()> {
    let data = globals.get_block_data(vm.cfp(), block);
    for arg in vec {
        match arg.is_array() {
            Some(ary) => {
                vm.invoke_block(globals, &data, &ary)?;
            }
            None => {
                vm.invoke_block(globals, &data, &[arg])?;
            }
        }
    }
    Ok(())
}

///
/// ### String#match
///
/// - match(regexp, pos = 0) -> MatchData | nil
/// - match(regexp, pos = 0) {|m| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/match.html]
#[monoruby_builtin]
fn string_match(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(1..=2)?;
    let pos = match len {
        1 => 0usize,
        2 => match lfp.arg(1).coerce_to_i64(globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        },
        _ => unreachable!(),
    };
    let self_ = lfp.self_val();
    let given = self_.expect_string(globals)?;
    let re = lfp.arg(0).expect_regexp_or_string(globals)?;

    RegexpInner::match_one(vm, globals, &re, &given, lfp.block(), pos)
}

/// ### String#to_s
#[monoruby_builtin]
fn tos(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
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
fn length(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
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
fn ljust(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(1..=2)?;
    let padding = if len == 2 {
        let arg = lfp.arg(1);
        arg.expect_string(globals)?
    } else {
        " ".to_string()
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = lfp.arg(0).coerce_to_i64(globals)?;
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
fn rjust(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(1..=2)?;
    let padding = if len == 2 {
        let arg = lfp.arg(1);
        arg.expect_string(globals)?
    } else {
        " ".to_string()
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = lfp.arg(0).coerce_to_i64(globals)?;
    let str_len = lhs.chars().count();
    if width <= 0 || width as usize <= str_len {
        return Ok(Value::string(lhs.to_string()));
    }
    let tail = width as usize - str_len;
    Ok(Value::string(format!("{}{}", gen_pad(&padding, tail), lhs)))
}

///
/// ### String#lines
///
/// - lines(rs = $/, chomp: false) -> [String]
/// [NOT SUPPORTED] - lines(rs = $/, chomp: false) {|line| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/lines.html]
#[monoruby_builtin]
fn lines(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    if lfp.block().is_some() {
        return Err(MonorubyErr::runtimeerr("block is not supported."));
    }
    let receiver = lfp.self_val();
    let string = receiver.expect_string(globals)?;
    let iter = string
        .split_inclusive('\n')
        .map(|line| Value::string_from_str(line));
    Ok(Value::array_from_iter(iter))
}

///
/// ### String#bytes
///
/// - bytes -> [Integer]
/// - [NOT SUPPORTED]bytes {|byte| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/bytes.html]
#[monoruby_builtin]
fn bytes(_: &mut Executor, _: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    if lfp.block().is_some() {
        return Err(MonorubyErr::runtimeerr("block is not supported."));
    }
    let receiver = lfp.self_val();
    let iter = receiver
        .as_bytes()
        .iter()
        .map(|b| Value::integer(*b as i64));
    Ok(Value::array_from_iter(iter))
}

///
/// ### String#each_line
///
/// - each_line(rs = $/, [NOT SUPPORTED] chomp: false) {|line| ... } -> self
/// - [NOT SUPPORTED]each_line(rs = $/, chomp: false) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/each_line.html]
#[monoruby_builtin]
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(0..=1)?;
    let rs = if len == 1 {
        Cow::Owned(lfp.arg(0).expect_string(globals)?)
    } else {
        Cow::Borrowed("\n")
    };
    let bh = lfp.expect_block()?;
    let receiver = lfp.self_val();
    let string = receiver.expect_string(globals)?;
    //if len < 2 || !lfp.arg(1).as_bool() {
    let iter = string
        .split_inclusive(rs.as_ref())
        .map(|s| Value::string_from_str(s));
    vm.invoke_block_iter1(globals, bh, iter)?;
    /* } else {
        let iter = string.split(&rs).map(|s| Value::string_from_str(s));
        vm.invoke_block_iter1(globals, iter)?;
    }*/

    Ok(receiver)
}

///
/// ### String#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/empty=3f.html]
#[monoruby_builtin]
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_bytes().is_empty()))
}

///
/// ### String#to_i
///
/// - to_i(base = 10) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_i.html]
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(0..=1)?;
    let self_ = lfp.self_val();
    let s = self_.as_str();
    let radix = if len == 0 {
        10
    } else {
        match lfp.arg(0).expect_integer(globals)? {
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
fn to_sym(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
    let self_val = lfp.self_val();
    let sym = Value::symbol(IdentId::get_id(&self_val.as_str()));
    Ok(sym)
}

///
/// ### String#upcase
///
/// - upcase([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase.html]
#[monoruby_builtin]
fn upcase(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
    let self_val = lfp.self_val();
    let s = self_val.as_str().to_uppercase();
    Ok(Value::string_from_vec(s.into_bytes()))
}

//
/// ### String#downcase
///
/// - downcase([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/downcase.html]
#[monoruby_builtin]
fn downcase(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(0)?;
    let self_val = lfp.self_val();
    let s = self_val.as_str().to_lowercase();
    Ok(Value::string_from_vec(s.into_bytes()))
}

///
/// ### String#tr
///
/// - tr(pattern, replace) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/tr.html]
#[monoruby_builtin]
fn tr(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(2)?;
    let rec = lfp.self_val().expect_string(globals)?;
    let from = lfp.arg(0).expect_string(globals)?;
    let to = lfp.arg(1).expect_string(globals)?;
    assert_eq!(1, from.chars().count());
    assert_eq!(1, to.chars().count());
    let res = rec.replace(&from, &to);
    Ok(Value::string(res))
}

///
/// ### String#sum
///
/// - sum(bits = 16) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sum.html]
#[monoruby_builtin]
fn sum(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    lfp.check_number_of_arguments_range(0..=1)?;
    let bits = if len == 0 {
        16
    } else {
        lfp.arg(0).coerce_to_i64(globals)?
    };
    let self_val = lfp.self_val();
    let bytes = self_val.as_bytes();
    let mut sum = 0;
    for b in bytes.as_bytes() {
        sum += *b as u64;
    }
    Ok(Value::integer((sum & ((1 << bits) - 1)) as i64))
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
    fn string_cmp() {
        run_test(r##""aaa" <=> "xxx""##);
        run_test(r##""aaa" <=> "aaa""##);
        run_test(r##""xxx" <=> "aaa""##);
        run_test(r##""aaaa" <=> "aaa""##);
        run_test(r##""aaaa" <=> "xxx""##);
        run_test(r##""xxx" <=> "aaaa""##);
        run_test(r##""aaa" <=> "aaaa""##);
        run_test(r##""aaaa" <=> "aaa""##);
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
    fn scan() {
        run_test(r##""foobar".scan(/../)"##);
        run_test(r##""foobar".scan("o")"##);
        run_test(r##""foobarbazfoobarbaz".scan(/ba./)"##);
        run_test(
            r##"
        a = []
        "foobarbazfoobarbaz".scan(/ba./) {|s| a << s.upcase }
        a
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
    fn bytes() {
        run_test(r##""aa\nbb\ncc\n".bytes"##);
    }

    #[test]
    fn each_line() {
        run_test(
            r##"
    text = "Hello\nこんにちは\nWorld\n世界\n"
    res = []
    text.each_line do |line|
        res << line
    end
    res
    "##,
        );
        run_test(
            r##"
    text = "Hello\nこんにちは\nWorld\n世界\n"
    res = []
    text.each_line("o") do |line|
        res << line
    end
    res
    "##,
        );
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
    fn chomp() {
        run_test(r##""foo\n".chomp"##);
        run_test(r##""foo\n".chomp("\n")"##);
        run_test(r##""foo\r\n".chomp("\r\n")"##);
        run_test(r##""string\n".chomp(nil)"##);
        run_test(r##""foo\r\n\n".chomp("")"##);
        run_test(r##""foo\n\r\n".chomp("")"##);
        run_test(r##""foo\n\r\r".chomp("")"##);
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
        run_test(r"'AkrFj妖精u35]['.upcase");
        run_test(r"'AkrFj妖精u35]['.downcase");
    }

    #[test]
    fn sum() {
        run_test(r#"File.read("../LICENSE-MIT").sum"#);
        run_test(r#"File.read("../LICENSE-MIT").sum(11)"#);
    }
}
