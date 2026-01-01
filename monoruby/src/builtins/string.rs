use num::{BigInt, Zero};

use super::*;

//
// String class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("String", STRING_CLASS, ObjTy::STRING);
    globals.define_builtin_class_func_with_kw(
        STRING_CLASS,
        "new",
        string_new,
        0,
        1,
        false,
        &["encoding", "capacity"],
        false,
    );
    globals.define_builtin_class_func(STRING_CLASS, "try_convert", string_try_convert, 1);
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
    globals.define_builtin_func(STRING_CLASS, "*", mul, 1);
    globals.define_builtin_func(STRING_CLASS, "==", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "===", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(STRING_CLASS, "!=", ne, 1);
    globals.define_builtin_func(STRING_CLASS, ">=", ge, 1);
    globals.define_builtin_func(STRING_CLASS, ">", gt, 1);
    globals.define_builtin_func(STRING_CLASS, "<=", le, 1);
    globals.define_builtin_func(STRING_CLASS, "<", lt, 1);
    globals.define_builtin_func(STRING_CLASS, "<<", shl, 1);
    globals.define_builtin_func(STRING_CLASS, "%", rem, 1);
    globals.define_builtin_func(STRING_CLASS, "=~", match_, 1);
    globals.define_builtin_funcs_with(STRING_CLASS, "[]", &["slice"], index, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "[]=", index_assign, 2, 3, false);
    globals.define_builtin_func_rest(STRING_CLASS, "start_with?", start_with);
    globals.define_builtin_func(STRING_CLASS, "include?", include_, 1);
    globals.define_builtin_func(STRING_CLASS, "delete_prefix!", delete_prefix_, 1);
    globals.define_builtin_func(STRING_CLASS, "delete_prefix", delete_prefix, 1);
    globals.define_builtin_func_rest(STRING_CLASS, "end_with?", end_with);
    globals.define_builtin_func_with(STRING_CLASS, "split", split, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "slice!", slice_, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "chomp", chomp, 0, 1, false);
    globals.define_builtin_func_with(STRING_CLASS, "chomp!", chomp_, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "strip", strip, 0);
    globals.define_builtin_func(STRING_CLASS, "strip!", strip_, 0);
    globals.define_builtin_func(STRING_CLASS, "rstrip", rstrip, 0);
    globals.define_builtin_func(STRING_CLASS, "rstrip!", rstrip_, 0);
    globals.define_builtin_func(STRING_CLASS, "lstrip", lstrip, 0);
    globals.define_builtin_func(STRING_CLASS, "lstrip!", lstrip_, 0);
    globals.define_builtin_func_with(STRING_CLASS, "sub", sub, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "sub!", sub_, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "gsub", gsub, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "gsub!", gsub_, 1, 2, false);
    globals.define_builtin_func(STRING_CLASS, "scan", scan, 1);
    globals.define_builtin_func_with(STRING_CLASS, "match", string_match, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "match?", string_match_, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "index", string_index, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "rindex", string_rindex, 1, 2, false);
    globals.define_builtin_funcs(STRING_CLASS, "length", &["size"], length, 0);
    globals.define_builtin_funcs(STRING_CLASS, "bytesize", &[], bytesize, 0);
    globals.define_builtin_func(STRING_CLASS, "ord", ord, 0);
    globals.define_builtin_func_with(STRING_CLASS, "ljust", ljust, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "rjust", rjust, 1, 2, false);
    globals.define_builtin_func(STRING_CLASS, "lines", lines, 0);
    globals.define_builtin_func(STRING_CLASS, "bytes", bytes, 0);
    globals.define_builtin_func_with(STRING_CLASS, "each_line", each_line, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "empty?", empty, 0);
    globals.define_builtin_funcs(STRING_CLASS, "to_s", &["to_str"], tos, 0);
    globals.define_builtin_func_with(STRING_CLASS, "to_i", to_i, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "to_f", to_f, 0);
    globals.define_builtin_funcs(STRING_CLASS, "to_sym", &["intern"], to_sym, 0);
    globals.define_builtin_func(STRING_CLASS, "upcase", upcase, 0);
    globals.define_builtin_func(STRING_CLASS, "upcase!", upcase_, 0);
    globals.define_builtin_func(STRING_CLASS, "downcase", downcase, 0);
    globals.define_builtin_func(STRING_CLASS, "downcase!", downcase_, 0);
    globals.define_builtin_func_with(STRING_CLASS, "delete", delete, 0, 0, true);
    globals.define_builtin_func(STRING_CLASS, "tr", tr, 2);
    globals.define_builtin_func_rest(STRING_CLASS, "count", count);
    globals.define_builtin_func_with(STRING_CLASS, "sum", sum, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "replace", replace, 1);
    globals.define_builtin_func(STRING_CLASS, "chars", chars, 0);
    globals.define_builtin_func(STRING_CLASS, "each_char", each_char, 0);
    globals.define_builtin_func_with(STRING_CLASS, "center", center, 1, 2, false);
    globals.define_builtin_funcs(STRING_CLASS, "next", &["succ"], next, 0);
    globals.define_builtin_func(STRING_CLASS, "encoding", encoding, 0);
    globals.define_builtin_func(STRING_CLASS, "b", b, 0);
    globals.define_builtin_func(STRING_CLASS, "unpack1", unpack1, 1);
    globals.define_builtin_func(STRING_CLASS, "unpack", unpack, 1);
    globals.define_builtin_func(STRING_CLASS, "dump", dump, 0);
    globals.define_builtin_func(STRING_CLASS, "force_encoding", force_encoding, 1);
    globals.define_builtin_func(STRING_CLASS, "valid_encoding?", valid_encoding, 0);
    globals.define_builtin_func(STRING_CLASS, "ascii_only?", ascii_only, 0);

    let enc = globals.define_class_under_obj("Encoding");
    let val = Value::object(enc.id());
    globals
        .store
        .set_ivar(
            val,
            IdentId::_NAME,
            Value::string_from_str("#<Encoding:UTF-8>"),
        )
        .unwrap();
    globals
        .store
        .set_ivar(val, IdentId::_ENCODING, Value::string_from_str("UTF-8"))
        .unwrap();
    globals.set_constant(enc.id(), IdentId::UTF_8, val);
    let val = Value::object(enc.id());
    globals
        .store
        .set_ivar(
            val,
            IdentId::_NAME,
            Value::string_from_str("#<Encoding:BINARY (ASCII-8BIT)>"),
        )
        .unwrap();
    globals
        .store
        .set_ivar(
            val,
            IdentId::_ENCODING,
            Value::string_from_str("ASCII-8BIT"),
        )
        .unwrap();
    globals.set_constant(enc.id(), IdentId::ASCII_8BIT, val);
    globals.set_constant_by_str(enc.id(), "BINARY", val);
}

fn encoding_class(globals: &Globals) -> ClassId {
    globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
        .unwrap()
        .as_class_id()
}

///
/// ### String.new
///
/// - new(string = "") -> String
/// - [NOT SUPPORTED] new(string = "", encoding) -> String
/// - [NOT SUPPORTED] new(string = "", encoding, capacity) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/s/new.html]
#[monoruby_builtin]
fn string_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let s = match lfp.try_arg(0) {
        Some(string) => string.expect_string(globals)?,
        None => "".to_string(),
    };
    Ok(Value::string(s))
}

///
/// ### String.try_convert
///
/// - try_convert(obj) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/s/try_convert.html]
#[monoruby_builtin]
fn string_try_convert(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match lfp.arg(0).coerce_to_rstring(vm, globals) {
        Ok(rstring) => Ok(rstring.into()),
        Err(_) => Ok(Value::nil()),
    }
}

///
/// ### String#+
///
/// - self + other -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2b.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val().dup();
    let other = lfp.arg(0).coerce_to_rstring(vm, globals)?;
    self_.as_rstring_inner_mut().extend(&other)?;
    Ok(self_)
}

///
/// ### String#*
///
/// - self * times -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=2a.html]
#[monoruby_builtin]
fn mul(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let count = match lfp.arg(0).coerce_to_i64(globals)? {
        i if i < 0 => return Err(MonorubyErr::negative_argument()),
        i => i as usize,
    };

    let self_ = lfp.self_val();
    let res = Value::string_from_inner(self_.as_rstring_inner().repeat(count));
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
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    let b = equal(lhs, lfp.arg(0));
    Ok(Value::bool(b))
}

///
/// ### String#!=
///
/// - self != other -> bool
///
/// []
#[monoruby_builtin]
fn ne(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    let b = equal(lhs, lfp.arg(0));
    Ok(Value::bool(!b))
}

fn equal(lhs: &RStringInner, rhs: Value) -> bool {
    match rhs.is_rstring_inner() {
        Some(rhs) => lhs == rhs,
        None => false,
    }
}

fn string_cmp(lfp: Lfp) -> Result<Option<std::cmp::Ordering>> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    let res = lfp.arg(0).is_rstring().map(|rhs| lhs.cmp(&rhs));
    Ok(res)
}

fn string_cmp2(lfp: Lfp) -> Result<std::cmp::Ordering> {
    match string_cmp(lfp)? {
        Some(ord) => Ok(ord),
        None => Err(MonorubyErr::argumenterr(
            "comparison of String with non-String failed",
        )),
    }
}

///
/// ### String#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(string_cmp(lfp)?.map(Value::from_ord).unwrap_or_default())
}

///
/// ### Comparable#<=
///
/// - self <= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3c=3d.html]
#[monoruby_builtin]
fn le(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ord = string_cmp2(lfp)?;
    Ok(Value::bool(ord != std::cmp::Ordering::Greater))
}

///
/// ### Comparable#<=
///
/// - self < other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3c.html]
#[monoruby_builtin]
fn lt(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ord = string_cmp2(lfp)?;
    Ok(Value::bool(ord == std::cmp::Ordering::Less))
}

///
/// ### Comparable#<=
///
/// - self >= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3e=3d.html]
#[monoruby_builtin]
fn ge(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ord = string_cmp2(lfp)?;
    Ok(Value::bool(ord != std::cmp::Ordering::Less))
}

///
/// ### Comparable#<=
///
/// - self > other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3e.html]
#[monoruby_builtin]
fn gt(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ord = string_cmp2(lfp)?;
    Ok(Value::bool(ord == std::cmp::Ordering::Greater))
}

///
/// ### Strring#<<
///
/// - self << other -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    if let Some(other) = lfp.arg(0).is_rstring() {
        self_.as_rstring_inner_mut().extend(&other)?;
    } else if let Some(i) = lfp.arg(0).try_fixnum() {
        let ch = match u32::try_from(i) {
            Ok(ch) => ch,
            Err(_) => return Err(MonorubyErr::char_out_of_range(&globals.store, lfp.arg(0))),
        };
        let bytes = self_.as_rstring_inner_mut();
        if let Ok(ch) = u8::try_from(ch) {
            bytes.extend_from_slice_checked(&[ch])?;
        } else {
            bytes.extend_from_slice_checked(&ch.to_ne_bytes())?;
        }
        bytes.check_utf8()?;
    } else {
        return Err(MonorubyErr::no_implicit_conversion(
            globals,
            lfp.arg(0),
            STRING_CLASS,
        ));
    }
    Ok(self_)
}

///
/// ### String#%
///
/// - self % args -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=25.html]
#[monoruby_builtin]
fn rem(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arguments = match lfp.arg(0).try_array_ty() {
        Some(ary) => ary.to_vec(),
        None => vec![lfp.arg(0)],
    };
    let self_ = lfp.self_val();
    let format_str = globals.format_by_args(self_.as_str(), &arguments)?;

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
fn match_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let given = self_val.expect_str(globals)?;
    let regex = &lfp.arg(0).expect_regexp_or_string(globals)?;
    let res = match regex.find_one(vm, given)? {
        Some(r) => Value::integer(r.start as i64),
        None => Value::nil(),
    };
    Ok(res)
}

///
/// Convert `i` to the position of the char in the string with `len` chars.
///
/// Return None if `i` is out of range.
///
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

fn get_range(s: &str, index: usize, len: usize) -> std::ops::Range<usize> {
    let mut iter = s.char_indices().skip(index).peekable();
    let start = iter.peek().map(|(i, _)| *i).unwrap_or(s.len());
    let end = iter.nth(len).map(|(i, _)| i).unwrap_or(s.len());
    start..end
}

///
/// ### String#[]
///
/// - self[nth] -> String | nil
/// - self[nth, len] -> String | nil
/// [NOT SUPPORTED] - self[substr] -> String | nil
/// - self[regexp, nth = 0] -> String
/// [NOT SUPPORTED] - self[regexp, nth = 0] -> String
/// - self[range] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    let enc = lhs.encoding();
    if let Some(i) = lfp.arg(0).try_fixnum() {
        let index = match lhs.conv_char_index(i)? {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if let Some(arg1) = lfp.try_arg(1) {
            let len = match arg1.coerce_to_i64(globals)? {
                0 => return Ok(Value::string_from_str("")),
                i if i < 0 => return Ok(Value::nil()),
                i => i as usize,
            };
            let r = lhs.get_range(index, len);
            Ok(Value::string_from_inner(RStringInner::from_encoding(
                &lhs[r], enc,
            )))
        } else {
            let r = lhs.get_range(index, 1);
            if !r.is_empty() {
                Ok(Value::string_from_inner(RStringInner::from_encoding(
                    &lhs[r], enc,
                )))
            } else {
                Ok(Value::nil())
            }
        }
    } else if let Some(info) = lfp.arg(0).is_range() {
        let (start, end) = (
            info.start().expect_integer(globals)?,
            info.end().expect_integer(globals)? - info.exclude_end() as i64,
        );
        let (start, len) = match (lhs.conv_char_index(start)?, lhs.conv_char_index(end)?) {
            (Some(start), Some(end)) => {
                if start > end {
                    (start, 0)
                } else {
                    (start, end - start + 1)
                }
            }
            _ => return Ok(Value::nil()),
        };
        let r = lhs.get_range(start, len);
        Ok(Value::string_from_inner(RStringInner::from_encoding(
            &lhs[r], enc,
        )))
    } else if let Some(re) = lfp.arg(0).is_regex() {
        let nth = if let Some(i) = lfp.try_arg(1) {
            i.coerce_to_i64(globals)?
        } else {
            0
        };
        string_match_index(vm, lhs, &re, nth)
    } else {
        Err(MonorubyErr::argumenterr("Bad type for index."))
    }
}

fn string_match_index(
    vm: &mut Executor,
    s: &RStringInner,
    re: &RegexpInner,
    nth: i64,
) -> Result<Value> {
    let lhs = s.check_utf8()?;
    match re.captures(lhs, vm)? {
        None => Ok(Value::nil()),
        Some(captures) => {
            let len = captures.len() as i64;
            let nth = if nth >= 0 {
                nth as usize
            } else {
                match len + nth {
                    i if i > 0 => i as usize,
                    _ => return Ok(Value::nil()),
                }
            };
            match captures.get(nth) {
                Some(m) => Ok(Value::string_from_str(m.as_str())),
                None => Ok(Value::nil()),
            }
        }
    }
}

///
/// ### String#[]=
///
/// - self[nth] = val
/// - self[nth, len] = val
/// - [NOT SUPPORTED] self[substr] = val
/// - [NOT SUPPORTED] self[regexp, nth] = val
/// - [NOT SUPPORTED] self[regexp, name] = val
/// - [NOT SUPPORTED] self[regexp] = val
/// - [NOT SUPPORTED] self[range] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let (arg1, subst) = if let Some(arg2) = lfp.try_arg(2) {
        (Some(lfp.arg(1)), arg2.expect_string(globals)?)
    } else {
        (None, lfp.arg(1).expect_string(globals)?)
    };
    let self_ = lfp.self_val();
    let mut lhs = self_.expect_string(globals)?;
    let len = lhs.chars().count();
    if let Some(arg0) = lfp.arg(0).try_fixnum() {
        let start = match conv_index(arg0, len) {
            Some(i) => i,
            None => return Err(MonorubyErr::indexerr("index out of range.")),
        };
        let start = match lhs.char_indices().nth(start) {
            Some((i, _)) => i,
            None => return Err(MonorubyErr::indexerr("index out of range.")),
        };
        let len = if let Some(arg1) = arg1 {
            // self[nth, len] = val
            match arg1.expect_integer(globals)? {
                i if i < 0 => return Err(MonorubyErr::indexerr("negative length.")),
                i => i as usize,
            }
        } else {
            // self[nth] = val
            1
        };
        let end = if let Some((i, _)) = lhs.char_indices().nth(start + len) {
            i
        } else {
            lhs.len()
        };
        lhs.replace_range(start..end, &subst);
        *lfp.self_val().as_rstring_inner_mut() = RStringInner::from_string(lhs);
        Ok(lfp.self_val())
    } else {
        Err(MonorubyErr::argumenterr("Bad type for index."))
    }
}

///
/// Get the successor of `self_`.
///
/// https://hackmd.io/@zuby/BklVXzZ6w
///
pub fn str_next(self_: &str) -> String {
    use unicode_general_category::{GeneralCategory, get_general_category};
    #[derive(Clone, Copy)]
    struct Char(char);

    impl Char {
        fn from(ch: char) -> Self {
            Char(ch)
        }

        fn category(&self) -> Category {
            use GeneralCategory::*;
            match get_general_category(self.0) {
                DecimalNumber => Category::Digit,
                EnclosingMark | SpacingMark | NonspacingMark | LetterNumber | OtherLetter
                | ModifierLetter | LowercaseLetter | UppercaseLetter | TitlecaseLetter => {
                    Category::Alpha
                }
                _ => Category::Other,
            }
        }

        fn is_alnum(&self) -> bool {
            self.category() != Category::Other
        }

        fn is_digit(&self) -> bool {
            self.category() == Category::Digit
        }

        fn is_alpha(&self) -> bool {
            self.category() == Category::Alpha
        }

        fn succ(&self) -> Self {
            Self(std::char::from_u32(self.0 as u32 + 1).expect("Error occured in succ_char()"))
        }

        fn pred(&self) -> Self {
            Self(std::char::from_u32(self.0 as u32 - 1).expect("Error occured in succ_char()"))
        }

        fn succ_char(&self) -> (Self, bool) {
            let cat = self.category();
            let mut next = self.succ();
            if cat == next.category() {
                return (next, false);
            } else {
                next = next.succ();
                if cat == next.category() {
                    return (next, false);
                }
            }
            next = *self;
            while next.category() == cat {
                next = next.pred();
            }
            (next.succ(), true)
        }
    }

    #[derive(PartialEq, Eq)]
    enum Category {
        Alpha,
        Digit,
        Other,
    }

    #[derive(PartialEq)]
    enum Type {
        Alpha,
        Digit,
        NotDetermined,
    }

    if self_.is_empty() {
        return "".to_string();
    }
    let chars = self_.chars().map(Char::from);
    let mut buf = vec![];
    let mut carry_flag = true;
    let mut finish_flag = false;
    let mut last_alnum = 0;
    if self_.chars().map(Char::from).all(|c| !c.is_alnum()) {
        // non-alnum mode
        for c in chars.rev() {
            if carry_flag {
                buf.push(c.succ());
                carry_flag = false;
            } else {
                buf.push(c);
            }
        }
        return buf.iter().rev().map(|c| c.0).collect::<String>();
    }
    let mut pad_flag = false;
    let mut state = Type::NotDetermined;
    for c in chars.rev() {
        if finish_flag || !carry_flag {
            buf.push(c);
            continue;
        }
        if !c.is_alnum() {
            pad_flag = true;
            buf.push(c);
            continue;
        }
        if pad_flag {
            if c.is_digit() && state == Type::Alpha || c.is_alpha() && state == Type::Digit {
                finish_flag = true;
                buf.push(c);
                continue;
            }
            pad_flag = false;
        } else {
            if c.is_alpha() {
                state = Type::Alpha
            } else if c.is_digit() {
                state = Type::Digit
            };
        }
        let (next, is_carry) = c.succ_char();
        if is_carry {
            last_alnum = buf.len();
        } else {
            carry_flag = false;
        }
        buf.push(next);
    }
    if carry_flag {
        let c = buf[last_alnum];
        if c.is_digit() {
            buf.insert(last_alnum + 1, c.succ_char().0);
        } else {
            buf.insert(last_alnum + 1, c);
        }
    }
    buf.iter().rev().map(|c| c.0).collect::<String>()
}

///
/// ### String#start_with?
///
/// - start_with?(*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/start_with=3f.html]
#[monoruby_builtin]
fn start_with(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg0 = lfp.arg(0).as_array();
    for a in arg0.iter().map(|v| v.expect_str(globals)) {
        if string.starts_with(a?) {
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

///
/// ### String#include?
///
/// - include?(substr) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/include=3f.html]
#[monoruby_builtin]
fn include_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let substr = lfp.arg(0);
    let b = string.contains(substr.expect_str(globals)?);
    Ok(Value::bool(b))
}

///
/// ### String#delete_prefix!
///
/// - delete_prefix!(prefix) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/delete_prefix=21.html]
#[monoruby_builtin]
fn delete_prefix_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg0 = lfp.arg(0);
    let arg = arg0.expect_str(globals)?;
    if let Some(stripped) = string.strip_prefix(arg) {
        lfp.self_val().replace_str(stripped);
        Ok(lfp.self_val())
    } else {
        Ok(Value::nil())
    }
}

///
/// ### String#delete_prefix!
///
/// - delete_prefix(prefix) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/delete_prefix.html]
#[monoruby_builtin]
fn delete_prefix(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg0 = lfp.arg(0);
    let arg = arg0.expect_str(globals)?;
    if let Some(stripped) = string.strip_prefix(arg) {
        Ok(Value::string_from_str(stripped))
    } else {
        Ok(Value::string_from_str(string))
    }
}

///
/// ### String#end_with?
///
/// - end_with?(*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/end_with=3f.html]
#[monoruby_builtin]
fn end_with(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg0 = lfp.arg(0).as_array();
    for a in arg0.iter().map(|v| v.expect_str(globals)) {
        if string.ends_with(a?) {
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

///
/// ### String#split
///
/// - split(sep = $;, limit = 0) -> [String]
/// - split(sep = $;, limit = 0) {|s| ... } -> self
///
/// TODO: support nil and ' ' for sep.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/split.html]
#[monoruby_builtin]
fn split(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let lim = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_i64(globals)?
    } else {
        0
    };
    if let Some(sep) = lfp.arg(0).is_str() {
        let v: Vec<Value> = if sep == " " {
            match lim {
                lim if lim < 0 => {
                    let end_with = string.ends_with(|c: char| c.is_ascii_whitespace());
                    let mut v: Vec<_> = string
                        .split_ascii_whitespace()
                        .map(Value::string_from_str)
                        .collect();
                    if end_with {
                        v.push(Value::string_from_str(""))
                    }
                    v
                }
                0 => {
                    let mut vec: Vec<&str> = string.split_ascii_whitespace().collect();
                    while let Some(s) = vec.last() {
                        if s.is_empty() {
                            vec.pop().unwrap();
                        } else {
                            break;
                        }
                    }
                    vec.into_iter().map(Value::string_from_str).collect()
                }
                _ => string
                    .trim_start()
                    .splitn(lim as usize, |c: char| c.is_ascii_whitespace())
                    .map(|s| s.trim_start())
                    .map(Value::string_from_str)
                    .collect(),
            }
        } else if lim < 0 {
            string.split(sep).map(Value::string_from_str).collect()
        } else if lim == 0 {
            let mut vec: Vec<&str> = string.split(sep).collect();
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
                .splitn(lim as usize, sep)
                .map(Value::string_from_str)
                .collect()
        };
        match lfp.block() {
            Some(bh) => {
                let ary = Value::array_from_vec(v);
                vm.temp_push(ary);
                vm.invoke_block_iter1(globals, bh, ary.as_array().iter().cloned())?;
                vm.temp_pop();
                Ok(lfp.self_val())
            }
            None => Ok(Value::array_from_vec(v)),
        }
    } else if let Some(re) = lfp.arg(0).is_regex() {
        let all_cap = re.captures_iter(string);
        let mut cursor = 0usize;
        let mut res = vec![];
        let mut count = 0;
        'l: for c in all_cap {
            let c = c.unwrap();
            let mut iter = c.iter();
            let m = if let Some(m) = iter.next().unwrap() {
                m
            } else {
                continue;
            };
            count += 1;
            if count == lim {
                break 'l;
            } else if cursor != 0 || cursor != m.start() || !m.range().is_empty() {
                res.push(cursor..m.start());
            }
            for m in iter {
                let m = if let Some(m) = m {
                    m
                } else {
                    continue;
                };
                count += 1;
                if count == lim {
                    cursor = m.start();
                    break 'l;
                } else {
                    res.push(m.range())
                }
            }
            cursor = m.end();
        }
        if cursor <= string.len() {
            res.push(cursor..string.len());
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
        let iter = res.into_iter().map(|r| Value::string_from_str(&string[r]));
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
/// ### String#slice!
///
/// - slice!(nth) -> String
/// - slice!(pos, len) -> String
/// - [NOT SUPPRTED] slice!(substr) -> String
/// - slice!(regexp, nth = 0) -> String
/// - slice!(first..last) -> String
/// - slice!(first...last) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/slice=21.html]
#[monoruby_builtin]
fn slice_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn slice_sub(lfp: Lfp, mut lhs: String, r: std::ops::Range<usize>) -> Value {
        let res = Value::string_from_str(&lhs[r.clone()]);
        lhs.replace_range(r, "");
        *lfp.self_val().as_rstring_inner_mut() = RStringInner::from_string(lhs);
        res
    }
    let self_ = lfp.self_val();
    let lhs = self_.expect_string(globals)?;
    if let Some(i) = lfp.arg(0).try_fixnum() {
        let index = match conv_index(i, lhs.chars().count()) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if let Some(arg1) = lfp.try_arg(1) {
            let len = match arg1.coerce_to_i64(globals)? {
                i if i < 0 => return Ok(Value::nil()),
                i => i as usize,
            };
            let r = get_range(&lhs, index, len);
            Ok(slice_sub(lfp, lhs, r))
        } else {
            let r = get_range(&lhs, index, 1);
            if !r.is_empty() {
                Ok(slice_sub(lfp, lhs, r))
            } else {
                Ok(Value::nil())
            }
        }
    } else if let Some(info) = lfp.arg(0).is_range() {
        let len = lhs.chars().count();
        let (start, end) = (
            info.start().expect_integer(globals)?,
            info.end().expect_integer(globals)? - info.exclude_end() as i64,
        );
        let (start, len) = match (
            conv_index(start, len),
            if end >= 0 {
                Some(end as usize)
            } else if len as i64 + end >= 0 {
                Some((len as i64 + end) as usize)
            } else {
                None
            },
        ) {
            (Some(start), Some(end)) => {
                if start > end {
                    (start, 0)
                } else {
                    (start, end - start + 1)
                }
            }
            _ => return Ok(Value::nil()),
        };
        let r = get_range(&lhs, start, len);
        Ok(slice_sub(lfp, lhs, r))
    } else if let Some(info) = lfp.arg(0).is_regex() {
        let nth = if lfp.try_arg(1).is_none() {
            0
        } else {
            lfp.arg(1).coerce_to_i64(globals)?
        };
        match info.captures(&lhs, vm)? {
            None => Ok(Value::nil()),
            Some(captures) => {
                let len = captures.len() as i64;
                let nth = if nth >= 0 {
                    nth as usize
                } else {
                    match len + nth {
                        i if i > 0 => i as usize,
                        _ => return Ok(Value::nil()),
                    }
                };
                match captures.get(nth) {
                    Some(m) => {
                        let r = m.range();
                        Ok(slice_sub(lfp, lhs, r))
                    }
                    None => Ok(Value::nil()),
                }
            }
        }
    } else {
        Err(MonorubyErr::argumenterr("Bad type for index."))
    }
}

fn chomp_sub<'a>(self_: &'a str, rs: &str) -> &'a str {
    if rs.is_empty() {
        let mut s = self_;
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
        s
    } else if rs == "\n" {
        self_.trim_end_matches(&['\n', '\r'])
    } else {
        self_.trim_end_matches(rs)
    }
}

///
/// ### String#chomp
///
/// - chomp(rs = $/) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/chomp.html]
#[monoruby_builtin]
fn chomp(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let rs = if let Some(arg0) = &arg0 {
        if arg0.is_nil() {
            return Ok(lfp.self_val());
        }
        arg0.expect_str(globals)?
    } else {
        "\n"
    };

    let self_ = lfp.self_val();
    let self_s = self_.expect_str(globals)?;
    let res = chomp_sub(self_s, rs);
    Ok(Value::string_from_str(res))
}

///
/// ### String#chomp!
///
/// - chomp!(rs = $/) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/chomp.html]
#[monoruby_builtin]
fn chomp_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let rs = if let Some(arg0) = &arg0 {
        if arg0.is_nil() {
            return Ok(Value::nil());
        }
        arg0.expect_str(globals)?
    } else {
        "\n"
    };

    let self_ = lfp.self_val();
    let self_s = self_.expect_str(globals)?;
    let res = chomp_sub(self_s, rs);
    if res.len() == self_s.len() {
        Ok(Value::nil())
    } else {
        *lfp.self_val().as_rstring_inner_mut() = RStringInner::from_str(res);
        Ok(lfp.self_val())
    }
}

const STRIP: &[char] = &[' ', '\n', '\t', '\x0d', '\x0c', '\x0b', '\x00'];

///
/// ### String#strip
///
/// - strip -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/strip.html]
#[monoruby_builtin]
fn strip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_ = self_val
        .expect_str(globals)?
        .trim_end_matches(STRIP)
        .trim_start_matches(STRIP);
    Ok(Value::string_from_str(self_))
}

///
/// ### String#strip!
///
/// - strip! -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/strip=21.html]
#[monoruby_builtin]
fn strip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let orig = self_val.expect_str(globals)?;
    let self_ = orig.trim_end_matches(STRIP).trim_start_matches(STRIP);
    if self_.len() == orig.len() {
        return Ok(Value::nil());
    }
    lfp.self_val().replace_str(self_);
    Ok(lfp.self_val())
}

///
/// ### String#rstrip
///
/// - rstrip -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/rstrip.html]
#[monoruby_builtin]
fn rstrip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_ = self_val.expect_str(globals)?.trim_end_matches(STRIP);
    Ok(Value::string_from_str(self_))
}

///
/// ### String#rstrip!
///
/// - rstrip! -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/rstrip=21.html]
#[monoruby_builtin]
fn rstrip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let orig = self_val.expect_str(globals)?;
    let self_ = orig.trim_end_matches(STRIP);
    if self_.len() == orig.len() {
        return Ok(Value::nil());
    }
    lfp.self_val().replace_str(self_);
    Ok(lfp.self_val())
}

///
/// ### String#lstrip
///
/// - lstrip -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/lstrip.html]
#[monoruby_builtin]
fn lstrip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_ = self_val.expect_str(globals)?.trim_start_matches(STRIP);
    Ok(Value::string_from_str(self_))
}

///
/// ### String#lstrip!
///
/// - lstrip! -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/lstrip=21.html]
#[monoruby_builtin]
fn lstrip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let orig = self_val.expect_str(globals)?;
    let self_ = orig.trim_start_matches(STRIP);
    if self_.len() == orig.len() {
        return Ok(Value::nil());
    }
    lfp.self_val().replace_str(self_);
    Ok(lfp.self_val())
}

///
/// ### String#sub
///
/// - sub(pattern, replace) -> String
/// - sub(pattern) {|matched| .... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sub.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn sub_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    lfp: Lfp,
) -> Result<(String, bool)> {
    if let Some(arg1) = lfp.try_arg(1) {
        if lfp.block().is_some() {
            eprintln!("warning: default value argument supersedes block");
        }
        let given = self_val.expect_str(globals)?;
        let replace = arg1.expect_str(globals)?;
        RegexpInner::replace_one(vm, lfp.arg(0), given, replace)
    } else {
        match lfp.block() {
            None => Err(MonorubyErr::runtimeerr("Currently, not supported.")),
            Some(bh) => {
                let given = self_val.expect_str(globals)?;
                RegexpInner::replace_one_block(vm, globals, lfp.arg(0), given, bh)
            }
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
fn gsub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn gsub_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    lfp: Lfp,
) -> Result<(String, bool)> {
    if let Some(arg1) = lfp.try_arg(1) {
        if lfp.block().is_some() {
            eprintln!("warning: default value argument supersedes block");
        }
        let given = self_val.expect_str(globals)?;
        let replace = arg1.expect_str(globals)?;
        RegexpInner::replace_all(vm, lfp.arg(0), given, replace)
    } else {
        match lfp.block() {
            None => Err(MonorubyErr::runtimeerr("Currently, not supported.")),
            Some(bh) => {
                let given = self_val.expect_str(globals)?;
                RegexpInner::replace_all_block(vm, globals, lfp.arg(0), given, bh)
            }
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
fn scan(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let vec = if let Some(s) = lfp.arg(0).is_str() {
        let re = RegexpInner::from_escaped(s)?;
        re.scan(vm, given)?
    } else if let Some(re) = lfp.arg(0).is_regex() {
        re.scan(vm, given)?
    } else {
        return Err(MonorubyErr::argumenterr(
            "1st arg must be RegExp or String.",
        ));
    };
    match lfp.block() {
        None => Ok(Value::array_from_vec(vec)),
        Some(block) => {
            let ary = Value::array_from_vec(vec);
            vm.temp_push(ary);
            let res = scan_inner(vm, globals, block, &ary.as_array());
            vm.temp_pop();
            res?;
            Ok(lfp.self_val())
        }
    }
}

fn scan_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    block: BlockHandler,
    vec: &[Value],
) -> Result<()> {
    let data = vm.get_block_data(globals, block)?;
    for arg in vec {
        match arg.try_array_ty() {
            Some(ary) => {
                vm.invoke_block(globals, &data, &ary)?;
            }
            None => {
                vm.invoke_block(globals, &data, &[*arg])?;
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
fn string_match(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let pos = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.coerce_to_i64(globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        }
    } else {
        0usize
    };
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let re = lfp.arg(0).expect_regexp_or_string(globals)?;

    RegexpInner::match_one(vm, globals, &re, given, lfp.block(), pos)
}

///
/// ### String#match?
///
/// - match?(regexp, pos = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/match=3f.html]
#[monoruby_builtin]
fn string_match_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let pos = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.coerce_to_i64(globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        }
    } else {
        0usize
    };
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let re = lfp.arg(0).expect_regexp_or_string(globals)?;

    let res = RegexpInner::match_one(vm, globals, &re, given, lfp.block(), pos)?;
    Ok(Value::bool(!res.is_nil()))
}

///
/// ### String#index
///
/// - index(pattern, pos = 0) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/index.html]
#[monoruby_builtin]
fn string_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let char_pos = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_i64(globals)?
    } else {
        0
    };
    let self_ = lfp.self_val();
    let given = self_.is_rstring().unwrap();
    let re = lfp.arg(0).expect_regexp_or_string(globals)?;

    let char_pos = match given.conv_char_index(char_pos)? {
        Some(pos) => pos,
        None => return Ok(Value::nil()),
    };

    let s = given.check_utf8()?;
    let byte_pos = s.char_indices().nth(char_pos).unwrap().0;
    match re.captures_from_pos(s, byte_pos, vm)? {
        None => Ok(Value::nil()),
        Some(captures) => {
            let start = captures.get(0).unwrap().start();
            let char_pos = given.byte_to_char_index(start)?;
            Ok(Value::integer(char_pos as i64))
        }
    }
}

///
/// ### String#rindex
///
/// - rindex(pattern, pos = self.size) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/rindex.html]
#[monoruby_builtin]
fn string_rindex(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let given = self_.is_rstring().unwrap();
    let re = lfp.arg(0).expect_regexp_or_string(globals)?;
    let max_char_pos = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_i64(globals)?
    } else {
        -1
    };
    let max_char_pos = match given.conv_char_index2(max_char_pos)? {
        Some(pos) => pos,
        None => return Ok(Value::nil()),
    };

    let s = given.check_utf8()?;

    let mut last_byte_pos = match re.captures_from_pos(s, 0, vm)? {
        None => {
            return Ok(Value::nil());
        }
        Some(captures) => captures.get(0).unwrap().start(),
    };

    // Option<(char_pos:usize, byte_pos:usize)>
    let mut last_char_pos = if last_byte_pos == 0 { Some(0) } else { None };
    for (char_pos, (byte_pos, _)) in s.char_indices().enumerate() {
        if last_byte_pos == byte_pos {
            if char_pos > max_char_pos {
                return Ok(match last_char_pos {
                    Some(pos) => Value::integer(pos as i64),
                    None => Value::nil(),
                });
            }
            last_char_pos = Some(char_pos);
        }
        if last_byte_pos >= byte_pos {
            continue;
        }
        match re.captures_from_pos(s, byte_pos, vm)? {
            None => return Ok(Value::integer(last_char_pos.unwrap() as i64)),
            Some(captures) => {
                last_byte_pos = captures.get(0).unwrap().start();
                if last_byte_pos == byte_pos {
                    if char_pos > max_char_pos {
                        return Ok(match last_char_pos {
                            Some(pos) => Value::integer(pos as i64),
                            None => Value::nil(),
                        });
                    }
                    last_char_pos = Some(char_pos);
                }
            }
        }
    }
    Ok(Value::integer(last_char_pos.unwrap() as i64))
}

///
/// ### String#to_s
///
/// - to_s -> String
/// - to_str -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_s.html]
#[monoruby_builtin]
fn tos(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn length(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let length = lfp.self_val().as_rstring_inner().char_length()?;
    Ok(Value::integer(length as i64))
}

///
/// ### String#bytesize
///
/// - bytesize -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/bytesize.html]
#[monoruby_builtin]
fn bytesize(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let length = lfp.self_val().as_rstring_inner().len();
    Ok(Value::integer(length as i64))
}

///
/// ### String#ord
///
/// - ord -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/ord.html]
#[monoruby_builtin]
fn ord(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::integer(
        lfp.self_val().as_rstring_inner().first_code()? as _,
    ))
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
fn ljust(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding = if let Some(arg1) = &arg1 {
        arg1.expect_str(globals)?
    } else {
        " "
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
    Ok(Value::string(format!("{}{}", lhs, gen_pad(padding, tail))))
}

///
/// ### String#rjust
///
/// - rjust(width, padding = ' ') -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/rjust.html]
#[monoruby_builtin]
fn rjust(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding = if let Some(arg1) = &arg1 {
        arg1.expect_str(globals)?
    } else {
        " "
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
    Ok(Value::string(format!("{}{}", gen_pad(padding, tail), lhs)))
}

///
/// ### String#lines
///
/// - lines([NOT SUPPORTED] rs = $/, [NOT SUPPORTED] chomp: false) -> [String]
/// - [NOT SUPPORTED] lines(rs = $/, chomp: false) {|line| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/lines.html]
#[monoruby_builtin]
fn lines(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if lfp.block().is_some() {
        return Err(MonorubyErr::runtimeerr("block is not supported."));
    }
    let receiver = lfp.self_val();
    let string = receiver.expect_str(globals)?;
    let iter = string.split_inclusive('\n').map(Value::string_from_str);
    Ok(Value::array_from_iter(iter))
}

///
/// ### String#bytes
///
/// - bytes -> [Integer]
/// - bytes {|byte| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/bytes.html]
#[monoruby_builtin]
fn bytes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let receiver = lfp.self_val();
    let iter = receiver
        .as_rstring_inner()
        .iter()
        .map(|b| Value::integer(*b as i64));
    if let Some(bh) = lfp.block() {
        vm.invoke_block_iter1(globals, bh, iter)?;
        Ok(lfp.self_val())
    } else {
        Ok(Value::array_from_iter(iter))
    }
}

///
/// ### String#each_line
///
/// - each_line(rs = $/, [NOT SUPPORTED] chomp: false) {|line| ... } -> self
/// - [NOT SUPPORTED]each_line(rs = $/, chomp: false) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/each_line.html]
#[monoruby_builtin]
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let rs = if let Some(arg0) = &arg0 {
        arg0.expect_str(globals)?
    } else {
        "\n"
    };
    let bh = lfp.expect_block()?;
    let receiver = lfp.self_val();
    let string = receiver.expect_str(globals)?;
    let iter = string.split_inclusive(rs).map(Value::string_from_str);
    vm.invoke_block_iter1(globals, bh, iter)?;

    Ok(receiver)
}

///
/// ### String#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/empty=3f.html]
#[monoruby_builtin]
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_rstring_inner().is_empty()))
}

///
/// ### String#to_f
///
/// - to_f -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_f.html]
#[monoruby_builtin]
fn to_f(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.expect_str(globals)?;
    let f = parse_f64(s).0;
    Ok(Value::float(f))
}

///
/// ### String#to_i
///
/// - to_i(base = 10) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_i.html]
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.as_str();
    let radix = if let Some(arg0) = lfp.try_arg(0) {
        match arg0.expect_integer(globals)? {
            n if !(2..=36).contains(&n) => {
                return Err(MonorubyErr::argumenterr(format!("invalid radix {n}")));
            }
            n => n as u32,
        }
    } else {
        10
    };
    if let Some((i, negative)) = parse_i64(s, radix) {
        if negative {
            if let Some(i) = i.checked_neg() {
                Ok(Value::integer(i))
            } else {
                Ok(Value::bigint(-BigInt::from(i)))
            }
        } else {
            Ok(Value::integer(i))
        }
    } else {
        Ok(Value::bigint(parse_bigint(s, radix)))
    }
}

fn parse_i64(s: &str, radix: u32) -> Option<(i64, bool)> {
    let mut i = 0i64;
    let mut sign = None;
    let mut allow_underscore = false;
    let iter = s.chars().skip_while(|c| c.is_ascii_whitespace());
    for c in iter {
        if allow_underscore && c == '_' {
            allow_underscore = false;
            continue;
        }

        if sign.is_none() {
            match c {
                '-' => {
                    sign = Some(-1);
                    allow_underscore = false;
                    continue;
                }
                '+' => {
                    sign = Some(1);
                    allow_underscore = false;
                    continue;
                }
                _ => {
                    sign = Some(1);
                }
            };
        }
        allow_underscore = true;
        let d = match c.to_digit(radix) {
            Some(d) => d,
            None => break,
        };
        i = i.checked_mul(radix as i64)?.checked_add(d as i64)?;
    }
    Some((i, sign.unwrap_or(1) == -1))
}

fn parse_bigint(s: &str, radix: u32) -> BigInt {
    let mut i = BigInt::zero();
    let mut sign = None;
    let mut allow_underscore = false;
    let iter = s.chars().skip_while(|c| c.is_ascii_whitespace());
    for c in iter {
        if allow_underscore && c == '_' {
            allow_underscore = false;
            continue;
        }

        if sign.is_none() {
            match c {
                '-' => {
                    sign = Some(-1);
                    allow_underscore = false;
                    continue;
                }
                '+' => {
                    sign = Some(1);
                    allow_underscore = false;
                    continue;
                }
                _ => {
                    sign = Some(1);
                }
            };
        }
        allow_underscore = true;
        let d = match c.to_digit(radix) {
            Some(d) => d,
            None => break,
        };
        i = i * radix + d;
    }

    if sign == Some(-1) { -i } else { i }
}

///
/// ### String#intern
///
/// - intern -> Symbol
/// - to_sym -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/intern.html]
#[monoruby_builtin]
fn to_sym(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let sym = Value::symbol_from_str(self_val.as_str());
    Ok(sym)
}

///
/// ### String#upcase
///
/// - upcase([NOT SUPPORTED] *options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase.html]
#[monoruby_builtin]
fn upcase(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.as_str().to_uppercase();
    Ok(Value::string(s))
}

///
/// ### String#upcase!
///
/// - upcase!([NOT SUPPORTED] *options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase=21.html]
#[monoruby_builtin]
fn upcase_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let s = self_val.as_str().to_uppercase();
    let changed = &s != self_val.as_str();
    self_val.replace_string(s);

    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

//
/// ### String#downcase
///
/// - downcase([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/downcase.html]
#[monoruby_builtin]
fn downcase(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.as_str().to_lowercase();
    Ok(Value::string(s))
}

///
/// ### String#downcase!
///
/// - downcase!([NOT SUPPORTED] *options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/downcase=21.html]
#[monoruby_builtin]
fn downcase_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let s = self_val.as_str().to_lowercase();
    let changed = &s != self_val.as_str();
    self_val.replace_string(s);

    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Tr {
    elems: Vec<TrElement>,
    exclude: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TrElement {
    Char(char),
    Range(char, char),
}

impl Tr {
    fn from_str(s: &str) -> Result<Tr> {
        let mut elems = Vec::new();
        let mut chars = s.chars().peekable();
        let exclude = if let Some('^') = chars.peek() {
            assert_eq!('^', chars.next().unwrap());
            true
        } else {
            false
        };
        while let Some(c) = chars.next() {
            if c == '\\' {
                if let Some(next) = chars.next() {
                    elems.push(TrElement::Char(next));
                } else {
                    elems.push(TrElement::Char(c));
                }
            } else if let Some('-') = chars.peek() {
                assert_eq!('-', chars.next().unwrap());
                if let Some(end) = chars.next() {
                    if c as u32 > end as u32 {
                        return Err(MonorubyErr::argumenterr(format!(
                            "Invalid range \"{c}-{end}\" in string transliteration",
                        )));
                    }
                    elems.push(TrElement::Range(c, end));
                } else {
                    elems.push(TrElement::Char(c));
                    elems.push(TrElement::Char('-'));
                }
            } else {
                elems.push(TrElement::Char(c));
            }
        }
        Ok(Tr { elems, exclude })
    }

    fn check(&self, c: char) -> bool {
        for elem in &self.elems {
            match elem {
                TrElement::Char(ch) => {
                    if *ch == c {
                        return !self.exclude;
                    }
                }
                TrElement::Range(start, end) => {
                    if *start <= c && c <= *end {
                        return !self.exclude;
                    }
                }
            }
        }
        self.exclude
    }
}

#[test]
fn tr_test() {
    assert_eq!(
        Tr::from_str("abc-def").unwrap(),
        Tr {
            elems: vec![
                TrElement::Char('a'),
                TrElement::Char('b'),
                TrElement::Range('c', 'd'),
                TrElement::Char('e'),
                TrElement::Char('f')
            ],
            exclude: false
        }
    );

    assert_eq!(
        Tr::from_str("-def").unwrap(),
        Tr {
            elems: vec![
                TrElement::Char('-'),
                TrElement::Char('d'),
                TrElement::Char('e'),
                TrElement::Char('f')
            ],
            exclude: false
        }
    );

    assert_eq!(
        Tr::from_str("^-def").unwrap(),
        Tr {
            elems: vec![
                TrElement::Char('-'),
                TrElement::Char('d'),
                TrElement::Char('e'),
                TrElement::Char('f')
            ],
            exclude: true
        }
    );

    assert_eq!(
        Tr::from_str("--def").unwrap(),
        Tr {
            elems: vec![
                TrElement::Range('-', 'd'),
                TrElement::Char('e'),
                TrElement::Char('f')
            ],
            exclude: false
        }
    );

    let res = Tr::from_str("a-z");
    assert_eq!(
        res.unwrap(),
        Tr {
            elems: vec![TrElement::Range('a', 'z')],
            exclude: false
        }
    );

    let res = Tr::from_str("^a-z");
    assert_eq!(
        res.unwrap(),
        Tr {
            elems: vec![TrElement::Range('a', 'z')],
            exclude: true
        }
    );
    let res = Tr::from_str("a\\-z\\");
    assert_eq!(
        res.unwrap(),
        Tr {
            elems: vec![
                TrElement::Char('a'),
                TrElement::Char('-'),
                TrElement::Char('z'),
                TrElement::Char('\\'),
            ],
            exclude: false
        }
    );
}

///
/// ### String#delete
///
/// - delete(*strs) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/delete.html]
#[monoruby_builtin]
fn delete(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut res = lfp.self_val().as_str().to_string();
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    let pred = args
        .iter()
        .map(|arg| arg.expect_str(globals))
        .flat_map(|arg| arg.map(|arg| Tr::from_str(arg)))
        .collect::<Result<Vec<Tr>>>()?;

    res.retain(|c| !pred.iter().all(|tr| tr.check(c)));

    Ok(Value::string(res))
}

///
/// ### String#tr
///
/// - tr(pattern, replace) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/tr.html]
#[monoruby_builtin]
fn tr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    // TODO: support tr(1)
    let self_ = lfp.self_val();
    let arg0 = lfp.arg(0);
    let arg1 = lfp.arg(1);
    let rec = self_.expect_str(globals)?;
    let from = arg0.expect_str(globals)?;
    let to = arg1.expect_str(globals)?;
    let res = rec.replace(from, to);
    Ok(Value::string(res))
}

///
/// ### String#count
///
/// - count(*chars) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/count.html]
#[monoruby_builtin]
fn count(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let self_ = lfp.self_val();
    let target = self_.as_str();
    let mut c = 0;
    for ch in target.chars() {
        for arg in args.iter() {
            let s = arg.expect_str(globals)?;
            if s.chars().any(|c2| c2 == ch) {
                c += 1;
                break;
            }
        }
    }
    Ok(Value::integer(c as i64))
}

///
/// ### String#sum
///
/// - sum(bits = 16) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/sum.html]
#[monoruby_builtin]
fn sum(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bits = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64(globals)?
    } else {
        16
    };
    let self_val = lfp.self_val();
    let bytes = self_val.as_rstring_inner();
    let mut sum = 0;
    for b in bytes.as_bytes() {
        sum += *b as u64;
    }
    Ok(Value::integer((sum & ((1 << bits) - 1)) as i64))
}

///
/// String#replace
///
/// - replace(other) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/replace.html]
#[monoruby_builtin]
fn replace(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.self_val().replace_str(lfp.arg(0).expect_str(globals)?);
    Ok(lfp.self_val())
}

///
/// ### String#chars
///
/// - chars -> [String]
/// - chars {|cstr| block } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/chars.html]
#[monoruby_builtin]
fn chars(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    let iter = recv.chars().map(|c| Value::string(c.to_string()));
    if let Some(bh) = lfp.block() {
        vm.invoke_block_map1(globals, bh, iter, None)?;
        Ok(lfp.self_val())
    } else {
        Ok(Value::array_from_iter(iter))
    }
}

///
/// ### String#each_char
///
/// - each_char {|cstr| block } -> self
/// - each_char -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/each_char.html]
#[monoruby_builtin]
fn each_char(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    if let Some(bh) = lfp.block() {
        let iter = recv.chars().map(|c| Value::string(c.to_string()));
        vm.invoke_block_iter1(globals, bh, iter)?;
        Ok(lfp.self_val())
    } else {
        vm.generate_enumerator(IdentId::get_id("each_char"), lfp.self_val(), vec![])
    }
}

///
/// ### String#center
///
/// - center(width, padding = ' ') -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/center.html]
#[monoruby_builtin]
fn center(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding = if let Some(arg) = &arg1 {
        arg.expect_str(globals)?
    } else {
        " "
    };
    if padding.is_empty() {
        return Err(MonorubyErr::argumenterr("Zero width padding."));
    };
    let lhs = lfp.self_val();
    let width = lfp.arg(0).coerce_to_i64(globals)?;
    let str_len = lhs.as_str().chars().count();
    if width <= 0 || width as usize <= str_len {
        return Ok(lhs.dup());
    }
    let head = (width as usize - str_len) / 2;
    let tail = width as usize - str_len - head;
    Ok(Value::string(format!(
        "{}{}{}",
        gen_pad(padding, head),
        lhs.as_str(),
        gen_pad(padding, tail)
    )))
}

///
/// ### String#next
///
/// - succ -> String
/// - next -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/next.html]
#[monoruby_builtin]
fn next(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    let res = Value::string(str_next(recv));
    Ok(res)
}

///
/// ### String#encoding
///
/// - encoding -> Encoding
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/encoding.html]
#[monoruby_builtin]
fn encoding(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let enc = self_.as_rstring_inner().encoding();
    let enc_class = vm
        .get_constant_checked(globals, OBJECT_CLASS, IdentId::get_id("Encoding"))?
        .expect_class(globals)?
        .id();
    let res = match enc {
        Encoding::Ascii8 => {
            vm.get_constant_checked(globals, enc_class, IdentId::get_id("ASCII_8BIT"))?
        }
        Encoding::Utf8 => vm.get_constant_checked(globals, enc_class, IdentId::get_id("UTF_8"))?,
    };
    Ok(res)
}

///
/// ### String#b
///
/// - b -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/b.html]
#[monoruby_builtin]
fn b(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut res = lfp.self_val().dup();
    res.as_rstring_inner_mut().set_encoding(Encoding::Ascii8);
    Ok(res)
}

///
/// ### String#unpack
///
/// - unpack(template) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/unpack.html]
#[monoruby_builtin]
fn unpack(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    rvalue::unpack(
        self_.as_rstring_inner(),
        lfp.arg(0).expect_str(globals)?,
        false,
    )
}

///
/// ### String#unpack1
///
/// - unpack1(format) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/unpack1.html]
#[monoruby_builtin]
fn unpack1(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    rvalue::unpack(
        self_.as_rstring_inner(),
        lfp.arg(0).expect_str(globals)?,
        true,
    )
}

///
/// ### String#dump
///
/// - dump -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/dump.html]
#[monoruby_builtin]
fn dump(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::string(format!(
        r#""{}""#,
        lfp.self_val().as_rstring_inner().dump()
    )))
}

///
/// ### String#force_encoding
///
/// - force_encoding(encoding) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/force_encoding.html]
#[monoruby_builtin]
fn force_encoding(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let enc = if let Some(s) = arg0.is_str() {
        Encoding::try_from_str(s)?
    } else if arg0.class() == encoding_class(globals) {
        let s = globals.store.get_ivar(arg0, IdentId::_ENCODING).unwrap();
        Encoding::try_from_str(s.as_str())?
    } else {
        return Err(MonorubyErr::argumenterr("1st arg must be String."));
    };
    lfp.self_val().as_rstring_inner_mut().set_encoding(enc);
    Ok(lfp.self_val())
}

///
/// ### String#valid_encoding?
///
/// - valid_encoding? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/valid_encoding=3f.html]
#[monoruby_builtin]
fn valid_encoding(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_rstring_inner().valid()))
}

///
/// ### String#ascii_only?
///
/// - ascii_only? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/ascii_only=3f.html]
#[monoruby_builtin]
fn ascii_only(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_rstring_inner().is_ascii()))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn string_new() {
        run_test(r##"String.new"##);
        run_test(r##"String.new("Ruby")"##);
    }

    #[test]
    fn string_try_convert() {
        run_test(r##"String.try_convert("str")"##);
        run_test(r##"String.try_convert(/re/)"##);
    }

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
    fn string_shl() {
        run_test(
            r##"
            a = "Ruby"
            a << " on Rails"
            a << 100
            a
        "##,
        );
        /*run_test(
            r##"
            a = "Ruby"
            a << 1024
            a
        "##,
        );*/
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
        run_binop_tests2(
            &[
                "'a'", "'aa'", "'aaa'", "'x'", "'xx'", "'xxx'", "'山田'", "'山川'",
            ],
            &["<=", "<", ">", ">=", "<=>", "==", "!="],
            &[
                "'a'", "'aa'", "'aaa'", "'x'", "'xx'", "'xxx'", "'山田'", "'山川'",
            ],
        );
    }

    #[test]
    fn string_ord() {
        run_test("'ruby'.ord");
        run_test("'ルビー'.ord");
        run_test_error("''.ord");
        run_test(
            r#"
            a = ""
            a << 1
            a << "Ruby"
            a.ord
        "#,
        );
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
    fn slice() {
        run_test(r##"s = "this is a string"; [s.slice!(2), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-2), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-20), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(20), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(3..6), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(3...6), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-3...6), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-12...1), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-12...6), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(-12...50), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(30...50), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s.*t/), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s/), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s/, 2), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s/, 5), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s/, -1), s]"##);
        run_test(r##"s = "this is a string"; [s.slice!(/s/, -10), s]"##);
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
        run_test(
            r##"
        res = "abcdefgdef".gsub(/def/, "!!")
        [res, $&, $']
        "##,
        );
        run_test(
            r##"
        res = "2.5".gsub(".", ",")
        [res, $&, $']
        "##,
        );
        run_test(
            r##"
        res = "abcdefgdddefjklefl".gsub(/d*ef/) {
            |matched| "+" + matched + "+"
        }
        [res, $&, $']
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
        res = s.gsub!(/def/, "!!")
        [res, s, $&, $']
        "##,
        );
        run_test(
            r##"
        s = "2.5.3..75841."
        res = s.gsub!(".", ",")
        [res, s, $&, $']
        "##,
        );
        run_test(
            r##"
        s = "abcdefghdefr"
        res = s.gsub!(/def1/, "!!")
        [res, s, $&, $']
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
    fn delete() {
        run_test(r##""abcdefg".delete("b")"##);
        run_test(r##""abcdefg".delete("b", "c")"##);
        run_test(r##""abcdefg".delete("b", "c", "d")"##);
        run_test(r##""abcdefg".delete("b", "c", "d", "e")"##);
        run_test(r##""abcdefg".delete("b", "c", "d", "e", "f")"##);
        run_test(r##""abcdefg".delete("b", "c", "d", "e", "f", "g")"##);
        run_test(r##""abcdefg".delete("bcd")"##);
        run_test(r##""abcdefg".delete("bcd", "efg")"##);
        run_test(r##""123456789".delete("2378")"##);
        run_test(r##""123456789".delete("2-8", "^4-6")"##);
        run_test(r##""12345-6789".delete("-8")"##);
        run_test(r##""12345-6789".delete("8-")"##);
        run_test(r##""12345-6789".delete("--")"##);
        run_test(r##""12345-6789".delete("---")"##);
        run_test(r##""345678-\\".delete("3\-6")"##);
        run_test(r##""345678-\\".delete("3\\\-6")"##);
        run_test(r##""345678-\\".delete("3\\-6")"##);
        run_test(r##""345678-\\".delete("3\\")"##);
        run_test_error(r##""abcd".delete"##);
        run_test_error(r##""abcd".delete("d-a")"##);
    }

    #[test]
    fn strip() {
        run_test(r##""  abc  \r\n".strip"##);
        run_test(r##""abc\n".strip"##);
        run_test(r##""  abc".strip"##);
        run_test(r##""abc".strip"##);
        run_test(r##""  \0  abc  \0".strip"##);
        run_test(
            r##"
        str = "abc\n"
        str.strip
        str
        "##,
        );
        run_test(
            r##"
        str = "  abc\r\n"
        [str.strip!, str]
        "##,
        );
        run_test(
            r##"
        str = "abc"
        [str.strip!, str]
        "##,
        );
        run_test(
            r##"
        str = "  \0  abc  \0"
        [str.strip!, str]
        "##,
        );
        run_test(r##""  abc\n".lstrip"##);
        run_test(r##""\t abc\n".lstrip"##);
        run_test(r##""abc\n".lstrip"##);
        run_test(
            r##"
        str = "abc\n"
        str.lstrip
        str
        "##,
        );
        run_test(
            r##"
        str = "  abc\r\n"
        [str.lstrip!, str]
        "##,
        );
        run_test(
            r##"
        str = "abc"
        [str.lstrip!, str]
        "##,
        );
        run_test(
            r##"
        str = "  \0  abc  \0"
        [str.lstrip!, str]
        "##,
        );
        run_test(r##""  abc\n".rstrip"##);
        run_test(r##""  abc \t\r\n\0".rstrip"##);
        run_test(r##""  abc".rstrip"##);
        run_test(r##""  abc\0 ".rstrip"##);
        run_test(
            r##"
        str = "abc\n"
        str.rstrip
        str
        "##,
        );
        run_test(
            r##"
        str = "  abc\r\n"
        [str.rstrip!, str]
        "##,
        );
        run_test(
            r##"
        str = "abc"
        [str.rstrip!, str]
        "##,
        );
        run_test(
            r##"
        str = "  \0  abc  \0"
        [str.rstrip!, str]
        "##,
        );
    }

    #[test]
    fn scan() {
        run_test(
            r##"
        res = "foobar".scan(/../)
        [res, $&, $']
        "##,
        );
        run_test(
            r##"
        res = "foobar".scan("o")
        [res, $&, $']
        "##,
        );
        run_test(
            r##"
        res = "foobarbazfoobarbaz".scan(/ba./)
        [res, $&, $']
        "##,
        );
        run_test(
            r##"
        a = []
        res = "foobarbazfoobarbaz".scan(/ba./) {|s| a << s.upcase }
        [res, a, $&, $']
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

        run_test(r##""Ruby".match?(/R.../)"##);
        run_test(r##""Ruby".match?(/R.../, 1)"##);
        run_test(r##""Ruby".match?(/P.../)"##);
    }

    #[test]
    fn length() {
        run_test(r##""本日は快晴なり".length"##);
        run_test(r##""本日はsunnyなり".length"##);
        run_test(r##""本日は快晴なり".bytesize"##);
        run_test(r##""本日はsunnyなり".bytesize"##);
    }

    #[test]
    fn index() {
        run_test(r##""超時空要塞".index(/時/)"##);
        run_test(r##""超時空要塞".index(/海/)"##);
        run_test(r##""超時空要塞".index(/時/, 3)"##);
        run_test(r##""超時空要塞".index(/時/, 30)"##);
        run_test(r##""超時空要塞".index(/時/, -30)"##);
        run_test(r##""超時空要塞".index(/時/, -3)"##);
        run_test(r##""超時空要塞".index(/時/, -4)"##);
        run_test(r##""超時空要塞".index(/時/, -4.0)"##);
    }

    #[test]
    fn rindex() {
        run_test(r##""astrochemistry".rindex("str")"##);
        run_test(r##""character".rindex(?c)"##);
        run_test(r##""regexprindex".rindex(/e.*x/, 2)"##);
        run_test(r##""foobarfoobar".rindex("bar", 6)"##);
        run_test(r##""foobarfoobar".rindex("bar", -6)"##);
        run_test(r##""windows".rindex("w", 100)"##);
        run_test(r##""windows".rindex("w", -100)"##);
        run_test(r##""windows".rindex("w", -1)"##);
        run_test(r##""windows".rindex("w", -3)"##);
        run_test(r##""windows".rindex("w", -8)"##);
        run_test(r##""".rindex("")"##);
        run_test(r##""".rindex(".")"##);
        run_test(r##""超時空要塞超時空要塞超時空要塞".index(/時/, 11)"##);
        run_test(r##""超時空要塞超時空要塞超時空要塞".index(/時/, 1)"##);
        run_test(r##""超時空要塞超時空要塞超時空要塞".index(/時/, 0)"##);
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
        run_test(r##""string".start_with?("jng", "hng", "ing")"##);
        run_test_error(r##""string".start_with?("jng", 3, "ing")"##);
        run_test(r##""hello".delete_prefix("hel")"##);
        run_test(r##""hello".delete_prefix("her")"##);
        run_test(r##"s = "hello"; [s.delete_prefix!("hel"), s]"##);
        run_test(r##"s = "hello"; [s.delete_prefix!("her"), s]"##);
        run_test(r##""string".end_with?("str")"##);
        run_test(r##""string".end_with?("ing")"##);
        run_test(r##""string".end_with?("jng", "hng", "ing")"##);
        run_test_error(r##""string".end_with?("jng", 3, "ing")"##);
        run_test(r##""string".include?("str")"##);
        run_test(r##""string".include?("ing")"##);
        run_test(r##""string".include?("ingi")"##);
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
    fn chomp_() {
        run_test(
            r##"
        s = "foo\n"
        [s.chomp!, s]
        "##,
        );
        run_test(
            r##"
        s = "foo\n"
        [s.chomp!("\n"), s]
        "##,
        );
        run_test(
            r##"
        s = "foo\r\n"
        [s.chomp!("\r\n"), s]
        "##,
        );
        run_test(
            r##"
        s = "string\n"
        [s.chomp!(nil), s]
        "##,
        );
        run_test(
            r##"
        s = "foo\r\n\n"
        [s.chomp!(""), s]
        "##,
        );
        run_test(
            r##"
        s = "foo\n\r\n"
        [s.chomp!(""), s]
        "##,
        );
        run_test(
            r##"
        s = "foo\n\r\r"
        [s.chomp!(""), s]
        "##,
        );
    }

    #[test]
    fn to_i() {
        run_test(r"'42581'.to_i");
        run_test(r"'-42581'.to_i");
        run_test(r"'_-42581'.to_i");
        run_test(r"'-_42581'.to_i");
        run_test(r"'-42_581'.to_i");
        run_test(r"'-42_58__1'.to_i");
        run_test(r"'4a5f1'.to_i(16)");
        run_test(r"'4258159248352010254587519982001542568633842205196875555'.to_i");
        run_test(r"'42581592483edrcs0254587519982001ipgomrn568633842205196875555'.to_i(36)");
        run_test(r"'42581592483edr_cs02545875199_82001ipgomrn56863384220_5196_875555'.to_i(36)");
        run_test(r"'42581592483edr__cs02545875199_82001ipgomrn56863384220_5196_875555'.to_i(36)");
        run_test(r"'-42581592483edr_cs02545875199_82001ipgomrn56863384220_5196_875555'.to_i(36)");
        run_test(r"'_-42581592483edr_cs02545875199_82001ipgomrn56863384220_5196_875555'.to_i(36)");
        run_test_error(r"'42581'.to_i(-10)");
        run_test_error(r"'42581'.to_i(100)");
    }
    #[test]
    fn to_f() {
        run_test(r"'4285'.to_f");
        run_test(r"'-4285'.to_f");
        run_test(r"'428.55'.to_f");
        run_test(r"'-428.55'.to_f");
        run_test(r"'-428.55e12'.to_f");
        run_test(r"'-428.55e-12'.to_f");
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
        run_test(r"s = 'AkrFj妖精u35]['; [s.upcase!, s]");
        run_test(r"s = 'RUBY'; [s.upcase!, s]");
        run_test(r"s = 'AkrFj妖精u35]['; [s.downcase!, s]");
        run_test(r"s = 'rust'; [s.downcase!, s]");
    }

    #[test]
    fn sum() {
        run_test(r#"File.read("../LICENSE-MIT").sum"#);
        run_test(r#"File.read("../LICENSE-MIT").sum(11)"#);
    }

    #[test]
    fn replace() {
        run_test(
            r#"
        str = "foo"
        [str.replace("bar"), str]
        "#,
        );
    }

    #[test]
    fn chars() {
        run_test(
            r#"
        "hello世界".chars 
        "#,
        );
        run_test(
            r#"
        x = []
        ["hello world".chars do |c| x << c.upcase end, x]
        "#,
        );
    }

    #[test]
    fn each_char() {
        run_test(
            r#"
        x = []; res = "hello世界".each_char {|c| x << c.upcase }; [res, x]
        "#,
        );
        run_test(
            r#"
        x = []; "hello世界".each_char.each_with_index {|c, i| x << c + i.to_s }; x
        "#,
        );
    }

    #[test]
    fn count() {
        run_test(r#"'abcdefg'.count('c') "#);
        run_test(r#"'123456789'.count('2378') "#);
        run_test(r#"'123456789'.count('2378', '2378') "#);
    }

    #[test]
    fn center() {
        run_test(r#""foo".center(10)"#);
        run_test(r#""foo".center(9)"#);
        run_test(r#""foo".center(8)"#);
        run_test(r#""foo".center(7)"#);
        run_test(r#""foo".center(3)"#);
        run_test(r#""foo".center(2)"#);
        run_test(r#""foo".center(1)"#);
        run_test(r#""foo".center(10, "*")"#);
    }

    #[test]
    fn index_assign() {
        run_test(r#"buf = "string"; buf[0]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[1]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[5]="!!"; buf"#);
        run_test_error(r#"buf = "string"; buf[10]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[-1]="!!"; buf"#);
        run_test_error(r#"buf = "string"; buf[-10]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[1,3]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[-1,3]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[-1,10]="!!"; buf"#);
        run_test_error(r#"buf = "string"; buf[-100,100]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[3,10]="!!"; buf"#);
        run_test(r#"buf = "string"; buf[3,0]="!!"; buf"#);
        run_test_error(r#"buf = "string"; buf[3,-1]="!!"; buf"#);
    }

    #[test]
    fn succ() {
        run_test(r#""aa".succ"#);
        run_test(r#""88".succ.succ"#);
        run_test(r#""99".succ"#);
        run_test(r#""９９".succ"#);
        run_test(r#""z９9".succ"#);
        run_test(r#""zz".succ"#);
        run_test(r#""ZZ".succ"#);
        run_test(r#""ＺＺ".succ"#);
        run_test(r#""ｚｚ".succ"#);
        run_test(r#""Ｚｚ".succ"#);
        run_test(r#""Ｚ＃ｚ".succ"#);
        run_test(r#""a9".succ"#);
        run_test(r#""-9".succ"#);
        run_test(r#""9".succ"#);
        run_test(r#""09".succ"#);
        run_test(r#""1.9.9".succ"#);
        run_test(r#""v9.9.9".succ"#);
        run_test(r#"".".succ"#);
        run_test(r#""".succ"#);
        run_test(r#""AZ".succ"#);
        run_test(r#""1##9".succ"#);
        run_test(r#""1&&Z".succ"#);
        run_test(r#""z&&Z".succ"#);
        run_test(r#""Ρ".succ"#);
        run_test(r#""を".succ"#);
        run_test(r#####""###".succ"#####);
        run_test(r#""9Zz9".succ"#);
        run_test(r#"12.times.reduce("ン"){|c|c.succ}"#);
    }

    #[test]
    fn pack_unpack() {
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack('csl')"#);

        run_test(r#""\x00\x01\x02\x03\x04\x05\x06".unpack1('q')"#);
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06".unpack1('Q')"#);
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06\x07".unpack1('q')"#);
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06\x07".unpack1('Q')"#);
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack1('q')"#);
        run_test(r#""\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack1('Q')"#);

        run_test(r#""\x00\x01\x02".unpack1('l')"#);
        run_test(r#""\x00\x01\x02".unpack1('L')"#);
        run_test(r#""\x00\x01\x02\x03".unpack1('l')"#);
        run_test(r#""\x00\x01\x02\x03".unpack1('L')"#);
        run_test(r#""\x00\x01\x02\x03\x04".unpack1('l')"#);
        run_test(r#""\x00\x01\x02\x03\x04".unpack1('L')"#);

        run_test(r#""\x00".unpack1('s')"#);
        run_test(r#""\x00".unpack1('S')"#);
        run_test(r#""\x00\x01".unpack1('s')"#);
        run_test(r#""\x00\x01".unpack1('S')"#);
        run_test(r#""\x00\x01\x02".unpack1('s')"#);
        run_test(r#""\x00\x01\x02".unpack1('S')"#);

        run_test(r#""\x00".unpack1('c')"#);
        run_test(r#""\x00".unpack1('C')"#);
        run_test(r#""\x00\x01".unpack1('c')"#);
        run_test(r#""\x00\x01".unpack1('C')"#);

        run_test(r#""\x01\xFE".unpack1("c*")"#);
        run_test(r#""Ruby".unpack("c*")"#);
        run_test(r#""Ruby".unpack1("c*")"#);
        run_test(r#""戦闘妖精雪風".unpack("c*")"#);
        run_test(r#""戦闘妖精雪風".unpack1("c*")"#);
        run_test(r#""\x01\xFE".unpack1("C*")"#);
        run_test(r#""Ruby".unpack("C*")"#);
        run_test(r#""Ruby".unpack1("C*")"#);

        run_test(r#""\x01\xFE".unpack("c*")"#);
        run_test(r#"[1, -2].pack("c*")"#);
        run_test(r#"[1, 254].pack("c*")"#);
        run_test(r#""\x01\xFE".unpack("C*")"#);
        run_test(r#"[1, -2].pack("C*")"#);
        run_test(r#"[1, 254].pack("C*")"#);

        run_test(r#""\x01\x02\xFE\xFD".unpack("s*")"#);
        run_test(r#"[513, 65022].pack("s*")"#);
        run_test(r#"[513, -514].pack("s*")"#);
        run_test(r#""\x01\x02\xFE\xFD".unpack("s*")"#);
        run_test(r#"[258, 65277].pack("s*")"#);
        run_test(r#"[258, -259].pack("s*")"#);

        run_test(r#"[0,1,-1,32767,-32768,65535].pack("n*")"#);
        run_test(r#""\x00\x00\x00\x01\xFF\xFF\x7F\xFF\x80\x00\xFF\xFF".unpack("n*")"#);

        run_test(r#"[0,1,-1].pack("N*")"#);
        run_test(r#""\x00\x00\x00\x00\x00\x00\x00\x01\xFF\xFF\xFF\xFF".unpack("N*")"#);

        run_test(r#"[97, 98].pack("CxC")"#);
        run_test(r#"[97, 98].pack("Cx3C")"#);
        run_test(r#"[97, 98].pack("Cx*C")"#);
        run_test(r#""abc".unpack("CxC")"#);
        run_test_error(r#""abc".unpack("Cx3C")"#);
        run_test(r#"[97, 98, 99].pack("CCXC")"#);
        run_test(r#""abcdef".unpack("x*XC")"#);

        run_test(r#""\x01\xFE".unpack("h*")"#);
        run_test(r#""\x01\xFE".unpack("h3")"#);
        run_test(r#""\x01\xFE".unpack("H*")"#);
        run_test(r#""\x01\xFE".unpack("H3")"#);
    }

    #[test]
    fn unpack1g() {
        run_test(r#"'a'.b.encoding.inspect"#);
    }

    #[test]
    fn dump() {
        run_test(r#""abc\r\n\f\x70'\b10\\\"魁\u1234".dump"#);
        run_test(r#""abc\r\n\f\x80'\b10\\\"魁\u1234".b"#);
    }

    #[test]
    fn force_encoding() {
        run_test(r#""Ruby".force_encoding("ASCII-8BIT")"#);
        run_test(r#""Ruby".force_encoding("UTF-8")"#);
        run_test(r#""Ruby".force_encoding(Encoding::UTF_8)"#);
        run_test(r#""Ruby".force_encoding(Encoding::ASCII_8BIT)"#);
        run_test_error(r#""Ruby".force_encoding(:ASCII)"#);
    }

    #[test]
    fn ascii_only() {
        run_test(r#"'abc123'.ascii_only?"#);
        run_test(r#"''.ascii_only?"#);
        run_test(r#"'日本語'.ascii_only?"#);
        run_test(r#"'日本語abc123'.ascii_only?"#);
    }
}
