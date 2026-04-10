use num::{BigInt, Zero};

use super::*;
use jitgen::JitContext;

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
    globals.define_builtin_class_func(STRING_CLASS, "allocate", allocate, 0);
    globals.define_builtin_class_func(STRING_CLASS, "try_convert", string_try_convert, 1);
    globals.define_builtin_func(STRING_CLASS, "+", add, 1);
    globals.define_builtin_func(STRING_CLASS, "*", mul, 1);
    globals.define_builtin_func(STRING_CLASS, "==", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "===", eq, 1);
    globals.define_builtin_func(STRING_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(STRING_CLASS, "casecmp", casecmp, 1);
    globals.define_builtin_func(STRING_CLASS, "casecmp?", casecmp_p, 1);
    globals.define_basic_op(STRING_CLASS, "!=", ne, 1);
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
    globals.define_builtin_func_rest(STRING_CLASS, "end_with?", end_with);
    globals.define_builtin_func(STRING_CLASS, "include?", include_, 1);
    globals.define_builtin_func(STRING_CLASS, "delete_prefix!", delete_prefix_, 1);
    globals.define_builtin_func(STRING_CLASS, "delete_prefix", delete_prefix, 1);
    globals.define_builtin_func_with(STRING_CLASS, "split", split, 0, 2, false);
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
    globals.define_builtin_inline_funcs(
        STRING_CLASS,
        "bytesize",
        &[],
        bytesize,
        Box::new(string_bytesize),
        0,
    );
    globals.define_builtin_func(STRING_CLASS, "ord", ord, 0);
    globals.define_builtin_func_with(STRING_CLASS, "ljust", ljust, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "rjust", rjust, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "lines", lines, 0, 2, false);
    globals.define_builtin_func(STRING_CLASS, "bytes", bytes, 0);
    globals.define_builtin_func(STRING_CLASS, "getbyte", getbyte, 1);
    globals.define_builtin_func_with(STRING_CLASS, "byteslice", byteslice, 1, 2, false);
    globals.define_builtin_func_with(STRING_CLASS, "bytesplice", bytesplice, 2, 5, false);
    globals.define_builtin_func(STRING_CLASS, "setbyte", setbyte, 2);
    globals.define_builtin_func_with(STRING_CLASS, "each_line", each_line, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "empty?", empty, 0);
    globals.define_builtin_func_with(STRING_CLASS, "to_i", to_i, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "to_f", to_f, 0);
    globals.define_builtin_func(STRING_CLASS, "hex", hex, 0);
    globals.define_builtin_func(STRING_CLASS, "oct", oct, 0);
    globals.define_builtin_funcs(STRING_CLASS, "to_sym", &["intern"], to_sym, 0);
    globals.define_builtin_func_rest(STRING_CLASS, "upcase", upcase);
    globals.define_builtin_func_rest(STRING_CLASS, "upcase!", upcase_);
    globals.define_builtin_func_rest(STRING_CLASS, "downcase", downcase);
    globals.define_builtin_func_rest(STRING_CLASS, "downcase!", downcase_);
    globals.define_builtin_func_rest(STRING_CLASS, "capitalize", capitalize);
    globals.define_builtin_func_rest(STRING_CLASS, "capitalize!", capitalize_);
    globals.define_builtin_func_rest(STRING_CLASS, "swapcase", swapcase);
    globals.define_builtin_func_rest(STRING_CLASS, "swapcase!", swapcase_);
    globals.define_builtin_func_with(STRING_CLASS, "delete", delete, 0, 0, true);
    globals.define_builtin_func(STRING_CLASS, "tr", tr, 2);
    globals.define_builtin_func_rest(STRING_CLASS, "count", count);
    globals.define_builtin_func_with(STRING_CLASS, "sum", sum, 0, 1, false);
    globals.define_builtin_func(STRING_CLASS, "replace", replace, 1);
    globals.define_builtin_func(STRING_CLASS, "chars", chars, 0);
    globals.define_builtin_func(STRING_CLASS, "each_char", each_char, 0);
    globals.define_builtin_func_with(STRING_CLASS, "center", center, 1, 2, false);
    globals.define_builtin_funcs(STRING_CLASS, "next", &["succ"], next, 0);
    globals.define_builtin_funcs(STRING_CLASS, "next!", &["succ!"], next_mut, 0);
    globals.define_builtin_func(STRING_CLASS, "encoding", super::encoding::str_encoding, 0);
    globals.define_builtin_func(STRING_CLASS, "b", super::encoding::b, 0);
    globals.define_builtin_func_with_kw(
        STRING_CLASS,
        "encode",
        super::encoding::encode,
        0,
        2,
        false,
        &[],
        true,
    );
    globals.define_builtin_func_with_kw(
        STRING_CLASS,
        "encode!",
        super::encoding::encode_,
        0,
        2,
        false,
        &[],
        true,
    );
    globals.define_builtin_func_with_kw(
        STRING_CLASS,
        "unpack1",
        unpack1,
        1,
        1,
        false,
        &["offset"],
        false,
    );
    globals.define_builtin_func_with_kw(
        STRING_CLASS,
        "unpack",
        unpack,
        1,
        1,
        false,
        &["offset"],
        false,
    );
    globals.define_builtin_func_with(STRING_CLASS, "byteindex", byteindex, 1, 2, false);
    globals.define_builtin_func(STRING_CLASS, "to_c", to_c, 0);
    globals.define_builtin_func(STRING_CLASS, "to_r", to_r, 0);
    globals.define_builtin_func(STRING_CLASS, "dump", dump, 0);
    globals.define_builtin_func(
        STRING_CLASS,
        "force_encoding",
        super::encoding::force_encoding,
        1,
    );
    globals.define_builtin_func(
        STRING_CLASS,
        "valid_encoding?",
        super::encoding::valid_encoding,
        0,
    );
    globals.define_builtin_func(STRING_CLASS, "ascii_only?", super::encoding::ascii_only, 0);

    super::encoding::init_encoding(globals);
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
fn string_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.try_arg(0) {
        Some(string) => string.coerce_to_string(vm, globals)?,
        None => "".to_string(),
    };
    Ok(Value::string(s))
}

/// ### String.allocate
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(Value::string_with_class("", class_id))
}

///
/// ### String.try_convert
///
/// - try_convert(obj) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/s/try_convert.html]
#[monoruby_builtin]
fn string_try_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn mul(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let count = match lfp.arg(0).coerce_to_int_i64(vm, globals)? {
        i if i < 0 => return Err(MonorubyErr::negative_argument()),
        i => i as usize,
    };

    let self_ = lfp.self_val();
    let inner = self_.as_rstring_inner();
    let byte_len = inner.as_bytes().len();
    // Guard against overflow / OOM for huge counts
    if byte_len > 0 && count > (isize::MAX as usize) / byte_len {
        return Err(MonorubyErr::argumenterr("argument too big"));
    }
    let res = Value::string_from_inner(inner.repeat(count));
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
fn eq(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn ne(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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

fn string_cmp(
    lfp: Lfp,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<std::cmp::Ordering>> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    if let Some(rhs) = lfp.arg(0).is_rstring() {
        return Ok(Some(lhs.cmp(&rhs)));
    }
    // Try to_str coercion
    if let Ok(rhs) = lfp.arg(0).coerce_to_rstring(vm, globals) {
        return Ok(Some(lhs.cmp(&rhs)));
    }
    Ok(None)
}

fn string_cmp2(lfp: Lfp, vm: &mut Executor, globals: &mut Globals) -> Result<std::cmp::Ordering> {
    match string_cmp(lfp, vm, globals)? {
        Some(ord) => Ok(ord),
        None => {
            let other = lfp.arg(0);
            Err(MonorubyErr::argumenterr(format!(
                "comparison of String with {} failed",
                other.inspect(&globals.store)
            )))
        }
    }
}

///
/// ### String#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(string_cmp(lfp, vm, globals)?
        .map(Value::from_ord)
        .unwrap_or_default())
}

///
/// ### String#casecmp
///
/// - casecmp(other) -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/casecmp.html]
#[monoruby_builtin]
fn casecmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let rhs_inner = if let Some(inner) = rhs.is_rstring_inner() {
        inner.as_bytes().to_vec()
    } else if let Ok(s) = rhs.coerce_to_rstring(vm, globals) {
        s.as_bytes().to_vec()
    } else {
        return Ok(Value::nil());
    };
    let lhs_inner = lhs.as_rstring_inner();
    // ASCII case-insensitive byte comparison (matches CRuby behavior)
    let ord = lhs_inner
        .iter()
        .map(|b| b.to_ascii_lowercase())
        .cmp(rhs_inner.iter().map(|b| b.to_ascii_lowercase()));
    Ok(Value::from_ord(ord))
}

///
/// ### String#casecmp?
///
/// - casecmp?(other) -> true | false | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/casecmp=3f.html]
#[monoruby_builtin]
fn casecmp_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let (rhs_bytes, rhs_enc) = if let Some(inner) = rhs.is_rstring_inner() {
        (inner.as_bytes().to_vec(), inner.encoding())
    } else if let Ok(s) = rhs.coerce_to_rstring(vm, globals) {
        (s.as_bytes().to_vec(), s.encoding())
    } else {
        return Ok(Value::nil());
    };
    let lhs_inner = lhs.as_rstring_inner();
    let lhs_enc = lhs_inner.encoding();
    // If encodings are incompatible, return nil.
    // Binary vs UTF-8 is compatible if at least one side is ASCII-only.
    if lhs_enc != rhs_enc && !(lhs_enc.is_utf8_compatible() && rhs_enc.is_utf8_compatible()) {
        let lhs_ascii_only = lhs_inner.is_ascii();
        let rhs_ascii_only = rhs_bytes.is_ascii();
        if !lhs_ascii_only && !rhs_ascii_only {
            return Ok(Value::nil());
        }
    }
    if lhs_enc.is_utf8_compatible() && rhs_enc.is_utf8_compatible() {
        // UTF-8 compatible: do Unicode case folding.
        let lhs_str = lhs_inner.check_utf8()?;
        let rhs_str = std::str::from_utf8(&rhs_bytes)
            .map_err(|_| MonorubyErr::argumenterr("invalid byte sequence in UTF-8".to_string()))?;
        let lhs_lower = lhs_str.to_lowercase();
        let rhs_lower = rhs_str.to_lowercase();
        Ok(Value::bool(lhs_lower == rhs_lower))
    } else {
        // Binary: ASCII-only case-insensitive byte comparison.
        let eq = lhs_inner.len() == rhs_bytes.len()
            && lhs_inner
                .iter()
                .zip(rhs_bytes.iter())
                .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase());
        Ok(Value::bool(eq))
    }
}

///
/// ### Comparable#<=
///
/// - self <= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3c=3d.html]
#[monoruby_builtin]
fn le(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ord = string_cmp2(lfp, vm, globals)?;
    Ok(Value::bool(ord != std::cmp::Ordering::Greater))
}

///
/// ### Comparable#<=
///
/// - self < other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3c.html]
#[monoruby_builtin]
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ord = string_cmp2(lfp, vm, globals)?;
    Ok(Value::bool(ord == std::cmp::Ordering::Less))
}

///
/// ### Comparable#<=
///
/// - self >= other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3e=3d.html]
#[monoruby_builtin]
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ord = string_cmp2(lfp, vm, globals)?;
    Ok(Value::bool(ord != std::cmp::Ordering::Less))
}

///
/// ### Comparable#<=
///
/// - self > other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Comparable/i/=3e.html]
#[monoruby_builtin]
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ord = string_cmp2(lfp, vm, globals)?;
    Ok(Value::bool(ord == std::cmp::Ordering::Greater))
}

///
/// ### Strring#<<
///
/// - self << other -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ = lfp.self_val();
    if let Some(other) = lfp.arg(0).is_rstring() {
        self_.as_rstring_inner_mut().extend(&other)?;
    } else if let Some(i) = lfp.arg(0).try_fixnum() {
        let ch = match u32::try_from(i) {
            Ok(ch) => ch,
            Err(_) => return Err(MonorubyErr::char_out_of_range(&globals.store, lfp.arg(0))),
        };
        let bytes = self_.as_rstring_inner_mut();
        if bytes.encoding().is_utf8_compatible() {
            let c = char::from_u32(ch)
                .ok_or_else(|| MonorubyErr::char_out_of_range(&globals.store, lfp.arg(0)))?;
            let mut buf = [0u8; 4];
            let encoded = c.encode_utf8(&mut buf);
            bytes.extend_from_slice_checked(encoded.as_bytes())?;
        } else {
            // ASCII-8BIT: append raw byte(s)
            if let Ok(b) = u8::try_from(ch) {
                bytes.extend_from_slice_checked(&[b])?;
            } else {
                return Err(MonorubyErr::char_out_of_range(&globals.store, lfp.arg(0)));
            }
        }
    } else {
        // Try to_str coercion
        let coerced = lfp.arg(0).coerce_to_rstring(vm, globals)?;
        self_.as_rstring_inner_mut().extend(&coerced)?;
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
fn rem(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    let self_ = lfp.self_val();
    if let Some(hash) = arg.try_hash_ty() {
        let format_str = vm.format_by_hash(globals, self_.as_str(), hash)?;
        let res = Value::string(format_str);
        Ok(res)
    } else {
        let arguments = match arg.try_array_ty() {
            Some(ary) => ary.to_vec(),
            None => vec![arg],
        };
        let format_str = vm.format_by_args(globals, self_.as_str(), &arguments)?;
        let res = Value::string(format_str);
        Ok(res)
    }
}

///
/// ### String#=~
///
/// - self =~ other -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/=3d=7e.html]
#[monoruby_builtin]
fn match_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let given = self_val.expect_str(globals)?;
    let regex = &lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;
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
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_rstring_inner();
    let enc = lhs.encoding();
    if let Some(i) = lfp.arg(0).try_fixnum() {
        let index = match lhs.conv_char_index(i)? {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if let Some(arg1) = lfp.try_arg(1) {
            let len = match arg1.coerce_to_int_i64(vm, globals)? {
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
            info.start().coerce_to_int_i64(vm, globals)?,
            info.end().coerce_to_int_i64(vm, globals)? - info.exclude_end() as i64,
        );
        let (start, len) = match (lhs.conv_char_index(start)?, lhs.conv_char_index(end)?) {
            (Some(start), Some(end)) => {
                if start > end {
                    (start, 0)
                } else {
                    (start, end - start + 1)
                }
            }
            (Some(start), None) => (start, 0),
            (None, _) => return Ok(Value::nil()),
        };
        let r = lhs.get_range(start, len);
        Ok(Value::string_from_inner(RStringInner::from_encoding(
            &lhs[r], enc,
        )))
    } else if let Some(re) = lfp.arg(0).is_regex() {
        let nth = if let Some(i) = lfp.try_arg(1) {
            i.coerce_to_int_i64(vm, globals)?
        } else {
            0
        };
        string_match_index(vm, lhs, &re, nth)
    } else if let Some(s) = lfp.arg(0).is_str() {
        // String argument: return the string if it's a substring
        let given = lhs.check_utf8()?;
        if given.contains(s) {
            Ok(Value::string_from_str(s))
        } else {
            Ok(Value::nil())
        }
    } else {
        // Try to_int coercion first, then to_str
        let arg0 = lfp.arg(0);
        if let Some(func_id) = globals.check_method(arg0, IdentId::TO_INT) {
            let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
            if let RV::Fixnum(i) = result.unpack() {
                let index = match lhs.conv_char_index(i)? {
                    Some(i) => i,
                    None => return Ok(Value::nil()),
                };
                if let Some(arg1) = lfp.try_arg(1) {
                    let len = match arg1.coerce_to_int_i64(vm, globals)? {
                        0 => return Ok(Value::string_from_str("")),
                        i if i < 0 => return Ok(Value::nil()),
                        i => i as usize,
                    };
                    let r = lhs.get_range(index, len);
                    return Ok(Value::string_from_inner(RStringInner::from_encoding(
                        &lhs[r], enc,
                    )));
                } else {
                    let r = lhs.get_range(index, 1);
                    if !r.is_empty() {
                        return Ok(Value::string_from_inner(RStringInner::from_encoding(
                            &lhs[r], enc,
                        )));
                    } else {
                        return Ok(Value::nil());
                    }
                }
            }
        }
        if let Some(func_id) = globals.check_method(arg0, IdentId::TO_STR) {
            let result = vm.invoke_func_inner(globals, func_id, arg0, &[], None, None)?;
            if let Some(s) = result.is_str() {
                let given = lhs.check_utf8()?;
                if given.contains(s) {
                    return Ok(Value::string_from_str(s));
                } else {
                    return Ok(Value::nil());
                }
            }
        }
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            arg0,
            INTEGER_CLASS,
        ))
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
fn index_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let (arg1, subst) = if let Some(arg2) = lfp.try_arg(2) {
        (Some(lfp.arg(1)), arg2.coerce_to_string(vm, globals)?)
    } else {
        (None, lfp.arg(1).coerce_to_string(vm, globals)?)
    };
    let self_ = lfp.self_val();
    let mut lhs = self_.expect_string(globals)?;
    let len = lhs.chars().count();
    let arg0_val = lfp.arg(0);
    if let Some(arg0) = arg0_val.try_fixnum() {
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
            match arg1.coerce_to_int_i64(vm, globals)? {
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
        // Try to_int coercion on the index argument
        let idx = arg0_val.coerce_to_int_i64(vm, globals)?;
        let start = match conv_index(idx, len) {
            Some(i) => i,
            None => return Err(MonorubyErr::indexerr("index out of range.")),
        };
        let start = match lhs.char_indices().nth(start) {
            Some((i, _)) => i,
            None => return Err(MonorubyErr::indexerr("index out of range.")),
        };
        let len = if let Some(arg1) = arg1 {
            match arg1.coerce_to_int_i64(vm, globals)? {
                i if i < 0 => return Err(MonorubyErr::indexerr("negative length.")),
                i => i as usize,
            }
        } else {
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
fn start_with(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let self_inner = self_.as_rstring_inner();
    let self_enc = self_inner.encoding();
    let self_bytes = self_inner.as_bytes();
    let arg0 = lfp.arg(0).as_array();
    for v in arg0.iter() {
        if let Some(re) = v.is_regex() {
            let string = self_inner.check_utf8()?;
            if let Some(mat) = re.captures(string, vm)? {
                if let Some(m) = mat.get(0) {
                    if m.start() == 0 {
                        return Ok(Value::bool(true));
                    }
                }
            } else {
                vm.clear_capture_special_variables();
            }
            continue;
        }
        let arg_inner = coerce_to_rstring_inner(v, vm, globals)?;
        check_encoding_compat(self_enc, self_bytes, &arg_inner, globals)?;
        let arg_bytes = arg_inner.as_bytes();
        if self_bytes.starts_with(arg_bytes) {
            if self_enc.is_utf8_compatible() && arg_bytes.len() < self_bytes.len() {
                if !is_utf8_char_boundary(self_bytes, arg_bytes.len()) {
                    continue;
                }
            }
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

///
/// ### String#end_with?
///
/// - end_with?(*strs) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/end_with=3f.html]
#[monoruby_builtin]
fn end_with(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let self_inner = self_.as_rstring_inner();
    let self_enc = self_inner.encoding();
    let self_bytes = self_inner.as_bytes();
    let arg0 = lfp.arg(0).as_array();
    for v in arg0.iter() {
        if v.is_regex().is_some() {
            return Err(MonorubyErr::typeerr(
                "no implicit conversion of Regexp into String",
            ));
        }
        let arg_inner = coerce_to_rstring_inner(v, vm, globals)?;
        check_encoding_compat(self_enc, self_bytes, &arg_inner, globals)?;
        let arg_bytes = arg_inner.as_bytes();
        if self_bytes.ends_with(arg_bytes) {
            if self_enc.is_utf8_compatible() && arg_bytes.len() < self_bytes.len() {
                let boundary = self_bytes.len() - arg_bytes.len();
                if !is_utf8_char_boundary(self_bytes, boundary) {
                    continue;
                }
            }
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

/// Coerce a Value to RStringInner for start_with?/end_with?.
fn coerce_to_rstring_inner(
    v: &Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<RStringInner> {
    if let Some(inner) = v.is_rstring_inner() {
        Ok(inner.clone())
    } else {
        let s = v.coerce_to_str(vm, globals)?;
        Ok(RStringInner::from_str(&s))
    }
}

/// Check encoding compatibility between self and argument.
/// Raises Encoding::CompatibilityError if incompatible.
fn check_encoding_compat(
    self_enc: Encoding,
    self_bytes: &[u8],
    arg_inner: &RStringInner,
    globals: &Globals,
) -> Result<()> {
    let arg_enc = arg_inner.encoding();
    if self_enc != arg_enc
        && !(self_enc.is_utf8_compatible() && arg_enc.is_utf8_compatible())
        && !(self_bytes.is_ascii() || arg_inner.as_bytes().is_ascii())
    {
        let enc_class = super::encoding::encoding_class(globals);
        let compat_err_class = globals
            .store
            .get_constant_noautoload(enc_class, IdentId::get_id("CompatibilityError"))
            .map(|v| v.as_class_id());
        let msg = format!(
            "incompatible character encodings: {:?} and {:?}",
            self_enc, arg_enc,
        );
        return Err(match compat_err_class {
            Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg),
            None => MonorubyErr::runtimeerr(msg),
        });
    }
    Ok(())
}

///
/// ### String#include?
///
/// - include?(substr) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/include=3f.html]
#[monoruby_builtin]
fn include_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let substr_s = lfp.arg(0).coerce_to_str(vm, globals)?;
    let b = string.contains(substr_s.as_str());
    Ok(Value::bool(b))
}

///
/// ### String#delete_prefix!
///
/// - delete_prefix!(prefix) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/delete_prefix=21.html]
#[monoruby_builtin]
fn delete_prefix_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg = lfp.arg(0).coerce_to_str(vm, globals)?;
    if let Some(stripped) = string.strip_prefix(arg.as_str()) {
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
fn delete_prefix(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let arg = lfp.arg(0).coerce_to_str(vm, globals)?;
    if let Some(stripped) = string.strip_prefix(arg.as_str()) {
        Ok(Value::string_from_str(stripped))
    } else {
        Ok(Value::string_from_str(string))
    }
}

/// Check if a byte position in a UTF-8 byte slice is at a character boundary.
fn is_utf8_char_boundary(bytes: &[u8], pos: usize) -> bool {
    if pos >= bytes.len() {
        return pos == bytes.len();
    }
    // A byte is NOT at a character boundary if it's a continuation byte (10xxxxxx)
    (bytes[pos] & 0xC0) != 0x80
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
fn split(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let string = self_.expect_str(globals)?;
    let lim = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let arg0 = lfp.try_arg(0).unwrap_or(Value::string_from_str(" "));
    if let Some(sep_inner) = arg0.is_rstring_inner() {
        let sep = sep_inner.check_utf8()?;
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
    } else if let Some(re) = arg0.is_regex() {
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
        // Try to_str coercion for the separator
        let coerced = arg0.coerce_to_str(vm, globals)?;
        let v: Vec<Value> = if coerced == " " {
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
            string
                .split(&*coerced)
                .map(Value::string_from_str)
                .collect()
        } else if lim == 0 {
            let mut vec: Vec<&str> = string.split(&*coerced).collect();
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
                .splitn(lim as usize, &*coerced)
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
fn slice_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
            let len = match arg1.coerce_to_int_i64(vm, globals)? {
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
            info.start().coerce_to_int_i64(vm, globals)?,
            info.end().coerce_to_int_i64(vm, globals)? - info.exclude_end() as i64,
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
        let nth = if let Some(arg1) = lfp.try_arg(1) {
            arg1.coerce_to_int_i64(vm, globals)?
        } else {
            0
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
    } else if let Some(s) = lfp.arg(0).is_str() {
        // String argument: return the string if it's a substring
        if let Some(pos) = lhs.find(s) {
            let end = pos + s.len();
            Ok(slice_sub(lfp, lhs, pos..end))
        } else {
            Ok(Value::nil())
        }
    } else {
        // Try to_int coercion
        let arg0 = lfp.arg(0);
        let i = arg0.coerce_to_int_i64(vm, globals)?;
        let index = match conv_index(i, lhs.chars().count()) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        if let Some(arg1) = lfp.try_arg(1) {
            let len = match arg1.coerce_to_int_i64(vm, globals)? {
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
fn chomp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let rs_owned;
    let rs = if let Some(arg0) = &arg0 {
        if arg0.is_nil() {
            return Ok(lfp.self_val());
        }
        rs_owned = arg0.coerce_to_string(vm, globals)?;
        rs_owned.as_str()
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
fn chomp_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let arg0 = lfp.try_arg(0);
    let rs_owned;
    let rs = if let Some(arg0) = &arg0 {
        if arg0.is_nil() {
            return Ok(Value::nil());
        }
        rs_owned = arg0.coerce_to_string(vm, globals)?;
        rs_owned.as_str()
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
fn strip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn strip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
fn rstrip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn rstrip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
fn lstrip(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn lstrip_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn sub_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
        if arg1.try_hash_ty().is_some() {
            RegexpInner::replace_one_hash(vm, globals, lfp.arg(0), given, arg1)
        } else {
            let replace = arg1.coerce_to_str(vm, globals)?;
            RegexpInner::replace_one(vm, globals, lfp.arg(0), given, &replace)
        }
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
fn gsub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn gsub_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
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
        if arg1.try_hash_ty().is_some() {
            RegexpInner::replace_all_hash(vm, globals, lfp.arg(0), given, arg1)
        } else {
            let replace = arg1.coerce_to_str(vm, globals)?;
            RegexpInner::replace_all(vm, globals, lfp.arg(0), given, &replace)
        }
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
fn scan(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let arg0 = lfp.arg(0);
    let owned_re;
    let coerced_re;
    let re: &RegexpInner = if let Some(s) = arg0.is_str() {
        owned_re = RegexpInner::from_escaped(s)?;
        &owned_re
    } else if arg0.is_regex().is_some() {
        arg0.as_regexp_inner()
    } else {
        // Try to_str coercion
        let coerced = arg0.coerce_to_str(vm, globals)?;
        coerced_re = RegexpInner::from_escaped(&coerced)?;
        &coerced_re
    };
    match lfp.block() {
        None => {
            let vec = re.scan(vm, given)?;
            Ok(Value::array_from_vec(vec))
        }
        Some(block) => {
            scan_with_block(vm, globals, re, given, block)?;
            Ok(lfp.self_val())
        }
    }
}

fn scan_with_block(
    vm: &mut Executor,
    globals: &mut Globals,
    re: &RegexpInner,
    given: &str,
    block: BlockHandler,
) -> Result<()> {
    // We must own the string since `given` borrows from self_val,
    // and invoke_block may mutate self_val.
    let given = given.to_string();
    let data = vm.get_block_data(globals, block)?;
    vm.clear_capture_special_variables();
    for cap in re.captures_iter(&given) {
        let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
        vm.save_capture_special_variables(&cap);
        match cap.len() {
            0 => unreachable!(),
            1 => {
                let val = Value::string(cap.get(0).unwrap().to_string());
                vm.invoke_block(globals, &data, &[val])?;
            }
            len => {
                let mut vec = vec![];
                for i in 1..len {
                    match cap.get(i) {
                        Some(m) => {
                            vec.push(Value::string(m.as_str().to_string()));
                        }
                        None => vec.push(Value::nil()),
                    }
                }
                let val = Value::array_from_vec(vec);
                vm.invoke_block(globals, &data, &val.as_array())?;
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
fn string_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let pos = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.coerce_to_int_i64(vm, globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        }
    } else {
        0usize
    };
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let re = lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;

    RegexpInner::match_one(vm, globals, re, given, lfp.block(), pos)
}

///
/// ### String#match?
///
/// - match?(regexp, pos = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/match=3f.html]
#[monoruby_builtin]
fn string_match_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let pos = if let Some(arg1) = lfp.try_arg(1) {
        match arg1.coerce_to_int_i64(vm, globals)? {
            pos if pos >= 0 => pos as usize,
            _ => return Ok(Value::nil()),
        }
    } else {
        0usize
    };
    let self_ = lfp.self_val();
    let given = self_.expect_str(globals)?;
    let re = lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;

    let res = RegexpInner::match_pred(&re, given, pos)?;
    Ok(Value::bool(res))
}

///
/// ### String#index
///
/// - index(pattern, pos = 0) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/index.html]
#[monoruby_builtin]
fn string_index(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let char_pos = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_int_i64(vm, globals)?
    } else {
        0
    };
    let self_ = lfp.self_val();
    let given = self_.is_rstring().unwrap();
    let re = lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;

    let char_pos = match given.conv_char_index(char_pos)? {
        Some(pos) => pos,
        None => return Ok(Value::nil()),
    };

    let s = given.check_utf8()?;
    let char_len = s.chars().count();
    if char_pos == char_len {
        // At end of string, only empty-width matches are possible
        return match re.captures("", vm)? {
            Some(captures) if captures.get(0).unwrap().range().is_empty() => {
                Ok(Value::integer(char_pos as i64))
            }
            _ => Ok(Value::nil()),
        };
    }
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
/// ### String#byteindex
///
/// - byteindex(pattern, offset = 0) -> Integer | nil
///
/// Returns the byte-based index of the first occurrence of +pattern+ in the string.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/byteindex.html]
#[monoruby_builtin]
fn byteindex(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let byte_offset = if let Some(arg1) = lfp.try_arg(1) {
        let offset = arg1.coerce_to_int_i64(vm, globals)?;
        if offset < 0 {
            let self_ = lfp.self_val();
            let given = self_.is_rstring().unwrap();
            let len = given.as_bytes().len() as i64;
            let pos = len + offset;
            if pos < 0 {
                return Ok(Value::nil());
            }
            pos as usize
        } else {
            offset as usize
        }
    } else {
        0
    };
    let self_ = lfp.self_val();
    let given = self_.is_rstring().unwrap();
    let haystack = given.as_bytes();
    if byte_offset > haystack.len() {
        return Ok(Value::nil());
    }
    let re = lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;
    let s = std::str::from_utf8(haystack).map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;

    // Ensure byte_offset falls on a valid UTF-8 character boundary.
    if !s.is_char_boundary(byte_offset) {
        return Err(MonorubyErr::argumenterr(format!(
            "invalid byte offset {}",
            byte_offset
        )));
    }

    match re.captures_from_pos(s, byte_offset, vm)? {
        None => Ok(Value::nil()),
        Some(captures) => {
            let start = captures.get(0).unwrap().start();
            Ok(Value::integer(start as i64))
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
fn string_rindex(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let given = self_.is_rstring().unwrap();
    let re = lfp.arg(0).coerce_to_regexp_or_string(vm, globals)?;

    let s = given.check_utf8()?;
    let char_len = s.chars().count();

    let max_char_pos = if let Some(arg1) = lfp.try_arg(1) {
        let pos = arg1.coerce_to_int_i64(vm, globals)?;
        match given.conv_char_index2(pos)? {
            Some(pos) => pos,
            None => return Ok(Value::nil()),
        }
    } else {
        char_len
    };

    let mut last_byte_pos = match re.captures_from_pos(s, 0, vm)? {
        None => {
            return Ok(Value::nil());
        }
        Some(captures) => captures.get(0).unwrap().start(),
    };

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
    // Handle match at end of string (e.g. zero-width match past last char_indices entry)
    // Check if the pattern can match empty at the end of string
    if char_len <= max_char_pos {
        if last_byte_pos == s.len() {
            last_char_pos = Some(char_len);
        } else if last_byte_pos < s.len() {
            // Try to find a zero-width match at the end of string
            if let Ok(Some(captures)) = re.captures("", vm) {
                if captures.get(0).unwrap().range().is_empty() {
                    last_char_pos = Some(char_len);
                }
            }
        }
    }
    Ok(match last_char_pos {
        Some(pos) => Value::integer(pos as i64),
        None => Value::nil(),
    })
}

///
/// ### String#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/length.html]
#[monoruby_builtin]
fn length(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn bytesize(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let length = lfp.self_val().as_rstring_inner().len();
    Ok(Value::integer(length as i64))
}

fn string_bytesize(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (STRING_INLINE_CAP);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            salq rax, 1;
            orq  rax, 1;
        }
    });
    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

///
/// ### String#ord
///
/// - ord -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/ord.html]
#[monoruby_builtin]
fn ord(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn ljust(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding_owned;
    let padding = if let Some(arg1) = &arg1 {
        padding_owned = arg1.coerce_to_string(vm, globals)?;
        padding_owned.as_str()
    } else {
        " "
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
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
fn rjust(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding_owned;
    let padding = if let Some(arg1) = &arg1 {
        padding_owned = arg1.coerce_to_string(vm, globals)?;
        padding_owned.as_str()
    } else {
        " "
    };
    if padding.is_empty() {
        return Err(MonorubyErr::zero_width_padding());
    };
    let self_ = lfp.self_val();
    let lhs = self_.as_str();
    let width = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
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
fn lines(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn bytes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
/// ### String#getbyte
///
/// - getbyte(index) -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/getbyte.html]
#[monoruby_builtin]
fn getbyte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let receiver = lfp.self_val();
    let s = receiver.as_rstring_inner();
    let len = s.len() as i64;
    let mut idx = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if idx < 0 {
        idx += len;
    }
    if idx < 0 || idx >= len {
        Ok(Value::nil())
    } else {
        Ok(Value::integer(s[idx as usize] as i64))
    }
}

///
/// ### String#setbyte
///
/// - setbyte(index, value) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/setbyte.html]
#[monoruby_builtin]
fn setbyte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ = lfp.self_val();
    let byte_val = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
    let s = self_.as_rstring_inner();
    let len = s.len() as i64;
    let mut idx = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if idx < 0 {
        idx += len;
    }
    if idx < 0 || idx >= len {
        return Err(MonorubyErr::indexerr(format!(
            "index {} out of string",
            lfp.arg(0).coerce_to_int_i64(vm, globals)?
        )));
    }
    self_
        .as_rstring_inner_mut()
        .set_byte(idx as usize, byte_val as u8);
    Ok(lfp.arg(1))
}

///
/// ### String#byteslice
///
/// - byteslice(nth) -> String | nil
/// - byteslice(nth, len) -> String | nil
/// - byteslice(range) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/byteslice.html]
#[monoruby_builtin]
fn byteslice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.as_rstring_inner();
    let byte_len = s.len();
    let enc = s.encoding();

    let conv_byte_index = |i: i64| -> Option<usize> {
        if i >= 0 {
            if (i as usize) < byte_len {
                Some(i as usize)
            } else {
                None
            }
        } else {
            let idx = byte_len as i64 + i;
            if idx < 0 { None } else { Some(idx as usize) }
        }
    };

    if let Some(range) = lfp.arg(0).is_range() {
        let start = range.start().coerce_to_int_i64(vm, globals)?;
        let end = range.end().coerce_to_int_i64(vm, globals)?;
        let start = match {
            if start >= 0 {
                if (start as usize) <= byte_len {
                    Some(start as usize)
                } else {
                    None
                }
            } else {
                let idx = byte_len as i64 + start;
                if idx < 0 { None } else { Some(idx as usize) }
            }
        } {
            Some(s) => s,
            None => return Ok(Value::nil()),
        };
        let end = if end >= 0 {
            let e = if range.exclude_end() {
                end as usize
            } else {
                (end as usize).saturating_add(1)
            };
            e.min(byte_len)
        } else {
            let idx = byte_len as i64 + end;
            if idx < 0 {
                return Ok(Value::nil());
            }
            let e = if range.exclude_end() {
                idx as usize
            } else {
                (idx as usize).saturating_add(1)
            };
            e.min(byte_len)
        };
        if start > end {
            return Ok(Value::string_from_inner(RStringInner::from_encoding(
                &[],
                enc,
            )));
        }
        Ok(Value::string_from_inner(RStringInner::from_encoding(
            &s[start..end],
            enc,
        )))
    } else {
        let i = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        if let Some(arg1) = lfp.try_arg(1) {
            // byteslice(nth, len)
            let len = arg1.coerce_to_int_i64(vm, globals)?;
            if len < 0 {
                return Ok(Value::nil());
            }
            let start = match {
                if i >= 0 {
                    if (i as usize) <= byte_len {
                        Some(i as usize)
                    } else {
                        None
                    }
                } else {
                    let idx = byte_len as i64 + i;
                    if idx < 0 { None } else { Some(idx as usize) }
                }
            } {
                Some(s) => s,
                None => return Ok(Value::nil()),
            };
            let end = (start + len as usize).min(byte_len);
            Ok(Value::string_from_inner(RStringInner::from_encoding(
                &s[start..end],
                enc,
            )))
        } else {
            // byteslice(nth)
            match conv_byte_index(i) {
                Some(idx) => Ok(Value::string_from_inner(RStringInner::from_encoding(
                    &s[idx..idx + 1],
                    enc,
                ))),
                None => Ok(Value::nil()),
            }
        }
    }
}

///
/// ### String#bytesplice
///
/// - bytesplice(index, length, str) -> self
/// - bytesplice(index, length, str, str_index, str_length) -> self
/// - bytesplice(range, str) -> self
/// - bytesplice(range, str, str_range) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/bytesplice.html]
#[monoruby_builtin]
fn bytesplice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let arg_count = if lfp.try_arg(4).is_some() {
        5
    } else if lfp.try_arg(3).is_some() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 4, expected 2, 3, or 5)",
        ));
    } else if lfp.try_arg(2).is_some() {
        3
    } else if lfp.try_arg(1).is_some() {
        2
    } else {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 1, expected 2, 3, or 5)",
        ));
    };

    let mut self_ = lfp.self_val();
    let byte_len = self_.as_rstring_inner().len();

    // Parse target range (index, length) from the first args
    let (start, splice_len) = if let Some(range) = lfp.arg(0).is_range() {
        let rstart = range.start().coerce_to_int_i64(vm, globals)?;
        let rend = range.end().coerce_to_int_i64(vm, globals)?;
        let start = conv_byte_index_for_splice(rstart, byte_len)?;
        let end = if rend >= 0 {
            let e = if range.exclude_end() {
                rend as usize
            } else {
                (rend as usize).saturating_add(1)
            };
            e.min(byte_len)
        } else {
            let idx = byte_len as i64 + rend;
            if idx < 0 {
                // Negative end that resolves below 0 means length=0 (insert, no replace)
                0
            } else {
                let e = if range.exclude_end() {
                    idx as usize
                } else {
                    (idx as usize).saturating_add(1)
                };
                e.min(byte_len)
            }
        };
        let len = if end > start { end - start } else { 0 };
        (start, len)
    } else {
        // index, length form
        if arg_count < 3 {
            return Err(MonorubyErr::typeerr(
                "wrong argument type Integer (expected Range)",
            ));
        }
        let idx = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        let len = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
        if len < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length {}", len)));
        }
        let start = conv_byte_index_for_splice(idx, byte_len)?;
        let splice_len = (len as usize).min(byte_len - start);
        (start, splice_len)
    };

    // Determine the replacement string and optional sub-range
    let (str_arg_idx, has_src_range) = if lfp.arg(0).is_range().is_some() {
        // range form: bytesplice(range, str) or bytesplice(range, str, str_range)
        (1, arg_count == 3)
    } else {
        // index form: bytesplice(idx, len, str) or bytesplice(idx, len, str, str_idx, str_len)
        (2, arg_count == 5)
    };

    let str_val = lfp.arg(str_arg_idx);
    let str_inner = str_val.as_rstring_inner();
    let str_bytes = str_inner.as_bytes();
    let str_byte_len = str_bytes.len();

    let replacement = if has_src_range {
        if lfp.arg(0).is_range().is_some() {
            // bytesplice(range, str, str_range)
            let src_range = lfp.arg(str_arg_idx + 1);
            let src_range = src_range.is_range().ok_or_else(|| {
                MonorubyErr::typeerr("wrong argument type Integer (expected Range)")
            })?;
            let src_start = src_range.start().coerce_to_int_i64(vm, globals)?;
            let src_end = src_range.end().coerce_to_int_i64(vm, globals)?;
            let src_start = conv_byte_index_for_splice(src_start, str_byte_len)?;
            let src_end_val = if src_end >= 0 {
                let e = if src_range.exclude_end() {
                    src_end as usize
                } else {
                    (src_end as usize).saturating_add(1)
                };
                e.min(str_byte_len)
            } else {
                let idx = str_byte_len as i64 + src_end;
                if idx < 0 {
                    0
                } else {
                    let e = if src_range.exclude_end() {
                        idx as usize
                    } else {
                        (idx as usize).saturating_add(1)
                    };
                    e.min(str_byte_len)
                }
            };
            if src_start > src_end_val {
                &[]
            } else {
                &str_bytes[src_start..src_end_val]
            }
        } else {
            // bytesplice(idx, len, str, str_idx, str_len)
            let src_idx = lfp.arg(str_arg_idx + 1).coerce_to_int_i64(vm, globals)?;
            let src_len = lfp.arg(str_arg_idx + 2).coerce_to_int_i64(vm, globals)?;
            if src_len < 0 {
                return Err(MonorubyErr::indexerr(format!(
                    "negative length {}",
                    src_len
                )));
            }
            let src_start = conv_byte_index_for_splice(src_idx, str_byte_len)?;
            let src_splice_len = (src_len as usize).min(str_byte_len - src_start);
            &str_bytes[src_start..src_start + src_splice_len]
        }
    } else {
        str_bytes
    };

    // Check character boundary for UTF-8 strings
    let self_enc = self_.as_rstring_inner().encoding();
    if self_enc.is_utf8_compatible() {
        let self_bytes = self_.as_rstring_inner().as_bytes();
        if !is_char_boundary(self_bytes, start) {
            return Err(MonorubyErr::indexerr(format!(
                "offset {} does not land on character boundary",
                start
            )));
        }
        let end = start + splice_len;
        if !is_char_boundary(self_bytes, end) {
            return Err(MonorubyErr::indexerr(format!(
                "offset {} does not land on character boundary",
                end
            )));
        }
    }

    // Check character boundary for UTF-8 source string
    let str_enc = str_inner.encoding();
    if str_enc.is_utf8_compatible() && has_src_range {
        // replacement slice was already extracted from str_bytes,
        // but we need to verify the offsets used were on char boundaries.
        // The offsets are relative to str_bytes, so we check using the
        // replacement pointer offset from str_bytes start.
        let rep_start = if replacement.is_empty() {
            0
        } else {
            // SAFETY: replacement is a subslice of str_bytes
            unsafe { replacement.as_ptr().offset_from(str_bytes.as_ptr()) as usize }
        };
        let rep_end = rep_start + replacement.len();
        if !is_char_boundary(str_bytes, rep_start) {
            return Err(MonorubyErr::indexerr(format!(
                "offset {} does not land on character boundary",
                rep_start
            )));
        }
        if !is_char_boundary(str_bytes, rep_end) {
            return Err(MonorubyErr::indexerr(format!(
                "offset {} does not land on character boundary",
                rep_end
            )));
        }
    }

    // Check encoding compatibility
    match (self_enc, str_enc) {
        (Encoding::Utf8 | Encoding::UsAscii, Encoding::Ascii8) => {
            if !replacement.is_ascii() && !self_.as_rstring_inner().as_bytes().is_ascii() {
                return Err(MonorubyErr::runtimeerr(
                    "incompatible character encodings: UTF-8 and ASCII-8BIT",
                ));
            }
        }
        (Encoding::Ascii8, Encoding::Utf8 | Encoding::UsAscii) => {
            // OK
        }
        _ => {}
    }

    self_
        .as_rstring_inner_mut()
        .bytesplice(start, splice_len, replacement);

    Ok(self_)
}

/// Convert a signed byte index to unsigned, raising IndexError if out of bounds.
/// For splice operations, index == byte_len is valid (for appending).
fn conv_byte_index_for_splice(idx: i64, byte_len: usize) -> Result<usize> {
    if idx >= 0 {
        if (idx as usize) <= byte_len {
            Ok(idx as usize)
        } else {
            Err(MonorubyErr::indexerr(format!(
                "index {} out of string",
                idx
            )))
        }
    } else {
        let actual = byte_len as i64 + idx;
        if actual < 0 {
            Err(MonorubyErr::indexerr(format!(
                "index {} out of string",
                idx
            )))
        } else {
            Ok(actual as usize)
        }
    }
}

/// Check if the given byte offset is on a UTF-8 character boundary.
/// Returns true for offset == bytes.len() (end of string).
fn is_char_boundary(bytes: &[u8], offset: usize) -> bool {
    if offset == 0 || offset >= bytes.len() {
        return true;
    }
    // A byte is a char boundary if it's not a UTF-8 continuation byte (0x80..0xBF)
    !matches!(bytes[offset], 0x80..=0xBF)
}

///
/// ### String#each_line
///
/// - each_line(rs = $/, [NOT SUPPORTED] chomp: false) {|line| ... } -> self
/// - [NOT SUPPORTED]each_line(rs = $/, chomp: false) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/each_line.html]
#[monoruby_builtin]
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let rs_owned;
    let rs: &str = if let Some(arg0) = &arg0 {
        rs_owned = arg0.coerce_to_str(vm, globals)?;
        &rs_owned
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
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_rstring_inner().is_empty()))
}

///
/// ### String#to_f
///
/// - to_f -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_f.html]
#[monoruby_builtin]
fn to_f(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.expect_str(globals)?;
    let f = parse_f64(s).0;
    Ok(Value::float(f))
}

///
/// ### String#to_c
///
/// - to_c -> Complex
///
/// Returns a complex number parsed from the string.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_c.html]
#[monoruby_builtin]
fn to_c(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.expect_str(globals)?;
    let (re, im) = parse_complex(s);
    Ok(Value::complex(parse_real(re), parse_real(im)))
}

/// Parse a numeric string to a Real -- integer if possible, float otherwise.
fn parse_real(s: f64) -> Real {
    if s == (s as i64) as f64 && s.is_finite() {
        Real::from(s as i64)
    } else {
        Real::from(s)
    }
}

///
/// ### String#to_r
///
/// - to_r -> Rational
///
/// Returns a rational number parsed from the string.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_r.html]
#[monoruby_builtin]
fn to_r(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.expect_str(globals)?;
    let (num, den) = parse_rational(s);
    // Call Kernel#Rational to create the Rational object
    let rational_id = IdentId::get_id("Rational");
    let main_obj = globals.main_object;
    let func_id = globals
        .check_method(main_obj, rational_id)
        .ok_or_else(|| MonorubyErr::runtimeerr("Rational method not found"))?;
    vm.invoke_func_inner(
        globals,
        func_id,
        main_obj,
        &[Value::integer(num), Value::integer(den)],
        None,
        None,
    )
}

/// Parse a string as a complex number, returning (real, imaginary) as f64.
fn parse_complex(s: &str) -> (f64, f64) {
    let s = s.trim();
    if s.is_empty() {
        return (0.0, 0.0);
    }

    // Handle pure imaginary: "3i", "-2i", "+5i", "i", "-i", "+i"
    if let Some(rest) = s.strip_suffix('i') {
        if rest.is_empty() {
            return (0.0, 1.0);
        }
        if rest == "+" {
            return (0.0, 1.0);
        }
        if rest == "-" {
            return (0.0, -1.0);
        }
        // Try "a+bi" or "a-bi" pattern
        // Find the last '+' or '-' that is not at the start and not part of an exponent
        if let Some(pos) = find_complex_split(rest) {
            let real_part = &rest[..pos];
            let imag_part = &rest[pos..];
            let re = parse_f64_simple(real_part);
            let im = if imag_part == "+" {
                1.0
            } else if imag_part == "-" {
                -1.0
            } else {
                parse_f64_simple(imag_part)
            };
            return (re, im);
        }
        // Pure imaginary
        let im = parse_f64_simple(rest);
        return (0.0, im);
    }

    // Pure real
    let re = parse_f64_simple(s);
    (re, 0.0)
}

/// Find the split point between real and imaginary parts in a complex string.
/// Returns the position of the '+' or '-' that separates them.
fn find_complex_split(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        i -= 1;
        if (bytes[i] == b'+' || bytes[i] == b'-') && i > 0 {
            // Make sure this isn't part of a scientific notation exponent
            if i >= 2 && (bytes[i - 1] == b'e' || bytes[i - 1] == b'E') {
                continue;
            }
            return Some(i);
        }
    }
    None
}

/// Simple f64 parser that returns 0.0 for invalid input.
fn parse_f64_simple(s: &str) -> f64 {
    let s = s.trim();
    if s.is_empty() {
        return 0.0;
    }
    s.parse::<f64>().unwrap_or(0.0)
}

/// Parse a string as a rational number, returning (numerator, denominator).
fn parse_rational(s: &str) -> (i64, i64) {
    let s = s.trim();
    if s.is_empty() {
        return (0, 1);
    }

    // Try "a/b" format
    if let Some(pos) = s.find('/') {
        let num_str = &s[..pos];
        let den_str = &s[pos + 1..];
        let num = parse_rational_int(num_str);
        let den = parse_rational_int(den_str);
        if den == 0 {
            return (0, 1);
        }
        return (num, den);
    }

    // Try decimal format "0.5" -> 1/2
    if let Some(pos) = s.find('.') {
        let int_part = &s[..pos];
        let frac_part = &s[pos + 1..];
        // Count decimal digits (stop at first non-digit)
        let frac_digits: String = frac_part
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .collect();
        let dec_places = frac_digits.len();
        if dec_places == 0 {
            let num = parse_rational_int(int_part);
            return (num, 1);
        }
        let int_val = parse_rational_int(int_part);
        let frac_val: i64 = frac_digits.parse().unwrap_or(0);
        let den = 10i64.pow(dec_places as u32);
        let negative = s.starts_with('-');
        let num = if negative {
            int_val * den - frac_val
        } else {
            int_val * den + frac_val
        };
        // Simplify
        let g = gcd_u64(num.unsigned_abs(), den as u64) as i64;
        return (num / g, den / g);
    }

    // Plain integer
    let num = parse_rational_int(s);
    (num, 1)
}

fn parse_rational_int(s: &str) -> i64 {
    let s = s.trim();
    if s.is_empty() {
        return 0;
    }
    // Parse leading integer, stop at first non-digit (after optional sign)
    let mut chars = s.chars().peekable();
    let negative = if chars.peek() == Some(&'-') {
        chars.next();
        true
    } else if chars.peek() == Some(&'+') {
        chars.next();
        false
    } else {
        false
    };
    let digits: String = chars.take_while(|c| c.is_ascii_digit()).collect();
    if digits.is_empty() {
        return 0;
    }
    let val: i64 = digits.parse().unwrap_or(0);
    if negative { -val } else { val }
}

fn gcd_u64(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd_u64(b, a % b) }
}

///
/// ### String#to_i
///
/// - to_i(base = 10) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/to_i.html]
#[monoruby_builtin]
fn to_i(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.expect_str(globals)?;
    let radix = if let Some(arg0) = lfp.try_arg(0) {
        match arg0.coerce_to_int_i64(vm, globals)? {
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

///
/// ### String#hex
///
/// - hex -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/hex.html]
#[monoruby_builtin]
fn hex(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.as_str();
    let s = s.trim_start();
    // Handle optional sign before 0x/0X prefix
    let (s, negative) = if let Some(rest) = s.strip_prefix('-') {
        (rest, true)
    } else if let Some(rest) = s.strip_prefix('+') {
        (rest, false)
    } else {
        (s, false)
    };
    let s = s
        .strip_prefix("0x")
        .or_else(|| s.strip_prefix("0X"))
        .unwrap_or(s);
    Ok(parse_int_value(s, 16, negative))
}

///
/// ### String#oct
///
/// - oct -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/oct.html]
#[monoruby_builtin]
fn oct(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let s = self_.as_str();
    let s = s.trim_start();
    // Handle optional sign before prefix
    let (s, negative) = if let Some(rest) = s.strip_prefix('-') {
        (rest, true)
    } else if let Some(rest) = s.strip_prefix('+') {
        (rest, false)
    } else {
        (s, false)
    };
    let (s, radix) = if let Some(rest) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        (rest, 16)
    } else if let Some(rest) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        (rest, 2)
    } else if let Some(rest) = s.strip_prefix("0d").or_else(|| s.strip_prefix("0D")) {
        (rest, 10)
    } else if let Some(rest) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
        (rest, 8)
    } else {
        (s, 8)
    };
    Ok(parse_int_value(s, radix, negative))
}

/// Parse a non-negative integer string with the given radix, then optionally negate.
fn parse_int_value(s: &str, radix: u32, negative: bool) -> Value {
    if let Some((i, _)) = parse_i64(s, radix) {
        if negative {
            if let Some(i) = (i as i64).checked_neg() {
                Value::integer(i)
            } else {
                Value::bigint(-BigInt::from(i))
            }
        } else {
            Value::integer(i)
        }
    } else {
        let v = parse_bigint(s, radix);
        if negative {
            Value::bigint(-v)
        } else {
            Value::bigint(v)
        }
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
fn to_sym(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let inner = self_val.as_rstring_inner();
    let id = if inner.encoding().is_utf8_compatible() {
        let s = inner.check_utf8()?;
        IdentId::get_id(s)
    } else {
        IdentId::get_id_from_bytes(inner.as_bytes().to_vec())
    };
    Ok(Value::symbol(id))
}

///
/// ### String#upcase
///
/// - upcase([NOT SUPPORTED] *options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase.html]
#[monoruby_builtin]
fn upcase(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?.to_uppercase();
    Ok(Value::string(s))
}

///
/// ### String#upcase!
///
/// - upcase!([NOT SUPPORTED] *options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/upcase=21.html]
#[monoruby_builtin]
fn upcase_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?.to_uppercase();
    let changed = &s != self_val.expect_str(globals)?;
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
fn downcase(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?.to_lowercase();
    Ok(Value::string(s))
}

///
/// ### String#downcase!
///
/// - downcase!([NOT SUPPORTED] *options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/downcase=21.html]
#[monoruby_builtin]
fn downcase_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?.to_lowercase();
    let changed = &s != self_val.expect_str(globals)?;
    self_val.replace_string(s);

    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

///
/// ### String#capitalize
///
/// - capitalize([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/capitalize.html]
#[monoruby_builtin]
fn capitalize(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?;
    let mut result = String::with_capacity(s.len());
    let mut first = true;
    for c in s.chars() {
        if first {
            for uc in c.to_uppercase() {
                result.push(uc);
            }
            first = false;
        } else {
            for lc in c.to_lowercase() {
                result.push(lc);
            }
        }
    }
    Ok(Value::string(result))
}

///
/// ### String#capitalize!
///
/// - capitalize!([NOT SUPPORTED]*options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/capitalize=21.html]
#[monoruby_builtin]
fn capitalize_(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?;
    let mut result = String::with_capacity(s.len());
    let mut first = true;
    for c in s.chars() {
        if first {
            for uc in c.to_uppercase() {
                result.push(uc);
            }
            first = false;
        } else {
            for lc in c.to_lowercase() {
                result.push(lc);
            }
        }
    }
    let changed = &result != self_val.expect_str(globals)?;
    self_val.replace_string(result);
    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

///
/// ### String#swapcase
///
/// - swapcase([NOT SUPPORTED]*options) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/swapcase.html]
#[monoruby_builtin]
fn swapcase(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?;
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_uppercase() {
            for lc in c.to_lowercase() {
                result.push(lc);
            }
        } else if c.is_lowercase() {
            for uc in c.to_uppercase() {
                result.push(uc);
            }
        } else {
            result.push(c);
        }
    }
    Ok(Value::string(result))
}

///
/// ### String#swapcase!
///
/// - swapcase!([NOT SUPPORTED]*options) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/swapcase=21.html]
#[monoruby_builtin]
fn swapcase_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_val = lfp.self_val();
    let s = self_val.expect_str(globals)?;
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_uppercase() {
            for lc in c.to_lowercase() {
                result.push(lc);
            }
        } else if c.is_lowercase() {
            for uc in c.to_uppercase() {
                result.push(uc);
            }
        } else {
            result.push(c);
        }
    }
    let changed = &result != self_val.expect_str(globals)?;
    self_val.replace_string(result);
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
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut res = lfp.self_val().as_str().to_string();
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    let strs: Vec<String> = args
        .iter()
        .map(|arg| arg.coerce_to_string(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    let pred = strs
        .iter()
        .map(|s| Tr::from_str(s))
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
fn tr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // TODO: support tr(1)
    let self_ = lfp.self_val();
    let from = lfp.arg(0).coerce_to_string(vm, globals)?;
    let to = lfp.arg(1).coerce_to_string(vm, globals)?;
    let rec = self_.expect_str(globals)?;
    let res = rec.replace(&from, &to);
    Ok(Value::string(res))
}

///
/// ### String#count
///
/// - count(*chars) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/count.html]
#[monoruby_builtin]
fn count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let strs: Vec<String> = args
        .iter()
        .map(|arg| arg.coerce_to_string(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    let self_ = lfp.self_val();
    let target = self_.as_str();
    let mut c = 0;
    for ch in target.chars() {
        for s in strs.iter() {
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
fn sum(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bits = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_int_i64(vm, globals)?
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
fn replace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let s = lfp.arg(0).coerce_to_string(vm, globals)?;
    lfp.self_val().replace_str(&s);
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
fn chars(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn each_char(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    if let Some(bh) = lfp.block() {
        let iter = recv.chars().map(|c| Value::string(c.to_string()));
        vm.invoke_block_iter1(globals, bh, iter)?;
        Ok(lfp.self_val())
    } else {
        vm.generate_enumerator(IdentId::get_id("each_char"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### String#center
///
/// - center(width, padding = ' ') -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/center.html]
#[monoruby_builtin]
fn center(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg1 = lfp.try_arg(1);
    let padding_owned;
    let padding = if let Some(arg) = &arg1 {
        padding_owned = arg.coerce_to_string(vm, globals)?;
        padding_owned.as_str()
    } else {
        " "
    };
    if padding.is_empty() {
        return Err(MonorubyErr::argumenterr("Zero width padding."));
    };
    let lhs = lfp.self_val();
    let width = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
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
fn next(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    let res = Value::string(str_next(recv));
    Ok(res)
}

///
/// ### String#next!
///
/// - next! -> self
/// - succ! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/next=21.html]
#[monoruby_builtin]
fn next_mut(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ = lfp.self_val();
    let recv = self_.expect_str(globals)?;
    let new_str = str_next(recv);
    self_.replace_string(new_str);
    Ok(self_)
}

///
/// ### String#unpack
///
/// - unpack(template) -> Array
/// - unpack(template, offset: integer) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/unpack.html]
#[monoruby_builtin]
fn unpack(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let offset = unpack_offset(vm, globals, lfp, self_.as_rstring_inner().len())?;
    let template = lfp.arg(0).coerce_to_string(vm, globals)?;
    rvalue::unpack(&self_.as_rstring_inner()[offset..], &template, false)
}

///
/// ### String#unpack1
///
/// - unpack1(format) -> object
/// - unpack1(format, offset: integer) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/unpack1.html]
#[monoruby_builtin]
fn unpack1(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let offset = unpack_offset(vm, globals, lfp, self_.as_rstring_inner().len())?;
    let template = lfp.arg(0).coerce_to_string(vm, globals)?;
    rvalue::unpack(&self_.as_rstring_inner()[offset..], &template, true)
}

fn unpack_offset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, len: usize) -> Result<usize> {
    match lfp.try_arg(1) {
        Some(v) => {
            let offset = v.coerce_to_int_i64(vm, globals)?;
            if offset < 0 || offset as usize > len {
                Err(MonorubyErr::argumenterr("offset outside of string"))
            } else {
                Ok(offset as usize)
            }
        }
        None => Ok(0),
    }
}

///
/// ### String#dump
///
/// - dump -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/dump.html]
#[monoruby_builtin]
fn dump(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string(format!(
        r#""{}""#,
        lfp.self_val().as_rstring_inner().dump()
    )))
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
    fn string_shl_encoding() {
        // UTF-8 << ASCII-8BIT (ASCII chars only) => keeps UTF-8
        run_test(
            r##"
            s = "hello".encode("UTF-8")
            s << "world".force_encoding("ASCII-8BIT")
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // UTF-8 << ASCII-8BIT (non-ASCII) => downgrades to ASCII-8BIT
        run_test(
            r##"
            s = "hello".encode("UTF-8")
            s << "\x80".force_encoding("ASCII-8BIT")
            s.encoding == Encoding::ASCII_8BIT
        "##,
        );
        // ASCII-8BIT (ASCII only) << UTF-8 (non-ASCII) => upgrades to UTF-8
        run_test(
            r##"
            s = "hello".force_encoding("ASCII-8BIT")
            s << "こんにちは"
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // UTF-8 (non-ASCII) << ASCII-8BIT (non-ASCII) => Encoding::CompatibilityError
        run_test_error(
            r##"
            s = "こんにちは"
            s << "\x80".force_encoding("ASCII-8BIT")
        "##,
        );
        // ASCII-8BIT << UTF-8 (ASCII only) => keeps ASCII-8BIT
        run_test(
            r##"
            s = "hello".force_encoding("ASCII-8BIT")
            s << "world".encode("UTF-8")
            [s, s.encoding == Encoding::ASCII_8BIT]
        "##,
        );
        // empty UTF-8 << ASCII-8BIT (ASCII only) => keeps UTF-8
        run_test(
            r##"
            s = "".encode("UTF-8")
            s << "hello".force_encoding("ASCII-8BIT")
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // empty UTF-8 << ASCII-8BIT (non-ASCII) => downgrades to ASCII-8BIT
        run_test(
            r##"
            s = "".encode("UTF-8")
            s << "\x80".force_encoding("ASCII-8BIT")
            s.encoding == Encoding::ASCII_8BIT
        "##,
        );
        // empty ASCII-8BIT << UTF-8 (ASCII only) => keeps ASCII-8BIT
        run_test(
            r##"
            s = "".force_encoding("ASCII-8BIT")
            s << "hello".encode("UTF-8")
            [s, s.encoding == Encoding::ASCII_8BIT]
        "##,
        );
        // empty ASCII-8BIT << UTF-8 (non-ASCII) => upgrades to UTF-8
        run_test(
            r##"
            s = "".force_encoding("ASCII-8BIT")
            s << "こんにちは"
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // Integer argument: UTF-8 string << Unicode codepoint
        run_test(
            r##"
            s = "hello"
            s << 0x3042
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // Integer argument: ASCII-8BIT << 0x80
        run_test(
            r##"
            s = "hello".force_encoding("ASCII-8BIT")
            s << 0x80
            [s.bytes.to_a, s.encoding == Encoding::ASCII_8BIT]
        "##,
        );
        // Integer argument: ASCII char
        run_test(
            r##"
            s = "hello"
            s << 33
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
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
        // Range with start in-bounds but end out-of-bounds returns ""
        run_test(r##""symbol"[-2..-10]"##);
        run_test(r##""symbol"[2..-10]"##);
        // Range with start out-of-bounds returns nil
        run_test(r##""symbol"[10..12]"##);
        run_test(r##""symbol"[-10..-12]"##);
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
    fn string_format_g() {
        // %g and %G: shortest representation
        run_test2(r###""%g" % 100.0"###);
        run_test2(r###""%g" % 0.0001"###);
        run_test2(r###""%g" % 123456.789"###);
        run_test2(r###""%g" % 1.0e-5"###);
        run_test2(r###""%G" % 100.0"###);
        run_test2(r###""%G" % 1.0e-5"###);
        run_test2(r###""%g" % 0.0"###);
        run_test2(r###""%g" % 1.0"###);
        run_test2(r###""%20.10g" % 1.23456789"###);
    }

    #[test]
    fn string_format_b_upper() {
        // %B: binary uppercase
        run_test2(r###""%B" % 10"###);
        run_test2(r###""%#B" % 10"###);
        run_test2(r###""%B" % 0"###);
    }

    #[test]
    fn string_format_u() {
        // %u: unsigned decimal
        run_test2(r###""%u" % 42"###);
        run_test2(r###""%u" % 0"###);
        run_test2(r###""%10u" % 42"###);
    }

    #[test]
    fn string_format_p() {
        // %p: inspect
        run_test2(r###""%p" % "hello""###);
        run_test2(r###""%p" % 42"###);
        run_test2(r###""%p" % nil"###);
        run_test2(r###""%20p" % "hello""###);
    }

    #[test]
    fn string_format_o() {
        // %o: octal
        run_test2(r###""%o" % 255"###);
        run_test2(r###""%o" % 8"###);
        run_test2(r###""%o" % 0"###);
        run_test2(r###""%#o" % 255"###);
        run_test2(r###""%#o" % 0"###);
    }

    #[test]
    fn string_format_alternate() {
        // # flag (alternate form)
        run_test2(r###""%#x" % 255"###);
        run_test2(r###""%#X" % 255"###);
        run_test2(r###""%#o" % 255"###);
        run_test2(r###""%#b" % 10"###);
        run_test2(r###""%#B" % 10"###);
        // # flag on zero should not add prefix
        run_test2(r###""%#x" % 0"###);
        run_test2(r###""%#b" % 0"###);
    }

    #[test]
    fn string_format_space_flag() {
        // space flag
        run_test2(r###""% d" % 42"###);
        run_test2(r###""% d" % -42"###);
    }

    #[test]
    fn string_format_plus_flag() {
        // plus flag
        run_test2(r###""%+d" % 42"###);
        run_test2(r###""%+d" % -42"###);
        run_test2(r###""%+d" % 0"###);
    }

    #[test]
    fn string_format_minus_flag() {
        // left-align
        run_test2(r###""%-10d" % 42"###);
        run_test2(r###""%-10s" % "hi""###);
    }

    #[test]
    fn string_format_dynamic_width() {
        // * dynamic width
        run_test2(r###""%*d" % [10, 42]"###);
        run_test2(r###""%*d" % [-10, 42]"###);
    }

    #[test]
    fn string_format_neg_twos_complement() {
        // Negative numbers with %b, %o, %x (two's complement)
        run_test2(r###""%b" % -1"###);
        run_test2(r###""%o" % -1"###);
        run_test2(r###""%x" % -1"###);
        run_test2(r###""%b" % -10"###);
        run_test2(r###""%o" % -10"###);
        run_test2(r###""%x" % -10"###);
        run_test2(r###""%X" % -10"###);
    }

    #[test]
    fn string_format_scientific() {
        // Scientific notation %e/%E
        run_test2(r###""%e" % 1234.5"###);
        run_test2(r###""%E" % 1234.5"###);
        run_test2(r###""%.2e" % 1234.5"###);
        run_test2(r###""%e" % 0.0"###);
        run_test2(r###""%e" % -1234.5"###);
        run_test2(r###""%+e" % 1234.5"###);
        run_test2(r###""% e" % 1234.5"###);
        run_test2(r###""%015.3e" % 1234.5"###);
    }

    #[test]
    fn string_format_hash() {
        run_test2(r###""%{name} is %{age}" % {name: "Alice", age: 30}"###);
        run_test2(r###""%{x}" % {x: "hello"}"###);
        run_test2(r###""%{a}-%{b}" % {a: 1, b: 2}"###);
    }

    #[test]
    fn string_format_overflow() {
        run_test_error(r##""%.25555555555555555555555555555555555555s" % "hello""##);
        run_test_error(r##""%25555555555555555555555555555555555555s" % "hello""##);
    }

    #[test]
    fn string_encode_xml() {
        run_test_no_result_check(r###""hello".encode(xml: :attr)"###);
        run_test_no_result_check(r###""a&b<c>d".encode(xml: :attr)"###);
        run_test_no_result_check(r###""a&b<c>d".encode(xml: :text)"###);
        run_test_no_result_check(r###""plain".encode(xml: :text)"###);
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
        // backreference expansion in gsub
        run_test(
            r##"
        "abc-def-ghi".gsub(/(\w+)-(\w+)/, '\2-\1')
        "##,
        );
        run_test(
            r##"
        "hello world".gsub(/(\w+)/, '[\1]')
        "##,
        );
        // escaped backslash in gsub replacement
        run_test(
            r##"
        "abc".gsub(/b/, '\\\\')
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
        // backreference expansion in sub
        run_test(
            r##"
        "abc-def".sub(/(\w+)-(\w+)/, '\2-\1')
        "##,
        );
        // escaped backslash in sub replacement
        run_test(
            r##"
        "abc".sub(/b/, '\\\\')
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
        run_test(
            r##"
        a = []
        "hello world hello".scan(/hel+o/) {|s| a << $& }
        a
        "##,
        );
        run_test(
            r##"
        a = []
        "abc def ghi".scan(/\w+/) {|m| a << [$&, m] }
        a
        "##,
        );
        run_test(
            r##"
        "nothing".scan(/xyz/) {|s| }
        $&
        "##,
        );
        run_test(
            r##"
        "abc-def-ghi".scan(/(.)-(.)/)
        "##,
        );
        run_test(
            r##"
        "test".scan(/(z)?e/)
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

        // match with negative position returns nil
        run_test(r##""hello".match(/ell/, -10)"##);
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
        // Regexp argument
        run_test(r##""string".start_with?(/str/)"##);
        run_test(r##""string".start_with?(/ing/)"##);
        run_test(r##""hello".delete_prefix("hel")"##);
        run_test(r##""hello".delete_prefix("her")"##);
        run_test(r##"s = "hello"; [s.delete_prefix!("hel"), s]"##);
        run_test(r##"s = "hello"; [s.delete_prefix!("her"), s]"##);
        run_test(r##""string".end_with?("str")"##);
        run_test(r##""string".end_with?("ing")"##);
        run_test(r##""string".end_with?("jng", "hng", "ing")"##);
        run_test_error(r##""string".end_with?("jng", 3, "ing")"##);
        // end_with? UTF-8 character boundary check
        // monoruby assigns BINARY encoding to \xNN literals (CRuby keeps UTF-8),
        // so results differ. Use run_test_no_result_check.
        run_test_no_result_check(r#""\xC3\xA9".end_with?("\xA9")"#);
        run_test_no_result_check(r#""\xe3\x81\x82".end_with?("\x82")"#);
        // Explicit UTF-8 string with force_encoding: boundary check works
        run_test_once(
            r#""\xC3\xA9".force_encoding("UTF-8").end_with?("\xA9".force_encoding("UTF-8"))"#,
        );
        // start_with? UTF-8 character boundary check
        run_test_no_result_check(r#""\xC3\xA9".start_with?("\xC3")"#);
        run_test_once(
            r#""\xC3\xA9".force_encoding("UTF-8").start_with?("\xC3".force_encoding("UTF-8"))"#,
        );
        // start_with? with Regexp sets/clears $~
        run_test(r#""test-123".start_with?(/test/); $~[0]"#);
        run_test(r#""test-123".start_with?(/xxx/); $~"#);
        // end_with? with Regexp raises TypeError
        run_test_error(r#""hello".end_with?(/lo/)"#);
        // Binary string start_with?/end_with?
        run_test_once(r#""\xC3".b.start_with?("\xC3".b)"#);
        run_test_once(r#""\xC3".b.end_with?("\xC3".b)"#);
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
    fn getbyte() {
        run_test(r##""ABC".getbyte(0)"##);
        run_test(r##""ABC".getbyte(2)"##);
        run_test(r##""ABC".getbyte(-1)"##);
        run_test(r##""ABC".getbyte(3)"##);
        run_test(r##""ABC".getbyte(-4)"##);
    }

    #[test]
    fn setbyte() {
        run_test(r##"s = "ABC"; s.setbyte(0, 90); s"##);
        run_test(r##"s = "ABC"; s.setbyte(-1, 90); s"##);
        run_test(r##"s = "ABC"; s.setbyte(0, 255); s.getbyte(0)"##);
        run_test(r##"s = "ABC"; s.setbyte(0, -1); s.getbyte(0)"##);
        run_test(r##"s = "ABC"; s.setbyte(0, 256); s.getbyte(0)"##);
        run_test(r##"s = "ABC"; s.setbyte(0, -129); s.getbyte(0)"##);
        run_test_error(r##""ABC".setbyte(3, 0)"##);
        run_test_error(r##""ABC".setbyte(-4, 0)"##);
    }

    #[test]
    fn byteslice() {
        run_test(r##""hello".byteslice(1)"##);
        run_test(r##""hello".byteslice(-1)"##);
        run_test(r##""hello".byteslice(5)"##);
        run_test(r##""hello".byteslice(-6)"##);
        run_test(r##""hello".byteslice(1, 2)"##);
        run_test(r##""hello".byteslice(1, 0)"##);
        run_test(r##""hello".byteslice(1, -1)"##);
        run_test(r##""hello".byteslice(1..3)"##);
        run_test(r##""hello".byteslice(1...3)"##);
        run_test(r##""hello".byteslice(-3..-1)"##);
        run_test(r##""hello".byteslice(0, 5)"##);
        run_test(r##""hello".byteslice(0, 100)"##);
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
        run_test(r##""   a \t  b \n  c".split"##);
        run_test(r##""hello world".split"##);
        run_test(r##""  hello  world  ".split"##);
        run_test(r##""\t\n hello \t world \n".split"##);
        run_test(r##""".split"##);
        run_test(r##""   ".split"##);
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
    fn hex() {
        run_test(r#""0x0a".hex"#);
        run_test(r#""ff".hex"#);
        run_test(r#""0xFF".hex"#);
        run_test(r#""-0x7f".hex"#);
        run_test(r#""+0x7f".hex"#);
        run_test(r#""  -0xFF".hex"#);
        run_test(r#""0".hex"#);
        run_test(r#""".hex"#);
        run_test(r#""xyz".hex"#);
        run_test(r#""10".hex"#);
        run_test(r#""CE".hex"#);
        run_test(r#""0xdeadbeef".hex"#);
    }

    #[test]
    fn oct() {
        run_test(r#""77".oct"#);
        run_test(r#""0o77".oct"#);
        run_test(r#""077".oct"#);
        run_test(r#""-077".oct"#);
        run_test(r#""0x1f".oct"#);
        run_test(r#""-0x1f".oct"#);
        run_test(r#""0b1010".oct"#);
        run_test(r#""-0b1010".oct"#);
        run_test(r#""0d99".oct"#);
        run_test(r#""-0d99".oct"#);
        run_test(r#""0".oct"#);
        run_test(r#""".oct"#);
        run_test(r#""xyz".oct"#);
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
    fn capitalize() {
        run_test(r"'hello'.capitalize");
        run_test(r"'HELLO'.capitalize");
        run_test(r"'hELLO'.capitalize");
        run_test(r"s = 'hello'; [s.capitalize!, s]");
        run_test(r"s = 'Hello'; [s.capitalize!, s]");
    }

    #[test]
    fn swapcase() {
        run_test(r"'Hello'.swapcase");
        run_test(r"'hELLO'.swapcase");
        run_test(r"s = 'Hello'; [s.swapcase!, s]");
        run_test(r"s = 'hello'; [s.swapcase!, s]");
    }

    #[test]
    fn casecmp() {
        run_test(r#""abc".casecmp("ABC")"#);
        run_test(r#""abc".casecmp("abd")"#);
        run_test(r#""abc".casecmp?("ABC")"#);
        run_test(r#""abc".casecmp?("abd")"#);
        run_test(r#""abc".casecmp(42)"#);
        run_test(r#""abc".casecmp?(42)"#);
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
        run_test(
            r#####"
        [
            "aa".succ,
            "88".succ.succ,
            "99".succ,
            "９９".succ,
            "z９9".succ,
            "zz".succ,
            "ZZ".succ,
            "ＺＺ".succ,
            "ｚｚ".succ,
            "Ｚｚ".succ,
            "Ｚ＃ｚ".succ,
            "a9".succ,
            "-9".succ,
            "9".succ,
            "09".succ,
            "1.9.9".succ,
            "v9.9.9".succ,
            ".".succ,
            "".succ,
            "AZ".succ,
            "1##9".succ,
            "1&&Z".succ,
            "z&&Z".succ,
            "Ρ".succ,
            "を".succ,
            "9Zz9".succ,
            "###".succ
        ]"#####,
        );
        run_test(r#"12.times.reduce("ン"){|c|c.succ}"#);
    }

    #[test]
    fn pack_unpack() {
        run_test(
            r#"
        [
            "\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack('csl'),
            "\x00\x01\x02\x03\x04\x05\x06".unpack1('q'),
            "\x00\x01\x02\x03\x04\x05\x06".unpack1('Q'),
            "\x00\x01\x02\x03\x04\x05\x06\x07".unpack1('q'),
            "\x00\x01\x02\x03\x04\x05\x06\x07".unpack1('Q'),
            "\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack1('q'),
            "\x00\x01\x02\x03\x04\x05\x06\x07\x08".unpack1('Q'),
            "\x00\x01\x02".unpack1('l'),
            "\x00\x01\x02".unpack1('L'),
            "\x00\x01\x02\x03".unpack1('l'),
            "\x00\x01\x02\x03".unpack1('L'),
            "\x00\x01\x02\x03\x04".unpack1('l'),
            "\x00\x01\x02\x03\x04".unpack1('L'),
            "\x00".unpack1('s'),
            "\x00".unpack1('S'),
            "\x00\x01".unpack1('s'),
            "\x00\x01".unpack1('S'),
            "\x00\x01\x02".unpack1('s'),
            "\x00\x01\x02".unpack1('S'),
            "\x00".unpack1('c'),
            "\x00".unpack1('C'),
            "\x00\x01".unpack1('c'),
            "\x00\x01".unpack1('C'),
            "\x01\xFE".unpack1("c*"),
            "Ruby".unpack("c*"),
            "Ruby".unpack1("c*"),
            "戦闘妖精雪風".unpack("c*"),
            "戦闘妖精雪風".unpack1("c*"),
            "\x01\xFE".unpack1("C*"),
            "Ruby".unpack("C*"),
            "Ruby".unpack1("C*")
        ]"#,
        );

        run_test(
            r#"
        [
            "\x01\xFE".unpack("c*"),
            [1, -2].pack("c*"),
            [1, 254].pack("c*"),
            "\x01\xFE".unpack("C*"),
            [1, -2].pack("C*"),
            [1, 254].pack("C*"),
            "\x01\x02\xFE\xFD".unpack("s*"),
            [513, 65022].pack("s*"),
            [513, -514].pack("s*"),
            "\x01\x02\xFE\xFD".unpack("s*"),
            [258, 65277].pack("s*"),
            [258, -259].pack("s*"),
            [0,1,-1,32767,-32768,65535].pack("n*"),
            "\x00\x00\x00\x01\xFF\xFF\x7F\xFF\x80\x00\xFF\xFF".unpack("n*"),
            [0,1,-1].pack("N*"),
            "\x00\x00\x00\x00\x00\x00\x00\x01\xFF\xFF\xFF\xFF".unpack("N*"),
            [97, 98].pack("CxC"),
            [97, 98].pack("Cx3C"),
            [97, 98].pack("Cx*C"),
            "abc".unpack("CxC"),
            [97, 98, 99].pack("CCXC"),
            "abcdef".unpack("x*XC"),
            "\x01\xFE".unpack("h*"),
            "\x01\xFE".unpack("h3"),
            "\x01\xFE".unpack("H*"),
            "\x01\xFE".unpack("H3"),
            [0,1,65535].pack("v*"),
            "\x01\x00\xFF\x7F\x00\x80".unpack("v*"),
            [0,1,4294967295].pack("V*"),
            "\x01\x00\x00\x00\xFF\xFF\xFF\x7F\x00\x00\x00\x80".unpack("V*"),
            [0x1234].pack("v"),
            [0x12345678].pack("V")
        ]"#,
        );

        run_test_error(r#""abc".unpack("Cx3C")"#);
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
    fn unpack1_offset() {
        run_test(r#""\x01\x02\x03\x04\x05\x06\x07\x08".unpack1("L", offset: 4)"#);
        run_test(r#""\x01\x02\x03\x04\x05\x06\x07\x08".unpack1("L", offset: 0)"#);
        run_test(r#""\x01\x02\x03\x04\x05\x06\x07\x08".unpack("CC", offset: 2)"#);
        run_test(r#""\x01\x02\x03\x04".unpack1("L")"#);
        run_test(r#""\x01\x02".unpack("CC")"#);
        run_test_error(r#""\x01\x02\x03".unpack1("C", offset: 10)"#);
        run_test_error(r#""\x01\x02\x03".unpack("C", offset: -1)"#);
    }

    #[test]
    fn pack_unpack_float() {
        run_test(r#"[1.5].pack("E")"#);
        run_test(r#"[1.5].pack("E").unpack1("E")"#);
        run_test(r#"[1.5, -2.25, 0.0].pack("E*").unpack("E*")"#);
        run_test(r#"[1.5].pack("e")"#);
        run_test(r#"[1.5].pack("e").unpack1("e")"#);
        run_test(r#"[1.5, -2.25].pack("e*").unpack("e*")"#);
        run_test(r#"[1.5].pack("G")"#);
        run_test(r#"[1.5].pack("G").unpack1("G")"#);
        run_test(r#"[1.5].pack("g")"#);
        run_test(r#"[1.5].pack("g").unpack1("g")"#);
        run_test(r#"[3.14].pack("d").unpack1("d")"#);
        run_test(r#"[3.14].pack("D").unpack1("D")"#);
        run_test(r#"[3.14].pack("f").unpack1("f")"#);
        run_test(r#"[3.14].pack("F").unpack1("F")"#);
    }

    #[test]
    fn string_bytesplice() {
        // bytesplice(index, length, str)
        run_test(r#"s = "hello world"; s.bytesplice(5, 1, "---"); s"#);
        run_test(r#"s = "hello"; s.bytesplice(0, 1, "H"); s"#);
        run_test(r#"s = "hello"; s.bytesplice(5, 0, " world"); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(-5, 5, "WORLD"); s"#);
        // bytesplice(index, length, str, str_index, str_length)
        run_test(r#"s = "hello world"; s.bytesplice(0, 5, "HELLO WORLD", 0, 5); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(6, 5, "BEAUTIFUL WORLD", 10, 5); s"#);
        // bytesplice(range, str)
        run_test(r#"s = "hello world"; s.bytesplice(0..4, "HELLO"); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(0...5, "HELLO"); s"#);
        // bytesplice(range, str, str_range)
        run_test(r#"s = "hello world"; s.bytesplice(0..4, "HELLO WORLD", 0..4); s"#);
        // return value is self
        run_test(r#"s = "hello"; r = s.bytesplice(0, 1, "H"); r.equal?(s)"#);
        // negative index
        run_test(r#"s = "hello"; s.bytesplice(-5, 5, "HELLO"); s"#);
        // insert at end
        run_test(r#"s = "hello"; s.bytesplice(5, 0, "!"); s"#);
        // UTF-8 splice at character boundary
        run_test(r#"s = "あいう"; s.bytesplice(0, 3, "X"); s"#);
        run_test(r#"s = "あいう"; s.bytesplice(3, 3, "X"); s"#);
        run_test(r#"s = "あいう"; s.bytesplice(6, 3, "え"); s"#);
        // UTF-8 splice at boundary, replace with ASCII
        run_test(r#"s = "あいう"; s.bytesplice(3, 3, "B"); s"#);
        // UTF-8 + ASCII-8BIT (ascii-only) at char boundary
        run_test(
            r##"
            s = "あいう"
            s.bytesplice(0, 3, "AB".force_encoding("ASCII-8BIT"))
            [s, s.encoding == Encoding::UTF_8]
        "##,
        );
        // ASCII-8BIT string: no boundary check, splice freely
        run_test(
            r##"
            s = "\xe3\x81\x82\xe3\x81\x84".force_encoding("ASCII-8BIT")
            s.bytesplice(1, 2, "AB")
            s.bytes.to_a
        "##,
        );
        // src string UTF-8, valid boundary in src range
        run_test(r#"s = "hello"; s.bytesplice(0, 5, "あいう", 0, 3); s"#);
        // bytesplice(range, str, str_range) with valid boundaries
        run_test(r#"s = "hello"; s.bytesplice(0..4, "あいう", 0..2); s"#);
    }

    #[test]
    fn string_bytesplice_boundary_error() {
        // UTF-8: start offset not on character boundary
        run_test_error(r#"s = "あいう"; s.bytesplice(1, 2, "X")"#);
        // UTF-8: end offset not on character boundary
        run_test_error(r#"s = "あいう"; s.bytesplice(0, 2, "X")"#);
        // UTF-8 range form: start not on boundary
        run_test_error(r#"s = "あいう"; s.bytesplice(1..2, "X")"#);
        // UTF-8 range form: end not on boundary
        run_test_error(r#"s = "あいう"; s.bytesplice(0..1, "X")"#);
        // src string UTF-8: non-boundary in src range (index, length form)
        run_test_error(r#"s = "hello"; s.bytesplice(0, 5, "あいう", 1, 2)"#);
        // src string UTF-8: non-boundary in src range (range form)
        run_test_error(r#"s = "hello"; s.bytesplice(0..4, "あいう", 1..2)"#);
        // wrong number of arguments (1 arg)
        run_test_error(r#""hello".bytesplice("x")"#);
        // wrong number of arguments (4 args)
        run_test_error(r#""hello".bytesplice(0, 1, "x", 0)"#);
        // Range with negative end (exclude_end=false)
        run_test(r#"s = "hello world"; s.bytesplice(0..-6, "HELLO"); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(0..-1, "BYE"); s"#);
        // Range with negative end (exclude_end=true)
        run_test(r#"s = "hello world"; s.bytesplice(0...-6, "HELLO"); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(0...-1, "HELLO"); s"#);
        // Negative end resolves to < start => len=0 (insert)
        run_test(r#"s = "hello"; s.bytesplice(0..-7, "X"); s"#);
        run_test(r#"s = "hello"; s.bytesplice(0...-7, "X"); s"#);
        run_test(r#"s = "hello"; s.bytesplice(0..-100, "X"); s"#);
        // Negative start out of range => RangeError
        run_test_error(r#"s = "hello"; s.bytesplice(-100..-1, "X")"#);
        // bytesplice(range, str, str_range) with negative end in str_range
        run_test(r#"s = "hello world"; s.bytesplice(0..4, "HELLO WORLD", 0..-7); s"#);
        run_test(r#"s = "hello world"; s.bytesplice(0..4, "HELLO WORLD", 0...-7); s"#);
        // str_range with very negative end => len=0
        run_test(r#"s = "hello world"; s.bytesplice(0..4, "ABCDE", 0..-100); s"#);
        // str_range with negative start out of range => RangeError
        run_test_error(r#"s = "hello"; s.bytesplice(0..4, "AB", -100..-1)"#);
    }

    #[test]
    fn string_casecmp_invalid_utf8() {
        run_test(r#""\xc3".casecmp("\xe3")"#);
        run_test(r#""\xc3".casecmp("\xc3")"#);
        run_test(r#""a".casecmp("A")"#);
        run_test(r#""abc".casecmp("ABC")"#);
    }

    #[test]
    fn string_casecmp_p_invalid_utf8() {
        // In monoruby, string literals with non-UTF-8 bytes are automatically
        // assigned ASCII-8BIT encoding, so casecmp? performs byte comparison
        // instead of raising ArgumentError as CRuby does.
        run_test_no_result_check(r#""\xc3".casecmp?("\xc3")"#);
        run_test_no_result_check(r#""\xc3".casecmp?("\xe3")"#);
        run_test(r#""a".casecmp?("A")"#);
        run_test(r#""abc".casecmp?("ABC")"#);
    }

    #[test]
    fn string_casecmp_p_encoding() {
        // Binary vs binary: ASCII-only case-insensitive comparison
        run_test_once(r#""\x41".b.casecmp?("\x61".b)"#);
        // Binary vs binary: non-ASCII bytes are compared as-is
        run_test_once(r#""\xC3".b.casecmp?("\xE3".b)"#);
        // Binary vs UTF-8: incompatible encodings return nil
        run_test_once(r#""\xC3".b.casecmp?("abc")"#);
        // UTF-8 vs UTF-8: Unicode case folding
        run_test(r#""ä".casecmp?("Ä")"#);
    }

    #[test]
    fn string_index_at_end() {
        run_test(r#""blablabla".index("", 9)"#);
        run_test(r#""blablabla".index("", 10)"#);
        run_test(r#""abc".index("", 3)"#);
        run_test(r#""abc".index("", 0)"#);
    }

    #[test]
    fn string_rindex_zero_width() {
        run_test(r#""blablabla".rindex(/.{0}/)"#);
        run_test(r#""blablabla".rindex(/.*/)"#);
        run_test(r#""hello".rindex(/.{0}/)"#);
    }

    #[test]
    fn string_split_invalid_utf8() {
        run_test_error(r#""\xDF".force_encoding("UTF-8").split"#);
        run_test_error(r#""\xDF".force_encoding("UTF-8").split(":")"#);
    }

    #[test]
    fn implicit_to_str_include() {
        run_test(
            r#"
            class MyStr
              def to_str
                "world"
              end
            end
            "hello world".include?(MyStr.new)
            "#,
        );
    }

    #[test]
    fn implicit_to_str_start_with() {
        run_test(
            r#"
            class MyStr
              def to_str
                "hel"
              end
            end
            "hello".start_with?(MyStr.new)
            "#,
        );
    }

    #[test]
    fn implicit_to_str_end_with() {
        run_test(
            r#"
            class MyStr
              def to_str
                "llo"
              end
            end
            "hello".end_with?(MyStr.new)
            "#,
        );
    }

    #[test]
    fn to_int_conversion() {
        run_test(
            r#"
            class MyInt
              def to_int
                3
              end
            end
            "abc" * MyInt.new
            "#,
        );
        run_test(
            r#"
            class MyInt
              def to_int
                10
              end
            end
            "hello".ljust(MyInt.new, ".")
            "#,
        );
        run_test(
            r#"
            class MyInt
              def to_int
                10
              end
            end
            "hello".center(MyInt.new, ".")
            "#,
        );
        run_test(
            r#"
            class MyInt
              def to_int
                1
              end
            end
            "hello".getbyte(MyInt.new)
            "#,
        );
    }

    #[test]
    fn succ_bang() {
        run_test(r#"s = "a"; s.succ!; s"#);
        run_test(r#"s = "az"; s.succ!; s"#);
        run_test(r#"s = "zz"; s.succ!; s"#);
        run_test(r#"s = "9"; s.succ!; s"#);
    }

    #[test]
    fn insert() {
        run_test(r#""hello".insert(0, "X")"#);
        run_test(r#""hello".insert(2, "X")"#);
    }

    #[test]
    fn pack_unpack_a() {
        // pack 'a' — null padded
        run_test(r#"["abc"].pack("a")"#);
        run_test(r#"["abc"].pack("a3")"#);
        run_test(r#"["abc"].pack("a5")"#);
        run_test(r#"["abc"].pack("a*")"#);
        run_test(r#"["a", "b"].pack("a3a3")"#);
        // unpack 'a' — raw bytes
        run_test(r#""abc\0\0".unpack("a3")"#);
        run_test(r#""abc\0\0".unpack("a*")"#);
        run_test(r#""abc\0\0".unpack("a5")"#);
        run_test(r#""abc".unpack("a")"#);
        run_test(r#""abcdef".unpack("a3a3")"#);
    }

    #[test]
    fn pack_unpack_a_upper() {
        // pack 'A' — space padded
        run_test(r#"["abc"].pack("A")"#);
        run_test(r#"["abc"].pack("A3")"#);
        run_test(r#"["abc"].pack("A5")"#);
        run_test(r#"["abc"].pack("A*")"#);
        // unpack 'A' — strips trailing spaces and nulls
        run_test(r#""abc  ".unpack("A5")"#);
        run_test(r#""abc\0\0".unpack("A5")"#);
        run_test(r#""abc  ".unpack("A*")"#);
        run_test(r#""abc".unpack("A")"#);
    }

    #[test]
    fn pack_unpack_z() {
        // pack 'Z' — null-terminated
        run_test(r#"["abc"].pack("Z")"#);
        run_test(r#"["abc"].pack("Z5")"#);
        run_test(r#"["abc"].pack("Z*")"#);
        // unpack 'Z' — stops at null
        run_test(r#""abc\0def".unpack("Z*")"#);
        run_test(r#""abc\0def".unpack("Z3")"#);
        run_test(r#""abc\0def".unpack("Z5")"#);
    }

    #[test]
    fn pack_unpack_m() {
        // Base64
        run_test(r#"["hello"].pack("m")"#);
        run_test(r#"["hello"].pack("m0")"#);
        run_test(r#"["hello"].pack("m").unpack("m")"#);
        run_test(r#"[""].pack("m")"#);
        run_test(r#"["a"].pack("m")"#);
        run_test(r#"["ab"].pack("m")"#);
        run_test(r#"["abc"].pack("m")"#);
    }

    #[test]
    fn pack_unpack_big_m() {
        // MIME quoted-printable
        run_test(r#"["hello"].pack("M")"#);
        run_test(r#"["hello=world"].pack("M")"#);
        run_test(r#"["hello"].pack("M").unpack("M")"#);
    }

    #[test]
    fn pack_unpack_u() {
        // UU encoding
        run_test(r#"["hello"].pack("u")"#);
        run_test(r#"["hello"].pack("u").unpack("u")"#);
        run_test(r#"["abc"].pack("u")"#);
    }

    #[test]
    fn pack_unpack_w() {
        // BER compressed integer
        run_test(r#"[0].pack("w")"#);
        run_test(r#"[127].pack("w")"#);
        run_test(r#"[128].pack("w")"#);
        run_test(r#"[16384].pack("w")"#);
        run_test(r#"[0, 127, 128, 16384].pack("w*")"#);
        run_test(r#"[0, 127, 128, 16384].pack("w*").unpack("w*")"#);
        run_test(r#""\x00\x7f\x81\x00\x81\x80\x00".unpack("w*")"#);
    }

    #[test]
    fn pack_unpack_at_pos() {
        // @ template (AtPos) in pack and unpack
        run_test_no_result_check(
            r#"
            packed = [65].pack("C@3")
            raise "expected 3 bytes" unless packed.bytesize == 3
            raise "expected 65 at pos 0" unless packed.bytes[0] == 65
            "#,
        );
    }

    #[test]
    fn pack_template_comments() {
        // # starts a comment until end of line
        run_test(r#"[65, 66].pack("C # first byte\nC")"#);
    }

    #[test]
    fn pack_template_whitespace() {
        // Whitespace in templates is ignored
        run_test(r#"[65, 66].pack("C C")"#);
    }

    #[test]
    fn pack_pointer_error() {
        // p/P should raise an error
        run_test_error(r#"[1].pack("p")"#);
        run_test_error(r#"[1].pack("P")"#);
    }

    #[test]
    fn pack_unpack_j() {
        // j/J templates (intptr_t / uintptr_t, same as q/Q on x86-64)
        run_test(r#"[42].pack("j").unpack1("j")"#);
        run_test(r#"[42].pack("J").unpack1("J")"#);
        run_test(r#"[-1].pack("j").unpack1("j")"#);
    }

    #[test]
    fn unpack_big_m_soft_line_breaks() {
        // M unpack: =\n is a soft line break (removed)
        run_test_once(r#""hello=\nworld".unpack("M")"#);
        // =\r\n is also a soft line break
        run_test_once(r#""hello=\r\nworld".unpack("M")"#);
        // =\r alone is NOT a soft line break (kept as-is)
        run_test_once(r#""hello=\rworld".unpack("M")"#);
        // =XX hex decoding
        run_test(r#""=41=42=43".unpack("M")"#);
        // Mixed content
        run_test_once(r#""line1=\r\nline2=\nline3".unpack("M")"#);
    }

    #[test]
    fn to_str_coercion() {
        // String.new calls to_str
        run_test(
            r#"
            class Foo; def to_str; "hello"; end; end
            String.new(Foo.new)
            "#,
        );
        // String#replace calls to_str
        run_test(
            r#"
            class Foo; def to_str; "world"; end; end
            s = "hello"
            s.replace(Foo.new)
            s
            "#,
        );
        // Array#join calls to_str on separator
        run_test(
            r#"
            class Foo; def to_str; "-"; end; end
            [1, 2, 3].join(Foo.new)
            "#,
        );
        // String#chomp calls to_str on separator
        run_test(
            r#"
            class Foo; def to_str; "lo"; end; end
            "hello".chomp(Foo.new)
            "#,
        );
        // String#ljust calls to_str on padding
        run_test(
            r#"
            class Foo; def to_str; "*"; end; end
            "hi".ljust(10, Foo.new)
            "#,
        );
        // String#center calls to_str on padding
        run_test(
            r#"
            class Foo; def to_str; "-"; end; end
            "hi".center(10, Foo.new)
            "#,
        );
    }

    #[test]
    fn to_int_coercion() {
        // Float#ceil calls to_int on ndigits
        run_test(
            r#"
            class Foo; def to_int; 2; end; end
            3.14159.ceil(Foo.new)
            "#,
        );
        // Float#round calls to_int on ndigits
        run_test(
            r#"
            class Foo; def to_int; 1; end; end
            3.14159.round(Foo.new)
            "#,
        );
        // Float#truncate calls to_int on ndigits
        run_test(
            r#"
            class Foo; def to_int; 2; end; end
            3.14159.truncate(Foo.new)
            "#,
        );
    }

    #[test]
    fn string_implicit_conversions() {
        // String#<=> with to_str
        run_test_with_prelude(
            r#""hello" <=> o"#,
            r#"class C; def to_str; "hello"; end; end; o = C.new"#,
        );
        run_test_with_prelude(
            r#""abc" <=> o"#,
            r#"class C; def to_str; "def"; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_shl_to_str_coercion() {
        // String#<< should call to_str on non-String arguments
        run_test_with_prelude(
            r#"s = "hello"; s << o; s"#,
            r#"class C; def to_str; " world"; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_split_to_str_coercion() {
        // String#split should call to_str on separator argument
        run_test_with_prelude(
            r#""a,b,c".split(o)"#,
            r#"class C; def to_str; ","; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_sub_gsub_to_str_coercion() {
        // String#sub and String#gsub should call to_str on pattern argument
        run_test_with_prelude(
            r#""abcde".sub(o, "XX")"#,
            r#"class C; def to_str; "bc"; end; end; o = C.new"#,
        );
        run_test_with_prelude(
            r#""abcbc".gsub(o, "XX")"#,
            r#"class C; def to_str; "bc"; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_index_with_string() {
        // String#[] with a String argument should return substring or nil
        run_test(r#""abcde"["bc"]"#);
        run_test(r#""abcde"["xy"]"#);
    }

    #[test]
    fn string_index_to_int_coercion() {
        // String#[] should call to_int on non-integer arguments
        run_test_with_prelude(
            r#""abcde"[o]"#,
            r#"class C; def to_int; 2; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_scan_to_str_coercion() {
        // String#scan should call to_str on pattern argument
        run_test_with_prelude(
            r#""abcabc".scan(o)"#,
            r#"class C; def to_str; "bc"; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_each_line_to_str_coercion() {
        // String#each_line should call to_str on separator argument
        run_test_with_prelude(
            r#"r = []; "a,b,c".each_line(o) { |l| r << l }; r"#,
            r#"class C; def to_str; ","; end; end; o = C.new"#,
        );
    }

    #[test]
    fn string_byteindex() {
        run_test(r#""hello".byteindex("ll")"#);
        run_test(r#""hello".byteindex("ll", 3)"#);
        run_test(r#""hello".byteindex("o")"#);
        run_test(r#""hello".byteindex("h")"#);
        run_test(r#""hello".byteindex("x")"#);
        run_test(r#""hello".byteindex("lo", -3)"#);
        // Multibyte: offset on char boundary should work
        run_test(r#""わたし".byteindex("た")"#);
        // Multibyte: offset inside a multibyte char should raise ArgumentError
        run_test_error(r#""わたし".byteindex(/た/, 1)"#);
    }

    #[test]
    fn string_to_c() {
        run_test(r#""1+2i".to_c.inspect"#);
        run_test(r#""3".to_c.inspect"#);
        run_test(r#""i".to_c.inspect"#);
        run_test(r#""".to_c.inspect"#);
        run_test(r#""-3+4i".to_c.inspect"#);
    }

    #[test]
    fn string_to_r() {
        run_test(r#""1/3".to_r.inspect"#);
        run_test(r#""0.5".to_r.inspect"#);
        run_test(r#""1".to_r.inspect"#);
        run_test(r#""".to_r.inspect"#);
    }

    #[test]
    fn slice_bang_with_string() {
        run_test(r#"s = "hello world"; s.slice!("world"); s"#);
        run_test(r#"s = "hello"; s.slice!("xyz")"#);
        run_test(r#"s = "abcabc"; s.slice!("bc"); s"#);
    }

    #[test]
    fn slice_bang_with_to_int() {
        run_test(
            r#"
            class MyIdx; def to_int; 1; end; end
            s = "hello"
            s.slice!(MyIdx.new)
            "#,
        );
    }

    #[test]
    fn string_mul_overflow() {
        run_test_no_result_check(
            r#"
            begin
              "abc" * (2**62)
            rescue ArgumentError => e
              raise "wrong error" unless e.message.include?("too big")
            end
            "#,
        );
        run_test(r#""" * 1000000"#);
        run_test(r#""ab" * 3"#);
    }
}
