use super::*;
use jitgen::JitContext;
use num::{BigInt, ToPrimitive, Zero};
use std::ops::{BitAnd, BitOr, BitXor};

//
// Integer class
//

pub(super) fn init(globals: &mut Globals, numeric: Module) {
    globals.define_builtin_class("Integer", INTEGER_CLASS, numeric, OBJECT_CLASS, None);
    globals.store[INTEGER_CLASS].clear_alloc_func();
    globals.define_builtin_func_with(INTEGER_CLASS, "chr", chr, 0, 1, false);
    //globals.define_builtin_inline_func(INTEGER_CLASS, "succ", succ, Box::new(integer_succ), 0);
    //globals.define_builtin_func(INTEGER_CLASS, "times", times, 0);
    //globals.define_builtin_func_with(INTEGER_CLASS, "step", step, 1, 2, false);
    globals.define_builtin_func(INTEGER_CLASS, "upto", upto, 1);
    globals.define_builtin_func(INTEGER_CLASS, "downto", downto, 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "to_f", to_f, Box::new(integer_tof), 0);
    globals.define_basic_op(INTEGER_CLASS, "+", add, 1);
    globals.define_basic_op(INTEGER_CLASS, "-", sub, 1);
    globals.define_basic_op(INTEGER_CLASS, "*", mul, 1);
    globals.define_basic_op(INTEGER_CLASS, "/", div, 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "%", int_rem, Box::new(integer_rem), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "**", int_pow, Box::new(integer_pow), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "&", bitand, Box::new(integer_bitand), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "|", bitor, Box::new(integer_bitor), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "^", bitxor, Box::new(integer_bitxor), 1);
    globals.define_builtin_func(INTEGER_CLASS, "divmod", divmod, 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, ">>", shr, Box::new(integer_shr), 1);
    globals.define_builtin_inline_func(INTEGER_CLASS, "<<", shl, Box::new(integer_shl), 1);
    globals.define_builtin_func(INTEGER_CLASS, "==", eq, 1);
    globals.define_builtin_func(INTEGER_CLASS, "===", eq, 1);
    globals.define_builtin_func(INTEGER_CLASS, ">=", ge, 1);
    globals.define_builtin_func(INTEGER_CLASS, ">", gt, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<=", le, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<", lt, 1);
    globals.define_basic_op(INTEGER_CLASS, "!=", ne, 1);
    globals.define_builtin_func(INTEGER_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func_with(INTEGER_CLASS, "[]", index, 1, 2, false);
    globals.define_builtin_func(INTEGER_CLASS, "even?", even_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "odd?", odd_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "nonzero?", nonzero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "zero?", zero_, 0);
    globals.define_builtin_func(INTEGER_CLASS, "size", size, 0);
    globals.define_builtin_func(INTEGER_CLASS, "bit_length", bit_length, 0);
    globals.define_builtin_func_with(INTEGER_CLASS, "to_s", to_s, 0, 1, false);
    globals.define_builtin_func_with(INTEGER_CLASS, "inspect", to_s, 0, 1, false);
    globals.define_builtin_func(INTEGER_CLASS, "eql?", eql_, 1);
    globals.define_builtin_func(INTEGER_CLASS, "abs", abs, 0);
    globals.define_builtin_func(INTEGER_CLASS, "magnitude", abs, 0);
    globals.define_builtin_func_with(INTEGER_CLASS, "pow", pow, 1, 2, false);
    globals.define_builtin_func_with(INTEGER_CLASS, "floor", int_floor, 0, 1, false);
    globals.define_builtin_func_with(INTEGER_CLASS, "ceil", int_ceil, 0, 1, false);
    globals.define_builtin_func_with(INTEGER_CLASS, "truncate", int_truncate, 0, 1, false);
    globals.define_builtin_func_with_kw(
        INTEGER_CLASS,
        "round",
        int_round,
        0,
        1,
        false,
        &["half"],
        false,
    );
}

/*///
/// ### Integer#times
///
/// - times {|n| ... } -> self
/// - [TODO] times -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/times.html]
#[monoruby_builtin]
fn times(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => vm.invoke_block_iter1(globals, bh, (0..i).map(Value::integer))?,
        RV::BigInt(_) => unimplemented!(),
        _ => unreachable!(),
    };

    Ok(lfp.self_val())
}*/

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

/*///
/// ### Integer#step
///
/// - step(limit, step = 1) {|n| ... } -> self
/// - step(limit, step = 1) -> Enumerator
/// - [NOT SUPPORTED] step(limit, step = 1) -> Enumerator::ArithmeticSequence
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/step.html]
#[monoruby_builtin]
fn step(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("step");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = lfp.arg(0).coerce_to_int(vm, globals)?;
    let step = if let Some(arg1) = lfp.try_arg(1) {
        let step = arg1.coerce_to_int(vm, globals)?;
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
}*/

///
/// ### Integer#upto
///
/// - upto(max) {|n| ... } -> Integer
/// - upto(max) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/upto.html]
#[monoruby_builtin]
fn upto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = if let Some(f) = lfp.arg(0).try_float() {
        if f.is_nan() {
            return Ok(lfp.self_val());
        }
        f.floor() as i64
    } else {
        match lfp.arg(0).coerce_to_int_i64(vm, globals) {
            Ok(v) => v,
            Err(_) => return Err(MonorubyErr::argumenterr(format!("bad value for range"))),
        }
    };
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("upto");
            // Size hint: `stop - start + 1` if `stop >= start`, else 0.
            let size = if limit >= cur {
                Value::integer(limit - cur + 1)
            } else {
                Value::integer(0)
            };
            return vm.generate_enumerator_with_size(
                id,
                lfp.self_val(),
                lfp.iter().collect(),
                pc,
                Some(size),
            );
        }
        Some(block) => block,
    };
    if cur > limit {
        return Ok(lfp.self_val());
    }

    let iter = PosStep {
        cur,
        limit,
        step: 1,
    };
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Integer#downto
///
/// - downto(min) {|n| ... } -> self
/// - downto(min) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/downto.html]
#[monoruby_builtin]
fn downto(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let cur = lfp.self_val().expect_integer(globals)?;
    let limit = if let Some(f) = lfp.arg(0).try_float() {
        if f.is_nan() {
            return Ok(lfp.self_val());
        }
        f.ceil() as i64
    } else {
        match lfp.arg(0).coerce_to_int_i64(vm, globals) {
            Ok(v) => v,
            Err(_) => return Err(MonorubyErr::argumenterr(format!("bad value for range"))),
        }
    };
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("downto");
            // Size hint: `start - stop + 1` if `start >= stop`, else 0.
            let size = if cur >= limit {
                Value::integer(cur - limit + 1)
            } else {
                Value::integer(0)
            };
            return vm.generate_enumerator_with_size(
                id,
                lfp.self_val(),
                lfp.iter().collect(),
                pc,
                Some(size),
            );
        }
        Some(block) => block,
    };
    if cur < limit {
        return Ok(lfp.self_val());
    }

    let iter = NegStep {
        cur,
        limit,
        step: -1,
    };
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

/// ### Integer#chr
/// - chr -> String
/// - chr(encoding) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/chr.html]
#[monoruby_builtin]
fn chr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let i = match lfp.self_val().try_fixnum() {
        Some(i) => i,
        None => {
            return Err(MonorubyErr::rangeerr("bignum out of char range"));
        }
    };

    // Resolve encoding name: explicit argument, or Encoding.default_internal, or none
    let enc_name: Option<String> = if let Some(enc_val) = lfp.try_arg(0) {
        Some(chr_resolve_encoding_name(vm, globals, enc_val)?)
    } else {
        // No argument: check Encoding.default_internal for codepoints > 255
        if i > 255 {
            let di = globals
                .get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))
                .unwrap_or(Value::nil());
            if !di.is_nil() {
                if let Some(name) = globals.store.get_ivar(di, IdentId::_ENCODING) {
                    Some(name.as_str().to_string())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    };

    chr_with_encoding(globals, i, enc_name.as_deref())
}

/// Resolve an encoding argument (String or Encoding object) to an encoding name.
fn chr_resolve_encoding_name(
    vm: &mut Executor,
    globals: &mut Globals,
    enc_val: Value,
) -> Result<String> {
    if let Some(s) = enc_val.is_str() {
        // Normalize via Encoding.try_from_str to validate, then use the original name
        let _ = crate::value::rvalue::Encoding::try_from_str(s)?;
        Ok(s.to_string())
    } else if let Some(name) = globals.store.get_ivar(enc_val, IdentId::_ENCODING) {
        Ok(name.as_str().to_string())
    } else {
        let s = enc_val.coerce_to_string(vm, globals)?;
        let _ = crate::value::rvalue::Encoding::try_from_str(&s)?;
        Ok(s)
    }
}

/// Produce a chr String for codepoint `i` with optional encoding name.
fn chr_with_encoding(globals: &mut Globals, i: i64, enc_name: Option<&str>) -> Result<Value> {
    use crate::value::rvalue::{Encoding, RStringInner};

    let normalized = enc_name.map(|s| s.to_uppercase().replace('-', "_"));
    match normalized.as_deref() {
        Some("US_ASCII") | Some("ASCII") => {
            if i < 0 || i > 0x7F {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            let inner = RStringInner::from_encoding(&[i as u8], Encoding::UsAscii);
            Ok(Value::string_from_inner(inner))
        }
        Some("UTF_8") | Some("UTF8") => {
            if i < 0 || i > 0x10FFFF {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            match char::from_u32(i as u32) {
                Some(c) => {
                    let mut buf = [0u8; 4];
                    let s = c.encode_utf8(&mut buf);
                    Ok(Value::string_from_str(s))
                }
                None => Err(MonorubyErr::rangeerr(format!("{} out of char range", i))),
            }
        }
        Some("CESU_8") | Some("CESU8") => {
            // CESU-8: U+0000..U+FFFF same as UTF-8, U+10000..U+10FFFF as surrogate pairs
            if i < 0 || i > 0x10FFFF {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            let cp = i as u32;
            let bytes = if cp <= 0xFFFF {
                // Same as UTF-8 for BMP
                match char::from_u32(cp) {
                    Some(c) => {
                        let mut buf = [0u8; 4];
                        let s = c.encode_utf8(&mut buf);
                        s.as_bytes().to_vec()
                    }
                    None => return Err(MonorubyErr::rangeerr(format!("{} out of char range", i))),
                }
            } else {
                // Supplementary: encode as surrogate pair in CESU-8
                let cp = cp - 0x10000;
                let hi = 0xD800 + (cp >> 10);
                let lo = 0xDC00 + (cp & 0x3FF);
                let mut bytes = Vec::with_capacity(6);
                // Encode each surrogate as 3-byte CESU-8
                for surrogate in [hi, lo] {
                    bytes.push(0xE0 | ((surrogate >> 12) & 0x0F) as u8);
                    bytes.push(0x80 | ((surrogate >> 6) & 0x3F) as u8);
                    bytes.push(0x80 | (surrogate & 0x3F) as u8);
                }
                bytes
            };
            let inner = RStringInner::from_encoding(&bytes, Encoding::Ascii8);
            let val = Value::string_from_inner(inner);
            chr_set_encoding_label(globals, val, enc_name.unwrap());
            Ok(val)
        }
        Some("ASCII_8BIT") | Some("BINARY") => {
            if i < 0 || i > 0xFF {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            Ok(Value::bytes_from_slice(&[i as u8]))
        }
        Some(norm) => {
            // Mock encoding: encode codepoint as big-endian bytes,
            // store with the requested encoding name on the String object.
            if i < 0 {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            let cp = i as u64;
            // Validate codepoint for the specific encoding (mock)
            if !chr_valid_mock_codepoint(norm, cp) {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            }
            let bytes: Vec<u8> = if cp <= 0xFF {
                vec![cp as u8]
            } else if cp <= 0xFFFF {
                vec![(cp >> 8) as u8, (cp & 0xFF) as u8]
            } else if cp <= 0xFFFFFF {
                vec![
                    (cp >> 16) as u8,
                    ((cp >> 8) & 0xFF) as u8,
                    (cp & 0xFF) as u8,
                ]
            } else {
                return Err(MonorubyErr::rangeerr(format!("{} out of char range", i)));
            };
            // Create a binary string with the requested encoding label
            let enc_label = enc_name.unwrap();
            let inner = RStringInner::from_encoding(&bytes, Encoding::Ascii8);
            let val = Value::string_from_inner(inner);
            // Set the encoding name on the string so .encoding returns the right object
            chr_set_encoding_label(globals, val, enc_label);
            Ok(val)
        }
        None => {
            // No encoding specified, no default_internal
            if let Ok(b) = u8::try_from(i) {
                if b <= 0x7f {
                    let inner = RStringInner::from_encoding(&[b], Encoding::UsAscii);
                    return Ok(Value::string_from_inner(inner));
                }
                return Ok(Value::bytes_from_slice(&[b]));
            }
            Err(MonorubyErr::char_out_of_range(
                &globals.store,
                Value::integer(i),
            ))
        }
    }
}

/// Validate whether a codepoint is valid for a mock (non-UTF-8/ASCII/Binary) encoding.
fn chr_valid_mock_codepoint(normalized_enc: &str, cp: u64) -> bool {
    match normalized_enc {
        "SHIFT_JIS" | "SJIS" | "WINDOWS_31J" | "CP932" | "CSWINDOWS31J" | "MACJAPANESE"
        | "MACJAPAN" => {
            if cp <= 0x7F {
                return true;
            }
            // Single-byte kana: 0xA1-0xDF
            if (0xA1..=0xDF).contains(&cp) {
                return true;
            }
            // Double-byte: high byte 0x81-0x9F or 0xE0-0xFC, low byte 0x40-0x7E or 0x80-0xFC
            if cp > 0xFF && cp <= 0xFFFF {
                let hi = (cp >> 8) as u8;
                let lo = (cp & 0xFF) as u8;
                let hi_ok = (0x81..=0x9F).contains(&hi) || (0xE0..=0xFC).contains(&hi);
                let lo_ok = (0x40..=0x7E).contains(&lo) || (0x80..=0xFC).contains(&lo);
                return hi_ok && lo_ok;
            }
            false
        }
        "EUC_JP" | "EUCJP" | "EUCJP_MS" | "EUCJP_WIN" | "CP51932" => {
            if cp <= 0x7F {
                return true;
            }
            // EUC-JP double-byte: both bytes in 0xA1-0xFE
            if cp > 0xFF && cp <= 0xFFFF {
                let hi = (cp >> 8) as u8;
                let lo = (cp & 0xFF) as u8;
                return (0xA1..=0xFE).contains(&hi) && (0xA1..=0xFE).contains(&lo);
            }
            false
        }
        _ => {
            // Other mock encodings: only single-byte (0-255)
            cp <= 0xFF
        }
    }
}

/// Set the encoding label on a string value by finding the Encoding constant.
fn chr_set_encoding_label(globals: &mut Globals, val: Value, enc_name: &str) {
    let enc_class_id = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
        .map(|v| v.as_class_id());
    if let Some(class_id) = enc_class_id {
        let const_name = enc_name.replace('-', "_");
        if let Some(enc_obj) = globals
            .store
            .get_constant_noautoload(class_id, IdentId::get_id(&const_name))
        {
            let override_id = IdentId::get_id("/encoding_override");
            globals.store.set_ivar(val, override_id, enc_obj).ok();
        }
    }
}

// Integer#succ is defined in Ruby (integer.rb).
// The Rust implementation and JIT inline specialization are commented out
// because the Ruby definition takes precedence at runtime.
/*
#[monoruby_builtin]
fn succ(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match lfp.self_val().unpack() {
        RV::Fixnum(i) => i
            .checked_add(1)
            .map(Value::integer)
            .unwrap_or_else(|| Value::bigint(BigInt::from(i) + 1)),
        RV::BigInt(b) => Value::bigint(b + 1),
        _ => unimplemented!(),
    })
}

fn integer_succ(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { dst, recv, .. } = *callsite;
    if let Some(i) = state.is_fixnum_literal(recv)
        && Value::is_i63(i.get() + 1)
    {
        state.def_C(dst, Immediate::check_fixnum(i.get() + 1).unwrap());
        return true;
    }

    let deopt = ir.new_deopt(state);
    state.load(ir, recv, GP::Rdi);
    ir.inline(move |r#gen, _, labels, _| {
        let deopt = &labels[deopt];
        monoasm! { &mut r#gen.jit,
            addq  rdi, 2;
            jo    deopt;
        }
    });
    if let Some(dst) = dst {
        state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    }
    true
}
*/

#[monoruby_builtin]
fn to_f(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let f = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => unimplemented!(),
    };
    Ok(Value::float(f))
}

fn integer_tof(
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
    let CallSiteInfo { dst, recv, .. } = *callsite;
    if let Some(ret) = dst {
        // Load `recv` BEFORE allocating `ret`'s xmm. When `ret == recv`
        // (a common slot-reuse pattern, e.g. `496.to_f` or chained
        // `v.x.to_f`), `def_F(ir, ret)` discards `recv`'s existing
        // binding and sets the slot to `F(fresh_xmm)`. A subsequent
        // `state.load(ir, recv, ..)` then sees the slot in `F` mode
        // and emits an `xmm2stack` write-back of the *uninitialised*
        // fresh xmm — so `rdi` ends up holding boxed-NaN garbage, the
        // `sarq` / `cvtsi2sdq` below produce NaN, and the caller sees
        // `496.to_f #=> NaN`.
        state.load(ir, recv, GP::Rdi);
        let fret = state.def_F(ir, ret);
        ir.inline(move |r#gen, _, _, base| {
            // Convert into xmm0, then store into fret (pool or spill).
            monoasm! { &mut r#gen.jit,
                sarq  rdi, 1;
                cvtsi2sdq xmm0, rdi;
            }
            r#gen.store_xmm0_into_xmm(fret, base);
        });
    }
    true
}

///
/// ### Integer#to_i
/// ### Integer#eql?
///
/// - eql?(other) -> bool
///
/// Returns true if other is an Integer with the same value.
#[monoruby_builtin]
fn eql_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    if lhs.id() == rhs.id() {
        return Ok(Value::bool(true));
    }
    let res = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(a), RV::Fixnum(b)) => a == b,
        (RV::BigInt(a), RV::BigInt(b)) => a == b,
        (RV::Fixnum(_), RV::BigInt(_)) | (RV::BigInt(_), RV::Fixnum(_)) => false,
        _ => false,
    };
    Ok(Value::bool(res))
}

// Bitwise operations.

macro_rules! binop {
    ($op:ident, $op_str:expr) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
                let lhs = lfp.self_val();
                let rhs = lfp.arg(0);
                match (lhs.unpack(), rhs.unpack()) {
                    (RV::Fixnum(lhs), RV::Fixnum(rhs)) => Ok(Value::integer(lhs.$op(rhs))),
                    (RV::Fixnum(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(BigInt::from(lhs).$op(rhs))),
                    (RV::BigInt(lhs), RV::Fixnum(rhs)) => Ok(Value::bigint(lhs.$op(BigInt::from(rhs)))),
                    (RV::BigInt(lhs), RV::BigInt(rhs)) => Ok(Value::bigint(lhs.$op(rhs))),
                    _ => {
                        // CRuby-compatible: call coerce, check result is Integer
                        let op_id = IdentId::get_id($op_str);
                        match crate::executor::op::binary_ops::try_coerce_and_apply_bit(vm, globals, op_id, lhs, rhs) {
                            Some(v) => Ok(v),
                            None => Err(vm.take_error()),
                        }
                    }
                }
            }
        }
    };
    (($op1:ident, $op_str1:expr), $(($op2:ident, $op_str2:expr)),+) => {
        binop!($op1, $op_str1);
        binop!($(($op2, $op_str2)),+);
    };
    (($op1:ident, $op_str1:expr)) => {
        binop!($op1, $op_str1);
    };
}

binop!((bitand, "&"), (bitor, "|"), (bitxor, "^"));

// Compare operations.

///
/// ### Integer#==
///
/// - self == other -> bool
/// - self === other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let b = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(l), RV::Fixnum(r)) => l == r,
        (RV::Fixnum(l), RV::BigInt(r)) => BigInt::from(l) == *r,
        (RV::Fixnum(l), RV::Float(r)) => (l as f64) == r,
        (RV::BigInt(l), RV::Fixnum(r)) => *l == BigInt::from(r),
        (RV::BigInt(l), RV::BigInt(r)) => l == r,
        (RV::BigInt(l), RV::Float(r)) => {
            super::super::op::bigint_cmp_float(l, r) == Some(std::cmp::Ordering::Equal)
        }
        _ => {
            // Reverse dispatch: try rhs == lhs
            let eq_id = IdentId::get_id("==");
            let result = vm.invoke_method_inner(globals, eq_id, rhs, &[lhs], None, None)?;
            return Ok(Value::bool(result.as_bool()));
        }
    };
    Ok(Value::bool(b))
}

///
/// ### Integer#!=
///
/// - self != other -> bool
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = vm.ne_values_bool(globals, lfp.self_val(), lfp.arg(0))?;
    Ok(Value::bool(b))
}

///
/// ### Integer#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    let ord = match (lhs.unpack(), rhs.unpack()) {
        (RV::Fixnum(lhs), RV::Fixnum(rhs)) => lhs.cmp(&rhs),
        (RV::Fixnum(lhs), RV::BigInt(rhs)) => BigInt::from(lhs).cmp(rhs),
        (RV::Fixnum(lhs), RV::Float(rhs)) => match (lhs as f64).partial_cmp(&rhs) {
            Some(ord) => ord,
            None => {
                return Ok(Value::nil());
            }
        },
        (RV::BigInt(lhs), RV::Fixnum(rhs)) => lhs.cmp(&BigInt::from(rhs)),
        (RV::BigInt(lhs), RV::BigInt(rhs)) => lhs.cmp(rhs),
        (RV::BigInt(lhs), RV::Float(rhs)) => match super::super::op::bigint_cmp_float(lhs, rhs) {
            Some(ord) => ord,
            None => return Ok(Value::nil()),
        },
        _ => {
            // Try coerce protocol: call rhs.coerce(lhs), propagate exceptions
            let coerce_id = IdentId::get_id("coerce");
            match vm.invoke_method_if_exists(globals, coerce_id, rhs, &[lhs], None, None) {
                Ok(Some(result)) => {
                    if let Some(ary) = result.try_array_ty() {
                        if ary.len() == 2 {
                            let cmp_id = IdentId::get_id("<=>");
                            return vm.invoke_method_inner(
                                globals,
                                cmp_id,
                                ary[0],
                                &[ary[1]],
                                None,
                                None,
                            );
                        }
                    }
                    return Ok(Value::nil());
                }
                Ok(None) => return Ok(Value::nil()),
                Err(e) => return Err(e), // Propagate exception from coerce
            }
        }
    };
    Ok(Value::from_ord(ord))
}

macro_rules! cmpop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
                let lhs = lfp.self_val();
                let rhs = lfp.arg(0);
                match crate::executor::op::[<cmp_ $op _values>](vm, globals, lhs, rhs) {
                    Some(res) => Ok(res),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmpop!($op1);
        cmpop!($($op2),+);
    };
}

cmpop!(ge, gt, le, lt);

///
/// ### Integer#>>
///
/// - self >> bits -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3e=3e.html]
#[monoruby_builtin]
fn shr(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::shr_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

/// Constant-fold `lhs >> rhs` for two i63 fixnums.
///
/// Returns `Some(result)` if the result fits in an i63 fixnum and matches
/// `safe_int_shr` / `safe_int_shl` semantics. Returns `None` if folding
/// would require BigInt allocation or would error at runtime.
fn fold_shr(lhs: i64, rhs: i64) -> Option<i64> {
    if rhs >= 0 {
        // right shift never grows; always fits in i63.
        let r = if rhs >= 64 {
            if lhs >= 0 { 0 } else { -1 }
        } else {
            lhs >> rhs
        };
        Some(r)
    } else {
        // n >> -k == n << k
        let k = (-rhs) as u64;
        fold_shl_pos(lhs, k)
    }
}

/// Constant-fold `lhs << rhs` for two i63 fixnums.
///
/// Returns `Some(result)` if the result fits in an i63 fixnum. Returns
/// `None` if the result would overflow into a BigInt or if `rhs` is too
/// large for a runtime-safe shift (matching `safe_int_shl` semantics).
fn fold_shl(lhs: i64, rhs: i64) -> Option<i64> {
    if rhs >= 0 {
        let k = rhs as u64;
        fold_shl_pos(lhs, k)
    } else {
        // n << -k == n >> k; right shift always fits.
        let k = (-rhs) as u64;
        let r = if k >= 64 {
            if lhs >= 0 { 0 } else { -1 }
        } else {
            lhs >> k
        };
        Some(r)
    }
}

/// Helper: fold `lhs << k` (k >= 0). Returns `None` if the result would
/// overflow i63 (and thus require BigInt) or fail at runtime.
fn fold_shl_pos(lhs: i64, k: u64) -> Option<i64> {
    if lhs == 0 {
        return Some(0);
    }
    if k >= 63 {
        // Would overflow i63 for any non-zero lhs, or be a runtime error
        // when k > u32::MAX (handled by safe_int_shl as RangeError).
        return None;
    }
    let result = lhs.checked_shl(k as u32)?;
    if Value::is_i63(result) {
        Some(result)
    } else {
        None
    }
}

fn integer_shr(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    if rhs_class != Some(INTEGER_CLASS) {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    // Constant folding: both operands are concrete fixnums.
    if let Some(lhs) = state.is_fixnum_literal(recv)
        && let Some(rhs) = state.is_fixnum_literal(args)
        && let Some(folded) = fold_shr(lhs.get(), rhs.get())
    {
        state.def_C(dst, Immediate::check_fixnum(folded).unwrap());
        return true;
    }

    state.load(ir, recv, GP::Rdi);
    if let Some(rhs) = state.is_fixnum_literal(args) {
        let rhs = rhs.get();
        if rhs >= 0 {
            // n >> k (k >= 0): right shift
            ir.inline(move |r#gen, _, _, _| r#gen.gen_shr_imm(rhs.min(64) as u8));
        } else {
            // n >> -k: equivalent to n << k
            let k = (-rhs) as u64;
            if k < 64 {
                let deopt = ir.new_deopt(state);
                ir.inline(move |r#gen, _, labels, _| r#gen.gen_shl_rhs_imm(k as u8, &labels[deopt]));
            } else {
                // shift too large for inline, deopt
                let deopt = ir.new_deopt(state);
                ir.inline(move |r#gen, _, labels, _| {
                    let deopt = &labels[deopt];
                    monoasm!( &mut r#gen.jit,
                        cmpq rdi, (Value::i32(0).id());
                        jne deopt;
                        movq rdi, (Value::i32(0).id());
                    );
                });
            }
        }
    } else {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels, _| r#gen.gen_shr(&labels[deopt]));
    }
    state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    true
}

///
/// ### Integer#<<
///
/// - self << bits -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::shl_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_shl(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    if rhs_class != Some(INTEGER_CLASS) {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    // Constant folding: both operands are concrete fixnums.
    if let Some(lhs) = state.is_fixnum_literal(recv)
        && let Some(rhs) = state.is_fixnum_literal(args)
        && let Some(folded) = fold_shl(lhs.get(), rhs.get())
    {
        state.def_C(dst, Immediate::check_fixnum(folded).unwrap());
        return true;
    }

    state.load(ir, recv, GP::Rdi);
    if let Some(rhs) = state.is_fixnum_literal(args) {
        let rhs = rhs.get();
        if rhs >= 0 && rhs < 64 {
            // n << k (0 <= k < 64): left shift
            let deopt = ir.new_deopt(state);
            ir.inline(move |r#gen, _, labels, _| r#gen.gen_shl_rhs_imm(rhs as u8, &labels[deopt]));
        } else if rhs < 0 {
            // n << -k: equivalent to n >> k
            let k = (-rhs) as u64;
            ir.inline(move |r#gen, _, _, _| r#gen.gen_shr_imm(k.min(64) as u8));
        } else {
            // rhs >= 64: deopt (overflow for non-zero lhs)
            let deopt = ir.new_deopt(state);
            ir.inline(move |r#gen, _, labels, _| {
                let deopt = &labels[deopt];
                monoasm!( &mut r#gen.jit,
                    cmpq rdi, (Value::i32(0).id());
                    jne deopt;
                    movq rdi, (Value::i32(0).id());
                );
            });
        }
    } else if let Some(lhs) = state.is_fixnum_literal(recv) {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels, _| r#gen.gen_shl_lhs_imm(lhs.get(), &labels[deopt]));
    } else {
        state.load_fixnum(ir, args, GP::Rcx);
        let deopt = ir.new_deopt(state);
        ir.inline(move |r#gen, _, labels, _| r#gen.gen_shl(&labels[deopt]));
    }
    state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    true
}

///
/// ### Integer#%
///
/// - self % other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=25.html]
#[monoruby_builtin]
fn int_rem(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::rem_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_rem(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    match rhs_class {
        Some(INTEGER_CLASS) => integer_rem_int_rhs(state, ir, dst, recv, args),
        Some(FLOAT_CLASS) => integer_rem_float_rhs(state, ir, dst, recv, args),
        _ => false,
    }
}

fn integer_rem_int_rhs(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    dst: Option<SlotId>,
    recv: SlotId,
    args: SlotId,
) -> bool {
    // Constant folding: both operands are concrete fixnums and rhs != 0.
    // ruby_mod of two i63 values always fits in i63.
    if let Some(lhs) = state.is_fixnum_literal(recv)
        && let Some(rhs) = state.is_fixnum_literal(args)
        && !rhs.get().is_zero()
    {
        let result = lhs.get().ruby_mod(&rhs.get());
        state.def_C(dst, Immediate::check_fixnum(result).unwrap());
        return true;
    }

    // Power-of-two RI optimization: lhs % positive_power_of_2
    // → lhs & mask, where mask = (rhs - 1) << 1 | 1 = rhs * 2 - 1
    // applied directly on the tagged fixnum (the tag bit is preserved
    // since both operands have LSB = 1). The bitwise AND matches Ruby's
    // floor-mod semantics for any sign of lhs as long as the divisor
    // is a positive power of 2 (two's-complement makes this work).
    if let Some(rhs) = state.is_fixnum_literal(args) {
        let rhs_val = rhs.get();
        if rhs_val > 0 && (rhs_val as u64).is_power_of_two() {
            state.load_fixnum(ir, recv, GP::Rdi);
            let mask = rhs_val * 2 - 1;
            ir.inline(move |r#gen, _, _, _| {
                if let Ok(imm32) = i32::try_from(mask) {
                    let imm = imm32 as i64;
                    monoasm!( &mut r#gen.jit,
                        andq rdi, (imm);
                    );
                } else {
                    monoasm!( &mut r#gen.jit,
                        movq rax, (mask);
                        andq rdi, rax;
                    );
                }
            });
            state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
            return true;
        }
    }

    state.load_fixnum(ir, recv, GP::Rdi);
    state.load_fixnum(ir, args, GP::Rsi);
    let deopt = ir.new_deopt(state);
    ir.inline(move |r#gen, _, labels, _| r#gen.gen_int_rem(&labels[deopt]));
    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

fn integer_rem_float_rhs(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    dst: Option<SlotId>,
    recv: SlotId,
    args: SlotId,
) -> bool {
    // Constant folding: both operands are concrete (Integer | Float).
    // ruby_mod of any two reals where one is float produces a float.
    if let Some(lhs) = state.coerce_C_f64(recv)
        && let Some(rhs) = state.coerce_C_f64(args)
    {
        let result = lhs.ruby_mod(&rhs);
        if state.def_C_float(dst, result) {
            return true;
        }
    }

    let lhs_xmm = state.load_xmm_fixnum(ir, recv);
    let rhs_xmm = state.load_xmm(ir, args);
    let Some(dst) = dst else {
        // Result discarded; no work needed (rem_ff is pure).
        return true;
    };
    let dst_xmm = state.def_F(ir, dst);
    let using_xmm = state.get_using_xmm();
    ir.inline(move |r#gen, _, _, base| {
        r#gen.gen_int_rem_if(lhs_xmm, rhs_xmm, dst_xmm, using_xmm, base)
    });
    true
}

///
/// ### Integer#**
///
/// - self ** other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2a=2a.html]
#[monoruby_builtin]
fn int_pow(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    super::op::pow_values(vm, globals, lfp.self_val(), lfp.arg(0)).ok_or_else(|| vm.take_error())
}

fn integer_pow(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    match rhs_class {
        Some(INTEGER_CLASS) => integer_pow_int_rhs(state, ir, dst, recv, args),
        Some(FLOAT_CLASS) => integer_pow_float_rhs(state, ir, dst, recv, args),
        _ => false,
    }
}

fn integer_pow_int_rhs(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    dst: Option<SlotId>,
    recv: SlotId,
    args: SlotId,
) -> bool {
    // Constant folding: both operands are concrete fixnums.
    // Only fold when the result fits in i63 (matches the previous
    // hardcoded BinOpK::Exp folding).
    if let Some(lhs) = state.is_fixnum_literal(recv)
        && let Some(rhs) = state.is_fixnum_literal(args)
        && let Ok(rhs_u32) = u32::try_from(rhs.get())
        && let Some(result) = lhs.get().checked_pow(rhs_u32)
        && let Some(imm) = Immediate::check_fixnum(result)
    {
        state.def_C(dst, imm);
        return true;
    }

    state.load_fixnum(ir, recv, GP::Rdi);
    state.load_fixnum(ir, args, GP::Rsi);
    let using_xmm = state.get_using_xmm();
    let error = ir.new_error(state);
    ir.inline(move |r#gen, _, labels, _| r#gen.gen_int_pow(using_xmm, &labels[error]));
    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

fn integer_pow_float_rhs(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    dst: Option<SlotId>,
    recv: SlotId,
    args: SlotId,
) -> bool {
    // Constant folding: both operands are concrete (Integer or Float).
    // The result of (lhs as f64).powf(rhs) is a Float for non-negative
    // base, and may be Complex (NaN-trigger) for negative base. Only
    // fold the safe Float case here; let the runtime helper handle
    // the Complex path.
    if let Some(lhs) = state.coerce_C_f64(recv)
        && let Some(rhs) = state.coerce_C_f64(args)
    {
        let result = lhs.powf(rhs);
        if !(result.is_nan() && lhs < 0.0) && state.def_C_float(dst, result) {
            return true;
        }
        // else: fall through to runtime call
    }

    let lhs_xmm = state.load_xmm_fixnum(ir, recv);
    let rhs_xmm = state.load_xmm(ir, args);
    let using_xmm = state.get_using_xmm();
    ir.inline(move |r#gen, _, _, base| r#gen.gen_int_pow_if(lhs_xmm, rhs_xmm, using_xmm, base));
    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

///
/// Common helper for inline JIT compilation of Integer#| / Integer#& / Integer#^.
///
/// Loads operands into registers (or detects a literal fixnum operand for
/// immediate encoding) and emits the corresponding bitwise instruction.
/// Returns false if rhs is not Integer or the call site is not simple,
/// falling back to the regular method dispatch path.
///
/// `fold` performs the operation on two i64 fixnum values for constant
/// folding. Bitwise ops on two i63 fixnums always produce an i63 result.
/// `emit_imm` generates `<op>q rdi, imm` (rdi: tagged fixnum lhs, imm: tagged
/// fixnum rhs that fits in `i32`).
/// `emit_rr` generates `<op>q rdi, rsi` (both tagged fixnums in rdi/rsi).
fn integer_bitop_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    store: &Store,
    callid: CallSiteId,
    rhs_class: Option<ClassId>,
    fold: impl Fn(i64, i64) -> i64,
    emit_imm: impl Fn(&mut Codegen, i64) + Copy + 'static,
    emit_rr: impl Fn(&mut Codegen) + Copy + 'static,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    if rhs_class != Some(INTEGER_CLASS) {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;

    let lhs_lit = state.is_fixnum_literal(recv);
    let rhs_lit = state.is_fixnum_literal(args);

    if let (Some(lhs), Some(rhs)) = (lhs_lit, rhs_lit) {
        // constant folding: both operands are concrete fixnums.
        // Bitwise ops on two i63 values always produce an i63 result.
        let result = fold(lhs.get(), rhs.get());
        state.def_C(dst, Immediate::check_fixnum(result).unwrap());
        return true;
    }

    if let Some(rhs) = rhs_lit {
        // recv op <fixnum literal>
        state.load(ir, recv, GP::Rdi);
        emit_bitop_imm(ir, rhs, emit_imm, emit_rr);
    } else if let Some(lhs) = lhs_lit {
        // <fixnum literal> op args (commutative — swap to use immediate form)
        state.load_fixnum(ir, args, GP::Rdi);
        emit_bitop_imm(ir, lhs, emit_imm, emit_rr);
    } else {
        state.load(ir, recv, GP::Rdi);
        state.load_fixnum(ir, args, GP::Rsi);
        ir.inline(move |r#gen, _, _, _| emit_rr(r#gen));
    }
    state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
    true
}

/// Emit a bitwise op with a literal `imm` rhs (lhs already in rdi as a tagged
/// fixnum).
///
/// `imm` is converted to its tagged-fixnum `Value` form. If the tagged
/// representation fits in `i32`, emit the immediate form directly via
/// `emit_imm`. Otherwise load the full 64-bit tagged value into rsi and
/// emit the register form via `emit_rr`.
fn emit_bitop_imm(
    ir: &mut AsmIr,
    imm: Fixnum,
    emit_imm: impl Fn(&mut Codegen, i64) + Copy + 'static,
    emit_rr: impl Fn(&mut Codegen) + Copy + 'static,
) {
    let tagged = Value::from(imm).id() as i64;
    if i32::try_from(tagged).is_ok() {
        ir.inline(move |r#gen, _, _, _| emit_imm(r#gen, tagged));
    } else {
        ir.inline(move |r#gen, _, _, _| {
            monoasm!( &mut r#gen.jit,
                movq rsi, (tagged);
            );
            emit_rr(r#gen);
        });
    }
}

fn integer_bitor(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    integer_bitop_inline(
        state,
        ir,
        store,
        callid,
        rhs_class,
        |a, b| a | b,
        |r#gen, imm| {
            monoasm!( &mut r#gen.jit,
                orq rdi, (imm);
            );
        },
        |r#gen| {
            monoasm!( &mut r#gen.jit,
                orq rdi, rsi;
            );
        },
    )
}

fn integer_bitand(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    integer_bitop_inline(
        state,
        ir,
        store,
        callid,
        rhs_class,
        |a, b| a & b,
        |r#gen, imm| {
            monoasm!( &mut r#gen.jit,
                andq rdi, (imm);
            );
        },
        |r#gen| {
            monoasm!( &mut r#gen.jit,
                andq rdi, rsi;
            );
        },
    )
}

fn integer_bitxor(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    rhs_class: Option<ClassId>,
) -> bool {
    // XOR of two tagged fixnums (both LSB=1) yields LSB=0, so we need to
    // restore the tag bit. For the immediate path we instead strip the tag
    // bit from the immediate (`imm - 1`) so the result keeps lhs's tag.
    integer_bitop_inline(
        state,
        ir,
        store,
        callid,
        rhs_class,
        |a, b| a ^ b,
        |r#gen, imm| {
            monoasm!( &mut r#gen.jit,
                xorq rdi, (imm - 1);
            );
        },
        |r#gen| {
            monoasm!( &mut r#gen.jit,
                xorq rdi, rsi;
                addq rdi, 1;
            );
        },
    )
}

///
/// ### Integer#[]
///
/// self[nth] -> Integer
/// self[nth, len] -> Integer
/// self[range] -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(len_val) = lfp.try_arg(1) {
        // self[nth, len] form
        let nth = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        let len = len_val.coerce_to_int_i64(vm, globals)?;
        if len < 0 {
            // Negative length: ignore length, extract all bits from nth onward
            // Equivalent to n >> nth (or n << |nth| for negative nth)
            return match self_val.unpack() {
                RV::Fixnum(base) => {
                    if nth >= 0 {
                        if nth >= 64 {
                            Ok(Value::integer(if base < 0 { -1 } else { 0 }))
                        } else {
                            Ok(Value::integer(base >> nth))
                        }
                    } else {
                        Ok(Value::bigint(BigInt::from(base) << (-nth) as usize))
                    }
                }
                RV::BigInt(base) => {
                    if nth >= 0 {
                        Ok(Value::bigint(base >> nth as usize))
                    } else {
                        Ok(Value::bigint(base << (-nth) as usize))
                    }
                }
                _ => unreachable!(),
            };
        }
        match self_val.unpack() {
            RV::Fixnum(base) => {
                if nth < 0 {
                    // Negative nth: shift left by |nth|, then mask
                    let shifted = BigInt::from(base) << (-nth) as usize;
                    let mask = if len >= 64 {
                        (BigInt::from(1) << len as usize) - 1
                    } else {
                        BigInt::from((1i64 << len) - 1)
                    };
                    return Ok(Value::bigint(shifted & mask));
                }
                let shifted = if nth >= 64 {
                    if base < 0 { -1i64 } else { 0i64 }
                } else {
                    base >> nth
                };
                let mask = if len >= 64 { -1i64 } else { (1i64 << len) - 1 };
                Ok(Value::integer(shifted & mask))
            }
            RV::BigInt(base) => {
                let shifted = if nth >= 0 {
                    base >> nth as usize
                } else {
                    base << (-nth) as usize
                };
                // Limit mask size to avoid OOM on huge len values.
                // If len exceeds the bit length of shifted, masking is a no-op for non-negative values.
                let bits = shifted.bits() as i64;
                if len > bits + 1 {
                    // For non-negative shifted, all bits fit within len, so no masking needed.
                    // For negative shifted, extracting len bits gives the two's complement representation.
                    if shifted >= BigInt::ZERO {
                        Ok(Value::bigint(shifted))
                    } else {
                        let mask = (BigInt::from(1) << len as usize) - 1;
                        Ok(Value::bigint(shifted & mask))
                    }
                } else {
                    let mask = (BigInt::from(1) << len as usize) - 1;
                    Ok(Value::bigint(shifted & mask))
                }
            }
            _ => unreachable!(),
        }
    } else {
        let arg0 = lfp.arg(0);
        // Try to_int coercion for non-integer, non-bigint arguments
        let index = match arg0.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => arg0,
            _ => {
                if arg0.is_range().is_some() {
                    arg0
                } else {
                    // Coerce to Integer via to_int; allow BigInt results
                    arg0.coerce_to_int(vm, globals)?
                }
            }
        };
        op::integer_index1(vm, globals, self_val, index)
    }
}

///
/// ### Integer#even?
///
/// - even? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/even=3f.html]
#[monoruby_builtin]
fn even_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn odd_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i % 2 != 0,
        RV::BigInt(b) => !(b % 2u32).is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

///
/// ### Numeric#nonzero?
///
/// - nonzero? -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/nonzero=3f.html]
#[monoruby_builtin]
fn nonzero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if match lfp.self_val().unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => unreachable!(),
    } {
        Ok(Value::nil())
    } else {
        Ok(lfp.self_val())
    }
}

///
/// ### Numeric#zero?
///
/// - zero? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/zero=3f.html]
#[monoruby_builtin]
fn zero_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = match lfp.self_val().unpack() {
        RV::Fixnum(i) => i == 0,
        RV::BigInt(b) => b.is_zero(),
        _ => unreachable!(),
    };
    Ok(Value::bool(b))
}

///
/// ### Integer#abs
///
/// - abs -> Integer
///
/// Returns the absolute value of the integer.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/abs.html]
#[monoruby_builtin]
fn abs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match lfp.self_val().unpack() {
        RV::Fixnum(i) => match i.checked_neg() {
            Some(neg) if i < 0 => Value::integer(neg),
            None if i < 0 => Value::bigint(-BigInt::from(i)),
            _ => lfp.self_val(),
        },
        RV::BigInt(b) => {
            if b < &BigInt::ZERO {
                Value::bigint(-b)
            } else {
                lfp.self_val()
            }
        }
        _ => unreachable!(),
    })
}

///
/// ### Integer#pow
///
/// - pow(other) -> Numeric
/// - pow(other, mod) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/pow.html]
#[monoruby_builtin]
fn pow(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let exp = lfp.arg(0);
    if let Some(mod_val) = lfp.try_arg(1) {
        // 3-argument form: pow(exp, mod)
        if !exp.is_integer() {
            return Err(MonorubyErr::typeerr(
                "Integer#pow() 2nd argument not allowed unless a 1st argument is integer",
            ));
        }
        if !mod_val.is_integer() {
            return Err(MonorubyErr::typeerr(
                "Integer#pow() 2nd argument not allowed unless all arguments are integers",
            ));
        }
        let exp_i = exp.coerce_to_int_i64(vm, globals)?;
        if exp_i < 0 {
            return Err(MonorubyErr::rangeerr(
                "Integer#pow() 1st argument cannot be negative when 2nd argument specified",
            ));
        }
        let mod_i = mod_val.coerce_to_int_i64(vm, globals)?;
        if mod_i == 0 {
            return Err(MonorubyErr::divide_by_zero());
        }
        // Modular exponentiation: self**exp % mod
        let base_big = match lfp.self_val().unpack() {
            RV::Fixnum(i) => BigInt::from(i),
            RV::BigInt(b) => b.clone(),
            _ => unreachable!(),
        };
        let mod_big = BigInt::from(mod_i.abs());
        let mut base = base_big % &mod_big;
        let mut result = BigInt::from(1);
        let mut e = exp_i as u64;
        while e > 0 {
            if e & 1 == 1 {
                result = result * &base % &mod_big;
            }
            base = &base * &base % &mod_big;
            e >>= 1;
        }
        // Normalize result to match Ruby semantics:
        // the result has the same sign as mod.
        if result < BigInt::ZERO {
            result += &mod_big;
        }
        if mod_i < 0 && result > BigInt::ZERO {
            result -= &mod_big;
        }
        Ok(Value::bigint(result))
    } else {
        // 2-argument form: pow(exp) — delegate to ** operator
        let lhs = lfp.self_val();
        vm.invoke_method_inner(globals, IdentId::_POW, lhs, &[exp], None, None)
    }
}

// ---- Integer#floor, ceil, truncate, round ----

/// Convert self to BigInt.
fn self_to_bigint(lfp: Lfp) -> BigInt {
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => BigInt::from(i),
        RV::BigInt(b) => b.clone(),
        _ => unreachable!(),
    }
}

/// Compute 10^n as BigInt.
fn pow10(n: u32) -> BigInt {
    BigInt::from(10u64).pow(n)
}

///
/// ### Integer#floor
///
/// - floor(ndigits = 0) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/floor.html]
#[monoruby_builtin]
fn int_floor(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        return Ok(lfp.self_val());
    };
    if ndigits >= 0 {
        return Ok(lfp.self_val());
    }
    let neg = (-ndigits) as u32;
    let d = pow10(neg);
    let val = self_to_bigint(lfp);
    // Ruby integer division floors toward negative infinity.
    let quot = num::integer::div_floor(val, d.clone());
    Ok(Value::bigint(quot * d))
}

///
/// ### Integer#ceil
///
/// - ceil(ndigits = 0) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/ceil.html]
#[monoruby_builtin]
fn int_ceil(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        return Ok(lfp.self_val());
    };
    if ndigits >= 0 {
        return Ok(lfp.self_val());
    }
    let neg = (-ndigits) as u32;
    let d = pow10(neg);
    let val = self_to_bigint(lfp);
    let quot = num::integer::div_ceil(val, d.clone());
    Ok(Value::bigint(quot * d))
}

///
/// ### Integer#truncate
///
/// - truncate(ndigits = 0) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/truncate.html]
#[monoruby_builtin]
fn int_truncate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        return Ok(lfp.self_val());
    };
    if ndigits >= 0 {
        return Ok(lfp.self_val());
    }
    let neg = (-ndigits) as u32;
    let d = pow10(neg);
    let val = self_to_bigint(lfp);
    // Truncate toward zero.
    let (quot, _) = num::integer::div_rem(val, d.clone());
    Ok(Value::bigint(quot * d))
}

///
/// ### Integer#round
///
/// - round(ndigits = 0, half: :up) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/round.html]
#[monoruby_builtin]
fn int_round(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ndigits = if let Some(d) = lfp.try_arg(0) {
        d.coerce_to_int_i64(vm, globals)?
    } else {
        return Ok(lfp.self_val());
    };
    if ndigits > 0x3FFFFFFF || ndigits < -0x40000000 {
        return Err(MonorubyErr::rangeerr(format!(
            "integer {} too big to convert to `int'",
            ndigits
        )));
    }
    let half = if let Some(kw_val) = lfp.try_arg(1) {
        super::float::parse_half_mode(kw_val)?
    } else {
        None
    };
    if ndigits > (i32::MAX as i64) || ndigits < -(i32::MAX as i64) {
        return Err(MonorubyErr::rangeerr(format!(
            "integer {} too big to convert to `int'",
            ndigits
        )));
    }
    if ndigits >= 0 {
        return Ok(lfp.self_val());
    }
    let neg = (-ndigits) as u32;
    let d = pow10(neg);
    let val = self_to_bigint(lfp);
    let is_neg = val < BigInt::ZERO;
    let abs_val = if is_neg { -&val } else { val };
    let (quot, rem) = num::integer::div_rem(abs_val, d.clone());
    let half_d = &d / 2;
    let rounded_quot = if rem > half_d {
        &quot + 1
    } else if rem < half_d {
        quot.clone()
    } else {
        // Exactly halfway
        use super::float::RoundHalf;
        match half {
            Some(RoundHalf::Down) => quot.clone(),
            Some(RoundHalf::Even) => {
                if &quot % 2 == BigInt::ZERO {
                    quot.clone()
                } else {
                    &quot + 1
                }
            }
            _ => {
                // :up (default) — round away from zero
                &quot + 1
            }
        }
    };
    let result = rounded_quot * d;
    Ok(Value::bigint(if is_neg { -result } else { result }))
}

///
/// ### Integer#size
///
/// - size -> Integer
///
/// Returns the number of bytes in the machine representation of the integer.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/size.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Fixnum(_) => Ok(Value::integer(std::mem::size_of::<i64>() as i64)),
        RV::BigInt(b) => {
            // CRuby returns the number of bytes for the unsigned magnitude
            let bits = b.bits();
            let bytes = ((bits + 7) / 8) as i64;
            // Minimum 1 byte for zero
            Ok(Value::integer(bytes.max(1)))
        }
        _ => unreachable!(),
    }
}

///
/// ### Integer#bit_length
///
/// - bit_length -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/bit_length.html]
#[monoruby_builtin]
fn bit_length(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    match lfp.self_val().unpack() {
        RV::Fixnum(i) => {
            let n = if i < 0 { !(i as u64) } else { i as u64 };
            Ok(Value::integer(64 - n.leading_zeros() as i64))
        }
        RV::BigInt(b) => {
            use num::traits::sign::Signed;
            let bits = if b.is_negative() {
                // For negative numbers, bit_length = bits(~n) = bits(-n - 1)
                let complement: BigInt = -(b) - 1;
                complement.bits()
            } else {
                b.bits()
            };
            Ok(Value::integer(bits as i64))
        }
        _ => unreachable!(),
    }
}

///
/// ### Integer#to_s
///
/// - to_s(base = 10) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/to_s.html]
#[monoruby_builtin]
fn to_s(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let base = if let Some(b) = lfp.try_arg(0) {
        let b = b.coerce_to_int_i64(vm, globals)?;
        if !(2..=36).contains(&b) {
            return Err(MonorubyErr::argumenterr(format!("invalid radix {}", b)));
        }
        b as u32
    } else {
        10
    };
    let s = match lfp.self_val().unpack() {
        RV::Fixnum(i) => {
            if base == 10 {
                format!("{}", i)
            } else {
                let negative = i < 0;
                let abs = if negative {
                    (i as i128).unsigned_abs()
                } else {
                    i as u128
                };
                let s = format_integer_base(abs, base);
                if negative { format!("-{}", s) } else { s }
            }
        }
        RV::BigInt(b) => {
            if base == 10 {
                format!("{}", b)
            } else {
                use num::traits::sign::Signed;
                let negative = b.is_negative();
                let abs = if negative { -b } else { b.clone() };
                let s = format_bigint_base(&abs, base);
                if negative { format!("-{}", s) } else { s }
            }
        }
        _ => unreachable!(),
    };
    use crate::value::rvalue::{Encoding, RStringInner};
    Ok(Value::string_from_inner(RStringInner::from_encoding(
        s.as_bytes(),
        Encoding::UsAscii,
    )))
}

fn format_integer_base(mut n: u128, base: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let base = base as u128;
    let mut buf = Vec::new();
    while n > 0 {
        buf.push(DIGITS[(n % base) as usize]);
        n /= base;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

fn format_bigint_base(n: &BigInt, base: u32) -> String {
    use num::Zero;
    if n.is_zero() {
        return "0".to_string();
    }
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let base_big = BigInt::from(base);
    let mut buf = Vec::new();
    let mut val = n.clone();
    while !val.is_zero() {
        let rem = &val % &base_big;
        buf.push(DIGITS[rem.to_usize().unwrap()]);
        val /= &base_big;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// Regression test for `integer_tof`: when the to_f result aliases the
    /// receiver slot (very common — e.g. `496.to_f` or chained
    /// `v.x.to_f` where the bytecodegen reuses the slot), the inline
    /// allocator used to discard the receiver's binding before reading it
    /// and produced NaN.
    ///
    /// Discovered while running the `khasinski/doom` Ruby port under
    /// monoruby — the renderer's `draw_seg_range` does many
    /// `seg_v1.x.to_f` / `seg_v1.y.to_f` calls and the chained form
    /// returned NaN, corrupting all downstream wall-texture mapping.
    #[test]
    fn integer_tof_jit_inline_aliased_dst() {
        run_test(
            r##"
        class V
          attr_reader :x
          def initialize(x); @x = x; end
        end
        v = V.new(496)
        # Chained: `v.x.to_f`. bytecodegen tends to reuse the slot
        # receiving `v.x`'s return as the dst of `to_f`, so dst aliases recv.
        def chained(v); v.x.to_f; end
        # Literal Integer: same shape -- the LOAD_CONST result and the to_f
        # destination land in the same slot.
        def literal; 496.to_f; end
        # Warm the JIT, then check.
        3000.times { chained(v); literal }
        [chained(v), literal]
        "##,
        );
    }

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
    fn upto() {
        run_test(
            r##"
        a = 0
        x = 0.upto(10) do |z|
          b = 0
          13.step(12) do |y|
            15.step(25) do |x|
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
        run_test(
            r##"
        res = 0
        10.upto(8) do |z|
          res += z
        end
        res
        "##,
        );
    }

    #[test]
    fn downto() {
        run_test(
            r##"
        a = 0
        x = 10.downto(0) do |z|
          b = 0
          13.step(12) do |y|
            15.step(25) do |x|
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
        run_test(
            r##"
        res = 0
        8.downto(10) do |z|
          res += z
        end
        res
        "##,
        );
    }

    #[test]
    fn succ() {
        run_test("253.succ.succ.succ");
        run_test_with_prelude("$a.succ.succ.succ", "$a = 100");
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
    fn non_zero() {
        run_test("253.nonzero?");
        run_test("0.nonzero?");
        run_test("253.zero?");
        run_test("0.zero?");
    }

    #[test]
    fn index() {
        run_tests(&[
            "999999999999999999999999999999999[-100]",
            "999999999999999999999999999999999[0]",
            "999999999999999999999999999999999[47]",
            "999999999999999999999999999999999[48]",
            "999999999999999999999999999999999[82]",
            "999999999999999999999999999999999[85]",
            "999999999999999999999999999999999[86]",
            "999999999999999999999999999999999[999999999999999999999999999999999]",
            "27[-2]",
            "27[0]",
            "27[2]",
            "27[200]",
            "27[2000000000000000000000000000000000000000000]",
            // Integer#[Range]
            "0b11110000[4..6]",
            "0b11110000[4...7]",
            "0b11110000[-4..6]",
            "0b11110000[-4..-6]",
            "0b11110000[4..-6]",
            "0b11110000[6..4]",
            "0b11110000[7...4]",
            "0b11110000[6..-4]",
            "0b11110000[-6..-4]",
            "0b11110000[-6..4]",
            "0xFF[0..3]",
            "0xFF[4..7]",
            "255[0..7]",
            "0b11110000[0..3]",
            "123[0..0]",
            "123[0...0]",
            "999999999999999999999999999999999[0..31]",
            "999999999999999999999999999999999[-10..31]",
            "999999999999999999999999999999999[-10..-31]",
            "999999999999999999999999999999999[0..-31]",
            "999999999999999999999999999999999[32..63]",
            "999999999999999999999999999999999[63..32]",
            "999999999999999999999999999999999[63..-32]",
            "999999999999999999999999999999999[-63..-32]",
            "999999999999999999999999999999999[-63..32]",
            "999999999999999999999999999999999[64..95]",
            // Negative self (Fixnum)
            "-1[0..7]",
            "-1[0..63]",
            "-1[0...64]",
            "-1[4..7]",
            "-1[-4..7]",
            "-1[-4..-6]",
            "-1[4..-6]",
            "-256[0..7]",
            "-256[8..15]",
            "-256[-4..11]",
            "-127[0..7]",
            "-127[4..11]",
            "-127[0..0]",
            "-127[0...0]",
            "-2[0..63]",
            "-2[63..32]",
            // Negative self (BigInt)
            "-999999999999999999999999999999999[0..31]",
            "-999999999999999999999999999999999[32..63]",
            "-999999999999999999999999999999999[64..95]",
            "-999999999999999999999999999999999[-10..31]",
            "-999999999999999999999999999999999[-10..-31]",
            "-999999999999999999999999999999999[63..32]",
            "-999999999999999999999999999999999[-63..32]",
        ]);
    }

    #[test]
    fn even_() {
        run_tests(&[
            "100.even?",
            "-100.even?",
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?",
            "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.even?",
            "100.odd?",
            "-100.odd?",
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?",
            "-10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.odd?",
        ]);
    }

    #[test]
    fn cmp() {
        run_tests(&[
            "100.send(:==, 100)",
            "100.send(:!=, 100)",
            "100.send(:>=, 100)",
            "100.send(:<=, 100)",
            "100.==(100)",
            "100.==(50)",
            "100.==(100.0)",
            r#"100.==("100")"#,
            "100.!=(100)",
            "100.!=(50)",
            "100.!=(100.0)",
            r#"100.!=("100")"#,
        ]);
    }

    #[test]
    fn digits() {
        run_test("100.digits");
        run_test("100.digits(10)");
        run_test("100.digits(16)");
        run_test("100.digits(16.5)");
        run_test("class C; def to_int; 10; end; end; 100.digits(C.new)");

        run_test_error("(-100).digits(16)");
        run_test_error("-100.digits(16)");
        run_test_error("100.digits(-16)");
        run_test_error("100.digits(0)");
        run_test_error("100.digits(1)");
    }

    #[test]
    fn integer_abs() {
        run_test("42.abs");
        run_test("(-42).abs");
        run_test("0.abs");
        run_test("42.magnitude");
        run_test("(-42).magnitude");
    }

    #[test]
    fn integer_negative() {
        run_test("42.negative?");
        run_test("(-42).negative?");
        run_test("0.negative?");
        run_test("42.positive?");
        run_test("(-42).positive?");
        run_test("0.positive?");
    }

    #[test]
    fn integer_integer() {
        run_test("42.integer?");
        run_test("0.integer?");
    }

    #[test]
    fn integer_ord() {
        run_test("42.ord");
        run_test("0.ord");
    }

    #[test]
    fn integer_ceil() {
        run_test("42.ceil");
        run_test("42.ceil(0)");
        run_test("42.ceil(-1)");
        run_test("45.ceil(-1)");
        run_test("(-42).ceil(-1)");
        run_test("123.ceil(-2)");
    }

    #[test]
    fn integer_round() {
        run_tests(&[
            "42.round",
            "42.round(0)",
            "42.round(-1)",
            "45.round(-1)",
            "(-42).round(-1)",
            "15.round(-1)",
            "25.round(-1)",
            "35.round(-1)",
        ]);
    }

    #[test]
    fn integer_truncate() {
        run_test("42.truncate");
        run_test("42.truncate(0)");
        run_test("42.truncate(-1)");
        run_test("(-42).truncate(-1)");
        run_test("123.truncate(-2)");
    }

    #[test]
    fn integer_next_pred() {
        run_test("42.next");
        run_test("(-1).next");
        run_test("42.pred");
        run_test("0.pred");
    }

    #[test]
    fn integer_remainder() {
        run_test("5.remainder(3)");
        run_test("(-5).remainder(3)");
        run_test("5.remainder(-3)");
        run_test("(-5).remainder(-3)");
    }

    #[test]
    fn integer_fdiv() {
        run_test("1.fdiv(2)");
        run_test("(-1).fdiv(2)");
        run_test("1.fdiv(2.0)");
    }

    #[test]
    fn integer_gcd_lcm() {
        run_tests(&[
            "12.gcd(8)",
            "12.gcd(-8)",
            "(-12).gcd(8)",
            "0.gcd(5)",
            "5.gcd(0)",
            "12.lcm(8)",
            "12.lcm(-8)",
            "0.lcm(5)",
            "12.gcdlcm(8)",
        ]);
    }

    #[test]
    fn integer_pow() {
        run_test("2.pow(10)");
        run_test("2.pow(10, 1000)");
        run_test("2.pow(0)");
    }

    #[test]
    fn integer_allbits() {
        run_tests(&[
            "0b1010.allbits?(0b1010)",
            "0b1010.allbits?(0b1000)",
            "0b1010.allbits?(0b1011)",
            "0b1010.anybits?(0b0001)",
            "0b1010.anybits?(0b1001)",
            "0b1010.nobits?(0b0101)",
            "0b1010.nobits?(0b0001)",
        ]);
    }

    #[test]
    fn integer_bit_length() {
        run_tests(&[
            "0.bit_length",
            "1.bit_length",
            "255.bit_length",
            "256.bit_length",
            "(-1).bit_length",
            "(-256).bit_length",
            "(-257).bit_length",
            "(2**100).bit_length",
            "(-(2**100)).bit_length",
        ]);
    }

    #[test]
    fn integer_coerce() {
        run_test("1.coerce(2)");
        run_test("1.coerce(2.5)");
    }

    #[test]
    fn integer_numerator_denominator() {
        run_test("42.numerator");
        run_test("42.denominator");
        run_test("0.numerator");
        run_test("0.denominator");
    }

    #[test]
    fn integer_sqrt() {
        run_test("Integer.sqrt(0)");
        run_test("Integer.sqrt(1)");
        run_test("Integer.sqrt(4)");
        run_test("Integer.sqrt(9)");
        run_test("Integer.sqrt(10)");
        run_test("Integer.sqrt(100)");
    }

    #[test]
    fn integer_try_convert() {
        run_test("Integer.try_convert(1)");
        run_test("Integer.try_convert(nil)");
        run_test(r#"Integer.try_convert("1")"#);
    }

    #[test]
    fn positive_negative() {
        run_test("42.positive?");
        run_test("(-42).positive?");
        run_test("0.positive?");
        run_test("42.negative?");
        run_test("(-42).negative?");
        run_test("0.negative?");
    }

    #[test]
    fn bop_redefinition() {
        // Verify that redefining Integer#+ does not crash the JIT.
        // Uses run_test_no_result_check because BOP redefinition
        // affects global state and the result may differ from CRuby.
        run_test_no_result_check(
            r##"
            res = []
            res << (1 + 2)
            class Integer
              def +(other)
                self - other
              end
            end
            res << (1 + 2)
            class Integer
              remove_method :+
            end
            res << (1 + 2)
            res
            "##,
        );
    }

    #[test]
    fn integer_to_r() {
        run_test("3.respond_to?(:to_r)");
        run_test("3.respond_to?(:rationalize)");
    }

    #[test]
    fn bitwise_type_error() {
        // Non-integer arguments raise TypeError
        run_test_error("5 | 'a'");
        run_test_error("5 & 'a'");
        run_test_error("5 ^ 'a'");
        run_test_error("5 | 1.5");
        run_test_error("5 & nil");
    }

    #[test]
    fn to_s_with_base() {
        run_test("255.to_s(16)");
        run_test("(-255).to_s(16)");
        run_test("10.to_s(2)");
        run_test("0.to_s(16)");
        run_test("123.to_s");
        run_test("255.to_s(36)");
    }

    #[test]
    fn chr_with_encoding() {
        run_test("65.chr");
        run_test("97.chr");
    }

    #[test]
    fn index_with_length() {
        run_test("42[0, 3]");
        run_test("0b11010[1, 3]");
        run_test("255[4, 4]");
    }

    #[test]
    fn integer_eql() {
        run_test("(2**64).eql?(2**64)");
        run_test("1.eql?(1)");
        run_test("1.eql?(1.0)");
        run_test("(2**64).eql?(2**64 + 1)");
    }

    #[test]
    fn pow_bignum_exponent() {
        run_test("1 ** (2**100)");
        run_test("(-1) ** (2**100)");
        run_test("(-1) ** (2**100 + 1)");
        run_test_error("2 ** (2**100)");
    }

    #[test]
    fn shift_to_int() {
        run_test_with_prelude(
            "[8 << Foo.new, 8 >> Foo.new]",
            r#"
            class Foo; def to_int; 2; end; end
        "#,
        );
    }

    #[test]
    fn bitwise_float_typeerror() {
        run_test_error("1 & 3.0");
        run_test_error("1 | 3.0");
        run_test_error("1 ^ 3.0");
    }

    #[test]
    fn fdiv_fixnum() {
        run_test("100.fdiv(3)");
        run_test("10.fdiv(2)");
    }

    #[test]
    fn divmod_nan() {
        run_test_error("1.divmod(Float::NAN)");
    }

    #[test]
    fn div_coerce() {
        run_test("10 / 3.0");
        run_test("10 / 2.5");
    }

    #[test]
    fn modulo_float_zero() {
        run_test_error("10 % 0.0");
    }

    #[test]
    fn eq_reverse_dispatch() {
        run_test_with_prelude(
            "[42 == Foo.new, 42 === Foo.new, (2**64) == Foo.new, 42 == 99]",
            r#"
            class Foo; def ==(other); other == 42; end; end
            "#,
        );
    }

    #[test]
    fn integer_dup() {
        run_test("42.dup");
        run_test("42.dup.equal?(42)");
        run_test("a = 2**100; a.dup.equal?(a)");
    }

    #[test]
    fn integer_round_errors() {
        run_test_error("42.round(Float::INFINITY)");
        run_test_error("42.round(Float::NAN)");
        run_test_error("42.round('string')");
    }

    #[test]
    fn integer_floor_errors() {
        run_test_error("42.floor(Float::INFINITY)");
        run_test_error("42.floor(Float::NAN)");
        run_test_error("42.floor('string')");
    }

    #[test]
    fn integer_ceil_errors() {
        run_test_error("42.ceil(Float::INFINITY)");
        run_test_error("42.ceil(Float::NAN)");
        run_test_error("42.ceil('string')");
    }

    #[test]
    fn integer_truncate_errors() {
        run_test_error("42.truncate(Float::INFINITY)");
        run_test_error("42.truncate(Float::NAN)");
        run_test_error("42.truncate('string')");
    }

    #[test]
    fn integer_round_float_ndigits() {
        run_test("42.round(1.5)");
        run_test("42.round(-1.5)");
    }

    #[test]
    fn integer_coerce_extended() {
        run_test_error(r#"1.coerce("")"#);
        run_test_with_prelude(
            "1.coerce(Foo.new)",
            r#"
            class Foo; def to_f; 99.5; end; end
        "#,
        );
    }

    #[test]
    fn integer_chr_utf8() {
        run_tests(&[
            r#"256.chr("UTF-8")"#,
            r#"900.chr("UTF-8").bytes"#,
            r#"128.chr("UTF-8").bytes"#,
            r#"0x0100.chr(Encoding::UTF_8).bytes.to_a"#,
            r#"0x3000.chr(Encoding::UTF_8).bytes.to_a"#,
            r#"0x0080.chr(Encoding::UTF_8).bytes.to_a"#,
            r#"0x00FF.chr(Encoding::UTF_8).bytes.to_a"#,
            r#"0x0100.chr(Encoding::UTF_8).encoding.to_s"#,
        ]);
        run_test_error(r#"(-1).chr"#);
        // Surrogates should raise RangeError
        run_test_error(r#"0xD800.chr("UTF-8")"#);
        run_test_error(r#"0xDFFF.chr("UTF-8")"#);
        run_test_error(r#"2206368128.chr(Encoding::UTF_8)"#);
    }

    #[test]
    fn integer_index_negative() {
        run_test("0b11010[-2, 3]");
        run_test("0b11010[1, 3]");
    }

    #[test]
    fn integer_index_endless_range() {
        run_test("0b11010[1..]");
        run_test("255[4..]");
        run_test("255[70..]");
        run_test("255[-2..]");
        run_test("123456789123456789123456789123456789123456789[70..]");
        run_test("123456789123456789123456789123456789123456789[-3..]");

        run_test_error("255[Float::NAN..]");
        run_test_error("255[Float::INFINITY..]");
        run_test_error("255[Float::-INFINITY..]");

        run_test_error("255[..4]");
        run_test_error("255[..70]");
        run_test("255[..-2]");
        run_test_error("123456789123456789123456789123456789123456789[..70]");
        run_test("123456789123456789123456789123456789123456789[..-3]");
    }

    #[test]
    fn integer_round_half() {
        run_test("25.round(-1, half: :up)");
        run_test("25.round(-1, half: :down)");
        run_test("25.round(-1, half: :even)");
        run_test("15.round(-1, half: :even)");
    }

    #[test]
    fn integer_allbits_typeerror() {
        run_test_error(r#"42.allbits?("foo")"#);
        run_test_error(r#"42.anybits?("foo")"#);
        run_test_error(r#"42.nobits?("foo")"#);
    }

    #[test]
    fn integer_abs_bigint() {
        run_test("(-(2**64)).abs");
        run_test("(2**64).abs");
        run_test("(-42).abs");
    }

    #[test]
    fn integer_neg_boundary() {
        run_test("-(2**62)");
    }

    #[test]
    fn integer_div() {
        run_test("7.div(3)");
        run_test("(-7).div(3)");
        run_test("7.div(-3)");
        run_test("7.div(3.0)");
        run_test("5.div(Rational(3))");
        run_test_error("7.div(0)");
        run_test_error("7.div(0.0)");
    }

    #[test]
    fn integer_ceildiv() {
        run_test("7.ceildiv(3)");
        run_test("(-7).ceildiv(3)");
        run_test("7.ceildiv(-3)");
        run_test_error("7.ceildiv(0)");
    }

    #[test]
    fn pow_zero_neg() {
        run_test_error("0 ** -1");
        run_test_error("0 ** -2");
    }

    #[test]
    fn pow_negative_base_float_exp() {
        // (-1) ** 0.5 should return Complex in CRuby
        run_test("((-1) ** 0.5).class");
    }

    #[test]
    fn command_call_with_bitnot_arg() {
        // `p ~x` / `foo ~x` — `~` is only ever a unary operator, so it
        // must begin an argument in a parenless method call (matches
        // CRuby's `p ~5 == -6`).
        run_test("p ~5");
        run_test("p ~5 + 3");
        run_test("a = 10; p ~a");
        run_test("a = 10; b = 20; p ~a + b");
        run_test(
            r#"
            def foo(x); x; end
            foo ~5
            "#,
        );
    }

    #[test]
    fn pow_unary_minus_rhs() {
        // `x ** -expr` where expr is a method call / identifier must parse
        // as `x ** (-expr)` (right-associative and tighter than outer `-`).
        run_test(
            r#"
            def three; 3; end
            2 ** -three
            "#,
        );
        // Double negation: `2 ** --3`.
        run_test("2 ** --3");
        // `-x ** y` keeps the `-` outside (unary minus binds looser than **).
        run_test("-3 ** 4");
        // Right-associative: `2 ** -3 ** 2` == `2 ** -(3 ** 2)`.
        run_test("2 ** -3 ** 2");
        // Negation of a BigInt expression as RHS.
        run_test(
            r#"
            big = 10 ** 20
            (Rational(1) ** -big).to_s
            "#,
        );
    }

    #[test]
    fn integer_size_bigint() {
        run_test("(2**64).size");
        run_test("(2**63).size");
    }

    #[test]
    fn integer_cmp_coerce() {
        run_test_with_prelude(
            "[1 <=> Foo.new, 100 <=> Foo.new]",
            r#"
            class Foo
              def coerce(other)
                [other.to_f, 42.0]
              end
            end
        "#,
        );
    }

    #[test]
    fn downto_upto_bad_arg() {
        run_test_error(r#"1.upto("foo") {}"#);
        run_test_error(r#"1.downto("foo") {}"#);
    }

    #[test]
    fn fdiv_large_bigint() {
        run_test("(10**200).fdiv(10**199)");
        run_test("(10**200).fdiv(3 * 10**199)");
    }

    #[test]
    fn integer_sqrt_newton() {
        run_test("Integer.sqrt(0)");
        run_test("Integer.sqrt(1)");
        run_test("Integer.sqrt(100)");
        run_test_error("Integer.sqrt(-1)");
        run_test_error(r#"Integer.sqrt("foo")"#);
    }

    #[test]
    fn round_negative_precision() {
        run_test("249.round(-2)");
        run_test("(-249).round(-2)");
        run_test("150.round(-2)");
    }

    #[test]
    fn floor_bigint_neg_precision() {
        run_test("(-130).floor(-1)");
        run_test("(-131).floor(-1)");
    }

    #[test]
    fn module_include_q() {
        run_test("Integer.include?(Comparable)");
        run_test("Integer.include?(Enumerable)");
    }

    #[test]
    fn rational_pow_zero_base() {
        run_test_error("Rational(0, 1) ** -1");
        run_test_error("Rational(0, 1) ** Rational(-1, 1)");
    }

    #[test]
    fn integer_coerce_reject_types() {
        run_test_error("1.coerce(nil)");
        run_test_error("1.coerce(true)");
        run_test_error("1.coerce(false)");
        run_test_error("1.coerce(:sym)");
    }

    #[test]
    fn integer_upto_float_endpoint() {
        run_test("res = []; 1.upto(3.5) {|i| res << i}; res");
        run_test("res = []; 5.downto(2.5) {|i| res << i}; res");
    }

    #[test]
    fn integer_to_s_encoding() {
        run_test("42.to_s.encoding.to_s");
        run_test("255.to_s(16).encoding.to_s");
    }

    #[test]
    fn integer_chr_ascii() {
        run_tests(&[
            "65.chr",
            r#"65.chr("US-ASCII")"#,
            "65.chr.encoding.to_s",
            "200.chr.encoding.to_s",
            r#"0.chr(Encoding::BINARY).encoding.to_s"#,
            r#"255.chr(Encoding::BINARY).bytes.to_a"#,
        ]);
        run_test_error(r#"128.chr("US-ASCII")"#);
        run_test_error(r#"256.chr(Encoding::BINARY)"#);
        run_test_error(r#"256.chr"#);
        run_test_error("(-1).chr(Encoding::US_ASCII)");
    }

    #[test]
    fn integer_chr_encoding_arg() {
        // String encoding argument
        run_tests(&[
            r#"0x0045.chr("utf-8").bytes.to_a"#,
            r#"0x0045.chr("UTF-8").bytes.to_a"#,
            r#"0x0045.chr("Utf-8").bytes.to_a"#,
            r#"0x0045.chr("utf-8").encoding.to_s"#,
        ]);
        // Shift_JIS mock encoding
        run_tests(&[
            r#"0x0000.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            r#"0x007F.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            r#"0x00A1.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            r#"0x00DF.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            r#"0x8140.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            r#"0xFC4B.chr(Encoding::SHIFT_JIS).bytes.to_a"#,
            // Encoding name comparison skipped — monoruby uses "SHIFT-JIS" vs CRuby "Shift_JIS"
        ]);
        // Invalid codepoints
        run_test_error(r#"0x80.chr("US-ASCII")"#);
        run_test_error(r#"0x0100.chr("BINARY")"#);
        run_test_error(r#"0x80.chr("SHIFT_JIS")"#);
        run_test_error(r#"0xE0.chr("SHIFT_JIS")"#);
        // Bignum always out of range
        run_test_error("(2**100).chr");
    }

    #[test]
    fn integer_chr_default_internal() {
        // default_internal = nil: >255 should raise
        run_test("Encoding.default_internal");
        run_test_error("256.chr");
        // default_internal = UTF-8: >255 uses UTF-8
        run_test(
            r#"
            Encoding.default_internal = Encoding::UTF_8
            res = 0x3000.chr.bytes.to_a
            Encoding.default_internal = nil
            res
        "#,
        );
        run_test(
            r#"
            Encoding.default_internal = Encoding::UTF_8
            res = 0x3000.chr.encoding.to_s
            Encoding.default_internal = nil
            res
        "#,
        );
        // 0-127 always US-ASCII regardless of default_internal
        run_test(
            r#"
            Encoding.default_internal = Encoding::UTF_8
            res = 65.chr.encoding.to_s
            Encoding.default_internal = nil
            res
        "#,
        );
    }

    #[test]
    fn pow_modular_sign() {
        run_test("(-2).pow(3, 12)");
        run_test("2.pow(3, 12)");
        run_test("2.pow(3, -12)");
        run_test("(-2).pow(3, -12)");
        run_test("(-7).pow(3, 19)");
    }

    #[test]
    fn try_convert() {
        run_test("Integer.try_convert(1)");
        run_test("Integer.try_convert(1.0)");
        run_test("Integer.try_convert(nil)");
    }

    #[test]
    fn integer_floor_ceil_truncate_round() {
        run_tests(&[
            // ndigits >= 0 returns self
            "15.floor",
            "15.ceil",
            "15.truncate",
            "15.round",
            "15.floor(2)",
            "15.ceil(2)",
            // negative ndigits
            "15.floor(-1)",
            "(-15).floor(-1)",
            "15.ceil(-1)",
            "(-15).ceil(-1)",
            "15.truncate(-1)",
            "(-15).truncate(-1)",
            "15.round(-1)",
            "(-15).round(-1)",
            // round with half: keyword
            "25.round(-1, half: :up)",
            "25.round(-1, half: :down)",
            "25.round(-1, half: :even)",
            "35.round(-1, half: :even)",
            "(-25).round(-1, half: :up)",
            "(-25).round(-1, half: :down)",
            "(-25).round(-1, half: :even)",
            // larger ndigits
            "1234567.floor(-3)",
            "1234567.ceil(-3)",
            "1234567.truncate(-3)",
            "1234567.round(-3)",
            // bignum
            "(10**20 + 555).round(-1)",
            "(10**20 + 555).floor(-1)",
        ]);
    }

    #[test]
    fn try_convert_error_message() {
        run_test_error(r#"class C; def to_int; "str"; end; end; Integer.try_convert(C.new)"#);
    }

    #[test]
    fn upto_with_float() {
        run_tests(&[
            "res = []; 9.upto(13) {|i| res << i}; res",
            "res = []; (-5).upto(-1.3) {|i| res << i}; res",
            "res = []; 1.upto(3.9) {|i| res << i}; res",
            "res = []; 1.upto(3.0) {|i| res << i}; res",
            "(-5).upto(-1.3).to_a",
        ]);
    }

    #[test]
    fn upto_downto_non_numeric_size() {
        run_test_error(r#"1.upto("a").size"#);
        run_test_error(r#"1.downto("a").size"#);
    }

    #[test]
    fn round_range_error() {
        run_test_error("42.round(1 << 31)");
        run_test_error("42.round(-(1 << 31) - 1)");
    }

    #[test]
    fn integer_shr() {
        run_tests(&[
            "8 >> 1",
            "-8 >> 1",
            "256 >> 4",
            "1 >> 100",
            // BigInt
            "(10**20) >> 10",
            "(10**20) >> 0",
        ]);
    }

    #[test]
    fn integer_shr_jit_edge_cases() {
        run_tests(&[
            // negative constant rhs (>> -k == << k)
            "a = 1; a >> -3",
            "a = 5; a >> -10",
            "a = -1; a >> -3",
            // large negative rhs (overflow → deopt to BigInt)
            "a = 0; a >> -100",
            "a = 1; a >> -100",
            // large positive rhs
            "a = 100; a >> 64",
            "a = 100; a >> 100",
            "a = -1; a >> 64",
            "a = -1; a >> 100",
            // zero lhs
            "a = 0; a >> 10",
            "a = 0; a >> -10",
        ]);
    }

    #[test]
    fn integer_shl_extended() {
        run_tests(&[
            "1 << 10",
            "-1 << 10",
            // BigInt shift
            "(10**20) << 10",
        ]);
    }

    #[test]
    fn integer_shl_jit_edge_cases() {
        run_tests(&[
            // negative constant rhs (<< -k == >> k)
            "a = 256; a << -4",
            "a = -256; a << -4",
            "a = 1; a << -100",
            "a = -1; a << -100",
            // large positive rhs (overflow → deopt to BigInt)
            "a = 0; a << 100",
            "a = 1; a << 100",
            // large negative rhs
            "a = 100; a << -64",
            "a = 100; a << -100",
            "a = -1; a << -64",
            "a = -1; a << -100",
            // zero lhs
            "a = 0; a << 10",
            "a = 0; a << -10",
        ]);
    }

    #[test]
    fn integer_shift_constant_folding() {
        // Both operands are literals: should be constant-folded at JIT time.
        // Wrap in a method (via prelude) so the call is JIT'd; defining the
        // method inside `run_test` would deopt the JIT every iteration.
        let prelude = "
            def shr_a; 8 >> 1; end
            def shr_b; 8 >> 0; end
            def shr_c; -8 >> 2; end
            def shr_d; 1 >> 64; end
            def shr_e; -1 >> 64; end
            def shr_f; 1 >> 100; end
            def shr_g; -1 >> 100; end
            def shr_h; 1 >> -3; end
            def shr_i; 0 >> -100; end
            def shl_a; 1 << 10; end
            def shl_b; -1 << 10; end
            def shl_c; 0 << 100; end
            def shl_d; 256 << -4; end
            def shl_e; -256 << -4; end
            def shl_f; 1 << -100; end
            def shl_g; -1 << -100; end
            def shl_h; 1 << 62; end
            def shl_i; 1 << 100; end
        ";
        run_test_with_prelude(
            "[shr_a, shr_b, shr_c, shr_d, shr_e, shr_f, shr_g, shr_h, shr_i,
              shl_a, shl_b, shl_c, shl_d, shl_e, shl_f, shl_g, shl_h, shl_i]",
            prelude,
        );
    }

    #[test]
    fn integer_bitop_constant_folding() {
        // Both operands are literals: should be constant-folded at JIT time.
        let prelude = "
            def or_a; 0xFF | 0x0F; end
            def and_a; 0xFF & 0x0F; end
            def xor_a; 0xFF ^ 0x0F; end
            def or_b; -1 | 0x0F; end
            def and_b; -1 & 0x0F; end
            def xor_b; -1 ^ 0x0F; end
            def or_c; 42 | 0; end
            def and_c; 42 & 0; end
            def xor_c; 42 ^ 0; end
            def and_d; 42 & -1; end
            def xor_d; 42 ^ -1; end
            def or_big; 0x12345678 | 0xDEAD_BEEF_CAFE; end
            def and_big; 0x12345678 & 0xDEAD_BEEF_CAFE; end
            def xor_big; 0x12345678 ^ 0xDEAD_BEEF_CAFE; end
        ";
        run_test_with_prelude(
            "[or_a, and_a, xor_a, or_b, and_b, xor_b, or_c, and_c, xor_c, and_d, xor_d,
              or_big, and_big, xor_big]",
            prelude,
        );
    }

    #[test]
    fn integer_bitop_jit_edge_cases() {
        run_tests(&[
            // immediate rhs (small, tagged form fits in i32)
            "a = 0xFF; a | 0x0F",
            "a = 0xFF; a & 0x0F",
            "a = 0xFF; a ^ 0x0F",
            // immediate lhs (commutative)
            "a = 0x0F; 0xFF | a",
            "a = 0x0F; 0xFF & a",
            "a = 0x0F; 0xFF ^ a",
            // both register
            "a = 0xFF; b = 0x0F; a | b",
            "a = 0xFF; b = 0x0F; a & b",
            "a = 0xFF; b = 0x0F; a ^ b",
            // large fixnum literal (tagged form does NOT fit in i32 → register-loaded path)
            "a = 0x12345678; a | 0xDEADBEEF_CAFE",
            "a = 0x12345678; a & 0xDEADBEEF_CAFE",
            "a = 0x12345678; a ^ 0xDEADBEEF_CAFE",
            // commutative form with large literal
            "a = 0x12345678; 0xDEADBEEF_CAFE | a",
            "a = 0x12345678; 0xDEADBEEF_CAFE & a",
            "a = 0x12345678; 0xDEADBEEF_CAFE ^ a",
            // negative values (sign bit interaction with tag)
            "a = -1; a | 0x0F",
            "a = -1; a & 0x0F",
            "a = -1; a ^ 0x0F",
            "a = -1; b = 5; a | b",
            "a = -1; b = 5; a & b",
            "a = -1; b = 5; a ^ b",
            // negative large literal
            "a = -1; a & 0xDEADBEEF_CAFE",
            "a = -1; a ^ 0xDEADBEEF_CAFE",
            // identity / zero
            "a = 42; a | 0",
            "a = 42; a & 0",
            "a = 42; a ^ 0",
            "a = 42; a & -1",
            "a = 42; a ^ -1",
            // self
            "a = 42; a | a",
            "a = 42; a & a",
            "a = 42; a ^ a",
            // BigInt fallback (uses generic method dispatch)
            "a = 10**20; a | 0xFF",
            "a = 10**20; a & 0xFF",
            "a = 10**20; a ^ 0xFF",
        ]);
    }

    #[test]
    fn integer_pow_jit_edge_cases() {
        run_tests(&[
            // small fixnum^fixnum that fits in i63
            "a = 2; a ** 10",
            "a = 3; a ** 5",
            "a = 0; a ** 0", // 0**0 == 1
            "a = 1; a ** 100",
            "a = -2; a ** 5",
            "a = -2; a ** 4",
            // Result overflows fixnum → BigInt (handled by pow_ii runtime call)
            "a = 2; a ** 62",
            "a = 2; a ** 100",
            "a = 10; a ** 20",
            // Negative exponent → Rational
            "a = 2; a ** -3",
            "a = -2; a ** -3",
            // 1**n / (-1)**n / 0**n special cases
            "a = 1; a ** -100",
            "a = -1; a ** 5",
            "a = -1; a ** -5",
            // Float rhs (non-Integer rhs falls back to method dispatch)
            "a = 2; a ** 3.0",
            "a = 2; a ** -2.0",
        ]);
    }

    #[test]
    fn integer_pow_constant_folding() {
        // Both operands are fixnum literals: should fold at JIT time
        // when result fits in fixnum.
        let prelude = "
            def pow_a; 2 ** 10; end
            def pow_b; 3 ** 5; end
            def pow_c; 10 ** 5; end
            def pow_d; 1 ** 100; end
            def pow_e; (-2) ** 4; end
            def pow_f; (-2) ** 5; end
            def pow_g; 0 ** 0; end
            def pow_h; 0 ** 5; end
            # Overflow / non-fixnum result: folder bails out, runtime path taken.
            def pow_overflow; 2 ** 62; end
            def pow_overflow2; 2 ** 100; end
            # Negative exponent: folder bails (rhs as u32 fails)
            def pow_neg; 2 ** -3; end
        ";
        run_test_with_prelude(
            "[pow_a, pow_b, pow_c, pow_d, pow_e, pow_f, pow_g, pow_h,
              pow_overflow, pow_overflow2, pow_neg]",
            prelude,
        );
    }

    #[test]
    fn integer_rem_jit_edge_cases() {
        run_test("a = 17; a % 5");
        run_test("a = 17; a % -5");
        run_test("a = -17; a % 5");
        run_test("a = -17; a % -5");
        run_test("a = 1000; a % 7");
        run_test("a = 0; a % 5");
        // Float rhs (non-Integer rhs falls back to method dispatch)
        run_test("a = 17; a % 5.0");
        // BigInt rhs (non-Integer slot type → method dispatch)
        run_test("a = 17; a % (10**20)");
        // Division by zero raises ZeroDivisionError
        run_test_error("a = 17; a % 0");
    }

    #[test]
    fn integer_rem_pow2_optimization() {
        run_tests(&[
            // Power-of-two RI: rhs is a positive power of 2 literal,
            // mask fits in i32.
            "a = 100; a % 1",
            "a = 100; a % 2",
            "a = 100; a % 4",
            "a = 100; a % 8",
            "a = 100; a % 16",
            "a = 100; a % 1024",
            // negative lhs (two's-complement bitwise AND matches Ruby's floor-mod)
            "a = -1; a % 8",
            "a = -7; a % 8",
            "a = -8; a % 8",
            "a = -100; a % 16",
            "a = -100; a % 1024",
            // zero lhs
            "a = 0; a % 8",
            // lhs is multiple of rhs
            "a = 1024; a % 8",
            "a = -1024; a % 8",
            // rhs near i32 boundary for the mask (mask = rhs*2 - 1)
            "a = 12345678; a % (1 << 30)", // mask = 2^31 - 1, fits in i32
            "a = -12345678; a % (1 << 30)",
            // rhs needs register-loaded mask (mask exceeds i32)
            "a = 12345678; a % (1 << 31)",
            "a = 12345678; a % (1 << 40)",
            "a = -12345678; a % (1 << 31)",
            "a = -12345678; a % (1 << 40)",
            // Non-power-of-two divisor: falls through to generic path
            "a = 100; a % 6",
            "a = 100; a % 10",
            // Negative power-of-two: optimization not applied (rhs > 0 check)
            "a = 100; a % -8",
        ]);
    }

    #[test]
    fn integer_rem_constant_folding() {
        let prelude = "
            def rem_a; 17 % 5; end
            def rem_b; -17 % 5; end
            def rem_c; 17 % -5; end
            def rem_d; -17 % -5; end
            def rem_e; 100 % 8; end
            def rem_f; 0 % 5; end
        ";
        run_test_with_prelude("[rem_a, rem_b, rem_c, rem_d, rem_e, rem_f]", prelude);
    }

    #[test]
    fn float_pow_rem_jit() {
        // Float#** and Float#% go through CFunc_FF_F path (already inline).
        run_test("a = 2.5; a ** 3");
        run_test("a = 2.5; a ** 3.0");
        run_test("a = 2.0; a ** -2");
        run_test("a = 17.5; a % 5.0");
        run_test("a = -17.5; a % 5.0");
        run_test("a = 17.5; a % -5.0");
    }

    #[test]
    fn integer_rem_float_rhs() {
        // Integer % Float: rhs_class = FLOAT_CLASS path.
        // The inline JIT converts lhs to f64 and calls rem_ff.
        run_test("a = 17; b = 5.0; a % b");
        run_test("a = -17; b = 5.0; a % b");
        run_test("a = 17; b = -5.0; a % b");
        run_test("a = -17; b = -5.0; a % b");
        run_test("a = 0; b = 5.0; a % b");
        run_test("a = 17; b = 2.5; a % b");
        run_test("a = 17; b = 1.5; a % b");
        // rhs is a float literal (still through Float-rhs path because the
        // class cache records rhs_class = FLOAT_CLASS)
        run_test("a = 17; a % 5.0");
        run_test("a = -17; a % 5.0");
    }

    #[test]
    fn integer_rem_float_constant_folding() {
        // Both operands literal: should fold at JIT time.
        let prelude = "
            def fa; 17 % 5.0; end
            def fb; -17 % 5.0; end
            def fc; 17 % -5.0; end
            def fd; -17 % -5.0; end
            def fe; 0 % 5.0; end
            def ff; 17 % 2.5; end
            def fg; 17 % 1.5; end
        ";
        run_test_with_prelude("[fa, fb, fc, fd, fe, ff, fg]", prelude);
    }

    #[test]
    fn integer_pow_float_rhs() {
        // Integer ** Float: rhs_class = FLOAT_CLASS path.
        // The result is a regular Float for non-negative base or
        // integer-valued float exponent. (Negative base with non-integer
        // exponent produces Complex via pow_ff, but the existing helper
        // has a known precision drift in its trig-based formula, so we
        // don't test that case here.)
        run_test("a = 2; b = 3.0; a ** b");
        run_test("a = 2; b = 0.5; a ** b");
        run_test("a = 2; b = -3.0; a ** b");
        run_test("a = 0; b = 3.0; a ** b");
        run_test("a = 1; b = 100.0; a ** b");
        // Negative base with integer-valued float: regular Float result
        run_test("a = -2; b = 3.0; a ** b");
        run_test("a = -2; b = 4.0; a ** b");
    }

    #[test]
    fn integer_pow_float_constant_folding() {
        // Both operands literal Float-rhs case: folds when result is plain Float.
        let prelude = "
            def fa; 2 ** 3.0; end
            def fb; 2 ** 0.5; end
            def fc; 2 ** -3.0; end
            def fd; 0 ** 3.0; end
            def fe; 1 ** 100.0; end
            def ff; -(2 ** 3.0); end
            def fg; (-2) ** 4.0; end
        ";
        run_test_with_prelude("[fa, fb, fc, fd, fe, ff, fg]", prelude);
    }

    #[test]
    fn integer_to_s_with_base() {
        run_tests(&[
            "255.to_s(16)",
            "255.to_s(2)",
            "255.to_s(8)",
            "-255.to_s(16)",
            // BigInt
            "(10**20).to_s",
            "(10**20).to_s(16)",
            "(10**20).to_s(2)",
        ]);
    }

    #[test]
    fn integer_bit_length_extended() {
        run_tests(&[
            "0.bit_length",
            "1.bit_length",
            "255.bit_length",
            "-1.bit_length",
            "-128.bit_length",
            // BigInt
            "(10**20).bit_length",
            "(-(10**20)).bit_length",
        ]);
    }

    #[test]
    fn integer_pow_modular() {
        run_tests(&[
            "2.pow(10, 1000)",
            "2.pow(100, 97)",
            // BigInt
            "(10**20).pow(2, 97)",
        ]);
    }

    #[test]
    fn integer_size() {
        run_tests(&["1.size", "(-1).size", "(10**20).size"]);
    }

    #[test]
    fn integer_abs_bigint_extended() {
        run_tests(&["(10**20).abs", "(-(10**20)).abs"]);
    }

    #[test]
    fn integer_zero_nonzero_bigint() {
        run_tests(&[
            "(10**20).zero?",
            "(10**20).nonzero?",
            "0.zero?",
            "0.nonzero?",
        ]);
    }

    #[test]
    fn integer_eql_extended() {
        run_tests(&[
            "1.eql?(1)",
            "1.eql?(1.0)",
            "1.eql?(2)",
            "(10**20).eql?(10**20)",
            "(10**20).eql?(1)",
        ]);
    }

    #[test]
    fn partial_compile_arg_type_guard() {
        run_test_with_prelude(
            "my_div_loop(100, 16.5)",
            r#"
            def my_div_loop(num, base)
              base = base.to_i
              n = num
              while n > 0
                n /= base
              end
              n
            end
            "#,
        );
    }
}
