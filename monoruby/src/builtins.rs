use super::*;

mod arithmetic_sequence;
mod array;
mod binding;
mod bool_class;
mod class;
mod dir;
pub(crate) mod encoding;
pub(crate) mod errno;
#[cfg(test)]
mod encoding_tests;
pub(crate) mod enumerator;
mod exception;
mod false_class;
mod ffi;
mod fiber;
mod fiddle;
mod file;
mod gc;
mod hash;
mod io;
mod json;
mod kernel;
mod main_object;
mod marshal;
mod match_data;
mod math;
mod method;
mod module;
mod nil_class;
mod numeric;
mod object;
mod proc;
mod process;
mod random;
mod range;
mod regexp;
mod set;
mod string;
pub(crate) mod struct_class;
mod symbol;
mod time;
mod true_class;

use crate::codegen::jitgen::AbstractState;
use codegen::jitgen::asmir::*;
pub use enumerator::YIELDER;
pub use monoasm::*;
pub use monoasm_macro::*;
use monoruby_attr::monoruby_builtin;
use num::ToPrimitive;
pub(crate) use kernel::{object_send, parse_kernel_float, parse_kernel_integer, send};
pub use time::TimeInner;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    object::init(globals);
    let module = globals.define_builtin_class_under_obj("Module", MODULE_CLASS, ObjTy::MODULE);
    globals.define_builtin_class("Class", CLASS_CLASS, module, OBJECT_CLASS, ObjTy::CLASS);
    true_class::init(globals);
    false_class::init(globals);
    nil_class::init(globals);
    bool_class::init(globals);
    module::init(globals);
    class::init(globals);

    let kernel = kernel::init(globals);
    exception::init(globals);
    errno::init(globals);
    numeric::init(globals);
    string::init(globals);
    array::init(globals);
    hash::init(globals);
    regexp::init(globals);
    range::init(globals);
    proc::init(globals);
    method::init(globals);
    fiber::init(globals);
    enumerator::init(globals);
    arithmetic_sequence::init(globals);
    time::init(globals);
    io::init(globals);
    struct_class::init(globals);
    file::init(globals);
    fiddle::init(globals);
    ffi::init(globals);
    marshal::init(globals);
    math::init(globals);
    process::init(globals);
    gc::init(globals);
    random::init(globals);
    symbol::init(globals);
    binding::init(globals);
    dir::init(globals);
    match_data::init(globals);
    set::init(globals);
    json::init(globals);
    main_object::init(globals);
    globals.object_class().include_module(kernel).unwrap();
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Arg(*const Option<Value>);

impl std::ops::Index<usize> for Arg {
    type Output = Option<Value>;
    fn index(&self, index: usize) -> &Option<Value> {
        unsafe { &*self.0.sub(index) }
    }
}

impl std::ops::Add<usize> for Arg {
    type Output = Arg;
    fn add(self, rhs: usize) -> Arg {
        Arg(unsafe { self.0.sub(rhs) })
    }
}

/// Parse `s` as a floating-point number using CRuby's `String#to_f`
/// rules: leading whitespace and an optional sign are allowed,
/// underscores may sit *between* two digits (a leading `_` or `__`
/// terminates the run), and any trailing junk is silently dropped.
/// Returns `(value, had_trailing_junk)` so callers like
/// `Kernel#Float` can reject the strict-mode case while `String#to_f`
/// ignores it.
///
/// Internally we walk the input once, build a normalized
/// `[+-]?DIGITS[.DIGITS][eEXP]` string with the underscores stripped,
/// and hand the result to Rust's `f64::from_str` so the final
/// conversion uses the same correctly-rounded path Ruby relies on.
fn parse_f64(s: &str) -> (f64, bool) {
    let trimmed = s.trim_start_matches(|c: char| c.is_ascii_whitespace());
    let mut iter = trimmed.chars().peekable();
    let mut buf = String::with_capacity(trimmed.len());

    if let Some(&c) = iter.peek() {
        if c == '+' || c == '-' {
            buf.push(c);
            iter.next();
        }
    }

    /// Consume a digit run, tolerating `_` between two digit
    /// characters. Returns true if at least one digit was consumed.
    fn consume_digits(
        iter: &mut std::iter::Peekable<std::str::Chars<'_>>,
        out: &mut String,
    ) -> bool {
        let mut any = false;
        loop {
            match iter.peek() {
                Some(c) if c.is_ascii_digit() => {
                    out.push(*c);
                    iter.next();
                    any = true;
                }
                Some(&'_') if any => {
                    // `_` is only accepted when the previous char was
                    // a digit *and* the next is also a digit. Peek
                    // ahead to verify the trailing side; on failure
                    // (e.g. `1_e10` or `1__0`) leave the `_` in place
                    // so the outer parser stops here.
                    let mut clone = iter.clone();
                    clone.next();
                    if matches!(clone.peek(), Some(c) if c.is_ascii_digit()) {
                        iter.next();
                        continue;
                    }
                    break;
                }
                _ => break,
            }
        }
        any
    }

    let any_int = consume_digits(&mut iter, &mut buf);

    if iter.peek() == Some(&'.') {
        // Only consume the dot if a digit follows. `"1.".to_f == 1.0`
        // but `"1.foo".to_f` stops at the integer run.
        let mut clone = iter.clone();
        clone.next();
        if matches!(clone.peek(), Some(c) if c.is_ascii_digit()) {
            buf.push('.');
            iter.next();
            consume_digits(&mut iter, &mut buf);
        }
    }

    if matches!(iter.peek(), Some(&'e') | Some(&'E')) {
        let mut clone = iter.clone();
        clone.next();
        if matches!(clone.peek(), Some(&'+') | Some(&'-')) {
            clone.next();
        }
        if matches!(clone.peek(), Some(c) if c.is_ascii_digit()) {
            buf.push('e');
            iter.next();
            if matches!(iter.peek(), Some(&'+') | Some(&'-')) {
                buf.push(*iter.peek().unwrap());
                iter.next();
            }
            consume_digits(&mut iter, &mut buf);
        }
    }

    let err = iter.peek().is_some();
    let f = if any_int || buf.contains('.') {
        // Strip the optional leading sign for the actual parse — Rust's
        // `f64::from_str` accepts it but we want the all-zero short
        // circuit to avoid `-0`-vs-`+0` weirdness on empty mantissas.
        buf.parse::<f64>().unwrap_or(0.0)
    } else {
        0.0
    };
    (f, err)
}
