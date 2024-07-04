use super::*;

mod array;
mod binding;
mod class;
mod dir;
pub(crate) mod enumerator;
mod exception;
mod fiber;
mod file;
mod hash;
mod io;
mod kernel;
mod main_object;
mod math;
mod method;
mod module;
mod numeric;
mod object;
mod proc;
mod process;
mod random;
mod range;
mod regexp;
mod string;
mod struct_class;
mod symbol;
mod time;

use crate::compiler::jitgen::BBContext;
use compiler::jitgen::asmir::*;
pub use enumerator::YIELDER;
pub use monoasm::*;
pub use monoasm_macro::*;
use monoruby_attr::monoruby_builtin;
use num::ToPrimitive;
pub(crate) use object::{object_send, object_send_splat, send};
pub use time::TimeInner;

//
// Builtin methods.
//

pub(crate) fn init_builtins(globals: &mut Globals) {
    object::init(globals);
    globals.define_builtin_class_under_obj("NilClass", NIL_CLASS);
    globals.define_builtin_class_under_obj("TrueClass", TRUE_CLASS);
    globals.define_builtin_class_under_obj("FalseClass", FALSE_CLASS);
    module::init(globals);
    class::init(globals);
    let kernel = kernel::init(globals);
    exception::init(globals);
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
    time::init(globals);
    io::init(globals);
    struct_class::init(globals);
    file::init(globals);
    math::init(globals);
    process::init(globals);
    random::init(globals);
    symbol::init(globals);
    binding::init(globals);
    dir::init(globals);
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

fn parse_f64(s: &str) -> (f64, bool) {
    let mut f = <num::BigInt as num::Zero>::zero();
    let mut e = 0;
    let mut positive = true;
    let mut iter = s.chars().skip_while(|c| c.is_ascii_whitespace()).peekable();

    let c = iter.peek();
    if c == Some(&'+') {
        iter.next().unwrap();
    } else if c == Some(&'-') {
        iter.next().unwrap();
        positive = false;
    }

    while let Some(c) = iter.peek()
        && c.is_ascii_digit()
    {
        let c = iter.next().unwrap();
        f = f * 10 + (c as u32 - '0' as u32);
    }

    if iter.peek() == Some(&'.') {
        iter.next();
        while let Some(c) = iter.peek()
            && c.is_ascii_digit()
        {
            let c = iter.next().unwrap();
            f = f * 10 + (c as u32 - '0' as u32);
            e -= 1;
        }
    }

    let peek = iter.peek();
    if peek == Some(&'e') || peek == Some(&'E') {
        let mut sign = 1i32;
        let mut i = 0;
        iter.next().unwrap();
        let c = iter.peek();
        if c == Some(&'+') {
            iter.next().unwrap();
        } else if c == Some(&'-') {
            iter.next().unwrap();
            sign = -1;
        }
        while let Some(c) = iter.peek()
            && c.is_ascii_digit()
        {
            let c = iter.next().unwrap();
            i = i * 10 + (c as u32 - '0' as u32);
        }
        e += (i as i32) * sign;
    }

    while e > 0 {
        f *= 10;
        e -= 1;
    }

    let mut f = f.to_f64().unwrap();
    if e < 0 {
        f /= 10.0f64.powi(-e);
    }
    let err = iter.peek().is_some();
    if positive {
        (f, err)
    } else {
        (-f, err)
    }
}
