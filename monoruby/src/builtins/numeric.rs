use super::*;
use paste::paste;

mod complex;
mod float;
mod integer;

//
// Numeric class
//

pub(super) fn init(globals: &mut Globals) {
    let numeric = globals.define_builtin_class_under_obj("Numeric", NUMERIC_CLASS);
    integer::init(globals, numeric);
    float::init(globals, numeric);
    complex::init(globals, numeric);
    globals.define_builtin_func(NUMERIC_CLASS, "+", add, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-", sub, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "*", mul, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "/", div, 1);
    globals.define_builtin_funcs(NUMERIC_CLASS, "%", &["module"], rem, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "**", pow, 1);
    globals.define_builtin_func(NUMERIC_CLASS, "-@", neg, 0);
    globals.define_builtin_func(NUMERIC_CLASS, "+@", pos, 0);
    globals.define_builtin_func(NUMERIC_CLASS, "~", bitnot, 0);
}

macro_rules! binop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
                match super::op::[<$op _values>](vm, globals, lfp.self_val(), lfp.arg(0)) {
                    Some(val) => Ok(val),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        binop!($op1);
        binop!($($op2),+);
    };
}

binop!(add, sub, mul, div, rem, pow);

macro_rules! unop {
    ($op:ident) => {
        paste! {
            #[monoruby_builtin]
            fn $op(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
                match super::op::[<$op _value>](vm, globals, lfp.self_val()) {
                    Some(val) => Ok(val),
                    None => {
                        let err = vm.take_error();
                        Err(err)
                    }
                }
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        unop!($op1);
        unop!($($op2),+);
    };
}

unop!(neg, bitnot);

///
/// ### Integer#+@
///
/// - + self -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Numeric/i/=2b=40.html]
#[monoruby_builtin]
fn pos(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}
