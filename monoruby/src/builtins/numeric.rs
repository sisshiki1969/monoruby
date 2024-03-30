use super::*;

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
}

///
/// ### Integer#+
///
/// - self + other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2b.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match super::op::add_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#-
///
/// - self - other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2d.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match super::op::sub_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#*
///
/// - self * other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2a.html]
#[monoruby_builtin]
fn mul(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match super::op::mul_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#/
///
/// - self / other -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=2f.html]
#[monoruby_builtin]
fn div(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match super::op::div_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}

///
/// ### Integer#%
///
/// - self % other -> Numeric
/// - modulo(other) -> Numeric
///
/// [https://docs.ruby-lang.org/ja/latest/method/Integer/i/=25.html]
#[monoruby_builtin]
fn rem(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match super::op::rem_values(vm, globals, lfp.self_val(), lfp.arg(0)) {
        Some(val) => Ok(val),
        None => {
            let err = vm.take_error();
            Err(err)
        }
    }
}
