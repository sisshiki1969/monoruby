use super::*;

//
// Boolean methods (internal): `BOOL_CLASS` is *not* a Ruby-visible class.
// `true` and `false` keep their normal `TrueClass` / `FalseClass`
// identities at the user level. The constant only exists as an inline-
// cache tag so a polymorphic-on-true/false call site does not deopt on
// every flip.
//
// To make that tag safe, the methods that are otherwise defined twice
// (once on TrueClass and once on FalseClass with subtly different bodies)
// are registered **once** in Rust and `add_method` is called on **both**
// classes with the same `FuncId`. Subsequent JIT lookups under either
// class therefore resolve to the same `FuncId`, which is the invariant
// the IC relies on.
//

pub(super) fn init(globals: &mut Globals) {
    define_bool_method(globals, "&", and_, 1, 1, false);
    define_bool_method(globals, "|", or_, 1, 1, false);
    define_bool_method(globals, "^", xor_, 1, 1, false);
}

/// Register `address` once and add it as `name` to both `TRUE_CLASS` and
/// `FALSE_CLASS` with the **same** `FuncId`.
fn define_bool_method(
    globals: &mut Globals,
    name: &str,
    address: BuiltinFn,
    min: usize,
    max: usize,
    rest: bool,
) {
    let func_id = globals
        .store
        .new_builtin_func(name, address, min, max, rest, &[], false);
    globals.gen_wrapper(func_id);
    let method_name = IdentId::get_id(name);
    globals
        .store
        .add_method(TRUE_CLASS, method_name, func_id, Visibility::Public);
    globals
        .store
        .add_method(FALSE_CLASS, method_name, func_id, Visibility::Public);
}

///
/// ### TrueClass#& / FalseClass#&
/// - self & other -> bool
///
/// Shared between `true` and `false` so the JIT inline cache observes a
/// single `FuncId` regardless of which boolean the receiver happens to be.
///
#[monoruby_builtin]
fn and_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.self_val().as_bool() {
        Ok(Value::bool(lfp.arg(0).as_bool()))
    } else {
        Ok(Value::bool(false))
    }
}

///
/// ### TrueClass#| / FalseClass#|
/// - self | other -> bool
///
#[monoruby_builtin]
fn or_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.self_val().as_bool() {
        Ok(Value::bool(true))
    } else {
        Ok(Value::bool(lfp.arg(0).as_bool()))
    }
}

///
/// ### TrueClass#^ / FalseClass#^
/// - self ^ other -> bool
///
#[monoruby_builtin]
fn xor_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0).as_bool();
    if lfp.self_val().as_bool() {
        Ok(Value::bool(!other))
    } else {
        Ok(Value::bool(other))
    }
}
