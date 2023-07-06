use crate::*;

//
// Fiber class
//

pub(super) fn init(globals: &mut Globals, klass: ClassId) {
    globals.define_builtin_class_under_obj("Fiber", klass);
    globals.define_builtin_class_func(klass, "new", fiber_new, -1);
    globals.define_builtin_class_func(klass, "yield", fiber_yield, -1);
    globals.define_builtin_func(klass, "resume", resume, 0);
}

///
/// ### Array.new
///
/// - new(size = 0, val = nil) -> Array
/// - new(ary) -> Array
/// - new(size) {|index| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/s/new.html]
///
/// TODO: Support arguments.
#[monoruby_builtin]
fn fiber_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_with_class(vec![], class);
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Ok(obj)
}

///
/// ### Array.new
///
/// - new(size = 0, val = nil) -> Array
/// - new(ary) -> Array
/// - new(size) {|index| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/s/new.html]
///
/// TODO: Support arguments.
#[monoruby_builtin]
fn fiber_yield(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_with_class(vec![], class);
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Ok(obj)
}

///
/// ### Array#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
#[monoruby_builtin]
fn resume(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let len = lfp.self_val().as_array().len();
    Ok(Value::integer(len as i64))
}

#[cfg(test)]
mod test {
    use super::tests::*;
}
