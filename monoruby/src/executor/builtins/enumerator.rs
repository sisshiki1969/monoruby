use crate::*;

use std::sync::Once;

static mut YIELDER: Option<Module> = None;
static YIELDER_INIT: Once = Once::new();

//
// Enumerator class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Enumerator", ENUMERATOR_CLASS);
    globals.define_builtin_class_func(ENUMERATOR_CLASS, "new", enumerator_new, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next", next, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "peek", peek, 0);

    let yielder =
        globals.define_class_by_str("Yielder", ARRAY_CLASS.get_module(globals), ENUMERATOR_CLASS);
    unsafe { YIELDER_INIT.call_once(|| YIELDER = Some(yielder)) }
    //globals.define_builtin_class_func(FIBER_CLASS, "yield", fiber_yield, -1);
    globals.define_builtin_func(yielder.id(), "<<", yielder_shl, 0);
    globals.define_builtin_func(yielder.id(), "yield", yielder_yield, -1);
}

///
/// ### Enumerator.new
///
/// - new(size=nil) {|y| ... } -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/s/new.html]
#[monoruby_builtin]
fn enumerator_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let block_data = vm.get_block_data(globals, bh);
    Ok(Value::new_enumerator(block_data))
}

fn get_next_value(
    vm: &mut Executor,
    globals: &mut Globals,
    enum_: &mut EnumeratorInner,
) -> Result<Value> {
    match enum_.internal.state() {
        FiberState::Created => {
            let yielder = Value::object(unsafe { YIELDER.unwrap().id() });
            enum_.yielder = Some(yielder);
            enum_.internal.init();
            let arg = Arg::from(&yielder);
            vm.invoke_fiber(globals, &mut enum_.internal, arg, 1)
        }
        FiberState::Suspended => vm.resume_fiber(&mut enum_.internal, enum_.yielder.unwrap()),
        FiberState::Terminated => Err(MonorubyErr::stopiterationerr(
            "iteration reached an end".to_string(),
        )),
    }
}

///
/// ### Enumerator#next
///
/// - next -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/next.html]
#[monoruby_builtin]
fn next(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let mut self_val = lfp.self_val();
    let enum_ = self_val.as_enumerator_mut();
    let v = if let Some(v) = std::mem::take(&mut enum_.buffer) {
        v
    } else {
        let v = get_next_value(vm, globals, enum_)?;
        v
    };
    Ok(v)
}

///
/// ### Enumerator#peek
///
/// - peek -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/peek.html]
#[monoruby_builtin]
fn peek(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let mut self_val = lfp.self_val();
    let enum_ = self_val.as_enumerator_mut();
    let v = if let Some(v) = enum_.buffer {
        v
    } else {
        let v = get_next_value(vm, globals, enum_)?;
        enum_.buffer = Some(v);
        v
    };
    Ok(v)
}

///
/// ### Enumerator::Yielder#<<
///
/// - self << object -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aYielder/i/=3c=3c.html]
#[monoruby_builtin]
fn yielder_shl(
    vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 1)?;
    vm.yield_fiber(arg[0])
}

///
/// ### Enumerator::Yielder#yield
///
/// - yield(*object) -> ()
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator=3a=3aYielder/i/yield.html]
#[monoruby_builtin]
fn yielder_yield(
    vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    vm.yield_fiber(Value::array_from_iter(arg.iter(len)))
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn enumerator() {
        run_test(
            r##"
            a = Enumerator.new do |y|
                3.times do |i|
                    y << i
                end
            end
            [a.next, a.peek, a.peek, a.next, a.peek, a.next]
        "##,
        );
        run_test_no_result_check(
            r##"
            a = Enumerator.new do |y|
                3.times do |i|
                    y << i
                end
            end
            [a.inspect, a.to_s]
        "##,
        );
    }

    #[test]
    fn fib() {
        run_test(
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do 
                    y << a
                    a, b = a + b, a
                end
            end
            30.times do fib.next end
            fib.next
        "##,
        );
    }
}
