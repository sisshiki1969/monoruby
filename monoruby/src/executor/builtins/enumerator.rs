use crate::*;

use std::sync::Once;

pub static mut YIELDER: Option<Module> = None;
static YIELDER_INIT: Once = Once::new();

//
// Enumerator class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Enumerator", ENUMERATOR_CLASS);
    globals.define_builtin_class_func(ENUMERATOR_CLASS, "new", enumerator_new, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "next", next, 0);
    globals.define_builtin_func(ENUMERATOR_CLASS, "each", each, -1);
    globals.define_builtin_func(ENUMERATOR_CLASS, "peek", peek, 0);

    let yielder =
        globals.define_class_by_str("Yielder", ARRAY_CLASS.get_module(globals), ENUMERATOR_CLASS);
    unsafe { YIELDER_INIT.call_once(|| YIELDER = Some(yielder)) }
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
    lfp.expect_block()?;
    vm.move_caller_frames_to_heap();
    let block_data = globals.get_block_data(vm.cfp());
    Ok(Value::new_generator(block_data))
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
    lfp.self_val().as_enumerator_mut().next(vm, globals)
}

///
/// ### Enumerator#each
///
/// - each {...} -> object
/// - each -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerator/i/each.html]
#[monoruby_builtin]
fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let data = if lfp.block().is_some() {
        globals.get_block_data(vm.cfp())
    } else {
        return Ok(lfp.self_val());
    };
    match lfp
        .self_val()
        .as_enumerator_mut()
        .iterate(vm, globals, &data)
    {
        Ok(val) => Ok(val),
        Err(err) => {
            if err.kind() == &MonorubyErrKind::StopIteration {
                Ok(lfp.self_val())
            } else {
                Err(err)
            }
        }
    }
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
    lfp.self_val().as_enumerator_mut().peek(vm, globals)
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
    fn enum_free() {
        run_test(
            r##"
            1000.times do
                a = Enumerator.new do |y|
                    3.times do |i|
                        y << i
                    end
                end
                a.next
                a.next
            end
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

    #[test]
    fn fib_each() {
        run_test(
            r##"
            fib = Enumerator.new do |y|
                a = b = 1
                loop do 
                    y << a
                    a, b = a + b, a
                    if a > 30 then break end
                end
            end
            ans = []
            ans << fib.each {|x| ans << x}
        "##,
        );
    }

    #[test]
    fn each() {
        run_test(
            r##"
            res = []
            e = [1,2,3,4].to_enum
            e.each do |x|
                res << x.to_s
            end
            res
        "##,
        );
    }
}
