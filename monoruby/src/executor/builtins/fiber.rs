use crate::*;

//
// Fiber class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Fiber", FIBER_CLASS);
    globals.define_builtin_class_func(FIBER_CLASS, "new", fiber_new, 0);
    globals.define_builtin_class_func(FIBER_CLASS, "yield", fiber_yield, -1);
    globals.define_builtin_func(FIBER_CLASS, "resume", resume, -1);
}

///
/// ### Fiber.new
///
/// - new {|obj| ... } -> Fiber
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/new.html]
#[monoruby_builtin]
fn fiber_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let block_data = vm.get_block_data(globals, bh);
    Ok(Value::new_fiber(block_data))
}

///
/// ### Fiber.yield
///
/// - yield(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/yield.html]
#[monoruby_builtin]
fn fiber_yield(
    vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    if vm.parent_fiber.is_none() {
        return Err(MonorubyErr::fibererr(
            "attempt to yield on a not resumed fiber".to_string(),
        ));
    }
    let val = if len == 0 {
        Value::nil()
    } else if len == 1 {
        arg[0]
    } else {
        Value::array_from_iter(arg.iter(len))
    };
    vm.yield_fiber(val)
}

///
/// ### Fiber#resume
///
/// - resume(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/resume.html]
#[monoruby_builtin]
fn resume(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let fiber = self_val.as_fiber_mut();
    match fiber.state() {
        FiberState::Created => {
            fiber.init();
            vm.invoke_fiber(globals, fiber, arg, len)
        }
        FiberState::Terminated => Err(MonorubyErr::fibererr(
            "attempt to resume a terminated fiber".to_string(),
        )),
        FiberState::Suspended => {
            let val = if len == 0 {
                Value::nil()
            } else if len == 1 {
                arg[0]
            } else {
                Value::array_from_iter(arg.iter(len))
            };
            vm.resume_fiber(fiber, val)
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;
    #[test]
    fn fiber_error() {
        run_test_error("Fiber.yield");
        run_test_error(
            r#"
            f = Fiber.new do
            end
            f.resume
            f.resume
        "#,
        );
    }

    #[test]
    fn fiber() {
        run_test(
            r##"
            answer = []
            f = Fiber.new do
                outer = 42
                answer << "invoked #{outer}"
                30.times {|i|
                    answer << "yield = #{Fiber.yield i}"
                }
                "terminated #{outer}"
            end
            31.times do |i|
              answer << "resume = #{f.resume i}"
            end
            answer
        "##,
        );
    }
}
