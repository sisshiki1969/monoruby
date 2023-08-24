use crate::*;

//
// Fiber class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Fiber", FIBER_CLASS);
    globals.define_builtin_class_func(FIBER_CLASS, "new", fiber_new);
    globals.define_builtin_class_func(FIBER_CLASS, "yield", fiber_yield);
    globals.define_builtin_func(FIBER_CLASS, "resume", resume);
}

///
/// ### Fiber.new
///
/// - new {|obj| ... } -> Fiber
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/new.html]
#[monoruby_builtin]
fn fiber_new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh)?;
    Ok(Value::new_fiber(proc))
}

///
/// ### Fiber.yield
///
/// - yield(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/yield.html]
#[monoruby_builtin]
fn fiber_yield(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
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
        Value::array_from_iter(lfp.iter())
    };
    vm.yield_fiber(globals, val)
}

///
/// ### Fiber#resume
///
/// - resume(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/resume.html]
#[monoruby_builtin]
fn resume(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let mut self_val: Fiber = lfp.self_val().into();
    self_val.resume(vm, globals, lfp)
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

    #[test]
    fn fiber_closure() {
        run_test_with_prelude(
            r#"
            create_fiber.resume
        "#,
            r#"
            def create_fiber
              a = 100
              Fiber.new do
                Fiber.yield a
              end
            end
        "#,
        )
    }

    #[test]
    fn fib() {
        run_test(
            r##"
            fib = Fiber.new do
                a = b = 1
                loop do 
                    Fiber.yield a
                    a, b = a + b, a
                end
            end
            
            30.times do fib.resume end
            fib.resume
        "##,
        );
    }
}
