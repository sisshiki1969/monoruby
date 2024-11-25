use super::*;

//
// Fiber class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Fiber", FIBER_CLASS);
    globals.define_builtin_class_func(FIBER_CLASS, "new", fiber_new, 0);
    globals.define_builtin_class_inline_func_rest(
        FIBER_CLASS,
        "yield",
        fiber_yield,
        Box::new(fiber_yield_inline),
    );
    globals.define_builtin_func_rest(FIBER_CLASS, "resume", resume);
}

///
/// ### Fiber.new
///
/// - new {|obj| ... } -> Fiber
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/new.html]
#[monoruby_builtin]
fn fiber_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn fiber_yield(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if vm.parent_fiber().is_none() {
        return Err(MonorubyErr::fibererr(
            "attempt to yield on a not resumed fiber".to_string(),
        ));
    }
    let len = lfp.arg(0).as_array().len();
    let val = if len == 0 {
        Value::nil()
    } else if len == 1 {
        lfp.arg(0).as_array()[0]
    } else {
        lfp.arg(0)
    };
    vm.yield_fiber(globals, val)
}

fn fiber_yield_inline(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BytecodePtr,
) {
    let callsite = &store[callid];
    let CallSiteInfo {
        args, pos_num, dst, ..
    } = *callsite;
    ir.write_back_callargs_and_dst(bb, callsite);
    let using = bb.get_using_xmm();
    let error = ir.new_error(bb, pc);
    ir.inline(move |gen, labels| {
        let error = labels[error];
        let fiber_yield = gen.yield_fiber;
        // TODO: we must check if the parent fiber exits.

        if pos_num == 0 {
            monoasm! { &mut gen.jit,
                movq rsi, (Value::nil().id());
            }
        } else if pos_num == 1 {
            monoasm! { &mut gen.jit,
                movq rsi, [r14 - (jitgen::conv(args))];
            }
        } else {
            monoasm! { &mut gen.jit,
                lea rdi, [r14 - (jitgen::conv(args))];
                movq rsi, (pos_num);
                movq rax, (crate::runtime::create_array);
                call rax;
                movq rsi, rax;
            }
        }

        gen.xmm_save(using);
        monoasm! { &mut gen.jit,
            movq rdi, rbx;
            movq rax, (fiber_yield);
            call rax;
        }
        gen.xmm_restore(using);
        gen.handle_error(error);
    });
    ir.rax2acc(bb, dst);
}

///
/// ### Fiber#resume
///
/// - resume(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/i/resume.html]
#[monoruby_builtin]
fn resume(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_val = Fiber::new(lfp.self_val());
    self_val.resume(vm, globals, lfp)
}

mod test {
    use crate::tests::*;
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
                    answer << "yield = #{Fiber.yield}"
                    answer << "yield = #{Fiber.yield i}"
                    answer << "yield = #{Fiber.yield i, i+1, i+2}"
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
