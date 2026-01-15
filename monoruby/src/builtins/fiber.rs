use super::*;

//
// Fiber class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Fiber", FIBER_CLASS, ObjTy::FIBER);
    globals.define_builtin_class_func_with_effect(
        FIBER_CLASS,
        "new",
        fiber_new,
        0,
        0,
        Effect::CAPTURE,
    );
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
fn fiber_new(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(bh)?;
    Ok(Value::new_fiber(proc))
}

///
/// ### Fiber.yield
///
/// - yield(*arg = nil) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Fiber/s/yield.html]
#[monoruby_builtin]
fn fiber_yield(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    vm.yield_fiber(val)
}

fn fiber_yield_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        args, pos_num, dst, ..
    } = *callsite;
    let using_xmm = state.get_using_xmm();
    let error = ir.new_error(state);
    ir.xmm_save(using_xmm);
    if pos_num == 0 {
        ir.inline(move |r#gen, _, _| {
            // TODO: we must check if the parent fiber exits.
            monoasm! { &mut r#gen.jit,
                movq rsi, (Value::nil().id());
            }
        });
    } else if pos_num == 1 {
        state.load(ir, args, GP::Rsi);
    } else {
        state.write_back_recv_and_callargs(ir, callsite);
        ir.inline(move |r#gen, _, _| {
            // TODO: we must check if the parent fiber exits.
            monoasm! { &mut r#gen.jit,
                lea rdi, [r14 - (jitgen::conv(args))];
                movq rsi, (pos_num);
                movq rax, (crate::runtime::create_array);
                call rax;
                movq rsi, rax;
            }
        });
    }
    ir.inline(move |r#gen, _, _| {
        let fiber_yield = r#gen.yield_fiber;
        monoasm! { &mut r#gen.jit,
            movq rdi, rbx;
            movq rax, (fiber_yield);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    ir.handle_error(error);
    state.def_rax2acc(ir, dst);
    true
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

#[cfg(test)]
mod tests {
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
