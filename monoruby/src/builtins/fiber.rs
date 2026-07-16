use super::*;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};

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
        1,
        Effect::CAPTURE,
    );
    globals.define_builtin_class_inline_func_rest(
        FIBER_CLASS,
        "yield",
        fiber_yield,
        inline_gen2!(fiber_yield_inline),
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
fn fiber_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(globals, bh, pc)?;
    Ok(Value::new_fiber(proc))
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
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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

/// Out-of-line error path for the inlined `Fiber.yield`: yielding with
/// no parent fiber (the main fiber, or a green thread's root) raises
/// FiberError instead of switching through a null parent pointer.
pub(crate) extern "C" fn fiber_yield_no_parent(vm: &mut Executor) -> Option<Value> {
    vm.set_error(MonorubyErr::fibererr(
        "attempt to yield on a not resumed fiber".to_string(),
    ));
    None
}

fn fiber_yield_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        args, pos_num, dst, ..
    } = *callsite;
    // aarch64 reaches the args via `sub x0, lfp, #conv(args)` (bounded
    // immediate); bail to the slow path on an out-of-range frame.
    #[cfg(target_arch = "aarch64")]
    if pos_num > 1 && jitgen::conv(args) as u32 > 4095 {
        return false;
    }
    // `Fiber.yield` suspends this fiber: control returns to the resumer
    // and arbitrary code (including GCs) runs while this frame stays live
    // on the fiber's stack. Unlike a normal call, the inlined yield does
    // not otherwise spill register/literal-resident slots, so the
    // suspended frame would not be GC-complete and a collection in the
    // resumer could free a value the frame still holds (e.g. an
    // interpolation operand). Write the frame back (the standard GC
    // safepoint) before yielding so every live slot is materialised.
    state.exec_gc(ir, false);
    let using_fpr = state.get_using_fpr(ir);
    let error = ir.new_error(state);
    ir.fpr_save(using_fpr);
    if pos_num == 0 {
        ir.inline(move |r#gen, _, _, _| r#gen.emit_fiber_yield_value_nil());
    } else if pos_num == 1 {
        state.load(ir, args, GP::Rsi);
    } else {
        state.write_back_recv_and_callargs(ir, callsite);
        let args_off = jitgen::conv(args) as usize;
        ir.inline(move |r#gen, _, _, _| r#gen.emit_fiber_yield_value_array(args_off, pos_num));
    }
    ir.inline(move |r#gen, _, _, _| {
        let yield_fiber = r#gen.yield_fiber as *const () as u64;
        let no_parent = fiber_yield_no_parent as *const () as u64;
        r#gen.emit_fiber_yield_call(yield_fiber, no_parent)
    });
    ir.fpr_restore(using_fpr);
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
fn resume(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    fn fiber_resume_self_or_ancestor() {
        // Resuming the currently-running fiber, or a still-active
        // ancestor on the resume chain, must raise `FiberError` (was
        // SIGSEGV from native-stack switching into a live frame).
        // CRuby:
        //   self-resume      ⇒ "attempt to resume the current fiber"
        //   ancestor-resume  ⇒ "attempt to resume a resumed fiber (double resume)"
        run_test_error(
            r#"
            f = nil
            f = Fiber.new { f.resume }
            f.resume
        "#,
        );
        run_test_error(
            r#"
            outer = nil
            inner = nil
            outer = Fiber.new {
              inner = Fiber.new { outer.resume }
              inner.resume
            }
            outer.resume
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

    #[test]
    fn fiber_current() {
        run_test("Fiber.current.is_a?(Fiber)");
    }

    /// Fiber-local storage class-method sugar (`Fiber[:k]` /
    /// `Fiber[:k] = v`) and the Symbol-keyed contract.
    #[test]
    fn fiber_storage_class_sugar() {
        run_test(
            r##"
            Fiber[:a] = 1
            Fiber[:b] = "x"
            [Fiber.current.storage, Fiber[:a], Fiber[:missing]]
            "##,
        );
        run_test_error(r##"Fiber["str_key"] = 1"##);
        run_test_error(r##"Fiber["str_key"]"##);
    }

    /// `Fiber#storage=` replaces / clears; non-Hash raises.
    #[test]
    fn fiber_storage_assign() {
        run_test(
            r##"
            f = Fiber.current
            f.storage = {c: 3}
            r = f.storage
            f.storage = nil
            [r, f.storage]
            "##,
        );
        run_test_error(r##"Fiber.current.storage = 1"##);
    }

    /// `Fiber.set_scheduler(nil)` / `Fiber.scheduler` round-trip;
    /// `Fiber.current_scheduler` stays nil because monoruby fibers
    /// are always blocking; `Fiber.blocking?` / `Fiber#blocking?`
    /// return `1`.
    ///
    /// Only `nil` is round-tripped here because CRuby's
    /// `set_scheduler` validates the scheduler interface
    /// (`#block`, `#kernel_sleep`, …), which monoruby's stub does
    /// not enforce. Interface validation is intentional follow-up.
    #[test]
    fn fiber_scheduler_blocking() {
        run_test_once(
            r##"
            res = []
            res << Fiber.scheduler
            res << Fiber.set_scheduler(nil)
            res << Fiber.scheduler
            res << Fiber.current_scheduler
            res << Fiber.blocking?
            res << Fiber.current.blocking?
            res
            "##,
        );
    }
}
