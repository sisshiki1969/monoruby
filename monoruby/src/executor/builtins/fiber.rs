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
    match yield_fiber(vm as _, val) {
        Some(res) => Ok(res),
        None => Err(unsafe { vm.parent_fiber.unwrap().as_mut().take_error() }),
    }
}

const FIBER_STACK_SIZE: usize = 8192 * 8;

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
    use std::alloc::*;
    let self_val = lfp.self_val();
    let FiberInner {
        mut handle,
        block_data,
        ..
    } = self_val.as_fiber();
    let res = match unsafe { handle.as_ref().fiber_state() } {
        FiberState::Created => {
            let layout = Layout::from_size_align(FIBER_STACK_SIZE, 4096).unwrap();
            unsafe {
                let stack_bottom = alloc(layout);
                libc::mprotect(stack_bottom as _, 4096, libc::PROT_NONE);
                let stack_top = stack_bottom.add(FIBER_STACK_SIZE);
                handle.as_mut().rsp_save = Some(std::ptr::NonNull::new(stack_top).unwrap());
            }
            (globals.codegen.fiber_invoker)(
                vm,
                globals,
                block_data as _,
                handle.as_ptr(),
                arg.0,
                len,
            )
        }
        FiberState::Terminated => {
            return Err(MonorubyErr::fibererr(
                "attempt to resume a terminated fiber".to_string(),
            ))
        }
        FiberState::Suspended => {
            let val = if len == 0 {
                Value::nil()
            } else if len == 1 {
                arg[0]
            } else {
                Value::array_from_iter(arg.iter(len))
            };
            resume_fiber(vm as _, handle.as_ptr(), val)
        }
    };
    match res {
        Some(val) => Ok(val),
        None => Err(unsafe { handle.as_mut().take_error() }),
    }
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn resume_fiber(vm: *mut Executor, child: *mut Executor, val: Value) -> Option<Value> {
    unsafe {
        std::arch::asm!(
            "push r15",
            "push r14",
            "push r13",
            "push r12",
            "push rbx",
            "push rbp",
            "mov  [rdi + 16], rsp", // [vm.rsp_save] <- rsp
            "mov  rsp, [rsi + 16]", // rsp <- [child_vm.rsp_save]
            "mov  [rsi + 24], rdi", // [child_vm.parent_fiber] <- vm
            "pop  rbp",
            "pop  rbx",
            "pop  r12",
            "pop  r13",
            "pop  r14",
            "pop  r15",
            "mov  rax, rdx",
            "ret",
            options(noreturn)
        );
    }
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn yield_fiber(vm: *mut Executor, val: Value) -> Option<Value> {
    unsafe {
        std::arch::asm!(
            "push r15",
            "push r14",
            "push r13",
            "push r12",
            "push rbx",
            "push rbp",
            "mov  [rdi + 16], rsp", // [vm.rsp_save] <- rsp
            "mov  rdi, [rdi + 24]", // rdi <- [vm.parent_fiber]
            "mov  rsp, [rdi + 16]", // rsp <- [parent.rsp_save]
            "pop  rbp",
            "pop  rbx",
            "pop  r12",
            "pop  r13",
            "pop  r14",
            "pop  r15",
            "mov  rax, rsi",
            "ret",
            options(noreturn)
        );
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
