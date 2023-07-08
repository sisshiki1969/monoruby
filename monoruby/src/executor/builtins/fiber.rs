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
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let parent = vm.parent_fiber;
    if parent.is_null() {
        return Err(MonorubyErr::fibererr(
            "attempt to yield on a not resumed fiber".to_string(),
        ));
    }
    yield_fiber(vm as _, parent as _);
    Ok(Value::nil())
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
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let FiberInner { handle, block_data } = self_val.as_fiber();
    if unsafe { (**handle).rsp_save.is_null() } {
        invoke_fiber(vm as _, *handle, block_data as _);
    } else {
        resume_fiber(vm as _, *handle);
    }
    Ok(Value::nil())
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn invoke_fiber(vm: *mut Executor, child: *mut Executor, block_data: *const BlockData) {
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
            "ret",
            options(noreturn)
        );
    }
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn resume_fiber(vm: *mut Executor, child: *mut Executor) {
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
            "ret",
            options(noreturn)
        );
    }
}

#[cfg(not(tarpaulin_include))]
#[naked]
extern "C" fn yield_fiber(vm: *mut Executor, parent: *mut Executor) {
    unsafe {
        std::arch::asm!(
            "push r15",
            "push r14",
            "push r13",
            "push r12",
            "push rbx",
            "push rbp",
            "mov  [rdi + 16], rsp", // [vm.rsp_save] <- rsp
            "mov  rsp, [rsi + 16]", // rsp <- [parent.rsp_save]
            "pop  rbp",
            "pop  rbx",
            "pop  r12",
            "pop  r13",
            "pop  r14",
            "pop  r15",
            "ret",
            options(noreturn)
        );
    }
}

#[cfg(test)]
mod test {
    //use super::tests::*;
}
