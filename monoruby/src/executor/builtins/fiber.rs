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
        None => Err(vm.take_error()),
    }
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
    use std::alloc::*;
    let self_val = lfp.self_val();
    let FiberInner { handle, block_data } = self_val.as_fiber();
    let res = match unsafe { (**handle).rsp_save } {
        None => {
            let layout = Layout::from_size_align(8192, 8192).unwrap();
            unsafe {
                let ptr = alloc(layout).add(8192);
                (**handle).rsp_save = Some(std::ptr::NonNull::new(ptr).unwrap());
            }
            (globals.codegen.fiber_invoker)(vm, globals, block_data as _, *handle, arg.0, len)
        }
        Some(ptr) if ptr.as_ptr() as i64 == -1 => {
            return Err(MonorubyErr::fibererr(
                "attempt to resume a terminated fiber".to_string(),
            ))
        }
        Some(_) => {
            let val = if len == 0 {
                Value::nil()
            } else if len == 1 {
                arg[0]
            } else {
                Value::array_from_iter(arg.iter(len))
            };
            resume_fiber(vm as _, *handle, val)
        }
    };
    match res {
        Some(val) => Ok(val),
        None => Err(vm.take_error()),
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
    }
}
