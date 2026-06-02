use super::*;
#[cfg(jit_x86)]
use monoasm_macro::monoasm;

impl JitModule {
    #[cfg(jit_x86)]
    pub fn jit_check_stack(&mut self, wb: &jitgen::WriteBack, error: &DestLabel, base: usize) {
        let overflow = self.jit.label();
        assert_eq!(0, self.jit.get_page());
        monoasm! { &mut self.jit,
            cmpq rsp, [rbx + (EXECUTOR_STACK_LIMIT)];
            jle  overflow;
        }
        self.jit.select_page(1);
        self.jit.bind_label(overflow);
        self.gen_write_back(wb, base);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (stack_overflow);
            call rax;
            jmp error;
        }
        self.jit.select_page(0);
    }
}

pub(in crate::codegen) extern "C" fn unimplemented_inst(
    vm: &mut Executor,
    _: &mut Globals,
    opcode: u16,
) -> Option<Value> {
    vm.set_error(MonorubyErr::fatal(format!(
        "internal error: unimplemented instruction. {:04x}",
        opcode
    )));
    None
}

#[repr(C)]
pub(super) struct ErrorReturn {
    value: Option<Value>,
    dest: Option<BytecodePtr>,
}

impl ErrorReturn {
    fn return_err() -> Self {
        Self {
            value: None,
            dest: None,
        }
    }

    fn return_normal(val: Value) -> Self {
        Self {
            value: Some(val),
            dest: None,
        }
    }

    fn goto(dest: BytecodePtr) -> Self {
        Self {
            value: None,
            dest: Some(dest),
        }
    }
}

pub(super) extern "C" fn handle_error(
    vm: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BytecodePtr,
) -> ErrorReturn {
    if vm.exception().is_none() {
        runtime::_dump_stacktrace(vm, globals);
        vm.set_error(MonorubyErr::fatal(
            "internal error: unknown exception.",
        ));
    }
    let func_info = &globals.store[meta.func_id()];
    match &func_info.kind {
        FuncKind::ISeq(info) => {
            let bc_base = globals.store[*info].get_top_pc();
            // check Retry/Redo first.
            // Retry/Redo opcode encodes disp in the lower 32 bits of op1.
            // handle_error receives pc pointing to the retry/redo instruction itself
            // (entry_raise subtracts 16 from r13).
            // dest = pc + 1 + disp
            if let MonorubyErrKind::Retry | MonorubyErrKind::Redo = vm.exception().unwrap().kind() {
                vm.take_error();
                let disp = pc.op1() as i32;
                let dest = pc + (1 + disp as isize);
                return ErrorReturn::goto(dest);
            }
            let pc = pc - bc_base;
            let mut lfp = vm.cfp().lfp();
            let info = &globals.store[*info];
            // check exception table.
            // First, we check method_return.
            let method_return = match vm.exception().unwrap().kind() {
                MonorubyErrKind::MethodReturn(val, target_lfp) => Some((*val, *target_lfp)),
                _ => None,
            };
            if let Some((val, target_lfp)) = method_return {
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    // Suspend the non-local return across the ensure body so
                    // the body can `raise` (set_error) without tripping the
                    // empty-exception guard; `EnsureEnd` restores it.
                    vm.defer_unwind(lfp);
                    ErrorReturn::goto(bc_base + ensure)
                } else if lfp == target_lfp || meta.is_proc_method() {
                    // Stop unwinding either at the matching target LFP
                    // or at a `define_method` proc-method boundary, so
                    // that `return` inside a wrapped block exits the
                    // method rather than escaping to the block's
                    // lexical enclosing scope.
                    vm.take_error();
                    // This frame returns now; abandon any deferral it owns.
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_normal(val)
                } else {
                    // Propagating past this frame; its deferral (if any) can
                    // no longer reach its `EnsureEnd`.
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_err()
                };
            }
            if let MonorubyErrKind::Throw(..) = vm.exception().unwrap().kind() {
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    vm.defer_unwind(lfp);
                    ErrorReturn::goto(bc_base + ensure)
                } else {
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_err()
                };
            }
            let sourceinfo = info.sourceinfo.clone();
            let loc = info.sourcemap[pc.to_usize()];
            let fid = info.func_id();
            vm.push_error_location(loc, sourceinfo, fid);
            // Fatal errors (Rust panics caught at an `extern "C"` boundary)
            // must never be caught by `rescue`, and we also skip `ensure`
            // since the interpreter/VM state may be inconsistent after a
            // panic. Propagate straight up to the top.
            if vm.exception().unwrap().is_fatal() {
                vm.discard_deferred_unwind(lfp);
                return ErrorReturn::return_err();
            }
            if let Some((Some(rescue), _, err_reg)) = info.get_exception_dest(pc) {
                let err_val = vm.take_ex_obj(globals);
                globals.set_gvar(IdentId::get_id("$!"), err_val);
                if let Some(err_reg) = err_reg {
                    unsafe { lfp.set_register(err_reg, Some(err_val)) };
                }
                return ErrorReturn::goto(bc_base + rescue);
            }
        }
        FuncKind::Builtin { .. } => {
            let lfp = vm.cfp().lfp();
            // First, we check method_return.
            if let MonorubyErrKind::MethodReturn(val, target_lfp) = vm.exception().unwrap().kind() {
                return if lfp == *target_lfp {
                    let val = *val;
                    vm.take_error();
                    ErrorReturn::return_normal(val)
                } else {
                    ErrorReturn::return_err()
                };
            }
            if let MonorubyErrKind::Throw(..) = vm.exception().unwrap().kind() {
                return ErrorReturn::return_err();
            }
            vm.push_internal_error_location(meta.func_id());
        }
        _ => unreachable!(),
    }
    // A normal exception is propagating out of this frame: its `EnsureEnd`
    // (if any) will not run, so drop a deferral it may still own.
    let lfp = vm.cfp().lfp();
    vm.discard_deferred_unwind(lfp);
    ErrorReturn::return_err()
}
