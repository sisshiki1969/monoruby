use super::*;

impl JitModule {
    pub(super) fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        let const_version = jit.data_i64(1);
        let alloc_flag = jit.data_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });
        let sigint_flag = jit.data_i32(0);
        let entry_raise = jit.label();
        let entry_panic = jit.label();
        let exec_gc = jit.label();
        let f64_to_val = jit.label();
        let stack_overflow = jit.label();

        // dispatch table.
        let entry_unimpl = jit.get_current_address();
        monoasm! { &mut jit,
                movq rdi, rbx;
                movq rsi, r12;
                movzxw rdx, [r13 - 10];
                movq rax, (unimplemented_inst);
                call rax;
                leave;
                ret;
        }
        let dispatch = vec![entry_unimpl; 256];
        let mut j = Self {
            jit,
            class_version,
            const_version,
            alloc_flag,
            sigint_flag,
            entry_raise,
            exec_gc,
            f64_to_val,
            vm_stack_overflow: stack_overflow,
            entry_panic,
            dispatch: dispatch.into_boxed_slice().try_into().unwrap(),
            bop_redefined_flags,
        };
        j.init();
        j.jit.finalize();
        j
    }

    ///
    /// Generate code for GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - stack
    ///
    fn init(&mut self) {
        let raise = self.entry_raise.clone();
        let overflow = self.vm_stack_overflow.clone();
        let leave = self.jit.label();
        let goto = self.jit.label();
        monoasm! { &mut self.jit,
        raise:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LFP_META)];
            movq rcx, r13;
            subq rcx, 16;
            movq rax, (handle_error);
            call rax;
            testq rax, rax;
            jne  goto;
            testq rdx, rdx;
            jz   leave;
            movq rax, rdx;
            jmp  leave;
        goto:
            movq r13, rax;
        }
        self.fetch_and_dispatch();
        monoasm! { &mut self.jit,
        leave:
            leave;
            ret;
        }

        let label = self.entry_panic.clone();
        self.gen_entry_panic(label);

        let label = self.f64_to_val.clone();
        self.gen_f64_to_val(label);

        monoasm! { &mut self.jit,
        overflow:
            movq rdi, rbx;
            movq rax, (stack_overflow);
            call rax;
            jmp raise;
        };

        let label = self.exec_gc.clone();
        monoasm! { &mut self.jit,
        label:
            subq rsp, 8;
        }
        self.save_registers();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (executor::execute_gc);
            call rax;
        }
        self.restore_registers();
        monoasm! { &mut self.jit,
            addq rsp, 8;
            ret;
        }
    }

    ///
    /// Dump stack trace and go panic.
    ///
    /// #### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    fn gen_entry_panic(&mut self, label: DestLabel) {
        monoasm! {&mut self.jit,
        label:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::_dump_stacktrace);
            call rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::panic);
            jmp rax;
            leave;
            ret;
        }
    }

    ///
    /// Convert f64 to Value.
    ///
    /// ### in
    /// - xmm0: f64
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    fn gen_f64_to_val(&mut self, label: DestLabel) {
        let normal = self.label();
        let heap_alloc = self.label();
        monoasm! {&mut self.jit,
        label:
            xorps xmm1, xmm1;
            ucomisd xmm0, xmm1;
            jne normal;
            jp normal;
            movq rax, (FLOAT_ZERO);
            ret;
        normal:
            movq rax, xmm0;
            movq rcx, rax;
            shrq rcx, 60;
            addl rcx, 1;
            andl rcx, 6;
            cmpl rcx, 4;
            jne heap_alloc;
            rolq rax, 3;
            andq rax, (-4);
            orq rax, 2;
            ret;
        heap_alloc:
        // we must save rdi for log_deoptimize.
            subq rsp, 152;
            movq [rsp + 144], r9;
            movq [rsp + 136], r8;
            movq [rsp + 128], rdx;
            movq [rsp + 120], rsi;
            movq [rsp + 112], rdi;
            movq [rsp + 104], xmm15;
            movq [rsp + 96], xmm14;
            movq [rsp + 88], xmm13;
            movq [rsp + 80], xmm12;
            movq [rsp + 72], xmm11;
            movq [rsp + 64], xmm10;
            movq [rsp + 56], xmm9;
            movq [rsp + 48], xmm8;
            movq [rsp + 40], xmm7;
            movq [rsp + 32], xmm6;
            movq [rsp + 24], xmm5;
            movq [rsp + 16], xmm4;
            movq [rsp + 8], xmm3;
            movq [rsp + 0], xmm2;
            movq rax, (Value::float_heap);
            call rax;
            movq xmm2, [rsp + 0];
            movq xmm3, [rsp + 8];
            movq xmm4, [rsp + 16];
            movq xmm5, [rsp + 24];
            movq xmm6, [rsp + 32];
            movq xmm7, [rsp + 40];
            movq xmm8, [rsp + 48];
            movq xmm9, [rsp + 56];
            movq xmm10, [rsp + 64];
            movq xmm11, [rsp + 72];
            movq xmm12, [rsp + 80];
            movq xmm13, [rsp + 88];
            movq xmm14, [rsp + 96];
            movq xmm15, [rsp + 104];
            movq rdi, [rsp + 112];
            movq rsi, [rsp + 120];
            movq rdx, [rsp + 128];
            movq r8, [rsp + 136];
            movq r9, [rsp + 144];
            addq rsp, 152;
            ret;
        }
    }

    ///
    /// Execute GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax, rcx
    /// - stack
    ///
    pub(super) fn execute_gc_inner(&mut self, wb: Option<&jitgen::WriteBack>, error: &DestLabel) {
        let alloc_flag = self.alloc_flag.clone();
        let gc = self.jit.label();
        let exit = self.jit.label();
        let exec_gc = self.exec_gc.clone();
        assert_eq!(0, self.jit.get_page());
        monoasm! { &mut self.jit,
            cmpl [rip + alloc_flag], 8;
            jge  gc;
        exit:
        };
        self.jit.select_page(1);
        self.jit.bind_label(gc);
        if let Some(wb) = wb {
            self.gen_write_back(wb);
        }
        monoasm! { &mut self.jit,
            call exec_gc;
            testq rax, rax;
            jne  exit;
            jmp  error;
        }
        self.jit.select_page(0);
    }

    pub fn jit_check_stack(&mut self, wb: &jitgen::WriteBack, error: &DestLabel) {
        let overflow = self.jit.label();
        assert_eq!(0, self.jit.get_page());
        monoasm! { &mut self.jit,
            cmpq rsp, [rbx + (EXECUTOR_STACK_LIMIT)];
            jle  overflow;
        }
        self.jit.select_page(1);
        self.jit.bind_label(overflow);
        self.gen_write_back(wb);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (stack_overflow);
            call rax;
            jmp error;
        }
        self.jit.select_page(0);
    }
}

extern "C" fn unimplemented_inst(vm: &mut Executor, _: &mut Globals, opcode: u16) -> Option<Value> {
    vm.set_error(MonorubyErr::runtimeerr(format!(
        "[FATAL] internal error: unimplemented instruction. {:04x}",
        opcode
    )));
    None
}

#[repr(C)]
pub(super) struct ErrorReturn {
    dest: Option<BytecodePtr>,
    value: Option<Value>,
}

impl ErrorReturn {
    fn return_err() -> Self {
        Self {
            dest: None,
            value: None,
        }
    }

    fn return_normal(val: Value) -> Self {
        Self {
            dest: None,
            value: Some(val),
        }
    }

    fn goto(dest: BytecodePtr) -> Self {
        Self {
            dest: Some(dest),
            value: None,
        }
    }
}

pub(super) extern "C" fn handle_error(
    vm: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BytecodePtr,
) -> ErrorReturn {
    let func_info = &globals.store[meta.func_id()];
    match &func_info.kind {
        FuncKind::ISeq(info) => {
            let bc_base = globals.store[*info].get_top_pc();
            let pc = pc - bc_base;
            // check exception table.
            let mut lfp = vm.cfp().lfp();
            // First, we check method_return.
            let info = &globals.store[*info];
            if vm.exception().is_none() {
                vm.set_error(MonorubyErr::runtimeerr(
                    "[FATAL] internal error: unknown exception.",
                ));
            }
            if let MonorubyErrKind::MethodReturn(val, target_lfp) = vm.exception().unwrap().kind() {
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    ErrorReturn::goto(bc_base + ensure)
                } else if lfp == *target_lfp {
                    let val = *val;
                    vm.take_error();
                    ErrorReturn::return_normal(val)
                } else {
                    ErrorReturn::return_err()
                };
            }
            let sourceinfo = info.sourceinfo.clone();
            let loc = info.sourcemap[pc.to_usize()];
            let fid = info.func_id();
            vm.push_error_location(loc, sourceinfo, fid);
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
            // First, we check method_return.
            if vm.exception().is_none() {
                vm.set_error(MonorubyErr::runtimeerr(
                    "[FATAL] internal error: unknown exception.",
                ));
            }
            vm.push_internal_error_location(meta.func_id());
        }
        _ => unreachable!(),
    }
    ErrorReturn::return_err()
}
