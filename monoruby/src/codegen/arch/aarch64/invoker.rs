//! aarch64 method/block/fiber invokers and `get_class`.
//!
//! Counterpart of `arch/x86_64/invoker.rs`.

use super::*;

impl JitModule {
    pub(in crate::codegen) fn a64_invoker_prologue(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.stp_pre(EXEC, GLOBALS, SP, -16);
        self.jit.stp_pre(PC, LFP, SP, -16);
        self.jit.stp_pre(ACC, X24, SP, -16);
        self.jit.stp_pre(X25, X26, SP, -16);
        self.jit.mov(EXEC, X0);
        self.jit.mov(GLOBALS, X1);
    }

    /// fdata(X9) = funcinfo_base + (X2 = funcid)*64 + FUNCINFO_DATA.
    pub(in crate::codegen) fn a64_get_func_data_x2(&mut self) {
        self.jit.lsl_imm(X10, X2, 32); // zero-extend funcid
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.lsl_imm(X10, X10, 6); // * size_of::<FuncInfo>() (64)
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X9, X10, FUNCINFO_DATA as u32, 0);
    }

    /// Shared invoker tail (after the frame self/block/outer/meta are set):
    /// argument setup (simple copy or runtime arg massager), call into the
    /// callee, then the epilogue. Assumes fdata in X9, meta in X14, x4 = args
    /// ptr, x5 = len, x7 = kw (Option<Hashmap>).
    pub(in crate::codegen) fn a64_invoker_args_and_call(&mut self) {
        let fdata = X9;
        let generic = self.jit.label();
        let simple_done = self.jit.label();
        let argloop = self.jit.label();
        let aftargs = self.jit.label();
        let error_exit = self.jit.label();
        // Simple iff callee is simple (Meta kind bit 4) AND len == min AND
        // no keyword args; otherwise fall back to the runtime arg massager.
        self.jit.lsr_imm(X10, X14, 56); // kind byte
        self.jit.tbz_label(X10, 4, &generic);
        self.jit.ldrh(X10, fdata, FUNCDATA_MIN as u32);
        self.jit.cmp(X5, X10);
        self.jit.bcond_label(Cond::Ne, &generic);
        self.jit.cbnz_label(X7, &generic); // kw present
        // simple copy (upward): args[0..len] -> callee slots 1..=len
        self.jit.cbz_label(X5, &simple_done);
        self.jit.mov(X10, X5); // down counter = len
        self.jit.neg(X11, X5); // up counter = -len
        self.jit.sub_imm(X12, X4, 8, 0); // args - 8
        self.jit.bind_label(argloop.clone());
        self.jit.add_lsl(X13, X12, X10, 3); // &args[down-1]
        self.jit.ldr(X14, X13, 0);
        self.jit.sub_imm(X15, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.add_lsl(X15, X15, X11, 3);
        self.jit.str(X14, X15, 0);
        self.jit.sub_imm(X10, X10, 1, 0);
        self.jit.add_imm(X11, X11, 1, 0);
        self.jit.cbnz_label(X11, &argloop);
        self.jit.bind_label(simple_done);
        self.jit.b_label(&aftargs);
        // generic: handle_invoker_arguments(exec, globals, callee_lfp,
        // arg_num, args, kw). Reserve scratch below the callee frame and
        // preserve SP (X25) + funcdata (X26, caller-saved) across the call.
        self.jit.bind_label(generic);
        self.jit.mov_sp(X25, SP);
        self.jit.mov(X26, fdata);
        self.jit.ldrh(X10, fdata, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.sub_imm(X2, X25, RSP_LOCAL_FRAME as u32, 0); // callee_lfp
        self.jit.mov(X3, X5); // arg_num = len
        self.jit.mov(X5, X7); // kw
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        // x4 = args (unchanged); upward path => handle_invoker_arguments
        self.jit.mov_imm(X9, runtime::handle_invoker_arguments as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov(X9, X26); // restore fdata
        self.jit.mov_sp(SP, X25); // restore SP
        self.jit.cbz_label(X0, &error_exit);
        self.jit.bind_label(aftargs);
        // call_invoker: push_frame
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0); // [new_cfp] = prev cfp
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32); // exec.cfp = new cfp
        // set_lfp
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        // pc = funcdata.pc; call funcdata.codeptr
        self.jit.ldr(PC, fdata, FUNCDATA_PC as u32);
        self.jit.ldr(X10, fdata, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        // restore exec.cfp = [sp - RSP_CFP] (the prev cfp saved above)
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // Converge with the result (or 0 on error) in x0; the caller emits
        // its own epilogue (standard restore for method/block invokers, or a
        // fiber stack-switch-back for the fiber invoker).
        self.jit.bind_label(error_exit);
    }

    /// Standard invoker epilogue: restore the callee-saved registers saved by
    /// a64_invoker_prologue and return (result in x0).
    pub(in crate::codegen) fn a64_invoker_epilogue(&mut self) {
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(ACC, X24, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Save all callee-saved GPRs (x19-x28) + fp/lr for a fiber context switch.
    pub(in crate::codegen) fn a64_push_callee_save(&mut self) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.stp_pre(X27, X28, SP, -16);
        self.jit.stp_pre(X25, X26, SP, -16);
        self.jit.stp_pre(X23, X24, SP, -16);
        self.jit.stp_pre(X21, X22, SP, -16);
        self.jit.stp_pre(X19, X20, SP, -16);
    }

    /// Restore what a64_push_callee_save saved (reverse order).
    pub(in crate::codegen) fn a64_pop_callee_save(&mut self) {
        self.jit.ldp_post(X19, X20, SP, 16);
        self.jit.ldp_post(X21, X22, SP, 16);
        self.jit.ldp_post(X23, X24, SP, 16);
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(X27, X28, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
    }

    pub(in crate::codegen) fn method_invoker(&mut self) -> MethodInvoker {
        let codeptr = self.jit.get_current_address();
        let fdata = X9;
        // AAPCS64 in: x0 exec, x1 globals, x2 funcid, x3 self, x4 args,
        // x5 len, x6 block, x7 kw (Option<Hashmap>).
        self.a64_invoker_prologue();
        self.a64_get_func_data_x2();
        // frame setup (method): FB = sp - (RSP_LOCAL_FRAME + LFP_SELF).
        let fb = X12;
        self.jit.sub_imm(fb, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.mov_imm(X13, 0);
        self.jit.str(X3, fb, 0); // LFP_SELF  = self
        self.jit.str(X6, fb, 8); // LFP_BLOCK = block
        self.jit.str(X13, fb, 16); // LFP_CME   = 0
        self.jit.str(X13, fb, 24); // LFP_SVAR  = 0
        self.jit.ldr(X14, fdata, FUNCDATA_META as u32);
        self.jit.str(X14, fb, 32); // LFP_META  = funcdata.meta
        self.jit.str(X13, fb, 40); // LFP_OUTER = 0
        self.a64_invoker_args_and_call();
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the MethodInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, MethodInvoker>(&codeptr.as_ptr()) }
    }

    pub(in crate::codegen) fn method_invoker2(&mut self) -> MethodInvoker2 {
        self.a64_stub_fn(0x2)
    }

    /// Block invoker. AAPCS64 in: x0 exec, x1 globals, x2 &ProcData,
    /// x3 self (dummy unless `with_self`), x4 args, x5 len, x6 kw.
    /// `self` for a plain block comes from the captured outer frame.
    /// TODO(aarch64): resolve_invalidated_outer (heap-promoted outer frames).
    /// Set up the callee block frame from a `&ProcData` in x2 (self in x3 when
    /// `with_self`). Produces fdata in X9 and meta in X14, and writes self /
    /// outer / block / cme / svar / meta into the new frame. Shared by the
    /// block and fiber invokers.
    /// TODO(aarch64): resolve_invalidated_outer (heap-promoted outer frames).
    pub(in crate::codegen) fn a64_block_frame_setup(&mut self, with_self: bool) {
        let fdata = X9;
        if with_self {
            // X3 = self_val (block_invoker_with_self ABI). Store at LFP_SELF.
            // No caller-supplied block is forwarded in this variant — the
            // LFP_BLOCK slot is zeroed below.
            self.jit.sub_imm(X10, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
            self.jit.str(X3, X10, 0);
        } else {
            // X3 = block_val (block_invoker ABI: 4th AAPCS64 arg). Stash it
            // into LFP_BLOCK *before* X3 is reused for outer_lfp. Without
            // this the block passed to `Proc#call` (and through proc
            // composition's `f.call(*a, &b)`) is silently dropped, so e.g.
            // `(one >> two).call { |x| ... }` ran with `&arg == nil` in the
            // inner procs.
            self.jit.sub_imm(X10, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
            self.jit.str(X3, X10, 0);
        }
        // outer = [&ProcData + PROCDATA_OUTER]; func_id = [+ PROCDATA_FUNCID]
        self.jit.ldr(X3, X2, runtime::PROCDATA_OUTER as u32); // X3 = outer lfp
        self.jit.ldr32(X2, X2, runtime::PROCDATA_FUNCID as u32); // X2 = func_id
        self.a64_get_func_data_x2(); // fdata = X9 (clobbers X10, X11)
        let fb = X12;
        self.jit.sub_imm(fb, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.mov_imm(X13, 0);
        if !with_self {
            // self = outer.self = [outer - LFP_SELF]
            self.jit.sub_imm(X10, X3, LFP_SELF as u32, 0);
            self.jit.ldr(X10, X10, 0);
            self.jit.str(X10, fb, 0); // LFP_SELF
            // LFP_BLOCK was already populated above with X3 (block_val).
        } else {
            // block_invoker_with_self has no block to forward.
            self.jit.str(X13, fb, 8); // LFP_BLOCK = 0
        }
        self.jit.str(X13, fb, 16); // LFP_CME = 0
        self.jit.str(X13, fb, 24); // LFP_SVAR = 0
        self.jit.ldr(X14, fdata, FUNCDATA_META as u32);
        self.jit.str(X14, fb, 32); // LFP_META
        self.jit.str(X3, fb, 40); // LFP_OUTER = outer
    }

    pub(in crate::codegen) fn a64_block_invoker(&mut self, with_self: bool) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        self.a64_block_frame_setup(with_self);
        self.jit.mov(X7, X6); // shared arg setup expects kw in x7
        self.a64_invoker_args_and_call();
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the BlockInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, BlockInvoker>(&codeptr.as_ptr()) }
    }

    pub(in crate::codegen) fn block_invoker(&mut self) -> BlockInvoker {
        self.a64_block_invoker(false)
    }
    pub(in crate::codegen) fn block_invoker_with_self(&mut self) -> BlockInvoker {
        self.a64_block_invoker(true)
    }

    /// Fiber invoker (first resume). AAPCS64 in: x0 vm, x1 globals,
    /// x2 &ProcData, x3 self, x4 args, x5 len, x6 child_vm (&mut Executor).
    /// Switch onto the fiber's pre-allocated stack, run its block body, then
    /// switch back to the parent on completion. (Mirrors x86 fiber_invoker.)
    pub(in crate::codegen) fn a64_fiber_invoker(&mut self, with_self: bool) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();
        // switch in: save parent regs + SP, jump to the fiber's stack.
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // parent.rsp_save = SP
        self.jit.ldr(X10, X6, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = child.rsp_save (fresh stack top)
        self.jit.str(X0, X6, EXECUTOR_PARENT_FIBER as u32); // child.parent_fiber = parent
        self.jit.mov(EXEC, X6); // EXEC = child_vm
        self.jit.mov(GLOBALS, X1);
        self.a64_block_frame_setup(with_self);
        self.jit.mov_imm(X7, 0); // fibers carry no keyword args
        self.a64_invoker_args_and_call();
        // body completed: mark this fiber terminated, switch back to parent.
        self.jit.mov_imm(X10, u64::MAX); // -1 = terminated
        self.jit.str(X10, EXEC, EXECUTOR_RSP_SAVE as u32);
        self.jit.ldr(EXEC, EXEC, EXECUTOR_PARENT_FIBER as u32);
        self.jit.ldr(X10, EXEC, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10);
        self.a64_pop_callee_save();
        self.jit.ret();
        // SAFETY: codeptr is an extern "C" fn with the FiberInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, FiberInvoker>(&codeptr.as_ptr()) }
    }
    /// Binding invoker: run a func body reusing a captured frame as its LFP
    /// (no new local frame, no args). AAPCS64 in: x0 exec, x1 globals, x2 lfp.
    pub(in crate::codegen) fn binding_invoker(&mut self) -> BindingInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        self.jit.mov(LFP, X2); // reuse the binding's frame as LFP
        self.jit.sub_imm(X2, LFP, LFP_FUNCID as u32, 0);
        self.jit.ldr32(X2, X2, 0); // funcid = [lfp - LFP_FUNCID]
        self.a64_get_func_data_x2(); // X9 = fdata
        // push_frame; cfp.lfp = LFP (the captured frame)
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.ldr(PC, X9, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X9, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // epilogue
        self.jit.ldp_post(X25, X26, SP, 16);
        self.jit.ldp_post(ACC, X24, SP, 16);
        self.jit.ldp_post(PC, LFP, SP, 16);
        self.jit.ldp_post(EXEC, GLOBALS, SP, 16);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
        // SAFETY: codeptr is an extern "C" fn with the BindingInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, BindingInvoker>(&codeptr.as_ptr()) }
    }
    pub(in crate::codegen) fn fiber_invoker(&mut self) -> FiberInvoker {
        self.a64_fiber_invoker(false)
    }
    pub(in crate::codegen) fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
        self.a64_fiber_invoker(true)
    }

    /// Resume a suspended fiber. In: x0 parent_vm, x1 child_vm, x2 value.
    /// Save the parent context, switch onto the child's saved stack, and
    /// return (the child resumes where it last yielded). x86 resume_fiber.
    pub(in crate::codegen) fn resume_fiber(
        &mut self,
    ) -> extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // parent.rsp_save = SP
        self.jit.ldr(X10, X1, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = child.rsp_save
        self.jit.str(X0, X1, EXECUTOR_PARENT_FIBER as u32); // child.parent_fiber = parent
        self.a64_pop_callee_save();
        self.jit.mov(X0, X2); // resume value
        self.jit.ret();
        // SAFETY: codeptr matches the resume_fiber ABI.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }

    /// Yield from a fiber. In: x0 vm (child), x1 value. Save the child
    /// context and switch back to the parent. x86 yield_fiber.
    pub(in crate::codegen) fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();
        self.a64_push_callee_save();
        self.jit.mov_sp(X10, SP);
        self.jit.str(X10, X0, EXECUTOR_RSP_SAVE as u32); // child.rsp_save = SP
        self.jit.ldr(X0, X0, EXECUTOR_PARENT_FIBER as u32); // vm = parent
        self.jit.ldr(X10, X0, EXECUTOR_RSP_SAVE as u32);
        self.jit.mov_sp(SP, X10); // SP = parent.rsp_save
        self.a64_pop_callee_save();
        self.jit.mov(X0, X1); // yielded value
        self.jit.ret();
        // SAFETY: codeptr matches the yield_fiber ABI.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }
    pub(in crate::codegen) fn init_stack_limit(&mut self) -> extern "C" fn(&mut Executor) -> *const u8 {
        // executor.stack_limit = sp - MAX_STACK_SIZE (= 65536 = 16 << 12).
        // x0 = &mut Executor (AAPCS64 arg0).
        let codeptr = self.jit.get_current_address();
        self.jit.mov_sp(X10, SP);
        self.jit.sub_imm(X10, X10, 16, 1); // 16 << 12 = 65536
        self.jit.str(X10, X0, EXECUTOR_STACK_LIMIT as u32);
        self.jit.ret();
        // SAFETY: codeptr is an `extern "C" fn(&mut Executor) -> *const u8`.
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }
    /// `get_class`: x0 = Value in → x0 = ClassId (u32) out. Mirrors the x86
    /// `get_class` tag dispatch. (Invalid receivers trap rather than calling
    /// the x86-gated `illegal_classid`.)
    pub(in crate::codegen) fn get_class(&mut self) -> DestLabel {
        let label = self.jit.label();
        let l1 = self.jit.label();
        let err = self.jit.label();
        let fixnum = self.jit.label();
        let flonum = self.jit.label();
        let symbol = self.jit.label();
        let nil = self.jit.label();
        let bool_ = self.jit.label();
        self.jit.bind_label(label.clone());
        self.jit.tbnz_label(X0, 0, &fixnum); // bit0: fixnum
        self.jit.tbnz_label(X0, 1, &flonum); // bit1: flonum
        self.jit.tbnz_label(X0, 2, &l1); // bit2: other immediate
        self.jit.cbz_label(X0, &err); // 0 is invalid
        self.jit.ldr32(X0, X0, RVALUE_OFFSET_CLASS as u32); // heap: RValue.class
        self.jit.ret();
        self.jit.bind_label(l1);
        self.jit.mov_imm(X1, 0xff);
        self.jit.and_(X2, X0, X1);
        self.jit.cmp_imm(X2, TAG_SYMBOL as u32, 0);
        self.jit.bcond_label(Cond::Eq, &symbol);
        self.jit.cmp_imm(X0, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, &nil);
        self.jit.mov_imm(X1, 8);
        self.jit.orr(X1, X0, X1);
        self.jit.cmp_imm(X1, TRUE_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, &bool_);
        self.jit.bind_label(err);
        self.jit.mov_imm(X0, 0xc1a5); // DIAG: illegal_classid (get_class)
        self.jit
            .mov_imm(X9, crate::codegen::runtime::report_unimpl_op as *const () as u64);
        self.jit.blr(X9);
        self.jit.brk(0); // TODO(aarch64): illegal_classid
        self.jit.bind_label(fixnum);
        self.jit.mov_imm(X0, INTEGER_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(flonum);
        self.jit.mov_imm(X0, FLOAT_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(symbol);
        self.jit.mov_imm(X0, SYMBOL_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(nil);
        self.jit.mov_imm(X0, NIL_CLASS.u32() as u64);
        self.jit.ret();
        self.jit.bind_label(bool_);
        self.jit.mov_imm(X0, BOOL_CLASS.u32() as u64);
        self.jit.ret();
        label
    }
}
