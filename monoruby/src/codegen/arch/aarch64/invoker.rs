//! aarch64 method/block/fiber invokers and `get_class`.
//!
//! Counterpart of `arch/x86_64/invoker.rs`.

use super::*;
use monoasm_macro::monoasm_arm64;

impl JitModule {
    /// Save the AAPCS64 callee-saved FP registers D8-D15 (64 bytes) below sp.
    /// The JIT uses the whole FP pool D2-D15 as scratch (see `a64_fpr`), so
    /// every Rust↔JIT boundary must preserve D8-D15 for the Rust caller.
    /// `monoasm_arm64`'s `stp`/`ldp` are GPR-only, so save them individually.
    pub(in crate::codegen) fn a64_save_fp_callee_save(&mut self) {
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(64);
            str d8,  [sp];
            str d9,  [sp, #(8)];
            str d10, [sp, #(16)];
            str d11, [sp, #(24)];
            str d12, [sp, #(32)];
            str d13, [sp, #(40)];
            str d14, [sp, #(48)];
            str d15, [sp, #(56)];
        );
    }

    /// Restore what `a64_save_fp_callee_save` saved and pop the 64-byte area.
    pub(in crate::codegen) fn a64_restore_fp_callee_save(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr d8,  [sp];
            ldr d9,  [sp, #(8)];
            ldr d10, [sp, #(16)];
            ldr d11, [sp, #(24)];
            ldr d12, [sp, #(32)];
            ldr d13, [sp, #(40)];
            ldr d14, [sp, #(48)];
            ldr d15, [sp, #(56)];
            add sp, sp, #(64);
        );
    }

    pub(in crate::codegen) fn a64_invoker_prologue(&mut self) {
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            stp x(EXEC.0), x(GLOBALS.0), [sp, #(-16)]!;
            stp x(PC.0), x(LFP.0), [sp, #(-16)]!;
            stp x(ACC.0), x24, [sp, #(-16)]!;
            stp x25, x26, [sp, #(-16)]!;
            mov x(EXEC.0), x0;
            mov x(GLOBALS.0), x1;
        );
        // Preserve the Rust caller's callee-saved FP regs (the JIT body
        // clobbers D8-D15 as FP-pool scratch). Mirrored by the epilogue.
        self.a64_save_fp_callee_save();
    }

    /// fdata(X9) = funcinfo_base + (X2 = funcid)*64 + FUNCINFO_DATA.
    pub(in crate::codegen) fn a64_get_func_data_x2(&mut self) {
        monoasm_arm64!(&mut self.jit,
            lsl x10, x2, #(32);  // zero-extend funcid
            lsr x10, x10, #(32);
            lsl x10, x10, #(6);  // * size_of::<FuncInfo>() (64)
            mov x11, (GLOBALS_FUNCINFO as u64);
            add x11, x(GLOBALS.0), x11;
            ldr x11, [x11];
            add x10, x10, x11;
            add x9, x10, #(FUNCINFO_DATA as u32);
        );
    }

    /// Shared invoker tail (after the frame self/block/outer/meta are set):
    /// argument setup (simple copy or runtime arg massager), call into the
    /// callee, then the epilogue. Assumes fdata in X9, meta in X14, x4 = args
    /// ptr, x5 = len, x7 = kw (Option<Hashmap>).
    /// `upward == true`: args are an ascending `*const Value` (method/block
    /// invoker). `upward == false`: args are a descending `Arg` in a caller
    /// frame (method_invoker2). Only the simple-copy direction and the generic
    /// runtime handler (`handle_invoker_arguments` vs `…2`) differ.
    pub(in crate::codegen) fn a64_invoker_args_and_call(&mut self, upward: bool) {
        let fdata = X9;
        let generic = self.jit.label();
        let simple_done = self.jit.label();
        let argloop = self.jit.label();
        let aftargs = self.jit.label();
        let error_exit = self.jit.label();
        // Simple iff callee is simple (Meta kind bit 4) AND len == min AND
        // no keyword args; otherwise fall back to the runtime arg massager.
        monoasm_arm64!(&mut self.jit,
            lsr x10, x14, #(56);  // kind byte
            tbz x10, #(4), generic;
            ldrh x10, [x(fdata.0), #(FUNCDATA_MIN as u32)];
            cmp x5, x10;
        );
        self.jit.bcond_label(Cond::Ne, &generic);
        monoasm_arm64!(&mut self.jit,
            cbnz x7, generic;  // kw present
            cbz x5, simple_done;
        );
        if upward {
            // ascending args: args[len-1..0] -> callee slots, src counts down.
            monoasm_arm64!(&mut self.jit,
                mov x10, x5;  // down counter = len
                neg x11, x5;  // up counter = -len
                sub x12, x4, #(8);  // args - 8
                argloop:
                add x13, x12, x10, lsl #(3);  // &args[down-1]
                ldr x14, [x13];
                sub x15, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
                add x15, x15, x11, lsl #(3);
                str x14, [x15];
                sub x10, x10, #(1);
                add x11, x11, #(1);
                cbnz x11, argloop;
                simple_done:
                b aftargs;
            );
        } else {
            // descending args (Arg): args[0] at x4, args[i] at x4 - i*8.
            // counter x11 runs -len..0; src = [x4 + x11*8 + 8].
            monoasm_arm64!(&mut self.jit,
                neg x11, x5;  // counter = -len
                argloop:
                add x13, x4, x11, lsl #(3);
                ldr x14, [x13, #(8)];         // [x4 + x11*8 + 8]
                sub x15, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
                add x15, x15, x11, lsl #(3);
                str x14, [x15];
                add x11, x11, #(1);
                cbnz x11, argloop;
                simple_done:
                b aftargs;
            );
        }
        // generic: handle_invoker_arguments[2](exec, globals, callee_lfp,
        // arg_num, args, kw). Reserve scratch below the callee frame and
        // preserve SP (X25) + funcdata (X26, caller-saved) across the call.
        monoasm_arm64!(&mut self.jit,
            generic:
            mov x25, sp;
            mov x26, x(fdata.0);
            ldrh x10, [x(fdata.0), #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);
            sub x11, x25, x10;
            mov sp, x11;
            sub x2, x25, #(RSP_LOCAL_FRAME as u32);  // callee_lfp
            mov x3, x5;  // arg_num = len
            mov x5, x7;  // kw
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
        // x4 = args (unchanged)
        );
        let handler = if upward {
            runtime::handle_invoker_arguments as *const () as u64
        } else {
            runtime::handle_invoker_arguments2 as *const () as u64
        };
        monoasm_arm64!(&mut self.jit,
            mov x9, (handler);
            blr x9;
            mov x9, x26;  // restore fdata
            mov sp, x25;  // restore SP
            cbz x0, error_exit;
            aftargs:
        // call_invoker: push_frame
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];  // [new_cfp] = prev cfp
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];  // exec.cfp = new cfp
        // set_lfp
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x(LFP.0), [x10];
        // pc = funcdata.pc; call funcdata.codeptr
            ldr x(PC.0), [x(fdata.0), #(FUNCDATA_PC as u32)];
            ldr x10, [x(fdata.0), #(FUNCDATA_CODEPTR as u32)];
            blr x10;
        // restore exec.cfp = [sp - RSP_CFP] (the prev cfp saved above)
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        // Converge with the result (or 0 on error) in x0; the caller emits
        // its own epilogue (standard restore for method/block invokers, or a
        // fiber stack-switch-back for the fiber invoker).
            error_exit:
        );
    }

    /// Standard invoker epilogue: restore the callee-saved registers saved by
    /// a64_invoker_prologue and return (result in x0).
    pub(in crate::codegen) fn a64_invoker_epilogue(&mut self) {
        self.a64_restore_fp_callee_save();
        monoasm_arm64!(&mut self.jit,
            ldp x25, x26, [sp], #(16);
            ldp x(ACC.0), x24, [sp], #(16);
            ldp x(PC.0), x(LFP.0), [sp], #(16);
            ldp x(EXEC.0), x(GLOBALS.0), [sp], #(16);
            ldp x29, x30, [sp], #(16);
            ret;
        );
    }

    /// Save all callee-saved GPRs (x19-x28) + fp/lr + the callee-saved FP regs
    /// (D8-D15) for a fiber context switch. D8-D15 are part of the JIT FP pool
    /// (see `a64_fpr`), so a fiber yielding mid-computation — and the parent
    /// whose D8-D15 the fiber body clobbers — both need them preserved across
    /// the switch.
    pub(in crate::codegen) fn a64_push_callee_save(&mut self) {
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            stp x27, x28, [sp, #(-16)]!;
            stp x25, x26, [sp, #(-16)]!;
            stp x23, x24, [sp, #(-16)]!;
            stp x21, x22, [sp, #(-16)]!;
            stp x19, x20, [sp, #(-16)]!;
        );
        self.a64_save_fp_callee_save();
    }

    /// Restore what a64_push_callee_save saved (reverse order).
    pub(in crate::codegen) fn a64_pop_callee_save(&mut self) {
        self.a64_restore_fp_callee_save();
        monoasm_arm64!(&mut self.jit,
            ldp x19, x20, [sp], #(16);
            ldp x21, x22, [sp], #(16);
            ldp x23, x24, [sp], #(16);
            ldp x25, x26, [sp], #(16);
            ldp x27, x28, [sp], #(16);
            ldp x29, x30, [sp], #(16);
        );
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
        monoasm_arm64!(&mut self.jit,
            sub x(fb.0), sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            mov x13, (0);
            str x3, [x(fb.0)];  // LFP_SELF  = self
            str x6, [x(fb.0), #(8)];  // LFP_BLOCK = block
            str x13, [x(fb.0), #(16)];  // LFP_CME   = 0
            str x13, [x(fb.0), #(24)];  // LFP_SVAR  = 0
            ldr x14, [x(fdata.0), #(FUNCDATA_META as u32)];
            str x14, [x(fb.0), #(32)];  // LFP_META  = funcdata.meta
            str x13, [x(fb.0), #(40)];  // LFP_OUTER = 0
        );
        self.a64_invoker_args_and_call(true);
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the MethodInvoker ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, MethodInvoker>(&codeptr.as_ptr()) }
    }

    /// Like `method_invoker`, but the args come as a descending `Arg` in a
    /// caller frame (not an ascending `*const Value`) and there is no block or
    /// keyword argument. ABI: x0 exec, x1 globals, x2 funcid, x3 self, x4 args
    /// (Arg), x5 len. Used by the inlined `Class#new` to run `initialize`.
    pub(in crate::codegen) fn method_invoker2(&mut self) -> MethodInvoker2 {
        let codeptr = self.jit.get_current_address();
        let fdata = X9;
        self.a64_invoker_prologue();
        self.a64_get_func_data_x2();
        let fb = X12;
        monoasm_arm64!(&mut self.jit,
            sub x(fb.0), sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            mov x13, (0);
            mov x7, (0);                // no keyword args
            str x3, [x(fb.0)];          // LFP_SELF  = self
            str x13, [x(fb.0), #(8)];   // LFP_BLOCK = 0
            str x13, [x(fb.0), #(16)];  // LFP_CME   = 0
            str x13, [x(fb.0), #(24)];  // LFP_SVAR  = 0
            ldr x14, [x(fdata.0), #(FUNCDATA_META as u32)];
            str x14, [x(fb.0), #(32)];  // LFP_META  = funcdata.meta
            str x13, [x(fb.0), #(40)];  // LFP_OUTER = 0
        );
        self.a64_invoker_args_and_call(false);
        self.a64_invoker_epilogue();
        // SAFETY: codeptr points at an extern "C" fn with the MethodInvoker2 ABI.
        unsafe { std::mem::transmute_copy::<*mut u8, MethodInvoker2>(&codeptr.as_ptr()) }
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
            monoasm_arm64!(&mut self.jit,
                sub x10, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
                str x3, [x10];
            );
        } else {
            // X3 = block_val (block_invoker ABI: 4th AAPCS64 arg). Stash it
            // into LFP_BLOCK *before* X3 is reused for outer_lfp. Without
            // this the block passed to `Proc#call` (and through proc
            // composition's `f.call(*a, &b)`) is silently dropped, so e.g.
            // `(one >> two).call { |x| ... }` ran with `&arg == nil` in the
            // inner procs.
            monoasm_arm64!(&mut self.jit,
                sub x10, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
                str x3, [x10];
            );
        }
        // outer = [&ProcData + PROCDATA_OUTER]; func_id = [+ PROCDATA_FUNCID]
        monoasm_arm64!(&mut self.jit,
            ldr x3, [x2, #(runtime::PROCDATA_OUTER as u32)];  // X3 = outer lfp
            ldr w2, [x2, #(runtime::PROCDATA_FUNCID as u32)];  // X2 = func_id
        );
        self.a64_get_func_data_x2(); // fdata = X9 (clobbers X10, X11)
        let fb = X12;
        monoasm_arm64!(&mut self.jit,
            sub x(fb.0), sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            mov x13, (0);
        );
        if !with_self {
            // self = outer.self = [outer - LFP_SELF]
            monoasm_arm64!(&mut self.jit,
                sub x10, x3, #(LFP_SELF as u32);
                ldr x10, [x10];
                str x10, [x(fb.0)];  // LFP_SELF
            // LFP_BLOCK was already populated above with X3 (block_val).
            );
        } else {
            // block_invoker_with_self has no block to forward.
            monoasm_arm64!(&mut self.jit,
                str x13, [x(fb.0), #(8)];  // LFP_BLOCK = 0
            );
        }
        monoasm_arm64!(&mut self.jit,
            str x13, [x(fb.0), #(16)];  // LFP_CME = 0
            str x13, [x(fb.0), #(24)];  // LFP_SVAR = 0
            ldr x14, [x(fdata.0), #(FUNCDATA_META as u32)];
            str x14, [x(fb.0), #(32)];  // LFP_META
            str x3, [x(fb.0), #(40)];  // LFP_OUTER = outer
        );
    }

    pub(in crate::codegen) fn a64_block_invoker(&mut self, with_self: bool) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        self.a64_block_frame_setup(with_self);
        monoasm_arm64!(&mut self.jit,
            mov x7, x6;  // shared arg setup expects kw in x7
        );
        self.a64_invoker_args_and_call(true);
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
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            str x10, [x0, #(EXECUTOR_RSP_SAVE as u32)];  // parent.rsp_save = SP
            ldr x10, [x6, #(EXECUTOR_RSP_SAVE as u32)];
            mov sp, x10;  // SP = child.rsp_save (fresh stack top)
            str x0, [x6, #(EXECUTOR_PARENT_FIBER as u32)];  // child.parent_fiber = parent
            mov x(EXEC.0), x6;  // EXEC = child_vm
            mov x(GLOBALS.0), x1;
        );
        self.a64_block_frame_setup(with_self);
        monoasm_arm64!(&mut self.jit,
            mov x7, (0);  // fibers carry no keyword args
        );
        self.a64_invoker_args_and_call(true);
        // body completed: mark this fiber terminated, switch back to parent.
        monoasm_arm64!(&mut self.jit,
            mov x10, (u64::MAX);  // -1 = terminated
            str x10, [x(EXEC.0), #(EXECUTOR_RSP_SAVE as u32)];
            ldr x(EXEC.0), [x(EXEC.0), #(EXECUTOR_PARENT_FIBER as u32)];
            ldr x10, [x(EXEC.0), #(EXECUTOR_RSP_SAVE as u32)];
            mov sp, x10;
        );
        self.a64_pop_callee_save();
        monoasm_arm64!(&mut self.jit,
            ret;
        // SAFETY: codeptr is an extern "C" fn with the FiberInvoker ABI.
        );
        unsafe { std::mem::transmute_copy::<*mut u8, FiberInvoker>(&codeptr.as_ptr()) }
    }
    /// Binding invoker: run a func body reusing a captured frame as its LFP
    /// (no new local frame, no args). AAPCS64 in: x0 exec, x1 globals, x2 lfp.
    pub(in crate::codegen) fn binding_invoker(&mut self) -> BindingInvoker {
        let codeptr = self.jit.get_current_address();
        self.a64_invoker_prologue();
        monoasm_arm64!(&mut self.jit,
            mov x(LFP.0), x2;  // reuse the binding's frame as LFP
            sub x2, x(LFP.0), #(LFP_FUNCID as u32);
            ldr w2, [x2];  // funcid = [lfp - LFP_FUNCID]
        );
        self.a64_get_func_data_x2(); // X9 = fdata
        // push_frame; cfp.lfp = LFP (the captured frame)
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x(LFP.0), [x10];
            ldr x(PC.0), [x9, #(FUNCDATA_PC as u32)];
            ldr x10, [x9, #(FUNCDATA_CODEPTR as u32)];
            blr x10;
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        );
        // epilogue (restores D8-D15 + the saved GPRs, then ret)
        self.a64_invoker_epilogue();
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
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            str x10, [x0, #(EXECUTOR_RSP_SAVE as u32)];  // parent.rsp_save = SP
            ldr x10, [x1, #(EXECUTOR_RSP_SAVE as u32)];
            mov sp, x10;  // SP = child.rsp_save
            str x0, [x1, #(EXECUTOR_PARENT_FIBER as u32)];  // child.parent_fiber = parent
        );
        self.a64_pop_callee_save();
        monoasm_arm64!(&mut self.jit,
            mov x0, x2;  // resume value
            ret;
        // SAFETY: codeptr matches the resume_fiber ABI.
        );
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }

    /// Yield from a fiber. In: x0 vm (child), x1 value. Save the child
    /// context and switch back to the parent. x86 yield_fiber.
    pub(in crate::codegen) fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();
        self.a64_push_callee_save();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            str x10, [x0, #(EXECUTOR_RSP_SAVE as u32)];  // child.rsp_save = SP
            ldr x0, [x0, #(EXECUTOR_PARENT_FIBER as u32)];  // vm = parent
            ldr x10, [x0, #(EXECUTOR_RSP_SAVE as u32)];
            mov sp, x10;  // SP = parent.rsp_save
        );
        self.a64_pop_callee_save();
        monoasm_arm64!(&mut self.jit,
            mov x0, x1;  // yielded value
            ret;
        // SAFETY: codeptr matches the yield_fiber ABI.
        );
        unsafe { std::mem::transmute_copy(&codeptr.as_ptr()) }
    }
    pub(in crate::codegen) fn init_stack_limit(&mut self) -> extern "C" fn(&mut Executor) -> *const u8 {
        // executor.stack_limit = sp - MAX_STACK_SIZE (= 65536 = 16 << 12).
        // x0 = &mut Executor (AAPCS64 arg0).
        let codeptr = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            sub x10, x10, #16, lsl #12; // 16 << 12 = 65536
            str x10, [x0, #(EXECUTOR_STACK_LIMIT as u32)];
            ret;
        // SAFETY: codeptr is an `extern "C" fn(&mut Executor) -> *const u8`.
        );
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
        monoasm_arm64!(&mut self.jit,
            label:
            tbnz x0, #(0), fixnum;  // bit0: fixnum
            tbnz x0, #(1), flonum;  // bit1: flonum
            tbnz x0, #(2), l1;  // bit2: other immediate
            cbz x0, err;  // 0 is invalid
            ldr w0, [x0, #(RVALUE_OFFSET_CLASS as u32)];  // heap: RValue.class
            ret;
            l1:
            mov x1, (0xff);
            and x2, x0, x1;
            cmp x2, #(TAG_SYMBOL as u32);
        );
        self.jit.bcond_label(Cond::Eq, &symbol);
        monoasm_arm64!(&mut self.jit,
            cmp x0, #(NIL_VALUE as u32);
        );
        self.jit.bcond_label(Cond::Eq, &nil);
        monoasm_arm64!(&mut self.jit,
            mov x1, (8);
            orr x1, x0, x1;
            cmp x1, #(TRUE_VALUE as u32);
        );
        self.jit.bcond_label(Cond::Eq, &bool_);
        monoasm_arm64!(&mut self.jit,
            err:
            mov x0, (0xc1a5);  // DIAG: illegal_classid (get_class)
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #(0);  // TODO(aarch64): illegal_classid
            fixnum:
            mov x0, (INTEGER_CLASS.u32() as u64);
            ret;
            flonum:
            mov x0, (FLOAT_CLASS.u32() as u64);
            ret;
            symbol:
            mov x0, (SYMBOL_CLASS.u32() as u64);
            ret;
            nil:
            mov x0, (NIL_CLASS.u32() as u64);
            ret;
            bool_:
            mov x0, (BOOL_CLASS.u32() as u64);
            ret;
        );
        label
    }
}
