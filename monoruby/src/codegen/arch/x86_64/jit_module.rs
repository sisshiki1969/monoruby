//! x86-64 `JitModule` asm methods: VM construction primitives, frame
//! setup, register save/restore, GC poll, and the per-signal stub.
//!
//! Counterpart of `arch/aarch64/jit_module.rs`. These are inherent
//! `impl JitModule` methods on the arch-neutral `JitModule` defined in
//! `crate::codegen`; only the x86-64 implementations live here.

use super::*;
use crate::codegen::jit_module::{handle_error, unimplemented_inst};
use monoasm_macro::monoasm;

impl JitModule {
    pub(in crate::codegen) fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        let const_version = jit.data_i64(1);
        let alloc_flag = jit.data_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });
        let pending_signals = jit.data_i32(0);
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
            pending_signals,
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
    pub(in crate::codegen) fn init(&mut self) {
        let raise = self.entry_raise.clone();
        let overflow = self.vm_stack_overflow.clone();
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
            // rax: Option<Value>
            // rdx: Option<BytecodePtr>
            testq rdx, rdx;
            jne  goto;
            leave;
            ret;
        goto:
            movq r13, rdx;
        }
        self.fetch_and_dispatch();

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
    pub(in crate::codegen) fn gen_entry_panic(&mut self, label: DestLabel) {
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
    pub(in crate::codegen) fn gen_f64_to_val(&mut self, label: DestLabel) {
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
    /// Emit the GC poll. `write_back` emits any register write-back into the
    /// cold (page-1) path before the GC call; the VM passes a no-op, the JIT
    /// passes the spill write-back. Taking a closure keeps the JIT-only
    /// `WriteBack` type out of this VM-tier helper's signature.
    pub(in crate::codegen) fn execute_gc_inner(
        &mut self,
        error: &DestLabel,
        write_back: impl FnOnce(&mut Self),
    ) {
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
        write_back(self);
        monoasm! { &mut self.jit,
            call exec_gc;
            testq rax, rax;
            jne  exit;
            jmp  error;
        }
        self.jit.select_page(0);
    }

    /// Emit a per-signal asm stub. Each stub OR-sets its own bit into
    /// `pending_signals` and nudges `alloc_flag` so the next GC poll
    /// in `execute_gc` notices the signal and converts it to a Ruby
    /// exception. `signo` is the POSIX signal number (1..32).
    ///
    /// The handler runs in async-signal context; the only operations
    /// performed are a memory OR and an ADD, both of which are
    /// async-signal-safe on x86-64. No call into Rust.
    pub(crate) fn signal_handler_for(
        &mut self,
        alloc_flag: DestLabel,
        pending_signals: DestLabel,
        signo: i32,
    ) -> CodePtr {
        debug_assert!((1..=32).contains(&signo));
        let bit: i32 = 1 << (signo - 1);
        let codeptr = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            addl [rip + alloc_flag], 10;
            orl  [rip + pending_signals], (bit);
            ret;
        }
        codeptr
    }
    ///
    /// Save caller-save registers (except rax) in stack.
    ///
    pub(in crate::codegen) fn save_registers(&mut self) {
        monoasm! { &mut self.jit,
            subq rsp, 192;
            //movq [rsp + 176], rax;
            movq [rsp + 168], r11;
            movq [rsp + 160], r10;
            movq [rsp + 152], r9;
            movq [rsp + 144], r8;
            movq [rsp + 136], rcx;
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
        }
    }

    ///
    /// Restore caller-save registers (except rax) from stack.
    ///
    pub(in crate::codegen) fn restore_registers(&mut self) {
        monoasm! { &mut self.jit,
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
            movq rcx, [rsp + 136];
            movq r8, [rsp + 144];
            movq r9, [rsp + 152];
            movq r10, [rsp + 160];
            movq r11, [rsp + 168];
            //movq rax, [rsp + 176];
            addq rsp, 192;
        }
    }

    ///
    /// Test whether the current local frame has been "captured" —
    /// either already promoted to heap (bit 7 `on_heap`) or still at
    /// its stack address but content has been moved to heap with a
    /// tombstone (bit 3 `invalidated`). Both cases violate the JIT's
    /// stack-layout assumptions for register-cached locals / the
    /// specialised rbp-relative outer access and require a deopt.
    ///
    /// The `invalidated` case arises when a method call (often an
    /// inlined wrapper like the Ruby `Integer#downto`) promotes an
    /// ancestor frame via `move_frame_to_heap` but JIT-inlined code
    /// never does a `pop_frame` to reload r14 from the updated
    /// `cfp.lfp` slot. The tombstone bit lets the guard catch that
    /// case too.
    ///
    /// if the frame is captured, jump to *label*.
    ///
    pub(in crate::codegen) fn branch_if_captured(&mut self, label: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_1000_u8 as i8);
            jnz label;
        }
    }
    ///
    /// Fetch instruction and dispatch.
    ///
    /// ### in
    /// - r13: BcPc
    ///
    /// ### destroy
    /// - rax, r15
    ///
    pub(in crate::codegen) fn fetch_and_dispatch(&mut self) {
        monoasm! { &mut self.jit,
            movq r15, (self.dispatch.as_ptr());
            movzxb rax, [r13 + (OPECODE)]; // rax <- :0
            addq r13, 16;
            jmp [r15 + rax * 8];
        };
    }
    /// Set outer and self for block.
    ///
    /// ### in
    /// - rax: outer_lfp
    ///
    /// ### destroy
    /// - rsi
    ///
    pub(in crate::codegen) fn set_block_self_outer(&mut self) {
        self.set_block_outer();
        monoasm! { &mut self.jit,
            // set self
            movq rsi, [rax - (LFP_SELF)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rsi;
        };
    }

    /// Set outer for block.
    ///
    /// ### in
    /// - rax: outer_lfp
    ///
    pub(in crate::codegen) fn set_block_outer(&mut self) {
        monoasm! { &mut self.jit,
            // set outer
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], rax;
            // SVAR / CME are unused by block frames (they walk the
            // outer chain to the LEP for `$~`); zero them so any stray
            // reader sees a clean "absent" sentinel and so the GC
            // doesn't follow uninitialised stack memory.
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
        };
    }

    /// Set outer.
    pub(in crate::codegen) fn set_method_outer(&mut self) {
        monoasm! { &mut self.jit,
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], 0;
            // Method-introducing frame: own SVAR slot starts unset
            // (zero = "no MatchData captured"). CME also zero for
            // now — reserved for a future migration.
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
        };
    }

    ///
    /// Set lfp(r14) for callee.
    ///
    /// the local frame MUST BE on the stack.
    pub(in crate::codegen) fn set_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // set lfp
            lea  r14, [rsp - (RSP_LOCAL_FRAME)];
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
        );
    }

    /// Push control frame and set outer.
    ///
    /// ### destroy
    /// - rdi, rsi
    ///
    pub(in crate::codegen) fn push_frame(&mut self) {
        monoasm!( &mut self.jit,
            // push cfp
            movq rdi, [rbx + (EXECUTOR_CFP)];
            lea  rsi, [rsp - (RSP_CFP)];
            movq [rsi], rdi;
            movq [rbx + (EXECUTOR_CFP)], rsi;
        );
    }

    pub(in crate::codegen) fn restore_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // restore lfp
            movq r14, [rbp - (BP_CFP + CFP_LFP)];
        );
    }

    /// Pop control frame
    pub(in crate::codegen) fn pop_frame(&mut self) {
        monoasm!( &mut self.jit,
            // pop cfp
            lea  r14, [rbp - (BP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], r14;
        );
        self.restore_lfp();
    }

    ///
    /// Get FuncData.
    ///
    /// ### in
    /// - r12: &Globals
    /// - rdx: FuncId
    ///
    /// ### out
    /// - r15: &FuncData
    ///
    pub(in crate::codegen) fn get_func_data(&mut self) {
        monoasm! { &mut self.jit,
            movl rdx, rdx;
            // assumes size_of::<FuncInfo>() is 64,
            shlq rdx, 6;
            addq rdx, [r12 + (GLOBALS_FUNCINFO)];
            lea  r15, [rdx + (FUNCINFO_DATA)];
        };
    }

    ///
    /// Get ProcData.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &Globals
    ///
    /// ### out
    /// - rax: outer_lfp: Option<LFP>
    /// - rdx: func_id: Option<FuncId>
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(in crate::codegen) fn get_proc_data(&mut self) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
        self.resolve_invalidated_outer(GP::Rax);
    }

    ///
    /// If the outer LFP in `R(reg)` points at a stack frame whose
    /// content has already been copied to heap by `move_frame_to_heap`
    /// (flagged `invalidated` on the tombstone), forward the pointer
    /// to the live heap copy via the owning CFP's LFP slot.
    ///
    /// - `invalidated` bit = bit 3 of the `kind` byte in Meta; `kind`
    ///   lives at `lfp - (LFP_META - META_KIND)` = `lfp - 1`.
    /// - the LFP's CFP is at `lfp + 16`, and `cfp.lfp` slot is at
    ///   `cfp - 8` = `lfp + 8`. Reading 8 bytes there gives the heap
    ///   pointer that `cfp.set_lfp(heap)` stored at promotion time.
    ///
    /// Emits 3 instructions + 1 conditional branch on the happy
    /// (not-invalidated) path.
    pub(in crate::codegen) fn resolve_invalidated_outer(&mut self, reg: GP) {
        let skip = self.jit.label();
        monoasm! { &mut self.jit,
            // Skip if outer is NULL (e.g. `get_yield_data` returned
            // the default ProcData on a no-block-given error). The
            // ASM callers handle that via a subsequent error check,
            // but we must not dereference a null lfp here.
            testq R(reg as _), R(reg as _);
            jz   skip;
            testb [R(reg as _) - 1], (0b0000_1000_u8 as i8);
            jz   skip;
            movq R(reg as _), [R(reg as _) + 8];
        skip:
        }
    }

    ///
    /// ### in
    /// - r15: &FuncData
    ///
    pub(in crate::codegen) fn call_funcdata(&mut self) -> CodePtr {
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // set call site bc ptr in rcx for with_pc builtins (r13 = InlineCache = call_site + 16)
            lea  rcx, [r13 - 16];
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
        }
        let return_addr = self.jit.get_current_address();
        self.pop_frame();
        return_addr
    }

    ///
    /// Invoke the function.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### out
    /// - rax: return value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(in crate::codegen) fn call_invoker(&mut self) {
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    ///
    /// Invoke the function.
    ///
    /// ### in
    /// - r15: &FuncData
    /// - r14: callee's Lfp
    ///
    /// ### out
    /// - rax: return value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(in crate::codegen) fn call_invoker_with_binding(&mut self) {
        self.push_frame();
        monoasm! { &mut self.jit,
            // set lfp
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    pub(in crate::codegen) fn push_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            pushq r15;
            pushq r14;
            pushq r13;
            pushq r12;
            pushq rbx;
            pushq rbp;
        };
    }

    pub(in crate::codegen) fn pop_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            popq rbp;
            popq rbx;
            popq r12;
            popq r13;
            popq r14;
            popq r15;
        };
    }

    ///
    /// Push stack offset for callee.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### destoroy
    /// - rdi
    ///
    pub(in crate::codegen) fn push_stack_offset(&mut self) {
        monoasm! { &mut self.jit,
            movzxw rdi, [r15 + (FUNCDATA_OFS)];
            shlq rdi, 4;
            addq rdi, 8;
            subq rsp, rdi;
            pushq rdi;
        }
    }

    ///
    /// Pop stack offset for callee.
    ///
    /// ### destoroy
    /// - rdi
    ///
    pub(in crate::codegen) fn pop_stack_offset(&mut self) {
        monoasm! { &mut self.jit,
            popq rdi;
            addq rsp, rdi;
        }
    }

    ///
    /// Execute GC. (for interpreter)
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    /// - r13: PC + 1
    /// - r14: LFP
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax
    /// - stack
    ///
    pub(in crate::codegen) fn vm_execute_gc(&mut self) {
        let raise = self.entry_raise.clone();
        self.execute_gc_inner(&raise, |_| {});
    }

    ///
    /// Check stack overflow.
    ///
    /// raise StackOverFlow error if the stack pointer is below the stack limit.
    ///
    pub(in crate::codegen) fn vm_check_stack(&mut self) {
        let overflow = self.vm_stack_overflow.clone();
        monoasm! { &mut self.jit,
            cmpq rsp, [rbx + (EXECUTOR_STACK_LIMIT)];
            jle overflow;
        };
    }

}
