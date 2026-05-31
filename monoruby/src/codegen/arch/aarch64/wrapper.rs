//! aarch64 per-method entry wrappers: `gen_wrapper`, attr/struct
//! accessors, and the native (builtin) function wrapper.
//!
//! Counterpart of `arch/x86_64/wrapper.rs`.

use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {
    /// Per-method entry wrapper. For ISeq methods (VM-only build) this is the
    /// vm-stub `b vm_entry` (x86 `gen_vm_stub`). TODO(aarch64): other
    /// FuncKinds (Builtin/AttrReader/ConstReturn/…).
    pub(crate) fn gen_wrapper(&mut self, globals: &Globals, fid: FuncId) -> DestLabel {
        let entry = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            entry:
        );
        match &globals.store[fid].kind {
            FuncKind::ISeq(_) => {
                let vm_entry = self.vm_entry();
                // JIT trigger (Phase 3a/3b). Per-method JIT-entry slot + call
                // counter (both heap-leaked so their absolute addresses are
                // known at emit time — aarch64 `adr` can't reach a JIT data
                // label from the lazily-generated wrapper). On each call:
                //   - if the slot is non-zero, the method was compiled → branch
                //     to the JIT code (indirect-dispatch install; aarch64 has no
                //     runtime branch patching);
                //   - else decrement the counter; when it hits 0 call
                //     jit_compile_patch, which runs the front-end + aarch64
                //     lowering and, on success, writes the compiled entry into
                //     the slot. Unsupported AsmInsts bail → stays VM.
                // x19..x23 are callee-saved across the `extern "C"` call; LR is
                // saved across `blr`.
                #[cfg(jit)]
                {
                    let counter_addr = Box::into_raw(Box::new(COUNT_START_COMPILE)) as u64;
                    let jit_slot = Box::into_raw(Box::new(0u64)) as u64;
                    let run_jit = self.jit.label();
                    let compile_hot = self.jit.label();
                    monoasm_arm64!(&mut self.jit,
                        mov x9, (jit_slot);
                        ldr x10, [x9];
                        cbnz x10, run_jit;       // already compiled → JIT code
                        mov x9, (counter_addr);
                        ldr w11, [x9];           // 32-bit counter (zero-extended)
                        sub x11, x11, #1;
                        str w11, [x9];
                        // not hot yet → VM. `vm_entry` lives in the startup VM
                        // blob, but this wrapper is generated lazily and can sit
                        // >1MB away, out of a conditional branch's ±1MB (imm19)
                        // range. So branch over a *near* label and reach
                        // `vm_entry` with an unconditional `b` (±128MB).
                        cbz x11, compile_hot;    // hot → fall through to compile
                        b vm_entry;              // not hot yet → VM (long range)
                    compile_hot:
                        // hot: jit_compile_patch(globals=x20, lfp=x22, slot)
                        mov x0, x(GLOBALS.0);
                        mov x1, x(LFP.0);
                        mov x2, (jit_slot);      // where to publish the entry
                        str x30, [sp, #-16]!;    // save LR
                        // Reserve scratch below SP so the (deep) compile C-call
                        // can't trample the just-built callee Ruby frame, which
                        // sits just below SP and which compile_patch inspects
                        // (mirrors x86 `subq rsp, 4088`). 4080 fits a 12-bit imm
                        // and is 16-aligned.
                        sub sp, sp, #(4080);
                        mov x9, (crate::codegen::compiler::jit_compile_patch as *const () as u64);
                        blr x9;
                        add sp, sp, #(4080);
                        ldr x30, [sp], #16;      // restore LR
                        b entry;                 // retry: slot now set if compiled
                    run_jit:
                        br x10;                  // tail-call the JIT code
                    );
                }
                #[cfg(not(jit))]
                monoasm_arm64!(&mut self.jit,
                    b vm_entry;
                );
            }
            FuncKind::Builtin { abs_address } => {
                let abs = *abs_address;
                self.a64_gen_native_func_wrapper(abs);
            }
            FuncKind::AttrReader { ivar_name } => {
                let name = *ivar_name;
                self.a64_gen_attr_reader(name);
            }
            FuncKind::AttrWriter { ivar_name } => {
                let name = *ivar_name;
                self.a64_gen_attr_writer(name);
            }
            FuncKind::StructReader { slot_index, inline } => {
                let (slot_index, inline) = (*slot_index, *inline);
                self.a64_gen_struct_reader(slot_index, inline);
            }
            FuncKind::StructWriter { slot_index, .. } => {
                let slot_index = *slot_index;
                self.a64_gen_struct_writer(slot_index);
            }
            FuncKind::Proc(proc) => {
                // Wrapper for a Proc converted into a method (via
                // `define_method(:m, a_proc)`): patch the caller-set frame so
                // that the proc's outer LFP is wired up and the meta carries
                // PROC_METHOD_MASK (so a `return` from inside the proc behaves
                // as a method-return rather than a non-local block exit), then
                // tail-call the proc's codeptr.
                let outer_ptr = match proc.outer_lfp() {
                    Some(outer) => outer.as_ptr() as u64,
                    None => 0,
                };
                let funcid = proc.func_id().get();
                // Pre-load the outer pointer into X12 (caller-saved, but we
                // don't call anything between this and the str below).
                monoasm_arm64!(&mut self.jit,
                    mov x12, (outer_ptr);
                // X2 = FuncId, then resolve &FuncData into X9 (clobbers X10/X11).
                    mov x2, (funcid as u64);
                );
                self.a64_get_func_data_x2();
                // [LFP - LFP_OUTER] = outer  (LFP_OUTER == 0)
                monoasm_arm64!(&mut self.jit,
                    str x12, [x(LFP.0)];
                // [LFP - LFP_META]  = funcdata.meta | PROC_METHOD_MASK
                    ldr x10, [x9, #(FUNCDATA_META as u32)];
                    mov x11, (Meta::PROC_METHOD_MASK);
                    orr x10, x10, x11;
                    sub x13, x(LFP.0), #(LFP_META as u32);
                    str x10, [x13];
                // PC = funcdata.pc; tail-call funcdata.codeptr (preserve lr so
                // the proc body returns to the original caller).
                    ldr x(PC.0), [x9, #(FUNCDATA_PC as u32)];
                    ldr x10, [x9, #(FUNCDATA_CODEPTR as u32)];
                    br x10;
                );
            }
        }
        self.jit.finalize();
        entry
    }

    /// Self-class guard stub fronting a freshly compiled method `jit_entry`
    /// (compiled for `self_class`). Mirrors the x86 `class_guard_stub`, but
    /// because aarch64 has no runtime branch patching the per-receiver-class
    /// specializations are chained through an indirect pointer slot rather
    /// than by rewriting jump targets.
    ///
    /// An inherited (shared) method has one `FuncId` reached by every receiver
    /// class, yet the JIT body assumes `self == self_class` (slot %0 is hard
    /// `S(Class(self_class))`, so inner `self.foo` calls resolve monomorphically
    /// and skip their receiver guard). Without this stub a method compiled for
    /// one subclass would be wrongly reused for a sibling subclass.
    ///
    /// ```text
    /// guard:  self.class == self_class ? -> jit_entry
    ///   miss: next_slot set ? -> br it
    ///         else countdown; hot -> compile this new class (extends chain)
    ///                         cold -> vm_entry
    /// ```
    #[cfg(jit)]
    pub(in crate::codegen) fn a64_gen_class_guard_stub(
        &mut self,
        self_class: ClassId,
        jit_entry: &DestLabel,
    ) -> DestLabel {
        let vm_entry = self.vm_entry();
        let guard = self.jit.label();
        let miss = self.jit.label();
        let run_next = self.jit.label();
        let compile_next = self.jit.label();
        // Per-class chain link (init 0 = not compiled) + recompile counter.
        // Heap-leaked so their absolute addresses can be baked into the stub
        // (aarch64 ADR can't reach a JIT data label from a lazily-emitted stub).
        let next_slot = Box::into_raw(Box::new(0u64)) as u64;
        let next_counter = Box::into_raw(Box::new(COUNT_RECOMPILE_ARECV_CLASS)) as u64;
        monoasm_arm64!(&mut self.jit,
            guard:
            sub x0, x(LFP.0), #(LFP_SELF as u32);
            ldr x0, [x0];            // self (GP::Rax == x0; a64_guard_class uses x9/x10)
        );
        // self.class != self_class -> miss
        self.a64_guard_class(GP::Rax, self_class, &miss);
        monoasm_arm64!(&mut self.jit,
            b jit_entry;             // matched -> tail-call the JIT code
        miss:
            mov x9, (next_slot);
            ldr x10, [x9];
            cbnz x10, run_next;      // chain already extended -> dispatch on
            mov x9, (next_counter);
            ldr w11, [x9];
            sub x11, x11, #1;
            str w11, [x9];
            cbz x11, compile_next;   // hot for this new class -> compile it
            b vm_entry;              // still cold -> VM
        compile_next:
            // jit_compile_patch(globals=x20, lfp=x22, slot=next_slot)
            mov x0, x(GLOBALS.0);
            mov x1, x(LFP.0);
            mov x2, (next_slot);     // publish the new class's guard here
            str x30, [sp, #-16]!;    // save LR
            sub sp, sp, #(4080);     // scratch below the callee frame (see gen_wrapper)
            mov x9, (crate::codegen::compiler::jit_compile_patch as *const () as u64);
            blr x9;
            add sp, sp, #(4080);
            ldr x30, [sp], #16;      // restore LR
            b guard;                 // retry: next_slot now holds the new guard
        run_next:
            br x10;
        );
        guard
    }

    /// attr_reader: return `self.@ivar_name` via the cached ivar accessor.
    pub(in crate::codegen) fn a64_gen_attr_reader(&mut self, ivar_name: IdentId) {
        // Per-method inline cache (InstanceVarCache, 8 bytes, init -1 = miss).
        // Leaked on the heap so its address is known at emit time; aarch64 ADR
        // can't reach a JIT data label from the lazily-generated wrapper.
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            sub x0, x(LFP.0), #(LFP_SELF as u32);
            ldr x0, [x0];  // self
            mov x1, (ivar_name.get() as u64);  // name
            mov x2, x(GLOBALS.0);
            mov x3, (cache_addr);  // &cache
            mov x9, (get_instance_var_with_cache as *const () as u64);
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
        );
    }

    /// attr_writer: `self.@ivar_name = arg0` via the cached ivar setter.
    pub(in crate::codegen) fn a64_gen_attr_writer(&mut self, ivar_name: IdentId) {
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            sub x2, x(LFP.0), #(LFP_SELF as u32);
            ldr x2, [x2];  // self
            mov x3, (ivar_name.get() as u64);  // name
            sub x4, x(LFP.0), #(LFP_ARG0 as u32);
            ldr x4, [x4];  // val
            mov x5, (cache_addr);  // &cache
            mov x9, (set_instance_var_with_cache as *const () as u64);
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
        );
    }

    /// Struct member reader: read inline slot or heap slot directly (no call).
    pub(in crate::codegen) fn a64_gen_struct_reader(&mut self, slot_index: u16, inline: bool) {
        monoasm_arm64!(&mut self.jit,
            sub x0, x(LFP.0), #(LFP_SELF as u32);
            ldr x0, [x0];  // self
        );
        if inline {
            monoasm_arm64!(&mut self.jit,
                ldr x0, [x0, #((slot_index as u32) * 8 + RVALUE_OFFSET_INLINE as u32)];
            );
        } else {
            monoasm_arm64!(&mut self.jit,
                ldr x0, [x0, #(RVALUE_OFFSET_HEAP_PTR as u32)];
                ldr x0, [x0, #((slot_index as u32) * 8)];
            );
        }
        monoasm_arm64!(&mut self.jit,
            ret;
        );
    }

    /// Struct member writer: route through set_struct_slot_with_check.
    pub(in crate::codegen) fn a64_gen_struct_writer(&mut self, slot_index: u16) {
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            sub x2, x(LFP.0), #(LFP_SELF as u32);
            ldr x2, [x2];  // self
            sub x3, x(LFP.0), #(LFP_ARG0 as u32);
            ldr x3, [x3];  // val
            mov x4, (slot_index as u64);
        );
        self.jit.mov_imm(
            X9,
            crate::builtins::struct_class::set_struct_slot_with_check as *const () as u64,
        );
        monoasm_arm64!(&mut self.jit,
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
        );
    }

    /// Native (builtin) method wrapper: allocate the arg stack region and call
    /// the Rust builtin `fn(vm, globals, lfp)`. Mirrors x86
    /// `gen_native_func_wrapper`.
    pub(in crate::codegen) fn a64_gen_native_func_wrapper(&mut self, abs_address: u64) {
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
        // stack offset = ((reg_num + (RSP_LOCAL_FRAME+LFP_ARG0)/8 + 1) & !1) * 8
            sub x10, x(LFP.0), #(LFP_REGNUM as u32);
            ldrh x10, [x10];  // reg_num
            add x10, x10, #(((RSP_LOCAL_FRAME + LFP_ARG0) / 8 + 1) as u32);
            mov x11, (!1u64);
            and x10, x10, x11;
            lsl x10, x10, #(3);
            mov x11, sp;
            sub x11, x11, x10;
            mov sp, x11;
        // builtin(vm, globals, lfp)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);
            mov x9, (abs_address);
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
        );
    }
}
