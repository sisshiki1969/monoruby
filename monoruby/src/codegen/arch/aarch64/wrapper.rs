//! aarch64 per-method entry wrappers: `gen_wrapper`, attr/struct
//! accessors, and the native (builtin) function wrapper.
//!
//! Counterpart of `arch/x86_64/wrapper.rs`.

use super::*;

impl Codegen {
    /// Per-method entry wrapper. For ISeq methods (VM-only build) this is the
    /// vm-stub `b vm_entry` (x86 `gen_vm_stub`). TODO(aarch64): other
    /// FuncKinds (Builtin/AttrReader/ConstReturn/…).
    pub(crate) fn gen_wrapper(&mut self, globals: &Globals, fid: FuncId) -> DestLabel {
        let entry = self.jit.label();
        self.jit.bind_label(entry.clone());
        match &globals.store[fid].kind {
            FuncKind::ISeq(_) => {
                let vm_entry = self.vm_entry();
                self.jit.b_label(&vm_entry);
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
                self.jit.mov_imm(X12, outer_ptr);
                // X2 = FuncId, then resolve &FuncData into X9 (clobbers X10/X11).
                self.jit.mov_imm(X2, funcid as u64);
                self.a64_get_func_data_x2();
                // [LFP - LFP_OUTER] = outer  (LFP_OUTER == 0)
                self.jit.str(X12, LFP, 0);
                // [LFP - LFP_META]  = funcdata.meta | PROC_METHOD_MASK
                self.jit.ldr(X10, X9, FUNCDATA_META as u32);
                self.jit.mov_imm(X11, Meta::PROC_METHOD_MASK);
                self.jit.orr(X10, X10, X11);
                self.jit.sub_imm(X13, LFP, LFP_META as u32, 0);
                self.jit.str(X10, X13, 0);
                // PC = funcdata.pc; tail-call funcdata.codeptr (preserve lr so
                // the proc body returns to the original caller).
                self.jit.ldr(PC, X9, FUNCDATA_PC as u32);
                self.jit.ldr(X10, X9, FUNCDATA_CODEPTR as u32);
                self.jit.br(X10);
            }
        }
        self.jit.finalize();
        entry
    }

    /// attr_reader: return `self.@ivar_name` via the cached ivar accessor.
    pub(in crate::codegen) fn a64_gen_attr_reader(&mut self, ivar_name: IdentId) {
        // Per-method inline cache (InstanceVarCache, 8 bytes, init -1 = miss).
        // Leaked on the heap so its address is known at emit time; aarch64 ADR
        // can't reach a JIT data label from the lazily-generated wrapper.
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // self
        self.jit.mov_imm(X1, ivar_name.get() as u64); // name
        self.jit.mov(X2, GLOBALS);
        self.jit.mov_imm(X3, cache_addr); // &cache
        self.jit
            .mov_imm(X9, get_instance_var_with_cache as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// attr_writer: `self.@ivar_name = arg0` via the cached ivar setter.
    pub(in crate::codegen) fn a64_gen_attr_writer(&mut self, ivar_name: IdentId) {
        let cache_addr = Box::into_raw(Box::new(-1i64)) as u64;
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // self
        self.jit.mov_imm(X3, ivar_name.get() as u64); // name
        self.jit.sub_imm(X4, LFP, LFP_ARG0 as u32, 0);
        self.jit.ldr(X4, X4, 0); // val
        self.jit.mov_imm(X5, cache_addr); // &cache
        self.jit
            .mov_imm(X9, set_instance_var_with_cache as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Struct member reader: read inline slot or heap slot directly (no call).
    pub(in crate::codegen) fn a64_gen_struct_reader(&mut self, slot_index: u16, inline: bool) {
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // self
        if inline {
            self.jit
                .ldr(X0, X0, (slot_index as u32) * 8 + RVALUE_OFFSET_INLINE as u32);
        } else {
            self.jit.ldr(X0, X0, RVALUE_OFFSET_HEAP_PTR as u32);
            self.jit.ldr(X0, X0, (slot_index as u32) * 8);
        }
        self.jit.ret();
    }

    /// Struct member writer: route through set_struct_slot_with_check.
    pub(in crate::codegen) fn a64_gen_struct_writer(&mut self, slot_index: u16) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // self
        self.jit.sub_imm(X3, LFP, LFP_ARG0 as u32, 0);
        self.jit.ldr(X3, X3, 0); // val
        self.jit.mov_imm(X4, slot_index as u64);
        self.jit.mov_imm(
            X9,
            crate::builtins::struct_class::set_struct_slot_with_check as *const () as u64,
        );
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }

    /// Native (builtin) method wrapper: allocate the arg stack region and call
    /// the Rust builtin `fn(vm, globals, lfp)`. Mirrors x86
    /// `gen_native_func_wrapper`.
    pub(in crate::codegen) fn a64_gen_native_func_wrapper(&mut self, abs_address: u64) {
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        // stack offset = ((reg_num + (RSP_LOCAL_FRAME+LFP_ARG0)/8 + 1) & !1) * 8
        self.jit.sub_imm(X10, LFP, LFP_REGNUM as u32, 0);
        self.jit.ldrh(X10, X10, 0); // reg_num
        self.jit
            .add_imm(X10, X10, ((RSP_LOCAL_FRAME + LFP_ARG0) / 8 + 1) as u32, 0);
        self.jit.mov_imm(X11, !1u64);
        self.jit.and_(X10, X10, X11);
        self.jit.lsl_imm(X10, X10, 3);
        self.jit.mov_sp(X11, SP);
        self.jit.sub(X11, X11, X10);
        self.jit.mov_sp(SP, X11);
        // builtin(vm, globals, lfp)
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP);
        self.jit.mov_imm(X9, abs_address);
        self.jit.blr(X9);
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
    }
}
