use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Shared tail of `ClassDef` / `SingletonClassDef`: enter the class
    /// context, run the class body, store the result, then leave. Expects the
    /// new class/module `self` in x0. Mirrors x86 `jit_class_def_sub`; the
    /// call_funcdata sequence is the same as `emit_yield`'s. `dst_off`, if set,
    /// is the pre-range-checked `conv(dst)` byte offset of the result slot.
    pub(super) fn a64_jit_class_def_sub(
        &mut self,
        func_id: FuncId,
        dst_off: Option<u32>,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) {
        let lfp = GP::R14.a64().0; // x22
        let f_enter = runtime::enter_classdef as *const () as u64;
        let f_exit = runtime::exit_classdef as *const () as u64;
        // x25 <- self (callee-saved, survives the C calls). enter_classdef(
        // vm, globals, func_id, self) -> x0 = &FuncData; saved in x26.
        monoasm_arm64!(&mut self.jit,
            mov x25, x0;
            mov x0, x19;
            mov x1, x20;
            mov x2, (func_id.get() as u64);
            mov x3, x25;
            str x30, [sp, #-16]!;
            mov x9, (f_enter);
            blr x9;
            ldr x30, [sp], #16;
            mov x26, x0;                                  // &FuncData
            // callee block/method frame fields below sp.
            mov x12, (0u64);
            ldr x10, [x26, #(FUNCDATA_META as u32)];
            stur x10, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];
            stur x25, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
            // set_method_outer: outer/svar = 0
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            // call_funcdata (fdata in x26): push frame, set callee LFP/PC, call.
            ldr x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x19, #(EXECUTOR_CFP as u32)];
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);
            stur x22, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
            sub x3, x21, #(16u32);                        // with-pc call-site bc ptr
            ldr x21, [x26, #(FUNCDATA_PC as u32)];
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;                                       // x0 = body result
            ldur x10, [sp, #(-(RSP_CFP as i32))];
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        );
        // store_rax(dst)
        if let Some(off) = dst_off {
            self.a64_frame_store(0, lfp, off);
        }
        // pop class context: exit_classdef(vm, globals), preserving the result.
        monoasm_arm64!(&mut self.jit,
            mov x25, x0;
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f_exit);
            blr x9;
            ldr x30, [sp], #16;
            mov x0, x25;
        );
        // Reload the pool (clobbered by the class body + exit_classdef) and pop
        // the save area before the final HandleError branch.
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
    }

    /// `ClassDef`: define (or reopen) a class/module, then run its body. The
    /// live FP pool is saved once for the whole sequence (the define/enter/exit
    /// C calls and the class body all clobber d2..) and reloaded into the pool
    /// registers before each HandleError branch.
    pub(in crate::codegen::jitgen) fn class_def(
        &mut self,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        let sc_off = superclass.map(|s| conv(s) as u32);
        let base_off = base.map(|b| conv(b) as u32);
        let dst_off = dst.map(|d| conv(d) as u32);
        let lfp = GP::R14.a64().0; // x22
        let f = runtime::define_class as *const () as u64;
        // Save the live FP pool for the whole ClassDef sequence; it persists
        // across define_class, the class body, and exit_classdef, is reloaded
        // into d2.. before each HandleError, and is popped once at the end.
        self.emit_fpr_save(using_fpr, false);
        // superclass -> x3, base -> x5 (Option<Value>; 0 == None)
        match sc_off {
            Some(off) => self.a64_frame_load(3, lfp, off),
            None => monoasm_arm64!(&mut self.jit, mov x3, (0u64);),
        }
        match base_off {
            Some(off) => self.a64_frame_load(5, lfp, off),
            None => monoasm_arm64!(&mut self.jit, mov x5, (0u64);),
        }
        // define_class(vm, globals, name, superclass, is_module, base)
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            mov x2, (name.get() as u64);
            mov x4, (is_module as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                                        // x0 = Option<Value> self
            ldr x30, [sp], #16;
        );
        self.a64_fpr_reload(using_fpr);
        self.emit_handle_error(error);
        self.a64_jit_class_def_sub(func_id, dst_off, using_fpr, error);
        true
    }

    /// `SingletonClassDef`: `class << obj; … end`. Like `class_def` but the
    /// class is obtained via `define_singleton_class(vm, globals, obj)`.
    pub(in crate::codegen::jitgen) fn singleton_class_def(
        &mut self,
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        let base_off = conv(base) as u32;
        let dst_off = dst.map(|d| conv(d) as u32);
        let lfp = GP::R14.a64().0; // x22
        let f = runtime::define_singleton_class as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        // define_singleton_class(vm, globals, base)
        self.a64_frame_load(2, lfp, base_off);             // x2 = base (receiver Value)
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                                        // x0 = Option<Value> self
            ldr x30, [sp], #16;
        );
        self.a64_fpr_reload(using_fpr);
        self.emit_handle_error(error);
        self.a64_jit_class_def_sub(func_id, dst_off, using_fpr, error);
        true
    }

    /// `undef`-method via runtime::undef_method(vm=x19, globals=x20, id). Bails
    /// when an xmm pool register is live (no aarch64 xmm save around C calls
    /// yet); lr is preserved across the `blr`.
    pub(in crate::codegen::jitgen) fn emit_undef_method(&mut self, undef: IdentId, using_fpr: UsingFpr) -> bool {
        let f = runtime::undef_method as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                   // vm (Executor)
            mov x1, x20;                   // globals
            mov x2, (undef.get() as u64);  // undef (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// Alias a method via runtime::alias_method(vm, globals, old, new) where
    /// `old`/`new` are the symbol/string Values read from the `old`/`new`
    /// frame slots. The Option<Value> result (None == error) is checked by a
    /// following HandleError. Bails when an xmm pool register is live or a slot
    /// offset exceeds the 12-bit scaled load immediate.
    pub(in crate::codegen::jitgen) fn emit_alias_method(
        &mut self,
        new: SlotId,
        old: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off_old = old.0 as u32 * 8 + LFP_SELF as u32;
        let off_new = new.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::alias_method as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm (Executor)
            mov x1, x20;                 // globals
        );
        self.a64_frame_load(2, lfp, off_old); // x2 = old (slot value)
        self.a64_frame_load(3, lfp, off_new); // x3 = new (slot value)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// `def name; … end` — runtime::define_method(vm, globals, name, func_id).
    /// The Option<Value> result (None == error) is checked by the trailing
    /// HandleError. Bails when an xmm pool register is live (no save around the
    /// C call).
    pub(in crate::codegen::jitgen) fn emit_method_def(
        &mut self,
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        let f = runtime::define_method as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                     // vm (Executor)
            mov x1, x20;                     // globals
            mov x2, (name.get() as u64);     // name (IdentId)
            mov x3, (func_id.get() as u64);  // func_id (FuncId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                          // x0 = Option<Value>
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
        true
    }

    /// `def obj.name; … end` — runtime::singleton_define_method(vm, globals,
    /// name, func_id, obj) where `obj` is the receiver Value read from its
    /// frame slot (5th AAPCS arg = x4). Bails on a live xmm pool reg or an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_singleton_method_def(
        &mut self,
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = obj.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::singleton_define_method as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                     // vm (Executor)
            mov x1, x20;                     // globals
            mov x2, (name.get() as u64);     // name (IdentId)
            mov x3, (func_id.get() as u64);  // func_id (FuncId)
        );
        self.a64_frame_load(4, lfp, off);    // x4 = obj (receiver Value)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                          // x0 = Option<Value>
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
        true
    }
}
