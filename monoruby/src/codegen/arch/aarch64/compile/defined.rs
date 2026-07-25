use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    // ---- defined? runtime-call family --------------------------------------
    //
    // Two ABI shapes (mirroring the x86 helpers):
    //  * result-in-x0 then stored to `dst` (yield/super/gvar/cvar)
    //  * `dst` passed as an out-pointer the runtime writes through
    //    (const/method/ivar)
    // All bail when an xmm pool reg is live or a slot offset exceeds the 12-bit
    // `sub`/`ldr`/`str` immediate.

    /// "yield" if a block is callable, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_yield(
        &mut self,
        dst: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_yield as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // result in x0
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off); // -> dst
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// "super" if super is callable, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_super(
        &mut self,
        dst: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_super as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off);
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// "global-variable" if $name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_gvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_gvar as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            mov x2, (name.get() as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off);
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// "class variable" if @@name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_cvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_cvar as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            mov x2, (name.get() as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off);
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// defined?(Const): runtime writes the result through the `dst` out-pointer.
    pub(in crate::codegen::jitgen) fn emit_defined_const(
        &mut self,
        dst: SlotId,
        siteid: ConstSiteId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_const as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
        );
        self.a64_addr_sub(2, lfp, off);  // x2 = &dst (out-pointer)
        monoasm_arm64!(&mut self.jit,
            mov x3, (siteid.0 as u64);   // ConstSiteId
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// defined?(recv.name): runtime writes the result through `dst`.
    pub(in crate::codegen::jitgen) fn emit_defined_method(
        &mut self,
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off_dst = dst.0 as u32 * 8 + LFP_SELF as u32;
        let off_recv = recv.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_method as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
        );
        self.a64_addr_sub(2, lfp, off_dst);   // x2 = &dst (out-pointer)
        self.a64_frame_load(3, lfp, off_recv); // x3 = recv (slot value)
        monoasm_arm64!(&mut self.jit,
            mov x4, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// defined?(@name): runtime writes the result through `dst`.
    pub(in crate::codegen::jitgen) fn emit_defined_ivar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_ivar as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
        );
        self.a64_addr_sub(2, lfp, off);  // x2 = &dst (out-pointer)
        monoasm_arm64!(&mut self.jit,
            mov x3, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }
}
