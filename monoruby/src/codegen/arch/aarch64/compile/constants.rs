use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Store the accumulator to a constant via set_constant(vm, globals, id,
    /// val), bumping the global constant version. Bails (`false`) if any xmm is
    /// live (no FP save/restore yet). Mirrors x86 `store_constant` + error check.
    pub(in crate::codegen::jitgen) fn emit_store_constant(
        &mut self,
        id: ConstSiteId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        let error = error.clone();
        let cv_addr = self
            .jit
            .get_label_address(&self.const_version_label())
            .as_ptr() as u64;
        let f = runtime::set_constant as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x3, x0;                 // val (was in rax)
            mov x0, x19;                // vm
            mov x1, x20;                // globals
            mov x2, (id.0 as u64);      // ConstSiteId
            mov x9, (cv_addr);
            ldr x10, [x9];
            add x10, x10, #(1u32);
            str x10, [x9];              // bump global const version
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        // Restore the FP pool *before* the error branch: the cold error handler
        // writes the live floats back from the pool, so they must be valid.
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            cbz x0, error;              // None -> error
        );
        true
    }
}
