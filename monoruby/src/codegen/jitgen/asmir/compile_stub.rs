//! aarch64 AsmIR→machine-code lowering (Phase 3b, incremental).
//!
//! The arch-neutral front-end builds `AsmIr`; this drives the lowering on
//! aarch64. `compile_asmir` returns `true` if it emitted code for the
//! instruction and `false` if the instruction is not yet ported — in which
//! case the driver returns `false`, `jit_compile` returns `Err`, and the
//! method stays VM-interpreted (the existing `compile() -> None` bail). So
//! only methods whose every `AsmInst` is supported actually JIT; coverage
//! grows one variant at a time. See `doc/aarch64-jitgen-plan.md`.

use super::*;

impl Codegen {
    /// Drive AsmIR→A64 lowering for a whole method. Returns `false` (bail) if
    /// anything is not yet supported; the caller then keeps the method on the
    /// VM. `entry_label` is bound first so it always resolves at `finalize`,
    /// even on the bail path (the partial/empty code is simply not installed).
    pub(in crate::codegen::jitgen) fn a64_gen_machine_code(
        &mut self,
        mut frame: AsmInfo,
        store: &Store,
        entry_label: DestLabel,
        class_version: DestLabel,
    ) -> bool {
        self.jit.bind_label(entry_label);
        // Specialized methods / inline bridges are not supported yet.
        if !frame.specialized_methods.is_empty() {
            return false;
        }
        for (_bb, ir) in frame.detach_ir() {
            if !self.a64_gen_asm(ir, store, &mut frame, class_version.clone()) {
                return false;
            }
        }
        true
    }

    fn a64_gen_asm(
        &mut self,
        ir: AsmIr,
        store: &Store,
        frame: &mut AsmInfo,
        class_version: DestLabel,
    ) -> bool {
        // Side exits (deopt/evict/error) are not lowered on aarch64 yet.
        if !ir.side_exit.is_empty() {
            return false;
        }
        let labels = SideExitLabels::new();
        for inst in ir.inst {
            if !self.compile_asmir(store, frame, &labels, inst, class_version.clone()) {
                return false;
            }
        }
        true
    }

    /// Lower one `AsmInst`. Returns `false` for any not-yet-ported variant.
    pub(super) fn compile_asmir(
        &mut self,
        _store: &Store,
        _frame: &mut AsmInfo,
        _labels: &SideExitLabels,
        inst: AsmInst,
        _class_version: DestLabel,
    ) -> bool {
        match inst {
            // Phase 3b: AsmInst lowerings land here, one category at a time.
            _ => false,
        }
    }
}
