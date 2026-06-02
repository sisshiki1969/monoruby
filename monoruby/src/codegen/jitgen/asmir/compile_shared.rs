//! Arch-neutral `AsmInst` lowering dispatcher (step ① of x86/aarch64 JIT
//! emission sharing).
//!
//! The AsmIR→machine-code lowering used to be two fully parallel matches —
//! `asmir/compile/*.rs` (x86, `monoasm!`) and `asmir/compile_stub.rs`
//! (aarch64, `monoasm_arm64!`). Instructions whose lowering is identical in
//! *structure* (only the emitted bytes differ) are handled here ONCE and call
//! tiny per-arch **emission primitives** (`emit_reg_move`, `emit_reg_to_stack`,
//! `emit_stack_to_reg`, `emit_lit_to_reg`). Everything else is forwarded to the
//! per-arch `compile_asmir_arch`. Coverage of the shared match grows one
//! instruction family at a time; see `doc/aarch64-x86-jit-differences.md`.

use super::*;

impl Codegen {
    ///
    /// Lower one `AsmInst`. The single entry point both backends' drivers call.
    ///
    /// Returns `true` if the instruction was emitted, `false` to bail (only the
    /// aarch64 backend bails today, on a not-yet-ported instruction; the x86
    /// backend always returns `true`). The shared arms never bail.
    ///
    pub(in crate::codegen::jitgen) fn compile_asmir(
        &mut self,
        store: &Store,
        frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        class_version: DestLabel,
    ) -> bool {
        match inst {
            // Source-position record (no machine code).
            AsmInst::BcIndex(i) => {
                let pos = self.jit.get_current() - frame.start_codepos;
                frame.sourcemap.push((i, pos));
            }
            // Bind a JIT label at the current position.
            AsmInst::Label(label) => {
                let label = frame.resolve_label(&mut self.jit, label);
                self.jit.bind_label(label);
            }
            // dst <- src
            AsmInst::RegMove(src, dst) => self.emit_reg_move(src, dst),
            // acc <- reg
            AsmInst::RegToAcc(r) => self.emit_reg_move(r, GP::R15),
            // [slot] <- acc
            AsmInst::AccToStack(slot) => self.emit_reg_to_stack(GP::R15, slot),
            // [slot] <- reg
            AsmInst::RegToStack(r, slot) => self.emit_reg_to_stack(r, slot),
            // reg <- [slot]
            AsmInst::StackToReg(slot, r) => self.emit_stack_to_reg(slot, r),
            // reg <- literal Value (immediate)
            AsmInst::LitToReg(v, r) => self.emit_lit_to_reg(v, r),
            // [slot] <- literal Value (aarch64 bails if the frame offset
            // exceeds the 12-bit immediate range, hence the bool result).
            AsmInst::LitToStack(v, slot) => return self.emit_lit_to_stack(v, slot),
            // Conditional branch on the truthiness of the accumulator.
            AsmInst::CondBr(brkind, dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                self.emit_cond_br(dest, brkind);
            }
            // Branch to dest if the accumulator is nil.
            AsmInst::NilBr(dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                self.emit_nil_br(dest);
            }
            // Branch to dest if the local (accumulator) is already set (non-zero).
            AsmInst::CheckLocal(dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                self.emit_check_local(dest);
            }
            // Not a shared instruction: hand off to the per-arch backend.
            other => return self.compile_asmir_arch(store, frame, labels, other, class_version),
        }
        true
    }
}
