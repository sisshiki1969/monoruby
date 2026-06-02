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
            // Type guard: deopt if `r`'s runtime class is not `class`. (aarch64
            // bails for not-yet-supported class kinds, hence the bool result.)
            AsmInst::GuardClass(r, class, deopt) => {
                return self.emit_guard_class(r, class, &labels[deopt]);
            }
            // Unconditional jump to a side-exit (deopt) label.
            AsmInst::Deopt(deopt) => self.emit_deopt(&labels[deopt]),
            // Branch to the error handler if the preceding runtime call failed
            // (returned a null/None result in the accumulator).
            AsmInst::HandleError(error) => self.emit_handle_error(&labels[error]),
            // Stack-overflow check before establishing a callee frame (aarch64
            // bails if the write-back needs an unsupported feature).
            AsmInst::CheckStack { write_back, error } => {
                return self.emit_check_stack(write_back, &labels[error], frame.base_stack_offset);
            }
            // GC safepoint (aarch64 bails like CheckStack).
            AsmInst::ExecGc { write_back, error } => {
                return self.emit_exec_gc(write_back, &labels[error], frame.base_stack_offset);
            }
            // Constant base-class guard: deopt if the constant's base class (in
            // the accumulator) is not the cached one.
            AsmInst::GuardConstBaseClass { base_class, deopt } => {
                self.emit_guard_const_base_class(base_class, &labels[deopt]);
            }
            // Constant version guard: deopt if the global constant version moved
            // since compilation.
            AsmInst::GuardConstVersion { const_version, deopt } => {
                self.emit_guard_const_version(const_version, &labels[deopt]);
            }
            // Store to a constant, bumping the global constant version (aarch64
            // bails if any xmm is live, hence the bool result).
            AsmInst::StoreConstant { id, using_xmm, error } => {
                return self.emit_store_constant(id, using_xmm, &labels[error]);
            }
            // Variable access (aarch64 bails on a live xmm / range overflow,
            // hence the bool results). gvar/cvar go via a runtime call; dynvar
            // walks the outer-LFP chain.
            AsmInst::LoadGVar { name, using_xmm } => return self.emit_load_gvar(name, using_xmm),
            AsmInst::StoreGVar { name, src, using_xmm } => {
                return self.emit_store_gvar(name, src, using_xmm);
            }
            AsmInst::LoadCVar { name, using_xmm } => return self.emit_load_cvar(name, using_xmm),
            AsmInst::LoadDynVar { src } => return self.emit_load_dyn_var(src),
            AsmInst::StoreDynVar { dst, src } => return self.emit_store_dyn_var(dst, src),
            // Runtime allocation / C-call family: each builds a heap object via
            // a runtime call (aarch64 bails on a live xmm / range overflow,
            // hence the bool results).
            AsmInst::CreateArray { src, len } => return self.emit_create_array(src, len),
            AsmInst::NewArray { callid, using_xmm } => {
                return self.emit_new_array(callid, using_xmm);
            }
            AsmInst::NewHash(args, len, using_xmm) => {
                return self.emit_new_hash(args, len, using_xmm);
            }
            AsmInst::NewRange { start, end, exclude_end, using_xmm } => {
                return self.emit_new_range(start, end, exclude_end, using_xmm);
            }
            AsmInst::ConcatStr { arg, len, using_xmm } => {
                return self.emit_concat_str(arg, len, using_xmm);
            }
            AsmInst::ToA { src, using_xmm } => return self.emit_to_a(src, using_xmm),
            AsmInst::DeepCopyLit(v, using_xmm) => return self.emit_deep_copy_lit(v, using_xmm),
            // Floating-point register transfer/convert family (aarch64 bails if
            // the FP pool register is not lowerable, hence the bool results).
            // `base` is the spill base; deopt is a side-exit label.
            AsmInst::FprMove(src, dst) => {
                return self.emit_fpr_move(src, dst, frame.base_stack_offset);
            }
            AsmInst::FprSwap(l, r) => return self.emit_fpr_swap(l, r, frame.base_stack_offset),
            AsmInst::F64ToFpr(f, x) => return self.emit_f64_to_fpr(f, x, frame.base_stack_offset),
            AsmInst::FixnumToFpr(r, x) => {
                return self.emit_fixnum_to_fpr(r, x, frame.base_stack_offset);
            }
            AsmInst::FloatToFpr(reg, x, deopt) => {
                return self.emit_float_to_fpr(reg, x, &labels[deopt], frame.base_stack_offset);
            }
            AsmInst::FprToStack(x, slot) => {
                return self.emit_fpr_to_stack(x, slot, frame.base_stack_offset);
            }
            // Not a shared instruction: hand off to the per-arch backend.
            other => return self.compile_asmir_arch(store, frame, labels, other, class_version),
        }
        true
    }
}
