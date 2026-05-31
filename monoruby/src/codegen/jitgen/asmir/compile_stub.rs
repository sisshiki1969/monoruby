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
use monoasm_macro::monoasm_arm64;

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
        frame.start_codepos = self.jit.get_current();
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
        frame: &mut AsmInfo,
        _labels: &SideExitLabels,
        inst: AsmInst,
        _class_version: DestLabel,
    ) -> bool {
        // lfp (x22) for slot access — same addressing as the VM's a64_op_ret:
        // slot `s` lives at `[lfp - s*8 - LFP_SELF]`.
        if std::env::var("DUMP").is_ok() { eprintln!("[asmir] {inst:?}"); }
        let lfp = GP::R14.a64().0;
        match inst {
            // Source-position record (no code).
            AsmInst::BcIndex(i) => {
                let pos = self.jit.get_current() - frame.start_codepos;
                frame.sourcemap.push((i, pos));
                true
            }
            // Bind a JIT label.
            AsmInst::Label(label) => {
                let label = frame.resolve_label(&mut self.jit, label);
                self.jit.bind_label(label);
                true
            }
            // Method prologue: establish fp/lr, reserve the local frame, and
            // nil-fill the non-argument locals/temps. Mirrors x86 `init_func`
            // (rbp == lfp + RBP_LOCAL_FRAME, so slots are lfp-relative here).
            AsmInst::Init {
                info,
                prologue_offset,
            } => {
                let prologue_bytes = prologue_offset.unwrap_concrete();
                // A64 `sub sp, sp, #imm` takes a 12-bit immediate; bail if the
                // frame is larger than that for now (rare; revisit with a
                // shifted/!2-instruction form).
                if prologue_bytes > 4095 {
                    return false;
                }
                monoasm_arm64!(&mut self.jit,
                    stp x29, x30, [sp, #-16]!;
                    mov x29, sp;
                );
                if prologue_bytes > 0 {
                    monoasm_arm64!(&mut self.jit,
                        sub sp, sp, #(prologue_bytes as u32);
                    );
                }
                let clear_len = info.reg_num - info.arg_num;
                if clear_len > 0 {
                    monoasm_arm64!(&mut self.jit, mov x9, (NIL_VALUE););
                    for i in 0..clear_len {
                        let off = (info.arg_num + i) as u32 * 8 + LFP_ARG0 as u32;
                        monoasm_arm64!(&mut self.jit,
                            sub x10, x(lfp), #(off);
                            str x9, [x10];
                        );
                    }
                }
                true
            }
            // Per-method ivar-cache prep: only needed when heap ivars are
            // accessed; otherwise a no-op. Bail on the heap path for now.
            AsmInst::Preparation => !frame.ivar_heap_accessed,
            // reg <- [lfp - slot*8 - LFP_SELF]
            AsmInst::StackToReg(slot, r) => {
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                let dst = r.a64().0;
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(lfp), #(off);
                    ldr x(dst), [x10];
                );
                true
            }
            // Return: the value is already in x0 (acc = Rax -> x0). Epilogue
            // matches the VM's a64_op_ret (`mov sp,x29; ldp; ret`).
            AsmInst::Ret => {
                monoasm_arm64!(&mut self.jit,
                    mov sp, x29;
                    ldp x29, x30, [sp], #16;
                    ret;
                );
                true
            }
            // reg <- literal Value (immediate).
            AsmInst::LitToReg(v, r) => {
                let dst = r.a64().0;
                let imm = v.id();
                monoasm_arm64!(&mut self.jit, mov x(dst), (imm););
                true
            }
            // [lfp - slot*8 - LFP_SELF] <- reg
            AsmInst::RegToStack(r, slot) => {
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                let src = r.a64().0;
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(lfp), #(off);
                    str x(src), [x10];
                );
                true
            }
            // [lfp - slot*8 - LFP_SELF] <- acc (R15 -> x23)
            AsmInst::AccToStack(slot) => {
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                let acc = GP::R15.a64().0;
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(lfp), #(off);
                    str x(acc), [x10];
                );
                true
            }
            // dst <- src
            AsmInst::RegMove(src, dst) => {
                let (s, d) = (src.a64().0, dst.a64().0);
                if s != d {
                    monoasm_arm64!(&mut self.jit, mov x(d), x(s););
                }
                true
            }
            // acc (R15 -> x23) <- reg
            AsmInst::RegToAcc(r) => {
                if r != GP::R15 {
                    let (acc, src) = (GP::R15.a64().0, r.a64().0);
                    monoasm_arm64!(&mut self.jit, mov x(acc), x(src););
                }
                true
            }
            // if Rax (x0) is non-zero (the local is already set), branch to dest.
            // Mirrors x86 `testq rax, rax; jnz dest`.
            AsmInst::CheckLocal(dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit, cbnz x(rax), dest;);
                true
            }
            // Phase 3b: more AsmInst lowerings land here, one category at a time.
            _ => false,
        }
    }
}
