//! aarch64 AsmIR→machine-code lowering.
//!
//! The arch-neutral front-end builds `AsmIr`; this drives the lowering on
//! aarch64. Every `AsmInst` and side exit is lowered: large frame/field/sp
//! offsets are materialized through scratch registers rather than bailing, and
//! the `...`-forwarding deferral (D1) is now lowered here too — the
//! source-routed `SetArgumentsForwarded` copy (`a64_set_arguments_forwarded_deferred`)
//! and the deopt-time rest-array rebuild (`a64_gen_forward_rest_materialize`).
//! So aarch64 never bails out of JIT compilation; `compile_asmir`'s `bool` is
//! vestigial. See `doc/aarch64-jitgen-plan.md`.

use super::*;
use crate::alloc::{BUMP_INLINE_LIMIT, CELL_SIZE_SHIFT, PAGE_DATA_OFFSET};
use crate::codegen::jitgen::asmir::ArrayIndexKind;
use crate::codegen::jitgen::asmir::compile_shared::{
    extend_ivar, set_array_integer_index, set_ivar, unreachable,
};
use crate::codegen::jitgen::lir::{
    LAluOp, LCond, LInst, LMem, LOperand, LReg, LSideExitKind, Lir,
};

/// Resolve a LIR register operand to its aarch64 register number. The scratch
/// pointer is `x9`.
fn a64_lreg(r: LReg) -> u32 {
    match r {
        LReg::Gp(v) => v.phys().a64().0,
        LReg::Scratch => 9,
    }
}
use monoasm_macro::monoasm_arm64;
use crate::codegen::jitgen::deopt_log::DeoptCause;
use crate::codegen::jitgen::lir::ConstMiss;

mod binary_op;
mod builtin;
mod constants;
mod defined;
mod definition;
mod index;
mod init_method;
mod method_call;
mod variables;


/// Signed aarch64 condition for a fixnum comparison. `BrIf` gives the
/// "taken-when-true" condition; `BrIfNot` gives its inverse (so a CmpBr lands
/// on the fall-through case). TEq behaves like Eq for integers.
fn a64_cond_for_cmp(kind: CmpKind, brkind: BrKind) -> monoasm::Cond {
    use monoasm::Cond;
    let taken = match kind {
        CmpKind::Eq | CmpKind::TEq => Cond::Eq,
        CmpKind::Ne => Cond::Ne,
        CmpKind::Lt => Cond::Lt,
        CmpKind::Le => Cond::Le,
        CmpKind::Gt => Cond::Gt,
        CmpKind::Ge => Cond::Ge,
    };
    match brkind {
        BrKind::BrIf => taken,
        BrKind::BrIfNot => match taken {
            Cond::Eq => Cond::Ne,
            Cond::Ne => Cond::Eq,
            Cond::Lt => Cond::Ge,
            Cond::Ge => Cond::Lt,
            Cond::Le => Cond::Gt,
            Cond::Gt => Cond::Le,
            other => other,
        },
    }
}

/// Like `a64_cond_for_cmp` but for `fcmp`: NaN (unordered) must compare false
/// for every operator except `!=`. After `fcmp`, NZCV is set so that `<` needs
/// `MI` (not `LT`, which is true when unordered) and `<=` needs `LS` (not `LE`);
/// the inverse for `BrIfNot` is always the ARM bit-complement of the condition.
fn a64_float_cond_for_cmp(kind: CmpKind, brkind: BrKind) -> monoasm::Cond {
    use monoasm::Cond;
    let taken = match kind {
        CmpKind::Eq | CmpKind::TEq => Cond::Eq,
        CmpKind::Ne => Cond::Ne,
        CmpKind::Lt => Cond::Mi,
        CmpKind::Le => Cond::Ls,
        CmpKind::Gt => Cond::Gt,
        CmpKind::Ge => Cond::Ge,
    };
    match brkind {
        BrKind::BrIf => taken,
        BrKind::BrIfNot => match taken {
            Cond::Eq => Cond::Ne,
            Cond::Ne => Cond::Eq,
            Cond::Mi => Cond::Pl,
            Cond::Ls => Cond::Hi,
            Cond::Gt => Cond::Le,
            Cond::Ge => Cond::Lt,
            other => other,
        },
    }
}


impl Codegen {

    /// Lower one block's `AsmIr`. `entry` (if any) is bound first; `exit` (if
    /// any) appends an unconditional branch to that basic block at the end.
    /// Every `AsmInst` and side exit lowers, so aarch64 never bails out of JIT
    /// compilation. (aarch64 half of the per-arch `gen_asm` the shared
    /// `gen_machine_code` driver calls; the x86 half is in `asmir.rs`.)
    pub(in crate::codegen::jitgen) fn gen_asm(
        &mut self,
        ir: AsmIr,
        store: &Store,
        frame: &mut AsmInfo,
        entry: Option<DestLabel>,
        exit: Option<BasicBlockId>,
        class_version: DestLabel,
        // Is this block reachable by fall-through from the code emitted just
        // before it? See `gen_machine_code`. When `false`, the body label binds
        // *after* the handlers and is the only way in, so the `b skip` that
        // jumps over the handlers is dead and we omit it.
        fallthrough_in: bool,
    ) {
        // Pure-deopt block (e.g. a loop's natural exit): its whole body is
        // `[Label(bb), Deopt(d)]`, i.e. a bare jump to its deopt handler. Emit
        // the deopt inline *at* the block label instead of laying a cold
        // handler and branching to it. Combined with the empty-bridge threading
        // in `gen_machine_code`, the predecessor's branch then lands straight on
        // the deopt code with no intervening `b`. Only the plain main-block case
        // (no `entry`/`exit` wrapper) is handled; anything else falls through to
        // the ordinary path. (The block's `BcIndex` source-position marker is
        // dropped here — perf/emit-asm cosmetic only, no machine-code effect.)
        if entry.is_none()
            && exit.is_none()
            && let Some((bb, deopt)) = ir.as_pure_deopt()
            && let Some((pc, wb, chain)) = ir.pure_deopt_target(deopt)
        {
            let wb = wb.clone();
            let bb_label = frame.resolve_label(&mut self.jit, bb);
            self.a64_gen_deopt(
                pc,
                &wb,
                bb_label,
                frame.loop_jit_spill_bytes,
                frame.base_stack_offset,
                chain,
            );
            return;
        }

        // Generate the block's side-exit (deopt/evict/error) handlers. They are
        // cold, so we lay them out here but jump over them (`b skip`); guards in
        // the main body branch *back* to them (short-range b.cond). aarch64 has
        // no separate cold page in this path, but `b` reaches them either way.
        let mut labels = SideExitLabels::new();
        let skip = if ir.side_exit.is_empty() {
            None
        } else {
            Some(self.jit.label())
        };
        // Only guard against fall-through into the handlers when the block can
        // actually be entered that way; a block reached solely by branches to
        // its (post-handler) body label needs no `b skip`.
        if let Some(skip) = &skip
            && fallthrough_in
        {
            monoasm_arm64!(&mut self.jit, b skip;);
        }
        #[cfg(feature = "deopt")]
        let mut deopt_table: std::collections::HashMap<
            (BytecodePtr, WriteBack, bool),
            (DestLabel, u32),
        > = std::collections::HashMap::new();
        #[cfg(not(feature = "deopt"))]
        let mut deopt_table: std::collections::HashMap<(BytecodePtr, WriteBack, bool), DestLabel> =
            std::collections::HashMap::new();
        // Loop-JIT entry sp-bump to undo before any exit resumes the VM.
        let bump = frame.loop_jit_spill_bytes;
        // §9 (9a, first brick): reify the side-exit handler block into a
        // whole-region `Lir` buffer, drained through `encode_linst` below. The
        // handlers sit between the `b skip` (already emitted) and `bind(skip)`,
        // so draining here — before the `skip` bind — preserves emission order
        // and is byte-identical. Labels are still created eagerly for the body to
        // reference; the drain is the seam the future phys-alloc pass slots into.
        let mut lir = Lir::new();
        #[cfg(feature = "deopt")]
        let mut created_at_iter = ir.created_at.into_iter();
        for side_exit in ir.side_exit {
            #[cfg(feature = "deopt")]
            let created_at = created_at_iter.next().flatten();
            #[cfg(feature = "deopt")]
            let mut exit_id = u32::MAX;
            let label = match side_exit {
                // Eviction falls back to the interpreter like a deopt (the
                // `__immediate_evict` logging is `cfg(deopt/profile)`-only).
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    #[cfg(feature = "deopt")]
                    {
                        exit_id = crate::codegen::jitgen::deopt_log::register_exit(
                            crate::codegen::jitgen::deopt_log::DeoptExit::Evict,
                        );
                    }
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::Evict,
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes: bump,
                        base: frame.base_stack_offset,
                        #[cfg(feature = "deopt")]
                        exit_id,
                    });
                    label
                }
                SideExit::Deoptimize(pc, wb, chain) => {
                    let key = (pc, wb, chain);
                    if let Some(entry) = deopt_table.get(&key) {
                        #[cfg(feature = "deopt")]
                        {
                            exit_id = entry.1;
                        }
                        #[cfg(feature = "deopt")]
                        let label = entry.0.clone();
                        #[cfg(not(feature = "deopt"))]
                        let label = entry.clone();
                        label
                    } else {
                        let label = self.jit.label();
                        #[cfg(feature = "deopt")]
                        {
                            exit_id = crate::codegen::jitgen::deopt_log::register_exit(
                                crate::codegen::jitgen::deopt_log::DeoptExit::Deopt { chain: key.2 },
                            );
                        }
                        lir.push(LInst::SideExit {
                            kind: LSideExitKind::Deopt { chain: key.2 },
                            pc: key.0,
                            wb: key.1.clone(),
                            entry: label.clone(),
                            loop_jit_spill_bytes: bump,
                            base: frame.base_stack_offset,
                            #[cfg(feature = "deopt")]
                            exit_id,
                        });
                        #[cfg(feature = "deopt")]
                        deopt_table.insert(key, (label.clone(), exit_id));
                        #[cfg(not(feature = "deopt"))]
                        deopt_table.insert(key, label.clone());
                        label
                    }
                }
                SideExit::RecompileDeoptimize(pc, wb, reason, target, chain) => {
                    let label = self.jit.label();
                    #[cfg(feature = "deopt")]
                    {
                        exit_id = crate::codegen::jitgen::deopt_log::register_exit(
                            crate::codegen::jitgen::deopt_log::DeoptExit::Recompile { reason, chain },
                        );
                    }
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::RecompileDeopt {
                            reason,
                            target,
                            chain,
                        },
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes: bump,
                        base: frame.base_stack_offset,
                        #[cfg(feature = "deopt")]
                        exit_id,
                    });
                    label
                }
                SideExit::Error(pc, wb, chain) => {
                    let label = self.jit.label();
                    lir.push(LInst::SideExit {
                        kind: LSideExitKind::Error { chain },
                        pc,
                        wb,
                        entry: label.clone(),
                        loop_jit_spill_bytes: bump,
                        base: frame.base_stack_offset,
                        #[cfg(feature = "deopt")]
                        exit_id,
                    });
                    label
                }
                // Evict(None) is a placeholder always overwritten with
                // Evict(Some(..)) before codegen (mirrors x86 gen_asm's
                // `_ => unreachable!()`).
                _ => unreachable!("unexpected {side_exit:?}"),
            };
            labels.push(
                label,
                #[cfg(feature = "deopt")]
                exit_id,
                #[cfg(feature = "deopt")]
                created_at,
            );
        }
        for inst in lir.into_insts() {
            self.encode_linst(inst);
        }
        if let Some(skip) = &skip {
            self.jit.bind_label(skip.clone());
        }

        if let Some(entry) = &entry {
            self.jit.bind_label(entry.clone());
        }
        for inst in ir.inst {
            self.compile_asmir(store, frame, &labels, inst, class_version.clone());
        }
        if let Some(exit) = exit {
            let exit = frame.resolve_bb_label(&mut self.jit, exit);
            monoasm_arm64!(&mut self.jit, b exit;);
        }
    }

    /// Release the spill region the loop-JIT entry pinned sp below
    /// (`emit_loop_jit_rsp_bump`) before resuming the interpreter. Unlike x86
    /// — whose VM frame is rbp-relative, so a stale sp is harmless — the
    /// aarch64 VM sets up callee frames sp-relative, so every loop-JIT exit
    /// that resumes the VM (deopt, error, raise, retry, redo) must restore sp
    /// first. `bytes` is `frame.loop_jit_spill_bytes` (0 for a non-loop frame
    /// or a loop without spill).
    ///
    /// Not the inverse of the entry, which pins an absolute depth rather than
    /// subtracting: adding the spill region back to
    /// `x29 - (total - PROLOGUE_OVERHEAD)` lands at
    /// `x29 - (base - PROLOGUE_OVERHEAD)` — the local area the VM's
    /// `init_method` reserves — whichever producer built the frame.
    /// Deliberate: the VM resumes here, and that is its own depth.
    fn a64_undo_loop_rsp_bump(&mut self, bytes: usize) {
        if bytes > 0 {
            self.a64_sp_add(bytes as u32);
        }
    }

    /// Deopt handler: write all live Ruby values back to the LFP (so the frame
    /// is GC-consistent and the interpreter can resume), set PC, and jump to
    /// the VM fetch loop. Mirrors x86 `side_exit_with_label` (deopt path).
    fn a64_gen_deopt(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
        chain: bool,
    ) {
        self.jit.bind_label(entry);
        // Write back FIRST, while the loop sp-bump still keeps sp below the
        // spill region: the write-back boxes spilled floats with calls whose
        // callee prologues push below sp, and undoing the bump first would let
        // those pushes overwrite the spill slots being read (see the x86 twin
        // `side_exit_with_label` and doc/regalloc_separation.md §39).
        self.a64_gen_write_back_for_deopt(wb, base);
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        // Chain escalation (`doc/chain_deopt.md` §5 step 4): convert every
        // suspended JIT frame in the caller chain before this frame resumes
        // in the interpreter. After the write-back, so the frame is fully
        // homed in the LFP for the walk.
        if chain {
            self.a64_call_chain_deopt();
        }
        let pc_ptr = pc.as_ptr() as u64;
        let fetch = self.vm_fetch();
        // PC == x21.
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc_ptr);
            b fetch;
        );
    }

    /// `runtime::chain_deopt(vm)` — the escalated-side-exit walk. x19 holds
    /// `&mut Executor`; LR is saved around the `blr` like every other runtime
    /// call emitted into a JIT body.
    fn a64_call_chain_deopt(&mut self) {
        let f = runtime::chain_deopt as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            str x30, [sp, #-16]!;              // save LR (16-aligned)
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;                // restore LR
        );
    }

    /// Error handler: write back, set PC to *this* instruction, and jump to
    /// `entry_raise` (which calls `handle_error`).
    ///
    /// Unlike x86 — where `gen_handle_error` sets PC to the next instruction
    /// and `init`'s `raise:` subtracts 16 (one bytecode op) before calling
    /// `handle_error` — aarch64's `entry_raise` passes PC through unchanged
    /// (matching the VM raise ops, which leave PC at the current instruction).
    /// So point PC at the raising instruction itself; otherwise the
    /// exception-table lookup in `handle_error` is off by one and an in-frame
    /// `rescue` / `ensure` is skipped.
    fn a64_gen_handle_error(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
        chain: bool,
    ) {
        self.jit.bind_label(entry);
        // Write back before undoing the bump — same spill-clobber reason as
        // `a64_gen_deopt` / the x86 `side_exit_with_label` (§39).
        self.a64_gen_write_back_for_deopt(wb, base);
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        // Chain escalation: the raise may be rescued *inside* this frame
        // (resuming it in the interpreter) or unwind through the suspended
        // callers — either way they must be converted first
        // (`doc/chain_deopt.md` §5 step 4 / §8.4).
        if chain {
            self.a64_call_chain_deopt();
        }
        let pc0 = pc.as_ptr() as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc0);
            b raise;
        );
    }

    /// Write back live values to LFP slots for a side exit, r14(x22)-relative
    /// (the local frame may be on the heap after a call returns). Mirrors x86
    /// `gen_write_back_for_deopt`, including the D1 deferred forwarding-rest
    /// materialization, which runs last (see `a64_gen_forward_rest_materialize`).
    fn a64_gen_write_back_for_deopt(&mut self, wb: &WriteBack, base: usize) {
        // Spill each live FP-pool register to its slot(s) as a boxed Float
        // Value, so the interpreter sees the up-to-date float after the deopt.
        for (fpr, slots) in &wb.fpr {
            for slot in slots {
                self.emit_fpr_to_stack(*fpr, *slot, base);
            }
        }
        let lfp = GP::R14.a64().0; // x22
        for (v, slot) in &wb.literal {
            self.a64_store_imm_to_slot(v.id(), *slot, lfp);
        }
        for slot in &wb.void {
            self.a64_store_imm_to_slot(NIL_VALUE as u64, *slot, lfp);
        }
        for (reg, slot) in &wb.gp {
            let off = slot.0 as u32 * 8 + LFP_SELF as u32;
            self.a64_frame_store(reg.a64().0, lfp, off);
        }
        // D1: materialize deferred forwarding-rest arrays. Runs last so the
        // literal loop above has already written the `dst` slot (mode `C(nil)`),
        // keeping the frame GC-consistent during the `create_array` call (which
        // may itself allocate).
        for (dst, src, len) in wb.forward_rest.clone() {
            self.a64_gen_forward_rest_materialize(dst, src, len);
        }
        // K1: materialize deferred `**kwrest` Hashes after the rest
        // arrays (each helper call may allocate; every not-yet-written
        // deferred slot still physically holds the `nil` the caller
        // stored, so the frame stays GC-consistent throughout).
        for (dst, table) in wb.forward_kwrest.clone() {
            self.a64_gen_forward_kwrest_materialize(dst, &table);
        }
    }

    /// K1: rebuild a deferred `**kwrest` Hash from the caller's kw slots
    /// for an interpreter resuming inside the trampoline frame. Twin of
    /// x86 `gen_forward_kwrest_materialize`: the caller `Lfp` handed to
    /// `correct_rest_kw` is derived from the caller frame pointer saved
    /// at `[x29]` (`lfp == fp - RBP_LOCAL_FRAME` in every JIT frame).
    fn a64_gen_forward_kwrest_materialize(&mut self, dst: SlotId, table: &[(IdentId, SlotId)]) {
        let lfp = GP::R14.a64().0; // x22
        let data = self.jit.const_align8();
        for (name, slot) in table {
            self.jit.const_i32(name.get() as i32);
            self.jit.const_i32(slot.0 as i32);
        }
        self.jit.const_i32(0);
        self.jit.const_i32(0);
        let f = runtime::correct_rest_kw as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            ldr x1, [x29];                     // caller fp
            mov x9, (RBP_LOCAL_FRAME as u64);
            sub x1, x1, x9;                    // caller lfp
            adr x0, data;                      // &table
            str x30, [sp, #-16]!;              // save LR
            mov x9, (f);
            blr x9;                            // x0 = kwrest Hash
            ldr x30, [sp], #16;                // restore LR
            mov x12, x0;
        );
        self.a64_frame_store(12, lfp, conv(dst) as u32);
    }

    /// Rebuild a deferred `...` rest `Array` for an interpreter resuming inside
    /// the trampoline frame `f`. Twin of x86 `gen_forward_rest_materialize`.
    /// The positional source lives in the *caller* (outermost, non-specialized)
    /// frame — `f` saved the caller's frame pointer at `[x29]` — so the array is
    /// built from `[caller_fp - rbp_local(src)]` and stored into `f`'s rest
    /// local `dst` (`x22`/LFP-relative). `create_array(ptr = &caller[src], len)`.
    /// x9/x10 are lowering scratch here (x10 is used internally by
    /// `a64_addr_sub`/`a64_frame_store` for large offsets).
    ///
    /// aarch64 counterpart of `JitModule::gen_chain_replay_stub`: emit this
    /// call site's chain-deopt conversion once, at compile time.
    ///
    /// ### calling convention
    /// - `x0` — the callee frame's fp (the suspended call's own frame)
    /// - `x1` — the caller frame's fp (the frame being converted)
    /// - `x2` — the caller frame's `Lfp`
    ///
    /// The three are parked in `x23`-`x25` across the helper calls — not
    /// `x19`-`x22`, which are the JIT's global registers — and the stub
    /// saves and restores those three itself along with LR.
    ///
    pub(in crate::codegen) fn a64_gen_chain_replay_stub(
        &mut self,
        replay: &ChainReplay,
        cont_stub: CodePtr,
    ) -> CodePtr {
        let entry = self.jit.get_current_address();
        let wb = replay.write_back_all();
        let base = replay.base();
        let f64_to_val_addr = self.jit.get_label_address(&self.f64_to_val).as_ptr() as u64;
        // Park the three arguments where the helper calls cannot clobber
        // them, and keep LR.
        monoasm_arm64!(&mut self.jit,
            stp x23, x24, [sp, #-32]!;
            stp x25, x30, [sp, #16];
            mov x23, x0;                       // callee fp
            mov x24, x1;                       // caller fp
            mov x25, x2;                       // caller lfp
        );
        for (fpr, slots) in wb.fpr_entries() {
            if slots.is_empty() {
                continue;
            }
            match replay.fpr_save_index(*fpr) {
                Some(i) => {
                    let off = 32 + 8 * i as u32;
                    monoasm_arm64!(&mut self.jit, ldr d0, [x23, #(off)];);
                }
                None => {
                    let off = (base as u32) - 24 + 8 * ((fpr.0 - PHYS_FPR_POOL) as u32);
                    self.a64_addr_sub(9, 24, off);
                    monoasm_arm64!(&mut self.jit, ldr d0, [x9];);
                }
            }
            // Absolute call, not `bl f64_to_val`. This stub is emitted on
            // page 1 (see `register_chain_exit`) while the helper sits on
            // page 0, and the two are separately mapped: `BL` reaches
            // +/-128MB, so a label-relative branch between them is only
            // sometimes in range — it took a macOS arm64 run to fall out of
            // it ("B/BL displacement out of range"). Every other call in
            // this stub already goes through a register for the same
            // reason. x86-64's `call` has +/-2GB and does not care.
            monoasm_arm64!(&mut self.jit,
                mov x9, (f64_to_val_addr);
                blr x9;                        // x0 = Value(f64)
            );
            for slot in slots {
                self.a64_frame_store(0, 25, conv(*slot) as u32);
            }
        }
        for (v, slot) in wb.literal_entries() {
            monoasm_arm64!(&mut self.jit, mov x9, (v.id()););
            self.a64_frame_store(9, 25, conv(*slot) as u32);
        }
        for slot in wb.void_entries() {
            monoasm_arm64!(&mut self.jit, mov x9, (NIL_VALUE as u64););
            self.a64_frame_store(9, 25, conv(*slot) as u32);
        }
        debug_assert!(wb.gp_is_empty());
        for (dst, src, len) in wb.forward_rest_entries() {
            let f = runtime::create_array as *const () as u64;
            monoasm_arm64!(&mut self.jit, ldr x9, [x24];);
            self.a64_addr_sub(0, 9, rbp_local(*src) as u32);
            monoasm_arm64!(&mut self.jit,
                mov x1, (*len as u64);
                mov x9, (f);
                blr x9;
            );
            self.a64_frame_store(0, 25, conv(*dst) as u32);
        }
        for (dst, table) in wb.forward_kwrest_entries() {
            let data = self.jit.const_align8();
            for (name, slot) in table.iter() {
                self.jit.const_i32(name.get() as i32);
                self.jit.const_i32(slot.0 as i32);
            }
            self.jit.const_i32(0);
            self.jit.const_i32(0);
            let f = runtime::correct_rest_kw as *const () as u64;
            // Load the caller's fp into a scratch and subtract *out of* it:
            // `a64_addr_sub` materializes an offset over 4095 into `addr`,
            // so `addr` doubling as `base` would destroy the base before it
            // is read. The `forward_rest` loop above has this right; this
            // one did not, and the debug assertion caught it on a macOS
            // arm64 run (`forwarded_struct_rest_native`).
            monoasm_arm64!(&mut self.jit, ldr x9, [x24];);
            self.a64_addr_sub(1, 9, RBP_LOCAL_FRAME as u32);
            monoasm_arm64!(&mut self.jit,
                adr x0, data;
                mov x9, (f);
                blr x9;
            );
            self.a64_frame_store(0, 25, conv(*dst) as u32);
        }
        // The two frame stores: the per-site continuation word into the
        // callee's cont-frame pad (CFP+32 == callee_fp - BP_CFP + 32), and
        // the callee's return address pointed at the shared VM stub.
        let cont = replay.cont_data();
        let pad_off = (32 - BP_CFP) as u32;
        let stub_addr = cont_stub.as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (cont);
            str x9, [x23, #(pad_off)];
            mov x9, (stub_addr);
            str x9, [x23, #(8)];
            ldp x25, x30, [sp, #16];
            ldp x23, x24, [sp], #32;
            ret;
        );
        entry
    }

    fn a64_gen_forward_rest_materialize(&mut self, dst: SlotId, src: SlotId, len: u16) {
        let lfp = GP::R14.a64().0; // x22
        let f = runtime::create_array as *const () as u64;
        // x9 = caller fp (the value `f` saved at `[x29]`); x0 = &caller[src].
        monoasm_arm64!(&mut self.jit, ldr x9, [x29];);
        self.a64_addr_sub(0, 9, rbp_local(src) as u32);
        monoasm_arm64!(&mut self.jit,
            mov x1, (len as u64);
            str x30, [sp, #-16]!;              // save LR (16-aligned)
            mov x9, (f);
            blr x9;                            // x0 = create_array(ptr, len)
            ldr x30, [sp], #16;                // restore LR
        );
        self.a64_frame_store(0, lfp, conv(dst) as u32);
    }

    /// `[sp - off] <- x9`. The callee frame is built at negative offsets from
    /// the current sp (mirrors x86 `[rsp - off]`).
    fn a64_store_x9_below_sp(&mut self, off: u32) {
        // Callers pass frame-field offsets (RSP_LOCAL_FRAME + LFP_*), all well
        // within the unscaled 9-bit range, so a single `stur` always suffices.
        debug_assert!(off <= 256, "store-below-sp offset out of stur range");
        monoasm_arm64!(&mut self.jit, stur x9, [sp, #(-(off as i32))];);
    }

    /// Lower `MethodRet`: an explicit `return` (possibly non-local). Set PC to
    /// *this* instruction, call `err_method_return(vm, globals, val)` with the
    /// value in rax, then jump to `entry_raise`.
    ///
    /// PC must point at the raising instruction (not the next one): aarch64's
    /// `entry_raise` passes PC straight to `handle_error` without the x86 `-16`
    /// fixup, so a `pc + 1` here would leave `handle_error`'s exception-table
    /// lookup off by one and skip the `ensure` body protecting the `return`.
    fn a64_method_ret(&mut self, pc: BytecodePtr) {
        let pc0 = pc.as_ptr() as u64;
        let f = runtime::err_method_return as *const () as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc0);
            mov x2, x0;       // val (was in rax/x0)
            mov x0, x19;      // vm
            mov x1, x20;      // globals
            mov x9, (f);
            blr x9;
            b raise;
        );
    }

    /// Lower `BlockBreak`: a `break` out of a block. Same shape as
    /// `a64_method_ret` (set PC, call the error builder with the break value in
    /// x0, jump to `entry_raise`) but through `err_block_break`, which unwinds
    /// to the block's defining method rather than the current frame's caller.
    ///
    /// As in `a64_method_ret`, PC points at *this* instruction (not `pc + 1`):
    /// aarch64's `entry_raise` hands PC to `handle_error` without the x86 `-16`
    /// fixup, so storing `pc` (the value x86 reaches via `pc + 1` then `-16`)
    /// keeps the exception-table lookup aligned with the raising instruction.
    fn a64_block_break(&mut self, pc: BytecodePtr) {
        let pc0 = pc.as_ptr() as u64;
        let f = runtime::err_block_break as *const () as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc0);
            mov x2, x0;       // val (was in rax/x0)
            mov x0, x19;      // vm
            mov x1, x20;      // globals
            mov x9, (f);
            blr x9;
            b raise;
        );
    }

    /// `[lfp - slot*8 - LFP_SELF] <- imm` via a scratch register (x9/x10).
    fn a64_store_imm_to_slot(&mut self, imm: u64, slot: SlotId, lfp: u32) {
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        monoasm_arm64!(&mut self.jit, mov x9, (imm););
        self.a64_frame_store(9, lfp, off);
    }

    // ---- spill-aware FP-register access -----------------------------------
    // The FP register allocator keeps unboxed floats in the pool D2-D15
    // (`PHYS_FPR_POOL` = 14 ⇒ `FPRegLoc::Xmm(2..=15)`); when it needs more live
    // floats than that, the overflow spills to frame slots
    // (`FPRegLoc::Spill(off)` ⇒ `[x29-off]`, mirroring x86's `[rbp-off]`).
    // D0/D1 are reserved scratch and never alias a pool register, so they carry
    // spilled operands during an op. The common (pool-resident) case emits
    // exactly the same code as before — `read`/`wtmp` return the pool register
    // and emit nothing, `commit` is a no-op.
    //
    // Pool save/restore invariant: D2-D7 are AAPCS64 caller-saved, D8-D15
    // callee-saved, so the Rust↔JIT boundary (the invoker prologue/epilogue and
    // the fiber-switch `a64_{push,pop}_callee_save`) preserves D8-D15 for the
    // Rust caller. Within the JIT world the whole pool is treated as
    // caller-saved — `FprSave`/`FprRestore` spill the live subset around any
    // clobbering call (a JIT→JIT `blr` clobbers D2-D15; a C-call's Rust callee
    // preserves D8-D15 but the spill is harmless), and `f64_to_val`'s heap path
    // saves D2-D7 while relying on `float_heap` to preserve D8-D15.

    /// Place `src`'s value into physical D-register `dreg` unconditionally
    /// (`fmov` for a pool register, a frame load for a spill slot).
    fn a64_fpr_load(&mut self, src: FPReg, dreg: u32, base: usize) {
        match PhysMap::new(base).resolve(src) {
            FPRegLoc::Xmm(p) => {
                if p as u32 != dreg {
                    monoasm_arm64!(&mut self.jit, fmov d(dreg), d(p as u32););
                }
            }
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;
                ldr d(dreg), [x10];
            ),
        }
    }

    /// Spill an FP-pool register to `slot` as a boxed Float `Value` (via
    /// `f64_to_val`), r14(x22)-relative. Shared by the `FprToStack` LIR op and
    /// the deopt write-back.
    fn emit_fpr_to_stack(&mut self, src: FPReg, slot: SlotId, base: usize) {
        let lfp = GP::R14.a64().0;
        let f64_to_val = self.f64_to_val.clone();
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        self.a64_fpr_load(src, 0, base); // value -> d0 (pool fmov or spill load)
        monoasm_arm64!(&mut self.jit, bl f64_to_val;); // x0 = Value(f64)
        self.a64_frame_store(0, lfp, off);
    }

    /// Store physical D-register `dreg` into `dst` unconditionally (`fmov` for a
    /// pool register, a frame store for a spill slot).
    fn a64_fpr_save(&mut self, dst: FPReg, dreg: u32, base: usize) {
        match PhysMap::new(base).resolve(dst) {
            FPRegLoc::Xmm(p) => {
                if p as u32 != dreg {
                    monoasm_arm64!(&mut self.jit, fmov d(p as u32), d(dreg););
                }
            }
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;
                str d(dreg), [x10];
            ),
        }
    }

    /// Register holding `src`'s value, ready to read: its pool register if
    /// resident (no code emitted), otherwise the spill loaded into scratch
    /// `dreg`.
    fn a64_fpr_read(&mut self, src: FPReg, dreg: u32, base: usize) -> u32 {
        match PhysMap::new(base).resolve(src) {
            FPRegLoc::Xmm(p) => p as u32,
            FPRegLoc::Spill(_) => {
                self.a64_fpr_load(src, dreg, base);
                dreg
            }
        }
    }

    /// Register to write `dst` into: its pool register if resident, else scratch
    /// `dreg` (the caller must follow with `a64_fpr_commit`). Emits nothing.
    fn a64_fpr_wtmp(&self, dst: FPReg, dreg: u32, base: usize) -> u32 {
        match PhysMap::new(base).resolve(dst) {
            FPRegLoc::Xmm(p) => p as u32,
            FPRegLoc::Spill(_) => dreg,
        }
    }

    /// Flush scratch `dreg` back to `dst`'s spill slot; a no-op when `dst` is
    /// pool-resident (the op already wrote its pool register in place).
    fn a64_fpr_commit(&mut self, dst: FPReg, dreg: u32, base: usize) {
        if let FPRegLoc::Spill(_) = PhysMap::new(base).resolve(dst) {
            self.a64_fpr_save(dst, dreg, base);
        }
    }

    // ---- emission primitives (aarch64) ------------------------------------
    // Tiny arch-specific helpers the arch-neutral `compile_asmir` dispatcher
    // calls. The x86 twins live in `compile.rs`. Slot `s` lives at
    // `[lfp(x22) - s*8 - LFP_SELF]` (same addressing as the VM's `a64_op_ret`).

    /// Trap for statically-unreachable code: call the panicking helper.
    pub(in crate::codegen::jitgen) fn emit_unreachable(&mut self) {
        let f = unreachable as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;
        );
    }

    ///
    /// Per-arch (aarch64) LIR encoder seam (Phase-1 Stage 2).
    ///
    /// Lower one already-register-allocated `LInst` to machine code via
    /// `monoasm_arm64!`, emitting byte-identical output to the hand-written
    /// `emit_*` primitive it replaces (and legalizing immediates/displacements
    /// through scratch x9/x10 as the migrated families grow). Only the migrated
    /// families are implemented; the rest `todo!()` until ported. See
    /// `doc/lir.md`.
    ///
    pub(in crate::codegen::jitgen) fn encode_linst(&mut self, inst: LInst) {
        // (§9a-ii) Buffering pass: collect the lowered op, don't emit. The
        // region driver drains the buffer (with `lir_buf` cleared) afterwards.
        if let Some(buf) = self.lir_buf.as_mut() {
            buf.push(inst);
            return;
        }
        match inst {
            // dst <- src (elided when the physical registers coincide)
            LInst::Mov { dst, src } => {
                let (s, d) = (src.phys().a64().0, dst.phys().a64().0);
                if s != d {
                    monoasm_arm64!(&mut self.jit, mov x(d), x(s););
                }
            }
            // dst <- imm (monoasm_arm64! expands a 64-bit immediate to the
            // movz/movk sequence as needed)
            LInst::LoadImm { dst, imm } => {
                let d = dst.phys().a64().0;
                monoasm_arm64!(&mut self.jit, mov x(d), (imm););
            }
            // dst <- [lfp - slot]. `a64_frame_load` legalizes the (negative)
            // frame displacement: it folds small offsets into `ldur` and
            // materializes large ones through scratch x10.
            LInst::Load {
                dst,
                mem: LMem::Slot(slot),
            } => {
                let lfp = GP::R14.a64().0;
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                self.a64_frame_load(a64_lreg(dst), lfp, off);
            }
            // dst <- [base + disp] (object field). `a64_field_load` legalizes
            // the (positive) displacement: scaled ldr immediate, else scratch
            // x10 materialization.
            LInst::Load {
                dst,
                mem: LMem::Field { base, disp },
            } => {
                self.a64_field_load(a64_lreg(dst), a64_lreg(base), disp as u32);
            }
            // dst <- bool([base + disp]): 32-bit raw-bool field → Ruby bool Value.
            // `lsl` clears the low 3 bits, so `add #FALSE_VALUE` == `orr`.
            LInst::BoolFieldToReg { dst, base, disp } => {
                let (d, b) = (dst.a64().0, base.a64().0);
                monoasm_arm64!(&mut self.jit,
                    ldr w(d), [x(b), #(disp as u32)];
                    lsl x(d), x(d), #3;
                    add x(d), x(d), #(FALSE_VALUE);
                );
            }
            // dst <- fixnum(Array#size): inline-or-heap length, fixnum-tagged.
            // `lsl` clears bit 0, so `add #1` == `orr #1`. Scratch x9 holds heap_len.
            LInst::ArrayLenFixnum { dst, base } => {
                let (d, b) = (dst.a64().0, base.a64().0);
                monoasm_arm64!(&mut self.jit,
                    ldr x(d), [x(b), #(RVALUE_OFFSET_ARY_CAPA as u32)];
                    ldr x9, [x(b), #(RVALUE_OFFSET_HEAP_LEN as u32)];
                    cmp x(d), #(ARRAY_INLINE_CAPA as u32);
                    csel x(d), x9, x(d), gt;
                    lsl x(d), x(d), #1;
                    add x(d), x(d), #1;
                );
            }
            // dst <- fixnum(String#bytesize): inline-or-heap byte length, tagged.
            LInst::StringLenFixnum { dst, base } => {
                let (d, b) = (dst.a64().0, base.a64().0);
                monoasm_arm64!(&mut self.jit,
                    ldr x(d), [x(b), #(RVALUE_OFFSET_ARY_CAPA as u32)];
                    ldr x9, [x(b), #(RVALUE_OFFSET_HEAP_LEN as u32)];
                    cmp x(d), #(STRING_INLINE_CAP as u32);
                    csel x(d), x9, x(d), gt;
                    lsl x(d), x(d), #1;
                    add x(d), x(d), #1;
                );
            }
            // dst <- (src == nil) ? true : false (Ruby bool). x3(Rsi) scratch.
            LInst::IsNilToBool { dst, src } => {
                let (d, s, sc) = (dst.a64().0, src.a64().0, GP::Rsi.a64().0);
                monoasm_arm64!(&mut self.jit,
                    mov  x(d), #(FALSE_VALUE);
                    mov  x(sc), #(TRUE_VALUE);
                    cmp  x(s), #(NIL_VALUE);
                    csel x(d), x(sc), x(d), eq;
                );
            }
            // dst <- (!src) ? true : false (Ruby bool). Destroys src; x9/x3 scratch.
            LInst::NotToBool { dst, src } => {
                let (d, s, sc) = (dst.a64().0, src.a64().0, GP::Rsi.a64().0);
                monoasm_arm64!(&mut self.jit,
                    mov  x9, #(0x10);
                    orr  x(s), x(s), x9;
                    mov  x(d), #(TRUE_VALUE);
                    mov  x(sc), #(FALSE_VALUE);
                    cmp  x(s), #(FALSE_VALUE);
                    csel x(d), x(d), x(sc), eq;
                );
            }
            // Math.sqrt: fcmp vs 0.0; NaN (unordered, Vs) -> sqrt, negative (Mi) -> deopt.
            LInst::MathSqrt {
                fsrc,
                fret,
                deopt,
                base,
            } => {
                self.a64_fpr_into_d0(fsrc, base);
                let do_sqrt = self.jit.label();
                monoasm_arm64!(&mut self.jit, fcmp d0, #0.0;);
                self.jit.bcond_label(monoasm::Cond::Vs, &do_sqrt);
                self.jit.bcond_label(monoasm::Cond::Mi, &deopt);
                self.jit.bind_label(do_sqrt);
                if let Some(fret) = fret {
                    monoasm_arm64!(&mut self.jit, fsqrt d0, d0;);
                    self.a64_d0_into_fpr(fret, base);
                }
            }
            // Integer#succ: tagged +1 (= +2), deopt on signed overflow.
            LInst::IntegerSucc { reg, deopt } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit, adds x(r), x(r), #(2u32););
                self.jit.bcond_label(monoasm::Cond::Vs, &deopt);
            }
            // Kernel#block_given?: walk to the frame `yield` would read its
            // block from, then report whether its block slot is set &
            // non-nil. That is the *end* of the outer chain — mirroring
            // `Lfp::yield_home`: unlike `Lfp::outermost` there is NO stop at
            // a `proc_method` (define_method body) boundary; yield keeps
            // block semantics there and ignores the call-site block (CRuby).
            LInst::BlockGiven { dst } => {
                let (d, lfp, rdi) = (dst.a64().0, GP::R14.a64().0, GP::Rdi.a64().0);
                let exit = self.jit.label();
                let walk = self.jit.label();
                let found = self.jit.label();
                monoasm_arm64!(&mut self.jit,
                    mov x(rdi), x(lfp);              // rdi = current LFP
                walk:
                    ldr x9, [x(rdi)];                // outer LFP (LFP_OUTER == 0)
                    cbz x9, found;                   // no outer -> rdi is the home
                    mov x(rdi), x9;
                    b walk;
                found:
                    mov x(d), #(FALSE_VALUE);
                    sub x9, x(rdi), #(LFP_BLOCK as u32);
                    ldr x(rdi), [x9];
                    cbz x(rdi), exit;
                    cmp x(rdi), #(NIL_VALUE);
                );
                self.jit.bcond_label(monoasm::Cond::Eq, &exit);
                monoasm_arm64!(&mut self.jit, mov x(d), #(TRUE_VALUE););
                self.jit.bind_label(exit);
            }
            // [lfp - slot] <- src (legalized like `Load`).
            // aarch64 already addresses every slot via the LFP, so `Slot` and
            // `LfpSlot` lower identically here (the distinction only matters on
            // x86, where ordinary `Slot` is rbp-relative).
            LInst::Store {
                src,
                mem: LMem::Slot(slot) | LMem::LfpSlot(slot),
            } => {
                let lfp = GP::R14.a64().0;
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                self.a64_frame_store(src.a64().0, lfp, off);
            }
            // [base + disp] <- src (object field; `a64_field_store` legalizes
            // the positive displacement).
            LInst::Store {
                src,
                mem: LMem::Field { base, disp },
            } => {
                self.a64_field_store(src.a64().0, a64_lreg(base), disp as u32);
            }
            // [rsp + (disp - RSP_LOCAL_FRAME)] <- src (callee-frame arg slot).
            // `a64_rsp_slot_addr` forms the address in scratch x10.
            LInst::Store {
                src,
                mem: LMem::RspRel { disp },
            } => {
                self.a64_rsp_slot_addr(disp, 10);
                let s = src.a64().0;
                monoasm_arm64!(&mut self.jit, str x(s), [x10];);
            }
            // [lfp - slot] <- imm. aarch64 has no store-immediate, so the
            // immediate is staged through scratch x9 (no allocated GP clobbered),
            // then stored via the legalizing `a64_frame_store`.
            LInst::StoreImm {
                imm,
                mem: LMem::Slot(slot),
            } => {
                let lfp = GP::R14.a64().0;
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                monoasm_arm64!(&mut self.jit, mov x9, (imm););
                self.a64_frame_store(9, lfp, off);
            }
            // [rsp + (disp - RSP_LOCAL_FRAME)] <- imm (callee-frame arg slot):
            // address in x10, immediate staged through x9.
            LInst::StoreImm {
                imm,
                mem: LMem::RspRel { disp },
            } => {
                self.a64_rsp_slot_addr(disp, 10);
                monoasm_arm64!(&mut self.jit, mov x9, (imm); str x9, [x10];);
            }
            // dst <op>= imm (in-place register/immediate ALU; the only Alu
            // shape produced so far, from RegAdd/RegSub). The immediate is
            // staged through scratch x9; SP (reg 31 == XZR in the
            // shifted-register form) is updated via x10. No-op when imm == 0.
            LInst::Alu {
                op,
                dst,
                lhs,
                rhs: LOperand::Imm(i),
            } if dst == lhs => {
                if i != 0 {
                    let d = dst.phys().a64().0;
                    let imm = i as u64;
                    match (op, dst.phys() == GP::Rsp) {
                        (LAluOp::Add, false) => {
                            monoasm_arm64!(&mut self.jit, mov x9, (imm); add x(d), x(d), x9;)
                        }
                        (LAluOp::Add, true) => monoasm_arm64!(&mut self.jit,
                            mov x9, (imm); mov x10, sp; add x10, x10, x9; mov sp, x10;
                        ),
                        (LAluOp::Sub, false) => {
                            monoasm_arm64!(&mut self.jit, mov x9, (imm); sub x(d), x(d), x9;)
                        }
                        (LAluOp::Sub, true) => monoasm_arm64!(&mut self.jit,
                            mov x9, (imm); mov x10, sp; sub x10, x10, x9; mov sp, x10;
                        ),
                        _ => todo!(
                            "LIR encode (aarch64): Alu {op:?} imm not yet migrated (Phase-1 Stage > 2-C)"
                        ),
                    }
                }
            }
            // Set flags from `lhs - rhs`. An `Imm` (the operand's raw tagged-
            // fixnum bits) is staged through scratch x9, matching
            // `a64_cmp_integer`.
            LInst::Cmp { lhs, rhs } => {
                let l = lhs.phys().a64().0;
                match rhs {
                    LOperand::Reg(r) => {
                        let r = r.phys().a64().0;
                        monoasm_arm64!(&mut self.jit, cmp x(l), x(r););
                    }
                    LOperand::Imm(i) => {
                        let imm = i as u64;
                        monoasm_arm64!(&mut self.jit, mov x9, (imm); cmp x(l), x9;);
                    }
                }
            }
            // Conditional branch on the preceding `Cmp` (mirrors
            // `bcond_label(a64_cond_for_cmp(..), dest)`; the BrKind inversion is
            // folded into `cond` by the builder).
            // Unconditional branch (a dispatch arm funnelling into its merge).
            LInst::Br(target) => {
                monoasm_arm64!(&mut self.jit, b target;);
            }
            LInst::CondBr { cond, target } => {
                let c = match cond {
                    LCond::Eq => monoasm::Cond::Eq,
                    LCond::Ne => monoasm::Cond::Ne,
                    LCond::Lt => monoasm::Cond::Lt,
                    LCond::Le => monoasm::Cond::Le,
                    LCond::Gt => monoasm::Cond::Gt,
                    LCond::Ge => monoasm::Cond::Ge,
                };
                self.jit.bcond_label(c, &target);
            }
            // Ruby-truthiness branch: `orr 0x10` folds nil(0x04)/false(0x14) to
            // FALSE_VALUE; truthy (!= FALSE) takes Ne, falsy takes Eq.
            LInst::BranchTruthy { negate, target } => {
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x10, (0x10);
                    orr x(rax), x(rax), x10;
                    cmp x(rax), #(FALSE_VALUE as u32);
                );
                let cond = if negate {
                    monoasm::Cond::Eq
                } else {
                    monoasm::Cond::Ne
                };
                self.jit.bcond_label(cond, &target);
            }
            LInst::BranchIfNil { target } => {
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit, cmp x(rax), #(NIL_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Eq, &target);
            }
            LInst::BranchIfNonzero { target } => {
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit, cbnz x(rax), target;);
            }
            // GC write barrier (aarch64 takes the parent register explicitly).
            LInst::WriteBarrier { parent, value } => {
                self.emit_write_barrier(parent, value);
            }
            // reg <- nil if reg == 0 (aarch64: branchless csel).
            LInst::NilIfZero { reg } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (NIL_VALUE);
                    cmp x(r), #(0u32);
                    csel x(r), x(r), x9, ne;
                );
            }
            // Class guard: deopt unless `reg`'s runtime class matches.
            LInst::GuardClass { reg, class, deopt } => {
                self.a64_guard_class(reg, class, &deopt);
            }
            // Dispatch arm: the miss is the next arm, not a side exit. aarch64
            // has no `jit_class_guard_fail` stub (the profile table is fed by
            // the x86 dispatch stub only), so guards and arms lower alike
            // here — the ops stay distinct so the distinction survives if
            // aarch64 grows the recorder.
            LInst::BrClassNe { reg, class, target } => {
                self.a64_guard_class(reg, class, &target);
            }
            // Class-set guard: membership chain built from the single-class
            // guard — each class's check falls through on match (branch to
            // ok) and moves to the next candidate on mismatch; the last
            // candidate's mismatch is the real deopt.
            // Same membership chain as `GuardClassIn`, with the last
            // candidate's miss going to the next arm instead of a side exit.
            LInst::BrClassNotIn {
                reg,
                classes,
                target,
            } => {
                let ok = self.jit.label();
                let len = classes.len();
                for (i, class) in classes.iter().enumerate() {
                    if i + 1 < len {
                        let next = self.jit.label();
                        self.a64_guard_class(reg, *class, &next);
                        monoasm_arm64!(&mut self.jit,
                            b ok;
                        );
                        self.jit.bind_label(next);
                    } else {
                        self.a64_guard_class(reg, *class, &target);
                    }
                }
                self.jit.bind_label(ok);
            }
            LInst::GuardClassIn {
                reg,
                classes,
                deopt,
            } => {
                let ok = self.jit.label();
                let len = classes.len();
                for (i, class) in classes.iter().enumerate() {
                    if i + 1 < len {
                        let next = self.jit.label();
                        self.a64_guard_class(reg, *class, &next);
                        monoasm_arm64!(&mut self.jit,
                            b ok;
                        );
                        self.jit.bind_label(next);
                    } else {
                        self.a64_guard_class(reg, *class, &deopt);
                    }
                }
                self.jit.bind_label(ok);
            }
            // Type guard: deopt unless `reg` is an Array (immediate check, then
            // the RValue.ty byte).
            LInst::GuardArrayTy { reg, deopt } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (0b111);
                    and x9, x(r), x9;
                    cbnz x9, deopt;                              // immediate -> deopt
                    ldrb w9, [x(r), #(RVALUE_OFFSET_TY as u32)]; // RValue.ty (u8)
                    cmp x9, #(ObjTy::ARRAY.get() as u32);
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
            }
            // Deopt if the receiver (rdi) is frozen.
            LInst::GuardFrozen { deopt } => {
                let rdi = GP::Rdi.a64().0;
                monoasm_arm64!(&mut self.jit,
                    ldrb w9, [x(rdi), #(RVALUE_OFFSET_FLAG as u32)];
                    mov x10, (0b10);
                    and x9, x9, x10;       // isolate the frozen bit
                    cbnz x9, deopt;        // frozen -> deopt
                );
            }
            // Constant-load base-class guard.
            LInst::GuardConstBaseClass { base_class, deopt } => {
                let rax = GP::Rax.a64().0;
                let cached = base_class.id() as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x10, (cached);
                    cmp x(rax), x10;
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
            }
            // Constant-load version guard.
            // The snapshot side is the unit's shared patchable word
            // (`Codegen::unit_const_version`), so a successful const salvage
            // re-validates every guard in the unit with one store (mirrors
            // x86). A miss always deopts; `miss` says what it tries first —
            // the salvaging recompile entry (the class-version guard's shape)
            // or, for a block root, salvage alone.
            LInst::GuardConstVersion {
                const_version: _,
                miss,
                deopt,
            } => {
                let gv_addr = self
                    .jit
                    .get_label_address(&self.const_version_label())
                    .as_ptr() as u64;
                let unit_word = self
                    .unit_const_version
                    .clone()
                    .expect("const guard emitted outside a constant-folding unit");
                let unit_addr = self.jit.get_label_address(&unit_word).as_ptr() as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (gv_addr);
                    ldr x9, [x9];
                    mov x10, (unit_addr);
                    ldr x10, [x10];
                    cmp x9, x10;
                );
                {
                    let miss_label = self.jit.label();
                    let done = self.jit.label();
                    self.jit.bcond_label(monoasm::Cond::Ne, &miss_label);
                    monoasm_arm64!(&mut self.jit, b done;);
                    self.jit.bind_label(miss_label);
                    match miss {
                        ConstMiss::Recompile(position) => self.a64_call_recompile(
                            position,
                            RecompileReason::ConstVersionGuardFailed,
                        ),
                        ConstMiss::Salvage => self.a64_call_salvage_const(),
                    }
                    monoasm_arm64!(&mut self.jit, b deopt;);
                    self.jit.bind_label(done);
                }
            }
            // Block-passing side-effect guard: deopt if the frame was captured
            // or invalidated.
            LInst::GuardCapture { deopt } => {
                let lfp = GP::R14.a64().0; // x22
                let off = (LFP_META as i64 - META_KIND as i64) as u32; // == 1 (kind byte)
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(lfp), #(off);
                    ldrb w9, [x10];
                    mov x11, (0b1000_1000u64);
                    tst x9, x11;                 // captured or invalidated?
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // set -> deopt to VM
            }
            // BOP-redefinition guard.
            LInst::CheckBOP { deopt, version } => {
                let flag_addr = self
                    .jit
                    .get_label_address(&self.bop_redefined_flags)
                    .as_ptr() as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (flag_addr);
                    ldr w9, [x9];
                    mov x10, (version as u64);
                    cmp x9, x10;      // version moved since compile -> deopt
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
            }
            // Fixnum fast-path arithmetic with an overflow deopt.
            LInst::IntegerBinOp {
                kind,
                lhs,
                rhs,
                deopt,
            } => {
                self.a64_integer_binop(lhs, rhs, kind, &deopt);
            }
            LInst::IntegerBinOpImm {
                kind,
                lhs,
                imm,
                deopt,
            } => {
                self.a64_integer_binop_imm(lhs, imm, kind, &deopt);
            }
            // Fixnum doubling: add the tagged value to itself *before* the
            // retag adjustment (safe for a shared operand register), overflow
            // -> deopt, then `-1` retags. Twin of x86 `integer_double`.
            LInst::IntegerDouble { reg, deopt } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit, adds x(r), x(r), x(r););
                self.jit.bcond_label(monoasm::Cond::Vs, &deopt);
                monoasm_arm64!(&mut self.jit, sub x(r), x(r), #(1u32););
            }
            // Fixnum unary negate (tagged); deopt on i63 overflow.
            LInst::FixnumNeg { reg, deopt } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (2u64);
                    subs x(r), x9, x(r);   // 2 - t  == tagged(-n)
                );
                self.jit.bcond_label(monoasm::Cond::Vs, &deopt);
            }
            // Fixnum bitwise-not (tagged); cannot overflow.
            LInst::FixnumBitNot { reg } => {
                let r = reg.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (0u64);
                    sub x(r), x9, x(r);    // -t  == tagged(~n)
                );
            }
            // ---- FP transfer / convert (spill-aware) -------------------------
            LInst::FprMove { src, dst, base } => {
                let s = self.a64_fpr_read(src, 0, base);
                let d = self.a64_fpr_wtmp(dst, 0, base);
                if s != d {
                    monoasm_arm64!(&mut self.jit, fmov d(d), d(s););
                }
                self.a64_fpr_commit(dst, 0, base);
            }
            LInst::F64ToFpr { f, dst, base } => {
                let p = self.a64_fpr_wtmp(dst, 0, base);
                let bits = f.to_bits();
                monoasm_arm64!(&mut self.jit,
                    mov x9, (bits);
                    fmov d(p), x9;
                );
                self.a64_fpr_commit(dst, 0, base);
            }
            LInst::FixnumToFpr { src, dst, base } => {
                let p = self.a64_fpr_wtmp(dst, 0, base);
                let r = src.a64().0;
                monoasm_arm64!(&mut self.jit,
                    asr x9, x(r), #(1);   // untag: value >> 1
                    scvtf d(p), x9;
                );
                self.a64_fpr_commit(dst, 0, base);
            }
            LInst::FprToStack { src, slot, base } => {
                self.emit_fpr_to_stack(src, slot, base);
            }
            LInst::FprSwap { lhs, rhs, base } => {
                // Force both values into scratch, then store back crossed.
                self.a64_fpr_load(lhs, 0, base);
                self.a64_fpr_load(rhs, 1, base);
                self.a64_fpr_save(lhs, 1, base);
                self.a64_fpr_save(rhs, 0, base);
            }
            LInst::FloatToFpr { src, dst, deopt, base } => {
                let p = self.a64_fpr_wtmp(dst, 0, base);
                let r = src.a64().0;
                let heap = self.jit.label();
                let exit = self.jit.label();
                monoasm_arm64!(&mut self.jit,
                    tbnz x(r), #(0), deopt;   // fixnum -> deopt (expected a Float)
                    tbz x(r), #(1), heap;     // not flonum -> heap Float
                    // flonum: handle 0.0, else decode.
                    fmov d(p), xzr;
                    mov x9, (FLOAT_ZERO);
                    cmp x(r), x9;
                );
                self.jit.bcond_label(monoasm::Cond::Eq, &exit);
                monoasm_arm64!(&mut self.jit,
                    asr x9, x(r), #(63);      // sign: all-1s / all-0s
                    add x9, x9, #(2);         // 2 - signbit  (1 or 2)
                    lsr x10, x(r), #(2);
                    lsl x10, x10, #(2);       // reg & ~3
                    orr x10, x10, x9;
                    ror x10, x10, #(3);       // rotate_right 3
                    fmov d(p), x10;
                    b exit;
                    heap:
                );
                self.a64_guard_rvalue(r, FLOAT_CLASS, &deopt);
                monoasm_arm64!(&mut self.jit,
                    ldr d(p), [x(r), #(RVALUE_OFFSET_KIND as u32)];
                    exit:
                );
                self.a64_fpr_commit(dst, 0, base);
            }
            LInst::I64ToBoth { i, slot, dst, base } => {
                let p = self.a64_fpr_wtmp(dst, 0, base);
                let lfp = GP::R14.a64().0;
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                let id = Value::integer(i).id();
                let bits = (i as f64).to_bits();
                monoasm_arm64!(&mut self.jit, mov x9, (id););
                self.a64_frame_store(9, lfp, off);
                monoasm_arm64!(&mut self.jit,
                    mov x9, (bits);
                    fmov d(p), x9;
                );
                self.a64_fpr_commit(dst, 0, base);
            }
            // ---- FP arithmetic / comparison ----------------------------------
            LInst::FloatBinOp { kind, lhs, rhs, dst, base } => {
                // Only Add/Sub/Mul/Div reach FloatBinOp (Rem/pow are method
                // calls). Operands resolve to a pool reg or load a spill into
                // d0/d1; the result writes its pool reg or scratch d0.
                let ld = self.a64_fpr_read(lhs, 0, base);
                let rd = self.a64_fpr_read(rhs, 1, base);
                let dd = self.a64_fpr_wtmp(dst, 0, base);
                match kind {
                    BinOpK::Add => monoasm_arm64!(&mut self.jit, fadd d(dd), d(ld), d(rd);),
                    BinOpK::Sub => monoasm_arm64!(&mut self.jit, fsub d(dd), d(ld), d(rd);),
                    BinOpK::Mul => monoasm_arm64!(&mut self.jit, fmul d(dd), d(ld), d(rd);),
                    BinOpK::Div => monoasm_arm64!(&mut self.jit, fdiv d(dd), d(ld), d(rd);),
                    _ => unreachable!(),
                }
                self.a64_fpr_commit(dst, 0, base);
            }
            LInst::FloatUnOp { kind, dst, base } => match kind {
                UnOpK::Neg => {
                    let p = self.a64_fpr_read(dst, 0, base);
                    monoasm_arm64!(&mut self.jit,
                        fmov x9, d(p);
                        mov x10, (0x8000_0000_0000_0000u64);
                        eor x9, x9, x10;
                        fmov d(p), x9;
                    );
                    self.a64_fpr_commit(dst, 0, base);
                }
                UnOpK::Pos => {}
                _ => unreachable!(),
            },
            LInst::FloatCmp { kind, lhs, rhs, base } => {
                let lp = self.a64_fpr_read(lhs, 0, base);
                let rp = self.a64_fpr_read(rhs, 1, base);
                monoasm_arm64!(&mut self.jit, fcmp d(lp), d(rp););
                let cond = a64_float_cond_for_cmp(kind, BrKind::BrIf);
                let rax = GP::Rax.a64();
                self.jit.cset(rax, cond);
                monoasm_arm64!(&mut self.jit,
                    lsl x(rax.0), x(rax.0), #(3u32);
                    mov x9, (FALSE_VALUE);
                    orr x(rax.0), x(rax.0), x9;
                );
            }
            LInst::FloatCmpBr {
                kind,
                lhs,
                rhs,
                brkind,
                dest,
                base,
            } => {
                let lp = self.a64_fpr_read(lhs, 0, base);
                let rp = self.a64_fpr_read(rhs, 1, base);
                monoasm_arm64!(&mut self.jit, fcmp d(lp), d(rp););
                let cond = a64_float_cond_for_cmp(kind, brkind);
                self.jit.bcond_label(cond, &dest);
            }
            // ---- FP pool save/restore + FP C-calls ---------------------------
            LInst::FprSave { using_fpr, cont } => {
                self.emit_fpr_save(using_fpr, cont);
            }
            LInst::FprRestore { using_fpr, cont } => {
                self.emit_fpr_restore(using_fpr, cont);
            }
            LInst::CFunc_F_F { f, src, dst, using_fpr, base } => {
                let fp = f as u64;
                monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
                self.emit_fpr_save(using_fpr, false);
                self.a64_fpr_load(src, 0, base); // arg -> d0
                monoasm_arm64!(&mut self.jit,
                    mov x9, (fp);
                    blr x9;            // result in d0
                );
                self.emit_fpr_restore(using_fpr, false);
                monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
                self.a64_fpr_save(dst, 0, base); // result d0 -> dst
            }
            LInst::CFunc_FF_F { f, lhs, rhs, dst, using_fpr, base } => {
                let fp = f as u64;
                monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
                self.emit_fpr_save(using_fpr, false);
                self.a64_fpr_load(lhs, 0, base); // arg0 -> d0
                self.a64_fpr_load(rhs, 1, base); // arg1 -> d1
                monoasm_arm64!(&mut self.jit,
                    mov x9, (fp);
                    blr x9;            // result in d0
                );
                self.emit_fpr_restore(using_fpr, false);
                monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
                self.a64_fpr_save(dst, 0, base); // result d0 -> dst
            }
            // Speculated-unboxed outer local (doc/chain_deopt.md §5 step 5):
            // one f64 move against the speculating frame's FP save/spill
            // area at `[x29 + offset + disp]`. The frame-chain offset can
            // exceed the ldr/str immediate range, so materialize it through
            // the x10 scratch like `load_dyn_var_specialized`; d0 is the f64
            // scratch (dead outside FP calls, like the CFunc lowerings).
            LInst::LoadDynVarSpecF { offset, disp, dst, base } => {
                let e = offset as i64 + disp as i64;
                monoasm_arm64!(&mut self.jit,
                    mov x10, (e as u64);
                    add x10, x29, x10;
                    ldr d0, [x10];
                );
                self.a64_fpr_save(dst, 0, base);
            }
            LInst::StoreDynVarSpecF { offset, disp, src, base } => {
                let e = offset as i64 + disp as i64;
                self.a64_fpr_load(src, 0, base);
                monoasm_arm64!(&mut self.jit,
                    mov x10, (e as u64);
                    add x10, x29, x10;
                    str d0, [x10];
                );
            }
            // Cold side-exit handler blocks. Deopt / Evict re-enter the VM.
            LInst::SideExit {
                kind,
                pc,
                wb,
                entry,
                loop_jit_spill_bytes,
                base,
                // aarch64 handlers do not call `log_deoptimize` (they never
                // have), so the exit id is recorded but unused here.
                #[cfg(feature = "deopt")]
                    exit_id: _,
            } => match kind {
                LSideExitKind::Deopt { chain } => {
                    self.a64_gen_deopt(pc, &wb, entry, loop_jit_spill_bytes, base, chain)
                }
                LSideExitKind::Evict => {
                    self.a64_gen_deopt(pc, &wb, entry, loop_jit_spill_bytes, base, false)
                }
                // A monomorphically-compiled site (e.g. a `BinCmp`) whose
                // receiver-class guard missed because it went polymorphic.
                // Route the miss through the same counter-gated recompiler the
                // main-body `AsmInst::RecompileDeopt` / `GuardClassVersionSpecialized`
                // use, so the site re-JITs to the guard-free (polymorphic) path
                // instead of deopting to the VM on every off-class receiver
                // forever. `emit_recompile_deopt` runs the recompile once (or
                // falls straight through while the counter is unexhausted),
                // branching to `deopt_body` to resume in the interpreter, or to
                // `error_body` if the recompile itself raised a FatalError.
                LSideExitKind::RecompileDeopt {
                    reason,
                    target,
                    chain,
                } => {
                    let deopt_body = self.jit.label();
                    let error_body = self.jit.label();
                    self.jit.bind_label(entry);
                    self.emit_recompile_deopt(target, &deopt_body, Some(&error_body), reason);
                    self.a64_gen_deopt(pc, &wb, deopt_body, loop_jit_spill_bytes, base, chain);
                    self.a64_gen_handle_error(pc, &wb, error_body, loop_jit_spill_bytes, base, chain);
                }
                LSideExitKind::Error { chain } => {
                    self.a64_gen_handle_error(pc, &wb, entry, loop_jit_spill_bytes, base, chain)
                }
            },
            // Macro-ops (irreducible runtime-call shapes) are delegated to the
            // arch-neutral fallback, which dispatches to the per-arch `emit_*`.
            other => self.encode_linst_macro(other),
        }
    }

    /// Emit `addr <- base - off` (a byte offset into a frame).
    ///
    /// `sub`'s 12-bit immediate caps a direct subtract at `off <= 4095`; above
    /// that the offset is materialized into `addr` first and a register subtract
    /// is used, so *any* frame offset is addressable (no bail). `addr` doubles
    /// as the materialization scratch, so it must differ from `base`.
    fn a64_addr_sub(&mut self, addr: u32, base: u32, off: u32) {
        debug_assert_ne!(addr, base, "a64_addr_sub: addr must differ from base");
        if off <= 4095 {
            monoasm_arm64!(&mut self.jit, sub x(addr), x(base), #(off););
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x(addr), (off as u64);
                sub x(addr), x(base), x(addr);
            );
        }
    }

    /// `x(dst) <- [lfp - off]`: load the value of a frame slot. Slots sit at a
    /// *negative* displacement from the LFP, which the scaled `ldr` immediate
    /// cannot encode; `ldur`'s signed 9-bit offset can, so a small frame folds
    /// to one instruction. A larger offset forms the address in scratch x10
    /// (so `dst` must not be x10).
    fn a64_frame_load(&mut self, dst: u32, lfp: u32, off: u32) {
        if off <= 256 {
            let neg = -(off as i32);
            monoasm_arm64!(&mut self.jit, ldur x(dst), [x(lfp), #(neg)];);
        } else {
            self.a64_addr_sub(10, lfp, off);
            monoasm_arm64!(&mut self.jit, ldr x(dst), [x10];);
        }
    }

    /// `[lfp - off] <- x(src)`: store into a frame slot. `stur`'s signed 9-bit
    /// offset folds a small (negative) frame displacement to one instruction;
    /// a larger offset forms the address in scratch x10 (so `src` must not be
    /// x10).
    fn a64_frame_store(&mut self, src: u32, lfp: u32, off: u32) {
        if off <= 256 {
            let neg = -(off as i32);
            monoasm_arm64!(&mut self.jit, stur x(src), [x(lfp), #(neg)];);
        } else {
            self.a64_addr_sub(10, lfp, off);
            monoasm_arm64!(&mut self.jit, str x(src), [x10];);
        }
    }

    /// `x(dst) <- [x(base) + off]`: load an object field. The scaled `ldr`
    /// immediate covers `off <= 32760` (8-aligned); a larger/unaligned offset
    /// goes through a register-offset load (byte offset materialized in scratch
    /// x10, so `dst`/`base` must not be x10).
    fn a64_field_load(&mut self, dst: u32, base: u32, off: u32) {
        if Self::a64_field_off_ok(off) {
            monoasm_arm64!(&mut self.jit, ldr x(dst), [x(base), #(off)];);
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x10, (off as u64);
                ldr x(dst), [x(base), x10];
            );
        }
    }

    /// `[x(base) + off] <- x(src)`: store an object field. Large-offset safe
    /// like `a64_field_load` (`src`/`base` must not be x10).
    fn a64_field_store(&mut self, src: u32, base: u32, off: u32) {
        if Self::a64_field_off_ok(off) {
            monoasm_arm64!(&mut self.jit, str x(src), [x(base), #(off)];);
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x10, (off as u64);
                str x(src), [x(base), x10];
            );
        }
    }

    /// `sp <- sp - off`, materializing offsets beyond the 12-bit `sub sp`
    /// immediate. The register form addresses sp via a GP temp (x9/x10) because
    /// reg 31 decodes as XZR — not SP — in the shifted-register add/sub.
    fn a64_sp_sub(&mut self, off: u32) {
        if off == 0 {
            return;
        }
        if off <= 4095 {
            monoasm_arm64!(&mut self.jit, sub sp, sp, #(off););
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x9, (off as u64);
                mov x10, sp;
                sub x10, x10, x9;
                mov sp, x10;
            );
        }
    }

    /// `sp <- sp + off`, the inverse of `a64_sp_sub` (same sp-via-temp caveat).
    fn a64_sp_add(&mut self, off: u32) {
        if off == 0 {
            return;
        }
        if off <= 4095 {
            monoasm_arm64!(&mut self.jit, add sp, sp, #(off););
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x9, (off as u64);
                mov x10, sp;
                add x10, x10, x9;
                mov sp, x10;
            );
        }
    }

    /// Unconditional jump to a side-exit (deopt) label.
    pub(in crate::codegen::jitgen) fn emit_deopt(&mut self, deopt: &DestLabel) {
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit, b deopt;);
    }

    /// Error check: a runtime helper returns rax==0 (None) on error; branch to
    /// the error side-exit handler in that case. Mirrors x86 `handle_error`
    /// (`testq rax,rax; jeq error`).
    pub(in crate::codegen::jitgen) fn emit_handle_error(&mut self, error: &DestLabel) {
        let error = error.clone();
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit, cbz x(rax), error;);
    }

    /// Stack-overflow check: if sp <= limit, write back live values, call
    /// stack_overflow(vm), and jump to the error handler. The overflow path is
    /// laid out inline but skipped on the common path. Bails (`false`) if the
    /// write-back needs an unsupported feature. `base` is unused on aarch64.
    /// Mirrors x86 `jit_check_stack`.
    pub(in crate::codegen::jitgen) fn emit_check_stack(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        base: usize,
    ) -> bool {
        let error = error.clone();
        let ok = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            ldr x11, [x19, #(crate::executor::EXECUTOR_STACK_LIMIT as u32)];
            cmp x10, x11;
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &ok); // sp > limit -> ok
        self.a64_gen_write_back_for_deopt(&write_back, base);
        let f = crate::codegen::stack_overflow as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            b error;
        );
        self.jit.bind_label(ok);
        true
    }

    /// Safepoint poll: if any lane of the poll word is set (GC request,
    /// preempt tick, pending signal — see poll_flag.rs), write back live
    /// values, run execute_gc(vm, globals), and on error jump to the error
    /// handler. The slow path is laid out inline but skipped on the common
    /// path. Bails (`false`) like `emit_check_stack`. `base` is unused on
    /// aarch64. Mirrors x86 `jit_execute_gc`.
    pub(in crate::codegen::jitgen) fn emit_exec_gc(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        base: usize,
    ) -> bool {
        let error = error.clone();
        let skip = self.jit.label();
        let pf_addr = self
            .jit
            .get_label_address(&self.poll_flag.clone())
            .as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (pf_addr);
            ldr w9, [x9];         // zero-extends into x9
            cbz x9, skip;         // all lanes clear -> no poll
        );
        self.a64_gen_write_back_for_deopt(&write_back, base);
        let f = crate::executor::execute_gc as *const () as u64;
        // Preserve the caller-saved halves of BOTH allocation pools across the
        // call: the GP pool (x5-x8 = GP::R8-R11) and the FP pool (d2-d7).
        // Both are AAPCS64 caller-saved and hold live pool values, and the
        // write-back above only spills them to their frame homes for GC
        // marking — the post-GC code keeps reading the *registers* (e.g. a
        // pool-resident call receiver, or an unboxed float operand), so they
        // must survive `execute_gc`. d8-d15 / x19-x28 are callee-saved, so the
        // Rust callee preserves those itself.
        //
        // x86 preserves the equivalent registers in its `exec_gc` stub:
        // `save_registers`/`restore_registers` cover r8-r11 *and* xmm2-xmm15.
        // Omitting the d2-d7 half here let `execute_gc` clobber a live unboxed
        // float, so e.g. `10.upto(Float::INFINITY)`'s enumerator saw a garbage
        // limit and terminated immediately (#1079) — the same failure the
        // recompile path below already guards against with the identical save.
        //
        // The GC is mark-and-sweep (non-moving), so a heap value the write-back
        // kept alive is not relocated and restoring the raw register value is
        // correct (Fixnum immediates are trivially fine for the same reason).
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(80);
            str d2, [sp, #(0)];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            stp x5, x6, [sp, #(48)];
            stp x7, x8, [sp, #(64)];
        );
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        monoasm_arm64!(&mut self.jit,
            ldr d2, [sp, #(0)];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            ldp x5, x6, [sp, #(48)];
            ldp x7, x8, [sp, #(64)];
            add sp, sp, #(80);
        );
        monoasm_arm64!(&mut self.jit,
            cbz x0, error;             // None -> error
        );
        self.jit.bind_label(skip);
        true
    }

    // ---- runtime allocation primitives (aarch64) --------------------------
    // Each builds a heap object via a runtime C call. All bail (`false`) on a
    // live xmm (no FP save/restore yet) or an out-of-range frame offset.

    /// rax <- Array built from the `len` slots starting at `src`.
    /// create_array(ptr=&slot[src], len). No xmm save (matches x86).
    pub(in crate::codegen::jitgen) fn emit_create_array(&mut self, src: SlotId, len: usize) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::create_array as *const () as u64;
        self.a64_addr_sub(0, lfp, off); // x0 = &slot[src]
        monoasm_arm64!(&mut self.jit,
            mov x1, (len as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                   // result in x0
            ldr x30, [sp], #16;
        );
        true
    }

    /// rax <- Array literal via gen_array(vm, globals, callid, &self).
    pub(in crate::codegen::jitgen) fn emit_new_array(
        &mut self,
        callid: CallSiteId,
        inline: Option<(SlotId, u16)>,
        using_fpr: UsingFpr,
    ) -> bool {
        match inline {
            // Inline the common case (small no-splat literal) when the
            // allocator free-list addresses were captured at startup.
            Some((args, len)) if !self.alloc_free_head_addr.is_null() => {
                self.new_array_inline(callid, args, len, using_fpr);
            }
            _ => self.new_array_runtime(callid, using_fpr),
        }
        true
    }

    /// rax(x0) <- Array literal (splat-aware) via the runtime call site.
    fn new_array_runtime(&mut self, callid: CallSiteId, using_fpr: UsingFpr) {
        let lfp = GP::R14.a64().0;
        let f = runtime::gen_array as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, x(lfp), #(LFP_SELF as u32); // &[lfp - LFP_SELF]
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
    }

    /// Inline GC-cell allocation, shared by every inline constructor (array
    /// literals, plain objects). Mirrors `Allocator::alloc`'s two fast
    /// paths — pop a recycled cell from the free list, else bump-allocate
    /// out of the current page — and writes the 8-byte object header,
    /// leaving `var_table` and the type-specific body to the caller.
    ///
    /// Jumps to `slow` only where the runtime does real work: at
    /// `BUMP_INLINE_LIMIT` the allocator sets the GC alloc flag and starts a
    /// new page. The caller must emit a runtime fallback there.
    ///
    /// Callers must first check that `alloc_free_head_addr` is non-null (the
    /// allocator addresses are captured once at `Codegen::new`).
    ///
    /// #### out
    /// - x0 (Rax): the fresh cell, header already stored
    ///
    /// #### destroy
    /// - x9, x11, x12
    pub(crate) fn emit_alloc_cell(&mut self, header: CellHeader, slow: &DestLabel) {
        let rax = GP::Rax.a64().0; // x0 (result)
        // The cell acquisition itself (free-list pop / bump) lives in the
        // shared `alloc_cell` stub; only the null test and the per-site
        // header write are laid inline. The site's live x30 is preserved
        // around the `bl` (same convention as the write-barrier stub).
        let alloc_cell = self.alloc_cell.clone();
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            bl alloc_cell;
            ldr x30, [sp], #16;
        );
        // alloc-flag / new-page territory: hand back to the runtime.
        let slow = slow.clone();
        monoasm_arm64!(&mut self.jit,
            cbz x(rax), slow;
        );
        match header {
            CellHeader::Imm(h) => monoasm_arm64!(&mut self.jit,
                mov x11, (h);
            ),
            CellHeader::NewbornOf(src) => {
                // Keep class/type (the upper 48 bits) and the non-GC flags.
                let mask: u64 = !0xffffu64 | NEWBORN_FLAG_MASK as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x11, (src);
                    ldr x11, [x11];
                    mov x12, (mask);
                    and x11, x11, x12;
                );
            }
        }
        monoasm_arm64!(&mut self.jit,
            str x11, [x(rax)];         // header (offset 0); overwrites the free link
        );
    }

    /// Bind the shared `alloc_cell` stub (see the field doc in
    /// `codegen.rs`): `Allocator::alloc`'s two fast paths — free-list pop,
    /// else bump — returning the fresh cell in x0, or 0 when the runtime
    /// must take over. No inner call, so the `bl`-written x30 carries the
    /// return address to `ret`. Emitted at the end of `Codegen::new`, after
    /// the allocator addresses are captured. aarch64 twin of the x86_64
    /// stub.
    pub(in crate::codegen) fn gen_alloc_cell_stub(&mut self) {
        let rax = GP::Rax.a64().0; // x0 (result)
        let label = self.alloc_cell.clone();
        let free_head = self.alloc_free_head_addr as u64;
        let free_count = self.alloc_free_count_addr as u64;
        let total = self.alloc_total_addr as u64;
        let used = self.alloc_used_addr as u64;
        let page = self.alloc_page_addr as u64;
        let bump = self.jit.label();
        let fail = self.jit.label();
        let done = self.jit.label();
        self.jit.bind_label(label);
        monoasm_arm64!(&mut self.jit,
            mov x9, (free_head);
            ldr x(rax), [x9];          // rax = free-list head (cell ptr, or 0 = None)
            cbz x(rax), bump;
            ldr x12, [x(rax)];         // x12 = (*cell).header.next (free link @ offset 0)
            str x12, [x9];             // free = next
            mov x9, (free_count);
            ldr x12, [x9];
            sub x12, x12, #1;
            str x12, [x9];
            b done;
        );
        self.jit.bind_label(bump.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, (used);
            ldr x12, [x9];             // x12 = used_in_current
            mov x11, (BUMP_INLINE_LIMIT as u64);
            cmp x12, x11;
        );
        // alloc-flag / new-page territory: hand back to the runtime.
        self.jit.bcond_label(monoasm::Cond::Hs, &fail);
        monoasm_arm64!(&mut self.jit,
            mov x11, (page);
            ldr x(rax), [x11];         // rax = current page
            lsl x11, x12, #(CELL_SIZE_SHIFT);
            add x(rax), x(rax), x11;   // + used_in_current * CELL_SIZE
        );
        if PAGE_DATA_OFFSET != 0 {
            monoasm_arm64!(&mut self.jit,
                mov x11, (PAGE_DATA_OFFSET as u64);
                add x(rax), x(rax), x11;
            );
        }
        monoasm_arm64!(&mut self.jit,
            add x12, x12, #1;
            str x12, [x9];             // used_in_current += 1
        );
        self.jit.bind_label(done.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, (total);
            ldr x12, [x9];
            add x12, x12, #1;
            str x12, [x9];
            ret;
        );
        self.jit.bind_label(fail);
        monoasm_arm64!(&mut self.jit,
            mov x(rax), (0);
            ret;
        );
    }

    /// Inline allocation of a small (`0..=ARRAY_INLINE_CAPA`) no-splat array
    /// literal: pop a recycled cell from the GC free list and initialise it
    /// directly as an inline-storage Array, falling back to the runtime
    /// `gen_array` when the free list is empty. See the x86_64
    /// `new_array_inline` for the correctness argument (young object needs no
    /// write barrier; no GC safepoint mid-build).
    fn new_array_inline(
        &mut self,
        callid: CallSiteId,
        args: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) {
        let rax = GP::Rax.a64().0; // x0 (result)
        let lfp = GP::R14.a64().0; // x22
        let slow = self.jit.label();
        let cont = self.jit.label();
        // 8-byte object header: flag=1 | ty=ARRAY<<16 | class=ARRAY_CLASS<<32.
        let header: u64 =
            ((ARRAY_CLASS.u32() as u64) << 32) | ((ObjTy::ARRAY.get() as u64) << 16) | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm_arm64!(&mut self.jit,
            mov x12, #0;
            str x12, [x(rax), #(RVALUE_OFFSET_VAR as u32)]; // var_table = None
            mov x12, (len as u64);
            str x12, [x(rax), #(RVALUE_OFFSET_ARY_CAPA as u32)]; // inline length
        );
        for k in 0..len {
            let slot = SlotId(args.0 + k);
            let off = RVALUE_OFFSET_INLINE as u32 + (k as u32) * 8;
            self.a64_frame_load(12, lfp, conv(slot) as u32);
            self.a64_field_store(12, rax, off);
        }
        monoasm_arm64!(&mut self.jit, b cont;);
        self.jit.bind_label(slow);
        self.new_array_runtime(callid, using_fpr);
        self.jit.bind_label(cont);
    }

    /// rax <- Hash literal via gen_hash(vm, globals, &slot[args], len).
    /// x0 <- min/max of the `len` values at `args`, computed in place —
    /// the fused, allocation-free `[a, b, …].min` / `.max`. Same call
    /// shape as `emit_new_hash`.
    pub(in crate::codegen::jitgen) fn emit_array_min_max(
        &mut self,
        args: SlotId,
        len: u16,
        min: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = args.0 as u32 * 8 + LFP_SELF as u32;
        let f = if min {
            runtime::opt_array_min as *const () as u64
        } else {
            runtime::opt_array_max as *const () as u64
        };
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;              // vm
            mov x1, x20;              // globals
        );
        self.a64_addr_sub(2, lfp, off); // x2 = &slot[args]
        monoasm_arm64!(&mut self.jit,
            mov x3, (len as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// x0 <- Hash literal from the `len` key/value slots at `args`.
    pub(in crate::codegen::jitgen) fn emit_new_hash(
        &mut self,
        args: SlotId,
        len: usize,
        using_fpr: UsingFpr,
    ) -> bool {
        if len <= HASH_INLINE_CAP && !self.alloc_free_head_addr.is_null() {
            self.new_hash_inline(args, len, using_fpr);
        } else {
            self.new_hash_runtime(args, len, using_fpr);
        }
        true
    }

    /// x0 <- Hash literal via gen_hash(vm, globals, &slot[args], len).
    fn new_hash_runtime(&mut self, args: SlotId, len: usize, using_fpr: UsingFpr) {
        let lfp = GP::R14.a64().0;
        let off = args.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::gen_hash as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;              // vm
            mov x1, x20;              // globals
        );
        self.a64_addr_sub(2, lfp, off); // x2 = &slot[args]
        monoasm_arm64!(&mut self.jit,
            mov x3, (len as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
    }

    /// Inline allocation of a small (`0..=HASH_INLINE_CAP` pairs) Hash
    /// literal: pop a recycled cell from the GC free list and initialise it
    /// directly as an inline-representation Hash, with no runtime call.
    /// Fast-path conditions and the correctness argument (packed distinct
    /// keys — eql? is bit equality, no observable `#hash`; nil-nil
    /// placeholder pairs; young object needs no write barrier; no GC
    /// safepoint mid-build) are documented on the x86_64
    /// `new_hash_inline`; anything else falls back to `gen_hash`.
    fn new_hash_inline(&mut self, args: SlotId, len: usize, using_fpr: UsingFpr) {
        let rax = GP::Rax.a64().0; // x0 (result)
        let lfp = GP::R14.a64().0; // x22
        let slow = self.jit.label();
        let cont = self.jit.label();
        if len > 0 {
            // x9 = packed-value mask (`Value::is_packed_value`: heap iff
            // (bits & 0b111) == 0; monoasm's `and` is register-only).
            monoasm_arm64!(&mut self.jit,
                mov x9, #7;
            );
            for i in 0..len {
                let key = SlotId(args.0 + 2 * i as u16);
                self.a64_frame_load(12, lfp, conv(key) as u32);
                monoasm_arm64!(&mut self.jit,
                    and x11, x12, x9;
                    cbz x11, slow;
                );
            }
            for j in 1..len {
                for i in 0..j {
                    let ki = SlotId(args.0 + 2 * i as u16);
                    let kj = SlotId(args.0 + 2 * j as u16);
                    self.a64_frame_load(11, lfp, conv(ki) as u32);
                    self.a64_frame_load(12, lfp, conv(kj) as u32);
                    monoasm_arm64!(&mut self.jit,
                        cmp x11, x12;
                    );
                    self.jit.bcond_label(monoasm::Cond::Eq, &slow);
                }
            }
        }
        // 8-byte object header: flag=1 | ty=HASH<<16 | rep(len)<<24 |
        // class=HASH_CLASS<<32 (the ty_flags byte holds the inline
        // representation bits: pair count, eql?-keyed).
        let header: u64 = ((HASH_CLASS.u32() as u64) << 32)
            | ((len as u64) << 24)
            | ((ObjTy::HASH.get() as u64) << 16)
            | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm_arm64!(&mut self.jit,
            mov x12, #0;
            str x12, [x(rax), #(RVALUE_OFFSET_VAR as u32)]; // var_table = None
        );
        for i in 0..HASH_INLINE_CAP {
            let pair = HASH_INLINE_PAIRS_OFFSET + i * HASH_INLINE_PAIR_STRIDE;
            let key_off = (pair + HASH_INLINE_KEY_OFFSET) as u32;
            let val_off = (pair + HASH_INLINE_VALUE_OFFSET) as u32;
            if i < len {
                let key = SlotId(args.0 + 2 * i as u16);
                let val = SlotId(args.0 + 2 * i as u16 + 1);
                self.a64_frame_load(12, lfp, conv(key) as u32);
                self.a64_field_store(12, rax, key_off);
                self.a64_frame_load(12, lfp, conv(val) as u32);
                self.a64_field_store(12, rax, val_off);
            } else {
                monoasm_arm64!(&mut self.jit,
                    mov x12, (NIL_VALUE as u64);
                );
                self.a64_field_store(12, rax, key_off);
                self.a64_field_store(12, rax, val_off);
            }
        }
        monoasm_arm64!(&mut self.jit, b cont;);
        self.jit.bind_label(slow);
        self.new_hash_runtime(args, len, using_fpr);
        self.jit.bind_label(cont);
    }

    /// rax <- the Hash in `hash` after inserting the `len` key/value pairs
    /// at `args`, via hash_insert(vm, globals, &slot[args], len, hash)
    /// (chunked Hash literal).
    pub(in crate::codegen::jitgen) fn emit_hash_insert(
        &mut self,
        hash: SlotId,
        args: SlotId,
        len: usize,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let args_off = args.0 as u32 * 8 + LFP_SELF as u32;
        let hash_off = hash.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::hash_insert as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;              // vm
            mov x1, x20;              // globals
        );
        self.a64_addr_sub(2, lfp, args_off); // x2 = &slot[args]
        self.a64_frame_load(4, lfp, hash_off); // x4 = hash value
        monoasm_arm64!(&mut self.jit,
            mov x3, (len as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- the Array in `dst` after concatenating the Array in `src`,
    /// via array_concat(vm, globals, dst, src) (chunked Array literal).
    pub(in crate::codegen::jitgen) fn emit_array_concat(
        &mut self,
        dst: SlotId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let dst_off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let src_off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::array_concat as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;              // vm
            mov x1, x20;              // globals
        );
        self.a64_frame_load(2, lfp, dst_off); // x2 = dst value
        self.a64_frame_load(3, lfp, src_off); // x3 = src value
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- Range via gen_range(start, end, vm, globals, exclude_end).
    pub(in crate::codegen::jitgen) fn emit_new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let soff = start.0 as u32 * 8 + LFP_SELF as u32;
        let eoff = end.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::gen_range as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        self.a64_frame_load(0, lfp, soff); // x0 = start value
        self.a64_frame_load(1, lfp, eoff); // x1 = end value
        monoasm_arm64!(&mut self.jit,
            mov x2, x19;              // vm
            mov x3, x20;              // globals
            mov x4, (exclude_end as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- the `len` slots at `arg` concatenated into a String via
    /// concatenate_string(vm, globals, &slot[arg], len). Result is Option<Value>
    /// (followed by a HandleError in the IR).
    pub(in crate::codegen::jitgen) fn emit_concat_str(
        &mut self,
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = arg.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::concatenate_string as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;              // vm
            mov x1, x20;              // globals
        );
        self.a64_addr_sub(2, lfp, off); // x2 = &slot[arg]
        monoasm_arm64!(&mut self.jit,
            mov x3, (len as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- `src` coerced to an Array: load slot `src`; if already an Array
    /// keep it, otherwise call runtime::to_a(vm, globals, val). Mirrors x86 to_a.
    pub(in crate::codegen::jitgen) fn emit_to_a(&mut self, src: SlotId, using_fpr: UsingFpr) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let toa = self.jit.label();
        let exit = self.jit.label();
        // Reserve the FP-pool save area for the whole sequence; both the
        // already-Array fast path and the to_a C call fall through to the
        // matching restore, so sp stays balanced either way.
        self.emit_fpr_save(using_fpr, false);
        self.a64_frame_load(0, lfp, off); // val (rax)
        self.a64_guard_rvalue(GP::Rax.a64().0, ARRAY_CLASS, &toa); // not Array -> toa
        monoasm_arm64!(&mut self.jit, b exit;); // already Array
        let f = runtime::to_a as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            toa:
            mov x2, x0;             // val
            mov x0, x19;            // vm
            mov x1, x20;            // globals
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                 // result in x0
            ldr x30, [sp], #16;
            exit:
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- a deep copy of literal `v` (a fresh mutable object per
    /// evaluation). Mirrors x86 `deepcopy_literal`.
    ///
    /// A short all-immediate Array literal (`[1, 2]` and friends, which
    /// `bytecodegen` bakes as a constant rather than compiling to
    /// `NewArray`) is copied inline: its deep copy is just a header, the
    /// inline length, and the element words. Everything else calls
    /// `value_deep_copy`.
    pub(in crate::codegen::jitgen) fn emit_deep_copy_lit(
        &mut self,
        v: Value,
        using_fpr: UsingFpr,
    ) -> bool {
        if let Some(elems) = v
            .inline_copyable_array()
            .filter(|_| !self.alloc_free_head_addr.is_null())
        {
            let rax = GP::Rax.a64().0; // x0 (result)
            let slow = self.jit.label();
            let cont = self.jit.label();
            self.emit_alloc_cell(CellHeader::NewbornOf(v.id()), &slow);
            monoasm_arm64!(&mut self.jit,
                mov x12, #0;
                str x12, [x(rax), #(RVALUE_OFFSET_VAR as u32)]; // var_table = None
                mov x12, (elems.len() as u64);
                str x12, [x(rax), #(RVALUE_OFFSET_ARY_CAPA as u32)]; // inline length
            );
            for (k, e) in elems.iter().enumerate() {
                let off = RVALUE_OFFSET_INLINE as u32 + (k as u32) * 8;
                monoasm_arm64!(&mut self.jit,
                    mov x12, (e.id());
                    str x12, [x(rax), #(off)];
                );
            }
            monoasm_arm64!(&mut self.jit, b cont;);
            self.jit.bind_label(slow);
            self.deep_copy_lit_call(v, using_fpr);
            self.jit.bind_label(cont);
            return true;
        }
        self.deep_copy_lit_call(v, using_fpr);
        true
    }

    fn deep_copy_lit_call(&mut self, v: Value, using_fpr: UsingFpr) {
        let imm = v.id();
        let f = Value::value_deep_copy as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, (imm);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;            // result in x0 (= rax)
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
    }

    // ---- floating-point transfer primitives (aarch64) ---------------------
    // Operands resolve through the spill-aware accessors above (`a64_fpr_read`/
    // `_wtmp`/`_load`/`_save`/`_commit`). All return bool; `base` is the spill
    // base.

    /// Save the live FP pool registers (D2..) below sp before a C-call.
    /// With `cont`, the 16-byte continuation frame is reserved at the
    /// bottom (`[sp, sp+16)`) and the fp saves are placed *above* it,
    /// so the call-site pc store (`ContFramePc`, `[sp]`) and the
    /// callee never touch a live saved float.
    pub(in crate::codegen::jitgen) fn emit_fpr_save(&mut self, using_fpr: UsingFpr, cont: bool) -> bool {
        if using_fpr.not_any() && !cont {
            return true;
        }
        let pad = if cont { CONTINUATION_FRAME_SIZE } else { 0 } as u32;
        let sp_offset = using_fpr.offset() as u32 + pad;
        monoasm_arm64!(&mut self.jit, sub sp, sp, #(sp_offset););
        let mut i = 0u32;
        for (xi, b) in using_fpr.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = pad + 8 * i;
                monoasm_arm64!(&mut self.jit, str d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
        true
    }

    /// Restore the live FP pool registers and pop the save area after a C-call.
    pub(in crate::codegen::jitgen) fn emit_fpr_restore(&mut self, using_fpr: UsingFpr, cont: bool) -> bool {
        if using_fpr.not_any() && !cont {
            return true;
        }
        let pad = if cont { CONTINUATION_FRAME_SIZE } else { 0 } as u32;
        let sp_offset = using_fpr.offset() as u32 + pad;
        let mut i = 0u32;
        for (xi, b) in using_fpr.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = pad + 8 * i;
                monoasm_arm64!(&mut self.jit, ldr d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
        monoasm_arm64!(&mut self.jit, add sp, sp, #(sp_offset););
        true
    }

    /// Reload the live FP pool registers from the save area *without* popping
    /// it. Used inside a multi-C-call emit (class_def) whose save area must
    /// persist across several clobbering calls: the pool regs are reloaded
    /// before each intermediate side-exit branch (whose handler reads the pool
    /// *registers*), while the save area itself is popped only once at the end
    /// via `emit_fpr_restore`. No-op when the pool is empty.
    fn a64_fpr_reload(&mut self, using_fpr: UsingFpr) {
        let mut i = 0u32;
        for (xi, b) in using_fpr.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = 8 * i;
                monoasm_arm64!(&mut self.jit, ldr d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
    }

    /// Method epilogue: the result is already in x0; tear down the frame and
    /// return (matches the VM's `a64_op_ret`: `mov sp,x29; ldp; ret`).
    pub(in crate::codegen::jitgen) fn emit_ret(&mut self) {
        monoasm_arm64!(&mut self.jit,
            mov sp, x29;
            ldp x29, x30, [sp], #16;
            ret;
        );
    }

    /// Return through the method-return path, resuming the caller at `pc + 1`.
    pub(in crate::codegen::jitgen) fn emit_method_ret(&mut self, pc: BytecodePtr) {
        self.a64_method_ret(pc);
    }

    /// Non-local exit through the block-break path (a `break` out of a block).
    pub(in crate::codegen::jitgen) fn emit_block_break(&mut self, pc: BytecodePtr) {
        self.a64_block_break(pc);
    }

    /// Store the call-site pc into the outgoing cont-frame slot
    /// (`[sp]` = the callee frame's CFP+24). The 16-byte region was
    /// reserved by the preceding cont-mode `FprSave`, whose fp saves
    /// sit above it — so this is a plain store, no sp adjustment.
    /// See `AsmInst::ContFramePc`.
    pub(in crate::codegen) fn emit_cont_frame_pc(&mut self, call_site_pc: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x10, (call_site_pc);
            str x10, [sp];
        );
    }

    /// Inline-cache class-version guard: deopt if the global class version moved
    /// since compilation (compared against the unit's patchable snapshot word,
    /// so a salvage can heal the unit in place). x86's `with_recovery`
    /// (resume-in-place) is still not ported — a salvaged miss deopts once.
    pub(in crate::codegen::jitgen) fn emit_guard_class_version(
        &mut self,
        class_version: DestLabel,
        position: Option<BytecodePtr>,
        _with_recovery: bool,
        deopt: DestLabel,
    ) {
        // On a class-version miss, recompile (loop → `jit_recompile_loop`,
        // method → `jit_recompile_method`) then resume via the deopt side exit,
        // instead of deopting to the VM forever: `insert_method` bumps the
        // global class version on any `def`, so without this a hot JIT'd method
        // is stranded in the interpreter for the rest of the process after any
        // unrelated method definition. Mirrors x86 `guard_class_version` and the
        // aarch64 `GuardClassVersionSpecialized` twin; no counter — the
        // recompile bakes in the new version, so the guard won't re-fire. The
        // cheap x86 `with_recovery` path (an in-place inline-cache re-bake that
        // resumes in JIT without recompiling) is not ported —
        // `jit_recompile_method_with_recovery` is x86-only — so `_with_recovery`
        // is ignored and this always full-recompiles. On a recompile panic the
        // helper leaves a FatalError set and x0 == 0; we still branch to the
        // deopt, which resumes at the guarded call where the interpreter
        // propagates the fatal (matching the specialized twin).
        let miss = self.jit.label();
        let done = self.jit.label();
        self.a64_guard_class_version(&class_version, &miss); // version mismatch -> miss
        monoasm_arm64!(&mut self.jit, b done;); // match -> continue in JIT
        self.jit.bind_label(miss);
        self.a64_call_recompile(position, RecompileReason::ClassVersionGuardFailed);
        monoasm_arm64!(&mut self.jit, b deopt;); // recompiled -> resume via deopt
        self.jit.bind_label(done);
    }

    /// Recompile-or-deopt point. Counter-gates a one-shot recompile, then falls
    /// through to the deopt side exit. A `Whole` target calls
    /// `jit_recompile_method` (`position` = None) / `jit_recompile_loop`
    /// (`position` = Some loop-pc); a `Specialized` target calls
    /// `jit_recompile_specialized` on its `specialized_info` slot. Mirrors x86
    /// `side_exit_with_label`'s recompile arm.
    pub(in crate::codegen::jitgen) fn emit_recompile_deopt(
        &mut self,
        target: RecompileTarget,
        deopt: &DestLabel,
        error: Option<&DestLabel>,
        reason: RecompileReason,
    ) {
        let deopt = deopt.clone();
        // Counter-gated one-shot recompile, then fall through to the deopt side
        // exit (which undoes any loop sp-bump, writes back live values, and
        // re-enters the VM). The call helpers (`a64_call_recompile`,
        // `a64_call_recompile_specialized`) save the caller-saved d2-d7 FP pool
        // and x5-x8 GP pool (R8-R11) around the C call because the deopt
        // write-back that follows reads both (d8-d15 / x19-x28 are
        // callee-saved).
        let counter = Box::into_raw(Box::new(match target {
            RecompileTarget::Whole(_) => COUNT_DEOPT_RECOMPILE,
            RecompileTarget::Specialized(_) => COUNT_DEOPT_RECOMPILE_SPECIALIZED,
        })) as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (counter);
            ldr w11, [x9];
            cmp w11, #0;
        );
        self.jit.bcond_label(monoasm::Cond::Le, &deopt); // <= 0 -> just deopt
        monoasm_arm64!(&mut self.jit,
            sub w11, w11, #1;
            str w11, [x9];
            cbnz w11, deopt;                 // not yet exhausted -> just deopt
        );
        // counter hit 0: recompile once, then deopt.
        match target {
            RecompileTarget::Whole(position) => {
                self.a64_call_recompile(position, reason);
                // Check the compiler's return value: the recompiler caught a
                // panic, set a Ruby `FatalError`, and returned None (x0 = 0).
                // Branch to the error side-exit (write-back + raise via
                // entry_raise) instead of resuming the interpreter. On success
                // (x0 != 0) just deopt.
                if let Some(error) = error {
                    let error = error.clone();
                    monoasm_arm64!(&mut self.jit,
                        cbz x0, error;
                    );
                }
            }
            RecompileTarget::Specialized(idx) => {
                // `jit_recompile_specialized` returns nothing (it catches its
                // own panics and just leaves the old body installed), so there
                // is no error branch — mirrors the
                // `AsmInst::RecompileDeoptSpecialized` lowering.
                self.a64_call_recompile_specialized(self.specialized_base + idx, reason);
            }
        }
        monoasm_arm64!(&mut self.jit,
            b deopt;
        );
    }

    /// `ldr`/`str` use a 12-bit scaled (×8) immediate offset; bail above that.
    fn a64_field_off_ok(off: u32) -> bool {
        off <= 32760 && off % 8 == 0
    }

    /// Loop-JIT entry stack setup: pin `sp` to this frame's canonical depth
    /// (`a64_addr_sub` materializes an out-of-immediate-range offset).
    pub(in crate::codegen::jitgen) fn emit_loop_jit_rsp_bump(
        &mut self,
        offset: LoopRspOffset,
    ) -> bool {
        // This body is reached by `loop_start` from *either* producer of a
        // Ruby frame, and inherits that frame's `sp`. The two do not reserve
        // the same thing: the VM's `init_method` reserves the iseq's
        // `FnInitInfo::stack_offset` (`base - PROLOGUE_OVERHEAD`) and knows
        // nothing of spill slots, while an `AsmInst::Init` prologue reserves
        // `total - PROLOGUE_OVERHEAD` — the same area *plus this unit's
        // spill region*, since the region is part of `total`. Subtracting
        // `total - base` from what we inherit would therefore count the
        // spill region twice on the JIT-prologue path, landing that much
        // deeper than on the VM path.
        //
        // Pin `sp` to the frame's canonical depth instead, so both producers
        // agree: `total - PROLOGUE_OVERHEAD`, exactly what the prologue
        // reserves, which is what `offset` carries. The frames this body
        // then builds below `sp` stay where the compile assumed, which is
        // what the fixed `x29`-relative offsets addressing them
        // (`LoadDynVarSpecialized`, the spill slots) require. `x29` is
        // dependable here — `x29 - lfp` is invariant across entries.
        let below = offset.unwrap_concrete();
        self.a64_addr_sub(10, 29, below as u32);
        monoasm_arm64!(&mut self.jit, mov sp, x10;);
        true
    }

    /// `lhs === rhs` for an Array lhs via runtime::array_teq (x0=vm, x1=globals,
    /// x2=lhs, x3=rhs); Option<Value> result in x0. Bails as above.
    pub(in crate::codegen::jitgen) fn emit_array_teq(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::array_teq as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
        );
        self.a64_frame_load(2, lfp, off_l); // x2 = lhs
        self.a64_frame_load(3, lfp, off_r); // x3 = rhs
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// `any element truthy` for the array in `reg` via runtime::array_any
    /// (x0=vm, x1=globals, x2=val); Value result in x0. Cannot raise.
    pub(in crate::codegen::jitgen) fn emit_array_any(
        &mut self,
        reg: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = reg.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::array_any as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
        );
        self.a64_frame_load(2, lfp, off); // x2 = val
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// Build a Regexp from the `len` interpolated parts based at slot `arg` via
    /// runtime::concatenate_regexp (x0=vm, x1=globals, x2=&arg, x3=len);
    /// Option<Value> result in x0. The runtime reads `arg, arg-1, …` (descending
    /// addresses), matching the x86 `lea rdx,[rbp-rbp_local(arg)]`. Bails on a
    /// live xmm pool reg or an out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_concat_regexp(
        &mut self,
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = arg.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::concatenate_regexp as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
        );
        self.a64_addr_sub(2, lfp, off);  // x2 = &arg (slot address)
        monoasm_arm64!(&mut self.jit,
            mov x3, (len as u64);        // len
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// Multiple-assignment array expansion via
    /// runtime::expand_array(vm, globals, src, &dst, len, rest). `src` is
    /// already in GP::Rdx (x2) — the C-arg slot for `src` — from the
    /// preceding load; `dst` is the (descending) destination base
    /// x22-conv(dst). aarch64 C-args: x0=vm, x1=globals, x2=src, x3=&dst,
    /// x4=len, x5=rest (rest = rest_pos+1, or 0 for none). May dispatch
    /// `#to_ary` and raise — the caller emits a `HandleError` after this.
    pub(in crate::codegen::jitgen) fn emit_expand_array(
        &mut self,
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_fpr: UsingFpr,
    ) -> bool {
        let rest = if let Some(rest_pos) = rest_pos {
            rest_pos as u64 + 1
        } else {
            0
        };
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::expand_array as *const () as u64;
        // The fast path (when emitted) falls through to the runtime call
        // below on `slow`, and jumps past it on success.
        let fast = self.expand_array_fast_path(off, len, rest_pos);
        self.emit_fpr_save(using_fpr, false);
        // x2 already holds `src` (GP::Rdx). Fill the remaining C-args.
        // x19 = EXEC (&mut Executor), x20 = GLOBALS (&mut Globals).
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;   // &mut Executor
            mov x1, x20;   // &mut Globals
        );
        self.a64_addr_sub(3, lfp, off);  // x3 = &dst (descending base)
        monoasm_arm64!(&mut self.jit,
            mov x4, (len as u64);        // len
            mov x5, (rest);              // rest (0 = none)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        if let Some(exit) = fast {
            self.jit.bind_label(exit);
        }
        true
    }

    ///
    /// aarch64 twin of the x86 `expand_array_fast_path`: when `src` (x2) is
    /// already an `Array` holding at least `len` elements, destructuring is
    /// just `len` moves onto the destination slots. `off` is the byte
    /// displacement of `dst` below the LFP; slot `dst.0 + i` sits a further
    /// `i * 8` below. Cold blocks stay on this page (aarch64 b/b.cond cannot
    /// reach monoasm's second page).
    ///
    /// Returns the `exit` label the caller must bind after the runtime call
    /// (which doubles as the fall-through `slow` target), or `None` when no
    /// fast path was emitted.
    ///
    fn expand_array_fast_path(
        &mut self,
        off: u32,
        len: usize,
        rest_pos: Option<usize>,
    ) -> Option<DestLabel> {
        const MAX_INLINE_EXPAND: usize = 8;
        if rest_pos.is_some() || len == 0 || len > MAX_INLINE_EXPAND {
            return None;
        }
        let src = GP::Rdx.a64().0; // x2
        let lfp = GP::R14.a64().0; // x22
        let heap = self.jit.label();
        let copy = self.jit.label();
        let slow = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x9, (0b111);
            and x9, x(src), x9;
            cbnz x9, slow;                                  // immediate -> slow
            ldrb w9, [x(src), #(RVALUE_OFFSET_TY as u32)];
            cmp x9, #(ObjTy::ARRAY.get() as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &slow);
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(src), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x9, #(ARRAY_INLINE_CAPA as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &heap);
        // Inline buffer: x9 is the length, the elements follow in place.
        monoasm_arm64!(&mut self.jit,
            cmp x9, #(len as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &slow);
        monoasm_arm64!(&mut self.jit,
            add x11, x(src), #(RVALUE_OFFSET_INLINE as u32);
        );
        self.jit.bind_label(copy.clone());
        for i in 0..len {
            let src_disp = (i * 8) as u32;
            monoasm_arm64!(&mut self.jit, ldr x9, [x11, #(src_disp)];);
            self.a64_frame_store(9, lfp, off + (i * 8) as u32);
        }
        monoasm_arm64!(&mut self.jit,
            // Non-null x0: `expand_array` signals errors with a null return,
            // and the caller's `handle_error` checks it.
            mov x0, #(1);
            b exit;
        );
        self.jit.bind_label(heap);
        // Spilled buffer: x9 is the capacity, the length lives beside the
        // pointer.
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(src), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x10, #(len as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &slow);
        monoasm_arm64!(&mut self.jit,
            ldr x11, [x(src), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            b copy;
        );
        // The runtime call the caller emits next *is* the slow path.
        self.jit.bind_label(slow);
        Some(exit)
    }

    // ---- exception / non-local control flow -------------------------------
    // All four branch into `entry_raise` (the shared unwind/dispatch entry,
    // bound by a64_gen_entry_raise). None carry a `using_fpr` set — an
    // in-flight exception abandons the FP pool. C-arg regs: x0=vm (x19).

    /// `raise` — runtime::raise_err(vm, err_val) then unwind. The value to
    /// raise is in the accumulator scratch (GP::Rax = x0), so it is moved into
    /// x1 *before* x0 is overwritten with the executor.
    pub(in crate::codegen::jitgen) fn emit_raise(&mut self, loop_jit_spill_bytes: usize) -> bool {
        let raise = self.entry_raise();
        let acc = GP::Rax.a64().0; // x0
        let f = runtime::raise_err as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x1, x(acc);          // err_val (read before clobbering x0)
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        monoasm_arm64!(&mut self.jit, b raise;);
        true
    }

    /// `retry` — set PC (x21) to `pc + 1`, call runtime::err_retry(vm), unwind.
    pub(in crate::codegen::jitgen) fn emit_retry(
        &mut self,
        pc: BytecodePtr,
        loop_jit_spill_bytes: usize,
    ) -> bool {
        let raise = self.entry_raise();
        // Point PC at the retry instruction itself: aarch64's `entry_raise`
        // forwards PC to `handle_error` unchanged (x86 subtracts one bytecode),
        // and `handle_error` reads the retry op's `op1` disp to compute the
        // resume target `pc + 1 + disp`. Using `pc + 1` here would read the
        // *next* op's disp and jump to the wrong place. Mirrors
        // `a64_gen_handle_error`.
        let pcv = pc.as_ptr() as u64;
        let f = runtime::err_retry as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x21, (pcv);          // PC <- retry instruction
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        monoasm_arm64!(&mut self.jit, b raise;);
        true
    }

    /// `redo` — like `retry` but runtime::err_redo(vm).
    pub(in crate::codegen::jitgen) fn emit_redo(
        &mut self,
        pc: BytecodePtr,
        loop_jit_spill_bytes: usize,
    ) -> bool {
        let raise = self.entry_raise();
        // PC at the redo instruction itself (see `emit_retry` for why).
        let pcv = pc.as_ptr() as u64;
        let f = runtime::err_redo as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x21, (pcv);          // PC <- redo instruction
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        monoasm_arm64!(&mut self.jit, b raise;);
        true
    }

    /// End of an `ensure` clause — runtime::ensure_end(vm) returns a nonzero
    /// value when a pending exception must keep propagating (→ entry_raise);
    /// zero means fall through to the normal continuation.
    pub(in crate::codegen::jitgen) fn emit_ensure_end(&mut self, loop_jit_spill_bytes: usize) -> bool {
        let raise = self.entry_raise();
        let cont = self.jit.label();
        let f = runtime::ensure_end as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                  // x0 = 0 (continue) / nonzero (re-raise)
            ldr x30, [sp], #16;
            cbz x0, cont;            // continue: stay in the (still-bumped) loop body
        );
        // Re-raise path resumes the VM, so undo the loop sp-bump first.
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        monoasm_arm64!(&mut self.jit,
            b raise;
            cont:
        );
        true
    }

    /// If the outer LFP in `x(reg)` points at a stack frame already promoted to
    /// the heap (its Meta `kind` byte at `[lfp - 1]` has the `invalidated` bit
    /// 0b1000 set), forward the pointer to the live heap copy stored in the
    /// owning CFP's LFP slot (`[lfp + 8]`). Null `reg` (default ProcData on a
    /// no-block error) is left as-is. Mirrors x86 `resolve_invalidated_outer`.
    fn a64_resolve_invalidated_outer(&mut self, reg: u32) {
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            cbz x(reg), skip;             // null outer -> leave (error checked later)
            sub x10, x(reg), #(1u32);
            ldrb w9, [x10];               // Meta.kind byte
            mov x11, (0b1000u64);
            tst x9, x11;                  // invalidated bit?
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &skip); // clear -> not promoted
        monoasm_arm64!(&mut self.jit,
            ldr x(reg), [x(reg), #(8u32)]; // forward to heap copy (cfp.lfp slot)
            skip:
        );
    }

    /// `dst <- sp + (ofs - RSP_LOCAL_FRAME)` (the absolute callee-slot address).
    /// The displacement is usually negative (the callee frame sits below sp).
    /// Large displacements go through a GP temp (x9): reg 31 decodes as XZR, not
    /// SP, in the shifted-register add/sub, so sp is read via an sp-aware `mov`.
    /// `dst` must not be x9.
    fn a64_rsp_slot_addr(&mut self, ofs: i32, dst: u32) {
        debug_assert_ne!(dst, 9);
        let signed = ofs - RSP_LOCAL_FRAME;
        if signed >= 0 {
            let n = signed as u32;
            if n <= 4095 {
                monoasm_arm64!(&mut self.jit, add x(dst), sp, #(n););
            } else {
                monoasm_arm64!(&mut self.jit,
                    mov x(dst), sp;
                    mov x9, (n as u64);
                    add x(dst), x(dst), x9;
                );
            }
        } else {
            let n = (-signed) as u32;
            if n <= 4095 {
                monoasm_arm64!(&mut self.jit, sub x(dst), sp, #(n););
            } else {
                monoasm_arm64!(&mut self.jit,
                    mov x(dst), sp;
                    mov x9, (n as u64);
                    sub x(dst), x(dst), x9;
                );
            }
        }
    }

    /// Per-arch (aarch64) lowering for every `AsmInst` not handled by the
    /// arch-neutral `compile_asmir` dispatcher. Returns `false` for any
    /// not-yet-ported variant (the method then stays VM-interpreted).
    pub(in crate::codegen::jitgen) fn compile_asmir_arch(
        &mut self,
        store: &Store,
        _frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        class_version: DestLabel,
    ) -> bool {
        // The specialized (inlined-frame) AsmInst family is lowered here; every
        // other variant is handled by the shared `compile_asmir` dispatcher.
        // Anything still reaching the wildcard is not yet ported, so bail and
        // keep the method VM-interpreted.
        match inst {
            // Specialized class-version guard: on a version mismatch, recompile
            // the specialized body (rewriting its `SpecializedCall` `bl`) then
            // deopt. Mirrors x86 `guard_class_version_specialized` (no counter —
            // the recompile bakes in the new version, so it won't re-fire).
            // `dst <- [caller_fp - rbp_local(slot)]`. The caller's frame
            // pointer is the one saved at `[x29]` (the D1 gate guarantees
            // exactly one level up); x11 is a free lowering temp, and
            // `a64_frame_load` owns the offset-range handling through x10.
            AsmInst::LoadCallerSlot { slot, dst } => {
                let d = dst.a64().0;
                monoasm_arm64!(&mut self.jit,
                    ldr x11, [x29];
                );
                self.a64_frame_load(d, 11, rbp_local(slot) as u32);
            }
            AsmInst::GuardClassVersionSpecialized { idx, deopt } => {
                let global_idx = self.specialized_base + idx;
                let deopt = self.deopt_label(labels, deopt, DeoptCause::Static("class version (specialized)"));
                let miss = self.jit.label();
                let done = self.jit.label();
                self.a64_guard_class_version(&class_version, &miss); // mismatch -> miss
                monoasm_arm64!(&mut self.jit, b done;); // match -> continue
                self.jit.bind_label(miss.clone());
                self.a64_call_recompile_specialized(
                    global_idx,
                    RecompileReason::ClassVersionGuardFailed,
                );
                monoasm_arm64!(&mut self.jit, b deopt;);
                self.jit.bind_label(done);
            }
            // Constant-version twin of GuardClassVersionSpecialized: on a
            // version move, recompile this specialized entry (re-folding the
            // constants at the new version), then deopt.
            AsmInst::GuardConstVersionSpecialized {
                const_version: _,
                idx,
                deopt,
            } => {
                let global_idx = self.specialized_base + idx;
                let deopt = self.deopt_label(labels, deopt, DeoptCause::Static("const version (specialized)"));
                let miss = self.jit.label();
                let done = self.jit.label();
                let gv_addr = self
                    .jit
                    .get_label_address(&self.const_version_label())
                    .as_ptr() as u64;
                let unit_word = self
                    .unit_const_version
                    .clone()
                    .expect("const guard emitted outside a constant-folding unit");
                let unit_addr = self.jit.get_label_address(&unit_word).as_ptr() as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (gv_addr);
                    ldr x9, [x9];
                    mov x10, (unit_addr);
                    ldr x10, [x10];
                    cmp x9, x10;
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &miss); // mismatch -> miss
                monoasm_arm64!(&mut self.jit, b done;); // match -> continue
                self.jit.bind_label(miss.clone());
                self.a64_call_recompile_specialized(
                    global_idx,
                    RecompileReason::ConstVersionGuardFailed,
                );
                monoasm_arm64!(&mut self.jit, b deopt;);
                self.jit.bind_label(done);
            }
            // Counter-gated specialized recompile-or-deopt point (mirrors x86
            // `recompile_and_deopt_specialized`).
            AsmInst::RecompileDeoptSpecialized { idx, deopt, reason } => {
                let global_idx = self.specialized_base + idx;
                let deopt = self.deopt_label(labels, deopt, DeoptCause::Static("recompile counter (specialized)"));
                let counter =
                    Box::into_raw(Box::new(COUNT_DEOPT_RECOMPILE_SPECIALIZED)) as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x9, (counter);
                    ldr w11, [x9];
                    cmp w11, #0;
                );
                self.jit.bcond_label(monoasm::Cond::Le, &deopt); // <= 0 -> deopt
                monoasm_arm64!(&mut self.jit,
                    sub w11, w11, #1;
                    str w11, [x9];
                    cbnz w11, deopt;                 // not yet exhausted -> deopt
                );
                self.a64_call_recompile_specialized(global_idx, reason);
                monoasm_arm64!(&mut self.jit, b deopt;);
            }
            AsmInst::SetArgumentsForwarded {
                callid,
                callee_fid,
                recv,
                args,
                lead_num,
                kwrest_guard,
                deferred_src,
                kw_route,
            } => {
                let offset = store[callee_fid].get_offset();
                // D1 source-routed: the whole bind is a compile-time
                // constant (see `forwarded_deferred_layout`).
                return match deferred_src {
                    Some((src, len)) => {
                        let layout = store[callee_fid]
                            .forwarded_deferred_layout(lead_num, len as usize);
                        // K1: pair the routed caller slots with the
                        // callee's kw register base.
                        let kw_route =
                            kw_route.map(|route| (store[callee_fid].kw_reg_pos(), route));
                        self.a64_set_arguments_forwarded_deferred(
                            offset, recv, args, lead_num, src, layout, kw_route,
                        )
                    }
                    // Eager (rest Array materialized): the callee is req-only
                    // (the front-end routes optional-taking eager forwards to
                    // SetArgumentsForwardedHelper), so `expected_len` is the
                    // fixed req count minus the leading args. Inline the
                    // array-copy fast path with a helper fallback.
                    None => {
                        // The eager path never routes keywords (a
                        // kw-declaring callee is only admitted with an
                        // active deferral).
                        assert!(kw_route.is_none());
                        let expected_len = store[callee_fid].req_num() - lead_num;
                        self.a64_set_arguments_forwarded_eager(
                            callid,
                            callee_fid,
                            offset,
                            recv,
                            args,
                            lead_num,
                            expected_len,
                            kwrest_guard,
                        )
                    }
                };
            }
            // Every other AsmInst variant is handled by the shared
            // `compile_asmir` dispatcher before reaching here, so the wildcard
            // is unreachable (mirrors x86 `compile_asmir_arch`'s wildcard).
            _ => unreachable!("handled by the shared compile_asmir dispatcher"),
        }
        true
    }
}
