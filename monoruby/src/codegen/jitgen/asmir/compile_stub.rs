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

        let ir_vec = frame.detach_ir();

        // Basic blocks that actually emit code (or have an inline bridge).
        // Used to decide whether an inline bridge needs a trailing jump.
        let mut live_bb: std::collections::HashSet<BasicBlockId> =
            std::collections::HashSet::new();
        for (bb, ir) in ir_vec.iter() {
            if let Some(bb) = bb {
                if !ir.is_empty() || frame.inline_bridge_exists(*bb) {
                    live_bb.insert(*bb);
                }
            }
        }

        // Main blocks (+ their inline bridges). Bridges carry the trampoline
        // code that *binds* branch-target labels (e.g. a `CondBr` dest that is
        // distinct from the destination block's own label); skipping them was
        // why such branches resolved to offset 0 (an infinite self-loop).
        for (bbid, ir) in ir_vec.into_iter() {
            if !self.a64_gen_asm(ir, store, &mut frame, None, None, class_version.clone()) {
                return false;
            }
            if let Some((ir, exit)) = frame.remove_inline_bridge(bbid) {
                let exit = if let Some(bbid) = bbid {
                    if let Some(exit) = exit
                        && (bbid >= exit
                            || ((bbid + 1)..exit).any(|bb| live_bb.contains(&bb)))
                    {
                        Some(exit)
                    } else {
                        None
                    }
                } else {
                    None
                };
                if !self.a64_gen_asm(ir, store, &mut frame, None, exit, class_version.clone()) {
                    return false;
                }
            }
        }

        // Outlined bridges: each has an explicit entry label to bind and an
        // exit block to jump to. These commonly host the `CondBr`/branch
        // destinations.
        for (ir, entry, exit) in frame.detach_outline_bridges() {
            let entry = frame.resolve_label(&mut self.jit, entry);
            if !self.a64_gen_asm(
                ir,
                store,
                &mut frame,
                Some(entry),
                Some(exit),
                class_version.clone(),
            ) {
                return false;
            }
        }

        true
    }

    /// Lower one block's `AsmIr`. `entry` (if any) is bound first; `exit` (if
    /// any) appends an unconditional branch to that basic block at the end.
    /// Returns `false` (bail) on any unsupported instruction or side exit.
    fn a64_gen_asm(
        &mut self,
        ir: AsmIr,
        store: &Store,
        frame: &mut AsmInfo,
        entry: Option<DestLabel>,
        exit: Option<BasicBlockId>,
        class_version: DestLabel,
    ) -> bool {
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
        if let Some(skip) = &skip {
            monoasm_arm64!(&mut self.jit, b skip;);
        }
        let mut deopt_table: std::collections::HashMap<(BytecodePtr, WriteBack), DestLabel> =
            std::collections::HashMap::new();
        for side_exit in ir.side_exit {
            let label = match side_exit {
                // Eviction falls back to the interpreter like a deopt (the
                // `__immediate_evict` logging is `cfg(deopt/profile)`-only).
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    if !self.a64_gen_deopt(pc, &wb, label.clone()) {
                        return false;
                    }
                    label
                }
                SideExit::Deoptimize(pc, wb) => {
                    let key = (pc, wb);
                    if let Some(label) = deopt_table.get(&key) {
                        label.clone()
                    } else {
                        let label = self.jit.label();
                        if !self.a64_gen_deopt(key.0, &key.1, label.clone()) {
                            return false;
                        }
                        deopt_table.insert(key, label.clone());
                        label
                    }
                }
                // Treat recompile-deopt as a plain deopt for now: fall back to
                // the interpreter without the counter-gated recompile (still
                // correct, just not yet self-optimizing).
                SideExit::RecompileDeoptimize(pc, wb, _reason, _position) => {
                    let label = self.jit.label();
                    if !self.a64_gen_deopt(pc, &wb, label.clone()) {
                        return false;
                    }
                    label
                }
                SideExit::Error(pc, wb) => {
                    let label = self.jit.label();
                    if !self.a64_gen_handle_error(pc, &wb, label.clone()) {
                        return false;
                    }
                    label
                }
                // Evict(None) is not expected here.
                _ => return false,
            };
            labels.push(label);
        }
        if let Some(skip) = &skip {
            self.jit.bind_label(skip.clone());
        }

        if let Some(entry) = &entry {
            self.jit.bind_label(entry.clone());
        }
        for inst in ir.inst {
            if !self.compile_asmir(store, frame, &labels, inst, class_version.clone()) {
                return false;
            }
        }
        if let Some(exit) = exit {
            let exit = frame.resolve_bb_label(&mut self.jit, exit);
            monoasm_arm64!(&mut self.jit, b exit;);
        }
        true
    }

    /// Deopt handler: write all live Ruby values back to the LFP (so the frame
    /// is GC-consistent and the interpreter can resume), set PC, and jump to
    /// the VM fetch loop. Mirrors x86 `side_exit_with_label` (deopt path).
    /// Returns `false` (bail) if the write-back needs an unsupported feature.
    fn a64_gen_deopt(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) -> bool {
        self.jit.bind_label(entry);
        if !self.a64_gen_write_back_for_deopt(wb) {
            return false;
        }
        let pc_ptr = pc.as_ptr() as u64;
        let fetch = self.vm_fetch();
        // PC == x21.
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc_ptr);
            b fetch;
        );
        true
    }

    /// Error handler: write back, set PC to the *next* instruction, and jump to
    /// `entry_raise` (which calls `handle_error`). Mirrors x86 `gen_handle_error`.
    fn a64_gen_handle_error(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) -> bool {
        self.jit.bind_label(entry);
        if !self.a64_gen_write_back_for_deopt(wb) {
            return false;
        }
        let pc1 = (pc + 1isize).as_ptr() as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc1);
            b raise;
        );
        true
    }

    /// Write back live values to LFP slots for a side exit, r14(x22)-relative
    /// (the local frame may be on the heap after a call returns). Mirrors x86
    /// `gen_write_back_for_deopt`. Bails on FP / forwarding-rest write-backs,
    /// which need machinery not yet ported.
    fn a64_gen_write_back_for_deopt(&mut self, wb: &WriteBack) -> bool {
        if !wb.fpr.is_empty() || !wb.forward_rest.is_empty() {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        for (v, slot) in &wb.literal {
            if !self.a64_store_imm_to_slot(v.id(), *slot, lfp) {
                return false;
            }
        }
        for slot in &wb.void {
            if !self.a64_store_imm_to_slot(NIL_VALUE as u64, *slot, lfp) {
                return false;
            }
        }
        if let Some(slot) = wb.r15 {
            let off = slot.0 as u32 * 8 + LFP_SELF as u32;
            if off > 4095 {
                return false;
            }
            let acc = GP::R15.a64().0; // x23
            monoasm_arm64!(&mut self.jit,
                sub x10, x(lfp), #(off);
                str x(acc), [x10];
            );
        }
        true
    }

    /// Type guard: branch to `fail` if the Value in `reg` is not of `class_id`.
    /// Mirrors x86 `guard_class` (immediate tag checks + heap class check).
    /// Returns `false` (bail) for class kinds not yet handled.
    fn a64_guard_class(&mut self, reg: GP, class_id: ClassId, fail: &DestLabel) -> bool {
        let r = reg.a64().0;
        let fail = fail.clone();
        match class_id {
            INTEGER_CLASS => {
                // fixnum: bit0 == 1; fail when clear.
                monoasm_arm64!(&mut self.jit, tbz x(r), #(0), fail;);
            }
            NIL_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(NIL_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            TRUE_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(TRUE_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            FALSE_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(FALSE_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            SYMBOL_CLASS => {
                monoasm_arm64!(&mut self.jit,
                    mov x9, (0xff);
                    and x9, x(r), x9;
                    cmp x9, #(TAG_SYMBOL as u32);
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            FLOAT_CLASS => {
                // flonum (bit1 set, bit0 clear) is ok; fixnum (bit0) fails;
                // otherwise check the heap Float class.
                let exit = self.jit.label();
                monoasm_arm64!(&mut self.jit,
                    tbnz x(r), #(0), fail;   // fixnum -> fail
                    tbnz x(r), #(1), exit;   // flonum -> ok
                );
                self.a64_guard_rvalue(r, class_id, &fail);
                self.jit.bind_label(exit);
            }
            _ => {
                self.a64_guard_rvalue(r, class_id, &fail);
            }
        }
        true
    }

    /// Heap-object class guard: branch to `fail` unless `reg` is a heap pointer
    /// (low 3 bits zero) whose RValue class equals `class_id`. Mirrors x86
    /// `guard_rvalue`.
    fn a64_guard_rvalue(&mut self, r: u32, class_id: ClassId, fail: &DestLabel) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (0b111);
            and x9, x(r), x9;
            cbnz x9, fail;                                  // immediate -> fail
            ldr w9, [x(r), #(RVALUE_OFFSET_CLASS as u32)];  // RValue.class (u32)
            mov x10, (class_id.u32() as u64);
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, fail);
    }

    /// Class-version guard: branch to `fail` if the global class version no
    /// longer matches the version this method was compiled against. The cached
    /// version is baked in as an immediate (compilation is atomic at a fixed
    /// version). Unlike x86 we do not recompile on miss yet — just deopt.
    fn a64_guard_class_version(&mut self, fail: &DestLabel) {
        let gv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        let cached = self.class_version();
        monoasm_arm64!(&mut self.jit,
            mov x9, (gv_addr);
            ldr w9, [x9];
            mov x10, (cached as u64);
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, fail);
    }

    /// `[lfp - slot*8 - LFP_SELF] <- imm` via a scratch register (x9/x10).
    fn a64_store_imm_to_slot(&mut self, imm: u64, slot: SlotId, lfp: u32) -> bool {
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        monoasm_arm64!(&mut self.jit,
            mov x9, (imm);
            sub x10, x(lfp), #(off);
            str x9, [x10];
        );
        true
    }

    /// Walk `outer` outer-LFP links, leaving the target outer LFP in `dst`.
    /// `[lfp]` is the immediately-enclosing frame (LFP_OUTER == 0); each extra
    /// step dereferences again. Mirrors x86 `get_outer`. `outer >= 1`.
    fn a64_get_outer(&mut self, outer: usize, lfp: u32, dst: u32) {
        monoasm_arm64!(&mut self.jit, ldr x(dst), [x(lfp)];);
        for _ in 0..outer.saturating_sub(1) {
            monoasm_arm64!(&mut self.jit, ldr x(dst), [x(dst)];);
        }
    }

    /// Lower one `AsmInst`. Returns `false` for any not-yet-ported variant.
    pub(super) fn compile_asmir(
        &mut self,
        _store: &Store,
        frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        _class_version: DestLabel,
    ) -> bool {
        // lfp (x22) for slot access — same addressing as the VM's a64_op_ret:
        // slot `s` lives at `[lfp - s*8 - LFP_SELF]`.
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
            // rax <- dynamic (outer-frame) local. Walk `outer` outer-LFP links
            // (LFP_OUTER == 0, so `[lfp]` is the next outer frame), then load
            // the slot. Mirrors x86 `load_dyn_var` (`get_outer` + `movq rax,
            // [rax - offset]`).
            AsmInst::LoadDynVar { src } => {
                let off = src.reg.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let rax = GP::Rax.a64().0;
                self.a64_get_outer(src.outer, lfp, rax);
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(rax), #(off);
                    ldr x(rax), [x10];
                );
                true
            }
            // dynamic (outer-frame) local <- src. Symmetric to LoadDynVar.
            AsmInst::StoreDynVar { dst, src } => {
                let off = dst.reg.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                // walk to the outer LFP in x9 (avoid clobbering `src`).
                let outer = GP::Rcx.a64().0;
                let s = src.a64().0;
                if s == outer || s == 10 {
                    // src would be clobbered by the walk/scratch; bail (rare).
                    return false;
                }
                self.a64_get_outer(dst.outer, lfp, outer);
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(outer), #(off);
                    str x(s), [x10];
                );
                true
            }
            // rax <- $gvar via runtime::get_global_var(vm, globals, name).
            // EXEC (Executor) lives in x19, GLOBALS in x20 (= GP::R12). We have
            // no FP save/restore yet, so bail if any xmm is live across the call.
            AsmInst::LoadGVar { name, using_xmm } => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let f = runtime::get_global_var as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x0, x19;                  // vm (Executor)
                    mov x1, x20;                  // globals
                    mov x2, (name.get() as u64); // name (IdentId)
                    str x30, [sp, #-16]!;         // save LR across the call
                    mov x9, (f);
                    blr x9;                       // result in x0 (= rax)
                    ldr x30, [sp], #16;
                );
                true
            }
            // $gvar <- src via runtime::set_global_var(vm, globals, name, val).
            // Mirrors x86 store_gvar (the Option<Value> return is discarded).
            AsmInst::StoreGVar {
                name,
                src,
                using_xmm,
            } => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let off = src.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let f = runtime::set_global_var as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x0, x19;                  // vm (Executor)
                    mov x1, x20;                  // globals
                    mov x2, (name.get() as u64); // name (IdentId)
                    sub x10, x(lfp), #(off);
                    ldr x3, [x10];                // val (from slot)
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;
                    ldr x30, [sp], #16;
                );
                true
            }
            // Conditional branch on the truthiness of rax (x0). `orr 0x10`
            // folds nil(0x04) and false(0x14) to FALSE_VALUE(0x14); everything
            // else stays != FALSE_VALUE. BrIf jumps when truthy (Ne), BrIfNot
            // when falsy (Eq). Mirrors x86 `cond_br` and the VM's CondBr.
            AsmInst::CondBr(brkind, dest) => {
                let branch_dest = frame.resolve_label(&mut self.jit, dest);
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit,
                    mov x10, (0x10);
                    orr x(rax), x(rax), x10;
                    cmp x(rax), #(FALSE_VALUE as u32);
                );
                let cond = match brkind {
                    BrKind::BrIf => monoasm::Cond::Ne,
                    BrKind::BrIfNot => monoasm::Cond::Eq,
                };
                self.jit.bcond_label(cond, &branch_dest);
                true
            }
            // Branch to dest if rax (x0) is nil. Mirrors x86 `cmpq rax,
            // NIL_VALUE; jeq dest`.
            AsmInst::NilBr(dest) => {
                let branch_dest = frame.resolve_label(&mut self.jit, dest);
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit,
                    cmp x(rax), #(NIL_VALUE as u32);
                );
                self.jit.bcond_label(monoasm::Cond::Eq, &branch_dest);
                true
            }
            // [lfp - slot*8 - LFP_SELF] <- literal Value. The immediate is
            // materialized in a scratch reg (aarch64 has no store-immediate),
            // so no GP register is clobbered. Mirrors x86 `literal_to_stack`.
            AsmInst::LitToStack(v, slot) => {
                let off = slot.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let imm = v.id();
                monoasm_arm64!(&mut self.jit,
                    mov x9, (imm);
                    sub x10, x(lfp), #(off);
                    str x9, [x10];
                );
                true
            }
            // rax <- deep copy of literal Value (so each evaluation yields a
            // fresh mutable object). Runtime C call; bails if any xmm is live
            // (no FP save/restore yet). Mirrors x86 `deepcopy_literal`.
            AsmInst::DeepCopyLit(v, using_xmm) => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let imm = v.id();
                let f = Value::value_deep_copy as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x0, (imm);
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;            // result in x0 (= rax)
                    ldr x30, [sp], #16;
                );
                true
            }
            // Type guard: deopt if `r` is not of `class`.
            AsmInst::GuardClass(r, class, deopt) => {
                let deopt = labels[deopt].clone();
                self.a64_guard_class(r, class, &deopt)
            }
            // Inline-cache class-version guard: deopt if the global class
            // version changed since compilation.
            AsmInst::GuardClassVersion { deopt, .. } => {
                let deopt = labels[deopt].clone();
                self.a64_guard_class_version(&deopt);
                true
            }
            // Unconditional deopt to the interpreter.
            AsmInst::Deopt(deopt) => {
                let deopt = labels[deopt].clone();
                monoasm_arm64!(&mut self.jit, b deopt;);
                true
            }
            // Error check: a runtime helper returns rax==0 (None) on error;
            // branch to the error side-exit handler in that case. Mirrors x86
            // `handle_error` (`testq rax,rax; jeq error`).
            AsmInst::HandleError(error) => {
                let error = labels[error].clone();
                let rax = GP::Rax.a64().0;
                monoasm_arm64!(&mut self.jit, cbz x(rax), error;);
                true
            }
            // rax <- Array built from the `len` slots starting at `src`.
            // create_array(ptr=&slot[src], len). No xmm save (matches x86).
            AsmInst::CreateArray { src, len } => {
                let off = src.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let f = runtime::create_array as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    sub x0, x(lfp), #(off);   // &slot[src]
                    mov x1, (len as u64);
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;                   // result in x0
                    ldr x30, [sp], #16;
                );
                true
            }
            // rax <- Array literal via gen_array(vm, globals, callid, &self).
            AsmInst::NewArray { callid, using_xmm } => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let f = runtime::gen_array as *const () as u64;
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
                true
            }
            // rax <- Hash literal via gen_hash(vm, globals, &slot[args], len).
            AsmInst::NewHash(args, len, using_xmm) => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let off = args.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let f = runtime::gen_hash as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x0, x19;              // vm
                    mov x1, x20;              // globals
                    sub x2, x(lfp), #(off);   // &slot[args]
                    mov x3, (len as u64);
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;
                    ldr x30, [sp], #16;
                );
                true
            }
            // rax <- concatenated string via concatenate_string(vm, globals,
            // &slot[arg], len). Result is Option<Value> (followed by a
            // HandleError in the IR). Bails if any xmm is live.
            AsmInst::ConcatStr { arg, len, using_xmm } => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let off = arg.0 as u32 * 8 + LFP_SELF as u32;
                if off > 4095 {
                    return false;
                }
                let f = runtime::concatenate_string as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    mov x0, x19;              // vm
                    mov x1, x20;              // globals
                    sub x2, x(lfp), #(off);   // &slot[arg]
                    mov x3, (len as u64);
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;
                    ldr x30, [sp], #16;
                );
                true
            }
            // rax <- Range via gen_range(start, end, vm, globals, exclude_end).
            AsmInst::NewRange { start, end, exclude_end, using_xmm } => {
                if using_xmm.iter().any(|b| *b) {
                    return false;
                }
                let soff = start.0 as u32 * 8 + LFP_SELF as u32;
                let eoff = end.0 as u32 * 8 + LFP_SELF as u32;
                if soff > 4095 || eoff > 4095 {
                    return false;
                }
                let f = runtime::gen_range as *const () as u64;
                monoasm_arm64!(&mut self.jit,
                    sub x10, x(lfp), #(soff);
                    ldr x0, [x10];            // start value
                    sub x10, x(lfp), #(eoff);
                    ldr x1, [x10];            // end value
                    mov x2, x19;              // vm
                    mov x3, x20;              // globals
                    mov x4, (exclude_end as u64);
                    str x30, [sp, #-16]!;
                    mov x9, (f);
                    blr x9;
                    ldr x30, [sp], #16;
                );
                true
            }
            // Phase 3b: more AsmInst lowerings land here, one category at a time.
            _ => false,
        }
    }
}
