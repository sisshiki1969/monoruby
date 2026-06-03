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
        // Specialized (inlined) frames are unsupported by the A64 lowering, so
        // the front-end never builds them on aarch64 (see the `cfg!(jit_x86)`
        // gate in compile.rs / method_call.rs). This stays as a defensive net:
        // if one ever appears, bail to the VM rather than emit wrong code.
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

        // Dump the generated A64 machine code, mirroring x86's `gen_machine_code`
        // tail. `dump_code` shells out to GNU objdump (set `OBJDUMP` to a
        // binutils objdump on macOS, where the system objdump is LLVM's and
        // rejects `-b binary`). The trailing `finalize` resolves branch
        // displacements so the listing shows real targets; `compile_patch`
        // finalizes again after emitting the class-guard stub (same as x86).
        #[cfg(feature = "emit-asm")]
        if self.startup_flag {
            self.jit.finalize();
            let iseq_id = frame.iseq_id;
            let fid = store[iseq_id].func_id();
            eprintln!("  >>> JIT (aarch64) <{}>", store.func_description(fid));
            self.dump_disas(store, &frame.sourcemap, iseq_id);
            eprintln!("  <<<");
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
    fn a64_gen_handle_error(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) -> bool {
        self.jit.bind_label(entry);
        if !self.a64_gen_write_back_for_deopt(wb) {
            return false;
        }
        let pc0 = pc.as_ptr() as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc0);
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

    /// Inline fixnum binary op. Fixnums are tagged `2n+1`; signed 64-bit
    /// overflow of the tagged arithmetic == fixnum overflow, so we branch to
    /// `deopt` on the V flag. Result is left in `lhs`'s register, mirroring x86
    /// `integer_binop`. Mul/Div/etc. not yet ported (bail).
    fn a64_integer_binop(
        &mut self,
        lhs: GP,
        rhs: GP,
        mode: &OpMode,
        kind: BinOpK,
        deopt: &DestLabel,
    ) -> bool {
        let l = lhs.a64().0;
        let r = rhs.a64().0;
        match kind {
            BinOpK::Add => {
                match mode {
                    OpMode::RR(..) => monoasm_arm64!(&mut self.jit,
                        sub x(l), x(l), #(1u32);
                        adds x(l), x(l), x(r);
                    ),
                    OpMode::RI(_, i) | OpMode::IR(i, _) => {
                        let imm = Value::i32(*i as i32).id().wrapping_sub(1);
                        monoasm_arm64!(&mut self.jit,
                            mov x9, (imm);
                            adds x(l), x(l), x9;
                        );
                    }
                }
                self.jit.bcond_label(monoasm::Cond::Vs, deopt);
            }
            BinOpK::Sub => match mode {
                OpMode::RR(..) => {
                    monoasm_arm64!(&mut self.jit, subs x(l), x(l), x(r););
                    self.jit.bcond_label(monoasm::Cond::Vs, deopt);
                    monoasm_arm64!(&mut self.jit, add x(l), x(l), #(1u32););
                }
                OpMode::RI(_, rhs_i) => {
                    let imm = Value::i32(*rhs_i as i32).id().wrapping_sub(1);
                    monoasm_arm64!(&mut self.jit,
                        mov x9, (imm);
                        subs x(l), x(l), x9;
                    );
                    self.jit.bcond_label(monoasm::Cond::Vs, deopt);
                }
                OpMode::IR(lhs_i, _) => {
                    let imm = Value::i32(*lhs_i as i32).id();
                    monoasm_arm64!(&mut self.jit,
                        mov x(l), (imm);
                        subs x(l), x(l), x(r);
                    );
                    self.jit.bcond_label(monoasm::Cond::Vs, deopt);
                    monoasm_arm64!(&mut self.jit, add x(l), x(l), #(1u32););
                }
            },
            // Mul: compute `2a * b` (matching x86's `imul` on the half-untagged
            // lhs). aarch64 has no `smulh`, so detect overflow with a checking
            // `sdiv`: if `2a != 0` and `(2a*b)/(2a) != b` the product wrapped.
            // Mul: compute `2a * b` (matching x86's `imul` on the half-untagged
            // lhs), then detect 64-bit signed overflow the standard way — the
            // high half (`smulh`) must equal the sign-extension of the low half
            // (`low >> 63`); a mismatch means the product wrapped.
            BinOpK::Mul => {
                match mode {
                    OpMode::RR(..) => monoasm_arm64!(&mut self.jit,
                        asr x(r), x(r), #(1u32);   // b (untagged)
                        sub x(l), x(l), #(1u32);   // 2a
                    ),
                    OpMode::RI(_, i) | OpMode::IR(i, _) => {
                        let imm = *i as i64 as u64; // raw multiplier
                        monoasm_arm64!(&mut self.jit,
                            mov x(r), (imm);
                            sub x(l), x(l), #(1u32);
                        );
                    }
                }
                monoasm_arm64!(&mut self.jit, mul x9, x(l), x(r);); // low 64
                monoasm_arm64!(&mut self.jit, smulh x10, x(l), x(r););
                monoasm_arm64!(&mut self.jit,
                    asr x11, x9, #(63u32);         // sign-extension of low
                    cmp x10, x11;                  // high == sign(low)?
                );
                self.jit.bcond_label(monoasm::Cond::Ne, deopt);
                monoasm_arm64!(&mut self.jit, add x(l), x9, #(1u32););
            }
            // Div: Ruby integer division floors toward negative infinity, but
            // `sdiv` truncates toward zero, so adjust the quotient down by 1
            // when the remainder is non-zero and its sign differs from the
            // divisor's. Both operands are already in registers (the front-end
            // materializes RI/IR immediates), and the result goes to rax (x0),
            // matching x86 `integer_binop`'s Div. b==0 deopts (ZeroDivisionError).
            BinOpK::Div => {
                let rax = GP::Rax.a64().0;
                let done = self.jit.label();
                let deopt = deopt.clone();
                monoasm_arm64!(&mut self.jit,
                    asr x(r), x(r), #(1u32);     // b (untagged)
                    cbz x(r), deopt;             // b==0 -> ZeroDivisionError (deopt)
                    asr x9, x(l), #(1u32);       // a (untagged)
                    sdiv x10, x9, x(r);          // q = trunc(a/b)
                    msub x11, x10, x(r), x9;     // rem = a - q*b
                    cbz x11, done;               // exact -> no floor adjust
                    eor x12, x11, x(r);          // rem ^ b
                    tbz x12, #(63), done;        // same sign -> no adjust
                    sub x10, x10, #(1u32);       // floor: q -= 1
                    done:
                    lsl x(rax), x10, #(1u32);    // 2q
                    add x(rax), x(rax), #(1u32); // 2q+1 (tagged)
                );
            }
            // Rem/bit-ops are compiled as method calls, never IntegerBinOp.
            _ => return false,
        }
        true
    }

    /// Compare two tagged fixnums (the tag preserves order). Mirrors x86
    /// `cmp_integer`.
    fn a64_cmp_integer(&mut self, mode: &OpMode, lhs: GP, rhs: GP) {
        let l = lhs.a64().0;
        match mode {
            OpMode::RR(..) => {
                let r = rhs.a64().0;
                monoasm_arm64!(&mut self.jit, cmp x(l), x(r););
            }
            OpMode::RI(_, i) => {
                let imm = Value::i32(*i as i32).id();
                monoasm_arm64!(&mut self.jit, mov x9, (imm); cmp x(l), x9;);
            }
            OpMode::IR(i, _) => {
                let r = rhs.a64().0;
                let imm = Value::i32(*i as i32).id();
                monoasm_arm64!(&mut self.jit, mov x(l), (imm); cmp x(l), x(r););
            }
        }
    }

    /// After `a64_cmp_integer`, materialize a Ruby boolean in rax (x0):
    /// `FALSE_VALUE | (cond << 3)` (== 0x14 or 0x1c). Mirrors the VM's
    /// `a64_op_cmp` and x86 `flag_to_bool`.
    fn a64_flag_to_bool(&mut self, kind: CmpKind) {
        let cond = a64_cond_for_cmp(kind, BrKind::BrIf);
        let rax = GP::Rax.a64();
        self.jit.cset(rax, cond);
        monoasm_arm64!(&mut self.jit,
            lsl x(rax.0), x(rax.0), #(3u32);
            mov x9, (FALSE_VALUE);
            orr x(rax.0), x(rax.0), x9;
        );
    }

    /// `[sp - off] <- x9`. The callee frame is built at negative offsets from
    /// the current sp (mirrors x86 `[rsp - off]`).
    fn a64_store_x9_below_sp(&mut self, off: u32) {
        monoasm_arm64!(&mut self.jit,
            sub x10, sp, #(off);
            str x9, [x10];
        );
    }

    /// Lower `SetupMethodFrame`: write the callee frame's outer/meta/svar/cme
    /// and block fields at `[sp - (RSP_LOCAL_FRAME + LFP_*)]`. Mirrors x86
    /// `setup_method_frame`.
    fn a64_setup_method_frame(
        &mut self,
        store: &Store,
        meta: Meta,
        callid: CallSiteId,
        outer_lfp: Option<Lfp>,
    ) {
        let outer = match outer_lfp {
            Some(lfp) => lfp.as_ptr() as u64,
            None => 0,
        };
        monoasm_arm64!(&mut self.jit, mov x9, (outer););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
        monoasm_arm64!(&mut self.jit, mov x9, (meta.get()););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_META) as u32);
        monoasm_arm64!(&mut self.jit, mov x9, (0u64););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_CME) as u32);
        let callsite = &store[callid];
        let (block_fid, block_arg) = (callsite.block_fid, callsite.block_arg);
        self.a64_set_block(block_fid, block_arg);
    }

    /// Write the callee frame's block-handler slot. Mirrors x86 `set_block`.
    fn a64_set_block(&mut self, block_fid: Option<FuncId>, block_arg: Option<SlotId>) {
        let block_off = (RSP_LOCAL_FRAME + LFP_BLOCK) as u32;
        if let Some(func_id) = block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm_arm64!(&mut self.jit, mov x9, (bh.id()););
        } else if let Some(block) = block_arg {
            let lfp = GP::R14.a64().0;
            let off = block.0 as u32 * 8 + LFP_SELF as u32;
            monoasm_arm64!(&mut self.jit,
                sub x10, x(lfp), #(off);
                ldr x9, [x10];
            );
        } else {
            monoasm_arm64!(&mut self.jit, mov x9, (0u64););
        }
        self.a64_store_x9_below_sp(block_off);
    }

    /// Lower `SetArguments`: one C call to `jit_generic_set_arguments(vm,
    /// globals, callid, callee_lfp, fid)` which massages the caller's args into
    /// the callee frame. Returns rax==0 (None) on error (followed by a
    /// HandleError in the IR). `offset` (callee frame size, 16-aligned) is
    /// reserved below sp around the call. Bails if it exceeds a 12-bit imm.
    fn a64_set_arguments(&mut self, callid: CallSiteId, fid: FuncId, offset: usize) -> bool {
        if offset > 4080 {
            return false;
        }
        let f = crate::runtime::jit_generic_set_arguments as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, sp, #(RSP_LOCAL_FRAME as u32); // callee_lfp (call-site sp)
            mov x4, (fid.get() as u64);        // callee fid
            str x30, [sp, #-16]!;              // save LR
            sub sp, sp, #(offset as u32);      // reserve callee scratch
            mov x9, (f);
            blr x9;
            add sp, sp, #(offset as u32);
            ldr x30, [sp], #16;                // restore LR
        );
        true
    }

    /// Lower `Call` (the call itself): set the callee LFP, push a control
    /// frame, set PC, `blr` the callee codeptr, then restore the caller's
    /// cfp/lfp. Mirrors x86 `do_call` (set_lfp + push_frame + call + pop_frame)
    /// and the VM invoker's `aftargs` sequence. The eviction-on-return
    /// patching (`set_deopt_with_return_addr`) is x86-only (runtime branch
    /// patching), so it is skipped — class-version changes are caught by
    /// `GuardClassVersion` deopts instead.
    fn a64_do_call(&mut self, store: &Store, callee_fid: FuncId, call_site_bc_ptr: BytecodePtr) {
        let callee = &store[callee_fid];
        let is_iseq = callee.is_iseq().is_some();
        let (_meta, codeptr, pc) = callee.get_data();
        let codeptr_addr = codeptr.as_ptr() as u64;
        // set_lfp + push_frame (EXEC=x19, LFP=x22).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)]; // prev cfp
            sub x11, sp, #(RSP_CFP as u32);          // new cfp addr
            str x10, [x11];                          // new_cfp.prev = prev
            str x11, [x19, #(EXECUTOR_CFP as u32)];  // exec.cfp = new cfp
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);  // callee LFP
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x22, [x10];                          // new_cfp.lfp = LFP
        );
        if is_iseq {
            // iseq: PC <- callee pc (read by the VM tier / prologue).
            if let Some(pc) = pc {
                let pc_ptr = pc.as_ptr() as u64;
                monoasm_arm64!(&mut self.jit, mov x21, (pc_ptr););
            }
        } else {
            // builtin: x3 is the 4th C-arg = the `pc` parameter, which with-pc
            // builtins use as the call-site bytecode pointer. The native-func
            // wrapper passes x3 through untouched (mirrors x86 do_call setting
            // rcx to the call-site bc ptr).
            let cs = call_site_bc_ptr.as_ptr() as u64;
            monoasm_arm64!(&mut self.jit, mov x3, (cs););
        }
        monoasm_arm64!(&mut self.jit,
            mov x10, (codeptr_addr);
            blr x10;                                 // result in x0
        );
        // pop_frame: restore caller cfp + lfp from x29 (== x86 rbp).
        monoasm_arm64!(&mut self.jit,
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
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

    /// Physical D-register index for a pool-resident `FPReg`, or `None` (bail →
    /// the method stays VM-interpreted) otherwise.
    ///
    /// Capped to the caller-saved D2-D7: under AAPCS64 D8-D15 are callee-saved
    /// (unlike x86-64 SysV where every xmm is caller-saved), and the aarch64
    /// invoker / JIT prologue does not yet preserve them. Restricting the JIT'd
    /// FP pool to caller-saved registers keeps the ABI sound without that
    /// prologue work; methods needing >6 live floats (or any spill) just don't
    /// JIT yet. `f64_to_val`'s heap path and `XmmSave` are consistent with this
    /// (they only ever touch D2-D7).
    fn a64_fpr(&self, x: FPReg, base: usize) -> Option<u32> {
        match x.loc(base) {
            FPRegLoc::Xmm(p) if p <= 7 => Some(p as u32),
            _ => None,
        }
    }

    // ---- specialized (inlined) frame lowering (aarch64) -------------------

    /// `MethodRetSpecialized` / `BlockBreakSpecialized`: a clean return that
    /// unwinds `rbp_offset` bytes of inlined frames at once. Mirrors x86
    /// `method_return_specialized` (`lea rbp,[rbp+off]; leave; ret`): adjust the
    /// native frame base (x29), then run the standard epilogue. No error path —
    /// the value is already in the accumulator and the caller frame is JIT'd.
    fn a64_method_ret_specialized(&mut self, rbp_offset: usize) {
        monoasm_arm64!(&mut self.jit,
            mov x10, (rbp_offset as u64);
            add x29, x29, x10;          // rbp += off (skip inlined frames)
            mov sp, x29;                // leave
            ldp x29, x30, [sp], #(16);  // restore caller fp/lr
            ret;
        );
    }

    /// `LoadDynVarSpecialized`: rax <- outer-scope local at a pre-resolved
    /// frame-base offset. Mirrors x86
    /// `movq rax, [rbp + (offset - (BP_CFP+CFP_LFP) - 8 - conv(reg))]`. The
    /// effective displacement can be negative, so it is materialized in a
    /// scratch and added to x29 (x9..x15 are reserved lowering temps, never a
    /// GP-mapped register).
    fn a64_load_dyn_var_specialized(&mut self, offset: usize, reg: SlotId) {
        let e: i64 = offset as i64 - (BP_CFP + CFP_LFP) as i64 - 8 - conv(reg) as i64;
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x10, (e as u64);
            add x10, x29, x10;
            ldr x(rax), [x10];
        );
    }

    /// `StoreDynVarSpecialized`: outer-scope local <- src, symmetric to
    /// `a64_load_dyn_var_specialized`. `src` maps to x0..x8 / x20..x23, never
    /// the x10 scratch, so there is no clobber.
    fn a64_store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
        let e: i64 = offset as i64 - (BP_CFP + CFP_LFP) as i64 - 8 - conv(dst) as i64;
        let s = src.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x10, (e as u64);
            add x10, x29, x10;
            str x(s), [x10];
        );
    }

    /// `SpecializedCall` / `SpecializedYield`: a direct branch-with-link into
    /// an inlined method/block entry already emitted in this code buffer.
    /// Mirrors x86 `do_specialized_call`: set_lfp + push_frame, optionally bind
    /// the deopt re-entry `patch_point`, `bl entry`, then pop_frame. Returns the
    /// post-`bl` address (the return continuation); the caller records it via
    /// `set_deopt_with_return_addr` so `immediate_eviction` can later overwrite
    /// the continuation with a `B deopt` on BOP redefinition.
    fn a64_do_specialized_call(
        &mut self,
        entry: DestLabel,
        patch_point: Option<DestLabel>,
    ) -> CodePtr {
        // set_lfp + push_frame (mirror a64_do_call).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)]; // prev cfp
            sub x11, sp, #(RSP_CFP as u32);          // new cfp addr
            str x10, [x11];                          // new_cfp.prev = prev
            str x11, [x19, #(EXECUTOR_CFP as u32)];  // exec.cfp = new cfp
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);  // callee LFP
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x22, [x10];                          // new_cfp.lfp = LFP
        );
        if let Some(patch) = patch_point {
            self.jit.bind_label(patch);
        }
        monoasm_arm64!(&mut self.jit, bl entry;);
        let return_addr = self.jit.get_current_address();
        // pop_frame: restore caller cfp + lfp from x29 (== x86 rbp).
        monoasm_arm64!(&mut self.jit,
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
        return_addr
    }

    // ---- emission primitives (aarch64) ------------------------------------
    // Tiny arch-specific helpers the arch-neutral `compile_asmir` dispatcher
    // calls. The x86 twins live in `compile.rs`. Slot `s` lives at
    // `[lfp(x22) - s*8 - LFP_SELF]` (same addressing as the VM's `a64_op_ret`).

    /// dst <- src (general-purpose register move; self-move is a no-op).
    pub(in crate::codegen::jitgen) fn emit_reg_move(&mut self, src: GP, dst: GP) {
        let (s, d) = (src.a64().0, dst.a64().0);
        if s != d {
            monoasm_arm64!(&mut self.jit, mov x(d), x(s););
        }
    }

    /// [lfp - slot*8 - LFP_SELF] <- reg
    pub(in crate::codegen::jitgen) fn emit_reg_to_stack(&mut self, r: GP, slot: SlotId) {
        let lfp = GP::R14.a64().0;
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        let src = r.a64().0;
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            str x(src), [x10];
        );
    }

    /// reg <- [lfp - slot*8 - LFP_SELF]
    pub(in crate::codegen::jitgen) fn emit_stack_to_reg(&mut self, slot: SlotId, r: GP) {
        let lfp = GP::R14.a64().0;
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        let dst = r.a64().0;
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            ldr x(dst), [x10];
        );
    }

    /// reg <- literal Value (immediate)
    pub(in crate::codegen::jitgen) fn emit_lit_to_reg(&mut self, v: Value, r: GP) {
        let dst = r.a64().0;
        let imm = v.id();
        monoasm_arm64!(&mut self.jit, mov x(dst), (imm););
    }

    /// [lfp - slot*8 - LFP_SELF] <- literal Value. The immediate is
    /// materialized in a scratch reg (aarch64 has no store-immediate), so no GP
    /// register is clobbered. Bails (`false`) if the frame offset exceeds the
    /// 12-bit immediate range. Mirrors x86 `literal_to_stack`.
    pub(in crate::codegen::jitgen) fn emit_lit_to_stack(&mut self, v: Value, slot: SlotId) -> bool {
        let lfp = GP::R14.a64().0;
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

    /// Conditional branch on the truthiness of rax (x0). `orr 0x10` folds
    /// nil(0x04) and false(0x14) to FALSE_VALUE(0x14); everything else stays
    /// != FALSE_VALUE. BrIf jumps when truthy (Ne), BrIfNot when falsy (Eq).
    /// Mirrors x86 `cond_br` and the VM's CondBr.
    pub(in crate::codegen::jitgen) fn emit_cond_br(&mut self, dest: DestLabel, brkind: BrKind) {
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
        self.jit.bcond_label(cond, &dest);
    }

    /// Branch to dest if rax (x0) is nil. Mirrors x86 `cmpq rax, NIL_VALUE; jeq`.
    pub(in crate::codegen::jitgen) fn emit_nil_br(&mut self, dest: DestLabel) {
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            cmp x(rax), #(NIL_VALUE as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &dest);
    }

    /// Branch to dest if the local (rax/x0) is already set (non-zero).
    /// Mirrors x86 `testq rax, rax; jnz dest`.
    pub(in crate::codegen::jitgen) fn emit_check_local(&mut self, dest: DestLabel) {
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit, cbnz x(rax), dest;);
    }

    /// Type guard: deopt (jump to `fail`) if `r`'s class is not `class`.
    /// Returns `false` (bail) for not-yet-supported class kinds.
    pub(in crate::codegen::jitgen) fn emit_guard_class(
        &mut self,
        r: GP,
        class: ClassId,
        fail: &DestLabel,
    ) -> bool {
        self.a64_guard_class(r, class, fail)
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
        _base: usize,
    ) -> bool {
        let error = error.clone();
        let ok = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            ldr x11, [x19, #(crate::executor::EXECUTOR_STACK_LIMIT as u32)];
            cmp x10, x11;
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &ok); // sp > limit -> ok
        if !self.a64_gen_write_back_for_deopt(&write_back) {
            return false;
        }
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

    /// GC safepoint: if alloc_flag >= 8 (signal/gc-stress nudge), write back
    /// live values, run execute_gc(vm, globals), and on error jump to the error
    /// handler. The GC path is laid out inline but skipped on the common path.
    /// Bails (`false`) like `emit_check_stack`. `base` is unused on aarch64.
    /// Mirrors x86 `jit_execute_gc`.
    pub(in crate::codegen::jitgen) fn emit_exec_gc(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        _base: usize,
    ) -> bool {
        let error = error.clone();
        let skip = self.jit.label();
        let af_addr = self
            .jit
            .get_label_address(&self.alloc_flag.clone())
            .as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (af_addr);
            ldr w9, [x9];
            cmp x9, #(8u32);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &skip); // < 8 -> no GC
        if !self.a64_gen_write_back_for_deopt(&write_back) {
            return false;
        }
        let f = crate::executor::execute_gc as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            cbz x0, error;             // None -> error
        );
        self.jit.bind_label(skip);
        true
    }

    /// Constant base-class guard: deopt if the accumulator (rax/x0) is not the
    /// cached base class. Mirrors x86 `GuardConstBaseClass`.
    pub(in crate::codegen::jitgen) fn emit_guard_const_base_class(
        &mut self,
        base_class: Value,
        deopt: &DestLabel,
    ) {
        let deopt = deopt.clone();
        let rax = GP::Rax.a64().0;
        let cached = base_class.id() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x10, (cached);
            cmp x(rax), x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
    }

    /// Constant version guard: deopt if the global constant version moved since
    /// compilation (`const_version` is the baked-in cached value). Mirrors x86
    /// `guard_const_version`.
    pub(in crate::codegen::jitgen) fn emit_guard_const_version(
        &mut self,
        const_version: usize,
        deopt: &DestLabel,
    ) {
        let deopt = deopt.clone();
        let gv_addr = self
            .jit
            .get_label_address(&self.const_version_label())
            .as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (gv_addr);
            ldr x9, [x9];
            mov x10, (const_version as u64);
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
    }

    /// Store the accumulator to a constant via set_constant(vm, globals, id,
    /// val), bumping the global constant version. Bails (`false`) if any xmm is
    /// live (no FP save/restore yet). Mirrors x86 `store_constant` + error check.
    pub(in crate::codegen::jitgen) fn emit_store_constant(
        &mut self,
        id: ConstSiteId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let error = error.clone();
        let cv_addr = self
            .jit
            .get_label_address(&self.const_version_label())
            .as_ptr() as u64;
        let f = runtime::set_constant as *const () as u64;
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
            cbz x0, error;              // None -> error
        );
        true
    }

    // ---- variable-access primitives (aarch64) -----------------------------
    // gvar/cvar go through a runtime C call; dynvar walks the outer-LFP chain.
    // All bail (`false`) on a live xmm / out-of-range offset (no FP save yet).

    /// rax <- $gvar via runtime::get_global_var(vm, globals, name).
    pub(in crate::codegen::jitgen) fn emit_load_gvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
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

    /// $gvar <- src via runtime::set_global_var(vm, globals, name, val).
    pub(in crate::codegen::jitgen) fn emit_store_gvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
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

    /// rax <- @@cvar via runtime::get_class_var(vm, globals, name).
    pub(in crate::codegen::jitgen) fn emit_load_cvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let f = runtime::get_class_var as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                       // result in x0
            ldr x30, [sp], #16;
        );
        true
    }

    /// rax <- dynamic (outer-frame) local. Walk `outer` outer-LFP links
    /// (LFP_OUTER == 0, so `[lfp]` is the next outer frame), then load the slot.
    /// Mirrors x86 `load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_load_dyn_var(&mut self, src: DynVar) -> bool {
        let lfp = GP::R14.a64().0;
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

    /// dynamic (outer-frame) local <- src. Symmetric to `emit_load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_store_dyn_var(&mut self, dst: DynVar, src: GP) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.reg.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        // walk to the outer LFP in `outer` (avoid clobbering `src`).
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

    // ---- runtime allocation primitives (aarch64) --------------------------
    // Each builds a heap object via a runtime C call. All bail (`false`) on a
    // live xmm (no FP save/restore yet) or an out-of-range frame offset.

    /// rax <- Array built from the `len` slots starting at `src`.
    /// create_array(ptr=&slot[src], len). No xmm save (matches x86).
    pub(in crate::codegen::jitgen) fn emit_create_array(&mut self, src: SlotId, len: usize) -> bool {
        let lfp = GP::R14.a64().0;
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

    /// rax <- Array literal via gen_array(vm, globals, callid, &self).
    pub(in crate::codegen::jitgen) fn emit_new_array(
        &mut self,
        callid: CallSiteId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
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

    /// rax <- Hash literal via gen_hash(vm, globals, &slot[args], len).
    pub(in crate::codegen::jitgen) fn emit_new_hash(
        &mut self,
        args: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
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

    /// rax <- Range via gen_range(start, end, vm, globals, exclude_end).
    pub(in crate::codegen::jitgen) fn emit_new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
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

    /// rax <- the `len` slots at `arg` concatenated into a String via
    /// concatenate_string(vm, globals, &slot[arg], len). Result is Option<Value>
    /// (followed by a HandleError in the IR).
    pub(in crate::codegen::jitgen) fn emit_concat_str(
        &mut self,
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
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

    /// rax <- `src` coerced to an Array: load slot `src`; if already an Array
    /// keep it, otherwise call runtime::to_a(vm, globals, val). Mirrors x86 to_a.
    pub(in crate::codegen::jitgen) fn emit_to_a(&mut self, src: SlotId, using_xmm: UsingXmm) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let toa = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            ldr x0, [x10];          // val (rax)
        );
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
        true
    }

    /// rax <- a deep copy of literal `v` (a fresh mutable object per
    /// evaluation). Mirrors x86 `deepcopy_literal`.
    pub(in crate::codegen::jitgen) fn emit_deep_copy_lit(
        &mut self,
        v: Value,
        using_xmm: UsingXmm,
    ) -> bool {
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

    // ---- floating-point transfer primitives (aarch64) ---------------------
    // FP pool registers map to D-registers via `a64_fpr` (None -> bail). All
    // return bool; `base` is the spill base.

    /// dst(f64) <- src.
    pub(in crate::codegen::jitgen) fn emit_fpr_move(
        &mut self,
        src: FPReg,
        dst: FPReg,
        base: usize,
    ) -> bool {
        let (Some(s), Some(d)) = (self.a64_fpr(src, base), self.a64_fpr(dst, base)) else {
            return false;
        };
        if s != d {
            monoasm_arm64!(&mut self.jit, fmov d(d), d(s););
        }
        true
    }

    /// swap two FP registers (via scratch D0).
    pub(in crate::codegen::jitgen) fn emit_fpr_swap(&mut self, l: FPReg, r: FPReg, base: usize) -> bool {
        let (Some(a), Some(b)) = (self.a64_fpr(l, base), self.a64_fpr(r, base)) else {
            return false;
        };
        if a != b {
            monoasm_arm64!(&mut self.jit,
                fmov d0, d(a);
                fmov d(a), d(b);
                fmov d(b), d0;
            );
        }
        true
    }

    /// dst(f64) <- f64 constant `f`.
    pub(in crate::codegen::jitgen) fn emit_f64_to_fpr(&mut self, f: f64, x: FPReg, base: usize) -> bool {
        let Some(p) = self.a64_fpr(x, base) else {
            return false;
        };
        let bits = f.to_bits();
        monoasm_arm64!(&mut self.jit,
            mov x9, (bits);
            fmov d(p), x9;
        );
        true
    }

    /// dst(f64) <- fixnum in GP `reg` (untag, signed int -> double).
    pub(in crate::codegen::jitgen) fn emit_fixnum_to_fpr(&mut self, reg: GP, x: FPReg, base: usize) -> bool {
        let Some(p) = self.a64_fpr(x, base) else {
            return false;
        };
        let r = reg.a64().0;
        monoasm_arm64!(&mut self.jit,
            asr x9, x(r), #(1);   // untag: value >> 1
            scvtf d(p), x9;
        );
        true
    }

    /// dst(f64) <- Float Value in GP `reg`; deopt if `reg` is not a Float.
    /// Mirrors x86 float_to_f64 / float_val_to_f64 (flonum decode + heap-Float
    /// load). `and`/`ror` have no immediate form / the macro grew `ror`, so the
    /// rotate uses `ror` and the mask uses shifts.
    pub(in crate::codegen::jitgen) fn emit_float_to_fpr(
        &mut self,
        reg: GP,
        x: FPReg,
        deopt: &DestLabel,
        base: usize,
    ) -> bool {
        let Some(p) = self.a64_fpr(x, base) else {
            return false;
        };
        let deopt = deopt.clone();
        let r = reg.a64().0;
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
        true
    }

    /// [slot] <- box(f64 in x): flonum-encode or heap-allocate.
    pub(in crate::codegen::jitgen) fn emit_fpr_to_stack(&mut self, x: FPReg, slot: SlotId, base: usize) -> bool {
        let Some(p) = self.a64_fpr(x, base) else {
            return false;
        };
        let lfp = GP::R14.a64().0;
        let f64_to_val = self.f64_to_val.clone();
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        monoasm_arm64!(&mut self.jit,
            fmov d0, d(p);
            bl f64_to_val;          // x0 = Value(f64)
            sub x10, x(lfp), #(off);
            str x0, [x10];
        );
        true
    }

    /// Save the live FP pool registers (D2..) below sp before a C-call.
    pub(in crate::codegen::jitgen) fn emit_xmm_save(&mut self, using_xmm: UsingXmm, cont: bool) -> bool {
        if using_xmm.not_any() && !cont {
            return true;
        }
        let sp_offset =
            (using_xmm.offset() + if cont { CONTINUATION_FRAME_SIZE } else { 0 }) as u32;
        monoasm_arm64!(&mut self.jit, sub sp, sp, #(sp_offset););
        let mut i = 0u32;
        for (xi, b) in using_xmm.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = 8 * i;
                monoasm_arm64!(&mut self.jit, str d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
        true
    }

    /// Restore the live FP pool registers and pop the save area after a C-call.
    pub(in crate::codegen::jitgen) fn emit_xmm_restore(&mut self, using_xmm: UsingXmm, cont: bool) -> bool {
        if using_xmm.not_any() && !cont {
            return true;
        }
        let sp_offset =
            (using_xmm.offset() + if cont { CONTINUATION_FRAME_SIZE } else { 0 }) as u32;
        let mut i = 0u32;
        for (xi, b) in using_xmm.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = 8 * i;
                monoasm_arm64!(&mut self.jit, ldr d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
        monoasm_arm64!(&mut self.jit, add sp, sp, #(sp_offset););
        true
    }

    /// Integer binary op fast path (guarded; deopts to `deopt`). Independent of
    /// `inline_gen`; emitted when the inline cache says both operands are
    /// Integer. Bails (`false`) on an unsupported `BinOpK`.
    pub(in crate::codegen::jitgen) fn emit_integer_binop(
        &mut self,
        lhs: GP,
        rhs: GP,
        mode: OpMode,
        kind: BinOpK,
        deopt: DestLabel,
    ) -> bool {
        self.a64_integer_binop(lhs, rhs, &mode, kind, &deopt)
    }

    /// Integer comparison; result Value lands in the accumulator.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp(
        &mut self,
        kind: CmpKind,
        mode: OpMode,
        lhs: GP,
        rhs: GP,
    ) -> bool {
        self.a64_cmp_integer(&mode, lhs, rhs);
        self.a64_flag_to_bool(kind);
        true
    }

    /// Fused integer compare + conditional branch to `branch_dest`.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp_br(
        &mut self,
        kind: CmpKind,
        mode: OpMode,
        lhs: GP,
        rhs: GP,
        brkind: BrKind,
        branch_dest: DestLabel,
    ) -> bool {
        self.a64_cmp_integer(&mode, lhs, rhs);
        let cond = a64_cond_for_cmp(kind, brkind);
        self.jit.bcond_label(cond, &branch_dest);
        true
    }

    /// Float (four-arithmetic) binary op: dst <- lhs <op> rhs in D-registers.
    /// Bails (`false`) if an operand is not lowerable or `kind` is unsupported.
    pub(in crate::codegen::jitgen) fn emit_float_binop(
        &mut self,
        kind: BinOpK,
        binary_xmm: (FPReg, FPReg),
        dst: FPReg,
        base: usize,
    ) -> bool {
        let (l, r) = binary_xmm;
        let (Some(ld), Some(rd), Some(dd)) = (
            self.a64_fpr(l, base),
            self.a64_fpr(r, base),
            self.a64_fpr(dst, base),
        ) else {
            return false;
        };
        // aarch64 FP 3-op writes rd, reads rn/rm — no aliasing hazard.
        match kind {
            BinOpK::Add => monoasm_arm64!(&mut self.jit, fadd d(dd), d(ld), d(rd);),
            BinOpK::Sub => monoasm_arm64!(&mut self.jit, fsub d(dd), d(ld), d(rd);),
            BinOpK::Mul => monoasm_arm64!(&mut self.jit, fmul d(dd), d(ld), d(rd);),
            BinOpK::Div => monoasm_arm64!(&mut self.jit, fdiv d(dd), d(ld), d(rd);),
            _ => return false,
        }
        true
    }

    /// Float unary op: negate (flip bit 63) or unary-plus (no-op). The macro
    /// lacks `fneg`, so the sign flip goes through a GPR (`fmov`/`eor`/`fmov`).
    /// Bails (`false`) on a spilled operand or an unsupported `UnOpK`.
    pub(in crate::codegen::jitgen) fn emit_float_unop(&mut self, kind: UnOpK, dst: FPReg, base: usize) -> bool {
        match kind {
            UnOpK::Neg => {
                let Some(p) = self.a64_fpr(dst, base) else {
                    return false;
                };
                monoasm_arm64!(&mut self.jit,
                    fmov x9, d(p);
                    mov x10, (0x8000_0000_0000_0000u64);
                    eor x9, x9, x10;
                    fmov d(p), x9;
                );
            }
            UnOpK::Pos => {}
            _ => return false,
        }
        true
    }

    /// `[lfp - slot*8 - LFP_SELF] <- Value::integer(i)` and `fpr(x) <- i as f64`
    /// (a constant int materialized as both a boxed integer and a double).
    /// Bails (`false`) if the FP destination is not lowerable (spilled).
    pub(in crate::codegen::jitgen) fn emit_i64_to_both(&mut self, i: i64, slot: SlotId, x: FPReg, base: usize) -> bool {
        let Some(p) = self.a64_fpr(x, base) else {
            return false;
        };
        let lfp = GP::R14.a64().0;
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        let id = Value::integer(i).id();
        let bits = (i as f64).to_bits();
        monoasm_arm64!(&mut self.jit,
            mov x9, (id);
            sub x10, x(lfp), #(off);
            str x9, [x10];
            mov x9, (bits);
            fmov d(p), x9;
        );
        true
    }

    /// Float comparison; NaN-correct boolean Value in the accumulator. Mirrors
    /// `a64_flag_to_bool` but on `fcmp` flags with float condition codes. Bails
    /// (`false`) if either operand is spilled.
    pub(in crate::codegen::jitgen) fn emit_float_cmp(&mut self, kind: CmpKind, lhs: FPReg, rhs: FPReg, base: usize) -> bool {
        let (Some(lp), Some(rp)) = (self.a64_fpr(lhs, base), self.a64_fpr(rhs, base)) else {
            return false;
        };
        monoasm_arm64!(&mut self.jit, fcmp d(lp), d(rp););
        let cond = a64_float_cond_for_cmp(kind, BrKind::BrIf);
        let rax = GP::Rax.a64();
        self.jit.cset(rax, cond);
        monoasm_arm64!(&mut self.jit,
            lsl x(rax.0), x(rax.0), #(3u32);
            mov x9, (FALSE_VALUE);
            orr x(rax.0), x(rax.0), x9;
        );
        true
    }

    /// Fused float compare + conditional branch (NaN compares false except
    /// `!=`). Bails (`false`) if either operand is spilled.
    pub(in crate::codegen::jitgen) fn emit_float_cmp_br(
        &mut self,
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
        brkind: BrKind,
        branch_dest: DestLabel,
        base: usize,
    ) -> bool {
        let (Some(lp), Some(rp)) = (self.a64_fpr(lhs, base), self.a64_fpr(rhs, base)) else {
            return false;
        };
        monoasm_arm64!(&mut self.jit, fcmp d(lp), d(rp););
        let cond = a64_float_cond_for_cmp(kind, brkind);
        self.jit.bcond_label(cond, &branch_dest);
        true
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

    /// Dense-integer `case` dispatch — aarch64 twin of x86 `emit_opt_case`. The
    /// condition (a tagged fixnum) is in x4 (`GP::Rdi`), placed by the
    /// front-end. Untag, range-check `[min, max]` (signed, both < 2048 so they
    /// fit a 12-bit `cmp` immediate), then index a jump table of absolute
    /// branch-target addresses by `cond - min` and branch indirectly.
    ///
    /// The table is built with `const_align8` + `abs_address`, exactly as on
    /// x86; `resolve_constants` emits it into this method's own code page right
    /// after the body, so it is well within `adr`'s ±1MB reach. Terminates the
    /// basic block (the `br` is an unconditional indirect branch).
    pub(in crate::codegen::jitgen) fn emit_opt_case(
        &mut self,
        frame: &mut AsmInfo,
        max: u16,
        min: u16,
        else_label: JitLabel,
        branch_labels: Box<[JitLabel]>,
    ) {
        let jump_table = self.jit.const_align8();
        for label in branch_labels.iter() {
            let dest_label = frame.resolve_label(&mut self.jit, *label);
            self.jit.abs_address(dest_label);
        }
        let else_dest = frame.resolve_label(&mut self.jit, else_label);
        let cond = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            asr x(cond), x(cond), #1;   // untag fixnum: x4 = n
            cmp x(cond), #(max as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &else_dest); // n > max -> else
        monoasm_arm64!(&mut self.jit,
            cmp x(cond), #(min as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &else_dest); // n < min -> else
        monoasm_arm64!(&mut self.jit,
            sub x(cond), x(cond), #(min as u32);     // index = n - min
            adr x10, jump_table;                     // table base (PC-relative)
            ldr x10, [x10, x(cond), lsl #3];         // table[n - min]
            br x10;
        );
    }

    /// Record this position (the return continuation of a specialized call) as
    /// the return-address patch point for `evict`. On BOP redefinition,
    /// `Codegen::immediate_eviction` overwrites the instruction here with a
    /// `B deopt` so the stale specialized frame deopts on return. Mirrors x86.
    pub(in crate::codegen::jitgen) fn emit_immediate_evict(&mut self, evict: AsmEvict) {
        // Only specialized calls register a return address on aarch64; normal
        // calls (`emit_call`) ignore `evict` and rely on the callee's own entry
        // guard rather than return-address patching. So an unregistered evict
        // here is a normal call with nothing to patch — skip it. (On x86 every
        // call registers, so the lookup always succeeds there.)
        if let Some(&return_addr) = self.asm_return_addr_table.get(&evict) {
            let patch_point = self.jit.get_current_address();
            self.return_addr_table
                .entry(return_addr)
                .and_modify(|e| e.0 = Some(patch_point));
        }
    }

    /// Register a specialized call's return address so `immediate_eviction` can
    /// find and patch it. Identical to the x86 helper (the tables are arch-
    /// neutral `#[cfg(jit)]` fields on `Codegen`).
    fn set_deopt_with_return_addr(
        &mut self,
        return_addr: CodePtr,
        evict: AsmEvict,
        evict_label: &DestLabel,
    ) {
        self.asm_return_addr_table.insert(evict, return_addr);
        self.return_addr_table
            .insert(return_addr, (None, evict_label.clone()));
    }

    /// Inline-cache class-version guard: deopt if the global class version moved
    /// since compilation. aarch64 has no recompiler, so the x86 recompile params
    /// (`class_version`/`position`/`with_recovery`) are unused here.
    pub(in crate::codegen::jitgen) fn emit_guard_class_version(
        &mut self,
        _class_version: DestLabel,
        _position: Option<BytecodePtr>,
        _with_recovery: bool,
        deopt: DestLabel,
    ) {
        self.a64_guard_class_version(&deopt);
    }

    /// Write the callee frame's meta/outer/block fields before a call.
    pub(in crate::codegen::jitgen) fn emit_setup_method_frame(
        &mut self,
        store: &Store,
        meta: Meta,
        callid: CallSiteId,
        outer_lfp: Option<Lfp>,
    ) {
        self.a64_setup_method_frame(store, meta, callid, outer_lfp);
    }

    /// Marshal the call arguments into the callee frame. Bails (`false`) on a
    /// not-yet-ported argument shape.
    pub(in crate::codegen::jitgen) fn emit_set_arguments(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
    ) -> bool {
        let offset = store[callee_fid].get_offset();
        self.a64_set_arguments(callid, callee_fid, offset)
    }

    /// Basic-operator-redefinition guard: deopt if any BOP has been redefined
    /// since compilation. The deopt PC is the BOP instruction itself, so the
    /// interpreter re-runs it through the de-optimized VM handler.
    pub(in crate::codegen::jitgen) fn emit_check_bop(&mut self, deopt: &DestLabel) {
        let deopt = deopt.clone();
        let flag_addr = self
            .jit
            .get_label_address(&self.bop_redefined_flags)
            .as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (flag_addr);
            ldr w9, [x9];
            cbnz x9, deopt;   // any BOP redefined -> deopt
        );
    }

    /// Recompile-or-deopt point: aarch64 has no recompiler yet, so treat it as a
    /// plain deopt to the interpreter (correct, just not re-optimized). The x86
    /// recompile params are unused here.
    pub(in crate::codegen::jitgen) fn emit_recompile_deopt(
        &mut self,
        _position: Option<BytecodePtr>,
        deopt: &DestLabel,
        _reason: RecompileReason,
    ) {
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit, b deopt;);
    }

    /// The call itself. aarch64 has no runtime branch patching, so the `evict`
    /// return-address patch point is not registered (class-version guards cover
    /// the staleness it would otherwise catch); the x86-only params are unused.
    pub(in crate::codegen::jitgen) fn emit_call(
        &mut self,
        store: &Store,
        callee_fid: FuncId,
        _recv_class: ClassId,
        _evict: AsmEvict,
        _evict_label: &DestLabel,
        pc: BytecodePtr,
    ) {
        self.a64_do_call(store, callee_fid, pc);
    }

    /// Method prologue: establish fp/lr, reserve the local frame, and nil-fill
    /// the non-argument locals/temps. Mirrors x86 `init_func` (rbp == lfp +
    /// RBP_LOCAL_FRAME, so slots are lfp-relative here). Bails (`false`) if the
    /// frame exceeds the 12-bit `sub sp, sp, #imm` immediate.
    pub(in crate::codegen::jitgen) fn emit_init(
        &mut self,
        info: FnInitInfo,
        prologue_offset: PrologueOffset,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let prologue_bytes = prologue_offset.unwrap_concrete();
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

    /// Per-method ivar-cache prep: only needed when heap ivars are accessed;
    /// otherwise a no-op. Bails (`false`) on the heap path for now.
    pub(in crate::codegen::jitgen) fn emit_preparation(&mut self, _store: &Store, frame: &AsmInfo) -> bool {
        !frame.ivar_heap_accessed
    }

    /// Fixnum negate. For a tagged value `t = 2n+1`, `tagged(-n) = 2 - t`, which
    /// overflows i64 exactly when `-n` is out of i63 range (e.g. -i63::MIN), so
    /// deopt on the V flag.
    pub(in crate::codegen::jitgen) fn emit_fixnum_neg(&mut self, reg: GP, deopt: &DestLabel) {
        let r = reg.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x9, (2u64);
            subs x(r), x9, x(r);   // 2 - t  == tagged(-n)
        );
        self.jit.bcond_label(monoasm::Cond::Vs, deopt);
    }

    /// Fixnum bitwise-not. For a tagged value `t = 2n+1`, `tagged(~n) = -t`
    /// (since ~n = -n-1), which is always a valid tagged fixnum (no overflow).
    pub(in crate::codegen::jitgen) fn emit_fixnum_bit_not(&mut self, reg: GP) {
        let r = reg.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x9, (0u64);
            sub x(r), x9, x(r);    // -t  == tagged(~n)
        );
    }

    /// Guard that `reg` is an Array RValue: deopt if it is an immediate (low 3
    /// bits set) or its `ty` byte is not `ObjTy::ARRAY`.
    pub(in crate::codegen::jitgen) fn emit_guard_array_ty(&mut self, reg: GP, deopt: &DestLabel) {
        let r = reg.a64().0;
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            mov x9, (0b111);
            and x9, x(r), x9;
            cbnz x9, deopt;                              // immediate -> deopt
            ldrb w9, [x(r), #(RVALUE_OFFSET_TY as u32)]; // RValue.ty (u8)
            cmp x9, #(ObjTy::ARRAY.get() as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
    }

    /// Guard that the receiver in rdi (x-reg) is not frozen: deopt if the frozen
    /// flag bit (0b10) is set.
    pub(in crate::codegen::jitgen) fn emit_guard_frozen(&mut self, deopt: &DestLabel) {
        let rdi = GP::Rdi.a64().0;
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            ldrb w9, [x(rdi), #(RVALUE_OFFSET_FLAG as u32)];
            mov x10, (0b10);
            and x9, x9, x10;       // isolate the frozen bit
            cbnz x9, deopt;        // frozen -> deopt
        );
    }

    /// `ldr`/`str` use a 12-bit scaled (×8) immediate offset; bail above that.
    fn a64_field_off_ok(off: u32) -> bool {
        off <= 32760 && off % 8 == 0
    }

    /// Load an inline (object-embedded) instance variable into the accumulator
    /// (x23), substituting nil for an unset (zero) slot. Bails if the field
    /// offset is out of the load immediate's range.
    pub(in crate::codegen::jitgen) fn emit_load_ivar_inline(&mut self, ivarid: IvarId) -> bool {
        let off = RVALUE_OFFSET_KIND as u32 + ivarid.get() as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let r15 = GP::R15.a64().0;
        monoasm_arm64!(&mut self.jit,
            ldr x(r15), [x(rdi), #(off)];
            mov x9, (NIL_VALUE);
            cmp x(r15), #(0u32);
            csel x(r15), x(r15), x9, ne;   // unset slot (0) -> nil
        );
        true
    }

    /// Store the accumulator-side `src` into an inline instance-variable slot.
    pub(in crate::codegen::jitgen) fn emit_store_ivar_inline(&mut self, src: GP, ivarid: IvarId) -> bool {
        let off = RVALUE_OFFSET_KIND as u32 + ivarid.get() as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let s = src.a64().0;
        monoasm_arm64!(&mut self.jit, str x(s), [x(rdi), #(off)];);
        true
    }

    /// Load an inline Struct member slot into the accumulator (x23).
    pub(in crate::codegen::jitgen) fn emit_load_struct_slot_inline(&mut self, slot_index: u16) -> bool {
        let off = RVALUE_OFFSET_INLINE as u32 + slot_index as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let r15 = GP::R15.a64().0;
        monoasm_arm64!(&mut self.jit, ldr x(r15), [x(rdi), #(off)];);
        true
    }

    /// Store `src` into an inline Struct member slot (also returned in rax/x0).
    pub(in crate::codegen::jitgen) fn emit_store_struct_slot_inline(&mut self, src: GP, slot_index: u16) -> bool {
        let off = RVALUE_OFFSET_INLINE as u32 + slot_index as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let s = src.a64().0;
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            str x(s), [x(rdi), #(off)];
            mov x(rax), x(s);
        );
        true
    }

    /// Load a heap-spilled Struct member slot: deref the heap pointer (into rdi)
    /// then load the slot into the accumulator (x23).
    pub(in crate::codegen::jitgen) fn emit_load_struct_slot_heap(&mut self, slot_index: u16) -> bool {
        let off = slot_index as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let r15 = GP::R15.a64().0;
        monoasm_arm64!(&mut self.jit,
            ldr x(rdi), [x(rdi), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            ldr x(r15), [x(rdi), #(off)];
        );
        true
    }

    /// Store `src` into a heap-spilled Struct member slot (also returned in
    /// rax/x0). Derefs the heap pointer first (clobbering rdi, like x86).
    pub(in crate::codegen::jitgen) fn emit_store_struct_slot_heap(&mut self, src: GP, slot_index: u16) -> bool {
        let off = slot_index as u32 * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let s = src.a64().0;
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            ldr x(rdi), [x(rdi), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            str x(s), [x(rdi), #(off)];
            mov x(rax), x(s);
        );
        true
    }

    /// reg += i (no-op when i == 0). `i` is materialized so any i32 works.
    pub(in crate::codegen::jitgen) fn emit_reg_add(&mut self, reg: GP, i: i32) {
        if i != 0 {
            let d = reg.a64().0;
            if reg == GP::Rsp {
                // Register 31 decodes as XZR (not SP) in the add/sub
                // shifted-register form, so `add x31, x31, x9` would be a no-op.
                // Go through a GP temp: mov to/from SP is the sp-aware form.
                monoasm_arm64!(&mut self.jit,
                    mov x9, (i as i64 as u64);
                    mov x10, sp;
                    add x10, x10, x9;
                    mov sp, x10;
                );
            } else {
                monoasm_arm64!(&mut self.jit,
                    mov x9, (i as i64 as u64);
                    add x(d), x(d), x9;
                );
            }
        }
    }

    /// reg -= i (no-op when i == 0).
    pub(in crate::codegen::jitgen) fn emit_reg_sub(&mut self, reg: GP, i: i32) {
        if i != 0 {
            let d = reg.a64().0;
            if reg == GP::Rsp {
                // See emit_reg_add: SP must be updated via a GP temp (reg 31 is
                // XZR in the shifted-register form).
                monoasm_arm64!(&mut self.jit,
                    mov x9, (i as i64 as u64);
                    mov x10, sp;
                    sub x10, x10, x9;
                    mov sp, x10;
                );
            } else {
                monoasm_arm64!(&mut self.jit,
                    mov x9, (i as i64 as u64);
                    sub x(d), x(d), x9;
                );
            }
        }
    }

    /// Loop-JIT entry stack bump. Bails if the byte count exceeds the 12-bit
    /// `sub sp, sp, #imm` immediate.
    pub(in crate::codegen::jitgen) fn emit_loop_jit_rsp_bump(&mut self, offset: LoopRspOffset) -> bool {
        let bytes = offset.unwrap_concrete();
        if bytes == 0 {
            return true;
        }
        if bytes > 4095 {
            return false;
        }
        monoasm_arm64!(&mut self.jit, sub sp, sp, #(bytes as u32););
        true
    }

    /// Store `src` into a heap-spilled instance variable of self: deref the
    /// var-table and its data pointer (via scratch x9), then store. No bounds
    /// check (the table is pre-sized). Bails on an out-of-range field offset.
    pub(in crate::codegen::jitgen) fn emit_store_self_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
    ) -> bool {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let off = idx * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let s = src.a64().0;
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];
            ldr x9, [x9, #(MONOVEC_PTR as u32)];
            str x(s), [x9, #(off)];
        );
        true
    }

    /// Store `src` into a heap-spilled instance variable of the object in rdi
    /// (x4) — the non-self twin of emit_store_self_ivar_heap. The var-table may
    /// be too small (None / capa 0 / len <= idx), so the fast inline store is
    /// guarded by a bounds check that falls through to a cold
    /// `set_ivar(obj, ivarid, src)` runtime call (which grows the table). The
    /// live FP pool is saved around that call. aarch64 lays the cold path inline
    /// (no separate page). Bails on an out-of-range field offset.
    pub(in crate::codegen::jitgen) fn emit_store_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using_xmm: UsingXmm,
    ) -> bool {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let off = idx * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0; // object (&RValue)
        let s = src.a64().0;
        let generic = self.jit.label();
        let exit = self.jit.label();
        // var_table bounds check (None / capa 0 / len <= idx -> grow via runtime).
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];
            cbz x9, generic;
            ldr x10, [x9, #(MONOVEC_CAPA as u32)];
            cbz x10, generic;
            ldr x10, [x9, #(MONOVEC_LEN as u32)];
            cmp x10, #(idx);
        );
        self.jit.bcond_label(monoasm::Cond::Le, &generic); // len <= idx -> grow
        // fast path: write straight into the table slot.
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x9, #(MONOVEC_PTR as u32)];
            str x(s), [x9, #(off)];
            b exit;
        );
        // cold path: set_ivar(obj, ivarid, src), preserving the FP pool. src (s)
        // and rdi survive emit_xmm_save (it only touches d-regs / sp) and are
        // read into the C-arg regs just before the call.
        let f = set_ivar as *const () as u64;
        monoasm_arm64!(&mut self.jit, generic:);
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);            // base: &mut RValue
            mov x1, (ivar as u64);     // id: IvarId
            mov x2, x(s);              // val
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit, exit:);
        true
    }

    /// Load a heap-spilled instance variable into the accumulator (x23). Unless
    /// loading from self, bounds-check the var-table (None / capa 0 / len <= idx
    /// -> nil); an unset (zero) slot also reads nil. Bails on an out-of-range
    /// field offset. `x9` is the scratch for the table/data pointer chain.
    pub(in crate::codegen::jitgen) fn emit_load_ivar_heap(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
    ) -> bool {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let off = idx * 8;
        if !Self::a64_field_off_ok(off) {
            return false;
        }
        let rdi = GP::Rdi.a64().0;
        let r15 = GP::R15.a64().0;
        let nil = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];   // var_table
        );
        if !self_ {
            monoasm_arm64!(&mut self.jit,
                cbz x9, nil;                                 // None -> nil
                ldr x10, [x9, #(MONOVEC_CAPA as u32)];
                cbz x10, nil;                                // capa 0 -> nil
                ldr x10, [x9, #(MONOVEC_LEN as u32)];
                cmp x10, #(idx);
            );
            self.jit.bcond_label(monoasm::Cond::Le, &nil);   // len <= idx -> nil
        }
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x9, #(MONOVEC_PTR as u32)];             // data ptr
            ldr x(r15), [x9, #(off)];                        // value
            cbnz x(r15), exit;                               // set -> exit
        nil:
            mov x(r15), (NIL_VALUE);
        exit:
        );
        true
    }

    /// `undef`-method via runtime::undef_method(vm=x19, globals=x20, id). Bails
    /// when an xmm pool register is live (no aarch64 xmm save around C calls
    /// yet); lr is preserved across the `blr`.
    pub(in crate::codegen::jitgen) fn emit_undef_method(&mut self, undef: IdentId, using_xmm: UsingXmm) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let f = runtime::undef_method as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                   // vm (Executor)
            mov x1, x20;                   // globals
            mov x2, (undef.get() as u64);  // undef (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// Alias a global var via runtime::alias_global_var(globals=x20, new, old).
    /// Bails when an xmm pool register is live.
    pub(in crate::codegen::jitgen) fn emit_alias_gvar(&mut self, new: IdentId, old: IdentId, using_xmm: UsingXmm) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let f = runtime::alias_global_var as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x20;                 // globals
            mov x1, (new.get() as u64);  // new IdentId
            mov x2, (old.get() as u64);  // old IdentId
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// Check class variable existence via runtime::check_class_var(vm, globals,
    /// name); the looked-up Value lands in x0. Bails when an xmm pool register
    /// is live (no xmm save around the C call yet).
    pub(in crate::codegen::jitgen) fn emit_check_cvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let f = runtime::check_class_var as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// @@cvar <- src via runtime::set_class_var(vm, globals, name, val). The
    /// Option<Value> result (None == error) is checked by a following
    /// HandleError. Bails when an xmm pool register is live or the slot offset
    /// exceeds the 12-bit scaled load immediate.
    pub(in crate::codegen::jitgen) fn emit_store_cvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::set_class_var as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            sub x10, x(lfp), #(off);
            ldr x3, [x10];               // val (from slot)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
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
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off_old = old.0 as u32 * 8 + LFP_SELF as u32;
        let off_new = new.0 as u32 * 8 + LFP_SELF as u32;
        if off_old > 4095 || off_new > 4095 {
            return false;
        }
        let f = runtime::alias_method as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm (Executor)
            mov x1, x20;                 // globals
            sub x10, x(lfp), #(off_old);
            ldr x2, [x10];              // old (slot value)
            sub x10, x(lfp), #(off_new);
            ldr x3, [x10];             // new (slot value)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

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
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_yield as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // result in x0
            ldr x30, [sp], #16;
            sub x10, x(lfp), #(off);
            str x0, [x10];               // -> dst
        );
        true
    }

    /// "super" if super is callable, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_super(
        &mut self,
        dst: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_super as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            sub x10, x(lfp), #(off);
            str x0, [x10];
        );
        true
    }

    /// "global-variable" if $name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_gvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_gvar as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            mov x2, (name.get() as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            sub x10, x(lfp), #(off);
            str x0, [x10];
        );
        true
    }

    /// "class variable" if @@name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_cvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_cvar as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            mov x2, (name.get() as u64);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            sub x10, x(lfp), #(off);
            str x0, [x10];
        );
        true
    }

    /// defined?(Const): runtime writes the result through the `dst` out-pointer.
    pub(in crate::codegen::jitgen) fn emit_defined_const(
        &mut self,
        dst: SlotId,
        siteid: ConstSiteId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_const as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            sub x2, x(lfp), #(off);      // &dst (out-pointer)
            mov x3, (siteid.0 as u64);   // ConstSiteId
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// defined?(recv.name): runtime writes the result through `dst`.
    pub(in crate::codegen::jitgen) fn emit_defined_method(
        &mut self,
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off_dst = dst.0 as u32 * 8 + LFP_SELF as u32;
        let off_recv = recv.0 as u32 * 8 + LFP_SELF as u32;
        if off_dst > 4095 || off_recv > 4095 {
            return false;
        }
        let f = runtime::defined_method as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            sub x2, x(lfp), #(off_dst);  // &dst (out-pointer)
            sub x10, x(lfp), #(off_recv);
            ldr x3, [x10];               // recv (slot value)
            mov x4, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// defined?(@name): runtime writes the result through `dst`.
    pub(in crate::codegen::jitgen) fn emit_defined_ivar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::defined_ivar as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            sub x2, x(lfp), #(off);      // &dst (out-pointer)
            mov x3, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// Generic binary-op C-call (no receiver-class guard), mirroring the VM's
    /// call_binop convention: x0=vm, x1=globals, x2=lhs, x3=rhs; Option<Value>
    /// result in x0. Bails on a live xmm pool reg or an out-of-range offset.
    pub(in crate::codegen::jitgen) fn emit_generic_binop(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        if off_l > 4095 || off_r > 4095 {
            return false;
        }
        let f = func as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            sub x10, x(lfp), #(off_l);
            ldr x2, [x10];               // lhs
            sub x10, x(lfp), #(off_r);
            ldr x3, [x10];               // rhs
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// `lhs === rhs` for an Array lhs via runtime::array_teq (x0=vm, x1=globals,
    /// x2=lhs, x3=rhs); Option<Value> result in x0. Bails as above.
    pub(in crate::codegen::jitgen) fn emit_array_teq(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0;
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        if off_l > 4095 || off_r > 4095 {
            return false;
        }
        let f = runtime::array_teq as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            sub x10, x(lfp), #(off_l);
            ldr x2, [x10];               // lhs
            sub x10, x(lfp), #(off_r);
            ldr x3, [x10];               // rhs
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
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
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off = arg.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::concatenate_regexp as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            sub x2, x(lfp), #(off);      // &arg (slot address)
            mov x3, (len as u64);        // len
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// Keyword-rest fixup: if the `slot` is nil, replace it with a fresh empty
    /// Hash (runtime::empty_hash, no args, result in x0). Mirrors the x86 inline
    /// path (no xmm save — no xmm is live at kw-rest setup). Bails on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_check_kw_rest(&mut self, slot: SlotId) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let exit = self.jit.label();
        let f = runtime::empty_hash as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            ldr x11, [x10];
            cmp x11, #(NIL_VALUE);       // slot == nil ?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &exit); // not nil -> keep
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // x0 = {}
            ldr x30, [sp], #16;
            sub x10, x(lfp), #(off);     // x10 clobbered by the call; recompute
            str x0, [x10];               // slot = {}
        exit:
        );
        true
    }

    /// Multiple-assignment array expansion via runtime::expand_array(src, dst,
    /// len, rest). `src` is already in GP::Rdi (x4) from the preceding load;
    /// `dst` is the (descending) destination base x22-conv(dst). aarch64 C-args:
    /// x0=src, x1=&dst, x2=len, x3=rest (rest = rest_pos+1, or 0 for none).
    /// Bails on a live xmm pool reg or an out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_expand_array(
        &mut self,
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let rest = if let Some(rest_pos) = rest_pos {
            rest_pos as u64 + 1
        } else {
            0
        };
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let rdi = GP::Rdi.a64().0; // x4 holds src
        let f = runtime::expand_array as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);              // src (from GP::Rdi)
            sub x1, x(lfp), #(off);      // &dst (descending base)
            mov x2, (len as u64);        // len
            mov x3, (rest);              // rest (0 = none)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        true
    }

    /// `==` / `!=` with an inline immediate fast path (mirrors the x86
    /// `opt_eq_cmp`). If BOTH operands are non-heap, non-flonum immediates the
    /// Ruby result is exact bit (identity) equality, produced inline via
    /// cmp + cset; otherwise fall through to the generic C-call `func`
    /// (x0=vm, x1=globals, x2=lhs, x3=rhs). `lhs`/`rhs` are loaded into x2/x3
    /// up front so the slow path can reuse them. Bails on a live xmm pool reg
    /// or an out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        if off_l > 4095 || off_r > 4095 {
            return false;
        }
        let f = func as u64;
        let slow = self.jit.label();
        let done = self.jit.label();
        // Load operands into the C-arg registers (reused by the slow path).
        // Heap iff (bits & 0b111) == 0; Flonum iff (bits & 0b011) == 0b010.
        // Either operand heap/flonum -> generic C-call.
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off_l);
            ldr x2, [x10];               // lhs
            sub x10, x(lfp), #(off_r);
            ldr x3, [x10];               // rhs
            mov x14, (7u64);
            and x9, x2, x14;
            cbz x9, slow;                // lhs heap -> slow
            mov x14, (3u64);
            and x9, x2, x14;
            cmp x9, #(2u32);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &slow); // lhs flonum -> slow
        monoasm_arm64!(&mut self.jit,
            mov x14, (7u64);
            and x9, x3, x14;
            cbz x9, slow;                // rhs heap -> slow
            mov x14, (3u64);
            and x9, x3, x14;
            cmp x9, #(2u32);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &slow); // rhs flonum -> slow
        // Fast path: both identity-comparable immediates -> bit equality.
        monoasm_arm64!(&mut self.jit,
            cmp x2, x3;
        );
        self.a64_flag_to_bool(kind); // x0 = bool Value
        monoasm_arm64!(&mut self.jit,
            b done;
        slow:
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals (x2=lhs, x3=rhs intact)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        done:
        );
        true
    }

    /// Unary float C helper `f(f64) -> f64` (sin, sqrt, …): load the operand
    /// into d0 (the first/only AAPCS f64 arg = return reg), call, store d0 into
    /// dst. The live FP pool (d2-d7, caller-saved = clobbered by the callee) is
    /// saved/restored around the call exactly like the x86 twin. Bails if the
    /// source or destination is a spill slot (a64_fpr only handles d2-d7).
    pub(in crate::codegen::jitgen) fn emit_cfunc_f_f(
        &mut self,
        f: unsafe extern "C" fn(f64) -> f64,
        src: FPReg,
        dst: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) -> bool {
        let Some(s) = self.a64_fpr(src, base) else {
            return false;
        };
        let Some(d) = self.a64_fpr(dst, base) else {
            return false;
        };
        let fp = f as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            fmov d0, d(s);
            mov x9, (fp);
            blr x9;            // result in d0
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            ldr x30, [sp], #16;
            fmov d(d), d0;
        );
        true
    }

    /// Binary float C helper `f(f64, f64) -> f64` (atan2, hypot, …): load lhs
    /// into d0 and rhs into d1 (the first two AAPCS f64 args), call, store the
    /// d0 result into dst. Pool sources resolve to d2-d7 so they never alias the
    /// d0/d1 scratch regs. Saves/restores the live FP pool like the x86 twin.
    /// Bails if any operand or the destination is a spill slot.
    pub(in crate::codegen::jitgen) fn emit_cfunc_ff_f(
        &mut self,
        f: extern "C" fn(f64, f64) -> f64,
        lhs: FPReg,
        rhs: FPReg,
        dst: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) -> bool {
        let Some(l) = self.a64_fpr(lhs, base) else {
            return false;
        };
        let Some(r) = self.a64_fpr(rhs, base) else {
            return false;
        };
        let Some(d) = self.a64_fpr(dst, base) else {
            return false;
        };
        let fp = f as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            fmov d0, d(l);
            fmov d1, d(r);
            mov x9, (fp);
            blr x9;            // result in d0
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            ldr x30, [sp], #16;
            fmov d(d), d0;
        );
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
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let f = runtime::define_method as *const () as u64;
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
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off = obj.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let f = runtime::singleton_define_method as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                     // vm (Executor)
            mov x1, x20;                     // globals
            mov x2, (name.get() as u64);     // name (IdentId)
            mov x3, (func_id.get() as u64);  // func_id (FuncId)
            sub x10, x(lfp), #(off);
            ldr x4, [x10];                   // obj (receiver Value)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                          // x0 = Option<Value>
            ldr x30, [sp], #16;
        );
        self.emit_handle_error(error);
        true
    }

    // ---- exception / non-local control flow -------------------------------
    // All four branch into `entry_raise` (the shared unwind/dispatch entry,
    // bound by a64_gen_entry_raise). None carry a `using_xmm` set — an
    // in-flight exception abandons the FP pool. C-arg regs: x0=vm (x19).

    /// `raise` — runtime::raise_err(vm, err_val) then unwind. The value to
    /// raise is in the accumulator scratch (GP::Rax = x0), so it is moved into
    /// x1 *before* x0 is overwritten with the executor.
    pub(in crate::codegen::jitgen) fn emit_raise(&mut self) -> bool {
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
            b raise;
        );
        true
    }

    /// `retry` — set PC (x21) to `pc + 1`, call runtime::err_retry(vm), unwind.
    pub(in crate::codegen::jitgen) fn emit_retry(&mut self, pc: BytecodePtr) -> bool {
        let raise = self.entry_raise();
        let pcv = (pc + 1).as_ptr() as u64;
        let f = runtime::err_retry as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x21, (pcv);          // PC <- pc + 1
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            b raise;
        );
        true
    }

    /// `redo` — like `retry` but runtime::err_redo(vm).
    pub(in crate::codegen::jitgen) fn emit_redo(&mut self, pc: BytecodePtr) -> bool {
        let raise = self.entry_raise();
        let pcv = (pc + 1).as_ptr() as u64;
        let f = runtime::err_redo as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x21, (pcv);          // PC <- pc + 1
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            b raise;
        );
        true
    }

    /// End of an `ensure` clause — runtime::ensure_end(vm) returns a nonzero
    /// value when a pending exception must keep propagating (→ entry_raise);
    /// zero means fall through to the normal continuation.
    pub(in crate::codegen::jitgen) fn emit_ensure_end(&mut self) -> bool {
        let raise = self.entry_raise();
        let cont = self.jit.label();
        let f = runtime::ensure_end as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;             // vm
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                  // x0 = 0 (continue) / nonzero (re-raise)
            ldr x30, [sp], #16;
            cbz x0, cont;
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

    /// Lower the generic `Yield` (block call whose target is resolved at runtime
    /// via `get_yield_data`). Mirrors x86 `gen_yield`: fetch the block's
    /// ProcData, build the callee block frame, massage arguments, then call the
    /// block's funcdata indirectly. The eviction-on-return patching is x86-only
    /// (runtime branch patching), so it is skipped — class-version guards cover
    /// it. `error` catches a missing block, an argument error, or a callee
    /// raise. Bails on an out-of-range callee-frame offset.
    pub(in crate::codegen::jitgen) fn emit_yield(
        &mut self,
        callid: CallSiteId,
        error: &DestLabel,
        _evict: AsmEvict,
        _evict_label: &DestLabel,
    ) -> bool {
        // Closely mirrors the proven VM `a64_op_yield`. x25/x26 are callee-saved
        // and used by neither the JIT global set (x19-x23) nor JIT'd code, so
        // they survive the C calls and hold the outer LFP / funcdata. The
        // continuation frame is already reserved by the surrounding
        // xmm_save_cont, so no extra push here.
        let f_yield = runtime::get_yield_data as *const () as u64;
        let f_args = runtime::jit_handle_arguments_no_block as *const () as u64;
        // get_yield_data(vm, globals) -> x0 = outer Lfp, x1 = FuncId.
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f_yield);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_resolve_invalidated_outer(0);
        self.emit_handle_error(error); // null outer (no block given) -> error
        monoasm_arm64!(&mut self.jit, mov x25, x0;); // outer (callee-saved)
        // get_func_data: FuncId (x1) -> &FuncData (x9 -> x26).
        monoasm_arm64!(&mut self.jit, mov x2, x1;);
        self.a64_get_func_data_x2(); // x9 = &FuncData (clobbers x10, x11)
        monoasm_arm64!(&mut self.jit, mov x26, x9;);
        // Build the callee block frame fields below sp (outer/svar/cme/block/
        // self/meta). self is inherited from the outer frame.
        monoasm_arm64!(&mut self.jit,
            mov x12, (0u64);
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
            str x25, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_CME) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
            str x12, [x11];
            sub x10, x25, #(LFP_SELF as u32);
            ldr x10, [x10];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            str x10, [x11];
            ldr x10, [x26, #(FUNCDATA_META as u32)];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_META) as u32);
            str x10, [x11];
        );
        // jit_handle_arguments_no_block(vm, globals, caller_lfp, callee_lfp,
        // callid). callee_lfp is computed before the dynamic callee-scratch
        // reservation; the pre-reservation sp is saved in x25 and restored
        // afterwards (x26 survives as fdata).
        monoasm_arm64!(&mut self.jit,
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);   // callee_lfp
            mov x25, sp;                             // save sp (outer no longer needed)
            ldrh w10, [x26, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);                     // 16-aligned reservation
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x19;
            mov x1, x20;
            mov x2, x22;                             // caller LFP
            mov x4, (callid.get() as u64);
            mov x9, (f_args);
            blr x9;                                  // x0 = Option<Value>
            mov sp, x25;                             // restore sp
        );
        self.emit_handle_error(error); // argument error -> error
        // call_funcdata (indirect, fdata in x26): push the control frame, set
        // the callee LFP/PC, blr the codeptr, then restore the caller frame
        // (cfp from the saved prev slot, lfp from x29 == the JIT frame pointer).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x19, #(EXECUTOR_CFP as u32)];
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x22, [x10];
            sub x3, x21, #(16u32);                       // x3 = pc arg (call-site bc ptr) for with-pc callees
            ldr x21, [x26, #(FUNCDATA_PC as u32)];        // PC <- callee pc
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;                                       // result in x0
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
        true
    }

    /// `dst <- sp + (ofs - RSP_LOCAL_FRAME)` (the absolute callee-slot address).
    /// The displacement is usually negative (the callee frame sits below sp).
    /// Returns `false` if it exceeds the 12-bit add/sub immediate.
    fn a64_rsp_slot_addr(&mut self, ofs: i32, dst: u32) -> bool {
        let signed = ofs - RSP_LOCAL_FRAME;
        if signed >= 0 {
            if signed > 4095 {
                return false;
            }
            monoasm_arm64!(&mut self.jit, add x(dst), sp, #(signed as u32););
        } else {
            let n = (-signed) as u32;
            if n > 4095 {
                return false;
            }
            monoasm_arm64!(&mut self.jit, sub x(dst), sp, #(n););
        }
        true
    }

    // ---- callee-frame argument stores ([sp + (ofs - RSP_LOCAL_FRAME)]) ------
    // Used by the inline argument-setup fast path (fetch_for_callee). Bail on an
    // out-of-range slot offset.

    /// `[sp + (ofs - RSP_LOCAL_FRAME)] <- reg`
    pub(in crate::codegen::jitgen) fn emit_reg_to_rsp_offset(&mut self, r: GP, ofs: i32) -> bool {
        if !self.a64_rsp_slot_addr(ofs, 10) {
            return false;
        }
        let r = r.a64().0;
        monoasm_arm64!(&mut self.jit, str x(r), [x10];);
        true
    }

    /// `[sp + (ofs - RSP_LOCAL_FRAME)] <- 0`
    pub(in crate::codegen::jitgen) fn emit_zero_to_rsp_offset(&mut self, ofs: i32) -> bool {
        if !self.a64_rsp_slot_addr(ofs, 10) {
            return false;
        }
        monoasm_arm64!(&mut self.jit,
            mov x9, (0u64);
            str x9, [x10];
        );
        true
    }

    /// `[sp + (ofs - RSP_LOCAL_FRAME)] <- imm`
    pub(in crate::codegen::jitgen) fn emit_u64_to_rsp_offset(&mut self, i: u64, ofs: i32) -> bool {
        if !self.a64_rsp_slot_addr(ofs, 10) {
            return false;
        }
        monoasm_arm64!(&mut self.jit,
            mov x9, (i);
            str x9, [x10];
        );
        true
    }

    /// `&block` proxy: materialize the current method's block handler into
    /// `ret`. Walk `outer` outer-frame links to reach the method LFP (x0), load
    /// its block slot ([lfp - LFP_BLOCK]); if the low bit is set (already a
    /// BlockHandler proxy rather than a frame pointer) bump the nesting tag by
    /// `(outer << 2) + 2`. No runtime call, no xmm pressure. Bails on an
    /// out-of-range frame offset or nesting tag immediate.
    pub(in crate::codegen::jitgen) fn emit_block_arg_proxy(
        &mut self,
        ret: SlotId,
        outer: usize,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let rax = GP::Rax.a64().0; // x0
        let off = ret.0 as u32 * 8 + LFP_SELF as u32;
        let tag = ((outer << 2) + 2) as u32;
        if off > 4095 || tag > 4095 {
            return false;
        }
        // get_method_lfp(outer): x0 <- method LFP (walk `outer` outer links).
        if outer == 0 {
            monoasm_arm64!(&mut self.jit, mov x(rax), x(lfp););
        } else {
            monoasm_arm64!(&mut self.jit, ldr x(rax), [x(lfp)];);
            for _ in 0..outer - 1 {
                monoasm_arm64!(&mut self.jit, ldr x(rax), [x(rax)];);
            }
        }
        // block_arg_proxy(outer): x0 <- [x0 - LFP_BLOCK]; if (x0 & 1) bump tag.
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            sub x10, x(rax), #(LFP_BLOCK as u32);
            ldr x(rax), [x10];
            mov x11, (1u64);
            tst x(rax), x11;             // Z = ((x0 & 1) == 0)
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            add x(rax), x(rax), #(tag);
            exit:
        );
        // store_rax(ret): [lfp - off] <- x0
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            str x(rax), [x10];
        );
        true
    }

    /// `&block` captured as a Proc value: runtime::block_arg(vm, globals, lfp,
    /// call_site) materializes the current frame's block handler into a Proc
    /// (promoting the frame to the heap if needed). The Option<Value> result is
    /// stored to `ret` after a HandleError. Bails on a live xmm pool reg (no
    /// save around the C call) or an out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_block_arg(
        &mut self,
        ret: SlotId,
        using_xmm: UsingXmm,
        call_site_bc_ptr: BytecodePtr,
        error: &DestLabel,
    ) -> bool {
        if using_xmm.iter().any(|b| *b) {
            return false;
        }
        let lfp = GP::R14.a64().0; // x22
        let off = ret.0 as u32 * 8 + LFP_SELF as u32;
        if off > 4095 {
            return false;
        }
        let cs = call_site_bc_ptr.as_ptr() as u64;
        let f = runtime::block_arg as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;          // vm
            mov x1, x20;          // globals
            mov x2, x(lfp);       // caller LFP
            mov x3, (cs);         // call-site bc ptr
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;               // x0 = Option<Value>
            ldr x30, [sp], #16;
        );
        self.emit_handle_error(error);
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            str x(rax), [x10];    // ret <- Proc
        );
        true
    }

    /// Side-effect guard for a method that passes a block: deopt if the current
    /// frame has been captured/promoted (so a materialized closure observes
    /// later local writes). Tests the captured (0b1000_0000) / invalidated
    /// (0b0000_1000) bits of the Meta `kind` byte at `[lfp - 1]`. Mirrors x86
    /// `branch_if_captured` + `guard_capture`; aarch64 lays the deopt inline (no
    /// cold page) so this is just a conditional branch to the deopt label.
    pub(in crate::codegen::jitgen) fn emit_guard_capture(&mut self, deopt: &DestLabel) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = (LFP_META as i64 - META_KIND as i64) as u32; // == 1 (kind byte)
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            sub x10, x(lfp), #(off);
            ldrb w9, [x10];
            mov x11, (0b1000_1000u64);
            tst x9, x11;                 // captured or invalidated?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // set -> deopt to VM
        true
    }

    /// Per-arch (aarch64) lowering for every `AsmInst` not handled by the
    /// arch-neutral `compile_asmir` dispatcher. Returns `false` for any
    /// not-yet-ported variant (the method then stays VM-interpreted).
    pub(in crate::codegen::jitgen) fn compile_asmir_arch(
        &mut self,
        _store: &Store,
        frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        _class_version: DestLabel,
    ) -> bool {
        // The specialized (inlined-frame) AsmInst family is lowered here; every
        // other variant is handled by the shared `compile_asmir` dispatcher.
        // Anything still reaching the wildcard is not yet ported, so bail and
        // keep the method VM-interpreted.
        match inst {
            // Clean return / block-break out of an inlined frame.
            AsmInst::MethodRetSpecialized { rbp_offset }
            | AsmInst::BlockBreakSpecialized { rbp_offset } => {
                self.a64_method_ret_specialized(rbp_offset.unwrap_concrete());
            }
            // Outer-scope local access at a pre-resolved frame offset.
            AsmInst::LoadDynVarSpecialized { offset, reg } => {
                self.a64_load_dyn_var_specialized(offset.unwrap_concrete(), reg);
            }
            AsmInst::StoreDynVarSpecialized { offset, dst, src } => {
                self.a64_store_dyn_var_specialized(offset.unwrap_concrete(), dst, src);
            }
            // Inline-cache class-version guard. aarch64 has no recompiler, so
            // both the specialized guard and the recompile-or-deopt point
            // degrade to a plain deopt (sound — the global class-version
            // compare conservatively deopts on any class change since compile).
            AsmInst::GuardClassVersionSpecialized { idx: _, deopt } => {
                self.a64_guard_class_version(&labels[deopt]);
            }
            AsmInst::RecompileDeoptSpecialized {
                idx: _,
                deopt,
                reason: _,
            } => {
                self.emit_deopt(&labels[deopt]);
            }
            // Direct call into an inlined method entry, with the return-address
            // patch point recorded for BOP-redefinition eviction.
            AsmInst::SpecializedCall {
                entry,
                patch_point,
                evict,
            } => {
                let patch_point = patch_point.map(|l| frame.resolve_label(&mut self.jit, l));
                let entry_label = frame.resolve_label(&mut self.jit, entry);
                let return_addr = self.a64_do_specialized_call(entry_label, patch_point);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            // SpecializedYield + SetupYieldFrame (block/yield inlining) are not
            // ported yet: bail so the method stays VM-interpreted (sound).
            _ => return false,
        }
        true
    }
}

/// `set_ivar(base, id, val)` runtime helper for the StoreIVarHeap cold path
/// (grows the var-table as needed). The x86 twin lives in
/// `compile/variables.rs`; that module is `jit_x86`-only, so aarch64 carries
/// its own copy (the two are never compiled together).
extern "C" fn set_ivar(base: &mut RValue, id: IvarId, val: Value) {
    base.set_ivar_by_ivarid(id, val)
}
