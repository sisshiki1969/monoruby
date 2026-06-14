//! aarch64 AsmIR→machine-code lowering.
//!
//! The arch-neutral front-end builds `AsmIr`; this drives the lowering on
//! aarch64. Every `AsmInst` and side exit is lowered: large frame/field/sp
//! offsets are materialized through scratch registers rather than bailing, and
//! the `...`-forwarding deferral — the one shape that needed unported
//! caller-relative codegen — is disabled for aarch64 in `forward_rest_deferral`.
//! So aarch64 never bails out of JIT compilation; `compile_asmir`'s `bool` is
//! vestigial. See `doc/aarch64-jitgen-plan.md`.

use super::*;
use crate::codegen::jitgen::asmir::compile_shared::{
    extend_ivar, set_array_integer_index, set_ivar, unreachable,
};
use crate::codegen::jitgen::lir::{LAluOp, LCond, LInst, LMem, LOperand};
use monoasm_macro::monoasm_arm64;

///
/// Generational GC write barrier slow path, called from JIT inline stores.
///
/// The inline fast path has already verified that `parent` is old, not yet
/// remembered, and that the stored child is a heap object; this records
/// `parent` in the remembered set. See `doc/generational_gc_plan.md`.
///
extern "C" fn jit_write_barrier(parent: *mut RValue) {
    // SAFETY: `parent` is the live `&RValue` the store wrote into. The
    // inline fast path has already checked it is `wb_pending` with a heap
    // child, so `write_barrier_bulk` records it in the remembered set.
    unsafe { (*parent).write_barrier_bulk() };
}

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
    ) {
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
        // Loop-JIT entry sp-bump to undo before any exit resumes the VM.
        let bump = frame.loop_jit_spill_bytes;
        for side_exit in ir.side_exit {
            let label = match side_exit {
                // Eviction falls back to the interpreter like a deopt (the
                // `__immediate_evict` logging is `cfg(deopt/profile)`-only).
                SideExit::Evict(Some((pc, wb))) => {
                    let label = self.jit.label();
                    self.a64_gen_deopt(pc, &wb, label.clone(), bump, frame.base_stack_offset);
                    label
                }
                SideExit::Deoptimize(pc, wb) => {
                    let key = (pc, wb);
                    if let Some(label) = deopt_table.get(&key) {
                        label.clone()
                    } else {
                        let label = self.jit.label();
                        self.a64_gen_deopt(key.0, &key.1, label.clone(), bump, frame.base_stack_offset);
                        deopt_table.insert(key, label.clone());
                        label
                    }
                }
                // Treat recompile-deopt as a plain deopt for now: fall back to
                // the interpreter without the counter-gated recompile (still
                // correct, just not yet self-optimizing).
                SideExit::RecompileDeoptimize(pc, wb, _reason, _position) => {
                    let label = self.jit.label();
                    self.a64_gen_deopt(pc, &wb, label.clone(), bump, frame.base_stack_offset);
                    label
                }
                SideExit::Error(pc, wb) => {
                    let label = self.jit.label();
                    self.a64_gen_handle_error(pc, &wb, label.clone(), bump, frame.base_stack_offset);
                    label
                }
                // Evict(None) is a placeholder always overwritten with
                // Evict(Some(..)) before codegen (mirrors x86 gen_asm's
                // `_ => unreachable!()`).
                _ => unreachable!("unexpected {side_exit:?}"),
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
            self.compile_asmir(store, frame, &labels, inst, class_version.clone());
        }
        if let Some(exit) = exit {
            let exit = frame.resolve_bb_label(&mut self.jit, exit);
            monoasm_arm64!(&mut self.jit, b exit;);
        }
    }

    /// Undo the loop-JIT entry sp-bump (`emit_loop_jit_rsp_bump`) before
    /// resuming the interpreter. Unlike x86 — whose VM frame is rbp-relative,
    /// so a stale sp is harmless — the aarch64 VM sets up callee frames
    /// sp-relative, so every loop-JIT exit that resumes the VM (deopt, error,
    /// raise, retry, redo) must restore sp first. `bytes` is
    /// `frame.loop_jit_spill_bytes` (0 for a non-loop frame or a loop without
    /// spill); large bumps go through `a64_sp_add` (mirroring the entry
    /// `a64_sp_sub`).
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
    ) {
        self.jit.bind_label(entry);
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        self.a64_gen_write_back_for_deopt(wb, base);
        let pc_ptr = pc.as_ptr() as u64;
        let fetch = self.vm_fetch();
        // PC == x21.
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc_ptr);
            b fetch;
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
    ) {
        self.jit.bind_label(entry);
        self.a64_undo_loop_rsp_bump(loop_jit_spill_bytes);
        self.a64_gen_write_back_for_deopt(wb, base);
        let pc0 = pc.as_ptr() as u64;
        let raise = self.entry_raise();
        monoasm_arm64!(&mut self.jit,
            mov x21, (pc0);
            b raise;
        );
    }

    /// Write back live values to LFP slots for a side exit, r14(x22)-relative
    /// (the local frame may be on the heap after a call returns). Mirrors x86
    /// `gen_write_back_for_deopt`. The deferred forwarding-rest write-back never
    /// occurs on aarch64 (the deferral is disabled in `forward_rest_deferral`),
    /// so every write-back lowers.
    fn a64_gen_write_back_for_deopt(&mut self, wb: &WriteBack, base: usize) {
        debug_assert!(
            wb.forward_rest.is_empty(),
            "aarch64 disables forward-rest deferral, so a deopt write-back never carries one",
        );
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
        if let Some(slot) = wb.r15 {
            let off = slot.0 as u32 * 8 + LFP_SELF as u32;
            let acc = GP::R15.a64().0; // x23
            self.a64_frame_store(acc, lfp, off);
        }
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
            // Rem/bit-ops are compiled as method calls, never IntegerBinOp
            // (mirrors x86 `integer_binop`'s `_ => unreachable!()`).
            _ => unreachable!(),
        }
        true
    }

    /// Inlined `Integer#>>` / `Integer#<<` by a constant shift amount (the
    /// "shift right by `imm`" primitive). Operand is the tagged fixnum `2n+1`
    /// in Rdi (x4). aarch64 twin of x86 `gen_shr_imm`.
    pub(crate) fn gen_shr_imm(&mut self, imm: u8) {
        let rdi = GP::Rdi.a64().0; // x4
        if imm >= 64 {
            // Shift-out: -1 (all bits) collapses to -1, everything else to 0.
            let zero = self.jit.label();
            let exit = self.jit.label();
            let neg1 = Value::i32(-1).id();
            let z = Value::i32(0).id();
            monoasm_arm64!(&mut self.jit,
                tbz x(rdi), #(63), zero;   // non-negative -> 0
                mov x(rdi), (neg1);
                b exit;
                zero:
                mov x(rdi), (z);
                exit:
            );
        } else {
            // `((2n+1) >>a imm) | 1` == `2*(n>>imm) + 1`. monoasm has no
            // `orr`-immediate, so set the tag bit via a scratch register.
            monoasm_arm64!(&mut self.jit,
                asr x(rdi), x(rdi), #(imm as u32);
                mov x9, #(1);
                orr x(rdi), x(rdi), x9;
            );
        }
    }

    /// Inlined `Integer#<<` by a constant shift amount, with a fixnum-overflow
    /// guard that deopts. Operand `2n+1` in Rdi (x4). aarch64 twin of x86
    /// `gen_shl_rhs_imm`. x86 uses `lzcnt` for the overflow test; monoasm has
    /// no `clz`, so detect overflow by shifting back: a fixnum `n<<rhs` fits
    /// i63 iff the tagged `2n<<rhs` fits i64, i.e. `(2n<<rhs) >>a rhs == 2n`.
    pub(crate) fn gen_shl_rhs_imm(&mut self, rhs: u8, deopt: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            sub x(rdi), x(rdi), #(1);          // 2n (strip tag)
            lsl x9, x(rdi), #(rhs as u32);     // 2n << rhs
            asr x10, x9, #(rhs as u32);        // shift back (signed)
            cmp x(rdi), x10;                   // lost significant bits?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, deopt); // overflow -> deopt
        monoasm_arm64!(&mut self.jit,
            add x(rdi), x9, #(1);              // 2(n<<rhs) is even, so +1 sets the tag
        );
    }

    /// `Integer#>>` by a variable amount. lhs (tagged) in Rdi (x4), shift amount
    /// (tagged) in Rcx (x1); result tagged in Rdi. A negative shift means a left
    /// shift, which overflows -> deopt. aarch64 twin of x86 `gen_shr`, but
    /// without `lzcnt`/`select_page`: overflow is checked by shifting back, and
    /// the cold (left-shift / >=64) blocks are laid out inline.
    pub(crate) fn gen_shr(&mut self, deopt: &DestLabel) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        let cont = self.jit.label();
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x1, x1, #1;            // untag shift amount (Rcx == x1)
            cmp x1, #0;
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &shl); // negative -> left shift
        monoasm_arm64!(&mut self.jit, cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &under);
        monoasm_arm64!(&mut self.jit, asr x4, x4, x1;); // tagged sar (Rdi == x4)
        self.jit.bind_label(after.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, #1;
            orr x4, x4, x9;           // re-tag fixnum
            b cont;
        );
        // left shift by -k (cold)
        self.jit.bind_label(shl);
        monoasm_arm64!(&mut self.jit, neg x1, x1; cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &deopt); // left >=64 -> overflow
        monoasm_arm64!(&mut self.jit,
            sub x4, x4, #1;           // strip tag -> 2n
            lsl x9, x4, x1;           // 2n << k
            asr x10, x9, x1;          // shift back
            cmp x10, x4;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // overflow
        monoasm_arm64!(&mut self.jit, mov x4, x9; b after;);
        // right shift by >= 64 (cold): 0 if lhs >= 0, else -1
        self.jit.bind_label(under);
        self.a64_shift_under(&after);
        self.jit.bind_label(cont);
    }

    /// `Integer#<<` by a variable amount. lhs (tagged) in Rdi (x4), shift amount
    /// (tagged) in Rcx (x1); result tagged in Rdi. A left shift that overflows
    /// the fixnum range deopts; a negative shift means a right shift. aarch64
    /// twin of x86 `gen_shl` (shift-back overflow, inline cold blocks). Used for
    /// both the literal- and register-lhs cases (recv is always loaded into Rdi
    /// by the builtin), so x86's `gen_shl_lhs_imm` has no aarch64 counterpart.
    pub(crate) fn gen_shl(&mut self, deopt: &DestLabel) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        let cont = self.jit.label();
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x1, x1, #1;            // untag shift amount
            cmp x1, #0;
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &shr); // negative -> right shift
        monoasm_arm64!(&mut self.jit, cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &deopt); // left >=64 -> overflow
        monoasm_arm64!(&mut self.jit,
            sub x4, x4, #1;           // strip tag -> 2n
            lsl x9, x4, x1;           // 2n << k
            asr x10, x9, x1;          // shift back
            cmp x10, x4;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // overflow
        monoasm_arm64!(&mut self.jit, mov x4, x9;);
        self.jit.bind_label(after.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, #1;
            orr x4, x4, x9;           // re-tag fixnum
            b cont;
        );
        // right shift by -k (cold)
        self.jit.bind_label(shr);
        monoasm_arm64!(&mut self.jit, neg x1, x1; cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &under);
        monoasm_arm64!(&mut self.jit, asr x4, x4, x1; b after;);
        // right shift by >= 64 (cold): 0 if lhs >= 0, else -1
        self.jit.bind_label(under);
        self.a64_shift_under(&after);
        self.jit.bind_label(cont);
    }

    /// Shared cold tail for a shift-right by >= 64 bits: the tagged lhs is in
    /// Rdi (x4); leave 0 (Value 0) for a non-negative lhs or -1 (Value -1) for a
    /// negative one, then branch to `after` (which re-tags). Mirrors x86
    /// `shift_under`.
    fn a64_shift_under(&mut self, after: &DestLabel) {
        let zero = self.jit.label();
        monoasm_arm64!(&mut self.jit, cmp x4, #0;);
        self.jit.bcond_label(monoasm::Cond::Ge, &zero);
        monoasm_arm64!(&mut self.jit,
            mov x4, #0;
            sub x4, x4, #1;           // -1 (sar of a negative number by >=64)
            b after;
        );
        self.jit.bind_label(zero);
        monoasm_arm64!(&mut self.jit,
            mov x4, #0;
            b after;
        );
    }

    /// Inlined `Integer#%` (general fixnum case). `a` (Rdi/x4) and `b` (Rsi/x3)
    /// are tagged; the floor-mod remainder is returned tagged in Rax (x0). b==0
    /// deopts with a `_divide_by_zero` marker. aarch64 twin of x86
    /// `gen_int_rem`; mirrors the floor adjustment of `a64_integer_binop`'s Div
    /// (`sdiv` truncates toward zero, so when the remainder is non-zero and its
    /// sign differs from the divisor's, add the divisor back).
    pub(crate) fn gen_int_rem(&mut self, deopt: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4 (a, tagged)
        let rsi = GP::Rsi.a64().0; // x3 (b, tagged)
        let rax = GP::Rax.a64().0; // x0 (result)
        let zero_div = self.jit.label();
        let exit = self.jit.label();
        let done = self.jit.label();
        let deopt = deopt.clone();
        let sym = Value::symbol_from_str("_divide_by_zero").id();
        monoasm_arm64!(&mut self.jit,
            asr x(rsi), x(rsi), #(1);        // b untagged
            cbz x(rsi), zero_div;            // b==0 -> ZeroDivisionError
            asr x9, x(rdi), #(1);            // a untagged
            sdiv x10, x9, x(rsi);            // q = trunc(a/b)
            msub x11, x10, x(rsi), x9;       // rem = a - q*b
            cbz x11, exit;                   // exact -> no floor adjust
            eor x12, x11, x(rsi);            // sign(rem) vs sign(b)
            tbz x12, #(63), exit;            // same sign -> no adjust
            add x11, x11, x(rsi);            // floor-mod: rem += b
            exit:
            lsl x(rax), x11, #(1);           // re-tag the remainder
            add x(rax), x(rax), #(1);
            b done;
            zero_div:
            mov x(rdi), (sym);               // deopt marker reg (mirrors x86)
            b deopt;
            done:
        );
    }

    /// Inlined `Integer#**` between two fixnums: untag and call the runtime
    /// `pow_ii(a, b, vm)` (which returns the boxed result, possibly a BigInt,
    /// or 0/None on error). Result Value lands in Rax (x0). aarch64 twin of x86
    /// `gen_int_pow`. The FP pool is saved around the C-call.
    pub(crate) fn gen_int_pow(&mut self, using_xmm: UsingXmm, error: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4 (a, tagged)
        let rsi = GP::Rsi.a64().0; // x3 (b, tagged)
        let f = crate::executor::op::pow_ii as *const () as u64;
        let error = error.clone();
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            asr x0, x(rdi), #(1);    // a (untagged) -> arg0
            asr x1, x(rsi), #(1);    // b (untagged) -> arg1
            mov x2, x19;             // vm (EXEC) -> arg2
            str x30, [sp, #-16]!;    // save LR
            mov x9, (f);
            blr x9;                  // x0 = pow_ii(a, b, vm)
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            cbz x0, error;           // 0/None -> raise
        );
    }

    /// `Integer#%` with a Float rhs: `rem_ff(lhs, rhs)` (f64,f64 -> f64). The
    /// operands are loaded into d0/d1, the result (d0) stored into `dst_xmm`.
    /// aarch64 twin of x86 `gen_int_rem_if`.
    pub(crate) fn gen_int_rem_if(
        &mut self,
        lhs_xmm: FPReg,
        rhs_xmm: FPReg,
        dst_xmm: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) {
        let f = crate::executor::op::rem_ff as *const () as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        self.a64_fpr_into_d(lhs_xmm, 0, base);
        self.a64_fpr_into_d(rhs_xmm, 1, base);
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;                  // d0 = rem_ff(d0, d1)
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
        self.a64_d0_into_fpr(dst_xmm, base);
    }

    /// `Integer#**` with a Float rhs: `pow_ff(lhs, rhs)` (f64,f64 -> Value, which
    /// may be Complex). The operands are loaded into d0/d1; the result Value
    /// lands in Rax (x0). aarch64 twin of x86 `gen_int_pow_if`.
    pub(crate) fn gen_int_pow_if(
        &mut self,
        lhs_xmm: FPReg,
        rhs_xmm: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) {
        let f = crate::executor::op::pow_ff as *const () as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        self.a64_fpr_into_d(lhs_xmm, 0, base);
        self.a64_fpr_into_d(rhs_xmm, 1, base);
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;                  // x0 = pow_ff(d0, d1)
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
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
        // Callers pass frame-field offsets (RSP_LOCAL_FRAME + LFP_*), all well
        // within the unscaled 9-bit range, so a single `stur` always suffices.
        debug_assert!(off <= 256, "store-below-sp offset out of stur range");
        monoasm_arm64!(&mut self.jit, stur x9, [sp, #(-(off as i32))];);
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
            self.a64_frame_load(9, lfp, off);
        } else {
            monoasm_arm64!(&mut self.jit, mov x9, (0u64););
        }
        self.a64_store_x9_below_sp(block_off);
    }

    /// Lower `SetArguments`: one C call to `jit_generic_set_arguments(vm,
    /// globals, callid, callee_lfp, fid)` which massages the caller's args into
    /// the callee frame. Returns rax==0 (None) on error (followed by a
    /// HandleError in the IR). `offset` (callee frame size, 16-aligned) is
    /// reserved below sp around the call (large frames go through `a64_sp_*`).
    fn a64_set_arguments(&mut self, callid: CallSiteId, fid: FuncId, offset: usize) -> bool {
        let f = crate::runtime::jit_generic_set_arguments as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, sp, #(RSP_LOCAL_FRAME as u32); // callee_lfp (call-site sp)
            mov x4, (fid.get() as u64);        // callee fid
            str x30, [sp, #-16]!;              // save LR
        );
        self.a64_sp_sub(offset as u32);        // reserve callee scratch
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;
        );
        self.a64_sp_add(offset as u32);
        monoasm_arm64!(&mut self.jit,
            ldr x30, [sp], #16;                // restore LR
        );
        true
    }

    /// Lower `SetArgumentsForwarded` (the `g(*rest, **kw, &blk)` trampoline
    /// fast path). aarch64 does not emit the inline element-copy fast path, so it
    /// always falls back to the generic `jit_generic_set_arguments` — the x86
    /// fast path's own miss target, so correct, just unoptimized. The deferred
    /// `...`-rest (`deferred_src`) shape never occurs because aarch64 disables
    /// the deferral (`forward_rest_deferral`), so the rest array is always built
    /// normally and forwarding goes through the generic helper.
    fn a64_set_arguments_forwarded(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
        deferred_src: Option<(SlotId, u16)>,
    ) -> bool {
        debug_assert!(
            deferred_src.is_none(),
            "aarch64 disables forward-rest deferral, so SetArgumentsForwarded never carries deferred_src",
        );
        let _ = deferred_src;
        self.a64_set_arguments(callid, fid, offset)
    }

    /// Lower `SetArgumentsForwardedHelper`: same asm shape as
    /// `a64_set_arguments`, but dispatches to the specialized
    /// `jit_forwarded_set_arguments` runtime helper (forwarding `g(x.., ...)`
    /// into a no-keyword iseq with opt/post/rest). Large callee frames go
    /// through `a64_sp_*`.
    pub(in crate::codegen::jitgen) fn jit_set_arguments_forwarded_helper(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
    ) -> bool {
        let f = crate::runtime::jit_forwarded_set_arguments as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, sp, #(RSP_LOCAL_FRAME as u32); // callee_lfp (call-site sp)
            mov x4, (fid.get() as u64);        // callee fid
            str x30, [sp, #-16]!;              // save LR
        );
        self.a64_sp_sub(offset as u32);        // reserve callee scratch
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;
        );
        self.a64_sp_add(offset as u32);
        monoasm_arm64!(&mut self.jit,
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
            stur x22, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];  // new_cfp.lfp = LFP
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
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
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
    fn a64_store_imm_to_slot(&mut self, imm: u64, slot: SlotId, lfp: u32) {
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        monoasm_arm64!(&mut self.jit, mov x9, (imm););
        self.a64_frame_store(9, lfp, off);
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

    // ---- spill-aware FP-register access -----------------------------------
    // The FP register allocator keeps unboxed floats in the pool D2-D15
    // (`PHYS_XMM_POOL` = 14 ⇒ `FPRegLoc::Xmm(2..=15)`); when it needs more live
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
    // caller-saved — `XmmSave`/`XmmRestore` spill the live subset around any
    // clobbering call (a JIT→JIT `blr` clobbers D2-D15; a C-call's Rust callee
    // preserves D8-D15 but the spill is harmless), and `f64_to_val`'s heap path
    // saves D2-D7 while relying on `float_heap` to preserve D8-D15.

    /// Place `src`'s value into physical D-register `dreg` unconditionally
    /// (`fmov` for a pool register, a frame load for a spill slot).
    fn a64_fpr_load(&mut self, src: FPReg, dreg: u32, base: usize) {
        match src.loc(base) {
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

    /// Store physical D-register `dreg` into `dst` unconditionally (`fmov` for a
    /// pool register, a frame store for a spill slot).
    fn a64_fpr_save(&mut self, dst: FPReg, dreg: u32, base: usize) {
        match dst.loc(base) {
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
        match src.loc(base) {
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
        match dst.loc(base) {
            FPRegLoc::Xmm(p) => p as u32,
            FPRegLoc::Spill(_) => dreg,
        }
    }

    /// Flush scratch `dreg` back to `dst`'s spill slot; a no-op when `dst` is
    /// pool-resident (the op already wrote its pool register in place).
    fn a64_fpr_commit(&mut self, dst: FPReg, dreg: u32, base: usize) {
        if let FPRegLoc::Spill(_) = dst.loc(base) {
            self.a64_fpr_save(dst, dreg, base);
        }
    }

    // ---- specialized (inlined) frame lowering (aarch64) -------------------

    /// `MethodRetSpecialized` / `BlockBreakSpecialized`: a clean return that
    /// unwinds `rbp_offset` bytes of inlined frames at once. Mirrors x86
    /// `method_return_specialized` (`lea rbp,[rbp+off]; leave; ret`): adjust the
    /// native frame base (x29), then run the standard epilogue. No error path —
    /// the value is already in the accumulator and the caller frame is JIT'd.
    pub(super) fn method_return_specialized(&mut self, rbp_offset: usize) {
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
    pub(super) fn load_dyn_var_specialized(&mut self, offset: usize, reg: SlotId) {
        let e: i64 = offset as i64 - (BP_CFP + CFP_LFP) as i64 - 8 - conv(reg) as i64;
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x10, (e as u64);
            add x10, x29, x10;
            ldr x(rax), [x10];
        );
    }

    /// `StoreDynVarSpecialized`: outer-scope local <- src, symmetric to
    /// `load_dyn_var_specialized`. `src` maps to x0..x8 / x20..x23, never
    /// the x10 scratch, so there is no clobber.
    pub(super) fn store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
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
    pub(super) fn do_specialized_call(
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
            stur x22, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];  // new_cfp.lfp = LFP
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
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        );
        return_addr
    }

    /// `SetupYieldFrame`: build the callee **block** frame for a specialized
    /// `yield` before `SpecializedYield` branches into it. Walks `outer - 1`
    /// outer-LFP links to the block's defining frame, then writes the callee
    /// frame's outer/meta/svar/cme/block/self slots. A literal translation of
    /// x86 `setup_yield_frame` (x29-free; uses x9 = outer LFP, x11 = value
    /// scratch — neither of which is GP-mapped; `stur`/`ldur` address the
    /// frame fields directly off sp/x9 with no address-scratch register). The cfp
    /// prev/lfp it also writes are immediately overwritten by the following
    /// `SpecializedYield`'s push_frame, exactly as on x86.
    pub(super) fn setup_yield_frame(&mut self, meta: Meta, outer: usize) {
        let outer = outer - 1;
        monoasm_arm64!(&mut self.jit, ldr x9, [x19, #(EXECUTOR_CFP as u32)];);
        for _ in 0..outer {
            monoasm_arm64!(&mut self.jit, ldr x9, [x9];);
        }
        monoasm_arm64!(&mut self.jit,
            ldur x9, [x9, #(-(CFP_LFP as i32))];              // x9 <- outer LFP
            // new_cfp.prev = exec.cfp
            ldr x11, [x19, #(EXECUTOR_CFP as u32)];
            stur x11, [sp, #(-(RSP_CFP as i32))];
            // new_cfp.lfp = rsp + (24 - RSP_LOCAL_FRAME) = sp - 16
            sub x11, sp, #(16u32);
            stur x11, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
            // frame.outer = outer LFP
            stur x9, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            // frame.meta
            mov x11, (meta.get());
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
            // svar / cme / block = 0 (block callee resolves via outer chain;
            // zeroed so the GC mark walker stays sound)
            mov x11, (0u64);
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_CME) as i32))];
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];
            // frame.self = [outer LFP - LFP_SELF]
            ldur x11, [x9, #(-(LFP_SELF as i32))];
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
        );
    }

    /// `RestKw`: build a const-data table of (name: i32, slot-id: i32) pairs
    /// terminated by (0, 0), then call `correct_rest_kw(&table, lfp)` which
    /// reads the listed slots and returns the `**kwrest` Hash in x0. Mirrors
    /// the x86 `RestKw` arm; the const-table emission is arch-neutral and the
    /// table address is taken with PC-relative `adr` (as in OptCase).
    pub(in crate::codegen::jitgen) fn emit_rest_kw(&mut self, rest_kw: Vec<(SlotId, IdentId)>) {
        let data = self.jit.const_align8();
        for (i, name) in rest_kw.into_iter() {
            self.jit.const_i32(name.get() as i32);
            self.jit.const_i32(i.0 as i32);
        }
        self.jit.const_i32(0);
        self.jit.const_i32(0);
        let f = runtime::correct_rest_kw as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            adr x0, data;          // &table
            mov x1, x22;           // lfp (R14)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                // x0 = kwrest Hash
            ldr x30, [sp], #16;
        );
    }

    /// Shared tail of `ClassDef` / `SingletonClassDef`: enter the class
    /// context, run the class body, store the result, then leave. Expects the
    /// new class/module `self` in x0. Mirrors x86 `jit_class_def_sub`; the
    /// call_funcdata sequence is the same as `emit_yield`'s. `dst_off`, if set,
    /// is the pre-range-checked `conv(dst)` byte offset of the result slot.
    fn a64_jit_class_def_sub(
        &mut self,
        func_id: FuncId,
        dst_off: Option<u32>,
        using_xmm: UsingXmm,
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
            // set_method_outer: outer/svar/cme = 0
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_CME) as i32))];
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
        self.emit_xmm_restore(using_xmm, false);
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
        using_xmm: UsingXmm,
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
        self.emit_xmm_save(using_xmm, false);
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
        self.a64_xmm_reload(using_xmm);
        self.emit_handle_error(error);
        self.a64_jit_class_def_sub(func_id, dst_off, using_xmm, error);
        true
    }

    /// `SingletonClassDef`: `class << obj; … end`. Like `class_def` but the
    /// class is obtained via `define_singleton_class(vm, globals, obj)`.
    pub(in crate::codegen::jitgen) fn singleton_class_def(
        &mut self,
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        let base_off = conv(base) as u32;
        let dst_off = dst.map(|d| conv(d) as u32);
        let lfp = GP::R14.a64().0; // x22
        let f = runtime::define_singleton_class as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.a64_xmm_reload(using_xmm);
        self.emit_handle_error(error);
        self.a64_jit_class_def_sub(func_id, dst_off, using_xmm, error);
        true
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

    /// dst <- src (general-purpose register move; self-move is a no-op).
    pub(in crate::codegen::jitgen) fn emit_reg_move(&mut self, src: GP, dst: GP) {
        self.encode_linst(LInst::Mov { dst, src });
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
        match inst {
            // dst <- src (elided when the physical registers coincide)
            LInst::Mov { dst, src } => {
                let (s, d) = (src.a64().0, dst.a64().0);
                if s != d {
                    monoasm_arm64!(&mut self.jit, mov x(d), x(s););
                }
            }
            // dst <- imm (monoasm_arm64! expands a 64-bit immediate to the
            // movz/movk sequence as needed)
            LInst::LoadImm { dst, imm } => {
                let d = dst.a64().0;
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
                self.a64_frame_load(dst.a64().0, lfp, off);
            }
            // dst <- [base + disp] (object field). `a64_field_load` legalizes
            // the (positive) displacement: scaled ldr immediate, else scratch
            // x10 materialization.
            LInst::Load {
                dst,
                mem: LMem::Field { base, disp },
            } => {
                self.a64_field_load(dst.a64().0, base.a64().0, disp as u32);
            }
            // [lfp - slot] <- src (legalized like `Load`).
            LInst::Store {
                src,
                mem: LMem::Slot(slot),
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
                self.a64_field_store(src.a64().0, base.a64().0, disp as u32);
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
                    let d = dst.a64().0;
                    let imm = i as u64;
                    match (op, dst == GP::Rsp) {
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
                let l = lhs.a64().0;
                match rhs {
                    LOperand::Reg(r) => {
                        let r = r.a64().0;
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
            other => {
                todo!("LIR encode (aarch64): {other:?} not yet migrated (Phase-1 Stage > 2-A)")
            }
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

    /// [lfp - slot*8 - LFP_SELF] <- reg
    pub(in crate::codegen::jitgen) fn emit_reg_to_stack(&mut self, r: GP, slot: SlotId) {
        self.encode_linst(LInst::Store {
            src: r,
            mem: LMem::Slot(slot),
        });
    }

    /// reg <- [lfp - slot*8 - LFP_SELF]
    pub(in crate::codegen::jitgen) fn emit_stack_to_reg(&mut self, slot: SlotId, r: GP) {
        self.encode_linst(LInst::Load {
            dst: r,
            mem: LMem::Slot(slot),
        });
    }

    /// reg <- literal Value (immediate)
    pub(in crate::codegen::jitgen) fn emit_lit_to_reg(&mut self, v: Value, r: GP) {
        self.encode_linst(LInst::LoadImm {
            dst: r,
            imm: v.id(),
        });
    }

    /// [lfp - slot*8 - LFP_SELF] <- literal Value. The immediate is
    /// materialized in a scratch reg (aarch64 has no store-immediate), so no GP
    /// register is clobbered. Mirrors x86 `literal_to_stack`.
    pub(in crate::codegen::jitgen) fn emit_lit_to_stack(&mut self, v: Value, slot: SlotId) -> bool {
        self.encode_linst(LInst::StoreImm {
            imm: v.id(),
            mem: LMem::Slot(slot),
        });
        true
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

    /// GC safepoint: if alloc_flag >= 8 (signal/gc-stress nudge), write back
    /// live values, run execute_gc(vm, globals), and on error jump to the error
    /// handler. The GC path is laid out inline but skipped on the common path.
    /// Bails (`false`) like `emit_check_stack`. `base` is unused on aarch64.
    /// Mirrors x86 `jit_execute_gc`.
    pub(in crate::codegen::jitgen) fn emit_exec_gc(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        base: usize,
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
        self.a64_gen_write_back_for_deopt(&write_back, base);
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
        let error = error.clone();
        let cv_addr = self
            .jit
            .get_label_address(&self.const_version_label())
            .as_ptr() as u64;
        let f = runtime::set_constant as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
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
        let f = runtime::get_global_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            str x30, [sp, #-16]!;         // save LR across the call
            mov x9, (f);
            blr x9;                       // result in x0 (= rax)
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// $gvar <- src via runtime::set_global_var(vm, globals, name, val).
    pub(in crate::codegen::jitgen) fn emit_store_gvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::set_global_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
        );
        self.a64_frame_load(3, lfp, off); // x3 = val (from slot)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- @@cvar via runtime::get_class_var(vm, globals, name).
    pub(in crate::codegen::jitgen) fn emit_load_cvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        let f = runtime::get_class_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                       // result in x0
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- dynamic (outer-frame) local. Walk `outer` outer-LFP links
    /// (LFP_OUTER == 0, so `[lfp]` is the next outer frame), then load the slot.
    /// Mirrors x86 `load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_load_dyn_var(&mut self, src: DynVar) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.reg.0 as u32 * 8 + LFP_SELF as u32;
        let rax = GP::Rax.a64().0;
        self.a64_get_outer(src.outer, lfp, rax);
        self.a64_frame_load(rax, rax, off);
        true
    }

    /// dynamic (outer-frame) local <- src. Symmetric to `emit_load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_store_dyn_var(&mut self, dst: DynVar, src: GP) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.reg.0 as u32 * 8 + LFP_SELF as u32;
        let s = src.a64().0;
        // Walk to the outer LFP in x9 and form the address in x10 — both pure
        // lowering scratch (x9..x15 never alias a GP value register, which map
        // only to x0..x8 / x20..x23), so `src` is never clobbered whatever
        // register it is.
        self.a64_get_outer(dst.outer, lfp, 9);
        self.a64_frame_store(s, 9, off);
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
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let f = runtime::gen_array as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- Hash literal via gen_hash(vm, globals, &slot[args], len).
    pub(in crate::codegen::jitgen) fn emit_new_hash(
        &mut self,
        args: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = args.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::gen_hash as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- the Hash in `hash` after inserting the `len` key/value pairs
    /// at `args`, via hash_insert(vm, globals, &slot[args], len, hash)
    /// (chunked Hash literal).
    pub(in crate::codegen::jitgen) fn emit_hash_insert(
        &mut self,
        hash: SlotId,
        args: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let args_off = args.0 as u32 * 8 + LFP_SELF as u32;
        let hash_off = hash.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::hash_insert as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- the Array in `dst` after concatenating the Array in `src`,
    /// via array_concat(vm, globals, dst, src) (chunked Array literal).
    pub(in crate::codegen::jitgen) fn emit_array_concat(
        &mut self,
        dst: SlotId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let dst_off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let src_off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::array_concat as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0;
        let soff = start.0 as u32 * 8 + LFP_SELF as u32;
        let eoff = end.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::gen_range as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0;
        let off = arg.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::concatenate_string as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- `src` coerced to an Array: load slot `src`; if already an Array
    /// keep it, otherwise call runtime::to_a(vm, globals, val). Mirrors x86 to_a.
    pub(in crate::codegen::jitgen) fn emit_to_a(&mut self, src: SlotId, using_xmm: UsingXmm) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let toa = self.jit.label();
        let exit = self.jit.label();
        // Reserve the FP-pool save area for the whole sequence; both the
        // already-Array fast path and the to_a C call fall through to the
        // matching restore, so sp stays balanced either way.
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// rax <- a deep copy of literal `v` (a fresh mutable object per
    /// evaluation). Mirrors x86 `deepcopy_literal`.
    pub(in crate::codegen::jitgen) fn emit_deep_copy_lit(
        &mut self,
        v: Value,
        using_xmm: UsingXmm,
    ) -> bool {
        let imm = v.id();
        let f = Value::value_deep_copy as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, (imm);
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;            // result in x0 (= rax)
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    // ---- floating-point transfer primitives (aarch64) ---------------------
    // Operands resolve through the spill-aware accessors above (`a64_fpr_read`/
    // `_wtmp`/`_load`/`_save`/`_commit`). All return bool; `base` is the spill
    // base.

    /// dst(f64) <- src.
    pub(in crate::codegen::jitgen) fn emit_fpr_move(
        &mut self,
        src: FPReg,
        dst: FPReg,
        base: usize,
    ) -> bool {
        let s = self.a64_fpr_read(src, 0, base);
        let d = self.a64_fpr_wtmp(dst, 0, base);
        if s != d {
            monoasm_arm64!(&mut self.jit, fmov d(d), d(s););
        }
        self.a64_fpr_commit(dst, 0, base);
        true
    }

    /// swap two FP registers (via scratch D0/D1, spill-aware).
    pub(in crate::codegen::jitgen) fn emit_fpr_swap(&mut self, l: FPReg, r: FPReg, base: usize) -> bool {
        // Force both values into scratch, then store back crossed. (When both
        // are pool-resident this is the old `fmov`-through-d0 swap.)
        self.a64_fpr_load(l, 0, base);
        self.a64_fpr_load(r, 1, base);
        self.a64_fpr_save(l, 1, base);
        self.a64_fpr_save(r, 0, base);
        true
    }

    /// dst(f64) <- f64 constant `f`.
    pub(in crate::codegen::jitgen) fn emit_f64_to_fpr(&mut self, f: f64, x: FPReg, base: usize) -> bool {
        let p = self.a64_fpr_wtmp(x, 0, base);
        let bits = f.to_bits();
        monoasm_arm64!(&mut self.jit,
            mov x9, (bits);
            fmov d(p), x9;
        );
        self.a64_fpr_commit(x, 0, base);
        true
    }

    /// dst(f64) <- fixnum in GP `reg` (untag, signed int -> double).
    pub(in crate::codegen::jitgen) fn emit_fixnum_to_fpr(&mut self, reg: GP, x: FPReg, base: usize) -> bool {
        let p = self.a64_fpr_wtmp(x, 0, base);
        let r = reg.a64().0;
        monoasm_arm64!(&mut self.jit,
            asr x9, x(r), #(1);   // untag: value >> 1
            scvtf d(p), x9;
        );
        self.a64_fpr_commit(x, 0, base);
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
        let p = self.a64_fpr_wtmp(x, 0, base);
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
        self.a64_fpr_commit(x, 0, base);
        true
    }

    /// [slot] <- box(f64 in x): flonum-encode or heap-allocate.
    pub(in crate::codegen::jitgen) fn emit_fpr_to_stack(&mut self, x: FPReg, slot: SlotId, base: usize) -> bool {
        let lfp = GP::R14.a64().0;
        let f64_to_val = self.f64_to_val.clone();
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        self.a64_fpr_load(x, 0, base); // value -> d0 (pool fmov or spill load)
        monoasm_arm64!(&mut self.jit, bl f64_to_val;); // x0 = Value(f64)
        self.a64_frame_store(0, lfp, off);
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

    /// Reload the live FP pool registers from the save area *without* popping
    /// it. Used inside a multi-C-call emit (class_def) whose save area must
    /// persist across several clobbering calls: the pool regs are reloaded
    /// before each intermediate side-exit branch (whose handler reads the pool
    /// *registers*), while the save area itself is popped only once at the end
    /// via `emit_xmm_restore`. No-op when the pool is empty.
    fn a64_xmm_reload(&mut self, using_xmm: UsingXmm) {
        let mut i = 0u32;
        for (xi, b) in using_xmm.iter().enumerate() {
            if *b {
                let pr = xi as u32 + 2;
                let ofs = 8 * i;
                monoasm_arm64!(&mut self.jit, ldr d(pr), [sp, #(ofs)];);
                i += 1;
            }
        }
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
        // Only the four arithmetic ops are emitted as FloatBinOp (Rem/pow/etc.
        // are method calls); the `_ => unreachable!()` in the op match below
        // mirrors x86 `float_binop`.
        // Operands resolve to their pool register, or load a spill into d0/d1;
        // the result writes its pool register or scratch d0 (committed below).
        let ld = self.a64_fpr_read(l, 0, base);
        let rd = self.a64_fpr_read(r, 1, base);
        let dd = self.a64_fpr_wtmp(dst, 0, base);
        // aarch64 FP 3-op writes rd, reads rn/rm — no aliasing hazard.
        match kind {
            BinOpK::Add => monoasm_arm64!(&mut self.jit, fadd d(dd), d(ld), d(rd);),
            BinOpK::Sub => monoasm_arm64!(&mut self.jit, fsub d(dd), d(ld), d(rd);),
            BinOpK::Mul => monoasm_arm64!(&mut self.jit, fmul d(dd), d(ld), d(rd);),
            BinOpK::Div => monoasm_arm64!(&mut self.jit, fdiv d(dd), d(ld), d(rd);),
            _ => unreachable!(),
        }
        self.a64_fpr_commit(dst, 0, base);
        true
    }

    /// Float unary op: negate (flip bit 63) or unary-plus (no-op). The macro
    /// lacks `fneg`, so the sign flip goes through a GPR (`fmov`/`eor`/`fmov`).
    /// Bails (`false`) on a spilled operand or an unsupported `UnOpK`.
    pub(in crate::codegen::jitgen) fn emit_float_unop(&mut self, kind: UnOpK, dst: FPReg, base: usize) -> bool {
        match kind {
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
            // Float unary ops are only Neg/Pos (BitNot is integer-only).
            _ => unreachable!(),
        }
        true
    }

    /// `[lfp - slot*8 - LFP_SELF] <- Value::integer(i)` and `fpr(x) <- i as f64`
    /// (a constant int materialized as both a boxed integer and a double).
    /// Bails (`false`) if the FP destination is not lowerable (spilled).
    pub(in crate::codegen::jitgen) fn emit_i64_to_both(&mut self, i: i64, slot: SlotId, x: FPReg, base: usize) -> bool {
        let p = self.a64_fpr_wtmp(x, 0, base);
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
        self.a64_fpr_commit(x, 0, base);
        true
    }

    /// Float comparison; NaN-correct boolean Value in the accumulator. Mirrors
    /// `a64_flag_to_bool` but on `fcmp` flags with float condition codes. Bails
    /// (`false`) if either operand is spilled.
    pub(in crate::codegen::jitgen) fn emit_float_cmp(&mut self, kind: CmpKind, lhs: FPReg, rhs: FPReg, base: usize) -> bool {
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
        let lp = self.a64_fpr_read(lhs, 0, base);
        let rp = self.a64_fpr_read(rhs, 1, base);
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
    /// neutral fields on `Codegen`).
    pub(super) fn set_deopt_with_return_addr(
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

    /// Recompile-or-deopt point. Counter-gates a one-shot recompile, then falls
    /// through to the deopt side exit. For a whole-method recompile (`position`
    /// = None) it calls `jit_recompile_method`; for a loop-JIT recompile point
    /// (`position` = Some loop-pc) it calls `jit_recompile_loop`. Mirrors x86
    /// `recompile_and_deopt` / `jit_recompile_loop`.
    pub(in crate::codegen::jitgen) fn emit_recompile_deopt(
        &mut self,
        position: Option<BytecodePtr>,
        deopt: &DestLabel,
        error: Option<&DestLabel>,
        reason: RecompileReason,
    ) {
        let deopt = deopt.clone();
        // Counter-gated one-shot recompile, then fall through to the deopt side
        // exit (which undoes any loop sp-bump, writes back live values, and
        // re-enters the VM). x19-x23 are AAPCS64 callee-saved so the C call
        // preserves them, and the deopt does the value write-back; the
        // caller-saved d2-d7 pool is saved around the call because that
        // write-back reads it (d8-d15 are callee-saved).
        let counter = Box::into_raw(Box::new(COUNT_DEOPT_RECOMPILE)) as u64;
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
            // counter hit 0: recompile once, then deopt.
            sub sp, sp, #(48);
            str d2, [sp, #(0)];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            mov x0, x19;                     // vm (Executor)
            mov x1, x20;                     // globals
            mov x2, x22;                     // lfp
        );
        // Select the recompiler + set up its trailing args. Both take
        // (vm, globals, lfp, ...) above; the loop variant adds the loop pc.
        let f = if let Some(pc) = position {
            let pc_ptr = pc.as_ptr() as u64;
            monoasm_arm64!(&mut self.jit,
                mov x3, (pc_ptr);            // loop pc
                mov x4, (reason as u64);
            );
            crate::codegen::compiler::jit_recompile_loop as *const () as u64
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x3, (reason as u64);
            );
            crate::codegen::compiler::jit_recompile_method as *const () as u64
        };
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                          // -> Option<Value>: None (x0=0) = panic
            ldr x30, [sp], #16;
            ldr d2, [sp, #(0)];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            add sp, sp, #(48);
        );
        // Check the compiler's return value: the recompiler caught a panic, set
        // a Ruby `FatalError`, and returned None (x0 = 0). Branch to the error
        // side-exit (write-back + raise via entry_raise) instead of resuming the
        // interpreter. On success (x0 != 0) just deopt.
        if let Some(error) = error {
            let error = error.clone();
            monoasm_arm64!(&mut self.jit,
                cbz x0, error;
            );
        }
        monoasm_arm64!(&mut self.jit,
            b deopt;
        );
    }

    /// Emit the FP-pool-preserving C call to
    /// `jit_recompile_specialized(globals, idx, reason)`. The caller-saved
    /// d2-d7 pool is saved around the call because the following deopt's
    /// write-back reads it (d8-d15 are callee-saved); x19-x23 are AAPCS64
    /// callee-saved so the VM globals survive. `global_idx` is the resolved
    /// `specialized_base + idx` slot in `specialized_info`.
    fn a64_call_recompile_specialized(&mut self, global_idx: usize, reason: RecompileReason) {
        let f = crate::codegen::compiler::jit_recompile_specialized as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(48);
            str d2, [sp, #(0)];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            mov x0, x20;                  // globals
            mov x1, (global_idx as u64);  // specialized_info index
            mov x2, (reason as u64);      // RecompileReason
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            ldr d2, [sp, #(0)];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            add sp, sp, #(48);
        );
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
        monoasm_arm64!(&mut self.jit,
            stp x29, x30, [sp, #-16]!;
            mov x29, sp;
        );
        self.a64_sp_sub(prologue_bytes as u32);
        let clear_len = info.reg_num - info.arg_num;
        if clear_len > 0 {
            monoasm_arm64!(&mut self.jit, mov x9, (NIL_VALUE););
            for i in 0..clear_len {
                let off = (info.arg_num + i) as u32 * 8 + LFP_ARG0 as u32;
                self.a64_frame_store(9, lfp, off);
            }
        }
        true
    }

    /// Per-method ivar-cache prep: when the method accesses heap ivars, ensure
    /// self's var-table is large enough (so the later `Load/StoreIVarHeap` fast
    /// paths can write straight to a slot); grow it via `extend_ivar` otherwise.
    /// A no-op when no heap ivar is accessed or self is always-frozen. Mirrors
    /// x86 `emit_preparation` (inline cold path instead of a page-1 split).
    pub(in crate::codegen::jitgen) fn emit_preparation(&mut self, store: &Store, frame: &AsmInfo) -> bool {
        if frame.self_class.is_always_frozen() || !frame.ivar_heap_accessed {
            return true;
        }
        let ivar_len = store[frame.self_class].ivar_len();
        let heap_len = if frame.self_ty == Some(ObjTy::OBJECT) {
            ivar_len - OBJECT_INLINE_IVAR
        } else {
            ivar_len
        };
        let lfp = GP::R14.a64().0; // x22
        let f = extend_ivar as *const () as u64;
        let extend = self.jit.label();
        let exit = self.jit.label();
        // x0 = self (&RValue) and x1 = heap_len are also the `extend_ivar` args,
        // so they are set up *before* the var-table checks (which may branch to
        // `extend` straight away on a None table).
        monoasm_arm64!(&mut self.jit,
            ldur x0, [x(lfp), #(-(LFP_SELF as i32))];   // self
            mov x1, (heap_len as u64);                  // heap_len
            ldr x9, [x0, #(RVALUE_OFFSET_VAR as u32)];  // var_table ptr
            cbz x9, extend;                             // None -> grow
            ldr x10, [x9, #(MONOVEC_CAPA as u32)];
            cbz x10, extend;                            // capa 0 -> grow
            ldr x10, [x9, #(MONOVEC_LEN as u32)];
            cmp x10, x1;                                // len vs heap_len
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &extend); // len < heap_len -> grow
        monoasm_arm64!(&mut self.jit, b exit;);
        // cold: extend_ivar(self, heap_len). At method prologue, so no live FP
        // pool register to preserve (matches x86, which also omits xmm save).
        monoasm_arm64!(&mut self.jit,
            extend:
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            exit:
        );
        true
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

    ///
    /// Emit the generational GC write barrier after a JIT inline store whose
    /// parent object is in `parent` and whose stored child value is in `child`.
    ///
    /// Fast path — a young parent (the common case), an already-remembered
    /// parent, or an immediate child — is one flag-bit test plus an
    /// immediate-tag test, with no call. The rare slow path saves the
    /// caller-saved registers the JIT may have live (the abstract scratch GPs
    /// `x0..x8` and the caller-saved FP regs `d0..d7`), so it is fully
    /// transparent to the surrounding code and needs no liveness information,
    /// then calls `jit_write_barrier`. aarch64 twin of x86
    /// `emit_write_barrier_rdi`. See `doc/generational_gc_plan.md`.
    ///
    pub(in crate::codegen::jitgen) fn emit_write_barrier(&mut self, parent: GP, child: GP) {
        let skip = self.jit.label();
        let p = parent.a64().0;
        let c = child.a64().0;
        monoasm_arm64!(&mut self.jit,
            // barrier armed?  (WB_PENDING = flag bit 6 = old & not remembered)
            ldrb w9, [x(p), #(RVALUE_OFFSET_FLAG as u32)];
            tbz x9, #(6), skip;        // WB_PENDING clear -> skip
            // child immediate?  (heap pointers have the low 3 bits clear)
            mov x9, (0b111);
            and x9, x(c), x9;
            cbnz x9, skip;             // immediate child -> skip
        );
        // Slow path: save the caller-saved regs the JIT may have live, pass the
        // parent in the C-ABI arg0 (x0), and call. `x(p)` is untouched by the
        // saves, so it still holds the parent when read into x0.
        let f = jit_write_barrier as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(144);
            str x0, [sp, #(0)];
            str x1, [sp, #(8)];
            str x2, [sp, #(16)];
            str x3, [sp, #(24)];
            str x4, [sp, #(32)];
            str x5, [sp, #(40)];
            str x6, [sp, #(48)];
            str x7, [sp, #(56)];
            str x8, [sp, #(64)];
            str d0, [sp, #(72)];
            str d1, [sp, #(80)];
            str d2, [sp, #(88)];
            str d3, [sp, #(96)];
            str d4, [sp, #(104)];
            str d5, [sp, #(112)];
            str d6, [sp, #(120)];
            str d7, [sp, #(128)];
            str x30, [sp, #(136)];
            mov x0, x(p);              // parent -> arg0
            mov x9, (f);
            blr x9;
            ldr x30, [sp, #(136)];
            ldr d7, [sp, #(128)];
            ldr d6, [sp, #(120)];
            ldr d5, [sp, #(112)];
            ldr d4, [sp, #(104)];
            ldr d3, [sp, #(96)];
            ldr d2, [sp, #(88)];
            ldr d1, [sp, #(80)];
            ldr d0, [sp, #(72)];
            ldr x8, [sp, #(64)];
            ldr x7, [sp, #(56)];
            ldr x6, [sp, #(48)];
            ldr x5, [sp, #(40)];
            ldr x4, [sp, #(32)];
            ldr x3, [sp, #(24)];
            ldr x2, [sp, #(16)];
            ldr x1, [sp, #(8)];
            ldr x0, [sp, #(0)];
            add sp, sp, #(144);
        );
        self.jit.bind_label(skip);
    }

    /// `ldr`/`str` use a 12-bit scaled (×8) immediate offset; bail above that.
    fn a64_field_off_ok(off: u32) -> bool {
        off <= 32760 && off % 8 == 0
    }

    /// reg += i (no-op when i == 0). `i` is materialized so any i32 works.
    pub(in crate::codegen::jitgen) fn emit_reg_add(&mut self, reg: GP, i: i32) {
        self.encode_linst(LInst::Alu {
            op: LAluOp::Add,
            dst: reg,
            lhs: reg,
            rhs: LOperand::Imm(i as i64),
        });
    }

    /// reg -= i (no-op when i == 0).
    pub(in crate::codegen::jitgen) fn emit_reg_sub(&mut self, reg: GP, i: i32) {
        self.encode_linst(LInst::Alu {
            op: LAluOp::Sub,
            dst: reg,
            lhs: reg,
            rhs: LOperand::Imm(i as i64),
        });
    }

    /// Loop-JIT entry stack bump (large bumps go through `a64_sp_sub`).
    pub(in crate::codegen::jitgen) fn emit_loop_jit_rsp_bump(&mut self, offset: LoopRspOffset) -> bool {
        let bytes = offset.unwrap_concrete();
        self.a64_sp_sub(bytes as u32);
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
        let rdi = GP::Rdi.a64().0;
        let s = src.a64().0;
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];
            ldr x9, [x9, #(MONOVEC_PTR as u32)];
        );
        self.a64_field_store(s, 9, off);
        // Write barrier: rdi = self (parent), src = stored value.
        self.emit_write_barrier(GP::Rdi, src);
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
        monoasm_arm64!(&mut self.jit, ldr x9, [x9, #(MONOVEC_PTR as u32)];);
        self.a64_field_store(s, 9, off);
        // Write barrier (rdi still holds the parent &RValue). The cold path
        // below goes through `set_ivar`, which already barriers, so it jumps
        // straight to `exit`.
        self.emit_write_barrier(GP::Rdi, src);
        monoasm_arm64!(&mut self.jit, b exit;);
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
        monoasm_arm64!(&mut self.jit, ldr x9, [x9, #(MONOVEC_PTR as u32)];); // data ptr
        self.a64_field_load(r15, 9, off);                    // value
        monoasm_arm64!(&mut self.jit,
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
        let f = runtime::undef_method as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                   // vm (Executor)
            mov x1, x20;                   // globals
            mov x2, (undef.get() as u64);  // undef (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// Alias a global var via runtime::alias_global_var(globals=x20, new, old).
    /// Bails when an xmm pool register is live.
    pub(in crate::codegen::jitgen) fn emit_alias_gvar(&mut self, new: IdentId, old: IdentId, using_xmm: UsingXmm) -> bool {
        let f = runtime::alias_global_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x20;                 // globals
            mov x1, (new.get() as u64);  // new IdentId
            mov x2, (old.get() as u64);  // old IdentId
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
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
        let f = runtime::check_class_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::set_class_var as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
        );
        self.a64_frame_load(3, lfp, off); // x3 = val (from slot)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off_old = old.0 as u32 * 8 + LFP_SELF as u32;
        let off_new = new.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::alias_method as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_yield as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // result in x0
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off); // -> dst
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// "super" if super is callable, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_super(
        &mut self,
        dst: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_super as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off);
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// "global-variable" if $name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_gvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_gvar as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// "class variable" if @@name exists, else nil. result -> dst.
    pub(in crate::codegen::jitgen) fn emit_defined_cvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_cvar as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// defined?(Const): runtime writes the result through the `dst` out-pointer.
    pub(in crate::codegen::jitgen) fn emit_defined_const(
        &mut self,
        dst: SlotId,
        siteid: ConstSiteId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_const as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0;
        let off_dst = dst.0 as u32 * 8 + LFP_SELF as u32;
        let off_recv = recv.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_method as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// defined?(@name): runtime writes the result through `dst`.
    pub(in crate::codegen::jitgen) fn emit_defined_ivar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::defined_ivar as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = func as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0;
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::array_teq as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off = arg.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::concatenate_regexp as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// Keyword-rest fixup: if the `slot` is nil, replace it with a fresh empty
    /// Hash (runtime::empty_hash, no args, result in x0). Mirrors the x86 inline
    /// path (no xmm save — no xmm is live at kw-rest setup). Bails on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_check_kw_rest(&mut self, slot: SlotId) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        let exit = self.jit.label();
        let f = runtime::empty_hash as *const () as u64;
        self.a64_frame_load(11, lfp, off);
        monoasm_arm64!(&mut self.jit,
            cmp x11, #(NIL_VALUE);       // slot == nil ?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &exit); // not nil -> keep
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // x0 = {}
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off); // slot = {}
        monoasm_arm64!(&mut self.jit,
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
        let rest = if let Some(rest_pos) = rest_pos {
            rest_pos as u64 + 1
        } else {
            0
        };
        let lfp = GP::R14.a64().0; // x22
        let off = dst.0 as u32 * 8 + LFP_SELF as u32;
        let rdi = GP::Rdi.a64().0; // x4 holds src
        let f = runtime::expand_array as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit, mov x0, x(rdi);); // src (from GP::Rdi)
        self.a64_addr_sub(1, lfp, off);  // x1 = &dst (descending base)
        monoasm_arm64!(&mut self.jit,
            mov x2, (len as u64);        // len
            mov x3, (rest);              // rest (0 = none)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        true
    }

    /// `==` / `!=` with an inline immediate fast path (mirrors the x86
    /// `opt_eq_cmp`). If BOTH operands are non-heap, non-flonum immediates the
    /// Ruby result is exact bit (identity) equality, produced inline via
    /// cmp + cset; otherwise fall through to the generic C-call `func`
    /// (x0=vm, x1=globals, x2=lhs, x3=rhs). `lhs`/`rhs` are loaded into x2/x3
    /// up front so the slow path can reuse them. The live xmm pool is
    /// saved/restored *only* around the slow-path C call (the inline fast path
    /// never touches the caller-saved d2.. pool regs); bails only on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = func as u64;
        let slow = self.jit.label();
        let done = self.jit.label();
        // Load operands into the C-arg registers (reused by the slow path).
        // Heap iff (bits & 0b111) == 0; Flonum iff (bits & 0b011) == 0b010.
        // Either operand heap/flonum -> generic C-call.
        self.a64_frame_load(2, lfp, off_l); // lhs
        self.a64_frame_load(3, lfp, off_r); // rhs
        monoasm_arm64!(&mut self.jit,
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
        );
        // Save the live FP pool only on the slow path: the C call clobbers the
        // caller-saved d2.. pool regs, but the inline fast path above (which
        // branches straight to `done`) leaves them untouched, so both paths
        // reach `done` with sp and the pool registers consistent.
        self.emit_xmm_save(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit,
        done:
        );
        true
    }

    /// Unary float C helper `f(f64) -> f64` (sin, sqrt, …): load the operand
    /// into d0 (the first/only AAPCS f64 arg = return reg), call, store d0 into
    /// dst. The live FP pool (d2-d7, caller-saved = clobbered by the callee) is
    /// saved/restored around the call exactly like the x86 twin. A spilled
    /// source/destination is handled by the spill-aware load/save helpers.
    pub(in crate::codegen::jitgen) fn emit_cfunc_f_f(
        &mut self,
        f: unsafe extern "C" fn(f64) -> f64,
        src: FPReg,
        dst: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) -> bool {
        let fp = f as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        self.a64_fpr_load(src, 0, base); // arg -> d0 (spill slots are x29-relative)
        monoasm_arm64!(&mut self.jit,
            mov x9, (fp);
            blr x9;            // result in d0
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
        self.a64_fpr_save(dst, 0, base); // result d0 -> dst (after the pool restore)
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
        let fp = f as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_xmm_save(using_xmm, false);
        self.a64_fpr_load(lhs, 0, base); // arg0 -> d0
        self.a64_fpr_load(rhs, 1, base); // arg1 -> d1
        monoasm_arm64!(&mut self.jit,
            mov x9, (fp);
            blr x9;            // result in d0
        );
        self.emit_xmm_restore(using_xmm, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
        self.a64_fpr_save(dst, 0, base); // result d0 -> dst (after the pool restore)
        true
    }

    /// Load a `FPReg` (pool reg or spill slot) into `d0`.
    fn a64_fpr_into_d0(&mut self, src: FPReg, base: usize) {
        self.a64_fpr_into_d(src, 0, base);
    }

    /// Load a `FPReg` (pool reg or spill slot) into the scratch register `dreg`
    /// (D0/D1, outside the D2-D15 pool). Spill-aware, so it never bails.
    fn a64_fpr_into_d(&mut self, src: FPReg, dreg: u32, base: usize) {
        match src.loc(base) {
            FPRegLoc::Xmm(p) => monoasm_arm64!(&mut self.jit, fmov d(dreg), d(p as u32);),
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;        // [x29 - off] (mirrors x86 [rbp - off])
                ldr d(dreg), [x10];
            ),
        }
    }

    /// Store `d0` into a `FPReg` (pool reg or spill slot).
    fn a64_d0_into_fpr(&mut self, dst: FPReg, base: usize) {
        match dst.loc(base) {
            FPRegLoc::Xmm(p) => monoasm_arm64!(&mut self.jit, fmov d(p as u32), d0;),
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;
                str d0, [x10];
            ),
        }
    }

    /// Inlined `Math.sqrt`: `fsqrt` on the unboxed argument. NaN passes through
    /// (`sqrt(NaN) == NaN`); a negative argument deopts so the interpreter
    /// re-runs the builtin and raises DomainError (`-0.0` compares equal to
    /// `0.0`, so it falls through and `fsqrt(-0.0) == -0.0`, as CRuby).
    /// aarch64 twin of x86 `math_sqrt`'s inline body.
    pub(crate) fn emit_math_sqrt(
        &mut self,
        src: FPReg,
        dst: Option<FPReg>,
        deopt: &DestLabel,
        base: usize,
    ) {
        self.a64_fpr_into_d0(src, base);
        let do_sqrt = self.jit.label();
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit, fcmp d0, #0.0;);
        self.jit.bcond_label(monoasm::Cond::Vs, &do_sqrt); // NaN (unordered) -> sqrt
        self.jit.bcond_label(monoasm::Cond::Mi, &deopt); // negative -> deopt
        self.jit.bind_label(do_sqrt);
        if let Some(dst) = dst {
            monoasm_arm64!(&mut self.jit, fsqrt d0, d0;);
            self.a64_d0_into_fpr(dst, base);
        }
    }

    /// `Range#begin`: load the start Value from the receiver in Rdi (x4) → Rax.
    pub(crate) fn emit_range_begin(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x4, #(crate::rvalue::RANGE_START_OFFSET as u32)];
        );
    }

    /// `Range#end`: load the end Value from the receiver in Rdi (x4) → Rax.
    pub(crate) fn emit_range_end(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x4, #(crate::rvalue::RANGE_END_OFFSET as u32)];
        );
    }

    /// `Range#exclude_end?`: read the 0/1 flag, shift into bit 3, then add
    /// FALSE_VALUE (whose bit 3 is clear, so `add` == `or`): 0→FALSE_VALUE,
    /// 8→0x1c=TRUE_VALUE. Receiver in Rdi (x4) → Rax (x0).
    pub(crate) fn emit_range_exclude_end(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr w0, [x4, #(crate::rvalue::RANGE_EXCLUDE_END_OFFSET as u32)];
            lsl x0, x0, #3;
            add x0, x0, #(FALSE_VALUE);
        );
    }

    /// `Fiber.yield` with no args: the yielded value (Rsi/x3) is nil.
    pub(crate) fn emit_fiber_yield_value_nil(&mut self) {
        monoasm_arm64!(&mut self.jit,
            mov x3, (Value::nil().id());   // GP::Rsi == x3
        );
    }

    /// `Fiber.yield(*args)` with ≥2 args: build the args array, leaving it in
    /// Rsi (x3). `args_off` is `conv(args)`; bounded by the caller.
    pub(crate) fn emit_fiber_yield_value_array(&mut self, args_off: usize, pos_num: usize) {
        monoasm_arm64!(&mut self.jit,
            sub x0, x22, #(args_off as u32);   // &args (lfp - conv)
            mov x1, (pos_num);
            mov x9, (crate::runtime::create_array as *const () as u64);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
            mov x3, x0;
        );
    }

    /// `Fiber.yield`: call `yield_fiber(vm, value)` with value in Rsi (x3). The
    /// method's own LR is saved: the invoker's a64_push_callee_save stashes the
    /// *post-blr* x30, not ours, so we restore the real return address after the
    /// fiber resumes back here.
    pub(crate) fn emit_fiber_yield_call(&mut self, yield_fiber: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;          // vm (EXEC)
            mov x1, x3;           // value (Rsi)
            mov x9, (yield_fiber);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Load a 64-bit tagged fixnum literal into Rsi (x3) for an `Integer` bit-op
    /// whose immediate doesn't fit a 32-bit encoding.
    pub(crate) fn emit_load_tagged_rsi(&mut self, tagged: i64) {
        monoasm_arm64!(&mut self.jit, mov x3, (tagged as u64);); // GP::Rsi == x3
    }

    /// `Integer#|` with a tagged immediate (`(2a+1)|(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitor_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (imm as u64);
            orr x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#|` register-register.
    pub(crate) fn emit_bitor_rr(&mut self) {
        monoasm_arm64!(&mut self.jit, orr x4, x4, x3;); // Rdi==x4, Rsi==x3
    }
    /// `Integer#&` with a tagged immediate (`(2a+1)&(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitand_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (imm as u64);
            and x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#&` register-register.
    pub(crate) fn emit_bitand_rr(&mut self) {
        monoasm_arm64!(&mut self.jit, and x4, x4, x3;);
    }
    /// `Integer#^` with a tagged immediate (use `imm-1` so lhs's tag survives).
    pub(crate) fn emit_bitxor_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, ((imm - 1) as u64);
            eor x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#^` register-register (`(2a+1)^(2b+1)` clears LSB, re-tag +1).
    pub(crate) fn emit_bitxor_rr(&mut self) {
        monoasm_arm64!(&mut self.jit,
            eor x4, x4, x3;          // GP::Rdi == x4, GP::Rsi == x3
            add x4, x4, #(1);
        );
    }

    /// `n << k` / `n >> -k` with `k >= 64`: a non-zero `n` overflows (deopt);
    /// `0` shifts to `0`. lhs in Rdi (x4).
    pub(crate) fn emit_shl_overflow_zero(&mut self, z: i64, deopt: &DestLabel) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (z as u64);
            cmp x4, x9;              // GP::Rdi == x4
        );
        self.jit.bcond_label(monoasm::Cond::Ne, deopt);
        monoasm_arm64!(&mut self.jit,
            mov x4, x9;
        );
    }

    /// `Integer#%` by a positive power of two: `lhs & mask` on the tagged
    /// fixnum in Rdi (x4).
    pub(crate) fn emit_int_rem_pow2_mask(&mut self, mask: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (mask as u64);
            and x4, x4, x9;          // GP::Rdi == x4
        );
    }

    /// `Enumerator::ArithmeticSequence#exclude_end?`: same encoding as
    /// `emit_range_exclude_end` but from the AS field.
    pub(crate) fn emit_as_exclude_end(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr w0, [x4, #(crate::rvalue::AS_EXCLUDE_END_OFFSET as u32)];
            lsl x0, x0, #3;
            add x0, x0, #(FALSE_VALUE);
        );
    }

    /// Load a 64-bit Value field at `offset` from the receiver in Rdi (x4) → Rax
    /// (x0). Shared by `ArithmeticSequence#begin`/`#end`/`#step`.
    pub(crate) fn emit_load_value_field(&mut self, offset: usize) {
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x4, #(offset as u32)];
        );
    }

    /// `BasicObject#!`: `recv | 0x10` is FALSE_VALUE iff recv is nil/false; map
    /// eq→TRUE, ne→FALSE. Receiver in Rdi (x4) → Rax (x0).
    pub(crate) fn emit_object_not(&mut self) {
        monoasm_arm64!(&mut self.jit,
            mov  x9, #(0x10);
            orr  x4, x4, x9;            // GP::Rdi == x4
            mov  x0, #(TRUE_VALUE);     // GP::Rax == x0
            mov  x3, #(FALSE_VALUE);    // GP::Rsi == x3
            cmp  x4, #(FALSE_VALUE);
            csel x0, x0, x3, eq;        // eq -> TRUE, else FALSE
        );
    }

    /// `Kernel#nil?`: receiver in Rdi (x4) → Rax (x0), nil→TRUE else FALSE.
    pub(crate) fn emit_kernel_nil(&mut self) {
        monoasm_arm64!(&mut self.jit,
            mov  x0, #(FALSE_VALUE);    // GP::Rax == x0
            mov  x3, #(TRUE_VALUE);     // GP::Rsi == x3
            cmp  x4, #(NIL_VALUE);      // GP::Rdi == x4
            csel x0, x3, x0, eq;        // nil -> TRUE, else FALSE
        );
    }

    /// `Kernel#block_given?`: the block slot at [LFP - LFP_BLOCK] is 0 or NIL
    /// when no block was passed. Result Value in Rax (x0).
    pub(crate) fn emit_block_given(&mut self) {
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, #(FALSE_VALUE);             // GP::Rax == x0
            sub x9, x22, #(LFP_BLOCK as u32);   // x22 == LFP (r14)
            ldr x4, [x9];                       // block handle -> GP::Rdi
            cbz x4, exit;
            cmp x4, #(NIL_VALUE);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            mov x0, #(TRUE_VALUE);
        );
        self.jit.bind_label(exit);
    }

    /// `Array#size`/`#length`: untagged length (`get_array_length`) tagged as a
    /// fixnum in Rax (x0).
    pub(crate) fn emit_array_size(&mut self) {
        self.get_array_length();
        monoasm_arm64!(&mut self.jit,
            lsl x0, x0, #1;
            add x0, x0, #1;
        );
    }

    /// `String#bytesize`: inline-vs-heap length select, tagged as a fixnum in
    /// Rax (x0). Receiver in Rdi (x4).
    pub(crate) fn emit_string_bytesize(&mut self) {
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            ldr x9, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, #(STRING_INLINE_CAP as u32);
            csel x0, x9, x0, gt;   // capa > inline cap -> use heap_len
            lsl x0, x0, #1;
            add x0, x0, #1;
        );
    }

    /// Length of the array whose pointer is in Rdi (x4) → Rax (x0), untagged.
    /// Arrays store a short length inline (the `capa` field) and switch to a
    /// heap buffer past `ARRAY_INLINE_CAPA`, in which case the real length is
    /// `heap_len`. aarch64 twin of x86 `get_array_length` (`cmovgt` → `csel`).
    pub(crate) fn get_array_length(&mut self) {
        let rdi = GP::Rdi.a64().0; // x4
        let rax = GP::Rax.a64().0; // x0
        monoasm_arm64!(&mut self.jit,
            ldr x(rax), [x(rdi), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            ldr x9, [x(rdi), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x(rax), #(ARRAY_INLINE_CAPA as u32);
            csel x(rax), x9, x(rax), gt;   // capa > inline cap -> use heap_len
        );
    }

    ///
    /// Array index read with a non-negative i64 index. aarch64 twin of x86
    /// `array_index`.
    ///
    /// ### in
    /// - Rdi (x4): base Array
    /// - Rsi (x3): index, non-negative i64
    ///
    /// ### out
    /// - Rax (x0): result Value (NIL when out of range)
    ///
    pub(crate) fn array_index(&mut self, out_range: &DestLabel) {
        // Unlike x86, the cold (heap / out-of-range) blocks are laid out inline
        // on the same page: aarch64 b/b.cond can't reach monoasm's second page
        // (it is mapped far past the ±128 MB branch range).
        let exit = self.jit.label();
        let heap = self.jit.label();
        let out_range = out_range.clone();
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x0, #(ARRAY_INLINE_CAPA as u32);
            b.gt heap;
            // inline: x3 (index) is a non-negative integer.
            cmp x0, x3;                              // capa vs index
            b.le out_range;                          // index >= capa -> out of range
            add x9, x4, x3, lsl #3;
            ldr x0, [x9, #(RVALUE_OFFSET_INLINE as u32)];
            b exit;
        }
        self.jit.bind_label(heap);
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, x3;
            b.le out_range;
            ldr x4, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            ldr x0, [x4, x3, lsl #3];
            b exit;
        }
        self.jit.bind_label(out_range);
        monoasm_arm64! { &mut self.jit,
            mov x0, #(NIL_VALUE as u32);
        }
        self.jit.bind_label(exit); // out_range falls through to exit
    }

    ///
    /// Array index assign with a non-negative i64 index. aarch64 twin of x86
    /// `array_index_assign`.
    ///
    /// ### in
    /// - Rdi (x4): base Array
    /// - Rsi (x3): index, non-negative i64
    /// - Rdx (x2): source Value
    ///
    /// ### destroy
    /// - caller-save registers except the FP pool
    ///
    pub(crate) fn array_index_assign(
        &mut self,
        using_xmm: UsingXmm,
        generic: &DestLabel,
        error: &DestLabel,
    ) {
        // Cold (heap / generic-C-call) blocks laid out inline; see array_index
        // for why select_page can't be used on aarch64.
        let exit = self.jit.label();
        let heap = self.jit.label();
        let generic = generic.clone();
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x0, #(ARRAY_INLINE_CAPA as u32);
            b.gt heap;
            // inline: x3 (index) is a non-negative integer.
            cmp x0, x3;
            b.le generic;                            // index >= capa -> generic
            add x9, x4, x3, lsl #3;
            str x2, [x9, #(RVALUE_OFFSET_INLINE as u32)];  // src (Rdx) -> slot
        }
        // Write barrier: x4 (Rdi) = the array (parent), x2 (Rdx) = stored value.
        self.emit_write_barrier(GP::Rdi, GP::Rdx);
        monoasm_arm64! { &mut self.jit, b exit; }
        self.jit.bind_label(heap);
        monoasm_arm64! { &mut self.jit,
            ldr x0, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, x3;
            b.le generic;
        }
        // Write barrier before `x4` (Rdi) is repointed at the heap buffer:
        // x4 = the array (parent), x2 (Rdx) = stored value.
        self.emit_write_barrier(GP::Rdi, GP::Rdx);
        monoasm_arm64! { &mut self.jit,
            ldr x4, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            add x9, x4, x3, lsl #3;
            str x2, [x9];
            b exit;
        }
        self.jit.bind_label(generic);
        self.emit_xmm_save(using_xmm, false);
        // set_array_integer_index(base, index, vm, globals, src). Source regs at
        // entry: base=x4, index=x3, src=x2. Reorder into the C ABI args
        // (x0..x4) without clobbering a still-needed source.
        let f = set_array_integer_index as *const () as u64;
        monoasm_arm64! { &mut self.jit,
            mov x0, x4;            // base -> arg0
            mov x4, x2;            // src  -> arg4   (x2 free after this)
            mov x1, x3;            // index -> arg1
            mov x3, x20;           // globals -> arg3
            mov x2, x19;           // vm -> arg2
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        }
        self.emit_xmm_restore(using_xmm, false);
        self.emit_handle_error(error);
        self.jit.bind_label(exit); // generic C-call path falls through to exit
    }

    /// Inlined `Integer#to_f`: untag the tagged fixnum in Rdi (x4), convert to
    /// double with `scvtf`, and store into `fret`. aarch64 twin of x86
    /// `integer_tof`'s `sarq` + `cvtsi2sdq` + `store_fpr_into_xmm`.
    pub(crate) fn emit_int_to_float(&mut self, fret: FPReg, base: usize) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            asr x(rdi), x(rdi), #(1);  // untag
            scvtf d0, x(rdi);
        );
        self.a64_d0_into_fpr(fret, base);
    }

    /// Inlined `Float#to_i`: truncate the double in `fsrc` to i64 (`fcvtzs`),
    /// then tag it as a fixnum. `fcvtzs` saturates out-of-range doubles to
    /// i64::MIN/MAX; doubling the result (`adds`) then overflows for both the
    /// saturated case and any value that doesn't fit in a 63-bit fixnum, so a
    /// single signed-overflow branch covers both. aarch64 twin of x86
    /// `cvttsd2siq` + `addq;jo` + `orq 1`. Result Value lands in Rdi (x4).
    pub(crate) fn emit_float_to_int(&mut self, fsrc: FPReg, deopt: &DestLabel, base: usize) {
        let rdi = GP::Rdi.a64().0; // x4
        self.a64_fpr_into_d0(fsrc, base);
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            fcvtzs x(rdi), d0;
            adds x(rdi), x(rdi), x(rdi);   // ×2, set NZCV
        );
        self.jit.bcond_label(monoasm::Cond::Vs, &deopt); // overflow -> deopt
        monoasm_arm64!(&mut self.jit,
            add x(rdi), x(rdi), #(1);      // tag (low bit clear after ×2)
        );
    }

    /// Inlined `BasicObject#object_id`: `i64_to_value(self_id)`. The receiver
    /// (its raw id) is in Rdi (x4); move it to the C ABI arg0 (x0) and call.
    /// Result Value lands in Rax (x0). The FP pool is saved by the surrounding
    /// AsmIr xmm_save/xmm_restore; here we only preserve LR around the `blr`.
    pub(crate) fn emit_object_id(&mut self) {
        let rdi = GP::Rdi.a64().0; // x4
        let f = crate::executor::op::i64_to_value as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // self id -> arg0
            mov x9, (f);
            str x30, [sp, #-16]!; // save LR
            blr x9;               // x0 = i64_to_value(id)
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Hash#[]`: `hashindex(vm, globals, recv, key)`. The receiver is
    /// already in Rdx (x2 == C arg2) and the key in Rcx (x1); move the key to
    /// arg3 first (before x1 is overwritten by globals), then load vm/globals.
    /// Result Value lands in Rax (x0); errors are checked by the trailing
    /// HandleError. FP pool saved by the surrounding xmm_save/restore.
    pub(crate) fn emit_hash_index(&mut self, hashindex: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x3, x1;           // key (Rcx) -> arg3   [recv already in x2 == Rdx]
            mov x0, x19;          // vm (EXEC) -> arg0
            mov x1, x20;          // globals (GLOBALS) -> arg1
            mov x9, (hashindex);
            str x30, [sp, #-16]!; // save LR
            blr x9;               // x0 = hashindex(vm, globals, recv, key)
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Class#allocate`: `alloc_func(class_id, globals)`. The class id
    /// (a u32) and the resolved allocator pointer are embedded as constants;
    /// arg0 = class_id, arg1 = globals (GLOBALS/x20). Result Value in Rax (x0).
    /// FP pool saved by the surrounding xmm_save.
    pub(crate) fn emit_class_allocate(&mut self, class_id: u32, alloc_func: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x0, (class_id as u64); // class_id -> arg0 (low 32 bits read)
            mov x1, x20;               // globals -> arg1
            mov x9, (alloc_func);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Class#new`: allocate via the resolved `alloc_func`, then run
    /// `initialize` on the new instance through `method_invoker2` (now a real
    /// invoker on aarch64). aarch64 twin of x86 `gen_class_new_inline`, minus
    /// the self-modifying inline cache: `data_i32` slots are bound only at
    /// `finalize`, so an emit-time `get_label_address` would bake a bogus
    /// address. Instead `initialize` is re-resolved with `check_initializer` on
    /// every call (a cheap method-table lookup) — still cheaper than the
    /// un-inlined `Class#new` dispatch.
    ///
    /// The new instance is held in R15 (x23, ACC — overwritten by the trailing
    /// def_rax2acc anyway); the result Value lands in Rax (x0), 0 on an
    /// `initialize` error (checked by the trailing HandleError). FP pool saved
    /// by the surrounding xmm_save/restore. The args slot offset is bounded by
    /// the caller (`gen_class_new_inline` bails past the `sub` immediate range).
    pub(crate) fn emit_class_new(
        &mut self,
        class_id: u32,
        alloc_func: u64,
        args_off: u32,
        pos_num: usize,
        check_initializer: u64,
    ) {
        let r15 = GP::R15.a64().0; // x23 (instance holder)
        let m2 = self.method_invoker2 as *const () as u64;
        let use_instance = self.jit.label();
        let done = self.jit.label();
        monoasm_arm64! { &mut self.jit,
            // o = alloc_func(class_id, globals)
            mov x0, (class_id as u64);
            mov x1, x20;
            mov x9, (alloc_func);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
            mov x(r15), x0;               // R15 = new instance
            // fid = check_initializer(globals, instance)  (0 == no initialize)
            mov x0, x20;
            mov x1, x(r15);
            mov x9, (check_initializer);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
            cbz x0, use_instance;         // no initialize -> return instance
            // method_invoker2(vm, globals, fid, recv, args, pos_num)
            mov x2, x0;                   // fid
            mov x0, x19;                  // vm
            mov x1, x20;                  // globals
            mov x3, x(r15);               // receiver
            sub x4, x22, #(args_off);     // &args (lfp - conv(args))
            mov x5, (pos_num as u64);
            mov x9, (m2);
            str x30, [sp, #-16]!;
            blr x9;                       // x0 = Option<Value> (0 == error)
            ldr x30, [sp], #16;
            cbz x0, done;                 // initialize raised -> x0 = 0
        }
        self.jit.bind_label(use_instance);
        monoasm_arm64! { &mut self.jit,
            mov x0, x(r15);               // result = instance
        }
        self.jit.bind_label(done);
    }

    /// Inlined `Array#clone`: `array_clone_extern(recv)`. recv (Rdi/x4) -> arg0.
    /// Result Value in Rax (x0). FP pool saved by the surrounding xmm_save.
    pub(crate) fn emit_array_clone(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Array#dup`: `array_dup_extern(recv, globals)`. recv (Rdi/x4) ->
    /// arg0, globals (GLOBALS/x20) -> arg1. Result Value in Rax (x0).
    pub(crate) fn emit_array_dup(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x1, x20;          // globals -> arg1
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Array#<<`: `ary_shl(recv, arg)`. recv (Rdi/x4) -> arg0,
    /// arg (Rsi/x3) -> arg1. Result Value in Rax (x0).
    pub(crate) fn emit_array_shl(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        let rsi = GP::Rsi.a64().0; // x3
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x1, x(rsi);       // arg  -> arg1
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
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
        let f = runtime::define_method as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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
        let lfp = GP::R14.a64().0; // x22
        let off = obj.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::singleton_define_method as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
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

    // ---- callee-frame argument stores ([sp + (ofs - RSP_LOCAL_FRAME)]) ------
    // Used by the inline argument-setup fast path (fetch_for_callee).

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
            ldur x(rax), [x(rax), #(-(LFP_BLOCK as i32))];
            mov x11, (1u64);
            tst x(rax), x11;             // Z = ((x0 & 1) == 0)
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &exit);
        if tag <= 4095 {
            monoasm_arm64!(&mut self.jit, add x(rax), x(rax), #(tag););
        } else {
            monoasm_arm64!(&mut self.jit, mov x11, (tag as u64); add x(rax), x(rax), x11;);
        }
        monoasm_arm64!(&mut self.jit, exit:);
        // store_rax(ret): [lfp - off] <- x0
        self.a64_frame_store(rax, lfp, off);
        true
    }

    /// `&block` captured as a Proc value: runtime::block_arg(vm, globals, lfp,
    /// call_site) materializes the current frame's block handler into a Proc
    /// (promoting the frame to the heap if needed). The Option<Value> result is
    /// stored to `ret` after a HandleError. The live xmm pool is saved/restored
    /// around the C call (restore placed before the HandleError branch so the
    /// side exit writes the live floats back from the pool); bails only on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_block_arg(
        &mut self,
        ret: SlotId,
        using_xmm: UsingXmm,
        call_site_bc_ptr: BytecodePtr,
        error: &DestLabel,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = ret.0 as u32 * 8 + LFP_SELF as u32;
        let cs = call_site_bc_ptr.as_ptr() as u64;
        let f = runtime::block_arg as *const () as u64;
        self.emit_xmm_save(using_xmm, false);
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
        self.emit_xmm_restore(using_xmm, false);
        self.emit_handle_error(error);
        let rax = GP::Rax.a64().0;
        self.a64_frame_store(rax, lfp, off); // ret <- Proc
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
        store: &Store,
        _frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        _class_version: DestLabel,
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
            AsmInst::GuardClassVersionSpecialized { idx, deopt } => {
                let global_idx = self.specialized_base + idx;
                let deopt = labels[deopt].clone();
                let miss = self.jit.label();
                let done = self.jit.label();
                self.a64_guard_class_version(&miss); // mismatch -> miss
                monoasm_arm64!(&mut self.jit, b done;); // match -> continue
                self.jit.bind_label(miss.clone());
                self.a64_call_recompile_specialized(
                    global_idx,
                    RecompileReason::ClassVersionGuardFailed,
                );
                monoasm_arm64!(&mut self.jit, b deopt;);
                self.jit.bind_label(done);
            }
            // Counter-gated specialized recompile-or-deopt point (mirrors x86
            // `recompile_and_deopt_specialized`).
            AsmInst::RecompileDeoptSpecialized { idx, deopt, reason } => {
                let global_idx = self.specialized_base + idx;
                let deopt = labels[deopt].clone();
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
                deferred_src,
                ..
            } => {
                let offset = store[callee_fid].get_offset();
                return self.a64_set_arguments_forwarded(callid, callee_fid, offset, deferred_src);
            }
            // Every other AsmInst variant is handled by the shared
            // `compile_asmir` dispatcher before reaching here, so the wildcard
            // is unreachable (mirrors x86 `compile_asmir_arch`'s wildcard).
            _ => unreachable!("handled by the shared compile_asmir dispatcher"),
        }
        true
    }
}

