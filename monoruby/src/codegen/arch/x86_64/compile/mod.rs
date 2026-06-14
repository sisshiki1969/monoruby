use super::*;

mod binary_op;
mod builtin;
mod constants;
mod defined;
mod definition;
mod index;
mod init_method;
mod method_call;
mod variables;

use super::compile_shared::{extend_ivar, unreachable};
use crate::codegen::jitgen::lir::{LAluOp, LCond, LInst, LMem, LOperand, LReg};

/// Resolve a LIR register operand to its x86 register number. The scratch
/// pointer is `rdx`.
fn x86_lreg(r: LReg) -> u64 {
    match r {
        LReg::Gp(g) => g as u64,
        LReg::Scratch => GP::Rdx as u64,
    }
}

impl Codegen {
    ///
    /// Generate machine code for *inst*.
    ///
    ///
    /// Per-arch (x86-64) lowering for every `AsmInst` not handled by the
    /// arch-neutral `compile_asmir` dispatcher. Always emits (returns `true`).
    ///
    pub(in crate::codegen::jitgen) fn compile_asmir_arch(
        &mut self,
        store: &Store,
        _frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        class_version: DestLabel,
    ) -> bool {
        match inst {
            // Handled by the arch-neutral `compile_asmir` dispatcher.
            AsmInst::BcIndex(..)
            | AsmInst::Label(..)
            | AsmInst::RegMove(..)
            | AsmInst::RegToAcc(..)
            | AsmInst::AccToStack(..)
            | AsmInst::RegToStack(..)
            | AsmInst::StackToReg(..)
            | AsmInst::LitToReg(..)
            | AsmInst::LitToStack(..)
            | AsmInst::CondBr(..)
            | AsmInst::NilBr(..)
            | AsmInst::CheckLocal(..)
            | AsmInst::OptCase { .. }
            | AsmInst::GuardClass(..)
            | AsmInst::Deopt(..)
            | AsmInst::HandleError(..)
            | AsmInst::CheckStack { .. }
            | AsmInst::ExecGc { .. }
            | AsmInst::GuardConstBaseClass { .. }
            | AsmInst::GuardConstVersion { .. }
            | AsmInst::StoreConstant { .. }
            | AsmInst::LoadGVar { .. }
            | AsmInst::StoreGVar { .. }
            | AsmInst::LoadCVar { .. }
            | AsmInst::LoadDynVar { .. }
            | AsmInst::StoreDynVar { .. }
            | AsmInst::CreateArray { .. }
            | AsmInst::NewArray { .. }
            | AsmInst::NewHash(..)
            | AsmInst::HashInsert { .. }
            | AsmInst::ArrayConcat { .. }
            | AsmInst::NewRange { .. }
            | AsmInst::ConcatStr { .. }
            | AsmInst::ToA { .. }
            | AsmInst::DeepCopyLit(..)
            | AsmInst::FprMove(..)
            | AsmInst::FprSwap(..)
            | AsmInst::F64ToFpr(..)
            | AsmInst::FixnumToFpr(..)
            | AsmInst::FloatToFpr(..)
            | AsmInst::FprToStack(..)
            | AsmInst::XmmSave(..)
            | AsmInst::XmmRestore(..)
            | AsmInst::IntegerBinOp { .. }
            | AsmInst::IntegerCmp { .. }
            | AsmInst::IntegerCmpBr { .. }
            | AsmInst::FloatBinOp { .. }
            | AsmInst::FloatUnOp { .. }
            | AsmInst::I64ToBoth(..)
            | AsmInst::FloatCmp { .. }
            | AsmInst::FloatCmpBr { .. }
            | AsmInst::Ret
            | AsmInst::MethodRet(..)
            | AsmInst::BlockBreak(..)
            | AsmInst::ImmediateEvict { .. }
            | AsmInst::GuardClassVersion { .. }
            | AsmInst::SetupMethodFrame { .. }
            | AsmInst::SetArguments { .. }
            | AsmInst::CheckBOP { .. }
            | AsmInst::RecompileDeopt { .. }
            | AsmInst::Call { .. }
            | AsmInst::Init { .. }
            | AsmInst::Preparation
            | AsmInst::FixnumNeg { .. }
            | AsmInst::FixnumBitNot { .. }
            | AsmInst::GuardArrayTy(..)
            | AsmInst::GuardFrozen { .. }
            | AsmInst::LoadIVarInline { .. }
            | AsmInst::StoreIVarInline { .. }
            | AsmInst::LoadStructSlotInline { .. }
            | AsmInst::StoreStructSlotInline { .. }
            | AsmInst::LoadStructSlotHeap { .. }
            | AsmInst::StoreStructSlotHeap { .. }
            | AsmInst::RegAdd(..)
            | AsmInst::RegSub(..)
            | AsmInst::RegToRSPOffset(..)
            | AsmInst::ZeroToRSPOffset(..)
            | AsmInst::U64ToRSPOffset(..)
            | AsmInst::GuardCapture(..)
            | AsmInst::BlockArgProxy { .. }
            | AsmInst::BlockArg { .. }
            | AsmInst::LoopJitRspBump { .. }
            | AsmInst::StoreSelfIVarHeap { .. }
            | AsmInst::StoreIVarHeap { .. }
            | AsmInst::LoadIVarHeap { .. }
            | AsmInst::UndefMethod { .. }
            | AsmInst::AliasGvar { .. }
            | AsmInst::CheckCVar { .. }
            | AsmInst::StoreCVar { .. }
            | AsmInst::AliasMethod { .. }
            | AsmInst::DefinedYield { .. }
            | AsmInst::DefinedConst { .. }
            | AsmInst::DefinedMethod { .. }
            | AsmInst::DefinedSuper { .. }
            | AsmInst::DefinedGvar { .. }
            | AsmInst::DefinedIvar { .. }
            | AsmInst::DefinedCvar { .. }
            | AsmInst::GenericBinOp { .. }
            | AsmInst::ArrayTEq { .. }
            | AsmInst::ConcatRegexp { .. }
            | AsmInst::CheckKwRest(..)
            | AsmInst::ExpandArray { .. }
            | AsmInst::OptEqCmp { .. }
            | AsmInst::CFunc_F_F { .. }
            | AsmInst::CFunc_FF_F { .. }
            | AsmInst::MethodDef { .. }
            | AsmInst::SingletonMethodDef { .. }
            | AsmInst::Raise
            | AsmInst::Retry(..)
            | AsmInst::Redo(..)
            | AsmInst::EnsureEnd
            | AsmInst::Yield { .. }
            | AsmInst::MethodRetSpecialized { .. }
            | AsmInst::BlockBreakSpecialized { .. }
            | AsmInst::SetupYieldFrame { .. }
            | AsmInst::SpecializedCall { .. }
            | AsmInst::SpecializedYield { .. }
            | AsmInst::LoadDynVarSpecialized { .. }
            | AsmInst::StoreDynVarSpecialized { .. }
            | AsmInst::Inline(..)
            | AsmInst::ClassDef { .. }
            | AsmInst::SingletonClassDef { .. }
            | AsmInst::SetArgumentsForwardedHelper { .. }
            | AsmInst::Unreachable
            | AsmInst::RestKw { .. } => {
                unreachable!("handled by the shared compile_asmir dispatcher")
            }
            AsmInst::GuardClassVersionSpecialized { idx, deopt } => {
                let deopt = &labels[deopt];
                self.guard_class_version_specialized(
                    class_version,
                    self.specialized_base + idx,
                    deopt,
                );
            }
            AsmInst::RecompileDeoptSpecialized { idx, deopt, reason } => {
                let deopt = &labels[deopt];
                self.recompile_and_deopt_specialized(deopt, self.specialized_base + idx, reason)
            }
            AsmInst::SetArgumentsForwarded {
                callid,
                callee_fid,
                recv,
                args,
                lead_num,
                kwrest_guard,
                deferred_src,
            } => {
                let offset = store[callee_fid].get_offset();
                // gate guarantees req_num() >= lead_num
                let expected_len = store[callee_fid].req_num() - lead_num;
                self.jit_set_arguments_forwarded(
                    callid,
                    callee_fid,
                    offset,
                    args,
                    lead_num,
                    expected_len,
                    recv,
                    kwrest_guard,
                    deferred_src,
                );
            }
        }
        true
    }

    // ---- emission primitives (x86-64) -------------------------------------
    // Tiny arch-specific helpers the arch-neutral `compile_asmir` dispatcher
    // calls. The aarch64 twins live in `arch/aarch64/compile.rs`.

    /// Trap for statically-unreachable code: call the panicking helper.
    pub(in crate::codegen::jitgen) fn emit_unreachable(&mut self) {
        monoasm!( &mut self.jit,
            movq rax, (unreachable);
            call rax;
        );
    }

    /// `**kwrest` fixup: build a const table of (name, slot) pairs and call
    /// `correct_rest_kw(&table, lfp) -> kwrest Hash`.
    pub(in crate::codegen::jitgen) fn emit_rest_kw(&mut self, rest_kw: Vec<(SlotId, IdentId)>) {
        let data = self.jit.const_align8();
        for (i, name) in rest_kw.into_iter() {
            self.jit.const_i32(name.get() as i32);
            self.jit.const_i32(i.0 as i32);
        }
        self.jit.const_i32(0);
        self.jit.const_i32(0);

        monoasm!( &mut self.jit,
            lea  rdi, [rip + data];
            movq rsi, r14;
            movq rax, (runtime::correct_rest_kw);
            call rax;
        );
    }

    /// dst <- src (general-purpose register move; self-move is a no-op).
    pub(in crate::codegen::jitgen) fn emit_reg_move(&mut self, src: GP, dst: GP) {
        self.encode_linst(LInst::Mov { dst, src });
    }

    ///
    /// Per-arch (x86-64) LIR encoder seam (Phase-1 Stage 2).
    ///
    /// Lower one already-register-allocated `LInst` to machine code via
    /// `monoasm!`, emitting byte-identical output to the hand-written `emit_*`
    /// primitive it replaces. Only the migrated families are implemented; the
    /// rest `todo!()` until their `AsmInst` family is ported onto LIR. See
    /// `doc/lir.md`.
    ///
    pub(in crate::codegen::jitgen) fn encode_linst(&mut self, inst: LInst) {
        match inst {
            // dst <- src (elided when src == dst)
            LInst::Mov { dst, src } => {
                if src != dst {
                    let (src, dst) = (src as u64, dst as u64);
                    monoasm!( &mut self.jit,
                        movq R(dst), R(src);
                    );
                }
            }
            // dst <- imm (full 64-bit immediate; x86 movq r64, imm64)
            LInst::LoadImm { dst, imm } => {
                let r = dst as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (imm);
                );
            }
            // dst <- [lfp - slot]
            LInst::Load {
                dst,
                mem: LMem::Slot(slot),
            } => {
                let r = x86_lreg(dst);
                monoasm!( &mut self.jit,
                    movq R(r), [rbp - (rbp_local(slot))];
                );
            }
            // dst <- [base + disp] (object field; no immediate-range limit on x86)
            LInst::Load {
                dst,
                mem: LMem::Field { base, disp },
            } => {
                let (d, b) = (x86_lreg(dst), x86_lreg(base));
                monoasm!( &mut self.jit,
                    movq R(d), [R(b) + (disp)];
                );
            }
            // [lfp - slot] <- src
            LInst::Store {
                src,
                mem: LMem::Slot(slot),
            } => {
                let r = src as u64;
                monoasm!( &mut self.jit,
                    movq [rbp - (rbp_local(slot))], R(r);
                );
            }
            // [base + disp] <- src (object field; no immediate-range limit on x86)
            LInst::Store {
                src,
                mem: LMem::Field { base, disp },
            } => {
                let (s, b) = (src as u64, x86_lreg(base));
                monoasm!( &mut self.jit,
                    movq [R(b) + (disp)], R(s);
                );
            }
            // [rsp + (disp - RSP_LOCAL_FRAME)] <- src (callee-frame arg slot)
            LInst::Store {
                src,
                mem: LMem::RspRel { disp },
            } => {
                let s = src as u64;
                monoasm!( &mut self.jit,
                    movq [rsp + (disp - RSP_LOCAL_FRAME)], R(s);
                );
            }
            // [lfp - slot] <- imm. Legalization: a 64-bit immediate that does
            // not fit x86's imm32 store form is staged through rax (mirrors
            // `literal_to_stack`).
            LInst::StoreImm {
                imm,
                mem: LMem::Slot(slot),
            } => {
                if i32::try_from(imm as i64).is_ok() {
                    monoasm!( &mut self.jit,
                        movq [rbp - (rbp_local(slot))], (imm);
                    );
                } else {
                    monoasm!( &mut self.jit,
                        movq rax, (imm);
                        movq [rbp - (rbp_local(slot))], rax;
                    );
                }
            }
            // [rsp + (disp - RSP_LOCAL_FRAME)] <- imm (callee-frame arg slot)
            LInst::StoreImm {
                imm,
                mem: LMem::RspRel { disp },
            } => {
                monoasm!( &mut self.jit,
                    movq [rsp + (disp - RSP_LOCAL_FRAME)], (imm);
                );
            }
            // dst <op>= imm (in-place register/immediate ALU; the only Alu
            // shape produced so far, from RegAdd/RegSub). No-op when imm == 0.
            LInst::Alu {
                op,
                dst,
                lhs,
                rhs: LOperand::Imm(i),
            } if dst == lhs => {
                if i != 0 {
                    let r = dst as u64;
                    let imm = i as i32;
                    match op {
                        LAluOp::Add => monoasm! { &mut self.jit, addq R(r), (imm); },
                        LAluOp::Sub => monoasm! { &mut self.jit, subq R(r), (imm); },
                        _ => todo!(
                            "LIR encode (x86-64): Alu {op:?} imm not yet migrated (Phase-1 Stage > 2-C)"
                        ),
                    }
                }
            }
            // Set flags from `lhs - rhs`. An `Imm` is the operand's raw bit
            // pattern (a tagged fixnum), passed as u64 so the encoding matches
            // the hand-written `cmp_integer`.
            LInst::Cmp { lhs, rhs } => {
                let l = lhs as u64;
                match rhs {
                    LOperand::Reg(r) => monoasm! { &mut self.jit, cmpq R(l), R(r as u64); },
                    LOperand::Imm(i) => monoasm! { &mut self.jit, cmpq R(l), (i as u64); },
                }
            }
            // Signed conditional branch on the preceding `Cmp` (mirrors
            // `condbr_int`; the BrKind inversion is folded into `cond` by the
            // builder).
            LInst::CondBr { cond, target } => match cond {
                LCond::Eq => monoasm! { &mut self.jit, jeq target; },
                LCond::Ne => monoasm! { &mut self.jit, jne target; },
                LCond::Lt => monoasm! { &mut self.jit, jlt target; },
                LCond::Le => monoasm! { &mut self.jit, jle target; },
                LCond::Gt => monoasm! { &mut self.jit, jgt target; },
                LCond::Ge => monoasm! { &mut self.jit, jge target; },
            },
            // Ruby-truthiness branch: `orq 0x10` folds nil(0x04)/false(0x14) to
            // FALSE_VALUE; truthy (non-FALSE) takes jnz, falsy takes jz.
            LInst::BranchTruthy { negate, target } => {
                monoasm! { &mut self.jit,
                    orq  rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                };
                if negate {
                    monoasm! { &mut self.jit, jz  target; }
                } else {
                    monoasm! { &mut self.jit, jnz target; }
                }
            }
            LInst::BranchIfNil { target } => {
                monoasm! { &mut self.jit,
                    cmpq rax, (NIL_VALUE);
                    jeq  target;
                }
            }
            LInst::BranchIfNonzero { target } => {
                monoasm! { &mut self.jit,
                    testq rax, rax;
                    jnz  target;
                }
            }
            // GC write barrier (parent is fixed in rdi on x86).
            LInst::WriteBarrier { parent, value } => {
                debug_assert_eq!(parent, GP::Rdi, "x86 write barrier expects parent in rdi");
                self.emit_write_barrier_rdi(value);
            }
            // reg <- nil if reg == 0 (x86: branch over the nil mov).
            LInst::NilIfZero { reg } => {
                let r = reg as u64;
                let skip = self.jit.label();
                monoasm! { &mut self.jit,
                    testq R(r), R(r);
                    jne  skip;
                    movq R(r), (NIL_VALUE);
                skip:
                }
            }
            // Type / class guards: deopt (jump to the side-exit) on a mismatch.
            LInst::GuardClass { reg, class, deopt } => self.guard_class(reg, class, &deopt),
            LInst::GuardArrayTy { reg, deopt } => self.guard_array_ty(reg, &deopt),
            LInst::GuardFrozen { deopt } => self.guard_frozen(&deopt),
            // Constant-load base-class guard: deopt unless the accumulator equals
            // the cached base class.
            LInst::GuardConstBaseClass { base_class, deopt } => {
                let cached_base_class = self.jit.const_i64(base_class.id() as _);
                monoasm! { &mut self.jit,
                    cmpq rax, [rip + cached_base_class];
                    jne  deopt;
                }
            }
            LInst::GuardConstVersion { const_version, deopt } => {
                self.guard_const_version(const_version, &deopt);
            }
            // Fixnum fast-path arithmetic with an overflow deopt.
            LInst::IntegerBinOp {
                kind,
                mode,
                lhs,
                rhs,
                deopt,
            } => {
                self.integer_binop(lhs, rhs, &mode, kind, &deopt);
            }
            LInst::GuardCapture { deopt } => self.guard_capture(&deopt),
            // BOP-redefinition guard: outline the deopt path (page 1) so the hot
            // path is a single load + branch.
            LInst::CheckBOP { deopt } => {
                let bop_flag = self.bop_redefined_flags.clone();
                let l1 = self.jit.label();
                assert_eq!(0, self.jit.get_page());
                monoasm!(
                    &mut self.jit,
                    cmpl [rip + bop_flag], 0;
                    jnz l1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                l1:
                    movq rdi, (Value::symbol_from_str("_bop_guard").id());
                    jmp  deopt;
                );
                self.jit.select_page(0);
            }
            other => {
                todo!("LIR encode (x86-64): {other:?} not yet migrated (Phase-1 Stage > 2-A)")
            }
        }
    }

    /// [lfp - slot] <- reg
    pub(in crate::codegen::jitgen) fn emit_reg_to_stack(&mut self, r: GP, slot: SlotId) {
        self.encode_linst(LInst::Store {
            src: r,
            mem: LMem::Slot(slot),
        });
    }

    /// reg <- [lfp - slot]
    pub(in crate::codegen::jitgen) fn emit_stack_to_reg(&mut self, slot: SlotId, r: GP) {
        self.encode_linst(LInst::Load {
            dst: r.into(),
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

    /// [lfp - slot] <- literal Value. Always succeeds on x86 (no immediate-range
    /// limit); the bool result exists for the aarch64 twin.
    pub(in crate::codegen::jitgen) fn emit_lit_to_stack(&mut self, v: Value, slot: SlotId) -> bool {
        self.encode_linst(LInst::StoreImm {
            imm: v.id(),
            mem: LMem::Slot(slot),
        });
        true
    }

    /// Unconditional jump to a side-exit (deopt) label.
    pub(in crate::codegen::jitgen) fn emit_deopt(&mut self, deopt: &DestLabel) {
        monoasm!( &mut self.jit,
            jmp deopt;
        );
    }

    /// Branch to the error handler if the accumulator (rax) is null (the
    /// preceding runtime call failed).
    pub(in crate::codegen::jitgen) fn emit_handle_error(&mut self, error: &DestLabel) {
        self.handle_error(error);
    }

    /// Stack-overflow check. Always succeeds on x86 (the bool result exists for
    /// the aarch64 twin, which bails on an unsupported write-back).
    pub(in crate::codegen::jitgen) fn emit_check_stack(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        base: usize,
    ) -> bool {
        self.jit_check_stack(&write_back, error, base);
        true
    }

    /// GC safepoint. Always succeeds on x86 (see `emit_check_stack`).
    pub(in crate::codegen::jitgen) fn emit_exec_gc(
        &mut self,
        write_back: WriteBack,
        error: &DestLabel,
        base: usize,
    ) -> bool {
        self.jit_execute_gc(&write_back, error, base);
        true
    }

    /// Store the accumulator to a constant and bump the global constant
    /// version. Always succeeds on x86 (the bool result exists for the aarch64
    /// twin, which bails if any xmm is live).
    pub(in crate::codegen::jitgen) fn emit_store_constant(
        &mut self,
        id: ConstSiteId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        self.store_constant(id, using_xmm);
        self.handle_error(error);
        true
    }

    // ---- variable-access primitives (x86-64) ------------------------------
    // All delegate to the existing helpers and always succeed (the bool result
    // exists for the aarch64 twins, which bail on a live xmm / range overflow).

    /// rax <- $gvar.
    pub(in crate::codegen::jitgen) fn emit_load_gvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.load_gvar(name, using_xmm);
        true
    }

    /// $gvar <- src.
    pub(in crate::codegen::jitgen) fn emit_store_gvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.store_gvar(name, src, using_xmm);
        true
    }

    /// rax <- @@cvar.
    pub(in crate::codegen::jitgen) fn emit_load_cvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.load_cvar(name, using_xmm);
        true
    }

    /// rax <- dynamic (outer-frame) local.
    pub(in crate::codegen::jitgen) fn emit_load_dyn_var(&mut self, src: DynVar) -> bool {
        self.load_dyn_var(src);
        true
    }

    /// dynamic (outer-frame) local <- src.
    pub(in crate::codegen::jitgen) fn emit_store_dyn_var(&mut self, dst: DynVar, src: GP) -> bool {
        self.store_dyn_var(dst, src);
        true
    }

    // ---- runtime allocation primitives (x86-64) ---------------------------
    // All build a heap object via a runtime call and always succeed (the bool
    // result exists for the aarch64 twins, which bail on a live xmm / range
    // overflow).

    /// rax <- Array of the `len` slots starting at `src`.
    pub(in crate::codegen::jitgen) fn emit_create_array(&mut self, src: SlotId, len: usize) -> bool {
        monoasm!( &mut self.jit,
            lea  rdi, [r14 - (conv(src))];
            movq rsi, (len);
            movq rax, (runtime::create_array);
            call rax;
        );
        true
    }

    /// rax <- Array literal (splat-aware) via the call site.
    pub(in crate::codegen::jitgen) fn emit_new_array(
        &mut self,
        callid: CallSiteId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.new_array(callid, using_xmm);
        true
    }

    /// rax <- Hash literal from the `len` key/value slots at `args`.
    pub(in crate::codegen::jitgen) fn emit_new_hash(
        &mut self,
        args: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    ) -> bool {
        self.new_hash(args, len, using_xmm);
        true
    }

    /// rax <- the Hash in `hash` after inserting the `len` key/value pairs
    /// at `args` (chunked Hash literal).
    pub(in crate::codegen::jitgen) fn emit_hash_insert(
        &mut self,
        hash: SlotId,
        args: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    ) -> bool {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rbp - (rbp_local(args))];
            movq rcx, (len);
            movq r8, [rbp - (rbp_local(hash))];
            movq rax, (runtime::hash_insert);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    /// rax <- the Array in `dst` after concatenating the Array in `src`
    /// (chunked Array literal).
    pub(in crate::codegen::jitgen) fn emit_array_concat(
        &mut self,
        dst: SlotId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - (rbp_local(dst))];
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::array_concat);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    /// rax <- Range(start, end, exclude_end).
    pub(in crate::codegen::jitgen) fn emit_new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
    ) -> bool {
        self.load_rdi(start);
        self.load_rsi(end);
        self.new_range(exclude_end, using_xmm);
        true
    }

    /// rax <- the `len` slots at `arg` concatenated into a String.
    pub(in crate::codegen::jitgen) fn emit_concat_str(
        &mut self,
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    ) -> bool {
        self.concat_string(arg, len, using_xmm);
        true
    }

    /// rax <- `src` coerced to an Array (`Array(x)` / splat).
    pub(in crate::codegen::jitgen) fn emit_to_a(&mut self, src: SlotId, using_xmm: UsingXmm) -> bool {
        self.to_a(src, using_xmm);
        true
    }

    /// rax <- a deep copy of literal `v` (fresh mutable object per evaluation).
    pub(in crate::codegen::jitgen) fn emit_deep_copy_lit(
        &mut self,
        v: Value,
        using_xmm: UsingXmm,
    ) -> bool {
        self.deepcopy_literal(v, using_xmm);
        true
    }

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

    ///
    /// Get method lfp.
    ///
    /// ### in
    /// - r14: lfp
    ///
    /// ### out
    /// - rax: method lfp
    ///
    fn get_method_lfp(&mut self, outer: usize) {
        if outer == 0 {
            monoasm! { &mut self.jit,
                movq rax, r14;
            };
        } else {
            monoasm!( &mut self.jit,
                movq rax, [r14];
            );
            for _ in 0..outer - 1 {
                monoasm!( &mut self.jit,
                    movq rax, [rax];
                );
            }
        }
    }

    ///
    /// Compare `lhs and `rhs` with "===" and return the result in rax.
    ///
    /// If `lhs` is Array, compare `rhs` and each element of `lhs`.
    ///
    fn array_teq(&mut self, lhs: SlotId, rhs: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        self.load_rdx(lhs);
        self.load_rcx(rhs);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::array_teq);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// Call a generic `BinaryOpFn` C helper with no receiver-class
    /// guard. Mirrors the VM's `call_binop` calling convention
    /// (rdi=&Executor, rsi=&Globals, rdx=lhs, rcx=rhs); result
    /// `Option<Value>` in rax.
    ///
    fn generic_binop(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) {
        self.xmm_save(using_xmm);
        self.load_rdx(lhs);
        self.load_rcx(rhs);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// `==` / `!=` with an inline immediate fast path.
    ///
    /// `rdx = lhs`, `rcx = rhs`. If BOTH are non-heap, non-flonum
    /// immediates (Fixnum / nil / true / false / Symbol) the Ruby
    /// `==`/`!=` result is exactly bit (identity) equality, so it is
    /// produced inline. Float (`-0.0`/`0.0`, `NaN`), heap (`String`
    /// content, custom `==`), `BigInt`, and mixed numeric all fall
    /// through to the generic `cmp_*_values` C-call, which is
    /// correct for them. No receiver-class guard.
    ///
    /// ### out
    /// - rax: bool `Value` (fast path) or `Option<Value>` (slow)
    ///
    fn opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) {
        self.load_rdx(lhs);
        self.load_rcx(rhs);
        let slow = self.jit.label();
        let done = self.jit.label();
        // Heap iff (bits & 0b111) == 0; Flonum iff (bits & 0b011) == 0b010.
        // Either operand heap/flonum -> generic C-call.
        monoasm!( &mut self.jit,
            movq rax, rdx;
            andq rax, 0b111;
            jz   slow;
            movq rax, rdx;
            andq rax, 0b011;
            cmpq rax, 0b010;
            jeq  slow;
            movq rax, rcx;
            andq rax, 0b111;
            jz   slow;
            movq rax, rcx;
            andq rax, 0b011;
            cmpq rax, 0b010;
            jeq  slow;
            // both identity-comparable immediates: result = bit-eq
            xorq rax, rax;
            cmpq rdx, rcx;
        );
        match kind {
            CmpKind::Eq => self.set_eq(),
            CmpKind::Ne => self.set_ne(),
            _ => unreachable!("opt_eq_cmp only handles Eq/Ne"),
        }
        monoasm!( &mut self.jit,
            jmp  done;
        slow:
        );
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
        self.xmm_restore(using_xmm);
        monoasm!( &mut self.jit,
        done:
        );
    }

    ///
    /// Generate new Array object according to `callid`.
    ///
    /// ### out
    ///
    /// - rax: result Option<Value>
    ///
    /// ### destroy
    ///
    /// - caller save registers
    ///
    fn new_array(&mut self, callid: CallSiteId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movl rdx, (callid.get());
            lea  rcx, [r14 - (LFP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_array);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rbp - (rbp_local(args))];
            movq rcx, (len);
            movq rax, (runtime::gen_hash);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_range(&mut self, exclude_end: bool, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rbx; // &mut Executor
            movq rcx, r12; // &mut Globals
            movl r8, (exclude_end as u32);
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    fn concat_string(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [rbp - (rbp_local(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn to_a(&mut self, src: SlotId, using_xmm: UsingXmm) {
        let toa = self.jit.label();
        let exit = self.jit.label();
        monoasm!( &mut self.jit,
            movq rax, [rbp - (rbp_local(src))];
        );
        self.guard_rvalue(GP::Rax, ARRAY_CLASS, &toa);
        self.bind_label(exit.clone());

        self.select_page(1);
        self.bind_label(toa);
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rax;
            movq rax, (runtime::to_a);
            call rax;
        );
        self.xmm_restore(using_xmm);
        monoasm!( &mut self.jit,
            jmp  exit;
        );
        self.select_page(0);
    }

    fn concat_regexp(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [rbp - (rbp_local(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_regexp);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// Get block handler of a current method frame.
    ///
    /// ### in
    /// - rax: method lfp
    ///
    /// ### out
    /// - rax: block handler
    ///
    fn block_arg_proxy(&mut self, outer: usize) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rax - (LFP_BLOCK)];
            testq rax, 0b1;
            jeq exit;
            addq rax, ((outer << 2) + 2);
        exit:
        };
    }

    ///
    /// Get a block argument of current frame.
    ///
    fn block_arg(&mut self, using_xmm: UsingXmm, call_site_bc_ptr: BytecodePtr) {
        let call_site_ptr_val = call_site_bc_ptr.as_ptr() as u64;
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, r14;
            movq rdi, rbx;
            movq rsi, r12;
            movq rcx, (call_site_ptr_val);
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    ///
    /// Set self, req, opt and rest arguments on the callee frame.
    ///
    /// ### out
    /// - rax: None for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn jit_set_arguments(&mut self, callid: CallSiteId, fid: FuncId, offset: usize) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());
            lea  rcx, [rsp - (RSP_LOCAL_FRAME)];   // callee_lfp
            movl r8, (fid.get());
            subq rsp, (offset);
            movq rax, (crate::runtime::jit_generic_set_arguments);
            call rax;
            addq rsp, (offset);
        }
    }

    ///
    /// Same proven asm shape as `jit_set_arguments`, but dispatches to
    /// the specialized `jit_forwarded_set_arguments` runtime helper.
    ///
    /// ### out
    /// - rax: None for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(in crate::codegen::jitgen) fn jit_set_arguments_forwarded_helper(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
    ) -> bool {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());
            lea  rcx, [rsp - (RSP_LOCAL_FRAME)];   // callee_lfp
            movl r8, (fid.get());
            subq rsp, (offset);
            movq rax, (crate::runtime::jit_forwarded_set_arguments);
            call rax;
            addq rsp, (offset);
        }
        true
    }

    ///
    /// Spill-aware xmm-to-xmm move. Each operand may live in a phys
    /// xmm or a spill slot; we emit the cheapest form for each
    /// combination and avoid the round-trip through xmm0 that the
    /// generic `expand_spills` wrapping would otherwise produce.
    ///
    /// dst(f64) <- src. Spill-aware. Always succeeds on x86 (the bool result
    /// exists for the aarch64 twin, which bails on an unlowerable FP register).
    pub(in crate::codegen::jitgen) fn emit_fpr_move(
        &mut self,
        src: FPReg,
        dst: FPReg,
        base: usize,
    ) -> bool {
        if src == dst {
            return true;
        }
        match (src.loc(base), dst.loc(base)) {
            (FPRegLoc::Xmm(s), FPRegLoc::Xmm(d)) => monoasm!( &mut self.jit,
                movq xmm(d), xmm(s);
            ),
            (FPRegLoc::Xmm(s), FPRegLoc::Spill(d_off)) => monoasm!( &mut self.jit,
                movq [rbp - (d_off)], xmm(s);
            ),
            (FPRegLoc::Spill(s_off), FPRegLoc::Xmm(d)) => monoasm!( &mut self.jit,
                movq xmm(d), [rbp - (s_off)];
            ),
            (FPRegLoc::Spill(s_off), FPRegLoc::Spill(d_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (s_off)];
                movq [rbp - (d_off)], xmm0;
            ),
        }
        true
    }

    ///
    /// Spill-aware xmm swap. When both operands are spilled we swap
    /// the two memory slots through xmm0+xmm1 (avoiding rax/rcx so
    /// nothing in the surrounding code's GP state is disturbed).
    ///
    pub(in crate::codegen::jitgen) fn emit_fpr_swap(
        &mut self,
        l: FPReg,
        r: FPReg,
        base: usize,
    ) -> bool {
        if l == r {
            return true;
        }
        match (l.loc(base), r.loc(base)) {
            (FPRegLoc::Xmm(lp), FPRegLoc::Xmm(rp)) => monoasm!( &mut self.jit,
                movq xmm0, xmm(lp);
                movq xmm(lp), xmm(rp);
                movq xmm(rp), xmm0;
            ),
            (FPRegLoc::Xmm(lp), FPRegLoc::Spill(r_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (r_off)];
                movq [rbp - (r_off)], xmm(lp);
                movq xmm(lp), xmm0;
            ),
            (FPRegLoc::Spill(l_off), FPRegLoc::Xmm(rp)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                movq [rbp - (l_off)], xmm(rp);
                movq xmm(rp), xmm0;
            ),
            (FPRegLoc::Spill(l_off), FPRegLoc::Spill(r_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                movq xmm1, [rbp - (r_off)];
                movq [rbp - (r_off)], xmm0;
                movq [rbp - (l_off)], xmm1;
            ),
        }
        true
    }

    /// xmm(x) <- f64 constant `f`. Spill-aware.
    pub(in crate::codegen::jitgen) fn emit_f64_to_fpr(&mut self, f: f64, x: FPReg, base: usize) -> bool {
        let f_const = self.jit.const_f64(f);
        match x.loc(base) {
            FPRegLoc::Xmm(p) => monoasm!( &mut self.jit,
                movq xmm(p), [rip + f_const];
            ),
            FPRegLoc::Spill(off) => monoasm!( &mut self.jit,
                movq xmm0, [rip + f_const];
                movq [rbp - (off)], xmm0;
            ),
        }
        true
    }

    /// xmm(x) <- the fixnum in GP `r`, converted to f64. Spill-aware.
    pub(in crate::codegen::jitgen) fn emit_fixnum_to_fpr(&mut self, r: GP, x: FPReg, base: usize) -> bool {
        let (work, spill_off) = match x.loc(base) {
            FPRegLoc::Xmm(p) => (p, None),
            FPRegLoc::Spill(off) => (0u64, Some(off)),
        };
        self.integer_val_to_f64(r, work);
        if let Some(off) = spill_off {
            monoasm!( &mut self.jit,
                movq [rbp - (off)], xmm(work);
            );
        }
        true
    }

    /// xmm(x) <- the Float Value in GP `reg`, decoded to f64; deopt if `reg` is
    /// not a Float. Spill-aware.
    pub(in crate::codegen::jitgen) fn emit_float_to_fpr(
        &mut self,
        reg: GP,
        x: FPReg,
        deopt: &DestLabel,
        base: usize,
    ) -> bool {
        let (work, spill_off) = match x.loc(base) {
            FPRegLoc::Xmm(p) => (p, None),
            FPRegLoc::Spill(off) => (0u64, Some(off)),
        };
        self.float_to_f64(reg, work, deopt);
        if let Some(off) = spill_off {
            monoasm!( &mut self.jit,
                movq [rbp - (off)], xmm(work);
            );
        }
        true
    }

    /// [slot] <- box(xmm(x)) (flonum-encode or heap-allocate the f64).
    pub(in crate::codegen::jitgen) fn emit_fpr_to_stack(&mut self, x: FPReg, slot: SlotId, base: usize) -> bool {
        self.fpr_to_stack(x, &[slot], base);
        true
    }

    /// Save the live FP pool registers before a C-call. Always succeeds on x86
    /// (the bool result mirrors the aarch64 twin).
    pub(in crate::codegen::jitgen) fn emit_xmm_save(&mut self, using_xmm: UsingXmm, cont: bool) -> bool {
        self.xmm_save_with_cont(using_xmm, cont);
        true
    }

    /// Restore the live FP pool registers after a C-call.
    pub(in crate::codegen::jitgen) fn emit_xmm_restore(&mut self, using_xmm: UsingXmm, cont: bool) -> bool {
        self.xmm_restore_with_cont(using_xmm, cont);
        true
    }

    /// Integer comparison; result Value lands in the accumulator.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp(
        &mut self,
        kind: CmpKind,
        mode: OpMode,
        lhs: GP,
        rhs: GP,
    ) -> bool {
        self.integer_cmp(kind, mode, lhs, rhs);
        true
    }


    /// Float (four-arithmetic) binary op: dst <- lhs <op> rhs in FP registers.
    pub(in crate::codegen::jitgen) fn emit_float_binop(
        &mut self,
        kind: BinOpK,
        binary_xmm: (FPReg, FPReg),
        dst: FPReg,
        base: usize,
    ) -> bool {
        self.float_binop(kind, dst, binary_xmm, base);
        true
    }

    /// Float unary op: negate (flip the sign bit) or unary-plus (no-op).
    pub(in crate::codegen::jitgen) fn emit_float_unop(&mut self, kind: UnOpK, dst: FPReg, base: usize) -> bool {
        match kind {
            UnOpK::Neg => {
                let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                match dst.loc(base) {
                    FPRegLoc::Xmm(p) => monoasm!( &mut self.jit,
                        xorps xmm(p), [rip + imm];
                    ),
                    FPRegLoc::Spill(off) => monoasm!( &mut self.jit,
                        movq  xmm0, [rbp - (off)];
                        xorps xmm0, [rip + imm];
                        movq  [rbp - (off)], xmm0;
                    ),
                }
            }
            UnOpK::Pos => {}
            _ => unreachable!(),
        }
        true
    }

    /// [slot] <- box(i) (integer Value) and fpr(x) <- i as f64.
    pub(in crate::codegen::jitgen) fn emit_i64_to_both(&mut self, i: i64, slot: SlotId, x: FPReg, base: usize) -> bool {
        let f = self.jit.const_f64(i as f64);
        monoasm! {&mut self.jit,
            movq [rbp - (rbp_local(slot))], (Value::integer(i).id());
        }
        match x.loc(base) {
            FPRegLoc::Xmm(p) => monoasm!( &mut self.jit,
                movq xmm(p), [rip + f];
            ),
            FPRegLoc::Spill(off) => monoasm!( &mut self.jit,
                movq xmm0, [rip + f];
                movq [rbp - (off)], xmm0;
            ),
        }
        true
    }

    /// Float comparison; NaN-correct boolean Value lands in the accumulator.
    pub(in crate::codegen::jitgen) fn emit_float_cmp(&mut self, kind: CmpKind, lhs: FPReg, rhs: FPReg, base: usize) -> bool {
        monoasm! { &mut self.jit,
            xorq rax, rax;
        };
        self.cmp_float((lhs, rhs), base);
        self.setflag_float(kind);
        true
    }

    /// Fused float compare + conditional branch (NaN compares false except `!=`).
    pub(in crate::codegen::jitgen) fn emit_float_cmp_br(
        &mut self,
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
        brkind: BrKind,
        branch_dest: DestLabel,
        base: usize,
    ) -> bool {
        self.cmp_float((lhs, rhs), base);
        self.condbr_float(kind, branch_dest, brkind);
        true
    }

    /// Method epilogue: tear down the frame and return.
    pub(in crate::codegen::jitgen) fn emit_ret(&mut self) {
        self.epilogue();
    }

    /// Return through the method-return path, resuming the caller at `pc + 1`.
    pub(in crate::codegen::jitgen) fn emit_method_ret(&mut self, pc: BytecodePtr) {
        monoasm! { &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
        };
        self.method_return();
    }

    /// Non-local exit through the block-break path, resuming at `pc + 1`.
    pub(in crate::codegen::jitgen) fn emit_block_break(&mut self, pc: BytecodePtr) {
        monoasm! { &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
        };
        self.block_break();
    }

    /// Dense-integer `case` dispatch (cond fixnum in rdi). Build a jump table of
    /// absolute branch-target addresses, range-check `[min, max]`, then index
    /// it with `cond - min`.
    pub(in crate::codegen::jitgen) fn emit_opt_case(
        &mut self,
        frame: &mut AsmInfo,
        max: u16,
        min: u16,
        else_label: JitLabel,
        branch_labels: Box<[JitLabel]>,
    ) {
        // generate a jump table.
        let jump_table = self.jit.const_align8();
        for label in branch_labels.iter() {
            let dest_label = frame.resolve_label(&mut self.jit, *label);
            self.jit.abs_address(dest_label);
        }

        let else_dest = frame.resolve_label(&mut self.jit, else_label);
        monoasm! {&mut self.jit,
            sarq rdi, 1;
            cmpq rdi, (max);
            jgt  else_dest;
            subq rdi, (min);
            jlt  else_dest;
            lea  rax, [rip + jump_table];
            jmp  [rax + rdi * 8];
        };
    }

    /// Record this position as the return-address patch point for `evict`.
    pub(in crate::codegen::jitgen) fn emit_immediate_evict(&mut self, evict: AsmEvict) {
        let patch_point = self.jit.get_current_address();
        let return_addr = self.asm_return_addr_table.get(&evict).unwrap();
        self.return_addr_table
            .entry(*return_addr)
            .and_modify(|e| e.0 = Some(patch_point));
    }

    /// Inline-cache class-version guard: deopt if the global class version moved
    /// since compilation. `position`/`with_recovery` drive x86 recompilation.
    pub(in crate::codegen::jitgen) fn emit_guard_class_version(
        &mut self,
        class_version: DestLabel,
        position: Option<BytecodePtr>,
        with_recovery: bool,
        deopt: DestLabel,
    ) {
        self.guard_class_version(class_version, position, with_recovery, &deopt);
    }

    /// Write the callee frame's meta/outer/block fields before a call.
    pub(in crate::codegen::jitgen) fn emit_setup_method_frame(
        &mut self,
        store: &Store,
        meta: Meta,
        callid: CallSiteId,
        outer_lfp: Option<Lfp>,
    ) {
        self.setup_method_frame(store, meta, callid, outer_lfp);
    }

    /// Marshal the call arguments into the callee frame. Always succeeds on x86
    /// (the bool result mirrors the aarch64 twin, which bails on unsupported
    /// argument shapes).
    pub(in crate::codegen::jitgen) fn emit_set_arguments(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
    ) -> bool {
        let offset = store[callee_fid].get_offset();
        self.jit_set_arguments(callid, callee_fid, offset);
        true
    }

    /// Recompile-or-deopt: deopt now and schedule recompilation once the inline
    /// cache warms.
    pub(in crate::codegen::jitgen) fn emit_recompile_deopt(
        &mut self,
        position: Option<BytecodePtr>,
        deopt: &DestLabel,
        // x86 recompiles in place (no extern-boundary panic surfaced here), so
        // the aarch64-only error side-exit is unused.
        _error: Option<&DestLabel>,
        reason: RecompileReason,
    ) {
        self.recompile_and_deopt(position, deopt, reason);
    }

    /// The call itself: enter the callee and record a return-address deopt
    /// patch point for `evict`.
    pub(in crate::codegen::jitgen) fn emit_call(
        &mut self,
        store: &Store,
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
        evict_label: &DestLabel,
        pc: BytecodePtr,
    ) {
        let return_addr = self.do_call(store, callee_fid, recv_class, pc);
        self.set_deopt_with_return_addr(return_addr, evict, evict_label);
    }

    /// Method prologue. Always succeeds on x86 (the bool result mirrors the
    /// aarch64 twin, which bails on an over-large frame).
    pub(in crate::codegen::jitgen) fn emit_init(
        &mut self,
        info: FnInitInfo,
        prologue_offset: PrologueOffset,
    ) -> bool {
        self.init_func(&info, prologue_offset.unwrap_concrete());
        true
    }

    /// Per-method ivar-cache preparation: ensure the heap ivar table is large
    /// enough (extending it via a runtime call if not). No-op for frozen/
    /// inline-only selves. Always succeeds on x86.
    pub(in crate::codegen::jitgen) fn emit_preparation(&mut self, store: &Store, frame: &AsmInfo) -> bool {
        if !frame.self_class.is_always_frozen() && frame.ivar_heap_accessed {
            let ivar_len = store[frame.self_class].ivar_len();
            let heap_len = if frame.self_ty == Some(ObjTy::OBJECT) {
                ivar_len - OBJECT_INLINE_IVAR
            } else {
                ivar_len
            };
            let fail = self.jit.label();
            let exit = self.jit.label();
            monoasm!(&mut self.jit,
                movq rdi, [r14 - (LFP_SELF)];
                movq rsi, (heap_len);
                movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
                // check var_table is not None
                testq rdx, rdx;
                jz   fail;
                // check capa is not 0
                cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
                jz   fail;
                // check len >= heap_len
                cmpq [rdx + (MONOVEC_LEN)], rsi; // len
                jlt  fail;
            exit:
            );
            assert_eq!(0, self.jit.get_page());
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            fail:
                movq rax, (extend_ivar);
                call rax;
                jmp exit;
            );
            self.jit.select_page(0);
        }
        true
    }

    /// Fixnum negate (tagged): untag, negate, re-tag; deopt on i63 overflow.
    pub(in crate::codegen::jitgen) fn emit_fixnum_neg(&mut self, reg: GP, deopt: &DestLabel) {
        let r = reg as u64;
        monoasm! { &mut self.jit,
            sarq  R(r), 1;
            negq  R(r);
            jo    deopt;
            addq  R(r), R(r);
            jo    deopt;
            orq   R(r), 1;
        }
    }

    /// Fixnum bitwise-not (tagged): untag, complement, re-tag. Cannot overflow.
    pub(in crate::codegen::jitgen) fn emit_fixnum_bit_not(&mut self, reg: GP) {
        let r = reg as u64;
        monoasm! { &mut self.jit,
            sarq  R(r), 1;
            notq  R(r);
            salq  R(r), 1;
            orq   R(r), 1;
        }
    }

    /// reg += i (no-op when i == 0).
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

    /// Loop-JIT entry: reserve the loop body's spill area on the native stack.
    pub(in crate::codegen::jitgen) fn emit_loop_jit_rsp_bump(&mut self, offset: LoopRspOffset) -> bool {
        let bytes = offset.unwrap_concrete();
        if bytes > 0 {
            monoasm! { &mut self.jit, subq rsp, (bytes as i32); }
        }
        true
    }

    /// Load a heap-spilled instance variable into the accumulator, substituting
    /// nil for an out-of-range (non-self) or unset slot.
    pub(in crate::codegen::jitgen) fn emit_load_ivar_heap(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
    ) -> bool {
        self.load_ivar_heap(ivarid, is_object_ty, self_);
        true
    }

    /// `undef`-method via runtime::undef_method(vm, globals, id).
    pub(in crate::codegen::jitgen) fn emit_undef_method(&mut self, undef: IdentId, using_xmm: UsingXmm) -> bool {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (undef.get());
            movq rax, (runtime::undef_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    /// Alias a global var via runtime::alias_global_var(globals, new, old).
    pub(in crate::codegen::jitgen) fn emit_alias_gvar(&mut self, new: IdentId, old: IdentId, using_xmm: UsingXmm) -> bool {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, r12;          // &mut Globals
            movl rsi, (new.get());  // new IdentId
            movl rdx, (old.get());  // old IdentId
            movq rax, (runtime::alias_global_var);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    /// Check class-variable existence via runtime::check_class_var.
    pub(in crate::codegen::jitgen) fn emit_check_cvar(
        &mut self,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.check_cvar(name, using_xmm);
        true
    }

    /// @@cvar <- src via runtime::set_class_var.
    pub(in crate::codegen::jitgen) fn emit_store_cvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.store_cvar(name, src, using_xmm);
        true
    }

    /// Alias a method via runtime::alias_method (old/new read from frame slots).
    pub(in crate::codegen::jitgen) fn emit_alias_method(
        &mut self,
        new: SlotId,
        old: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (conv(old))];
            movq rcx, [r14 - (conv(new))];
            movq rax, (runtime::alias_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    // ---- defined? runtime-call family (delegate to the existing helpers) ----

    pub(in crate::codegen::jitgen) fn emit_defined_yield(
        &mut self,
        dst: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_yield(dst, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_super(
        &mut self,
        dst: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_super(dst, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_gvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_gvar(dst, name, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_cvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_cvar(dst, name, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_const(
        &mut self,
        dst: SlotId,
        siteid: ConstSiteId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_const(dst, siteid, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_method(
        &mut self,
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_method(dst, recv, name, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_ivar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.defined_ivar(dst, name, using_xmm);
        true
    }

    // ---- generic binary-op runtime calls (delegate to the helpers) ----

    pub(in crate::codegen::jitgen) fn emit_generic_binop(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) -> bool {
        self.generic_binop(lhs, rhs, func, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_array_teq(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        using_xmm: UsingXmm,
    ) -> bool {
        self.array_teq(lhs, rhs, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        using_xmm: UsingXmm,
    ) -> bool {
        self.opt_eq_cmp(lhs, rhs, kind, func, using_xmm);
        true
    }

    // ---- regexp build / kw-rest fixup runtime calls ----

    pub(in crate::codegen::jitgen) fn emit_concat_regexp(
        &mut self,
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    ) -> bool {
        self.concat_regexp(arg, len, using_xmm);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_check_kw_rest(&mut self, slot: SlotId) -> bool {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            cmpq [rbp - (rbp_local(slot))], (NIL_VALUE);
            jne  exit;
            movq rax, (runtime::empty_hash);
            call rax;
            movq [rbp - (rbp_local(slot))], rax;
        exit:
        };
        true
    }

    pub(in crate::codegen::jitgen) fn emit_expand_array(
        &mut self,
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
        using_xmm: UsingXmm,
    ) -> bool {
        let rest = if let Some(rest_pos) = rest_pos {
            rest_pos + 1
        } else {
            0
        };
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            lea rsi, [rbp - (rbp_local(dst))];
            movq rdx, (len);
            movq rcx, (rest);
            movq rax, (runtime::expand_array);
            call rax;
        );
        self.xmm_restore(using_xmm);
        true
    }

    // ---- float C-function calls (the former per-arch arms, verbatim) ----

    pub(in crate::codegen::jitgen) fn emit_cfunc_f_f(
        &mut self,
        f: unsafe extern "C" fn(f64) -> f64,
        src: FPReg,
        dst: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) -> bool {
        self.xmm_save(using_xmm);
        self.load_fpr_into_xmm0(src, base);
        monoasm!( &mut self.jit,
            movq rax, (f);
            call rax;
        );
        self.xmm_restore(using_xmm);
        self.store_fpr_into_xmm(dst, base);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_cfunc_ff_f(
        &mut self,
        f: extern "C" fn(f64, f64) -> f64,
        lhs: FPReg,
        rhs: FPReg,
        dst: FPReg,
        using_xmm: UsingXmm,
        base: usize,
    ) -> bool {
        self.xmm_save(using_xmm);
        // Load both args into xmm0/xmm1 (the SysV ABI passes f64 args
        // in xmm0, xmm1, ...). Pool ids resolve to xmm2..xmm15, so a
        // Phys source can never alias the scratch register written into.
        self.load_fpr_into_xmm0(lhs, base);
        self.load_fpr_into_xmm1(rhs, base);
        monoasm!( &mut self.jit,
            movq rax, (f);
            call rax;
        );
        self.xmm_restore(using_xmm);
        self.store_fpr_into_xmm(dst, base);
        true
    }

    // ---- method definition (the former per-arch arms, verbatim) ----

    pub(in crate::codegen::jitgen) fn emit_method_def(
        &mut self,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        self.method_def(name, func_id, using_xmm);
        self.handle_error(error);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_singleton_method_def(
        &mut self,
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> bool {
        self.singleton_method_def(obj, name, func_id, using_xmm);
        self.handle_error(error);
        true
    }

    // ---- exception / non-local control flow (former per-arch arms) ----

    // The `_loop_jit_spill_bytes` params mirror the aarch64 twins, which undo
    // the loop-JIT sp-bump before resuming the VM. x86's VM frame is
    // rbp-relative, so a stale rsp is harmless and the bump is ignored here.
    pub(in crate::codegen::jitgen) fn emit_raise(&mut self, _loop_jit_spill_bytes: usize) -> bool {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, rax;
            movq rax, (runtime::raise_err);
            call rax;
            jmp  raise;
        };
        true
    }

    pub(in crate::codegen::jitgen) fn emit_retry(
        &mut self,
        pc: BytecodePtr,
        _loop_jit_spill_bytes: usize,
    ) -> bool {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
            movq rdi, rbx;
            movq rax, (runtime::err_retry);
            call rax;
            jmp  raise;
        };
        true
    }

    pub(in crate::codegen::jitgen) fn emit_redo(
        &mut self,
        pc: BytecodePtr,
        _loop_jit_spill_bytes: usize,
    ) -> bool {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
            movq rdi, rbx;
            movq rax, (runtime::err_redo);
            call rax;
            jmp  raise;
        };
        true
    }

    pub(in crate::codegen::jitgen) fn emit_ensure_end(&mut self, _loop_jit_spill_bytes: usize) -> bool {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (runtime::ensure_end);
            call rax;
            testq rax, rax;
            jne  raise;
        };
        true
    }

    // ---- generic yield (former per-arch arm) ----

    pub(in crate::codegen::jitgen) fn emit_yield(
        &mut self,
        callid: CallSiteId,
        error: &DestLabel,
        evict: AsmEvict,
        evict_label: &DestLabel,
    ) -> bool {
        let return_addr = self.gen_yield(callid, error);
        self.set_deopt_with_return_addr(return_addr, evict, evict_label);
        true
    }


    // ---- &block forwarding (former per-arch arms) ----

    pub(in crate::codegen::jitgen) fn emit_block_arg_proxy(
        &mut self,
        ret: SlotId,
        outer: usize,
    ) -> bool {
        self.get_method_lfp(outer);
        self.block_arg_proxy(outer);
        self.store_rax(ret);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_block_arg(
        &mut self,
        ret: SlotId,
        using_xmm: UsingXmm,
        call_site_bc_ptr: BytecodePtr,
        error: &DestLabel,
    ) -> bool {
        self.block_arg(using_xmm, call_site_bc_ptr);
        self.handle_error(error);
        self.store_rax(ret);
        true
    }

    // ---- heap instance-variable store (former per-arch arm) ----

    pub(in crate::codegen::jitgen) fn emit_store_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using_xmm: UsingXmm,
    ) -> bool {
        self.store_ivar_heap(src, ivarid, is_object_ty, using_xmm);
        true
    }
}
