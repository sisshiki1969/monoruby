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
use crate::alloc::{BUMP_INLINE_LIMIT, CELL_SIZE_SHIFT, PAGE_DATA_OFFSET};
use crate::codegen::jitgen::lir::{LAluOp, LCond, LInst, LMem, LOperand, LReg, LSideExitKind};
use crate::codegen::jitgen::deopt_log::DeoptCause;

/// Resolve a LIR register operand to its x86 register number. The scratch
/// pointer is `rdx`.
fn x86_lreg(r: LReg) -> u64 {
    match r {
        LReg::Gp(v) => v.phys() as u64,
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
            | AsmInst::RegToStack(..)
            | AsmInst::RegToLfpStack(..)
            | AsmInst::StackToReg(..)
            | AsmInst::LitToReg(..)
            | AsmInst::LitToStack(..)
            | AsmInst::CondBr(..)
            | AsmInst::NilBr(..)
            | AsmInst::Br(..)
            | AsmInst::BrClassNe(..)
            | AsmInst::CheckLocal(..)
            | AsmInst::OptCase { .. }
            | AsmInst::GuardClass(..)
            | AsmInst::GuardClassIn(..)
            | AsmInst::BrClassNotIn(..)
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
            | AsmInst::ArrayMinMax { .. }
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
            | AsmInst::FprSave(..)
            | AsmInst::FprRestore(..)
            | AsmInst::IntegerBinOpReg { .. }
            | AsmInst::IntegerCmpReg { .. }
            | AsmInst::IntegerCmpImm { .. }
            | AsmInst::IntegerCmpBrReg { .. }
            | AsmInst::IntegerCmpBrImm { .. }
            | AsmInst::IntegerBinOpImm { .. }
            | AsmInst::IntegerDouble { .. }
            | AsmInst::FloatBinOp { .. }
            | AsmInst::FloatUnOp { .. }
            | AsmInst::I64ToBoth(..)
            | AsmInst::FloatCmp { .. }
            | AsmInst::FloatCmpBr { .. }
            | AsmInst::Ret
            | AsmInst::MethodRet(..)
            | AsmInst::BlockBreak(..)
            | AsmInst::ChainExit { .. }
            | AsmInst::GuardClassVersion { .. }
            | AsmInst::ContFramePc { .. }
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
            | AsmInst::ArrayAny { .. }
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
            | AsmInst::EnsureEnd { .. }
            | AsmInst::DeferSplicedExit { .. }
            | AsmInst::Yield { .. }
            | AsmInst::MethodRetSpecialized { .. }
            | AsmInst::BlockBreakSpecialized { .. }
            | AsmInst::YieldArrayExpand { .. }
            | AsmInst::SetupYieldFrame { .. }
            | AsmInst::SpecializedCall { .. }
            | AsmInst::SpecializedYield { .. }
            | AsmInst::LoadDynVarSpecialized { .. }
            | AsmInst::StoreOuterFprHomeF { .. }
            | AsmInst::LoadOuterFprHomeF { .. }
            | AsmInst::GuardFloatToOuterHomeF { .. }
            | AsmInst::BoxOuterHomeToDynVar { .. }
            | AsmInst::StoreDynVarSpecialized { .. }
            | AsmInst::Inline(..)
            | AsmInst::ArrayIndex { .. }
            | AsmInst::ArrayIndexAssign { .. }
            | AsmInst::LoadFieldToReg { .. }
            | AsmInst::BoolFieldToReg { .. }
            | AsmInst::ArrayLenFixnum { .. }
            | AsmInst::StringLenFixnum { .. }
            | AsmInst::HashLenFixnum { .. }
            | AsmInst::HashEntryAt { .. }
            | AsmInst::HashLiveAt { .. }
            | AsmInst::HashCompareByIdentity { .. }
            | AsmInst::HashDefault { .. }
            | AsmInst::IsNilToBool { .. }
            | AsmInst::NotToBool { .. }
            | AsmInst::MathSqrt { .. }
            | AsmInst::IntegerSucc { .. }
            | AsmInst::BlockGiven { .. }
            | AsmInst::ClassDef { .. }
            | AsmInst::SingletonClassDef { .. }
            | AsmInst::SetArgumentsForwardedHelper { .. }
            | AsmInst::Unreachable
            | AsmInst::RestKw { .. } => {
                unreachable!("handled by the shared compile_asmir dispatcher")
            }
            // `dst <- [caller_rbp - rbp_local(slot)]`. This body established
            // its own rbp (`init_func`), so the caller's is the value saved
            // at `[rbp]`; the D1 gate guarantees the caller is exactly one
            // level up. Same addressing as `jit_set_arguments_forwarded`.
            AsmInst::LoadCallerSlot { slot, dst } => {
                let r = dst as u64;
                let ofs = rbp_local(slot);
                monoasm! { &mut self.jit,
                    movq R(r), [rbp];
                    movq R(r), [R(r) - (ofs)];
                }
            }
            AsmInst::GuardClassVersionSpecialized { idx, deopt } => {
                let deopt = &self.deopt_label(labels, deopt, DeoptCause::Static("class version (specialized)"));
                self.guard_class_version_specialized(
                    class_version,
                    self.specialized_base + idx,
                    deopt,
                );
            }
            AsmInst::GuardConstVersionSpecialized {
                const_version,
                idx,
                deopt,
            } => {
                let deopt = &self.deopt_label(labels, deopt, DeoptCause::Static("const version (specialized)"));
                self.guard_const_version_specialized(
                    const_version,
                    self.specialized_base + idx,
                    deopt,
                );
            }
            AsmInst::RecompileDeoptSpecialized { idx, deopt, reason } => {
                let deopt = &self.deopt_label(labels, deopt, DeoptCause::Static("recompile counter (specialized)"));
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
                kw_route,
            } => {
                let offset = store[callee_fid].get_offset();
                // D1 source-routed: the whole bind is a compile-time
                // constant (see `forwarded_deferred_layout`). Eager: the
                // gate guarantees a plain req-only callee with
                // `req_num() >= lead_num`, so the length guard is exact.
                let layout = deferred_src.map(|(_, len)| {
                    store[callee_fid].forwarded_deferred_layout(lead_num, len as usize)
                });
                let expected_len = match &layout {
                    Some(l) => l.from_src,
                    None => store[callee_fid].req_num() - lead_num,
                };
                // K1: pair the routed caller slots with the callee's kw
                // register base.
                let kw_route =
                    kw_route.map(|route| (store[callee_fid].kw_reg_pos(), route));
                self.jit_set_arguments_forwarded(
                    callid,
                    callee_fid,
                    offset,
                    args,
                    lead_num,
                    expected_len,
                    layout,
                    recv,
                    kwrest_guard,
                    deferred_src,
                    kw_route,
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
        // (§9a-ii) Buffering pass: collect the lowered op, don't emit. The
        // region driver drains the buffer (with `lir_buf` cleared) afterwards.
        if let Some(buf) = self.lir_buf.as_mut() {
            buf.push(inst);
            return;
        }
        match inst {
            // dst <- src (elided when src == dst)
            LInst::Mov { dst, src } => {
                let (src, dst) = (src.phys(), dst.phys());
                if src != dst {
                    let (src, dst) = (src as u64, dst as u64);
                    monoasm!( &mut self.jit,
                        movq R(dst), R(src);
                    );
                }
            }
            // dst <- imm (full 64-bit immediate; x86 movq r64, imm64)
            LInst::LoadImm { dst, imm } => {
                let r = dst.phys() as u64;
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
            // dst <- bool([base + disp]): 32-bit raw-bool field → Ruby bool Value
            LInst::BoolFieldToReg { dst, base, disp } => {
                let (d, b) = (dst as u64, base as u64);
                monoasm!( &mut self.jit,
                    movl R(d), [R(b) + (disp)];
                    shlq R(d), 3;
                    orq  R(d), (FALSE_VALUE);
                );
            }
            // dst <- fixnum(Array#size): inline-or-heap length, fixnum-tagged.
            LInst::ArrayLenFixnum { dst, base } => {
                let (d, b) = (dst as u64, base as u64);
                monoasm!( &mut self.jit,
                    movq R(d), [R(b) + (RVALUE_OFFSET_ARY_CAPA)];
                    cmpq R(d), (ARRAY_INLINE_CAPA);
                    cmovgtq R(d), [R(b) + (RVALUE_OFFSET_HEAP_LEN)];
                    salq R(d), 1;
                    orq  R(d), 1;
                );
            }
            // dst <- fixnum(String#bytesize): inline-or-heap byte length, tagged.
            LInst::StringLenFixnum { dst, base } => {
                let (d, b) = (dst as u64, base as u64);
                monoasm!( &mut self.jit,
                    movq R(d), [R(b) + (RVALUE_OFFSET_ARY_CAPA)];
                    cmpq R(d), (STRING_INLINE_CAP);
                    cmovgtq R(d), [R(b) + (RVALUE_OFFSET_HEAP_LEN)];
                    salq R(d), 1;
                    orq  R(d), 1;
                );
            }
            // dst <- (src == nil) ? true : false (Ruby bool). Rsi scratch.
            LInst::IsNilToBool { dst, src } => {
                let (d, s, sc) = (dst as u64, src as u64, GP::Rsi as u64);
                monoasm!( &mut self.jit,
                    movq R(d), (FALSE_VALUE);
                    movq R(sc), (TRUE_VALUE);
                    cmpq R(s), (NIL_VALUE);
                    cmoveqq R(d), R(sc);
                );
            }
            // dst <- (!src) ? true : false (Ruby bool). Destroys src; Rsi scratch.
            LInst::NotToBool { dst, src } => {
                let (d, s, sc) = (dst as u64, src as u64, GP::Rsi as u64);
                monoasm!( &mut self.jit,
                    orq  R(s), (0x10);
                    movq R(d), (TRUE_VALUE);
                    movq R(sc), (FALSE_VALUE);
                    cmpq R(s), (FALSE_VALUE);
                    cmovneq R(d), R(sc);
                );
            }
            // Math.sqrt: ucomisd sets PF for NaN and CF for val < 0. NaN -> sqrt,
            // negative -> deopt.
            LInst::MathSqrt {
                fsrc,
                fret,
                deopt,
                base,
            } => {
                let do_sqrt = self.jit.label();
                self.load_fpr_into_xmm0(fsrc, base);
                monoasm!( &mut self.jit,
                    xorpd xmm1, xmm1;
                    ucomisd xmm0, xmm1;
                    jp do_sqrt;
                    jb deopt;
                do_sqrt:
                );
                if let Some(fret) = fret {
                    monoasm!( &mut self.jit,
                        sqrtsd xmm0, xmm0;
                    );
                    self.store_fpr_into_xmm(fret, base);
                }
            }
            // Integer#succ: tagged +1 (= +2), deopt on signed overflow.
            LInst::IntegerSucc { reg, deopt } => {
                let r = reg as u64;
                monoasm!( &mut self.jit,
                    addq R(r), 2;
                    jo   deopt;
                );
            }
            // Kernel#block_given?: walk to the outermost method frame, then
            // report whether its block slot is set & non-nil.
            LInst::BlockGiven { dst } => {
                let d = dst as u64;
                let exit = self.jit.label();
                let walk = self.jit.label();
                let found = self.jit.label();
                // `block_given?` reports whether the frame `yield` would
                // read its block from was given one. That is the *end* of
                // the outer chain — mirroring `Lfp::yield_home`: unlike
                // `Lfp::outermost` there is NO stop at a `proc_method`
                // (define_method body) boundary; yield keeps block
                // semantics there and ignores the call-site block (CRuby).
                monoasm!( &mut self.jit,
                    movq rdi, r14;                              // rdi = current LFP
                walk:
                    movq rax, [rdi - (LFP_OUTER)];              // rax = outer LFP (0 = none)
                    testq rax, rax;
                    jz found;                                   // no outer -> rdi is the home
                    movq rdi, rax;
                    jmp walk;
                found:
                    movq R(d), (FALSE_VALUE);
                    movq rdi, [rdi - (LFP_BLOCK)];
                    testq rdi, rdi;
                    jz exit;
                    cmpq rdi, (NIL_VALUE);
                    jeq exit;
                    movq R(d), (TRUE_VALUE);
                exit:
                );
            }
            // [rbp - slot] <- src (native-frame relative)
            LInst::Store {
                src,
                mem: LMem::Slot(slot),
            } => {
                let r = src as u64;
                monoasm!( &mut self.jit,
                    movq [rbp - (rbp_local(slot))], R(r);
                );
            }
            // [r14 - slot] <- src (LFP relative; follows a heap-moved frame)
            LInst::Store {
                src,
                mem: LMem::LfpSlot(slot),
            } => {
                let r = src as u64;
                monoasm!( &mut self.jit,
                    movq [r14 - (conv(slot))], R(r);
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
                    let r = dst.phys() as u64;
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
                let l = lhs.phys() as u64;
                match rhs {
                    LOperand::Reg(r) => {
                        monoasm! { &mut self.jit, cmpq R(l), R(r.phys() as u64); }
                    }
                    LOperand::Imm(i) => monoasm! { &mut self.jit, cmpq R(l), (i as u64); },
                }
            }
            // Signed conditional branch on the preceding `Cmp` (mirrors
            // `condbr_int`; the BrKind inversion is folded into `cond` by the
            // builder).
            // Unconditional branch (a dispatch arm funnelling into its merge).
            LInst::Br(target) => monoasm! { &mut self.jit, jmp target; },
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
            LInst::GuardClass { reg, class, deopt } => {
                self.guard_class_deopt(reg, class, &deopt)
            }
            // Dispatch arm: the miss is the next arm, not a side exit.
            LInst::BrClassNe { reg, class, target } => self.guard_class(reg, class, &target),
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
                        self.guard_class(reg, *class, &next);
                        monoasm!( &mut self.jit,
                            jmp ok;
                        );
                        self.jit.bind_label(next);
                    } else {
                        self.guard_class(reg, *class, &target);
                    }
                }
                self.jit.bind_label(ok);
            }
            LInst::GuardClassIn {
                reg,
                classes,
                deopt,
            } => {
                // Membership chain built from the single-class guard: each
                // class's check falls through on match (jump to ok) and
                // branches to the next candidate on mismatch; the last
                // candidate's mismatch is the real deopt.
                let ok = self.jit.label();
                let len = classes.len();
                for (i, class) in classes.iter().enumerate() {
                    if i + 1 < len {
                        let next = self.jit.label();
                        self.guard_class(reg, *class, &next);
                        monoasm!( &mut self.jit,
                            jmp ok;
                        );
                        self.jit.bind_label(next);
                    } else {
                        // Only the last candidate's miss is the real guard
                        // failure; the earlier ones are chain steps.
                        self.guard_class_deopt(reg, *class, &deopt);
                    }
                }
                self.jit.bind_label(ok);
            }
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
            LInst::GuardConstVersion {
                const_version: _,
                miss,
                deopt,
            } => {
                self.guard_const_version(miss, &deopt);
            }
            // Fixnum fast-path arithmetic with an overflow deopt.
            LInst::IntegerBinOp {
                kind,
                lhs,
                rhs,
                deopt,
            } => {
                self.integer_binop(lhs, rhs, kind, &deopt);
            }
            LInst::IntegerBinOpImm {
                kind,
                lhs,
                imm,
                deopt,
            } => {
                self.integer_binop_imm(lhs, imm, kind, &deopt);
            }
            LInst::IntegerDouble { reg, deopt } => {
                self.integer_double(reg, &deopt);
            }
            // Fixnum unary negate (tagged); deopt on i63 overflow.
            LInst::FixnumNeg { reg, deopt } => {
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
            // Fixnum bitwise-not (tagged); cannot overflow.
            LInst::FixnumBitNot { reg } => {
                let r = reg as u64;
                monoasm! { &mut self.jit,
                    sarq  R(r), 1;
                    notq  R(r);
                    salq  R(r), 1;
                    orq   R(r), 1;
                }
            }
            // ---- FP transfer / convert (spill-aware) -------------------------
            LInst::FprMove { src, dst, base } => {
                if src != dst {
                    match (
                        PhysMap::new(base).resolve(src),
                        PhysMap::new(base).resolve(dst),
                    ) {
                        (FPRegLoc::Xmm(s), FPRegLoc::Xmm(d)) => monoasm!( &mut self.jit,
                            movq xmm(d), xmm(s);
                        ),
                        (FPRegLoc::Xmm(s), FPRegLoc::Spill(d_off)) => monoasm!( &mut self.jit,
                            movq [rbp - (d_off)], xmm(s);
                        ),
                        (FPRegLoc::Spill(s_off), FPRegLoc::Xmm(d)) => monoasm!( &mut self.jit,
                            movq xmm(d), [rbp - (s_off)];
                        ),
                        (FPRegLoc::Spill(s_off), FPRegLoc::Spill(d_off)) => {
                            monoasm!( &mut self.jit,
                                movq xmm0, [rbp - (s_off)];
                                movq [rbp - (d_off)], xmm0;
                            )
                        }
                    }
                }
            }
            LInst::F64ToFpr { f, dst, base } => {
                let f_const = self.jit.const_f64(f);
                match PhysMap::new(base).resolve(dst) {
                    FPRegLoc::Xmm(p) => monoasm!( &mut self.jit,
                        movq xmm(p), [rip + f_const];
                    ),
                    FPRegLoc::Spill(off) => monoasm!( &mut self.jit,
                        movq xmm0, [rip + f_const];
                        movq [rbp - (off)], xmm0;
                    ),
                }
            }
            LInst::FixnumToFpr { src, dst, base } => {
                let (work, spill_off) = match PhysMap::new(base).resolve(dst) {
                    FPRegLoc::Xmm(p) => (p, None),
                    FPRegLoc::Spill(off) => (0u64, Some(off)),
                };
                self.integer_val_to_f64(src, work);
                if let Some(off) = spill_off {
                    monoasm!( &mut self.jit,
                        movq [rbp - (off)], xmm(work);
                    );
                }
            }
            LInst::FprToStack { src, slot, base } => {
                self.fpr_to_stack(src, &[slot], base);
            }
            LInst::FprSwap { lhs, rhs, base } => {
                if lhs != rhs {
                    match (
                        PhysMap::new(base).resolve(lhs),
                        PhysMap::new(base).resolve(rhs),
                    ) {
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
                        (FPRegLoc::Spill(l_off), FPRegLoc::Spill(r_off)) => {
                            monoasm!( &mut self.jit,
                                movq xmm0, [rbp - (l_off)];
                                movq xmm1, [rbp - (r_off)];
                                movq [rbp - (r_off)], xmm0;
                                movq [rbp - (l_off)], xmm1;
                            )
                        }
                    }
                }
            }
            LInst::FloatToFpr {
                src,
                dst,
                deopt,
                base,
            } => {
                let (work, spill_off) = match PhysMap::new(base).resolve(dst) {
                    FPRegLoc::Xmm(p) => (p, None),
                    FPRegLoc::Spill(off) => (0u64, Some(off)),
                };
                // A float unbox is a Float class guard in all but name — it is
                // what `guard_recv_class` emits for a `FLOAT_CLASS` receiver —
                // and its miss paths leave the offending value in rdi, so book
                // it alongside the `GuardClass` misses. This is the guard the
                // mixed Integer/Float arithmetic sites fail on.
                #[cfg(feature = "profile")]
                let deopt = self.class_guard_fail_recorder(&deopt);
                self.float_to_f64(src, work, &deopt);
                if let Some(off) = spill_off {
                    monoasm!( &mut self.jit,
                        movq [rbp - (off)], xmm(work);
                    );
                }
            }
            LInst::I64ToBoth { i, slot, dst, base } => {
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq [rbp - (rbp_local(slot))], (Value::integer(i).id());
                }
                match PhysMap::new(base).resolve(dst) {
                    FPRegLoc::Xmm(p) => monoasm!( &mut self.jit,
                        movq xmm(p), [rip + f];
                    ),
                    FPRegLoc::Spill(off) => monoasm!( &mut self.jit,
                        movq xmm0, [rip + f];
                        movq [rbp - (off)], xmm0;
                    ),
                }
            }
            // ---- FP arithmetic / comparison ----------------------------------
            LInst::FloatBinOp {
                kind,
                lhs,
                rhs,
                dst,
                base,
            } => {
                self.float_binop(kind, dst, (lhs, rhs), base);
            }
            LInst::FloatUnOp { kind, dst, base } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    match PhysMap::new(base).resolve(dst) {
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
            },
            LInst::FloatCmp {
                kind,
                lhs,
                rhs,
                base,
            } => {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                };
                self.cmp_float((lhs, rhs), base);
                self.setflag_float(kind);
            }
            LInst::FloatCmpBr {
                kind,
                lhs,
                rhs,
                brkind,
                dest,
                base,
            } => {
                self.cmp_float((lhs, rhs), base);
                self.condbr_float(kind, dest, brkind);
            }
            // ---- FP pool save/restore + FP C-calls ---------------------------
            LInst::FprSave { using_fpr, cont } => self.fpr_save_with_cont(using_fpr, cont),
            LInst::FprRestore { using_fpr, cont } => self.fpr_restore_with_cont(using_fpr, cont),
            LInst::CFunc_F_F {
                f,
                src,
                dst,
                using_fpr,
                base,
            } => {
                self.fpr_save(using_fpr);
                self.load_fpr_into_xmm0(src, base);
                monoasm!( &mut self.jit,
                    movq rax, (f);
                    call rax;
                );
                self.fpr_restore(using_fpr);
                self.store_fpr_into_xmm(dst, base);
            }
            LInst::CFunc_FF_F {
                f,
                lhs,
                rhs,
                dst,
                using_fpr,
                base,
            } => {
                self.fpr_save(using_fpr);
                self.load_fpr_into_xmm0(lhs, base);
                self.load_fpr_into_xmm1(rhs, base);
                monoasm!( &mut self.jit,
                    movq rax, (f);
                    call rax;
                );
                self.fpr_restore(using_fpr);
                self.store_fpr_into_xmm(dst, base);
            }
            // Speculated-unboxed outer local (doc/chain_deopt.md §5 step 5):
            // one f64 move against the speculating frame's FP save/spill
            // area, `[rbp + offset + disp]` (offset pre-resolved by the
            // DynVarOffset pass), through the xmm0 scratch.
            LInst::LoadDynVarSpecF {
                offset,
                disp,
                dst,
                base,
            } => {
                let off = offset as i32 + disp;
                monoasm!( &mut self.jit,
                    movq xmm0, [rbp + (off)];
                );
                self.store_fpr_into_xmm(dst, base);
            }
            LInst::StoreDynVarSpecF {
                offset,
                disp,
                src,
                base,
            } => {
                let off = offset as i32 + disp;
                self.load_fpr_into_xmm0(src, base);
                monoasm!( &mut self.jit,
                    movq [rbp + (off)], xmm0;
                );
            }
            LInst::GuardCapture { deopt } => self.guard_capture(&deopt),
            // BOP-redefinition guard: outline the deopt path (page 1) so the hot
            // path is a single load + branch.
            LInst::CheckBOP { deopt, version } => {
                let bop_flag = self.bop_redefined_flags.clone();
                let l1 = self.jit.label();
                assert_eq!(0, self.jit.get_page());
                monoasm!(
                    &mut self.jit,
                    cmpl [rip + bop_flag], (version as i32);
                    jne l1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                l1:
                    movq rdi, (Value::symbol_from_str("_bop_guard").id());
                    jmp  deopt;
                );
                self.jit.select_page(0);
            }
            // Cold side-exit (deopt) handler blocks. Dispatch on the kind to the
            // existing x86 handler emitters (defined in `jitgen.rs`).
            // Per-branch deopt trampoline (see `jitgen::deopt_log`). rbx is
            // `&mut Executor` for the whole body, so recording the cause
            // needs no scratch register, no stack traffic, and — crucially
            // for a guard that has just done its compare — no instruction
            // that touches the flags.
            #[cfg(all(feature = "deopt", target_arch = "x86_64"))]
            LInst::DeoptTrampoline {
                entry,
                deopt,
                cause,
                site,
            } => {
                use crate::codegen::jitgen::deopt_log::DeoptCause;
                // Page discipline mirrors `class_guard_fail_recorder`: from
                // the hot page, park the stub on the cold one; when already
                // emitting cold (bridge blocks), jump over it in place.
                let inline = self.jit.get_page() != 0;
                let skip = self.jit.label();
                if inline {
                    monoasm!( &mut self.jit, jmp skip; );
                } else {
                    self.jit.select_page(1);
                }
                self.jit.bind_label(entry);
                match cause {
                    DeoptCause::Value(r)
                    | DeoptCause::ClassGuard(r, _)
                    | DeoptCause::ValueVsBaked(r, _)
                    | DeoptCause::Raw(r) => {
                        monoasm!( &mut self.jit,
                            movq [rbx + (EXECUTOR_DEOPT_CAUSE)], R(r as u64);
                        );
                    }
                    DeoptCause::Static(_) => {
                        // No operand: zero the word so a previous branch's
                        // value cannot be mistaken for this one's.
                        monoasm!( &mut self.jit,
                            movq [rbx + (EXECUTOR_DEOPT_CAUSE)], 0;
                        );
                    }
                }
                monoasm!( &mut self.jit,
                    movl [rbx + (EXECUTOR_DEOPT_SITE)], (site as i32);
                    jmp  deopt;
                );
                if inline {
                    self.jit.bind_label(skip);
                } else {
                    self.jit.select_page(0);
                }
            }
            LInst::SideExit {
                kind,
                pc,
                wb,
                entry,
                loop_jit_spill_bytes,
                base,
                #[cfg(feature = "deopt")]
                exit_id,
            } => match kind {
                LSideExitKind::Deopt { chain } => self.gen_deopt_with_label(
                    pc,
                    &wb,
                    entry,
                    loop_jit_spill_bytes,
                    base,
                    chain,
                    #[cfg(feature = "deopt")]
                    exit_id,
                ),
                LSideExitKind::Evict => self.gen_evict_with_label(
                    pc,
                    &wb,
                    entry,
                    loop_jit_spill_bytes,
                    base,
                    #[cfg(feature = "deopt")]
                    exit_id,
                ),
                LSideExitKind::RecompileDeopt {
                    reason,
                    target,
                    chain,
                } => self.gen_recompile_deopt_with_label(
                    pc,
                    &wb,
                    reason,
                    target,
                    entry,
                    loop_jit_spill_bytes,
                    base,
                    chain,
                    #[cfg(feature = "deopt")]
                    exit_id,
                ),
                LSideExitKind::Error { chain } => self.gen_handle_error(pc, wb, entry, base, chain),
            },
            // Macro-ops (irreducible runtime-call shapes) are delegated to the
            // arch-neutral fallback, which dispatches to the per-arch `emit_*`.
            other => self.encode_linst_macro(other),
        }
    }

    /// [lfp - slot] <- reg
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
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        self.store_constant(id, using_fpr);
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
        using_fpr: UsingFpr,
    ) -> bool {
        self.load_gvar(name, using_fpr);
        true
    }

    /// $gvar <- src.
    pub(in crate::codegen::jitgen) fn emit_store_gvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.store_gvar(name, src, using_fpr);
        true
    }

    /// rax <- @@cvar.
    pub(in crate::codegen::jitgen) fn emit_load_cvar(
        &mut self,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.load_cvar(name, using_fpr);
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
    pub(in crate::codegen::jitgen) fn emit_create_array(
        &mut self,
        src: SlotId,
        len: usize,
    ) -> bool {
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
        inline: Option<(SlotId, u16)>,
        using_fpr: UsingFpr,
    ) -> bool {
        match inline {
            // Inline the common case (small no-splat literal) when the
            // allocator free-list addresses were captured at startup.
            Some((args, len)) if !self.alloc_free_head_addr.is_null() => {
                self.new_array_inline(callid, args, len, using_fpr);
            }
            _ => self.new_array(callid, using_fpr),
        }
        true
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
    /// - rax: the fresh cell, header already stored
    ///
    /// #### destroy
    /// - rdi, rcx
    pub(crate) fn emit_alloc_cell(&mut self, header: CellHeader, slow: &DestLabel) {
        // The cell acquisition itself (free-list pop / bump) lives in the
        // shared `alloc_cell` stub; only the null test and the per-site
        // header write are laid inline.
        let alloc_cell = self.alloc_cell.clone();
        monoasm! { &mut self.jit,
            call alloc_cell;
            testq rax, rax;
            jz   slow;                // alloc-flag / new-page territory
        }
        match header {
            CellHeader::Imm(h) => monoasm! { &mut self.jit,
                movq rdi, (h);
            },
            CellHeader::NewbornOf(src) => {
                // Keep class/type (the upper 48 bits) and the non-GC flags.
                let mask: u64 = !0xffffu64 | NEWBORN_FLAG_MASK as u64;
                monoasm! { &mut self.jit,
                    movq rdi, (src);
                    movq rdi, [rdi];
                    movq rcx, (mask);
                    andq rdi, rcx;
                }
            }
        }
        monoasm! { &mut self.jit,
            movq [rax + (RVALUE_OFFSET_FLAG)], rdi;   // header (offset 0); overwrites the free link
        }
    }

    /// Bind the shared `alloc_cell` stub (see the field doc in
    /// `codegen.rs`): `Allocator::alloc`'s two fast paths — free-list pop,
    /// else bump — returning the fresh cell in rax, or 0 when the runtime
    /// must take over. Emitted at the end of `Codegen::new`, after the
    /// allocator addresses are captured.
    pub(in crate::codegen) fn gen_alloc_cell_stub(&mut self) {
        let label = self.alloc_cell.clone();
        let free_head = self.alloc_free_head_addr as u64;
        let free_count = self.alloc_free_count_addr as u64;
        let total = self.alloc_total_addr as u64;
        let used = self.alloc_used_addr as u64;
        let page = self.alloc_page_addr as u64;
        let bump = self.jit.label();
        let fail = self.jit.label();
        let done = self.jit.label();
        monoasm! { &mut self.jit,
        label:
            movq rdi, (free_head);
            movq rax, [rdi];          // rax = free-list head (cell ptr, or 0 = None)
            testq rax, rax;
            jz   bump;
            movq rcx, [rax];          // rcx = (*cell).header.next (free link @ offset 0)
            movq [rdi], rcx;          // free = next
            movq rdi, (free_count);
            subq [rdi], 1;
            jmp  done;
        bump:
            movq rdi, (used);
            movq rcx, [rdi];          // rcx = used_in_current
            cmpq rcx, (BUMP_INLINE_LIMIT);
            jae  fail;                // alloc-flag / new-page territory
            movq rax, (page);
            movq rax, [rax];          // rax = current page
            shlq rcx, (CELL_SIZE_SHIFT);
            addq rax, rcx;            // + used_in_current * CELL_SIZE
        }
        if PAGE_DATA_OFFSET != 0 {
            monoasm! { &mut self.jit,
                addq rax, (PAGE_DATA_OFFSET);
            }
        }
        monoasm! { &mut self.jit,
            addq [rdi], 1;            // used_in_current += 1
        done:
            movq rdi, (total);
            addq [rdi], 1;
            ret;
        fail:
            xorq rax, rax;
            ret;
        }
    }

    /// Inline allocation of a small (`0..=ARRAY_INLINE_CAPA`) no-splat array
    /// literal: pop a recycled cell from the GC free list and initialise it
    /// directly as an inline-storage Array, with no runtime call. When the
    /// free list is empty, fall back to the runtime `gen_array` (which also
    /// handles bump allocation, page growth, and the alloc flag).
    ///
    /// The elements have already been written back to consecutive stack
    /// slots starting at `args` (see `TraceIr::Array`). A freshly allocated
    /// young Array needs no write barrier for its elements (the barrier
    /// guards old→young stores), and there is no GC safepoint between the
    /// free-list pop and the field initialisation, so the partially built
    /// object is never observed by a collection.
    fn new_array_inline(
        &mut self,
        callid: CallSiteId,
        args: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) {
        let slow = self.jit.label();
        let cont = self.jit.label();
        // 8-byte object header: flag=1 (live) | ty=ARRAY<<16 | class=ARRAY_CLASS<<32.
        let header: u64 =
            ((ARRAY_CLASS.u32() as u64) << 32) | ((ObjTy::ARRAY.get() as u64) << 16) | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm! { &mut self.jit,
            movq [rax + (RVALUE_OFFSET_VAR)], 0;      // var_table = None
            movq [rax + (RVALUE_OFFSET_ARY_CAPA)], (len as i32);  // inline length (capa == len <= 5)
        }
        for k in 0..len {
            let slot = SlotId(args.0 + k);
            let off = RVALUE_OFFSET_INLINE as i32 + (k as i32) * 8;
            monoasm! { &mut self.jit,
                movq rcx, [rbp - (rbp_local(slot))];
                movq [rax + (off)], rcx;
            }
        }
        monoasm! { &mut self.jit,
            jmp  cont;
        slow:
        }
        self.new_array(callid, using_fpr);
        monoasm! { &mut self.jit,
        cont:
        }
    }

    /// rax <- Hash literal from the `len` key/value slots at `args`.
    pub(in crate::codegen::jitgen) fn emit_new_hash(
        &mut self,
        args: SlotId,
        len: usize,
        using_fpr: UsingFpr,
    ) -> bool {
        if len <= HASH_INLINE_CAP && !self.alloc_free_head_addr.is_null() {
            self.new_hash_inline(args, len, using_fpr);
        } else {
            self.new_hash(args, len, using_fpr);
        }
        true
    }

    /// Inline allocation of a small (`0..=HASH_INLINE_CAP` pairs) Hash
    /// literal: pop a recycled cell from the GC free list and initialise it
    /// directly as an inline-representation Hash, with no runtime call.
    ///
    /// The fast path requires every key to be a packed immediate — for
    /// those eql? is exactly bit equality and no `#hash` dispatch is
    /// observable (see `is_inline_key`) — and the keys to be pairwise
    /// distinct (a duplicate needs last-wins overwrite). Anything else
    /// (heap keys with their `frozen_hash_key` and user-`#hash` protocol,
    /// duplicates, an empty free list) falls back to the runtime
    /// `gen_hash`, which the following `handle_error` already covers; on
    /// the fast path no error is possible.
    ///
    /// The pairs have been written back to consecutive slots (key0, val0,
    /// key1, …) starting at `args` (see `TraceIr::Hash`). Unused inline
    /// pairs are written nil-nil, per the `HashBody::inline` contract that
    /// the whole payload stays initialised. A freshly allocated young Hash
    /// needs no write barrier and there is no GC safepoint between the
    /// free-list pop and the field initialisation (same argument as
    /// `new_array_inline`).
    fn new_hash_inline(&mut self, args: SlotId, len: usize, using_fpr: UsingFpr) {
        let slow = self.jit.label();
        let cont = self.jit.label();
        for i in 0..len {
            let key = SlotId(args.0 + 2 * i as u16);
            // Heap iff (bits & 0b111) == 0 (`Value::is_packed_value`).
            monoasm! { &mut self.jit,
                movq rax, [rbp - (rbp_local(key))];
                andq rax, 0b111;
                jz   slow;
            }
        }
        for j in 1..len {
            for i in 0..j {
                let ki = SlotId(args.0 + 2 * i as u16);
                let kj = SlotId(args.0 + 2 * j as u16);
                monoasm! { &mut self.jit,
                    movq rax, [rbp - (rbp_local(ki))];
                    cmpq rax, [rbp - (rbp_local(kj))];
                    jeq  slow;
                }
            }
        }
        // 8-byte object header: flag=1 (live) | ty=HASH<<16 | rep<<24 |
        // class=HASH_CLASS<<32. The ty_flags byte (bits 24-31) carries the
        // inline representation: rep = pair count, eql?-keyed, no live
        // iteration, no ruby2_keywords.
        let header: u64 = ((HASH_CLASS.u32() as u64) << 32)
            | ((len as u64) << 24)
            | ((ObjTy::HASH.get() as u64) << 16)
            | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm! { &mut self.jit,
            movq [rax + (RVALUE_OFFSET_VAR)], 0;      // var_table = None
        }
        for i in 0..HASH_INLINE_CAP {
            let pair = HASH_INLINE_PAIRS_OFFSET + i * HASH_INLINE_PAIR_STRIDE;
            let key_off = (pair + HASH_INLINE_KEY_OFFSET) as i32;
            let val_off = (pair + HASH_INLINE_VALUE_OFFSET) as i32;
            if i < len {
                let key = SlotId(args.0 + 2 * i as u16);
                let val = SlotId(args.0 + 2 * i as u16 + 1);
                monoasm! { &mut self.jit,
                    movq rcx, [rbp - (rbp_local(key))];
                    movq [rax + (key_off)], rcx;
                    movq rcx, [rbp - (rbp_local(val))];
                    movq [rax + (val_off)], rcx;
                }
            } else {
                monoasm! { &mut self.jit,
                    movq [rax + (key_off)], (NIL_VALUE);
                    movq [rax + (val_off)], (NIL_VALUE);
                }
            }
        }
        monoasm! { &mut self.jit,
            jmp  cont;
        slow:
        }
        self.new_hash(args, len, using_fpr);
        monoasm! { &mut self.jit,
        cont:
        }
    }

    /// rax <- min/max of the `len` values at `args`, computed in place —
    /// the fused, allocation-free `[a, b, …].min` / `.max`.
    pub(in crate::codegen::jitgen) fn emit_array_min_max(
        &mut self,
        args: SlotId,
        len: u16,
        min: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let f = if min {
            runtime::opt_array_min as *const () as usize
        } else {
            runtime::opt_array_max as *const () as usize
        };
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rbp - (rbp_local(args))];
            movq rcx, (len as usize);
            movq rax, (f);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    /// rax <- the Hash in `hash` after inserting the `len` key/value pairs
    /// at `args` (chunked Hash literal).
    pub(in crate::codegen::jitgen) fn emit_hash_insert(
        &mut self,
        hash: SlotId,
        args: SlotId,
        len: usize,
        using_fpr: UsingFpr,
    ) -> bool {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rbp - (rbp_local(args))];
            movq rcx, (len);
            movq r8, [rbp - (rbp_local(hash))];
            movq rax, (runtime::hash_insert);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    /// rax <- the Array in `dst` after concatenating the Array in `src`
    /// (chunked Array literal).
    pub(in crate::codegen::jitgen) fn emit_array_concat(
        &mut self,
        dst: SlotId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - (rbp_local(dst))];
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::array_concat);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    /// rax <- Range(start, end, exclude_end).
    pub(in crate::codegen::jitgen) fn emit_new_range(
        &mut self,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        self.load_rdi(start);
        self.load_rsi(end);
        self.new_range(exclude_end, using_fpr);
        true
    }

    /// rax <- the `len` slots at `arg` concatenated into a String.
    pub(in crate::codegen::jitgen) fn emit_concat_str(
        &mut self,
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) -> bool {
        self.concat_string(arg, len, using_fpr);
        true
    }

    /// rax <- `src` coerced to an Array (`Array(x)` / splat).
    pub(in crate::codegen::jitgen) fn emit_to_a(
        &mut self,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.to_a(src, using_fpr);
        true
    }

    /// rax <- a deep copy of literal `v` (fresh mutable object per evaluation).
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
        let Some(elems) = v
            .inline_copyable_array()
            .filter(|_| !self.alloc_free_head_addr.is_null())
        else {
            self.deepcopy_literal(v, using_fpr);
            return true;
        };
        let slow = self.jit.label();
        let cont = self.jit.label();
        self.emit_alloc_cell(CellHeader::NewbornOf(v.id()), &slow);
        monoasm! { &mut self.jit,
            movq [rax + (RVALUE_OFFSET_VAR)], 0;  // var_table = None
            movq [rax + (RVALUE_OFFSET_ARY_CAPA)], (elems.len() as i32);  // inline length
        }
        for (k, e) in elems.iter().enumerate() {
            let off = RVALUE_OFFSET_INLINE as i32 + (k as i32) * 8;
            monoasm! { &mut self.jit,
                movq rcx, (e.id());
                movq [rax + (off)], rcx;
            }
        }
        monoasm! { &mut self.jit,
            jmp  cont;
        slow:
        }
        self.deepcopy_literal(v, using_fpr);
        monoasm! { &mut self.jit,
        cont:
        }
        true
    }

    /// Store the call-site pc into the outgoing cont-frame slot
    /// (`[rsp]` = the callee frame's CFP+24). The 16-byte region was
    /// reserved by the preceding cont-mode `FprSave`, whose xmm saves
    /// sit above it — so this is a plain store, no rsp adjustment.
    /// See `AsmInst::ContFramePc`.
    pub(in crate::codegen) fn emit_cont_frame_pc(&mut self, call_site_pc: u64) {
        monoasm! { &mut self.jit,
            movq r11, (call_site_pc);
            movq [rsp], r11;
        }
    }

    /// Record `return_addr` — the address the callee will `ret` to — under
    /// this call site's `evict` id, so the `AsmInst::ChainExit` pushed just
    /// after the call can key its replay data by it (`register_chain_exit`).
    pub(super) fn set_deopt_with_return_addr(&mut self, return_addr: CodePtr, evict: AsmEvict) {
        self.asm_return_addr_table.insert(evict, return_addr);
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
    fn array_teq(&mut self, lhs: SlotId, rhs: SlotId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        self.load_rdx(lhs);
        self.load_rcx(rhs);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::array_teq);
            call rax;
        );
        self.fpr_restore(using_fpr);
    }

    /// `any element truthy` for the array in `reg` via runtime::array_any
    /// (rdi=vm, rsi=globals, rdx=val); result Value in rax. Cannot raise.
    fn array_any(&mut self, reg: SlotId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        self.load_rdx(reg);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::array_any);
            call rax;
        );
        self.fpr_restore(using_fpr);
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
        is_func_call: bool,
        using_fpr: UsingFpr,
    ) {
        self.fpr_save(using_fpr);
        self.load_rdx(lhs);
        self.load_rcx(rhs);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq r8, (is_func_call as u64);
            movq rax, (func);
            call rax;
        );
        self.fpr_restore(using_fpr);
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
        is_func_call: bool,
        using_fpr: UsingFpr,
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
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq r8, (is_func_call as u64);
            movq rax, (func);
            call rax;
        );
        self.fpr_restore(using_fpr);
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
    fn new_array(&mut self, callid: CallSiteId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movl rdx, (callid.get());
            lea  rcx, [r14 - (LFP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_array);
            call rax;
        );
        self.fpr_restore(using_fpr);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea  rdx, [rbp - (rbp_local(args))];
            movq rcx, (len);
            movq rax, (runtime::gen_hash);
            call rax;
        );
        self.fpr_restore(using_fpr);
    }

    fn new_range(&mut self, exclude_end: bool, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdx, rbx; // &mut Executor
            movq rcx, r12; // &mut Globals
            movl r8, (exclude_end as u32);
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }

    fn concat_string(&mut self, arg: SlotId, len: u16, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [rbp - (rbp_local(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.fpr_restore(using_fpr);
    }

    fn to_a(&mut self, src: SlotId, using_fpr: UsingFpr) {
        let toa = self.jit.label();
        let exit = self.jit.label();
        monoasm!( &mut self.jit,
            movq rax, [rbp - (rbp_local(src))];
        );
        self.guard_rvalue(GP::Rax, ARRAY_CLASS, &toa);
        self.bind_label(exit.clone());

        self.select_page(1);
        self.bind_label(toa);
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rax;
            movq rax, (runtime::to_a);
            call rax;
        );
        self.fpr_restore(using_fpr);
        monoasm!( &mut self.jit,
            jmp  exit;
        );
        self.select_page(0);
    }

    fn concat_regexp(&mut self, arg: SlotId, len: u16, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [rbp - (rbp_local(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_regexp);
            call rax;
        );
        self.fpr_restore(using_fpr);
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
    fn block_arg(&mut self, using_fpr: UsingFpr, call_site_bc_ptr: BytecodePtr) {
        let call_site_ptr_val = call_site_bc_ptr.as_ptr() as u64;
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdx, r14;
            movq rdi, rbx;
            movq rsi, r12;
            movq rcx, (call_site_ptr_val);
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.fpr_restore(using_fpr);
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

    /// Integer comparison; result Value lands in the accumulator.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp(
        &mut self,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
    ) -> bool {
        self.integer_cmp(kind, lhs, rhs);
        true
    }

    /// Integer comparison against a tagged immediate; result Value lands in
    /// the accumulator.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp_imm(
        &mut self,
        kind: CmpKind,
        lhs: GP,
        imm: i32,
    ) -> bool {
        self.integer_cmp_imm(kind, lhs, imm);
        true
    }

    /// Method epilogue: tear down the frame and return.
    pub(in crate::codegen::jitgen) fn emit_ret(&mut self, check_deferred: bool) {
        // See the VM `Ret` opcode's twin gate (#1186): a frame returning
        // normally while owning the parked deferral (reachable in compiled
        // code via an OSR'd loop inside an `ensure` handler executing a
        // local `return`) must discard it. Emitted only for
        // handler-carrying iseqs; the call preserves rax (the return
        // value) around itself.
        if check_deferred {
            let no_defer = self.jit.label();
            monoasm! { &mut self.jit,
                movq rdi, [rbx + (EXECUTOR_DEFERRED_TOP)];
                cmpq rdi, r14;
                jne  no_defer;
                pushq rax;
                movq rdi, rbx;
                movq rax, (runtime::discard_deferred_on_ret);
                call rax;
                popq rax;
            no_defer:
            };
        }
        self.epilogue();
    }

    /// Return through the method-return path, resuming the caller at `pc + 1`.
    // `_loop_jit_spill_bytes` is deliberately unused, like the other
    // `entry_raise` exits (`emit_raise` etc.): x86's VM frames are
    // rbp-relative, so a stale `rsp` at the resume is unobservable.
    pub(in crate::codegen::jitgen) fn emit_method_ret(
        &mut self,
        pc: BytecodePtr,
        _loop_jit_spill_bytes: usize,
    ) {
        monoasm! { &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
        };
        self.method_return();
    }

    /// Non-local exit through the block-break path, resuming at `pc + 1`.
    pub(in crate::codegen::jitgen) fn emit_block_break(
        &mut self,
        pc: BytecodePtr,
        _loop_jit_spill_bytes: usize,
    ) {
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
        max: u16,
        min: u16,
        else_dest: DestLabel,
        branch_dests: Box<[DestLabel]>,
    ) {
        // generate a jump table.
        let jump_table = self.jit.const_align8();
        for dest_label in branch_dests.iter() {
            self.jit.abs_address(dest_label.clone());
        }

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

    /// Write the callee frame's meta/outer/block fields before a call. The
    /// store-dependent block info is pre-resolved by the dispatcher.
    pub(in crate::codegen::jitgen) fn emit_setup_method_frame(
        &mut self,
        meta: Meta,
        outer_lfp: Option<Lfp>,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
    ) {
        self.setup_method_frame(meta, outer_lfp, block_fid, block_arg);
    }

    /// Marshal the call arguments into the callee frame (`offset` is the callee
    /// scratch-area size, pre-resolved by the dispatcher).
    pub(in crate::codegen::jitgen) fn emit_set_arguments(
        &mut self,
        callid: CallSiteId,
        callee_fid: FuncId,
        offset: usize,
    ) {
        self.jit_set_arguments(callid, callee_fid, offset);
    }

    /// Recompile-or-deopt: deopt now and schedule recompilation once the inline
    /// cache warms.
    pub(in crate::codegen::jitgen) fn emit_recompile_deopt(
        &mut self,
        target: RecompileTarget,
        deopt: &DestLabel,
        // x86 recompiles in place (no extern-boundary panic surfaced here), so
        // the aarch64-only error side-exit is unused.
        _error: Option<&DestLabel>,
        reason: RecompileReason,
    ) {
        match target {
            RecompileTarget::Whole(position) => self.recompile_and_deopt(position, deopt, reason),
            RecompileTarget::Specialized(idx) => {
                self.recompile_and_deopt_specialized(deopt, self.specialized_base + idx, reason)
            }
        }
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
    /// enough (extending it via a runtime call if not). `heap_len` is `None`
    /// for a frozen / inline-only self (no-op); the value is pre-resolved by the
    /// dispatcher.
    pub(in crate::codegen::jitgen) fn emit_preparation(&mut self, heap_len: Option<usize>) {
        if let Some(heap_len) = heap_len {
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
    }

    /// Loop-JIT entry stack setup: pin `rsp` to this frame's canonical depth.
    pub(in crate::codegen::jitgen) fn emit_loop_jit_rsp_bump(
        &mut self,
        offset: LoopRspOffset,
    ) -> bool {
        // Pin rather than subtract, for the same reason aarch64 does: this
        // body is reached from *either* producer of a frame — the VM's
        // `init_method`, which reserves the iseq's local area and knows
        // nothing of spill slots, or a JIT prologue, which reserves that
        // plus this unit's spill region — so subtracting from the inherited
        // `rsp` would count the spill region twice on the prologue path.
        //
        // x86 is insensitive to the *addressing* consequences (its frames
        // are rbp-relative, so a body at the wrong depth still reads its own
        // slots correctly), but the inlined frames it builds below `rsp` do
        // move, and `resolve_specialized_id_chain`'s rbp-to-rbp distances
        // are fixed at compile time.
        let below = offset.unwrap_concrete();
        monoasm! { &mut self.jit, lea rsp, [rbp - (below as i32)]; }
        true
    }

    /// Load a heap-spilled instance variable into `dst`, substituting
    /// nil for an out-of-range (non-self) or unset slot.
    pub(in crate::codegen::jitgen) fn emit_load_ivar_heap(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
        dst: GP,
    ) -> bool {
        self.load_ivar_heap(ivarid, is_object_ty, self_, dst);
        true
    }

    /// `undef`-method via runtime::undef_method(vm, globals, id).
    pub(in crate::codegen::jitgen) fn emit_undef_method(
        &mut self,
        undef: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (undef.get());
            movq rax, (runtime::undef_method);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    /// Alias a global var via runtime::alias_global_var(globals, new, old).
    pub(in crate::codegen::jitgen) fn emit_alias_gvar(
        &mut self,
        new: IdentId,
        old: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, r12;          // &mut Globals
            movl rsi, (new.get());  // new IdentId
            movl rdx, (old.get());  // old IdentId
            movq rax, (runtime::alias_global_var);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    /// Check class-variable existence via runtime::check_class_var.
    pub(in crate::codegen::jitgen) fn emit_check_cvar(
        &mut self,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.check_cvar(name, using_fpr);
        true
    }

    /// @@cvar <- src via runtime::set_class_var.
    pub(in crate::codegen::jitgen) fn emit_store_cvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.store_cvar(name, src, using_fpr);
        true
    }

    /// Alias a method via runtime::alias_method (old/new read from frame slots).
    pub(in crate::codegen::jitgen) fn emit_alias_method(
        &mut self,
        new: SlotId,
        old: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (conv(old))];
            movq rcx, [r14 - (conv(new))];
            movq rax, (runtime::alias_method);
            call rax;
        );
        self.fpr_restore(using_fpr);
        true
    }

    // ---- defined? runtime-call family (delegate to the existing helpers) ----

    pub(in crate::codegen::jitgen) fn emit_defined_yield(
        &mut self,
        dst: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_yield(dst, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_super(
        &mut self,
        dst: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_super(dst, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_gvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_gvar(dst, name, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_cvar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_cvar(dst, name, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_const(
        &mut self,
        dst: SlotId,
        siteid: ConstSiteId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_const(dst, siteid, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_method(
        &mut self,
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_method(dst, recv, name, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_defined_ivar(
        &mut self,
        dst: SlotId,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.defined_ivar(dst, name, using_fpr);
        true
    }

    // ---- generic binary-op runtime calls (delegate to the helpers) ----

    pub(in crate::codegen::jitgen) fn emit_generic_binop(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        is_func_call: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        self.generic_binop(lhs, rhs, func, is_func_call, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_array_teq(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.array_teq(lhs, rhs, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_array_any(
        &mut self,
        reg: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        self.array_any(reg, using_fpr);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        is_func_call: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        self.opt_eq_cmp(lhs, rhs, kind, func, is_func_call, using_fpr);
        true
    }

    // ---- regexp build / kw-rest fixup runtime calls ----

    pub(in crate::codegen::jitgen) fn emit_concat_regexp(
        &mut self,
        arg: SlotId,
        len: u16,
        using_fpr: UsingFpr,
    ) -> bool {
        self.concat_regexp(arg, len, using_fpr);
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
        using_fpr: UsingFpr,
    ) -> bool {
        let rest = if let Some(rest_pos) = rest_pos {
            rest_pos + 1
        } else {
            0
        };
        // The fast path (when emitted) falls through to the runtime call
        // below on `slow`, and jumps past it on success.
        let fast = self.expand_array_fast_path(dst, len, rest_pos);
        self.fpr_save(using_fpr);
        // `src` is already in rdx (loaded by the caller). Args:
        // rdi = &mut Executor, rsi = &mut Globals, rdx = src, rcx = &dst,
        // r8 = len, r9 = rest.
        monoasm!( &mut self.jit,
            lea rcx, [rbp - (rbp_local(dst))];
            movq r8, (len);
            movq r9, (rest);
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::expand_array);
            call rax;
        );
        self.fpr_restore(using_fpr);
        if let Some(exit) = fast {
            self.jit.bind_label(exit);
        }
        true
    }

    ///
    /// The no-`#to_ary`, no-rest fast path of `AsmInst::ExpandArray`: when
    /// `src` (rdx) is already an `Array` holding at least `len` elements,
    /// destructuring is just `len` moves onto the destination slots — no
    /// `respond_to?`/`#to_ary` dispatch, no nil padding, nothing that can
    /// raise. Everything else jumps to `slow` (the full `runtime::expand_array`
    /// call, which re-derives its own view of `src`).
    ///
    /// Returns the `exit` label the caller must bind after the runtime call
    /// (which doubles as the fall-through `slow` target), or `None` when no
    /// fast path was emitted and the runtime call stands alone as before.
    ///
    /// A `rest_pos` site (`a, *b = …`) allocates the rest `Array`, so it stays
    /// on the runtime path, as do wide destructurings whose unrolled copy
    /// would outweigh the call it replaces.
    ///
    fn expand_array_fast_path(
        &mut self,
        dst: SlotId,
        len: usize,
        rest_pos: Option<usize>,
    ) -> Option<DestLabel> {
        const MAX_INLINE_EXPAND: usize = 8;
        if rest_pos.is_some() || len == 0 || len > MAX_INLINE_EXPAND {
            return None;
        }
        let heap = self.jit.label();
        let copy = self.jit.label();
        let slow = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq rdx, 0b111;                                   // immediate?
            jnz  slow;
            cmpb [rdx + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  slow;
            movq rax, [rdx + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            // Inline buffer: rax is the length, the elements follow in place.
            cmpq rax, (len);
            jlt  slow;
            lea  rcx, [rdx + (RVALUE_OFFSET_INLINE)];
        copy:
        }
        // `dst` descends: slot `dst.0 + i` sits `i * 8` below `&dst`.
        for i in 0..len {
            let src_disp = (i * 8) as i32;
            let dst_disp = rbp_local(dst) + (i * 8) as i32;
            monoasm! { &mut self.jit,
                movq rax, [rcx + (src_disp)];
                movq [rbp - (dst_disp)], rax;
            }
        }
        monoasm! { &mut self.jit,
            // Non-null rax: `expand_array` signals errors with a null return,
            // and the caller's `handle_error` checks it.
            movq rax, 1;
            jmp  exit;
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        heap:
            // Spilled buffer: rax is the capacity, the length lives beside
            // the pointer.
            movq rcx, [rdx + (RVALUE_OFFSET_HEAP_LEN)];
            cmpq rcx, (len);
            jlt  slow;
            movq rcx, [rdx + (RVALUE_OFFSET_HEAP_PTR)];
            jmp  copy;
        }
        self.jit.select_page(0);
        // The runtime call the caller emits next *is* the slow path.
        self.jit.bind_label(slow);
        Some(exit)
    }

    // ---- method definition (the former per-arch arms, verbatim) ----

    pub(in crate::codegen::jitgen) fn emit_method_def(
        &mut self,
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        self.method_def(name, func_id, using_fpr);
        self.handle_error(error);
        true
    }

    pub(in crate::codegen::jitgen) fn emit_singleton_method_def(
        &mut self,
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_fpr: UsingFpr,
        error: &DestLabel,
    ) -> bool {
        self.singleton_method_def(obj, name, func_id, using_fpr);
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

    pub(in crate::codegen::jitgen) fn emit_ensure_end(
        &mut self,
        pc: BytecodePtr,
        _loop_jit_spill_bytes: usize,
        spliced_break: Option<usize>,
        spliced_ret: Option<usize>,
    ) -> bool {
        let raise = self.entry_raise();
        if spliced_break.is_none() && spliced_ret.is_none() {
            let cont = self.jit.label();
            monoasm! { &mut self.jit,
                movq rdi, rbx;
                movq rax, (runtime::ensure_end);
                call rax;
                testq rax, rax;
                jz   cont;
                movq r13, (pc.as_ptr());
                jmp  raise;
            cont:
            };
            return true;
        }
        // Spliced form (#1185): the runtime dispatch classifies the deferred
        // unwind — rax = code (0 continue / 1 re-raise / 2 spliced break /
        // 3 spliced return), rdx = the delivered value. The teardown arms are
        // the same machine sequence as `BlockBreakSpecialized` /
        // `MethodRetSpecialized`, with the value moved into rax first. A code
        // whose arm was not emitted falls through to the re-raise, whose
        // `entry_raise` surfaces the (error-less) state as a fatal — by
        // construction the runtime only returns codes for kinds this unit
        // spliced.
        let cont = self.jit.label();
        let reraise = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (runtime::ensure_end_spliced);
            call rax;
            testq rax, rax;
            jz   cont;
            cmpq rax, 1;
            jeq  reraise;
        };
        if let Some(off) = spliced_break {
            let skip = self.jit.label();
            monoasm! { &mut self.jit,
                cmpq rax, 2;
                jne  skip;
                movq rax, rdx;
            };
            self.method_return_specialized(off);
            self.jit.bind_label(skip);
        }
        if let Some(off) = spliced_ret {
            let skip = self.jit.label();
            monoasm! { &mut self.jit,
                cmpq rax, 3;
                jne  skip;
                movq rax, rdx;
            };
            self.method_return_specialized(off);
            self.jit.bind_label(skip);
        }
        monoasm! { &mut self.jit,
        reraise:
            movq r13, (pc.as_ptr());
            jmp  raise;
        cont:
        };
        true
    }

    /// Stage 1' write-through: store the raw f64 in `src` (this frame's
    /// fpr, pool or spill) to `[rbp + disp]` — an outer frame's `Sf`
    /// home. `None` elides the store (the binding was widened after
    /// emission). Goes through rax: a plain 8-byte copy, no boxing.
    pub(in crate::codegen::jitgen) fn emit_store_outer_fpr_home_f(
        &mut self,
        src: OuterFprSrc,
        disp: Option<i64>,
        base: usize,
    ) -> bool {
        let Some(disp) = disp else { return true };
        let disp = i32::try_from(disp).expect("outer fpr home displacement out of i32 range");
        match src {
            OuterFprSrc::Fpr(src) => match PhysMap::new(base).resolve(src) {
                FPRegLoc::Xmm(p) => monoasm! { &mut self.jit,
                    movq [rbp + (disp)], xmm(p);
                },
                FPRegLoc::Spill(off) => monoasm! { &mut self.jit,
                    movq rax, [rbp - (off)];
                    movq [rbp + (disp)], rax;
                },
            },
            OuterFprSrc::Imm(bits) => monoasm! { &mut self.jit,
                movq rax, (bits);
                movq [rbp + (disp)], rax;
            },
        }
        true
    }

    /// Stage-C loop-entry init: guard that the Value in `src` is a Float
    /// (jump to `deopt` otherwise, offending value in rdi), unbox it into
    /// xmm0, and store the raw f64 to `[rbp + disp]` — the adopted outer
    /// view's spill home.
    ///
    /// ### destroy
    /// - rax, rdi, xmm0
    pub(in crate::codegen::jitgen) fn emit_guard_float_to_outer_home_f(
        &mut self,
        src: GP,
        disp: i64,
        deopt: &DestLabel,
    ) {
        let disp = i32::try_from(disp).expect("outer home displacement out of i32 range");
        self.float_to_f64(src, 0, deopt);
        monoasm! { &mut self.jit,
            movq [rbp + (disp)], xmm0;
        }
    }

    /// Stage 1'' surrender write: raw f64 at `[rbp + disp]` (the owner's
    /// spill home) into xmm0, boxed via `f64_to_val`, and stored to the
    /// owner's slot through the chain (same addressing as
    /// `store_dyn_var_specialized`).
    ///
    /// ### destroy
    /// - rax, rcx, xmm0
    pub(in crate::codegen::jitgen) fn emit_box_outer_home_to_dynvar(
        &mut self,
        disp: i64,
        offset: usize,
        reg: SlotId,
    ) {
        let disp = i32::try_from(disp).expect("outer home displacement out of i32 range");
        let f64_to_val = self.f64_to_val.clone();
        monoasm! { &mut self.jit,
            movq xmm0, [rbp + (disp)];
            call f64_to_val;
        }
        self.store_dyn_var_specialized(offset, reg, GP::Rax);
    }

    /// Stage 3a home read: raw f64 from `[rbp + disp]` into `dst` (this
    /// frame's fpr, pool or spill). Twin of `emit_store_outer_fpr_home_f`.
    pub(in crate::codegen::jitgen) fn emit_load_outer_fpr_home_f(
        &mut self,
        dst: FPReg,
        disp: i64,
        base: usize,
    ) -> bool {
        let disp = i32::try_from(disp).expect("outer fpr home displacement out of i32 range");
        match PhysMap::new(base).resolve(dst) {
            FPRegLoc::Xmm(p) => monoasm! { &mut self.jit,
                movq xmm(p), [rbp + (disp)];
            },
            FPRegLoc::Spill(off) => monoasm! { &mut self.jit,
                movq rax, [rbp + (disp)];
                movq [rbp - (off)], rax;
            },
        }
        true
    }

    /// A JIT-spliced non-local exit (#1185): build and defer the exit's
    /// unwind (value in rdx) so the following compiled branch enters the
    /// shared `ensure` body directly. A degenerate outcome (the runtime
    /// helper returns non-zero, error left in-flight) raises generically
    /// from the exit's own pc.
    pub(in crate::codegen::jitgen) fn emit_defer_spliced_exit(
        &mut self,
        kind: SplicedExitKind,
        pc: BytecodePtr,
    ) -> bool {
        let raise = self.entry_raise();
        let f = match kind {
            SplicedExitKind::Break => runtime::defer_block_break as *const u8,
            SplicedExitKind::MethodReturn => runtime::defer_method_return as *const u8,
        };
        let cont = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (f);
            call rax;
            testq rax, rax;
            jz   cont;
            movq r13, (pc.as_ptr());
            jmp  raise;
        cont:
        };
        true
    }

    // ---- generic yield (former per-arch arm) ----

    pub(in crate::codegen::jitgen) fn emit_yield(
        &mut self,
        callid: CallSiteId,
        simple: bool,
        error: &DestLabel,
        evict: AsmEvict,
    ) -> bool {
        let return_addr = self.gen_yield(callid, simple, error);
        self.set_deopt_with_return_addr(return_addr, evict);
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
        using_fpr: UsingFpr,
        call_site_bc_ptr: BytecodePtr,
        error: &DestLabel,
    ) -> bool {
        self.block_arg(using_fpr, call_site_bc_ptr);
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
        using_fpr: UsingFpr,
        wb: bool,
    ) -> bool {
        self.store_ivar_heap(src, ivarid, is_object_ty, using_fpr, wb);
        true
    }
}
