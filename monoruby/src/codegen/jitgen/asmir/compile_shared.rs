//! Arch-neutral `AsmInst` lowering dispatcher (step ① of x86/aarch64 JIT
//! emission sharing).
//!
//! The AsmIR→machine-code lowering used to be two fully parallel matches —
//! `arch/x86_64/compile/*.rs` (x86, `monoasm!`) and `arch/aarch64/compile.rs`
//! (aarch64, `monoasm_arm64!`). Instructions whose lowering is identical in
//! *structure* (only the emitted bytes differ) are handled here ONCE: each is
//! lowered to one or more arch-neutral `LInst`s and handed to the per-arch
//! `Codegen::encode_linst` (the single machine-code emission seam; see
//! `doc/lir.md`). Macro-op `LInst`s that still wrap a substantive per-arch
//! helper are routed through `encode_linst_macro` below. Everything else is
//! forwarded to the per-arch `compile_asmir_arch`. Coverage of the shared match
//! grows one instruction family at a time; see `doc/arch_difference.md`.

use super::*;
use crate::codegen::jitgen::lir::{LAluOp, LCond, LInst, LMem, LOperand, LReg};

impl Codegen {
    ///
    /// Lower one `AsmInst`. The single entry point both backends' drivers call.
    ///
    /// Both backends lower every `AsmInst`, so this never bails — the `bool`
    /// it (and the per-arch emission primitives) returns is always `true` and
    /// is ignored by the callers. It is kept only because flipping ~80 dispatch
    /// arms and ~100 leaf emitters to `()` is pure churn; the driver chain
    /// (`gen_asm` / `gen_machine_code` / `jit_compile`) no longer acts on it.
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
                self.encode_linst_frame(LInst::SourcePos { idx: i }, frame);
            }
            // Bind a JIT label at the current position.
            AsmInst::Label(label) => {
                let label = frame.resolve_label(&mut self.jit, label);
                self.encode_linst(LInst::BindLabel(label));
            }
            // dst <- src
            AsmInst::RegMove(src, dst) => self.encode_linst(LInst::Mov {
                dst: dst.into(),
                src: src.into(),
            }),
            // [slot] <- reg
            AsmInst::RegToStack(r, slot) => self.encode_linst(LInst::Store {
                src: r,
                mem: LMem::Slot(slot),
            }),
            // [lfp - slot] <- reg (LFP-relative; follows a heap-moved frame)
            AsmInst::RegToLfpStack(r, slot) => self.encode_linst(LInst::Store {
                src: r,
                mem: LMem::LfpSlot(slot),
            }),
            // reg <- [slot]
            AsmInst::StackToReg(slot, r) => self.encode_linst(LInst::Load {
                dst: r.into(),
                mem: LMem::Slot(slot),
            }),
            // reg <- literal Value (immediate)
            AsmInst::LitToReg(v, r) => self.encode_linst(LInst::LoadImm {
                dst: r.into(),
                imm: v.id(),
            }),
            // [slot] <- literal Value. The encoder legalizes the immediate
            // (aarch64 stages an over-large frame offset through a scratch reg).
            AsmInst::LitToStack(v, slot) => self.encode_linst(LInst::StoreImm {
                imm: v.id(),
                mem: LMem::Slot(slot),
            }),
            // Conditional branch on the truthiness of the accumulator.
            AsmInst::CondBr(brkind, dest) => {
                let target = frame.resolve_label(&mut self.jit, dest);
                self.encode_linst(LInst::BranchTruthy {
                    negate: brkind == BrKind::BrIfNot,
                    target,
                });
            }
            // Branch to dest if the accumulator is nil.
            AsmInst::NilBr(dest) => {
                let target = frame.resolve_label(&mut self.jit, dest);
                self.encode_linst(LInst::BranchIfNil { target });
            }
            // Branch to dest if the local (accumulator) is already set (non-zero).
            AsmInst::CheckLocal(dest) => {
                let target = frame.resolve_label(&mut self.jit, dest);
                self.encode_linst(LInst::BranchIfNonzero { target });
            }
            // Dense-integer `case`: range-check the (fixnum) condition against
            // `[min, max]`, then dispatch through a jump table of absolute
            // branch-target addresses (else for out-of-range). Terminates the
            // basic block (no fallthrough).
            AsmInst::OptCase {
                max,
                min,
                else_label,
                branch_labels,
            } => {
                let branch_dests: Box<[DestLabel]> = branch_labels
                    .iter()
                    .map(|l| frame.resolve_label(&mut self.jit, *l))
                    .collect();
                let else_dest = frame.resolve_label(&mut self.jit, else_label);
                self.encode_linst(LInst::OptCase {
                    max,
                    min,
                    else_dest,
                    branch_dests,
                });
            }
            // Type guard: deopt if `r`'s runtime class is not `class`.
            AsmInst::GuardClass(r, class, deopt) => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardClass {
                    reg: r,
                    class,
                    deopt,
                });
            }
            // Unconditional jump to a side-exit (deopt) label.
            AsmInst::Deopt(deopt) => self.encode_linst(LInst::Deopt {
                deopt: labels[deopt].clone(),
            }),
            // Branch to the error handler if the preceding runtime call failed
            // (returned a null/None result in the accumulator).
            AsmInst::HandleError(error) => self.encode_linst(LInst::HandleError {
                error: labels[error].clone(),
            }),
            // Stack-overflow check before establishing a callee frame (aarch64
            // bails if the write-back needs an unsupported feature).
            AsmInst::CheckStack { write_back, error } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::CheckStack {
                    write_back,
                    error,
                    base: frame.base_stack_offset,
                });
            }
            // GC safepoint.
            AsmInst::ExecGc { write_back, error } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::ExecGc {
                    write_back,
                    error,
                    base: frame.base_stack_offset,
                });
            }
            // Constant base-class guard: deopt if the constant's base class (in
            // the accumulator) is not the cached one.
            AsmInst::GuardConstBaseClass { base_class, deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardConstBaseClass { base_class, deopt });
            }
            // Constant version guard: deopt if the global constant version moved
            // since compilation.
            AsmInst::GuardConstVersion { const_version, deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardConstVersion {
                    const_version,
                    deopt,
                });
            }
            // Store to a constant, bumping the global constant version (aarch64
            // bails if any fpr is live, hence the bool result).
            AsmInst::StoreConstant { id, using_fpr, error } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::StoreConstant { id, using_fpr, error })
            }
            // Variable access. gvar/cvar go via a runtime call; dynvar walks the
            // outer-LFP chain.
            AsmInst::LoadGVar { name, using_fpr } => {
                self.encode_linst(LInst::LoadGVar { name, using_fpr })
            }
            AsmInst::StoreGVar { name, src, using_fpr } => {
                self.encode_linst(LInst::StoreGVar { name, src, using_fpr })
            }
            AsmInst::LoadCVar { name, using_fpr } => {
                self.encode_linst(LInst::LoadCVar { name, using_fpr })
            }
            AsmInst::LoadDynVar { src } => self.encode_linst(LInst::LoadDynVar { src }),
            AsmInst::StoreDynVar { dst, src } => {
                self.encode_linst(LInst::StoreDynVar { dst, src })
            }
            // Runtime allocation / C-call family: each builds a heap object via a
            // runtime call.
            AsmInst::CreateArray { src, len } => {
                self.encode_linst(LInst::CreateArray { src, len })
            }
            AsmInst::NewArray { callid, using_fpr } => {
                self.encode_linst(LInst::NewArray { callid, using_fpr })
            }
            AsmInst::NewHash(args, len, using_fpr) => {
                self.encode_linst(LInst::NewHash { args, len, using_fpr })
            }
            AsmInst::HashInsert { hash, args, len, using_fpr } => {
                self.encode_linst(LInst::HashInsert { hash, args, len, using_fpr })
            }
            AsmInst::ArrayConcat { dst, src, using_fpr } => {
                self.encode_linst(LInst::ArrayConcat { dst, src, using_fpr })
            }
            AsmInst::NewRange { start, end, exclude_end, using_fpr } => {
                self.encode_linst(LInst::NewRange { start, end, exclude_end, using_fpr })
            }
            AsmInst::ConcatStr { arg, len, using_fpr } => {
                self.encode_linst(LInst::ConcatStr { arg, len, using_fpr })
            }
            AsmInst::ToA { src, using_fpr } => self.encode_linst(LInst::ToA { src, using_fpr }),
            AsmInst::DeepCopyLit(v, using_fpr) => {
                self.encode_linst(LInst::DeepCopyLit { v, using_fpr })
            }
            // Floating-point register transfer/convert family (aarch64 bails if
            // the FP pool register is not lowerable, hence the bool results).
            // `base` is the spill base; deopt is a side-exit label.
            AsmInst::FprMove(src, dst) => self.encode_linst(LInst::FprMove {
                src,
                dst,
                base: frame.base_stack_offset,
            }),
            AsmInst::FprSwap(l, r) => self.encode_linst(LInst::FprSwap {
                lhs: l,
                rhs: r,
                base: frame.base_stack_offset,
            }),
            AsmInst::F64ToFpr(f, x) => self.encode_linst(LInst::F64ToFpr {
                f,
                dst: x,
                base: frame.base_stack_offset,
            }),
            AsmInst::FixnumToFpr(r, x) => self.encode_linst(LInst::FixnumToFpr {
                src: r,
                dst: x,
                base: frame.base_stack_offset,
            }),
            AsmInst::FloatToFpr(reg, x, deopt) => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::FloatToFpr {
                    src: reg,
                    dst: x,
                    deopt,
                    base: frame.base_stack_offset,
                });
            }
            AsmInst::FprToStack(x, slot) => self.encode_linst(LInst::FprToStack {
                src: x,
                slot,
                base: frame.base_stack_offset,
            }),
            // Save / restore live FP pool registers around a C-call.
            AsmInst::FprSave(using_fpr, cont) => {
                self.encode_linst(LInst::FprSave { using_fpr, cont })
            }
            AsmInst::FprRestore(using_fpr, cont) => {
                self.encode_linst(LInst::FprRestore { using_fpr, cont })
            }
            // Integer / float arithmetic fast paths. Each backend resolves the
            // same operands and dispatches to its own emission primitive
            // (aarch64 bails on an unsupported BinOpK, hence the bool results).
            AsmInst::IntegerBinOp {
                kind,
                lhs,
                rhs,
                mode,
                deopt,
            } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::IntegerBinOp {
                    kind,
                    mode,
                    lhs,
                    rhs,
                    deopt,
                });
            }
            // §slot-IR: lower the slot-based fixnum binop to the physical
            // sequence — load each operand slot into a scratch reg, fixnum-guard
            // it, compute in place in `Rdi` (overflow -> deopt), and store the
            // result to `dst`'s slot. This is where the GP registers the AsmIR
            // layer no longer carries are materialized (`%dst = %lhs op %rhs` ->
            // `Rdi=%lhs; Rsi=%rhs; Rdi=Rdi op Rsi; %dst=Rdi`).
            AsmInst::IntegerBinOpSlot {
                kind,
                dst,
                mode,
                deopt,
            } => {
                let deopt = labels[deopt].clone();
                // Place the slot operand(s) into the registers `integer_binop`
                // expects for this (mode, kind) and fixnum-guard each; the
                // immediate (if any) is folded by `integer_binop` from `mode`.
                // The result is computed in place in `Rdi` (= `lhs`).
                let mut load_guard = |me: &mut Self, slot: SlotId, reg: GP| {
                    me.encode_linst(LInst::Load {
                        dst: reg.into(),
                        mem: LMem::Slot(slot),
                    });
                    me.encode_linst(LInst::GuardClass {
                        reg,
                        class: INTEGER_CLASS,
                        deopt: deopt.clone(),
                    });
                };
                // `Div` needs both operands in registers (Rdi=lhs, Rsi=rhs) with
                // immediates materialized, and produces its quotient in `rax`.
                // Add/Sub/Mul fold the immediate from `mode` and compute in place
                // in `Rdi` (with a commutative-aware operand placement).
                let result_reg = if matches!(kind, BinOpK::Div) {
                    match mode {
                        OpMode::RR(l, r) => {
                            load_guard(self, l, GP::Rdi);
                            load_guard(self, r, GP::Rsi);
                        }
                        OpMode::RI(l, i) => {
                            load_guard(self, l, GP::Rdi);
                            self.encode_linst(LInst::LoadImm {
                                dst: GP::Rsi.into(),
                                imm: Value::i32(i as i32).id(),
                            });
                        }
                        OpMode::IR(i, r) => {
                            self.encode_linst(LInst::LoadImm {
                                dst: GP::Rdi.into(),
                                imm: Value::i32(i as i32).id(),
                            });
                            load_guard(self, r, GP::Rsi);
                        }
                    }
                    GP::Rax
                } else {
                    match mode {
                        OpMode::RR(l, r) => {
                            load_guard(self, l, GP::Rdi);
                            load_guard(self, r, GP::Rsi);
                        }
                        // rhs is the immediate: only lhs slot is loaded (into Rdi).
                        OpMode::RI(l, _) => load_guard(self, l, GP::Rdi),
                        // lhs is the immediate. Add/Mul are commutative, so the reg
                        // operand becomes the in-place accumulator (Rdi); Sub
                        // materializes the immediate into Rdi and subtracts the reg
                        // operand from Rsi.
                        OpMode::IR(_, r) => {
                            let reg = if matches!(kind, BinOpK::Sub) {
                                GP::Rsi
                            } else {
                                GP::Rdi
                            };
                            load_guard(self, r, reg);
                        }
                    }
                    GP::Rdi
                };
                self.encode_linst(LInst::IntegerBinOp {
                    kind,
                    mode,
                    lhs: GP::Rdi,
                    rhs: GP::Rsi,
                    deopt,
                });
                if let Some(dst) = dst {
                    self.encode_linst(LInst::Store {
                        src: result_reg,
                        mem: LMem::Slot(dst),
                    });
                }
            }
            AsmInst::IntegerCmp {
                mode,
                kind,
                lhs,
                rhs,
            } => self.encode_linst(LInst::IntegerCmp { kind, mode, lhs, rhs }),
            // §slot-IR: lower the slot-based fixnum comparison — load each operand
            // slot into the scratch reg `integer_cmp` expects (Rdi=lhs, Rsi=rhs;
            // RI loads only lhs, IR only rhs), fixnum-guard it, compare (result in
            // rax), and store the boolean to `dst`'s slot.
            AsmInst::IntegerCmpSlot {
                mode,
                kind,
                dst,
                deopt,
            } => {
                let deopt = labels[deopt].clone();
                let mut load_guard = |me: &mut Self, slot: SlotId, reg: GP| {
                    me.encode_linst(LInst::Load {
                        dst: reg.into(),
                        mem: LMem::Slot(slot),
                    });
                    me.encode_linst(LInst::GuardClass {
                        reg,
                        class: INTEGER_CLASS,
                        deopt: deopt.clone(),
                    });
                };
                match mode {
                    OpMode::RR(l, r) => {
                        load_guard(self, l, GP::Rdi);
                        load_guard(self, r, GP::Rsi);
                    }
                    OpMode::RI(l, _) => load_guard(self, l, GP::Rdi),
                    OpMode::IR(_, r) => load_guard(self, r, GP::Rsi),
                }
                self.encode_linst(LInst::IntegerCmp {
                    kind,
                    mode,
                    lhs: GP::Rdi,
                    rhs: GP::Rsi,
                });
                if let Some(dst) = dst {
                    self.encode_linst(LInst::Store {
                        src: GP::Rax,
                        mem: LMem::Slot(dst),
                    });
                }
            }
            // Fused integer compare + conditional branch, lowered to LIR here
            // (arch-neutral) and encoded per-arch. The compare sets flags; the
            // branch is a signed conditional jump with the BrKind inversion
            // folded into the condition.
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let target = frame.resolve_label(&mut self.jit, branch_dest);
                match mode {
                    OpMode::RR(..) => self.encode_linst(LInst::Cmp {
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                    }),
                    OpMode::RI(_, i) => self.encode_linst(LInst::Cmp {
                        lhs: lhs.into(),
                        rhs: LOperand::Imm(Value::i32(i as i32).id() as i64),
                    }),
                    // imm on the left: materialize it into lhs, then compare
                    // reg-reg (mirrors `cmp_integer`'s IR path, which clobbers
                    // lhs).
                    OpMode::IR(i, _) => {
                        self.encode_linst(LInst::LoadImm {
                            dst: lhs.into(),
                            imm: Value::i32(i as i32).id(),
                        });
                        self.encode_linst(LInst::Cmp {
                            lhs: lhs.into(),
                            rhs: rhs.into(),
                        });
                    }
                }
                // TEq compares like Eq for integers; BrIfNot takes the inverse.
                let mut cond = LCond::from_int_cmp(kind).unwrap_or(LCond::Eq);
                if brkind == BrKind::BrIfNot {
                    cond = cond.invert();
                }
                self.encode_linst(LInst::CondBr { cond, target });
            }
            // §slot-IR: lower the slot-based compare+branch — load each operand
            // slot into the scratch reg the compare expects (Rdi=lhs, Rsi=rhs),
            // fixnum-guard it, then emit the same Cmp/CondBr as `IntegerCmpBr`.
            AsmInst::IntegerCmpBrSlot {
                mode,
                kind,
                brkind,
                branch_dest,
                deopt,
            } => {
                let deopt = labels[deopt].clone();
                let mut load_guard = |me: &mut Self, slot: SlotId, reg: GP| {
                    me.encode_linst(LInst::Load {
                        dst: reg.into(),
                        mem: LMem::Slot(slot),
                    });
                    me.encode_linst(LInst::GuardClass {
                        reg,
                        class: INTEGER_CLASS,
                        deopt: deopt.clone(),
                    });
                };
                match mode {
                    OpMode::RR(l, r) => {
                        load_guard(self, l, GP::Rdi);
                        load_guard(self, r, GP::Rsi);
                    }
                    OpMode::RI(l, _) => load_guard(self, l, GP::Rdi),
                    OpMode::IR(_, r) => load_guard(self, r, GP::Rsi),
                }
                let target = frame.resolve_label(&mut self.jit, branch_dest);
                match mode {
                    OpMode::RR(..) => self.encode_linst(LInst::Cmp {
                        lhs: GP::Rdi.into(),
                        rhs: GP::Rsi.into(),
                    }),
                    OpMode::RI(_, i) => self.encode_linst(LInst::Cmp {
                        lhs: GP::Rdi.into(),
                        rhs: LOperand::Imm(Value::i32(i as i32).id() as i64),
                    }),
                    OpMode::IR(i, _) => {
                        self.encode_linst(LInst::LoadImm {
                            dst: GP::Rdi.into(),
                            imm: Value::i32(i as i32).id(),
                        });
                        self.encode_linst(LInst::Cmp {
                            lhs: GP::Rdi.into(),
                            rhs: GP::Rsi.into(),
                        });
                    }
                }
                let mut cond = LCond::from_int_cmp(kind).unwrap_or(LCond::Eq);
                if brkind == BrKind::BrIfNot {
                    cond = cond.invert();
                }
                self.encode_linst(LInst::CondBr { cond, target });
            }
            AsmInst::FloatBinOp {
                kind,
                binary_fpr,
                dst,
            } => self.encode_linst(LInst::FloatBinOp {
                kind,
                lhs: binary_fpr.0,
                rhs: binary_fpr.1,
                dst,
                base: frame.base_stack_offset,
            }),
            AsmInst::FloatUnOp { kind, dst } => self.encode_linst(LInst::FloatUnOp {
                kind,
                dst,
                base: frame.base_stack_offset,
            }),
            // [slot] <- Value::integer(i) and fpr(x) <- i as f64 (constant int
            // materialized as both a boxed integer and a double).
            AsmInst::I64ToBoth(i, slot, x) => self.encode_linst(LInst::I64ToBoth {
                i,
                slot,
                dst: x,
                base: frame.base_stack_offset,
            }),
            // Float comparison. NaN compares false (except `!=`); each backend
            // picks NaN-correct condition codes (x86 ucomisd + setp tricks,
            // aarch64 fcmp + MI/LS conditions).
            AsmInst::FloatCmp { kind, lhs, rhs } => self.encode_linst(LInst::FloatCmp {
                kind,
                lhs,
                rhs,
                base: frame.base_stack_offset,
            }),
            AsmInst::FloatCmpBr {
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let dest = frame.resolve_label(&mut self.jit, branch_dest);
                self.encode_linst(LInst::FloatCmpBr {
                    kind,
                    lhs,
                    rhs,
                    brkind,
                    dest,
                    base: frame.base_stack_offset,
                });
            }
            // Method return / eviction family. `Ret` tears down the frame and
            // returns; `MethodRet` sets the resume PC then returns through the
            // method-return path; `BlockBreak` does the same through the
            // block-break path (a non-local `break` out of a block);
            // `ImmediateEvict` records a return-address patch point on x86 (a
            // no-op on aarch64, which can't patch them).
            AsmInst::Ret => self.encode_linst(LInst::Ret),
            AsmInst::MethodRet(pc) => self.encode_linst(LInst::MethodRet { pc }),
            AsmInst::BlockBreak(pc) => self.encode_linst(LInst::BlockBreak { pc }),
            AsmInst::ImmediateEvict { evict } => self.encode_linst(LInst::ImmediateEvict { evict }),
            // Method-call prologue: class-version guard, callee frame fields,
            // argument massage. (aarch64 SetArguments bails on a not-yet-ported
            // argument shape, hence the bool result; the guard ignores the x86
            // recompile params it has no recompiler for.)
            AsmInst::GuardClassVersion {
                position,
                with_recovery,
                deopt,
            } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardClassVersion {
                    class_version,
                    position,
                    with_recovery,
                    deopt,
                });
            }
            AsmInst::SetupMethodFrame {
                meta,
                callid,
                outer_lfp,
            } => {
                let callsite = &store[callid];
                let (block_fid, block_arg) = (callsite.block_fid, callsite.block_arg);
                self.encode_linst(LInst::SetupMethodFrame {
                    meta,
                    outer_lfp,
                    block_fid,
                    block_arg,
                });
            }
            AsmInst::SetArguments { callid, callee_fid } => {
                let offset = store[callee_fid].get_offset();
                self.encode_linst(LInst::SetArguments {
                    callid,
                    callee_fid,
                    offset,
                });
            }
            // Basic-operator-redefinition guard: deopt if any BOP was redefined.
            AsmInst::CheckBOP { deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::CheckBOP { deopt });
            }
            // Recompile-or-deopt point: both arches recompile the whole method
            // (or loop body) once a small miss counter warms, then deopt.
            // (Specialized frames recompile via RecompileDeoptSpecialized /
            // GuardClassVersionSpecialized in the per-arch lowering.)
            AsmInst::RecompileDeopt {
                position,
                deopt,
                error,
                reason,
            } => {
                let error = error.map(|e| labels[e].clone());
                self.encode_linst(LInst::RecompileDeopt {
                    position,
                    deopt: labels[deopt].clone(),
                    error,
                    reason,
                });
            }
            // The call itself. x86 records a return-address deopt patch point;
            // aarch64 has no branch patching (class-version guards cover it).
            AsmInst::Call {
                callee_fid,
                recv_class,
                evict,
                pc,
            } => {
                let evict_label = labels[evict].clone();
                let callee = &store[callee_fid];
                let is_iseq = callee.is_iseq();
                let (_, codeptr, callee_pc) = callee.get_data();
                // x86 JIT-entry lookup (aarch64 ignores it; the lookup is a
                // side-effect-free table read, so pre-resolving here is safe).
                let jit_entry = is_iseq.and_then(|iseq| store[iseq].get_jit_entry(recv_class));
                self.encode_linst(LInst::Call {
                    codeptr,
                    is_iseq: is_iseq.is_some(),
                    callee_pc,
                    call_site_bc_ptr: pc,
                    jit_entry,
                    evict,
                    evict_label,
                });
            }
            // Method prologue: establish fp/lr, reserve the local frame, nil-fill
            // non-argument locals (aarch64 bails if the frame exceeds the 12-bit
            // sub-sp immediate).
            AsmInst::Init {
                info,
                prologue_offset,
            } => self.encode_linst(LInst::Init {
                info,
                prologue_offset,
            }),
            // Per-method ivar-cache prep. The store/frame-dependent heap length
            // is resolved here; the encoder only emits the table-extend guard.
            AsmInst::Preparation => {
                let heap_len = if !frame.self_class.is_always_frozen() && frame.ivar_heap_accessed {
                    let ivar_len = store[frame.self_class].ivar_len();
                    Some(if frame.self_ty == Some(ObjTy::OBJECT) {
                        ivar_len - OBJECT_INLINE_IVAR
                    } else {
                        ivar_len
                    })
                } else {
                    None
                };
                self.encode_linst(LInst::Preparation { heap_len });
            }
            // Fixnum unary ops on the tagged value. Negate deopts on i63
            // overflow (e.g. -i63::MIN); bitwise-not cannot overflow.
            AsmInst::FixnumNeg { reg, deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::FixnumNeg { reg, deopt });
            }
            AsmInst::FixnumBitNot { reg } => self.encode_linst(LInst::FixnumBitNot { reg }),
            // Type guards: deopt unless `reg` is an Array / the receiver in rdi
            // is unfrozen.
            AsmInst::GuardArrayTy(reg, deopt) => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardArrayTy { reg, deopt });
            }
            AsmInst::GuardFrozen { deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardFrozen { deopt });
            }
            // Inline instance-variable / struct-member access on the receiver in
            // rdi (no Box deref). aarch64 bails if the field offset exceeds the
            // 12-bit scaled load/store immediate.
            // Inline ivar load: `r15 <- self.@ivar` at a fixed field offset on
            // the receiver (rdi); an unset slot reads as 0 and becomes nil.
            AsmInst::LoadIVarInline { ivarid } => {
                let disp = RVALUE_OFFSET_KIND as i32 + ivarid.get() as i32 * 8;
                self.encode_linst(LInst::Load {
                    dst: GP::R15.into(),
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp,
                    },
                });
                self.encode_linst(LInst::NilIfZero { reg: GP::R15 });
            }
            // Inline ivar store: `self.@ivar = src` at a fixed field offset on
            // the receiver (rdi), followed by the GC write barrier.
            AsmInst::StoreIVarInline { src, ivarid } => {
                let disp = RVALUE_OFFSET_KIND as i32 + ivarid.get() as i32 * 8;
                self.encode_linst(LInst::Store {
                    src,
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp,
                    },
                });
                self.encode_linst(LInst::WriteBarrier {
                    parent: GP::Rdi,
                    value: src,
                });
            }
            // Inline struct-member load: `r15 <- self.@slot` at a fixed field
            // offset on the receiver (rdi). Lowered to a field load whose
            // (positive) displacement the encoder legalizes per arch.
            AsmInst::LoadStructSlotInline { slot_index } => {
                let disp = slot_index as i32 * 8 + RVALUE_OFFSET_INLINE as i32;
                self.encode_linst(LInst::Load {
                    dst: GP::R15.into(),
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp,
                    },
                });
            }
            // Inline struct-member store: field store + write barrier, with the
            // stored value also returned in the accumulator (rax).
            AsmInst::StoreStructSlotInline { src, slot_index } => {
                let disp = slot_index as i32 * 8 + RVALUE_OFFSET_INLINE as i32;
                self.encode_linst(LInst::Store {
                    src,
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp,
                    },
                });
                self.encode_linst(LInst::WriteBarrier {
                    parent: GP::Rdi,
                    value: src,
                });
                self.encode_linst(LInst::Mov {
                    dst: GP::Rax.into(),
                    src: src.into(),
                });
            }
            // Heap (Box-spilled) Struct member access: deref the heap buffer
            // pointer into rdi, then load/store the slot at `slot_index * 8`.
            AsmInst::LoadStructSlotHeap { slot_index } => {
                self.encode_linst(LInst::Load {
                    dst: GP::Rdi.into(),
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp: RVALUE_OFFSET_HEAP_PTR as i32,
                    },
                });
                self.encode_linst(LInst::Load {
                    dst: GP::R15.into(),
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp: slot_index as i32 * 8,
                    },
                });
            }
            AsmInst::StoreStructSlotHeap { src, slot_index } => {
                // Barrier first: it needs rdi = the struct (parent), before the
                // deref repoints rdi at the heap buffer.
                self.encode_linst(LInst::WriteBarrier {
                    parent: GP::Rdi,
                    value: src,
                });
                self.encode_linst(LInst::Load {
                    dst: GP::Rdi.into(),
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp: RVALUE_OFFSET_HEAP_PTR as i32,
                    },
                });
                self.encode_linst(LInst::Store {
                    src,
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp: slot_index as i32 * 8,
                    },
                });
                self.encode_linst(LInst::Mov {
                    dst: GP::Rax.into(),
                    src: src.into(),
                });
            }
            // reg += i / reg -= i (no-op when i == 0).
            AsmInst::RegAdd(reg, i) => self.encode_linst(LInst::Alu {
                op: LAluOp::Add,
                dst: reg.into(),
                lhs: reg.into(),
                rhs: LOperand::Imm(i as i64),
            }),
            AsmInst::RegSub(reg, i) => self.encode_linst(LInst::Alu {
                op: LAluOp::Sub,
                dst: reg.into(),
                lhs: reg.into(),
                rhs: LOperand::Imm(i as i64),
            }),
            // Loop-JIT entry stack bump (aarch64 bails on a frame larger than
            // the 12-bit sub-sp immediate).
            AsmInst::LoopJitRspBump { offset } => self.encode_linst(LInst::LoopJitRspBump { offset }),
            // Inline argument-setup stores into the callee frame, at a
            // (raw) rsp-relative offset the encoder legalizes per arch.
            AsmInst::RegToRSPOffset(r, ofs) => self.encode_linst(LInst::Store {
                src: r,
                mem: LMem::RspRel { disp: ofs },
            }),
            AsmInst::ZeroToRSPOffset(ofs) => self.encode_linst(LInst::StoreImm {
                imm: 0,
                mem: LMem::RspRel { disp: ofs },
            }),
            AsmInst::U64ToRSPOffset(i, ofs) => self.encode_linst(LInst::StoreImm {
                imm: i,
                mem: LMem::RspRel { disp: ofs },
            }),
            // Side-effect guard for block-passing calls: deopt if the frame was
            // captured/promoted.
            AsmInst::GuardCapture(deopt) => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::GuardCapture { deopt });
            }
            // `&block` forwarding: proxy the block handler, or materialize it
            // into a Proc value (aarch64 bails on a live fpr / range overflow).
            AsmInst::BlockArgProxy { ret, outer } => self.encode_linst(LInst::BlockArgProxy { ret, outer }),
            AsmInst::BlockArg { ret, _outer: _, using_fpr, error, call_site_bc_ptr } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::BlockArg {
                    ret,
                    using_fpr,
                    call_site_bc_ptr,
                    error,
                });
            }
            // Store into a heap-spilled instance variable of self (the table is
            // known large enough, so no bounds check / runtime extend).
            // `self.@ivar = src` where the ivar spilled to self's heap var-table
            // (which is known large enough — no bounds check). The two derefs
            // (RValue → var-table struct → buffer pointer) go through the scratch
            // pointer so rdi is preserved as the barrier's parent.
            AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
            } => {
                let ivar = ivarid.get() as i32;
                let idx = if is_object_ty {
                    ivar - OBJECT_INLINE_IVAR as i32
                } else {
                    ivar
                };
                self.encode_linst(LInst::Load {
                    dst: LReg::Scratch,
                    mem: LMem::Field {
                        base: GP::Rdi.into(),
                        disp: RVALUE_OFFSET_VAR as i32,
                    },
                });
                self.encode_linst(LInst::Load {
                    dst: LReg::Scratch,
                    mem: LMem::Field {
                        base: LReg::Scratch,
                        disp: MONOVEC_PTR as i32,
                    },
                });
                self.encode_linst(LInst::Store {
                    src,
                    mem: LMem::Field {
                        base: LReg::Scratch,
                        disp: idx * 8,
                    },
                });
                self.encode_linst(LInst::WriteBarrier {
                    parent: GP::Rdi,
                    value: src,
                });
            }
            // Store into a heap-spilled ivar of another object (bounds-checked;
            // grows the table via a runtime call on miss). aarch64 bails on an
            // out-of-range field offset.
            AsmInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                using_fpr,
            } => self.encode_linst(LInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                using_fpr,
            }),
            // Load a heap-spilled instance variable (bounds-checked unless self),
            // substituting nil for an out-of-range / unset slot.
            AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_,
            } => self.encode_linst(LInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_,
            }),
            // Runtime-call definition ops (undef a method / alias a global var).
            // aarch64 bails when an fpr pool register is live (no fpr save yet).
            AsmInst::UndefMethod { undef, using_fpr } => {
                self.encode_linst(LInst::UndefMethod { undef, using_fpr })
            }
            AsmInst::AliasGvar { new, old, using_fpr } => {
                self.encode_linst(LInst::AliasGvar { new, old, using_fpr })
            }
            // Runtime-call class-variable / method-alias ops.
            AsmInst::CheckCVar { name, using_fpr } => {
                self.encode_linst(LInst::CheckCVar { name, using_fpr })
            }
            AsmInst::StoreCVar { name, src, using_fpr } => {
                self.encode_linst(LInst::StoreCVar { name, src, using_fpr })
            }
            AsmInst::AliasMethod { new, old, using_fpr } => {
                self.encode_linst(LInst::AliasMethod { new, old, using_fpr })
            }
            // defined? runtime-call family.
            AsmInst::DefinedYield { dst, using_fpr } => {
                self.encode_linst(LInst::DefinedYield { dst, using_fpr })
            }
            AsmInst::DefinedSuper { dst, using_fpr } => {
                self.encode_linst(LInst::DefinedSuper { dst, using_fpr })
            }
            AsmInst::DefinedGvar { dst, name, using_fpr } => {
                self.encode_linst(LInst::DefinedGvar { dst, name, using_fpr })
            }
            AsmInst::DefinedCvar { dst, name, using_fpr } => {
                self.encode_linst(LInst::DefinedCvar { dst, name, using_fpr })
            }
            AsmInst::DefinedConst { dst, siteid, using_fpr } => {
                self.encode_linst(LInst::DefinedConst { dst, siteid, using_fpr })
            }
            AsmInst::DefinedMethod { dst, recv, name, using_fpr } => {
                self.encode_linst(LInst::DefinedMethod { dst, recv, name, using_fpr })
            }
            AsmInst::DefinedIvar { dst, name, using_fpr } => {
                self.encode_linst(LInst::DefinedIvar { dst, name, using_fpr })
            }
            // Generic binary-op / Array=== runtime calls.
            AsmInst::GenericBinOp { lhs, rhs, func, using_fpr } => {
                self.encode_linst(LInst::GenericBinOp { lhs, rhs, func, using_fpr })
            }
            AsmInst::OptEqCmp { lhs, rhs, kind, func, using_fpr } => {
                self.encode_linst(LInst::OptEqCmp { lhs, rhs, kind, func, using_fpr })
            }
            AsmInst::ArrayTEq { lhs, rhs, using_fpr } => {
                self.encode_linst(LInst::ArrayTEq { lhs, rhs, using_fpr })
            }
            // Regexp interpolation / keyword-rest fixup runtime calls.
            AsmInst::ConcatRegexp { arg, len, using_fpr } => {
                self.encode_linst(LInst::ConcatRegexp { arg, len, using_fpr })
            }
            AsmInst::CheckKwRest(slot) => self.encode_linst(LInst::CheckKwRest { slot }),
            // Multiple-assignment array expansion.
            AsmInst::ExpandArray { dst, len, rest_pos, using_fpr } => {
                self.encode_linst(LInst::ExpandArray { dst, len, rest_pos, using_fpr })
            }
            // Float C-function calls (Math.sqrt/sin/…): save the live FP pool
            // around the call.
            AsmInst::CFunc_F_F { f, src, dst, using_fpr } => self.encode_linst(LInst::CFunc_F_F {
                f,
                src,
                dst,
                using_fpr,
                base: frame.base_stack_offset,
            }),
            AsmInst::CFunc_FF_F { f, lhs, rhs, dst, using_fpr } => {
                self.encode_linst(LInst::CFunc_FF_F {
                    f,
                    lhs,
                    rhs,
                    dst,
                    using_fpr,
                    base: frame.base_stack_offset,
                })
            }
            // Method definition (`def`). aarch64 bails on a live fpr pool reg.
            AsmInst::MethodDef { name, func_id, using_fpr, error } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::MethodDef {
                    name,
                    func_id,
                    using_fpr,
                    error,
                });
            }
            AsmInst::SingletonMethodDef { obj, name, func_id, using_fpr, error } => {
                let error = labels[error].clone();
                self.encode_linst(LInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_fpr,
                    error,
                });
            }
            // Exception / non-local control flow (raise / retry / redo / ensure).
            // All branch into the shared entry_raise unwind path.
            AsmInst::Raise => self.encode_linst(LInst::Raise {
                loop_jit_spill_bytes: frame.loop_jit_spill_bytes,
            }),
            AsmInst::Retry(pc) => self.encode_linst(LInst::Retry {
                pc,
                loop_jit_spill_bytes: frame.loop_jit_spill_bytes,
            }),
            AsmInst::Redo(pc) => self.encode_linst(LInst::Redo {
                pc,
                loop_jit_spill_bytes: frame.loop_jit_spill_bytes,
            }),
            AsmInst::EnsureEnd => self.encode_linst(LInst::EnsureEnd {
                loop_jit_spill_bytes: frame.loop_jit_spill_bytes,
            }),
            // Generic `yield` (block target resolved at runtime). aarch64 builds
            // the block frame and calls the funcdata indirectly; the x86-only
            // return-address eviction patch is applied by the x86 emit_yield.
            AsmInst::Yield { callid, error, evict } => {
                let evict_label = labels[evict].clone();
                let error = labels[error].clone();
                self.encode_linst(LInst::Yield {
                    callid,
                    error,
                    evict,
                    evict_label,
                });
            }
            // ---- Specialized inlined-frame family ------------------------------
            // These lower an inlined callee / block frame. Each arm is identical
            // on both arches and dispatches to a per-arch method of the same name
            // (defined in `arch/x86_64/compile/` for x86, `arch/aarch64/compile.rs` for aarch64).
            // Clean return / block-break out of an inlined frame.
            AsmInst::MethodRetSpecialized { rbp_offset }
            | AsmInst::BlockBreakSpecialized { rbp_offset } => {
                let off = rbp_offset.unwrap_concrete();
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, _, _| {
                    cg.method_return_specialized(off);
                });
            }
            // Outer-scope local access at a pre-resolved frame offset.
            AsmInst::LoadDynVarSpecialized { offset, reg } => {
                let off = offset.unwrap_concrete();
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, _, _| {
                    cg.load_dyn_var_specialized(off, reg);
                });
            }
            AsmInst::StoreDynVarSpecialized { offset, dst, src } => {
                let off = offset.unwrap_concrete();
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, _, _| {
                    cg.store_dyn_var_specialized(off, dst, src);
                });
            }
            // Direct call into an inlined method entry; the return-address patch
            // point is recorded for BOP-redefinition eviction. Labels are
            // resolved now (frame); the call + patch run at drain time, where
            // `do_specialized_call`'s return address is the correct position.
            AsmInst::SpecializedCall {
                entry,
                patch_point,
                evict,
            } => {
                let patch_point =
                    patch_point.map(|label| frame.resolve_label(&mut self.jit, label));
                let entry_label = frame.resolve_label(&mut self.jit, entry);
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, labels, _| {
                    let return_addr = cg.do_specialized_call(entry_label, patch_point);
                    cg.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
                });
            }
            // Specialized `yield`: build the block frame, then branch into the
            // inlined block entry (no patch point).
            AsmInst::SetupYieldFrame { meta, outer } => {
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, _, _| {
                    cg.setup_yield_frame(meta, outer);
                });
            }
            AsmInst::SpecializedYield { entry, evict } => {
                let entry_label = frame.resolve_label(&mut self.jit, entry);
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, labels, _| {
                    let return_addr = cg.do_specialized_call(entry_label, None);
                    cg.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
                });
            }
            // Inlined builtin method body: lower to `LInst::Inline`, the
            // context-carrying escape hatch. Unlike every other `LInst` (which
            // is store-free and goes through `encode_linst`), its emit needs
            // `store`/`labels`/`base`, so it is dispatched here via
            // `encode_linst_inline` — the one LIR op whose machine-code emit
            // lives at the lowering boundary rather than in `encode_linst`.
            AsmInst::Inline(proc) => {
                self.encode_linst_inline(LInst::Inline(proc), store, labels, frame.base_stack_offset)
            }
            // §20 (B): typed array integer-index read/assign (replaces the
            // `ir.inline` closures, so `AsmInst` is `Clone`). The per-arch
            // index-register setup + `array_index*` call lives in
            // `gen_array_index*`.
            AsmInst::ArrayIndex { kind } => {
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, _, _| {
                    cg.gen_array_index(kind);
                });
            }
            AsmInst::ArrayIndexAssign {
                kind,
                using_fpr,
                error,
            } => {
                self.lower_via_inline(store, labels, frame.base_stack_offset, move |cg, _, labels, _| {
                    cg.gen_array_index_assign(kind, using_fpr, &labels[error]);
                });
            }
            // Typed field-load (replaces the `ir.inline` escape hatch for trivial
            // field-reader inline builtins). Goal-2 proof: an inline builtin's
            // codegen expressed once in arch-neutral LIR via an existing op.
            AsmInst::LoadFieldToReg { dst, base, disp } => self.encode_linst(LInst::Load {
                dst: dst.into(),
                mem: LMem::Field {
                    base: base.into(),
                    disp,
                },
            }),
            // Typed bool-field load (replaces the `emit_*_exclude_end` closures).
            AsmInst::BoolFieldToReg { dst, base, disp } => {
                self.encode_linst(LInst::BoolFieldToReg { dst, base, disp })
            }
            // Typed container-length-as-fixnum (replaces `emit_array_size` /
            // `emit_string_bytesize`).
            AsmInst::ArrayLenFixnum { dst, base } => {
                self.encode_linst(LInst::ArrayLenFixnum { dst, base })
            }
            AsmInst::StringLenFixnum { dst, base } => {
                self.encode_linst(LInst::StringLenFixnum { dst, base })
            }
            // Typed bool predicates (replace `emit_kernel_nil` / `emit_object_not`).
            AsmInst::IsNilToBool { dst, src } => {
                self.encode_linst(LInst::IsNilToBool { dst, src })
            }
            AsmInst::NotToBool { dst, src } => {
                self.encode_linst(LInst::NotToBool { dst, src })
            }
            // `Math.sqrt` (replaces the `emit_math_sqrt` closure). Resolve the
            // deopt label and pass the frame base, like the guard family.
            AsmInst::MathSqrt { fsrc, fret, deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::MathSqrt {
                    fsrc,
                    fret,
                    deopt,
                    base: frame.base_stack_offset,
                });
            }
            // `Integer#succ` (replaces `emit_integer_succ`): resolve the deopt.
            AsmInst::IntegerSucc { reg, deopt } => {
                let deopt = labels[deopt].clone();
                self.encode_linst(LInst::IntegerSucc { reg, deopt });
            }
            // `Kernel#block_given?` (replaces `emit_block_given`).
            AsmInst::BlockGiven { dst } => self.encode_linst(LInst::BlockGiven { dst }),
            // ---- Class/module definition + misc runtime-call ops --------------
            // `class`/`module` (re)definition + body, and `class << obj`. aarch64
            // bails on a live fpr pool reg / out-of-range frame offset.
            AsmInst::ClassDef {
                base,
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_fpr,
                error,
            } => {
                self.encode_linst(LInst::ClassDef {
                    base,
                    superclass,
                    dst,
                    name,
                    func_id,
                    is_module,
                    using_fpr,
                    error: labels[error].clone(),
                });
            }
            AsmInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_fpr,
                error,
            } => {
                self.encode_linst(LInst::SingletonClassDef {
                    base,
                    dst,
                    func_id,
                    using_fpr,
                    error: labels[error].clone(),
                });
            }
            // Forwarding-trampoline helper frame setup (aarch64 bails on an
            // out-of-range frame offset).
            AsmInst::SetArgumentsForwardedHelper { callid, callee_fid } => {
                let offset = store[callee_fid].get_offset();
                self.encode_linst(LInst::SetArgumentsForwardedHelper {
                    callid,
                    callee_fid,
                    offset,
                });
            }
            // Trap for statically-unreachable code: call the panicking helper.
            AsmInst::Unreachable => self.encode_linst(LInst::Unreachable),
            // `**kwrest` fixup: build a (name, slot) const table and call
            // `correct_rest_kw(&table, lfp) -> kwrest Hash`.
            AsmInst::RestKw { rest_kw } => self.encode_linst(LInst::RestKw { rest_kw }),
            // Not a shared instruction: hand off to the per-arch backend.
            // (§9a-ii) Not-yet-LIR-ized arms still emit directly in the per-arch
            // `compile_asmir_arch`. During the buffering pass, defer them as
            // `LInst::DeferredArch` so they run at drain (correct position)
            // rather than emitting now (which would scramble order vs. the
            // buffered ops). Otherwise (immediate path) dispatch directly.
            other => {
                if let Some(buf) = self.lir_buf.as_mut() {
                    buf.push(LInst::DeferredArch {
                        inst: other,
                        class_version,
                    });
                    return true;
                }
                return self.compile_asmir_arch(store, frame, labels, other, class_version);
            }
        }
        true
    }

    ///
    /// Emit the one context-carrying `LInst`: `LInst::Inline`. The wrapped
    /// generator closure emits arch-appropriate asm directly and, unlike the
    /// store-free `encode_linst`, needs the compile context (`store`, the
    /// side-exit `labels`, and the frame `base`). It is therefore dispatched
    /// from the AsmIR→LIR lowering boundary (`compile_asmir`) rather than from
    /// `encode_linst`. Accessing `InlineProcedure::proc` is sound here because
    /// `compile_shared` is a child module of `asmir`, where the field is
    /// declared.
    ///
    pub(in crate::codegen::jitgen) fn encode_linst_inline(
        &mut self,
        inst: LInst,
        store: &Store,
        labels: &SideExitLabels,
        base: usize,
    ) {
        // (§9a-ii) Buffering pass: collect, don't run the generator yet.
        if let Some(buf) = self.lir_buf.as_mut() {
            buf.push(inst);
            return;
        }
        match inst {
            LInst::Inline(proc) => (proc.proc)(self, store, labels, base),
            _ => unreachable!("encode_linst_inline only handles LInst::Inline"),
        }
    }

    /// (§9 9d) GP physical-allocation pass over a buffered region body. This is
    /// the seam between buffering (`gen_asm` collects the body into one ordered
    /// `Vec<LInst>`) and the drain (emission). Today it is the **identity**: the
    /// body drains exactly as buffered, so the output is byte-identical. §9d
    /// fills it in — walking the stream, assigning each allocatable virtual GP
    /// (`VReg::Alloc`, a bytecode slot) to a pool register (`GP_ALLOC_POOL`) or
    /// leaving it in its stack home, inserting loads/stores and the call /
    /// safepoint flush-and-demote (`G -> S`, no reload — a GP pool value's spill
    /// home *is* its slot). The ① fixpoint's loop-head liveness / loop-carried
    /// metadata is threaded in here when that step lands (the §9c wiring).
    pub(in crate::codegen::jitgen) fn allocate_gp(&self, body: Vec<LInst>) -> Vec<LInst> {
        body
    }

    /// (§9 9a) Drain a frame-needing ordering pseudo-op. Unlike `encode_linst`
    /// (frameless) and `encode_linst_inline` (`store`/`labels`/`base`), this
    /// reproduces the position-metadata side effects that read/write the frame.
    /// It is the frame-aware seam the whole-region buffering will route through.
    pub(in crate::codegen::jitgen) fn encode_linst_frame(
        &mut self,
        inst: LInst,
        frame: &mut AsmInfo,
    ) {
        // (§9a-ii) Buffering pass: collect, don't record the position yet (the
        // position is only meaningful at drain time, in emission order).
        if let Some(buf) = self.lir_buf.as_mut() {
            buf.push(inst);
            return;
        }
        match inst {
            LInst::SourcePos { idx } => {
                let pos = self.jit.get_current() - frame.start_codepos;
                frame.sourcemap.push((idx, pos));
            }
            _ => unreachable!("encode_linst_frame only handles frame-needing pseudo-ops"),
        }
    }

    /// (§9a) Lower a not-yet-typed family op (specialized inlined-frame ops,
    /// `gen_array_index*`) through the `LInst::Inline` escape hatch, so every
    /// `AsmInst` produces an `LInst`. Resolve any frame `JitLabel`s to
    /// `DestLabel`s *before* calling — the closure runs at drain time with only
    /// `(store, labels, base)`, exactly like the inline-builtin generators.
    fn lower_via_inline(
        &mut self,
        store: &Store,
        labels: &SideExitLabels,
        base: usize,
        f: impl FnOnce(&mut Codegen, &Store, &SideExitLabels, usize) + 'static,
    ) {
        self.encode_linst_inline(LInst::Inline(InlineProcedure::new(f)), store, labels, base);
    }

    ///
    /// Arch-neutral fallback of `encode_linst` for the macro-op `LInst`s — the
    /// irreducible per-arch sequences that delegate to an existing `emit_*`
    /// helper. Routing them here (rather than duplicating the delegation in each
    /// backend's `encode_linst`) keeps the per-arch encoder for the decomposed
    /// ops while still funnelling *all* emission through `encode_linst`.
    ///
    pub(in crate::codegen::jitgen) fn encode_linst_macro(&mut self, inst: LInst) {
        match inst {
            // (§9 9a) Ordering pseudo-ops: reproduce the inline jit calls at
            // drain time. Arch-neutral (pure `self.jit` ops), so they live here
            // rather than in each backend's `encode_linst`.
            LInst::SelectPage(page) => self.jit.select_page(page),
            LInst::BindLabel(label) => self.jit.bind_label(label),
            LInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_,
            } => {
                self.emit_load_ivar_heap(ivarid, is_object_ty, self_);
            }
            LInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                using_fpr,
            } => {
                self.emit_store_ivar_heap(src, ivarid, is_object_ty, using_fpr);
            }
            LInst::StoreConstant { id, using_fpr, error } => {
                self.emit_store_constant(id, using_fpr, &error);
            }
            LInst::LoadGVar { name, using_fpr } => {
                self.emit_load_gvar(name, using_fpr);
            }
            LInst::StoreGVar { name, src, using_fpr } => {
                self.emit_store_gvar(name, src, using_fpr);
            }
            LInst::LoadCVar { name, using_fpr } => {
                self.emit_load_cvar(name, using_fpr);
            }
            LInst::LoadDynVar { src } => {
                self.emit_load_dyn_var(src);
            }
            LInst::StoreDynVar { dst, src } => {
                self.emit_store_dyn_var(dst, src);
            }
            LInst::CreateArray { src, len } => {
                self.emit_create_array(src, len);
            }
            LInst::NewArray { callid, using_fpr } => {
                self.emit_new_array(callid, using_fpr);
            }
            LInst::NewHash { args, len, using_fpr } => {
                self.emit_new_hash(args, len, using_fpr);
            }
            LInst::HashInsert { hash, args, len, using_fpr } => {
                self.emit_hash_insert(hash, args, len, using_fpr);
            }
            LInst::ArrayConcat { dst, src, using_fpr } => {
                self.emit_array_concat(dst, src, using_fpr);
            }
            LInst::NewRange { start, end, exclude_end, using_fpr } => {
                self.emit_new_range(start, end, exclude_end, using_fpr);
            }
            LInst::ConcatStr { arg, len, using_fpr } => {
                self.emit_concat_str(arg, len, using_fpr);
            }
            LInst::ToA { src, using_fpr } => {
                self.emit_to_a(src, using_fpr);
            }
            LInst::DeepCopyLit { v, using_fpr } => {
                self.emit_deep_copy_lit(v, using_fpr);
            }
            LInst::UndefMethod { undef, using_fpr } => {
                self.emit_undef_method(undef, using_fpr);
            }
            LInst::AliasGvar { new, old, using_fpr } => {
                self.emit_alias_gvar(new, old, using_fpr);
            }
            LInst::CheckCVar { name, using_fpr } => {
                self.emit_check_cvar(name, using_fpr);
            }
            LInst::StoreCVar { name, src, using_fpr } => {
                self.emit_store_cvar(name, src, using_fpr);
            }
            LInst::AliasMethod { new, old, using_fpr } => {
                self.emit_alias_method(new, old, using_fpr);
            }
            LInst::DefinedYield { dst, using_fpr } => {
                self.emit_defined_yield(dst, using_fpr);
            }
            LInst::DefinedSuper { dst, using_fpr } => {
                self.emit_defined_super(dst, using_fpr);
            }
            LInst::DefinedGvar { dst, name, using_fpr } => {
                self.emit_defined_gvar(dst, name, using_fpr);
            }
            LInst::DefinedCvar { dst, name, using_fpr } => {
                self.emit_defined_cvar(dst, name, using_fpr);
            }
            LInst::DefinedConst { dst, siteid, using_fpr } => {
                self.emit_defined_const(dst, siteid, using_fpr);
            }
            LInst::DefinedMethod { dst, recv, name, using_fpr } => {
                self.emit_defined_method(dst, recv, name, using_fpr);
            }
            LInst::DefinedIvar { dst, name, using_fpr } => {
                self.emit_defined_ivar(dst, name, using_fpr);
            }
            LInst::GenericBinOp { lhs, rhs, func, using_fpr } => {
                self.emit_generic_binop(lhs, rhs, func, using_fpr);
            }
            LInst::OptEqCmp { lhs, rhs, kind, func, using_fpr } => {
                self.emit_opt_eq_cmp(lhs, rhs, kind, func, using_fpr);
            }
            LInst::ArrayTEq { lhs, rhs, using_fpr } => {
                self.emit_array_teq(lhs, rhs, using_fpr);
            }
            LInst::ConcatRegexp { arg, len, using_fpr } => {
                self.emit_concat_regexp(arg, len, using_fpr);
            }
            LInst::CheckKwRest { slot } => {
                self.emit_check_kw_rest(slot);
            }
            LInst::ExpandArray { dst, len, rest_pos, using_fpr } => {
                self.emit_expand_array(dst, len, rest_pos, using_fpr);
            }
            LInst::Deopt { deopt } => {
                self.emit_deopt(&deopt);
            }
            LInst::HandleError { error } => {
                self.emit_handle_error(&error);
            }
            LInst::CheckStack { write_back, error, base } => {
                self.emit_check_stack(write_back, &error, base);
            }
            LInst::ExecGc { write_back, error, base } => {
                self.emit_exec_gc(write_back, &error, base);
            }
            LInst::IntegerCmp { kind, mode, lhs, rhs } => {
                self.emit_integer_cmp(kind, mode, lhs, rhs);
            }
            LInst::Ret => {
                self.emit_ret();
            }
            LInst::MethodRet { pc } => {
                self.emit_method_ret(pc);
            }
            LInst::BlockBreak { pc } => {
                self.emit_block_break(pc);
            }
            LInst::ImmediateEvict { evict } => {
                self.emit_immediate_evict(evict);
            }
            LInst::Init { info, prologue_offset } => {
                self.emit_init(info, prologue_offset);
            }
            LInst::LoopJitRspBump { offset } => {
                self.emit_loop_jit_rsp_bump(offset);
            }
            LInst::BlockArgProxy { ret, outer } => {
                self.emit_block_arg_proxy(ret, outer);
            }
            LInst::BlockArg { ret, using_fpr, call_site_bc_ptr, error } => {
                self.emit_block_arg(ret, using_fpr, call_site_bc_ptr, &error);
            }
            LInst::MethodDef { name, func_id, using_fpr, error } => {
                self.emit_method_def(name, func_id, using_fpr, &error);
            }
            LInst::SingletonMethodDef { obj, name, func_id, using_fpr, error } => {
                self.emit_singleton_method_def(obj, name, func_id, using_fpr, &error);
            }
            LInst::Raise { loop_jit_spill_bytes } => {
                self.emit_raise(loop_jit_spill_bytes);
            }
            LInst::Retry { pc, loop_jit_spill_bytes } => {
                self.emit_retry(pc, loop_jit_spill_bytes);
            }
            LInst::Redo { pc, loop_jit_spill_bytes } => {
                self.emit_redo(pc, loop_jit_spill_bytes);
            }
            LInst::EnsureEnd { loop_jit_spill_bytes } => {
                self.emit_ensure_end(loop_jit_spill_bytes);
            }
            LInst::Yield { callid, error, evict, evict_label } => {
                self.emit_yield(callid, &error, evict, &evict_label);
            }
            LInst::Unreachable => {
                self.emit_unreachable();
            }
            LInst::RestKw { rest_kw } => {
                self.emit_rest_kw(rest_kw);
            }
            LInst::GuardClassVersion {
                class_version,
                position,
                with_recovery,
                deopt,
            } => {
                self.emit_guard_class_version(class_version, position, with_recovery, deopt);
            }
            LInst::RecompileDeopt {
                position,
                deopt,
                error,
                reason,
            } => {
                self.emit_recompile_deopt(position, &deopt, error.as_ref(), reason);
            }
            LInst::ClassDef {
                base,
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_fpr,
                error,
            } => {
                self.class_def(
                    base,
                    superclass,
                    dst,
                    name,
                    func_id,
                    is_module,
                    using_fpr,
                    &error,
                );
            }
            LInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_fpr,
                error,
            } => {
                self.singleton_class_def(base, dst, func_id, using_fpr, &error);
            }
            LInst::SetupMethodFrame {
                meta,
                outer_lfp,
                block_fid,
                block_arg,
            } => {
                self.emit_setup_method_frame(meta, outer_lfp, block_fid, block_arg);
            }
            LInst::SetArguments {
                callid,
                callee_fid,
                offset,
            } => {
                self.emit_set_arguments(callid, callee_fid, offset);
            }
            LInst::SetArgumentsForwardedHelper {
                callid,
                callee_fid,
                offset,
            } => {
                self.jit_set_arguments_forwarded_helper(callid, callee_fid, offset);
            }
            LInst::Preparation { heap_len } => {
                self.emit_preparation(heap_len);
            }
            LInst::OptCase {
                max,
                min,
                else_dest,
                branch_dests,
            } => {
                self.emit_opt_case(max, min, else_dest, branch_dests);
            }
            LInst::Call {
                codeptr,
                is_iseq,
                callee_pc,
                call_site_bc_ptr,
                jit_entry,
                evict,
                evict_label,
            } => {
                self.emit_call(
                    codeptr,
                    is_iseq,
                    callee_pc,
                    call_site_bc_ptr,
                    jit_entry,
                    evict,
                    &evict_label,
                );
            }
            other => unreachable!("encode_linst_macro: unexpected {other:?}"),
        }
    }
}

// Arch-neutral runtime helpers called (as function pointers) from both
// backends' emission primitives. The asm that loads and calls them differs per
// arch, but the Rust bodies are identical, so they live here once rather than
// being duplicated in `arch/x86_64/compile/*.rs` (x86) and `arch/aarch64/compile.rs` (aarch64).

/// `self.@ivar = val` cold path (StoreIVarHeap): set via IvarId, growing the
/// var-table as needed.
pub(in crate::codegen::jitgen) extern "C" fn set_ivar(base: &mut RValue, id: IvarId, val: Value) {
    base.set_ivar_by_ivarid(id, val)
}

/// Grow `rvalue`'s heap ivar var-table to at least `heap_len` slots.
pub(in crate::codegen::jitgen) extern "C" fn extend_ivar(rvalue: &mut RValue, heap_len: usize) {
    rvalue.extend_ivar(heap_len);
}

/// Trap target for the `Unreachable` AsmInst (statically-unreachable code).
pub(in crate::codegen::jitgen) extern "C" fn unreachable() {
    unreachable!("reached unreachable code");
}

/// Generic `Array#[]=` fallback (out-of-fast-path index). Returns `None` and
/// sets the error on failure (negative index past the start).
pub(in crate::codegen::jitgen) extern "C" fn set_array_integer_index(
    base: Value,
    index: i64,
    vm: &mut Executor,
    _globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    base.as_array()
        .set_index(index, src)
        .map_err(|err| vm.set_error(err))
        .ok()
}
