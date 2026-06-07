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
            // Dense-integer `case`: range-check the (fixnum) condition against
            // `[min, max]`, then dispatch through a jump table of absolute
            // branch-target addresses (else for out-of-range). Terminates the
            // basic block (no fallthrough).
            AsmInst::OptCase {
                max,
                min,
                else_label,
                branch_labels,
            } => self.emit_opt_case(frame, max, min, else_label, branch_labels),
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
            // Save / restore live FP pool registers around a C-call.
            AsmInst::XmmSave(using_xmm, cont) => return self.emit_xmm_save(using_xmm, cont),
            AsmInst::XmmRestore(using_xmm, cont) => return self.emit_xmm_restore(using_xmm, cont),
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
                return self.emit_integer_binop(lhs, rhs, mode, kind, deopt);
            }
            AsmInst::IntegerCmp {
                mode,
                kind,
                lhs,
                rhs,
            } => return self.emit_integer_cmp(kind, mode, lhs, rhs),
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let branch_dest = frame.resolve_label(&mut self.jit, branch_dest);
                return self.emit_integer_cmp_br(kind, mode, lhs, rhs, brkind, branch_dest);
            }
            AsmInst::FloatBinOp {
                kind,
                binary_xmm,
                dst,
            } => return self.emit_float_binop(kind, binary_xmm, dst, frame.base_stack_offset),
            AsmInst::FloatUnOp { kind, dst } => {
                return self.emit_float_unop(kind, dst, frame.base_stack_offset);
            }
            // [slot] <- Value::integer(i) and fpr(x) <- i as f64 (constant int
            // materialized as both a boxed integer and a double).
            AsmInst::I64ToBoth(i, slot, x) => {
                return self.emit_i64_to_both(i, slot, x, frame.base_stack_offset);
            }
            // Float comparison. NaN compares false (except `!=`); each backend
            // picks NaN-correct condition codes (x86 ucomisd + setp tricks,
            // aarch64 fcmp + MI/LS conditions).
            AsmInst::FloatCmp { kind, lhs, rhs } => {
                return self.emit_float_cmp(kind, lhs, rhs, frame.base_stack_offset);
            }
            AsmInst::FloatCmpBr {
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let branch_dest = frame.resolve_label(&mut self.jit, branch_dest);
                return self.emit_float_cmp_br(
                    kind,
                    lhs,
                    rhs,
                    brkind,
                    branch_dest,
                    frame.base_stack_offset,
                );
            }
            // Method return / eviction family. `Ret` tears down the frame and
            // returns; `MethodRet` sets the resume PC then returns through the
            // method-return path; `BlockBreak` does the same through the
            // block-break path (a non-local `break` out of a block);
            // `ImmediateEvict` records a return-address patch point on x86 (a
            // no-op on aarch64, which can't patch them).
            AsmInst::Ret => self.emit_ret(),
            AsmInst::MethodRet(pc) => self.emit_method_ret(pc),
            AsmInst::BlockBreak(pc) => self.emit_block_break(pc),
            AsmInst::ImmediateEvict { evict } => self.emit_immediate_evict(evict),
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
                self.emit_guard_class_version(class_version, position, with_recovery, deopt);
            }
            AsmInst::SetupMethodFrame {
                meta,
                callid,
                outer_lfp,
            } => self.emit_setup_method_frame(store, meta, callid, outer_lfp),
            AsmInst::SetArguments { callid, callee_fid } => {
                return self.emit_set_arguments(store, callid, callee_fid);
            }
            // Basic-operator-redefinition guard: deopt if any BOP was redefined.
            AsmInst::CheckBOP { deopt } => self.emit_check_bop(&labels[deopt]),
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
                self.emit_recompile_deopt(position, &labels[deopt], error.as_ref(), reason)
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
                self.emit_call(store, callee_fid, recv_class, evict, &evict_label, pc);
            }
            // Method prologue: establish fp/lr, reserve the local frame, nil-fill
            // non-argument locals (aarch64 bails if the frame exceeds the 12-bit
            // sub-sp immediate).
            AsmInst::Init {
                info,
                prologue_offset,
            } => return self.emit_init(info, prologue_offset),
            // Per-method ivar-cache prep; aarch64 bails on the heap-ivar path.
            AsmInst::Preparation => return self.emit_preparation(store, frame),
            // Fixnum unary ops on the tagged value. Negate deopts on i63
            // overflow (e.g. -i63::MIN); bitwise-not cannot overflow.
            AsmInst::FixnumNeg { reg, deopt } => self.emit_fixnum_neg(reg, &labels[deopt]),
            AsmInst::FixnumBitNot { reg } => self.emit_fixnum_bit_not(reg),
            // Type guards: deopt unless `reg` is an Array / the receiver in rdi
            // is unfrozen.
            AsmInst::GuardArrayTy(reg, deopt) => self.emit_guard_array_ty(reg, &labels[deopt]),
            AsmInst::GuardFrozen { deopt } => self.emit_guard_frozen(&labels[deopt]),
            // Inline instance-variable / struct-member access on the receiver in
            // rdi (no Box deref). aarch64 bails if the field offset exceeds the
            // 12-bit scaled load/store immediate.
            AsmInst::LoadIVarInline { ivarid } => return self.emit_load_ivar_inline(ivarid),
            AsmInst::StoreIVarInline { src, ivarid } => {
                return self.emit_store_ivar_inline(src, ivarid);
            }
            AsmInst::LoadStructSlotInline { slot_index } => {
                return self.emit_load_struct_slot_inline(slot_index);
            }
            AsmInst::StoreStructSlotInline { src, slot_index } => {
                return self.emit_store_struct_slot_inline(src, slot_index);
            }
            // Heap (Box-spilled) Struct member access: deref the heap pointer
            // first, then load/store the slot (aarch64 bails on a large offset).
            AsmInst::LoadStructSlotHeap { slot_index } => {
                return self.emit_load_struct_slot_heap(slot_index);
            }
            AsmInst::StoreStructSlotHeap { src, slot_index } => {
                return self.emit_store_struct_slot_heap(src, slot_index);
            }
            // reg += i / reg -= i (no-op when i == 0).
            AsmInst::RegAdd(reg, i) => self.emit_reg_add(reg, i),
            AsmInst::RegSub(reg, i) => self.emit_reg_sub(reg, i),
            // Loop-JIT entry stack bump (aarch64 bails on a frame larger than
            // the 12-bit sub-sp immediate).
            AsmInst::LoopJitRspBump { offset } => return self.emit_loop_jit_rsp_bump(offset),
            // Inline argument-setup stores into the callee frame (aarch64 bails
            // on an out-of-range slot offset).
            AsmInst::RegToRSPOffset(r, ofs) => return self.emit_reg_to_rsp_offset(r, ofs),
            AsmInst::ZeroToRSPOffset(ofs) => return self.emit_zero_to_rsp_offset(ofs),
            AsmInst::U64ToRSPOffset(i, ofs) => return self.emit_u64_to_rsp_offset(i, ofs),
            // Side-effect guard for block-passing calls: deopt if the frame was
            // captured/promoted.
            AsmInst::GuardCapture(deopt) => return self.emit_guard_capture(&labels[deopt]),
            // `&block` forwarding: proxy the block handler, or materialize it
            // into a Proc value (aarch64 bails on a live xmm / range overflow).
            AsmInst::BlockArgProxy { ret, outer } => return self.emit_block_arg_proxy(ret, outer),
            AsmInst::BlockArg { ret, _outer: _, using_xmm, error, call_site_bc_ptr } => {
                return self.emit_block_arg(ret, using_xmm, call_site_bc_ptr, &labels[error]);
            }
            // Store into a heap-spilled instance variable of self (the table is
            // known large enough, so no bounds check / runtime extend).
            AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
            } => return self.emit_store_self_ivar_heap(src, ivarid, is_object_ty),
            // Store into a heap-spilled ivar of another object (bounds-checked;
            // grows the table via a runtime call on miss). aarch64 bails on an
            // out-of-range field offset.
            AsmInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                using_xmm,
            } => return self.emit_store_ivar_heap(src, ivarid, is_object_ty, using_xmm),
            // Load a heap-spilled instance variable (bounds-checked unless self),
            // substituting nil for an out-of-range / unset slot.
            AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_,
            } => return self.emit_load_ivar_heap(ivarid, is_object_ty, self_),
            // Runtime-call definition ops (undef a method / alias a global var).
            // aarch64 bails when an xmm pool register is live (no xmm save yet).
            AsmInst::UndefMethod { undef, using_xmm } => {
                return self.emit_undef_method(undef, using_xmm);
            }
            AsmInst::AliasGvar { new, old, using_xmm } => {
                return self.emit_alias_gvar(new, old, using_xmm);
            }
            // Runtime-call class-variable / method-alias ops. aarch64 bails
            // when an xmm pool register is live (no xmm save yet).
            AsmInst::CheckCVar { name, using_xmm } => {
                return self.emit_check_cvar(name, using_xmm);
            }
            AsmInst::StoreCVar { name, src, using_xmm } => {
                return self.emit_store_cvar(name, src, using_xmm);
            }
            AsmInst::AliasMethod { new, old, using_xmm } => {
                return self.emit_alias_method(new, old, using_xmm);
            }
            // defined? runtime-call family (aarch64 bails on a live xmm pool reg
            // or an out-of-range frame offset).
            AsmInst::DefinedYield { dst, using_xmm } => {
                return self.emit_defined_yield(dst, using_xmm);
            }
            AsmInst::DefinedSuper { dst, using_xmm } => {
                return self.emit_defined_super(dst, using_xmm);
            }
            AsmInst::DefinedGvar { dst, name, using_xmm } => {
                return self.emit_defined_gvar(dst, name, using_xmm);
            }
            AsmInst::DefinedCvar { dst, name, using_xmm } => {
                return self.emit_defined_cvar(dst, name, using_xmm);
            }
            AsmInst::DefinedConst { dst, siteid, using_xmm } => {
                return self.emit_defined_const(dst, siteid, using_xmm);
            }
            AsmInst::DefinedMethod { dst, recv, name, using_xmm } => {
                return self.emit_defined_method(dst, recv, name, using_xmm);
            }
            AsmInst::DefinedIvar { dst, name, using_xmm } => {
                return self.emit_defined_ivar(dst, name, using_xmm);
            }
            // Generic binary-op / Array=== runtime calls (aarch64 bails on a
            // live xmm pool reg or an out-of-range frame offset).
            AsmInst::GenericBinOp { lhs, rhs, func, using_xmm } => {
                return self.emit_generic_binop(lhs, rhs, func, using_xmm);
            }
            AsmInst::OptEqCmp { lhs, rhs, kind, func, using_xmm } => {
                return self.emit_opt_eq_cmp(lhs, rhs, kind, func, using_xmm);
            }
            AsmInst::ArrayTEq { lhs, rhs, using_xmm } => {
                return self.emit_array_teq(lhs, rhs, using_xmm);
            }
            // Regexp interpolation / keyword-rest fixup runtime calls.
            AsmInst::ConcatRegexp { arg, len, using_xmm } => {
                return self.emit_concat_regexp(arg, len, using_xmm);
            }
            AsmInst::CheckKwRest(slot) => return self.emit_check_kw_rest(slot),
            // Multiple-assignment array expansion (aarch64 bails on a live xmm
            // pool reg or an out-of-range frame offset).
            AsmInst::ExpandArray { dst, len, rest_pos, using_xmm } => {
                return self.emit_expand_array(dst, len, rest_pos, using_xmm);
            }
            // Float C-function calls (Math.sqrt/sin/…). aarch64 saves the live
            // FP pool (d2-d7) around the call and bails on a spilled operand.
            AsmInst::CFunc_F_F { f, src, dst, using_xmm } => {
                return self.emit_cfunc_f_f(f, src, dst, using_xmm, frame.base_stack_offset);
            }
            AsmInst::CFunc_FF_F { f, lhs, rhs, dst, using_xmm } => {
                return self.emit_cfunc_ff_f(f, lhs, rhs, dst, using_xmm, frame.base_stack_offset);
            }
            // Method definition (`def`). aarch64 bails on a live xmm pool reg.
            AsmInst::MethodDef { name, func_id, using_xmm, error } => {
                return self.emit_method_def(name, func_id, using_xmm, &labels[error]);
            }
            AsmInst::SingletonMethodDef { obj, name, func_id, using_xmm, error } => {
                return self.emit_singleton_method_def(obj, name, func_id, using_xmm, &labels[error]);
            }
            // Exception / non-local control flow (raise / retry / redo / ensure).
            // All branch into the shared entry_raise unwind path.
            AsmInst::Raise => return self.emit_raise(frame.loop_jit_spill_bytes),
            AsmInst::Retry(pc) => return self.emit_retry(pc, frame.loop_jit_spill_bytes),
            AsmInst::Redo(pc) => return self.emit_redo(pc, frame.loop_jit_spill_bytes),
            AsmInst::EnsureEnd => return self.emit_ensure_end(frame.loop_jit_spill_bytes),
            // Generic `yield` (block target resolved at runtime). aarch64 builds
            // the block frame and calls the funcdata indirectly; the x86-only
            // return-address eviction patch is applied by the x86 emit_yield.
            AsmInst::Yield { callid, error, evict } => {
                let evict_label = labels[evict].clone();
                return self.emit_yield(callid, &labels[error], evict, &evict_label);
            }
            // Not a shared instruction: hand off to the per-arch backend.
            other => return self.compile_asmir_arch(store, frame, labels, other, class_version),
        }
        true
    }
}
