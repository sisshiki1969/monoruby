use super::*;

mod binary_op;
mod constants;
mod defined;
mod definition;
mod index;
mod init_method;
mod method_call;
mod variables;

///
/// Location where a `VirtFPReg`'s value lives at code-generation
/// time. `Phys(N)` means physical `xmm{N}` directly usable in
/// `monoasm! { ... xmm(N) ... }`; `Spill(off)` means the 8-byte slot
/// at `[rbp - off]`. Spill-aware codegen lowerings build per-operand
/// asm based on this — e.g. `addsd xmm, mem` when the rhs operand is
/// spilled, instead of forcing a scratch load.
///
#[derive(Debug, Clone, Copy)]
pub(super) enum XmmLoc {
    Phys(u64),
    Spill(i32),
}

pub(super) fn xmm_loc(virt: VirtFPReg, base_stack_offset: usize) -> XmmLoc {
    let pool = super::state::PHYS_XMM_POOL;
    if virt.0 < pool {
        XmmLoc::Phys(virt.0 as u64 + 2)
    } else {
        let n = virt.0 - pool;
        let off = (base_stack_offset as i32) - 24 + 8 * (n as i32);
        XmmLoc::Spill(off)
    }
}

extern "C" fn extend_ivar(rvalue: &mut RValue, heap_len: usize) {
    rvalue.extend_ivar(heap_len);
}

extern "C" fn unreachable() {
    unreachable!("reached unreachable code");
}

impl Codegen {
    ///
    /// Generate machine code for *inst*.
    ///
    pub(super) fn compile_asmir(
        &mut self,
        store: &Store,
        frame: &mut AsmInfo,
        labels: &SideExitLabels,
        inst: AsmInst,
        class_version: DestLabel,
    ) {
        match inst {
            AsmInst::BcIndex(i) => {
                frame
                    .sourcemap
                    .push((i, self.jit.get_current() - frame.start_codepos));
            }
            AsmInst::Init {
                info,
                prologue_offset,
            } => {
                self.init_func(&info, prologue_offset.unwrap_concrete());
            }
            AsmInst::LoopJitRspBump { offset } => {
                let bytes = offset.unwrap_concrete();
                if bytes > 0 {
                    monoasm! { &mut self.jit,
                        subq rsp, (bytes as i32);
                    }
                }
            }
            AsmInst::LoadSpill {
                scratch,
                rbp_offset,
            } => {
                monoasm!( &mut self.jit,
                    movq xmm(scratch.enc()), [rbp - (rbp_offset)];
                );
            }
            AsmInst::StoreSpill {
                scratch,
                rbp_offset,
            } => {
                monoasm!( &mut self.jit,
                    movq [rbp - (rbp_offset)], xmm(scratch.enc());
                );
            }
            AsmInst::Unreachable => {
                monoasm!( &mut self.jit,
                    movq rax, (unreachable);
                    call rax;
                );
            }
            AsmInst::Preparation => {
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
            }
            AsmInst::Label(label) => {
                let label = frame.resolve_label(&mut self.jit, label);
                self.jit.bind_label(label);
            }
            AsmInst::AccToStack(slot) => {
                self.store_r15(slot);
            }
            AsmInst::RegToAcc(r) => {
                if r != GP::R15 {
                    let r = r as u64;
                    monoasm!( &mut self.jit,
                        movq r15, R(r);
                    );
                }
            }
            AsmInst::RegToStack(r, slot) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [rbp - (rbp_local(slot))], R(r);
                );
            }
            AsmInst::StackToReg(slot, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), [rbp - (rbp_local(slot))];
                );
            }
            AsmInst::LitToReg(v, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (v.id());
                );
            }
            AsmInst::RegMove(src, dst) => {
                let src = src as u64;
                let dst = dst as u64;
                monoasm!( &mut self.jit,
                    movq R(dst), R(src);
                );
            }
            AsmInst::RegAdd(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        addq R(r), (i);
                    }
                }
            }
            AsmInst::RegSub(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        subq R(r), (i);
                    }
                }
            }
            AsmInst::RegToRSPOffset(r, ofs) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs - RSP_LOCAL_FRAME)], R(r);
                );
            }
            AsmInst::ZeroToRSPOffset(ofs) => {
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs - RSP_LOCAL_FRAME)], 0;
                );
            }
            AsmInst::U64ToRSPOffset(i, ofs) => {
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs - RSP_LOCAL_FRAME)], (i);
                );
            }

            AsmInst::XmmMove(src, dst) => {
                self.emit_xmm_move(src, dst, frame.base_stack_offset);
            }
            AsmInst::XmmSwap(l, r) => {
                self.emit_xmm_swap(l, r, frame.base_stack_offset);
            }
            AsmInst::XmmBinOp {
                kind,
                binary_xmm,
                dst,
            } => self.float_binop(kind, dst, binary_xmm, frame.base_stack_offset),
            AsmInst::XmmUnOp { kind, dst } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    monoasm!( &mut self.jit,
                        xorps xmm(dst.enc()), [rip + imm];
                    );
                }
                UnOpK::Pos => {}
                _ => unreachable!(),
            },

            AsmInst::F64ToXmm(f, x) => {
                let f_const = self.jit.const_f64(f);
                match xmm_loc(x, frame.base_stack_offset) {
                    XmmLoc::Phys(p) => monoasm!( &mut self.jit,
                        movq xmm(p), [rip + f_const];
                    ),
                    XmmLoc::Spill(off) => monoasm!( &mut self.jit,
                        movq xmm0, [rip + f_const];
                        movq [rbp - (off)], xmm0;
                    ),
                }
            }
            AsmInst::FixnumToXmm(r, x) => {
                let (work, spill_off) = match xmm_loc(x, frame.base_stack_offset) {
                    XmmLoc::Phys(p) => (p, None),
                    XmmLoc::Spill(off) => (0u64, Some(off)),
                };
                self.integer_val_to_f64(r, work);
                if let Some(off) = spill_off {
                    monoasm!( &mut self.jit,
                        movq [rbp - (off)], xmm(work);
                    );
                }
            }
            AsmInst::FloatToXmm(reg, x, deopt) => {
                let (work, spill_off) = match xmm_loc(x, frame.base_stack_offset) {
                    XmmLoc::Phys(p) => (p, None),
                    XmmLoc::Spill(off) => (0u64, Some(off)),
                };
                self.float_to_f64(reg, work, &labels[deopt]);
                if let Some(off) = spill_off {
                    monoasm!( &mut self.jit,
                        movq [rbp - (off)], xmm(work);
                    );
                }
            }
            AsmInst::I64ToBoth(i, r, x) => {
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq [rbp - (rbp_local(r))], (Value::integer(i).id());
                    movq xmm(x.enc()), [rip + f];
                }
            }
            AsmInst::XmmToStack(x, slots) => self.xmm_to_stack(x, &[slots]),
            AsmInst::LitToStack(v, slot) => self.literal_to_stack(slot, v),
            AsmInst::DeepCopyLit(v, using_xmm) => self.deepcopy_literal(v, using_xmm),

            AsmInst::GuardClass(r, class, deopt) => {
                let deopt = &labels[deopt];
                self.guard_class(r, class, deopt);
            }
            AsmInst::GuardArrayTy(r, deopt) => {
                let deopt = &labels[deopt];
                self.guard_array_ty(r, deopt)
            }
            AsmInst::GuardCapture(deopt) => {
                let deopt = &labels[deopt];
                self.guard_capture(deopt)
            }

            AsmInst::HandleError(error) => {
                let error = &labels[error];
                self.handle_error(&error);
            }
            AsmInst::GuardClassVersion {
                position,
                with_recovery,
                deopt,
            } => {
                let deopt = &labels[deopt];
                self.guard_class_version(class_version, position, with_recovery, deopt);
            }
            AsmInst::GuardClassVersionSpecialized { idx, deopt } => {
                let deopt = &labels[deopt];
                self.guard_class_version_specialized(
                    class_version,
                    self.specialized_base + idx,
                    deopt,
                );
            }
            AsmInst::Deopt(deopt) => {
                let deopt = &labels[deopt];
                monoasm!( &mut self.jit,
                    jmp deopt;
                );
            }
            AsmInst::RecompileDeopt {
                position,
                deopt,
                reason,
            } => {
                let deopt = &labels[deopt];
                self.recompile_and_deopt(position, deopt, reason)
            }
            AsmInst::RecompileDeoptSpecialized { idx, deopt, reason } => {
                let deopt = &labels[deopt];
                self.recompile_and_deopt_specialized(deopt, self.specialized_base + idx, reason)
            }
            AsmInst::CheckBOP { deopt } => {
                let deopt = &labels[deopt];
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
            AsmInst::XmmSave(using_xmm, cont) => self.xmm_save_with_cont(using_xmm, cont),
            AsmInst::XmmRestore(using_xmm, cont) => self.xmm_restore_with_cont(using_xmm, cont),
            AsmInst::ExecGc { write_back, error } => {
                let error = &labels[error];
                self.jit_execute_gc(&write_back, error)
            }
            AsmInst::CheckStack { write_back, error } => {
                let error = &labels[error];
                self.jit_check_stack(&write_back, error);
            }
            AsmInst::SetArguments { callid, callee_fid } => {
                let offset = store[callee_fid].get_offset();
                self.jit_set_arguments(callid, callee_fid, offset);
            }

            AsmInst::Ret => {
                self.epilogue();
            }
            AsmInst::MethodRet(pc) => {
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).as_ptr());
                };
                self.method_return();
            }
            AsmInst::BlockBreak(pc) => {
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).as_ptr());
                };
                self.block_break();
            }
            AsmInst::MethodRetSpecialized { rbp_offset } => {
                self.method_return_specialized(rbp_offset.unwrap_concrete());
            }
            AsmInst::BlockBreakSpecialized { rbp_offset } => {
                self.method_return_specialized(rbp_offset.unwrap_concrete());
            }
            AsmInst::Raise => {
                let raise = self.entry_raise();
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, rax;
                    movq rax, (runtime::raise_err);
                    call rax;
                    jmp  raise;
                };
            }
            AsmInst::Retry(pc) => {
                let raise = self.entry_raise();
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).as_ptr());
                    movq rdi, rbx;
                    movq rax, (runtime::err_retry);
                    call rax;
                    jmp  raise;
                };
            }
            AsmInst::Redo(pc) => {
                let raise = self.entry_raise();
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).as_ptr());
                    movq rdi, rbx;
                    movq rax, (runtime::err_redo);
                    call rax;
                    jmp  raise;
                };
            }
            AsmInst::EnsureEnd => {
                let raise = self.entry_raise();
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rax, (runtime::check_err);
                    call rax;
                    testq rax, rax;
                    jne  raise;
                };
            }
            AsmInst::CondBr(brkind, dest) => {
                let branch_dest = frame.resolve_label(&mut self.jit, dest);
                self.cond_br(branch_dest, brkind);
            }
            AsmInst::NilBr(dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    cmpq rax, (NIL_VALUE);
                    jeq  dest;
                );
            }
            AsmInst::CheckLocal(dest) => {
                let dest = frame.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    testq rax, rax;
                    jnz  dest;
                );
            }
            AsmInst::CheckKwRest(slot) => {
                let exit = self.jit.label();
                monoasm! { &mut self.jit,
                    cmpq [rbp - (rbp_local(slot))], (NIL_VALUE);
                    jne  exit;
                    movq rax, (runtime::empty_hash);
                    call rax;
                    movq [rbp - (rbp_local(slot))], rax;
                exit:
                };
            }
            AsmInst::OptCase {
                max,
                min,
                else_label,
                branch_labels,
            } => {
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

            AsmInst::ImmediateEvict { evict } => {
                let patch_point = self.jit.get_current_address();
                let return_addr = self.asm_return_addr_table.get(&evict).unwrap();
                self.return_addr_table
                    .entry(*return_addr)
                    .and_modify(|e| e.0 = Some(patch_point));
            }

            AsmInst::SetupMethodFrame {
                meta,
                callid,
                outer_lfp,
            } => {
                self.setup_method_frame(store, meta, callid, outer_lfp);
            }
            AsmInst::SetupYieldFrame { meta, outer } => {
                self.setup_yield_frame(meta, outer);
            }
            AsmInst::Call {
                callee_fid,
                recv_class,
                evict,
                pc: call_site_bc_ptr,
            } => {
                let return_addr = self.do_call(store, callee_fid, recv_class, call_site_bc_ptr);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::SpecializedCall {
                entry,
                patch_point,
                evict,
            } => {
                let patch_point =
                    patch_point.map(|label| frame.resolve_label(&mut self.jit, label));
                let entry_label = frame.resolve_label(&mut self.jit, entry);
                let return_addr = self.do_specialized_call(entry_label, patch_point);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::Yield {
                callid,
                error,
                evict,
            } => {
                let error = &labels[error];
                let return_addr = self.gen_yield(callid, error);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::SpecializedYield { entry, evict } => {
                let block_entry = frame.resolve_label(&mut self.jit, entry);
                let return_addr = self.do_specialized_call(block_entry, None);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }

            AsmInst::FixnumNeg { reg, deopt } => {
                let deopt = &labels[deopt];
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
            AsmInst::FixnumBitNot { reg } => {
                let r = reg as u64;
                monoasm! { &mut self.jit,
                    sarq  R(r), 1;
                    notq  R(r);
                    salq  R(r), 1;
                    orq   R(r), 1;
                }
            }

            AsmInst::IntegerBinOp {
                kind,
                lhs,
                rhs,
                mode,
                deopt,
            } => {
                let deopt = &labels[deopt];
                self.integer_binop(lhs, rhs, &mode, kind, deopt);
            }
            AsmInst::IntegerCmp {
                mode,
                kind,
                lhs,
                rhs,
            } => self.integer_cmp(kind, mode, lhs, rhs),
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let branch_dest = frame.resolve_label(&mut self.jit, branch_dest);
                self.cmp_integer(&mode, lhs, rhs);
                self.condbr_int(kind, branch_dest, brkind);
            }
            AsmInst::FloatCmp { kind, lhs, rhs } => {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                };
                self.cmp_float((lhs, rhs));
                self.setflag_float(kind);
            }
            AsmInst::FloatCmpBr {
                kind,
                lhs,
                rhs,
                brkind,
                branch_dest,
            } => {
                let branch_dest = frame.resolve_label(&mut self.jit, branch_dest);
                self.cmp_float((lhs, rhs));
                self.condbr_float(kind, branch_dest, brkind);
            }

            AsmInst::GuardConstBaseClass { base_class, deopt } => {
                let deopt = &labels[deopt];
                let cached_base_class = self.jit.const_i64(base_class.id() as _);
                monoasm! { &mut self.jit,
                    cmpq rax, [rip + cached_base_class];  // rax: base_class
                    jne  deopt;
                }
            }
            AsmInst::GuardConstVersion {
                const_version,
                deopt,
            } => {
                let deopt = &labels[deopt];
                self.guard_const_version(const_version, deopt);
            }
            AsmInst::StoreConstant {
                id,
                using_xmm,
                error,
            } => {
                self.store_constant(id, using_xmm);
                self.handle_error(&labels[error]);
            }

            AsmInst::ArrayTEq {
                lhs,
                rhs,
                using_xmm,
            } => {
                self.array_teq(lhs, rhs, using_xmm);
            }
            AsmInst::NewArray { callid, using_xmm } => {
                self.new_array(callid, using_xmm);
            }
            AsmInst::NewHash(args, len, using_xmm) => {
                self.new_hash(args, len, using_xmm);
            }
            AsmInst::NewRange {
                start,
                end,
                exclude_end,
                using_xmm,
            } => {
                self.load_rdi(start);
                self.load_rsi(end);
                self.new_range(exclude_end, using_xmm);
            }

            AsmInst::BlockArgProxy { ret, outer } => {
                self.get_method_lfp(outer);
                self.block_arg_proxy(outer);
                self.store_rax(ret);
            }
            AsmInst::BlockArg {
                ret,
                _outer: _,
                using_xmm,
                error,
                call_site_bc_ptr,
            } => {
                self.block_arg(using_xmm, call_site_bc_ptr);
                self.handle_error(&labels[error]);
                self.store_rax(ret);
            }

            AsmInst::LoadDynVar { src } => self.load_dyn_var(src),
            AsmInst::LoadDynVarSpecialized { offset, reg } => {
                self.load_dyn_var_specialized(offset.unwrap_concrete(), reg);
            }
            AsmInst::StoreDynVar { dst, src } => self.store_dyn_var(dst, src),
            AsmInst::StoreDynVarSpecialized { offset, dst, src } => {
                self.store_dyn_var_specialized(offset.unwrap_concrete(), dst, src);
            }

            AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_,
            } => self.load_ivar_heap(ivarid, is_object_ty, self_),
            AsmInst::LoadIVarInline { ivarid } => self.load_ivar_inline(ivarid),
            AsmInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                using_xmm,
            } => self.store_ivar_heap(src, ivarid, is_object_ty, using_xmm),
            AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
            } => self.store_self_ivar_heap(src, ivarid, is_object_ty),
            AsmInst::StoreIVarInline { src, ivarid } => self.store_ivar_object_inline(src, ivarid),
            AsmInst::LoadStructSlotInline { slot_index } => {
                self.load_struct_slot_inline(slot_index)
            }
            AsmInst::LoadStructSlotHeap { slot_index } => {
                self.load_struct_slot_heap(slot_index)
            }
            AsmInst::StoreStructSlotInline { src, slot_index } => {
                self.store_struct_slot_inline(src, slot_index)
            }
            AsmInst::StoreStructSlotHeap { src, slot_index } => {
                self.store_struct_slot_heap(src, slot_index)
            }
            AsmInst::GuardFrozen { deopt } => {
                let deopt = &labels[deopt];
                self.guard_frozen(deopt);
            }

            AsmInst::LoadCVar { name, using_xmm } => {
                self.load_cvar(name, using_xmm);
            }
            AsmInst::CheckCVar { name, using_xmm } => {
                self.check_cvar(name, using_xmm);
            }
            AsmInst::StoreCVar {
                name,
                src,
                using_xmm,
            } => {
                self.store_cvar(name, src, using_xmm);
            }

            AsmInst::LoadGVar { name, using_xmm } => self.load_gvar(name, using_xmm),
            AsmInst::StoreGVar {
                name,
                src,
                using_xmm,
            } => self.store_gvar(name, src, using_xmm),

            AsmInst::ClassDef {
                base,
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_xmm,
                error,
            } => {
                self.class_def(
                    base,
                    superclass,
                    dst,
                    name,
                    func_id,
                    is_module,
                    using_xmm,
                    &labels[error],
                );
            }
            AsmInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_xmm,
                error,
            } => {
                self.singleton_class_def(base, dst, func_id, using_xmm, &labels[error]);
            }
            AsmInst::MethodDef {
                name,
                func_id,
                using_xmm,
                error,
            } => {
                self.method_def(name, func_id, using_xmm);
                self.handle_error(&labels[error]);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
                error,
            } => {
                self.singleton_method_def(obj, name, func_id, using_xmm);
                self.handle_error(&labels[error]);
            }

            AsmInst::ExpandArray {
                dst,
                len,
                rest_pos,
                using_xmm,
            } => {
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
            }
            AsmInst::CreateArray { src, len } => {
                monoasm!( &mut self.jit,
                    lea  rdi, [r14 - (conv(src))];
                    movq rsi, (len);
                    movq rax, (runtime::create_array);
                    call rax;
                );
            }
            AsmInst::RestKw { rest_kw } => {
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
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_string(arg, len, using_xmm);
            }
            AsmInst::ToA { src, using_xmm } => {
                self.to_a(src, using_xmm);
            }
            AsmInst::ConcatRegexp {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_regexp(arg, len, using_xmm);
            }
            AsmInst::UndefMethod { undef, using_xmm } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    movl rdx, (undef.get());
                    movq rax, (runtime::undef_method);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::AliasMethod {
                new,
                old,
                using_xmm,
            } => {
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
            }
            AsmInst::AliasGvar {
                new,
                old,
                using_xmm,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, r12;          // &mut Globals
                    movl rsi, (new.get());  // new IdentId
                    movl rdx, (old.get());  // old IdentId
                    movq rax, (runtime::alias_global_var);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::DefinedYield { dst, using_xmm } => self.defined_yield(dst, using_xmm),
            AsmInst::DefinedConst {
                dst,
                siteid,
                using_xmm,
            } => self.defined_const(dst, siteid, using_xmm),
            AsmInst::DefinedMethod {
                dst,
                recv,
                name,
                using_xmm,
            } => self.defined_method(dst, recv, name, using_xmm),
            AsmInst::DefinedSuper { dst, using_xmm } => self.defined_super(dst, using_xmm),
            AsmInst::DefinedGvar {
                dst,
                name,
                using_xmm,
            } => self.defined_gvar(dst, name, using_xmm),

            AsmInst::DefinedIvar {
                dst,
                name,
                using_xmm,
            } => self.defined_ivar(dst, name, using_xmm),

            AsmInst::DefinedCvar {
                dst,
                name,
                using_xmm,
            } => self.defined_cvar(dst, name, using_xmm),

            AsmInst::Inline(proc) => (proc.proc)(self, store, labels),
            AsmInst::CFunc_F_F {
                f,
                src,
                dst,
                using_xmm,
            } => {
                let fsrc = src.enc();
                let fret = dst.enc();
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(fsrc);
                    movq rax, (f);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm(fret), xmm0;
                );
            }
            AsmInst::CFunc_FF_F {
                f,
                lhs,
                rhs,
                dst,
                using_xmm,
            } => {
                let flhs = lhs.enc();
                let frhs = rhs.enc();
                let fret = dst.enc();
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(flhs);
                    movq xmm1, xmm(frhs);
                    movq rax, (f);
                    call rax;
                );
                self.xmm_restore(using_xmm);
                monoasm!( &mut self.jit,
                    movq xmm(fret), xmm0;
                );
            }
        }
    }

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
    /// Spill-aware xmm-to-xmm move. Each operand may live in a phys
    /// xmm or a spill slot; we emit the cheapest form for each
    /// combination and avoid the round-trip through xmm0 that the
    /// generic `expand_spills` wrapping would otherwise produce.
    ///
    fn emit_xmm_move(&mut self, src: VirtFPReg, dst: VirtFPReg, base: usize) {
        if src == dst {
            return;
        }
        match (xmm_loc(src, base), xmm_loc(dst, base)) {
            (XmmLoc::Phys(s), XmmLoc::Phys(d)) => monoasm!( &mut self.jit,
                movq xmm(d), xmm(s);
            ),
            (XmmLoc::Phys(s), XmmLoc::Spill(d_off)) => monoasm!( &mut self.jit,
                movq [rbp - (d_off)], xmm(s);
            ),
            (XmmLoc::Spill(s_off), XmmLoc::Phys(d)) => monoasm!( &mut self.jit,
                movq xmm(d), [rbp - (s_off)];
            ),
            (XmmLoc::Spill(s_off), XmmLoc::Spill(d_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (s_off)];
                movq [rbp - (d_off)], xmm0;
            ),
        }
    }

    ///
    /// Spill-aware xmm swap. When both operands are spilled we swap
    /// the two memory slots through xmm0+xmm1 (avoiding rax/rcx so
    /// nothing in the surrounding code's GP state is disturbed).
    ///
    fn emit_xmm_swap(&mut self, l: VirtFPReg, r: VirtFPReg, base: usize) {
        if l == r {
            return;
        }
        match (xmm_loc(l, base), xmm_loc(r, base)) {
            (XmmLoc::Phys(lp), XmmLoc::Phys(rp)) => monoasm!( &mut self.jit,
                movq xmm0, xmm(lp);
                movq xmm(lp), xmm(rp);
                movq xmm(rp), xmm0;
            ),
            (XmmLoc::Phys(lp), XmmLoc::Spill(r_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (r_off)];
                movq [rbp - (r_off)], xmm(lp);
                movq xmm(lp), xmm0;
            ),
            (XmmLoc::Spill(l_off), XmmLoc::Phys(rp)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                movq [rbp - (l_off)], xmm(rp);
                movq xmm(rp), xmm0;
            ),
            (XmmLoc::Spill(l_off), XmmLoc::Spill(r_off)) => monoasm!( &mut self.jit,
                movq xmm0, [rbp - (l_off)];
                movq xmm1, [rbp - (r_off)];
                movq [rbp - (r_off)], xmm0;
                movq [rbp - (l_off)], xmm1;
            ),
        }
    }
}
