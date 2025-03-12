use super::*;

mod binary_op;
mod constants;
mod defined;
mod definition;
mod index;
mod method_call;
mod variables;

extern "C" fn extend_ivar(rvalue: &mut RValue, heap_len: usize) {
    rvalue.extend_ivar(heap_len);
}

impl Codegen {
    ///
    /// Generate machine code for *inst*.
    ///
    pub(super) fn compile_asmir(
        &mut self,
        store: &Store,
        ctx: &mut JitContext,
        labels: &SideExitLabels,
        inst: AsmInst,
    ) {
        match inst {
            #[cfg(feature = "emit-asm")]
            AsmInst::BcIndex(_) => {}
            AsmInst::Init(info) => {
                self.init_func(&info);
            }
            AsmInst::Preparation => {
                if !ctx.self_class().is_always_frozen() && ctx.ivar_heap_accessed {
                    let ivar_len = store[ctx.self_class()].ivar_len();
                    let heap_len = if ctx.self_ty() == Some(ObjTy::OBJECT) {
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
                let label = ctx.resolve_label(&mut self.jit, label);
                self.jit.bind_label(label);
            }
            AsmInst::AccToStack(r) => {
                self.store_r15(r);
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
                    movq [r14 - (conv(slot))], R(r);
                );
            }
            AsmInst::StackToReg(slot, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), [r14 - (conv(slot))];
                );
            }
            AsmInst::LitToReg(v, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (v.id());
                );
            }
            AsmInst::I32ToReg(i, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movl R(r), (i);
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
            AsmInst::RegAnd(r, i) => {
                let r = r as u64;
                monoasm! { &mut self.jit,
                    andq R(r), (i);
                }
            }
            AsmInst::RegToRSPOffset(r, ofs) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs)], R(r);
                );
            }
            AsmInst::RSPOffsetToArray(ofs) => {
                monoasm!( &mut self.jit,
                    movq rdi, [rsp + (ofs)];
                    movq rax, (to_array);
                    call rax;
                    movq [rsp + (ofs)], rax;
                );
            }
            AsmInst::I32ToRSPOffset(i, ofs) => {
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs)], (Value::i32(i).id());
                );
            }

            AsmInst::XmmMove(l, r) => self.xmm_mov(l, r),
            AsmInst::XmmSwap(l, r) => self.xmm_swap(l, r),
            AsmInst::XmmBinOp {
                kind,
                mode,
                dst,
                using_xmm,
            } => self.float_binop(kind, using_xmm, dst, mode),
            AsmInst::XmmUnOp { kind, dst } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    monoasm!( &mut self.jit,
                        xorps xmm(dst.enc()), [rip + imm];
                    );
                }
                UnOpK::Pos => {}
            },

            AsmInst::NumToXmm(reg, x, side_exit) => {
                self.numeric_val_to_f64(reg, x, &labels[side_exit]);
            }
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::FixnumToXmm(r, x) => {
                self.integer_val_to_f64(r, x);
            }
            AsmInst::FloatToXmm(reg, x, deopt) => {
                self.float_to_f64(reg, x, &labels[deopt]);
            }
            AsmInst::I64ToBoth(i, r, x) => {
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq [r14 - (conv(r))], (Value::integer(i).id());
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

            AsmInst::HandleError(error) => {
                let error = &labels[error];
                self.handle_error(&error);
            }
            AsmInst::GuardClassVersion {
                version,
                position,
                deopt,
            } => {
                let deopt = &labels[deopt];
                self.guard_class_version(version, position, deopt);
            }
            AsmInst::GuardClassVersionSpecialized {
                version,
                idx,
                deopt,
            } => {
                let deopt = &labels[deopt];
                self.guard_class_version_specialized(version, self.specialized_base + idx, deopt);
            }
            AsmInst::Deopt(deopt) => {
                let deopt = &labels[deopt];
                monoasm!( &mut self.jit,
                    jmp deopt;
                );
            }
            AsmInst::RecompileDeopt { position, deopt } => {
                let deopt = &labels[deopt];
                self.recompile_and_deopt(position, deopt)
            }
            AsmInst::RecompileDeoptSpecialized { idx, deopt } => {
                let deopt = &labels[deopt];
                self.recompile_and_deopt_specialized(deopt, self.specialized_base + idx)
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
            AsmInst::WriteBack(wb) => self.gen_write_back(&wb),
            AsmInst::XmmSave(using_xmm) => self.xmm_save(using_xmm),
            AsmInst::XmmRestore(using_xmm) => self.xmm_restore(using_xmm),
            AsmInst::ExecGc(wb) => self.execute_gc(Some(&wb)),
            AsmInst::SetArguments { callid, callee_fid } => {
                let meta = store[callee_fid].meta();
                let offset = store[callee_fid].get_offset();
                let args = store[callid].args;
                self.jit_set_arguments(callid, args, offset, meta);
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
            AsmInst::BlockBreak => {
                self.block_break();
                self.epilogue();
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
                let branch_dest = ctx.resolve_label(&mut self.jit, dest);
                self.cond_br(branch_dest, brkind);
            }
            AsmInst::NilBr(dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    cmpq rax, (NIL_VALUE);
                    jeq  dest;
                );
            }
            AsmInst::CheckLocal(dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    testq rax, rax;
                    jnz  dest;
                );
            }
            AsmInst::OptCase {
                max,
                min,
                else_dest,
                branch_table,
            } => {
                // generate a jump table.
                let jump_table = self.jit.const_align8();
                for bbid in branch_table.iter() {
                    let dest_label = ctx.get_bb_label(*bbid);
                    let dest_label = ctx.resolve_label(&mut self.jit, dest_label);
                    self.jit.abs_address(dest_label);
                }

                let else_dest = ctx.get_bb_label(else_dest);
                let else_dest = ctx.resolve_label(&mut self.jit, else_dest);
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
            AsmInst::BinopCached {
                callee_fid,
                recv_class,
                evict,
            } => {
                let return_addr = self.gen_binop_cached(store, callee_fid, recv_class);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::Send {
                callid,
                callee_fid,
                recv_class,
                error,
                evict,
            } => {
                let error = &labels[error];
                let return_addr = self.gen_send(store, callid, callee_fid, recv_class, error);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::SendSpecialized {
                callid,
                callee_fid,
                entry,
                patch_point,
                error,
                evict,
            } => {
                let error = &labels[error];
                let patch_point = patch_point.map(|label| ctx.resolve_label(&mut self.jit, label));
                let entry_label = ctx.resolve_label(&mut self.jit, entry);
                let return_addr = self.gen_send_specialized(
                    store,
                    callid,
                    callee_fid,
                    entry_label,
                    patch_point,
                    error,
                );
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            /*AsmInst::SendNotCached {
                self_class,
                callid,
                pc,
                error,
                evict,
            } => {
                let error = labels[error];
                let return_addr = self.send_not_cached(store, callid, self_class, pc, error);
                self.set_deopt_with_return_addr(return_addr, evict, labels[evict]);
            }*/
            AsmInst::Yield {
                callid,
                using_xmm,
                error,
                evict,
            } => {
                let error = &labels[error];
                let return_addr = self.gen_yield(callid, using_xmm, error);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }
            AsmInst::YieldSpecialized {
                callid,
                block_iseq,
                block_entry,
                error,
                evict,
            } => {
                let error = &labels[error];
                let block_entry = ctx.resolve_label(&mut self.jit, block_entry);
                let return_addr =
                    self.gen_yield_specialized(store, callid, block_iseq, block_entry, error);
                self.set_deopt_with_return_addr(return_addr, evict, &labels[evict]);
            }

            AsmInst::Not => {
                self.not_rdi_to_rax();
            }
            AsmInst::FixnumNeg { reg, deopt } => {
                let deopt = &labels[deopt];
                let r = reg as u64;
                monoasm! { &mut self.jit,
                    sarq  R(r), 1;
                    negq  R(r);
                    jo    deopt;
                    salq  R(r), 1;
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
            AsmInst::GenericUnOp { func, using_xmm } => {
                self.xmm_save(using_xmm);
                self.call_unop(func);
                self.xmm_restore(using_xmm);
            }

            /*AsmInst::GenericBinOp { kind, using_xmm } => {
                self.generic_binop(kind, using_xmm);
            }*/
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
            AsmInst::IntegerExp { using_xmm } => {
                self.integer_exp(using_xmm);
            }

            AsmInst::GenericCmp { kind, using_xmm } => {
                self.generic_cmp(&kind, using_xmm);
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
                let branch_dest = ctx.resolve_label(&mut self.jit, branch_dest);
                self.cmp_integer(&mode, lhs, rhs);
                self.condbr_int(kind, branch_dest, brkind);
            }
            AsmInst::FloatCmp { kind, mode } => {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                };
                self.cmp_float(&mode);
                self.setflag_float(kind);
            }
            AsmInst::FloatCmpBr {
                kind,
                mode,
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx.resolve_label(&mut self.jit, branch_dest);
                self.cmp_float(&mode);
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
            AsmInst::StoreConstant { id, using_xmm } => {
                self.store_constant(id, using_xmm);
            }

            /*AsmInst::GenericIndex {
                base,
                idx,
                pc,
                using_xmm,
            } => {
                self.generic_index(using_xmm, base, idx, pc);
            }*/
            AsmInst::ArrayU16Index { idx } => {
                self.gen_array_u16_index(idx);
            }
            AsmInst::ArrayIndex => {
                self.gen_array_index();
            }
            AsmInst::GenericIndexAssign {
                src,
                base,
                idx,
                pc,
                using_xmm,
            } => {
                self.generic_index_assign(using_xmm, base, idx, src, pc);
            }
            AsmInst::ArrayU16IndexAssign {
                idx,
                using_xmm,
                error,
            } => {
                self.gen_array_u16_index_assign(using_xmm, &labels[error], idx);
            }
            AsmInst::ArrayIndexAssign { using_xmm, error } => {
                self.gen_array_index_assign(using_xmm, &labels[error]);
            }

            AsmInst::NewArray { callid, using_xmm } => {
                self.new_array(callid, using_xmm);
            }
            AsmInst::NewLambda(func_id, using_xmm) => {
                self.new_lambda(func_id, using_xmm);
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
                self.block_arg_proxy();
                self.store_rax(ret);
            }
            AsmInst::BlockArg {
                ret,
                outer,
                using_xmm,
                error,
            } => {
                self.get_method_lfp(outer);
                self.block_arg(using_xmm);
                self.handle_error(&labels[error]);
                self.store_rax(ret);
            }

            AsmInst::LoadDynVar { src } => self.load_dyn_var(src),
            AsmInst::StoreDynVar { dst, src } => self.store_dyn_var(dst, src),

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
                self_,
                using_xmm,
            } => self.store_ivar_heap(src, ivarid, is_object_ty, self_, using_xmm),
            AsmInst::StoreIVarInline { src, ivarid } => self.store_ivar_object_inline(src, ivarid),

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
            AsmInst::LoadSVar { id, using_xmm } => self.load_svar(id, using_xmm),

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
            } => {
                self.method_def(name, func_id, using_xmm);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
            } => {
                self.singleton_method_def(obj, name, func_id, using_xmm);
            }

            AsmInst::ExpandArray {
                dst,
                len,
                using_xmm,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    lea rsi, [r14 - (conv(dst))];
                    movq rdx, (len);
                    movq rax, (runtime::expand_array);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_string(arg, len, using_xmm);
            }
            AsmInst::ConcatRegexp {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_regexp(arg, len, using_xmm);
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
                    movq rdx, [r14 - (LFP_SELF)];
                    movl rcx, (new.get());
                    movl r8, (old.get());
                    movq r9, [r14 - (LFP_META)];
                    movq rax, (runtime::alias_method);
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

            AsmInst::Inline(proc) => (proc.proc)(self, store, labels),
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

    fn new_lambda(&mut self, func_id: FuncId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movl rdx, (func_id.get());
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_lambda);
            call rax;
        };
        self.restore_lfp();
        self.xmm_restore(using_xmm);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            lea  rdi, [r14 - (conv(args))];
            movq rsi, (len);
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
            lea rdx, [r14 - (conv(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn concat_regexp(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [r14 - (conv(arg))];
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
    fn block_arg_proxy(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rax - (LFP_BLOCK)];
            xorq rdi, rdi;
            movq rsi, 0b10;
            testq rax, 0b1;
            cmovneq rdi, rsi;
            addq rax, rdi;
        };
    }

    ///
    /// Get a block argument of current frame.
    ///
    fn block_arg(&mut self, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, [rax - (LFP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }
}

extern "C" fn to_array(val: Value) -> Value {
    Value::array1(val)
}
