use super::*;

impl JitContext {
    pub(super) fn compile_basic_block(
        &mut self,
        codegen: &mut Codegen,
        store: &Store,
        func: &ISeqInfo,
        position: Option<BytecodePtr>,
        bbid: BasicBlockId,
    ) -> AsmIr {
        let mut ir = AsmIr::new();
        ir.inst.push(AsmInst::Label(self.basic_block_labels[&bbid]));
        let mut bbctx = if let Some(bb) = self.target_ctx.remove(&bbid) {
            bb
        } else if let Some(bb) = self.incoming_context(&mut ir, func, bbid) {
            self.gen_continuation(&mut ir);
            bb
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return ir;
        };

        let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc_pos in begin..=end {
            ir.bc_index(bc_pos);
            bbctx.next_sp = func.get_sp(bc_pos);

            match self.compile_instruction(codegen, &mut ir, &mut bbctx, store, func, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Exit => return ir,
                CompileResult::Recompile => {
                    let pc = func.get_pc(bc_pos);
                    ir.recompile_and_deopt(&mut bbctx, pc, position);
                    return ir;
                }
                CompileResult::Break => break,
            }

            ir.clear(&mut bbctx);
            bbctx.sp = bbctx.next_sp;
        }

        let next_idx = end + 1;
        if let Some(next_bbid) = func.bb_info.is_bb_head(next_idx) {
            let label = codegen.jit.label();
            self.new_continue(func, end, next_bbid, bbctx, label);
            if let Some(target_ctx) = self.incoming_context(&mut ir, func, next_bbid) {
                self.gen_continuation(&mut ir);
                assert!(self.target_ctx.insert(next_bbid, target_ctx).is_none());
            }
        } else {
            unreachable!();
        }
        ir
    }

    fn compile_instruction(
        &mut self,
        codegen: &mut Codegen,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        store: &Store,
        func: &ISeqInfo,
        bc_pos: BcIndex,
    ) -> CompileResult {
        let pc = func.get_pc(bc_pos);
        let trace_ir = func.trace_ir(store, bc_pos);
        match trace_ir {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart { .. } => {
                self.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count);
                self.loop_count -= 1;
                if self.is_loop && self.loop_count == 0 {
                    ir.deopt(bbctx, pc);
                    return CompileResult::Break;
                }
            }
            TraceIr::Integer(dst, i) => {
                ir.store_concrete_value(bbctx, dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                ir.store_concrete_value(bbctx, dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                ir.store_concrete_value(bbctx, dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                ir.unlink(bbctx, dst);
                if val.is_packed_value() || val.is_float() {
                    ir.store_concrete_value(bbctx, dst, val);
                } else {
                    ir.deep_copy_lit(bbctx, val);
                    ir.reg2acc_guarded(bbctx, GP::Rax, dst, Guarded::from_concrete_value(val));
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.write_back_range(bbctx, args, pos_num as u16);
                ir.unlink(bbctx, dst);
                ir.new_array(bbctx, callid);
                ir.reg2acc_guarded(bbctx, GP::Rax, dst, Guarded::ArrayTy);
            }
            TraceIr::Lambda { dst, func_id } => {
                ir.unlink(bbctx, dst);
                ir.new_lambda(bbctx, func_id);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.write_back_range(bbctx, args, len * 2);
                ir.unlink(bbctx, dst);
                ir.new_hash(bbctx, args, len as _);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.write_back_slots(bbctx, &[start, end]);
                ir.unlink(bbctx, dst);
                ir.new_range(bbctx, pc, start, end, exclude_end);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Index {
                dst,
                base,
                idx,
                base_class,
                idx_class,
            } => {
                ir.index(bbctx, dst, base, idx, base_class, idx_class, pc);
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                base_class,
                idx_class,
            } => {
                ir.index_assign(bbctx, src, base, idx, base_class, idx_class, pc);
            }
            TraceIr::LoadConst(dst, id) => {
                ir.unlink(bbctx, dst);

                if let ConstCache {
                    cached_version,
                    cached_base_class,
                    cached_value: Some(cached_val),
                } = store[id].cache
                {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_base_class {
                            ir.fetch_to_reg(bbctx, slot, GP::Rax);
                            let deopt = ir.new_deopt(bbctx, pc);
                            ir.inst.push(AsmInst::GuardBaseClass { base_class, deopt });
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    let deopt = ir.new_deopt(bbctx, pc);
                    if let Some(f) = cached_val.try_float() {
                        let fdst = ir.store_new_both(bbctx, dst, Guarded::Float);
                        ir.inst.push(AsmInst::LoadFloatConstant {
                            fdst,
                            f,
                            cached_version,
                            deopt,
                        });
                        ir.reg2stack(GP::Rax, dst);
                    } else {
                        ir.inst.push(AsmInst::LoadGenericConstant {
                            cached_val,
                            cached_version,
                            deopt,
                        });
                        ir.rax2acc(bbctx, dst);
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, id) => {
                ir.fetch_to_reg(bbctx, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                ir.unlink(bbctx, ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                ir.unlink(bbctx, ret);
                ir.block_arg(bbctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.load_ivar(bbctx, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.store_ivar(bbctx, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                ir.jit_load_cvar(bbctx, pc, name, dst);
            }
            TraceIr::CheckCvar { dst, name } => {
                ir.jit_check_cvar(bbctx, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                ir.jit_store_cvar(bbctx, pc, name, val);
            }
            TraceIr::LoadGvar { dst, name } => {
                ir.jit_load_gvar(bbctx, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                ir.jit_store_gvar(bbctx, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                ir.unlink(bbctx, dst);
                ir.load_svar(bbctx, id);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                ir.unlink(bbctx, dst);
                if !dst.is_self() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, bitnot_value);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    ir.store_concrete_value(bbctx, dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    ir.store_concrete_value(bbctx, dst, Value::bool(true));
                } else {
                    ir.fetch_to_reg(bbctx, src, GP::Rdi);
                    ir.inst.push(AsmInst::Not);
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fsrc = ir.fetch_float_assume_float(bbctx, src, deopt);
                let dst = ir.xmm_write(bbctx, dst);
                ir.xmm_move(fsrc, dst);
                ir.inst.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::IBinOp {
                kind, dst, mode, ..
            } => {
                ir.gen_binop_integer(bbctx, pc, kind, dst, mode);
            }
            TraceIr::FBinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fmode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                if let Some(ret) = dst {
                    let dst = ir.xmm_write(bbctx, ret);
                    let using_xmm = bbctx.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if lhs_class.is_none() || rhs_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_binop(bbctx, pc, kind);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::FCmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if kind != CmpKind::Cmp {
                    let deopt = ir.new_deopt(bbctx, pc);
                    let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                    ir.unlink(bbctx, dst);
                    ir.clear(bbctx);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else {
                    ir.fetch_binary(bbctx, mode);
                    ir.generic_cmp(bbctx, pc, kind);
                }
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ICmp { kind, dst, mode } => {
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Cmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if lhs_class.is_none() || rhs_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_cmp(bbctx, pc, kind);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::FCmpBr {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
                dest,
                brkind,
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                let deopt = ir.new_deopt(bbctx, pc);
                let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.float_cmp_br(mode, kind, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::ICmpBr {
                kind,
                dst,
                mode,
                dest,
                brkind,
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::CmpBr {
                kind,
                dst,
                mode,
                dest,
                brkind,
                ..
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                ir.fetch_binary(bbctx, mode);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.generic_cmp(bbctx, pc, kind);
                ir.inst.push(AsmInst::GenericCondBr {
                    brkind,
                    branch_dest,
                });
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::Mov(dst, src) => {
                ir.copy_slot(bbctx, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                ir.write_back_range(bbctx, arg, len);
                ir.unlink(bbctx, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.write_back_range(bbctx, arg, len);
                ir.unlink(bbctx, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_regexp(bbctx, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    ir.unlink(bbctx, SlotId(reg));
                }
                ir.expand_array(bbctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(bbctx, pc, new, old);
            }
            TraceIr::MethodCall {
                callid,
                recv_class,
                fid,
                version,
            }
            | TraceIr::MethodCallWithBlock {
                callid,
                recv_class,
                fid,
                version,
            } => return ir.compile_call(store, bbctx, pc, callid, recv_class, fid, version),
            TraceIr::Yield { callid } => {
                ir.compile_yield(store, bbctx, pc, callid);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx, pc);
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                ir.write_back_slots(bbctx, &[obj]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx, pc);
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                ir.class_def(bbctx, dst, base, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                ir.class_def(bbctx, dst, base, None, name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                ir.singleton_class_def(bbctx, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                ir.write_back_slots(bbctx, &[dst, recv]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::Ret(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Ret);
                return CompileResult::Exit;
            }
            TraceIr::MethodRet(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::MethodRet(pc));
                return CompileResult::Exit;
            }
            TraceIr::Break(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Break);
                return CompileResult::Exit;
            }
            TraceIr::Raise(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Raise);
                return CompileResult::Exit;
            }
            TraceIr::EnsureEnd => {
                ir.write_back_locals(bbctx);
                ir.inst.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(dest_idx) => {
                self.compile_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                return CompileResult::Exit;
            }
            TraceIr::CondBr(cond_, dest_idx, false, brkind) => {
                if bbctx.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        self.compile_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Exit;
                    }
                } else if bbctx.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        self.compile_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Exit;
                    }
                } else {
                    let branch_dest = codegen.jit.label();
                    ir.fetch_to_reg(bbctx, cond_, GP::Rax);
                    ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                    self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::NilBr(cond_, dest_idx) => {
                let branch_dest = codegen.jit.label();
                ir.fetch_to_reg(bbctx, cond_, GP::Rax);
                ir.inst.push(AsmInst::NilBr(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_idx) => {
                let branch_dest = codegen.jit.label();
                ir.fetch_to_reg(bbctx, local, GP::Rax);
                ir.inst.push(AsmInst::CheckLocal(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                dest_bb,
                branch_table,
            } => {
                let else_idx = dest_bb[0];
                for bbid in dest_bb {
                    let branch_dest = codegen.jit.label();
                    self.new_branch(func, bc_pos, bbid, bbctx.clone(), branch_dest);
                }
                let deopt = ir.new_deopt(bbctx, pc);
                ir.fetch_guard_fixnum(bbctx, cond, GP::Rdi, deopt);
                ir.opt_case(max, min, else_idx, branch_table);
                return CompileResult::Exit;
            }
        }
        CompileResult::Continue
    }

    fn compile_branch(
        &mut self,
        codegen: &mut Codegen,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        func: &ISeqInfo,
        bc_pos: BcIndex,
        dest: BasicBlockId,
    ) {
        let branch_dest = codegen.jit.label();
        ir.inst.push(AsmInst::Br(branch_dest));
        self.new_branch(func, bc_pos, dest, bbctx.clone(), branch_dest);
    }
}
