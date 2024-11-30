use super::*;

impl JitContext {
    pub(super) fn compile_basic_block(
        &mut self,
        store: &Store,
        func: &ISeqInfo,
        position: Option<BytecodePtr>,
        bbid: BasicBlockId,
    ) -> AsmIr {
        let mut ir = AsmIr::new();
        ir.inst.push(AsmInst::Label(self.basic_block_labels[&bbid]));

        let mut bbctx = match self.generate_entry_bb(&mut ir, func, bbid) {
            Some(bb) => bb,
            None => {
                #[cfg(feature = "jit-debug")]
                eprintln!("=== no entry");
                return ir;
            }
        };

        let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc_pos in begin..=end {
            ir.bc_index(bc_pos);
            bbctx.next_sp = func.get_sp(bc_pos);

            match self.compile_instruction(&mut ir, &mut bbctx, store, func, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Branch | CompileResult::Leave => return ir,
                CompileResult::Deopt => {
                    let pc = func.get_pc(bc_pos);
                    ir.recompile_and_deopt(&mut bbctx, pc, position);
                    return ir;
                }
                CompileResult::ExitLoop => break,
            }

            ir.clear(&mut bbctx);
            bbctx.sp = bbctx.next_sp;
        }

        self.prepare_next(&mut ir, bbctx, func, end);

        ir
    }

    pub(super) fn analyse_loop(
        &mut self,
        store: &Store,
        func: &ISeqInfo,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) {
        let mut ctx = JitContext::from(self);
        let mut liveness = Liveness::new(self.total_reg_num);

        let bbctx = BBContext::new(&ctx);
        let branch_dest = ctx.label();
        ctx.branch_map.insert(
            loop_start,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                branch_dest,
                cont: true,
            }],
        );

        for bbid in loop_start..=loop_end {
            ctx.analyse_basic_block(store, func, &mut liveness, bbid);
        }

        if let Some(branches) = ctx.branch_map.remove(&loop_start) {
            for entry in branches {
                liveness.merge(entry.bbctx);
            }
        }

        //dbg!(loop_start, loop_end, &liveness);

        self.loop_info.insert(loop_start, liveness);
    }

    fn analyse_basic_block(
        &mut self,
        store: &Store,
        func: &ISeqInfo,
        liveness: &mut Liveness,
        bbid: BasicBlockId,
    ) {
        let mut ir = AsmIr::new();
        let mut bbctx = match self.generate_entry_bb(&mut ir, func, bbid) {
            Some(bb) => bb,
            None => return,
        };

        let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc_pos in begin..=end {
            bbctx.next_sp = func.get_sp(bc_pos);

            match self.compile_instruction(&mut ir, &mut bbctx, store, func, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Branch => return,
                CompileResult::Leave | CompileResult::Deopt | CompileResult::ExitLoop => {
                    liveness.merge(bbctx);
                    return;
                }
            }

            //ir.clear(&mut bbctx);
            bbctx.sp = bbctx.next_sp;
        }

        self.prepare_next(&mut ir, bbctx, func, end);
    }

    fn generate_entry_bb(
        &mut self,
        ir: &mut AsmIr,
        func: &ISeqInfo,
        bbid: BasicBlockId,
    ) -> Option<BBContext> {
        if let Some(bb) = self.target_ctx.remove(&bbid) {
            Some(bb)
        } else if let Some(bb) = self.incoming_context(ir, func, bbid) {
            self.gen_continuation(ir);
            Some(bb)
        } else {
            None
        }
    }

    fn prepare_next(&mut self, ir: &mut AsmIr, bbctx: BBContext, func: &ISeqInfo, end: BcIndex) {
        let next_idx = end + 1;
        if let Some(next_bbid) = func.bb_info.is_bb_head(next_idx) {
            let label = self.label();
            self.new_continue(func, end, next_bbid, bbctx, label);
            if let Some(target_ctx) = self.incoming_context(ir, func, next_bbid) {
                self.gen_continuation(ir);
                assert!(self.target_ctx.insert(next_bbid, target_ctx).is_none());
            }
        } else {
            unreachable!();
        }
    }

    fn compile_instruction(
        &mut self,
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
                    return CompileResult::ExitLoop;
                }
            }
            TraceIr::Integer(dst, i) => {
                bbctx.unlink(ir, dst);
                bbctx.store_concrete_value(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                bbctx.unlink(ir, dst);
                bbctx.store_concrete_value(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                bbctx.unlink(ir, dst);
                bbctx.store_concrete_value(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                bbctx.unlink(ir, dst);
                if val.is_packed_value() || val.is_float() {
                    bbctx.store_concrete_value(dst, val);
                } else {
                    ir.deep_copy_lit(bbctx, val);
                    bbctx.reg2acc_concrete_value(ir, GP::Rax, dst, val);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.write_back_range(bbctx, args, pos_num as u16);
                bbctx.unlink(ir, dst);
                ir.new_array(bbctx, callid);
                bbctx.reg2acc_array_ty(ir, GP::Rax, dst);
            }
            TraceIr::Lambda { dst, func_id } => {
                bbctx.unlink(ir, dst);
                ir.new_lambda(bbctx, func_id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.write_back_range(bbctx, args, len * 2);
                bbctx.unlink(ir, dst);
                ir.new_hash(bbctx, args, len as _);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.write_back_slots(bbctx, &[start, end]);
                bbctx.unlink(ir, dst);
                ir.new_range(bbctx, pc, start, end, exclude_end);
                bbctx.rax2acc(ir, dst);
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
                bbctx.unlink(ir, dst);

                if let ConstCache {
                    cached_version,
                    cached_base_class,
                    cached_value: Some(cached_val),
                } = store[id].cache
                {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_base_class {
                            ir.guard_base_class(bbctx, slot, base_class, pc);
                        } else {
                            return CompileResult::Deopt;
                        }
                    }
                    ir.load_constant(bbctx, dst, cached_version, cached_val, pc);
                } else {
                    return CompileResult::Deopt;
                }
            }
            TraceIr::StoreConst(src, id) => {
                bbctx.fetch_for_gpr(ir, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                bbctx.unlink(ir, ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                bbctx.unlink(ir, ret);
                ir.block_arg(bbctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.load_ivar(bbctx, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Deopt;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.store_ivar(bbctx, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Deopt;
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
                bbctx.unlink(ir, dst);
                ir.load_svar(bbctx, id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                bbctx.unlink(ir, dst);
                if !dst.is_self() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Deopt;
                }
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, bitnot_value);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    bbctx.unlink(ir, dst);
                    bbctx.store_concrete_value(dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    bbctx.unlink(ir, dst);
                    bbctx.store_concrete_value(dst, Value::bool(true));
                } else {
                    bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                    bbctx.unlink(ir, dst);
                    ir.inst.push(AsmInst::Not);
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fsrc = bbctx.fetch_float_for_xmm(ir, src, deopt);
                let dst = ir.xmm_write(bbctx, dst);
                ir.xmm_move(fsrc, dst);
                ir.inst.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Deopt;
                }
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                bbctx.rax2acc(ir, dst);
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
                let fmode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, pc);
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
                    return CompileResult::Deopt;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_binop(bbctx, pc, kind);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::FCmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if kind != CmpKind::Cmp {
                    let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, pc);
                    bbctx.unlink(ir, dst);
                    ir.clear(bbctx);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else {
                    ir.fetch_binary(bbctx, mode);
                    bbctx.unlink(ir, dst);
                    ir.generic_cmp(bbctx, pc, kind);
                }
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ICmp { kind, dst, mode } => {
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Cmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if lhs_class.is_none() || rhs_class.is_none() {
                    return CompileResult::Deopt;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_cmp(bbctx, pc, kind);
                bbctx.rax2acc(ir, dst);
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
                let branch_dest = self.label();
                let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, pc);
                bbctx.unlink(ir, dst);
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
                let branch_dest = self.label();
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                bbctx.unlink(ir, dst);
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
                let branch_dest = self.label();
                ir.fetch_binary(bbctx, mode);
                bbctx.unlink(ir, dst);
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
                bbctx.unlink(ir, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.write_back_range(bbctx, arg, len);
                bbctx.unlink(ir, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_regexp(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    bbctx.unlink(ir, SlotId(reg));
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
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.inst.push(AsmInst::Ret);
                return CompileResult::Leave;
            }
            TraceIr::MethodRet(ret) => {
                ir.write_back_locals(bbctx);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.inst.push(AsmInst::MethodRet(pc));
                return CompileResult::Leave;
            }
            TraceIr::BlockBreak(ret) => {
                ir.write_back_locals(bbctx);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.inst.push(AsmInst::BlockBreak);
                return CompileResult::Leave;
            }
            TraceIr::Raise(ret) => {
                ir.write_back_locals(bbctx);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.inst.push(AsmInst::Raise);
                return CompileResult::Leave;
            }
            TraceIr::EnsureEnd => {
                ir.write_back_locals(bbctx);
                ir.inst.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(dest_idx) => {
                self.compile_branch(ir, bbctx, func, bc_pos, dest_idx);
                return CompileResult::Branch;
            }
            TraceIr::CondBr(cond_, dest_idx, false, brkind) => {
                if bbctx.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        self.compile_branch(ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Branch;
                    }
                } else if bbctx.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        self.compile_branch(ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Branch;
                    }
                } else {
                    let branch_dest = self.label();
                    bbctx.fetch_for_gpr(ir, cond_, GP::Rax);
                    ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                    self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::NilBr(cond_, dest_idx) => {
                let branch_dest = self.label();
                bbctx.fetch_for_gpr(ir, cond_, GP::Rax);
                ir.inst.push(AsmInst::NilBr(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_idx) => {
                let branch_dest = self.label();
                bbctx.fetch_for_gpr(ir, local, GP::Rax);
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
                    let branch_dest = self.label();
                    self.new_branch(func, bc_pos, bbid, bbctx.clone(), branch_dest);
                }
                let deopt = ir.new_deopt(bbctx, pc);
                bbctx.fetch_guard_fixnum(ir, cond, GP::Rdi, deopt);
                ir.opt_case(max, min, else_idx, branch_table);
                return CompileResult::Branch;
            }
        }
        CompileResult::Continue
    }

    fn compile_branch(
        &mut self,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        func: &ISeqInfo,
        bc_pos: BcIndex,
        dest: BasicBlockId,
    ) {
        let branch_dest = self.label();
        ir.inst.push(AsmInst::Br(branch_dest));
        self.new_branch(func, bc_pos, dest, bbctx.clone(), branch_dest);
    }
}
