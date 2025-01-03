use super::*;

impl JitContext {
    pub(super) fn compile(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        position: Option<BytecodePtr>,
    ) {
        let func = &store[iseq_id];

        for (loop_start, loop_end) in func.bb_info.loops() {
            self.analyse_loop(store, func, *loop_start, *loop_end);
        }

        let mut bbctx = BBContext::new(&self);

        let mut ir = AsmIr::new();
        if let Some(pc) = position {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let deopt = ir.new_deopt(&bbctx, pc + 1);
            ir.stack2reg(SlotId::self_(), GP::Rdi);
            ir.guard_class(&mut bbctx, SlotId::self_(), GP::Rdi, self.self_class, deopt);
        } else {
            for i in (1 + self.local_num)..self.total_reg_num {
                bbctx.store_concrete_value(SlotId(i as u16), Value::nil());
            }
            bbctx.set_guard_class(SlotId::self_(), self.self_class);
            // for method JIT, class of *self* is already checked in an entry stub.
            match func.trace_ir(store, BcIndex::from(0)) {
                TraceIr::InitMethod(fn_info) => {
                    ir.push(AsmInst::Init(fn_info));
                }
                _ => unreachable!(),
            }
        }
        ir.push(AsmInst::Preparation);

        assert!(self.ir.is_empty());
        let mut ir = vec![ir];
        let start_pos = func.get_pc_index(position);

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);

        let bb_begin = func.bb_info.get_bb_id(start_pos);
        let branch_dest = self.label();
        self.branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                branch_dest,
                cont: true,
            }],
        );

        let bb_end = match func.bb_info.get_loop(bb_begin) {
            Some((a, b)) => {
                assert_eq!(a, bb_begin);
                b
            }
            None => BasicBlockId(func.bb_info.len() - 1),
        };

        ir.extend(
            (bb_begin..=bb_end).map(|bbid| self.compile_basic_block(store, func, position, bbid)),
        );
        self.ir = ir;

        self.backedge_branches(func);

        #[cfg(feature = "emit-cfg")]
        dump_cfg(func, store, bb_begin, bb_end);
    }

    fn compile_basic_block(
        &mut self,
        store: &Store,
        iseq: &ISeqInfo,
        position: Option<BytecodePtr>,
        bbid: BasicBlockId,
    ) -> AsmIr {
        let mut ir = AsmIr::new();
        ir.push(AsmInst::Label(self.basic_block_labels[&bbid]));

        let mut bbctx = match self.generate_entry_bb(&mut ir, iseq, bbid) {
            Some(bb) => bb,
            None => {
                #[cfg(feature = "jit-debug")]
                eprintln!("=== no entry");
                return ir;
            }
        };

        let BasciBlockInfoEntry { begin, end, .. } = iseq.bb_info[bbid];
        for bc_pos in begin..=end {
            #[cfg(feature = "emit-asm")]
            ir.bc_index(bc_pos);
            bbctx.next_sp = iseq.get_sp(bc_pos);

            match self.compile_instruction(&mut ir, &mut bbctx, store, iseq, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Branch | CompileResult::Leave => return ir,
                CompileResult::Recompile => {
                    let pc = iseq.get_pc(bc_pos);
                    ir.recompile_and_deopt(&mut bbctx, pc, position);
                    return ir;
                }
                CompileResult::ExitLoop => break,
            }

            bbctx.clear();
            bbctx.sp = bbctx.next_sp;
        }

        self.prepare_next(&mut ir, bbctx, iseq, end);

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
                CompileResult::Leave | CompileResult::Recompile | CompileResult::ExitLoop => {
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
        } else if let Some(bb) = self.incoming_context(func, bbid) {
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
            if let Some(target_ctx) = self.incoming_context(func, next_bbid) {
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
                bbctx.unlink(dst);
                bbctx.store_concrete_value(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                bbctx.unlink(dst);
                bbctx.store_concrete_value(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                bbctx.unlink(dst);
                bbctx.store_concrete_value(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                bbctx.unlink(dst);
                if val.is_packed_value() || val.is_float() {
                    bbctx.store_concrete_value(dst, val);
                } else {
                    ir.deep_copy_lit(bbctx, val);
                    bbctx.reg2acc_concrete_value(ir, GP::Rax, dst, val);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                bbctx.write_back_range(ir, args, pos_num as u16);
                bbctx.unlink(dst);
                ir.new_array(bbctx, callid);
                bbctx.reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            TraceIr::Lambda { dst, func_id } => {
                bbctx.unlink(dst);
                ir.new_lambda(bbctx, func_id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                bbctx.write_back_range(ir, args, len * 2);
                bbctx.unlink(dst);
                ir.new_hash(bbctx, args, len as _);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                bbctx.write_back_slots(ir, &[start, end]);
                bbctx.unlink(dst);
                ir.new_range(bbctx, pc, start, end, exclude_end);
                bbctx.rax2acc(ir, dst);
            }

            TraceIr::LoadConst(dst, id) => {
                bbctx.unlink(dst);

                if let Some(cache) = &store[id].cache {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cache.base_class {
                            ir.guard_base_class(bbctx, slot, base_class, pc);
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    ir.load_constant(bbctx, dst, cache, pc);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, id) => {
                bbctx.fetch_for_gpr(ir, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                bbctx.unlink(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                bbctx.unlink(ret);
                ir.block_arg(bbctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(dst, name, cache) => {
                let self_class = self.self_class;
                if let Some(ivarid) = store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if bbctx.load_ivar(ir, dst, self_class, ivarid) {
                        self.ivar_heap_accessed = true;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, name, cache) => {
                let self_class = self.self_class;
                if let Some(ivarid) = store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if bbctx.store_ivar(ir, src, self_class, ivarid) {
                        self.ivar_heap_accessed = true;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                bbctx.jit_load_cvar(ir, pc, name, dst);
            }
            TraceIr::CheckCvar { dst, name } => {
                bbctx.jit_check_cvar(ir, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                bbctx.jit_store_cvar(ir, pc, name, val);
            }
            TraceIr::LoadGvar { dst, name } => {
                bbctx.jit_load_gvar(ir, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                bbctx.jit_store_gvar(ir, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                bbctx.unlink(dst);
                ir.load_svar(bbctx, id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                bbctx.unlink(dst);
                if !dst.is_self() {
                    ir.push(AsmInst::LoadDynVar { src });
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    bbctx.unlink(dst);
                    bbctx.store_concrete_value(dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    bbctx.unlink(dst);
                    bbctx.store_concrete_value(dst, Value::bool(true));
                } else {
                    bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                    bbctx.unlink(dst);
                    ir.push(AsmInst::Not);
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                if let Some(INTEGER_CLASS) = src_class {
                    let deopt = ir.new_deopt(bbctx, pc);
                    bbctx.fetch_fixnum(ir, src, GP::Rdi, deopt);
                    ir.push(AsmInst::FixnumBitNot { reg: GP::Rdi });
                    bbctx.reg2acc_fixnum(ir, GP::Rdi, dst);
                } else {
                    bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                    ir.generic_unop(bbctx, pc, bitnot_value);
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fsrc = bbctx.fetch_float_for_xmm(ir, src, deopt);
                let dst = bbctx.xmm_write(dst);
                ir.xmm_move(fsrc, dst);
                ir.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx, pc);
                bbctx.fetch_fixnum(ir, src, GP::Rdi, deopt);
                match kind {
                    UnOpK::Neg => ir.push(AsmInst::FixnumNeg {
                        reg: GP::Rdi,
                        deopt,
                    }),
                    UnOpK::Pos => {}
                }
                bbctx.reg2acc_fixnum(ir, GP::Rdi, dst);
            }
            TraceIr::UnOp { kind, dst, src, .. } => {
                bbctx.fetch_for_gpr(ir, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::IBinOp {
                kind, dst, mode, ..
            } => {
                bbctx.gen_binop_integer(ir, pc, kind, dst, mode);
            }
            TraceIr::FBinOp { kind, info } => {
                let fmode = bbctx.fmode(ir, info, pc);
                if let Some(dst) = info.dst {
                    let dst = bbctx.xmm_write(dst);
                    let using_xmm = bbctx.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::GBinOp { kind, info } => {
                let recv_class = info.lhs_class;
                let class_version = self.class_version;
                if let Some(entry) =
                    store.check_method_for_class(recv_class, kind.to_id(), class_version)
                    && let Some(fid) = entry.func_id()
                {
                    return bbctx.compile_binop_call(ir, store, fid, class_version, info, pc);
                } else {
                    bbctx.fetch_binary(ir, info.mode);
                    bbctx.generic_binop(ir, pc, kind);
                    bbctx.rax2acc(ir, info.dst);
                }
            }
            TraceIr::GBinOpNotrace { .. } => return CompileResult::Recompile,
            TraceIr::FCmp { kind, info } => {
                if kind != CmpKind::Cmp {
                    let mode = bbctx.fmode(ir, info, pc);
                    bbctx.unlink(info.dst);
                    bbctx.clear();
                    ir.push(AsmInst::FloatCmp { kind, mode });
                } else {
                    bbctx.fetch_binary(ir, info.mode);
                    bbctx.unlink(info.dst);
                    bbctx.generic_cmp(ir, pc, kind);
                }
                bbctx.rax2acc(ir, info.dst);
            }
            TraceIr::ICmp { kind, dst, mode } => {
                bbctx.fetch_fixnum_binary(ir, pc, &mode);
                ir.push(AsmInst::IntegerCmp { kind, mode });
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::GCmp { kind, info } => {
                let recv_class = info.lhs_class;
                let class_version = self.class_version;
                if let Some(entry) = store.check_method_for_class(
                    recv_class,
                    Self::cmpkind_to_id(kind),
                    class_version,
                ) && let Some(fid) = entry.func_id()
                {
                    return bbctx.compile_binop_call(ir, store, fid, class_version, info, pc);
                } else {
                    bbctx.gen_cmp_generic(ir, pc, kind, info);
                }
            }
            TraceIr::GCmpNotrace { .. } => return CompileResult::Recompile,
            TraceIr::FCmpBr {
                kind,
                info,
                dest,
                brkind,
            } => {
                let index = bc_pos + 1;
                let branch_dest = self.label();
                let mode = bbctx.fmode(ir, info, pc);
                bbctx.unlink(info.dst);
                bbctx.clear();
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
                bbctx.fetch_fixnum_binary(ir, pc, &mode);
                bbctx.unlink(dst);
                bbctx.clear();
                ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::GCmpBr {
                kind,
                info,
                dest,
                brkind,
                ..
            } => {
                let index = bc_pos + 1;
                let branch_dest = self.label();
                bbctx.gen_cmpbr_generic(ir, pc, kind, info.mode, info.dst, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::GCmpBrNotrace { .. } => return CompileResult::Recompile,
            TraceIr::Index {
                dst,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    bbctx.index(ir, store, dst, base, idx, base_class, idx_class, pc);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    bbctx.index_assign(ir, store, src, base, idx, base_class, idx_class, pc);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::Mov(dst, src) => {
                bbctx.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.unlink(dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.unlink(dst);
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
                    bbctx.unlink(SlotId(reg));
                }
                ir.expand_array(bbctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(bbctx, pc, new, old);
            }
            TraceIr::MethodCall { callid, cache } => {
                if let Some(cache) = cache {
                    return bbctx.compile_call(ir, store, pc, callid, cache);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::Yield { callid } => {
                bbctx.compile_yield(ir, store, pc, callid);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx, pc);
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                bbctx.write_back_slots(ir, &[obj]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::SingletonMethodDef {
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
                bbctx.class_def(ir, dst, base, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, base, None, name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                bbctx.singleton_class_def(ir, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                bbctx.write_back_slots(ir, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                bbctx.write_back_slots(ir, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                bbctx.write_back_slots(ir, &[dst, recv]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                bbctx.write_back_slots(ir, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                bbctx.write_back_slots(ir, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::Ret(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.push(AsmInst::Ret);
                return CompileResult::Leave;
            }
            TraceIr::MethodRet(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.push(AsmInst::MethodRet(pc));
                return CompileResult::Leave;
            }
            TraceIr::BlockBreak(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.push(AsmInst::BlockBreak);
                return CompileResult::Leave;
            }
            TraceIr::Raise(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch_for_gpr(ir, ret, GP::Rax);
                ir.push(AsmInst::Raise);
                return CompileResult::Leave;
            }
            TraceIr::EnsureEnd => {
                bbctx.write_back_locals(ir);
                ir.push(AsmInst::EnsureEnd);
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
                    ir.push(AsmInst::CondBr(brkind, branch_dest));
                    self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::NilBr(cond_, dest_idx) => {
                let branch_dest = self.label();
                bbctx.fetch_for_gpr(ir, cond_, GP::Rax);
                ir.push(AsmInst::NilBr(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_idx) => {
                let branch_dest = self.label();
                bbctx.fetch_for_gpr(ir, local, GP::Rax);
                ir.push(AsmInst::CheckLocal(branch_dest));
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
                bbctx.fetch_fixnum(ir, cond, GP::Rdi, deopt);
                ir.opt_case(max, min, else_idx, branch_table);
                return CompileResult::Branch;
            }
        }
        CompileResult::Continue
    }

    fn cmpkind_to_id(kind: CmpKind) -> IdentId {
        match kind {
            CmpKind::Eq => IdentId::_EQ,
            CmpKind::Ne => IdentId::_NEQ,
            CmpKind::Lt => IdentId::_LT,
            CmpKind::Le => IdentId::_LE,
            CmpKind::Gt => IdentId::_GT,
            CmpKind::Ge => IdentId::_GE,
            CmpKind::TEq => IdentId::_TEQ,
            CmpKind::Cmp => IdentId::_CMP,
        }
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
        ir.push(AsmInst::Br(branch_dest));
        self.new_branch(func, bc_pos, dest, bbctx.clone(), branch_dest);
    }
}

#[cfg(feature = "emit-cfg")]
fn dump_cfg(func: &ISeqInfo, store: &Store, bb_begin: BasicBlockId, bb_end: BasicBlockId) {
    let mut s = format!(
        r###"digraph graph_name {{
  graph [
    charset = "UTF-8";
    label = "{}",
    labelloc = "t",
    labeljust = "c",
    bgcolor = "#343434",
    fontcolor = white,
    fontsize = 20,
    rankdir = TB,
    margin = 0.2,
    splines = spline,
    nodesep = 0.8,
    ranksep = 1.1
  ];

  node [
    colorscheme = "accent8"
    shape = box,
    style = "solid,filled",
    fontsize = 16,
    fontcolor = 5,
    fontname = "Consolas",
    color = 5,
    fillcolor = 4,
  ];

  edge [
    style = solid,
    fontsize = 14,
    fontcolor = white,
    fontname = "Migu 1M",
    color = white,
    labelfloat = true,
    labeldistance = 2.5,
    labelangle = 70
  ];"###,
        store.func_description(func.func_id())
    );
    s += "\n";
    for bbid in bb_begin..=bb_end {
        s += &format!("  {:?} [\n    shape=record\n    label=\"{{{:?}", bbid, bbid);
        let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc in begin..=end {
            if let Some(inst) = func.trace_ir(store, bc).format(store) {
                s += "|";
                let html = html_escape::encode_text(&inst)
                    .replace('|', "\\|")
                    .replace('"', "\\\"");
                s += &format!("{} {}\\l", bc, html);
            }
        }
        s += "}\"\n  ];\n";
    }

    for bbid in bb_begin..=bb_end {
        let entry = &func.bb_info[bbid];
        for succ in &entry.succ {
            s += &format!("  {:?} -> {:?} [headport = n, tailport = s];\n", bbid, succ);
        }
    }

    s += "}\n";
    let path = std::path::PathBuf::from(".cfg");
    match path.try_exists() {
        Ok(true) => {}
        _ => std::fs::create_dir(&path).unwrap(),
    }
    std::fs::write(path.join(format!("fid-{}.dot", func.func_id().get())), s).unwrap();
}
