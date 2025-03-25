use super::*;

impl JitContext {
    pub(super) fn compile(&mut self, store: &Store) {
        let iseq_id = self.iseq_id();
        let iseq = &store[iseq_id];

        for (loop_start, loop_end) in iseq.bb_info.loops() {
            self.analyse_loop(store, iseq, *loop_start, *loop_end);
        }

        let mut bbctx = BBContext::new_with_args(&self);

        let mut ir = AsmIr::new();
        if let Some(pc) = self.position() {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let deopt = ir.new_deopt_with_pc(&bbctx, pc + 1);
            ir.self2reg(GP::Rdi);
            ir.push(AsmInst::GuardClass(GP::Rdi, self.self_class(), deopt));
        } else {
            for i in (1 + self.local_num())..self.total_reg_num() {
                bbctx.def_concrete_value(SlotId(i as u16), Value::nil());
            }
            //bbctx.set_guard_class(SlotId::self_(), self.self_class());
            // for method JIT, class of *self* is already checked in an entry stub.
            match iseq.trace_ir(store, BcIndex::from(0)) {
                TraceIr::InitMethod(fn_info) => {
                    ir.push(AsmInst::Init(fn_info));
                }
                _ => unreachable!(),
            }
        }
        ir.push(AsmInst::Preparation);

        assert!(self.ir.is_empty());
        self.ir.push((None, ir));

        let start_pos = iseq.get_pc_index(self.position());

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);

        let bb_begin = iseq.bb_info.get_bb_id(start_pos);
        self.branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_bb: BasicBlockId(0),
                bbctx,
                mode: BranchMode::Continue,
            }],
        );

        let bb_end = match iseq.bb_info.get_loop(bb_begin) {
            Some((a, b)) => {
                assert_eq!(a, bb_begin);
                b
            }
            None => BasicBlockId(iseq.bb_info.len() - 1),
        };

        for bbid in bb_begin..=bb_end {
            let ir = self.compile_basic_block(store, iseq, bbid, bbid == bb_end);
            self.ir.push((Some(bbid), ir));
        }

        self.backedge_branches(iseq);

        #[cfg(feature = "emit-cfg")]
        dump_cfg(iseq, store, bb_begin, bb_end);
    }

    fn compile_basic_block(
        &mut self,
        store: &Store,
        iseq: &ISeqInfo,
        bbid: BasicBlockId,
        last: bool,
    ) -> AsmIr {
        let mut ir = AsmIr::new();

        let mut bbctx = match self.incoming_context(iseq, bbid) {
            Some(bb) => bb,
            None => {
                #[cfg(feature = "jit-debug")]
                eprintln!("=== no entry");
                return ir;
            }
        };
        ir.push(AsmInst::Label(self.get_bb_label(bbid)));

        let BasciBlockInfoEntry { begin, end, .. } = iseq.bb_info[bbid];
        for bc_pos in begin..=end {
            #[cfg(feature = "emit-asm")]
            ir.bc_index(bc_pos);
            bbctx.next_sp = iseq.get_sp(bc_pos);

            match self.compile_instruction(&mut ir, &mut bbctx, store, iseq, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Branch | CompileResult::Leave => return ir,
                CompileResult::Recompile => {
                    self.recompile_and_deopt(&mut bbctx, &mut ir);
                    return ir;
                }
                CompileResult::ExitLoop => break,
            }

            bbctx.clear_above_next_sp();
            bbctx.sp = bbctx.next_sp;
        }

        bbctx.clear_above_next_sp();
        if !last {
            self.prepare_next(bbctx, iseq, end)
        }

        ir
    }

    pub(super) fn analyse_loop(
        &mut self,
        store: &Store,
        iseq: &ISeqInfo,
        loop_start: BasicBlockId,
        loop_end: BasicBlockId,
    ) {
        let mut ctx = JitContext::from_ctx(self);
        let mut liveness = Liveness::new(self.total_reg_num());

        let bbctx = BBContext::new(&ctx);
        ctx.branch_map.insert(
            loop_start,
            vec![BranchEntry {
                src_bb: BasicBlockId(0),
                bbctx,
                mode: BranchMode::Continue,
            }],
        );

        for bbid in loop_start..=loop_end {
            ctx.analyse_basic_block(store, iseq, &mut liveness, bbid, bbid == loop_end);
        }

        let mut backedge: Option<BBContext> = None;
        if let Some(branches) = ctx.branch_map.remove(&loop_start) {
            for BranchEntry { src_bb, bbctx, .. } in branches {
                liveness.merge(&bbctx);
                assert!(src_bb > loop_start);
                // backegde
                if let Some(ctx) = &mut backedge {
                    ctx.merge(&bbctx);
                } else {
                    backedge = Some(bbctx);
                }
            }
        }

        self.loop_info.insert(loop_start, (liveness, backedge));
    }

    fn analyse_basic_block(
        &mut self,
        store: &Store,
        iseq: &ISeqInfo,
        liveness: &mut Liveness,
        bbid: BasicBlockId,
        last: bool,
    ) {
        let mut ir = AsmIr::new();
        let mut bbctx = match self.incoming_context(iseq, bbid) {
            Some(bb) => bb,
            None => return,
        };

        let BasciBlockInfoEntry { begin, end, .. } = iseq.bb_info[bbid];
        for bc_pos in begin..=end {
            bbctx.next_sp = iseq.get_sp(bc_pos);

            match self.compile_instruction(&mut ir, &mut bbctx, store, iseq, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Branch => return,
                CompileResult::Leave | CompileResult::Recompile | CompileResult::ExitLoop => {
                    liveness.merge(&bbctx);
                    return;
                }
            }

            bbctx.sp = bbctx.next_sp;
        }
        if !last {
            self.prepare_next(bbctx, iseq, end);
        }
    }

    fn prepare_next(&mut self, bbctx: BBContext, iseq: &ISeqInfo, end: BcIndex) {
        let next_idx = end + 1;
        let next_bbid = iseq.bb_info.is_bb_head(next_idx).unwrap();
        self.new_continue(iseq, end, next_bbid, bbctx);
    }

    fn compile_instruction(
        &mut self,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        store: &Store,
        iseq: &ISeqInfo,
        bc_pos: BcIndex,
    ) -> CompileResult {
        bbctx.set_pc(self.bytecode(bc_pos));
        bbctx.clear_above_sp();
        let trace_ir = iseq.trace_ir(store, bc_pos);
        match trace_ir {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart { .. } => {
                self.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count);
                self.loop_count -= 1;
                if self.is_loop() && self.loop_count == 0 {
                    ir.deopt(bbctx);
                    return CompileResult::ExitLoop;
                }
            }
            TraceIr::Integer(dst, i) => {
                bbctx.discard(dst);
                bbctx.def_concrete_value(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                bbctx.discard(dst);
                bbctx.def_concrete_value(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                bbctx.discard(dst);
                bbctx.def_concrete_value(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                bbctx.discard(dst);
                if val.is_packed_value() || val.is_float() {
                    bbctx.def_concrete_value(dst, val);
                } else {
                    ir.deep_copy_lit(bbctx.get_using_xmm(), val);
                    bbctx.reg2acc_concrete_value(ir, GP::Rax, dst, val);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                bbctx.write_back_range(ir, args, pos_num as u16);
                bbctx.discard(dst);
                ir.new_array(bbctx.get_using_xmm(), callid);
                bbctx.reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            TraceIr::Lambda { dst, func_id } => {
                bbctx.discard(dst);
                ir.new_lambda(bbctx.get_using_xmm(), func_id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                bbctx.write_back_range(ir, args, len * 2);
                bbctx.discard(dst);
                ir.new_hash(bbctx.get_using_xmm(), args, len as _);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                bbctx.write_back_slots(ir, &[start, end]);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx);
                let using_xmm = bbctx.get_using_xmm();
                ir.new_range(start, end, exclude_end, using_xmm, error);
                bbctx.rax2acc(ir, dst);
            }

            TraceIr::LoadConst(dst, id) => {
                bbctx.discard(dst);

                if let Some(cache) = &store[id].cache {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cache.base_class {
                            bbctx.guard_const_base_class(ir, slot, base_class);
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    bbctx.load_constant(ir, dst, cache);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, id) => {
                bbctx.fetch(ir, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                bbctx.discard(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                bbctx.discard(ret);
                ir.block_arg(bbctx, ret, outer);
            }
            TraceIr::LoadIvar(dst, name, cache) => {
                let self_class = self.self_class();
                if let Some(ivarid) = store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if self.load_ivar(bbctx, ir, dst, self_class, ivarid) {
                        self.ivar_heap_accessed = true;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, name, cache) => {
                let self_class = self.self_class();
                if let Some(ivarid) = store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if self.store_ivar(bbctx, ir, src, self_class, ivarid) {
                        self.ivar_heap_accessed = true;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                bbctx.jit_load_cvar(ir, name, dst);
            }
            TraceIr::CheckCvar { dst, name } => {
                bbctx.jit_check_cvar(ir, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                bbctx.jit_store_cvar(ir, name, val);
            }
            TraceIr::LoadGvar { dst, name } => {
                bbctx.jit_load_gvar(ir, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                bbctx.jit_store_gvar(ir, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                bbctx.discard(dst);
                ir.load_svar(bbctx, id);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                bbctx.discard(dst);
                if !dst.is_self() {
                    ir.push(AsmInst::LoadDynVar { src });
                    bbctx.rax2acc(ir, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                bbctx.fetch(ir, src, GP::Rdi);
                ir.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    bbctx.def_concrete_value(dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    bbctx.def_concrete_value(dst, Value::bool(true));
                } else {
                    bbctx.fetch(ir, src, GP::Rdi);
                    bbctx.discard(dst);
                    ir.push(AsmInst::Not);
                    bbctx.rax2acc(ir, dst);
                    bbctx.unset_class_version_guard();
                }
            }
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                if let Some(INTEGER_CLASS) = src_class {
                    let deopt = ir.new_deopt(bbctx);
                    bbctx.fetch_fixnum(ir, src, GP::Rdi, deopt);
                    ir.push(AsmInst::FixnumBitNot { reg: GP::Rdi });
                    bbctx.reg2acc_fixnum(ir, GP::Rdi, dst);
                } else {
                    bbctx.fetch(ir, src, GP::Rdi);
                    bbctx.generic_unop(ir, bitnot_value);
                    bbctx.rax2acc(ir, dst);
                    bbctx.unset_class_version_guard();
                }
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx);
                let fsrc = bbctx.fetch_float_for_xmm(ir, src, deopt);
                let dst = bbctx.xmm_write(dst);
                ir.xmm_move(fsrc, dst);
                ir.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx);
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
                bbctx.fetch(ir, src, GP::Rdi);
                bbctx.generic_unop(ir, kind.generic_func());
                bbctx.rax2acc(ir, dst);
                bbctx.unset_class_version_guard();
            }
            TraceIr::IBinOp {
                kind, dst, mode, ..
            } => {
                bbctx.gen_binop_integer(ir, kind, dst, mode);
            }
            TraceIr::FBinOp { kind, info } => {
                bbctx.gen_binop_float(ir, kind, info);
            }
            TraceIr::GBinOp { kind, info } => {
                let recv_class = info.lhs_class;
                let name = kind.to_id();
                if let Some(fid) = self.jit_check_method(store, recv_class, name) {
                    return self.compile_binop_call(bbctx, ir, store, fid, info);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::GBinOpNotrace { .. } => return CompileResult::Recompile,
            TraceIr::FCmp { kind, info } => {
                bbctx.gen_cmp_float(ir, info, kind);
            }
            TraceIr::ICmp { kind, dst, mode } => bbctx.gen_cmp_integer(ir, kind, dst, mode),
            TraceIr::GCmp { kind, info } => {
                let recv_class = info.lhs_class;
                let name = Self::cmpkind_to_id(kind);
                if let Some(fid) = self.jit_check_method(store, recv_class, name) {
                    return self.compile_binop_call(bbctx, ir, store, fid, info);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::GCmpNotrace { .. } => return CompileResult::Recompile,
            TraceIr::FCmpBr {
                kind,
                info,
                dest_bb,
                brkind,
            } => {
                if let Some(result) = bbctx.check_concrete_float_cmpbr(info.mode, kind, brkind) {
                    if let CompileResult::Branch = result {
                        self.gen_branch(bbctx, iseq, bc_pos, dest_bb);
                    }
                    return result;
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                let mode = bbctx.fmode(ir, info);
                bbctx.discard(info.dst);
                bbctx.clear_above_next_sp();
                ir.float_cmp_br(mode, kind, brkind, dest);
                self.new_side_branch(iseq, src_idx, dest_bb, bbctx.clone(), dest);
            }
            TraceIr::ICmpBr {
                kind,
                dst: _,
                mode,
                dest_bb,
                brkind,
            } => {
                if let Some(result) = bbctx.check_concrete_integer_cmpbr(mode, kind, brkind) {
                    if let CompileResult::Branch = result {
                        self.gen_branch(bbctx, iseq, bc_pos, dest_bb);
                    }
                    return result;
                }
                let src_idx = bc_pos + 1;
                let dest = self.label();
                bbctx.gen_cmpbr_integer(ir, kind, mode, brkind, dest);
                self.new_side_branch(iseq, src_idx, dest_bb, bbctx.clone(), dest);
            }
            TraceIr::GCmpBr {
                kind,
                info,
                dest_bb,
                brkind,
            } => {
                let recv_class = info.lhs_class;
                let name = Self::cmpkind_to_id(kind);
                if let Some(fid) = self.jit_check_method(store, recv_class, name) {
                    match self.compile_binop_call(bbctx, ir, store, fid, info) {
                        CompileResult::Continue => {
                            let src_idx = bc_pos + 1;
                            bbctx.unset_class_version_guard();
                            self.gen_cond_br(bbctx, ir, iseq, src_idx, dest_bb, brkind);
                        }
                        res => return res,
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::GCmpBrNotrace { .. } => return CompileResult::Recompile,
            TraceIr::Index {
                dst,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
                        bbctx.array_integer_index(ir, store, dst, base, idx);
                        return CompileResult::Continue;
                    } else if let Some(fid) =
                        self.jit_check_method(store, base_class, IdentId::_INDEX)
                    {
                        return self
                            .compile_index_call(bbctx, ir, store, fid, base_class, base, idx, dst);
                    }
                }
                return CompileResult::Recompile;
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
                        bbctx.array_integer_index_assign(ir, store, src, base, idx);
                    } else {
                        bbctx.write_back_slots(ir, &[base, idx, src]);
                        ir.generic_index_assign(bbctx, base, idx, src);
                        bbctx.unset_class_version_guard();
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::ToA { dst, src } => {
                let error = ir.new_error(bbctx);
                bbctx.write_back_slot(ir, src);
                ir.to_a(bbctx, src);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::Mov(dst, src) => {
                bbctx.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx);
                ir.concat_regexp(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.rax2acc(ir, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                bbctx.fetch(ir, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    bbctx.discard(SlotId(reg));
                }
                ir.expand_array(bbctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(bbctx, new, old);
                bbctx.unset_class_version_guard();
            }
            TraceIr::MethodCall {
                polymorphic: _,
                callid,
                cache,
            } => {
                if let Some(cache) = cache {
                    return self.compile_method_call(bbctx, ir, store, cache, &store[callid]);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::Yield { callid } => {
                if let Some(block_info) = self.block_info()
                    && let Some(block_iseq) = block_info.is_iseq(store)
                {
                    self.compile_yield_inlined(
                        bbctx,
                        ir,
                        store,
                        callid,
                        block_iseq,
                        block_info.block_self(),
                    );
                } else {
                    bbctx.compile_yield(ir, store, callid);
                }
                bbctx.unset_class_version_guard();
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx);
                bbctx.unset_class_version_guard();
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
                ir.check_bop(bbctx);
                bbctx.unset_class_version_guard();
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, base, superclass, name, func_id, false);
                bbctx.unset_class_version_guard();
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, base, None, name, func_id, true);
                bbctx.unset_class_version_guard();
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                bbctx.singleton_class_def(ir, dst, base, func_id);
                bbctx.unset_class_version_guard();
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
                bbctx.fetch(ir, ret, GP::Rax);
                ir.push(AsmInst::Ret);
                return CompileResult::Leave;
            }
            TraceIr::MethodRet(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch(ir, ret, GP::Rax);
                let pc = bbctx.pc();
                ir.push(AsmInst::MethodRet(pc));
                return CompileResult::Leave;
            }
            TraceIr::BlockBreak(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch(ir, ret, GP::Rax);
                ir.push(AsmInst::BlockBreak);
                return CompileResult::Leave;
            }
            TraceIr::Raise(ret) => {
                bbctx.write_back_locals(ir);
                bbctx.fetch(ir, ret, GP::Rax);
                ir.push(AsmInst::Raise);
                return CompileResult::Leave;
            }
            TraceIr::EnsureEnd => {
                bbctx.write_back_locals(ir);
                ir.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(dest_idx) => {
                self.gen_branch(bbctx, iseq, bc_pos, dest_idx);
                return CompileResult::Branch;
            }
            TraceIr::CondBr(cond_, dest_idx, false, brkind) => {
                if bbctx.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        self.gen_branch(bbctx, iseq, bc_pos, dest_idx);
                        return CompileResult::Branch;
                    }
                } else if bbctx.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        self.gen_branch(bbctx, iseq, bc_pos, dest_idx);
                        return CompileResult::Branch;
                    }
                } else {
                    bbctx.fetch(ir, cond_, GP::Rax);
                    self.gen_cond_br(bbctx, ir, iseq, bc_pos, dest_idx, brkind);
                }
            }
            TraceIr::NilBr(cond_, dest_idx) => {
                if bbctx.is_nil(cond_) {
                    self.gen_branch(bbctx, iseq, bc_pos, dest_idx);
                    return CompileResult::Branch;
                } else if bbctx.is_not_nil(cond_) {
                } else {
                    let branch_dest = self.label();
                    bbctx.fetch(ir, cond_, GP::Rax);
                    ir.push(AsmInst::NilBr(branch_dest));
                    self.new_side_branch(iseq, bc_pos, dest_idx, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_idx) => {
                let branch_dest = self.label();
                bbctx.fetch(ir, local, GP::Rax);
                ir.push(AsmInst::CheckLocal(branch_dest));
                self.new_side_branch(iseq, bc_pos, dest_idx, bbctx.clone(), branch_dest);
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
                    self.new_side_branch(iseq, bc_pos, bbid, bbctx.clone(), branch_dest);
                }
                let deopt = ir.new_deopt(bbctx);
                bbctx.fetch_fixnum(ir, cond, GP::Rdi, deopt);
                ir.opt_case(max, min, else_idx, branch_table);
                return CompileResult::Branch;
            }
        }
        CompileResult::Continue
    }

    fn gen_cond_br(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        iseq: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        brkind: BrKind,
    ) {
        let branch_dest = self.label();
        ir.push(AsmInst::CondBr(brkind, branch_dest));
        self.new_side_branch(iseq, src_idx, dest, bbctx.clone(), branch_dest);
    }

    fn gen_branch(
        &mut self,
        bbctx: &mut BBContext,
        iseq: &ISeqInfo,
        bc_pos: BcIndex,
        dest: BasicBlockId,
    ) {
        self.new_branch(iseq, bc_pos, dest, bbctx.clone());
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
        }
    }

    fn recompile_and_deopt(&self, bbctx: &mut BBContext, ir: &mut AsmIr) {
        let deopt = ir.new_deopt(bbctx);
        match self.jit_type() {
            JitType::Specialized { idx, .. } => {
                ir.push(AsmInst::RecompileDeoptSpecialized { idx: *idx, deopt })
            }
            _ => ir.push(AsmInst::RecompileDeopt {
                position: self.position(),
                deopt,
            }),
        }
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
