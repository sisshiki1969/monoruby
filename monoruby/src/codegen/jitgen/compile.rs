use crate::{bytecodegen::BinOpK, codegen::jitgen::state::LinkMode};

use super::*;

mod loop_analysis;

impl<'a> JitContext<'a> {
    pub(super) fn traceir_to_asmir(&mut self, frame: JitStackFrame) -> Result<JitStackFrame> {
        self.push_frame(frame);

        let state = AbstractState::new(&self);
        let iseq = self.iseq();
        let (bb_begin, bb_end) = if let Some(pc) = self.position() {
            let start_pos = iseq.get_pc_index(Some(pc));
            let bb_begin = iseq.bb_info.get_bb_id(start_pos);
            iseq.bb_info.is_loop_begin(bb_begin).unwrap()
        } else {
            let start_pos = BcIndex::from(0);
            let bb_begin = iseq.bb_info.get_bb_id(start_pos);
            let bb_end = BasicBlockId(iseq.bb_info.len() - 1);
            (bb_begin, bb_end)
        };

        let mut ir = AsmIr::new(self);
        if let Some(pc) = self.position() {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let deopt = ir.new_deopt_with_pc(&state, pc + 1);
            ir.self2reg(GP::Rdi);
            ir.push(AsmInst::GuardClass(GP::Rdi, self.self_class(), deopt));
            ir.push(AsmInst::Preparation);
        };

        //assert!(self.ir.is_empty());
        self.push_ir(None, ir);

        self.branch_continue(bb_begin, state);

        for bbid in bb_begin..=bb_end {
            let ir = self.compile_basic_block(bbid, bbid == bb_end)?;
            self.push_ir(Some(bbid), ir);
        }

        self.backedge_branches();

        #[cfg(feature = "emit-cfg")]
        dump_cfg(self.iseq(), &self.store, bb_begin, bb_end);

        Ok(self.pop_frame())
    }

    fn compile_basic_block(&mut self, bbid: BasicBlockId, last: bool) -> Result<AsmIr> {
        let mut ir = AsmIr::new(self);

        let mut state = match self.incoming_context(bbid, false)? {
            Some(bb) => bb,
            None => {
                #[cfg(feature = "jit-debug")]
                eprintln!("=== no entry {bbid:?}");
                return Ok(ir);
            }
        };
        ir.push(AsmInst::Label(self.get_bb_label(bbid)));

        let BasciBlockInfoEntry { begin, end, .. } = self.iseq().bb_info[bbid];
        for bc_pos in begin..=end {
            ir.bc_index(bc_pos);
            state.set_next_sp(self.iseq().get_sp(bc_pos));

            match self.compile_instruction(&mut ir, &mut state, bc_pos)? {
                CompileResult::Continue => {}
                CompileResult::Raise => {
                    self.unset_return_context_side_effect_guard();
                    return Ok(ir);
                }
                CompileResult::Cease => {
                    return Ok(ir);
                }
                CompileResult::Branch(dest_bb) => {
                    self.new_branch(bc_pos, dest_bb, state);
                    return Ok(ir);
                }
                CompileResult::Return(ret) => {
                    self.new_return(ret);
                    return Ok(ir);
                }
                CompileResult::MethodReturn(ret) => {
                    self.unset_return_context_side_effect_guard();
                    self.new_method_return(ret);
                    return Ok(ir);
                }
                CompileResult::Break(ret) => {
                    self.unset_return_context_side_effect_guard();
                    self.new_break(ret);
                    return Ok(ir);
                }
                CompileResult::ExitLoop => {
                    state.clear_above_next_sp();
                    break;
                }
                CompileResult::Recompile(reason) => {
                    self.new_return(ResultState::default());
                    let pc = self.get_pc(bc_pos);
                    self.recompile_and_deopt(&mut state, &mut ir, reason, pc);
                    return Ok(ir);
                }
                CompileResult::Abort => {
                    #[cfg(feature = "emit-bc")]
                    self.dump_iseq();
                    unreachable!()
                }
            }
            state.clear_above_next_sp();
        }

        if !last {
            self.prepare_next(state, end)
        }

        Ok(ir)
    }

    fn prepare_next(&mut self, state: AbstractState, end: BcIndex) {
        let next_idx = end + 1;
        let next_bbid = self.iseq().bb_info.is_bb_head(next_idx).unwrap();
        self.new_continue(end, next_bbid, state);
    }

    fn compile_instruction(
        &mut self,
        ir: &mut AsmIr,
        state: &mut AbstractState,
        bc_pos: BcIndex,
    ) -> Result<CompileResult> {
        let pc = self.get_pc(bc_pos);
        let trace_ir = self.iseq().trace_ir(self.store, bc_pos);
        #[cfg(feature = "jit-debug")]
        if let Some(fmt) = trace_ir.format(self.store) {
            eprintln!("{fmt}");
        }
        match trace_ir {
            TraceIr::InitMethod(fn_info) => {
                assert!(!self.is_loop());
                ir.push(AsmInst::Init {
                    info: fn_info,
                    not_captured: state.no_capture_guard(),
                });
                ir.push(AsmInst::Preparation);
            }
            TraceIr::LoopStart { .. } => {
                state.unset_side_effect_guard();
                self.inc_loop_count();
                state.exec_gc(ir, false, pc);
            }
            TraceIr::LoopEnd => {
                state.unset_side_effect_guard();
                assert_ne!(0, self.loop_count());
                self.dec_loop_count();
                if self.is_loop() && self.loop_count() == 0 {
                    ir.deopt(state, pc);
                    return Ok(CompileResult::ExitLoop);
                }
            }
            TraceIr::Integer(dst, i) => {
                state.def_C(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                state.def_C(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                state.def_C(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                if val.is_packed_value() || val.is_float() {
                    state.def_C(dst, val);
                } else {
                    state.discard(dst);
                    ir.deep_copy_lit(state.get_using_xmm(), val);
                    state.def_reg2acc_concrete_value(ir, GP::Rax, dst, val);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = self.store[callid];
                state.write_back_range(ir, args, pos_num as u16);
                state.discard(dst);
                ir.new_array(state.get_using_xmm(), callid);
                state.def_reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            TraceIr::Lambda { dst, func_id } => {
                state.unset_side_effect_guard();
                state.discard(dst);
                ir.new_lambda(state.get_using_xmm(), func_id);
                state.def_rax2acc(ir, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                state.write_back_range(ir, args, len * 2);
                state.discard(dst);
                ir.new_hash(state.get_using_xmm(), args, len as _);
                state.def_rax2acc(ir, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                state.write_back_slots(ir, &[start, end]);
                state.discard(dst);
                let error = ir.new_error(state, pc);
                let using_xmm = state.get_using_xmm();
                ir.new_range(start, end, exclude_end, using_xmm, error);
                state.def_rax2acc(ir, dst);
            }

            TraceIr::LoadConst(dst, id) => {
                state.discard(dst);

                if let Some(cache) = &self.store[id].cache {
                    let base_slot = self.store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cache.base_class {
                            state.guard_const_base_class(ir, slot, base_class, pc);
                        } else {
                            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                        }
                    }
                    state.load_constant(ir, dst, cache, pc);
                    state.unset_side_effect_guard();
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                }
            }
            TraceIr::StoreConst(src, id) => {
                state.load(ir, src, GP::Rax);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::StoreConstant { id, using_xmm });
                state.unset_side_effect_guard();
            }
            TraceIr::LoadIvar(dst, name, cache) => {
                let self_class = self.self_class();
                if let Some(ivarid) = self.store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if self.load_ivar(state, ir, dst, self_class, ivarid) {
                        self.set_ivar_heap_accessed();
                    }
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::IvarIdNotFound));
                }
            }
            TraceIr::StoreIvar(src, name, cache) => {
                let self_class = self.self_class();
                if let Some(ivarid) = self.store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    if self.store_ivar(state, ir, src, self_class, ivarid) {
                        self.set_ivar_heap_accessed();
                    }
                    state.unset_side_effect_guard();
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::IvarIdNotFound));
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                state.jit_load_cvar(ir, name, dst, pc);
                state.unset_side_effect_guard();
            }
            TraceIr::CheckCvar { dst, name } => {
                state.jit_check_cvar(ir, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                state.jit_store_cvar(ir, name, val, pc);
                state.unset_side_effect_guard();
            }
            TraceIr::LoadGvar { dst, name } => {
                state.jit_load_gvar(ir, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                state.jit_store_gvar(ir, name, val);
                state.unset_side_effect_guard();
            }
            TraceIr::LoadSvar { dst, id } => {
                ir.load_svar(state, id);
                state.def_rax2acc(ir, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                assert!(!dst.is_self());
                if let Some((offset, not_captured)) = self.outer_stack_offset(state, src.outer) {
                    ir.push(AsmInst::LoadDynVarSpecialized {
                        offset,
                        reg: src.reg,
                        not_captured,
                    });
                } else {
                    ir.push(AsmInst::LoadDynVar { src });
                }
                state.def_rax2acc(ir, dst);
            }
            TraceIr::StoreDynVar(dst, src) => {
                state.load(ir, src, GP::Rdi);
                if let Some((offset, not_captured)) = self.outer_stack_offset(state, dst.outer) {
                    ir.push(AsmInst::StoreDynVarSpecialized {
                        offset,
                        dst: dst.reg,
                        src: GP::Rdi,
                        not_captured,
                    });
                } else {
                    ir.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
                }
                state.unset_side_effect_guard();
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                state.def_S(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                state.def_S(ret);
                ir.block_arg(state, ret, outer, pc);
                state.unset_side_effect_guard();
            }

            TraceIr::Not { dst, src, .. } => {
                if state.is_truthy(src) {
                    state.def_C(dst, Value::bool(false));
                } else if state.is_falsy(src) {
                    state.def_C(dst, Value::bool(true));
                } else {
                    state.load(ir, src, GP::Rdi);
                    ir.push(AsmInst::Not);
                    state.def_rax2acc(ir, dst);
                }
            }
            TraceIr::UnOp { kind, dst, src, ic } => {
                let class = if let Some(class) = state.class(src) {
                    Some(class)
                } else {
                    ic
                };
                match class {
                    Some(INTEGER_CLASS) => {
                        if let Some(src) = state.is_fixnum_literal(src) {
                            match kind {
                                UnOpK::Neg => {
                                    if let Some(res) = src.checked_neg()
                                        && Value::is_i63(res)
                                    {
                                        state.def_C(dst, Value::fixnum(res));
                                        return Ok(CompileResult::Continue);
                                    }
                                }
                                UnOpK::Pos => {
                                    return Ok(CompileResult::Continue);
                                }
                                UnOpK::BitNot => {
                                    state.def_C(dst, Value::integer(!src));
                                    return Ok(CompileResult::Continue);
                                }
                            };
                        }
                        state.load_fixnum(ir, src, GP::Rdi, pc);
                        match kind {
                            UnOpK::Neg => {
                                let deopt = ir.new_deopt(state, pc);
                                ir.push(AsmInst::FixnumNeg {
                                    reg: GP::Rdi,
                                    deopt,
                                })
                            }
                            UnOpK::Pos => {}
                            UnOpK::BitNot => {
                                ir.push(AsmInst::FixnumBitNot { reg: GP::Rdi });
                            }
                        }
                        state.def_reg2acc_fixnum(ir, GP::Rdi, dst);
                    }
                    Some(FLOAT_CLASS) if kind != UnOpK::BitNot => {
                        if let Some(f) = state.is_float_literal(src) {
                            let res = match kind {
                                UnOpK::Neg => -f,
                                UnOpK::Pos => f,
                                UnOpK::BitNot => unreachable!(),
                            };
                            state.def_C(dst, Value::float(res));
                            return Ok(CompileResult::Continue);
                        }
                        let fsrc = state.load_xmm(ir, src, pc);
                        let dst = state.def_F(dst);
                        ir.xmm_move(fsrc, dst);
                        ir.push(AsmInst::XmmUnOp { kind, dst });
                    }
                    Some(recv_class) => {
                        return self
                            .call_unary_method(state, ir, src, recv_class, kind, bc_pos, pc);
                    }
                    _ => {
                        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                    }
                }
            }

            TraceIr::BinOp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => {
                return match state.binop_type(lhs, rhs, ic) {
                    BinaryOpType::Integer(mode) => {
                        state.gen_binop_integer(ir, kind, dst, mode, pc);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Float(info) => {
                        match kind {
                            BinOpK::Exp | BinOpK::Rem => {
                                return self.call_binary_method(
                                    state,
                                    ir,
                                    lhs,
                                    rhs,
                                    info.lhs_class.into(),
                                    kind,
                                    bc_pos,
                                    pc,
                                );
                            }
                            _ => {}
                        }
                        state.gen_binop_float(ir, kind, dst, info, pc);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Other(Some(lhs_class)) => {
                        self.call_binary_method(state, ir, lhs, rhs, lhs_class, kind, bc_pos, pc)
                    }
                    BinaryOpType::Other(None) => {
                        Ok(CompileResult::Recompile(RecompileReason::NotCached))
                    }
                };
            }
            TraceIr::BinCmp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => {
                return match state.binop_type(lhs, rhs, ic) {
                    BinaryOpType::Integer(mode) => {
                        state.gen_cmp_integer(ir, kind, dst, mode, pc);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Float(info) => {
                        state.gen_cmp_float(ir, dst, info, kind, pc);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Other(Some(lhs_class)) => {
                        self.call_binary_method(state, ir, lhs, rhs, lhs_class, kind, bc_pos, pc)
                    }
                    BinaryOpType::Other(None) => {
                        Ok(CompileResult::Recompile(RecompileReason::NotCached))
                    }
                };
            }
            TraceIr::BinCmpBr {
                kind,
                _dst: _,
                lhs,
                rhs,
                dest_bb,
                brkind,
                ic,
            } => {
                return match state.binop_type(lhs, rhs, ic) {
                    BinaryOpType::Integer(mode) => {
                        if let Some(result) =
                            state.check_concrete_i64_cmpbr(mode, kind, brkind, dest_bb)
                        {
                            return Ok(result);
                        }
                        let src_idx = bc_pos + 1;
                        let dest = self.label();
                        state.gen_cmpbr_integer(ir, kind, mode, brkind, dest, pc);
                        self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Float(info) => {
                        if let Some(result) =
                            state.check_concrete_f64_cmpbr(lhs, rhs, kind, brkind, dest_bb)
                        {
                            return Ok(result);
                        }
                        let src_idx = bc_pos + 1;
                        let dest = self.label();
                        let mode = state.load_binary_xmm(ir, info, pc);
                        ir.float_cmp_br(mode, kind, brkind, dest);
                        self.new_side_branch(src_idx, dest_bb, state.clone(), dest);
                        Ok(CompileResult::Continue)
                    }
                    BinaryOpType::Other(Some(lhs_class)) => {
                        let res = self
                            .call_binary_method(state, ir, lhs, rhs, lhs_class, kind, bc_pos, pc)?;
                        if let CompileResult::Continue = res {
                            let src_idx = bc_pos + 1;
                            state.unset_class_version_guard();
                            self.gen_cond_br(state, ir, src_idx, dest_bb, brkind);
                        }
                        Ok(res)
                    }
                    BinaryOpType::Other(None) => {
                        Ok(CompileResult::Recompile(RecompileReason::NotCached))
                    }
                };
            }
            TraceIr::Index {
                dst,
                base,
                idx,
                class: ic,
            } => {
                let (base_class, idx_class) = state.binary_class(base, idx, ic);
                if let (Some(base_class), Some(INTEGER_CLASS)) = (base_class, idx_class) {
                    if self.store[base_class].is_array_ty_instance() {
                        state.array_integer_index(ir, &self.store, dst, base, idx, pc);
                        return Ok(CompileResult::Continue);
                    }
                }
                if let Some(base_class) = base_class {
                    return self.call_binary_method(
                        state,
                        ir,
                        base,
                        idx,
                        base_class,
                        IdentId::_INDEX,
                        bc_pos,
                        pc,
                    );
                }
                return Ok(CompileResult::Recompile(RecompileReason::NotCached));
            }
            TraceIr::IndexAssign {
                base: recv,
                idx,
                src,
                class,
            } => {
                assert_eq!(idx.0 + 1, src.0);
                if let Some((recv_class, idx_class)) = class {
                    if self.store[recv_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
                        state.array_integer_index_assign(ir, self.store, src, recv, idx, pc);
                        return Ok(CompileResult::Continue);
                    }
                    return self.call_ternary_method(
                        state,
                        ir,
                        recv,
                        idx,
                        recv_class,
                        IdentId::_INDEX_ASSIGN,
                        bc_pos,
                        pc,
                    );
                }
                return Ok(CompileResult::Recompile(RecompileReason::NotCached));
            }

            TraceIr::ArrayTEq { lhs, rhs } => {
                state.write_back_slot(ir, lhs);
                state.write_back_slot(ir, rhs);
                state.discard(lhs);
                let error = ir.new_error(state, pc);
                ir.array_teq(state, lhs, rhs);
                ir.handle_error(error);
                state.def_rax2acc(ir, lhs);
                state.unset_side_effect_guard();
            }

            TraceIr::ToA { dst, src } => {
                let error = ir.new_error(state, pc);
                state.write_back_slot(ir, src);
                ir.to_a(state, src);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                state.unset_side_effect_guard();
            }
            TraceIr::Mov(dst, src) => {
                state.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                state.write_back_range(ir, arg, len);
                state.discard(dst);
                let error = ir.new_error(state, pc);
                ir.concat_str(state, arg, len);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                state.unset_side_effect_guard();
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                state.write_back_range(ir, arg, len);
                state.discard(dst);
                let error = ir.new_error(state, pc);
                ir.concat_regexp(state, arg, len);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                state.unset_side_effect_guard();
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len, rest_pos),
            } => {
                state.load(ir, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    state.def_S(SlotId(reg));
                }
                ir.expand_array(state, dst, len, rest_pos);
            }
            TraceIr::UndefMethod { undef } => {
                ir.undef_method(state, undef, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(state, new, old, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::MethodCall {
                _polymorphic: _,
                callid,
                cache,
            } => {
                let callsite = &self.store[callid];
                let recv_class = state.class(callsite.recv);
                let (recv_class, func_id) = if let Some(cache) = cache {
                    if cache.version != self.class_version()
                        || (recv_class.is_some() && Some(cache.recv_class) != recv_class)
                    {
                        // the inline method cache is invalid.
                        let recv_class = if let Some(recv_class) = recv_class {
                            recv_class
                        } else {
                            cache.recv_class
                        };
                        (
                            recv_class,
                            if let Some(fid) = self.jit_check_call(recv_class, callsite.name) {
                                fid
                            } else {
                                return Ok(CompileResult::Recompile(
                                    RecompileReason::MethodNotFound,
                                ));
                            },
                        )
                    } else {
                        // the inline method cache is valid.
                        (cache.recv_class, cache.func_id)
                    }
                } else if let Some(recv_class) = recv_class {
                    // no inline cache, but receiver class is known.
                    if let Some(func_id) = self.jit_check_call(recv_class, callsite.name) {
                        let cache = MethodCacheEntry {
                            recv_class,
                            func_id,
                            version: self.class_version(),
                        };
                        pc.write_method_cache(&cache);
                        (recv_class, func_id)
                    } else {
                        return Ok(CompileResult::Recompile(RecompileReason::MethodNotFound));
                    }
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                };

                return self.compile_method_call(state, ir, pc, recv_class, func_id, callid);
            }
            TraceIr::Yield { callid } => {
                if let Some(block_info) = self.current_method_given_block()
                    && let Some(iseq) = self.store[block_info.block_fid].is_iseq()
                {
                    return self.compile_yield_specialized(
                        state,
                        ir,
                        callid,
                        &block_info,
                        iseq,
                        pc,
                    );
                }
                state.compile_yield(ir, &self.store, callid, pc);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(state, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                state.write_back_slots(ir, &[obj]);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(state, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                state.class_def(ir, dst, base, superclass, name, func_id, false, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                state.class_def(ir, dst, base, None, name, func_id, true, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                state.singleton_class_def(ir, dst, base, func_id, pc);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }

            TraceIr::DefinedYield { dst } => {
                state.def_S(dst);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                state.write_back_slots(ir, &[dst]);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                state.write_back_slots(ir, &[dst, recv]);
                state.def_S(dst);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedSuper { dst } => {
                state.def_S(dst);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedSuper { dst, using_xmm });
            }
            TraceIr::DefinedGvar { dst, name } => {
                state.def_S(dst);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                state.write_back_slots(ir, &[dst]);
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }

            TraceIr::Ret(ret) => {
                state.write_back_locals_if_captured(ir);
                state.load(ir, ret, GP::Rax);
                ir.push(AsmInst::Ret);
                let result = state.as_result(ret);
                state.discard_temps();
                return Ok(CompileResult::Return(result));
            }
            TraceIr::MethodRet(ret) => {
                state.write_back_locals_if_captured(ir);
                state.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.method_caller_stack_offset() {
                    ir.push(AsmInst::MethodRetSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::MethodRet(pc));
                }
                let result = state.as_result(ret);
                state.discard_temps();
                return Ok(CompileResult::MethodReturn(result));
            }
            TraceIr::BlockBreak(ret) => {
                state.write_back_locals_if_captured(ir);
                state.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.iter_caller_stack_offset() {
                    ir.push(AsmInst::BlockBreakSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::BlockBreak(pc));
                }
                let result = state.as_result(ret);
                state.discard_temps();
                return Ok(CompileResult::Break(result));
            }
            TraceIr::Raise(ret) => {
                state.locals_to_S(ir);
                state.load(ir, ret, GP::Rax);
                ir.push(AsmInst::Raise);
                return Ok(CompileResult::Raise);
            }

            TraceIr::EnsureEnd => {
                state.locals_to_S(ir);
                ir.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(dest_bb) => {
                return Ok(CompileResult::Branch(dest_bb));
            }
            TraceIr::CondBr(cond_, dest_bb, false, brkind) => {
                if state.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        return Ok(CompileResult::Branch(dest_bb));
                    }
                } else if state.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        return Ok(CompileResult::Branch(dest_bb));
                    }
                } else {
                    state.load(ir, cond_, GP::Rax);
                    self.gen_cond_br(state, ir, bc_pos, dest_bb, brkind);
                }
            }
            TraceIr::NilBr(cond_, dest_bb) => {
                if state.is_nil(cond_) {
                    return Ok(CompileResult::Branch(dest_bb));
                } else if state.is_not_nil(cond_) {
                } else {
                    let branch_dest = self.label();
                    state.load(ir, cond_, GP::Rax);
                    ir.push(AsmInst::NilBr(branch_dest));
                    self.new_side_branch(bc_pos, dest_bb, state.clone(), branch_dest);
                }
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_bb) => match state.mode(local) {
                LinkMode::S(_) | LinkMode::C(_) => {
                    return Ok(CompileResult::Branch(dest_bb));
                }
                LinkMode::MaybeNone => {
                    let branch_dest = self.label();
                    state.load(ir, local, GP::Rax);
                    ir.push(AsmInst::CheckLocal(branch_dest));
                    let mut side_bb = state.clone();
                    side_bb.set_S(local);
                    self.new_side_branch(bc_pos, dest_bb, side_bb, branch_dest);
                    state.set_None(local);
                }
                LinkMode::None => {}
                _ => unreachable!(),
            },
            TraceIr::CheckKwRest(local) => {
                ir.push(AsmInst::CheckKwRest(local));
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                else_dest,
                branch_table,
            } => {
                state.load_fixnum(ir, cond, GP::Rdi, pc);
                let mut branch_labels = vec![];
                for bbid in branch_table {
                    let branch_dest = self.label();
                    branch_labels.push(branch_dest);
                    self.new_side_branch(bc_pos, bbid, state.clone(), branch_dest);
                }
                let else_label = self.label();
                self.new_side_branch(bc_pos, else_dest, state.clone(), else_label);
                ir.opt_case(max, min, else_label, branch_labels.into());
                return Ok(CompileResult::Cease);
            }
        }
        Ok(CompileResult::Continue)
    }

    fn call_unary_method(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        recv: SlotId,
        recv_class: ClassId,
        name: impl Into<IdentId>,
        bc_pos: BcIndex,
        pc: BytecodePtr,
    ) -> Result<CompileResult> {
        if let Some(func_id) = self.jit_check_method(recv_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, recv);
            assert_eq!(self.store[callid].pos_num, 0);
            self.compile_method_call(state, ir, pc, recv_class, func_id, callid)
        } else {
            Ok(CompileResult::Recompile(RecompileReason::MethodNotFound))
        }
    }

    fn call_binary_method(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        lhs: SlotId,
        rhs: SlotId,
        lhs_class: ClassId,
        name: impl Into<IdentId>,
        bc_pos: BcIndex,
        pc: BytecodePtr,
    ) -> Result<CompileResult> {
        if let Some(fid) = self.jit_check_method(lhs_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, lhs);
            assert_eq!(self.store[callid].args, rhs);
            assert_eq!(self.store[callid].pos_num, 1);
            self.compile_method_call(state, ir, pc, lhs_class, fid, callid)
        } else {
            Ok(CompileResult::Recompile(RecompileReason::MethodNotFound))
        }
    }

    fn call_ternary_method(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        recv: SlotId,
        idx: SlotId,
        recv_class: ClassId,
        name: impl Into<IdentId>,
        bc_pos: BcIndex,
        pc: BytecodePtr,
    ) -> Result<CompileResult> {
        if let Some(fid) = self.jit_check_method(recv_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, recv);
            assert_eq!(self.store[callid].args, idx);
            assert_eq!(self.store[callid].pos_num, 2);
            self.compile_method_call(state, ir, pc, recv_class, fid, callid)
        } else {
            Ok(CompileResult::Recompile(RecompileReason::MethodNotFound))
        }
    }

    fn gen_cond_br(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        src_idx: BcIndex,
        dest: BasicBlockId,
        brkind: BrKind,
    ) {
        let branch_dest = self.label();
        ir.push(AsmInst::CondBr(brkind, branch_dest));
        self.new_side_branch(src_idx, dest, state.clone(), branch_dest);
    }

    fn recompile_and_deopt(
        &self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        reason: RecompileReason,
        pc: BytecodePtr,
    ) {
        let deopt = ir.new_deopt(state, pc);
        match self.jit_type() {
            JitType::Specialized { idx, .. } => ir.push(AsmInst::RecompileDeoptSpecialized {
                idx: *idx,
                deopt,
                reason,
            }),
            _ => ir.push(AsmInst::RecompileDeopt {
                position: self.position(),
                deopt,
                reason,
            }),
        }
    }
}

impl<'a> JitContext<'a> {
    ///
    /// Check whether a method or `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_call(&mut self, recv_class: ClassId, name: Option<IdentId>) -> Option<FuncId> {
        if let Some(name) = name {
            // for method call
            self.jit_check_method(recv_class, name)
        } else {
            // for super
            self.jit_check_super(recv_class)
        }
    }

    ///
    /// Check whether a method *name* of class *class_id* exists in compile time.
    ///
    fn jit_check_method(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        let class_version = self.class_version();
        self.store
            .check_method_for_class_with_version(class_id, name, class_version)?
            .func_id()
    }

    ///
    /// Check whether `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_super(&mut self, recv_class: ClassId) -> Option<FuncId> {
        // for super
        let mother = self.iseq().mother().0;
        let mother_fid = self.store[mother].func_id();
        let owner = self.store[mother_fid].owner_class().unwrap();
        let func_name = self.store[mother_fid].name().unwrap();
        self.store.check_super(recv_class, owner, func_name)
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

enum BinaryOpType {
    Integer(OpMode),
    Float(FBinOpInfo),
    Other(Option<ClassId>),
}

impl AbstractState {
    fn binary_integer_mode(&self, lhs: SlotId, rhs: SlotId) -> OpMode {
        if let Some(rhs) = self.is_i16_literal(rhs) {
            OpMode::RI(lhs, rhs)
        } else if let Some(lhs) = self.is_i16_literal(lhs) {
            OpMode::IR(lhs, rhs)
        } else {
            OpMode::RR(lhs, rhs)
        }
    }

    fn binary_class(
        &self,
        lhs: SlotId,
        rhs: SlotId,
        ic: Option<(ClassId, ClassId)>,
    ) -> (Option<ClassId>, Option<ClassId>) {
        let lhs_class = if let Some(class) = self.class(lhs) {
            Some(class)
        } else {
            ic.map(|(class, _)| class)
        };
        let rhs_class = if let Some(class) = self.class(rhs) {
            Some(class)
        } else {
            ic.map(|(_, class)| class)
        };
        (lhs_class, rhs_class)
    }

    fn binop_type(&self, lhs: SlotId, rhs: SlotId, ic: Option<(ClassId, ClassId)>) -> BinaryOpType {
        let (lhs_class, rhs_class) = self.binary_class(lhs, rhs, ic);
        if let (Some(lhs_class), Some(rhs_class)) = (lhs_class, rhs_class) {
            match (lhs_class, rhs_class) {
                (INTEGER_CLASS, INTEGER_CLASS) => {
                    let mode = self.binary_integer_mode(lhs, rhs);
                    return BinaryOpType::Integer(mode);
                }
                (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => {
                    let info = FBinOpInfo {
                        lhs,
                        rhs,
                        lhs_class: lhs_class.into(),
                        rhs_class: rhs_class.into(),
                    };
                    return BinaryOpType::Float(info);
                }
                _ => {}
            }
        }
        BinaryOpType::Other(lhs_class)
    }
}
