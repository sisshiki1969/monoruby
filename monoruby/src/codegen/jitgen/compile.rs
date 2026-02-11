use crate::codegen::jitgen::state::LinkMode;

use super::*;

mod binary_op;
#[cfg(feature = "emit-cfg")]
mod dump_cfg;
mod index;
mod loop_analysis;
mod method_call;
mod variables;

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
            let start_pos = BcIndex::default();
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
        dump_cfg::dump_cfg(&self.store, self.iseq_id(), bb_begin, bb_end);

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

        let BasicBlockInfoEntry { begin, end, .. } = self.iseq().bb_info[bbid];
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
                    self.new_return(ReturnState::default());
                    self.recompile_and_deopt(&mut state, &mut ir, reason);
                    return Ok(ir);
                }
                CompileResult::Abort => {
                    #[cfg(feature = "emit-bc")]
                    self.dump_iseq();
                    unreachable!("JIT compilation aborted at bc_pos={:?}", bc_pos);
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
        assert!(state.no_capture_guard());
        let pc = self.get_pc(bc_pos);
        state.set_pc(pc);
        let trace_ir = TraceIr::from_pc(pc, self.store);
        #[cfg(feature = "jit-debug")]
        if let Some(fmt) = TraceIr::format(self.store, self.iseq_id(), pc) {
            eprintln!("{fmt}");
        }
        match trace_ir {
            TraceIr::InitMethod(info) => {
                assert!(!self.is_loop());
                ir.push(AsmInst::Init { info });
                ir.push(AsmInst::Preparation);
            }
            TraceIr::LoopStart { .. } => {
                state.unset_side_effect_guard();
                self.inc_loop_count();
                state.exec_gc(ir, false);
            }
            TraceIr::LoopEnd => {
                state.unset_side_effect_guard();
                assert_ne!(0, self.loop_count());
                self.dec_loop_count();
                if self.is_loop() && self.loop_count() == 0 {
                    ir.deopt(state);
                    return Ok(CompileResult::ExitLoop);
                }
            }
            TraceIr::Immediate(dst, i) => {
                state.def_C(dst, i);
            }
            TraceIr::Literal(dst, val) => {
                if val.is_packed_value() || val.is_float() || val.is_range().is_some() {
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
                let error = ir.new_error(state);
                let using_xmm = state.get_using_xmm();
                ir.new_range(start, end, exclude_end, using_xmm, error);
                state.def_rax2acc(ir, dst);
            }

            TraceIr::LoadConst(dst, id) => {
                return self.load_constant(state, ir, dst, id);
            }
            TraceIr::StoreConst(src, id) => {
                state.store_constant(ir, src, id);
            }
            TraceIr::LoadIvar(dst, name, cache) => {
                let self_class = self.self_class();
                if let Some(ivarid) = self.store[self_class].get_ivarid(name) {
                    if let Some((cached_class, cached_ivarid)) = cache
                        && cached_class == self_class
                    {
                        assert_eq!(ivarid, cached_ivarid);
                    }
                    self.load_ivar(state, ir, dst, self_class, ivarid);
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
                    self.store_ivar(state, ir, src, self_class, ivarid);
                    state.unset_side_effect_guard();
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::IvarIdNotFound));
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                state.jit_load_cvar(ir, name, dst);
                state.unset_side_effect_guard();
            }
            TraceIr::CheckCvar { dst, name } => {
                state.jit_check_cvar(ir, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                state.jit_store_cvar(ir, name, val);
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
                self.load_dynvar(state, ir, src);
                state.def_rax2acc(ir, dst);
            }
            TraceIr::StoreDynVar(dst, src) => {
                self.store_dynvar(state, ir, dst, src);
                state.unset_side_effect_guard();
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                state.def_S(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                state.def_S(ret);
                ir.block_arg(state, ret, outer);
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
                                        && let Some(res) = Value::check_fixnum(res)
                                    {
                                        state.def_C(dst, res);
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
                        state.load_fixnum(ir, src, GP::Rdi);
                        match kind {
                            UnOpK::Neg => {
                                let deopt = ir.new_deopt(state);
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
                        let fsrc = state.load_xmm(ir, src);
                        let dst = state.def_F(dst);
                        ir.xmm_move(fsrc, dst);
                        ir.push(AsmInst::XmmUnOp { kind, dst });
                    }
                    Some(recv_class) => {
                        return self.call_unary_method(state, ir, src, recv_class, kind, bc_pos);
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
            } => return self.binary_op(state, ir, kind, dst, lhs, rhs, ic, bc_pos),
            TraceIr::BinCmp {
                kind,
                dst,
                lhs,
                rhs,
                ic,
            } => return self.binary_cmp(state, ir, kind, dst, lhs, rhs, ic, bc_pos),
            TraceIr::BinCmpBr {
                kind,
                _dst: _,
                lhs,
                rhs,
                disp,
                brkind,
                ic,
            } => {
                let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
                return self.binart_cmp_br(state, ir, kind, lhs, rhs, dest_bb, brkind, ic, bc_pos);
            }
            TraceIr::Index {
                dst,
                base,
                idx,
                class: ic,
            } => return self.index(state, ir, dst, base, idx, ic, bc_pos),
            TraceIr::IndexAssign {
                base,
                idx,
                src,
                class: ic,
            } => {
                assert_eq!(idx.0 + 1, src.0);
                return self.index_assign(state, ir, base, idx, src, ic, bc_pos);
            }
            TraceIr::MethodCall {
                _polymorphic: _,
                callid,
                cache,
            } => return self.method_call(state, ir, callid, cache),
            TraceIr::Yield { callid } => {
                if let Some(block_info) = self.current_method_given_block()
                    && let Some(iseq) = self.store[block_info.block_fid].is_iseq()
                {
                    return self.compile_yield_specialized(state, ir, callid, &block_info, iseq);
                }
                state.compile_yield(ir, &self.store, callid);
            }
            TraceIr::InlineCache => {}

            TraceIr::ArrayTEq { lhs, rhs } => {
                state.write_back_slot(ir, lhs);
                state.write_back_slot(ir, rhs);
                state.discard(lhs);
                let error = ir.new_error(state);
                ir.array_teq(state, lhs, rhs);
                ir.handle_error(error);
                state.def_rax2acc(ir, lhs);
                state.unset_side_effect_guard();
            }

            TraceIr::ToA { dst, src } => {
                let error = ir.new_error(state);
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
                let error = ir.new_error(state);
                ir.concat_str(state, arg, len);
                ir.handle_error(error);
                state.def_rax2acc(ir, dst);
                state.unset_side_effect_guard();
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                state.write_back_range(ir, arg, len);
                state.discard(dst);
                let error = ir.new_error(state);
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
                ir.undef_method(state, undef);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(state, new, old);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }

            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = state.get_using_xmm();
                ir.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(state);
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
                ir.check_bop(state);
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
                state.class_def(ir, dst, base, superclass, name, func_id, false);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                state.class_def(ir, dst, base, None, name, func_id, true);
                state.unset_class_version_guard();
                state.unset_side_effect_guard();
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                state.singleton_class_def(ir, dst, base, func_id);
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
                assert!(state.no_capture_guard());
                state.load(ir, ret, GP::Rax);
                ir.push(AsmInst::Ret);
                let result = state.as_return(ret);
                state.discard_temps();
                return Ok(CompileResult::Return(result));
            }
            TraceIr::MethodRet(ret) => {
                assert!(state.no_capture_guard());
                state.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.method_caller_stack_offset() {
                    ir.push(AsmInst::MethodRetSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::MethodRet(pc));
                }
                let result = state.as_return(ret);
                state.discard_temps();
                return Ok(CompileResult::MethodReturn(result));
            }
            TraceIr::BlockBreak(ret) => {
                assert!(state.no_capture_guard());
                state.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.iter_caller_stack_offset() {
                    ir.push(AsmInst::BlockBreakSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::BlockBreak(pc));
                }
                let result = state.as_return(ret);
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
            TraceIr::Br(disp) => {
                let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
                return Ok(CompileResult::Branch(dest_bb));
            }
            TraceIr::CondBr(cond_, disp, false, brkind) => {
                let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
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
            TraceIr::NilBr(cond_, disp) => {
                let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
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
            TraceIr::CheckLocal(local, disp) => {
                let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
                match state.mode(local) {
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
                }
            }
            TraceIr::CheckKwRest(local) => {
                ir.push(AsmInst::CheckKwRest(local));
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                else_disp,
                branch_table,
            } => {
                state.load_fixnum(ir, cond, GP::Rdi);
                let mut branch_labels = vec![];
                let else_dest = self.iseq().get_bb(bc_pos + 1 + else_disp);
                for disp in branch_table {
                    let branch_dest = self.label();
                    branch_labels.push(branch_dest);
                    let dest_bb = self.iseq().get_bb(bc_pos + 1 + disp);
                    self.new_side_branch(bc_pos, dest_bb, state.clone(), branch_dest);
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
    ) -> Result<CompileResult> {
        if let Some(func_id) = self.jit_check_method(recv_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, recv);
            assert_eq!(self.store[callid].pos_num, 0);
            self.compile_method_call(state, ir, recv_class, func_id, callid)
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
    ) -> Result<CompileResult> {
        if let Some(fid) = self.jit_check_method(lhs_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, lhs);
            assert_eq!(self.store[callid].args, rhs);
            assert_eq!(self.store[callid].pos_num, 1);
            self.compile_method_call(state, ir, lhs_class, fid, callid)
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
    ) -> Result<CompileResult> {
        if let Some(fid) = self.jit_check_method(recv_class, name.into()) {
            let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
            assert_eq!(self.store[callid].recv, recv);
            assert_eq!(self.store[callid].args, idx);
            assert_eq!(self.store[callid].pos_num, 2);
            self.compile_method_call(state, ir, recv_class, fid, callid)
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
    ) {
        let deopt = ir.new_deopt(state);
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
