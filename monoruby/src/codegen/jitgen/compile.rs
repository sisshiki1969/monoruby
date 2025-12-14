use crate::codegen::jitgen::slot::LinkMode;

use super::*;

mod loop_analysis;

impl JitContext {
    pub(super) fn traceir_to_asmir(&mut self, store: &Store) {
        let iseq_id = self.iseq_id();
        //store.dump_iseq(iseq_id);

        let iseq = &store[iseq_id];

        let bbctx = BBContext::new_entry(&self, store);
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
            let deopt = ir.new_deopt_with_pc(&bbctx, pc + 1);
            ir.self2reg(GP::Rdi);
            ir.push(AsmInst::GuardClass(GP::Rdi, self.self_class(), deopt));
            ir.push(AsmInst::Preparation);
        };

        //assert!(self.ir.is_empty());
        self.push_ir(None, ir);

        self.branch_continue(bb_begin, bbctx);

        for bbid in bb_begin..=bb_end {
            let ir = self.compile_basic_block(store, iseq_id, bbid, bbid == bb_end);
            self.push_ir(Some(bbid), ir);
        }

        self.backedge_branches(iseq);

        #[cfg(feature = "emit-cfg")]
        dump_cfg(iseq, store, bb_begin, bb_end);
    }

    fn compile_basic_block(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        bbid: BasicBlockId,
        last: bool,
    ) -> AsmIr {
        let mut ir = AsmIr::new(self);
        let iseq = &store[iseq_id];

        let mut bbctx = match self.incoming_context(store, iseq, bbid, false) {
            Some(bb) => bb,
            None => {
                #[cfg(feature = "jit-debug")]
                eprintln!("=== no entry {bbid:?}");
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
                CompileResult::Leave | CompileResult::Cease => {
                    return ir;
                }
                CompileResult::Branch(dest_bb) => {
                    self.new_branch(iseq, bc_pos, dest_bb, bbctx);
                    return ir;
                }
                CompileResult::Return(ret) => {
                    self.new_return(ret);
                    return ir;
                }
                CompileResult::MethodReturn(ret) => {
                    self.new_method_return(ret);
                    return ir;
                }
                CompileResult::Break(ret) => {
                    self.new_break(ret);
                    return ir;
                }
                CompileResult::Recompile(reason) => {
                    self.new_return(ResultState::Value);
                    let pc = self.get_pc(store, bc_pos);
                    self.recompile_and_deopt(&mut bbctx, &mut ir, reason, pc);
                    return ir;
                }
                CompileResult::ExitLoop => {
                    bbctx.clear_above_next_sp();
                    break;
                }
                CompileResult::Abort => {
                    store.dump_iseq(iseq_id);
                    unreachable!()
                }
            }
            bbctx.clear_above_next_sp();
        }

        if !last {
            self.prepare_next(bbctx, iseq, end)
        }

        ir
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
        let pc = self.get_pc(store, bc_pos);
        let trace_ir = iseq.trace_ir(store, bc_pos);
        #[cfg(feature = "jit-debug")]
        if let Some(fmt) = trace_ir.format(store) {
            eprintln!("{fmt}");
        }
        match trace_ir {
            TraceIr::InitMethod(fn_info) => {
                assert!(!self.is_loop());
                ir.push(AsmInst::Init {
                    info: fn_info,
                    is_method: store[iseq.func_id()].is_not_block(),
                });
                ir.push(AsmInst::Preparation);
            }
            TraceIr::LoopStart { .. } => {
                self.inc_loop_count();
                bbctx.exec_gc(ir, false, pc);
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count());
                self.dec_loop_count();
                if self.is_loop() && self.loop_count() == 0 {
                    ir.deopt(bbctx, pc);
                    return CompileResult::ExitLoop;
                }
            }
            TraceIr::Integer(dst, i) => {
                bbctx.def_C(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                bbctx.def_C(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                bbctx.def_C(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                if val.is_packed_value() || val.is_float() {
                    bbctx.def_C(dst, val);
                } else {
                    bbctx.discard(dst);
                    ir.deep_copy_lit(bbctx.get_using_xmm(), val);
                    bbctx.def_reg2acc_concrete_value(ir, GP::Rax, dst, val);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                bbctx.write_back_range(ir, args, pos_num as u16);
                bbctx.discard(dst);
                ir.new_array(bbctx.get_using_xmm(), callid);
                bbctx.def_reg2acc_class(ir, GP::Rax, dst, ARRAY_CLASS);
            }
            TraceIr::Lambda { dst, func_id } => {
                bbctx.discard(dst);
                ir.new_lambda(bbctx.get_using_xmm(), func_id);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                bbctx.write_back_range(ir, args, len * 2);
                bbctx.discard(dst);
                ir.new_hash(bbctx.get_using_xmm(), args, len as _);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                bbctx.write_back_slots(ir, &[start, end]);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx, pc);
                let using_xmm = bbctx.get_using_xmm();
                ir.new_range(start, end, exclude_end, using_xmm, error);
                bbctx.def_rax2acc(ir, dst);
            }

            TraceIr::LoadConst(dst, id) => {
                bbctx.discard(dst);

                if let Some(cache) = &store[id].cache {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cache.base_class {
                            bbctx.guard_const_base_class(ir, slot, base_class, pc);
                        } else {
                            return CompileResult::Recompile(RecompileReason::NotCached);
                        }
                    }
                    bbctx.load_constant(ir, dst, cache, pc);
                } else {
                    return CompileResult::Recompile(RecompileReason::NotCached);
                }
            }
            TraceIr::StoreConst(src, id) => {
                bbctx.load(ir, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::StoreConstant { id, using_xmm });
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
                        self.set_ivar_heap_accessed();
                    }
                } else {
                    return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
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
                        self.set_ivar_heap_accessed();
                    }
                } else {
                    return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                bbctx.jit_load_cvar(ir, name, dst, pc);
            }
            TraceIr::CheckCvar { dst, name } => {
                bbctx.jit_check_cvar(ir, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                bbctx.jit_store_cvar(ir, name, val, pc);
            }
            TraceIr::LoadGvar { dst, name } => {
                bbctx.jit_load_gvar(ir, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                bbctx.jit_store_gvar(ir, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                ir.load_svar(bbctx, id);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                //bbctx.discard(dst);
                assert!(!dst.is_self());
                if let Some(offset) = self.outer_stack_offset(src.outer) {
                    ir.push(AsmInst::LoadDynVarSpecialized {
                        offset,
                        src: src.reg,
                    });
                } else {
                    ir.push(AsmInst::LoadDynVar { src });
                }
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::StoreDynVar(dst, src) => {
                bbctx.load(ir, src, GP::Rdi);
                if let Some(offset) = self.outer_stack_offset(dst.outer) {
                    ir.push(AsmInst::StoreDynVarSpecialized {
                        offset,
                        dst: dst.reg,
                        src: GP::Rdi,
                    });
                } else {
                    ir.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
                }
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                bbctx.def_S(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                bbctx.def_S(ret);
                ir.block_arg(bbctx, ret, outer, pc);
            }

            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    bbctx.def_C(dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    bbctx.def_C(dst, Value::bool(true));
                } else {
                    bbctx.load(ir, src, GP::Rdi);
                    bbctx.discard(dst);
                    ir.push(AsmInst::Not);
                    bbctx.def_rax2acc(ir, dst);
                    bbctx.unset_class_version_guard();
                }
            }
            TraceIr::BitNot {
                dst,
                src,
                ic: src_class,
            } => {
                let src_class = if let Some(class) = bbctx.class(src) {
                    Some(class)
                } else {
                    src_class
                };
                if let Some(INTEGER_CLASS) = src_class {
                    bbctx.load_fixnum(ir, src, GP::Rdi, pc);
                    ir.push(AsmInst::FixnumBitNot { reg: GP::Rdi });
                    bbctx.def_reg2acc_fixnum(ir, GP::Rdi, dst);
                } else {
                    bbctx.load(ir, src, GP::Rdi);
                    bbctx.generic_unop(ir, bitnot_value, pc);
                    bbctx.def_rax2acc(ir, dst);
                    bbctx.unset_class_version_guard();
                }
            }
            TraceIr::UnOp { kind, dst, src, ic } => {
                let class = if let Some(class) = bbctx.class(src) {
                    Some(class)
                } else {
                    ic
                };
                match class {
                    Some(INTEGER_CLASS) => {
                        bbctx.load_fixnum(ir, src, GP::Rdi, pc);
                        match kind {
                            UnOpK::Neg => {
                                let deopt = ir.new_deopt(bbctx, pc);
                                ir.push(AsmInst::FixnumNeg {
                                    reg: GP::Rdi,
                                    deopt,
                                })
                            }
                            UnOpK::Pos => {}
                        }
                        bbctx.def_reg2acc_fixnum(ir, GP::Rdi, dst);
                    }
                    Some(FLOAT_CLASS) => {
                        let fsrc = bbctx.load_xmm(ir, src, pc);
                        let dst = bbctx.def_F(dst);
                        ir.xmm_move(fsrc, dst);
                        ir.push(AsmInst::XmmUnOp { kind, dst });
                    }
                    _ => {
                        bbctx.load(ir, src, GP::Rdi);
                        bbctx.generic_unop(ir, kind.generic_func(), pc);
                        bbctx.def_rax2acc(ir, dst);
                        bbctx.unset_class_version_guard();
                    }
                }
            }

            TraceIr::BinOp {
                kind,
                dst,
                mode,
                ic,
            } => {
                let (lhs, rhs) = bbctx.binary_class(mode, ic);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => match (lhs, rhs) {
                        (INTEGER_CLASS, INTEGER_CLASS) => {
                            bbctx.gen_binop_fixnum(ir, kind, dst, mode, pc);
                            return CompileResult::Continue;
                        }
                        (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => {
                            let info = FBinOpInfo {
                                dst,
                                mode,
                                lhs_class: lhs.into(),
                                rhs_class: rhs.into(),
                            };
                            bbctx.gen_binop_float(ir, kind, info, pc);
                            return CompileResult::Continue;
                        }
                        _ => {}
                    },
                    _ => {}
                }
                if let Some(lhs) = lhs {
                    let name = kind.to_id();
                    if let Some(fid) = self.jit_check_method(store, lhs, name) {
                        return self.compile_binop_call(bbctx, ir, store, fid, dst, mode, lhs, pc);
                    } else {
                        return CompileResult::Recompile(RecompileReason::MethodNotFound);
                    }
                }
                return CompileResult::Recompile(RecompileReason::NotCached);
            }

            TraceIr::BinCmp {
                kind,
                dst,
                mode,
                ic,
            } => {
                let (lhs, rhs) = bbctx.binary_class(mode, ic);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => match (lhs, rhs) {
                        (INTEGER_CLASS, INTEGER_CLASS) => {
                            bbctx.gen_cmp_integer(ir, kind, dst, mode, pc);
                            return CompileResult::Continue;
                        }
                        (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => {
                            let info = FBinOpInfo {
                                dst,
                                mode,
                                lhs_class: lhs.into(),
                                rhs_class: rhs.into(),
                            };
                            bbctx.gen_cmp_float(ir, info, kind, pc);
                            return CompileResult::Continue;
                        }
                        _ => {}
                    },
                    _ => {}
                }
                if let Some(lhs) = lhs {
                    let name = Self::cmpkind_to_id(kind);
                    if let Some(fid) = self.jit_check_method(store, lhs, name) {
                        return self.compile_binop_call(bbctx, ir, store, fid, dst, mode, lhs, pc);
                    } else {
                        return CompileResult::Recompile(RecompileReason::MethodNotFound);
                    }
                }
                return CompileResult::Recompile(RecompileReason::NotCached);
            }

            TraceIr::BinCmpBr {
                kind,
                dst,
                mode,
                dest_bb,
                brkind,
                ic,
            } => {
                let (lhs, rhs) = bbctx.binary_class(mode, ic);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => match (lhs, rhs) {
                        (INTEGER_CLASS, INTEGER_CLASS) => {
                            if let Some(result) =
                                bbctx.check_concrete_i64_cmpbr(mode, kind, brkind, dest_bb)
                            {
                                return result;
                            }
                            let src_idx = bc_pos + 1;
                            let dest = self.label();
                            bbctx.gen_cmpbr_integer(ir, kind, mode, brkind, dest, pc);
                            self.new_side_branch(iseq, src_idx, dest_bb, bbctx.clone(), dest);
                            return CompileResult::Continue;
                        }
                        (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => {
                            if let Some(result) =
                                bbctx.check_concrete_f64_cmpbr(mode, kind, brkind, dest_bb)
                            {
                                return result;
                            }
                            let info = FBinOpInfo {
                                dst,
                                mode,
                                lhs_class: lhs.into(),
                                rhs_class: rhs.into(),
                            };
                            let src_idx = bc_pos + 1;
                            let dest = self.label();
                            let mode = bbctx.fmode(ir, info, pc);
                            bbctx.discard(dst);
                            ir.float_cmp_br(mode, kind, brkind, dest);
                            self.new_side_branch(iseq, src_idx, dest_bb, bbctx.clone(), dest);
                            return CompileResult::Continue;
                        }
                        _ => {}
                    },
                    _ => {}
                }
                if let Some(lhs_class) = lhs {
                    let name = Self::cmpkind_to_id(kind);
                    if let Some(fid) = self.jit_check_method(store, lhs_class, name) {
                        match self
                            .compile_binop_call(bbctx, ir, store, fid, dst, mode, lhs_class, pc)
                        {
                            CompileResult::Continue => {
                                let src_idx = bc_pos + 1;
                                bbctx.unset_class_version_guard();
                                self.gen_cond_br(bbctx, ir, iseq, src_idx, dest_bb, brkind);
                            }
                            res => return res,
                        }
                        return CompileResult::Continue;
                    } else {
                        return CompileResult::Recompile(RecompileReason::MethodNotFound);
                    }
                }
                return CompileResult::Recompile(RecompileReason::NotCached);
            }
            TraceIr::ArrayTEq { lhs, rhs } => {
                bbctx.write_back_slot(ir, lhs);
                bbctx.write_back_slot(ir, rhs);
                bbctx.discard(lhs);
                let error = ir.new_error(bbctx, pc);
                ir.array_teq(bbctx, lhs, rhs);
                ir.handle_error(error);
                bbctx.def_rax2acc(ir, lhs);
            }

            TraceIr::Index {
                dst,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
                        bbctx.array_integer_index(ir, store, dst, base, idx, pc);
                        return CompileResult::Continue;
                    } else if let Some(fid) =
                        self.jit_check_method(store, base_class, IdentId::_INDEX)
                    {
                        let mode = if let Some(idx) = bbctx.is_fixnum_literal(idx)
                            && let Some(idx) = i16::try_from(idx).ok()
                        {
                            OpMode::RI(base, idx)
                        } else {
                            OpMode::RR(base, idx)
                        };
                        return self.compile_binop_call(
                            bbctx,
                            ir,
                            store,
                            fid,
                            Some(dst),
                            mode,
                            base_class,
                            pc,
                        );
                    }
                }
                return CompileResult::Recompile(RecompileReason::NotCached);
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                class,
            } => {
                if let Some((base_class, idx_class)) = class {
                    if store[base_class].is_array_ty_instance() && idx_class == INTEGER_CLASS {
                        bbctx.array_integer_index_assign(ir, store, src, base, idx, pc);
                    } else {
                        bbctx.write_back_slots(ir, &[base, idx, src]);
                        ir.generic_index_assign(bbctx, base, idx, src, pc);
                        bbctx.unset_class_version_guard();
                    }
                } else {
                    return CompileResult::Recompile(RecompileReason::NotCached);
                }
            }
            TraceIr::ToA { dst, src } => {
                let error = ir.new_error(bbctx, pc);
                bbctx.write_back_slot(ir, src);
                ir.to_a(bbctx, src);
                ir.handle_error(error);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::Mov(dst, src) => {
                bbctx.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                bbctx.write_back_range(ir, arg, len);
                bbctx.discard(dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_regexp(bbctx, arg, len);
                ir.handle_error(error);
                bbctx.def_rax2acc(ir, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len, rest_pos),
            } => {
                bbctx.load(ir, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    bbctx.def_S(SlotId(reg));
                }
                ir.expand_array(bbctx, dst, len, rest_pos);
            }
            TraceIr::UndefMethod { undef } => {
                ir.undef_method(bbctx, undef, pc);
                bbctx.unset_class_version_guard();
            }
            TraceIr::AliasMethod { new, old } => {
                ir.alias_method(bbctx, new, old, pc);
                bbctx.unset_class_version_guard();
            }
            TraceIr::MethodCall {
                _polymorphic: _,
                callid,
                cache,
            } => {
                let callsite = &store[callid];
                let recv_class = bbctx.class(callsite.recv);
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
                            if let Some(fid) = self.jit_check_call(store, recv_class, callsite.name)
                            {
                                fid
                            } else {
                                return CompileResult::Recompile(RecompileReason::MethodNotFound);
                            },
                        )
                    } else {
                        // the inline method cache is valid.
                        (cache.recv_class, cache.func_id)
                    }
                } else if let Some(recv_class) = recv_class {
                    // no inline cache, but receiver class is known.
                    if let Some(func_id) = self.jit_check_call(store, recv_class, callsite.name) {
                        let cache = MethodCacheEntry {
                            recv_class,
                            func_id,
                            version: self.class_version(),
                        };
                        pc.write_method_cache(&cache);
                        (recv_class, func_id)
                    } else {
                        return CompileResult::Recompile(RecompileReason::MethodNotFound);
                    }
                } else {
                    return CompileResult::Recompile(RecompileReason::NotCached);
                };
                self.inline_method_cache
                    .push((recv_class, callsite.name, func_id));
                return self.compile_method_call(bbctx, ir, store, pc, recv_class, func_id, callid);
            }
            TraceIr::Yield { callid } => {
                if let Some(block_info) = self.current_method_given_block()
                    && let Some(iseq) = store[block_info.block_fid].is_iseq()
                {
                    if !self.compile_yield_specialized(
                        bbctx,
                        ir,
                        store,
                        callid,
                        &block_info,
                        iseq,
                        pc,
                    ) {
                        return CompileResult::Cease;
                    };
                } else {
                    bbctx.compile_yield(ir, store, callid, pc);
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
                ir.check_bop(bbctx, pc);
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
                ir.check_bop(bbctx, pc);
                bbctx.unset_class_version_guard();
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, base, superclass, name, func_id, false, pc);
                bbctx.unset_class_version_guard();
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, base, None, name, func_id, true, pc);
                bbctx.unset_class_version_guard();
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                bbctx.singleton_class_def(ir, dst, base, func_id, pc);
                bbctx.unset_class_version_guard();
            }

            TraceIr::DefinedYield { dst } => {
                bbctx.def_S(dst);
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
                bbctx.def_S(dst);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedSuper { dst } => {
                bbctx.def_S(dst);
                let using_xmm = bbctx.get_using_xmm();
                ir.push(AsmInst::DefinedSuper { dst, using_xmm });
            }
            TraceIr::DefinedGvar { dst, name } => {
                bbctx.def_S(dst);
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
                bbctx.write_back_locals_if_captured(ir);
                bbctx.load(ir, ret, GP::Rax);
                ir.push(AsmInst::Ret);
                return CompileResult::Return(bbctx.mode(ret).as_result());
            }
            TraceIr::MethodRet(ret) => {
                bbctx.write_back_locals_if_captured(ir);
                bbctx.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.method_caller_stack_offset(store) {
                    ir.push(AsmInst::MethodRetSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::MethodRet(pc));
                }
                return CompileResult::MethodReturn(bbctx.mode(ret).as_result());
            }
            TraceIr::BlockBreak(ret) => {
                bbctx.write_back_locals_if_captured(ir);
                bbctx.load(ir, ret, GP::Rax);
                if let Some(rbp_offset) = self.iter_caller_stack_offset(store) {
                    ir.push(AsmInst::BlockBreakSpecialized { rbp_offset });
                } else {
                    ir.push(AsmInst::BlockBreak);
                }
                return CompileResult::Break(bbctx.mode(ret).as_result());
            }
            TraceIr::Raise(ret) => {
                bbctx.locals_to_S(ir);
                bbctx.load(ir, ret, GP::Rax);
                ir.push(AsmInst::Raise);
                return CompileResult::Leave;
            }

            TraceIr::EnsureEnd => {
                bbctx.locals_to_S(ir);
                ir.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(dest_bb) => {
                return CompileResult::Branch(dest_bb);
            }
            TraceIr::CondBr(cond_, dest_bb, false, brkind) => {
                if bbctx.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        return CompileResult::Branch(dest_bb);
                    }
                } else if bbctx.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        return CompileResult::Branch(dest_bb);
                    }
                } else {
                    bbctx.load(ir, cond_, GP::Rax);
                    self.gen_cond_br(bbctx, ir, iseq, bc_pos, dest_bb, brkind);
                }
            }
            TraceIr::NilBr(cond_, dest_bb) => {
                if bbctx.is_nil(cond_) {
                    return CompileResult::Branch(dest_bb);
                } else if bbctx.is_not_nil(cond_) {
                } else {
                    let branch_dest = self.label();
                    bbctx.load(ir, cond_, GP::Rax);
                    ir.push(AsmInst::NilBr(branch_dest));
                    self.new_side_branch(iseq, bc_pos, dest_bb, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_bb) => match bbctx.mode(local) {
                LinkMode::S(_) | LinkMode::C(_) => {
                    return CompileResult::Branch(dest_bb);
                }
                LinkMode::MaybeNone => {
                    let branch_dest = self.label();
                    bbctx.load(ir, local, GP::Rax);
                    ir.push(AsmInst::CheckLocal(branch_dest));
                    let mut side_bb = bbctx.clone();
                    side_bb.set_S(local);
                    self.new_side_branch(iseq, bc_pos, dest_bb, side_bb, branch_dest);
                    bbctx.set_None(local);
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
                bbctx.load_fixnum(ir, cond, GP::Rdi, pc);
                let mut branch_labels = vec![];
                for bbid in branch_table {
                    let branch_dest = self.label();
                    branch_labels.push(branch_dest);
                    self.new_side_branch(iseq, bc_pos, bbid, bbctx.clone(), branch_dest);
                }
                let else_label = self.label();
                self.new_side_branch(iseq, bc_pos, else_dest, bbctx.clone(), else_label);
                ir.opt_case(max, min, else_label, branch_labels.into());
                return CompileResult::Cease;
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

    fn recompile_and_deopt(
        &self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        reason: RecompileReason,
        pc: BytecodePtr,
    ) {
        let deopt = ir.new_deopt(bbctx, pc);
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

impl JitContext {
    ///
    /// Check whether a method or `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_call(
        &mut self,
        store: &Store,
        recv_class: ClassId,
        name: Option<IdentId>,
    ) -> Option<FuncId> {
        if let Some(name) = name {
            // for method call
            self.jit_check_method(store, recv_class, name)
        } else {
            // for super
            self.jit_check_super(store, recv_class)
        }
    }

    ///
    /// Check whether a method *name* of class *class_id* exists in compile time.
    ///
    fn jit_check_method(&self, store: &Store, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        let class_version = self.class_version();
        store
            .check_method_for_class_with_version(class_id, name, class_version)?
            .func_id()
    }

    ///
    /// Check whether `super` of class *class_id* exists in compile time.
    ///
    fn jit_check_super(&mut self, store: &Store, recv_class: ClassId) -> Option<FuncId> {
        // for super
        let iseq_id = self.iseq_id();
        let mother = store[iseq_id].mother().0;
        let owner = store[mother].owner_class().unwrap();
        let func_name = store[mother].name().unwrap();
        store.check_super(recv_class, owner, func_name)
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

impl BBContext {
    fn binary_class(
        &self,
        mode: OpMode,
        ic: Option<(ClassId, ClassId)>,
    ) -> (Option<ClassId>, Option<ClassId>) {
        let (lhs_class, rhs_class) = match mode {
            OpMode::RR(l, r) => (self.class(l), self.class(r)),
            OpMode::RI(l, _) => (self.class(l), Some(INTEGER_CLASS)),
            OpMode::IR(_, r) => (Some(INTEGER_CLASS), self.class(r)),
        };
        let lhs_class = if lhs_class.is_some() {
            lhs_class
        } else {
            ic.map(|(class, _)| class)
        };
        let rhs_class = if rhs_class.is_some() {
            rhs_class
        } else {
            ic.map(|(_, class)| class)
        };
        (lhs_class, rhs_class)
    }
}
