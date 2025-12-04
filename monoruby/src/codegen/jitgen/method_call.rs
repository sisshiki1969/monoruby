use crate::{
    codegen::jitgen::{context::JitStackFrame, slot::LinkMode},
    executor::inline::InlineFuncInfo,
};

use super::{
    context::{JitArgumentInfo, JitBlockInfo},
    *,
};

impl JitContext {
    ///
    /// Compile TraceIr::MethodCall with inline method cache info.
    ///
    pub(super) fn compile_method_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        bc_pos: BcIndex,
        cache: MethodCacheEntry,
        callid: CallSiteId,
    ) -> CompileResult {
        let callsite = &store[callid];
        let recv = callsite.recv;
        let MethodCacheEntry {
            mut recv_class,
            mut func_id,
            version,
        } = cache;
        let pc = store[self.iseq_id()].get_pc(bc_pos);
        if (version != self.class_version()) || (recv.is_self() && self.self_class() != recv_class)
        {
            // the inline method cache is invalid because the receiver class is not matched.
            if recv.is_self() {
                recv_class = self.self_class()
            };
            if let Some(fid) = self.jit_check_call(store, recv_class, callsite.name) {
                pc.write_method_cache(recv_class, fid, self.class_version());
                func_id = fid;
            } else {
                return CompileResult::Recompile(RecompileReason::MethodNotFound);
            }
        }

        // We must write back and unlink all local vars when they are possibly accessed or captured from inner blocks.
        let possibly_captured = callsite.block_fid.is_some() || store[func_id].meta().is_eval();
        if possibly_captured {
            bbctx.locals_to_S(ir);
        }

        self.recv_version_guard(bbctx, ir, recv, recv_class, pc);

        self.inline_method_cache.insert(pc, cache);

        if callsite.block_fid.is_none()
            && let Some(info) = store.inline_info.get_inline(func_id)
        {
            match info {
                InlineFuncInfo::InlineGen(f) => {
                    if self.inline_asm(bbctx, ir, store, f, callsite, recv_class, pc) {
                        if possibly_captured {
                            bbctx.unset_frame_capture_guard();
                        }
                        return CompileResult::Continue;
                    }
                }
                InlineFuncInfo::CFunc_F_F(f) => {
                    let CallSiteInfo { args, dst, .. } = *callsite;
                    let src = bbctx.load_xmm(ir, args, pc);
                    if let Some(dst) = dst {
                        let dst = bbctx.def_F(dst);
                        let using_xmm = bbctx.get_using_xmm();
                        ir.push(AsmInst::CFunc {
                            f: *f,
                            src,
                            dst,
                            using_xmm,
                        });
                    }
                    return CompileResult::Continue;
                }
            }
        }

        let res = self.call(bbctx, ir, store, callid, func_id, recv_class, pc);
        if possibly_captured {
            bbctx.unset_frame_capture_guard();
        }
        res
    }

    pub(super) fn compile_binop_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        fid: FuncId,
        dst: Option<SlotId>,
        mode: OpMode,
        lhs_class: ClassId,
        pc: BytecodePtr,
    ) -> CompileResult {
        assert!(matches!(
            store[fid].kind,
            FuncKind::Builtin { .. } | FuncKind::ISeq(_)
        ));
        let callee = &store[fid];
        if (!callee.is_rest() && callee.max_positional_args() < 1) || callee.req_num() > 1 {
            return CompileResult::Recompile(RecompileReason::MaybeError);
        }

        // class version guard
        let deopt = ir.new_deopt(bbctx, pc);
        self.guard_class_version(bbctx, ir, false, deopt);

        // receiver class guard
        bbctx.load_lhs(ir, mode, GP::Rdi);
        match mode {
            OpMode::RR(lhs, _) | OpMode::RI(lhs, _) => {
                bbctx.guard_class(ir, lhs, GP::Rdi, lhs_class, deopt);
            }
            OpMode::IR(_, _) => {
                assert!(lhs_class == INTEGER_CLASS);
            }
        }

        ir.reg_move(GP::Rdi, GP::R13);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);

        bbctx.set_binop_arguments(store, ir, fid, mode);

        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx, pc);
        bbctx.writeback_acc(ir);
        let evict = ir.new_evict();
        ir.push(AsmInst::SetupBinopFrame {
            meta: callee.meta(),
        });
        ir.push(AsmInst::Call {
            callee_fid: fid,
            recv_class: lhs_class,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.def_rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict, pc);
        CompileResult::Continue
    }

    fn recv_version_guard(
        &self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        recv: SlotId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) {
        // class version guard
        let deopt = ir.new_deopt(bbctx, pc);
        self.guard_class_version(bbctx, ir, true, deopt);

        // receiver class guard
        bbctx.load(ir, recv, GP::Rdi);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_self() && !bbctx.is_class(recv, recv_class) {
            bbctx.guard_class(ir, recv, GP::Rdi, recv_class, deopt);
        }
    }

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    fn guard_class_version(
        &self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        with_recovery: bool,
        deopt: AsmDeopt,
    ) {
        if bbctx.class_version_guarded {
            return;
        }
        match self.jit_type() {
            JitType::Specialized { idx, .. } => {
                ir.push(AsmInst::GuardClassVersionSpecialized { idx: *idx, deopt });
            }
            _ => {
                ir.push(AsmInst::GuardClassVersion {
                    position: self.position(),
                    with_recovery,
                    deopt,
                });
            }
        }
        bbctx.set_class_version_guard();
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn compile_yield_specialized(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        block: &JitBlockInfo,
        iseq: ISeqId,
        pc: BytecodePtr,
    ) {
        let dst = store[callid].dst;
        bbctx.exec_gc(ir, true, pc);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        let JitBlockInfo {
            block_fid: callee_fid,
            self_class,
            outer,
        } = block.add(1);
        let simple = bbctx.set_arguments(store, ir, callid, callee_fid, pc);
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx, pc);
        bbctx.writeback_acc(ir);
        let args_info = if simple {
            JitArgumentInfo::new(slot::LinkMode::from_caller_yield(
                store, callee_fid, callid, bbctx, self_class,
            ))
        } else {
            JitArgumentInfo::default()
        };
        let entry = self.compile_inlined_func(
            store,
            iseq,
            self_class,
            None,
            args_info,
            None,
            Some(outer),
            callid,
        );
        let evict = ir.new_evict();
        let meta = store[callee_fid].meta();
        ir.push(AsmInst::SetupYieldFrame { meta, outer });
        ir.handle_hash_splat_kwrest(store, callid, callee_fid, error);
        ir.push(AsmInst::SpecializedYield { entry, evict });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.def_rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict, pc);
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rax: return value: Value
    ///
    fn call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        mut fid: FuncId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> CompileResult {
        let callsite = &store[callid];
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            ..
        } = *callsite;
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        let evict = match store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                bbctx.discard(dst);
                bbctx.writeback_acc(ir);
                if recv_class.is_always_frozen() {
                    if dst.is_some() {
                        ir.lit2reg(Value::nil(), GP::Rax);
                    }
                } else {
                    let ivarid = if let Some(id) = store[recv_class].get_ivarid(ivar_name) {
                        id
                    } else {
                        return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
                    };
                    let is_object_ty = store[recv_class].is_object_ty_instance();
                    if is_object_ty && ivarid.is_inline() {
                        ir.push(AsmInst::LoadIVarInline { ivarid })
                    } else {
                        ir.push(AsmInst::LoadIVarHeap {
                            ivarid,
                            is_object_ty,
                            self_: false,
                        });
                    }
                }
                bbctx.def_reg2acc(ir, GP::R15, dst);
                return CompileResult::Continue;
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(block_fid.is_none());
                /*if callsite.block_arg.is_some() {
                    dbg!(ivar_name);
                    dbg!(callsite);
                    return CompileResult::Abort;
                }*/
                let ivarid = if let Some(id) = store[recv_class].get_ivarid(ivar_name) {
                    id
                } else {
                    return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
                };
                let src = bbctx.load_or_reg(ir, args, GP::Rax);
                let is_object_ty = store[recv_class].is_object_ty_instance();
                let using_xmm = bbctx.get_using_xmm();
                if is_object_ty && ivarid.is_inline() {
                    ir.push(AsmInst::StoreIVarInline { src, ivarid })
                } else {
                    ir.push(AsmInst::StoreIVarHeap {
                        src,
                        ivarid,
                        using_xmm,
                        is_object_ty,
                    });
                }
                bbctx.def_rax2acc(ir, dst);
                return CompileResult::Continue;
            }
            FuncKind::Builtin { .. } => {
                let evict = ir.new_evict();
                bbctx.send(ir, store, callid, fid, recv_class, evict, None, pc);
                evict
            }
            FuncKind::Proc(proc) => {
                let evict = ir.new_evict();
                fid = proc.func_id();
                bbctx.send(
                    ir,
                    store,
                    callid,
                    fid,
                    recv_class,
                    evict,
                    Some(proc.outer_lfp()),
                    pc,
                );
                evict
            }
            FuncKind::ISeq(iseq) => {
                if let Some(v) = store[iseq].is_const_fn() {
                    bbctx.def_C(dst, v);
                    return CompileResult::Continue;
                }
                let evict = ir.new_evict();
                let specializable = store.is_simple_call(fid, callid)
                    && (bbctx.is_C(callsite.recv)
                        || (pos_num != 0 && (args..args + pos_num).any(|i| bbctx.is_C(i))));
                let iseq_block = block_fid.map(|fid| store[fid].is_iseq()).flatten();

                if iseq_block.is_some() || (specializable && self.specialize_level() < 3)
                /*name == Some(IdentId::NEW)*/
                {
                    let args_info = if specializable {
                        JitArgumentInfo::new(LinkMode::from_caller(store, fid, callid, bbctx))
                    } else {
                        JitArgumentInfo::default()
                    };
                    let block = block_fid.map(|fid| JitBlockInfo::new(fid, self.self_class()));
                    let patch_point = if self.is_specialized() {
                        None
                    } else {
                        Some(self.label())
                    };
                    let entry = self.compile_inlined_func(
                        store,
                        iseq,
                        recv_class,
                        patch_point,
                        args_info,
                        block,
                        None,
                        callid,
                    );
                    bbctx.send_specialized(ir, store, callid, fid, entry, patch_point, evict, pc);
                } else {
                    bbctx.send(ir, store, callid, fid, recv_class, evict, None, pc);
                }
                evict
            }
        };
        bbctx.def_rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict, pc);
        bbctx.unset_class_version_guard();
        CompileResult::Continue
    }

    fn compile_inlined_func(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        self_class: ClassId,
        patch_point: Option<JitLabel>,
        args_info: JitArgumentInfo,
        block: Option<JitBlockInfo>,
        outer: Option<usize>,
        callid: CallSiteId,
    ) -> JitLabel {
        let idx = match self.jit_type() {
            JitType::Specialized { idx, .. } => *idx,
            _ => self.specialized_methods_len(),
        };
        let jit_type = JitType::Specialized { idx, args_info };
        let specialize_level = self.specialize_level() + 1;
        self.stack_frame.push(JitStackFrame::new(
            store,
            jit_type,
            specialize_level,
            iseq_id,
            outer,
            block,
            Some(callid),
            self_class,
        ));
        self.traceir_to_asmir(store);
        let frame = self.stack_frame.pop().unwrap();
        //dbg!(&frame.return_context);
        let entry = self.label();
        self.specialized_methods_push(context::SpecializeInfo {
            entry,
            frame,
            patch_point,
        });
        entry
    }

    fn inline_asm(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        f: impl Fn(
            &mut BBContext,
            &mut AsmIr,
            &JitContext,
            &Store,
            &CallSiteInfo,
            ClassId,
            BytecodePtr,
        ) -> bool,
        callsite: &CallSiteInfo,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> bool {
        let bbctx_save = bbctx.clone();
        let ir_save = ir.save();
        if f(bbctx, ir, self, store, callsite, recv_class, pc) {
            true
        } else {
            *bbctx = bbctx_save;
            ir.restore(ir_save);
            false
        }
    }
}

impl BBContext {
    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
        outer_lfp: Option<Lfp>,
        pc: BytecodePtr,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        self.exec_gc(ir, true, pc);
        let using_xmm = self.get_using_xmm();
        ir.xmm_save(using_xmm);
        self.set_arguments(store, ir, callid, callee_fid, pc);
        if let Some(dst) = store[callid].dst {
            self.discard(dst);
        }
        self.clear_above_next_sp();
        let error = ir.new_error(self, pc);
        self.writeback_acc(ir);
        let meta = store[callee_fid].meta();
        ir.push(AsmInst::SetupMethodFrame {
            meta,
            callid,
            outer_lfp,
        });
        ir.handle_hash_splat_kwrest(store, callid, callee_fid, error);
        ir.push(AsmInst::Call {
            callee_fid,
            recv_class,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send_specialized(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        inlined_entry: JitLabel,
        patch_point: Option<JitLabel>,
        evict: AsmEvict,
        pc: BytecodePtr,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        self.exec_gc(ir, true, pc);
        let using_xmm = self.get_using_xmm();
        ir.xmm_save(using_xmm);
        self.set_arguments(store, ir, callid, callee_fid, pc);
        if let Some(dst) = store[callid].dst {
            self.discard(dst);
        }
        self.clear_above_next_sp();
        let error = ir.new_error(self, pc);
        self.writeback_acc(ir);
        let meta = store[callee_fid].meta();
        ir.push(AsmInst::SetupMethodFrame {
            meta,
            callid,
            outer_lfp: None,
        });
        ir.handle_hash_splat_kwrest(store, callid, callee_fid, error);
        ir.push(AsmInst::SpecializedCall {
            entry: inlined_entry,
            patch_point,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }

    pub(super) fn compile_yield(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        pc: BytecodePtr,
    ) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        self.write_back_callargs_and_dst(ir, &callinfo);
        self.writeback_acc(ir);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let evict = ir.new_evict();
        self.exec_gc(ir, true, pc);
        ir.xmm_save(using_xmm);
        ir.push(AsmInst::Yield {
            callid,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        self.def_rax2acc(ir, dst);
        self.immediate_evict(ir, evict, pc);
        self.unset_class_version_guard();
    }

    fn immediate_evict(&self, ir: &mut AsmIr, evict: AsmEvict, pc: BytecodePtr) {
        ir.push(AsmInst::ImmediateEvict { evict });
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
    }

    #[allow(non_snake_case)]
    fn callsite_exists_F(&self, store: &Store, callid: CallSiteId) -> bool {
        let callsite = &store[callid];
        let args = callsite.args;
        let pos_num = callsite.pos_num;
        let kw_pos = callsite.kw_pos;
        let kw_num = callsite.kw_len();
        (args..args + pos_num).any(|reg| matches!(self.mode(reg), LinkMode::F(_)))
            || (kw_pos..kw_pos + kw_num).any(|reg| matches!(self.mode(reg), LinkMode::F(_)))
    }

    ///
    /// Set positional arguments for callee.
    ///
    fn set_arguments(
        &mut self,
        store: &Store,
        ir: &mut AsmIr,
        callid: CallSiteId,
        callee_fid: FuncId,
        pc: BytecodePtr,
    ) -> bool {
        let callee = &store[callee_fid];
        let callsite = &store[callid];
        if store.is_simple_call(callee_fid, callid) {
            let stack_offset = if self.callsite_exists_F(store, callid) {
                callee.get_offset() as i32
            } else {
                0
            };
            ir.reg_sub(GP::Rsp, stack_offset);

            let args = callsite.args;
            let pos_num = callsite.pos_num;
            let kw_pos = callsite.kw_pos;
            let kw_num = callsite.kw_len();
            // write back block argument.
            if let Some(block_arg) = callsite.block_arg {
                self.write_back_slot(ir, block_arg);
            }

            // fetch positional arguments.
            let (filled_req, filled_opt, filled_post) = callee.apply_args(pos_num);

            // fill required params.
            for i in 0..filled_req {
                let reg = args + i;
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, ofs);
            }
            if filled_req != callee.req_num() {
                for i in filled_req..callee.req_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                }
            }

            // fill optional params.
            for i in callee.req_num()..callee.req_num() + filled_opt {
                let reg = args + filled_req + (i - callee.req_num());
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, ofs);
            }
            if filled_opt != callee.opt_num() {
                for i in callee.req_num() + filled_opt..callee.reqopt_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.zero2rsp_offset(ofs);
                }
            }

            // fill post params.
            let start = callee.reqopt_num() + callee.is_rest() as usize;
            for i in start..start + filled_post {
                let reg = args + filled_req + filled_opt + (i - start);
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, ofs);
            }
            if filled_post != callee.post_num() {
                for i in start + filled_post..start + callee.post_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                }
            }

            // fill keyword arguments
            let CallSiteInfo { kw_args, .. } = callsite;
            let mut used_kw = vec![];
            for (i, param_name) in callee.kw_names().iter().enumerate() {
                let ofs = stack_offset - (LFP_SELF + (callee.kw_reg_pos() + i).0 as i32 * 8);
                match kw_args.get(param_name) {
                    Some(i) => {
                        used_kw.push(*i);
                        let slot = kw_pos + *i;
                        self.fetch_for_callee(ir, slot, ofs);
                    }
                    None => {
                        ir.zero2rsp_offset(ofs);
                    }
                }
            }

            // write back unused keyword arguments.
            for i in 0..kw_num {
                if !used_kw.contains(&i) {
                    self.write_back_slot(ir, kw_pos + i);
                }
            }

            ir.reg_add(GP::Rsp, stack_offset);
            true
        } else {
            self.write_back_args(ir, callsite);

            let error = ir.new_error(self, pc);
            ir.push(AsmInst::SetArguments { callid, callee_fid });
            ir.handle_error(error);
            ir.push(AsmInst::CopyKeywordArgs { callid, callee_fid });
            false
        }
    }

    fn set_binop_arguments(
        &mut self,
        store: &Store,
        ir: &mut AsmIr,
        callee_fid: FuncId,
        mode: OpMode,
    ) {
        let callee = &store[callee_fid];
        // callee.req_num() <= 1 at this point.
        // callee.is_rest() || callee.max_positional_args() >= 1 at this point.
        let xmm_flag = match mode {
            OpMode::RR(_, rhs) | OpMode::IR(_, rhs) => {
                matches!(self.mode(rhs), LinkMode::F(_))
            }
            OpMode::RI(_, _) => false,
        };
        let ofs = if xmm_flag || callee.is_rest() {
            (RSP_LOCAL_FRAME + LFP_ARG0 + 16 as i32) & !0xf
        } else {
            0
        };

        ir.reg_sub(GP::Rsp, ofs);
        let offset = ofs - LFP_ARG0;
        self.fetch_rhs_for_callee(ir, mode, offset);
        if 1 < callee.max_positional_args() {
            //ir.push(AsmInst::U32ToReg(0, GP::Rax));
            for i in 1..callee.max_positional_args() {
                let offset = ofs - (LFP_ARG0 as i32 + (8 * i) as i32);
                ir.zero2rsp_offset(offset);
            }
        }
        if callee.is_rest() {
            ir.push(AsmInst::RSPOffsetToArray(offset));
        }
        ir.reg_add(GP::Rsp, ofs);
    }
}
