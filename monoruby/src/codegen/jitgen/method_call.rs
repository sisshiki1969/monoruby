use crate::{
    codegen::jitgen::{context::JitStackFrame, slot::LinkMode},
    executor::inline::InlineFuncInfo,
};

use super::{
    context::{JitArgumentInfo, JitBlockInfo},
    *,
};

impl<'a> JitContext<'a> {
    ///
    /// Compile TraceIr::MethodCall with inline method cache info.
    ///
    pub(super) fn compile_method_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        recv_class: ClassId,
        func_id: FuncId,
        callid: CallSiteId,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        self.inline_method_cache
            .push((recv_class, callsite.name, func_id));
        let recv = callsite.recv;

        // We must write back and unlink all local vars when they are possibly accessed or captured from inner blocks.
        if callsite.block_fid.is_some() || self.store[func_id].possibly_capture_without_block() {
            bbctx.locals_to_S(ir);
        }

        // class version guard
        self.guard_class_version(bbctx, ir, true, pc);

        // receiver class guard
        if bbctx.class(recv) != Some(recv_class) {
            let deopt = ir.new_deopt(bbctx, pc);
            bbctx.load(ir, recv, GP::Rdi);
            bbctx.guard_class(ir, recv, GP::Rdi, recv_class, deopt);
        }

        if callsite.block_fid.is_none()
            && let Some(info) = self.store.inline_info.get_inline(func_id)
        {
            match info {
                InlineFuncInfo::InlineGen(f) => {
                    bbctx.load(ir, recv, GP::Rdi);
                    if self.inline_asm(bbctx, ir, f, callsite, recv_class, pc) {
                        bbctx.unset_side_effect_guard();
                        if self.store[func_id].possibly_capture_without_block() {
                            bbctx.unset_frame_capture_guard(self);
                        }
                        return CompileResult::Continue;
                    }
                }
                InlineFuncInfo::CFunc_F_F(f) => {
                    let CallSiteInfo { args, dst, .. } = *callsite;
                    if let Some(args) = bbctx.coerce_C_f64(args) {
                        let res = f(args);
                        if let Some(dst) = dst {
                            bbctx.def_C_float(dst, res);
                        }
                        return CompileResult::Continue;
                    }
                    if let Some(dst) = dst {
                        let src = bbctx.load_xmm(ir, args, pc);
                        bbctx.discard(dst);
                        let using_xmm = bbctx.get_using_xmm();
                        let dst = bbctx.def_F(dst);
                        ir.push(AsmInst::CFunc_F_F {
                            f: *f,
                            src,
                            dst,
                            using_xmm,
                        });
                    }
                    return CompileResult::Continue;
                }
                InlineFuncInfo::CFunc_FF_F(f) => {
                    let CallSiteInfo {
                        recv, args, dst, ..
                    } = *callsite;
                    if let Some((lhs, rhs)) = bbctx.check_binary_C_f64(recv, args) {
                        let res = f(lhs, rhs);
                        if let Some(dst) = dst {
                            bbctx.def_C_float(dst, res);
                        }
                        return CompileResult::Continue;
                    }
                    if let Some(dst) = dst {
                        let lhs = bbctx.load_xmm(ir, recv, pc);
                        let rhs = bbctx.load_xmm(ir, args, pc);
                        bbctx.discard(dst);
                        let using_xmm = bbctx.get_using_xmm();
                        let dst = bbctx.def_F(dst);
                        ir.push(AsmInst::CFunc_FF_F {
                            f: *f,
                            lhs,
                            rhs,
                            dst,
                            using_xmm,
                        });
                    }
                    return CompileResult::Continue;
                }
            }
        }

        bbctx.load(ir, recv, GP::Rdi);
        self.call(bbctx, ir, callid, func_id, recv_class, pc)
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
        pc: BytecodePtr,
    ) {
        if bbctx.class_version_guarded {
            return;
        }
        let deopt = ir.new_deopt(bbctx, pc);
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
        callid: CallSiteId,
        block: &JitBlockInfo,
        iseq: ISeqId,
        pc: BytecodePtr,
    ) -> CompileResult {
        let dst = self.store[callid].dst;
        bbctx.exec_gc(ir, true, pc);
        let using_xmm = bbctx.get_using_xmm();
        // stack pointer adjustment
        // -using_xmm.offset()
        ir.xmm_save(using_xmm);
        let JitBlockInfo {
            block_fid: callee_fid,
            self_class,
            outer,
        } = block.add(1);
        let simple = self.store.is_simple_call(callee_fid, callid);
        bbctx.set_arguments(&self.store, ir, callid, callee_fid, pc);
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx, pc);
        bbctx.writeback_acc(ir);
        let args_info = if simple {
            JitArgumentInfo::new(slot::LinkMode::from_caller_yield(
                &self.store,
                callee_fid,
                callid,
                bbctx,
                self_class,
            ))
        } else {
            JitArgumentInfo::default()
        };
        let (entry, result) = match self.compile_specialized_func(
            iseq,
            self_class,
            None,
            args_info,
            Some(outer),
            callid,
            bbctx,
        ) {
            SpecializedCompileResult::Const(v) => {
                bbctx.def_C(dst, v);
                ir.xmm_restore(using_xmm);
                return CompileResult::Continue;
            }
            SpecializedCompileResult::Compiled { entry, result } => (entry, result),
        };
        let evict = ir.new_evict();
        let meta = self.store[callee_fid].meta();
        ir.push(AsmInst::SetupYieldFrame { meta, outer });
        ir.handle_hash_splat_kwrest(&self.store, callid, callee_fid, error);
        ir.push(AsmInst::SpecializedYield { entry, evict });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        let res = bbctx.def_rax2acc_result(ir, dst, result);
        bbctx.immediate_evict(ir, evict, pc);
        res
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
        callid: CallSiteId,
        fid: FuncId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            ..
        } = *callsite;
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        let (fid, outer_lfp) = match self.store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                return self.attr_reader(bbctx, ir, callid, recv_class, ivar_name);
            }
            FuncKind::AttrWriter { ivar_name } => {
                return self.attr_writer(bbctx, ir, callid, recv_class, ivar_name);
            }
            FuncKind::Builtin { .. } => (fid, None),
            FuncKind::Proc(proc) => (proc.func_id(), Some(proc.outer_lfp())),
            FuncKind::ISeq(iseq) => {
                if let Some(v) = self.store[iseq].is_const_fn() {
                    bbctx.def_C(dst, v);
                    return CompileResult::Continue;
                }

                let specializable = self.store.is_simple_call(fid, callid)
                    && (bbctx.is_C(callsite.recv)
                        || (pos_num != 0 && (args..args + pos_num).any(|i| bbctx.is_C(i))));
                let iseq_block = block_fid.map(|fid| self.store[fid].is_iseq()).flatten();

                if iseq_block.is_some() || (specializable && self.specialize_level() < 3)
                /*name == Some(IdentId::NEW)*/
                {
                    return self.specialized_iseq(
                        bbctx,
                        ir,
                        callid,
                        recv_class,
                        fid,
                        iseq,
                        specializable,
                        pc,
                    );
                }
                (fid, None)
            }
        };
        let evict = ir.new_evict();
        bbctx.send(
            ir,
            &self.store,
            callid,
            fid,
            recv_class,
            evict,
            outer_lfp,
            pc,
        );
        if self.store[fid].possibly_capture_without_block() || block_fid.is_some() {
            bbctx.unset_frame_capture_guard(self);
        }
        bbctx.def_rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict, pc);

        CompileResult::Continue
    }

    fn attr_reader(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        ivar_name: IdentId,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        let CallSiteInfo {
            pos_num,
            dst,
            block_fid,
            ..
        } = *callsite;
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
            let ivarid = if let Some(id) = self.store[recv_class].get_ivarid(ivar_name) {
                id
            } else {
                return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
            };
            let is_object_ty = self.store[recv_class].is_object_ty_instance();
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
        CompileResult::Continue
    }

    fn attr_writer(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        ivar_name: IdentId,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            ..
        } = *callsite;
        assert_eq!(1, pos_num);
        assert!(!callsite.kw_may_exists());
        assert!(block_fid.is_none());
        let ivarid = if let Some(id) = self.store[recv_class].get_ivarid(ivar_name) {
            id
        } else {
            return CompileResult::Recompile(RecompileReason::IvarIdNotFound);
        };
        let src = bbctx.load_or_reg(ir, args, GP::Rax);
        let is_object_ty = self.store[recv_class].is_object_ty_instance();
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
        bbctx.unset_side_effect_guard();
        CompileResult::Continue
    }

    fn specialized_iseq(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        fid: FuncId,
        iseq: ISeqId,
        specializable: bool,
        pc: BytecodePtr,
    ) -> CompileResult {
        let dst = self.store[callid].dst;
        let args_info = if specializable {
            JitArgumentInfo::new(LinkMode::from_caller(&self.store, fid, callid, bbctx))
        } else {
            JitArgumentInfo::default()
        };
        let patch_point = if self.is_specialized() {
            None
        } else {
            Some(self.label())
        };
        let (entry, result) = match self.compile_specialized_func(
            iseq,
            recv_class,
            patch_point,
            args_info,
            None,
            callid,
            bbctx,
        ) {
            SpecializedCompileResult::Const(v) => {
                bbctx.def_C(dst, v);
                return CompileResult::Continue;
            }
            SpecializedCompileResult::Compiled { entry, result } => (entry, result),
        };
        let evict = ir.new_evict();
        bbctx.send_specialized(ir, &self.store, callid, fid, entry, patch_point, evict, pc);
        let res = bbctx.def_rax2acc_result(ir, dst, result);
        bbctx.immediate_evict(ir, evict, pc);
        return res;
    }
}

pub(super) enum SpecializedCompileResult {
    Const(Value),
    Compiled {
        entry: JitLabel,
        result: Option<ResultState>,
    },
}

impl<'a> JitContext<'a> {
    fn new_specialized_frame(
        &self,
        iseq_id: ISeqId,
        outer: Option<usize>,
        args_info: JitArgumentInfo,
        self_class: ClassId,
    ) -> JitStackFrame {
        let idx = match self.jit_type() {
            JitType::Specialized { idx, .. } => *idx,
            _ => self.specialized_methods_len(),
        };
        let jit_type = JitType::Specialized { idx, args_info };
        let specialize_level = self.specialize_level() + 1;
        JitStackFrame::new(
            &self.store,
            jit_type,
            specialize_level,
            iseq_id,
            outer,
            self_class,
        )
    }

    fn compile_specialized_func(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        patch_point: Option<JitLabel>,
        args_info: JitArgumentInfo,
        outer: Option<usize>,
        callid: CallSiteId,
        bbctx: &mut BBContext,
    ) -> SpecializedCompileResult {
        let using_xmm = bbctx.get_using_xmm();
        self.xmm_save(using_xmm);
        self.set_callsite(callid);
        self.set_not_captured(bbctx.frame_capture_guarded);
        let frame = self.new_specialized_frame(iseq_id, outer, args_info, self_class);
        self.stack_frame.push(frame);
        self.traceir_to_asmir();
        let mut frame = self.stack_frame.pop().unwrap();
        bbctx.frame_capture_guarded &= self.not_captured();
        self.unset_callsite();
        self.xmm_restore(using_xmm);
        let pos = self.stack_frame.len() - 1;
        let mut return_context = frame.detach_return_context();
        let result = return_context.remove(&pos);
        self.merge_return_context(return_context);
        if let Some(result) = &result {
            if let Some(v) = result.const_folded() {
                #[cfg(feature = "jit-log")]
                if self.codegen_mode() {
                    eprintln!(
                        "const folded: {} {:?}",
                        self.store.func_description(self.store[iseq_id].func_id()),
                        result
                    );
                }
                return SpecializedCompileResult::Const(v);
            }
        }
        #[cfg(feature = "jit-log")]
        if self.codegen_mode() {
            eprintln!(
                "return: {} {:?}",
                self.store.func_description(self.store[iseq_id].func_id()),
                result
            );
        }
        let entry = self.label();
        self.specialized_methods_push(context::SpecializeInfo {
            entry,
            frame,
            patch_point,
        });
        SpecializedCompileResult::Compiled { entry, result }
    }

    fn inline_asm(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
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
        if f(bbctx, ir, self, &self.store, callsite, recv_class, pc) {
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
    /// ### guards
    /// - unset side_effect_guard
    /// - unset class_version_guard
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
        // stack pointer adjustment
        // -using_xmm.offset()
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
        self.unset_class_version_guard();
        self.unset_side_effect_guard();
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
        // stack pointer adjustment
        // -using_xmm.offset()
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
        self.unset_side_effect_guard();
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
        // stack pointer adjustment
        // -using_xmm.offset()
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
        self.unset_side_effect_guard();
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
    /// Set positional and keyword arguments for callee.
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
}
