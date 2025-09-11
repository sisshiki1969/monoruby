use crate::codegen::jitgen::context::JitStackFrame;

use super::{
    context::{JitArgumentInfo, JitBlockInfo},
    slot::SlotState,
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
        callsite: &CallSiteInfo,
    ) -> CompileResult {
        let recv = callsite.recv;
        let MethodCacheEntry {
            mut recv_class,
            mut func_id,
            version,
        } = cache;
        if (version != self.class_version()) || (recv.is_self() && self.self_class() != recv_class)
        {
            // the inline method cache is invalid because the receiver class is not matched.
            if recv.is_self() {
                recv_class = self.self_class()
            };
            if let Some(fid) = self.jit_check_call(store, recv_class, callsite.name) {
                store[self.iseq_id()].get_pc(bc_pos).write_method_cache(
                    recv_class,
                    fid,
                    self.class_version(),
                );
                func_id = fid;
            } else {
                return CompileResult::Recompile(RecompileReason::MethodNotFound);
            }
        }

        // We must write back and unlink all local vars when they are possibly accessed from inner blocks.
        if callsite.block_fid.is_some() || store[func_id].meta().is_eval() {
            bbctx.write_back_locals(ir);
        }

        self.recv_version_guard(bbctx, ir, recv, recv_class);

        self.inline_method_cache.insert(bc_pos, cache);

        if callsite.block_fid.is_none()
            && let Some(info) = store.inline_info.get_inline(func_id)
        {
            let f = &info.inline_gen;
            if self.inline_asm(bbctx, ir, store, f, callsite, recv_class) {
                return CompileResult::Continue;
            }
        }

        self.call(bbctx, ir, store, callsite, func_id, recv_class)
    }

    /*pub(super) fn compile_index_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        func_id: FuncId,
        recv_class: ClassId,
        base: SlotId,
        idx: SlotId,
        dst: SlotId,
    ) -> CompileResult {
        let callsite = CallSiteInfo {
            id: CallSiteId(0),
            name: Some(IdentId::get_id("[]")),
            recv: base,
            args: idx,
            pos_num: 1,
            dst: Some(dst),
            block_fid: None,
            block_arg: None,
            splat_pos: vec![],
            hash_splat_pos: vec![],
            kw_pos: idx,
            kw_args: Default::default(),
        };

        let recv = callsite.recv;
        self.recv_version_guard(bbctx, ir, recv, recv_class);

        if let Some(info) = store.inline_info.get_inline(func_id) {
            let f = &info.inline_gen;
            if self.inline_asm(bbctx, ir, store, f, &callsite, recv_class) {
                return CompileResult::Continue;
            }
        }

        self.call(bbctx, ir, store, &callsite, func_id, recv_class)
    }*/

    pub(super) fn compile_binop_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        fid: FuncId,
        info: BinOpInfo,
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
        let deopt = ir.new_deopt(bbctx);
        self.guard_class_version(bbctx, ir, false, deopt);

        // receiver class guard
        let BinOpInfo {
            dst,
            mode,
            lhs_class,
            ..
        } = info;
        bbctx.fetch_lhs(ir, mode);
        bbctx.guard_lhs_class_for_mode(ir, mode, lhs_class, deopt);

        ir.reg_move(GP::Rdi, GP::R13);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);

        bbctx.set_binop_arguments(store, ir, fid, mode);

        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx);
        bbctx.writeback_acc(ir);
        let evict = ir.new_evict();
        ir.push(AsmInst::BinopCached {
            callee_fid: fid,
            recv_class: lhs_class,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict);
        CompileResult::Continue
    }

    fn recv_version_guard(
        &self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        recv: SlotId,
        recv_class: ClassId,
    ) {
        // class version guard
        let deopt = ir.new_deopt(bbctx);
        self.guard_class_version(bbctx, ir, true, deopt);

        // receiver class guard
        bbctx.fetch(ir, recv, GP::Rdi);
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
        block: JitBlockInfo,
    ) {
        let dst = store[callid].dst;
        bbctx.exec_gc(ir, true);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        let JitBlockInfo {
            iseq,
            self_class,
            outer,
        } = block.add(1);
        bbctx.set_arguments(store, ir, &store[callid], store[iseq].func_id());
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx);
        bbctx.writeback_acc(ir);
        let stack_len = self.stack_frame.len();
        let block = if let Some(info) =
            &self.stack_frame[stack_len.checked_sub(outer).unwrap()].given_block()
        {
            Some(info.add(outer))
        } else {
            None
        };
        let entry = self.compile_specialized_func(
            store,
            iseq,
            self_class,
            None,
            JitArgumentInfo::new(),
            block,
            Some(outer),
        );
        let evict = ir.new_evict();
        ir.push(AsmInst::YieldSpecialized {
            callid,
            iseq,
            outer,
            entry,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict);
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
        callsite: &CallSiteInfo,
        mut fid: FuncId,
        recv_class: ClassId,
    ) -> CompileResult {
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
                bbctx.reg2acc(ir, GP::R15, dst);
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
                let src = bbctx.fetch_or_reg(ir, args, GP::Rax);
                let is_object_ty = store[recv_class].is_object_ty_instance();
                let using_xmm = bbctx.get_using_xmm();
                if is_object_ty && ivarid.is_inline() {
                    ir.push(AsmInst::StoreIVarInline { src, ivarid })
                } else {
                    ir.push(AsmInst::StoreIVarHeap {
                        src,
                        ivarid,
                        using_xmm,
                        self_: false,
                        is_object_ty,
                    });
                }
                bbctx.rax2acc(ir, dst);
                return CompileResult::Continue;
            }
            FuncKind::Builtin { .. } => {
                let evict = ir.new_evict();
                self.send(bbctx, ir, store, callsite, fid, recv_class, evict, None);
                evict
            }
            FuncKind::Proc(proc) => {
                let evict = ir.new_evict();
                fid = proc.func_id();
                self.send(
                    bbctx,
                    ir,
                    store,
                    callsite,
                    fid,
                    recv_class,
                    evict,
                    Some(proc.outer_lfp()),
                );
                evict
            }
            FuncKind::ISeq(iseq) => {
                if let Some(v) = store[iseq].is_const_fn() {
                    bbctx.discard(dst);
                    bbctx.def_concrete_value(dst, v);
                    return CompileResult::Continue;
                }
                let evict = ir.new_evict();
                let specializable = callsite.splat_pos.is_empty()
                    && !store[fid].is_rest()
                    && !(pos_num == 1 && store[fid].single_arg_expand())
                    && (bbctx.state(callsite.recv).is_concrete_value()
                        || (args..args + pos_num).any(|i| bbctx.state(i).is_concrete_value()));
                if (if let Some(fid) = block_fid {
                    store[fid].is_iseq().is_some()
                } else {
                    false
                }) || (specializable && self.specialize_level() < 5)
                /*name == Some(IdentId::NEW)*/
                {
                    let mut slots = vec![];
                    if specializable {
                        slots.push(bbctx.state(callsite.recv).clone());
                        let (filled_req, filled_opt, filled_post) = store[fid].apply_args(pos_num);
                        for i in 0..filled_req {
                            slots.push(bbctx.state(args + i).clone());
                        }
                        for _ in filled_req..store[fid].req_num() {
                            slots.push(SlotState::default());
                        }
                        for i in filled_req..filled_req + filled_opt {
                            slots.push(bbctx.state(args + i).clone());
                        }
                        for _ in filled_opt..store[fid].opt_num() {
                            slots.push(SlotState::default());
                        }
                        for i in filled_req + filled_opt..filled_req + filled_opt + filled_post {
                            slots.push(bbctx.state(args + i).clone());
                        }
                        for _ in filled_post..store[fid].post_num() {
                            slots.push(SlotState::default());
                        }
                    }
                    let block = if let Some(fid) = block_fid
                        && let Some(block_iseq) = store[fid].is_iseq()
                    {
                        Some(JitBlockInfo::new(block_iseq, self.self_class()))
                    } else {
                        None
                    };
                    let args_info = JitArgumentInfo(slots);
                    let patch_point = if self.is_specialized() {
                        None
                    } else {
                        Some(self.label())
                    };
                    let entry = self.compile_specialized_func(
                        store,
                        iseq,
                        recv_class,
                        patch_point,
                        args_info,
                        block,
                        None,
                    );
                    self.send_specialized(
                        bbctx,
                        ir,
                        store,
                        callsite,
                        fid,
                        entry,
                        patch_point,
                        evict,
                    );
                } else {
                    self.send(bbctx, ir, store, callsite, fid, recv_class, evict, None);
                }
                evict
            }
        };
        bbctx.rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict);
        bbctx.unset_class_version_guard();
        CompileResult::Continue
    }

    fn compile_specialized_func(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        self_class: ClassId,
        patch_point: Option<JitLabel>,
        args_info: JitArgumentInfo,
        block: Option<JitBlockInfo>,
        outer: Option<usize>,
    ) -> JitLabel {
        let specialize_level = self.specialize_level() + 1;
        let idx = match self.jit_type() {
            JitType::Specialized { idx, .. } => *idx,
            _ => self.specialized_methods.len(),
        };
        let jit_type = JitType::Specialized { idx, args_info };
        let mut stack_frame = self.stack_frame.clone();
        let self_ty = store[self_class].instance_ty();
        stack_frame.push(JitStackFrame::new(
            iseq_id, outer, block, self_class, self_ty,
        ));
        let mut ctx = JitContext::new_with_stack_frame(
            store,
            iseq_id,
            jit_type,
            self.class_version(),
            self.class_version_label(),
            specialize_level,
            stack_frame,
        );
        ctx.compile(store);
        let entry = self.label();
        self.specialized_methods.push(context::SpecializeInfo {
            entry,
            ctx,
            patch_point,
        });
        entry
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
        outer_lfp: Option<Lfp>,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir, true);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(store, ir, callsite, callee_fid);
        bbctx.discard(callsite.dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::Send {
            callid: callsite.id,
            callee_fid,
            recv_class,
            error,
            evict,
            outer_lfp,
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
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
        inlined_entry: JitLabel,
        patch_point: Option<JitLabel>,
        evict: AsmEvict,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir, true);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(store, ir, callsite, callee_fid);
        bbctx.discard(callsite.dst);
        bbctx.clear_above_next_sp();
        let error = ir.new_error(bbctx);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::SendSpecialized {
            callid: callsite.id,
            callee_fid,
            entry: inlined_entry,
            patch_point,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }

    fn inline_asm(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        f: impl Fn(&mut BBContext, &mut AsmIr, &JitContext, &Store, &CallSiteInfo, ClassId) -> bool,
        callsite: &CallSiteInfo,
        recv_class: ClassId,
    ) -> bool {
        let mut ctx_save = bbctx.clone();
        let ir_save = ir.save();
        if f(bbctx, ir, self, store, callsite, recv_class) {
            true
        } else {
            std::mem::swap(bbctx, &mut ctx_save);
            ir.restore(ir_save);
            false
        }
    }
}

impl BBContext {
    pub(super) fn compile_yield(&mut self, ir: &mut AsmIr, store: &Store, callid: CallSiteId) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        self.write_back_callargs_and_dst(ir, &callinfo);
        self.writeback_acc(ir);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        let evict = ir.new_evict();
        self.exec_gc(ir, true);
        ir.push(AsmInst::Yield {
            callid,
            using_xmm,
            error,
            evict,
        });
        self.rax2acc(ir, dst);
        self.immediate_evict(ir, evict);
        self.unset_class_version_guard();
    }

    fn immediate_evict(&self, ir: &mut AsmIr, evict: AsmEvict) {
        ir.push(AsmInst::ImmediateEvict { evict });
        let pc = self.pc();
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
    }
}
