use super::*;

impl JitContext {
    ///
    /// Compile TraceIr::MethodCall with inline method cache info.
    ///
    pub(super) fn compile_method_call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
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
            recv_class = self.self_class();
            if let Some(fid) = self.jit_check_call(store, recv_class, callsite.name) {
                func_id = fid;
            } else {
                return CompileResult::Recompile;
            }
        }

        // We must write back and unlink all local vars when they are possibly accessed from inner blocks.
        if callsite.block_fid.is_some() || store[func_id].meta().is_eval() {
            bbctx.write_back_locals(ir);
        }

        self.recv_version_guard(bbctx, ir, recv, recv_class);

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

    pub(super) fn compile_index_call(
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
    }

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
            return CompileResult::Recompile;
        }

        // class version guard
        let class_version = self.class_version();
        let deopt = bbctx.new_deopt(ir);
        bbctx.guard_class_version(ir, class_version, deopt);

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
        let error = bbctx.new_error(ir);
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
        let version = self.class_version();

        // class version guard
        let deopt = bbctx.new_deopt(ir);
        bbctx.guard_class_version(ir, version, deopt);

        // receiver class guard
        bbctx.fetch_for_gpr(ir, recv, GP::Rdi);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_self() && !bbctx.is_class(recv, recv_class) {
            bbctx.guard_class(ir, recv, GP::Rdi, recv_class, deopt);
        }
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn compile_yield_inlined(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        block_iseq: ISeqId,
        block_self: ClassId,
    ) {
        let dst = store[callid].dst;
        bbctx.exec_gc(ir);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(store, ir, &store[callid], store[block_iseq].func_id());
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        let block_entry = self.inlined_method(store, block_iseq, block_self, None);
        let evict = ir.new_evict();
        ir.push(AsmInst::YieldInlined {
            callid,
            block_iseq,
            block_entry,
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
        fid: FuncId,
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
        match store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                if recv_class.is_always_frozen() {
                    if dst.is_some() {
                        ir.lit2reg(Value::nil(), GP::Rax);
                    }
                } else {
                    let ivarid = if let Some(id) = store[recv_class].get_ivarid(ivar_name) {
                        id
                    } else {
                        return CompileResult::Recompile;
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
                bbctx.rax2acc(ir, dst);
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                let ivarid = if let Some(id) = store[recv_class].get_ivarid(ivar_name) {
                    id
                } else {
                    return CompileResult::Recompile;
                };
                bbctx.fetch_for_gpr(ir, args, GP::Rax);
                let is_object_ty = store[recv_class].is_object_ty_instance();
                let using_xmm = bbctx.get_using_xmm();
                if is_object_ty && ivarid.is_inline() {
                    ir.push(AsmInst::StoreIVarInline { ivarid })
                } else {
                    ir.push(AsmInst::StoreIVarHeap {
                        ivarid,
                        using_xmm,
                        self_: false,
                        is_object_ty,
                    });
                }
                bbctx.rax2acc(ir, dst);
            }
            FuncKind::Builtin { .. } => {
                let evict = ir.new_evict();
                self.send(bbctx, ir, store, callsite, fid, recv_class, evict);
                bbctx.rax2acc(ir, dst);
                bbctx.immediate_evict(ir, evict);
                bbctx.unset_class_version_guard();
            }
            FuncKind::ISeq(iseq_id) => {
                let evict = ir.new_evict();
                if let Some(block_fid) = block_fid {
                    let block_info = Some((block_fid, self.self_class()));
                    let inlined_entry = self.inlined_method(store, iseq_id, recv_class, block_info);
                    self.send_inlined(bbctx, ir, store, callsite, fid, inlined_entry, evict);
                } else {
                    self.send(bbctx, ir, store, callsite, fid, recv_class, evict);
                }
                bbctx.rax2acc(ir, dst);
                bbctx.immediate_evict(ir, evict);
                bbctx.unset_class_version_guard();
            }
        };
        CompileResult::Continue
    }

    fn inlined_method(
        &mut self,
        store: &Store,
        inlined_iseq_id: ISeqId,
        inlined_self_class: ClassId,
        block_info: Option<(FuncId, ClassId)>,
    ) -> JitLabel {
        let mut ctx = JitContext::new(
            store,
            inlined_iseq_id,
            JitType::Inlined,
            self.class_version(),
            inlined_self_class,
            self.inlining_level() + 1,
            block_info,
        );
        ctx.compile(store);
        let inlined_entry = self.label();
        self.inlined_methods.push((inlined_entry, ctx));
        inlined_entry
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
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(store, ir, callsite, callee_fid);
        bbctx.discard(callsite.dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::Send {
            callid: callsite.id,
            callee_fid,
            recv_class,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send_inlined(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
        inlined_entry: JitLabel,
        evict: AsmEvict,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(store, ir, callsite, callee_fid);
        bbctx.discard(callsite.dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::SendInlined {
            callid: callsite.id,
            callee_fid,
            inlined_entry,
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
        let error = self.new_error(ir);
        let evict = ir.new_evict();
        ir.push(AsmInst::Yield {
            callid,
            using_xmm,
            error,
            evict,
        });
        self.rax2acc(ir, dst);
        self.unset_class_version_guard();
    }

    fn immediate_evict(&self, ir: &mut AsmIr, evict: AsmEvict) {
        ir.push(AsmInst::ImmediateEvict { evict });
        let pc = self.pc();
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
    }
}
