use super::*;

impl JitContext {
    pub(super) fn compile_yield_inlined(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        pc: BytecodePtr,
        callid: CallSiteId,
        block_iseq: ISeqId,
        block_self: ClassId,
    ) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        bbctx.write_back_callargs_and_dst(ir, &callinfo);
        bbctx.writeback_acc(ir);
        let using_xmm = bbctx.get_using_xmm();
        let error = ir.new_error(bbctx, pc);
        let evict = ir.new_evict();
        let block_entry = self.compile_inline_method(store, block_iseq, block_self, None);
        ir.push(AsmInst::YieldInlined {
            callid,
            using_xmm,
            block_iseq,
            block_entry,
            error,
            evict,
        });
        bbctx.rax2acc(ir, dst);
    }

    pub(super) fn call(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        cache: MethodCacheEntry,
        callid: CallSiteId,
        pc: BytecodePtr,
    ) -> CompileResult {
        let CallSiteInfo { dst, recv, .. } = store[callid];
        let MethodCacheEntry {
            mut recv_class,
            mut func_id,
            mut version,
        } = cache;
        if recv.is_self() && self.self_class != recv_class {
            // the inline method cache is invalid because the receiver class is not matched.
            let class_version = self.class_version;
            let name = store[callid].name.unwrap();
            if let Some(entry) = store.check_method_for_class(self.self_class, name, class_version)
                && let Some(fid) = entry.func_id()
            {
                recv_class = self.self_class;
                func_id = fid;
                version = class_version;
            } else {
                return CompileResult::Recompile;
            }
        }
        // We must write back and unlink all local vars when they are possibly accessed from inner blocks.
        if store[callid].block_fid.is_some() || store[func_id].meta().is_eval() {
            bbctx.write_back_locals(ir);
        }
        bbctx.fetch_for_gpr(ir, recv, GP::Rdi);
        let (deopt, error) = ir.new_deopt_error(bbctx, pc);
        let using_xmm = bbctx.get_using_xmm();
        ir.guard_version(func_id, version, callid, using_xmm, deopt, error);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_self() && !bbctx.is_class(recv, recv_class) {
            ir.guard_class(bbctx, recv, GP::Rdi, recv_class, deopt);
        }
        if let Some(evict) = self.call_cached(bbctx, ir, store, callid, func_id, recv_class, pc) {
            bbctx.rax2acc(ir, dst);
            if let Some(evict) = evict {
                ir.push(AsmInst::ImmediateEvict { evict });
                ir[evict] = SideExit::Evict(Some((pc + 2, bbctx.get_write_back())));
            }
        } else {
            return CompileResult::Recompile;
        }

        CompileResult::Continue
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
    fn call_cached(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        fid: FuncId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> Option<Option<AsmEvict>> {
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            ..
        } = store[callid];
        let callsite = &store[callid];
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
                    let ivarid = store[recv_class].get_ivarid(ivar_name)?;
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
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                let ivarid = store[recv_class].get_ivarid(ivar_name)?;
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
            }
            FuncKind::Builtin { .. } => {
                let evict = ir.new_evict();
                self.send(bbctx, ir, store, pc, callid, fid, recv_class, evict);
                return Some(Some(evict));
            }
            FuncKind::ISeq(iseq_id) => {
                let evict = ir.new_evict();
                if self.inlining_level < 3 {
                    let block_info = block_fid.map(|fid| (fid, self.self_class));
                    let inlined_entry =
                        self.compile_inline_method(store, iseq_id, recv_class, block_info);
                    self.send_inlined(bbctx, ir, store, pc, callid, fid, inlined_entry, evict);
                    return Some(Some(evict));
                } else {
                    self.send(bbctx, ir, store, pc, callid, fid, recv_class, evict);
                    return Some(Some(evict));
                }
            }
        };
        Some(None)
    }

    fn compile_inline_method(
        &mut self,
        store: &Store,
        inlined_iseq_id: ISeqId,
        inlined_self_class: ClassId,
        block_info: Option<(FuncId, ClassId)>,
    ) -> JitLabel {
        let mut ctx = JitContext::new(
            store,
            inlined_iseq_id,
            None,
            self.class_version,
            inlined_self_class,
            self.inlining_level + 1,
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
        pc: BytecodePtr,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        ir.exec_gc(bbctx.get_register());
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        ir.set_arguments(store, bbctx, callid, callee_fid, pc);
        bbctx.unlink(store[callid].dst);
        bbctx.clear();
        let error = ir.new_error(bbctx, pc);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::Send {
            callid,
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
        pc: BytecodePtr,
        callid: CallSiteId,
        callee_fid: FuncId,
        inlined_entry: JitLabel,
        evict: AsmEvict,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        ir.exec_gc(bbctx.get_register());
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        ir.set_arguments(store, bbctx, callid, callee_fid, pc);
        bbctx.unlink(store[callid].dst);
        bbctx.clear();
        let error = ir.new_error(bbctx, pc);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::SendInlined {
            callid,
            callee_fid,
            inlined_entry,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }
}

impl BBContext {
    pub(super) fn compile_binop_call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        fid: FuncId,
        version: u32,
        info: BinOpInfo,
        pc: BytecodePtr,
    ) -> CompileResult {
        assert!(matches!(
            store[fid].kind,
            FuncKind::Builtin { .. } | FuncKind::ISeq(_)
        ));
        let callee = &store[fid];
        if (!callee.is_rest() && callee.max_positional_args() < 1) || callee.req_num() > 1 {
            return CompileResult::Recompile;
        }
        let BinOpInfo {
            dst,
            mode,
            lhs_class,
            ..
        } = info;
        let deopt = ir.new_deopt(self, pc);
        self.fetch_lhs(ir, mode);
        ir.guard_lhs_class_for_mode(self, mode, lhs_class, deopt);
        ir.push(AsmInst::GuardClassVersion(version, deopt));

        let evict = ir.new_evict();
        ir.reg_move(GP::Rdi, GP::R13);
        let using_xmm = self.get_using_xmm();
        ir.xmm_save(using_xmm);

        ir.set_binop_arguments(store, self, fid, mode);

        self.unlink(dst);
        self.clear();
        let error = ir.new_error(self, pc);
        self.writeback_acc(ir);
        ir.push(AsmInst::BinopCached {
            callee_fid: fid,
            recv_class: lhs_class,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        self.rax2acc(ir, dst);
        ir.push(AsmInst::ImmediateEvict { evict });
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
        CompileResult::Continue
    }

    pub(super) fn inline_asm(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        f: impl Fn(&mut AsmIr, &Store, &mut BBContext, CallSiteId, ClassId, BytecodePtr) -> bool,
        callid: CallSiteId,
        cache: &MethodCacheEntry,
        pc: BytecodePtr,
    ) -> bool {
        let mut ctx_save = self.clone();
        let ir_save = ir.save();
        let recv = store[callid].recv;
        self.fetch_for_gpr(ir, recv, GP::Rdi);
        let (deopt, error) = ir.new_deopt_error(self, pc);
        let using_xmm = self.get_using_xmm();
        let MethodCacheEntry {
            recv_class,
            func_id,
            version,
        } = cache;
        ir.guard_version(*func_id, *version, callid, using_xmm, deopt, error);
        if !recv.is_self() && !self.is_class(recv, *recv_class) {
            ir.guard_class(self, recv, GP::Rdi, *recv_class, deopt);
        }
        if f(ir, store, self, callid, *recv_class, pc) {
            true
        } else {
            std::mem::swap(self, &mut ctx_save);
            ir.restore(ir_save);
            false
        }
    }

    pub(super) fn compile_yield(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        pc: BytecodePtr,
        callid: CallSiteId,
    ) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        self.write_back_callargs_and_dst(ir, &callinfo);
        self.writeback_acc(ir);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let evict = ir.new_evict();
        ir.push(AsmInst::Yield {
            callid,
            using_xmm,
            error,
            evict,
        });
        self.rax2acc(ir, dst);
    }

    /*fn send_not_cached(&self, ir: &mut AsmIr, pc: BytecodePtr, callid: CallSiteId) {
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let evict = ir.new_evict();
        let self_class = self.self_value.class();
        ir.xmm_save(using_xmm);
        ir.push(AsmInst::SendNotCached {
            self_class,
            callid,
            pc,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }*/
}
