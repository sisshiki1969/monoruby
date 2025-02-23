use super::*;

#[derive(Debug, Clone)]
pub(super) struct JitBlockInfo {
    func_id: FuncId,
    pub(super) self_class: ClassId,
}

impl JitBlockInfo {
    pub(super) fn is_iseq(&self, store: &Store) -> Option<ISeqId> {
        store[self.func_id].is_iseq()
    }
}

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
            if recv.is_self() {
                recv_class = self.self_class();
            }
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
            forwarding: false,
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
        bbctx.fetch(ir, recv, GP::Rdi);
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
    pub(super) fn compile_yield_specialized(
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
        bbctx.set_arguments(ir, store, &store[callid], store[block_iseq].func_id());
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        let block_entry =
            self.compile_specialized_method(store, block_iseq, block_self, None, None, None);
        let evict = ir.new_evict();
        ir.push(AsmInst::YieldSpecialized {
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
            forwarding,
            ..
        } = *callsite;
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        match store[fid].kind {
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
                bbctx.reg2acc(ir, GP::R15, dst);
                return CompileResult::Continue;
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
            FuncKind::Builtin { .. } => {}
            FuncKind::ISeq(iseq_id) => {
                let params = &store[iseq_id].params;
                if block_fid.is_some() || params.forwarding() {
                    let block_info = block_fid.map(|fid| JitBlockInfo {
                        func_id: fid,
                        self_class: self.self_class(),
                    });
                    let patch_point = match self.jit_type() {
                        JitType::Specialized(_) => None,
                        _ => Some(self.label()),
                    };
                    let forwarding_info = if let Some((id, i)) = self.forwarding_info {
                        if forwarding {
                            Some((id, i + 1))
                        } else {
                            None
                        }
                    } else {
                        if params.forwarding() {
                            Some((callsite.id, 1))
                        } else {
                            None
                        }
                    };

                    let entry = self.compile_specialized_method(
                        store,
                        iseq_id,
                        recv_class,
                        patch_point,
                        block_info,
                        forwarding_info,
                    );
                    self.send_specialized(bbctx, ir, store, callsite, fid, entry, patch_point);

                    return CompileResult::Continue;
                }
            }
        };

        if let Some((id, i)) = self.forwarding_info
            && forwarding
        {
            #[cfg(feature = "jit-log")]
            eprintln!(
                "FORWARD [{}] {}->{:?}:{} ({i})\n{:?}\n{:?}",
                self.specialize_level(),
                store.func_description(store[self.iseq_id()].func_id()),
                recv_class.get_name_id(store),
                store.func_description(fid),
                &store[id],
                store[fid].params(),
            );
            self.send_forwarding(
                bbctx,
                ir,
                store,
                &store[id],
                i,
                callsite.dst,
                fid,
                recv_class,
            );
        } else {
            self.send(bbctx, ir, store, callsite, fid, recv_class);
        }

        CompileResult::Continue
    }

    fn compile_specialized_method(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        self_class: ClassId,
        patch_point: Option<JitLabel>,
        block_info: Option<JitBlockInfo>,
        forwarding_info: Option<(CallSiteId, usize)>,
    ) -> JitLabel {
        let entry = self.label();
        if self.analyse_mode() {
            return entry;
        }
        let specialize_level = self.specialize_level() + 1;
        let jit_type = if !self.is_specialized() {
            JitType::Specialized(self.specialized_methods.len())
        } else {
            self.jit_type().clone()
        };
        let mut ctx = JitContext::new(
            store,
            iseq_id,
            jit_type,
            self.class_version(),
            self_class,
            specialize_level,
            block_info,
            forwarding_info,
        );
        ctx.compile(store);
        self.specialized_methods.push(context::SpecializeInfo {
            entry,
            ctx,
            patch_point,
        });
        entry
    }

    fn send_prep(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
    ) -> (UsingXmm, AsmError, AsmEvict) {
        let evict = ir.new_evict();
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        bbctx.set_arguments(ir, store, callsite, callee_fid);
        bbctx.discard(callsite.dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        (using_xmm, error, evict)
    }

    fn send_finish(
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        using_xmm: UsingXmm,
        error: AsmError,
        evict: AsmEvict,
    ) {
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict);
        bbctx.unset_class_version_guard();
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
    ) {
        let (using_xmm, error, evict) = self.send_prep(bbctx, ir, store, callsite, callee_fid);
        ir.push(AsmInst::Send {
            callid: callsite.id,
            callee_fid,
            recv_class,
            error,
            evict,
        });
        Self::send_finish(bbctx, ir, callsite.dst, using_xmm, error, evict);
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send_forwarding(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        i: usize,
        dst: Option<SlotId>,
        callee_fid: FuncId,
        recv_class: ClassId,
    ) {
        let evict = ir.new_evict();
        ir.reg_move(GP::Rdi, GP::R13);
        bbctx.exec_gc(ir);
        let using_xmm = bbctx.get_using_xmm();
        ir.xmm_save(using_xmm);
        ir.push(AsmInst::LfpForward { i });
        bbctx.set_arguments_forwarding(store, ir, callsite, callee_fid);
        bbctx.discard(dst);
        bbctx.clear_above_next_sp();
        let error = bbctx.new_error(ir);
        bbctx.writeback_acc(ir);
        ir.push(AsmInst::SendForwarding {
            callid: callsite.id,
            i,
            callee_fid,
            recv_class,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bbctx.rax2acc(ir, dst);
        bbctx.immediate_evict(ir, evict);
        bbctx.unset_class_version_guard();
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
    ) {
        let (using_xmm, error, evict) = self.send_prep(bbctx, ir, store, callsite, callee_fid);
        ir.push(AsmInst::SendSpecialized {
            callid: callsite.id,
            callee_fid,
            entry: inlined_entry,
            patch_point,
            error,
            evict,
        });
        Self::send_finish(bbctx, ir, callsite.dst, using_xmm, error, evict);
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
        ir.xmm_save(using_xmm);
        ir.push(AsmInst::Yield {
            callid,
            error,
            evict,
        });
        JitContext::send_finish(self, ir, dst, using_xmm, error, evict);
    }

    fn immediate_evict(&self, ir: &mut AsmIr, evict: AsmEvict) {
        ir.push(AsmInst::ImmediateEvict { evict });
        let pc = self.pc();
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
    }

    ///
    /// Verify if the invokation is **simple**.
    ///
    /// **Simple** invokation means:
    /// - no rest argument.
    /// - no splat arguments.
    /// - no hash splat arguments.
    /// - no extra positional argument.
    /// - no single argument expansion.
    /// - in the case of method call, the number of positional arguments is not greater than the maximal positional parameters.
    /// - the number of required parameters is not greater than the number of positional arguments.
    ///
    fn is_simple(callee: &FuncInfo, callsite: &CallSiteInfo) -> bool {
        let pos_num = callsite.pos_num;
        let single_arg_expand = pos_num == 1 && callee.single_arg_expand();
        let ex_positional = callee.no_keyword() && callsite.kw_may_exists();
        !callsite.has_splat()
            && !callsite.has_hash_splat()
            && !ex_positional
            && !single_arg_expand
            && !callee.is_rest()
            && (callee.is_block_style() || (pos_num <= callee.max_positional_args()))
            && callee.req_num() <= pos_num
    }

    ///
    /// Set positional arguments for callee.
    ///
    fn set_arguments(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
    ) {
        let callee = &store[callee_fid];
        let args = callsite.args;
        let pos_num = callsite.pos_num;
        let kw_pos = callsite.kw_pos;
        let kw_num = callsite.kw_len();
        if Self::is_simple(callee, callsite) {
            // write back keyword arguments.
            for arg in kw_pos..kw_pos + kw_num {
                self.write_back_slot(ir, arg);
            }
            // write back block argument.
            if let Some(block_arg) = callsite.block_arg {
                self.write_back_slot(ir, block_arg);
            }
            let ofs =
                if (args..args + pos_num).any(|reg| matches!(self.mode(reg), LinkMode::Xmm(_))) {
                    (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * pos_num) as i32 + 8) & !0xf
                } else {
                    0
                };

            ir.reg_sub(GP::Rsp, ofs);
            for i in 0..pos_num {
                let reg = args + i;
                let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, offset);
            }

            Self::fill_opt(ir, pos_num, callee.max_positional_args(), ofs);

            ir.reg_add(GP::Rsp, ofs);
        } else {
            self.write_back_args(ir, callsite);

            let error = self.new_error(ir);
            ir.push(AsmInst::SetArguments {
                callid: callsite.id,
                callee_fid,
            });
            ir.handle_error(error);
        }
    }

    ///
    /// Set positional arguments for callee.
    ///
    fn set_arguments_forwarding(
        &self,
        store: &Store,
        ir: &mut AsmIr,
        callsite: &CallSiteInfo,
        callee_fid: FuncId,
    ) {
        let callee = &store[callee_fid];
        let args = callsite.args;
        let pos_num = callsite.pos_num;
        if Self::is_simple(callee, callsite) {
            for i in 0..pos_num {
                let reg = args + i;
                let offset = -(RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32);
                ir.stack2reg(reg, GP::Rax);
                ir.reg2rsp_offset(GP::Rax, offset);
            }

            Self::fill_opt(ir, pos_num, callee.max_positional_args(), 0);
        } else {
            let error = self.new_error(ir);
            ir.push(AsmInst::SetArguments {
                callid: callsite.id,
                callee_fid,
            });
            ir.handle_error(error);
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
                matches!(self.mode(rhs), LinkMode::Xmm(_))
            }
            OpMode::RI(_, _) => false,
        };
        let ofs = if xmm_flag || callee.is_rest() {
            (RSP_LOCAL_FRAME + LFP_ARG0 + 16 as i32) & !0xf
        } else {
            0
        };

        ir.reg_sub(GP::Rsp, ofs);
        let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0);
        self.fetch_rhs_for_callee(ir, mode, offset);

        Self::fill_opt(ir, 1, callee.max_positional_args(), ofs);

        if callee.is_rest() {
            ir.push(AsmInst::RSPOffsetToArray(offset));
        }
        ir.reg_add(GP::Rsp, ofs);
    }

    fn fill_opt(ir: &mut AsmIr, start: usize, end: usize, ofs: i32) {
        if start < end {
            ir.push(AsmInst::I32ToReg(0, GP::Rax));
            for i in start..end {
                let offset = ofs - (RSP_LOCAL_FRAME + LFP_ARG0 as i32 + (8 * i) as i32);
                ir.reg2rsp_offset(GP::Rax, offset);
            }
        }
    }
}
