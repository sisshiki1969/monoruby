use crate::{
    codegen::jitgen::{context::JitStackFrame, state::LinkMode},
    executor::inline::InlineFuncInfo,
};

use super::{
    context::{JitArgumentInfo, JitBlockInfo},
    *,
};

impl<'a> JitContext<'a> {
    pub(super) fn method_call(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        cache: MethodCache,
    ) -> JitResult<CompileResult> {
        let callsite = &self.store[callid];
        let recv_class = state.class(callsite.recv);
        let (recv_class, func_id) = if let Some(recv_class) = recv_class {
            // the receiver class is known.
            if let Some(func_id) = self.jit_check_call(recv_class, callsite.name) {
                (recv_class, func_id)
            } else {
                return Ok(CompileResult::Recompile(RecompileReason::MethodNotFound));
            }
        } else {
            // here, recv_class is none.
            match cache {
                MethodCache::Cached(cache) => {
                    if cache.version != self.class_version() {
                        // the inline method cache is invalid.
                        let recv_class = cache.recv_class;
                        let func_id =
                            if let Some(fid) = self.jit_check_call(recv_class, callsite.name) {
                                fid
                            } else {
                                return Ok(CompileResult::Recompile(
                                    RecompileReason::MethodNotFound,
                                ));
                            };
                        (recv_class, func_id)
                    } else {
                        // the inline method cache is valid.
                        (cache.recv_class, cache.func_id)
                    }
                }
                // The VM resolved this call to `method_missing` and the cached
                // class version is still current. The JIT has no lowering for a
                // method_missing dispatch, so plain-deopt to the VM here instead
                // of requesting a recompile. A recompile would re-read the null
                // fid → `NotCached` → recompile again, never stabilizing — the
                // recompile-thrash that makes method_missing-heavy hot loops
                // ~100x slower than the interpreter.
                MethodCache::MethodMissing { version, .. } if version == self.class_version() => {
                    return Ok(CompileResult::Deopt);
                }
                // No cache, or a stale method_missing cache (the resolution may
                // have changed): fall back to the recompile-once path, which
                // re-reads the cache after the VM warms it.
                _ => return Ok(CompileResult::Recompile(RecompileReason::NotCached)),
            }
        };
        // Feed the class of a single positional argument — when the abstract
        // state has *proven* it — to the inline generators. `array_index` and
        // the String `==`/`!=` constant folds gate on the argument class;
        // a generic call site (e.g. an explicit `a.[](i)` spelling) always
        // arrived here with `None` and fell back to a full non-inlined call
        // even when the argument was a known Fixnum. Restricted to simple
        // 1-positional-argument sites: the `[]=` inliner asserts `dst` is
        // absent, which only the specialized IndexAssign path guarantees.
        let arg_class = {
            let callsite = &self.store[callid];
            if callsite.is_simple() && callsite.pos_num == 1 {
                state.class(callsite.args)
            } else {
                None
            }
        };
        self.compile_method_call(state, ir, recv_class, arg_class, func_id, callid, false)
    }

    ///
    /// Compile TraceIr::MethodCall with inline method cache info.
    ///
    #[cfg_attr(target_arch = "aarch64", allow(unused_variables))]
    pub(super) fn compile_method_call(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        recv_class: ClassId,
        arg_class: Option<ClassId>,
        func_id: FuncId,
        callid: CallSiteId,
        // When the receiver-class guard misses, recompile (so the
        // site flips to the non-deopting polymorphic path) instead
        // of plain-deopting forever. Only set for monomorphic-
        // compiled BinCmp sites, which have such a path (Part B).
        recompile_on_recv_miss: bool,
    ) -> JitResult<CompileResult> {
        let callsite = &self.store[callid];
        self.inline_method_cache
            .push((recv_class, callsite.name, func_id));
        let recv = callsite.recv;

        if self.store[func_id].possibly_capture_without_block() {
            return Err(CompileError);
        }
        // Methods that forward `&block` via a BlockArg instruction
        // trigger `move_frame_to_heap` on an outer frame when invoked.
        // JIT specialisation inlines the callee into the caller's
        // frame, which means no `pop_frame` to reload r14 to the heap
        // copy after the promotion. Subsequent reads of the caller's
        // locals / outer would split between the invalidated stack
        // tombstone (via r14) and the heap copy (via the materialised
        // Proc's `outer_lfp`). Refuse specialisation so the call is
        // dispatched normally (push_frame / pop_frame) and r14 is
        // refreshed after return.
        if let Some(iseq) = self.store[func_id].is_iseq()
            && self.store[iseq].has_block_arg()
        {
            return Err(CompileError);
        }
        // We must write back all local vars to the stack and set the state to LinkMode::S when they are possibly accessed or captured from inner blocks.
        if callsite.block_fid.is_some() {
            state.locals_to_S(ir);
        }

        // class version guard
        self.guard_class_version(state, ir, true);

        // receiver class guard
        if state.class(recv) != Some(recv_class) {
            // Specialized JIT recompiles via an idx, not a position;
            // keep it on the plain deopt path (no Part B there).
            let use_recompile = recompile_on_recv_miss
                && !matches!(self.jit_type(), JitType::Specialized { .. });
            let deopt = if use_recompile {
                ir.new_recompile_deopt(
                    state,
                    RecompileReason::BecamePolymorphic,
                    self.position(),
                )
            } else {
                ir.new_deopt(state)
            };
            state.load(ir, recv, GP::Rdi);
            state.guard_class(ir, recv, GP::Rdi, recv_class, deopt);
        }

        if callsite.block_fid.is_none()
            && let Some(info) = self.store.inline_info.get_inline(func_id)
        {
            match info {
                InlineFuncInfo::InlineGen(f) => {
                    if self.inline_asm(state, ir, f, callid, recv_class, arg_class) {
                        state.unset_side_effect_guard();
                        return Ok(CompileResult::Continue);
                    }
                }
                InlineFuncInfo::CFunc_F_F(f) => {
                    let CallSiteInfo { args, dst, .. } = *callsite;
                    if let Some(args) = state.coerce_C_f64(args) {
                        let res = unsafe { f(args) };
                        if match dst {
                            Some(dst) => state.def_C_float(dst, res),
                            None => true,
                        } {
                            return Ok(CompileResult::Continue);
                        }
                    }
                    if let Some(dst) = dst {
                        let src = state.load_fpr(ir, args);
                        state.pin_fpr(src);
                        state.discard(dst);
                        let using_fpr = state.get_using_fpr(ir);
                        let dst = state.def_F(dst);
                        state.unpin_fpr(src);
                        ir.push(AsmInst::CFunc_F_F {
                            f: *f,
                            src,
                            dst,
                            using_fpr,
                        });
                    }
                    return Ok(CompileResult::Continue);
                }
                InlineFuncInfo::CFunc_FF_F(f) => {
                    let CallSiteInfo {
                        recv, args, dst, ..
                    } = *callsite;
                    if let Some((lhs, rhs)) = state.check_binary_C_f64(recv, args) {
                        let res = f(lhs, rhs);
                        if match dst {
                            Some(dst) => state.def_C_float(dst, res),
                            None => true,
                        } {
                            return Ok(CompileResult::Continue);
                        }
                    }
                    if let Some(dst) = dst {
                        // Pin lhs across rhs load and dst alloc; otherwise the
                        // allocator can pick lhs's fpr as spill victim and the
                        // consuming CFunc gets aliased operands. Same for rhs
                        // across the dst alloc.
                        let lhs = state.load_fpr(ir, recv);
                        state.pin_fpr(lhs);
                        let rhs = state.load_fpr(ir, args);
                        state.pin_fpr(rhs);
                        state.discard(dst);
                        let using_fpr = state.get_using_fpr(ir);
                        let dst = state.def_F(dst);
                        state.unpin_fpr(rhs);
                        state.unpin_fpr(lhs);
                        ir.push(AsmInst::CFunc_FF_F {
                            f: *f,
                            lhs,
                            rhs,
                            dst,
                            using_fpr,
                        });
                    }
                    return Ok(CompileResult::Continue);
                }
            }
        }

        //
        // generate JIT code for a cached method call.
        //
        // ### in
        // - rdi: receiver: Value
        //
        // ### out
        // - rax: return value: Value
        //
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            ..
        } = *callsite;
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        let (fid, outer_lfp) = match self.store[func_id].kind {
            FuncKind::AttrReader { ivar_name } => {
                return Ok(self.attr_reader(state, ir, callid, recv_class, ivar_name));
            }
            FuncKind::AttrWriter { ivar_name } => {
                return Ok(self.attr_writer(state, ir, callid, recv_class, ivar_name));
            }
            FuncKind::StructReader { slot_index, inline } => {
                return Ok(self.struct_slot_reader(state, ir, callid, slot_index, inline));
            }
            FuncKind::StructWriter { slot_index, inline } => {
                return Ok(self.struct_slot_writer(state, ir, callid, slot_index, inline));
            }
            FuncKind::Builtin { .. } => (func_id, None),
            FuncKind::Proc(proc) => (proc.func_id(), proc.outer_lfp()),
            FuncKind::ISeq(iseq) => {
                // Check ISeq hint for trivial methods. Only fold when the
                // call site's argument shape would actually dispatch
                // without raising; otherwise CRuby raises ArgumentError
                // and we must fall through to the normal path so the
                // runtime can do the same.
                //
                // `is_simple_call` covers positional arity, splats, and
                // the "callee has no kw + callsite passes kw" case, but
                // it does NOT validate keyword matching when the callee
                // accepts kwargs — required-kw-missing and unknown-kw
                // would silently fold otherwise. Restrict folding to the
                // "neither side touches kwargs" case to avoid that.
                if self.store.is_simple_call(func_id, callid)
                    && self.store[func_id].no_keyword()
                    && !callsite.kw_may_exists()
                {
                    match self.store[iseq].hint {
                        ISeqHint::ConstReturn(v) => {
                            state.def_C(dst, v);
                            return Ok(CompileResult::Continue);
                        }
                        ISeqHint::SelfReturn => {
                            if let Some(dst) = dst {
                                state.copy_slot(ir, callsite.recv, dst);
                            }
                            return Ok(CompileResult::Continue);
                        }
                        ISeqHint::Normal => {}
                    }
                }
                // Use `is_C_immediate` here, not `is_C`: heap-resident
                // `LinkMode::C` (e.g. class constants newly folded by
                // `load_constant`) would otherwise trigger specialization
                // of methods like `Array.new`, whose body contains a
                // polymorphic-on-`o` `__send__(:initialize, ...)`. The
                // JIT picks up the inline cache's currently-cached
                // receiver class, propagates it as `Guarded::Class(...)`
                // onto `o`, and the trailing `o` becomes
                // `ReturnValue::Class(...)` — overwriting the caller's
                // dst slot with the wrong class. See the
                // `attr_reader_in_different_class` regression for the
                // observable failure.
                // Always specialize calls to an argument-forwarding
                // method (`def f(...)`): forwarding bodies are thin
                // trampolines whose cost is dominated by the
                // re-parse / rest-array of the forwarded `...`, which
                // specialization (and D1) removes — so don't gate them
                // on the immediate-arg heuristic.
                let forwarding_callee = self.store[func_id].params().forwarding();
                let specializable = self.store.is_simple_call(func_id, callid)
                    && (forwarding_callee
                        || state.is_C_immediate(callsite.recv)
                        || (pos_num != 0
                            && (args..args + pos_num).any(|i| state.is_C_immediate(i))));
                let iseq_block = block_fid.map(|fid| self.store[fid].is_iseq()).flatten();

                // Method specialization (inlining a callee iseq) and block-
                // argument inlining (`iseq_block`, which drives specialized
                // `yield`) are both lowered on x86 and aarch64 now.
                if (specializable && self.specialize_level() < 5) || iseq_block.is_some()
                /*name == Some(IdentId::NEW)*/
                {
                    return self.specialized_iseq(
                        state,
                        ir,
                        callid,
                        recv_class,
                        func_id,
                        iseq,
                        specializable,
                    );
                }
                (func_id, None)
            }
        };

        if block_fid.is_some() {
            state.unset_no_capture_guard(self);
        }

        state.send(ir, &self.store, callid, fid, recv_class, outer_lfp);

        Ok(CompileResult::Continue)
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
    pub(super) fn guard_class_version(
        &self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        with_recovery: bool,
    ) {
        if state.class_version_guard() {
            return;
        }
        let deopt = ir.new_deopt(state);
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
        state.set_class_version_guard();
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn compile_yield_specialized(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        block: &JitBlockInfo,
        iseq: ISeqId,
    ) -> JitResult<CompileResult> {
        let dst = self.store[callid].dst;
        let JitBlockInfo {
            block_fid: callee_fid,
            self_class,
            outer,
        } = block.add(1);
        let simple = self.store.is_simple_call(callee_fid, callid);
        let args_info = if simple {
            JitArgumentInfo::new(LinkMode::from_caller_yield(
                &self.store,
                callee_fid,
                callid,
                state,
                self_class,
            ))
        } else {
            JitArgumentInfo::default()
        };
        let SpecializedCompileResult {
            entry,
            return_state,
            deferred_rest: _,
            needs_rest_array: _,
        } = self.compile_specialized_func(
            state,
            iseq,
            self_class,
            None,
            args_info,
            Some(outer),
            callid,
        )?;
        state.exec_gc(ir, true);
        let using_fpr = state.get_using_fpr(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        state.set_arguments(&self.store, ir, callid, callee_fid, false);
        state.discard(dst);
        state.clear_above_next_sp();
        let error = ir.new_error(state);
        let evict = ir.new_evict();
        let meta = self.store[callee_fid].meta();
        ir.push(AsmInst::SetupYieldFrame { meta, outer });
        ir.push(AsmInst::SpecializedYield { entry, evict });
        ir.fpr_restore_cont(using_fpr);
        ir.handle_error(error);
        let res = state.def_rax2acc_return(ir, dst, return_state);
        state.immediate_evict(ir, evict);
        Ok(res)
    }

    fn attr_reader(
        &mut self,
        state: &mut AbstractState,
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
            recv,
            ..
        } = *callsite;
        assert_eq!(0, pos_num);
        assert!(!callsite.kw_may_exists());
        assert!(block_fid.is_none());
        assert!(callsite.block_arg.is_none());
        state.load(ir, recv, GP::Rdi);
        state.discard(dst);
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
                ir.push(AsmInst::LoadIVarInline {
                    ivarid,
                    dst: GP::R15,
                })
            } else {
                ir.push(AsmInst::LoadIVarHeap {
                    ivarid,
                    is_object_ty,
                    self_: false,
                    dst: GP::R15,
                });
            }
        }
        state.def_reg2acc(ir, GP::R15, dst);
        CompileResult::Continue
    }

    fn attr_writer(
        &mut self,
        state: &mut AbstractState,
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
            recv,
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
        state.load(ir, recv, GP::Rdi);
        let deopt = ir.new_deopt(state);
        ir.guard_frozen(deopt);
        let src = state.load_or_reg(ir, args, GP::Rax);
        let is_object_ty = self.store[recv_class].is_object_ty_instance();
        let using_fpr = state.get_using_fpr(ir);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid })
        } else {
            ir.push(AsmInst::StoreIVarHeap {
                src,
                ivarid,
                using_fpr,
                is_object_ty,
            });
        }
        state.def_rax2acc(ir, dst);
        state.unset_side_effect_guard();
        CompileResult::Continue
    }

    /// JIT inline a `Struct` member reader. Receiver class is already
    /// guarded by the call-site cache; the JIT picks INLINE vs HEAP
    /// statically based on the class's member count, so the emitted
    /// code is exactly **1 mov** for the inline case (≤
    /// `STRUCT_INLINE_SLOTS` members) and 2 movs for the heap case.
    /// Mirrors how `attr_reader` distinguishes inline vs heap ivars.
    fn struct_slot_reader(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        slot_index: u16,
        inline: bool,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        let CallSiteInfo {
            pos_num,
            dst,
            block_fid,
            recv,
            ..
        } = *callsite;
        assert_eq!(0, pos_num);
        assert!(!callsite.kw_may_exists());
        assert!(block_fid.is_none());
        assert!(callsite.block_arg.is_none());
        state.load(ir, recv, GP::Rdi);
        state.discard(dst);
        if inline {
            ir.push(AsmInst::LoadStructSlotInline { slot_index });
        } else {
            ir.push(AsmInst::LoadStructSlotHeap { slot_index });
        }
        state.def_reg2acc(ir, GP::R15, dst);
        CompileResult::Continue
    }

    /// JIT inline a `Struct` member writer. Mirrors `attr_writer` —
    /// guard frozen, then emit a 1-mov inline store or 2-mov heap
    /// store based on the FuncKind's `inline` flag.
    fn struct_slot_writer(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        slot_index: u16,
        inline: bool,
    ) -> CompileResult {
        let callsite = &self.store[callid];
        let CallSiteInfo {
            args,
            pos_num,
            dst,
            block_fid,
            recv,
            ..
        } = *callsite;
        assert_eq!(1, pos_num);
        assert!(!callsite.kw_may_exists());
        assert!(block_fid.is_none());
        state.load(ir, recv, GP::Rdi);
        let deopt = ir.new_deopt(state);
        ir.guard_frozen(deopt);
        let src = state.load_or_reg(ir, args, GP::Rax);
        if inline {
            ir.push(AsmInst::StoreStructSlotInline { src, slot_index });
        } else {
            ir.push(AsmInst::StoreStructSlotHeap { src, slot_index });
        }
        state.def_rax2acc(ir, dst);
        state.unset_side_effect_guard();
        CompileResult::Continue
    }

    fn specialized_iseq(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        fid: FuncId,
        iseq: ISeqId,
        specializable: bool,
    ) -> JitResult<CompileResult> {
        let dst = self.store[callid].dst;
        let args_info = if specializable {
            JitArgumentInfo::new(LinkMode::from_caller(&self.store, fid, callid, state))
        } else {
            JitArgumentInfo::default()
        };
        let patch_point = if self.is_specialized() {
            None
        } else {
            Some(self.label())
        };
        let SpecializedCompileResult {
            entry,
            return_state,
            deferred_rest,
            needs_rest_array,
        } = self.compile_specialized_func(
            state,
            iseq,
            recv_class,
            patch_point,
            args_info,
            None,
            callid,
        )?;
        let evict = ir.new_evict();
        state.send_specialized(
            ir,
            &self.store,
            callid,
            fid,
            entry,
            patch_point,
            evict,
            deferred_rest,
            needs_rest_array,
        );
        let res = state.def_rax2acc_return(ir, dst, return_state);
        state.immediate_evict(ir, evict);
        return Ok(res);
    }
}

pub(super) struct SpecializedCompileResult {
    pub entry: JitLabel,
    pub return_state: Option<ReturnState>,
    /// D1: the trampoline body's forwarding consumer elided `f`'s rest
    /// `Array` (routed straight from the caller source); the caller-side
    /// `set_arguments` must skip the `create_array`.
    pub deferred_rest: bool,
    /// D1 veto: some forwarding consume needs the real rest `Array`.
    pub needs_rest_array: bool,
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
            None,
        )
    }

    fn compile_specialized_func(
        &mut self,
        state: &mut AbstractState,
        iseq_id: ISeqId,
        self_class: ClassId,
        patch_point: Option<JitLabel>,
        args_info: JitArgumentInfo,
        outer: Option<usize>,
        callid: CallSiteId,
    ) -> JitResult<SpecializedCompileResult> {
        let frame = self.new_specialized_frame(iseq_id, outer, args_info, self_class);

        let mut frame = self.specialized_compile(state, callid, frame)?;
        // we must unset no_capture_guard for all state frames if no_capture_guard of the current frame became false.
        if !state.no_capture_guard() {
            state.unset_all_no_capture_guard();
        }

        let pos = self.current_frame_pos();
        let mut return_context = frame.detach_return_context();
        let return_state = return_context.remove(&pos);
        self.merge_return_context(return_context);
        // Capture before `frame.asm_info` is moved below.
        let frame_had_deopt = frame.had_deopt;
        let frame_deferred_rest = frame.deferred_rest;
        let frame_needs_rest_array = frame.needs_rest_array;
        // Two unmodeled-path conditions taint the return state so the
        // caller doesn't propagate a speculative `Const` past us:
        //
        // 1. `has_exception_handler`: the BB graph doesn't include
        //    rescue/ensure successors, so the computed return state
        //    only reflects the happy path. See issue #405.
        //
        // 2. `frame_had_deopt`: any deopt-able side exit emitted
        //    during this iseq's compile means the runtime can resume
        //    in the interpreter from the deopt PC and produce a
        //    different rax than the abstract interpreter's
        //    speculation predicted (e.g. `Array#assoc`'s block doing
        //    `return elem` after the recv-class guard fails). See
        //    PR #505.
        let return_state = return_state.map(|mut s| {
            if self.store[iseq_id].has_exception_handler() || frame_had_deopt {
                s.taint_for_unmodeled_rescue();
            }
            s
        });
        // Even when the specialized body has been fully const-folded,
        // we keep the call site: the asm still contains the
        // speculation's deopt-able guards and the non-local-return
        // jump targets that a deopt'd interp may use. Skipping the
        // call site (the previous `SpecializedCompileResult::Const`
        // shortcut) made those runtime paths unreachable — which
        // broke e.g. `Array#assoc`, whose block's `return elem`
        // depends on the deopt path to set rax for the non-local
        // return.
        #[cfg(feature = "jit-debug")]
        if self.codegen_mode() {
            eprintln!(
                "return: {} {:?}",
                self.store.func_description(self.store[iseq_id].func_id()),
                return_state
            );
        }
        let entry = self.label();
        self.specialized_methods_push(context::SpecializeInfo {
            entry,
            info: frame.asm_info,
            patch_point,
        });
        // Propagate the deopt fact one level up: if this inlined
        // sub-iseq could deopt, the caller's compiled body also
        // contains that deopt-able path, so the caller's own
        // return-state taint check needs to see it. We are always
        // inside the caller's frame here (specialized_compile pushed
        // and popped the sub-frame internally).
        if frame_had_deopt {
            self.current_frame_mut().had_deopt = true;
        }
        Ok(SpecializedCompileResult {
            entry,
            return_state,
            deferred_rest: frame_deferred_rest,
            needs_rest_array: frame_needs_rest_array,
        })
    }

    fn inline_asm(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        f: impl Fn(
            &mut AbstractState,
            &mut AsmIr,
            &JitContext,
            &Store,
            CallSiteId,
            ClassId,
            Option<ClassId>,
        ) -> bool,
        callid: CallSiteId,
        recv_class: ClassId,
        arg_class: Option<ClassId>,
    ) -> bool {
        // No GP flush here: a register-only inline keeps the residents live,
        // while a C-ABI-call inline flushes them at its `get_using_fpr`
        // chokepoint (see `SlotState::get_using_fpr`).
        let state_save = state.clone();
        let ir_save = ir.save();
        if f(state, ir, self, &self.store, callid, recv_class, arg_class) {
            true
        } else {
            *state = state_save;
            ir.restore(ir_save);
            false
        }
    }
}

impl AbstractState {
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
        outer_lfp: Option<Lfp>,
    ) {
        let evict = ir.new_evict();
        let dst = store[callid].dst;
        self.exec_gc(ir, true);
        // Flush the GP pool up front (folded into `get_using_fpr`), before
        // `set_arguments`. An earlier optimization deferred this for simple,
        // block-less calls — reading args straight from the pool registers and
        // letting a later flush handle it — to skip spilling the dead `dst`/temp
        // args. That deferral proved unsound under register pressure: keeping
        // locals pool-resident through `set_arguments` / `discard` /
        // `clear_above_next_sp` diverged from the up-front-flush semantics
        // (optcarrot `--opt` mis-emulated a few frames in on aarch64 `gp-alloc`,
        // e.g. a wrong PPU value broke the vblank-wait loop), so we always flush
        // before the call now.
        let using_fpr = self.get_using_fpr(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        self.set_arguments(store, ir, callid, callee_fid, false);
        self.discard(dst);
        self.clear_above_next_sp();
        let error = ir.new_error(self);
        let meta = store[callee_fid].meta();
        ir.push(AsmInst::SetupMethodFrame {
            meta,
            callid,
            outer_lfp,
        });
        ir.push(AsmInst::Call {
            callee_fid,
            recv_class,
            evict,
            pc: self.pc(),
        });
        ir.fpr_restore_cont(using_fpr);
        ir.handle_error(error);
        // When a capture guard follows (the callee may `move_frame_to_heap`,
        // e.g. by turning a block into a Proc), the result must be homed via
        // the LFP so it follows the frame onto the heap — see
        // `def_rax2acc_capturing`. Otherwise park the result in a GP-pool
        // register (a resident) so a following integer op consumes it without a
        // stack round-trip — see `def_rax2gp`.
        if self.no_capture_guard() {
            self.def_rax2gp(ir, dst);
        } else {
            self.def_rax2acc_capturing(ir, dst);
        }
        self.immediate_evict(ir, evict);
        self.unset_class_version_guard();
        self.unset_const_version_guard();
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
        deferred_rest: bool,
        needs_rest_array: bool,
    ) {
        // D1: skip the caller-side `create_array` only when at least
        // one forwarding consume was source-routed AND no forwarding
        // consume needs the real rest `Array`.
        let defer_rest = deferred_rest && !needs_rest_array;
        self.exec_gc(ir, true);
        let using_fpr = self.get_using_fpr(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        self.set_arguments(store, ir, callid, callee_fid, defer_rest);
        self.discard(store[callid].dst);
        self.clear_above_next_sp();
        let error = ir.new_error(self);
        let meta = store[callee_fid].meta();
        ir.push(AsmInst::SetupMethodFrame {
            meta,
            callid,
            outer_lfp: None,
        });
        ir.push(AsmInst::SpecializedCall {
            entry: inlined_entry,
            patch_point,
            evict,
        });
        ir.fpr_restore_cont(using_fpr);
        ir.handle_error(error);
        self.unset_side_effect_guard();
    }

    pub(super) fn compile_yield(&mut self, ir: &mut AsmIr, store: &Store, callid: CallSiteId) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        self.write_back_recv_and_callargs(ir, &callinfo);
        let using_fpr = self.get_using_fpr(ir);
        let error = ir.new_error(self);
        let evict = ir.new_evict();
        self.exec_gc(ir, true);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        ir.push(AsmInst::Yield {
            callid,
            error,
            evict,
        });
        ir.fpr_restore_cont(using_fpr);
        ir.handle_error(error);
        // A yielded block can capture this frame; home the result via the LFP
        // when a capture guard follows (see `def_rax2acc_capturing`). Otherwise
        // park it in a GP-pool register resident (see `def_rax2gp`).
        if self.no_capture_guard() {
            self.def_rax2gp(ir, dst);
        } else {
            self.def_rax2acc_capturing(ir, dst);
        }
        self.immediate_evict(ir, evict);
        self.unset_class_version_guard();
        self.unset_const_version_guard();
        self.unset_side_effect_guard();
    }

    fn immediate_evict(&mut self, ir: &mut AsmIr, evict: AsmEvict) {
        let next_pc = self.pc().next();
        ir.push(AsmInst::ImmediateEvict { evict });
        ir[evict] = SideExit::Evict(Some((next_pc, self.get_write_back())));
        if !self.no_capture_guard() {
            let deopt = ir.new_deopt_with_pc(self, next_pc);
            ir.guard_capture(deopt);
            self.set_no_capture_guard();
        }
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
        defer_rest: bool,
    ) {
        let callee = &store[callee_fid];
        let callsite = &store[callid];
        if store.is_simple_call(callee_fid, callid) {
            let args = callsite.args;
            let pos_num = callsite.pos_num;
            let kw_pos = callsite.kw_pos;
            let kw_num = callsite.kw_len();

            let (filled_req, filled_opt, filled_post, rest_len) = callee.apply_args(pos_num);
            let stack_offset = if self.callsite_exists_F(store, callid)
                || callee.is_rest()
                || callee.kw_rest().is_some()
            {
                callee.get_offset() as i32
            } else {
                0
            };
            ir.reg_sub(GP::Rsp, stack_offset);

            // write back block argument.
            if let Some(block_arg) = callsite.block_arg {
                self.write_back_slot(ir, block_arg);
            }

            // fill self.
            let ofs = stack_offset - LFP_SELF;
            self.fetch_for_callee(ir, callsite.recv, ofs);

            let req = filled_req.len();
            let opt = filled_opt.len();
            let post = filled_post.len();

            // fill required params.
            for i in filled_req {
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, args + i, ofs);
            }
            if req != callee.req_num() {
                for i in req..callee.req_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                }
            }

            // fill optional params.
            for i in filled_opt {
                let reg = args + req + (i - callee.req_num());
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, ofs);
            }
            if opt != callee.opt_num() {
                for i in callee.req_num() + opt..callee.reqopt_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.zero2rsp_offset(ofs);
                }
            }

            // fill a rest param.
            if callee.is_rest() {
                let ofs = stack_offset - (LFP_ARG0 + (8 * (callee.reqopt_num())) as i32);
                if defer_rest {
                    // D1: the trampoline body's forwarding consumer
                    // copied straight from these source slots, so no
                    // `Array` is built here. Spill the source range so
                    // it is memory-resident for the deopt-time
                    // materialization, and store a real `nil` into the
                    // rest slot (matches its `C(nil)` LinkMode, keeps
                    // the frame GC-safe).
                    self.write_back_range(ir, args + req + opt, rest_len as u16);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                } else {
                    self.fetch_rest_for_callee(ir, args + req + opt, rest_len, ofs);
                }
            }

            // fill post params.
            let start = filled_post.start;
            for i in filled_post {
                let reg = args + (pos_num - post) + (i - start);
                let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                self.fetch_for_callee(ir, reg, ofs);
            }
            if post != callee.post_num() {
                for i in start + post..start + callee.post_num() {
                    let ofs = stack_offset - (LFP_ARG0 + (8 * i) as i32);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                }
            }

            // fill keyword arguments
            let kw_args = &callsite.kw_args;
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

            // check unused keyword arguments.
            let mut rest_kw = vec![];
            for i in 0..kw_num {
                if !used_kw.contains(&i) {
                    let (k, v) = callsite.kw_args.get_index(i).unwrap();
                    assert_eq!(i, *v);
                    rest_kw.push((kw_pos + i, *k));
                }
            }

            // fill kw rest param.
            if let Some(kw_rest) = callee.kw_rest() {
                let ofs = stack_offset - (LFP_SELF + kw_rest.0 as i32 * 8);
                self.fetch_kwrest_for_callee(ir, rest_kw, ofs);
            }

            ir.reg_add(GP::Rsp, stack_offset);
        } else if callsite.forwarding
            && callsite.pos_num >= 1
            && callsite.splat_pos.as_slice() == [callsite.pos_num - 1]
            && callee.is_iseq().is_some()
            && callee.no_keyword()
            && !callee.is_rest()
            && callee.opt_num() == 0
            && callee.post_num() == 0
            && callee.req_num() + 1 >= callsite.pos_num
        {
            // Forwarding `g(x.., ...)` where `g` takes only required
            // positionals and the only splat is the trailing `...` rest
            // (`splat_pos == [pos_num-1]`). The `lead_num = pos_num-1`
            // leading args sit at `callsite.args ..`, the `...` Array at
            // `args + lead_num`; copy both straight into the callee frame
            // (length-guarded) instead of re-parsing via the runtime.
            // `req_num()+1 >= pos_num` ensures `req_num() >= lead_num`.
            let recv = callsite.recv;
            let args = callsite.args;
            let lead_num = callsite.pos_num - 1;
            let kwrest_guard = callsite.hash_splat_pos.first().copied();
            // D1: if `f`'s `...` rest array was deferred at frame entry,
            // route the copy straight from the caller's source slots.
            // Only when the forwarded arity exactly matches `g`'s
            // required params (this branch already guarantees `g` is
            // req-only: no opt/post/rest/kw, so no `ArgumentError`-
            // shaped case the eager `create_array` + length-guard would
            // raise) and only for the `g(*rest, **kwrest, &blk)`
            // trampoline shape (`kwrest_guard.is_some()`; the structural
            // gate guarantees no kw reaches `f`, so the forwarded
            // `**kwrest` is nil). `ir.set_deferred_rest` makes the
            // caller skip `create_array`. The annotation is NOT cleared:
            // the window's and the `g` call's own side exits must still
            // rebuild the array for an interpreter resuming inside `f`.
            let deferred_src = match self.deferred_rest_src(args + lead_num) {
                Some((src, len))
                    if (len as usize) == callee.req_num() - lead_num
                        && kwrest_guard.is_some() =>
                {
                    ir.set_deferred_rest();
                    Some((src, len))
                }
                _ => {
                    // Not source-routed (slot/arity/kwrest mismatch):
                    // this forwarding consume reads `f`'s rest slot as a
                    // real `Array`, so veto the caller-side skip.
                    if self.deferred_rest_tuple().is_some() {
                        ir.set_needs_rest_array();
                    }
                    None
                }
            };
            self.write_back_recv_and_callargs(ir, callsite);
            let error = ir.new_error(self);
            ir.push(AsmInst::SetArgumentsForwarded {
                callid,
                callee_fid,
                recv,
                args,
                lead_num,
                kwrest_guard,
                deferred_src,
            });
            ir.handle_error(error);
        } else if callsite.forwarding
            && callsite.splat_pos.len() == 1
            && callsite.splat_pos[0] < callsite.pos_num
            && callee.is_iseq().is_some()
            && callee.no_keyword()
        {
            // Forwarding with a single splat at any position — `g(x.., ...)`
            // (trailing) or implicit `super` of a `def m(a,*r,z)` method
            // (splat before post params). Handled by the specialized
            // runtime helper, which skips the generic CallSiteInfo
            // re-parse on the common no-forwarded-kw path (building
            // lead ++ splat-array ++ post directly) and delegates the
            // subtle kw case to the proven generic.
            // Array-path forwarding consume (iseq with opt/post/rest):
            // it reads `f`'s rest slot as a real `Array`.
            if self.deferred_rest_tuple().is_some() {
                ir.set_needs_rest_array();
            }
            self.write_back_recv_and_callargs(ir, callsite);
            let error = ir.new_error(self);
            ir.push(AsmInst::SetArgumentsForwardedHelper { callid, callee_fid });
            ir.handle_error(error);
        } else {
            // Generic path. A forwarding call here (e.g. native callee
            // such as `Array.new`'s `o.__send__(:initialize, ...)`, or
            // a leading-arg forward like `File.read(@path, ...)`) reads
            // `f`'s rest slot as a real `Array` via the runtime
            // `jit_generic_set_arguments`, so veto the skip.
            if callsite.forwarding && self.deferred_rest_tuple().is_some() {
                ir.set_needs_rest_array();
            }
            self.write_back_recv_and_callargs(ir, callsite);
            let error = ir.new_error(self);
            ir.push(AsmInst::SetArguments { callid, callee_fid });
            ir.handle_error(error);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// Regression test for issue #405. After JIT compilation of a block
    /// passed to `Array#map`, calls inside the block that raise and are
    /// caught by an inner `rescue` used to silently take the happy path's
    /// return value: the abstract interpreter never visited the rescue BB
    /// (no incoming edge), so the block's return state collapsed to
    /// `Const(<happy-path literal>)`, which `def_rax2acc_return` then wrote
    /// directly into the destination slot — discarding the actual `rax`
    /// produced by the rescue path.
    ///
    /// Each subtest below exercises a different "happy path" return shape
    /// (literal symbol, `nil`, different class) and confirms the rescue
    /// path's value reaches the caller after the JIT compiles the block.
    #[test]
    fn rescue_inside_map_block_returns_rescue_value() {
        run_test(
            r#"
            def boom; raise "no"; end
            def test; [1].map { begin; boom; :no_rescue; rescue; :rescued; end }; end
            30.times { test }
            test
            "#,
        );
    }

    #[test]
    fn rescue_inside_map_block_with_nil_happy_path() {
        run_test(
            r#"
            def boom; raise "no"; end
            def test; [1].map { begin; boom; nil; rescue; :rescued; end }; end
            30.times { test }
            test
            "#,
        );
    }

    #[test]
    fn rescue_inside_map_block_different_classes() {
        // Happy path returns Symbol; rescue returns String. The pre-fix
        // bug also baked in the Symbol's class, so even with a Value
        // fallback we want to confirm the actual rescue String comes back.
        run_test(
            r#"
            def boom; raise "no"; end
            def test; [1].map { begin; boom; :sym_path; rescue; "string_path"; end }; end
            30.times { test }
            test
            "#,
        );
    }

    #[test]
    fn rescue_inside_select_block_uses_rescue_value() {
        // `select` keeps elements whose block returns truthy. Pre-fix the
        // baked-in `true` from the happy path made `select` keep the
        // element even though the rescue returned `false`.
        run_test(
            r#"
            def boom; raise "no"; end
            def test; [1].select { begin; boom; true; rescue; false; end }; end
            30.times { test }
            test
            "#,
        );
    }

    #[test]
    fn rescue_inside_map_block_typeerror_dispatch() {
        // The original surfacing case from PR #404: dispatching multiple
        // values into a builtin that may raise TypeError, and rescuing the
        // TypeError inside the .map block.
        run_test(
            r#"
            def t(x)
              [x].map do |v|
                begin
                  Signal.signame(v)
                  :ok
                rescue TypeError
                  :typeerr
                end
              end
            end
            30.times { t("hello"); t(:HUP); t(nil); t(0) }
            [t("hello"), t(:HUP), t(nil), t(0)]
            "#,
        );
    }
}
