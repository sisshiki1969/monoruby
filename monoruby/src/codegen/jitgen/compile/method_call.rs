use crate::{
    codegen::jitgen::{context::JitStackFrame, state::LinkMode},
    executor::inline::InlineFuncInfo,
};

use super::{
    context::{JitArgumentInfo, JitBlockInfo},
    *,
};

///
/// Minimum share of a call site's slow-path observations a receiver class
/// must hold to earn a slot in the `GuardClassIn` membership chain: a way
/// below `1 / PMC_SET_SHARE_DIVISOR` of the total is treated as a rare tail
/// and left to deopt (see [`JitContext::pmc_same_target_classes`]).
///
/// The chain is a linear compare sequence, so every member taxes the classes
/// ahead of it on each dispatch; 1/8 keeps a genuinely alternating pair or
/// quad while rejecting the "one hot class plus a handful of stragglers"
/// shape that a way count alone cannot tell apart.
///
pub(super) const PMC_SET_SHARE_DIVISOR: u32 = 8;

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
        // A frame-dependent `super` site (the method body occupies several
        // positions in the receiver's ancestor chain, or is a define_method
        // block whose super name follows the called name) cannot be resolved
        // to one target at compile time — and the VM never warms its inline
        // cache for such sites (see `runtime::find_method`). Plain-deopt to
        // the VM, which re-resolves per call; recompiling would never
        // stabilize.
        if callsite.name.is_none() {
            let mother_fid = self.store[self.iseq().mother().0].func_id();
            let ambiguous = self.store[mother_fid].is_block_style()
                || match (recv_class, self.store[mother_fid].name()) {
                    (Some(rc), Some(name)) => {
                        self.store.super_occurrences(rc, mother_fid, name) > 1
                    }
                    _ => false,
                };
            if ambiguous {
                return Ok(CompileResult::Deopt);
            }
        }
        // A site the VM saw reaching several *different* targets: dispatch on
        // the receiver class instead of guarding it against the one class the
        // inline cache happens to hold (see `compile/pic.rs`). Only when the
        // class is not already proven — a proven class is monomorphic here
        // whatever the VM observed elsewhere.
        if recv_class.is_none() && self.compile_pic_call(state, ir, callid)? {
            return Ok(CompileResult::Continue);
        }
        let (recv_class, func_id, visibility) = if let Some(recv_class) = recv_class {
            // the receiver class is known.
            if let Some((func_id, visibility)) = self.jit_check_call(recv_class, callsite.name) {
                (recv_class, func_id, visibility)
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
                        let (func_id, visibility) =
                            if let Some(x) = self.jit_check_call(recv_class, callsite.name) {
                                x
                            } else {
                                return Ok(CompileResult::Recompile(
                                    RecompileReason::MethodNotFound,
                                ));
                            };
                        (recv_class, func_id, visibility)
                    } else {
                        // The inline method cache is valid: the VM already
                        // enforced visibility when it warmed the cache (a
                        // private non-func-call call never caches successfully),
                        // so no compile-time visibility gate is needed here.
                        (cache.recv_class, cache.func_id, Visibility::Public)
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
        self.compile_method_call(
            state, ir, recv_class, arg_class, func_id, visibility, callid, false,
        )
    }

    ///
    /// The "polymorphic but single-target" test: collect the receiver
    /// classes the VM observed at this call site (its polymorphic method
    /// cache plus the current inline-cache class — the PMC alone can miss a
    /// pair the fixnum fast path stamped and never displaced), and answer
    /// the subset that re-resolves the called name to `func_id`. Resolution
    /// is re-computed per class with `jit_check_call` under the
    /// already-emitted class version guard; the FuncId the PMC recorded is
    /// deliberately not trusted (it may predate a redefinition).
    ///
    /// The set is a **subset**, not an all-or-nothing test: `GuardClassIn`
    /// deopts on any class outside the set, so leaving a class out is always
    /// sound and never worse than the single-class guard it replaces. Two
    /// kinds of class are dropped rather than disqualifying the whole site:
    ///
    /// - one that re-resolves elsewhere (or whose resolution's visibility
    ///   would block this call site) — it cannot share this body;
    /// - one whose share of the site's observations is below
    ///   `1 / PMC_SET_SHARE_DIVISOR` — a rare tail. Every member costs a
    ///   compare on the membership chain that the hot classes pay on every
    ///   dispatch, so a class that shows up once in a hundred misses is not
    ///   worth the width. `recv_class` is exempt: the rest of the compile is
    ///   arranged for it, so a set without it would deopt on the very class
    ///   it was compiled for.
    ///
    /// A megamorphic site therefore qualifies now (the overflow counts
    /// toward the denominator, so it makes the surviving ways look *less*
    /// dominant, and the unobserved fifth-and-later classes deopt as they
    /// always did) — the previous outright rejection left those sites with a
    /// single-class guard, which is the worst of both.
    ///
    /// The classes are ordered most-observed-first so the emitted
    /// membership chain tests the hottest class first.
    ///
    /// Restricted to **native (builtin) targets**: a shared FuncId is only
    /// a shared *implementation* for those. An ISeq target's JIT code is
    /// specialized on the receiver class (its internal `self` dispatch —
    /// `def g = f` resolving `f` per subclass — is baked in), an attr
    /// accessor's ivar slot is class-layout-dependent, and a Struct
    /// reader's slot index is per-class, so for all of them the set's
    /// members must not funnel into one compiled body.
    ///
    fn pmc_same_target_classes(
        &mut self,
        callid: CallSiteId,
        recv_class: ClassId,
        func_id: FuncId,
    ) -> Option<Box<[ClassId]>> {
        if !matches!(self.store[func_id].kind, FuncKind::Builtin { .. }) {
            return None;
        }
        let callsite = &self.store[callid];
        let name = callsite.name?;
        let pmc = &callsite.pmc;
        let observations = pmc.observations();
        let mut classes: Vec<(ClassId, u32)> = pmc
            .entries()
            .iter()
            .map(|e| (e.recv, e.count))
            .collect();
        if !classes.iter().any(|(c, _)| *c == recv_class) {
            classes.push((recv_class, 0));
        }
        if classes.len() < 2 {
            // Monomorphic — the ordinary single-class guard is strictly
            // better (it refines the state for the inliners downstream).
            return None;
        }
        classes.sort_unstable_by_key(|(_, count)| std::cmp::Reverse(*count));
        let mut set = Vec::with_capacity(classes.len());
        for (class, count) in classes {
            if class != recv_class
                && count.saturating_mul(PMC_SET_SHARE_DIVISOR) < observations
            {
                // A rare tail: not worth a compare on the hot path.
                continue;
            }
            let Some((fid, visibility)) = self.jit_check_call(class, Some(name)) else {
                continue;
            };
            if fid != func_id || self.jit_visibility_blocks(callid, visibility) {
                continue;
            }
            set.push(class);
        }
        // `recv_class` resolved to `func_id` by construction, so it survives
        // the loop; guard the invariant anyway, because a set that omits it
        // would deopt on every receiver this compile was specialized for.
        if set.len() < 2 || !set.contains(&recv_class) {
            return None;
        }
        Some(set.into_boxed_slice())
    }

    ///
    /// Compile TraceIr::MethodCall with inline method cache info.
    ///
    /// *visibility* is the resolved method's visibility, obtained together with
    /// *func_id* in the same method-table probe (see `jit_check_call`), so the
    /// visibility gate below costs no extra lookup.
    ///
    #[cfg_attr(target_arch = "aarch64", allow(unused_variables))]
    pub(super) fn compile_method_call(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        recv_class: ClassId,
        arg_class: Option<ClassId>,
        func_id: FuncId,
        visibility: Visibility,
        callid: CallSiteId,
        // When the receiver-class guard misses, recompile (so the
        // site flips to the non-deopting polymorphic path) instead
        // of plain-deopting forever. Only set for monomorphic-
        // compiled BinCmp sites, which have such a path (Part B).
        recompile_on_recv_miss: bool,
    ) -> JitResult<CompileResult> {
        // CRuby method visibility. This is the single dispatch choke point for
        // operators (`#[]`, `#[]=`, arithmetic / comparison / unary) and for
        // ordinary calls whose receiver class is known at compile time, so the
        // visibility gate lives here rather than in each caller. A private
        // method reached without a func-call receiver deopts to the VM, which
        // raises `NoMethodError` (a plain `obj[i]` / `obj + x` while an
        // explicit-`self` `self[i]` / `self + x` compiles inline).
        if self.jit_visibility_blocks(callid, visibility) {
            return Ok(CompileResult::Deopt);
        }
        let callsite = &self.store[callid];
        self.inline_method_cache
            .push(InlineCacheEntry {
                recv_class,
                name: callsite.name,
                refinements: self.refinements(),
                func_id,
            });
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
        // We must write back all local vars to the stack and set the state to
        // LinkMode::S when they are possibly accessed or captured from inner
        // blocks — EXCEPT at a site qualifying for the unboxed-locals
        // speculation (doc/chain_deopt.md §5 step 5): there the pure-`F`
        // locals stay unboxed and the specialized blocks in the subtree
        // access them in this frame's FP save/spill area. Qualification
        // guarantees the site either reaches `specialized_iseq` (which arms
        // the speculation around the subtree compile) or folds the call away
        // entirely (no invocation — nothing observes the locals).
        let mut spec_set = None;
        let mut spec_qualified = false;
        if callsite.block_fid.is_some() {
            if self.float_speculation_qualifies(callsite, func_id) {
                spec_qualified = true;
                let set = state.locals_to_S_keep_F(ir);
                if !set.is_empty() {
                    // Keep the pool-resident speculated floats where the
                    // site's UsingFpr snapshot will find them: a spill
                    // between here and the call emission would silently
                    // move a local out of the save-area slot the compiled
                    // block addresses.
                    for (_, x) in &set {
                        state.pin_fpr(*x);
                    }
                    spec_set = Some(set);
                }
            } else {
                state.locals_to_S(ir);
            }
        }

        // class version guard
        self.guard_class_version(state, ir, true);

        // receiver class guard
        //
        // When the class-set guard below is taken, the receiver's class is
        // NOT statically refined (it may be any member of the set), so the
        // generator is handed `None` and decides for itself whether it can
        // still emit.
        // A set-guarded dispatch arm has already emitted the membership test
        // that admitted this receiver, and every class it admits resolves to
        // `func_id`. Emitting a receiver guard here would be redundant at
        // best and — since only one of the arm's classes is `recv_class` —
        // would deopt the rest at worst.
        let mut same_target_set_guarded = self.in_set_guarded_arm();
        if !same_target_set_guarded && state.class(recv) != Some(recv_class) {
            if !recompile_on_recv_miss
                && let Some(classes) = self.pmc_same_target_classes(callid, recv_class, func_id)
            {
                // The site is polymorphic, but every receiver class the VM
                // observed (PMC ∪ the inline cache) re-resolves — at compile
                // time, `jit_check_call` per class, never the PMC's stored
                // FuncId — to this very `func_id` (`Kernel#nil?`-,
                // `Kernel#is_a?`-style sites). One membership guard over the
                // observed set replaces the single-class guard that deopted
                // on every off-class receiver; an unobserved class still
                // deopts, and a redefinition recompiles via the class
                // version guard above.
                //
                // Every member of the set reaches the cached `func_id`
                // through this body, so every member needs its own
                // inline-cache record. `update_inline_cache` re-asks the
                // resolution question once per recorded entry when the
                // class version moves; a member missing from the map would
                // let a redefinition of *that* class pass the repair
                // untouched, and the body would keep calling the target it
                // resolved at compile time. Only `recv_class` was recorded
                // above, so add the rest.
                //
                for &class in classes.iter() {
                    if class != recv_class {
                        self.inline_method_cache.push(InlineCacheEntry {
                            recv_class: class,
                            name: callsite.name,
                            refinements: self.refinements(),
                            func_id,
                        });
                    }
                }
                let deopt = ir.new_deopt(state);
                state.load(ir, recv, GP::Rdi);
                ir.push(AsmInst::GuardClassIn(GP::Rdi, classes, deopt));
                same_target_set_guarded = true;
            } else {
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
        }

        if callsite.block_fid.is_none()
            && let Some(info) = self.store.inline_info.get_inline(func_id)
        {
            match info {
                // The generator is handed the receiver class only when the
                // site proved one: behind the class-set guard (or a
                // multi-class dispatch arm) it gets `None` and decides for
                // itself, which is how `nil?` / `frozen?` / `__id__` /
                // `object_id` keep firing there while a generator that needs
                // the class declines to the ordinary call.
                InlineFuncInfo::InlineGen(f) => {
                    let proven = (!same_target_set_guarded).then_some(recv_class);
                    if self.inline_asm(state, ir, f, callid, proven, arg_class) {
                        state.unset_side_effect_guard();
                        return Ok(CompileResult::Continue);
                    }
                }
                // The operator generators still take a definite receiver
                // class, so behind the set guard they fall through to the
                // ordinary (set-guarded) builtin call.
                _ if same_target_set_guarded => {}
                // Explicit-send spelling of a numeric operator (`1.+(2)`,
                // `a.==(b)`): fire the binary generator in Value mode. The
                // class-version and receiver guards were emitted above; the
                // generator's own operand handling emits no duplicates (its
                // guard decisions are state-driven, and the receiver's class
                // was just refined). The generators emit pure register /
                // xmm code — no C call — so no guard invalidation is needed.
                // Explicit-send spelling of a numeric unary operator
                // (`1.-@`, `x.~`): same generator, guards already emitted.
                InlineFuncInfo::InlineGenUnary(f) => {
                    if self.inline_asm_unary(state, ir, f, callid, recv_class) {
                        return Ok(CompileResult::Continue);
                    }
                    // Declined: fall through to the ordinary builtin call.
                }
                InlineFuncInfo::InlineGenBinary(f) => {
                    if let BinaryInlineOutcome::Done = self.inline_asm_binary(
                        state,
                        ir,
                        f,
                        callid,
                        recv_class,
                        arg_class,
                        BinaryInlineMode::Value,
                    ) {
                        return Ok(CompileResult::Continue);
                    }
                    // Declined: fall through to the ordinary builtin call.
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
            FuncKind::Proc(proc) => {
                // A `define_method` proc-method. Its body is a real iseq,
                // so give a monomorphic site the same treatment as a plain
                // method — hint folding and specialization — with the two
                // bmethod extras at the frame: the definition-time outer
                // LFP (baked into `SetupMethodFrame`, exactly like the
                // generic path below and the wrapper) and the proc-method
                // meta bit (pre-set on the block's static Meta at
                // `define_method` time — see `Meta::set_proc_method`).
                // Everything params-driven (`is_simple_call`,
                // `from_caller`, `set_arguments`) runs on the block fid,
                // whose ParamsInfo is the entry's verbatim copy; the
                // block-style `single_arg_expand` only makes the simple
                // gate *more* conservative, never lenient, so arity
                // semantics stay with the generic path. Sites that pass a
                // block stay generic: a block flowing into a bmethod body
                // meets `yield` resolution the specialized compile does
                // not model for proc-method frames.
                let block_iseq = self.store[proc.func_id()].is_iseq();
                if let Some(iseq) = block_iseq
                    && !self.in_dispatch_arm()
                    && block_fid.is_none()
                    && callsite.block_arg.is_none()
                    && self.store.is_simple_call(proc.func_id(), callid)
                {
                    debug_assert!(spec_set.is_none());
                    if self.store[proc.func_id()].no_keyword() && !callsite.kw_may_exists() {
                        // The same trivial-body folds as the ISeq arm: a
                        // body that returns a constant (or self) observes
                        // neither its outer environment nor its receiver,
                        // so eliding the call skips the bmethod frame
                        // setup entirely. Soundness rides on the site's
                        // class-version guard: re-`define_method` is a
                        // method (re)definition and bumps the version.
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
                    if self.specialize_level() < 5 {
                        return self.specialized_iseq(
                            state,
                            ir,
                            callid,
                            recv_class,
                            proc.func_id(),
                            iseq,
                            true,
                            None,
                            false,
                            Some(proc.outer_lfp()),
                        );
                    }
                }
                (proc.func_id(), proc.outer_lfp())
            }
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
                let simple_fold = self.store.is_simple_call(func_id, callid)
                    && self.store[func_id].no_keyword()
                    && !callsite.kw_may_exists();
                // A `...`-forwarding call site never satisfies
                // `is_simple_call` (its splat and hash-splat disqualify it),
                // so the fold above never sees the
                // `o.__builtin_initialize__(...)` inside the Ruby
                // `Class#new` — i.e. the overwhelmingly common
                // `def initialize(x); end`. When the frame's forwarded `...`
                // rest was deferred (D1) the argument shape is a
                // compile-time constant, which is all the fold needs; see
                // `forwarded_trivial_pos_num`.
                let forwarded_fold = !simple_fold
                    && self
                        .forwarded_trivial_pos_num(state, callsite)
                        .is_some_and(|n| {
                            let callee = &self.store[func_id];
                            callee.no_keyword()
                                && !callee.single_arg_expand()
                                && callee.positional_arity_ok(n)
                        });
                if simple_fold || forwarded_fold {
                    let folded = match self.store[iseq].hint {
                        ISeqHint::ConstReturn(v) => {
                            state.def_C(dst, v);
                            true
                        }
                        ISeqHint::SelfReturn => {
                            if let Some(dst) = dst {
                                state.copy_slot(ir, callsite.recv, dst);
                            }
                            true
                        }
                        ISeqHint::Normal => false,
                    };
                    if folded {
                        // Call elided — the kept-unboxed locals were never
                        // observable; just drop the speculation pins.
                        release_speculation_pins(state, &mut spec_set);
                        if forwarded_fold {
                            // Eliding the call *is* the forwarding consume:
                            // no one reads the rest `Array`, so keep the
                            // caller-side `create_array` skip on (without
                            // this the producer would materialize an Array
                            // nothing ever looks at). Deopt side exits still
                            // rebuild it from the frame's D1 annotation,
                            // which is left in place.
                            ir.set_deferred_rest();
                        }
                        return Ok(CompileResult::Continue);
                    }
                    // Frame-free expansion of the constructor idiom
                    // (`def initialize(a, b) = (@a = a; @b = b)`): emit the
                    // stores as the caller's own instructions instead of
                    // pushing a frame to run three `mov`s. Same gate as the
                    // folds above, because it needs the same thing they do —
                    // an argument shape that binds without `ArgumentError`.
                    if let Some(body) = frameless::ivar_store_body(&self.store, iseq) {
                        let callee_pos = self.store[func_id].params().total_positional_args();
                        // Where each callee parameter lives in *this* frame.
                        // Direct call sites hand them over contiguously from
                        // `args`; a `...` forward splits them into the lead
                        // positionals plus the D1-deferred rest range, which
                        // is where `Class#new`'s `__builtin_initialize__(...)`
                        // — the shape that actually matters — lands.
                        let arg_slots: Option<Vec<frameless::ArgSlot>> = if simple_fold {
                            (pos_num == callee_pos).then(|| {
                                (0..callee_pos)
                                    .map(|i| frameless::ArgSlot::Own(args + i))
                                    .collect()
                            })
                        } else {
                            let lead_num = pos_num - 1;
                            state
                                .deferred_rest_src(args + lead_num)
                                .filter(|(_, len)| lead_num + *len as usize == callee_pos)
                                .map(|(rest, len)| {
                                    (0..lead_num)
                                        .map(|i| frameless::ArgSlot::Own(args + i))
                                        .chain(
                                            (0..len as usize)
                                                .map(|i| frameless::ArgSlot::Caller(rest + i)),
                                        )
                                        .collect()
                                })
                        };
                        if let Some(arg_slots) = arg_slots
                            && self.expand_ivar_stores(
                                state, ir, recv_class, recv, dst, &body, &arg_slots,
                            )
                        {
                            // Frame-free expansion — same as the fold above.
                            release_speculation_pins(state, &mut spec_set);
                            if forwarded_fold {
                                // Same reasoning as the fold above: the
                                // expansion *is* the forwarding consume, so
                                // keep the caller-side `create_array` skip on.
                                ir.set_deferred_rest();
                            }
                            return Ok(CompileResult::Continue);
                        }
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
                // The forwarded `initialize` inside the Ruby `Class#new`
                // (the privileged `recv.__builtin_initialize__(...)`
                // spelling, marked `bypass_visibility`): specialize the
                // callee for the statically-known allocated class even
                // though the forwarding splat keeps the call site
                // non-simple. Argument link-modes are NOT propagated
                // (`specializable` stays false, so `specialized_iseq`
                // uses `JitArgumentInfo::default()`); the arguments flow
                // through the same forwarded `set_arguments` (D1
                // source-routed when the trampoline's rest is deferred).
                // This turns construction into
                // allocate + direct `SpecializedCall` into an
                // initialize compiled for exactly that class.
                let forwarded_initialize = callsite.forwarding && callsite.bypass_visibility;

                // Method specialization (inlining a callee iseq) and block-
                // argument inlining (`iseq_block`, which drives specialized
                // `yield`) are both lowered on x86 and aarch64 now.
                // Inside a dispatch arm, specialization is off: the arm
                // cannot back out of a `CompileError`, and a `Cease` return
                // would leave it with no path to the merge.
                if (((specializable || forwarded_initialize) && self.specialize_level() < 5)
                    || iseq_block.is_some())
                    && !self.in_dispatch_arm()
                {
                    return self.specialized_iseq(
                        state,
                        ir,
                        callid,
                        recv_class,
                        func_id,
                        iseq,
                        specializable,
                        spec_set,
                        spec_qualified,
                        None,
                    );
                }
                debug_assert!(spec_set.is_none());
                (func_id, None)
            }
        };

        if block_fid.is_some() {
            state.unset_no_capture_guard(self);
        }

        // A generic dispatch that passes a block hands a handler to code
        // the unboxed-locals speculation cannot see through (the callee —
        // a builtin iterator, a proc, a super target — may run the block
        // generically, i.e. against LFP slots, or materialize it): poison
        // every armed speculation so its site recompiles unspeculated.
        if block_fid.is_some() || self.store[callid].block_arg.is_some() {
            self.poison_float_speculations();
        }

        state.send(ir, &self.store, callid, fid, recv_class, outer_lfp);

        Ok(CompileResult::Continue)
    }

    ///
    /// §7 gating (doc/chain_deopt.md) for the unboxed-locals speculation at
    /// a block-passing call site: the callee is a plain iseq — so the site
    /// provably reaches `specialized_iseq` (which arms the speculation) or
    /// folds the call away (no invocation observes the locals) — and the
    /// literal block is an iseq that can capture neither its own frame nor,
    /// through the outer chain, this one. Everything subtler (a nested
    /// generic block-passing call, a proxy materialization, a no-capture
    /// invalidation) is caught by the poison hooks during the subtree
    /// compile and triggers the unspeculated recompile.
    ///
    fn float_speculation_qualifies(&self, callsite: &CallSiteInfo, func_id: FuncId) -> bool {
        if self.in_dispatch_arm() {
            return false;
        }
        let Some(bfid) = callsite.block_fid else {
            return false;
        };
        if !matches!(self.store[func_id].kind, FuncKind::ISeq(_)) {
            return false;
        }
        let Some(biseq) = self.store[bfid].is_iseq() else {
            return false;
        };
        !self.store[bfid].possibly_capture_without_block() && !self.store[biseq].has_block_arg()
    }

    ///
    /// The statically-known positional-argument count of a `...`-forwarding
    /// call site, or `None` when the shape is not a compile-time constant.
    ///
    /// This is the trivial-method fold's (`ISeqHint`) entry for forwarding
    /// call sites, which `is_simple_call` rejects out of hand. It leans
    /// entirely on the D1 deferral annotation: `forward_rest_deferral` only
    /// annotates a *pure forwarding trampoline* (`def f(...) = g(...)`,
    /// single basic block) whose own caller passes no keywords and no `&blk`
    /// through a simple (splat-free) call site. So for an annotated frame:
    ///
    /// * the forwarded positional count is exactly the caller's `pos_num`
    ///   (`len` below), and
    /// * `f`'s `**kwrest` local is statically nil — the `...` forward's
    ///   hash-splat contributes no keywords, which is what lets the fold
    ///   ignore `kw_may_exists` (true here only because of that nil splat).
    ///
    /// The consuming site must therefore add no keyword of its own
    /// (`def f(...) = g(k: 1, ...)`) and no second hash-splat
    /// (`def f(...) = g(**h, ...)`), and its only splat must be the trailing
    /// `...` rest. The *callee*-side conditions (no keyword surface, arity
    /// binds without `ArgumentError`) are checked by the caller of this fn.
    ///
    fn forwarded_trivial_pos_num(
        &self,
        state: &AbstractState,
        callsite: &CallSiteInfo,
    ) -> Option<usize> {
        if !callsite.forwarding || callsite.pos_num == 0 {
            return None;
        }
        if callsite.splat_pos.as_slice() != [callsite.pos_num - 1]
            || !callsite.kw_args.is_empty()
            || callsite.hash_splat_pos.len() != 1
        {
            return None;
        }
        let lead_num = callsite.pos_num - 1;
        let (_, len) = state.deferred_rest_src(callsite.args + lead_num)?;
        Some(lead_num + len as usize)
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
            false,
        )?;
        // Stack check only: the specialized block body compiles its own
        // `InitMethod` entry poll, so no call-site GC poll is needed.
        state.check_stack(ir);
        let using_fpr = state.get_using_fpr(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        ir.push(AsmInst::ContFramePc {
            call_site_pc: state.pc().as_ptr() as u64,
        });
        state.set_arguments(&self.store, ir, callid, callee_fid, false);
        state.discard(dst);
        state.clear_above_next_sp();
        let error = ir.new_error(state);
        let evict = ir.new_evict();
        let meta = self.store[callee_fid].meta();
        ir.push(AsmInst::SetupYieldFrame { meta, outer });
        ir.push(AsmInst::SpecializedYield { entry, evict });
        state.chain_exit(ir, evict, using_fpr, dst);
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
        // A provably-immediate stored value needs no GC write barrier.
        let wb = !state.is_guarded_immediate(args);
        let src = state.load_or_reg(ir, args, GP::Rax);
        let is_object_ty = self.store[recv_class].is_object_ty_instance();
        let using_fpr = state.get_using_fpr(ir);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid, wb })
        } else {
            ir.push(AsmInst::StoreIVarHeap {
                src,
                ivarid,
                using_fpr,
                is_object_ty,
                wb,
            });
        }
        state.def_rax2acc(ir, dst);
        state.unset_side_effect_guard();
        CompileResult::Continue
    }

    ///
    /// Emit a recognised constructor body ([`frameless::ivar_store_body`])
    /// as the caller's own instructions — no frame pushed, no call made.
    ///
    /// `arg_slots[i]` is the caller slot supplying the callee's parameter
    /// `i`. Returns `false` when the receiver class cannot take the stores
    /// this way, in which case nothing has been emitted and the caller
    /// falls through to the ordinary call.
    ///
    /// # Why the frozen guard is hoisted
    ///
    /// Storing to a frozen object must raise `FrozenError`, and the raise
    /// has to come from a real `initialize` frame with the right backtrace
    /// — which is exactly the frame this path does not build. So the guard
    /// runs **before any store**, and a frozen receiver deopts to the call
    /// instruction: the VM then performs the whole call itself and raises
    /// properly. That is only sound while no store has happened yet, which
    /// is why the body must be straight-line (a conditional store would
    /// leave the guard proving something about a path that never stores).
    ///
    fn expand_ivar_stores(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        recv_class: ClassId,
        recv: SlotId,
        dst: Option<SlotId>,
        body: &frameless::IvarStoreBody,
        arg_slots: &[frameless::ArgSlot],
    ) -> bool {
        // Only `RValue`s with the object layout have inline ivar slots at a
        // fixed offset; anything else stores through the heap table, which
        // needs `using_fpr` bookkeeping and a possible reallocation call.
        if !self.store[recv_class].is_object_ty_instance() {
            return false;
        }
        // Resolve every slot *before* emitting anything, so a body that is
        // only partly expandable emits nothing at all.
        let mut plan = Vec::with_capacity(body.stores.len());
        for &(name, param) in &body.stores {
            // The ivar id is created by the first execution of this store,
            // and `initialize` has necessarily run in the interpreter to get
            // the caller this hot — so a miss here means the class reaching
            // this site is not the one the body writes (a subclass whose own
            // `new` has never run). Decline rather than recompile: the
            // ordinary call is correct and this is only an optimization.
            let Some(ivarid) = self.store[recv_class].get_ivarid(name) else {
                return false;
            };
            if !ivarid.is_inline() {
                return false;
            }
            plan.push((ivarid, arg_slots[param as usize]));
        }
        state.load(ir, recv, GP::Rdi);
        let deopt = ir.new_deopt(state);
        ir.guard_frozen(deopt);
        for (ivarid, src_slot) in plan {
            // A caller-frame slot has no abstract-state proof here, so it
            // keeps the barrier; an own slot elides it when the state
            // proves the value immediate.
            let (src, wb) = match src_slot {
                frameless::ArgSlot::Own(slot) => (
                    state.load_or_reg(ir, slot, GP::Rax),
                    !state.is_guarded_immediate(slot),
                ),
                frameless::ArgSlot::Caller(slot) => {
                    ir.push(AsmInst::LoadCallerSlot {
                        slot,
                        dst: GP::Rax,
                    });
                    (GP::Rax, true)
                }
            };
            // Re-materialize the base: an argument living in an FP register
            // is boxed on the way out, and boxing is a call.
            state.load(ir, recv, GP::Rdi);
            ir.push(AsmInst::StoreIVarInline { src, ivarid, wb });
        }
        if let Some(dst) = dst {
            // The body returns its last assignment's RHS.
            match arg_slots[body.ret as usize] {
                // Copying the slot (rather than reading it back) keeps an
                // unboxed float unboxed.
                frameless::ArgSlot::Own(slot) => state.copy_slot(ir, slot, dst),
                frameless::ArgSlot::Caller(slot) => {
                    ir.push(AsmInst::LoadCallerSlot {
                        slot,
                        dst: GP::Rax,
                    });
                    state.def_rax2acc(ir, Some(dst));
                }
            }
        }
        state.unset_side_effect_guard();
        true
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
        mut spec_set: Option<Vec<(SlotId, crate::codegen::FPReg)>>,
        spec_qualified: bool,
        // `Some` marks a define_method proc-method callee: the
        // definition-time outer LFP to bake into `SetupMethodFrame`
        // (`None` inside the option is impossible to distinguish from a
        // plain method here, so the whole option is the bmethod marker —
        // a bmethod defined at toplevel still carries its outer).
        bmethod_outer: Option<Option<Lfp>>,
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
        // Unboxed-locals speculation (doc/chain_deopt.md §5 steps 4–5):
        // arm the caller's kept-`F` set around the subtree compile so the
        // specialized blocks inside route their accesses to those locals
        // through this frame's FP save/spill area. If the subtree turns
        // out to contain something the speculation cannot see through
        // (poison), discard it, box the kept locals after all, and
        // recompile the subtree unspeculated — the orphaned first attempt
        // is emitted but never referenced.
        if let Some(set) = &spec_set {
            self.begin_float_speculation(set.clone(), state.using_fpr_offset());
        }
        let capture_events0 = self.capture_events();
        let mut used_patch_point = patch_point;
        let mut compiled = self.compile_specialized_func(
            state,
            iseq,
            recv_class,
            used_patch_point,
            args_info.clone(),
            None,
            callid,
            bmethod_outer.is_some(),
        )?;
        if spec_set.is_some() && self.end_float_speculation() {
            release_speculation_pins(state, &mut spec_set);
            state.locals_to_S(ir);
            used_patch_point = patch_point.map(|_| self.label());
            compiled = self.compile_specialized_func(
                state,
                iseq,
                recv_class,
                used_patch_point,
                args_info,
                None,
                callid,
                bmethod_outer.is_some(),
            )?;
        }
        // A qualified site whose subtree compiled without a single
        // capture-relevant event provably cannot capture this frame.
        let clean_speculation = spec_qualified && self.capture_events() == capture_events0;
        let SpecializedCompileResult {
            entry,
            return_state,
            deferred_rest,
            needs_rest_array,
        } = compiled;
        // The call site passes a block literal: if the callee heapifies
        // its *own* frame during the call (`Proc.new` / `lambda` /
        // `binding`), `materialize_escaped_block_handlers` turns the
        // passed block handler into a Proc — whose home is THIS frame —
        // and promotes this frame to the heap as well. Mirror the
        // generic-send rule (see `compile_method_call`): drop the
        // no-capture invariant so the result store below goes via the
        // LFP and `immediate_evict` emits a capture guard.
        //
        // A *cleanly speculated* subtree is the exception: its gating and
        // poison hooks proved no path in the subtree can materialize a
        // block handler (a generic block-passing site, a generic yield, or
        // a proxy materialization would have poisoned it), so the
        // invariant survives — which is also what lets an enclosing
        // frame's own speculation nest across this site instead of being
        // poisoned by the blanket unset.
        if self.store[callid].block_fid.is_some() && !clean_speculation {
            state.unset_no_capture_guard(self);
        }
        let evict = ir.new_evict();
        state.send_specialized(
            ir,
            &self.store,
            callid,
            fid,
            entry,
            used_patch_point,
            evict,
            deferred_rest,
            needs_rest_array,
            bmethod_outer,
        );
        let res = state.def_rax2acc_return(ir, dst, return_state);
        state.immediate_evict(ir, evict);
        release_speculation_pins(state, &mut spec_set);
        return Ok(res);
    }
}

///
/// Drop the fpr pins that held a speculation set's pool floats in place
/// (see the arming site in `compile_method_call`). Idempotent via `take`.
///
fn release_speculation_pins(
    state: &mut AbstractState,
    spec_set: &mut Option<Vec<(SlotId, crate::codegen::FPReg)>>,
) {
    if let Some(set) = spec_set.take() {
        for (_, x) in set {
            state.unpin_fpr(x);
        }
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
        bmethod: bool,
    ) -> JitResult<SpecializedCompileResult> {
        let mut frame = self.new_specialized_frame(iseq_id, outer, args_info, self_class);
        if bmethod {
            // A define_method proc-method body: semantically a METHOD
            // frame even though its iseq is a block — `return` targets
            // this frame itself (lambda-style), and `Lfp::outermost`
            // stops here. Marking it not-a-block makes
            // `current_method_frame` resolve `return` to this frame, so
            // `MethodRet` compiles to the empty-chain static teardown (a
            // plain epilogue) and the value joins the caller's return
            // context like an ordinary specialized return.
            frame.set_bmethod_home();
        }

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
        // `has_exception_handler` taints the return state so the caller
        // doesn't propagate a speculative `Const` past us: the BB graph
        // doesn't include rescue/ensure successors, so the computed
        // return state only reflects the happy path — and an `ensure`
        // runs on the happy path too, without any side exit in between.
        // See issue #405.
        //
        // `frame_had_deopt` used to taint as well (a deopt-able side
        // exit means the runtime can resume in the interpreter from the
        // deopt PC and produce a different rax than the abstract
        // interpreter predicted — e.g. `Array#assoc`'s block doing
        // `return elem` after the recv-class guard fails, PR #505).
        // With side-exit escalation unconditional (doc/chain_deopt.md
        // §8.6) that widening is no longer needed: every deopt or error
        // exit taken anywhere under this call walks the chain and
        // converts the caller — its return-address slot is rewritten to
        // the VM continuation stub, so the compiled continuation that
        // consumed this return state never runs on a deopt path. The
        // continuation executes only when the callee completed on the
        // compiled happy path, which is exactly what the abstract
        // return state describes. This is sound because every call and
        // yield site registers for chain conversion unconditionally
        // (`chain_exit`; the walk skips unregistered return addresses,
        // so registration is load-bearing here).
        let return_state = return_state.map(|mut s| {
            if self.store[iseq_id].has_exception_handler() {
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
        let speculated = self.under_armed_speculation();
        self.specialized_methods_push(context::SpecializeInfo {
            entry,
            info: frame.asm_info,
            patch_point,
            speculated,
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

    pub(super) fn inline_asm(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        f: impl Fn(
            &mut AbstractState,
            &mut AsmIr,
            &JitContext,
            &Store,
            CallSiteId,
            Option<ClassId>,
            Option<ClassId>,
        ) -> bool,
        callid: CallSiteId,
        // `None` when the call site could not prove the receiver's class —
        // see `InlineGen`.
        recv_class: Option<ClassId>,
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

    /// [`inline_asm`](Self::inline_asm) for binary-operator generators —
    /// same transactional save/restore protocol, with the firing mode passed
    /// through and the three-way [`BinaryInlineOutcome`] returned (`Declined`
    /// rolls back).
    pub(super) fn inline_asm_binary(
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
            BinaryInlineMode,
        ) -> BinaryInlineOutcome,
        callid: CallSiteId,
        recv_class: ClassId,
        arg_class: Option<ClassId>,
        mode: BinaryInlineMode,
    ) -> BinaryInlineOutcome {
        let state_save = state.clone();
        let ir_save = ir.save();
        match f(state, ir, self, &self.store, callid, recv_class, arg_class, mode) {
            BinaryInlineOutcome::Declined => {
                *state = state_save;
                ir.restore(ir_save);
                BinaryInlineOutcome::Declined
            }
            outcome => outcome,
        }
    }

    /// [`inline_asm`](Self::inline_asm) for unary-operator generators —
    /// same transactional save/restore protocol, with the receiver class
    /// passed through so a generator registered on a shared ancestor
    /// (`Numeric#+@`) can decline for receivers it does not cover.
    pub(super) fn inline_asm_unary(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        f: impl Fn(&mut AbstractState, &mut AsmIr, &JitContext, &Store, CallSiteId, ClassId) -> bool,
        callid: CallSiteId,
        recv_class: ClassId,
    ) -> bool {
        let state_save = state.clone();
        let ir_save = ir.save();
        if f(state, ir, self, &self.store, callid, recv_class) {
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
        // Stack check only — no call-site GC/preempt poll. Ruby callees
        // poll at their entry (`InitMethod` / `vm_init`); native callees
        // are bounded between the caller's loop-edge/entry polls.
        self.check_stack(ir);
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
        ir.push(AsmInst::ContFramePc {
            call_site_pc: self.pc().as_ptr() as u64,
        });
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
        self.chain_exit(ir, evict, using_fpr, dst);
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
        bmethod_outer: Option<Option<Lfp>>,
    ) {
        // D1: skip the caller-side `create_array` only when at least
        // one forwarding consume was source-routed AND no forwarding
        // consume needs the real rest `Array`.
        let defer_rest = deferred_rest && !needs_rest_array;
        // Stack check only: the specialized callee body compiles its own
        // `InitMethod` entry poll.
        self.check_stack(ir);
        let using_fpr = self.get_using_fpr(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        ir.push(AsmInst::ContFramePc {
            call_site_pc: self.pc().as_ptr() as u64,
        });
        self.set_arguments(store, ir, callid, callee_fid, defer_rest);
        self.discard(store[callid].dst);
        self.clear_above_next_sp();
        let error = ir.new_error(self);
        let meta = store[callee_fid].meta();
        // A bmethod frame gets its definition-time outer LFP; the
        // proc-method meta bit is already static on the block's Meta.
        ir.push(AsmInst::SetupMethodFrame {
            meta,
            callid,
            outer_lfp: bmethod_outer.flatten(),
        });
        ir.push(AsmInst::SpecializedCall {
            entry: inlined_entry,
            patch_point,
            evict,
        });
        self.chain_exit(ir, evict, using_fpr, store[callid].dst);
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
        // Stack check only: the block body polls at its entry
        // (`InitMethod` / `vm_init`) on every yield.
        self.check_stack(ir);
        // stack pointer adjustment
        // -using_fpr.offset()
        ir.fpr_save_cont(using_fpr);
        ir.push(AsmInst::ContFramePc {
            call_site_pc: self.pc().as_ptr() as u64,
        });
        // A statically simple call site (plain positional arguments) can
        // hand the values to the block via the direct-copy path; the
        // callee side stays dynamic (see
        // `jit_handle_arguments_no_block_for_yield`).
        let simple = callinfo.splat_pos.is_empty()
            && callinfo.kw_args.is_empty()
            && callinfo.hash_splat_pos.is_empty()
            && !callinfo.forwarding
            && callinfo.block_fid.is_none()
            && callinfo.block_arg.is_none();
        ir.push(AsmInst::Yield {
            callid,
            simple,
            error,
            evict,
        });
        self.chain_exit(ir, evict, using_fpr, dst);
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

    ///
    /// Register this call site for chain deopt (`doc/chain_deopt.md` §2/§9.3),
    /// so the walk can convert a frame suspended here into an interpreter
    /// frame: replay its write-back from Rust and rewrite the callee's
    /// return-address slot to the shared VM continuation stub.
    ///
    /// Call this immediately after the site's call instruction: the write-back
    /// is read off the live state, which must still be the post-`discard(dst)`
    /// state the post-call continuation expects (it stores the result into
    /// `dst` itself).
    ///
    /// Registration is unconditional: chain conversion is now the *only* way
    /// an on-stack JIT frame is dropped to the interpreter, so every call and
    /// yield site must be convertible from its return address alone. A site
    /// the table does not know is a site the walk has to leave running its
    /// compiled body — which was tolerable only while immediate eviction
    /// existed as a fallback. When the speculation (§5 step 5) lands, the
    /// per-site decision §6 argues for rides on top of this, not instead
    /// of it.
    ///
    fn chain_exit(&self, ir: &mut AsmIr, evict: AsmEvict, using_fpr: UsingFpr, dst: Option<SlotId>) {
        let spec = Box::new(ChainExitSpec::new(self, using_fpr, dst));
        ir.push(AsmInst::ChainExit { evict, spec });
    }

    /// Post-call bookkeeping: fill in the `Evict` side-exit slot this site
    /// reserved, and re-arm the capture guard.
    ///
    /// The name is historical. The `Evict` handler used to be *entered* by
    /// immediate eviction, which overwrote this site's return continuation
    /// with a `jmp` to it; that mechanism is gone (chain conversion is the
    /// only way a suspended frame is dropped to the interpreter), so nothing
    /// branches to the handler any more. The slot is still filled because
    /// `AsmEvict` is the id under which `chain_exit` finds this call's
    /// return address, and `gen_asm` requires every reserved slot to carry a
    /// write-back.
    fn immediate_evict(&mut self, ir: &mut AsmIr, evict: AsmEvict) {
        let next_pc = self.pc().next();
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
    /// K1: the per-callee-kw-param caller source slots when this frame's
    /// deferred literal keywords statically bind to `callee`'s keyword
    /// declaration: every passed name declared, every required name
    /// passed, and no `**kwrest` on the callee (a leftover hash would
    /// need building). `route[i]` feeds callee kw param `i` (at
    /// `kw_reg_pos() + i`); `None` 0-fills an absent optional keyword so
    /// the callee prologue runs its default.
    fn kw_forward_route(
        &self,
        _callsite: &CallSiteInfo,
        callee: &FuncInfo,
    ) -> Option<Box<[Option<SlotId>]>> {
        let df = self.deferred_forward_info()?;
        if callee.kw_rest().is_some() {
            return None;
        }
        let kw_names = callee.kw_names();
        let Some((_, kw_pos, names)) = df.kw.as_ref() else {
            // The caller passed no keywords at all: routable iff every
            // callee keyword is optional — an all-None route 0-fills
            // them so their defaults run. (A required keyword must
            // raise, which only the generic path does.)
            if (0..kw_names.len()).any(|i| callee.kw_is_required(i)) {
                return None;
            }
            return Some(vec![None; kw_names.len()].into_boxed_slice());
        };
        if !names.iter().all(|n| kw_names.contains(n)) {
            return None;
        }
        let route: Box<[Option<SlotId>]> = kw_names
            .iter()
            .map(|pn| names.iter().position(|n| n == pn).map(|idx| *kw_pos + idx))
            .collect();
        for (i, r) in route.iter().enumerate() {
            if callee.kw_is_required(i) && r.is_none() {
                return None;
            }
        }
        Some(route)
    }

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
                if defer_rest && !rest_kw.is_empty() {
                    // K1: the specialized trampoline body source-routed
                    // these literal keywords straight from our slots;
                    // store the same GC-safe `nil` the deferred rest
                    // gets. Spill the kw window first so it is
                    // memory-resident for the routed reads and for a
                    // deopt-time Hash rebuild (`forward_kwrest`
                    // write-back), exactly as the deferred rest spills
                    // its positional window.
                    self.write_back_range(ir, kw_pos, kw_num as u16);
                    ir.u64torsp_offset(NIL_VALUE, ofs);
                } else {
                    self.fetch_kwrest_for_callee(ir, rest_kw, ofs);
                }
            }

            ir.reg_add(GP::Rsp, stack_offset);
        } else if callsite.pos_num == 1
            && callee.single_arg_expand()
            && callee.meta().is_simple()
            && callee.post_num() == 0
            && !callsite.has_splat()
            && !callsite.has_hash_splat()
            && !callsite.kw_may_exists()
            && callsite.block_arg.is_none()
        {
            // `yield v` into a plain multi-parameter block (`h.each { |k, v| .. }`
            // receiving one `[k, v]` pair): block-style single-Array auto-splat.
            // `single_arg_expand` makes `is_simple_call` false, so without this
            // arm every such yield pays the generic runtime-call binding — and
            // this is the argument shape every `each`-style Ruby builtin yields
            // on every element.
            //
            // The lowering peels the by-far-common case in line: when the value
            // is an Array (by ty, as the runtime's `check_single_arg_expand`
            // decides it), its elements fill the parameters directly —
            // nil-filled past the end, extras dropped, block-style loose
            // binding. Anything else — a non-Array (whose `#to_ary` may run
            // arbitrary code) — branches to the same generic runtime helper the
            // arm below uses, so semantics never depend on the fast path.
            let req_num = callee.req_num();
            self.write_back_recv_and_callargs(ir, callsite);
            self.load(ir, callsite.args, GP::Rdi);
            let error = ir.new_error(self);
            ir.push(AsmInst::YieldArrayExpand {
                callid,
                callee_fid,
                req_num,
            });
            ir.handle_error(error);
        } else if callsite.forwarding
            && callsite.pos_num >= 1
            && callsite.splat_pos.as_slice() == [callsite.pos_num - 1]
            && callee.post_num() == 0
            && callee.reqopt_num() + 1 >= callsite.pos_num
            // A bare `**kwrest` is fine — the lowering stores `nil` into
            // that slot, exactly as the runtime's `store_empty_kw_rest`
            // does. Requiring the call site to carry keyword syntax is
            // what preserves ruby2_keywords (see `forwarded_fast_path_ok`
            // in runtime/args.rs, which this mirrors); a `...` forward
            // always carries the `**kwrest` hash-splat.
            //
            // K1: a callee that *declares* keywords is also allowed when
            // the frame's deferred literal keywords statically bind to
            // that declaration (`kw_forward_route`); the deferred match
            // below couples the routing with the rest source-routing.
            && (callee.no_keyword()
                || (callee.kw_names().is_empty() && callsite.kw_may_exists())
                || (callsite.kw_may_exists()
                    && self.kw_forward_route(callsite, callee).is_some()))
            // A block-style callee auto-splats a lone Array argument
            // (`single_arg_expand`); the direct fills below do not model
            // that, so leave those to the generic path — matching the
            // runtime's own fast-path gate.
            && !callee.single_arg_expand()
            // An *implicit* rest (`|a,|`) does not accept extra args, so
            // the "surplus goes to the rest" reasoning below needs an
            // explicit `*rest`.
            && (!callee.is_rest() || callee.is_explicit_rest())
        {
            // Forwarding `g(x.., ...)` where `g` takes only required (and
            // possibly optional) positionals and the only splat is the
            // trailing `...` rest (`splat_pos == [pos_num-1]`). Applies
            // to iseq and native callees alike — a native's callee-frame
            // protocol is identical (fixed slots, None for an absent
            // optional; `Class#new`'s forward to a native `initialize`
            // such as `BasicObject#initialize` lands here). The
            // `lead_num = pos_num-1` leading args sit at `callsite.args ..`,
            // the `...` Array at `args + lead_num`; copy both straight into
            // the callee frame instead of re-parsing via the runtime.
            // `reqopt_num()+1 >= pos_num` ensures `reqopt_num() >= lead_num`.
            let recv = callsite.recv;
            let args = callsite.args;
            let lead_num = callsite.pos_num - 1;
            let kwrest_guard = callsite.hash_splat_pos.first().copied();
            // K1: the deferred literal keywords' static binding to the
            // callee declaration (None when the frame defers no keywords
            // or they don't bind; the arm gate only admits a
            // kw-declaring callee in the bound case).
            let kw_route = self.kw_forward_route(callsite, callee);
            // A deferral carrying keywords routes them together with the
            // rest window or not at all: the routed reads go straight to
            // the caller frame, which is only sound while the caller-side
            // skip (one flag covers the array and the hash) is in force.
            // The routed keywords must come through this callsite's own
            // `**kwrest` hash-splat slot.
            let deferred_kw_ok =
                match self.deferred_forward_info().and_then(|df| df.kw.as_ref()) {
                    None => true,
                    Some((kwrest_local, _, _)) => {
                        kw_route.is_some() && kwrest_guard == Some(*kwrest_local)
                    }
                };
            // D1: if `f`'s `...` rest array was deferred at frame entry,
            // route the copy straight from the caller's source slots.
            // Only when the forwarded arity statically binds to `g`'s
            // positional params — `req <= lead+len`, no post, and the
            // surplus over `req+opt` either absorbed by an explicit
            // `*rest` or absent — so the whole fill layout (copied slots,
            // None-filled optionals whose defaults the callee prologue
            // runs, and the rest `Array`'s contiguous source window) is a
            // compile-time constant and no `ArgumentError`-shaped case
            // remains. Also only for the `g(*rest, **kwrest, &blk)`
            // trampoline shape (`kwrest_guard.is_some()`; the structural
            // gate guarantees no kw reaches `f`, so the forwarded
            // `**kwrest` is nil). `ir.set_deferred_rest` makes the
            // caller skip `create_array`. The annotation is NOT cleared:
            // the window's and the `g` call's own side exits must still
            // rebuild the array for an interpreter resuming inside `f`.
            let deferred_src = match self.deferred_rest_src(args + lead_num) {
                Some((src, len))
                    if {
                        let n = lead_num + len as usize;
                        callee.req_num() <= n
                            && (callee.is_rest() || n <= callee.reqopt_num())
                    } && kwrest_guard.is_some()
                        && deferred_kw_ok =>
                {
                    ir.set_deferred_rest();
                    Some((src, len))
                }
                _ => {
                    // Not source-routed (slot/arity/kwrest mismatch):
                    // this forwarding consume reads `f`'s rest slot as a
                    // real `Array`, so veto the caller-side skip.
                    if self.deferred_forward_info().is_some() {
                        ir.set_needs_rest_array();
                    }
                    None
                }
            };
            self.write_back_recv_and_callargs(ir, callsite);
            if deferred_src.is_some() {
                // D1/K1 cannot fail: the gate proved the fill layout is a
                // compile-time constant, so the lowering has no length
                // guard, no fallback, and no call that can raise. Emit it
                // without an error side exit — a `HandleError` here would
                // test a constant-`nil` sentinel against a handler nothing
                // can ever branch to.
                ir.push(AsmInst::SetArgumentsForwarded {
                    callid,
                    callee_fid,
                    recv,
                    args,
                    lead_num,
                    kwrest_guard,
                    deferred_src,
                    kw_route,
                });
            } else if !callee.kw_names().is_empty() {
                // K1 admitted a kw-declaring callee but the deferral did
                // not activate: the keywords live in the real kwrest
                // Hash, which only the generic path binds.
                let error = ir.new_error(self);
                ir.push(AsmInst::SetArguments { callid, callee_fid });
                ir.handle_error(error);
            } else {
                let error = ir.new_error(self);
                if callee.opt_num() != 0 || callee.is_rest() || !callee.no_keyword() {
                    // Eager (the `...` Array really was materialized): the
                    // bind length is only known at run time, so anything but
                    // a plain req-only callee — optional params, a `*rest`
                    // to size, or a `**kwrest` slot to initialize — keeps the
                    // proven specialized runtime helper. The inline fast path
                    // below guards on an exact length and would have to
                    // re-derive all of that at run time.
                    ir.push(AsmInst::SetArgumentsForwardedHelper { callid, callee_fid });
                } else {
                    ir.push(AsmInst::SetArgumentsForwarded {
                        callid,
                        callee_fid,
                        recv,
                        args,
                        lead_num,
                        kwrest_guard,
                        deferred_src,
                        kw_route: None,
                    });
                }
                ir.handle_error(error);
            }
        } else if callsite.forwarding
            && callsite.splat_pos.len() == 1
            && callsite.splat_pos[0] < callsite.pos_num
            && (callee.no_keyword()
                || (callee.kw_names().is_empty() && callsite.kw_may_exists()))
        {
            // Forwarding with a single splat at any position — `g(x.., ...)`
            // (trailing) or implicit `super` of a `def m(a,*r,z)` method
            // (splat before post params). Handled by the specialized
            // runtime helper, which skips the generic CallSiteInfo
            // re-parse on the common no-forwarded-kw path (building
            // lead ++ splat-array ++ post directly) and delegates the
            // subtle kw case to the proven generic. Native callees share
            // the same callee-frame protocol (rest natives get their rest
            // Array materialized by the same `fill_positional_args`).
            //
            // A callee whose only keyword surface is a bare `**kwrest`
            // (no declared keyword names) is admitted too, *provided the
            // call site itself carries keyword syntax*: the helper's
            // runtime core then checks whether keywords are actually
            // being forwarded and stores `nil` into the kwrest slot when
            // they are not — the same value the generic path would
            // write. `Struct#initialize` is exactly this shape (rest +
            // kwrest, for `keyword_init:`) and a `...` forward always
            // carries the `**kwrest` hash-splat, so this is what keeps
            // `S.new(...)` off the generic re-parse.
            //
            // The `kw_may_exists()` requirement is what keeps
            // ruby2_keywords correct: promotion of a flagged trailing
            // Hash into the callee's keywords only happens when the call
            // site passes no keywords of its own (`r2k_promote` in
            // `set_callee_frame_arguments`), and the fast path does not
            // implement it. Forwarding call sites without keyword syntax
            // — `ruby2_keywords def t(*args); super; end`, or a
            // delegating block's `target(*args, **kwargs)` — must keep
            // taking the generic path.
            // Array-path forwarding consume (callee with opt/post/rest):
            // it reads `f`'s rest slot as a real `Array`.
            if self.deferred_forward_info().is_some() {
                ir.set_needs_rest_array();
            }
            self.write_back_recv_and_callargs(ir, callsite);
            let error = ir.new_error(self);
            ir.push(AsmInst::SetArgumentsForwardedHelper { callid, callee_fid });
            ir.handle_error(error);
        } else if !callsite.forwarding
            && callsite.pos_num >= 1
            && callsite.splat_pos.as_slice() == [callsite.pos_num - 1]
            && !callsite.kw_may_exists()
            && callee.no_keyword()
            && callee.opt_num() == 0
            && !callee.is_rest()
            && callee.post_num() == 0
            && !callee.single_arg_expand()
            && callee.req_num() + 1 >= callsite.pos_num
        {
            // Plain trailing-splat call `g(x.., *ary)` into a req-only
            // callee — `item_check(*node)`-style recursion is this shape on
            // every call. The eager `SetArgumentsForwarded` lowering fits
            // it exactly: if at run time the splat operand is an `Array` of
            // exactly `req_num - lead_num` elements, the leading args and
            // the elements copy straight into the callee frame; any other
            // length or a non-Array operand (whose bind may coerce or
            // raise) falls back to the generic runtime inside the emitted
            // code. No keywords exist on either side (`kw_may_exists` /
            // `no_keyword`), so no kw-rest guard is needed, and without
            // keyword syntax at the call site no ruby2_keywords promotion
            // can apply. A non-forwarding splat operand is an ordinary
            // evaluated slot, so no deferred-rest handling applies either.
            self.write_back_recv_and_callargs(ir, callsite);
            let error = ir.new_error(self);
            ir.push(AsmInst::SetArgumentsForwarded {
                callid,
                callee_fid,
                recv: callsite.recv,
                args: callsite.args,
                lead_num: callsite.pos_num - 1,
                kwrest_guard: None,
                deferred_src: None,
                kw_route: None,
            });
            ir.handle_error(error);
        } else {
            // Generic path. A forwarding call here (e.g. native callee
            // such as `Array.new`'s `o.__send__(:initialize, ...)`, or
            // a leading-arg forward like `File.read(@path, ...)`) reads
            // `f`'s rest slot as a real `Array` via the runtime
            // `jit_generic_set_arguments`, so veto the skip.
            if callsite.forwarding && self.deferred_forward_info().is_some() {
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

    /// D1 forwarding-rest deferral into an *optional*-parameter callee:
    /// the forwarded count is statically known per specialization, so the
    /// fill layout (copied slots + None-filled optionals running their
    /// defaults) is a compile-time constant. Cover under-, exactly-, and
    /// Polymorphic-but-single-target call sites: every observed receiver
    /// class re-resolves the name to one FuncId (`Kernel#is_a?` /
    /// `Kernel#frozen?`), so the JIT emits a class-set membership guard
    /// instead of a single-class guard that would deopt on 3 of every 4
    /// receivers. Covers: the set guard's hit path for each member class,
    /// a third class with a *different* resolution arriving after warmup
    /// (must deopt and dispatch its override), and nil/bool members.
    #[test]
    fn polymorphic_same_target() {
        run_test(
            r#"
            def check(x) = x.is_a?(Integer)
            def fro(x) = x.frozen?
            vals = [[1, 2], nil, "s", { a: 1 }]
            res = []
            c = 0
            f = 0
            200.times do |i|
                c += 1 if check(vals[i % 4])
                f += 1 if fro(vals[i % 4])
            end
            res << c << f
            class WeirdIsA
              def is_a?(k) = "custom"
            end
            res << check(WeirdIsA.new)
            res << check(7) << check(nil) << check(false)
            res
            "#,
        );
    }

    /// A megamorphic site (a fifth distinct receiver class overflows the
    /// 4-way PMC) takes the same-target set guard over the ways it *did*
    /// observe; the unobserved classes deopt, which is what the ordinary
    /// single-class guard did for all of them. Also exercises the PMC
    /// overflow counter itself, and a float-typed branch behind a
    /// polymorphic `nil?` (the receiver state stays unrefined after the set
    /// guard, so the Float conversions downstream must still be correct).
    #[test]
    fn polymorphic_megamorphic_and_float() {
        run_test(
            r#"
            def check(x) = x.nil?
            def step_like(limit, by)
              acc = 0.0
              unless limit.nil?
                acc += limit * 2.0
              end
              unless by.nil?
                acc += by + 1.5
              end
              acc
            end
            vals = [[1, 2], nil, "s", { a: 1 }, :sym, 7, 2.5, false]
            c = 0
            r = 0.0
            200.times do |i|
              c += 1 if check(vals[i % 8])
              case i % 4
              when 0 then r += step_like(2.5, nil)
              when 1 then r += step_like(nil, 3.5)
              when 2 then r += step_like(1.5, 0.5)
              when 3 then r += step_like(nil, nil)
              end
            end
            [c, r]
            "#,
        );
    }

    /// A generator that reads only the receiver Value
    /// keep firing behind the polymorphic class-set guard. Covers: `nil?` /
    /// `frozen?` / `object_id` at a 4-class site (set guard + inline, the
    /// receiver class statically unrefined), every representation arm of the
    /// new `frozen?` predicate — packed values, heap Numerics (Bignum /
    /// Rational / Complex, always frozen), a frozen string, a chilled string
    /// literal (FROZEN bit clear → false), a mutable Array — and a receiver
    /// class overriding `frozen?` after warmup, which must dispatch its
    /// override (class-version bump + set-membership deopt).
    /// `object_id` values differ from CRuby's, so only identity-stability
    /// (`oid(v) == v.object_id`) is compared, never the raw id.
    #[test]
    fn polymorphic_classless_inline() {
        run_test(
            r#"
            def fro(x) = x.frozen?
            def oid(x) = x.object_id
            def nl(x) = x.nil?
            quad = [nil, "s", 1, [1]]
            q = []
            n = 0
            200.times do
              q = quad.map { |v| [fro(v), oid(v) == v.object_id] }
              n += quad.count { |v| nl(v) }
            end
            all = [1, :sym, nil, true, 10**30, 1r/3, 1+2i, "chilled", "frozen".freeze, [1]]
            res = all.map { |v| fro(v) }
            class FrozenLiar
              def frozen? = :nope
            end
            [q, n, res, fro(FrozenLiar.new)]
            "#,
        );
    }

    /// A megamorphic site whose observed receiver classes converge on one
    /// builtin target takes the set guard over the ways the PMC kept; the
    /// overflowed classes deopt. In one hot rotation: six classes that
    /// converge on `Kernel#is_a?` (two of them answering `true`), one that
    /// overrides the name — it must be kept out of the set and dispatch its
    /// own body — and a rare receiver reached once every 131 iterations.
    /// Then a set member gains an override after warmup, which the class
    /// version guard must catch.
    #[test]
    fn polymorphic_megamorphic_same_target() {
        run_test(
            r#"
            class A; end
            class B; end
            class C; end
            class D; end
            class E; end
            class Own
              def is_a?(k) = :own
            end
            def check(x) = x.is_a?(Numeric)
            hot = [A.new, B.new, C.new, D.new, E.new, 1, 2.5, Own.new]
            t = 0
            o = 0
            n = 0
            400.times do |i|
              case check(hot[i % 8])
              when true then t += 1
              when :own then o += 1
              else n += 1
              end
              check(nil) if i % 131 == 0
            end
            res = [t, o, n]
            class C
              def is_a?(k) = :c
            end
            res << check(C.new) << check(A.new) << check(1) << check(nil)
            res
            "#,
        );
    }

    /// Redefining a **member of the class-set guard other than the compiled
    /// receiver class** must invalidate the body. The set guard lets three
    /// classes reach one cached target, so all three are recorded in the
    /// inline cache map; recording only `recv_class` (as the set guard first
    /// shipped) let `update_inline_cache` confirm the stale resolution and
    /// stamp the new class version into a body that kept calling
    /// `Kernel#is_a?` / `Kernel#frozen?`. Covers both the ordinary
    /// set-guarded builtin call (`is_a?`) and the class-independent inline
    /// generator that fires behind the same guard (`frozen?`).
    #[test]
    fn polymorphic_set_member_redefined() {
        run_test_once(
            r#"
            class A; end
            class B; end
            class C; end
            def check(x) = x.is_a?(Numeric)
            def fro(x) = x.frozen?
            hot = [A.new, B.new, C.new]
            300.times { |i| v = hot[i % 3]; check(v); fro(v) }
            class C
              def is_a?(k) = :c
              def frozen? = :cf
            end
            [check(A.new), check(C.new), fro(A.new), fro(C.new), C.new.is_a?(Numeric)]
            "#,
        );
    }

    /// The share threshold: a site dominated by two alternating classes
    /// plus a handful of stragglers keeps the set to the dominant pair, and
    /// the stragglers deopt. Only correctness is asserted here — the width
    /// of the membership chain is not observable from Ruby — so this pins
    /// that excluding a class from the set never changes its answer.
    #[test]
    fn polymorphic_rare_tail() {
        run_test(
            r#"
            class Tail1; end
            class Tail2; end
            class Tail3; end
            def probe(x) = x.is_a?(String)
            hot = ["a", :b]
            tail = [Tail1.new, Tail2.new, Tail3.new]
            t = 0
            300.times do |i|
              t += 1 if probe(hot[i % 2])
              probe(tail[i % 3]) if i % 89 == 0
            end
            [t, probe(Tail1.new), probe("z"), probe(:z), probe(nil)]
            "#,
        );
    }

    /// over-supplied optionals (the last vetoes the deferral and takes
    /// the eager helper path, raising ArgumentError like CRuby), plus a
    /// default expression with a side effect (must run only when the
    /// slot is None-filled).
    #[test]
    fn forwarded_opt_callee() {
        run_test(
            r#"
            $effects = []
            def g(a = ($effects << :a; 1), b = ($effects << :b; 2)); [a, b]; end
            def f(...) = g(...)
            def strict(x, y = 9) = [x, y]
            def fs(...) = strict(...)
            res = []
            res << f
            res << f(10)
            res << f(10, 20)
            res << (begin; fs(1, 2, 3); rescue ArgumentError => e; e.message; end)
            res << fs(1)
            res << $effects.size
            res
            "#,
        );
    }

    /// Trivial-method (`ISeqHint`) fold through a `...` forward. The Ruby
    /// `Class#new` reaches `initialize` via `o.__builtin_initialize__(...)`,
    /// a forwarding call site `is_simple_call` always rejects — so the fold
    /// applies only once the D1 deferral makes the forwarded argument shape
    /// a compile-time constant. Covers what the fold must NOT swallow:
    /// arity and keyword errors still raise, an impure default still runs,
    /// and a real `initialize` still executes.
    #[test]
    fn trivial_initialize_folded() {
        run_test(
            r#"
            $effects = []
            class Fold
              def initialize(x); end
            end
            class FoldRest
              def initialize(*a); end
            end
            class FoldOpt
              def initialize(x, y = 1); end
            end
            class Impure
              def initialize(x = ($effects << :d; 1)); end
            end
            class Real
              def initialize(x); @x = x; end
              attr_reader :x
            end
            res = []
            r = []; 100.times {|i| r << Fold.new(i).class }; res << r.uniq
            r = []; 100.times {|i| r << FoldRest.new(i, i, i).class }; res << r.uniq
            r = []; 100.times {|i| r << FoldOpt.new(i).class }; res << r.uniq
            r = []; 100.times {|i| r << Impure.new.class }; res << r.uniq
            r = []; 100.times {|i| r << Real.new(i).x }; res << r.last
            res << (begin; Fold.new; rescue ArgumentError => e; e.class; end)
            res << (begin; Fold.new(1, 2); rescue ArgumentError => e; e.class; end)
            res << (begin; Fold.new(1, k: 2); rescue ArgumentError => e; e.class; end)
            res << Fold.new(1) { :blk }.class
            res << $effects.size
            res
            "#,
        );
    }

    /// Frame-free expansion of `def initialize(a, b) = (@a = a; @b = b)`:
    /// the stores must land in the right slots for a plain construction, a
    /// subclass (whose ivar table is its own), a body reached through
    /// `send` (where `initialize`'s return value — the last RHS — is
    /// actually observable), and values of every representation the store
    /// has to box on the way in (nil, unboxed float, Bignum).
    #[test]
    fn frameless_ivar_stores() {
        run_test(
            r#"
            class V
              def initialize(x, y, z); @x = x; @y = y; @z = z; end
              attr_reader :x, :y, :z
            end
            class Sub < V; end
            # A subclass that assigns its own ivars first, so its slot
            # numbering diverges from the superclass's: the expansion must
            # resolve names against the *receiver's* class, not the one the
            # body was written in.
            class Skew < V
              def pre; @w = 0; @z = 0; @y = 0; @x = 0; end
            end
            Skew.allocate.pre
            res = []
            r = []; 100.times {|i| v = V.new(i, i * 2, i.to_f); r << [v.x, v.y, v.z] }
            res << r.last
            r = []; 100.times {|i| v = Sub.new(i, i, i); r << [v.class, v.x] }
            res << r.last
            r = []; 100.times {|i| v = Skew.new(i, i + 1, i + 2); r << [v.x, v.y, v.z] }
            res << r.last
            r = []; 100.times {|i| v = V.new(nil, 1.5, 2 ** 70); r << [v.x, v.y, v.z] }
            res << r.last
            # `initialize` returns its last assignment's RHS.
            r = []; 100.times {|i| o = V.allocate; r << o.send(:initialize, i, 2, 3) }
            res << r.last
            res << V.new(1, 2, 3).instance_variables
            res
            "#,
        );
    }

    /// What the expansion must NOT swallow. The frozen guard is hoisted
    /// ahead of every store, so a frozen receiver has to raise
    /// `FrozenError` with **nothing written** — the deopt hands the whole
    /// call back to the interpreter. Arity still raises, a redefinition
    /// still takes effect, and a body just past the inline-slot budget
    /// (more than `OBJECT_INLINE_IVAR` ivars) still runs correctly through
    /// the ordinary call.
    #[test]
    fn frameless_ivar_stores_declines() {
        run_test(
            r#"
            class V
              def initialize(x, y); @x = x; @y = y; end
              attr_reader :x, :y
            end
            class Wide
              def initialize(a, b, c, d, e, f, g, h)
                @a = a; @b = b; @c = c; @d = d; @e = e; @f = f; @g = g; @h = h
              end
              def all = [@a, @b, @c, @d, @e, @f, @g, @h]
            end
            res = []
            r = nil
            100.times do
              o = V.allocate.freeze
              begin
                o.send(:initialize, 1, 2)
              rescue => e
                r = [e.class, o.instance_variables]
              end
            end
            res << r
            r = []; 100.times {|i| r << Wide.new(1, 2, 3, 4, 5, 6, 7, i).all }
            res << r.last
            res << (begin; V.new(1); rescue ArgumentError => e; e.class; end)
            res << (begin; V.new(1, 2, 3); rescue ArgumentError => e; e.class; end)
            100.times { V.new(1, 2) }
            class V
              def initialize(x, y); @x = x + 100; @y = y; end
            end
            r = []; 100.times { r << V.new(1, 2).x }
            res << r.uniq
            res
            "#,
        );
    }

    /// The expansion is not about `initialize`: **any** method whose body
    /// is just ivar stores qualifies, and on a direct call site the
    /// arguments are the caller's own slots rather than a `...` forward's
    /// deferred window. That also makes the return value — the last
    /// assignment's RHS — routinely observable, which `Class#new` discards.
    ///
    /// Alongside those, the four shapes that must fall through to the
    /// ordinary call: a receiver whose layout has no inline ivar slots
    /// (`Struct`), a body whose ivars land past the inline budget, a body
    /// with a branch (the hoisted frozen guard needs every path to store),
    /// and a body with no store at all.
    #[test]
    fn frameless_ivar_stores_direct_call() {
        run_test(
            r#"
            class C
              def set2(a, b); @a = a; @b = b; end
              def set1(x); @x = x; end
              def echo(x) = x
              def call_set(...) = set2(...)
              def maybe(a, b); @a = a if a; @b = b; end
              def pair = [@a, @b]
              attr_reader :x
            end
            # Six ivars already claim every inline slot, so this class's
            # `@a` / `@b` live in the heap table.
            class Heapy
              def pre; @p0 = 0; @p1 = 0; @p2 = 0; @p3 = 0; @p4 = 0; @p5 = 0; end
              def set2(a, b); @a = a; @b = b; end
              def pair = [@a, @b]
            end
            Heapy.new.pre
            S = Struct.new(:q) do
              def set2(a, b); @a = a; @b = b; end
              def pair = [@a, @b]
            end
            res = []
            c = C.new
            r = []; 100.times {|i| r << c.set2(i, i + 1) }; res << [r.last, c.pair]
            r = []; 100.times {|i| r << c.set1(i.to_f) };   res << [r.last, c.x]
            r = []; 100.times {|i| r << c.echo(i) };        res << r.last
            # Through a `...` trampoline, where the result *is* read.
            r = []; 100.times {|i| r << c.call_set(i, i * 2) }; res << [r.last, c.pair]
            h = Heapy.new
            r = []; 100.times {|i| r << h.set2(i, i + 1) }; res << [r.last, h.pair]
            s = S.new(0)
            r = []; 100.times {|i| r << s.set2(i, i + 1) }; res << [r.last, s.pair]
            r = []; 100.times {|i| r << c.maybe(nil, i) }; res << [r.last, c.pair]
            r = []; 100.times {|i| r << c.maybe(i, i) };   res << [r.last, c.pair]
            d = C.new.freeze
            res << (begin; d.set2(1, 2); rescue => e; [e.class, d.pair]; end)
            res
            "#,
        );
    }

    /// The same fold on a plain `def f(...) = g(...)` trampoline (not just
    /// the `Class#new` shape), and the invalidation path: once a folded
    /// callee is redefined with a body that has a side effect, the compiled
    /// caller must stop folding and run it.
    #[test]
    fn trivial_forwarded_fold_and_redefine() {
        run_test(
            r#"
            $log = []
            def g(x) = 42
            def f(...) = g(...)
            res = []
            r = []; 100.times {|i| r << f(i) }; res << r.uniq
            class Re
              def initialize(x); end
            end
            100.times {|i| Re.new(i) }
            class Re
              def initialize(x); $log << x; end
            end
            Re.new(7)
            res << $log
            res
            "#,
        );
    }

    /// A JIT-specialized *deferred* construction that deopts mid-run. The
    /// Ruby `Class#new` trampoline defers `Foo.new`'s `...` rest (D1), so the
    /// forwarded ctor args are source-routed straight from the caller frame
    /// and no rest array is built on the fast path. A class-version bump then
    /// invalidates the compiled construction, so the interpreter resumes
    /// inside `new` and must rebuild the forwarded `...` array via the
    /// `forward_rest` side-exit materialize (`gen_forward_rest_materialize` /
    /// `a64_gen_forward_rest_materialize`). Exercises both the deferred
    /// source-routing and the deopt materialization on both backends; the
    /// ctor takes both req-only and optional shapes.
    #[test]
    fn deferred_construction_deopt() {
        run_test(
            r#"
            class Foo
              def initialize(a, b, c); @a = a; @b = b; @c = c; end
              attr_reader :a, :b, :c
            end
            class Bar
              def initialize(a, b = 10, c = 100); @a = a; @b = b; @c = c; end
              attr_reader :a, :b, :c
            end
            def mk_foo(i); Foo.new(i, i * 2, i * 3); end
            def mk_bar1(i); Bar.new(i); end
            def mk_bar2(i); Bar.new(i, i + 1); end
            res = 0
            i = 0
            while i < 300
              f = mk_foo(i)
              res += f.a + f.b + f.c
              b1 = mk_bar1(i)
              b2 = mk_bar2(i)
              res += b1.a + b1.b + b1.c + b2.b + b2.c
              # bump the class version mid-run -> deopt the compiled ctors
              Foo.class_eval { def extra; 1; end } if i == 150
              Bar.class_eval { def extra; 1; end } if i == 175
              i += 1
            end
            res
            "#,
        );
    }

    /// D1 forwarding-rest deferral into a `*rest` / bare-`**kwrest`
    /// callee: the rest `Array` is built directly from the caller's
    /// slots (single `create_array`, no intermediate) and the kwrest
    /// slot is nil-initialized. Cover empty/short/long binds, leading
    /// args, opt+rest mixes, arity errors, and the freshness of the
    /// built Array (mutating one result must not affect another call).
    #[test]
    fn forwarded_rest_callee() {
        run_test(
            r#"
            def r0(*a) = [:r0, a]
            def r1(x, *a) = [:r1, x, a]
            def ro(x, y = :dy, *a) = [:ro, x, y, a]
            def rk(*a, **k) = [:rk, a, k]
            def f0(...) = r0(...)
            def f1(...) = r1(...)
            def fo(...) = ro(...)
            def fk(...) = rk(...)
            def lead(x, ...) = ro(x, ...)
            def cap(*a) = a
            def fc(...) = cap(...)
            res = []
            res << f0() << f0(1, 2, 3)
            res << f1(1) << f1(1, 2, 3)
            res << fo(1) << fo(1, 2) << fo(1, 2, 3, 4)
            res << fk() << fk(1, 2)
            res << lead(:L) << lead(:L, 1, 2, 3)
            res << (begin; f1(); rescue ArgumentError => e; e.message; end)
            x = fc(1, 2); y = fc(1, 2); x << 3
            res << x << y
            res
            "#,
        );
    }

    /// The motivating shape: Struct construction through the Ruby
    /// `Class#new` — `Struct#initialize` is a rest + kwrest native.
    #[test]
    fn forwarded_struct_rest_native() {
        run_test_with_prelude(
            r#"
            res = []
            100.times { res = [S.new(1, 2).to_a, S.new(1).to_a, K.new(x: 5).x] }
            res << (begin; S.new(1, 2, 3); rescue ArgumentError => e; e.message; end)
            res
            "#,
            r#"
            S = Struct.new(:a, :b)
            K = Struct.new(:x, keyword_init: true)
            "#,
        );
    }

    /// Regression test: a specialized (inlined-compiled) callee that
    /// captures its own frame (`Proc.new` with a literal block) while the
    /// call site passes a block literal. Materializing the escaped block
    /// handler promotes the *caller's* frame to the heap mid-call
    /// (`materialize_escaped_block_handlers`), so the caller's result
    /// store must go via the LFP — an rbp-relative store lands on the
    /// abandoned stack frame and the result reads back as nil after the
    /// capture deopt. This is the `Timeout.timeout` shape: its `perform`
    /// proc's non-local `return yield(sec)` silently became nil once the
    /// caller was JIT-compiled, so `Net::HTTP#connect` saw a nil socket.
    #[test]
    fn specialized_call_result_survives_transitive_capture() {
        run_test(
            r#"
            def tmo(klass)
              perform = Proc.new do
                begin
                  return :tok
                ensure
                  @x = 1
                end
              end
              if klass
                perform.call
              else
                dummy(&perform)
              end
            end
            res = []
            50.times { |n| res << (tmo(RuntimeError) { :blk }) }
            res
            "#,
        );
    }

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
