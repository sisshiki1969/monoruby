use super::*;
use crate::codegen::jitgen::state::Guarded;

impl<'a> JitContext<'a> {
    pub(super) fn load_ivar(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) {
        assert!(!self_class.is_always_frozen());
        // Allocate a pool register and load the ivar value straight into it (a
        // resident), so a following integer op consumes it without a stack
        // round-trip. `alloc_gp_for` clears `dst`'s stale link and spills an
        // evicted victim; the load writes only `gp` (its scratch is rdi/rsi/rdx,
        // none of them pool registers), so other residents survive.
        let gp = state.alloc_gp_for(ir, dst, Guarded::Value);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::LoadIVarInline { ivarid, dst: gp });
        } else {
            ir.push(AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_: true,
                dst: gp,
            });
            self.set_ivar_heap_accessed();
        };
        state.bind_gp_resident(gp, dst);
    }

    pub(super) fn store_ivar(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        src: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) {
        assert!(!self_class.is_always_frozen());
        // A provably-immediate stored value needs no GC write barrier.
        let wb = !state.is_guarded_immediate(src);
        let src = state.load_or_reg(ir, src, GP::Rax);
        ir.self2reg(GP::Rdi);
        let deopt = ir.new_deopt(state);
        ir.guard_frozen(deopt);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid, wb });
        } else {
            ir.push(AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
                wb,
            });
            self.set_ivar_heap_accessed();
        }
    }

    pub(super) fn load_constant(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        id: ConstSiteId,
    ) -> JitResult<CompileResult> {
        state.discard(dst);

        if let Some(cache) = &self.store[id].cache {
            // Only fold a cache that was resolved at the compile-time const
            // version. The global const version is monotonic, so a single
            // `GuardConstVersion` against that snapshot validates every
            // folded constant in the trace (mirroring how the class-version
            // guard works). A staler cache would be unsound to fold against
            // that snapshot, so bail and let the VM refresh it.
            if cache.version as u64 != self.const_version() {
                return Ok(CompileResult::Recompile(RecompileReason::NotCached));
            }
            // A self-dependent resolution (singleton-cref frame) is only
            // foldable when this compilation's self class matches — the
            // dispatch guard then pins it at runtime.
            match cache.self_class {
                Some(sc) => {
                    if sc != self.store.const_self_key_for_class(self.self_class()) {
                        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                    }
                }
                None => {
                    // Constant sites in singleton-lexical methods are
                    // always filled with a Some key (resolution there is
                    // self-class dependent), so a None entry can only
                    // predate this receiver's resolution — let the VM
                    // re-resolve rather than fold a value whose validity
                    // rests on a re-stampable static cref.
                    if self.iseq().in_singleton_lexical {
                        return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                    }
                }
            }
            let base_slot = self.store[id].base;
            if let Some(slot) = base_slot {
                if let Some(base_class) = cache.base_class {
                    state.guard_const_base_class(ir, slot, base_class);
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                }
            }
            self.guard_const_version(state, ir, cache.version);
            state.load_constant(ir, dst, cache);
            state.unset_side_effect_guard();
            Ok(CompileResult::Continue)
        } else {
            Ok(CompileResult::Recompile(RecompileReason::NotCached))
        }
    }

    ///
    /// Constant version guard.
    ///
    /// The global constant version is monotonic and every folded constant is
    /// gated on the compile-time snapshot, so one guard per trace covers them
    /// all (`AbstractState::const_version_guard` skips the rest).
    ///
    /// A miss takes the **counter-gated recompile** side exit rather than a
    /// plain deopt. That distinction is the whole point: the fold is only
    /// valid at the snapshot version, so once the version moves the guard can
    /// never pass again and a plain deopt fires on *every* subsequent
    /// execution, forever — a single `CONST = ...` anywhere in the program
    /// permanently unwinds every body that folded a constant. Recompiling
    /// re-reads the constant at the new version. The counter
    /// (`COUNT_DEOPT_RECOMPILE`) bounds the cost for programs that keep
    /// assigning constants: at worst 10 deopts and one recompile per version
    /// move, instead of one deopt per call for the rest of the run.
    ///
    fn guard_const_version(&self, state: &mut AbstractState, ir: &mut AsmIr, version: usize) {
        if state.const_version_guard() {
            return;
        }
        // Two shapes stay on the plain deopt. A specialized (inlined-frame)
        // compile recompiles via an idx, not a position, so it has no
        // recompile side exit to take — the same restriction the
        // receiver-class guard makes in `compile_method_call`. And a *block*
        // body must not take it either: the side exit recompiles whatever
        // `lfp.func_id()` names, and `Codegen::recompile_method` re-enters
        // the compiler as if that were a method — for a block frame that
        // rebuilds the body under the wrong argument convention, which showed
        // up as `Kernel#caller_locations`' block losing the argument it
        // forwards to `Thread::Backtrace::Location.new`.
        let deopt = if matches!(self.jit_type(), JitType::Specialized { .. })
            || self.store[self.func_id()].is_block_style()
        {
            ir.new_deopt(state)
        } else {
            ir.new_recompile_deopt(
                state,
                RecompileReason::ConstVersionGuardFailed,
                self.position(),
            )
        };
        ir.push(AsmInst::GuardConstVersion {
            const_version: version,
            deopt,
        });
        state.set_const_version_guard();
    }

    pub(super) fn load_dynvar(&self, state: &AbstractState, ir: &mut AsmIr, src: DynVar) {
        if let Some((spec_ids, extra, not_captured)) = self.outer_specialized_ids(state, src.outer)
            && not_captured
        {
            assert!(not_captured);
            ir.push(AsmInst::LoadDynVarSpecialized {
                offset: DynVarOffset::Hint {
                    ids: spec_ids,
                    extra,
                },
                reg: src.reg,
            });
        } else {
            ir.push(AsmInst::LoadDynVar { src });
        }
    }

    pub(super) fn store_dynvar(
        &self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: DynVar,
        src: SlotId,
    ) {
        let r = GP::Rdi;
        state.load(ir, src, r);
        if let Some((spec_ids, extra, not_captured)) = self.outer_specialized_ids(state, dst.outer)
            && not_captured
        {
            assert!(not_captured);
            ir.push(AsmInst::StoreDynVarSpecialized {
                offset: DynVarOffset::Hint {
                    ids: spec_ids,
                    extra,
                },
                dst: dst.reg,
                src: r,
            });
        } else {
            ir.push(AsmInst::StoreDynVar { dst, src: r });
        }
    }
}

impl AbstractState {
    fn load_constant(&mut self, ir: &mut AsmIr, dst: SlotId, cache: &ConstCache) {
        let ConstCache { value, .. } = cache;
        // The version guard is emitted by the caller
        // (`JitContext::guard_const_version`), which knows the JIT type and
        // so can pick the recompiling side exit; by here it is in place.
        debug_assert!(self.const_version_guard());
        // Heap-allocated Float: keep the Sf optimization so subsequent
        // float ops can read the f64 from fpr without re-extracting it
        // from the RValue. Immediate flonums skip this path because
        // `def_C` is already cheaper than emitting an `Sf` materialization.
        if value.is_immediate().is_none()
            && let Some(f) = value.try_float()
        {
            ir.lit2reg(*value, GP::Rax);
            let fdst = self.def_Sf_float(dst);
            ir.f64_to_fpr(f, fdst);
            ir.reg2stack(GP::Rax, dst);
            return;
        }
        // All other values (immediates, class objects, modules, strings,
        // bignums, …) are folded into the abstract state as `LinkMode::C`.
        // GC safety: the version guard above deopts on any redefinition,
        // and `wb_literal` writes the value back to the stack slot before
        // every GC safepoint, so a non-moving mark-and-sweep collector
        // sees it through the normal stack scan.
        self.def_C(dst, *value);
    }

    ///
    /// Guard for the base class object of the constant in *slot*.
    ///
    /// ### destroy
    /// - rax
    ///
    fn guard_const_base_class(&mut self, ir: &mut AsmIr, slot: SlotId, base_class: Value) {
        self.load(ir, slot, GP::Rax);
        let deopt = ir.new_deopt(self);
        ir.push(AsmInst::GuardConstBaseClass { base_class, deopt });
    }

    pub(super) fn store_constant(&mut self, ir: &mut AsmIr, src: SlotId, id: ConstSiteId) {
        self.load(ir, src, GP::Rax);
        let using_fpr = self.get_using_fpr(ir);
        let error = ir.new_error(self);
        ir.push(AsmInst::StoreConstant {
            id,
            using_fpr,
            error,
        });
        // Storing a constant bumps the global const version, so any guard
        // we'd previously emitted no longer holds.
        self.unset_const_version_guard();
        self.unset_side_effect_guard();
    }

    pub(super) fn jit_load_gvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_fpr = self.get_using_fpr(ir);
        ir.push(AsmInst::LoadGVar { name, using_fpr });
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_store_gvar(&mut self, ir: &mut AsmIr, name: IdentId, src: SlotId) {
        self.write_back_slots(ir, &[src]);
        let using_fpr = self.get_using_fpr(ir);
        let error = ir.new_error(self);
        ir.push(AsmInst::StoreGVar {
            name,
            src,
            using_fpr,
        });
        ir.handle_error(error);
    }

    pub(super) fn jit_load_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_fpr = self.get_using_fpr(ir);
        let error = ir.new_error(self);
        ir.push(AsmInst::LoadCVar { name, using_fpr });
        ir.handle_error(error);
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_fpr = self.get_using_fpr(ir);
        ir.push(AsmInst::CheckCVar { name, using_fpr });
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_store_cvar(&mut self, ir: &mut AsmIr, name: IdentId, src: SlotId) {
        self.write_back_slots(ir, &[src]);
        let using_fpr = self.get_using_fpr(ir);
        let error = ir.new_error(self);
        ir.push(AsmInst::StoreCVar {
            name,
            src,
            using_fpr,
        });
        ir.handle_error(error);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// A body that folded a constant must survive a later constant
    /// assignment. The fold is only valid at the compile-time constant
    /// version, so once the global version moves the guard can never pass
    /// again — before the guard recompiled, one `CONST = ...` anywhere in
    /// the program left the body deopting on *every* call for the rest of
    /// the run. Covers a constant read after an unrelated assignment, one
    /// whose own value is reassigned, and a namespaced read.
    #[test]
    fn const_fold_survives_a_later_assignment() {
        run_test_once(
            r##"
            class Foo
              BAR = 42
              def get = BAR
              def qux = Foo::BAR
            end
            f = Foo.new
            res = []
            200.times { res << f.get }
            UNRELATED = 1
            200.times { res << f.get }
            res << f.qux
            class Foo
              remove_const(:BAR)
              BAR = 99
            end
            200.times { res << f.get }
            res << f.qux
            res.tally.sort_by { |k, _| k.to_s }
        "##,
        );
    }
}
