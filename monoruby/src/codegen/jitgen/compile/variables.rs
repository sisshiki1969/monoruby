use super::*;

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
        state.discard(dst);
        state.writeback_acc(ir);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::LoadIVarInline { ivarid });
        } else {
            ir.push(AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_: true,
            });
            self.set_ivar_heap_accessed();
        };
        state.def_reg2acc(ir, GP::R15, dst);
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
        let src = state.load_or_reg(ir, src, GP::Rax);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid });
        } else {
            ir.push(AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
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
    ) -> Result<CompileResult> {
        state.discard(dst);

        if let Some(cache) = &self.store[id].cache {
            let base_slot = self.store[id].base;
            if let Some(slot) = base_slot {
                if let Some(base_class) = cache.base_class {
                    state.guard_const_base_class(ir, slot, base_class);
                } else {
                    return Ok(CompileResult::Recompile(RecompileReason::NotCached));
                }
            }
            state.load_constant(ir, dst, cache);
            state.unset_side_effect_guard();
            Ok(CompileResult::Continue)
        } else {
            Ok(CompileResult::Recompile(RecompileReason::NotCached))
        }
    }

    pub(super) fn load_dynvar(&self, state: &AbstractState, ir: &mut AsmIr, src: DynVar) {
        if let Some((offset, not_captured)) = self.outer_stack_offset(state, src.outer) {
            assert!(not_captured);
            ir.push(AsmInst::LoadDynVarSpecialized {
                offset,
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
        if let Some((offset, not_captured)) = self.outer_stack_offset(state, dst.outer) {
            assert!(not_captured);
            ir.push(AsmInst::StoreDynVarSpecialized {
                offset,
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
        let ConstCache { version, value, .. } = cache;
        let deopt = ir.new_deopt(self);
        ir.push(AsmInst::GuardConstVersion {
            const_version: *version,
            deopt,
        });
        ir.lit2reg(*value, GP::Rax);
        if let Some(f) = value.try_float() {
            let fdst = self.def_Sf_float(dst);
            ir.f64_to_xmm(f, fdst);
            ir.reg2stack(GP::Rax, dst);
        } else {
            self.def_reg2acc(ir, GP::Rax, dst);
        }
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
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::StoreConstant { id, using_xmm });
        self.unset_side_effect_guard();
    }

    pub(super) fn jit_load_gvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::LoadGVar { name, using_xmm });
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_store_gvar(&mut self, ir: &mut AsmIr, name: IdentId, src: SlotId) {
        self.write_back_slots(ir, &[src]);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::StoreGVar {
            name,
            src,
            using_xmm,
        });
    }

    pub(super) fn jit_load_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        ir.push(AsmInst::LoadCVar { name, using_xmm });
        ir.handle_error(error);
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::CheckCVar { name, using_xmm });
        self.def_rax2acc(ir, dst);
    }

    pub(super) fn jit_store_cvar(&mut self, ir: &mut AsmIr, name: IdentId, src: SlotId) {
        self.write_back_slots(ir, &[src]);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        ir.push(AsmInst::StoreCVar {
            name,
            src,
            using_xmm,
        });
        ir.handle_error(error);
    }
}
