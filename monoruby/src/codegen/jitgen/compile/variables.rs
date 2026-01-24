use super::*;

impl<'a> JitContext<'a> {
    pub(super) fn load_ivar(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        dst: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) -> bool {
        assert!(!self_class.is_always_frozen());
        state.discard(dst);
        state.writeback_acc(ir);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        let ivar_heap = if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::LoadIVarInline { ivarid });
            false
        } else {
            ir.push(AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_: true,
            });
            true
        };
        state.def_reg2acc(ir, GP::R15, dst);
        ivar_heap
    }

    pub(super) fn store_ivar(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        src: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) -> bool {
        assert!(!self_class.is_always_frozen());
        let src = state.load_or_reg(ir, src, GP::Rax);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid });
            false
        } else {
            ir.push(AsmInst::StoreSelfIVarHeap {
                src,
                ivarid,
                is_object_ty,
            });
            true
        }
    }
}

impl AbstractState {
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
