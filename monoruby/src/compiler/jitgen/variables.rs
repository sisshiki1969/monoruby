use super::*;

impl JitContext {
    pub(in crate::compiler::jitgen) fn load_ivar(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        dst: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) -> bool {
        assert!(!self_class.is_always_frozen());
        bbctx.discard(dst);
        bbctx.writeback_acc(ir);
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
        bbctx.reg2acc(ir, GP::R15, dst);
        ivar_heap
    }

    pub(in crate::compiler::jitgen) fn store_ivar(
        &mut self,
        bbctx: &mut BBContext,
        ir: &mut AsmIr,
        src: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) -> bool {
        assert!(!self_class.is_always_frozen());
        let src = bbctx.fetch_or_reg(ir, src, GP::Rax);
        ir.self2reg(GP::Rdi);
        let is_object_ty = self.self_ty() == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { src, ivarid });
            false
        } else {
            ir.push(AsmInst::StoreIVarHeap {
                src,
                ivarid,
                is_object_ty,
                self_: true,
                using_xmm: bbctx.get_using_xmm(),
            });
            true
        }
    }
}

impl BBContext {
    pub(super) fn jit_load_gvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::LoadGVar { name, using_xmm });
        self.rax2acc(ir, dst);
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
        let error = self.new_error(ir);
        ir.push(AsmInst::LoadCVar { name, using_xmm });
        ir.handle_error(error);
        self.rax2acc(ir, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::CheckCVar { name, using_xmm });
        self.rax2acc(ir, dst);
    }

    pub(super) fn jit_store_cvar(&mut self, ir: &mut AsmIr, name: IdentId, src: SlotId) {
        self.write_back_slots(ir, &[src]);
        let using_xmm = self.get_using_xmm();
        let error = self.new_error(ir);
        ir.push(AsmInst::StoreCVar {
            name,
            src,
            using_xmm,
        });
        ir.handle_error(error);
    }
}
