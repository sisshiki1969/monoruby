use super::*;

impl BBContext {
    pub(in crate::compiler::jitgen) fn load_ivar(
        &mut self,
        ir: &mut AsmIr,
        dst: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) {
        assert!(!self_class.is_always_frozen());
        self.unlink(ir, dst);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        ir.push(AsmInst::LoadIVar {
            ivarid,
            is_object_ty,
        });
        self.rax2acc(ir, dst);
    }

    pub(in crate::compiler::jitgen) fn store_ivar(
        &mut self,
        ir: &mut AsmIr,
        src: SlotId,
        self_class: ClassId,
        ivarid: IvarId,
    ) {
        assert!(!self_class.is_always_frozen());
        self.fetch_for_gpr(ir, src, GP::Rax);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = self.get_using_xmm();
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        ir.push(AsmInst::StoreIVar {
            ivarid,
            is_object_ty,
            using_xmm,
        });
    }
}

impl BBContext {
    pub(super) fn jit_load_gvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.unlink(ir, dst);
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

    pub(super) fn jit_load_cvar(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        name: IdentId,
        dst: SlotId,
    ) {
        self.unlink(ir, dst);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.push(AsmInst::LoadCVar { name, using_xmm });
        ir.handle_error(error);
        self.rax2acc(ir, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.unlink(ir, dst);
        let using_xmm = self.get_using_xmm();
        ir.push(AsmInst::CheckCVar { name, using_xmm });
        self.rax2acc(ir, dst);
    }

    pub(super) fn jit_store_cvar(
        &mut self,
        ir: &mut AsmIr,
        pc: BytecodePtr,
        name: IdentId,
        src: SlotId,
    ) {
        self.write_back_slots(ir, &[src]);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.push(AsmInst::StoreCVar {
            name,
            src,
            using_xmm,
        });
        ir.handle_error(error);
    }
}
