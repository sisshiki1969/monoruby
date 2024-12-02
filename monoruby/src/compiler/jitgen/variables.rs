use super::*;

impl BBContext {
    pub(in crate::compiler::jitgen) fn load_ivar(
        &mut self,
        ir: &mut AsmIr,
        name: IdentId,
        dst: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.unlink(ir, dst);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = self.get_using_xmm();
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = self.self_value.class() == cached_class;
        ir.push(AsmInst::LoadIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
        });
        self.rax2acc(ir, dst);
    }

    pub(in crate::compiler::jitgen) fn store_ivar(
        &mut self,
        ir: &mut AsmIr,
        name: IdentId,
        src: SlotId,
        pc: BytecodePtr,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.fetch_for_gpr(ir, src, GP::Rax);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = self.self_value.class() == cached_class;
        ir.push(AsmInst::StoreIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
            error,
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
