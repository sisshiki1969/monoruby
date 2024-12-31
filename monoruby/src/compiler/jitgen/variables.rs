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
        self.unlink(dst);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let is_object_ty = self.self_ty == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::LoadIVarInline { ivarid })
        } else {
            ir.push(AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                min_len: self.self_ivar_len,
            });
        }
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
        let is_object_ty = self.self_ty == Some(ObjTy::OBJECT);
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::StoreIVarInline { ivarid });
        } else {
            ir.push(AsmInst::StoreIVarHeap {
                ivarid,
                is_object_ty,
                min_len: self.self_ivar_len,
                using_xmm: self.get_using_xmm(),
            });
            self.self_ivar_len = std::cmp::max(ivarid.get() as usize + 1, self.self_ivar_len);
        }
    }
}

impl BBContext {
    pub(super) fn jit_load_gvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.unlink(dst);
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
        self.unlink(dst);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.push(AsmInst::LoadCVar { name, using_xmm });
        ir.handle_error(error);
        self.rax2acc(ir, dst);
    }

    pub(super) fn jit_check_cvar(&mut self, ir: &mut AsmIr, name: IdentId, dst: SlotId) {
        self.unlink(dst);
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
