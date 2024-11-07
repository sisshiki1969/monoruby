use super::*;

impl AsmIr {
    pub(in crate::compiler::jitgen) fn load_ivar(
        &mut self,
        bb: &mut BBContext,
        name: IdentId,
        dst: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.unlink(bb, dst);
        self.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = bb.get_using_xmm();
        let is_object_ty = bb.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = bb.self_value.class() == cached_class;
        self.inst.push(AsmInst::LoadIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
        });
        self.rax2acc(bb, dst);
    }

    pub(in crate::compiler::jitgen) fn store_ivar(
        &mut self,
        bb: &mut BBContext,
        name: IdentId,
        src: SlotId,
        pc: BytecodePtr,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.fetch_for_gpr(bb, src, GP::Rax);
        self.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = bb.get_using_xmm();
        let error = self.new_error(bb, pc);
        let is_object_ty = bb.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = bb.self_value.class() == cached_class;
        self.inst.push(AsmInst::StoreIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
            error,
        });
    }
}
