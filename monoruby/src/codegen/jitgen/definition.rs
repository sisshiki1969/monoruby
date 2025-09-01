use super::*;

impl BBContext {
    pub(super) fn class_def(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
    ) {
        if let Some(base) = base {
            self.write_back_slots(ir, &[base]);
        }
        if let Some(superclass) = superclass {
            self.write_back_slots(ir, &[superclass]);
        }
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        ir.push(AsmInst::ClassDef {
            base,
            superclass,
            dst,
            name,
            func_id,
            is_module,
            using_xmm,
            error,
        });
    }

    pub(super) fn singleton_class_def(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
    ) {
        self.write_back_slots(ir, &[base]);
        self.discard(dst);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        ir.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
