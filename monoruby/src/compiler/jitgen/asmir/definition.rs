use super::*;

impl JitContext {
    pub(in crate::compiler::jitgen) fn class_def(
        &mut self,
        bb: &mut BBContext,
        dst: Option<SlotId>,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BcPc,
    ) {
        if let Some(base) = base {
            self.ir.write_back_slots(bb, &[base]);
        }
        if let Some(superclass) = superclass {
            self.ir.write_back_slots(bb, &[superclass]);
        }
        self.ir.unlink(bb, dst);
        let using_xmm = bb.get_using_xmm();
        let error = self.ir.new_error(bb, pc);
        self.ir.inst.push(AsmInst::ClassDef {
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

    pub(in crate::compiler::jitgen) fn singleton_class_def(
        &mut self,
        bb: &mut BBContext,
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
        pc: BcPc,
    ) {
        self.ir.write_back_slots(bb, &[base]);
        self.ir.unlink(bb, dst);
        let using_xmm = bb.get_using_xmm();
        let error = self.ir.new_error(bb, pc);
        self.ir.inst.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
