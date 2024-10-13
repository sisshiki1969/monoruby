use super::*;

impl AsmIr {
    pub(in crate::compiler::jitgen) fn class_def(
        &mut self,
        bbctx: &mut BBContext,
        dst: Option<SlotId>,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BytecodePtr,
    ) {
        if let Some(base) = base {
            self.write_back_slots(bbctx, &[base]);
        }
        if let Some(superclass) = superclass {
            self.write_back_slots(bbctx, &[superclass]);
        }
        self.unlink(bbctx, dst);
        let using_xmm = bbctx.get_using_xmm();
        let error = self.new_error(bbctx, pc);
        self.inst.push(AsmInst::ClassDef {
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
        bbctx: &mut BBContext,
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
        pc: BytecodePtr,
    ) {
        self.write_back_slots(bbctx, &[base]);
        self.unlink(bbctx, dst);
        let using_xmm = bbctx.get_using_xmm();
        let error = self.new_error(bbctx, pc);
        self.inst.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
