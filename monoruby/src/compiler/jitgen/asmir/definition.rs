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
            bbctx.write_back_slots(self, &[base]);
        }
        if let Some(superclass) = superclass {
            bbctx.write_back_slots(self, &[superclass]);
        }
        bbctx.unlink(self, dst);
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
        bbctx.write_back_slots(self, &[base]);
        bbctx.unlink(self, dst);
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
