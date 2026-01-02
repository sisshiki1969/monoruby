use super::*;

impl AbstractContext {
    pub(super) fn class_def(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BytecodePtr,
    ) {
        if let Some(base) = base {
            self.write_back_slots(ir, &[base]);
        }
        if let Some(superclass) = superclass {
            self.write_back_slots(ir, &[superclass]);
        }
        if let Some(dst) = dst {
            self.def_S(dst);
        }
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
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
        pc: BytecodePtr,
    ) {
        self.write_back_slots(ir, &[base]);
        if let Some(dst) = dst {
            self.def_S(dst);
        }
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        ir.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
