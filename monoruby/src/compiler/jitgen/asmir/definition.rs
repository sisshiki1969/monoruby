use super::*;

impl JitContext {
    pub(in crate::compiler::jitgen) fn class_def(
        &mut self,
        ir: &mut AsmIr,
        bb: &mut BBContext,
        dst: Option<SlotId>,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BytecodePtr,
    ) {
        if let Some(base) = base {
            ir.write_back_slots(bb, &[base]);
        }
        if let Some(superclass) = superclass {
            ir.write_back_slots(bb, &[superclass]);
        }
        ir.unlink(bb, dst);
        let using_xmm = bb.get_using_xmm();
        let error = ir.new_error(bb, pc);
        ir.inst.push(AsmInst::ClassDef {
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
        ir: &mut AsmIr,
        bb: &mut BBContext,
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
        pc: BytecodePtr,
    ) {
        ir.write_back_slots(bb, &[base]);
        ir.unlink(bb, dst);
        let using_xmm = bb.get_using_xmm();
        let error = ir.new_error(bb, pc);
        ir.inst.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
