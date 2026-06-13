use super::*;

impl AbstractState {
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
        // A class/module body is a full call (`enter_classdef`) that
        // clobbers the accumulator (r15). When `class ... end` appears in
        // expression position (e.g. `a << (class C; ...; end)`) the
        // accumulator holds a live slot — spill it first, exactly like a
        // method call does, or the later use reads a clobbered register.
        self.writeback_acc(ir);
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
        // See `class_def`: `class << obj` runs a body that clobbers the
        // accumulator, so spill it before the call.
        self.writeback_acc(ir);
        self.write_back_slots(ir, &[base]);
        if let Some(dst) = dst {
            self.def_S(dst);
        }
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
