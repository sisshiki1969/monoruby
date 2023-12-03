use super::*;

impl Codegen {
    pub(super) fn jit_class_def_sub(&mut self, func_id: FuncId, ret: Option<SlotId>) {
        monoasm! { &mut self.jit,
            movq r15, rax; // r15 <- self
            movq rcx, rax; // rcx <- self
            movl rdx, (func_id.get());  // rdx <- func_id
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::enter_classdef);
            call rax; // rax <- &FuncData

            movq r8, rax;
            movq rdi, [r8 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_SELF)], r15;
        }
        self.set_method_outer();
        monoasm! { &mut self.jit,
            movq r13 , [r8 + (FUNCDATA_PC)];
            movq rax, [r8 + (FUNCDATA_CODEPTR)];
            xorq rdx, rdx;
        }
        self.call_rax();
        self.store_rax(ret);
        // pop class context.
        monoasm! { &mut self.jit,
            movq r13, rax;
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::exit_classdef);
            call rax;
            movq rax, r13;
        }
    }
}

impl BBContext {
    pub(super) fn jit_class_def(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BcPc,
    ) {
        self.fetch_slots(ir, &[superclass]);
        self.release(dst);
        let using_xmm = self.get_xmm_using();
        let error = ir.new_error(pc, self.get_write_back());
        ir.inst.push(AsmInst::ClassDef {
            superclass,
            dst,
            name,
            func_id,
            is_module,
            using_xmm,
            error,
        });
    }

    pub(super) fn jit_singleton_class_def(
        &mut self,
        ir: &mut AsmIr,
        dst: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
        pc: BcPc,
    ) {
        self.fetch_slots(ir, &[base]);
        self.release(dst);
        let using_xmm = self.get_xmm_using();
        let error = ir.new_error(pc, self.get_write_back());
        ir.inst.push(AsmInst::SingletonClassDef {
            base,
            dst,
            func_id,
            using_xmm,
            error,
        });
    }
}
