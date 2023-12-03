use super::*;

impl Codegen {
    pub(super) fn jit_class_def(
        &mut self,
        ctx: &mut BBContext,
        ret: Option<SlotId>,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        if superclass.is_zero() {
            monoasm! { &mut self.jit,
                xorq rcx, rcx;
            }
        } else {
            monoasm! { &mut self.jit,
                movq rcx, [r14 - (conv(superclass))];  // rcx <- superclass: Option<Value>
            }
        }
        if is_module {
            monoasm! { &mut self.jit,
                movl r8, 1; // r8 <- is_module
            }
        } else {
            monoasm! { &mut self.jit,
                xorq r8, r8;
            }
        }
        monoasm! { &mut self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value
        };
        self.jit_handle_error(ctx, pc);
        self.jit_class_def_sub(ctx, func_id, ret, pc);
        self.xmm_restore(xmm_using);
    }

    pub(super) fn jit_singleton_class_def(
        &mut self,
        ctx: &mut BBContext,
        ret: Option<SlotId>,
        base: SlotId,
        func_id: FuncId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_singleton_class);
            call rax;  // rax <- self: Value
        };
        self.jit_handle_error(ctx, pc);
        self.jit_class_def_sub(ctx, func_id, ret, pc);
        self.xmm_restore(xmm_using);
    }

    fn jit_class_def_sub(
        &mut self,
        ctx: &mut BBContext,
        func_id: FuncId,
        ret: Option<SlotId>,
        pc: BcPc,
    ) {
        self.writeback_acc(ctx);
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
        self.jit_handle_error(ctx, pc);
    }
}
