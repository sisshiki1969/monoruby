use super::*;

impl Codegen {
    pub(super) fn defined_yield(&mut self, dst: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            lea  rdx, [r14 - (conv(dst))];
            movq rax, (runtime::defined_yield);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn defined_const(&mut self, dst: SlotId, siteid: ConstSiteId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            lea  rdx, [r14 - (conv(dst))];
            movl rcx, (siteid.0);
            movq rax, (runtime::defined_const);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn defined_method(
        &mut self,
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    ) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            lea  rdx, [r14 - (conv(dst))];
            movq rcx, [r14 - (conv(recv))];
            movl r8, (name.get());
            movq rax, (runtime::defined_method);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn defined_gvar(&mut self, dst: SlotId, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            lea  rdx, [r14 - (conv(dst))];
            movl rcx, (name.get());
            movq rax, (runtime::defined_gvar);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn defined_ivar(&mut self, dst: SlotId, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            lea  rdx, [r14 - (conv(dst))];
            movl rcx, (name.get());
            movq rax, (runtime::defined_ivar);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }
}
