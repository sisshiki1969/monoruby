use super::*;

impl Codegen {
    ///
    /// Check if `yield` is callable.
    ///
    /// return "yield" if callable, `nil` if not.
    ///
    pub(super) fn defined_yield(&mut self, dst: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_yield);
            call rax;
            lea  rdi, [r14 - (conv(dst))];
            movq [rdi], rax;
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

    ///
    /// Check if `super` is callable.
    ///
    /// Set `dst` to "super" if callable, `nil` if not.
    ///
    pub(super) fn defined_super(&mut self, dst: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_super);
            call rax;
            lea  rdi, [r14 - (conv(dst))];
            movq [rdi], rax;
        };
        self.xmm_restore(using_xmm);
    }

    ///
    /// Check if global var `name` exists.
    ///
    /// Set `dst`` to "global-variable" if exists, `nil` if not.
    ///
    pub(super) fn defined_gvar(&mut self, dst: SlotId, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movl rdx, (name.get());
            movq rax, (runtime::defined_gvar);
            call rax;
            lea  rdi, [r14 - (conv(dst))];
            movq [rdi], rax;
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
