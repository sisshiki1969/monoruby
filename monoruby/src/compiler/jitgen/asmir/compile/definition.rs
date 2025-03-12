use super::*;

impl Codegen {
    /// Class definition
    ///
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |sup|   |dst| op||  name |       |
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    ///
    /// - sup: superclass (SlotId). If no superclass, sup is 0.
    /// - dst: destination slot (SlotId).
    /// - name: class name (IdentId)
    /// ~~~
    pub(super) fn class_def(
        &mut self,
        base: Option<SlotId>,
        superclass: Option<SlotId>,
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) {
        self.xmm_save(using_xmm);
        // r9 <- base: Option<Value>
        if let Some(base) = base {
            monoasm! { &mut self.jit, movq r9, [r14 - (conv(base))]; }
        } else {
            monoasm! { &mut self.jit, xorq r9, r9; }
        }
        // rcx <- superclass: Option<Value>
        if let Some(superclass) = superclass {
            monoasm! { &mut self.jit, movq rcx, [r14 - (conv(superclass))]; }
        } else {
            monoasm! { &mut self.jit, xorq rcx, rcx; }
        }
        // r8 <- is_module
        if is_module {
            monoasm! { &mut self.jit, movl r8, 1; }
        } else {
            monoasm! { &mut self.jit, xorq r8, r8; }
        }
        monoasm! { &mut self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value
        };
        self.handle_error(&error);
        self.jit_class_def_sub(func_id, dst, error);
        self.xmm_restore(using_xmm);
    }

    pub(super) fn singleton_class_def(
        &mut self,
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_singleton_class);
            call rax;  // rax <- self: Value
        };
        self.handle_error(&error);
        self.jit_class_def_sub(func_id, dst, error);
        self.xmm_restore(using_xmm);
    }

    pub(super) fn method_def(&mut self, name: IdentId, func_id: FuncId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq rax, (runtime::define_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    pub(super) fn singleton_method_def(
        &mut self,
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
    ) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq r8, [r14 - (conv(obj))];
            movq rax, (runtime::singleton_define_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn jit_class_def_sub(&mut self, func_id: FuncId, dst: Option<SlotId>, error: &DestLabel) {
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
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rdi;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], r15;
        }
        self.set_method_outer();
        monoasm! { &mut self.jit,
            movq r13 , [r8 + (FUNCDATA_PC)];
            movq rax, [r8 + (FUNCDATA_CODEPTR)];
            xorq rdx, rdx;
        }
        self.call_rax();
        self.store_rax(dst);
        // pop class context.
        monoasm! { &mut self.jit,
            movq r13, rax;
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::exit_classdef);
            call rax;
            movq rax, r13;
        }
        self.handle_error(&error);
    }
}
