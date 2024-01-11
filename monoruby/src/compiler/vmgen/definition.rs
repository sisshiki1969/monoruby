use super::*;

impl Codegen {
    /// Class definition
    ///
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |sup|bas|dst| op||  name |       |
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    ///
    /// - sup: superclass (SlotId). If no superclass, sup is 0.
    /// - base: base class Object(SlotId). If no base class, base is 0.
    /// - dst: destination slot (SlotId).
    /// - name: class name (IdentId)
    /// ~~~
    pub(super) fn vm_class_def(&mut self, is_module: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        if is_module {
            monoasm! { &mut self.jit,
                movl r8, 1;
            }
        } else {
            monoasm! { &mut self.jit,
                xorq r8, r8;
            }
        }
        self.vm_get_slot_value_if_nonzero(GP::Rdi);
        self.vm_get_slot_value_if_nonzero(GP::Rsi);
        monoasm! { &mut self.jit,
            movq r9, rdi; // r9 <- base: Value
            movq rcx, rsi; // rcx <- superclass: Option<Value>
            movl rdx, [r13 - 8];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value

        };
        self.class_def_sub();
        label
    }

    pub(super) fn vm_singleton_class_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let super_ = self.jit.label();
        self.fetch2();
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
        super_:
            movq rdx, rdi; // rdx <- base: Value
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_singleton_class);
            call rax;  // rax <- self: Value
        };
        self.class_def_sub();
        label
    }

    fn class_def_sub(&mut self) {
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            pushq r13;
            pushq r15;

            movq r15, rax; // r15 <- self
            movq rcx, rax; // rcx <- self
            movl rdx, [r13 - 4];  // rdx <- func_id
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::enter_classdef);
            call rax; // rax <- &FuncData

            movq r13, rax;
            movq rdi, [r13 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_SELF)], r15;
        };
        self.set_method_outer();
        monoasm! { &mut self.jit,
            movq rax, [r13 + (FUNCDATA_CODEPTR)];
            movq r13 , [r13 + (FUNCDATA_PC)];
            xorq rdx, rdx;
        };
        self.call_rax();
        // pop class context.
        monoasm!( &mut self.jit,
            movq r15, rax;
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::exit_classdef);
            call rax;
            movq rax, r15;
        );
        monoasm! { &mut self.jit,
            popq r15;
            popq r13;
        };
        self.vm_handle_error();
        self.vm_store_r15_if_nonzero();
        self.fetch_and_dispatch();
    }
}

impl Codegen {
    /// Method definition
    ///
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |   |   |   | op||  name |func_id|
    /// +---+---+---+---++---+---+---+---+
    ///
    /// - name: class name (IdentId)
    /// - func_id: (FuncId)
    /// ~~~
    pub(super) fn vm_method_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let raise = self.entry_raise;
        //self.fetch2();
        monoasm! { &mut self.jit,
            movl rdx, [r13 - 8];  // name
            movl rcx, [r13 - 4];  // func_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_method);
            call rax;
            testq rax, rax;
            jz   raise;
        };
        self.fetch_and_dispatch();
        label
    }

    /// Singleton method definition
    ///
    /// ~~~text
    /// -16 -14 -12 -10  -8  -6  -4  -2
    /// +---+---+---+---++---+---+---+---+
    /// |   |   |obj| op||  name |func_id|
    /// +---+---+---+---++---+---+---+---+
    ///
    /// - obj: singleton object (Value)
    /// - name: class name (IdentId)
    /// - func_id: (FuncId)
    /// ~~~
    pub(super) fn vm_singleton_method_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_val_r15();
        monoasm! { &mut self.jit,
            movq r8, r15;
            movl rdx, [r13 - 8];  // name
            movl rcx, [r13 - 4];  // func_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::singleton_define_method);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }
}
