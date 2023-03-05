use super::*;

impl Codegen {
    /// Call Method
    ///
    /// ~~~text
    /// MethodCall
    /// +---+---+---+---++---+---+---+---+
    /// | op|ret|callid || class |version|
    /// +---+---+---+---++---+---+---+---+
    /// MethodArgs
    /// +---+---+---+---++---+---+---+---+
    /// | op|rcv|arg|len||   func data   |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// operands
    /// ret:  return register
    /// id:   call site id
    /// rcv:  receiver register
    /// arg:  the start of argument registers
    /// len:  the number of argument registers
    ///
    /// inline method cache
    /// class:    a class of the receiver
    /// version:  class version
    /// func data: the data of the function
    /// ~~~
    pub(super) fn vm_method_call(&mut self, with_block: bool, has_splat: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let exit = self.jit.label();
        let slowpath = self.jit.label();
        let exec = self.jit.label();
        let vm_return = self.vm_return;
        let class_version = self.class_version;
        //      -16    -12    -8     -4
        //      +------+------+------+------+
        //      | MethodCall  |class | ver  |
        //      +------+------+------+------+
        // r13->| MethodArgs  |  FuncData   |
        //      +------+------+------+------+
        //
        // rdi: IdentId
        // r15: %ret
        // [r13 - 16]: CallSiteId
        // [r13 -  8]: class_id
        // [r13 -  4]: class_version
        // [r13 +  0]; len
        // [r13 +  2]; %args
        // [r13 +  4]: %recv
        // [r13 +  8]: FuncData

        monoasm! { self.jit,
            pushq r15;
            pushq r13;
            pushq rdi;
            movzxw rdi, [r13 + 4];
        };
        self.vm_get_rdi();
        monoasm! { self.jit,
            pushq rdi;
        }
        // rsp + 24:[%ret]
        // rsp + 16:[pc]
        // rsp + 08:[callid:CallSiteId]
        // rsp + 00:[recv:Value]

        self.execute_gc();
        // rdi: receiver: Value
        monoasm! { self.jit,
            movq rdi, [rsp];
            movq rax, (Value::get_class);
            call rax;
            movl r15, rax;
            cmpl r15, [r13 - 8];
            jne  slowpath;
            movl rdi, [r13 - 4];
            cmpl rdi, [rip + class_version];
            jne  slowpath;
        exec:
        };
        self.set_method_outer();
        monoasm! { self.jit,
            // set meta
            movq rdi, [r13 + 8];
            movq rdi, [rdi + (FUNCDATA_OFFSET_META)];
            movq [rsp -(16 + LBP_META)], rdi;
            movzxw rdi, [r13 + 0];  // rdi <- len
            movzxw rcx, [r13 + 2]; // rcx <- args
            // set self (= receiver)
            movq rax, [rsp];
            movq [rsp - (16 + LBP_SELF)], rax;
        };
        self.set_frame(with_block, has_splat);
        monoasm! { self.jit,
            lea  r8, [rsp - (16 + LBP_SELF)];
            movq r9, rdi;
            movl rcx, [rsp - (16 + LBP_META)];
            subq rsp, 4096;
            movq rdi, r12; // &Globals
            movl rsi, [r13 - 16]; // CallSiteId
            lea  rdx, [r14 - (LBP_SELF)];
            movq rax, (runtime::vm_handle_arguments);
            call rax;
            movq rdi, rax;
            addq rsp, 4096;
            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq r13, [r13 + 8];
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];    // r13: BcPc
        };
        self.call_rax();
        monoasm! { self.jit,
            addq rsp, 16;
            popq r13;   // pop pc
            popq r15;   // pop %ret
            addq r13, 16;
            testq rax, rax;
            jeq vm_return;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();

        self.jit.select_page(1);
        monoasm!(self.jit,
        slowpath:
            movq rdi, r12;
            movq rsi, [rsp + 8];  // rsi: CallSiteId
            movzxw rdx, [r13];  // rdx: len
            movq rcx, [rsp]; // rcx: receiver:Value
            movzxw r8, [r13 +  4];
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<&FuncData>
            testq rax, rax;
            jeq vm_return;
            movq [r13 + 8], rax;    // FuncData
            movl [r13 - 8], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 - 4], rdi;    // class_version
            jmp exec;
        );
        self.jit.select_page(0);

        label
    }

    /// Yield
    ///
    /// ~~~text
    /// Yield
    /// +---+---+---+---++---+---+---+---+
    /// | op|ret|arg|len||               |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// ret:  return register
    /// arg:  the start of argument registers
    /// len:  the number of argument registers
    /// ~~~
    pub(super) fn vm_yield(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let exit = self.jit.label();
        let vm_return = self.vm_return;
        let no_block = self.no_block;
        // r15: %ret
        // rdi: %args
        // rsi: len

        monoasm! { self.jit,
            // rsp + 08:[%ret]
            // rsp + 00:[pc]
            pushq rdi;
            pushq rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
            // rax <- outer_cfp, rdx <- &FuncData
            popq rdi;  // rdi <- len
            popq rcx;  // rcx <- %args
            testq rax, rax;
            jz  no_block;
            pushq r15;
            pushq r13; // push pc
            // set meta
            movq rsi, [rdx + (FUNCDATA_OFFSET_META)];
            movq [rsp -(16 + LBP_META)], rsi;
        };
        self.set_block_self_outer();
        self.set_frame(false, true);
        monoasm! { self.jit,
            lea  r8, [rsp - (16 + LBP_SELF)];
            movq r9, rdi;
            movl rcx, [rsp - (16 + LBP_META)];
            subq rsp, 4096;
            //pushq rdi;
            movq rdi, r12; // &Globals
            movl rsi, [r13 - 8]; // CallSiteId
            movq r13, rdx;
            lea  rdx, [r14 - (LBP_SELF)];
            movq rax, (runtime::vm_handle_arguments);
            call rax;
            movq rdi, rax;
            addq rsp, 4096;
            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            // set codeptr
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];
        };
        self.call_rax();
        monoasm! { self.jit,
            popq r13;   // pop pc
            popq r15;   // pop %ret
            testq rax, rax;
            jeq vm_return;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
        label
    }

    /// Set frame (BLOCK, arguments)
    ///
    /// ### in
    ///
    /// - rdi: arg len
    /// - rcx: %args
    ///
    /// ### out
    ///
    /// - rdi: arg len
    ///
    /// ### destroy
    ///
    /// - rax
    /// - rcx
    /// - rsi
    /// - r15
    fn set_frame(&mut self, with_block: bool, has_splat: bool) {
        self.vm_get_addr_rcx(); // rcx <- *args
        if with_block {
            // set block
            monoasm! { self.jit,
                movq rax, [rcx];
                movq [rsp - (16 + LBP_BLOCK)], rax;
                subq rcx, 8;
            };
        } else {
            monoasm! { self.jit,
                // set block
                movq [rsp - (16 + LBP_BLOCK)], 0;
            };
        }
        self.set_arguments(has_splat);
    }

    /// Set arguments
    ///
    /// ### in
    ///
    /// - rdi: arg len
    /// - rcx: the first argument address
    ///
    /// ### out
    ///
    /// - rdi: arg len
    ///
    /// ### destroy
    ///
    /// - rax
    /// - rcx
    /// - rsi
    /// - r15
    fn set_arguments(&mut self, has_splat: bool) {
        let loop_ = self.jit.label();
        let no_splat = self.jit.label();
        let next = self.jit.label();
        let loop_exit = self.jit.label();
        monoasm! { self.jit,
            testq rdi, rdi;
            jeq  loop_exit;
            movl r15, rdi;
            // rsi <- destination address
            lea  rsi, [rsp - (16 + LBP_ARG0)];
            // TODO: this possibly cause problem.
            subq rsp, 4096;
        loop_:
            // rax <- source value
            movq rax, [rcx];
        }
        if has_splat {
            monoasm! { self.jit,
                // check whether the source value is SPLAT.
                testq rax, 0b111;
                jne  no_splat;
                cmpw [rax + 2], (ObjKind::SPLAT);
                jne  no_splat;
                pushq rdi;
                pushq rsi;
                pushq rdx;
                pushq rcx;
                // rdi <- source value
                // rsi <- destination address
                movq rdi, rax;
                movq rax, (expand_splat);
                call rax;
                // rax <- length
                popq rcx;
                popq rdx;
                popq rsi;
                popq rdi;
                lea  rdi, [rdi + rax * 1 - 1];
                shlq rax, 3;
                subq rsi, rax;
                jmp next;
                no_splat:
            }
        }
        monoasm! { self.jit,
            movq [rsi], rax;
            subq rsi, 8;
        next:
            subq rcx, 8;
            subl r15, 1;
            jne  loop_;
            addq rsp, 4096;
        loop_exit:
        };
    }
}
