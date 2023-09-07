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
        let slow_path = self.jit.label();
        let exec = self.jit.label();
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

        let label = self.vm_method_call_main(slow_path, exec, with_block, has_splat);

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r13 - 16];  // CallSiteId
            movq rcx, [rsp]; // receiver:Value
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<&FuncData>
        );
        self.vm_handle_error();
        monoasm!( &mut self.jit,
            movq [r13 + 8], rax;    // FuncData
            movl [r13 - 8], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 - 4], rdi;    // class_version
            jmp exec;
        );
        self.jit.select_page(0);

        label
    }

    /// Super
    ///
    /// ~~~text
    /// Super
    /// +---+---+---+---++---+---+---+---+
    /// | op|ret|callid || class |version|
    /// +---+---+---+---++---+---+---+---+
    /// MethodArgs
    /// +---+---+---+---++---+---+---+---+
    /// | op| - |arg|len||   func data   |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// operands
    /// ret:  return register
    /// id:   call site id
    /// arg:  the start of argument registers
    /// len:  the number of argument registers
    ///
    /// inline method cache
    /// class:    a class of the receiver
    /// version:  class version
    /// func data: the data of the function
    /// ~~~
    pub(super) fn vm_super(&mut self) -> CodePtr {
        let slow_path = self.jit.label();
        let exec = self.jit.label();
        let class_version = self.class_version;

        let label = self.vm_method_call_main(slow_path, exec, false, false);

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rsp];
            movq rax, (runtime::get_super_data);
            call rax;   // rax <- Option<&FuncData>
        );
        self.vm_handle_error();
        monoasm!( &mut self.jit,
            movq [r13 + 8], rax;    // FuncData
            movl [r13 - 8], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 - 4], rdi;    // class_version
            jmp exec;
        );
        self.jit.select_page(0);

        label
    }

    fn vm_method_call_main(
        &mut self,
        slow_path: DestLabel,
        exec: DestLabel,
        with_block: bool,
        has_splat: bool,
    ) -> CodePtr {
        let label = self.jit.get_current_address();
        let exit = self.jit.label();
        let class_version = self.class_version;
        let get_class = self.get_class;
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
        self.execute_gc();
        monoasm! { &mut self.jit,
            pushq r13;
            movzxw rdi, [r13 + 4];
        };
        self.vm_get_rdi();
        monoasm! { &mut self.jit,
            pushq rdi;
        }
        // rsp + 08:[pc]
        // rsp + 00:[recv:Value]
        // rdi: receiver: Value
        monoasm! { &mut self.jit,
            call get_class;
            movl r15, rax;
            cmpl r15, [r13 - 8];
            jne  slow_path;
            movl rdi, [r13 - 4];
            cmpl rdi, [rip + class_version];
            jne  slow_path;
        exec:
        };
        self.set_method_outer();
        monoasm! { &mut self.jit,
            // set meta
            movq r15, [r13 + 8];
            movq rdi, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rdi;
            movzxw rdi, [r13 + 0];  // rdi <- len
            movzxw rcx, [r13 + 2]; // rcx <- args
            // set self (= receiver)
            movq rax, [rsp];
            movq [rsp - (16 + LBP_SELF)], rax;
            movl r8, [r13 - 16]; // CallSiteId
        };
        self.set_frame(with_block, has_splat);
        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();
        monoasm! { &mut self.jit,
            movq rcx, [r13 + 8];
            movl rdx, [r13 - 16]; // CallSiteId
        }
        self.handle_arguments();
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rdx, rdi;
            movq r13, [r13 + 8];
            movq rax, [r13 + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_PC)];    // r13: BcPc
        };
        self.call_rax();
        monoasm! { &mut self.jit,
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 - 12];  // r15 <- :1
            addq r13, 16;
        };
        self.vm_handle_error();
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
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
        // r15: %ret
        // rdi: %args
        // rsi: len
        self.fetch3();
        monoasm! { &mut self.jit,
            // rsp + 08:[%ret]
            // rsp + 00:[pc]
            pushq rdi;
            pushq rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            lea  rdx, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_FUNCDATA))];
            movq rax, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_OUTER))];
            // rax <- outer_cfp, rdx <- &FuncData
            popq rdi;  // rdi <- len
            popq rcx;  // rcx <- %args
            pushq r15;
            pushq r13; // push pc
            movq r15, rdx;
            // set meta
            movq rsi, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rsi;
            movl r8, [r13 - 8];    // CallSiteId
        };
        self.set_block_self_outer();
        self.set_frame(false, true);
        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();
        monoasm! { &mut self.jit,
            movq rcx, r15;          // &FuncData
            movl rdx, [r13 - 8];    // CallSiteId
        }
        self.handle_arguments();
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            // argument registers:
            //   rdx: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rdx, rdi;
            // set codeptr
            movq rax, [r15 + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
        };
        self.call_rax();
        monoasm! { &mut self.jit,
            popq r13;   // pop pc
            popq r15;   // pop %ret
        };
        self.vm_handle_error();
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
    /// - r8:  CallSiteId
    ///
    /// ### out
    ///
    /// - rdi: arg len
    ///
    /// ### destroy
    ///
    /// - rax
    /// - rcx
    /// - rdx
    /// - rsi
    fn set_frame(&mut self, with_block: bool, has_splat: bool) {
        self.vm_get_addr_rcx(); // rcx <- *args
        if with_block {
            monoasm! { &mut self.jit,
                subq rcx, 8;
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
    /// - r8:  CallSiteId
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
    /// - rdx
    fn set_arguments(&mut self, has_splat: bool) {
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq rdi, rdi;
            jeq  loop_exit;
            // rdx <- len
            movl rdx, rdi;
            // rsi <- destination address
            lea  rsi, [rsp - (16 + LBP_ARG0)];
        }
        if has_splat {
            monoasm! { &mut self.jit,
                // TODO: this possibly cause problem.
                subq rsp, 4096;
                // rdi <- source address
                movq rdi, rcx;
                // rsi <- destination address
                // rdx <- len
                movq rcx, r12;
                movq rax, (vm_expand_splat);
                call rax;
                // rax <- length
                movq rdi, rax;
                addq rsp, 4096;
            };
        } else {
            monoasm! { &mut self.jit,
            loop_:
                // rax <- source value
                movq rax, [rcx];
                movq [rsi], rax;
                subq rsi, 8;
                subq rcx, 8;
                subl rdx, 1;
                jne  loop_;
            };
        }
        self.jit.bind_label(loop_exit);
    }
}
