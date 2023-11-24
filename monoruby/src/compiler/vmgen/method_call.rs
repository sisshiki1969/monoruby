use super::*;

const CALLSITE_ID: i64 = 0 - 16;
const CACHED_CLASS: i64 = 24 - 16;
const CACHED_VERSION: i64 = 28 - 16;
const CACHED_FUNCID: i64 = 16 - 16;
const RET_REG: i64 = 4 - 16;
const POS_NUM: i64 = 8 - 16;
const ARG_REG: i64 = 10 - 16;
const RECV_REG: i64 = 12 - 16;

impl Codegen {
    ///
    /// Call Method
    ///
    /// ~~~text
    /// MethodCall
    ///  0   2   4   6    8  10  12  14
    /// +---+---+---+---++---+---+---+---+
    /// |callid |ret| op||pos|arg|rcv| - |
    /// +---+---+---+---++---+---+---+---+
    /// 16  18  20  22   24  26  28  30
    /// +---+---+---+---++---+---+---+---+
    /// |  fid  |   | op|| class |version|
    /// +---+---+---+---++---+---+---+---+
    ///
    /// operands
    /// ret:  return register
    /// callid:   call site id
    /// rcv:  receiver register
    /// arg:  the start of argument registers
    /// len:  the number of argument registers
    ///
    /// inline method cache
    /// class:    a class of the receiver
    /// version:  class version
    /// fid:      FuncId
    /// ~~~
    pub(super) fn vm_call(&mut self, has_splat: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let exec = self.jit.label();
        let exit = self.jit.label();
        let slow_path = self.jit.label();
        let class_version = self.class_version;
        let get_class = self.get_class;
        self.execute_gc();
        monoasm! { &mut self.jit,
            pushq r13;
            subq  rsp, 8;
            movzxw rdi, [r13 + (RECV_REG)];
        };
        self.vm_get_rdi();
        monoasm! { &mut self.jit,
            // set self (= receiver)
            movq [rsp - (16 + LBP_SELF)], rdi;
        }
        // rdi: receiver: Value
        monoasm! { &mut self.jit,
            call get_class;
            movl r15, rax;
            cmpl r15, [r13 + (CACHED_CLASS)];
            jne  slow_path;
            movl rdi, [r13 + (CACHED_VERSION)];
            cmpl rdi, [rip + class_version];
            jne  slow_path;
        exec:
        };
        self.set_method_outer();
        monoasm! { &mut self.jit,
            movl rdx, [r13 + (CACHED_FUNCID)];
        }
        self.get_func_data();
        monoasm! { &mut self.jit,
            // set meta
            movq r15, rdx;
            movq rdi, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rdi;
            movzxw rdi, [r13 + (POS_NUM)];  // rdi <- len
            movzxw rcx, [r13 + (ARG_REG)]; // rcx <- args
            movl r8, [r13 + (CALLSITE_ID)]; // CallSiteId
        };

        self.set_arguments(has_splat);
        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();
        monoasm! { &mut self.jit,
            movl rdx, [r13 + (CALLSITE_ID)]; // CallSiteId
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
            movq rax, [r15 + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];    // r13: BcPc
        };
        self.call_rax();
        monoasm! { &mut self.jit,
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 + (RET_REG)];  // r15 <- :1
            addq r13, 16;
        };
        self.vm_handle_error();
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();

        self.slow_path(exec, slow_path);

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
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            lea  r15, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_FUNCDATA))];
            movq rax, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_OUTER))];
            // rax <- outer_cfp, r15 <- &FuncData
            movsxw rdi, [r13 - 16]; // rdi <- len
            movzxw rcx, [r13 - 14]; // rcx <- %args
            pushq r13; // push pc
            subq rsp, 8;
            // set meta
            movq rsi, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rsi;
            movl r8, [r13 - 8];    // CallSiteId
        };
        self.set_block_self_outer();

        self.set_arguments(true);
        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();
        monoasm! { &mut self.jit,
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
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 - 12]; // r15 <- %ret
        };
        self.vm_handle_error();
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
        label
    }

    fn slow_path(&mut self, exec: DestLabel, slow_path: DestLabel) {
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 + (CALLSITE_ID)];  // CallSiteId
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<FuncId>
        );
        self.vm_handle_error();
        self.save_cache(exec);
        self.jit.select_page(0);
    }

    ///
    /// Save inline method cache.
    ///
    /// ### in
    /// - rax: FuncId
    /// - r15: ClassId of receiver
    ///
    fn save_cache(&mut self, exec: DestLabel) {
        let class_version = self.class_version;
        monoasm!( &mut self.jit,
            movl [r13 + (CACHED_FUNCID)], rax;    // FuncId
            movl [r13 + (CACHED_CLASS)], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 + (CACHED_VERSION)], rdi;    // class_version
            jmp exec;
        );
    }

    /// Set arguments
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
    /// - rsi
    /// - rdx
    fn set_arguments(&mut self, has_splat: bool) {
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        self.vm_get_addr_rcx(); // rcx <- *args
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