use super::*;

const CALLSITE_ID: i64 = 0 - 16;
const CACHED_CLASS: i64 = 24 - 16;
const CACHED_VERSION: i64 = 28 - 16;
const CACHED_FUNCID: i64 = 8 - 16;
const RET_REG: i64 = 4 - 16;
const OPCODE_SUB: i64 = 7 - 16;
const POS_NUM: i64 = 0;
const ARG_REG: i64 = 18 - 16;
const RECV_REG: i64 = 20 - 16;

impl Codegen {
    ///
    /// Call Method
    ///
    /// ~~~text
    /// MethodCall
    /// 0   2   4   6    8   10  12  14
    /// +---+---+---+---++---+---+---+---+
    /// |callid |ret| op||  fid  |   -   |
    /// +---+---+---+---++---+---+---+---+
    /// InlineCache
    /// 16  18  20  22   24  26  28  30
    /// +---+---+---+---++---+---+---+---+
    /// |pos|arg|rcv| op|| class |version|
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
    pub(super) fn vm_call(&mut self, is_simple: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let exec = self.jit.label();
        let slow_path1 = self.jit.label();
        let slow_path2 = self.jit.label();
        let method_missing = self.jit.label();
        let class_version = self.class_version_label();
        let get_class = self.get_class.clone();
        self.vm_execute_gc();
        monoasm! { &mut self.jit,
            pushq r13;
            subq  rsp, 8;
            // set self (= receiver)
            movzxw rdi, [r13 + (RECV_REG)];
        };
        // rdi: receiver: Value
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rdi;
            call get_class;
            movq rcx, rdi;
            movl r15, rax;
            // r15: class of receiver: ClassId
            cmpl r15, [r13 + (CACHED_CLASS)];
            jne  slow_path1;
            movl rdi, [r13 + (CACHED_VERSION)];
            cmpl rdi, [rip + class_version];
            jne  slow_path2;
        exec:
            movl rdx, [r13 + (CACHED_FUNCID)];
            testq rdx, rdx;
            jeq  method_missing;
        };
        self.get_func_data();
        self.set_method_outer();
        self.call(is_simple);
        monoasm! { &mut self.jit,
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 + (RET_REG)];  // r15 <- :1
            addq r13, 16;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();

        self.slow_path(&exec, &slow_path1, &slow_path2);

        let raise = self.entry_raise();
        // r15: class of receiver: ClassId
        monoasm! { &mut self.jit,
        method_missing:
            jmp  raise;
        };

        label
    }

    /// Yield
    ///
    /// ~~~text
    /// Yield
    /// 0   2   4   6    8   10  12  14
    /// +---+---+---+---++---+---+---+---+
    /// |callid |ret| op||  fid  |   -   |
    /// +---+---+---+---++---+---+---+---+
    /// InlineCache
    /// 16  18  20  22   24  26  28  30
    /// +---+---+---+---++---+---+---+---+
    /// |pos|arg|rcv| op|| class |version|
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    pub(super) fn vm_yield(&mut self, is_simple: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_execute_gc();
        self.get_proc_data();
        // rax: outer, rdx: FuncId
        self.vm_handle_error();
        self.get_func_data();
        // rax: outer, r15: &FuncData
        monoasm! { &mut self.jit,
            pushq r13; // push pc
            subq rsp, 8;
        };
        self.set_block_self_outer();
        self.call(is_simple);
        monoasm! { &mut self.jit,
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 + (RET_REG)];  // r15 <- :1
            addq r13, 16;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Preparation for call.
    ///
    /// ### in
    /// - r13: pc
    ///
    /// ### out
    /// - r8: CallSiteId
    /// - r9: pos_num
    /// - rdx: *const args
    ///
    fn call_prep(&mut self) {
        monoasm! { &mut self.jit,
            movl r8, [r13 + (CALLSITE_ID)]; // r8 <- CallSiteId
        }
    }

    ///
    /// Call
    ///
    /// ### in
    /// - r13: pc
    /// - r15: &FuncData
    ///
    fn call(
        &mut self,
        // The call site has no keyword arguments, no splat arguments, no hash splat arguments, and no block argument.
        is_simple: bool,
    ) {
        monoasm! { &mut self.jit,
            // set meta
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rax;
        }
        monoasm! { &mut self.jit,
            movzxw r9, [r13 + (POS_NUM)];
            movzxw rdx, [r13 + (ARG_REG)];
        }
        self.vm_get_slot_addr(GP::Rdx);
        if is_simple {
            // if callee is "simple" (has no optional parameter, no rest parameter, no keyword parameters,
            // no keyword rest parameter), no single arg expansion, and req == pos_num == req_opt,
            // we can optimize this.
            let generic = self.jit.label();
            let exit = self.jit.label();
            let loop_ = self.jit.label();
            //let opt = self.jit.label();
            // rax: Meta
            // r9: number of positional arguments passed to callee
            // rdx: *const args
            //self.guard_simple_call(GP::Rax, GP::R9, GP::Rdx, opt, generic);
            monoasm! { &mut self.jit,
            //opt:
                // check Meta. if !is_simple || is_block_style, go to generic.
                shrq rax, 56;
                testq rax, 0b1_0000;
                jz  generic;
                cmpw r9, [r15 + (FUNCDATA_MIN)];
                jne  generic;
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
                // rdx : *args
                // r9 : len
                testq r9, r9;
                jeq  exit;
                negq r9;
            loop_:
                movq rax, [rdx + r9 * 8 + 8];
                movq [rsp + r9 * 8 - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
                addq r9, 1;
                jne  loop_;
            exit:
            };
            self.jit.select_page(1);
            self.jit.bind_label(generic);
            self.call_prep();
            self.generic_handle_arguments(runtime::vm_handle_arguments);
            self.vm_handle_error();
            monoasm! { &mut self.jit,
                jmp exit;
            }
            self.jit.select_page(0);
        } else {
            self.call_prep();
            self.generic_handle_arguments(runtime::vm_handle_arguments);
            self.vm_handle_error();
        }
        self.call_funcdata();
        //monoasm! { &mut self.jit,
        //    addq rsp, 8;
        //    popq r13;   // pop pc
        //    movzxw r15, [r13 + (RET_REG)];  // r15 <- :1
        //    addq r13, 16;
        //};
        //self.vm_handle_error();
        //self.vm_store_r15(GP::Rax);
    }

    ///
    /// Generate slow path.
    ///
    /// When the receiver class is cached **and** the receiver class is different from the cached class,
    /// opcode_sub is set to 1.
    ///
    /// ### in
    /// - r15: ClassId of receiver
    /// - rcx: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn slow_path(&mut self, exec: &DestLabel, slow_path1: &DestLabel, slow_path2: &DestLabel) {
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
            // receiver mismatch
        slow_path1:
            movl rax, [r13 + (CACHED_FUNCID)];
            testq rax, rax;
            // if receiver class was not cached, go to slow_path2.
            je   slow_path2;
            // if the receiver class was different from cached class
            movb [r13 + (OPCODE_SUB)], 1;
            // version mismatch
        slow_path2:
            subq rsp, 1024;
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 + (CALLSITE_ID)];  // CallSiteId
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<FuncId>
            movl rax, rax;
            addq rsp, 1024;
        );
        //self.vm_handle_error();
        self.save_cache();
        monoasm!( &mut self.jit,
            jmp exec;
        );
        self.jit.select_page(0);
    }

    ///
    /// Save inline method cache.
    ///
    /// ### in
    /// - rax: FuncId
    /// - r15: ClassId of receiver
    ///
    fn save_cache(&mut self) {
        let class_version = self.class_version_label();
        monoasm!( &mut self.jit,
            movl [r13 + (CACHED_FUNCID)], rax;    // FuncId
            movl [r13 + (CACHED_CLASS)], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 + (CACHED_VERSION)], rdi;    // class_version
        );
    }
}
