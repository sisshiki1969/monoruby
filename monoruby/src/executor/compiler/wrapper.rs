use super::*;

impl Codegen {
    pub(in crate::executor) fn gen_wrapper(&mut self, kind: FuncKind, no_jit: bool) -> CodePtr {
        let codeptr = match kind {
            FuncKind::ISeq(_) => {
                if !no_jit {
                    self.gen_jit_stub()
                } else {
                    self.gen_vm_stub()
                }
            }
            FuncKind::Builtin { abs_address } => self.wrap_native_func(abs_address),
            FuncKind::AttrReader { ivar_name } => self.gen_attr_reader(ivar_name),
            FuncKind::AttrWriter { ivar_name } => self.gen_attr_writer(ivar_name),
        };
        self.jit.finalize();
        codeptr
    }

    ///
    /// Set jit compilation stub code for an entry point of each Ruby methods.
    ///
    /// ```text
    ///
    ///       wrapper                               class guard stub
    ///    +-------------------------------+     +------------------------------------------+
    ///    |                               |     |                                          |        JIT code for self_class
    /// ---+-> entry:                      |  /--+-> guard:                                 |     +---------------------------+
    ///    |     jmp [next]; --------------+-/   |     movq rdi, [r14 - (LBP_SELF)];        |     |                           |
    ///    |   next:                       |     |     <class_guard(self_class, vm_entry)>  |  /--+-> jit_code_body:          |
    ///    |     subl [rip + counter], 1;  |     |   jit_entry:                             |  |  |     <jit_code>            |
    ///    |     jne vm_entry;             |     |     jmp [jit_code_body]; ----------------+--/  |                           |
    ///    |     <exec_compile_and_patch>  |     |                                          |     |                           |
    ///    |     jmp entry;                |     +------------------------------------------+     
    ///    |                               |
    ///    +-------------------------------+
    ///
    /// ```
    fn gen_jit_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        let counter = self.jit.const_i32(5);
        let entry = self.jit.label();
        let next = self.jit.label();
        monoasm!( &mut self.jit,
        entry:
            jmp  next;
        next:
            subl [rip + counter], 1;
            jne vm_entry;
            movq rdi, r12;
            movl rsi, [r14 - (LBP_META_FUNCID)];
            movq rdx, [r14 - (LBP_SELF)];
            movq rcx, (entry.to_usize());
            subq rsp, 1032;
            movq rax, (exec_jit_compile_patch);
            call rax;
            addq rsp, 1032;
            jmp entry;
        );
        codeptr
    }

    fn gen_vm_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        monoasm!( &mut self.jit,
            jmp vm_entry;
        );
        codeptr
    }

    ///
    /// Generate a wrapper for a native function with C ABI.
    ///
    /// - stack layout at the point of just after a wrapper was called.
    /// ~~~text
    ///       +-------------+
    ///  0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |             |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |  %0 (self)  |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///
    ///  meta
    /// +-------------------+ -0x08
    /// |     2:Native      |
    /// +-------------------+ -0x0a
    /// |    register_len   |
    /// +-------------------+ -0x0c
    /// |                   |
    /// +      FuncId       + -0x0e
    /// |                   |
    /// +-------------------+ -0x10
    ///
    /// argument registers:
    ///   rdx: number of args
    ///
    /// global registers:
    ///   rbx: &mut Interp
    ///   r12: &mut Globals
    ///   r13: pc (dummy for builtin funcions)
    /// ~~~
    ///
    fn wrap_native_func(&mut self, abs_address: u64) -> CodePtr {
        let label = self.jit.get_current_address();
        // calculate stack offset
        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            movq r8, rdx;
            movq rax, rdx;
        );
        self.calc_offset();
        monoasm!( &mut self.jit,
            subq rsp, rax;
            lea  rcx, [r14 - (LBP_ARG0)];     // rcx <- *const arg[0]
            // we should overwrite reg_num because the func itself does not know actual number of arguments.
            movw [r14 - (LBP_META_REGNUM)], rdx;

            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, r14;    // rdx <- lfp
            movq rax, (abs_address);
            call rax;

            leave;
            ret;
        );
        label
    }

    ///
    /// Generate attr_reader.
    ///
    fn gen_attr_reader(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cache = self.jit.const_i64(-1);
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // self: Value
            movq rsi, (ivar_name.get()); // name: IdentId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cache];
            movq rax, (get_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
        label
    }

    ///
    /// Generate attr_writer.
    ///
    fn gen_attr_writer(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cache = self.jit.const_i64(-1);
        monoasm!( &mut self.jit,
            movq rdi, rbx; //&mut Executor
            movq rsi, r12; //&mut Globals
            movq rdx, [r14 - (LBP_SELF)];  // self: Value
            movq rcx, (ivar_name.get()); // name: IdentId
            movq r8, [r14 - (LBP_ARG0)];  //val: Value
            lea  r9, [rip + cache];
            movq rax, (set_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
        label
    }
}