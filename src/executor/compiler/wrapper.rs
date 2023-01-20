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
    fn gen_jit_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        let counter = self.jit.const_i32(5);
        let entry = self.jit.label();
        let next = self.jit.label();
        monoasm!(self.jit,
        entry:
            jmp  next;
        next:
            subl [rip + counter], 1;
            jne vm_entry;
            movl rsi, [rsp - (8 + LBP_META_FUNCID)];
            movq rdx, [rsp - (8 + LBP_SELF)];
            subq rsp, 1024;
            pushq rdi;
            movq rdi, r12;
            movq rax, (exec_jit_compile);
            call rax;
            lea rdi, [rip + entry];
            addq rdi, 5;
            subq rax, rdi;
            movl [rdi - 4], rax;
            popq rdi;
            addq rsp, 1024;
            jmp entry;
        );
        codeptr
    }

    fn gen_vm_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        monoasm!(self.jit,
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
    ///   rdi: number of args
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
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            movq r8, rdi;
            movq rax, rdi;
        );
        self.calc_offset();
        monoasm!(self.jit,
            subq rsp, rax;
            lea  rcx, [r14 - (LBP_ARG0)];     // rcx <- *const arg[0]
            movq  r9, [r14 - (LBP_BLOCK)];     // r9 <- block
            movq  rdx, [r14 - (LBP_SELF)];    // rdx <- self
            // we should overwrite reg_num because the func itself does not know actual number of arguments.
            movw [r14 - (LBP_META_REGNUM)], rdi;

            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (abs_address);
            // fn(&mut Interp, &mut Globals, Value, *const Value, len:usize, block:Option<Value>)
            call rax;

            leave;
            ret;
        );
        label
    }

    ///
    /// Generate attr_reader.
    ///
    /// - stack layout at the point of just after being called.
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
    /// ~~~
    fn gen_attr_reader(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(0);
        monoasm!(self.jit,
            movq rdi, [rsp - (8 + LBP_SELF)];  // self: Value
            movq rsi, (ivar_name.get()); // name: IdentId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cached_class];
            lea  r8, [rip + cached_ivarid];
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
    /// - stack layout at the point of just after being called.
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
    /// -0x20 |   %1(val)   |
    ///       +-------------+
    /// ~~~
    fn gen_attr_writer(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(0);
        monoasm!(self.jit,
            movq rdi, r12; //&mut Globals
            movq rsi, [rsp - (8 + LBP_SELF)];  // self: Value
            movq rdx, (ivar_name.get()); // name: IdentId
            movq rcx, [rsp - (8 + LBP_ARG0)];  //val: Value
            lea  r8, [rip + cached_class];
            lea  r9, [rip + cached_ivarid];
            movq rax, (set_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
        label
    }
}