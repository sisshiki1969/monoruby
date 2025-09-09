use super::*;

impl Codegen {
    pub(crate) fn gen_wrapper(&mut self, globals: &Globals, fid: FuncId) -> DestLabel {
        let no_jit = globals.no_jit;
        let entry = self.jit.label();
        self.jit.bind_label(entry.clone());
        match &globals.store[fid].kind {
            FuncKind::ISeq(_) => {
                if !no_jit && !cfg!(feature = "no-jit") {
                    self.gen_jit_stub();
                } else {
                    self.gen_vm_stub()
                }
            }
            FuncKind::Proc(proc) => {
                //self.vm_execute_gc();
                monoasm! { &mut self.jit,
                    movl rdx, (proc.func_id().get());
                    movq rax, (proc.outer_lfp().as_ptr());
                }
                // rax: outer, rdx: FuncId
                self.get_func_data();
                // rax: outer, r15: &FuncData
                monoasm! { &mut self.jit,
                    // set outer
                    movq [r14 - (LFP_OUTER)], rax;
                    // use given self
                    // set meta
                    movq rax, [r15 + (FUNCDATA_META)];
                    movq [r14 - (LFP_META)], rax;
                    // set pc
                    movq r13, [r15 + (FUNCDATA_PC)];
                    jmp [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
                }
            }
            FuncKind::Builtin { abs_address } => self.gen_native_func_wrapper(*abs_address),
            FuncKind::AttrReader { ivar_name } => self.gen_attr_reader(*ivar_name),
            FuncKind::AttrWriter { ivar_name } => self.gen_attr_writer(*ivar_name),
            FuncKind::Const { value } => self.gen_const_fn(value),
        };
        self.jit.finalize();
        entry
    }

    ///
    /// Set jit compilation stub code for an entry point of each Ruby methods.
    ///
    fn gen_jit_stub(&mut self) {
        let vm_entry = self.vm_entry();
        let entry = self.jit.label();
        self.gen_compile_patch(&vm_entry, &entry, COUNT_START_COMPILE);
    }

    fn gen_vm_stub(&mut self) {
        let vm_entry = self.vm_entry();
        monoasm!( &mut self.jit,
            jmp vm_entry;
        );
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
    fn gen_native_func_wrapper(&mut self, abs_address: u64) {
        // calculate stack offset
        monoasm!( &mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            movzxw rax, [r14 - (LFP_META_REGNUM)];
            addq rax, ((RSP_LOCAL_FRAME + LFP_ARG0) / 8 + 1);
            andq rax, (-2);
            shlq rax, 3;
            subq rsp, rax;

            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, r14;    // rdx <- lfp
            movq rax, (abs_address);
            call rax;   // CALL_SITE

            leave;
            ret;
        );
    }

    ///
    /// Generate a function that always returns the constant value.
    ///
    fn gen_const_fn(&mut self, value: &Value) {
        monoasm!( &mut self.jit,
            movq rax, (value.id());
            ret;
        );
    }

    ///
    /// Generate attr_reader.
    ///
    fn gen_attr_reader(&mut self, ivar_name: IdentId) {
        let cache = self.jit.data_i64(-1);
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LFP_SELF)];  // self: Value
            movq rsi, (ivar_name.get()); // name: IdentId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cache];
            movq rax, (get_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
    }

    ///
    /// Generate attr_writer.
    ///
    fn gen_attr_writer(&mut self, ivar_name: IdentId) {
        let cache = self.jit.data_i64(-1);
        monoasm!( &mut self.jit,
            movq rdi, rbx; //&mut Executor
            movq rsi, r12; //&mut Globals
            movq rdx, [r14 - (LFP_SELF)];  // self: Value
            movq rcx, (ivar_name.get()); // name: IdentId
            movq r8, [r14 - (LFP_ARG0)];  //val: Value
            lea  r9, [rip + cache];
            movq rax, (set_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
    }
}
