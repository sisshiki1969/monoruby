use super::*;

impl Codegen {
    pub(crate) fn gen_wrapper(&mut self, globals: &Globals, fid: FuncId) -> DestLabel {
        let no_jit = globals.no_jit;
        let entry = self.jit.label();
        self.jit.bind_label(entry.clone());
        match &globals.store[fid].kind {
            FuncKind::ISeq(iseq) => {
                match globals.store[*iseq].hint {
                    ISeqHint::ConstReturn(imm) => {
                        // Trivial method: return constant immediately without
                        // frame creation or bytecode execution.
                        monoasm!( &mut self.jit,
                            movq rax, (imm.id());
                            ret;
                        );
                    }
                    ISeqHint::SelfReturn => {
                        // Trivial method: return self (LFP_SELF) immediately.
                        monoasm!( &mut self.jit,
                            movq rax, [r14 - (LFP_SELF)];
                            ret;
                        );
                    }
                    ISeqHint::Normal => {
                        if !no_jit && !cfg!(feature = "no-jit") {
                            self.gen_jit_stub();
                        } else {
                            self.gen_vm_stub()
                        }
                    }
                }
            }
            FuncKind::Proc(proc) => {
                //self.vm_execute_gc();
                let outer_ptr = match proc.outer_lfp() {
                    Some(outer) => outer.as_ptr() as usize,
                    None => 0,
                };
                monoasm! { &mut self.jit,
                    movl rdx, (proc.func_id().get());
                    movq rax, (outer_ptr);
                }
                // rax: outer, rdx: FuncId
                self.get_func_data();
                // rax: outer, r15: &FuncData
                monoasm! { &mut self.jit,
                    // set outer
                    movq [r14 - (LFP_OUTER)], rax;
                    // use given self
                    // set meta; tag with the proc-method bit so that
                    // `handle_error` absorbs `MethodReturn` at this
                    // frame (define_method's `return` acts like a
                    // lambda-style method return).
                    movq rax, [r15 + (FUNCDATA_META)];
                    movq rdi, (Meta::PROC_METHOD_MASK);
                    orq  rax, rdi;
                    movq [r14 - (LFP_META)], rax;
                    // set pc
                    movq r13, [r15 + (FUNCDATA_PC)];
                    jmp [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
                }
            }
            FuncKind::Builtin { abs_address } => self.gen_native_func_wrapper(*abs_address),
            FuncKind::AttrReader { ivar_name } => self.gen_attr_reader(*ivar_name),
            FuncKind::AttrWriter { ivar_name } => self.gen_attr_writer(*ivar_name),
            FuncKind::StructReader { slot_index } => self.gen_struct_reader(*slot_index),
            FuncKind::StructWriter { slot_index } => self.gen_struct_writer(*slot_index),
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
            movzxw rax, [r14 - (LFP_REGNUM)];
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

    ///
    /// Generate a `Struct` member reader. The slot index is hard-coded
    /// at codegen time, so the wrapper is a fixed-cost three-load
    /// sequence with no Rust call:
    ///
    /// ```asm
    ///   mov rdi, [r14 - LFP_SELF]              ; self (Value, heap ptr)
    ///   mov rdi, [rdi + RVALUE_OFFSET_KIND]    ; Box<StructInner>
    ///   mov rdi, [rdi + STRUCT_INNER_PTR_OFFSET]; slot array
    ///   mov rax, [rdi + slot * 8]              ; the Value
    ///   ret
    /// ```
    ///
    /// Receiver type is guaranteed by the dispatch path (the method
    /// is only registered on `ObjTy::STRUCT` subclasses), so no class
    /// guard is needed at this level.
    fn gen_struct_reader(&mut self, slot_index: u16) {
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LFP_SELF)];
            movq rdi, [rdi + (RVALUE_OFFSET_KIND as i32)];
            movq rdi, [rdi + (STRUCT_INNER_PTR_OFFSET as i32)];
            movq rax, [rdi + ((slot_index as i32) * 8)];
            ret;
        );
    }

    ///
    /// Generate a `Struct` member writer. Routes through a small
    /// runtime helper (`set_struct_slot_with_check`) to keep the
    /// frozen check + error-flag plumbing in Rust:
    ///
    /// ```asm
    ///   mov rdi, rbx                  ; &mut Executor
    ///   mov rsi, r12                  ; &mut Globals
    ///   mov rdx, [r14 - LFP_SELF]     ; self (Value)
    ///   mov rcx, [r14 - LFP_ARG0]     ; the value to store
    ///   mov r8, slot_index
    ///   call set_struct_slot_with_check
    ///   ret
    /// ```
    ///
    /// On FrozenError the helper returns NULL and the caller picks up
    /// the error via the existing extern-"C" None-means-error contract
    /// (same as `set_instance_var_with_cache`).
    fn gen_struct_writer(&mut self, slot_index: u16) {
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LFP_SELF)];
            movq rcx, [r14 - (LFP_ARG0)];
            movq r8, (slot_index as u32);
            movq rax, (crate::builtins::struct_class::set_struct_slot_with_check);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
    }
}
