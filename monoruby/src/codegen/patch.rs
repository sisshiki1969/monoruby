use super::*;

impl Codegen {
    ///
    /// Execute JIT compile for the self class, and generate the class guard chain.
    ///
    /// ```text
    ///
    ///       wrapper                               class guard stub                                compile & patch stub
    ///    +-------------------------------+     +-------------------------------------+        +--------------------------------+
    ///    |                               |     |                                     |        |                                |
    /// ---+-> entry:                      |  /--+-> guard:                            |   /----+-> failed:                      |
    ///    |     jmp [[[next]]]; ----------+-/   |     movq rdi, [r14 - (LFP_SELF)];   |  /     |     jmp [[[next]]];            |
    ///    |   next:                       |     |     <class_guard(self_class)>-------+-/      |   next:                        |
    ///    |     subl [rip + counter], 1;  |     |   patch_point:                      |        |     subl [rip + counter], 1;   |
    ///    |     jne vm_entry;             |     |     jmp jit_entry --+               |        |     jne vm_entry;              |
    ///    |     <exec_compile_and_patch>  |     |                      \              |        |     <exec_compile_and_patch>   |
    ///    |     jmp entry;                |     +-----------------------\-------------+        |     jmp faild;                 |
    ///    |                               |                              \                     |                                |
    ///    +-------------------------------+                               +--> JIT code        +--------------------------------+
    ///
    /// ```
    pub(super) fn compile_patch(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        entry: monoasm::CodePtr,
    ) {
        let patch_point = self.jit.label();
        let jit_entry = self.jit.label();
        let class_version = self.class_version();
        let class_version_label = self.jit.const_i32(class_version as _);
        let guard = self.jit.label();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        let self_class = lfp.self_val().class();
        self.class_guard_stub(self_class, &patch_point, &jit_entry, &guard);
        let old_entry = globals.store[iseq_id].add_jit_code(
            self_class,
            patch_point,
            class_version_label.clone(),
        );
        assert!(old_entry.is_none());
        let cache = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry,
            class_version,
            class_version_label,
            None,
        );

        globals.store[iseq_id].set_cache_map(self_class, cache);
        self.jit.apply_jmp_patch_address(entry, &guard);
    }

    ///
    /// Generate class guard stub for JIT code.
    ///
    /// ~~~text
    ///
    /// guard:
    ///     movq rdi, [r14 - (LFP_SELF)];
    ///     guard_class_rdi(self_class, vm_entry);
    /// patch_point:
    ///     jmp jit_entry;
    ///
    /// ~~~
    ///
    fn class_guard_stub(
        &mut self,
        self_class: ClassId,
        patch_point: &DestLabel,
        jit_entry: &DestLabel,
        guard: &DestLabel,
    ) {
        let exit = self.jit_class_guard_fail.clone();
        let failed = self.jit.label();

        monoasm! { &mut self.jit,
        guard:
            movq rdi, [r14 - (LFP_SELF)];
        }
        self.guard_class_rdi(self_class, &failed);
        monoasm! { &mut self.jit,
        patch_point:
            jmp jit_entry;
        }

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.gen_compile_patch(&exit, &failed, COUNT_RECOMPILE_ARECV_CLASS);
        self.jit.select_page(0);
    }

    pub(super) fn gen_compile_patch(
        &mut self,
        no_compile_exit: &DestLabel,
        entry: &DestLabel,
        counter: i32,
    ) {
        let counter = self.jit.data_i32(counter);
        let cont = self.jit.label();
        let patch_point_addr = self.jit.get_current_address();
        monoasm! { &mut self.jit,
        entry:
            jmp cont;
        cont:
            subl [rip + counter], 1;
            jne no_compile_exit;

            movq rdi, r12;
            movq rsi, r14;
            movq rdx, (patch_point_addr.as_ptr());
            subq rsp, 4088;
            movq rax, (compiler::exec_jit_compile_patch as usize);
            call rax;
            addq rsp, 4088;
            jmp entry;
        }
    }
}
