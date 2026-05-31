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
    ///
    /// aarch64 has no emission yet: `compile_method` runs the arch-neutral
    /// front-end and bails (returns `None`), so a minimal `not(jit_emit)`
    /// variant just drives that and never patches — the A64 trigger falls
    /// through to `vm_entry` on its own. See `doc/aarch64-jitgen-plan.md`.
    #[cfg(not(jit_emit))]
    pub(super) fn compile_patch(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        _entry: monoasm::CodePtr,
    ) -> Option<()> {
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        if globals.store[iseq_id].jit_invalidated() {
            self.jit.finalize();
            return None;
        }
        let self_class = lfp.self_val().class();
        let class_version = self.class_version();
        let jit_entry = self.jit.label();
        // Drives the front-end + aarch64 lowering. `jit_entry` is bound by
        // `a64_gen_machine_code` (even on the bail path), so it always resolves
        // at `finalize`. The result is currently ignored: aarch64 has no
        // install path yet (no runtime patching), so the method stays
        // VM-interpreted whether or not it compiled. See Phase 3b in
        // doc/aarch64-jitgen-plan.md.
        let _ =
            self.compile_method(globals, iseq_id, self_class, jit_entry, class_version, None);
        self.jit.finalize();
        None
    }

    #[cfg(jit_emit)]
    pub(super) fn compile_patch(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        entry: monoasm::CodePtr,
    ) -> Option<()> {
        let jit_entry = self.jit.label();
        let class_version = self.class_version();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        let self_class = lfp.self_val().class();
        // Skip compilation if JIT entries were invalidated (e.g. by BOP redefinition).
        if globals.store[iseq_id].jit_invalidated() {
            let vm_entry = self.vm_entry();
            self.jit.apply_jmp_patch_address(entry, &vm_entry);
            self.jit.finalize();
            return None;
        }
        if let Some((cache, class_version_label)) = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            None,
        ) {
            let patch_point = self.jit.label();
            let guard = self.jit.label();
            self.class_guard_stub(self_class, &patch_point, &jit_entry, &guard);
            let old_entry =
                globals.store[iseq_id].add_jit_code(self_class, patch_point, class_version_label);
            if let Some(_) = old_entry {
                globals.store[iseq_id].dump_jit_enntry();
                panic!(
                    "JIT code already exists for {}#{} {entry:?}",
                    self_class.get_name(globals),
                    globals.store[iseq_id].name()
                );
            }
            globals.store[iseq_id].set_cache_map(self_class, cache);
            self.jit.apply_jmp_patch_address(entry, &guard);
            self.jit.finalize();
            Some(())
        } else {
            let vm_entry = self.vm_entry();
            self.jit.apply_jmp_patch_address(entry, &vm_entry);
            self.jit.finalize();
            None
        }
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
    #[cfg(jit_emit)]
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
        self.guard_class2(GP::Rdi, self_class, &failed);
        monoasm! { &mut self.jit,
        patch_point:
            jmp jit_entry;
        }

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.gen_compile_patch(&exit, &failed, COUNT_RECOMPILE_ARECV_CLASS);
        self.jit.select_page(0);
    }
}
