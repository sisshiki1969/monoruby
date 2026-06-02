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
    /// front-end and bails (returns `None`), so a minimal `not(jit_x86)`
    /// variant just drives that and never patches — the A64 trigger falls
    /// through to `vm_entry` on its own. See `doc/aarch64-jitgen-plan.md`.
    /// aarch64 install via **indirect dispatch** (no runtime branch patching):
    /// `slot` points at the wrapper's per-method JIT-entry word (init 0). On a
    /// successful compile we write the compiled entry address there; the
    /// wrapper loads it and branches to the JIT code on the next call. On bail
    /// the slot stays 0 and the method stays VM-interpreted.
    #[cfg(not(jit_x86))]
    pub(super) fn compile_patch(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        slot: monoasm::CodePtr,
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
        // `jit_entry` is bound by `a64_gen_machine_code` (even on the bail
        // path) so it always resolves at `finalize`.
        let compiled = self
            .compile_method(globals, iseq_id, self_class, jit_entry.clone(), class_version, None)
            .is_some();
        if !compiled {
            self.jit.finalize();
            return None;
        }
        // Front the compiled `jit_entry` with a self-class guard. The JIT body
        // assumes `self == self_class`, but a single per-method slot is shared
        // by every receiver class of an inherited method — so publishing the
        // bare entry would mis-run a sibling subclass. The guard dispatches the
        // matching class to `jit_entry` and chains other classes to their own
        // specialization (mirrors the x86 `class_guard_stub`).
        let guard = self.a64_gen_class_guard_stub(self_class, &jit_entry);
        self.jit.finalize();
        // Publish the guard stub (not the bare entry) into the slot.
        let guard_addr = self.jit.get_label_address(&guard).as_ptr() as u64;
        // SAFETY: `slot` is the address of the wrapper's (or a parent guard's)
        // heap-leaked u64 chain word (see arch/aarch64/wrapper.rs).
        unsafe { *(slot.as_ptr() as *mut u64) = guard_addr };
        Some(())
    }

    #[cfg(jit_x86)]
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
    #[cfg(jit_x86)]
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
