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
    /// aarch64 installs JIT code via **indirect dispatch** (no runtime branch
    /// patching): `slot` points at the wrapper's per-method JIT-entry word
    /// (init 0). `compile_method` runs the arch-neutral front-end and the A64
    /// lowering; on success we publish the compiled entry's address (fronted by
    /// a self-class guard, see below) into `slot`, and the wrapper loads it and
    /// branches to the JIT code on the next call. If the lowering bails on an
    /// unported AsmInst the slot stays 0 and the method stays VM-interpreted.
    /// See `doc/aarch64-jitgen-plan.md`.
    #[cfg(target_arch = "aarch64")]
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
        let class_version = self.jit_class_version();
        let jit_entry = self.jit.label();
        // `jit_entry` is bound by `gen_machine_code` (even on the bail path)
        // so it always resolves at `finalize`.
        let compiled = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            None,
        );
        let Some((cache, singleton_deps, class_version_label, const_map)) = compiled else {
            self.jit.finalize();
            return None;
        };
        // Register the unit's salvage record (inline caches, version word,
        // const folds) so class/const guard failures can re-validate instead
        // of recompiling. On aarch64 dispatch goes through `jit_slot`, so the
        // `jit_entry` table exists purely as this record — a republish for the
        // same class simply replaces it.
        globals.store.note_jit_iseq(iseq_id);
        globals.store[iseq_id].add_jit_code(
            self_class,
            jit_entry.clone(),
            class_version_label.clone(),
        );
        globals.store.set_salvage_record(
            iseq_id,
            self_class,
            class_version_label,
            cache,
            singleton_deps,
            const_map,
        );
        // Front the compiled `jit_entry` with a self-class guard. The JIT body
        // assumes `self == self_class`, but a single per-method slot is shared
        // by every receiver class of an inherited method — so publishing the
        // bare entry would mis-run a sibling subclass. The guard dispatches the
        // matching class to `jit_entry` and chains other classes to their own
        // specialization (mirrors the x86 `class_guard_stub`).
        let (guard, tail) = self.a64_gen_class_guard_stub(self_class, &jit_entry);
        self.jit.finalize();
        // Publish the guard stub (not the bare entry) into the slot.
        let guard_addr = self.jit.get_label_address(&guard).as_ptr() as u64;
        let tail_addr = self.jit.get_label_address(&tail).as_ptr() as u64;
        // SAFETY: `slot` is the address of the wrapper's (or a parent guard's)
        // heap-leaked u64 chain word (see arch/aarch64/wrapper.rs).
        unsafe { *(slot.as_ptr() as *mut u64) = guard_addr };
        // Record the slot so the recompiler can overwrite it in place later
        // (this is the slot the wrapper / guard chain uses to reach
        // `self_class`'s entry; `jit_profile_patch` passes the right one for
        // both the head class and chained classes).
        globals.store[iseq_id].set_jit_slot(self_class, slot.as_ptr() as u64);
        // Publish the *guard-free* dispatch slot: a heap word holding the bare
        // `jit_entry` address (past the wrapper counter logic and the
        // class-guard chain). JIT call sites that statically know — and have
        // already guarded — the receiver class dispatch through it, skipping
        // the redundant re-guard (see `ISeqInfo::jit_guard_free_slot`).
        // `recompile_method` re-points it; `invalidate_jit_code` zeroes it.
        //
        // ONE CELL PER (iseq, class), FOREVER: earlier-compiled callers bake
        // the slot's *address* into their code, so a second `compile_patch`
        // for the same pair (reachable when a whole-method recompile rebuilds
        // the class-guard chain with a fresh empty `next_slot`, dropping the
        // chained classes, which then re-warm through `jit_profile_patch`)
        // must update the EXISTING word in place — allocating a fresh Box
        // would orphan the baked one, making it invisible to both
        // `invalidate_jit_code`'s zeroing and future recompiles' re-pointing
        // (x86's twin invariant: it repatches its `patch_point` in place and
        // panics on a double `add_jit_code`).
        let entry_addr = self.jit.get_label_address(&jit_entry).as_ptr() as u64;
        match globals.store[iseq_id].get_jit_guard_free_slot(self_class) {
            Some(guard_free) => {
                // SAFETY: heap-leaked `u64` published by an earlier
                // `compile_patch`; valid for the process lifetime,
                // written only under the GVL.
                unsafe { *(guard_free as *mut u64) = entry_addr };
            }
            None => {
                let guard_free = Box::into_raw(Box::new(entry_addr)) as u64;
                globals.store[iseq_id].set_jit_guard_free_slot(self_class, guard_free);
            }
        }
        self.class_dispatch_note(
            globals,
            iseq_id,
            self_class,
            slot.as_ptr() as u64,
            entry_addr,
            tail_addr,
        );
        Some(())
    }

    ///
    /// A guard for *self_class* has just joined the method's class-guard
    /// chain, published into *slot*: keep the class → entry table in step,
    /// and once the chain is [`CLASS_DISPATCH_THRESHOLD`] long, build the
    /// table and publish its stub in the wrapper's dispatch slot instead of
    /// the first guard (see `class_dispatch.rs`; the x86-64 twin patches
    /// the wrapper's jump).
    ///
    /// - *entry*: the guard-free entry of the class. The recompiler
    ///   replaces the body, so it re-enters the class here
    ///   (`recompile_method_by_id`).
    /// - *tail*: the miss half of the new guard, the new end of the chain.
    ///
    #[cfg(target_arch = "aarch64")]
    fn class_dispatch_note(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        slot: u64,
        entry: u64,
        tail: u64,
    ) {
        let info = &mut globals.store[iseq_id];
        if info.jit_chain_head() == 0 {
            // The first class of a chain is published in the wrapper's own
            // dispatch slot.
            info.set_jit_chain_head(slot);
        }
        if let Some(dispatch) = info.class_dispatch_mut() {
            if ClassDispatch::holds(self_class) {
                dispatch.insert(self_class, entry);
            }
            dispatch.set_miss(tail);
            return;
        }
        if info.jit_entry_len() < CLASS_DISPATCH_THRESHOLD {
            return;
        }
        let head = info.jit_chain_head();
        // SAFETY: `head` is the wrapper's heap-leaked dispatch word, which
        // holds the address of the chain's first guard.
        let chain = unsafe { *(head as *const u64) };
        let mut dispatch = ClassDispatch::new(chain, tail);
        for (class, guard_free) in info.jit_guard_free_slots() {
            // SAFETY: a heap-leaked word holding the class's current entry
            // (`compile_patch`, `recompile_method_by_id`), or zero while a
            // caller waits for a class not compiled yet
            // (`ISeqInfo::add_late_direct_call`).
            let entry = unsafe { *(guard_free as *const u64) };
            if entry != 0 && ClassDispatch::holds(class) {
                dispatch.insert(class, entry);
            }
        }
        let header = dispatch.header_addr();
        info.set_class_dispatch(dispatch);
        let stub = self.a64_class_dispatch_stub(header);
        self.jit.finalize();
        let stub_addr = self.jit.get_label_address(&stub).as_ptr() as u64;
        // SAFETY: as above; written under the GVL like every dispatch word.
        unsafe { *(head as *mut u64) = stub_addr };
    }

    #[cfg(target_arch = "x86_64")]
    pub(super) fn compile_patch(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        entry: monoasm::CodePtr,
    ) -> Option<()> {
        let jit_entry = self.jit.label();
        let class_version = self.jit_class_version();
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
        if let Some((cache, singleton_deps, class_version_label, const_map)) = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            None,
        ) {
            let patch_point = self.jit.label();
            let guard = self.jit.label();
            let tail = self.class_guard_stub(self_class, &patch_point, &jit_entry, &guard);
            globals.store.note_jit_iseq(iseq_id);
            let old_entry = globals.store[iseq_id].add_jit_code(
                self_class,
                patch_point.clone(),
                class_version_label.clone(),
            );
            if let Some(_) = old_entry {
                globals.store[iseq_id].dump_jit_enntry();
                panic!(
                    "JIT code already exists for {}#{} {entry:?}",
                    self_class.get_name(globals),
                    globals.store[iseq_id].name()
                );
            }
            globals.store.set_salvage_record(
                iseq_id,
                self_class,
                class_version_label,
                cache,
                singleton_deps,
                const_map,
            );
            self.jit.apply_jmp_patch_address(entry, &guard);
            // Callers compiled before this body went through the wrapper;
            // give them the direct entry later callers get
            // (`ISeqInfo::add_late_direct_call`).
            for call in globals.store[iseq_id].take_late_direct_calls(self_class) {
                self.jit
                    .apply_jmp_patch_address(CodePtr::from(call as *mut u8), &patch_point);
            }
            self.class_dispatch_note(globals, iseq_id, self_class, &guard, &patch_point, &tail);
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
    /// x86-64: send the `patch_point`s of *iseq_id*'s dropped bodies
    /// (`ISeqInfo::retired_patch_points`) through the function's own entry.
    ///
    /// A caller that proved the receiver class calls a `patch_point`
    /// directly — compiled after the body, or repointed to it once the body
    /// existed (`ISeqInfo::add_late_direct_call`) — and none of its own
    /// guards is about the callee: a basic-op redefinition evicts only the
    /// units that inlined the operator, and a permanently invalidated callee
    /// is withdrawn from the index that would poison its callers. Left
    /// alone, such a call enters the dropped body for good. Through the
    /// entry it meets the warm-up stub (or `vm_entry`), as a call through
    /// the wrapper does; a body compiled for the class later gets a
    /// `patch_point` of its own.
    ///
    /// The wrapper reads the callee's first pc from `r13`, which a call
    /// through it sets (`emit_call`) and a direct call does not, so the
    /// `patch_point`s go to a stub that sets it first.
    ///
    #[cfg(target_arch = "x86_64")]
    pub(crate) fn retire_patch_points(&mut self, store: &mut Store, iseq_id: ISeqId) {
        let patch_points = store[iseq_id].take_retired_patch_points();
        if patch_points.is_empty() {
            return;
        }
        let func_id = store[iseq_id].func_id();
        let entry = store[func_id].entry_label();
        let (_, _, pc) = store[func_id].get_data();
        let pc = pc.expect("an iseq function has a pc").as_ptr();
        let retired = self.jit.label();
        monoasm! { &mut self.jit,
        retired:
            movq r13, (pc);
            jmp entry;
        }
        self.jit.finalize();
        for patch_point in patch_points {
            let addr = self.jit.get_label_address(&patch_point);
            self.jit.apply_jmp_patch_address(addr, &retired);
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
    /// Returns the label of the stub the guard's miss goes to: the end of
    /// the chain until the next class is compiled, when its jump is
    /// redirected to that class's guard.
    #[cfg(target_arch = "x86_64")]
    fn class_guard_stub(
        &mut self,
        self_class: ClassId,
        patch_point: &DestLabel,
        jit_entry: &DestLabel,
        guard: &DestLabel,
    ) -> DestLabel {
        let exit = self.jit_class_guard_fail.clone();
        let failed = self.jit.label();

        monoasm! { &mut self.jit,
        guard:
            movq rdi, [r14 - (LFP_SELF)];
        }
        self.guard_class2(GP::Rdi, CachedClass::from_class(self_class), &failed);
        monoasm! { &mut self.jit,
        patch_point:
            jmp jit_entry;
        }

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let _ = self.gen_compile_patch(&exit, &failed, COUNT_RECOMPILE_ARECV_CLASS);
        self.jit.select_page(0);
        failed
    }

    ///
    /// A guard for *self_class* has just joined the method's class-guard
    /// chain: keep the class → entry table in step, and once the chain is
    /// [`CLASS_DISPATCH_THRESHOLD`] long, build the table and send the
    /// method's entry through it (see `class_dispatch.rs`).
    ///
    /// - *guard*: the new guard.
    /// - *patch_point*: the guard-free entry of the class, a jump the
    ///   recompiler re-points, so its address stays good for the table.
    /// - *tail*: the new end of the chain.
    ///
    #[cfg(target_arch = "x86_64")]
    fn class_dispatch_note(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        guard: &DestLabel,
        patch_point: &DestLabel,
        tail: &DestLabel,
    ) {
        let addr =
            |jit: &JitMemory, label: &DestLabel| jit.get_label_address(label).as_ptr() as u64;
        let tail = addr(&self.jit, tail);
        let info = &mut globals.store[iseq_id];
        if info.jit_chain_head() == 0 {
            info.set_jit_chain_head(addr(&self.jit, guard));
        }
        if let Some(dispatch) = info.class_dispatch_mut() {
            if ClassDispatch::holds(self_class) {
                dispatch.insert(self_class, addr(&self.jit, patch_point));
            }
            dispatch.set_miss(tail);
            return;
        }
        if info.jit_entry_len() < CLASS_DISPATCH_THRESHOLD {
            return;
        }
        let mut dispatch = ClassDispatch::new(info.jit_chain_head(), tail);
        for (class, entry) in info.jit_entries() {
            if ClassDispatch::holds(class) {
                dispatch.insert(class, addr(&self.jit, entry));
            }
        }
        let stub = self.class_dispatch_stub(dispatch.header_addr());
        let func_id = info.func_id();
        info.set_class_dispatch(dispatch);
        let head = self
            .jit
            .get_label_address(&globals.store[func_id].entry_label());
        self.jit.apply_jmp_patch_address(head, &stub);
    }

    ///
    /// The stub a method's entry jumps to once its class-guard chain is
    /// long: one probe of the class → entry table instead of a walk.
    ///
    /// ~~~text
    /// stub:
    ///     rdi <- self
    ///     immediate?                      -> the chain
    ///     probe the table for self's class
    ///       found                         -> the class's entry
    ///       not found, Integer or Float   -> the chain
    ///       not found                     -> the end of the chain
    /// ~~~
    ///
    /// Every caller-saved register is free here, as it is for the guards
    /// (the stub at the end of the chain calls into Rust before entering
    /// the method either way).
    ///
    #[cfg(target_arch = "x86_64")]
    fn class_dispatch_stub(&mut self, header: u64) -> DestLabel {
        let stub = self.jit.label();
        let probe = self.jit.label();
        let hit = self.jit.label();
        let miss = self.jit.label();
        let chain = self.jit.label();
        monoasm! { &mut self.jit,
        stub:
            movq rdi, [r14 - (LFP_SELF)];
            testq rdi, 0b111;
            jnz  chain;
            movl rax, [rdi + (RVALUE_OFFSET_CLASS)];
            movq r10, (header);
            movq rsi, [r10 + (CLASS_DISPATCH_TABLE)];
            movq rcx, [r10 + (CLASS_DISPATCH_MASK)];
            movq rdx, rax;
        probe:
            andq rdx, rcx;
            movq r8, rdx;
            shlq r8, (CLASS_DISPATCH_SLOT_SHIFT);
            addq r8, rsi;
            movl r9, [r8];
            cmpq r9, rax;
            jeq  hit;
            testq r9, r9;
            jz   miss;
            addq rdx, 1;
            jmp  probe;
        hit:
            jmp  [r8 + (CLASS_DISPATCH_SLOT_TARGET)];
        miss:
            // A heap Integer or Float is not in the table: its class's
            // guard does more than compare the class word.
            subq rax, (INTEGER_CLASS.u32());
            cmpq rax, (FLOAT_CLASS.u32() - INTEGER_CLASS.u32());
            jbe  chain;
            jmp  [r10 + (CLASS_DISPATCH_MISS)];
        chain:
            movq r10, (header);
            jmp  [r10 + (CLASS_DISPATCH_CHAIN)];
        }
        stub
    }
}
