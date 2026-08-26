use super::*;

impl Codegen {
    ///
    /// Compile the Ruby method.
    ///
    pub(super) fn compile_method(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        jit_entry: DestLabel,
        class_version: u32,
        is_recompile: Option<RecompileReason>,
    ) -> Option<(Vec<InlineCacheEntry>, DestLabel, ConstSalvageMap)> {
        self.compile(
            globals,
            iseq_id,
            self_class,
            None,
            jit_entry,
            class_version,
            is_recompile,
        )
    }

    #[cfg(target_arch = "x86_64")]
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

            // hot enough overall: profile this self-class and compile only if
            // *it* (not just any class) is hot. jit_profile_patch re-arms the
            // counter (rcx) when the class isn't hot yet, so the stub keeps
            // interpreting and samples again later.
            movq rdi, r12;
            movq rsi, r14;
            movq rdx, (patch_point_addr.as_ptr());
            lea  rcx, [rip + counter];
            subq rsp, 4088;
            movq rax, (jit_profile_patch as *const ());
            call rax;
            addq rsp, 4088;
            jmp entry;
        }
    }

    ///
    /// Generate code for recompilation of the loop / method.
    ///
    /// ### in
    /// - r12: &mut Globals
    /// - r14: Lfp
    ///
    /// ### destroy
    /// - rax
    ///
    #[cfg(target_arch = "x86_64")]
    pub(super) fn gen_recompile(
        &mut self,
        position: Option<BytecodePtr>,
        label: DestLabel,
        reason: RecompileReason,
        with_recovery: Option<DestLabel>,
    ) {
        self.jit.bind_label(label);
        monoasm!( &mut self.jit,
            movq rdi, r12;
            movq rsi, r14;
        );
        if let Some(pc) = position {
            self.jit.save_registers();
            monoasm!( &mut self.jit,
                movq rdx, (pc.as_ptr());
                movl rcx, (reason as u32);
                movq rax, (jit_recompile_loop);
                call rax;
            );
            self.jit.restore_registers();
        } else if let Some(recover) = with_recovery {
            self.save_registers();
            monoasm!( &mut self.jit,
                movl rdx, (reason as u32);
                movq rax, (jit_recompile_method_with_recovery);
                call rax;
            );
            self.restore_registers();
            monoasm!( &mut self.jit,
                testq rax, rax;
                jnz recover;
            );
        } else {
            self.save_registers();
            monoasm!( &mut self.jit,
                movl rdx, (reason as u32);
                movq rax, (jit_recompile_method);
                call rax;
            );
            self.restore_registers();
        }
    }

    ///
    /// Salvage-only miss handler for a constant-version guard: re-validate the
    /// unit's folded constants and, if they all still hold, re-stamp its
    /// version word. Never recompiles.
    ///
    /// This is what a block-style root gets. Its whole-method recompile entry
    /// would rebuild the wrong frame shape, so it used to take a bare deopt
    /// with no healing at all — and since the global const version never moves
    /// back, such a unit then failed its guard on *every* later call, for the
    /// rest of the process. Salvage reads the unit's recorded folds, compares
    /// them against the live constants and writes one word; it builds no
    /// frame, so the reason block roots cannot recompile does not apply.
    ///
    /// ### in
    /// - r12: &mut Globals
    /// - r14: Lfp
    ///
    /// ### destroy
    /// - rax
    ///
    #[cfg(target_arch = "x86_64")]
    pub(super) fn gen_salvage_const(&mut self, label: DestLabel) {
        self.jit.bind_label(label);
        monoasm!( &mut self.jit,
            movq rdi, r12;
            movq rsi, r14;
        );
        self.save_registers();
        monoasm!( &mut self.jit,
            movq rax, (jit_salvage_const);
            call rax;
        );
        self.restore_registers();
    }

    #[cfg(target_arch = "x86_64")]
    pub(super) fn gen_recompile_specialized(
        &mut self,
        idx: usize,
        label: DestLabel,
        reason: RecompileReason,
    ) {
        self.jit.bind_label(label);
        self.jit.save_registers();
        monoasm!( &mut self.jit,
            movq rdi, r12;
            movq rsi, (idx);
            movl rdx, (reason as u32);
            movq rax, (jit_recompile_specialized);
            call rax;
        );
        self.jit.restore_registers();
    }

    #[cfg(target_arch = "x86_64")]
    pub(super) fn gen_compile_loop(&mut self, entry: &DestLabel, cont: &DestLabel) {
        monoasm!( &mut self.jit,
        entry:
            movq rdi, r12;
            movq rsi, r14;
            lea  rdx, [r13 - 16];
            movq rax, (jit_compile_loop);
            call rax;
            movq rax, [r13 - 8];
            testq rax, rax;
            jeq cont;
            jmp rax;
        );
    }

    ///
    /// Entry point of JIT compilation.
    ///
    fn compile(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
        class_version: u32,
        _is_recompile: Option<RecompileReason>,
    ) -> Option<(Vec<InlineCacheEntry>, DestLabel, ConstSalvageMap)> {
        if position.is_none() && globals.store[iseq_id].jit_invalidated() {
            return None;
        }
        // A `while`/`until` `redo` (the `Redo` op) bails through the error
        // path, whose `goto` resumes the interpreter in the current native
        // frame without flushing JIT register state or tearing the frame
        // down. Inside a JIT'd enclosing loop that corrupts the enclosing
        // loop's state (method JIT) or re-enters the loop-JIT and overflows
        // the stack (loop JIT). Decline to JIT any iseq containing one —
        // both the method and loop JIT opt out; the interpreter runs `redo`
        // correctly.
        if globals.store[iseq_id].contains_redo() {
            return None;
        }
        #[cfg(feature = "profile")]
        {
            if let Some(reason) = &_is_recompile {
                let func_id = globals.store[iseq_id].func_id();
                globals.countup_recompile(func_id, self_class, reason);
            }
        }

        #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
        if self.startup_flag {
            let iseq = &globals.store[iseq_id];
            let func_id = iseq.func_id();
            let start_pos = iseq.get_pc_index(position);
            let name = globals.store.func_description(func_id);
            eprintln!(
                "==> start {} {}compile: {}{:?} <{}> {}self_class: {} {}:{}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                if _is_recompile.is_some() { "re" } else { "" },
                if let Some(reason) = _is_recompile {
                    format!("({:?}) ", reason)
                } else {
                    String::new()
                },
                func_id,
                name,
                if position.is_some() {
                    format!("start:[{}] ", start_pos)
                } else {
                    String::new()
                },
                globals.store.debug_class_name(self_class),
                iseq.sourceinfo.file_name(),
                iseq.sourceinfo.get_line(&iseq.loc),
            );
        }

        let now = std::time::Instant::now();
        let (start0, start1) = self.get_address_pair();

        let const_version = self.const_version();
        match self.jit_compile(
            &globals.store,
            iseq_id,
            self_class,
            position,
            entry_label.clone(),
            class_version,
            const_version,
        ) {
            Ok((cache, specialized_info, class_version_label, bop_deps, const_map)) => {
                globals.store[iseq_id].add_bop_deps(bop_deps);
                let codeptr = self.jit.get_label_address(&entry_label);
                let (end0, end1) = self.get_address_pair();
                let elapsed = now.elapsed();
                let span = (
                    (start0, (end0 - start0) as usize),
                    (start1, (end1 - start1) as usize),
                );
                #[cfg(feature = "jit-log")]
                {
                    if self.startup_flag {
                        eprintln!(
                            "{} {} {position:?} ({} bytes, {} bytes) [wm0:{:?}-{:?} wm1:{:?}-{:?}] {elapsed:?}",
                            globals
                                .store
                                .func_description(globals.store[iseq_id].func_id()),
                            globals.store.debug_class_name(self_class),
                            span.0.1,
                            span.1.1,
                            start0,
                            end0,
                            start1,
                            end1
                        );
                        eprintln!("{}", specialized_info.format(&globals.store));
                    }
                }
                self.add_compilation_unit(
                    iseq_id,
                    self_class,
                    position,
                    codeptr,
                    specialized_info,
                    span,
                    elapsed,
                );
                #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
                if self.startup_flag {
                    eprintln!("<== finished compile. elapsed:{:?}", elapsed);
                    self.jit_compile_time += elapsed;

                    self.jit.select_page(0);
                    eprint!("    total bytes(0):{:?}    ", self.jit.get_current());
                    self.jit.select_page(1);
                    eprintln!("(1):{:?}", self.jit.get_current());
                    self.jit.select_page(0);
                }

                Some((cache, class_version_label, const_map))
            }
            Err(_) => {
                if position.is_none() {
                    globals.store[iseq_id].invalidate_jit_code();
                    // The permanent give-up dropped every jit_entry, but on
                    // x86 the wrapper's entry jump may still point at the old
                    // class-guard chain (a whole-compile for one self class
                    // can bail after others compiled fine — `Class#new` bails
                    // this way, since its `(...)` forward is only compilable
                    // specialized). Left as-is, every call re-enters the
                    // stale body, fails its version guard, requests a
                    // recompile that `recompile_method_by_id` must skip, and
                    // deopts — per call, forever. Revert the entry to
                    // `vm_entry` so the method just interprets (mirrors the
                    // BOP-evict revert in `invalidate_bop_fast_paths`; the
                    // aarch64 twin already zeroes its dispatch slots in
                    // `invalidate_jit_code`). Trivial hints never point their
                    // entry at compiled bytecode — skip them, as the BOP
                    // revert does.
                    #[cfg(target_arch = "x86_64")]
                    if globals.store[iseq_id].hint == ISeqHint::Normal {
                        let func_id = globals.store[iseq_id].func_id();
                        let entry = self
                            .jit
                            .get_label_address(&globals.store[func_id].entry_label());
                        let vm_entry = self.vm_entry();
                        self.jit.apply_jmp_patch_address(entry, &vm_entry);
                    }
                }
                #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
                if self.startup_flag {
                    let elapsed = now.elapsed();
                    eprintln!("<== abort compile. elapsed:{:?}", elapsed);
                    self.jit_compile_time += elapsed;
                }
                None
            }
        }
    }

    fn recompile_method(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        reason: RecompileReason,
    ) -> Option<()> {
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        self.recompile_method_by_id(globals, iseq_id, self_class, reason)
    }

    #[cfg(target_arch = "x86_64")]
    fn recompile_method_by_id(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        reason: RecompileReason,
    ) -> Option<()> {
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(match reason {
            RecompileReason::ClassVersionGuardFailed => {
                &crate::codegen::jit_stats::RECOMPILE_METHOD_CLASS_VER
            }
            RecompileReason::ConstVersionGuardFailed => {
                &crate::codegen::jit_stats::RECOMPILE_METHOD_CONST_VER
            }
            _ => &crate::codegen::jit_stats::RECOMPILE_METHOD_OTHER,
        });
        // Bail *before* compiling when there is no patch point to install
        // into — after a BOP eviction, or when the method only ever ran as a
        // specialized child of some root (its own `jit_entry` map was never
        // populated). Compiling first and then discarding used to livelock:
        // the stale body kept requesting a whole recompile on every call,
        // each one paying a full compile + `finalize` over the ever-growing
        // relocation table, only to be thrown away (mirrors the aarch64
        // variant, which has always early-outed on an unknown slot).
        let patch_point = match globals.store[iseq_id].get_jit_entry(self_class) {
            Some(patch_point) => patch_point,
            None => {
                #[cfg(feature = "jit-log")]
                crate::codegen::jit_stats::bump(
                    &crate::codegen::jit_stats::RECOMPILE_METHOD_SKIPPED,
                );
                #[cfg(feature = "jit-log")]
                eprintln!(
                    "[JIT] recompilation skipped for invalidated method: {:?} (jit_invalidated={}, entry_classes={:?}, wanted={:?})",
                    globals.store[globals.store[iseq_id].func_id()].name(),
                    globals.store[iseq_id].jit_invalidated(),
                    globals.store[iseq_id].jit_entry_classes(),
                    self_class,
                );
                return None;
            }
        };
        let jit_entry = self.jit.label();
        let class_version = self.class_version();
        let (cache, class_version_label, const_map) = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            Some(reason),
        )?;
        // Refresh the salvage record — including the version word this fresh
        // body reads. Leaving the previous compile's label in place made
        // every later salvage stamp a word no live body reads, so the guard
        // failed again on the next call (270k steady-state misses per
        // activerecord run, and the ones whose exit had no recovery path
        // deopted every time).
        globals.store[iseq_id].set_salvage_record(
            self_class,
            class_version_label,
            cache,
            const_map,
        );
        let patch_point = self.jit.get_label_address(&patch_point);
        self.jit.apply_jmp_patch_address(patch_point, &jit_entry);
        Some(())
    }

    /// aarch64 whole-method recompile: re-run the front-end + A64 lowering and
    /// overwrite the method's dispatch slot in place (aarch64 installs JIT code
    /// via the indirect slot recorded at `compile_patch`, not x86 branch
    /// patching). Bails (leaving the old code) if the slot is unknown or the
    /// method was JIT-invalidated.
    #[cfg(target_arch = "aarch64")]
    fn recompile_method_by_id(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        reason: RecompileReason,
    ) -> Option<()> {
        if globals.store[iseq_id].jit_invalidated() {
            return None;
        }
        let slot = globals.store[iseq_id].get_jit_slot(self_class)?;
        let jit_entry = self.jit.label();
        let class_version = self.class_version();
        let compiled = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            Some(reason),
        );
        let Some((cache, class_version_label, const_map)) = compiled else {
            self.jit.finalize();
            return None;
        };
        // Refresh the salvage record so later guard failures re-validate
        // against what *this* body folded, and against the version word it
        // actually reads (mirrors the x86 variant).
        globals.store[iseq_id].set_salvage_record(
            self_class,
            class_version_label,
            cache,
            const_map,
        );
        let guard = self.a64_gen_class_guard_stub(self_class, &jit_entry);
        self.jit.finalize();
        let guard_addr = self.jit.get_label_address(&guard).as_ptr() as u64;
        // SAFETY: `slot` is the dispatch word recorded by `compile_patch`.
        unsafe { *(slot as *mut u64) = guard_addr };
        // Re-point the guard-free dispatch slot at the recompiled body so
        // every JIT call site that dispatches through it (skipping the
        // wrapper's re-guard) redirects too — the aarch64 analogue of x86
        // repatching the `patch_point` jump. The slot exists whenever
        // `compile_patch` compiled this (iseq, class) pair, which is a
        // precondition for reaching the recompiler.
        if let Some(guard_free) = globals.store[iseq_id].get_jit_guard_free_slot(self_class) {
            let entry_addr = self.jit.get_label_address(&jit_entry).as_ptr() as u64;
            // SAFETY: heap-leaked `u64` published by `compile_patch`; valid
            // for the process lifetime, single-threaded write (see
            // `invalidate_jit_code`).
            unsafe { *(guard_free as *mut u64) = entry_addr };
        }
        Some(())
    }

    fn compile_partial(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        pc: BytecodePtr,
        is_recompile: Option<RecompileReason>,
    ) -> Option<()> {
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        self.compile_partial_by_id(globals, iseq_id, self_class, pc, is_recompile)
    }

    fn compile_partial_by_id(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        pc: BytecodePtr,
        is_recompile: Option<RecompileReason>,
    ) -> Option<()> {
        let entry_label = self.jit.label();
        let class_version = self.class_version();
        #[cfg(feature = "jit-log")]
        if let Some(reason) = is_recompile {
            crate::codegen::jit_stats::bump(match reason {
                RecompileReason::ClassVersionGuardFailed => {
                    &crate::codegen::jit_stats::RECOMPILE_LOOP_CLASS_VER
                }
                RecompileReason::ConstVersionGuardFailed => {
                    &crate::codegen::jit_stats::RECOMPILE_LOOP_CONST_VER
                }
                _ => &crate::codegen::jit_stats::RECOMPILE_LOOP_OTHER,
            });
        }

        let ret = if let Some((cache, version_label, const_map)) = self.compile(
            globals,
            iseq_id,
            self_class,
            Some(pc),
            entry_label.clone(),
            class_version,
            is_recompile,
        ) {
            let codeptr = self.jit.get_label_address(&entry_label);
            pc.write2(codeptr.as_ptr() as u64);
            // Record the unit's salvage info so a later class-version guard
            // failure can re-validate and patch instead of recompiling.
            let index = globals.store[iseq_id].get_pc_index(Some(pc));
            globals.store[iseq_id].set_loop_jit_info(
                self_class,
                index,
                version_label,
                cache,
                const_map,
            );
            Some(())
        } else {
            None
        };
        // Re-arm executable permission (and flush the icache) on the JIT
        // region: `compile` toggled it writable to emit the loop body, and the
        // VM/JIT code we return into lives in that same region — without this
        // it would fault on the next instruction (W^X on Apple Silicon). A
        // no-op on x86. Mirrors `compile_patch`.
        self.jit.finalize();
        ret
    }

    ///
    /// A specialized body compiled under an armed unboxed-Float speculation
    /// must not be replaced standalone: its dynvar accesses address the FP
    /// save/spill slots of the root body that armed the speculation, and a
    /// context-free recompile would read the caller's speculated locals
    /// through never-written LFP slots (issue #1140). Rebuild the whole root
    /// compilation unit instead — the fresh root re-specializes (and
    /// re-arms) under the inline caches that triggered the recompile, and
    /// takes over via its own entry patch; the old root body stays
    /// internally consistent for frames already running it.
    ///
    fn recompile_speculated_root(
        &mut self,
        globals: &mut Globals,
        root: (ISeqId, ClassId, Option<BytecodePtr>),
        reason: RecompileReason,
    ) -> Option<()> {
        let (iseq_id, self_class, position) = root;
        #[cfg(feature = "jit-log")]
        eprintln!(
            "[JIT] speculated specialized entry: rebuilding root {:?} pos={:?} ({:?})",
            iseq_id, position, reason
        );
        match position {
            None => self.recompile_method_by_id(globals, iseq_id, self_class, reason),
            // Loop root: recompile from the loop head and re-point the
            // loop-entry word at it, exactly as a plain loop recompile does.
            Some(pc) => {
                self.compile_partial_by_id(globals, iseq_id, self_class, pc, Some(reason))
            }
        }
    }

    #[cfg(target_arch = "x86_64")]
    fn recompile_specialized(
        &mut self,
        globals: &mut Globals,
        idx: usize,
        reason: RecompileReason,
    ) -> Option<()> {
        let SpecializedPatchEntry {
            iseq_id,
            self_class,
            patch_point,
            speculated_root,
            ..
        } = self.specialized_info[idx].clone();
        #[cfg(feature = "jit-log")]
        eprintln!(
            "[JIT] recompile_specialized idx={idx} iseq={:?} ({:?}) speculated={}",
            globals.store[iseq_id].name(),
            reason,
            speculated_root.is_some(),
        );
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(match reason {
            RecompileReason::ClassVersionGuardFailed => {
                &crate::codegen::jit_stats::RECOMPILE_SPEC_CLASS_VER
            }
            RecompileReason::ConstVersionGuardFailed => {
                &crate::codegen::jit_stats::RECOMPILE_SPEC_CONST_VER
            }
            _ => &crate::codegen::jit_stats::RECOMPILE_SPEC_OTHER,
        });
        if let Some(root) = speculated_root {
            return self.recompile_speculated_root(globals, root, reason);
        }

        let entry = self.jit.label();
        let class_version = self.class_version();
        self.compile(
            globals,
            iseq_id,
            self_class,
            None,
            entry.clone(),
            class_version,
            Some(reason),
        )?;

        let patch_point = self.jit.get_label_address(&patch_point);
        self.jit.apply_jmp_patch_address(patch_point, &entry);
        // The fresh body reads its own version words; the owner's salvage
        // records no longer cover it (see `SpecializedPatchEntry::owner`).
        self.specialized_info[idx].owner = None;
        Some(())
    }

    /// aarch64 specialized recompile. Mirrors the x86 variant, but installs the
    /// recompiled body by rewriting the `SpecializedCall` site's `bl` to it
    /// (aarch64 has no `apply_jmp_patch_address`, but it can patch a single
    /// branch instruction — see [`Codegen::patch_call_to_entry`]). Bails
    /// (leaving the old specialized body in place) if the lowering bails.
    #[cfg(target_arch = "aarch64")]
    fn recompile_specialized(
        &mut self,
        globals: &mut Globals,
        idx: usize,
        reason: RecompileReason,
    ) -> Option<()> {
        let SpecializedPatchEntry {
            iseq_id,
            self_class,
            patch_point,
            speculated_root,
            ..
        } = self.specialized_info[idx].clone();
        if let Some(root) = speculated_root {
            return self.recompile_speculated_root(globals, root, reason);
        }
        let entry = self.jit.label();
        let class_version = self.class_version();
        let compiled = self
            .compile(
                globals,
                iseq_id,
                self_class,
                None,
                entry.clone(),
                class_version,
                Some(reason),
            )
            .is_some();
        // Re-arm executable permission / flush the I-cache for the freshly
        // emitted body (and resolve `entry`'s address) before patching.
        self.jit.finalize();
        if !compiled {
            return None;
        }
        let patch_point = self.jit.get_label_address(&patch_point);
        self.patch_call_to_entry(patch_point, &entry);
        // The fresh body reads its own version words; the owner's salvage
        // records no longer cover it (see `SpecializedPatchEntry::owner`).
        self.specialized_info[idx].owner = None;
        Some(())
    }
}

//
// JIT Compiler API for asm codes.
//

#[cfg(target_arch = "x86_64")]
extern "C" fn jit_recompile_specialized(
    globals: &mut Globals,
    idx: usize,
    reason: RecompileReason,
) {
    if salvage_specialized(globals, idx, reason) {
        return;
    }
    CODEGEN.with(|codegen| {
        codegen
            .borrow_mut()
            .recompile_specialized(globals, idx, reason);
    });
}

/// ④: before recompiling a specialized body whose class-version guard failed,
/// try to salvage the *owning* compilation unit (all of its guards read the
/// same version word). On success nothing is recompiled; the current
/// invocation deopts to the VM once and the healed code passes its guards
/// from the next call on.
///
/// This runs *outside* the CODEGEN borrow that `recompile_specialized` takes:
/// the salvage validation re-resolves methods through caches that read
/// `Globals::class_version()`, which borrows the same thread-local.
fn salvage_specialized(globals: &mut Globals, idx: usize, reason: RecompileReason) -> bool {
    // `owner: None` means this body was individually recompiled: it reads
    // its own version words, which no owner record names — fall through to
    // the recompile.
    let Some((owner_iseq, owner_class, owner_pos)) =
        CODEGEN.with(|codegen| codegen.borrow().specialized_info[idx].owner)
    else {
        return false;
    };
    match reason {
        RecompileReason::ClassVersionGuardFailed => {
            #[cfg(feature = "jit-log")]
            crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::RECOVERY_ATTEMPT_SPEC);
            let salvaged = match owner_pos {
                None => globals
                    .store
                    .salvage_method_unit(owner_iseq, owner_class, None),
                Some(pc) => {
                    let index = globals.store[owner_iseq].get_pc_index(Some(pc));
                    globals
                        .store
                        .salvage_loop_unit(owner_iseq, owner_class, index, None)
                }
            };
            if let Some(version_label) = salvaged {
                let version = Globals::class_version();
                CODEGEN.with(|codegen| {
                    let mut codegen = codegen.borrow_mut();
                    // The failing body reads its *own* unit's version word
                    // (`SpecializedPatchEntry::class_version_label`), which
                    // the owner record's label stops naming after the first
                    // whole-method recompile of the pair (the record label
                    // is written only by the initial `compile_patch`).
                    // Stamping only the record's label would then leave the
                    // live body's word stale and it would deopt on every
                    // call forever. Stamp both: the record's word (what
                    // future lookups re-validate against) and the word the
                    // failing guard actually compared.
                    let own = codegen.specialized_info[idx].class_version_label.clone();
                    codegen.set_class_version(version, &version_label);
                    codegen.set_class_version(version, &own);
                });
                #[cfg(feature = "jit-log")]
                crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SALVAGE_OK_SPEC);
                true
            } else {
                false
            }
        }
        RecompileReason::ConstVersionGuardFailed => {
            let loop_index = owner_pos.map(|pc| globals.store[owner_iseq].get_pc_index(Some(pc)));
            salvage_const(globals, owner_iseq, owner_class, loop_index)
        }
        _ => false,
    }
}

/// Two-tier const-version salvage of the unit `(iseq_id, self_class[,
/// loop_index])` — validation in `Store::salvage_const_unit`, word patch
/// here. Like the class-version twin this runs *outside* the CODEGEN borrow
/// (`Globals::const_version` reads the same thread-local). On success (or a
/// bounded stale deferral) the current invocation deopts to the VM once and
/// the salvage retries or the healed unit passes its guards from the next
/// execution on.
fn salvage_const(
    globals: &mut Globals,
    iseq_id: ISeqId,
    self_class: ClassId,
    loop_index: Option<crate::bytecodegen::BcIndex>,
) -> bool {
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::CONST_RECOVERY_ATTEMPT);
    match globals
        .store
        .salvage_const_unit(iseq_id, self_class, loop_index)
    {
        ConstSalvage::Healed(word) => {
            let version = Globals::const_version();
            CODEGEN.with(|codegen| {
                codegen.borrow_mut().set_const_version(version, &word);
            });
            #[cfg(feature = "jit-log")]
            crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::CONST_SALVAGE_OK);
            true
        }
        // Skip the recompile but leave the guard failing: this invocation
        // deopts to the VM, whose execution refreshes the suspect site's
        // cache, and the next miss retries the salvage.
        ConstSalvage::Defer => true,
        ConstSalvage::Recompile => false,
    }
}

/// Whole-method const salvage entry: on a `GuardConstVersion` failure, try
/// the two-tier validation for the frame's unit before recompiling. Unlike
/// the class-version guard (whose x86 recovery resumes in place), a const
/// salvage lets this invocation deopt to the VM once; the healed unit
/// passes its guard from the next call.
fn salvage_method_const(globals: &mut Globals, lfp: Lfp, reason: RecompileReason) -> bool {
    if !matches!(reason, RecompileReason::ConstVersionGuardFailed) {
        return false;
    }
    let self_class = lfp.self_val().class();
    let iseq_id = globals.store[lfp.func_id()].as_iseq();
    salvage_const(globals, iseq_id, self_class, None)
}

/// aarch64 specialized recompile entry, called from the
/// `GuardClassVersionSpecialized` / `RecompileDeoptSpecialized` lowering
/// (`arch/aarch64/compile.rs`). Unlike x86 this catches a recompile-time panic so it
/// cannot abort across the `extern "C"` boundary (aarch64 can panic emitting a
/// large body); on panic it re-arms the JIT region as executable (mirrors
/// `jit_compile_loop`) and returns — the generated code then just deopts to the
/// interpreter (the old specialized body stays installed). No `Option<Value>`
/// is returned: there is no FatalError path here, matching x86's recompile
/// behavior (which is best-effort and falls back to deopt on failure).
#[cfg(target_arch = "aarch64")]
pub(in crate::codegen) extern "C" fn jit_recompile_specialized(
    globals: &mut Globals,
    idx: usize,
    reason: RecompileReason,
) {
    if salvage_specialized(globals, idx, reason) {
        return;
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .recompile_specialized(globals, idx, reason);
        });
    }));
    if result.is_err() {
        CODEGEN.with(|codegen| codegen.borrow_mut().jit.finalize());
    }
}

#[allow(dead_code)] // referenced by the aarch64 patch path; dead on x86.
pub(in crate::codegen) extern "C" fn jit_compile_patch(
    globals: &mut Globals,
    lfp: Lfp,
    entry_patch_point: monoasm::CodePtr,
) {
    //let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
    CODEGEN.with(|codegen| {
        codegen
            .borrow_mut()
            .compile_patch(globals, lfp, entry_patch_point);
    });
    //}));
    //if result.is_err() {
    //    #[cfg(feature = "jit-log")]
    //    eprintln!("[JIT] compile_patch panicked, falling back to VM interpreter");
    //}
}

/// Warm-up profiler hook: called from the JIT stub when its per-stub warm-up
/// counter expires. Instead of unconditionally compiling for whatever receiver
/// class happens to be current (which thrashes the compiler when each call has
/// a fresh receiver class — e.g. `Class.new.foo` in a loop), it samples the
/// current `self`'s class and only compiles once that class is hot.
///
/// If the class is not hot yet, it re-arms the stub's counter (`counter_addr`)
/// so the stub keeps interpreting and samples again after another interval.
///
/// `entry_patch_point` is the stub's patch/slot pointer, forwarded verbatim to
/// `compile_patch` (an entry to branch-patch on x86; an indirect-dispatch slot
/// on aarch64).
pub(in crate::codegen) extern "C" fn jit_profile_patch(
    globals: &mut Globals,
    lfp: Lfp,
    entry_patch_point: monoasm::CodePtr,
    counter_addr: *mut i32,
) {
    let self_class = lfp.self_val().class();
    let func_id = lfp.func_id();
    let iseq_id = globals.store[func_id].as_iseq();
    if globals.store[iseq_id].profile_self_class(self_class) {
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .compile_patch(globals, lfp, entry_patch_point);
        });
    } else {
        // Not hot yet — keep interpreting and sample again later.
        // SAFETY: `counter_addr` is the address of the calling stub's own i32
        // warm-up counter (a heap-leaked word on aarch64, a JIT data slot on
        // x86); the stub passes its own counter's address.
        unsafe { *counter_addr = COUNT_START_COMPILE };
    }
}

/// Salvage-only entry for a constant-version guard miss (`ConstMiss::Salvage`).
///
/// [`salvage_method_const`] without the recompile fallback: on success the
/// unit's version word is re-stamped and its guard passes again from the next
/// call; on failure this invocation deopts and the next miss retries. Either
/// way the caller falls through to its deopt, so there is nothing to report.
///
/// Both backends call this one entry: unlike the recompile entries it neither
/// emits code nor touches a frame, so it needs no per-arch variant.
pub(in crate::codegen) extern "C" fn jit_salvage_const(globals: &mut Globals, lfp: Lfp) {
    salvage_method_const(globals, lfp, RecompileReason::ConstVersionGuardFailed);
}

#[cfg(target_arch = "x86_64")]
extern "C" fn jit_recompile_method(globals: &mut Globals, lfp: Lfp, reason: RecompileReason) {
    if salvage_method_const(globals, lfp, reason) {
        return;
    }
    //let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
    CODEGEN.with(|codegen| {
        codegen.borrow_mut().recompile_method(globals, lfp, reason);
    });
    //}));
    //if result.is_err() {
    //    #[cfg(feature = "jit-log")]
    //    eprintln!("[JIT] recompile_method panicked, falling back to VM interpreter");
    //}
}

/// aarch64 counterpart, called from the `RecompileDeopt` lowering
/// (`arch/aarch64/compile.rs`). Recompiles the current method and overwrites its
/// dispatch slot via [`Codegen::recompile_method`].
///
/// Returns `Option<Value>`: `Some(nil)` on success, `None` if recompilation
/// panicked. A panic must not cross this `extern "C"` boundary and abort the
/// process — aarch64 can panic while emitting a large method body (e.g. an
/// out-of-range branch to a far deopt). We catch it, re-arm the JIT region as
/// executable (the aborted emit left it writable, mirroring `jit_compile_loop`),
/// and set a Ruby `FatalError` on `vm`. The caller (`emit_recompile_deopt`)
/// checks the `None` return and branches to the error side-exit so the
/// `FatalError` is raised (it is uncatchable by `rescue` and propagates up).
#[cfg(target_arch = "aarch64")]
pub(in crate::codegen) extern "C" fn jit_recompile_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    reason: RecompileReason,
) -> Option<Value> {
    // Salvage first (the per-unit version words are patchable on aarch64
    // too): a successful validation heals the unit in place — this
    // invocation deopts to the VM once and the guards pass from the next
    // call on, skipping the full recompile below.
    if matches!(reason, RecompileReason::ClassVersionGuardFailed) {
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::RECOVERY_ATTEMPT);
        if globals.store.update_inline_cache(lfp) {
            #[cfg(feature = "jit-log")]
            crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SALVAGE_OK);
            return Some(Value::nil());
        }
    }
    if salvage_method_const(globals, lfp, reason) {
        return Some(Value::nil());
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CODEGEN.with(|codegen| {
            codegen.borrow_mut().recompile_method(globals, lfp, reason);
        });
    }));
    if result.is_err() {
        CODEGEN.with(|codegen| codegen.borrow_mut().jit.finalize());
        vm.set_error(MonorubyErr::fatal(
            "internal error: JIT method recompilation panicked.",
        ));
        return None;
    }
    Some(Value::nil())
}

/// aarch64 twin of the x86 `jit_recompile_method_with_recovery`: the
/// class-version-guard miss handler that distinguishes a successful salvage
/// from a full recompile, so the emitted guard can resume compiled code in
/// place instead of paying a deopt-to-VM for the rest of the invocation.
///
/// Returns a tri-state (the x86 version's `u64` grown by the panic case,
/// which x86 does not surface):
/// - `2` — salvaged: every assumption re-validated, the unit's version word
///   re-stamped. The caller jumps straight back to the guarded fall-through.
/// - `1` — recompiled (or the recompile found real changes): deopt once and
///   enter the new code on the next call, as before.
/// - `0` — the recompile panicked; a Ruby `FatalError` is set on `vm` and
///   the caller deopts, where the interpreter propagates it (same contract
///   as [`jit_recompile_method`]).
#[cfg(target_arch = "aarch64")]
pub(in crate::codegen) extern "C" fn jit_recompile_method_with_recovery(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    reason: RecompileReason,
) -> u64 {
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::RECOVERY_ATTEMPT);
    if globals.store.update_inline_cache(lfp) {
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SALVAGE_OK);
        return 2;
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CODEGEN.with(|codegen| {
            codegen.borrow_mut().recompile_method(globals, lfp, reason);
        });
    }));
    if result.is_err() {
        CODEGEN.with(|codegen| codegen.borrow_mut().jit.finalize());
        vm.set_error(MonorubyErr::fatal(
            "internal error: JIT method recompilation panicked.",
        ));
        return 0;
    }
    1
}

/// aarch64 loop recompile, called from the `RecompileDeopt` lowering
/// (`arch/aarch64/compile.rs`) when `position` is the loop pc. Re-runs `compile_partial`
/// for the loop body and rewrites the loop's codeptr at `[pc + 8]`, so the next
/// `loop_start` enters the freshly specialized loop.
///
/// Like [`jit_recompile_method`] it returns `Option<Value>` (`Some(nil)` on
/// success, `None` on a recompile-time panic) and surfaces a panic as a Ruby
/// `FatalError`: aarch64 can panic while emitting a large loop body (an
/// out-of-range branch to a far deopt), exactly the case `jit_compile_loop`
/// already catches for the initial loop JIT.
#[cfg(target_arch = "aarch64")]
pub(in crate::codegen) extern "C" fn jit_recompile_loop(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    reason: RecompileReason,
) -> Option<Value> {
    if salvage_loop(globals, lfp, pc, reason) {
        return Some(Value::nil());
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .compile_partial(globals, lfp, pc, Some(reason));
        });
    }));
    if result.is_err() {
        CODEGEN.with(|codegen| codegen.borrow_mut().jit.finalize());
        vm.set_error(MonorubyErr::fatal(
            "internal error: JIT loop recompilation panicked.",
        ));
        return None;
    }
    Some(Value::nil())
}

#[cfg(target_arch = "x86_64")]
extern "C" fn jit_recompile_method_with_recovery(
    globals: &mut Globals,
    lfp: Lfp,
    reason: RecompileReason,
) -> u64 {
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::RECOVERY_ATTEMPT);
    if globals.store.update_inline_cache(lfp) {
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SALVAGE_OK);
        return 1;
    };
    //    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
    CODEGEN.with(|codegen| {
        codegen.borrow_mut().recompile_method(globals, lfp, reason);
    });
    //    }));
    //if result.is_err() {
    //    #[cfg(feature = "jit-log")]
    //    eprintln!("[JIT] recompile_method_with_recovery panicked, falling back to VM interpreter");
    //}
    0
}

///
/// Compile the loop.
///
/// Loop (partial) JIT is arch-neutral: `compile_partial` drives the shared
/// front-end and the per-arch `*_gen_machine_code` lowering, so this runs on
/// every arch, not just x86. On
/// aarch64 the lowering bails (and `compile_partial` writes no codeptr) for a
/// loop body that still contains an unported AsmInst; the VM stub parks the
/// loop counter so it does not retry the failed compile every iteration.
pub(in crate::codegen) extern "C" fn jit_compile_loop(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) {
    if globals.no_jit {
        return;
    }
    // A panic during loop compilation must not abort the process across this
    // `extern "C"` boundary. The aarch64 backend can still panic on a large
    // loop body (e.g. an out-of-range `TBZ`/`TBNZ` to a far deopt — imm14 is
    // only ±32 KiB); catch it, leave the codeptr unpublished (the VM keeps
    // interpreting and parks the loop counter), and re-arm the JIT region as
    // executable since the aborted emit left it writable. A no-op on x86,
    // whose branches reach far enough never to overflow.
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compile_loop(globals, lfp, pc, None);
    }));
    if result.is_err() {
        CODEGEN.with(|codegen| codegen.borrow_mut().jit.finalize());
    }
}

///
/// Recompile the loop.
///
#[cfg(target_arch = "x86_64")]
extern "C" fn jit_recompile_loop(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    reason: RecompileReason,
) {
    if salvage_loop(globals, lfp, pc, reason) {
        return;
    }
    compile_loop(globals, lfp, pc, Some(reason));
}

/// ④: before recompiling a loop body whose class-version guard failed,
/// try to salvage it — re-resolve the unit's cached callees and, when
/// nothing changed, patch its version word. The current invocation still
/// deopts to the VM once; the healed body passes its guard from the next
/// iteration on. `lfp` is the loop's own frame, so `super` sites re-resolve
/// soundly.
fn salvage_loop(globals: &mut Globals, lfp: Lfp, pc: BytecodePtr, reason: RecompileReason) -> bool {
    let self_class = lfp.self_val().class();
    let func_id = lfp.func_id();
    let iseq_id = globals.store[func_id].as_iseq();
    let index = globals.store[iseq_id].get_pc_index(Some(pc));
    if matches!(reason, RecompileReason::ConstVersionGuardFailed) {
        return salvage_const(globals, iseq_id, self_class, Some(index));
    }
    if !matches!(reason, RecompileReason::ClassVersionGuardFailed) {
        return false;
    }
    #[cfg(feature = "jit-log")]
    crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::RECOVERY_ATTEMPT_LOOP);
    if let Some(version_label) = globals
        .store
        .salvage_loop_unit(iseq_id, self_class, index, Some(lfp))
    {
        // Patch here rather than inside the salvage helper: this entry point
        // is called *outside* any CODEGEN borrow, but the helper is shared
        // with `recompile_specialized`, which runs inside one. Read the
        // version before the mutable borrow — it borrows the same
        // thread-local.
        let version = Globals::class_version();
        CODEGEN.with(|codegen| {
            codegen
                .borrow_mut()
                .set_class_version(version, &version_label);
        });
        #[cfg(feature = "jit-log")]
        crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::SALVAGE_OK_LOOP);
        true
    } else {
        false
    }
}

fn compile_loop(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    is_recompile: Option<RecompileReason>,
) {
    CODEGEN.with(|codegen| {
        codegen
            .borrow_mut()
            .compile_partial(globals, lfp, pc, is_recompile);
    });
}
