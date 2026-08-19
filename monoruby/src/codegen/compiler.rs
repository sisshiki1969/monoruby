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
    ) -> Option<(Vec<InlineCacheEntry>, DestLabel)> {
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
    ) -> Option<(Vec<InlineCacheEntry>, DestLabel)> {
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
            Ok((cache, specialized_info, class_version_label, bop_deps)) => {
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
                            "{} {} {position:?} ({} bytes, {} bytes) {elapsed:?}",
                            globals
                                .store
                                .func_description(globals.store[iseq_id].func_id()),
                            globals.store.debug_class_name(self_class),
                            span.0.1,
                            span.1.1
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

                Some((cache, class_version_label))
            }
            Err(_) => {
                if position.is_none() {
                    globals.store[iseq_id].invalidate_jit_code();
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
        let (cache, _) = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            Some(reason),
        )?;
        globals.store[iseq_id].set_cache_map(self_class, cache);
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
        let compiled = self
            .compile_method(
                globals,
                iseq_id,
                self_class,
                jit_entry.clone(),
                class_version,
                Some(reason),
            )
            .is_some();
        if !compiled {
            self.jit.finalize();
            return None;
        }
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

        let ret = if self
            .compile(
                globals,
                iseq_id,
                self_class,
                Some(pc),
                entry_label.clone(),
                class_version,
                is_recompile,
            )
            .is_some()
        {
            let codeptr = self.jit.get_label_address(&entry_label);
            pc.write2(codeptr.as_ptr() as u64);
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
        } = self.specialized_info[idx].clone();
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
    CODEGEN.with(|codegen| {
        codegen
            .borrow_mut()
            .recompile_specialized(globals, idx, reason);
    });
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

#[cfg(target_arch = "x86_64")]
extern "C" fn jit_recompile_method(globals: &mut Globals, lfp: Lfp, reason: RecompileReason) {
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
    if globals.store.update_inline_cache(lfp) {
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
    compile_loop(globals, lfp, pc, Some(reason));
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
