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
    ) -> Option<(Vec<(ClassId, Option<IdentId>, FuncId)>, DestLabel)> {
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

    #[cfg(jit_x86)]
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
            movq rax, (jit_compile_patch as *const ());
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
    #[cfg(jit_x86)]
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

    #[cfg(jit_x86)]
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

    #[cfg(jit_x86)]
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
    ) -> Option<(Vec<(ClassId, Option<IdentId>, FuncId)>, DestLabel)> {
        if position.is_none() && globals.store[iseq_id].jit_invalidated() {
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
            Ok((cache, specialized_info, class_version_label)) => {
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

    #[cfg(jit_x86)]
    fn recompile_method(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        reason: RecompileReason,
    ) -> Option<()> {
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
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
        // get_jit_code() must not be None.
        // After BOP redefinition occurs, recompilation in invalidated methods cause None.
        if let Some(patch_point) = globals.store[iseq_id].get_jit_entry(self_class) {
            globals.store[iseq_id].set_cache_map(self_class, cache);
            let patch_point = self.jit.get_label_address(&patch_point);
            self.jit.apply_jmp_patch_address(patch_point, &jit_entry);
        } else {
            // JIT entry was invalidated (e.g. by BOP redefinition).
            // The compiled code is discarded; execution falls back to the VM interpreter.
            #[cfg(feature = "jit-log")]
            eprintln!(
                "[JIT] recompilation skipped for invalidated method: {:?}",
                globals.store[func_id].name()
            );
            return None;
        }
        Some(())
    }

    /// aarch64 whole-method recompile: re-run the front-end + A64 lowering and
    /// overwrite the method's dispatch slot in place (aarch64 installs JIT code
    /// via the indirect slot recorded at `compile_patch`, not x86 branch
    /// patching). Bails (leaving the old code) if the slot is unknown or the
    /// method was JIT-invalidated.
    #[cfg(not(jit_x86))]
    fn recompile_method(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        reason: RecompileReason,
    ) -> Option<()> {
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
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
        Some(())
    }

    #[cfg(jit)]
    fn compile_partial(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        pc: BytecodePtr,
        is_recompile: Option<RecompileReason>,
    ) -> Option<()> {
        let entry_label = self.jit.label();
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
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

    #[cfg(jit_x86)]
    fn recompile_specialized(
        &mut self,
        globals: &mut Globals,
        idx: usize,
        reason: RecompileReason,
    ) -> Option<()> {
        let (iseq_id, self_class, patch_point) = self.specialized_info[idx].clone();

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
}

//
// JIT Compiler API for asm codes.
//

#[cfg(jit_x86)]
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

#[cfg(jit_x86)]
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
/// (`compile_stub.rs`). Recompiles the current method and overwrites its
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
#[cfg(not(jit_x86))]
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

#[cfg(jit_x86)]
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
/// front-end and the per-arch `*_gen_machine_code` lowering, so this is
/// enabled on every JIT-capable arch (`jit`), not just x86 (`jit_x86`). On
/// aarch64 the lowering bails (and `compile_partial` writes no codeptr) for a
/// loop body that still contains an unported AsmInst; the VM stub parks the
/// loop counter so it does not retry the failed compile every iteration.
#[cfg(jit)]
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
#[cfg(jit_x86)]
extern "C" fn jit_recompile_loop(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    reason: RecompileReason,
) {
    compile_loop(globals, lfp, pc, Some(reason));
}

#[cfg(jit)]
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
