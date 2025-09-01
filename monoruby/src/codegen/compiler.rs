use super::*;

//
// JIT Compiler API for asm codes.
//

pub(super) extern "C" fn exec_jit_specialized_recompile(
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

pub(super) extern "C" fn exec_jit_compile_patch(
    globals: &mut Globals,
    lfp: Lfp,
    entry_patch_point: monoasm::CodePtr,
) {
    CODEGEN.with(|codegen| {
        codegen
            .borrow_mut()
            .compile_patch(globals, lfp, entry_patch_point);
    });
}

pub(super) extern "C" fn exec_jit_recompile_method(
    globals: &mut Globals,
    lfp: Lfp,
    reason: RecompileReason,
) {
    CODEGEN.with(|codegen| {
        codegen.borrow_mut().recompile_method(globals, lfp, reason);
    });
}

pub(super) extern "C" fn exec_jit_recompile_method_with_recovery(
    globals: &mut Globals,
    lfp: Lfp,
    reason: RecompileReason,
) -> u64 {
    if globals.store.update_inline_cache(lfp) {
        return 1;
    };
    CODEGEN.with(|codegen| {
        codegen.borrow_mut().recompile_method(globals, lfp, reason);
    });
    0
}

///
/// Compile the loop.
///
pub(super) extern "C" fn exec_jit_partial_compile(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) {
    if globals.no_jit {
        return;
    }
    partial_compile(globals, lfp, pc, None);
}

///
/// Recompile the loop.
///
pub(super) extern "C" fn exec_jit_recompile_partial(
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    reason: RecompileReason,
) {
    partial_compile(globals, lfp, pc, Some(reason));
}

fn partial_compile(
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
        class_version_label: DestLabel,
        is_recompile: Option<RecompileReason>,
    ) -> Vec<(bytecodegen::BcIndex, InlineCacheType)> {
        self.compile(
            globals,
            iseq_id,
            self_class,
            None,
            jit_entry,
            class_version,
            class_version_label,
            is_recompile,
        )
    }

    fn compile(
        &mut self,
        globals: &mut Globals,
        iseq_id: ISeqId,
        self_class: ClassId,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
        class_version: u32,
        class_version_label: DestLabel,
        is_recompile: Option<RecompileReason>,
    ) -> Vec<(bytecodegen::BcIndex, InlineCacheType)> {
        #[cfg(feature = "profile")]
        {
            if let Some(reason) = &is_recompile {
                globals.countup_recompile(globals.store[iseq_id].func_id(), self_class, reason);
            }
        }

        #[cfg(any(feature = "emit-asm", feature = "jit-log", feature = "jit-debug"))]
        if self.startup_flag {
            let iseq = &globals.store[iseq_id];
            let start_pos = iseq.get_pc_index(position);
            let name = globals.store.func_description(iseq.func_id());
            eprintln!(
                "==> start {} {}compile: {}{:?} <{}> {}self_class: {} {}:{}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                if is_recompile.is_some() { "re" } else { "" },
                if let Some(reason) = is_recompile {
                    format!("({:?}) ", reason)
                } else {
                    String::new()
                },
                iseq.func_id(),
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
        #[cfg(feature = "emit-asm")]
        {
            globals.store.dump_iseq(iseq_id);
        }

        #[cfg(feature = "jit-log")]
        let now = std::time::Instant::now();

        let cache = self.jit_compile(
            &globals.store,
            iseq_id,
            self_class,
            position,
            entry_label,
            class_version,
            class_version_label,
        );

        if self.startup_flag {
            #[cfg(feature = "jit-log")]
            {
                let elapsed = now.elapsed();
                eprintln!("<== finished compile. elapsed:{:?}", elapsed);
                self.jit_compile_time += elapsed;
            }

            #[cfg(any(feature = "jit-debug", feature = "jit-log"))]
            {
                self.jit.select_page(0);
                eprintln!("    total bytes(0):{:?}", self.jit.get_current());
                self.jit.select_page(1);
                eprintln!("    total bytes(1):{:?}", self.jit.get_current());
                self.jit.select_page(0);
            }
            #[cfg(feature = "emit-asm")]
            eprintln!("<== finished compile.");
        }

        cache
    }

    fn recompile_method(&mut self, globals: &mut Globals, lfp: Lfp, reason: RecompileReason) {
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        let jit_entry = self.jit.label();
        let class_version = self.class_version();
        let class_version_label = self.jit.const_i32(class_version as _);
        let cache = self.compile_method(
            globals,
            iseq_id,
            self_class,
            jit_entry.clone(),
            class_version,
            class_version_label,
            Some(reason),
        );
        // get_jit_code() must not be None.
        // After BOP redefinition occurs, recompilation in invalidated methods cause None.
        if let Some(patch_point) = globals.store[iseq_id].get_jit_entry(self_class) {
            globals.store[iseq_id].set_cache_map(self_class, cache);
            let patch_point = self.jit.get_label_address(&patch_point);
            self.jit.apply_jmp_patch_address(patch_point, &jit_entry);
        } else {
            unreachable!(
                "Warning: recompilation of invalidated method: {:?}",
                globals.store[func_id].name()
            );
        }
    }

    fn compile_partial(
        &mut self,
        globals: &mut Globals,
        lfp: Lfp,
        pc: BytecodePtr,
        is_recompile: Option<RecompileReason>,
    ) {
        let entry_label = self.jit.label();
        let self_class = lfp.self_val().class();
        let func_id = lfp.func_id();
        let iseq_id = globals.store[func_id].as_iseq();
        let class_version = self.class_version();
        let class_version_label = self.jit.const_i32(class_version as _);
        self.compile(
            globals,
            iseq_id,
            self_class,
            Some(pc),
            entry_label.clone(),
            class_version,
            class_version_label,
            is_recompile,
        );
        let codeptr = self.jit.get_label_address(&entry_label);
        pc.write2(codeptr.as_ptr() as u64);
    }

    fn recompile_specialized(
        &mut self,
        globals: &mut Globals,
        idx: usize,
        reason: RecompileReason,
    ) {
        let (iseq_id, self_class, patch_point) = self.specialized_patch_point[idx].clone();

        let entry = self.jit.label();
        let class_version = self.class_version();
        let class_version_label = self.jit.const_i32(class_version as _);
        self.compile(
            globals,
            iseq_id,
            self_class,
            None,
            entry.clone(),
            class_version,
            class_version_label,
            Some(reason),
        );

        let patch_point = self.jit.get_label_address(&patch_point);
        self.jit.apply_jmp_patch_address(patch_point, &entry);
    }
}
