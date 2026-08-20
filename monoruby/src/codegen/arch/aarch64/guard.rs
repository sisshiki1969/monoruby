//! aarch64 type / class-version guards (AsmIR→machine-code lowering).
//!
//! Counterpart of the x86 `guard.rs`. Only the guards the aarch64 port
//! currently emits live here; the set grows as the lowering in
//! `arch/aarch64/compile.rs` covers more `AsmInst` variants.

use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {
    /// Type guard: branch to `fail` if the Value in `reg` is not of `class_id`.
    /// Mirrors x86 `guard_class` (immediate tag checks + heap class check).
    /// Returns `false` (bail) for class kinds not yet handled.
    pub(in crate::codegen) fn a64_guard_class(
        &mut self,
        reg: GP,
        class_id: ClassId,
        fail: &DestLabel,
    ) -> bool {
        let r = reg.a64().0;
        let fail = fail.clone();
        match class_id {
            INTEGER_CLASS => {
                // fixnum: bit0 == 1; fail when clear.
                monoasm_arm64!(&mut self.jit, tbz x(r), #(0), fail;);
            }
            NIL_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(NIL_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            TRUE_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(TRUE_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            FALSE_CLASS => {
                monoasm_arm64!(&mut self.jit, cmp x(r), #(FALSE_VALUE as u32););
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            SYMBOL_CLASS => {
                monoasm_arm64!(&mut self.jit,
                    mov x9, (0xff);
                    and x9, x(r), x9;
                    cmp x9, #(TAG_SYMBOL as u32);
                );
                self.jit.bcond_label(monoasm::Cond::Ne, &fail);
            }
            FLOAT_CLASS => {
                // flonum (bit1 set, bit0 clear) is ok; fixnum (bit0) fails;
                // otherwise check the heap Float class.
                let exit = self.jit.label();
                monoasm_arm64!(&mut self.jit,
                    tbnz x(r), #(0), fail;   // fixnum -> fail
                    tbnz x(r), #(1), exit;   // flonum -> ok
                );
                self.a64_guard_rvalue(r, class_id, &fail);
                self.jit.bind_label(exit);
            }
            _ => {
                self.a64_guard_rvalue(r, class_id, &fail);
            }
        }
        true
    }

    /// Method-entry variant of [`a64_guard_class`] for the class-guard stub:
    /// identical except a heap `Integer` (BigNum) receiver is routed straight to
    /// `vm_entry` — the JIT body is compiled for fixnum `Integer` and can't run
    /// a BigNum — instead of falling into the guard-miss / profile-patch chain
    /// (which would keep re-compiling a specialization that rejects it anyway).
    /// Mirrors x86 `guard_class2`. Only `INTEGER_CLASS` differs from
    /// `a64_guard_class`: a heap `Float` is still handled by the Float JIT body,
    /// so `FLOAT_CLASS` and every other class delegate unchanged.
    pub(in crate::codegen) fn a64_guard_class2(
        &mut self,
        reg: GP,
        class_id: ClassId,
        fail: &DestLabel,
    ) {
        if class_id == INTEGER_CLASS {
            let r = reg.a64().0;
            let vm_entry = self.vm_entry();
            let exit = self.jit.label();
            monoasm_arm64!(&mut self.jit,
                tbnz x(r), #(0), exit;   // fixnum -> JIT body
            );
            self.a64_guard_rvalue(r, INTEGER_CLASS, fail); // heap non-Integer -> miss
            monoasm_arm64!(&mut self.jit,
                b vm_entry;              // heap Integer (BigNum) -> VM
            );
            self.jit.bind_label(exit);
        } else {
            self.a64_guard_class(reg, class_id, fail);
        }
    }

    /// Heap-object class guard: branch to `fail` unless `reg` is a heap pointer
    /// (low 3 bits zero) whose RValue class equals `class_id`. Mirrors x86
    /// `guard_rvalue`.
    pub(in crate::codegen) fn a64_guard_rvalue(
        &mut self,
        r: u32,
        class_id: ClassId,
        fail: &DestLabel,
    ) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (0b111);
            and x9, x(r), x9;
            cbnz x9, fail;                                  // immediate -> fail
            ldr w9, [x(r), #(RVALUE_OFFSET_CLASS as u32)];  // RValue.class (u32)
            mov x10, (class_id.u32() as u64);
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, fail);
    }

    /// Class-version guard: branch to `fail` if the global class version no
    /// longer matches the *unit's snapshot word* — the patchable per-unit
    /// `class_version_label` created by `jit_compile`. Reading the word
    /// (rather than baking the version as an immediate, as this used to)
    /// lets a successful salvage re-validate the unit's code in place by
    /// storing the current version into the word (`set_class_version`),
    /// exactly like x86's `check_version`.
    pub(in crate::codegen) fn a64_guard_class_version(
        &mut self,
        unit_word: &DestLabel,
        fail: &DestLabel,
    ) {
        let gv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        let unit_addr = self.jit.get_label_address(unit_word).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x9, (gv_addr);
            ldr w9, [x9];
            mov x10, (unit_addr);
            ldr w10, [x10];
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, fail);
    }
}
