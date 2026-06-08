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
    /// longer matches the version this method was compiled against. The cached
    /// version is baked in as an immediate (compilation is atomic at a fixed
    /// version). Unlike x86 we do not recompile on miss yet — just deopt.
    pub(in crate::codegen) fn a64_guard_class_version(&mut self, fail: &DestLabel) {
        let gv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        let cached = self.class_version();
        monoasm_arm64!(&mut self.jit,
            mov x9, (gv_addr);
            ldr w9, [x9];
            mov x10, (cached as u64);
            cmp x9, x10;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, fail);
    }
}
