//! aarch64 (JIT front-end, `jit && !jit_emit`) stub for AsmIR→machine-code
//! lowering. The front-end builds `AsmIr`, but no aarch64 emission exists yet,
//! so `compile_asmir` bails: it marks the compilation unsupported so the
//! driver aborts (`compile() -> Err -> None`) and the method stays
//! VM-interpreted. Opcodes are ported incrementally from here.
//! See `doc/aarch64-jitgen-plan.md`.

use super::*;

impl Codegen {
    pub(super) fn compile_asmir(
        &mut self,
        _store: &Store,
        _frame: &mut AsmInfo,
        _labels: &SideExitLabels,
        _inst: AsmInst,
        _class_version: DestLabel,
    ) {
        // Driver is jit_emit-gated and not wired on aarch64 yet, so this is
        // unreachable; opcode lowering will replace it incrementally.
        unreachable!("aarch64 AsmIR lowering not implemented")
    }
}
