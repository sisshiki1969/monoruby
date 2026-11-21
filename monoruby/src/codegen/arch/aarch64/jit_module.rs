//! aarch64 `JitModule` asm methods: bytecode dispatch + frame
//! primitives, `brk` stubs, `JitModule::new`, and the per-signal stub.
//!
//! Counterpart of `arch/x86_64/jit_module.rs`. Instruction sequences are
//! written with the `monoasm_arm64!` A64 assembly DSL (the aarch64 analogue
//! of x86's `monoasm!`); the role registers (`EXEC`/`PC`/`TBL`/…) stay
//! `GReg` constants and are spelled `x(NAME.0)` in the DSL. Non-instruction
//! builder calls (label allocation/binding, `get_current_address`,
//! `finalize`, …) remain plain method calls and interleave with the macro,
//! exactly as in the x86 backend.

use super::*;
use monoasm_macro::monoasm_arm64;

impl JitModule {
    /// Fetch the opcode and dispatch through the 256-entry table.
    ///
    /// x86: `movq r15,(table); movzxb rax,[r13+OPECODE]; addq r13,16;
    /// jmp [r15+rax*8]`. On aarch64 the pc is *not* pre-advanced, so handlers
    /// read operands at positive offsets and advance the pc themselves before
    /// re-dispatching.
    ///
    /// ### in
    /// - x21 (PC): bytecode pointer
    /// ### destroy
    /// - x9 (TBL), x10 (OP), x11 (TGT)
    pub(in crate::codegen) fn a64_fetch_and_dispatch(&mut self) {
        let table = self.dispatch.as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov  x(TBL.0), (table);                       // table base
            ldrb x(OP.0), [x(PC.0), #(OPECODE)];          // opcode <- [pc + OPECODE]
            ldr  x(TGT.0), [x(TBL.0), x(OP.0), lsl #3];   // handler <- table[opcode]
            br   x(TGT.0);
        );
    }

    /// Address of the local slot whose index is in `reg`:
    /// `[r14 + reg*8 - LFP_SELF]`. x86: `negq R(r); lea R(r),[r14+R(r)*8-SELF]`.
    /// aarch64: `neg; add Xr,x_lfp,Xr,lsl #3; sub #LFP_SELF`.
    ///
    /// ### in / out
    /// - `dst`: slot index in → slot *address* out
    pub(in crate::codegen) fn a64_slot_addr(&mut self, dst: GReg) {
        monoasm_arm64!(&mut self.jit,
            neg x(dst.0), x(dst.0);
            add x(dst.0), x(LFP.0), x(dst.0), lsl #3;
            sub x(dst.0), x(dst.0), #(LFP_SELF);
        );
    }

    /// Value of the local slot whose index is in `reg` (slot index → value).
    pub(in crate::codegen) fn a64_slot_value(&mut self, dst: GReg) {
        self.a64_slot_addr(dst);
        monoasm_arm64!(&mut self.jit,
            ldr x(dst.0), [x(dst.0)];
        );
    }

    /// Like `a64_brk_stub` but reports a diagnostic id before trapping.
    pub(in crate::codegen) fn a64_brk_stub_diag(&mut self, label: &DestLabel, id: u64) {
        monoasm_arm64!(&mut self.jit,
        label:
            mov x0, (id);
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #0;
        );
    }

    pub(in crate::codegen) fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let jit_class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        // One bit per `VmBop`, tested inline by the VM handler that assumes
        // that operator (see `VmBop`).
        let bop_flags = jit.data_i64(0);
        let const_version = jit.data_i64(1);
        // The poll word (poll_flag.rs). Under `gc-stress` the GC lane
        // starts armed so the very first safepoint collects.
        let poll_flag = jit.data_i32(if cfg!(feature = "gc-stress") {
            crate::poll_flag::GC_LANE as i32
        } else {
            0
        });
        let entry_raise = jit.label();
        let entry_panic = jit.label();
        let exec_gc = jit.label();
        let f64_to_val = jit.label();
        let stack_overflow = jit.label();
        let write_barrier = jit.label();
        let ivar_ty_observe = jit.label();

        // TODO(aarch64): emit the real entry stubs (raise/fetch_and_dispatch/
        // panic/f64_to_val/gc). For now trap so the module links + constructs.
        let entry_unimpl = jit.get_current_address();
        monoasm_arm64!(&mut jit,
            mov x0, x(OP.0);                             // OP (X10) holds the opcode at dispatch time
            mov x9, (crate::codegen::runtime::report_unimpl_op as *const () as u64);
            blr x9;
            brk #0;
        );
        let dispatch = vec![entry_unimpl; 256];
        let mut j = Self {
            jit,
            class_version,
            unit_version_patch_sites: Vec::new(),
            version_imm_sites: std::collections::HashMap::default(),
            jit_class_version,
            const_version,
            poll_flag,
            entry_raise,
            exec_gc,
            f64_to_val,
            vm_stack_overflow: stack_overflow,
            entry_panic,
            write_barrier,
            ivar_ty_observe,
            dispatch: dispatch.into_boxed_slice().try_into().unwrap(),
            bop_redefined_flags,
            bop_flags,
        };
        let raise = j.entry_raise.clone();
        let panic = j.entry_panic.clone();
        let gc = j.exec_gc.clone();
        let f64v = j.f64_to_val.clone();
        let ovf = j.vm_stack_overflow.clone();
        // entry_raise is emitted later in construct_vm (it needs
        // fetch_and_dispatch); the rest stay stubs for now.
        let _ = &raise;
        j.a64_brk_stub_diag(&panic, 0xffff02); // entry_panic
        // exec_gc is bound by `construct_vm` (real handler defers to
        // entry_raise on a poll-time error).
        let _ = gc;
        // f64_to_val is bound later in `construct_vm` (a Codegen context):
        // `a64_gen_f64_to_val` is an `impl Codegen` helper and nothing in `new`
        // references the label yet, so leaving it unbound here is fine.
        let _ = &f64v;
        // vm_stack_overflow is bound by `construct_vm` (the real handler
        // needs entry_raise, which isn't emitted until then).
        let _ = ovf;
        j.jit.finalize();
        j
    }

}
