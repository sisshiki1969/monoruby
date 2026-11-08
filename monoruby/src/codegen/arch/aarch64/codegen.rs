//! aarch64 `Codegen` asm helpers: entry-raise / stack-overflow /
//! stack-check / GC-poll code generation.
//!
//! Counterpart of `arch/x86_64/codegen.rs`.

use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {
    /// entry_raise: error/exception dispatch. Calls `handle_error`, which
    /// returns (value, dest): if `dest` is Some, resume execution there (a
    /// rescue/ensure/retry target); otherwise unwind this VM frame and
    /// return the error (None) to the caller (x86 `init`'s `raise:` block).
    pub(in crate::codegen) fn a64_gen_entry_raise(&mut self) {
        let raise = self.entry_raise.clone();
        let goto = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            raise:
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldur x2, [x(LFP.0), #(-(LFP_META as i32))];  // meta = [LFP - LFP_META]
            mov x3, x(PC.0);  // pc = current instruction
            mov x9, (crate::codegen::jit_module::handle_error as *const () as u64);
            blr x9;
        // x0 = value (Option<Value>), x1 = dest (Option<BytecodePtr>)
            cbnz x1, goto;
        // no handler: unwind this frame and return the error (x0 = None)
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
            goto:
            mov x(PC.0), x1;  // resume at the handler pc
        );
        self.a64_fetch_and_dispatch();
    }

    /// Bind `vm_stack_overflow`: call `stack_overflow(EXEC)` to set the
    /// SystemStackError, then branch into `entry_raise` which unwinds the VM
    /// frame and returns the error to the Rust caller.
    pub(in crate::codegen) fn a64_gen_stack_overflow(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ovf:
            mov x0, x(EXEC.0);
            mov x9, (super::stack_overflow as *const () as u64);
            blr x9;
            b raise;
        );
    }

    /// Compare SP to `executor.stack_limit`; if SP <= limit, branch to
    /// `vm_stack_overflow`. Uses X10 and X11 as scratch. EXEC must be valid.
    pub(in crate::codegen) fn a64_check_stack(&mut self) {
        let ovf = self.vm_stack_overflow.clone();
        monoasm_arm64!(&mut self.jit,
            mov x10, sp;
            ldr x11, [x(EXEC.0), #(EXECUTOR_STACK_LIMIT as u32)];
            cmp x10, x11;
        );
        self.jit.bcond_label(Cond::Le, &ovf);
    }

    /// Bind `exec_gc`: call `executor::execute_gc(vm, globals)` (which also
    /// drains pending signals). On error (returns None / X0 == 0) branch to
    /// `entry_raise`; otherwise return to the caller. VM globals X19-X24 are
    /// callee-saved across the Rust call so we only need to preserve fp/lr.
    pub(in crate::codegen) fn a64_gen_exec_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            gc:
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            // Red-zone cushion: the poll can fire while frame data still
            // lives just below sp (a staged callee frame); drop sp past it
            // so the Rust callee's stack frames cannot overwrite it.
            // Mirrors the x86-64 exec_gc stub's `subq rsp, 1032`.
            sub sp, sp, #(1024);
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (crate::executor::execute_gc as *const () as u64);
            blr x9;
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
        // Option<Value>: None (0) → raise; else continue.
        );
        let ok = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            cbnz x0, ok;
            b raise;
            ok:
            ret;
        );
    }

    /// Bind `write_barrier`: the shared out-of-line half of the generational
    /// GC write barrier (see the field doc in `codegen.rs`). Parent in x9;
    /// saves the abstract scratch GPs (x0-x8), the caller-saved FP regs
    /// (d0-d7) and the `bl`-written return address, calls
    /// `jit_module::jit_write_barrier(parent)`, restores everything, and
    /// returns. The *site* preserves its own live x30 around the `bl`, so
    /// the stub only has to keep the return address alive across its inner
    /// `blr`.
    pub(in crate::codegen) fn a64_gen_write_barrier(&mut self) {
        let wb = self.write_barrier.clone();
        let f = crate::codegen::jit_module::jit_write_barrier as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            wb:
            sub sp, sp, #(144);
            str x0, [sp, #(0)];
            str x1, [sp, #(8)];
            str x2, [sp, #(16)];
            str x3, [sp, #(24)];
            str x4, [sp, #(32)];
            str x5, [sp, #(40)];
            str x6, [sp, #(48)];
            str x7, [sp, #(56)];
            str x8, [sp, #(64)];
            str d0, [sp, #(72)];
            str d1, [sp, #(80)];
            str d2, [sp, #(88)];
            str d3, [sp, #(96)];
            str d4, [sp, #(104)];
            str d5, [sp, #(112)];
            str d6, [sp, #(120)];
            str d7, [sp, #(128)];
            str x30, [sp, #(136)];
            mov x0, x9;                // parent -> arg0
            mov x9, (f);
            blr x9;
            ldr x30, [sp, #(136)];
            ldr d7, [sp, #(128)];
            ldr d6, [sp, #(120)];
            ldr d5, [sp, #(112)];
            ldr d4, [sp, #(104)];
            ldr d3, [sp, #(96)];
            ldr d2, [sp, #(88)];
            ldr d1, [sp, #(80)];
            ldr d0, [sp, #(72)];
            ldr x8, [sp, #(64)];
            ldr x7, [sp, #(56)];
            ldr x6, [sp, #(48)];
            ldr x5, [sp, #(40)];
            ldr x4, [sp, #(32)];
            ldr x3, [sp, #(24)];
            ldr x2, [sp, #(16)];
            ldr x1, [sp, #(8)];
            ldr x0, [sp, #(0)];
            add sp, sp, #(144);
            ret;
        );
    }

    /// The ivar type check's shared out-of-line half (object in x9, value
    /// in x10, ivar id in x11); same register discipline as
    /// [`Self::a64_gen_write_barrier`].
    pub(in crate::codegen) fn a64_gen_ivar_ty_observe(&mut self) {
        let wb = self.ivar_ty_observe.clone();
        let f = crate::ivar_ty::jit_ivar_ty_observe as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            wb:
            sub sp, sp, #(144);
            str x0, [sp, #(0)];
            str x1, [sp, #(8)];
            str x2, [sp, #(16)];
            str x3, [sp, #(24)];
            str x4, [sp, #(32)];
            str x5, [sp, #(40)];
            str x6, [sp, #(48)];
            str x7, [sp, #(56)];
            str x8, [sp, #(64)];
            str d0, [sp, #(72)];
            str d1, [sp, #(80)];
            str d2, [sp, #(88)];
            str d3, [sp, #(96)];
            str d4, [sp, #(104)];
            str d5, [sp, #(112)];
            str d6, [sp, #(120)];
            str d7, [sp, #(128)];
            str x30, [sp, #(136)];
            mov x0, x9;                // object -> arg0
            mov x1, x11;               // ivar id -> arg1
            mov x2, x10;               // value -> arg2
            mov x9, (f);
            blr x9;
            ldr x30, [sp, #(136)];
            ldr d7, [sp, #(128)];
            ldr d6, [sp, #(120)];
            ldr d5, [sp, #(112)];
            ldr d4, [sp, #(104)];
            ldr d3, [sp, #(96)];
            ldr d2, [sp, #(88)];
            ldr d1, [sp, #(80)];
            ldr d0, [sp, #(72)];
            ldr x8, [sp, #(64)];
            ldr x7, [sp, #(56)];
            ldr x6, [sp, #(48)];
            ldr x5, [sp, #(40)];
            ldr x4, [sp, #(32)];
            ldr x3, [sp, #(24)];
            ldr x2, [sp, #(16)];
            ldr x1, [sp, #(8)];
            ldr x0, [sp, #(0)];
            add sp, sp, #(144);
            ret;
        );
    }

    /// `f64_to_val`: convert the f64 in `D0` to a boxed `Value` in `X0` —
    /// flonum-encode when the exponent is in range, else heap-allocate a
    /// `Float`. Mirrors x86 `gen_f64_to_val`. Called via `bl` from `FprToStack`.
    ///
    /// `and`/`orr` have no immediate form and there is no `ror` in the
    /// `monoasm_arm64!` macro, so the bit-twiddling uses shift + register-logical
    /// ops (rotate = `lsl`/`lsr`/`orr`; clear-low-2 = `lsr #2; lsl #2`).
    pub(in crate::codegen) fn a64_gen_f64_to_val(&mut self, label: &DestLabel) {
        let normal = self.jit.label();
        let heap = self.jit.label();
        self.jit.bind_label(label.clone());
        // Only +0.0 (all bits clear) has the FLOAT_ZERO encoding. -0.0
        // compares equal to 0.0, so test the bits: it falls through to the
        // exponent check, fails it and is heap-allocated with its sign kept.
        monoasm_arm64!(&mut self.jit,
            fmov x0, d0;             // x0 = bits(d0)
            cmp x0, #(0);
        );
        self.jit.bcond_label(Cond::Ne, &normal); // not +0.0 -> normal
        monoasm_arm64!(&mut self.jit,
            mov x0, (FLOAT_ZERO);
            ret;
            normal:
            lsr x1, x0, #(60);
            add x1, x1, #(1);
            mov x9, (6);
            and x1, x1, x9;
            cmp x1, #(4);
        );
        self.jit.bcond_label(Cond::Ne, &heap); // exponent out of flonum range -> heap
        monoasm_arm64!(&mut self.jit,
            // flonum-encode: rol 3, clear low 2 bits, set bit1 (0b10).
            rol x0, x0, #(3);
            lsr x0, x0, #(2);
            lsl x0, x0, #(2);        // clear low 2 bits (and -4)
            mov x9, (2);
            orr x0, x0, x9;          // set 0b10
            ret;
            heap:
            // Heap-allocate. Save the caller-saved FP pool (D2-D7); D8-D15 are
            // AAPCS64 callee-saved and preserved by float_heap. D0 still holds
            // the f64 argument that float_heap reads.
            str x30, [sp, #(-16)]!;
            sub sp, sp, #(48);
            str d2, [sp];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            mov x9, (Value::float_heap as *const () as u64);
            blr x9;                  // x0 = Value::float_heap(d0)
            ldr d2, [sp];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            add sp, sp, #(48);
            ldr x30, [sp], #(16);
            ret;
        );
    }

    /// VM-side safepoint poll. If any lane of the poll word is set (GC
    /// request, preempt tick, pending signal — see poll_flag.rs), call
    /// `exec_gc`. The hot path is a load and a fall-through `cbz`.
    pub(in crate::codegen) fn a64_vm_execute_gc(&mut self) {
        let gc = self.exec_gc.clone();
        let skip = self.jit.label();
        let pf_addr = self.jit.get_label_address(&self.poll_flag).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            mov x10, (pf_addr);
            ldr w11, [x10];       // zero-extends into x11
            cbz x11, skip;
            bl gc;
            skip:
        );
    }

    /// `b target` (or `bl target` with `link`) to a shared stub that is
    /// already bound: `entry_raise`, `vm_fetch`, `vm_entry`, the
    /// write-barrier / `alloc_cell` / `get_class` / `f64_to_val` helpers.
    ///
    /// `B` / `BL` reach +/-128MB, and those stubs sit at the start of the
    /// code page while JIT code keeps being appended after them (a
    /// superseded unit is never reclaimed), so a unit emitted far enough in
    /// cannot reach them directly — rubyboy's `Cpu#exec`, recompiled some
    /// eighteen times at ~14MB a time, got there before its first
    /// iteration and died with "B/BL displacement out of range". Past the
    /// range the branch goes through x16 (IP0, the ABI's veneer register,
    /// which JIT code never holds a value in). x86-64's rel32 reaches
    /// +/-2GB and does not need this.
    pub(in crate::codegen) fn a64_far_branch(&mut self, target: &DestLabel, link: bool) {
        let dest = self.jit.get_label_address(target).as_ptr() as i64;
        let here = self.jit.get_current_address().as_ptr() as i64;
        // Keep a margin below the architectural +/-2^27 bytes.
        const REACH: i64 = (1 << 27) - (1 << 20);
        if (dest - here).abs() < REACH {
            if link {
                monoasm_arm64!(&mut self.jit, bl target;);
            } else {
                monoasm_arm64!(&mut self.jit, b target;);
            }
        } else {
            monoasm_arm64!(&mut self.jit, mov x16, (dest as u64););
            if link {
                monoasm_arm64!(&mut self.jit, blr x16;);
            } else {
                monoasm_arm64!(&mut self.jit, br x16;);
            }
        }
    }
}

/// Hand-encoded AdvSIMD (NEON) stores for the zero/nil fills. monoasm's
/// arm64 DSL has no Q-register forms, so these emit the instruction words
/// directly; each encoding is pinned against the architectural one by
/// `neon_fill_encodings` below.
impl Codegen {
    /// `dup v(vd).2d, x(xn)`: broadcast a 64-bit GPR into both lanes.
    pub(in crate::codegen) fn a64_dup_2d(&mut self, vd: u32, xn: u32) {
        self.jit.emitl(a64_enc::dup_2d(vd, xn));
    }

    /// `str q(vt), [x(xn)], #16`: 16-byte store, post-incrementing `xn`.
    pub(in crate::codegen) fn a64_str_q_post16(&mut self, vt: u32, xn: u32) {
        self.jit.emitl(a64_enc::str_q_post(vt, xn, 16));
    }

    /// `stp q(vt), q(vt), [x(xn)], #32`: 32-byte store, post-incrementing
    /// `xn`.
    pub(in crate::codegen) fn a64_stp_q_post32(&mut self, vt: u32, xn: u32) {
        self.jit.emitl(a64_enc::stp_q_post(vt, vt, xn, 32));
    }

    /// Store `n_slots` 8-byte words upward from `x(base)` (which the fill
    /// may advance): 32 bytes per `stp q(v), q(v)`, then a 16-byte
    /// `str q(v)` and an 8-byte `str x(xs)` for the tail. `v(v)` holds
    /// the pattern in both lanes and `x(xs)` the same pattern as a scalar.
    pub(in crate::codegen) fn a64_fill_slots_q(
        &mut self,
        base: u32,
        n_slots: usize,
        v: u32,
        xs: u32,
    ) {
        let bytes = n_slots as u32 * 8;
        // `stp`'s scaled imm7 reaches +1008; past it, step the base on.
        let mut done = 0;
        let mut pos = 0u32;
        while done + 32 <= bytes {
            if pos > 1008 {
                monoasm_arm64!(&mut self.jit, add x(base), x(base), #(pos););
                pos = 0;
            }
            self.jit.emitl(a64_enc::stp_q_off(v, v, base, pos as i32));
            pos += 32;
            done += 32;
        }
        if done + 16 <= bytes {
            self.jit.emitl(a64_enc::str_q_off(v, base, pos));
            pos += 16;
            done += 16;
        }
        if done + 8 <= bytes {
            monoasm_arm64!(&mut self.jit, str x(xs), [x(base), #(pos)];);
        }
    }
}

pub(crate) mod a64_enc {
    /// `dup Vd.2D, Xn`
    pub(crate) fn dup_2d(vd: u32, xn: u32) -> u32 {
        0x4E08_0C00 | (xn << 5) | vd
    }
    /// `str Qt, [Xn, #imm]` (unsigned offset, a multiple of 16 up to 65520)
    pub(crate) fn str_q_off(vt: u32, xn: u32, imm: u32) -> u32 {
        assert!(imm % 16 == 0 && imm / 16 < 4096);
        0x3D80_0000 | ((imm / 16) << 10) | (xn << 5) | vt
    }
    /// `str Qt, [Xn], #imm` (post-index, -256..=255)
    pub(crate) fn str_q_post(vt: u32, xn: u32, imm: i32) -> u32 {
        assert!((-256..256).contains(&imm));
        0x3C80_0400 | (((imm as u32) & 0x1ff) << 12) | (xn << 5) | vt
    }
    /// `stp Qt1, Qt2, [Xn, #imm]` (signed offset, a multiple of 16 in
    /// -1024..=1008)
    pub(crate) fn stp_q_off(vt1: u32, vt2: u32, xn: u32, imm: i32) -> u32 {
        assert!(imm % 16 == 0 && (-1024..=1008).contains(&imm));
        0xAD00_0000 | ((((imm / 16) as u32) & 0x7f) << 15) | (vt2 << 10) | (xn << 5) | vt1
    }
    /// `stp Qt1, Qt2, [Xn], #imm` (post-index)
    pub(crate) fn stp_q_post(vt1: u32, vt2: u32, xn: u32, imm: i32) -> u32 {
        assert!(imm % 16 == 0 && (-1024..=1008).contains(&imm));
        0xAC80_0000 | ((((imm / 16) as u32) & 0x7f) << 15) | (vt2 << 10) | (xn << 5) | vt1
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        /// The words `llvm-mc -triple=aarch64 -show-encoding` gives.
        #[test]
        fn neon_fill_encodings() {
            assert_eq!(dup_2d(0, 9), 0x4E08_0D20); // dup v0.2d, x9
            assert_eq!(dup_2d(0, 14), 0x4E08_0DC0); // dup v0.2d, x14
            assert_eq!(str_q_off(0, 10, 16), 0x3D80_0540); // str q0, [x10, #16]
            assert_eq!(str_q_post(0, 10, 16), 0x3C81_0540); // str q0, [x10], #16
            assert_eq!(stp_q_off(0, 0, 10, 32), 0xAD01_0140); // stp q0, q0, [x10, #32]
            assert_eq!(stp_q_off(0, 0, 22, -64), 0xAD3E_02C0); // stp q0, q0, [x22, #-64]
            assert_eq!(stp_q_post(0, 0, 10, 32), 0xAC81_0140); // stp q0, q0, [x10], #32
        }
    }
}
