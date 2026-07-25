use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Inline fixnum binary op. Fixnums are tagged `2n+1`; signed 64-bit
    /// overflow of the tagged arithmetic == fixnum overflow, so we branch to
    /// `deopt` on the V flag. Result is left in `lhs`'s register, mirroring x86
    /// `integer_binop`. Mul/Div/etc. not yet ported (bail).
    pub(super) fn a64_integer_binop(&mut self, lhs: GP, rhs: GP, kind: BinOpK, deopt: &DestLabel) -> bool {
        let l = lhs.a64().0;
        let r = rhs.a64().0;
        match kind {
            BinOpK::Add => {
                monoasm_arm64!(&mut self.jit,
                    sub x(l), x(l), #(1u32);
                    adds x(l), x(l), x(r);
                );
                self.jit.bcond_label(monoasm::Cond::Vs, deopt);
            }
            BinOpK::Sub => {
                monoasm_arm64!(&mut self.jit, subs x(l), x(l), x(r););
                self.jit.bcond_label(monoasm::Cond::Vs, deopt);
                monoasm_arm64!(&mut self.jit, add x(l), x(l), #(1u32););
            }
            // Mul: compute `2a * b` (matching x86's `imul` on the half-untagged
            // lhs). aarch64 has no `smulh`, so detect overflow with a checking
            // `sdiv`: if `2a != 0` and `(2a*b)/(2a) != b` the product wrapped.
            // Mul: compute `2a * b` (matching x86's `imul` on the half-untagged
            // lhs), then detect 64-bit signed overflow the standard way — the
            // high half (`smulh`) must equal the sign-extension of the low half
            // (`low >> 63`); a mismatch means the product wrapped.
            BinOpK::Mul => {
                monoasm_arm64!(&mut self.jit,
                    asr x(r), x(r), #(1u32);   // b (untagged)
                    sub x(l), x(l), #(1u32);   // 2a
                );
                monoasm_arm64!(&mut self.jit, mul x9, x(l), x(r);); // low 64
                monoasm_arm64!(&mut self.jit, smulh x10, x(l), x(r););
                monoasm_arm64!(&mut self.jit,
                    asr x11, x9, #(63u32);         // sign-extension of low
                    cmp x10, x11;                  // high == sign(low)?
                );
                self.jit.bcond_label(monoasm::Cond::Ne, deopt);
                monoasm_arm64!(&mut self.jit, add x(l), x9, #(1u32););
            }
            // Div: Ruby integer division floors toward negative infinity, but
            // `sdiv` truncates toward zero, so adjust the quotient down by 1
            // when the remainder is non-zero and its sign differs from the
            // divisor's. Both operands are already in registers (the front-end
            // materializes RI/IR immediates), and the result goes to rax (x0),
            // matching x86 `integer_binop`'s Div. b==0 deopts (ZeroDivisionError).
            BinOpK::Div => {
                let rax = GP::Rax.a64().0;
                let done = self.jit.label();
                let deopt = deopt.clone();
                monoasm_arm64!(&mut self.jit,
                    asr x(r), x(r), #(1u32);     // b (untagged)
                    cbz x(r), deopt;             // b==0 -> ZeroDivisionError (deopt)
                    asr x9, x(l), #(1u32);       // a (untagged)
                    sdiv x10, x9, x(r);          // q = trunc(a/b)
                    msub x11, x10, x(r), x9;     // rem = a - q*b
                    cbz x11, done;               // exact -> no floor adjust
                    eor x12, x11, x(r);          // rem ^ b
                    tbz x12, #(63), done;        // same sign -> no adjust
                    sub x10, x10, #(1u32);       // floor: q -= 1
                    done:
                    lsl x(rax), x10, #(1u32);    // 2q
                    add x(rax), x(rax), #(1u32); // 2q+1 (tagged)
                );
            }
            // Bitwise ops on tagged fixnums need no overflow check and never
            // clobber `rhs`; the result lands in `lhs` like Add/Sub. `&`/`|` keep
            // the LSB tag; `^` clears it, so re-tag with `+1`.
            BinOpK::BitOr => {
                monoasm_arm64!(&mut self.jit, orr x(l), x(l), x(r););
            }
            BinOpK::BitAnd => {
                monoasm_arm64!(&mut self.jit, and x(l), x(l), x(r););
            }
            BinOpK::BitXor => {
                monoasm_arm64!(&mut self.jit,
                    eor x(l), x(l), x(r);
                    add x(l), x(l), #(1u32);
                );
            }
            // Rem/Exp/Shl/Shr are compiled as method calls, never IntegerBinOp
            // (mirrors x86 `integer_binop`'s `_ => unreachable!()`).
            _ => unreachable!(),
        }
        true
    }

    /// Inlined `Integer#>>` / `Integer#<<` by a constant shift amount (the
    /// "shift right by `imm`" primitive). Operand is the tagged fixnum `2n+1`
    /// in Rdi (x4). aarch64 twin of x86 `gen_shr_imm`.
    pub(crate) fn gen_shr_imm(&mut self, imm: u8) {
        let rdi = GP::Rdi.a64().0; // x4
        if imm >= 64 {
            // Shift-out: -1 (all bits) collapses to -1, everything else to 0.
            let zero = self.jit.label();
            let exit = self.jit.label();
            let neg1 = Value::i32(-1).id();
            let z = Value::i32(0).id();
            monoasm_arm64!(&mut self.jit,
                tbz x(rdi), #(63), zero;   // non-negative -> 0
                mov x(rdi), (neg1);
                b exit;
                zero:
                mov x(rdi), (z);
                exit:
            );
        } else {
            // `((2n+1) >>a imm) | 1` == `2*(n>>imm) + 1`. monoasm has no
            // `orr`-immediate, so set the tag bit via a scratch register.
            monoasm_arm64!(&mut self.jit,
                asr x(rdi), x(rdi), #(imm as u32);
                mov x9, #(1);
                orr x(rdi), x(rdi), x9;
            );
        }
    }

    /// Inlined `Integer#<<` by a constant shift amount, with a fixnum-overflow
    /// guard that deopts. Operand `2n+1` in Rdi (x4). aarch64 twin of x86
    /// `gen_shl_rhs_imm`. x86 uses `lzcnt` for the overflow test; monoasm has
    /// no `clz`, so detect overflow by shifting back: a fixnum `n<<rhs` fits
    /// i63 iff the tagged `2n<<rhs` fits i64, i.e. `(2n<<rhs) >>a rhs == 2n`.
    pub(crate) fn gen_shl_rhs_imm(&mut self, rhs: u8, deopt: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            sub x(rdi), x(rdi), #(1);          // 2n (strip tag)
            lsl x9, x(rdi), #(rhs as u32);     // 2n << rhs
            asr x10, x9, #(rhs as u32);        // shift back (signed)
            cmp x(rdi), x10;                   // lost significant bits?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, deopt); // overflow -> deopt
        monoasm_arm64!(&mut self.jit,
            add x(rdi), x9, #(1);              // 2(n<<rhs) is even, so +1 sets the tag
        );
    }

    /// `Integer#>>` by a variable amount. lhs (tagged) in Rdi (x4), shift amount
    /// (tagged) in Rcx (x1); result tagged in Rdi. A negative shift means a left
    /// shift, which overflows -> deopt. aarch64 twin of x86 `gen_shr`, but
    /// without `lzcnt`/`select_page`: overflow is checked by shifting back, and
    /// the cold (left-shift / >=64) blocks are laid out inline.
    pub(crate) fn gen_shr(&mut self, deopt: &DestLabel) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        let cont = self.jit.label();
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x1, x1, #1;            // untag shift amount (Rcx == x1)
            cmp x1, #0;
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &shl); // negative -> left shift
        monoasm_arm64!(&mut self.jit, cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &under);
        monoasm_arm64!(&mut self.jit, asr x4, x4, x1;); // tagged sar (Rdi == x4)
        self.jit.bind_label(after.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, #1;
            orr x4, x4, x9;           // re-tag fixnum
            b cont;
        );
        // left shift by -k (cold)
        self.jit.bind_label(shl);
        monoasm_arm64!(&mut self.jit, neg x1, x1; cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &deopt); // left >=64 -> overflow
        monoasm_arm64!(&mut self.jit,
            sub x4, x4, #1;           // strip tag -> 2n
            lsl x9, x4, x1;           // 2n << k
            asr x10, x9, x1;          // shift back
            cmp x10, x4;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // overflow
        monoasm_arm64!(&mut self.jit, mov x4, x9; b after;);
        // right shift by >= 64 (cold): 0 if lhs >= 0, else -1
        self.jit.bind_label(under);
        self.a64_shift_under(&after);
        self.jit.bind_label(cont);
    }

    /// `Integer#<<` by a variable amount. lhs (tagged) in Rdi (x4), shift amount
    /// (tagged) in Rcx (x1); result tagged in Rdi. A left shift that overflows
    /// the fixnum range deopts; a negative shift means a right shift. aarch64
    /// twin of x86 `gen_shl` (shift-back overflow, inline cold blocks). Used for
    /// both the literal- and register-lhs cases (recv is always loaded into Rdi
    /// by the builtin), so x86's `gen_shl_lhs_imm` has no aarch64 counterpart.
    pub(crate) fn gen_shl(&mut self, deopt: &DestLabel) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        let cont = self.jit.label();
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x1, x1, #1;            // untag shift amount
            cmp x1, #0;
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &shr); // negative -> right shift
        monoasm_arm64!(&mut self.jit, cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &deopt); // left >=64 -> overflow
        monoasm_arm64!(&mut self.jit,
            sub x4, x4, #1;           // strip tag -> 2n
            lsl x9, x4, x1;           // 2n << k
            asr x10, x9, x1;          // shift back
            cmp x10, x4;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt); // overflow
        monoasm_arm64!(&mut self.jit, mov x4, x9;);
        self.jit.bind_label(after.clone());
        monoasm_arm64!(&mut self.jit,
            mov x9, #1;
            orr x4, x4, x9;           // re-tag fixnum
            b cont;
        );
        // right shift by -k (cold)
        self.jit.bind_label(shr);
        monoasm_arm64!(&mut self.jit, neg x1, x1; cmp x1, #64;);
        self.jit.bcond_label(monoasm::Cond::Ge, &under);
        monoasm_arm64!(&mut self.jit, asr x4, x4, x1; b after;);
        // right shift by >= 64 (cold): 0 if lhs >= 0, else -1
        self.jit.bind_label(under);
        self.a64_shift_under(&after);
        self.jit.bind_label(cont);
    }

    /// Shared cold tail for a shift-right by >= 64 bits: the tagged lhs is in
    /// Rdi (x4); leave 0 (Value 0) for a non-negative lhs or -1 (Value -1) for a
    /// negative one, then branch to `after` (which re-tags). Mirrors x86
    /// `shift_under`.
    pub(super) fn a64_shift_under(&mut self, after: &DestLabel) {
        let zero = self.jit.label();
        monoasm_arm64!(&mut self.jit, cmp x4, #0;);
        self.jit.bcond_label(monoasm::Cond::Ge, &zero);
        monoasm_arm64!(&mut self.jit,
            mov x4, #0;
            sub x4, x4, #1;           // -1 (sar of a negative number by >=64)
            b after;
        );
        self.jit.bind_label(zero);
        monoasm_arm64!(&mut self.jit,
            mov x4, #0;
            b after;
        );
    }

    /// Inlined `Integer#%` (general fixnum case). `a` (Rdi/x4) and `b` (Rsi/x3)
    /// are tagged; the floor-mod remainder is returned tagged in Rax (x0). b==0
    /// deopts with a `_divide_by_zero` marker. aarch64 twin of x86
    /// `gen_int_rem`; mirrors the floor adjustment of `a64_integer_binop`'s Div
    /// (`sdiv` truncates toward zero, so when the remainder is non-zero and its
    /// sign differs from the divisor's, add the divisor back).
    pub(crate) fn gen_int_rem(&mut self, deopt: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4 (a, tagged)
        let rsi = GP::Rsi.a64().0; // x3 (b, tagged)
        let rax = GP::Rax.a64().0; // x0 (result)
        let zero_div = self.jit.label();
        let exit = self.jit.label();
        let done = self.jit.label();
        let deopt = deopt.clone();
        let sym = Value::symbol_from_str("_divide_by_zero").id();
        monoasm_arm64!(&mut self.jit,
            asr x(rsi), x(rsi), #(1);        // b untagged
            cbz x(rsi), zero_div;            // b==0 -> ZeroDivisionError
            asr x9, x(rdi), #(1);            // a untagged
            sdiv x10, x9, x(rsi);            // q = trunc(a/b)
            msub x11, x10, x(rsi), x9;       // rem = a - q*b
            cbz x11, exit;                   // exact -> no floor adjust
            eor x12, x11, x(rsi);            // sign(rem) vs sign(b)
            tbz x12, #(63), exit;            // same sign -> no adjust
            add x11, x11, x(rsi);            // floor-mod: rem += b
            exit:
            lsl x(rax), x11, #(1);           // re-tag the remainder
            add x(rax), x(rax), #(1);
            b done;
            zero_div:
            mov x(rdi), (sym);               // deopt marker reg (mirrors x86)
            b deopt;
            done:
        );
    }

    /// Inlined `Integer#**` between two fixnums: untag and call the runtime
    /// `pow_ii(a, b, vm)` (which returns the boxed result, possibly a BigInt,
    /// or 0/None on error). Result Value lands in Rax (x0). aarch64 twin of x86
    /// `gen_int_pow`. The FP pool is saved around the C-call.
    pub(crate) fn gen_int_pow(&mut self, using_fpr: UsingFpr, error: &DestLabel) {
        let rdi = GP::Rdi.a64().0; // x4 (a, tagged)
        let rsi = GP::Rsi.a64().0; // x3 (b, tagged)
        let f = crate::executor::op::pow_ii as *const () as u64;
        let error = error.clone();
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            asr x0, x(rdi), #(1);    // a (untagged) -> arg0
            asr x1, x(rsi), #(1);    // b (untagged) -> arg1
            mov x2, x19;             // vm (EXEC) -> arg2
            str x30, [sp, #-16]!;    // save LR
            mov x9, (f);
            blr x9;                  // x0 = pow_ii(a, b, vm)
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            cbz x0, error;           // 0/None -> raise
        );
    }

    /// `Integer#%` with a Float rhs: `rem_ff(lhs, rhs)` (f64,f64 -> f64). The
    /// operands are loaded into d0/d1, the result (d0) stored into `dst_fpr`.
    /// aarch64 twin of x86 `gen_int_rem_if`.
    pub(crate) fn gen_int_rem_if(
        &mut self,
        lhs_fpr: FPReg,
        rhs_fpr: FPReg,
        dst_fpr: FPReg,
        using_fpr: UsingFpr,
        base: usize,
    ) {
        let f = crate::executor::op::rem_ff as *const () as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_fpr_save(using_fpr, false);
        self.a64_fpr_into_d(lhs_fpr, 0, base);
        self.a64_fpr_into_d(rhs_fpr, 1, base);
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;                  // d0 = rem_ff(d0, d1)
        );
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
        self.a64_d0_into_fpr(dst_fpr, base);
    }

    /// `Integer#**` with a Float rhs: `pow_ff(lhs, rhs)` (f64,f64 -> Value, which
    /// may be Complex). The operands are loaded into d0/d1; the result Value
    /// lands in Rax (x0). aarch64 twin of x86 `gen_int_pow_if`.
    pub(crate) fn gen_int_pow_if(
        &mut self,
        lhs_fpr: FPReg,
        rhs_fpr: FPReg,
        using_fpr: UsingFpr,
        base: usize,
    ) {
        let f = crate::executor::op::pow_ff as *const () as u64;
        monoasm_arm64!(&mut self.jit, str x30, [sp, #-16]!;);
        self.emit_fpr_save(using_fpr, false);
        self.a64_fpr_into_d(lhs_fpr, 0, base);
        self.a64_fpr_into_d(rhs_fpr, 1, base);
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;                  // x0 = pow_ff(d0, d1)
        );
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit, ldr x30, [sp], #16;);
    }

    /// Compare two tagged fixnums (the tag preserves order). Mirrors x86
    /// `cmp_integer`.
    pub(super) fn a64_cmp_integer(&mut self, lhs: GP, rhs: GP) {
        let l = lhs.a64().0;
        let r = rhs.a64().0;
        monoasm_arm64!(&mut self.jit, cmp x(l), x(r););
    }

    /// After `a64_cmp_integer`, materialize a Ruby boolean in rax (x0):
    /// `FALSE_VALUE | (cond << 3)` (== 0x14 or 0x1c). Mirrors the VM's
    /// `a64_op_cmp` and x86 `flag_to_bool`.
    pub(super) fn a64_flag_to_bool(&mut self, kind: CmpKind) {
        let cond = a64_cond_for_cmp(kind, BrKind::BrIf);
        let rax = GP::Rax.a64();
        self.jit.cset(rax, cond);
        monoasm_arm64!(&mut self.jit,
            lsl x(rax.0), x(rax.0), #(3u32);
            mov x9, (FALSE_VALUE);
            orr x(rax.0), x(rax.0), x9;
        );
    }

    /// Integer comparison; result Value lands in the accumulator.
    pub(in crate::codegen::jitgen) fn emit_integer_cmp(
        &mut self,
        kind: CmpKind,
        lhs: GP,
        rhs: GP,
    ) -> bool {
        self.a64_cmp_integer(lhs, rhs);
        self.a64_flag_to_bool(kind);
        true
    }

    /// Generic binary-op C-call (no receiver-class guard), mirroring the VM's
    /// call_binop convention: x0=vm, x1=globals, x2=lhs, x3=rhs; Option<Value>
    /// result in x0. Bails on a live xmm pool reg or an out-of-range offset.
    pub(in crate::codegen::jitgen) fn emit_generic_binop(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        func: crate::executor::BinaryOpFn,
        is_func_call: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = func as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals
        );
        self.a64_frame_load(2, lfp, off_l); // x2 = lhs
        self.a64_frame_load(3, lfp, off_r); // x3 = rhs
        monoasm_arm64!(&mut self.jit,
            mov x4, (is_func_call as u64); // is_func_call
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// `==` / `!=` with an inline immediate fast path (mirrors the x86
    /// `opt_eq_cmp`). If BOTH operands are non-heap, non-flonum immediates the
    /// Ruby result is exact bit (identity) equality, produced inline via
    /// cmp + cset; otherwise fall through to the generic C-call `func`
    /// (x0=vm, x1=globals, x2=lhs, x3=rhs). `lhs`/`rhs` are loaded into x2/x3
    /// up front so the slow path can reuse them. The live xmm pool is
    /// saved/restored *only* around the slow-path C call (the inline fast path
    /// never touches the caller-saved d2.. pool regs); bails only on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_opt_eq_cmp(
        &mut self,
        lhs: SlotId,
        rhs: SlotId,
        kind: CmpKind,
        func: crate::executor::BinaryOpFn,
        is_func_call: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off_l = lhs.0 as u32 * 8 + LFP_SELF as u32;
        let off_r = rhs.0 as u32 * 8 + LFP_SELF as u32;
        let f = func as u64;
        let slow = self.jit.label();
        let done = self.jit.label();
        // Load operands into the C-arg registers (reused by the slow path).
        // Heap iff (bits & 0b111) == 0; Flonum iff (bits & 0b011) == 0b010.
        // Either operand heap/flonum -> generic C-call.
        self.a64_frame_load(2, lfp, off_l); // lhs
        self.a64_frame_load(3, lfp, off_r); // rhs
        monoasm_arm64!(&mut self.jit,
            mov x14, (7u64);
            and x9, x2, x14;
            cbz x9, slow;                // lhs heap -> slow
            mov x14, (3u64);
            and x9, x2, x14;
            cmp x9, #(2u32);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &slow); // lhs flonum -> slow
        monoasm_arm64!(&mut self.jit,
            mov x14, (7u64);
            and x9, x3, x14;
            cbz x9, slow;                // rhs heap -> slow
            mov x14, (3u64);
            and x9, x3, x14;
            cmp x9, #(2u32);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &slow); // rhs flonum -> slow
        // Fast path: both identity-comparable immediates -> bit equality.
        monoasm_arm64!(&mut self.jit,
            cmp x2, x3;
        );
        self.a64_flag_to_bool(kind); // x0 = bool Value
        monoasm_arm64!(&mut self.jit,
            b done;
        slow:
            mov x0, x19;                 // vm
            mov x1, x20;                 // globals (x2=lhs, x3=rhs intact)
            mov x4, (is_func_call as u64); // is_func_call
        );
        // Save the live FP pool only on the slow path: the C call clobbers the
        // caller-saved d2.. pool regs, but the inline fast path above (which
        // branches straight to `done`) leaves them untouched, so both paths
        // reach `done` with sp and the pool registers consistent.
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
        done:
        );
        true
    }

    /// Load a `FPReg` (pool reg or spill slot) into `d0`.
    pub(super) fn a64_fpr_into_d0(&mut self, src: FPReg, base: usize) {
        self.a64_fpr_into_d(src, 0, base);
    }

    /// Load a `FPReg` (pool reg or spill slot) into the scratch register `dreg`
    /// (D0/D1, outside the D2-D15 pool). Spill-aware, so it never bails.
    pub(super) fn a64_fpr_into_d(&mut self, src: FPReg, dreg: u32, base: usize) {
        match PhysMap::new(base).resolve(src) {
            FPRegLoc::Xmm(p) => monoasm_arm64!(&mut self.jit, fmov d(dreg), d(p as u32);),
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;        // [x29 - off] (mirrors x86 [rbp - off])
                ldr d(dreg), [x10];
            ),
        }
    }

    /// Store `d0` into a `FPReg` (pool reg or spill slot).
    pub(super) fn a64_d0_into_fpr(&mut self, dst: FPReg, base: usize) {
        match PhysMap::new(base).resolve(dst) {
            FPRegLoc::Xmm(p) => monoasm_arm64!(&mut self.jit, fmov d(p as u32), d0;),
            FPRegLoc::Spill(off) => monoasm_arm64!(&mut self.jit,
                mov x10, (off as i64 as u64);
                sub x10, x29, x10;
                str d0, [x10];
            ),
        }
    }
}
