//! x86-64 machine-code emitters for builtin-method JIT inliners.
//!
//! These are the assembly bodies of the `#[monoruby_builtin]` inline
//! generators (`builtins/*.rs`). The generators themselves are arch-neutral
//! (they build `AsmIr` and call one of these `emit_*` methods from inside
//! `ir.inline`); the aarch64 counterparts live in `arch/aarch64/compile.rs`
//! with the same method names.

use super::*;

impl Codegen {
    /// `BasicObject#object_id`: `i64_to_value(self_id)`; self id in rdi → rax.
    pub(crate) fn emit_object_id(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, (crate::executor::op::i64_to_value);
            call rax;
        }
    }

    /// `Kernel#block_given?`: the block slot at [r14 - LFP_BLOCK] is 0 or NIL
    /// when no block was passed. Result Value in rax.
    pub(crate) fn emit_block_given(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, (FALSE_VALUE);
            movq rdi, [r14 - (LFP_BLOCK)];
            testq rdi, rdi;
            jz exit;
            cmpq rdi, (NIL_VALUE);
            jeq exit;
            movq rax, (TRUE_VALUE);
        exit:
        }
    }

    /// `String#getbyte`: receiver String in rdi, fixnum index in rsi →
    /// rax = byte tagged as a fixnum, or nil when the (negative-adjusted)
    /// index is out of range.
    pub(crate) fn emit_string_getbyte(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            sarq rsi, 1;
            // rax = len, rcx = data ptr (inline vs heap storage select)
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            lea  rcx, [rdi + (RVALUE_OFFSET_INLINE)];
            cmpq rax, (STRING_INLINE_CAP);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmovgtq rcx, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            // negative index counts back from the end
            movq rdx, rsi;
            addq rdx, rax;
            testq rsi, rsi;
            cmovsq rsi, rdx;
            // unsigned bound check covers a still-negative index too
            cmpq rsi, rax;
            movq rax, (NIL_VALUE);
            jae  exit;
            movzxb rax, [rcx + rsi];
            salq rax, 1;
            orq  rax, 1;
        exit:
        }
    }

    /// `String#setbyte`: receiver String in rdi, fixnum index in rsi, fixnum
    /// byte value in rdx. Deopts when the receiver is frozen or chilled
    /// (interpreter raises / warns) or the index is out of range
    /// (interpreter raises IndexError). Keeps the cached code-range
    /// classification consistent with `RStringInner::set_byte`.
    ///
    /// ### destroy
    /// - rax, rcx, rdx, rsi, r8
    pub(crate) fn emit_string_setbyte(&mut self, deopt: &DestLabel) {
        let exit = self.jit.label();
        let set_unknown = self.jit.label();
        monoasm! { &mut self.jit,
            // frozen (0b010) or chilled (0b100) → deopt
            movzxw rax, [rdi + (RVALUE_OFFSET_FLAG)];
            testq rax, (0b110);
            jne  deopt;
            sarq rsi, 1;
            sarq rdx, 1;
            // rax = len, rcx = data ptr (inline vs heap storage select)
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            // Shared (copy-on-write) string: the buffer is aliased by
            // other sharers and must not be written in place — deopt to
            // the interpreter, which detaches via `owned_mut`.
            movq r8, (crate::rvalue::STRING_SHARED_TAG);
            cmpq rax, r8;
            jeq  deopt;
            lea  rcx, [rdi + (RVALUE_OFFSET_INLINE)];
            cmpq rax, (STRING_INLINE_CAP);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmovgtq rcx, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            // negative index counts back from the end
            movq r8, rsi;
            addq r8, rax;
            testq rsi, rsi;
            cmovsq rsi, r8;
            // out of range (unsigned check covers still-negative) → IndexError
            cmpq rsi, rax;
            jae  deopt;
            movb [rcx + rsi], rdx;
            // code range cache: poking an ASCII byte into a SevenBit string
            // keeps SevenBit; anything else degrades to Unknown.
            cmpb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::SevenBit as u64);
            jne  set_unknown;
            testq rdx, (0x80);
            jeq  exit;
        set_unknown:
            movb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::Unknown as u64);
        exit:
        }
    }

    /// `Integer#succ` / `#next`: fixnum in rdi; tagged `+1` is `+2` on the
    /// raw bits. Deopts on i63 overflow (interpreter returns a Bignum).
    pub(crate) fn emit_integer_succ(&mut self, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            addq rdi, 2;
            jo   deopt;
        }
    }

    /// `Hash#[]`: `hashindex(vm, globals, recv, key)`. recv in rdx, key in rcx.
    /// Result Value in rax (errors via the trailing HandleError).
    pub(crate) fn emit_hash_index(&mut self, hashindex: u64) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (hashindex);
            call rax;
        }
    }

    /// `Array#clone`: `array_clone_extern(recv)`. recv in rdi → rax.
    pub(crate) fn emit_array_clone(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rax, (f);
            call rax;
        }
    }

    /// `Array#dup`: `array_dup_extern(recv, globals)`. recv in rdi → rax.
    pub(crate) fn emit_array_dup(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rsi, r12; // globals
            movq rax, (f);
            call rax;
        }
    }

    /// `Array#<<`: `ary_shl(recv, arg)`. recv in rdi, arg in rsi → rax.
    pub(crate) fn emit_array_shl(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rax, (f);
            call rax;
        }
    }

    /// `Class#allocate`: `alloc_func(class_id, globals)` → rax.
    pub(crate) fn emit_class_allocate(&mut self, class_id: u32, alloc_func: u64) {
        monoasm! { &mut self.jit,
            movl rdi, (class_id);
            movq rsi, r12;
            movq rax, (alloc_func);
            call rax;
        }
    }


    /// `Float#to_i`: truncate `fsrc` to i64, tag as fixnum in rdi, deopt on
    /// out-of-fixnum overflow.
    pub(crate) fn emit_float_to_int(&mut self, fsrc: FPReg, deopt: &DestLabel, base: usize) {
        self.load_fpr_into_xmm0(fsrc, base);
        monoasm! { &mut self.jit,
            cvttsd2siq rdi, xmm0;
            addq  rdi, rdi;
            jo    deopt;
            orq   rdi, 1;
        }
    }

    /// `Math.sqrt`: `sqrtsd` on `fsrc`; NaN passes through, a negative argument
    /// deopts (the interpreter re-runs and raises DomainError).
    pub(crate) fn emit_math_sqrt(
        &mut self,
        fsrc: FPReg,
        fret: Option<FPReg>,
        deopt: &DestLabel,
        base: usize,
    ) {
        let do_sqrt = self.jit.label();
        // ucomisd sets PF=1 for NaN and CF=1 for val < 0.
        self.load_fpr_into_xmm0(fsrc, base);
        monoasm!( &mut self.jit,
            xorpd xmm1, xmm1;
            ucomisd xmm0, xmm1;
            jp do_sqrt;
            jb deopt;
        do_sqrt:
        );
        if let Some(fret) = fret {
            monoasm!( &mut self.jit,
                sqrtsd xmm0, xmm0;
            );
            self.store_fpr_into_xmm(fret, base);
        }
    }

    /// `Fiber.yield` with no args: the yielded value (rsi) is nil.
    pub(crate) fn emit_fiber_yield_value_nil(&mut self) {
        monoasm! { &mut self.jit,
            movq rsi, (Value::nil().id());
        }
    }

    /// `Fiber.yield(*args)` with ≥2 args: build the args array, leaving it in
    /// rsi. `args_off` is `conv(args)`.
    pub(crate) fn emit_fiber_yield_value_array(&mut self, args_off: usize, pos_num: usize) {
        monoasm! { &mut self.jit,
            lea rdi, [r14 - (args_off as i32)];
            movq rsi, (pos_num);
            movq rax, (crate::runtime::create_array);
            call rax;
            movq rsi, rax;
        }
    }

    /// `Fiber.yield`: call `yield_fiber(vm, value)` (value already in rsi).
    pub(crate) fn emit_fiber_yield_call(&mut self, yield_fiber: u64) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (yield_fiber);
            call rax;
        }
    }

    /// Load a 64-bit tagged fixnum literal into rsi (the RHS register) for an
    /// `Integer` bit-op whose immediate doesn't fit a 32-bit encoding.
    pub(crate) fn emit_load_tagged_rsi(&mut self, tagged: i64) {
        monoasm!( &mut self.jit, movq rsi, (tagged); );
    }

    /// `Integer#|` with a tagged immediate (`(2a+1)|(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitor_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, orq rdi, (imm); );
    }
    /// `Integer#|` register-register.
    pub(crate) fn emit_bitor_rr(&mut self) {
        monoasm!( &mut self.jit, orq rdi, rsi; );
    }
    /// `Integer#&` with a tagged immediate (`(2a+1)&(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitand_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, andq rdi, (imm); );
    }
    /// `Integer#&` register-register.
    pub(crate) fn emit_bitand_rr(&mut self) {
        monoasm!( &mut self.jit, andq rdi, rsi; );
    }
    /// `Integer#^` with a tagged immediate (use `imm-1` so lhs's tag survives).
    pub(crate) fn emit_bitxor_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, xorq rdi, (imm - 1); );
    }
    /// `Integer#^` register-register (`(2a+1)^(2b+1)` clears LSB, re-tag +1).
    pub(crate) fn emit_bitxor_rr(&mut self) {
        monoasm!( &mut self.jit,
            xorq rdi, rsi;
            addq rdi, 1;
        );
    }

    /// `n << k` / `n >> -k` with `k >= 64`: a non-zero `n` overflows (deopt);
    /// `0` shifts to `0`. lhs in rdi.
    pub(crate) fn emit_shl_overflow_zero(&mut self, z: i64, deopt: &DestLabel) {
        monoasm!( &mut self.jit,
            cmpq rdi, (z);
            jne deopt;
            movq rdi, (z);
        );
    }

    /// `Fiddle.___read` integer load: untag the pointer in rdi, deopt on NULL,
    /// load a `width`-byte value (sign/zero-extended per `signed`), tag the
    /// result as a fixnum in rax.
    pub(crate) fn emit_fiddle_read_int(&mut self, width: u8, signed: bool, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
        }
        match (width, signed) {
            (1, true) => monoasm! { &mut self.jit, movsxb rax, [rdi]; },
            (1, false) => monoasm! { &mut self.jit, movzxb rax, [rdi]; },
            (2, true) => monoasm! { &mut self.jit, movsxw rax, [rdi]; },
            (2, false) => monoasm! { &mut self.jit, movzxw rax, [rdi]; },
            (4, true) => monoasm! { &mut self.jit, movsxl rax, [rdi]; },
            (4, false) => monoasm! { &mut self.jit, movl rax, [rdi]; },
            _ => unreachable!(),
        }
        // Tag as Fixnum: rax = (rax << 1) | 1.
        monoasm! { &mut self.jit,
            addq rax, rax;
            orq rax, 1;
        }
    }

    /// `Fiddle.___read` f64 load: untag the pointer in rdi, deopt on NULL, load
    /// the double into `fret`.
    pub(crate) fn emit_fiddle_read_f64(&mut self, fret: FPReg, deopt: &DestLabel, base: usize) {
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            movq xmm0, [rdi];
        }
        self.store_fpr_into_xmm(fret, base);
    }

    /// `Fiddle.___write` integer store: save the tagged pointer (the return
    /// value) in rax, untag the pointer in rdi, deopt on NULL, untag the value
    /// in rsi and store its low `width` bytes.
    pub(crate) fn emit_fiddle_write_int(&mut self, width: u8, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            sarq rsi, 1;
        }
        match width {
            1 => monoasm! { &mut self.jit, movb [rdi], rsi; },
            2 => monoasm! { &mut self.jit, movw [rdi], rsi; },
            4 => monoasm! { &mut self.jit, movl [rdi], rsi; },
            _ => unreachable!(),
        }
    }

    /// `Fiddle.___write` f64 store: load the source double into xmm0, save the
    /// tagged pointer in rax, untag the pointer in rdi, deopt on NULL, store the
    /// double.
    pub(crate) fn emit_fiddle_write_f64(&mut self, xsrc: FPReg, deopt: &DestLabel, base: usize) {
        self.load_fpr_into_xmm0(xsrc, base);
        monoasm! { &mut self.jit,
            movq rax, rdi;
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            movq [rdi], xmm0;
        }
    }

    /// `Integer#%` by a positive power of two: `lhs & mask` on the tagged
    /// fixnum in rdi.
    pub(crate) fn emit_int_rem_pow2_mask(&mut self, mask: i64) {
        if let Ok(imm32) = i32::try_from(mask) {
            let imm = imm32 as i64;
            monoasm!( &mut self.jit, andq rdi, (imm); );
        } else {
            monoasm!( &mut self.jit, movq rax, (mask); andq rdi, rax; );
        }
    }
}
