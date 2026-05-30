//! x86-64 `Codegen` asm helpers: integer-compare helpers, fixnum
//! guards, unary/binary-op calls, epilogue, block-break / method-return,
//! and argument-forwarding. VM-tier (also used by the JIT).
//!
//! Counterpart of `arch/aarch64/codegen.rs`. Inherent `impl Codegen`
//! methods on the arch-neutral `Codegen` defined in `crate::codegen`.

use super::*;
use monoasm_macro::monoasm;
use paste::paste;

/// Integer comparison helpers (`icmp_eq` … `icmp_ge`): set `rax` to the Ruby
/// boolean `Value` for `rdi <cond> rsi`. These are VM-tier — emitted by the
/// bytecode handlers (`vmgen`'s `vm_*_opt_rr`) as well as the JIT — so they
/// live here rather than in `codegen::jitgen` (which is JIT-only and gets
/// `#[cfg]`-excluded from the aarch64 / no-jit VM build).
macro_rules! icmp_main {
    ($op:ident) => {
        paste! {
            pub(in crate::codegen) fn [<icmp_ $op>](&mut self) {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                };
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        icmp_main!($op1);
        icmp_main!($($op2),+);
    };
}

impl Codegen {
    icmp_main!(eq, ne, lt, le, gt, ge);

    pub(in crate::codegen) fn icmp_teq(&mut self) {
        self.icmp_eq()
    }

    /*///
    /// Compare(<=>) Fixnums.
    ///
    /// ### in
    /// - rdi: lhs (must be Fixnum)
    /// - rsi: rhs (must be Fixnum)
    ///
    /// ### out
    /// - rax: result(Value)
    ///
    /// ### destroy
    /// - rdx
    ///
    pub(in crate::codegen) fn icmp_cmp(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, (Value::from_ord(std::cmp::Ordering::Equal).id());
            movq rdx, (Value::from_ord(std::cmp::Ordering::Greater).id());
            cmpq rdi, rsi;
            cmovgtq rax, rdx;
            movq rdx, (Value::from_ord(std::cmp::Ordering::Less).id());
            cmovltq rax, rdx;
        };
    }*/

    ///
    /// check whether lhs and rhs are fixnum.
    ///
    pub(in crate::codegen) fn guard_rdi_rsi_fixnum(&mut self, generic: &DestLabel) {
        self.guard_rdi_fixnum(generic);
        self.guard_rsi_fixnum(generic);
    }

    ///
    /// check whether lhs is fixnum.
    ///
    pub(in crate::codegen) fn guard_rdi_fixnum(&mut self, generic: &DestLabel) {
        monoasm!( &mut self.jit,
            testq rdi, 0x1;
            jz generic;
        );
    }

    ///
    /// check whether rhs is fixnum.
    ///
    pub(in crate::codegen) fn guard_rsi_fixnum(&mut self, generic: &DestLabel) {
        monoasm!( &mut self.jit,
            testq rsi, 0x1;
            jz generic;
        );
    }

    ///
    /// Call unary operator function.
    ///
    /// ### in
    /// - rdi: receiver
    ///
    /// ### out
    /// - rax: result
    ///
    pub(in crate::codegen) fn call_unop(&mut self, func: UnaryOpFn) {
        monoasm!( &mut self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func as usize);
            call rax;
        );
    }

    pub(in crate::codegen) fn call_binop(&mut self, func: BinaryOpFn) {
        monoasm!( &mut self.jit,
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
    }

    pub(in crate::codegen) fn epilogue(&mut self) {
        monoasm!( &mut self.jit,
            leave;
            ret;
        );
    }

    ///
    /// Gen code for break in block.
    ///
    /// rbp <- bp for a context of the outer of the block.
    ///
    pub(in crate::codegen) fn block_break(&mut self) {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rax;
            movq rax, (runtime::err_block_break);
            call rax;
            jmp  raise;
        }
    }

    ///
    /// Gen code for return in block.
    ///
    /// #### in
    /// - rax: return value
    /// - r13: pc + 1
    ///
    pub(in crate::codegen) fn method_return(&mut self) {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rax;
            movq rax, (runtime::err_method_return);
            call rax;
            jmp  raise;
        }
    }

    pub(in crate::codegen) fn method_return_specialized(&mut self, rbp_offset: usize) {
        monoasm! { &mut self.jit,
            lea  rbp, [rbp + (rbp_offset)];
            leave;
            ret;
        }
    }

    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*dst*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    pub(in crate::codegen) fn integer_val_to_f64(&mut self, reg: GP, dst: u64) {
        monoasm!(&mut self.jit,
            sarq R(reg as _), 1;
            cvtsi2sdq xmm(dst), R(reg as _);
        );
    }

    ///
    /// ### in
    /// - r15: &FuncData
    /// - r8: CallsiteId
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(in crate::codegen) fn generic_handle_arguments(
        &mut self,
        f: extern "C" fn(&mut Executor, &mut Globals, Lfp, Lfp, CallSiteId) -> Option<Value>,
    ) {
        monoasm! { &mut self.jit,
            // rcx <- callee LFP
            lea  rcx, [rsp - (RSP_LOCAL_FRAME)];
        }
        self.push_stack_offset();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            // rdi: &mut Executor
            // rsi: &mut Globals
            // rdx: caller LFP
            // rcx: callee LFP
            // r8: CallsiteId
            movq rax, (f);
            movq rdx, r14;
            call rax;
        }
        self.pop_stack_offset();
    }
}
