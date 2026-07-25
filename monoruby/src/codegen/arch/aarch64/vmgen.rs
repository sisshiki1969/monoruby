//! aarch64 VM construction (`construct_vm`), every bytecode opcode
//! handler (`a64_op_*`), and BOP de-optimization.
//!
//! Counterpart of `arch/x86_64/vmgen.rs` (+ its `vmgen/` submodules).

use super::*;
use monoasm_macro::monoasm_arm64;

mod definition;
pub mod init_method;
mod method_call;
mod variables;



impl Codegen {
    pub(in crate::codegen) fn gen_vm_handlers(&mut self) -> VmHandlers {
        self.a64_gen_entry_raise();
        self.a64_gen_stack_overflow();
        self.a64_gen_exec_gc();
        // f64_to_val helper (D0 f64 -> X0 boxed Value), used by `FprToStack`.
        let f64_to_val = self.f64_to_val.clone();
        self.a64_gen_f64_to_val(&f64_to_val);
        let vm_entry = self.jit.label();
        let entry_fetch = self.jit.label();
        // vm_entry: establish the frame pointer (x86: `pushq rbp; movq rbp,rsp`).
        monoasm_arm64!(&mut self.jit,
            vm_entry:
            stp x29, x30, [sp, #(-16)]!;
            mov x29, sp;
            entry_fetch:
        );
        self.a64_fetch_and_dispatch();
        self.vm_fetch = entry_fetch;
        self.vm_entry = vm_entry;

        let init_method = self.a64_op_init_method();
        let immediate = self.a64_op_immediate();
        let literal = self.a64_op_literal();
        let mov = self.a64_op_mov();
        let ret = self.a64_op_ret();
        let add_rr = self.a64_op_iadd(false);
        let sub_rr = self.a64_op_iadd(true);
        let mul_rr = self.a64_op_muldiv(mul_values);
        let div_rr = self.a64_op_muldiv(div_values);

        // loop_end (15): just advance to the next instruction.
        // loop_start (14): drives the loop (partial) JIT and polls GC/signals
        // on the back-edge (see `a64_op_loop_start`).
        let loop_end = self.a64_op_loop();
        let loop_start = self.a64_op_loop_start();

        // branches (the shared `branch` target lives inside `br_inst`).
        let (br_inst, branch) = self.a64_op_br();
        let condbr = self.a64_op_condbr(&branch, false);
        let condnotbr = self.a64_op_condbr(&branch, true);
        let check_local = self.a64_op_check_local(&branch);
        let nilbr = self.a64_op_nilbr(&branch);
        let optcase = self.a64_op_optcase(&branch);
        let lambda = self.a64_op_lambda();

        // integer comparisons (fixnum fast path; generic runtime fallback).
        // ops 140-146 and 150-156 share these single copies (the latter range
        // is emitted when the result feeds a branch).
        let eq = self.a64_op_cmp(Cond::Eq, cmp_eq_values as *const () as u64);
        let ne = self.a64_op_cmp(Cond::Ne, cmp_ne_values as *const () as u64);
        let lt = self.a64_op_cmp(Cond::Lt, cmp_lt_values as *const () as u64);
        let le = self.a64_op_cmp(Cond::Le, cmp_le_values as *const () as u64);
        let gt = self.a64_op_cmp(Cond::Gt, cmp_gt_values as *const () as u64);
        let ge = self.a64_op_cmp(Cond::Ge, cmp_ge_values as *const () as u64);
        let teq = self.a64_op_cmp(Cond::Eq, cmp_teq_values as *const () as u64);
        // Funcall-semantics TEq for the optimizable opcode (case/when
        // and rescue matching).
        let teq_case = self.a64_op_cmp(Cond::Eq, cmp_teq_case_values as *const () as u64);
        // RescueTEq: no fixnum fast path (a non-Module clause must
        // raise TypeError, so everything goes through the runtime
        // helper).
        let teq_rescue = self.a64_op_cmp_no_opt(cmp_teq_rescue_values as *const () as u64);
        let method_def = self.a64_op_method_def();
        let send_simple = self.a64_op_send(true);
        let send = self.a64_op_send(false);

        let yield_op = self.a64_op_yield();

        // break / raise / retry / redo / ensure-end: set an error and route
        // through entry_raise, which handle_error turns into the right control
        // flow (break value / re-raise / retry / redo).
        let method_ret = self.a64_op_err1(runtime::err_method_return as *const () as u64, true);
        let block_break = self.a64_op_err1(runtime::err_block_break as *const () as u64, true);
        let raise_err = self.a64_op_err_raise();
        let retry_op = self.a64_op_err1(runtime::err_retry as *const () as u64, false);
        let redo_op = self.a64_op_err1(runtime::err_redo as *const () as u64, false);
        let ensure_end = self.a64_op_ensure_end();

        let class_def = self.a64_op_class_def(false);
        let module_def = self.a64_op_class_def(true);
        let singleton_class_def = self.a64_op_singleton_class_def();

        let load_const = self.a64_op_load_const(runtime::vm_get_constant as *const () as u64);
        let check_const = self.a64_op_load_const(runtime::vm_check_constant as *const () as u64);
        let store_const = self.a64_op_store_const();

        let load_ivar = self.a64_op_load_ivar();
        let store_ivar = self.a64_op_store_ivar();

        // `defined?` family (ops 64-69): each computes a truthy/nil result.
        // const/method/ivar write through a *mut Value (dst address);
        // yield/super return the Value and we store it.
        let defined_yield = self.a64_op_defined_to_dst(runtime::defined_yield as *const () as u64);
        let defined_super = self.a64_op_defined_to_dst(runtime::defined_super as *const () as u64);
        let defined_const = self.a64_op_defined_const();
        let defined_method = self.a64_op_defined_method();
        let defined_gvar = self.a64_op_defined_gvar();
        let defined_ivar = self.a64_op_defined_ivar();
        let defined_cvar = self.a64_op_defined_cvar();

        // literal constructors / aggregate ops
        let array = self.a64_op_array();
        let array_teq = self.a64_op_array_teq(runtime::array_teq as *const () as u64);
        let rescue_array_teq = self.a64_op_array_teq(runtime::rescue_array_teq as *const () as u64);
        let array_any = self.a64_op_array_any();
        let array_concat = self.a64_op_array_concat();
        let hash = self.a64_op_hash();
        let hash_insert = self.a64_op_hash_insert();
        let concat = self.a64_op_concat(runtime::concatenate_string as *const () as u64);
        let concat_regexp = self.a64_op_concat(runtime::concatenate_regexp as *const () as u64);
        let range_incl = self.a64_op_range(false);
        let range_excl = self.a64_op_range(true);
        let expand_array = self.a64_op_expand_array();

        let index = self.a64_op_index();
        let index_assign = self.a64_op_index_assign();

        let singleton_method_def = self.a64_op_singleton_method_def();

        let alias_method = self.a64_op_alias_method();
        let undef_method = self.a64_op_undef_method();

        let load_gvar = self.a64_op_load_gvar();
        let store_gvar = self.a64_op_store_var(runtime::set_global_var as *const () as u64);
        let load_cvar = self.a64_op_load_cvar();
        let store_cvar = self.a64_op_store_var(runtime::set_class_var as *const () as u64);
        let alias_gvar = self.a64_op_alias_gvar();

        let block_arg = self.a64_op_block_arg();
        let check_cvar = self.a64_op_check_cvar();
        let check_kw_rest = self.a64_op_check_kw_rest();

        let load_dvar = self.a64_op_load_dvar();
        let store_dvar = self.a64_op_store_dvar();

        let block_arg_proxy = self.a64_op_block_arg_proxy();

        let to_a = self.a64_op_to_a();

        // remaining binary operators (ops 164-170): bitor/bitand/bitxor/
        // rem/pow/shl/shr -- no fixnum fast path, straight to the runtime op.
        let bitor = self.a64_op_binop(bitor_values);
        let bitand = self.a64_op_binop(bitand_values);
        let bitxor = self.a64_op_binop(bitxor_values);
        let rem = self.a64_op_binop(rem_values);
        let pow = self.a64_op_binop(pow_values);
        let shl = self.a64_op_binop(shl_values);
        let shr = self.a64_op_binop(shr_values);

        // unary operators (ops 121-124): pos, neg, bitnot, not
        let pos = self.a64_op_unop(pos_value as *const () as u64);
        let neg = self.a64_op_unop(neg_value as *const () as u64);
        let bitnot = self.a64_op_unop(bitnot_value as *const () as u64);
        let not = self.a64_op_unop(not_value as *const () as u64);

        VmHandlers {
            singleton_method_def,
            method_def,
            br_inst,
            condbr,
            condnotbr,
            immediate,
            literal,
            load_const,
            store_const,
            loop_start,
            loop_end,
            load_ivar,
            store_ivar,
            check_const,
            check_kw_rest,
            check_local,
            block_arg_proxy,
            singleton_class_def,
            block_arg,
            check_cvar,
            load_gvar,
            store_gvar,
            load_cvar,
            alias_gvar,
            store_cvar,
            send_simple,
            send,
            yield_: yield_op,
            yield2: yield_op,
            optcase,
            nilbr,
            lambda,
            array,
            array_teq,
            rescue_array_teq,
            array_any,
            array_concat,
            hash_insert,
            defined_yield,
            defined_const,
            defined_method,
            defined_gvar,
            defined_ivar,
            defined_super,
            class_def,
            module_def,
            ret,
            method_ret,
            block_break,
            raise_err,
            retry: retry_op,
            ensure_end,
            concat_regexp,
            redo: redo_op,
            defined_cvar,
            pos,
            neg,
            bitnot,
            not,
            index,
            index_assign,
            eq,
            ne,
            lt,
            le,
            gt,
            ge,
            teq,
            teq_case,
            teq_rescue,
            load_dvar,
            store_dvar,
            add: add_rr,
            sub: sub_rr,
            mul: mul_rr,
            div: div_rr,
            bitor,
            bitand,
            bitxor,
            rem,
            pow,
            shl,
            shr,
            init: init_method,
            expand_array,
            undef_method,
            alias_method,
            hash,
            to_a,
            mov,
            range_incl,
            range_excl,
            concat,
        }
    }

    /// ops 121-124 `UnOp` (pos/neg/bitnot/not): fn(vm, globals, src `[pc+2]`)
    /// -> dst `[pc+4]`.
    pub(in crate::codegen) fn a64_op_unop(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // src (for get_class)
        // Fill the UnOp inline cache (operand class @ classid1 = [PC+8]) so the
        // JIT can type the operand instead of bailing NotCached. Mirrors x86
        // vm_save_lhs_class. NB: get_class clobbers x1/x2 for immediate
        // receivers (nil/bool/symbol), so the operand is reloaded below before
        // the op call.
        self.a64_save_lhs_class();
        monoasm_arm64!(&mut self.jit,
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // reload src (get_class clobbered x2)
        monoasm_arm64!(&mut self.jit,
            // is_func_call = (operand slot == self slot 0). The operand slot is
            // the `[PC+2]` bytecode operand; passed in x3 (4th C-arg).
            ldrh x3, [x(PC.0), #(2)];
            cmp x3, #(0);
            cset x3, eq;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (abs);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 20 `CheckLocal`: branch by disp `[pc+0]` if local `[pc+4]` is set
    /// (non-zero); otherwise fall through (used for optional-param defaults).
    pub(in crate::codegen) fn a64_op_check_local(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrsw x10, [x(PC.0)];  // disp (for the shared branch target)
            ldrh x12, [x(PC.0), #(4)];  // local slot
        );
        self.a64_slot_value(X12);
        monoasm_arm64!(&mut self.jit,
            cbnz x12, branch;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 36 `OptCase`: dense `case`/`when` jump table. opt_case returns the
    /// branch displacement for cond slot `[pc+4]` against OptCaseId `[pc+0]`,
    /// which feeds the shared branch target.
    pub(in crate::codegen) fn a64_op_optcase(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0)];  // OptCaseId
            ldrh x10, [x(PC.0), #(4)];  // cond slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x3, x10;  // cond value
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::opt_case as *const () as u64);
            blr x9;
            lsl x10, x0, #(32);  // zero-extend u32 disp into X10
            lsr x10, x10, #(32);
            b branch;
        );
        p
    }

    /// op 38 `Lambda`: dst `[pc+4]` <- a lambda Proc for func_id `[pc+0]`.
    /// gen_lambda may promote the current frame to the heap, so LFP is
    /// reloaded from the cfp afterward.
    pub(in crate::codegen) fn a64_op_lambda(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // func_id
            mov x3, x(PC.0);  // call-site pc
            mov x9, (runtime::gen_lambda as *const () as u64);
            blr x9;
            ldur x(LFP.0), [x29, #(-((BP_CFP + CFP_LFP) as i32))];  // restore (possibly heap-promoted) LFP
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 37 `NilBr`: branch by disp `[pc+0]` if cond slot `[pc+4]` is nil.
    pub(in crate::codegen) fn a64_op_nilbr(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrsw x10, [x(PC.0)];  // disp (for shared branch target)
            ldrh x11, [x(PC.0), #(4)];  // cond slot
        );
        self.a64_slot_value(X11);
        monoasm_arm64!(&mut self.jit,
            cmp x11, #(NIL_VALUE as u32);
        );
        self.jit.bcond_label(Cond::Eq, branch);
        monoasm_arm64!(&mut self.jit,
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 23 `BlockArg`: block_arg(vm, globals, lfp, pc) -> dst `[pc+4]`.
    pub(in crate::codegen) fn a64_op_block_arg(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);
            mov x3, x(PC.0);  // BytecodePtr (instruction start)
            mov x9, (runtime::block_arg as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 177 `ToA`: dst `[pc+4]` <- `to_a(src `[pc+2]`)` (splat coercion).
    pub(in crate::codegen) fn a64_op_to_a(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // src
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::to_a as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 21 `BlockArgProxy`: dst `[pc+4]` <- the block handler of the frame
    /// `[pc+0]` levels up, re-encoding a proxy handler's depth. (x86
    /// `vm_block_arg_proxy`.)
    pub(in crate::codegen) fn a64_op_block_arg_proxy(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let notzero = self.jit.label();
        let exit = self.jit.label();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x10, x(LFP.0);
            ldr w11, [x(PC.0)];  // outer level
            cbz x11, loop_exit;
            loop_:
            ldr x10, [x10];  // walk outer chain
            subs x11, x11, #(1);
        );
        self.jit.bcond_label(Cond::Ne, &loop_);
        monoasm_arm64!(&mut self.jit,
            loop_exit:
        // block handler = [outer - LFP_BLOCK]
            ldur x10, [x10, #(-(LFP_BLOCK as i32))];
            cbnz x10, notzero;
            mov x10, (NIL_VALUE);  // no block -> nil
            notzero:
        // if bit0 == 0 (Proc/nil), keep as-is; else re-encode proxy depth.
            tbz x10, #(0), exit;
            ldrsw x12, [x(PC.0)];  // outer (signed)
            lsl x12, x12, #(2);
            add x10, x10, x12;
            add x10, x10, #(2);
            exit:
        // store X10 to dst [pc+4]
            ldrh x11, [x(PC.0), #(4)];
            cbz x11, skip;
            neg x11, x11;
            add x12, x(LFP.0), x11, lsl #(3);
            stur x10, [x12, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 19 `CheckKwRest`: if the kw-rest slot `[pc+4]` is nil, replace it
    /// with a fresh empty hash.
    pub(in crate::codegen) fn a64_op_check_kw_rest(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(4)];
        );
        self.a64_slot_addr(X10); // &slot
        monoasm_arm64!(&mut self.jit,
            ldr x11, [x10];
            cmp x11, #(NIL_VALUE as u32);
        );
        self.jit.bcond_label(Cond::Ne, &exit);
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::empty_hash as *const () as u64);
            blr x9;
            ldrh x10, [x(PC.0), #(4)];
        );
        self.a64_slot_addr(X10); // re-compute (clobbered by call)
        monoasm_arm64!(&mut self.jit,
            str x0, [x10];
            exit:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 175 `AliasMethod`: alias_method(vm, globals, old `[pc+2]`,
    /// new `[pc+4]`).
    pub(in crate::codegen) fn a64_op_alias_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // old
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X3); // new
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::alias_method as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 174 `UndefMethod`: undef_method(vm, globals, name `[pc+0]`).
    pub(in crate::codegen) fn a64_op_undef_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::undef_method as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 132 `Index`: dst[`[pc+4]`] <- base[`[pc+2]`][idx[`[pc+0]`]], with an
    /// inline ClassId cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_index(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];  // base slot
            // is_func_call = (base slot == self, slot 0): a literal `self[i]`
            // reaches a private `#[]`; any other receiver enforces visibility.
            cmp x2, #(0);
        );
        self.jit.cset(X5, Cond::Eq); // x5 <- (base slot == 0)
        self.a64_slot_value(X2); // base
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];
        );
        self.a64_slot_value(X3); // idx
        monoasm_arm64!(&mut self.jit,
            add x4, x(PC.0), #(8);  // &cache (8-aligned)
            orr x4, x4, x5;         // fold is_func_call into bit 0
            mov x9, (runtime::get_index as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 133 `IndexAssign`: base[`[pc+2]`][idx[`[pc+0]`]] <- src[`[pc+4]`],
    /// with an inline ClassId cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_index_assign(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];  // base slot
            // is_func_call = (base slot == self, slot 0): `self[i] = v` reaches
            // a private `#[]=`; any other receiver enforces visibility.
            cmp x2, #(0);
        );
        self.jit.cset(X6, Cond::Eq); // x6 <- (base slot == 0)
        self.a64_slot_value(X2); // base
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];
        );
        self.a64_slot_value(X3); // idx
        monoasm_arm64!(&mut self.jit,
            ldrh x4, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X4); // src
        monoasm_arm64!(&mut self.jit,
            add x5, x(PC.0), #(8);  // &cache (8-aligned)
            orr x5, x5, x6;         // fold is_func_call into bit 0
            mov x9, (runtime::set_index as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store the Option<Value> result in X0 to the dst slot `[pc+4]`: branch
    /// to `raise` if it is 0 (error), else store, advance PC, and dispatch.
    pub(in crate::codegen) fn a64_checked_store_next(&mut self, raise: &DestLabel) {
        let skip = self.jit.label();
        let ok = self.jit.label();
        // `cbz x0, raise` but long-range. `remove_vm_bop_optimization`
        // regenerates the binop/cmp/unop handlers at runtime, far (> ±1MB)
        // from the startup `entry_raise`, so a bare conditional branch (imm19)
        // to `raise` overflows. Invert past a near label and reach `raise`
        // with an unconditional `b` (±128MB).
        monoasm_arm64!(&mut self.jit,
            cbnz x0, ok;   // result != 0 -> ok
            b raise;       // result 0 -> error (long range)
            ok:
            ldrh x10, [x(PC.0), #(4)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
    }

    /// op 39 `Array`: gen_array(vm, globals, callid `[pc+0]`, &self).
    pub(in crate::codegen) fn a64_op_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // callid
            sub x3, x(LFP.0), #(LFP_SELF as u32);  // &self
            mov x9, (runtime::gen_array as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 40 `ArrayTEq`: %lhs = (%lhs === %rhs). If %lhs is an Array, returns
    /// true iff some element matches %rhs (via `===`). The result overwrites
    /// the lhs slot. Bytecode: `+0` rhs, `+2` lhs (also dst).
    /// Shared generator for op 40 (`ArrayTEq`, `f` = runtime::array_teq)
    /// and op 44 (rescue-splat variant, `f` = runtime::rescue_array_teq).
    pub(in crate::codegen) fn a64_op_array_teq(&mut self, f: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot (also dst)
        );
        self.a64_load_slot(X11, X3, X12); // X3 = lhs value
        self.a64_load_slot(X10, X4, X12); // X4 = rhs value
        // array_teq / rescue_array_teq (vm, globals, lhs, rhs) -> Option<Value>
        monoasm_arm64!(&mut self.jit,
            mov x2, x3;  // lhs (arg #3)
            mov x3, x4;  // rhs (arg #4)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (f);
            blr x9;
            cbz x0, raise;
        // dst slot = lhs slot from [PC+2]
            ldrh x10, [x(PC.0), #(2)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 43 `ArrayAny`: %reg = any element of the array in %reg is truthy.
    /// The result overwrites the reg slot. Bytecode: `+2` reg (also dst).
    /// `array_any` returns a plain `Value` and cannot raise.
    pub(in crate::codegen) fn a64_op_array_any(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x11, [x(PC.0), #(2)];  // reg slot (also dst)
        );
        self.a64_load_slot(X11, X3, X12); // X3 = val
        // array_any(vm, globals, val) -> Value
        monoasm_arm64!(&mut self.jit,
            mov x2, x3;  // val (arg #3)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::array_any as *const () as u64);
            blr x9;
        // dst slot = reg slot from [PC+2]
            ldrh x10, [x(PC.0), #(2)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 176 `Hash`: gen_hash(vm, globals, src `[pc+2]`, len `[pc+0]`).
    pub(in crate::codegen) fn a64_op_hash(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_addr(X2); // src
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];  // len
            mov x9, (runtime::gen_hash as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 42 `HashInsert`: hash_insert(vm, globals, src `[pc+2]`,
    /// len `[pc+0]`, hash `[pc+4]`). The hash slot doubles as the
    /// destination (the runtime returns the same hash).
    pub(in crate::codegen) fn a64_op_hash_insert(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_addr(X2); // src
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];      // len
            ldrh x10, [x(PC.0), #(4)];  // hash slot
        );
        self.a64_load_slot(X10, X4, X11); // x4 = hash value
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::hash_insert as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 41 `ArrayConcat`: array_concat(vm, globals, dst `[pc+4]`,
    /// src `[pc+2]`). The dst slot doubles as the destination (the runtime
    /// returns dst).
    pub(in crate::codegen) fn a64_op_array_concat(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            ldrh x11, [x(PC.0), #(2)];  // src slot
        );
        self.a64_load_slot(X10, X2, X12); // x2 = dst value
        self.a64_load_slot(X11, X3, X12); // x3 = src value
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::array_concat as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 181 `ConcatStr` / op 86 `ConcatRegexp`: fn(vm, globals,
    /// args `[pc+2]`, len `[pc+0]`).
    pub(in crate::codegen) fn a64_op_concat(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_addr(X2); // args
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];  // len
            mov x9, (abs);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 179/180 `Range`: gen_range(start `[pc+2]`, end `[pc+0]`, vm,
    /// globals, exclude_end).
    pub(in crate::codegen) fn a64_op_range(&mut self, exclude_end: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldrh x0, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X0); // start
        monoasm_arm64!(&mut self.jit,
            ldrh x1, [x(PC.0)];
        );
        self.a64_slot_value(X1); // end
        monoasm_arm64!(&mut self.jit,
            mov x2, x(EXEC.0);
            mov x3, x(GLOBALS.0);
            mov x4, (if exclude_end { 1 } else { 0 });
            mov x9, (runtime::gen_range as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 173 `ExpandArray`: expand_array(vm, globals, src `[pc+4]`,
    /// &dst `[pc+2]`, len `[pc+0]`, rest `[pc+8]`). May dispatch `#to_ary`
    /// and raise, so the `X0 == 0` error path branches to `entry_raise`.
    pub(in crate::codegen) fn a64_op_expand_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X2); // src
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0), #(2)];
        );
        self.a64_slot_addr(X3); // &dst
        monoasm_arm64!(&mut self.jit,
            ldrh x4, [x(PC.0)];  // len
            ldrh x5, [x(PC.0), #(8)];  // rest
            mov x9, (runtime::expand_array as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store `X0` (a Value) into the dst slot at `[pc+4]`, advance PC, dispatch.
    pub(in crate::codegen) fn a64_store_dst_and_next(&mut self, skip: &DestLabel) {
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(4)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
    }

    /// `defined?` ops 64/69 (yield/super): fn(vm, globals) -> Value, stored to
    /// the dst slot `[pc+4]`.
    pub(in crate::codegen) fn a64_op_defined_to_dst(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (abs);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 65 `DefinedConst`: defined_const(vm, globals, &dst, site_id `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(4)];
        );
        self.a64_slot_addr(X2); // &dst
        monoasm_arm64!(&mut self.jit,
            ldr w3, [x(PC.0), #(8)];  // site_id
            mov x9, (runtime::defined_const as *const () as u64);
            blr x9;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 66 `DefinedMethod`: defined_method(vm, globals, &dst, recv `[pc+2]`,
    /// name `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(4)];
        );
        self.a64_slot_addr(X2); // &dst
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X3); // recv
        monoasm_arm64!(&mut self.jit,
            ldr w4, [x(PC.0), #(8)];  // name
            mov x9, (runtime::defined_method as *const () as u64);
            blr x9;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 67 `DefinedGvar`: defined_gvar(vm, globals, name `[pc+8]`) -> Value.
    pub(in crate::codegen) fn a64_op_defined_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0), #(8)];  // name
            mov x9, (runtime::defined_gvar as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 88 `DefinedCvar`: defined_cvar(vm, globals, name `[pc+8]`) -> Value.
    pub(in crate::codegen) fn a64_op_defined_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0), #(8)];  // name
            mov x9, (runtime::defined_cvar as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 68 `DefinedIvar`: defined_ivar(vm, globals, &dst, name `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(4)];
        );
        self.a64_slot_addr(X2); // &dst
        monoasm_arm64!(&mut self.jit,
            ldr w3, [x(PC.0), #(8)];  // name
            mov x9, (runtime::defined_ivar as *const () as u64);
            blr x9;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// ops 82/84/87 `BlockBreak`/`Retry`/`Redo`: call `f(vm[, globals, val])`
    /// to set the control-flow error, then enter entry_raise. `with_val`
    /// passes slot `[pc+4]`'s value as a 3rd argument (BlockBreak).
    pub(in crate::codegen) fn a64_op_err1(&mut self, abs: u64, with_val: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
        );
        if with_val {
            monoasm_arm64!(&mut self.jit,
                mov x1, x(GLOBALS.0);
                ldrh x2, [x(PC.0), #(4)];
            );
            self.a64_slot_value(X2); // val
        }
        monoasm_arm64!(&mut self.jit,
            mov x9, (abs);
            blr x9;
            b raise;
        );
        p
    }

    /// op 83 `Raise`: raise_err(vm, exc `[pc+4]`), then enter entry_raise.
    pub(in crate::codegen) fn a64_op_err_raise(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            ldrh x1, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X1); // exception value
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::raise_err as *const () as u64);
            blr x9;
            b raise;
        );
        p
    }

    /// op 85 `EnsureEnd`: if an error is still pending after an ensure block,
    /// re-enter entry_raise; otherwise continue.
    pub(in crate::codegen) fn a64_op_ensure_end(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x9, (runtime::ensure_end as *const () as u64);
            blr x9;
            cbnz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// loop_end (op 15): plain advance + dispatch.
    pub(in crate::codegen) fn a64_op_loop(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// loop_start (op 14) with loop (partial) JIT. Mirrors x86 `vm_loop_start`.
    ///
    /// The loop_start bytecode reserves two operand slots: a per-loop hit
    /// counter at `[PC+0]` (i32) and the compiled-loop codeptr at `[PC+8]`
    /// (written by `compile_partial` via `BytecodePtr::write2`). When the
    /// codeptr is set, jump straight into the compiled loop. Otherwise bump the
    /// counter and, once it reaches `COUNT_LOOP_START_COMPILE`, call
    /// `jit_compile_loop` to compile the loop body. A captured (on-heap /
    /// invalidated) frame skips the JIT — its locals may be aliased on the
    /// heap, which the register-caching JIT can't honour.
    ///
    /// The compiled loop is entered with PC advanced 16 bytes past the
    /// loop_start op, matching x86 (whose `fetch_and_dispatch` advanced r13 by
    /// 16 before dispatching to the handler).
    ///
    /// The codeptr slot at `[PC+8]` is tri-state: `0` = not compiled yet, `1` =
    /// a disabled sentinel (the compile bailed on an unported AsmInst — never
    /// retry, just keep interpreting), any other value = the real compiled
    /// entry. Without the sentinel a bailing hot loop would re-run the (failed,
    /// non-trivial) compile every time the counter crosses the threshold —
    /// thousands of times per call — which is far slower than just interpreting
    /// it. aarch64-only; x86 effectively never bails so it does not need this.
    pub(in crate::codegen) fn a64_op_loop_start(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let compile = self.jit.label();
        let cont = self.jit.label();
        let count = self.jit.label();
        let enter = self.jit.label();
        let f = crate::codegen::compiler::jit_compile_loop as *const () as u64;
        // GC + signal poll on the loop back-edge (x86 `vm_loop_start` calls
        // `vm_execute_gc` first). A tight loop that never calls a method would
        // otherwise never reach a safepoint, so a pending Signal.trap callback
        // (or the GC the signal handler requested) could not run. The non-opt
        // loop_start (`a64_op_loop`, also used for loop_end)
        // omits this, mirroring x86 `vm_loop_start_no_opt`.
        self.a64_vm_execute_gc();
        monoasm_arm64!(&mut self.jit,
            // Skip the JIT for a captured frame: the meta `kind` byte at
            // [LFP - (LFP_META - META_KIND)] has bit7 = on_heap, bit3 = invalidated.
            sub x10, x(LFP.0), #((LFP_META - META_KIND as i32) as u32);
            ldrb x10, [x10];
            tbnz x10, #(7), cont;
            tbnz x10, #(3), cont;
            ldr x10, [x(PC.0), #(8)];      // codeptr slot (0 / 1-sentinel / codeptr)
            cbz x10, count;                // 0 -> count toward the threshold
            cmp x10, #(1);
        );
        self.jit.bcond_label(Cond::Eq, &cont); // 1 -> compile bailed, interpret
        monoasm_arm64!(&mut self.jit,
            enter:
            add x(PC.0), x(PC.0), #(16);   // PC past loop_start (x86 r13 convention)
            br x10;                         // real codeptr -> enter the compiled loop
            count:
            ldr w11, [x(PC.0)];            // per-loop hit counter (i32)
            add w11, w11, #(1);
            str w11, [x(PC.0)];
            cmp w11, #(COUNT_LOOP_START_COMPILE);
        );
        self.jit.bcond_label(Cond::Ge, &compile);
        monoasm_arm64!(&mut self.jit,
            cont:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        monoasm_arm64!(&mut self.jit,
            // Threshold reached: compile the loop body now.
            compile:
            mov x0, x(GLOBALS.0);          // globals
            mov x1, x(LFP.0);              // lfp
            mov x2, x(PC.0);               // pc (loop_start)
            mov x9, (f);
            blr x9;
            ldr x10, [x(PC.0), #(8)];      // codeptr written on success, else 0
            cbnz x10, enter;
            // Bail: stamp the sentinel so this loop is never re-compiled.
            mov x10, #(1);
            str x10, [x(PC.0), #(8)];
            b cont;
        );
        p
    }

    /// Unconditional branch (op 3) + the shared `branch` target used by the
    /// conditional branches. `pc += disp*16 + 16` (x86 `br_inst`/`branch:`).
    pub(in crate::codegen) fn a64_op_br(&mut self) -> (CodePtr, DestLabel) {
        let p = self.jit.get_current_address();
        let branch = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrsw x10, [x(PC.0)];  // disp (signed, instruction-relative)
            branch:
            lsl x10, x10, #(4);
            add x(PC.0), x(PC.0), x10;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        (p, branch)
    }

    /// Conditional branch (op 4/12 `condbr`, op 5/13 `condnotbr`). Bytecode:
    /// `+0` disp (i32), `+4` cond slot. Truthiness: `(v | 0x10) != FALSE_VALUE`
    /// (both nil and false collapse to FALSE_VALUE). `not` = branch-if-falsy.
    pub(in crate::codegen) fn a64_op_condbr(&mut self, branch: &DestLabel, not: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrsw x10, [x(PC.0)];  // disp (kept in X10 for `branch`)
            ldrh x11, [x(PC.0), #(4)];  // cond slot
        );
        self.a64_load_slot(X11, X12, X13); // cond value
        monoasm_arm64!(&mut self.jit,
            mov x13, (0x10);
            orr x12, x12, x13;
            cmp x12, #(FALSE_VALUE as u32);
        );
        let cond = if not { Cond::Eq } else { Cond::Ne };
        self.jit.bcond_label(cond, branch);
        monoasm_arm64!(&mut self.jit,
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// Integer comparison (ops 140-146): `%dst = (%lhs <cond> %rhs)` as a Ruby
    /// boolean. Bytecode: `+0` rhs, `+2` lhs, `+4` dst. Non-fixnum traps
    /// (generic runtime fallback TODO).
    pub(in crate::codegen) fn a64_op_cmp(&mut self, cond: Cond, cmp_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot
            ldrh x12, [x(PC.0), #(4)];  // dst slot
        );
        self.a64_load_slot(X11, X13, X14); // lhs
        self.a64_load_slot(X10, X14, X15); // rhs
        monoasm_arm64!(&mut self.jit,
            tbz x13, #(0), generic;
            tbz x14, #(0), generic;
        );
        self.a64_save_binary_integer();
        monoasm_arm64!(&mut self.jit,
            cmp x13, x14;
        );
        self.jit.cset(X13, cond);
        monoasm_arm64!(&mut self.jit,
            lsl x13, x13, #(3);
            mov x14, (FALSE_VALUE);
            orr x13, x13, x14;  // FALSE_VALUE | (result << 3)
            cbz x12, skip;
            neg x12, x12;
            add x10, x(LFP.0), x12, lsl #(3);
            stur x13, [x10, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        monoasm_arm64!(&mut self.jit,
            generic:
        );
        // Fill the BinOp inline cache on the generic (non-fixnum) path too, so
        // the JIT can type non-integer comparisons (Float/String/...) instead
        // of bailing NotCached and recompiling forever. Reads X13/X14 (they
        // survive get_class), clobbers x0/x1/x2 and x10/x11/x12.
        self.a64_save_binary_class();
        monoasm_arm64!(&mut self.jit,
        // cmp_*_values(vm, globals, lhs=X13, rhs=X14, is_func_call) -> Option<Value>
            mov x2, x13;  // lhs
            mov x3, x14;  // rhs
            // is_func_call = (lhs slot == self slot 0); the lhs slot is the
            // `[PC+2]` bytecode operand. Passed in x4 (5th C-arg).
            ldrh x4, [x(PC.0), #(2)];
            cmp x4, #(0);
            cset x4, eq;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (cmp_fn);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// Load the value of the slot whose (positive) index is in `idx`, into
    /// `dst`. `idx` is clobbered. (the `[r14+reg*8-LFP_SELF]` idiom.)
    pub(in crate::codegen) fn a64_load_slot(&mut self, idx: GReg, dst: GReg, scratch: GReg) {
        monoasm_arm64!(&mut self.jit,
            neg x(idx.0), x(idx.0);
            add x(scratch.0), x(LFP.0), x(idx.0), lsl #(3);
            sub x(scratch.0), x(scratch.0), #(LFP_SELF as u32);
            ldr x(dst.0), [x(scratch.0)];
        );
    }

    /// Generic binary-op fallback: call the runtime `func(vm, globals, lhs,
    /// rhs)` and store the result. Expects lhs in X13, rhs in X14, dst slot in
    /// X12 (all intact). VM globals are callee-saved so no register save is
    /// needed. On a Ruby error (result 0) jumps to entry_raise.
    /// ops 164-170: a binary operator with no fixnum fast path. Loads
    /// lhs `[pc+2]`, rhs `[pc+0]`, dst `[pc+4]` and calls the runtime op.
    pub(in crate::codegen) fn a64_op_binop(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot
            ldrh x12, [x(PC.0), #(4)];  // dst slot
        );
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.a64_generic_binop(func);
        p
    }

    /// Record the runtime operand classes into the BinOp inline cache so the
    /// JIT can type the site (e.g. classify a Float `+`): `[PC+8]` = classid1,
    /// `[PC+12]` = classid2. Mirrors x86 `vm_save_binary_class`, including the
    /// polymorphic-flag bookkeeping (sets `opcode_sub` = 1 when an operand
    /// class changes after the cache is populated). Operands are the Values in
    /// X13 (lhs) / X14 (rhs); `get_class` reads X0 only, so X13/X14 survive.
    /// Clobbers x0/x1/x2 and x10/x11/x12 and the link register.
    /// Record the unary operand's class into the UnOp inline cache (classid1
    /// `[PC+8]`) so the JIT can type the site. Mirrors x86 `vm_save_lhs_class`.
    /// Operand is the Value in x2. NB: `get_class` clobbers x1/x2 for
    /// immediate receivers, so callers must reload the operand afterwards.
    /// Clobbers x0/x1/x2 and the link register.
    pub(in crate::codegen) fn a64_save_lhs_class(&mut self) {
        let get_class = self.get_class.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x2;
            bl get_class;             // x0 = class(operand)
            str w0, [x(PC.0), #(8)];  // classid1
        );
    }

    pub(in crate::codegen) fn a64_save_binary_class(&mut self) {
        let get_class = self.get_class.clone();
        let set_poly = self.jit.label();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            // Read the previously-cached operand classes before overwriting
            // them (x10 = old classid1, x12 = old classid2; 0 = cache empty),
            // for polymorphic detection. get_class only touches x0/x1/x2/x9/lr,
            // so x10/x12/x13/x14 survive across the calls below.
            ldr w10, [x(PC.0), #(8)];   // old classid1
            ldr w12, [x(PC.0), #(12)];  // old classid2
            mov x0, x13;
            bl get_class;             // x0 = class(lhs)
            str w0, [x(PC.0), #(8)];  // classid1
            mov x0, x14;
            bl get_class;             // x0 = class(rhs)
            str w0, [x(PC.0), #(12)]; // classid2
            // Polymorphic detection (mirrors x86 `vm_save_binary_class`): once
            // the cache is populated (old classid1 != 0), if either operand's
            // class changed, mark the site polymorphic (opcode_sub = 1, offset
            // +7) so the JIT emits a non-deoptimizing dispatch instead of a
            // monomorphic class guard.
            cbz w10, skip;
            ldr w11, [x(PC.0), #(8)];
            cmp w10, w11;
        );
        self.jit.bcond_label(Cond::Ne, &set_poly);
        monoasm_arm64!(&mut self.jit,
            ldr w11, [x(PC.0), #(12)];
            cmp w12, w11;
        );
        self.jit.bcond_label(Cond::Ne, &set_poly);
        monoasm_arm64!(&mut self.jit,
            b skip;
            set_poly:
            mov x11, (1);
            strb w11, [x(PC.0), #(7)];  // opcode_sub = 1 (polymorphic)
            skip:
        );
    }

    /// Fixnum-fast-path counterpart of [`Self::a64_save_binary_class`]: stamp
    /// `Integer`/`Integer` into the BinOp inline cache (`[PC+8]` classid1,
    /// `[PC+12]` classid2) so the JIT can type integer arithmetic/compare
    /// sites. Without this the cache stays empty (`<INVALID>`) and the JIT
    /// deopts every integer binop. Mirrors x86 `vm_save_binary_integer`.
    /// Clobbers X10; leaves the NZCV flags untouched (mov-immediate + stores).
    pub(in crate::codegen) fn a64_save_binary_integer(&mut self) {
        let int_class: u32 = INTEGER_CLASS.into();
        monoasm_arm64!(&mut self.jit,
            mov x10, (int_class);
            str w10, [x(PC.0), #(8)];   // classid1
            str w10, [x(PC.0), #(12)];  // classid2
        );
    }

    pub(in crate::codegen) fn a64_generic_binop(&mut self, func: BinaryOpFn) {
        let raise = self.entry_raise.clone();
        self.a64_save_binary_class();
        monoasm_arm64!(&mut self.jit,
            mov x2, x13;  // lhs
            mov x3, x14;  // rhs
            // is_func_call = (lhs slot == self slot 0). The lhs slot is the
            // `[PC+2]` bytecode operand; passed in x4 (5th C-arg).
            ldrh x4, [x(PC.0), #(2)];
            cmp x4, #(0);
            cset x4, eq;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (func as u64);
            blr x9;
        // The dst slot is re-read from PC (callee-saved): `func` may re-enter
        // the VM (e.g. string `+` dispatches a method), clobbering the
        // caller-saved dst register held before the call.
        );
        self.a64_checked_store_next(&raise);
    }

    /// op 162/163 `mul_rr`/`div_rr`: no fixnum fast path — straight to the
    /// runtime fallback (matches x86 `vm_binops`). Bytecode: `+0` rhs, `+2`
    /// lhs, `+4` dst.
    pub(in crate::codegen) fn a64_op_muldiv(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];
            ldrh x11, [x(PC.0), #(2)];
            ldrh x12, [x(PC.0), #(4)];
        );
        self.a64_load_slot(X11, X13, X14); // lhs
        self.a64_load_slot(X10, X14, X15); // rhs
        self.a64_generic_binop(func);
        p
    }

    /// op 160/161 `add_rr`/`sub_rr`: fixnum fast path (`%dst = %lhs ± %rhs`)
    /// with a runtime fallback on non-fixnum/overflow. Operands are kept in
    /// X13/X14 so the fallback can use them; the result is computed in X9.
    pub(in crate::codegen) fn a64_op_iadd(&mut self, is_sub: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let generic = self.jit.label();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot
            ldrh x12, [x(PC.0), #(4)];  // dst slot
        );
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        monoasm_arm64!(&mut self.jit,
            tbz x13, #(0), generic;
            tbz x14, #(0), generic;
        );
        self.a64_save_binary_integer();
        if is_sub {
            monoasm_arm64!(&mut self.jit,
                subs x9, x13, x14;
            );
            self.jit.bcond_label(Cond::Vs, &generic);
            monoasm_arm64!(&mut self.jit,
                add x9, x9, #(1);  // re-tag
            );
        } else {
            monoasm_arm64!(&mut self.jit,
                sub x9, x13, #(1);  // untag one
                adds x9, x9, x14;
            );
            self.jit.bcond_label(Cond::Vs, &generic);
        }
        monoasm_arm64!(&mut self.jit,
            cbz x12, skip;
            neg x12, x12;
            add x10, x(LFP.0), x12, lsl #(3);
            stur x9, [x10, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        monoasm_arm64!(&mut self.jit,
            generic:
        );
        self.a64_generic_binop(if is_sub {
            sub_values
        } else {
            add_values
        });
        p
    }

    /// op 6 `immediate`: slot[`[pc+4]`] <- the immediate Value at `[pc+8]`.
    /// (x86 `vm_immediate`: `fetch_r15; movq rax,[r13-8]; vm_store_r15`.)
    pub(in crate::codegen) fn a64_op_immediate(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(4)];  // dst slot index
            ldr x11, [x(PC.0), #(8)];  // immediate value
            cbz x10, skip;  // slot 0 => discard
            neg x10, x10;
            add x12, x(LFP.0), x10, lsl #(3);
            stur x11, [x12, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 7 `literal`: slot[`[pc+4]`] <- a deep copy of the literal Value at
    /// `[pc+8]`. Each evaluation yields a fresh object (mutable literals like
    /// strings/arrays). x86 `vm_literal`: `movq rdi,[r13-8]; value_deep_copy`.
    pub(in crate::codegen) fn a64_op_literal(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x(PC.0), #(8)];  // literal Value
            mov x9, (Value::value_deep_copy as *const () as u64);
            blr x9;  // x0 = deep copy (PC/LFP are callee-saved)
            ldrh x10, [x(PC.0), #(4)];  // dst slot index
            cbz x10, skip;  // slot 0 => discard
            neg x10, x10;
            add x12, x(LFP.0), x10, lsl #(3);
            stur x0, [x12, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 178 `Mov`: slot[`[pc+4]`] <- slot[`[pc+2]`]. (x86 `fetch3` +
    /// slot copy.)
    pub(in crate::codegen) fn a64_op_mov(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(2)];  // src slot
        );
        self.a64_slot_value(X10); // X10 = slot[src]
        monoasm_arm64!(&mut self.jit,
            ldrh x11, [x(PC.0), #(4)];  // dst slot
            cbz x11, skip;
            neg x11, x11;
            add x12, x(LFP.0), x11, lsl #(3);
            stur x10, [x12, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 80 `ret`: return slot[`[pc+4]`]'s value (x86 `fetch_addr_r15;
    /// movq rax,[r15]; epilogue`).
    pub(in crate::codegen) fn a64_op_ret(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0), #(4)];  // slot index
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            ldur x0, [x11, #(-(LFP_SELF as i32))];  // return value
        // epilogue (x86 `leave; ret`): restore the frame pointer and return.
            mov sp, x29;
            ldp x29, x30, [sp], #(16);
            ret;
        );
        p
    }
    /// Generic comparison handler with no fixnum fast path: calls
    /// `cmp_fn(vm, globals, lhs, rhs) -> Option<Value>` and stores the result.
    /// Used by `remove_vm_bop_optimization` to swap in the `_no_opt` runtimes
    /// after a BOP redefinition so the inline `==`/`<`/… fast paths stop being
    /// taken. Bytecode: `+0` rhs, `+2` lhs, `+4` dst (same as a64_op_cmp).
    pub(in crate::codegen) fn a64_op_cmp_no_opt(&mut self, cmp_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot
        );
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        monoasm_arm64!(&mut self.jit,
            mov x2, x13;
            mov x3, x14;
            // is_func_call = (lhs slot == self slot 0); the lhs slot is the
            // `[PC+2]` bytecode operand. Passed in x4 (5th C-arg).
            ldrh x4, [x(PC.0), #(2)];
            cmp x4, #(0);
            cset x4, eq;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (cmp_fn);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// Patch the dispatch table so the fixnum fast paths for arithmetic /
    /// comparison / unary ops stop firing. Called from `set_bop_redefined`
    /// when a basic op (e.g. `Integer#*`) is overridden — the new handlers
    /// call the `_no_opt` runtimes which always invoke the redefined method
    /// instead of returning the fixnum result inline. Mirrors the x86
    /// `remove_vm_bop_optimization` in `vmgen.rs`.
    pub(in crate::codegen) fn remove_vm_bop_optimization(&mut self) {
        let add = self.a64_op_binop(add_values_no_opt);
        let sub = self.a64_op_binop(sub_values_no_opt);
        let mul = self.a64_op_binop(mul_values_no_opt);
        let div = self.a64_op_binop(div_values_no_opt);
        let bitor = self.a64_op_binop(bitor_values_no_opt);
        let bitand = self.a64_op_binop(bitand_values_no_opt);
        let bitxor = self.a64_op_binop(bitxor_values_no_opt);
        let rem = self.a64_op_binop(rem_values_no_opt);
        let pow = self.a64_op_binop(pow_values_no_opt);
        let shl = self.a64_op_binop(shl_values_no_opt);
        let shr = self.a64_op_binop(shr_values_no_opt);
        self.dispatch[160] = add;
        self.dispatch[161] = sub;
        self.dispatch[162] = mul;
        self.dispatch[163] = div;
        self.dispatch[164] = bitor;
        self.dispatch[165] = bitand;
        self.dispatch[166] = bitxor;
        self.dispatch[167] = rem;
        self.dispatch[168] = pow;
        self.dispatch[169] = shl;
        self.dispatch[170] = shr;

        let eq = self.a64_op_cmp_no_opt(cmp_eq_values_no_opt as *const () as u64);
        let ne = self.a64_op_cmp_no_opt(cmp_ne_values_no_opt as *const () as u64);
        let lt = self.a64_op_cmp_no_opt(cmp_lt_values_no_opt as *const () as u64);
        let le = self.a64_op_cmp_no_opt(cmp_le_values_no_opt as *const () as u64);
        let gt = self.a64_op_cmp_no_opt(cmp_gt_values_no_opt as *const () as u64);
        let ge = self.a64_op_cmp_no_opt(cmp_ge_values_no_opt as *const () as u64);
        let teq = self.a64_op_cmp_no_opt(cmp_teq_values_no_opt as *const () as u64);
        self.dispatch[140] = eq;
        self.dispatch[141] = ne;
        self.dispatch[142] = lt;
        self.dispatch[143] = le;
        self.dispatch[144] = gt;
        self.dispatch[145] = ge;
        self.dispatch[146] = teq;
        self.dispatch[150] = eq;
        self.dispatch[151] = ne;
        self.dispatch[152] = lt;
        self.dispatch[153] = le;
        self.dispatch[154] = gt;
        self.dispatch[155] = ge;
        self.dispatch[156] = teq;

        let pos = self.a64_op_unop(pos_value_no_opt as *const () as u64);
        let neg = self.a64_op_unop(neg_value_no_opt as *const () as u64);
        let bitnot = self.a64_op_unop(bitnot_value_no_opt as *const () as u64);
        let not = self.a64_op_unop(not_value_no_opt as *const () as u64);
        self.dispatch[121] = pos;
        self.dispatch[122] = neg;
        self.dispatch[123] = bitnot;
        self.dispatch[124] = not;

        // Publish the freshly-emitted no-opt replacements: on macOS/aarch64
        // this flips the MAP_JIT pages back to executable so the dispatch
        // table entries above can actually be jumped to. Mirrors the
        // matching `self.jit.finalize()` at the tail of vmgen.rs's x86-64
        // `remove_vm_bop_optimization`; without it, BOP-redefinition tests
        // SIGBUS on Apple Silicon.
        self.jit.finalize();
    }
}
