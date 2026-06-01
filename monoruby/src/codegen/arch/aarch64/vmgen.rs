//! aarch64 VM construction (`construct_vm`), every bytecode opcode
//! handler (`a64_op_*`), and BOP de-optimization.
//!
//! Counterpart of `arch/x86_64/vmgen.rs` (+ its `vmgen/` submodules).

use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {
    pub(in crate::codegen) fn construct_vm(&mut self) {
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
        self.dispatch[6] = immediate;
        self.dispatch[7] = literal;
        self.dispatch[178] = mov;
        self.dispatch[80] = ret;
        self.dispatch[172] = init_method;
        self.dispatch[160] = add_rr;
        self.dispatch[161] = sub_rr;
        self.dispatch[162] = mul_rr;
        self.dispatch[163] = div_rr;

        // loop_start (14) / loop_end (15): in the VM-only build these just
        // advance to the next instruction. TODO(aarch64): the GC poll in
        // loop_start (vm_execute_gc) — currently skipped (works with --no-gc).
        let loop_op = self.a64_op_loop();
        self.dispatch[14] = loop_op;
        self.dispatch[15] = loop_op;

        // branches (the shared `branch` target lives inside `br_inst`).
        let (br_inst, branch) = self.a64_op_br();
        let condbr = self.a64_op_condbr(&branch, false);
        let condnotbr = self.a64_op_condbr(&branch, true);
        self.dispatch[3] = br_inst;
        self.dispatch[4] = condbr;
        self.dispatch[5] = condnotbr;
        self.dispatch[12] = condbr;
        self.dispatch[13] = condnotbr;
        let check_local = self.a64_op_check_local(&branch);
        self.dispatch[20] = check_local;
        let nilbr = self.a64_op_nilbr(&branch);
        self.dispatch[37] = nilbr;
        let optcase = self.a64_op_optcase(&branch);
        self.dispatch[36] = optcase;
        let lambda = self.a64_op_lambda();
        self.dispatch[38] = lambda;

        // integer comparisons (fixnum fast path; generic runtime fallback)
        let eq = self.a64_op_cmp(Cond::Eq, cmp_eq_values as *const () as u64);
        let ne = self.a64_op_cmp(Cond::Ne, cmp_ne_values as *const () as u64);
        let lt = self.a64_op_cmp(Cond::Lt, cmp_lt_values as *const () as u64);
        let le = self.a64_op_cmp(Cond::Le, cmp_le_values as *const () as u64);
        let gt = self.a64_op_cmp(Cond::Gt, cmp_gt_values as *const () as u64);
        let ge = self.a64_op_cmp(Cond::Ge, cmp_ge_values as *const () as u64);
        let teq = self.a64_op_cmp(Cond::Eq, cmp_teq_values as *const () as u64);
        self.dispatch[140] = eq;
        self.dispatch[141] = ne;
        self.dispatch[142] = lt;
        self.dispatch[143] = le;
        self.dispatch[144] = gt;
        self.dispatch[145] = ge;
        self.dispatch[146] = teq; // teq
        // 150-156: same comparisons, emitted when the result feeds a branch.
        let method_def = self.a64_op_method_def();
        self.dispatch[2] = method_def;
        let send_simple = self.a64_op_send(true);
        let send = self.a64_op_send(false);
        self.dispatch[30] = send_simple;
        self.dispatch[31] = send;
        self.dispatch[32] = send_simple;
        self.dispatch[33] = send;

        let yield_op = self.a64_op_yield();
        self.dispatch[34] = yield_op;
        self.dispatch[35] = yield_op;

        // break / raise / retry / redo / ensure-end: set an error and route
        // through entry_raise, which handle_error turns into the right control
        // flow (break value / re-raise / retry / redo).
        let method_ret = self.a64_op_err1(runtime::err_method_return as *const () as u64, true);
        let block_break = self.a64_op_err1(runtime::err_block_break as *const () as u64, true);
        let raise_err = self.a64_op_err_raise();
        let retry_op = self.a64_op_err1(runtime::err_retry as *const () as u64, false);
        let redo_op = self.a64_op_err1(runtime::err_redo as *const () as u64, false);
        let ensure_end = self.a64_op_ensure_end();
        self.dispatch[81] = method_ret;
        self.dispatch[82] = block_break;
        self.dispatch[83] = raise_err;
        self.dispatch[84] = retry_op;
        self.dispatch[85] = ensure_end;
        self.dispatch[87] = redo_op;

        self.dispatch[150] = eq;
        self.dispatch[151] = ne;
        self.dispatch[152] = lt;
        self.dispatch[153] = le;
        self.dispatch[154] = gt;
        self.dispatch[155] = ge;
        self.dispatch[156] = teq; // teq

        let class_def = self.a64_op_class_def(false);
        let module_def = self.a64_op_class_def(true);
        let singleton_class_def = self.a64_op_singleton_class_def();
        self.dispatch[70] = class_def;
        self.dispatch[71] = module_def;
        self.dispatch[22] = singleton_class_def;

        let load_const = self.a64_op_load_const(runtime::vm_get_constant as *const () as u64);
        let check_const = self.a64_op_load_const(runtime::vm_check_constant as *const () as u64);
        let store_const = self.a64_op_store_const();
        self.dispatch[10] = load_const;
        self.dispatch[18] = check_const;
        self.dispatch[11] = store_const;

        let load_ivar = self.a64_op_load_ivar();
        let store_ivar = self.a64_op_store_ivar();
        self.dispatch[16] = load_ivar;
        self.dispatch[17] = store_ivar;

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
        self.dispatch[64] = defined_yield;
        self.dispatch[65] = defined_const;
        self.dispatch[66] = defined_method;
        self.dispatch[67] = defined_gvar;
        self.dispatch[68] = defined_ivar;
        self.dispatch[69] = defined_super;
        self.dispatch[88] = defined_cvar;

        // literal constructors / aggregate ops
        let array = self.a64_op_array();
        let array_teq = self.a64_op_array_teq();
        let hash = self.a64_op_hash();
        let concat = self.a64_op_concat(runtime::concatenate_string as *const () as u64);
        let concat_regexp = self.a64_op_concat(runtime::concatenate_regexp as *const () as u64);
        let range_incl = self.a64_op_range(false);
        let range_excl = self.a64_op_range(true);
        let expand_array = self.a64_op_expand_array();
        self.dispatch[39] = array;
        self.dispatch[40] = array_teq;
        self.dispatch[176] = hash;
        self.dispatch[181] = concat;
        self.dispatch[86] = concat_regexp;
        self.dispatch[179] = range_incl;
        self.dispatch[180] = range_excl;
        self.dispatch[173] = expand_array;

        let index = self.a64_op_index();
        let index_assign = self.a64_op_index_assign();
        self.dispatch[132] = index;
        self.dispatch[133] = index_assign;

        let singleton_method_def = self.a64_op_singleton_method_def();
        self.dispatch[1] = singleton_method_def;

        let alias_method = self.a64_op_alias_method();
        let undef_method = self.a64_op_undef_method();
        self.dispatch[175] = alias_method;
        self.dispatch[174] = undef_method;

        let load_gvar = self.a64_op_load_gvar();
        let store_gvar = self.a64_op_store_var(runtime::set_global_var as *const () as u64);
        let load_cvar = self.a64_op_load_cvar();
        let store_cvar = self.a64_op_store_var(runtime::set_class_var as *const () as u64);
        let alias_gvar = self.a64_op_alias_gvar();
        self.dispatch[25] = load_gvar;
        self.dispatch[26] = store_gvar;
        self.dispatch[27] = load_cvar;
        self.dispatch[28] = alias_gvar;
        self.dispatch[29] = store_cvar;

        let block_arg = self.a64_op_block_arg();
        let check_cvar = self.a64_op_check_cvar();
        let check_kw_rest = self.a64_op_check_kw_rest();
        self.dispatch[23] = block_arg;
        self.dispatch[24] = check_cvar;
        self.dispatch[19] = check_kw_rest;

        let load_dvar = self.a64_op_load_dvar();
        let store_dvar = self.a64_op_store_dvar();
        self.dispatch[148] = load_dvar;
        self.dispatch[149] = store_dvar;

        let block_arg_proxy = self.a64_op_block_arg_proxy();
        self.dispatch[21] = block_arg_proxy;

        let to_a = self.a64_op_to_a();
        self.dispatch[177] = to_a;

        // remaining binary operators (ops 164-170): bitor/bitand/bitxor/
        // rem/pow/shl/shr -- no fixnum fast path, straight to the runtime op.
        let bitor = self.a64_op_binop(bitor_values);
        let bitand = self.a64_op_binop(bitand_values);
        let bitxor = self.a64_op_binop(bitxor_values);
        let rem = self.a64_op_binop(rem_values);
        let pow = self.a64_op_binop(pow_values);
        let shl = self.a64_op_binop(shl_values);
        let shr = self.a64_op_binop(shr_values);
        self.dispatch[164] = bitor;
        self.dispatch[165] = bitand;
        self.dispatch[166] = bitxor;
        self.dispatch[167] = rem;
        self.dispatch[168] = pow;
        self.dispatch[169] = shl;
        self.dispatch[170] = shr;

        // unary operators (ops 121-124): pos, neg, bitnot, not
        let pos = self.a64_op_unop(pos_value as *const () as u64);
        let neg = self.a64_op_unop(neg_value as *const () as u64);
        let bitnot = self.a64_op_unop(bitnot_value as *const () as u64);
        let not = self.a64_op_unop(not_value as *const () as u64);
        self.dispatch[121] = pos;
        self.dispatch[122] = neg;
        self.dispatch[123] = bitnot;
        self.dispatch[124] = not;
    }

    /// ops 121-124 `UnOp` (pos/neg/bitnot/not): fn(vm, globals, src `[pc+2]`)
    /// -> dst `[pc+4]`.
    pub(in crate::codegen) fn a64_op_unop(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // src
        monoasm_arm64!(&mut self.jit,
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
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x(LFP.0), [x10];  // restore (possibly heap-promoted) LFP
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
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
            sub x12, x10, #(LFP_BLOCK as u32);
            ldr x10, [x12];
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
            sub x12, x12, #(LFP_SELF as u32);
            str x10, [x12];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 148 `LoadDynVar`: dst `[pc+4]` <- the slot `[pc+2]` of the outer
    /// frame `[pc+0]` levels up the captured outer chain.
    pub(in crate::codegen) fn a64_op_load_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(LFP.0)];  // X10 = level-1 outer ([LFP] = LFP_OUTER)
            ldrh x11, [x(PC.0)];  // outer level
            loop_:
            subs x11, x11, #(1);
        );
        self.jit.bcond_label(Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x10];  // walk up
            b loop_;
            exit:
            cbz x10, raise;
            ldrh x12, [x(PC.0), #(2)];  // src slot in outer frame
            neg x12, x12;
            add x13, x10, x12, lsl #(3);
            sub x13, x13, #(LFP_SELF as u32);
            ldr x14, [x13];  // value
        // store to dst [pc+4]
            ldrh x12, [x(PC.0), #(4)];
            cbz x12, skip;
            neg x12, x12;
            add x13, x(LFP.0), x12, lsl #(3);
            sub x13, x13, #(LFP_SELF as u32);
            str x14, [x13];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 149 `StoreDynVar`: the slot `[pc+4]` of the outer frame `[pc+2]`
    /// levels up <- src slot `[pc+0]` of the current frame.
    pub(in crate::codegen) fn a64_op_store_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(LFP.0)];  // level-1 outer
            ldrh x11, [x(PC.0), #(2)];  // outer level
            loop_:
            subs x11, x11, #(1);
        );
        self.jit.bcond_label(Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x10];
            b loop_;
            exit:
        // src value from the current frame (slot [pc+0])
            ldrh x12, [x(PC.0)];
            neg x12, x12;
            add x13, x(LFP.0), x12, lsl #(3);
            sub x13, x13, #(LFP_SELF as u32);
            ldr x14, [x13];
        // store to dst slot [pc+4] in the outer frame
            ldrh x12, [x(PC.0), #(4)];
            neg x12, x12;
            add x13, x10, x12, lsl #(3);
            sub x13, x13, #(LFP_SELF as u32);
            str x14, [x13];
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

    /// op 24 `CheckCvar`: check_class_var(vm, globals, name `[pc+0]`) -> dst.
    pub(in crate::codegen) fn a64_op_check_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::check_class_var as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 25 `LoadGvar`: get_global_var(vm, globals, name `[pc+0]`) -> Value.
    pub(in crate::codegen) fn a64_op_load_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::get_global_var as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 27 `LoadCvar`: get_class_var(vm, globals, name `[pc+0]`) -> Option.
    pub(in crate::codegen) fn a64_op_load_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::get_class_var as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// ops 26/29 `StoreGvar`/`StoreCvar`: set_*_var(vm, globals, name `[pc+0]`,
    /// val `[pc+4]`) -> Option (error-only; no result slot).
    pub(in crate::codegen) fn a64_op_store_var(&mut self, set_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            ldrh x3, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X3); // val
        monoasm_arm64!(&mut self.jit,
            mov x9, (set_fn);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 28 `AliasGvar`: alias_global_var(globals, new `[pc+0]`, old `[pc+8]`).
    pub(in crate::codegen) fn a64_op_alias_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(GLOBALS.0);
            ldr w1, [x(PC.0)];  // new
            ldr w2, [x(PC.0), #(8)];  // old
            mov x9, (runtime::alias_global_var as *const () as u64);
            blr x9;
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

    /// op 1 `SingletonMethodDef`: `def obj.name` -- singleton_define_method(
    /// vm, globals, name `[pc+8]`, func_id `[pc+12]`, obj slot `[pc+4]`).
    pub(in crate::codegen) fn a64_op_singleton_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0), #(8)];  // name
            ldr w3, [x(PC.0), #(12)];  // func_id
            ldrh x4, [x(PC.0), #(4)];  // obj slot
        );
        self.a64_slot_value(X4); // obj
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::singleton_define_method as *const () as u64);
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
            ldrh x2, [x(PC.0), #(2)];
        );
        self.a64_slot_value(X2); // base
        monoasm_arm64!(&mut self.jit,
            ldrh x3, [x(PC.0)];
        );
        self.a64_slot_value(X3); // idx
        monoasm_arm64!(&mut self.jit,
            add x4, x(PC.0), #(8);  // &cache
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
            ldrh x2, [x(PC.0), #(2)];
        );
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
            add x5, x(PC.0), #(8);  // &cache
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
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
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
    pub(in crate::codegen) fn a64_op_array_teq(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];  // rhs slot
            ldrh x11, [x(PC.0), #(2)];  // lhs slot (also dst)
        );
        self.a64_load_slot(X11, X3, X12); // X3 = lhs value
        self.a64_load_slot(X10, X4, X12); // X4 = rhs value
        // array_teq(vm, globals, lhs, rhs) -> Option<Value>
        monoasm_arm64!(&mut self.jit,
            mov x2, x3;  // lhs (arg #3)
            mov x3, x4;  // rhs (arg #4)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::array_teq as *const () as u64);
            blr x9;
            cbz x0, raise;
        // dst slot = lhs slot from [PC+2]
            ldrh x10, [x(PC.0), #(2)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
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

    /// op 173 `ExpandArray`: expand_array(src `[pc+4]`, &dst `[pc+2]`,
    /// len `[pc+0]`, rest `[pc+8]`). No result.
    pub(in crate::codegen) fn a64_op_expand_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            ldrh x0, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X0); // src (an Array Value)
        monoasm_arm64!(&mut self.jit,
            ldrh x1, [x(PC.0), #(2)];
        );
        self.a64_slot_addr(X1); // &dst
        monoasm_arm64!(&mut self.jit,
            ldrh x2, [x(PC.0)];  // len
            ldrh x3, [x(PC.0), #(8)];  // rest
            mov x9, (runtime::expand_array as *const () as u64);
            blr x9;
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
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
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

    /// op 16 `LoadIvar`: slot[`[pc+4]`] <- `self.@name` (name `[pc+0]`),
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_load_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            sub x0, x(LFP.0), #(LFP_SELF as u32);
            ldr x0, [x0];  // base = self
            ldr w1, [x(PC.0)];  // name
            mov x2, x(GLOBALS.0);
            add x3, x(PC.0), #(8);  // &cache
            mov x9, (get_instance_var_with_cache as *const () as u64);
            blr x9;
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 17 `StoreIvar`: `self.@name` (name `[pc+0]`) <- slot[`[pc+4]`],
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_store_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            sub x2, x(LFP.0), #(LFP_SELF as u32);
            ldr x2, [x2];  // base = self
            ldr w3, [x(PC.0)];  // name
            ldrh x10, [x(PC.0), #(4)];  // src slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x4, x10;  // val
            add x5, x(PC.0), #(8);  // &cache
            mov x9, (set_instance_var_with_cache as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 10 `LoadConst`: slot[`[pc+4]`] <- constant at ConstSiteId `[pc+0]`.
    /// (x86 `vm_load_const`; the JIT inline-cache slot at `[pc+8]` is not
    /// written — the VM relies on the ConstSite cache + const_version.)
    pub(in crate::codegen) fn a64_op_load_const(&mut self, get_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0)];  // ConstSiteId
            mov x11, (cv_addr);
            ldr x3, [x11];  // const_version
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (get_fn);
            blr x9;
            cbz x0, raise;
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 11 `StoreConst`: define constant ConstSiteId `[pc+0]` <- slot
    /// `[pc+4]`, bumping const_version. (x86 `vm_store_const`.)
    pub(in crate::codegen) fn a64_op_store_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0)];  // ConstSiteId
            ldrh x10, [x(PC.0), #(4)];  // src slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x3, x10;  // val
        // const_version += 1
            mov x11, (cv_addr);
            ldr x12, [x11];
            add x12, x12, #(1);
            str x12, [x11];
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::set_constant as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 70 `ClassDef` / op 71 `ModuleDef`: define the class/module, then
    /// run its body as a method with the class as `self`. Bytecode (16B):
    /// `+0` superclass slot (0 = none), `+2` base slot (0 = none),
    /// `+4` dst, `+8` name (IdentId), `+12` func_id (class body).
    pub(in crate::codegen) fn a64_op_class_def(&mut self, is_module: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let sup_zero = self.jit.label();
        let sup_done = self.jit.label();
        let base_zero = self.jit.label();
        let base_done = self.jit.label();
        // define_class(vm, globals, name, superclass, is_module, base)
        // superclass (x3): slot[+0] value, or 0 (None) if slot index is 0.
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];
            cbz x10, sup_zero;
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x3, x10;
            b sup_done;
            sup_zero:
            mov x3, (0);
            sup_done:
        // base (x5): slot[+2] value, or 0 (None).
            ldrh x10, [x(PC.0), #(2)];
            cbz x10, base_zero;
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x5, x10;
            b base_done;
            base_zero:
            mov x5, (0);
            base_done:
            ldr w2, [x(PC.0), #(8)];  // name
            mov x4, (if is_module { 1 } else { 0 });
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::define_class as *const () as u64);
            blr x9;
            cbz x0, raise;
            mov x25, x0;  // X25 = self (the class), callee-saved
        );
        self.a64_class_def_run();
        p
    }

    /// op 22 `SingletonClassDef`: `class << base`. base = slot `[pc+0]`.
    pub(in crate::codegen) fn a64_op_singleton_class_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        // define_singleton_class(vm, globals, base) -> self
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0)];
        );
        self.a64_slot_value(X2); // base
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::define_singleton_class as *const () as u64);
            blr x9;
            cbz x0, raise;
            mov x25, x0;  // self = singleton class
        );
        self.a64_class_def_run();
        p
    }

    /// Shared tail of class/module/singleton-class definition: with `X25` set
    /// to the (singleton) class, run the class body (`enter_classdef` ->
    /// call_funcdata -> `exit_classdef`) and store the result to dst `[pc+4]`.
    /// func_id is read from `[pc+12]`.
    pub(in crate::codegen) fn a64_class_def_run(&mut self) {
        let skip = self.jit.label();
        let raise = self.entry_raise.clone();
        // enter_classdef(vm, globals, func_id, self) -> &FuncData
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0), #(12)];  // func_id
            mov x3, x25;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::enter_classdef as *const () as u64);
            blr x9;
            mov x26, x0;  // X26 = &FuncData, callee-saved
        // cont frame: save caller PC + ACC (the body clobbers them).
            sub sp, sp, #(16);
            str x(PC.0), [sp];
            str x(ACC.0), [sp, #(8)];
        // frame setup: zero outer/svar/cme/block; self = class; meta.
            mov x12, (0);
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_CME) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            str x25, [x11];  // self = class
            ldr x10, [x26, #(FUNCDATA_META as u32)];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_META) as u32);
            str x10, [x11];
        // call_funcdata: push frame, set lfp, pc, blr codeptr, restore cfp
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x(LFP.0), [x10];
            ldr x(PC.0), [x26, #(FUNCDATA_PC as u32)];
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;  // x0 = class body result
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        // restore caller LFP from its own frame (x29-relative)
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x(LFP.0), [x10];
            mov x25, x0;  // save result across exit_classdef
        // exit_classdef(vm, globals)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::exit_classdef as *const () as u64);
            blr x9;
            mov x0, x25;  // restore result
        // pop cont frame: restore PC + ACC
            ldr x(PC.0), [sp];
            ldr x(ACC.0), [sp, #(8)];
            add sp, sp, #(16);
        // If the class/module body raised, X0 is null: propagate the
        // error now (mirrors the trailing `vm_handle_error` in the x86
        // `class_def_sub`). Without this the exception is silently
        // dropped and left pending, tripping the `set_error` guard on the
        // next error. PC/ACC are already restored so entry_raise sees the
        // caller's frame, and exit_classdef above has popped the class
        // context.
            cbz x0, raise;
        // store result to dst [PC+4]
            ldrh x10, [x(PC.0), #(4)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
    }

    /// op 2 `method_def`: `define_method(vm, globals, name, func_id)`.
    /// Bytecode: `+8` name, `+12` func_id.
    pub(in crate::codegen) fn a64_op_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0), #(8)];  // name
            ldr w3, [x(PC.0), #(12)];  // func_id
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::define_method as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 30-33 `send`/`send_simple`: method call (slow-path only — no inline
    /// cache; `find_method` does the lookup every time). Handles the simple
    /// case (no kw/block/splat). Bytecode (32 bytes): `+0` callid, `+4` ret
    /// slot, `+8` pos_num, `+10` arg slot, `+12` recv slot.
    pub(in crate::codegen) fn a64_op_send(&mut self, is_simple: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let mm = self.jit.label();
        let argloop = self.jit.label();
        let argdone = self.jit.label();
        let generic = self.jit.label();
        let docall = self.jit.label();
        let skip = self.jit.label();
        let after_call = self.jit.label();
        let raise = self.entry_raise.clone();
        // Raise SystemStackError before pushing the new frame (so the caller's
        // LFP is still intact when entry_raise inspects it for a rescue).
        self.a64_check_stack();
        // GC + signal poll. The signal handler nudges alloc_flag by 10 so a
        // pending Signal.trap callback runs at this safepoint.
        self.a64_vm_execute_gc();
        // push_cont_frame: save caller PC (sp -= 16; [sp] = PC)
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(16);
            str x(PC.0), [sp];
        // receiver
            ldrh x10, [x(PC.0), #(12)];
        );
        self.a64_load_slot(X10, X4, X11); // X4 = recv
        // callee self slot
        monoasm_arm64!(&mut self.jit,
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            str x4, [x11];
        // find_method(vm, globals, callid, recv) -> funcid
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // callid
            mov x3, x4;  // recv
            mov x9, (runtime::find_method as *const () as u64);
            blr x9;
            cbz x0, mm;
        // get_func_data: X15 = funcinfo_base + funcid*64 + FUNCINFO_DATA
            lsl x10, x0, #(6);
            mov x11, (GLOBALS_FUNCINFO as u64);
            add x11, x(GLOBALS.0), x11;
            ldr x11, [x11];
            add x10, x10, x11;
            add x15, x10, #(FUNCINFO_DATA as u32);
        // set_method_outer: zero outer/svar/cme; set meta (kept in X14).
            mov x12, (0);
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_CME) as u32);
            str x12, [x11];
            ldr x14, [x15, #(FUNCDATA_META as u32)];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_META) as u32);
            str x14, [x11];
        // Simple-send opcodes (no block/splat/kw at the call site) may take
        // the fast positional-copy path when the callee is also simple and
        // arity matches. The full-send opcodes always go generic so that
        // set_frame_block / splat / keyword handling runs.
            ldrh x9, [x(PC.0), #(8)];  // pos_num
        );
        if is_simple {
            monoasm_arm64!(&mut self.jit,
                lsr x16, x14, #(56);  // kind byte
                tbz x16, #(4), generic;
                ldrh x16, [x15, #(FUNCDATA_MIN as u32)];
                cmp x9, x16;
            );
            self.jit.bcond_label(Cond::Ne, &generic);
        } else {
            monoasm_arm64!(&mut self.jit,
                b generic;
            );
        }
        // --- simple path: zero block + copy positional args directly ---
        monoasm_arm64!(&mut self.jit,
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
            str x12, [x11];  // block = 0
            ldrh x10, [x(PC.0), #(10)];  // arg slot
            neg x10, x10;
            add x10, x(LFP.0), x10, lsl #(3);
            sub x10, x10, #(LFP_SELF as u32);  // args base (caller)
            cbz x9, argdone;
            neg x9, x9;
            argloop:
            add x11, x10, x9, lsl #(3);
            ldr x12, [x11, #(8)];  // src = [base + i*8 + 8]
            sub x13, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            add x13, x13, x9, lsl #(3);
            str x12, [x13];  // dst = callee self slot + i*8
            add x9, x9, #(1);
            cbnz x9, argloop;
            argdone:
            b docall;
        // --- generic path: vm_handle_arguments(exec, globals, caller_lfp,
        // callee_lfp, callid). Handles rest/optional/keyword/splat + block. ---
            generic:
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);  // callee lfp
        // Reserve scratch below the callee frame (= ofs*16 + 16, 16-aligned)
        // so the C call's frame can't trample the callee frame being built.
        // Save the pre-reservation SP (X25) and funcdata ptr (X26) in
        // callee-saved registers (AAPCS64 preserves x19-x28); X15 is
        // caller-saved so it would otherwise be lost. Restore SP directly
        // from X25 afterwards.
            mov x25, sp;  // X25 = SP before reservation
            mov x26, x15;
            ldrh x10, [x15, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);  // caller lfp
            ldr w4, [x(PC.0)];  // callid
            mov x9, (runtime::vm_handle_arguments as *const () as u64);
            blr x9;
            mov x15, x26;  // restore funcdata ptr
            mov sp, x25;  // restore SP directly
            cbz x0, raise;
            docall:
        // call_funcdata: push_frame + set_lfp + pc + blr codeptr + restore cfp
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x(LFP.0), [x10];
        // 4th arg (X3) = call-site BytecodePtr, for with-pc builtins (x86
        // sets `rcx = r13 - 16`); aarch64 PC is already the call site.
            mov x3, x(PC.0);
            ldr x(PC.0), [x15, #(FUNCDATA_PC as u32)];
            ldr x10, [x15, #(FUNCDATA_CODEPTR as u32)];
            blr x10;
        // pop_frame: EXEC.cfp = (X29 - BP_CFP). Mirrors x86 `lea r14,[rbp-8]`
        // — set EXEC.cfp to the *address* of this frame's CFP descriptor (set
        // up by the caller's push_frame before our vm_entry). We must NOT
        // reload from `[SP - RSP_CFP]`: AAPCS64 has no red zone, so the inner
        // BLR's callee may use that slot as a local and clobber it. The
        // descriptor at `[X29 - BP_CFP]` lives in this frame's "header" area
        // (above the locals/LFP) and is safe across nested calls.
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        // restore caller LFP from its own frame (x86 `restore_lfp`):
        // LFP = [x29 - (BP_CFP + CFP_LFP)]. The callee clobbers LFP, so we
        // reload it from the caller's stable frame pointer (x29 == x86 rbp).
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x(LFP.0), [x10];
        // pop_cont_frame: restore PC, advance past the 32-byte send
            after_call:
            ldr x(PC.0), [sp];
            add sp, sp, #(16);
            cbz x0, raise;  // result 0 => error
            ldrh x10, [x(PC.0), #(4)];  // ret slot
            add x(PC.0), x(PC.0), #(32);
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
            skip:
        );
        self.a64_fetch_and_dispatch();
        // method_missing: invoke_method_missing(vm, globals, recv, lfp,
        // callid) -> Option, then join the result path. The receiver register
        // was clobbered by find_method, so reload it from the recv slot.
        // invoke_method_missing manages its own frames and preserves PC/LFP
        // (callee-saved), so no cfp/LFP restore is needed here.
        monoasm_arm64!(&mut self.jit,
            mm:
            ldrh x10, [x(PC.0), #(12)];  // recv slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x2, x10;  // receiver
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x3, x(LFP.0);
            ldr w4, [x(PC.0)];  // callid
            mov x9, (crate::codegen::runtime::invoke_method_missing as *const () as u64);
            blr x9;
            b after_call;
        );
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

    /// op 34/35 `Yield`: invoke the current block. Bytecode (32 bytes):
    /// `+0` callid, `+4` ret slot. The block's func/outer come from
    /// `get_yield_data`; self is the block's captured self. Args are set up
    /// via the runtime arg massager (callsite-driven).
    pub(in crate::codegen) fn a64_op_yield(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        // push_cont_frame: save caller PC
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(16);
            str x(PC.0), [sp];
        // get_yield_data(vm, globals) -> x0 = outer (Lfp), x1 = func_id
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::get_yield_data as *const () as u64);
            blr x9;
            cbz x1, raise;  // no block -> error set
            mov x25, x0;  // X25 = outer (callee-saved across later calls)
        // get_func_data from func_id (X1) -> X15
            lsl x10, x1, #(32);
            lsr x10, x10, #(32);
            lsl x10, x10, #(6);
            mov x11, (GLOBALS_FUNCINFO as u64);
            add x11, x(GLOBALS.0), x11;
            ldr x11, [x11];
            add x10, x10, x11;
            add x15, x10, #(FUNCINFO_DATA as u32);
        // block frame setup: outer = X25, self = outer.self, svar/cme/block 0.
            mov x12, (0);
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
            str x25, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_CME) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
            str x12, [x11];
            sub x10, x25, #(LFP_SELF as u32);
            ldr x10, [x10];  // self = outer.self
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            str x10, [x11];
            ldr x14, [x15, #(FUNCDATA_META as u32)];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_META) as u32);
            str x14, [x11];
        // generic arg setup: vm_handle_arguments(vm, globals, caller_lfp,
        // callee_lfp, callid). Reserve scratch; preserve SP/funcdata.
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);  // callee_lfp
            mov x25, sp;
            mov x26, x15;
            ldrh x10, [x15, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);  // caller lfp
            ldr w4, [x(PC.0)];  // callid
            mov x9, (runtime::vm_handle_arguments as *const () as u64);
            blr x9;
            mov x15, x26;
            mov sp, x25;
            cbz x0, raise;
        // call_funcdata
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x(LFP.0), [x10];
            mov x3, x(PC.0);  // call-site pc for with-pc builtins
            ldr x(PC.0), [x15, #(FUNCDATA_PC as u32)];
            ldr x10, [x15, #(FUNCDATA_CODEPTR as u32)];
            blr x10;
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x(LFP.0), [x10];  // restore caller LFP
        // pop_cont_frame + store result to ret slot [pc+4]
            ldr x(PC.0), [sp];
            add sp, sp, #(16);
            cbz x0, raise;
            ldrh x10, [x(PC.0), #(4)];  // ret slot
            add x(PC.0), x(PC.0), #(32);
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            sub x11, x11, #(LFP_SELF as u32);
            str x0, [x11];
            skip:
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// loop_start / loop_end (ops 14/15): advance + dispatch (VM-only).
    pub(in crate::codegen) fn a64_op_loop(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
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
            sub x10, x10, #(LFP_SELF as u32);
            str x13, [x10];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        monoasm_arm64!(&mut self.jit,
            generic:
        // cmp_*_values(vm, globals, lhs=X13, rhs=X14) -> Option<Value>
            mov x2, x13;  // lhs
            mov x3, x14;  // rhs
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

    pub(in crate::codegen) fn a64_generic_binop(&mut self, func: BinaryOpFn) {
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x2, x13;  // lhs
            mov x3, x14;  // rhs
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
            sub x10, x10, #(LFP_SELF as u32);
            str x9, [x10];
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

    /// op 172 `init_method`: allocate the method's stack frame and nil-fill the
    /// uninitialized local slots. Bytecode (relative to instruction start):
    /// `+0` stack-offset, `+2` arg_num, `+4` reg_num. (x86 `vm_init`.)
    pub(in crate::codegen) fn a64_op_init_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        let loop_ = self.jit.label();
        // allocate stack: sp -= stack_offset * 16
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];
            lsl x10, x10, #(4);
            mov x13, sp;  // sp -= X10 (A64 sub can't take SP as a
            sub x13, x13, x10;  // shifted-reg operand, so via a GPR)
            mov sp, x13;
        // Skip the nil-fill for a captured (on-heap / invalidated) frame.
        // Its locals live on the heap and may already hold values written in
        // by `new_binding_frame` — e.g. a binding-eval frame that introduced a
        // brand-new local in a previous eval; nil-filling would wipe it.
        // Mirrors x86 `fill_nil`'s leading `branch_if_captured`. The `kind`
        // byte sits at `[LFP - (LFP_META - META_KIND)]`; bit 7 = on_heap,
        // bit 3 = invalidated.
            sub x10, x(LFP.0), #((LFP_META - META_KIND as i32) as u32);
            ldrb x13, [x10];
            tbnz x13, #(7), skip;  // on_heap
            tbnz x13, #(3), skip;  // invalidated
        // count = reg_num - arg_num
            ldrh x15, [x(PC.0), #(4)];  // reg_num
            ldrh x11, [x(PC.0), #(2)];  // arg_num
            sub x12, x15, x11;
            cbz x12, skip;
        // base = lfp - reg_num*8 - LFP_ARG0 ; fill [base + count*8] downward
            neg x15, x15;
            add x15, x(LFP.0), x15, lsl #(3);
            sub x15, x15, #(LFP_ARG0 as u32);
            mov x14, (NIL_VALUE);
            loop_:
            add x10, x15, x12, lsl #(3);
            str x14, [x10];
            sub x12, x12, #(1);
            cbnz x12, loop_;
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
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
            sub x12, x12, #(LFP_SELF as u32);
            str x11, [x12];
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
            sub x12, x12, #(LFP_SELF as u32);
            str x0, [x12];
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
            sub x12, x12, #(LFP_SELF as u32);
            str x10, [x12];
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
            sub x11, x11, #(LFP_SELF as u32);
            ldr x0, [x11];  // return value
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
