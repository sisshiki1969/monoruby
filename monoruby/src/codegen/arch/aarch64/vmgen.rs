//! aarch64 VM construction (`construct_vm`), every bytecode opcode
//! handler (`a64_op_*`), and BOP de-optimization.
//!
//! Counterpart of `arch/x86_64/vmgen.rs` (+ its `vmgen/` submodules).

use super::*;

impl Codegen {
    pub(in crate::codegen) fn construct_vm(&mut self) {
        self.a64_gen_entry_raise();
        self.a64_gen_stack_overflow();
        self.a64_gen_exec_gc();
        let vm_entry = self.jit.label();
        let entry_fetch = self.jit.label();
        // vm_entry: establish the frame pointer (x86: `pushq rbp; movq rbp,rsp`).
        self.jit.bind_label(vm_entry.clone());
        self.jit.stp_pre(X29, X30, SP, -16);
        self.jit.mov_sp(X29, SP);
        self.jit.bind_label(entry_fetch.clone());
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
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // src
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 20 `CheckLocal`: branch by disp `[pc+0]` if local `[pc+4]` is set
    /// (non-zero); otherwise fall through (used for optional-param defaults).
    pub(in crate::codegen) fn a64_op_check_local(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (for the shared branch target)
        self.jit.ldrh(X12, PC, 4); // local slot
        self.a64_slot_value(X12);
        self.jit.cbnz_label(X12, branch);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 36 `OptCase`: dense `case`/`when` jump table. opt_case returns the
    /// branch displacement for cond slot `[pc+4]` against OptCaseId `[pc+0]`,
    /// which feeds the shared branch target.
    pub(in crate::codegen) fn a64_op_optcase(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldr32(X2, PC, 0); // OptCaseId
        self.jit.ldrh(X10, PC, 4); // cond slot
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10); // cond value
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::opt_case as *const () as u64);
        self.jit.blr(X9);
        self.jit.lsl_imm(X10, X0, 32); // zero-extend u32 disp into X10
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.b_label(branch);
        p
    }

    /// op 38 `Lambda`: dst `[pc+4]` <- a lambda Proc for func_id `[pc+0]`.
    /// gen_lambda may promote the current frame to the heap, so LFP is
    /// reloaded from the cfp afterward.
    pub(in crate::codegen) fn a64_op_lambda(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // func_id
        self.jit.mov(X3, PC); // call-site pc
        self.jit.mov_imm(X9, runtime::gen_lambda as *const () as u64);
        self.jit.blr(X9);
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0); // restore (possibly heap-promoted) LFP
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 37 `NilBr`: branch by disp `[pc+0]` if cond slot `[pc+4]` is nil.
    pub(in crate::codegen) fn a64_op_nilbr(&mut self, branch: &DestLabel) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (for shared branch target)
        self.jit.ldrh(X11, PC, 4); // cond slot
        self.a64_slot_value(X11);
        self.jit.cmp_imm(X11, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Eq, branch);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 23 `BlockArg`: block_arg(vm, globals, lfp, pc) -> dst `[pc+4]`.
    pub(in crate::codegen) fn a64_op_block_arg(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP);
        self.jit.mov(X3, PC); // BytecodePtr (instruction start)
        self.jit.mov_imm(X9, runtime::block_arg as *const () as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 177 `ToA`: dst `[pc+4]` <- `to_a(src `[pc+2]`)` (splat coercion).
    pub(in crate::codegen) fn a64_op_to_a(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // src
        self.jit.mov_imm(X9, runtime::to_a as *const () as u64);
        self.jit.blr(X9);
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
        self.jit.mov(X10, LFP);
        self.jit.ldr32(X11, PC, 0); // outer level
        self.jit.cbz_label(X11, &loop_exit);
        self.jit.bind_label(loop_.clone());
        self.jit.ldr(X10, X10, 0); // walk outer chain
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Ne, &loop_);
        self.jit.bind_label(loop_exit);
        // block handler = [outer - LFP_BLOCK]
        self.jit.sub_imm(X12, X10, LFP_BLOCK as u32, 0);
        self.jit.ldr(X10, X12, 0);
        self.jit.cbnz_label(X10, &notzero);
        self.jit.mov_imm(X10, NIL_VALUE); // no block -> nil
        self.jit.bind_label(notzero);
        // if bit0 == 0 (Proc/nil), keep as-is; else re-encode proxy depth.
        self.jit.tbz_label(X10, 0, &exit);
        self.jit.ldrsw(X12, PC, 0); // outer (signed)
        self.jit.lsl_imm(X12, X12, 2);
        self.jit.add(X10, X10, X12);
        self.jit.add_imm(X10, X10, 2, 0);
        self.jit.bind_label(exit);
        // store X10 to dst [pc+4]
        self.jit.ldrh(X11, PC, 4);
        self.jit.cbz_label(X11, &skip);
        self.jit.neg(X11, X11);
        self.jit.add_lsl(X12, LFP, X11, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X10, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.ldr(X10, LFP, 0); // X10 = level-1 outer ([LFP] = LFP_OUTER)
        self.jit.ldrh(X11, PC, 0); // outer level
        self.jit.bind_label(loop_.clone());
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Eq, &exit);
        self.jit.ldr(X10, X10, 0); // walk up
        self.jit.b_label(&loop_);
        self.jit.bind_label(exit);
        self.jit.cbz_label(X10, &raise);
        self.jit.ldrh(X12, PC, 2); // src slot in outer frame
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, X10, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.ldr(X14, X13, 0); // value
        // store to dst [pc+4]
        self.jit.ldrh(X12, PC, 4);
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, LFP, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.str(X14, X13, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 149 `StoreDynVar`: the slot `[pc+4]` of the outer frame `[pc+2]`
    /// levels up <- src slot `[pc+0]` of the current frame.
    pub(in crate::codegen) fn a64_op_store_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        self.jit.ldr(X10, LFP, 0); // level-1 outer
        self.jit.ldrh(X11, PC, 2); // outer level
        self.jit.bind_label(loop_.clone());
        self.jit.subs_imm(X11, X11, 1, 0);
        self.jit.bcond_label(Cond::Eq, &exit);
        self.jit.ldr(X10, X10, 0);
        self.jit.b_label(&loop_);
        self.jit.bind_label(exit);
        // src value from the current frame (slot [pc+0])
        self.jit.ldrh(X12, PC, 0);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, LFP, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.ldr(X14, X13, 0);
        // store to dst slot [pc+4] in the outer frame
        self.jit.ldrh(X12, PC, 4);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X13, X10, X12, 3);
        self.jit.sub_imm(X13, X13, LFP_SELF as u32, 0);
        self.jit.str(X14, X13, 0);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 19 `CheckKwRest`: if the kw-rest slot `[pc+4]` is nil, replace it
    /// with a fresh empty hash.
    pub(in crate::codegen) fn a64_op_check_kw_rest(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let exit = self.jit.label();
        self.jit.ldrh(X10, PC, 4);
        self.a64_slot_addr(X10); // &slot
        self.jit.ldr(X11, X10, 0);
        self.jit.cmp_imm(X11, NIL_VALUE as u32, 0);
        self.jit.bcond_label(Cond::Ne, &exit);
        self.jit.mov_imm(X9, runtime::empty_hash as *const () as u64);
        self.jit.blr(X9);
        self.jit.ldrh(X10, PC, 4);
        self.a64_slot_addr(X10); // re-compute (clobbered by call)
        self.jit.str(X0, X10, 0);
        self.jit.bind_label(exit);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 24 `CheckCvar`: check_class_var(vm, globals, name `[pc+0]`) -> dst.
    pub(in crate::codegen) fn a64_op_check_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::check_class_var as *const () as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 25 `LoadGvar`: get_global_var(vm, globals, name `[pc+0]`) -> Value.
    pub(in crate::codegen) fn a64_op_load_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::get_global_var as *const () as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 27 `LoadCvar`: get_class_var(vm, globals, name `[pc+0]`) -> Option.
    pub(in crate::codegen) fn a64_op_load_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::get_class_var as *const () as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// ops 26/29 `StoreGvar`/`StoreCvar`: set_*_var(vm, globals, name `[pc+0]`,
    /// val `[pc+4]`) -> Option (error-only; no result slot).
    pub(in crate::codegen) fn a64_op_store_var(&mut self, set_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.ldrh(X3, PC, 4);
        self.a64_slot_value(X3); // val
        self.jit.mov_imm(X9, set_fn);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 28 `AliasGvar`: alias_global_var(globals, new `[pc+0]`, old `[pc+8]`).
    pub(in crate::codegen) fn a64_op_alias_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, GLOBALS);
        self.jit.ldr32(X1, PC, 0); // new
        self.jit.ldr32(X2, PC, 8); // old
        self.jit.mov_imm(X9, runtime::alias_global_var as *const () as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 175 `AliasMethod`: alias_method(vm, globals, old `[pc+2]`,
    /// new `[pc+4]`).
    pub(in crate::codegen) fn a64_op_alias_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // old
        self.jit.ldrh(X3, PC, 4);
        self.a64_slot_value(X3); // new
        self.jit.mov_imm(X9, runtime::alias_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 174 `UndefMethod`: undef_method(vm, globals, name `[pc+0]`).
    pub(in crate::codegen) fn a64_op_undef_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // name
        self.jit.mov_imm(X9, runtime::undef_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 1 `SingletonMethodDef`: `def obj.name` -- singleton_define_method(
    /// vm, globals, name `[pc+8]`, func_id `[pc+12]`, obj slot `[pc+4]`).
    pub(in crate::codegen) fn a64_op_singleton_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.ldr32(X3, PC, 12); // func_id
        self.jit.ldrh(X4, PC, 4); // obj slot
        self.a64_slot_value(X4); // obj
        self.jit.mov_imm(X9, runtime::singleton_define_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 132 `Index`: dst[`[pc+4]`] <- base[`[pc+2]`][idx[`[pc+0]`]], with an
    /// inline ClassId cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_index(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // base
        self.jit.ldrh(X3, PC, 0);
        self.a64_slot_value(X3); // idx
        self.jit.add_imm(X4, PC, 8, 0); // &cache
        self.jit.mov_imm(X9, runtime::get_index as *const () as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 133 `IndexAssign`: base[`[pc+2]`][idx[`[pc+0]`]] <- src[`[pc+4]`],
    /// with an inline ClassId cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_index_assign(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_value(X2); // base
        self.jit.ldrh(X3, PC, 0);
        self.a64_slot_value(X3); // idx
        self.jit.ldrh(X4, PC, 4);
        self.a64_slot_value(X4); // src
        self.jit.add_imm(X5, PC, 8, 0); // &cache
        self.jit.mov_imm(X9, runtime::set_index as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store the Option<Value> result in X0 to the dst slot `[pc+4]`: branch
    /// to `raise` if it is 0 (error), else store, advance PC, and dispatch.
    pub(in crate::codegen) fn a64_checked_store_next(&mut self, raise: &DestLabel) {
        let skip = self.jit.label();
        self.jit.cbz_label(X0, raise);
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// op 39 `Array`: gen_array(vm, globals, callid `[pc+0]`, &self).
    pub(in crate::codegen) fn a64_op_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // callid
        self.jit.sub_imm(X3, LFP, LFP_SELF as u32, 0); // &self
        self.jit.mov_imm(X9, runtime::gen_array as *const () as u64);
        self.jit.blr(X9);
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
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot (also dst)
        self.a64_load_slot(X11, X3, X12); // X3 = lhs value
        self.a64_load_slot(X10, X4, X12); // X4 = rhs value
        // array_teq(vm, globals, lhs, rhs) -> Option<Value>
        self.jit.mov(X2, X3); // lhs (arg #3)
        self.jit.mov(X3, X4); // rhs (arg #4)
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::array_teq as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        // dst slot = lhs slot from [PC+2]
        self.jit.ldrh(X10, PC, 2);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 176 `Hash`: gen_hash(vm, globals, src `[pc+2]`, len `[pc+0]`).
    pub(in crate::codegen) fn a64_op_hash(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_addr(X2); // src
        self.jit.ldrh(X3, PC, 0); // len
        self.jit.mov_imm(X9, runtime::gen_hash as *const () as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 181 `ConcatStr` / op 86 `ConcatRegexp`: fn(vm, globals,
    /// args `[pc+2]`, len `[pc+0]`).
    pub(in crate::codegen) fn a64_op_concat(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 2);
        self.a64_slot_addr(X2); // args
        self.jit.ldrh(X3, PC, 0); // len
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 179/180 `Range`: gen_range(start `[pc+2]`, end `[pc+0]`, vm,
    /// globals, exclude_end).
    pub(in crate::codegen) fn a64_op_range(&mut self, exclude_end: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.ldrh(X0, PC, 2);
        self.a64_slot_value(X0); // start
        self.jit.ldrh(X1, PC, 0);
        self.a64_slot_value(X1); // end
        self.jit.mov(X2, EXEC);
        self.jit.mov(X3, GLOBALS);
        self.jit.mov_imm(X4, if exclude_end { 1 } else { 0 });
        self.jit.mov_imm(X9, runtime::gen_range as *const () as u64);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// op 173 `ExpandArray`: expand_array(src `[pc+4]`, &dst `[pc+2]`,
    /// len `[pc+0]`, rest `[pc+8]`). No result.
    pub(in crate::codegen) fn a64_op_expand_array(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X0, PC, 4);
        self.a64_slot_value(X0); // src (an Array Value)
        self.jit.ldrh(X1, PC, 2);
        self.a64_slot_addr(X1); // &dst
        self.jit.ldrh(X2, PC, 0); // len
        self.jit.ldrh(X3, PC, 8); // rest
        self.jit.mov_imm(X9, runtime::expand_array as *const () as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Store `X0` (a Value) into the dst slot at `[pc+4]`, advance PC, dispatch.
    pub(in crate::codegen) fn a64_store_dst_and_next(&mut self, skip: &DestLabel) {
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip.clone());
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// `defined?` ops 64/69 (yield/super): fn(vm, globals) -> Value, stored to
    /// the dst slot `[pc+4]`.
    pub(in crate::codegen) fn a64_op_defined_to_dst(&mut self, abs: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 65 `DefinedConst`: defined_const(vm, globals, &dst, site_id `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldr32(X3, PC, 8); // site_id
        self.jit.mov_imm(X9, runtime::defined_const as *const () as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 66 `DefinedMethod`: defined_method(vm, globals, &dst, recv `[pc+2]`,
    /// name `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_method(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldrh(X3, PC, 2);
        self.a64_slot_value(X3); // recv
        self.jit.ldr32(X4, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 67 `DefinedGvar`: defined_gvar(vm, globals, name `[pc+8]`) -> Value.
    pub(in crate::codegen) fn a64_op_defined_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_gvar as *const () as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 88 `DefinedCvar`: defined_cvar(vm, globals, name `[pc+8]`) -> Value.
    pub(in crate::codegen) fn a64_op_defined_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_cvar as *const () as u64);
        self.jit.blr(X9);
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 68 `DefinedIvar`: defined_ivar(vm, globals, &dst, name `[pc+8]`).
    pub(in crate::codegen) fn a64_op_defined_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 4);
        self.a64_slot_addr(X2); // &dst
        self.jit.ldr32(X3, PC, 8); // name
        self.jit.mov_imm(X9, runtime::defined_ivar as *const () as u64);
        self.jit.blr(X9);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 16 `LoadIvar`: slot[`[pc+4]`] <- `self.@name` (name `[pc+0]`),
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_load_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.sub_imm(X0, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X0, 0); // base = self
        self.jit.ldr32(X1, PC, 0); // name
        self.jit.mov(X2, GLOBALS);
        self.jit.add_imm(X3, PC, 8, 0); // &cache
        self.jit
            .mov_imm(X9, get_instance_var_with_cache as *const () as u64);
        self.jit.blr(X9);
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 17 `StoreIvar`: `self.@name` (name `[pc+0]`) <- slot[`[pc+4]`],
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_store_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.sub_imm(X2, LFP, LFP_SELF as u32, 0);
        self.jit.ldr(X2, X2, 0); // base = self
        self.jit.ldr32(X3, PC, 0); // name
        self.jit.ldrh(X10, PC, 4); // src slot
        self.a64_slot_value(X10);
        self.jit.mov(X4, X10); // val
        self.jit.add_imm(X5, PC, 8, 0); // &cache
        self.jit
            .mov_imm(X9, set_instance_var_with_cache as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.ldr32(X2, PC, 0); // ConstSiteId
        self.jit.mov_imm(X11, cv_addr);
        self.jit.ldr(X3, X11, 0); // const_version
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, get_fn);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.ldrh(X10, PC, 4); // dst slot
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 11 `StoreConst`: define constant ConstSiteId `[pc+0]` <- slot
    /// `[pc+4]`, bumping const_version. (x86 `vm_store_const`.)
    pub(in crate::codegen) fn a64_op_store_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        self.jit.ldr32(X2, PC, 0); // ConstSiteId
        self.jit.ldrh(X10, PC, 4); // src slot
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10); // val
        // const_version += 1
        self.jit.mov_imm(X11, cv_addr);
        self.jit.ldr(X12, X11, 0);
        self.jit.add_imm(X12, X12, 1, 0);
        self.jit.str(X12, X11, 0);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::set_constant as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.ldrh(X10, PC, 0);
        self.jit.cbz_label(X10, &sup_zero);
        self.a64_slot_value(X10);
        self.jit.mov(X3, X10);
        self.jit.b_label(&sup_done);
        self.jit.bind_label(sup_zero);
        self.jit.mov_imm(X3, 0);
        self.jit.bind_label(sup_done);
        // base (x5): slot[+2] value, or 0 (None).
        self.jit.ldrh(X10, PC, 2);
        self.jit.cbz_label(X10, &base_zero);
        self.a64_slot_value(X10);
        self.jit.mov(X5, X10);
        self.jit.b_label(&base_done);
        self.jit.bind_label(base_zero);
        self.jit.mov_imm(X5, 0);
        self.jit.bind_label(base_done);
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.mov_imm(X4, if is_module { 1 } else { 0 });
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::define_class as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.mov(X25, X0); // X25 = self (the class), callee-saved
        self.a64_class_def_run();
        p
    }

    /// op 22 `SingletonClassDef`: `class << base`. base = slot `[pc+0]`.
    pub(in crate::codegen) fn a64_op_singleton_class_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        // define_singleton_class(vm, globals, base) -> self
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldrh(X2, PC, 0);
        self.a64_slot_value(X2); // base
        self.jit.mov_imm(X9, runtime::define_singleton_class as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.mov(X25, X0); // self = singleton class
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
        self.jit.ldr32(X2, PC, 12); // func_id
        self.jit.mov(X3, X25);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::enter_classdef as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov(X26, X0); // X26 = &FuncData, callee-saved
        // cont frame: save caller PC + ACC (the body clobbers them).
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        self.jit.str(ACC, SP, 8);
        // frame setup: zero outer/svar/cme/block; self = class; meta.
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X25, X11, 0); // self = class
        self.jit.ldr(X10, X26, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X10, X11, 0);
        // call_funcdata: push frame, set lfp, pc, blr codeptr, restore cfp
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.ldr(PC, X26, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X26, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10); // x0 = class body result
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // restore caller LFP from its own frame (x29-relative)
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0);
        self.jit.mov(X25, X0); // save result across exit_classdef
        // exit_classdef(vm, globals)
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::exit_classdef as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov(X0, X25); // restore result
        // pop cont frame: restore PC + ACC
        self.jit.ldr(PC, SP, 0);
        self.jit.ldr(ACC, SP, 8);
        self.jit.add_imm(SP, SP, 16, 0);
        // If the class/module body raised, X0 is null: propagate the
        // error now (mirrors the trailing `vm_handle_error` in the x86
        // `class_def_sub`). Without this the exception is silently
        // dropped and left pending, tripping the `set_error` guard on the
        // next error. PC/ACC are already restored so entry_raise sees the
        // caller's frame, and exit_classdef above has popped the class
        // context.
        self.jit.cbz_label(X0, &raise);
        // store result to dst [PC+4]
        self.jit.ldrh(X10, PC, 4);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
    }

    /// op 2 `method_def`: `define_method(vm, globals, name, func_id)`.
    /// Bytecode: `+8` name, `+12` func_id.
    pub(in crate::codegen) fn a64_op_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.ldr32(X2, PC, 8); // name
        self.jit.ldr32(X3, PC, 12); // func_id
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, runtime::define_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        // receiver
        self.jit.ldrh(X10, PC, 12);
        self.a64_load_slot(X10, X4, X11); // X4 = recv
        // callee self slot
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X4, X11, 0);
        // find_method(vm, globals, callid, recv) -> funcid
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.ldr32(X2, PC, 0); // callid
        self.jit.mov(X3, X4); // recv
        self.jit.mov_imm(X9, runtime::find_method as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X0, &mm);
        // get_func_data: X15 = funcinfo_base + funcid*64 + FUNCINFO_DATA
        self.jit.lsl_imm(X10, X0, 6);
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X15, X10, FUNCINFO_DATA as u32, 0);
        // set_method_outer: zero outer/svar/cme; set meta (kept in X14).
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.ldr(X14, X15, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X14, X11, 0);
        // Simple-send opcodes (no block/splat/kw at the call site) may take
        // the fast positional-copy path when the callee is also simple and
        // arity matches. The full-send opcodes always go generic so that
        // set_frame_block / splat / keyword handling runs.
        self.jit.ldrh(X9, PC, 8); // pos_num
        if is_simple {
            self.jit.lsr_imm(X16, X14, 56); // kind byte
            self.jit.tbz_label(X16, 4, &generic);
            self.jit.ldrh(X16, X15, FUNCDATA_MIN as u32);
            self.jit.cmp(X9, X16);
            self.jit.bcond_label(Cond::Ne, &generic);
        } else {
            self.jit.b_label(&generic);
        }
        // --- simple path: zero block + copy positional args directly ---
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0); // block = 0
        self.jit.ldrh(X10, PC, 10); // arg slot
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X10, LFP, X10, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0); // args base (caller)
        self.jit.cbz_label(X9, &argdone);
        self.jit.neg(X9, X9);
        self.jit.bind_label(argloop.clone());
        self.jit.add_lsl(X11, X10, X9, 3);
        self.jit.ldr(X12, X11, 8); // src = [base + i*8 + 8]
        self.jit.sub_imm(X13, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.add_lsl(X13, X13, X9, 3);
        self.jit.str(X12, X13, 0); // dst = callee self slot + i*8
        self.jit.add_imm(X9, X9, 1, 0);
        self.jit.cbnz_label(X9, &argloop);
        self.jit.bind_label(argdone);
        self.jit.b_label(&docall);
        // --- generic path: vm_handle_arguments(exec, globals, caller_lfp,
        // callee_lfp, callid). Handles rest/optional/keyword/splat + block. ---
        self.jit.bind_label(generic);
        self.jit.sub_imm(X3, SP, RSP_LOCAL_FRAME as u32, 0); // callee lfp
        // Reserve scratch below the callee frame (= ofs*16 + 16, 16-aligned)
        // so the C call's frame can't trample the callee frame being built.
        // Save the pre-reservation SP (X25) and funcdata ptr (X26) in
        // callee-saved registers (AAPCS64 preserves x19-x28); X15 is
        // caller-saved so it would otherwise be lost. Restore SP directly
        // from X25 afterwards.
        self.jit.mov_sp(X25, SP); // X25 = SP before reservation
        self.jit.mov(X26, X15);
        self.jit.ldrh(X10, X15, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP); // caller lfp
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit.mov_imm(X9, runtime::vm_handle_arguments as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov(X15, X26); // restore funcdata ptr
        self.jit.mov_sp(SP, X25); // restore SP directly
        self.jit.cbz_label(X0, &raise);
        self.jit.bind_label(docall);
        // call_funcdata: push_frame + set_lfp + pc + blr codeptr + restore cfp
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        // 4th arg (X3) = call-site BytecodePtr, for with-pc builtins (x86
        // sets `rcx = r13 - 16`); aarch64 PC is already the call site.
        self.jit.mov(X3, PC);
        self.jit.ldr(PC, X15, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X15, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        // pop_frame: EXEC.cfp = (X29 - BP_CFP). Mirrors x86 `lea r14,[rbp-8]`
        // — set EXEC.cfp to the *address* of this frame's CFP descriptor (set
        // up by the caller's push_frame before our vm_entry). We must NOT
        // reload from `[SP - RSP_CFP]`: AAPCS64 has no red zone, so the inner
        // BLR's callee may use that slot as a local and clobber it. The
        // descriptor at `[X29 - BP_CFP]` lives in this frame's "header" area
        // (above the locals/LFP) and is safe across nested calls.
        self.jit.sub_imm(X10, X29, BP_CFP as u32, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        // restore caller LFP from its own frame (x86 `restore_lfp`):
        // LFP = [x29 - (BP_CFP + CFP_LFP)]. The callee clobbers LFP, so we
        // reload it from the caller's stable frame pointer (x29 == x86 rbp).
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0);
        // pop_cont_frame: restore PC, advance past the 32-byte send
        self.jit.bind_label(after_call.clone());
        self.jit.ldr(PC, SP, 0);
        self.jit.add_imm(SP, SP, 16, 0);
        self.jit.cbz_label(X0, &raise); // result 0 => error
        self.jit.ldrh(X10, PC, 4); // ret slot
        self.jit.add_imm(PC, PC, 32, 0);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.a64_fetch_and_dispatch();
        // method_missing: invoke_method_missing(vm, globals, recv, lfp,
        // callid) -> Option, then join the result path. The receiver register
        // was clobbered by find_method, so reload it from the recv slot.
        // invoke_method_missing manages its own frames and preserves PC/LFP
        // (callee-saved), so no cfp/LFP restore is needed here.
        self.jit.bind_label(mm);
        self.jit.ldrh(X10, PC, 12); // recv slot
        self.a64_slot_value(X10);
        self.jit.mov(X2, X10); // receiver
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X3, LFP);
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit
            .mov_imm(X9, crate::codegen::runtime::invoke_method_missing as *const () as u64);
        self.jit.blr(X9);
        self.jit.b_label(&after_call);
        p
    }

    /// ops 82/84/87 `BlockBreak`/`Retry`/`Redo`: call `f(vm[, globals, val])`
    /// to set the control-flow error, then enter entry_raise. `with_val`
    /// passes slot `[pc+4]`'s value as a 3rd argument (BlockBreak).
    pub(in crate::codegen) fn a64_op_err1(&mut self, abs: u64, with_val: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        if with_val {
            self.jit.mov(X1, GLOBALS);
            self.jit.ldrh(X2, PC, 4);
            self.a64_slot_value(X2); // val
        }
        self.jit.mov_imm(X9, abs);
        self.jit.blr(X9);
        self.jit.b_label(&raise);
        p
    }

    /// op 83 `Raise`: raise_err(vm, exc `[pc+4]`), then enter entry_raise.
    pub(in crate::codegen) fn a64_op_err_raise(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.ldrh(X1, PC, 4);
        self.a64_slot_value(X1); // exception value
        self.jit.mov_imm(X9, runtime::raise_err as *const () as u64);
        self.jit.blr(X9);
        self.jit.b_label(&raise);
        p
    }

    /// op 85 `EnsureEnd`: if an error is still pending after an ensure block,
    /// re-enter entry_raise; otherwise continue.
    pub(in crate::codegen) fn a64_op_ensure_end(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        self.jit.mov(X0, EXEC);
        self.jit.mov_imm(X9, runtime::ensure_end as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbnz_label(X0, &raise);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.sub_imm(SP, SP, 16, 0);
        self.jit.str(PC, SP, 0);
        // get_yield_data(vm, globals) -> x0 = outer (Lfp), x1 = func_id
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit
            .mov_imm(X9, runtime::get_yield_data as *const () as u64);
        self.jit.blr(X9);
        self.jit.cbz_label(X1, &raise); // no block -> error set
        self.jit.mov(X25, X0); // X25 = outer (callee-saved across later calls)
        // get_func_data from func_id (X1) -> X15
        self.jit.lsl_imm(X10, X1, 32);
        self.jit.lsr_imm(X10, X10, 32);
        self.jit.lsl_imm(X10, X10, 6);
        self.jit.mov_imm(X11, GLOBALS_FUNCINFO as u64);
        self.jit.add(X11, GLOBALS, X11);
        self.jit.ldr(X11, X11, 0);
        self.jit.add(X10, X10, X11);
        self.jit.add_imm(X15, X10, FUNCINFO_DATA as u32, 0);
        // block frame setup: outer = X25, self = outer.self, svar/cme/block 0.
        self.jit.mov_imm(X12, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_OUTER) as u32, 0);
        self.jit.str(X25, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SVAR) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_CME) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_BLOCK) as u32, 0);
        self.jit.str(X12, X11, 0);
        self.jit.sub_imm(X10, X25, LFP_SELF as u32, 0);
        self.jit.ldr(X10, X10, 0); // self = outer.self
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_SELF) as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.ldr(X14, X15, FUNCDATA_META as u32);
        self.jit.sub_imm(X11, SP, (RSP_LOCAL_FRAME + LFP_META) as u32, 0);
        self.jit.str(X14, X11, 0);
        // generic arg setup: vm_handle_arguments(vm, globals, caller_lfp,
        // callee_lfp, callid). Reserve scratch; preserve SP/funcdata.
        self.jit.sub_imm(X3, SP, RSP_LOCAL_FRAME as u32, 0); // callee_lfp
        self.jit.mov_sp(X25, SP);
        self.jit.mov(X26, X15);
        self.jit.ldrh(X10, X15, FUNCDATA_OFS as u32);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add_imm(X10, X10, 16, 0);
        self.jit.sub(X11, X25, X10);
        self.jit.mov_sp(SP, X11);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov(X2, LFP); // caller lfp
        self.jit.ldr32(X4, PC, 0); // callid
        self.jit
            .mov_imm(X9, runtime::vm_handle_arguments as *const () as u64);
        self.jit.blr(X9);
        self.jit.mov(X15, X26);
        self.jit.mov_sp(SP, X25);
        self.jit.cbz_label(X0, &raise);
        // call_funcdata
        self.jit.ldr(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.str(X10, X11, 0);
        self.jit.str(X11, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(LFP, SP, RSP_LOCAL_FRAME as u32, 0);
        self.jit.sub_imm(X10, SP, (RSP_CFP + CFP_LFP) as u32, 0);
        self.jit.str(LFP, X10, 0);
        self.jit.mov(X3, PC); // call-site pc for with-pc builtins
        self.jit.ldr(PC, X15, FUNCDATA_PC as u32);
        self.jit.ldr(X10, X15, FUNCDATA_CODEPTR as u32);
        self.jit.blr(X10);
        self.jit.sub_imm(X11, SP, RSP_CFP as u32, 0);
        self.jit.ldr(X10, X11, 0);
        self.jit.str(X10, EXEC, EXECUTOR_CFP as u32);
        self.jit.sub_imm(X10, X29, (BP_CFP + CFP_LFP) as u32, 0);
        self.jit.ldr(LFP, X10, 0); // restore caller LFP
        // pop_cont_frame + store result to ret slot [pc+4]
        self.jit.ldr(PC, SP, 0);
        self.jit.add_imm(SP, SP, 16, 0);
        self.jit.cbz_label(X0, &raise);
        self.jit.ldrh(X10, PC, 4); // ret slot
        self.jit.add_imm(PC, PC, 32, 0);
        self.jit.cbz_label(X10, &skip);
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.str(X0, X11, 0);
        self.jit.bind_label(skip);
        self.a64_fetch_and_dispatch();
        p
    }

    /// loop_start / loop_end (ops 14/15): advance + dispatch (VM-only).
    pub(in crate::codegen) fn a64_op_loop(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// Unconditional branch (op 3) + the shared `branch` target used by the
    /// conditional branches. `pc += disp*16 + 16` (x86 `br_inst`/`branch:`).
    pub(in crate::codegen) fn a64_op_br(&mut self) -> (CodePtr, DestLabel) {
        let p = self.jit.get_current_address();
        let branch = self.jit.label();
        self.jit.ldrsw(X10, PC, 0); // disp (signed, instruction-relative)
        self.jit.bind_label(branch.clone());
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.add(PC, PC, X10);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        (p, branch)
    }

    /// Conditional branch (op 4/12 `condbr`, op 5/13 `condnotbr`). Bytecode:
    /// `+0` disp (i32), `+4` cond slot. Truthiness: `(v | 0x10) != FALSE_VALUE`
    /// (both nil and false collapse to FALSE_VALUE). `not` = branch-if-falsy.
    pub(in crate::codegen) fn a64_op_condbr(&mut self, branch: &DestLabel, not: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrsw(X10, PC, 0); // disp (kept in X10 for `branch`)
        self.jit.ldrh(X11, PC, 4); // cond slot
        self.a64_load_slot(X11, X12, X13); // cond value
        self.jit.mov_imm(X13, 0x10);
        self.jit.orr(X12, X12, X13);
        self.jit.cmp_imm(X12, FALSE_VALUE as u32, 0);
        let cond = if not { Cond::Eq } else { Cond::Ne };
        self.jit.bcond_label(cond, branch);
        self.jit.add_imm(PC, PC, 16, 0);
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
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // lhs
        self.a64_load_slot(X10, X14, X15); // rhs
        self.jit.tbz_label(X13, 0, &generic);
        self.jit.tbz_label(X14, 0, &generic);
        self.jit.cmp(X13, X14);
        self.jit.cset(X13, cond);
        self.jit.lsl_imm(X13, X13, 3);
        self.jit.mov_imm(X14, FALSE_VALUE);
        self.jit.orr(X13, X13, X14); // FALSE_VALUE | (result << 3)
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X10, LFP, X12, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0);
        self.jit.str(X13, X10, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        self.jit.bind_label(generic);
        // cmp_*_values(vm, globals, lhs=X13, rhs=X14) -> Option<Value>
        self.jit.mov(X2, X13); // lhs
        self.jit.mov(X3, X14); // rhs
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, cmp_fn);
        self.jit.blr(X9);
        self.a64_checked_store_next(&raise);
        p
    }

    /// Load the value of the slot whose (positive) index is in `idx`, into
    /// `dst`. `idx` is clobbered. (the `[r14+reg*8-LFP_SELF]` idiom.)
    pub(in crate::codegen) fn a64_load_slot(&mut self, idx: GReg, dst: GReg, scratch: GReg) {
        self.jit.neg(idx, idx);
        self.jit.add_lsl(scratch, LFP, idx, 3);
        self.jit.sub_imm(scratch, scratch, LFP_SELF as u32, 0);
        self.jit.ldr(dst, scratch, 0);
    }

    /// Generic binary-op fallback: call the runtime `func(vm, globals, lhs,
    /// rhs)` and store the result. Expects lhs in X13, rhs in X14, dst slot in
    /// X12 (all intact). VM globals are callee-saved so no register save is
    /// needed. On a Ruby error (result 0) jumps to entry_raise.
    /// ops 164-170: a binary operator with no fixnum fast path. Loads
    /// lhs `[pc+2]`, rhs `[pc+0]`, dst `[pc+4]` and calls the runtime op.
    pub(in crate::codegen) fn a64_op_binop(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.a64_generic_binop(func);
        p
    }

    pub(in crate::codegen) fn a64_generic_binop(&mut self, func: BinaryOpFn) {
        let raise = self.entry_raise.clone();
        self.jit.mov(X2, X13); // lhs
        self.jit.mov(X3, X14); // rhs
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, func as u64);
        self.jit.blr(X9);
        // The dst slot is re-read from PC (callee-saved): `func` may re-enter
        // the VM (e.g. string `+` dispatches a method), clobbering the
        // caller-saved dst register held before the call.
        self.a64_checked_store_next(&raise);
    }

    /// op 162/163 `mul_rr`/`div_rr`: no fixnum fast path — straight to the
    /// runtime fallback (matches x86 `vm_binops`). Bytecode: `+0` rhs, `+2`
    /// lhs, `+4` dst.
    pub(in crate::codegen) fn a64_op_muldiv(&mut self, func: BinaryOpFn) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 0);
        self.jit.ldrh(X11, PC, 2);
        self.jit.ldrh(X12, PC, 4);
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
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.jit.ldrh(X12, PC, 4); // dst slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.jit.tbz_label(X13, 0, &generic);
        self.jit.tbz_label(X14, 0, &generic);
        if is_sub {
            self.jit.subs(X9, X13, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
            self.jit.add_imm(X9, X9, 1, 0); // re-tag
        } else {
            self.jit.sub_imm(X9, X13, 1, 0); // untag one
            self.jit.adds(X9, X9, X14);
            self.jit.bcond_label(Cond::Vs, &generic);
        }
        self.jit.cbz_label(X12, &skip);
        self.jit.neg(X12, X12);
        self.jit.add_lsl(X10, LFP, X12, 3);
        self.jit.sub_imm(X10, X10, LFP_SELF as u32, 0);
        self.jit.str(X9, X10, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        self.jit.bind_label(generic);
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
        self.jit.ldrh(X10, PC, 0);
        self.jit.lsl_imm(X10, X10, 4);
        self.jit.mov_sp(X13, SP); // sp -= X10 (A64 sub can't take SP as a
        self.jit.sub(X13, X13, X10); // shifted-reg operand, so via a GPR)
        self.jit.mov_sp(SP, X13);
        // Skip the nil-fill for a captured (on-heap / invalidated) frame.
        // Its locals live on the heap and may already hold values written in
        // by `new_binding_frame` — e.g. a binding-eval frame that introduced a
        // brand-new local in a previous eval; nil-filling would wipe it.
        // Mirrors x86 `fill_nil`'s leading `branch_if_captured`. The `kind`
        // byte sits at `[LFP - (LFP_META - META_KIND)]`; bit 7 = on_heap,
        // bit 3 = invalidated.
        self.jit
            .sub_imm(X10, LFP, (LFP_META - META_KIND as i32) as u32, 0);
        self.jit.ldrb(X13, X10, 0);
        self.jit.tbnz_label(X13, 7, &skip); // on_heap
        self.jit.tbnz_label(X13, 3, &skip); // invalidated
        // count = reg_num - arg_num
        self.jit.ldrh(X15, PC, 4); // reg_num
        self.jit.ldrh(X11, PC, 2); // arg_num
        self.jit.sub(X12, X15, X11);
        self.jit.cbz_label(X12, &skip);
        // base = lfp - reg_num*8 - LFP_ARG0 ; fill [base + count*8] downward
        self.jit.neg(X15, X15);
        self.jit.add_lsl(X15, LFP, X15, 3);
        self.jit.sub_imm(X15, X15, LFP_ARG0 as u32, 0);
        self.jit.mov_imm(X14, NIL_VALUE);
        self.jit.bind_label(loop_.clone());
        self.jit.add_lsl(X10, X15, X12, 3);
        self.jit.str(X14, X10, 0);
        self.jit.sub_imm(X12, X12, 1, 0);
        self.jit.cbnz_label(X12, &loop_);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 6 `immediate`: slot[`[pc+4]`] <- the immediate Value at `[pc+8]`.
    /// (x86 `vm_immediate`: `fetch_r15; movq rax,[r13-8]; vm_store_r15`.)
    pub(in crate::codegen) fn a64_op_immediate(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 4); // dst slot index
        self.jit.ldr(X11, PC, 8); // immediate value
        self.jit.cbz_label(X10, &skip); // slot 0 => discard
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X12, LFP, X10, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X11, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 7 `literal`: slot[`[pc+4]`] <- a deep copy of the literal Value at
    /// `[pc+8]`. Each evaluation yields a fresh object (mutable literals like
    /// strings/arrays). x86 `vm_literal`: `movq rdi,[r13-8]; value_deep_copy`.
    pub(in crate::codegen) fn a64_op_literal(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldr(X0, PC, 8); // literal Value
        self.jit.mov_imm(X9, Value::value_deep_copy as *const () as u64);
        self.jit.blr(X9); // x0 = deep copy (PC/LFP are callee-saved)
        self.jit.ldrh(X10, PC, 4); // dst slot index
        self.jit.cbz_label(X10, &skip); // slot 0 => discard
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X12, LFP, X10, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X0, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 178 `Mov`: slot[`[pc+4]`] <- slot[`[pc+2]`]. (x86 `fetch3` +
    /// slot copy.)
    pub(in crate::codegen) fn a64_op_mov(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        self.jit.ldrh(X10, PC, 2); // src slot
        self.a64_slot_value(X10); // X10 = slot[src]
        self.jit.ldrh(X11, PC, 4); // dst slot
        self.jit.cbz_label(X11, &skip);
        self.jit.neg(X11, X11);
        self.jit.add_lsl(X12, LFP, X11, 3);
        self.jit.sub_imm(X12, X12, LFP_SELF as u32, 0);
        self.jit.str(X10, X12, 0);
        self.jit.bind_label(skip);
        self.jit.add_imm(PC, PC, 16, 0);
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 80 `ret`: return slot[`[pc+4]`]'s value (x86 `fetch_addr_r15;
    /// movq rax,[r15]; epilogue`).
    pub(in crate::codegen) fn a64_op_ret(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        self.jit.ldrh(X10, PC, 4); // slot index
        self.jit.neg(X10, X10);
        self.jit.add_lsl(X11, LFP, X10, 3);
        self.jit.sub_imm(X11, X11, LFP_SELF as u32, 0);
        self.jit.ldr(X0, X11, 0); // return value
        // epilogue (x86 `leave; ret`): restore the frame pointer and return.
        self.jit.mov_sp(SP, X29);
        self.jit.ldp_post(X29, X30, SP, 16);
        self.jit.ret();
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
        self.jit.ldrh(X10, PC, 0); // rhs slot
        self.jit.ldrh(X11, PC, 2); // lhs slot
        self.a64_load_slot(X11, X13, X14); // X13 = lhs
        self.a64_load_slot(X10, X14, X15); // X14 = rhs
        self.jit.mov(X2, X13);
        self.jit.mov(X3, X14);
        self.jit.mov(X0, EXEC);
        self.jit.mov(X1, GLOBALS);
        self.jit.mov_imm(X9, cmp_fn);
        self.jit.blr(X9);
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
