use super::*;
use monoasm_macro::monoasm;
use paste::paste;

mod definition;
pub mod init_method;
mod method_call;
mod variables;

macro_rules! vm_cmp_opt {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op _opt_rr>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic = self.jit.label();
              self.fetch3();
              self.vm_get_slot_value(GP::Rdi);
              self.vm_get_slot_value(GP::Rsi);
              self.guard_rdi_rsi_fixnum(&generic);
              self.vm_save_binary_integer();

              self.[<icmp_ $op>]();
              self.vm_store_r15(GP::Rax);
              self.fetch_and_dispatch();

              self.vm_generic_binop(&generic, [<cmp_ $op _values>] as _);
              self.fetch_and_dispatch();

              label
          }

          fn [<vm_ $op _rr>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic = self.jit.label();
              self.fetch3();
              self.vm_get_slot_value(GP::Rdi);
              self.vm_get_slot_value(GP::Rsi);
              self.vm_save_binary_integer();
              self.vm_generic_binop(&generic, [<cmp_ $op _values_no_opt>] as _);
              self.fetch_and_dispatch();

              label
          }

      }
  };
  ($op1:ident, $($op2:ident),+) => {
      vm_cmp_opt!($op1);
      vm_cmp_opt!($($op2),+);
  };
}

impl Codegen {
    vm_cmp_opt!(eq, ne, gt, ge, lt, le, teq);

    ///
    /// Generate interpreter.
    ///
    pub(super) fn construct_vm(&mut self) {
        let vm_entry = self.jit.label();
        let entry_fetch = self.jit.label();

        //
        // VM entry
        //
        // argument registers:
        //   rdx: args len
        //
        // global registers:
        //   rbx: &mut Interp
        //   r12: &mut Globals
        //   r13: pc
        //
        monoasm! { &mut self.jit,
        vm_entry:
            pushq rbp;
            movq rbp, rsp;
        entry_fetch:
        };
        self.fetch_and_dispatch();

        let jit_class_guard_fail = self.jit.label();
        #[cfg(feature = "profile")]
        monoasm! { &mut self.jit,
        jit_class_guard_fail:
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (guard_fail);
            subq rsp, 4088;
            call rax;
            addq rsp, 4088;
        }
        monoasm! { &mut self.jit,
            jmp vm_entry;
        }

        let div_by_zero = self.jit.label();
        let vm_raise = self.entry_raise.clone();
        monoasm!( &mut self.jit,
        div_by_zero:
            movq rdi, rbx;
            movq rax, (runtime::err_divide_by_zero);
            call rax;
            jmp vm_raise;
        );

        self.vm_fetch = entry_fetch;
        self.vm_entry = vm_entry;
        self.jit_class_guard_fail = jit_class_guard_fail;
        self.div_by_zero = div_by_zero;

        //BcOp::Ret
        let ret = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq rax, [r15];
        };
        self.epilogue();

        //BcOp::MethodRet
        let method_ret = self.jit.get_current_address();
        self.fetch_val_r15();
        monoasm! { &mut self.jit,
            movq rax, r15;
        };
        self.method_return();

        //BcOp::Break
        let block_break = self.jit.get_current_address();
        self.fetch_val_r15();
        monoasm! { &mut self.jit,
            movq rax, r15;
        };
        self.block_break();

        //BcOp::Raise
        let raise_err = self.jit.get_current_address();
        let raise = self.entry_raise();
        self.fetch_val_r15();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r15;
            movq rax, (runtime::raise_err);
            call rax;
            jmp raise;
        };

        //BcOp::EnsureEnd
        let ensure_end = self.jit.get_current_address();
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (runtime::check_err);
            call rax;
            testq rax, rax;
            jne  raise;
        };
        self.fetch_and_dispatch();

        //BcOp::ToA
        let toa = self.jit.get_current_address();
        let raise = self.entry_raise();
        self.fetch3();
        self.vm_get_slot_addr(GP::R15);
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::to_a);
            call rax;
            testq rax, rax;
            je  raise;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();

        //BcOp::Mov
        let mov = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::R15);
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();

        let br_inst = self.jit.get_current_address();
        let branch = self.jit.label();
        monoasm! { &mut self.jit,
            movsxl rdi, [r13 - 16];  // rdi <- :2:3
        branch:
            shlq rdi, 4;
            lea r13, [r13 + rdi];
        };
        self.fetch_and_dispatch();

        let add_rr = self.vm_binops_opt(Self::int_add, add_values);
        let sub_rr = self.vm_binops_opt(Self::int_sub, sub_values);
        let or_rr = self.vm_binops_opt(Self::int_or, bitor_values);
        let and_rr = self.vm_binops_opt(Self::int_and, bitand_values);
        let xor_rr = self.vm_binops_opt(Self::int_xor, bitxor_values);
        let div_rr = self.vm_binops(div_values);
        let mul_rr = self.vm_binops(mul_values);
        let rem_rr = self.vm_binops(rem_values);
        let pow_rr = self.vm_binops(pow_values);
        let vm_send_simple = self.vm_send(true);
        let vm_send = self.vm_send(false);

        self.dispatch[1] = self.vm_singleton_method_def();
        self.dispatch[2] = self.vm_method_def();
        self.dispatch[3] = br_inst;
        self.dispatch[4] = self.vm_condbr(&branch);
        self.dispatch[5] = self.vm_condnotbr(&branch);
        self.dispatch[6] = self.vm_immediate();
        self.dispatch[7] = self.vm_literal();
        self.dispatch[10] = self.vm_load_const();
        self.dispatch[11] = self.vm_store_const();
        self.dispatch[12] = self.vm_condbr(&branch);
        self.dispatch[13] = self.vm_condnotbr(&branch);
        self.dispatch[14] = self.vm_loop_start();
        self.dispatch[15] = self.vm_loop_end();
        self.dispatch[16] = self.vm_load_ivar();
        self.dispatch[17] = self.vm_store_ivar();
        self.dispatch[18] = self.vm_check_const();
        self.dispatch[19] = self.vm_check_kw_rest();
        self.dispatch[20] = self.vm_check_local(&branch);
        self.dispatch[21] = self.vm_block_arg_proxy();
        self.dispatch[22] = self.vm_singleton_class_def();
        self.dispatch[23] = self.vm_block_arg();
        self.dispatch[24] = self.vm_check_cvar();
        self.dispatch[25] = self.vm_load_gvar();
        self.dispatch[26] = self.vm_store_gvar();
        self.dispatch[27] = self.vm_load_cvar();
        self.dispatch[28] = self.vm_load_svar();
        self.dispatch[29] = self.vm_store_cvar();
        self.dispatch[30] = vm_send_simple;
        self.dispatch[31] = vm_send;
        self.dispatch[32] = vm_send;
        self.dispatch[33] = vm_send;
        self.dispatch[34] = self.vm_yield(true);
        self.dispatch[35] = self.vm_yield(false);

        self.dispatch[36] = self.vm_optcase(&branch);
        self.dispatch[37] = self.vm_nilbr(&branch);
        self.dispatch[38] = self.vm_lambda();
        self.dispatch[39] = self.vm_array();
        self.dispatch[40] = self.vm_array_teq();

        self.dispatch[64] = self.vm_defined_yield();
        self.dispatch[65] = self.vm_defined_const();
        self.dispatch[66] = self.vm_defined_method();
        self.dispatch[67] = self.vm_defined_gvar();
        self.dispatch[68] = self.vm_defined_ivar();
        self.dispatch[69] = self.vm_defined_super();
        self.dispatch[70] = self.vm_class_def(false);
        self.dispatch[71] = self.vm_class_def(true);
        self.dispatch[80] = ret;
        self.dispatch[81] = method_ret;
        self.dispatch[82] = block_break;
        self.dispatch[83] = raise_err;
        self.dispatch[85] = ensure_end;
        self.dispatch[86] = self.vm_concat_regexp();

        self.dispatch[120] = self.vm_not();
        self.dispatch[121] = self.vm_pos();
        self.dispatch[122] = self.vm_neg();
        self.dispatch[123] = self.vm_bitnot();
        self.dispatch[132] = self.vm_index();
        self.dispatch[133] = self.vm_index_assign();

        self.dispatch[140] = self.vm_eq_opt_rr();
        self.dispatch[141] = self.vm_ne_opt_rr();
        self.dispatch[142] = self.vm_lt_opt_rr();
        self.dispatch[143] = self.vm_le_opt_rr();
        self.dispatch[144] = self.vm_gt_opt_rr();
        self.dispatch[145] = self.vm_ge_opt_rr();
        self.dispatch[146] = self.vm_teq_opt_rr();

        self.dispatch[148] = self.vm_load_dvar();
        self.dispatch[149] = self.vm_store_dvar();

        self.dispatch[150] = self.vm_eq_opt_rr();
        self.dispatch[151] = self.vm_ne_opt_rr();
        self.dispatch[152] = self.vm_lt_opt_rr();
        self.dispatch[153] = self.vm_le_opt_rr();
        self.dispatch[154] = self.vm_gt_opt_rr();
        self.dispatch[155] = self.vm_ge_opt_rr();
        self.dispatch[156] = self.vm_teq_opt_rr();

        self.dispatch[160] = add_rr;
        self.dispatch[161] = sub_rr;
        self.dispatch[162] = mul_rr;
        self.dispatch[163] = div_rr;
        self.dispatch[164] = or_rr;
        self.dispatch[165] = and_rr;
        self.dispatch[166] = xor_rr;
        self.dispatch[167] = rem_rr;
        self.dispatch[168] = pow_rr;

        self.dispatch[170] = self.vm_init();
        self.dispatch[171] = self.vm_expand_array();
        self.dispatch[172] = self.vm_undef_method();
        self.dispatch[173] = self.vm_alias_method();
        self.dispatch[174] = self.vm_hash();
        self.dispatch[175] = toa;
        self.dispatch[176] = mov;
        self.dispatch[177] = self.vm_range(false);
        self.dispatch[178] = self.vm_range(true);
        self.dispatch[179] = self.vm_concat();
    }

    ///
    /// Replace VM instruction routines with non-basic-op-optimized routines.
    ///
    pub(super) fn remove_vm_bop_optimization(&mut self) {
        self.dispatch[14] = self.vm_loop_start_no_opt();

        self.dispatch[120] = self.vm_not();
        self.dispatch[121] = self.vm_pos_no_opt();
        self.dispatch[122] = self.vm_neg_no_opt();
        self.dispatch[123] = self.vm_bitnot_no_opt();

        self.dispatch[140] = self.vm_eq_rr();
        self.dispatch[141] = self.vm_ne_rr();
        self.dispatch[142] = self.vm_lt_rr();
        self.dispatch[143] = self.vm_le_rr();
        self.dispatch[144] = self.vm_gt_rr();
        self.dispatch[145] = self.vm_ge_rr();
        self.dispatch[146] = self.vm_teq_rr();

        self.dispatch[150] = self.vm_eq_rr();
        self.dispatch[151] = self.vm_ne_rr();
        self.dispatch[152] = self.vm_lt_rr();
        self.dispatch[153] = self.vm_le_rr();
        self.dispatch[154] = self.vm_gt_rr();
        self.dispatch[155] = self.vm_ge_rr();
        self.dispatch[156] = self.vm_teq_rr();

        self.dispatch[160] = self.vm_binops(add_values_no_opt);
        self.dispatch[161] = self.vm_binops(sub_values_no_opt);
        self.dispatch[162] = self.vm_binops(mul_values_no_opt);
        self.dispatch[163] = self.vm_binops(div_values_no_opt);
        self.dispatch[164] = self.vm_binops(bitor_values_no_opt);
        self.dispatch[165] = self.vm_binops(bitand_values_no_opt);
        self.dispatch[166] = self.vm_binops(bitxor_values_no_opt);
        self.dispatch[167] = self.vm_binops(rem_values_no_opt);
        self.dispatch[168] = self.vm_binops(pow_values_no_opt);

        self.jit.finalize();
    }

    fn fetch2(&mut self) {
        monoasm! { &mut self.jit,
            movsxl rdi, [r13 - 16];  // rdi <- :2:3
            movzxw r15, [r13 - 12];  // r15 <- :1
        };
    }

    fn fetch3(&mut self) {
        monoasm! { &mut self.jit,
            movsxw rsi, [r13 - 16];    // rsi <- :3
            movzxw rdi, [r13 - 14];    // rdi <- :2
            movzxw r15, [r13 - 12];  // r15 <- :1
        };
    }
    ///
    /// Get an address of the slot specified by *reg*.
    ///
    /// ### in
    /// - *reg*: GP which has the slot number.
    ///
    /// ### return
    /// - *reg*: address of the slot
    ///
    fn vm_get_slot_addr(&mut self, reg: GP) {
        let r = reg as u64;
        monoasm! { &mut self.jit,
            negq R(r);
            lea R(r), [r14 + R(r) * 8 - (LFP_SELF)];
        };
    }

    ///
    /// Get a value of the slot specified by *reg*.
    ///
    /// #### in
    /// - *reg*: GP which has the slot number.
    ///
    /// #### out
    /// - *reg*: value of the slot
    ///
    fn vm_get_slot_value(&mut self, reg: GP) {
        let r = reg as u64;
        monoasm! { &mut self.jit,
            negq R(r);
            movq R(r), [r14 + R(r) * 8 - (LFP_SELF)];
        };
    }

    ///
    /// Get a value of the slot specified by *reg* if *reg* is not zero.
    ///
    /// #### in
    /// - *reg*: GP which has the slot number.
    ///
    /// #### out
    /// - *reg*: value of the slot or 0.
    ///
    fn vm_get_slot_value_if_nonzero(&mut self, reg: GP) {
        let r = reg as u64;
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            cmpw R(r), 0;
            jeq exit;
        }
        self.vm_get_slot_value(reg);
        self.jit.bind_label(exit);
    }

    fn fetch_r15(&mut self) {
        monoasm! { &mut self.jit,
            movzxw r15, [r13 - 12];  // r15 <- :1
        };
    }

    fn fetch_addr_r15(&mut self) {
        monoasm! { &mut self.jit,
            movzxw r15, [r13 - 12];  // r15 <- :1
            negq r15;
            lea r15, [r14 + r15 * 8 - (LFP_SELF)];
        };
    }

    fn fetch_val_r15(&mut self) {
        monoasm! { &mut self.jit,
            movzxw r15, [r13 - 12];  // r15 <- :1
            negq r15;
            movq r15, [r14 + r15 * 8 - (LFP_SELF)];
        };
    }

    fn vm_store_r15(&mut self, reg: GP) {
        let exit = self.jit.label();
        let r = reg as u64;
        monoasm! { &mut self.jit,
            testq r15, r15;
            jeq exit;
        };
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            movq [r15], R(r);
        exit:
        };
    }

    fn vm_store_rdi(&mut self, reg: GP) {
        let exit = self.jit.label();
        let r = reg as u64;
        monoasm! { &mut self.jit,
            testq rdi, rdi;
            jeq exit;
        };
        self.vm_get_slot_addr(GP::Rdi);
        monoasm! { &mut self.jit,
            movq [rdi], R(r);
        exit:
        };
    }

    ///
    /// Save a class of the left-hand side value in the inline cache.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - rax: ClassId
    ///
    fn vm_save_lhs_class(&mut self) {
        let get_class = self.get_class.clone();
        monoasm! { &mut self.jit,
            call  get_class;
            movl  [r13 - 8], rax;
        };
    }

    ///
    /// Save classes of the binary valuees in the inline cache.
    ///
    /// ### in
    /// - rdi: Value
    /// - rsi: Value
    ///
    /// ### destroy
    /// - rax
    ///
    fn vm_save_binary_class(&mut self) {
        let get_class = self.get_class.clone();
        monoasm! { &mut self.jit,
            call  get_class;
            movl  [r13 - 8], rax;
            xchgq rdi, rsi;
            //movq  rdi, rsi;
            call  get_class;
            movl  [r13 - 4], rax;
            xchgq rdi, rsi;
        };
    }

    fn vm_lhs_integer(&mut self) {
        let int_class: u32 = INTEGER_CLASS.into();
        monoasm! { &mut self.jit,
            movl  [r13 - 8], (int_class);
            movl  [r13 - 4], (int_class);
        };
    }

    fn vm_save_binary_integer(&mut self) {
        let int_class: u32 = INTEGER_CLASS.into();
        monoasm! { &mut self.jit,
            movl  [r13 - 8], (int_class);
            movl  [r13 - 4], (int_class);
        };
    }

    fn vm_handle_error(&mut self) {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq  raise;
        };
    }

    fn vm_generic_unop(&mut self, generic: &DestLabel, func: UnaryOpFn) {
        self.jit.bind_label(generic.clone());
        self.vm_save_lhs_class();
        self.call_unop(func);
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: &DestLabel, func: BinaryOpFn) {
        self.jit.bind_label(generic.clone());
        self.vm_save_binary_class();
        self.call_binop(func);
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
    }

    /// Expand array
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// |len|dst|src| op||rst|   |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// src: the source resister
    /// dst: the start of destination reginsters
    /// len: the number of destination registers
    /// rst: the position of *rest* register.
    ///
    /// ex)
    ///     a, *b, c = ...
    ///     len = 3
    ///     rst = 1
    /// ~~~
    fn vm_expand_array(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        monoasm! { &mut self.jit,
            // rcx <- rst
            movzxw rcx, [r13 - 8];
            // rdx <- len
            movq rdx, rsi;
            // rsi <- dst
            negq rdi;
            lea rsi, [r14 + rdi * 8 - (LFP_SELF)];
            // rdi <- *src
            negq r15;
            movq rdi, [r14 + r15 * 8 - (LFP_SELF)];
            movq rax, (runtime::expand_array);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    /// Undef method
    ///
    /// ~~~text
    ///                  -8      -4
    /// +---+---+---+---++---+---+---+---+
    /// | undef |   | op||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// undef: a symbol
    /// ~~~
    fn vm_undef_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 - 16];
            movq rax, (runtime::undef_method);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    /// Alias method
    ///
    /// ~~~text
    ///                  -8      -4
    /// +---+---+---+---++---+---+---+---+
    /// | op|   |   |   ||  new  |  old  |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// new: a new symbol
    /// old: a old symbol
    /// ~~~
    fn vm_alias_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 - 4];  // old
            movl rcx, [r13 - 8];  // new
            movq rax, (runtime::alias_method);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    /// Concatenate string
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|ret|reg|len||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// ret: return register
    /// reg: the start of argument registers
    /// len: the number of argument registers
    /// ~~~
    fn vm_concat(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::Rdi);
        monoasm! { &mut self.jit,
            movq rcx, rsi;
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::concatenate_string);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_concat_regexp(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::Rdi);
        monoasm! { &mut self.jit,
            movq rcx, rsi;
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::concatenate_regexp);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_loop_start(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let compile = self.jit.label();
        let cont = self.jit.label();
        self.vm_execute_gc();
        if !cfg!(feature = "no-jit") {
            let count = self.jit.label();
            self.jit.branch_if_captured(&cont);
            monoasm! { &mut self.jit,
                movq rax, [r13 - 8];
                testq rax, rax;
                jeq count;
                jmp rax;
            count:
                addl [r13 - 16], 1;
                cmpl [r13 - 16], (COUNT_LOOP_START_COMPILE);
                jae   compile;
            };
        };
        self.jit.bind_label(cont.clone());
        self.fetch_and_dispatch();
        if !cfg!(feature = "no-jit") {
            self.gen_compile_loop(&compile, &cont);
        }
        label
    }

    fn vm_loop_start_no_opt(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_and_dispatch();
        label
    }

    fn vm_loop_end(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_and_dispatch();
        label
    }

    fn vm_immediate(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_r15();
        monoasm! { &mut self.jit,
            movq rax, [r13 - 8];
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_literal(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_r15();
        monoasm! { &mut self.jit,
            movq rdi, [r13 - 8];
            movq rax, (Value::value_deep_copy);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_array(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        monoasm! { &mut self.jit,
            movl rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            lea  rcx, [r14 - (LFP_SELF)];
            movq rax, (runtime::gen_array);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_array_teq(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            movzxw rdi, [r13 - 14];     // rdi <- :2
            movsxw rsi, [r13 - 16];     // rsi <- :3
            movl   r15, rdi;
        }
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::array_teq);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_lambda(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            movl rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_lambda);
            call rax;
        }
        self.restore_lfp();
        monoasm! { &mut self.jit,
            movzxw rdi, [r13 - 12];  // r15 <- :1
            negq rdi;
            lea  rdi, [r14 + rdi * 8 - (LFP_SELF)];
            movq [rdi], rax;
        }
        self.fetch_and_dispatch();
        label
    }

    fn vm_hash(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::Rdi);
        monoasm! { &mut self.jit,
            movq rdx, rdi;      // src: *const Value
            movzxw rcx, rsi;    // len: usize
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_hash);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_range(&mut self, exclude_end: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rdx, rbx;
            movq rcx, r12;
            movl r8, (if exclude_end {1} else {0});
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|ret|bas|idx||basecls|idxclas|
    // +---+---+---+---++---+---+---+---+
    //
    fn vm_index(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            lea  r8, [r13 - 8]; // &mut ClassId
            movq rax, (runtime::get_index);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_index_assign(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::R15);
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq r8, r15;  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            lea  r9, [r13 - 8];
            movq rax, (runtime::set_index);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|dst| outer ||               |
    // +---+---+---+---++---+---+---+---+
    //
    fn vm_block_arg_proxy(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        //let panic = self.entry_panic;
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let exit = self.jit.label();
        self.fetch2();
        monoasm! { &mut self.jit,
            movq  rax, r14;
            testq rdi, rdi;
            jz   loop_exit;
        loop_:
            movq rax, [rax];
            subl rdi, 1;
            jnz  loop_;
        loop_exit:
            movq rax, [rax - (LFP_BLOCK)];
            movq rdi, (Value::nil().id());
            testq rax, rax;
            cmoveqq rax, rdi;
            testq rax, 0b1;
            jeq exit;
            movsxl rdi, [r13 - 16];
            shlq rdi, 2;
            addq rax, rdi;
            addq rax, 0b10;
        exit:
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|dst| outer ||               |
    // +---+---+---+---++---+---+---+---+
    //
    fn vm_block_arg(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let raise = self.entry_raise();
        self.fetch2();
        monoasm! { &mut self.jit,
            movq  rax, r14;
            testq rdi, rdi;
            jz   loop_exit;
        loop_:
            movq rax, [rax];
            subl rdi, 1;
            jnz  loop_;
        loop_exit:
            movq rdx, [rax - (LFP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
            testq rax, rax;
            jz raise;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq rdx, r15;
            movl rcx, [r13 - 8];
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_const);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movl rdx, [r13 - 8];
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_gvar);
            call rax;
            movq [r15], rax;
        }
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_ivar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq rdx, r15;
            movl rcx, [r13 - 8];
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_ivar);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::R15);
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
            movq rdx, r15;
            movq rcx, rdi;
            movl r8, [r13 - 8];
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_method);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_yield(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq rdx, r15;
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_yield);
            call rax;
            movq [r15], rax;
        }
        self.fetch_and_dispatch();
        label
    }

    fn vm_defined_super(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_super);
            call rax;
            movq [r15], rax;
        }
        self.fetch_and_dispatch();
        label
    }

    fn vm_not(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.not_rdi_to_rax();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    fn vm_bitnot(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_generic_unop(&generic, bitnot_value);
        label
    }

    fn vm_bitnot_no_opt(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_generic_unop(&generic, bitnot_value_no_opt);
        label
    }

    fn vm_neg(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.guard_rdi_fixnum(&generic);
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            negq rdi;
            lea rdi, [rdi + rdi + 1];
        };
        self.vm_lhs_integer();
        self.vm_store_r15(GP::Rdi);
        self.fetch_and_dispatch();
        self.vm_generic_unop(&generic, neg_value);
        label
    }

    fn vm_neg_no_opt(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_generic_unop(&generic, neg_value_no_opt);
        label
    }

    fn vm_pos(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        monoasm! { &mut self.jit,
            testq rdi, 0x1;
            jz generic;
        }
        self.vm_store_r15(GP::Rdi);
        self.fetch_and_dispatch();
        self.vm_generic_unop(&generic, pos_value);
        label
    }

    fn vm_pos_no_opt(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_generic_unop(&generic, pos_value_no_opt);
        label
    }

    fn int_add(&mut self, generic: DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            subb rax, 1;
            addq rax, rsi;
            jo generic;
        };
    }

    fn int_sub(&mut self, generic: DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addb rax, 1;
        };
    }

    fn int_or(&mut self, _generic: DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            orq rax, rsi;
        };
    }

    fn int_and(&mut self, _generic: DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            andq rax, rsi;
        };
    }

    fn int_xor(&mut self, _generic: DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            xorq rax, rsi;
            orb rax, 1;
        };
    }

    fn vm_binops_opt(
        &mut self,
        opt_func: fn(&mut Codegen, DestLabel),
        generic_func: BinaryOpFn,
    ) -> CodePtr {
        let common = self.jit.label();
        let ptr = self.jit.get_current_address();
        let generic = self.jit.label();
        let exit = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        self.guard_rdi_rsi_fixnum(&generic);
        self.jit.bind_label(common.clone());
        self.vm_save_binary_integer();
        opt_func(self, generic.clone());
        self.vm_store_r15(GP::Rax);
        self.jit.bind_label(exit.clone());
        self.fetch_and_dispatch();

        self.vm_generic_binop(&generic, generic_func);
        monoasm! { &mut self.jit, jmp exit; };

        ptr
    }

    fn vm_binops(&mut self, func: BinaryOpFn) -> CodePtr {
        let common = self.jit.label();
        let ptr = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        self.vm_generic_binop(&common, func);
        self.fetch_and_dispatch();

        ptr
    }

    fn vm_check_local(&mut self, branch: &DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            testq r15, r15;
            jne  branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_check_kw_rest(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let exit = self.jit.label();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            cmpq [r15], (NIL_VALUE);
            jne  exit;
            movq rax, (runtime::empty_hash);
            call rax;
            movq [r15], rax;
        exit:
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condbr(&mut self, branch: &DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jne branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_nilbr(&mut self, branch: &DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            cmpq r15, (NIL_VALUE);
            je branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condnotbr(&mut self, branch: &DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jeq branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_optcase(&mut self, branch: &DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            movl rdx, rdi;
            movq rcx, [r15];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (opt_case);
            call rax;
            movl rdi, rax;
            jmp branch;
        };
        label
    }
}

extern "C" fn opt_case(
    _vm: &mut Executor,
    globals: &mut Globals,
    callid: OptCaseId,
    idx: Value,
) -> u32 {
    globals.store[callid].find(idx)
}
