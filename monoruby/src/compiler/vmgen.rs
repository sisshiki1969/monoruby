#[cfg(feature = "perf")]
use std::io::Write;

use super::*;
use monoasm_macro::monoasm;
use paste::paste;

mod definition;
pub mod init_method;
mod invoker;
mod method_call;
mod variables;

const OPECODE: i64 = 6;

macro_rules! cmp_ops {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op rr>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic = self.jit.label();
              self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
              self.guard_rdi_rsi_fixnum(generic);
              self.vm_save_binary_integer();

              self.[<icmp_ $op>]();
              self.vm_store_r15();
              self.fetch_and_dispatch();

              self.vm_generic_binop(generic, [<cmp_ $op _values>] as _);
              self.fetch_and_dispatch();

              label
          }

          fn [<vm_ $op ri>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic = self.jit.label();
              self.vm_get_ri_r15();
              self.guard_rdi_fixnum(generic);
              self.vm_save_binary_integer();

              self.[<icmp_ $op>]();
              self.vm_store_r15();
              self.fetch_and_dispatch();

              self.vm_generic_binop(generic, [<cmp_ $op _values>] as _);
              self.fetch_and_dispatch();

              label
          }
      }
  };
  ($op1:ident, $($op2:ident),+) => {
      cmp_ops!($op1);
      cmp_ops!($($op2),+);
  };
}

impl Codegen {
    cmp_ops!(eq, ne, gt, ge, lt, le, teq, cmp);

    ///
    /// Generate interpreter.
    ///
    pub(super) fn construct_vm(&mut self, no_jit: bool, main_object: Value) {
        self.entry_point = self.gen_entry_point(main_object);
        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();
        let vm_entry = self.jit.label();
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
            //movq rdx, rdi;
        };
        let entry_fetch = self.jit.label();
        self.jit.bind_label(entry_fetch);
        self.fetch_and_dispatch();
        self.vm_fetch = entry_fetch;

        self.vm_entry = vm_entry;
        self.jit_class_guard_fail = self.jit.label();
        self.jit.bind_label(self.jit_class_guard_fail);
        #[cfg(feature = "profile")]
        monoasm! { &mut self.jit,
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

        let vm_raise = self.jit.label();
        let leave = self.jit.label();
        let goto = self.jit.label();
        monoasm! { &mut self.jit,
        vm_raise:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LBP_META)];
            movq rcx, r13;
            subq rcx, 16;
            movq rax, (runtime::handle_error);
            call rax;
            testq rax, rax;
            jne  goto;
            testq rdx, rdx;
            jz   leave;
            movq rax, rdx;
            jmp  leave;
        goto:
            movq r13, rax;
        }
        self.fetch_and_dispatch();
        monoasm! { &mut self.jit,
        leave:
            leave;
            ret;
        }
        self.entry_raise = vm_raise;

        let div_by_zero = self.jit.label();
        monoasm!( &mut self.jit,
        div_by_zero:
            movq rdi, rbx;
            movq rax, (runtime::err_divide_by_zero);
            call rax;
            jmp vm_raise;
        );
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
        self.block_break();
        monoasm! { &mut self.jit,
            movq rax, r15;
        };
        self.epilogue();

        //BcOp::Raise
        let raise_err = self.jit.get_current_address();
        let raise = self.entry_raise;
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
        let raise = self.entry_raise;
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (runtime::check_err);
            call rax;
            testq rax, rax;
            jne  raise;
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

        let (add_rr, add_ri, add_ir) = self.vm_binops_opt(Self::int_add, add_values);
        let (sub_rr, sub_ri, sub_ir) = self.vm_binops_opt(Self::int_sub, sub_values);
        let (or_rr, or_ri, or_ir) = self.vm_binops_opt(Self::int_or, bitor_values);
        let (and_rr, and_ri, and_ir) = self.vm_binops_opt(Self::int_and, bitand_values);
        let (xor_rr, xor_ri, xor_ir) = self.vm_binops_opt(Self::int_xor, bitxor_values);
        let (div_rr, div_ri, div_ir) = self.vm_binops(div_values);
        let (mul_rr, mul_ri, mul_ir) = self.vm_binops(mul_values);
        let (rem_rr, rem_ri, rem_ir) = self.vm_binops(rem_values);
        let (pow_rr, pow_ri, pow_ir) = self.vm_binops(pow_values);
        let vm_call_simple = self.vm_call(true);
        let vm_call = self.vm_call(false);

        self.dispatch[1] = self.vm_singleton_method_def();
        self.dispatch[2] = self.vm_method_def();
        self.dispatch[3] = br_inst;
        self.dispatch[4] = self.vm_condbr(branch);
        self.dispatch[5] = self.vm_condnotbr(branch);
        self.dispatch[6] = self.vm_integer();
        self.dispatch[7] = self.vm_literal();
        self.dispatch[8] = self.vm_nil();
        self.dispatch[9] = self.vm_symbol();
        self.dispatch[10] = self.vm_load_const();
        self.dispatch[11] = self.vm_store_const();
        self.dispatch[12] = self.vm_condbr(branch);
        self.dispatch[13] = self.vm_condnotbr(branch);
        self.dispatch[14] = self.vm_loop_start(no_jit);
        self.dispatch[15] = self.vm_loop_end();
        self.dispatch[16] = self.vm_load_ivar();
        self.dispatch[17] = self.vm_store_ivar();
        self.dispatch[20] = self.vm_check_local(branch);
        self.dispatch[21] = self.vm_block_arg_proxy();
        self.dispatch[22] = self.vm_singleton_class_def();
        self.dispatch[23] = self.vm_block_arg();
        self.dispatch[25] = self.vm_load_gvar();
        self.dispatch[26] = self.vm_store_gvar();
        self.dispatch[27] = self.vm_load_cvar();
        self.dispatch[28] = self.vm_load_svar();
        self.dispatch[29] = self.vm_store_cvar();
        self.dispatch[30] = vm_call_simple;
        self.dispatch[31] = vm_call;
        self.dispatch[32] = vm_call;
        self.dispatch[33] = vm_call;
        self.dispatch[34] = self.vm_yield();
        self.dispatch[35] = self.vm_array();
        self.dispatch[36] = self.vm_optcase(branch);
        self.dispatch[37] = self.vm_nilbr(branch);
        self.dispatch[38] = self.vm_lambda();

        self.dispatch[64] = self.vm_defined_yield();
        self.dispatch[65] = self.vm_defined_const();
        self.dispatch[66] = self.vm_defined_method();
        self.dispatch[67] = self.vm_defined_gvar();
        self.dispatch[68] = self.vm_defined_ivar();
        self.dispatch[70] = self.vm_class_def(false);
        self.dispatch[71] = self.vm_class_def(true);
        self.dispatch[80] = ret;
        self.dispatch[81] = method_ret;
        self.dispatch[82] = block_break;
        self.dispatch[83] = raise_err;
        self.dispatch[85] = ensure_end;
        self.dispatch[86] = self.vm_concat_regexp();
        self.dispatch[126] = self.vm_pos();
        self.dispatch[127] = self.vm_bitnot();
        self.dispatch[128] = self.vm_not();
        self.dispatch[129] = self.vm_neg();
        self.dispatch[132] = self.vm_index();
        self.dispatch[133] = self.vm_index_assign();

        self.dispatch[134] = self.vm_eqrr();
        self.dispatch[135] = self.vm_nerr();
        self.dispatch[136] = self.vm_ltrr();
        self.dispatch[137] = self.vm_lerr();
        self.dispatch[138] = self.vm_gtrr();
        self.dispatch[139] = self.vm_gerr();
        self.dispatch[140] = self.vm_teqrr();
        self.dispatch[141] = self.vm_cmprr();

        self.dispatch[142] = self.vm_eqri();
        self.dispatch[143] = self.vm_neri();
        self.dispatch[144] = self.vm_ltri();
        self.dispatch[145] = self.vm_leri();
        self.dispatch[146] = self.vm_gtri();
        self.dispatch[147] = self.vm_geri();
        self.dispatch[148] = self.vm_teqri();
        self.dispatch[149] = self.vm_cmpri();

        self.dispatch[150] = self.vm_load_dvar();
        self.dispatch[151] = self.vm_store_dvar();

        self.dispatch[154] = self.vm_eqrr();
        self.dispatch[155] = self.vm_nerr();
        self.dispatch[156] = self.vm_ltrr();
        self.dispatch[157] = self.vm_lerr();
        self.dispatch[158] = self.vm_gtrr();
        self.dispatch[159] = self.vm_gerr();
        self.dispatch[160] = self.vm_teqrr();
        self.dispatch[161] = self.vm_cmprr();

        self.dispatch[162] = self.vm_eqri();
        self.dispatch[163] = self.vm_neri();
        self.dispatch[164] = self.vm_ltri();
        self.dispatch[165] = self.vm_leri();
        self.dispatch[166] = self.vm_gtri();
        self.dispatch[167] = self.vm_geri();
        self.dispatch[168] = self.vm_teqri();
        self.dispatch[169] = self.vm_cmpri();

        self.dispatch[170] = self.vm_init();
        self.dispatch[171] = self.vm_expand_array();
        self.dispatch[172] = self.vm_init();
        self.dispatch[173] = self.vm_alias_method();
        self.dispatch[174] = self.vm_hash();
        self.dispatch[176] = mov;
        self.dispatch[177] = self.vm_range(false);
        self.dispatch[178] = self.vm_range(true);
        self.dispatch[179] = self.vm_concat();

        self.dispatch[180] = add_ir;
        self.dispatch[181] = sub_ir;
        self.dispatch[182] = mul_ir;
        self.dispatch[183] = div_ir;
        self.dispatch[184] = or_ir;
        self.dispatch[185] = and_ir;
        self.dispatch[186] = xor_ir;
        self.dispatch[187] = rem_ir;
        self.dispatch[188] = pow_ir;

        self.dispatch[190] = add_ri;
        self.dispatch[191] = sub_ri;
        self.dispatch[192] = mul_ri;
        self.dispatch[193] = div_ri;
        self.dispatch[194] = or_ri;
        self.dispatch[195] = and_ri;
        self.dispatch[196] = xor_ri;
        self.dispatch[197] = rem_ri;
        self.dispatch[198] = pow_ri;

        self.dispatch[200] = add_rr;
        self.dispatch[201] = sub_rr;
        self.dispatch[202] = mul_rr;
        self.dispatch[203] = div_rr;
        self.dispatch[204] = or_rr;
        self.dispatch[205] = and_rr;
        self.dispatch[206] = xor_rr;
        self.dispatch[207] = rem_rr;
        self.dispatch[208] = pow_rr;

        #[cfg(feature = "perf")]
        self.perf_info(pair, "monoruby-vm");

        self.gen_invoker();
    }

    #[cfg(feature = "perf")]
    pub(crate) fn get_wrapper_info(
        &mut self,
        pair: (CodePtr, CodePtr),
    ) -> (CodePtr, usize, CodePtr, usize) {
        let (ptr0, ptr1) = pair;
        assert_eq!(0, self.jit.get_page());
        let size0 = self.jit.get_current_address() - ptr0;
        self.jit.select_page(1);
        let size1 = self.jit.get_current_address() - ptr1;
        self.jit.select_page(0);
        (ptr0, size0 as usize, ptr1, size1 as usize)
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_info(&mut self, pair: (CodePtr, CodePtr), func_name: &str) {
        let info = self.get_wrapper_info(pair);
        self.perf_info2(info, func_name);
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_info2(&mut self, info: (CodePtr, usize, CodePtr, usize), desc: &str) {
        self.perf_file
            .write_all(format!("{:x} {:x} {desc}\n", info.0.as_ptr() as usize, info.1).as_bytes())
            .unwrap();
        self.perf_file
            .write_all(format!("{:x} {:x} {desc}\n", info.2.as_ptr() as usize, info.3).as_bytes())
            .unwrap();
    }

    fn push_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            pushq r15;
            pushq r14;
            pushq r13;
            pushq r12;
            pushq rbx;
            pushq rbp;
        };
    }

    fn pop_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            popq rbp;
            popq rbx;
            popq r12;
            popq r13;
            popq r14;
            popq r15;
        };
    }

    ///
    /// Fetch instruction and dispatch.
    ///
    /// ### in
    /// - r13: BcPc
    ///
    /// ### destroy
    /// - rax, r15
    ///
    fn fetch_and_dispatch(&mut self) {
        monoasm! { &mut self.jit,
            movq r15, (self.dispatch.as_ptr());
            movzxb rax, [r13 + (OPECODE)]; // rax <- :0
            addq r13, 16;
            jmp [r15 + rax * 8];
        };
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
            lea R(r), [r14 + R(r) * 8 - (LBP_SELF)];
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
            movq R(r), [r14 + R(r) * 8 - (LBP_SELF)];
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

    fn vm_get_smi_rdi(&mut self) {
        monoasm! { &mut self.jit,
            movsxw rdi, rdi;
            shlq rdi, 1;
            orq  rdi, 1;
        };
    }

    fn vm_get_smi_rsi(&mut self) {
        monoasm! { &mut self.jit,
            movsxw rsi, rsi;
            shlq rsi, 1;
            orq  rsi, 1;
        };
    }

    fn fetch_addr_r15(&mut self) {
        monoasm! { &mut self.jit,
            movzxw r15, [r13 - 12];  // r15 <- :1
            negq r15;
            lea r15, [r14 + r15 * 8 - (LBP_SELF)];
        };
    }

    fn fetch_val_r15(&mut self) {
        monoasm! { &mut self.jit,
            movzxw r15, [r13 - 12];  // r15 <- :1
            negq r15;
            movq r15, [r14 + r15 * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get values of registers(rdi, rsi) and address of r15.
    ///
    /// ### in
    /// - rdi: register number
    /// - rsi: register number
    /// - r15: register number
    ///
    /// ### out
    /// - rdi: value of the register
    /// - rsi: value of the register
    /// - r15: address of the register
    ///
    fn vm_get_rr_r15(&mut self) {
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_slot_value(GP::Rsi);
        self.vm_get_slot_addr(GP::R15);
    }

    fn vm_get_ir_r15(&mut self) {
        self.fetch3();
        self.vm_get_smi_rdi();
        self.vm_get_slot_value(GP::Rsi); // rsi <- rhs addr
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
    }

    fn vm_get_ri_r15(&mut self) {
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi);
        self.vm_get_smi_rsi(); // rsi <- rhs addr
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
    }

    fn vm_store_r15(&mut self) {
        monoasm! { &mut self.jit,
            movq [r15], rax;
        };
    }

    fn vm_store_r15_if_nonzero(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq r15, r15;
            jeq exit;
        };
        self.vm_get_slot_addr(GP::R15);
        self.vm_store_r15();
        self.jit.bind_label(exit);
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
        let get_class = self.get_class;
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
        let get_class = self.get_class;
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
        let raise = self.entry_raise;
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq  raise;
        };
    }

    fn vm_generic_unop(&mut self, generic: DestLabel, func: UnaryOpFn) {
        self.jit.bind_label(generic);
        self.vm_save_lhs_class();
        self.call_unop(func);
        self.vm_handle_error();
        self.vm_store_r15();
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: DestLabel, func: BinaryOpFn) {
        self.jit.bind_label(generic);
        self.vm_save_binary_class();
        self.call_binop(func);
        self.vm_handle_error();
        self.vm_store_r15();
    }

    /// Expand array
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|src|dst|len||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// src: the source resister
    /// dst: the start of destination reginsters
    /// len: the number of destination registers
    /// ~~~
    fn vm_expand_array(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        monoasm! { &mut self.jit,
            // rdx <- len
            movq rdx, rsi;
            // rsi <- dst
            negq rdi;
            lea rsi, [r14 + rdi * 8 - (LBP_SELF)];
            // rdi <- *src
            negq r15;
            movq rdi, [r14 + r15 * 8 - (LBP_SELF)];
            movq rax, (runtime::expand_array);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    /// Alias method
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|   |new|old||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// new: a register for a new symbol
    /// old: a register for a old symbol
    /// ~~~
    fn vm_alias_method(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        monoasm! { &mut self.jit,
            movl rcx, rdi;  // new
            negq rcx;
            movq rcx, [r14 + rcx * 8 - (LBP_SELF)];
            movl r8, rsi;  // old
            negq r8;
            movq r8, [r14 + r8 * 8 - (LBP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LBP_SELF)];
            movq r9, [r14 - (LBP_META)];
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
            movq rdx, rsi;
            movq rsi, rdi;
            movq rdi, r12;
            movq rax, (runtime::concatenate_string);
            call rax;
        };
        self.vm_store_r15_if_nonzero();
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
        self.vm_store_r15_if_nonzero();
        self.fetch_and_dispatch();
        label
    }

    fn vm_loop_start(&mut self, no_jit: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let count = self.jit.label();
        let compile = self.jit.label();
        if !no_jit {
            monoasm! { &mut self.jit,
                movq rax, [r13 - 8];
                testq rax, rax;
                jeq count;
                jmp rax;
            count:
                addl [r13 - 16], 1;
                cmpl [r13 - 16], 5;
                jae   compile;
            };
        };
        self.fetch_and_dispatch();
        if !no_jit {
            monoasm!( &mut self.jit,
            compile:
                movq rdi, rbx;
                movq rsi, r12;
                lea  rdx, [r13 - 16];
                movq rax, (exec_jit_partial_compile);
                call rax;
                jmp rax;
            );
        }
        label
    }

    fn vm_loop_end(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_and_dispatch();
        label
    }

    fn vm_nil(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch_addr_r15();
        monoasm! { &mut self.jit,
            movq [r15], (NIL_VALUE);
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_integer(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_symbol(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            shlq rdi, 32;
            orq rdi, (TAG_SYMBOL);
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_literal(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            movq rdi, [r13 - 8];
            movq rax, (Value::value_deep_copy);
            call rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_array(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            movl rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            lea  rcx, [r14 - (LBP_SELF)];
            movq rax, (runtime::gen_array);
            call rax;
        };
        self.vm_store_r15();
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
            movzxw rdi, [r13 - 12];  // r15 <- :1
            negq rdi;
            lea  rdi, [r14 + rdi * 8 - (LBP_SELF)];
            movq [rdi], rax;
        };
        //self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_hash(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_addr(GP::R15);
        self.vm_get_slot_addr(GP::Rdi);
        monoasm! { &mut self.jit,
            // src: *const Value
            movzxw rsi, rsi;  // len: usize
            movq rax, (runtime::gen_hash);
            call rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_range(&mut self, exclude_end: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_rr_r15();
        monoasm! { &mut self.jit,
            movq rdx, rbx;
            movq rcx, r12;
            movl r8, (if exclude_end {1} else {0});
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15();
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
        self.vm_get_rr_r15();
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
        self.vm_store_r15();
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
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            lea  rax, [r14 - (LBP_OUTER)];
            testq rdi, rdi;
            jz   loop_exit;
        loop_:
            movq rax, [rax];
            subl rdi, 1;
            jnz  loop_;
        loop_exit:
            lea  rax, [rax + (LBP_OUTER)];
            movq rax, [rax - (LBP_BLOCK)];
            movq rdi, (Value::nil().id());
            testq rax, rax;
            cmoveqq rax, rdi;
            testq rax, 0b1;
            jeq exit;
            addq rax, 0b10;
        exit:
        };
        self.vm_store_r15();
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
        let raise = self.entry_raise;
        self.fetch2();
        self.vm_get_slot_addr(GP::R15);
        monoasm! { &mut self.jit,
            lea  rax, [r14 - (LBP_OUTER)];
            testq rdi, rdi;
            jz   loop_exit;
        loop_:
            movq rax, [rax];
            subl rdi, 1;
            jnz  loop_;
        loop_exit:
            lea  rax, [rax + (LBP_OUTER)];
            movq rdx, [rax - (LBP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
            testq rax, rax;
            jz raise;
        };
        self.vm_store_r15();
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
        };
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
            movq rdx, r15;
            movl rcx, [r13 - 8];
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::defined_gvar);
            call rax;
        };
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

    fn vm_not(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
        self.not_rdi_to_rax();
        monoasm! { &mut self.jit,
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_bitnot(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
        self.vm_generic_unop(generic, bitnot_value);
        label
    }

    fn vm_neg(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
        self.guard_rdi_fixnum(generic);
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            negq rdi;
            lea rdi, [rdi + rdi + 1];
            movq [r15], rdi;
        };
        self.vm_lhs_integer();
        self.fetch_and_dispatch();
        self.vm_generic_unop(generic, neg_value);
        label
    }

    fn vm_pos(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.fetch3();
        self.vm_get_slot_value(GP::Rdi); // rdi <- lhs
        self.vm_get_slot_addr(GP::R15); // r15 <- ret addr
        monoasm! { &mut self.jit,
            testq rdi, 0x1;
            jz generic;
            movq [r15], rdi;
        }
        self.fetch_and_dispatch();
        self.vm_generic_unop(generic, pos_value);
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
    ) -> (CodePtr, CodePtr, CodePtr) {
        let common = self.jit.label();
        let ptr_rr = self.jit.get_current_address();
        let generic = self.jit.label();
        let exit = self.jit.label();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        self.jit.bind_label(common);
        self.vm_save_binary_integer();
        opt_func(self, generic);
        self.vm_store_r15();
        self.jit.bind_label(exit);
        self.fetch_and_dispatch();

        self.vm_generic_binop(generic, generic_func);
        monoasm! { &mut self.jit, jmp exit; };

        let ptr_ri = self.jit.get_current_address();
        self.vm_get_ri_r15();
        self.guard_rdi_fixnum(generic);
        monoasm!( &mut self.jit,
            jmp common;
        );

        let ptr_ir = self.jit.get_current_address();
        self.vm_get_ir_r15();
        self.guard_rsi_fixnum(generic);
        monoasm!( &mut self.jit,
            jmp common;
        );

        (ptr_rr, ptr_ri, ptr_ir)
    }

    fn vm_binops(&mut self, func: BinaryOpFn) -> (CodePtr, CodePtr, CodePtr) {
        let common = self.jit.label();
        let ptr_rr = self.jit.get_current_address();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.vm_generic_binop(common, func);
        self.fetch_and_dispatch();

        let ptr_ri = self.jit.get_current_address();
        self.vm_get_ri_r15();
        monoasm!( &mut self.jit, jmp common; );

        let ptr_ir = self.jit.get_current_address();
        self.vm_get_ir_r15();
        monoasm!( &mut self.jit, jmp common; );

        (ptr_rr, ptr_ri, ptr_ir)
    }

    fn vm_check_local(&mut self, branch: DestLabel) -> CodePtr {
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

    fn vm_condbr(&mut self, branch: DestLabel) -> CodePtr {
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

    fn vm_nilbr(&mut self, branch: DestLabel) -> CodePtr {
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

    fn vm_condnotbr(&mut self, branch: DestLabel) -> CodePtr {
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

    fn vm_optcase(&mut self, branch: DestLabel) -> CodePtr {
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
