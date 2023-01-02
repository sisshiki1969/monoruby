use super::*;
use monoasm_macro::monoasm;
use paste::paste;

mod init_method;
mod method_call;
mod variables;

macro_rules! cmp_ops {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op rr>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic0 = self.jit.label();
              let generic1 = self.jit.label();
              self.vm_get_rdi(); // rdi <- lhs addr
              self.vm_get_rsi(); // rsi <- rhs addr
              self.vm_get_addr_r15(); // r15 <- ret addr
              self.guard_rdi_fixnum(generic0);
              self.guard_rsi_fixnum(generic0);
              self.vm_save_binary_integer();
              self.[<vm_cmp_ $op>](generic1);
              self.vm_store_r15();
              self.fetch_and_dispatch();
              self.jit.select_page(1);
              self.jit.bind_label(generic0);
              self.vm_save_binary_class();
              monoasm!(self.jit,
                  jmp  generic1;
              );
              self.jit.select_page(0);
              label
          }

          fn [<vm_ $op ri>](&mut self) -> CodePtr {
            let label = self.jit.get_current_address();
            let generic0 = self.jit.label();
            let generic1 = self.jit.label();
            self.vm_get_rdi();      // rdi <- lhs addr
            self.vm_get_addr_r15(); // r15 <- ret addr
            monoasm! { self.jit,
              shlq rsi, 1;
              orq  rsi, 1;
            };
            self.guard_rdi_fixnum(generic0);
            self.vm_save_binary_integer();
            self.[<vm_cmp_ $op>](generic1);
            self.vm_store_r15();
            self.fetch_and_dispatch();
            self.jit.select_page(1);
            self.jit.bind_label(generic0);
            self.vm_save_lhs_class();
            monoasm!(self.jit,
                jmp  generic1;
            );
            self.jit.select_page(0);
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
    pub(super) fn gen_entry_point(&mut self, main_object: Value) {
        let entry = self.jit.get_current_address();
        monoasm! { self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            pushq rbp;
            subq rsp, 8;

            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
            // set meta func_id
            movq rax, [rdx + (FUNCDATA_OFFSET_META)];  // rdx: *const FuncData
            movq [rsp - (16 + LBP_META)], rax;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_OUTER)], 0;
            movq [rsp - (16 + BP_PREV_CFP)], 0;
            lea  rax, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx], rax;
            // set pc
            movq r13, [rdx + (FUNCDATA_OFFSET_PC)];
        }
        self.set_lfp();
        monoasm! {self.jit,
            //
            //       +-------------+
            //  0x00 |             | <- rsp
            //       +-------------+
            // -0x08 | return addr |
            //       +-------------+
            // -0x10 |   old rbp   |
            //       +-------------+
            // -0x18 |    meta     |
            //       +-------------+
            // -0x20 |    block    |
            //       +-------------+
            // -0x28 |     %0      |
            //       +-------------+
            // -0x30 | %1(1st arg) |
            //       +-------------+
            //       |             |
            //
            // set self
            movq rax, (main_object.get());
            movq [rsp - (16 + LBP_SELF)], rax;
            movq rax, [rdx + (FUNCDATA_OFFSET_CODEPTR)];
            xorq rdi, rdi;
            call rax;
            // pop frame
            movq [rbx], 0;
            addq rsp, 8;
            popq rbp;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };

        self.entry_point = unsafe { std::mem::transmute(entry.as_ptr()) };
    }

    ///
    /// Generator of virtual machine.
    ///
    pub(super) fn construct_vm(&mut self, no_jit: bool) {
        let entry = self.jit.label();
        //
        // VM entry
        //
        // argument registers:
        //   rdi: args len
        //
        // global registers:
        //   rbx: &mut Interp
        //   r12: &mut Globals
        //   r13: pc
        //

        monoasm! { self.jit,
        entry:
            pushq rbp;
            movq rbp, rsp;
            movq rdx, rdi;
        };
        let entry_fetch = self.jit.label();
        self.jit.bind_label(entry_fetch);
        self.vm_fetch = entry_fetch;
        self.fetch_and_dispatch();

        self.vm_entry = entry;

        //BcOp::Ret
        let ret = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Mov
        let mov = self.jit.get_current_address();
        self.vm_get_addr_r15();
        self.vm_get_rdi();
        monoasm! { self.jit,
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();

        let br_inst = self.jit.get_current_address();
        let branch = self.jit.label();
        monoasm! { self.jit,
        branch:
            shlq rdi, 4;
            lea r13, [r13 + rdi];
        };
        self.fetch_and_dispatch();

        let (shl, shr) = self.vm_shift();
        let (add_rr, add_ri, add_ir) = self.vm_binops_opt(Self::int_add, add_values as _);
        let (sub_rr, sub_ri, sub_ir) = self.vm_binops_opt(Self::int_sub, sub_values as _);
        let (div_rr, div_ri, div_ir) = self.vm_binops(div_values as _);
        let (mul_rr, mul_ri, mul_ir) = self.vm_binops(mul_values as _);
        let (pow_rr, pow_ri, pow_ir) = self.vm_binops(pow_values as _);

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
        self.dispatch[18] = self.vm_class_def();
        self.dispatch[20] = self.vm_check_local(branch);
        self.dispatch[21] = self.vm_block_arg_proxy();
        self.dispatch[25] = self.vm_load_gvar();
        self.dispatch[26] = self.vm_store_gvar();
        self.dispatch[27] = self.vm_splat();
        self.dispatch[30] = self.vm_method_call(false, true);
        self.dispatch[31] = self.vm_method_call(false, false);
        self.dispatch[32] = self.vm_method_call(true, true);
        self.dispatch[33] = self.vm_method_call(true, false);

        self.dispatch[129] = self.vm_neg();
        self.dispatch[131] = self.vm_array();
        self.dispatch[132] = self.vm_index();
        self.dispatch[133] = self.vm_index_assign();
        self.dispatch[134] = self.vm_eqrr();
        self.dispatch[135] = self.vm_nerr();
        self.dispatch[136] = self.vm_ltrr();
        self.dispatch[137] = self.vm_lerr();
        self.dispatch[138] = self.vm_gtrr();
        self.dispatch[139] = self.vm_gerr();

        self.dispatch[142] = self.vm_eqri();
        self.dispatch[143] = self.vm_neri();
        self.dispatch[144] = self.vm_ltri();
        self.dispatch[145] = self.vm_leri();
        self.dispatch[146] = self.vm_gtri();
        self.dispatch[147] = self.vm_geri();

        self.dispatch[148] = ret;
        self.dispatch[149] = mov;

        self.dispatch[150] = self.vm_load_dvar();
        self.dispatch[151] = self.vm_store_dvar();
        self.dispatch[152] = self.vm_yield();
        self.dispatch[153] = self.vm_range(false);
        self.dispatch[154] = self.vm_range(true);
        self.dispatch[155] = self.vm_concat();

        self.dispatch[156] = self.vm_eqrr();
        self.dispatch[157] = self.vm_nerr();
        self.dispatch[158] = self.vm_ltrr();
        self.dispatch[159] = self.vm_lerr();
        self.dispatch[160] = self.vm_gtrr();
        self.dispatch[161] = self.vm_gerr();
        self.dispatch[162] = self.vm_eqri();
        self.dispatch[163] = self.vm_neri();
        self.dispatch[164] = self.vm_ltri();
        self.dispatch[165] = self.vm_leri();
        self.dispatch[166] = self.vm_gtri();
        self.dispatch[167] = self.vm_geri();

        self.dispatch[170] = self.vm_init_method();
        self.dispatch[171] = self.vm_expand_array();
        self.dispatch[172] = self.vm_init_block();
        self.dispatch[173] = self.vm_alias_method();

        self.dispatch[180] = add_ir;
        self.dispatch[181] = sub_ir;
        self.dispatch[182] = mul_ir;
        self.dispatch[183] = div_ir;
        self.dispatch[190] = pow_ir;

        self.dispatch[200] = add_rr;
        self.dispatch[201] = sub_rr;
        self.dispatch[202] = mul_rr;
        self.dispatch[203] = div_rr;
        self.dispatch[204] = self.vm_bitorrr();
        self.dispatch[205] = self.vm_bitandrr();
        self.dispatch[206] = self.vm_bitxorrr();
        self.dispatch[207] = shr;
        self.dispatch[208] = shl;
        self.dispatch[209] = self.vm_remrr();
        self.dispatch[210] = pow_rr;

        self.dispatch[220] = add_ri;
        self.dispatch[221] = sub_ri;
        self.dispatch[222] = mul_ri;
        self.dispatch[223] = div_ri;
        self.dispatch[230] = pow_ri;

        // method invoker.
        self.method_invoker =
            unsafe { std::mem::transmute(self.jit.get_current_address().as_ptr()) };
        // rdi: &mut Interp
        // rsi: &mut Globals
        // rdx: *const FuncData
        // rcx: receiver: Value
        // r8:  *args: *const Value
        // r9:  len: usize

        self.gen_invoker_prologue(false);
        self.gen_invoker_prep();
        self.gen_invoker_epilogue();

        // block invoker.
        self.block_invoker =
            unsafe { std::mem::transmute(self.jit.get_current_address().as_ptr()) };
        // rdi: &mut Interp
        // rsi: &mut Globals
        // rdx: *const FuncData
        // rcx: <dummy>
        // r8:  *args: *const Value
        // r9:  len: usize
        self.gen_invoker_prologue(true);
        self.gen_invoker_prep();
        self.gen_invoker_epilogue();

        // method invoker.
        self.method_invoker2 =
            unsafe { std::mem::transmute(self.jit.get_current_address().as_ptr()) };
        // rdi: &mut Interp
        // rsi: &mut Globals
        // rdx: *const FuncData
        // rcx: receiver: Value
        // r8:  args: Arg
        // r9:  len: usize
        self.gen_invoker_prologue(false);
        self.gen_invoker_prep2();
        self.gen_invoker_epilogue();
    }

    fn gen_invoker_prologue(&mut self, invoke_block: bool) {
        // rdi: &mut Interp
        // rsi: &mut Globals
        // rdx: (method)*const FuncData
        // rdx: (block) *const BlockData
        if invoke_block {
            monoasm! { self.jit,
                movq rax, [rdx];        // rax <- outer_lfp
                movq rdx, [rdx + 8];    // rdx <- &FuncData
            };
        }
        monoasm! { self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            movq rbx, rdi;
            movq r12, rsi;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
            // set meta
            movq rdi, [rdx + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            // set pc
            movq r13, [rdx + (FUNCDATA_OFFSET_PC)];
        };
        if invoke_block {
            self.set_block_self_outer()
        } else {
            monoasm! { self.jit,
                movq [rsp - (16 + LBP_SELF)], rcx;
            }
            self.set_method_outer()
        }
    }

    fn gen_invoker_epilogue(&mut self) {
        monoasm! { self.jit,
            movq rax, [rdx + (FUNCDATA_OFFSET_CODEPTR)];
        }
        self.push_frame();
        self.set_lfp();
        monoasm! { self.jit,
            call rax;
            movq rdi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx], rdi;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };
    }

    fn gen_invoker_prep(&mut self) {
        let loop_exit = self.jit.label();
        let loop_ = self.jit.label();
        monoasm! { self.jit,
            // r8 <- *args
            // r9 <- len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            movq r10, r9;
            negq r9;
        loop_:
            movq rax, [r8 + r10 * 8 - 8];
            movq [rsp + r9 * 8 - (16 + LBP_SELF)], rax;
            subq r10, 1;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    fn gen_invoker_prep2(&mut self) {
        let loop_exit = self.jit.label();
        let loop_ = self.jit.label();
        monoasm! { self.jit,
            // r8 <- *args
            // r9 <- len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            negq r9;
        loop_:
            movq rax, [r8 + r9 * 8 + 8];
            movq [rsp + r9 * 8 - (16 + LBP_SELF)], rax;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    ///
    /// Fetch instruction and decode
    ///
    /// #### requirement:
    /// - *r13*: BcPc
    ///
    /// #### returns:
    /// - *r15d*: :1
    /// - *edi*: :2 or *rdi*: :2:3
    /// - *esi*: :3
    ///
    /// ### registers destroyed
    /// - rax, r8
    ///
    fn fetch_and_dispatch(&mut self) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            movq r8, (self.dispatch.as_ptr());
            addq r13, 16;
            movzxw rax, [r13 - 10]; // rax <- :0
            movzxw r15, [r13 - 12];  // r15 <- :1
            // dispatch
            testq rax, 0x80;
            jeq l1;
            movsxw rsi, [r13 - 16];    // rsi <- :3
            movzxw rdi, [r13 - 14];    // rdi <- :2
            movq rax, [r8 + rax * 8];
            jmp rax;
        l1:
            movsxl rdi, [r13 - 16];  // rdi <- :2:3
            movq rax, [r8 + rax * 8];
            jmp rax;
        };
    }

    fn vm_handle_error(&mut self) {
        let entry_return = self.vm_return;
        monoasm! { self.jit,
            testq rax, rax;
            jeq  entry_return;
        };
    }

    ///
    /// Get absolute address of the register.
    /// #### args
    /// - *rdi*: register number
    /// #### return
    /// - *rdi*: absolute address of the register
    ///
    fn vm_get_addr_rdi(&mut self) {
        monoasm! { self.jit,
            negq rdi;
            lea rdi, [r14 + rdi * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get absolute address of the register.
    /// #### args
    /// - *rcx*: register number
    /// #### return
    /// - *rcx*: absolute address of the register
    ///
    fn vm_get_addr_rcx(&mut self) {
        monoasm! { self.jit,
            negq rcx;
            lea rcx, [r14 + rcx * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get value of the register.
    /// #### args
    /// - *rdi*: register number
    /// #### return
    /// - *rdi*: value of the register
    ///
    fn vm_get_rdi(&mut self) {
        monoasm! { self.jit,
            negq rdi;
            movq rdi, [r14 + rdi * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get value of the register.
    /// #### args
    /// - *rsi*: register number
    /// #### return
    /// - *rsi*: value of the register
    ///
    fn vm_get_rsi(&mut self) {
        monoasm! { self.jit,
            negq rsi;
            movq rsi, [r14 + rsi * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get value of the register.
    /// #### args
    /// - *r15*: register number
    /// #### return
    /// - *r15*: value of the register
    ///
    fn vm_get_r15(&mut self) {
        monoasm! { self.jit,
            negq r15;
            movq r15, [r14 + r15 * 8 - (LBP_SELF)];
        };
    }

    fn vm_get_smi_rdi(&mut self) {
        monoasm! { self.jit,
            movsxw rdi, rdi;
            shlq rdi, 1;
            addq rdi, 1;
        };
    }

    fn vm_get_smi_rsi(&mut self) {
        monoasm! { self.jit,
            movsxw rsi, rsi;
            shlq rsi, 1;
            addq rsi, 1;
        };
    }

    ///
    /// Get address of the register.
    /// #### args
    /// - *r15*: register number
    /// #### return
    /// - *r15*: address of the register
    ///
    fn vm_get_addr_r15(&mut self) {
        monoasm! { self.jit,
            negq r15;
            lea r15, [r14 + r15 * 8 - (LBP_SELF)];
        };
    }

    ///
    /// Get values of registers(rdi, rsi) and address of r15.
    /// #### args
    /// - *rdi*: register number
    /// - *rsi*: register number
    /// - *r15*: register number
    /// #### return
    /// - *rdi*: value of the register
    /// - *rsi*: value of the register
    /// - *r15*: address of the register
    ///
    fn vm_get_rr_r15(&mut self) {
        self.vm_get_rdi();
        self.vm_get_rsi();
        self.vm_get_addr_r15();
    }

    fn vm_get_ir_r15(&mut self) {
        self.vm_get_smi_rdi();
        self.vm_get_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
    }

    fn vm_get_ri_r15(&mut self) {
        self.vm_get_rdi();
        self.vm_get_smi_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
    }

    fn vm_store_r15(&mut self) {
        monoasm! { self.jit,
            movq [r15], rax;
        };
    }

    fn vm_store_r15_if_nonzero(&mut self, exit: DestLabel) {
        monoasm! { self.jit,
            testq r15, r15;
            jeq exit;
        };
        self.vm_get_addr_r15();
        self.vm_store_r15();
        self.jit.bind_label(exit);
    }

    fn vm_save_lhs_class(&mut self) {
        monoasm! { self.jit,
            pushq rdi;
            pushq rsi;
            movq  rax, (Value::get_class);
            call rax;
            movl  [r13 - 8], rax;
            popq  rsi;
            popq  rdi;
        };
    }

    /*fn vm_save_rhs_class(&mut self) {
        monoasm! { self.jit,
            pushq rdi;
            pushq rsi;
            movq  rdi, rsi;
            movq  rax, (Value::get_class);
            call  rax;
            movl  [r13 - 4], rax;
            popq  rsi;
            popq  rdi;
        };
    }*/

    fn vm_save_binary_class(&mut self) {
        monoasm! { self.jit,
            pushq rdi;
            pushq rsi;
            movq  rax, (Value::get_class);
            call rax;
            movl  [r13 - 8], rax;
            movq  rdi, [rsp];
            movq  rax, (Value::get_class);
            call rax;
            movl  [r13 - 4], rax;
            popq  rsi;
            popq  rdi;
        };
    }

    fn vm_save_binary_integer(&mut self) {
        let int_class: u32 = INTEGER_CLASS.into();
        monoasm! { self.jit,
            movl  [r13 - 8], (int_class);
            movl  [r13 - 4], (int_class);
        };
    }

    fn vm_generic_unop(&mut self, generic: DestLabel, func: usize) {
        self.jit.bind_label(generic);
        self.vm_save_lhs_class();
        self.call_unop(func);
        self.vm_handle_error();
        self.vm_store_r15();
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: DestLabel, exit: DestLabel, func: usize) {
        self.jit.bind_label(generic);
        self.vm_save_binary_class();
        self.call_binop(func);
        self.vm_handle_error();
        self.vm_store_r15();
        monoasm! { self.jit,
            jmp exit;
        };
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
        monoasm! { self.jit,
            // rdx <- len
            movq rdx, rsi;
            // rsi <- dst
            negq rdi;
            lea rsi, [r14 + rdi * 8 - (LBP_SELF)];
            // rdi <- *src
            negq r15;
            movq rdi, [r14 + r15 * 8 - (LBP_SELF)];
            movq rax, (expand_array);
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
        monoasm! { self.jit,
            movl rdx, rdi;
            negq rdx;
            movq rdx, [r14 + rdx * 8 - (LBP_SELF)];
            movl rcx, rsi;
            negq rcx;
            movq rcx, [r14 + rcx * 8 - (LBP_SELF)];
            movq rdi, r12;
            movq rsi, [r14 - (LBP_SELF)];
            movq r8, [r14 - (LBP_META)];
            movq rax, (alias_method);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    /// Make splat
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|src|       ||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// src: the source resister
    /// ~~~
    fn vm_splat(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        monoasm! { self.jit,
            // rdi <- *mut Value
            negq r15;
            lea rdi, [r14 + r15 * 8 - (LBP_SELF)];
            movq rax, (make_splat);
            call rax;
        };
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
        let exit = self.jit.label();
        self.vm_get_addr_rdi();
        monoasm! { self.jit,
            movq rdx, rsi;
            movq rsi, rdi;
            movq rdi, r12;
            movq rax, (concatenate_string);
            call rax;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
        label
    }

    fn vm_loop_start(&mut self, no_jit: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let count = self.jit.label();
        let compile = self.jit.label();
        if !no_jit {
            monoasm! { self.jit,
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
            monoasm!(self.jit,
            compile:
                movq rdi, r12;
                movl rsi, [r14 - (LBP_META_FUNCID)];
                movq rdx, [r14 - (LBP_SELF)];
                lea rcx, [r13 - 16];
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
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq [r15], (NIL_VALUE);
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_integer(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_symbol(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            shlq rdi, 32;
            orq rdi, (TAG_SYMBOL);
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_literal(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdi, [r13 - 8];
            movq rax, (Value::deep_copy);
            call rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_array(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        self.vm_get_addr_rdi();
        monoasm! { self.jit,
            // src: *const Value
            movzxw rsi, rsi;  // len: usize
            movq rax, (gen_array);
            call rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_range(&mut self, exclude_end: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        self.vm_get_rdi();
        self.vm_get_rsi();
        monoasm! { self.jit,
            movq rdx, r12;
            movl rcx, (if exclude_end {1} else {0});
            movq rax, (gen_range);
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
        self.vm_get_addr_r15();
        self.vm_get_rdi();
        self.vm_get_rsi();
        monoasm! { self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            lea  r8, [r13 - 8]; // &mut ClassId
            movq rax, (get_index);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_index_assign(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_r15();
        self.vm_get_rdi();
        self.vm_get_rsi();
        monoasm! { self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq r8, r15;  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            lea  r9, [r13 + 8];
            movq rax, (set_index);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|dst|       ||               |
    // +---+---+---+---++---+---+---+---+
    //
    fn vm_block_arg_proxy(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let panic = self.entry_panic;
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rax, [r14 - (LBP_BLOCK)];
            testq rax, 0b1;
            jeq panic;
            addq rax, 0b10;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_neg(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_fixnum(generic);
        monoasm! { self.jit,
            sarq rdi, 1;
            negq rdi;
            lea rdi, [rdi + rdi + 1];
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_unop(generic, neg_value as _);
        label
    }

    fn int_add(&mut self, generic: DestLabel) {
        monoasm! { self.jit,
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
        };
    }

    fn int_sub(&mut self, generic: DestLabel) {
        monoasm! { self.jit,
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
        };
    }

    fn vm_binops_opt(
        &mut self,
        opt_func: fn(&mut Codegen, DestLabel),
        generic_func: usize,
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
        self.vm_generic_binop(generic, exit, generic_func);

        let ptr_ri = self.jit.get_current_address();
        self.vm_get_ri_r15();
        self.guard_rdi_fixnum(generic);
        monoasm!(self.jit,
            jmp common;
        );

        let ptr_ir = self.jit.get_current_address();
        self.vm_get_ir_r15();
        self.guard_rsi_fixnum(generic);
        monoasm!(self.jit,
            jmp common;
        );

        (ptr_rr, ptr_ri, ptr_ir)
    }

    fn vm_binops(&mut self, func: usize) -> (CodePtr, CodePtr, CodePtr) {
        let common = self.jit.label();
        let ptr_rr = self.jit.get_current_address();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.jit.bind_label(common);
        self.vm_save_binary_class();
        self.call_binop(func);
        self.vm_handle_error();
        self.vm_store_r15();
        self.fetch_and_dispatch();

        let ptr_ri = self.jit.get_current_address();
        self.vm_get_ri_r15();
        monoasm!(self.jit, jmp common; );

        let ptr_ir = self.jit.get_current_address();
        self.vm_get_ir_r15();
        monoasm!(self.jit, jmp common; );

        (ptr_rr, ptr_ri, ptr_ir)
    }

    // TODO: result in not correct when lhs < 0 or rhs < 0.
    fn vm_remrr(&mut self) -> CodePtr {
        let common = self.jit.label();
        let ptr_rr = self.jit.get_current_address();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.jit.bind_label(common);
        self.vm_save_binary_class();
        self.call_binop(rem_values as _);
        self.vm_handle_error();
        self.vm_store_r15();
        self.fetch_and_dispatch();
        ptr_rr
    }

    fn vm_bitorrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        let exit = self.jit.label();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        self.vm_save_binary_integer();
        monoasm! { self.jit,
            orq rdi, rsi;
            movq [r15], rdi;
        exit:
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, exit, bitor_values as _);
        label
    }

    fn vm_bitandrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        let exit = self.jit.label();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        self.vm_save_binary_integer();
        monoasm! { self.jit,
            andq rdi, rsi;
            movq [r15], rdi;
        exit:
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, exit, bitand_values as _);
        label
    }

    fn vm_bitxorrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        let exit = self.jit.label();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        self.vm_save_binary_integer();
        monoasm! { self.jit,
            xorq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        exit:
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, exit, bitxor_values as _);
        label
    }

    fn vm_shift(&mut self) -> (CodePtr, CodePtr) {
        let shl_label = self.jit.get_current_address();
        let generic_shl = self.jit.label();
        let generic_shr = self.jit.label();
        let exit = self.jit.label();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.vm_generic_binop(generic_shl, exit, shl_values as _);

        let shr_label = self.jit.get_current_address();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.vm_generic_binop(generic_shr, exit, shr_values as _);

        self.jit.bind_label(exit);
        self.fetch_and_dispatch();

        (shl_label, shr_label)
    }

    cmp_ops!(eq, ne, gt, ge, lt, le);

    fn vm_method_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let class_version = self.class_version;
        monoasm! { self.jit,
            movl rdx, [r13 - 8];  // name
            movl rcx, [r13 - 4];  // func_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_method);
            call rax;
            addl [rip + class_version], 1;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_class_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let vm_return = self.vm_return;
        let super_ = self.jit.label();
        monoasm! { self.jit,
            cmpl rdi, 0;
            jeq super_;
        }
        self.vm_get_rdi();
        monoasm! { self.jit,
        super_:
            movq rcx, rdi;
            movl rdx, [r13 - 8];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_class);
            call rax;  // rax <- self: Value
            pushq r13;
            pushq r15;
            testq rax, rax; // rax: Option<Value>
            jeq  vm_return;
            movq r15, rax; // r15 <- self
            movl rsi, [r13 - 4];  // rdx <- func_id
            //movq rdi, rbx;  // &mut Interp
            movq rdi, r12;  // &mut Globals
            movq rax, (get_func_data);
            call rax; // rax <- &FuncData
            //
            //       +-------------+
            // +0x08 |     pc      |
            //       +-------------+
            //  0x00 |   ret reg   | <- rsp
            //       +-------------+
            // -0x08 | return addr |
            //       +-------------+
            // -0x10 |   old rbp   |
            //       +-------------+
            // -0x18 |    meta     |
            //       +-------------+
            // -0x20 |    block    |
            //       +-------------+
            // -0x28 |     %0      |
            //       +-------------+
            // -0x30 | %1(1st arg) | <- rdx
            //       +-------------+
            //       |             |
            //
            movq r8, rax;
            movq rdi, [r8 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_SELF)], r15;
        };
        self.set_method_outer();
        monoasm! { self.jit,
            movq r13 , [r8 + (FUNCDATA_OFFSET_PC)];
            movq rax, [r8 + (FUNCDATA_OFFSET_CODEPTR)];
            xorq rdi, rdi;
        };
        self.call_rax();
        monoasm! { self.jit,
            popq r15;
            popq r13;
            testq rax, rax;
            jeq vm_return;
        };
        let exit = self.jit.label();
        self.vm_store_r15_if_nonzero(exit);
        // pop class context.
        monoasm!(self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (pop_class_context);
            call rax;
        );
        self.fetch_and_dispatch();
        label
    }

    fn vm_check_local(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq r15, [r15];
            testq r15, r15;
            jne  branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jne branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condnotbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jeq branch;
        };
        self.fetch_and_dispatch();
        label
    }
}
