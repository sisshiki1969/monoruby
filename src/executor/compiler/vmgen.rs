use super::*;
use monoasm_macro::monoasm;
use paste::paste;

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
              self.[<cmp_ $op>](generic1, None);
              self.vm_store_r15();
              self.fetch_and_dispatch();
              self.jit.select(1);
              self.jit.bind_label(generic0);
              self.vm_save_binary_class();
              monoasm!(self.jit,
                  jmp  generic1;
              );
              self.jit.select(0);
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
            self.[<cmp_ $op>](generic1, None);
            self.vm_store_r15();
            self.fetch_and_dispatch();
            self.jit.select(1);
            self.jit.bind_label(generic0);
            self.vm_save_lhs_class();
            monoasm!(self.jit,
                jmp  generic1;
            );
            self.jit.select(0);
            label
          }
      }
  };
  ($op1:ident, $($op2:ident),+) => {
      cmp_ops!($op1);
      cmp_ops!($($op2),+);
  };
}

extern "C" fn vm_define_method(
    _interp: &mut Interp,
    globals: &mut Globals,
    name: IdentId,
    func_id: FuncId,
) {
    globals.class.add_method(OBJECT_CLASS, name, func_id);
}

impl Codegen {
    pub(super) fn get_entry_point(&mut self) {
        let entry = self.jit.get_current_address();
        let return_label = self.jit.label();
        monoasm! { self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;

            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
            // set meta func_id
            movq rax, [rdx + (FUNCDATA_OFFSET_META)];  // rdx: *const FuncData
            movq [rsp - 0x18], rax;
            movq r13, [rdx + (FUNCDATA_OFFSET_PC)];    // r13: BcPc
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
            // -0x20 |     %0      |
            //       +-------------+
            // -0x28 | %1(1st arg) |
            //       +-------------+
            //       |             |
            //
            // set self
            movq [rsp - 0x20], (NIL_VALUE);
            movq rax, [rdx + (FUNCDATA_OFFSET_CODEPTR)];
            xorq rdi, rdi;
            call rax;
        return_label:
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };

        self.entry_point = unsafe { std::mem::transmute(entry.as_ptr()) };
        self.entry_point_return = self.jit.get_label_address(return_label);
    }

    ///
    /// Generator of virtual machine.
    ///
    pub(super) fn construct_vm(&mut self, no_jit: bool) {
        let entry = self.jit.label();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
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
            movw [rbp - 0x02], 0;       // kind
            movzxw rax, [rbp - 0x04];   // reg_num
            movq rdx, rax;  // rdx = reg_num

            subq rdx, rdi;
            subq rdx, 1;    // rax = reg_num - 1 - args_len
            jeq  loop_exit;
            movq rdi, rax;
            negq rdi;
            lea  rcx, [rsp + rdi * 8 - 16];
        loop_:
            movq [rcx + rdx * 8], (FALSE_VALUE);
            subq rdx, 1;
            jne  loop_;
        loop_exit:
            //movq rax, rdx;
        };
        self.calc_offset();
        monoasm! { self.jit,
            subq rsp, rax;
        };
        let entry_fetch = self.jit.label();
        self.jit.bind_label(entry_fetch);
        self.vm_fetch = entry_fetch;
        self.fetch_and_dispatch();

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

        self.vm_entry = entry;
        self.dispatch[1] = self.vm_method_call();
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

        self.dispatch[170] = add_rr;
        self.dispatch[171] = sub_rr;
        self.dispatch[172] = mul_rr;
        self.dispatch[173] = div_rr;
        self.dispatch[174] = self.vm_bitorrr();
        self.dispatch[175] = self.vm_bitandrr();
        self.dispatch[176] = self.vm_bitxorrr();
        self.dispatch[177] = shr;
        self.dispatch[178] = shl;

        self.dispatch[180] = add_ir;
        self.dispatch[181] = sub_ir;
        self.dispatch[182] = mul_ir;
        self.dispatch[183] = div_ir;

        self.dispatch[190] = add_ri;
        self.dispatch[191] = sub_ri;
        self.dispatch[192] = mul_ri;
        self.dispatch[193] = div_ri;
    }

    ///
    /// Fetch instruction and decode
    ///
    /// #### requirement:
    /// - *r13*: BcPc
    ///
    /// #### returns:
    /// - *eax*:  :0
    /// - *r15d*: :1
    /// - *edi*: :2 or *rdi*: :2:3
    /// - *esi*: :3
    ///
    /// ### registers destroyed
    /// - r8, r9
    ///
    pub fn fetch_and_dispatch(&mut self) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            movq rax, [r13]; // rax <- :0:1:2:3
            addq r13, 16;
            movsxl rdi, rax;  // rdi <- :2:3
            shrq rax, 32;
            movzxw r15, rax;  // r15 <- :1
            shrq rax, 16;
            movzxw rax, rax;   // rax <- :0
            // dispatch
            testq rax, 0x80;
            jeq l1;
            movsxw rsi, rdi;    // rsi <- :3
            shrq rdi, 16;
            movzxw rdi, rdi;    // rdi <- :2
        l1:
            movq r8, (self.dispatch.as_ptr());
            movzxb rax, rax;
            movq rax, [r8 + rax * 8];
            jmp rax;
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
            lea rdi, [rbp + rdi * 8 - 16];
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
            lea rcx, [rbp + rcx * 8 - 16];
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
            movq rdi, [rbp + rdi * 8 - 16];
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
            movq rsi, [rbp + rsi * 8 - 16];
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
            movq r15, [rbp + r15 * 8 - 16];
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
            lea r15, [rbp + r15 * 8 - 16];
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

    fn vm_generic_unop(&mut self, generic: DestLabel, func: u64) {
        self.jit.bind_label(generic);
        self.vm_save_lhs_class();
        self.call_unop(func);
        self.vm_store_r15();
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: DestLabel, exit: DestLabel, func: u64) {
        self.jit.bind_label(generic);
        self.vm_save_binary_class();
        self.call_binop(func);
        self.vm_store_r15();
        monoasm! { self.jit,
            jmp exit;
        };
    }

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

    fn vm_method_call(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let exit = self.jit.label();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let slowpath = self.jit.label();
        let exec = self.jit.label();
        let vm_return = self.vm_return;
        let class_version = self.class_version;
        //
        //      +------+------+------+------+
        //      | MethodCall  |class | ver  |
        //      +------+------+------+------+
        // r13->| MethodArgs  |   CodePtr   |
        //      +------+------+------+------+
        //      |     Meta    |     PC      |
        //      +------+------+------+------+
        //
        // rdi: IdentId
        // r15: %ret
        // [r13 -  8]: class_id
        // [r13 -  4]: class_version
        // [r13 +  0]; len
        // [r13 +  2]; %args
        // [r13 +  4]: %recv
        // [r13 +  8]: CodePtr
        // [r13 + 16]: Meta
        // [r13 + 24]: PC

        monoasm! { self.jit,
            pushq r15;
            pushq r13; // push pc
            pushq rdi; // push IdentId
            movzxw rdi, [r13 + 4];
        };
        self.vm_get_rdi();
        monoasm! { self.jit,
            pushq rdi;
            // rsp + 24:[%ret]
            // rsp + 16:[pc]
            // rsp + 08:[method_name:IdentId]
            // rsp + 00:[recv:Value]

            // rdi: receiver: Value
            movq rax, (Value::get_class);
            call rax;
            movl r15, rax;
            cmpl r15, [r13 - 8];
            jne  slowpath;
            movl rdi, [r13 - 4];
            cmpl rdi, [rip + class_version];
            jne  slowpath;

        exec:
            // set meta
            movq rdi, [r13 + 16];
            movq [rsp - 0x18], rdi;
            movzxw rcx, [r13 + 2]; // rcx <- args
            movzxw rdi, [r13 + 0];  // rdi <- len
            // set self (= receiver)
            movq rax, [rsp];
            movq [rsp - 0x20], rax;
        };
        self.vm_get_addr_rcx(); // rcx <- *args

        monoasm! { self.jit,
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
            // -0x20 |     %0      |
            //       +-------------+
            // -0x28 | %1(1st arg) | <- rdx
            //       +-------------+
            //       |             |
            //
            movq r8, rdi;
            testq r8, r8;
            jeq  loop_exit;
            negq r8;
        loop_:
            movq rax, [rcx + r8 * 8 + 8];
            movq [rsp + r8 * 8 - 0x20], rax;
            addq r8, 1;
            jne  loop_;
        loop_exit:

            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rax, [r13 + 8];
            // set pc
            movq r13, [r13 + 24];    // r13: BcPc
            call rax;
            addq rsp, 16;
            popq r13;   // pop pc
            popq r15;   // pop %ret
            addq r13, 32;
            testq rax, rax;
            jeq vm_return;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();

        self.jit.select(1);
        let entry_find_method = self.entry_find_method;
        monoasm!(self.jit,
        slowpath:
            movq rdx, [rsp + 8];  // rdx: IdentId
            movzxw rcx, [r13];  // rcx: len
            movq r8, [rsp]; // r8: receiver:Value
            call entry_find_method; // rax <- Option<&FuncData>
            testq rax, rax;
            jeq vm_return;
            movl [r13 - 8], r15;
            movl rdi, [rip + class_version];
            movl [r13 - 4], rdi;
            movq rdi, [rax + (FUNCDATA_OFFSET_CODEPTR)];
            movq [r13 + 8], rdi;
            movq rdi, [rax + (FUNCDATA_OFFSET_META)];
            movq [r13 + 16], rdi;
            movq rdi, [rax + (FUNCDATA_OFFSET_PC)];
            movq [r13 + 24], rdi;
            jmp exec;
        );
        self.jit.select(0);

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
                movq rdi, rbx;
                movq rsi, r12;
                movl rdx, [rbp - 8];
                lea rcx, [r13 - 16];
                movq rax, (Self::exec_jit_partial_compile);
                call rax;
                movq [r13 - 8], rax;
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
            movq rax, [r13 - 8];
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

    fn vm_index(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let entry_return = self.vm_return;
        self.vm_get_addr_r15();
        self.vm_get_rdi();
        self.vm_get_rsi();
        monoasm! { self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (get_index);
            call rax;
            testq rax, rax;
            jeq  entry_return;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_index_assign(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let entry_return = self.vm_return;
        self.vm_get_r15();
        self.vm_get_rdi();
        self.vm_get_rsi();
        monoasm! { self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq r8, r15;  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (set_index);
            call rax;
            testq rax, rax;
            jeq  entry_return;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_load_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let entry_return = self.vm_return;
        let const_version = self.const_version;
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdx, rdi;  // name: ConstSiteId
            movq rcx, [rip + const_version]; // usize
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (vm_get_constant);
            call rax;
            testq rax, rax;
            jeq  entry_return;
            movq [r13 - 8], rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    fn vm_store_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let const_version = self.const_version;
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdx, rdi;  // name: IdentId
            movq rcx, [r15];  // val: Value
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            addq [rip + const_version], 1;
            movq rax, (set_constant);
            call rax;
        };
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
        generic_func: u64,
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

    fn vm_binops(&mut self, func: u64) -> (CodePtr, CodePtr, CodePtr) {
        let common = self.jit.label();
        let ptr_rr = self.jit.get_current_address();
        self.vm_get_rr_r15(); // rdi <- lhs, rsi <- rhs, r15 <- ret addr
        self.jit.bind_label(common);
        self.vm_save_binary_class();
        self.call_binop(func);
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
            movq rax, (vm_define_method);
            call rax;
            addl [rip + class_version], 1;
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
