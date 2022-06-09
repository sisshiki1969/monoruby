use super::*;

macro_rules! cmp_ops {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op rr>](&mut self) -> CodePtr {
              let label = self.jit_gen.jit.get_current_address();
              self.vm_get_rdi(); // rdi <- lhs addr
              self.vm_get_rsi(); // rsi <- rhs addr
              self.vm_get_addr_r15(); // r15 <- ret addr
              monoasm! { self.jit_gen.jit,
                  movq rax, ([<cmp_ $op _values>]);
                  call rax;
                  // store the result to return reg.
                  movq [r15], rax;
              };
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

macro_rules! cmp_ri_ops {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op ri>](&mut self) -> CodePtr {
              let label = self.jit_gen.jit.get_current_address();
              self.vm_get_rdi(); // rdi <- lhs addr
              self.vm_get_addr_r15(); // r15 <- ret addr
              monoasm! { self.jit_gen.jit,
                  movq rax, ([<cmp_ $op _ri_values>]);
                  call rax;
                  // store the result to return reg.
                  movq [r15], rax;
              };
              self.fetch_and_dispatch();
              label
          }
      }
  };
  ($op1:ident, $($op2:ident),+) => {
      cmp_ri_ops!($op1);
      cmp_ri_ops!($($op2),+);
  };
}

extern "C" fn find_method(
    interp: &mut Interp,
    globals: &mut Globals,
    callsite_id: CallsiteId,
    data: &mut FuncData,
    receiver: Value,
) -> Option<EncodedCallInfo> {
    match interp.find_method(globals, callsite_id, receiver) {
        Some((func_id, args, len, ret)) => {
            get_func_data(interp, globals, func_id, data);
            data.ret = ret as usize;
            let info = EncodedCallInfo::new(func_id, args, len);
            Some(info)
        }
        // If error occurs, return 0.
        None => None,
    }
}

extern "C" fn get_func_data(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    data: &mut FuncData,
) {
    let label = globals.func[func_id].jit_label().unwrap();
    data.address = label.as_ptr();
    data.offset = globals.func[func_id].stack_offset();
    data.pc = globals.func[func_id].inst_pc();
}

extern "C" fn get_literal(_interp: &mut Interp, globals: &mut Globals, literal_id: u32) -> Value {
    Value::dup(globals.func.get_literal(literal_id))
}

extern "C" fn define_method(interp: &mut Interp, globals: &mut Globals, def_id: MethodDefId) {
    let MethodDefInfo { name, func } = globals.func[def_id];
    globals.class.add_method(0, name, func);
    interp.class_version += 1;
}

/*extern "C" fn eprintln(data: u64) {
    eprintln!("{:016x}", data);
}*/

impl Interp {
    pub(super) fn construct_vm(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let entry = self.jit_gen.jit.get_current_address();
        let func_offset = self.jit_gen.jit.const_i64(0);
        let func_address = self.jit_gen.jit.const_i64(0);
        let func_pc = self.jit_gen.jit.const_i64(0);
        let func_ret = self.jit_gen.jit.const_i64(0);

        monoasm! { self.jit_gen.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;

            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
            movl r13, rdx;  // rdx: FuncId
            lea rcx, [rip + func_offset];
            movq rax, (get_func_data);
            call rax;
            movl [rsp - 0x14], r13;
            movq r13, [rip + func_pc];    // r13: BcPc
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
            movq [rsp - 0x20], (NIL_VALUE);
            movq rax, [rip + func_address];
            call rax;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
            //
            // VM entry
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            //   stack_offset: [rip + func_offset]
            //
        vm_entry:
            pushq rbp;
            movq rbp, rsp;
            subq rsp, [rip + func_offset];
        };
        self.fetch_and_dispatch();

        //BcOp::Ret
        let ret = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Mov
        let mov = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        self.vm_get_rdi();
        monoasm! { self.jit_gen.jit,
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();

        let br_inst = self.jit_gen.jit.get_current_address();
        let branch = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
        branch:
            lea r13, [r13 + rdi * 8];
        };
        self.fetch_and_dispatch();

        let (shl, shr) = self.vm_shift();

        self.dispatch[1] = self.vm_method_call(func_offset, func_address, func_pc, func_ret);
        self.dispatch[2] = self.vm_method_def();
        self.dispatch[3] = br_inst;
        self.dispatch[4] = self.vm_condbr(branch);
        self.dispatch[5] = self.vm_condnotbr(branch);
        self.dispatch[6] = self.vm_integer();
        self.dispatch[7] = self.vm_literal();
        self.dispatch[8] = self.vm_nil();

        self.dispatch[129] = self.vm_neg();
        self.dispatch[130] = self.vm_addrr();
        self.dispatch[131] = self.vm_subrr();
        self.dispatch[132] = self.vm_mulrr();
        self.dispatch[133] = self.vm_divrr();
        self.dispatch[134] = self.vm_eqrr();
        self.dispatch[135] = self.vm_nerr();
        self.dispatch[136] = self.vm_ltrr();
        self.dispatch[137] = self.vm_lerr();
        self.dispatch[138] = self.vm_gtrr();
        self.dispatch[139] = self.vm_gerr();

        self.dispatch[140] = self.vm_addri();
        self.dispatch[141] = self.vm_subri();
        self.dispatch[142] = self.vm_eqri();
        self.dispatch[143] = self.vm_neri();
        self.dispatch[144] = self.vm_ltri();
        self.dispatch[145] = self.vm_leri();
        self.dispatch[146] = self.vm_gtri();
        self.dispatch[147] = self.vm_geri();

        self.dispatch[148] = ret;
        self.dispatch[149] = mov;

        self.dispatch[150] = self.vm_bitorrr();
        self.dispatch[151] = self.vm_bitandrr();
        self.dispatch[152] = self.vm_bitxorrr();
        self.dispatch[153] = shr;
        self.dispatch[154] = shl;
        self.dispatch[155] = self.vm_concat();

        self.jit_gen.jit.finalize();
        entry
    }

    /// Fetch instruction and decode
    ///
    /// requirement:
    /// r13: BcPc
    ///
    /// returns:
    /// rax: :0
    /// r15: :1
    /// rdi: :2 or :2:3
    /// rsi: :3
    ///
    /// use: r8, r9
    fn fetch_and_dispatch(&mut self) {
        let l1 = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
            movq rax, [r13]; // rax <- :0:1:2:3
            addq r13, 8;
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

    /// Get absolute address of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: absolute address of the register
    fn vm_get_addr_rdi(&mut self) {
        monoasm! { self.jit_gen.jit,
            negq rdi;
            lea rdi, [rbp + rdi * 8 - 16];
        };
    }

    /// Get value of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: value of the register
    fn vm_get_rdi(&mut self) {
        monoasm! { self.jit_gen.jit,
            negq rdi;
            lea rdi, [rbp + rdi * 8 - 16];
            movq rdi, [rdi];
        };
    }

    /// Get value of the register.
    /// #### @args
    /// - *rsi*: register number
    /// #### @return
    /// - *rsi*: value of the register
    fn vm_get_rsi(&mut self) {
        monoasm! { self.jit_gen.jit,
            //addq rdi, 2;
            negq rsi;
            lea rsi, [rbp + rsi * 8 - 16];
            movq rsi, [rsi];
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *r15*: register number
    /// #### @return
    /// - *r15*: absolute address of the register
    fn vm_get_addr_r15(&mut self) {
        monoasm! { self.jit_gen.jit,
            negq r15;
            lea r15, [rbp + r15 * 8 - 16];
        };
    }

    fn vm_store_r15_if_nonzero(&mut self, exit: DestLabel) {
        monoasm! { self.jit_gen.jit,
            testq r15, r15;
            jeq exit;
        };
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], rax;
        exit:
        };
    }

    fn vm_generic_unop(&mut self, generic: DestLabel, func: u64) {
        self.jit_gen.jit.bind_label(generic);
        self.jit_gen.call_unop(func, self.jit_gen.vm_return);
        monoasm! { self.jit_gen.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: DestLabel, func: u64) {
        self.jit_gen.jit.bind_label(generic);
        self.jit_gen.call_binop(func, self.jit_gen.vm_return);
        monoasm! { self.jit_gen.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
    }

    fn vm_concat(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let exit = self.jit_gen.jit.label();
        self.vm_get_addr_rdi();
        monoasm! { self.jit_gen.jit,
            movq rax, (concatenate_string);
            call rax;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
        label
    }

    fn vm_method_call(
        &mut self,
        func_offset: DestLabel,
        func_address: DestLabel,
        func_pc: DestLabel,
        ret: DestLabel,
    ) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let exit = self.jit_gen.jit.label();
        let loop_ = self.jit_gen.jit.label();
        let loop_exit = self.jit_gen.jit.label();
        let vm_return = self.jit_gen.vm_return;
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // rdx: CallsiteId
            movq rdi, rbx;  // rdi: &mut Interp
            movq rsi, r12;  // rsi: &mut Globals
            lea rcx, [rip + func_offset]; // rcx: &mut FuncData
            movq r8, [r15]; // r8: receiver:Value
            movq rax, (find_method);
            call rax;       // rax <- EncodedCallInfo
            testq rax, rax;
            jeq vm_return;

            pushq r13;
            pushq [rip + ret];
            movq r13, [rip + func_pc];    // r13: BcPc
            movl [rsp - 0x14], rax;
            shrq rax, 32;
            movl rdi, rax;
            shrq rdi, 16;   // rdi <- args
            movzxw r14, rax;    // r14 <- len
            shlq r14, 3;
            lea rdx, [rsp - 0x28];
            movq rax, [r15];
            movq [rsp - 0x20], rax; // set self
        };
        self.vm_get_addr_rdi(); // rdi <- *args

        monoasm! { self.jit_gen.jit,
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
            // -0x28 | %1(1st arg) |
            //       +-------------+
            //       |             |
            //
            movq rsi, rdi;
            subq rsi, r14;
        loop_:
            cmpq rdi, rsi;
            jeq loop_exit;
            movq rax, [rdi];
            movq [rdx], rax;
            subq rdi, 8;
            subq rdx, 8;
            jmp loop_;
        loop_exit:

            movq rax, [rip + func_address];
            call rax;
            popq r15;
            popq r13;
            testq rax, rax;
            jeq vm_return;
        };
        self.vm_store_r15_if_nonzero(exit);
        self.fetch_and_dispatch();
        label
    }

    fn vm_nil(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], (NIL_VALUE);
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_integer(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_literal(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // literal_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (get_literal);
            call rax;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_neg(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            sarq rdi, 1;
            negq rdi;
            lea rdi, [rdi + rdi + 1];
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_unop(generic, neg_value as _);
        label
    }

    fn vm_addri(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            testq rdi, 0x1;
            jeq generic;
            movq rax, rdi;
            addq rax, rsi;
            jo generic;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            addq rsi, 1;
        };
        self.jit_gen
            .call_binop(add_values as _, self.jit_gen.vm_return);
        monoasm! { self.jit_gen.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, add_values as _);
        label
    }

    fn vm_subri(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            addq rsi, 1;
            testq rdi, 0x1;
            jeq generic;
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, sub_values as _);
        label
    }

    fn vm_subrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, sub_values as _);
        label
    }

    fn vm_mulrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen
            .call_binop(mul_values as _, self.jit_gen.vm_return);
        monoasm! { self.jit_gen.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_divrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen
            .call_binop(div_values as _, self.jit_gen.vm_return);
        monoasm! { self.jit_gen.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_bitorrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            orq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitor_values as _);
        label
    }

    fn vm_bitandrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            andq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitand_values as _);
        label
    }

    fn vm_bitxorrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.jit_gen.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit_gen.jit,
            xorq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitxor_values as _);
        label
    }

    fn vm_shift(&mut self) -> (CodePtr, CodePtr) {
        let shl_label = self.jit_gen.jit.get_current_address();
        let generic_shl = self.jit_gen.jit.label();
        let generic_shr = self.jit_gen.jit.label();
        let to_shl = self.jit_gen.jit.label();
        let to_shr = self.jit_gen.jit.label();
        let shl = self.jit_gen.jit.label();
        let shr = self.jit_gen.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.vm_generic_binop(generic_shl, shl_values as _);
        monoasm! { self.jit_gen.jit,
        to_shr:
            negq rcx;
            jmp shr;
        };

        let shr_label = self.jit_gen.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.vm_generic_binop(generic_shr, shr_values as _);
        monoasm! { self.jit_gen.jit,
        to_shl:
            negq rcx;
            jmp shl;
        };
        (shl_label, shr_label)
    }

    cmp_ops!(eq, ne, gt, ge, lt, le);
    cmp_ri_ops!(eq, ne, gt, ge, lt, le);

    fn vm_method_def(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // method_def_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_method);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jne branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condnotbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jeq branch;
        };
        self.fetch_and_dispatch();
        label
    }
}
