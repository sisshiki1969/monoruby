use super::*;
use monoasm_macro::monoasm;
use paste::paste;

macro_rules! cmp_ops {
  ($op:ident) => {
      paste! {
          fn [<vm_ $op rr>](&mut self) -> CodePtr {
              let label = self.jit.get_current_address();
              let generic = self.jit.label();
              self.vm_get_rdi(); // rdi <- lhs addr
              self.vm_get_rsi(); // rsi <- rhs addr
              self.vm_get_addr_r15(); // r15 <- ret addr
              self.guard_rdi_fixnum(generic);
              self.guard_rsi_fixnum(generic);
              self.[<cmp_ $op>](generic);
              monoasm! { self.jit,
                movq [r15], rax;
              };
              self.fetch_and_dispatch();
              label
          }

          fn [<vm_ $op ri>](&mut self) -> CodePtr {
            let label = self.jit.get_current_address();
            let generic = self.jit.label();
            self.vm_get_rdi();      // rdi <- lhs addr
            self.vm_get_addr_r15(); // r15 <- ret addr
            monoasm! { self.jit,
              shlq rsi, 1;
              orq  rsi, 1;
            };
            self.guard_rdi_fixnum(generic);
            self.[<cmp_ $op>](generic);
            monoasm! { self.jit,
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

extern "C" fn find_method<'a>(
    interp: &mut Interp,
    globals: &'a mut Globals,
    callsite_id: CallsiteId,
    len: usize,
    receiver: Value,
) -> Option<&'a FuncData> {
    let class_version = interp.codegen.class_version();
    let CallsiteInfo {
        name,
        cache: (version, cached_class_id, _),
        ..
    } = globals.func[callsite_id];
    let recv_class = receiver.class_id();
    if version != class_version || cached_class_id != recv_class {
        let id = globals.get_method(recv_class, name, len)?;
        let data = interp.get_func_data(globals, id).clone();
        globals.func[callsite_id].cache = (class_version, recv_class, data);
    }
    Some(&globals.func[callsite_id].cache.2)
}

extern "C" fn get_literal(_interp: &mut Interp, globals: &mut Globals, literal_id: u32) -> Value {
    Value::dup(globals.func.get_literal(literal_id))
}

extern "C" fn vm_define_method(_interp: &mut Interp, globals: &mut Globals, def_id: MethodDefId) {
    let MethodDefInfo { name, func } = globals.func[def_id];
    globals.class.add_method(OBJECT_CLASS, name, func);
}

/*extern "C" fn eprintln(data: u64) {
    eprintln!("{:016x}", data);
}*/

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
    pub(super) fn construct_vm(&mut self) {
        self.vm_entry = self.jit.get_current_address();
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
            pushq rbp;
            movq rbp, rsp;
            movw [rbp - 0x02], 0;
            movzxw rax, [rbp - 0x04];
        };
        self.calc_offset();
        monoasm! { self.jit,
            subq rsp, rax;
        };
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

        self.dispatch[129] = self.vm_neg();
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

        self.dispatch[170] = self.vm_addrr();
        self.dispatch[171] = self.vm_subrr();
        self.dispatch[172] = self.vm_mulrr();
        self.dispatch[173] = self.vm_divrr();
        self.dispatch[174] = self.vm_bitorrr();
        self.dispatch[175] = self.vm_bitandrr();
        self.dispatch[176] = self.vm_bitxorrr();
        self.dispatch[177] = shr;
        self.dispatch[178] = shl;
    }

    ///
    /// Fetch instruction and decode
    ///
    /// requirement:
    /// r13: BcPc
    ///
    /// returns:
    /// eax:  :0
    /// r15d: :1
    /// edi: :2 or rdi: :2:3
    /// esi: :3
    ///
    /// use: r8, r9
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

    /// Get absolute address of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: absolute address of the register
    fn vm_get_addr_rdi(&mut self) {
        monoasm! { self.jit,
            negq rdi;
            lea rdi, [rbp + rdi * 8 - 16];
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *rcx*: register number
    /// #### @return
    /// - *rcx*: absolute address of the register
    fn vm_get_addr_rcx(&mut self) {
        monoasm! { self.jit,
            negq rcx;
            lea rcx, [rbp + rcx * 8 - 16];
        };
    }

    /// Get value of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: value of the register
    fn vm_get_rdi(&mut self) {
        monoasm! { self.jit,
            negq rdi;
            movq rdi, [rbp + rdi * 8 - 16];
        };
    }

    /// Get value of the register.
    /// #### @args
    /// - *rsi*: register number
    /// #### @return
    /// - *rsi*: value of the register
    fn vm_get_rsi(&mut self) {
        monoasm! { self.jit,
            negq rsi;
            movq rsi, [rbp + rsi * 8 - 16];
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *r15*: register number
    /// #### @return
    /// - *r15*: absolute address of the register
    fn vm_get_addr_r15(&mut self) {
        monoasm! { self.jit,
            negq r15;
            lea r15, [rbp + r15 * 8 - 16];
        };
    }

    fn vm_store_r15_if_nonzero(&mut self, exit: DestLabel) {
        monoasm! { self.jit,
            testq r15, r15;
            jeq exit;
        };
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq [r15], rax;
        exit:
        };
    }

    fn vm_generic_unop(&mut self, generic: DestLabel, func: u64) {
        self.jit.bind_label(generic);
        self.call_unop(func, self.vm_return);
        monoasm! { self.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
    }

    fn vm_generic_binop(&mut self, generic: DestLabel, func: u64) {
        self.jit.bind_label(generic);
        self.call_binop(func, self.vm_return);
        monoasm! { self.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
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
        let vm_return = self.vm_return;
        // rdi: CallsiteId
        // r15: receiver reg
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            pushq r13; // push pc
            movzxw rax, [r13 + 4];
            pushq rax; // push ret

            movq rdx, rdi;  // rdx: CallsiteId
            movq rdi, rbx;  // rdi: &mut Interp
            movq rsi, r12;  // rsi: &mut Globals
            movzxw rcx, [r13 + 0];  // rcx: len
            movq r8, [r15]; // r8: receiver:Value
            movq rax, (find_method);
            call rax;       // rax <- Option<&FuncData>
            testq rax, rax;
            jeq vm_return;

            movq rsi, rax; // rsi <- *const FuncData
            // set meta
            movq rdi, [rsi + (FUNCDATA_OFFSET_META)];
            movq [rsp - 0x18], rdi;
            movzxw rcx, [r13 + 2]; // rcx <- args
            movzxw rdi, [r13 + 0];  // rdi <- len
            // set self (= receiver)
            movq rax, [r15];
            movq [rsp - 0x20], rax;
            // set pc
            movq r13, [rsi + (FUNCDATA_OFFSET_PC)];    // r13: BcPc
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
            movq [rsp + r8 * 8- 0x20], rax;
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
            movq rax, [rsi + (FUNCDATA_OFFSET_CODEPTR)];
            call rax;
            popq r15;   // pop ret
            popq r13;   // pop pc
            addq r13, 16;
            testq rax, rax;
            jeq vm_return;
        };
        self.vm_store_r15_if_nonzero(exit);
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
            movq [r15], rax;
        };
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
            lea  r8, [rip + const_version]; // &mut usize
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
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

    fn vm_addri(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit,
            shlq rsi, 1;
            testq rdi, 0x1;
            jeq generic;
            movq rax, rdi;
            addq rax, rsi;
            jo generic;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit,
        generic:
            // generic path
            addq rsi, 1;
        };
        self.call_binop(add_values as _, self.vm_return);
        monoasm! { self.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit,
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
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit,
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
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit,
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
        let label = self.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.call_binop(mul_values as _, self.vm_return);
        monoasm! { self.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_divrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.call_binop(div_values as _, self.vm_return);
        monoasm! { self.jit,
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_bitorrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit,
            orq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitor_values as _);
        label
    }

    fn vm_bitandrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit,
            andq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitand_values as _);
        label
    }

    fn vm_bitxorrr(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let generic = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.guard_rdi_rsi_fixnum(generic);
        monoasm! { self.jit,
            xorq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        self.vm_generic_binop(generic, bitxor_values as _);
        label
    }

    fn vm_shift(&mut self) -> (CodePtr, CodePtr) {
        let shl_label = self.jit.get_current_address();
        let generic_shl = self.jit.label();
        let generic_shr = self.jit.label();
        let to_shl = self.jit.label();
        let to_shr = self.jit.label();
        let shl = self.jit.label();
        let shr = self.jit.label();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.vm_generic_binop(generic_shl, shl_values as _);
        monoasm! { self.jit,
        to_shr:
            negq rcx;
            jmp shr;
        };

        let shr_label = self.jit.get_current_address();
        self.vm_get_rdi(); // rdi <- lhs
        self.vm_get_rsi(); // rsi <- rhs
        self.vm_get_addr_r15(); // r15 <- ret addr
        self.vm_generic_binop(generic_shr, shr_values as _);
        monoasm! { self.jit,
        to_shl:
            negq rcx;
            jmp shl;
        };
        (shl_label, shr_label)
    }

    cmp_ops!(eq, ne, gt, ge, lt, le);

    fn vm_method_def(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let class_version = self.class_version;
        monoasm! { self.jit,
            movq rdx, rdi;  // method_def_id
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
