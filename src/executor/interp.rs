use super::compiler::JitGen;
use super::*;
use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BcPcBase(*const u64);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs as isize) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    fn new(func: &NormalFuncInfo) -> Self {
        BcPcBase(&func.bytecode()[0] as *const _)
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BcPc(*const u64);

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc(self.0.offset(offset as isize));
        }
    }
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: FuncId,
    pc: BcPc,
    pc_top: BcPcBase,
    pub jit_gen: JitGen,
    pub error: Option<MonorubyErr>,
    dispatch: Vec<u64>,
    class_version: usize,
}

/*fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}*/

extern "C" fn get_func_data(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    data: &mut usize,
) -> BcPc {
    let main = globals.func[func_id].as_normal();
    let regs = main.total_reg_num();
    *data = ((regs + 2) & (-2i64 as usize)) * 8;
    BcPcBase::new(main) + 0
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct EncodedCallInfo(u64);

impl EncodedCallInfo {
    fn new(func_id: FuncId, args: u16, len: u16) -> Self {
        Self((func_id.0 as u64) + ((args as u64) << 48) + ((len as u64) << 32))
    }

    fn none() -> Self {
        Self(-1i64 as u64)
    }
}

extern "C" fn find_method(
    interp: &mut Interp,
    globals: &mut Globals,
    callsite_id: CallsiteId,
) -> EncodedCallInfo {
    let CallsiteInfo {
        name,
        args,
        len,
        cache: (version, cached_func),
    } = globals.func[callsite_id];
    //eprintln!("{}", globals.id_store.get_name(name));
    let func_id = if version == interp.class_version {
        cached_func
    } else {
        match globals.get_method(name) {
            Some(func_id) => {
                globals.func[callsite_id].cache = (interp.class_version, func_id);
                func_id
            }
            None => {
                interp.error = Some(MonorubyErr::MethodNotFound(name));
                return EncodedCallInfo::none();
            }
        }
    };
    let info = &globals.func[func_id];
    if info.arity() != len as usize {
        interp.error = Some(MonorubyErr::WrongArguments(format!(
            "number of arguments mismatch. expected:{} actual:{}",
            info.arity(),
            len
        )));
        EncodedCallInfo::none()
    } else {
        EncodedCallInfo::new(func_id, args, len)
    }
}

extern "C" fn get_constant(_interp: &mut Interp, globals: &mut Globals, const_id: u32) -> Value {
    globals.func.get_literal(const_id)
}

extern "C" fn define_method(interp: &mut Interp, globals: &mut Globals, def_id: MethodDefId) {
    let MethodDefInfo { name, func } = globals.func[def_id];
    globals.func.insert(name, func);
    interp.class_version += 1;
}

extern "C" fn eprintln(data: u64) {
    eprintln!("{:016x}", data);
}

macro_rules! cmp_ops {
    ($op:ident) => {
        paste! {
            fn [<vm_ $op rr>](&mut self) -> DestLabel {
                let label = self.jit_gen.jit.label();
                self.jit_gen.jit.bind_label(label);
                self.vm_get_addr_rdi(); // rdi <- lhs addr
                self.vm_get_addr_rsi(); // rsi <- rhs addr
                self.vm_get_addr_r15(); // r15 <- ret addr
                monoasm! { self.jit_gen.jit,
                    movq rdi, [rdi];
                    movq rsi, [rsi];
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
            fn [<vm_ $op ri>](&mut self) -> DestLabel {
                let label = self.jit_gen.jit.label();
                self.jit_gen.jit.bind_label(label);
                self.vm_get_addr_rdi(); // rdi <- lhs addr
                self.vm_get_addr_r15(); // r15 <- ret addr
                monoasm! { self.jit_gen.jit,
                    movq rdi, [rdi];
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

impl Interp {
    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let main_id = globals.get_main_func();
        let main = globals.func[main_id].as_normal();
        let mut eval = Self::new(main);
        let entry = eval.construct_vm();

        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(eval.jit_gen.jit.get_label_absolute_address(entry)) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
    }

    fn construct_vm(&mut self) -> DestLabel {
        let entry = self.jit_gen.jit.label();
        let fn_entry = self.jit_gen.jit.label();
        let func_offset = self.jit_gen.jit.const_i64(0);

        monoasm! {
            self.jit_gen.jit,
            entry:
                pushq rbx;
                pushq r12;
                pushq r13;
                pushq r14;
                pushq r15;

                movq rbx, rdi;  // rdi: &mut Interp
                movq r12, rsi;  // rsi: &mut Globals
                                // rdx: FuncId
                lea rcx, [rip + func_offset];
                movq rax, (get_func_data);
                call rax;
                movq r13, rax;    // r13: BcPc
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
                call fn_entry;
                popq r15;
                popq r14;
                popq r13;
                popq r12;
                popq rbx;
                ret;
            fn_entry:
                pushq rbp;
                movq rbp, rsp;
                subq rsp, [rip + func_offset];
        };
        self.fetch_and_dispatch();

        //BcOp::FnCall
        let fncall = self.vm_fncall(fn_entry, func_offset);
        //BcOp::MethodDef
        let method_def = self.vm_method_def();
        //BcOp::Br
        let br = self.vm_br();
        //BcOp::CondBr
        let condbr = self.vm_condbr();
        //BcOp::CondNotBr
        let condnotbr = self.vm_condnotbr();
        //BcOp::Nil
        let nil = self.vm_nil();
        //BcOp::Integer
        let integer = self.vm_integer();
        //BcOp::Const
        let constant = self.vm_constant();

        //BcOp::Ret
        let ret = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(ret);
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Mov
        let mov = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(mov);
        self.vm_get_addr_r15();
        self.vm_get_addr_rdi();
        monoasm! { self.jit_gen.jit,
            movq rax, [rdi];
            movq [r15], rax;
        };
        self.fetch_and_dispatch();

        //BcOp::Neg
        let neg = self.vm_neg();

        //BcOp::Add
        let addrr = self.vm_addrr();
        //BcOp::Sub
        let subrr = self.vm_subrr();
        //BcOp::Mul
        let mulrr = self.vm_mulrr();
        //BcOp::Div
        let divrr = self.vm_divrr();

        //BcOp::Eq
        let eqrr = self.vm_eqrr();
        //BcOp::Ne
        let nerr = self.vm_nerr();
        //BcOp::Lt
        let ltrr = self.vm_ltrr();
        //BcOp::Le
        let lerr = self.vm_lerr();
        //BcOp::Gt
        let gtrr = self.vm_gtrr();
        //BcOp::Ge
        let gerr = self.vm_gerr();

        //BcOp::Addri
        let addri = self.vm_addri();
        //BcOp::Subri
        let subri = self.vm_subri();

        //BcOp::Eqri
        let eqri = self.vm_eqri();
        //BcOp::Neri
        let neri = self.vm_neri();
        //BcOp::Ltri
        let ltri = self.vm_ltri();
        //BcOp::Leri
        let leri = self.vm_leri();
        //BcOp::Gtri
        let gtri = self.vm_gtri();
        //BcOp::Geri
        let geri = self.vm_geri();

        self.jit_gen.jit.finalize();

        self.dispatch[1] = self.jit_gen.jit.get_label_absolute_address(fncall) as _;
        self.dispatch[2] = self.jit_gen.jit.get_label_absolute_address(method_def) as _;
        self.dispatch[3] = self.jit_gen.jit.get_label_absolute_address(br) as _;
        self.dispatch[4] = self.jit_gen.jit.get_label_absolute_address(condbr) as _;
        self.dispatch[5] = self.jit_gen.jit.get_label_absolute_address(condnotbr) as _;
        self.dispatch[6] = self.jit_gen.jit.get_label_absolute_address(integer) as _;
        self.dispatch[7] = self.jit_gen.jit.get_label_absolute_address(constant) as _;
        self.dispatch[8] = self.jit_gen.jit.get_label_absolute_address(nil) as _;

        self.dispatch[129] = self.jit_gen.jit.get_label_absolute_address(neg) as _;
        self.dispatch[130] = self.jit_gen.jit.get_label_absolute_address(addrr) as _;
        self.dispatch[131] = self.jit_gen.jit.get_label_absolute_address(subrr) as _;
        self.dispatch[132] = self.jit_gen.jit.get_label_absolute_address(mulrr) as _;
        self.dispatch[133] = self.jit_gen.jit.get_label_absolute_address(divrr) as _;
        self.dispatch[134] = self.jit_gen.jit.get_label_absolute_address(eqrr) as _;
        self.dispatch[135] = self.jit_gen.jit.get_label_absolute_address(nerr) as _;
        self.dispatch[136] = self.jit_gen.jit.get_label_absolute_address(ltrr) as _;
        self.dispatch[137] = self.jit_gen.jit.get_label_absolute_address(lerr) as _;
        self.dispatch[138] = self.jit_gen.jit.get_label_absolute_address(gtrr) as _;
        self.dispatch[139] = self.jit_gen.jit.get_label_absolute_address(gerr) as _;

        self.dispatch[140] = self.jit_gen.jit.get_label_absolute_address(addri) as _;
        self.dispatch[141] = self.jit_gen.jit.get_label_absolute_address(subri) as _;
        self.dispatch[142] = self.jit_gen.jit.get_label_absolute_address(eqri) as _;
        self.dispatch[143] = self.jit_gen.jit.get_label_absolute_address(neri) as _;
        self.dispatch[144] = self.jit_gen.jit.get_label_absolute_address(ltri) as _;
        self.dispatch[145] = self.jit_gen.jit.get_label_absolute_address(leri) as _;
        self.dispatch[146] = self.jit_gen.jit.get_label_absolute_address(gtri) as _;
        self.dispatch[147] = self.jit_gen.jit.get_label_absolute_address(geri) as _;

        self.dispatch[148] = self.jit_gen.jit.get_label_absolute_address(ret) as _;
        self.dispatch[149] = self.jit_gen.jit.get_label_absolute_address(mov) as _;

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
            movzxb r9, rax;
            shlq r9, 3;
            addq r9, r8;
            movq rax, [r9];
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
            shlq rdi, 3;
            addq rdi, 16;
            negq rdi;
            addq rdi, rbp;
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *rsi*: register number
    /// #### @return
    /// - *rsi*: absolute address of the register
    fn vm_get_addr_rsi(&mut self) {
        monoasm! { self.jit_gen.jit,
            shlq rsi, 3;
            addq rsi, 16;
            negq rsi;
            addq rsi, rbp;
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *r15*: register number
    /// #### @return
    /// - *r15*: absolute address of the register
    fn vm_get_addr_r15(&mut self) {
        monoasm! { self.jit_gen.jit,
            shlq r15, 3;
            addq r15, 16;
            negq r15;
            addq r15, rbp;
        };
    }

    fn vm_fncall(&mut self, fn_entry: DestLabel, func_offset: DestLabel) -> DestLabel {
        let label = self.jit_gen.jit.label();
        let exit = self.jit_gen.jit.label();
        let loop_ = self.jit_gen.jit.label();
        let loop_exit = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // rdx: CallsiteId
            movq rdi, rbx;  // rdi: &mut Interp
            movq rsi, r12;  // rsi: &mut Globals
            movq rax, (find_method);
            call rax;       // rax <- EncodedCallInfo

            movl rdx, rax;  // rdx: FuncId
            shrq rax, 32;
            movl r14, rax;  // r14: args:len.
            movq rdi, rbx;  // rdi: &mut Interp
            movq rsi, r12;  // rsi: &mut Globals
            lea rcx, [rip + func_offset]; // rcx: &mut usize
            movq rax, (get_func_data);
            call rax;

            pushq r13;
            pushq r15;
            movq r13, rax;    // r13: BcPc
            movl rdi, r14;
            shrq rdi, 16;   // rdi <- args
            movzxw r14, r14;    // r14 <- len
            shlq r14, 3;
            lea rdx, [rsp - 0x28];
        };
        self.vm_get_addr_rdi();
        /*for i in 0..len {
            let reg = args + i;
            monoasm!(self.jit,
                movq rax, [rbp - (conv(reg))];
                movq [rsp - ((0x28 + i * 8) as i64)], rax;
            );
        }*/
        monoasm! { self.jit_gen.jit,
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

            call fn_entry;
            popq r15;
            popq r13;
            movsxl r15, r15;
            cmpq r15, (-1);
            jeq exit;
        };
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], rax;
        exit:
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_nil(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], (NIL_VALUE);
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_integer(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        self.vm_get_addr_r15();
        monoasm! {
            self.jit_gen.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_constant(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        self.vm_get_addr_r15();
        monoasm! {
            self.jit_gen.jit,
            movq rdx, rdi;  // const_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (get_constant);
            call rax;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addri(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            addq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            addq rsi, 1;
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_neg(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            shrq rdi, 1;
            negq rdi;
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (neg_value);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addrr(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            testq rdi, 0x1;
            jeq generic;
            testq rsi, 0x1;
            jeq generic;
            subq rdi, 1;
            addq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_subri(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            addq rsi, 1;
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            subq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (sub_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_subrr(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            testq rdi, 0x1;
            jeq generic;
            testq rsi, 0x1;
            jeq generic;
            subq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (sub_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_mulrr(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            movq rax, (mul_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_divrr(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(label);
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            movq rax, (div_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    cmp_ops!(eq, ne, gt, ge, lt, le);
    cmp_ri_ops!(eq, ne, gt, ge, lt, le);

    fn vm_method_def(&mut self) -> DestLabel {
        let label = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
        label:
            movq rdx, rdi;  // const_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_method);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_br(&mut self) -> DestLabel {
        let br = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
        br:
            shlq rdi, 3;
            addq r13, rdi;
        };
        self.fetch_and_dispatch();
        br
    }

    fn vm_condbr(&mut self) -> DestLabel {
        let br = self.jit_gen.jit.label();
        let exit = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(br);
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jne exit;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        exit:
            shlq rdi, 3;
            addq r13, rdi;
        };
        self.fetch_and_dispatch();
        br
    }

    fn vm_condnotbr(&mut self) -> DestLabel {
        let br = self.jit_gen.jit.label();
        let exit = self.jit_gen.jit.label();
        self.jit_gen.jit.bind_label(br);
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jeq exit;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        exit:
            shlq rdi, 3;
            addq r13, rdi;
        };
        self.fetch_and_dispatch();
        br
    }

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let main = globals.func[globals.get_main_func()].as_normal();
        let mut eval = Self::new(main);
        eval.jit_gen.exec_toplevel(globals)(&mut eval, globals)
            .ok_or_else(|| std::mem::take(&mut eval.error).unwrap())
    }

    fn new(main: &NormalFuncInfo) -> Self {
        let pc_top = BcPcBase::new(main);
        let mut jit_gen = JitGen::new();
        // dispatch table.
        let entry_panic = jit_gen.jit.label();
        monoasm! { jit_gen.jit,
            entry_panic:
                movq rdi, rbx;
                movq rsi, r12;
                movq rax, (super::compiler::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        jit_gen.jit.finalize();
        let panic_addr = jit_gen.jit.get_label_absolute_address(entry_panic) as u64;
        let dispatch = vec![panic_addr; 256];
        Self {
            cur_fn: main.id,
            pc: pc_top + 0,
            pc_top,
            jit_gen,
            error: None,
            dispatch,
            class_version: 0,
        }
    }
}
