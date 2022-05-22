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

impl BcPc {
    fn get(&self) -> u64 {
        unsafe { *(self.0) }
    }
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: FuncId,
    pc: BcPc,
    pc_top: BcPcBase,
    call_stack: Stack,
    pub jit_gen: JitGen,
    pub error: Option<MonorubyErr>,
    dispatch: Vec<u64>,
    class_version: usize,
}

impl std::ops::Index<u16> for Interp {
    type Output = Value;
    fn index(&self, index: u16) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<u16> for Interp {
    fn index_mut(&mut self, index: u16) -> &mut Value {
        &mut self.call_stack[index]
    }
}

/*fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}*/

extern "C" fn get_constant(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    const_id: u32,
) -> Value {
    globals.func[func_id].as_normal().get_literal(const_id)
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
        let bc_top = BcPcBase::new(main);
        let bc_pc = bc_top + 0;
        let entry = eval.jit_gen.jit.label();
        let fn_entry = eval.jit_gen.jit.label();
        let regs = main.total_reg_num();

        monoasm! {
            eval.jit_gen.jit,
            entry:
                pushq rbx;
                pushq r12;
                pushq r13;
                pushq r14;
                pushq r15;
                movq rbx, rdi;  // rbx: &mut Interp
                movq r12, rsi;  // r12: &mut Globals
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
                subq rsp, (((regs + 1) & (-2i64 as usize)) * 8);
                movq r13, (bc_pc.0);    // r13: BcPc
        };
        eval.fetch_and_dispatch();

        //BcOp::Br
        let br = eval.vm_br();

        //BcOp::CondBr
        let condbr = eval.vm_condbr();

        //BcOp::CondNotBr
        let condnotbr = eval.vm_condnotbr();

        //BcOp::Nil
        let nil = eval.vm_nil();

        //BcOp::Integer
        let integer = eval.jit_gen.jit.label();
        eval.jit_gen.jit.bind_label(integer);
        eval.vm_get_addr_r15();
        monoasm! {
            eval.jit_gen.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        eval.fetch_and_dispatch();

        //BcOp::Const
        let constant = eval.jit_gen.jit.label();
        eval.jit_gen.jit.bind_label(constant);
        eval.vm_get_addr_r15();
        monoasm! {
            eval.jit_gen.jit,
            movq rcx, rdi;  // const_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rdx, (main_id.0);  // FuncId
            movq rax, (get_constant);
            call rax;
            movq [r15], rax;
        };
        eval.fetch_and_dispatch();

        //BcOp::Ret
        let ret = eval.jit_gen.jit.label();
        eval.jit_gen.jit.bind_label(ret);
        eval.vm_get_addr_r15();
        monoasm! { eval.jit_gen.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Mov
        let mov = eval.jit_gen.jit.label();
        eval.jit_gen.jit.bind_label(mov);
        eval.vm_get_addr_r15();
        eval.vm_get_addr_rdi();
        monoasm! { eval.jit_gen.jit,
            movq rax, [rdi];
            movq [r15], rax;
        };
        eval.fetch_and_dispatch();

        //BcOp::Add
        let addrr = eval.vm_addrr();
        //BcOp::Sub
        let subrr = eval.vm_subrr();
        //BcOp::Mul
        let mulrr = eval.vm_mulrr();
        //BcOp::Div
        let divrr = eval.vm_divrr();

        //BcOp::Eq
        let eqrr = eval.vm_eqrr();
        //BcOp::Ne
        let nerr = eval.vm_nerr();
        //BcOp::Lt
        let ltrr = eval.vm_ltrr();
        //BcOp::Le
        let lerr = eval.vm_lerr();
        //BcOp::Gt
        let gtrr = eval.vm_gtrr();
        //BcOp::Ge
        let gerr = eval.vm_gerr();

        //BcOp::Addri
        let addri = eval.vm_addri();
        //BcOp::Subri
        let subri = eval.vm_subri();

        //BcOp::Eqri
        let eqri = eval.vm_eqri();
        //BcOp::Neri
        let neri = eval.vm_neri();
        //BcOp::Ltri
        let ltri = eval.vm_ltri();
        //BcOp::Leri
        let leri = eval.vm_leri();
        //BcOp::Gtri
        let gtri = eval.vm_gtri();
        //BcOp::Geri
        let geri = eval.vm_geri();

        eval.jit_gen.jit.finalize();

        eval.dispatch[3] = eval.jit_gen.jit.get_label_absolute_address(br) as _;
        eval.dispatch[4] = eval.jit_gen.jit.get_label_absolute_address(condbr) as _;
        eval.dispatch[5] = eval.jit_gen.jit.get_label_absolute_address(condnotbr) as _;
        eval.dispatch[6] = eval.jit_gen.jit.get_label_absolute_address(integer) as _;
        eval.dispatch[7] = eval.jit_gen.jit.get_label_absolute_address(constant) as _;
        eval.dispatch[8] = eval.jit_gen.jit.get_label_absolute_address(nil) as _;

        eval.dispatch[130] = eval.jit_gen.jit.get_label_absolute_address(addrr) as _;
        eval.dispatch[131] = eval.jit_gen.jit.get_label_absolute_address(subrr) as _;
        eval.dispatch[132] = eval.jit_gen.jit.get_label_absolute_address(mulrr) as _;
        eval.dispatch[133] = eval.jit_gen.jit.get_label_absolute_address(divrr) as _;
        eval.dispatch[134] = eval.jit_gen.jit.get_label_absolute_address(eqrr) as _;
        eval.dispatch[135] = eval.jit_gen.jit.get_label_absolute_address(nerr) as _;
        eval.dispatch[136] = eval.jit_gen.jit.get_label_absolute_address(ltrr) as _;
        eval.dispatch[137] = eval.jit_gen.jit.get_label_absolute_address(lerr) as _;
        eval.dispatch[138] = eval.jit_gen.jit.get_label_absolute_address(gtrr) as _;
        eval.dispatch[139] = eval.jit_gen.jit.get_label_absolute_address(gerr) as _;

        eval.dispatch[140] = eval.jit_gen.jit.get_label_absolute_address(addri) as _;
        eval.dispatch[141] = eval.jit_gen.jit.get_label_absolute_address(subri) as _;
        eval.dispatch[142] = eval.jit_gen.jit.get_label_absolute_address(eqri) as _;
        eval.dispatch[143] = eval.jit_gen.jit.get_label_absolute_address(neri) as _;
        eval.dispatch[144] = eval.jit_gen.jit.get_label_absolute_address(ltri) as _;
        eval.dispatch[145] = eval.jit_gen.jit.get_label_absolute_address(leri) as _;
        eval.dispatch[146] = eval.jit_gen.jit.get_label_absolute_address(gtri) as _;
        eval.dispatch[147] = eval.jit_gen.jit.get_label_absolute_address(geri) as _;

        eval.dispatch[148] = eval.jit_gen.jit.get_label_absolute_address(ret) as _;
        eval.dispatch[149] = eval.jit_gen.jit.get_label_absolute_address(mov) as _;

        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(eval.jit_gen.jit.get_label_absolute_address(entry)) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
    }

    /// Fetch instruction and decode
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
                movq rax, (super::compiler::panic);
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
            call_stack: Stack::new(),
            jit_gen,
            error: None,
            dispatch,
            class_version: 0,
        }
    }

    fn get_op(&mut self) -> BcOp {
        let op = self.pc.get();
        self.pc += 1;
        BcOp::from_u64(op)
    }

    fn push_frame(&mut self, args: u16, len: u16, func: &NormalFuncInfo, ret: Option<u16>) {
        let pc = self.pc - self.pc_top;
        self.call_stack
            .push_frame(args, len, func, self.cur_fn, pc, ret);
        let pc = BcPcBase::new(func);
        self.pc_top = pc;
        self.pc = pc + 0;
        self.cur_fn = func.id;
    }

    fn pop_frame(&mut self, val: Value, globals: &Globals) -> bool {
        let (b, func, pc, ret) = self.call_stack.pop_frame();
        if b {
            return true;
        };
        self.cur_fn = func;
        self.pc_top = BcPcBase::new(&globals.func[func].as_normal());
        self.pc = self.pc_top + pc;
        if let Some(ret) = ret {
            self[ret] = val;
        }
        false
    }

    fn eval_loop(&mut self, globals: &mut Globals) -> Result<Value> {
        loop {
            let op = self.get_op();
            match op {
                BcOp::Integer(ret, i) => {
                    self[ret] = Value::integer(i);
                }
                BcOp::Const(ret, id) => {
                    self[ret] = globals.func[self.cur_fn].as_normal().get_literal(id);
                }
                BcOp::Nil(ret) => {
                    self[ret] = Value::nil();
                }
                BcOp::Neg(dst, src) => {
                    self[dst] = match self[src].unpack() {
                        RV::Integer(i) => Value::integer(-i),
                        RV::Float(f) => Value::float(-f),
                        _ => unreachable!(),
                    };
                }
                BcOp::Add(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    self[ret] = add_values(lhs, rhs);
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = match lhs.unpack() {
                        RV::Integer(lhs) => Value::integer(lhs + rhs as i32),
                        RV::Float(lhs) => Value::float(lhs + rhs as f64),
                        _ => unreachable!(),
                    };
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    self[ret] = sub_values(lhs, rhs);
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = match lhs.unpack() {
                        RV::Integer(lhs) => Value::integer(lhs - rhs as i32),
                        RV::Float(lhs) => Value::float(lhs - rhs as f64),
                        _ => unreachable!(),
                    };
                }
                BcOp::Mul(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = mul_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Div(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = div_values(lhs, rhs);
                    self[ret] = v;
                }
                BcOp::Eq(ret, lhs, rhs) => {
                    self[ret] = cmp_eq_values(self[lhs], self[rhs]);
                }
                BcOp::Ne(ret, lhs, rhs) => {
                    self[ret] = cmp_ne_values(self[lhs], self[rhs]);
                }
                BcOp::Ge(ret, lhs, rhs) => {
                    self[ret] = cmp_ge_values(self[lhs], self[rhs]);
                }
                BcOp::Gt(ret, lhs, rhs) => {
                    self[ret] = cmp_gt_values(self[lhs], self[rhs]);
                }
                BcOp::Le(ret, lhs, rhs) => {
                    self[ret] = cmp_le_values(self[lhs], self[rhs]);
                }
                BcOp::Lt(ret, lhs, rhs) => {
                    self[ret] = cmp_lt_values(self[lhs], self[rhs]);
                }
                BcOp::Eqri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_eq_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Neri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_ne_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Geri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_ge_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Gtri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_gt_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Leri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_le_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Ltri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = cmp_lt_values(lhs, Value::integer(rhs as i32));
                }
                BcOp::Ret(lhs) => {
                    let val = self[lhs];
                    if self.pop_frame(val, globals) {
                        return Ok(val);
                    };
                }
                BcOp::Mov(dst, local) => {
                    self[dst] = self[local];
                }
                BcOp::FnCall(id) => {
                    fn check_arity(arity: usize, len: u16) -> Result<()> {
                        if arity != len as usize {
                            Err(MonorubyErr::WrongArguments(format!(
                                "number of arguments mismatch. expected:{} actual:{}",
                                arity, len
                            )))
                        } else {
                            Ok(())
                        }
                    }
                    let CallsiteInfo {
                        name,
                        ret,
                        args,
                        len,
                        cache: (version, cached_func),
                    } = globals.func[id];
                    let func_id = if version == self.class_version {
                        cached_func
                    } else {
                        match globals.get_method(name) {
                            Some(func_id) => {
                                globals.func[id].cache = (self.class_version, func_id);
                                func_id
                            }
                            None => return Err(MonorubyErr::MethodNotFound(name)),
                        }
                    };
                    let info = &globals.func[func_id];
                    check_arity(info.arity(), len)?;
                    match &info.kind {
                        FuncKind::Normal(info) => {
                            self.push_frame(args, len, info, ret);
                        }
                        FuncKind::Builtin {
                            abs_address: address,
                        } => {
                            let arg_ptr = Arg::new(&self[args] as *const _);
                            let func = unsafe { std::mem::transmute::<u64, BuiltinFn>(*address) };
                            let v = func(self, globals, arg_ptr, len as usize);
                            match ret {
                                None => {}
                                Some(ret) => {
                                    self[ret] = v;
                                }
                            }
                        }
                    }
                }
                BcOp::Br(disp) => {
                    self.pc += disp;
                }
                BcOp::CondBr(cond_, disp) => {
                    if self[cond_].to_bool() {
                        self.pc += disp;
                    };
                }
                BcOp::CondNotBr(cond_, disp) => {
                    if !self[cond_].to_bool() {
                        self.pc += disp;
                    };
                }
                BcOp::MethodDef(id) => {
                    let MethodDefInfo { name, func } = globals.func[id];
                    globals.func.insert(name, func);
                    self.class_version += 1;
                }
            }
        }
    }
}
