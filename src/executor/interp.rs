use super::compiler::JitGen;
use super::*;
use monoasm::*;
use monoasm_macro::monoasm;

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

impl std::ops::AddAssign<usize> for BcPc {
    fn add_assign(&mut self, offset: usize) {
        unsafe {
            *self = BcPc(self.0.add(offset));
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
    globals.func[func_id].as_normal().get_constant(const_id)
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
        let loop_ = eval.jit_gen.jit.label();
        let integer = eval.jit_gen.jit.label();
        let constant = eval.jit_gen.jit.label();
        let addri = eval.jit_gen.jit.label();
        let addrr = eval.jit_gen.jit.label();
        let ret = eval.jit_gen.jit.label();
        let entry_panic = eval.jit_gen.entry_panic;
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
        loop_:
        // fetch instruction and decode
            movq rax, [r13]; // rax <- :0:1:2:3
            addq r13, 8;
            movl rdi, rax;  // rdi <- :2:3
            shrq rax, 32;
            movzxw r15, rax;  // r15 <- :1
            shrq rax, 16;
            movzxw rax, rax;   // rax <- :0
        // dispatch
            cmpq rax, 6;
            jeq integer;
            cmpq rax, 7;
            jeq constant;

            movsxw rsi, rdi;    // rsi <- :3
            shrq rdi, 16;
            movzxw rdi, rdi;    // rdi <- :2

            cmpq rax, 130;
            jeq addrr;
            cmpq rax, 140;
            jeq addri;
            cmpq rax, 148;
            jeq ret;
            call entry_panic;
        };

        //BcOp::Integer
        eval.jit_gen.jit.bind_label(integer);
        eval.vm_get_addr_r15();
        monoasm! {
            eval.jit_gen.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
            jmp loop_;
        };

        //BcOp::Const
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
            jmp loop_;
        };

        //BcOp::Ret
        eval.jit_gen.jit.bind_label(ret);
        eval.vm_get_addr_r15();
        monoasm! {
            eval.jit_gen.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Addri
        eval.vm_addri(addri, loop_);
        //BcOp::Addrr
        eval.vm_addrr(addrr, loop_);

        eval.jit_gen.jit.finalize();
        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(eval.jit_gen.jit.get_label_absolute_address(entry)) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: absolute address of the register
    fn vm_get_addr_rdi(&mut self) {
        monoasm! {
            self.jit_gen.jit,
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
        monoasm! {
            self.jit_gen.jit,
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
        monoasm! {
            self.jit_gen.jit,
            shlq r15, 3;
            addq r15, 16;
            negq r15;
            addq r15, rbp;
        };
    }

    fn vm_addri(&mut self, addri: DestLabel, loop_: DestLabel) {
        self.jit_gen.jit.bind_label(addri);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! {
            self.jit_gen.jit,
            shlq rsi, 1;
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            addq rdi, rsi;
            movq [r15], rdi;
            jmp loop_;
        generic:
            // generic path
            addq rsi, 1;
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
            jmp loop_;
        };
    }

    fn vm_addrr(&mut self, addrr: DestLabel, loop_: DestLabel) {
        self.jit_gen.jit.bind_label(addrr);
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! {
            self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            testq rdi, 0x1;
            jeq generic;
            testq rsi, 0x1;
            jeq generic;
            subq rdi, 1;
            addq rdi, rsi;
            movq [r15], rdi;
            jmp loop_;
        generic:
            // generic path
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
            jmp loop_;
        };
    }

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let main = globals.func[globals.get_main_func()].as_normal();
        let mut eval = Self::new(main);
        eval.jit_gen.exec_toplevel(globals)(&mut eval, globals)
            .ok_or_else(|| std::mem::take(&mut eval.error).unwrap())
    }

    fn new(main: &NormalFuncInfo) -> Self {
        let pc_top = BcPcBase::new(main);
        Self {
            cur_fn: main.id,
            pc: pc_top + 0,
            pc_top,
            call_stack: Stack::new(),
            jit_gen: JitGen::new(),
            error: None,
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
                    self[ret] = globals.func[self.cur_fn].as_normal().get_constant(id);
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
                BcOp::Br(next_pc) => {
                    self.pc = self.pc_top + next_pc;
                }
                BcOp::CondBr(cond_, then_) => {
                    if self[cond_].to_bool() {
                        self.pc = self.pc_top + then_;
                    };
                }
                BcOp::CondNotBr(cond_, else_) => {
                    if !self[cond_].to_bool() {
                        self.pc = self.pc_top + else_;
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
