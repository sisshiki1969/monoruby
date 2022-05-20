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

fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}

impl Interp {
    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let main = globals.func[globals.get_main_func()].as_normal();
        let mut eval = Self::new(main);
        //eval.push_frame(0, 0, main, None);
        //eval.eval_loop(globals)
        let bc_top = BcPcBase::new(main);
        let bc_pc = bc_top + 0;
        let entry = eval.jit_gen.jit.label();
        let loop_ = eval.jit_gen.jit.label();
        let integer = eval.jit_gen.jit.label();
        let ret = eval.jit_gen.jit.label();
        let entry_panic = eval.jit_gen.entry_panic;
        let regs = main.total_reg_num();
        monoasm! {
            eval.jit_gen.jit,
        entry:
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (((regs + 1) & (-2i64 as usize)) * 8);
            movq r8, (bc_pc.0);
        loop_:
            movq rax, [r8]; // rax <- :0:1:2:3
            movl rsi, rax;  // rsi <- :2:3
            shrq rax, 32;
            movl rdi, rax;  // rdi <- :0:1
            andq rdi, 0xffff;   // rdi <- :1
            shrq rax, 16;
            andq rax, 0xffff;   // rax <- :0
            cmpq rax, 6;
            jeq integer;
            cmpq rax, 148;
            jeq ret;
            jmp entry_panic;
        integer:
            shlq rsi, 1;
            addq rsi, 1;
            shlq rdi, 3;
            //addq rdi, 16;
            movq rax, rbp;
            subq rax, rdi;
            movq [rax - 16], rsi;
            addq r8, 8;
            jmp loop_;
        ret:
            shlq rdi, 3;
            //addq rdi, 16;
            movq rax, rbp;
            subq rax, rdi;
            movq rax, [rax - 16];
            leave;
            ret;
        };
        eval.jit_gen.jit.finalize();
        let addr: fn() -> Option<Value> =
            unsafe { std::mem::transmute(eval.jit_gen.jit.get_label_absolute_address(entry)) };
        match addr() {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
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
