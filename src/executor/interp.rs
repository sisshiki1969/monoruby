use super::*;

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: FuncId,
    pc: BcPc,
    pc_top: BcPcBase,
    call_stack: Stack,
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

impl Interp {
    pub fn eval_toplevel(fn_store: &FuncStore) -> Value {
        let mut eval = Self::new(fn_store);
        let hir_func = &fn_store[FuncId(0)].as_normal_ref();
        eval.push_frame(0, 0, hir_func, None);
        eval.eval_loop(fn_store)
    }

    fn new(fn_store: &FuncStore) -> Self {
        let cur_fn = FuncId(0);
        let pc_top = BcPcBase::new(&fn_store[cur_fn].as_normal_ref());
        Self {
            cur_fn,
            pc: pc_top + 0,
            pc_top,
            call_stack: Stack::new(),
        }
    }

    fn get_op(&mut self) -> BcOp {
        let op = self.pc.get();
        self.pc += 1;
        op
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

    fn pop_frame(&mut self, val: Value, fn_store: &FuncStore) -> bool {
        let (b, func, pc, ret) = self.call_stack.pop_frame();
        if b {
            return true;
        };
        self.cur_fn = func;
        self.pc_top = BcPcBase::new(&fn_store[func].as_normal_ref());
        self.pc = self.pc_top + pc;
        if let Some(ret) = ret {
            self[ret] = val;
        }
        false
    }

    fn eval_loop(&mut self, fn_store: &FuncStore) -> Value {
        loop {
            let op = self.get_op();
            match op {
                BcOp::Integer(ret, i) => {
                    self[ret] = Value::integer(i);
                }
                BcOp::Const(ret, id) => {
                    self[ret] = fn_store[self.cur_fn].as_normal_ref().get_constant(id);
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
                    let v = if lhs.is_fnum() && rhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() + rhs.as_fnum())
                    } else {
                        add_values(lhs, rhs)
                    };
                    self[ret] = v;
                }
                BcOp::Addri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() + rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs + rhs as i32),
                            RV::Float(lhs) => Value::float(lhs + rhs as f64),
                            _ => unreachable!(),
                        }
                    };
                }
                BcOp::Sub(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    let rhs = self[rhs];
                    let v = if lhs.is_fnum() && rhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() - rhs.as_fnum())
                    } else {
                        sub_values(lhs, rhs)
                    };
                    self[ret] = v;
                }
                BcOp::Subri(ret, lhs, rhs) => {
                    let lhs = self[lhs];
                    self[ret] = if lhs.is_fnum() {
                        Value::fixnum(lhs.as_fnum() - rhs as i64)
                    } else {
                        match lhs.unpack() {
                            RV::Integer(lhs) => Value::integer(lhs - rhs as i32),
                            RV::Float(lhs) => Value::float(lhs - rhs as f64),
                            _ => unreachable!(),
                        }
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
                    self[ret] = cmp_eq_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Neri(ret, lhs, rhs) => {
                    self[ret] = cmp_ne_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Geri(ret, lhs, rhs) => {
                    self[ret] = cmp_ge_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Gtri(ret, lhs, rhs) => {
                    self[ret] = cmp_gt_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Leri(ret, lhs, rhs) => {
                    self[ret] = cmp_le_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Ltri(ret, lhs, rhs) => {
                    self[ret] = cmp_lt_ri_values(self[lhs], rhs as i64);
                }
                BcOp::Ret(lhs) => {
                    let val = self[lhs];
                    if self.pop_frame(val, fn_store) {
                        return val;
                    };
                }
                BcOp::Mov(dst, local) => {
                    self[dst] = self[local];
                }
                BcOp::FnCall(id, ret, args, len) => {
                    fn check_arity(arity: usize, len: u16) {
                        if arity != len as usize {
                            panic!(
                                "number of arguments mismatch. expected:{} actual:{}",
                                arity, len
                            );
                        }
                    }
                    let ret = if ret == u16::MAX { None } else { Some(ret) };
                    let func_id = fn_store.find_method_or_panic(id);
                    match &fn_store[func_id].kind {
                        FuncKind::Normal(info) => {
                            check_arity(info.arity(), len);
                            self.push_frame(args, len, info, ret);
                        }
                        FuncKind::Builtin {
                            abs_address: address,
                            arity,
                        } => {
                            check_arity(*arity, len);
                            let v = match len {
                                0 => unsafe {
                                    std::mem::transmute::<u64, extern "C" fn() -> Value>(*address)()
                                },
                                1 => unsafe {
                                    std::mem::transmute::<u64, extern "C" fn(Value) -> Value>(
                                        *address,
                                    )(self[args])
                                },
                                2 => unsafe {
                                    std::mem::transmute::<u64, extern "C" fn(Value, Value) -> Value>(
                                        *address,
                                    )(self[args], self[args + 1])
                                },
                                _ => unimplemented!(),
                            };
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
                    if self[cond_] != Value::bool(false) {
                        self.pc = self.pc_top + then_;
                    };
                }
                BcOp::CondNotBr(cond_, else_) => {
                    if self[cond_] == Value::bool(false) {
                        self.pc = self.pc_top + else_;
                    };
                }
            }
        }
    }
}