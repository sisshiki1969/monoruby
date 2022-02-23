use super::*;

pub struct Evaluator {
    ssareg: Vec<Value>,
    cur_bb: usize,
    prev_bb: usize,
    pc: usize,
}

impl std::ops::Index<SsaReg> for Evaluator {
    type Output = Value;

    fn index(&self, i: SsaReg) -> &Value {
        &self.ssareg[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for Evaluator {
    fn index_mut(&mut self, i: SsaReg) -> &mut Value {
        &mut self.ssareg[i.to_usize()]
    }
}

impl Evaluator {
    pub fn eval_hir(
        hir_context: &HIRContext,
        local_map: &mut HashMap<String, (usize, Type)>,
        locals: &mut Vec<Value>,
    ) -> Value {
        let locals_num = local_map.len();
        locals.resize(locals_num, Value::Integer(0));
        let register_num = hir_context.register_num();
        let mut eval = Self {
            ssareg: vec![Value::Integer(0); register_num],
            cur_bb: 0,
            prev_bb: 0,
            pc: 0,
        };
        loop {
            let op = &hir_context[eval.cur_bb].insts[eval.pc];
            eval.pc += 1;
            if let Some(val) = eval.eval(op, locals) {
                return val;
            }
        }
    }

    fn goto(&mut self, bb: usize) {
        self.prev_bb = self.cur_bb;
        self.cur_bb = bb;
        self.pc = 0;
    }

    fn eval_operand(&self, op: &HirOperand) -> Value {
        match op {
            HirOperand::Const(c) => c.clone(),
            HirOperand::Reg(r) => self[*r].clone(),
        }
    }

    fn eval(&mut self, hir: &Hir, locals: &mut Vec<Value>) -> Option<Value> {
        match hir {
            Hir::Integer(ret, i) => {
                self[*ret] = Value::Integer(*i);
                None
            }
            Hir::Float(ret, f) => {
                self[*ret] = Value::Float(*f);
                None
            }
            Hir::IntAsFloat(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Float(src.as_i() as f64);
                None
            }
            Hir::INeg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Integer(-src.as_i());
                None
            }
            Hir::FNeg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Float(-src.as_f());
                None
            }
            Hir::IAdd(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Integer(lhs.as_i() + rhs.as_i());
                None
            }
            Hir::FAdd(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() + rhs.as_f());
                None
            }
            Hir::ISub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Integer(lhs.as_i() - rhs.as_i());
                None
            }
            Hir::FSub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() - rhs.as_f());
                None
            }
            Hir::IMul(op) => {
                let lhs = self[op.lhs].clone();
                let rhs = self[op.rhs].clone();
                self[op.ret] = Value::Integer(lhs.as_i() * rhs.as_i());
                None
            }
            Hir::FMul(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() * rhs.as_f());
                None
            }
            Hir::IDiv(op) => {
                let lhs = self[op.lhs].clone();
                let rhs = self[op.rhs].clone();
                self[op.ret] = Value::Integer(lhs.as_i() / rhs.as_i());
                None
            }
            Hir::FDiv(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() / rhs.as_f());
                None
            }
            Hir::ICmp(kind, op) => {
                let lhs = self[op.lhs].as_i();
                let rhs = self[op.rhs].as_i();
                self[op.ret] = Value::Bool(match kind {
                    CmpKind::Eq => lhs == rhs,
                    CmpKind::Ne => lhs != rhs,
                    CmpKind::Lt => lhs < rhs,
                    CmpKind::Gt => lhs > rhs,
                    CmpKind::Le => lhs <= rhs,
                    CmpKind::Ge => lhs >= rhs,
                });
                None
            }
            Hir::ICmpBr(kind, lhs, rhs, then_, else_) => {
                /*let lhs = match lhs {
                    HirOperand::Reg(r) => self[*r].as_i(),
                    HirOperand::Const(c) => c.as_i(),
                };
                let rhs = match rhs {
                    HirOperand::Reg(r) => self[*r].as_i(),
                    HirOperand::Const(c) => c.as_i(),
                };*/
                let lhs = self[*lhs].as_i();
                let rhs = self[*rhs].as_i();
                let b = match kind {
                    CmpKind::Eq => lhs == rhs,
                    CmpKind::Ne => lhs != rhs,
                    CmpKind::Lt => lhs < rhs,
                    CmpKind::Gt => lhs > rhs,
                    CmpKind::Le => lhs <= rhs,
                    CmpKind::Ge => lhs >= rhs,
                };
                let next_bb = if b { else_ } else { then_ };
                self.goto(*next_bb);
                None
            }
            Hir::Ret(lhs) => match lhs {
                HirOperand::Reg(lhs) => Some(self[*lhs].clone()),
                HirOperand::Const(c) => Some(c.clone()),
            },
            Hir::LocalStore(ret, ident, rhs) => {
                locals[ident.0] = self[*rhs].clone();
                if let Some(ret) = ret {
                    self[*ret] = self[*rhs].clone();
                }
                None
            }
            Hir::LocalLoad(ident, lhs) => {
                self[*lhs] = locals[ident.0].clone();
                None
            }
            Hir::Br(next_bb) => {
                self.goto(*next_bb);
                None
            }
            Hir::CondBr(cond_, then_, else_) => {
                let next_bb = if self[*cond_] == Value::Bool(false) {
                    else_
                } else {
                    then_
                };
                self.goto(*next_bb);
                None
            }
            Hir::Phi(ret, phi) => {
                let reg = phi
                    .iter()
                    .find(|(bb, _)| self.prev_bb == *bb)
                    .unwrap()
                    .1
                    .clone();
                self[*ret] = self[reg].clone();
                None
            }
        }
    }
}
