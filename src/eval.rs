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
    pub fn eval_hir(hir_context: &HIRContext, cur_fn: usize, args: &[Value]) -> Value {
        let locals_num = hir_context.functions[cur_fn].locals.len();
        let mut locals = vec![Value::Integer(0); locals_num];
        locals[0..args.len()].clone_from_slice(args);
        let register_num = hir_context.functions[cur_fn].register_num();
        let mut eval = Self {
            ssareg: vec![Value::Integer(0); register_num],
            cur_bb: hir_context.functions[cur_fn].entry_bb,
            prev_bb: 0,
            pc: 0,
        };
        loop {
            let op = &hir_context[eval.cur_bb].insts[eval.pc];
            eval.pc += 1;
            if let Some(val) = eval.eval(hir_context, op, &mut locals) {
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

    fn eval(
        &mut self,
        hir_context: &HIRContext,
        hir: &Hir,
        locals: &mut Vec<Value>,
    ) -> Option<Value> {
        match hir {
            Hir::Integer(ret, i) => {
                self[*ret] = Value::Integer(*i);
                None
            }
            Hir::Float(ret, f) => {
                self[*ret] = Value::Float(*f);
                None
            }
            Hir::CastIntFloat(op) => {
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
                let lhs = self.eval_operand(&op.lhs).as_i();
                let rhs = self.eval_operand(&op.rhs).as_i();
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
            Hir::FCmp(kind, op) => {
                let lhs = self[op.lhs].clone().as_f();
                let rhs = self[op.rhs].clone().as_f();
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
                let lhs = self[*lhs].as_i();
                let rhs = self.eval_operand(rhs).as_i();
                let b = match kind {
                    CmpKind::Eq => lhs == rhs,
                    CmpKind::Ne => lhs != rhs,
                    CmpKind::Lt => lhs < rhs,
                    CmpKind::Gt => lhs > rhs,
                    CmpKind::Le => lhs <= rhs,
                    CmpKind::Ge => lhs >= rhs,
                };
                let next_bb = if b { then_ } else { else_ };
                self.goto(*next_bb);
                None
            }
            Hir::FCmpBr(kind, lhs, rhs, then_, else_) => {
                let lhs = self[*lhs].as_f();
                let rhs = self[*rhs].as_f();
                let b = match kind {
                    CmpKind::Eq => lhs == rhs,
                    CmpKind::Ne => lhs != rhs,
                    CmpKind::Lt => lhs < rhs,
                    CmpKind::Gt => lhs > rhs,
                    CmpKind::Le => lhs <= rhs,
                    CmpKind::Ge => lhs >= rhs,
                };
                let next_bb = if b { then_ } else { else_ };
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
            Hir::Call(id, ret, arg) => {
                let arg = self[*arg].clone();
                self[*ret] = Evaluator::eval_hir(hir_context, *id, &[arg]);
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
                let reg = phi.iter().find(|(bb, _, _)| self.prev_bb == *bb).unwrap().1;
                self[*ret] = self[reg].clone();
                None
            }
        }
    }
}
