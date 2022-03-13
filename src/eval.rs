use super::hir::SsaReg;
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

macro_rules! value_op {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match ($lhs, $rhs) {
            (Value::Integer($lhs), Value::Integer($rhs)) => $lhs.$op(&$rhs),
            (Value::Integer($lhs), Value::Float($rhs)) => ($lhs as f64).$op(&$rhs),
            (Value::Float($lhs), Value::Integer($rhs)) => $lhs.$op(&($rhs as f64)),
            (Value::Float($lhs), Value::Float($rhs)) => $lhs.$op(&$rhs),
            _ => unreachable!(),
        }
    }};
}

impl Evaluator {
    pub fn eval_function(hir_context: &HirContext, cur_fn: usize, args: &[Value]) -> Value {
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
            let bb = &hir_context[eval.cur_bb];
            let op = &bb.insts[eval.pc];
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
        hir_context: &HirContext,
        hir: &Hir,
        locals: &mut Vec<Value>,
    ) -> Option<Value> {
        match hir {
            Hir::Integer(ret, i) => {
                self[*ret] = Value::Integer(*i);
            }
            Hir::Float(ret, f) => {
                self[*ret] = Value::Float(*f);
            }
            Hir::Neg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = match src {
                    Value::Integer(i) => Value::Integer(-i),
                    Value::Float(f) => Value::Float(-f),
                    _ => unreachable!(),
                };
            }
            Hir::Add(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs, rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs + rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Sub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs, rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 - rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs - rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Mul(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs, rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs * rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Div(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs, rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 / rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs / rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Cmp(kind, op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Bool(match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                });
            }
            Hir::CmpBr(kind, lhs, rhs, then_, else_) => {
                let lhs = self[*lhs].clone();
                let rhs = self.eval_operand(rhs);
                let b = match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                };
                let next_bb = if b { then_ } else { else_ };
                self.goto(*next_bb);
            }
            Hir::Ret(lhs) => return Some(self.eval_operand(lhs)),
            Hir::LocalStore(ret, ident, rhs) => {
                locals[*ident] = self[*rhs].clone();
                if let Some(ret) = ret {
                    self[*ret] = self[*rhs].clone();
                }
            }
            Hir::LocalLoad(ident, lhs) => {
                self[*lhs] = locals[*ident].clone();
            }
            Hir::Call(id, ret, args) => {
                let args = args
                    .iter()
                    .map(|op| self.eval_operand(op))
                    .collect::<Vec<Value>>();
                if let Some(ret) = *ret {
                    self[ret] = Evaluator::eval_function(hir_context, *id, &args)
                }
            }
            Hir::Br(next_bb) => {
                self.goto(*next_bb);
            }
            Hir::CondBr(cond_, then_, else_) => {
                let next_bb = if self[*cond_] == Value::Bool(false) {
                    else_
                } else {
                    then_
                };
                self.goto(*next_bb);
            }
            Hir::Phi(ret, phi) => {
                let reg = phi.iter().find(|(bb, _)| self.prev_bb == *bb).unwrap().1;
                self[*ret] = self[reg].clone();
            }
        }
        None
    }
}
