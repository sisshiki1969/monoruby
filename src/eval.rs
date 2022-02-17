use super::*;

pub struct Evaluator {
    ssareg: Vec<Value>,
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
        };
        let mut pc = 0usize;
        loop {
            let op = &hir_context.insts[pc];
            pc += 1;
            if let Some(val) = eval.eval(op, locals) {
                return val;
            }
        }
    }

    fn eval_operand(&self, op: &HIROperand) -> Value {
        match op {
            HIROperand::Const(c) => c.clone(),
            HIROperand::Reg(r) => self[*r].clone(),
        }
    }

    fn eval(&mut self, hir: &HIR, locals: &mut Vec<Value>) -> Option<Value> {
        match hir {
            HIR::Integer(ret, i) => {
                self[*ret] = Value::Integer(*i);
                None
            }
            HIR::Float(ret, f) => {
                self[*ret] = Value::Float(*f);
                None
            }
            HIR::IntAsFloat(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Float(src.as_i() as f64);
                None
            }
            HIR::INeg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Integer(-src.as_i());
                None
            }
            HIR::FNeg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = Value::Float(-src.as_f());
                None
            }
            HIR::IAdd(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Integer(lhs.as_i() + rhs.as_i());
                None
            }
            HIR::FAdd(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() + rhs.as_f());
                None
            }
            HIR::ISub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Integer(lhs.as_i() - rhs.as_i());
                None
            }
            HIR::FSub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() - rhs.as_f());
                None
            }
            HIR::IMul(op) => {
                let lhs = self[op.lhs].clone();
                let rhs = self[op.rhs].clone();
                self[op.ret] = Value::Integer(lhs.as_i() * rhs.as_i());
                None
            }
            HIR::FMul(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() * rhs.as_f());
                None
            }
            HIR::IDiv(op) => {
                let lhs = self[op.lhs].clone();
                let rhs = self[op.rhs].clone();
                self[op.ret] = Value::Integer(lhs.as_i() / rhs.as_i());
                None
            }
            HIR::FDiv(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::Float(lhs.as_f() / rhs.as_f());
                None
            }
            HIR::Ret(lhs) => match lhs {
                HIROperand::Reg(lhs) => Some(self[*lhs].clone()),
                HIROperand::Const(c) => Some(c.clone()),
            },
            HIR::LocalStore(ident, rhs) => {
                locals[ident.0] = self[*rhs].clone();
                None
            }
            HIR::LocalLoad(ident, lhs) => {
                self[*lhs] = locals[ident.0].clone();
                None
            }
        }
    }
}
