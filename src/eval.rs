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
    pub fn eval_hir(hir_context: &HIRContext) -> Value {
        let register_num = hir_context.register_num();
        let mut eval = Self {
            ssareg: vec![Value::Integer(0); register_num],
        };
        let mut pc = 0usize;
        loop {
            let op = &hir_context.hirs[pc];
            pc += 1;
            if let Some(val) = eval.eval(op) {
                return val;
            }
        }
    }

    fn eval(&mut self, hir: &HIR) -> Option<Value> {
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
                let src = self[op.src];
                self[op.ret] = Value::Float(src.as_i() as f64);
                None
            }
            HIR::INeg(op) => {
                let src = self[op.src];
                self[op.ret] = Value::Integer(-src.as_i());
                None
            }
            HIR::FNeg(op) => {
                let src = self[op.src];
                self[op.ret] = Value::Float(-src.as_f());
                None
            }
            HIR::IAdd(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Integer(lhs.as_i() + rhs.as_i());
                None
            }
            HIR::FAdd(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Float(lhs.as_f() + rhs.as_f());
                None
            }
            HIR::ISub(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Integer(lhs.as_i() - rhs.as_i());
                None
            }
            HIR::FSub(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Float(lhs.as_f() - rhs.as_f());
                None
            }
            HIR::IMul(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Integer(lhs.as_i() * rhs.as_i());
                None
            }
            HIR::FMul(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Float(lhs.as_f() * rhs.as_f());
                None
            }
            HIR::IDiv(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Integer(lhs.as_i() / rhs.as_i());
                None
            }
            HIR::FDiv(op) => {
                let lhs = self[op.lhs];
                let rhs = self[op.rhs];
                self[op.ret] = Value::Float(lhs.as_f() / rhs.as_f());
                None
            }
            HIR::Ret(lhs) => Some(self[*lhs]),
        }
    }
}
