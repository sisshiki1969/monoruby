use super::*;

pub struct Evaluator {
    register: Vec<Value>,
}

impl Evaluator {
    pub fn eval_hir(hir_context: &HIRContext) -> Value {
        let register_num = hir_context.register_num();
        let mut eval = Self {
            register: vec![Value::Integer(0); register_num],
        };
        for (idx, hir) in hir_context.hirs.iter().enumerate() {
            let val = eval.eval(hir);
            eval.register[idx] = val;
        }
        eval.register[register_num - 1]
    }

    fn eval(&self, hir: &HIR) -> Value {
        match hir.kind() {
            HIRKind::Integer(i) => Value::Integer(*i),
            HIRKind::Float(f) => Value::Float(*f),
            HIRKind::IntAsFloat(lhs) => {
                let lhs = self.register[*lhs];
                Value::Float(lhs.as_i() as f64)
            }
            HIRKind::INeg(lhs) => {
                let lhs = self.register[*lhs];
                Value::Integer(-lhs.as_i())
            }
            HIRKind::FNeg(lhs) => {
                let lhs = self.register[*lhs];
                Value::Float(-lhs.as_f())
            }
            HIRKind::IAdd(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Integer(lhs.as_i() + rhs.as_i())
            }
            HIRKind::FAdd(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Float(lhs.as_f() + rhs.as_f())
            }
            HIRKind::ISub(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Integer(lhs.as_i() - rhs.as_i())
            }
            HIRKind::FSub(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Float(lhs.as_f() - rhs.as_f())
            }
            HIRKind::IMul(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Integer(lhs.as_i() * rhs.as_i())
            }
            HIRKind::FMul(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Float(lhs.as_f() * rhs.as_f())
            }
            HIRKind::IDiv(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Integer(lhs.as_i() / rhs.as_i())
            }
            HIRKind::FDiv(lhs, rhs) => {
                let lhs = self.register[*lhs];
                let rhs = self.register[*rhs];
                Value::Float(lhs.as_f() / rhs.as_f())
            }
        }
    }
}
