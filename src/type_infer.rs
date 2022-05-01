use monoasm::DestLabel;

use super::*;

mod codegen;
mod hir;
mod mcir;
mod mir;
use codegen::*;
pub use hir::HirContext;
use hir::*;
use mcir::*;
use mir::*;

pub struct Evaluator {
    codegen: Codegen,
    mir_context: MirContext,
    mcir_context: McIrContext,
    jit_state: HashMap<(HirFuncId, Vec<Type>), JitState>,
    call_stack: Stack,
}

struct Stack {
    stack: Vec<Value>,
    bp: usize,
    ssa_base: usize,
    args_len: usize,
}

impl std::ops::Index<SsaReg> for Stack {
    type Output = Value;
    fn index(&self, index: SsaReg) -> &Value {
        &self.stack[self.ssa_base + index.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for Stack {
    fn index_mut(&mut self, index: SsaReg) -> &mut Value {
        &mut self.stack[self.ssa_base + index.to_usize()]
    }
}

impl Stack {
    fn new() -> Self {
        Self {
            stack: vec![],
            bp: 0,
            ssa_base: 0,
            args_len: 0,
        }
    }

    fn get_local(&self, index: usize) -> Value {
        self.stack[self.bp + index]
    }

    fn set_local(&mut self, index: usize, value: Value) {
        self.stack[self.bp + index] = value;
    }

    /*fn args_len(&self) -> usize {
        self.args_len
    }*/

    fn args(&self) -> &[Value] {
        &self.stack[self.bp..self.bp + self.args_len]
    }

    fn push_frame(&mut self, args: &[Value], hir_func: &HirFunction) {
        let local_num = hir_func.locals.len();
        let reg_num = hir_func.register_num();
        let args_len = args.len();
        self.stack.push(Value::from_unchecked(self.args_len as u64));
        self.stack.push(Value::from_unchecked(self.ssa_base as u64));
        self.stack.push(Value::from_unchecked(self.bp as u64));
        self.bp = self.stack.len();
        self.ssa_base = self.bp + local_num;
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num;
        self.stack.extend(args);
        self.stack.resize(new_len, Value::nil());
    }

    fn pop_frame(&mut self) {
        let old_bp = self.bp;
        if old_bp == 0 {
            return;
        }
        self.args_len = self.stack[old_bp - 3].get() as usize;
        self.ssa_base = self.stack[old_bp - 2].get() as usize;
        self.bp = self.stack[old_bp - 1].get() as usize;
        self.stack.truncate(old_bp - 3);
    }
}

enum JitState {
    Fail,
    Success(DestLabel, Type),
}

macro_rules! value_op {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match ($lhs.unpack(), $rhs.unpack()) {
            (RV::Integer($lhs), RV::Integer($rhs)) => $lhs.$op(&$rhs),
            (RV::Integer($lhs), RV::Float($rhs)) => ($lhs as f64).$op(&$rhs),
            (RV::Float($lhs), RV::Integer($rhs)) => $lhs.$op(&($rhs as f64)),
            (RV::Float($lhs), RV::Float($rhs)) => $lhs.$op(&$rhs),
            _ => unreachable!(),
        }
    }};
}

macro_rules! value_eq {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match ($lhs.unpack(), $rhs.unpack()) {
            (RV::Integer($lhs), RV::Integer($rhs)) => $lhs.$op(&$rhs),
            (RV::Integer($lhs), RV::Float($rhs)) => ($lhs as f64).$op(&$rhs),
            (RV::Float($lhs), RV::Integer($rhs)) => $lhs.$op(&($rhs as f64)),
            (RV::Float($lhs), RV::Float($rhs)) => $lhs.$op(&$rhs),
            (RV::Bool($lhs), RV::Bool($rhs)) => $lhs.$op(&$rhs),
            _ => unreachable!(),
        }
    }};
}

impl std::ops::Index<SsaReg> for Evaluator {
    type Output = Value;
    fn index(&self, index: SsaReg) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<SsaReg> for Evaluator {
    fn index_mut(&mut self, index: SsaReg) -> &mut Value {
        &mut self.call_stack[index]
    }
}

impl Evaluator {
    fn new() -> Self {
        Self {
            codegen: Codegen::new(),
            mir_context: MirContext::new(),
            mcir_context: McIrContext::new(),
            jit_state: HashMap::default(),
            call_stack: Stack::new(),
        }
    }

    fn eval_operand(&self, op: &HirOperand) -> Value {
        match op {
            HirOperand::Const(c) => *c,
            HirOperand::Reg(r) => self[*r],
        }
    }

    fn jit_compile(&mut self, hir_func: &HirFunction) -> Option<(usize, DestLabel, Type)> {
        //return None;
        let args = hir_func
            .args
            .iter()
            .zip(self.call_stack.args())
            .map(|(name, val)| (name.clone(), val.ty()))
            .collect();
        let mir_id = match self
            .mir_context
            .new_func_from_hir(hir_func.name.clone(), args, hir_func)
        {
            Ok(id) => id,
            Err(err) => {
                dbg!(err);
                self.mir_context.functions.pop().unwrap();
                return None;
            }
        };
        //dbg!(&self.mir_context);
        let func_id = self.mcir_context.from_mir(&self.mir_context[mir_id]);
        Some(
            self.codegen
                .compile_func(&self.mcir_context.functions[func_id], func_id),
        )
    }

    pub fn eval_toplevel(hir_context: &HirContext) -> RV {
        let mut eval = Self::new();
        let hir_func = &hir_context[HirFuncId::default()];
        let args: Vec<Value> = vec![];
        eval.call_stack.push_frame(&args, hir_func);
        let res = eval.eval_function(hir_context, HirFuncId::default());
        res.unpack()
    }

    fn eval_function(&mut self, hir_context: &HirContext, cur_fn: HirFuncId) -> Value {
        let hir_func = &hir_context[cur_fn];
        let arg_ty: Vec<_> = self.call_stack.args().iter().map(|v| v.ty()).collect();
        match self.jit_state.get(&(cur_fn, arg_ty.clone())) {
            None => {
                if let Some((func_id, dest, ty)) = self.jit_compile(hir_func) {
                    let func = &self.mir_context.functions[func_id];
                    eprintln!(
                        "JIT success: {} ({:?})->{:?}",
                        func.name, &arg_ty, func.ret_ty
                    );
                    self.jit_state
                        .insert((cur_fn, arg_ty), JitState::Success(dest, ty));
                    eprintln!("call JIT");
                    let res = self.codegen.call_jit_func(dest, ty, self.call_stack.args());
                    self.call_stack.pop_frame();
                    return res;
                } else {
                    eprintln!("JIT fail: {} ({:?})", hir_func.name, &arg_ty);
                    self.jit_state.insert((cur_fn, arg_ty), JitState::Fail);
                }
            }
            Some(JitState::Fail) => {}
            Some(JitState::Success(dest, ty)) => {
                eprintln!("call JIT");
                let res = self
                    .codegen
                    .call_jit_func(*dest, *ty, self.call_stack.args());
                self.call_stack.pop_frame();
                return res;
            }
        }

        let mut eval = FuncContext {
            cur_bb: HirBBId::default(),
            prev_bb: HirBBId::default(),
            pc: 0,
        };
        loop {
            let bb = &hir_func[eval.cur_bb];
            let op = &bb.insts[eval.pc];
            eval.pc += 1;
            if let Some(val) = self.eval(&mut eval, hir_context, op) {
                self.call_stack.pop_frame();
                return val;
            }
        }
    }

    fn eval(
        &mut self,
        ctx: &mut FuncContext,
        hir_context: &HirContext,
        hir: &Hir,
    ) -> Option<Value> {
        match hir {
            Hir::Integer(ret, i) => {
                self[*ret] = Value::integer(*i);
            }
            Hir::Float(ret, f) => {
                self[*ret] = Value::float(*f);
            }
            Hir::Nil(ret) => {
                self[*ret] = Value::nil();
            }
            Hir::Bool(ret, b) => {
                self[*ret] = Value::bool(*b);
            }
            Hir::Neg(op) => {
                let src = self.eval_operand(&op.src);
                self[op.ret] = match src.unpack() {
                    RV::Integer(i) => Value::integer(-i),
                    RV::Float(f) => Value::float(-f),
                    _ => unreachable!(),
                };
            }
            Hir::Add(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Sub(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Mul(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Div(op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Cmp(kind, op) => {
                let lhs = self.eval_operand(&op.lhs);
                let rhs = self.eval_operand(&op.rhs);
                self[op.ret] = Value::bool(match kind {
                    CmpKind::Eq => value_eq!(lhs, rhs, eq),
                    CmpKind::Ne => value_eq!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                });
            }
            Hir::CmpBr(kind, lhs, rhs, then_, else_) => {
                let lhs = self[*lhs];
                let rhs = self.eval_operand(rhs);
                let b = match kind {
                    CmpKind::Eq => value_eq!(lhs, rhs, eq),
                    CmpKind::Ne => value_eq!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                };
                let next_bb = if b { then_ } else { else_ };
                ctx.goto(*next_bb);
            }
            Hir::Ret(lhs) => return Some(self.eval_operand(lhs)),
            Hir::LocalStore(ret, ident, rhs) => {
                let v = self.eval_operand(rhs);
                if let Some(ret) = ret {
                    self[*ret] = v;
                }
                self.call_stack.set_local(*ident, v);
            }
            Hir::LocalLoad(ident, lhs) => {
                self[*lhs] = self.call_stack.get_local(*ident);
            }
            Hir::Call(id, ret, args) => {
                let args: Vec<_> = args.iter().map(|op| self.eval_operand(op)).collect();
                let hir_func = &hir_context[*id];
                self.call_stack.push_frame(&args, hir_func);
                let res = self.eval_function(hir_context, *id);
                if let Some(ret) = *ret {
                    self[ret] = res;
                }
            }
            Hir::Br(next_bb) => {
                ctx.goto(*next_bb);
            }
            Hir::CondBr(cond_, then_, else_) => {
                let next_bb = if self[*cond_] == Value::bool(false) {
                    else_
                } else {
                    then_
                };
                ctx.goto(*next_bb);
            }
            Hir::Phi(ret, phi) => {
                let reg = phi.iter().find(|(bb, _)| ctx.prev_bb == *bb).unwrap().1;
                self[*ret] = self[reg];
            }
        }
        None
    }
}

struct FuncContext {
    //ssareg: Vec<Value>,
    //locals: Vec<Value>,
    cur_bb: HirBBId,
    prev_bb: HirBBId,
    pc: usize,
}

/*impl std::ops::Index<SsaReg> for FuncContext {
    type Output = Value;

    fn index(&self, i: SsaReg) -> &Value {
        &self.ssareg[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for FuncContext {
    fn index_mut(&mut self, i: SsaReg) -> &mut Value {
        &mut self.ssareg[i.to_usize()]
    }
}*/

impl FuncContext {
    fn goto(&mut self, bb: HirBBId) {
        self.prev_bb = self.cur_bb;
        self.cur_bb = bb;
        self.pc = 0;
    }
}
