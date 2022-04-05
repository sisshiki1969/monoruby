use monoasm::DestLabel;

use super::hir::SsaReg;
use super::*;

pub struct Evaluator {
    codegen: Codegen,
    mir_context: MirContext,
    mcir_context: McIrContext,
    jit_state: HashMap<(HirFuncId, Vec<Type>), JitState>,
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

impl Evaluator {
    fn new() -> Self {
        Self {
            codegen: Codegen::new(),
            mir_context: MirContext::new(),
            mcir_context: McIrContext::new(),
            jit_state: HashMap::default(),
        }
    }

    fn jit_compile(
        &mut self,
        hir_context: &HirContext,
        hir_id: HirFuncId,
        args: &[Value],
    ) -> Option<(usize, DestLabel, Type)> {
        return None;
        let hir_func = &hir_context[hir_id];
        let args = hir_func
            .args
            .iter()
            .zip(args.iter())
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
        dbg!(&self.mir_context);
        let func_id = self.mcir_context.from_mir(&self.mir_context[mir_id]);
        Some(
            self.codegen
                .compile_func(&self.mcir_context.functions[func_id], func_id),
        )
    }

    pub fn eval_toplevel(hir_context: &HirContext) -> RV {
        let mut eval = Self::new();
        let res = eval.eval_function(hir_context, HirFuncId::default(), vec![]);
        res.unpack()
    }

    fn eval_function(
        &mut self,
        hir_context: &HirContext,
        cur_fn: HirFuncId,
        args: Vec<Value>,
    ) -> Value {
        let arg_ty: Vec<_> = args.iter().map(|v| v.ty()).collect();
        match self.jit_state.get(&(cur_fn, arg_ty.clone())) {
            None => {
                if let Some((func_id, dest, ty)) = self.jit_compile(hir_context, cur_fn, &args) {
                    let func = &self.mir_context.functions[func_id];
                    eprintln!(
                        "JIT success: {} ({:?})->{:?}",
                        func.name, &arg_ty, func.ret_ty
                    );
                    self.jit_state
                        .insert((cur_fn, arg_ty), JitState::Success(dest, ty));
                    eprintln!("call JIT");
                    return self.codegen.call_jit_func(dest, ty, &args);
                } else {
                    let func = &hir_context[cur_fn];
                    eprintln!("JIT fail: {} ({:?})", func.name, &arg_ty);
                    self.jit_state.insert((cur_fn, arg_ty), JitState::Fail);
                }
            }
            Some(JitState::Fail) => {}
            Some(JitState::Success(dest, ty)) => {
                eprintln!("call JIT");
                return self.codegen.call_jit_func(*dest, *ty, &args);
            }
        }

        let func = &hir_context[cur_fn];
        let locals_num = func.locals.len();
        let mut locals = vec![Value::nil(); locals_num];
        locals[0..args.len()].clone_from_slice(&args);
        let register_num = func.register_num();
        let mut eval = FuncContext {
            ssareg: vec![Value::nil(); register_num],
            locals,
            cur_bb: HirBBId::default(),
            prev_bb: HirBBId::default(),
            pc: 0,
        };
        loop {
            let bb = &func[eval.cur_bb];
            let op = &bb.insts[eval.pc];
            eval.pc += 1;
            if let Some(val) = self.eval(&mut eval, hir_context, op) {
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
                ctx[*ret] = Value::integer(*i);
            }
            Hir::Float(ret, f) => {
                ctx[*ret] = Value::float(*f);
            }
            Hir::Nil(ret) => {
                ctx[*ret] = Value::nil();
            }
            Hir::Neg(op) => {
                let src = ctx.eval_operand(&op.src);
                ctx[op.ret] = match src.unpack() {
                    RV::Integer(i) => Value::integer(-i),
                    RV::Float(f) => Value::float(-f),
                    _ => unreachable!(),
                };
            }
            Hir::Add(op) => {
                let lhs = ctx.eval_operand(&op.lhs);
                let rhs = ctx.eval_operand(&op.rhs);
                ctx[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Sub(op) => {
                let lhs = ctx.eval_operand(&op.lhs);
                let rhs = ctx.eval_operand(&op.rhs);
                ctx[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Mul(op) => {
                let lhs = ctx.eval_operand(&op.lhs);
                let rhs = ctx.eval_operand(&op.rhs);
                ctx[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Div(op) => {
                let lhs = ctx.eval_operand(&op.lhs);
                let rhs = ctx.eval_operand(&op.rhs);
                ctx[op.ret] = match (lhs.unpack(), rhs.unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                    _ => unreachable!(),
                };
            }
            Hir::Cmp(kind, op) => {
                let lhs = ctx.eval_operand(&op.lhs);
                let rhs = ctx.eval_operand(&op.rhs);
                ctx[op.ret] = Value::bool(match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                });
            }
            Hir::CmpBr(kind, lhs, rhs, then_, else_) => {
                let lhs = ctx[*lhs].clone();
                let rhs = ctx.eval_operand(rhs);
                let b = match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                };
                let next_bb = if b { then_ } else { else_ };
                ctx.goto(*next_bb);
            }
            Hir::Ret(lhs) => return Some(ctx.eval_operand(lhs)),
            Hir::LocalStore(ret, ident, rhs) => {
                let v = ctx.eval_operand(rhs);
                if let Some(ret) = ret {
                    ctx[*ret] = v.clone();
                }
                ctx.locals[*ident] = v;
            }
            Hir::LocalLoad(ident, lhs) => {
                ctx[*lhs] = ctx.locals[*ident].clone();
            }
            Hir::Call(id, ret, args) => {
                let args = args
                    .iter()
                    .map(|op| ctx.eval_operand(op))
                    .collect::<Vec<Value>>();
                let res = self.eval_function(hir_context, *id, args);
                if let Some(ret) = *ret {
                    ctx[ret] = res;
                }
            }
            Hir::Br(next_bb) => {
                ctx.goto(*next_bb);
            }
            Hir::CondBr(cond_, then_, else_) => {
                let next_bb = if ctx[*cond_] == Value::bool(false) {
                    else_
                } else {
                    then_
                };
                ctx.goto(*next_bb);
            }
            Hir::Phi(ret, phi) => {
                let reg = phi.iter().find(|(bb, _)| ctx.prev_bb == *bb).unwrap().1;
                ctx[*ret] = ctx[reg].clone();
            }
        }
        None
    }
}

struct FuncContext {
    ssareg: Vec<Value>,
    locals: Vec<Value>,
    cur_bb: HirBBId,
    prev_bb: HirBBId,
    pc: usize,
}

impl std::ops::Index<SsaReg> for FuncContext {
    type Output = Value;

    fn index(&self, i: SsaReg) -> &Value {
        &self.ssareg[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for FuncContext {
    fn index_mut(&mut self, i: SsaReg) -> &mut Value {
        &mut self.ssareg[i.to_usize()]
    }
}

impl FuncContext {
    fn goto(&mut self, bb: HirBBId) {
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
}
