use super::*;
use num::BigInt;
use paste::paste;
use ruruby_parse::{ArgList, BinOp, BlockInfo, CmpKind, Loc, Node, NodeKind, UnOp};

///
/// ID of register.
///
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum BcReg {
    Self_,
    Local(BcLocal),
    Temp(BcTemp),
}

impl std::fmt::Debug for BcReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Self_ => write!(f, "S"),
            Self::Local(local) => write!(f, "{:?}", local),
            Self::Temp(temp) => write!(f, "{:?}", temp),
        }
    }
}

impl std::convert::From<BcLocal> for BcReg {
    fn from(local: BcLocal) -> Self {
        BcReg::Local(local)
    }
}

impl std::convert::From<BcTemp> for BcReg {
    fn from(temp: BcTemp) -> Self {
        BcReg::Temp(temp)
    }
}

///
/// ID of temporary register.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct BcTemp(pub u16);

impl std::fmt::Debug for BcTemp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl std::ops::AddAssign<usize> for BcTemp {
    fn add_assign(&mut self, rhs: usize) {
        self.0 = self.0 + rhs as u16;
    }
}

///
/// ID of local variable.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct BcLocal(pub u16);

impl std::fmt::Debug for BcLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum LoopKind {
    For,
    While,
}

#[derive(Debug, Clone)]
pub(crate) struct IrContext {
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// loop information.
    loops: Vec<(LoopKind, usize, Option<BcReg>)>, // (kind, label for exit, return register)
}

impl IrContext {
    fn new() -> Self {
        Self {
            ir: vec![],
            labels: vec![],
            loops: vec![],
        }
    }

    fn push(&mut self, op: BcIr, loc: Loc) {
        self.ir.push((op, loc));
    }

    /// get new destination label.
    fn new_label(&mut self) -> usize {
        let label = self.labels.len();
        self.labels.push(None);
        label
    }

    /// apply current instruction pointer to the destination label.
    fn apply_label(&mut self, label: usize) {
        let pos = InstId(self.ir.len() as u32);
        self.labels[label] = Some(pos);
    }

    fn gen_br(&mut self, cond_pos: usize) {
        self.push(BcIr::Br(cond_pos), Loc::default());
    }

    fn gen_condbr(&mut self, cond: BcReg, then_pos: usize, optimizable: bool) {
        self.push(
            BcIr::CondBr(cond, then_pos, optimizable, BrKind::BrIf),
            Loc::default(),
        );
    }

    fn gen_condnotbr(&mut self, cond: BcReg, else_pos: usize, optimizable: bool) {
        self.push(
            BcIr::CondBr(cond, else_pos, optimizable, BrKind::BrIfNot),
            Loc::default(),
        );
    }

    fn gen_opt_condbr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        jmp_if_true: bool,
        cond: Node,
        else_pos: usize,
    ) -> Result<()> {
        if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
            let loc = cond.loc;
            let cond = info.next_reg().into();
            self.gen_cmp(ctx, info, None, kind, lhs, rhs, true, loc)?;
            info.pop();
            if jmp_if_true {
                self.gen_condbr(cond, else_pos, true);
            } else {
                self.gen_condnotbr(cond, else_pos, true);
            }
        } else if let NodeKind::BinOp(BinOp::LAnd, box lhs, box rhs) = cond.kind {
            if jmp_if_true {
                self.gen_opt_lor_condbr(ctx, info, jmp_if_true, lhs, rhs, else_pos)?;
            } else {
                self.gen_opt_land_condbr(ctx, info, jmp_if_true, lhs, rhs, else_pos)?;
            }
        } else if let NodeKind::BinOp(BinOp::LOr, box lhs, box rhs) = cond.kind {
            if jmp_if_true {
                self.gen_opt_land_condbr(ctx, info, jmp_if_true, lhs, rhs, else_pos)?;
            } else {
                self.gen_opt_lor_condbr(ctx, info, jmp_if_true, lhs, rhs, else_pos)?;
            }
        } else {
            let cond = self.gen_temp_expr(ctx, info, cond)?;
            if jmp_if_true {
                self.gen_condbr(cond, else_pos, false);
            } else {
                self.gen_condnotbr(cond, else_pos, false);
            }
        }
        Ok(())
    }

    fn gen_opt_land_condbr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        jmp_if_true: bool,
        lhs: Node,
        rhs: Node,
        else_pos: usize,
    ) -> Result<()> {
        self.gen_opt_condbr(ctx, info, jmp_if_true, lhs, else_pos)?;
        self.gen_opt_condbr(ctx, info, jmp_if_true, rhs, else_pos)
    }

    fn gen_opt_lor_condbr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        jmp_if_true: bool,
        lhs: Node,
        rhs: Node,
        else_pos: usize,
    ) -> Result<()> {
        let cont_pos = self.new_label();
        self.gen_opt_condbr(ctx, info, !jmp_if_true, lhs, cont_pos)?;
        self.gen_opt_condbr(ctx, info, jmp_if_true, rhs, else_pos)?;
        self.apply_label(cont_pos);
        Ok(())
    }
}

pub(crate) fn is_smi(node: &Node) -> Option<i16> {
    if let NodeKind::Integer(i) = &node.kind {
        if *i == *i as i16 as i64 {
            return Some(*i as i16);
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq)]
enum LvalueKind {
    Const(IdentId),
    InstanceVar(IdentId),
    DynamicVar { outer: usize, dst: BcReg },
    Index { base: BcReg, index: BcReg },
    Send { recv: BcReg, method: IdentId },
    Other,
}

impl IrContext {
    pub(crate) fn compile(info: &mut ISeqInfo, ctx: &mut FnStore) -> Result<IrContext> {
        let mut ir = IrContext::new();
        let ast = std::mem::take(&mut info.ast).unwrap();
        ir.gen_expr(ctx, info, ast, true, true)?;
        assert_eq!(0, info.temp);
        Ok(ir)
    }

    fn gen_load_const(
        &mut self,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        toplevel: bool,
        name: String,
        prefix: Vec<String>,
        loc: Loc,
    ) {
        let name = IdentId::get_ident_id_from_string(name);
        let prefix = prefix
            .into_iter()
            .map(IdentId::get_ident_id_from_string)
            .collect();
        let reg = match dst {
            Some(local) => local,
            None => info.push().into(),
        };
        self.push(BcIr::LoadConst(reg, toplevel, prefix, name), loc);
    }

    fn gen_load_ivar(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let reg = match dst {
            Some(local) => local,
            None => info.push().into(),
        };
        self.push(BcIr::LoadIvar(reg, name), loc);
    }

    fn gen_literal(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, v: Value) {
        let reg = match dst {
            Some(local) => local,
            None => info.push().into(),
        };
        self.push(BcIr::Literal(reg, v), Loc::default());
    }

    fn gen_integer(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local,
                None => info.push().into(),
            };
            self.push(BcIr::Integer(reg, i), Loc::default());
        } else {
            self.gen_literal(info, dst, Value::new_integer(i));
        }
    }

    fn gen_float(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, f: f64) {
        self.gen_literal(info, dst, Value::new_float(f));
    }

    fn gen_symbol(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, sym: IdentId) {
        let reg = match dst {
            Some(local) => local,
            None => info.push().into(),
        };
        self.push(BcIr::Symbol(reg, sym), Loc::default());
    }

    fn gen_string(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, s: String) {
        self.gen_literal(info, dst, Value::new_string(s));
    }

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, loc: Loc) {
        self.push(BcIr::Array(ret, src, len as u16), loc);
    }

    fn gen_array(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        ret: Option<BcReg>,
        nodes: Vec<Node>,
        loc: Loc,
    ) -> Result<()> {
        let len = nodes.len();
        let src = self.gen_args(ctx, info, nodes)?.into();
        info.popn(len);
        let ret = match ret {
            Some(local) => local,
            None => info.push().into(),
        };
        self.emit_array(ret, src, len, loc);
        Ok(())
    }

    fn gen_index(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        ret: Option<BcReg>,
        base: Node,
        index: Node,
        loc: Loc,
    ) -> Result<()> {
        let (base, idx) = self.gen_binary_temp_expr(ctx, info, base, index)?;
        let ret = match ret {
            None => info.push().into(),
            Some(local) => local,
        };
        self.push(BcIr::Index(ret, base, idx), loc);
        Ok(())
    }

    ///
    /// Evaluaate *lhs* as a lvalue.
    ///
    fn eval_lvalue(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        lhs: &Node,
    ) -> Result<LvalueKind> {
        let lhs = match &lhs.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } if !toplevel && parent.is_none() && prefix.is_empty() => {
                let name = IdentId::get_ident_id(name);
                LvalueKind::Const(name)
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_ident_id(name);
                LvalueKind::InstanceVar(name)
            }
            NodeKind::DynamicLocalVar(outer, ident) => {
                let outer = *outer;
                let dst = BcLocal(info.refer_dynamic_local(outer, ident)).into();
                LvalueKind::DynamicVar { outer, dst }
            }
            NodeKind::Index { box base, index } => {
                assert_eq!(1, index.len());
                let index = index[0].clone();
                let base = self.gen_expr_reg(ctx, info, base.clone())?;
                let index = self.gen_expr_reg(ctx, info, index)?;
                LvalueKind::Index { base, index }
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav,
            } if arglist.args.is_empty()
                && arglist.block.is_none()
                && arglist.kw_args.is_empty()
                && !safe_nav =>
            {
                let recv = self.gen_expr_reg(ctx, info, receiver.clone())?;
                let method = IdentId::get_ident_id(&format!("{}=", method));
                LvalueKind::Send { recv, method }
            }
            NodeKind::LocalVar(_) => LvalueKind::Other,
            _ => return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone())),
        };
        Ok(lhs)
    }

    fn gen_assign(&mut self, src: BcReg, lhs: LvalueKind, loc: Loc) {
        match lhs {
            LvalueKind::Const(name) => {
                self.push(BcIr::StoreConst(src, name), loc);
            }
            LvalueKind::InstanceVar(name) => {
                self.push(BcIr::StoreIvar(src, name), loc);
            }
            LvalueKind::DynamicVar { outer, dst } => {
                self.push(BcIr::StoreDynVar { dst, outer, src }, loc);
            }
            LvalueKind::Index { base, index } => {
                self.push(BcIr::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Send { recv, method } => {
                self.gen_method_assign(method, recv, src, loc);
            }
            LvalueKind::Other => unreachable!(),
        }
    }

    fn gen_bigint(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, bigint: BigInt) {
        self.gen_literal(info, dst, Value::new_bigint(bigint));
    }

    fn gen_nil(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>) {
        let reg = match dst {
            Some(dst) => dst,
            None => info.push().into(),
        };
        self.push(BcIr::Nil(reg), Loc::default());
    }

    fn gen_neg(&mut self, info: &mut ISeqInfo, local: Option<BcReg>, loc: Loc) {
        match local {
            Some(local) => {
                self.push(BcIr::Neg(local, local), loc);
            }
            None => {
                let src = info.pop().into();
                let dst = info.push().into();
                self.push(BcIr::Neg(dst, src), loc);
            }
        };
    }

    fn gen_ret(&mut self, info: &mut ISeqInfo, local: Option<BcReg>) {
        let ret = match local {
            Some(ret) => ret,
            None => info.pop().into(),
        };
        assert_eq!(0, info.temp);
        self.push(BcIr::Ret(ret), Loc::default());
    }

    fn gen_mov(&mut self, dst: BcReg, src: BcReg) {
        if dst != src {
            self.push(BcIr::Mov(dst, src), Loc::default());
        }
    }

    fn gen_temp_mov(&mut self, info: &mut ISeqInfo, rhs: BcReg) {
        let lhs = info.push();
        self.gen_mov(lhs.into(), rhs);
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        mut nodes: Vec<Node>,
        ret: Option<BcReg>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(ctx, info, node, false, false)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, info, ret, last)?;
                if is_ret {
                    self.gen_ret(info, ret.into());
                } else if use_value {
                    self.gen_temp_mov(info, ret);
                }
            }
            None => {
                self.gen_expr(ctx, info, last, use_value, is_ret)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_expr_reg(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        expr: Node,
    ) -> Result<BcReg> {
        Ok(match info.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => self.push_expr(ctx, info, expr)?,
        })
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        expr: Node,
    ) -> Result<BcReg> {
        Ok(match info.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => {
                self.push_expr(ctx, info, expr)?;
                info.pop().into()
            }
        })
    }

    fn gen_binary_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        match (info.is_refer_local(&lhs), info.is_refer_local(&rhs)) {
            (None, None) => {
                let lhs = self.push_expr(ctx, info, lhs)?;
                let rhs = self.push_expr(ctx, info, rhs)?;
                info.temp -= 2;
                Ok((lhs, rhs))
            }
            _ => {
                let lhs = self.gen_temp_expr(ctx, info, lhs)?;
                let rhs = self.gen_temp_expr(ctx, info, rhs)?;
                Ok((lhs, rhs))
            }
        }
    }

    /// Generate bytecode Ir for binary operations.
    fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<BcReg> {
        match op {
            BinOp::Add => self.gen_add(ctx, info, dst, lhs, rhs, loc),
            BinOp::Sub => self.gen_sub(ctx, info, dst, lhs, rhs, loc),
            BinOp::Mul => self.gen_mul(ctx, info, dst, lhs, rhs, loc),
            BinOp::Div => self.gen_div(ctx, info, dst, lhs, rhs, loc),
            BinOp::Rem => self.gen_rem(ctx, info, dst, lhs, rhs, loc),
            BinOp::Exp => self.gen_exp(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitOr => self.gen_bitor(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitAnd => self.gen_bitand(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitXor => self.gen_bitxor(ctx, info, dst, lhs, rhs, loc),
            BinOp::Shr => self.gen_shr(ctx, info, dst, lhs, rhs, loc),
            BinOp::Shl => self.gen_shl(ctx, info, dst, lhs, rhs, loc),
            BinOp::LAnd => self.gen_land(ctx, info, dst, lhs, rhs),
            BinOp::LOr => self.gen_lor(ctx, info, dst, lhs, rhs),
            BinOp::Cmp(kind) => self.gen_cmp(ctx, info, dst, kind, lhs, rhs, false, loc),
            _ => Err(MonorubyErr::unsupported_operator(
                op,
                loc,
                info.sourceinfo.clone(),
            )),
        }
    }

    fn push_expr(&mut self, ctx: &mut FnStore, info: &mut ISeqInfo, expr: Node) -> Result<BcReg> {
        let ret = info.next_reg().into();
        self.gen_expr(ctx, info, expr, true, false)?;
        Ok(ret)
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        expr: Node,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        if !use_value {
            match &expr.kind {
                NodeKind::Nil
                | NodeKind::Bool(_)
                | NodeKind::SelfValue
                | NodeKind::Integer(_)
                | NodeKind::Symbol(_)
                | NodeKind::Bignum(_)
                | NodeKind::Float(_)
                | NodeKind::String(_) => return Ok(()),
                _ => {}
            }
        }
        let loc = expr.loc;
        match expr.kind {
            NodeKind::Nil => self.gen_nil(info, None),
            NodeKind::Bool(b) => self.gen_literal(info, None, Value::bool(b)),
            NodeKind::SelfValue => self.gen_temp_mov(info, BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(info, None, i);
            }
            NodeKind::Symbol(sym) => {
                let sym = IdentId::get_ident_id_from_string(sym);
                self.gen_symbol(info, None, sym);
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(info, None, bigint),
            NodeKind::Float(f) => self.gen_float(info, None, f),
            NodeKind::String(s) => self.gen_string(info, None, s),
            NodeKind::Array(nodes, _) => self.gen_array(ctx, info, None, nodes, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                assert_eq!(1, index.len());
                self.gen_index(ctx, info, None, base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                    NodeKind::Float(f) => self.gen_float(info, None, -f),
                    _ => {
                        self.push_expr(ctx, info, rhs)?;
                        self.gen_neg(info, None, loc);
                    }
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                if let Some(local) = info.is_refer_local(&lhs) {
                    self.gen_binop(ctx, info, op, lhs, rhs, Some(local.into()), loc)?;
                    if is_ret {
                        self.gen_ret(info, Some(local.into()));
                    } else if use_value {
                        self.gen_temp_mov(info, local.into());
                    }
                    return Ok(());
                }
                let lhs_loc = lhs.loc;
                let temp = info.temp;
                // First, evaluate lvalue.
                let lhs_kind = self.eval_lvalue(ctx, info, &lhs)?;
                // Evaluate rvalue.
                let src = self.gen_binop(ctx, info, op, lhs, rhs, None, loc)?;
                // Assign rvalue to lvalue.
                self.gen_assign(src, lhs_kind, lhs_loc);
                info.temp = temp;
                let res = info.push().into();
                if use_value {
                    self.gen_mov(res, src);
                }
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, op, lhs, rhs, None, loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(local) = info.is_assign_local(&lhs) {
                        self.gen_store_expr(ctx, info, local.into(), rhs)?;
                        if is_ret {
                            self.gen_ret(info, Some(local.into()));
                        } else if use_value {
                            self.gen_temp_mov(info, local.into());
                        }
                        return Ok(());
                    }
                    let temp = info.temp;
                    let lhs = self.eval_lvalue(ctx, info, &lhs)?;
                    let src = self.gen_expr_reg(ctx, info, rhs)?;
                    self.gen_assign(src, lhs, loc);
                    info.temp = temp;
                    let res = info.push().into();
                    if use_value {
                        self.gen_mov(res, src);
                    }
                } else {
                    return self.gen_mul_assign(ctx, info, mlhs, mrhs, use_value, is_ret);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local = info.refer_local(&ident);
                if is_ret {
                    self.gen_ret(info, Some(local.into()));
                } else if use_value {
                    self.gen_temp_mov(info, local.into());
                }
                return Ok(());
            }
            NodeKind::DynamicLocalVar(outer, ident) => {
                let ret = info.push().into();
                let src = BcLocal(info.refer_dynamic_local(outer, &ident)).into();
                self.push(BcIr::LoadDynVar { ret, src, outer }, loc);
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.gen_load_const(info, None, toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_ident_id_from_string(name);
                self.gen_load_ivar(info, None, name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_method_call(
                    ctx,
                    info,
                    method,
                    Some(receiver),
                    arglist,
                    ret,
                    is_ret,
                    loc,
                );
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_method_call(ctx, info, method, None, arglist, ret, is_ret, loc);
            }
            NodeKind::Yield(arglist) => {
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_yield(ctx, info, arglist, ret, is_ret, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_method_call(ctx, info, method, None, arglist, ret, is_ret, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_opt_condbr(ctx, info, false, cond, else_pos)?;
                self.gen_expr(ctx, info, then_, use_value, is_ret)?;
                if !is_ret {
                    self.gen_br(succ_pos);
                    if use_value {
                        info.pop();
                    }
                }
                self.apply_label(else_pos);
                self.gen_expr(ctx, info, else_, use_value, is_ret)?;
                self.apply_label(succ_pos);
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
                postfix,
            } => {
                if postfix && matches!(body.kind, NodeKind::Begin { .. }) {
                    self.gen_while_begin_postfix(ctx, info, cond_op, cond, body, use_value)?;
                } else {
                    self.gen_while(ctx, info, cond_op, cond, body, use_value)?;
                }
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::For {
                param,
                box iter,
                body,
            } => {
                self.gen_for(ctx, info, param, iter, body, use_value)?;
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::Break(box val) => {
                let (_kind, break_pos, ret_reg) = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        return Err(MonorubyErr::escape_from_eval(loc, info.sourceinfo.clone()))
                    }
                };
                if let Some(reg) = ret_reg {
                    let temp = self.gen_temp_expr(ctx, info, val)?;
                    self.gen_mov(reg, temp)
                }
                self.push(BcIr::Br(break_pos), loc);
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = info.is_refer_local(&expr) {
                    self.gen_ret(info, Some(local.into()));
                } else {
                    self.gen_expr(ctx, info, expr, true, true)?;
                }
                if use_value && !is_ret {
                    unreachable!();
                }
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, info, nodes, None, use_value, is_ret)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_: None,
                ensure: None,
            } => {
                assert!(rescue.is_empty());
                self.gen_expr(ctx, info, body, use_value, is_ret)?;
                return Ok(());
            }
            NodeKind::MethodDef(name, block) => {
                self.gen_method_def(ctx, info, name.clone(), block, loc)?;
                if use_value {
                    self.gen_symbol(info, None, IdentId::get_ident_id_from_string(name));
                }
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info: block_info,
                is_module,
            } => {
                assert!(base.is_none());
                assert!(!is_module);
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                let superclass = superclass.map(|c| *c);
                self.gen_class_def(ctx, info, name, superclass, *block_info.body, ret, loc)?;
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                let len = nodes.len();
                let arg = info.next_reg();
                for expr in nodes {
                    self.push_expr(ctx, info, expr)?;
                }
                info.temp -= len as u16;
                let ret = match use_value {
                    true => Some(info.push().into()),
                    false => None,
                };
                self.push(BcIr::ConcatStr(ret, arg, len), Loc::default());
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            _ => return Err(MonorubyErr::unsupported_node(expr, info.sourceinfo.clone())),
        }
        if is_ret {
            self.gen_ret(info, None);
        } else if !use_value {
            info.pop();
        }
        Ok(())
    }

    fn gen_store_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: BcReg,
        rhs: Node,
    ) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(info, Some(dst)),
            NodeKind::Bool(b) => self.gen_literal(info, Some(dst), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(dst, BcReg::Self_),
            NodeKind::Integer(i) => self.gen_integer(info, Some(dst), i),
            NodeKind::Symbol(sym) => {
                let sym = IdentId::get_ident_id_from_string(sym);
                self.gen_symbol(info, Some(dst), sym)
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(info, Some(dst), bigint),
            NodeKind::Float(f) => self.gen_float(info, Some(dst), f),
            NodeKind::String(s) => self.gen_string(info, Some(dst), s),
            NodeKind::Array(nodes, _) => self.gen_array(ctx, info, Some(dst), nodes, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                assert_eq!(1, index.len());
                self.gen_index(ctx, info, Some(dst), base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(info, Some(dst), -i),
                    NodeKind::Float(f) => self.gen_float(info, Some(dst), -f),
                    _ => {
                        self.gen_store_expr(ctx, info, dst, rhs)?;
                        self.gen_neg(info, Some(dst), loc);
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, op, lhs, rhs, Some(dst), loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(src) = info.is_assign_local(&lhs) {
                        self.gen_store_expr(ctx, info, src.into(), rhs)?;
                        self.gen_mov(dst, src.into());
                    } else {
                        let temp = info.temp;
                        let lhs = self.eval_lvalue(ctx, info, &lhs)?;
                        self.gen_store_expr(ctx, info, dst, rhs)?;
                        self.gen_assign(dst, lhs, loc);
                        info.temp = temp;
                    }
                } else {
                    self.gen_mul_assign(ctx, info, mlhs, mrhs, true, false)?;
                    let temp = info.pop().into();
                    self.gen_mov(dst, temp);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = info.refer_local(&ident);
                self.gen_mov(dst, local2.into());
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.gen_load_const(info, dst.into(), toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_ident_id_from_string(name);
                self.gen_load_ivar(info, dst.into(), name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(dst);
                self.gen_method_call(ctx, info, method, Some(receiver), arglist, ret, false, loc)?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(dst);
                self.gen_method_call(ctx, info, method, None, arglist, ret, false, loc)?;
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(ctx, info, nodes, Some(dst), false, false)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info: block_info,
                is_module,
            } => {
                assert!(base.is_none());
                assert!(!is_module);
                let ret = Some(dst);
                let superclass = superclass.map(|c| *c);
                self.gen_class_def(ctx, info, name, superclass, *block_info.body, ret, loc)?;
            }
            _ => {
                let ret = self.push_expr(ctx, info, rhs)?;
                self.gen_mov(dst, ret);
                info.pop();
            }
        };
        Ok(())
    }

    fn gen_method_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        name: String,
        block: BlockInfo,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_iseq(Some(name.clone()), None, block, info.sourceinfo.clone())?;
        let name = IdentId::get_ident_id_from_string(name);
        self.push(BcIr::MethodDef(name, func_id), loc);
        Ok(())
    }

    fn gen_class_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        name: String,
        superclass: Option<Node>,
        body: Node,
        ret: Option<BcReg>,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_classdef(Some(name.clone()), body, info.sourceinfo.clone());
        let name = IdentId::get_ident_id_from_string(name);
        let superclass = match superclass {
            Some(superclass) => Some(self.gen_temp_expr(ctx, info, superclass)?),
            None => None,
        };
        self.push(
            BcIr::ClassDef {
                ret,
                superclass,
                name,
                func_id,
            },
            loc,
        );
        Ok(())
    }

    fn gen_args(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = info.next_reg();
        for arg in args {
            self.push_expr(ctx, info, arg)?;
        }
        Ok(arg)
    }

    fn gen_call(
        &mut self,
        recv: BcReg,
        method: IdentId,
        ret: Option<BcReg>,
        arg: BcReg,
        len: usize,
        block: bool,
        loc: Loc,
    ) {
        if block {
            self.push(BcIr::MethodCallBlock(ret, method), loc)
        } else {
            self.push(BcIr::MethodCall(ret, method), loc)
        };
        self.push(BcIr::MethodArgs(recv, arg, len), loc);
        self.push(BcIr::InlineCache, loc);
    }
}
enum RecvKind {
    SelfValue,
    Local(BcReg),
    Temp,
}

impl IrContext {
    fn gen_method_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        method: String,
        receiver: Option<Node>,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let method = IdentId::get_ident_id_from_string(method);
        let recv_kind = match receiver {
            Some(receiver) => {
                if receiver.kind == NodeKind::SelfValue {
                    RecvKind::SelfValue
                } else if let Some(local) = info.is_refer_local(&receiver) {
                    RecvKind::Local(local.into())
                } else {
                    self.push_expr(ctx, info, receiver)?;
                    RecvKind::Temp
                }
            }
            None => RecvKind::SelfValue,
        };

        assert!(arglist.kw_args.is_empty());
        assert!(arglist.hash_splat.is_empty());
        assert!(!arglist.delegate);
        let mut has_block = false;
        let old_temp = info.temp;
        let arg = info.next_reg();
        if let Some(box block) = arglist.block {
            has_block = true;

            match block.kind {
                NodeKind::Lambda(block) => {
                    let outer_locals = info.get_locals();
                    let func_id = ctx.add_iseq(
                        None,
                        Some((info.id, outer_locals)),
                        block,
                        info.sourceinfo.clone(),
                    )?;
                    self.gen_literal(info, None, Value::new_integer(func_id.0 as i64));
                }
                _ => unimplemented!(),
            }
        }
        let args = arglist.args;
        let len = args.len();
        self.gen_args(ctx, info, args)?;
        info.temp = old_temp;

        let recv = match recv_kind {
            RecvKind::SelfValue => BcReg::Self_,
            RecvKind::Local(reg) => reg,
            RecvKind::Temp => info.pop().into(),
        };
        self.gen_call(recv, method, ret, arg.into(), len, has_block, loc);
        if is_ret {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn gen_yield(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        assert!(arglist.kw_args.is_empty());
        assert!(arglist.hash_splat.is_empty());
        assert!(!arglist.delegate);
        assert!(arglist.block.is_none());
        let old_temp = info.temp;
        let arg = info.next_reg();
        let args = arglist.args;
        let len = args.len();
        self.gen_args(ctx, info, args)?;
        info.temp = old_temp;

        self.push(
            BcIr::Yield {
                ret,
                args: arg.into(),
                len,
            },
            loc,
        );

        if is_ret {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn gen_method_assign(&mut self, method: IdentId, receiver: BcReg, val: BcReg, loc: Loc) {
        self.gen_call(receiver, method, None, val, 1, false, loc);
    }

    fn gen_binary(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = self.gen_binary_temp_expr(ctx, info, lhs, rhs)?;
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = self.gen_temp_expr(ctx, info, lhs)?;
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs))
    }

    fn gen_land(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<BcReg> {
        let exit_pos = self.new_label();
        let dst = match dst {
            None => info.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(ctx, info, dst, lhs)?;
        self.gen_condnotbr(dst, exit_pos, false);
        self.gen_store_expr(ctx, info, dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_lor(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<BcReg> {
        let exit_pos = self.new_label();
        let dst = match dst {
            None => info.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(ctx, info, dst, lhs)?;
        self.gen_condbr(dst, exit_pos, false);
        self.gen_store_expr(ctx, info, dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_cmp(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<BcReg> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, info, dst, lhs)?;
            self.push(BcIr::Cmpri(kind, dst, lhs, i, optimizable), loc);
            Ok(dst)
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
            self.push(BcIr::Cmp(kind, dst, lhs, rhs, optimizable), loc);
            Ok(dst)
        }
    }

    ///
    /// Generate multiple assignment.
    ///
    /// This func always use a new temporary register for rhs even if the number of rhs is 1.
    ///
    fn gen_mul_assign(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        mlhs: Vec<Node>,
        mrhs: Vec<Node>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        assert!(mlhs_len == mrhs_len);

        let temp = info.temp;
        // At first, we evaluate lvalues and save their info(LhsKind).
        let mut lhs_kind: Vec<LvalueKind> = vec![];
        for lhs in &mlhs {
            lhs_kind.push(self.eval_lvalue(ctx, info, lhs)?);
        }

        // Next, we evaluate rvalues and save them in temporory registers which start from temp_reg.
        let rhs_reg = info.next_reg();
        let mut temp_reg = rhs_reg;
        for rhs in mrhs {
            self.push_expr(ctx, info, rhs)?;
        }

        // Finally, assign rvalues to lvalue.
        for (lhs, kind) in mlhs.into_iter().zip(lhs_kind) {
            if let Some(local) = info.is_assign_local(&lhs) {
                assert_eq!(LvalueKind::Other, kind);
                self.gen_mov(local.into(), temp_reg.into());
            } else {
                let src = temp_reg.into();
                self.gen_assign(src, kind, loc);
            }
            temp_reg += 1;
        }
        info.temp = temp;

        // Generate return value if needed.
        if is_ret || use_value {
            let ret = info.push().into();
            self.emit_array(ret, rhs_reg.into(), mrhs_len, loc);
        }
        if is_ret {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn gen_for(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        param: Vec<String>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        assert_eq!(1, param.len());
        let counter = info.assign_local(&param[0]);
        let break_pos = self.new_label();
        self.loops.push((
            LoopKind::For,
            break_pos,
            match use_value {
                true => Some(info.next_reg().into()),
                false => None,
            },
        ));
        let loc = iter.loc;
        if let NodeKind::Range {
            box start,
            box end,
            exclude_end,
            ..
        } = iter.kind
        {
            let loop_entry = self.new_label();
            let loop_exit = self.new_label();
            self.gen_store_expr(ctx, info, counter.into(), start)?;
            let end = self.push_expr(ctx, info, end)?;

            self.apply_label(loop_entry);
            self.push(BcIr::LoopStart, loc);
            let dst = info.push().into();
            self.push(
                BcIr::Cmp(
                    if exclude_end {
                        CmpKind::Ge
                    } else {
                        CmpKind::Gt
                    },
                    dst,
                    counter.into(),
                    end,
                    true,
                ),
                loc,
            );
            self.gen_condbr(dst, loop_exit, true);
            info.pop();

            self.gen_expr(ctx, info, *body.body, false, false)?;

            self.push(
                BcIr::BinOpRi(BinOpK::Add, counter.into(), counter.into(), 1),
                loc,
            );
            self.gen_br(loop_entry);

            self.apply_label(loop_exit);
            info.pop();
        } else {
            unimplemented!()
        }
        if use_value {
            // TODO: we must return iter object.
            self.gen_nil(info, None);
        }
        self.loops.pop().unwrap();
        self.apply_label(break_pos);
        self.push(BcIr::LoopEnd, loc);
        Ok(())
    }

    fn gen_while(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        cond_op: bool,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        let break_pos = self.new_label();
        self.loops.push((
            LoopKind::While,
            break_pos,
            match use_value {
                true => Some(info.next_reg().into()),
                false => None,
            },
        ));
        let loc = body.loc;
        self.apply_label(cond_pos);
        self.push(BcIr::LoopStart, loc);
        self.gen_opt_condbr(ctx, info, !cond_op, cond, succ_pos)?;
        self.gen_expr(ctx, info, body, false, false)?;
        self.gen_br(cond_pos);
        self.apply_label(succ_pos);

        if use_value {
            self.gen_nil(info, None);
        }
        self.loops.pop().unwrap();
        self.apply_label(break_pos);
        self.push(BcIr::LoopEnd, loc);

        Ok(())
    }

    fn gen_while_begin_postfix(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        cond_op: bool,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let loop_pos = self.new_label();
        let break_pos = self.new_label();
        self.loops.push((
            LoopKind::While,
            break_pos,
            match use_value {
                true => Some(info.next_reg().into()),
                false => None,
            },
        ));
        let loc = body.loc;
        self.apply_label(loop_pos);
        self.push(BcIr::LoopStart, loc);
        self.gen_expr(ctx, info, body, false, false)?;
        self.gen_opt_condbr(ctx, info, cond_op, cond, loop_pos)?;

        if use_value {
            self.gen_nil(info, None);
        }
        self.loops.pop().unwrap();
        self.apply_label(break_pos);
        self.push(BcIr::LoopEnd, loc);

        Ok(())
    }
}

macro_rules! gen_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                info: &mut ISeqInfo,
                dst: Option<BcReg>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<BcReg> {
                let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
                self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
                Ok(dst)
            }
        }
    };
    (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
        gen_ops!(($op1, $inst1));
        gen_ops!($(($op2, $inst2)),+);
    };
}

macro_rules! gen_ri_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                info: &mut ISeqInfo,
                dst: Option<BcReg>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<BcReg> {
                if let Some(i) = is_smi(&rhs) {
                    let (dst, lhs) = self.gen_singular(ctx, info, dst, lhs)?;
                    self.push(BcIr::BinOpRi(BinOpK::$inst, dst, lhs, i), loc);
                    Ok(dst)
                } else if let Some(i) = is_smi(&lhs) {
                    let (dst, rhs) = self.gen_singular(ctx, info, dst, rhs)?;
                    self.push(BcIr::BinOpIr(BinOpK::$inst, dst, i, rhs), loc);
                    Ok(dst)
                } else {
                    let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
                    self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
                    Ok(dst)
                }
            }
        }
    };
    (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
        gen_ri_ops!(($op1, $inst1));
        gen_ri_ops!($(($op2, $inst2)),+);
    };
}

impl IrContext {
    gen_ri_ops!((add, Add), (sub, Sub), (mul, Mul), (div, Div), (exp, Exp));
    gen_ops!(
        (rem, Rem),
        (bitor, BitOr),
        (bitand, BitAnd),
        (bitxor, BitXor),
        (shr, Shr),
        (shl, Shl)
    );
}

fn enc_w(opcode: u16, op1: u16) -> u64 {
    enc_www(opcode, op1, 0, 0)
}

fn enc_wl(opcode: u16, op1: u16, op2: u32) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + (op2 as u64)
}

fn enc_l(opcode: u16, op1: u32) -> u64 {
    enc_wl(opcode, 0, op1)
}

fn enc_ww(opcode: u16, op1: u16, op2: u16) -> u64 {
    enc_www(opcode, op1, op2, 0)
}

fn enc_www(opcode: u16, op1: u16, op2: u16, op3: u16) -> u64 {
    ((opcode as u64) << 48) + ((op1 as u64) << 32) + ((op2 as u64) << 16) + (op3 as u64)
}

fn enc_wsww(opcode: u16, op1: u16, op2: i16, op3: u16) -> u64 {
    enc_www(opcode, op1, op2 as u16, op3)
}

fn enc_wwsw(opcode: u16, op1: u16, op2: u16, op3: i16) -> u64 {
    enc_www(opcode, op1, op2, op3 as u16)
}

impl IrContext {
    pub(crate) fn ir_to_bytecode(&mut self, info: &mut ISeqInfo, store: &mut FnStore) {
        let mut ops = vec![];
        let mut locs = vec![];
        for (idx, (inst, loc)) in self.ir.iter().enumerate() {
            let op = match inst {
                BcIr::LoopStart => Bc::from(enc_l(14, 0)),
                BcIr::LoopEnd => Bc::from(enc_l(15, 0)),
                BcIr::Br(dst) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let op1 = dst - idx as i32 - 1;
                    Bc::from(enc_l(3, op1 as u32))
                }
                BcIr::CondBr(reg, dst, optimizable, kind) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let op1 = info.get_index(reg);
                    let op2 = dst - idx as i32 - 1;
                    let kind = *kind as u16;
                    let op = enc_wl(
                        if *optimizable { 12 + kind } else { 4 + kind },
                        op1.0,
                        op2 as u32,
                    );
                    Bc::from(op)
                }
                BcIr::Integer(reg, num) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(6, op1.0, *num as u32))
                }
                BcIr::Symbol(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(9, op1.0, name.get()))
                }
                BcIr::Nil(reg) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_w(8, op1.0))
                }
                BcIr::Literal(reg, val) => {
                    let op1 = info.get_index(reg);
                    Bc::from_with_value(enc_wl(7, op1.0, 0), *val)
                }
                BcIr::Array(ret, src, len) => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(src);
                    Bc::from(enc_www(131, op1.0, op2.0, *len))
                }
                BcIr::Index(ret, base, idx) => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(base);
                    let op3 = info.get_index(idx);
                    Bc::from_with_class2(enc_www(132, op1.0, op2.0, op3.0))
                }
                BcIr::StoreIndex(src, base, idx) => {
                    let op1 = info.get_index(src);
                    let op2 = info.get_index(base);
                    let op3 = info.get_index(idx);
                    Bc::from(enc_www(133, op1.0, op2.0, op3.0))
                }
                BcIr::LoadDynVar { ret, src, outer } => {
                    let op1 = info.get_index(ret);
                    let op2 = info.get_index(src);
                    let op3 = *outer as u16;
                    Bc::from(enc_www(150, op1.0, op2.0, op3))
                }
                BcIr::StoreDynVar { dst, outer, src } => {
                    let op1 = info.get_index(dst);
                    let op2 = *outer as u16;
                    let op3 = info.get_index(src);
                    Bc::from(enc_www(151, op1.0, op2, op3.0))
                }
                BcIr::LoadConst(reg, toplevel, prefix, name) => {
                    let op1 = info.get_index(reg);
                    let op2 = info.add_constsite(store, *name, prefix.clone(), *toplevel);
                    Bc::from(enc_wl(10, op1.0, op2.0))
                }
                BcIr::StoreConst(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(11, op1.0, name.get()))
                }
                BcIr::LoadIvar(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(16, op1.0, name.get()))
                }
                BcIr::StoreIvar(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(17, op1.0, name.get()))
                }
                BcIr::Neg(dst, src) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(src);
                    Bc::from_with_class_and_version(
                        enc_ww(129, op1.0, op2.0),
                        ClassId::default(),
                        -1i32 as u32,
                    )
                }
                BcIr::BinOpIr(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_wsww(180 + *kind as u16, op1.0, *lhs, op3.0))
                }
                BcIr::BinOp(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_www(200 + *kind as u16, op1.0, op2.0, op3.0))
                }
                BcIr::BinOpRi(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    Bc::from_with_class2(enc_wwsw(220 + *kind as u16, op1.0, op2.0, *rhs))
                }
                BcIr::Cmp(kind, dst, lhs, rhs, optimizable) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op3 = info.get_index(rhs);
                    let op = if *optimizable {
                        enc_www(156 + *kind as u16, op1.0, op2.0, op3.0)
                    } else {
                        enc_www(134 + *kind as u16, op1.0, op2.0, op3.0)
                    };
                    Bc::from_with_class2(op)
                }
                BcIr::Cmpri(kind, dst, lhs, rhs, optimizable) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op = if *optimizable {
                        enc_wwsw(162 + *kind as u16, op1.0, op2.0, *rhs)
                    } else {
                        enc_wwsw(142 + *kind as u16, op1.0, op2.0, *rhs)
                    };
                    Bc::from_with_class2(op)
                }
                BcIr::Ret(reg) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_w(148, op1.0))
                }
                BcIr::Mov(dst, src) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(src);
                    Bc::from(enc_ww(149, op1.0, op2.0))
                }
                BcIr::MethodCall(ret, name) => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_class_and_version(
                        enc_wl(1, op1.0, name.get()),
                        ClassId::new(0),
                        -1i32 as u32,
                    )
                }
                BcIr::MethodCallBlock(ret, name) => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_class_and_version(
                        enc_wl(19, op1.0, name.get()),
                        ClassId::new(0),
                        -1i32 as u32,
                    )
                }
                BcIr::Yield { ret, args, len } => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    let op2 = info.get_index(args);
                    Bc::from(enc_www(152, op1.0, op2.0, *len as u16))
                }
                BcIr::MethodArgs(recv, args, len) => {
                    let op1 = info.get_index(recv);
                    let op2 = info.get_index(args);
                    Bc::from(enc_www(130, op1.0, op2.0, *len as u16))
                }
                BcIr::InlineCache => Bc::from(0),
                BcIr::MethodDef(name, func_id) => {
                    Bc::from_with_func_name_id(enc_l(2, 0), *name, *func_id)
                }
                BcIr::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    let op1 = match ret {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    let op2 = match superclass {
                        None => SlotId::new(0),
                        Some(ret) => info.get_index(ret),
                    };
                    Bc::from_with_func_name_id(enc_wl(18, op1.0, op2.0 as u32), *name, *func_id)
                }
                BcIr::ConcatStr(ret, arg, len) => {
                    let op1 = ret.map_or(SlotId::self_(), |ret| info.get_index(&ret));
                    let op2 = info.get_index(&BcReg::from(*arg));
                    Bc::from(enc_www(155, op1.0, op2.0, *len as u16))
                }
            };
            ops.push(op);
            locs.push(*loc);
        }
        info.set_bytecode(ops);
        info.sourcemap = locs;
    }
}
