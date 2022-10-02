use super::*;
use num::BigInt;
use paste::paste;

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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IrContext {
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// loop information.
    loops: Vec<(LoopKind, usize, Option<BcReg>)>, // (kind, label for exit, return register)
}

impl IrContext {
    pub(crate) fn new() -> Self {
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
}

pub fn is_smi(node: &Node) -> Option<i16> {
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
    Index { base: BcReg, index: BcReg },
    Send { recv: BcReg, method: IdentId },
    Other,
}

impl IrContext {
    pub fn compile_ast(
        info: &mut RubyFuncInfo,
        ctx: &mut FnStore,
        id_store: &mut IdentifierTable,
    ) -> Result<IrContext> {
        let mut ir = IrContext::new();
        let ast = std::mem::take(&mut info.ast).unwrap();
        ir.gen_expr(ctx, info, id_store, ast, true, true)?;
        assert_eq!(0, info.temp);
        Ok(ir)
    }

    fn gen_load_const(
        &mut self,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        toplevel: bool,
        name: String,
        prefix: Vec<String>,
        loc: Loc,
    ) {
        let name = id_store.get_ident_id_from_string(name);
        let prefix = prefix
            .into_iter()
            .map(|s| id_store.get_ident_id_from_string(s))
            .collect();
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::LoadConst(reg, toplevel, prefix, name), loc);
    }

    fn gen_load_ivar(
        &mut self,
        info: &mut RubyFuncInfo,
        dst: Option<BcLocal>,
        name: IdentId,
        loc: Loc,
    ) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::LoadIvar(reg, name), loc);
    }

    fn gen_literal(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, v: Value) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::Literal(reg, v), Loc::default());
    }

    fn gen_integer(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local.into(),
                None => info.push().into(),
            };
            self.push(BcIr::Integer(reg, i), Loc::default());
        } else {
            self.gen_literal(info, dst, Value::new_integer(i));
        }
    }

    fn gen_float(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, f: f64) {
        self.gen_literal(info, dst, Value::new_float(f));
    }

    fn gen_symbol(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, sym: IdentId) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::Symbol(reg, sym), Loc::default());
    }

    fn gen_string(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, b: Vec<u8>) {
        self.gen_literal(info, dst, Value::new_string(b));
    }

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, loc: Loc) {
        self.push(BcIr::Array(ret, src, len as u16), loc);
    }

    fn gen_array(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        ret: Option<BcLocal>,
        nodes: Vec<Node>,
        loc: Loc,
    ) -> Result<()> {
        let len = nodes.len();
        let src = self.gen_args(ctx, info, id_store, nodes)?.into();
        info.popn(len);
        let ret = match ret {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.emit_array(ret, src, len, loc);
        Ok(())
    }

    fn gen_index(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        ret: Option<BcLocal>,
        base: Node,
        index: Node,
        loc: Loc,
    ) -> Result<()> {
        let (base, idx) = self.gen_binary_temp_expr(ctx, info, id_store, base, index)?;
        let ret = match ret {
            None => info.push().into(),
            Some(local) => local.into(),
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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        lhs: &Node,
    ) -> Result<LvalueKind> {
        let lhs = match &lhs.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } if !toplevel && parent.is_none() && prefix.is_empty() => {
                let name = id_store.get_ident_id(name);
                LvalueKind::Const(name)
            }
            NodeKind::InstanceVar(name) => {
                let name = id_store.get_ident_id(name);
                LvalueKind::InstanceVar(name)
            }
            NodeKind::Index { box base, index } => {
                assert_eq!(1, index.len());
                let index = index[0].clone();
                let base = self.gen_expr_reg(ctx, info, id_store, base.clone())?;
                let index = self.gen_expr_reg(ctx, info, id_store, index)?;
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
                let recv = self.gen_expr_reg(ctx, info, id_store, receiver.clone())?;
                let method = id_store.get_ident_id(&format!("{}=", method));
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
            LvalueKind::Index { base, index } => {
                self.push(BcIr::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Send { recv, method } => {
                self.gen_method_assign(method, recv, src, loc);
            }
            LvalueKind::Other => unreachable!(),
        }
    }

    fn gen_bigint(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>, bigint: BigInt) {
        self.gen_literal(info, dst, Value::new_bigint(bigint));
    }

    fn gen_nil(&mut self, info: &mut RubyFuncInfo, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::Nil(reg), Loc::default());
    }

    fn gen_neg(&mut self, info: &mut RubyFuncInfo, local: Option<BcLocal>, loc: Loc) {
        match local {
            Some(local) => {
                let local = local.into();
                self.push(BcIr::Neg(local, local), loc);
            }
            None => {
                let src = info.pop().into();
                let dst = info.push().into();
                self.push(BcIr::Neg(dst, src), loc);
            }
        };
    }

    fn gen_ret(&mut self, info: &mut RubyFuncInfo, local: Option<BcLocal>) {
        let ret = match local {
            Some(local) => local.into(),
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

    fn gen_temp_mov(&mut self, info: &mut RubyFuncInfo, rhs: BcReg) {
        let lhs = info.push();
        self.gen_mov(lhs.into(), rhs);
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        mut nodes: Vec<Node>,
        ret: Option<BcLocal>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(ctx, info, id_store, node, false, false)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, info, id_store, ret, last)?;
                if is_ret {
                    self.gen_ret(info, ret.into());
                } else if use_value {
                    self.gen_temp_mov(info, ret.into());
                }
            }
            None => {
                self.gen_expr(ctx, info, id_store, last, use_value, is_ret)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_expr_reg(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        expr: Node,
    ) -> Result<BcReg> {
        Ok(match info.is_local(&expr) {
            Some(lhs) => lhs.into(),
            None => self.push_expr(ctx, info, id_store, expr)?,
        })
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        expr: Node,
    ) -> Result<BcReg> {
        Ok(match info.is_local(&expr) {
            Some(lhs) => lhs.into(),
            None => {
                self.push_expr(ctx, info, id_store, expr)?;
                info.pop().into()
            }
        })
    }

    fn gen_binary_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        match (info.is_local(&lhs), info.is_local(&rhs)) {
            (None, None) => {
                let lhs = self.push_expr(ctx, info, id_store, lhs)?;
                let rhs = self.push_expr(ctx, info, id_store, rhs)?;
                info.temp -= 2;
                Ok((lhs, rhs))
            }
            _ => {
                let lhs = self.gen_temp_expr(ctx, info, id_store, lhs)?;
                let rhs = self.gen_temp_expr(ctx, info, id_store, rhs)?;
                Ok((lhs, rhs))
            }
        }
    }

    /// Generate bytecode Ir for binary operations.
    fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcLocal>,
        loc: Loc,
    ) -> Result<BcReg> {
        match op {
            BinOp::Add => self.gen_add(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Sub => self.gen_sub(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Mul => self.gen_mul(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Div => self.gen_div(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::BitOr => self.gen_bitor(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::BitAnd => self.gen_bitand(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::BitXor => self.gen_bitxor(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Shr => self.gen_shr(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Shl => self.gen_shl(ctx, info, id_store, dst, lhs, rhs, loc),
            BinOp::Cmp(kind) => self.gen_cmp(ctx, info, id_store, dst, kind, lhs, rhs, false, loc),
            _ => {
                return Err(MonorubyErr::unsupported_operator(
                    op,
                    loc,
                    info.sourceinfo.clone(),
                ))
            }
        }
    }

    fn push_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        expr: Node,
    ) -> Result<BcReg> {
        let ret = info.next_reg().into();
        self.gen_expr(ctx, info, id_store, expr, true, false)?;
        Ok(ret)
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
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
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(info, None, sym);
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(info, None, bigint),
            NodeKind::Float(f) => self.gen_float(info, None, f),
            NodeKind::String(s) => self.gen_string(info, None, s.into_bytes()),
            NodeKind::Array(nodes, _) => self.gen_array(ctx, info, id_store, None, nodes, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                assert_eq!(1, index.len());
                self.gen_index(ctx, info, id_store, None, base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                    NodeKind::Float(f) => self.gen_float(info, None, -f),
                    _ => {
                        self.push_expr(ctx, info, id_store, rhs)?;
                        self.gen_neg(info, None, loc);
                    }
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                if let Some(local) = info.is_local(&lhs) {
                    self.gen_binop(ctx, info, id_store, op, lhs, rhs, Some(local), loc)?;
                    if is_ret {
                        self.gen_ret(info, Some(local));
                    } else if use_value {
                        self.gen_temp_mov(info, local.into());
                    }
                    return Ok(());
                }
                let lhs_loc = lhs.loc;
                let temp = info.temp;
                // First, evaluate lvalue.
                let lhs_kind = self.eval_lvalue(ctx, info, id_store, &lhs)?;
                // Evaluate rvalue.
                let src = self.gen_binop(ctx, info, id_store, op, lhs, rhs, None, loc)?;
                // Assign rvalue to lvalue.
                self.gen_assign(src, lhs_kind, lhs_loc);
                info.temp = temp;
                let res = info.push().into();
                if use_value {
                    self.gen_mov(res, src);
                }
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, id_store, op, lhs, rhs, None, loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(local) = info.is_local(&lhs) {
                        self.gen_store_expr(ctx, info, id_store, local, rhs)?;
                        if is_ret {
                            self.gen_ret(info, Some(local));
                        } else if use_value {
                            self.gen_temp_mov(info, local.into());
                        }
                        return Ok(());
                    }
                    let temp = info.temp;
                    let lhs = self.eval_lvalue(ctx, info, id_store, &lhs)?;
                    let src = self.gen_expr_reg(ctx, info, id_store, rhs)?;
                    self.gen_assign(src, lhs, loc);
                    info.temp = temp;
                    let res = info.push().into();
                    if use_value {
                        self.gen_mov(res, src);
                    }
                } else {
                    return self.gen_mul_assign(ctx, info, id_store, mlhs, mrhs, use_value, is_ret);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local = info.find_local(&ident);
                if is_ret {
                    self.gen_ret(info, Some(local));
                } else if use_value {
                    self.gen_temp_mov(info, local.into());
                }
                return Ok(());
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.gen_load_const(info, id_store, None, toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = id_store.get_ident_id_from_string(name);
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
                    ctx, info, id_store, method, receiver, arglist, ret, is_ret, loc,
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
                return self.gen_func_call(ctx, info, id_store, method, arglist, ret, is_ret, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let ret = if use_value {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_func_call(ctx, info, id_store, method, arglist, ret, is_ret, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                let succ_pos = self.new_label();
                if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
                    let loc = cond.loc;
                    let cond = info.next_reg().into();
                    self.gen_cmp(ctx, info, id_store, None, kind, lhs, rhs, true, loc)?;
                    info.pop();
                    self.gen_condnotbr(cond, else_pos, true);
                } else {
                    let cond = self.gen_temp_expr(ctx, info, id_store, cond)?;
                    self.gen_condnotbr(cond, else_pos, false);
                }
                self.gen_expr(ctx, info, id_store, then_, use_value, is_ret)?;
                if !is_ret {
                    self.gen_br(succ_pos);
                    if use_value {
                        info.pop();
                    }
                }
                self.apply_label(else_pos);
                self.gen_expr(ctx, info, id_store, else_, use_value, is_ret)?;
                self.apply_label(succ_pos);
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
            } => {
                assert!(cond_op);
                self.gen_while(ctx, info, id_store, cond, body, use_value)?;
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
                self.gen_for(ctx, info, id_store, param, iter, body, use_value)?;
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
                match ret_reg {
                    Some(reg) => {
                        let temp = self.gen_temp_expr(ctx, info, id_store, val)?;
                        self.gen_mov(reg, temp)
                    }
                    None => {}
                }
                self.push(BcIr::Br(break_pos), loc);
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = info.is_local(&expr) {
                    self.gen_ret(info, Some(local));
                } else {
                    self.gen_expr(ctx, info, id_store, expr, true, true)?;
                }
                if use_value && !is_ret {
                    unreachable!();
                }
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, info, id_store, nodes, None, use_value, is_ret)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_: None,
                ensure: None,
            } => {
                assert!(rescue.len() == 0);
                self.gen_expr(ctx, info, id_store, body, use_value, is_ret)?;
                return Ok(());
            }
            NodeKind::MethodDef(name, params, box body, _lv) => {
                self.gen_method_def(ctx, info, id_store, name.clone(), params, body, loc)?;
                if use_value {
                    self.gen_symbol(info, None, id_store.get_ident_id_from_string(name));
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
                box body,
                lvar: _,
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
                self.gen_class_def(
                    ctx,
                    info,
                    id_store,
                    name.clone(),
                    superclass,
                    body,
                    ret,
                    loc,
                )?;
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                let len = nodes.len();
                let arg = info.next_reg();
                for expr in nodes {
                    self.push_expr(ctx, info, id_store, expr)?;
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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        local: BcLocal,
        rhs: Node,
    ) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(info, Some(local)),
            NodeKind::Bool(b) => self.gen_literal(info, Some(local), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(local.into(), BcReg::Self_),
            NodeKind::Integer(i) => self.gen_integer(info, Some(local), i),
            NodeKind::Symbol(sym) => {
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(info, Some(local), sym)
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(info, Some(local), bigint),
            NodeKind::Float(f) => self.gen_float(info, Some(local), f),
            NodeKind::String(s) => self.gen_string(info, Some(local), s.into_bytes()),
            NodeKind::Array(nodes, _) => {
                self.gen_array(ctx, info, id_store, Some(local), nodes, loc)?
            }
            NodeKind::Index {
                box base,
                mut index,
            } => {
                assert_eq!(1, index.len());
                self.gen_index(ctx, info, id_store, Some(local), base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(info, Some(local), -i),
                    NodeKind::Float(f) => self.gen_float(info, Some(local), -f),
                    _ => {
                        self.gen_store_expr(ctx, info, id_store, local, rhs)?;
                        self.gen_neg(info, Some(local), loc);
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, id_store, op, lhs, rhs, Some(local), loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(src) = info.is_local(&lhs) {
                        self.gen_store_expr(ctx, info, id_store, src, rhs)?;
                        self.gen_mov(local.into(), src.into());
                    } else {
                        let temp = info.temp;
                        let lhs = self.eval_lvalue(ctx, info, id_store, &lhs)?;
                        let src = local.into();
                        self.gen_store_expr(ctx, info, id_store, local, rhs)?;
                        self.gen_assign(src, lhs, loc);
                        info.temp = temp;
                    }
                } else {
                    self.gen_mul_assign(ctx, info, id_store, mlhs, mrhs, true, false)?;
                    let temp = info.pop().into();
                    self.gen_mov(local.into(), temp);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = info.find_local(&ident);
                self.gen_mov(local.into(), local2.into());
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.gen_load_const(info, id_store, local.into(), toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = id_store.get_ident_id_from_string(name);
                self.gen_load_ivar(info, local.into(), name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(local.into());
                self.gen_method_call(
                    ctx, info, id_store, method, receiver, arglist, ret, false, loc,
                )?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(local.into());
                self.gen_func_call(ctx, info, id_store, method, arglist, ret, false, loc)?;
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(ctx, info, id_store, nodes, Some(local), false, false)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                box body,
                lvar: _,
                is_module,
            } => {
                assert!(base.is_none());
                assert!(!is_module);
                let ret = Some(local.into());
                let superclass = superclass.map(|c| *c);
                self.gen_class_def(ctx, info, id_store, name, superclass, body, ret, loc)?;
            }
            _ => {
                let ret = self.push_expr(ctx, info, id_store, rhs)?;
                self.gen_mov(local.into(), ret.into());
                info.pop();
            }
        };
        Ok(())
    }

    fn gen_method_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        name: String,
        params: Vec<FormalParam>,
        node: Node,
        loc: Loc,
    ) -> Result<()> {
        let mut args = vec![];
        for param in params {
            match param.kind {
                ParamKind::Param(name) => args.push(name),
                _ => {
                    return Err(MonorubyErr::unsupported_parameter_kind(
                        param.kind,
                        param.loc,
                        info.sourceinfo.clone(),
                    ))
                }
            }
        }
        let func_id = ctx.add_ruby_func(
            Some(name.clone()),
            args,
            node,
            info.sourceinfo.clone(),
            false,
        );
        let name = id_store.get_ident_id_from_string(name);
        self.push(BcIr::MethodDef(name, func_id), loc);
        Ok(())
    }

    fn gen_class_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        name: String,
        superclass: Option<Node>,
        node: Node,
        ret: Option<BcReg>,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_ruby_func(
            Some(name.clone()),
            vec![],
            node,
            info.sourceinfo.clone(),
            true,
        );
        let name = id_store.get_ident_id_from_string(name);
        let superclass = match superclass {
            Some(superclass) => Some(self.gen_temp_expr(ctx, info, id_store, superclass)?),
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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = info.next_reg();
        for arg in args {
            self.push_expr(ctx, info, id_store, arg)?;
        }
        Ok(arg)
    }

    fn check_fast_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        arglist: ArgList,
    ) -> Result<(BcTemp, usize)> {
        assert!(arglist.kw_args.len() == 0);
        assert!(arglist.hash_splat.len() == 0);
        assert!(arglist.block.is_none());
        assert!(!arglist.delegate);
        self.check_fast_call_inner(ctx, info, id_store, arglist.args)
    }

    fn check_fast_call_inner(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<(BcTemp, usize)> {
        let len = args.len();
        let arg = self.gen_args(ctx, info, id_store, args)?;
        info.temp -= len as u16;
        Ok((arg, len))
    }

    fn gen_call(
        &mut self,
        recv: BcReg,
        method: IdentId,
        ret: Option<BcReg>,
        arg: BcReg,
        len: usize,
        loc: Loc,
    ) {
        self.push(BcIr::MethodCall(ret, method), loc);
        self.push(BcIr::MethodArgs(recv, arg, len), loc);
        self.push(BcIr::InlineCache, loc);
    }

    fn gen_method_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        method: String,
        receiver: Node,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let method = id_store.get_ident_id_from_string(method);
        let (recv, arg, len) = if receiver.kind == NodeKind::SelfValue {
            let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
            (BcReg::Self_, arg.into(), len)
        } else {
            self.push_expr(ctx, info, id_store, receiver)?;
            let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
            let recv = info.pop().into();
            (recv, arg.into(), len)
        };
        self.gen_call(recv, method, ret, arg, len, loc);
        if is_ret {
            self.gen_ret(info, None);
        }
        return Ok(());
    }

    fn gen_method_assign(&mut self, method: IdentId, receiver: BcReg, val: BcReg, loc: Loc) {
        self.gen_call(receiver, method, None, val, 1, loc);
    }

    fn gen_func_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        method: String,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
        let method = id_store.get_ident_id_from_string(method);
        self.gen_call(BcReg::Self_, method, ret, arg.into(), len, loc);
        if is_ret {
            self.gen_ret(info, None);
        }
        return Ok(());
    }

    fn gen_binary(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = self.gen_binary_temp_expr(ctx, info, id_store, lhs, rhs)?;
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = self.gen_temp_expr(ctx, info, id_store, lhs)?;
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }

    fn gen_cmp(
        &mut self,
        ctx: &mut FnStore,
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<BcReg> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, info, id_store, dst, lhs)?;
            self.push(BcIr::Cmpri(kind, dst, lhs, i, optimizable), loc);
            Ok(dst)
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
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
            lhs_kind.push(self.eval_lvalue(ctx, info, id_store, lhs)?);
        }

        // Next, we evaluate rvalues and save them in temporory registers which start from temp_reg.
        let rhs_reg = info.next_reg();
        let mut temp_reg = rhs_reg;
        for rhs in mrhs {
            self.push_expr(ctx, info, id_store, rhs)?;
        }

        // Finally, assign rvalues to lvalue.
        for (lhs, kind) in mlhs.into_iter().zip(lhs_kind) {
            if let Some(local) = info.is_local(&lhs) {
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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
        param: Vec<String>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        assert_eq!(1, param.len());
        let counter = info.find_local(&param[0]);
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
            exclude_end: false,
            ..
        } = iter.kind
        {
            let loop_entry = self.new_label();
            let loop_exit = self.new_label();
            self.gen_store_expr(ctx, info, id_store, counter, start)?;
            let end = self.push_expr(ctx, info, id_store, end)?;

            self.apply_label(loop_entry);
            self.push(BcIr::LoopStart, loc);
            let dst = info.push().into();
            self.push(BcIr::Cmp(CmpKind::Gt, dst, counter.into(), end, true), loc);
            self.gen_condbr(dst, loop_exit, true);
            info.pop();

            self.gen_expr(ctx, info, id_store, *body.body, false, false)?;

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
        info: &mut RubyFuncInfo,
        id_store: &mut IdentifierTable,
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
        if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
            let loc = cond.loc;
            let cond = info.next_reg().into();
            self.gen_cmp(ctx, info, id_store, None, kind, lhs, rhs, true, loc)?;
            info.pop();
            self.gen_condnotbr(cond, succ_pos, true);
        } else {
            let cond = self.gen_temp_expr(ctx, info, id_store, cond)?;
            self.gen_condnotbr(cond, succ_pos, false);
        }
        self.gen_expr(ctx, info, id_store, body, false, false)?;
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
}

macro_rules! gen_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                info: &mut RubyFuncInfo,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<BcReg> {
                let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
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
                info: &mut RubyFuncInfo,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<BcReg> {
                if let Some(i) = is_smi(&rhs) {
                    let (dst, lhs) = self.gen_singular(ctx, info, id_store, dst, lhs)?;
                    self.push(BcIr::BinOpRi(BinOpK::$inst, dst, lhs, i), loc);
                    Ok(dst)
                } else if let Some(i) = is_smi(&lhs) {
                    let (dst, rhs) = self.gen_singular(ctx, info, id_store, dst, rhs)?;
                    self.push(BcIr::BinOpIr(BinOpK::$inst, dst, i, rhs), loc);
                    Ok(dst)
                } else {
                    let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
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
    gen_ri_ops!((add, Add), (sub, Sub), (mul, Mul), (div, Div));
    gen_ops!(
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
    pub(crate) fn ir_to_bytecode(&mut self, info: &mut RubyFuncInfo, store: &mut FnStore) {
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
                    Bc::from(enc_www(132, op1.0, op2.0, op3.0))
                }
                BcIr::StoreIndex(src, base, idx) => {
                    let op1 = info.get_index(src);
                    let op2 = info.get_index(base);
                    let op3 = info.get_index(idx);
                    Bc::from(enc_www(133, op1.0, op2.0, op3.0))
                }
                BcIr::LoadConst(reg, toplevel, prefix, name) => {
                    let op1 = info.get_index(reg);
                    let op2 = info.add_constsite(store, *name, prefix.clone(), *toplevel);
                    Bc::from(enc_wl(10, op1.0, op2.get()))
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
                        INTEGER_CLASS,
                        -1i32 as u32,
                    )
                }
                BcIr::BinOp(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_www(170 + *kind as u16, op1.0, op2.0, op3.0))
                }
                BcIr::BinOpRi(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op2 = info.get_index(lhs);
                    Bc::from_with_class2(enc_wwsw(190 + *kind as u16, op1.0, op2.0, *rhs))
                }
                BcIr::BinOpIr(kind, dst, lhs, rhs) => {
                    let op1 = info.get_index(dst);
                    let op3 = info.get_index(rhs);
                    Bc::from_with_class2(enc_wsww(180 + *kind as u16, op1.0, *lhs, op3.0))
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
                BcIr::MethodArgs(recv, args, len) => {
                    let op1 = info.get_index(recv);
                    let op2 = info.get_index(&BcReg::from(*args));
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
