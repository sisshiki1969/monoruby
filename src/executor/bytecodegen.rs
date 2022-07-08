use super::*;
use num::BigInt;
use paste::paste;

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BcPcBase(*const Bc);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs as isize) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    pub(super) fn new(func: &NormalFuncInfo) -> Self {
        BcPcBase(&func.bytecode()[0] as *const _)
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BcPc(*const Bc);

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::Sub<BcPc> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPc) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc(self.0.offset(offset as isize));
        }
    }
}

impl std::default::Default for BcPc {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

///
/// ID of instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(pub u32);

impl std::fmt::Debug for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

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
        self.push(BcIr::CondBr(cond, then_pos, optimizable), Loc::default());
    }

    fn gen_condnotbr(&mut self, cond: BcReg, else_pos: usize, optimizable: bool) {
        self.push(BcIr::CondNotBr(cond, else_pos, optimizable), Loc::default());
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

pub fn is_local(node: &Node) -> Option<&String> {
    if let NodeKind::LocalVar(name) = &node.kind {
        Some(name)
    } else {
        None
    }
}

impl IrContext {
    pub fn compile_ast(
        info: &mut NormalFuncInfo,
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
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        name: IdentId,
        loc: Loc,
    ) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::LoadConst(reg, name), loc);
    }

    fn gen_store_const(&mut self, src: BcReg, name: IdentId, loc: Loc) {
        self.push(BcIr::StoreConst(src, name), loc);
    }

    fn gen_literal(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        v: Value,
    ) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        let id = ctx.new_literal(v);
        self.push(BcIr::Literal(reg, id), Loc::default());
    }

    fn gen_integer(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        i: i64,
    ) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local.into(),
                None => info.push().into(),
            };
            self.push(BcIr::Integer(reg, i), Loc::default());
        } else {
            self.gen_literal(ctx, info, dst, Value::new_integer(i));
        }
    }

    fn gen_float(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        f: f64,
    ) {
        self.gen_literal(ctx, info, dst, Value::new_float(f));
    }

    fn gen_symbol(&mut self, info: &mut NormalFuncInfo, dst: Option<BcLocal>, sym: IdentId) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::Symbol(reg, sym), Loc::default());
    }

    fn gen_string(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        b: Vec<u8>,
    ) {
        self.gen_literal(ctx, info, dst, Value::new_string(b));
    }

    fn gen_bigint(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        dst: Option<BcLocal>,
        bigint: BigInt,
    ) {
        self.gen_literal(ctx, info, dst, Value::new_bigint(bigint));
    }

    fn gen_nil(&mut self, info: &mut NormalFuncInfo, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => info.push().into(),
        };
        self.push(BcIr::Nil(reg), Loc::default());
    }

    fn gen_neg(&mut self, info: &mut NormalFuncInfo, local: Option<BcLocal>, loc: Loc) {
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

    fn gen_ret(&mut self, info: &mut NormalFuncInfo, local: Option<BcLocal>) {
        let ret = match local {
            Some(local) => local.into(),
            None => info.pop().into(),
        };
        assert_eq!(0, info.temp);
        self.push(BcIr::Ret(ret), Loc::default());
    }

    fn gen_mov(&mut self, dst: BcReg, src: BcReg) {
        self.push(BcIr::Mov(dst, src), Loc::default());
    }

    fn gen_temp_mov(&mut self, info: &mut NormalFuncInfo, rhs: BcReg) {
        let lhs = info.push();
        self.gen_mov(lhs.into(), rhs);
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
                self.gen_store_expr(ctx, info, id_store, ret, last, use_value)?;
                if is_ret {
                    self.gen_ret(info, ret.into());
                }
            }
            None => {
                self.gen_expr(ctx, info, id_store, last, use_value, is_ret)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        expr: Node,
    ) -> Result<BcTemp> {
        self.gen_expr(ctx, info, id_store, expr, true, false)?;
        Ok(info.pop())
    }

    /// Generate bytecode Ir for binary operations.
    fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcLocal>,
        loc: Loc,
    ) -> Result<()> {
        match op {
            BinOp::Add => self.gen_add(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Sub => self.gen_sub(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Mul => self.gen_mul(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Div => self.gen_div(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitOr => self.gen_bitor(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitAnd => self.gen_bitand(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitXor => self.gen_bitxor(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Shr => self.gen_shr(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Shl => self.gen_shl(ctx, info, id_store, dst, lhs, rhs, loc)?,
            BinOp::Cmp(kind) => {
                self.gen_cmp(ctx, info, id_store, dst, kind, lhs, rhs, false, loc)?
            }
            _ => {
                return Err(MonorubyErr::unsupported_operator(
                    op,
                    loc,
                    info.sourceinfo.clone(),
                ))
            }
        };
        Ok(())
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
                | NodeKind::Float(_) => return Ok(()),
                _ => {}
            }
        }
        let loc = expr.loc;
        match expr.kind {
            NodeKind::Nil => self.gen_nil(info, None),
            NodeKind::Bool(b) => self.gen_literal(ctx, info, None, Value::bool(b)),
            NodeKind::SelfValue => self.gen_temp_mov(info, BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ctx, info, None, i);
            }
            NodeKind::Symbol(sym) => {
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(info, None, sym);
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(ctx, info, None, bigint),
            NodeKind::Float(f) => self.gen_float(ctx, info, None, f),
            NodeKind::String(s) => self.gen_string(ctx, info, None, s.into_bytes()),
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                    NodeKind::Float(f) => self.gen_float(ctx, info, None, -f),
                    _ => {
                        self.gen_expr(ctx, info, id_store, rhs, true, false)?;
                        self.gen_neg(info, None, loc);
                    }
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                match &lhs.kind {
                    NodeKind::LocalVar(lhs_) | NodeKind::Ident(lhs_) => {
                        let local = info.find_local(lhs_);
                        self.gen_binop(ctx, info, id_store, op, lhs, rhs, Some(local), loc)?;
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
                        prefix: _,
                    } => {
                        assert!(!toplevel);
                        let name = id_store.get_ident_id(name);
                        let src = info.next_reg();
                        let lhs_loc = lhs.loc;
                        self.gen_binop(ctx, info, id_store, op, lhs, rhs, None, loc)?;
                        self.gen_store_const(src.into(), name, lhs_loc);
                    }
                    _ => return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone())),
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, id_store, op, lhs, rhs, None, loc)?
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let local = info.find_local(&lhs);
                            if is_ret {
                                self.gen_store_expr(ctx, info, id_store, local, rhs, false)?;
                                self.gen_ret(info, Some(local));
                            } else {
                                self.gen_store_expr(ctx, info, id_store, local, rhs, use_value)?;
                            }
                            return Ok(());
                        }
                        NodeKind::Const {
                            toplevel,
                            name,
                            parent: _,
                            prefix: _,
                        } => {
                            assert!(!toplevel);
                            let name = id_store.get_ident_id_from_string(name);
                            let src = info.next_reg();
                            self.gen_expr(ctx, info, id_store, rhs, true, false)?;
                            self.gen_store_const(src.into(), name, loc);
                        }
                        _ => {
                            return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone()))
                        }
                    }
                } else {
                    return self.gen_mul_assign(ctx, info, id_store, mlhs, mrhs, use_value, is_ret);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local = info.load_local(&ident, loc)?;
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
                prefix: _,
            } => {
                assert!(!toplevel);
                let name = id_store.get_ident_id_from_string(name);
                self.gen_load_const(info, None, name, loc);
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
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
                    let loc = cond.loc;
                    let cond = info.next_reg().into();
                    self.gen_cmp(ctx, info, id_store, None, kind, lhs, rhs, true, loc)?;
                    info.pop();
                    self.gen_condbr(cond, then_pos, true);
                } else {
                    let cond = self.gen_temp_expr(ctx, info, id_store, cond)?.into();
                    self.gen_condbr(cond, then_pos, false);
                }
                self.gen_expr(ctx, info, id_store, else_, use_value, is_ret)?;
                if !is_ret {
                    self.gen_br(succ_pos);
                    if use_value {
                        info.pop();
                    }
                }
                self.apply_label(then_pos);
                self.gen_expr(ctx, info, id_store, then_, use_value, is_ret)?;
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
                        self.gen_mov(reg, temp.into())
                    }
                    None => {}
                }
                self.push(BcIr::Br(break_pos), loc);
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = is_local(&expr) {
                    let local = info.load_local(local, expr.loc)?;
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
            NodeKind::MethodDef(name, params, box node, _lv) => {
                self.gen_method_def(ctx, info, id_store, name.clone(), params, node)?;
                if use_value {
                    self.gen_symbol(info, None, id_store.get_ident_id_from_string(name));
                }
                if is_ret {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                let len = nodes.len();
                let arg = info.next_reg();
                for expr in nodes {
                    self.gen_expr(ctx, info, id_store, expr, true, false)?;
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
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        local: BcLocal,
        rhs: Node,
        use_value: bool,
    ) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(info, Some(local)),
            NodeKind::Bool(b) => self.gen_literal(ctx, info, Some(local), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(local.into(), BcReg::Self_),
            NodeKind::Integer(i) => self.gen_integer(ctx, info, Some(local), i),
            NodeKind::Symbol(sym) => {
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(info, Some(local), sym)
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(ctx, info, Some(local), bigint),
            NodeKind::Float(f) => self.gen_float(ctx, info, Some(local), f),
            NodeKind::String(s) => self.gen_string(ctx, info, Some(local), s.into_bytes()),
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(ctx, info, Some(local), -i),
                    NodeKind::Float(f) => self.gen_float(ctx, info, Some(local), -f),
                    _ => {
                        self.gen_store_expr(ctx, info, id_store, local, rhs, false)?;
                        self.gen_neg(info, Some(local), loc);
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, id_store, op, lhs, rhs, Some(local), loc)?
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let src = info.find_local(&lhs);
                            self.gen_store_expr(ctx, info, id_store, src, rhs, false)?;
                            self.gen_mov(local.into(), src.into());
                        }
                        _ => {
                            return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone()))
                        }
                    }
                } else {
                    self.gen_mul_assign(ctx, info, id_store, mlhs, mrhs, true, false)?;
                    let temp = info.pop().into();
                    self.gen_mov(local.into(), temp);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = info.load_local(&ident, loc)?;
                self.gen_mov(local.into(), local2.into());
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix: _,
            } => {
                assert!(!toplevel);
                let name = id_store.get_ident_id_from_string(name);
                self.gen_load_const(info, local.into(), name, loc);
                return Ok(());
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
                return self.gen_comp_stmts(
                    ctx,
                    info,
                    id_store,
                    nodes,
                    Some(local),
                    use_value,
                    false,
                )
            }
            _ => {
                let ret = info.next_reg();
                self.gen_expr(ctx, info, id_store, rhs, true, false)?;
                self.gen_mov(local.into(), ret.into());
                if !use_value {
                    info.pop();
                }
                return Ok(());
            }
        };
        if use_value {
            self.gen_temp_mov(info, local.into());
        }
        Ok(())
    }

    fn gen_method_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        name: String,
        params: Vec<FormalParam>,
        node: Node,
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
        let func_id = ctx.add_normal_func(Some(name.clone()), args, node, info.sourceinfo.clone());
        let name = id_store.get_ident_id_from_string(name);
        self.push(BcIr::MethodDef(name, func_id), Loc::default());
        Ok(())
    }

    fn gen_args(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = info.next_reg();
        for arg in args {
            self.gen_expr(ctx, info, id_store, arg, true, false)?;
        }
        Ok(arg)
    }

    fn check_fast_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<(BcTemp, usize)> {
        let len = args.len();
        let arg = self.gen_args(ctx, info, id_store, args)?;
        info.temp -= len as u16;
        Ok((arg, len))
    }

    fn gen_method_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
            (BcReg::Self_, arg, len)
        } else {
            self.gen_expr(ctx, info, id_store, receiver, true, false)?;
            let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
            let recv = info.pop().into();
            (recv, arg, len)
        };
        self.push(BcIr::MethodCall(recv, method), loc);
        self.push(BcIr::MethodArgs(ret, arg, len), loc);
        if is_ret {
            self.gen_ret(info, None);
        }
        return Ok(());
    }

    fn gen_func_call(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        method: String,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
        let method = id_store.get_ident_id_from_string(method);
        self.push(BcIr::MethodCall(BcReg::Self_, method), loc);
        self.push(BcIr::MethodArgs(ret, arg, len), loc);
        if is_ret {
            self.gen_ret(info, None);
        }
        return Ok(());
    }

    fn gen_binary(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = match (is_local(&lhs), is_local(&rhs)) {
            (Some(lhs), Some(rhs)) => {
                let lhs = info.find_local(lhs).into();
                let rhs = info.find_local(rhs).into();
                (lhs, rhs)
            }
            (Some(lhs), None) => {
                let lhs = info.find_local(lhs).into();
                let rhs = self.gen_temp_expr(ctx, info, id_store, rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(ctx, info, id_store, lhs)?.into();
                let rhs = info.find_local(rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(ctx, info, id_store, lhs, true, false)?;
                self.gen_expr(ctx, info, id_store, rhs, true, false)?;
                let rhs = info.pop().into();
                let lhs = info.pop().into();
                (lhs, rhs)
            }
        };
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = match is_local(&lhs) {
            Some(lhs) => info.find_local(lhs).into(),
            None => self.gen_temp_expr(ctx, info, id_store, lhs)?.into(),
        };
        let dst = match dst {
            None => info.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }

    fn gen_cmp(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, info, id_store, dst, lhs)?;
            self.push(BcIr::Cmpri(kind, dst, lhs, i, optimizable), loc);
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
            self.push(BcIr::Cmp(kind, dst, lhs, rhs, optimizable), loc);
        }
        Ok(())
    }

    fn gen_mul_assign(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
        id_store: &mut IdentifierTable,
        mlhs: Vec<Node>,
        mrhs: Vec<Node>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        assert!(mlhs_len == mrhs.len());
        let mut temp_reg = info.next_reg();
        // At first we evaluate right-hand side values and save them in temporory registers.
        for rhs in mrhs {
            self.gen_expr(ctx, info, id_store, rhs, true, false)?;
        }
        // Assign values to left-hand side expressions.
        for lhs in mlhs {
            match lhs.kind {
                NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                    let local = info.find_local(&lhs);
                    self.gen_mov(local.into(), temp_reg.into());
                }
                NodeKind::Const {
                    toplevel,
                    parent,
                    prefix,
                    name,
                } if !toplevel && parent.is_none() && prefix.len() == 0 => {
                    let name = id_store.get_ident_id_from_string(name);
                    self.gen_store_const(temp_reg.into(), name, lhs.loc);
                }
                _ => return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone())),
            }
            temp_reg += 1;
        }
        info.popn(mlhs_len);
        // TODO: This is not correct. We must make an Array.
        if is_ret {
            self.gen_nil(info, None);
            self.gen_ret(info, None);
        } else if use_value {
            self.gen_nil(info, None);
        }
        Ok(())
    }

    fn gen_for(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
            self.gen_store_expr(ctx, info, id_store, counter, start, false)?;
            self.gen_temp_expr(ctx, info, id_store, end)?;
            let end = info.push().into();

            self.apply_label(loop_entry);
            let dst = info.push().into();
            self.push(BcIr::Cmp(CmpKind::Gt, dst, counter.into(), end, true), loc);
            self.gen_condbr(dst, loop_exit, true);
            info.pop();

            self.gen_expr(ctx, info, id_store, *body.body, false, false)?;

            self.push(BcIr::Addri(counter.into(), counter.into(), 1), loc);
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
        Ok(())
    }

    fn gen_while(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
        self.apply_label(cond_pos);
        if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
            let loc = cond.loc;
            let cond = info.next_reg().into();
            self.gen_cmp(ctx, info, id_store, None, kind, lhs, rhs, true, loc)?;
            info.pop();
            self.gen_condnotbr(cond, succ_pos, true);
        } else {
            let cond = self.gen_temp_expr(ctx, info, id_store, cond)?.into();
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

        Ok(())
    }
}

macro_rules! gen_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                info: &mut NormalFuncInfo,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<()> {
                let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
                self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
                Ok(())
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
                info: &mut NormalFuncInfo,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<()> {
                if let Some(i) = is_smi(&rhs) {
                    let (dst, lhs) = self.gen_singular(ctx, info, id_store, dst, lhs)?;
                    self.push(BcIr::[<$inst ri>](dst, lhs, i), loc);
                } else {
                    let (dst, lhs, rhs) = self.gen_binary(ctx, info, id_store, dst, lhs, rhs)?;
                    self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
                }
                Ok(())
            }
        }
    };
    (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
        gen_ri_ops!(($op1, $inst1));
        gen_ri_ops!($(($op2, $inst2)),+);
    };
}

impl IrContext {
    gen_ri_ops!((add, Add), (sub, Sub));
    gen_ops!(
        (mul, Mul),
        (div, Div),
        (bitor, BitOr),
        (bitand, BitAnd),
        (bitxor, BitXor),
        (shr, Shr),
        (shl, Shl)
    );
}

impl IrContext {
    pub(crate) fn ir_to_bytecode(&mut self, info: &mut NormalFuncInfo, store: &mut FnStore) {
        let mut ops = vec![];
        let mut locs = vec![];
        for (idx, (inst, loc)) in self.ir.iter().enumerate() {
            let op = match inst {
                BcIr::Br(dst) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    BcOp1::Br(dst - idx as i32 - 1).to_bc()
                }
                BcIr::CondBr(reg, dst, optimizable) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let cond_reg = info.get_index(reg);
                    let disp = dst - idx as i32 - 1;
                    BcOp1::CondBr(cond_reg, disp, *optimizable).to_bc()
                }
                BcIr::CondNotBr(reg, dst, optimizable) => {
                    let dst = self.labels[*dst].unwrap().0 as i32;
                    let cond_reg = info.get_index(reg);
                    let disp = dst - idx as i32 - 1;
                    BcOp1::CondNotBr(cond_reg, disp, *optimizable).to_bc()
                }
                BcIr::Integer(reg, num) => BcOp1::Integer(info.get_index(reg), *num).to_bc(),
                BcIr::Symbol(reg, name) => BcOp1::Symbol(info.get_index(reg), *name).to_bc(),
                BcIr::Literal(reg, num) => BcOp1::Literal(info.get_index(reg), *num).to_bc(),
                BcIr::LoadConst(reg, name) => BcOp1::LoadConst(
                    info.get_index(reg),
                    info.add_constsite(store, *name, vec![], false),
                )
                .to_bc(),
                BcIr::StoreConst(reg, name) => {
                    BcOp1::StoreConst(info.get_index(reg), *name).to_bc()
                }
                BcIr::Nil(reg) => BcOp1::Nil(info.get_index(reg)).to_bc(),
                BcIr::Neg(dst, src) => BcOp1::Neg(info.get_index(dst), info.get_index(src)).to_bc(),
                BcIr::BinOp(kind, dst, lhs, rhs) => BcOp1::BinOp(
                    *kind,
                    info.get_index(dst),
                    info.get_index(lhs),
                    info.get_index(rhs),
                )
                .to_bc(),
                BcIr::Addri(dst, lhs, rhs) => {
                    BcOp1::Addri(info.get_index(dst), info.get_index(lhs), *rhs).to_bc()
                }
                BcIr::Subri(dst, lhs, rhs) => {
                    BcOp1::Subri(info.get_index(dst), info.get_index(lhs), *rhs).to_bc()
                }
                BcIr::Cmp(kind, dst, lhs, rhs, optimizable) => {
                    let dst = info.get_index(dst);
                    let lhs = info.get_index(lhs);
                    let rhs = info.get_index(rhs);
                    BcOp1::Cmp(*kind, dst, lhs, rhs, *optimizable).to_bc()
                }
                BcIr::Cmpri(kind, dst, lhs, rhs, optimizable) => {
                    let dst = info.get_index(dst);
                    let lhs = info.get_index(lhs);
                    let rhs = *rhs;
                    BcOp1::Cmpri(*kind, dst, lhs, rhs, *optimizable).to_bc()
                }
                BcIr::Ret(reg) => BcOp1::Ret(info.get_index(reg)).to_bc(),
                BcIr::Mov(dst, src) => BcOp1::Mov(info.get_index(dst), info.get_index(src)).to_bc(),
                BcIr::MethodCall(recv, name) => {
                    let id = info.add_callsite(store, *name);
                    let recv = info.get_index(recv);
                    BcOp1::MethodCall(recv, id).to_bc()
                }
                BcIr::MethodArgs(ret, args, len) => BcOp1::MethodArgs(
                    match ret {
                        None => 0,
                        Some(ret) => info.get_index(ret),
                    },
                    info.get_index(&BcReg::from(*args)),
                    *len as u16,
                )
                .to_bc(),
                BcIr::MethodDef(name, func_id) => {
                    BcOp1::MethodDef(store.add_method_def(*name, *func_id)).to_bc()
                }
                BcIr::ConcatStr(ret, arg, len) => {
                    let ret = ret.map_or(0, |ret| info.get_index(&ret));
                    BcOp1::ConcatStr(ret, info.get_index(&BcReg::from(*arg)), *len as u16).to_bc()
                }
            };
            ops.push(op);
            locs.push(*loc);
        }
        info.set_bytecode(ops);
        info.sourcemap = locs;
    }
}
