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
        BcPcBase(func.bytecode_top())
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BcPc(pub(crate) *const Bc);

impl BcPc {
    pub(crate) fn from(bc: &Bc) -> Self {
        Self(bc as *const _)
    }
}

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

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, loc: Loc) {
        self.push(BcIr::Array(ret, src, len as u16), loc);
    }

    fn gen_array(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
        self.push_expr(ctx, info, id_store, expr)?;
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

    fn push_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut NormalFuncInfo,
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
            NodeKind::Array(nodes, _) => self.gen_array(ctx, info, id_store, None, nodes, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                assert_eq!(1, index.len());
                let ret = self.push_expr(ctx, info, id_store, base)?;
                let idx = self.push_expr(ctx, info, id_store, index.remove(0))?;
                info.pop();
                self.push(BcIr::Index(ret, ret, idx), loc);
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                    NodeKind::Float(f) => self.gen_float(ctx, info, None, -f),
                    _ => {
                        self.push_expr(ctx, info, id_store, rhs)?;
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
                            let src = self.push_expr(ctx, info, id_store, rhs)?;
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
            NodeKind::Array(nodes, _) => {
                self.gen_array(ctx, info, id_store, Some(local), nodes, loc)?
            }
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
                        NodeKind::Const {
                            toplevel,
                            name,
                            parent: _,
                            prefix: _,
                        } => {
                            assert!(!toplevel);
                            let name = id_store.get_ident_id_from_string(name);
                            self.gen_store_expr(ctx, info, id_store, local, rhs, false)?;
                            self.gen_store_const(local.into(), name, loc);
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
                let ret = self.push_expr(ctx, info, id_store, rhs)?;
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
            self.push_expr(ctx, info, id_store, arg)?;
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

    fn gen_call(
        &mut self,
        recv: BcReg,
        method: IdentId,
        ret: Option<BcReg>,
        arg: BcTemp,
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
            self.push_expr(ctx, info, id_store, receiver)?;
            let (arg, len) = self.check_fast_call(ctx, info, id_store, arglist)?;
            let recv = info.pop().into();
            (recv, arg, len)
        };
        self.gen_call(recv, method, ret, arg, len, loc);
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
        self.gen_call(BcReg::Self_, method, ret, arg, len, loc);
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
                self.push_expr(ctx, info, id_store, lhs)?;
                self.push_expr(ctx, info, id_store, rhs)?;
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
        let mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        assert!(mlhs_len == mrhs_len);
        let mut temp_reg = info.next_reg();
        // At first we evaluate right-hand side values and save them in temporory registers.
        for rhs in mrhs {
            self.push_expr(ctx, info, id_store, rhs)?;
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
        info.popn(mrhs_len);

        if is_ret || use_value {
            let ret = info.push().into();
            self.emit_array(ret, ret, mrhs_len, loc);
        }
        if is_ret {
            self.gen_ret(info, None);
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
                    self.push(BcIr::BinOpRi(BinOpK::$inst, dst, lhs, i), loc);
                } else if let Some(i) = is_smi(&lhs) {
                    let (dst, rhs) = self.gen_singular(ctx, info, id_store, dst, rhs)?;
                    self.push(BcIr::BinOpIr(BinOpK::$inst, dst, i, rhs), loc);
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
    pub(crate) fn ir_to_bytecode(&mut self, info: &mut NormalFuncInfo, store: &mut FnStore) {
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
                BcIr::Literal(reg, num) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(7, op1.0, *num))
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
                BcIr::LoadConst(reg, name) => {
                    let op1 = info.get_index(reg);
                    let op2 = info.add_constsite(store, *name, vec![], false);
                    Bc::from(enc_wl(10, op1.0, op2.get()))
                }
                BcIr::StoreConst(reg, name) => {
                    let op1 = info.get_index(reg);
                    Bc::from(enc_wl(11, op1.0, name.get()))
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
                    Bc::from(op)
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
                    let op1 = store.add_method_def(*name, *func_id);
                    Bc::from(enc_l(2, op1.0))
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
