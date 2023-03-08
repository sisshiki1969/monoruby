use super::*;
use num::BigInt;
use paste::paste;
use ruruby_parse::{ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, Node, NodeKind, UnOp};

mod binary;
mod encode;
mod method_call;

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
            Self::Local(local) => write!(f, "{local:?}"),
            Self::Temp(temp) => write!(f, "{temp:?}"),
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

impl BcReg {
    fn get_reg(info: &mut ISeqInfo, reg: Option<Self>) -> Self {
        match reg {
            Some(local) => local,
            None => info.push().into(),
        }
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

fn is_smi(node: &Node) -> Option<i16> {
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
    GlobalVar(IdentId),
    DynamicVar { outer: usize, dst: BcReg },
    Index { base: BcReg, index: BcReg },
    Send { recv: BcReg, method: IdentId },
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseMode {
    Ret,
    Use,
    NotUse,
}

impl UseMode {
    fn use_val(&self) -> bool {
        *self != Self::NotUse
    }

    fn is_ret(&self) -> bool {
        *self == UseMode::Ret
    }
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
    pub(crate) fn compile_func(info: &mut ISeqInfo, ctx: &mut FnStore) -> Result<()> {
        let mut ir = IrContext::new();
        let ast = std::mem::take(&mut info.ast).unwrap();
        ir.gen_dummy_init(info.is_block_style);
        for ForParamInfo {
            dst_outer,
            dst_reg,
            src_reg,
        } in info.args.for_param_info.clone()
        {
            ir.push(
                BcIr::StoreDynVar {
                    dst: dst_reg.into(),
                    outer: dst_outer,
                    src: BcReg::Local(BcLocal(src_reg as u16)),
                },
                Loc::default(),
            );
        }
        for ExpandInfo { src, dst, len } in &info.args.expand_info {
            ir.gen_expand_array(*src, *dst, *len);
        }
        for OptionalInfo { local, initializer } in info.args.optional_info.clone() {
            let local = local.into();
            let next = ir.new_label();
            ir.gen_check_local(local, next);
            ir.gen_store_expr(ctx, info, local, initializer)?;
            ir.apply_label(next);
        }
        let kw_reg = info.pos_num();
        for (id, (_, initializer)) in info.args.keyword_args.clone().into_iter().enumerate() {
            let local = BcLocal((kw_reg + id) as u16).into();
            let next = ir.new_label();
            ir.gen_check_local(local, next);
            if let Some(box init) = initializer {
                ir.gen_store_expr(ctx, info, local, init)?;
            } else {
                ir.gen_nil(info, Some(local));
            }
            ir.apply_label(next);
        }
        ir.gen_expr(ctx, info, ast, UseMode::Ret)?;
        ir.replace_init(info);
        assert_eq!(0, info.temp);
        ir.ir_to_bytecode(info, ctx);
        Ok(())
    }
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

    fn gen_ret(&mut self, info: &mut ISeqInfo, src: Option<BcReg>) {
        let ret = match src {
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

    fn gen_temp_mov(&mut self, info: &mut ISeqInfo, src: BcReg) {
        let dst = info.push();
        self.gen_mov(dst.into(), src);
    }

    fn gen_br(&mut self, jmp_pos: usize) {
        self.push(BcIr::Br(jmp_pos), Loc::default());
    }

    fn gen_condbr(&mut self, cond: BcReg, jmp_pos: usize, jmp_if_true: bool, optimizable: bool) {
        self.push(
            BcIr::CondBr(
                cond,
                jmp_pos,
                optimizable,
                if jmp_if_true {
                    BrKind::BrIf
                } else {
                    BrKind::BrIfNot
                },
            ),
            Loc::default(),
        );
    }

    fn gen_check_local(&mut self, local: BcReg, else_pos: usize) {
        self.push(BcIr::CheckLocal(local, else_pos), Loc::default());
    }

    fn gen_nil(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>) {
        let reg = match dst {
            Some(dst) => dst,
            None => info.push().into(),
        };
        self.push(BcIr::Nil(reg), Loc::default());
    }

    fn gen_literal(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, v: Value) {
        let reg = BcReg::get_reg(info, dst);
        info.literals.push(v);
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

    fn gen_bigint(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, bigint: BigInt) {
        self.gen_literal(info, dst, Value::new_bigint(bigint));
    }

    fn gen_float(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, f: f64) {
        self.gen_literal(info, dst, Value::new_float(f));
    }

    fn gen_symbol(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, sym: String) {
        let reg = BcReg::get_reg(info, dst);
        self.push(
            BcIr::Symbol(reg, IdentId::get_ident_id_from_string(sym)),
            Loc::default(),
        );
    }

    fn gen_string(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, s: String) {
        self.gen_literal(info, dst, Value::new_string(s));
    }

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, loc: Loc) {
        self.push(BcIr::Array(ret, src, len as u16), loc);
    }

    fn emit_hash(&mut self, ret: BcReg, args: BcReg, len: usize, loc: Loc) {
        self.push(
            BcIr::Hash {
                ret,
                args,
                len: len as u16,
            },
            loc,
        );
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
        let ret = BcReg::get_reg(info, ret);
        self.emit_array(ret, src, len, loc);
        Ok(())
    }

    fn gen_hash(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        ret: Option<BcReg>,
        nodes: Vec<(Node, Node)>,
        loc: Loc,
    ) -> Result<()> {
        let len = nodes.len();
        let old_reg = info.temp;
        let args = info.next_reg();
        for (k, v) in nodes {
            self.push_expr(ctx, info, k)?;
            self.push_expr(ctx, info, v)?;
        }
        info.temp = old_reg;
        let ret = BcReg::get_reg(info, ret);
        self.emit_hash(ret, args.into(), len, loc);
        Ok(())
    }

    fn gen_range(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        ret: Option<BcReg>,
        start: Node,
        end: Node,
        exclude_end: bool,
        loc: Loc,
    ) -> Result<()> {
        let ret = BcReg::get_reg(info, ret);
        let (start, end) = self.gen_binary_temp_expr(ctx, info, start, end)?;
        self.push(
            BcIr::Range {
                ret,
                start,
                end,
                exclude_end,
            },
            loc,
        );
        Ok(())
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
            self.gen_condbr(cond, else_pos, jmp_if_true, true);
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
            self.gen_condbr(cond, else_pos, jmp_if_true, false);
        }
        Ok(())
    }

    fn gen_teq_condbr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        lhs: Node,
        rhs: BcReg,
        cont_pos: usize,
        jmp_if_true: bool,
    ) -> Result<()> {
        let loc = lhs.loc;
        let lhs = self.gen_temp_expr(ctx, info, lhs)?;
        self.push(
            BcIr::Cmp(CmpKind::TEq, lhs, BinopMode::RR(lhs, rhs), true),
            loc,
        );
        self.gen_condbr(lhs, cont_pos, jmp_if_true, true);
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
        let reg = BcReg::get_reg(info, dst);
        self.push(BcIr::LoadConst(reg, toplevel, prefix, name), loc);
    }

    fn gen_load_ivar(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let reg = BcReg::get_reg(info, dst);
        self.push(BcIr::LoadIvar(reg, name), loc);
    }

    fn gen_load_gvar(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let ret = BcReg::get_reg(info, dst);
        self.push(BcIr::LoadGvar { ret, name }, loc);
    }

    fn gen_load_svar(&mut self, info: &mut ISeqInfo, dst: Option<BcReg>, id: u32, loc: Loc) {
        let ret = BcReg::get_reg(info, dst);
        self.push(BcIr::LoadSvar { ret, id }, loc);
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
    /// Evaluate *lhs* as a lvalue.
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
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_ident_id(name);
                LvalueKind::GlobalVar(name)
            }
            NodeKind::LocalVar(0, _) => LvalueKind::Other,
            NodeKind::LocalVar(outer, ident) => {
                let outer = *outer;
                let dst = info.refer_dynamic_local(outer, ident).into();
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
                let method = IdentId::get_ident_id(&format!("{method}="));
                LvalueKind::Send { recv, method }
            }
            NodeKind::SpecialVar(id) => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                return Err(MonorubyErr::cant_set_variable(
                    *id,
                    lhs.loc,
                    info.sourceinfo.clone(),
                ));
            }
            _ => return Err(MonorubyErr::unsupported_lhs(lhs, info.sourceinfo.clone())),
        };
        Ok(lhs)
    }

    fn gen_assign(&mut self, ctx: &mut FnStore, src: BcReg, lhs: LvalueKind, loc: Loc) {
        match lhs {
            LvalueKind::Const(name) => {
                self.push(BcIr::StoreConst(src, name), loc);
            }
            LvalueKind::InstanceVar(name) => {
                self.push(BcIr::StoreIvar(src, name), loc);
            }
            LvalueKind::GlobalVar(name) => {
                self.push(BcIr::StoreGvar { val: src, name }, loc);
            }
            LvalueKind::DynamicVar { outer, dst } => {
                self.push(BcIr::StoreDynVar { dst, outer, src }, loc);
            }
            LvalueKind::Index { base, index } => {
                self.push(BcIr::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Send { recv, method } => {
                let callid = ctx.add_callsite(method, 1, HashMap::default(), 0);
                self.gen_method_assign(callid, recv, src, loc);
            }
            LvalueKind::Other => unreachable!(),
        }
    }

    fn gen_neg(&mut self, info: &mut ISeqInfo, local: Option<BcReg>, loc: Loc) {
        match local {
            Some(local) => {
                self.push(
                    BcIr::Neg {
                        ret: local,
                        src: local,
                    },
                    loc,
                );
            }
            None => {
                let src = info.pop().into();
                let ret = info.push().into();
                self.push(BcIr::Neg { ret, src }, loc);
            }
        };
    }

    fn gen_not(&mut self, ret: BcReg, src: BcReg, loc: Loc) {
        self.push(BcIr::Not { ret, src }, loc);
    }

    fn handle_mode(&mut self, info: &mut ISeqInfo, use_mode: UseMode, src: BcReg) {
        match use_mode {
            UseMode::Ret => {
                self.gen_ret(info, Some(src));
            }
            UseMode::Use => {
                self.gen_temp_mov(info, src);
            }
            UseMode::NotUse => {}
        }
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        mut nodes: Vec<Node>,
        ret: Option<BcReg>,
        use_mode: UseMode,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(ctx, info, node, UseMode::NotUse)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, info, ret, last)?;
                match use_mode {
                    UseMode::Ret => {
                        self.gen_ret(info, ret.into());
                    }
                    UseMode::Use => {
                        self.gen_temp_mov(info, ret);
                    }
                    UseMode::NotUse => {}
                }
            }
            None => {
                self.gen_expr(ctx, info, last, use_mode)?;
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

    fn push_expr(&mut self, ctx: &mut FnStore, info: &mut ISeqInfo, expr: Node) -> Result<BcReg> {
        let ret = info.next_reg().into();
        self.gen_expr(ctx, info, expr, UseMode::Use)?;
        Ok(ret)
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        expr: Node,
        use_mode: UseMode,
    ) -> Result<()> {
        if !use_mode.use_val() {
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
                self.gen_symbol(info, None, sym);
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(info, None, bigint),
            NodeKind::Float(f) => self.gen_float(info, None, f),
            NodeKind::String(s) => self.gen_string(info, None, s),
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&expr);
                self.gen_literal(info, None, val);
            }
            NodeKind::RegExp(nodes, true) => {
                let val = self.const_regexp(info, nodes, loc)?;
                self.gen_literal(info, None, val);
            }
            NodeKind::Array(nodes, false) => self.gen_array(ctx, info, None, nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(ctx, info, None, nodes, loc)?,
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(ctx, info, None, start, end, exclude_end, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() != 1 {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        info.sourceinfo.clone(),
                    ));
                };
                self.gen_index(ctx, info, None, base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                        NodeKind::Float(f) => self.gen_float(info, None, -f),
                        _ => {
                            self.push_expr(ctx, info, rhs)?;
                            self.gen_neg(info, None, loc);
                        }
                    };
                }
                UnOp::Not => {
                    let src = self.push_expr(ctx, info, rhs)?;
                    let ret = info.push().into();
                    self.gen_not(ret, src, loc);
                }
                _ => {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported unop. {:?}", op),
                        loc,
                        info.sourceinfo.clone(),
                    ))
                }
            },
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                if let Some(local) = info.is_refer_local(&lhs) {
                    self.gen_binop(ctx, info, op, lhs, rhs, Some(local.into()), loc)?;
                    self.handle_mode(info, use_mode, local.into());
                    return Ok(());
                }
                let lhs_loc = lhs.loc;
                let temp = info.temp;
                // First, evaluate lvalue.
                let lhs_kind = self.eval_lvalue(ctx, info, &lhs)?;
                // Evaluate rvalue.
                let src = self.gen_binop(ctx, info, op, lhs, rhs, None, loc)?;
                // Assign rvalue to lvalue.
                self.gen_assign(ctx, src, lhs_kind, lhs_loc);
                info.temp = temp;
                self.handle_mode(info, use_mode, src);
                return Ok(());
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, info, op, lhs, rhs, None, loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(local) = info.is_assign_local(&lhs) {
                        self.gen_store_expr(ctx, info, local.into(), rhs)?;
                        self.handle_mode(info, use_mode, local.into());
                        return Ok(());
                    }
                    let temp = info.temp;
                    let lhs = self.eval_lvalue(ctx, info, &lhs)?;
                    let src = self.gen_expr_reg(ctx, info, rhs)?;
                    self.gen_assign(ctx, src, lhs, loc);
                    info.temp = temp;
                    self.handle_mode(info, use_mode, src);
                    return Ok(());
                } else {
                    return self.gen_mul_assign(ctx, info, mlhs, mrhs, use_mode);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                let local = info.refer_local(&ident);
                self.handle_mode(info, use_mode, local.into());
                return Ok(());
            }
            NodeKind::LocalVar(outer, ident) => {
                let ret = info.push().into();
                let src = info.refer_dynamic_local(outer, &ident).into();
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
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_ident_id_from_string(name);
                self.gen_load_gvar(info, None, name, loc);
            }
            NodeKind::SpecialVar(id) => {
                self.gen_load_svar(info, None, id as u32, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                return self.gen_method_call(
                    ctx,
                    info,
                    method,
                    Some(receiver),
                    arglist,
                    None,
                    use_mode,
                    loc,
                );
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                return self.gen_method_call(ctx, info, method, None, arglist, None, use_mode, loc);
            }
            NodeKind::Command(box expr) => {
                let mut arglist = ArgList::default();
                arglist.args.push(expr);
                return self.gen_method_call(
                    ctx,
                    info,
                    "`".to_string(),
                    None,
                    arglist,
                    None,
                    use_mode,
                    loc,
                );
            }
            NodeKind::Yield(arglist) => {
                let ret = if use_mode.use_val() {
                    Some(info.push().into())
                } else {
                    None
                };
                return self.gen_yield(ctx, info, arglist, ret, use_mode.is_ret(), loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                return self.gen_method_call(ctx, info, method, None, arglist, None, use_mode, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_opt_condbr(ctx, info, false, cond, else_pos)?;
                self.gen_expr(ctx, info, then_, use_mode)?;
                match use_mode {
                    UseMode::Ret => {}
                    UseMode::NotUse => {
                        self.gen_br(succ_pos);
                    }
                    UseMode::Use => {
                        self.gen_br(succ_pos);
                        info.pop();
                    }
                }
                self.apply_label(else_pos);
                self.gen_expr(ctx, info, else_, use_mode)?;
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
                    self.gen_while_begin_postfix(
                        ctx,
                        info,
                        cond_op,
                        cond,
                        body,
                        use_mode.use_val(),
                    )?;
                } else {
                    self.gen_while(ctx, info, cond_op, cond, body, use_mode.use_val())?;
                }
                if use_mode.is_ret() {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::For {
                param,
                box iter,
                body,
            } => {
                self.gen_for(ctx, info, param, iter, body, use_mode.use_val())?;
                if use_mode.is_ret() {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::Case {
                cond,
                when_,
                box else_,
            } => {
                let exit_pos = self.new_label();
                if let Some(box cond) = cond {
                    let ret = info.push().into();
                    let rhs = info.next_reg().into();
                    self.gen_expr(ctx, info, cond, UseMode::Use)?;
                    for branch in when_ {
                        let CaseBranch { box body, mut when } = branch;
                        let succ_pos = self.new_label();
                        if when.len() == 1 {
                            let when = when.remove(0);
                            self.gen_teq_condbr(ctx, info, when, rhs, succ_pos, false)?;
                        } else {
                            let then_pos = self.new_label();
                            for when in when {
                                self.gen_teq_condbr(ctx, info, when, rhs, then_pos, true)?;
                            }
                            self.gen_br(succ_pos);
                            self.apply_label(then_pos);
                        }
                        self.gen_store_expr(ctx, info, ret, body)?;

                        if use_mode.is_ret() {
                            self.push(BcIr::Ret(ret), Loc::default());
                        } else {
                            self.gen_br(exit_pos);
                        }

                        self.apply_label(succ_pos);
                    }
                    info.pop();
                    self.gen_store_expr(ctx, info, ret, else_)?;
                    match use_mode {
                        UseMode::Ret => {
                            self.gen_ret(info, None);
                        }
                        UseMode::NotUse => {
                            info.pop();
                        }
                        UseMode::Use => {}
                    }
                } else {
                    let temp = info.temp;
                    for branch in when_ {
                        info.temp = temp;
                        let CaseBranch { box body, mut when } = branch;
                        let succ_pos = self.new_label();
                        if when.len() == 1 {
                            let when = when.remove(0);
                            self.gen_opt_condbr(ctx, info, false, when, succ_pos)?;
                        } else {
                            let then_pos = self.new_label();
                            for when in when {
                                self.gen_opt_condbr(ctx, info, true, when, then_pos)?;
                            }
                            self.gen_br(succ_pos);
                            self.apply_label(then_pos);
                        }
                        self.gen_expr(ctx, info, body, use_mode)?;
                        if !use_mode.is_ret() {
                            self.gen_br(exit_pos);
                        }
                        self.apply_label(succ_pos);
                    }
                    info.temp = temp;
                    self.gen_expr(ctx, info, else_, use_mode)?;
                }
                self.apply_label(exit_pos);
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
                    self.gen_expr(ctx, info, expr, UseMode::Ret)?;
                }
                assert_ne!(use_mode, UseMode::Use);
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, info, nodes, None, use_mode)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_,
                ensure,
            } => {
                assert!(rescue.is_empty());

                if let Some(box else_) = else_ {
                    self.gen_expr(ctx, info, body, UseMode::NotUse)?;
                    self.gen_expr(ctx, info, else_, use_mode)?;
                } else {
                    self.gen_expr(ctx, info, body, use_mode)?;
                }
                if let Some(box ensure) = ensure {
                    self.gen_expr(ctx, info, ensure, UseMode::NotUse)?;
                }
                return Ok(());
            }
            NodeKind::MethodDef(name, block) => {
                self.gen_method_def(ctx, info, name.clone(), block, loc)?;
                if use_mode.use_val() {
                    self.gen_symbol(info, None, name);
                }
                if use_mode.is_ret() {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::SingletonMethodDef(box obj, name, block) => {
                self.gen_expr(ctx, info, obj, UseMode::Use)?;
                self.gen_singleton_method_def(ctx, info, name.clone(), block, loc)?;
                if use_mode.use_val() {
                    self.gen_symbol(info, None, name);
                }
                if use_mode.is_ret() {
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
                let dst = if use_mode.use_val() {
                    Some(info.push().into())
                } else {
                    None
                };
                self.gen_class_def(
                    ctx,
                    info,
                    name,
                    base,
                    superclass,
                    *block_info.body,
                    dst,
                    is_module,
                    loc,
                )?;
                if use_mode.is_ret() {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::SingletonClassDef {
                box singleton,
                info: block_info,
            } => {
                let dst = if use_mode.use_val() {
                    Some(info.push().into())
                } else {
                    None
                };
                self.gen_singleton_class_def(ctx, info, singleton, *block_info.body, dst, loc)?;
                if use_mode.is_ret() {
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
                let ret = if use_mode.use_val() {
                    Some(info.push().into())
                } else {
                    None
                };
                self.push(BcIr::ConcatStr(ret, arg, len), Loc::default());
                if use_mode.is_ret() {
                    self.gen_ret(info, None);
                }
                return Ok(());
            }
            NodeKind::AliasMethod(box new, box old) => {
                match (new.kind, old.kind) {
                    (NodeKind::Symbol(new), NodeKind::Symbol(old)) => {
                        self.gen_symbol(info, None, new);
                        self.gen_symbol(info, None, old);
                        let old = info.pop().into();
                        let new = info.pop().into();
                        self.push(BcIr::AliasMethod { new, old }, loc);
                    }
                    _ => unimplemented!(),
                };
                match use_mode {
                    UseMode::Ret => {
                        self.gen_nil(info, None);
                        self.gen_ret(info, None);
                    }
                    UseMode::NotUse => {}
                    UseMode::Use => {
                        self.gen_nil(info, None);
                    }
                }
                return Ok(());
            }
            _ => return Err(MonorubyErr::unsupported_node(expr, info.sourceinfo.clone())),
        }
        match use_mode {
            UseMode::Ret => {
                self.gen_ret(info, None);
            }
            UseMode::NotUse => {
                info.pop();
            }
            UseMode::Use => {}
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
            NodeKind::Symbol(sym) => self.gen_symbol(info, Some(dst), sym),
            NodeKind::Bignum(bigint) => self.gen_bigint(info, Some(dst), bigint),
            NodeKind::Float(f) => self.gen_float(info, Some(dst), f),
            NodeKind::String(s) => self.gen_string(info, Some(dst), s),
            NodeKind::Array(nodes, false) => self.gen_array(ctx, info, Some(dst), nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(ctx, info, Some(dst), nodes, loc)?,
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&rhs);
                self.gen_literal(info, Some(dst), val);
            }
            NodeKind::RegExp(nodes, true) => {
                let val = self.const_regexp(info, nodes, loc)?;
                self.gen_literal(info, Some(dst), val);
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(ctx, info, Some(dst), start, end, exclude_end, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() != 1 {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        info.sourceinfo.clone(),
                    ));
                };
                self.gen_index(ctx, info, Some(dst), base, index.remove(0), loc)?;
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.gen_integer(info, Some(dst), -i),
                        NodeKind::Float(f) => self.gen_float(info, Some(dst), -f),
                        _ => {
                            self.gen_store_expr(ctx, info, dst, rhs)?;
                            self.gen_neg(info, Some(dst), loc);
                        }
                    };
                }
                UnOp::Not => {
                    self.gen_store_expr(ctx, info, dst, rhs)?;
                    self.gen_not(dst, dst, loc);
                }
                _ => {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported unop. {:?}", op),
                        loc,
                        info.sourceinfo.clone(),
                    ))
                }
            },
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
                        self.gen_assign(ctx, dst, lhs, loc);
                        info.temp = temp;
                    }
                } else {
                    self.gen_mul_assign(ctx, info, mlhs, mrhs, UseMode::Use)?;
                    let temp = info.pop().into();
                    self.gen_mov(dst, temp);
                }
            }
            NodeKind::LocalVar(0, ident) => {
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
                self.gen_method_call(
                    ctx,
                    info,
                    method,
                    Some(receiver),
                    arglist,
                    ret,
                    UseMode::Use,
                    loc,
                )?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(dst);
                self.gen_method_call(ctx, info, method, None, arglist, ret, UseMode::Use, loc)?;
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(ctx, info, nodes, Some(dst), UseMode::NotUse)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info: block_info,
                is_module,
            } => {
                let dst = Some(dst);
                self.gen_class_def(
                    ctx,
                    info,
                    name,
                    base,
                    superclass,
                    *block_info.body,
                    dst,
                    is_module,
                    loc,
                )?;
            }
            _ => {
                let ret = self.push_expr(ctx, info, rhs)?;
                self.gen_mov(dst, ret);
                info.pop();
            }
        };
        Ok(())
    }

    fn const_regexp(&self, info: &ISeqInfo, nodes: Vec<Node>, loc: Loc) -> Result<Value> {
        let mut string = String::new();
        for node in nodes {
            match &node.kind {
                NodeKind::String(s) => string += s,
                _ => unreachable!(),
            }
        }
        match string.pop().unwrap() {
            'i' => string.insert_str(0, "(?mi)"),
            'm' => string.insert_str(0, "(?ms)"),
            'x' => string.insert_str(0, "(?mx)"),
            'o' => string.insert_str(0, "(?mo)"),
            '-' => string.insert_str(0, "(?m)"),
            _ => {
                return Err(MonorubyErr::syntax(
                    "Illegal internal regexp expression.".to_string(),
                    loc,
                    info.sourceinfo.clone(),
                ))
            }
        };
        let re = match RegexpInner::new(string) {
            Ok(re) => re,
            Err(err) => return Err(MonorubyErr::syntax(err, loc, info.sourceinfo.clone())),
        };
        Ok(Value::new_regexp(re))
    }

    fn gen_method_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        name: String,
        block: BlockInfo,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_method(Some(name.clone()), block, info.sourceinfo.clone())?;
        let name = IdentId::get_ident_id_from_string(name);
        self.push(BcIr::MethodDef { name, func_id }, loc);
        Ok(())
    }

    fn gen_singleton_method_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        name: String,
        block: BlockInfo,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_method(Some(name.clone()), block, info.sourceinfo.clone())?;
        let obj = info.pop().into();
        let name = IdentId::get_ident_id_from_string(name);
        self.push(BcIr::SingletonMethodDef { obj, name, func_id }, loc);
        Ok(())
    }

    fn gen_class_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        name: String,
        base: Option<Box<Node>>,
        superclass: Option<Box<Node>>,
        body: Node,
        dst: Option<BcReg>,
        is_module: bool,
        loc: Loc,
    ) -> Result<()> {
        if let Some(base) = base {
            return Err(MonorubyErr::unsupported_feature(
                &format!("base in class def. {:?}", base.kind),
                loc,
                info.sourceinfo.clone(),
            ));
        };
        let func_id = ctx.add_classdef(Some(name.clone()), body, info.sourceinfo.clone());
        let name = IdentId::get_ident_id_from_string(name);
        let superclass = match superclass {
            Some(superclass) => Some(self.gen_temp_expr(ctx, info, *superclass)?),
            None => None,
        };
        self.push(
            if is_module {
                BcIr::ModuleDef {
                    ret: dst,
                    name,
                    func_id,
                }
            } else {
                BcIr::ClassDef {
                    ret: dst,
                    superclass,
                    name,
                    func_id,
                }
            },
            loc,
        );
        Ok(())
    }

    fn gen_singleton_class_def(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        base: Node,
        body: Node,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<()> {
        let func_id = ctx.add_classdef(None, body, info.sourceinfo.clone());
        let base = self.gen_temp_expr(ctx, info, base)?;
        self.push(
            BcIr::SingletonClassDef {
                ret: dst,
                base,
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
            if let NodeKind::Splat(box expr) = arg.kind {
                let src = self.push_expr(ctx, info, expr)?;
                self.push(BcIr::Splat(src), arg.loc);
            } else {
                self.push_expr(ctx, info, arg)?;
            }
        }
        Ok(arg)
    }

    fn gen_dummy_init(&mut self, is_block: bool) {
        self.push(
            if is_block {
                BcIr::InitBlock(FnInitInfo::default())
            } else {
                BcIr::InitMethod(FnInitInfo::default())
            },
            Loc::default(),
        );
    }

    fn gen_expand_array(&mut self, src: usize, dst: usize, len: usize) {
        self.push(
            BcIr::ExpandArray(
                BcLocal(src as u16).into(),
                BcLocal(dst as u16).into(),
                len as u16,
            ),
            Loc::default(),
        );
    }

    fn replace_init(&mut self, info: &ISeqInfo) {
        let fninfo = FnInitInfo::new(info);
        self.ir[0] = (
            if info.is_block_style {
                BcIr::InitBlock(fninfo)
            } else {
                BcIr::InitMethod(fninfo)
            },
            Loc::default(),
        );
    }
}
enum RecvKind {
    SelfValue,
    Local(BcReg),
    Temp,
}

impl IrContext {
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
        let (dst, mode) = if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, info, dst, lhs)?;
            (dst, BinopMode::RI(lhs, i))
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
            (dst, BinopMode::RR(lhs, rhs))
        };
        self.push(BcIr::Cmp(kind, dst, mode, optimizable), loc);
        Ok(dst)
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
        use_mode: UseMode,
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

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
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
                self.gen_assign(ctx, src, kind, loc);
            }
            temp_reg += 1;
        }
        info.temp = temp;

        // Generate return value if needed.
        if use_mode.use_val() {
            let ret = info.push().into();
            self.emit_array(ret, rhs_reg.into(), mrhs_len, loc);
        }
        if use_mode.is_ret() {
            self.gen_ret(info, None);
        }
        Ok(())
    }

    fn gen_for(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        param: Vec<(usize, String)>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        assert_eq!(1, param.len());
        let loc = iter.loc;
        if let NodeKind::Range {
            box start,
            box end,
            exclude_end,
            ..
        } = iter.kind
        {
            let counter = info.assign_local(&param[0].1);
            let break_pos = self.new_label();
            self.loops.push((
                LoopKind::For,
                break_pos,
                match use_value {
                    true => Some(info.next_reg().into()),
                    false => None,
                },
            ));
            // +------+
            // | iter | (when use_value)
            // +------+
            // | end  |
            // +------+
            // | dst  |
            // +------+
            let loop_entry = self.new_label();
            let loop_exit = self.new_label();
            self.gen_store_expr(ctx, info, counter.into(), start)?;
            let end = if use_value {
                let iter = info.push();
                let end = self.push_expr(ctx, info, end)?;
                self.push(
                    BcIr::Range {
                        ret: iter.into(),
                        start: counter.into(),
                        end,
                        exclude_end,
                    },
                    loc,
                );
                end
            } else {
                self.push_expr(ctx, info, end)?
            };
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
                    BinopMode::RR(counter.into(), end),
                    true,
                ),
                loc,
            );
            self.gen_condbr(dst, loop_exit, true, true);
            info.pop(); // pop *dst*

            self.gen_expr(ctx, info, *body.body, UseMode::NotUse)?;

            self.push(
                BcIr::BinOp(
                    BinOpK::Add,
                    counter.into(),
                    BinopMode::RI(counter.into(), 1),
                ),
                loc,
            );
            self.gen_br(loop_entry);

            self.apply_label(loop_exit);
            info.pop(); // pop *end*

            self.loops.pop().unwrap();
            self.apply_label(break_pos);
            self.push(BcIr::LoopEnd, loc);
        } else {
            let use_mode = if use_value {
                UseMode::Use
            } else {
                UseMode::NotUse
            };
            self.gen_each(ctx, info, param, iter, body, None, use_mode, loc)?;
        }
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
        self.gen_expr(ctx, info, body, UseMode::NotUse)?;
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

    // in postfix while, we must evaluate the body expr once at least.
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
        self.gen_expr(ctx, info, body, UseMode::NotUse)?;
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
