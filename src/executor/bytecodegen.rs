use super::*;
use num::BigInt;
use paste::paste;
use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, Node, NodeKind, RescueEntry,
    SourceInfoRef, UnOp,
};

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
    LocalVar { dst: BcReg },
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

/// Infomation for a call site.
#[derive(Debug, Clone)]
struct CallSite {
    /// Name of method. (None for *super*)
    name: Option<IdentId>,
    /// Number of positional arguments.
    arg_num: usize,
    /// *BcTemp* of keyword arguments.
    kw: Option<KeywordArgs>,
    /// Positions of splat arguments.
    splat_pos: Vec<usize>,
}

///
/// keyword arguments information in *Callsite*.
///
#[derive(Debug, Clone)]
struct KeywordArgs {
    kw_pos: BcReg,
    /// Names and positions of keyword arguments.
    kw_args: HashMap<IdentId, usize>,
    hash_splat_pos: Vec<BcReg>,
}

#[derive(Debug, Clone)]
enum Functions {
    Method {
        name: Option<IdentId>,
        info: BlockInfo,
    },
    ClassDef {
        name: Option<IdentId>,
        info: BlockInfo,
    },
    Block {
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
    },
}

#[derive(Debug)]
pub(crate) struct IrContext {
    /// ID of this function.
    id: FuncId,
    /// ID of the mother method.
    mother: Option<(FuncId, ParamsInfo)>,
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// loop information.
    loops: Vec<(LoopKind, usize, Option<BcReg>)>, // (kind, label for exit, return register)
    /// local variables.
    locals: HashMap<IdentId, u16>,
    /// outer local variables. (dynamic_locals, block_param)
    outer_locals: Vec<(HashMap<IdentId, u16>, Option<IdentId>)>,
    /// literal values. (for GC)
    literals: Vec<Value>,
    /// The name of the block param.
    block_param: Option<IdentId>,
    /// The current register id.
    temp: u16,
    /// The number of temporary registers.
    temp_num: u16,
    /// The number of non-temporary registers.
    non_temp_num: u16,
    /// Source info.
    sourceinfo: SourceInfoRef,
    /// Exception jump table.
    exception_table: Vec<(
        std::ops::Range<usize>,
        usize,
        Option<BcReg>,
        Option<(BcReg, usize, (Loc, SourceInfoRef))>,
    )>,
    /// Call site info.
    callsites: Vec<CallSite>,
    /// Offset of call site info.
    callsite_offset: usize,
    /// Func info.
    functions: Vec<Functions>,
    /// Offset of func info.
    functions_offset: usize,
}

impl IrContext {
    pub(crate) fn compile_func(func_id: FuncId, ctx: &mut FnStore) -> Result<()> {
        let CompileInfo {
            ast,
            for_param_info,
            keyword_initializers,
            destruct_info: expand_info,
            optional_info,
        } = ctx.get_init();
        let info = ctx[func_id].as_ruby_func();
        let mother = info
            .mother
            .map(|fid| (fid, ctx[fid].as_ruby_func().args.clone()));
        let mut ir = IrContext::new(info, mother, ctx.callsite_offset(), ctx.functions_offset());
        // arguments preparation
        for ForParamInfo {
            dst_outer,
            dst_reg,
            src_reg,
        } in for_param_info
        {
            ir.emit(
                BcIr::StoreDynVar {
                    dst: (dst_reg).into(),
                    outer: dst_outer,
                    src: BcLocal(src_reg as u16).into(),
                },
                Loc::default(),
            );
        }
        for DestructureInfo { src, dst, len } in expand_info {
            ir.gen_expand_array(src, dst, len);
        }
        for OptionalInfo { local, initializer } in optional_info {
            let local = local.into();
            let next = ir.new_label();
            ir.emit_check_local(local, next);
            ir.gen_store_expr(local, initializer)?;
            ir.apply_label(next);
        }
        let kw_reg = info.pos_num();
        // keyword args preparation
        for (id, initializer) in keyword_initializers.into_iter().enumerate() {
            let local = BcLocal((kw_reg + id) as u16).into();
            let next = ir.new_label();
            ir.emit_check_local(local, next);
            if let Some(box init) = initializer {
                ir.gen_store_expr(local, init)?;
            } else {
                ir.emit_nil(Some(local));
            }
            ir.apply_label(next);
        }
        ir.gen_expr(ast, UseMode::Ret)?;
        ir.replace_init(info);
        assert_eq!(0, ir.temp);
        ir.into_bytecode(ctx, func_id)?;
        Ok(())
    }
}

impl IrContext {
    fn new(
        info: &ISeqInfo,
        mother: Option<(FuncId, ParamsInfo)>,
        callsite_offset: usize,
        functions_offset: usize,
    ) -> Self {
        let mut ir = Self {
            id: info.id(),
            mother,
            ir: vec![],
            labels: vec![],
            loops: vec![],
            locals: HashMap::default(),
            outer_locals: info.outer_locals.clone(),
            literals: vec![],
            block_param: info.block_param_name(),
            temp: 0,
            temp_num: 0,
            non_temp_num: 0,
            sourceinfo: info.sourceinfo.clone(),
            exception_table: vec![],
            callsites: vec![],
            callsite_offset,
            functions: vec![],
            functions_offset,
        };
        info.args.args_names.iter().for_each(|name| {
            ir.add_local(*name);
        });
        ir.gen_dummy_init(info.is_block_style);

        ir
    }

    fn add_callsite(
        &mut self,
        name: impl Into<Option<IdentId>>,
        arg_num: usize,
        kw: Option<KeywordArgs>,
        splat_pos: Vec<usize>,
    ) -> CallSiteId {
        let name = name.into();
        let id = self.callsite_offset + self.callsites.len();
        self.callsites.push(CallSite {
            name,
            arg_num,
            kw,
            splat_pos,
        });
        CallSiteId(id as u32)
    }

    fn add_method(&mut self, name: Option<IdentId>, info: BlockInfo) -> FuncId {
        let id = self.functions_offset + self.functions.len();
        self.functions.push(Functions::Method { name, info });
        FuncId::new(id as _)
    }

    fn add_classdef(&mut self, name: Option<IdentId>, info: BlockInfo) -> FuncId {
        let id = self.functions_offset + self.functions.len();
        self.functions.push(Functions::ClassDef { name, info });
        FuncId::new(id as _)
    }

    fn add_block(
        &mut self,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
    ) -> FuncId {
        let id = self.functions_offset + self.functions.len();
        self.functions.push(Functions::Block {
            mother,
            outer,
            optional_params,
            info,
        });
        FuncId::new(id as _)
    }
}

//
// temporary registers handling.
//
impl IrContext {
    /// get a number of registers.
    fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    /// get the next register id.
    fn next_reg(&mut self) -> BcTemp {
        self.push();
        self.pop()
    }

    fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.temp_num {
            self.temp_num = self.temp;
        }
        reg
    }

    fn pop(&mut self) -> BcTemp {
        self.temp -= 1;
        BcTemp(self.temp)
    }

    fn popn(&mut self, len: usize) {
        self.temp -= len as u16;
    }

    fn get_reg(&mut self, reg: Option<BcReg>) -> BcReg {
        match reg {
            Some(local) => local,
            None => self.push().into(),
        }
    }

    fn get_index(&self, reg: &BcReg) -> SlotId {
        let id = match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.non_temp_num + i.0,
            BcReg::Local(i) => 1 + i.0,
        };
        SlotId(id)
    }
}

//
// local variables handling.
//
impl IrContext {
    fn get_locals(&self) -> Vec<(HashMap<IdentId, u16>, Option<IdentId>)> {
        let mut locals = vec![(self.locals.clone(), self.block_param.clone())];
        locals.extend_from_slice(&self.outer_locals);
        locals
    }

    /// get the outer block argument name.
    fn outer_block_param_name(&self, outer: usize) -> Option<IdentId> {
        self.outer_locals[outer - 1].1
    }

    fn assign_local(&mut self, name: IdentId) -> BcLocal {
        match self.locals.get(&name) {
            Some(local) => BcLocal(*local),
            None => self.add_local(name),
        }
    }

    fn refer_local(&mut self, ident: &str) -> BcLocal {
        let name = IdentId::get_id(ident);
        match self.locals.get(&name) {
            Some(local) => BcLocal(*local),
            None => panic!("undefined local var `{}`", ident),
        }
    }

    fn refer_dynamic_local(&self, outer: usize, name: IdentId) -> BcLocal {
        BcLocal(
            *self.outer_locals[outer - 1]
                .0
                .get(&name)
                .unwrap_or_else(|| {
                    panic!(
                        "Bytecodegen: dynamic local was not found. {outer} {name} {:?} {:?}",
                        self.outer_locals, self.locals
                    )
                }),
        )
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: impl Into<Option<IdentId>>) -> BcLocal {
        let local = self.non_temp_num;
        if let Some(ident) = ident.into() {
            assert!(self.locals.insert(ident, local).is_none());
        };
        self.non_temp_num += 1;
        BcLocal(local)
    }

    fn is_assign_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(0, name) = &node.kind {
            let name = IdentId::get_id(name);
            Some(self.assign_local(name))
        } else {
            None
        }
    }

    fn is_refer_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(0, name) = &node.kind {
            Some(self.refer_local(name))
        } else {
            None
        }
    }
}

//
// emit bytecode ir.
//
impl IrContext {
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

    fn emit(&mut self, op: BcIr, loc: Loc) {
        self.ir.push((op, loc));
    }

    fn emit_ret(&mut self, src: Option<BcReg>) {
        let ret = match src {
            Some(ret) => ret,
            None => self.pop().into(),
        };
        //assert_eq!(0, self.temp);
        self.emit(BcIr::Ret(ret), Loc::default());
    }

    fn emit_mov(&mut self, dst: BcReg, src: BcReg) {
        if dst != src {
            self.emit(BcIr::Mov(dst, src), Loc::default());
        }
    }

    fn emit_temp_mov(&mut self, src: BcReg) {
        let dst = self.push();
        self.emit_mov(dst.into(), src);
    }

    fn emit_br(&mut self, jmp_pos: usize) {
        self.emit(BcIr::Br(jmp_pos), Loc::default());
    }

    fn emit_condbr(&mut self, cond: BcReg, jmp_pos: usize, jmp_if_true: bool, optimizable: bool) {
        self.emit(
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

    fn emit_check_local(&mut self, local: BcReg, else_pos: usize) {
        self.emit(BcIr::CheckLocal(local, else_pos), Loc::default());
    }

    fn emit_nil(&mut self, dst: Option<BcReg>) {
        let reg = match dst {
            Some(dst) => dst,
            None => self.push().into(),
        };
        self.emit(BcIr::Nil(reg), Loc::default());
    }

    fn emit_literal(&mut self, dst: Option<BcReg>, v: Value) {
        let reg = self.get_reg(dst);
        self.literals.push(v);
        self.emit(BcIr::Literal(reg, v), Loc::default());
    }

    fn emit_integer(&mut self, dst: Option<BcReg>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local,
                None => self.push().into(),
            };
            self.emit(BcIr::Integer(reg, i), Loc::default());
        } else {
            self.emit_literal(dst, Value::new_integer(i));
        }
    }

    fn emit_bigint(&mut self, dst: Option<BcReg>, bigint: BigInt) {
        self.emit_literal(dst, Value::new_bigint(bigint));
    }

    fn emit_float(&mut self, dst: Option<BcReg>, f: f64) {
        self.emit_literal(dst, Value::new_float(f));
    }

    fn emit_symbol(&mut self, dst: Option<BcReg>, sym: IdentId) {
        let reg = self.get_reg(dst);
        self.emit(BcIr::Symbol(reg, sym), Loc::default());
    }

    fn emit_string(&mut self, dst: Option<BcReg>, s: String) {
        self.emit_literal(dst, Value::new_string(s));
    }

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, loc: Loc) {
        self.emit(BcIr::Array(ret, src, len as u16), loc);
    }

    fn emit_hash(&mut self, ret: BcReg, args: BcReg, len: usize, loc: Loc) {
        self.emit(
            BcIr::Hash {
                ret,
                args,
                len: len as u16,
            },
            loc,
        );
    }

    fn gen_array(&mut self, ret: Option<BcReg>, nodes: Vec<Node>, loc: Loc) -> Result<()> {
        let (src, len, _) = self.gen_args(nodes)?;
        self.popn(len);
        let ret = self.get_reg(ret);
        self.emit_array(ret, src, len, loc);
        Ok(())
    }

    fn gen_hash(&mut self, ret: Option<BcReg>, nodes: Vec<(Node, Node)>, loc: Loc) -> Result<()> {
        let len = nodes.len();
        let old_reg = self.temp;
        let args = self.next_reg();
        for (k, v) in nodes {
            self.push_expr(k)?;
            self.push_expr(v)?;
        }
        self.temp = old_reg;
        let ret = self.get_reg(ret);
        self.emit_hash(ret, args.into(), len, loc);
        Ok(())
    }

    fn gen_range(
        &mut self,
        ret: Option<BcReg>,
        start: Node,
        end: Node,
        exclude_end: bool,
        loc: Loc,
    ) -> Result<()> {
        let ret = self.get_reg(ret);
        let (start, end) = self.gen_binary_temp_expr(start, end)?;
        self.emit(
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

    fn emit_load_const(
        &mut self,
        dst: Option<BcReg>,
        toplevel: bool,
        name: String,
        prefix: Vec<String>,
        loc: Loc,
    ) {
        let name = IdentId::get_id_from_string(name);
        let prefix = prefix
            .into_iter()
            .map(IdentId::get_id_from_string)
            .collect();
        let reg = self.get_reg(dst);
        self.emit(BcIr::LoadConst(reg, toplevel, prefix, name), loc);
    }

    fn emit_load_ivar(&mut self, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let reg = self.get_reg(dst);
        self.emit(BcIr::LoadIvar(reg, name), loc);
    }

    fn emit_load_gvar(&mut self, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BcIr::LoadGvar { ret, name }, loc);
    }

    fn emit_load_svar(&mut self, dst: Option<BcReg>, id: u32, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BcIr::LoadSvar { ret, id }, loc);
    }

    fn emit_neg(&mut self, local: Option<BcReg>, loc: Loc) {
        match local {
            Some(local) => {
                self.emit(
                    BcIr::Neg {
                        ret: local,
                        src: local,
                    },
                    loc,
                );
            }
            None => {
                let src = self.pop().into();
                let ret = self.push().into();
                self.emit(BcIr::Neg { ret, src }, loc);
            }
        };
    }

    fn emit_not(&mut self, ret: BcReg, src: BcReg, loc: Loc) {
        self.emit(BcIr::Not { ret, src }, loc);
    }

    fn gen_index(&mut self, ret: Option<BcReg>, base: Node, index: Node, loc: Loc) -> Result<()> {
        let (base, idx) = self.gen_binary_temp_expr(base, index)?;
        let ret = match ret {
            None => self.push().into(),
            Some(local) => local,
        };
        self.emit(BcIr::Index(ret, base, idx), loc);
        Ok(())
    }

    ///
    /// Evaluate *lhs* as a lvalue.
    ///
    fn eval_lvalue(&mut self, lhs: &Node) -> Result<LvalueKind> {
        let lhs = match &lhs.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } if !toplevel && parent.is_none() && prefix.is_empty() => {
                let name = IdentId::get_id(name);
                LvalueKind::Const(name)
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id(name);
                LvalueKind::InstanceVar(name)
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id(name);
                LvalueKind::GlobalVar(name)
            }
            NodeKind::LocalVar(0, name) | NodeKind::Ident(name) => {
                let name = IdentId::get_id(name);
                let dst = self.assign_local(name).into();
                LvalueKind::LocalVar { dst }
            }
            NodeKind::LocalVar(outer, ident) => {
                let outer = *outer;
                let name = IdentId::get_id(ident);
                let dst = self.refer_dynamic_local(outer, name).into();
                LvalueKind::DynamicVar { outer, dst }
            }
            NodeKind::Index { box base, index } => {
                assert_eq!(1, index.len());
                let index = index[0].clone();
                let base = self.gen_expr_reg(base.clone())?;
                let index = self.gen_expr_reg(index)?;
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
                let recv = self.gen_expr_reg(receiver.clone())?;
                let method = IdentId::get_id_from_string(format!("{method}="));
                LvalueKind::Send { recv, method }
            }
            NodeKind::SpecialVar(id) => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                return Err(MonorubyErr::cant_set_variable(
                    *id,
                    lhs.loc,
                    self.sourceinfo.clone(),
                ));
            }
            _ => return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone())),
        };
        Ok(lhs)
    }

    fn gen_assign(&mut self, src: BcReg, lhs: LvalueKind, loc: Loc) {
        match lhs {
            LvalueKind::Const(name) => {
                self.emit(BcIr::StoreConst(src, name), loc);
            }
            LvalueKind::InstanceVar(name) => {
                self.emit(BcIr::StoreIvar(src, name), loc);
            }
            LvalueKind::GlobalVar(name) => {
                self.emit(BcIr::StoreGvar { val: src, name }, loc);
            }
            LvalueKind::DynamicVar { outer, dst } => {
                self.emit(BcIr::StoreDynVar { dst, outer, src }, loc);
            }
            LvalueKind::Index { base, index } => {
                self.emit(BcIr::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Send { recv, method } => {
                let callid = self.add_callsite(method, 1, None, vec![]);
                self.gen_method_assign(callid, recv, src, loc);
            }
            LvalueKind::LocalVar { dst } => {
                self.emit_mov(dst, src);
            }
        }
    }

    fn handle_mode(&mut self, use_mode: UseMode, src: BcReg) {
        match use_mode {
            UseMode::Ret => {
                self.emit_ret(Some(src));
            }
            UseMode::Use => {
                self.emit_temp_mov(src);
            }
            UseMode::NotUse => {}
        }
    }

    fn gen_comp_stmts(
        &mut self,
        mut nodes: Vec<Node>,
        ret: Option<BcReg>,
        use_mode: UseMode,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(node, UseMode::NotUse)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ret, last)?;
                match use_mode {
                    UseMode::Ret => {
                        self.emit_ret(ret.into());
                    }
                    UseMode::Use => {
                        self.emit_temp_mov(ret);
                    }
                    UseMode::NotUse => {}
                }
            }
            None => {
                self.gen_expr(last, use_mode)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_expr_reg(&mut self, expr: Node) -> Result<BcReg> {
        Ok(match self.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => self.push_expr(expr)?,
        })
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(&mut self, expr: Node) -> Result<BcReg> {
        Ok(match self.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => {
                self.push_expr(expr)?;
                self.pop().into()
            }
        })
    }

    fn gen_binary_temp_expr(&mut self, lhs: Node, rhs: Node) -> Result<(BcReg, BcReg)> {
        match (self.is_refer_local(&lhs), self.is_refer_local(&rhs)) {
            (None, None) => {
                let lhs = self.push_expr(lhs)?;
                let rhs = self.push_expr(rhs)?;
                self.temp -= 2;
                Ok((lhs, rhs))
            }
            _ => {
                let lhs = self.gen_temp_expr(lhs)?;
                let rhs = self.gen_temp_expr(rhs)?;
                Ok((lhs, rhs))
            }
        }
    }

    fn push_expr(&mut self, expr: Node) -> Result<BcReg> {
        let ret = self.next_reg().into();
        self.gen_expr(expr, UseMode::Use)?;
        Ok(ret)
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(&mut self, expr: Node, use_mode: UseMode) -> Result<()> {
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
            NodeKind::Nil => self.emit_nil(None),
            NodeKind::Bool(b) => self.emit_literal(None, Value::bool(b)),
            NodeKind::SelfValue => self.emit_temp_mov(BcReg::Self_),
            NodeKind::Integer(i) => {
                self.emit_integer(None, i);
            }
            NodeKind::Symbol(sym) => {
                let sym = IdentId::get_id_from_string(sym);
                self.emit_symbol(None, sym);
            }
            NodeKind::Bignum(bigint) => self.emit_bigint(None, bigint),
            NodeKind::Float(f) => self.emit_float(None, f),
            NodeKind::String(s) => self.emit_string(None, s),
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&expr);
                self.emit_literal(None, val);
            }
            NodeKind::RegExp(nodes, true) => {
                let val = self.const_regexp(nodes, loc)?;
                self.emit_literal(None, val);
            }
            NodeKind::Array(nodes, false) => self.gen_array(None, nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(None, nodes, loc)?,
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(None, start, end, exclude_end, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() == 1 {
                    self.gen_index(None, base, index.remove(0), loc)?;
                } else if index.len() == 2 {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        None,
                        use_mode,
                        loc,
                    )?;
                    return Ok(());
                } else {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        self.sourceinfo.clone(),
                    ));
                };
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        //NodeKind::Integer(i) => self.gen_integer(ctx, info, None, -i),
                        NodeKind::Float(f) => self.emit_float(None, -f),
                        _ => {
                            self.push_expr(rhs)?;
                            self.emit_neg(None, loc);
                        }
                    };
                }
                UnOp::Not => {
                    let src = self.push_expr(rhs)?;
                    let ret = self.push().into();
                    self.emit_not(ret, src, loc);
                }
                _ => {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported unop. {:?}", op),
                        loc,
                        self.sourceinfo.clone(),
                    ))
                }
            },
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                if let Some(local) = self.is_refer_local(&lhs) {
                    self.gen_binop(op, lhs, rhs, Some(local.into()), loc)?;
                    self.handle_mode(use_mode, local.into());
                    return Ok(());
                }
                let lhs_loc = lhs.loc;
                let temp = self.temp;
                // First, evaluate lvalue.
                let lhs_kind = self.eval_lvalue(&lhs)?;
                // Evaluate rvalue.
                let src = self.gen_binop(op, lhs, rhs, None, loc)?;
                // Assign rvalue to lvalue.
                self.gen_assign(src, lhs_kind, lhs_loc);
                self.temp = temp;
                self.handle_mode(use_mode, src);
                return Ok(());
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, None, loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(local) = self.is_assign_local(&lhs) {
                        self.gen_store_expr(local.into(), rhs)?;
                        self.handle_mode(use_mode, local.into());
                        return Ok(());
                    }
                    let temp = self.temp;
                    let lhs = self.eval_lvalue(&lhs)?;
                    let src = self.gen_expr_reg(rhs)?;
                    self.gen_assign(src, lhs, loc);
                    self.temp = temp;
                    self.handle_mode(use_mode, src);
                    return Ok(());
                } else {
                    return self.gen_mul_assign(mlhs, mrhs, use_mode);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                let local = self.refer_local(&ident);
                self.handle_mode(use_mode, local.into());
                return Ok(());
            }
            NodeKind::LocalVar(outer, ident) => {
                let ret = self.push().into();
                let name = IdentId::get_id_from_string(ident);
                let src = self.refer_dynamic_local(outer, name).into();
                self.emit(BcIr::LoadDynVar { ret, src, outer }, loc);
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.emit_load_const(None, toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_ivar(None, name, loc);
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_gvar(None, name, loc);
            }
            NodeKind::SpecialVar(id) => {
                self.emit_load_svar(None, id as u32, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, Some(receiver), arglist, None, use_mode, loc);
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::Super(arglist) => {
                return self.gen_super(arglist, None, use_mode, loc);
            }
            NodeKind::Command(box expr) => {
                let arglist = ArgList::from_args(vec![expr]);
                let method = IdentId::get_id("`");
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::Yield(arglist) => {
                let ret = if use_mode.use_val() {
                    Some(self.push().into())
                } else {
                    None
                };
                return self.gen_yield(arglist, ret, use_mode.is_ret(), loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let method = IdentId::get_id_from_string(method);
                return self.gen_method_call(method, None, arglist, None, use_mode, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let else_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_opt_condbr(false, cond, else_pos)?;
                self.gen_expr(then_, use_mode)?;
                match use_mode {
                    UseMode::Ret => {}
                    UseMode::NotUse => {
                        self.emit_br(succ_pos);
                    }
                    UseMode::Use => {
                        self.emit_br(succ_pos);
                        self.pop();
                    }
                }
                self.apply_label(else_pos);
                self.gen_expr(else_, use_mode)?;
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
                    self.gen_while_begin_postfix(cond_op, cond, body, use_mode.use_val())?;
                } else {
                    self.gen_while(cond_op, cond, body, use_mode.use_val())?;
                }
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::For {
                param,
                box iter,
                body,
            } => {
                self.gen_for(param, iter, body, use_mode.use_val())?;
                if use_mode.is_ret() {
                    self.emit_ret(None);
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
                    let ret = self.push().into();
                    let rhs = self.next_reg().into();
                    self.gen_expr(cond, UseMode::Use)?;
                    for branch in when_ {
                        let CaseBranch { box body, mut when } = branch;
                        let succ_pos = self.new_label();
                        if when.len() == 1 {
                            let when = when.remove(0);
                            self.gen_teq_condbr(when, rhs, succ_pos, false)?;
                        } else {
                            let then_pos = self.new_label();
                            for when in when {
                                self.gen_teq_condbr(when, rhs, then_pos, true)?;
                            }
                            self.emit_br(succ_pos);
                            self.apply_label(then_pos);
                        }
                        self.gen_store_expr(ret, body)?;

                        if use_mode.is_ret() {
                            self.emit(BcIr::Ret(ret), Loc::default());
                        } else {
                            self.emit_br(exit_pos);
                        }

                        self.apply_label(succ_pos);
                    }
                    self.pop();
                    self.gen_store_expr(ret, else_)?;
                    match use_mode {
                        UseMode::Ret => {
                            self.emit_ret(None);
                        }
                        UseMode::NotUse => {
                            self.pop();
                        }
                        UseMode::Use => {}
                    }
                } else {
                    let temp = self.temp;
                    for branch in when_ {
                        self.temp = temp;
                        let CaseBranch { box body, mut when } = branch;
                        let succ_pos = self.new_label();
                        if when.len() == 1 {
                            let when = when.remove(0);
                            self.gen_opt_condbr(false, when, succ_pos)?;
                        } else {
                            let then_pos = self.new_label();
                            for when in when {
                                self.gen_opt_condbr(true, when, then_pos)?;
                            }
                            self.emit_br(succ_pos);
                            self.apply_label(then_pos);
                        }
                        self.gen_expr(body, use_mode)?;
                        if !use_mode.is_ret() {
                            self.emit_br(exit_pos);
                        }
                        self.apply_label(succ_pos);
                    }
                    self.temp = temp;
                    self.gen_expr(else_, use_mode)?;
                }
                self.apply_label(exit_pos);
                return Ok(());
            }
            NodeKind::Break(box val) => {
                let (_kind, break_pos, ret_reg) = match self.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        return Err(MonorubyErr::escape_from_eval(loc, self.sourceinfo.clone()))
                    }
                };
                if let Some(reg) = ret_reg {
                    let temp = self.gen_temp_expr(val)?;
                    self.emit_mov(reg, temp)
                }
                self.emit(BcIr::Br(break_pos), loc);
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = self.is_refer_local(&expr) {
                    self.emit_ret(Some(local.into()));
                } else {
                    self.gen_expr(expr, UseMode::Ret)?;
                }
                assert_ne!(use_mode, UseMode::Use);
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => return self.gen_comp_stmts(nodes, None, use_mode),
            NodeKind::Begin {
                box body,
                rescue,
                else_,
                ensure,
            } => {
                let ensure_label = self.new_label();

                let body_use = if else_.is_some() {
                    UseMode::NotUse
                } else if ensure.is_some() && use_mode.is_ret() {
                    UseMode::Use
                } else {
                    use_mode
                };
                let rescue_use = if ensure.is_some() && use_mode.is_ret() {
                    UseMode::Use
                } else {
                    use_mode
                };
                let body_start = self.new_label();
                let body_end = self.new_label();
                self.apply_label(body_start);
                self.gen_expr(body, body_use)?;
                self.apply_label(body_end);
                if !rescue.is_empty() {
                    let else_label = self.new_label();
                    self.emit_br(else_label);
                    let rescue_start = self.new_label();
                    let mut err_reg = None;
                    let mut ex_reg = None;
                    self.apply_label(rescue_start);
                    assert_eq!(1, rescue.len());
                    for RescueEntry {
                        exception_list,
                        assign,
                        box body,
                    } in rescue
                    {
                        let old = self.temp;
                        if !exception_list.is_empty() {
                            let start: BcReg = self.next_reg().into();
                            let len = exception_list.len();
                            let mut loc = exception_list[0].loc;
                            for ex in exception_list {
                                loc = loc.merge(ex.loc);
                                self.gen_expr(ex, UseMode::Use)?;
                            }
                            ex_reg = Some((start, len, (loc, self.sourceinfo.clone())));
                        };
                        if let Some(box assign) = assign {
                            let lhs = self.eval_lvalue(&assign)?;
                            let loc = assign.loc;
                            let src = self.next_reg().into();
                            self.gen_assign(src, lhs, loc);
                            err_reg = Some(src);
                        };
                        self.gen_expr(body, rescue_use)?;
                        self.emit_br(ensure_label);
                        self.temp = old;
                    }
                    self.exception_table.push((
                        body_start..body_end,
                        rescue_start,
                        err_reg,
                        ex_reg,
                    ));
                    self.apply_label(else_label);
                } else {
                    if let Some(else_) = else_ {
                        return Err(MonorubyErr::syntax(
                            "else without rescue is useless. (SyntaxError)".to_string(),
                            else_.loc,
                            self.sourceinfo.clone(),
                        ));
                    }
                }
                if let Some(box else_) = else_ {
                    self.gen_expr(else_, rescue_use)?;
                }
                self.apply_label(ensure_label);
                if let Some(box ensure) = ensure {
                    self.gen_expr(ensure, UseMode::NotUse)?;
                    if use_mode.is_ret() {
                        self.emit_ret(None);
                    }
                }
                return Ok(());
            }
            NodeKind::MethodDef(name, block) => {
                let name = IdentId::get_id_from_string(name);
                self.gen_method_def(name, block, loc)?;
                if use_mode.use_val() {
                    self.emit_symbol(None, name);
                }
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::SingletonMethodDef(box obj, name, block) => {
                self.gen_expr(obj, UseMode::Use)?;
                let name = IdentId::get_id_from_string(name);
                self.gen_singleton_method_def(name, block, loc)?;
                if use_mode.use_val() {
                    self.emit_symbol(None, name);
                }
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info,
                is_module,
            } => {
                let dst = if use_mode.use_val() {
                    Some(self.push().into())
                } else {
                    None
                };
                let name = IdentId::get_id_from_string(name);
                self.gen_class_def(name, base, superclass, info, dst, is_module, loc)?;
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::SingletonClassDef {
                box singleton,
                info,
            } => {
                let dst = if use_mode.use_val() {
                    Some(self.push().into())
                } else {
                    None
                };
                self.gen_singleton_class_def(singleton, info, dst, loc)?;
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                let len = nodes.len();
                let arg = self.next_reg();
                for expr in nodes {
                    self.push_expr(expr)?;
                }
                self.temp -= len as u16;
                let ret = if use_mode.use_val() {
                    Some(self.push().into())
                } else {
                    None
                };
                self.emit(BcIr::ConcatStr(ret, arg, len), Loc::default());
                if use_mode.is_ret() {
                    self.emit_ret(None);
                }
                return Ok(());
            }
            NodeKind::AliasMethod(box new, box old) => {
                match (new.kind, old.kind) {
                    (NodeKind::Symbol(new), NodeKind::Symbol(old)) => {
                        let new = IdentId::get_id_from_string(new);
                        let old = IdentId::get_id_from_string(old);
                        self.emit_symbol(None, new);
                        self.emit_symbol(None, old);
                        let old = self.pop().into();
                        let new = self.pop().into();
                        self.emit(BcIr::AliasMethod { new, old }, loc);
                    }
                    _ => unimplemented!(),
                };
                match use_mode {
                    UseMode::Ret => {
                        self.emit_nil(None);
                        self.emit_ret(None);
                    }
                    UseMode::NotUse => {}
                    UseMode::Use => {
                        self.emit_nil(None);
                    }
                }
                return Ok(());
            }
            _ => return Err(MonorubyErr::unsupported_node(expr, self.sourceinfo.clone())),
        }
        match use_mode {
            UseMode::Ret => {
                self.emit_ret(None);
            }
            UseMode::NotUse => {
                self.pop();
            }
            UseMode::Use => {}
        }
        Ok(())
    }

    fn gen_store_expr(&mut self, dst: BcReg, rhs: Node) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.emit_nil(Some(dst)),
            NodeKind::Bool(b) => self.emit_literal(Some(dst), Value::bool(b)),
            NodeKind::SelfValue => self.emit_mov(dst, BcReg::Self_),
            NodeKind::Integer(i) => self.emit_integer(Some(dst), i),
            NodeKind::Symbol(sym) => {
                let sym = IdentId::get_id_from_string(sym);
                self.emit_symbol(Some(dst), sym)
            }
            NodeKind::Bignum(bigint) => self.emit_bigint(Some(dst), bigint),
            NodeKind::Float(f) => self.emit_float(Some(dst), f),
            NodeKind::String(s) => self.emit_string(Some(dst), s),
            NodeKind::Array(nodes, false) => self.gen_array(Some(dst), nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(Some(dst), nodes, loc)?,
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&rhs);
                self.emit_literal(Some(dst), val);
            }
            NodeKind::RegExp(nodes, true) => {
                let val = self.const_regexp(nodes, loc)?;
                self.emit_literal(Some(dst), val);
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(Some(dst), start, end, exclude_end, loc)?,
            NodeKind::Index {
                box base,
                mut index,
            } => {
                if index.len() == 1 {
                    self.gen_index(Some(dst), base, index.remove(0), loc)?;
                } else if index.len() == 2 {
                    let arglist = ArgList::from_args(index);
                    self.gen_method_call(
                        IdentId::_INDEX,
                        Some(base),
                        arglist,
                        Some(dst),
                        UseMode::Use,
                        loc,
                    )?;
                } else {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        loc,
                        self.sourceinfo.clone(),
                    ));
                };
            }
            NodeKind::UnOp(op, box rhs) => match op {
                UnOp::Neg => {
                    match rhs.kind {
                        NodeKind::Integer(i) => self.emit_integer(Some(dst), -i),
                        NodeKind::Float(f) => self.emit_float(Some(dst), -f),
                        _ => {
                            self.gen_store_expr(dst, rhs)?;
                            self.emit_neg(Some(dst), loc);
                        }
                    };
                }
                UnOp::Not => {
                    self.gen_store_expr(dst, rhs)?;
                    self.emit_not(dst, dst, loc);
                }
                _ => {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported unop. {:?}", op),
                        loc,
                        self.sourceinfo.clone(),
                    ))
                }
            },
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, Some(dst), loc)?;
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    if let Some(src) = self.is_assign_local(&lhs) {
                        self.gen_store_expr(src.into(), rhs)?;
                        self.emit_mov(dst, src.into());
                    } else {
                        let temp = self.temp;
                        let lhs = self.eval_lvalue(&lhs)?;
                        self.gen_store_expr(dst, rhs)?;
                        self.gen_assign(dst, lhs, loc);
                        self.temp = temp;
                    }
                } else {
                    self.gen_mul_assign(mlhs, mrhs, UseMode::Use)?;
                    let temp = self.pop().into();
                    self.emit_mov(dst, temp);
                }
            }
            NodeKind::LocalVar(0, ident) => {
                let local2 = self.refer_local(&ident);
                self.emit_mov(dst, local2.into());
            }
            NodeKind::Const {
                toplevel,
                name,
                parent: _,
                prefix,
            } => {
                self.emit_load_const(dst.into(), toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_ivar(dst.into(), name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(dst);
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(method, Some(receiver), arglist, ret, UseMode::Use, loc)?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(dst);
                let method = IdentId::get_id_from_string(method);
                self.gen_method_call(method, None, arglist, ret, UseMode::Use, loc)?;
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                self.gen_comp_stmts(nodes, Some(dst), UseMode::NotUse)?;
            }
            NodeKind::ClassDef {
                base,
                name,
                superclass,
                info,
                is_module,
            } => {
                let dst = Some(dst);
                let name = IdentId::get_id_from_string(name);
                self.gen_class_def(name, base, superclass, info, dst, is_module, loc)?;
            }
            _ => {
                let ret = self.push_expr(rhs)?;
                self.emit_mov(dst, ret);
                self.pop();
            }
        };
        Ok(())
    }

    fn const_regexp(&self, nodes: Vec<Node>, loc: Loc) -> Result<Value> {
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
                    self.sourceinfo.clone(),
                ))
            }
        };
        let re = match RegexpInner::new(string) {
            Ok(re) => re,
            Err(err) => return Err(MonorubyErr::syntax(err, loc, self.sourceinfo.clone())),
        };
        Ok(Value::new_regexp(re))
    }

    fn gen_method_def(&mut self, name: IdentId, block: BlockInfo, loc: Loc) -> Result<()> {
        let func_id = self.add_method(Some(name), block);
        self.emit(BcIr::MethodDef { name, func_id }, loc);
        Ok(())
    }

    fn gen_singleton_method_def(
        &mut self,
        name: IdentId,
        block: BlockInfo,
        loc: Loc,
    ) -> Result<()> {
        let func_id = self.add_method(Some(name), block);
        let obj = self.pop().into();
        self.emit(BcIr::SingletonMethodDef { obj, name, func_id }, loc);
        Ok(())
    }

    fn gen_class_def(
        &mut self,
        name: IdentId,
        base: Option<Box<Node>>,
        superclass: Option<Box<Node>>,
        info: BlockInfo,
        ret: Option<BcReg>,
        is_module: bool,
        loc: Loc,
    ) -> Result<()> {
        if let Some(base) = base {
            return Err(MonorubyErr::unsupported_feature(
                &format!("base in class def. {:?}", base.kind),
                loc,
                self.sourceinfo.clone(),
            ));
        };
        let func_id = self.add_classdef(Some(name), info);
        let superclass = match superclass {
            Some(superclass) => Some(self.gen_temp_expr(*superclass)?),
            None => None,
        };
        self.emit(
            if is_module {
                BcIr::ModuleDef { ret, name, func_id }
            } else {
                BcIr::ClassDef {
                    ret,
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
        base: Node,
        info: BlockInfo,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<()> {
        let func_id = self.add_classdef(None, info);
        let base = self.gen_temp_expr(base)?;
        self.emit(
            BcIr::SingletonClassDef {
                ret: dst,
                base,
                func_id,
            },
            loc,
        );
        Ok(())
    }

    ///
    /// ### return
    /// (start of reg: BcTemp, reg length: usize, splat position:Vec<usize>)
    ///
    fn gen_args(&mut self, args: Vec<Node>) -> Result<(BcReg, usize, Vec<usize>)> {
        let arg = self.next_reg().into();
        let mut splat_pos = vec![];
        let len = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            if let NodeKind::Splat(box expr) = arg.kind {
                self.push_expr(expr)?;
                splat_pos.push(i);
            } else {
                self.push_expr(arg)?;
            }
        }
        Ok((arg, len, splat_pos))
    }

    fn gen_dummy_init(&mut self, is_block: bool) {
        self.emit(
            if is_block {
                BcIr::InitBlock(FnInitInfo::default())
            } else {
                BcIr::InitMethod(FnInitInfo::default())
            },
            Loc::default(),
        );
    }

    fn gen_expand_array(&mut self, src: usize, dst: usize, len: usize) {
        self.emit(
            BcIr::ExpandArray(
                BcLocal(src as u16).into(),
                BcLocal(dst as u16).into(),
                len as u16,
            ),
            Loc::default(),
        );
    }

    fn replace_init(&mut self, info: &ISeqInfo) {
        let fninfo = FnInitInfo::new(self.total_reg_num(), info);
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
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = self.gen_binary_temp_expr(lhs, rhs)?;
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(&mut self, dst: Option<BcReg>, lhs: Node) -> Result<(BcReg, BcReg)> {
        let lhs = self.gen_temp_expr(lhs)?;
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs))
    }

    fn gen_cmp(
        &mut self,
        dst: Option<BcReg>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<BcReg> {
        let (dst, mode) = if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(dst, lhs)?;
            (dst, BinopMode::RI(lhs, i))
        } else {
            let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
            (dst, BinopMode::RR(lhs, rhs))
        };
        self.emit(BcIr::Cmp(kind, dst, mode, optimizable), loc);
        Ok(dst)
    }

    ///
    /// Generate multiple assignment.
    ///
    /// This func always use a new temporary register for rhs even if the number of rhs is 1.
    ///
    fn gen_mul_assign(
        &mut self,
        mlhs: Vec<Node>,
        mrhs: Vec<Node>,
        use_mode: UseMode,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        assert!(mlhs_len == mrhs_len);

        let temp = self.temp;
        // At first, we evaluate lvalues and save their info(LhsKind).
        let mut lhs_kind: Vec<LvalueKind> = vec![];
        for lhs in &mlhs {
            lhs_kind.push(self.eval_lvalue(lhs)?);
        }

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
        let rhs_reg = self.next_reg();
        let mut temp_reg = rhs_reg;
        for rhs in mrhs {
            self.push_expr(rhs)?;
        }

        // Finally, assign rvalues to lvalue.
        for (lhs, kind) in mlhs.into_iter().zip(lhs_kind) {
            if let Some(local) = self.is_assign_local(&lhs) {
                assert!(matches!(kind, LvalueKind::LocalVar { .. }));
                self.emit_mov(local.into(), temp_reg.into());
            } else {
                let src = temp_reg.into();
                self.gen_assign(src, kind, loc);
            }
            temp_reg += 1;
        }
        self.temp = temp;

        // Generate return value if needed.
        if use_mode.use_val() {
            let ret = self.push().into();
            self.emit_array(ret, rhs_reg.into(), mrhs_len, loc);
        }
        if use_mode.is_ret() {
            self.emit_ret(None);
        }
        Ok(())
    }

    fn gen_for(
        &mut self,
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
            let name = IdentId::get_id(&param[0].1);
            let counter = self.assign_local(name);
            let break_pos = self.new_label();
            let ret_reg = match use_value {
                true => Some(self.next_reg().into()),
                false => None,
            };
            self.loops.push((LoopKind::For, break_pos, ret_reg));
            // +------+
            // | iter | (when use_value)
            // +------+
            // | end  |
            // +------+
            // | dst  |
            // +------+
            let loop_entry = self.new_label();
            let loop_exit = self.new_label();
            self.gen_store_expr(counter.into(), start)?;
            let end = if use_value {
                let iter = self.push();
                let end = self.push_expr(end)?;
                self.emit(
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
                self.push_expr(end)?
            };
            self.apply_label(loop_entry);
            self.emit(BcIr::LoopStart, loc);
            let dst = self.push().into();
            self.emit(
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
            self.emit_condbr(dst, loop_exit, true, true);
            self.pop(); // pop *dst*

            self.gen_expr(*body.body, UseMode::NotUse)?;

            self.emit(
                BcIr::BinOp(
                    BinOpK::Add,
                    counter.into(),
                    BinopMode::RI(counter.into(), 1),
                ),
                loc,
            );
            self.emit_br(loop_entry);

            self.apply_label(loop_exit);
            self.pop(); // pop *end*

            self.loops.pop().unwrap();
            self.apply_label(break_pos);
            self.emit(BcIr::LoopEnd, loc);
        } else {
            let use_mode = if use_value {
                UseMode::Use
            } else {
                UseMode::NotUse
            };
            self.gen_each(param, iter, body, None, use_mode, loc)?;
        }
        Ok(())
    }

    fn gen_while(&mut self, cond_op: bool, cond: Node, body: Node, use_value: bool) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        let break_pos = self.new_label();
        let ret_reg = match use_value {
            true => Some(self.next_reg().into()),
            false => None,
        };
        self.loops.push((LoopKind::While, break_pos, ret_reg));
        let loc = body.loc;
        self.apply_label(cond_pos);
        self.emit(BcIr::LoopStart, loc);
        self.gen_opt_condbr(!cond_op, cond, succ_pos)?;
        self.gen_expr(body, UseMode::NotUse)?;
        self.emit_br(cond_pos);
        self.apply_label(succ_pos);

        if use_value {
            self.emit_nil(None);
        }
        self.loops.pop().unwrap();
        self.apply_label(break_pos);
        self.emit(BcIr::LoopEnd, loc);

        Ok(())
    }

    // in postfix while, we must evaluate the body expr once at least.
    fn gen_while_begin_postfix(
        &mut self,
        cond_op: bool,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let loop_pos = self.new_label();
        let break_pos = self.new_label();
        let ret_reg = match use_value {
            true => Some(self.next_reg().into()),
            false => None,
        };
        self.loops.push((LoopKind::While, break_pos, ret_reg));
        let loc = body.loc;
        self.apply_label(loop_pos);
        self.emit(BcIr::LoopStart, loc);
        self.gen_expr(body, UseMode::NotUse)?;
        self.gen_opt_condbr(cond_op, cond, loop_pos)?;

        if use_value {
            self.emit_nil(None);
        }
        self.loops.pop().unwrap();
        self.apply_label(break_pos);
        self.emit(BcIr::LoopEnd, loc);

        Ok(())
    }
}

impl IrContext {
    fn level_down(&mut self, node: &mut Node, level: usize) {
        match &mut node.kind {
            NodeKind::LocalVar(l, _) => {
                if *l >= level {
                    *l += 1;
                }
            }
            NodeKind::MulAssign(n1, n2) => {
                n1.iter_mut().for_each(|n| {
                    if level == 0 {
                        if let NodeKind::LocalVar(0, name) = &n.kind {
                            let name = IdentId::get_id(name);
                            self.assign_local(name);
                        }
                    }
                    self.level_down(n, level);
                });
                n2.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Lambda(BlockInfo { params, body, .. }) => {
                self.level_down(body, level + 1);
                for p in params {
                    match &mut p.kind {
                        ruruby_parse::ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ruruby_parse::ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::SelfValue
            | NodeKind::Nil
            | NodeKind::Integer(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::Bool(_)
            | NodeKind::String(_)
            | NodeKind::Symbol(_)
            | NodeKind::Ident(_)
            | NodeKind::InstanceVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::MethodDef(..)
            | NodeKind::SingletonMethodDef(..)
            | NodeKind::ClassDef { .. }
            | NodeKind::SingletonClassDef { .. } => {}
            NodeKind::CompStmt(nodes)
            | NodeKind::InterporatedString(nodes)
            | NodeKind::Array(nodes, ..)
            | NodeKind::RegExp(nodes, ..) => {
                nodes.into_iter().for_each(|n| self.level_down(n, level));
            }
            NodeKind::Command(n)
            | NodeKind::UnOp(_, n)
            | NodeKind::Splat(n)
            | NodeKind::Break(n)
            | NodeKind::Next(n)
            | NodeKind::Return(n)
            | NodeKind::Defined(n) => {
                self.level_down(n, level);
            }
            NodeKind::Const { parent, .. } => {
                if let Some(n) = parent {
                    self.level_down(n, level);
                }
            }
            NodeKind::BinOp(_, box n1, box n2)
            | NodeKind::AssignOp(_, box n1, box n2)
            | NodeKind::Range {
                start: box n1,
                end: box n2,
                ..
            }
            | NodeKind::While {
                cond: box n1,
                body: box n2,
                ..
            }
            | NodeKind::AliasMethod(box n1, box n2) => {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }
            NodeKind::If {
                cond: n1,
                then_: n2,
                else_: n3,
            } => {
                self.level_down(n1, level);
                self.level_down(n2, level);
                self.level_down(n3, level);
            }
            NodeKind::Hash(pairs, ..) => pairs.iter_mut().for_each(|(n1, n2)| {
                self.level_down(n1, level);
                self.level_down(n2, level);
            }),
            NodeKind::FuncCall { arglist, .. } | NodeKind::Yield(arglist) => {
                self.level_down_arglist(arglist, level);
            }
            NodeKind::MethodCall {
                receiver, arglist, ..
            } => {
                self.level_down(receiver, level);
                self.level_down_arglist(arglist, level);
            }
            NodeKind::Index { base, index } => {
                self.level_down(base, level);
                index.iter_mut().for_each(|n| self.level_down(n, level));
            }
            NodeKind::For { param, iter, body } => {
                for (outer, name) in param {
                    if level == *outer {
                        let name = IdentId::get_id(name);
                        self.assign_local(name);
                    }
                    if *outer >= level {
                        *outer += 1;
                    }
                }
                self.level_down(iter, level);
                let BlockInfo { params, body, .. } = body;
                self.level_down(body, level);
                for p in params {
                    match &mut p.kind {
                        ruruby_parse::ParamKind::Optional(_, n) => {
                            self.level_down(n, level);
                        }
                        ruruby_parse::ParamKind::Keyword(_, Some(n)) => {
                            self.level_down(n, level);
                        }
                        _ => {}
                    }
                }
            }
            NodeKind::Case { cond, when_, else_ } => {
                if let Some(n) = cond {
                    self.level_down(n, level);
                }
                self.level_down(else_, level);
                for CaseBranch { when, body } in when_ {
                    when.into_iter().for_each(|n| self.level_down(n, level));
                    self.level_down(body, level);
                }
            }
            NodeKind::Super(args) => {
                if let Some(arglist) = args {
                    self.level_down_arglist(arglist, level);
                }
            }
            NodeKind::Begin {
                body,
                rescue,
                else_,
                ensure,
            } => {
                self.level_down(body, level);
                for ruruby_parse::RescueEntry {
                    exception_list,
                    assign,
                    body,
                } in rescue
                {
                    exception_list
                        .into_iter()
                        .for_each(|n| self.level_down(n, level));
                    if let Some(n) = assign {
                        self.level_down(n, level);
                    }
                    self.level_down(body, level);
                }
                if let Some(n) = else_ {
                    self.level_down(n, level);
                }
                if let Some(n) = ensure {
                    self.level_down(n, level);
                }
            }
        }
    }

    fn level_down_arglist(&mut self, arglist: &mut ArgList, level: usize) {
        let ArgList {
            args,
            kw_args,
            hash_splat,
            block,
            ..
        } = arglist;
        args.iter_mut().for_each(|n| self.level_down(n, level));
        kw_args
            .iter_mut()
            .for_each(|(_, n)| self.level_down(n, level));
        hash_splat
            .iter_mut()
            .for_each(|n| self.level_down(n, level));
        if let Some(n) = block {
            self.level_down(n, level);
        }
    }
}
