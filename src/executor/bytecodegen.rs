use super::*;
use num::BigInt;
use paste::paste;
use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, Node, NodeKind, RescueEntry,
    SourceInfoRef, UnOp,
};

mod binary;
mod defined;
mod encode;
pub mod inst;
mod method_call;
mod statement;
pub(in crate::executor) use inst::*;

///
/// ID of register.
///
#[derive(Clone, Copy, PartialEq, Eq)]
enum BcReg {
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
struct BcTemp(pub u16);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Label(usize);

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

#[derive(Debug, Clone)]
struct LoopInfo {
    break_dest: Label,
    next_dest: Label,
    ret: Option<BcReg>,
}

#[derive(Debug)]
struct ExceptionEntry {
    range: std::ops::Range<Label>,
    rescue: Option<Label>,
    ensure: Option<Label>,
    err_reg: Option<BcReg>,
}

#[derive(Debug)]
pub struct BytecodeGen {
    /// ID of this function.
    id: FuncId,
    /// ID of the mother method.
    mother: Option<(FuncId, ParamsInfo)>,
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// loop information.
    loops: Vec<LoopInfo>, // (kind, label for exit, return register)
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
    exception_table: Vec<ExceptionEntry>,
    /// Call site info.
    callsites: Vec<CallSite>,
    /// Offset of call site info.
    callsite_offset: usize,
    /// Func info.
    functions: Vec<Functions>,
    /// Offset of func info.
    functions_offset: usize,
}

impl std::ops::Index<Label> for BytecodeGen {
    type Output = InstId;
    fn index(&self, index: Label) -> &Self::Output {
        self.labels[index.0].as_ref().unwrap()
    }
}

impl BytecodeGen {
    pub fn compile_script(
        globals: &mut Globals,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let store = &mut globals.func;
        let main_fid = store.add_main(ast, sourceinfo)?;
        let mut fid = main_fid;

        while store.len() > fid.get() as usize {
            BytecodeGen::compile_func(store, fid)?;
            fid = FuncId::new(fid.get() + 1);
        }

        Ok(main_fid)
    }

    fn compile_func(store: &mut FnStore, func_id: FuncId) -> Result<()> {
        let CompileInfo {
            ast,
            for_param_info,
            keyword_initializers,
            destruct_info,
            optional_info,
        } = store.get_init();
        let info = store[func_id].as_ruby_func();
        let mother = info
            .mother
            .map(|fid| (fid, store[fid].as_ruby_func().args.clone()));
        let mut gen = BytecodeGen::new(
            info,
            mother,
            store.callsite_offset(),
            store.functions_offset(),
        );
        // arguments preparation
        for ForParamInfo {
            dst_outer,
            dst_reg,
            src_reg,
        } in for_param_info
        {
            gen.emit(
                BcIr::StoreDynVar {
                    dst: (dst_reg).into(),
                    outer: dst_outer,
                    src: BcLocal(src_reg as u16).into(),
                },
                Loc::default(),
            );
        }
        for DestructureInfo { src, dst, len } in destruct_info {
            gen.gen_expand_array(src, dst, len);
        }
        for OptionalInfo { local, initializer } in optional_info {
            let local = local.into();
            let next = gen.new_label();
            gen.emit_check_local(local, next);
            gen.gen_store_expr(local, initializer)?;
            gen.apply_label(next);
        }
        let kw_reg = info.pos_num();
        // keyword args preparation
        for (id, initializer) in keyword_initializers.into_iter().enumerate() {
            let local = BcLocal((kw_reg + id) as u16).into();
            let next = gen.new_label();
            gen.emit_check_local(local, next);
            if let Some(box init) = initializer {
                gen.gen_store_expr(local, init)?;
            } else {
                gen.emit_nil(local);
            }
            gen.apply_label(next);
        }
        gen.gen_expr(ast, UseMode::Ret)?;
        gen.replace_init(info);
        //assert_eq!(0, ir.temp);
        gen.into_bytecode(store, func_id)?;
        store.set_func_data(func_id);
        Ok(())
    }
}

impl BytecodeGen {
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

    fn is_block(&self) -> bool {
        !self.outer_locals.is_empty()
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

    fn loop_push(&mut self, break_dest: Label, next_dest: Label, ret: Option<BcReg>) {
        self.loops.push(LoopInfo {
            break_dest,
            next_dest,
            ret,
        });
    }

    fn loop_pop(&mut self) {
        self.loops.pop().unwrap();
    }
}

//
// temporary registers handling.
//
impl BytecodeGen {
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
impl BytecodeGen {
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
impl BytecodeGen {
    /// get new destination label.
    fn new_label(&mut self) -> Label {
        let label = self.labels.len();
        self.labels.push(None);
        Label(label)
    }

    /// apply current instruction pointer to the destination label.
    fn apply_label(&mut self, label: Label) {
        let pos = InstId(self.ir.len() as u32);
        self.labels[label.0] = Some(pos);
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

    fn emit_method_ret(&mut self, src: Option<BcReg>) {
        let ret = match src {
            Some(ret) => ret,
            None => self.pop().into(),
        };
        //assert_eq!(0, self.temp);
        self.emit(BcIr::MethodRet(ret), Loc::default());
    }

    fn emit_break(&mut self, src: Option<BcReg>) {
        let ret = match src {
            Some(ret) => ret,
            None => self.pop().into(),
        };
        //assert_eq!(0, self.temp);
        self.emit(BcIr::Break(ret), Loc::default());
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

    fn emit_br(&mut self, jmp_pos: Label) {
        self.emit(BcIr::Br(jmp_pos), Loc::default());
    }

    fn emit_condbr(&mut self, cond: BcReg, jmp_pos: Label, jmp_if_true: bool, optimizable: bool) {
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

    fn emit_check_local(&mut self, local: BcReg, else_pos: Label) {
        self.emit(BcIr::CheckLocal(local, else_pos), Loc::default());
    }

    fn emit_nil(&mut self, dst: BcReg) {
        self.emit(BcIr::Nil(dst), Loc::default());
    }

    fn emit_literal(&mut self, dst: BcReg, v: Value) {
        self.literals.push(v);
        self.emit(BcIr::Literal(dst, v), Loc::default());
    }

    fn emit_integer(&mut self, dst: BcReg, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            self.emit(BcIr::Integer(dst, i), Loc::default());
        } else {
            self.emit_literal(dst, Value::new_integer(i));
        }
    }

    fn emit_bigint(&mut self, dst: BcReg, bigint: BigInt) {
        self.emit_literal(dst, Value::new_bigint(bigint));
    }

    fn emit_float(&mut self, dst: BcReg, f: f64) {
        self.emit_literal(dst, Value::new_float(f));
    }

    fn emit_symbol(&mut self, dst: BcReg, sym: IdentId) {
        self.emit(BcIr::Symbol(dst, sym), Loc::default());
    }

    fn emit_string(&mut self, dst: BcReg, s: String) {
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
        let ret = self.get_reg(dst);
        self.emit(
            BcIr::LoadConst {
                ret,
                toplevel,
                prefix,
                name,
            },
            loc,
        );
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

    fn push_nil(&mut self) {
        let reg = self.push().into();
        self.emit_nil(reg);
    }

    fn push_symbol(&mut self, sym: IdentId) {
        let reg = self.push().into();
        self.emit_symbol(reg, sym);
    }

    fn gen_array(&mut self, ret: BcReg, nodes: Vec<Node>, loc: Loc) -> Result<()> {
        let (src, len, _) = self.gen_args(nodes)?;
        self.popn(len);
        self.emit_array(ret, src, len, loc);
        Ok(())
    }

    fn gen_hash(&mut self, ret: BcReg, nodes: Vec<(Node, Node)>, loc: Loc) -> Result<()> {
        let len = nodes.len();
        let old_reg = self.temp;
        let args = self.next_reg();
        for (k, v) in nodes {
            self.push_expr(k)?;
            self.push_expr(v)?;
        }
        self.temp = old_reg;
        self.emit_hash(ret, args.into(), len, loc);
        Ok(())
    }

    fn gen_range(
        &mut self,
        ret: BcReg,
        start: Node,
        end: Node,
        exclude_end: bool,
        loc: Loc,
    ) -> Result<()> {
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
                self.handle_mode(use_mode, ret);
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
            NodeKind::Nil
            | NodeKind::Bool(_)
            | NodeKind::SelfValue
            | NodeKind::Integer(_)
            | NodeKind::Symbol(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::String(_)
            | NodeKind::Array(..)
            | NodeKind::Hash(..)
            | NodeKind::Range { .. }
            | NodeKind::RegExp(_, _, true)
            | NodeKind::UnOp(..)
            | NodeKind::Const { .. }
            | NodeKind::InstanceVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_) => {
                let ret = self.push().into();
                self.gen_store_expr(ret, expr)?;
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(op, lhs, rhs, None, loc)?;
            }
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
                self.gen_opt_condbr(false, cond, else_pos)?;
                let old = self.temp;
                self.gen_expr(then_, use_mode)?;
                if else_.is_empty() && use_mode == UseMode::NotUse {
                    self.apply_label(else_pos);
                } else {
                    let succ_pos = self.new_label();
                    match use_mode {
                        UseMode::NotUse | UseMode::Use => {
                            self.emit_br(succ_pos);
                        }
                        UseMode::Ret => {}
                    }
                    self.temp = old;
                    self.apply_label(else_pos);
                    self.gen_expr(else_, use_mode)?;
                    self.apply_label(succ_pos);
                }
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
                return self.gen_case(cond, when_, else_, use_mode);
            }
            NodeKind::Break(box val) => {
                let LoopInfo {
                    break_dest, ret, ..
                } = match self.loops.last().cloned() {
                    Some(data) => data,
                    None => {
                        if self.is_block() {
                            assert_ne!(use_mode, UseMode::Use);
                            return self.gen_break(val);
                        } else {
                            return Err(MonorubyErr::escape_from_eval(
                                loc,
                                self.sourceinfo.clone(),
                            ));
                        }
                    }
                };
                if let Some(reg) = ret {
                    let temp = self.gen_temp_expr(val)?;
                    self.emit_mov(reg, temp)
                } else {
                    self.gen_expr(val, UseMode::NotUse)?;
                }
                self.emit(BcIr::Br(break_dest), loc);
                return Ok(());
            }
            NodeKind::Next(box val) => {
                let LoopInfo { next_dest, ret, .. } = match self.loops.last().cloned() {
                    Some(data) => data,
                    None => {
                        if self.is_block() {
                            return self.gen_return(val);
                        } else {
                            return Err(MonorubyErr::escape_from_eval(
                                loc,
                                self.sourceinfo.clone(),
                            ));
                        }
                    }
                };
                if let Some(reg) = ret {
                    let temp = self.gen_temp_expr(val)?;
                    self.emit_mov(reg, temp)
                } else {
                    self.gen_expr(val, UseMode::NotUse)?;
                }
                self.emit(BcIr::Br(next_dest), loc);
                return Ok(());
            }
            NodeKind::Return(box val) => {
                if self.is_block() {
                    return self.gen_method_return(val);
                } else {
                    return self.gen_return(val);
                }
            }
            NodeKind::CompStmt(nodes) => return self.gen_comp_stmts(nodes, None, use_mode),
            NodeKind::Begin {
                box body,
                rescue,
                else_,
                ensure,
            } => {
                return self.gen_begin(body, rescue, else_, ensure, use_mode);
            }
            NodeKind::MethodDef(name, block) => {
                let name = IdentId::get_id_from_string(name);
                self.gen_method_def(name, block, loc)?;
                if use_mode.use_val() {
                    self.push_symbol(name);
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
                    self.push_symbol(name);
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
                        self.push_symbol(new);
                        self.push_symbol(old);
                        let old = self.pop().into();
                        let new = self.pop().into();
                        self.emit(BcIr::AliasMethod { new, old }, loc);
                    }
                    _ => unimplemented!(),
                };
                match use_mode {
                    UseMode::Ret => {
                        self.push_nil();
                        self.emit_ret(None);
                    }
                    UseMode::NotUse => {}
                    UseMode::Use => {
                        self.push_nil();
                    }
                }
                return Ok(());
            }
            NodeKind::Defined(box node) => {
                self.gen_defined(node)?;
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
            NodeKind::Nil => self.emit_nil(dst),
            NodeKind::Bool(b) => self.emit_literal(dst, Value::bool(b)),
            NodeKind::SelfValue => self.emit_mov(dst, BcReg::Self_),
            NodeKind::Integer(i) => self.emit_integer(dst, i),
            NodeKind::Symbol(sym) => {
                let sym = IdentId::get_id_from_string(sym);
                self.emit_symbol(dst, sym)
            }
            NodeKind::Bignum(bigint) => self.emit_bigint(dst, bigint),
            NodeKind::Float(f) => self.emit_float(dst, f),
            NodeKind::String(s) => self.emit_string(dst, s),
            NodeKind::Array(nodes, false) => self.gen_array(dst, nodes, loc)?,
            NodeKind::Hash(nodes, false) => self.gen_hash(dst, nodes, loc)?,
            NodeKind::Array(_, true)
            | NodeKind::Hash(_, true)
            | NodeKind::Range { is_const: true, .. } => {
                let val = Value::from_ast2(&rhs);
                self.emit_literal(dst, val);
            }
            NodeKind::RegExp(nodes, op, true) => {
                let val = self.const_regexp(nodes, op, loc)?;
                self.emit_literal(dst, val);
            }
            NodeKind::Range {
                box start,
                box end,
                exclude_end,
                is_const: false,
            } => self.gen_range(dst, start, end, exclude_end, loc)?,
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
                        NodeKind::Integer(i) => self.emit_integer(dst, -i),
                        NodeKind::Float(f) => self.emit_float(dst, -f),
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
            NodeKind::LocalVar(outer, ident) => {
                let ret = dst.into();
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
                self.emit_load_const(dst.into(), toplevel, name, prefix, loc);
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_ivar(dst.into(), name, loc);
            }
            NodeKind::GlobalVar(name) => {
                let name = IdentId::get_id_from_string(name);
                self.emit_load_gvar(dst.into(), name, loc);
            }
            NodeKind::SpecialVar(id) => {
                self.emit_load_svar(dst.into(), id as u32, loc);
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
            NodeKind::Return(box val) => {
                if self.is_block() {
                    return self.gen_method_return(val);
                } else {
                    return self.gen_return(val);
                }
            }
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

    fn const_regexp(&self, nodes: Vec<Node>, option: String, loc: Loc) -> Result<Value> {
        let mut string = String::new();
        for node in nodes {
            match &node.kind {
                NodeKind::String(s) => string += s,
                _ => unreachable!(),
            }
        }
        /*match string.pop().unwrap() {
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
        };*/
        let string = format!("(?{}){}", option, string);
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

impl BytecodeGen {
    ///
    /// Generate multiple assignment.
    ///
    /// This func always use a new temporary register for rhs even if the number of rhs is 1.
    ///
    fn gen_mul_assign(
        &mut self,
        mlhs: Vec<Node>,
        mut mrhs: Vec<Node>,
        use_mode: UseMode,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        let mut mrhs_len = mrhs.len();
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        if mlhs_len != mrhs_len && mrhs_len != 1 {
            return Err(MonorubyErr::unsupported_feature(
                "mlhs_len != mrhs_len",
                loc,
                self.sourceinfo.clone(),
            ));
        };

        let temp = self.temp;
        // At first, we evaluate lvalues and save their info(LhsKind).
        let mut lhs_kind: Vec<LvalueKind> = vec![];
        for lhs in &mlhs {
            lhs_kind.push(self.eval_lvalue(lhs)?);
        }

        // Next, we evaluate rvalues and save them in temporary registers which start from temp_reg.
        let (rhs_reg, ret_val) = if mlhs_len != 1 && mrhs_len == 1 {
            let rhs = self.push_expr(std::mem::take(&mut mrhs[0]))?;
            mrhs_len = mlhs_len;
            let rhs_reg = self.next_reg();
            self.emit(
                BcIr::ExpandArray(rhs, rhs_reg.into(), mlhs_len as u16),
                Loc::default(),
            );
            (rhs_reg, Some(rhs))
        } else {
            let rhs_reg = self.next_reg();
            for rhs in mrhs {
                self.push_expr(rhs)?;
            }
            (rhs_reg, None)
        };
        let mut temp_reg = rhs_reg;

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
            if ret_val.is_none() {
                self.emit_array(ret, rhs_reg.into(), mrhs_len, loc);
            }
        }
        if use_mode.is_ret() {
            self.emit_ret(None);
        }
        Ok(())
    }

    fn gen_return(&mut self, val: Node) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit_ret(Some(local.into()));
        } else {
            self.gen_expr(val, UseMode::Ret)?;
        }
        Ok(())
    }

    fn gen_method_return(&mut self, val: Node) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit_method_ret(Some(local.into()));
        } else {
            self.gen_expr(val, UseMode::Use)?;
            self.emit_method_ret(None);
        }
        Ok(())
    }

    fn gen_break(&mut self, val: Node) -> Result<()> {
        if let Some(local) = self.is_refer_local(&val) {
            self.emit_break(Some(local.into()));
        } else {
            self.gen_expr(val, UseMode::Use)?;
            self.emit_break(None);
        }
        Ok(())
    }
}

impl BytecodeGen {
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
            | NodeKind::SingletonClassDef { .. }
            | NodeKind::Redo => {}
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
