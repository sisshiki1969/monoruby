use super::*;
use num::BigInt;
use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, Node, NodeKind, RescueEntry,
    SourceInfoRef, UnOp,
};

mod binary;
mod defined;
mod encode;
mod expression;
pub mod inst;
mod method_call;
mod statement;
use inst::*;

pub fn compile_script(
    globals: &mut Globals,
    ast: Node,
    sourceinfo: SourceInfoRef,
) -> Result<FuncId> {
    let main_fid = globals.store.add_main(ast, sourceinfo)?;
    let mut fid = main_fid;

    while globals.store.func_len() > fid.get() as usize {
        compile_func(&mut globals.store, fid)?;
        globals.gen_wrapper(fid);
        fid = FuncId::new(fid.get() + 1);
    }

    Ok(main_fid)
}

fn compile_func(store: &mut Store, func_id: FuncId) -> Result<()> {
    let CompileInfo {
        ast,
        for_param_info,
        keyword_initializers,
        destruct_info,
        optional_info,
        loc,
    } = store.get_compile_info();
    let info = store[func_id].as_ruby_func();
    let (fid, outer) = info.mother;
    let params = store[fid].as_ruby_func().args.clone();
    let mut gen = BytecodeGen::new(
        info,
        (fid, params, outer),
        store.callsite_offset(),
        store.func_len(),
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
    gen.gen_expr(ast, UseMode2::Ret)?;
    gen.replace_init(info);
    //assert_eq!(0, ir.temp);
    gen.into_bytecode(store, func_id, loc)?;
    store.set_func_data(func_id);
    Ok(())
}

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

impl std::ops::Add<usize> for BcTemp {
    type Output = Self;
    fn add(self, rhs: usize) -> Self {
        Self(self.0 + rhs as u16)
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

///
/// Label for jump destination.
///
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
    Index2 { base: BcReg, index1: BcTemp },
    Send { recv: BcReg, method: IdentId },
    LocalVar { dst: BcReg },
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseMode {
    Ret,
    Push,
    NotUse,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseMode2 {
    Ret,
    Push,
    Store(BcReg),
    NotUse,
}

impl From<UseMode> for UseMode2 {
    fn from(value: UseMode) -> Self {
        match value {
            UseMode::Ret => Self::Ret,
            UseMode::Push => Self::Push,
            UseMode::NotUse => Self::NotUse,
        }
    }
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
    /// Name of the method. (None for *super*)
    name: Option<IdentId>,
    /// Number of positional arguments.
    pos_num: usize,
    /// Keyword arguments information.
    kw: Option<KeywordArgs>,
    /// Positions of splat arguments.
    splat_pos: Vec<usize>,
    /// *FuncId* of passed block.
    block_fid: Option<FuncId>,
    block_arg: Option<BcReg>,
    /// *BcReg* of the first arguments.
    args: BcReg,
    /// Number of arguments.
    len: usize,
    /// *BcReg* of the receiver.
    recv: BcReg,
    /// *BcReg* of the return value. If None, the return value is discarded.
    ret: Option<BcReg>,
}

///
/// keyword arguments information of *Callsite*.
///
#[derive(Debug, Clone)]
struct KeywordArgs {
    /// Position of the first keyword argument.
    kw_start: BcReg,
    /// Names and positions of keyword arguments.
    kw_args: HashMap<IdentId, usize>,
    /// Positions of splat keyword arguments.
    hash_splat_pos: Vec<BcReg>,
}

impl KeywordArgs {
    fn len(&self) -> usize {
        self.kw_args.len() + self.hash_splat_pos.len()
    }
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
        mother: (FuncId, usize),
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

///
/// Information for bytecode compiler.
///
/// this includes AST and information for initialization of optional, keyword, destructuring parameters.
///
pub(crate) struct CompileInfo {
    /// AST.
    ast: Node,
    /// keyword params initializers.
    keyword_initializers: Vec<Option<Box<Node>>>,
    /// param expansion info
    destruct_info: Vec<DestructureInfo>,
    /// optional parameters initializers.
    optional_info: Vec<OptionalInfo>,
    /// *for* statement parameters info.
    for_param_info: Vec<ForParamInfo>,
    /// Location in a source code.
    loc: Loc,
}

impl CompileInfo {
    pub fn new(
        ast: Node,
        keyword_initializers: Vec<Option<Box<Node>>>,
        expand_info: Vec<DestructureInfo>,
        optional_info: Vec<OptionalInfo>,
        for_param_info: Vec<ForParamInfo>,
        loc: Loc,
    ) -> Self {
        Self {
            ast,
            keyword_initializers,
            destruct_info: expand_info,
            optional_info,
            for_param_info,
            loc,
        }
    }
}

///
/// Information for destructuring parameters.
///
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct DestructureInfo {
    src: usize,
    dst: usize,
    len: usize,
}

impl DestructureInfo {
    pub fn new(src: usize, dst: usize, len: usize) -> Self {
        Self { src, dst, len }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct OptionalInfo {
    local: BcLocal,
    initializer: Node,
}

impl OptionalInfo {
    pub fn new(local: BcLocal, initializer: Node) -> Self {
        Self { local, initializer }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ForParamInfo {
    dst_outer: usize,
    dst_reg: BcLocal,
    src_reg: usize,
}

impl ForParamInfo {
    pub fn new(dst_outer: usize, dst_reg: BcLocal, src_reg: usize) -> Self {
        Self {
            dst_outer,
            dst_reg,
            src_reg,
        }
    }
}

#[derive(Debug, Clone)]
struct MergeSourceInfo {
    /// Label of the merge source.
    idx: BcIndex,
    /// Stack pointer of the merge source.
    sp: BcTemp,
}

#[derive(Debug)]
struct BytecodeGen {
    /// ID of this function.
    id: FuncId,
    /// ID of the mother method.
    mother: (FuncId, ParamsInfo, usize),
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// the temp stack pointer for each bytecode instruction.
    sp: Vec<BcTemp>,
    /// destination labels.
    labels: Vec<Option<BcIndex>>,
    /// loop information.
    loops: Vec<LoopInfo>, // (kind, label for exit, return register)
    /// ensure clause information.
    ensure: Vec<Option<Node>>,
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
    /// Merge info.
    merge_info: HashMap<Label, (Option<BcTemp>, Vec<MergeSourceInfo>)>,
}

impl std::ops::Index<Label> for BytecodeGen {
    type Output = BcIndex;
    fn index(&self, index: Label) -> &Self::Output {
        self.labels[index.0].as_ref().unwrap()
    }
}

impl std::ops::Index<CallSiteId> for BytecodeGen {
    type Output = CallSite;
    fn index(&self, index: CallSiteId) -> &Self::Output {
        &self.callsites[index.0 as usize - self.callsite_offset]
    }
}

impl BytecodeGen {
    fn new(
        info: &ISeqInfo,
        mother: (FuncId, ParamsInfo, usize),
        callsite_offset: usize,
        functions_offset: usize,
    ) -> Self {
        let mut ir = Self {
            id: info.id(),
            mother,
            ir: vec![],
            sp: vec![],
            labels: vec![],
            loops: vec![],
            ensure: vec![],
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
            merge_info: HashMap::default(),
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
        pos_num: usize,
        kw: Option<KeywordArgs>,
        splat_pos: Vec<usize>,
        block_fid: Option<FuncId>,
        block_arg: Option<BcReg>,
        args: BcReg,
        recv: BcReg,
        ret: Option<BcReg>,
    ) -> CallSiteId {
        let name = name.into();
        let id = self.callsite_offset + self.callsites.len();
        let kw_len = kw.as_ref().map_or(0, |kw| kw.len());
        let len = pos_num + kw_len + block_arg.is_some() as usize;
        self.callsites.push(CallSite {
            name,
            pos_num,
            kw,
            splat_pos,
            block_fid,
            block_arg,
            args,
            len,
            recv,
            ret,
        });
        CallSiteId(id as u32)
    }

    fn add_callsite_simple(
        &mut self,
        name: impl Into<Option<IdentId>>,
        len: usize,
        args: BcReg,
        recv: BcReg,
        ret: Option<BcReg>,
    ) -> CallSiteId {
        self.add_callsite(name, len, None, vec![], None, None, args, recv, ret)
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
        mother: (FuncId, usize),
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
    fn sp(&mut self) -> BcTemp {
        self.push();
        self.pop()
    }

    fn pc(&self) -> BcIndex {
        BcIndex(self.ir.len() as u32)
    }

    fn add_merge(&mut self, dest: Label) {
        let idx = self.pc();
        let sp = self.sp();
        let new_entry = MergeSourceInfo { idx, sp };
        if let Some((_, info)) = self.merge_info.get_mut(&dest) {
            info.push(new_entry);
        } else {
            self.merge_info.insert(dest, (None, vec![new_entry]));
        }
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

    fn refer_local(&mut self, ident: &str) -> Option<BcLocal> {
        let name = IdentId::get_id(ident);
        match self.locals.get(&name) {
            Some(r) => Some(BcLocal(*r)),
            None => {
                assert_eq!(Some(name), self.block_param);
                None
            }
        }
    }

    fn refer_dynamic_local(&self, outer: usize, name: IdentId) -> Option<BcLocal> {
        self.outer_locals[outer - 1]
            .0
            .get(&name)
            .map(|r| BcLocal(*r))
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
            self.refer_local(name)
        } else {
            None
        }
    }

    fn is_refer_block_arg(&mut self, node: &Node) -> bool {
        if let NodeKind::LocalVar(outer, name) = &node.kind {
            let lvar = IdentId::get_id(name);
            if *outer == 0 {
                return self.block_param == Some(lvar);
            } else {
                return self.outer_block_param_name(*outer) == Some(lvar);
            }
        }
        false
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
        let pos = self.pc();
        let dest_sp = self.sp();
        if let Some((sp, _)) = self.merge_info.get_mut(&label) {
            assert_eq!(*sp, None);
            *sp = Some(dest_sp);
        } else {
            self.merge_info.insert(label, (Some(dest_sp), vec![]));
        }
        self.labels[label.0] = Some(pos);
    }

    fn emit(&mut self, op: BcIr, loc: Loc) {
        self.ir.push((op, loc));
        self.sp.push(BcTemp(self.temp));
    }

    fn emit_ret(&mut self, src: Option<BcReg>) -> Result<()> {
        let ensure: Vec<_> = self.ensure.iter().rev().filter_map(|e| e.clone()).collect();
        for ensure in ensure.into_iter() {
            self.gen_expr(ensure.clone(), UseMode2::NotUse)?;
        }
        let ret = match src {
            Some(ret) => ret,
            None => self.pop().into(),
        };
        self.emit(BcIr::Ret(ret), Loc::default());
        Ok(())
    }

    fn emit_mov(&mut self, dst: BcReg, src: BcReg) {
        if dst != src {
            self.emit(BcIr::Mov(dst, src), Loc::default());
        }
    }

    fn emit_br(&mut self, jmp_pos: Label) {
        self.add_merge(jmp_pos);
        self.emit(BcIr::Br(jmp_pos), Loc::default());
    }

    fn emit_condbr(&mut self, cond: BcReg, jmp_pos: Label, jmp_if_true: bool, optimizable: bool) {
        self.add_merge(jmp_pos);
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
        self.add_merge(else_pos);
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
            self.emit_literal(dst, Value::integer(i));
        }
    }

    fn emit_bigint(&mut self, dst: BcReg, bigint: BigInt) {
        self.emit_literal(dst, Value::bigint(bigint));
    }

    fn emit_float(&mut self, dst: BcReg, f: f64) {
        self.emit_literal(dst, Value::float(f));
    }

    fn emit_symbol(&mut self, dst: BcReg, sym: IdentId) {
        self.emit(BcIr::Symbol(dst, sym), Loc::default());
    }

    fn emit_string(&mut self, dst: BcReg, s: String) {
        self.emit_literal(dst, Value::string(s));
    }

    fn emit_array(&mut self, ret: BcReg, src: BcReg, len: usize, splat: Vec<usize>, loc: Loc) {
        let id = self.add_callsite(
            None,
            len,
            None,
            splat,
            None,
            None,
            src,
            BcReg::Self_,
            Some(ret),
        );
        self.emit(BcIr::Array(ret, id), loc);
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
        base: Option<BcReg>,
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
        let dst = self.get_reg(dst);
        self.emit(
            BcIr::LoadConst {
                dst,
                base,
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
        self.emit(BcIr::LoadGvar { dst: ret, name }, loc);
    }

    fn emit_load_svar(&mut self, dst: Option<BcReg>, id: u32, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BcIr::LoadSvar { ret, id }, loc);
    }

    fn emit_pos(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BcIr::Pos { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BcIr::Pos { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_neg(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BcIr::Neg { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BcIr::Neg { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_not(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BcIr::Not { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BcIr::Not { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_bitnot(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BcIr::BitNot { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BcIr::BitNot { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn push_nil(&mut self) -> BcReg {
        let reg = self.push().into();
        self.emit_nil(reg);
        reg
    }

    fn push_symbol(&mut self, sym: IdentId) -> BcReg {
        let reg = self.push().into();
        self.emit_symbol(reg, sym);
        reg
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
                let dst = self.refer_dynamic_local(outer, name).unwrap().into();
                LvalueKind::DynamicVar { outer, dst }
            }
            NodeKind::Index { box base, index } => {
                if index.len() == 1 {
                    let base = self.gen_expr_reg(base.clone())?;
                    let index = self.gen_expr_reg(index[0].clone())?;
                    LvalueKind::Index { base, index }
                } else if index.len() == 2 {
                    let base = self.gen_expr_reg(base.clone())?;
                    let index1 = self.push_expr(index[0].clone())?;
                    self.push_expr(index[1].clone())?;
                    self.push(); // register for src.
                    LvalueKind::Index2 { base, index1 }
                } else {
                    return Err(MonorubyErr::unsupported_feature(
                        &format!("unsupported index. {}", index.len()),
                        lhs.loc,
                        self.sourceinfo.clone(),
                    ));
                }
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

    fn emit_assign(&mut self, src: BcReg, lhs: LvalueKind, old_temp: Option<u16>, loc: Loc) {
        match lhs {
            LvalueKind::Const(name) => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::StoreConst(src, name), loc);
            }
            LvalueKind::InstanceVar(name) => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::StoreIvar(src, name), loc);
            }
            LvalueKind::GlobalVar(name) => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::StoreGvar { val: src, name }, loc);
            }
            LvalueKind::DynamicVar { outer, dst } => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::StoreDynVar { dst, outer, src }, loc);
            }
            LvalueKind::Index { base, index } => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Index2 { base, index1 } => {
                let callid =
                    self.add_callsite_simple(IdentId::_INDEX_ASSIGN, 3, index1.into(), base, None);
                self.emit_mov((index1 + 2).into(), src);
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit(BcIr::MethodCall(None, callid, false), loc);
                self.emit(BcIr::MethodArgs(base, index1.into(), 3), loc);
            }
            LvalueKind::Send { recv, method } => {
                let callid = self.add_callsite_simple(method, 1, src, recv, None);
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit_method_assign(callid, loc);
            }
            LvalueKind::LocalVar { dst } => {
                if let Some(old_temp) = old_temp {
                    self.temp = old_temp;
                }
                self.emit_mov(dst, src);
            }
        }
    }

    ///
    /// Evaluate *expr* and return the register which the result is stored.
    ///
    /// if *expr* is a local variable, return it. `temp` is not moved.
    ///
    /// otherwise, push the result and return the register. `temp` moves to  +1.
    ///
    fn gen_expr_reg(&mut self, expr: Node) -> Result<BcReg> {
        Ok(match self.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => self.push_expr(expr)?.into(),
        })
    }

    ///
    /// Evaluate *expr* and
    ///
    /// if *expr* is a local variable, return it.
    /// otherwise, push the result and return the register.
    ///
    /// `temp` is not moved.
    ///
    fn gen_temp_expr(&mut self, expr: Node) -> Result<BcReg> {
        Ok(match self.is_refer_local(&expr) {
            Some(lhs) => lhs.into(),
            None => {
                self.push_expr(expr)?;
                self.pop().into()
            }
        })
    }

    ///
    /// Handle ordinary arguments with/without splat operator.
    ///
    /// ### return
    /// (start of reg: BcTemp, reg length: usize, splat position:Vec<usize>)
    ///
    fn ordinary_args(&mut self, args: Vec<Node>) -> Result<(BcReg, usize, Vec<usize>)> {
        let arg = self.sp().into();
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

///
/// an index of bytecode instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd)]
#[repr(transparent)]
pub(crate) struct BcIndex(pub u32);

impl std::fmt::Debug for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::fmt::Display for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::ops::Add<usize> for BcIndex {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self((self.0 as usize + rhs) as u32)
    }
}

impl std::ops::Add<i64> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i64) -> Self::Output {
        Self((self.0 as i64 + rhs) as u32)
    }
}

impl std::ops::Add<i32> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self((self.0 as i64 + rhs as i64) as u32)
    }
}

impl std::iter::Step for BcIndex {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        if start > end {
            None
        } else {
            Some((end.0 - start.0) as usize)
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start + count)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if (start.0 as usize) < count {
            None
        } else {
            Some(BcIndex((start.0 as usize - count) as _))
        }
    }
}

impl BcIndex {
    pub(crate) fn from(i: usize) -> Self {
        Self(i as u32)
    }

    pub(crate) fn to_usize(&self) -> usize {
        self.0 as usize
    }
}
