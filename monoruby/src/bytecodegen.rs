use super::*;
use num::BigInt;
use ruruby_parse::{
    ArgList, BinOp, BlockInfo, CaseBranch, CmpKind, Loc, LvarCollector, Node, NodeKind,
    ParseResult, RescueEntry, SourceInfoRef, UnOp,
};

mod binary;
mod defined;
mod encode;
mod expression;
pub mod inst;
mod method_call;
mod statement;
use inst::*;

pub fn bytecode_compile_script(globals: &mut Globals, result: ParseResult) -> Result<FuncId> {
    let main_fid = globals.store.add_main(result)?;
    bytecode_compile(globals, main_fid, None)?;
    Ok(main_fid)
}

pub fn bytecode_compile_eval(
    globals: &mut Globals,
    result: ParseResult,
    mother: (FuncId, usize),
    outer: (FuncId, ExternalContext),
    loc: Loc,
    binding: Option<LvarCollector>,
) -> Result<FuncId> {
    let main_fid = globals.store.add_eval(mother, result, outer, loc)?;
    bytecode_compile(globals, main_fid, binding)?;
    Ok(main_fid)
}

fn bytecode_compile(
    globals: &mut Globals,
    main_fid: FuncId,
    binding: Option<LvarCollector>,
) -> Result<()> {
    assert!(globals.store.func_len() > main_fid.get() as usize);
    bytecode_compile_func(&mut globals.store, main_fid, binding)?;
    globals.gen_wrapper(main_fid);
    let mut fid = FuncId::new(main_fid.get() + 1);

    while globals.store.func_len() > fid.get() as usize {
        bytecode_compile_func(&mut globals.store, fid, None)?;
        globals.gen_wrapper(fid);
        fid = FuncId::new(fid.get() + 1);
    }

    Ok(())
}

fn bytecode_compile_func(
    store: &mut Store,
    func_id: FuncId,
    binding: Option<LvarCollector>,
) -> Result<()> {
    let CompileInfo {
        ast,
        for_param_info,
        keyword_initializers,
        destruct_info,
        optional_info,
        loc,
    } = store.functions.get_compile_info();
    let info = store.iseq(func_id);
    let (fid, outer) = info.mother();
    let params = store.iseq(fid).args.clone();
    let mut gen = BytecodeGen::new(info, (fid, params, outer), binding);
    // arguments preparation
    for ForParamInfo {
        dst_outer,
        dst_reg,
        src_reg,
    } in for_param_info
    {
        gen.emit(
            BytecodeInst::StoreDynVar {
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
    gen.apply_label(gen.redo_label);
    // we must check whether redo exist in the function.
    let mut v = Visitor { redo_flag: false };
    v.visit(&ast);
    if v.redo_flag {
        gen.emit(BytecodeInst::LoopStart, loc);
    }
    gen.gen_expr(ast, UseMode2::Ret)?;
    if v.redo_flag {
        gen.emit(BytecodeInst::LoopEnd, loc);
    }
    gen.replace_init(info);
    gen.into_bytecode(store, loc)?;

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
    Const {
        toplevel: bool,
        parent: Option<BcReg>,
        prefix: Vec<IdentId>,
        name: IdentId,
    },
    InstanceVar(IdentId),
    ClassVar(IdentId),
    GlobalVar(IdentId),
    DynamicVar {
        outer: usize,
        dst: BcReg,
    },
    Index {
        base: BcReg,
        index: BcReg,
    },
    Index2 {
        base: BcReg,
        index1: BcTemp,
    },
    Send {
        recv: BcReg,
        method: IdentId,
    },
    LocalVar {
        dst: BcReg,
    },
    Discard,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum UseMode2 {
    Ret,
    Push,
    Store(BcReg),
    NotUse,
}

/*impl From<UseMode> for UseMode2 {
    fn from(value: UseMode) -> Self {
        match value {
            UseMode::Ret => Self::Ret,
            UseMode::Push => Self::Push,
            UseMode::NotUse => Self::NotUse,
        }
    }
}*/

impl UseMode2 {
    fn use_val(&self) -> bool {
        self != &Self::NotUse
    }

    fn is_ret(&self) -> bool {
        self == &UseMode2::Ret
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
    block_fid: Option<Functions>,
    block_arg: Option<BcReg>,
    /// *BcReg* of the first arguments.
    args: BcReg,
    /// *BcReg* of the receiver.
    recv: BcReg,
    /// *BcReg* of the return value. If None, the return value is discarded.
    dst: Option<BcReg>,
}

impl CallSite {
    fn new(
        name: impl Into<Option<IdentId>>,
        pos_num: usize,
        kw: Option<KeywordArgs>,
        splat_pos: Vec<usize>,
        block_fid: Option<Functions>,
        block_arg: Option<BcReg>,
        args: BcReg,
        recv: BcReg,
        dst: Option<BcReg>,
    ) -> Self {
        let name = name.into();
        CallSite {
            name,
            pos_num,
            kw,
            splat_pos,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
        }
    }

    fn simple(
        name: impl Into<Option<IdentId>>,
        len: usize,
        args: BcReg,
        recv: BcReg,
        dst: Option<BcReg>,
    ) -> CallSite {
        CallSite::new(name, len, None, vec![], None, None, args, recv, dst)
    }

    fn has_splat(&self) -> bool {
        !self.splat_pos.is_empty()
    }

    ///
    /// This call site has no keyword arguments, no splat arguments, no hash splat arguments, and no block argument.
    ///
    fn is_simple(&self) -> bool {
        self.kw.is_none() && !self.has_splat() && self.block_arg.is_none()
    }
}

///
/// keyword arguments information of *Callsite*.
///
#[derive(Debug, Clone)]
struct KeywordArgs {
    /// Position of the first keyword argument.
    kw_start: BcReg,
    /// Names and positions of keyword arguments.
    kw_args: IndexMap<IdentId, usize>,
    /// Positions of splat keyword arguments.
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
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
        is_block_style: bool,
    },
}

#[derive(Debug, Clone)]
struct LoopInfo {
    break_dest: Label,
    next_dest: Label,
    redo_dest: Label,
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
    ir: Vec<(BytecodeInst, Loc)>,
    /// the temp stack pointer for each bytecode instruction.
    sp: Vec<BcTemp>,
    /// destination labels.
    labels: Vec<Option<BcIndex>>,
    /// loop information.
    loops: Vec<LoopInfo>, // (kind, label for exit, return register)
    /// ensure clause information.
    ensure: Vec<Option<Node>>,
    /// local variables.
    locals: IndexMap<IdentId, BcLocal>,
    /// outer local variables. (dynamic_locals, block_param)
    outer_locals: ExternalContext,
    /// literal values. (for GC)
    literals: Vec<Value>,
    /// The name of the block param.
    block_param: Option<IdentId>,
    /// The label for redo.
    redo_label: Label,
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
    /// Merge info.
    merge_info: HashMap<Label, (Option<BcTemp>, Vec<MergeSourceInfo>)>,
}

impl std::ops::Index<Label> for BytecodeGen {
    type Output = BcIndex;
    fn index(&self, index: Label) -> &Self::Output {
        self.labels[index.0].as_ref().unwrap()
    }
}

impl BytecodeGen {
    fn new(
        info: &ISeqInfo,
        mother: (FuncId, ParamsInfo, usize),
        binding: Option<LvarCollector>,
    ) -> Self {
        let mut ir = Self {
            id: info.func_id(),
            mother,
            ir: vec![],
            sp: vec![],
            labels: vec![None], // The first label is for redo.
            loops: vec![],
            ensure: vec![],
            locals: IndexMap::default(),
            outer_locals: info.outer_locals.clone(),
            literals: vec![],
            block_param: info.block_param(),
            redo_label: Label(0),
            temp: 0,
            temp_num: 0,
            non_temp_num: 0,
            sourceinfo: info.sourceinfo.clone(),
            exception_table: vec![],
            merge_info: HashMap::default(),
        };
        if let Some(lvc) = binding {
            assert!(info.args.args_names.is_empty());
            lvc.table.0.iter().for_each(|name| {
                ir.add_local(IdentId::get_id(name));
            });
        } else {
            info.args.args_names.iter().for_each(|name| {
                ir.add_local(*name);
            });
        }
        ir.gen_dummy_init();

        ir
    }

    fn is_block(&self) -> bool {
        !self.outer_locals.is_empty()
    }

    fn add_method(&mut self, name: Option<IdentId>, info: BlockInfo) -> Functions {
        Functions::Method { name, info }
    }

    fn add_classdef(&mut self, name: Option<IdentId>, info: BlockInfo) -> Functions {
        Functions::ClassDef { name, info }
    }

    fn add_block(
        &mut self,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        info: BlockInfo,
    ) -> Functions {
        Functions::Block {
            mother,
            outer,
            optional_params,
            info,
            is_block_style: true,
        }
    }

    fn add_lambda(
        &mut self,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        info: BlockInfo,
    ) -> Functions {
        Functions::Block {
            mother,
            outer,
            optional_params: vec![],
            info,
            is_block_style: false,
        }
    }

    fn loop_push(
        &mut self,
        break_dest: Label,
        next_dest: Label,
        redo_dest: Label,
        ret: Option<BcReg>,
    ) {
        self.loops.push(LoopInfo {
            break_dest,
            next_dest,
            redo_dest,
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

    fn slot_id(&self, reg: &BcReg) -> SlotId {
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
    fn get_locals(&self) -> ExternalContext {
        let mut locals = ExternalContext::one(self.locals.clone(), self.block_param);
        locals.extend_from_slice(&self.outer_locals);
        locals
    }

    /// get the outer block argument name.
    fn outer_block_param_name(&self, outer: usize) -> Option<IdentId> {
        self.outer_locals[outer - 1].1
    }

    fn assign_local(&mut self, name: IdentId) -> BcLocal {
        match self.locals.get(&name) {
            Some(local) => *local,
            None => self.add_local(name),
        }
    }

    fn refer_local(&mut self, ident: &str) -> Option<BcLocal> {
        let name = IdentId::get_id(ident);
        match self.locals.get(&name) {
            Some(r) => Some(*r),
            None => {
                assert_eq!(Some(name), self.block_param);
                None
            }
        }
    }

    fn refer_dynamic_local(&self, outer: usize, name: IdentId) -> Option<BcLocal> {
        self.outer_locals[outer - 1].0.get(&name).cloned()
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: impl Into<Option<IdentId>>) -> BcLocal {
        let local = BcLocal(self.non_temp_num);
        if let Some(ident) = ident.into() {
            assert!(self.locals.insert(ident, local).is_none());
        };
        self.non_temp_num += 1;
        local
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

    fn handle_block(
        &mut self,
        optional_params: Vec<(usize, BcLocal, IdentId)>,
        block: BlockInfo,
    ) -> Functions {
        let outer_locals = self.get_locals();
        let (mother, _, outer) = self.mother;
        self.add_block(
            (mother, outer + 1),
            (self.id, outer_locals),
            optional_params,
            block,
        )
    }

    fn handle_lambda(&mut self, block: BlockInfo) -> Functions {
        let outer_locals = self.get_locals();
        let (mother, _, outer) = self.mother;
        self.add_lambda((mother, outer + 1), (self.id, outer_locals), block)
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

    fn emit(&mut self, op: BytecodeInst, loc: Loc) {
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
        self.emit(BytecodeInst::Ret(ret), Loc::default());
        Ok(())
    }

    fn emit_mov(&mut self, dst: BcReg, src: BcReg) {
        if dst != src {
            self.emit(BytecodeInst::Mov(dst, src), Loc::default());
        }
    }

    fn emit_br(&mut self, jmp_pos: Label) {
        self.add_merge(jmp_pos);
        self.emit(BytecodeInst::Br(jmp_pos), Loc::default());
    }

    fn emit_condbr(&mut self, cond: BcReg, jmp_pos: Label, jmp_if_true: bool, optimizable: bool) {
        self.add_merge(jmp_pos);
        self.emit(
            BytecodeInst::CondBr(
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

    fn emit_nilbr(&mut self, cond: BcReg, jmp_pos: Label) {
        self.add_merge(jmp_pos);
        self.emit(BytecodeInst::NilBr(cond, jmp_pos), Loc::default());
    }

    fn emit_check_local(&mut self, local: BcReg, else_pos: Label) {
        self.add_merge(else_pos);
        self.emit(BytecodeInst::CheckLocal(local, else_pos), Loc::default());
    }

    fn emit_nil(&mut self, dst: BcReg) {
        self.emit(BytecodeInst::Nil(dst), Loc::default());
    }

    fn emit_literal(&mut self, dst: BcReg, v: Value) {
        self.literals.push(v);
        self.emit(BytecodeInst::Literal(dst, v), Loc::default());
    }

    fn emit_integer(&mut self, dst: BcReg, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            self.emit(BytecodeInst::Integer(dst, i), Loc::default());
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

    fn emit_imaginary(&mut self, dst: BcReg, r: Real) {
        self.emit_literal(dst, Value::complex(0, r));
    }

    fn emit_symbol(&mut self, dst: BcReg, sym: IdentId) {
        self.emit(BytecodeInst::Symbol(dst, sym), Loc::default());
    }

    fn emit_string(&mut self, dst: BcReg, s: String) {
        self.emit_literal(dst, Value::string(s));
    }

    fn emit_bytes(&mut self, dst: BcReg, b: Vec<u8>) {
        self.emit_literal(dst, Value::bytes(b));
    }

    fn emit_array(&mut self, dst: BcReg, src: BcReg, len: usize, splat: Vec<usize>, loc: Loc) {
        let calsite = CallSite::new(
            None,
            len,
            None,
            splat,
            None,
            None,
            src,
            BcReg::Self_,
            Some(dst),
        );
        self.emit(BytecodeInst::Array(dst, Box::new(calsite)), loc);
    }

    fn emit_hash(&mut self, ret: BcReg, args: BcReg, len: usize, loc: Loc) {
        self.emit(
            BytecodeInst::Hash {
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
            BytecodeInst::LoadConst {
                dst,
                base,
                toplevel,
                prefix,
                name,
            },
            loc,
        );
    }

    fn emit_check_const(
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
            BytecodeInst::CheckConst {
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
        self.emit(BytecodeInst::LoadIvar(reg, name), loc);
    }

    fn emit_load_gvar(&mut self, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BytecodeInst::LoadGvar { dst: ret, name }, loc);
    }

    fn emit_load_cvar(&mut self, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BytecodeInst::LoadCvar { dst: ret, name }, loc);
    }

    fn emit_check_cvar(&mut self, dst: Option<BcReg>, name: IdentId, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BytecodeInst::CheckCvar { dst: ret, name }, loc);
    }

    fn emit_load_svar(&mut self, dst: Option<BcReg>, id: u32, loc: Loc) {
        let ret = self.get_reg(dst);
        self.emit(BytecodeInst::LoadSvar { ret, id }, loc);
    }

    fn emit_pos(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BytecodeInst::Pos { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BytecodeInst::Pos { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_neg(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BytecodeInst::Neg { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BytecodeInst::Neg { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_not(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BytecodeInst::Not { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BytecodeInst::Not { ret, src: ret }, loc);
        }
        Ok(())
    }

    fn emit_bitnot(&mut self, ret: BcReg, rhs: Node, loc: Loc) -> Result<()> {
        if let Some(rhs) = self.is_refer_local(&rhs) {
            let rhs = rhs.into();
            self.emit(BytecodeInst::BitNot { ret, src: rhs }, loc);
        } else {
            self.gen_store_expr(ret, rhs)?;
            self.emit(BytecodeInst::BitNot { ret, src: ret }, loc);
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
    /// ### Constant (e.g. C::D = 1)
    ///
    /// ```text
    ///
    /// +-----------------+
    /// |                 | <= sp
    /// +-----------------+
    /// | parent (e.g. C) |
    /// +-----------------+
    /// |                 |
    ///
    /// ```
    ///
    /// ### Instance variable (e.g. @a = 1)
    ///
    /// ### Class variable (e.g. @@a = 1)
    ///
    /// ### Global variable (e.g. $a = 1)
    ///
    /// ### Local variable (e.g. a = 1)
    ///
    /// ### Dynamic variable (e.g. a = 1)
    ///
    /// ### Index assign (e.g. base[idx] = 1)
    ///
    /// ```text
    ///
    /// +-----------------+
    /// |                 | <= sp
    /// +-----------------+
    /// |       idx       |
    /// +-----------------+
    /// |       base      |
    /// +-----------------+
    /// |                 |
    ///
    /// ```
    ///
    /// ### Index assign2 (e.g. base[idx0, idx1] = 1)
    ///
    /// ```text
    ///
    /// +-----------------+
    /// |                 | <= sp
    /// +-----------------+
    /// |      (src)      |
    /// +-----------------+
    /// |      idx1       |
    /// +-----------------+
    /// |      idx0       |
    /// +-----------------+
    /// |      base       |
    /// +-----------------+
    /// |                 |
    ///
    /// ```
    ///
    /// ### Method call (e.g. recv.method = 1)
    ///
    /// ```text
    ///
    /// +-----------------+
    /// |                 | <= sp
    /// +-----------------+
    /// |      recv       |
    /// +-----------------+
    ///
    /// ```
    fn eval_lvalue(&mut self, lhs: &Node) -> Result<LvalueKind> {
        let lhs = match &lhs.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } => {
                let name = IdentId::get_id(name);
                let prefix = prefix.iter().map(|s| IdentId::get_id(s)).collect();
                let parent = if let Some(box parent) = parent {
                    Some(self.push_expr(parent.clone())?.into())
                } else {
                    None
                };
                LvalueKind::Const {
                    toplevel: *toplevel,
                    parent,
                    prefix,
                    name,
                }
            }
            NodeKind::InstanceVar(name) => {
                let name = IdentId::get_id(name);
                LvalueKind::InstanceVar(name)
            }
            NodeKind::ClassVar(name) => {
                let name = IdentId::get_id(name);
                LvalueKind::ClassVar(name)
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
                    let base = self.push_expr(base.clone())?.into();
                    let index = self.push_expr(index[0].clone())?.into();
                    LvalueKind::Index { base, index }
                } else if index.len() == 2 {
                    let base = self.push_expr(base.clone())?.into();
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
                return Err(self.cant_set_variable(*id, lhs.loc));
            }
            NodeKind::DiscardLhs => LvalueKind::Discard,
            _ => return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone())),
        };
        Ok(lhs)
    }

    fn emit_assign(&mut self, src: BcReg, lhs: LvalueKind, old_temp: Option<u16>, loc: Loc) {
        match lhs {
            LvalueKind::Const {
                toplevel,
                name,
                parent,
                prefix,
            } => {
                self.set_temp(old_temp);
                self.emit(
                    BytecodeInst::StoreConst {
                        src,
                        toplevel,
                        base: parent,
                        prefix,
                        name,
                    },
                    loc,
                );
            }
            LvalueKind::InstanceVar(name) => {
                self.set_temp(old_temp);
                self.emit(BytecodeInst::StoreIvar(src, name), loc);
            }
            LvalueKind::ClassVar(name) => {
                self.set_temp(old_temp);
                self.emit(BytecodeInst::StoreCvar { val: src, name }, loc);
            }
            LvalueKind::GlobalVar(name) => {
                self.set_temp(old_temp);
                self.emit(BytecodeInst::StoreGvar { val: src, name }, loc);
            }
            LvalueKind::DynamicVar { outer, dst } => {
                self.set_temp(old_temp);
                self.emit(BytecodeInst::StoreDynVar { dst, outer, src }, loc);
            }
            LvalueKind::Index { base, index } => {
                self.set_temp(old_temp);
                self.emit(BytecodeInst::StoreIndex(src, base, index), loc);
            }
            LvalueKind::Index2 { base, index1 } => {
                let callsite =
                    CallSite::simple(IdentId::_INDEX_ASSIGN, 3, index1.into(), base, None);
                self.emit_mov((index1 + 2).into(), src);
                self.set_temp(old_temp);
                self.emit_call(callsite, loc);
            }
            LvalueKind::Send { recv, method } => {
                let callsite = CallSite::simple(method, 1, src, recv, None);
                self.set_temp(old_temp);
                self.emit_method_assign(callsite, loc);
            }
            LvalueKind::LocalVar { dst } => {
                self.set_temp(old_temp);
                self.emit_mov(dst, src);
            }
            LvalueKind::Discard => {
                self.set_temp(old_temp);
            }
        }
    }

    fn set_temp(&mut self, old_temp: Option<u16>) {
        if let Some(old_temp) = old_temp {
            self.temp = old_temp;
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

    fn gen_dummy_init(&mut self) {
        self.emit(
            BytecodeInst::InitMethod(FnInitInfo::default()),
            Loc::default(),
        );
    }

    fn gen_expand_array(&mut self, src: usize, dst: usize, len: usize) {
        self.emit(
            BytecodeInst::ExpandArray(
                BcLocal(src as u16).into(),
                BcLocal(dst as u16).into(),
                len as u16,
            ),
            Loc::default(),
        );
    }

    fn replace_init(&mut self, info: &ISeqInfo) {
        let fninfo = FnInitInfo::new(self.total_reg_num(), info);
        self.ir[0] = (BytecodeInst::InitMethod(fninfo), Loc::default());
    }
}

//
// Error handling.
//
impl BytecodeGen {
    fn syntax_error(&self, msg: impl Into<String>, loc: Loc) -> MonorubyErr {
        MonorubyErr::syntax(msg.into(), loc, self.sourceinfo.clone())
    }

    fn escape_from_eval(&self, msg: &str, loc: Loc) -> MonorubyErr {
        MonorubyErr::escape_from_eval(msg, loc, self.sourceinfo.clone())
    }

    fn cant_set_variable(&self, id: u32, loc: Loc) -> MonorubyErr {
        // 0 => $&
        // 1 => $'
        // 100 + n => $n
        self.syntax_error(
            format!(
                "can't set variable ${}.",
                match id {
                    ruruby_parse::SPECIAL_LASTMATCH => "&".to_string(),
                    ruruby_parse::SPECIAL_POSTMATCH => "'".to_string(),
                    ruruby_parse::SPECIAL_LOADPATH => "LOAD_PATH".to_string(),
                    ruruby_parse::SPECIAL_LOADEDFEATURES => "LOADED_FEATURES".to_string(),
                    n => (n - 100).to_string(),
                }
            ),
            loc,
        )
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
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        if start > end {
            (0, None)
        } else {
            let d = end.0 as usize - start.0 as usize;
            (d, Some(d))
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

    pub(crate) fn to_usize(self) -> usize {
        self.0 as usize
    }
}

///
/// kinds of binary operation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BinOpK {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    BitOr = 4,
    BitAnd = 5,
    BitXor = 6,
    Rem = 7,
    Exp = 8,
}

impl std::fmt::Display for BinOpK {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match *self {
            BinOpK::Add => "+",
            BinOpK::Sub => "-",
            BinOpK::Mul => "*",
            BinOpK::Div => "/",
            BinOpK::BitOr => "|",
            BinOpK::BitAnd => "&",
            BinOpK::BitXor => "^",
            BinOpK::Rem => "%",
            BinOpK::Exp => "**",
        };
        write!(f, "{}", s)
    }
}

impl BinOpK {
    pub fn from(i: u8) -> Self {
        match i {
            0 => BinOpK::Add,
            1 => BinOpK::Sub,
            2 => BinOpK::Mul,
            3 => BinOpK::Div,
            4 => BinOpK::BitOr,
            5 => BinOpK::BitAnd,
            6 => BinOpK::BitXor,
            7 => BinOpK::Rem,
            8 => BinOpK::Exp,
            _ => unreachable!(),
        }
    }

    pub(crate) fn generic_func(&self) -> BinaryOpFn {
        match self {
            BinOpK::Add => add_values,
            BinOpK::Sub => sub_values,
            BinOpK::Mul => mul_values,
            BinOpK::Div => div_values,
            BinOpK::BitOr => bitor_values,
            BinOpK::BitAnd => bitand_values,
            BinOpK::BitXor => bitxor_values,
            BinOpK::Rem => rem_values,
            BinOpK::Exp => pow_values,
        }
    }

    pub(crate) fn to_id(&self) -> IdentId {
        match self {
            BinOpK::Add => IdentId::_ADD,
            BinOpK::Sub => IdentId::_SUB,
            BinOpK::Mul => IdentId::_MUL,
            BinOpK::Div => IdentId::_DIV,
            BinOpK::BitOr => IdentId::_BOR,
            BinOpK::BitAnd => IdentId::_BAND,
            BinOpK::BitXor => IdentId::_BXOR,
            BinOpK::Rem => IdentId::_REM,
            BinOpK::Exp => IdentId::_POW,
        }
    }
}

///
/// kinds of unary operation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum UnOpK {
    Pos = 0,
    Neg = 1,
}

impl std::fmt::Display for UnOpK {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match *self {
            UnOpK::Pos => "+",
            UnOpK::Neg => "-",
        };
        write!(f, "{}", s)
    }
}

impl UnOpK {
    /*pub fn from(i: u16) -> Self {
        match i {
            0 => UnOpK::Pos,
            1 => UnOpK::Neg,
            _ => unreachable!(),
        }
    }*/

    pub(crate) fn generic_func(&self) -> UnaryOpFn {
        match self {
            UnOpK::Pos => pos_value,
            UnOpK::Neg => neg_value,
        }
    }
}

struct Visitor {
    redo_flag: bool,
}

impl Visitor {
    fn visit(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Nil
            | NodeKind::Bool(_)
            | NodeKind::SelfValue
            | NodeKind::Integer(_)
            | NodeKind::Symbol(_)
            | NodeKind::Bignum(_)
            | NodeKind::Float(_)
            | NodeKind::Imaginary(_)
            | NodeKind::String(_)
            | NodeKind::Bytes(_)
            | NodeKind::Const { .. }
            | NodeKind::InstanceVar(_)
            | NodeKind::ClassVar(_)
            | NodeKind::GlobalVar(_)
            | NodeKind::SpecialVar(_)
            | NodeKind::LocalVar(..)
            | NodeKind::Ident(..)
            | NodeKind::Lambda(_)
            | NodeKind::DiscardLhs => {}
            NodeKind::Array(nodes, _)
            | NodeKind::RegExp(nodes, _, _)
            | NodeKind::InterporatedString(nodes)
            | NodeKind::CompStmt(nodes) => self.visit_nodes(nodes),
            NodeKind::Hash(pairs, _) => pairs.iter().for_each(|(k, v)| {
                self.visit(k);
                self.visit(v);
            }),
            NodeKind::UnOp(_, node)
            | NodeKind::Next(node)
            | NodeKind::Break(node)
            | NodeKind::Return(node)
            | NodeKind::Splat(node)
            | NodeKind::Command(node) => self.visit(node),
            NodeKind::Range { start, end, .. } => {
                self.visit(start);
                self.visit(end);
            }
            NodeKind::BinOp(_, lhs, rhs) | NodeKind::AssignOp(_, lhs, rhs) => {
                self.visit(lhs);
                self.visit(rhs);
            }
            NodeKind::Index { base, index } => {
                self.visit(base);
                self.visit_nodes(index);
            }
            NodeKind::MulAssign(lhs, rhs) => {
                self.visit_nodes(lhs);
                self.visit_nodes(rhs);
            }
            NodeKind::MethodCall {
                box receiver,
                arglist,
                ..
            } => {
                self.visit(receiver);
                self.visit_arglist(arglist);
            }
            NodeKind::FuncCall { arglist, .. } | NodeKind::Yield(arglist) => {
                self.visit_arglist(arglist);
            }
            NodeKind::Super(arglist) => {
                if let Some(arglist) = arglist {
                    self.visit_arglist(arglist);
                }
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                self.visit(cond);
                self.visit(then_);
                self.visit(else_);
            }
            NodeKind::While { .. } => {}
            NodeKind::For { .. } => {}
            NodeKind::Case {
                cond,
                when_,
                box else_,
            } => {
                if let Some(cond) = cond {
                    self.visit(cond);
                }
                for case in when_ {
                    self.visit_nodes(&case.when);
                    self.visit(&case.body)
                }
                self.visit(else_);
            }
            NodeKind::Redo => {
                self.redo_flag = true;
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_,
                ensure,
            } => {
                self.visit(body);
                for rescue in rescue {
                    self.visit_nodes(&rescue.exception_list);
                    if let Some(assign) = &rescue.assign {
                        self.visit(assign);
                    }
                    self.visit(&rescue.body);
                }
                if let Some(else_) = else_ {
                    self.visit(else_);
                }
                if let Some(ensure) = ensure {
                    self.visit(ensure);
                }
            }
            NodeKind::MethodDef(..) => {}
            NodeKind::SingletonMethodDef(box obj, ..) => {
                self.visit(obj);
            }
            NodeKind::ClassDef {
                base, superclass, ..
            } => {
                if let Some(base) = base {
                    self.visit(base);
                }
                if let Some(superclass) = superclass {
                    self.visit(superclass);
                }
            }
            NodeKind::SingletonClassDef { box singleton, .. } => {
                self.visit(singleton);
            }

            NodeKind::AliasMethod(box new, box old) => {
                self.visit(new);
                self.visit(old);
            }
            NodeKind::Defined(box node) => {
                self.visit(node);
            }
        }
    }

    fn visit_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.visit(node);
        }
    }

    fn visit_arglist(&mut self, arglist: &ArgList) {
        for arg in &arglist.args {
            self.visit(arg);
        }
        for kw in &arglist.kw_args {
            self.visit(&kw.1);
        }
        self.visit_nodes(&arglist.hash_splat);
        if let Some(node) = &arglist.block {
            self.visit(node);
        }
    }
}
