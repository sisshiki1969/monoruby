use std::hash::Hash;

use super::*;

pub type Result<T> = std::result::Result<T, BcErr>;

//
// Builtin methods.
//

extern "C" fn puts(arg1: Value) -> Value {
    println!("{}", arg1);
    Value::nil()
}

extern "C" fn assert(expected: Value, actual: Value) -> Value {
    assert_eq!(expected, actual);
    Value::nil()
}

///
/// ID of function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct FuncId(pub u32);

impl From<FuncId> for u32 {
    fn from(id: FuncId) -> u32 {
        id.0
    }
}

#[derive(Clone, PartialEq)]
struct TempInfo {
    temp_id: usize,
    name: IdentId,
    args: Vec<IdentId>,
    ast: Node,
}

#[derive(Clone, PartialEq)]
struct Context {
    /// identifier table.
    id_store: IdentifierTable,
    /// remaining functions to be compiled.
    remaining: Vec<TempInfo>,
}

impl Context {
    fn new(id_store: IdentifierTable) -> Self {
        Self {
            id_store,
            remaining: vec![],
        }
    }

    fn push_remaining(&mut self, name: IdentId, args: Vec<IdentId>, ast: Node) -> usize {
        let temp_id = self.remaining.len();
        self.remaining.push(TempInfo {
            temp_id,
            name,
            args,
            ast,
        });
        temp_id
    }
}

///
/// Store of functions.
///
#[derive(Clone, PartialEq)]
pub struct FuncStore {
    /// Functions.
    pub functions: Vec<FuncInfo>,
    pub func_map: HashMap<IdentId, FuncId>,
    ctx: Context,
}

impl std::ops::Index<FuncId> for FuncStore {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index.0 as usize]
    }
}

impl std::ops::IndexMut<FuncId> for FuncStore {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index.0 as usize]
    }
}

impl FuncStore {
    pub fn new(id_store: IdentifierTable) -> Self {
        let mut store = Self {
            functions: vec![],
            func_map: HashMap::default(),
            ctx: Context::new(id_store),
        };
        store.add_builtin_func("puts".to_string(), puts as *const u8 as u64, 1);
        store.add_builtin_func("assert".to_string(), assert as *const u8 as u64, 2);
        store
    }

    pub fn from_ast(&mut self, ast: Node) -> Result<()> {
        self.ctx.push_remaining(IdentId::_MAIN, vec![], ast);
        // a map which associate temporaryid with *FuncId*.
        let mut temp_map = HashMap::default();

        let mut irs = vec![];
        #[cfg(debug_assertions)]
        let mut funcs = vec![];
        while let Some(TempInfo {
            temp_id,
            name,
            args,
            ast,
        }) = self.ctx.remaining.pop()
        {
            let ir = self.compile_func(&mut temp_map, temp_id, name, args, ast)?;
            irs.push(ir.1);
            #[cfg(debug_assertions)]
            funcs.push(ir.0);
        }

        for ctx in irs {
            match &mut self[ctx.func_id].kind {
                FuncKind::Normal(info) => {
                    info.bc = info.gen_bytecode_from_ir(ctx, &temp_map);
                }
                _ => unreachable!(),
            };
        }
        #[cfg(debug_assertions)]
        for f in funcs {
            self.dump_bytecode(f);
        }
        Ok(())
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&mut self) -> FuncId {
        self.get_method_or_panic(IdentId::_MAIN)
    }

    pub fn get_ident_name(&self, id: IdentId) -> &str {
        self.ctx.id_store.get_name(id)
    }

    pub fn get_ident_id(&mut self, name: &str) -> IdentId {
        self.ctx.id_store.get_ident_id(name)
    }

    pub fn get_method_or_panic(&self, name: IdentId) -> FuncId {
        *self
            .func_map
            .get(&name)
            .unwrap_or_else(|| panic!("undefined method {:?}.", self.get_ident_name(name)))
    }

    fn add_builtin_func(&mut self, name: String, address: u64, arity: usize) -> FuncId {
        let id = FuncId(self.functions.len() as u32);
        let name_id = self.get_ident_id(&name);
        self.func_map.insert(name_id, id);
        self.functions
            .push(FuncInfo::new_builtin(id, name, address, arity));
        id
    }

    fn dump_bytecode(&self, id: FuncId) {
        match &self[id].kind {
            FuncKind::Builtin { .. } => {}
            FuncKind::Normal(info) => {
                eprintln!(
                    "{:?} name:{} args:{:?}",
                    info.id,
                    self[id].name,
                    info.args
                        .iter()
                        .map(|id| self.get_ident_name(*id))
                        .collect::<Vec<_>>()
                );
                for (i, inst) in info.bc.iter().enumerate() {
                    eprint!(":{:05} ", i);
                    match inst {
                        BcOp::Br(dst) => {
                            eprintln!("br =>{:?}", dst);
                        }
                        BcOp::CondBr(reg, dst) => {
                            eprintln!("condbr %{} =>{:?}", reg, dst);
                        }
                        BcOp::CondNotBr(reg, dst) => {
                            eprintln!("condnbr %{} =>{:?}", reg, dst);
                        }
                        BcOp::Integer(reg, num) => eprintln!("%{} = {}: i32", reg, num),
                        BcOp::Const(reg, id) => eprintln!("%{} = constants[{}]", reg, id),
                        BcOp::Nil(reg) => eprintln!("%{} = nil", reg),
                        BcOp::Neg(dst, src) => eprintln!("%{} = neg %{}", dst, src),
                        BcOp::Add(dst, lhs, rhs) => eprintln!("%{} = %{} + %{}", dst, lhs, rhs),
                        BcOp::Addri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} + {}: i16", dst, lhs, rhs)
                        }
                        BcOp::Sub(dst, lhs, rhs) => eprintln!("%{} = %{} - %{}", dst, lhs, rhs),
                        BcOp::Subri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} - {}: i16", dst, lhs, rhs)
                        }
                        BcOp::Mul(dst, lhs, rhs) => eprintln!("%{} = %{} * %{}", dst, lhs, rhs),
                        BcOp::Div(dst, lhs, rhs) => eprintln!("%{} = %{} / %{}", dst, lhs, rhs),
                        BcOp::Eq(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Eq, rhs)
                        }
                        BcOp::Ne(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Ne, rhs)
                        }
                        BcOp::Gt(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Gt, rhs)
                        }
                        BcOp::Ge(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Ge, rhs)
                        }
                        BcOp::Lt(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Lt, rhs)
                        }
                        BcOp::Le(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Le, rhs)
                        }

                        BcOp::Eqri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Eq, rhs)
                        }
                        BcOp::Neri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Ne, rhs)
                        }
                        BcOp::Gtri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Gt, rhs)
                        }
                        BcOp::Geri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Ge, rhs)
                        }
                        BcOp::Ltri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Lt, rhs)
                        }
                        BcOp::Leri(dst, lhs, rhs) => {
                            eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Le, rhs)
                        }

                        BcOp::Ret(reg) => eprintln!("ret %{}", reg),
                        BcOp::Mov(dst, src) => eprintln!("%{} = %{}", dst, src),
                        BcOp::FnCall(id, ret, arg, len) => {
                            let name = self.get_ident_name(*id);
                            match *ret {
                                u16::MAX => {
                                    eprintln!("_ = call {}(%{}; {})", name, arg, len)
                                }
                                ret => {
                                    eprintln!("%{:?} = call {}(%{}; {})", ret, name, arg, len)
                                }
                            }
                        }
                        BcOp::MethodDef(id, fid) => {
                            eprintln!("define {:?}: {:?}", id, fid)
                        }
                    }
                }
                eprintln!("------------------------------------")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncKind {
    Normal(NormalFuncInfo),
    Builtin { abs_address: u64 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncInfo {
    /// ID of this function.
    pub id: FuncId,
    /// name of this function.
    name: String,
    /// arity of this function.
    arity: usize,
    pub(super) kind: FuncKind,
}

impl FuncInfo {
    fn new_normal(name: String, info: NormalFuncInfo) -> Self {
        Self {
            id: info.id,
            name,
            arity: info.args.len(),
            kind: FuncKind::Normal(info),
        }
    }

    fn new_builtin(id: FuncId, name: String, address: u64, arity: usize) -> Self {
        Self {
            id,
            name,
            arity,
            kind: FuncKind::Builtin {
                abs_address: address,
            },
        }
    }

    pub(super) fn arity(&self) -> usize {
        self.arity
    }

    pub(super) fn as_normal(&self) -> &NormalFuncInfo {
        match &self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }
}

impl FuncStore {
    /// Generate bytecode Ir in a new function from [(Stmt, Span)].
    fn compile_func(
        &mut self,
        temp_map: &mut HashMap<usize, FuncId>,
        temp_id: usize,
        name: IdentId,
        args: Vec<IdentId>,
        ast: Node,
    ) -> Result<(FuncId, IrContext)> {
        let func_id = FuncId(self.functions.len() as u32);
        self.func_map.insert(name, func_id);
        let mut info = NormalFuncInfo::new(func_id, args);
        let mut ir = IrContext::new(func_id);
        info.compile_ast(&mut self.ctx, &mut ir, ast)?;
        let name = self.get_ident_name(name).to_string();
        self.functions.push(FuncInfo::new_normal(name, info));
        temp_map.insert(temp_id, func_id);
        Ok((func_id, ir))
    }
}

#[derive(Debug, Clone, PartialEq)]
struct IrContext {
    func_id: FuncId,
    /// bytecode IR.
    ir: Vec<BcIr>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
}

impl IrContext {
    fn new(func_id: FuncId) -> Self {
        Self {
            func_id,
            ir: vec![],
            labels: vec![],
        }
    }

    fn push(&mut self, op: BcIr) {
        self.ir.push(op);
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalFuncInfo {
    /// ID of this function.
    pub id: FuncId,
    /// Bytecode.
    bc: Vec<BcOp>,
    /// the name of arguments.
    args: Vec<IdentId>,
    /// local variables.
    locals: HashMap<IdentId, u16>,
    /// The current register id.
    temp: u16,
    /// The number of temporary registers.
    reg_num: u16,
    /// literal values.
    constants: Vec<Value>,
}

impl NormalFuncInfo {
    fn new(id: FuncId, args: Vec<IdentId>) -> Self {
        let mut info = NormalFuncInfo {
            id,
            bc: vec![],
            args: args.clone(),
            locals: HashMap::default(),
            temp: 0,
            reg_num: 0,
            constants: vec![],
        };
        args.into_iter().for_each(|name| {
            info.add_local(name);
        });
        info
    }

    /// get a number of arguments.
    pub(super) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.reg_num as usize
    }

    /// get bytecode.
    pub(super) fn bytecode(&self) -> &Vec<BcOp> {
        &self.bc
    }

    /// get a constant.
    pub(super) fn get_constant(&self, id: u32) -> Value {
        self.constants[id as usize]
    }

    /// register a new constant.
    fn new_constant(&mut self, val: Value) -> u32 {
        let constants = self.constants.len();
        self.constants.push(val);
        constants as u32
    }

    /// get the next register id.
    fn next_reg(&self) -> BcTemp {
        BcTemp(self.temp)
    }

    fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.reg_num {
            self.reg_num = self.temp;
        }
        reg
    }

    fn pop(&mut self) -> BcTemp {
        self.temp -= 1;
        BcTemp(self.temp)
    }

    fn load_local(&mut self, ident: IdentId) -> Result<BcLocal> {
        match self.locals.get(&ident) {
            Some(local) => Ok(BcLocal(*local)),
            None => Err(BcErr::UndefinedLocal(ident.to_owned())),
        }
    }

    fn find_local(&mut self, ident: IdentId) -> BcLocal {
        match self.locals.get(&ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.clone()),
        }
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: IdentId) -> BcLocal {
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    fn gen_integer(&mut self, ir: &mut IrContext, dst: Option<BcLocal>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local.into(),
                None => self.push().into(),
            };
            ir.push(BcIr::Integer(reg, i));
        } else {
            self.gen_const(ir, dst, Value::fixnum(i));
        }
    }

    fn gen_const(&mut self, ir: &mut IrContext, dst: Option<BcLocal>, v: Value) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        let id = self.new_constant(v);
        ir.push(BcIr::Const(reg, id));
    }

    fn gen_float(&mut self, ir: &mut IrContext, dst: Option<BcLocal>, f: f64) {
        self.gen_const(ir, dst, Value::float(f));
    }

    fn gen_nil(&mut self, ir: &mut IrContext, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        ir.push(BcIr::Nil(reg));
    }

    fn gen_neg(&mut self, ir: &mut IrContext, local: Option<BcLocal>) {
        match local {
            Some(local) => {
                let local = local.into();
                ir.push(BcIr::Neg(local, local));
            }
            None => {
                let src = self.pop().into();
                let dst = self.push().into();
                ir.push(BcIr::Neg(dst, src));
            }
        };
    }

    fn gen_ret(&mut self, ir: &mut IrContext, local: Option<BcLocal>) {
        let ret = match local {
            Some(local) => local.into(),
            None => self.pop().into(),
        };
        assert_eq!(0, self.temp);
        ir.push(BcIr::Ret(ret));
    }

    fn gen_mov(&mut self, ir: &mut IrContext, dst: BcReg, src: BcReg) {
        ir.push(BcIr::Mov(dst, src));
    }

    fn gen_temp_mov(&mut self, ir: &mut IrContext, rhs: BcReg) {
        let lhs = self.push();
        self.gen_mov(ir, lhs.into(), rhs);
        //lhs.into()
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

pub fn is_local(node: &Node) -> Option<IdentId> {
    if let NodeKind::LocalVar(id) = &node.kind {
        Some(*id)
    } else {
        None
    }
}

impl NormalFuncInfo {
    fn compile_ast(&mut self, ctx: &mut Context, ir: &mut IrContext, ast: Node) -> Result<()> {
        self.gen_expr(ctx, ir, ast, true)?;
        if self.temp == 1 {
            self.gen_ret(ir, None);
        };
        Ok(())
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        mut nodes: Vec<Node>,
        ret: Option<BcLocal>,
        use_value: bool,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(ctx, ir, node, false)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, ir, ret, last, use_value)?;
            }
            None => {
                self.gen_expr(ctx, ir, last, use_value)?;
            }
        }
        Ok(())
    }

    fn gen_temp_expr(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        expr: Node,
    ) -> Result<BcTemp> {
        self.gen_expr(ctx, ir, expr, true)?;
        Ok(self.pop())
    }

    /// Generate bytecode Ir from an *Node*.
    fn gen_expr(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        expr: Node,
        use_value: bool,
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
        match expr.kind {
            NodeKind::Nil => self.gen_nil(ir, None),
            NodeKind::Bool(b) => self.gen_const(ir, None, Value::bool(b)),
            NodeKind::SelfValue => self.gen_temp_mov(ir, BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ir, None, i);
            }
            NodeKind::Float(f) => {
                self.gen_float(ir, None, f);
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(ir, None, -i),
                    NodeKind::Float(f) => self.gen_float(ir, None, -f),
                    _ => {
                        self.gen_expr(ctx, ir, rhs, true)?;
                        self.gen_neg(ir, None);
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => match op {
                BinOp::Add => self.gen_add(ctx, ir, None, lhs, rhs)?,
                BinOp::Sub => self.gen_sub(ctx, ir, None, lhs, rhs)?,
                BinOp::Mul => self.gen_mul(ctx, ir, None, lhs, rhs)?,
                BinOp::Div => self.gen_div(ctx, ir, None, lhs, rhs)?,
                BinOp::Eq => self.gen_cmp(ctx, ir, None, CmpKind::Eq, lhs, rhs)?,
                BinOp::Ne => self.gen_cmp(ctx, ir, None, CmpKind::Ne, lhs, rhs)?,
                BinOp::Ge => self.gen_cmp(ctx, ir, None, CmpKind::Ge, lhs, rhs)?,
                BinOp::Gt => self.gen_cmp(ctx, ir, None, CmpKind::Gt, lhs, rhs)?,
                BinOp::Le => self.gen_cmp(ctx, ir, None, CmpKind::Le, lhs, rhs)?,
                BinOp::Lt => self.gen_cmp(ctx, ir, None, CmpKind::Lt, lhs, rhs)?,
                _ => unimplemented!(),
            },
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                assert!(mlhs.len() == 1);
                assert!(mrhs.len() == 1);
                let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                match lhs.kind {
                    NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                        let local2 = self.find_local(lhs);
                        return self.gen_store_expr(ctx, ir, local2, rhs, use_value);
                    }
                    _ => unimplemented!(),
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = self.load_local(ident)?;
                self.gen_temp_mov(ir, local2.into());
            }
            NodeKind::Send {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let (arg, len) = self.check_fast_call(ctx, ir, receiver, arglist)?;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                ir.push(BcIr::FnCall(method, ret, arg, len));
                return Ok(());
            }
            NodeKind::Ident(method) => {
                let (arg, len) = self.check_fast_call_inner(ctx, ir, vec![])?;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                ir.push(BcIr::FnCall(method, ret, arg, len));
                return Ok(());
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let then_pos = ir.new_label();
                let succ_pos = ir.new_label();
                let cond = self.gen_temp_expr(ctx, ir, cond)?.into();
                let inst = BcIr::CondBr(cond, then_pos);
                ir.push(inst);
                self.gen_expr(ctx, ir, else_, use_value)?;
                ir.push(BcIr::Br(succ_pos));
                if use_value {
                    self.pop();
                }
                ir.apply_label(then_pos);
                self.gen_expr(ctx, ir, then_, use_value)?;
                ir.apply_label(succ_pos);
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
            } => {
                assert!(cond_op);
                self.gen_while(ctx, ir, cond, body)?;
                if use_value {
                    self.gen_nil(ir, None);
                }
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = is_local(&expr) {
                    let local = self.load_local(local)?;
                    self.gen_ret(ir, Some(local));
                } else {
                    self.gen_expr(ctx, ir, expr, true)?;
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, nodes, None, use_value)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_: None,
                ensure: None,
            } => {
                assert!(rescue.len() == 0);
                self.gen_expr(ctx, ir, body, use_value)?;
                return Ok(());
            }
            NodeKind::MethodDef(name, params, box node, _lv) => {
                let mut args = vec![];
                for param in params {
                    match param.kind {
                        ParamKind::Param(name) => args.push(name),
                        _ => unimplemented!("{:?}", param.kind),
                    }
                }
                let temp_id = ctx.push_remaining(name, args, node);
                ir.push(BcIr::MethodDef(name, temp_id));
                if use_value {
                    // TODO: This should be a Symbol.
                    self.gen_nil(ir, None);
                }
                return Ok(());
            }
            _ => unimplemented!("{:?}", expr.kind),
        }
        if !use_value {
            self.pop();
        }
        Ok(())
    }

    fn gen_store_expr(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        local: BcLocal,
        rhs: Node,
        use_value: bool,
    ) -> Result<()> {
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(ir, Some(local)),
            NodeKind::Bool(b) => self.gen_const(ir, Some(local), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(ir, local.into(), BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ir, Some(local), i);
            }
            NodeKind::Float(f) => {
                self.gen_float(ir, Some(local), f);
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(ir, Some(local), -i),
                    NodeKind::Float(f) => self.gen_float(ir, Some(local), -f),
                    _ => {
                        self.gen_store_expr(ctx, ir, local, rhs, false)?;
                        self.gen_neg(ir, Some(local));
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => match op {
                BinOp::Add => self.gen_add(ctx, ir, Some(local), lhs, rhs)?,
                BinOp::Sub => self.gen_sub(ctx, ir, Some(local), lhs, rhs)?,
                BinOp::Mul => self.gen_mul(ctx, ir, Some(local), lhs, rhs)?,
                BinOp::Div => self.gen_div(ctx, ir, Some(local), lhs, rhs)?,
                BinOp::Eq => self.gen_cmp(ctx, ir, Some(local), CmpKind::Eq, lhs, rhs)?,
                BinOp::Ne => self.gen_cmp(ctx, ir, Some(local), CmpKind::Ne, lhs, rhs)?,
                BinOp::Ge => self.gen_cmp(ctx, ir, Some(local), CmpKind::Ge, lhs, rhs)?,
                BinOp::Gt => self.gen_cmp(ctx, ir, Some(local), CmpKind::Gt, lhs, rhs)?,
                BinOp::Le => self.gen_cmp(ctx, ir, Some(local), CmpKind::Le, lhs, rhs)?,
                BinOp::Lt => self.gen_cmp(ctx, ir, Some(local), CmpKind::Lt, lhs, rhs)?,
                _ => unimplemented!(),
            },
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                assert!(mlhs.len() == 1);
                assert!(mrhs.len() == 1);
                let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                match lhs.kind {
                    NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                        let src = self.find_local(lhs);
                        self.gen_store_expr(ctx, ir, src, rhs, false)?;
                        self.gen_mov(ir, local.into(), src.into());
                    }
                    _ => unimplemented!(),
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = self.load_local(ident)?;
                self.gen_mov(ir, local.into(), local2.into());
            }
            NodeKind::Send {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let (arg, len) = self.check_fast_call(ctx, ir, receiver, arglist)?;
                let inst = BcIr::FnCall(method, Some(local.into()), arg, len);
                ir.push(inst);
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, nodes, Some(local), use_value)
            }
            _ => {
                let ret = self.next_reg();
                self.gen_expr(ctx, ir, rhs, true)?;
                self.gen_mov(ir, local.into(), ret.into());
                if !use_value {
                    self.pop();
                }
                return Ok(());
            }
        };
        if use_value {
            self.gen_temp_mov(ir, local.into());
        }
        Ok(())
    }

    fn gen_args(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(ctx, ir, arg, true)?;
        }
        Ok(arg)
    }

    fn check_fast_call(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        receiver: Node,
        arglist: ArgList,
    ) -> Result<(BcTemp, usize)> {
        assert!(receiver.kind == NodeKind::SelfValue);
        assert!(arglist.kw_args.len() == 0);
        assert!(arglist.hash_splat.len() == 0);
        assert!(arglist.block.is_none());
        assert!(!arglist.delegate);
        self.check_fast_call_inner(ctx, ir, arglist.args)
    }

    fn check_fast_call_inner(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        args: Vec<Node>,
    ) -> Result<(BcTemp, usize)> {
        let len = args.len();
        let arg = self.gen_args(ctx, ir, args)?;
        self.temp -= len as u16;
        Ok((arg, len))
    }

    fn gen_binary(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = match (is_local(&lhs), is_local(&rhs)) {
            (Some(lhs), Some(rhs)) => {
                let lhs = self.find_local(lhs).into();
                let rhs = self.find_local(rhs).into();
                (lhs, rhs)
            }
            (Some(lhs), None) => {
                let lhs = self.find_local(lhs).into();
                let rhs = self.gen_temp_expr(ctx, ir, rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(ctx, ir, lhs)?.into();
                let rhs = self.find_local(rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(ctx, ir, lhs, true)?;
                self.gen_expr(ctx, ir, rhs, true)?;
                let rhs = self.pop().into();
                let lhs = self.pop().into();
                (lhs, rhs)
            }
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = match is_local(&lhs) {
            Some(lhs) => self.find_local(lhs).into(),
            None => self.gen_temp_expr(ctx, ir, lhs)?.into(),
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }

    fn gen_add(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, ir, dst, lhs)?;
            ir.push(BcIr::Addri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
            ir.push(BcIr::Add(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_sub(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, ir, dst, lhs)?;
            ir.push(BcIr::Subri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
            ir.push(BcIr::Sub(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_mul(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
        ir.push(BcIr::Mul(dst, lhs, rhs));
        Ok(())
    }

    fn gen_div(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
        ir.push(BcIr::Div(dst, lhs, rhs));
        Ok(())
    }

    fn gen_cmp(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, ir, dst, lhs)?;
            ir.push(BcIr::Cmpri(kind, dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
            ir.push(BcIr::Cmp(kind, dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_while(
        &mut self,
        ctx: &mut Context,
        ir: &mut IrContext,
        cond: Node,
        body: Node,
    ) -> Result<()> {
        let cond_pos = ir.new_label();
        let succ_pos = ir.new_label();
        ir.apply_label(cond_pos);
        let cond = self.gen_temp_expr(ctx, ir, cond)?.into();
        let inst = BcIr::CondNotBr(cond, succ_pos);
        ir.push(inst);
        self.gen_expr(ctx, ir, body, false)?;
        ir.push(BcIr::Br(cond_pos));
        ir.apply_label(succ_pos);
        Ok(())
    }
}

impl NormalFuncInfo {
    fn get_index(&self, reg: &BcReg) -> u16 {
        match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        }
    }

    fn gen_bytecode_from_ir(&self, ir: IrContext, temp_map: &HashMap<usize, FuncId>) -> Vec<BcOp> {
        let mut ops = vec![];
        for inst in &ir.ir {
            let op = match inst {
                BcIr::Br(dst) => {
                    let dst = ir.labels[*dst].unwrap();
                    BcOp::Br(dst)
                }
                BcIr::CondBr(reg, dst) => {
                    let dst = ir.labels[*dst].unwrap();
                    BcOp::CondBr(self.get_index(reg), dst)
                }
                BcIr::CondNotBr(reg, dst) => {
                    let dst = ir.labels[*dst].unwrap();
                    BcOp::CondNotBr(self.get_index(reg), dst)
                }
                BcIr::Integer(reg, num) => BcOp::Integer(self.get_index(reg), *num),
                BcIr::Const(reg, num) => BcOp::Const(self.get_index(reg), *num),
                BcIr::Nil(reg) => BcOp::Nil(self.get_index(reg)),
                BcIr::Neg(dst, src) => BcOp::Neg(self.get_index(dst), self.get_index(src)),
                BcIr::Add(dst, lhs, rhs) => BcOp::Add(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Addri(dst, lhs, rhs) => {
                    BcOp::Addri(self.get_index(dst), self.get_index(lhs), *rhs)
                }
                BcIr::Sub(dst, lhs, rhs) => BcOp::Sub(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Subri(dst, lhs, rhs) => {
                    BcOp::Subri(self.get_index(dst), self.get_index(lhs), *rhs)
                }
                BcIr::Mul(dst, lhs, rhs) => BcOp::Mul(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Div(dst, lhs, rhs) => BcOp::Div(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Cmp(kind, dst, lhs, rhs) => {
                    let dst = self.get_index(dst);
                    let lhs = self.get_index(lhs);
                    let rhs = self.get_index(rhs);
                    match kind {
                        CmpKind::Eq => BcOp::Eq(dst, lhs, rhs),
                        CmpKind::Ne => BcOp::Ne(dst, lhs, rhs),
                        CmpKind::Gt => BcOp::Gt(dst, lhs, rhs),
                        CmpKind::Ge => BcOp::Ge(dst, lhs, rhs),
                        CmpKind::Lt => BcOp::Lt(dst, lhs, rhs),
                        CmpKind::Le => BcOp::Le(dst, lhs, rhs),
                    }
                }
                BcIr::Cmpri(kind, dst, lhs, rhs) => {
                    let dst = self.get_index(dst);
                    let lhs = self.get_index(lhs);
                    let rhs = *rhs;
                    match kind {
                        CmpKind::Eq => BcOp::Eqri(dst, lhs, rhs),
                        CmpKind::Ne => BcOp::Neri(dst, lhs, rhs),
                        CmpKind::Gt => BcOp::Gtri(dst, lhs, rhs),
                        CmpKind::Ge => BcOp::Geri(dst, lhs, rhs),
                        CmpKind::Lt => BcOp::Ltri(dst, lhs, rhs),
                        CmpKind::Le => BcOp::Leri(dst, lhs, rhs),
                    }
                }
                BcIr::Ret(reg) => BcOp::Ret(self.get_index(reg)),
                BcIr::Mov(dst, src) => BcOp::Mov(self.get_index(dst), self.get_index(src)),
                BcIr::FnCall(id, ret, arg, len) => BcOp::FnCall(
                    *id,
                    match ret {
                        Some(ret) => self.get_index(ret),
                        None => u16::MAX,
                    },
                    self.get_index(&BcReg::from(*arg)),
                    *len as u16,
                ),
                BcIr::MethodDef(id, temp_id) => {
                    BcOp::MethodDef(*id, *temp_map.get(&temp_id).unwrap())
                }
            };
            ops.push(op);
        }
        ops
    }
}
