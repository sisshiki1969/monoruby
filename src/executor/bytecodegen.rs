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

///
/// ID of identifier.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct IdentId(pub u32);

#[derive(Clone, PartialEq)]
struct IdStore {
    id_map: HashMap<String, IdentId>,
    id_names: Vec<String>,
}

impl IdStore {
    fn new() -> Self {
        Self {
            id_map: HashMap::default(),
            id_names: vec![],
        }
    }

    fn get_ident_id(&mut self, ident: &str) -> IdentId {
        match self.id_map.get(ident) {
            Some(id) => *id,
            None => {
                let id = IdentId(self.id_map.len() as u32);
                self.id_map.insert(ident.to_string(), id);
                self.id_names.push(ident.to_string());
                id
            }
        }
    }

    fn get_ident_name(&self, id: IdentId) -> &String {
        &self.id_names[id.0 as usize]
    }
}

#[derive(Clone, PartialEq)]
struct TempInfo {
    name: String,
    args: Vec<String>,
    ast: Vec<(Stmt, Span)>,
}

///
/// Store of functions.
///
#[derive(Clone, PartialEq)]
pub struct FuncStore {
    /// Functions.
    pub functions: Vec<FuncInfo>,
    func_map: HashMap<IdentId, FuncId>,
    id_store: IdStore,
    /// remaining functions to be compiled.
    remaining: Vec<TempInfo>,
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
    fn new() -> Self {
        Self {
            functions: vec![],
            func_map: HashMap::default(),
            id_store: IdStore::new(),
            remaining: vec![],
        }
    }

    pub fn from_ast(ast: Vec<(Stmt, Span)>) -> Result<Self> {
        let mut store = Self::new();
        store.remaining.push(TempInfo {
            name: "/main".to_string(),
            args: vec![],
            ast: ast,
        });
        store.add_builtin_func("puts".to_string(), puts as *const u8 as u64, 1);
        store.add_builtin_func("assert".to_string(), assert as *const u8 as u64, 2);
        while let Some(TempInfo { name, args, ast }) = store.remaining.pop() {
            store.compile_func(name, args, ast)?;
        }
        #[cfg(debug_assertions)]
        for func in &store.functions {
            store.dump_bcir(func.id);
        }
        for func in store.functions.iter_mut() {
            match &mut func.kind {
                FuncKind::Builtin { .. } => {}
                FuncKind::Normal(ref mut info) => {
                    info.gen_bytecode_from_ir();
                }
            }
        }
        Ok(store)
    }

    /// Get *FuncId* of the toplevel function.
    pub fn get_main_func(&self) -> FuncId {
        let id = *self.id_store.id_map.get("/main").unwrap();
        self.get_method_or_panic(id)
    }

    pub fn get_ident_name(&self, id: IdentId) -> &String {
        self.id_store.get_ident_name(id)
    }

    fn get_method(&self, name: IdentId) -> Option<&FuncId> {
        self.func_map.get(&name)
    }

    pub fn get_method_or_panic(&self, name: IdentId) -> FuncId {
        *self
            .get_method(name)
            .unwrap_or_else(|| panic!("undefined method {:?}.", self.id_store.get_ident_name(name)))
    }

    fn add_builtin_func(&mut self, name: String, address: u64, arity: usize) -> FuncId {
        let id = FuncId(self.functions.len() as u32);
        let name_id = self.id_store.get_ident_id(&name);
        self.func_map.insert(name_id, id);
        self.functions
            .push(FuncInfo::new_builtin(id, name, address, arity));
        id
    }

    fn dump_bcir(&self, id: FuncId) {
        match &self[id].kind {
            FuncKind::Builtin { .. } => {}
            FuncKind::Normal(info) => {
                eprintln!("{:?} name:{} args:{:?}", info.id, self[id].name, info.args);
                let mut i = 0;
                let mut iter = info.bc_ir.iter();
                while let Some(inst) = iter.next() {
                    eprint!(":{:05} ", i);
                    match inst {
                        BcIr::Br(dst) => {
                            let dst = info.labels[*dst].unwrap();
                            eprintln!("br =>{:?}", dst);
                        }
                        BcIr::CondBr(reg, dst) => {
                            let dst = info.labels[*dst].unwrap();
                            eprintln!("condbr {:?} =>{:?}", reg, dst);
                        }
                        BcIr::CondNotBr(reg, dst) => {
                            let dst = info.labels[*dst].unwrap();
                            eprintln!("condnbr {:?} =>{:?}", reg, dst);
                        }
                        BcIr::Integer(reg, num) => eprintln!("{:?} = {}", reg, num),
                        BcIr::Const(reg, id) => eprintln!("{:?} = constants[{}]", reg, id),
                        BcIr::Nil(reg) => eprintln!("{:?} = nil", reg),
                        BcIr::Neg(dst, src) => eprintln!("{:?} = neg {:?}", dst, src),
                        BcIr::Add(dst, lhs, rhs) => eprintln!("{:?} = {:?} + {:?}", dst, lhs, rhs),
                        BcIr::Addri(dst, lhs, rhs) => {
                            eprintln!("{:?} = {:?} + {}:i16", dst, lhs, rhs)
                        }
                        BcIr::Sub(dst, lhs, rhs) => eprintln!("{:?} = {:?} - {:?}", dst, lhs, rhs),
                        BcIr::Subri(dst, lhs, rhs) => {
                            eprintln!("{:?} = {:?} - {}:i16", dst, lhs, rhs)
                        }
                        BcIr::Mul(dst, lhs, rhs) => eprintln!("{:?} = {:?} * {:?}", dst, lhs, rhs),
                        BcIr::Div(dst, lhs, rhs) => eprintln!("{:?} = {:?} / {:?}", dst, lhs, rhs),
                        BcIr::Cmp(kind, dst, lhs, rhs) => {
                            eprintln!("{:?} = {:?} {:?} {:?}", dst, lhs, kind, rhs)
                        }
                        BcIr::Cmpri(kind, dst, lhs, rhs) => {
                            eprintln!("{:?} = {:?} {:?} {}:i16", dst, lhs, kind, rhs)
                        }
                        BcIr::Ret(reg) => eprintln!("ret {:?}", reg),
                        BcIr::Mov(dst, src) => eprintln!("{:?} = {:?}", dst, src),
                        BcIr::FnCall(id, ret, arg, len) => {
                            let id = self.func_map.get(id).unwrap();
                            match ret {
                                Some(ret) => {
                                    eprintln!(
                                        "{:?} = call {}({:?}; {})",
                                        ret, self[*id].name, arg, len
                                    )
                                }
                                None => {
                                    eprintln!("_ = call {}({:?}; {})", self[*id].name, arg, len)
                                }
                            }
                        }
                    }
                    i += 1;
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
        name: String,
        args: Vec<String>,
        ast: Vec<(Stmt, Span)>,
    ) -> Result<()> {
        let id = FuncId(self.functions.len() as u32);
        let name_id = self.id_store.get_ident_id(&name);
        self.func_map.insert(name_id, id);
        let mut info = NormalFuncInfo::new(id, args, ast);
        info.compile(&mut self.id_store, &mut self.remaining)?;
        self.functions.push(FuncInfo::new_normal(name, info));
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalFuncInfo {
    /// ID of this function.
    pub id: FuncId,
    /// Bytecode.
    bc: Vec<BcOp>,
    /// the name of arguments.
    args: Vec<String>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// The current register id.
    temp: u16,
    /// The number of temporary registers.
    reg_num: u16,
    /// corresponding AST.
    ///
    /// !!CAUTION!!:this field is consumed by bytecode compiler.
    ast: Vec<(Stmt, Span)>,
    /// corresponding bytecode ir.
    bc_ir: Vec<BcIr>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// literal values.
    constants: Vec<Value>,
}

impl NormalFuncInfo {
    fn new(id: FuncId, args: Vec<String>, ast: Vec<(Stmt, Span)>) -> Self {
        let mut info = NormalFuncInfo {
            id,
            bc: vec![],
            args: args.clone(),
            locals: HashMap::default(),
            temp: 0,
            reg_num: 0,
            ast,
            bc_ir: vec![],
            labels: vec![],
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

    /// get new destination label.
    fn new_label(&mut self) -> usize {
        let label = self.labels.len();
        self.labels.push(None);
        label
    }

    /// apply current instruction pointer to the destination label.
    fn apply_label(&mut self, label: usize) {
        let pos = InstId(self.bc_ir.len() as u32);
        self.labels[label] = Some(pos);
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

    fn load_local(&mut self, ident: &String) -> Result<BcLocal> {
        match self.locals.get(ident) {
            Some(local) => Ok(BcLocal(*local)),
            None => Err(BcErr::UndefinedLocal(ident.to_owned())),
        }
    }

    fn find_local(&mut self, ident: &String) -> BcLocal {
        match self.locals.get(ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.clone()),
        }
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: String) -> BcLocal {
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    fn gen_integer(&mut self, dst: Option<BcLocal>, i: i32) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        self.bc_ir.push(BcIr::Integer(reg, i));
    }

    fn gen_float(&mut self, dst: Option<BcLocal>, f: f64) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        let id = self.new_constant(Value::float(f));
        self.bc_ir.push(BcIr::Const(reg, id));
    }

    fn gen_nil(&mut self, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        self.bc_ir.push(BcIr::Nil(reg));
    }

    fn gen_neg(&mut self, local: Option<BcLocal>) {
        match local {
            Some(local) => {
                let local = local.into();
                self.bc_ir.push(BcIr::Neg(local, local));
            }
            None => {
                let src = self.pop().into();
                let dst = self.push().into();
                self.bc_ir.push(BcIr::Neg(dst, src));
            }
        };
    }

    fn gen_ret(&mut self) -> Result<()> {
        let ret = self.pop().into();
        assert_eq!(0, self.temp);
        self.bc_ir.push(BcIr::Ret(ret));
        Ok(())
    }

    fn gen_mov(&mut self, dst: BcReg, src: BcReg) {
        self.bc_ir.push(BcIr::Mov(dst, src));
    }

    fn gen_temp_mov(&mut self, rhs: BcReg) -> BcReg {
        let lhs = self.push();
        self.gen_mov(lhs.into(), rhs);
        lhs.into()
    }
}

impl NormalFuncInfo {
    fn compile(&mut self, id_store: &mut IdStore, remaining: &mut Vec<TempInfo>) -> Result<()> {
        let mut ast = std::mem::take(&mut self.ast);
        let last = ast.pop().unwrap();
        for node in ast.into_iter() {
            self.gen_stmt(id_store, remaining, node, false)?;
        }
        self.gen_stmt(id_store, remaining, last, true)
    }

    fn gen_stmt(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        ast: (Stmt, Span),
        use_value: bool,
    ) -> Result<()> {
        match ast.0 {
            Stmt::Expr(expr) => self.gen_expr(id_store, remaining, expr.0, use_value),
            Stmt::Decl(decl) => {
                if use_value {
                    self.gen_decl(remaining, &decl.0)
                } else {
                    self.gen_decl_no(remaining, &decl.0)
                }
            }
        }
    }

    fn gen_decl(&mut self, remaining: &mut Vec<TempInfo>, decl: &Decl) -> Result<()> {
        self.gen_decl_no(remaining, decl)?;
        self.gen_nil(None);
        Ok(())
    }

    fn gen_decl_no(&mut self, remaining: &mut Vec<TempInfo>, decl: &Decl) -> Result<()> {
        match decl {
            Decl::MethodDef(name, arg_name, body) => {
                let _ =
                    self.decl_func(remaining, name.to_string(), arg_name.clone(), body.clone())?;
                Ok(())
            }
        }
    }

    fn decl_func(
        &mut self,
        remaining: &mut Vec<TempInfo>,
        name: String,
        args: Vec<String>,
        ast: Vec<(Expr, Span)>,
    ) -> Result<()> {
        let ast = ast
            .into_iter()
            .map(|(expr, span)| (Stmt::Expr((expr, span.clone())), span))
            .collect();
        remaining.push(TempInfo { name, args, ast });
        Ok(())
    }

    fn gen_exprs(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        mut ast: Vec<(Expr, Span)>,
        use_value: bool,
    ) -> Result<()> {
        let len = ast.len();
        assert!(len != 0);
        let last = ast.pop().unwrap();
        for (expr, _) in ast {
            self.gen_expr(id_store, remaining, expr, false)?;
        }
        self.gen_expr(id_store, remaining, last.0, use_value)
    }

    fn gen_temp_expr(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        expr: Expr,
    ) -> Result<BcTemp> {
        self.gen_expr(id_store, remaining, expr, true)?;
        Ok(self.pop())
    }

    /// Generate bytecode Ir from an *Expr*.
    fn gen_expr(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        expr: Expr,
        use_value: bool,
    ) -> Result<()> {
        match expr {
            Expr::Nil => {
                self.gen_nil(None);
            }
            Expr::Integer(i) => {
                self.gen_integer(None, i);
            }
            Expr::Float(f) => {
                self.gen_float(None, f);
            }
            Expr::Neg(box (rhs, _)) => {
                match rhs {
                    Expr::Integer(i) => self.gen_integer(None, -i),
                    Expr::Float(f) => self.gen_float(None, -f),
                    _ => {
                        self.gen_expr(id_store, remaining, rhs, true)?;
                        self.gen_neg(None);
                    }
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(id_store, remaining, None, lhs, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(id_store, remaining, None, lhs, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(id_store, remaining, None, lhs, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(id_store, remaining, None, lhs, rhs)?;
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(id_store, remaining, None, kind, lhs, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let local2 = self.find_local(&ident);
                return self.gen_store_expr(id_store, remaining, local2, rhs, use_value);
            }
            Expr::LocalLoad(ident) => {
                let local2 = self.load_local(&ident)?;
                self.gen_temp_mov(local2.into());
            }
            Expr::Call(name, args) => {
                let name_id = id_store.get_ident_id(&name);
                let len = args.len();
                let arg = self.gen_args(id_store, remaining, args)?;
                self.temp -= len as u16;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                self.bc_ir.push(BcIr::FnCall(name_id, ret, arg, len));
                return Ok(());
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                let cond = self.gen_temp_expr(id_store, remaining, cond_)?.into();
                let inst = BcIr::CondBr(cond, then_pos);
                self.bc_ir.push(inst);
                self.gen_exprs(id_store, remaining, else_, use_value)?;
                self.bc_ir.push(BcIr::Br(succ_pos));
                if use_value {
                    self.pop();
                }
                self.apply_label(then_pos);
                self.gen_exprs(id_store, remaining, then_, use_value)?;
                self.apply_label(succ_pos);
                return Ok(());
            }
            Expr::While(box (cond, _), body) => {
                self.gen_while(id_store, remaining, cond, body)?;
                if use_value {
                    self.gen_nil(None);
                }
                return Ok(());
            }
            Expr::Return(box stmt) => {
                self.gen_stmt(id_store, remaining, stmt, true)?;
                self.gen_ret()?;
                return Ok(());
            }
        }
        if !use_value {
            self.pop();
        }
        Ok(())
    }

    fn gen_args(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        args: Vec<(Expr, std::ops::Range<usize>)>,
    ) -> Result<BcTemp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(id_store, remaining, arg.0, true)?;
        }
        Ok(arg)
    }

    fn gen_binary(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = match (lhs.is_local(), rhs.is_local()) {
            (Some(lhs), Some(rhs)) => {
                let lhs = self.find_local(&lhs).into();
                let rhs = self.find_local(&rhs).into();
                (lhs, rhs)
            }
            (Some(lhs), None) => {
                let lhs = self.find_local(&lhs).into();
                let rhs = self.gen_temp_expr(id_store, remaining, rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(id_store, remaining, lhs)?.into();
                let rhs = self.find_local(&rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(id_store, remaining, lhs, true)?;
                self.gen_expr(id_store, remaining, rhs, true)?;
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
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = match lhs.is_local() {
            Some(lhs) => self.find_local(&lhs).into(),
            None => self.gen_temp_expr(id_store, remaining, lhs)?.into(),
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }

    fn gen_add(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(id_store, remaining, dst, lhs)?;
            self.bc_ir.push(BcIr::Addri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(id_store, remaining, dst, lhs, rhs)?;
            self.bc_ir.push(BcIr::Add(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_sub(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(id_store, remaining, dst, lhs)?;
            self.bc_ir.push(BcIr::Subri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(id_store, remaining, dst, lhs, rhs)?;
            self.bc_ir.push(BcIr::Sub(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_mul(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(id_store, remaining, dst, lhs, rhs)?;
        self.bc_ir.push(BcIr::Mul(dst, lhs, rhs));
        Ok(())
    }

    fn gen_div(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(id_store, remaining, dst, lhs, rhs)?;
        self.bc_ir.push(BcIr::Div(dst, lhs, rhs));
        Ok(())
    }

    fn gen_cmp(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(id_store, remaining, dst, lhs)?;
            self.bc_ir.push(BcIr::Cmpri(kind, dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(id_store, remaining, dst, lhs, rhs)?;
            self.bc_ir.push(BcIr::Cmp(kind, dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_store_expr(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        local: BcLocal,
        rhs: Expr,
        use_value: bool,
    ) -> Result<()> {
        match rhs {
            Expr::Nil => {
                self.gen_nil(Some(local));
            }
            Expr::Integer(i) => {
                self.gen_integer(Some(local), i);
            }
            Expr::Float(f) => {
                self.gen_float(Some(local), f);
            }
            Expr::Neg(box (rhs, _)) => {
                match rhs {
                    Expr::Integer(i) => self.gen_integer(Some(local), -i),
                    Expr::Float(f) => self.gen_float(Some(local), -f),
                    _ => {
                        self.gen_store_expr(id_store, remaining, local, rhs, false)?;
                        self.gen_neg(Some(local));
                    }
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(id_store, remaining, Some(local), lhs, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(id_store, remaining, Some(local), lhs, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(id_store, remaining, Some(local), lhs, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(id_store, remaining, Some(local), lhs, rhs)?;
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(id_store, remaining, Some(local), kind, lhs, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let src = self.find_local(&ident);
                self.gen_store_expr(id_store, remaining, src, rhs, false)?;
                self.gen_mov(local.into(), src.into());
            }
            Expr::LocalLoad(ident) => {
                let local2 = self.load_local(&ident)?;
                self.gen_mov(local.into(), local2.into());
            }
            Expr::Call(name, args) => {
                let name_id = id_store.get_ident_id(&name);
                let len = args.len();
                let arg = self.gen_args(id_store, remaining, args)?;
                self.temp -= len as u16;
                let inst = BcIr::FnCall(name_id, Some(local.into()), arg, len);
                self.bc_ir.push(inst);
            }
            Expr::Return(_) => unreachable!(),
            rhs => {
                let ret = self.next_reg();
                self.gen_expr(id_store, remaining, rhs, true)?;
                self.gen_mov(local.into(), ret.into());
                if !use_value {
                    self.pop();
                }
                return Ok(());
            }
        };
        if use_value {
            self.gen_temp_mov(local.into());
        }
        Ok(())
    }

    fn gen_while(
        &mut self,
        id_store: &mut IdStore,
        remaining: &mut Vec<TempInfo>,
        cond: Expr,
        body: Vec<(Expr, Span)>,
    ) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        self.apply_label(cond_pos);
        let cond = self.gen_temp_expr(id_store, remaining, cond)?.into();
        let inst = BcIr::CondNotBr(cond, succ_pos);
        self.bc_ir.push(inst);
        self.gen_exprs(id_store, remaining, body, false)?;
        self.bc_ir.push(BcIr::Br(cond_pos));
        self.apply_label(succ_pos);
        Ok(())
    }
}

impl NormalFuncInfo {
    fn get_index(&self, reg: &BcReg) -> u16 {
        match reg {
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        }
    }

    fn gen_bytecode_from_ir(&mut self) {
        let mut ops = vec![];
        for inst in &self.bc_ir {
            let op = match inst {
                BcIr::Br(dst) => {
                    let dst = self.labels[*dst].unwrap();
                    BcOp::Br(dst)
                }
                BcIr::CondBr(reg, dst) => {
                    let dst = self.labels[*dst].unwrap();
                    BcOp::CondBr(self.get_index(reg), dst)
                }
                BcIr::CondNotBr(reg, dst) => {
                    let dst = self.labels[*dst].unwrap();
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
            };
            ops.push(op);
        }
        self.bc = ops;
    }
}
