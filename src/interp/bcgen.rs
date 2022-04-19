use super::*;

pub type Result<T> = std::result::Result<T, BcErr>;

///
/// A state of Bytecode generator.
///
#[derive(Clone, PartialEq)]
pub struct BcGen {
    /// Functions.
    functions: Vec<BcFunc>,
    func_map: HashMap<String, BcFuncId>,
    cur_fn: BcFuncId,
}

impl std::fmt::Debug for BcGen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Func name:{}", self.name)?;
        for inst in &self.insts {
            writeln!(f, "{:?}", inst)?;
        }
        writeln!(f, "-------------------------")?;
        Ok(())
    }
}

impl std::ops::Index<BcFuncId> for BcGen {
    type Output = BcFunc;
    fn index(&self, index: BcFuncId) -> &BcFunc {
        &self.functions[index.0]
    }
}

impl std::ops::IndexMut<BcFuncId> for BcGen {
    fn index_mut(&mut self, index: BcFuncId) -> &mut BcFunc {
        &mut self.functions[index.0]
    }
}

impl std::ops::Deref for BcGen {
    type Target = BcFunc;
    fn deref(&self) -> &BcFunc {
        let func = self.cur_fn;
        &self[func]
    }
}

impl std::ops::DerefMut for BcGen {
    fn deref_mut(&mut self) -> &mut BcFunc {
        let func = self.cur_fn;
        &mut self[func]
    }
}

impl BcGen {
    fn new() -> Self {
        Self {
            functions: vec![],
            func_map: HashMap::default(),
            cur_fn: BcFuncId(0),
        }
    }

    pub fn new_bytecode(ast: &[(Stmt, Span)]) -> Result<Self> {
        let mut gen = Self::new();
        let _func = gen.new_func("/main".to_string(), vec![])?;
        gen.gen_stmts(ast)?;
        gen.ast = ast.to_vec();
        gen.dump_bcir();
        for func in gen.functions.iter_mut() {
            func.gen_bc_from_inst();
        }
        Ok(gen)
    }

    fn new_func(&mut self, name: String, args: Vec<String>) -> Result<BcFuncId> {
        let id = BcFuncId(self.functions.len());
        self.func_map.insert(name.clone(), id);
        self.functions.push(BcFunc::new(id, name, args.clone()));
        self.cur_fn = id;
        args.iter().for_each(|name| {
            self.add_local(name);
        });
        Ok(id)
    }

    fn gen_stmts(&mut self, ast: &[(Stmt, Span)]) -> Result<()> {
        let len = ast.len();
        for node in &ast[..len - 1] {
            self.gen_stmt(node, false)?;
        }
        self.gen_stmt(&ast[len - 1], true)
    }

    fn gen_stmt(&mut self, ast: &(Stmt, Span), use_value: bool) -> Result<()> {
        match &ast.0 {
            Stmt::Expr(expr) => self.gen_expr(&expr.0, use_value),
            Stmt::Decl(decl) => {
                if use_value {
                    self.gen_decl(&decl.0)
                } else {
                    self.gen_decl_no(&decl.0)
                }
            }
        }
    }

    fn dump_bcir(&self) {
        eprintln!("{:?} name:{} args:{:?}", self.id, self.name, self.args);
        for (i, inst) in self.insts.iter().enumerate() {
            eprint!(":{:05} ", i);
            match inst {
                BcIr::Br(dst) => {
                    let dst = self.labels[*dst].unwrap();
                    eprintln!("br =>{:?}", dst);
                }
                BcIr::CondBr(reg, dst) => {
                    let dst = self.labels[*dst].unwrap();
                    eprintln!("condbr {:?} =>{:?}", reg, dst);
                }
                BcIr::CondNotBr(reg, dst) => {
                    let dst = self.labels[*dst].unwrap();
                    eprintln!("condnbr {:?} =>{:?}", reg, dst);
                }
                BcIr::Integer(reg, num) => eprintln!("{:?} = {}", reg, num),
                BcIr::Float(reg, num) => eprintln!("{:?} = {}", reg, num),
                BcIr::Nil(reg) => eprintln!("{:?} = nil", reg),
                BcIr::Neg(dst, src) => eprintln!("{:?} = neg {:?}", dst, src),
                BcIr::Add(dst, lhs, rhs) => eprintln!("{:?} = {:?} + {:?}", dst, lhs, rhs),
                BcIr::Addri(dst, lhs, rhs) => eprintln!("{:?} = {:?} + {}:i16", dst, lhs, rhs),
                BcIr::Sub(dst, lhs, rhs) => eprintln!("{:?} = {:?} - {:?}", dst, lhs, rhs),
                BcIr::Subri(dst, lhs, rhs) => eprintln!("{:?} = {:?} - {}:i16", dst, lhs, rhs),
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
                BcIr::Call(id, ret, arg, len) => {
                    let name = self[*id].name.clone();
                    match ret {
                        Some(ret) => eprintln!("{:?} = call {}({:?}; {})", ret, name, arg, len),
                        None => eprintln!("_ = call {}({:?}; {})", name, arg, len),
                    }
                }
            }
        }
        eprintln!("------------------------------------")
    }
}

///
/// ID of function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BcFuncId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct BcFunc {
    /// ID of this function.
    pub id: BcFuncId,
    /// name of this function.
    name: String,
    /// the name of arguments.
    args: Vec<String>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// The number of registers.
    temp: u16,
    /// The number of registers.
    pub reg_num: u16,
    /// AST.
    ast: Vec<(Stmt, Span)>,
    /// Bytecode.
    insts: Vec<BcIr>,
    /// Labels.
    labels: Vec<Option<InstId>>,
    /// Bytecode.
    pub(super) bc: Vec<BcOp>,
}

impl BcFunc {
    fn new(id: BcFuncId, name: String, args: Vec<String>) -> Self {
        Self {
            id,
            name,
            args,
            locals: HashMap::default(),
            temp: 0,
            reg_num: 0,
            ast: vec![],
            insts: vec![],
            labels: vec![],
            bc: vec![],
        }
    }
    pub(super) fn local_num(&self) -> usize {
        self.locals.len()
    }

    fn new_label(&mut self) -> usize {
        let label = self.labels.len();
        self.labels.push(None);
        label
    }

    fn apply_label(&mut self, label: usize) {
        let pos = InstId(self.insts.len());
        self.labels[label] = Some(pos);
    }

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
            None => self.add_local(ident),
        }
    }

    fn add_local(&mut self, ident: &String) -> BcLocal {
        let local = self.locals.len() as u16;
        self.locals.insert(ident.to_owned(), local);
        BcLocal(local)
    }

    fn gen_integer(&mut self, dst: Option<BcLocal>, i: i32) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        self.insts.push(BcIr::Integer(reg, i));
    }

    fn gen_float(&mut self, dst: Option<BcLocal>, f: f64) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        self.insts.push(BcIr::Float(reg, f));
    }

    fn gen_nil(&mut self, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        self.insts.push(BcIr::Nil(reg));
    }

    fn gen_neg(&mut self, local: Option<BcLocal>) {
        match local {
            Some(local) => {
                let local = local.into();
                self.insts.push(BcIr::Neg(local, local));
            }
            None => {
                let src = self.pop().into();
                let dst = self.push().into();
                self.insts.push(BcIr::Neg(dst, src));
            }
        };
    }

    fn gen_ret(&mut self) -> Result<()> {
        let ret = self.pop().into();
        assert_eq!(0, self.temp);
        self.insts.push(BcIr::Ret(ret));
        Ok(())
    }

    fn gen_mov(&mut self, dst: BcReg, src: BcReg) {
        self.insts.push(BcIr::Mov(dst, src));
    }

    fn gen_temp_mov(&mut self, rhs: BcReg) -> BcReg {
        let lhs = self.push();
        self.gen_mov(lhs.into(), rhs);
        lhs.into()
    }
}

impl BcGen {
    /// Generate HIR in new function from [(Stmt, Span)].
    fn decl_func(
        &mut self,
        func_name: String,
        args: Vec<String>,
        ast: &[(Expr, Span)],
    ) -> Result<BcFuncId> {
        let save = self.cur_fn;
        let func = self.new_func(func_name, args)?;
        self.gen_exprs(ast, true)?;
        self.ast = ast
            .iter()
            .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
            .collect();
        self.dump_bcir();
        self.cur_fn = save;
        Ok(func)
    }

    fn gen_decl(&mut self, decl: &Decl) -> Result<()> {
        self.gen_decl_no(decl)?;
        self.gen_nil(None);
        Ok(())
    }

    fn gen_decl_no(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::MethodDef(name, arg_name, body) => {
                let _ = self.decl_func(name.to_string(), arg_name.clone(), body)?;
                Ok(())
            }
        }
    }

    fn gen_exprs(&mut self, ast: &[(Expr, Span)], use_value: bool) -> Result<()> {
        let len = ast.len();
        assert!(len != 0);
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr(&expr, false)?;
        }
        self.gen_expr(&ast[len - 1].0, use_value)
    }

    fn gen_temp_expr(&mut self, expr: &Expr) -> Result<BcTemp> {
        self.gen_expr(expr, true)?;
        Ok(self.pop())
    }

    /// Generate bytecode from an *Expr*.
    fn gen_expr(&mut self, expr: &Expr, use_value: bool) -> Result<()> {
        match expr {
            Expr::Nil => {
                self.gen_nil(None);
            }
            Expr::Integer(i) => {
                self.gen_integer(None, *i);
            }
            Expr::Float(f) => {
                self.gen_float(None, *f);
            }
            Expr::Neg(box (rhs, _)) => {
                match rhs {
                    Expr::Integer(i) => self.gen_integer(None, -i),
                    Expr::Float(f) => self.gen_float(None, -f),
                    _ => {
                        self.gen_expr(rhs, true)?;
                        self.gen_neg(None);
                    }
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(None, lhs, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(None, lhs, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(None, lhs, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(None, lhs, rhs)?;
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(None, *kind, lhs, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let local2 = self.find_local(ident);
                return self.gen_store_expr(local2, rhs, use_value);
            }
            Expr::LocalLoad(ident) => {
                let local2 = self.load_local(ident)?;
                self.gen_temp_mov(local2.into());
            }
            Expr::Call(name, args) => {
                let func = *self.func_map.get(name).unwrap();
                let arg = self.gen_args(args)?;
                self.temp -= args.len() as u16;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                self.insts.push(BcIr::Call(func, ret, arg, args.len()));
                return Ok(());
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                let cond = self.gen_temp_expr(cond_)?.into();
                let inst = BcIr::CondBr(cond, then_pos);
                self.insts.push(inst);
                self.gen_exprs(else_, use_value)?;
                self.insts.push(BcIr::Br(succ_pos));
                if use_value {
                    self.pop();
                }
                self.apply_label(then_pos);
                self.gen_exprs(then_, use_value)?;
                self.apply_label(succ_pos);
                return Ok(());
            }
            Expr::While(box (cond, _), body) => {
                self.gen_while(cond, body)?;
                if use_value {
                    self.gen_nil(None);
                }
                return Ok(());
            }
            Expr::Return(box stmt) => {
                self.gen_stmt(stmt, true)?;
                self.gen_ret()?;
                return Ok(());
            }
        }
        if !use_value {
            self.pop();
        }
        Ok(())
    }
}

impl BcGen {
    fn gen_args(&mut self, args: &[(Expr, std::ops::Range<usize>)]) -> Result<BcTemp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(&arg.0, true)?;
        }
        Ok(arg)
    }

    fn gen_binary(
        &mut self,
        dst: Option<BcLocal>,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = match (lhs.is_local(), rhs.is_local()) {
            (Some(lhs), Some(rhs)) => {
                let lhs = self.find_local(&lhs).into();
                let rhs = self.find_local(&rhs).into();
                (lhs, rhs)
            }
            (Some(lhs), None) => {
                let lhs = self.find_local(&lhs).into();
                let rhs = self.gen_temp_expr(rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(lhs)?.into();
                let rhs = self.find_local(&rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(lhs, true)?;
                self.gen_expr(rhs, true)?;
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

    fn gen_singular(&mut self, dst: Option<BcLocal>, lhs: &Expr) -> Result<(BcReg, BcReg)> {
        let lhs = match lhs.is_local() {
            Some(lhs) => self.find_local(&lhs).into(),
            None => self.gen_temp_expr(lhs)?.into(),
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }

    fn gen_add(&mut self, dst: Option<BcLocal>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(dst, lhs)?;
            self.insts.push(BcIr::Addri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
            self.insts.push(BcIr::Add(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_sub(&mut self, dst: Option<BcLocal>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(dst, lhs)?;
            self.insts.push(BcIr::Subri(dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
            self.insts.push(BcIr::Sub(dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_mul(&mut self, dst: Option<BcLocal>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(BcIr::Mul(dst, lhs, rhs));
        Ok(())
    }

    fn gen_div(&mut self, dst: Option<BcLocal>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(BcIr::Div(dst, lhs, rhs));
        Ok(())
    }

    fn gen_cmp(
        &mut self,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<()> {
        if let Some(i) = rhs.is_smi() {
            let (dst, lhs) = self.gen_singular(dst, lhs)?;
            self.insts.push(BcIr::Cmpri(kind, dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
            self.insts.push(BcIr::Cmp(kind, dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_store_expr(&mut self, local: BcLocal, rhs: &Expr, use_value: bool) -> Result<()> {
        match rhs {
            Expr::Nil => {
                self.gen_nil(Some(local));
            }
            Expr::Integer(i) => {
                self.gen_integer(Some(local), *i);
            }
            Expr::Float(f) => {
                self.gen_float(Some(local), *f);
            }
            Expr::Neg(box (rhs, _)) => {
                match rhs {
                    Expr::Integer(i) => self.gen_integer(Some(local), -i),
                    Expr::Float(f) => self.gen_float(Some(local), -f),
                    _ => {
                        self.gen_store_expr(local, rhs, false)?;
                        self.gen_neg(Some(local));
                    }
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(Some(local), lhs, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(Some(local), lhs, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(Some(local), lhs, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(Some(local), lhs, rhs)?;
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(Some(local), *kind, lhs, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let local2 = self.find_local(ident);
                self.gen_store_expr(local2, rhs, false)?;
                self.gen_mov(local.into(), local2.into());
            }
            Expr::LocalLoad(ident) => {
                let local2 = self.load_local(ident)?;
                self.gen_mov(local.into(), local2.into());
            }
            Expr::Call(name, args) => {
                let func = *self.func_map.get(name).unwrap();
                let arg = self.gen_args(args)?;
                self.temp -= args.len() as u16;
                let inst = BcIr::Call(func, Some(local.into()), arg, args.len());
                self.insts.push(inst);
            }
            Expr::Return(_) => unreachable!(),
            rhs => {
                let ret = self.next_reg();
                self.gen_expr(rhs, true)?;
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

    fn gen_while(&mut self, cond: &Expr, body: &[(Expr, Span)]) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        self.apply_label(cond_pos);
        let cond = self.gen_temp_expr(cond)?.into();
        let inst = BcIr::CondNotBr(cond, succ_pos);
        self.insts.push(inst);
        self.gen_exprs(body, false)?;
        self.insts.push(BcIr::Br(cond_pos));
        self.apply_label(succ_pos);
        Ok(())
    }
}

impl BcFunc {
    fn get_index(&self, reg: &BcReg) -> u16 {
        match reg {
            BcReg::Temp(i) => self.locals.len() as u16 + i.0,
            BcReg::Local(i) => i.0,
        }
    }

    fn gen_bc_from_inst(&mut self) {
        let mut ops = vec![];
        for inst in &self.insts {
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
                BcIr::Float(reg, num) => BcOp::Const(self.get_index(reg), Value::float(*num)),
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
                BcIr::Cmp(kind, dst, lhs, rhs) => BcOp::Cmp(
                    *kind,
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Cmpri(kind, dst, lhs, rhs) => {
                    BcOp::Cmpri(*kind, self.get_index(dst), self.get_index(lhs), *rhs)
                }
                BcIr::Ret(reg) => BcOp::Ret(self.get_index(reg)),
                BcIr::Mov(dst, src) => BcOp::Mov(self.get_index(dst), self.get_index(src)),
                BcIr::Call(id, ret, arg, len) => BcOp::Call(
                    *id,
                    ret.map(|r| self.get_index(&r)),
                    self.get_index(&BcReg::from(*arg)),
                    *len,
                ),
            };
            ops.push(op);
        }
        self.bc = ops;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum BcOp {
    Br(InstId),
    CondBr(u16, InstId),
    CondNotBr(u16, InstId),
    Integer(u16, i32),
    Const(u16, Value),
    Nil(u16),
    Neg(u16, u16),                 // ret, src
    Add(u16, u16, u16),            // ret, lhs, rhs
    Addri(u16, u16, i16),          // ret, lhs, rhs
    Sub(u16, u16, u16),            // ret, lhs, rhs
    Subri(u16, u16, i16),          // ret, lhs, rhs
    Mul(u16, u16, u16),            // ret, lhs, rhs
    Div(u16, u16, u16),            // ret, lhs, rhs
    Cmp(CmpKind, u16, u16, u16),   // kind, lhs, rhs
    Cmpri(CmpKind, u16, u16, i16), // kind, lhs, rhs
    Ret(u16),
    Mov(u16, u16),                           // dst, offset
    Call(BcFuncId, Option<u16>, u16, usize), // (id, ret, args, args_len)
}
