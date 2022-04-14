use super::*;

pub type Result<T> = std::result::Result<T, BcErr>;

///
/// A state of Bytecode generator.
///
#[derive(Debug, Clone, PartialEq)]
pub struct BcGen {
    /// Functions.
    functions: Vec<BcFunc>,
    func_map: HashMap<String, BcFuncId>,
    cur_fn: BcFuncId,
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

    pub fn new_bc(ast: &[(Stmt, Span)]) -> Result<Self> {
        let mut gen = Self::new();
        let _func = gen.new_func("main".to_string(), vec![])?;
        let len = ast.len();
        if len == 0 {
            gen.gen_nil()
        } else {
            gen.gen_stmts(ast)?;
        };
        gen.gen_ret()?;
        gen.ast = ast.to_vec();
        eprintln!("{:?}", &gen.insts);
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
        for (node, _) in &ast[..len - 1] {
            match node {
                Stmt::Expr(expr) => self.gen_expr(&expr.0, false)?,
                Stmt::Decl(decl) => self.gen_decl_no(&decl.0)?,
            }
        }
        match &ast[len - 1].0 {
            Stmt::Expr(expr) => self.gen_expr(&expr.0, true),
            Stmt::Decl(decl) => self.gen_decl(&decl.0),
        }
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
    locals: HashMap<String, usize>,
    /// The number of registers.
    temp: usize,
    /// The number of registers.
    pub reg_num: usize,
    /// AST.
    ast: Vec<(Stmt, Span)>,
    /// Bytecode.
    insts: Vec<Inst>,
    /// Labels.
    labels: Vec<Option<InstId>>,
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
        }
    }
    pub(super) fn local_num(&self) -> usize {
        self.locals.len()
    }

    pub(super) fn insts(&self) -> &[Inst] {
        &self.insts
    }

    pub(super) fn labels(&self) -> &[Option<InstId>] {
        &self.labels
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

    fn next_reg(&self) -> Temp {
        Temp(self.temp)
    }

    fn push(&mut self) -> Temp {
        let reg = Temp(self.temp);
        self.temp += 1;
        if self.temp > self.reg_num {
            self.reg_num = self.temp;
        }
        reg
    }

    fn pop(&mut self) -> Temp {
        self.temp -= 1;
        Temp(self.temp)
    }

    fn load_local(&mut self, ident: &String) -> Result<Local> {
        match self.locals.get(ident) {
            Some(local) => Ok(Local(*local)),
            None => Err(BcErr::UndefinedLocal(ident.to_owned())),
        }
    }

    fn find_local(&mut self, ident: &String) -> Local {
        match self.locals.get(ident) {
            Some(local) => Local(*local),
            None => self.add_local(ident),
        }
    }

    fn add_local(&mut self, ident: &String) -> Local {
        let local = self.locals.len();
        self.locals.insert(ident.to_owned(), local);
        Local(local)
    }

    fn gen_integer(&mut self, i: i32) {
        let reg = self.push();
        self.insts.push(Inst::Integer(reg.into(), i));
    }

    fn gen_float(&mut self, f: f64) {
        let reg = self.push();
        self.insts.push(Inst::Float(reg, f));
    }

    fn gen_nil(&mut self) {
        let reg = self.push();
        self.insts.push(Inst::Nil(reg));
    }

    fn gen_neg(&mut self) {
        let src = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Neg(dst, src));
    }

    fn gen_ret(&mut self) -> Result<()> {
        let ret = self.pop();
        assert_eq!(0, self.temp);
        self.insts.push(Inst::Ret(ret));
        Ok(())
    }

    fn gen_mov(&mut self, dst: Reg, src: Reg) {
        self.insts.push(Inst::Mov(dst, src));
    }

    fn gen_temp_mov(&mut self, rhs: Reg) -> Reg {
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

        let len = ast.len();
        if len == 0 {
            self.gen_nil()
        } else {
            self.gen_exprs(ast, true)?;
        };
        self.gen_ret()?;
        self.ast = ast
            .iter()
            .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
            .collect();
        eprintln!("{:?}", &self.insts);
        self.cur_fn = save;
        Ok(func)
    }

    fn gen_decl(&mut self, decl: &Decl) -> Result<()> {
        self.gen_decl_no(decl)?;
        self.gen_nil();
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
        if len == 0 {
            if use_value {
                self.gen_nil();
            }
            return Ok(());
        }
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr(&expr, false)?;
        }
        self.gen_expr(&ast[len - 1].0, use_value)
    }

    fn gen_temp_expr(&mut self, expr: &Expr) -> Result<Temp> {
        self.gen_expr(expr, true)?;
        Ok(self.pop())
    }

    /// Generate bytecode from an *Expr*.
    fn gen_expr(&mut self, expr: &Expr, use_value: bool) -> Result<()> {
        match expr {
            Expr::Integer(i) => {
                self.gen_integer(*i);
            }
            Expr::Float(f) => {
                self.gen_float(*f);
            }
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(i) => self.gen_integer(-i),
                    Expr::Float(f) => self.gen_float(-f),
                    _ => {
                        self.gen_expr(lhs, true)?;
                        self.gen_neg();
                    }
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(None, lhs, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(None, lhs, rhs)?;
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(None, *kind, lhs, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(None, lhs, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(None, lhs, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                return self.gen_store_expr(ident, rhs, use_value);
            }
            Expr::LocalLoad(ident) => {
                let local = self.load_local(ident)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Call(name, args) => {
                let func = *self.func_map.get(name).unwrap();
                let arg = self.gen_args(args)?;
                self.temp -= args.len();
                let inst = if use_value {
                    let ret = self.push().into();
                    Inst::Call(func, Some(ret), arg, args.len())
                } else {
                    Inst::Call(func, None, arg, args.len())
                };
                self.insts.push(inst);
                return Ok(());
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                let cond = self.gen_temp_expr(cond_)?;
                let inst = Inst::CondBr(cond, then_pos);
                self.insts.push(inst);
                self.gen_exprs(else_, use_value)?;
                self.insts.push(Inst::Br(succ_pos));
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
                    self.gen_nil();
                }
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
    fn gen_args(&mut self, args: &[(Expr, std::ops::Range<usize>)]) -> Result<Temp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(&arg.0, true)?;
        }
        Ok(arg)
    }

    fn gen_binary(
        &mut self,
        dst: Option<Local>,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<(Reg, Reg, Reg)> {
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

    fn gen_add(&mut self, dst: Option<Local>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(Inst::Add(dst, lhs, rhs));
        Ok(())
    }

    fn gen_sub(&mut self, dst: Option<Local>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(Inst::Sub(dst, lhs, rhs));
        Ok(())
    }

    fn gen_mul(&mut self, dst: Option<Local>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(Inst::Mul(dst, lhs, rhs));
        Ok(())
    }

    fn gen_div(&mut self, dst: Option<Local>, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(Inst::Div(dst, lhs, rhs));
        Ok(())
    }

    fn gen_cmp(&mut self, dst: Option<Local>, kind: CmpKind, lhs: &Expr, rhs: &Expr) -> Result<()> {
        let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
        self.insts.push(Inst::Cmp(kind, dst, lhs, rhs));
        Ok(())
    }

    fn gen_store_expr(&mut self, ident: &String, rhs: &Expr, use_value: bool) -> Result<()> {
        let local = self.find_local(ident);
        match rhs {
            Expr::Integer(i) => {
                self.insts.push(Inst::Integer(local.into(), *i));
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_add(Some(local), lhs, rhs)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_sub(Some(local), lhs, rhs)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_mul(Some(local), lhs, rhs)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_div(Some(local), lhs, rhs)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_cmp(Some(local), *kind, lhs, rhs)?;
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            Expr::Call(name, args) => {
                let func = *self.func_map.get(name).unwrap();
                let arg = self.gen_args(args)?;
                self.temp -= args.len();
                let inst = Inst::Call(func, Some(local.into()), arg, args.len());
                self.insts.push(inst);
                if use_value {
                    self.gen_temp_mov(local.into());
                }
            }
            rhs => {
                let ret = self.next_reg();
                self.gen_expr(rhs, true)?;
                self.gen_mov(local.into(), ret.into());
                if !use_value {
                    self.pop();
                }
            }
        };
        Ok(())
    }

    fn gen_while(&mut self, cond: &Expr, body: &[(Expr, Span)]) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        self.apply_label(cond_pos);
        let cond = self.gen_temp_expr(cond)?;
        let inst = Inst::CondNotBr(cond, succ_pos);
        self.insts.push(inst);
        self.gen_exprs(body, false)?;
        self.insts.push(Inst::Br(cond_pos));
        self.apply_label(succ_pos);
        Ok(())
    }
}
