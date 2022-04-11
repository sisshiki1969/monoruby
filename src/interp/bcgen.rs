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
        self.functions.push(BcFunc::new(name, args.clone()));
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
                Stmt::Expr(expr) => self.gen_expr_no(&expr.0)?,
                Stmt::Decl(decl) => self.gen_decl_no(&decl.0)?,
            }
        }
        match &ast[len - 1].0 {
            Stmt::Expr(expr) => self.gen_expr(&expr.0),
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
    id: usize,
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
    fn new(name: String, args: Vec<String>) -> Self {
        Self {
            id: 0,
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

    fn next_reg(&self) -> Reg {
        Reg(self.temp)
    }

    fn push(&mut self) -> Reg {
        let reg = Reg(self.temp);
        self.temp += 1;
        if self.temp > self.reg_num {
            self.reg_num = self.temp;
        }
        reg
    }

    fn pop(&mut self) -> Reg {
        self.temp -= 1;
        Reg(self.temp)
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
        self.insts.push(Inst::Integer(reg, i));
    }

    fn gen_float(&mut self, f: f64) {
        let reg = self.push();
        self.insts.push(Inst::Float(reg, f));
    }

    fn gen_nil(&mut self) {
        let reg = self.push();
        self.insts.push(Inst::Nil(reg));
    }

    fn gen_neg(&mut self) -> Result<()> {
        let src = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Neg(dst, src));
        Ok(())
    }

    fn gen_add(&mut self) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Add(dst, lhs, rhs));
        Ok(())
    }

    fn gen_sub(&mut self) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Sub(dst, lhs, rhs));
        Ok(())
    }

    fn gen_mul(&mut self) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Mul(dst, lhs, rhs));
        Ok(())
    }

    fn gen_div(&mut self) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Div(dst, lhs, rhs));
        Ok(())
    }

    fn gen_cmp(&mut self, kind: CmpKind) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let dst = self.push();
        self.insts.push(Inst::Cmp(kind, dst, lhs, rhs));
        Ok(())
    }

    fn gen_ret(&mut self) -> Result<()> {
        let ret = self.pop();
        self.insts.push(Inst::Ret(ret));
        Ok(())
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
            self.gen_exprs(ast)?;
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

    fn gen_exprs(&mut self, ast: &[(Expr, Span)]) -> Result<()> {
        let len = ast.len();
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr_no(&expr)?;
        }
        self.gen_expr(&ast[len - 1].0)
    }

    fn gen_exprs_no(&mut self, ast: &[(Expr, Span)]) -> Result<()> {
        for (expr, _) in ast {
            self.gen_expr_no(&expr)?;
        }
        Ok(())
    }
    /// Generate bytecode from an *Expr*.
    fn gen_expr(&mut self, ast: &Expr) -> Result<()> {
        match ast {
            Expr::Integer(i) => Ok(self.gen_integer(*i)),
            Expr::Float(f) => Ok(self.gen_float(*f)),
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(i) => return Ok(self.gen_integer(-i)),
                    Expr::Float(f) => return Ok(self.gen_float(-f)),
                    _ => {}
                };
                self.gen_expr(lhs)?;
                self.gen_neg()
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.gen_add()
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.gen_sub()
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.gen_cmp(*kind)
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.gen_mul()
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.gen_div()
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                self.gen_expr(rhs)?;
                let rhs = self.pop();
                let ret = self.push();
                let local = self.find_local(ident);
                self.insts.push(Inst::LocalStore(Some(ret), local, rhs));
                Ok(())
            }
            Expr::LocalLoad(ident) => {
                let local = match self.locals.get(ident) {
                    Some(local) => Local(*local),
                    None => return Err(BcErr::UndefinedLocal(ident.to_owned())),
                };
                let lhs = self.push();
                self.insts.push(Inst::LocalLoad(local, lhs));
                Ok(())
            }
            Expr::Call(name, args) => {
                for arg in args {
                    self.gen_expr(&arg.0)?;
                }
                self.temp -= args.len();
                let ret = self.push();
                let func = *self.func_map.get(name).unwrap();
                self.insts
                    .push(Inst::Call(func, Some(ret), ret, args.len()));
                Ok(())
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_expr(cond_)?;
                let cond = self.pop();
                let inst = Inst::CondBr(cond, then_pos);
                self.insts.push(inst);
                // generate bb for else clause
                // return value of else clause.
                self.gen_exprs(else_)?;
                // terminal bb of else clause.
                self.insts.push(Inst::Br(succ_pos));
                self.pop();
                // generate bb for then clause
                // return value of then clause.
                self.apply_label(then_pos);
                self.gen_exprs(then_)?;
                // terminal bb of then clause.
                //self.insts.push(Inst::Br(succ_pos));

                self.apply_label(succ_pos);
                Ok(())
            }
            /*Expr::While(box (cond, _), body) => self.gen_while(cond, body),*/
            _ => unreachable!(),
        }
    }

    /// Generate bytecode from an *Expr* without returning value.
    fn gen_expr_no(&mut self, ast: &Expr) -> Result<()> {
        match ast {
            Expr::Integer(_i) => Ok(()),
            Expr::Float(_f) => Ok(()),
            Expr::Neg(box (lhs, _)) => self.gen_expr_no(lhs),
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_no(lhs)?;
                self.gen_expr_no(rhs)?;
                Ok(())
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_no(lhs)?;
                self.gen_expr_no(rhs)?;
                Ok(())
            }
            Expr::Cmp(_kind, box (lhs, _), box (rhs, _)) => {
                self.gen_expr_no(lhs)?;
                self.gen_expr_no(rhs)?;
                Ok(())
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_no(lhs)?;
                self.gen_expr_no(rhs)?;
                Ok(())
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_no(lhs)?;
                self.gen_expr_no(rhs)?;
                Ok(())
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                self.gen_expr(rhs)?;
                let rhs = self.pop();
                let local = self.find_local(ident);
                self.insts.push(Inst::LocalStore(None, local, rhs));
                Ok(())
            }
            Expr::LocalLoad(ident) => match self.locals.get(ident) {
                Some(_local) => Ok(()),
                None => return Err(BcErr::UndefinedLocal(ident.to_owned())),
            },
            Expr::Call(name, args) => {
                let arg = self.next_reg();
                for arg in args {
                    self.gen_expr(&arg.0)?;
                }
                self.temp -= args.len();
                let func = *self.func_map.get(name).unwrap();
                self.insts.push(Inst::Call(func, None, arg, args.len()));
                Ok(())
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_expr(cond_)?;
                let cond = self.pop();
                let inst = Inst::CondBr(cond, then_pos);
                self.insts.push(inst);
                // generate bb for else clause
                // return value of else clause.
                self.gen_exprs_no(else_)?;
                // terminal bb of else clause.
                self.insts.push(Inst::Br(succ_pos));
                // generate bb for then clause
                // return value of then clause.
                self.apply_label(then_pos);
                self.gen_exprs_no(then_)?;
                // terminal bb of then clause.
                self.insts.push(Inst::Br(succ_pos));

                self.apply_label(succ_pos);
                Ok(())
            }
            Expr::While(box (cond, _), body) => self.gen_while(cond, body),
            //_ => unreachable!(),
        }
    }
}

impl BcGen {
    fn gen_while(&mut self, cond: &Expr, body: &[(Expr, Span)]) -> Result<()> {
        let cond_pos = self.new_label();
        let succ_pos = self.new_label();
        self.apply_label(cond_pos);
        self.gen_expr(cond)?;
        let cond = self.pop();
        let inst = Inst::CondNotBr(cond, succ_pos);
        self.insts.push(inst);
        self.gen_exprs_no(body)?;
        self.insts.push(Inst::Br(cond_pos));
        self.apply_label(succ_pos);
        self.gen_nil();
        Ok(())
    }
}
