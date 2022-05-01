use std::collections::HashMap;

use super::parse::Span;
use super::*;

type Result<T> = std::result::Result<T, HirErr>;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Hir {
    Br(HirBBId),
    CondBr(SsaReg, HirBBId, HirBBId),
    CmpBr(CmpKind, SsaReg, HirOperand, HirBBId, HirBBId),
    Phi(SsaReg, Vec<(HirBBId, SsaReg)>), // ret, [(bb, reg)]
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    Nil(SsaReg),
    Bool(SsaReg, bool),
    Neg(HirUnop),
    Add(HirBinop2),
    Sub(HirBinop2),
    Mul(HirBinop2),
    Div(HirBinop2),
    Cmp(CmpKind, HirBinop2),
    Ret(HirOperand),
    LocalStore(Option<SsaReg>, usize, HirOperand), // (ret, offset, rhs)
    LocalLoad(usize, SsaReg),
    Call(HirFuncId, Option<SsaReg>, Vec<HirOperand>), // (id, ret, arg)
}

///
/// Binary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HirBinop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: SsaReg,
    /// Register ID of right-hand side.
    pub rhs: SsaReg,
}

///
/// Binary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HirBinop2 {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: HirOperand,
    /// Register ID of right-hand side.
    pub rhs: HirOperand,
}

///
/// Unary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HirUnop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of source value.
    pub src: HirOperand,
}

#[derive(Clone, PartialEq)]
pub enum HirOperand {
    Reg(SsaReg),
    Const(Value),
}

impl std::fmt::Debug for HirOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%{}", r.to_usize()),
            Self::Const(c) => write!(f, "{:?}", c),
        }
    }
}

///
/// ID of SSA registers.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaReg(usize);

impl std::fmt::Debug for SsaReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl SsaReg {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn to_usize(self) -> usize {
        self.0
    }
}

///
/// ID of HIR function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct HirFuncId(usize);

///
/// ID of HIR basic block.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct HirBBId(usize);

impl HirBBId {
    pub fn to_usize(&self) -> usize {
        self.0
    }
}
/*
///
/// A state of Hir.
///
#[derive(Clone, PartialEq)]
pub struct HirContext {
    /// Functions.
    pub functions: Vec<HirFunction>,
    cur_fn: HirFuncId,
    pub func_map: HashMap<String, HirFuncId>,
}

impl std::fmt::Debug for HirContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HirContxt {{")?;

        for func in &self.functions {
            writeln!(
                f,
                "\tFunction {} ({:?}) {:?}{{",
                func.name, func.args, func.ast
            )?;
            for (i, bb) in func.basic_block.iter().enumerate() {
                writeln!(f, "\t\tBasicBlock {:?} {{ owner:{:?}", i, bb.owner_function)?;
                for hir in &bb.insts {
                    let s = match hir {
                        Hir::Integer(ret, i) => format!("{:?} = {}: i32", ret, i),
                        Hir::Float(ret, f) => format!("{:?} = {}: f64", ret, f),
                        Hir::Nil(ret) => format!("{:?} = nil", ret),
                        Hir::Bool(ret, b) => format!("{:?} = {:?}", ret, b),
                        Hir::Neg(op) => {
                            format!("{:?} = neg {:?}", op.ret, op.src)
                        }
                        Hir::Add(op) => format!("{:?} = add {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Sub(op) => format!("{:?} = sub {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Mul(op) => format!("{:?} = mul {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Div(op) => format!("{:?} = div {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Cmp(kind, op) => {
                            format!("{:?} = icmp {:?} {:?}, {:?}", op.ret, kind, op.lhs, op.rhs)
                        }
                        Hir::Ret(ret) => format!("ret {:?}", ret),
                        Hir::LocalStore(ret, ident, rhs) => {
                            if let Some(ret) = ret {
                                format!("${} | {:?} = {:?}", ident, ret, rhs)
                            } else {
                                format!("${} = {:?}", ident, rhs)
                            }
                        }
                        Hir::LocalLoad(ident, lhs) => {
                            format!("{:?} = ${}", lhs, ident)
                        }
                        Hir::Call(id, ret, arg) => {
                            let name = &self[*id].name;
                            match ret {
                                Some(ret) => format!("{:?} = call {} ({:?})", ret, name, arg),
                                None => format!("%_ = call {} ({:?})", name, arg),
                            }
                        }
                        Hir::Br(dest) => format!("br {:?}", dest),
                        Hir::CmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmpbr ({:?} {:?}, {:?}) then {:?} else {:?}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Hir::CondBr(cond, then_, else_) => {
                            format!("condbr {:?} then {:?} else {:?}", cond, then_, else_)
                        }
                        Hir::Phi(ret, phi) => {
                            let phi_s = phi
                                .iter()
                                .map(|(bb, r)| format!("({:?}, {:?})", bb, r))
                                .collect::<Vec<String>>()
                                .join(", ");
                            format!("{:?} = phi {}", ret, phi_s)
                        }
                    };
                    writeln!(f, "\t\t\t{}", s)?;
                }
                writeln!(f, "\t\t}}")?;
            }
            writeln!(f, "\t}}")?;
        }
        writeln!(f, "}}")
    }
}

impl std::ops::Deref for HirContext {
    type Target = HirFunction;

    fn deref(&self) -> &Self::Target {
        let cur_fn = self.cur_fn;
        &self[cur_fn]
    }
}

impl std::ops::DerefMut for HirContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let cur_fn = self.cur_fn;
        &mut self[cur_fn]
    }
}

impl std::ops::Index<HirFuncId> for HirContext {
    type Output = HirFunction;

    fn index(&self, i: HirFuncId) -> &HirFunction {
        &self.functions[i.0]
    }
}

impl std::ops::IndexMut<HirFuncId> for HirContext {
    fn index_mut(&mut self, i: HirFuncId) -> &mut HirFunction {
        &mut self.functions[i.0]
    }
}

impl HirContext {
    pub fn new() -> Self {
        let cur_fn = HirFuncId(0);
        let function = HirFunction::new(cur_fn, "/main".to_string(), vec![]);
        HirContext {
            functions: vec![function],
            cur_fn,
            func_map: HashMap::default(),
        }
    }

    fn next_fn(&self) -> HirFuncId {
        HirFuncId(self.functions.len())
    }

    fn enter_new_func(&mut self, name: String, args: Vec<String>) -> HirFuncId {
        let next_fn = self.next_fn();
        let func = HirFunction::new(next_fn, name.clone(), args);
        self.func_map.insert(name, next_fn);
        self.functions.push(func);
        self.cur_fn = next_fn;
        next_fn
    }

    fn new_call(&mut self, name: &str, args: Vec<HirOperand>) -> Result<SsaReg> {
        let ret = self.next_reg();
        let id = self.get_function(&name)?;
        Ok(self.add_assign(Hir::Call(id, Some(ret), args)))
    }

    fn new_call_nouse(&mut self, name: &str, args: Vec<HirOperand>) -> Result<()> {
        let id = self.get_function(&name)?;
        let hir = Hir::Call(id, None, args);
        self.insts.push(hir);
        Ok(())
    }

    fn get_function(&mut self, name: &str) -> Result<HirFuncId> {
        let id = self
            .func_map
            .get(name)
            .ok_or(HirErr::UndefinedMethod(name.to_string()))?;
        Ok(*id)
    }
}

#[derive(Clone, PartialEq)]
pub struct HirFunction {
    /// ID of this function.
    pub id: HirFuncId,
    /// name of this function.
    pub name: String,
    /// return values (SSA reg) of this function.
    ret: Vec<SsaReg>,
    /// local variables.
    pub locals: HashMap<String, usize>,
    /// the name of arguments.
    pub args: Vec<String>,
    /// The number of registers.
    regs: usize,
    /// AST.
    pub ast: Vec<(Stmt, Span)>,
    /// Basic blocks.
    pub basic_block: Vec<HirBasicBlock>,
    /// current BB.
    cur_bb: HirBBId,
}

impl std::ops::Deref for HirFunction {
    type Target = HirBasicBlock;

    fn deref(&self) -> &Self::Target {
        let cur_bb = self.cur_bb;
        &self[cur_bb]
    }
}

impl std::ops::DerefMut for HirFunction {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let cur_bb = self.cur_bb;
        &mut self[cur_bb]
    }
}

impl std::ops::Index<HirBBId> for HirFunction {
    type Output = HirBasicBlock;

    fn index(&self, i: HirBBId) -> &HirBasicBlock {
        &self.basic_block[i.0]
    }
}

impl std::ops::IndexMut<HirBBId> for HirFunction {
    fn index_mut(&mut self, i: HirBBId) -> &mut HirBasicBlock {
        &mut self.basic_block[i.0]
    }
}

impl HirFunction {
    fn new(id: HirFuncId, name: String, args: Vec<String>) -> Self {
        let mut locals = HashMap::default();
        let mut len = 0;
        args.iter().for_each(|arg| {
            locals.insert(arg.to_string(), len);
            len += 1;
        });
        Self {
            id,
            name,
            ret: vec![],
            locals,
            args,
            regs: 0,
            ast: vec![],
            basic_block: vec![HirBasicBlock::new(id)],
            cur_bb: HirBBId::default(),
        }
    }

    fn add_assign(&mut self, hir: Hir) -> SsaReg {
        let ret_reg = self.next_reg();
        self.regs += 1;
        self.insts.push(hir);
        ret_reg
    }

    fn next_reg(&self) -> SsaReg {
        SsaReg(self.register_num())
    }

    pub fn register_num(&self) -> usize {
        self.regs
    }

    fn next_bb(&self) -> HirBBId {
        HirBBId(self.basic_block.len())
    }

    fn new_bb(&mut self) -> HirBBId {
        let cur_fn = self.id;
        let bb = HirBasicBlock::new(cur_fn);
        let next = self.next_bb();
        self.basic_block.push(bb);
        next
    }

    fn new_integer(&mut self, i: i32) -> SsaReg {
        self.add_assign(Hir::Integer(self.next_reg(), i))
    }

    fn new_float(&mut self, f: f64) -> SsaReg {
        self.add_assign(Hir::Float(self.next_reg(), f))
    }

    fn new_nil(&mut self) -> SsaReg {
        self.add_assign(Hir::Nil(self.next_reg()))
    }

    fn new_bool(&mut self, b: bool) -> SsaReg {
        self.add_assign(Hir::Bool(self.next_reg(), b))
    }

    fn new_neg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Neg(HirUnop {
            ret,
            src: HirOperand::Reg(src),
        }))
    }

    fn new_add(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Add(HirBinop2 { ret, lhs, rhs }))
    }

    fn new_sub(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Sub(HirBinop2 { ret, lhs, rhs }))
    }

    fn new_mul(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Mul(HirBinop2 { ret, lhs, rhs }))
    }

    fn new_div(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Div(HirBinop2 { ret, lhs, rhs }))
    }

    fn new_cmp(&mut self, kind: CmpKind, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Cmp(kind, HirBinop2 { ret, lhs, rhs }))
    }

    fn new_local_store_nouse(&mut self, ident: &String, rhs: HirOperand) -> Result<()> {
        let info = self.add_local_var_if_new(ident);
        let hir = Hir::LocalStore(None, info, rhs);
        self.insts.push(hir);
        Ok(())
    }

    fn new_local_store(&mut self, ident: &String, rhs: HirOperand) -> Result<SsaReg> {
        let info = self.add_local_var_if_new(ident);
        let ret = self.next_reg();
        self.add_assign(Hir::LocalStore(Some(ret), info, rhs));
        Ok(ret)
    }

    fn add_local_var_if_new(&mut self, ident: &String) -> usize {
        let len = self.locals.len();
        match self.locals.get(ident) {
            Some(info) => *info,
            None => {
                self.locals.insert(ident.to_string(), len);
                len
            }
        }
    }

    fn new_local_load(&mut self, ident: &String) -> Result<SsaReg> {
        let info = match self.locals.get(ident) {
            Some(info) => *info,
            None => return Err(HirErr::UndefinedLocal(ident.clone())),
        };
        let hir = Hir::LocalLoad(info, self.next_reg());
        Ok(self.add_assign(hir))
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = Hir::Ret(HirOperand::Reg(lhs));
        self.insts.push(hir);
    }

    fn new_phi(&mut self, phi: Vec<(HirBBId, SsaReg)>) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Phi(ret, phi))
    }
}

#[derive(Clone, PartialEq)]
pub struct HirBasicBlock {
    /// HIR instructions.
    pub insts: Vec<Hir>,
    /// The function this bb is owned.
    pub owner_function: HirFuncId,
}

impl HirBasicBlock {
    fn new(owner_function: HirFuncId) -> Self {
        Self {
            insts: vec![],
            owner_function,
        }
    }
}

#[derive(Debug, Clone)]
pub enum HirErr {
    UndefinedLocal(String),
    UndefinedMethod(String),
}

impl HirContext {
    /// Generate HIR in top level from [(Stmt, Span)].
    pub fn from_ast(&mut self, ast: &[(Stmt, Span)]) -> Result<()> {
        assert_eq!(0, self.cur_fn.0);
        let len = ast.len();
        let ret = if len == 0 {
            self.new_nil()
        } else {
            self.gen_stmts(ast)?
        };
        self.new_ret(ret);
        self.ret.push(ret);
        self.ast = ast.to_vec();
        Ok(())
    }

    /// Generate HIR in new function from [(Stmt, Span)].
    pub fn new_func_from_ast(
        &mut self,
        func_name: String,
        args: Vec<String>,
        ast: &[(Expr, Span)],
    ) -> Result<HirFuncId> {
        let save = (self.cur_fn, self.cur_bb);
        let func = self.enter_new_func(func_name, args);

        let len = ast.len();
        let ret = if len == 0 {
            self.new_nil()
        } else {
            let ast = ast
                .iter()
                .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
                .collect::<Vec<(Stmt, Span)>>();
            self.gen_stmts(&ast)?
        };
        self.new_ret(ret);
        self.ret.push(ret);
        self.ast = ast
            .iter()
            .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
            .collect();
        (self.cur_fn, self.cur_bb) = save;
        Ok(func)
    }

    fn gen_operand(&mut self, expr: &Expr) -> Result<HirOperand> {
        let op = match expr {
            Expr::Integer(i) => HirOperand::Const(Value::integer(*i)),
            Expr::Float(f) => HirOperand::Const(Value::float(*f)),
            _ => HirOperand::Reg(self.gen_expr(expr)?),
        };
        Ok(op)
    }

    /// Generate HIR from [(Stmt, Span)].
    fn gen_stmts(&mut self, ast: &[(Stmt, Span)]) -> Result<SsaReg> {
        let len = ast.len();
        for node in &ast[..len - 1] {
            self.gen_stmt_nouse(node)?;
        }
        self.gen_stmt(&ast[len - 1])
    }

    fn gen_stmt(&mut self, ast: &(Stmt, Span)) -> Result<SsaReg> {
        match &ast.0 {
            Stmt::Expr(expr) => self.gen_expr(&expr.0),
            Stmt::Decl(decl) => self.gen_decl(&decl.0),
        }
    }

    fn gen_stmt_nouse(&mut self, ast: &(Stmt, Span)) -> Result<()> {
        match &ast.0 {
            Stmt::Expr(expr) => self.gen_expr_nouse(&expr.0),
            Stmt::Decl(decl) => self.gen_decl_nouse(&decl.0),
        }
    }

    fn gen_exprs(&mut self, ast: &[(Expr, Span)]) -> Result<SsaReg> {
        let len = ast.len();
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr_nouse(&expr)?;
        }
        self.gen_expr(&ast[len - 1].0)
    }

    fn gen_exprs_nouse(&mut self, ast: &[(Expr, Span)]) -> Result<()> {
        for (expr, _) in ast {
            self.gen_expr_nouse(&expr)?;
        }
        Ok(())
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr(&mut self, ast: &Expr) -> Result<SsaReg> {
        match ast {
            Expr::Nil => Ok(self.new_nil()),
            Expr::True => Ok(self.new_bool(true)),
            Expr::False => Ok(self.new_bool(false)),
            Expr::Self_ => unimplemented!(),
            Expr::Integer(i) => Ok(self.new_integer(*i)),
            Expr::Float(f) => Ok(self.new_float(*f)),
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(i) => return Ok(self.new_integer(-i)),
                    Expr::Float(f) => return Ok(self.new_float(-f)),
                    _ => {}
                };
                let lhs_i = self.gen_expr(lhs)?;
                let ssa = self.new_neg(lhs_i);
                Ok(ssa)
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(lhs)?;
                let rhs = self.gen_operand(rhs)?;
                Ok(self.new_add(lhs, rhs))
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(lhs)?;
                let rhs = self.gen_operand(rhs)?;
                Ok(self.new_sub(lhs, rhs))
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(lhs)?;
                let rhs = self.gen_operand(rhs)?;
                Ok(self.new_cmp(*kind, lhs, rhs))
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(lhs)?;
                let rhs = self.gen_operand(rhs)?;
                Ok(self.new_mul(lhs, rhs))
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(lhs)?;
                let rhs = self.gen_operand(rhs)?;
                Ok(self.new_div(lhs, rhs))
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_operand(rhs)?;
                self.new_local_store(ident, rhs)
            }
            Expr::LocalLoad(ident) => self.new_local_load(ident),
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(&arg.0)?;
                    arg_regs.push(reg);
                }
                self.new_call(name, arg_regs)
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let else_bb = self.new_bb();
                let then_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb)?;
                // generate bb for else clause
                self.cur_bb = else_bb;
                // return value of else clause.
                let else_reg = self.gen_exprs(else_)?;
                // terminal bb of else clause.
                let else_bb = self.cur_bb;
                self.insts.push(Hir::Br(succ_bb));

                // generate bb for then clause
                self.cur_bb = then_bb;
                // return value of then clause.
                let then_reg = self.gen_exprs(then_)?;
                // terminal bb of then clause.
                let then_bb = self.cur_bb;
                self.insts.push(Hir::Br(succ_bb));

                // generate phi on the top of successor bb.
                self.cur_bb = succ_bb;
                let ret = self.new_phi(vec![(then_bb, then_reg), (else_bb, else_reg)]);
                Ok(ret)
            }
            Expr::While(box (cond, _), body) => self.gen_while(cond, body),
            Expr::Return(box stmt) => {
                let ret = self.gen_stmt(stmt)?;
                self.new_ret(ret);
                Ok(ret)
            }
        }
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr_nouse(&mut self, ast: &Expr) -> Result<()> {
        match ast {
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(_) | Expr::Float(_) => {}
                    _ => self.gen_expr_nouse(lhs)?,
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(lhs)?;
                self.gen_expr_nouse(rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(lhs)?;
                self.gen_expr_nouse(rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(lhs)?;
                self.gen_expr_nouse(rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(lhs)?;
                self.gen_expr_nouse(rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_operand(rhs)?;
                self.new_local_store_nouse(ident, rhs)?;
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_bb = self.new_bb();
                let else_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb)?;

                self.cur_bb = else_bb;
                self.gen_exprs_nouse(else_)?;
                self.insts.push(Hir::Br(succ_bb));

                self.cur_bb = then_bb;
                self.gen_exprs_nouse(then_)?;
                self.insts.push(Hir::Br(succ_bb));

                self.cur_bb = succ_bb;
            }
            Expr::While(box (cond, _), body) => {
                let _ = self.gen_while(cond, body)?;
            }
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(&arg.0)?;
                    arg_regs.push(reg);
                }
                self.new_call_nouse(name, arg_regs)?;
            }
            Expr::Nil
            | Expr::True
            | Expr::False
            | Expr::Self_
            | Expr::Integer(_)
            | Expr::Float(_)
            | Expr::LocalLoad(_)
            | Expr::Cmp(_, _, _) => {}
            Expr::Return(box stmt) => {
                let ret = self.gen_stmt(stmt)?;
                self.new_ret(ret);
            }
        };
        Ok(())
    }

    fn gen_decl(&mut self, decl: &Decl) -> Result<SsaReg> {
        self.gen_decl_nouse(decl)?;
        Ok(self.new_nil())
    }

    fn gen_decl_nouse(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::MethodDef(name, arg_name, body) => {
                let _ = self.new_func_from_ast(name.to_string(), arg_name.clone(), body)?;
                Ok(())
            }
        }
    }

    fn gen_cond(&mut self, cond_: &Expr, then_bb: HirBBId, else_bb: HirBBId) -> Result<()> {
        if let Expr::Cmp(kind, box (lhs, _), box (rhs, _)) = cond_ {
            let lhs = self.gen_expr(lhs)?;
            let rhs = self.gen_operand(rhs)?;
            self.insts
                .push(Hir::CmpBr(*kind, lhs, rhs, then_bb, else_bb));
        } else {
            let cond_ = self.gen_expr(cond_)?;
            self.insts.push(Hir::CondBr(cond_, then_bb, else_bb));
        }
        Ok(())
    }

    fn gen_while(&mut self, cond: &Expr, body: &[(Expr, Span)]) -> Result<SsaReg> {
        let cond_bb = self.new_bb();
        self.insts.push(Hir::Br(cond_bb));
        self.cur_bb = cond_bb;
        let body_bb = self.new_bb();
        let succ_bb = self.new_bb();
        self.gen_cond(cond, body_bb, succ_bb)?;
        self.cur_bb = body_bb;
        self.gen_exprs_nouse(body)?;
        self.insts.push(Hir::Br(cond_bb));
        self.cur_bb = succ_bb;
        let ret = self.new_nil();
        Ok(ret)
    }
}
*/
