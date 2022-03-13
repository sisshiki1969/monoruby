use std::collections::{BTreeSet, HashMap};

use super::parse::Span;
use super::*;

///
/// A state of Hir.
///
#[derive(Clone, PartialEq)]
pub struct HirContext {
    /// Basic blocks.
    pub basic_block: Vec<HirBasicBlock>,
    cur_bb: usize,
    /// Functions.
    pub functions: Vec<HirFunction>,
    cur_fn: usize,
}

impl std::fmt::Debug for HirContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HirContxt {{")?;

        for func in &self.functions {
            writeln!(f, "\tFunction {} {{", func.name)?;
            for i in func.bbs.iter() {
                let bb = &self.basic_block[*i];
                writeln!(f, "\t\tBasicBlock {} {{ owner:{:?}", i, bb.owner_function)?;
                for hir in &bb.insts {
                    let s = match hir {
                        Hir::Integer(ret, i) => format!("%{} = {}: i32", ret, i),
                        Hir::Float(ret, f) => format!("%{} = {}: f64", ret, f),
                        Hir::Neg(op) => {
                            format!("%{} = neg {:?}", op.ret, op.src)
                        }
                        Hir::Add(op) => format!("%{} = add {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Sub(op) => format!("%{} = sub {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Mul(op) => format!("%{} = mul {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Div(op) => format!("%{} = div {:?}, {:?}", op.ret, op.lhs, op.rhs),
                        Hir::Cmp(kind, op) => {
                            format!("%{} = icmp {:?} {:?}, {:?}", op.ret, kind, op.lhs, op.rhs)
                        }
                        Hir::Ret(ret) => format!("ret {:?}", ret),
                        Hir::LocalStore(ret, ident, rhs) => {
                            if let Some(ret) = ret {
                                format!("${} | %{} = %{}", ident, ret, rhs)
                            } else {
                                format!("${} = %{}", ident, rhs)
                            }
                        }
                        Hir::LocalLoad(ident, lhs) => {
                            format!("%{} = ${}", lhs, ident)
                        }
                        Hir::Call(id, ret, arg) => {
                            let name = &self.functions[*id].name;
                            match ret {
                                Some(ret) => format!("%{} = call {} ({:?})", ret, name, arg),
                                None => format!("%_ = call {} ({:?})", name, arg),
                            }
                        }
                        Hir::Br(dest) => format!("br {}", dest),
                        Hir::CmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmpbr ({:?} %{}, {:?}) then {} else {}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Hir::CondBr(cond, then_, else_) => {
                            format!("condbr %{} then {} else {}", cond, then_, else_)
                        }
                        Hir::Phi(ret, phi) => {
                            let phi_s = phi
                                .iter()
                                .map(|(bb, r)| format!("({}, %{})", bb, r))
                                .collect::<Vec<String>>()
                                .join(", ");
                            format!("%{} = phi {}", ret, phi_s)
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
    type Target = HirBasicBlock;

    fn deref(&self) -> &Self::Target {
        &self.basic_block[self.cur_bb]
    }
}

impl std::ops::DerefMut for HirContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.basic_block[self.cur_bb]
    }
}

impl std::ops::Index<usize> for HirContext {
    type Output = HirBasicBlock;

    fn index(&self, i: usize) -> &HirBasicBlock {
        &self.basic_block[i]
    }
}

impl std::ops::IndexMut<usize> for HirContext {
    fn index_mut(&mut self, i: usize) -> &mut HirBasicBlock {
        &mut self.basic_block[i]
    }
}

impl HirContext {
    pub fn new() -> Self {
        let cur_bb = 0;
        let cur_fn = 0;
        let basic_block = HirBasicBlock::new(cur_fn);
        let mut function = HirFunction::new("/main".to_string(), cur_bb, vec![]);
        function.bbs.insert(cur_bb);
        HirContext {
            basic_block: vec![basic_block],
            cur_bb,
            functions: vec![function],
            cur_fn,
        }
    }

    fn new_bb(&mut self) -> usize {
        let bb = HirBasicBlock::new(self.cur_fn);
        let next = self.basic_block.len();
        self.functions[self.cur_fn].bbs.insert(next);
        self.basic_block.push(bb);
        next
    }

    fn func(&self) -> &HirFunction {
        &self.functions[self.cur_fn]
    }

    fn func_mut(&mut self) -> &mut HirFunction {
        &mut self.functions[self.cur_fn]
    }

    fn enter_new_func(&mut self, name: String, args: Vec<(String, Type)>) -> usize {
        let entry_bb = self.basic_block.len();
        let next_fn = self.functions.len();

        let bb = HirBasicBlock::new(next_fn);
        self.basic_block.push(bb);

        let mut func = HirFunction::new(name, entry_bb, args);
        func.bbs.insert(entry_bb);
        self.functions.push(func);

        self.cur_fn = next_fn;
        self.cur_bb = entry_bb;
        next_fn
    }

    fn add_assign(&mut self, hir: Hir) -> SsaReg {
        let ret_reg = self.next_reg();
        self.func_mut().regs += 1;
        self.insts.push(hir);
        ret_reg
    }

    pub fn register_num(&self) -> usize {
        self.func().regs
    }

    fn next_reg(&self) -> SsaReg {
        SsaReg(self.register_num())
    }

    fn new_integer(&mut self, i: i32) -> SsaReg {
        self.add_assign(Hir::Integer(self.next_reg(), i))
    }

    fn new_float(&mut self, f: f64) -> SsaReg {
        self.add_assign(Hir::Float(self.next_reg(), f))
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

    fn get_function(&mut self, name: &str) -> Result<usize> {
        let id = self
            .functions
            .iter()
            .enumerate()
            .find(|(_, func)| &func.name == name)
            .ok_or(HirErr::UndefinedMethod(name.to_string()))?
            .0;
        Ok(id)
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = Hir::Ret(HirOperand::Reg(lhs));
        self.insts.push(hir);
    }

    fn new_local_store(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ident: &String,
        rhs: SsaReg,
    ) -> Result<SsaReg> {
        let info = self.add_local_var_if_new(local_map, ident);
        let ret = self.next_reg();
        self.add_assign(Hir::LocalStore(Some(ret), info, rhs));
        Ok(ret)
    }

    fn add_local_var_if_new(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ident: &String,
    ) -> usize {
        let len = local_map.len();
        match local_map.get(ident) {
            Some(info) => *info,
            None => {
                local_map.insert(ident.to_string(), len);
                len
            }
        }
    }

    fn new_local_store_nouse(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ident: &String,
        rhs: SsaReg,
    ) -> Result<()> {
        let len = local_map.len();
        let info = match local_map.get(ident) {
            Some(info) => *info,
            None => {
                local_map.insert(ident.to_string(), len);
                len
            }
        };
        let hir = Hir::LocalStore(None, info, rhs);
        self.insts.push(hir);
        Ok(())
    }

    fn new_local_load(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ident: &String,
    ) -> Result<SsaReg> {
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => return Err(HirErr::UndefinedLocal(ident.clone())),
        };
        let hir = Hir::LocalLoad(info, self.next_reg());
        Ok(self.add_assign(hir))
    }

    fn new_phi(&mut self, phi: Vec<(usize, SsaReg)>) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::Phi(ret, phi))
    }
}

#[derive(Clone, PartialEq)]
pub struct HirFunction {
    pub name: String,
    pub entry_bb: usize,
    pub ret: Option<SsaReg>,
    pub bbs: BTreeSet<usize>,
    pub locals: HashMap<String, usize>,
    pub args: Vec<(String, Type)>,
    pub regs: usize,
}

impl HirFunction {
    fn new(name: String, entry_bb: usize, args: Vec<(String, Type)>) -> Self {
        Self {
            name,
            entry_bb,
            ret: None,
            bbs: BTreeSet::default(),
            locals: HashMap::default(),
            args,
            regs: 0,
        }
    }

    pub fn register_num(&self) -> usize {
        self.regs
    }
}

#[derive(Clone, PartialEq)]
pub struct HirBasicBlock {
    /// HIR instructions.
    pub insts: Vec<Hir>,
    /// The function this bb is owned.
    pub owner_function: usize,
}

impl HirBasicBlock {
    fn new(owner_function: usize) -> Self {
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

type Result<T> = std::result::Result<T, HirErr>;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Hir {
    Br(usize),
    CondBr(SsaReg, usize, usize),
    CmpBr(CmpKind, SsaReg, HirOperand, usize, usize),
    Phi(SsaReg, Vec<(usize, SsaReg)>), // ret, [(bb, reg)]
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    Neg(HirUnop),
    Add(HirBinop2),
    Sub(HirBinop2),
    Mul(HirBinop2),
    Div(HirBinop2),
    Cmp(CmpKind, HirBinop2),
    Ret(HirOperand),
    LocalStore(Option<SsaReg>, usize, SsaReg), // (ret, offset, rhs)
    LocalLoad(usize, SsaReg),
    Call(usize, Option<SsaReg>, Vec<HirOperand>), // (id, ret, arg)
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
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SsaReg(usize);

impl std::fmt::Display for SsaReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SsaReg {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl HirContext {
    /// Generate HIR in top level from [(Stmt, Span)].
    pub fn from_ast(&mut self, ast: &[(Stmt, Span)]) -> Result<()> {
        assert_eq!(0, self.cur_fn);
        let mut local_map = HashMap::default();
        let len = ast.len();
        let ret = if len == 0 {
            self.new_integer(0)
        } else {
            self.gen_stmts(&mut local_map, ast)?
        };
        self.func_mut().locals = local_map.clone();
        self.new_ret(ret);
        self.func_mut().ret = Some(ret);
        Ok(())
    }

    /// Generate HIR in new function from [(Stmt, Span)].
    pub fn new_func_from_ast(
        &mut self,
        func_name: String,
        args: Vec<(String, Type)>,
        ast: &[(Expr, Span)],
    ) -> Result<usize> {
        let save = (self.cur_fn, self.cur_bb);
        let mut local_map = HashMap::default();
        args.iter().for_each(|(arg, _)| {
            self.add_local_var_if_new(&mut local_map, arg);
        });
        let func = self.enter_new_func(func_name, args);
        let len = ast.len();
        let ret = if len == 0 {
            self.new_integer(0)
        } else {
            self.gen_stmts(
                &mut local_map,
                &ast.iter()
                    .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
                    .collect::<Vec<(Stmt, Span)>>(),
            )?
        };
        self.func_mut().locals = local_map;
        self.new_ret(ret);
        self.func_mut().ret = Some(ret);
        (self.cur_fn, self.cur_bb) = save;
        Ok(func)
    }

    fn gen_operand(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        expr: &Expr,
    ) -> Result<HirOperand> {
        let op = match expr {
            Expr::Integer(i) => HirOperand::Const(Value::Integer(*i)),
            Expr::Float(f) => HirOperand::Const(Value::Float(*f)),
            _ => HirOperand::Reg(self.gen_expr(local_map, expr)?),
        };
        Ok(op)
    }

    /// Generate HIR from [(Stmt, Span)].
    fn gen_stmts(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ast: &[(Stmt, Span)],
    ) -> Result<SsaReg> {
        let len = ast.len();
        for (node, _) in &ast[..len - 1] {
            match node {
                Stmt::Expr(expr) => self.gen_expr_nouse(local_map, &expr.0)?,
                Stmt::Decl(decl) => self.gen_decl_nouse(&decl.0)?,
            }
        }
        match &ast[len - 1].0 {
            Stmt::Expr(expr) => self.gen_expr(local_map, &expr.0),
            Stmt::Decl(decl) => self.gen_decl(&decl.0),
        }
    }

    fn gen_exprs(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ast: &[(Expr, Span)],
    ) -> Result<SsaReg> {
        let len = ast.len();
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr_nouse(local_map, &expr)?;
        }
        self.gen_expr(local_map, &ast[len - 1].0)
    }

    fn gen_exprs_nouse(
        &mut self,
        local_map: &mut HashMap<String, usize>,
        ast: &[(Expr, Span)],
    ) -> Result<()> {
        for (expr, _) in ast {
            self.gen_expr_nouse(local_map, &expr)?;
        }
        Ok(())
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr(&mut self, local_map: &mut HashMap<String, usize>, ast: &Expr) -> Result<SsaReg> {
        match ast {
            Expr::Integer(i) => Ok(self.new_integer(*i)),
            Expr::Float(f) => Ok(self.new_float(*f)),
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(i) => return Ok(self.new_integer(-i)),
                    Expr::Float(f) => return Ok(self.new_float(-f)),
                    _ => {}
                };
                let lhs_i = self.gen_expr(local_map, lhs)?;
                let ssa = self.new_neg(lhs_i);
                Ok(ssa)
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(local_map, lhs)?;
                let rhs = self.gen_operand(local_map, rhs)?;
                Ok(self.new_add(lhs, rhs))
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(local_map, lhs)?;
                let rhs = self.gen_operand(local_map, rhs)?;
                Ok(self.new_sub(lhs, rhs))
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(local_map, lhs)?;
                let rhs = self.gen_operand(local_map, rhs)?;
                Ok(self.new_cmp(*kind, lhs, rhs))
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(local_map, lhs)?;
                let rhs = self.gen_operand(local_map, rhs)?;
                Ok(self.new_mul(lhs, rhs))
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_operand(local_map, lhs)?;
                let rhs = self.gen_operand(local_map, rhs)?;
                Ok(self.new_div(lhs, rhs))
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_expr(local_map, rhs)?;
                self.new_local_store(local_map, ident, rhs)
            }
            Expr::LocalLoad(ident) => self.new_local_load(local_map, ident),
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(local_map, &arg.0)?;
                    arg_regs.push(reg);
                }
                self.new_call(name, arg_regs)
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let else_bb = self.new_bb();
                let then_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb, local_map)?;
                // generate bb for else clause
                self.cur_bb = else_bb;
                // return value of else clause.
                let else_reg = self.gen_exprs(local_map, else_)?;
                // terminal bb of else clause.
                let else_bb = self.cur_bb;
                self.insts.push(Hir::Br(succ_bb));

                // generate bb for then clause
                self.cur_bb = then_bb;
                // return value of then clause.
                let then_reg = self.gen_exprs(local_map, then_)?;
                // terminal bb of then clause.
                let then_bb = self.cur_bb;
                self.insts.push(Hir::Br(succ_bb));

                // generate phi on the top of successor bb.
                self.cur_bb = succ_bb;
                let ret = self.new_phi(vec![(then_bb, then_reg), (else_bb, else_reg)]);
                Ok(ret)
            }
            Expr::While(box (cond, _), body) => self.gen_while(cond, body, local_map),
        }
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr_nouse(&mut self, local_map: &mut HashMap<String, usize>, ast: &Expr) -> Result<()> {
        match ast {
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(_) | Expr::Float(_) => {}
                    _ => self.gen_expr_nouse(local_map, lhs)?,
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_expr(local_map, rhs)?;
                self.new_local_store_nouse(local_map, ident, rhs)?;
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_bb = self.new_bb();
                let else_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb, local_map)?;

                self.cur_bb = else_bb;
                self.gen_exprs_nouse(local_map, else_)?;
                self.insts.push(Hir::Br(succ_bb));

                self.cur_bb = then_bb;
                self.gen_exprs_nouse(local_map, then_)?;
                self.insts.push(Hir::Br(succ_bb));

                self.cur_bb = succ_bb;
            }
            Expr::While(box (cond, _), body) => {
                let _ = self.gen_while(cond, body, local_map)?;
            }
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(local_map, &arg.0)?;
                    arg_regs.push(reg);
                }
                self.new_call_nouse(name, arg_regs)?;
            }
            Expr::Integer(_) => {}
            Expr::Float(_) => {}
            Expr::LocalLoad(_) => {}
            Expr::Cmp(_, _, _) => {}
        };
        Ok(())
    }

    fn gen_decl(&mut self, decl: &Decl) -> Result<SsaReg> {
        self.gen_decl_nouse(decl)?;
        Ok(self.new_integer(0))
    }

    fn gen_decl_nouse(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::MethodDef(name, arg_name, body) => {
                let args = arg_name
                    .iter()
                    .map(|arg_name| (arg_name.to_string(), Type::Integer))
                    .collect();
                let _ = self.new_func_from_ast(name.to_string(), args, body)?;
                Ok(())
            }
        }
    }

    fn gen_cond(
        &mut self,
        cond_: &Expr,
        then_bb: usize,
        else_bb: usize,
        local_map: &mut HashMap<String, usize>,
    ) -> Result<()> {
        if let Expr::Cmp(kind, box (lhs, _), box (rhs, _)) = cond_ {
            let lhs = self.gen_expr(local_map, lhs)?;

            let rhs = self.gen_expr(local_map, rhs)?;
            self.insts.push(Hir::CmpBr(
                *kind,
                lhs,
                HirOperand::Reg(rhs),
                then_bb,
                else_bb,
            ));
        } else {
            let cond_ = self.gen_expr(local_map, cond_)?;
            self.insts.push(Hir::CondBr(cond_, then_bb, else_bb));
        }
        Ok(())
    }

    fn gen_while(
        &mut self,
        cond: &Expr,
        body: &[(Expr, Span)],
        local_map: &mut HashMap<String, usize>,
    ) -> Result<SsaReg> {
        let cond_bb = self.new_bb();
        self.insts.push(Hir::Br(cond_bb));
        self.cur_bb = cond_bb;
        let body_bb = self.new_bb();
        let succ_bb = self.new_bb();
        self.gen_cond(cond, body_bb, succ_bb, local_map)?;
        self.cur_bb = body_bb;
        self.gen_exprs_nouse(local_map, body)?;
        self.insts.push(Hir::Br(cond_bb));
        self.cur_bb = succ_bb;
        let ret = self.new_integer(0);
        Ok(ret)
    }
}
