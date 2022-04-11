use super::*;

type Result<T> = std::result::Result<T, BcErr>;

#[derive(Debug, Clone)]
pub enum BcErr {
    UndefinedLocal(String),
    UndefinedMethod(String),
}

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
struct BcFuncId(usize);

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
    temp: u16,
    /// The number of registers.
    reg_num: u16,
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
                self.temp -= args.len() as u16;
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
            Expr::Integer(i) => Ok(()),
            Expr::Float(f) => Ok(()),
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
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
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
                self.temp -= args.len() as u16;
                let func = *self.func_map.get(name).unwrap();
                self.insts.push(Inst::Call(func, None, arg, args.len()));
                Ok(())
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_pos = self.new_label();
                let succ_pos = self.new_label();
                self.gen_expr(cond_);
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
            /*Expr::While(box (cond, _), body) => self.gen_while(cond, body),*/
            _ => unreachable!(),
        }
    }
}

///
/// ID of instruction.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(usize);

///
/// ID of register.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Reg(u16);

///
/// ID of local variable.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Local(usize);

#[derive(Debug, Clone, PartialEq)]
enum Inst {
    Br(usize),
    CondBr(Reg, usize),
    Integer(Reg, i32),
    Float(Reg, f64),
    Nil(Reg),
    Neg(Reg, Reg),               // ret, src
    Add(Reg, Reg, Reg),          // ret, lhs, rhs
    Sub(Reg, Reg, Reg),          // ret, lhs, rhs
    Mul(Reg, Reg, Reg),          // ret, lhs, rhs
    Div(Reg, Reg, Reg),          // ret, lhs, rhs
    Cmp(CmpKind, Reg, Reg, Reg), // kind, lhs, rhs
    Ret(Reg),
    LocalStore(Option<Reg>, Local, Reg),     // offset, src
    LocalLoad(Local, Reg),                   // offset, dst
    Call(BcFuncId, Option<Reg>, Reg, usize), // (id, ret, args, args_len)
}

pub struct Interp {
    cur_fn: BcFuncId,
    pc: usize,
    call_stack: Stack,
}

impl std::ops::Index<Reg> for Interp {
    type Output = Value;
    fn index(&self, index: Reg) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<Reg> for Interp {
    fn index_mut(&mut self, index: Reg) -> &mut Value {
        &mut self.call_stack[index]
    }
}

impl std::ops::Index<Local> for Interp {
    type Output = Value;
    fn index(&self, index: Local) -> &Value {
        &self.call_stack[index]
    }
}

impl std::ops::IndexMut<Local> for Interp {
    fn index_mut(&mut self, index: Local) -> &mut Value {
        &mut self.call_stack[index]
    }
}

#[derive(Debug, Clone)]
struct Stack {
    stack: Vec<Value>,
    bp: usize,
    reg_base: usize,
    args_len: usize,
}

impl std::ops::Index<Reg> for Stack {
    type Output = Value;
    fn index(&self, index: Reg) -> &Value {
        &self.stack[self.reg_base + index.0 as usize]
    }
}

impl std::ops::IndexMut<Reg> for Stack {
    fn index_mut(&mut self, index: Reg) -> &mut Value {
        &mut self.stack[self.reg_base + index.0 as usize]
    }
}

impl std::ops::Index<Local> for Stack {
    type Output = Value;
    fn index(&self, index: Local) -> &Value {
        &self.stack[self.bp + index.0]
    }
}

impl std::ops::IndexMut<Local> for Stack {
    fn index_mut(&mut self, index: Local) -> &mut Value {
        &mut self.stack[self.bp + index.0]
    }
}

impl Stack {
    fn new() -> Self {
        Self {
            stack: vec![],
            bp: 0,
            reg_base: 0,
            args_len: 0,
        }
    }

    fn args(&self) -> &[Value] {
        &self.stack[self.bp..self.bp + self.args_len]
    }

    fn reg_slice(&self, reg: Reg, len: usize) -> std::ops::Range<usize> {
        let start = self.reg_base + reg.0 as usize;
        start..start + len
    }

    fn push_frame(
        &mut self,
        args: Reg,
        args_len: usize,
        bc_func: &BcFunc,
        cur_fn: BcFuncId,
        pc: usize,
    ) {
        let args = self.reg_slice(args, args_len);
        let local_num = bc_func.locals.len();
        let reg_num = bc_func.reg_num;
        self.stack.push(Value::from_unchecked(cur_fn.0 as u64));
        self.stack.push(Value::from_unchecked(pc as u64));
        self.stack.push(Value::from_unchecked(self.args_len as u64));
        self.stack.push(Value::from_unchecked(self.reg_base as u64));
        self.stack.push(Value::from_unchecked(self.bp as u64));
        self.bp = self.stack.len();
        self.reg_base = self.bp + local_num;
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num as usize;
        self.stack.extend_from_within(args);
        self.stack.resize(new_len, Value::nil());
    }

    fn pop_frame(&mut self) -> (bool, BcFuncId, usize) {
        let old_bp = self.bp;
        let cur_fn = self.stack[old_bp - 5].get() as usize;
        let pc = self.stack[old_bp - 4].get() as usize;
        self.args_len = self.stack[old_bp - 3].get() as usize;
        self.reg_base = self.stack[old_bp - 2].get() as usize;
        self.bp = self.stack[old_bp - 1].get() as usize;
        self.stack.truncate(old_bp - 5);
        (self.bp == 0, BcFuncId(cur_fn), pc)
    }
}

impl Interp {
    fn new() -> Self {
        Self {
            cur_fn: BcFuncId(0),
            pc: 0,
            call_stack: Stack::new(),
        }
    }

    fn push_frame(&mut self, args: Reg, len: usize, bc_func: &BcFunc) {
        self.call_stack
            .push_frame(args, len, bc_func, self.cur_fn, self.pc);
    }

    fn pop_frame(&mut self) -> bool {
        let (b, func, pc) = self.call_stack.pop_frame();
        self.cur_fn = func;
        self.pc = pc;
        b
    }

    pub fn eval_toplevel(bc_context: &BcGen) -> Value {
        let mut eval = Self::new();
        let hir_func = &bc_context[BcFuncId(0)];
        eval.push_frame(Reg(0), 0, hir_func);
        eval.eval_function(bc_context)
    }

    fn eval_function(&mut self, bc_context: &BcGen) -> Value {
        self.pc = 0;
        loop {
            let inst = &bc_context[self.cur_fn].insts[self.pc];
            //eprintln!("{:?}", &inst);
            self.pc += 1;
            if let Some(val) = self.eval(bc_context, inst) {
                let _ = self.pop_frame();
                return val;
            }
        }
    }
}

macro_rules! value_op {
    ($lhs:ident, $rhs:ident, $op:ident) => {{
        match ($lhs.unpack(), $rhs.unpack()) {
            (RV::Integer($lhs), RV::Integer($rhs)) => $lhs.$op(&$rhs),
            (RV::Integer($lhs), RV::Float($rhs)) => ($lhs as f64).$op(&$rhs),
            (RV::Float($lhs), RV::Integer($rhs)) => $lhs.$op(&($rhs as f64)),
            (RV::Float($lhs), RV::Float($rhs)) => $lhs.$op(&$rhs),
            _ => unreachable!(),
        }
    }};
}

impl Interp {
    fn eval(&mut self, bc_context: &BcGen, inst: &Inst) -> Option<Value> {
        match inst {
            Inst::Integer(ret, i) => {
                self[*ret] = Value::integer(*i);
            }
            Inst::Float(ret, f) => {
                self[*ret] = Value::float(*f);
            }
            Inst::Nil(ret) => {
                self[*ret] = Value::nil();
            }
            Inst::Neg(dst, src) => {
                self[*dst] = match self[*src].unpack() {
                    RV::Integer(i) => Value::integer(-i),
                    RV::Float(f) => Value::float(-f),
                    _ => unreachable!(),
                };
            }
            Inst::Add(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs + rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 + rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs + rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs + rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Sub(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs - rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 - rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs - rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs - rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Mul(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs * rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 * rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs * rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs * rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Div(ret, lhs, rhs) => {
                self[*ret] = match (self[*lhs].unpack(), self[*rhs].unpack()) {
                    (RV::Integer(lhs), RV::Integer(rhs)) => Value::integer(lhs / rhs),
                    (RV::Integer(lhs), RV::Float(rhs)) => Value::float(lhs as f64 / rhs),
                    (RV::Float(lhs), RV::Integer(rhs)) => Value::float(lhs / rhs as f64),
                    (RV::Float(lhs), RV::Float(rhs)) => Value::float(lhs / rhs),
                    _ => unreachable!(),
                };
            }
            Inst::Cmp(kind, ret, lhs, rhs) => {
                let lhs = self[*lhs];
                let rhs = self[*rhs];
                self[*ret] = Value::bool(match kind {
                    CmpKind::Eq => value_op!(lhs, rhs, eq),
                    CmpKind::Ne => value_op!(lhs, rhs, ne),
                    CmpKind::Lt => value_op!(lhs, rhs, lt),
                    CmpKind::Gt => value_op!(lhs, rhs, gt),
                    CmpKind::Le => value_op!(lhs, rhs, le),
                    CmpKind::Ge => value_op!(lhs, rhs, ge),
                });
            }
            Inst::Ret(lhs) => return Some(self[*lhs]),
            Inst::LocalStore(ret, local, src) => {
                let v = self[*src];
                self[*local] = v;
                if let Some(ret) = ret {
                    self[*ret] = v;
                }
            }
            Inst::LocalLoad(local, dst) => {
                self[*dst] = self[*local];
            }
            Inst::Call(id, ret, args, len) => {
                let bc_func = &bc_context[*id];
                self.push_frame(*args, *len, bc_func);
                self.cur_fn = *id;
                let res = self.eval_function(bc_context);
                if let Some(ret) = *ret {
                    self[ret] = res;
                }
            }
            Inst::Br(next_pc) => {
                self.pc = bc_context[self.cur_fn].labels[*next_pc].unwrap().0;
            }
            Inst::CondBr(cond_, then_) => {
                if self[*cond_] != Value::bool(false) {
                    self.pc = bc_context[self.cur_fn].labels[*then_].unwrap().0;
                };
            }
        }
        None
    }
}
