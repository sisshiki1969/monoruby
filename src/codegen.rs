use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

// System V ABI
// registers for args: rdi(R7), rsi(R6), rdx(R2), rcx(R1)
const ABI_GENERAL_REGS: [u64; 4] = [7, 6, 2, 1];

///
/// Physical registers for general purpose.
///
enum GeneralPhysReg {
    /// General purpose register (r8-r11)
    Reg(u64),
    /// On stack with offset from rbp.
    Stack(i64),
}

///
/// Physical registers for double-precision floating point numbers.
///
enum FloatPhysReg {
    /// Xmm registers (xmm1-xmm15)
    Xmm(u64),
    /// On stack with offset from rbp.
    Stack(i64),
}

///
/// Code generator
///
/// This generates x86-64 machine code from McIR into heap memory .
///
pub struct Codegen {
    jit: JitMemory,
    g_offset: usize,
    f_offset: usize,
    block_labels: Vec<Vec<DestLabel>>,
    func_labels: Vec<DestLabel>,
    no_prologue: bool,
    cur_fn: usize,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            g_offset: 0,
            f_offset: 0,
            block_labels: vec![],
            func_labels: vec![],
            no_prologue: false,
            cur_fn: 0,
        }
    }

    ///
    /// Allocate general register to physical register.
    ///
    /// Currently, first 4 registers are allocated to R8-R11 registers.
    ///
    fn g_phys_reg(&self, reg: GReg) -> GeneralPhysReg {
        let reg = reg.to_usize();
        if reg < 5 {
            GeneralPhysReg::Reg(reg as u64 + 8)
        } else {
            GeneralPhysReg::Stack(((reg - 4 + self.g_offset) * 8) as i64 + 8)
        }
    }

    ///
    /// Allocate general register to physical register.
    ///
    /// Currently, first 14 registers are allocated to xmm7-xmm15 registers.
    ///
    fn f_phys_reg(&self, reg: FReg) -> FloatPhysReg {
        let reg = reg.to_usize();
        if reg < 9 {
            FloatPhysReg::Xmm((reg + 7) as u64)
        } else {
            FloatPhysReg::Stack(((reg - 8 + self.f_offset) * 8) as i64 + 8)
        }
    }

    fn emit_jcc(&mut self, kind: CmpKind, br: DestLabel) {
        match kind {
            CmpKind::Eq => {
                monoasm!(self.jit, jeq br;);
            }
            CmpKind::Ne => {
                monoasm!(self.jit, jne br;);
            }
            CmpKind::Gt => {
                monoasm!(self.jit, jgt br;);
            }
            CmpKind::Ge => {
                monoasm!(self.jit, jge br;);
            }
            CmpKind::Lt => {
                monoasm!(self.jit, jlt br;);
            }
            CmpKind::Le => {
                monoasm!(self.jit, jle br;);
            }
        };
    }

    fn emit_jump_next(&mut self, dest: usize, next_bb: Option<usize>) {
        if next_bb != Some(dest) {
            let label = self.block_labels[self.cur_fn][dest];
            monoasm!(self.jit,
              jmp label;
            );
        }
    }

    fn emit_fjcc(&mut self, kind: CmpKind, br: DestLabel) {
        match kind {
            CmpKind::Eq => {
                monoasm!(self.jit, jeq br;);
            }
            CmpKind::Ne => {
                monoasm!(self.jit, jne br;);
            }
            CmpKind::Gt => {
                monoasm!(self.jit, ja br;);
            }
            CmpKind::Ge => {
                monoasm!(self.jit, jae br;);
            }
            CmpKind::Lt => {
                monoasm!(self.jit, jb br;);
            }
            CmpKind::Le => {
                monoasm!(self.jit, jbe br;);
            }
        }
    }

    fn emit_setcc(&mut self, kind: CmpKind, dest: &GeneralPhysReg) {
        match dest {
            GeneralPhysReg::Reg(dest) => match kind {
                CmpKind::Eq => {
                    monoasm!( self.jit, seteq R(*dest); );
                }
                CmpKind::Ne => {
                    monoasm!( self.jit, setne R(*dest); );
                }
                CmpKind::Gt => {
                    monoasm!( self.jit, setgt R(*dest); );
                }
                CmpKind::Ge => {
                    monoasm!( self.jit, setge R(*dest); );
                }
                CmpKind::Lt => {
                    monoasm!( self.jit, setlt R(*dest); );
                }
                CmpKind::Le => {
                    monoasm!( self.jit, setle R(*dest); );
                }
            },
            GeneralPhysReg::Stack(dest) => match kind {
                CmpKind::Eq => {
                    monoasm!( self.jit, seteq [rbp-(*dest)]; );
                }
                CmpKind::Ne => {
                    monoasm!( self.jit, setne [rbp-(*dest)]; );
                }
                CmpKind::Gt => {
                    monoasm!( self.jit, setgt [rbp-(*dest)]; );
                }
                CmpKind::Ge => {
                    monoasm!( self.jit, setge [rbp-(*dest)]; );
                }
                CmpKind::Lt => {
                    monoasm!( self.jit, setlt [rbp-(*dest)]; );
                }
                CmpKind::Le => {
                    monoasm!( self.jit, setle [rbp-(*dest)]; );
                }
            },
        };
    }

    fn emit_fsetcc(&mut self, kind: CmpKind, dest: &GeneralPhysReg) {
        match dest {
            GeneralPhysReg::Reg(dest) => match kind {
                CmpKind::Eq => {
                    monoasm!( self.jit, seteq R(*dest); );
                }
                CmpKind::Ne => {
                    monoasm!( self.jit, setne R(*dest); );
                }
                CmpKind::Gt => {
                    monoasm!( self.jit, seta R(*dest); );
                }
                CmpKind::Ge => {
                    monoasm!( self.jit, setae R(*dest); );
                }
                CmpKind::Lt => {
                    monoasm!( self.jit, setb R(*dest); );
                }
                CmpKind::Le => {
                    monoasm!( self.jit, setbe R(*dest); );
                }
            },
            GeneralPhysReg::Stack(dest) => match kind {
                CmpKind::Eq => {
                    monoasm!( self.jit, seteq [rbp-(*dest)]; );
                }
                CmpKind::Ne => {
                    monoasm!( self.jit, setne [rbp-(*dest)]; );
                }
                CmpKind::Gt => {
                    monoasm!( self.jit, seta [rbp-(*dest)]; );
                }
                CmpKind::Ge => {
                    monoasm!( self.jit, setae [rbp-(*dest)]; );
                }
                CmpKind::Lt => {
                    monoasm!( self.jit, setb [rbp-(*dest)]; );
                }
                CmpKind::Le => {
                    monoasm!( self.jit, setbe [rbp-(*dest)]; );
                }
            },
        };
    }

    fn emit_call(&mut self, g_using: &[GReg], f_using: &[FReg], dest: DestLabel) {
        for greg in g_using {
            match self.g_phys_reg(*greg) {
                GeneralPhysReg::Reg(reg) => {
                    monoasm!(self.jit, pushq R(reg); );
                }
                _ => {}
            }
        }
        for freg in f_using {
            match self.f_phys_reg(*freg) {
                FloatPhysReg::Xmm(reg) => {
                    monoasm!(self.jit,
                        movq rax, xmm(reg);
                        pushq rax;
                    );
                }
                _ => {}
            }
        }
        monoasm!(self.jit, call dest; );
        for freg in f_using.iter().rev() {
            match self.f_phys_reg(*freg) {
                FloatPhysReg::Xmm(reg) => {
                    monoasm!(self.jit,
                        popq rax;
                        movq xmm(reg), rax;
                    );
                }
                _ => {}
            }
        }
        for greg in g_using.iter().rev() {
            match self.g_phys_reg(*greg) {
                GeneralPhysReg::Reg(reg) => {
                    monoasm!(self.jit, popq R(reg); );
                }
                _ => {}
            }
        }
    }

    fn emit_store_args(&mut self, args: &[McOperand]) {
        let mut arg_greg = 0; // Must be n<=3
                              //let mut arg_freg = 0; // Must be 1<=n<=6 (xmm1-xmm6)
        for arg in args {
            match arg {
                McOperand::General(op) => {
                    assert!(arg_greg <= 3);
                    let dst = ABI_GENERAL_REGS[arg_greg];
                    match op {
                        McGeneralOperand::Integer(i) => {
                            monoasm!(self.jit, movq R(dst), (*i); );
                        }
                        McGeneralOperand::Reg(reg) => match self.g_phys_reg(*reg) {
                            GeneralPhysReg::Reg(reg) => {
                                monoasm!(self.jit, movq R(dst), R(reg); );
                            }
                            GeneralPhysReg::Stack(ofs) => {
                                monoasm!(self.jit, movq R(dst), [rbp-(ofs)]; );
                            }
                        },
                    }
                    arg_greg += 1;
                }
                McOperand::Float(op) => {
                    assert!(arg_greg <= 3);
                    let dst = ABI_GENERAL_REGS[arg_greg];
                    match op {
                        McFloatOperand::Float(f) => {
                            let i = f64::to_bits(*f);
                            monoasm!(self.jit,
                                movq R(dst), (i);
                            );
                        }
                        McFloatOperand::Reg(reg) => match self.f_phys_reg(*reg) {
                            FloatPhysReg::Xmm(reg) => {
                                monoasm!(self.jit, movq R(dst), xmm(reg); );
                            }
                            FloatPhysReg::Stack(ofs) => {
                                monoasm!(self.jit, movq R(dst), [rbp-(ofs)]; );
                            }
                        },
                    }
                    arg_greg += 1;
                }
            }
        }
    }

    fn prologue(&mut self, locals: usize) {
        if locals != 0 {
            monoasm!(self.jit,
                pushq rbp;
                movq rbp, rsp;
                subq rsp, ((locals + locals % 2) * 8);
            );
            self.no_prologue = false;
        } else {
            self.no_prologue = true;
        }
    }

    fn epilogue(&mut self) {
        if !self.no_prologue {
            monoasm!(self.jit,
                movq rsp, rbp;
                popq rbp;
            );
        }
        monoasm!( self.jit, ret; );
    }
}

macro_rules! integer_ops {
    ($self: ident, $op: ident, $lhs:ident, $rhs:ident) => {{
        let lhs = $self.g_phys_reg(*$lhs);
        match &$rhs {
            McGeneralOperand::Reg(rhs) => {
                let rhs = $self.g_phys_reg(*rhs);
                match (lhs, rhs) {
                    (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                        monoasm!($self.jit, $op  R(lhs), R(rhs););
                    }
                    (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                        monoasm!($self.jit, $op  R(lhs), [rbp-(rhs)];);
                    }
                    (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                        monoasm!($self.jit, $op  [rbp-(lhs)], R(rhs););
                    }
                    (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Stack(rhs)) => {
                        monoasm!($self.jit,
                          movq  rax, [rbp-(rhs)];
                          $op  [rbp-(lhs)], rax;
                        );
                    }
                };
            }
            McGeneralOperand::Integer(rhs) => {
                let lhs = $self.g_phys_reg(*$lhs);
                match lhs {
                    GeneralPhysReg::Reg(lhs) => {
                        monoasm!($self.jit, $op  R(lhs), (*rhs as i64););
                    }
                    GeneralPhysReg::Stack(lhs) => {
                        monoasm!($self.jit, $op  [rbp-(lhs)], (*rhs as i64););
                    }
                };
            }
        }
    }};
}

macro_rules! float_ops {
    ($self: ident, $op: ident, $lhs:ident, $rhs:ident) => {{
        let lhs = $self.f_phys_reg(*$lhs);
        match &$rhs {
            McFloatOperand::Reg(rhs) => {
                let rhs = $self.f_phys_reg(*rhs);
                match (lhs, rhs) {
                    (FloatPhysReg::Xmm(lhs), FloatPhysReg::Xmm(rhs)) => {
                        monoasm!($self.jit,
                          $op    xmm(lhs), xmm(rhs);
                        );
                    }
                    (FloatPhysReg::Xmm(lhs), FloatPhysReg::Stack(rhs)) => {
                        monoasm!($self.jit,
                          movsd  xmm0, [rbp-(rhs)];
                          $op    xmm(lhs), xmm0;
                        );
                    }
                    (FloatPhysReg::Stack(lhs), FloatPhysReg::Xmm(rhs)) => {
                        monoasm!($self.jit,
                          movsd  xmm0, [rbp-(lhs)];
                          $op    xmm0, xmm(rhs);
                          movsd  [rbp-(lhs)], xmm0;
                        );
                    }
                    (FloatPhysReg::Stack(lhs), FloatPhysReg::Stack(rhs)) => {
                        monoasm!($self.jit,
                          movsd  xmm0, [rbp-(lhs)];
                          $op    xmm0, [rbp-(rhs)];
                          movsd  [rbp-(lhs)], xmm0;
                        );
                    }
                }
            }
            McFloatOperand::Float(rhs) => {
                let lhs = $self.f_phys_reg(*$lhs);
                match lhs {
                    FloatPhysReg::Xmm(lhs) => {
                        let label = $self.jit.const_f64(*rhs);
                        monoasm!($self.jit,
                            movq   xmm0, [rip + label];
                            $op    xmm(lhs), xmm0;
                        );
                    }
                    FloatPhysReg::Stack(lhs) => {
                        let label = $self.jit.const_f64(*rhs);
                        monoasm!($self.jit,
                          movsd  xmm0, [rbp-(lhs)];
                          $op    xmm0, [rip + label];
                          movsd  [rbp-(lhs)], xmm0;
                        );
                    }
                }
            }
        };
    }}
}

impl Codegen {
    pub fn call_jit_func(&mut self, func_label: DestLabel, ret_ty: Type, args: &[Value]) -> Value {
        use std::mem::transmute;
        let func = self.jit.get_label_u64(func_label);
        let res = match args.len() {
            0 => unsafe { transmute::<u64, extern "C" fn() -> u64>(func)() },
            1 => unsafe { transmute::<u64, extern "C" fn(u64) -> u64>(func)(args[0].pack()) },
            2 => unsafe {
                transmute::<u64, extern "C" fn(u64, u64) -> u64>(func)(
                    args[0].pack(),
                    args[1].pack(),
                )
            },
            _ => unimplemented!(),
        };
        let res = match ret_ty {
            Type::Integer => Value::integer_fromu64(res),
            Type::Float => Value::float_fromu64(res),
            Type::Bool => Value::bool_fromu64(res),
            Type::Nil => Value::nil(),
        };

        res
    }

    pub fn compile_func(
        &mut self,
        mcir_func: &McIrFunc,
        cur_fn: usize,
    ) -> (usize, DestLabel, Type) {
        let ret_ty = mcir_func.ret_ty;
        let g_spill = match mcir_func.g_regs {
            i if i < 5 => 0,
            i => i - 4,
        };
        let f_spill = match mcir_func.f_regs {
            i if i < 15 => 0,
            i => i - 14,
        };
        let locals_num = mcir_func.locals.len();
        self.g_offset = locals_num;
        self.f_offset = locals_num + g_spill;
        let func_label = self.jit.label();
        assert_eq!(cur_fn, self.func_labels.len());
        self.cur_fn = cur_fn;
        self.func_labels.push(func_label);
        let mut block_labels = vec![];
        for _ in &mcir_func.blocks {
            let label = self.jit.label();
            block_labels.push(label);
        }
        self.block_labels.push(block_labels);
        self.jit.bind_label(func_label);
        self.prologue(locals_num + g_spill + f_spill);
        let mut greg = 0;
        //let mut freg = 0;
        for (i, _) in mcir_func.args.iter().enumerate() {
            let offset = 8 + 8 * i as i64;
            //match ty {
            //Type::Integer | Type::Bool | Type::Nil => {
            let src = ABI_GENERAL_REGS[greg];
            monoasm!(self.jit, movq  [rbp - (offset)], R(src); );
            greg += 1;
            //}
            //Type::Float => {
            //    monoasm!(self.jit, movq  [rbp - (offset)], xmm(freg); );
            //    freg += 1;
            //}
            //}
        }

        for bbi in 0..mcir_func.blocks.len() - 1 {
            self.compile_bb(mcir_func, bbi, ret_ty, Some(bbi + 1));
        }
        self.compile_bb(mcir_func, mcir_func.blocks.len() - 1, ret_ty, None);
        self.jit.finalize();
        (cur_fn, func_label, ret_ty)
    }

    fn compile_bb(
        &mut self,
        mcir_func: &McIrFunc,
        bbi: usize,
        ret_ty: Type,
        next_bb: Option<usize>,
    ) {
        let bb = &mcir_func.blocks[bbi];
        let label = self.block_labels[self.cur_fn][bbi];
        self.jit.bind_label(label);
        for op in &bb.insts {
            match op {
                McIR::Integer(reg, i) => match self.g_phys_reg(*reg) {
                    GeneralPhysReg::Reg(reg) => {
                        monoasm!(self.jit, movq  R(reg), (*i););
                    }
                    GeneralPhysReg::Stack(ofs) => {
                        monoasm!(self.jit, movq  [rbp-(ofs)], (*i););
                    }
                },
                McIR::Nil(_) => {}
                McIR::Float(reg, f) => {
                    let label = self.jit.const_f64(*f);
                    let f = u64::from_ne_bytes(f.to_ne_bytes()) as i64;
                    match self.f_phys_reg(*reg) {
                        FloatPhysReg::Xmm(reg) => {
                            monoasm!(self.jit,
                              movq   xmm(reg), [rip + label];
                            );
                        }
                        FloatPhysReg::Stack(ofs) => {
                            monoasm!(self.jit,
                              movq  [rbp-(ofs)], (f);
                            );
                        }
                    }
                }
                McIR::IAdd(lhs, rhs) => integer_ops!(self, addq, lhs, rhs),
                McIR::ISub(lhs, rhs) => integer_ops!(self, subq, lhs, rhs),
                McIR::IMul(lhs, rhs) => {
                    fn emit_rhs(mut jit: &mut JitMemory, rhs: GeneralPhysReg) {
                        match rhs {
                            GeneralPhysReg::Reg(rhs) => {
                                monoasm!(jit, imul  rax, R(rhs); );
                            }
                            GeneralPhysReg::Stack(rhs) => {
                                monoasm!(jit, imul  rax, [rbp-(rhs)]; );
                            }
                        }
                    }
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match lhs {
                        GeneralPhysReg::Reg(lhs) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                            );
                            emit_rhs(&mut self.jit, rhs);
                            monoasm!(self.jit,
                              movq  R(lhs), rax;
                            );
                        }
                        GeneralPhysReg::Stack(lhs) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                            );
                            emit_rhs(&mut self.jit, rhs);
                            monoasm!(self.jit,
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
                McIR::IDiv(lhs, rhs) => {
                    fn emit_rhs(mut jit: &mut JitMemory, rhs: GeneralPhysReg) {
                        match rhs {
                            GeneralPhysReg::Reg(rhs) => {
                                monoasm!(jit, idiv  R(rhs););
                            }
                            GeneralPhysReg::Stack(rhs) => {
                                monoasm!(jit, idiv  [rbp-(rhs)];);
                            }
                        }
                    }
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match lhs {
                        GeneralPhysReg::Reg(lhs) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                              cqo;
                            );
                            emit_rhs(&mut self.jit, rhs);
                            monoasm!(self.jit,
                              movq  R(lhs), rax;
                            );
                        }
                        GeneralPhysReg::Stack(lhs) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                              cqo;
                            );
                            emit_rhs(&mut self.jit, rhs);
                            monoasm!(self.jit,
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
                McIR::ICmp(kind, lhs, rhs) => {
                    let lhs = self.g_phys_reg(*lhs);
                    match rhs {
                        McGeneralOperand::Reg(rhs) => {
                            let rhs = self.g_phys_reg(*rhs);
                            match (&lhs, &rhs) {
                                (GeneralPhysReg::Reg(lhs_), rhs) => match rhs {
                                    GeneralPhysReg::Reg(rhs) => {
                                        monoasm!(self.jit, cmpq  R(*lhs_), R(*rhs););
                                    }
                                    GeneralPhysReg::Stack(rhs) => {
                                        monoasm!(self.jit, cmpq  R(*lhs_), [rbp-(rhs)];);
                                    }
                                },
                                (GeneralPhysReg::Stack(lhs_), rhs) => match rhs {
                                    GeneralPhysReg::Reg(rhs) => {
                                        monoasm!(self.jit, cmpq  [rbp-(lhs_)], R(*rhs););
                                    }
                                    GeneralPhysReg::Stack(rhs) => {
                                        monoasm!(self.jit,
                                            movq  rax, [rbp-(rhs)];
                                            cmpq  [rbp-(lhs_)], rax;
                                        );
                                    }
                                },
                            };
                        }
                        McGeneralOperand::Integer(rhs) => {
                            match &lhs {
                                GeneralPhysReg::Reg(lhs_) => {
                                    monoasm!(self.jit, cmpq  R(*lhs_), (*rhs););
                                }
                                GeneralPhysReg::Stack(lhs_) => {
                                    monoasm!(self.jit, cmpq  [rbp-(lhs_)], (*rhs););
                                }
                            };
                        }
                    }
                    self.emit_setcc(*kind, &lhs);
                }
                McIR::FCmp(kind, ret, lhs, rhs) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    let ret = self.g_phys_reg(*ret);
                    match &lhs {
                        FloatPhysReg::Xmm(lhs) => match rhs {
                            FloatPhysReg::Xmm(rhs) => {
                                monoasm!(self.jit, ucomisd  xmm(*lhs), xmm(rhs););
                            }
                            FloatPhysReg::Stack(rhs) => {
                                monoasm!(self.jit, ucomisd  xmm(*lhs), [rbp-(rhs)];);
                            }
                        },
                        FloatPhysReg::Stack(lhs) => match rhs {
                            FloatPhysReg::Xmm(rhs) => {
                                monoasm!(self.jit,
                                    movq  xmm0, [rbp-(*lhs)];
                                    ucomisd  xmm0, xmm(rhs);
                                );
                            }
                            FloatPhysReg::Stack(rhs) => {
                                monoasm!(self.jit,
                                    movq  xmm0, [rbp-(*lhs)];
                                    ucomisd  xmm0, [rbp-(rhs)];
                                );
                            }
                        },
                    };
                    self.emit_fsetcc(*kind, &ret);
                }

                McIR::FAdd(lhs, rhs) => float_ops!(self, addsd, lhs, rhs),
                McIR::FSub(lhs, rhs) => float_ops!(self, subsd, lhs, rhs),
                McIR::FMul(lhs, rhs) => float_ops!(self, mulsd, lhs, rhs),
                McIR::FDiv(lhs, rhs) => float_ops!(self, divsd, lhs, rhs),

                McIR::CastIntFloat(dst, src) => match src {
                    &McGeneralOperand::Reg(src) => {
                        let src = self.g_phys_reg(src);
                        let dst = self.f_phys_reg(*dst);
                        match (src, dst) {
                            (GeneralPhysReg::Reg(src), FloatPhysReg::Xmm(dst)) => {
                                monoasm!(self.jit,
                                  cvtsi2sdq xmm(dst), R(src);
                                );
                            }
                            (GeneralPhysReg::Reg(src), FloatPhysReg::Stack(dst)) => {
                                monoasm!(self.jit,
                                  cvtsi2sdq xmm0, R(src);
                                  movsd  [rbp-(dst)], xmm0;
                                );
                            }
                            (GeneralPhysReg::Stack(src), FloatPhysReg::Xmm(dst)) => {
                                monoasm!(self.jit,
                                  cvtsi2sdq xmm(dst), [rbp-(src)];
                                );
                            }
                            (GeneralPhysReg::Stack(src), FloatPhysReg::Stack(dst)) => {
                                monoasm!(self.jit,
                                  cvtsi2sdq xmm0, [rbp-(src)];
                                  movsd  [rbp-(dst)], xmm0;
                                );
                            }
                        }
                    }
                    &McGeneralOperand::Integer(n) => {
                        let dst = self.f_phys_reg(*dst);
                        match dst {
                            FloatPhysReg::Xmm(dst) => {
                                monoasm!(self.jit,
                                  movq      rax, (n as i64);
                                  cvtsi2sdq xmm(dst), rax;
                                );
                            }
                            FloatPhysReg::Stack(dst) => {
                                monoasm!(self.jit,
                                  movq      rax, (n as i64);
                                  cvtsi2sdq xmm0, rax;
                                  movsd     [rbp-(dst)], xmm0;
                                );
                            }
                        }
                    }
                },
                McIR::FRet(lhs) => {
                    match ret_ty {
                        Type::Float => {}
                        _ => panic!("Return type mismatch {:?} {:?}.", ret_ty, Type::Float),
                    }
                    match lhs {
                        McFloatOperand::Float(f) => {
                            let n = i64::from_ne_bytes(f.to_le_bytes());
                            monoasm!(self.jit,
                              movq rax, (n);
                            );
                        }
                        McFloatOperand::Reg(lhs) => match self.f_phys_reg(*lhs) {
                            FloatPhysReg::Xmm(lhs) => {
                                monoasm!(self.jit,
                                  movq rax, xmm(lhs);
                                );
                            }
                            FloatPhysReg::Stack(ofs) => {
                                monoasm!(self.jit,
                                  movq rax, [rbp-(ofs)];
                                );
                            }
                        },
                    }
                    self.epilogue();
                }
                McIR::IRet(lhs, ty) => {
                    if ret_ty != *ty {
                        panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                    }
                    if ty != &Type::Nil {
                        match lhs {
                            McGeneralOperand::Integer(i) => {
                                monoasm!(self.jit,
                                  movq rax, (*i as i64);
                                );
                            }
                            McGeneralOperand::Reg(lhs) => {
                                match self.g_phys_reg(*lhs) {
                                    GeneralPhysReg::Reg(reg) => {
                                        monoasm!(self.jit,
                                          movq rax, R(reg);
                                        );
                                    }
                                    GeneralPhysReg::Stack(lhs) => {
                                        monoasm!(self.jit,
                                          movq rax, [rbp-(lhs)];
                                        );
                                    }
                                };
                            }
                        }
                    }
                    self.epilogue();
                }
                McIR::INeg(reg) => {
                    match self.g_phys_reg(*reg) {
                        GeneralPhysReg::Reg(reg) => {
                            monoasm!(self.jit, negq R(reg););
                        }
                        GeneralPhysReg::Stack(lhs) => {
                            monoasm!(self.jit, negq [rbp-(lhs)];);
                        }
                    };
                }
                McIR::FNeg(reg) => {
                    let n = i64::from_ne_bytes((0.0f64).to_le_bytes());
                    match self.f_phys_reg(*reg) {
                        FloatPhysReg::Xmm(reg) => {
                            monoasm!(self.jit,
                              movq  rax, (n);
                              movq  xmm0, rax;
                              subsd xmm0, xmm(reg);
                              movsd xmm(reg), xmm0;
                            );
                        }
                        FloatPhysReg::Stack(lhs) => {
                            monoasm!(self.jit,
                              movq  rax, (n);
                              movq  xmm0, rax;
                              subsd xmm0, [rbp-(lhs)];
                              movsd [rbp-(lhs)], xmm0;
                            );
                        }
                    };
                }
                McIR::LocalStore(ofs, ret, src) => {
                    let ofs = (ofs * 8 + 8) as i64;
                    match src {
                        McOperand::General(src) => match src {
                            McGeneralOperand::Reg(src) => {
                                match self.g_phys_reg(*src) {
                                    GeneralPhysReg::Reg(src) => {
                                        monoasm!(self.jit,
                                          movq [rbp-(ofs)], R(src);
                                        );
                                        if let Some(ret) = ret {
                                            match self.g_phys_reg(ret.as_g()) {
                                                GeneralPhysReg::Reg(dst) => {
                                                    if dst != src {
                                                        monoasm!(self.jit,
                                                          movq R(dst), R(src);
                                                        );
                                                    }
                                                }
                                                GeneralPhysReg::Stack(dst) => {
                                                    monoasm!(self.jit,
                                                      movq [rbp-(dst)], R(src);
                                                    );
                                                }
                                            }
                                        }
                                    }
                                    GeneralPhysReg::Stack(src) => {
                                        monoasm!(self.jit,
                                          movq rax, [rbp-(src)];
                                          movq [rbp-(ofs)], rax;
                                        );
                                        if let Some(ret) = ret {
                                            match self.g_phys_reg(ret.as_g()) {
                                                GeneralPhysReg::Reg(dst) => {
                                                    monoasm!(self.jit,
                                                      movq R(dst), rax;
                                                    );
                                                }
                                                GeneralPhysReg::Stack(dst) => {
                                                    monoasm!(self.jit,
                                                      movq [rbp-(dst)], rax;
                                                    );
                                                }
                                            }
                                        }
                                    }
                                };
                            }
                            McGeneralOperand::Integer(i) => {
                                monoasm!(self.jit,
                                  movq [rbp-(ofs)], (*i);
                                );
                                if let Some(ret) = ret {
                                    match self.g_phys_reg(ret.as_g()) {
                                        GeneralPhysReg::Reg(dst) => {
                                            monoasm!(self.jit,
                                              movq R(dst), (*i);
                                            );
                                        }
                                        GeneralPhysReg::Stack(dst) => {
                                            monoasm!(self.jit,
                                              movq [rbp-(dst)], (*i);
                                            );
                                        }
                                    }
                                }
                            }
                        },
                        McOperand::Float(src) => match src {
                            McFloatOperand::Reg(src) => {
                                match self.f_phys_reg(*src) {
                                    FloatPhysReg::Xmm(src) => {
                                        monoasm!(self.jit,
                                          movq [rbp-(ofs)], xmm(src);
                                        );
                                        if let Some(ret) = ret {
                                            match self.f_phys_reg(ret.as_f()) {
                                                FloatPhysReg::Xmm(dst) => {
                                                    if dst != src {
                                                        monoasm!(self.jit,
                                                          movq xmm(dst), xmm(src);
                                                        );
                                                    }
                                                }
                                                FloatPhysReg::Stack(dst) => {
                                                    monoasm!(self.jit,
                                                      movq [rbp-(dst)], xmm(src);
                                                    );
                                                }
                                            }
                                        }
                                    }
                                    FloatPhysReg::Stack(lhs) => {
                                        monoasm!(self.jit,
                                          movq rax, [rbp-(lhs)];
                                          movq [rbp-(ofs)], rax;
                                        );
                                        if let Some(ret) = ret {
                                            match self.f_phys_reg(ret.as_f()) {
                                                FloatPhysReg::Xmm(dst) => {
                                                    monoasm!(self.jit,
                                                      movq xmm(dst), rax;
                                                    );
                                                }
                                                FloatPhysReg::Stack(dst) => {
                                                    monoasm!(self.jit,
                                                      movq [rbp-(dst)], rax;
                                                    );
                                                }
                                            }
                                        }
                                    }
                                };
                            }
                            McFloatOperand::Float(f) => {
                                let i = i64::from_ne_bytes(f.to_ne_bytes());
                                monoasm!(self.jit,
                                  movq rax, (i);
                                  movq [rbp-(ofs)], rax;
                                );
                                if let Some(ret) = ret {
                                    match self.f_phys_reg(ret.as_f()) {
                                        FloatPhysReg::Xmm(dst) => {
                                            monoasm!(self.jit,
                                              movq xmm(dst), rax;
                                            );
                                        }
                                        FloatPhysReg::Stack(dst) => {
                                            monoasm!(self.jit,
                                              movq [rbp-(dst)], rax;
                                            );
                                        }
                                    }
                                }
                            }
                        },
                    };
                }
                McIR::LocalLoad(ofs, reg) => {
                    let ofs = (ofs * 8 + 8) as i64;
                    match reg {
                        McReg::GReg(reg) => {
                            match self.g_phys_reg(*reg) {
                                GeneralPhysReg::Reg(reg) => {
                                    monoasm!(self.jit,
                                      movq R(reg), [rbp-(ofs)];
                                    );
                                }
                                GeneralPhysReg::Stack(lhs) => {
                                    monoasm!(self.jit,
                                      movq rax, [rbp-(ofs)];
                                      movq [rbp-(lhs)], rax;
                                    );
                                }
                            };
                        }
                        McReg::FReg(reg) => {
                            match self.f_phys_reg(*reg) {
                                FloatPhysReg::Xmm(reg) => {
                                    monoasm!(self.jit,
                                      movq xmm(reg), [rbp-(ofs)];
                                    );
                                }
                                FloatPhysReg::Stack(lhs) => {
                                    monoasm!(self.jit,
                                      movq rax, [rbp-(ofs)];
                                      movq [rbp-(lhs)], rax;
                                    );
                                }
                            };
                        }
                    };
                }
                McIR::Call(func_id, ret, args, g_using, f_using) => {
                    let dest = self.func_labels[*func_id];
                    self.emit_store_args(args);
                    self.emit_call(g_using, f_using, dest);
                    if let Some(ret) = *ret {
                        match self.g_phys_reg(ret) {
                            GeneralPhysReg::Reg(ret) => {
                                monoasm!(self.jit, movq R(ret), rax; );
                            }
                            GeneralPhysReg::Stack(ofs) => {
                                monoasm!(self.jit, movq [rbp-(ofs)], rax; );
                            }
                        }
                    }
                }
                McIR::Jmp(dest) => {
                    self.emit_jump_next(*dest, next_bb);
                }
                McIR::CondJmp(cond_, then_, else_) => {
                    // cond_ must be Type::Bool.
                    match cond_ {
                        McReg::GReg(reg) => {
                            match self.g_phys_reg(*reg) {
                                GeneralPhysReg::Reg(reg) => {
                                    monoasm!(self.jit,
                                        cmpb R(reg), 0;
                                    );
                                }
                                GeneralPhysReg::Stack(lhs) => {
                                    monoasm!(self.jit,
                                        cmpb [rbp-(lhs)], 0;
                                    );
                                }
                            };
                        }
                        _ => unreachable!(),
                    };
                    let label = self.block_labels[self.cur_fn][*else_];
                    monoasm!(self.jit, jeq label; );
                    self.emit_jump_next(*then_, next_bb);
                }
                McIR::ICmpJmp(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.g_phys_reg(*lhs);
                    match (lhs, rhs) {
                        (GeneralPhysReg::Reg(lhs), rhs) => match rhs {
                            McGeneralOperand::Integer(rhs) => {
                                monoasm!(self.jit, cmpq  R(lhs), (*rhs););
                            }
                            McGeneralOperand::Reg(rhs) => {
                                let rhs = self.g_phys_reg(*rhs);
                                match rhs {
                                    GeneralPhysReg::Reg(rhs) => {
                                        monoasm!(self.jit, cmpq  R(lhs), R(rhs););
                                    }
                                    GeneralPhysReg::Stack(rhs) => {
                                        monoasm!(self.jit, cmpq  R(lhs), [rbp-(rhs)];);
                                    }
                                }
                            }
                        },
                        (GeneralPhysReg::Stack(lhs), rhs) => match rhs {
                            McGeneralOperand::Integer(rhs) => {
                                monoasm!(self.jit, cmpq  [rbp-(lhs)], (*rhs););
                            }
                            McGeneralOperand::Reg(rhs) => {
                                let rhs = self.g_phys_reg(*rhs);
                                match rhs {
                                    GeneralPhysReg::Reg(rhs) => {
                                        monoasm!(self.jit, cmpq  [rbp-(lhs)], R(rhs););
                                    }
                                    GeneralPhysReg::Stack(rhs) => {
                                        monoasm!(self.jit,
                                            movq  rax, [rbp-(rhs)];
                                            cmpq  [rbp-(lhs)], rax;
                                        );
                                    }
                                }
                            }
                        },
                    };
                    let label = self.block_labels[self.cur_fn][*then_bb];
                    self.emit_jcc(*kind, label);
                    self.emit_jump_next(*else_bb, next_bb);
                }
                McIR::FCmpJmp(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (FloatPhysReg::Xmm(lhs), rhs) => match rhs {
                            FloatPhysReg::Xmm(rhs) => {
                                monoasm!(self.jit, ucomisd xmm(lhs), xmm(rhs););
                            }
                            FloatPhysReg::Stack(rhs) => {
                                monoasm!(self.jit, ucomisd xmm(lhs), [rbp-(rhs)];);
                            }
                        },
                        (FloatPhysReg::Stack(lhs), rhs) => match rhs {
                            FloatPhysReg::Xmm(rhs) => {
                                monoasm!(self.jit,
                                    movq  xmm0, [rbp-(lhs)];
                                    ucomisd xmm0, xmm(rhs);
                                );
                            }
                            FloatPhysReg::Stack(rhs) => {
                                monoasm!(self.jit,
                                    movq  xmm0, [rbp-(lhs)];
                                    ucomisd xmm0, [rbp-(rhs)];
                                );
                            }
                        },
                    };
                    let label = self.block_labels[self.cur_fn][*then_bb];
                    self.emit_fjcc(*kind, label);
                    self.emit_jump_next(*else_bb, next_bb);
                }
                McIR::GMove(src, dst) => {
                    if src != dst {
                        match (self.g_phys_reg(*src), self.g_phys_reg(*dst)) {
                            (GeneralPhysReg::Reg(src), GeneralPhysReg::Reg(dst)) => {
                                monoasm!(self.jit, movq R(dst), R(src); );
                            }
                            (GeneralPhysReg::Reg(src), GeneralPhysReg::Stack(dst)) => {
                                monoasm!(self.jit, movq [rbp-(dst)], R(src); );
                            }
                            (GeneralPhysReg::Stack(src), GeneralPhysReg::Reg(dst)) => {
                                monoasm!(self.jit, movq R(dst), [rbp-(src)]; );
                            }
                            (GeneralPhysReg::Stack(src), GeneralPhysReg::Stack(dst)) => {
                                monoasm!(self.jit,
                                    movq rax, [rbp-(src)];
                                    movq [rbp-(dst)], rax;
                                );
                            }
                        };
                    }
                }
                McIR::FMove(src, dst) => {
                    if src != dst {
                        match (self.f_phys_reg(*src), self.f_phys_reg(*dst)) {
                            (FloatPhysReg::Xmm(src), FloatPhysReg::Xmm(dst)) => {
                                monoasm!(self.jit, movq xmm(dst), xmm(src); );
                            }
                            (FloatPhysReg::Xmm(src), FloatPhysReg::Stack(dst)) => {
                                monoasm!(self.jit, movq [rbp-(dst)], xmm(src); );
                            }
                            (FloatPhysReg::Stack(src), FloatPhysReg::Xmm(dst)) => {
                                monoasm!(self.jit, movq xmm(dst), [rbp-(src)]; );
                            }
                            (FloatPhysReg::Stack(src), FloatPhysReg::Stack(dst)) => {
                                monoasm!(self.jit,
                                    movq rax, [rbp-(src)];
                                    movq [rbp-(dst)], rax;
                                );
                            }
                        };
                    }
                }
            }
        }
    }
}
