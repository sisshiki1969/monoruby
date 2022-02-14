use std::io::Write;

use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

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
    /// A number of general registers which is spilled to the stack..
    g_locals: usize,
}

macro_rules! integer_ops {
    ($self: ident, $op: ident, $lhs:ident, $rhs:ident) => {{
        let lhs = $self.g_phys_reg(*$lhs);
        match &$rhs {
            McGeneralOperand::Reg(rhs) => {
                let rhs = $self.g_phys_reg(*rhs);
                match (lhs, rhs) {
                    (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                        monoasm!($self.jit,
                          $op  R(lhs), R(rhs);
                        );
                    }
                    (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                        monoasm!($self.jit,
                          $op  R(lhs), [rbp-(rhs)];
                        );
                    }
                    (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                        monoasm!($self.jit,
                          $op  [rbp-(lhs)], R(rhs);
                        );
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
                        monoasm!($self.jit,
                          $op  R(lhs), (*rhs as i64);
                        );
                    }
                    GeneralPhysReg::Stack(lhs) => {
                        monoasm!($self.jit,
                          $op  [rbp-(lhs)], (*rhs as i64);
                        );
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
                let label = $self.jit.const_f64(*rhs);
                match lhs {
                    FloatPhysReg::Xmm(lhs) => {
                        monoasm!($self.jit,
                          $op    xmm(lhs), [rip + label];
                        );
                    }
                    FloatPhysReg::Stack(lhs) => {
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
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            g_locals: 0,
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
            GeneralPhysReg::Stack(((reg - 4) * 8) as i64 + 8)
        }
    }

    ///
    /// Allocate general register to physical register.
    ///
    /// Currently, first 14 registers are allocated to xmm1-xmm15 registers.
    ///
    fn f_phys_reg(&self, reg: FReg) -> FloatPhysReg {
        let reg = reg.to_usize();
        if reg < 15 {
            FloatPhysReg::Xmm((reg + 1) as u64)
        } else {
            FloatPhysReg::Stack((((reg - 14) + self.g_locals) * 8) as i64 + 8)
        }
    }

    pub fn compile_and_run(&mut self, mcir_context: &MachineIRContext) -> Value {
        let g_locals = match mcir_context.g_reg_num() {
            i if i < 5 => 0,
            i => i - 4,
        };
        let f_locals = match mcir_context.f_reg_num() {
            i if i < 15 => 0,
            i => i - 14,
        };
        self.g_locals = g_locals;
        self.prologue(g_locals + f_locals);
        let epilogue = self.jit.label();
        let mut ret_ty = None;

        for op in &mcir_context.insts {
            match op {
                McIR::Integer(reg, i) => match self.g_phys_reg(*reg) {
                    GeneralPhysReg::Reg(reg) => {
                        monoasm!(self.jit,
                          movq  R(reg), (*i);
                        );
                    }
                    GeneralPhysReg::Stack(ofs) => {
                        monoasm!(self.jit,
                          movq  [rbp-(ofs)], (*i);
                        );
                    }
                },
                McIR::Float(reg, f) => {
                    let label = self.jit.const_f64(*f);
                    match self.f_phys_reg(*reg) {
                        FloatPhysReg::Xmm(reg) => {
                            monoasm!(self.jit,
                              movsd  xmm(reg), [rip + label];
                            );
                        }
                        FloatPhysReg::Stack(ofs) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rip + label];
                              movsd  [rbp-(ofs)], xmm0;
                            );
                        }
                    }
                }
                McIR::IAdd(lhs, rhs) => integer_ops!(self, addq, lhs, rhs),
                McIR::ISub(lhs, rhs) => integer_ops!(self, subq, lhs, rhs),
                McIR::IMul(lhs, rhs) => {
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                              imul  rax, R(rhs);
                              movq  R(lhs), rax;
                            );
                        }
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                              imul  rax, [rbp-(rhs)];
                              movq  R(lhs), rax;
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                              imul  rax, R(rhs);
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                              imul  rax, [rbp-(rhs)];
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
                McIR::IDiv(lhs, rhs) => {
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                              xorq  rdx, rdx;
                              idiv  R(rhs);
                              movq  R(lhs), rax;
                            );
                        }
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, R(lhs);
                              xorq  rdx, rdx;
                              idiv  [rbp-(rhs)];
                              movq  R(lhs), rax;
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                              xorq  rdx, rdx;
                              idiv  R(rhs);
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(lhs)];
                              xorq  rdx, rdx;
                              idiv  [rbp-(rhs)];
                              movq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
                McIR::FAdd(lhs, rhs) => float_ops!(self, addsd, lhs, rhs),
                McIR::FSub(lhs, rhs) => float_ops!(self, subsd, lhs, rhs),
                McIR::FMul(lhs, rhs) => float_ops!(self, mulsd, lhs, rhs),
                McIR::FDiv(lhs, rhs) => float_ops!(self, divsd, lhs, rhs),

                McIR::IntAsFloat(dst, src) => match src {
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
                    match lhs {
                        McFloatOperand::Float(f) => {
                            let label = self.jit.const_f64(*f);
                            monoasm!(self.jit,
                              movsd xmm0, [rip + label];
                              jmp epilogue;
                            );
                        }
                        McFloatOperand::Reg(lhs) => match self.f_phys_reg(*lhs) {
                            FloatPhysReg::Xmm(lhs) => {
                                monoasm!(self.jit,
                                  movsd xmm0, xmm(lhs);
                                  jmp epilogue;
                                );
                            }
                            FloatPhysReg::Stack(ofs) => {
                                monoasm!(self.jit,
                                  movsd xmm0, [rbp-(ofs)];
                                  jmp epilogue;
                                );
                            }
                        },
                    }
                    match ret_ty {
                        None => ret_ty = Some(Type::Float),
                        Some(Type::Float) => {}
                        Some(ty) => {
                            panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                        }
                    }
                }
                McIR::IRet(lhs) => {
                    match lhs {
                        McGeneralOperand::Integer(i) => {
                            monoasm!(self.jit,
                              movq rax, (*i as i64);
                              jmp epilogue;
                            );
                        }
                        McGeneralOperand::Reg(lhs) => {
                            match self.g_phys_reg(*lhs) {
                                GeneralPhysReg::Reg(reg) => {
                                    monoasm!(self.jit,
                                      movq rax, R(reg);
                                      jmp epilogue;
                                    );
                                }
                                GeneralPhysReg::Stack(lhs) => {
                                    monoasm!(self.jit,
                                      movq rax, [rbp-(lhs)];
                                      jmp epilogue;
                                    );
                                }
                            };
                        }
                    }

                    match ret_ty {
                        None => ret_ty = Some(Type::Integer),
                        Some(Type::Integer) => {}
                        Some(ty) => {
                            panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                        }
                    }
                }
                McIR::INeg(reg) => {
                    match self.g_phys_reg(*reg) {
                        GeneralPhysReg::Reg(reg) => {
                            monoasm!(self.jit,
                              negq R(reg);
                            );
                        }
                        GeneralPhysReg::Stack(lhs) => {
                            monoasm!(self.jit,
                              negq [rbp-(lhs)];
                            );
                        }
                    };
                }
                McIR::FNeg(reg) => {
                    let label = self.jit.const_f64(0.0);
                    match self.f_phys_reg(*reg) {
                        FloatPhysReg::Xmm(reg) => {
                            monoasm!(self.jit,
                              movsd xmm0, [rip + label];
                              subsd xmm0, xmm(reg);
                              movsd xmm(reg), xmm0;
                            );
                        }
                        FloatPhysReg::Stack(lhs) => {
                            monoasm!(self.jit,
                                movsd xmm0, [rip + label];
                                subsd xmm0, [rbp-(lhs)];
                                movsd [rbp-(lhs)], xmm0;
                            );
                        }
                    };
                }
            }
        }
        self.jit.bind_label(epilogue);
        self.epilogue();
        let res = match ret_ty {
            None => unreachable!(),
            Some(Type::Integer) => {
                let func = self.jit.finalize::<f64, i64>();
                let i = dbg!(func(0.0));
                Value::Integer(i as i32)
            }
            Some(Type::Float) => {
                let func = self.jit.finalize::<f64, f64>();
                let f = dbg!(func(0.0));
                Value::Float(f)
            }
        };
        self.dump_code();

        res
    }

    /// Dump generated code.
    fn dump_code(&self) {
        use std::fs::File;
        use std::process::Command;
        let asm = self.jit.to_vec();
        let mut file = File::create("tmp.bin").unwrap();
        file.write_all(&asm).unwrap();

        let output = Command::new("objdump")
            .args(&[
                "-D",
                "-Mintel,x86-64",
                "-b",
                "binary",
                "-m",
                "i386",
                "tmp.bin",
            ])
            .output();
        let asm = match &output {
            Ok(output) => std::str::from_utf8(&output.stdout).unwrap().to_string(),
            Err(err) => err.to_string(),
        };
        eprintln!("asm: {}", asm);
    }

    fn prologue(&mut self, locals: usize) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
        );
        if locals != 0 {
            monoasm!(self.jit,
                subq rsp, ((locals + locals % 2) * 8);
            );
        }
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            movq rsp, rbp;
            popq rbp;
            ret;
        );
    }
}
