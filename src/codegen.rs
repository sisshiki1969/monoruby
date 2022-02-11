use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

pub struct Codegen {
    jit: JitMemory,
    g_locals: usize,
}

enum GeneralPhysReg {
    Reg(u64),
    Stack(i64),
}

enum FloatPhysReg {
    Xmm(u64),
    Stack(i64),
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            g_locals: 0,
        }
    }

    fn g_phys_reg(&self, reg: GReg) -> GeneralPhysReg {
        let reg = reg.to_usize();
        if reg < 4 {
            GeneralPhysReg::Reg(reg as u64 + 8)
        } else {
            GeneralPhysReg::Stack((reg * 8) as i64 + 8)
        }
    }

    fn f_phys_reg(&self, reg: FReg) -> FloatPhysReg {
        let reg = reg.to_usize();
        if reg < 15 {
            FloatPhysReg::Xmm((reg + 1) as u64)
        } else {
            FloatPhysReg::Stack(((reg + self.g_locals) * 8) as i64 + 8)
        }
    }

    pub fn compile_and_run(&mut self, mcir_context: &McIRContext) -> Value {
        let g_locals = match mcir_context.g_reg_num() {
            i if i < 4 => 0,
            i => i - 3,
        };
        let f_locals = match mcir_context.f_reg_num() {
            i if i < 15 => 0,
            i => i - 14,
        };
        self.g_locals = g_locals;
        self.prologue(g_locals + f_locals);
        let epilogue = self.jit.label();
        let mut ret_ty = None;
        for op in &mcir_context.mcirs {
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
                McIR::IAdd(lhs, rhs) => {
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              addq  R(lhs), R(rhs);
                            );
                        }
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              addq  R(lhs), [rbp-(rhs)];
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              addq  [rbp-(lhs)], R(rhs);
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(rhs)];
                              addq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
                McIR::ISub(lhs, rhs) => {
                    let lhs = self.g_phys_reg(*lhs);
                    let rhs = self.g_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              subq  R(lhs), R(rhs);
                            );
                        }
                        (GeneralPhysReg::Reg(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              subq  R(lhs), [rbp-(rhs)];
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Reg(rhs)) => {
                            monoasm!(self.jit,
                              subq  [rbp-(lhs)], R(rhs);
                            );
                        }
                        (GeneralPhysReg::Stack(lhs), GeneralPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movq  rax, [rbp-(rhs)];
                              subq  [rbp-(lhs)], rax;
                            );
                        }
                    };
                }
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
                McIR::FAdd(lhs, rhs) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              addsd  xmm(lhs), xmm(rhs);
                            );
                        }
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(rhs)];
                              addsd  xmm(lhs), xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              addsd  xmm0, xmm(rhs);
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              addsd  xmm0, [rbp-(rhs)];
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                    }
                }
                McIR::FSub(lhs, rhs) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              subsd  xmm(lhs), xmm(rhs);
                            );
                        }
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(rhs)];
                              subsd  xmm(lhs), xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              subsd  xmm0, xmm(rhs);
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              subsd  xmm0, [rbp-(rhs)];
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                    }
                }
                McIR::FMul(lhs, rhs) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              mulsd  xmm(lhs), xmm(rhs);
                            );
                        }
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(rhs)];
                              mulsd  xmm(lhs), xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              mulsd  xmm0, xmm(rhs);
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              mulsd  xmm0, [rbp-(rhs)];
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                    }
                }
                McIR::FDiv(lhs, rhs) => {
                    let lhs = self.f_phys_reg(*lhs);
                    let rhs = self.f_phys_reg(*rhs);
                    match (lhs, rhs) {
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              divsd  xmm(lhs), xmm(rhs);
                            );
                        }
                        (FloatPhysReg::Xmm(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(rhs)];
                              divsd  xmm(lhs), xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Xmm(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              divsd  xmm0, xmm(rhs);
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                        (FloatPhysReg::Stack(lhs), FloatPhysReg::Stack(rhs)) => {
                            monoasm!(self.jit,
                              movsd  xmm0, [rbp-(lhs)];
                              divsd  xmm0, [rbp-(rhs)];
                              movsd  [rbp-(lhs)], xmm0;
                            );
                        }
                    }
                }
                McIR::IntAsFloat(dst, src) => {
                    let src = self.g_phys_reg(*src);
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
                McIR::FRet(lhs) => {
                    match self.f_phys_reg(*lhs) {
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

                    match ret_ty {
                        None => ret_ty = Some(Type::Integer),
                        Some(Type::Integer) => {}
                        Some(ty) => {
                            panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        self.jit.bind_label(epilogue);
        self.epilogue();
        match ret_ty {
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
        }
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
