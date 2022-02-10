use monoasm::*;
use monoasm_macro::monoasm;

use super::*;

pub struct Codegen {
    jit: JitMemory,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            jit: JitMemory::new(),
        }
    }

    pub fn compile_and_run(&mut self, hir_context: &HIRContext) {
        let locals = hir_context.register_num();
        self.prologue(locals);
        let epilogue = self.jit.label();
        let mut ret_ty = None;
        for op in &hir_context.hirs {
            match op.kind() {
                HIRKind::Integer(i) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movq  [rbp-(Self::local_offset(reg))], (*i);
                    );
                }
                HIRKind::Float(f) => {
                    let reg = op.reg();
                    let label = self.jit.const_f64(*f);
                    monoasm!(self.jit,
                      movsd  xmm0, [rip + label];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::IAdd(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(*lhs))];
                      addq  rax, [rbp-(Self::local_offset(*rhs))];
                      movq  [rbp-(Self::local_offset(reg))], rax;
                    );
                }
                HIRKind::ISub(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(*lhs))];
                      subq  rax, [rbp-(Self::local_offset(*rhs))];
                      movq  [rbp-(Self::local_offset(reg))], rax;
                    );
                }
                HIRKind::IMul(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(*lhs))];
                      imul  rax, [rbp-(Self::local_offset(*rhs))];
                      movq  [rbp-(Self::local_offset(reg))], rax;
                    );
                }
                HIRKind::IDiv(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(*lhs))];
                      xorq  rdx, rdx;
                      idiv  [rbp-(Self::local_offset(*rhs))];
                      movq  [rbp-(Self::local_offset(reg))], rax;
                    );
                }
                HIRKind::FAdd(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(*lhs))];
                      addsd  xmm0, [rbp-(Self::local_offset(*rhs))];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::FSub(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(*lhs))];
                      subsd  xmm0, [rbp-(Self::local_offset(*rhs))];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::FMul(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(*lhs))];
                      mulsd  xmm0, [rbp-(Self::local_offset(*rhs))];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::FDiv(lhs, rhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(*lhs))];
                      divsd  xmm0, [rbp-(Self::local_offset(*rhs))];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::IntAsFloat(lhs) => {
                    let reg = op.reg();
                    monoasm!(self.jit,
                      cvtsi2sdq xmm0, [rbp-(Self::local_offset(*lhs))];
                      movsd  [rbp-(Self::local_offset(reg))], xmm0;
                    );
                }
                HIRKind::Ret(lhs) => {
                    let ty = hir_context.reg_types[*lhs];
                    match ty {
                        Type::Float => {
                            monoasm!(self.jit,
                              movsd xmm0, [rbp-(Self::local_offset(*lhs))];
                              jmp epilogue;
                            );
                            match ret_ty {
                                None => ret_ty = Some(Type::Float),
                                Some(Type::Float) => {}
                                Some(ret_ty) => {
                                    panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                                }
                            }
                        }
                        Type::Integer => {
                            monoasm!(self.jit,
                              movq rax, [rbp-(Self::local_offset(*lhs))];
                              jmp epilogue;
                            );
                            match ret_ty {
                                None => ret_ty = Some(Type::Integer),
                                Some(Type::Integer) => {}
                                Some(ret_ty) => {
                                    panic!("Return type mismatch {:?} {:?}.", ret_ty, ty)
                                }
                            }
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
                dbg!(func(0.0));
            }
            Some(Type::Float) => {
                let func = self.jit.finalize::<f64, f64>();
                dbg!(func(0.0));
            }
        }
    }

    fn local_offset(register: usize) -> i64 {
        (register * 8) as i64 + 8
    }

    fn prologue(&mut self, locals: usize) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, ((locals + locals % 2)*8);
        );
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            movq rsp, rbp;
            popq rbp;
            ret;
        );
    }
}
