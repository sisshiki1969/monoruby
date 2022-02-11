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

    pub fn compile_and_run(&mut self, hir_context: &HIRContext) -> Value {
        let locals = hir_context.register_num();
        self.prologue(locals);
        let epilogue = self.jit.label();
        let mut ret_ty = None;
        for op in &hir_context.hirs {
            match op {
                HIR::Integer(reg, i) => {
                    monoasm!(self.jit,
                      movq  [rbp-(Self::local_offset(*reg))], (*i);
                    );
                }
                HIR::Float(reg, f) => {
                    let label = self.jit.const_f64(*f);
                    monoasm!(self.jit,
                      movsd  xmm0, [rip + label];
                      movsd  [rbp-(Self::local_offset(*reg))], xmm0;
                    );
                }
                HIR::IAdd(op) => {
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(op.lhs))];
                      addq  rax, [rbp-(Self::local_offset(op.rhs))];
                      movq  [rbp-(Self::local_offset(op.ret))], rax;
                    );
                }
                HIR::ISub(op) => {
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(op.lhs))];
                      subq  rax, [rbp-(Self::local_offset(op.rhs))];
                      movq  [rbp-(Self::local_offset(op.ret))], rax;
                    );
                }
                HIR::IMul(op) => {
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(op.lhs))];
                      imul  rax, [rbp-(Self::local_offset(op.rhs))];
                      movq  [rbp-(Self::local_offset(op.ret))], rax;
                    );
                }
                HIR::IDiv(op) => {
                    monoasm!(self.jit,
                      movq  rax, [rbp-(Self::local_offset(op.lhs))];
                      xorq  rdx, rdx;
                      idiv  [rbp-(Self::local_offset(op.rhs))];
                      movq  [rbp-(Self::local_offset(op.ret))], rax;
                    );
                }
                HIR::FAdd(op) => {
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(op.lhs))];
                      addsd  xmm0, [rbp-(Self::local_offset(op.rhs))];
                      movsd  [rbp-(Self::local_offset(op.ret))], xmm0;
                    );
                }
                HIR::FSub(op) => {
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(op.lhs))];
                      subsd  xmm0, [rbp-(Self::local_offset(op.rhs))];
                      movsd  [rbp-(Self::local_offset(op.ret))], xmm0;
                    );
                }
                HIR::FMul(op) => {
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(op.lhs))];
                      mulsd  xmm0, [rbp-(Self::local_offset(op.rhs))];
                      movsd  [rbp-(Self::local_offset(op.ret))], xmm0;
                    );
                }
                HIR::FDiv(op) => {
                    monoasm!(self.jit,
                      movsd  xmm0, [rbp-(Self::local_offset(op.lhs))];
                      divsd  xmm0, [rbp-(Self::local_offset(op.rhs))];
                      movsd  [rbp-(Self::local_offset(op.ret))], xmm0;
                    );
                }
                HIR::IntAsFloat(op) => {
                    monoasm!(self.jit,
                      cvtsi2sdq xmm0, [rbp-(Self::local_offset(op.src))];
                      movsd  [rbp-(Self::local_offset(op.ret))], xmm0;
                    );
                }
                HIR::Ret(lhs) => {
                    let ty = hir_context[*lhs].ty;
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

    fn local_offset(register: SsaReg) -> i64 {
        (register.to_usize() * 8) as i64 + 8
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
