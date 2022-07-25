use monoasm_macro::monoasm;

use super::*;

#[derive(PartialEq)]
enum TIr {
    /// branch(dest)
    Br(i32),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was true.
    CondBr(u16, i32, bool),
    /// conditional branch(%reg, dest, optimizable)  : branch when reg was false.
    CondNotBr(u16, i32, bool),
    /// integer(%reg, i32)
    Integer(u16, i32),
    /// Symbol(%reg, IdentId)
    Symbol(u16, IdentId),
    /// literal(%ret, literal_id)
    Literal(u16, u32),
    LoadConst(u16, ConstSiteId),
    StoreConst(u16, IdentId),
    /// nil(%reg)
    Nil(u16),
    /// negate(%ret, %src)
    Neg(u16, u16),
    /// binop(kind, %ret, %lhs, %rhs)
    BinOp(BinOpK, u16, u16, u16, ClassId),
    /// binop with small integer(kind, %ret, %lhs, %rhs)
    BinOpRi(BinOpK, u16, u16, i16, ClassId),
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(CmpKind, u16, u16, u16, bool, ClassId),
    /// cmpri(%ret, %lhs, rhs: i16, optimizable)
    Cmpri(CmpKind, u16, u16, i16, bool, ClassId),
    /// return(%ret)
    Ret(u16),
    /// move(%dst, %src)
    Mov(u16, u16),
    /// func call(%ret, name)
    MethodCall(u16, IdentId),
    /// func call 2nd opecode(%recv, %args, %len)
    MethodArgs(u16, u16, u16),
    /// method definition(method_def_id)
    MethodDef(MethodDefId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(u16, u16, u16),
    /// float_binop(kind, Fret, Flhs, Frhs)
    FBinOp(BinOpK, u16, u16, u16),
    /// float_binop(kind, Fret, Flhs, f64)
    FBinOpRf(BinOpK, u16, u16, f64),
    /// float_cmp(%ret, Flhs, Frhs, optimizable)
    FCmp(CmpKind, u16, u16, u16, bool),
    /// float_cmpri(%ret, Flhs, rhs: f16, optimizable)
    FCmpRf(CmpKind, u16, u16, f64, bool),
    /// negate(Fret, Fsrc)
    FNeg(u16, u16),
    /// load float from register(%src, Fdst)
    FLoad(u16, u16),
    /// store float to register(Fsrc, %dst)
    FStore(u16, u16),
    /// write back float to registers(Vec<(Fsrc, %dst)>).
    WriteBack(Vec<(u16, u16)>),
}

impl std::fmt::Debug for TIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn optstr(opt: &bool) -> &'static str {
            if *opt {
                "_"
            } else {
                ""
            }
        }
        fn disp_str(disp: &i32) -> String {
            if *disp >= 0 {
                format!("+{:05}", disp + 1)
            } else {
                format!("{:05}", disp + 1)
            }
        }
        fn disp_write_back(v: &[(u16, u16)]) -> String {
            v.iter()
                .map(|(fsrc, dst)| format!("%{}:=F{} ", dst, fsrc))
                .fold(String::new(), |acc, s| acc + &s)
        }
        match self {
            TIr::Br(disp) => {
                writeln!(f, "br => {}", disp_str(disp),)
            }
            TIr::CondBr(reg, disp, opt) => {
                writeln!(f, "condbr {}%{} => {}", optstr(opt), reg, disp_str(disp))
            }
            TIr::CondNotBr(reg, disp, opt) => {
                writeln!(f, "condnbr {}%{} => {}", optstr(opt), reg, disp_str(disp))
            }
            TIr::Integer(reg, num) => writeln!(f, "%{} = {}: i32", reg, num),
            TIr::Symbol(reg, id) => {
                writeln!(f, "%{} = {:?}", reg, id)
            }
            TIr::Literal(reg, id) => {
                writeln!(f, "%{} = literal[#{}]", reg, id)
            }
            TIr::LoadConst(reg, id) => {
                writeln!(f, "%{} = const[{:?}]", reg, id)
            }
            TIr::StoreConst(reg, id) => {
                writeln!(f, "const[{:?}] = %{}", id, reg)
            }
            TIr::FLoad(src, fdst) => {
                writeln!(f, "F{} = %{}", fdst, src)
            }
            TIr::FStore(fsrc, dst) => {
                writeln!(f, "%{} = F{}", dst, fsrc)
            }
            TIr::Nil(reg) => writeln!(f, "%{} = nil", reg),
            TIr::Neg(dst, src) => writeln!(f, "%{} = neg %{}", dst, src),
            TIr::FNeg(dst, src) => writeln!(f, "F{} = neg F{}", dst, src),
            TIr::BinOp(kind, dst, lhs, rhs, class) => {
                writeln!(f, "%{} = %{}:{:?} {} %{}", dst, lhs, class, kind, rhs)
            }
            TIr::BinOpRi(kind, dst, lhs, rhs, class) => {
                writeln!(f, "%{} = %{}:{:?} {} {}: i16", dst, lhs, class, kind, rhs)
            }
            TIr::FBinOp(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = F{} {} F{}", dst, lhs, kind, rhs)
            }
            TIr::FBinOpRf(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = F{} {} {}: f64", dst, lhs, kind, rhs)
            }
            TIr::Cmp(kind, dst, lhs, rhs, opt, class) => {
                writeln!(
                    f,
                    "{}%{} = %{}:{:?} {:?} %{}",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::Cmpri(kind, dst, lhs, rhs, opt, class) => {
                writeln!(
                    f,
                    "{}%{} = %{}:{:?} {:?} {}: i16",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::FCmp(kind, dst, lhs, rhs, opt) => {
                writeln!(f, "{}%{} = F{} {:?} F{}", optstr(opt), dst, lhs, kind, rhs)
            }
            TIr::FCmpRf(kind, dst, lhs, rhs, opt) => {
                writeln!(
                    f,
                    "{}%{} = F{} {:?} {}: f64",
                    optstr(opt),
                    dst,
                    lhs,
                    kind,
                    rhs
                )
            }

            TIr::Ret(reg) => writeln!(f, "ret %{}", reg),
            TIr::Mov(dst, src) => writeln!(f, "%{} = %{}", dst, src),
            TIr::MethodCall(ret, name) => {
                writeln!(
                    f,
                    "{} = call {:?}",
                    match ret {
                        0 => "_".to_string(),
                        ret => format!("%{:?}", ret),
                    },
                    name,
                )
            }
            TIr::MethodArgs(recv, args, len) => {
                writeln!(f, "%{}.call_args (%{}; {})", recv, args, len)
            }
            TIr::MethodDef(id) => {
                writeln!(f, "define {:?}", id)
            }
            TIr::ConcatStr(ret, args, len) => match ret {
                0 => writeln!(f, "_ = concat(%{}; {})", args, len),
                ret => writeln!(f, "%{:?} = concat(%{}; {})", ret, args, len),
            },
            TIr::WriteBack(v) => writeln!(f, "write_back {}", disp_write_back(v)),
        }
    }
}

struct FloatContext {
    reg_info: Vec<Option<u16>>,
    xmm: [Vec<u16>; 13],
    tir: Vec<TIr>,
}

impl FloatContext {
    fn new(reg_num: usize) -> Self {
        let xmm = (0..13)
            .map(|_| vec![])
            .collect::<Vec<Vec<u16>>>()
            .try_into()
            .unwrap();
        Self {
            reg_info: vec![None; reg_num],
            xmm,
            tir: vec![],
        }
    }

    fn push(&mut self, tir: TIr) {
        self.tir.push(tir);
    }

    fn alloc(&mut self, reg: u16) -> u16 {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                self.reg_info[reg as usize] = Some(flhs as u16);
                xmm.push(reg);
                return flhs as u16;
            }
        }
        unreachable!()
    }

    fn dealloc(&mut self, reg: u16) {
        if let Some(freg) = self.reg_info[reg as usize] {
            assert!(self.xmm[freg as usize].contains(&reg));
            self.xmm[freg as usize].retain(|e| *e != reg);
            self.reg_info[reg as usize] = None;
        };
    }

    fn copy(&mut self, src: u16, dst: u16) -> Option<u16> {
        self.dealloc(dst);
        let f = self.reg_info[src as usize];
        if let Some(freg) = f {
            self.reg_info[dst as usize] = Some(freg);
            self.xmm[freg as usize].push(dst);
        }
        f
    }

    fn write_back(&mut self, codegen: &mut Codegen, reg: u16) {
        if let Some(freg) = self.reg_info[reg as usize] {
            self.push(TIr::FStore(freg, reg));
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
                movq [rbp - (conv(reg))], rax;
            );
            self.dealloc(reg);
        }
    }

    fn write_back_no_dealloc(&mut self, codegen: &mut Codegen, reg: u16) {
        if let Some(freg) = self.reg_info[reg as usize] {
            //self.push(TIr::FStore(freg, reg));
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
                movq [rbp - (conv(reg))], rax;
            );
        }
    }

    fn write_back_range(&mut self, codegen: &mut Codegen, arg: u16, len: u16) {
        for reg in arg..arg + len {
            self.write_back(codegen, reg)
        }
    }

    fn branch_write_back(&mut self, codegen: &mut Codegen) {
        let v = self.get_write_back();
        v.iter()
            .for_each(|(_, reg)| self.write_back_no_dealloc(codegen, *reg));
    }

    fn all_write_back(&mut self, codegen: &mut Codegen) {
        let v = self.get_write_back();
        v.iter().for_each(|(_, reg)| self.write_back(codegen, *reg));
        /*if !v.is_empty() {
            self.push(TIr::WriteBack(v));
        }*/
        assert!(self.xmm.iter().all(|v| v.is_empty()));
        assert!(self.reg_info.iter().all(|r| r.is_none()));
    }

    fn get_write_back(&mut self) -> Vec<(u16, u16)> {
        self.reg_info
            .iter()
            .enumerate()
            .filter_map(|(reg, freg)| freg.map(|freg| (freg as u16, reg as u16)))
            .collect()
    }

    fn get_float(&mut self, codegen: &mut Codegen, reg: u16) -> u16 {
        if let Some(freg) = self.reg_info[reg as usize] {
            freg
        } else {
            let freg = self.alloc(reg);
            self.push(TIr::FLoad(reg, freg as u16));
            let val_to_f64 = codegen.val_to_f64;
            monoasm!(codegen.jit,
                movq rdi, [rbp - (conv(reg))];
                call val_to_f64;
                movq xmm(freg as u64 + 2), xmm0;
            );
            freg
        }
    }
}

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            pub(crate) fn [<cmp_ $op>](&mut self, generic:DestLabel) {
                let exit = self.jit.label();
                monoasm! { self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                exit:
                };
                self.jit.select(1);
                monoasm!(self.jit,
                    generic:
                );
                self.xmm_save();
                monoasm!(self.jit,
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                );
                self.xmm_restore();
                monoasm!(self.jit,
                    jmp  exit;
                );
                self.jit.select(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_main!($op1);
        cmp_main!($($op2),+);
    };
}

macro_rules! cmp_opt_main {
    ($op:ident) => {
        paste! {
            fn [<cmp_opt_ $op>](&mut self, dest: DestLabel, generic:DestLabel, switch:bool, ctx: &mut FloatContext) {
                let cont = self.jit.label();
                monoasm! { self.jit,
                    cmpq rdi, rsi;
                };
                if switch {
                    let write_back = self.jit.label();
                    monoasm! { self.jit,
                        [<j $op>] write_back;
                        jmp cont;
                    write_back:
                    };
                    ctx.branch_write_back(self);
                    monoasm! { self.jit,
                        jmp dest;
                    };
                } else {
                    monoasm! { self.jit,
                        [<j $op>] cont;
                    };
                    ctx.branch_write_back(self);
                    monoasm! { self.jit,
                        jmp dest;
                    };
                }
                self.jit.bind_label(cont);
                self.jit.select(1);
                monoasm!(self.jit,
                    generic:
                );
                self.xmm_save();
                monoasm!(self.jit,
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                );
                self.xmm_restore();
                monoasm!(self.jit,
                    orq  rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                    // if true, Z=0(not set).
                );
                if switch {
                    monoasm!(self.jit,
                        jz   cont;
                    );
                    ctx.branch_write_back(self);
                    monoasm! { self.jit,
                        jmp dest;
                    };
                } else {
                    monoasm!(self.jit,
                        jnz  cont;
                    );
                    ctx.branch_write_back(self);
                    monoasm! { self.jit,
                        jmp dest;
                    };
                }
                self.jit.select(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_opt_main!($op1);
        cmp_opt_main!($($op2),+);
    };
}

impl Codegen {
    cmp_opt_main!(eq, ne, lt, le, gt, ge);
    cmp_main!(eq, ne, lt, le, gt, ge);

    pub(super) fn jit_compile_normal(
        &mut self,
        func: &NormalFuncInfo,
        store: &FnStore,
        position: Option<usize>,
    ) -> DestLabel {
        let start_pos = position.unwrap_or_default();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            eprintln!(
                "--> start {} compile: {} {:?} position={}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                match func.name() {
                    Some(name) => name,
                    None => "<unnamed>",
                },
                func.id,
                start_pos
            );
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = Instant::now();

        let entry = self.jit.label();
        let mut labels = vec![];
        for _ in &func.bytecode()[start_pos..] {
            labels.push(self.jit.label());
        }
        self.jit.bind_label(entry);

        if position.is_none() {
            self.prologue(func.total_reg_num());
        }

        let info = func.get_bb_info();

        let mut skip = false;
        let mut loop_count = 0;
        let mut ctx = FloatContext::new(func.total_reg_num());
        for (idx, op) in func.bytecode()[start_pos..].iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            if let Some(_) = info[start_pos + idx] {
                ctx.all_write_back(self);
            }
            self.jit.bind_label(labels[idx]);
            let ops = BcOp1::from_bc(*op);
            match ops {
                BcOp1::LoopStart(_) => {
                    loop_count += 1;
                }
                BcOp1::LoopEnd => {
                    assert_ne!(0, loop_count);
                    loop_count -= 1;
                    if position.is_some() && loop_count == 0 {
                        let fetch = self.vm_fetch;
                        let pc = func.get_bytecode_address(start_pos + idx + 1);

                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("    return pc:[{}] {:?}", start_pos + idx + 1, pc);

                        monoasm!(self.jit,
                            movq r13, (pc.0);
                            jmp fetch;
                        );

                        eprint!("{:?}", ctx.tir);
                        break;
                    }
                }
                BcOp1::Integer(ret, i) => {
                    ctx.push(TIr::Integer(ret, i));
                    ctx.dealloc(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp1::Symbol(ret, id) => {
                    ctx.push(TIr::Symbol(ret, id));
                    ctx.dealloc(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                BcOp1::Literal(ret, id) => {
                    ctx.push(TIr::Literal(ret, id));
                    ctx.dealloc(ret);
                    let v = store.get_literal(id);
                    if v.is_packed_value() {
                        monoasm!(self.jit,
                          movq rax, (v.get());
                        );
                    } else {
                        monoasm!(self.jit,
                          movq rdi, (v.get());
                          movq rax, (Value::dup);
                          call rax;
                        );
                    }
                    self.store_rax(ret);
                }
                BcOp1::LoadConst(ret, id) => {
                    ctx.push(TIr::LoadConst(ret, id));
                    ctx.dealloc(ret);
                    let jit_return = self.vm_return;
                    let cached_const_version = self.jit.const_i64(-1);
                    let cached_value = self.jit.const_i64(0);
                    let global_const_version = self.const_version;
                    let slow_path = self.jit.label();
                    let exit = self.jit.label();
                    monoasm!(self.jit,
                        movq rax, [rip + global_const_version];
                        cmpq rax, [rip + cached_const_version];
                        jne  slow_path;
                        movq rax, [rip + cached_value];
                    exit:
                    );
                    self.store_rax(ret);
                    self.jit.select(1);
                    monoasm!(self.jit,
                    slow_path:
                        movq rdx, (id.get());  // name: ConstSiteId
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        movq rax, (get_constant);
                        call rax;
                        testq rax, rax;
                        jeq  jit_return;
                        movq [rip + cached_value], rax;
                        movq rdi, [rip + global_const_version];
                        movq [rip + cached_const_version], rdi;
                        jmp  exit;
                    );
                    self.jit.select(0);
                }
                BcOp1::StoreConst(ret, id) => {
                    ctx.push(TIr::StoreConst(ret, id));
                    ctx.dealloc(ret);
                    let const_version = self.const_version;
                    monoasm!(self.jit,
                      movq rdx, (id.get());  // name: IdentId
                      movq rcx, [rbp - (conv(ret))];  // val: Value
                      movq rdi, rbx;  // &mut Interp
                      movq rsi, r12;  // &mut Globals
                      addq [rip + const_version], 1;
                      movq rax, (set_constant);
                      call rax;
                    );
                }
                BcOp1::Nil(ret) => {
                    ctx.push(TIr::Nil(ret));
                    ctx.dealloc(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp1::Neg(dst, src) => {
                    if op.is_float() {
                        let fsrc = ctx.get_float(self, src);
                        ctx.dealloc(dst);
                        let fdst = ctx.alloc(dst);
                        ctx.push(TIr::FNeg(fdst, fsrc));
                        monoasm!(self.jit,
                            movq rax, (-1);
                            movq xmm(fdst as u64 + 2), rax;
                            mulsd xmm(fdst as u64 + 2), xmm(fsrc as u64 + 2);
                        );
                    } else {
                        ctx.dealloc(dst);
                        ctx.write_back(self, src);
                        ctx.push(TIr::Neg(dst, src));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.store_rax(dst);
                    }
                }
                BcOp1::BinOp(kind, ret, lhs, rhs) => {
                    if op.is_float() {
                        let flhs = ctx.get_float(self, lhs);
                        let frhs = ctx.get_float(self, rhs);
                        ctx.dealloc(ret);
                        let fret = ctx.alloc(ret);
                        ctx.push(TIr::FBinOp(kind, fret as u16, flhs as u16, frhs as u16));
                        //self.xmm_mov(flhs, fret);
                        match kind {
                            BinOpK::Add => monoasm!(self.jit,
                                movq  xmm0, xmm(flhs as u64 + 2);
                                addsd xmm0, xmm(frhs as u64 + 2);
                                movq  xmm(fret as u64 + 2), xmm0;
                            ),
                            BinOpK::Sub => monoasm!(self.jit,
                                movq  xmm0, xmm(flhs as u64 + 2);
                                subsd xmm0, xmm(frhs as u64 + 2);
                                movq  xmm(fret as u64 + 2), xmm0;
                            ),
                            BinOpK::Mul => monoasm!(self.jit,
                                movq  xmm0, xmm(flhs as u64 + 2);
                                mulsd xmm0, xmm(frhs as u64 + 2);
                                movq  xmm(fret as u64 + 2), xmm0;
                            ),
                            BinOpK::Div => {
                                let div_by_zero = self.div_by_zero;
                                monoasm!(self.jit,
                                    movq  rax, xmm(frhs as u64 + 2);
                                    cmpq  rax, 0;
                                    jeq   div_by_zero;
                                    movq  xmm0, xmm(flhs as u64 + 2);
                                    divsd xmm0, xmm(frhs as u64 + 2);
                                    movq  xmm(fret as u64 + 2), xmm0;
                                )
                            }
                            _ => unimplemented!(),
                        }
                    } else {
                        ctx.dealloc(ret);
                        ctx.write_back(self, lhs);
                        ctx.write_back(self, rhs);
                        ctx.push(TIr::BinOp(kind, ret, lhs, rhs, op.classid()));
                        self.load_binary_args(lhs, rhs);
                        match kind {
                            BinOpK::Add => match op.classid() {
                                INTEGER_CLASS => {
                                    let generic = self.jit.label();
                                    self.guard_rdi_rsi_fixnum(generic);
                                    self.gen_add(generic, ret);
                                }
                                _ => self.generic_binop(ret, add_values as _),
                            },
                            BinOpK::Sub => match op.classid() {
                                INTEGER_CLASS => {
                                    let generic = self.jit.label();
                                    self.guard_rdi_rsi_fixnum(generic);
                                    self.gen_sub(generic, ret);
                                }
                                _ => self.generic_binop(ret, sub_values as _),
                            },
                            BinOpK::Mul => {
                                self.generic_binop(ret, mul_values as _);
                            }
                            BinOpK::Div => {
                                self.generic_binop(ret, div_values as _);
                            }
                            _ => {
                                let generic = self.jit.label();
                                self.guard_rdi_rsi_fixnum(generic);
                                match kind {
                                    BinOpK::BitOr => self.gen_bit_or(generic, ret),
                                    BinOpK::BitAnd => self.gen_bit_and(generic, ret),
                                    BinOpK::BitXor => self.gen_bit_xor(generic, ret),
                                    BinOpK::Shr => self.gen_shr(generic, ret),
                                    BinOpK::Shl => self.gen_shl(generic, ret),
                                    _ => unimplemented!(),
                                }
                            }
                        }
                    }
                }

                BcOp1::BinOpRi(kind, ret, lhs, rhs) => {
                    if op.is_float() {
                        let flhs = ctx.get_float(self, lhs);
                        ctx.dealloc(ret);
                        let fret = ctx.alloc(ret);
                        ctx.push(TIr::FBinOpRf(kind, fret as u16, flhs as u16, rhs as f64));
                        let imm = self.jit.const_f64(rhs as f64);
                        self.xmm_mov(flhs, fret);
                        match kind {
                            BinOpK::Add => monoasm!(self.jit,
                                addsd xmm(fret as u64 + 2), [rip + imm];
                            ),
                            BinOpK::Sub => monoasm!(self.jit,
                                subsd xmm(fret as u64 + 2), [rip + imm];
                            ),
                            BinOpK::Mul => monoasm!(self.jit,
                                mulsd xmm(fret as u64 + 2), [rip + imm];
                            ),
                            BinOpK::Div => {
                                let div_by_zero = self.div_by_zero;
                                monoasm!(self.jit,
                                    xorq  rax, rax;
                                    cmpq  rax, [rip + imm];
                                    jeq   div_by_zero;
                                    divsd xmm(fret as u64 + 2), [rip + imm];
                                )
                            }
                            _ => unimplemented!(),
                        }
                    } else {
                        ctx.dealloc(ret);
                        ctx.write_back(self, lhs);
                        ctx.push(TIr::BinOpRi(kind, ret, lhs, rhs, op.classid()));

                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                        );
                        match kind {
                            BinOpK::Add => match op.classid() {
                                INTEGER_CLASS => {
                                    let generic = self.jit.label();
                                    monoasm!(self.jit,
                                        movq rsi, (Value::int32(rhs as i32).get());
                                    );
                                    self.guard_rdi_fixnum(generic);
                                    self.gen_add(generic, ret);
                                }
                                _ => {
                                    monoasm!(self.jit,
                                        movq rsi, (Value::int32(rhs as i32).get());
                                    );
                                    self.generic_binop(ret, add_values as _);
                                }
                            },
                            BinOpK::Sub => match op.classid() {
                                INTEGER_CLASS => {
                                    let generic = self.jit.label();
                                    monoasm!(self.jit,
                                        movq rsi, (Value::int32(rhs as i32).get());
                                    );
                                    self.guard_rdi_fixnum(generic);
                                    self.gen_sub(generic, ret);
                                }
                                _ => {
                                    monoasm!(self.jit,
                                        movq rsi, (Value::int32(rhs as i32).get());
                                    );
                                    self.generic_binop(ret, sub_values as _);
                                }
                            },
                            _ => unimplemented!(),
                        }
                    }
                }

                BcOp1::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    //if op.is_float() {
                    //    let flhs = ctx.get_float(self, lhs);
                    //    let frhs = ctx.get_float(self, rhs);
                    //    ctx.dealloc(ret);
                    //    ctx.push(TIr::FCmp(kind, ret, flhs as u16, frhs as u16, optimizable));
                    //} else {
                    ctx.dealloc(ret);
                    ctx.write_back(self, lhs);
                    ctx.write_back(self, rhs);
                    ctx.push(TIr::Cmp(kind, ret, lhs, rhs, optimizable, op.classid()));
                    //}
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(*op);
                    } else {
                        let generic = self.jit.label();
                        self.load_binary_args(lhs, rhs);
                        self.guard_rdi_rsi_fixnum(generic);
                        self.gen_cmp_kind(kind, generic, ret);
                    }
                }
                BcOp1::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    //if op.is_float() {
                    //    let flhs = ctx.get_float(self, lhs);
                    //    ctx.dealloc(ret);
                    //    ctx.push(TIr::FCmpRf(kind, ret, flhs as u16, rhs as f64, optimizable));
                    //} else {
                    ctx.dealloc(ret);
                    ctx.write_back(self, lhs);
                    ctx.push(TIr::Cmpri(kind, ret, lhs, rhs, optimizable, op.classid()));
                    //}
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(*op);
                    } else {
                        let generic = self.jit.label();
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::new_integer(rhs as i64).get());
                        );
                        self.guard_rdi_fixnum(generic);
                        self.gen_cmp_kind(kind, generic, ret);
                    }
                }
                BcOp1::Mov(dst, src) => {
                    if ctx.copy(src, dst).is_none() {
                        ctx.push(TIr::Mov(dst, src));
                        monoasm!(self.jit,
                          movq rax, [rbp - (conv(src))];
                          movq [rbp - (conv(dst))], rax;
                        );
                    }
                }
                BcOp1::Ret(lhs) => {
                    ctx.push(TIr::Ret(lhs));
                    ctx.write_back(self, lhs);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                }
                BcOp1::ConcatStr(ret, arg, len) => {
                    ctx.push(TIr::ConcatStr(ret, arg, len));
                    ctx.write_back_range(self, arg, len);
                    ctx.dealloc(ret);
                    self.xmm_save();
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
                        call rax;
                    );
                    self.xmm_restore();
                    if ret != 0 {
                        self.store_rax(ret);
                    }
                }
                BcOp1::MethodCall(ret, name) => {
                    ctx.push(TIr::MethodCall(ret, name));
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(*op);
                }
                BcOp1::MethodArgs(recv, args, len) => {
                    ctx.write_back(self, recv);
                    ctx.write_back_range(self, args, len);
                    ctx.push(TIr::MethodArgs(recv, args, len));

                    let op = std::mem::take(&mut self.opt_buf).unwrap();
                    match BcOp1::from_bc(op) {
                        BcOp1::MethodCall(ret, name) => {
                            ctx.dealloc(ret);
                            self.jit_method_call(recv, name, ret, args, len)
                        }
                        _ => unreachable!(),
                    }
                    skip = true;
                }
                BcOp1::MethodDef(id) => {
                    ctx.push(TIr::MethodDef(id));
                    let MethodDefInfo { name, func } = store[id];
                    let class_version = self.class_version;
                    monoasm!(self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func)); // FuncId
                        movq rax, (define_method);
                        call rax;
                        addl [rip + class_version], 1;
                    );
                }
                BcOp1::Br(disp) => {
                    ctx.all_write_back(self);
                    ctx.push(TIr::Br(disp));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    monoasm!(self.jit,
                        jmp dest;
                    );
                }
                BcOp1::CondBr(cond_, disp, false) => {
                    ctx.push(TIr::CondBr(cond_, disp, false));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let cont = self.jit.label();
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                        jeq cont;
                    );
                    ctx.branch_write_back(self);
                    monoasm!(self.jit,
                        jmp dest;
                    cont:
                    );
                }
                BcOp1::CondNotBr(cond_, disp, false) => {
                    ctx.push(TIr::CondNotBr(cond_, disp, false));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let cont = self.jit.label();
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                        jne cont;
                    );
                    ctx.branch_write_back(self);
                    monoasm!(self.jit,
                        jmp dest;
                    cont:
                    );
                }
                BcOp1::CondBr(cond_, disp, true) => {
                    ctx.push(TIr::CondBr(cond_, disp, true));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let generic = self.jit.label();
                    let op = std::mem::take(&mut self.opt_buf).unwrap();
                    let kind = match BcOp1::from_bc(op) {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => {
                            self.load_binary_args(lhs, rhs);
                            self.guard_rdi_rsi_fixnum(generic);
                            kind
                        }
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => {
                            monoasm!(self.jit,
                                movq rdi, [rbp - (conv(lhs))];
                                movq rsi, (Value::new_integer(rhs as i64).get());
                            );
                            self.guard_rdi_fixnum(generic);
                            kind
                        }
                        _ => unreachable!(),
                    };
                    self.gen_cmp_opt(kind, dest, generic, true, &mut ctx);
                }
                BcOp1::CondNotBr(cond_, disp, true) => {
                    ctx.push(TIr::CondBr(cond_, disp, true));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let generic = self.jit.label();
                    let op = std::mem::take(&mut self.opt_buf).unwrap();
                    let kind = match BcOp1::from_bc(op) {
                        BcOp1::Cmp(kind, _ret, lhs, rhs, true) => {
                            self.load_binary_args(lhs, rhs);
                            self.guard_rdi_rsi_fixnum(generic);
                            kind
                        }
                        BcOp1::Cmpri(kind, _ret, lhs, rhs, true) => {
                            monoasm!(self.jit,
                                movq rdi, [rbp - (conv(lhs))];
                                movq rsi, (Value::new_integer(rhs as i64).get());
                            );
                            self.guard_rdi_fixnum(generic);
                            kind
                        }
                        _ => unreachable!(),
                    };
                    self.gen_cmp_opt(kind, dest, generic, false, &mut ctx);
                }
            }
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let elapsed = now.elapsed();
        #[cfg(any(feature = "emit-asm"))]
        {
            let (start, code_end, end) = self.jit.code_block.last().unwrap();
            eprintln!(
                "offset:{:?} code: {} bytes  data: {} bytes",
                start,
                *code_end - *start,
                *end - *code_end
            );
            self.jit.select(0);
            eprintln!("{}", self.jit.dump_code().unwrap());
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("<-- finished compile. elapsed:{:?}", elapsed);

        entry
    }

    fn prologue(&mut self, regs: usize) {
        let offset = (regs + regs % 2) * 8 + 16;
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            movq rax, (regs - 1);
            subq rax, rdi;
            jeq  loop_exit;
            lea  rcx, [rsp - ((regs as i32) * 8 + 16)];
            //
            // rax: counter, regs - 1 - args_len
            // rcx: pointer, the next quad word of the last register.
            //       +-------------+
            //       | return addr |
            //       +-------------+
            //       |   old rbp   |
            //       +-------------+
            //       |    meta     |
            //       +-------------+
            //       |     %0      |
            //       +-------------+
            //       |     %1      |
            //       +-------------+
            //       |     %2      |
            //       +-------------+
            // rcx-> |             |
            //       +-------------+
        loop_:
            movq [rcx + rax * 8], (FALSE_VALUE);
            subq rax, 1;
            jne  loop_;
        loop_exit:
            subq rsp, (offset);
        );
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            leave;
            ret;
        );
    }

    fn load_binary_args(&mut self, lhs: u16, rhs: u16) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
    }

    fn gen_cmp_kind(&mut self, kind: CmpKind, generic: DestLabel, ret: u16) {
        match kind {
            CmpKind::Eq => self.cmp_eq(generic),
            CmpKind::Ne => self.cmp_ne(generic),
            CmpKind::Ge => self.cmp_ge(generic),
            CmpKind::Gt => self.cmp_gt(generic),
            CmpKind::Le => self.cmp_le(generic),
            CmpKind::Lt => self.cmp_lt(generic),
            _ => unimplemented!(),
        }
        self.store_rax(ret);
    }

    fn gen_cmp_opt(
        &mut self,
        kind: CmpKind,
        dest: DestLabel,
        generic: DestLabel,
        switch: bool,
        ctx: &mut FloatContext,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_eq(dest, generic, switch, ctx),
            CmpKind::Ne => self.cmp_opt_ne(dest, generic, switch, ctx),
            CmpKind::Ge => self.cmp_opt_ge(dest, generic, switch, ctx),
            CmpKind::Gt => self.cmp_opt_gt(dest, generic, switch, ctx),
            CmpKind::Le => self.cmp_opt_le(dest, generic, switch, ctx),
            CmpKind::Lt => self.cmp_opt_lt(dest, generic, switch, ctx),
            _ => unimplemented!(),
        }
    }

    fn gen_add(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, add_values as _);
    }

    fn gen_sub(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, sub_values as _);
    }

    fn gen_bit_or(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: u16) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _);
    }

    fn shift_under(&mut self, under: DestLabel, after: DestLabel) {
        self.jit.select(1);
        let zero = self.jit.label();
        monoasm!(self.jit,
        under:
            testq rdi, rdi;
            jns zero;
            xorq rdi, rdi;
            subq rdi, 1;
            jmp after;
        zero:
            xorq rdi, rdi;
            jmp after;
        );
        self.jit.select(0);
    }

    fn gen_shr(&mut self, generic: DestLabel, ret: u16) {
        let shl = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shl;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, shr_values as _);
        self.jit.select(1);
        monoasm!(self.jit,
        shl:
            negq rcx;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
            jmp after;
        );
        self.jit.select(0);
        self.shift_under(under, after);
    }

    fn gen_shl(&mut self, generic: DestLabel, ret: u16) {
        let shr = self.jit.label();
        let after = self.jit.label();
        let under = self.jit.label();
        monoasm!(self.jit,
            // fastpath
            movq rcx, rsi;
            sarq rcx, 1;
            js shr;
            lzcntq rax, rdi;
            cmpq rcx, rax;
            jgt generic;
            subq rdi, 1;
            salq rdi, rcx;
        after:
            orq rdi, 1;
        );
        self.store_rdi(ret);

        self.side_generic_op(generic, ret, shl_values as _);
        self.jit.select(1);
        monoasm!(self.jit,
        shr:
            negq rcx;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
            jmp after;
        );
        self.jit.select(0);
        self.shift_under(under, after);
    }

    fn side_generic_op(&mut self, generic: DestLabel, ret: u16, func: u64) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn generic_binop(&mut self, ret: u16, func: u64) {
        self.call_binop(func);
        self.store_rax(ret);
    }

    fn jit_method_call(&mut self, recv: u16, name: IdentId, ret: u16, args: u16, len: u16) {
        // set arguments to a callee stack.
        //
        //       +-------------+
        //  0x00 |             | <- rsp
        //       +-------------+
        // -0x08 | return addr |
        //       +-------------+
        // -0x10 |   old rbp   |
        //       +-------------+
        // -0x18 |    meta     |
        //       +-------------+
        // -0x20 |     %0      |
        //       +-------------+
        // -0x28 | %1(1st arg) |
        //       +-------------+
        //       |             |
        //
        // argument registers:
        //   rdi: args len
        //
        let exit = self.jit.label();
        let patch_meta = self.jit.label();
        let patch_adr = self.jit.label();
        let patch_pc = self.jit.label();
        let slow_path = self.jit.label();
        let cached_class_version = self.jit.const_i32(-1);
        let cached_recv_class = self.jit.const_i32(0);
        let global_class_version = self.class_version;
        let entry_find_method = self.entry_find_method;
        let entry_panic = self.entry_panic;
        let entry_return = self.vm_return;
        self.xmm_save();
        if recv != 0 {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(recv))];
                movq rax, (Value::get_class);
                call rax;
                movl r15, rax;  // r15: receiver class_id
                cmpl r15, [rip + cached_recv_class];
                jne slow_path;
            );
        }
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl [rip + cached_class_version], rax;
            jne slow_path;
        exit:
        );

        // set self
        monoasm!(self.jit,
            movq rax, [rbp - (conv(recv))];
            movq [rsp - 0x20], rax;
        );
        // set arguments
        for i in 0..len {
            let reg = args + i;
            monoasm!(self.jit,
                movq rax, [rbp - (conv(reg))];
                movq [rsp - ((0x28 + i * 8) as i64)], rax;
            );
        }

        monoasm!(self.jit,
            // set meta.
            movq rax, 0x8000_0000_0000_0000;
        patch_meta:
            movq [rsp - 0x18], rax;

            movq r13, 0x8000_0000_0000_0000;
        patch_pc:
            movq rdi, (len);
            // patch point
            call entry_panic;
        patch_adr:
        );
        self.xmm_restore();
        monoasm!(self.jit,
            testq rax, rax;
            jeq entry_return;
        );
        if ret != 0 {
            self.store_rax(ret);
        }

        self.jit.select(1);
        // call site stub code.
        monoasm!(self.jit,
        slow_path:
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (len as usize); // args_len: usize
            movq r8, [rbp - (conv(recv))]; // receiver: Value
            call entry_find_method;
            // absolute address was returned to rax.
            testq rax, rax;
            jeq entry_return;

            lea rdi, [rip + patch_meta];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_META)];
            movq [rdi], rcx;

            lea rdi, [rip + patch_pc];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_PC)];
            movq [rdi], rcx;

            movq rax, [rax + (FUNCDATA_OFFSET_CODEPTR)];
            lea rdi, [rip + patch_adr];
            // calculate a displacement to the function address.
            subq rax, rdi;
            // set patch point address (= return address - 4) to rdi.
            //subq rdi, 4;
            // apply patch.
            movl [rdi - 4], rax;

            movl rax, [rip + global_class_version];
            movl [rip + cached_class_version], rax;
        );
        if recv != 0 {
            monoasm!(self.jit,
                movl [rip + cached_recv_class], r15;
            );
        }
        monoasm!(self.jit,
            jmp exit;
        );
        self.jit.select(0);
    }
}
