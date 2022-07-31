use monoasm_macro::monoasm;

use super::*;

//
// Just-in-time compiler module.
//

///
/// Type Ir.
///
#[derive(PartialEq)]
enum TIr {
    /// branch(dest, write_back)
    Br(i32, Vec<(u16, SlotId)>),
    /// conditional branch(%reg, dest, optimizable, write_back)  : branch when reg was true.
    CondBr(SlotId, i32, bool, Vec<(u16, SlotId)>, BrKind),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, literal_id)
    Literal(SlotId, u32),
    /// float_literal(Fret, f64)
    FLiteral(u16, f64),
    LoadConst(SlotId, ConstSiteId),
    StoreConst(SlotId, IdentId),
    /// nil(%reg)
    Nil(SlotId),
    /// negate(%ret, %src)
    Neg(SlotId, SlotId),
    /// binop(kind, %ret, %lhs, %rhs)
    BinOp(BinOpK, SlotId, SlotId, SlotId, ClassId),
    /// binop with small integer(kind, %ret, %lhs, rhs)
    BinOpRi(BinOpK, SlotId, SlotId, i16, ClassId),
    /// binop with small integer(kind, %ret, lhs, %rhs)
    BinOpIr(BinOpK, SlotId, i16, SlotId, ClassId),
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(CmpKind, SlotId, SlotId, SlotId, bool, ClassId),
    /// cmpri(%ret, %lhs, rhs: i16, optimizable)
    Cmpri(CmpKind, SlotId, SlotId, i16, bool, ClassId),
    /// return(%ret)
    Ret(SlotId),
    /// move(%dst, %src)
    Mov(SlotId, SlotId),
    /// func call(%ret, name)
    MethodCall(SlotId, IdentId),
    /// func call 2nd opecode(%recv, %args, len)
    MethodArgs(SlotId, SlotId, u16),
    /// method definition(method_def_id)
    MethodDef(MethodDefId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(SlotId, SlotId, u16),
    /// float_binop(kind, Fret, Flhs, Frhs)
    FBinOp(BinOpK, u16, u16, u16),
    /// float_binop(kind, Fret, Flhs, f64)
    FBinOpRf(BinOpK, u16, u16, f64),
    /// float_binop(kind, Fret, f64, Frhs)
    FBinOpFr(BinOpK, u16, f64, u16),
    /// float_cmp(%ret, Flhs, Frhs, optimizable)
    FCmp(CmpKind, SlotId, u16, u16, bool),
    /// float_cmpri(%ret, Flhs, rhs: f16, optimizable)
    FCmpRf(CmpKind, SlotId, u16, f64, bool),
    /// negate(Fret, Fsrc)
    FNeg(u16, u16),
    /// load float from register(%src, Fdst)
    FLoad(SlotId, u16),
    /// store float to register(Fsrc, %dst)
    FStore(u16, SlotId),
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
        fn disp_write_back(v: &[(u16, SlotId)]) -> String {
            v.iter()
                .map(|(fsrc, dst)| format!("%{}:=F{} ", dst, fsrc))
                .fold(String::new(), |acc, s| acc + &s)
        }
        match self {
            TIr::Br(disp, write_back) => {
                writeln!(
                    f,
                    "br => {}; {}",
                    disp_str(disp),
                    disp_write_back(write_back)
                )
            }
            TIr::CondBr(reg, disp, opt, write_back, kind) => {
                writeln!(
                    f,
                    "cond{}br {}%{} => {}; {}",
                    kind.to_s(),
                    optstr(opt),
                    reg,
                    disp_str(disp),
                    disp_write_back(write_back)
                )
            }
            TIr::Integer(reg, num) => writeln!(f, "%{} = {}: i32", reg, num),
            TIr::Symbol(reg, id) => {
                writeln!(f, "%{} = {:?}", reg, id)
            }
            TIr::Literal(reg, id) => {
                writeln!(f, "%{} = literal[#{}]", reg, id)
            }
            TIr::FLiteral(freg, float) => {
                writeln!(f, "F{} = {}: f64", freg, float)
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
            TIr::BinOpIr(kind, dst, lhs, rhs, class) => {
                writeln!(f, "%{} = {}:i16 {} %{}: {:?}", dst, lhs, kind, rhs, class)
            }
            TIr::FBinOp(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = F{} {} F{}", dst, lhs, kind, rhs)
            }
            TIr::FBinOpRf(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = F{} {} {}: f64", dst, lhs, kind, rhs)
            }
            TIr::FBinOpFr(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = {}: f64 {} F{}", dst, lhs, kind, rhs)
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
                writeln!(f, "{} = call {:?}", ret.ret_str(), name,)
            }
            TIr::MethodArgs(recv, args, len) => {
                writeln!(f, "%{}.call_args (%{}; {})", recv, args, len)
            }
            TIr::MethodDef(id) => {
                writeln!(f, "define {:?}", id)
            }
            TIr::ConcatStr(ret, args, len) => {
                writeln!(f, "{} = concat(%{}; {})", ret.ret_str(), args, len)
            }
        }
    }
}

///
/// Mode if linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum LinkMode {
    ///
    /// Linked to an xmm register and we can read and write.
    ///
    /// mutation of the corresponding xmm register (lazily) affects the stack slot.
    ///
    XmmRW(u16),
    /// Linked to an xmm register but we can only read.
    XmmR(u16),
    /// No linkage with any xmm regiter.
    None,
}

impl LinkMode {
    fn is_none(&self) -> bool {
        !matches!(self, LinkMode::XmmRW(_))
    }
}

///
/// Context of the current Basic block.
///
#[derive(Debug)]
pub(crate) struct BBContext {
    /// information for stack slots.
    stack_slot: StackSlotInfo,
    /// information for xmm registers.
    xmm: [Vec<SlotId>; 14],
    /// typed IR.
    tir: Vec<TIr>,
}

#[derive(Debug)]
struct StackSlotInfo(Vec<LinkMode>);

impl std::ops::Index<SlotId> for StackSlotInfo {
    type Output = LinkMode;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.0[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for StackSlotInfo {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.0[i.0 as usize]
    }
}

impl StackSlotInfo {
    fn clear(&mut self) {
        self.0.iter_mut().for_each(|info| *info = LinkMode::None);
    }
}

impl BBContext {
    fn new(reg_num: usize) -> Self {
        let xmm = (0..14)
            .map(|_| vec![])
            .collect::<Vec<Vec<SlotId>>>()
            .try_into()
            .unwrap();
        Self {
            stack_slot: StackSlotInfo(vec![LinkMode::None; reg_num]),
            xmm,
            tir: vec![],
        }
    }

    fn push(&mut self, tir: TIr) {
        self.tir.push(tir);
    }

    ///
    /// Allocate new xmm register to the given stack slot for read/write f64.
    ///
    fn alloc_xmm(&mut self, reg: SlotId) -> u16 {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            if self.xmm[freg as usize].len() == 1 {
                assert_eq!(reg, self.xmm[freg as usize][0]);
                return freg;
            }
        };
        self.dealloc_xmm(reg);
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                self.stack_slot[reg] = LinkMode::XmmRW(flhs as u16);
                xmm.push(reg);
                return flhs as u16;
            }
        }
        unreachable!()
    }

    ///
    /// Allocate new xmm register to the given stack slot for read f64.
    ///
    fn alloc_read_xmm(&mut self, reg: SlotId) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                if self.xmm[freg as usize].len() == 1 {
                    assert_eq!(reg, self.xmm[freg as usize][0]);
                    return freg;
                }
            }
            _ => {}
        };
        self.dealloc_xmm(reg);
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                self.stack_slot[reg] = LinkMode::XmmR(flhs as u16);
                xmm.push(reg);
                return flhs as u16;
            }
        }
        unreachable!()
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    fn dealloc_xmm(&mut self, reg: SlotId) {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => {
                assert!(self.xmm[freg as usize].contains(&reg));
                self.xmm[freg as usize].retain(|e| *e != reg);
                self.stack_slot[reg] = LinkMode::None;
            }
            _ => {}
        }
    }

    fn copy(&mut self, src: SlotId, dst: SlotId) -> Option<u16> {
        self.dealloc_xmm(dst);
        let f = self.stack_slot[src];
        match f {
            LinkMode::XmmRW(freg) => {
                self.stack_slot[dst] = LinkMode::XmmRW(freg);
                self.xmm[freg as usize].push(dst);
                Some(freg)
            }
            LinkMode::XmmR(freg) => {
                self.stack_slot[dst] = LinkMode::XmmR(freg);
                self.xmm[freg as usize].push(dst);
                Some(freg)
            }
            _ => None,
        }
    }

    fn clear_write_back(&mut self) {
        self.get_write_back()
            .iter()
            .for_each(|(_, reg)| self.dealloc_xmm(*reg));
    }

    fn write_back(&mut self, codegen: &mut Codegen, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            self.push(TIr::FStore(freg, reg));
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
                movq [rbp - (conv(reg))], rax;
            );
            self.dealloc_xmm(reg);
        }
    }

    fn write_back_no_dealloc(&mut self, codegen: &mut Codegen, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
                movq [rbp - (conv(reg))], rax;
            );
        }
    }

    fn write_back_range(&mut self, codegen: &mut Codegen, arg: SlotId, len: u16) {
        for reg in arg.0..arg.0 + len {
            self.write_back(codegen, SlotId::new(reg))
        }
    }

    ///
    /// Get *DestLabel* for a branch to *dest*.
    ///
    fn get_branch_dest(&mut self, codegen: &mut Codegen, dest: DestLabel) -> DestLabel {
        codegen.jit.select(1);
        let entry = codegen.jit.label();
        codegen.jit.bind_label(entry);
        self.get_write_back()
            .iter()
            .for_each(|(_, reg)| self.write_back_no_dealloc(codegen, *reg));
        monoasm!(codegen.jit,
            jmp  dest;
        );
        codegen.jit.select(0);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    fn get_fallback_dest(&mut self, codegen: &mut Codegen, pc: &Bc) -> DestLabel {
        codegen.jit.select(1);
        let entry = codegen.jit.label();
        codegen.jit.bind_label(entry);
        self.get_write_back()
            .iter()
            .for_each(|(_, reg)| self.write_back_no_dealloc(codegen, *reg));
        let fetch = codegen.vm_fetch;
        let pc = BcPc::from(pc);
        monoasm!(codegen.jit,
            movq r13, (pc.0);
            jmp fetch;
        );
        codegen.jit.select(0);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn fallback(&mut self, codegen: &mut Codegen, pc: &Bc) {
        let fallback = self.get_fallback_dest(codegen, pc);
        monoasm!(codegen.jit,
            jmp fallback;
        );
    }

    fn all_write_back(&mut self, codegen: &mut Codegen) {
        self.get_write_back()
            .iter()
            .for_each(|(_, reg)| self.write_back(codegen, *reg));

        self.xmm.iter_mut().for_each(|info| *info = vec![]);
        assert!(self.stack_slot.0.iter().all(|r| r.is_none()));
        self.stack_slot.clear()
    }

    fn get_write_back(&self) -> Vec<(u16, SlotId)> {
        self.stack_slot
            .0
            .iter()
            .enumerate()
            .filter_map(|(reg, freg)| {
                if let LinkMode::XmmRW(freg) = freg {
                    Some((*freg as u16, SlotId::new(reg as u16)))
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_xmm_using(&self) -> Vec<usize> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i) })
            .collect()
    }

    fn get_float(&mut self, codegen: &mut Codegen, reg: SlotId, op: &Bc) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = self.alloc_read_xmm(reg);
                self.push(TIr::FLoad(reg, freg as u16));
                let val_to_f64 = codegen.val_to_f64;
                let fallback = self.get_fallback_dest(codegen, op);
                monoasm!(codegen.jit,
                    movq rdi, [rbp - (conv(reg))];
                    call val_to_f64;
                    testq rax, rax;
                    jz   fallback;
                    movq xmm(freg as u64 + 2), xmm0;
                );
                freg
            }
        }
    }

    fn get_binary_float(
        &mut self,
        codegen: &mut Codegen,
        lhs: SlotId,
        rhs: SlotId,
        op: &Bc,
    ) -> (u16, u16) {
        match (self.stack_slot[lhs], self.stack_slot[rhs]) {
            (LinkMode::XmmR(lhs) | LinkMode::XmmRW(lhs), xrhs) => match xrhs {
                LinkMode::None => (lhs, self.get_float(codegen, rhs, op)),
                LinkMode::XmmR(rhs) | LinkMode::XmmRW(rhs) => (lhs, rhs),
            },
            (LinkMode::None, xrhs) => match xrhs {
                LinkMode::XmmR(rhs) | LinkMode::XmmRW(rhs) => {
                    (self.get_float(codegen, lhs, op), rhs)
                }
                LinkMode::None => {
                    let cont = codegen.jit.label();
                    let fallback = self.get_fallback_dest(codegen, op);
                    // if both of lhs and rhs were not Flonum, deoptimize.
                    monoasm!(codegen.jit,
                        movq  rax, [rbp - (conv(lhs))];
                        andq  rax, 3;
                        cmpq  rax, 2;
                        jeq   cont;
                        movq  rax, [rbp - (conv(rhs))];
                        andq  rax, 3;
                        cmpq  rax, 2;
                        jne   fallback;
                    cont:
                    );
                    (
                        self.get_float(codegen, lhs, op),
                        self.get_float(codegen, rhs, op),
                    )
                }
            },
        }
    }
}

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            pub(crate) fn [<cmp_ $op>](&mut self, generic:DestLabel, ctx: Option<&BBContext>) {
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
                if let Some(ctx) = ctx {
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        // generic path
                        movq rax, ([<cmp_ $op _values>]);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                } else {
                    monoasm!(self.jit,
                        // generic path
                        movq rax, ([<cmp_ $op _values>]);
                        call rax;
                    );
                }
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
    (($op:ident, $sop:ident)) => {
        paste! {
            fn [<cmp_opt_ $sop>](&mut self, dest: DestLabel, generic:DestLabel, switch:bool, ctx: &mut BBContext) {
                let cont = self.jit.label();
                let side_exit = ctx.get_branch_dest(self, dest);
                if switch {
                    monoasm! { self.jit,
                        [<j $op>] side_exit;
                    };

                } else {
                    monoasm! { self.jit,
                        [<j $op>] cont;
                        jmp side_exit;
                    };
                }
                self.jit.bind_label(cont);
                self.jit.select(1);
                monoasm!(self.jit,
                    generic:
                );
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    // generic path
                    movq rax, ([<cmp_ $sop _values>]);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    orq  rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                    // if true, Z=0(not set).
                );
                if switch {
                    monoasm!(self.jit,
                        jz  cont;
                        jmp side_exit;
                    );
                } else {
                    monoasm!(self.jit,
                        jnz  cont;
                        jmp side_exit;
                    );
                }
                self.jit.select(0);
            }
        }
    };
    (($op1:ident, $sop1:ident), $(($op2:ident, $sop2:ident)),+) => {
        cmp_opt_main!(($op1, $sop1));
        cmp_opt_main!($(($op2, $sop2)),+);
    };
}

impl Codegen {
    cmp_opt_main!((eq, eq), (ne, ne), (a, gt), (b, lt), (ae, ge), (be, le));
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
        let mut ctx = BBContext::new(func.total_reg_num());
        for (idx, op) in func.bytecode()[start_pos..].iter().enumerate() {
            if skip {
                skip = false;
                continue;
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
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!(
                            "    return pc:[{}] {:?}",
                            start_pos + idx + 1,
                            op as *const _
                        );

                        ctx.fallback(self, op);
                        break;
                    }
                }
                BcOp1::Integer(ret, i) => {
                    ctx.push(TIr::Integer(ret, i));
                    ctx.dealloc_xmm(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp1::Symbol(ret, id) => {
                    ctx.push(TIr::Symbol(ret, id));
                    ctx.dealloc_xmm(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                BcOp1::Literal(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    let v = store.get_literal(id);
                    if let RV::Float(f) = v.unpack() {
                        let fdst = ctx.alloc_xmm(dst);
                        ctx.push(TIr::FLiteral(fdst, f));
                        let imm = self.jit.const_f64(f);
                        monoasm!(self.jit,
                            movq xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.push(TIr::Literal(dst, id));
                        if v.is_packed_value() {
                            monoasm!(self.jit,
                              movq rax, (v.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            monoasm!(self.jit,
                              movq rdi, (v.get());
                              movq rax, (Value::dup);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(dst);
                    }
                }
                BcOp1::LoadConst(dst, id) => {
                    ctx.push(TIr::LoadConst(dst, id));
                    ctx.dealloc_xmm(dst);
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
                    self.store_rax(dst);
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
                BcOp1::StoreConst(src, id) => {
                    ctx.push(TIr::StoreConst(src, id));
                    ctx.write_back(self, src);
                    let const_version = self.const_version;
                    monoasm!(self.jit,
                      movq rdx, (id.get());  // name: IdentId
                      movq rcx, [rbp - (conv(src))];  // val: Value
                      movq rdi, rbx;  // &mut Interp
                      movq rsi, r12;  // &mut Globals
                      addq [rip + const_version], 1;
                      movq rax, (set_constant);
                      call rax;
                    );
                }
                BcOp1::Nil(ret) => {
                    ctx.push(TIr::Nil(ret));
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp1::Neg(dst, src) => {
                    if op.is_float() {
                        let fsrc = ctx.get_float(self, src, op);
                        let fdst = ctx.alloc_xmm(dst);
                        ctx.push(TIr::FNeg(fdst, fsrc));
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.write_back(self, src);
                        ctx.dealloc_xmm(dst);
                        ctx.push(TIr::Neg(dst, src));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.store_rax(dst);
                    }
                }
                BcOp1::BinOp(kind, ret, lhs, rhs) => {
                    if op.is_binary_float() {
                        let (flhs, frhs) = ctx.get_binary_float(self, lhs, rhs, op);
                        let fret = ctx.alloc_xmm(ret);
                        ctx.push(TIr::FBinOp(kind, fret as u16, flhs as u16, frhs as u16));

                        if fret == frhs {
                            let lhs = flhs as u64 + 2;
                            let ret = fret as u64 + 2;
                            match kind {
                                BinOpK::Add => monoasm!(self.jit,
                                    addsd xmm(ret), xmm(lhs);
                                ),
                                BinOpK::Sub => monoasm!(self.jit,
                                    movq  xmm0, xmm(lhs);
                                    subsd xmm0, xmm(ret);
                                    movq  xmm(ret), xmm0;
                                ),
                                BinOpK::Mul => monoasm!(self.jit,
                                    mulsd xmm(ret), xmm(lhs);
                                ),
                                BinOpK::Div => {
                                    let div_by_zero = self.div_by_zero;
                                    monoasm!(self.jit,
                                        movq  rax, xmm(ret);
                                        cmpq  rax, 0;
                                        jeq   div_by_zero;
                                        movq  xmm0, xmm(lhs);
                                        divsd xmm0, xmm(ret);
                                        movq  xmm(ret), xmm0;
                                    )
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            let rhs = frhs as u64 + 2;
                            let ret = fret as u64 + 2;
                            self.xmm_mov(flhs, fret);
                            match kind {
                                BinOpK::Add => monoasm!(self.jit,
                                    addsd xmm(ret), xmm(rhs);
                                ),
                                BinOpK::Sub => monoasm!(self.jit,
                                    subsd xmm(ret), xmm(rhs);
                                ),
                                BinOpK::Mul => monoasm!(self.jit,
                                    mulsd xmm(ret), xmm(rhs);
                                ),
                                BinOpK::Div => {
                                    let div_by_zero = self.div_by_zero;
                                    monoasm!(self.jit,
                                        movq  rax, xmm(frhs as u64 + 2);
                                        cmpq  rax, 0;
                                        jeq   div_by_zero;
                                        divsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                    )
                                }
                                _ => unimplemented!(),
                            }
                        }
                    } else {
                        ctx.write_back(self, lhs);
                        ctx.write_back(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOp(kind, ret, lhs, rhs, op.classid()));
                        self.load_binary_args(lhs, rhs);
                        self.gen_binop_kind(&ctx, op, kind, ret);
                    }
                }

                BcOp1::BinOpRi(kind, ret, lhs, rhs) => {
                    if op.is_binary_float() {
                        let flhs = ctx.get_float(self, lhs, op);
                        let fret = ctx.alloc_xmm(ret);
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
                                if rhs == 0 {
                                    let div_by_zero = self.div_by_zero;
                                    monoasm!(self.jit,
                                        jmp   div_by_zero;
                                    )
                                } else {
                                    monoasm!(self.jit,
                                        divsd xmm(fret as u64 + 2), [rip + imm];
                                    )
                                }
                            }
                            _ => unimplemented!(),
                        }
                    } else {
                        ctx.write_back(self, lhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpRi(kind, ret, lhs, rhs, op.classid()));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_binop_kind(&ctx, op, kind, ret);
                    }
                }

                BcOp1::BinOpIr(kind, ret, lhs, rhs) => {
                    if op.is_binary_float() {
                        let frhs = ctx.get_float(self, rhs, op);
                        let fret = ctx.alloc_xmm(ret);
                        ctx.push(TIr::FBinOpFr(kind, fret as u16, lhs as f64, frhs as u16));
                        let imm0 = self.jit.const_f64(lhs as f64);
                        if fret != frhs {
                            monoasm!(self.jit,
                                movq xmm(fret as u64 + 2), [rip + imm0];
                            );
                            match kind {
                                BinOpK::Add => monoasm!(self.jit,
                                    addsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                ),
                                BinOpK::Sub => monoasm!(self.jit,
                                    subsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                ),
                                BinOpK::Mul => monoasm!(self.jit,
                                    mulsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                ),
                                BinOpK::Div => {
                                    let div_by_zero = self.div_by_zero;
                                    monoasm!(self.jit,
                                        movq  rax, xmm(frhs as u64 + 2);
                                        cmpq  rax, 0;
                                        jeq   div_by_zero;
                                        divsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                    )
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            match kind {
                                BinOpK::Add => monoasm!(self.jit,
                                    addsd xmm(fret as u64 + 2), [rip + imm0];
                                ),
                                BinOpK::Sub => monoasm!(self.jit,
                                    movq  xmm0, xmm(frhs as u64 + 2);
                                    movq  xmm(fret as u64 + 2), [rip + imm0];
                                    subsd xmm(fret as u64 + 2), xmm0;
                                ),
                                BinOpK::Mul => monoasm!(self.jit,
                                    mulsd xmm(fret as u64 + 2), [rip + imm0];
                                ),
                                BinOpK::Div => {
                                    let div_by_zero = self.div_by_zero;
                                    monoasm!(self.jit,
                                        movq  rax, xmm(frhs as u64 + 2);
                                        cmpq  rax, 0;
                                        jeq   div_by_zero;
                                        movq  xmm(fret as u64 + 2), [rip + imm0];
                                        movq  xmm0, rax;
                                        divsd xmm(fret as u64 + 2), xmm0;
                                    );
                                }
                                _ => unimplemented!(),
                            }
                        }
                    } else {
                        ctx.write_back(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpIr(kind, ret, lhs, rhs, op.classid()));
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_binop_kind(&ctx, op, kind, ret);
                    }
                }

                BcOp1::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(*op);
                    } else {
                        if op.is_binary_float() {
                            let (flhs, frhs) = ctx.get_binary_float(self, lhs, rhs, op);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                            ctx.push(TIr::FCmp(kind, ret, flhs as u16, frhs as u16, false));
                        } else {
                            let generic = self.jit.label();
                            self.gen_cmp_prep(&mut ctx, ret, lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                            ctx.push(TIr::Cmp(kind, ret, lhs, rhs, optimizable, op.classid()));
                        }
                    }
                }
                BcOp1::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(*op);
                    } else {
                        if op.is_float() {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = ctx.get_float(self, lhs, op);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                            ctx.push(TIr::FCmpRf(kind, ret, flhs as u16, rhs as f64, false));
                        } else {
                            let generic = self.jit.label();
                            self.gen_cmpri_prep(&mut ctx, ret, lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                            ctx.push(TIr::Cmpri(kind, ret, lhs, rhs, optimizable, op.classid()));
                        }
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
                    ctx.write_back(self, lhs);
                    ctx.push(TIr::Ret(lhs));
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    ctx.clear_write_back();
                    continue;
                }
                BcOp1::ConcatStr(ret, arg, len) => {
                    ctx.push(TIr::ConcatStr(ret, arg, len));
                    ctx.write_back_range(self, arg, len);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                    if ret.0 != 0 {
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
                            ctx.dealloc_xmm(ret);
                            self.jit_method_call(recv, name, ret, args, len, &ctx);
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
                    let v = ctx.get_write_back();
                    ctx.push(TIr::Br(disp, v.clone()));
                    ctx.all_write_back(self);
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    monoasm!(self.jit,
                        jmp dest;
                    );
                    continue;
                }
                BcOp1::CondBr(cond_, disp, false, kind) => {
                    ctx.push(TIr::CondBr(cond_, disp, false, ctx.get_write_back(), kind));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let side_exit = ctx.get_branch_dest(self, dest);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!(self.jit, jne side_exit;),
                        BrKind::BrIfNot => monoasm!(self.jit, jeq side_exit;),
                    }
                }
                BcOp1::CondBr(cond_, disp, true, brkind) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let generic = self.jit.label();
                    let opcode = std::mem::take(&mut self.opt_buf).unwrap();
                    if opcode.is_binary_float() {
                        let kind = match BcOp1::from_bc(opcode) {
                            BcOp1::Cmp(kind, ret, lhs, rhs, true) => {
                                let flhs = ctx.get_float(self, lhs, op);
                                let frhs = ctx.get_float(self, rhs, op);
                                monoasm! { self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                ctx.push(TIr::FCmp(kind, ret, flhs, frhs, true));
                                kind
                            }
                            BcOp1::Cmpri(kind, ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = ctx.get_float(self, lhs, op);
                                monoasm! { self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                                };
                                ctx.push(TIr::FCmpRf(kind, ret, flhs, rhs as f64, true));
                                kind
                            }
                            _ => unreachable!(),
                        };
                        ctx.push(TIr::CondBr(cond_, disp, true, ctx.get_write_back(), brkind));
                        self.gen_cmp_opt(kind, dest, generic, brkind == BrKind::BrIf, &mut ctx);
                    } else {
                        let kind = match BcOp1::from_bc(opcode) {
                            BcOp1::Cmp(kind, ret, lhs, rhs, true) => {
                                self.gen_cmp_prep(&mut ctx, ret, lhs, rhs, generic);
                                ctx.push(TIr::Cmp(kind, ret, lhs, rhs, true, opcode.classid()));
                                kind
                            }
                            BcOp1::Cmpri(kind, ret, lhs, rhs, true) => {
                                self.gen_cmpri_prep(&mut ctx, ret, lhs, rhs, generic);
                                ctx.push(TIr::Cmpri(kind, ret, lhs, rhs, true, opcode.classid()));
                                kind
                            }
                            _ => unreachable!(),
                        };
                        ctx.push(TIr::CondBr(cond_, disp, true, ctx.get_write_back(), brkind));
                        monoasm! { self.jit,
                            cmpq rdi, rsi;
                        };
                        self.gen_cmp_opt(kind, dest, generic, brkind == BrKind::BrIf, &mut ctx);
                    }
                }
            }

            if let Some(Some(_)) = info.get(start_pos + idx + 1) {
                ctx.all_write_back(self);
            }
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let elapsed = now.elapsed();
        #[cfg(any(feature = "emit-asm"))]
        {
            eprintln!("{:?}", ctx.tir);
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

    fn load_binary_args(&mut self, lhs: SlotId, rhs: SlotId) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
    }

    fn xmm_save(&mut self, xmm_using: &Vec<usize>) {
        let len = xmm_using.len();
        let sp_offset = (len + len % 2) * 8;
        monoasm!(self.jit,
            subq rsp, (sp_offset);
        );
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
                movq [rsp + (8 * i)], xmm(*freg as u64 + 2);
            );
        }
    }

    fn xmm_restore(&mut self, xmm_using: &Vec<usize>) {
        let len = xmm_using.len();
        let sp_offset = (len + len % 2) * 8;
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
                movq xmm(*freg as u64 + 2), [rsp + (8 * i)];
            );
        }
        monoasm!(self.jit,
            addq rsp, (sp_offset);
        );
    }

    fn gen_binop_kind(&mut self, ctx: &BBContext, op: &Bc, kind: BinOpK, ret: SlotId) {
        match kind {
            BinOpK::Add => match op.classid() {
                INTEGER_CLASS => {
                    let generic = self.jit.label();
                    self.guard_rdi_rsi_fixnum(generic);
                    self.gen_add(generic, ret, &ctx);
                }
                _ => self.generic_binop(ret, add_values as _, &ctx),
            },
            BinOpK::Sub => match op.classid() {
                INTEGER_CLASS => {
                    let generic = self.jit.label();
                    self.guard_rdi_rsi_fixnum(generic);
                    self.gen_sub(generic, ret, &ctx);
                }
                _ => self.generic_binop(ret, sub_values as _, &ctx),
            },
            BinOpK::Mul => {
                self.generic_binop(ret, mul_values as _, &ctx);
            }
            BinOpK::Div => {
                self.generic_binop(ret, div_values as _, &ctx);
            }
            _ => {
                let generic = self.jit.label();
                self.guard_rdi_rsi_fixnum(generic);
                match kind {
                    BinOpK::BitOr => self.gen_bit_or(generic, ret, &ctx),
                    BinOpK::BitAnd => self.gen_bit_and(generic, ret, &ctx),
                    BinOpK::BitXor => self.gen_bit_xor(generic, ret, &ctx),
                    BinOpK::Shr => self.gen_shr(generic, ret, &ctx),
                    BinOpK::Shl => self.gen_shl(generic, ret, &ctx),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn setflag_float(&mut self, kind: CmpKind) {
        match kind {
            CmpKind::Eq => monoasm! { self.jit, seteq rax; },
            CmpKind::Ne => monoasm! { self.jit, setne rax; },
            CmpKind::Ge => monoasm! { self.jit, setae rax; },
            CmpKind::Gt => monoasm! { self.jit, seta rax; },
            CmpKind::Le => monoasm! { self.jit, setbe rax; },
            CmpKind::Lt => monoasm! { self.jit, setb rax; },
            _ => unimplemented!(),
        }
        monoasm! { self.jit,
            shlq rax, 3;
            orq rax, (FALSE_VALUE);
        };
    }

    fn gen_cmp_prep(
        &mut self,
        ctx: &mut BBContext,
        ret: SlotId,
        lhs: SlotId,
        rhs: SlotId,
        generic: DestLabel,
    ) {
        ctx.write_back(self, lhs);
        ctx.write_back(self, rhs);
        ctx.dealloc_xmm(ret);
        self.load_binary_args(lhs, rhs);
        self.guard_rdi_rsi_fixnum(generic);
    }

    fn gen_cmpri_prep(
        &mut self,
        ctx: &mut BBContext,
        ret: SlotId,
        lhs: SlotId,
        rhs: i16,
        generic: DestLabel,
    ) {
        ctx.write_back(self, lhs);
        ctx.dealloc_xmm(ret);
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, (Value::new_integer(rhs as i64).get());
        );
        self.guard_rdi_fixnum(generic);
    }

    fn gen_cmp_kind(&mut self, kind: CmpKind, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        match kind {
            CmpKind::Eq => self.cmp_eq(generic, Some(ctx)),
            CmpKind::Ne => self.cmp_ne(generic, Some(ctx)),
            CmpKind::Ge => self.cmp_ge(generic, Some(ctx)),
            CmpKind::Gt => self.cmp_gt(generic, Some(ctx)),
            CmpKind::Le => self.cmp_le(generic, Some(ctx)),
            CmpKind::Lt => self.cmp_lt(generic, Some(ctx)),
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
        ctx: &mut BBContext,
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

    fn gen_add(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, 1;
            addq rax, rsi;
            jo generic;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, add_values as _, &ctx);
    }

    fn gen_sub(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        monoasm!(self.jit,
            // fastpath
            movq rax, rdi;
            subq rax, rsi;
            jo generic;
            addq rax, 1;
        );
        self.store_rax(ret);
        self.side_generic_op(generic, ret, sub_values as _, &ctx);
    }

    fn gen_bit_or(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _, &ctx);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _, &ctx);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _, &ctx);
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

    fn gen_shr(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
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
        self.side_generic_op(generic, ret, shr_values as _, &ctx);
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

    fn gen_shl(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
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

        self.side_generic_op(generic, ret, shl_values as _, &ctx);
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

    fn side_generic_op(&mut self, generic: DestLabel, ret: SlotId, func: u64, ctx: &BBContext) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func, ctx);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn generic_binop(&mut self, ret: SlotId, func: u64, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        self.call_binop(func);
        self.xmm_restore(&xmm_using);
        self.store_rax(ret);
    }

    fn jit_method_call(
        &mut self,
        recv: SlotId,
        name: IdentId,
        ret: SlotId,
        args: SlotId,
        len: u16,
        ctx: &BBContext,
    ) {
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
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        if !recv.is_zero() {
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
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            testq rax, rax;
            jeq entry_return;
        );
        if !ret.is_zero() {
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
        if !recv.is_zero() {
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
