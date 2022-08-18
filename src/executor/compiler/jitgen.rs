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
    Br(i32, Vec<u16>),
    /// conditional branch(%reg, dest, optimizable, write_back)  : branch when reg was true.
    CondBr(SlotId, i32, bool, Vec<u16>, BrKind),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, literal_id)
    Literal(SlotId, u32),
    /// array(%ret, %src, len)
    Array(SlotId, SlotId, u16),
    /// index(%ret, %base, %idx)
    Index(SlotId, SlotId, SlotId),
    /// vm_index_assign(%src, %base, %idx)
    IndexAssign(SlotId, SlotId, SlotId),
    /// float_literal(Fret, f64)
    FLiteral(u16, f64),
    LoadConst(SlotId, ConstSiteId),
    FLoadConst(u16, SlotId, ConstSiteId),
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
    /// float_move(Fdst, Fsrc)
    FMov(SlotId, u16),
    /// func call(%ret, name, %recv, %args, len)
    MethodCall(SlotId, IdentId, SlotId, SlotId, u16),
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
        fn disp_write_back(v: &[u16]) -> String {
            v.iter()
                .map(|freg| format!("F{} ", freg))
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
            TIr::Array(ret, src, len) => {
                writeln!(f, "%{} = [%{}; {}]", ret, src, len)
            }
            TIr::Index(ret, base, idx) => {
                writeln!(f, "%{} = %{}.[%{}]", ret, base, idx)
            }
            TIr::IndexAssign(src, base, idx) => {
                writeln!(f, "%{}.[%{}] = %{}", base, idx, src)
            }
            TIr::FLiteral(freg, float) => {
                writeln!(f, "F{} = {}: f64", freg, float)
            }
            TIr::LoadConst(reg, id) => {
                writeln!(f, "%{} = const[{:?}]", reg, id)
            }
            TIr::FLoadConst(freg, reg, id) => {
                writeln!(f, "F{} %{} = const[{:?}]", freg, reg, id)
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
            TIr::FMov(dst, src) => writeln!(f, "%{} = F{}", dst, src),
            TIr::MethodCall(ret, name, recv, args, len) => {
                writeln!(
                    f,
                    "{} = %{}.call {:?}(%{}; {})",
                    ret.ret_str(),
                    recv,
                    name,
                    args,
                    len
                )
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
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum LinkMode {
    ///
    /// Linked to an xmm register and we can read and write.
    ///
    /// mutation of the corresponding xmm register (lazily) affects the stack slot.
    ///
    XmmRW(u16),
    ///
    /// Linked to an xmm register but we can only read.
    ///
    XmmR(u16),
    ///
    /// No linkage with any xmm regiter.
    ///
    None,
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
    fn xmm_write(&mut self, reg: SlotId) -> u16 {
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
    fn xmm_read(&mut self, reg: SlotId) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => return freg,
            LinkMode::None => {
                for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
                    if xmm.is_empty() {
                        self.stack_slot[reg] = LinkMode::XmmR(flhs as u16);
                        xmm.push(reg);
                        return flhs as u16;
                    }
                }
            }
        };
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

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, codegen: &mut Codegen, src: SlotId, dst: SlotId) {
        self.dealloc_xmm(dst);
        match self.stack_slot[src] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                self.stack_slot[dst] = LinkMode::XmmRW(freg);
                self.xmm[freg as usize].push(dst);
                self.push(TIr::FMov(dst, freg));
            }
            _ => {
                self.push(TIr::Mov(dst, src));
                monoasm!(codegen.jit,
                  movq rax, [rbp - (conv(src))];
                  movq [rbp - (conv(dst))], rax;
                );
            }
        }
    }

    fn clear_write_back(&mut self, wb: Vec<u16>) {
        wb.iter().for_each(|freg| {
            for reg in self.xmm[*freg as usize].clone() {
                self.dealloc_xmm(reg);
            }
        });
    }

    ///
    /// Write back a corresponding xmm register to the stack slot *reg*.
    ///
    /// the xmm is deallocated.
    ///
    fn read_slot(&mut self, codegen: &mut Codegen, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            self.push(TIr::FStore(freg, reg));
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
            );
            codegen.store_rax(reg);
            self.stack_slot[reg] = LinkMode::XmmR(freg);
        }
    }

    fn all_write_back(&mut self, codegen: &mut Codegen) {
        let wb = self.get_write_back();
        self.write_back_no_dealloc(codegen, wb);

        self.xmm.iter_mut().for_each(|info| info.clear());
        self.stack_slot.clear()
    }

    ///
    /// Write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn write_back_no_dealloc(&mut self, codegen: &mut Codegen, wb: Vec<u16>) {
        for freg in wb {
            let v: Vec<_> = self.xmm[freg as usize]
                .iter()
                .filter(|reg| matches!(self.stack_slot[**reg], LinkMode::XmmRW(_)))
                .cloned()
                .collect();
            if v.is_empty() {
                continue;
            }

            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
            );
            for reg in v {
                codegen.store_rax(reg);
            }
        }
    }

    fn write_back_range(&mut self, codegen: &mut Codegen, arg: SlotId, len: u16) {
        for reg in arg.0..arg.0 + len {
            self.read_slot(codegen, SlotId::new(reg))
        }
    }

    ///
    /// Get *DestLabel* for a branch to *dest*.
    ///
    fn get_branch_dest(
        &mut self,
        codegen: &mut Codegen,
        dest: DestLabel,
        wb: Vec<u16>,
    ) -> DestLabel {
        codegen.jit.select(1);
        let entry = codegen.jit.label();
        codegen.jit.bind_label(entry);
        self.write_back_no_dealloc(codegen, wb);
        monoasm!(codegen.jit,
            jmp  dest;
        );
        codegen.jit.select(0);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    fn get_deopt_dest(&mut self, codegen: &mut Codegen, pc: BcPc) -> DestLabel {
        let wb = self.get_write_back();
        codegen.jit.select(1);
        let entry = codegen.jit.label();
        codegen.jit.bind_label(entry);
        self.write_back_no_dealloc(codegen, wb);
        let fetch = codegen.vm_fetch;
        monoasm!(codegen.jit,
            movq r13, (pc.0);
        );
        #[cfg(any(feature = "log-jit"))]
        monoasm!(codegen.jit,
            movq r8, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - 8];
            movq rcx, r13;
            movq rax, (Self::log_deoptimize);
            call rax;
        );
        monoasm!(codegen.jit,
            jmp fetch;
        );
        codegen.jit.select(0);
        entry
    }

    #[cfg(any(feature = "log-jit"))]
    extern "C" fn log_deoptimize(
        _interp: &mut Interp,
        globals: &mut Globals,
        func_id: FuncId,
        pc: BcPc,
        v: Value,
    ) {
        let name = match globals.func[func_id].as_normal().name() {
            Some(name) => name.to_string(),
            None => "<unnamed>".to_string(),
        };
        if let BcOp1::LoopEnd = pc.op1() {
            eprintln!("<-- exited from JIT code in {} {:?}.", name, func_id);
        } else {
            eprintln!(
                "<-- deoptimization occurs in {} {:?}. caused by {:?}",
                name, func_id, v
            );
        }
        let bc_begin = globals.func[func_id].as_normal().get_bytecode_address(0);
        let index = pc - bc_begin;
        eprint!("    [{:05}] {:?}", index, *pc);
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn deopt(&mut self, codegen: &mut Codegen, pc: BcPc) {
        let fallback = self.get_deopt_dest(codegen, pc);
        monoasm!(codegen.jit,
            jmp fallback;
        );
    }

    fn get_write_back(&self) -> Vec<u16> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i as u16) })
            .collect()
    }

    fn get_xmm_using(&self) -> Vec<usize> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i) })
            .collect()
    }

    fn convf64_assume(
        &mut self,
        codegen: &mut Codegen,
        rhs: SlotId,
        class: ClassId,
        side_exit: DestLabel,
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.convf64_assume_integer(codegen, rhs, side_exit),
            FLOAT_CLASS => self.convf64_assume_float(codegen, rhs, side_exit),
            _ => unreachable!(),
        }
    }

    fn convf64_assume_integer(
        &mut self,
        codegen: &mut Codegen,
        reg: SlotId,
        side_exit: DestLabel,
    ) -> u16 {
        let freg = self.xmm_read(reg);
        self.push(TIr::FLoad(reg, freg as u16));
        monoasm!(codegen.jit,
            movq rdi, [rbp - (conv(reg))];
        );
        codegen.assume_int_to_f64(freg as u64 + 2, side_exit);
        freg
    }

    fn convf64_assume_float(
        &mut self,
        codegen: &mut Codegen,
        reg: SlotId,
        side_exit: DestLabel,
    ) -> u16 {
        let freg = self.xmm_read(reg);
        self.push(TIr::FLoad(reg, freg as u16));
        monoasm!(codegen.jit,
            movq rdi, [rbp - (conv(reg))];
        );
        codegen.assume_float_to_f64(freg as u64 + 2, side_exit);
        freg
    }

    fn xmm_read_assume_float(&mut self, codegen: &mut Codegen, reg: SlotId, op: BcPc) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let side_exit = self.get_deopt_dest(codegen, op);
                self.convf64_assume_float(codegen, reg, side_exit)
            }
        }
    }

    fn xmm_read_binary(
        &mut self,
        codegen: &mut Codegen,
        lhs: SlotId,
        rhs: SlotId,
        op: BcPc,
    ) -> (u16, u16) {
        if lhs != rhs {
            match (self.stack_slot[lhs], self.stack_slot[rhs]) {
                (
                    LinkMode::XmmR(lhs) | LinkMode::XmmRW(lhs),
                    LinkMode::XmmR(rhs) | LinkMode::XmmRW(rhs),
                ) => (lhs, rhs),
                (LinkMode::XmmR(lhs) | LinkMode::XmmRW(lhs), LinkMode::None) => {
                    let side_exit = self.get_deopt_dest(codegen, op);
                    (
                        lhs,
                        self.convf64_assume(codegen, rhs, op.classid2(), side_exit),
                    )
                }
                (LinkMode::None, LinkMode::XmmR(rhs) | LinkMode::XmmRW(rhs)) => {
                    let side_exit = self.get_deopt_dest(codegen, op);
                    (
                        self.convf64_assume(codegen, lhs, op.classid1(), side_exit),
                        rhs,
                    )
                }
                (LinkMode::None, LinkMode::None) => {
                    let side_exit = self.get_deopt_dest(codegen, op);
                    (
                        self.convf64_assume(codegen, lhs, op.classid1(), side_exit),
                        self.convf64_assume(codegen, rhs, op.classid2(), side_exit),
                    )
                }
            }
        } else {
            match self.stack_slot[lhs] {
                LinkMode::XmmR(lhs) | LinkMode::XmmRW(lhs) => (lhs, lhs),
                LinkMode::None => {
                    let side_exit = self.get_deopt_dest(codegen, op);
                    match op.classid1() {
                        FLOAT_CLASS => {
                            let lhs = self.convf64_assume_float(codegen, lhs, side_exit);
                            (lhs, lhs)
                        }
                        _ => unreachable!(),
                    }
                }
            }
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
            fn [<cmp_opt_int_ $sop>](&mut self, dest: DestLabel, generic:DestLabel, switch:bool, ctx: &mut BBContext, v:Vec<u16>) {
                let cont = self.jit.label();
                let branch_dest = ctx.get_branch_dest(self, dest, v);
                if switch {
                    monoasm! { self.jit,
                        [<j $sop>] branch_dest;
                    };
                } else {
                    monoasm! { self.jit,
                        [<j $sop>] cont;
                        jmp branch_dest;
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
                        jmp branch_dest;
                    );
                } else {
                    monoasm!(self.jit,
                        jnz  cont;
                        jmp branch_dest;
                    );
                }
                self.jit.select(0);
            }

            fn [<cmp_opt_float_ $sop>](&mut self, dest: DestLabel, generic:DestLabel, switch:bool, ctx: &mut BBContext, v:Vec<u16>) {
                let cont = self.jit.label();
                let branch_dest = ctx.get_branch_dest(self, dest, v);
                if switch {
                    monoasm! { self.jit,
                        [<j $op>] branch_dest;
                    };

                } else {
                    monoasm! { self.jit,
                        [<j $op>] cont;
                        jmp branch_dest;
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
                        jmp branch_dest;
                    );
                } else {
                    monoasm!(self.jit,
                        jnz  cont;
                        jmp branch_dest;
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

enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

impl Codegen {
    cmp_opt_main!((eq, eq), (ne, ne), (a, gt), (b, lt), (ae, ge), (be, le));
    cmp_main!(eq, ne, lt, le, gt, ge);

    fn load_guard_rdi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(reg))];
        );
        self.guard_rdi_fixnum(deopt);
    }

    fn load_guard_rsi_fixnum(&mut self, reg: SlotId, deopt: DestLabel) {
        monoasm!(self.jit,
            movq rsi, [rbp - (conv(reg))];
        );
        self.guard_rsi_fixnum(deopt);
    }

    fn load_guard_binary_fixnum(&mut self, lhs: SlotId, rhs: SlotId, deopt: DestLabel) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
        self.guard_rdi_fixnum(deopt);
        self.guard_rsi_fixnum(deopt);
    }

    fn jit_get_constant(&mut self, ctx: &BBContext, id: ConstSiteId, pc: BcPc) {
        let jit_return = self.vm_return;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, (id.get());  // name: ConstSiteId
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (get_constant);
            call rax;
            movq r13, ((pc + 1).0);
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
        self.xmm_restore(&xmm_using);
    }

    fn jit_store_constant(&mut self, ctx: &BBContext, id: IdentId, src: SlotId) {
        let const_version = self.const_version;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
          movq rdx, (id.get());  // name: IdentId
          movq rcx, [rbp - (conv(src))];  // val: Value
          movq rdi, rbx;  // &mut Interp
          movq rsi, r12;  // &mut Globals
          addq [rip + const_version], 1;
          movq rax, (set_constant);
          call rax;
        );
        self.xmm_restore(&xmm_using);
    }

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
                "--> start {} compile: {} {:?} start:[{:05}] bytecode:{:?}",
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
                start_pos,
                func.bytecode().as_ptr(),
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
        for (idx, pc) in func.bytecode()[start_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            self.jit.bind_label(labels[idx]);
            match pc.op1() {
                BcOp1::LoopStart(_) => {
                    loop_count += 1;
                }
                BcOp1::LoopEnd => {
                    assert_ne!(0, loop_count);
                    loop_count -= 1;
                    if position.is_some() && loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", start_pos + idx);
                        ctx.deopt(self, pc);
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
                        let fdst = ctx.xmm_write(dst);
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
                BcOp1::Array(ret, src, len) => {
                    ctx.write_back_range(self, src, len);
                    ctx.dealloc_xmm(ret);
                    ctx.push(TIr::Array(ret, src, len));
                    monoasm!(self.jit,
                        lea  rdi, [rbp - (conv(src))];
                        movq rsi, (len);
                        movq rax, (gen_array);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                BcOp1::Index(ret, base, idx) => {
                    ctx.read_slot(self, base);
                    ctx.read_slot(self, idx);
                    ctx.dealloc_xmm(ret);
                    ctx.push(TIr::Index(ret, base, idx));
                    let jit_return = self.vm_return;
                    monoasm! { self.jit,
                        movq rdx, [rbp - (conv(base))]; // base: Value
                        movq rcx, [rbp - (conv(idx))]; // idx: Value
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &mut Globals
                        movq rax, (get_index);
                        call rax;
                        movq r13, ((pc + 1).0);
                        testq rax, rax;
                        jeq  jit_return;
                    };
                    self.store_rax(ret);
                }
                BcOp1::IndexAssign(src, base, idx) => {
                    ctx.read_slot(self, base);
                    ctx.read_slot(self, idx);
                    ctx.read_slot(self, src);
                    ctx.push(TIr::IndexAssign(src, base, idx));
                    let jit_return = self.vm_return;
                    monoasm! { self.jit,
                        movq rdx, [rbp - (conv(base))]; // base: Value
                        movq rcx, [rbp - (conv(idx))]; // idx: Value
                        movq r8, [rbp - (conv(src))];  // src: Value
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &mut Globals
                        movq rax, (set_index);
                        call rax;
                        movq r13, ((pc + 1).0);
                        testq rax, rax;
                        jeq  jit_return;
                    };
                }
                BcOp1::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    //let jit_return = self.vm_return;
                    let cached_value = self.jit.const_i64(0);
                    let cached_const_version = self.jit.const_i64(-1);
                    let global_const_version = self.const_version;
                    let slow_path = self.jit.label();
                    let exit = self.jit.label();

                    if pc.value().is_none() || pc.value().unwrap().class_id() != FLOAT_CLASS {
                        ctx.push(TIr::LoadConst(dst, id));

                        self.jit.select(1);
                        self.jit.bind_label(slow_path);
                        self.jit_get_constant(&ctx, id, pc);
                        monoasm!(self.jit,
                            movq [rip + cached_value], rax;
                            movq rdi, [rip + global_const_version];
                            movq [rip + cached_const_version], rdi;
                            jmp  exit;
                        );
                        self.jit.select(0);

                        monoasm!(self.jit,
                            movq rax, [rip + global_const_version];
                            cmpq rax, [rip + cached_const_version];
                            jne  slow_path;
                            movq rax, [rip + cached_value];
                        exit:
                        );
                        self.store_rax(dst);
                    } else {
                        let cached_float = self.jit.const_f64(0.0);
                        let side_exit = ctx.get_deopt_dest(self, pc);

                        self.jit.select(1);
                        self.jit.bind_label(slow_path);
                        self.jit_get_constant(&ctx, id, pc);
                        monoasm!(self.jit,
                            movq [rip + cached_value], rax;
                            movq rdi, rax;
                        );
                        self.assume_float_to_f64(0, side_exit);
                        monoasm!(self.jit,
                            movq [rip + cached_float], xmm0;
                            movq rax, [rip + global_const_version];
                            movq [rip + cached_const_version], rax;
                            jmp  exit;
                        );
                        self.jit.select(0);

                        let fdst = ctx.xmm_read(dst);
                        ctx.push(TIr::FLoadConst(fdst, dst, id));

                        monoasm!(self.jit,
                            movq rax, [rip + global_const_version];
                            cmpq rax, [rip + cached_const_version];
                            jne  slow_path;
                        exit:
                            movq xmm(fdst as u64 + 2), [rip + cached_float];
                            movq rax, [rip + cached_value];
                        );
                        self.store_rax(dst);
                    }
                }
                BcOp1::StoreConst(src, id) => {
                    ctx.push(TIr::StoreConst(src, id));
                    ctx.read_slot(self, src);
                    self.jit_store_constant(&ctx, id, src);
                }
                BcOp1::Nil(ret) => {
                    ctx.push(TIr::Nil(ret));
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp1::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = ctx.xmm_read_assume_float(self, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        ctx.push(TIr::FNeg(fdst, fsrc));
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.read_slot(self, src);
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
                    if pc.is_binary_integer() {
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOp(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(&mut ctx, pc, kind, ret, BinOpMode::RR(lhs, rhs));
                    } else if pc.is_binary_float() {
                        let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        ctx.push(TIr::FBinOp(kind, fret, flhs, frhs));

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
                                        testq  rax, rax;
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
                                        testq rax, rax;
                                        jz    div_by_zero;
                                        divsd xmm(fret as u64 + 2), xmm(frhs as u64 + 2);
                                    )
                                }
                                _ => unimplemented!(),
                            }
                        }
                    } else {
                        ctx.read_slot(self, lhs);
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOp(kind, ret, lhs, rhs, pc.classid1()));
                        self.load_binary_args(lhs, rhs);
                        self.gen_binop_kind(&ctx, pc, kind, ret);
                    }
                }

                BcOp1::BinOpRi(kind, ret, lhs, rhs) => {
                    if pc.is_integer1() {
                        ctx.read_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpRi(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(&mut ctx, pc, kind, ret, BinOpMode::RI(lhs, rhs));
                    } else if pc.is_float1() {
                        let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        ctx.push(TIr::FBinOpRf(kind, fret, flhs, rhs as f64));
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
                        ctx.read_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpRi(kind, ret, lhs, rhs, pc.classid1()));
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_binop_kind(&ctx, pc, kind, ret);
                    }
                }

                BcOp1::BinOpIr(kind, ret, lhs, rhs) => {
                    if pc.is_integer2() {
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpIr(kind, ret, lhs, rhs, pc.classid1()));
                        self.gen_binop_integer(&mut ctx, pc, kind, ret, BinOpMode::IR(lhs, rhs));
                    } else if pc.is_float2() {
                        let frhs = ctx.xmm_read_assume_float(self, rhs, pc);
                        let fret = ctx.xmm_write(ret);
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
                                        testq rax, rax;
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
                                        testq rax, rax;
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
                        ctx.read_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        ctx.push(TIr::BinOpIr(kind, ret, lhs, rhs, pc.classid1()));
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_binop_kind(&ctx, pc, kind, ret);
                    }
                }

                BcOp1::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(pc);
                    } else {
                        if pc.is_binary_float() {
                            let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
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
                            ctx.push(TIr::Cmp(kind, ret, lhs, rhs, optimizable, pc.classid1()));
                        }
                    }
                }
                BcOp1::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if optimizable {
                        assert!(self.opt_buf.is_none());
                        self.opt_buf = Some(pc);
                    } else {
                        if pc.is_float1() {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
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
                            ctx.push(TIr::Cmpri(kind, ret, lhs, rhs, optimizable, pc.classid1()));
                        }
                    }
                }
                BcOp1::Mov(dst, src) => {
                    ctx.copy_slot(self, src, dst);
                }
                BcOp1::Ret(lhs) => {
                    ctx.read_slot(self, lhs);
                    ctx.push(TIr::Ret(lhs));
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    let wb = ctx.get_write_back();
                    ctx.clear_write_back(wb);
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
                BcOp1::MethodCall(..) => {
                    assert!(self.opt_buf.is_none());
                    self.opt_buf = Some(pc);
                }
                BcOp1::MethodArgs(recv, args, len) => {
                    ctx.read_slot(self, recv);
                    ctx.write_back_range(self, args, len);

                    match std::mem::take(&mut self.opt_buf).unwrap().op1() {
                        BcOp1::MethodCall(ret, name) => {
                            ctx.push(TIr::MethodCall(ret, name, recv, args, len));
                            ctx.dealloc_xmm(ret);
                            self.jit_method_call(recv, name, ret, args, len, &ctx, pc + 2);
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
                    let wb = ctx.get_write_back();
                    ctx.push(TIr::CondBr(cond_, disp, false, wb.clone(), kind));
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let branch_dest = ctx.get_branch_dest(self, dest, wb);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!(self.jit, jne branch_dest;),
                        BrKind::BrIfNot => monoasm!(self.jit, jeq branch_dest;),
                    }
                }
                BcOp1::CondBr(cond_, disp, true, brkind) => {
                    let dest = labels[(idx as i32 + 1 + disp) as usize];
                    let generic = self.jit.label();
                    let pc = std::mem::take(&mut self.opt_buf).unwrap();
                    if pc.is_binary_float() {
                        let kind = match pc.op1() {
                            BcOp1::Cmp(kind, ret, lhs, rhs, true) => {
                                let (flhs, frhs) = ctx.xmm_read_binary(self, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                ctx.push(TIr::FCmp(kind, ret, flhs, frhs, true));
                                kind
                            }
                            BcOp1::Cmpri(kind, ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = ctx.xmm_read_assume_float(self, lhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                                };
                                ctx.push(TIr::FCmpRf(kind, ret, flhs, rhs as f64, true));
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let wb = ctx.get_write_back();
                        ctx.push(TIr::CondBr(cond_, disp, true, wb.clone(), brkind));
                        self.gen_cmp_float_opt(
                            kind,
                            dest,
                            generic,
                            brkind == BrKind::BrIf,
                            &mut ctx,
                            wb,
                        );
                    } else {
                        let kind = match pc.op1() {
                            BcOp1::Cmp(kind, ret, lhs, rhs, true) => {
                                self.gen_cmp_prep(&mut ctx, ret, lhs, rhs, generic);
                                ctx.push(TIr::Cmp(kind, ret, lhs, rhs, true, pc.classid1()));
                                kind
                            }
                            BcOp1::Cmpri(kind, ret, lhs, rhs, true) => {
                                self.gen_cmpri_prep(&mut ctx, ret, lhs, rhs, generic);
                                ctx.push(TIr::Cmpri(kind, ret, lhs, rhs, true, pc.classid1()));
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let wb = ctx.get_write_back();
                        ctx.push(TIr::CondBr(cond_, disp, true, wb.clone(), brkind));
                        monoasm! { self.jit,
                            cmpq rdi, rsi;
                        };
                        self.gen_cmp_int_opt(
                            kind,
                            dest,
                            generic,
                            brkind == BrKind::BrIf,
                            &mut ctx,
                            wb,
                        );
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
        eprintln!("    finished compile. elapsed:{:?}", elapsed);

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
        if len == 0 {
            return;
        }
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
        if len == 0 {
            return;
        }
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

    fn guard_binary_fixnum_with_mode(&mut self, generic: DestLabel, mode: BinOpMode) {
        match mode {
            BinOpMode::RR(..) => self.guard_rdi_rsi_fixnum(generic),
            BinOpMode::RI(..) => self.guard_rdi_fixnum(generic),
            BinOpMode::IR(..) => self.guard_rsi_fixnum(generic),
        }
    }

    fn load_binary_args_with_mode(&mut self, mode: &BinOpMode) {
        match mode {
            &BinOpMode::RR(lhs, rhs) => self.load_binary_args(lhs, rhs),
            &BinOpMode::RI(lhs, rhs) => {
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(lhs))];
                    movq rsi, (Value::int32(rhs as i32).get());
                );
            }
            &BinOpMode::IR(lhs, rhs) => {
                monoasm!(self.jit,
                    movq rdi, (Value::int32(lhs as i32).get());
                    movq rsi, [rbp - (conv(rhs))];
                );
            }
        }
    }

    fn gen_binop_integer(
        &mut self,
        ctx: &mut BBContext,
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
        mode: BinOpMode,
    ) {
        let deopt = ctx.get_deopt_dest(self, pc);
        match kind {
            BinOpK::Add => {
                match mode {
                    BinOpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, 1;
                            addq rdi, rsi;
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            addq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            addq rsi, (Value::int32(lhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rsi(ret);
                    }
                }
            }
            BinOpK::Sub => {
                match mode {
                    BinOpMode::RR(lhs, rhs) => {
                        self.load_guard_binary_fixnum(lhs, rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::RI(lhs, rhs) => {
                        self.load_guard_rdi_fixnum(lhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            subq rdi, (Value::int32(rhs as i32).get() - 1);
                            jo deopt;
                        );
                        self.store_rdi(ret);
                    }
                    BinOpMode::IR(lhs, rhs) => {
                        self.load_guard_rsi_fixnum(rhs, deopt);
                        monoasm!(self.jit,
                            // fastpath
                            movq rdi, (Value::int32(lhs as i32).get());
                            subq rdi, rsi;
                            jo deopt;
                            addq rdi, 1;
                        );
                        self.store_rdi(ret);
                    }
                }
            }
            BinOpK::Mul => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, mul_values as _, &ctx, pc);
            }
            BinOpK::Div => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, div_values as _, &ctx, pc);
            }
            _ => {
                let generic = self.jit.label();
                self.load_binary_args_with_mode(&mode);
                self.guard_binary_fixnum_with_mode(generic, mode);
                match kind {
                    BinOpK::BitOr => self.gen_bit_or(generic, ret, &ctx, pc),
                    BinOpK::BitAnd => self.gen_bit_and(generic, ret, &ctx, pc),
                    BinOpK::BitXor => self.gen_bit_xor(generic, ret, &ctx, pc),
                    BinOpK::Shr => self.gen_shr(generic, ret, &ctx, pc),
                    BinOpK::Shl => self.gen_shl(generic, ret, &ctx, pc),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn gen_binop_kind(&mut self, ctx: &BBContext, pc: BcPc, kind: BinOpK, ret: SlotId) {
        self.generic_binop(ret, kind.generic_func() as _, &ctx, pc);
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
        ctx.read_slot(self, lhs);
        ctx.read_slot(self, rhs);
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
        ctx.read_slot(self, lhs);
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

    fn gen_cmp_int_opt(
        &mut self,
        kind: CmpKind,
        dest: DestLabel,
        generic: DestLabel,
        switch: bool,
        ctx: &mut BBContext,
        v: Vec<u16>,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_int_eq(dest, generic, switch, ctx, v),
            CmpKind::Ne => self.cmp_opt_int_ne(dest, generic, switch, ctx, v),
            CmpKind::Ge => self.cmp_opt_int_ge(dest, generic, switch, ctx, v),
            CmpKind::Gt => self.cmp_opt_int_gt(dest, generic, switch, ctx, v),
            CmpKind::Le => self.cmp_opt_int_le(dest, generic, switch, ctx, v),
            CmpKind::Lt => self.cmp_opt_int_lt(dest, generic, switch, ctx, v),
            _ => unimplemented!(),
        }
    }

    fn gen_cmp_float_opt(
        &mut self,
        kind: CmpKind,
        dest: DestLabel,
        generic: DestLabel,
        switch: bool,
        ctx: &mut BBContext,
        v: Vec<u16>,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_float_eq(dest, generic, switch, ctx, v),
            CmpKind::Ne => self.cmp_opt_float_ne(dest, generic, switch, ctx, v),
            CmpKind::Ge => self.cmp_opt_float_ge(dest, generic, switch, ctx, v),
            CmpKind::Gt => self.cmp_opt_float_gt(dest, generic, switch, ctx, v),
            CmpKind::Le => self.cmp_opt_float_le(dest, generic, switch, ctx, v),
            CmpKind::Lt => self.cmp_opt_float_lt(dest, generic, switch, ctx, v),
            _ => unimplemented!(),
        }
    }

    fn gen_bit_or(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _, &ctx, pc);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _, &ctx, pc);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _, &ctx, pc);
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

    fn gen_shr(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext, pc: BcPc) {
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
        self.side_generic_op(generic, ret, shr_values as _, &ctx, pc);
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

    fn gen_shl(&mut self, generic: DestLabel, ret: SlotId, ctx: &BBContext, pc: BcPc) {
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

        self.side_generic_op(generic, ret, shl_values as _, &ctx, pc);
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

    fn side_generic_op(
        &mut self,
        generic: DestLabel,
        ret: SlotId,
        func: u64,
        ctx: &BBContext,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func, ctx, pc);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select(0);
    }

    fn generic_binop(&mut self, ret: SlotId, func: u64, ctx: &BBContext, pc: BcPc) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq r13, ((pc + 1).0);
        );
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
        pc: BcPc,
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
        let method_resolved = self.jit.label();
        let patch_meta = self.jit.label();
        let patch_adr = self.jit.label();
        let patch_pc = self.jit.label();
        let slow_path = self.jit.label();
        let raise = self.jit.label();
        let cached_class_version = self.jit.const_i32(-1);
        let cached_recv_class = self.jit.const_i32(0);
        let global_class_version = self.class_version;
        let entry_find_method = self.entry_find_method;
        let entry_panic = self.entry_panic;
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
        method_resolved:
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
            jeq raise;
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
            jeq raise;

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
            jmp method_resolved;
        );
        let entry_return = self.vm_return;
        // raise error.
        monoasm!(self.jit,
        raise:
            movq r13, (pc.0);
            jmp entry_return;
        );
        self.jit.select(0);
    }
}
