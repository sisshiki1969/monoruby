use crate::bytecodegen::BinOpK;

use super::*;

mod binary_op;
mod constants;
mod index;
mod ivar;

pub(crate) struct AsmIr {
    pub(super) inst: Vec<AsmInst>,
    pub(super) side_exit: Vec<SideExit>,
    pub(super) label: usize,
}

impl AsmIr {
    pub fn new() -> Self {
        Self {
            inst: vec![],
            side_exit: vec![],
            label: 0,
        }
    }

    pub(super) fn new_deopt(&mut self, pc: BcPc, wb: WriteBack) -> usize {
        let label = self.new_label();
        self.side_exit.push(SideExit::Deoptimize(pc, wb, label));
        label
    }

    pub(super) fn new_error(&mut self, pc: BcPc, wb: WriteBack) -> usize {
        let label = self.new_label();
        self.side_exit.push(SideExit::Error(pc, wb, label));
        label
    }

    fn new_label(&mut self) -> usize {
        let label = self.label;
        self.label += 1;
        label
    }

    pub(super) fn deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let exit = self.new_deopt(pc, ctx.get_write_back());
        self.inst.push(AsmInst::Deopt(exit));
    }

    pub(super) fn recompile_and_deopt(
        &mut self,
        ctx: &BBContext,
        pc: BcPc,
        position: Option<BcPc>,
    ) {
        let deopt = self.new_deopt(pc, ctx.get_write_back());
        self.inst.push(AsmInst::RecompileDeopt { position, deopt });
    }

    pub(super) fn rax2acc(&mut self, ctx: &mut BBContext, dst: impl Into<Option<SlotId>>) {
        self.reg2acc(ctx, GP::Rax, dst);
    }

    fn reg2acc(&mut self, ctx: &mut BBContext, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            ctx.clear();
            if let Some(acc) = ctx.clear_r15()
                && acc < ctx.sp
                && acc != dst
            {
                self.inst.push(AsmInst::AccToStack(acc));
            }
            ctx.link_r15(dst);
            self.inst.push(AsmInst::RegToAcc(src));
        }
    }

    pub(super) fn reg_move(&mut self, src: GP, dst: GP) {
        if src != dst {
            self.inst.push(AsmInst::RegMove(src, dst));
        }
    }

    pub(super) fn reg2stack(&mut self, src: GP, dst: impl Into<Option<SlotId>>) {
        if let Some(dst) = dst.into() {
            self.inst.push(AsmInst::RegToStack(src, dst));
        }
    }

    pub(super) fn stack2reg(&mut self, src: SlotId, dst: GP) {
        self.inst.push(AsmInst::StackToReg(src, dst));
    }

    pub(super) fn xmm_move(&mut self, src: Xmm, dst: Xmm) {
        self.inst.push(AsmInst::XmmMove(src, dst));
    }

    pub(super) fn xmm_swap(&mut self, x1: Xmm, x2: Xmm) {
        self.inst.push(AsmInst::XmmSwap(x1, x2));
    }

    pub(super) fn xmm_binop(&mut self, kind: BinOpK, mode: FMode, dst: Xmm, using_xmm: UsingXmm) {
        self.inst.push(AsmInst::XmmBinOp {
            kind,
            mode,
            dst,
            using_xmm,
        });
    }

    pub(super) fn xmm2both(&mut self, freg: Xmm, reg: Vec<SlotId>) {
        self.inst.push(AsmInst::XmmToBoth(freg, reg));
    }

    pub(super) fn lit2stack(&mut self, v: Value, reg: SlotId) {
        self.inst.push(AsmInst::LitToStack(v, reg));
    }

    pub(super) fn lit2reg(&mut self, v: Value, reg: GP) {
        self.inst.push(AsmInst::LitToReg(v, reg));
    }

    pub(super) fn acc2stack(&mut self, reg: SlotId) {
        self.inst.push(AsmInst::AccToStack(reg));
    }

    pub(super) fn int2xmm(&mut self, reg: GP, x: Xmm, deopt: usize) {
        self.inst.push(AsmInst::IntToXmm(reg, x, deopt));
    }

    pub(super) fn float2xmm(&mut self, reg: GP, x: Xmm, deopt: usize) {
        self.inst.push(AsmInst::FloatToXmm(reg, x, deopt));
    }

    pub(super) fn f64toxmm(&mut self, f: f64, x: Xmm) {
        self.inst.push(AsmInst::F64ToXmm(f, x));
    }

    pub(super) fn i64toboth(&mut self, i: i64, reg: SlotId, x: Xmm) {
        self.inst.push(AsmInst::I64ToBoth(i, reg, x));
    }

    /// rax = val
    pub(super) fn deep_copy_lit(&mut self, ctx: &BBContext, val: Value) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::DeepCopyLit(val, using_xmm));
    }

    pub(super) fn guard_fixnum(&mut self, r: GP, deopt: usize) {
        self.inst.push(AsmInst::GuardFixnum(r, deopt));
    }

    pub(super) fn guard_float(&mut self, r: GP, deopt: usize) {
        self.inst.push(AsmInst::GuardFloat(r, deopt));
    }

    pub(super) fn generic_unop(&mut self, ctx: &BBContext, pc: BcPc, func: UnaryOpFn) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::GenericUnOp {
            func,
            using_xmm,
            error,
        });
    }

    pub(super) fn generic_binop(&mut self, ctx: &BBContext, pc: BcPc, kind: BinOpK) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::GenericBinOp {
            kind,
            using_xmm,
            error,
        });
    }

    pub(super) fn integer_binop(&mut self, ctx: &BBContext, pc: BcPc, kind: BinOpK, mode: OpMode) {
        let using_xmm = ctx.get_using_xmm();
        let deopt = self.new_deopt(pc, ctx.get_write_back());
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::IntegerBinOp {
            kind,
            mode,
            using_xmm,
            deopt,
            error,
        });
    }

    pub(super) fn generic_cmp(&mut self, ctx: &BBContext, pc: BcPc, kind: CmpKind) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::GenericCmp {
            kind,
            using_xmm,
            error,
        });
    }

    pub(super) fn integer_cmp_br(
        &mut self,
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    ) {
        self.inst.push(AsmInst::IntegerCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn float_cmp_br(
        &mut self,
        mode: FMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    ) {
        self.inst.push(AsmInst::FloatCmpBr {
            mode,
            kind,
            brkind,
            branch_dest,
        });
    }

    pub(super) fn generic_index(&mut self, ctx: &BBContext, pc: BcPc, base: SlotId, idx: SlotId) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::GenericIndex {
            base,
            idx,
            pc,
            using_xmm,
            error,
        });
    }

    pub(super) fn array_u16_index_assign(&mut self, ctx: &BBContext, pc: BcPc, idx: u16) {
        let using_xmm = ctx.get_using_xmm();
        let deopt = self.new_deopt(pc, ctx.get_write_back());
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::ArrayU16IndexAssign {
            idx,
            using_xmm,
            deopt,
            error,
        });
    }

    pub(super) fn array_index_assign(&mut self, ctx: &BBContext, pc: BcPc) {
        let using_xmm = ctx.get_using_xmm();
        let deopt = self.new_deopt(pc, ctx.get_write_back());
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::ArrayIndexAssign {
            using_xmm,
            deopt,
            error,
        });
    }

    pub(super) fn generic_index_assign(
        &mut self,
        ctx: &BBContext,
        pc: BcPc,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
    ) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::GenericIndexAssign {
            src,
            base,
            idx,
            pc,
            using_xmm,
            error,
        });
    }

    pub(super) fn new_array(&mut self, ctx: &BBContext, callid: CallSiteId) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::NewArray(callid, using_xmm));
    }

    pub(super) fn new_hash(&mut self, ctx: &BBContext, args: SlotId, len: usize) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::NewHash(args, len, using_xmm));
    }

    pub(super) fn new_range(
        &mut self,
        ctx: &BBContext,
        pc: BcPc,
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
    ) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::NewRange {
            start,
            end,
            exclude_end,
            using_xmm,
            error,
        });
    }

    pub(super) fn block_arg_proxy(&mut self, ret: SlotId, outer: usize) {
        self.inst.push(AsmInst::BlockArgProxy { ret, outer });
    }

    pub(super) fn block_arg(&mut self, ctx: &BBContext, pc: BcPc, ret: SlotId, outer: usize) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_deopt(pc, ctx.get_write_back());
        self.inst.push(AsmInst::BlockArg {
            ret,
            outer,
            using_xmm,
            error,
        });
    }

    pub(super) fn load_gvar(&mut self, ctx: &BBContext, name: IdentId) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::LoadGVar { name, using_xmm });
    }

    pub(super) fn store_gvar(&mut self, ctx: &BBContext, name: IdentId, src: SlotId) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::StoreGVar {
            name,
            src,
            using_xmm,
        });
    }

    pub(super) fn concat_str(&mut self, ctx: &BBContext, arg: SlotId, len: u16) {
        let using_xmm = ctx.get_using_xmm();
        self.inst.push(AsmInst::ConcatStr {
            arg,
            len,
            using_xmm,
        });
    }

    pub(super) fn concat_regexp(&mut self, ctx: &BBContext, pc: BcPc, arg: SlotId, len: u16) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        let len = len as _;
        self.inst.push(AsmInst::ConcatRegexp {
            arg,
            len,
            using_xmm,
            error,
        });
    }

    pub(super) fn expand_array(&mut self, ctx: &BBContext, dst: SlotId, len: u16) {
        let using_xmm = ctx.get_using_xmm();
        let len = len as _;
        self.inst.push(AsmInst::ExpandArray {
            dst,
            len,
            using_xmm,
        });
    }

    pub(super) fn alias_method(&mut self, ctx: &BBContext, pc: BcPc, new: SlotId, old: SlotId) {
        let using_xmm = ctx.get_using_xmm();
        let error = self.new_error(pc, ctx.get_write_back());
        self.inst.push(AsmInst::AliasMethod {
            new,
            old,
            using_xmm,
            error,
        });
    }
}

impl AsmIr {
    pub(super) fn load_binary_with_mode(&mut self, mode: OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.stack2reg(lhs, GP::Rdi);
                self.stack2reg(rhs, GP::Rsi);
            }
            OpMode::RI(lhs, rhs) => {
                self.stack2reg(lhs, GP::Rdi);
                self.lit2reg(Value::i32(rhs as i32), GP::Rsi);
            }
            OpMode::IR(lhs, rhs) => {
                self.lit2reg(Value::i32(lhs as i32), GP::Rdi);
                self.stack2reg(rhs, GP::Rsi);
            }
        }
    }

    pub(super) fn fmode(
        &mut self,
        mode: &OpMode,
        ctx: &mut BBContext,
        pc: BcPc,
        deopt: usize,
    ) -> FMode {
        match mode {
            OpMode::RR(l, r) => {
                let (flhs, frhs) = ctx.fetch_float_binary(self, *l, *r, pc, deopt);
                FMode::RR(flhs, frhs)
            }
            OpMode::RI(l, r) => {
                let l = ctx.fetch_float_assume_float(self, *l, deopt);
                FMode::RI(l, *r)
            }
            OpMode::IR(l, r) => {
                let r = ctx.fetch_float_assume_float(self, *r, deopt);
                FMode::IR(*l, r)
            }
        }
    }
}

pub(super) enum AsmInst {
    /// move acc to stack
    AccToStack(SlotId),
    /// move reg to acc
    RegToAcc(GP),
    /// move reg to stack
    RegToStack(GP, SlotId),
    /// move reg to stack
    StackToReg(SlotId, GP),
    LitToReg(Value, GP),
    /// move reg to reg
    RegMove(GP, GP),

    XmmMove(Xmm, Xmm),
    XmmSwap(Xmm, Xmm),
    XmmBinOp {
        kind: BinOpK,
        mode: FMode,
        dst: Xmm,
        using_xmm: UsingXmm,
    },
    XmmUnOp {
        kind: UnOpK,
        dst: Xmm,
    },

    /// move f64 to xmm
    F64ToXmm(f64, Xmm),
    /// move i64 to both of xmm and a stack slot
    I64ToBoth(i64, SlotId, Xmm),
    XmmToBoth(Xmm, Vec<SlotId>),
    LitToStack(Value, SlotId),
    DeepCopyLit(Value, UsingXmm),
    NumToXmm(GP, Xmm, usize),
    IntToXmm(GP, Xmm, usize),
    /// move a Flonum Value in a reg to xmm reg, and deoptimize if it is not a Flonum.
    FloatToXmm(GP, Xmm, usize),

    /// check whether a Value in a stack slot is a Flonum, and if not, deoptimize.
    GuardFloat(GP, usize),
    GuardFixnum(GP, usize),

    ///
    /// Conditional branch
    ///
    /// When *kind* is BrKind::BrIf, jump to *dst* if the Value in `rax` is true.
    ///
    /// ### in
    /// - rax: Value
    ///
    GenericCondBr {
        brkind: BrKind,
        branch_dest: DestLabel,
    },
    /// deoptimize
    Deopt(usize),
    /// recompile and deoptimize
    RecompileDeopt {
        position: Option<BcPc>,
        deopt: usize,
    },

    GenericUnOp {
        func: UnaryOpFn,
        using_xmm: UsingXmm,
        error: usize,
    },

    GenericBinOp {
        kind: BinOpK,
        using_xmm: UsingXmm,
        error: usize,
    },
    IntegerBinOp {
        kind: BinOpK,
        mode: OpMode,
        using_xmm: UsingXmm,
        deopt: usize,
        error: usize,
    },

    ///
    /// Generic comparison
    ///
    /// Compare two Values in `rdi` and `rsi` with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    ///
    GenericCmp {
        kind: CmpKind,
        using_xmm: UsingXmm,
        error: usize,
    },
    ///
    /// Integer comparison
    ///
    /// Compare two Values in `rdi` and `rsi` with *kind*, and return the result in `rax` as Value.
    ///
    /// If error occurs in comparison operation, raise error.
    ///
    IntegerCmp {
        mode: OpMode,
        kind: CmpKind,
    },
    ///
    /// Integer comparison and conditional branch
    ///
    /// Compare two values with *mode*, jump to *branch_dest* if the condition specified by *kind*
    /// and *brkind* is met.
    ///
    IntegerCmpBr {
        mode: OpMode,
        kind: CmpKind,
        brkind: BrKind,
        branch_dest: DestLabel,
    },
    FloatCmp {
        kind: CmpKind,
        mode: FMode,
    },
    FloatCmpBr {
        kind: CmpKind,
        mode: FMode,
        brkind: BrKind,
        branch_dest: DestLabel,
    },

    GuardBaseClass {
        base_class: Value,
        deopt: usize,
    },
    LoadFloatConstant {
        fdst: Xmm,
        f: f64,
        cached_version: usize,
        deopt: usize,
    },
    LoadGenericConstant {
        cached_val: Value,
        cached_version: usize,
        deopt: usize,
    },
    StoreConstant {
        name: IdentId,
        using_xmm: UsingXmm,
    },

    GenericIndex {
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: usize,
    },
    ArrayU16Index {
        idx: u16,
        deopt: usize,
    },
    ArrayIndex {
        deopt: usize,
    },
    GenericIndexAssign {
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: usize,
    },
    ArrayU16IndexAssign {
        idx: u16,
        using_xmm: UsingXmm,
        deopt: usize,
        error: usize,
    },
    ArrayIndexAssign {
        using_xmm: UsingXmm,
        deopt: usize,
        error: usize,
    },

    /// create a new Array object and store it to rax
    NewArray(CallSiteId, UsingXmm),
    /// create a new Hash object and store it to rax
    NewHash(SlotId, usize, UsingXmm),
    /// create a new Range object and store it to rax
    NewRange {
        start: SlotId,
        end: SlotId,
        exclude_end: bool,
        using_xmm: UsingXmm,
        error: usize,
    },
    ConcatStr {
        arg: SlotId,
        len: u16,
        using_xmm: UsingXmm,
    },

    BlockArgProxy {
        ret: SlotId,
        outer: usize,
    },
    BlockArg {
        ret: SlotId,
        outer: usize,
        using_xmm: UsingXmm,
        error: usize,
    },

    LoadIVar {
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
    },
    StoreIVar {
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
        error: usize,
    },

    /// rax = DynVar(src)
    LoadDynVar {
        src: DynVar,
    },
    /// DynVar(dst) = src
    StoreDynVar {
        dst: DynVar,
        src: GP,
    },
    LoadGVar {
        name: IdentId,
        using_xmm: UsingXmm,
    },
    StoreGVar {
        name: IdentId,
        src: SlotId,
        using_xmm: UsingXmm,
    },

    ClassDef {
        superclass: SlotId,
        dst: Option<SlotId>,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        using_xmm: UsingXmm,
        error: usize,
    },
    SingletonClassDef {
        base: SlotId,
        dst: Option<SlotId>,
        func_id: FuncId,
        using_xmm: UsingXmm,
        error: usize,
    },
    MethodDef {
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
    },
    SingletonMethodDef {
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
    },

    ExpandArray {
        dst: SlotId,
        len: usize,
        using_xmm: UsingXmm,
    },
    ConcatRegexp {
        arg: SlotId,
        len: usize,
        using_xmm: UsingXmm,
        error: usize,
    },
    AliasMethod {
        new: SlotId,
        old: SlotId,
        using_xmm: UsingXmm,
        error: usize,
    },
    DefinedYield {
        dst: SlotId,
        using_xmm: UsingXmm,
    },
    DefinedConst {
        dst: SlotId,
        siteid: ConstSiteId,
        using_xmm: UsingXmm,
    },
    DefinedMethod {
        dst: SlotId,
        recv: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
    DefinedGvar {
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
    DefinedIvar {
        dst: SlotId,
        name: IdentId,
        using_xmm: UsingXmm,
    },
}

#[derive(Clone, Debug)]
pub(super) enum FMode {
    RR(Xmm, Xmm),
    RI(Xmm, i16),
    IR(i16, Xmm),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum GP {
    Rax = 0,
    Rsi = 6,
    Rdi = 7,
    R15 = 15,
}

pub(super) enum SideExit {
    Deoptimize(BcPc, WriteBack, usize),
    Error(BcPc, WriteBack, usize),
}

impl Codegen {
    pub(super) fn gen_asm(&mut self, ctx: &mut JitContext) {
        for (ir, entry, exit) in std::mem::take(&mut ctx.asmir) {
            self.gen_code_block(ir, entry, exit);
        }
    }

    fn gen_code_block(&mut self, ir: AsmIr, entry: DestLabel, exit: Option<DestLabel>) {
        let mut labels = vec![];
        for _ in 0..ir.label {
            labels.push(self.jit.label());
        }

        for side_exit in ir.side_exit {
            match side_exit {
                SideExit::Deoptimize(pc, wb, label) => {
                    self.gen_deopt_with_label(pc, &wb, labels[label])
                }
                SideExit::Error(pc, wb, label) => self.gen_handle_error(pc, wb, labels[label]),
            }
        }
        if exit.is_some() {
            self.jit.select_page(1);
        }
        self.jit.bind_label(entry);
        for inst in ir.inst {
            self.gen_asmir(&labels, &inst);
        }
        if let Some(exit) = exit {
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
    }

    pub(crate) fn gen_code(&mut self, ir: AsmIr) {
        let mut labels = vec![];
        for _ in 0..ir.label {
            labels.push(self.jit.label());
        }

        for side_exit in ir.side_exit {
            match side_exit {
                SideExit::Deoptimize(pc, wb, label) => {
                    self.gen_deopt_with_label(pc, &wb, labels[label])
                }
                SideExit::Error(pc, wb, label) => self.gen_handle_error(pc, wb, labels[label]),
            }
        }
        for inst in ir.inst {
            self.gen_asmir(&labels, &inst);
        }
    }

    fn gen_asmir(&mut self, labels: &[DestLabel], inst: &AsmInst) {
        match inst {
            AsmInst::AccToStack(r) => {
                self.store_r15(*r);
            }
            AsmInst::RegToAcc(r) => {
                let r = *r as u64;
                monoasm!( &mut self.jit,
                    movq r15, R(r);
                );
            }
            AsmInst::RegToStack(r, slot) => {
                let r = *r as u64;
                monoasm!( &mut self.jit,
                    movq [r14 - (conv(*slot))], R(r);
                );
            }
            AsmInst::StackToReg(slot, r) => {
                let r = *r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), [r14 - (conv(*slot))];
                );
            }
            AsmInst::LitToReg(v, r) => {
                let r = *r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (v.id());
                );
            }
            AsmInst::RegMove(src, dst) => {
                let src = *src as u64;
                let dst = *dst as u64;
                monoasm!( &mut self.jit,
                    movq R(dst), R(src);
                );
            }

            AsmInst::XmmMove(l, r) => self.xmm_mov(*l, *r),
            AsmInst::XmmSwap(l, r) => self.xmm_swap(*l, *r),
            AsmInst::XmmBinOp {
                kind,
                mode,
                dst,
                using_xmm,
            } => self.gen_float_binop(*kind, *using_xmm, *dst, mode.clone()),
            AsmInst::XmmUnOp { kind, dst } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    monoasm!( &mut self.jit,
                        xorps xmm(dst.enc()), [rip + imm];
                    );
                }
                UnOpK::Pos => {}
            },

            AsmInst::NumToXmm(reg, x, side_exit) => {
                self.numeric_val_to_f64(*reg, x.enc(), labels[*side_exit]);
            }
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(*f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::IntToXmm(r, x, side_exit) => {
                self.integer_val_to_f64(*r, x.enc(), labels[*side_exit]);
            }
            AsmInst::FloatToXmm(reg, x, side_exit) => {
                self.float_to_f64(*reg, x.enc(), labels[*side_exit]);
            }
            AsmInst::I64ToBoth(i, r, x) => {
                let f = self.jit.const_f64(*i as f64);
                monoasm! {&mut self.jit,
                    movq [r14 - (conv(*r))], (Value::integer(*i).id());
                    movq xmm(x.enc()), [rip + f];
                }
            }
            AsmInst::XmmToBoth(x, slots) => self.xmm_to_both(*x, slots),
            AsmInst::LitToStack(v, slot) => self.literal_to_stack(*slot, *v),
            AsmInst::DeepCopyLit(v, using_xmm) => {
                self.xmm_save(*using_xmm);
                monoasm!( &mut self.jit,
                  movq rdi, (v.id());
                  movq rax, (Value::value_deep_copy);
                  call rax;
                );
                self.xmm_restore(*using_xmm);
            }

            AsmInst::GuardFloat(r, deopt) => {
                let deopt = labels[*deopt];
                self.guard_float(*r, deopt);
            }
            AsmInst::GuardFixnum(r, deopt) => {
                let deopt = labels[*deopt];
                monoasm!( &mut self.jit,
                    testq R(*r as u64), 0x1;
                    jz deopt;
                );
            }
            AsmInst::Deopt(deopt) => {
                let deopt = labels[*deopt];
                monoasm!( &mut self.jit,
                    jmp deopt;
                );
            }
            AsmInst::RecompileDeopt { position, deopt } => {
                let deopt = labels[*deopt];
                self.recompile_and_deopt(*position, deopt)
            }

            AsmInst::GenericUnOp {
                func,
                using_xmm,
                error,
            } => {
                self.xmm_save(*using_xmm);
                self.call_unop(*func);
                self.xmm_restore(*using_xmm);
                self.handle_error(labels, *error);
            }

            AsmInst::GenericBinOp {
                kind,
                using_xmm,
                error,
            } => {
                self.gen_generic_binop(*kind, labels, *using_xmm, *error);
            }
            AsmInst::IntegerBinOp {
                kind,
                mode,
                using_xmm,
                deopt,
                error,
            } => {
                let deopt = labels[*deopt];
                match kind {
                    BinOpK::Add => match mode {
                        OpMode::RR(_, _) => {
                            monoasm!( &mut self.jit,
                                subq rdi, 1;
                                addq rdi, rsi;
                                jo deopt;
                            );
                        }
                        OpMode::RI(_, i) | OpMode::IR(i, _) => {
                            monoasm!( &mut self.jit,
                                addq rdi, (Value::i32(*i as i32).id() - 1);
                                jo deopt;
                            );
                        }
                    },
                    BinOpK::Sub => match mode {
                        OpMode::RR(_, _) => {
                            monoasm!( &mut self.jit,
                                subq rdi, rsi;
                                jo deopt;
                                addq rdi, 1;
                            );
                        }
                        OpMode::RI(_, rhs) => {
                            monoasm!( &mut self.jit,
                                subq rdi, (Value::i32(*rhs as i32).id() - 1);
                                jo deopt;
                            );
                        }
                        OpMode::IR(lhs, _) => {
                            monoasm!( &mut self.jit,
                                movq rdi, (Value::i32(*lhs as i32).id());
                                subq rdi, rsi;
                                jo deopt;
                                addq rdi, 1;
                            );
                        }
                    },
                    BinOpK::Exp => {
                        self.xmm_save(*using_xmm);
                        monoasm!( &mut self.jit,
                            sarq rdi, 1;
                            sarq rsi, 1;
                            movq rax, (pow_ii as u64);
                            call rax;
                        );
                        self.xmm_restore(*using_xmm);
                    }
                    BinOpK::Mul | BinOpK::Div => {
                        self.gen_generic_binop(*kind, labels, *using_xmm, *error);
                    }
                    BinOpK::Rem => match mode {
                        OpMode::RI(_, rhs) if *rhs > 0 && (*rhs as u64).is_power_of_two() => {
                            monoasm!( &mut self.jit,
                                andq rdi, (*rhs * 2 - 1);
                            );
                        }
                        _ => {
                            self.gen_generic_binop(*kind, labels, *using_xmm, *error);
                        }
                    },
                    BinOpK::BitOr => match mode {
                        OpMode::RR(_, _) => {
                            monoasm!( &mut self.jit,
                                orq rdi, rsi;
                            );
                        }
                        OpMode::RI(_, i) | OpMode::IR(i, _) => {
                            monoasm!( &mut self.jit,
                                orq rdi, (Value::i32(*i as i32).id());
                            );
                        }
                    },
                    BinOpK::BitAnd => match mode {
                        OpMode::RR(_, _) => {
                            monoasm!( &mut self.jit,
                                andq rdi, rsi;
                            );
                        }
                        OpMode::RI(_, i) | OpMode::IR(i, _) => {
                            monoasm!( &mut self.jit,
                                andq rdi, (Value::i32(*i as i32).id());
                            );
                        }
                    },
                    BinOpK::BitXor => match mode {
                        OpMode::RR(_, _) => {
                            monoasm!( &mut self.jit,
                                xorq rdi, rsi;
                                addq rdi, 1;
                            );
                        }
                        OpMode::RI(_, i) | OpMode::IR(i, _) => {
                            monoasm!( &mut self.jit,
                                xorq rdi, (Value::i32(*i as i32).id() - 1);
                            );
                        }
                    },
                }
            }

            AsmInst::GenericCmp {
                kind,
                using_xmm,
                error,
            } => {
                self.generic_cmp(kind, *using_xmm);
                self.handle_error(labels, *error);
            }
            AsmInst::IntegerCmp { kind, mode } => {
                if matches!(kind, CmpKind::Cmp) {
                    match mode {
                        OpMode::RR(..) => {}
                        OpMode::RI(_, r) => {
                            monoasm!( &mut self.jit,
                                movq rsi, (Value::i32(*r as i32).id());
                            );
                        }
                        OpMode::IR(l, _) => {
                            monoasm!( &mut self.jit,
                                movq rdi, (Value::i32(*l as i32).id());
                            );
                        }
                    }
                    self.icmp_cmp();
                } else {
                    monoasm! { &mut self.jit,
                        xorq rax, rax;
                    };
                    self.opt_integer_cmp(mode);
                    self.flag_to_bool(*kind);
                }
            }
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                brkind,
                branch_dest,
            } => {
                self.opt_integer_cmp(mode);
                self.condbr_int(*kind, *branch_dest, *brkind);
            }
            AsmInst::FloatCmp { kind, mode } => {
                match mode {
                    FMode::RR(l, r) => {
                        monoasm! { &mut self.jit,
                            xorq rax, rax;
                            ucomisd xmm(l.enc()), xmm(r.enc());
                        };
                    }
                    FMode::RI(l, r) => {
                        let rhs_label = self.jit.const_f64(*r as f64);
                        monoasm! { &mut self.jit,
                            xorq rax, rax;
                            ucomisd xmm(l.enc()), [rip + rhs_label];
                        };
                    }
                    _ => unreachable!(),
                }
                self.setflag_float(*kind);
            }
            AsmInst::FloatCmpBr {
                kind,
                mode,
                brkind,
                branch_dest,
            } => {
                match mode {
                    FMode::RR(l, r) => {
                        monoasm! { &mut self.jit,
                            ucomisd xmm(l.enc()), xmm(r.enc());
                        };
                    }
                    FMode::RI(l, r) => {
                        let r = self.jit.const_f64(*r as f64);
                        monoasm! { &mut self.jit,
                            ucomisd xmm(l.enc()), [rip + r];
                        };
                    }
                    _ => unreachable!(),
                }
                self.condbr_float(*kind, *branch_dest, *brkind);
            }

            AsmInst::GuardBaseClass { base_class, deopt } => {
                let deopt = labels[*deopt];
                let cached_base_class = self.jit.const_i64(base_class.id() as _);
                monoasm! { &mut self.jit,
                    cmpq rax, [rip + cached_base_class];  // rax: base_class
                    jne  deopt;
                }
            }
            AsmInst::LoadFloatConstant {
                fdst,
                f,
                cached_version: version,
                deopt,
            } => {
                let deopt = labels[*deopt];
                self.load_float_constant(*fdst, deopt, *f, *version);
            }
            AsmInst::LoadGenericConstant {
                cached_val,
                cached_version,
                deopt,
            } => {
                let deopt = labels[*deopt];
                self.load_generic_constant(deopt, *cached_val, *cached_version);
            }
            AsmInst::StoreConstant { name, using_xmm } => {
                self.store_constant(*name, *using_xmm);
            }

            AsmInst::GenericIndex {
                base,
                idx,
                pc,
                using_xmm,
                error,
            } => {
                self.generic_index(*using_xmm, *base, *idx, *pc);
                self.handle_error(labels, *error);
            }
            AsmInst::ArrayU16Index { idx, deopt } => {
                let deopt = labels[*deopt];
                self.gen_array_u16_index(*idx, deopt);
            }
            AsmInst::ArrayIndex { deopt } => {
                let deopt = labels[*deopt];
                self.gen_array_index(deopt);
            }
            AsmInst::GenericIndexAssign {
                src,
                base,
                idx,
                pc,
                using_xmm,
                error,
            } => {
                self.generic_index_assign(*using_xmm, *base, *idx, *src, *pc);
                self.handle_error(labels, *error);
            }
            AsmInst::ArrayU16IndexAssign {
                idx,
                using_xmm,
                deopt,
                error,
            } => {
                let deopt = labels[*deopt];
                self.gen_array_u16_index_assign(*using_xmm, *idx, deopt);
                self.handle_error(labels, *error);
            }
            AsmInst::ArrayIndexAssign {
                using_xmm,
                deopt,
                error,
            } => {
                let deopt = labels[*deopt];
                self.gen_array_index_assign(*using_xmm, deopt);
                self.handle_error(labels, *error);
            }

            AsmInst::NewArray(callid, using_xmm) => {
                self.new_array(*callid, *using_xmm);
            }
            AsmInst::NewHash(args, len, using_xmm) => {
                self.new_hash(*args, *len, *using_xmm);
            }
            AsmInst::NewRange {
                start,
                end,
                exclude_end,
                using_xmm,
                error,
            } => {
                self.load_rdi(*start);
                self.load_rsi(*end);
                self.new_range(*exclude_end, *using_xmm);
                self.handle_error(labels, *error);
            }

            AsmInst::BlockArgProxy { ret, outer } => {
                self.get_method_lfp(*outer);
                self.block_arg_proxy();
                self.store_rax(*ret);
            }
            AsmInst::BlockArg {
                ret,
                outer,
                using_xmm,
                error,
            } => {
                self.get_method_lfp(*outer);
                self.block_arg(*using_xmm);
                self.handle_error(labels, *error);
                self.store_rax(*ret);
            }
            AsmInst::LoadDynVar { src } => {
                self.get_outer(src.outer);
                let offset = conv(src.reg) - LBP_OUTER;
                monoasm!( &mut self.jit,
                    movq rax, [rax - (offset)];
                );
            }

            AsmInst::LoadIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
            } => {
                if *is_object_ty && *is_self_cached {
                    if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                        monoasm!( &mut self.jit,
                            movq rax, [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)];
                        );
                        // We must check whether the ivar slot is None.
                        monoasm!( &mut self.jit,
                            movq rdi, (NIL_VALUE);
                            testq rax, rax;
                            cmoveqq rax, rdi;
                        );
                    } else {
                        self.load_ivar_heap(*cached_ivarid, *is_object_ty);
                    }
                } else {
                    // ctx.self_class != cached_class merely happens, but possible.
                    self.xmm_save(*using_xmm);
                    monoasm!( &mut self.jit,
                        movq rsi, (name.get());  // id: IdentId
                        movq rdx, r12; // &mut Globals
                        movq rax, (ivar::get_instance_var);
                        call rax;
                    );
                    self.xmm_restore(*using_xmm);
                }
            }
            AsmInst::StoreIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
                error,
            } => {
                let exit = self.jit.label();
                if *is_self_cached {
                    if *is_object_ty && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                        monoasm!( &mut self.jit,
                            movq [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)], rax;
                        );
                    } else {
                        self.store_ivar_heap(*cached_ivarid, *is_object_ty, *using_xmm);
                    }
                } else {
                    self.xmm_save(*using_xmm);
                    monoasm!( &mut self.jit,
                        movq rdx, rdi;  // base: Value
                        movq rcx, (name.get());  // id: IdentId
                        movq r8, rax;   // val: Value
                        movq rdi, rbx; //&mut Executor
                        movq rsi, r12; //&mut Globals
                        movq rax, (ivar::set_instance_var);
                        call rax;
                    );
                    self.xmm_restore(*using_xmm);
                    self.handle_error(labels, *error);
                }
                self.jit.bind_label(exit);
            }

            AsmInst::StoreDynVar { dst, src } => {
                self.get_outer(dst.outer);
                let offset = conv(dst.reg) - LBP_OUTER;
                monoasm!( &mut self.jit,
                    movq [rax - (offset)], R(*src as _);
                );
            }
            AsmInst::LoadGVar { name, using_xmm } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, r12;
                    movl rsi, (name.get());
                    movq rax, (runtime::get_global_var);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }
            AsmInst::StoreGVar {
                name,
                src,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, r12;
                    movl rsi, (name.get());
                    movq rdx, [r14 - (conv(*src))];
                    movq rax, (runtime::set_global_var);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }

            AsmInst::ClassDef {
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_xmm,
                error,
            } => {
                self.xmm_save(*using_xmm);
                self.class_def(*superclass, *name, *is_module);
                self.handle_error(labels, *error);
                self.jit_class_def_sub(*func_id, *dst);
                self.handle_error(labels, *error);
                self.xmm_restore(*using_xmm);
            }
            AsmInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_xmm,
                error,
            } => {
                self.xmm_save(*using_xmm);
                self.singleton_class_def(*base);
                self.handle_error(labels, *error);
                self.jit_class_def_sub(*func_id, *dst);
                self.handle_error(labels, *error);
                self.xmm_restore(*using_xmm);
            }
            AsmInst::MethodDef {
                name,
                func_id,
                using_xmm,
            } => {
                self.method_def(*name, *func_id, *using_xmm);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
            } => {
                self.singleton_method_def(*obj, *name, *func_id, *using_xmm);
            }

            AsmInst::ExpandArray {
                dst,
                len,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm!( &mut self.jit,
                    lea rsi, [r14 - (conv(*dst))];
                    movq rdx, (*len);
                    movq rax, (runtime::expand_array);
                    call rax;
                );
                self.xmm_restore(*using_xmm);
            }
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_string(*arg, *len, *using_xmm);
            }
            AsmInst::ConcatRegexp {
                arg,
                len,
                using_xmm,
                error,
            } => {
                self.xmm_save(*using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    lea rdx, [r14 - (conv(*arg))];
                    movq rcx, (*len);
                    movq rax, (runtime::concatenate_regexp);
                    call rax;
                );
                self.xmm_restore(*using_xmm);
                self.handle_error(labels, *error);
            }
            AsmInst::AliasMethod {
                new,
                old,
                using_xmm,
                error,
            } => {
                self.xmm_save(*using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    movq rdx, [r14 - (LBP_SELF)];
                    movq rcx, [r14 - (conv(*new))];
                    movq r8, [r14 - (conv(*old))];
                    movq r9, [r14 - (LBP_META)];
                    movq rax, (runtime::alias_method);
                    call rax;
                );
                self.xmm_restore(*using_xmm);
                self.handle_error(labels, *error);
            }
            AsmInst::DefinedYield { dst, using_xmm } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, rbx;  // &mut Interp
                    movq rsi, r12;  // &mut Globals
                    lea  rdx, [r14 - (conv(*dst))];
                    movq rax, (runtime::defined_yield);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }
            AsmInst::DefinedConst {
                dst,
                siteid,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, rbx;  // &mut Interp
                    movq rsi, r12;  // &mut Globals
                    lea  rdx, [r14 - (conv(*dst))];
                    movl rcx, (siteid.0);
                    movq rax, (runtime::defined_const);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }
            AsmInst::DefinedMethod {
                dst,
                recv,
                name,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, rbx;  // &mut Interp
                    movq rsi, r12;  // &mut Globals
                    lea  rdx, [r14 - (conv(*dst))];
                    movq rcx, [r14 - (conv(*recv))];
                    movl r8, (name.get());
                    movq rax, (runtime::defined_method);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }
            AsmInst::DefinedGvar {
                dst,
                name,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, rbx;  // &mut Interp
                    movq rsi, r12;  // &mut Globals
                    lea  rdx, [r14 - (conv(*dst))];
                    movl rcx, (name.get());
                    movq rax, (runtime::defined_gvar);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }
            AsmInst::DefinedIvar {
                dst,
                name,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                monoasm! { &mut self.jit,
                    movq rdi, rbx;  // &mut Interp
                    movq rsi, r12;  // &mut Globals
                    lea  rdx, [r14 - (conv(*dst))];
                    movl rcx, (name.get());
                    movq rax, (runtime::defined_ivar);
                    call rax;
                };
                self.xmm_restore(*using_xmm);
            }

            AsmInst::GenericCondBr {
                brkind: kind,
                branch_dest: dst,
            } => {
                self.cond_br(*dst, *kind);
            }
        }
    }

    fn handle_error(&mut self, labels: &[DestLabel], error: usize) {
        let error = labels[error];
        monoasm! { &mut self.jit,
            testq rax, rax;
            jeq   error;
        }
    }

    ///
    /// Get method lfp.
    ///
    /// ### in
    /// - r14: lfp
    ///
    /// ### out
    /// - rax: method lfp
    ///
    fn get_method_lfp(&mut self, outer: usize) {
        if outer == 0 {
            monoasm! { &mut self.jit,
                movq rax, r14;
            };
        } else {
            monoasm!( &mut self.jit,
                movq rax, [r14 - (LBP_OUTER)];
            );
            for _ in 0..outer - 1 {
                monoasm!( &mut self.jit,
                    movq rax, [rax];
                );
            }
            monoasm!( &mut self.jit,
                lea rax, [rax + (LBP_OUTER)];
            );
        }
    }

    fn get_outer(&mut self, outer: usize) {
        monoasm!( &mut self.jit,
            movq rax, [r14 - (LBP_OUTER)];
        );
        for _ in 0..outer - 1 {
            monoasm!( &mut self.jit,
                movq rax, [rax];
            );
        }
    }
}

impl Codegen {
    fn new_array(&mut self, callid: CallSiteId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movl rdx, (callid.get());
            lea  rcx, [r14 - (LBP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_array);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            lea  rdi, [r14 - (conv(args))];
            movq rsi, (len);
            movq rax, (runtime::gen_hash);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_range(&mut self, exclude_end: bool, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rbx; // &mut Executor
            movq rcx, r12; // &mut Globals
            movl r8, (exclude_end as u32);
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    fn concat_string(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, r12;
            lea rsi, [r14 - (conv(arg))];
            movq rdx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn gen_generic_binop(
        &mut self,
        kind: BinOpK,
        labels: &[DestLabel],
        using_xmm: UsingXmm,
        error: usize,
    ) {
        let func = kind.generic_func();
        self.xmm_save(using_xmm);
        self.call_binop(func);
        self.xmm_restore(using_xmm);
        self.handle_error(labels, error);
    }

    ///
    /// Get block handler of a current method frame.
    ///
    /// ### in
    /// - rax: method lfp
    ///
    /// ### out
    /// - rax: block handler
    ///
    fn block_arg_proxy(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rax - (LBP_BLOCK)];
            xorq rdi, rdi;
            movq rsi, 0b10;
            testq rax, 0b1;
            cmovneq rdi, rsi;
            addq rax, rdi;
        };
    }

    ///
    /// Get a block argument of current frame.
    ///
    fn block_arg(&mut self, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, [rax - (LBP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    fn class_def(&mut self, superclass: SlotId, name: IdentId, is_module: bool) {
        // rcx <- superclass: Option<Value>
        if superclass.is_zero() {
            monoasm! { &mut self.jit,
                xorq rcx, rcx;
            }
        } else {
            monoasm! { &mut self.jit,
                movq rcx, [r14 - (conv(superclass))];
            }
        }
        // r8 <- is_module
        if is_module {
            monoasm! { &mut self.jit,
                movl r8, 1;
            }
        } else {
            monoasm! { &mut self.jit,
                xorq r8, r8;
            }
        }
        monoasm! { &mut self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value
        };
    }

    fn singleton_class_def(&mut self, base: SlotId) {
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_singleton_class);
            call rax;  // rax <- self: Value
        };
    }

    fn method_def(&mut self, name: IdentId, func_id: FuncId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq rax, (runtime::define_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn singleton_method_def(
        &mut self,
        obj: SlotId,
        name: IdentId,
        func_id: FuncId,
        using_xmm: UsingXmm,
    ) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq r8, [r14 - (conv(obj))];
            movq rax, (runtime::singleton_define_method);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }
}

impl BBContext {
    pub(super) fn array_index(
        &mut self,
        ir: &mut AsmIr,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        self.fetch_to_reg(ir, base, GP::Rdi);

        let deopt = ir.new_deopt(pc, self.get_write_back());
        if let Some(idx) = self.is_u16_literal(idx) {
            ir.inst.push(AsmInst::ArrayU16Index { idx, deopt });
        } else {
            self.fetch_to_reg(ir, idx, GP::Rsi);
            ir.inst.push(AsmInst::ArrayIndex { deopt });
        }
        self.release(dst);
    }

    pub(super) fn array_index_assign(
        &mut self,
        ir: &mut AsmIr,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        self.writeback_acc(ir);
        self.fetch_to_reg(ir, base, GP::Rdi);
        self.fetch_to_reg(ir, src, GP::R15);

        if let Some(idx) = self.is_u16_literal(idx) {
            ir.array_u16_index_assign(self, pc, idx);
        } else {
            self.fetch_to_reg(ir, idx, GP::Rsi);
            ir.array_index_assign(self, pc);
        }
    }
}
