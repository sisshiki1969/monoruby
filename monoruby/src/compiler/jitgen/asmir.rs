use crate::bytecodegen::BinOpK;

use super::*;

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

    pub(super) fn rax2acc(&mut self, ctx: &mut BBContext, dst: SlotId) {
        ctx.clear();
        if let Some(acc) = ctx.clear_r15() {
            if acc != dst {
                self.inst.push(AsmInst::AccToStack(acc));
            }
        }
        ctx.link_r15(dst);
        self.inst.push(AsmInst::RegToAcc(GP::Rax));
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

    pub(super) fn acc2stack(&mut self, reg: SlotId) {
        self.inst.push(AsmInst::AccToStack(reg));
    }

    pub(super) fn int2xmm(&mut self, ctx: &BBContext, pc: BcPc, reg: Option<SlotId>, x: Xmm) {
        let label = self.new_deopt(pc, ctx.get_write_back());
        self.inst.push(AsmInst::IntToXmm(reg, x, label));
    }

    pub(super) fn float2xmm(&mut self, ctx: &BBContext, pc: BcPc, reg: Option<SlotId>, x: Xmm) {
        let label = self.new_deopt(pc, ctx.get_write_back());
        self.inst.push(AsmInst::FloatToXmm(reg, x, label));
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

    /// move f64 to xmm
    F64ToXmm(f64, Xmm),
    /// move i64 to both of xmm and a stack slot
    I64ToBoth(i64, SlotId, Xmm),
    XmmToBoth(Xmm, Vec<SlotId>),
    LitToStack(Value, SlotId),
    DeepCopyLit(Value, UsingXmm),
    NumToXmm(SlotId, Xmm, usize),
    IntToXmm(Option<SlotId>, Xmm, usize),
    /// move a Flonum Value in a stack slot or acc to xmm, and deoptimize if it is not a Flonum.
    FloatToXmm(Option<SlotId>, Xmm, usize),

    /// check whether a Value in a stack slot is a Flonum, and if not, deoptimize.
    GuardFloat(SlotId, usize),
    /// deoptimize
    Deopt(usize),
    /// recompile and deoptimize
    RecompileDeopt {
        position: Option<BcPc>,
        deopt: usize,
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
    /// %dst = DynVar(src)
    LoadDynVar {
        dst: SlotId,
        src: DynVar,
    },
    /// DynVar(dst) = src
    StoreDynVar {
        dst: DynVar,
        src: GP,
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
}

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
            } => match mode {
                FMode::RR(l, r) => self.gen_binop_float_rr(*kind, *using_xmm, *dst, *l, *r),
                FMode::RI(l, r) => self.gen_binop_float_ri(*kind, *using_xmm, *dst, *l, *r),
                FMode::IR(l, r) => self.gen_binop_float_ir(*kind, *using_xmm, *dst, *l, *r),
            },

            AsmInst::NumToXmm(r, x, side_exit) => {
                self.load_rdi(*r);
                self.numeric_val_to_f64(x.enc(), labels[*side_exit]);
            }
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(*f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::IntToXmm(r, x, side_exit) => {
                if let Some(r) = r {
                    self.load_rdi(*r);
                } else {
                    monoasm! {&mut self.jit,
                        movq rdi, r15;
                    }
                }
                self.integer_val_to_f64(x.enc(), labels[*side_exit]);
            }
            AsmInst::FloatToXmm(r, x, side_exit) => {
                if let Some(r) = r {
                    self.load_rdi(*r);
                } else {
                    monoasm! {&mut self.jit,
                        movq rdi, r15;
                    }
                }
                self.float_to_f64(x.enc(), labels[*side_exit]);
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

            AsmInst::GuardFloat(r, side_exit) => self.slot_guard_float(*r, labels[*side_exit]),
            AsmInst::Deopt(side_exit) => {
                let exit = labels[*side_exit];
                monoasm!( &mut self.jit,
                    jmp exit;
                );
            }
            AsmInst::RecompileDeopt { position, deopt } => {
                let deopt = labels[*deopt];
                self.recompile_and_deopt(*position, deopt)
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
                let out_range = self.jit.label();
                monoasm! { &mut self.jit,
                    movl rsi, (*idx);
                }
                self.guard_rdi_array(labels[*deopt]);
                self.array_index(out_range);
            }
            AsmInst::ArrayIndex { deopt } => {
                let deopt = labels[*deopt];
                let out_range = self.jit.label();
                let exit = self.jit.label();
                self.array_bound_check(deopt);
                monoasm! { &mut self.jit,
                    jge  exit;
                }
                self.get_array_length();
                monoasm! { &mut self.jit,
                    addq rsi, rax;
                    js   out_range;
                exit:
                }
                self.guard_rdi_array(deopt);
                self.array_index(out_range);
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
                let generic = self.jit.label();
                let deopt = labels[*deopt];
                monoasm! { &mut self.jit,
                    movl rsi, (*idx);
                }
                self.guard_rdi_array(deopt);
                self.array_index_assign(*using_xmm, generic);
                self.handle_error(labels, *error);
            }
            AsmInst::ArrayIndexAssign {
                using_xmm,
                deopt,
                error,
            } => {
                let generic = self.jit.label();
                let deopt = labels[*deopt];
                self.array_bound_check(deopt);
                monoasm! { &mut self.jit,
                    jlt generic;
                };
                self.guard_rdi_array(deopt);
                self.array_index_assign(*using_xmm, generic);
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
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_string(*arg, *len, *using_xmm);
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
            AsmInst::LoadDynVar { dst, src } => {
                monoasm!( &mut self.jit,
                    movq rax, [r14 - (LBP_OUTER)];
                );
                for _ in 0..src.outer - 1 {
                    monoasm!( &mut self.jit,
                        movq rax, [rax];
                    );
                }
                let offset = conv(src.reg) - LBP_OUTER;
                monoasm!( &mut self.jit,
                    movq rax, [rax - (offset)];
                );
                if !dst.is_zero() {
                    self.store_rax(*dst);
                }
            }
            AsmInst::StoreDynVar { dst, src } => {
                monoasm!( &mut self.jit,
                    movq rax, [r14 - (LBP_OUTER)];
                );
                for _ in 0..dst.outer - 1 {
                    monoasm!( &mut self.jit,
                        movq rax, [rax];
                    );
                }
                let offset = conv(dst.reg) - LBP_OUTER;
                monoasm!( &mut self.jit,
                    movq [rax - (offset)], R(*src as _);
                );
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
                self.xmm_save(*using_xmm);
                self.method_def(*name, *func_id);
                self.xmm_restore(*using_xmm);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
            } => {
                self.xmm_save(*using_xmm);
                self.singleton_method_def(*obj, *name, *func_id);
                self.xmm_restore(*using_xmm);
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

    fn method_def(&mut self, name: IdentId, func_id: FuncId) {
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq rax, (runtime::define_method);
            call rax;
        );
    }

    fn singleton_method_def(&mut self, obj: SlotId, name: IdentId, func_id: FuncId) {
        monoasm!( &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &Globals
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (u32::from(func_id)); // FuncId
            movq r8, [r14 - (conv(obj))];
            movq rax, (runtime::singleton_define_method);
            call rax;
        );
    }
}
