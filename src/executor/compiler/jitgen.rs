use monoasm_macro::monoasm;

use super::*;

mod analysis;
mod compile;

//
// Just-in-time compiler module.
//

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
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BBContext {
    /// information for stack slots.
    stack_slot: StackSlotInfo,
    /// information for xmm registers.
    xmm: [Vec<SlotId>; 14],
}

#[derive(Clone, PartialEq)]
struct StackSlotInfo(Vec<LinkMode>);

impl std::fmt::Debug for StackSlotInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .0
            .iter()
            .enumerate()
            .flat_map(|(i, mode)| match mode {
                LinkMode::None => None,
                LinkMode::XmmR(x) => Some(format!("%{i}:R({}) ", x)),
                LinkMode::XmmRW(x) => Some(format!("%{i}:RW({}) ", x)),
            })
            .collect();
        write!(f, "[{s}]")
    }
}

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
    fn merge(&mut self, other: &StackSlotInfo) {
        self.0.iter_mut().zip(other.0.iter()).for_each(|(l, r)| {
            *l = match (&l, r) {
                (LinkMode::XmmR(l), LinkMode::XmmR(_) | LinkMode::XmmRW(_))
                | (LinkMode::XmmRW(l), LinkMode::XmmR(_)) => LinkMode::XmmR(*l),
                (LinkMode::XmmRW(l), LinkMode::XmmRW(_)) => LinkMode::XmmRW(*l),
                _ => LinkMode::None,
            };
        });
    }

    fn merge_entries(entries: &Vec<BranchEntry>) -> Self {
        let mut target = entries[0].bbctx.stack_slot.clone();
        #[cfg(feature = "emit-tir")]
        eprintln!("  <-{}: {:?}", entries[0].src_idx, target);
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            dest_label: _,
        } in entries.iter().skip(1)
        {
            #[cfg(feature = "emit-tir")]
            eprintln!("  <-{_src_idx}: {:?}", bbctx.stack_slot);
            target.merge(&bbctx.stack_slot);
        }
        target
    }
}

type WriteBack = Vec<(u16, Vec<SlotId>)>;
type UsingXmm = Vec<usize>;

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
        }
    }

    fn from(slot_info: &StackSlotInfo) -> Self {
        let mut ctx = Self::new(slot_info.0.len());
        for (i, mode) in slot_info.0.iter().enumerate() {
            let reg = SlotId(i as u16);
            match mode {
                LinkMode::None => {}
                LinkMode::XmmR(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmR(*x);
                    ctx.xmm[*x as usize].push(reg);
                }
                LinkMode::XmmRW(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmRW(*x);
                    ctx.xmm[*x as usize].push(reg);
                }
            }
        }
        ctx
    }

    fn remove_unused(&mut self, unused: &Vec<SlotId>) {
        unused.iter().for_each(|reg| self.dealloc_xmm(*reg));
    }

    ///
    /// Allocate a new xmm register.
    ///
    fn alloc_xmm(&mut self) -> u16 {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return flhs as u16;
            }
        }
        unreachable!()
    }

    fn link_rw_xmm(&mut self, reg: SlotId, freg: u16) {
        self.stack_slot[reg] = LinkMode::XmmRW(freg as u16);
        self.xmm[freg as usize].push(reg);
    }

    fn link_r_xmm(&mut self, reg: SlotId, freg: u16) {
        self.stack_slot[reg] = LinkMode::XmmR(freg as u16);
        self.xmm[freg as usize].push(reg);
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
            LinkMode::None => {}
        }
    }

    fn xmm_swap(&mut self, l: u16, r: u16) {
        self.xmm.swap(l as usize, r as usize);
        self.stack_slot.0.iter_mut().for_each(|mode| match mode {
            LinkMode::XmmR(x) | LinkMode::XmmRW(x) => {
                if *x == l {
                    *x = r;
                } else if *x == r {
                    *x = l;
                }
            }
            LinkMode::None => {}
        });
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
        let freg = self.alloc_xmm();
        self.link_rw_xmm(reg, freg);
        freg
    }

    ///
    /// Allocate new xmm register to the given stack slot for read f64.
    ///
    fn alloc_xmm_read(&mut self, reg: SlotId) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::None => {
                let freg = self.alloc_xmm();
                self.link_r_xmm(reg, freg);
                freg
            }
            _ => unreachable!(),
        }
    }

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, codegen: &mut Codegen, src: SlotId, dst: SlotId) {
        self.dealloc_xmm(dst);
        match self.stack_slot[src] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                self.link_rw_xmm(dst, freg);
            }
            LinkMode::None => {
                monoasm!(codegen.jit,
                  movq rax, [rbp - (conv(src))];
                  movq [rbp - (conv(dst))], rax;
                );
            }
        }
    }

    ///
    /// Write back a corresponding xmm register to the stack slot *reg*.
    ///
    /// the xmm will be deallocated.
    ///
    fn read_slot(&mut self, codegen: &mut Codegen, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
            );
            codegen.store_rax(reg);
            self.stack_slot[reg] = LinkMode::XmmR(freg);
        }
    }

    fn write_back_range(&mut self, codegen: &mut Codegen, arg: SlotId, len: u16) {
        for reg in arg.0..arg.0 + len {
            self.read_slot(codegen, SlotId::new(reg))
        }
    }

    fn get_write_back(&self) -> WriteBack {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| matches!(self.stack_slot[**reg], LinkMode::XmmRW(_)))
                        .cloned()
                        .collect();
                    Some((i as u16, v))
                }
            })
            .filter(|(_, v)| !v.is_empty())
            .collect()
    }

    fn get_xmm_using(&self) -> Vec<usize> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i) })
            .collect()
    }
}

#[derive(Debug)]
struct BranchEntry {
    src_idx: usize,
    bbctx: BBContext,
    dest_label: DestLabel,
}

struct CompileContext {
    labels: HashMap<usize, DestLabel>,
    /// (bb_id, Vec<src_idx>)
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    bb_pos: usize,
    loop_count: usize,
    is_loop: bool,
    branch_map: HashMap<usize, Vec<BranchEntry>>,
    backedge_map: HashMap<usize, (DestLabel, StackSlotInfo, Vec<SlotId>)>,
    start_codepos: usize,
    #[cfg(feature = "emit-asm")]
    sourcemap: Vec<(usize, usize)>,
}

impl CompileContext {
    fn new(func: &RubyFuncInfo, codegen: &mut Codegen, start_pos: usize, is_loop: bool) -> Self {
        let bb_info = func.get_bb_info();
        let mut labels = HashMap::default();
        bb_info.into_iter().enumerate().for_each(|(idx, elem)| {
            if elem.is_some() {
                labels.insert(idx, codegen.jit.label());
            }
        });
        Self {
            labels,
            bb_info: func.get_bb_info(),
            bb_pos: start_pos,
            loop_count: 0,
            is_loop,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            start_codepos: 0,
            #[cfg(feature = "emit-asm")]
            sourcemap: vec![],
        }
    }

    fn new_branch(&mut self, src_idx: usize, dest: usize, bbctx: BBContext, dest_label: DestLabel) {
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            dest_label,
        })
    }

    fn new_backedge(
        &mut self,
        bb_pos: usize,
        dest_label: DestLabel,
        slot_info: StackSlotInfo,
        unused: Vec<SlotId>,
    ) {
        self.backedge_map
            .insert(bb_pos, (dest_label, slot_info, unused));
    }

    fn get_backedge(&mut self, bb_pos: usize) -> (DestLabel, StackSlotInfo, Vec<SlotId>) {
        self.backedge_map.remove_entry(&bb_pos).unwrap().1
    }
}

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            pub(crate) fn [<cmp_ $op>](&mut self, generic:DestLabel, xmm_using: Vec<usize>) {
                let exit = self.jit.label();
                monoasm! { self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                exit:
                };
                self.jit.select_page(1);
                monoasm!(self.jit,
                    generic:
                );
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    jmp  exit;
                );
                self.jit.select_page(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_main!($op1);
        cmp_main!($($op2),+);
    };
}

macro_rules! cmp_opt_main {
    (($op:ident, $rev_op:ident, $sop:ident, $rev_sop:ident)) => {
        paste! {
            fn [<cmp_opt_int_ $sop>](&mut self, branch_dest: DestLabel, generic:DestLabel, brkind: BrKind, xmm_using: Vec<usize>) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $rev_sop>] branch_dest;
                    },
                }
                self.jit.bind_label(cont);
                self.jit.select_page(1);
                self.jit.bind_label(generic);
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
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        jz  cont;
                        jmp branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        jnz  cont;
                        jmp branch_dest;
                    },
                }
                self.jit.select_page(0);
            }

            fn [<cmp_opt_float_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $rev_op>] branch_dest;
                    },
                }
                self.jit.bind_label(cont);
            }
        }
    };
    (($op1:ident, $rev_op1:ident, $sop1:ident, $rev_sop1:ident), $(($op2:ident, $rev_op2:ident, $sop2:ident, $rev_sop2:ident)),+) => {
        cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Value,
) {
    let name = match globals.func[func_id].as_ruby_func().name() {
        Some(name) => name.to_string(),
        None => "<unnamed>".to_string(),
    };
    let bc_begin = globals.func[func_id].as_ruby_func().get_bytecode_address(0);
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let BcOp::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else if let BcOp::ClassDef { .. } = pc.op1() {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {} caused by {:?}", index, fmt, v);
    }
}

struct Cached {
    codeptr: CodePtr,
    meta: Meta,
    class_id: ClassId,
    version: u32,
    pc: BcPc,
}

impl Codegen {
    fn xmm_read_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    fn xmm_read_assume_float(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_float(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_integer(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (u16, u16) {
        if lhs != rhs {
            (
                self.xmm_read_assume(ctx, lhs, pc.classid1(), pc),
                self.xmm_read_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.xmm_read_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }
}

impl Codegen {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - rdi: Value
    ///
    /// ### registers destroyed
    ///
    /// - rax
    ///
    fn guard_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz side_exit;
                    testq rdi, 0b010;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (NIL_VALUE);
                    jnz side_exit;
                );
            }
            SYMBOL_CLASS => {
                monoasm!(self.jit,
                    cmpb rdi, (TAG_SYMBOL);
                    jnz side_exit;
                );
            }
            TRUE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (TRUE_VALUE);
                    jnz side_exit;
                );
            }
            FALSE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (FALSE_VALUE);
                    jnz side_exit;
                );
            }
            _ => self.guard_unpacked_class(class_id, side_exit),
        }
    }

    fn guard_unpacked_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        monoasm!(self.jit,
            testq rdi, 0b111;
            jnz side_exit;
            movl rax, [rdi + 4];    // rdi <- ClassId
            cmpl rax, (class_id.get());
            jne side_exit;
        )
    }

    fn guard_version(&mut self, cached_version: u32, side_exit: DestLabel) {
        let global_class_version = self.class_version;
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl rax, (cached_version);
            jne side_exit;
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;

        for (class, value) in [
            (INTEGER_CLASS, Value::new_integer(-2558)),
            (INTEGER_CLASS, Value::new_integer(i64::MAX)),
            (INTEGER_CLASS, Value::new_integer(i64::MIN)),
            (FLOAT_CLASS, Value::new_float(1.44e-17)),
            (FLOAT_CLASS, Value::new_float(0.0)),
            (FLOAT_CLASS, Value::new_float(f64::MAX)),
            (FLOAT_CLASS, Value::new_float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
        ] {
            let entry_point = gen.jit.get_current_address();
            gen.guard_class(class, side_exit);
            monoasm!(gen.jit,
                xorq rax, rax;
                ret;
            );
            gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
    }
}

impl Codegen {
    cmp_opt_main!(
        (eq, ne, eq, ne),
        (ne, eq, ne, eq),
        (a, be, gt, le),
        (b, ae, lt, ge),
        (ae, b, ge, lt),
        (be, a, le, gt)
    );
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

    fn handle_error(&mut self, pc: BcPc) {
        let jit_return = self.vm_return;
        monoasm!(self.jit,
            movq r13, ((pc + 1).get_u64());
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
    }

    fn load_constant(&mut self, dst: SlotId, id: ConstSiteId, pc: BcPc, ctx: &BBContext) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.jit_get_constant(id, pc, ctx);
        monoasm!(self.jit,
            movq [rip + cached_value], rax;
            movq rdi, [rip + global_const_version];
            movq [rip + cached_const_version], rdi;
            jmp  exit;
        );
        self.jit.select_page(0);

        monoasm!(self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  slow_path;
            movq rax, [rip + cached_value];
        exit:
        );
        self.store_rax(dst);
    }

    fn load_float_constant(
        &mut self,
        dst: SlotId,
        fdst: u16,
        id: ConstSiteId,
        pc: BcPc,
        ctx: &BBContext,
    ) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        let cached_float = self.jit.const_f64(0.0);
        let side_exit = self.gen_side_deopt_dest(pc, ctx);

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.jit_get_constant(id, pc, ctx);
        monoasm!(self.jit,
            movq [rip + cached_value], rax;
            movq rdi, rax;
        );
        self.gen_val_to_f64_assume_float(0, side_exit);
        monoasm!(self.jit,
            movq [rip + cached_float], xmm0;
            movq rax, [rip + global_const_version];
            movq [rip + cached_const_version], rax;
            jmp  exit;
        );
        self.jit.select_page(0);

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

    fn jit_get_constant(&mut self, id: ConstSiteId, pc: BcPc, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, (id.get());  // name: ConstSiteId
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (get_constant);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
    }

    fn jit_store_constant(&mut self, id: IdentId, src: SlotId, ctx: &BBContext) {
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

    fn jit_load_ivar(&mut self, id: IdentId, ret: SlotId, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
          movq rdi, [rbp - 16];  // base: Value
          movq rsi, (id.get());  // id: IdentId
          movq rdx, r12; // &mut Globals
          movq rax, (get_instance_var);
          call rax;
        );
        self.xmm_restore(&xmm_using);
        self.store_rax(ret);
    }

    fn jit_store_ivar(&mut self, id: IdentId, src: SlotId, ctx: &BBContext, pc: BcPc) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
          movq rdi, r12; //&mut Globals
          movq rsi, [rbp - 16];  // base: Value
          movq rdx, (id.get());  // id: IdentId
          movq rcx, [rbp - (conv(src))];   // val: Value
          movq rax, (set_instance_var);
          call rax;
        );
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
    }

    fn jit_get_index(&mut self, ret: SlotId, base: SlotId, idx: SlotId, pc: BcPc, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { self.jit,
            movq rdx, [rbp - (conv(base))]; // base: Value
            movq rcx, [rbp - (conv(idx))]; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (get_index);
            call rax;
        };
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.store_rax(ret);
    }

    fn jit_index_assign(
        &mut self,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        ctx: &BBContext,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { self.jit,
            movq rdx, [rbp - (conv(base))]; // base: Value
            movq rcx, [rbp - (conv(idx))]; // idx: Value
            movq r8, [rbp - (conv(src))];  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (set_index);
            call rax;
        };
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn gen_write_back(&mut self, wb: WriteBack) {
        for (freg, v) in wb {
            self.gen_write_back_single(freg, v);
        }
    }

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        pc: BcPc,
    ) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.stack_slot);
            eprintln!("      target: {:?}", target_ctx.stack_slot);
        }
        let len = src_ctx.stack_slot.0.len();

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx.stack_slot[reg] == LinkMode::None {
                match src_ctx.stack_slot[reg] {
                    LinkMode::XmmRW(freg) => {
                        let v = src_ctx.xmm[freg as usize].clone();
                        for i in &v {
                            src_ctx.stack_slot[*i] = LinkMode::XmmR(freg);
                        }
                        src_ctx.dealloc_xmm(reg);
                        self.gen_write_back_single(freg, v);
                    }
                    LinkMode::XmmR(_) => {
                        src_ctx.dealloc_xmm(reg);
                    }
                    _ => {}
                }
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (LinkMode::XmmRW(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                    guard_list.push(reg);
                }
                (_, LinkMode::None) => {}
                (LinkMode::XmmRW(l), LinkMode::XmmR(r)) => {
                    self.gen_write_back_single(l, vec![reg]);
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmR(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::None, LinkMode::XmmR(r)) => {
                    src_ctx.link_r_xmm(reg, r);
                    conv_list.push((reg, r));
                }
                _ => unreachable!(),
            }
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      src_end:   {:?}", src_ctx.stack_slot);

        let side_exit = self.gen_side_deopt_dest(pc + 1, &src_ctx);
        for (reg, freg) in conv_list {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(reg))];
            );
            self.gen_val_to_f64(freg as u64 + 2, side_exit);
            #[cfg(feature = "emit-tir")]
            eprintln!("      conv: {:?}->{:?}", reg, freg);
        }
        for reg in guard_list {
            self.gen_assume_float(reg, side_exit);
        }
    }

    fn gen_write_back_single(&mut self, freg: u16, v: Vec<SlotId>) {
        if v.len() == 0 {
            return;
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!(self.jit,
            movq xmm0, xmm(freg as u64 + 2);
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(reg);
        }
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    fn gen_side_deopt_dest(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let wb = ctx.get_write_back();
        let old_p = self.jit.get_page();
        self.jit.select_page(2);
        let entry = self.jit.label();
        self.jit.bind_label(entry);
        if wb.len() != 0 {
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt");
            self.gen_write_back(wb);
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt end");
        }
        let fetch = self.vm_fetch;
        monoasm!(self.jit,
            movq r13, (pc.get_u64());
        );
        #[cfg(feature = "log-jit")]
        monoasm!(self.jit,
            movq r8, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - 8];
            movq rcx, r13;
            movq rax, (log_deoptimize);
            call rax;
        );
        monoasm!(self.jit,
            jmp fetch;
        );
        self.jit.select_page(old_p);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_deopt_dest(pc, ctx);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    pub(super) fn jit_compile_ruby(
        &mut self,
        globals: &Globals,
        func_id: FuncId,
        position: Option<usize>,
    ) -> DestLabel {
        let func = globals.func[func_id].as_ruby_func();
        let start_pos = position.unwrap_or_default();

        #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
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
        self.jit.bind_label(entry);

        let mut cc = CompileContext::new(func, self, start_pos, position.is_some());
        let bb_start_pos: Vec<_> = cc
            .bb_info
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| match v {
                Some(_) => {
                    if idx >= start_pos {
                        Some(idx)
                    } else {
                        None
                    }
                }
                None => None,
            })
            .collect();
        let reg_num = func.total_reg_num();
        cc.start_codepos = self.jit.get_current();

        if position.is_none() {
            self.prologue(func.total_reg_num(), func.total_arg_num());
        }

        cc.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: 0,
                bbctx: BBContext::new(reg_num),
                dest_label: self.jit.label(),
            }],
        );
        for i in bb_start_pos {
            cc.bb_pos = i;
            if self.compile_bb(globals, func, &mut cc) {
                break;
            };
        }

        let keys: Vec<_> = cc.branch_map.keys().cloned().collect();
        for pos in keys.into_iter() {
            self.gen_backedge_branch(&mut cc, func, pos);
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let elapsed = now.elapsed();
        //#[cfg(feature = "emit-tir")]
        //eprintln!("{:?}", cc.tir);
        #[cfg(any(feature = "emit-asm"))]
        {
            let (start, code_end, end) = self.jit.code_block.last().unwrap();
            eprintln!(
                "offset:{:?} code: {} bytes  data: {} bytes",
                start,
                *code_end - *start,
                *end - *code_end
            );
            self.jit.select_page(0);
            let dump: Vec<(usize, String)> = self
                .jit
                .dump_code()
                .unwrap()
                .split('\n')
                .filter(|s| s.len() >= 29)
                .map(|x| {
                    (
                        usize::from_str_radix(&x[0..4].trim(), 16).unwrap(),
                        x[28..].to_string(),
                    )
                })
                .collect();
            for (i, text) in dump {
                cc.sourcemap
                    .iter()
                    .filter_map(
                        |(bc_pos, code_pos)| {
                            if *code_pos == i {
                                Some(*bc_pos)
                            } else {
                                None
                            }
                        },
                    )
                    .for_each(|bc_pos| {
                        let pc = BcPc::from(&func.bytecode()[bc_pos]);
                        eprintln!(
                            ":{:05} {}",
                            bc_pos,
                            match pc.format(globals, bc_pos) {
                                Some(s) => s,
                                None => "".to_string(),
                            }
                        );
                    });

                eprintln!("  {:05x}: {}", i, text);
            }
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("    finished compile. elapsed:{:?}", elapsed);
        #[cfg(feature = "emit-tir")]
        eprintln!("    finished compile.");

        entry
    }

    fn prologue(&mut self, regs: usize, args: usize) {
        let offset = (regs + regs % 2) * 8 + 16;
        let clear_len = regs - args;
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (offset);
        );
        //
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
        //       |     %3      |
        //       +-------------+
        // rsp-> |             |
        //       +-------------+
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + 16)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + 16)], (NIL_VALUE);
                );
            }
        }
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
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
        mode: BinOpMode,
        ctx: &BBContext,
    ) {
        let xmm_using = ctx.get_xmm_using();
        let deopt = self.gen_side_deopt_dest(pc, ctx);
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
                self.generic_binop(ret, mul_values as _, xmm_using, pc);
            }
            BinOpK::Div => {
                self.load_binary_args_with_mode(&mode);
                self.generic_binop(ret, div_values as _, xmm_using, pc);
            }
            _ => {
                let generic = self.jit.label();
                self.load_binary_args_with_mode(&mode);
                self.guard_binary_fixnum_with_mode(generic, mode);
                match kind {
                    BinOpK::BitOr => self.gen_bit_or(generic, ret, xmm_using, pc),
                    BinOpK::BitAnd => self.gen_bit_and(generic, ret, xmm_using, pc),
                    BinOpK::BitXor => self.gen_bit_xor(generic, ret, xmm_using, pc),
                    BinOpK::Shr => self.gen_shr(generic, ret, xmm_using, pc),
                    BinOpK::Shl => self.gen_shl(generic, ret, xmm_using, pc),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn gen_binop_float(&mut self, kind: BinOpK, fret: u16, flhs: u16, frhs: u16) {
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
    }

    fn gen_binop_float_ri(&mut self, kind: BinOpK, fret: u16, flhs: u16, rhs: i16) {
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
    }

    fn gen_binop_float_ir(&mut self, kind: BinOpK, fret: u16, lhs: i16, frhs: u16) {
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
    }

    fn gen_binop_kind(&mut self, ctx: &BBContext, pc: BcPc, kind: BinOpK, ret: SlotId) {
        let xmm_using = ctx.get_xmm_using();
        self.generic_binop(ret, kind.generic_func() as _, xmm_using, pc);
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

    fn gen_cmp_prep(&mut self, lhs: SlotId, rhs: SlotId, generic: DestLabel) {
        self.load_binary_args(lhs, rhs);
        self.guard_rdi_rsi_fixnum(generic);
    }

    fn gen_cmpri_prep(&mut self, lhs: SlotId, rhs: i16, generic: DestLabel) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, (Value::new_integer(rhs as i64).get());
        );
        self.guard_rdi_fixnum(generic);
    }

    fn gen_cmp_kind(&mut self, kind: CmpKind, generic: DestLabel, ret: SlotId, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        match kind {
            CmpKind::Eq => self.cmp_eq(generic, xmm_using),
            CmpKind::Ne => self.cmp_ne(generic, xmm_using),
            CmpKind::Ge => self.cmp_ge(generic, xmm_using),
            CmpKind::Gt => self.cmp_gt(generic, xmm_using),
            CmpKind::Le => self.cmp_le(generic, xmm_using),
            CmpKind::Lt => self.cmp_lt(generic, xmm_using),
            _ => unimplemented!(),
        }
        self.store_rax(ret);
    }

    fn gen_cmp_int_opt(
        &mut self,
        kind: CmpKind,
        branch_dest: DestLabel,
        generic: DestLabel,
        brkind: BrKind,
        xmm_using: Vec<usize>,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_int_eq(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ne => self.cmp_opt_int_ne(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ge => self.cmp_opt_int_ge(branch_dest, generic, brkind, xmm_using),
            CmpKind::Gt => self.cmp_opt_int_gt(branch_dest, generic, brkind, xmm_using),
            CmpKind::Le => self.cmp_opt_int_le(branch_dest, generic, brkind, xmm_using),
            CmpKind::Lt => self.cmp_opt_int_lt(branch_dest, generic, brkind, xmm_using),
            _ => unimplemented!(),
        }
    }

    fn gen_cmp_float_opt(&mut self, kind: CmpKind, branch_dest: DestLabel, brkind: BrKind) {
        match kind {
            CmpKind::Eq => self.cmp_opt_float_eq(branch_dest, brkind),
            CmpKind::Ne => self.cmp_opt_float_ne(branch_dest, brkind),
            CmpKind::Ge => self.cmp_opt_float_ge(branch_dest, brkind),
            CmpKind::Gt => self.cmp_opt_float_gt(branch_dest, brkind),
            CmpKind::Le => self.cmp_opt_float_le(branch_dest, brkind),
            CmpKind::Lt => self.cmp_opt_float_lt(branch_dest, brkind),
            _ => unimplemented!(),
        }
    }

    fn gen_bit_or(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            orq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitor_values as _, xmm_using, pc);
    }

    fn gen_bit_and(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            andq rdi, rsi;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitand_values as _, xmm_using, pc);
    }

    fn gen_bit_xor(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
        monoasm!(self.jit,
            // fastpath
            xorq rdi, rsi;
            addq rdi, 1;
        );
        self.store_rdi(ret);
        self.side_generic_op(generic, ret, bitxor_values as _, xmm_using, pc);
    }

    fn shift_under(&mut self, under: DestLabel, after: DestLabel) {
        self.jit.select_page(1);
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
        self.jit.select_page(0);
    }

    fn gen_shr(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
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
        self.side_generic_op(generic, ret, shr_values as _, xmm_using, pc);
        self.jit.select_page(1);
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
        self.jit.select_page(0);
        self.shift_under(under, after);
    }

    fn gen_shl(&mut self, generic: DestLabel, ret: SlotId, xmm_using: UsingXmm, pc: BcPc) {
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

        self.side_generic_op(generic, ret, shl_values as _, xmm_using, pc);
        self.jit.select_page(1);
        monoasm!(self.jit,
        shr:
            negq rcx;
            cmpq rcx, 64;
            jge under;
            sarq rdi, rcx;
            jmp after;
        );
        self.jit.select_page(0);
        self.shift_under(under, after);
    }

    fn side_generic_op(
        &mut self,
        generic: DestLabel,
        ret: SlotId,
        func: u64,
        xmm_using: UsingXmm,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        self.jit.bind_label(exit);
        self.jit.select_page(1);
        self.jit.bind_label(generic);
        self.generic_binop(ret, func, xmm_using, pc);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    fn generic_binop(&mut self, ret: SlotId, func: u64, xmm_using: UsingXmm, pc: BcPc) {
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq r13, ((pc + 1).get_u64());
        );
        self.call_binop(func);
        self.xmm_restore(&xmm_using);
        self.store_rax(ret);
    }

    fn jit_class_def(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        let jit_return = self.vm_return;
        if superclass.is_zero() {
            monoasm! { self.jit,
                xorq rcx, rcx;
            }
        } else {
            monoasm! { self.jit,
                movq rcx, [rbp - (conv(superclass))];  // rcx <- superclass: Option<Value>
            }
        }
        monoasm! { self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_class);
            call rax;  // rax <- self: Value
            testq rax, rax; // rax: Option<Value>
            jeq  jit_return;
            movq r15, rax; // r15 <- self
            movl rdx, (func_id.0);  // rdx <- func_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (vm_get_func_data);
            call rax; // rax <- &FuncData
            //
            //       +-------------+
            // +0x08 |     pc      |
            //       +-------------+
            //  0x00 |   ret reg   | <- rsp
            //       +-------------+
            // -0x08 | return addr |
            //       +-------------+
            // -0x10 |   old rbp   |
            //       +-------------+
            // -0x18 |    meta     |
            //       +-------------+
            // -0x20 |     %0      |
            //       +-------------+
            // -0x28 | %1(1st arg) | <- rdx
            //       +-------------+
            //       |             |
            //
            movq rdi, [rax + (FUNCDATA_OFFSET_META)];
            movq [rsp - 0x18], rdi;
            movq [rsp - 0x20], r15;
            movq r13 , [rax + (FUNCDATA_OFFSET_PC)];
            movq rax, [rax + (FUNCDATA_OFFSET_CODEPTR)];
            xorq rdi, rdi;
            call rax;
            testq rax, rax;
            jeq jit_return;
        };
        if !ret.is_zero() {
            monoasm!(self.jit,
                movq [rbp - (conv(ret))], rax;
            );
        }
        // pop class context.
        monoasm!(self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (pop_class_context);
            call rax;
        );
        self.xmm_restore(&xmm_using);
    }
}
