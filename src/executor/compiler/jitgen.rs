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
#[derive(Debug, Clone)]
pub(crate) struct BBContext {
    /// information for stack slots.
    stack_slot: StackSlotInfo,
    /// information for xmm registers.
    xmm: [Vec<SlotId>; 14],
}

#[derive(Clone)]
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
    fn alloc_xmm_read(&mut self, reg: SlotId) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::None => {
                for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
                    if xmm.is_empty() {
                        self.stack_slot[reg] = LinkMode::XmmR(flhs as u16);
                        xmm.push(reg);
                        return flhs as u16;
                    }
                }
                unreachable!()
            }
            _ => unreachable!(),
        };
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

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, codegen: &mut Codegen, src: SlotId, dst: SlotId) {
        self.dealloc_xmm(dst);
        match self.stack_slot[src] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                self.stack_slot[dst] = LinkMode::XmmRW(freg);
                self.xmm[freg as usize].push(dst);
            }
            _ => {
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
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i as u16) })
            .map(|freg| {
                let v: Vec<_> = self.xmm[freg as usize]
                    .iter()
                    .filter(|reg| matches!(self.stack_slot[**reg], LinkMode::XmmRW(_)))
                    .cloned()
                    .collect();
                (freg, v)
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

    fn xmm_read_assume(
        &mut self,
        codegen: &mut Codegen,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(codegen, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(codegen, rhs, pc),
            _ => unreachable!(),
        }
    }

    fn xmm_read_without_assumption(&mut self, codegen: &mut Codegen, reg: SlotId, pc: BcPc) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => freg,
            LinkMode::None => {
                let freg = self.alloc_xmm_read(reg);
                let wb = self.get_write_back();
                let side_exit = codegen.gen_side_deopt_dest(pc, wb);
                monoasm!(codegen.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                codegen.gen_val_to_f64(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_assume_float(&mut self, codegen: &mut Codegen, reg: SlotId, pc: BcPc) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = self.alloc_xmm_read(reg);
                let wb = self.get_write_back();
                let side_exit = codegen.gen_side_deopt_dest(pc, wb);
                monoasm!(codegen.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                codegen.gen_val_to_f64_assume_float(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_assume_integer(&mut self, codegen: &mut Codegen, reg: SlotId, pc: BcPc) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = self.alloc_xmm_read(reg);
                let wb = self.get_write_back();
                let side_exit = codegen.gen_side_deopt_dest(pc, wb);
                monoasm!(codegen.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                codegen.gen_val_to_f64_assume_integer(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_binary(
        &mut self,
        codegen: &mut Codegen,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (u16, u16) {
        if lhs != rhs {
            (
                self.xmm_read_assume(codegen, lhs, pc.classid1(), pc),
                self.xmm_read_assume(codegen, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.xmm_read_assume(codegen, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
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
}

impl CompileContext {
    fn new(func: &NormalFuncInfo, codegen: &mut Codegen, start_pos: usize, is_loop: bool) -> Self {
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
    (($op:ident, $sop:ident)) => {
        paste! {
            fn [<cmp_opt_int_ $sop>](&mut self, branch_dest: DestLabel, generic:DestLabel, brkind: BrKind, xmm_using: Vec<usize>) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $sop>] cont;
                        jmp branch_dest;
                    },
                }
                self.jit.bind_label(cont);
                self.jit.select_page(1);
                monoasm!(self.jit,
                    generic:
                );
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

            fn [<cmp_opt_float_ $sop>](&mut self, branch_dest: DestLabel, generic:DestLabel, brkind: BrKind, xmm_using: Vec<usize>) {
                let cont = self.jit.label();
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $op>] cont;
                        jmp branch_dest;
                    },
                }
                self.jit.bind_label(cont);
                self.jit.select_page(1);
                monoasm!(self.jit,
                    generic:
                );
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
                        jnz cont;
                        jmp branch_dest;
                    },
                }
                self.jit.select_page(0);
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

#[cfg(feature = "log-jit")]
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
    let bc_begin = globals.func[func_id].as_normal().get_bytecode_address(0);
    let index = pc - bc_begin;
    if let BcOp::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {:?}", index, *pc);
    } else {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {:?} caused by {:?}", index, *pc, v);
    }
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

    fn handle_error(&mut self, pc: BcPc) {
        let jit_return = self.vm_return;
        monoasm!(self.jit,
            movq r13, ((pc + 1).0);
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
    }

    fn load_constant(&mut self, dst: SlotId, id: ConstSiteId, pc: BcPc, xmm_using: UsingXmm) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.jit_get_constant(id, pc, xmm_using);
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
        xmm_using: UsingXmm,
        wb: WriteBack,
    ) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        let cached_float = self.jit.const_f64(0.0);
        let side_exit = self.gen_side_deopt_dest(pc, wb.clone());

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.jit_get_constant(id, pc, xmm_using.clone());
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

    fn jit_get_constant(&mut self, id: ConstSiteId, pc: BcPc, xmm_using: UsingXmm) {
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

    fn jit_store_constant(&mut self, id: IdentId, src: SlotId, xmm_using: UsingXmm) {
        let const_version = self.const_version;
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

    fn jit_get_index(
        &mut self,
        ret: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        xmm_using: UsingXmm,
    ) {
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
        xmm_using: UsingXmm,
    ) {
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

    fn gen_write_back_with(&mut self, src_ctx: &BBContext, target_ctx: &BBContext) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.stack_slot);
            eprintln!("      target: {:?}", target_ctx.stack_slot);
        }
        let len = src_ctx.stack_slot.0.len();
        let mut wb = vec![vec![]; 14];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (LinkMode::XmmRW(l), LinkMode::XmmRW(r)) if l == r => {}
                (_, LinkMode::XmmRW(_)) => unreachable!(),
                (LinkMode::XmmR(l), LinkMode::XmmR(r)) if l == r => {}
                (_, LinkMode::XmmR(_)) => unreachable!(),
                (LinkMode::None | LinkMode::XmmR(_), LinkMode::None) => {}
                (LinkMode::XmmRW(l), LinkMode::None) => {
                    wb[l as usize].push(reg);
                }
            }
        }
        for (i, v) in wb.into_iter().enumerate() {
            self.gen_write_back_single(i as u16, v);
        }
    }

    fn gen_write_back_backedge(
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
        let wb = src_ctx.get_write_back();
        self.gen_write_back(wb);
        for info in src_ctx.stack_slot.0.iter_mut() {
            if let LinkMode::XmmRW(_) = info {
                *info = LinkMode::None;
            }
        }
        let len = src_ctx.stack_slot.0.len();
        let mut conv_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (_, LinkMode::XmmRW(_)) => unreachable!(),
                (LinkMode::XmmRW(_), _) => unreachable!(),
                (LinkMode::None, LinkMode::None) => {}
                (LinkMode::XmmR(_), LinkMode::None) => {}
                (LinkMode::XmmR(l), LinkMode::XmmR(r)) => {
                    if l != r {
                        conv_list.push((reg, r));
                    }
                }
                (LinkMode::None, LinkMode::XmmR(r)) => {
                    conv_list.push((reg, r));
                }
            }
        }
        let wb = src_ctx.get_write_back();
        let side_exit = self.gen_side_deopt_dest(pc + 1, wb.clone());
        for (reg, freg) in conv_list {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(reg))];
            );
            self.gen_val_to_f64(freg as u64 + 2, side_exit);
            #[cfg(feature = "emit-tir")]
            eprintln!("      conv: {:?}->{:?}", reg, freg);
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
    fn gen_side_deopt_dest(&mut self, pc: BcPc, wb: WriteBack) -> DestLabel {
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
            movq r13, (pc.0);
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
        let wb = ctx.get_write_back();
        let fallback = self.gen_side_deopt_dest(pc, wb);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    pub(super) fn jit_compile_normal(
        &mut self,
        func: &NormalFuncInfo,
        position: Option<usize>,
    ) -> DestLabel {
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

        if position.is_none() {
            self.prologue(func.total_reg_num());
        }

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
            if self.compile_bb(func, &mut cc) {
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
            eprintln!("{}", self.jit.dump_code().unwrap());
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("    finished compile. elapsed:{:?}", elapsed);
        #[cfg(feature = "emit-tir")]
        eprintln!("    finished compile.");

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
        pc: BcPc,
        kind: BinOpK,
        ret: SlotId,
        mode: BinOpMode,
        wb: WriteBack,
        xmm_using: UsingXmm,
    ) {
        let deopt = self.gen_side_deopt_dest(pc, wb);
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

    fn gen_binop_kind(&mut self, using_xmm: UsingXmm, pc: BcPc, kind: BinOpK, ret: SlotId) {
        self.generic_binop(ret, kind.generic_func() as _, using_xmm, pc);
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

    fn gen_cmp_kind(
        &mut self,
        kind: CmpKind,
        generic: DestLabel,
        ret: SlotId,
        xmm_using: Vec<usize>,
    ) {
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

    fn gen_cmp_float_opt(
        &mut self,
        kind: CmpKind,
        branch_dest: DestLabel,
        generic: DestLabel,
        brkind: BrKind,
        xmm_using: Vec<usize>,
    ) {
        match kind {
            CmpKind::Eq => self.cmp_opt_float_eq(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ne => self.cmp_opt_float_ne(branch_dest, generic, brkind, xmm_using),
            CmpKind::Ge => self.cmp_opt_float_ge(branch_dest, generic, brkind, xmm_using),
            CmpKind::Gt => self.cmp_opt_float_gt(branch_dest, generic, brkind, xmm_using),
            CmpKind::Le => self.cmp_opt_float_le(branch_dest, generic, brkind, xmm_using),
            CmpKind::Lt => self.cmp_opt_float_lt(branch_dest, generic, brkind, xmm_using),
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

        self.jit.select_page(1);
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
        self.jit.select_page(0);
    }
}
