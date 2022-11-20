use monoasm_macro::monoasm;

use super::*;

mod analysis;
mod binary_op;
mod compile;
mod constants;
mod guard;
mod merge;
mod method_call;
mod read_slot;

//
// Just-in-time compiler module.
//

struct JitContext {
    labels: HashMap<usize, DestLabel>,
    /// (bb_id, Vec<src_idx>)
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    bb_pos: usize,
    loop_count: usize,
    is_loop: bool,
    branch_map: HashMap<usize, Vec<BranchEntry>>,
    backedge_map: HashMap<usize, (DestLabel, StackSlotInfo, Vec<SlotId>)>,
    start_codepos: usize,
    self_value: Value,
    sourcemap: Vec<(usize, usize)>,
}

impl JitContext {
    fn new(
        func: &ISeqInfo,
        codegen: &mut Codegen,
        start_pos: usize,
        is_loop: bool,
        self_value: Value,
    ) -> Self {
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
            self_value,
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

#[derive(Debug)]
struct BranchEntry {
    src_idx: usize,
    bbctx: BBContext,
    dest_label: DestLabel,
}

fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + OFFSET_SELF
}

type WriteBack = Vec<(u16, Vec<SlotId>)>;

///
/// Context of the current Basic block.
///
#[derive(Debug, Clone, PartialEq)]
struct BBContext {
    /// information for stack slots.
    stack_slot: StackSlotInfo,
    /// information for xmm registers.
    xmm: [Vec<SlotId>; 14],
    self_class: ClassId,
    self_kind: Option<u8>,
    local_num: usize,
}

impl BBContext {
    fn new(reg_num: usize, local_num: usize, self_value: Value) -> Self {
        let xmm = (0..14)
            .map(|_| vec![])
            .collect::<Vec<Vec<SlotId>>>()
            .try_into()
            .unwrap();
        Self {
            stack_slot: StackSlotInfo(vec![LinkMode::None; reg_num]),
            xmm,
            self_class: self_value.class_id(),
            self_kind: self_value.kind(),
            local_num,
        }
    }

    fn from(slot_info: &StackSlotInfo, local_num: usize, self_value: Value) -> Self {
        let mut ctx = Self::new(slot_info.0.len(), local_num, self_value);
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

    fn remove_unused(&mut self, unused: &[SlotId]) {
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
        self.stack_slot[reg] = LinkMode::XmmRW(freg);
        self.xmm[freg as usize].push(reg);
    }

    fn link_r_xmm(&mut self, reg: SlotId, freg: u16) {
        self.stack_slot[reg] = LinkMode::XmmR(freg);
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

    fn dealloc_locals(&mut self) {
        for reg in 1..1 + self.local_num as u16 {
            self.dealloc_xmm(SlotId(reg));
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
    fn write_back_slot(&mut self, codegen: &mut Codegen, reg: SlotId) {
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
            self.write_back_slot(codegen, SlotId::new(reg))
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

    fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| {
                            reg.0 as usize <= local_num
                                && matches!(self.stack_slot[**reg], LinkMode::XmmRW(_))
                        })
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

    fn merge_entries(entries: &[BranchEntry]) -> Self {
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

type UsingXmm = Vec<usize>;

#[derive(PartialEq)]
enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _interp: &mut Executor,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Value,
) {
    let name = globals.func[func_id].as_ruby_func().name();
    let bc_begin = globals.func[func_id].as_ruby_func().get_bytecode_address(0);
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let BcOp::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else if let BcOp::ClassDef { .. } = pc.op1() {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else {
        match globals.deopt_stats.get_mut(&(func_id, index)) {
            Some(c) => *c = *c + 1,
            None => {
                globals.deopt_stats.insert((func_id, index), 0);
            }
        };
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt} caused by {}", index, v.to_s(globals));
    }
}

impl Codegen {
    pub(super) fn jit_compile_ruby(
        &mut self,
        fnstore: &FnStore,
        func_id: FuncId,
        self_value: Value,
        position: Option<usize>,
    ) -> (DestLabel, Vec<(usize, usize)>) {
        let func = fnstore[func_id].as_ruby_func();
        let start_pos = position.unwrap_or_default();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = std::time::Instant::now();

        let entry = self.jit.label();
        self.jit.bind_label(entry);

        let mut cc = jitgen::JitContext::new(func, self, start_pos, position.is_some(), self_value);
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
        let local_num = func.local_num();
        cc.start_codepos = self.jit.get_current();

        if position.is_none() {
            self.prologue(func);
            let pc = func.get_pc(position.unwrap_or_default());
            let side_exit = self.gen_side_deopt(pc);
            monoasm!(self.jit,
                movq rdi, [rbp - (OFFSET_SELF)];
            );
            self.guard_class(self_value.class_id(), side_exit);
        }

        cc.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: 0,
                bbctx: BBContext::new(reg_num, local_num, self_value),
                dest_label: self.jit.label(),
            }],
        );
        for i in bb_start_pos {
            cc.bb_pos = i;
            if self.compile_bb(fnstore, func, &mut cc) {
                break;
            };
        }

        let keys: Vec<_> = cc.branch_map.keys().cloned().collect();
        for pos in keys.into_iter() {
            self.gen_backedge_branch(&mut cc, func, pos);
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("<== finished compile.");
        (entry, cc.sourcemap)
    }

    ///
    /// Convert f64 to Value.
    ///
    /// ### in
    ///
    /// - xmm0: f64
    ///
    /// ### out
    ///
    /// - rax: Value
    ///
    /// ### registers destroyed
    ///
    /// - rcx, xmm1
    ///
    pub(super) fn generate_f64_to_val(&mut self) -> DestLabel {
        let entry = self.jit.label();
        let normal = self.jit.label();
        let heap_alloc = self.jit.label();
        monoasm!(self.jit,
        entry:
            xorps xmm1, xmm1;
            ucomisd xmm0, xmm1;
            jne normal;
            jp normal;
            movq rax, (Value::new_float(0.0).get());
            ret;
        normal:
            movq rax, xmm0;
            movq rcx, rax;
            shrq rcx, 60;
            addl rcx, 1;
            andl rcx, 6;
            cmpl rcx, 4;
            jne heap_alloc;
            rolq rax, 3;
            andq rax, (-4);
            orq rax, 2;
            ret;
        heap_alloc:
        // we must save rdi for log_optimize.
            subq rsp, 120;
            movq [rsp + 112], rdi;
            movq [rsp + 104], xmm15;
            movq [rsp + 96], xmm14;
            movq [rsp + 88], xmm13;
            movq [rsp + 80], xmm12;
            movq [rsp + 72], xmm11;
            movq [rsp + 64], xmm10;
            movq [rsp + 56], xmm9;
            movq [rsp + 48], xmm8;
            movq [rsp + 40], xmm7;
            movq [rsp + 32], xmm6;
            movq [rsp + 24], xmm5;
            movq [rsp + 16], xmm4;
            movq [rsp + 8], xmm3;
            movq [rsp + 0], xmm2;
            movq rax, (Value::new_float);
            call rax;
            movq xmm2, [rsp + 0];
            movq xmm3, [rsp + 8];
            movq xmm4, [rsp + 16];
            movq xmm5, [rsp + 24];
            movq xmm6, [rsp + 32];
            movq xmm7, [rsp + 40];
            movq xmm8, [rsp + 48];
            movq xmm9, [rsp + 56];
            movq xmm10, [rsp + 64];
            movq xmm11, [rsp + 72];
            movq xmm12, [rsp + 80];
            movq xmm13, [rsp + 88];
            movq xmm14, [rsp + 96];
            movq xmm15, [rsp + 104];
            movq rdi, [rsp + 112];
            addq rsp, 120;
            ret;
        );
        entry
    }
}

impl Codegen {
    ///
    /// store rax to *ret*.
    ///
    fn store_rax(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            movq [rbp - (conv(ret))], rax;
        );
    }

    ///
    /// store rdi to *ret*.
    ///
    fn store_rdi(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            movq [rbp - (conv(ret))], rdi;
        );
    }

    ///
    /// store rsi to *ret*.
    ///
    fn store_rsi(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rsi;
        );
    }

    ///
    /// move xmm(*src*) to xmm(*dst*).
    ///
    fn xmm_mov(&mut self, src: u16, dst: u16) {
        if src != dst {
            monoasm!(self.jit,
                movq  xmm(dst as u64 + 2), xmm(src as u64 + 2);
            );
        }
    }

    ///
    /// Assume the Value is Integer, and convert to f64.
    ///
    /// side-exit if not Integer.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm0: f64
    ///
    fn gen_val_to_f64_assume_integer(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let entry = self.jit.label();
        monoasm!(&mut self.jit,
        entry:
            testq rdi, 0b01;
            jz side_exit;
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        );
        entry
    }

    ///
    /// Assume the Value is Float, and convert to f64.
    ///
    /// side-exit if not Float.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*): f64
    ///
    /// ### registers destroyed
    ///
    /// - rax, rdi
    ///
    fn gen_val_to_f64_assume_float(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let heap_to_f64 = self.heap_to_f64;
        let entry = self.jit.label();
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
        entry:
            testq rdi, 0b01;
            jnz side_exit;
            testq rdi, 0b10;
            jz heap;
            xorps xmm(xmm), xmm(xmm);
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            je exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(xmm), rdi;
            jmp exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
            movq xmm(xmm), xmm0;
        exit:
        );

        entry
    }

    ///
    /// Confirm the Value is Float.
    ///
    /// side-exit if not Float.
    ///
    /// ### registers destroyed
    ///
    /// - rax, rdi
    ///
    fn gen_assume_float(&mut self, reg: SlotId, side_exit: DestLabel) {
        let heap_to_f64 = self.heap_to_f64;
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
            movq rdi, [rbp - (conv(reg))];
            testq rdi, 0b01;
            jnz side_exit;
            testq rdi, 0b10;
            jnz exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
        exit:
        );
    }

    ///
    /// Convert the Value to f64.
    ///
    /// side-exit if neither Float nor Integer.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*): f64
    ///
    /// ### registers destroyed
    ///
    /// - caller save registers except xmm registers(xmm2-xmm15).
    ///
    fn gen_val_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        let heap_to_f64 = self.heap_to_f64;
        let integer = self.jit.label();
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
            testq rdi, 0b01;
            jnz integer;
            testq rdi, 0b10;
            jz  heap;
            xorps xmm(xmm), xmm(xmm);
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            je  exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(xmm), rdi;
            jmp exit;
        integer:
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
            jmp exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
            movq xmm(xmm), xmm0;
        exit:
        );
    }

    fn compile_bb(&mut self, fnstore: &FnStore, func: &ISeqInfo, cc: &mut JitContext) -> bool {
        let mut skip = false;
        let is_loop = matches!(func.get_pc(cc.bb_pos).op1(), BcOp::LoopStart(_));
        self.jit.bind_label(cc.labels[&cc.bb_pos]);
        let mut ctx = if is_loop {
            self.gen_merging_branches_loop(func, cc, cc.bb_pos)
        } else {
            self.gen_merging_branches(func, cc, cc.bb_pos)
        };
        for (ofs, pc) in func.bytecode()[cc.bb_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            #[cfg(feature = "emit-asm")]
            cc.sourcemap
                .push((cc.bb_pos + ofs, self.jit.get_current() - cc.start_codepos));
            match pc.op1() {
                BcOp::LoopStart(_) => {
                    cc.loop_count += 1;
                }
                BcOp::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.bb_pos + ofs);
                        self.deopt(&ctx, pc);
                        break;
                    }
                }
                BcOp::Integer(ret, i) => {
                    ctx.dealloc_xmm(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                BcOp::Symbol(ret, id) => {
                    ctx.dealloc_xmm(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                BcOp::Literal(dst, val) => {
                    ctx.dealloc_xmm(dst);
                    if let RV::Float(f) = val.unpack() {
                        let fdst = ctx.xmm_write(dst);
                        let imm = self.jit.const_f64(f);
                        monoasm!(self.jit,
                            movq xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        if val.is_packed_value() {
                            monoasm!(self.jit,
                                movq rax, (val.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            monoasm!(self.jit,
                              movq rdi, (val.get());
                              movq rax, (Value::deep_copy);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(dst);
                    }
                }
                BcOp::Array(ret, src, len) => {
                    ctx.write_back_range(self, src, len);
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        lea  rdi, [rbp - (conv(src))];
                        movq rsi, (len);
                        movq rax, (gen_array);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                BcOp::Index(ret, base, idx) => {
                    ctx.write_back_slot(self, base);
                    ctx.write_back_slot(self, idx);
                    ctx.dealloc_xmm(ret);
                    self.jit_get_index(ret, base, idx, pc, &ctx);
                }
                BcOp::IndexAssign(src, base, idx) => {
                    ctx.write_back_slot(self, base);
                    ctx.write_back_slot(self, idx);
                    ctx.write_back_slot(self, src);
                    self.jit_index_assign(src, base, idx, pc, &ctx);
                }
                BcOp::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    self.jit_load_constant(&mut ctx, dst, id, pc);
                }
                BcOp::StoreConst(src, id) => {
                    ctx.write_back_slot(self, src);
                    self.jit_store_constant(id, src, &ctx);
                }
                BcOp::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                    ctx.dealloc_xmm(ret);
                    self.jit_load_ivar(&ctx, id, ret, cached_class, cached_ivarid);
                }
                BcOp::StoreIvar(src, id, cached_class, cached_ivarid) => {
                    ctx.write_back_slot(self, src);
                    self.jit_store_ivar(&ctx, id, src, pc, cached_class, cached_ivarid);
                }
                BcOp::LoadDynVar(ret, src) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..src.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(src.reg) - OFFSET_OUTER;
                    monoasm!(self.jit,
                        movq rax, [rax - (offset)];
                    );
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                BcOp::StoreDynVar(dst, src) => {
                    ctx.write_back_slot(self, src);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..dst.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(dst.reg) - OFFSET_OUTER;
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(src))];
                        movq [rax - (offset)], rdi;
                    );
                }
                BcOp::Nil(ret) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                BcOp::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = self.xmm_read_assume_float(&mut ctx, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst as u64 + 2), [rip + imm];
                        );
                    } else {
                        ctx.write_back_slot(self, src);
                        ctx.dealloc_xmm(dst);
                        let xmm_using = ctx.get_xmm_using();
                        self.xmm_save(&xmm_using);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.xmm_restore(&xmm_using);
                        self.handle_error(pc);
                        self.store_rax(dst);
                    }
                }
                BcOp::BinOp(kind, ret, lhs, rhs) => {
                    if pc.is_integer_binop() {
                        ctx.write_back_slot(self, lhs);
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::RR(lhs, rhs), &ctx);
                    } else if pc.is_float_binop() {
                        let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float(kind, &ctx, fret, flhs, frhs);
                    } else {
                        ctx.write_back_slot(self, lhs);
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.load_binary_args(lhs, rhs);
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::BinOpRi(kind, ret, lhs, rhs) => {
                    if pc.is_integer1() {
                        ctx.write_back_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::RI(lhs, rhs), &ctx);
                    } else if pc.is_float1() {
                        let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ri(kind, &ctx, fret, flhs, rhs);
                    } else {
                        ctx.write_back_slot(self, lhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::BinOpIr(kind, ret, lhs, rhs) => {
                    if pc.is_integer2() {
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::IR(lhs, rhs), &ctx);
                    } else if pc.is_float2() {
                        let frhs = self.xmm_read_assume_float(&mut ctx, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ir(kind, &ctx, fret, lhs, frhs);
                    } else {
                        ctx.write_back_slot(self, rhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                BcOp::Cmp(kind, ret, lhs, rhs, optimizable) => {
                    if !optimizable {
                        if pc.is_float_binop() {
                            let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                        } else {
                            let generic = self.jit.label();
                            ctx.write_back_slot(self, lhs);
                            ctx.write_back_slot(self, rhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmp_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                        }
                    }
                }
                BcOp::Cmpri(kind, ret, lhs, rhs, optimizable) => {
                    if !optimizable {
                        if pc.is_float1() {
                            let rhs_label = self.jit.const_f64(rhs as f64);
                            let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                            ctx.dealloc_xmm(ret);
                            monoasm! { self.jit,
                                xorq rax, rax;
                                ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                            };
                            self.setflag_float(kind);
                            self.store_rax(ret);
                        } else {
                            let generic = self.jit.label();
                            ctx.write_back_slot(self, lhs);
                            ctx.dealloc_xmm(ret);
                            self.gen_cmpri_prep(lhs, rhs, generic);
                            self.gen_cmp_kind(kind, generic, ret, &ctx);
                        }
                    }
                }
                BcOp::Mov(dst, src) => {
                    ctx.copy_slot(self, src, dst);
                }
                BcOp::ConcatStr(ret, arg, len) => {
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
                BcOp::MethodCall(..) => {}
                BcOp::MethodCallBlock(..) => {}
                BcOp::Yield(..) => {}
                BcOp::MethodArgs(method_info) => {
                    self.gen_method_call(fnstore, &mut ctx, method_info, pc);
                    skip = true;
                }
                BcOp::MethodDef(name, func) => {
                    let class_version = self.class_version;
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func)); // FuncId
                        movq rax, (define_method);
                        call rax;
                        addl [rip + class_version], 1;
                    );
                    self.xmm_restore(&xmm_using);
                }
                BcOp::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    self.jit_class_def(&ctx, ret, superclass, name, func_id);
                }
                BcOp::Ret(lhs) => {
                    ctx.write_back_slot(self, lhs);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    return false;
                }
                BcOp::Br(disp) => {
                    let next_idx = cc.bb_pos + ofs + 1;
                    let dest_idx = (next_idx as i64 + disp as i64) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx, branch_dest);
                    monoasm!(self.jit,
                        jmp branch_dest;
                    );
                    return false;
                }
                BcOp::CondBr(cond_, disp, false, kind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
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
                BcOp::CondBr(_, disp, true, brkind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let pc = pc - 1;
                    if pc.is_float_binop() {
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, _ret, lhs, rhs, true) => {
                                let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), xmm(frhs as u64 + 2);
                                };
                                kind
                            }
                            BcOp::Cmpri(kind, _ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs as u64 + 2), [rip + rhs_label];
                                };
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_float_opt(kind, branch_dest, brkind);
                    } else {
                        let generic = self.jit.label();
                        let kind = match pc.op1() {
                            BcOp::Cmp(kind, ret, lhs, rhs, true) => {
                                ctx.write_back_slot(self, lhs);
                                ctx.write_back_slot(self, rhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmp_prep(lhs, rhs, generic);
                                kind
                            }
                            BcOp::Cmpri(kind, ret, lhs, rhs, true) => {
                                ctx.write_back_slot(self, lhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmpri_prep(lhs, rhs, generic);
                                kind
                            }
                            _ => unreachable!(),
                        };

                        let xmm_using = ctx.get_xmm_using();
                        monoasm! { self.jit,
                            cmpq rdi, rsi;
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_int_opt(kind, branch_dest, generic, brkind, xmm_using);
                    }
                }
            }

            let next_idx = cc.bb_pos + ofs + 1;
            if cc.bb_info[next_idx].is_some() {
                let branch_dest = self.jit.label();
                cc.new_branch(cc.bb_pos + ofs, next_idx, ctx.clone(), branch_dest);
                monoasm!(self.jit,
                    jmp branch_dest;
                );
                return false;
            }
        }
        true
    }
}

impl Codegen {
    fn handle_error(&mut self, pc: BcPc) {
        let jit_return = self.vm_return;
        monoasm!(self.jit,
            movq r13, ((pc + 1).get_u64());
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
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

    fn gen_write_back_single(&mut self, freg: u16, v: Vec<SlotId>) {
        if v.is_empty() {
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
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    fn gen_side_writeback_deopt(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        self.gen_side_deopt_main(pc, Some(ctx))
    }

    ///
    /// Get *DestLabel* for fallback to interpreter. (without write-back)
    ///
    pub(super) fn gen_side_deopt(&mut self, pc: BcPc) -> DestLabel {
        self.gen_side_deopt_main(pc, None)
    }

    fn gen_side_deopt_main(&mut self, pc: BcPc, ctx: Option<&BBContext>) -> DestLabel {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let entry = self.jit.label();
        self.jit.bind_label(entry);
        if let Some(ctx) = ctx {
            let wb = ctx.get_write_back();
            if !wb.is_empty() {
                #[cfg(feature = "emit-tir")]
                eprintln!("--gen deopt");
                self.gen_write_back(wb);
                #[cfg(feature = "emit-tir")]
                eprintln!("--gen deopt end");
            }
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
            movq rdx, [rbp - (OFFSET_META)];
            movq rcx, r13;
            movq rax, (log_deoptimize);
            call rax;
        );
        monoasm!(self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_writeback_deopt(pc, ctx);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    fn prologue(&mut self, func: &ISeqInfo) {
        let regs = func.total_reg_num();
        let args = func.total_arg_num();
        let offset = (regs * 8 + OFFSET_SELF as usize + 15) & !0xf;
        let clear_len = regs - args;
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (offset);
        );
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], (NIL_VALUE);
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

    fn xmm_save(&mut self, xmm_using: &[usize]) {
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

    fn xmm_restore(&mut self, xmm_using: &[usize]) {
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
}
