//! qemu-validated proof of the monoruby aarch64 VM dispatch core.
//!
//! This re-creates, in miniature, the exact machine-code shapes the real
//! VM-tier port (`codegen/vmgen` etc.) will emit on aarch64, so the encodings
//! and the register/ABI choices are validated under qemu before the (large,
//! otherwise-untestable-until-it-links) port is written:
//!
//! * the **global-register mapping** (x86 → aarch64): pc `r13`→`x21`, lfp
//!   `r14`→`x22`, accumulator `r15`→`x23` (all callee-saved under AAPCS64,
//!   saved in the VM entry prologue, like x86 `pushq rbp`);
//! * **`fetch_and_dispatch`**: `ldrb` the opcode, indirect-load the handler
//!   from a 256-entry dispatch table (`ldr Xtgt,[Xtbl,Xop,lsl #3]`), `br`;
//!   mirrors the x86 `movzxb`/`jmp [r15+rax*8]`;
//! * a **runtime call** in the AAPCS64 convention (`blr`), as most opcode
//!   handlers do;
//! * the **slot-access idiom** `[r14 + reg*8 - LFP_SELF]` (a register-indexed,
//!   displaced load): `neg`, `add Xa,x_lfp,Xreg,lsl #3`, `sub #LFP_SELF`,
//!   `ldr` — exactly the 2-3 instructions the plan calls for since A64 has no
//!   base+index+displacement addressing.
//!
//! Bytecode shape (matching monoruby's 16-byte instructions): the opcode byte
//! is at `[pc + 6]`; here each instruction carries a u64 operand at `[pc + 8]`.

#![cfg(target_arch = "aarch64")]

use monoasm::*;

// Global registers, mirroring the monoruby x86 assignment.
const PC: GReg = X21; // program counter   (x86 r13)
const LFP: GReg = X22; // local frame ptr  (x86 r14)
const ACC: GReg = X23; // accumulator      (x86 r15)
// Caller-saved scratch.
const TBL: GReg = X9;
const OP: GReg = X10;
const TGT: GReg = X11;
const TMP: GReg = X12;

const OPECODE: u32 = 6; // opcode byte offset within an instruction
const OPERAND: u32 = 8; // u64 operand offset within an instruction
const INST_LEN: u32 = 16;
const LFP_SELF: u32 = 40; // monoruby LFP_SELF

/// Opcodes for the mini-VM.
const OP_LOAD: u8 = 0; // acc <- operand
const OP_ADD: u8 = 1; // acc <- acc + operand
const OP_CALL: u8 = 2; // acc <- runtime(acc)   (operand = fn ptr)
const OP_LOADSLOT: u8 = 3; // acc <- frame slot (operand = slot index)
const OP_JMPZ: u8 = 4; // if acc == 0, pc += operand instructions
const OP_INTADD: u8 = 5; // guarded tagged-fixnum add (operand = tagged rhs)
const OP_RET: u8 = 6; // return acc

/// monoruby tags a fixnum as `(n << 1) | 1` (low bit set).
fn tag(n: i64) -> u64 {
    ((n << 1) | 1) as u64
}
fn untag(v: u64) -> i64 {
    (v as i64) >> 1
}

extern "C" fn double_it(x: u64) -> u64 {
    x * 2
}

/// Build the mini-VM and return (entry fn, dispatch table kept alive).
fn build() -> (extern "C" fn(*const u8, *const u64) -> u64, Box<[u64; 256]>) {
    let mut jit = JitMemory::new();
    let mut table: Box<[u64; 256]> = vec![0u64; 256].into_boxed_slice().try_into().unwrap();
    let table_ptr = table.as_ptr() as u64;

    let entry = jit.label();
    let dispatch = jit.label();
    let h_load = jit.label();
    let h_add = jit.label();
    let h_call = jit.label();
    let h_loadslot = jit.label();
    let h_jmpz = jit.label();
    let h_intadd = jit.label();
    let h_ret = jit.label();
    let do_jump = jit.label();
    let slow = jit.label();

    // ---- VM entry prologue: save callee-saved globals + fp/lr ----
    jit.bind_label(entry.clone());
    jit.stp_pre(X29, X30, SP, -16);
    jit.stp_pre(PC, ACC, SP, -16);
    jit.stp_pre(LFP, X19, SP, -16); // X19 is padding (kept 16-byte aligned)
    jit.mov(PC, X0); // pc  <- program pointer (arg0)
    jit.mov(LFP, X1); // lfp <- frame pointer   (arg1)
    jit.mov_imm(ACC, 0); // acc <- 0
    jit.b_label(&dispatch);

    // ---- fetch_and_dispatch ----
    jit.bind_label(dispatch.clone());
    jit.mov_imm(TBL, table_ptr); // table base (immediate; x86 movq r15,(ptr))
    jit.ldrb(OP, PC, OPECODE); // opcode <- [pc + 6]
    jit.ldr_reg(TGT, TBL, OP, true); // handler <- table[opcode]  (lsl #3)
    jit.br(TGT);

    // ---- handlers (each ends by advancing pc and re-dispatching) ----
    jit.bind_label(h_load.clone());
    jit.ldr(ACC, PC, OPERAND); // acc <- [pc + 8]
    jit.add_imm(PC, PC, INST_LEN, 0);
    jit.b_label(&dispatch);

    jit.bind_label(h_add.clone());
    jit.ldr(TMP, PC, OPERAND);
    jit.add(ACC, ACC, TMP);
    jit.add_imm(PC, PC, INST_LEN, 0);
    jit.b_label(&dispatch);

    // runtime call: x0 <- acc, blr operand(fn ptr), acc <- x0
    jit.bind_label(h_call.clone());
    jit.ldr(TGT, PC, OPERAND); // fn ptr
    jit.mov(X0, ACC); // arg0 = acc
    jit.blr(TGT);
    jit.mov(ACC, X0); // acc = result
    jit.add_imm(PC, PC, INST_LEN, 0);
    jit.b_label(&dispatch);

    // slot access: acc <- [lfp + (-slot)*8 - LFP_SELF]
    jit.bind_label(h_loadslot.clone());
    jit.ldr(TMP, PC, OPERAND); // TMP = slot index
    jit.neg(TMP, TMP); // TMP = -slot          (x86 negq)
    jit.add_lsl(TMP, LFP, TMP, 3); // TMP = lfp + (-slot)*8
    jit.sub_imm(TMP, TMP, LFP_SELF, 0); // TMP -= LFP_SELF
    jit.ldr(ACC, TMP, 0); // acc = [TMP]
    jit.add_imm(PC, PC, INST_LEN, 0);
    jit.b_label(&dispatch);

    // conditional branch: if acc == 0, pc += operand instructions
    jit.bind_label(h_jmpz.clone());
    jit.ldr(TMP, PC, OPERAND); // delta (instruction count)
    jit.cbz_label(ACC, &do_jump); // taken if acc == 0
    jit.add_imm(PC, PC, INST_LEN, 0); // not taken: fall through
    jit.b_label(&dispatch);
    jit.bind_label(do_jump.clone());
    jit.lsl_imm(TMP, TMP, 4); // delta * 16 (INST_LEN)
    jit.add(PC, PC, TMP); // pc <- pc + delta*16
    jit.b_label(&dispatch);

    // guarded tagged-fixnum add: both operands must be fixnums (low bit set),
    // result = acc + rhs - 1  (re-tag: (2a+1)+(2b+1)-1 = 2(a+b)+1).
    jit.bind_label(h_intadd.clone());
    jit.ldr(TMP, PC, OPERAND); // rhs (tagged)
    jit.tbz_label(ACC, 0, &slow); // acc not a fixnum -> slow
    jit.tbz_label(TMP, 0, &slow); // rhs not a fixnum -> slow
    jit.add(ACC, ACC, TMP);
    jit.sub_imm(ACC, ACC, 1, 0);
    jit.add_imm(PC, PC, INST_LEN, 0);
    jit.b_label(&dispatch);

    // guard-failure slow path (deopt point in the real VM); trap here.
    jit.bind_label(slow.clone());
    jit.brk(0);

    // ---- VM exit epilogue ----
    jit.bind_label(h_ret.clone());
    jit.mov(X0, ACC); // return acc
    jit.ldp_post(LFP, X19, SP, 16);
    jit.ldp_post(PC, ACC, SP, 16);
    jit.ldp_post(X29, X30, SP, 16);
    jit.ret();

    jit.finalize();

    table[OP_LOAD as usize] = jit.get_label_address(&h_load).as_ptr() as u64;
    table[OP_ADD as usize] = jit.get_label_address(&h_add).as_ptr() as u64;
    table[OP_CALL as usize] = jit.get_label_address(&h_call).as_ptr() as u64;
    table[OP_LOADSLOT as usize] = jit.get_label_address(&h_loadslot).as_ptr() as u64;
    table[OP_JMPZ as usize] = jit.get_label_address(&h_jmpz).as_ptr() as u64;
    table[OP_INTADD as usize] = jit.get_label_address(&h_intadd).as_ptr() as u64;
    table[OP_RET as usize] = jit.get_label_address(&h_ret).as_ptr() as u64;

    let f: extern "C" fn(*const u8, *const u64) -> u64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    (f, table)
}

/// Assemble a program from (opcode, operand) pairs into 16-byte instructions.
fn program(insts: &[(u8, u64)]) -> Vec<u8> {
    let mut prog = vec![0u8; insts.len() * INST_LEN as usize];
    for (i, (op, operand)) in insts.iter().enumerate() {
        let base = i * INST_LEN as usize;
        prog[base + OPECODE as usize] = *op;
        prog[base + OPERAND as usize..base + OPERAND as usize + 8]
            .copy_from_slice(&operand.to_le_bytes());
    }
    prog
}

#[test]
fn dispatch_load_add_ret() {
    let (f, _table) = build();
    // acc = 40; acc += 2; return acc  => 42
    let prog = program(&[(OP_LOAD, 40), (OP_ADD, 2), (OP_RET, 0)]);
    assert_eq!(f(prog.as_ptr(), std::ptr::null()), 42);
}

#[test]
fn dispatch_runtime_call_blr() {
    let (f, _table) = build();
    // acc = 21; acc = double_it(acc); return acc  => 42
    let prog = program(&[
        (OP_LOAD, 21),
        (OP_CALL, double_it as *const () as u64),
        (OP_RET, 0),
    ]);
    assert_eq!(f(prog.as_ptr(), std::ptr::null()), 42);
}

#[test]
fn dispatch_many_adds() {
    let (f, _table) = build();
    // acc = 0; +1 +2 ... +9 ; return  => 45 (exercises the dispatch loop)
    let mut insts = vec![(OP_LOAD, 0u64)];
    for k in 1..=9u64 {
        insts.push((OP_ADD, k));
    }
    insts.push((OP_RET, 0));
    let prog = program(&insts);
    assert_eq!(f(prog.as_ptr(), std::ptr::null()), 45);
}

#[test]
fn slot_access_idiom() {
    let (f, _table) = build();
    // Fake frame: slot `s` lives at byte `lfp - LFP_SELF - s*8`, i.e. in u64
    // units `frame[base - 5 - s]` for lfp = &frame[base] (LFP_SELF/8 = 5).
    let frame = {
        let mut v = vec![0u64; 16];
        v[5] = 1000; // slot 0
        v[4] = 2000; // slot 1
        v[3] = 3000; // slot 2
        v
    };
    let base = 10usize;
    let lfp = unsafe { frame.as_ptr().add(base) };
    // load slot 2 => 3000
    let prog = program(&[(OP_LOADSLOT, 2), (OP_RET, 0)]);
    assert_eq!(f(prog.as_ptr(), lfp), 3000);
    // load slot 0 => 1000
    let prog = program(&[(OP_LOADSLOT, 0), (OP_RET, 0)]);
    assert_eq!(f(prog.as_ptr(), lfp), 1000);
}

#[test]
fn conditional_branch_taken_and_not() {
    let (f, _table) = build();
    // acc = 0; if acc==0 skip the next instr; LOAD 99; RET  => 0 (branch taken)
    let prog = program(&[(OP_LOAD, 0), (OP_JMPZ, 2), (OP_LOAD, 99), (OP_RET, 0)]);
    assert_eq!(f(prog.as_ptr(), std::ptr::null()), 0);
    // acc = 5; branch NOT taken; LOAD 99; RET  => 99
    let prog = program(&[(OP_LOAD, 5), (OP_JMPZ, 2), (OP_LOAD, 99), (OP_RET, 0)]);
    assert_eq!(f(prog.as_ptr(), std::ptr::null()), 99);
}

#[test]
fn tagged_fixnum_add_with_guard() {
    let (f, _table) = build();
    // acc = tag(20); acc = guarded_add(acc, tag(22)); RET  => tag(42)
    let prog = program(&[(OP_LOAD, tag(20)), (OP_INTADD, tag(22)), (OP_RET, 0)]);
    let result = f(prog.as_ptr(), std::ptr::null());
    assert_eq!(result, tag(42));
    assert_eq!(untag(result), 42);
}
