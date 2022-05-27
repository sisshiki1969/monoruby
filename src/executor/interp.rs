use super::compiler::JitGen;
use super::*;
use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BcPcBase(*const u64);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs as isize) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    pub(super) fn new(func: &NormalFuncInfo) -> Self {
        BcPcBase(&func.bytecode()[0] as *const _)
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BcPc(*const u64);

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc(self.0.offset(offset as isize));
        }
    }
}

impl std::default::Default for BcPc {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

/*fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}*/

#[derive(Debug, Clone)]
#[repr(C)]
struct FuncData {
    offset: usize,
    address: u64,
    pc: BcPc,
}

extern "C" fn get_func_data(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    data: &mut FuncData,
) {
    let label = globals.func[func_id].jit_label().unwrap();
    data.address = label.0;
    data.offset = globals.func[func_id].stack_offset();
    data.pc = globals.func[func_id].inst_pc();
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct EncodedCallInfo(u64);

impl EncodedCallInfo {
    fn new(func_id: FuncId, args: u16, len: u16) -> Self {
        Self((func_id.0 as u64) + ((args as u64) << 48) + ((len as u64) << 32))
    }

    fn none() -> Self {
        Self(-1i64 as u64)
    }
}

extern "C" fn find_method(
    interp: &mut Interp,
    globals: &mut Globals,
    callsite_id: CallsiteId,
    data: &mut FuncData,
) -> EncodedCallInfo {
    match interp.find_method(globals, callsite_id) {
        Some((func_id, args, len)) => {
            get_func_data(interp, globals, func_id, data);
            EncodedCallInfo::new(func_id, args, len)
        }
        None => EncodedCallInfo::none(),
    }
}

extern "C" fn get_constant(_interp: &mut Interp, globals: &mut Globals, const_id: u32) -> Value {
    globals.func.get_literal(const_id)
}

extern "C" fn define_method(interp: &mut Interp, globals: &mut Globals, def_id: MethodDefId) {
    let MethodDefInfo { name, func } = globals.func[def_id];
    globals.func.insert(name, func);
    interp.class_version += 1;
}

extern "C" fn eprintln(data: u64) {
    eprintln!("{:016x}", data);
}

macro_rules! cmp_ops {
    ($op:ident) => {
        paste! {
            fn [<vm_ $op rr>](&mut self) -> CodePtr {
                let label = self.jit_gen.jit.get_current_address();
                self.vm_get_addr_rdi(); // rdi <- lhs addr
                self.vm_get_addr_rsi(); // rsi <- rhs addr
                self.vm_get_addr_r15(); // r15 <- ret addr
                monoasm! { self.jit_gen.jit,
                    movq rdi, [rdi];
                    movq rsi, [rsi];
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                    // store the result to return reg.
                    movq [r15], rax;
                };
                self.fetch_and_dispatch();
                label
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_ops!($op1);
        cmp_ops!($($op2),+);
    };
}

macro_rules! cmp_ri_ops {
    ($op:ident) => {
        paste! {
            fn [<vm_ $op ri>](&mut self) -> CodePtr {
                let label = self.jit_gen.jit.get_current_address();
                self.vm_get_addr_rdi(); // rdi <- lhs addr
                self.vm_get_addr_r15(); // r15 <- ret addr
                monoasm! { self.jit_gen.jit,
                    movq rdi, [rdi];
                    movq rax, ([<cmp_ $op _ri_values>]);
                    call rax;
                    // store the result to return reg.
                    movq [r15], rax;
                };
                self.fetch_and_dispatch();
                label
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_ri_ops!($op1);
        cmp_ri_ops!($($op2),+);
    };
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: FuncId,
    pub jit_gen: JitGen,
    pub error: Option<MonorubyErr>,
    dispatch: Vec<CodePtr>,
    class_version: usize,
    vm_entry: DestLabel,
}

impl Interp {
    fn new(main: &NormalFuncInfo) -> Self {
        let mut jit_gen = JitGen::new();
        let vm_entry = jit_gen.jit.label();
        // dispatch table.
        let entry_panic = jit_gen.jit.get_current_address();
        monoasm! { jit_gen.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rax, (super::compiler::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        let dispatch = vec![entry_panic; 256];
        Self {
            cur_fn: main.id,
            jit_gen,
            error: None,
            dispatch,
            class_version: 0,
            vm_entry,
        }
    }

    fn find_method(
        &mut self,
        globals: &mut Globals,
        callsite_id: CallsiteId,
    ) -> Option<(FuncId, u16, u16)> {
        let CallsiteInfo {
            name,
            args,
            len,
            cache: (version, cached_func),
        } = globals.func[callsite_id];
        let func_id = if version == self.class_version {
            cached_func
        } else {
            match globals.get_method(name) {
                Some(func_id) => {
                    globals.func[callsite_id].cache = (self.class_version, func_id);
                    func_id
                }
                None => {
                    self.error = Some(MonorubyErr::MethodNotFound(name));
                    return None;
                }
            }
        };
        let info = &globals.func[func_id];
        if info.arity() != len as usize {
            self.error = Some(MonorubyErr::WrongArguments(format!(
                "number of arguments mismatch. expected:{} actual:{}",
                info.arity(),
                len
            )));
            None
        } else {
            Some((func_id, args, len))
        }
    }

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let main = globals.func[globals.get_main_func()].as_normal();
        let mut eval = Self::new(main);
        eval.jit_gen.exec_toplevel(globals)(&mut eval, globals)
            .ok_or_else(|| std::mem::take(&mut eval.error).unwrap())
    }

    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let main_id = globals.get_main_func();
        let main = globals.func[main_id].as_normal();
        let mut eval = Self::new(main);

        let entry = eval.construct_vm(eval.vm_entry);
        let vm_entry = eval.jit_gen.jit.get_label_address(eval.vm_entry);
        globals.func.precompile(&mut eval.jit_gen, vm_entry);

        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(entry.0) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
    }

    fn construct_vm(&mut self, vm_entry: DestLabel) -> CodePtr {
        let entry = self.jit_gen.jit.get_current_address();
        let func_offset = self.jit_gen.jit.const_i64(0);
        let func_address = self.jit_gen.jit.const_i64(0);
        let func_pc = self.jit_gen.jit.const_i64(0);

        monoasm! { self.jit_gen.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;

            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
                            // rdx: FuncId
            lea rcx, [rip + func_offset];
            movq rax, (get_func_data);
            call rax;
            movq r13, [rip + func_pc];    // r13: BcPc
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
            movq rax, [rip + func_address];
            call rax;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        vm_entry:
            pushq rbp;
            movq rbp, rsp;
            subq rsp, [rip + func_offset];
        };
        self.fetch_and_dispatch();

        //BcOp::Ret
        let ret = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rax, [r15];
            leave;
            ret;
        };

        //BcOp::Mov
        let mov = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        self.vm_get_addr_rdi();
        monoasm! { self.jit_gen.jit,
            movq rax, [rdi];
            movq [r15], rax;
        };
        self.fetch_and_dispatch();

        let br_inst = self.jit_gen.jit.get_current_address();
        let branch = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
        branch:
            lea r13, [r13 + rdi * 8];
        };
        self.fetch_and_dispatch();

        self.dispatch[1] = self.vm_fncall(func_offset, func_address, func_pc);
        self.dispatch[2] = self.vm_method_def();
        self.dispatch[3] = br_inst;
        self.dispatch[4] = self.vm_condbr(branch);
        self.dispatch[5] = self.vm_condnotbr(branch);
        self.dispatch[6] = self.vm_integer();
        self.dispatch[7] = self.vm_constant();
        self.dispatch[8] = self.vm_nil();

        self.dispatch[129] = self.vm_neg();
        self.dispatch[130] = self.vm_addrr();
        self.dispatch[131] = self.vm_subrr();
        self.dispatch[132] = self.vm_mulrr();
        self.dispatch[133] = self.vm_divrr();
        self.dispatch[134] = self.vm_eqrr();
        self.dispatch[135] = self.vm_nerr();
        self.dispatch[136] = self.vm_ltrr();
        self.dispatch[137] = self.vm_lerr();
        self.dispatch[138] = self.vm_gtrr();
        self.dispatch[139] = self.vm_gerr();

        self.dispatch[140] = self.vm_addri();
        self.dispatch[141] = self.vm_subri();
        self.dispatch[142] = self.vm_eqri();
        self.dispatch[143] = self.vm_neri();
        self.dispatch[144] = self.vm_ltri();
        self.dispatch[145] = self.vm_leri();
        self.dispatch[146] = self.vm_gtri();
        self.dispatch[147] = self.vm_geri();

        self.dispatch[148] = ret;
        self.dispatch[149] = mov;

        self.jit_gen.jit.finalize();
        entry
    }

    /// Fetch instruction and decode
    ///
    /// requirement:
    /// r13: BcPc
    ///
    /// returns:
    /// rax: :0
    /// r15: :1
    /// rdi: :2 or :2:3
    /// rsi: :3
    ///
    /// use: r8, r9
    fn fetch_and_dispatch(&mut self) {
        let l1 = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
            movq rax, [r13]; // rax <- :0:1:2:3
            addq r13, 8;
            movsxl rdi, rax;  // rdi <- :2:3
            shrq rax, 32;
            movzxw r15, rax;  // r15 <- :1
            shrq rax, 16;
            movzxw rax, rax;   // rax <- :0
            // dispatch
            testq rax, 0x80;
            jeq l1;
            movsxw rsi, rdi;    // rsi <- :3
            shrq rdi, 16;
            movzxw rdi, rdi;    // rdi <- :2
        l1:
            movq r8, (self.dispatch.as_ptr());
            movzxb rax, rax;
            movq rax, [r8 + rax * 8];
            jmp rax;
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *rdi*: register number
    /// #### @return
    /// - *rdi*: absolute address of the register
    fn vm_get_addr_rdi(&mut self) {
        monoasm! { self.jit_gen.jit,
            //addq rdi, 2;
            negq rdi;
            lea rdi, [rbp + rdi * 8 - 16];
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *rsi*: register number
    /// #### @return
    /// - *rsi*: absolute address of the register
    fn vm_get_addr_rsi(&mut self) {
        monoasm! { self.jit_gen.jit,
            negq rsi;
            lea rsi, [rbp + rsi * 8 - 16];
        };
    }

    /// Get absolute address of the register.
    /// #### @args
    /// - *r15*: register number
    /// #### @return
    /// - *r15*: absolute address of the register
    fn vm_get_addr_r15(&mut self) {
        monoasm! { self.jit_gen.jit,
            negq r15;
            lea r15, [rbp + r15 * 8 - 16];
        };
    }

    fn vm_fncall(
        &mut self,
        func_offset: DestLabel,
        func_address: DestLabel,
        func_pc: DestLabel,
    ) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let exit = self.jit_gen.jit.label();
        let loop_ = self.jit_gen.jit.label();
        let loop_exit = self.jit_gen.jit.label();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // rdx: CallsiteId
            movq rdi, rbx;  // rdi: &mut Interp
            movq rsi, r12;  // rsi: &mut Globals
            lea rcx, [rip + func_offset]; // rcx: &mut usize
            movq rax, (find_method);
            call rax;       // rax <- EncodedCallInfo

            pushq r13;
            pushq r15;
            movq r13, [rip + func_pc];    // r13: BcPc
            shrq rax, 32;
            movl rdi, rax;
            shrq rdi, 16;   // rdi <- args
            movzxw r14, rax;    // r14 <- len
            shlq r14, 3;
            lea rdx, [rsp - 0x28];
        };
        self.vm_get_addr_rdi();

        monoasm! { self.jit_gen.jit,
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
            movq rsi, rdi;
            subq rsi, r14;
        loop_:
            cmpq rdi, rsi;
            jeq loop_exit;
            movq rax, [rdi];
            movq [rdx], rax;
            subq rdi, 8;
            subq rdx, 8;
            jmp loop_;
        loop_exit:

            movq rax, [rip + func_address];
            call rax;
            popq r15;
            popq r13;
            movsxl r15, r15;
            cmpq r15, (-1);
            jeq exit;
        };
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], rax;
        exit:
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_nil(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq [r15], (NIL_VALUE);
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_integer(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            shlq rdi, 1;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_constant(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // const_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (get_constant);
            call rax;
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_neg(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            shrq rdi, 1;
            negq rdi;
            //shlq rdi, 1;
            lea rdi, [rdi + rdi + 1];
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (neg_value);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addri(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            addq rdi, rsi;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            addq rsi, 1;
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_addrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            testq rdi, 0x1;
            jeq generic;
            testq rsi, 0x1;
            jeq generic;
            //subq rdi, 1;
            //addq rdi, rsi;
            lea rdi, [rdi + rsi - 1];
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (add_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_subri(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            shlq rsi, 1;
            addq rsi, 1;
            movq rdi, [rdi];
            testq rdi, 0x1;
            jeq generic;
            subq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (sub_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_subrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        let generic = self.jit_gen.jit.label();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            testq rdi, 0x1;
            jeq generic;
            testq rsi, 0x1;
            jeq generic;
            subq rdi, rsi;
            addq rdi, 1;
            movq [r15], rdi;
        };
        self.fetch_and_dispatch();
        monoasm! { self.jit_gen.jit,
        generic:
            // generic path
            movq rax, (sub_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_mulrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            movq rax, (mul_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_divrr(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_rdi(); // rdi <- lhs addr
        self.vm_get_addr_rsi(); // rsi <- rhs addr
        self.vm_get_addr_r15(); // r15 <- ret addr
        monoasm! { self.jit_gen.jit,
            movq rdi, [rdi];
            movq rsi, [rsi];
            movq rax, (div_values);
            call rax;
            // store the result to return reg.
            movq [r15], rax;
        };
        self.fetch_and_dispatch();
        label
    }

    cmp_ops!(eq, ne, gt, ge, lt, le);
    cmp_ri_ops!(eq, ne, gt, ge, lt, le);

    fn vm_method_def(&mut self) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        monoasm! { self.jit_gen.jit,
            movq rdx, rdi;  // const_id
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (define_method);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jne branch;
        };
        self.fetch_and_dispatch();
        label
    }

    fn vm_condnotbr(&mut self, branch: DestLabel) -> CodePtr {
        let label = self.jit_gen.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit_gen.jit,
            movq r15, [r15];
            orq r15, 0x10;
            cmpq r15, (FALSE_VALUE);
            jeq branch;
        };
        self.fetch_and_dispatch();
        label
    }
}
