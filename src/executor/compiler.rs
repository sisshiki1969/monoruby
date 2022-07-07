#[cfg(any(feature = "emit-asm", feature = "log-jit"))]
use std::time::Instant;

use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

use super::*;

mod jitgen;
mod vmgen;

pub type EntryPoint = extern "C" fn(&mut Interp, &mut Globals, *const FuncData) -> Option<Value>;

pub type Invoker = extern "C" fn(
    &mut Interp,
    &mut Globals,
    *const FuncData,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct Codegen {
    pub jit: JitMemory,
    pub class_version: DestLabel,
    pub const_version: DestLabel,
    pub entry_panic: DestLabel,
    pub vm_entry: CodePtr,
    pub entry_point: EntryPoint,
    pub entry_point_return: CodePtr,
    entry_find_method: DestLabel,
    pub vm_return: DestLabel,
    pub dispatch: Vec<CodePtr>,
    pub invoker: Invoker,
    pub func_data: FuncDataLabels,
}

pub struct FuncDataLabels {
    pub func_regnum: DestLabel,
    pub func_address: DestLabel,
    pub func_pc: DestLabel,
    pub func_meta: DestLabel,
}

fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}

//
// Runtime functions.
//

///
/// Get an absolute address of the given method.
///
/// If no method was found, return None (==0u64).
///
pub extern "C" fn jit_find_method(
    interp: &mut Interp,
    globals: &mut Globals,
    func_name: IdentId,
    args_len: usize,
    receiver: Value,
    funcid_patch: *mut Meta,
) -> Option<CodePtr> {
    let func_id = globals.get_method(receiver.class_id(), func_name, args_len)?;
    let data = interp.get_func_data(globals, func_id);
    unsafe { funcid_patch.write_unaligned(data.meta) };
    Some(data.codeptr.unwrap())
}

extern "C" fn define_method(
    _interp: &mut Interp,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) {
    globals.class.add_method(OBJECT_CLASS, name, func);
}

pub extern "C" fn unimplemented_inst(_: &mut Interp, _: &mut Globals) {
    panic!("unimplemented inst.");
}

pub extern "C" fn panic(_: &mut Interp, _: &mut Globals) {
    panic!("panic in jit code.");
}

extern "C" fn get_error_location(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
) {
    let normal_info = globals.func[func_id].as_normal();
    let sourceinfo = normal_info.sourceinfo.clone();
    let bc_base = globals.func[func_id].data.pc;
    let loc = normal_info.sourcemap[pc - bc_base];
    globals.push_error_location(loc, sourceinfo);
}

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            fn [<cmp_ $op>](&mut self) {
                let exit = self.jit.label();
                let generic = self.jit.label();
                self.guard_rdi_fixnum(generic);
                self.guard_rsi_fixnum(generic);
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
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
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
            fn [<cmp_opt_ $op>](&mut self) {
                let generic = self.jit.label();
                let exit = self.jit.label();
                self.guard_rdi_fixnum(generic);
                self.guard_rsi_fixnum(generic);
                monoasm! { self.jit,
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    testq rax, 0x1;
                exit:
                // if cond was met, Z=0(not set).
                };
                self.jit.select(1);
                monoasm!(self.jit,
                generic:
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                    orq rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                    jmp  exit;
                );
                self.jit.select(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_opt_main!($op1);
        cmp_opt_main!($($op2),+);
    };
}

macro_rules! cmp_ri_main {
    ($op:ident) => {
        paste! {
            fn [<cmp_ri_ $op>](&mut self) {
                let exit = self.jit.label();
                let generic = self.jit.label();
                self.guard_rdi_fixnum(generic);
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
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                    jmp  exit;
                );
                self.jit.select(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_ri_main!($op1);
        cmp_ri_main!($($op2),+);
    };
}

macro_rules! cmp_opt_ri_main {
    ($op:ident) => {
        paste! {
            fn [<cmp_opt_ri_ $op>](&mut self) {
                let exit = self.jit.label();
                let generic = self.jit.label();
                self.guard_rdi_fixnum(generic);
                monoasm! { self.jit,
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    testq rax, 0x1;
                exit:
                // if cond was met, Z=0(not set).
                };

                self.jit.select(1);
                monoasm!(self.jit,
                generic:
                    // generic path
                    movq rax, ([<cmp_ $op _values>]);
                    call rax;
                    orq rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                    jmp  exit;
                );
                self.jit.select(0);
            }
        }
    };
    ($op1:ident, $($op2:ident),+) => {
        cmp_opt_ri_main!($op1);
        cmp_opt_ri_main!($($op2),+);
    };
}

impl Codegen {
    pub fn new() -> Self {
        let mut jit = JitMemory::new();
        jit.add_page();
        let class_version = jit.const_i64(0);
        let const_version = jit.const_i64(0);
        let entry_panic = jit.label();
        let entry_find_method = jit.label();
        let jit_return = jit.label();
        let vm_return = jit.label();
        let func_regnum = jit.const_i64(0);
        let func_address = jit.const_i64(0);
        let func_pc = jit.const_i64(0);
        let func_meta = jit.const_i64(0);
        jit.select(1);
        monoasm!(&mut jit,
        entry_panic:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (panic);
            jmp rax;
        entry_find_method:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (jit_find_method);
            jmp  rax;
        vm_return:
            // check call_kind.
            movl r15, [rbp - 8];
            testq r15, r15;
            jne  jit_return;
            // save return value
            movq r15, rax;
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [rbp - 0x4];
            movq rcx, r13;
            subq rcx, 8;
            movq rax, (get_error_location);
            call rax;
            // restore return value
            movq rax, r15;
        jit_return:
            leave;
            ret;
        );
        // method invoker.
        let invoker: extern "C" fn(
            &mut Interp,
            &mut Globals,
            *const FuncData,
            Value,
            *const Value,
            usize,
        ) -> Option<Value> = unsafe { std::mem::transmute(jit.get_current_address().as_ptr()) };
        let loop_exit = jit.label();
        let loop_ = jit.label();
        // rdi: &mut Interp
        // rsi: &mut Globals
        // rdx: *const FuncData
        // rcx: receiver: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        monoasm! { &mut jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            movq rbx, rdi;
            movq r12, rsi;
            // set meta/func_id
            movq rax, [rdx + (FUNCDATA_OFFSET_META)];
            movq [rsp - 0x18], rax;
            //movq rax, [rdx + (FUNCDATA_OFFSET_REGNUM)];
            //movw [rsp - 0x16], rax;
            // set self (= receiver)
            movq [rsp - 0x20], rcx;

            movq r13, [rdx + (FUNCDATA_OFFSET_PC)];    // r13: BcPc
            //
            //       +-------------+
            // +0x08 |             |
            //       +-------------+
            //  0x00 |             | <- rsp
            //       +-------------+
            // -0x08 | return addr |
            //       +-------------+
            // -0x10 |   old rbp   |
            //       +-------------+
            // -0x18 |    meta     | func_id
            //       +-------------+
            // -0x20 |     %0      | receiver
            //       +-------------+
            // -0x28 | %1(1st arg) |
            //       +-------------+
            //       |             |
            //
            // r8 <- *args
            // r9 <- len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            negq r9;
        loop_:
            movq rax, [r8 + r9 * 8 + 8];
            movq [rsp + r9 * 8- 0x20], rax;
            addq r9, 1;
            jne  loop_;
        loop_exit:

            movq rax, [rdx + (FUNCDATA_OFFSET_CODEPTR)];
            call rax;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };

        // dispatch table.
        let entry_unimpl = jit.get_current_address();
        monoasm! { jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rax, (super::compiler::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        jit.select(0);
        let dispatch = vec![entry_unimpl; 256];
        let func_data = FuncDataLabels {
            func_regnum,
            func_address,
            func_pc,
            func_meta,
        };
        let mut codegen = Self {
            jit,
            class_version,
            const_version,
            entry_panic,
            entry_find_method,
            vm_entry: entry_unimpl,
            entry_point: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            entry_point_return: entry_unimpl,
            vm_return,
            dispatch,
            invoker,
            func_data,
        };
        codegen.construct_vm();
        codegen.get_entry_point();
        codegen.jit.finalize();
        codegen
    }

    fn calc_offset(&mut self) {
        monoasm!(self.jit,
            addq rax, 1;
            andq rax, (-2);
            shlq rax, 3;
            addq rax, 16;
        );
    }

    fn guard_rdi_rsi_fixnum(&mut self, generic: DestLabel) {
        self.guard_rdi_fixnum(generic);
        self.guard_rsi_fixnum(generic);
    }

    fn guard_rdi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether lhs is fixnum.
            testq rdi, 0x1;
            jeq generic;
        );
    }

    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether rhs is fixnum.
            testq rsi, 0x1;
            jeq generic;
        );
    }

    cmp_main!(eq, ne, lt, le, gt, ge);
    cmp_opt_main!(eq, ne, lt, le, gt, ge);
    cmp_ri_main!(eq, ne, lt, le, gt, ge);
    cmp_opt_ri_main!(eq, ne, lt, le, gt, ge);

    fn call_unop(&mut self, func: u64, entry_return: DestLabel) {
        monoasm!(self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
            testq rax, rax;
            jeq entry_return;
        );
    }

    fn call_binop(&mut self, func: u64, entry_return: DestLabel) {
        monoasm!(self.jit,
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
            testq rax, rax;
            jeq entry_return;
        );
    }

    ///
    /// ## stack layout for JIT-ed code (just after prologue).
    ///
    ///~~~text
    ///       +-------------+
    /// +0x08 | return addr |
    ///       +-------------+
    ///  0x00 |  prev rbp   | <- rbp
    ///       +-------------+  
    /// -0x08 |    meta     |  
    ///       +-------------+  
    /// -0x10 |     %0      |
    ///       +-------------+
    /// -0x18 |     %1      |
    ///       +-------------+
    ///       |      :      |
    ///       +-------------+
    /// -0xy0 |    %(n-1)   | <- rsp
    ///       +-------------+
    ///       |      :      |
    /// ~~~

    /// ## ABI of JIT-compiled code.
    ///
    /// ### argument registers:
    ///  - rdi: number pf args
    ///
    /// ### global registers:
    ///  - rbx: &mut Interp
    ///  - r12: &mut Globals
    ///  - r13: pc (dummy for JIT-ed code)
    ///
    /// ## stack layout when just after the code is called
    /// ~~~text
    ///       +-------------+
    /// -0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |  (old rbp)  |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |     %0      |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///       |             |
    /// ~~~~
    ///
    ///  - meta and arguments is set by caller.
    ///  - (old rbp) is to be set by callee.
    ///

    pub fn compile_on_demand(&mut self, globals: &mut Globals, func_id: FuncId) -> CodePtr {
        match globals.func[func_id].data.codeptr {
            Some(dest) => dest,
            None => {
                let mut info = std::mem::take(&mut globals.func[func_id]);
                let label = self.jit_compile(&mut info, &globals.func);
                //info.data.codeptr = Some(label);
                globals.func[func_id] = info;
                label
            }
        }
    }

    fn jit_compile(&mut self, func: &mut FuncInfo, store: &FnStore) -> CodePtr {
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            eprintln!(
                "--> start compile: {} {:?}",
                match func.name() {
                    Some(name) => name,
                    None => "<unnamed>",
                },
                func.data.meta.func_id()
            );
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = Instant::now();
        let label = match &func.kind {
            FuncKind::Normal(info) => self.jit_compile_normal(info, store),
            FuncKind::Builtin { abs_address } => self.wrap_builtin(*abs_address),
        };
        func.data.codeptr = Some(label);
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
            if let FuncKind::Normal(_) = func.kind {
                self.jit.select(0);
                eprintln!("{}", self.jit.dump_code().unwrap());
            }
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("<-- finished compile. elapsed:{:?}", elapsed);
        label
    }

    pub fn wrap_builtin(&mut self, abs_address: u64) -> CodePtr {
        //
        // generate a wrapper for a builtin function which has C ABI.
        // stack layout at the point of just after a wrapper was called.
        //
        //       +-------------+
        //  0x00 | return addr | <- rsp
        //       +-------------+
        // -0x08 |             |
        //       +-------------+
        // -0x10 |    meta     |
        //       +-------------+
        // -0x18 |  %0 (self)  |
        //       +-------------+
        // -0x20 | %1(1st arg) |
        //       +-------------+
        //
        //  meta
        // +-------------------+ -0x08
        // |0:VM 1:JIT 2:Native|
        // +-------------------+ -0x0a
        // |    register_len   |
        // +-------------------+ -0x0c
        // |                   |
        // +      FuncId       + -0x0e
        // |                   |
        // +-------------------+ -0x10
        //
        // argument registers:
        //   rdi: number of args
        //
        // global registers:
        //   rbx: &mut Interp
        //   r12: &mut Globals
        //   r13: pc (dummy for builtin funcions)
        //
        let label = self.jit.get_current_address();
        // calculate stack offset
        monoasm!(self.jit,
            movq rcx, rdi;
            movq rax, rdi;
        );
        self.calc_offset();
        monoasm!(self.jit,
            lea  rdx, [rsp - 0x20];
            movw [rsp - 0x0c], rdi;
            movw [rsp - 0x0a], 2;
            pushq rbp;
            movq rbp, rsp;

            movq rdi, rbx;
            movq rsi, r12;
            subq rsp, rax;
            movq rax, (abs_address);
            // fn(&mut Interp, &mut Globals, *const Value, len:usize)
            call rax;

            leave;
            ret;
        );
        label
    }
}

impl Codegen {
    pub fn precompile(&mut self, store: &mut FnStore) {
        for func in store.funcs_mut().iter_mut() {
            match &func.kind {
                FuncKind::Normal(_) => {
                    func.data.codeptr = Some(self.vm_entry);
                }
                _ => {}
            };
        }
    }
}
