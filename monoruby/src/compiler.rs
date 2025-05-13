use std::hash::Hash;

use monoasm::*;
use monoasm_macro::monoasm;

mod invoker;
mod jit_module;
pub mod jitgen;
pub mod runtime;
mod vmgen;
mod wrapper;

use self::jitgen::asmir::AsmEvict;

use super::*;
use crate::bytecodegen::inst::*;
use crate::executor::*;

const OPECODE: i64 = 6;

type MethodInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    FuncId,
    Value,
    *const Value,
    usize,
    Option<BlockHandler>,
) -> Option<Value>;

type MethodInvoker2 = extern "C" fn(
    &mut Executor,
    &mut Globals,
    FuncId,
    Value,
    Arg,
    usize,
    Option<BlockHandler>,
) -> Option<Value>;

type BlockInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcData,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

type BindingInvoker = extern "C" fn(&mut Executor, &mut Globals, Lfp) -> Option<Value>;

type FiberInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcData,
    Value,
    *const Value,
    usize,
    &mut Executor,
) -> Option<Value>;

const COUNT_START_COMPILE: i32 = 20;
const COUNT_LOOP_START_COMPILE: i32 = 100;
const COUNT_RECOMPILE_ARECV_CLASS: i32 = 5;
const COUNT_DEOPT_RECOMPILE: i32 = 10;
const COUNT_DEOPT_RECOMPILE_SPECIALIZED: i32 = 50;

///
/// General purpose registers.
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum GP {
    Rax = 0,
    Rcx = 1,
    Rdx = 2,
    Rsp = 4,
    Rsi = 6,
    Rdi = 7,
    //R8 = 8,
    //R9 = 9,
    R13 = 13,
    R15 = 15,
}

///
/// Floating point registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Xmm(u16);

impl Xmm {
    fn new(id: u16) -> Self {
        Self(id)
    }

    pub fn enc(&self) -> u64 {
        self.0 as u64 + 2
    }
}

pub struct JitModule {
    pub(crate) jit: JitMemory,
    class_version: DestLabel,
    const_version: DestLabel,
    alloc_flag: DestLabel,
    sigint_flag: DestLabel,
    ///
    /// Raise error.
    ///
    /// ### in
    /// - r13: PC + 1
    /// - r14: LFP
    ///
    /// ### destroy
    /// - caller saved registers
    ///
    entry_raise: DestLabel,
    ///
    /// Execute GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax
    /// - stack
    ///
    exec_gc: DestLabel,
    ///
    /// Convert f64 to Value.
    ///
    /// ### in
    /// - xmm0: f64
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    f64_to_val: DestLabel,
    ///
    /// Dump stack trace and go panic.
    ///
    /// #### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    entry_panic: DestLabel,
    dispatch: Box<[CodePtr; 256]>,
    bop_redefined_flags: DestLabel,
    #[cfg(feature = "perf")]
    perf_file: std::fs::File,
}

impl std::ops::Deref for JitModule {
    type Target = JitMemory;
    fn deref(&self) -> &Self::Target {
        &self.jit
    }
}

impl std::ops::DerefMut for JitModule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.jit
    }
}

impl JitModule {
    pub(crate) fn signal_handler(
        &mut self,
        alloc_flag: DestLabel,
        sigint_flag: DestLabel,
    ) -> CodePtr {
        let codeptr = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            addl [rip + alloc_flag], 10;
            movl [rip + sigint_flag], 1;
            ret;
        }
        codeptr
    }

    pub(crate) fn get_address_pair(&mut self) -> (CodePtr, CodePtr) {
        assert_eq!(0, self.jit.get_page());
        let ptr0 = self.jit.get_current_address();
        self.jit.select_page(1);
        let ptr1 = self.jit.get_current_address();
        self.jit.select_page(0);
        (ptr0, ptr1)
    }

    pub(crate) fn get_wrapper_info(
        &mut self,
        pair: (CodePtr, CodePtr),
    ) -> (CodePtr, usize, CodePtr, usize) {
        let (ptr0, ptr1) = pair;
        assert_eq!(0, self.jit.get_page());
        let size0 = self.jit.get_current_address() - ptr0;
        self.jit.select_page(1);
        let size1 = self.jit.get_current_address() - ptr1;
        self.jit.select_page(0);
        (ptr0, size0 as usize, ptr1, size1 as usize)
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_write(&mut self, info: (CodePtr, usize, CodePtr, usize), desc: &str) {
        use std::io::Write;
        self.perf_file
            .write_all(format!("{:x} {:x} {desc}\n", info.0.as_ptr() as usize, info.1).as_bytes())
            .unwrap();
        self.perf_file
            .write_all(format!("{:x} {:x} {desc}\n", info.2.as_ptr() as usize, info.3).as_bytes())
            .unwrap();
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_info(&mut self, pair: (CodePtr, CodePtr), func_name: &str) {
        let info = self.get_wrapper_info(pair);
        self.perf_write(info, func_name);
    }

    ///
    /// Save caller-save registers (except rax) in stack.
    ///
    fn save_registers(&mut self) {
        monoasm! { &mut self.jit,
            subq rsp, 192;
            //movq [rsp + 176], rax;
            movq [rsp + 168], r11;
            movq [rsp + 160], r10;
            movq [rsp + 152], r9;
            movq [rsp + 144], r8;
            movq [rsp + 136], rcx;
            movq [rsp + 128], rdx;
            movq [rsp + 120], rsi;
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
        }
    }

    ///
    /// Restore caller-save registers (except rax) from stack.
    ///
    fn restore_registers(&mut self) {
        monoasm! { &mut self.jit,
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
            movq rsi, [rsp + 120];
            movq rdx, [rsp + 128];
            movq rcx, [rsp + 136];
            movq r8, [rsp + 144];
            movq r9, [rsp + 152];
            movq r10, [rsp + 160];
            movq r11, [rsp + 168];
            //movq rax, [rsp + 176];
            addq rsp, 192;
        }
    }
}

impl JitModule {
    ///
    /// Fetch instruction and dispatch.
    ///
    /// ### in
    /// - r13: BcPc
    ///
    /// ### destroy
    /// - rax, r15
    ///
    fn fetch_and_dispatch(&mut self) {
        monoasm! { &mut self.jit,
            movq r15, (self.dispatch.as_ptr());
            movzxb rax, [r13 + (OPECODE)]; // rax <- :0
            addq r13, 16;
            jmp [r15 + rax * 8];
        };
    }
    /// Set outer and self for block.
    ///
    /// ### in
    /// - rax: outer_lfp
    ///
    /// ### destroy
    /// - rsi
    ///
    fn set_block_self_outer(&mut self) {
        self.set_block_outer();
        monoasm! { &mut self.jit,
            // set self
            movq rsi, [rax - (LFP_SELF)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rsi;
        };
    }

    /// Set outer for block.
    ///
    /// ### in
    /// - rax: outer_lfp
    ///
    fn set_block_outer(&mut self) {
        monoasm! { &mut self.jit,
            // set outer
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], rax;
        };
    }

    /// Set outer.
    fn set_method_outer(&mut self) {
        monoasm! { &mut self.jit,
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], 0;
        };
    }

    ///
    /// Set lfp(r14) for callee.
    ///
    /// the local frame MUST BE on the stack.
    fn set_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // set lfp
            lea  r14, [rsp - (RSP_LOCAL_FRAME)];
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
        );
    }

    /// Push control frame and set outer.
    ///
    /// ### destroy
    /// - rdi, rsi
    ///
    fn push_frame(&mut self) {
        monoasm!( &mut self.jit,
            // push cfp
            movq rdi, [rbx + (EXECUTOR_CFP)];
            lea  rsi, [rsp - (RSP_CFP)];
            movq [rsi], rdi;
            movq [rbx + (EXECUTOR_CFP)], rsi;
        );
    }

    fn restore_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // restore lfp
            movq r14, [rbp - (BP_CFP + CFP_LFP)];
        );
    }

    /// Pop control frame
    fn pop_frame(&mut self) {
        monoasm!( &mut self.jit,
            // pop cfp
            lea  r14, [rbp - (BP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], r14;
        );
        self.restore_lfp();
    }

    ///
    /// Get FuncData.
    ///
    /// ### in
    /// - r12: &Globals
    /// - rdx: FuncId
    ///
    /// ### out
    /// - r15: &FuncData
    ///
    fn get_func_data(&mut self) {
        monoasm! { &mut self.jit,
            movl rdx, rdx;
            // assumes size_of::<FuncInfo>() is 64,
            shlq rdx, 6;
            addq rdx, [r12 + (GLOBALS_FUNCINFO)];
            lea  r15, [rdx + (FUNCINFO_DATA)];
        };
    }

    ///
    /// Get ProcData.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &Globals
    ///
    /// ### out
    /// - rax: outer_lfp: Option<LFP>
    /// - rdx: func_id: Option<FuncId>
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn get_proc_data(&mut self) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
    }

    ///
    /// ### in
    /// - r15: &FuncData
    ///
    fn call_funcdata(&mut self) -> CodePtr {
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
        }
        let return_addr = self.jit.get_current_address();
        self.pop_frame();
        return_addr
    }

    ///
    /// Invoke the function.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### out
    /// - rax: return value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn call_invoker(&mut self) {
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    ///
    /// Invoke the function.
    ///
    /// ### in
    /// - r15: &FuncData
    /// - r14: callee's Lfp
    ///
    /// ### out
    /// - rax: return value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn call_invoker_with_binding(&mut self) {
        self.push_frame();
        monoasm! { &mut self.jit,
            // set lfp
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    fn push_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            pushq r15;
            pushq r14;
            pushq r13;
            pushq r12;
            pushq rbx;
            pushq rbp;
        };
    }

    fn pop_callee_save(&mut self) {
        monoasm! { &mut self.jit,
            popq rbp;
            popq rbx;
            popq r12;
            popq r13;
            popq r14;
            popq r15;
        };
    }

    ///
    /// Push stack offset for callee.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### destoroy
    /// - rdi
    ///
    fn push_stack_offset(&mut self) {
        monoasm! { &mut self.jit,
            movzxw rdi, [r15 + (FUNCDATA_OFS)];
            shlq rdi, 4;
            addq rdi, 8;
            subq rsp, rdi;
            pushq rdi;
        }
    }

    ///
    /// Pop stack offset for callee.
    ///
    /// ### destoroy
    /// - rdi
    ///
    fn pop_stack_offset(&mut self) {
        monoasm! { &mut self.jit,
            popq rdi;
            addq rsp, rdi;
        }
    }

    ///
    /// Execute GC. (for interpreter)
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    /// - r13: PC + 1
    /// - r14: LFP
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax
    /// - stack
    ///
    fn vm_execute_gc(&mut self) {
        let raise = self.entry_raise.clone();
        self.execute_gc_inner(None, &raise);
    }

    ///
    /// Execute GC. (for JIT code)
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax, rcx
    /// - stack
    ///
    fn execute_gc(&mut self, wb: &jitgen::WriteBack, error: &DestLabel) {
        self.execute_gc_inner(Some(wb), error);
    }
}

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct Codegen {
    pub(crate) jit: JitModule,
    class_version_addr: *mut u32,
    const_version_addr: *mut u64,

    /// return_addr => (patch_point, deopt)
    return_addr_table: HashMap<CodePtr, (Option<CodePtr>, DestLabel)>,
    asm_return_addr_table: HashMap<AsmEvict, CodePtr>,
    pub(crate) specialized_patch_point: Vec<(ISeqId, ClassId, DestLabel)>,
    pub(crate) specialized_base: usize,
    vm_code_position: (Option<CodePtr>, usize, Option<CodePtr>, usize),
    vm_entry: DestLabel,
    vm_fetch: DestLabel,
    jit_class_guard_fail: DestLabel,

    div_by_zero: DestLabel,
    ///
    /// Get class id.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - rax: ClassId
    ///
    get_class: DestLabel,

    pub(crate) method_invoker: MethodInvoker,
    pub(crate) method_invoker2: MethodInvoker2,
    pub(crate) block_invoker: BlockInvoker,
    pub(crate) block_invoker_with_self: BlockInvoker,
    pub(crate) binding_invoker: BindingInvoker,
    ///
    /// Fiber invoker.
    ///
    /// ### in
    /// - `rdi`: &mut Executor
    /// - `rsi`: &mut Globals
    /// - `rdx`: &ProcInner
    /// - `rcx`: (dummy)
    /// - `r8`:  *args: *const Value
    /// - `r9`:  len: usize
    /// - `[rsp + 8]`: *mut Executor of child Fiber.
    ///
    pub(crate) fiber_invoker: FiberInvoker,
    pub(crate) fiber_invoker_with_self: FiberInvoker,
    pub(crate) resume_fiber: extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value>,
    pub(crate) yield_fiber: extern "C" fn(*mut Executor, Value) -> Option<Value>,
    pub(crate) startup_flag: bool,
    #[cfg(feature = "jit-log")]
    pub(crate) jit_compile_time: std::time::Duration,
}

impl std::ops::Deref for Codegen {
    type Target = JitModule;
    fn deref(&self) -> &Self::Target {
        &self.jit
    }
}

impl std::ops::DerefMut for Codegen {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.jit
    }
}

impl Codegen {
    pub fn new(no_jit: bool) -> Self {
        let mut jit = JitModule::new();
        let pair = jit.get_address_pair();

        let class_version_addr = jit.get_label_address(&jit.class_version).as_ptr() as *mut u32;
        let const_version_addr = jit.get_label_address(&jit.const_version).as_ptr() as *mut u64;
        let entry_panic = jit.entry_panic.clone();
        let get_class = jit.get_class();
        let method_invoker = jit.method_invoker();
        let method_invoker2 = jit.method_invoker2();
        let block_invoker = jit.block_invoker();
        let block_invoker_with_self = jit.block_invoker_with_self();
        let binding_invoker = jit.binding_invoker();
        let fiber_invoker = jit.fiber_invoker();
        let fiber_invoker_with_self = jit.fiber_invoker_with_self();
        let resume_fiber = jit.resume_fiber();
        let yield_fiber = jit.yield_fiber();

        let mut codegen = Self {
            jit,
            class_version_addr,
            const_version_addr,
            return_addr_table: HashMap::default(),
            asm_return_addr_table: HashMap::default(),
            specialized_patch_point: Vec::new(),
            specialized_base: 0,
            vm_entry: entry_panic.clone(),
            vm_code_position: (None, 0, None, 0),
            vm_fetch: entry_panic.clone(),
            jit_class_guard_fail: entry_panic.clone(),
            div_by_zero: entry_panic.clone(),
            get_class,
            method_invoker,
            method_invoker2,
            block_invoker,
            block_invoker_with_self,
            binding_invoker,
            fiber_invoker,
            fiber_invoker_with_self,
            resume_fiber,
            yield_fiber,
            startup_flag: false,
            #[cfg(feature = "jit-log")]
            jit_compile_time: std::time::Duration::default(),
        };
        codegen.construct_vm(no_jit);
        let signal_handler = codegen.signal_handler();
        codegen.jit.finalize();

        unsafe {
            use libc::{sighandler_t, SA_RESTART, SIGINT};
            let mut sa: libc::sigaction = std::mem::zeroed();

            sa.sa_sigaction = signal_handler.as_ptr() as sighandler_t;
            sa.sa_flags = SA_RESTART;
            libc::sigemptyset(&mut sa.sa_mask);

            if libc::sigaction(SIGINT, &sa, std::ptr::null_mut()) != 0 {
                panic!("Failed to set signal handler.");
            }
        }

        #[cfg(feature = "perf")]
        codegen.perf_info(pair, "monoruby-vm");

        let info = codegen.get_wrapper_info(pair);
        codegen.vm_code_position = (Some(info.0), info.1, Some(info.2), info.3);

        let address = codegen.jit.get_label_address(&codegen.alloc_flag).as_ptr() as *mut u32;
        alloc::ALLOC.with(|alloc| {
            alloc.borrow_mut().set_alloc_flag_address(address);
        });

        codegen
    }

    pub(crate) fn class_version_label(&self) -> DestLabel {
        self.class_version.clone()
    }
    pub(crate) fn const_version_label(&self) -> DestLabel {
        self.const_version.clone()
    }

    pub(crate) fn sigint_flag(&self) -> bool {
        let ptr = self.jit.get_label_address(&self.sigint_flag).as_ptr() as *mut u32;
        unsafe { *ptr != 0 }
    }

    pub(crate) fn unset_sigint_flag(&self) {
        let ptr = self.jit.get_label_address(&self.sigint_flag).as_ptr() as *mut u32;
        unsafe { *ptr = 0 }
    }

    pub(crate) fn signal_handler(&mut self) -> CodePtr {
        self.jit
            .signal_handler(self.alloc_flag.clone(), self.sigint_flag.clone())
    }

    pub(crate) fn entry_raise(&self) -> DestLabel {
        self.entry_raise.clone()
    }

    pub(crate) fn vm_entry(&self) -> DestLabel {
        self.vm_entry.clone()
    }

    pub(crate) fn vm_fetch(&self) -> DestLabel {
        self.vm_fetch.clone()
    }

    pub(crate) fn class_version(&self) -> u32 {
        unsafe { *self.class_version_addr }
    }

    pub(crate) fn class_version_inc(&self) {
        unsafe { *self.class_version_addr += 1 }
    }

    pub(crate) fn const_version_inc(&self) {
        unsafe { *self.const_version_addr += 1 }
    }

    pub(crate) fn bop_redefine_flags(&self) -> u32 {
        let addr = self
            .jit
            .get_label_address(&self.bop_redefined_flags)
            .as_ptr() as *mut u32;
        unsafe { *addr }
    }

    pub(crate) fn set_bop_redefine(&mut self) {
        let addr = self
            .jit
            .get_label_address(&self.bop_redefined_flags)
            .as_ptr() as *mut u32;
        unsafe { *addr = !0 }
        self.remove_vm_bop_optimization();
        #[cfg(any(test, feature = "jit-log"))]
        eprintln!("### basic op redefined.");
    }

    pub(crate) fn get_deopt_with_return_addr(
        &self,
        return_addr: CodePtr,
    ) -> Option<(Option<CodePtr>, DestLabel)> {
        self.return_addr_table.get(&return_addr).cloned()
    }

    ///
    /// Check whether *addr* is in VM code or invokers.
    ///
    pub(crate) fn check_vm_address(&self, addr: CodePtr) -> bool {
        let (start1, size1, start2, size2) = self.vm_code_position;
        let start1 = start1.unwrap();
        let start2 = start2.unwrap();
        (start1..start1 + size1).contains(&addr) || (start2..start2 + size2).contains(&addr)
    }

    fn icmp_teq(&mut self) {
        self.icmp_eq()
    }

    /*///
    /// Compare(<=>) Fixnums.
    ///
    /// ### in
    /// - rdi: lhs (must be Fixnum)
    /// - rsi: rhs (must be Fixnum)
    ///
    /// ### out
    /// - rax: result(Value)
    ///
    /// ### destroy
    /// - rdx
    ///
    fn icmp_cmp(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, (Value::from_ord(std::cmp::Ordering::Equal).id());
            movq rdx, (Value::from_ord(std::cmp::Ordering::Greater).id());
            cmpq rdi, rsi;
            cmovgtq rax, rdx;
            movq rdx, (Value::from_ord(std::cmp::Ordering::Less).id());
            cmovltq rax, rdx;
        };
    }*/

    ///
    /// check whether lhs and rhs are fixnum.
    ///
    fn guard_rdi_rsi_fixnum(&mut self, generic: &DestLabel) {
        self.guard_rdi_fixnum(generic);
        self.guard_rsi_fixnum(generic);
    }

    ///
    /// check whether lhs is fixnum.
    ///
    fn guard_rdi_fixnum(&mut self, generic: &DestLabel) {
        monoasm!( &mut self.jit,
            testq rdi, 0x1;
            jz generic;
        );
    }

    ///
    /// check whether rhs is fixnum.
    ///
    fn guard_rsi_fixnum(&mut self, generic: &DestLabel) {
        monoasm!( &mut self.jit,
            testq rsi, 0x1;
            jz generic;
        );
    }

    ///
    /// Call unary operator function.
    ///
    /// ### in
    /// - rdi: receiver
    ///
    /// ### out
    /// - rax: result
    ///
    fn call_unop(&mut self, func: UnaryOpFn) {
        monoasm!( &mut self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func as usize);
            call rax;
        );
    }

    fn call_binop(&mut self, func: BinaryOpFn) {
        monoasm!( &mut self.jit,
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
    }

    fn epilogue(&mut self) {
        monoasm!( &mut self.jit,
            leave;
            ret;
        );
    }

    ///
    /// Test whether the current local frame is on the heap.
    ///
    /// if the frame is on the heap, jump to *label*.
    ///
    fn branch_if_heap_frame(&mut self, label: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
            jnz label;
        }
    }

    ///
    /// Generate class guard stub for JIT code.
    ///
    /// ~~~text
    ///
    /// guard:
    ///     movq rdi, [r14 - (LFP_SELF)];
    ///     guard_class_rdi(self_class, vm_entry);
    /// patch_point:
    ///     jmp jit_entry;
    ///
    /// ~~~
    ///
    pub(super) fn class_guard_stub(
        &mut self,
        self_class: ClassId,
        patch_point: &DestLabel,
        jit_entry: &DestLabel,
        guard: &DestLabel,
    ) {
        let exit = self.jit_class_guard_fail.clone();
        let exit_patch_point = self.jit.label();
        let counter = self.jit.data_i32(COUNT_RECOMPILE_ARECV_CLASS);

        monoasm! { &mut self.jit,
        guard:
            movq rdi, [r14 - (LFP_SELF)];
        }
        self.guard_class_rdi(self_class, &exit_patch_point);
        monoasm! { &mut self.jit,
        patch_point:
            jmp jit_entry;
        }

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let cont = self.jit.label();
        let exit_patch_point_addr = self.jit.get_current_address();
        monoasm! { &mut self.jit,
        exit_patch_point:
            jmp cont;
        cont:
            subl [rip + counter], 1;
            jne exit;

            movq rdi, r12;
            movq rsi, r14;
            movq rdx, (exit_patch_point_addr.as_ptr());
            subq rsp, 4088;
            movq rax, (exec_jit_compile_patch as usize);
            call rax;
            addq rsp, 4088;
            jmp exit_patch_point;
        }
        self.jit.select_page(0);
    }

    ///
    /// Gen code for break in block.
    ///
    /// rbp <- bp for a context of the outer of the block.
    ///
    fn block_break(&mut self) {
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq r14, [r14];
            movq rdi, [rbx + (EXECUTOR_CFP)];
        loop_:
            movq rsi, [rdi];    // rdi <- caller's cfp
            cmpq r14, [rsi - (CFP_LFP)];
            je  exit;
            movq rdi, rsi;
            jmp loop_;
        exit:
            lea  rbp, [rdi + (BP_CFP)];
        }
    }

    ///
    /// Gen code for return in block.
    ///
    /// #### in
    /// - rax: return value
    /// - r13: pc + 1
    ///
    fn method_return(&mut self) {
        let raise = self.entry_raise();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rax;
            movq rax, (runtime::err_method_return);
            call rax;
            jmp  raise;
        }
    }

    /// Check whether *rdi*(Value) is true or not, and store boolean result (Value) to *rax*.
    ///
    /// #### destoroy
    /// - rdi
    ///
    fn not_rdi_to_rax(&mut self) {
        monoasm! { &mut self.jit,
            orq  rdi, 0x10;
            xorq rax, rax;
            cmpq rdi, (FALSE_VALUE);
            seteq rax;
            shlq rax, 3;
            orq rax, (FALSE_VALUE);
        }
    }

    ///
    /// Convert Fixnum to f64.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - R(*reg*)
    ///
    fn integer_val_to_f64(&mut self, reg: GP, xmm: Xmm) {
        monoasm!(&mut self.jit,
            sarq R(reg as _), 1;
            cvtsi2sdq xmm(xmm.enc()), R(reg as _);
        );
    }

    ///
    /// ### in
    /// - r15: &FuncData
    /// - r8: CallsiteId
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn generic_handle_arguments(
        &mut self,
        f: extern "C" fn(&mut Executor, &mut Globals, Lfp, Lfp, CallSiteId) -> Option<Value>,
    ) {
        monoasm! { &mut self.jit,
            // rcx <- callee LFP
            lea  rcx, [rsp - (RSP_LOCAL_FRAME)];
        }
        self.push_stack_offset();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            // rdi: &mut Executor
            // rsi: &mut Globals
            // rdx: caller LFP
            // rcx: callee LFP
            // r8: CallsiteId
            movq rax, (f);
            movq rdx, r14;
            call rax;
        }
        self.pop_stack_offset();
    }

    #[cfg(feature = "emit-asm")]
    pub(crate) fn dump_disas(
        &mut self,
        store: &Store,
        sourcemap: &Vec<(bytecodegen::BcIndex, usize)>,
        iseq_id: ISeqId,
    ) {
        let (start, code_end, end) = self.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.jit.select_page(0);
        let dump = self.jit.dump_code().unwrap();
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                let i = x.find(':').unwrap();
                (
                    match usize::from_str_radix(&x[0..i].trim(), 16) {
                        Ok(i) => i,
                        _ => {
                            panic!("{}", &x[0..i].trim());
                        }
                    },
                    x[i + 24..].to_string(),
                )
            })
            .collect();
        let iseq = &store[iseq_id];
        for (i, text) in dump {
            sourcemap
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
                    if iseq.bb_info.is_bb_head(bc_pos).is_some() {
                        eprintln!("  {:?}", iseq.bb_info.get_bb_id(bc_pos));
                    }
                    eprintln!(
                        "    {bc_pos} {}",
                        match iseq.trace_ir(store, bc_pos).format(store) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("      {:06x}: {}", i, text);
        }
    }
}

#[repr(C)]
struct InstanceVarCache {
    class_id: ClassId,
    ivar_id: IvarId,
}

extern "C" fn get_instance_var_with_cache(
    mut base: Value,
    name: IdentId,
    globals: &mut Globals,
    cache: &mut InstanceVarCache,
) -> Value {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => return Value::nil(),
    };
    if class_id == cache.class_id {
        return rval.get_ivar_by_ivarid(cache.ivar_id).unwrap_or_default();
    }
    let ivar_id = globals.store.get_ivar_id(class_id, name);
    *cache = InstanceVarCache { class_id, ivar_id };
    rval.get_ivar_by_ivarid(ivar_id).unwrap_or_default()
}

extern "C" fn set_instance_var_with_cache(
    vm: &mut Executor,
    globals: &mut Globals,
    mut base: Value,
    name: IdentId,
    val: Value,
    cache: &mut InstanceVarCache,
) -> Option<Value> {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => {
            vm.err_cant_modify_frozen(&globals.store, base);
            return None;
        }
    };
    if class_id == cache.class_id {
        rval.set_ivar_by_ivarid(cache.ivar_id, val);
        return Some(Value::nil());
    }
    let ivar_id = globals.store.get_ivar_id(class_id, name);
    let new_cache = InstanceVarCache { class_id, ivar_id };
    *cache = new_cache;
    rval.set_ivar_by_ivarid(ivar_id, val);
    Some(Value::nil())
}

#[cfg(feature = "profile")]
extern "C" fn guard_fail(vm: &mut Executor, globals: &mut Globals, self_val: Value) {
    let func_id = vm.cfp().lfp().meta().func_id();
    globals.jit_class_guard_failed(func_id, self_val.class());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false);

        for (class, value) in [
            (INTEGER_CLASS, Value::integer(-2558)),
            (INTEGER_CLASS, Value::integer(i32::MAX as i64)),
            (INTEGER_CLASS, Value::integer(i32::MIN as i64)),
            (FLOAT_CLASS, Value::float(1.44e-17)),
            (FLOAT_CLASS, Value::float(0.0)),
            (FLOAT_CLASS, Value::float(f64::NAN)),
            (FLOAT_CLASS, Value::float(f64::INFINITY)),
            (FLOAT_CLASS, Value::float(f64::NEG_INFINITY)),
            (FLOAT_CLASS, Value::float(f64::MAX)),
            (FLOAT_CLASS, Value::float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
            (SYMBOL_CLASS, Value::symbol_from_str("Ruby")),
            (TRUE_CLASS, Value::bool(true)),
            (FALSE_CLASS, Value::bool(false)),
            (ARRAY_CLASS, Value::array_from_vec(vec![])),
            (HASH_CLASS, Value::hash(IndexMap::default())),
            (STRING_CLASS, Value::string_from_str("Ruby")),
        ] {
            let func = gen.jit.get_label_addr(&gen.get_class);

            assert_eq!(class, func(value))
        }
    }

    #[test]
    fn test_f64_to_val() {
        let mut gen = Codegen::new(false);

        for f in [
            1.44e-17,
            0.0,
            1285.333,
            -7512.0255,
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MAX,
            f64::MIN,
        ] {
            let f64_to_val = gen.f64_to_val.clone();
            let func: extern "C" fn(f64) -> Value = gen.jit.get_label_addr(&f64_to_val);
            if f.is_nan() {
                assert!(func(f).try_float().unwrap().is_nan())
            } else {
                assert_eq!(Value::float(f).try_float(), func(f).try_float());
            }
        }
    }
}

impl Globals {
    pub(super) fn exec_jit_compile(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
        is_recompile: bool,
    ) {
        #[cfg(feature = "profile")]
        {
            if is_recompile {
                self.countup_recompile(self.store[iseq_id].func_id(), self_class);
            }
        }
        self.codegen.jit_compile(
            &self.store,
            iseq_id,
            self_class,
            position,
            entry_label,
            is_recompile,
        );
    }

    ///
    /// Compile the Ruby method.
    ///
    pub(super) fn exec_jit_compile_method(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        jit_entry: DestLabel,
        is_recompile: bool,
    ) {
        self.exec_jit_compile(iseq_id, self_class, None, jit_entry, is_recompile)
    }
}
