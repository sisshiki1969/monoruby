use std::hash::Hash;

use monoasm::*;
use monoasm_macro::monoasm;

mod invoker;
pub mod jitgen;
pub mod runtime;
mod vmgen;
mod wrapper;

use self::jitgen::asmir::AsmEvict;

use super::*;
use crate::bytecodegen::inst::*;
use crate::executor::*;

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
    &ProcInner,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

type BindingInvoker = extern "C" fn(&mut Executor, &mut Globals, Lfp) -> Option<Value>;

type FiberInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcInner,
    Value,
    *const Value,
    usize,
    &mut Executor,
) -> Option<Value>;

const COUNT_START_COMPILE: i32 = 20;
const COUNT_LOOP_START_COMPILE: i32 = 40;
const COUNT_RECOMPILE_ARECV_CLASS: i32 = 5;
const COUNT_DEOPT_RECOMPILE: i32 = 5;

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
    fn new() -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        let const_version = jit.data_i64(1);
        jit.finalize();
        #[cfg(feature = "perf")]
        let perf_file = {
            let pid = std::process::id();
            let temp_file = format!("/tmp/perf-{pid}.map");
            let file = match std::fs::File::create(&temp_file) {
                Err(why) => panic!("couldn't create {}: {}", temp_file, why),
                Ok(file) => file,
            };
            file
        };
        Self {
            jit,
            class_version,
            const_version,
            bop_redefined_flags,
            #[cfg(feature = "perf")]
            perf_file,
        }
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
}

impl JitModule {
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
    alloc_flag: DestLabel,

    /// return_addr => (patch_point, deopt)
    return_addr_table: HashMap<CodePtr, (Option<CodePtr>, DestLabel)>,
    asm_return_addr_table: HashMap<AsmEvict, CodePtr>,
    #[allow(dead_code)]
    pub(crate) entry_panic: DestLabel,
    pub(crate) vm_entry: DestLabel,
    vm_code_position: (Option<CodePtr>, usize, Option<CodePtr>, usize),
    vm_fetch: DestLabel,
    jit_class_guard_fail: DestLabel,
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
    dispatch: Box<[CodePtr; 256]>,
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
        let alloc_flag = jit.data_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });

        let class_version_addr = jit.get_label_address(jit.class_version).as_ptr() as *mut u32;
        let const_version_addr = jit.get_label_address(jit.const_version).as_ptr() as *mut u64;
        let entry_panic = jit.entry_panic();
        let get_class = jit.get_class();
        let f64_to_val = jit.f64_to_val();
        let entry_unimpl = jit.unimplemented_inst();
        let method_invoker = jit.method_invoker();
        let method_invoker2 = jit.method_invoker2();
        let block_invoker = jit.block_invoker();
        let block_invoker_with_self = jit.block_invoker_with_self();
        let binding_invoker = jit.binding_invoker();
        let fiber_invoker = jit.fiber_invoker();
        let fiber_invoker_with_self = jit.fiber_invoker_with_self();
        let resume_fiber = jit.resume_fiber();
        let yield_fiber = jit.yield_fiber();

        // dispatch table.
        let dispatch = vec![entry_unimpl; 256];
        let mut codegen = Self {
            jit,
            class_version_addr,
            const_version_addr,
            alloc_flag,
            return_addr_table: HashMap::default(),
            asm_return_addr_table: HashMap::default(),
            entry_panic,
            vm_entry: entry_panic,
            vm_code_position: (None, 0, None, 0),
            vm_fetch: entry_panic,
            jit_class_guard_fail: entry_panic,
            entry_raise: entry_panic,
            f64_to_val,
            div_by_zero: entry_panic,
            get_class,
            dispatch: dispatch.into_boxed_slice().try_into().unwrap(),
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
        codegen.jit.finalize();

        let address = codegen.jit.get_label_address(alloc_flag).as_ptr() as *mut u32;
        alloc::ALLOC.with(|alloc| {
            alloc.borrow_mut().set_alloc_flag_address(address);
        });
        codegen
    }

    pub(crate) fn class_version_label(&self) -> DestLabel {
        self.class_version
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
            .get_label_address(self.bop_redefined_flags)
            .as_ptr() as *mut u32;
        unsafe { *addr }
    }

    pub(crate) fn set_bop_redefine(&mut self) {
        let addr = self
            .jit
            .get_label_address(self.bop_redefined_flags)
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

    ///
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
    }

    ///
    /// Execute garbage collection.
    ///
    fn execute_gc(&mut self, wb: Option<&jitgen::WriteBack>) {
        let alloc_flag = self.alloc_flag;
        let gc = self.jit.label();
        let exit = self.jit.label();
        assert_eq!(0, self.jit.get_page());
        monoasm! { &mut self.jit,
            cmpl [rip + alloc_flag], 0;
            jne  gc;
        exit:
        };
        self.jit.select_page(1);
        self.jit.bind_label(gc);
        if let Some(wb) = wb {
            self.gen_write_back(wb);
        }
        self.save_registers();
        monoasm! { &mut self.jit,
            movq rdi, r12;
            movq rsi, rbx;
            movq rax, (execute_gc);
            call rax;
        }
        self.restore_registers();
        monoasm! { &mut self.jit,
            jmp exit;
        }
        self.jit.select_page(0);
    }

    /// ## in
    /// - rax : CodePtr
    ///
    /// ## out
    /// - rax : result
    fn call_rax(&mut self) {
        self.push_frame();
        self.set_lfp();
        monoasm!( &mut self.jit,
            call rax;   // CALL_SITE
        );
        self.pop_frame();
    }

    ///
    /// check whether lhs and rhs are fixnum.
    ///
    fn guard_rdi_rsi_fixnum(&mut self, generic: DestLabel) {
        self.guard_rdi_fixnum(generic);
        self.guard_rsi_fixnum(generic);
    }

    ///
    /// check whether lhs is fixnum.
    ///
    fn guard_rdi_fixnum(&mut self, generic: DestLabel) {
        monoasm!( &mut self.jit,
            testq rdi, 0x1;
            jz generic;
        );
    }

    ///
    /// check whether rhs is fixnum.
    ///
    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
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
    fn branch_if_heap_frame(&mut self, label: DestLabel) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
            jnz label;
        }
    }

    fn save_registers(&mut self) {
        monoasm! { &mut self.jit,
            subq rsp, 192;
            movq [rsp + 176], rax;
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
            movq rax, [rsp + 176];
            addq rsp, 192;
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
        patch_point: DestLabel,
        jit_entry: DestLabel,
        guard: DestLabel,
    ) {
        let exit = self.jit_class_guard_fail;
        let exit_patch_point = self.jit.label();
        let counter = self.jit.data_i32(COUNT_RECOMPILE_ARECV_CLASS);

        monoasm! { &mut self.jit,
        guard:
            movq rdi, [r14 - (LFP_SELF)];
        }
        self.guard_class_rdi(self_class, exit_patch_point);
        monoasm! { &mut self.jit,
        patch_point:
            jmp jit_entry;
        }

        self.jit.select_page(1);
        let cont = self.jit.label();
        monoasm! { &mut self.jit,
        exit_patch_point:
            jmp cont;
        cont:
            subl [rip + counter], 1;
            jne exit;

            movq rdi, r12;
            movq rsi, r14;
            movl rdx, (exit_patch_point.to_usize());
            subq rsp, 4088;
            movq rax, (exec_jit_compile_patch as usize);
            call rax;
            addq rsp, 4088;
            jmp exit_patch_point;
        }
        self.jit.select_page(0);
        self.jit.finalize();
    }

    ///
    /// Gen code for break in block.
    ///
    /// rbp <- bp for a context which called the block.
    ///
    fn block_break(&mut self) {
        monoasm! { &mut self.jit,
            movq rdi, [rbx + (EXECUTOR_CFP)];
            movq rdi, [rdi];    // rdi <- caller's cfp
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
        let raise = self.entry_raise;
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
    /// Assume the Value is Integer, and convert to f64.
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
    /// - rdx: src: *const Value
    /// - r8: CallsiteId
    ///
    /// ### out
    /// - rax: arg_num: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn generic_handle_arguments(
        &mut self,
        f: extern "C" fn(
            &mut Executor,
            &mut Globals,
            *const Value,
            Lfp,
            CallSiteId,
        ) -> Option<Value>,
    ) {
        monoasm! { &mut self.jit,
            // rcx <- callee LFP
            lea  rcx, [rsp - (RSP_LOCAL_FRAME)];
            // rdi <- stack_offset
            movzxw rdi, [r15 + (FUNCDATA_OFS)];
            shlq rdi, 4;
            addq rdi, 8;
            subq rsp, rdi;
            pushq rdi;
            movq rdi, rbx;
            movq rsi, r12;
            // rdi: &mut Executor
            // rsi: &mut Globals
            // rdx: src: *const Value
            // rcx: callee LFP
            // r8: CallsiteId
            movq rax, (f);
            call rax;
            popq rdi;
            addq rsp, rdi;
        };
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
                        eprintln!("{:?}", iseq.bb_info.get_bb_id(bc_pos));
                    }
                    eprintln!(
                        "{bc_pos} {}",
                        match iseq.trace_ir(store, bc_pos).format(store) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
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
            let func = gen.jit.get_label_addr(gen.get_class);

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
            let func: extern "C" fn(f64) -> Value = gen.jit.get_label_addr(gen.f64_to_val);
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
    ) {
        self.codegen
            .jit_compile(&self.store, iseq_id, self_class, position, entry_label);
    }

    ///
    /// Compile the Ruby method.
    ///
    pub(super) fn exec_jit_compile_method(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        jit_entry: DestLabel,
    ) {
        self.exec_jit_compile(iseq_id, self_class, None, jit_entry)
    }
}
