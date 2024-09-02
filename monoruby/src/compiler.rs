use std::hash::Hash;

use monoasm::*;
use monoasm_macro::monoasm;

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

#[cfg(feature = "test")]
const COUNT_START_COMPILE: i32 = 5;
#[cfg(not(feature = "test"))]
const COUNT_START_COMPILE: i32 = 20;
const COUNT_RECOMPILE_ARECV_CLASS: i32 = 5;
const COUNT_DEOPT_RECOMPILE: i32 = 5;

///
/// General purpose registers.
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum GP {
    Rax = 0,
    //Rcx = 1,
    Rdx = 2,
    Rsp = 4,
    Rsi = 6,
    Rdi = 7,
    R8 = 8,
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

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct Codegen {
    pub(crate) jit: JitMemory,
    class_version: DestLabel,
    class_version_addr: *mut u32,
    alloc_flag: DestLabel,
    const_version: DestLabel,
    bop_redefined_flags: DestLabel,
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
    #[cfg(feature = "jit-log")]
    pub(crate) jit_compile_time: std::time::Duration,
    #[cfg(feature = "perf")]
    pub(crate) perf_file: std::fs::File,
}

impl Codegen {
    pub fn new(no_jit: bool) -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.data_i32(1);
        let bop_redefined_flags = jit.data_i32(0);
        let const_version = jit.data_i64(1);
        let alloc_flag = jit.data_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });

        let entry_panic = entry_panic(&mut jit);
        let get_class = get_class(&mut jit);
        let f64_to_val = f64_to_val(&mut jit);
        let entry_unimpl = unimplemented_inst(&mut jit);

        // dispatch table.
        let dispatch = vec![entry_unimpl; 256];
        let mut codegen = Self {
            jit,
            class_version,
            class_version_addr: std::ptr::null_mut(),
            alloc_flag,
            const_version,
            bop_redefined_flags,
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
            method_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            method_invoker2: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            block_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            block_invoker_with_self: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            binding_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            fiber_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            fiber_invoker_with_self: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            resume_fiber: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            yield_fiber: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            #[cfg(feature = "jit-log")]
            jit_compile_time: std::time::Duration::default(),
            #[cfg(feature = "perf")]
            perf_file: {
                let pid = std::process::id();
                let temp_file = format!("/tmp/perf-{pid}.map");
                let file = match std::fs::File::create(&temp_file) {
                    Err(why) => panic!("couldn't create {}: {}", temp_file, why),
                    Ok(file) => file,
                };
                file
            },
        };
        codegen.construct_vm(no_jit);
        codegen.jit.finalize();

        codegen.class_version_addr =
            codegen.jit.get_label_address(class_version).as_ptr() as *mut u32;
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

    pub(crate) fn set_deopt_with_return_addr(
        &mut self,
        return_addr: CodePtr,
        evict: AsmEvict,
        evict_label: DestLabel,
    ) {
        self.asm_return_addr_table.insert(evict, return_addr);
        self.return_addr_table
            .insert(return_addr, (None, evict_label));
    }

    pub(crate) fn set_deopt_patch_point_with_return_addr(
        &mut self,
        return_addr: CodePtr,
        patch_point: CodePtr,
    ) {
        self.return_addr_table
            .entry(return_addr)
            .and_modify(|e| e.0 = Some(patch_point));
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

    fn restore_lbp(&mut self) {
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
        self.restore_lbp();
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
            movq [rsp - (RSP_STACK_LFP + LFP_SELF)], rsi;
        };
    }

    /// Set outer for block.
    ///
    /// ### in
    /// - rax: outer_lfp
    ///
    /// ### destroy
    /// - rsi
    ///
    fn set_block_outer(&mut self) {
        monoasm! { &mut self.jit,
            // set outer
            lea  rsi, [rax - (LFP_OUTER)];
            movq [rsp - (RSP_STACK_LFP + LFP_OUTER)], rsi;
        };
    }

    /// Set outer.
    fn set_method_outer(&mut self) {
        monoasm! { &mut self.jit,
            movq [rsp - (RSP_STACK_LFP + LFP_OUTER)], 0;
        };
    }

    ///
    /// Set lfp(r14) for callee.
    ///
    /// the local frame MUST BE on the stack.
    fn set_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // set lfp
            lea  r14, [rsp - (RSP_STACK_LFP)];
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
        );
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
            call rax;
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

    fn test_heap_frame(&mut self) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
        }
    }

    fn save_registers(&mut self) {
        monoasm! { &mut self.jit,
            subq rsp, 176;
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
            addq rsp, 176;
        }
    }

    ///
    /// Generate class guard stub for JIT code.
    ///
    /// ~~~text
    ///
    /// guard:
    ///     movq rdi, [r14 - (LBP_SELF)];
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
            movq rax, (exec_jit_compile_patch);
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
    /// side-exit if not Integer.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - none
    fn integer_val_to_f64(&mut self, reg: GP, xmm: Xmm, side_exit: DestLabel) {
        let l1 = self.jit.label();
        monoasm!(&mut self.jit,
            testq R(reg as _), 0b01;
            jz l1;
            sarq R(reg as _), 1;
            cvtsi2sdq xmm(xmm.enc()), R(reg as _);
        );
        self.jit.select_page(1);
        monoasm!(&mut self.jit,
        l1:
            movq rdi, R(reg as _);
            jmp side_exit;
        );
        self.jit.select_page(0);
    }

    ///
    /// ### in
    /// - r15: &FuncData
    /// - rdx: src: *const Value
    /// - r8: CallsiteId
    /// - r9: arg_num
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
            lea  rcx, [rsp - (RSP_STACK_LFP)];
            // rdi <- stacck_offset
            movzxw rdi, [r15 + (FUNCDATA_OFS)];
            shlq rdi, 4;
            addq rdi, 24;
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

    pub(crate) fn get_address_pair(&mut self) -> (CodePtr, CodePtr) {
        assert_eq!(0, self.jit.get_page());
        let ptr0 = self.jit.get_current_address();
        self.jit.select_page(1);
        let ptr1 = self.jit.get_current_address();
        self.jit.select_page(0);
        (ptr0, ptr1)
    }
}

///
/// Get *ClassId* of the *Value*.
///
/// #### in
/// - rdi: Value
///
/// #### out
/// - rax: ClassId
///
fn get_class(jit: &mut JitMemory) -> DestLabel {
    let label = jit.label();
    let l1 = jit.label();
    let exit = jit.label();
    let err = jit.label();
    monoasm!(jit,
    label:
        movl  rax, (INTEGER_CLASS.u32());
        testq rdi, 0b001;
        jnz   exit;
        movl  rax, (FLOAT_CLASS.u32());
        testq rdi, 0b010;
        jnz   exit;
        testq rdi, 0b111;
        jnz   l1;
        testq rdi, rdi;
        jz    err;
        movl  rax, [rdi + 4];
        jmp   exit;
    l1:
        movl  rax, (SYMBOL_CLASS.u32());
        cmpb  rdi, (TAG_SYMBOL);
        je    exit;
        movl  rax, (NIL_CLASS.u32());
        cmpq  rdi, (NIL_VALUE);
        je    exit;
        movl  rax, (TRUE_CLASS.u32());
        cmpq  rdi, (TRUE_VALUE);
        je    exit;
        movl  rax, (FALSE_CLASS.u32());
        cmpq  rdi, (FALSE_VALUE);
        je    exit;
    err:
        movq  rax, (runtime::illegal_classid);  // rdi: Value
        call  rax;
        // no return
    exit:
        ret;
    );
    label
}

fn entry_panic(jit: &mut JitMemory) -> DestLabel {
    let label = jit.label();
    monoasm! {jit,
    label:
        movq rdi, rbx;
        movq rsi, r12;
        movq rax, (runtime::_dump_stacktrace);
        call rax;
        movq rdi, rbx;
        movq rsi, r12;
        movq rax, (runtime::panic);
        jmp rax;
        leave;
        ret;
    }
    label
}

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
fn f64_to_val(jit: &mut JitMemory) -> DestLabel {
    let label = jit.label();
    let normal = jit.label();
    let heap_alloc = jit.label();
    monoasm! {jit,
    label:
        xorps xmm1, xmm1;
        ucomisd xmm0, xmm1;
        jne normal;
        jp normal;
        movq rax, (FLOAT_ZERO);
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
    // we must save rdi for log_deoptimize.
        subq rsp, 152;
        movq [rsp + 144], r9;
        movq [rsp + 136], r8;
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
        movq rax, (Value::float_heap);
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
        movq rsi, [rsp + 120];
        movq rdx, [rsp + 128];
        movq r8, [rsp + 136];
        movq r9, [rsp + 144];
        addq rsp, 152;
        ret;
    }
    label
}

fn unimplemented_inst(jit: &mut JitMemory) -> CodePtr {
    let lebel = jit.get_current_address();
    monoasm! { jit,
            movq rdi, rbx;
            movq rsi, r12;
            movzxw rdx, [r13 - 10];
            movq rax, (runtime::unimplemented_inst);
            call rax;
            leave;
            ret;
    }
    lebel
}

#[cfg(feature = "profile")]
extern "C" fn guard_fail(vm: &mut Executor, globals: &mut Globals, self_val: Value) {
    let func_id = vm.cfp().lfp().meta().func_id();
    globals.jit_class_guard_failed(func_id, self_val.class());
}

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

impl Globals {
    pub(super) fn exec_jit_compile(
        &mut self,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
        entry_label: DestLabel,
    ) {
        #[cfg(any(feature = "emit-asm", feature = "jit-log", feature = "jit-debug"))]
        {
            let func = self[func_id].as_ruby_func();
            let start_pos = func.get_pc_index(position);
            let name = self.func_description(func_id);
            eprintln!(
                "==> start {} compile: {:?} <{}> {}self_class: {} {}:{}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                func.id(),
                name,
                if position.is_some() {
                    format!("start:[{}] ", start_pos)
                } else {
                    String::new()
                },
                self.get_class_name(self_value.class()),
                func.sourceinfo.file_name(),
                func.sourceinfo.get_line(&func.loc),
            );
        }

        #[cfg(feature = "perf")]
        let pair = self.codegen.get_address_pair();

        let _sourcemap =
            self.codegen
                .compile(&self.store, func_id, self_value, position, entry_label);
        #[cfg(feature = "perf")]
        {
            let desc = format!("JIT:<{}>", self.func_description(func_id));
            self.codegen.perf_info(pair, &desc);
        }
        #[cfg(feature = "emit-asm")]
        self.dump_disas(_sourcemap, func_id);
    }

    ///
    /// Compile the Ruby method.
    ///
    pub(super) fn exec_jit_compile_method(
        &mut self,
        func_id: FuncId,
        self_value: Value,
        jit_entry: DestLabel,
    ) {
        self.exec_jit_compile(func_id, self_value, None, jit_entry)
    }
}
