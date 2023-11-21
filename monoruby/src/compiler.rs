use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

pub mod jitgen;
pub mod runtime;
mod vmgen;
mod wrapper;

use super::*;
use crate::bytecodegen::inst::*;
use crate::executor::*;

type EntryPoint = extern "C" fn(&mut Executor, &mut Globals, FuncId) -> Option<Value>;

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

type FiberInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcInner,
    Value,
    *const Value,
    usize,
    &mut Executor,
) -> Option<Value>;

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            fn [<icmp_ $op>](&mut self) {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                    cmpq rdi, rsi;
                    [<set $op>] rax;
                    shlq rax, 3;
                    orq rax, (FALSE_VALUE);
                };
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
            fn [<condbr_int_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { &mut self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { &mut self.jit,
                        [<j $rev_sop>] branch_dest;
                    },
                }
            }

            fn [<condbr_float_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { &mut self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { &mut self.jit,
                        [<j $rev_op>] branch_dest;
                    },
                }
            }
        }
    };
    (($op1:ident, $rev_op1:ident, $sop1:ident, $rev_sop1:ident), $(($op2:ident, $rev_op2:ident, $sop2:ident, $rev_sop2:ident)),+) => {
        cmp_opt_main!(($op1, $rev_op1, $sop1, $rev_sop1));
        cmp_opt_main!($(($op2, $rev_op2, $sop2, $rev_sop2)),+);
    };
}

///
/// Bytecode compiler
///
/// This generates x86-64 machine code from a bytecode.
///
pub struct Codegen {
    pub jit: JitMemory,
    pub main_object: Value,
    pub(crate) class_version: DestLabel,
    pub(crate) class_version_addr: *mut u32,
    alloc_flag: DestLabel,
    const_version: DestLabel,
    #[allow(dead_code)]
    entry_panic: DestLabel,
    pub(super) vm_entry: DestLabel,
    vm_fetch: DestLabel,
    ///
    /// Raise error.
    ///
    /// ### in
    /// - r13: PC + 1
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
    ///
    /// Copy f64 of flonum to *xmm*.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - xmm0
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    flonum_to_f64: DestLabel,
    div_by_zero: DestLabel,
    ///
    /// Raise "wrong number of arguments" error.
    ///
    /// ### in
    /// - rdx: actual number of arguments
    /// - r13: pc (InitMethod)
    ///
    #[allow(dead_code)]
    wrong_argument: DestLabel,
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
    dispatch: Vec<CodePtr>,
    pub(super) entry_point: EntryPoint,
    pub(crate) method_invoker: MethodInvoker,
    pub(crate) method_invoker2: MethodInvoker2,
    pub(crate) block_invoker: BlockInvoker,
    pub(crate) block_invoker_with_self: BlockInvoker,
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
    #[cfg(feature = "perf")]
    pub(crate) perf_file: std::fs::File,
}

impl Codegen {
    cmp_main!(eq, ne, lt, le, gt, ge);
    cmp_opt_main!(
        (eq, ne, eq, ne),
        (ne, eq, ne, eq),
        (a, be, gt, le),
        (b, ae, lt, ge),
        (ae, b, ge, lt),
        (be, a, le, gt)
    );

    fn icmp_teq(&mut self) {
        self.icmp_eq()
    }

    fn icmp_cmp(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            xorq rax, rax;
            movq rdx, (Value::from_ord(std::cmp::Ordering::Greater).id());
            cmpq rdi, rsi;
            jeq  exit;
            cmovgeq rax, rdx;
            movq rdx, (Value::from_ord(std::cmp::Ordering::Less).id());
            cmovltq rax, rdx;
        };
    }

    pub fn new(no_jit: bool, main_object: Value) -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.const_i32(1);
        let const_version = jit.const_i64(1);
        let alloc_flag = jit.const_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });
        let entry_panic = jit.label();
        jit.bind_label(entry_panic);
        Self::entry_panic(&mut jit);
        let get_class = jit.label();
        jit.bind_label(get_class);
        Self::get_class(&mut jit);
        let wrong_argument = jit.label();
        jit.bind_label(wrong_argument);
        Self::wrong_arguments(&mut jit);
        let f64_to_val = jit.label();
        jit.bind_label(f64_to_val);
        Self::f64_to_val(&mut jit);
        let flonum_to_f64 = jit.label();
        jit.bind_label(flonum_to_f64);
        Self::flonum_to_f64(&mut jit);

        // dispatch table.
        let entry_unimpl = jit.get_current_address();
        monoasm! { &mut jit,
                movq rdi, rbx;
                movq rsi, r12;
                movzxw rdx, [r13 - 10];
                movq rax, (runtime::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        //jit.select_page(0);
        let dispatch = vec![entry_unimpl; 256];
        let mut codegen = Self {
            jit,
            main_object,
            class_version,
            class_version_addr: std::ptr::null_mut(),
            alloc_flag,
            const_version,
            entry_panic,
            vm_entry: entry_panic,
            vm_fetch: entry_panic,
            entry_raise: entry_panic,
            f64_to_val,
            flonum_to_f64,
            div_by_zero: entry_panic,
            wrong_argument,
            get_class,
            dispatch,
            entry_point: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            method_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            method_invoker2: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            block_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            block_invoker_with_self: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            fiber_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            fiber_invoker_with_self: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            resume_fiber: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            yield_fiber: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
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
        codegen.gen_entry_point(main_object);
        codegen.jit.finalize();
        codegen.class_version_addr =
            codegen.jit.get_label_address(class_version).as_ptr() as *mut u32;
        let address = codegen.jit.get_label_address(alloc_flag).as_ptr() as *mut u32;
        alloc::ALLOC.with(|alloc| {
            alloc.borrow_mut().set_alloc_flag_address(address);
        });
        codegen
    }

    fn gen_entry_point(&mut self, main_object: Value) {
        // "C" fn(&mut Executor, &mut Globals, FuncId) -> Option<Value>
        let entry = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            pushq rbp;
            subq rsp, 8;
        }
        self.set_lfp();
        monoasm! { &mut self.jit,
            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
        }
        self.get_func_data();
        monoasm! { &mut self.jit,
            // set meta func_id
            movq rax, [rdx + (FUNCDATA_META)];  // r13: *const FuncData
            movq [r14 - (LBP_META)], rax;
            // set block
            movq [r14 - (LBP_BLOCK)], 0;
            movq [r14 - (LBP_OUTER)], 0;
            movq [r14 - (BP_PREV_CFP)], 0;
            lea  rax, [r14 - (BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rax;
        };
        let l1 = self.jit.label();
        let l2 = self.jit.label();
        monoasm! { &mut self.jit,
            lea  rax, [r14 - (LBP_ARG0)];
            movzxw rdi, [rdx + (FUNCDATA_REGNUM)];
        l1:
            subq rdi, 1;
            je   l2;
            movq [rax], (NIL_VALUE);
            subq rax, 8;
            jmp  l1;
        l2:
        };
        monoasm! { &mut self.jit,
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
            // -0x20 |    block    |
            //       +-------------+
            // -0x28 |     %0      |
            //       +-------------+
            // -0x30 | %1(1st arg) |
            //       +-------------+
            //       |             |
            //
            // set self
            movq rax, (main_object.id());
            movq [r14 - (LBP_SELF)], rax;
            movq rax, [rdx + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [rdx + (FUNCDATA_PC)];
            // set arg len
            xorq rdx, rdx;
            call rax;
            // pop frame
            movq [rbx + (EXECUTOR_CFP)], 0;
            addq rsp, 8;
            popq rbp;
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };

        self.entry_point = unsafe { std::mem::transmute(entry.as_ptr()) };

        #[cfg(feature = "perf")]
        self.perf_info(entry, "entry-point");
    }

    ///
    /// Execute garbage collection.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn execute_gc(&mut self) {
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
        monoasm! { &mut self.jit,
        gc:
            movq rdi, r12;
            movq rsi, rbx;
            movq rax, (execute_gc);
            call rax;
            jmp exit;
        };
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
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rsi], rdi;
            movq [rbx + (EXECUTOR_CFP)], rsi;
        );
    }

    /// Pop control frame
    fn pop_frame(&mut self) {
        monoasm!( &mut self.jit,
            // pop cfp
            lea  r14, [rbp - (BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], r14;
            // restore lfp
            movq r14, [rbp - (BP_LFP)];
        );
    }

    ///
    /// Get FuncData.
    ///
    /// ### in
    /// - r12: &Globals
    /// - rdx: FuncId
    ///
    /// ### out
    /// - rdx: &FuncData
    ///
    fn get_func_data(&mut self) {
        monoasm! { &mut self.jit,
            movl rdx, rdx;
            shlq rdx, 6;
            addq rdx, [r12 + (GLOBALS_FUNCINFO)];
            lea  rdx, [rdx + (FUNCINFO_DATA)];
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
            movq  rsi, [rax - (LBP_SELF)];
            movq [rsp - (16 + LBP_SELF)], rsi;
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
            lea  rsi, [rax - (LBP_OUTER)];
            movq [rsp - (16 + LBP_OUTER)], rsi;
        };
    }

    /// Set outer.
    fn set_method_outer(&mut self) {
        monoasm! { &mut self.jit,
            movq [rsp - (16 + LBP_OUTER)], 0;
        };
    }

    /// Set lfp(r14) for callee.
    fn set_lfp(&mut self) {
        monoasm!( &mut self.jit,
            // set lfp
            lea  r14, [rsp - 16];
            movq [r14 - (BP_LFP)], r14;
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

    /// ## in
    ///
    /// ## out
    /// - rax : result
    fn call_codeptr(&mut self, codeptr: CodePtr) {
        self.push_frame();
        self.set_lfp();
        let src_point = self.jit.get_current_address();
        monoasm!( &mut self.jit,
            call (codeptr - src_point - 5);
        );
        self.pop_frame();
    }

    /// ## in
    ///
    /// ## out
    /// - rax : result
    fn call_label(&mut self, label: DestLabel) {
        self.push_frame();
        self.set_lfp();
        monoasm!( &mut self.jit,
            call label;
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

    fn call_unop(&mut self, func: usize) {
        monoasm!( &mut self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
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

    pub(super) fn class_guard_stub(
        &mut self,
        self_class: ClassId,
        patch_point: DestLabel,
        entry: DestLabel,
        guard: DestLabel,
    ) {
        let old = self.jit.get_page();
        self.jit.select_page(1);

        let vm_entry = self.vm_entry;
        monoasm!( &mut self.jit,
        guard:
            movq rdi, [r14 - (LBP_SELF)];
        );
        self.guard_class(self_class, vm_entry);
        monoasm! { &mut self.jit,
        patch_point:
            jmp entry;
        }
        self.jit.select_page(old);
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
            lea  rbp, [rdi + (BP_PREV_CFP)];
        };
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
        };
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
        };
    }

    ///
    /// Handle req/opt/rest arguments
    ///
    /// #### in
    /// - rdi: arg len
    /// - rdx: CallSiteId
    ///
    /// #### out
    /// - rdi: arg len
    /// - rax: Option<Value>
    ///  
    fn handle_arguments(&mut self) {
        monoasm! { &mut self.jit,
            lea  r8, [rsp - 16];   // callee_lfp
            movq rcx, rdi;
            subq rsp, 4088;
            pushq rdi;
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::vm_handle_arguments);
            call rax;
            popq rdi;
            addq rsp, 4088;
        }
    }

    ///
    /// block args expansion
    ///
    /// #### in
    /// - rdi: arg_num
    /// - rsi: pc
    ///
    /// #### out
    /// - rdi: arg_num
    ///
    /// #### destroy
    /// - caller save registers (except rdx)
    ///
    fn block_arg_expand(&mut self) {
        let l1 = self.jit.label();
        monoasm! { &mut self.jit,
            testq rsi, rsi;
            je   l1;
            // rax <- op
            movzxb rax, [rsi + 6];
            // block-style?
            cmpb rax, (172u8 as i8);
            jne  l1;
            // reqopt > 1?
            cmpw [rsi + 2], 1;
            jle  l1;
        }
        self.single_arg_expand();
        monoasm! { &mut self.jit,
        l1:
        };
    }

    ///
    /// Expand single Array argument.
    ///
    /// #### in/out
    /// - rdi: arg_num
    ///
    /// #### destroy
    /// - caller save registers
    ///
    fn single_arg_expand(&mut self) {
        let l1 = self.jit.label();
        monoasm! { &mut self.jit,
            // arg_num == 1?
            cmpl rdi, 1;
            jne  l1;
            // is val Array?
            movq rax, [rsp - (16 + LBP_ARG0)];
            testq rax, 0b111;
            jnz  l1;
            cmpl [rax + 4], (ARRAY_CLASS.0);
            jne  l1;
            movq rdi, rax;
            movzxw rdx, [rsi + 8];  // rdx <- req
            lea  rsi, [rsp - (16 + LBP_ARG0)]; // rsi <- dst
            subq rsp, 4096;
            movq rax, (block_expand_array); // extern "C" fn block_expand_array(src: Value, dst: *mut Value, min_len: usize) -> usize
            call rax;
            movq rdi, rax;
            addq rsp, 4096;
        l1:
        };
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
    fn get_class(jit: &mut JitMemory) {
        let l1 = jit.label();
        let exit = jit.label();
        let err = jit.label();
        monoasm!(jit,
                movl  rax, (INTEGER_CLASS.0);
                testq rdi, 0b001;
                jnz   exit;
                movl  rax, (FLOAT_CLASS.0);
                testq rdi, 0b010;
                jnz   exit;
                testq rdi, 0b111;
                jnz   l1;
                testq rdi, rdi;
                jz    err;
                movl  rax, [rdi + 4];
                jmp   exit;
            l1:
                movl  rax, (SYMBOL_CLASS.0);
                cmpb  rdi, (TAG_SYMBOL);
                je    exit;
                movl  rax, (NIL_CLASS.0);
                cmpq  rdi, (NIL_VALUE);
                je    exit;
                movl  rax, (TRUE_CLASS.0);
                cmpq  rdi, (TRUE_VALUE);
                je    exit;
                movl  rax, (FALSE_CLASS.0);
                cmpq  rdi, (FALSE_VALUE);
                je    exit;
            err:
                movq  rax, (runtime::illegal_classid);  // rdi: Value
                call  rax;
                // no return
            exit:
                ret;
        );
    }

    fn wrong_arguments(jit: &mut JitMemory) {
        monoasm! {jit,
            movq rdi, rbx;
            movl rsi, rdx;  // given
            movzxw rdx, [r13 - 8];  // min
            movzxw rcx, [r13 - 14];  // max
            movq rax, (runtime::err_wrong_number_of_arguments_range);
            call rax;
        }
    }

    fn entry_panic(jit: &mut JitMemory) {
        monoasm! {jit,
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
    }

    ///
    /// Copy f64 of flonum to *xmm*.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - xmm0
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    pub(super) fn flonum_to_f64(jit: &mut JitMemory) {
        let exit = jit.label();
        monoasm! {jit,
            xorps xmm0, xmm0;
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            // in the case of 0.0
            je exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm0, rdi;
        exit:
            ret;
        }
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
    pub(super) fn f64_to_val(jit: &mut JitMemory) {
        let normal = jit.label();
        let heap_alloc = jit.label();
        monoasm!(jit,
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
        );
    }
}

#[test]
fn guard_class() {
    let mut gen = Codegen::new(false, Value::object(OBJECT_CLASS));

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
        (SYMBOL_CLASS, Value::symbol(IdentId::get_id("Ruby"))),
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
    let mut gen = Codegen::new(false, Value::object(OBJECT_CLASS));

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
        #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "jit-debug"))]
        {
            let func = self[func_id].as_ruby_func();
            let start_pos = func.get_pc_index(position);
            let name = self.store.func_description(func_id);
            eprintln!(
                "==> start {} compile: {} {:?} self_class:{} start:[{start_pos}] {}:{}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                name,
                func.id(),
                self_value.class().get_name(self),
                func.sourceinfo.file_name(),
                func.sourceinfo.get_line(&func.loc),
            );
        }
        //#[cfg(feature = "emit-asm")]
        //self[func_id].dump_bc(self);

        #[cfg(feature = "perf")]
        let codeptr = self.codegen.jit.get_current_address();

        let _sourcemap =
            self.codegen
                .compile(&self.store, func_id, self_value, position, entry_label);
        #[cfg(feature = "perf")]
        {
            let class_name = self_value.class().get_name(self);
            let desc = format!("{}#{}", class_name, self.store.func_description(func_id));
            self.codegen.perf_info(codeptr, &desc);
        }
        #[cfg(any(feature = "emit-asm"))]
        self.dump_disas(_sourcemap, func_id);
    }

    ///
    /// Compile the Ruby method.
    ///
    pub(super) fn exec_jit_compile_method(
        &mut self,
        func_id: FuncId,
        self_value: Value,
        entry_label: DestLabel,
    ) {
        self.exec_jit_compile(func_id, self_value, None, entry_label)
    }
}
