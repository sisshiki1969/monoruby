use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

mod jitgen;
pub mod runtime;
mod vmgen;
mod wrapper;

use super::*;
//use runtime::*;

type EntryPoint = extern "C" fn(&mut Executor, &mut Globals, *const FuncData) -> Option<Value>;

type MethodInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    *const FuncData,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

type BlockInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    *const BlockData,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

type MethodInvoker2 =
    extern "C" fn(&mut Executor, &mut Globals, *const FuncData, Value, Arg, usize) -> Option<Value>;

macro_rules! cmp_main {
    ($op:ident) => {
        paste! {
            fn [<integer_cmp_ $op>](&mut self) {
                monoasm! { self.jit,
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
            fn [<cmp_opt_int_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $sop>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
                        [<j $rev_sop>] branch_dest;
                    },
                }
            }

            fn [<cmp_opt_float_ $sop>](&mut self, branch_dest: DestLabel, brkind: BrKind) {
                match brkind {
                    BrKind::BrIf => monoasm! { self.jit,
                        [<j $op>] branch_dest;
                    },
                    BrKind::BrIfNot => monoasm! { self.jit,
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
    class_version: DestLabel,
    pub(super) class_version_addr: *mut u32,
    alloc_flag: DestLabel,
    const_version: DestLabel,
    entry_panic: DestLabel,
    vm_entry: DestLabel,
    vm_fetch: DestLabel,
    pub(super) entry_point: EntryPoint,
    vm_return: DestLabel,
    f64_to_val: DestLabel,
    heap_to_f64: DestLabel,
    div_by_zero: DestLabel,
    no_block: DestLabel,
    ///
    /// Raise "wrong number of arguments" error.
    ///
    /// ### in
    /// - rdx: actual number of arguments
    /// - r13: pc (InitBlock/InitMethod)
    ///
    #[allow(dead_code)]
    wrong_argument: DestLabel,
    dispatch: Vec<CodePtr>,
    pub(super) method_invoker: MethodInvoker,
    pub(super) method_invoker2: MethodInvoker2,
    pub(super) block_invoker: BlockInvoker,
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

    pub(super) fn new(no_jit: bool, main_object: Value) -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.const_i32(1);
        let const_version = jit.const_i64(1);
        let alloc_flag = jit.const_i32(if cfg!(feature = "gc-stress") { 1 } else { 0 });
        let entry_panic = jit.label();
        let jit_return = jit.label();
        let vm_return = jit.label();
        let div_by_zero = jit.label();
        let no_block = jit.label();
        let wrong_argument = jit.label();
        let heap_to_f64 = jit.label();
        let splat = jit.label();
        monoasm!(&mut jit,
        entry_panic:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::_dump_stacktrace);
            call rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::panic);
            jmp rax;
        vm_return:
            movq r15, rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LBP_META)];
            movq rcx, r13;
            subq rcx, 8;
            movq rax, (runtime::get_error_location);
            call rax;
            // restore return value
            movq rax, r15;
        jit_return:
            leave;
            ret;
        div_by_zero:
            movq rdi, r12;
            movq rax, (runtime::err_divide_by_zero);
            call rax;
            xorq rax, rax;
            leave;
            ret;
        no_block:
            movq rdi, r12;
            movq rax, (runtime::err_no_block_given);
            call rax;
            xorq rax, rax;
            leave;
            ret;
        wrong_argument:
            movq rdi, r12;
            movl rsi, rdx;  // given
            movzxw rdx, [r13 - 8];  // min
            movzxw rcx, [r13 - 14];  // max
            movq rax, (runtime::err_wrong_number_of_arguments_range);
            call rax;
            jmp  vm_return;
        heap_to_f64:
            // we must save rdi for log_optimize.
            subq rsp, 128;
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
            movq rax, (Value::val_tof);
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
            addq rsp, 128;
            ret;
        splat:
            subq rsp, 1024;
            pushq rdi;
            pushq rsi;
            movq rdi, rax;
            movq rsi, r8;
            movq rax, (expand_splat);
            call rax;
            popq rsi;
            popq rdi;
            addq rsp, 1024;
            lea  rdi, [rdi + rax * 1 - 1];
            shlq rax, 3;
            subq r8, rax;
            ret;
        );

        // dispatch table.
        let entry_unimpl = jit.get_current_address();
        monoasm! { jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, [r13 - 16];
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
            entry_point: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            vm_return,
            f64_to_val: entry_panic,
            heap_to_f64,
            div_by_zero,
            no_block,
            wrong_argument,
            dispatch,
            method_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            method_invoker2: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            block_invoker: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
        };
        codegen.f64_to_val = codegen.generate_f64_to_val();
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
        // "C" fn(&mut Executor, &mut Globals, *const FuncData) -> Option<Value>
        let entry = self.jit.get_current_address();
        monoasm! { self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            pushq rbp;
            subq rsp, 8;

            movq rbx, rdi;  // rdi: &mut Interp
            movq r12, rsi;  // rsi: &mut Globals
            movq r13, rdx;
            // set meta func_id
            movq rax, [r13 + (FUNCDATA_OFFSET_META)];  // r13: *const FuncData
            movq [rsp - (16 + LBP_META)], rax;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_OUTER)], 0;
            movq [rsp - (16 + BP_PREV_CFP)], 0;
            lea  rax, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx], rax;
        };
        let l1 = self.jit.label();
        let l2 = self.jit.label();
        monoasm! { self.jit,
            lea  rax, [rsp - (16 + LBP_ARG0)];
            movzxw rdi, [r13 + (FUNCDATA_OFFSET_REGNUM)];
        l1:
            subq rdi, 1;
            je   l2;
            movq [rax], (NIL_VALUE);
            subq rax, 8;
            jmp  l1;
        l2:
        };
        self.set_lfp();
        monoasm! {self.jit,
            movq [rbx + 8], r14;
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
            movq rax, (main_object.get());
            movq [rsp - (16 + LBP_SELF)], rax;
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];
            // set arg len
            xorq rdx, rdx;
            call rax;
            // pop frame
            movq [rbx], 0;
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
    }

    ///
    /// Execute garbage collection.
    ///
    /// ### destroy
    /// - caller save registers
    fn execute_gc(&mut self) {
        let alloc_flag = self.alloc_flag;
        let gc = self.jit.label();
        let exit = self.jit.label();
        assert_eq!(0, self.jit.get_page());
        monoasm! { self.jit,
            cmpl [rip + alloc_flag], 0;
            jne  gc;
        exit:
        };
        self.jit.select_page(1);
        monoasm! { self.jit,
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
    /// - rsi
    fn push_frame(&mut self) {
        monoasm!(self.jit,
            // push cfp
            movq rsi, [rbx];
            movq [rsp - (16 + BP_PREV_CFP)], rsi;
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx], rsi;
        );
    }

    /// Set outer.
    ///
    /// ### in
    /// - rax: outer_cfp
    ///
    /// ### destroy
    /// - rsi
    ///
    fn set_block_self_outer(&mut self) {
        monoasm! { self.jit,
            // set outer
            lea  rsi, [rax - (LBP_OUTER)];
            movq [rsp - (16 + LBP_OUTER)], rsi;
            // set self
            movq  rsi, [rax - (LBP_SELF)];
            movq [rsp - (16 + LBP_SELF)], rsi;
        };
    }

    /// Set outer.
    fn set_method_outer(&mut self) {
        monoasm! { self.jit,
            movq [rsp - (16 + LBP_OUTER)], 0;
        };
    }

    /// Pop control frame
    fn pop_frame(&mut self) {
        monoasm!(self.jit,
            // pop cfp
            lea  r14, [rbp - (BP_PREV_CFP)];
            movq [rbx], r14;
            // restore lfp
            movq r14, [rbp - (BP_LFP)];
        );
    }

    /// Set lfp(r14) for callee.
    fn set_lfp(&mut self) {
        monoasm!(self.jit,
            // set lfp
            lea  r14, [rsp - 16];
            movq [rsp - (16 + BP_LFP)], r14;
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
        monoasm!(self.jit,
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
        monoasm!(self.jit,
            call (codeptr - src_point - 5);
        );
        self.pop_frame();
    }

    ///
    /// calculate an offset of stack pointer.
    ///
    fn calc_offset(&mut self) {
        monoasm!(self.jit,
            addq rax, (LBP_ARG0 / 8 + 1);
            andq rax, (-2);
            shlq rax, 3;
        );
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
        monoasm!(self.jit,
            testq rdi, 0x1;
            jz generic;
        );
    }

    ///
    /// check whether rhs is fixnum.
    ///
    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            testq rsi, 0x1;
            jz generic;
        );
    }

    fn call_unop(&mut self, func: usize) {
        monoasm!(self.jit,
            movq rdx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
    }

    fn call_binop(&mut self, func: usize) {
        monoasm!(self.jit,
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (func);
            call rax;
        );
    }

    /// Check whether *rdi*(Value) is true or not, and store boolean result (Value) to *rax*.
    ///
    /// #### destoroy
    /// - rdi
    ///
    fn not_rdi_to_rax(&mut self) {
        monoasm! { self.jit,
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
    /// - rcx: &FuncData
    /// - rsi: CallSiteId
    ///
    /// #### out
    /// - rax: Option<Value>
    ///  
    fn handle_arguments(&mut self) {
        monoasm! {self.jit,
            lea  r8, [rsp - (16 + LBP_SELF)];
            movq r9, rdi;
            subq rsp, 4088;
            pushq rdi;
            movq rdi, r12; // &mut Globals
            lea  rdx, [r14 - (LBP_SELF)];
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
        monoasm! { self.jit,
            testq rsi, rsi;
            je   l1;
            // rax <- op
            movzxb rax, [rsi + 6];
            // method-style
            //cmpb rax, (170u8 as i8);
            //je   l1;
            // block-style?
            cmpb rax, (172u8 as i8);
            jne  l1;
            // reqopt > 1?
            cmpw [rsi + 2], 1;
            jle  l1;
        }
        self.single_arg_expand();
        monoasm! { self.jit,
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
        monoasm! { self.jit,
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
            subq rsp, 1024;
            movq rax, (block_expand_array); // extern "C" fn block_expand_array(src: Value, dst: *mut Value, min_len: usize) -> usize
            call rax;
            movq rdi, rax;
            addq rsp, 1024;
        l1:
        };
    }
}

impl Globals {
    pub(super) fn jit_compile_ruby(
        &mut self,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
    ) -> DestLabel {
        #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
        {
            let func = self[func_id].as_ruby_func();
            let start_pos = func.get_pc_index(position);
            eprintln!(
                "==> start {} compile: {} {:?} self_class:{} start:[{:05}] bytecode:{:?}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                func.name(),
                func.id(),
                self_value.class().get_name(self),
                start_pos,
                func.bytecode().as_ptr(),
            );
        }
        let (label, _sourcemap) = self
            .codegen
            .compile(&self.func, func_id, self_value, position);

        #[cfg(any(feature = "emit-asm"))]
        self.dump_disas(_sourcemap, func_id);
        label
    }
}
