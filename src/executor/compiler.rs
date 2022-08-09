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
    pub class_version_addr: *mut u32,
    pub const_version: DestLabel,
    pub entry_panic: DestLabel,
    pub vm_entry: DestLabel,
    pub vm_fetch: DestLabel,
    pub entry_point: EntryPoint,
    pub entry_point_return: CodePtr,
    entry_find_method: DestLabel,
    pub vm_return: DestLabel,
    pub f64_to_val: DestLabel,
    pub div_by_zero: DestLabel,
    pub dispatch: Vec<CodePtr>,
    pub invoker: Invoker,
    opt_buf: Option<BcPc>,
}

fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + 16
}

//
// Runtime functions.
//

///
/// Get an absolute address of the given method.
///
/// If no method was found, return None (==0u64).
///
extern "C" fn find_method<'a>(
    interp: &mut Interp,
    globals: &'a mut Globals,
    func_name: IdentId,
    args_len: usize,
    receiver: Value,
) -> Option<&'a FuncData> {
    let func_id = globals.get_method(receiver.class_id(), func_name, args_len)?;
    let data = interp.get_func_data(globals, func_id);
    Some(data)
}

extern "C" fn define_method(
    _interp: &mut Interp,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) {
    globals.class.add_method(OBJECT_CLASS, name, func);
}

extern "C" fn gen_array(src: *const Value, len: usize) -> Value {
    let mut v = if len == 0 {
        vec![]
    } else {
        unsafe { std::slice::from_raw_parts(src.sub(len - 1), len).to_vec() }
    };
    v.reverse();
    Value::new_array(v)
}

extern "C" fn get_index(
    interp: &mut Interp,
    globals: &mut Globals,
    base: Value,
    index: Value,
) -> Option<Value> {
    match base.unpack() {
        RV::Object(rv) => match &rv.kind {
            ObjKind::Array(v) => {
                if let Some(idx) = index.try_fixnum() {
                    return Some(if idx >= 0 {
                        v.get(idx as usize).cloned().unwrap_or_default()
                    } else {
                        let idx = v.len() as i64 + idx;
                        if idx < 0 {
                            Value::nil()
                        } else {
                            v.get(idx as usize).cloned().unwrap_or_default()
                        }
                    });
                }
            }
            _ => {}
        },
        _ => {}
    }
    let method = globals.get_ident_id("[]");
    interp.invoke_method(globals, method, base, &[index])
}

extern "C" fn set_index(
    interp: &mut Interp,
    globals: &mut Globals,
    base: Value,
    index: Value,
    src: Value,
) -> Option<Value> {
    match base.class_id() {
        ARRAY_CLASS => {
            let v = base.as_array_mut();
            if let Some(idx) = index.try_fixnum() {
                if idx >= 0 {
                    match v.get_mut(idx as usize) {
                        Some(v) => *v = src,
                        None => {
                            let idx = idx as usize;
                            v.extend((v.len()..idx).into_iter().map(|_| Value::nil()));
                            v.push(src);
                        }
                    }
                } else {
                    let idx_positive = v.len() as i64 + idx;
                    if idx_positive < 0 {
                        globals.err_index_too_small(idx, -(v.len() as i64));
                        return None;
                    } else {
                        v[idx_positive as usize] = src;
                    }
                };
                return Some(src);
            }
        }
        _ => {}
    }
    let method = globals.get_ident_id("[]=");
    interp.invoke_method(globals, method, base, &[index, src])
}

extern "C" fn unimplemented_inst(_: &mut Interp, _: &mut Globals, opcode: u64) {
    panic!("unimplemented inst. {:016x}", opcode);
}

extern "C" fn panic(_: &mut Interp, _: &mut Globals) {
    panic!("panic in jit code.");
}

/*pub extern "C" fn eprintln(rdi: u64, rsi: u64) {
    eprintln!("rdi:{:016x} rsi:{:016x}", rdi, rsi);
}*/

extern "C" fn error_divide_by_zero(globals: &mut Globals) {
    globals.err_divide_by_zero();
}

extern "C" fn get_error_location(
    _interp: &mut Interp,
    globals: &mut Globals,
    meta: Meta,
    pc: BcPc,
) {
    if meta.kind() != 0 {
        // currently, JIT is not yet supported.
        return;
    }
    let func_info = &globals.func[meta.func_id()];
    let bc_base = func_info.data.pc;
    let normal_info = match &func_info.kind {
        FuncKind::Normal(info) => info,
        FuncKind::Builtin { .. } => return,
    };
    let sourceinfo = normal_info.sourceinfo.clone();
    let loc = normal_info.sourcemap[pc - bc_base];
    globals.push_error_location(loc, sourceinfo);
}

impl Codegen {
    pub fn new(no_jit: bool) -> Self {
        let mut jit = JitMemory::new();
        jit.add_page();

        let class_version = jit.const_i32(0);
        let const_version = jit.const_i64(0);
        let entry_panic = jit.label();
        let entry_find_method = jit.label();
        let jit_return = jit.label();
        let vm_return = jit.label();
        let div_by_zero = jit.label();
        jit.select(1);
        monoasm!(&mut jit,
        entry_panic:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, rbp;
            movq rax, (op::_dump_stacktrace);
            pushq rax;
            call rax;
            popq rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (panic);
            jmp rax;
        entry_find_method:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (find_method);
            jmp  rax;
        vm_return:
            // check call_kind.
            //movl r15, [rbp - 8];
            //testq r15, r15;
            //jne  jit_return;
            // save return value
            movq r15, rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - 8];
            movq rcx, r13;
            subq rcx, 8;
            movq rax, (get_error_location);
            call rax;
            // restore return value
            movq rax, r15;
        jit_return:
            leave;
            ret;
        div_by_zero:
            movq rdi, r12;
            movq rax, (error_divide_by_zero);
            call rax;
            xorq rax, rax;
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
                movq rdx, [r13 - 16];
                movq rax, (super::compiler::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        jit.select(0);
        let dispatch = vec![entry_unimpl; 256];
        let mut codegen = Self {
            jit,
            class_version,
            class_version_addr: std::ptr::null_mut(),
            const_version,
            entry_panic,
            entry_find_method,
            vm_entry: entry_panic,
            vm_fetch: entry_panic,
            entry_point: unsafe { std::mem::transmute(entry_unimpl.as_ptr()) },
            entry_point_return: entry_unimpl,
            vm_return,
            f64_to_val: entry_panic,
            div_by_zero,
            dispatch,
            invoker,
            opt_buf: None,
        };
        codegen.f64_to_val = codegen.generate_f64_to_val();
        codegen.construct_vm(no_jit);
        codegen.get_entry_point();
        codegen.jit.finalize();
        codegen.class_version_addr =
            codegen.jit.get_label_address(class_version).as_ptr() as *mut u32;
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
            jz generic;
        );
    }

    fn guard_rsi_fixnum(&mut self, generic: DestLabel) {
        monoasm!(self.jit,
            // check whether rhs is fixnum.
            testq rsi, 0x1;
            jz generic;
        );
    }

    fn store_rax(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rax;
        );
    }

    fn store_rdi(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rdi;
        );
    }

    fn xmm_mov(&mut self, src: u16, dst: u16) {
        if src != dst {
            monoasm!(self.jit,
                movq  xmm(dst as u64 + 2), xmm(src as u64 + 2);
            );
        }
    }

    ///
    /// Assume the Value in Integer, and convert to f64.
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
    fn assume_int_to_f64(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
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
    /// Assume the Value in Float, and convert to f64.
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
    /// - rax
    ///
    fn assume_float_to_f64(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let entry = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
        entry:
            testq rdi, 0b01;
            jnz side_exit;
            testq rdi, 0b10;
            jz side_exit;
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
        exit:
        );
        entry
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
    fn generate_f64_to_val(&mut self) -> DestLabel {
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
        heap_alloc:
            subq rsp, 120;
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
            addq rsp, 120;
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
        );
        entry
    }

    fn call_unop(&mut self, func: u64) {
        let entry_return = self.vm_return;
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

    fn call_binop(&mut self, func: u64) {
        let entry_return = self.vm_return;
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

    pub fn compile_on_demand(&mut self, globals: &mut Globals, func_id: FuncId) {
        if globals.func[func_id].data.codeptr.is_none() {
            let mut info = std::mem::take(&mut globals.func[func_id]);
            self.jit_compile(&mut info, &globals.func);
            globals.func[func_id] = info;
        }
    }

    fn jit_compile(&mut self, func: &mut FuncInfo, store: &FnStore) {
        let label = match &func.kind {
            FuncKind::Normal(info) => {
                func.data.meta.set_jit();
                let jit_entry = self.jit_compile_normal(info, store, None);
                let codeptr = self.jit.get_current_address();
                monoasm!(self.jit,
                    jmp jit_entry;
                );
                codeptr
            }
            FuncKind::Builtin { abs_address } => self.wrap_builtin(*abs_address),
        };
        self.jit.finalize();
        assert_eq!(None, func.data.codeptr);
        func.data.codeptr = Some(label);
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
        // |     2:Native      |
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
            // we should overwrite reg_num because the func itself does not know actual number of arguments.
            movw [rsp - 0x0c], rdi;
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
    extern "C" fn exec_jit_compile(
        interp: &mut Interp,
        globals: &mut Globals,
        func_id: FuncId,
    ) -> CodePtr {
        globals.func[func_id].data.meta.set_jit();
        let label = interp.codegen.jit_compile_normal(
            globals.func[func_id].as_normal(),
            &globals.func,
            None,
        );
        interp.codegen.jit.get_label_address(label)
    }

    extern "C" fn exec_jit_partial_compile(
        interp: &mut Interp,
        globals: &mut Globals,
        func_id: FuncId,
        pc: BcPc,
    ) -> CodePtr {
        let pc_index = pc - globals.func[func_id].data.pc;
        let label = interp.codegen.jit_compile_normal(
            globals.func[func_id].as_normal(),
            &globals.func,
            Some(pc_index),
        );
        interp.codegen.jit.get_label_address(label)
    }

    ///
    /// Set jit compilation stab code for an entry point of each Ruby methods.
    ///
    /// This code will not be executed in "AOT" mode.
    ///
    pub fn set_jit_stab(&mut self, store: &mut FnStore) {
        let vm_entry = self.vm_entry;
        for func in store.funcs_mut().iter_mut() {
            match &func.kind {
                FuncKind::Normal(_) => {
                    let codeptr = self.jit.get_current_address();
                    let counter = self.jit.const_i32(5);
                    let entry = self.jit.label();
                    monoasm!(self.jit,
                    entry:
                        subl [rip + counter], 1;
                        jne vm_entry;
                        movl rax, [rsp - 16];
                        subq rsp, 1024;
                        pushq rdi;
                        movq rdi, rbx;
                        movq rsi, r12;
                        movl rdx, rax;
                        movq rax, (Self::exec_jit_compile);
                        call rax;
                        movw [rip + entry], 0xe9;
                        lea rdi, [rip + entry];
                        addq rdi, 5;
                        subq rax, rdi;
                        movl [rdi - 4], rax;
                        popq rdi;
                        addq rsp, 1024;
                        jmp entry;
                    );
                    assert_eq!(
                        None,
                        std::mem::replace(&mut func.data.codeptr, Some(codeptr))
                    );
                }
                _ => {}
            };
        }
        self.jit.finalize();
    }

    pub fn set_vm_stab(&mut self, store: &mut FnStore) {
        let vm_entry = self.vm_entry;
        for func in store.funcs_mut().iter_mut() {
            match &func.kind {
                FuncKind::Normal(_) => {
                    let codeptr = self.jit.get_current_address();
                    monoasm!(self.jit,
                        jmp vm_entry;
                    );
                    assert_eq!(
                        None,
                        std::mem::replace(&mut func.data.codeptr, Some(codeptr))
                    );
                }
                _ => {}
            };
        }
        self.jit.finalize();
    }
}

#[test]
fn float_test() {
    let mut gen = Codegen::new(false);

    let panic = gen.entry_panic;
    let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
    let assume_float_to_f64 = gen.jit.label();
    monoasm!(&mut gen.jit,
    assume_float_to_f64:
        pushq rbp;
    );
    gen.assume_float_to_f64(0, panic);
    monoasm!(&mut gen.jit,
        popq rbp;
        ret;
    );
    gen.jit.finalize();
    let to_f64_entry = gen.jit.get_label_address(assume_float_to_f64);

    let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };
    let to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(to_f64_entry.as_ptr()) };

    for n in [
        0.0,
        4.2,
        35354354354.2135365,
        -3535354345111.5696876565435432,
        //f64::MAX,
        //f64::MAX / 10.0,
        //f64::MIN * 10.0,
        //f64::NAN,
    ] {
        let v = from_f64(n);
        //assert!(Value::eq(Value::new_float(n), v));
        let (lhs, rhs) = (n, to_f64(v));
        if lhs.is_nan() {
            assert!(rhs.is_nan());
        } else {
            assert_eq!(n, to_f64(v));
        }
    }
}

#[test]
fn float_test2() {
    let mut gen = Codegen::new(false);

    let panic = gen.entry_panic;
    let assume_float_to_f64 = gen.jit.label();
    monoasm!(&mut gen.jit,
    assume_float_to_f64:
        pushq rbp;
    );
    gen.assume_float_to_f64(0, panic);
    monoasm!(&mut gen.jit,
        popq rbp;
        ret;
    );
    let assume_int_to_f64 = gen.jit.label();
    monoasm!(&mut gen.jit,
    assume_int_to_f64:
        pushq rbp;
    );
    gen.assume_int_to_f64(0, panic);
    monoasm!(&mut gen.jit,
        popq rbp;
        ret;
    );
    gen.jit.finalize();
    let float_to_f64_entry = gen.jit.get_label_address(assume_float_to_f64);
    let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

    let float_to_f64: fn(Value) -> f64 =
        unsafe { std::mem::transmute(float_to_f64_entry.as_ptr()) };
    let int_to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
    assert_eq!(3.574, float_to_f64(Value::new_float(3.574)));
    assert_eq!(0.0, float_to_f64(Value::new_float(0.0)));
    assert_eq!(143.0, int_to_f64(Value::new_integer(143)));
    assert_eq!(14354813558.0, int_to_f64(Value::new_integer(14354813558)));
    assert_eq!(-143.0, int_to_f64(Value::new_integer(-143)));
}
