use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;

use super::*;

mod jitgen;
mod vmgen;

type EntryPoint = extern "C" fn(&mut Executor, &mut Globals, *const FuncData) -> Option<Value>;

type Invoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    *const FuncData,
    Value,
    *const Value,
    usize,
) -> Option<Value>;

type Invoker2 =
    extern "C" fn(&mut Executor, &mut Globals, *const FuncData, Value, Arg, usize) -> Option<Value>;

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
    entry_find_method: DestLabel,
    pub vm_return: DestLabel,
    pub f64_to_val: DestLabel,
    pub heap_to_f64: DestLabel,
    pub div_by_zero: DestLabel,
    pub dispatch: Vec<CodePtr>,
    pub method_invoker: Invoker,
    pub method_invoker2: Invoker2,
    pub block_invoker: Invoker,
}

//
// Runtime functions.
//

///
/// Get an absolute address of the given method.
///
/// If no method was found, return None (==0u64).
///
extern "C" fn find_method(
    globals: &mut Globals,
    func_name: IdentId,
    args_len: usize,
    receiver: Value,
) -> Option<&FuncData> {
    let func_id = globals.find_method_checked(receiver, func_name, args_len)?;
    let data = globals.compile_on_demand(func_id);
    Some(data)
}

extern "C" fn vm_get_func_data(globals: &mut Globals, func_id: FuncId) -> &FuncData {
    globals.compile_on_demand(func_id)
}

extern "C" fn vm_get_block_data(globals: &mut Globals, block: Value) -> &FuncData {
    if let Some(func_id) = block.try_fixnum() {
        if let Ok(func_id) = u32::try_from(func_id) {
            return globals.compile_on_demand(FuncId(func_id));
        }
    }
    unreachable!()
}

extern "C" fn gen_array(src: *const Value, len: usize) -> Value {
    let mut v = if len == 0 {
        vec![]
    } else {
        unsafe { std::slice::from_raw_parts(src.sub(len - 1), len).to_vec() }
    };
    v.reverse();
    Value::new_array_from_vec(v)
}

extern "C" fn gen_range(
    start: Value,
    end: Value,
    globals: &mut Globals,
    exclude_end: bool,
) -> Option<Value> {
    globals.generate_range(start, end, exclude_end)
}

#[repr(C)]
struct ClassIdSlot {
    base: ClassId,
    idx: ClassId,
}

extern "C" fn get_index(
    interp: &mut Executor,
    globals: &mut Globals,
    base: Value,
    index: Value,
    class_slot: &mut ClassIdSlot,
) -> Option<Value> {
    let base_classid = base.class_id();
    class_slot.base = base_classid;
    match base_classid {
        ARRAY_CLASS => {
            if let Some(idx) = index.try_fixnum() {
                class_slot.idx = INTEGER_CLASS;
                return base.as_array().get_index(idx);
            }
        }
        _ => {}
    }
    class_slot.idx = index.class_id();
    interp.invoke_method(globals, IdentId::_INDEX, base, &[index])
}

extern "C" fn set_index(
    interp: &mut Executor,
    globals: &mut Globals,
    mut base: Value,
    index: Value,
    src: Value,
) -> Option<Value> {
    match base.class_id() {
        ARRAY_CLASS => {
            if let Some(idx) = index.try_fixnum() {
                return base.as_array_mut().set_index(globals, idx, src);
            }
        }
        _ => {}
    }
    interp.invoke_method(globals, IdentId::_INDEX_ASSIGN, base, &[index, src])
}

extern "C" fn get_instance_var(base: Value, name: IdentId, globals: &mut Globals) -> Value {
    globals.get_ivar(base, name).unwrap_or_default()
}

extern "C" fn set_instance_var(
    globals: &mut Globals,
    base: Value,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    globals.set_ivar(base, name, val)?;
    Some(val)
}

extern "C" fn define_class(
    interp: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    superclass: Option<Value>,
) -> Option<Value> {
    let parent = interp.get_class_context();
    let self_val = match globals.get_constant(parent, name) {
        Some(val) => {
            let class = val.expect_class(name, globals)?;
            if let Some(superclass) = superclass {
                let super_name = globals.val_tos(superclass);
                let super_name = IdentId::get_ident_id(&super_name);
                let super_class = superclass.expect_class(super_name, globals)?;
                if Some(super_class) != class.super_class(globals) {
                    globals.err_superclass_mismatch(name);
                    return None;
                }
            }
            val
        }
        None => {
            let superclass = match superclass {
                Some(superclass) => {
                    let name = globals.val_tos(superclass);
                    let name = IdentId::get_ident_id_from_string(name);
                    superclass.expect_class(name, globals)?
                }
                None => OBJECT_CLASS,
            };
            globals.define_class_by_ident_id(name, Some(superclass), parent)
        }
    };
    //globals.get_singleton_id(self_val.as_class());
    interp.push_class_context(self_val.as_class());
    Some(self_val)
}

extern "C" fn pop_class_context(interp: &mut Executor, _globals: &mut Globals) {
    interp.pop_class_context();
}

extern "C" fn unimplemented_inst(_: &mut Executor, _: &mut Globals, opcode: u64) {
    panic!("unimplemented inst. {:016x}", opcode);
}

extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

/*pub extern "C" fn eprintln(rdi: u64, rsi: u64) {
    eprintln!("rdi:{:016x} rsi:{:016x}", rdi, rsi);
}*/

extern "C" fn error_divide_by_zero(globals: &mut Globals) {
    globals.err_divide_by_zero();
}

extern "C" fn err_wrong_number_of_arguments(globals: &mut Globals, given: usize, expected: usize) {
    globals.err_argument(&format!(
        "wrong number of arguments (given {given}, expected {expected})"
    ));
}

extern "C" fn get_error_location(
    _interp: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BcPc,
) {
    let func_info = &globals.func[meta.func_id()];
    let bc_base = func_info.data.pc;
    let normal_info = match &func_info.kind {
        FuncKind::ISeq(info) => info,
        FuncKind::Builtin { .. } => return,
        FuncKind::AttrReader { .. } => return,
        FuncKind::AttrWriter { .. } => return,
    };
    let sourceinfo = normal_info.sourceinfo.clone();
    let loc = normal_info.sourcemap[pc - bc_base];
    globals.push_error_location(loc, sourceinfo);
}

impl Codegen {
    pub(crate) fn new(no_jit: bool, main_object: Value) -> Self {
        let mut jit = JitMemory::new();
        let class_version = jit.const_i32(0);
        let const_version = jit.const_i64(0);
        let entry_panic = jit.label();
        let entry_find_method = jit.label();
        let jit_return = jit.label();
        let vm_return = jit.label();
        let div_by_zero = jit.label();
        let heap_to_f64 = jit.label();
        //jit.select_page(1);
        monoasm!(&mut jit,
        entry_panic:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (op::_dump_stacktrace);
            subq rax, 8;
            call rax;
            addq rax, 8;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (panic);
            jmp rax;
        entry_find_method:
            movq rdi, r12;
            movq rax, (find_method);
            jmp  rax;
        vm_return:
            movq r15, rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - (OFFSET_META)];
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
        );

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
        //jit.select_page(0);
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
            vm_return,
            f64_to_val: entry_panic,
            heap_to_f64,
            div_by_zero,
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
        codegen
    }

    /// Push control frame and set outer.
    ///
    /// in: rcx<-self
    /// destroy: rax, rdi
    fn push_frame(&mut self, invoke_block: bool) {
        monoasm!(self.jit,
            movq rax, [rbx];
            lea  rdi, [rsp - (16 + OFFSET_CFP)];
            movq [rdi], rax;
            movq [rbx], rdi;
        );
        if invoke_block {
            monoasm! { self.jit,
                movq rax, [rax];
                lea  rdi, [rax - ((OFFSET_OUTER - OFFSET_CFP) as i32)];
                movq [rsp - (16 + OFFSET_OUTER)], rdi;
                // set self
                movq  rdi, [rax - ((OFFSET_SELF - OFFSET_CFP) as i32)];
                movq [rsp - (16 + OFFSET_SELF)], rdi;
            };
        } else {
            monoasm! { self.jit,
                movq [rsp - (16 + OFFSET_OUTER)], 0;
                // set self
                movq [rsp - (16 + OFFSET_SELF)], rcx;
            };
        }
    }

    /// Pop control frame
    ///
    /// destroy: rdi
    fn pop_frame(&mut self) {
        monoasm!(self.jit,
            lea  rdi, [rbp - (OFFSET_CFP)];
            movq [rbx], rdi;
        );
    }

    ///
    /// calculate an offset of stack pointer.
    ///
    fn calc_offset(&mut self) {
        monoasm!(self.jit,
            addq rax, (OFFSET_ARG0 / 8 + 1);
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

    ///
    /// Set jit compilation stub code for an entry point of each Ruby methods.
    ///
    pub(super) fn gen_jit_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        let counter = self.jit.const_i32(5);
        let entry = self.jit.label();
        monoasm!(self.jit,
        entry:
            subl [rip + counter], 1;
            jne vm_entry;
            movl rsi, [rsp - (8 + OFFSET_FUNCID)];
            movq rdx, [rsp - (8 + OFFSET_SELF)];
            subq rsp, 1024;
            pushq rdi;
            movq rdi, r12;
            movq rax, (Self::exec_jit_compile);
            call rax;
            movw [rip + entry], 0xe9;   // jmp
            lea rdi, [rip + entry];
            addq rdi, 5;
            subq rax, rdi;
            movl [rdi - 4], rax;
            popq rdi;
            addq rsp, 1024;
            jmp entry;
        );
        codeptr
    }

    pub(super) fn gen_vm_stub(&mut self) -> CodePtr {
        let vm_entry = self.vm_entry;
        let codeptr = self.jit.get_current_address();
        monoasm!(self.jit,
            jmp vm_entry;
        );
        codeptr
    }

    ///
    /// Generate a wrapper for a native function with C ABI.
    ///
    /// - stack layout at the point of just after a wrapper was called.
    /// ~~~text
    ///       +-------------+
    ///  0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |             |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |  %0 (self)  |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///
    ///  meta
    /// +-------------------+ -0x08
    /// |     2:Native      |
    /// +-------------------+ -0x0a
    /// |    register_len   |
    /// +-------------------+ -0x0c
    /// |                   |
    /// +      FuncId       + -0x0e
    /// |                   |
    /// +-------------------+ -0x10
    ///
    /// argument registers:
    ///   rdi: number of args
    ///
    /// global registers:
    ///   rbx: &mut Interp
    ///   r12: &mut Globals
    ///   r13: pc (dummy for builtin funcions)
    /// ~~~
    ///
    pub(super) fn wrap_native_func(&mut self, abs_address: u64) -> CodePtr {
        let label = self.jit.get_current_address();
        // calculate stack offset
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            movq r8, rdi;
            movq rax, rdi;
        );
        self.calc_offset();
        monoasm!(self.jit,
            subq rsp, rax;
            lea  rcx, [rbp - (OFFSET_ARG0)];     // rcx <- *const arg[0]
            movq  r9, [rbp - (OFFSET_BLOCK)];     // r9 <- block
            movq  rdx, [rbp - (OFFSET_SELF)];    // rdx <- self
            // we should overwrite reg_num because the func itself does not know actual number of arguments.
            movw [rbp - (OFFSET_REGNUM)], rdi;

            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (abs_address);
            // fn(&mut Interp, &mut Globals, Value, *const Value, len:usize, block:Option<Value>)
            call rax;

            leave;
            ret;
        );
        label
    }

    ///
    /// Generate attr_reader.
    ///
    /// - stack layout at the point of just after being called.
    /// ~~~text
    ///       +-------------+
    ///  0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |             |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |  %0 (self)  |
    ///       +-------------+
    /// ~~~
    pub(super) fn gen_attr_reader(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(0);
        monoasm!(self.jit,
            movq rdi, [rsp - (8 + OFFSET_SELF)];  // self: Value
            movq rsi, (ivar_name.get()); // name: IdentId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cached_class];
            lea  r8, [rip + cached_ivarid];
            movq rax, (get_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
        label
    }

    ///
    /// Generate attr_writer.
    ///
    /// - stack layout at the point of just after being called.
    /// ~~~text
    ///       +-------------+
    ///  0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |             |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |  %0 (self)  |
    ///       +-------------+
    /// -0x20 |   %1(val)   |
    ///       +-------------+
    /// ~~~
    pub(super) fn gen_attr_writer(&mut self, ivar_name: IdentId) -> CodePtr {
        let label = self.jit.get_current_address();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(0);
        monoasm!(self.jit,
            movq rdi, r12; //&mut Globals
            movq rsi, [rsp - (8 + OFFSET_SELF)];  // self: Value
            movq rdx, (ivar_name.get()); // name: IdentId
            movq rcx, [rsp - (8 + OFFSET_ARG0)];  //val: Value
            lea  r8, [rip + cached_class];
            lea  r9, [rip + cached_ivarid];
            movq rax, (set_instance_var_with_cache);
            subq rsp, 8;
            call rax;
            addq rsp, 8;
            ret;
        );
        label
    }
}

impl Codegen {
    ///
    /// Compile the Ruby method.
    ///
    extern "C" fn exec_jit_compile(
        globals: &mut Globals,
        func_id: FuncId,
        self_value: Value,
    ) -> CodePtr {
        globals.func[func_id].data.meta.set_jit();
        let label = globals.jit_compile_ruby(func_id, self_value, None);
        globals.codegen.jit.get_label_address(label)
    }

    extern "C" fn exec_jit_recompile(
        globals: &mut Globals,
        func_id: FuncId,
        self_value: Value,
    ) -> CodePtr {
        let codeptr = Self::exec_jit_compile(globals, func_id, self_value);
        let target = globals.func[func_id].data.codeptr.unwrap();
        let offset = codeptr - target - 5;
        unsafe { *(target.as_ptr().add(1) as *mut i32) = offset as i32 };
        codeptr
    }

    ///
    /// Compile the loop.
    ///
    extern "C" fn exec_jit_partial_compile(
        globals: &mut Globals,
        func_id: FuncId,
        self_value: Value,
        pc: BcPc,
    ) {
        let label = globals.jit_compile_ruby(func_id, self_value, Some(pc));
        let codeptr = globals.codegen.jit.get_label_address(label);
        pc.write2(codeptr.as_ptr() as u64);
    }
}

impl Globals {
    fn jit_compile_ruby(
        &mut self,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
    ) -> DestLabel {
        #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
        {
            let func = self.func[func_id].as_ruby_func();
            let start_pos = func.get_pc_index(position);
            eprintln!(
                "==> start {} compile: {} {:?} self_class:{} start:[{:05}] bytecode:{:?}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                func.name(),
                func.id,
                self_value.class_id().get_name(self),
                start_pos,
                func.bytecode().as_ptr(),
            );
        }
        let (label, _sourcemap) = self
            .codegen
            .jit_compile_ruby(&self.func, func_id, self_value, position);

        #[cfg(any(feature = "emit-asm"))]
        self.dump_disas(_sourcemap, func_id);
        label
    }
}
