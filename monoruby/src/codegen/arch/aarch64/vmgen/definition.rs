use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// op 1 `SingletonMethodDef`: `def obj.name` -- singleton_define_method(
    /// vm, globals, name `[pc+8]`, func_id `[pc+12]`, obj slot `[pc+4]`).
    pub(in crate::codegen) fn a64_op_singleton_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0), #(8)];  // name
            ldr w3, [x(PC.0), #(12)];  // func_id
            ldrh x4, [x(PC.0), #(4)];  // obj slot
        );
        self.a64_slot_value(X4); // obj
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::singleton_define_method as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 70 `ClassDef` / op 71 `ModuleDef`: define the class/module, then
    /// run its body as a method with the class as `self`. Bytecode (16B):
    /// `+0` superclass slot (0 = none), `+2` base slot (0 = none),
    /// `+4` dst, `+8` name (IdentId), `+12` func_id (class body).
    pub(in crate::codegen) fn a64_op_class_def(&mut self, is_module: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let sup_zero = self.jit.label();
        let sup_done = self.jit.label();
        let base_zero = self.jit.label();
        let base_done = self.jit.label();
        // define_class(vm, globals, name, superclass, is_module, base)
        // superclass (x3): slot[+0] value, or 0 (None) if slot index is 0.
        monoasm_arm64!(&mut self.jit,
            ldrh x10, [x(PC.0)];
            cbz x10, sup_zero;
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x3, x10;
            b sup_done;
            sup_zero:
            mov x3, (0);
            sup_done:
        // base (x5): slot[+2] value, or 0 (None).
            ldrh x10, [x(PC.0), #(2)];
            cbz x10, base_zero;
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x5, x10;
            b base_done;
            base_zero:
            mov x5, (0);
            base_done:
            ldr w2, [x(PC.0), #(8)];  // name
            mov x4, (if is_module { 1 } else { 0 });
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::define_class as *const () as u64);
            blr x9;
            cbz x0, raise;
            mov x25, x0;  // X25 = self (the class), callee-saved
        );
        self.a64_class_def_run();
        p
    }

    /// op 22 `SingletonClassDef`: `class << base`. base = slot `[pc+0]`.
    pub(in crate::codegen) fn a64_op_singleton_class_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        // define_singleton_class(vm, globals, base) -> self
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldrh x2, [x(PC.0)];
        );
        self.a64_slot_value(X2); // base
        monoasm_arm64!(&mut self.jit,
            mov x9, (runtime::define_singleton_class as *const () as u64);
            blr x9;
            cbz x0, raise;
            mov x25, x0;  // self = singleton class
        );
        self.a64_class_def_run();
        p
    }

    /// Shared tail of class/module/singleton-class definition: with `X25` set
    /// to the (singleton) class, run the class body (`enter_classdef` ->
    /// call_funcdata -> `exit_classdef`) and store the result to dst `[pc+4]`.
    /// func_id is read from `[pc+12]`.
    pub(in crate::codegen) fn a64_class_def_run(&mut self) {
        let skip = self.jit.label();
        let raise = self.entry_raise.clone();
        // enter_classdef(vm, globals, func_id, self) -> &FuncData
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0), #(12)];  // func_id
            mov x3, x25;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::enter_classdef as *const () as u64);
            blr x9;
            mov x26, x0;  // X26 = &FuncData, callee-saved
        // cont frame: save caller PC + ACC (the body clobbers them).
            sub sp, sp, #(16);
            str x(PC.0), [sp];
            str x(ACC.0), [sp, #(8)];
        // frame setup: zero outer/svar/block; self = class; meta.
            mov x12, (0);
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];
            stur x25, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];  // self = class
            ldr x10, [x26, #(FUNCDATA_META as u32)];
            stur x10, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
        // call_funcdata: push frame, set lfp, pc, blr codeptr, restore cfp
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            stur x(LFP.0), [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
            ldr x(PC.0), [x26, #(FUNCDATA_PC as u32)];
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;  // x0 = class body result
            ldur x10, [sp, #(-(RSP_CFP as i32))];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        // restore caller LFP from its own frame (x29-relative)
            ldur x(LFP.0), [x29, #(-((BP_CFP + CFP_LFP) as i32))];
            mov x25, x0;  // save result across exit_classdef
        // exit_classdef(vm, globals)
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::exit_classdef as *const () as u64);
            blr x9;
            mov x0, x25;  // restore result
        // pop cont frame: restore PC + ACC
            ldr x(PC.0), [sp];
            ldr x(ACC.0), [sp, #(8)];
            add sp, sp, #(16);
        // If the class/module body raised, X0 is null: propagate the
        // error now (mirrors the trailing `vm_handle_error` in the x86
        // `class_def_sub`). Without this the exception is silently
        // dropped and left pending, tripping the `set_error` guard on the
        // next error. PC/ACC are already restored so entry_raise sees the
        // caller's frame, and exit_classdef above has popped the class
        // context.
            cbz x0, raise;
        // store result to dst [PC+4]
            ldrh x10, [x(PC.0), #(4)];
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
    }

    /// op 2 `method_def`: `define_method(vm, globals, name, func_id)`.
    /// Bytecode: `+8` name, `+12` func_id.
    pub(in crate::codegen) fn a64_op_method_def(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0), #(8)];  // name
            ldr w3, [x(PC.0), #(12)];  // func_id
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::define_method as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }
}
