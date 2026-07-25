use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// op 148 `LoadDynVar`: dst `[pc+4]` <- the slot `[pc+2]` of the outer
    /// frame `[pc+0]` levels up the captured outer chain.
    pub(in crate::codegen) fn a64_op_load_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(LFP.0)];  // X10 = level-1 outer ([LFP] = LFP_OUTER)
            ldrh x11, [x(PC.0)];  // outer level
            loop_:
            subs x11, x11, #(1);
        );
        self.jit.bcond_label(Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x10];  // walk up
            b loop_;
            exit:
            cbz x10, raise;
            ldrh x12, [x(PC.0), #(2)];  // src slot in outer frame
            neg x12, x12;
            add x13, x10, x12, lsl #(3);
            ldur x14, [x13, #(-(LFP_SELF as i32))];  // value
        // store to dst [pc+4]
            ldrh x12, [x(PC.0), #(4)];
            cbz x12, skip;
            neg x12, x12;
            add x13, x(LFP.0), x12, lsl #(3);
            stur x14, [x13, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 149 `StoreDynVar`: the slot `[pc+4]` of the outer frame `[pc+2]`
    /// levels up <- src slot `[pc+0]` of the current frame.
    pub(in crate::codegen) fn a64_op_store_dvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(LFP.0)];  // level-1 outer
            ldrh x11, [x(PC.0), #(2)];  // outer level
            loop_:
            subs x11, x11, #(1);
        );
        self.jit.bcond_label(Cond::Eq, &exit);
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x10];
            b loop_;
            exit:
        // src value from the current frame (slot [pc+0])
            ldrh x12, [x(PC.0)];
            neg x12, x12;
            add x13, x(LFP.0), x12, lsl #(3);
            ldur x14, [x13, #(-(LFP_SELF as i32))];
        // store to dst slot [pc+4] in the outer frame
            ldrh x12, [x(PC.0), #(4)];
            neg x12, x12;
            add x13, x10, x12, lsl #(3);
            stur x14, [x13, #(-(LFP_SELF as i32))];
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 24 `CheckCvar`: check_class_var(vm, globals, name `[pc+0]`) -> dst.
    pub(in crate::codegen) fn a64_op_check_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::check_class_var as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 25 `LoadGvar`: get_global_var(vm, globals, name `[pc+0]`) -> Value.
    pub(in crate::codegen) fn a64_op_load_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::get_global_var as *const () as u64);
            blr x9;
        );
        self.a64_store_dst_and_next(&skip);
        p
    }

    /// op 27 `LoadCvar`: get_class_var(vm, globals, name `[pc+0]`) -> Option.
    pub(in crate::codegen) fn a64_op_load_cvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            mov x9, (runtime::get_class_var as *const () as u64);
            blr x9;
        );
        self.a64_checked_store_next(&raise);
        p
    }

    /// ops 26/29 `StoreGvar`/`StoreCvar`: set_*_var(vm, globals, name `[pc+0]`,
    /// val `[pc+4]`) -> Option (error-only; no result slot).
    pub(in crate::codegen) fn a64_op_store_var(&mut self, set_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // name
            ldrh x3, [x(PC.0), #(4)];
        );
        self.a64_slot_value(X3); // val
        monoasm_arm64!(&mut self.jit,
            mov x9, (set_fn);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 28 `AliasGvar`: alias_global_var(globals, new `[pc+0]`, old `[pc+8]`).
    pub(in crate::codegen) fn a64_op_alias_gvar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(GLOBALS.0);
            ldr w1, [x(PC.0)];  // new
            ldr w2, [x(PC.0), #(8)];  // old
            mov x9, (runtime::alias_global_var as *const () as u64);
            blr x9;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 16 `LoadIvar`: slot[`[pc+4]`] <- `self.@name` (name `[pc+0]`),
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_load_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let skip = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldur x0, [x(LFP.0), #(-(LFP_SELF as i32))];  // base = self
            ldr w1, [x(PC.0)];  // name
            mov x2, x(GLOBALS.0);
            add x3, x(PC.0), #(8);  // &cache
            mov x9, (get_instance_var_with_cache as *const () as u64);
            blr x9;
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 17 `StoreIvar`: `self.@name` (name `[pc+0]`) <- slot[`[pc+4]`],
    /// with an inline (ClassId, IvarId) cache at `[pc+8]`.
    pub(in crate::codegen) fn a64_op_store_ivar(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldur x2, [x(LFP.0), #(-(LFP_SELF as i32))];  // base = self
            ldr w3, [x(PC.0)];  // name
            ldrh x10, [x(PC.0), #(4)];  // src slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x4, x10;  // val
            add x5, x(PC.0), #(8);  // &cache
            mov x9, (set_instance_var_with_cache as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 10 `LoadConst`: slot[`[pc+4]`] <- constant at ConstSiteId `[pc+0]`.
    /// (x86 `vm_load_const`; the JIT inline-cache slot at `[pc+8]` is not
    /// written — the VM relies on the ConstSite cache + const_version.)
    pub(in crate::codegen) fn a64_op_load_const(&mut self, get_fn: u64) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0)];  // ConstSiteId
            mov x11, (cv_addr);
            ldr x3, [x11];  // const_version
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (get_fn);
            blr x9;
            cbz x0, raise;
            ldrh x10, [x(PC.0), #(4)];  // dst slot
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// op 11 `StoreConst`: define constant ConstSiteId `[pc+0]` <- slot
    /// `[pc+4]`, bumping const_version. (x86 `vm_store_const`.)
    pub(in crate::codegen) fn a64_op_store_const(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let cv_addr = self.jit.get_label_address(&self.const_version_label()).as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            ldr w2, [x(PC.0)];  // ConstSiteId
            ldrh x10, [x(PC.0), #(4)];  // src slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x3, x10;  // val
        // const_version += 1
            mov x11, (cv_addr);
            ldr x12, [x11];
            add x12, x12, #(1);
            str x12, [x11];
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::set_constant as *const () as u64);
            blr x9;
            cbz x0, raise;
            add x(PC.0), x(PC.0), #(16);
        );
        self.a64_fetch_and_dispatch();
        p
    }
}
