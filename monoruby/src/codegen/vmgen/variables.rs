use super::*;

extern "C" fn vm_get_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    crate::executor::catch_panic_extern_c(vm, globals, "vm_get_constant", |vm, globals| {
        if let Some(cache) = &globals.store[site_id].cache {
            let base_class = globals.store[site_id]
                .base
                .map(|base| unsafe { vm.get_slot(base) }.unwrap());
            if cache.version == const_version && cache.base_class == base_class {
                return Some(cache.value);
            };
        }
        match vm.find_constant(globals, site_id) {
            Ok((value, base_class)) => {
                globals.store[site_id].cache = Some(ConstCache {
                    version: const_version,
                    base_class,
                    value,
                });
                Some(value)
            }
            Err(err) => {
                vm.set_error(err);
                None
            }
        }
    })
    .unwrap_or(None)
}

extern "C" fn vm_check_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    crate::executor::catch_panic_extern_c(vm, globals, "vm_check_constant", |vm, globals| {
        if let Some(cache) = &globals.store[site_id].cache {
            let base_class = globals.store[site_id]
                .base
                .map(|base| unsafe { vm.get_slot(base) }.unwrap());
            if cache.version == const_version && cache.base_class == base_class {
                return Some(cache.value);
            };
        }
        match vm.find_constant(globals, site_id) {
            Ok((value, base_class)) => {
                globals.store[site_id].cache = Some(ConstCache {
                    version: const_version,
                    base_class,
                    value,
                });
                Some(value)
            }
            Err(_) => Some(Value::nil()),
        }
    })
    .unwrap_or(None)
}

impl Codegen {
    //
    // +---+---+---+---++---+---+---+---+
    // | op|ret|constId||     Value     |
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_load_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let const_version = self.const_version_label();
        self.fetch2();
        monoasm! { &mut self.jit,
            movq rdx, rdi;  // ConstSiteId
            movq rcx, [rip + const_version]; // usize
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (vm_get_constant);
            call rax;
        };
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            movq [r13 - 8], rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|ret|constId||     Value     |
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_check_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let const_version = self.const_version_label();
        self.fetch2();
        monoasm! { &mut self.jit,
            movq rdx, rdi;  // ConstSiteId
            movq rcx, [rip + const_version]; // usize
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (vm_check_constant);
            call rax;
        };
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            movq [r13 - 8], rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|src|ConstId||               |
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_store_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let raise = self.entry_raise();
        let const_version = self.const_version_label();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            movq rdx, rdi;  // ConstSiteId
            movq rcx, r15;  // val: Value
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            addq [rip + const_version], 1;
            movq rax, (runtime::set_constant);
            call rax;
            testq rax, rax;
            jz   raise;
        };
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|dst|identId||ClassId| IvarId|
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_load_ivar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        monoasm! { &mut self.jit,
            movq rsi, rdi; // name: IdentId
            movq rdi, [r14 - (LFP_SELF)];  // base: Value
            movq rdx, r12; // &mut Globals
            lea rcx, [r13 - 8]; // &mut ClassId
            movq rax, (get_instance_var_with_cache);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|src|identId||ClassId| IvarId|
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_store_ivar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            movq rcx, rdi;  // name: IdentId
            movq rdi, rbx; //&mut Executor
            movq rsi, r12; //&mut Globals
            movq rdx, [r14 - (LFP_SELF)];  // base: Value
            movq r8, r15;     // val: Value
            lea r9, [r13 - 8]; // &mut ClassId
            movq rax, (set_instance_var_with_cache);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Load class variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | name  |dst| op||               |
    /// +---+---+---+---++---+---+---+---+
    ///    rdi   r15
    /// ~~~
    /// - name: name of class variable (IdentId)
    /// - dst: destination slot (SlotId)
    ///
    pub(super) fn vm_load_cvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        monoasm! { &mut self.jit,
            movl rdx, rdi; // name: IdentId
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::get_class_var);
            call rax;
        };
        self.vm_handle_error();
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Check class variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | name  |dst| op||               |
    /// +---+---+---+---++---+---+---+---+
    ///    rdi   r15
    /// ~~~
    /// - name: name of class variable (IdentId)
    /// - dst: destination slot (SlotId)
    ///
    pub(super) fn vm_check_cvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        monoasm! { &mut self.jit,
            movl rdx, rdi; // name: IdentId
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::check_class_var);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Store class variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | name  |val| op||               |
    /// +---+---+---+---++---+---+---+---+
    ///    rdi   r15
    /// ~~~
    /// - name: name of the class variable (IdentId)
    /// - val: source slot (Value)
    ///
    pub(super) fn vm_store_cvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            movl rdx, rdi; // name: IdentId
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rcx, r15;  // dst: Value
            movq rax, (runtime::set_class_var);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Load global variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | name  |dst| op||               |
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    /// - name: name of the global variable (IdentId)
    /// - dst: destination slot (SlotId)
    ///
    pub(super) fn vm_load_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        monoasm! { &mut self.jit,
            movl rdx, rdi; // name: IdentId
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::get_global_var);
            call rax;
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Store global variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | name  |val| op||               |
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    /// - name: name of the global variable (IdentId)
    /// - val: source slot (Value)
    ///
    pub(super) fn vm_store_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.fetch2();
        self.vm_get_slot_value(GP::R15);
        monoasm! { &mut self.jit,
            movl rdx, rdi;  // name: IdentId
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rcx, r15;  // val: Value
            movq rax, (runtime::set_global_var);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    /// Alias global variable.
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op| 0 |  new  ||      old      |
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    ///
    /// `new` and `old` are both [`IdentId`]s. `new` lives in the
    /// `op1_l` slot of the primary instruction word; `old` lives in the
    /// extension u32 right after it.
    pub(super) fn vm_alias_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            movl rsi, [r13 - 16];   // rsi <- new (op1_l)
            movl rdx, [r13 - 8];    // rdx <- old (extension u32)
            movq rdi, r12;          // &mut Globals
            movq rax, (runtime::alias_global_var);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    /// Load dynamic local variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// |out|src|dst|op ||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///  rsi rdi r15
    ///
    /// dst: destination slot (SlotId)
    /// src: source slot (SlotId)
    /// out: source outer level
    /// ~~~
    pub(super) fn vm_load_dvar(&mut self) -> CodePtr {
        // r15: dst
        // rdi: src
        // rsi: src outer
        let label = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let raise = self.entry_raise();
        self.fetch3();
        monoasm! { &mut self.jit,
            movq rax, [r14];
        loop_:
            subq rsi, 1;
            jz   loop_exit;
            movq rax, [rax];
            jmp  loop_;
        loop_exit:
            negq rdi;
            testq rax, rax;
            jz   raise;
            movq rax, [rax + rdi * 8 - (LFP_SELF)];
        };
        self.vm_store_r15(GP::Rax);
        self.fetch_and_dispatch();
        label
    }

    /// Store dynamic local variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|dst|out|src||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// dst: destination register
    /// out: destination outer level
    /// src: source register
    /// ~~~
    pub(super) fn vm_store_dvar(&mut self) -> CodePtr {
        // r15: dst reg
        // rdi: dst outer
        // rsi: src
        let label = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        self.fetch3();
        monoasm! { &mut self.jit,
            movq rax, [r14];
        loop_:
            subq rdi, 1;
            jz   loop_exit;
            movq rax, [rax];
            jmp  loop_;
        loop_exit:
            negq rsi;
            negq r15;
            movq rdi, [r14 + rsi * 8 - (LFP_SELF)];
            movq [rax + r15 * 8 - (LFP_SELF)], rdi;
        };
        self.fetch_and_dispatch();
        label
    }
}
