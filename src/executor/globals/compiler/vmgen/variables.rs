use super::*;

impl Codegen {
    //
    // +---+---+---+---++---+---+---+---+
    // | op|ret|constId||     Value     |
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_load_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let const_version = self.const_version;
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdx, rdi;  // name: ConstSiteId
            movq rcx, [rip + const_version]; // usize
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (vm_get_constant);
            call rax;
        };
        self.vm_handle_error();
        monoasm! { self.jit,
            movq [r13 - 8], rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    //
    // +---+---+---+---++---+---+---+---+
    // | op|src|identId||               |
    // +---+---+---+---++---+---+---+---+
    //
    pub(super) fn vm_store_const(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        let const_version = self.const_version;
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdx, rdi;  // name: IdentId
            movq rcx, [r15];  // val: Value
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            addq [rip + const_version], 1;
            movq rax, (set_constant);
            call rax;
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
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rsi, rdi; // name: IdentId
            movq rdi, [rbp - (BP_SELF)];  // base: Value
            movq rdx, r12; // &mut Globals
            lea rcx, [r13 - 8]; // &mut ClassId
            lea r8, [r13 - 4]; // &mut IvarId
            movq rax, (get_instance_var_with_cache);
            call rax;
        };
        self.vm_store_r15();
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
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movq rdx, rdi;  // name: IdentId
            movq rdi, r12; //&mut Globals
            movq rsi, [rbp - (BP_SELF)];  // base: Value
            movq rcx, [r15];     // val: Value
            lea r8, [r13 - 8]; // &mut ClassId
            lea r9, [r13 - 4]; // &mut IvarId
            movq rax, (set_instance_var_with_cache);
            call rax;
        };
        self.vm_handle_error();
        self.fetch_and_dispatch();
        label
    }

    /// Load global variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|dst|identId||               |
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    pub(super) fn vm_load_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movl rsi, rdi; // name: IdentId
            movq rdi, r12; // &mut Globals
            movq rax, (get_global_var);
            call rax;
        };
        self.vm_store_r15();
        self.fetch_and_dispatch();
        label
    }

    /// Store global variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|val|identId||               |
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    pub(super) fn vm_store_gvar(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.vm_get_addr_r15();
        monoasm! { self.jit,
            movl rsi, rdi;  // name: IdentId
            movq rdi, r12;  // &mut Globals
            movq rdx, [r15];  // base: Value
            movq rax, (set_global_var);
            call rax;
        };
        self.fetch_and_dispatch();
        label
    }

    /// Load dynamic local variable
    ///
    /// ~~~text
    /// +---+---+---+---++---+---+---+---+
    /// | op|dst|src|out||       |       |
    /// +---+---+---+---++---+---+---+---+
    ///
    /// dst: destination register
    /// src: source register
    /// out: source outer level
    /// ~~~
    pub(super) fn vm_load_dvar(&mut self) -> CodePtr {
        // r15: dst
        // rdi: src reg
        // rsi: src outer
        let label = self.jit.get_current_address();
        let loop_ = self.jit.label();
        let loop_exit = self.jit.label();
        let exit = self.jit.label();
        monoasm! { self.jit,
            movq rax, [rbp - (BP_OUTER)];
        loop_:
            subq rsi, 1;
            jz   loop_exit;
            movq rax, [rax];
            jmp  loop_;
        loop_exit:
            lea  rax, [rax + (BP_OUTER)];
            negq rdi;
            movq rax, [rax + rdi * 8 - (BP_SELF)];
        };
        self.vm_store_r15_if_nonzero(exit);
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
        monoasm! { self.jit,
            movq rax, [rbp - (BP_OUTER)];
        loop_:
            subq rdi, 1;
            jz   loop_exit;
            movq rax, [rax];
            jmp  loop_;
        loop_exit:
            lea  rax, [rax + (BP_OUTER)];
            negq rsi;
            negq r15;
            movq rdi, [rbp + rsi * 8 - (BP_SELF)];
            movq [rax + r15 * 8 - (BP_SELF)], rdi;
        };
        self.fetch_and_dispatch();
        label
    }
}
