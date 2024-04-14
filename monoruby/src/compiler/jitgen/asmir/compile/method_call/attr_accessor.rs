use super::*;

impl Codegen {
    ///
    /// Attribute reader
    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(in crate::compiler::jitgen) fn attr_reader(&mut self, ivar_id: IvarId) {
        let exit = self.jit.label();
        if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
            let not_object = self.jit.label();
            monoasm!( &mut self.jit,
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  not_object;
                movq rax, [rdi + (ivar_id.get() as i32 * 8 + RVALUE_OFFSET_KIND)];
                movq rdi, (NIL_VALUE);
                testq rax,rax;
                cmoveqq rax, rdi;
            exit:
            );
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            not_object:
                movl rsi, (ivar_id.get());
            );
            self.load_ivar_heap_index();
            monoasm!( &mut self.jit,
                jmp  exit;
            );
            self.jit.select_page(0);
        } else {
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
                xorq rax, rax;
                movl rdx, (OBJECT_INLINE_IVAR);
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                cmoveqq rax, rdx;
                subl rsi, rax;
            );
            self.load_ivar_heap_index();
        }
    }

    ///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - rsi: index
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rdx
    ///
    fn load_ivar_heap_index(&mut self) {
        let exit = self.jit.label();
        monoasm!( &mut self.jit,
            movq rax, (NIL_VALUE);
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rdx, rdx;
            jz   exit;
            movq rdi, [rdx + (MONOVEC_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rdx + (MONOVEC_LEN)]; // len
            cmpq rdi, rsi;
            movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
            cmovgtq rax, [rdi + rsi * 8];
        exit:
        );
    }

    ///
    /// Attribute writer
    ///
    /// ### in
    /// rdi: receiver: Value
    /// rdx: value: Value
    ///
    pub(in crate::compiler::jitgen) fn attr_writer(
        &mut self,
        using_xmm: UsingXmm,
        error: DestLabel,
        ivar_id: IvarId,
    ) {
        let exit = self.jit.label();
        let no_inline = self.jit.label();
        if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
            monoasm!( &mut self.jit,
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  no_inline;
                movq [rdi + (ivar_id.get() as i32 * 8 + RVALUE_OFFSET_KIND)], rdx;
            exit:
            );
            self.jit.select_page(1);
            self.jit.bind_label(no_inline);
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
            );
            self.set_ivar(using_xmm);
            self.handle_error(error);
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        } else {
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
            );
            self.set_ivar(using_xmm);
            self.handle_error(error);
        }
    }
}
