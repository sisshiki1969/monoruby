use super::*;

impl BBContext {
    pub(super) fn compile_call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        pc: BytecodePtr,
        callid: CallSiteId,
        cache: MethodCacheEntry,
    ) -> CompileResult {
        if store[callid].block_fid.is_none()
            && let Some(info) = store.inline_info.get_inline(cache.func_id)
        {
            let f = &info.inline_gen;
            if self.inline_call(ir, store, f, callid, &cache, pc) {
                return CompileResult::Continue;
            }
        }
        self.call(ir, store, cache, callid, pc)
    }

    pub(super) fn compile_binop_call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        fid: FuncId,
        version: u32,
        info: BinOpInfo,
        pc: BytecodePtr,
    ) -> CompileResult {
        assert!(matches!(
            store[fid].kind,
            FuncKind::Builtin { .. } | FuncKind::ISeq(_)
        ));
        let callee = &store[fid];
        if (!callee.is_rest() && callee.max_positional_args() < 1) || callee.req_num() > 1 {
            return CompileResult::Recompile;
        }
        let BinOpInfo {
            dst,
            mode,
            lhs_class,
            ..
        } = info;
        let deopt = ir.new_deopt(self, pc);
        self.fetch_lhs(ir, mode);
        ir.guard_lhs_class_for_mode(self, mode, lhs_class, deopt);
        ir.push(AsmInst::GuardClassVersion(version, deopt));

        let evict = ir.new_evict();
        ir.reg_move(GP::Rdi, GP::R13);
        let using_xmm = self.get_using_xmm();
        ir.xmm_save(using_xmm);

        ir.set_binop_arguments(store, self, fid, mode);

        self.unlink(ir, dst);
        self.clear(ir);
        let error = ir.new_error(self, pc);
        self.writeback_acc(ir);
        ir.push(AsmInst::BinopCached {
            callee_fid: fid,
            recv_class: lhs_class,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        self.rax2acc(ir, dst);
        ir.push(AsmInst::ImmediateEvict { evict });
        ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
        CompileResult::Continue
    }

    pub(in crate::compiler::jitgen) fn compile_yield(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        pc: BytecodePtr,
        callid: CallSiteId,
    ) {
        let callinfo = &store[callid];
        let dst = callinfo.dst;
        self.write_back_callargs_and_dst(ir, &callinfo);
        self.writeback_acc(ir);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let evict = ir.new_evict();
        ir.push(AsmInst::Yield {
            callid,
            using_xmm,
            error,
            evict,
        });
        self.rax2acc(ir, dst);
    }

    fn call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        cache: MethodCacheEntry,
        callid: CallSiteId,
        pc: BytecodePtr,
    ) -> CompileResult {
        let CallSiteInfo { dst, recv, .. } = store[callid];
        let MethodCacheEntry {
            mut recv_class,
            mut func_id,
            mut version,
        } = cache;
        if recv.is_self() && self.self_class != recv_class {
            // the inline method cache is invalid because the receiver class is not matched.
            let class_version = self.class_version();
            let name = store[callid].name.unwrap();
            if let Some(entry) = store.check_method_for_class(self.self_class, name, class_version)
                && let Some(fid) = entry.func_id()
            {
                recv_class = self.self_class;
                func_id = fid;
                version = class_version;
            } else {
                return CompileResult::Recompile;
            }
        }
        // We must write back and unlink all local vars when they are possibly accessed from inner blocks.
        if store[callid].block_fid.is_some() || store[func_id].meta().is_eval() {
            self.write_back_locals(ir);
        }
        self.fetch_for_gpr(ir, recv, GP::Rdi);
        let (deopt, error) = ir.new_deopt_error(self, pc);
        let using_xmm = self.get_using_xmm();
        ir.guard_version(func_id, version, callid, using_xmm, deopt, error);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_self() && !self.is_class(recv, recv_class) {
            ir.guard_class(self, recv, GP::Rdi, recv_class, deopt);
        }
        if let Some(evict) = self.call_cached(ir, store, callid, func_id, recv_class, pc) {
            self.rax2acc(ir, dst);
            if let Some(evict) = evict {
                ir.push(AsmInst::ImmediateEvict { evict });
                ir[evict] = SideExit::Evict(Some((pc + 2, self.get_write_back())));
            }
        } else {
            return CompileResult::Recompile;
        }

        CompileResult::Continue
    }

    fn inline_call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        f: impl Fn(&mut AsmIr, &Store, &mut BBContext, CallSiteId, ClassId, BytecodePtr) -> bool,
        callid: CallSiteId,
        cache: &MethodCacheEntry,
        pc: BytecodePtr,
    ) -> bool {
        let mut ctx_save = self.clone();
        let ir_save = ir.save();
        let recv = store[callid].recv;
        self.fetch_for_gpr(ir, recv, GP::Rdi);
        let (deopt, error) = ir.new_deopt_error(self, pc);
        let using_xmm = self.get_using_xmm();
        let MethodCacheEntry {
            recv_class,
            func_id,
            version,
        } = cache;
        ir.guard_version(*func_id, *version, callid, using_xmm, deopt, error);
        if !recv.is_self() && !self.is_class(recv, *recv_class) {
            ir.guard_class(self, recv, GP::Rdi, *recv_class, deopt);
        }
        if f(ir, store, self, callid, *recv_class, pc) {
            true
        } else {
            std::mem::swap(self, &mut ctx_save);
            ir.restore(ir_save);
            false
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rax: return value: Value
    ///
    fn call_cached(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        fid: FuncId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> Option<Option<AsmEvict>> {
        let CallSiteInfo {
            args, pos_num, dst, ..
        } = store[callid];
        let callsite = &store[callid];
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        match store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(callsite.block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                if recv_class.is_always_frozen() {
                    if dst.is_some() {
                        ir.lit2reg(Value::nil(), GP::Rax);
                    }
                } else {
                    let ivarid = store.classes[recv_class].get_ivarid(ivar_name)?;
                    let is_object_ty = store.classes[recv_class].instance_ty() == ObjKind::OBJECT;
                    ir.push(AsmInst::LoadIVar {
                        ivarid,
                        is_object_ty,
                        min_len: 0,
                    });
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, pos_num);
                assert!(!callsite.kw_may_exists());
                assert!(callsite.block_fid.is_none());
                assert!(callsite.block_arg.is_none());
                let ivarid = store.classes[recv_class].get_ivarid(ivar_name)?;
                self.fetch_for_gpr(ir, args, GP::Rax);
                let is_object_ty = store.classes[recv_class].instance_ty() == ObjKind::OBJECT;
                let using_xmm = self.get_using_xmm();
                ir.push(AsmInst::StoreIVar {
                    ivarid,
                    using_xmm,
                    min_len: 0,
                    is_object_ty,
                });
            }
            FuncKind::Builtin { .. } => {
                let evict = ir.new_evict();
                self.send_cached(ir, store, pc, callid, fid, recv_class, evict);
                return Some(Some(evict));
            }
            FuncKind::ISeq(iseq) => {
                if store[iseq].can_be_inlined
                    && store[iseq].args.is_simple()
                    && callsite.is_simple()
                    && callsite.block_fid.is_none()
                {
                    //let evict = ir.new_evict();
                    let error = ir.new_error(self, pc);
                    let deopt = ir.new_deopt(self, pc);
                    let reg_num = store[iseq].total_reg_num();
                    let stack_offset = (((reg_num + 1) & !1) * 8) as i32;
                    ir.reg_sub(GP::Rsp, stack_offset);

                    ir.stack2reg(callsite.recv, GP::Rax);
                    ir.reg2inline_stack(GP::Rax, SlotId(0));
                    for i in 0..callsite.pos_num {
                        ir.stack2reg(callsite.args + i, GP::Rax);
                        ir.reg2inline_stack(GP::Rax, SlotId(i as u16 + 1));
                    }

                    let iseq = &store[iseq];
                    let mut bbctx = BBContextInner::from_iseq(iseq, self.class_version, recv_class);
                    assert_eq!(1, iseq.bb_info.len());
                    let BasciBlockInfoEntry { begin, end, .. } = iseq.bb_info[BasicBlockId(0)];
                    for bc_pos in begin..=end {
                        bbctx.next_sp = iseq.get_sp(bc_pos);

                        match iseq.trace_ir(store, bc_pos) {
                            TraceIr::InitMethod(..) => {}
                            TraceIr::Nil(slot) => {
                                ir.lit2reg(Value::nil(), GP::Rax);
                                ir.reg2inline_stack(GP::Rax, slot);
                            }
                            TraceIr::Integer(slot, i) => {
                                ir.lit2reg(Value::integer(i as _), GP::Rax);
                                ir.reg2inline_stack(GP::Rax, slot);
                            }
                            TraceIr::Symbol(slot, sym) => {
                                ir.lit2reg(Value::symbol(sym), GP::Rax);
                                ir.reg2inline_stack(GP::Rax, slot);
                            }
                            TraceIr::Literal(slot, val) => {
                                ir.lit2reg(val, GP::Rax);
                                ir.reg2inline_stack(GP::Rax, slot);
                            }
                            TraceIr::Ret(slot) => {
                                ir.inline_stack2reg(slot, GP::Rax);
                                break;
                            }
                            TraceIr::StoreIvar(src, _name, cache) => {
                                if let Some((cached_class, cached_ivarid)) = cache {
                                    ir.inline_stack2reg(SlotId(0), GP::Rdi);
                                    ir.guard_class(
                                        &mut bbctx,
                                        SlotId(0),
                                        GP::Rdi,
                                        cached_class,
                                        deopt,
                                    );
                                    let is_object_ty = store.classes[cached_class].instance_ty()
                                        == ObjKind::OBJECT;
                                    ir.inline_stack2reg(src, GP::Rax);
                                    let using_xmm = self.get_using_xmm();
                                    ir.push(AsmInst::StoreIVar {
                                        ivarid: cached_ivarid,
                                        is_object_ty,
                                        min_len: bbctx.self_ivar_len,
                                        using_xmm,
                                    });
                                    bbctx.self_ivar_len = std::cmp::max(
                                        cached_ivarid.get() as usize + 1,
                                        bbctx.self_ivar_len,
                                    );
                                } else {
                                    unimplemented!()
                                }
                            }
                            TraceIr::LoadIvar(dst, _name, cache) => {
                                if let Some((cached_class, cached_ivarid)) = cache {
                                    self.unlink(ir, dst);
                                    ir.inline_stack2reg(SlotId(0), GP::Rdi);
                                    ir.guard_class(
                                        &mut bbctx,
                                        SlotId(0),
                                        GP::Rdi,
                                        cached_class,
                                        deopt,
                                    );
                                    let is_object_ty = store.classes[cached_class].instance_ty()
                                        == ObjKind::OBJECT;
                                    ir.push(AsmInst::LoadIVar {
                                        ivarid: cached_ivarid,
                                        is_object_ty,
                                        min_len: bbctx.self_ivar_len,
                                    });
                                    ir.reg2inline_stack(GP::Rax, dst);
                                } else {
                                    unimplemented!()
                                }
                            }
                            _ => unreachable!(),
                        }

                        //bbctx.clear(ir);
                        bbctx.sp = bbctx.next_sp;
                    }

                    ir.reg_add(GP::Rsp, stack_offset);
                    ir.handle_error(error);

                    return Some(None);
                } else {
                    let evict = ir.new_evict();
                    self.send_cached(ir, store, pc, callid, fid, recv_class, evict);
                    return Some(Some(evict));
                }
            }
        };
        Some(None)
    }

    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    fn send_cached(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        pc: BytecodePtr,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        evict: AsmEvict,
    ) {
        ir.reg_move(GP::Rdi, GP::R13);
        ir.exec_gc(self.get_register());
        let using_xmm = self.get_using_xmm();
        ir.xmm_save(using_xmm);
        ir.set_arguments(store, self, callid, callee_fid, pc);
        self.unlink(ir, store[callid].dst);
        self.clear(ir);
        let error = ir.new_error(self, pc);
        self.writeback_acc(ir);
        ir.push(AsmInst::SendCached {
            callid,
            callee_fid,
            recv_class,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }

    /*fn send_not_cached(&self, ir: &mut AsmIr, pc: BytecodePtr, callid: CallSiteId) {
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self, pc);
        let evict = ir.new_evict();
        let self_class = self.self_value.class();
        ir.xmm_save(using_xmm);
        ir.push(AsmInst::SendNotCached {
            self_class,
            callid,
            pc,
            error,
            evict,
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
    }*/
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn polymorphic() {
        run_test_with_prelude(
            r##"
        res = []
                
        a = [C1.new, C1.new, C1.new, C1.new, C.new, C.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        a = [C.new, C.new, C.new, C.new, C1.new, C1.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        res
        "##,
            r##"
        class C
          attr_accessor :a
          def initialize
            @a=10
          end
          def f
            @a
          end
        end

        class C1 < C
          attr_accessor :a
          def initialize
            @a=20
          end
        end
        "##,
        );
    }

    #[test]
    fn yield_test() {
        run_test(
            r##"
          def f(x,y)
            yield x,y
          end
          
          res = []
          for i in 0..10
            res << f(i,5) {|x,y| x+y}
            res << f(i,8) {|x,y| x+y}
          end
          res
        "##,
        );
    }

    #[test]
    fn iterator() {
        run_test(
            r##"
        class Array
          def iich
            for i in 0...self.size
              yield(self[i])
            end
          end
        end

        a = []
        [2,5,7,10,2.2,7,9].iich do |x|
          a << x*2
        end
        a
        "##,
        );
    }

    #[test]
    fn attr_accessor() {
        run_test_with_prelude(
            r##"
            x = [C.new, B.new, A.new]
            res = []
            for e in x
                e.a += 1000.0
                e.b += 1000.0
                e.c += 1000.0
                res << e.a
                res << e.b
                res << e.c
            end
            res
            "##,
            r##"
            class C
              def initialize
                @a = 1
                @b = 2
                @c = 3
              end
              attr_accessor :a, :b, :c
            end
            class B < C
              def initialize
                @b = 10
                @c = 20
                @a = 30
              end
              attr_accessor :a, :b, :c
            end
            class A < B
              def initialize
                @c = 100
                @a = 200
                @b = 300
              end
              attr_accessor :a, :b, :c
            end
        "##,
        );
    }

    #[test]
    fn jit_attr_reader() {
        run_test_with_prelude(
            r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
            r###"
        class C
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
        );
        run_test_with_prelude(
            r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
            r###"
        class C < Array
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
        );
    }

    #[test]
    fn deopt_method_recv_class() {
        run_test_error(
            r##"
          class A
            def w
              42
            end
          end
          class B
          end
          a = A.new
          res = []
          for i in 0..10
            if i == 8
              a = B.new
            end
            res << a.w
          end
          res
        "##,
        );
    }

    #[test]
    fn deopt_reader_recv_class() {
        run_test(
            r##"
            class A
                attr_accessor :w
            end
            class B
              def w
                100
              end
            end
            a = A.new
            a.w = 42
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              res << a.w
            end
            res
        "##,
        );
    }

    #[test]
    fn deopt_writer_recv_class() {
        run_test(
            r##"
            class A
              attr_accessor :w
            end
            class B
              attr_reader :w
              def w=(v)
                @w = v * 2
              end
            end
            a = A.new
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              a.w = 42
              res << a.w
            end
            res
        "##,
        );
    }

    #[test]
    fn deopt_reader_class_version() {
        run_test(
            r##"
        class A
          attr_accessor :w
        end
        a = A.new
        a.w = 42
        res = []
        for i in 0..10
          if i == 8
            class A
              def w
                99
              end
            end
          end
          res << a.w
        end
        res
        "##,
        );
    }

    #[test]
    fn deopt_writer_class_version() {
        run_test_once(
            r##"
        class A
          attr_accessor :w
        end
        a = A.new
        res = []
        for i in 0..10
          if i == 8
            class A
              def w=(v)
                @w = v * 2
              end
            end
          end
          a.w = 42
          res << a.w
        end
        res
        "##,
        );
    }

    #[test]
    fn attr_reader_in_different_class() {
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
        "##,
            r##"
            class S
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
        );
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
        "##,
            r##"
            class S < Array
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
        );
    }
}