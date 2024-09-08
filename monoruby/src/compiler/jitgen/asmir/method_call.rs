use super::*;

impl AsmIr {
    pub(in crate::compiler::jitgen) fn gen_call(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        fid: FuncId,
        callid: CallSiteId,
        pc: BytecodePtr,
    ) -> Option<()> {
        let CallSiteInfo { dst, recv, .. } = store[callid];
        if recv.is_self() && bb.self_value.class() != pc.cached_class1().unwrap() {
            // the inline method cache is invalid because the receiver class is not matched.
            self.write_back_locals(bb);
            self.write_back_callargs(bb, &store[callid]);
            self.unlink(bb, dst);
            self.writeback_acc(bb);
            self.send_not_cached(bb, pc, callid);
            self.rax2acc(bb, dst);
        } else {
            // We must write back and unlink all local vars when they are possibly accessed from inner blocks.
            if store[callid].block_fid.is_some() || store[fid].meta().is_eval() {
                self.write_back_locals(bb);
            }
            self.fetch_to_reg(bb, recv, GP::Rdi);
            let (deopt, error) = self.new_deopt_error(bb, pc);
            let using_xmm = bb.get_using_xmm();
            self.guard_version(pc, using_xmm, deopt, error);
            let cached_class = pc.cached_class1().unwrap();
            // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
            // Thus, we can omit a class guard.
            if !recv.is_self() && !bb.is_class(recv, cached_class) {
                self.guard_class(bb, recv, GP::Rdi, cached_class, deopt);
            }
            let evict = self.gen_call_cached(store, bb, callid, fid, pc)?;
            self.rax2acc(bb, dst);
            if let Some(evict) = evict {
                self.inst.push(AsmInst::ImmediateEvict { evict });
                self[evict] = SideExit::Evict(Some((pc + 2, bb.get_write_back())));
            }
        }

        Some(())
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    fn gen_call_cached(
        &mut self,
        store: &Store,
        bb: &mut BBContext,
        callid: CallSiteId,
        fid: FuncId,
        pc: BytecodePtr,
    ) -> Option<Option<AsmEvict>> {
        let CallSiteInfo {
            args, pos_num, dst, ..
        } = store[callid];
        // in this point, the receiver's class is guaranteed to be identical to cached_class.
        let recv_class = pc.cached_class1().unwrap();
        match store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, pos_num);
                assert!(!store[callid].kw_may_exists());
                assert!(store[callid].block_fid.is_none());
                assert!(store[callid].block_arg.is_none());
                if recv_class.is_always_frozen() {
                    if dst.is_some() {
                        self.lit2reg(Value::nil(), GP::Rax);
                    }
                } else {
                    let ivar_id = store.classes[recv_class].get_ivarid(ivar_name)?;
                    self.attr_reader(ivar_id);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, pos_num);
                assert!(!store[callid].kw_may_exists());
                assert!(store[callid].block_fid.is_none());
                assert!(store[callid].block_arg.is_none());
                let ivar_id = store.classes[recv_class].get_ivarid(ivar_name)?;
                self.fetch_to_reg(bb, args, GP::Rdx);
                self.attr_writer(bb, pc, ivar_id);
            }
            FuncKind::Builtin { .. } => {
                let evict = self.new_evict();
                self.send_cached(store, bb, pc, callid, fid, recv_class, true, evict);
                return Some(Some(evict));
            }
            FuncKind::ISeq(_) => {
                let evict = self.new_evict();
                self.send_cached(store, bb, pc, callid, fid, recv_class, false, evict);
                return Some(Some(evict));
            }
        };
        Some(None)
    }
}

#[cfg(test)]
mod test {
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
