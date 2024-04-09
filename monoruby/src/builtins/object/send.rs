use super::*;

///
/// Object#send
///
/// - send(name, *args) -> object
/// - send(name, *args) { .... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/send.html]
#[monoruby_builtin]
pub(crate) fn send(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let ary = arg0.as_array();
    if ary.len() < 1 {
        return Err(MonorubyErr::wrong_number_of_arg_min(ary.len(), 1));
    }
    let method = ary[0].expect_symbol_or_string()?;
    vm.invoke_method_inner(globals, method, lfp.self_val(), &ary[1..], lfp.block())
}

pub(crate) fn object_send(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BcPc,
) {
    object_send_inner(ir, store, bb, callid, pc, true)
}

pub(crate) fn object_send_splat(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BcPc,
) {
    object_send_inner(ir, store, bb, callid, pc, false)
}

fn object_send_inner(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BcPc,
    no_splat: bool,
) {
    let callsite = &store[callid];
    let CallSiteInfo {
        recv,
        dst,
        args,
        pos_num,
        block_fid,
        block_arg,
        ..
    } = *callsite;
    ir.write_back_callargs(bb, callsite);
    ir.unlink(bb, dst);
    ir.writeback_acc(bb);
    let using_xmm = bb.get_using_xmm();
    let error = ir.new_error(bb, pc);
    ir.inline(move |gen, labels| {
        let error = labels[error];
        gen.object_send_inline(
            callid, recv, args, pos_num, block_fid, block_arg, using_xmm, error, no_splat,
        );
    });
    ir.reg2acc(bb, GP::Rax, dst);
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn object_send() {
        run_test_with_prelude(
            r##"
        o = C.new
        [o.send(:foo), o.send("foo"), o.send(:bar, 20), o.send(*[:foo]), o.send(*["bar", 100])]
        "##,
            r##"
        class C
            def foo
                1
            end
            def bar(x)
                x
            end
        end
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(200, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(:foo, 100)
        "##,
        );
        run_test_error(
            r##"
        class C
            def foo
                1
            end
        end
        C.new.send(*[])
        "##,
        );
        run_test_error(
            r##"
          class C
            def b(x, y, z)
              puts "x=#{[x, y, z]}"
            end
          end
          
          o = C.new
          30.times do |x|
            if x == 28 then
              eval('class C; def b(x); puts "x=#{x}" ;end; end')
            end
            o.send(:b, x, 2, 3)
          end 
            "##,
        );
    }
}
