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
    let using_xmm = bb.get_using_xmm();
    let error = ir.new_error(bb, pc);
    ir.inline(move |gen, labels| {
        let error = labels[error];
        gen.object_send_inline(
            callid, recv, args, pos_num, block_fid, block_arg, using_xmm, error, true,
        );
    });
    ir.reg2acc(bb, GP::Rax, dst);
}

pub(crate) fn object_send_splat(
    ir: &mut AsmIr,
    store: &Store,
    bb: &mut BBContext,
    callid: CallSiteId,
    pc: BcPc,
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
    let using_xmm = bb.get_using_xmm();
    let error = ir.new_error(bb, pc);
    ir.inline(move |gen, labels| {
        let error = labels[error];
        gen.object_send_inline(
            callid, recv, args, pos_num, block_fid, block_arg, using_xmm, error, false,
        );
    });
    ir.reg2acc(bb, GP::Rax, dst);
}
