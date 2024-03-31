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
            callid, recv, args, pos_num, block_fid, block_arg, using_xmm, error,
        );
    });
    ir.reg2acc(bb, GP::Rax, dst);
}

const CACHE_SIZE: usize = 2;

#[repr(C)]
struct CacheEntry {
    method: Option<IdentId>,
    fid: FuncId,
    counter: usize,
}

impl std::fmt::Debug for CacheEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(method) = self.method {
            write!(
                f,
                "method: {:?}, fid: {:?}, counter: {}",
                method, self.fid, self.counter
            )
        } else {
            write!(f, "method: None")
        }
    }
}

#[repr(C)]
#[derive(Debug)]
struct Cache([CacheEntry; CACHE_SIZE]);

impl Cache {
    fn search(&mut self, globals: &mut Globals, recv: Value, method: IdentId) -> Result<FuncId> {
        let mut min_i = 0;
        let mut min_count = self.0[min_i].counter;
        for (i, entry) in self.0.iter_mut().enumerate() {
            match entry.method {
                Some(cached_method) if cached_method == method => {
                    let fid = entry.fid;
                    entry.counter += 1;
                    if i != min_i && entry.counter > min_count {
                        self.0.swap(min_i, i);
                    }
                    return Ok(fid);
                }
                Some(_) => {
                    if entry.counter < min_count {
                        min_count = entry.counter;
                        min_i = i;
                    }
                }
                None => {
                    min_i = i;
                    break;
                }
            }
        }
        //eprintln!("{:#?}", self);
        let fid = globals.find_method(recv, method, false)?;
        self.0[min_i].method = Some(method);
        self.0[min_i].fid = fid;
        self.0[min_i].counter = 1;
        Ok(fid)
    }

    fn clear(&mut self) {
        for entry in self.0.iter_mut() {
            entry.method = None;
        }
    }
}
