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
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
) {
    let CallSiteInfo {
        recv,
        dst,
        args,
        pos_num,
        block_fid,
        ref splat_pos,
        ..
    } = *callsite;
    let splatter = !splat_pos.is_empty();
    if splatter {
        assert_eq!(splat_pos, &[0]);
        assert_eq!(pos_num, 1);
    }
    ir.write_back_callargs(bb, callsite);
    ir.unlink(bb, dst);
    let using = bb.get_using_xmm();
    let bh = match block_fid {
        None => 0,
        Some(func_id) => BlockHandler::from_caller(func_id).id(),
    };
    let error = ir.new_error(bb, pc);
    ir.inline(move |gen, labels| {
        let cache = gen
            .jit
            .bytes(std::mem::size_of::<CacheEntry>() * CACHE_SIZE);
        let version = gen.jit.const_i32(-1);
        let error = labels[error];
        gen.xmm_save(using);
        if !splatter {
            monoasm! {&mut gen.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, [r14 - (conv(recv))];
                lea  rcx, [r14 - (conv(args))];
                movq r8, (pos_num);
                movq r9, (bh);
                lea  rax, [rip + version];
                pushq rax;
                lea  rax, [rip + cache];
                pushq rax;
                movq rax, (call_send_wrapper);
                call rax;
                addq rsp, 16;
            }
        } else {
            monoasm! {&mut gen.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, [r14 - (conv(recv))];
                movq rcx, [r14 - (conv(args))];
                movq r8, (bh);
                lea  r9, [rip + cache];
                subq rsp, 8;
                lea  rax, [rip + version];
                pushq rax;
                movq rax, (call_send_wrapper2);
                call rax;
                addq rsp, 16;
            }
        }
        gen.xmm_restore(using);
        gen.handle_error(error);
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

extern "C" fn call_send_wrapper(
    vm: &mut Executor,        // rdi
    globals: &mut Globals,    // rsi
    recv: Value,              // rdx
    args: Arg,                // rcx
    len: usize,               // r8
    bh: Option<BlockHandler>, // r9
    cache: &mut Cache,
    version: &mut u32,
) -> Option<Value> {
    fn call_send(
        globals: &mut Globals,
        recv: Value,
        args: Arg,
        len: usize,
        cache: &mut Cache,
    ) -> Result<FuncId> {
        if len < 1 {
            return Err(MonorubyErr::wrong_number_of_arg_min(len, 1));
        }
        let method = args[0].unwrap().expect_symbol_or_string()?;
        cache.search(globals, recv, method)
    }

    if globals.class_version() != *version {
        cache.clear();
        *version = globals.class_version();
    }
    let fid = match call_send(globals, recv, args, len, cache) {
        Ok(res) => res,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    // Currently, we don't support calling with block in inlined method.
    let bh = bh.map(|bh| bh.delegate());
    (globals.codegen.method_invoker2)(vm, globals, fid, recv, args + 1, len - 1, bh)
}

extern "C" fn call_send_wrapper2(
    vm: &mut Executor,        // rdi
    globals: &mut Globals,    // rsi
    recv: Value,              // rdx
    arg: Value,               // rcx
    bh: Option<BlockHandler>, // r8
    cache: &mut Cache,        // r9
    version: &mut u32,
) -> Option<Value> {
    fn call_send(
        globals: &mut Globals,
        recv: Value,
        arg: Value,
        cache: &mut Cache,
    ) -> Result<FuncId> {
        let len = arg.as_array().len();
        if len < 1 {
            return Err(MonorubyErr::wrong_number_of_arg_min(len, 1));
        }
        let method = arg.as_array()[0].expect_symbol_or_string()?;
        cache.search(globals, recv, method)
    }

    if globals.class_version() != *version {
        cache.clear();
        *version = globals.class_version();
    }
    let fid = match call_send(globals, recv, arg, cache) {
        Ok(res) => res,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    vm.invoke_func(globals, fid, recv, &arg.as_array()[1..], bh)
}
