# Inline function

## precondition

- rdi: receiver: Value

## output

- accumulator(r15): result: Value

```rust
  TraceIr::InlineCall {
      inline_id, callid, ..
  } => {
      let inline_gen = store.get_inline_info(inline_id).0;
      let recv = store[callid].recv;
      self.ir.fetch_to_reg(bb, recv, GP::Rdi);
      let (deopt, error) = self.ir.new_deopt_error(bb, pc);
      let using_xmm = bb.get_using_xmm();
      self.ir.guard_class_version(pc, using_xmm, deopt, error);
      inline_gen(&mut self.ir, store, bb, &store[callid], pc);
  }
```

## callee function example

- You must take arguments directly from the caller's stack using callsite information (`CallSiteInfo`).
- An 'inlinable' method call must have 'simple' call site, which means no keyword arguments, no splat arguments, and no block argument.
- use `AsmIr.inline(&mut self, f: impl FnOnce(&mut Codegen, &SideExitLabels))` to embed a unique code.
- use `AsmIr.reg2acc(&mut self, bb: &mut BBContext, src: GP, dst: impl Into<Option<SlotId>>)` for moving the result (in `src: GP`) to the accumulator.

```rust
fn object_send(
    ir: &mut AsmIr,
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    _pc: BcPc,
) {
    let CallSiteInfo {
        recv,
        dst,
        args,
        pos_num,
        block_fid,
        ..
    } = *callsite;
    ir.write_back_callargs(bb, callsite);
    ir.unlink(bb, dst);
    let using = bb.get_using_xmm();
    let bh = match block_fid {
        None => 0,
        Some(func_id) => BlockHandler::from(func_id).0.id(),
    };
    ir.inline(move |gen, _| {
        let cache = gen.jit.bytes(std::mem::size_of::<Cache>() * CACHE_SIZE);
        gen.xmm_save(using);
        monoasm! {&mut gen.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (conv(recv))];
            lea  rcx, [r14 - (conv(args))];
            movq r8, (pos_num);
            movq r9, (bh);
            subq rsp, 8;
            lea  rax, [rip + cache];
            pushq rax;
            movq rax, (call_send_wrapper);
            call rax;
            addq rsp, 16;
        }
        gen.xmm_restore(using);
    });
    ir.reg2acc(bb, GP::Rax, dst);
}
```
