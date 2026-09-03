# Stack layout for the bytecode interpreter / JIT'ed code

The interpreter and JIT'ed code share one frame layout: both go through the
same `set_lfp` / `set_method_outer` / `set_block_outer` primitives in
`codegen/arch/<arch>/jit_module.rs`, so a single picture covers a VM frame, a
JIT frame and a native wrapper alike. The offsets below are the constants in
`executor.rs`; when this document and that file disagree, the file is right.

## Slot offsets

Each Ruby-level call occupies three contiguous regions, stack growing down.
`cfp` and `lfp` are two pointers into it: the control frame is addressed at
**positive** offsets from `cfp`, the local frame at **negative** offsets from
`lfp`.

| Slot | Address | Constant |
| --- | --- | --- |
| pad / chain-deopt continuation word | `cfp + 0x20` | — |
| caller pc | `cfp + 0x18` | — |
| return address | `cfp + 0x10` | — |
| saved `rbp` | `cfp + 0x08` | `BP_CFP` (`bp == cfp + BP_CFP`) |
| prev cfp | `cfp + 0x00` | — |
| lfp | `cfp - 0x08` | `CFP_LFP` |
| outer | `lfp - 0x00` | `LFP_OUTER` = 0 |
| meta | `lfp - 0x08` | `LFP_META` = 8 |
| svar | `lfp - 0x10` | `LFP_SVAR` = 16 |
| block | `lfp - 0x18` | `LFP_BLOCK` = 24 |
| self | `lfp - 0x20` | `LFP_SELF` = 32 |
| arg0 | `lfp - 0x28` | `LFP_ARG0` = 40 |

The local frame starts at `cfp - 0x10`, one word below the `lfp` slot itself.
From a caller's `rsp` at the call, `RSP_CFP` (24) and `RSP_LOCAL_FRAME` (40)
name the same two points.

## Just after the prologue

```text
             +-------------+----------------------
   cfp+0x20  |     pad     |
             +-------------+
   cfp+0x18  |  caller pc  |
             +-------------+  continuation frame
   cfp+0x10  | return addr |
             +-------------+
   cfp+0x08  |  saved rbp  | <- rbp
             +-------------+----------------------
   cfp+0x00  |  prev cfp   | <- cfp
             +-------------+  control frame
   cfp-0x08  |     lfp     |
             +-------------+----------------------
       -0x00 |    outer    | <- r14 (lfp)
             +-------------+
       -0x08 |    meta     |
             +-------------+
       -0x10 |    svar     |
             +-------------+  local frame
       -0x18 |    block    |
             +-------------+
       -0x20 |    self     |
             +-------------+
       -0x28 |    arg0     |
             +-------------+
             |      :      |
             +-------------+
             |   arg(n-1)  |
             +-------------+----------------------
             |             | <- rsp
             +-------------+
             |      :      |
```

## Just before the call

The caller builds the callee's frame below its own `rsp` and then `call`s;
the two words above `rsp` are the cont-frame extension it reserved with
`sub rsp, 0x10`, and `call` / the callee prologue push the return address and
the saved `rbp` into the two words below it.

```text
             +-------------+----------------------
       +0x08 |     pad     |
             +-------------+  reserved by the caller
       +0x00 |  caller pc  | <- rsp
             +-------------+----------------------
       -0x08 | return addr |    pushed by `call`
             +-------------+
       -0x10 |  saved rbp  |    pushed by the callee prologue
             +-------------+----------------------
       -0x18 |  prev cfp   | <- cfp        (RSP_CFP)
             +-------------+  control frame
       -0x20 |     lfp     |
             +-------------+----------------------
       -0x28 |    outer    | <- r14 (lfp)  (RSP_LOCAL_FRAME)
             +-------------+
       -0x30 |    meta     |
             +-------------+
       -0x38 |    svar     |
             +-------------+  local frame
       -0x40 |    block    |
             +-------------+
       -0x48 |    self     |
             +-------------+
       -0x50 |    arg0     |
             +-------------+
             |      :      |
             +-------------+
             |   arg(n-1)  |
             +-------------+----------------------
             |      :      |
```

## What the header slots hold

- **outer** — the lexically enclosing frame's lfp for a block, `0` for a
  method-introducing frame. `$~` resolution and outer-local access walk this
  chain.
- **meta** — one packed 8-byte word: `FuncId` (4 bytes), `reg_num` (2), the
  argument `mode` byte, and a `kind` byte carrying on-stack/on-heap,
  simple-arity, invalidated, native, block-style and related flags.
  `LFP_REGNUM` and `LFP_FUNCID` address fields *inside* this word; they are
  not separate slots.
- **svar** — frame-local special variables, the counterpart of CRuby's
  `vm_svar`. `0` is the lazy-allocation sentinel ("nothing set in this scope
  yet"); otherwise a 2-element `Array` container `[$~, $_]`. Only a
  method-introducing frame owns one — blocks walk the outer chain to the LEP.
- **block** — the block passed to this call, if any.
- **self** — the receiver, and register slot `%0`. Locals follow it
  contiguously, so `Lfp::register_ptr` addresses slot `i` as
  `lfp - (LFP_SELF + 8 * i)`: `%0` is `self`, `%1` is `arg0`, and so on —
  which is why the bytecode dumps show a method's first parameter as `%1`.

## Continuation frame

The four words above `cfp` are written by different parties:

- **saved rbp** and **return address** by `call` and the callee prologue.
  Every frame — VM, JIT or native wrapper — establishes `bp == cfp + BP_CFP`
  in its prologue, so `Cfp::frame_bp` can recover the register's value from
  the CFP alone.
- **caller pc** by the caller just before dispatching (the VM's
  `pushq r13`, the JIT's equivalent store, or a zero sentinel from an
  invoker). Not every dispatch path writes it, so consumers must
  range-validate it against the caller frame's bytecode span before trusting
  it. This is what powers lazy backtraces, `Kernel#caller`, and `super`
  resolution (see [`super_resolution.md`](super_resolution.md)).
- **pad** is reserved by every caller and read by nothing on the normal
  return path. Chain deopt reuses it as the converted call's per-site
  continuation word (see [`chain_deopt.md`](chain_deopt.md) §9.3).

## ABI of the interpreter and JIT'ed code

Global registers, callee-saved on both architectures:

| Role | x86-64 | aarch64 |
| --- | --- | --- |
| `&mut Executor` (`[rbx]` points to cfp) | `rbx` | `x19` |
| `&mut Globals` | `r12` | `x20` |
| program counter | `r13` | `x21` |
| local frame pointer (lfp) | `r14` | `x22` |
| accumulator | `r15` | `x23` |

The accumulator is a **VM-tier** register. JIT'ed code does not keep a fixed
accumulator: `GP_ALLOC_POOL` is empty, and the local allocator in
`jitgen/gp_alloc.rs` assigns general-purpose registers per basic block
instead, so a compiled body's values live in whatever caller-saved register
it picked.

## Negative result: moving META / SVAR to the callee

The caller writes the whole header, including the two words that depend only
on the callee — `meta` (`store[fid].meta()`) and the `svar` zero sentinel.
They are the same at every call site of a given method, so they look like an
obvious thing to write once in the callee instead: 24 bytes off each of a
program's call sites, in exchange for 17 bytes added once per method.

This was implemented and measured, and it is **slower**. The x86-64 form
was: `Codegen::init_func` writes both words at `rbp - RBP_LOCAL_FRAME` (the
same addresses the caller reached at `rsp - RSP_LOCAL_FRAME`), and a call
site drops them exactly when it dispatches straight to the callee's compiled
entry — the same `get_jit_entry` lookup `AsmInst::Call` performs, resolved in
the same lowering so the two cannot disagree. Block-style callees were
excluded: `define_method` re-tags a block's `LFP_META` with
`Meta::PROC_METHOD_MASK` at call time, and a body rewriting the word from its
own compile-time copy would drop that bit.

Measured against the same tree, interleaved runs (x86-64, release):

| benchmark | delta |
|---|---|
| fib | +4% |
| 30k_methods | −5 to −10% |
| tarai | −5.8% |
| bedcov | −5.5% |
| sudoku | −3.6% |
| qsort | −3.3% |
| 30k_ifelse | −3% |
| aobench | −2.5% |
| nbody, mandelbrot, bf, 30k_variables | flat |

Two reasons, and both generalise:

* **The size win is much smaller than the call-site arithmetic suggests.**
  A `movabs` for the 64-bit `Meta` is 10 of the 17 bytes added per method,
  and these programs have nearly as many methods as call sites (30k_methods:
  ~31k sites over 30k methods). The net was ~6% of the JIT code, not the
  ~19% the caller-side saving alone implies. The trade only pays where call
  sites outnumber methods by a wide margin — which is what `fib` is, and why
  `fib` is the one benchmark that improved.
* **Placement matters more than count.** The stores moved from the caller,
  where they issue well ahead of the `call` and drain from the store buffer
  while the caller finishes its own work, to the head of the callee, where
  they are serialized behind `push rbp` / `mov rbp, rsp` / `sub rsp` and
  compete with the entry poll's load — on a taken branch, at the exact point
  the front end is refilling.

So the header stays where it is. A future attempt at trimming the call
sequence should target words that can be *dropped* rather than moved, or
move work off the callee's entry rather than onto it.
