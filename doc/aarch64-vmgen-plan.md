# Plan: porting monoruby's VM (`vmgen`) to aarch64

This is the continuation plan after **phase 1** (the monoasm A64 instruction
encoder, see `HANDOFF-aarch64.md` and `aarch64-monoasm.patch`). It covers
**phase 2**: making the bytecode VM dispatch loop run on aarch64
(macOS/Apple-Silicon target, validated under qemu-user on this x86 host).
JIT (`jitgen`) is explicitly **out of scope** for this phase.

## Status (updated this session)

Phase-1 foundation **re-verified locally** on top of monoasm `rc`:

- `aarch64-monoasm.patch` applies cleanly to a fresh clone of
  `sisshiki1969/monoasm` at branch `rc` (`6940e51`).
- `cargo test -p monoasm --test arm64_encoding` → **15 passed** (host).
- `cargo test -p monoasm --target aarch64-unknown-linux-gnu --test
  arm64_exec` → **9 passed** under qemu.
- `cargo test -p monoasm` (x86 host) → **all green, no regression**.

Toolchain installed in this container: rust target
`aarch64-unknown-linux-gnu` (nightly-2026-05-18), `gcc-aarch64-linux-gnu`
13.3.0, `qemu-aarch64` 8.2.2, `llvm-mc` (for golden encodings).

**Still blocked for landing:** the monoasm `aarch` branch is not pushed
(`origin` only has `main`/`master`/`rc`), and monoasm is not in this
session's GitHub MCP scope. monoruby's `Cargo.toml` therefore still points
at monoasm `rc`, which has no A64 API. Phase 2 cannot build until monoasm
`aarch` is published and `Cargo.toml` is repointed (see "Prerequisite to
land" below).

## Map of what must be ported (VM-only)

The whole monoruby engine emits x86-64 machine code; there is **no**
portable interpreter. Even with `no-jit`, the base VM tier is generated
code, so all of the following must gain an aarch64 path. `monoasm!{}`
block counts (a rough size proxy):

| File | `monoasm!` blocks | Role (VM-only relevance) |
|------|------:|--------------------------|
| `codegen/vmgen.rs` (+`vmgen/` submodules) | ~110 | bytecode opcode handlers + dispatch helpers |
| `codegen/invoker.rs` | 34 | method/block/fiber entry trampolines |
| `codegen/jit_module.rs` | 12 | low-level `JitMemory` setup (entry/exit, prologue) |
| `codegen.rs` | (dispatch + frame helpers) | `fetch_and_dispatch`, frame push/pop, `set_lfp` |
| `codegen/wrapper.rs` | 11 | `extern "C"` ↔ Ruby call wrappers for builtins |
| `codegen/compiler.rs` | 8 | compile entry, loop-compile patch points |
| `codegen/patch.rs` | 2 | JIT patch points (stub for VM-only) |

`jitgen/**` (the entire `monoasm!` cluster in `jitgen/asmir/compile*`,
`guard.rs`, etc.) is **deferred** — it only runs in the JIT tier.

### Dispatch core (the heart — port first)

`codegen.rs::fetch_and_dispatch` (x86):

```
movq   r15, (self.dispatch.as_ptr());   // table base (immediate)
movzxb rax, [r13 + OPECODE];            // opcode byte at pc
addq   r13, 16;                         // advance pc one instr (16 bytes)
jmp    [r15 + rax * 8];                 // indirect jump via table
```

aarch64 equivalent (all primitives already exist in `arm64.rs`):

```
mov_imm  Xtbl, dispatch_ptr            // movz/movk sequence
ldrb     Wop,  [Xpc, #OPECODE]         // ldrb (rt, rn, off)
add_imm  Xpc,  Xpc, #16, 0            // add immediate
ldr_reg  Xtgt, Xtbl, Xop, scaled=true  // ldr Xtgt,[Xtbl, Xop, lsl #3]
br       Xtgt
```

### Slot access pattern (pervasive)

`vm_get_slot_value(reg)` / `vm_get_slot_addr(reg)` (vmgen.rs:414/431):

```
negq R(r);
lea/movq R(r), [r14 + R(r)*8 - LFP_SELF]
```

aarch64: negate, then `add Xr, x_lfp, Xr, lsl #3` and `sub_imm ..,
#LFP_SELF` (or fold into an `ldur`/`ldr` with the computed base). The
existing `ldr_reg(..., scaled)` covers the `*8` index; the `-LFP_SELF`
constant needs a separate `sub_imm` since A64 has no base+index+disp form.

### Runtime calls (the bulk of handlers)

Most opcodes just marshal args and call a Rust `runtime::*` fn:

```
movq rdi, rbx; movq rsi, r12; movl rdx, [r13-16];
movq rax, (runtime::foo); call rax;
```

aarch64 (AAPCS64): args in `x0..x7`, `mov_imm Xt, addr; blr Xt`. This is
the single most common shape and is highly mechanical to port.

## Register mapping (x86-64 → aarch64)

monoruby's global registers (from `CLAUDE.md`) → callee-saved A64 regs:

| Purpose | x86 | aarch64 (proposed) |
|---------|-----|--------------------|
| `&mut Executor` | `rbx` | `x19` |
| `&mut Globals`  | `r12` | `x20` |
| Program counter | `r13` | `x21` |
| Local frame ptr (LFP) | `r14` | `x22` |
| Accumulator | `r15` | `x23` |
| scratch / args | rax,rdi,rsi,rdx,rcx,r8,r9 | x0..x7, x9..x15 |

`x19..x23` are callee-saved under AAPCS64 (must be saved in the VM entry
prologue, like the x86 `pushq rbp`). FP `xmm0/xmm1` scratch → `d0/d1`.
Frame offsets (`LFP_OUTER +0 … LFP_ARG0 +32`) are unchanged.

## The core problem: no aarch64 `monoasm!{}` DSL

Phase 1 added **builder methods** (`jit.add(X0,X1,X2)`), not a
`monoasm!{}`-style macro. Every handler today is written in the x86 macro
DSL. Two ways forward:

- **(A) aarch64 DSL macro** in `monoasm_macro` — large up-front cost, lives
  in the (out-of-scope) monoasm repo, but makes each handler a near
  line-for-line port. Best long-term ergonomics.
- **(B) builder-method rewrite** behind `cfg(target_arch)` — no macro work;
  verbose but mechanical; can start immediately once monoasm `aarch` is a
  dependency.

**Recommendation:** start with **(B)** for the dispatch core + the first
opcode family (proves the whole pipeline end-to-end under qemu fastest),
then decide whether the DSL (A) pays for itself before grinding through the
~150 remaining handlers. Either way, introduce an **arch-abstraction
seam** so handler bodies select x86 vs aarch64 via `cfg(target_arch)` at a
single, well-defined layer (the `Codegen` impl), keeping the x86 path
byte-for-byte unchanged.

## Recommended architecture seam

1. Keep `Codegen`/`JitMemory` shared. Add `#[cfg(target_arch =
   "aarch64")]` sibling modules: `codegen/vmgen_a64.rs`, etc., OR
   per-method `cfg` arms. Prefer **sibling modules** so the x86 source is
   untouched and diffs stay reviewable.
2. Define the A64 global-register constants once
   (`const EXEC: GReg = X19; …`) mirroring the x86 register choices.
3. Provide small A64 helper methods mirroring the x86 ones by name
   (`fetch_and_dispatch`, `fetch3`, `vm_get_slot_value`,
   `vm_store_r15`, `call_runtime(fn_ptr)`), so handler bodies read the
   same on both arches.
4. `construct_vm` (and `invoker`, `wrapper`, …) gets a `cfg`-selected
   aarch64 body that fills the same `self.dispatch[..]` table and sets the
   same entry labels.

## Milestones (each validated under qemu before moving on)

- **M0 — "hello dispatch"**: aarch64 VM entry prologue (save x19..x23, set
  up globals), `fetch_and_dispatch`, and just enough handlers to run a
  trivial program: `Integer literal`, `Mov`, `Ret`, plus `Nil/Immediate`.
  Goal: `puts`-free `42` returns 42 from `run_test_once`-style harness
  under qemu. This exercises dispatch, slots, frame entry/exit, and a
  runtime call.
- **M1 — integer arithmetic**: `add/sub/mul/div_rr` (+ fixnum guards,
  `div_by_zero`), comparisons (`vm_*_opt_rr`), conditional branches
  (`condbr`/`condnotbr`/`br_inst`). Fibonacci-class programs run.
- **M2 — method calls**: `vm_send`/`vm_send_simple`, `invoker.rs`
  trampolines, `wrapper.rs`, frame push/pop. Builtin + Ruby-defined
  method dispatch works.
- **M3 — objects & control**: ivars, constants, gvars/cvars/dvars, arrays,
  hashes, ranges, blocks, exceptions (`raise`/`ensure`/`retry`/`redo`).
- **M4 — full VM parity**: remaining opcodes; run the monoruby test suite
  with `--features no-jit` under qemu and diff against CRuby.
- **M5 — real macOS hardware** (no qemu): `MAP_JIT` +
  `pthread_jit_write_protect_np`, icache flush after emit, macOS errno
  table + platform string, dylib/loader paths. Then phase 3 (JIT).

## Prerequisite to land (do this first in a monoasm-scoped session)

1. Clone `sisshiki1969/monoasm`, `git checkout -b aarch origin/rc`,
   `git apply aarch64-monoasm.patch`, verify (commands below), then push
   `aarch` via the GitHub **MCP** `push_files`/`create_branch` tools
   (server-side commit avoids the local signing constraint noted in the
   handoff). Plain `git push` has no credentials here.
2. In monoruby `Cargo.toml`, repoint both `monoasm` and `monoasm_macro` to
   `branch = "aarch"` (or a pinned rev) for the duration of phase 2.
3. Add `monoruby/.cargo/config.toml` cross runner (see below). Note this
   container's repo ignores `/.*`, so it is not committed; recreate it.

## Dev/verify commands (this container)

```sh
# monoasm foundation (already passing)
cargo +nightly-2026-05-18 test -p monoasm --test arm64_encoding
cargo +nightly-2026-05-18 test -p monoasm --target aarch64-unknown-linux-gnu --test arm64_exec
cargo +nightly-2026-05-18 test -p monoasm                      # x86 regression

# monoruby aarch64 build (after Cargo.toml repoint), under qemu:
#   monoruby/.cargo/config.toml:
#     [target.aarch64-unknown-linux-gnu]
#     linker = "aarch64-linux-gnu-gcc"
#     runner = "qemu-aarch64 -L /usr/aarch64-linux-gnu"
cargo +nightly-2026-05-18 build --target aarch64-unknown-linux-gnu --features no-jit
```

Golden encodings for any new A64 instruction: `llvm-mc --triple=aarch64
--show-encoding` (per-instruction) and `llvm-mc -filetype=obj` +
`llvm-objdump -d` (resolved branch displacements). Verify before trusting.

## Risks / notes

- **macOS calling convention & W^X** differ from qemu-linux; M0–M4 prove
  correctness of *encodings/logic*, M5 handles the platform layer.
- A64 has no base+index+displacement addressing — the x86 `[r14 + r*8 -
  LFP_SELF]` idiom becomes 2–3 instructions; bake it into shared helpers
  so it is written once.
- A64 immediates are limited (12-bit add/sub, bitmask-encoded logical).
  Large constants (function pointers, dispatch table base) need
  `movz/movk` sequences — `mov_imm` already does this.
- Keep the x86 path **untouched**; gate all aarch64 code behind
  `cfg(target_arch = "aarch64")` so CI on x86 is unaffected.
