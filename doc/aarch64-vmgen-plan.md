# Plan: porting monoruby's VM (`vmgen`) to aarch64

This is the continuation plan after **phase 1** (the monoasm A64 instruction
encoder, now on the `aarch` branch of `sisshiki1969/monoasm`). It covers
**phase 2**: making the bytecode VM dispatch loop run on aarch64
(macOS/Apple-Silicon target, validated under qemu-user on this x86 host).
JIT (`jitgen`) is explicitly **out of scope** for this phase.

## Status (updated this session)

**Cross-test environment provisioned and committed** (this is the
foundation phase 2 builds on):

- `.cargo/config.toml` now wires the aarch64 target to the GNU cross linker
  and a `qemu-aarch64` runner, so `cargo build/test --target
  aarch64-unknown-linux-gnu` cross-compiles and runs the resulting binaries
  (including `cargo test` harnesses) under qemu user-mode emulation.
- `bin/setup-aarch64-cross` provisions the host tooling idempotently
  (qemu-user, `gcc-aarch64-linux-gnu`, rust std for the target) and runs a
  compile+qemu smoke test. Safe to re-run at the start of an ephemeral web
  session.
- End-to-end verified: `cargo test -p rubymap --target
  aarch64-unknown-linux-gnu` → **24 doc-tests passed under qemu** on the x86
  host (a pure-Rust workspace crate, no monoasm dependency).

Toolchain in this container: rust target `aarch64-unknown-linux-gnu`
(nightly-2026-05-18), `gcc-aarch64-linux-gnu` 13.3.0, `qemu-aarch64` 8.2.2.
For golden encodings use `llvm-mc` if available.

Phase-1 foundation (the A64 encoder) lives on the monoasm `aarch` branch,
which is **now published** at `sisshiki1969/monoasm` (`refs/heads/aarch`,
`b479de6`) on top of `rc` (`6940e51`). The earlier blocker (aarch branch
unpushed) is resolved.

**Next step to start phase 2 (M0):** repoint `monoasm` /`monoasm_macro` in
`monoruby/Cargo.toml` from `branch = "rc"` to `branch = "aarch"` (or a
pinned rev), so the A64 builder API is available; then build monoruby for
the target behind `cfg(target_arch = "aarch64")`. Note this repoint also
moves the **x86** build onto the `aarch` branch, so confirm `aarch` is a
strict superset of `rc` (it is, per phase 1) before landing it.

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

## The aarch64 DSL now exists (resolved)

The earlier blocker ("no aarch64 `monoasm!{}` DSL") is **gone**. The monoasm
`aarch` branch ships a full `monoasm_arm64!` proc-macro (a proc-macro can't
see `target_arch`, so it's a separate entry point from `monoasm!`). It
covers everything M0 needs — `ldr/str/ldrb/ldp/stp`, `mov/movz/movk`,
`add/sub/cmp/and/orr/eor/mul/sdiv/udiv`, `b/bl/blr/br/ret/cbz/cbnz/tbz`,
`b.<cond>`, `csel/cset`, and the FP ops — and is exercised by the branch's
`tests/arm64/dsl.rs`. So each handler can be a **near line-for-line port**
of its `monoasm!{}` source into `monoasm_arm64!{}` (option A is effectively
free; the builder-method rewrite of option B is unnecessary).

## Empirical build findings (this session)

Cross-compiling `cargo check -p monoruby --target aarch64-unknown-linux-gnu
--features no-jit` got all the way to the `monoruby` crate:

- **Every non-monoruby dependency cross-compiles cleanly**, including the C
  deps (`onigmo-regex`, `libffi-sys`, `ruby-prism{,-sys}`) and the rustls
  stack. The only cross-compile dep blocker was `openssl-sys` (via
  `ruruby-parse`'s `reqwest`), now fixed by switching to rustls.
- **monoruby fails only inside `monoasm!{}`** — `no method named enc_mr …`,
  `cannot find type Imm/Reg/Scale …`. These are x64-encoder internals that
  `monoasm`'s `lib.rs` gates behind `#[cfg(target_arch = "x86_64")]`. So the
  compiler *forces* the seam: the x86 `monoasm!` bodies simply do not exist
  on an aarch64 target and must be `cfg`-excluded there.

`monoasm!{}` block inventory (the work to port/exclude):

| Area | `monoasm!` blocks | Tier |
|------|------:|------|
| `codegen/jitgen/**` | ~347 | JIT — **out of scope**, exclude on aarch64 |
| `vmgen + invoker + wrapper + jit_module + patch + codegen.rs` | ~213 | VM — port to `monoasm_arm64!` |

Note `no-jit` is a **runtime** toggle (`if !cfg!(feature="no-jit")` inside
functions), **not** a module gate — so `jitgen` is compiled even with
`--features no-jit`. To build for aarch64 the entire `jitgen` subsystem must
be `cfg`-excluded (it has 347 x86-only blocks and is JIT-tier only).
`jitgen` is referenced from ~19 files / ~23 sites outside `codegen/jitgen/`
(executor JIT triggers + builtins' inline-JIT registrations); those sites
must be `cfg`-gated to fall back to the VM tier on aarch64.

### Open decision — seam selector: `target_arch` vs a `vm-only` feature

The plan above assumes `#[cfg(target_arch = "aarch64")]`. A cargo feature
(e.g. `vm-only`) that excises `jitgen` is worth considering **instead/also**
because it would let the VM-only configuration build and run **on x86 too**
— enabling incremental testing of the port without qemu and a cheap CI
guard against accidental JIT-coupling. Recommended: gate the JIT subsystem
on `any(target_arch = "aarch64", feature = "vm-only")` so x86 can opt in,
while aarch64 always gets the VM-only build. (Needs sign-off before the
~213-block port + JIT excision lands.)

## Architecture seam (sibling modules)

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

1. Ensure the monoasm `aarch` branch carries the A64 encoder on top of
   `rc`, and verify it (commands below). Note: from a monoruby-scoped
   session monoasm cannot be pushed (the git proxy and GitHub MCP scope
   both allow monoruby only, and plain `git push` has no credentials);
   land monoasm changes from a monoasm-scoped session via the GitHub MCP
   `push_files`/`create_branch` tools (server-side commit avoids the local
   signing constraint).
2. In monoruby `Cargo.toml`, repoint both `monoasm` and `monoasm_macro` to
   `branch = "aarch"` (or a pinned rev) for the duration of phase 2.
3. Cross runner is **already committed**: `.cargo/config.toml` carries the
   `[target.aarch64-unknown-linux-gnu]` linker+runner, and
   `bin/setup-aarch64-cross` installs the host tooling. No manual recreation
   needed — just run the script once per fresh container.

## Dev/verify commands (this container)

```sh
# one-time per container: install qemu + cross gcc + rust std, smoke-test
bin/setup-aarch64-cross

# sanity check the cross-test pipeline on a pure-Rust workspace crate
cargo test -p rubymap --target aarch64-unknown-linux-gnu        # runs under qemu

# monoasm foundation (requires monoasm checked out as a path/workspace dep)
cargo +nightly-2026-05-18 test -p monoasm --test arm64_encoding
cargo +nightly-2026-05-18 test -p monoasm --target aarch64-unknown-linux-gnu --test arm64_exec
cargo +nightly-2026-05-18 test -p monoasm                      # x86 regression

# monoruby aarch64 build (after Cargo.toml repoint to monoasm `aarch`):
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
