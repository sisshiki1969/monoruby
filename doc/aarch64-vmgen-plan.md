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

**Done this session toward M0:**

- `monoasm`/`monoasm_macro` repointed to `branch = "aarch"`; x86 build
  re-verified clean (the `aarch` branch is a strict superset of `rc`).
- `ruruby-parse` reqwest → rustls, removing the `openssl-sys` cross blocker;
  **all deps (incl. C deps) now cross-compile for aarch64** (see findings).
- Seam predicate committed: `build.rs` emits `cfg(jit)` ⇔ x86-64 && not
  `no-jit` (see "Seam selector" below).
- **VM-shared helpers relocated out of `jitgen`** (3 commits, each pure /
  dual-green) so `jitgen` can be cleanly `#[cfg(jit)]`-excluded:
  1. `basic_block` (BasicBlockInfo/Id) → `crate::basic_block`.
  2. `icmp_eq..ge` compare helpers → `codegen.rs` (the JIT-only `set_*`
     flag-to-bool helpers stay in `jitgen`).
  3. `execute_gc_inner` now takes a `write_back: impl FnOnce(&mut Self)`
     closure instead of `Option<&jitgen::WriteBack>` (VM passes a no-op).

**Next step (the remaining gating):** wire `#[cfg(jit)]` per the refined
worklist below; green-gate is `cargo check --features no-jit` (x86), no qemu.
Then port the VM-tier asm to aarch64. x86 default must stay green throughout.

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
`--features no-jit`. The JIT-tier code must be `cfg`-excluded for aarch64,
**but `jitgen` is not purely JIT** (see the review finding below): it also
houses VM/bytecode infrastructure (`basic_block`, `conv`) that the
interpreter and `bytecodegen` use unconditionally. So the module cannot be
excised wholesale — the infra must be relocated first, then the genuinely
JIT-tier remainder gated. `jitgen` is referenced from ~19 files / ~23 sites
outside `codegen/jitgen/`.

### Seam selector — decided: the existing `no-jit` feature + arch

Decision (owner): reuse the existing **`no-jit`** cargo feature rather than
inventing a new one. `build.rs` now emits a single source-of-truth cfg:

```rust
// monoruby/build.rs
println!("cargo::rustc-check-cfg=cfg(jit)");
let x86 = env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("x86_64");
let no_jit = env::var_os("CARGO_FEATURE_NO_JIT").is_some();
if x86 && !no_jit { println!("cargo::rustc-cfg=jit"); }
```

So **`jit` ⇔ x86-64 AND not `no-jit`**. Consequences:

- x86 default → `jit` on → JIT compiled (today's behaviour, unchanged).
- x86 `--features no-jit` → `jit` **off** → JIT *compile-excluded* (VM only).
- any aarch64 → `jit` off → JIT compile-excluded (VM only), always.

The big win: `cargo check --features no-jit` on **x86** is a faithful proxy
for the aarch64 JIT-excision — it exercises the exact same `#[cfg(not(jit))]`
paths **without qemu and without needing the A64 asm port done**. Make that
green first; aarch64 then only adds the VM-tier asm port on top.

### JIT-excision roadmap (verify with `cargo check --features no-jit` on x86)

Gating `#[cfg(jit)] pub mod jitgen;` and checking `--features no-jit`
surfaced **78 first-wave errors**. **Reviewing** them (rather than fixing
blindly) revealed that the naive "excise the whole `jitgen` module" plan is
**wrong** — `jitgen` mixes JIT-tier code with VM/bytecode infrastructure.
Two pieces are load-bearing for the interpreter and must NOT be gated:

- **`basic_block` (`BasicBlockId` / `BasicBlockInfo` /
  `BasicBlockInfoEntry`)** — built **unconditionally** by `bytecodegen`
  (`encode.rs:115`, for every method) and stored in `ISeqInfo.bb_info`;
  read by the VM/runtime in `globals/store/iseq.rs` (`get_bb_pc`, `get_bb`),
  `store.rs:826`, `function.rs:1370`. Despite living in `jitgen`, it is
  bytecode-level infra. **Relocate** the `basic_block` module out of
  `jitgen` (e.g. into `bytecodegen/` or `globals/store/`) and re-point the
  `crate::jitgen::BasicBlock*` imports.
(Correction after closer reading: `conv` does **not** need relocating. Its
`builtins/fiber.rs:106` and `class.rs:443` uses are inside `ir.inline(...)`
generators — JIT-tier, gated — and the `builtins/kernel.rs` `conv(...)` calls
are an unrelated local FFI helper. So `conv` stays in `jitgen` and is gated
with the rest. Only `basic_block` is load-bearing for the VM.)

**(Done — three VM-shared helpers relocated/decoupled out of `jitgen`:
`basic_block` → `crate::basic_block`; `icmp_*` → `codegen.rs`;
`execute_gc_inner` now takes a write-back closure. Each is a pure /
dual-green commit. With these out of the way the VM tier (`vmgen`,
`codegen.rs` GC poll) compiles with `jitgen` excluded.)**

### Remaining gating worklist (after relocations: ~79 errors)

Re-gating `#[cfg(jit)] pub mod jitgen;` + `lib.rs` `JitContext` re-export and
running `cargo check --features no-jit` now leaves ~79 errors, all genuinely
JIT-tier:

- **~60 in builtins — the inline-method system** (13 files:
  `object, array, string, kernel, fiddle, class, fiber, math, hash, range,
  arithmetic_sequence, numeric/float, numeric/integer`). Per file: gate the
  `use jitgen::…`/`use jitgen::JitContext` import, gate each inline generator
  fn (signature uses `&mut AsmIr`/`&JitContext`), and make the ~37
  `define_builtin_inline_func*` call sites conditional —
  `#[cfg(jit)]` the inline form + `#[cfg(not(jit))]` a plain
  `define_builtin_func`/`_with` (the VM fn is already registered separately,
  so this is a faithful fallback). Also gate the `InlineGen` type alias
  (`globals.rs:~27`) and `builtins.rs`'s `AbstractState`/`asmir::*` imports.
- **Codegen / driver JIT internals (codegen-side):**
  - `codegen.rs:44,48` imports `AsmEvict` / `SpecializedCodeInfo` → gate, plus
    the JIT-only `Codegen` fields they type and their init/uses:
    `compilation_unit` (4), `asm_return_addr_table` (2), `specialized_info`
    (7), `specialized_base` (3), `return_addr_table` (5).
  - `jit_execute_gc` (codegen.rs) and `jit_check_stack` (jit_module.rs:268)
    call `gen_write_back` → gate the methods `#[cfg(jit)]` (their callers are
    JIT: `compile.rs:330`, the JIT GC path).
  - `patch.rs` `guard_class2`, `compiler.rs` `jit_compile` (+ the
    `jit_compile_patch`/`jit_compile_loop` driver) → gate; the VM's
    "compile-when-hot" trigger becomes a no-op under `not(jit)`.
  - `bytecode.rs:357 method_cache()` + `MethodCacheEntry` import → gate
    (called only from `jitgen/trace_ir.rs`).
  - `TraceIr::format` dump sites (`codegen.rs` disasm dump, `dump.rs:116`,
    `store.rs:830`) → gate the `format`/`from_pc` calls.

This is mechanical but cascades (Codegen fields → init → uses; `compiler.rs`
is largely the JIT driver). It is **not** independently verifiable until
complete (no-jit only goes green at the end), but x86 **default** stays green
at every step — check it after each file.

Everything else flagged is genuinely **JIT-tier and gate-able** with
`#[cfg(jit)]`:

- **44 × `AsmIr` — builtins' inline-method generators** (the dominant
  coupling). `define_builtin_inline_funcs_with_kw` already registers the VM
  function via `new_builtin_fn(address,…)` *and* the inline generator
  separately, so **every inline builtin already has a VM fallback**. Give
  `define_builtin_inline_func*` a `#[cfg(not(jit))]` arm that ignores
  `inline_gen` (VM function only), gate each generator fn + the `ir.inline`
  closures under `#[cfg(jit)]`. Files:
  `builtins/{object,array,string,kernel,fiddle,class,fiber}.rs`,
  `builtins/numeric/{float,integer}.rs`, `builtins.rs`.
- **~15 × JIT-tier `Codegen`/`JitModule` methods**: `icmp_{eq,ne,lt,le,gt,
  ge}`, `guard_class2`, `jit_compile`, `gen_write_back`, `jit_execute_gc`
  (codegen.rs:768), `jit_check_stack` / `execute_gc_inner` (jit_module.rs:
  240/268), `immediate_eviction` — gate methods + call sites.
- **`MethodCacheEntry`** (`trace_ir.rs:50`) + `bytecode.rs:357
  method_cache()` — JIT-only (`method_cache()` is called *only* from
  `jitgen/trace_ir.rs:418`). Gate both.
- **`TraceIr` dump sites** (`codegen.rs:1355` disasm dump,
  `globals/dump.rs:116`, `store.rs:830`; `dump.rs:194` is already under
  `cfg(deopt/profile)`) — gate the `TraceIr::format`/`from_pc` calls under
  `#[cfg(jit)]` (the `bb_info.is_bb_head` part next to them stays — it's
  basic_block, relocated/kept).
- **Misc type re-exports/aliases**: `codegen.rs:17,21` (`AsmEvict`,
  `SpecializedCodeInfo` → the JIT-only `Codegen` fields
  `asm_return_addr_table`/`specialized_info`/`specialized_base`/
  `compilation_unit` and their init/uses), `lib.rs:25` (`JitContext`
  re-export), `globals.rs:28-30` (the `InlineGen` fn-type alias using
  `AbstractState`/`AsmIr`/`JitContext`), `builtins.rs:48-49`.

Green-gate: when `cargo check --features no-jit` (x86) passes, the excision
is complete and correct. x86 default must stay green at every commit (it
keeps `jit` on, so all `#[cfg(jit)]` code remains).

### Then: VM-tier asm port (aarch64)

Only after excision: port the ~213 VM-tier `monoasm!` blocks (+ the ~50
builtin runtime `monoasm!` blocks) to `monoasm_arm64!` behind
`#[cfg(not(jit))]` / sibling modules, per the milestones below, until
`cargo build --target aarch64-unknown-linux-gnu` links and runs under qemu.

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
