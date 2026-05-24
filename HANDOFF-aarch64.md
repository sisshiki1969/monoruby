# Handoff: monoruby macOS/aarch64 backend

Continuation notes for the next session. Goal: add a **native aarch64
backend** to monoruby (macOS/Apple Silicon target). Per the maintainer's
direction, the order is:

1. **monoasm gains an aarch64 instruction encoder** ← largely done (this handoff)
2. **VM-only execution first** (no JIT): port monoruby's `vmgen` bytecode
   dispatch to aarch64 ← next big step
3. JIT (`jitgen`) port ← later

The whole monoruby execution engine currently emits x86-64 only (the VM
dispatch loop itself is generated machine code via the `monoasm!{}`
macro; there is **no** portable interpreter). So an aarch64 assembler is
the prerequisite for everything, which is why we started there.

---

## What is done (monoasm aarch64 backend)

Work lives on the **`aarch`** branch of `sisshiki1969/monoasm`, cut from
`rc`. It is captured in `aarch64-monoasm.patch` (committed next to this
file) which applies cleanly on top of `rc`.

New/changed files:

- `monoasm/src/arm64.rs` (new) — A64 encoders + `GReg`/`FReg`/`Cond`/`Arm64Reloc`.
- `monoasm/src/lib.rs` — `pub mod arm64; pub use arm64::*;` and a new
  `TargetType::Arm64` relocation variant.
- `monoasm/src/jit_memory.rs` — `emit_arm64_branch()` + `write_reloc`
  handling for aarch64 PC-relative relocations (reuses the existing
  `DestLabel`/`finalize` machinery; x86 path untouched).
- `monoasm/src/test.rs` — gated the x86-only register-dump inline asm
  behind `#[cfg(target_arch = "x86_64")]` so the crate builds for aarch64.
- `monoasm/tests/arm64_encoding.rs` (new) — host-portable byte checks.
- `monoasm/tests/arm64_exec.rs` (new) — end-to-end execution (aarch64).

Instruction coverage (all cross-checked against `llvm-mc --triple=aarch64`
and executed under qemu):

- Move: `movz/movn/movk/mov/mov_imm/mov_sp`
- Add/sub: immediate + shifted-register, `cmp/cmn/neg`, SP forms
- Logical: `and_/orr/eor/ands/mvn/tst` (+ shifted)
- Mul/div: `mul/madd/msub/sdiv/udiv`
- Shifts: `lslv/lsrv/asrv`, immediate `lsl/lsr/asr`, `sxtw`
- Conditional select: `csel/csinc/cset/csetm` (`Cond` enum w/ `invert()`)
- Loads/stores: unsigned-offset (64/32/16/8 + signed), pre/post-index,
  register-offset, `ldp/stp` (+ `push_pair`/`pop_pair`), FP `ldr_f/str_f`
- Floating-point: `fmov` (reg + GPR<->FP), `fadd/fsub/fmul/fdiv`,
  `fcmp`/`fcmp_zero`, `scvtf`, `fcvtzs`
- Branches: `b/bl/b.cond/cbz/cbnz/tbz/tbnz/adr` (label-resolved),
  `br/blr/ret`
- System: `nop/brk`

Verification status: 15 encoding tests (any host) + 9 qemu execution
tests (constant return, arithmetic, signed div, sum loop = fwd+back
branch reloc, csel max, stack push/pop, `bl` call, FP, int<->float) all
green. Existing x86 monoasm tests: no regression.

API style: builder methods on `JitMemory` (e.g. `jit.add(X0,X1,X2)`),
**not** a `monoasm!{}`-style DSL yet. A proc-macro DSL (in
`monoasm_macro`) is desirable later to make the `vmgen` port ergonomic,
but is not required to make progress.

---

## Dev environment setup (x86-64 host, emulated aarch64 testing)

This container has no Apple hardware; aarch64 code is built with the GNU
cross toolchain and run under qemu-user.

```sh
# Rust aarch64 std (under the pinned nightly)
rustup target add aarch64-unknown-linux-gnu

# Cross linker + qemu (already needed: gcc-aarch64-linux-gnu, qemu-user,
# libc6-dev-arm64-cross)
sudo apt-get update
sudo apt-get install -y --no-install-recommends \
    qemu-user gcc-aarch64-linux-gnu libc6-dev-arm64-cross
```

monoasm needs nightly (`#![feature(proc_macro_hygiene)]`). Use
`cargo +nightly-2026-05-18 ...`. The aarch64 target was installed under
that toolchain.

`.cargo/config.toml` in the monoasm checkout (it is gitignored by the
repo's `/.*` rule, so re-create it each time):

```toml
[target.aarch64-unknown-linux-gnu]
linker = "aarch64-linux-gnu-gcc"
runner = "qemu-aarch64 -L /usr/aarch64-linux-gnu"
```

Test commands:

```sh
# host byte-encoding checks
cargo +nightly-2026-05-18 test -p monoasm --test arm64_encoding
# emulated execution
cargo +nightly-2026-05-18 test -p monoasm --target aarch64-unknown-linux-gnu --test arm64_exec
# regression check (x86 host)
cargo +nightly-2026-05-18 test -p monoasm
```

Golden encodings are obtained with `llvm-mc --triple=aarch64
--show-encoding` (instruction-level) and `llvm-mc -filetype=obj` +
`llvm-objdump -d` (resolved branch displacements). Always verify new
encoders this way before trusting them.

---

## How to land / continue the monoasm work

`origin/aarch` does not exist yet — the work has only been delivered as a
patch (committed here as `aarch64-monoasm.patch`).

In the new session (with `sisshiki1969/monoasm` added to the MCP
Repository Scope):

```sh
git clone https://github.com/sisshiki1969/monoasm.git
cd monoasm && git checkout -b aarch rc
git apply /path/to/aarch64-monoasm.patch   # from this monoruby branch
# verify (see commands above), then commit and push via the GitHub MCP
# push_files tool (server-side commit — avoids the local signing issue).
```

### Push / commit constraints learned this session

- The managed commit-signing server (`/tmp/code-sign`, `commit.gpgsign=true`
  globally) is bound to the **monoruby** session source; committing inside
  the **monoasm** repo fails with `missing source`. Pushing via the GitHub
  **MCP** tools (`push_files`/`create_branch`) sidesteps this because the
  commit is created server-side.
- Plain `git push` to monoasm has **no credentials** in this environment
  (HTTPS prompts for a username). Use MCP, or have the user supply a token.
- The GitHub MCP scope is fixed at session start. `monoasm` must be in the
  allowed-repos list of the *new* session.

---

## Next step: port `vmgen` to aarch64 (VM-only, no JIT)

In monoruby, the bytecode VM dispatch is generated in
`monoruby/src/codegen/vmgen.rs` (+ `vmgen/`) using the x86 `monoasm!{}`
DSL. Key facts for the port:

- JIT global registers (x86 -> pick aarch64 callee-saved equivalents):
  `rbx`=&Executor, `r12`=&Globals, `r13`=PC, `r14`=LFP, `r15`=accumulator.
  On aarch64 use callee-saved `x19..x28` for these (e.g. x19..x23) and
  define them centrally.
- Frame offsets: `LFP_OUTER +0, LFP_META +8, LFP_BLOCK +16, LFP_SELF +24,
  LFP_ARG0 +32` (unchanged; just different load/store encodings).
- Value repr is tagged (low 3 bits); fixnum = `v>>1`, etc. (see
  monoruby `CLAUDE.md`). The arithmetic guards translate directly.
- `no-jit` only disables the JIT tier; the base VM tier is still generated
  machine code, so the VM dispatch must be ported for *any* aarch64 run.
- macOS specifics (for real hardware, not qemu): W^X requires `MAP_JIT` +
  `pthread_jit_write_protect_np`; icache maintenance after emitting code;
  AAPCS64 calling convention; macOS errno table + platform string. None of
  these are needed under qemu but will be for the actual mac target.

Recommended approach: introduce a thin arch-abstraction so monoruby can
select x86 vs aarch64 codegen via `cfg(target_arch)`, then port the VM
dispatch handlers one opcode family at a time, validating each under qemu.

The instruction-set foundation needed for that port is now in place.
