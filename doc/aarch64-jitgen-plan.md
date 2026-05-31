# Plan: porting monoruby's JIT (`jitgen`) to aarch64

Continuation of the aarch64 backend work after the VM tier
(`doc/aarch64-vmgen-plan.md`, **done** — merged) and the `arch/` split
(#643) + `monoasm_arm64!` DSL rewrite (#644). This document covers bringing
the **JIT compiler** up on aarch64 (macOS/Apple-Silicon target, validated
under qemu-user on x86 hosts).

> Status: **Phase 1 (foundation) in progress.** The JIT runs only on
> x86-64; aarch64 is VM-only. This plan defines the seam and the phased
> path to an aarch64 JIT.

## Background: how the JIT is wired today

```
Bytecode ──▶ TraceIR ──▶ AsmIR ──▶ machine code ──▶ monoasm
            (front-end, arch-neutral)   (back-end, x86-64 only)
```

- **`cfg(jit)`** is set by `build.rs` **iff** `target_arch = x86_64 && !no-jit`.
  It gates the entire `codegen/jitgen/` tree, `codegen/compiler.rs`, and
  `codegen/patch.rs`. On aarch64 the JIT is compile-excluded and every method
  falls through to the VM (`arch/aarch64/wrapper.rs` emits `b vm_entry`).

### Empirically-confirmed seam

Naively enabling `cfg(jit)` on aarch64 produces **~1000 compile errors**, all
in the **machine-emission back-end**, never in the front-end logic:

- x86-only `monoasm!` expansions calling `JitMemory` encoder methods that
  don't exist on aarch64: `enc_rexw_mr` (×215), `enc_d` (×143), `enc_rexw_o`
  (×118), `enc_rexw_digit`, `enc_digit`, `enc_mr`, `enc_oi`, … (~900 total).
- x86-only monoasm operand types: `Reg`, `Scale`, `Imm` (~360).
- calls into x86 VM-tier helpers (`generic_handle_arguments`, `call_funcdata`,
  `set_lfp`, `push_frame`/`pop_frame`, `get_func_data`, `save_registers`,
  `method_return`, `integer_val_to_f64`, …) that live in `arch/x86_64/`.
- `JitMemory` x86 patch/reloc helpers: `apply_jmp_patch_address`,
  `emit_reloc`.

**Conclusion:** the x86 `monoasm!` back-end genuinely does not compile on
aarch64 (it is not merely emitting wrong bytes). The port must split the
JIT into an **arch-neutral front-end** (compiles everywhere) and an
**arch-specific back-end** (one impl per arch), exactly mirroring how the VM
tier is structured under `arch/{x86_64,aarch64}/`.

## Layer inventory (`jitgen/`, ~16,150 LoC)

| Layer | Files | LoC | Arch | Action |
|---|---|---|---|---|
| TraceIR + analysis | `trace_ir.rs`, `state*`, `context.rs`, `merge.rs`, `loop_analysis.rs`, `definition.rs` | ~6,500 | neutral | **reuse as-is** |
| Front-end lowering | `compile.rs`, `compile/*` (Bytecode→AsmIR) | ~2,400 | neutral | reuse (audit for stray asm) |
| AsmIR definitions | `asmir.rs` (AsmInst enum, WriteBack, SideExit) | ~2,040 | neutral* | reuse; `GP`/operand types need arch-awareness |
| **Back-end emission** | `asmir/compile.rs`, `asmir/compile/*` | ~4,300 | **x86 (310 `monoasm!`)** | **rewrite per-arch** |
| Guard / deopt asm | `guard.rs`, `deoptimize.rs` | ~650 | x86 (21 `monoasm!`) | split logic vs asm; rewrite asm |
| Integration | `compiler.rs`, `patch.rs` | ~570 | x86 | rewrite per-arch |
| Register model | `GP` enum, `FPReg::loc` (`codegen.rs`) | — | x86 | make arch-aware |

`*` `AsmInst` *structure* is arch-neutral, but operands reference the
x86-only `GP` enum and the x86 xmm spill-pool math.

## Register model

- **`GP`** (`codegen.rs`): 14-variant x86 enum (`Rax`..`R15`). aarch64 needs a
  parallel register set (`X0`..`X30`, `SP`/`XZR`). Decision: keep `jitgen`
  monoarch like the VM tier — gate `GP` per `target_arch` (x86 keeps current
  enum; aarch64 gets an `X`-register enum), rather than introducing a generic
  `Register` trait (too invasive across 310 emission sites).
- **`FPReg`** (`codegen.rs`): virtual FP register; `FPReg::loc` maps id→
  `xmm{id+2}` or `[rbp-…]` spill. aarch64: `d{id+2}` (d0/d1 scratch, d2..d15
  callee-saved pool of 14) + frame-relative spill. Spill base offset (the
  `-24` constant) is x86-specific and must be revisited against the aarch64
  frame layout in `arch/aarch64/vmgen.rs`.

## monoasm_arm64 readiness

`monoasm_arm64!` (monoasm `aarch` branch) already supports every instruction
class the JIT needs: integer arith/logical/shift, `cmp`/`tst`, `sdiv`/`udiv`,
scalar-double FP (`fadd`/`fsub`/`fmul`/`fdiv`/`fsqrt`/`fcmp`/`fcsel`),
conversions (`scvtf`/`fcvtzs`), conditional select (`csel`/`cset`), the full
branch/`tbz`/`cbz` family, and load/store incl. pre/post-index and scaled
register-offset. Two items to verify during implementation:

1. **PC-relative data**: x86 `[rip + label]` → aarch64 `adr`/literal-pool
   (`adr` has a 21-bit range; far refs need `ldr =label` literal pools).
   Used by class-version / inline-cache guards and JIT data labels.
2. **Branch patching / relocation**: `apply_jmp_patch_address`, `emit_reloc`
   must have aarch64 equivalents (B/BL imm26) for the deopt/recompile and
   class-guard patch points.

## Phased plan

### Findings from the foundation probe (2026-05) — REQUIRED READING

A spike enabled `cfg(jit)` on aarch64 and gated the `jitgen` emission
back-end to x86 (with an `unreachable!` `compile_asmir` stub). Outcome,
which **reshapes Phase 1**:

1. **`cfg(jit)` is not `jitgen`-local — it means "x86 JIT" codebase-wide.**
   Beyond `jitgen`, the JIT emission is woven into the **builtins**: each
   inlined method (`Integer#+`, `Array#push`, `Math.sqrt`, …) registers a
   `#[cfg(jit)]` inline-generator closure (the `inline_gen!` macro in
   `lib.rs`, with `#[cfg(not(jit))]` no-op twins for the VM build). Enabling
   `cfg(jit)` on aarch64 compiles **~14 builtins files'** worth of x86
   inline generators → hundreds of errors outside `jitgen`.
2. **The front-end is not cleanly AsmIR-only.** Several front-end lowering
   paths *inline-emit* by calling back-end helpers directly (e.g.
   `AbstractState::array_integer_index`, `store_fpr_into_xmm`, `gen_shr`,
   `gen_int_pow`) rather than only pushing `AsmInst`s. So "front-end compiles
   standalone" is not achievable by module gating alone.
3. **`GP` does NOT need splitting to compile.** The front-end uses
   `GP::Rdi` etc. as *abstract* register ids; `GP` is a plain enum that
   type-checks fine on aarch64. Register *meaning* only matters in the
   (arch-specific) back-end. Defer the `GP`/`FPReg` arch-mapping to Phase 2.
4. The `jitgen`-internal back-end gating itself is straightforward and
   x86-safe (gate `asmir::compile`, `guard`, `deoptimize`, the `jitgen.rs`
   emission impls, `compiler`/`patch`, `jit_check_stack`, the `compile/index`
   asm methods, `asmir.rs::{gen_asm,handle_error}` to `target_arch=x86_64`).

**Conclusion:** the seam is bigger than `jitgen`. The first real unit of
work is a **cfg architecture redesign** that cleanly separates "JIT
front-end" from "x86 emission" across *both* `jitgen` and the builtins
inline-generator surface (and likely a new `inline_gen!` arm that stubs the
generator on aarch64-jit). This is sizeable; it is the gate for everything
after.

### Phase 1 — cfg architecture seam (revised)
- Decide the cfg model: e.g. `jit` (front-end planning, both arches) vs
  `jit_emit_x86` (= `jit && target_arch=x86_64`, the x86 emission incl.
  builtins inline generators). Rework `inline_gen!` so the aarch64-jit build
  gets a stub/bail generator (mirroring the `not(jit)` twin).
- Gate the `jitgen` emission back-end + builtins inline generators to
  `jit_emit_x86`; provide aarch64 back-end stubs for the emission interface
  the front-end calls (incl. the inline-emit helpers in finding #2).
- Keep the aarch64 JIT **trigger disabled** (wrapper stays `b vm_entry`) so
  aarch64 stays VM-only at runtime; stubs are never hit.
- **Acceptance:** `cargo check` green on x86 (full JIT) **and** aarch64 (JIT
  front-end compiled, emission stubbed, VM at runtime). No behavior change.
- *(GP/FPReg arch-mapping moves to Phase 2 — not needed to compile.)*

### Phase 2 — core lowering
Implement `compile_asmir` AsmInst variants for aarch64, by category:
reg/stack moves + prologue/epilogue → integer arith/cmp + guards → FP →
method-call arg setup → variables/constants/definitions → deopt/recompile.

### Phase 3 — integration + trigger
Rewrite `compiler.rs` (counter/patch setup) and `patch.rs` (class-guard stub)
for aarch64; PC-relative data + branch patching; flip the aarch64 wrapper to
the JIT entry path; deopt/recompile round-trips.

### Phase 4 — validation
Run the JIT test suite + optcarrot on aarch64 (qemu + Apple Silicon);
benchmark JIT vs VM.

## Cross-test

Build/run aarch64 under qemu on x86 hosts via `.cargo/config.toml` +
`bin/setup-aarch64-cross` (already wired). The JIT emits and executes native
A64 under qemu-user.
