# Plan: porting monoruby's JIT (`jitgen`) to aarch64

Continuation of the aarch64 backend work after the VM tier
(`doc/aarch64-vmgen-plan.md`, **done** — merged) and the `arch/` split
(#643) + `monoasm_arm64!` DSL rewrite (#644). This document covers bringing
the **JIT compiler** up on aarch64 (macOS/Apple-Silicon target, validated
under qemu-user on x86 hosts).

> Status: **Phase 1 + 3a DONE; Phase 3b IN PROGRESS — aarch64 now JIT-compiles
> and executes methods.** The `jit`/`jit_emit` seam is in place, the
> initial-compile driver is arch-neutral, the A64 wrapper trigger fires for hot
> ISeq methods, runs the front-end, lowers the AsmIR (`a64_gen_machine_code` →
> `compile_asmir`), and **installs via indirect dispatch** (no runtime branch
> patching on aarch64: `compile_patch` publishes the compiled entry into a
> per-method heap slot the wrapper `br`s to). Any not-yet-ported `AsmInst`
> bails (`compile_asmir` → `false` → `Err` → `None`) so the method stays on the
> VM — so only fully-lowerable methods JIT and big.rb stays byte-identical to
> x86.
>
> **Supported AsmInst (~28):** frame/move/return — `Label`, `BcIndex`, `Init`
> (prologue + nil-fill), `Preparation`, `StackToReg`, `RegToStack`,
> `AccToStack`, `RegMove`, `RegToAcc`, `LitToReg`, `LitToStack`, `Ret`;
> control flow — `CondBr` (truthiness), `NilBr`, `CheckLocal`, `Deopt`,
> `HandleError`; variables — `LoadDynVar`/`StoreDynVar` (via `a64_get_outer`),
> `LoadGVar`/`StoreGVar`; allocation/runtime C calls — `DeepCopyLit`,
> `CreateArray`, `NewArray`, `NewHash`, `ConcatStr`, `NewRange`; guards —
> `GuardClass`, `GuardClassVersion` (foundational: compile-clean but only
> reached at call sites, which await method-call lowering). C calls bail if
> any xmm is live (no FP save/restore yet). `GP::a64()` maps regs (slots
> lfp(x22)-relative, matching `a64_op_ret`; acc=`R15`→x23; Executor=x19,
> Globals=x20=`R12`; result in x0).
>
> **Side-exit/deopt machinery (done):** `a64_gen_asm` emits each block's
> deopt/evict/error handlers (cold, skipped by a `b`; guards branch back).
> `a64_gen_deopt` writes live values back to the LFP
> (`a64_gen_write_back_for_deopt`, bailing on FP/forwarding-rest), sets PC,
> and jumps to `vm_fetch`; `a64_gen_handle_error` jumps to `entry_raise`.
> `RecompileDeoptimize` is treated as a plain deopt for now. Exercised
> end-to-end by hash creation (gen_hash → Option<Value> → `HandleError`
> `cbz x0`).
>
> **Bridge processing (critical):** the driver mirrors x86 `gen_machine_code`
> — it lowers main blocks **and** their inline/outline bridges. A branch
> destination is often a distinct `JitLabel` from the destination block's own
> label, bound only inside a bridge trampoline; skipping bridges left such
> branches at offset 0 (an infinite `b.eq #0` self-loop). `a64_gen_asm` takes
> optional `entry`/`exit` to bind a bridge's entry label and append `b <exit>`.
>
> **Next:** the big remaining gateway is **method-call lowering**
> (`SetupMethodFrame`, `SetArguments`, `do_call` + return-address deopt
> patching, `MethodRet`). With `inline_gen` off, `a + 1` de-inlines to a method
> call, so this unlocks the bulk of real methods (and finally exercises the
> class guards at call sites). After that: FP (`XmmSave`/`FloatBinOp`/…),
> ivars (`LoadIVar*`/`StoreIVar*`), constants, index ops. `inline_gen`
> re-enablement stays last.
>
> All work is on branch `claude/wizardly-pasteur-8N2Ub`; both arches
> build green at every commit.

## Current state (achieved — committed)

The seam from "Phase 1 (revised)" below is implemented and validated:

| Piece | What | cfg |
|---|---|---|
| `build.rs` | `jit` = front-end (x86-64 **and** aarch64); `jit_emit` = x86 emission only | — |
| inline generators | `inline_gen!`, `InlineGen`/`InlineFuncInfo::InlineGen`, `define_builtin_inline_func*`, 13 builtins generator files, the `method_call` invocation arm | `jit_emit` |
| jitgen emission | `asmir::compile` (+ aarch64 `compile_stub`), `jitgen.rs` emission impls (`load_store!`, xmm/write-back/deopt, `gen_machine_code`), `guard`/`deoptimize`, `gen_asm`, `compile/index` array fast-paths | `jit_emit` |
| runtime patch | `jit_check_stack`, `jit_execute_gc`, `immediate_eviction`, BOP-redefine entry repatch (`class.rs`) | `jit_emit` |
| driver entry | `jit_compile` split: arch-neutral front-end (`traceir_to_asmir`), then `gen_machine_code` (x86) **or** `Err(CompileError)` bail (aarch64) | `jit` / `jit_emit` |

**Validated:** both arches `cargo check` green, aarch64 warning-clean; x86
JIT output unchanged; aarch64 runs a 49-line opcode program **byte-identical
to x86** under qemu (front-end compiled but unreached — every method is
VM-interpreted, since the trigger is x86-only).

**Confirmed along the way:**
- `GP` needs **no** arch split to compile (the front-end uses it as abstract
  register ids).
- The compile-time bail path already exists: `jit_compile -> Err(CompileError)`
  → `compile() -> None` → method stays VM-interpreted (and is marked
  `jit_invalidated` so it won't retry). This is the hook for incremental
  TraceIR/AsmInst support.
- The front-end ↔ emission boundary in `jit_compile` splits cleanly.

## Remaining work (the next, coupled piece)

Making the front-end actually *run* on aarch64 (so it can bail per-method, then
JIT incrementally) needs the **driver chain** ported. A spike `un-gating`
`compiler.rs` showed it is more coupled than a single-file change:

1. **Driver entry points are x86-emission-laced.** `compiler.rs::compile()`
   patches the JIT entry on success (`apply_jmp_patch_address`), `compile_patch`
   lives in `patch.rs` (`jit_emit`), and `recompile_*` / `save_registers` /
   `gen_compile_patch` / `gen_recompile*` / `gen_compile_loop` are all x86
   `monoasm!`. The extern trigger entries (`jit_compile_patch`, …) call into
   `patch.rs`.
2. So the chain `trigger → jit_compile_patch → compile_patch[patch.rs] →
   compile_method → compile → jit_compile` must be arch-neutralized: keep the
   orchestration as `jit`, gate every emission/patch site to `jit_emit`, and
   provide aarch64 stubs (on aarch64 `compile()` returns `None`, so the
   success-path patching is never reached — but it must still type-check).
3. **A64 JIT trigger.** `arch/aarch64/wrapper.rs` (ISeq path) needs a
   `monoasm_arm64!` counter + an A64 `gen_compile_patch` equivalent that calls
   the (now arch-neutral) `jit_compile_patch`. `arch/x86_64/wrapper.rs` +
   `compiler.rs::gen_compile_patch` are the reference.

This is a coupled, multi-file refactor + new A64 emission with qemu iteration —
best done as a focused effort, not at the tail of a long session.

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

### Decisive finding: planning and emission are interleaved (no clean front-end-only build)

A second pass (introduce a `jit_emit` cfg = `jit && x86_64` and try to keep a
pure-`jit` front-end compiling on aarch64) shows there is **no useful
"front-end only" compilation unit**:

- monoruby's JIT does **not** do a clean two-pass *build-IR → lower-IR*. The
  front-end lowering interleaves planning with emission: it invokes the
  per-method **inline generators** (emission) during method-call lowering and
  calls emission helpers inline (`array_integer_index`, `store_fpr_into_xmm`,
  `gen_shr`, `gen_asm`, guards, write-back, xmm save/restore, …).
- Therefore any `jit`-but-not-`jit_emit` code that would compile on aarch64
  must have an aarch64 **stub for essentially the entire emission interface**
  (~dozens of `Codegen`/`JitModule`/`AbstractState` methods + an aarch64
  `inline_gen!`/`define_builtin_inline_func` form). The result would be a
  large body of dead, never-run stubs.
- Gating *everything* `jit` → `jit_emit` instead makes aarch64 identical to
  today's VM-only build (no JIT code at all) — i.e. no progress.

So there is **no behavior-neutral, validatable "foundation" milestone** short
of implementing real aarch64 emission. The honest path is to treat the
aarch64 JIT as the **emission port itself** (Phase 2), done as a **vertical
slice**: enable `jit` on aarch64 behind the `jit_emit` seam, then implement
just enough aarch64 emission (entry/wrapper + `compiler`/`patch` + a minimal
`compile_asmir` that handles a trivial method and **deopts to the VM on
everything else**) to get one method JIT-compiling end-to-end under qemu —
then widen opcode/inline coverage incrementally. The `jit_emit` cfg seam +
`inline_gen!` aarch64 arm are built as part of that slice (not as a separate
dormant step, since they can only be validated alongside emission).

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
- *(GP/FPReg arch-mapping moves later — not needed to compile.)*
- **Status: DONE** (see "Current state" above).

### Phase 3a — arch-neutralize the driver chain + A64 trigger (NEXT)
Goal: make the front-end actually *run* on aarch64 and bail to the VM, so the
trigger→compile→bail→VM loop works end-to-end (no method JITs yet). Steps:
1. Un-gate `compiler.rs` to `cfg(jit)`; gate its emission methods
   (`gen_compile_patch`, `gen_recompile`, `gen_recompile_specialized`,
   `gen_compile_loop`) and the success-path patch sites (`apply_jmp_patch_address`,
   `save_registers`/`restore_registers`) to `jit_emit`.
2. `patch.rs`: split `compile_patch` so its orchestration is `jit`
   (calls `compile_method`, checks the `Option`) and only the actual entry
   repatch is `jit_emit`. Same for any class-guard stub setup.
3. The extern entries (`jit_compile_patch`, `jit_recompile_*`, `jit_compile_loop`)
   become `jit` (they only call the now-arch-neutral driver).
4. `arch/aarch64/wrapper.rs`: emit the A64 JIT trigger for the ISeq path — a
   `monoasm_arm64!` counter + a `gen_compile_patch` equivalent that, after
   `COUNT_START_COMPILE` calls, calls `jit_compile_patch` and (on aarch64
   always) re-enters `vm_entry` since `compile()` returns `None`.
   Reference: `arch/x86_64/wrapper.rs::gen_jit_stub` + `compiler.rs::gen_compile_patch`.
5. **Acceptance:** under qemu, a hot method on aarch64 hits the threshold,
   runs the front-end (`traceir_to_asmir`), bails (`Err`→`None`), and keeps
   executing via the VM — output unchanged, no crash.

### Phase 3b — incremental AsmInst lowering (the multi-week core)

This is the bulk of the port: the x86 emission back-end is ~4,300 LoC across
`asmir/compile/*` (120+ `AsmInst` variants). The aarch64 side is built up
through a repeated **implement → verify (qemu) → debug** cycle. Groundwork and
design decisions:

- **Register map (done):** `GP::a64()` (codegen.rs) maps the x86-named abstract
  JIT registers to A64 — globals `R12/R13/R14/R15`→`x20/x21/x22/x23` (match the
  VM), scratch `Rax..R11`→`x0..x8`, `x9..x15` free for lowering temps, `Rsp`→`sp`.
  `FPReg::loc`'s `Xmm(n)` is read as `d{n}` on aarch64.
- **Call ABI:** x86 (rdi/rsi/rdx/rcx/r8/r9 args, rax ret) ≠ aarch64 (x0..x7 / x0).
  The `SetArguments` / `Call` / `Yield` lowerings must shuffle into x0..x7
  explicitly (not a 1:1 `GP::a64` remap), and read returns from x0.
- **Emission driver:** port `gen_machine_code` + `gen_asm` (the BB/label/
  side-exit iteration) to drive `compile_asmir` on aarch64. First cut can be a
  minimal iterate-and-bail (no JIT installed) to exercise the plumbing.
- **Bail hook:** add `Codegen::set_jit_unsupported()` + a flag; `compile_asmir`
  sets it for any not-yet-ported `AsmInst`; the driver returns `Err`
  (→`compile()` `None`) so the method stays VM-interpreted. Coverage grows
  safely — only fully-lowerable methods JIT.
- **Install mechanism:** aarch64 monoasm has **no runtime branch patch**
  (`apply_jmp_patch_address` is x86-only) and `adr` can't reach JIT data from the
  wrapper. So instead of patching the wrapper entry, install via **indirect
  dispatch**: store the compiled entry in a per-method heap slot (like the
  trigger counter); the wrapper loads it and branches if set. (PC-relative data
  similarly uses heap-leaked absolute addresses, per the trigger/attr_reader.)
- **Order:** prologue/`Init` + epilogue/`Ret` → `Integer` `Mov`/`LitToReg`/
  `Add`/`Sub`/`Cmp` + fixnum guards → conditional/uncond branches + loop → FP
  (`d`-regs) → method-call arg setup + `Call`/`Yield` → variables/constants/
  definitions → deopt/side-exit. Each step: implement the variant(s), build +
  run under qemu, confirm the target method JITs and output is unchanged.

**Milestone within 3b:** the first *observable* increment is a single trivial
method (e.g. `def f(a); a; end`) actually executing as JIT code on aarch64
(verified via qemu + a deopt/jit-log counter), with everything else deopting to
the VM. That first slice is itself a large coupled piece (driver + install +
the handful of `AsmInst` it needs); subsequent variants are smaller increments.

### Phase 4 — validation
Run the JIT test suite + optcarrot on aarch64 (qemu + Apple Silicon);
benchmark JIT vs VM.

## Cross-test

Build/run aarch64 under qemu on x86 hosts via `.cargo/config.toml` +
`bin/setup-aarch64-cross` (already wired). The JIT emits and executes native
A64 under qemu-user.
