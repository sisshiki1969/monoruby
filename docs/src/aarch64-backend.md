monoruby runs natively on **aarch64** — macOS on Apple Silicon is fully supported (VM tier + JIT), and CI runs on GitHub's Apple Silicon runners. The aarch64 backend lowers the **complete** instruction set: it never declines a compilation. This page is an overview; the detailed comparison is [`doc/arch_difference.md`](design/arch_difference.md).

## How the backends are organized

```
codegen/jitgen/                 arch-neutral front-end: bytecode → TraceIR → AsmIR
codegen/jitgen/asmir/           arch-neutral lowering dispatcher (compile_asmir)
codegen/jitgen/lir.rs           arch-neutral low-level IR (LInst)
codegen/arch/x86_64/            x86-64 backend: VM tier, invokers, wrappers, encode_linst, emit_*
codegen/arch/aarch64/           aarch64 backend: same structure, mirrored file layout
```

Everything above machine-code emission is shared. The arch-neutral dispatcher lowers each `AsmInst` either into arch-neutral **LIR** (`LInst`), which each architecture encodes with its own `encode_linst`, or by calling a per-arch emission helper (`emit_*`, `guard_class` / `a64_guard_class`, `integer_binop` / `a64_integer_binop`, …). Both are selected by `cfg(target_arch)`, with no dynamic dispatch. Machine code is emitted with the [monoasm](https://github.com/sisshiki1969/monoasm) dynamic assembler, which provides both `monoasm!` (x86-64) and `monoasm_arm64!` DSLs. See [Unified low-level IR](design/lir.md).

## Full coverage

Every `AsmInst` and every side exit is lowered on aarch64. Displacements that fit are folded into `ldur`/`stur`/scaled `ldr`/`str`, and larger frame/field/sp offsets are **materialized through reserved scratch registers** (`x9`/`x10`). The `bool` "decline" return still present in some lowering signatures is vestigial: it is always `true`.

## Register mapping

| Role | x86-64 | aarch64 |
| --- | --- | --- |
| `&mut Executor` | `rbx` | `x19` |
| `&mut Globals` | `r12` | `x20` |
| Program counter (VM tier) | `r13` | `x21` |
| Local frame pointer (LFP) | `r14` | `x22` |
| Accumulator (VM tier only) | `r15` | `x23` |
| `self`'s ivar buffer in JIT code (`SELF_IVAR_BASE`) | `r13` | `x24` |
| Scratch for lowering temps | — | `x9`–`x15` |

JIT code keeps no fixed accumulator. Inside it `r13` (aarch64: `x24`) caches the buffer of `self`'s heap ivar table across a run of instructions that cannot grow it.

The C-call ABIs differ (arguments in `rdi/rsi/rdx/…` vs `x0..x7`), so call-argument lowering shuffles into the ABI registers explicitly rather than using a 1:1 map.

## Remaining differences

Correctness and instruction coverage are identical across the two backends. What differs is mechanism, not coverage: x86-64 installs and re-points compiled code by patching branches in place while aarch64 goes through indirect dispatch words; cold handlers sit on a separate code page on x86-64 but in in-stream side-exit islands on aarch64 (page 1 is out of `B`/`BL` range); and a very large loop body can exceed aarch64's conditional-branch range, which leaves that loop interpreted. These are catalogued in [`doc/arch_difference.md`](design/arch_difference.md).

## Building and testing on aarch64

- On Apple Silicon macOS, a normal `cargo build` produces a native binary (Homebrew `libffi` + `pkg-config` are required — see the target-specific dependency block in `monoruby/Cargo.toml`).
- From an x86-64 Linux host, `bin/setup-aarch64-cross` sets up a cross toolchain and `bin/test-aarch64` runs the test suite under emulation.
- CI runs the test scope natively on `macos-latest` (Apple Silicon) for every push and pull request; the manual `gc-stress` workflow also runs on native arm64 Linux (`ubuntu-24.04-arm`).
