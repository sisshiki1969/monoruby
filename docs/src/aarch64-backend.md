monoruby runs natively on **aarch64** — macOS on Apple Silicon is fully supported (VM tier + JIT), and CI runs on GitHub's Apple Silicon runners. The aarch64 backend was ported in mid-2026 and lowers the **complete** instruction set: it never declines a compilation. This page is an overview; the detailed comparison is [`doc/arch_difference.md`](design/arch_difference.md).

## How the backends are organized

```
codegen/jitgen/                 arch-neutral front-end: bytecode → TraceIR → AsmIR
codegen/jitgen/asmir/           arch-neutral lowering dispatcher (compile_asmir → LIR)
codegen/arch/x86_64/            x86-64 backend: VM tier, invokers, wrappers, encode_linst
codegen/arch/aarch64/           aarch64 backend: same structure, mirrored file layout
```

Everything above machine-code emission is shared. The arch-neutral dispatcher lowers each `AsmInst` either through common code paths built on small per-arch emission primitives (`emit_reg_move`, `emit_guard_class`, `emit_integer_binop`, …) or into arch-neutral **LIR**, which each architecture encodes with its own `encode_linst` (selected by `cfg(target_arch)`, no dynamic dispatch). Machine code is emitted with the [monoasm](https://github.com/sisshiki1969/monoasm) dynamic assembler, which provides both `monoasm!` (x86-64) and `monoasm_arm64!` DSLs.

## Full coverage — no bail

Historically the aarch64 port could "bail" (fall back to the VM) on instructions it didn't yet support — almost always because an offset didn't fit AArch64's 12-bit immediate encodings. Today every `AsmInst` and every side exit is lowered: displacements that fit are folded into `ldur`/`stur`/scaled `ldr`/`str`, and larger frame/field/sp offsets are **materialized through reserved scratch registers** (`x9`/`x10`). The `bool` "decline" return still present in some lowering signatures is vestigial.

## Register mapping

| Role | x86-64 | aarch64 |
| --- | --- | --- |
| `&mut Executor` | `rbx` | `x19` |
| `&mut Globals` | `r12` | `x20` |
| Program counter | `r13` | `x21` |
| Local frame pointer (LFP) | `r14` | `x22` |
| (former accumulator slot) | `r15` | `x23` |
| Scratch for lowering temps | — | `x9`–`x15` |

The C-call ABIs differ (arguments in `rdi/rsi/rdx/…` vs `x0..x7`), so call-argument lowering shuffles into the ABI registers explicitly rather than using a 1:1 map.

## Remaining differences

Correctness and instruction coverage are identical across the two backends; the few remaining asymmetries only affect *transition costs* around recompilation (e.g. how a class-version guard miss recovers: x86-64 patches and recompiles in place in some paths where aarch64 deopts and re-JITs via warm-up counters). These are catalogued, with rationale, in [`doc/arch_difference.md`](design/arch_difference.md).

## Building and testing on aarch64

- On Apple Silicon macOS, a normal `cargo build` produces a native binary (Homebrew `libffi` + `pkg-config` are required — see the target-specific dependency block in `monoruby/Cargo.toml`).
- From an x86-64 Linux host, `bin/setup-aarch64-cross` sets up a cross toolchain and `bin/test-aarch64` runs the test suite under emulation.
- CI runs the full test scope natively on `macos-latest` (Apple Silicon) for every push and pull request.
