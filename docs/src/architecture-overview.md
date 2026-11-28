monoruby is a Ruby implementation written from scratch in Rust, featuring a register-based bytecode VM and a just-in-time (JIT) compiler. It has no dependency on CRuby or any other Ruby runtime. This page gives a bird's-eye view of the system; each section links to a dedicated chapter and to the detailed design documents (rendered in the "Design Documents" section of this book, sourced from the repository's [`doc/`](https://github.com/sisshiki1969/monoruby/tree/master/doc) directory).

## Compilation pipeline

```
Ruby source
    │
    ▼
prism (ruby-prism)      prism syntax tree — the official Ruby parser
    │
    ▼
parser/ + ast/          monoruby AST
    │
    ▼
bytecodegen/            register-based bytecode
    │
    ▼
Executor (VM)           interpreted execution, machine-code VM tier
    │  when hot (≥20 calls / ≥100 loop iterations)
    ▼
JIT: TraceIR            type-annotated IR built from inline-cache feedback
    │
    ▼
JIT: AsmIR              register-allocated, arch-neutral assembly IR
    │
    ▼
codegen/arch/<arch>     AsmIR → machine code (x86-64 / aarch64 backends)
    │
    ▼
monoasm                 self-made dynamic assembler
    │
    ▼
Native machine code
```

Ruby source is parsed by [prism](https://github.com/ruby/prism) (consumed as the `ruby-prism` crate) and converted into monoruby's own AST. The AST is compiled into register-based bytecode, which the VM executes. Hot methods (≥ 20 calls) and hot loops (≥ 100 iterations) are handed to the JIT, which uses runtime type feedback to produce specialized machine code, falling back to the VM through deoptimization when its assumptions are invalidated. See [JIT Compiler](jit-compiler.md) for details.

## Execution tiers

- **VM tier** — the bytecode executor. Its dispatch loop and operation handlers are themselves emitted as machine code through monoasm (per target architecture), rather than being a Rust `match` loop.
- **JIT tier** — specialized machine code per method / loop, guarded by type and class-version checks. Both x86-64 and aarch64 lower the full instruction set; see [aarch64 Backend](aarch64-backend.md).

## Major subsystems

| Subsystem | Chapter | Design documents |
| --- | --- | --- |
| Value representation (64-bit tagged union) | [Value Representation](value-representation.md) | — |
| JIT compiler (TraceIR / AsmIR / register allocation) | [JIT Compiler](jit-compiler.md) | [`jit.md`](design/jit.md), [`lir.md`](design/lir.md), [`jit_invariants.md`](design/jit_invariants.md) |
| Garbage collection (generational mark-and-sweep) | [Garbage Collection](garbage-collection.md) | [`gc.md`](design/gc.md) |
| Threads and fibers | [Threads and Fibers](threads-and-fibers.md) | [`threads.md`](design/threads.md) |
| Stack frames and method calls | [Stack Frames and Method Calls](stack-frames-and-method-calls.md) | [`stack_frame.md`](design/stack_frame.md), [`method_args.md`](design/method_args.md) |
| Exception handling | [Exception Handling](exception-handling.md) | [`exception_handling.md`](design/exception_handling.md) |
| aarch64 (Apple Silicon) backend | [aarch64 Backend](aarch64-backend.md) | [`arch_difference.md`](design/arch_difference.md) |

## Source layout

```
monoruby/                   workspace root
├── monoruby/src/
│   ├── parser/, ast/       prism → monoruby-AST bridge, AST definitions
│   ├── bytecodegen/        AST → register-based bytecode
│   ├── executor/           bytecode interpreter (VM), frames, operator dispatch
│   ├── codegen/            JIT compiler
│   │   ├── jitgen/         bytecode → TraceIR → AsmIR (arch-neutral front-end)
│   │   └── arch/           per-arch backends: x86_64/ and aarch64/
│   ├── value.rs, value/    Value type and heap objects (RValue)
│   ├── alloc.rs            garbage collector
│   ├── globals/            global interpreter state, function/class tables
│   └── builtins/           built-in Ruby classes implemented in Rust
├── monoruby/builtins/      built-in library code written in Ruby
├── monoruby_attr/          proc macros (#[monoruby_builtin], …)
├── monoruby_ext_sys/, monoruby_ext/   C ABI (and safe Rust over it) for dynamically loaded extensions
├── monoruby_capi/          CRuby-compatible C API layer, and CRuby's own stdlib C extensions built on it
├── ext/                    native halves of gems (sqlite3, zstd, psych, nokogiri, …) as loadable extensions
├── rubymap/, hashbrown/    order-preserving hash map for Ruby Hash
└── doc/                    detailed design documents
```

## Key global registers (JIT / VM tier)

Machine code keeps interpreter state in fixed registers:

| x86-64 | aarch64 | Holds |
| --- | --- | --- |
| `rbx` | `x19` | `&mut Executor` |
| `r12` | `x20` | `&mut Globals` |
| `r13` | `x21` | program counter (VM tier) |
| `r14` | `x22` | local frame pointer (LFP) |
| `r15` | `x23` | accumulator (VM tier only) |

Inside JIT code `r13` (aarch64: `x24`) instead caches the buffer of `self`'s heap ivar table (`SELF_IVAR_BASE`); compiled code keeps no fixed accumulator.

## Further reading

- [Installation and Build](installation.md) — how to build and run monoruby
- [Development and Build Options](development.md) — bytecode / TraceIR / assembly dumps and the other Cargo features
- [`doc/` directory](https://github.com/sisshiki1969/monoruby/tree/master/doc) — the full set of design documents, indexed in its `README.md`
