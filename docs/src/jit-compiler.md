monoruby executes bytecode in the VM until code gets hot, then compiles it to specialized machine code. This page is an overview; the detailed documents are [`doc/jit.md`](design/jit.md) (stub code), [`doc/lir.md`](design/lir.md) (the low-level IR), [`doc/inline.md`](design/inline.md) (inline builtins), [`doc/jit_invariants.md`](design/jit_invariants.md) (what compiled code speculates on) and [`doc/jit_invalidation.md`](design/jit_invalidation.md) (version guards and salvage).

## When compilation triggers

- **Method JIT** — after ≥ 20 calls (`COUNT_START_COMPILE`; 5 in test mode)
- **Loop JIT** — after ≥ 100 iterations of a loop (`COUNT_LOOP_START_COMPILE`; 15 in test mode), compiling the enclosing method from the loop entry

Each function starts behind a small *wrapper* that decrements a counter and enters the VM until the counter expires, then compiles the function and patches the wrapper to jump to the compiled code.

## IR pipeline

```
bytecode ──abstract interpretation──▶ TraceIR ──▶ AsmIR ──▶ LIR / emit_* ──▶ machine code (monoasm)
                (type feedback from inline caches)              (per-arch encoder)
```

- **TraceIR** — bytecode annotated with type information gathered from the VM tier's inline caches.
- **AsmIR** (`AsmInst`) — arch-neutral, register-allocated assembly IR produced by an abstract interpreter that tracks, per slot, whether a value lives on the stack, in a floating-point register (unboxed `f64`), both, or is a compile-time constant (`LinkMode`).
- **LIR** (`LInst`) — arch-neutral machine-level ops with logical addressing, encoded by each architecture's `encode_linst`. Moves, memory, ALU, branches, guards, integer and float arithmetic and side exits go through it; an `AsmInst` whose lowering is a single per-arch runtime-call helper calls that `emit_*` helper directly (see [aarch64 Backend](aarch64-backend.md)).

## Specialization and inline caches

Compiled code is specialized **per receiver class**. The method entry is a chain of self-class guard stubs: each guard tests the receiver's class and jumps to the machine code compiled for that class; a class that misses the whole chain runs in the VM, and gets a compiled body of its own only after the stub has seen it repeatedly (the *megamorphic gate*). Method calls inside JIT code are resolved from inline caches and guarded by a **class-version** check.

Small hot builtins (`Array#[]`, `Integer` arithmetic, `Math.sqrt`, `Object#is_a?`, `Struct` accessors, `Class#new`, `Fiber.yield`, …) are inlined directly by generator functions that can both consult the abstract state (folding results at compile time when types are proven) and emit code; a failed inline attempt rolls back cleanly and falls back to a normal call. Ruby callees can be **specialized** for the call site, and small loop-free callees, `Class#new`'s `initialize` and small `yield` blocks run **frameless**, inline in a window of the caller's frame.

Array and Range literals consumed where they are built (`[a, b].max`, `(lo..hi).include?(x)`, `when 1..5`) and `*rest` / `...` arguments handed into specialized callees are not allocated at all — see [Escape analysis](design/escape_analysis.md).

## Deoptimization, salvage and recompilation

JIT code is speculative. Guards — receiver class, class version, constant version, array type, frozen state, fixnum overflow, basic-operator (BOP) redefinition, frame capture — branch to **side exits** that write register-resident values back to the frame and resume in the VM at the equivalent program point. A side exit also converts the suspended JIT frames of the same compilation unit into interpreter frames ([chain deopt](design/chain_deopt.md)).

The class and constant version counters are global, so any `def` or constant assignment anywhere moves them. A version-guard failure therefore first re-validates what the compilation unit actually assumed (its resolved call sites, its folded constants); if every assumption still holds, the unit is stamped with the current version and the code stands (**salvage**). Only a genuine change, or a guard that keeps missing for a new reason (a polymorphic site, a newly resolved ivar or constant), triggers **recompilation**. BOP redefinition is not salvaged: it evicts the dependent code ([Basic-op redefinition](design/bop_redefinition.md)).

Deopt logging is available with the `deopt` Cargo feature, recompile/deopt statistics with `profile`; [Reading the deopt log](design/deopt_log.md) explains the format.

## Register allocation

- **Floating point** — virtual FP registers (`FPReg`) allocated greedily over a pool of 14 physical registers, with automatic **spill-to-stack** when the pool is exhausted; loop entries specialize float-typed slots so hot numeric loops keep values unboxed in registers.
- **General purpose** — a per-basic-block local allocator (`jitgen/gp_alloc.rs`) keeps Fixnum operands and results in a small register set (`r8`–`r11` on x86-64) within a block, reusing a register when a slot it already holds is read again and eliding redundant Fixnum guards. Every other operation, and every block boundary, flushes the residents to their frame homes; pool registers are not GC roots, so only Fixnums live there.

JIT code keeps no fixed accumulator register; `r15` is the VM tier's accumulator only.

## Argument forwarding

`def f(...)` forwarding is compiled as an opaque pipe: for simple callees the rest-Array / keyword-Hash allocation is elided entirely and arguments are copied (or lazily deferred) straight from the caller's frame, with deopt-safe lazy materialization if a side exit or frame capture ever needs the real objects. See [`doc/arg_forwarding_jit.md`](design/arg_forwarding_jit.md).

## Observing the JIT

| Cargo feature | Output |
| --- | --- |
| `dump-bc` / `emit-bc` | bytecode |
| `dump-traceir` | TraceIR |
| `emit-asm` | generated assembly |
| `emit-cfg` | control-flow graph per compiled function (DOT) |
| `jit-log` / `jit-debug` | compilation events / detailed debug |
| `deopt` | deoptimization log |
| `profile` | deopt & recompile statistics |
| `perf` | perf-compatible symbol maps |

The JIT is always built in; disable it at runtime with `--no-jit`. [Development and Build Options](development.md#worked-example-reading-the-pipeline) walks through the dumps for one function.
