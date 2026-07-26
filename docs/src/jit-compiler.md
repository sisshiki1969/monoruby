monoruby executes bytecode in the VM until code gets hot, then compiles it to specialized machine code. This page is an overview; the detailed documents are [`doc/jit.md`](design/jit.md) (stub/bridge code), [`doc/lir.md`](design/lir.md) (the low-level IR), [`doc/regalloc_separation.md`](design/regalloc_separation.md) (register allocation), and [`doc/inline.md`](design/inline.md) (inline builtins).

## When compilation triggers

- **Method JIT** — after ≥ 20 calls (`COUNT_START_COMPILE`; 5 in test mode)
- **Loop JIT** — after ≥ 100 iterations of a loop (`COUNT_LOOP_START_COMPILE`; 15 in test mode), compiling the enclosing method from the loop entry

Each function starts with a small *wrapper* that decrements a counter and falls through to the VM until the counter expires, then triggers compilation and patches itself.

## IR pipeline

```
bytecode ──abstract interpretation──▶ TraceIR ──▶ AsmIR ──▶ LIR ──▶ machine code (monoasm)
                (type feedback from inline caches)        (per-arch encoder)
```

- **TraceIR** — bytecode annotated with type information gathered from the VM tier's inline caches.
- **AsmIR** (`AsmInst`) — arch-neutral, register-allocated assembly IR produced by an abstract interpreter that tracks, per slot, whether a value lives on the stack, in a floating-point register (unboxed `f64`), both, or is a compile-time constant (`LinkMode`).
- **LIR** (`LInst`) — arch-neutral machine-level ops with offsets and labels resolved; the single seam where bytes are emitted. Each architecture implements one `encode_linst` (see [aarch64 Backend](aarch64-backend.md)).

## Specialization and inline caches

Compiled code is specialized **per receiver class**. The method entry is a chain of self-class guard stubs: each guard tests the receiver's class and jumps to the machine code compiled for that class; a miss falls through to the next guard or to the VM. Method calls inside JIT code are resolved through inline caches and guarded by a **class-version** check, so redefining a method invalidates dependent code. Small hot builtins (`Array#[]`, `Integer` arithmetic, `Math.sqrt`, `Object#is_a?`, `Struct` accessors, `Class#new`, `Fiber.yield`, …) are inlined directly by generator functions that can both consult the abstract state (folding results at compile time when types are proven) and emit code; a failed inline attempt rolls back cleanly and falls back to a normal call. Monomorphic methods can additionally be **specialized inline** — the callee's frame is inlined into the caller.

## Deoptimization and recompilation

JIT code is speculative. Guards — receiver class, class version, array type, frozen state, fixnum overflow, basic-operator (BOP) redefinition, frame capture — branch to **side exits** that write register-resident values back to the frame and resume in the VM at the equivalent program point. Repeated deopts trigger **recompilation** with the newly observed classes (e.g. polymorphic call sites, `method_missing` dispatch, newly resolved constants/ivars). Deopt logging is available with the `deopt` Cargo feature, recompile/deopt statistics with `profile`.

## Register allocation

- **Floating point** — virtual FP registers (`VirtFPReg`) allocated greedily over the physical pool (14 xmm registers on x86-64), with automatic **spill-to-stack** when the pool is exhausted; loop entries specialize float-typed slots so hot numeric loops keep values unboxed in registers.
- **General purpose** — a per-basic-block local GP register allocator keeps boxed values (notably Fixnums) in a small pool of scratch registers within a block, eliding redundant fixnum guards, and flushes the pool at calls and GC safepoints (pool registers are not GC roots).

The long-form design discussion — separating type inference from placement, the retirement of the dedicated accumulator register, and measured results — is in [`doc/regalloc_separation.md`](design/regalloc_separation.md) and [`doc/lir.md`](design/lir.md).

## Argument forwarding (D1)

`def f(...)` forwarding is compiled as an opaque pipe: for simple callees the rest-Array / keyword-Hash allocation is elided entirely and arguments are copied (or lazily deferred) straight from the caller's frame, with deopt-safe lazy materialization if a side exit or frame capture ever needs the real objects. See [`doc/arg_forwarding_jit.md`](design/arg_forwarding_jit.md).

## Observing the JIT

| Cargo feature | Output |
| --- | --- |
| `dump-bc` / `emit-bc` | bytecode |
| `dump-traceir` | TraceIR |
| `emit-asm` | generated assembly |
| `jit-log` / `jit-debug` | compilation events / detailed debug |
| `deopt` | deoptimization log |
| `profile` | deopt & recompile statistics |
| `perf` | perf-compatible symbol maps |

The JIT is always built in; disable it at runtime with `--no-jit`. See [Build options for performance tuning](https://github.com/sisshiki1969/monoruby/wiki/Build-options-for-performance-tuning) for example output.
