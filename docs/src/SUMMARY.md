# Summary

[Introduction](introduction.md)

# Architecture

- [Architecture Overview](architecture-overview.md)
- [Value Representation](value-representation.md)
- [JIT Compiler](jit-compiler.md)
- [Garbage Collection](garbage-collection.md)
- [Threads and Fibers](threads-and-fibers.md)
- [Stack Frames and Method Calls](stack-frames-and-method-calls.md)
- [Exception Handling](exception-handling.md)
- [aarch64 Backend](aarch64-backend.md)

# Design Documents

- [Stub code for JIT'ed code](design/jit.md)
- [Unified low-level IR (LIR)](design/lir.md)
- [Separating the abstract interpreter from register allocation](design/regalloc_separation.md)
- [Trace-wide abstract state and chain joins](design/trace_chain_joins.md)
- [Inline asm functions](design/inline.md)
- [JIT argument forwarding (Japanese)](design/arg_forwarding_jit.md)
- [x86-64 / aarch64 JIT backend differences](design/arch_difference.md)
- [Invariants compiled code speculates on (Japanese)](design/jit_invariants.md)
- [Version guards, salvage and the megamorphic gate](design/jit_invalidation.md)
- [Basic-op redefinition (Japanese)](design/bop_redefinition.md)
- [Chain deoptimization](design/chain_deopt.md)
- [Reading the deopt log](design/deopt_log.md)
- [Receiver-polymorphic call sites (Japanese)](design/polymorphic_call.md)

---

- [GC — mechanism and implementation (Japanese)](design/gc.md)
- [Thread / Fiber / non-blocking IO / preemption (Japanese)](design/threads.md)
- [Thread / Fiber state transition diagrams (Japanese)](design/scheduler_state_diagram.md)
- [Safepoints (Japanese)](design/safepoint.md)
- [Signal handling (Japanese)](design/signal.md)

---

- [Exception handling — mechanism and CRuby contrast](design/exception_handling.md)
- [Stack layout for the interpreter / JIT'ed code](design/stack_frame.md)
- [Method argument processing (Japanese)](design/method_args.md)
- [Native (builtin) function registration](design/native_func.md)
- [CREF — Class / Constant Reference](design/cref.md)
- [Refinements and method resolution](design/refinements.md)
- [TOPLEVEL_BINDING and the main script](design/toplevel_binding.md)
- [super resolution via the caller PC (Japanese)](design/super_resolution.md)

---

- [Per-encoding character iteration design](design/encoding_char_iteration_design.md)
- [C extension support — design notes (Japanese)](design/c_extention.md)
- [ruby/spec hang countermeasures (Japanese)](design/ruby_spec_skip_tags.md)
- [optcarrot --opt profile (Japanese)](design/optcarrot_opt_profile.md)
- [Progress summary (April 2025 – April 2026)](design/progress_2025-2026.md)
