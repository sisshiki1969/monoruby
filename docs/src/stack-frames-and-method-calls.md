This page describes how monoruby lays out call frames and processes method arguments. Details: [`doc/stack_frame.md`](design/stack_frame.md), [`doc/method_args.md`](design/method_args.md), [`doc/cref.md`](design/cref.md), [`doc/super_resolution.md`](design/super_resolution.md).

## Frame layout

Each Ruby-level call pushes three contiguous regions on the native stack (growing downward):

- **Continuation frame** — the caller's saved `lfp`, `pc`, return address, and `rbp`. The saved call-site pc is also what powers lazy backtraces, `Kernel#caller`, and `super` resolution.
- **Control frame (CFP)** — `prev cfp` and `lfp`; the executor's `cfp` chain links all active frames.
- **Local frame (LFP)** — the Ruby-visible part: `outer` (for blocks: the enclosing frame), `meta`, `block`, `self`, then the argument/local slots `arg0, arg1, …`.

The bytecode interpreter and JIT code share a fixed register ABI on x86-64 (the aarch64 backend uses an equivalent assignment): `rbx` = `&mut Executor`, `r12` = `&mut Globals`, `r13` = pc, `r14` = lfp.

Frames captured by blocks, Procs, or Bindings are **promoted to the heap lazily** — only when the capture actually escapes — and heap frames are reclaimed by the GC once unreachable.

## Argument processing

Formal parameters occupy frame slots in a fixed order: `required | optional | rest | keyword | block | destructured-children`. At call time the **caller** copies positional arguments into the callee frame, expanding splats, gathering overflow into `rest`, filling missing slots with `nil`/none-markers, and checking arity. Keyword arguments are then assigned by name, with surplus keywords gathered into the keyword-rest slot; if the callee accepts no keywords at all, trailing keywords are packed into a Hash and passed as one extra positional argument. Blocks additionally auto-splat a single Array argument when they take multiple parameters. The **callee prologue** (`InitMethod`) then links the frame, homes arguments, nil-fills the remaining slots — and runs the safepoint poll. Destructuring and optional-parameter default initializers are compiled as ordinary bytecode at the top of the method body.

Built-in (native) methods declare their arity and keyword names at registration time (e.g. `define_builtin_func_with_kw(..., min, max, rest, kw)`), and receive their arguments in the same fixed slot order via `Lfp`. The `#[monoruby_builtin]` proc macro wraps a Rust `fn(vm, globals, lfp) -> Result<Value>` into the VM's calling convention, converting errors into the VM's error protocol (`vm.set_error`).

## Lexical scope (CREF)

Where `def`, constant lookup, and visibility land is decided by monoruby's CREF model — a compact 16-byte `Cref` struct kept on a VM-wide stack, distinguishing the *definition* context (used by `def`) from the *lexical* context (used by `module`/`class` nesting and unqualified constants). This is monoruby's counterpart to CRuby's per-frame `rb_cref_t` chain; [`doc/cref.md`](design/cref.md) contrasts the two models in depth, including how `class_eval` / `instance_eval` / `Kernel#eval` push their scopes.

## `super` resolution

CRuby frames carry a callable-method-entry that tells `super` both the method name to search and where in the ancestor chain to continue. monoruby frames carry only a `FuncId`, so `super` **reconstructs** that information from the caller pc saved in the continuation frame: it recovers the call site's opcode and call-site info, derives the originally-called name (correct across `alias` and multi-name `define_method`), and counts how many times the running body occurs in the ancestor chain so a method aliased into several places supers past the right occurrence. See [`doc/super_resolution.md`](design/super_resolution.md).

## Further reading

- [`doc/stack_frame.md`](design/stack_frame.md) — exact slot offsets, pre-call vs post-prologue layouts
- [`doc/method_args.md`](design/method_args.md) — caller/callee argument-processing responsibilities
- [`doc/native_func.md`](design/native_func.md) — builtin registration and argument slots
