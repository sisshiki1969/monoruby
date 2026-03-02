# CLAUDE.md — monoruby

A comprehensive guide for AI assistants working on this repository.

## Project Overview

**monoruby** is a Ruby implementation written from scratch in Rust, featuring a hand-written parser, a register-based bytecode VM, and a just-in-time (JIT) compiler targeting x86-64 Linux. It is performance-focused and is comparable to ruby 3.4+YJIT on the optcarrot benchmark.

- **Platform**: x86-64 Linux **only** (assembly-level VM and JIT)
- **Rust channel**: Nightly (`nightly-2025-10-15`, pinned in `rust-toolchain.toml`)
- **No dependency on CRuby** or any other Ruby runtime

---

## Repository Layout

```
monoruby/                   # Workspace root
├── monoruby/               # Main crate — the Ruby interpreter & JIT
│   ├── src/
│   │   ├── main.rs         # CLI entry point (monoruby binary)
│   │   ├── lib.rs          # Library root; re-exports public API
│   │   ├── alloc.rs        # Custom GC allocator (mark-and-sweep)
│   │   ├── id_table.rs     # Interned identifier table (IdentId)
│   │   ├── value.rs        # Value type (NaN-boxed, 64-bit)
│   │   ├── value/
│   │   │   ├── numeric.rs  # Numeric helpers (Fixnum/Float/BigInt)
│   │   │   └── rvalue/     # Heap-allocated Ruby values (RValue)
│   │   ├── bytecode.rs     # Bytecode instruction index types (BcIndex)
│   │   ├── bytecodegen/    # AST → register-based bytecode compiler
│   │   │   ├── bytecodegen.rs
│   │   │   ├── inst.rs     # Bytecode instruction definitions
│   │   │   ├── expression.rs
│   │   │   ├── statement.rs
│   │   │   └── method_call/
│   │   ├── executor/       # Bytecode interpreter (VM)
│   │   │   ├── executor.rs
│   │   │   ├── frame.rs    # Stack/control frame layout
│   │   │   ├── op/         # Operator dispatch
│   │   │   ├── inline.rs   # Inline method dispatch table
│   │   │   └── constants.rs
│   │   ├── codegen/        # JIT compiler
│   │   │   ├── codegen.rs  # Thread-local CODEGEN singleton
│   │   │   ├── compiler.rs # JIT compilation entry point
│   │   │   ├── jit_module.rs
│   │   │   ├── invoker.rs  # Method/block/fiber invokers
│   │   │   ├── vmgen/      # x86-64 VM dispatch code generation
│   │   │   ├── runtime/    # JIT runtime helpers
│   │   │   └── jitgen/     # Bytecode → TraceIR → AsmIR → x86-64
│   │   │       ├── trace_ir.rs
│   │   │       ├── state/  # Abstract interpreter state
│   │   │       ├── asmir/  # Assembly IR definitions & lowering
│   │   │       └── compile.rs
│   │   ├── globals/        # Global interpreter state
│   │   │   ├── globals.rs  # Globals struct (main_object, Store, …)
│   │   │   ├── store/      # Function, class, and call-site tables
│   │   │   ├── error.rs    # MonorubyErr type
│   │   │   ├── method.rs   # Method lookup helpers
│   │   │   └── require.rs  # File loading (`require`/`load`)
│   │   ├── builtins/       # Built-in Ruby class implementations
│   │   │   ├── builtins.rs # init_builtins() — registers all classes
│   │   │   ├── array.rs, string.rs, hash.rs, numeric/, …
│   │   │   └── …
│   │   └── tests.rs        # Test harness helpers
│   └── build.rs            # Build script (sets up library path file)
│
├── ruruby-parse/           # Hand-written Ruby parser (produces AST)
├── monoruby_attr/          # Proc-macro crate (#[monoruby_builtin], etc.)
├── rubymap/                # Order-preserving hash map for Ruby Hash
├── hashbrown/              # Vendored hashbrown (local fork)
├── ruby_traits/            # Shared trait definitions
├── rust-smallvec/          # Vendored SmallVec (local fork)
│
├── benchmark/              # Ruby benchmark scripts + YAML configs
├── bin/                    # Shell helper scripts
│   ├── test                # Full test + coverage run (used in CI)
│   ├── bench               # Benchmark runner
│   └── irm                 # Launch REPL (cargo run --bin irm)
├── doc/                    # Architecture documentation
│   ├── jit_architecture.md # Detailed JIT pipeline doc
│   ├── jit.md              # JIT stub code details
│   └── method_args.md      # Method argument handling
├── Cargo.toml              # Workspace manifest
└── rust-toolchain.toml     # Pins nightly-2025-10-15
```

---

## Architecture Deep Dive

### Compilation Pipeline

```
Ruby source
    │
    ▼
ruruby-parse          (AST)
    │
    ▼
bytecodegen           (register-based bytecode)
    │
    ▼
Executor (VM)         (interpreted execution)
    │  when hot (≥20 calls / ≥100 loop iters)
    ▼
JIT: TraceIR          (type-annotated IR from inline caches)
    │
    ▼
JIT: AsmIR            (register-allocated assembly IR)
    │
    ▼
monoasm               (dynamic x86-64 assembler, external crate)
    │
    ▼
Native x86-64 code
```

### Value Representation (`value.rs`)

`Value` is a NaN-boxed 64-bit non-zero integer. Immediate types are embedded in the bits:

| Tag | Type |
|-----|------|
| `0x04` | `nil` |
| `0x14` | `false` |
| `0x1c` | `true` |
| `0x0c` | Symbol |
| Low bit = 1 | Fixnum (i63, shifted) |
| Specific NaN pattern | Float |
| Pointer (aligned) | Heap object (`RValue`) |

### Global State (`globals.rs`)

`Globals` is the top-level interpreter state, holding:
- `store: Store` — function/class/call-site tables
- `global_vars` — Ruby global variables
- `invokers` — pre-generated code for method/block/fiber entry
- `load_path`, `loaded_canonicalized_files` — `require` bookkeeping
- `random: Prng` — Mersenne Twister PRNG

`Globals` derefs to `Store`, so `Store` methods are callable directly on `Globals`.

### Executor (`executor.rs`)

The `Executor` struct is the bytecode interpreter. Key fields:
- `cfp: Option<Cfp>` — current control frame pointer
- `rsp_save` — native stack save for fiber switching

Stack frame offsets (from `executor.rs`):
- `LFP_OUTER` (`+0`), `LFP_META` (`+8`), `LFP_BLOCK` (`+16`), `LFP_SELF` (`+24`), `LFP_ARG0` (`+32`)

Global registers in JIT-compiled code:
| Register | Holds |
|----------|-------|
| `rbx` | `&mut Executor` |
| `r12` | `&mut Globals` |
| `r13` | Program counter |
| `r14` | Local frame pointer (LFP) |
| `r15` | Accumulator |

### JIT Compiler (`codegen/`)

**Threshold values**:
- Method JIT: ≥ 20 calls (`COUNT_START_COMPILE`, 5 in test mode)
- Loop JIT: ≥ 100 iterations (`COUNT_LOOP_START_COMPILE`, 15 in test mode)

**`CODEGEN`** is a thread-local `RefCell<Codegen>` singleton.

**Deoptimization** falls back to the interpreter when:
1. A type guard fails (type changed at runtime)
2. Class version mismatch (class was modified)
3. BOP (basic-op) redefinition

**Recompilation** reasons: `NotCached`, `MethodNotFound`, `IvarIdNotFound`.

### Built-in Methods (`builtins/`)

Built-in methods are Rust functions with the signature:

```rust
fn foo(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value>
```

The `#[monoruby_builtin]` proc-macro (from `monoruby_attr`) wraps them in an `extern "C"` trampoline that converts `Result<Value>` to `Option<Value>` and installs error handling.

Registration happens in `builtins/builtins.rs` → `init_builtins()`.

### Custom GC (`alloc.rs`)

- Mark-and-sweep garbage collector
- Page-based arena allocator for `RValue` objects (256 KB pages)
- `GC_THRESHOLD`, `MALLOC_THRESHOLD` control collection frequency
- Thread-local `ALLOC` holds the allocator instance
- Controlled via `--no-gc` flag or `Globals::gc_enable()`

---

## Workspace Crates

| Crate | Purpose |
|-------|---------|
| `monoruby` | Main interpreter + JIT |
| `ruruby-parse` | Hand-written Ruby parser → AST |
| `monoruby_attr` | Proc macros: `#[monoruby_builtin]`, `#[monoruby_object]` |
| `rubymap` | Order-preserving Ruby-compatible HashMap/Set |
| `hashbrown` | Vendored hash table (local fork) |
| `ruby_traits` | Shared trait definitions |
| `rust-smallvec` | Vendored SmallVec with const-generics (local fork) |

External crates (fetched from git):
- `monoasm` / `monoasm_macro` — dynamic x86-64 assembler
- `onigmo-regex` — Onigmo regular expression engine

---

## Development Workflows

### Building

```sh
# Debug build (opt-level = 1 per workspace Cargo.toml)
cargo build

# Release build (for benchmarking)
cargo build --release

# Run a Ruby file
cargo run --release -- path/to/script.rb

# Launch REPL
cargo run --bin irm
# or
bin/irm
```

### Testing

```sh
# Run unit/integration tests (cargo-nextest required in CI)
cargo test

# Run the full CI test suite (also runs benchmarks + coverage)
bin/test
```

`bin/test` does:
1. `cargo llvm-cov nextest` with all debug features enabled
2. Builds a debug binary with `gc-stress`
3. Runs benchmark scripts and `diff`s output against CRuby 3.4.1
4. Runs optcarrot and compares output
5. Generates `lcov.info` coverage report

**Test harness** (`tests.rs`):

```rust
run_test(code)       // runs code 25× (JIT warms up), compares result with CRuby
run_test_once(code)  // runs code once, compares result with CRuby
run_tests(codes)     // runs multiple expressions, compares each with CRuby
```

All test helpers invoke CRuby via `ruby` in `PATH` and assert output equality.

### Cargo Features

| Feature | Effect |
|---------|--------|
| `dump-bc` | Dump bytecode to stderr |
| `dump-traceir` | Dump TraceIR to stderr |
| `emit-asm` | Dump generated assembly (implies `dump-bc`, `dump-traceir`, `jit-log`) |
| `emit-bc` | Emit bytecode (implies `dump-bc`, `dump-traceir`) |
| `emit-cfg` | Emit CFG in DOT format |
| `jit-log` | Log JIT compilation events |
| `jit-debug` | Detailed JIT debug output |
| `no-jit` | Disable JIT (interpreter only) |
| `deopt` | Log deoptimizations |
| `gc-log` | Log GC statistics at exit |
| `gc-debug` | GC debug assertions |
| `gc-stress` | GC on every allocation (stress test) |
| `profile` | Collect deopt/recompile statistics |
| `perf` | Emit perf-compatible symbol maps |

### Cargo Config

`.cargo/config.toml` sets `-Cforce-frame-pointers=yes` globally (required for `perf` profiling).

---

## Key Conventions

### Identifier Naming

- `IdentId` — interned string ID used for variable/method/constant names; use `IdentId::get_id("name")` to intern
- `FuncId` — unique ID for a function/method body
- `ClassId` — unique ID for a class
- `IvarId` — unique ID for an instance variable slot within a class
- `SlotId` — bytecode register index
- `BcIndex` — bytecode instruction index
- `CallSiteId` — call site identifier (used for inline caches)

### Error Handling

- `Result<T>` in monoruby is `std::result::Result<T, MonorubyErr>`
- `MonorubyErr` is defined in `globals/error.rs`
- Built-in functions return `Result<Value>` and are wrapped by the `#[monoruby_builtin]` macro
- JIT/VM native functions return `Option<Value>` where `None` means an error was set via `vm.set_error()`

### Adding a Built-in Method

1. Implement the function in the appropriate `builtins/*.rs` file:
   ```rust
   #[monoruby_builtin]
   fn method_name(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
       // ...
   }
   ```
2. Register it in `init_builtins()` (or the class initializer) using helpers like:
   - `globals.define_builtin_func(class_id, "method_name", method_name, arity)`
   - `globals.define_builtin_func_rest(class_id, "method_name", method_name)`
   - `globals.define_builtin_class_func(class_id, "method_name", method_name, arity)`

### Safety Conventions

- `unsafe` blocks for raw pointer operations are expected in the VM, JIT, and frame code
- Always include a `// SAFETY:` comment explaining why the usage is sound
- Avoid introducing `unsafe` outside of these subsystems

### Nightly Features in Use

The following nightly features are enabled in `monoruby/src/lib.rs`:
- `box_patterns`
- `int_roundings`
- `iter_next_chunk`
- `step_trait`
- `iter_array_chunks`
- `let_chains` (used in proc-macro)

Do not add new nightly features without necessity; prefer stable alternatives when available.

---

## CI / GitHub Actions

File: `.github/workflows/rust.yml`

Triggered on push/PR to `master`. Steps:
1. Install Ruby 3.4.1 (used to validate monoruby output)
2. Install Rust nightly via `dtolnay/rust-toolchain@nightly`
3. Install `cargo-llvm-cov` and `cargo-nextest`
4. Install ImageMagick (for PPM→JPG conversion in aobench test)
5. Clone optcarrot benchmark
6. Run `bin/test` (full test + coverage)
7. Upload `lcov.info` to Codecov

---

## Benchmarking

```sh
# Release benchmark against a specific script
cargo run --release -- benchmark/app_fib.rb

# Use bin/bench for structured benchmarks
bin/bench

# Performance profiling with Linux perf (requires `perf` feature)
cargo build --release --features perf
perf record target/release/monoruby benchmark/app_fib.rb
perf report
```

Benchmark scripts live in `benchmark/`. YAML files (`*.yaml`, `*.yml`) contain benchmark configuration for the benchmark runner.

---

## Submodule / Vendored Dependencies

The repository vendors several dependencies as local paths rather than crates.io:
- `hashbrown/` — local fork
- `rust-smallvec/` — local fork with const-generics feature
- `ruruby-parse/` — developed in tandem with monoruby

When modifying these, be aware changes affect the whole workspace.

---

## Common Gotchas

1. **Nightly only**: Attempting to build with stable Rust will fail. The toolchain is pinned in `rust-toolchain.toml`.
2. **x86-64 Linux only**: The VM interpreter and JIT emit x86-64 assembly directly. No other architecture is supported.
3. **Ruby in PATH**: Tests compare output against a system `ruby` binary (3.4.1). Ensure Ruby is installed and the binary is accessible.
4. **optcarrot**: The full CI test requires optcarrot cloned at `../optcarrot` relative to the repo root.
5. **Library path**: At startup, monoruby reads `~/.monoruby/library_path` and `~/.monoruby/ruby_version` to configure `$LOAD_PATH` and `RUBY_VERSION`. Without these files a warning is printed.
6. **gc-stress in tests**: The `bin/test` script builds with `--features gc-stress` to catch GC bugs; this makes the binary much slower than a normal debug build.
7. **Thread-local CODEGEN**: The JIT compiler is a thread-local singleton. Do not attempt to use it across threads.
