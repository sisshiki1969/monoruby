# CLAUDE.md — monoruby

A comprehensive guide for AI assistants working on this repository.

## Project Overview

**monoruby** is a Ruby implementation written from scratch in Rust, featuring a register-based bytecode VM and a just-in-time (JIT) compiler. It is performance-focused and is comparable to ruby 3.4+YJIT on the optcarrot benchmark.

- **Parser**: Ruby source is parsed by **prism** (the official Ruby parser, consumed as the `ruby-prism` crate); monoruby converts prism's tree into its own AST (`monoruby/src/ast/`, `monoruby/src/parser/`). The old hand-written `ruruby-parse` crate has been removed.
- **Platform**: x86-64 **and** aarch64. The VM-tier backend (bytecode VM, invokers, native-function wrappers) and the JIT emit machine code directly via `monoasm`, with a per-`target_arch` backend under `codegen/arch/`. Both backends lower the **full** AsmInst set to machine code; aarch64 materializes large frame/field/sp offsets through scratch registers rather than bailing, so it never declines a compile (`compile_asmir`'s `bool` return is vestigial — see `doc/arch_difference.md`). Tested on Linux/x86-64 and macOS Apple Silicon (arm64-apple-darwin).
- **Rust channel**: Nightly (`nightly-2026-05-18`, pinned in `rust-toolchain.toml`)
- **No dependency on CRuby** or any other Ruby runtime

---

## Repository Layout

```
monoruby/                   # Workspace root
├── monoruby/               # Main crate — the Ruby interpreter & JIT
│   ├── src/
│   │   ├── main.rs         # CLI entry point (monoruby binary)
│   │   ├── lib.rs          # Library root; re-exports public API
│   │   ├── parser/         # prism → monoruby-AST bridge
│   │   │   ├── mod.rs
│   │   │   └── prism_backend.rs # Drives the `ruby-prism` crate
│   │   ├── ast/            # monoruby AST (node, lvar_collector, source_info, error)
│   │   ├── alloc.rs        # Custom GC allocator (mark-and-sweep)
│   │   ├── id_table.rs     # Interned identifier table (IdentId)
│   │   ├── value.rs        # Value type (tagged-union, 64-bit, NonZeroU64)
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
│   │   ├── codegen/        # JIT compiler + arch-neutral codegen glue
│   │   │   ├── codegen.rs  # Thread-local CODEGEN singleton; arch-neutral types
│   │   │   ├── compiler.rs # JIT compilation entry point
│   │   │   ├── jit_module.rs # Arch-neutral: handle_error, ErrorReturn, …
│   │   │   ├── arch.rs     # target_arch switch (x86_64 / aarch64)
│   │   │   ├── arch/       # Per-arch backends (VM-tier + JIT emission), mirrored
│   │   │   │   ├── x86_64/ # {codegen,jit_module,invoker,wrapper,vmgen(+vmgen/),
│   │   │   │   │           #  compile(+compile/),guard}  ← JIT lowering: compile,guard
│   │   │   │   └── aarch64/# {codegen,jit_module,invoker,wrapper,vmgen,compile,guard}
│   │   │   ├── runtime/    # JIT runtime helpers
│   │   │   └── jitgen/     # Bytecode → TraceIR → AsmIR (arch-neutral front-end)
│   │   │       ├── trace_ir.rs
│   │   │       ├── state/  # Abstract interpreter state
│   │   │       ├── asmir/  # AsmIR defs + arch-neutral lowering dispatch (compile_shared)
│   │   │       └── compile.rs # TraceIR → AsmIR (the per-arch AsmIR→machine-code
│   │   │                      # backend lives in arch/<arch>/compile, guard)
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
├── monoruby_attr/          # Proc-macro crate (#[monoruby_builtin], etc.)
├── rubymap/                # Order-preserving hash map for Ruby Hash
├── hashbrown/              # Vendored hashbrown (local fork)
├── ruby_traits/            # Shared trait definitions
│
├── benchmark/              # Ruby benchmark scripts + YAML configs
├── bin/                    # Shell helper scripts
│   ├── test                # Full test + coverage run (used in CI)
│   ├── test-aarch64        # aarch64 test run (qemu / cross)
│   ├── setup-aarch64-cross # Set up the aarch64 cross-build toolchain
│   ├── bench               # Benchmark runner
│   ├── refresh-prism-vendored # Rebuild/force-push the prism vendored branch
│   └── irm                 # Launch REPL (cargo run --bin irm)
├── doc/                    # Architecture documentation
│   ├── jit.md              # JIT stub code details
│   ├── method_args.md      # Method argument handling
│   └── progress_2025-2026.md # Progress notes
├── Cargo.toml              # Workspace manifest
└── rust-toolchain.toml     # Pins nightly-2026-05-18
```

---

## Development Environment Setup

### 1. Installing CRuby (rbenv)

The monoruby test harness compares output against CRuby, so CRuby 4.0 or later is required. To install via rbenv:
CRuby 4.0 or later can be obtained from the [ruby/ruby](https://github.com/ruby/ruby) repository on GitHub.

> **Important:** Starting with Ruby 3.4, `bigdecimal` is no longer a default
> gem. The monoruby `bigdecimal_*` integration tests `require "bigdecimal"`
> against the system Ruby, so install it explicitly after installing Ruby 4:
>
> ```sh
> gem install bigdecimal
> ```
>
> Without this, every `tests/bigdecimal.rs` test will fail because the
> reference Ruby process exits with `LoadError: cannot load such file --
bigdecimal`.

### 2. Verifying Ruby Version

Confirm that the `ruby` command launches CRuby 4.0 or later:

```sh
ruby --version
# => ruby 4.0.1 (2025-xx-xx ...) — must be 4.0 or later
```

`build.rs` uses this `ruby` binary at build time to capture `$LOAD_PATH` and `RUBY_VERSION`. Tests will fail if `ruby` is not in `PATH`.

### 3. Setting Up and Running ruby/spec

ruby/spec (mspec) is cloned outside the monoruby repository:

```sh
# Clone into the parent directory of monoruby
cd /path/to/parent-of-monoruby
git clone --depth 1 https://github.com/ruby/spec.git spec
git clone --depth 1 https://github.com/ruby/mspec.git mspec
```

Expected directory layout:

```
parent/
├── monoruby/    # this repository
├── spec/        # ruby/spec
└── mspec/       # mspec test runner
```

#### Running core category specs

```sh
# Build and install monoruby in release mode
cd monoruby
cargo install --path monoruby

# Run a specific category (e.g., core/array)
cd ../spec
../mspec/bin/mspec run core/array -t monoruby

# Run a single spec file
../mspec/bin/mspec run core/array/flatten_spec.rb -t monoruby

# Show results in dotted format
../mspec/bin/mspec run core/array -t monoruby --format dotted

# Run all core categories at once (bin/spec script)
cd ../monoruby
bin/spec
```

> **Note**: `core/io/copy_stream_spec.rb`, `core/io/select_spec.rb`, and `core/kernel/readlines_spec.rb` hang under monoruby's single-threaded runtime and are excluded by `bin/spec`.

---

## Architecture Deep Dive

### Compilation Pipeline

```
Ruby source
    │
    ▼
prism (ruby-prism)    (prism syntax tree)
    │
    ▼
parser/ + ast/        (monoruby AST)
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
JIT: AsmIR            (register-allocated, arch-neutral assembly IR)
    │
    ▼
codegen/arch/<arch>   (AsmIR → machine code; both arches lower the full AsmInst set)
    │
    ▼
monoasm               (dynamic assembler, external crate)
    │
    ▼
Native machine code   (x86-64 / aarch64)
```

### Value Representation (`value.rs`)

`Value` is a 64-bit non-zero integer using a **tagged-union** scheme: the lower 3 bits encode the kind of value. It is **not** NaN-boxing.

#### Dispatch on lower 3 bits

| Lower bits (`& 0b111`)             | Kind                                                             |
| ---------------------------------- | ---------------------------------------------------------------- |
| `???????1` (bit 0 = 1)             | **Fixnum** — integer stored in bits 63:1 as i63 (`value >> 1`)   |
| `??????10` (bits 1:0 = `10`)       | **Flonum** — double-precision float encoded inline (bit-rotated) |
| `??????00` (bits 2:0 = `000`)      | **Heap pointer** — raw pointer to a GC-managed `RValue`          |
| Other (bit 2 = 1, bits 1:0 ≠ `10`) | **Other immediate** — nil / true / false / Symbol                |

The method `is_packed_value()` tests `bits & 0b0111 != 0`; if true, the value is an immediate and `try_rvalue()` returns `None`. If false, the bits are a valid `*const RValue` pointer.

#### Immediate tag constants

| Constant      | Hex    | Binary      | Meaning                                  |
| ------------- | ------ | ----------- | ---------------------------------------- |
| `NIL_VALUE`   | `0x04` | `0000_0100` | `nil`                                    |
| `FALSE_VALUE` | `0x14` | `0001_0100` | `false`                                  |
| `TRUE_VALUE`  | `0x1c` | `0001_1100` | `true`                                   |
| `TAG_SYMBOL`  | `0x0c` | `0000_1100` | Symbol (IdentId packed in upper 32 bits) |

`FLOAT_ZERO` (`(0b1000 << 60) | 0b10`) is the flonum encoding of `0.0`.

Floats that cannot be represented as a flonum (exponent out of range) are heap-allocated as `RValue` objects of class `Float`.

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

Global registers in JIT-compiled code (x86-64; the aarch64 backend uses an
equivalent fixed register assignment in `codegen/arch/aarch64/`):
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
fn foo(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value>
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

Workspace members (`Cargo.toml`): `monoruby`, `monoruby_attr`, `rubymap`, `hashbrown`, `ruby_traits`.

| Crate           | Purpose                                                  |
| --------------- | -------------------------------------------------------- |
| `monoruby`      | Main interpreter + JIT (includes the prism→AST bridge)   |
| `monoruby_attr` | Proc macros: `#[monoruby_builtin]`, `#[monoruby_object]` |
| `rubymap`       | Order-preserving Ruby-compatible HashMap/Set             |
| `hashbrown`     | Vendored hash table (local fork)                         |
| `ruby_traits`   | Shared trait definitions                                 |

External crates (fetched from git):

- `monoasm` / `monoasm_macro` — dynamic assembler (`aarch` branch; x86-64 + aarch64)
- `onigmo-regex` — Onigmo regular expression engine
- `ruby-prism` — prism parser bindings (pinned `monoruby-vendored` branch; see below)
- `smallvec` — local fork with `const_generics` (pinned via git, not vendored in-tree)

---

## Development Workflows

### Build Script (`build.rs`)

`monoruby/build.rs` runs at **every `cargo build`** and performs two jobs:

1. **Capture CRuby metadata** — Executes the system `ruby` binary to query `$LOAD_PATH` and `RUBY_VERSION`, writing the results to:
   - `~/.monoruby/library_path` — newline-separated list of CRuby's stdlib directories
   - `~/.monoruby/ruby_version` — CRuby's version string (e.g. `3.4.1`)

   At runtime, monoruby reads these files to set `$LOAD_PATH` and `RUBY_VERSION` so that `require` resolves to CRuby's standard library. If `ruby` is not in `PATH`, a warning is printed and the files are not written.

2. **Install Ruby library stubs** — Recursively copies two source directories into `~/.monoruby/`:
   - `monoruby/startup/` → `~/.monoruby/` — Ruby files that are loaded automatically at interpreter start (e.g. `startup.rb`, `enumerable.rb`, `comparable.rb`, `integer.rb`, `range.rb`, …)
   - `monoruby/builtins/` → `~/.monoruby/builtins/` — additional built-in Ruby files (e.g. `array.rb`, `builtins.rb`)

   These files implement parts of the Ruby standard library in Ruby rather than Rust.

> **Note**: Because `build.rs` has no `cargo:rerun-if-changed` directive for the startup files (the line is commented out), changes to files in `startup/` or `builtins/` will **not** automatically trigger a rebuild. Run `touch monoruby/build.rs` or `cargo build` after editing them to force re-installation.

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

| Feature             | Effect                                                                 |
| ------------------- | ---------------------------------------------------------------------- |
| `dump-bc`           | Dump bytecode to stderr                                                |
| `dump-traceir`      | Dump TraceIR to stderr                                                 |
| `emit-asm`          | Dump generated assembly (implies `dump-bc`, `dump-traceir`, `jit-log`) |
| `emit-bc`           | Emit bytecode (implies `dump-bc`, `dump-traceir`)                      |
| `emit-cfg`          | Emit CFG in DOT format (implies `dump-bc`, `dump-traceir`)             |
| `jit-log`           | Log JIT compilation events                                             |
| `jit-debug`         | Detailed JIT debug output (implies `dump-traceir`)                     |
| `deopt`             | Log deoptimizations (implies `jit-log`, `dump-bc`, `dump-traceir`)     |
| `gc-log`            | Log GC statistics at exit                                              |
| `gc-debug`          | GC debug assertions                                                    |
| `gc-stress`         | GC on every allocation (stress test)                                   |
| `stress-spill-pool` | Shrink `PHYS_FPR_POOL` to 2 to stress the FP-register spill paths      |
| `profile`           | Collect deopt/recompile statistics (implies `dump-bc`, `dump-traceir`) |
| `perf`              | Emit perf-compatible symbol maps                                       |
| `dump-require`      | Log `require`/`load` file resolution                                   |

The JIT is always compiled in regardless of features; it is disabled at runtime
with the `--no-jit` flag. The old `jit`/`jit_x86` build cfgs and the `no-jit`
feature have been removed — backend selection is purely by `target_arch`.

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
- `iter_next_chunk`
- `step_trait`
- `coverage_attribute`

Do not add new nightly features without necessity; prefer stable alternatives when available.

---

## CI / GitHub Actions

File: `.github/workflows/rust.yml`

Triggered on push/PR to `master`. Two jobs run:

- **Linux/x86-64** (`ubuntu-latest`): install Ruby 4.0+, Rust nightly,
  `cargo-llvm-cov` + `cargo-nextest`, clone optcarrot / ruby-bench / ruby-spec,
  run `bin/test` (full test + coverage), upload `lcov.info` to Codecov.
- **macOS Apple Silicon** (`darwin`, `macos-latest`): native arm64-apple-darwin
  runner exercising the VM + AArch64 JIT. Installs `bigdecimal`, `libffi` +
  `pkg-config` (Homebrew libffi is required on aarch64 macOS — see the
  target-specific dep block in `monoruby/Cargo.toml`), then runs the test scope.

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

## Vendored Dependencies

The repository vendors `hashbrown/` as a local-path fork (a workspace member).
`smallvec` is a local fork too, but is now consumed as a **git** dependency
(`sisshiki1969/rust-smallvec`, `const_generics` feature) rather than an in-tree
path. The old in-tree `rust-smallvec/` and `ruruby-parse/` directories have been
removed.

The `ruby-prism` Rust wrapper is consumed as a `git` dependency against the
`monoruby-vendored` branch of [`sisshiki1969/prism`](https://github.com/sisshiki1969/prism).
That fork is split into two branches:

- `monoruby` — minimal upstream-bound diff (Rust `parse_with_options` API +
  `ruby-prism-sys` bindgen allowlist additions). Used as the base for any
  upstream PR to `ruby/prism`.
- `monoruby-vendored` — `monoruby` plus a single commit that checks in
  `rust/ruby-prism{,-sys}/vendor/prism-{ver}/` (the C source the upstream
  `vendored.rs` build script needs). This is the branch `monoruby/Cargo.toml`
  pins, so consumers don't need bundler or `rake cargo:build` locally.

To bump the prism rev: push the change to the fork's `monoruby` branch,
run `bin/refresh-prism-vendored` (rebuilds and force-pushes
`monoruby-vendored`), then `cargo update -p ruby-prism` in this repo.

---

## Common Gotchas

1. **Nightly only**: Attempting to build with stable Rust will fail. The toolchain is pinned in `rust-toolchain.toml`.
2. **Architecture-specific backends**: The VM and JIT emit machine code directly per `target_arch` (`codegen/arch/{x86_64,aarch64}/`). Both backends lower the full AsmInst set; aarch64 never bails (large immediates go through scratch registers, so the `bool` "decline" return is vestigial — see `doc/arch_difference.md`). Adding/altering low-level codegen usually means touching both backends. Use `bin/test-aarch64` / `bin/setup-aarch64-cross` for the aarch64 path.
3. **Ruby in PATH**: Tests compare output against a system `ruby` binary (3.4.1). Ensure Ruby is installed and the binary is accessible.
4. **optcarrot**: The full CI test requires optcarrot cloned at `../optcarrot` relative to the repo root.
5. **Library path**: `build.rs` writes `~/.monoruby/library_path` and `~/.monoruby/ruby_version` by running the system `ruby` binary at build time. At runtime, monoruby reads these files to configure `$LOAD_PATH` and `RUBY_VERSION`. If `ruby` was absent at build time, these files will be missing and a warning is printed at startup.
6. **gc-stress in tests**: The `bin/test` script builds with `--features gc-stress` to catch GC bugs; this makes the binary much slower than a normal debug build.
7. **Thread-local CODEGEN**: The JIT compiler is a thread-local singleton. Do not attempt to use it across threads.
