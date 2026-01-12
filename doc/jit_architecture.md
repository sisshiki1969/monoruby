# monoruby JIT Compiler Architecture

## Overview

The monoruby JIT compiler is a multi-stage compiler that transforms Ruby code into efficient x86-64 native code. It uses [monoasm](https://github.com/sisshiki1969/monoasm), a custom dynamic assembler developed for this project.

## Compilation Pipeline

```
┌─────────────┐    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌─────────────┐
│ Ruby Source │───>│   Parser     │───>│  Bytecode    │───>│   TraceIR    │───>│   AsmIR     │
│   (.rb)     │    │(ruruby-parse)│    │  (BcOp)      │    │              │    │             │
└─────────────┘    └──────────────┘    └──────────────┘    └──────────────┘    └─────────────┘
                                                                                      │
                                                                                      v
                                                                              ┌─────────────┐
                                                                              │   x86-64    │
                                                                              │ Native Code │
                                                                              └─────────────┘
```

### 1. Parser (ruruby-parse)

A custom Ruby parser implementation that converts Ruby source code into an AST (Abstract Syntax Tree).

**Location**: `ruruby-parse/`

### 2. Bytecode Generation (bytecodegen)

Transforms the AST into register-based bytecode.

**Location**: `monoruby/src/bytecodegen/`

**Key Files**:
- `bytecodegen.rs` - Entry point for bytecode generation
- `inst.rs` - Bytecode instruction definitions
- `expression.rs` - Expression compilation
- `statement.rs` - Statement compilation

### 3. TraceIR Generation

An intermediate representation that augments bytecode with type information from inline caches. This serves as input to the JIT compiler.

**Location**: `monoruby/src/codegen/jitgen/trace_ir.rs`

**Key Instructions**:
```rust
enum TraceIr {
    // Control flow
    Br(BasicBlockId),
    CondBr(SlotId, BasicBlockId, bool, BrKind),

    // Literals
    Integer(SlotId, i32),
    Symbol(SlotId, IdentId),
    Literal(SlotId, Value),

    // Variable access
    LoadIvar(SlotId, IdentId, Option<(ClassId, IvarId)>),
    StoreIvar(SlotId, IdentId, Option<(ClassId, IvarId)>),

    // Operations
    BinOp { kind, dst, lhs, rhs, ic },
    UnOp { kind, dst, src, ic },

    // Method calls
    MethodCall { callid, cache },
    Yield { callid },

    // Returns
    Ret(SlotId),
    MethodRet(SlotId),
    ...
}
```

### 4. AsmIR Generation

Transforms TraceIR into an assembly-like intermediate representation. Register allocation and stack management are performed at this stage.

**Location**: `monoruby/src/codegen/jitgen/asmir.rs`

**Key Instruction Categories**:
- **Register operations**: `RegMove`, `RegAdd`, `RegSub`
- **Type guards**: `GuardClass`, `GuardArrayTy`, `GuardClassVersion`
- **Method calls**: `Call`, `SpecializedCall`, `Yield`
- **Deoptimization**: `Deopt`, `RecompileDeopt`
- **XMM operations**: `XmmSave`, `XmmRestore`, `FloatToXmm`

### 5. Native Code Generation

Converts AsmIR to x86-64 native code using monoasm.

**Location**: `monoruby/src/codegen/jitgen/asmir/compile.rs`

## Key Components

### Codegen (`codegen.rs`)

The main JIT compiler structure. It exists as a thread-local singleton.

```rust
thread_local! {
    pub static CODEGEN: RefCell<Codegen> = RefCell::new(Codegen::new());
}
```

**Key Fields**:
- `jit: JitModule` - Native code generator
- `compilation_unit: Vec<CompilationUnitInfo>` - Compilation unit information
- `return_addr_table` - Return address table (for immediate eviction)
- `method_invoker` - Method invocation handler

### JitContext (`context.rs`)

Holds context information during JIT compilation.

```rust
struct JitContext<'a> {
    store: &'a Store,
    frame_stack: Vec<JitStackFrame>,
    // ...
}

enum JitType {
    Entry,                              // Method/block JIT
    Specialized { idx, args_info },     // Specialized JIT
    Loop(BytecodePtr),                  // Loop JIT
}
```

### AbstractState (`state.rs`)

Tracks the abstract state during compilation. Manages type information and register allocation state for each slot.

```rust
struct SlotState {
    slots: Vec<LinkMode>,
    // ...
}

enum LinkMode {
    None,           // Undefined
    MaybeNone,      // Possibly undefined
    S(SlotMode),    // On stack
    C(Value),       // Compile-time constant
    G(GP),          // General-purpose register
    Sf(Xmm, ...),   // XMM register (floating point)
}
```

## JIT Compilation Strategies

### Method JIT

Compiles entire methods. Compilation starts when the call count exceeds the threshold (default: 20 calls).

```rust
const COUNT_START_COMPILE: i32 = 20;  // 5 in test mode
```

### Loop JIT

Detects and compiles hot loops. Compilation starts when the loop execution count exceeds the threshold (default: 100 iterations).

```rust
const COUNT_LOOP_START_COMPILE: i32 = 100;  // 15 in test mode
```

**Important**: Loop JIT is skipped if the local frame is captured (checked via `branch_if_captured`).

### Specialized Compilation

Generates code specialized for receiver classes during method inlining.

## Deoptimization

Falls back to the interpreter when type guards fail or method redefinition is detected.

### Deoptimization Types

1. **Type Guard Failure**: When a type guard fails
2. **Class Version Mismatch**: When a class definition changes
3. **BOP Redefinition**: When basic operations (+, -, *, etc.) are redefined

### WriteBack

Writes register values back to the stack during deoptimization.

```rust
struct WriteBack {
    xmm: Vec<(Xmm, Vec<SlotId>)>,    // XMM register -> slots
    literal: Vec<(Value, SlotId)>,   // Literal -> slot
    r15: Option<SlotId>,             // R15 (accumulator) -> slot
    void: Vec<SlotId>,               // nil -> slots
}
```

## Register Conventions

### Global Registers
| Register | Usage |
|----------|-------|
| `rbx` | `&mut Executor` |
| `r12` | `&mut Globals` |
| `r13` | PC (Program Counter) |
| `r14` | LFP (Local Frame Pointer) |
| `r15` | Accumulator / Temporary register |

### XMM Registers
Used for floating-point operations. `xmm2` through `xmm15` are available.

## Recompilation

Recompilation occurs when type information is updated or inline cache misses.

```rust
enum RecompileReason {
    NotCached,          // Cache miss
    MethodNotFound,     // Method not found
    IvarIdNotFound,     // Instance variable ID not found
}
```

## Source Files Overview

```
monoruby/src/codegen/
├── codegen.rs              # Main Codegen structure (1358 lines)
├── compiler.rs             # Compilation entry point (436 lines)
├── jit_module.rs           # JIT module (381 lines)
├── vmgen.rs                # VM code generation (1331 lines)
├── runtime.rs              # Runtime functions (954 lines)
├── invoker.rs              # Method/block invocation (674 lines)
└── jitgen/
    ├── jitgen.rs           # JIT context (798 lines)
    ├── compile.rs          # TraceIR -> AsmIR conversion (1108 lines)
    ├── asmir.rs            # AsmIR definitions (1679 lines)
    ├── context.rs          # JIT context management (963 lines)
    ├── state.rs            # Abstract state management (518 lines)
    ├── trace_ir.rs         # TraceIR definitions (752 lines)
    ├── method_call.rs      # Method call compilation (799 lines)
    ├── binary_op.rs        # Binary operation compilation (482 lines)
    ├── basic_block.rs      # Basic block management (202 lines)
    └── asmir/
        └── compile.rs      # AsmIR -> x86-64 conversion
```

## Optimization Techniques

### 1. Type Specialization

Generates specialized code based on type information.

```rust
// Integer addition
if lhs_class == INTEGER_CLASS && rhs_class == INTEGER_CLASS {
    // Fast path for Fixnum
}
```

### 2. Inline Caching

Uses method caches to skip method lookup.

```rust
struct MethodCacheEntry {
    recv_class: ClassId,
    func_id: FuncId,
    version: u32,
}
```

### 3. Constant Folding

Folds constants at compile time.

```rust
if let Some(src) = state.is_fixnum_literal(src) {
    // Compute at compile time
    state.def_C(dst, Value::fixnum(-src));
}
```

### 4. Register Allocation

Utilizes XMM registers to speed up floating-point operations.

### 5. Method Inlining

Inlines frequently called methods.

## Debug Features

### Compile-time Features

| Feature | Description |
|---------|-------------|
| `jit-log` | Output JIT compilation logs |
| `jit-debug` | Output detailed debug information |
| `emit-asm` | Output generated assembly |
| `emit-bc` | Output bytecode |
| `emit-cfg` | Output CFG (Control Flow Graph) in DOT format |
| `perf` | Output symbols for perf profiling |

## Performance Characteristics

- **Warm-up**: Method JIT starts after 20 calls, Loop JIT after 100 iterations
- **Recompilation**: Progressively optimizes based on updated type information
- **Deoptimization**: Falls back to interpreter on type guard failure
- **Immediate Eviction**: Immediately invalidates JIT code on BOP redefinition

## See Also

- [doc/jit.md](./jit.md) - JIT stub code details
- [doc/stack_frame.md](./stack_frame.md) - Stack frame structure
- [doc/inline.md](./inline.md) - Inlining details
