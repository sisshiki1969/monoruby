# Development and Build Options

This page collects the build features, environment variables and helper
scripts used when working *on* monoruby, rather than with it. For getting a
working binary in the first place see [Installation and
Build](installation.md).

## Cargo features

Every feature is off by default. They are diagnostic or stress switches: none
of them changes what a correct program computes, and none is needed for a
normal build.

The JIT is **always** compiled in regardless of features — it is disabled at
run time with `--no-jit`. (The old `jit` / `jit_x86` build cfgs and the
`no-jit` feature are gone; backend selection is purely by `target_arch`.)

### Dumping the compilation pipeline

| Feature | Effect |
| --- | --- |
| `emit-bc` | Dump bytecode to stderr (implies `dump-bc`, `dump-traceir`) |
| `emit-asm` | Dump generated machine code to stderr (implies `dump-bc`, `dump-traceir`, `jit-log`) |
| `emit-cfg` | Write each JIT-compiled function's control-flow graph to `.cfg/fid-<id>.dot` (implies `dump-bc`, `dump-traceir`) |
| `dump-bc` | Enable the bytecode dumper |
| `dump-traceir` | Enable the TraceIR dumper |
| `dump-require` | Log `require` / `load` file resolution |

### JIT diagnostics

| Feature | Effect |
| --- | --- |
| `jit-log` | Log JIT compilation events |
| `jit-debug` | Detailed JIT debug output (implies `dump-traceir`) |
| `deopt` | Log deoptimizations (implies `jit-log`, `dump-bc`, `dump-traceir`) |
| `chain-deopt-log` | Trace every chain-deopt escalation to stderr. Deliberately **not** implied by `deopt` / `profile`: it fires hundreds of thousands of times per activerecord iteration and once wrote 160 MB of stderr in five iterations |
| `profile` | Collect deopt / recompile statistics (implies `dump-traceir` for the deopt-site table, but not the bytecode dumps) |
| `perf` | Emit perf-compatible symbol maps so JIT frames get names |

### GC

| Feature | Effect |
| --- | --- |
| `gc-log` | Log GC statistics at exit |
| `gc-debug` | GC debug assertions |
| `gc-stress` | Start in `GC.stress` and collect at **every** safepoint |
| `gc-verify` | After every minor GC, independently re-mark the whole live graph from the roots, so a missed write barrier trips an assertion. Debug-only and very slow |

### Register allocation and allocator experiments

| Feature | Effect |
| --- | --- |
| `stress-spill-pool` | Shrink `PHYS_FPR_POOL` to 2 so almost every float-resident slot becomes a spilled virtual FP register, stressing the spill paths |
| `shadow-placement` | Record every physical FP placement in emission order, producing a per-compile fingerprint of the lowering. The gate for the abstract-interpreter / register-allocation separation |
| `phys-table` | Move the physical FP placement *policy* out of the resolver into an explicit table-backed function. Byte-identical to the formula it replaces |
| `phys-loop-aware` | The loop-aware FP allocation policy: keep loop-carried floats resident so a fresh value spills instead of evicting one. Non-byte-identical by design |
| `mimalloc` | Route the global allocator's delegation to mimalloc instead of glibc, changing exactly one variable for an A/B |

The last three are gated on measurement and are off until their A/B clears;
see [Separating the abstract interpreter from register
allocation](design/regalloc_separation.md).

## Worked example: reading the pipeline

Everyone loves Fibonacci, so all three dumps below are of the same
`benchmark/app_fib.rb`:

```ruby
def fib n
  if n < 3
    1
  else
    fib(n-1) + fib(n-2)
  end
end

puts fib(34)
```

### Bytecode (`--features emit-bc`)

```sh
cargo build --release --features emit-bc
target/release/monoruby benchmark/app_fib.rb 2> fib.bytecode > /dev/null
```

Each method and block is dumped as a register-based instruction listing,
grouped into basic blocks (`BBx`). `%n` is a bytecode register (`SlotId`);
`_%n` marks a result the next instruction consumes directly without the value
ever being stored. The bracketed columns on the right are inline-cache slots,
shown as `<INVALID>` here because the dump happens at compile time, before any
cache has been filled.

```text
<fib> benchmark/app_fib.rb:1
FuncId(3835) SIMPLE stack reg_num:5 owner:[] local_vars:1 temp:3
ParamsInfo { required_num: 1, optional_num: 0, rest: None, rest_is_implicit: false, post_num: 0, args_names: [Some(n)], kw_names: [], kw_required: [], kw_rest: None, block_param: None, forwarding: false, it_param: false, forbid_keyword: false }
[]
  BB0
    :00000 [02] init_method reg:4 arg:1 stack_offset:8
    :00001 [03] %2 = 3
    :00002 [03] _%2 = %1 < %2                        [<INVALID>][<INVALID>]
    :00003 [02] condnotbr _%2 => BB2
  BB1
    :00004 [03] %2 = 1
    :00005 [02] ret %2
  BB2
    :00006 [03] %2 = 1
    :00007 [03] %2 = %1 - %2                         [<INVALID>][<INVALID>]
    :00008 [03] %2 = %0.fib(%2)                      [<INVALID>] -
    :00010 [04] %3 = 2
    :00011 [04] %3 = %1 - %3                         [<INVALID>][<INVALID>]
    :00012 [04] %3 = %0.fib(%3)                      [<INVALID>] -
    :00014 [03] %2 = %2 + %3                         [<INVALID>][<INVALID>]
    :00015 [02] ret %2
```

`%0` is `self`, `%1` is the parameter `n`, and `[nn]` is the source line.
See [Method argument processing](design/method_args.md) for what the
`ParamsInfo` counters mean.

### JIT-compiled machine code (`--features emit-asm`)

```sh
cargo build --release --features emit-asm
target/release/monoruby benchmark/app_fib.rb 2> fib.disas > /dev/null
```

Each bytecode instruction is printed with the machine code it lowered to, so
the dump reads as an annotated disassembly. The bracketed columns are now the
*resolved* inline caches — `[Integer][Integer]` for the arithmetic, and
`[#<Class:main>] FuncId(3835)` for the recursive call — because by the time
the JIT runs, the VM has executed the method often enough to fill them. That
is exactly the type information the compiler speculates on; see [Invariants
compiled code speculates on](design/jit_invariants.md).

```text
==> start whole compile: FuncId(3835) <Object#fib> self_class: #<Class:main> benchmark/app_fib.rb:1
  >>> [0] ISeqId(2164) <Object#fib> self_class:#<Class:main>
      offset:Pos(251180) code: 517 bytes  data: 0 bytes
  BB0
    :00000 init_method reg:4 arg:1 stack_offset:8
      000000: push   rbp
      000001: mov    rbp,rsp
      000004: sub    rsp,0x80
      00000b: movabs rax,0x4
      000015: mov    QWORD PTR [rbp-0x48],rax
      000019: mov    QWORD PTR [rbp-0x50],rax
      00001d: mov    QWORD PTR [rbp-0x58],rax
      000021: cmp    DWORD PTR [rip+0x7ffc2ad4],0x0        # 0x7ffc2afc
      000028: jne    0x3ffc60be
    :00001 %2 = 3
    :00002 _%2 = %1 < %2                        [Integer][Integer]
      00002e: mov    r8,QWORD PTR [rbp-0x40]
      000032: test   r8,0x1
      000039: je     0x3ffc60e9
      00003f: cmp    r8,0x7
      000043: jge    0x55
    :00003 condnotbr _%2 => BB2
  BB1
    :00004 %2 = 1
    :00005 ret %2
      000049: movabs rax,0x3
      000053: leave
      000054: ret
  BB2
    :00006 %2 = 1
    :00007 %2 = %1 - %2                         [Integer][Integer]
      000055: mov    r8,QWORD PTR [rbp-0x40]
      000059: sub    r8,0x2
      00005d: jo     0x3ffc61ce
    :00008 %2 = %0.fib(%2)                      [#<Class:main>] FuncId(3835)
      000063: mov    eax,DWORD PTR [rip+0x7ffc2a6b]        # 0x7ffc2ad4
      000069: cmp    eax,DWORD PTR [rip+0xffffffffffffff8d]        # 0xfffffffc
      00006f: jne    0x3ffc61dd
      000075: cmp    rsp,QWORD PTR [rbx+0x18]
      000079: jle    0x3ffc6344

      … call sequence and the second recursive call elided …

    :00014 %2 = %2 + %3                         [Integer][Integer]
      0001d5: mov    r9,QWORD PTR [rbp-0x48]
      0001d9: test   r9,0x1
      0001e0: je     0x3ffc6540
      0001e6: test   r8,0x1
      0001ed: je     0x3ffc6548
      0001f3: sub    r9,0x1
      0001f7: add    r9,r8
      0001fa: jo     0x3ffc6550
    :00015 ret %2
      000200: mov    rax,r9
      000203: leave
      000204: ret
  <<<
Object#fib #<Class:main> None (522 bytes, 1223 bytes) [wm0:CodePtr(139715286390055)-CodePtr(139715286390577) wm1:CodePtr(139716359894468)-CodePtr(139716359895691)] 5.462877ms
- [ISeqId(2164)] <Object#fib> self_class:#<Class:main>

```

Things worth reading out of that listing:

- **The guards are the speculation.** `test r8,0x1` / `je` is the Fixnum tag
  check on `n`; the `jo` after each `sub` / `add` catches overflow out of the
  63-bit Fixnum range; and `movl rax,[rip+global_version]` /
  `cmpl rax,[rip+cached_version]` / `jne` before each call is the class-version
  guard. Every one of those branch targets is a side exit.
- **Fixnums are tagged, and the constants are pre-encoded.** `n < 3` compiles
  to `cmp r8,0x7` because `3` as a Fixnum is `3 << 1 | 1`; `n - 1` is
  `sub r8,0x2`; and the base case returns `movabs rax,0x3`, which is `1`.
  The final `sub r9,0x1; add r9,r8` strips one tag bit before adding. See
  [Value Representation](value-representation.md).
- **No accumulator register.** Values live in whatever GP register the
  per-basic-block allocator picked (`r8` and `r9` here), with the frame at
  `[rbp-…]`. The fixed `r15` accumulator was retired in June 2026.
- **`cmp rsp,[rbx+0x18]` before each call** is the stack-limit check; `rbx`
  holds `&mut Executor`.

The summary line reports the code size and the two code regions the compiler
emitted into: `wm0` is the fast path, `wm1` the out-of-line page holding the
recompile and deopt stubs each failing guard jumps to.

### CFG (`--features emit-cfg`)

`emit-cfg` writes one DOT file per JIT-compiled function into a `.cfg/`
directory under the current working directory, named by `FuncId`:

```sh
cargo build --release --features emit-cfg
target/release/monoruby benchmark/app_fib.rb > /dev/null
dot -Tsvg .cfg/fid-3835.dot -o fib-cfg.svg
```

Only functions that actually reach the JIT are dumped, so the set of files
also tells you what got compiled.

## Testing

```sh
cargo test              # unit + integration tests
bin/test                # the full CI scope: tests + coverage + benchmarks + optcarrot + spec
```

`bin/test` is what CI runs. In order it:

1. Runs `cargo llvm-cov nextest` with `stress-spill-pool` (plus `gc-stress`
   only when `GC_STRESS=1` is exported).
2. Builds a debug benchmark binary with the **same** feature list, so a
   `GC_STRESS=1` run stresses the benchmark / optcarrot / spec phases too.
3. Runs the benchmark scripts and `diff`s their output against CRuby
   (`app_fib`, `tarai`, `so_nbody`, plb2 `nqueen` / `sudoku`, and
   `so_mandelbrot` both with and without the JIT).
4. Runs optcarrot plain and with `--opt`, comparing output against CRuby.
5. Runs a ruby/spec subset if `../spec` and `../mspec` exist.
6. Writes an `lcov.info` coverage report.

`SKIP_COV=1` runs the whole script without llvm-cov instrumentation.

### The snapshot oracle

Tests compare monoruby's output against CRuby, but they do **not** spawn CRuby
per test. The single-code helpers — `run_test`, `run_test_once`, `run_test2`,
`run_test_with_prelude` — memoize expected output in the checked-in file
`monoruby/tests/ruby_oracle.tsv` (`code-hash → output`, key-sorted and
flock-serialized so concurrent nextest processes cooperate). A live `ruby` is
invoked only on a cache miss, and the fresh entry is written back — **commit
it**. The batched helpers (`run_tests`, `run_tests2`, and the binop/unop
generators built on them) always spawn a live CRuby, because their generated
code strings churn too much to be worth snapshotting.

`MONORUBY_TEST_ORACLE` selects the mode:

| Value | Behavior |
| --- | --- |
| unset / `snapshot` | Replay stored entries; spawn and record on a miss (the default) |
| `ruby` | Always spawn CRuby and refresh stale entries in place — use this after bumping the reference CRuby version |

```sh
rm monoruby/tests/ruby_oracle.tsv && cargo test   # regenerate from scratch
MONORUBY_TEST_ORACLE=ruby cargo test              # re-verify against a new CRuby
```

Tests whose expected value varies per host or OS must use `run_test_live` /
`run_test_once_live`, which always compare against a live CRuby: anything
touching `Dir.home` / `Dir.pwd`, absolute checkout paths, `~user` expansion
(`/root` vs macOS `/var/root`), or `realpath` under `/tmp` (a symlink to
`/private/tmp` on macOS). As a safety net a cached entry that disagrees with
monoruby is re-verified against a live CRuby before failing — grep test output
for `re-verifying against a live ruby` to find tests that should move to the
`_live` helpers.

Running the suite needs a host `ruby` matching the vendored pin (**4.0.2**,
from `monoruby/vendor/ruby-stdlib/.ruby-version`). Since Ruby 3.4 `bigdecimal`
is no longer a default gem, so install it explicitly or every
`tests/bigdecimal.rs` test fails with `LoadError`:

```sh
gem install bigdecimal
```

### GC stress

`gc-stress` collects at **every** safepoint. It is what finds unrooted-`Value`
bugs — a builtin that creates a `Value` and then re-enters Ruby — and it is
opt-in precisely because it is expensive: stacked on llvm-cov it took the
x86-64 nextest phase from ~10 minutes to ~100, and a full `bin/test` scope
under stress is an hours-scale job.

```sh
GC_STRESS=1 bin/test    # applies to every phase, not just nextest
```

Nothing enables it implicitly, so the automatic CI never pays for it. Instead
dispatch the manual `gc-stress` workflow from the Actions tab when touching
the GC, frame layout, argument binding, or any builtin that creates a `Value`
and re-enters Ruby. Its inputs are `arch` (`both` / `x86_64` / `aarch64`),
`scope` (`nextest` = the unit + integration suite, minutes; `full` = the whole
`bin/test` scope, hours), `test_filter` (a nextest `-E` expression), and
`no_fail_fast`. It runs on `ubuntu-latest` and **native** arm64
(`ubuntu-24.04-arm`), not qemu, and collects no coverage.

Tests whose loop counts exist only to reach the JIT thresholds should shrink
them under `cfg!(feature = "gc-stress")` (see `tests/method_call.rs`), or they
blow past nextest's per-test cap.

### ruby/spec

ruby/spec is cloned **outside** the monoruby repository, alongside it:

```
parent/
├── monoruby/    # this repository
├── spec/        # ruby/spec
└── mspec/       # the mspec runner
```

```sh
cd /path/to/parent-of-monoruby
git clone --depth 1 https://github.com/ruby/spec.git spec
git clone --depth 1 https://github.com/ruby/mspec.git mspec

cd monoruby && cargo install --path monoruby

cd ../spec
../mspec/bin/mspec run core/array -t monoruby              # one category
../mspec/bin/mspec run core/array/flatten_spec.rb -t monoruby  # one file
../mspec/bin/mspec run core/array -t monoruby --format dotted
```

`bin/spec` from the monoruby checkout runs a standard set of categories in one
go. Pass rates are published continuously on the [spec
dashboard](https://sisshiki1969.github.io/monoruby/spec/); see [ruby/spec hang
countermeasures](design/ruby_spec_skip_tags.md) for how the suite avoids
hangs.

### aarch64

On an x86-64 host the aarch64 backend is cross-compiled and run under
qemu-user:

```sh
bin/setup-aarch64-cross   # qemu-aarch64, aarch64-linux-gnu-gcc, rust std
bin/test-aarch64          # same scope as bin/test, for aarch64
```

`bin/test-aarch64` takes `SKIP_HEAVY=1` (skip the benchmarks and specs that
take over ten minutes under emulation), `SKIP_COV=1`, and `STRESS=1`. The
`.cargo/config.toml` wiring points `aarch64-unknown-linux-gnu` at that cross
toolchain and runner, which is why the CI job on a *native* arm64 runner
overrides `CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_{LINKER,RUNNER}` — otherwise
it would emulate everything.

## Measurement and profiling

`--features profile` collects deopt and recompile statistics: which sites
deoptimized, how often, and why. The reasons are the `RecompileReason`
variants — `NotCached`, `MethodNotFound`, `IvarIdNotFound`,
`ClassVersionGuardFailed`, `BecamePolymorphic`, `ConstVersionGuardFailed`.

`--features deopt` logs individual deoptimizations;
[Reading the deopt log](design/deopt_log.md) explains the format, including
how the log names the guard that actually branched when exits have been
deduplicated.

For time rather than counts, see [Benchmarks](benchmarks.md#profiling).

## JIT thresholds

A function is compiled after **20 calls** (`COUNT_START_COMPILE`) and a loop
after **100 iterations** (`COUNT_LOOP_START_COMPILE`). Both drop to 5 and 15
in test builds, so tests reach compiled code without long warm-up loops.

## Vendored and pinned dependencies

`hashbrown/` is a workspace member (a local fork). `smallvec` is also a fork
but is consumed as a git dependency rather than in-tree.

The `ruby-prism` wrapper is pinned to the `monoruby-vendored` branch of
[`sisshiki1969/prism`](https://github.com/sisshiki1969/prism). That fork has
two branches:

- `monoruby` — the minimal upstream-bound diff (the Rust `parse_with_options`
  API plus `ruby-prism-sys` bindgen allowlist additions). The base for any
  upstream PR to `ruby/prism`.
- `monoruby-vendored` — `monoruby` plus one commit checking in the C sources
  the upstream `vendored.rs` build script needs, so consumers need neither
  bundler nor `rake cargo:build`.

To bump the prism revision: push to the fork's `monoruby` branch, run
`bin/refresh-prism-vendored` (which rebuilds and force-pushes
`monoruby-vendored`), then `cargo update -p ruby-prism` here.

`bin/vendor-ruby-stdlib` re-snapshots CRuby's pure-Ruby stdlib and default
gems into `monoruby/vendor/ruby-stdlib/`. It is a maintenance step, never run
by `cargo build`.

## Continuous integration

| Workflow | Trigger | What it does |
| --- | --- | --- |
| `rust.yml` | push / PR to `master` | `bin/test` on x86-64 Linux (with coverage to Codecov) and on native Apple Silicon |
| `bench.yml` | push to `master` touching the interpreter | yjit-bench vs YJIT on x86-64 and aarch64, published to the portal |
| `spec-core.yml`, `spec-library.yml` | push to `master` touching the interpreter, plus a weekly cron | ruby/spec pass rates, published to the portal |
| `docs.yml` | push to `master` touching `doc/**` or `docs/**` | Builds this book with mdBook and publishes it to `gh-pages` under `/docs/` |
| `release-binaries.yml` | push to `master`, release published, dispatch | Prebuilt binaries for the `setup-monoruby` action |
| `gc-stress.yml` | **dispatch only** | The stress run described above |

Neither `rust.yml` job sets `GC_STRESS`, so neither pays the per-safepoint
cost.

## Documentation

This book lives in `docs/`. `docs/build.sh` copies `doc/*.md` and the diagrams
into `docs/src/design/` (gitignored) and runs `mdbook build`; `docs.yml`
publishes the result. If you add a design document to `doc/`, give it a
chapter in `docs/src/SUMMARY.md` — mdBook renders only what `SUMMARY.md`
lists, so an unlisted document is copied and then silently dropped.

```sh
bash docs/build.sh          # output in docs/book/
mdbook serve docs           # live preview
```

### The changelog and the README

[`docs/src/changelog.md`](changelog.md) is the single source of truth for the
monthly changelog. The README shows only the two newest month sections,
copied verbatim between its `<!-- BEGIN LATEST-MONTHS -->` /
`<!-- END LATEST-MONTHS -->` markers — that block is generated, so edit the
changelog and regenerate rather than editing the README:

```sh
bin/sync-changelog-readme           # rewrite the README block
bin/sync-changelog-readme --check   # exit 1 if the two have drifted
```

A scheduled Routine runs on the 1st of each month: it summarizes the previous
month's merged PRs into a new `### <Month> <Year>` section at the top of the
changelog's current `## <year>` group, runs the sync script, and pushes both
files. Pushing `docs/src/changelog.md` also re-triggers `docs.yml`, so the
published book picks the new month up in the same run.
