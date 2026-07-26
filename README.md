# monoruby

[![Rust](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/monoruby/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/monoruby/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/monoruby)
[![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)

Ruby implementation with yet another JIT compiler written in Rust.

## What's New in 2026

Monthly highlights of improvements since January 2026 (with representative PRs).

### January 2026

- Hardened JIT frame handling so local frames are never captured by JIT-compiled code ([#93](https://github.com/sisshiki1969/monoruby/pull/93)).
- Added `SAFETY` comments to `unsafe` blocks across the codebase ([#94](https://github.com/sisshiki1969/monoruby/pull/94)).
- Introduced immediate operands (`BytecodeInst::Immediate`) into the bytecode ([#95](https://github.com/sisshiki1969/monoruby/pull/95)).

### February 2026

- Optimized rest / keyword-rest parameters in the JIT ([#96](https://github.com/sisshiki1969/monoruby/pull/96)).
- Corrected `/` and `%` semantics for Integer and Float ([#97](https://github.com/sisshiki1969/monoruby/pull/97)) and fixed `Integer#digits` ([#99](https://github.com/sisshiki1969/monoruby/pull/99)).
- Optimized the Range class ([#98](https://github.com/sisshiki1969/monoruby/pull/98)) and fixed `Range#include?` for string / beginless / endless ranges ([#104](https://github.com/sisshiki1969/monoruby/pull/104)).
- Introduced continuation frames ([#101](https://github.com/sisshiki1969/monoruby/pull/101)) and reworked JIT slot write-back logic ([#102](https://github.com/sisshiki1969/monoruby/pull/102)).

### March 2026

- Implemented `Marshal.dump` / `Marshal.load` ([#121](https://github.com/sisshiki1969/monoruby/pull/121), [#125](https://github.com/sisshiki1969/monoruby/pull/125)) and the `Set` class, a CRuby 4.0 built-in ([#130](https://github.com/sisshiki1969/monoruby/pull/130)).
- Implemented the `retry` statement ([#109](https://github.com/sisshiki1969/monoruby/pull/109)), module/class lifecycle hooks ([#127](https://github.com/sisshiki1969/monoruby/pull/127)), anonymous block forwarding ([#128](https://github.com/sisshiki1969/monoruby/pull/128)), and regex backreferences / special variables ([#129](https://github.com/sisshiki1969/monoruby/pull/129)).
- Added `Kernel.#load` ([#105](https://github.com/sisshiki1969/monoruby/pull/105)), correct `Dir.glob` ([#106](https://github.com/sisshiki1969/monoruby/pull/106)), `Kernel.#format` / `sprintf` ([#162](https://github.com/sisshiki1969/monoruby/pull/162)), and a SQLite3 FFI bridge for the sqlite3 gem ([#160](https://github.com/sisshiki1969/monoruby/pull/160)).
- Started a large ruby/spec compatibility push: implicit type conversions ([#225](https://github.com/sisshiki1969/monoruby/pull/225)), frozen-object support ([#240](https://github.com/sisshiki1969/monoruby/pull/240)), Errno exceptions ([#250](https://github.com/sisshiki1969/monoruby/pull/250)), and many crash fixes on invalid UTF-8 input ([#209](https://github.com/sisshiki1969/monoruby/pull/209)–[#212](https://github.com/sisshiki1969/monoruby/pull/212)).
- Fixed many issues to run optcarrot ([#115](https://github.com/sisshiki1969/monoruby/pull/115)) and the rubyboy / lee benchmarks ([#111](https://github.com/sisshiki1969/monoruby/pull/111), [#120](https://github.com/sisshiki1969/monoruby/pull/120)).

### April 2026

- Reimplemented `Rational` as a first-class Rust type with literal support ([#266](https://github.com/sisshiki1969/monoruby/pull/266)).
- Broad ruby/spec compliance work on Integer / Float / Array / Symbol / Class / Module (e.g. [#281](https://github.com/sisshiki1969/monoruby/pull/281), [#284](https://github.com/sisshiki1969/monoruby/pull/284), [#309](https://github.com/sisshiki1969/monoruby/pull/309)), with a live spec dashboard published on GitHub Pages ([#364](https://github.com/sisshiki1969/monoruby/pull/364), [#365](https://github.com/sisshiki1969/monoruby/pull/365)).
- Moved shift / bitwise / `**` / `%` into the inline-function JIT pipeline with constant folding ([#307](https://github.com/sisshiki1969/monoruby/pull/307), [#308](https://github.com/sisshiki1969/monoruby/pull/308)), and added frame-free trivial-method optimization via `ISeqHint` ([#290](https://github.com/sisshiki1969/monoruby/pull/290)).
- Struct: per-instance slot storage with JIT-inlined member accessors ([#367](https://github.com/sisshiki1969/monoruby/pull/367)–[#369](https://github.com/sisshiki1969/monoruby/pull/369)).
- String: encoding-aware data model — code ranges, `Encoding::CompatibilityError`, encoding-aware char iteration ([#382](https://github.com/sisshiki1969/monoruby/pull/382)–[#384](https://github.com/sisshiki1969/monoruby/pull/384)).
- Reworked `autoload` as a proper state machine ([#376](https://github.com/sisshiki1969/monoruby/pull/376)) and added lazy heap promotion for Proc / Lambda / Binding captures ([#332](https://github.com/sisshiki1969/monoruby/pull/332)).

### May 2026

- Switched the parser to Prism, the official Ruby parser ([#412](https://github.com/sisshiki1969/monoruby/pull/412)).
- Encoding subsystem overhaul: real `String#encode` ([#443](https://github.com/sisshiki1969/monoruby/pull/443)), `Encoding::Converter` ([#447](https://github.com/sisshiki1969/monoruby/pull/447), [#451](https://github.com/sisshiki1969/monoruby/pull/451)), ISO-2022-JP ([#449](https://github.com/sisshiki1969/monoruby/pull/449)), and EUC-JP / Shift_JIS-aware string operations ([#536](https://github.com/sisshiki1969/monoruby/pull/536)–[#545](https://github.com/sisshiki1969/monoruby/pull/545)).
- Native String fast paths (`reverse`, `rindex`, case mapping, …) that beat YJIT ([#493](https://github.com/sisshiki1969/monoruby/pull/493)–[#497](https://github.com/sisshiki1969/monoruby/pull/497)); dropped per-builtin `catch_unwind` for +8–10% on optcarrot ([#501](https://github.com/sisshiki1969/monoruby/pull/501)).
- JIT: virtual FP registers with spill-to-stack ([#387](https://github.com/sisshiki1969/monoruby/pull/387)), heap-constant folding and `Object#is_a?` inlining ([#504](https://github.com/sisshiki1969/monoruby/pull/504), [#505](https://github.com/sisshiki1969/monoruby/pull/505)), non-deopting polymorphic comparisons ([#519](https://github.com/sisshiki1969/monoruby/pull/519)).
- Decoupled monoruby from any host Ruby installation ([#579](https://github.com/sisshiki1969/monoruby/pull/579), [#595](https://github.com/sisshiki1969/monoruby/pull/595)); completed the Marshal format tags ([#588](https://github.com/sisshiki1969/monoruby/pull/588)–[#603](https://github.com/sisshiki1969/monoruby/pull/603)); real `File::Stat` ([#607](https://github.com/sisshiki1969/monoruby/pull/607)); frame-local `$~` / `$_` ([#608](https://github.com/sisshiki1969/monoruby/pull/608)).
- Started the aarch64 port: VM-tier backend and macOS (Apple Silicon) support ([#640](https://github.com/sisshiki1969/monoruby/pull/640), [#641](https://github.com/sisshiki1969/monoruby/pull/641), [#644](https://github.com/sisshiki1969/monoruby/pull/644)).
- Continuous yjit-bench benchmarking against YJIT, charted on GitHub Pages ([#411](https://github.com/sisshiki1969/monoruby/pull/411), [#416](https://github.com/sisshiki1969/monoruby/pull/416)).

### June 2026

- Completed the aarch64 JIT backend: full AsmInst coverage, inline methods, loop JIT, and recompilation on Apple Silicon ([#645](https://github.com/sisshiki1969/monoruby/pull/645)–[#704](https://github.com/sisshiki1969/monoruby/pull/704)).
- Introduced a generational GC (RGenGC-style) ([#705](https://github.com/sisshiki1969/monoruby/pull/705)); GC now also triggers on malloc growth ([#732](https://github.com/sisshiki1969/monoruby/pull/732)).
- Completed the Prism migration and removed the old hand-written parser (ruruby-parse) ([#657](https://github.com/sisshiki1969/monoruby/pull/657)).
- New JIT register allocation: LIR-based lowering with a per-basic-block GP register allocator; retired the R15 accumulator ([#741](https://github.com/sisshiki1969/monoruby/pull/741), [#756](https://github.com/sisshiki1969/monoruby/pull/756), [#763](https://github.com/sisshiki1969/monoruby/pull/763)).
- Cut allocation/dispatch overhead by 12–26% on addressable benchmarks ([#708](https://github.com/sisshiki1969/monoruby/pull/708)); zero-copy String operations (gsub, slice, lines, scan) ([#722](https://github.com/sisshiki1969/monoruby/pull/722)–[#724](https://github.com/sisshiki1969/monoruby/pull/724)).
- Made the build host-Ruby independent and reproducible ([#769](https://github.com/sisshiki1969/monoruby/pull/769)).

### July 2026

- Big language-semantics compliance drive on the ruby/spec "language" group: destructuring, block-argument semantics, `defined?`, flip-flops, `BEGIN`/`END`, and predefined globals ([#804](https://github.com/sisshiki1969/monoruby/pull/804)–[#874](https://github.com/sisshiki1969/monoruby/pull/874), notably [#861](https://github.com/sisshiki1969/monoruby/pull/861)).
- Implemented pattern matching (`case`/`in`, `=>`) ([#883](https://github.com/sisshiki1969/monoruby/pull/883)) and MRI's full eigenclass tower ([#877](https://github.com/sisshiki1969/monoruby/pull/877)).
- Green threads (M:1): scheduler core, real Thread / Mutex / Queue, scheduler-integrated blocking IO, and preemptive timeslice multithreading ([#941](https://github.com/sisshiki1969/monoruby/pull/941)–[#944](https://github.com/sisshiki1969/monoruby/pull/944), [#962](https://github.com/sisshiki1969/monoruby/pull/962)).
- Real TCP / UDP / UNIX-domain sockets ([#964](https://github.com/sisshiki1969/monoruby/pull/964), [#981](https://github.com/sisshiki1969/monoruby/pull/981)); `IO::Buffer` with mmap file mapping ([#927](https://github.com/sisshiki1969/monoruby/pull/927), [#931](https://github.com/sisshiki1969/monoruby/pull/931)); `IO.copy_stream` ([#924](https://github.com/sisshiki1969/monoruby/pull/924)).
- Added the `setup-monoruby` GitHub Action with prebuilt binaries ([#884](https://github.com/sisshiki1969/monoruby/pull/884), [#886](https://github.com/sisshiki1969/monoruby/pull/886)) and CRuby-compatible command-line option processing ([#891](https://github.com/sisshiki1969/monoruby/pull/891)).
- Completed the Exception API: raise-time backtraces, `full_message`, NameError / NoMethodError metadata ([#893](https://github.com/sisshiki1969/monoruby/pull/893)–[#896](https://github.com/sisshiki1969/monoruby/pull/896)).
- Brought the aarch64 JIT to optimization parity with x86-64 ([#993](https://github.com/sisshiki1969/monoruby/pull/993)–[#1004](https://github.com/sisshiki1969/monoruby/pull/1004)) and shrank call frames for ~7% faster fib ([#850](https://github.com/sisshiki1969/monoruby/pull/850)).

## Presentation

- Presentation movie and slides for RubyKaigi2024 is [here](https://rubykaigi.org/2024/presentations/s_isshiki1969.html#day2).
- Presentation movie and slides for RubyKaigi2025 is [here](https://rubykaigi.org/2025/presentations/s_isshiki1969.html#day2).

## Features

- Written in Rust from scratch. No dependencies on any other Ruby implementations.
- Fast. Currently, monoruby is comparable to ruby 4.0.5+YJIT/ZJIT on many benchmarks, not just optcarrot.
- Parses Ruby source with [prism](https://github.com/ruby/prism), the official Ruby parser (the original hand-written parser was replaced in May 2026).
- Register-based bytecode.
- Bytecode executor (virtual machine) emitting machine code directly, for both x86-64 and aarch64 (Apple Silicon).
- A compact and fast just-in-time compiler with full backends for x86-64 and aarch64. (internally using self-made dynamic assembler [monoasm](https://github.com/sisshiki1969/monoasm))

## Compatibility

Conformance with Ruby has improved dramatically over the course of 2026, driven by an extensive [ruby/spec](https://github.com/ruby/spec)-based compliance effort.
As of July 2026, monoruby passes 100% of the command-line specs, 99.6% of the language specs, and about 90% of the core-library specs.
Daily-updated pass statistics, compared with CRuby / TruffleRuby / JRuby, are published at [rubyspec-stats](https://sisshiki1969.github.io/rubyspec-stats/).

## Prerequisites

### Platform

- x86-64 Linux
- aarch64 — macOS on Apple Silicon is fully supported (VM + JIT); CI runs natively on GitHub's Apple Silicon (`macos-latest`) runners. Linux arm64 works as well.

## How to run

Please see [wiki](https://github.com/sisshiki1969/monoruby/wiki/Build-and-Install) for details. 

(1) Install nightly Rust.

First of all, install Rust nightly.
[Check here to install Rust](https://www.rust-lang.org/ja/tools/install)

_Caution!!_ **only nightly Rust works** for monoruby.
[See here to work with nightly Rust](https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust).

(2) Clone this repository.

```sh
> git clone https://github.com/sisshiki1969/monoruby.git
> cd monoruby
```

(3) Build and run monoruby with Ruby script file.

```sh
> cargo run --release -- test.rb
```

(4) or, Launch REPL.

```sh
> cargo run --bin irm
```

or

```sh
> bin/irm
```

## Using monoruby in GitHub Actions

This repository doubles as a `setup-monoruby` action (see `action.yml`),
modeled after [ruby/setup-ruby](https://github.com/ruby/setup-ruby). The ref
after `@` selects the monoruby version to install:

```yaml
steps:
  - uses: sisshiki1969/monoruby@master # or a release tag
  - run: monoruby my_script.rb
```

The action first tries to download a prebuilt binary attached to a GitHub
release (built by the `release binaries` workflow), which installs in seconds:
release tags use their own release's assets, while `@master` / `@nightly` use
the rolling `nightly` prerelease, rebuilt on every master push (so it
can lag a just-pushed HEAD by one build, ~15 min). Automatic builds cover
x86-64 Linux only; assets for Linux arm64 and macOS arm64 are published on
demand by dispatching the `release binaries` workflow with the wanted tag and
targets. When no asset exists for the ref/platform — or
`prefer-prebuilt: 'false'` is set — the action falls back to building monoruby
from source on the first run for each monoruby revision × runner OS/arch and
caches the resulting binary and runtime tree with `actions/cache`, so
subsequent runs restore in seconds. Linux (x64/arm64) and macOS (Apple
Silicon) hosted runners are supported either way.

Inputs and outputs:

| Name                        | Kind   | Description                                                            |
| --------------------------- | ------ | ---------------------------------------------------------------------- |
| `prefer-prebuilt` (`'true'`)| input  | Set to `'false'` to skip release assets and build the exact ref        |
| `cache` (`'true'`)          | input  | Set to `'false'` to disable the source-build cache                     |
| `cache-version` (`'v1'`)    | input  | Mixed into the cache key; bump to discard existing caches              |
| `prebuilt`                  | output | `'true'` if a prebuilt release asset was used                          |
| `cache-hit`                 | output | `'true'` if the built binary was restored from cache                   |
| `monoruby-version`          | output | Output of `monoruby --version`                                         |
| `ruby-version`              | output | `RUBY_VERSION` reported by the installed monoruby                      |

## Benchmark

Up-to-date benchmark results (yjit-bench, measured on every push to master) and a ruby/spec compliance dashboard are published on [the project portal](https://sisshiki1969.github.io/monoruby/). The numbers below are from older one-off measurements.

### 1. Optcarrot banechmark

Several Ruby implementations described below were measured by [optcarrot](https://github.com/mame/optcarrot) benchmark.
Please see [wiki](https://github.com/sisshiki1969/monoruby/wiki/Optcarrot-benchmark) for details. 

#### Versions of used Rubies

- ruby 3.4.0dev (2024-04-27T08:56:20Z master 9ea77cb351) [x86_64-linux]
- truffleruby 24.0.1, like ruby 3.2.2, Oracle GraalVM JVM [x86_64-linux]
- truffleruby 24.0.1, like ruby 3.2.2, Oracle GraalVM Native [x86_64-linux]
- monoruby: 3e348afd4141c40978342e67ad26d42dc0b8d2a7

#### Optcarrot benchmark

![optcarrot_benchmark](./doc/optcarrot_benchmark.png)

#### Optcarrot fps history (0-3000 frames)

![optcarrot_fps_history](./doc/optcarrot_fps_history.png)

### 2. Other benchmarks

Several Ruby implementations described below were measured by [yjit-bench](https://github.com/Shopify/yjit-bench).
Please see [wiki](https://github.com/sisshiki1969/monoruby/wiki/General-benchmarks) for details.

#### Versions of used Rubies

- monoruby: monoruby 0.3.0
- yjit: ruby 3.4.1 (2024-12-25 revision 48d4efcb85) +YJIT +PRISM [x86_64-linux]
- truffleruby-24.1.1: truffleruby 24.1.1, like ruby 3.2.4, Oracle GraalVM Native [x86_64-linux]

#### Results

The graph shows the speed ratio against truffleruby. (higher is better)

![micro_bench](./doc/chart.png)
