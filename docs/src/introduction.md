# monoruby

[monoruby](https://github.com/sisshiki1969/monoruby) is a Ruby implementation written from scratch in Rust, featuring a register-based bytecode VM and a just-in-time (JIT) compiler for x86-64 and aarch64 (Apple Silicon). It is fast — comparable to CRuby with YJIT/ZJIT on many benchmarks — and has no dependency on any other Ruby runtime.

This site is monoruby's documentation: how to build and run it, how it performs, and how it works inside.

- The **Getting Started** section covers [installing and building](installation.md) monoruby, and the [build options and test workflow](development.md) used when developing it.
- The **Performance and Compatibility** section is where the live dashboards are explained: [Benchmark](benchmarks.md) for speed against CRuby+YJIT, [Compatibility](compatibility.md) for ruby/spec conformance. The [Changelog](changelog.md) sits alongside them.
- The **Architecture** section contains overview pages: start with the [Architecture Overview](architecture-overview.md).
- The **Design Documents** section renders the full design documents from the repository's [`doc/`](https://github.com/sisshiki1969/monoruby/tree/master/doc) directory (some are written in Japanese, as marked).

## Quick start

```sh
git clone https://github.com/sisshiki1969/monoruby.git
cd monoruby
cargo install --path monoruby   # nightly Rust is installed automatically by rust-toolchain.toml
monoruby -e 'puts "hello"'
```

See [Installation and Build](installation.md) for prebuilt binaries, the GitHub Action, platform notes and the full command-line reference.

## Related resources

- [README](https://github.com/sisshiki1969/monoruby#readme) — features and the monthly changelog
- [Benchmark dashboard](https://sisshiki1969.github.io/monoruby/bench/) — continuously updated yjit-bench comparison, re-run on every push to `master` that touches the interpreter
- [ruby/spec dashboard](https://sisshiki1969.github.io/monoruby/spec/) — spec compliance for this repository
- [rubyspec-stats](https://sisshiki1969.github.io/rubyspec-stats/) — daily ruby/spec pass rates across Ruby implementations
