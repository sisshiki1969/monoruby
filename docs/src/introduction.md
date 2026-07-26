# monoruby

[monoruby](https://github.com/sisshiki1969/monoruby) is a Ruby implementation written from scratch in Rust, featuring a register-based bytecode VM and a just-in-time (JIT) compiler for x86-64 and aarch64 (Apple Silicon). It is fast — comparable to CRuby with YJIT/ZJIT on many benchmarks — and has no dependency on any other Ruby runtime.

This site documents monoruby's internals.

- The **Architecture** section contains overview pages: start with the [Architecture Overview](architecture-overview.md).
- The **Design Documents** section renders the full design documents from the repository's [`doc/`](https://github.com/sisshiki1969/monoruby/tree/master/doc) directory (some are written in Japanese, as marked).

## Related resources

- [README](https://github.com/sisshiki1969/monoruby#readme) — features, installation, monthly changelog
- [Build and Install](https://github.com/sisshiki1969/monoruby/wiki/Build-and-Install) — build instructions (wiki)
- [Benchmark results](https://sisshiki1969.github.io/monoruby/bench/) — continuously updated yjit-bench comparison
- [ruby/spec dashboard](https://sisshiki1969.github.io/monoruby/spec/) — spec compliance for this repository
- [rubyspec-stats](https://sisshiki1969.github.io/rubyspec-stats/) — daily ruby/spec pass rates across Ruby implementations
