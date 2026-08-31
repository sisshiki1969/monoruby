# Installation and Build

monoruby is a standalone Ruby implementation: it does **not** need CRuby or
any other Ruby runtime to build or to run. There are two ways to get it — a
prebuilt binary, or a build from source.

## Supported platforms

| Platform | Status |
| --- | --- |
| x86-64 Linux | Fully supported (VM + JIT). Primary CI target. |
| aarch64 macOS (Apple Silicon) | Fully supported (VM + JIT). CI runs natively on `macos-latest`. |
| aarch64 Linux | Supported (VM + JIT). Exercised by the manual `gc-stress` workflow on `ubuntu-24.04-arm`. |

Both architectures lower the complete instruction set; the JIT never declines
a compile on either. See [x86-64 / aarch64 JIT backend
differences](design/arch_difference.md) for what actually differs between the
two backends.

## Prebuilt binaries

Releases carry prebuilt tarballs, and a rolling `nightly` prerelease is
rebuilt on every push to `master`. The easiest way to consume them is the
`setup-monoruby` GitHub Action that this repository doubles as, modeled after
[ruby/setup-ruby](https://github.com/ruby/setup-ruby). The ref after `@`
selects the version:

```yaml
steps:
  - uses: sisshiki1969/monoruby@master # or a release tag
  - run: monoruby my_script.rb
```

The action downloads a release asset when one exists for the ref and platform
(seconds), and otherwise builds from source once per monoruby revision ×
runner OS/arch and caches the result with `actions/cache`. Automatic builds
cover x86-64 Linux; Linux arm64 and macOS arm64 assets are published on demand
by dispatching the `release binaries` workflow. See the [README's action
section](https://github.com/sisshiki1969/monoruby#using-monoruby-in-github-actions)
for the full input/output table.

Each asset is a tarball containing `bin/monoruby`, `bin/irm`, and the
`monoruby-home/v<version>/` runtime tree described under [What the build
installs](#what-the-build-installs) below. Because the binary bakes in the
build machine's install-root path, point `MONORUBY_INSTALL_ROOT` at wherever
you extracted that tree.

## Building from source

### 1. Install Rust

**Only nightly Rust works.** monoruby uses several nightly-only language
features (`box_patterns`, `iter_next_chunk`, `step_trait`,
`coverage_attribute`), so a stable toolchain fails to build.

You do not have to select the channel by hand: `rust-toolchain.toml` pins the
exact nightly the project is developed and tested against, and `rustup`
installs and uses it automatically the first time you run `cargo` in the
checkout.

```toml
# rust-toolchain.toml
[toolchain]
channel = "nightly-2026-08-18"
```

If you have no Rust at all yet, [install
rustup](https://www.rust-lang.org/tools/install) first.

### 2. Clone the repository

```sh
git clone https://github.com/sisshiki1969/monoruby.git
cd monoruby
```

### 3. Platform-specific dependencies

On **aarch64 macOS** only, monoruby links the system libffi instead of the
bundled one (the bundled `libffi-sys` fails to link `_ffi_prep_cif_machdep` on
arm64):

```sh
brew install libffi pkg-config
export PKG_CONFIG_PATH="$(brew --prefix libffi)/lib/pkgconfig"
```

Linux and x86-64 macOS need nothing extra.

### 4. Build and run

```sh
cargo build --release
cargo run --release -- test.rb
```

The debug profile is built at `opt-level = 1` (set in the workspace
`Cargo.toml`), so a debug build is usable for day-to-day work — but use
`--release` for anything you intend to measure.

A one-liner:

```sh
cargo run --release -- -e "puts 100"
```

### 5. Install

```sh
cargo install --path monoruby
```

This puts two binaries on your `PATH`:

```sh
monoruby test.rb   # the interpreter
irm                # the REPL
```

From a checkout you can launch the REPL without installing:

```sh
cargo run --bin irm
# or
bin/irm
```

## What the build installs

`monoruby/build.rs` runs on every `cargo build` and is **host-Ruby
independent** — it does not need a `ruby` on `PATH` to produce a correct,
reproducible build. It does two things:

1. **Bakes the reported Ruby version.** `MONORUBY_RUBY_VERSION` is read from
   `monoruby/vendor/ruby-stdlib/.ruby-version` (currently **4.0.2**) and
   reported at run time as `RUBY_VERSION`. Taking it from the vendored
   snapshot rather than a host `ruby` keeps the version monoruby reports in
   step with the stdlib it actually ships.

2. **Installs the runtime tree** into a per-version root,
   `~/.monoruby/v<version>/` (e.g. `~/.monoruby/v0.3.0/`), whose absolute path
   is baked into the binary as `MONORUBY_INSTALL_ROOT`:

   | Source | Installed as | Contents |
   | --- | --- | --- |
   | `monoruby/vendor/ruby-stdlib/` | `<root>/lib/` | Checked-in CRuby stdlib + default-gem snapshot |
   | `monoruby/builtins/` | `<root>/builtins/` | Ruby files loaded at interpreter start (`startup.rb`, `enumerable.rb`, …) |
   | `monoruby/stdlib/`, `monoruby/gem/` | `<root>/lib/` and `<root>/stub/` | monoruby's own host-independent replacements for C-extension-backed libraries |

   The install is staged in a private directory and swapped in with an atomic
   rename, so a running monoruby never sees a half-populated tree, and the
   per-version namespacing keeps concurrent builds and multiple checkouts from
   clobbering each other.

`build.rs` declares `cargo:rerun-if-changed` for each of those trees, so
editing `builtins/`, `stdlib/` or `gem/` reinstalls on the next build, and
deleting `~/.monoruby` re-triggers the whole install.

Setting `MONORUBY_INSTALL_ROOT` in the **runtime** environment overrides the
baked path, which is what makes a distributed binary relocatable.

## Do I need a host Ruby?

**To build and run monoruby: no.** The stdlib is vendored and the version is
read from the snapshot.

A host `ruby` is used for exactly two optional things:

- **Host-installed (non-default) gems.** At startup `src/ruby_probe.rs`
  invokes a host `ruby` once — if one is present and is 4.0 or later — to
  discover `$LOAD_PATH` and `Gem.paths.path`, and caches the answer in
  `~/.monoruby/{library_path,gem_path}` so the ~50 ms spawn is paid once per
  machine. Precedence is `MONORUBY_GEM_PATH` / `MONORUBY_LOAD_PATH`, then
  `GEM_PATH`, then the cache files, then the probe. `MONORUBY_REPROBE=1`
  forces a fresh probe. With no host Ruby, those caches stay empty and the
  vendored stdlib still loads normally — you will just see this on startup:

  ```text
  Warning: failed to read library path file: "~/.monoruby/library_path". Ruby may not be installed.
  ```

  It is a warning, not an error: only host-installed gems are unavailable.

- **Running the test suite**, which compares monoruby's output against CRuby.
  See [Development and Build Options](development.md#testing).

## Command-line options

monoruby accepts CRuby's command-line switches:

```text
Usage: monoruby [switches] [--] [programfile] [arguments]
  -0[octal]       specify record separator (\0, if no argument)
  -a              autosplit mode with -n or -p (splits $_ into $F)
  -i[extension]   edit ARGV files in place (make backup if extension supplied)
  -c              check syntax only
  -Cdirectory     cd to directory before executing your script
  -d, --debug     set debugging flags (set $DEBUG to true)
  -e 'command'    one line of script. Several -e's allowed. Omit [programfile]
  -Eex[:in], --encoding=ex[:in]
                  specify the default external and internal character encodings
  -Fpattern       split() pattern for autosplit (-a)
  -Idirectory     specify $LOAD_PATH directory (may be used more than once)
  -l              enable line ending processing
  -n              assume 'while gets(); ... end' loop around your script
  -p              assume loop like -n but print line also like sed
  -rlibrary       require the library before executing your script
  -s              enable some switch parsing for switches after script name
  -S              look for the script using PATH environment variable
  -U              set the internal encoding to UTF-8
  -v              print the version number, then turn on verbose mode
  -w              turn warnings on for your script
  -W[level=2|:category]
                  set warning level; 0=silence, 1=medium, 2=verbose
  -x[directory]   strip off text before #!ruby line and perhaps cd to directory
  -h              show this message, --help for more options
  --ast           dump the parsed ruby-prism AST and exit
  --no-jit        disable just-in-time compilation
  --no-gc         disable garbage collection
  --enable=feature[,...], --disable=feature[,...]
                  enable or disable features (gems, did_you_mean, rubyopt,
                  frozen-string-literal, all)
```

`RUBYOPT` is honoured for the subset of switches CRuby permits there.

The three monoruby-specific switches are worth knowing:

- `--no-jit` — run everything in the bytecode VM. The JIT is always compiled
  into the binary; this disables it at run time. Useful for isolating a JIT
  bug from a VM bug, and as the baseline half of a JIT A/B.
- `--no-gc` — disable garbage collection entirely.
- `--ast` — dump the parsed prism AST and exit.
