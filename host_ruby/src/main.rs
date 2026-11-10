//! Print the absolute path of the host CRuby monoruby pairs with — the
//! same answer `ruby_probe` and the test harness arrive at — for shell
//! scripts (`bin/test`, `bin/test-gems`, `bin/ruby-bench-diff`):
//!
//! ```sh
//! RUBY_BIN="$(cargo run -q -p host_ruby)"
//! ```
//!
//! Exits 1 with a message on stderr when no usable Ruby is installed.

fn main() {
    let target = host_ruby::pinned_version();
    match host_ruby::find(target, None) {
        Some(ruby) => println!("{}", ruby.path),
        None => {
            eprintln!(
                "host-ruby: no CRuby >= {}.{} found on PATH or under a version manager \
                 (the pin is {}.{}.{}; set MONORUBY_RUBY=<path> to name one)",
                host_ruby::MIN_RUBY_VERSION.0,
                host_ruby::MIN_RUBY_VERSION.1,
                target.0,
                target.1,
                target.2
            );
            std::process::exit(1);
        }
    }
}
