//! Tests that need a third-party gem installed for the host CRuby.
//!
//! Each one runs the same Ruby under monoruby and under CRuby — where
//! the gem's real C extension answers — and requires the two to agree.
//! That is what pins monoruby's stand-in (`gem/<name>/<name>.rb` over a
//! `String.__*` primitive, or a dynamically loaded extension) to the
//! thing it stands in for.
//!
//! They live in one crate, as modules of one test target, because each
//! `tests/*.rs` is a crate of its own and links the interpreter into its
//! own binary: as ten files they cost ten copies. They are separate from
//! the interpreter's own tests because they are the only ones whose
//! prerequisite is `gem install` — see this crate's `Cargo.toml`.

mod bcrypt;
mod coolio;
mod hexapdf_output;
mod markly;
mod msgpack;
mod nokogiri;
mod ruby_bench_outputs;
mod strptime;
mod yajl;
mod zstd;
