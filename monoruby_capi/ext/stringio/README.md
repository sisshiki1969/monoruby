# ext/stringio from CRuby, built against the C API layer

`stringio.c` is CRuby 4.0.6's `ext/stringio` (the stringio gem
3.2.0), unmodified.
`monoruby/build.rs` (`build_capi_extensions`) compiles it against
`monoruby_capi/include`, links the layer's archives in, and installs the
result as `<install root>/ext/stringio.so` (`.dylib` on macOS), which
`monoruby/stdlib/stringio.rb` requires. See `doc/c_api.md` §3.19.

To update: copy the same file from a newer CRuby and run the specs
(`spec/ruby/library/stringio`) and `cargo test --test stringio_encoding`.

The sources are distributed under CRuby's terms: the Ruby License
(`COPYING`) or the 2-clause BSD License (`BSDL`), both reproduced here.
