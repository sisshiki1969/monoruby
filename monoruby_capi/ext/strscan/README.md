# ext/strscan from CRuby, built against the C API layer

`strscan.c` is CRuby 4.0.6's `ext/strscan` (the strscan gem 3.1.6),
unmodified. `monoruby/build.rs` (`build_capi_extensions`) compiles it
against `monoruby_capi/include`, links the layer's archives in, and
installs the result as `<install root>/ext/strscan.so` (`.dylib` on
macOS). `monoruby/stdlib/strscan.rb` requires it and then
`strscan/strscan.rb` (CRuby's `ext/strscan/lib/strscan/strscan.rb`).
See `doc/c_api.md` §3.18.

To update: copy the same files from a newer CRuby and run the specs
(`spec/ruby/library/stringscanner`) and `cargo test --test strscan`.

The sources are distributed under CRuby's terms: the Ruby License
(`COPYING`) or the 2-clause BSD License (`BSDL`), both reproduced here.
