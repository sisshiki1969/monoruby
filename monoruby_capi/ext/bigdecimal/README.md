# bigdecimal from the bigdecimal gem, built against the C API layer

`bigdecimal.c` and its headers, `missing.c` and `missing/dtoa.c` are the
bigdecimal gem 4.0.1's `ext/bigdecimal` (the version CRuby 4.0.9 bundles),
unmodified. `monoruby/build.rs` (`build_capi_extensions`) compiles them
against `monoruby_capi/include`, links the layer's archives in, and
installs the result as `<install root>/ext/bigdecimal.so` (`.dylib` on
macOS). `monoruby/gem/bigdecimal.rb` and `monoruby/gem/bigdecimal/` are the
gem's own `lib/`, which requires it.

To update: copy the same files from a newer gem and run the specs
(`spec/ruby/library/bigdecimal`) and `cargo test --test bigdecimal`.

The sources are distributed under the gem's terms: the Ruby License or the
2-clause BSD License (`LICENSE`).
