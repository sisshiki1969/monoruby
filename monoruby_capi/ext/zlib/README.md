# ext/zlib from CRuby, built against the C API layer

`zlib.c` is CRuby 4.0.6's `ext/zlib`, unmodified. `monoruby/build.rs`
(`build_capi_extensions`) compiles it against `monoruby_capi/include`
and zlib's headers, and links it with the layer's archives and zlib
into `<install root>/ext/zlib.so` (`.dylib` on macOS), which
`monoruby/stdlib/zlib.rb` requires. zlib is the one `libz-sys` builds
from its bundled source (`bundled-zlib`, the default; linked statically
from `DEP_Z_ROOT/lib/libz.a`), or the platform's `-lz` under
`system-zlib`. See `doc/c_api.md` §3.19.

To update: copy the same file from a newer CRuby and run the specs
(`spec/ruby/library/zlib`) and `cargo test --test zlib_deflate --test
zlib_gzip`.

The sources are distributed under CRuby's terms: the Ruby License
(`COPYING`) or the 2-clause BSD License (`BSDL`), both reproduced here.
