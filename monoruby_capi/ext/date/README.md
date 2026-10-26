# ext/date from CRuby, built against the C API layer

`date_core.c`, `date_parse.c`, `date_strftime.c`, `date_strptime.c`,
`date_tmx.h` and `zonetab.h` are CRuby 4.0.6's `ext/date` (the date gem
3.5.1), unmodified. `monoruby/build.rs` (`build_capi_extensions`)
compiles them against `monoruby_capi/include`, links the layer's
archives in, and installs the result as `<install root>/ext/date_core.so`
(`.dylib` on macOS), where `require "date_core"` -- from
`monoruby/stdlib/date.rb`, CRuby's `ext/date/lib/date.rb` -- finds it.
See `doc/c_api.md` §3.17.

To update: copy the same files from a newer CRuby and run the date specs
(`spec/ruby/library/date`, `datetime`, `time`).

The sources are distributed under CRuby's terms: the Ruby License
(`COPYING`) or the 2-clause BSD License (`BSDL`), both reproduced here.
