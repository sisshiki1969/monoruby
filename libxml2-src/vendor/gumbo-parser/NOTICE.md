# gumbo-parser (vendored)

`src/` is the `gumbo-parser/src` directory of the nokogiri 1.19.1 gem
(https://rubygems.org/gems/nokogiri, the platform-independent release),
nokogiri's fork of the libgumbo HTML5 parser, copied unmodified. It is
licensed under the Apache License, Version 2.0 (see the header of every
source file and `README.md` for the fork's history):

    http://www.apache.org/licenses/LICENSE-2.0

The generated files (`char_ref.c`, `foreign_attrs.c`, `svg_attrs.c`,
`svg_tags.c`, `tag_lookup.c`) are checked in as nokogiri ships them; the
`.rl` / `.gperf` sources they come from are not needed for the build and
are left out.

monoruby builds it with `cc` from `libxml2-src/build.rs` (`-std=c99`,
`-O2`, as nokogiri's extconf does) into the `gumbo` archive, together with
`glue/monoruby_gumbo.c`.
