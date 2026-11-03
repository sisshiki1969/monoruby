# gumbo-parser (vendored)

`src/` is the `gumbo-parser/src` directory of the nokogiri 1.19.7 gem
(https://rubygems.org/gems/nokogiri, the platform-independent release),
nokogiri's fork of the libgumbo HTML5 parser, copied unmodified. It is
licensed under the Apache License, Version 2.0 (see the header of every
source file and `README.md` for the fork's history):

    http://www.apache.org/licenses/LICENSE-2.0

except `hashmap.c` / `hashmap.h` (https://github.com/tidwall/hashmap.c),
which nokogiri vendors under the MIT License:

    https://github.com/tidwall/hashmap.c/blob/master/LICENSE

    The MIT License (MIT)

    Copyright (c) 2020 Joshua J Baker

    Permission is hereby granted, free of charge, to any person obtaining a copy of
    this software and associated documentation files (the "Software"), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    the Software, and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
    FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

The generated files (`char_ref.c`, `foreign_attrs.c`, `svg_attrs.c`,
`svg_tags.c`, `tag_lookup.c`) are checked in as nokogiri ships them; the
`.rl` / `.gperf` sources they come from are not needed for the build and
are left out.

monoruby builds it with `cc` from `libxml2-src/build.rs` (`-std=c99`,
`-O2`, as nokogiri's extconf does) into the `gumbo` archive, together with
`glue/monoruby_gumbo.c`.
