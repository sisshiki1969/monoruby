# ruby-bench results (2026-04-12)

monoruby v0.3.0 on x86-64 Linux, single run (`--once`).

## Summary

- **Total benchmarks**: 72
- **Passed**: 50
- **Failed**: 22
- **Newly fixed** (this session): addressable-equality, addressable-merge, addressable-normalize, knucleotide, liquid-compile

## Successful benchmarks (time in ms)

| Benchmark | Time (ms) |
|---|---:|
| setivar | 3.7 |
| setivar_object | 3.9 |
| setivar_young | 4.6 |
| structaset | 5.7 |
| throw | 7.3 |
| getivar | 7.6 |
| attr_accessor | 7.9 |
| structaref | 8.3 |
| cfunc_itself | 14.9 |
| send_rubyfunc_block | 15.0 |
| nbody | 16.3 |
| getivar-module | 17.4 |
| nqueens | 20.7 |
| fib | 25.8 |
| object-new-initialize | 26.4 |
| send_bmethod | 35.1 |
| knucleotide | 39.8 |
| keyword_args | 40.3 |
| str_concat | 56.7 |
| object-new-no-escape | 61.4 |
| ruby-xor | 65.3 |
| object-new | 70.6 |
| sudoku | 70.8 |
| fannkuchredux | 73.4 |
| protoboeuf | 77.3 |
| matmul | 77.8 |
| addressable-new | 78.1 |
| blurhash | 79.8 |
| send_cfunc_block | 83.5 |
| addressable-setters | 130.7 |
| liquid-compile | 137.7 |
| loops-times | 142.0 |
| addressable-getters | 146.6 |
| 30k_methods | 155.2 |
| addressable-to-s | 166.3 |
| addressable-merge | 175.9 |
| addressable-parse | 242.3 |
| binarytrees | 262.0 |
| respond_to | 294.6 |
| addressable-normalize | 384.6 |
| addressable-join | 401.2 |
| 30k_ifelse | 421.3 |
| lee | 463.9 |
| splay | 482.7 |
| rubykon | 622.9 |
| addressable-equality | 693.3 |
| optcarrot | 1078.8 |
| gcbench | 1431.3 |
| rubyboy | 3431.2 |
| psych-load | 3566.9 |

## Failed benchmarks (22) by category

### 1. Missing C extension stubs (6 benchmarks)

No `.rb` replacement exists for the native `.so` extension.

| Benchmark | Missing stub |
|---|---|
| activerecord, mail | `x86_64-linux/io/wait.rb` (io-wait C extension) |
| graphql | `x86_64-linux/digest.rb` (digest C extension) |
| graphql-native | `graphql/graphql_c_parser_ext.rb` (graphql gem C parser) |
| fluentd | `yajl/yajl.rb` (yajl-ruby gem) |
| liquid-c | `liquid_c.rb` (liquid-c gem) |

### 2. Missing File#path method (3 benchmarks)

`File#path` is not implemented. Logger gem requires it.

| Benchmark | Error |
|---|---|
| erubi-rails | `undefined method 'path' for #<File:fd 120>` |
| railsbench | `undefined method 'path' for #<File:fd 111>` |
| shipit | `undefined method 'path' for #<File:fd 5>` |

### 3. Missing Thread support (1 benchmark)

| Benchmark | Error |
|---|---|
| lobsters | `undefined method 'start' for Thread` (Bundler worker threads) |

### 4. Parser limitations (2 benchmarks)

| Benchmark | Error |
|---|---|
| chunky-png | `unexpected end-of-file` (complex here-doc or multi-line syntax) |
| hexapdf | `Unexpected token: Punct(Assign)` (pattern matching `=>` syntax) |

### 5. Regexp engine limitations (2 benchmarks)

| Benchmark | Error |
|---|---|
| rack | `undefined group option` (unsupported Regexp syntax in Onigmo) |
| ruby-json | `undefined method 'source' for {` (Regexp#source on a non-Regexp in strscan.rb) |

### 6. Constant / module resolution issues (2 benchmarks)

| Benchmark | Error |
|---|---|
| sequel | `uninitialized constant Module` |
| rubocop | `uninitialized constant V4_0_0` (dynamic `const_get` for Ruby 4.0 version) |

### 7. Core method / type conversion issues (3 benchmarks)

| Benchmark | Error |
|---|---|
| erubi | `no implicit conversion of Range into Integer` (Range used as String index) |
| protoboeuf-encode | `index 11 out of string` (String byte manipulation bug) |
| ruby-lsp | `undefined method 'first' for ""` (sorbet-runtime internal) |

### 8. Runtime issues (1 benchmark)

| Benchmark | Error |
|---|---|
| liquid-render | `RuntimeError` in liquid.rb (empty message, likely Liquid version check) |

### 9. Unknown (2 benchmarks)

| Benchmark | Error |
|---|---|
| etanni | (no error message captured) |
| tinygql | (no error message captured) |
