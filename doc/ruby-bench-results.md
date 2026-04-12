# ruby-bench results (2026-04-13)

monoruby v0.3.0 on x86-64 Linux, single run (`--once`).

## Summary

- **Total benchmarks**: 72
- **Passed**: 51
- **Failed**: 21
- **Newly fixed** (this session): addressable-equality, addressable-merge, addressable-normalize, knucleotide, liquid-compile, protoboeuf-encode

## Successful benchmarks (time in ms)

| Benchmark | Time (ms) |
|---|---:|
| setivar_young | 3.3 |
| setivar_object | 3.5 |
| setivar | 3.7 |
| structaset | 4.6 |
| throw | 6.3 |
| structaref | 6.8 |
| getivar | 7.3 |
| attr_accessor | 7.9 |
| nbody | 12.1 |
| cfunc_itself | 13.1 |
| send_rubyfunc_block | 15.2 |
| getivar-module | 16.7 |
| nqueens | 16.8 |
| object-new-initialize | 24.0 |
| fib | 30.8 |
| keyword_args | 31.4 |
| send_bmethod | 33.8 |
| knucleotide | 35.9 |
| object-new | 47.9 |
| object-new-no-escape | 50.2 |
| str_concat | 51.8 |
| ruby-xor | 57.2 |
| matmul | 57.3 |
| sudoku | 58.5 |
| blurhash | 70.9 |
| fannkuchredux | 73.8 |
| protoboeuf | 75.1 |
| send_cfunc_block | 87.7 |
| addressable-new | 102.1 |
| protoboeuf-encode | 108.6 |
| loops-times | 123.6 |
| addressable-setters | 129.3 |
| liquid-compile | 133.9 |
| 30k_methods | 155.8 |
| addressable-getters | 173.2 |
| addressable-merge | 176.6 |
| addressable-to-s | 211.0 |
| binarytrees | 231.6 |
| addressable-parse | 242.5 |
| respond_to | 244.7 |
| addressable-join | 356.6 |
| splay | 428.3 |
| lee | 439.4 |
| addressable-normalize | 441.9 |
| 30k_ifelse | 446.4 |
| rubykon | 587.9 |
| addressable-equality | 840.3 |
| optcarrot | 852.9 |
| gcbench | 1499.2 |
| rubyboy | 2857.0 |
| psych-load | 2910.3 |

## Failed benchmarks (21) by category

### 1. Missing C extension stubs (6 benchmarks)

No `.rb` replacement exists for the native `.so` extension.

| Benchmark | Missing stub |
|---|---|
| activerecord, mail | `x86_64-linux/io/wait.rb` (io-wait C extension) |
| graphql | `x86_64-linux/digest.rb` (digest C extension) |
| graphql-native | `graphql/graphql_c_parser_ext.rb` (graphql gem C parser) |
| fluentd | `yajl/yajl.rb` (yajl-ruby gem) |
| liquid-c | `liquid_c.rb` (liquid-c gem) |

### 2. Missing Module#singleton_class? (3 benchmarks)

ActiveSupport's `mattr_reader` calls `singleton_class?` which is not implemented.

| Benchmark | Error |
|---|---|
| erubi-rails | `undefined method 'singleton_class?' for ActiveSupport::Logger` |
| railsbench | `undefined method 'singleton_class?' for ActiveSupport::Logger` |
| shipit | `undefined method 'singleton_class?' for ActiveSupport::Logger` |

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

### 7. Core method issues (1 benchmark)

| Benchmark | Error |
|---|---|
| ruby-lsp | `undefined method 'first' for ""` (sorbet-runtime internal) |

### 8. Runtime issues (1 benchmark)

| Benchmark | Error |
|---|---|
| liquid-render | `RuntimeError` in liquid.rb (empty message, likely Liquid version check) |

### 9. Unknown (3 benchmarks)

| Benchmark | Error |
|---|---|
| erubi | (no clear error captured) |
| etanni | (no clear error captured) |
| tinygql | (no clear error captured) |
