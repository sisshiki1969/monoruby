# ruruby-parse

[![Rust](https://github.com/sisshiki1969/ruruby-parse/actions/workflows/rust.yml/badge.svg?branch=master)](https://github.com/sisshiki1969/ruruby-parse/actions/workflows/rust.yml)
[![codecov](https://codecov.io/gh/sisshiki1969/ruruby-parse/branch/master/graph/badge.svg?token=vAvpafdKER)](https://codecov.io/gh/sisshiki1969/ruruby-parse)

a Ruby parser written in Rust.

## using as a library

```Rust
pub fn parse_program(code: String, path: impl Into<PathBuf>, context_name: &str) -> Result<ParseResult, ParseErr>
```


## comand line usage

### parse script file and print

```sh
> cargo run -- <awesome-ruby-script.rb>
```

### one liner

```sh
> cargo run -- -e 1+1
```
