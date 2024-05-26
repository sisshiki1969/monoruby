### native method definition

```rust
globals.define_builtin_class_func_with_kw(klass, "xxx", xxx, min: 1, max: 2, rest: true, kw: &["base", "sort"]);
```

- min: required arguments
- max: required + optional arguments
- rest: rest argument
- kw: keyword arguments

arg0: rewuired ----+
arg1: optional +-- poritional
arg2: rest ----+
arg3: keyword("base")
arg4: keyword("sort")
