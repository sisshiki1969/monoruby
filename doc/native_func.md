### native method definition with optional / rest / keyword parameters

```rust
globals.define_builtin_class_func_with_kw(klass, "xxx", xxx, min: 1, max: 2, rest: true, kw: &["base", "sort"]);
```

- min: required arguments
- max: required + optional arguments
- rest: rest argument
- kw: keyword arguments

in this examples, the method `xxx` has 1 required argument(=`arg0`), 1 optional argument(=`arg1`), rest argument(=`arg2`), and 2 keyword arguments (`base`(=`arg3`) and `sort`(=`arg4`)).

```text
arg0: rewuired ----+
arg1: optional ----+-- positional
arg2: rest --------+
arg3: keyword("base")
arg4: keyword("sort")
```
