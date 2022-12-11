# stack layout for the bytecode interpreter/ JIT-ed code (just after prologue)

```text
       +-------------+
 +0x08 | return addr |
       +-------------+
  0x00 |  prev rbp   | <- rbp
       +-------------+
 -0x08 |  prev cfp   |
       +-------------+
 -0x10 |    outer    |
       +-------------+ ------ +-------------------+
 -0x18 |    meta     |        |0:VM 1:JIT 2:Native| -0x12
       +-------------+        +-------------------+
 -0x20 |    block    | \      |    register_len   | -0x14
       +-------------+  \     +-------------------+
 -0x28 |    self     |   \    |                   | -0x16
       +-------------+    \   +       FuncId      +
 -0x30 |    arg0     |     \  |                   | -0x18
       +-------------+      \ +-------------------+
       |      :      |
       +-------------+
       |   arg(n-1)  |
       +-------------+
 -0xy0 |             | <- rsp
       +-------------+
       |      :      |
```

## ABI of JIT-compiled code

### argument registers

- rdi: number of args

### global registers

- rbx: &mut Interp
- r12: &mut Globals
- r13: pc (dummy for JIT-ed code)
