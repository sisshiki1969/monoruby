# stack layout for the bytecode interpreter/ JIT-ed code (just after prologue)

```text

stack frame structure
       +-------------+
 +0x08 | return addr |
       +-------------+
  0x00 |  prev rbp   | <- rbp
       +-------------+
 -0x08 |  prev cfp   |
       +-------------+
 -0x10 |     lfp     |
       +-------------+
 -0x18 |    outer    |
       +-------------+
 -0x20 |    meta     |
       +-------------+
 -0x28 |    block    |
       +-------------+
 -0x30 |    self     |
       +-------------+
 -0x38 |    arg0     |
       +-------------+
       |      :      |
       +-------------+
       |   arg(n-1)  |
       +-------------+
 -0xy0 |             | <- rsp
       +-------------+
       |      :      |
```

## ABI of interpreter and JIT-ed code

### argument register

- rdi: number of args

### global registers (callee save)

- rbx: &mut Executer ([rbx] points to cfp)
- r12: &mut Globals
- r13: pc (current bytecode address, dummy for JIT-ed code)
- r14: lfp (local frame pointer, points to the address of _self_)
