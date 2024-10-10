# Stack layout for the bytecode interpreter/ JIT-ed code

## stack frame structure (just after prologue)

```text
             +-------------+
             | return addr |
             +-------------+
 BP->        |  prev rbp   | <- rbp
             +-------------+-----------------
CFP->        |  prev cfp   |
             +-------------+  control frame
             |     lfp     |
             +-------------+-----------------
DFP->  -0x00 |    outer    | <- r14
             +-------------+
       -0x08 |    meta     |
             +-------------+
       -0x10 |    block    |
             +-------------+
       -0x18 |    self     |  local frame
             +-------------+
       -0x20 |    arg0     |
             +-------------+
             |      :      |
             +-------------+
             |   arg(n-1)  |
             +-------------+------------------
       -0xy0 |             | <- rsp
             +-------------+
             |      :      |
```

## stack frame structure (just before call)

```text
             +-------------+
       -0x00 |             | <- rsp
             +-------------+
       -0x08 |             |
             +-------------+
       -0x10 |             |
             +-------------+-----------------
       -0x18 |  prev cfp   |
             +-------------+  control frame
       -0x20 |     lfp     |
             +-------------+-----------------
       -0x28 |    outer    | <- r14
             +-------------+
       -0x30 |    meta     |
             +-------------+
       -0x38 |    block    |
             +-------------+
       -0x40 |    self     |  local frame
             +-------------+
       -0x48 |    arg0     |
             +-------------+
             |      :      |
             +-------------+
             |   arg(n-1)  |
             +-------------+------------------
             |      :      |
```

## ABI of interpreter and JIT-ed code

### global registers (callee save)

- rbx: &mut Executer ([rbx] points to cfp)
- r12: &mut Globals
- r13: pc (current bytecode address, dummy for JIT-ed code)
- r14: lfp (local frame pointer)
