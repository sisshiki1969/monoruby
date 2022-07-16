# stack layout for the bytecode interpreter/ JIT-ed code (just after prologue)

~~~text
       +-------------+
 +0x08 | return addr |
       +-------------+
  0x00 |  prev rbp   | <- rbp
       +-------------+ ------ +-------------------+ +8
 -0x08 |    meta     |        |0:VM 1:JIT 2:Native|   
       +-------------+        +-------------------+ +6
 -0x10 |     %0      | \      |    register_len   |   
       +-------------+  \     +-------------------+ +4
 -0x18 |     %1      |   \    |                   |
       +-------------+    \   +       FuncId      + +2
       |      :      |     \  |                   |
       +-------------+      \ +-------------------+ +0
 -0xy0 |    %(n-1)   | <- rsp       
       +-------------+
       |      :      |
 ~~~

## ABI of JIT-compiled code

### argument registers

- rdi: number of args

### global registers

- rbx: &mut Interp
- r12: &mut Globals
- r13: pc (dummy for JIT-ed code)

## stack layout when just after the code is called

 ~~~text
       +-------------+
 -0x00 | return addr | <- rsp
       +-------------+
 -0x08 |  (old rbp)  |
       +-------------+
 -0x10 |    meta     |
       +-------------+
 -0x18 |     %0      |
       +-------------+
 -0x20 | %1(1st arg) |
       +-------------+
       |             |
 ~~~~

- meta and arguments is set by caller.
- (old rbp) is to be set by callee.
