==> start whole compile: bottom_up_tree FuncId(144) self_class:Object start:[00000] bytecode:0x55bdf6d6b910
<== finished compile. elapsed:27.7µs
offset:Pos(20777) code: 385 bytes  data: 4 bytes
  00000: push   rbp
  00001: mov    rbp,rsp
  00004: sub    rsp,0x50
  00008: mov    QWORD PTR [r14-0x40],0x4
  00010: mov    QWORD PTR [r14-0x48],0x4
  00018: mov    rdi,QWORD PTR [r14-0x30]
  0001c: test   rdi,0x7
  00023: jne    0x3b027
  00029: cmp    DWORD PTR [rdi+0x4],0x1
  0002d: jne    0x3b027
:00000 init_method reg:3 arg:1 req:1 opt:0 rest:false block:None stack_offset:5
:00001 _%2 = %1 > 0: i16                    [Integer][Integer]
  00033: mov    rdi,QWORD PTR [r14-0x38]
  00037: test   rdi,0x1
  0003e: je     0x3b036
  00044: cmp    rdi,0x1
  00048: jle    0x53
:00002 condnotbr _%2 =>:00004
:00003 br =>:00006
  0004e: jmp    0x73
:00004 %2 = literal[[nil, nil]]
  00053: movabs rdi,0x7f4ab3202a00
  0005d: movabs rax,0x55bdf68472c0
  00067: call   rax
  00069: mov    QWORD PTR [r14-0x40],rax
:00005 ret %2
  0006d: mov    rax,QWORD PTR [r14-0x40]
  00071: leave  
  00072: ret    
:00006 %1 = %1 - 1: i16                     [Integer][Integer]
  00073: mov    rdi,QWORD PTR [r14-0x38]
  00077: test   rdi,0x1
  0007e: je     0x3b045
  00084: sub    rdi,0x2
  00088: jo     0x3b045
  0008e: mov    QWORD PTR [r14-0x38],rdi
:00007 %2 = %1
  00092: mov    rax,QWORD PTR [r14-0x38]
  00096: mov    QWORD PTR [r14-0x40],rax
:00008 %2 = %0.call bottom_up_tree(%2; 1)   [Object]
  0009a: mov    rdi,QWORD PTR [r14-0x30]
  0009e: cmp    DWORD PTR [rip+0xfffffffffffffc6d],0xc        # 0xfffffd12
  000a5: jne    0x3b054
  000ab: cmp    DWORD PTR [rip+0xfffffffffffffc6d],0x0        # 0xfffffd1f
  000b2: jne    0x3b063
  000b8: mov    QWORD PTR [rsp-0x28],0x0
  000c1: mov    rax,QWORD PTR [r14-0x30]
  000c5: mov    QWORD PTR [rsp-0x40],rax
  000ca: mov    rax,QWORD PTR [r14-0x40]
  000ce: mov    QWORD PTR [rsp-0x48],rax
  000d3: mov    rdi,0x1
  000da: mov    QWORD PTR [rsp-0x38],0x0
  000e3: mov    rdx,rdi
  000e6: movabs rax,0x1000400000090
  000f0: mov    QWORD PTR [rsp-0x30],rax
  000f5: movabs r13,0x55bdf6d6b910
  000ff: mov    rsi,QWORD PTR [rbx]
  00102: mov    QWORD PTR [rsp-0x18],rsi
  00107: lea    rsi,[rsp-0x18]
  0010c: mov    QWORD PTR [rbx],rsi
  0010f: lea    r14,[rsp-0x10]
  00114: mov    QWORD PTR [rsp-0x20],r14
  00119: call   0xffffffa9
  0011e: lea    r14,[rbp-0x8]
  00122: mov    QWORD PTR [rbx],r14
  00125: mov    r14,QWORD PTR [rbp-0x10]
  00129: test   rax,rax
  0012c: je     0x3b07a
  00132: mov    QWORD PTR [r14-0x40],rax
:00009 
:00010 %3 = %1
  00136: mov    rax,QWORD PTR [r14-0x38]
  0013a: mov    QWORD PTR [r14-0x48],rax
:00011 %3 = %0.call bottom_up_tree(%3; 1)   [<INVALID>]
  0013e: cmp    DWORD PTR [rip+0x3c],0x0        # 0x181
  00145: jl     0x3b089
  0014b: je     0x3b098
  00151: sub    DWORD PTR [rip+0x29],0x1        # 0x181
  00158: xor    rdi,rdi
  0015b: jmp    0x3b089
:00012 
:00013 %2 = array[%2; 2]
  00160: lea    rdi,[r14-0x40]
  00164: mov    rsi,0x2
  0016b: movabs rax,0x55bdf688c440
  00175: call   rax
  00177: mov    QWORD PTR [r14-0x40],rax
:00014 ret %2
  0017b: mov    rax,QWORD PTR [r14-0x40]
  0017f: leave  
  00180: ret    
==> start whole compile: bottom_up_tree FuncId(144) self_class:Object start:[00000] bytecode:0x55bdf6d6b910
<== finished compile. elapsed:42.8µs
offset:Pos(21166) code: 507 bytes  data: 0 bytes
  00000: push   rbp
  00001: mov    rbp,rsp
  00004: sub    rsp,0x50
  00008: mov    QWORD PTR [r14-0x40],0x4
  00010: mov    QWORD PTR [r14-0x48],0x4
  00018: mov    rdi,QWORD PTR [r14-0x30]
  0001c: test   rdi,0x7
  00023: jne    0x3af2f
  00029: cmp    DWORD PTR [rdi+0x4],0x1
  0002d: jne    0x3af2f
:00000 init_method reg:3 arg:1 req:1 opt:0 rest:false block:None stack_offset:5
:00001 _%2 = %1 > 0: i16                    [Integer][Integer]
  00033: mov    rdi,QWORD PTR [r14-0x38]
  00037: test   rdi,0x1
  0003e: je     0x3af3e
  00044: cmp    rdi,0x1
  00048: jle    0x53
:00002 condnotbr _%2 =>:00004
:00003 br =>:00006
  0004e: jmp    0x73
:00004 %2 = literal[[nil, nil]]
  00053: movabs rdi,0x7f4ab3202a00
  0005d: movabs rax,0x55bdf68472c0
  00067: call   rax
  00069: mov    QWORD PTR [r14-0x40],rax
:00005 ret %2
  0006d: mov    rax,QWORD PTR [r14-0x40]
  00071: leave  
  00072: ret    
:00006 %1 = %1 - 1: i16                     [Integer][Integer]
  00073: mov    rdi,QWORD PTR [r14-0x38]
  00077: test   rdi,0x1
  0007e: je     0x3af4d
  00084: sub    rdi,0x2
  00088: jo     0x3af4d
  0008e: mov    QWORD PTR [r14-0x38],rdi
:00007 %2 = %1
  00092: mov    rax,QWORD PTR [r14-0x38]
  00096: mov    QWORD PTR [r14-0x40],rax
:00008 %2 = %0.call bottom_up_tree(%2; 1)   [Object]
  0009a: mov    rdi,QWORD PTR [r14-0x30]
  0009e: cmp    DWORD PTR [rip+0xfffffffffffffae8],0xc        # 0xfffffb8d
  000a5: jne    0x3af5c
  000ab: cmp    DWORD PTR [rip+0xfffffffffffffae8],0x0        # 0xfffffb9a
  000b2: jne    0x3af6b
  000b8: mov    QWORD PTR [rsp-0x28],0x0
  000c1: mov    rax,QWORD PTR [r14-0x30]
  000c5: mov    QWORD PTR [rsp-0x40],rax
  000ca: mov    rax,QWORD PTR [r14-0x40]
  000ce: mov    QWORD PTR [rsp-0x48],rax
  000d3: mov    rdi,0x1
  000da: mov    QWORD PTR [rsp-0x38],0x0
  000e3: mov    rdx,rdi
  000e6: movabs rax,0x1000400000090
  000f0: mov    QWORD PTR [rsp-0x30],rax
  000f5: movabs r13,0x55bdf6d6b910
  000ff: mov    rsi,QWORD PTR [rbx]
  00102: mov    QWORD PTR [rsp-0x18],rsi
  00107: lea    rsi,[rsp-0x18]
  0010c: mov    QWORD PTR [rbx],rsi
  0010f: lea    r14,[rsp-0x10]
  00114: mov    QWORD PTR [rsp-0x20],r14
  00119: call   0xfffffe24
  0011e: lea    r14,[rbp-0x8]
  00122: mov    QWORD PTR [rbx],r14
  00125: mov    r14,QWORD PTR [rbp-0x10]
  00129: test   rax,rax
  0012c: je     0x3af82
  00132: mov    QWORD PTR [r14-0x40],rax
:00009 
:00010 %3 = %1
  00136: mov    rax,QWORD PTR [r14-0x38]
  0013a: mov    QWORD PTR [r14-0x48],rax
:00011 %3 = %0.call bottom_up_tree(%3; 1)   [Object]
  0013e: mov    rdi,QWORD PTR [r14-0x30]
  00142: cmp    DWORD PTR [rip+0xfffffffffffffa44],0xc        # 0xfffffb8d
  00149: jne    0x3af91
  0014f: cmp    DWORD PTR [rip+0xfffffffffffffa44],0x0        # 0xfffffb9a
  00156: jne    0x3afa0
  0015c: mov    QWORD PTR [rsp-0x28],0x0
  00165: mov    rax,QWORD PTR [r14-0x30]
  00169: mov    QWORD PTR [rsp-0x40],rax
  0016e: mov    rax,QWORD PTR [r14-0x48]
  00172: mov    QWORD PTR [rsp-0x48],rax
  00177: mov    rdi,0x1
  0017e: mov    QWORD PTR [rsp-0x38],0x0
  00187: mov    rdx,rdi
  0018a: movabs rax,0x1000400000090
  00194: mov    QWORD PTR [rsp-0x30],rax
  00199: movabs r13,0x55bdf6d6b910
  001a3: mov    rsi,QWORD PTR [rbx]
  001a6: mov    QWORD PTR [rsp-0x18],rsi
  001ab: lea    rsi,[rsp-0x18]
  001b0: mov    QWORD PTR [rbx],rsi
  001b3: lea    r14,[rsp-0x10]
  001b8: mov    QWORD PTR [rsp-0x20],r14
  001bd: call   0xfffffe24
  001c2: lea    r14,[rbp-0x8]
  001c6: mov    QWORD PTR [rbx],r14
  001c9: mov    r14,QWORD PTR [rbp-0x10]
  001cd: test   rax,rax
  001d0: je     0x3afb7
  001d6: mov    QWORD PTR [r14-0x48],rax
:00012 
:00013 %2 = array[%2; 2]
  001da: lea    rdi,[r14-0x40]
  001de: mov    rsi,0x2
  001e5: movabs rax,0x55bdf688c440
  001ef: call   rax
  001f1: mov    QWORD PTR [r14-0x40],rax
:00014 ret %2
  001f5: mov    rax,QWORD PTR [r14-0x40]
  001f9: leave  
  001fa: ret    
==> start partial compile: <unnamed> FuncId(145) self_class:Object start:[00010] bytecode:0x55bdf6d85fa0
<-- compile finished. end:[00018]
<== finished compile. elapsed:80.6µs
offset:Pos(21822) code: 248 bytes  data: 0 bytes
:00010 loop_start counter=5 jit-addr=0000000000000000
:00011 _%8 = %4 > %7                        [Integer][Integer]
  00000: mov    rdi,QWORD PTR [r14-0x50]
  00004: mov    rsi,QWORD PTR [r14-0x68]
  00008: test   rdi,0x1
  0000f: je     0x3ad4f
  00015: test   rsi,0x1
  0001c: je     0x3ad4f
  00022: cmp    rdi,rsi
  00025: jg     0xf3
:00012 condbr _%8 =>:00018
:00013 %8 = %1
  0002b: mov    rax,QWORD PTR [r14-0x38]
  0002f: mov    QWORD PTR [r14-0x70],rax
:00014 %5 = %0.call bottom_up_tree(%8; 1)   [Object]
  00033: mov    rdi,QWORD PTR [r14-0x30]
  00037: cmp    DWORD PTR [rip+0xfffffffffffff8bf],0xc        # 0xfffff8fd
  0003e: jne    0x3ad5e
  00044: cmp    DWORD PTR [rip+0xfffffffffffff8bf],0x0        # 0xfffff90a
  0004b: jne    0x3ad6d
  00051: mov    QWORD PTR [rsp-0x28],0x0
  0005a: mov    rax,QWORD PTR [r14-0x30]
  0005e: mov    QWORD PTR [rsp-0x40],rax
  00063: mov    rax,QWORD PTR [r14-0x70]
  00067: mov    QWORD PTR [rsp-0x48],rax
  0006c: mov    rdi,0x1
  00073: mov    QWORD PTR [rsp-0x38],0x0
  0007c: mov    rdx,rdi
  0007f: movabs rax,0x1000400000090
  00089: mov    QWORD PTR [rsp-0x30],rax
  0008e: movabs r13,0x55bdf6d6b910
  00098: mov    rsi,QWORD PTR [rbx]
  0009b: mov    QWORD PTR [rsp-0x18],rsi
  000a0: lea    rsi,[rsp-0x18]
  000a5: mov    QWORD PTR [rbx],rsi
  000a8: lea    r14,[rsp-0x10]
  000ad: mov    QWORD PTR [rsp-0x20],r14
  000b2: call   0xfffffb94
  000b7: lea    r14,[rbp-0x8]
  000bb: mov    QWORD PTR [rbx],r14
  000be: mov    r14,QWORD PTR [rbp-0x10]
  000c2: test   rax,rax
  000c5: je     0x3ad84
  000cb: mov    QWORD PTR [r14-0x58],rax
:00015 
:00016 %4 = %4 + 1: i16                     [Integer][Integer]
  000cf: mov    rdi,QWORD PTR [r14-0x50]
  000d3: test   rdi,0x1
  000da: je     0x3ad93
  000e0: add    rdi,0x2
  000e4: jo     0x3ad93
  000ea: mov    QWORD PTR [r14-0x50],rdi
:00017 br =>:00010
  000ee: jmp    0x3adb1
:00018 loop_end
  000f3: jmp    0x3ada2
==> start whole compile: <unnamed> FuncId(145) self_class:Object start:[00000] bytecode:0x55bdf6d85fa0
<== finished compile. elapsed:46.9µs
offset:Pos(22070) code: 564 bytes  data: 0 bytes
  00000: push   rbp
  00001: mov    rbp,rsp
  00004: sub    rsp,0x80
  0000b: mov    rax,0x4
  00012: mov    QWORD PTR [r14-0x40],rax
  00016: mov    QWORD PTR [r14-0x48],rax
  0001a: mov    QWORD PTR [r14-0x50],rax
  0001e: mov    QWORD PTR [r14-0x58],rax
  00022: mov    QWORD PTR [r14-0x60],rax
  00026: mov    QWORD PTR [r14-0x68],rax
  0002a: mov    QWORD PTR [r14-0x70],rax
  0002e: mov    rdi,QWORD PTR [r14-0x30]
  00032: test   rdi,0x7
  00039: jne    0x3acd2
  0003f: cmp    DWORD PTR [rdi+0x4],0x1
  00043: jne    0x3acd2
:00000 init_block reg:8 arg:1 req:1 opt:0 rest:false block:None stack_offset:8
:00001 %6 = dynvar(1, %1)
  00049: mov    rax,QWORD PTR [r14-0x18]
  0004d: mov    rax,QWORD PTR [rax-0x20]
  00051: mov    QWORD PTR [r14-0x60],rax
:00002 %6 = %6 - %1                         [Integer][Integer]
  00055: mov    rdi,QWORD PTR [r14-0x60]
  00059: mov    rsi,QWORD PTR [r14-0x38]
  0005d: test   rdi,0x1
  00064: je     0x3ace1
  0006a: test   rsi,0x1
  00071: je     0x3ace1
  00077: sub    rdi,rsi
  0007a: jo     0x3ace1
  00080: add    rdi,0x1
  00084: mov    QWORD PTR [r14-0x60],rdi
:00003 %7 = dynvar(1, %2)
  00088: mov    rax,QWORD PTR [r14-0x18]
  0008c: mov    rax,QWORD PTR [rax-0x28]
  00090: mov    QWORD PTR [r14-0x68],rax
:00004 %6 = %6 + %7                         [Integer][Integer]
  00094: mov    rdi,QWORD PTR [r14-0x60]
  00098: mov    rsi,QWORD PTR [r14-0x68]
  0009c: test   rdi,0x1
  000a3: je     0x3acf0
  000a9: test   rsi,0x1
  000b0: je     0x3acf0
  000b6: sub    rdi,0x1
  000ba: add    rdi,rsi
  000bd: jo     0x3acf0
  000c3: mov    QWORD PTR [r14-0x60],rdi
:00005 %2 = 2: i16 ** %6                    [Integer][Integer]
  000c7: mov    rsi,QWORD PTR [r14-0x60]
  000cb: test   rsi,0x1
  000d2: je     0x3acff
  000d8: mov    rdi,0x5
  000df: sar    rdi,1
  000e2: sar    rsi,1
  000e5: movabs rax,0x55bdf68537e0
  000ef: call   rax
  000f1: mov    QWORD PTR [r14-0x40],rax
:00006 %3 = 0: i32
  000f5: mov    QWORD PTR [r14-0x48],0x1
:00007 %4 = 1: i32
  000fd: mov    QWORD PTR [r14-0x50],0x3
:00008 %7 = %2
  00105: mov    rax,QWORD PTR [r14-0x40]
  00109: mov    QWORD PTR [r14-0x68],rax
:00009 %6 = %4 .. %7
  0010d: mov    rdi,QWORD PTR [r14-0x50]
  00111: mov    rsi,QWORD PTR [r14-0x68]
  00115: mov    rdx,r12
  00118: mov    ecx,0x0
  0011d: movabs rax,0x55bdf688c770
  00127: call   rax
  00129: test   rax,rax
  0012c: je     0x3ad0e
  00132: mov    QWORD PTR [r14-0x60],rax
  00136: jmp    0x3ad1d
:00010 loop_start counter=5 jit-addr=00007f4ab310553e
:00011 _%8 = %4 > %7                        [Integer][Integer]
  0013b: mov    rdi,QWORD PTR [r14-0x50]
  0013f: mov    rsi,QWORD PTR [r14-0x68]
  00143: test   rdi,0x1
  0014a: je     0x3ad36
  00150: test   rsi,0x1
  00157: je     0x3ad36
  0015d: cmp    rdi,rsi
  00160: jg     0x22e
:00012 condbr _%8 =>:00018
:00013 %8 = %1
  00166: mov    rax,QWORD PTR [r14-0x38]
  0016a: mov    QWORD PTR [r14-0x70],rax
:00014 %5 = %0.call bottom_up_tree(%8; 1)   [Object]
  0016e: mov    rdi,QWORD PTR [r14-0x30]
  00172: cmp    DWORD PTR [rip+0xfffffffffffff68c],0xc        # 0xfffff805
  00179: jne    0x3ad45
  0017f: cmp    DWORD PTR [rip+0xfffffffffffff68c],0x0        # 0xfffff812
  00186: jne    0x3ad54
  0018c: mov    QWORD PTR [rsp-0x28],0x0
  00195: mov    rax,QWORD PTR [r14-0x30]
  00199: mov    QWORD PTR [rsp-0x40],rax
  0019e: mov    rax,QWORD PTR [r14-0x70]
  001a2: mov    QWORD PTR [rsp-0x48],rax
  001a7: mov    rdi,0x1
  001ae: mov    QWORD PTR [rsp-0x38],0x0
  001b7: mov    rdx,rdi
  001ba: movabs rax,0x1000400000090
  001c4: mov    QWORD PTR [rsp-0x30],rax
  001c9: movabs r13,0x55bdf6d6b910
  001d3: mov    rsi,QWORD PTR [rbx]
  001d6: mov    QWORD PTR [rsp-0x18],rsi
  001db: lea    rsi,[rsp-0x18]
  001e0: mov    QWORD PTR [rbx],rsi
  001e3: lea    r14,[rsp-0x10]
  001e8: mov    QWORD PTR [rsp-0x20],r14
  001ed: call   0xfffffa9c
  001f2: lea    r14,[rbp-0x8]
  001f6: mov    QWORD PTR [rbx],r14
  001f9: mov    r14,QWORD PTR [rbp-0x10]
  001fd: test   rax,rax
  00200: je     0x3ad6b
  00206: mov    QWORD PTR [r14-0x58],rax
:00015 
:00016 %4 = %4 + 1: i16                     [Integer][Integer]
  0020a: mov    rdi,QWORD PTR [r14-0x50]
  0020e: test   rdi,0x1
  00215: je     0x3ad7a
  0021b: add    rdi,0x2
  0021f: jo     0x3ad7a
  00225: mov    QWORD PTR [r14-0x50],rdi
:00017 br =>:00010
  00229: jmp    0x3ad89
:00018 loop_end
:00019 ret %6
  0022e: mov    rax,QWORD PTR [r14-0x60]
  00232: leave  
  00233: ret    
