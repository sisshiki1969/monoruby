==> start whole compile: bottom_up_tree FuncId(144) self_class:Object start:[00000] bytecode:0x55c45a135910
<== finished compile. elapsed:29.3µs
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
  00053: movabs rdi,0x7f38e0002f40
  0005d: movabs rax,0x55c459697920
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
  000ca: mov    rdi,0x1
  000d1: mov    rax,QWORD PTR [r14-0x40]
  000d5: mov    QWORD PTR [rsp-0x48],rax
  000da: mov    QWORD PTR [rsp-0x38],0x0
  000e3: mov    rdx,rdi
  000e6: movabs rax,0x1000400000090
  000f0: mov    QWORD PTR [rsp-0x30],rax
  000f5: movabs r13,0x55c45a135910
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
  0016b: movabs rax,0x55c459733970
  00175: call   rax
  00177: mov    QWORD PTR [r14-0x40],rax
:00014 ret %2
  0017b: mov    rax,QWORD PTR [r14-0x40]
  0017f: leave  
  00180: ret    
==> start whole compile: bottom_up_tree FuncId(144) self_class:Object start:[00000] bytecode:0x55c45a135910
<== finished compile. elapsed:34.3µs
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
  00053: movabs rdi,0x7f38e0002f40
  0005d: movabs rax,0x55c459697920
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
  000ca: mov    rdi,0x1
  000d1: mov    rax,QWORD PTR [r14-0x40]
  000d5: mov    QWORD PTR [rsp-0x48],rax
  000da: mov    QWORD PTR [rsp-0x38],0x0
  000e3: mov    rdx,rdi
  000e6: movabs rax,0x1000400000090
  000f0: mov    QWORD PTR [rsp-0x30],rax
  000f5: movabs r13,0x55c45a135910
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
  0016e: mov    rdi,0x1
  00175: mov    rax,QWORD PTR [r14-0x48]
  00179: mov    QWORD PTR [rsp-0x48],rax
  0017e: mov    QWORD PTR [rsp-0x38],0x0
  00187: mov    rdx,rdi
  0018a: movabs rax,0x1000400000090
  00194: mov    QWORD PTR [rsp-0x30],rax
  00199: movabs r13,0x55c45a135910
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
  001e5: movabs rax,0x55c459733970
  001ef: call   rax
  001f1: mov    QWORD PTR [r14-0x40],rax
:00014 ret %2
  001f5: mov    rax,QWORD PTR [r14-0x40]
  001f9: leave  
  001fa: ret    
==> start whole compile: item_check FuncId(143) self_class:Object start:[00000] bytecode:0x55c45a136260
<== finished compile. elapsed:25.6µs
offset:Pos(21971) code: 345 bytes  data: 4 bytes
  00000: push   rbp
  00001: mov    rbp,rsp
  00004: sub    rsp,0x60
  00008: mov    QWORD PTR [r14-0x48],0x4
  00010: mov    QWORD PTR [r14-0x50],0x4
  00018: mov    rdi,QWORD PTR [r14-0x30]
  0001c: test   rdi,0x7
  00023: jne    0x3aca1
  00029: cmp    DWORD PTR [rdi+0x4],0x1
  0002d: jne    0x3aca1
:00000 init_method reg:4 arg:2 req:2 opt:0 rest:false block:None stack_offset:6
:00001 %3 = %1.inline ObjectNil()           [Array]
  00033: cmp    DWORD PTR [rip+0xfffffffffffff82e],0xc        # 0xfffff868
  0003a: jne    0x3acb0
  00040: mov    rdi,QWORD PTR [r14-0x38]
  00044: mov    rax,0x14
  0004b: cmp    rdi,0x4
  0004f: jne    0x5c
  00055: mov    rax,0x1c
  0005c: mov    QWORD PTR [r14-0x48],rax
:00002 
:00003 condnotbr %3 =>:00007
  00060: mov    rax,QWORD PTR [r14-0x48]
  00064: or     rax,0x10
  00068: cmp    rax,0x14
  0006c: je     0x80
:00004 %3 = 1: i32
  00072: mov    QWORD PTR [r14-0x48],0x3
:00005 ret %3
  0007a: mov    rax,QWORD PTR [r14-0x48]
  0007e: leave  
  0007f: ret    
:00007 %3 = %1
  00080: mov    rax,QWORD PTR [r14-0x38]
  00084: mov    QWORD PTR [r14-0x48],rax
:00008 %3 = %0.call item_check(%3; 1)*      [Object]
  00088: mov    rdi,QWORD PTR [r14-0x30]
  0008c: cmp    DWORD PTR [rip+0xfffffffffffff7d5],0xc        # 0xfffff868
  00093: jne    0x3acbf
  00099: cmp    DWORD PTR [rip+0xfffffffffffff7d5],0x0        # 0xfffff875
  000a0: jne    0x3acce
  000a6: mov    QWORD PTR [rsp-0x28],0x0
  000af: mov    rax,QWORD PTR [r14-0x30]
  000b3: mov    QWORD PTR [rsp-0x40],rax
  000b8: mov    rdi,0x1
  000bf: lea    r8,[rsp-0x48]
  000c4: mov    rax,QWORD PTR [r14-0x48]
  000c8: call   0xffffab99
  000cd: mov    QWORD PTR [rsp-0x38],0x0
  000d6: mov    rdx,rdi
  000d9: movabs rax,0x100050000008f
  000e3: mov    QWORD PTR [rsp-0x30],rax
  000e8: movabs r13,0x55c45a136260
  000f2: mov    rsi,QWORD PTR [rbx]
  000f5: mov    QWORD PTR [rsp-0x18],rsi
  000fa: lea    rsi,[rsp-0x18]
  000ff: mov    QWORD PTR [rbx],rsi
  00102: lea    r14,[rsp-0x10]
  00107: mov    QWORD PTR [rsp-0x20],r14
  0010c: call   0xffffff6b
  00111: lea    r14,[rbp-0x8]
  00115: mov    QWORD PTR [rbx],r14
  00118: mov    r14,QWORD PTR [rbp-0x10]
  0011c: test   rax,rax
  0011f: je     0x3ace5
  00125: mov    QWORD PTR [r14-0x48],rax
:00009 
:00010 %3 = 1: i16 + %3                     [<INVALID>][<INVALID>]
  00129: cmp    DWORD PTR [rip+0x29],0x0        # 0x159
  00130: jl     0x3acf4
  00136: je     0x3ad03
  0013c: sub    DWORD PTR [rip+0x16],0x1        # 0x159
  00143: xor    rdi,rdi
  00146: jmp    0x3acf4
:00011 %4 = %2
  0014b: mov    rax,QWORD PTR [r14-0x40]
  0014f: mov    QWORD PTR [r14-0x50],rax
:00012 %4 = %0.call item_check(%4; 1)*      [<INVALID>]
:00013 
:00014 %3 = %3 + %4                         [<INVALID>][<INVALID>]
:00015 ret %3
  00153: mov    rax,QWORD PTR [r14-0x48]
  00157: leave  
  00158: ret    
==> start whole compile: item_check FuncId(143) self_class:Object start:[00000] bytecode:0x55c45a136260
<== finished compile. elapsed:24.6µs
offset:Pos(22320) code: 554 bytes  data: 0 bytes
  00000: push   rbp
  00001: mov    rbp,rsp
  00004: sub    rsp,0x60
  00008: mov    QWORD PTR [r14-0x48],0x4
  00010: mov    QWORD PTR [r14-0x50],0x4
  00018: mov    rdi,QWORD PTR [r14-0x30]
  0001c: test   rdi,0x7
  00023: jne    0x3abc2
  00029: cmp    DWORD PTR [rdi+0x4],0x1
  0002d: jne    0x3abc2
:00000 init_method reg:4 arg:2 req:2 opt:0 rest:false block:None stack_offset:6
:00001 %3 = %1.inline ObjectNil()           [Array]
  00033: cmp    DWORD PTR [rip+0xfffffffffffff6d1],0xc        # 0xfffff70b
  0003a: jne    0x3abd1
  00040: mov    rdi,QWORD PTR [r14-0x38]
  00044: mov    rax,0x14
  0004b: cmp    rdi,0x4
  0004f: jne    0x5c
  00055: mov    rax,0x1c
  0005c: mov    QWORD PTR [r14-0x48],rax
:00002 
:00003 condnotbr %3 =>:00007
  00060: mov    rax,QWORD PTR [r14-0x48]
  00064: or     rax,0x10
  00068: cmp    rax,0x14
  0006c: je     0x80
:00004 %3 = 1: i32
  00072: mov    QWORD PTR [r14-0x48],0x3
:00005 ret %3
  0007a: mov    rax,QWORD PTR [r14-0x48]
  0007e: leave  
  0007f: ret    
:00007 %3 = %1
  00080: mov    rax,QWORD PTR [r14-0x38]
  00084: mov    QWORD PTR [r14-0x48],rax
:00008 %3 = %0.call item_check(%3; 1)*      [Object]
  00088: mov    rdi,QWORD PTR [r14-0x30]
  0008c: cmp    DWORD PTR [rip+0xfffffffffffff678],0xc        # 0xfffff70b
  00093: jne    0x3abe0
  00099: cmp    DWORD PTR [rip+0xfffffffffffff678],0x0        # 0xfffff718
  000a0: jne    0x3abef
  000a6: mov    QWORD PTR [rsp-0x28],0x0
  000af: mov    rax,QWORD PTR [r14-0x30]
  000b3: mov    QWORD PTR [rsp-0x40],rax
  000b8: mov    rdi,0x1
  000bf: lea    r8,[rsp-0x48]
  000c4: mov    rax,QWORD PTR [r14-0x48]
  000c8: call   0xffffaa3c
  000cd: mov    QWORD PTR [rsp-0x38],0x0
  000d6: mov    rdx,rdi
  000d9: movabs rax,0x100050000008f
  000e3: mov    QWORD PTR [rsp-0x30],rax
  000e8: movabs r13,0x55c45a136260
  000f2: mov    rsi,QWORD PTR [rbx]
  000f5: mov    QWORD PTR [rsp-0x18],rsi
  000fa: lea    rsi,[rsp-0x18]
  000ff: mov    QWORD PTR [rbx],rsi
  00102: lea    r14,[rsp-0x10]
  00107: mov    QWORD PTR [rsp-0x20],r14
  0010c: call   0xfffffe0e
  00111: lea    r14,[rbp-0x8]
  00115: mov    QWORD PTR [rbx],r14
  00118: mov    r14,QWORD PTR [rbp-0x10]
  0011c: test   rax,rax
  0011f: je     0x3ac06
  00125: mov    QWORD PTR [r14-0x48],rax
:00009 
:00010 %3 = 1: i16 + %3                     [Integer][Integer]
  00129: mov    rsi,QWORD PTR [r14-0x48]
  0012d: test   rsi,0x1
  00134: je     0x3ac15
  0013a: add    rsi,0x2
  0013e: jo     0x3ac15
  00144: mov    QWORD PTR [r14-0x48],rsi
:00011 %4 = %2
  00148: mov    rax,QWORD PTR [r14-0x40]
  0014c: mov    QWORD PTR [r14-0x50],rax
:00012 %4 = %0.call item_check(%4; 1)*      [Object]
  00150: mov    rdi,QWORD PTR [r14-0x30]
  00154: cmp    DWORD PTR [rip+0xfffffffffffff5b0],0xc        # 0xfffff70b
  0015b: jne    0x3ac24
  00161: cmp    DWORD PTR [rip+0xfffffffffffff5b0],0x0        # 0xfffff718
  00168: jne    0x3ac33
  0016e: mov    QWORD PTR [rsp-0x28],0x0
  00177: mov    rax,QWORD PTR [r14-0x30]
  0017b: mov    QWORD PTR [rsp-0x40],rax
  00180: mov    rdi,0x1
  00187: lea    r8,[rsp-0x48]
  0018c: mov    rax,QWORD PTR [r14-0x50]
  00190: call   0xffffaa3c
  00195: mov    QWORD PTR [rsp-0x38],0x0
  0019e: mov    rdx,rdi
  001a1: movabs rax,0x100050000008f
  001ab: mov    QWORD PTR [rsp-0x30],rax
  001b0: movabs r13,0x55c45a136260
  001ba: mov    rsi,QWORD PTR [rbx]
  001bd: mov    QWORD PTR [rsp-0x18],rsi
  001c2: lea    rsi,[rsp-0x18]
  001c7: mov    QWORD PTR [rbx],rsi
  001ca: lea    r14,[rsp-0x10]
  001cf: mov    QWORD PTR [rsp-0x20],r14
  001d4: call   0xfffffe0e
  001d9: lea    r14,[rbp-0x8]
  001dd: mov    QWORD PTR [rbx],r14
  001e0: mov    r14,QWORD PTR [rbp-0x10]
  001e4: test   rax,rax
  001e7: je     0x3ac4a
  001ed: mov    QWORD PTR [r14-0x50],rax
:00013 
:00014 %3 = %3 + %4                         [Integer][Integer]
  001f1: mov    rdi,QWORD PTR [r14-0x48]
  001f5: mov    rsi,QWORD PTR [r14-0x50]
  001f9: test   rdi,0x1
  00200: je     0x3ac59
  00206: test   rsi,0x1
  0020d: je     0x3ac59
  00213: sub    rdi,0x1
  00217: add    rdi,rsi
  0021a: jo     0x3ac59
  00220: mov    QWORD PTR [r14-0x48],rdi
:00015 ret %3
  00224: mov    rax,QWORD PTR [r14-0x48]
  00228: leave  
  00229: ret    
==> start partial compile: <unnamed> FuncId(145) self_class:Object start:[00010] bytecode:0x55c45a137d50
<-- compile finished. end:[00022]
<== finished compile. elapsed:64µs
offset:Pos(22874) code: 468 bytes  data: 0 bytes
:00010 loop_start counter=5 jit-addr=0000000000000000
:00011 _%8 = %4 > %7                        [Integer][Integer]
  00000: mov    rdi,QWORD PTR [r14-0x50]
  00004: mov    rsi,QWORD PTR [r14-0x68]
  00008: test   rdi,0x1
  0000f: je     0x3aa57
  00015: test   rsi,0x1
  0001c: je     0x3aa57
  00022: cmp    rdi,rsi
  00025: jg     0x1cf
:00012 condbr _%8 =>:00022
:00013 %8 = %1
  0002b: mov    rax,QWORD PTR [r14-0x38]
  0002f: mov    QWORD PTR [r14-0x70],rax
:00014 %5 = %0.call bottom_up_tree(%8; 1)   [Object]
  00033: mov    rdi,QWORD PTR [r14-0x30]
  00037: cmp    DWORD PTR [rip+0xfffffffffffff4a3],0xc        # 0xfffff4e1
  0003e: jne    0x3aa66
  00044: cmp    DWORD PTR [rip+0xfffffffffffff4a3],0x0        # 0xfffff4ee
  0004b: jne    0x3aa75
  00051: mov    QWORD PTR [rsp-0x28],0x0
  0005a: mov    rax,QWORD PTR [r14-0x30]
  0005e: mov    QWORD PTR [rsp-0x40],rax
  00063: mov    rdi,0x1
  0006a: mov    rax,QWORD PTR [r14-0x70]
  0006e: mov    QWORD PTR [rsp-0x48],rax
  00073: mov    QWORD PTR [rsp-0x38],0x0
  0007c: mov    rdx,rdi
  0007f: movabs rax,0x1000400000090
  00089: mov    QWORD PTR [rsp-0x30],rax
  0008e: movabs r13,0x55c45a135910
  00098: mov    rsi,QWORD PTR [rbx]
  0009b: mov    QWORD PTR [rsp-0x18],rsi
  000a0: lea    rsi,[rsp-0x18]
  000a5: mov    QWORD PTR [rbx],rsi
  000a8: lea    r14,[rsp-0x10]
  000ad: mov    QWORD PTR [rsp-0x20],r14
  000b2: call   0xfffff778
  000b7: lea    r14,[rbp-0x8]
  000bb: mov    QWORD PTR [rbx],r14
  000be: mov    r14,QWORD PTR [rbp-0x10]
  000c2: test   rax,rax
  000c5: je     0x3aa8c
  000cb: mov    QWORD PTR [r14-0x58],rax
:00015 
:00016 %8 = %5
  000cf: mov    rax,QWORD PTR [r14-0x58]
  000d3: mov    QWORD PTR [r14-0x70],rax
:00017 %8 = %0.call item_check(%8; 1)*      [Object]
  000d7: mov    rdi,QWORD PTR [r14-0x30]
  000db: cmp    DWORD PTR [rip+0xfffffffffffff3ff],0xc        # 0xfffff4e1
  000e2: jne    0x3aa9b
  000e8: cmp    DWORD PTR [rip+0xfffffffffffff3ff],0x0        # 0xfffff4ee
  000ef: jne    0x3aaaa
  000f5: mov    QWORD PTR [rsp-0x28],0x0
  000fe: mov    rax,QWORD PTR [r14-0x30]
  00102: mov    QWORD PTR [rsp-0x40],rax
  00107: mov    rdi,0x1
  0010e: lea    r8,[rsp-0x48]
  00113: mov    rax,QWORD PTR [r14-0x70]
  00117: call   0xffffa812
  0011c: mov    QWORD PTR [rsp-0x38],0x0
  00125: mov    rdx,rdi
  00128: movabs rax,0x100050000008f
  00132: mov    QWORD PTR [rsp-0x30],rax
  00137: movabs r13,0x55c45a136260
  00141: mov    rsi,QWORD PTR [rbx]
  00144: mov    QWORD PTR [rsp-0x18],rsi
  00149: lea    rsi,[rsp-0x18]
  0014e: mov    QWORD PTR [rbx],rsi
  00151: lea    r14,[rsp-0x10]
  00156: mov    QWORD PTR [rsp-0x20],r14
  0015b: call   0xfffffbe4
  00160: lea    r14,[rbp-0x8]
  00164: mov    QWORD PTR [rbx],r14
  00167: mov    r14,QWORD PTR [rbp-0x10]
  0016b: test   rax,rax
  0016e: je     0x3aac1
  00174: mov    QWORD PTR [r14-0x70],rax
:00018 
:00019 %3 = %3 + %8                         [Integer][Integer]
  00178: mov    rdi,QWORD PTR [r14-0x48]
  0017c: mov    rsi,QWORD PTR [r14-0x70]
  00180: test   rdi,0x1
  00187: je     0x3aad0
  0018d: test   rsi,0x1
  00194: je     0x3aad0
  0019a: sub    rdi,0x1
  0019e: add    rdi,rsi
  001a1: jo     0x3aad0
  001a7: mov    QWORD PTR [r14-0x48],rdi
:00020 %4 = %4 + 1: i16                     [Integer][Integer]
  001ab: mov    rdi,QWORD PTR [r14-0x50]
  001af: test   rdi,0x1
  001b6: je     0x3aadf
  001bc: add    rdi,0x2
  001c0: jo     0x3aadf
  001c6: mov    QWORD PTR [r14-0x50],rdi
:00021 br =>:00010
  001ca: jmp    0x3aafd
:00022 loop_end
  001cf: jmp    0x3aaee
==> start whole compile: <unnamed> FuncId(145) self_class:Object start:[00000] bytecode:0x55c45a137d50
<== finished compile. elapsed:78.3µs
offset:Pos(23342) code: 784 bytes  data: 0 bytes
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
  00039: jne    0x3a942
  0003f: cmp    DWORD PTR [rdi+0x4],0x1
  00043: jne    0x3a942
:00000 init_block reg:8 arg:1 req:1 opt:0 rest:false block:None stack_offset:8
:00001 %6 = dynvar(1, %1)
  00049: mov    rax,QWORD PTR [r14-0x18]
  0004d: mov    rax,QWORD PTR [rax-0x20]
  00051: mov    QWORD PTR [r14-0x60],rax
:00002 %6 = %6 - %1                         [Integer][Integer]
  00055: mov    rdi,QWORD PTR [r14-0x60]
  00059: mov    rsi,QWORD PTR [r14-0x38]
  0005d: test   rdi,0x1
  00064: je     0x3a951
  0006a: test   rsi,0x1
  00071: je     0x3a951
  00077: sub    rdi,rsi
  0007a: jo     0x3a951
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
  000a3: je     0x3a960
  000a9: test   rsi,0x1
  000b0: je     0x3a960
  000b6: sub    rdi,0x1
  000ba: add    rdi,rsi
  000bd: jo     0x3a960
  000c3: mov    QWORD PTR [r14-0x60],rdi
:00005 %2 = 2: i16 ** %6                    [Integer][Integer]
  000c7: mov    rsi,QWORD PTR [r14-0x60]
  000cb: test   rsi,0x1
  000d2: je     0x3a96f
  000d8: mov    rdi,0x5
  000df: sar    rdi,1
  000e2: sar    rsi,1
  000e5: movabs rax,0x55c45972c350
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
  0011d: movabs rax,0x55c459733ca0
  00127: call   rax
  00129: test   rax,rax
  0012c: je     0x3a97e
  00132: mov    QWORD PTR [r14-0x60],rax
  00136: jmp    0x3a98d
:00010 loop_start counter=5 jit-addr=00007f38dff0595a
:00011 _%8 = %4 > %7                        [Integer][Integer]
  0013b: mov    rdi,QWORD PTR [r14-0x50]
  0013f: mov    rsi,QWORD PTR [r14-0x68]
  00143: test   rdi,0x1
  0014a: je     0x3a9a6
  00150: test   rsi,0x1
  00157: je     0x3a9a6
  0015d: cmp    rdi,rsi
  00160: jg     0x30a
:00012 condbr _%8 =>:00022
:00013 %8 = %1
  00166: mov    rax,QWORD PTR [r14-0x38]
  0016a: mov    QWORD PTR [r14-0x70],rax
:00014 %5 = %0.call bottom_up_tree(%8; 1)   [Object]
  0016e: mov    rdi,QWORD PTR [r14-0x30]
  00172: cmp    DWORD PTR [rip+0xfffffffffffff194],0xc        # 0xfffff30d
  00179: jne    0x3a9b5
  0017f: cmp    DWORD PTR [rip+0xfffffffffffff194],0x0        # 0xfffff31a
  00186: jne    0x3a9c4
  0018c: mov    QWORD PTR [rsp-0x28],0x0
  00195: mov    rax,QWORD PTR [r14-0x30]
  00199: mov    QWORD PTR [rsp-0x40],rax
  0019e: mov    rdi,0x1
  001a5: mov    rax,QWORD PTR [r14-0x70]
  001a9: mov    QWORD PTR [rsp-0x48],rax
  001ae: mov    QWORD PTR [rsp-0x38],0x0
  001b7: mov    rdx,rdi
  001ba: movabs rax,0x1000400000090
  001c4: mov    QWORD PTR [rsp-0x30],rax
  001c9: movabs r13,0x55c45a135910
  001d3: mov    rsi,QWORD PTR [rbx]
  001d6: mov    QWORD PTR [rsp-0x18],rsi
  001db: lea    rsi,[rsp-0x18]
  001e0: mov    QWORD PTR [rbx],rsi
  001e3: lea    r14,[rsp-0x10]
  001e8: mov    QWORD PTR [rsp-0x20],r14
  001ed: call   0xfffff5a4
  001f2: lea    r14,[rbp-0x8]
  001f6: mov    QWORD PTR [rbx],r14
  001f9: mov    r14,QWORD PTR [rbp-0x10]
  001fd: test   rax,rax
  00200: je     0x3a9db
  00206: mov    QWORD PTR [r14-0x58],rax
:00015 
:00016 %8 = %5
  0020a: mov    rax,QWORD PTR [r14-0x58]
  0020e: mov    QWORD PTR [r14-0x70],rax
:00017 %8 = %0.call item_check(%8; 1)*      [Object]
  00212: mov    rdi,QWORD PTR [r14-0x30]
  00216: cmp    DWORD PTR [rip+0xfffffffffffff0f0],0xc        # 0xfffff30d
  0021d: jne    0x3a9ea
  00223: cmp    DWORD PTR [rip+0xfffffffffffff0f0],0x0        # 0xfffff31a
  0022a: jne    0x3a9f9
  00230: mov    QWORD PTR [rsp-0x28],0x0
  00239: mov    rax,QWORD PTR [r14-0x30]
  0023d: mov    QWORD PTR [rsp-0x40],rax
  00242: mov    rdi,0x1
  00249: lea    r8,[rsp-0x48]
  0024e: mov    rax,QWORD PTR [r14-0x70]
  00252: call   0xffffa63e
  00257: mov    QWORD PTR [rsp-0x38],0x0
  00260: mov    rdx,rdi
  00263: movabs rax,0x100050000008f
  0026d: mov    QWORD PTR [rsp-0x30],rax
  00272: movabs r13,0x55c45a136260
  0027c: mov    rsi,QWORD PTR [rbx]
  0027f: mov    QWORD PTR [rsp-0x18],rsi
  00284: lea    rsi,[rsp-0x18]
  00289: mov    QWORD PTR [rbx],rsi
  0028c: lea    r14,[rsp-0x10]
  00291: mov    QWORD PTR [rsp-0x20],r14
  00296: call   0xfffffa10
  0029b: lea    r14,[rbp-0x8]
  0029f: mov    QWORD PTR [rbx],r14
  002a2: mov    r14,QWORD PTR [rbp-0x10]
  002a6: test   rax,rax
  002a9: je     0x3aa10
  002af: mov    QWORD PTR [r14-0x70],rax
:00018 
:00019 %3 = %3 + %8                         [Integer][Integer]
  002b3: mov    rdi,QWORD PTR [r14-0x48]
  002b7: mov    rsi,QWORD PTR [r14-0x70]
  002bb: test   rdi,0x1
  002c2: je     0x3aa1f
  002c8: test   rsi,0x1
  002cf: je     0x3aa1f
  002d5: sub    rdi,0x1
  002d9: add    rdi,rsi
  002dc: jo     0x3aa1f
  002e2: mov    QWORD PTR [r14-0x48],rdi
:00020 %4 = %4 + 1: i16                     [Integer][Integer]
  002e6: mov    rdi,QWORD PTR [r14-0x50]
  002ea: test   rdi,0x1
  002f1: je     0x3aa2e
  002f7: add    rdi,0x2
  002fb: jo     0x3aa2e
  00301: mov    QWORD PTR [r14-0x50],rdi
:00021 br =>:00010
  00305: jmp    0x3aa3d
:00022 loop_end
:00023 ret %6
  0030a: mov    rax,QWORD PTR [r14-0x60]
  0030e: leave  
  0030f: ret    
