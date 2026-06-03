use std::hash::Hash;

use monoasm::*;
// Provides the `monoasm!` macro to the JIT subtree (`compiler`, `jitgen`,
// `patch`) via descendant visibility. The arch backends under `arch/` import
// it directly instead.
#[cfg(jit_x86)]
use monoasm_macro::monoasm;
#[cfg(jit)]
use std::time::Duration;

#[cfg(jit)]
mod compiler;
mod jit_module;
#[cfg(jit)]
pub mod jitgen;
#[cfg(jit)]
mod patch;
pub mod runtime;
pub(crate) mod signal_table;

// Architecture-specific VM-tier backend. The asm-level VM construction,
// frame setup, invokers, native-function wrappers, bytecode dispatch, and
// the small asm helpers have separate x86-64 and aarch64 implementations,
// selected here by `target_arch`. Both live under `arch/` with a mirrored
// file layout (`codegen.rs`, `jit_module.rs`, `invoker.rs`, `wrapper.rs`,
// `vmgen.rs`); only the inherent `impl Codegen` / `impl JitModule` methods
// they provide differ per arch — the types themselves stay arch-neutral.
mod arch;

#[cfg(jit)]
use self::jitgen::asmir::AsmEvict;

use super::*;
#[cfg(jit)]
use crate::bytecodegen::inst::*;
#[cfg(jit)]
use crate::codegen::jitgen::SpecializedCodeInfo;
use crate::executor::*;

const OPECODE: i64 = 6;

pub(crate) type MethodInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    FuncId,
    Value,
    *const Value,
    usize,
    Option<BlockHandler>,
    Option<Hashmap>,
) -> Option<Value>;

pub(crate) type MethodInvoker2 =
    extern "C" fn(&mut Executor, &mut Globals, FuncId, Value, Arg, usize) -> Option<Value>;

pub(crate) type BlockInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcData,
    Value,
    *const Value,
    usize,
    Option<Hashmap>,
) -> Option<Value>;

pub(crate) type BindingInvoker = extern "C" fn(&mut Executor, &mut Globals, Lfp) -> Option<Value>;

pub(crate) type FiberInvoker = extern "C" fn(
    &mut Executor,
    &mut Globals,
    &ProcData,
    Value,
    *const Value,
    usize,
    &mut Executor,
) -> Option<Value>;

thread_local! {
    pub static CODEGEN: std::cell::RefCell<Codegen> = std::cell::RefCell::new(
    {
        Codegen::new()
    });
}

#[cfg(feature = "perf")]
thread_local! {
    pub static PERF_FILE: std::cell::RefCell<std::fs::File> = std::cell::RefCell::new(
    {
        let pid = std::process::id();
        let temp_file = format!("/tmp/perf-{pid}.map");
        let file = match std::fs::File::create(&temp_file) {
            Err(why) => panic!("couldn't create {}: {}", temp_file, why),
            Ok(file) => file,
        };
        file
    });
}

#[cfg(not(test))]
const COUNT_START_COMPILE: i32 = 20;
#[cfg(test)]
const COUNT_START_COMPILE: i32 = 5;
#[cfg(not(test))]
const COUNT_LOOP_START_COMPILE: i32 = 100;
#[cfg(test)]
const COUNT_LOOP_START_COMPILE: i32 = 15;
const COUNT_RECOMPILE_ARECV_CLASS: i32 = 5;
const COUNT_DEOPT_RECOMPILE: i32 = 10;
const COUNT_DEOPT_RECOMPILE_SPECIALIZED: i32 = 50;

///
/// General purpose registers.
///
#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum GP {
    Rax = 0,
    Rcx = 1,
    Rdx = 2,
    Rsp = 4,
    Rsi = 6,
    Rdi = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

/// aarch64 mapping of the (x86-named) abstract JIT register enum to physical
/// A64 registers, used by the aarch64 AsmIR→machine-code lowering (Phase 3b).
///
/// The **global** roles must match the aarch64 VM's global registers so that
/// JIT↔VM transitions agree: `R12`(Globals)→x20, `R13`(PC)→x21, `R14`(LFP)→x22,
/// `R15`(accumulator)→x23 (and the executor `rbx`→x19 is implicit, not in this
/// enum). The **scratch** set maps to distinct caller-saved `x0..=x8`; `x9..=x15`
/// are left free for per-`AsmInst` lowering temps. `Rsp`→`sp`.
///
/// Note: the x86 and aarch64 C-call ABIs differ (x86 passes args in
/// rdi/rsi/rdx/rcx/r8/r9 and returns in rax; aarch64 uses x0..x7 / x0), so
/// call-argument lowering shuffles into x0..x7 explicitly rather than relying
/// on this 1:1 map.
#[cfg(target_arch = "aarch64")]
impl GP {
    pub(in crate::codegen) fn a64(self) -> monoasm::GReg {
        let n: u32 = match self {
            GP::Rax => 0,
            GP::Rcx => 1,
            GP::Rdx => 2,
            GP::Rsi => 3,
            GP::Rdi => 4,
            GP::R8 => 5,
            GP::R9 => 6,
            GP::R10 => 7,
            GP::R11 => 8,
            GP::R12 => 20,
            GP::R13 => 21,
            GP::R14 => 22,
            GP::R15 => 23,
            GP::Rsp => 31,
        };
        monoasm::GReg(n)
    }
}

/// `xmm{id+2}`; ids >= `PHYS_XMM_POOL` are spilled to a stack slot.
///
/// The `stress-spill-pool` cargo feature shrinks the pool to 2 so
/// that nearly every F-mode allocation overflows into a spill slot
/// — this exercises the LoadSpill / StoreSpill paths and the
/// spill-aware AsmInst lowerings throughout the entire test suite.
#[cfg(not(feature = "stress-spill-pool"))]
const PHYS_XMM_POOL: usize = 14;
#[cfg(feature = "stress-spill-pool")]
const PHYS_XMM_POOL: usize = 2;

///
/// Virtual floating-point register. The front-end (TraceIr → AsmIr)
/// allocates these and stores them in `LinkMode::F` / `LinkMode::Sf`
/// and inside `AsmInst` operands.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FPReg(pub(crate) usize);

impl FPReg {
    fn new(id: usize) -> Self {
        Self(id)
    }

    fn loc(self, base_stack_offset: usize) -> FPRegLoc {
        let pool = PHYS_XMM_POOL;
        if self.0 < pool {
            FPRegLoc::Xmm(self.0 as u64 + 2)
        } else {
            let n = self.0 - pool;
            let ofs = (base_stack_offset as i32) - 24 + 8 * (n as i32);
            FPRegLoc::Spill(ofs)
        }
    }
}

///
/// Location where a `VirtFPReg`'s value lives at code-generation
/// time. `Phys(N)` means physical `xmm{N}` directly usable in
/// `monoasm! { ... xmm(N) ... }`; `Spill(off)` means the 8-byte slot
/// at `[rbp - off]`. Spill-aware codegen lowerings build per-operand
/// asm based on this — e.g. `addsd xmm, mem` when the rhs operand is
/// spilled, instead of forcing a scratch load.
///
#[derive(Debug, Clone, Copy)]
pub(super) enum FPRegLoc {
    Xmm(u64),
    Spill(i32),
}

pub struct JitModule {
    pub(crate) jit: JitMemory,
    class_version: DestLabel,
    const_version: DestLabel,
    alloc_flag: DestLabel,
    /// 32-bit bitmap of pending Unix signals — bit `n` corresponds to
    /// signal `n + 1` (so SIGINT = 2 ⇒ bit 1). Written async-safely from
    /// the per-signal asm stubs in `signal_handler_for`; drained at
    /// poll-time by `take_pending_signals`. See doc/signal_handling.md.
    pending_signals: DestLabel,
    ///
    /// Raise error.
    ///
    /// ### in
    /// - r13: PC + 1
    /// - r14: LFP
    ///
    /// ### destroy
    /// - caller saved registers
    ///
    entry_raise: DestLabel,
    ///
    /// Execute GC.
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax
    /// - stack
    ///
    exec_gc: DestLabel,
    ///
    /// Convert f64 to Value.
    ///
    /// ### in
    /// - xmm0: f64
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    f64_to_val: DestLabel,
    ///
    /// Dump stack trace and go panic.
    ///
    /// #### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    entry_panic: DestLabel,
    ///
    /// Raise StackOverFlow error.
    ///
    /// #### in
    /// - rbx: &mut Executor
    ///
    vm_stack_overflow: DestLabel,
    dispatch: Box<[CodePtr; 256]>,
    bop_redefined_flags: DestLabel,
}

impl std::ops::Deref for JitModule {
    type Target = JitMemory;
    fn deref(&self) -> &Self::Target {
        &self.jit
    }
}

impl std::ops::DerefMut for JitModule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.jit
    }
}

impl JitModule {
    pub(crate) fn get_address_pair(&mut self) -> (CodePtr, CodePtr) {
        assert_eq!(0, self.jit.get_page());
        let ptr0 = self.jit.get_current_address();
        self.jit.select_page(1);
        let ptr1 = self.jit.get_current_address();
        self.jit.select_page(0);
        (ptr0, ptr1)
    }

    pub(crate) fn get_wrapper_info(
        &mut self,
        pair: (CodePtr, CodePtr),
    ) -> (CodePtr, usize, CodePtr, usize) {
        let (ptr0, ptr1) = pair;
        assert_eq!(0, self.jit.get_page());
        let size0 = self.jit.get_current_address() - ptr0;
        self.jit.select_page(1);
        let size1 = self.jit.get_current_address() - ptr1;
        self.jit.select_page(0);
        (ptr0, size0 as usize, ptr1, size1 as usize)
    }

    pub fn set_class_version(&mut self, version: u32, label: &DestLabel) {
        let p = self.jit.get_label_address(label).as_ptr() as *mut u32;
        unsafe { *p = version };
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_write(info: (CodePtr, usize, CodePtr, usize), desc: &str) {
        use std::io::Write;
        PERF_FILE.with(|file| {
            let mut f = file.borrow_mut();
            f.write_all(format!("{:x} {:x} {desc}\n", info.0.as_ptr() as usize, info.1).as_bytes())
                .unwrap();
            f.write_all(format!("{:x} {:x} {desc}\n", info.2.as_ptr() as usize, info.3).as_bytes())
                .unwrap();
        });
    }

    #[cfg(feature = "perf")]
    pub(crate) fn perf_info(&mut self, pair: (CodePtr, CodePtr), func_name: &str) {
        let info = self.get_wrapper_info(pair);
        Self::perf_write(info, func_name);
    }
}

impl JitModule {
    ///
    /// Execute GC. (for JIT code)
    ///
    /// ### in
    /// - rbx: &mut Executor
    /// - r12: &mut Globals
    ///
    /// ### out
    /// - rax: None if Interrupt is thrown.
    ///
    /// ### destroy
    /// - rax, rcx
    /// - stack
    ///
    #[cfg(jit_x86)]
    fn jit_execute_gc(&mut self, wb: &jitgen::WriteBack, error: &DestLabel, base: usize) {
        self.execute_gc_inner(error, |s| s.gen_write_back(wb, base));
    }
}

#[cfg(jit)]
#[derive(Clone, Copy, PartialEq, Debug)]
struct CompilationUnitId(usize);

#[cfg(jit)]
#[allow(dead_code)]
struct CompilationUnitInfo {
    /// `ISeqId``.
    iseq_id: ISeqId,
    /// `ClassId`` of *self*.
    self_class: ClassId,
    /// Bytecode position. (`Some`` for loop compilation, `None`` for method compilation)
    position: Option<BytecodePtr>,
    /// Entry point of the machine code.
    codeptr: CodePtr,
    /// Specialized code info in the compilation unit.
    specialized_info: SpecializedCodeInfo,
    /// span of the generated code ((inline_code_start, inline_code_len), (outline_code_start, outline_code_len)).
    span: ((CodePtr, usize), (CodePtr, usize)),
    /// compilation time.
    elapsed: Duration,
}

///
/// Machine code generator
///
pub struct Codegen {
    pub(crate) jit: JitModule,
    /// Pre-generated async-signal-safe asm stub for every signal in
    /// `signal_table::TRAPPABLE_SIGNALS`, keyed by signo. Generated once
    /// at `Codegen::new`; `Signal.trap` looks up the stub here and
    /// `sigaction(2)`s it at trap time. See doc/signal_handling.md A7.
    signal_stubs: HashMap<i32, CodePtr>,
    class_version_addr: *mut u32,
    const_version_addr: *mut u64,

    #[cfg(jit)]
    compilation_unit: Vec<CompilationUnitInfo>,

    /// return_addr => (patch_point, deopt)
    #[cfg(jit)]
    return_addr_table: HashMap<CodePtr, (Option<CodePtr>, DestLabel)>,
    #[cfg(jit)]
    asm_return_addr_table: HashMap<AsmEvict, CodePtr>,
    #[cfg(jit)]
    pub(crate) specialized_info: Vec<(ISeqId, ClassId, DestLabel)>,
    #[cfg(jit)]
    pub(crate) specialized_base: usize,
    vm_code_position: (Option<CodePtr>, usize, Option<CodePtr>, usize),
    vm_entry: DestLabel,
    vm_fetch: DestLabel,
    jit_class_guard_fail: DestLabel,

    div_by_zero: DestLabel,
    ///
    /// Get class id.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - rax: ClassId
    ///
    get_class: DestLabel,

    ///
    /// Initialize stack bottom.
    ///
    pub(crate) init_stack_limit: extern "C" fn(&mut Executor) -> *const u8,
    pub(crate) method_invoker: MethodInvoker,
    pub(crate) method_invoker2: MethodInvoker2,
    pub(crate) block_invoker: BlockInvoker,
    pub(crate) block_invoker_with_self: BlockInvoker,
    pub(crate) binding_invoker: BindingInvoker,
    ///
    /// Fiber invoker.
    ///
    /// ### in
    /// - `rdi`: &mut Executor
    /// - `rsi`: &mut Globals
    /// - `rdx`: &ProcInner
    /// - `rcx`: (dummy)
    /// - `r8`:  *args: *const Value
    /// - `r9`:  len: usize
    /// - `[rsp + 8]`: *mut Executor of child Fiber.
    ///
    pub(crate) fiber_invoker: FiberInvoker,
    pub(crate) fiber_invoker_with_self: FiberInvoker,
    pub(crate) resume_fiber: extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value>,
    pub(crate) yield_fiber: extern "C" fn(*mut Executor, Value) -> Option<Value>,
    pub(crate) startup_flag: bool,
    #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
    pub(crate) jit_compile_time: std::time::Duration,
}

impl std::ops::Deref for Codegen {
    type Target = JitModule;
    fn deref(&self) -> &Self::Target {
        &self.jit
    }
}

impl std::ops::DerefMut for Codegen {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.jit
    }
}

impl Codegen {
    pub fn new() -> Self {
        let mut jit = JitModule::new();
        let pair = jit.get_address_pair();

        let class_version_addr = jit.get_label_address(&jit.class_version).as_ptr() as *mut u32;
        let const_version_addr = jit.get_label_address(&jit.const_version).as_ptr() as *mut u64;
        let entry_panic = jit.entry_panic.clone();
        let get_class = jit.get_class();
        let init_stack_limit = jit.init_stack_limit();
        let method_invoker = jit.method_invoker();
        let method_invoker2 = jit.method_invoker2();
        let block_invoker = jit.block_invoker();
        let block_invoker_with_self = jit.block_invoker_with_self();
        let binding_invoker = jit.binding_invoker();
        let fiber_invoker = jit.fiber_invoker();
        let fiber_invoker_with_self = jit.fiber_invoker_with_self();
        let resume_fiber = jit.resume_fiber();
        let yield_fiber = jit.yield_fiber();

        let mut codegen = Self {
            jit,
            signal_stubs: HashMap::default(),
            class_version_addr,
            const_version_addr,
            #[cfg(jit)]
            compilation_unit: Vec::new(),
            #[cfg(jit)]
            return_addr_table: HashMap::default(),
            #[cfg(jit)]
            asm_return_addr_table: HashMap::default(),
            #[cfg(jit)]
            specialized_info: Vec::new(),
            #[cfg(jit)]
            specialized_base: 0,
            vm_entry: entry_panic.clone(),
            vm_code_position: (None, 0, None, 0),
            vm_fetch: entry_panic.clone(),
            jit_class_guard_fail: entry_panic.clone(),
            div_by_zero: entry_panic.clone(),
            get_class,
            init_stack_limit,
            method_invoker,
            method_invoker2,
            block_invoker,
            block_invoker_with_self,
            binding_invoker,
            fiber_invoker,
            fiber_invoker_with_self,
            resume_fiber,
            yield_fiber,
            startup_flag: false,
            #[cfg(any(feature = "jit-log", feature = "jit-debug"))]
            jit_compile_time: std::time::Duration::default(),
        };
        codegen.construct_vm();
        // Pre-generate an async-signal-safe stub for every signal we
        // permit trapping (signal_table::TRAPPABLE_SIGNALS). Doing it all
        // up front means `Signal.trap` only has to sigaction(2) at trap
        // time — no JIT codegen on a live buffer. SIGSEGV/SIGBUS/SIGFPE/
        // SIGILL/SIGABRT are deliberately absent: those are genuine
        // programming errors and are left to the kernel's core-dump path.
        // See doc/signal_handling.md A2/A7.
        for &signo in signal_table::TRAPPABLE_SIGNALS {
            let codeptr = codegen.signal_handler_for(signo);
            codegen.signal_stubs.insert(signo, codeptr);
        }
        codegen.jit.finalize();

        // Only the default-install set (POSIX_SIGNALS, i.e. SIGINT) is
        // armed now. The rest stay at their OS default until a user
        // `Signal.trap` wires the pre-generated stub in. See A3.
        for &signo in signal_table::POSIX_SIGNALS {
            if !codegen.install_signal_stub(signo) {
                panic!("Failed to set signal handler for signo {signo}");
            }
        }

        #[cfg(feature = "perf")]
        codegen.perf_info(pair, "monoruby-vm");

        let info = codegen.get_wrapper_info(pair);
        codegen.vm_code_position = (Some(info.0), info.1, Some(info.2), info.3);

        let address = codegen.jit.get_label_address(&codegen.alloc_flag).as_ptr() as *mut u32;
        alloc::ALLOC.with(|alloc| {
            alloc.borrow_mut().set_alloc_flag_address(address);
        });

        codegen
    }

    pub(crate) fn class_version_label(&self) -> DestLabel {
        self.class_version.clone()
    }
    pub(crate) fn const_version_label(&self) -> DestLabel {
        self.const_version.clone()
    }

    #[cfg(jit)]
    fn add_compilation_unit(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        position: Option<BytecodePtr>,
        codeptr: CodePtr,
        specialized_info: SpecializedCodeInfo,
        span: ((CodePtr, usize), (CodePtr, usize)),
        elapsed: Duration,
    ) -> CompilationUnitId {
        let id = self.compilation_unit.len();
        self.compilation_unit.push(CompilationUnitInfo {
            iseq_id,
            self_class,
            position,
            codeptr,
            specialized_info,
            span,
            elapsed,
        });
        CompilationUnitId(id)
    }

    /// Atomically read and clear the pending-signal bitmap. Returns the
    /// signals that were pending at call time; the bitmap is left zero.
    ///
    /// Uses `xchg` semantics via a volatile read-then-zero. The asm
    /// signal stub uses `or` to set bits, so a concurrent set racing
    /// with this clear may be lost if the read happens between OR and
    /// the bit's observation — acceptable: it gets picked up on the
    /// next poll. Use of `*mut u32` is fine here because the signal
    /// handler runs on the same thread as the poll.
    pub(crate) fn take_pending_signals(&self) -> u32 {
        let ptr = self.jit.get_label_address(&self.pending_signals).as_ptr() as *mut u32;
        unsafe {
            let v = std::ptr::read_volatile(ptr);
            if v != 0 {
                std::ptr::write_volatile(ptr, 0);
            }
            v
        }
    }

    pub(crate) fn signal_handler_for(&mut self, signo: i32) -> CodePtr {
        self.jit
            .signal_handler_for(self.alloc_flag.clone(), self.pending_signals.clone(), signo)
    }

    /// `sigaction(2)` `signo` to `handler` with `flags`. Returns true on
    /// success. The handler must be either one of the libc `SIG_*`
    /// sentinels or a pointer to a stub from `signal_stubs`.
    unsafe fn sigaction_to(signo: i32, handler: libc::sighandler_t, flags: i32) -> bool {
        // SAFETY: caller passes a valid signo and a handler that is
        // either a libc sentinel or a stable code pointer into the JIT
        // buffer (which lives for the process lifetime).
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler;
            sa.sa_flags = flags;
            libc::sigemptyset(&mut sa.sa_mask);
            libc::sigaction(signo, &sa, std::ptr::null_mut()) == 0
        }
    }

    /// Arm `signo` with its pre-generated async-signal-safe stub so the
    /// next poll point observes the pending bit. Returns false if no
    /// stub was generated for `signo` (i.e. it is not trappable) or the
    /// syscall failed.
    pub(crate) fn install_signal_stub(&self, signo: i32) -> bool {
        let Some(codeptr) = self.signal_stubs.get(&signo) else {
            return false;
        };
        // SAFETY: codeptr is a stable pointer into the finalized JIT
        // buffer; SA_RESTART matches the startup install.
        unsafe {
            Self::sigaction_to(
                signo,
                codeptr.as_ptr() as libc::sighandler_t,
                libc::SA_RESTART,
            )
        }
    }

    /// Set `signo` to `SIG_IGN` (silently discard).
    pub(crate) fn install_signal_ignore(&self, signo: i32) -> bool {
        // SAFETY: SIG_IGN is a valid disposition for any catchable signal.
        unsafe { Self::sigaction_to(signo, libc::SIG_IGN, 0) }
    }

    /// Restore `signo` to monoruby's default disposition: re-arm the stub
    /// for default-installed signals (so SIGINT keeps raising `Interrupt`),
    /// otherwise revert to the kernel `SIG_DFL`.
    pub(crate) fn install_signal_default(&self, signo: i32) -> bool {
        if signal_table::is_default_installed(signo) {
            self.install_signal_stub(signo)
        } else {
            // SAFETY: SIG_DFL is always a valid disposition.
            unsafe { Self::sigaction_to(signo, libc::SIG_DFL, 0) }
        }
    }

    /// Set `signo` to the OS default disposition (`SIG_DFL`)
    /// unconditionally — the `"SYSTEM_DEFAULT"` trap command. Unlike
    /// `install_signal_default`, this does *not* re-arm monoruby's stub
    /// for the default-installed set: the caller explicitly wants the
    /// kernel's behaviour.
    pub(crate) fn install_signal_system_default(&self, signo: i32) -> bool {
        // SAFETY: SIG_DFL is always a valid disposition.
        unsafe { Self::sigaction_to(signo, libc::SIG_DFL, 0) }
    }

    pub(crate) fn entry_raise(&self) -> DestLabel {
        self.entry_raise.clone()
    }

    pub(crate) fn vm_entry(&self) -> DestLabel {
        self.vm_entry.clone()
    }

    pub(crate) fn vm_fetch(&self) -> DestLabel {
        self.vm_fetch.clone()
    }

    pub(crate) fn class_version(&self) -> u32 {
        unsafe { *self.class_version_addr }
    }

    pub(crate) fn class_version_inc(&self) {
        unsafe { *self.class_version_addr += 1 }
    }

    pub(crate) fn const_version(&self) -> u64 {
        unsafe { *self.const_version_addr }
    }

    pub(crate) fn const_version_inc(&self) {
        unsafe { *self.const_version_addr += 1 }
    }

    pub(crate) fn set_bop_redefine(&mut self) {
        let addr = self
            .jit
            .get_label_address(&self.bop_redefined_flags)
            .as_ptr() as *mut u32;
        unsafe { *addr = !0 }
        self.remove_vm_bop_optimization();
        #[cfg(any(test, feature = "jit-log"))]
        eprintln!("### basic op redefined.");
    }

    ///
    /// Check whether *addr* is in VM code or invokers.
    ///
    pub(crate) fn check_vm_address(&self, addr: CodePtr) -> bool {
        let (start1, size1, start2, size2) = self.vm_code_position;
        let start1 = start1.unwrap();
        let start2 = start2.unwrap();
        (start1..start1 + size1).contains(&addr) || (start2..start2 + size2).contains(&addr)
    }

    #[cfg(feature = "emit-asm")]
    pub(crate) fn dump_disas(
        &mut self,
        store: &Store,
        sourcemap: &Vec<(bytecodegen::BcIndex, usize)>,
        iseq_id: ISeqId,
    ) {
        let (start, code_end, end) = self.jit.code_block.last().unwrap();
        eprintln!(
            "      offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.jit.select_page(0);
        let dump = self.jit.dump_code().unwrap();
        // x86-64 objdump pads the hex-bytes column to a fixed width, so the
        // mnemonic starts a fixed `i + 24` chars past the `:`. aarch64
        // instructions are always 4 bytes (`xxxxxxxx `), a much narrower
        // column, so parse its objdump output by tab field instead:
        // `<off>:\t<bytes>\t<mnemonic>\t<operands>`.
        #[cfg(jit_x86)]
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                let i = x.find(':').unwrap();
                (
                    match usize::from_str_radix(&x[0..i].trim(), 16) {
                        Ok(i) => i,
                        _ => {
                            panic!("{}", &x[0..i].trim());
                        }
                    },
                    x[i + 24..].to_string(),
                )
            })
            .collect();
        #[cfg(not(jit_x86))]
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter_map(|x| {
                let i = x.find(':')?;
                let off = usize::from_str_radix(x[0..i].trim(), 16).ok()?;
                let text = x[i + 1..].splitn(3, '\t').nth(2)?.to_string();
                Some((off, text))
            })
            .collect();
        let iseq = &store[iseq_id];
        for (i, text) in dump {
            sourcemap
                .iter()
                .filter_map(
                    |(bc_pos, code_pos)| {
                        if *code_pos == i { Some(*bc_pos) } else { None }
                    },
                )
                .for_each(|bc_pos| {
                    if iseq.bb_info.is_bb_head(bc_pos).is_some() {
                        eprintln!("  {:?}", iseq.bb_info.get_bb_id(bc_pos));
                    }
                    let pc = iseq.get_pc(bc_pos);
                    eprintln!(
                        "    {bc_pos} {}",
                        match jitgen::trace_ir::TraceIr::format(store, iseq_id, pc) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("      {:06x}: {}", i, text);
        }
    }
}

// handling invariants

impl Codegen {
    pub fn check_bop_redefine(cfp: Cfp) {
        CODEGEN.with(|codegen| {
            let mut codegen = codegen.borrow_mut();
            if codegen.bop_redefine_flags() != 0 {
                // Eviction only applies to JIT-compiled code; the VM-only
                // build has none, so `cfp` goes unused there.
                #[cfg(jit)]
                codegen.immediate_eviction(cfp);
                #[cfg(not(jit))]
                let _ = cfp;
            }
        });
    }

    fn bop_redefine_flags(&self) -> u32 {
        let addr = self
            .jit
            .get_label_address(&self.bop_redefined_flags)
            .as_ptr() as *mut u32;
        unsafe { *addr }
    }

    #[cfg(jit)]
    fn immediate_eviction(&mut self, mut cfp: Cfp) {
        let mut return_addr = unsafe { cfp.return_addr() };
        while let Some(prev_cfp) = cfp.prev() {
            let ret = return_addr.unwrap();
            if !self.check_vm_address(ret) {
                if let Some((patch_point, deopt)) = self.get_deopt_with_return_addr(ret) {
                    let patch_point = patch_point.unwrap();
                    self.patch_return_to_deopt(patch_point, &deopt);
                }
            }
            cfp = prev_cfp;
            return_addr = unsafe { cfp.return_addr() };
        }
    }

    /// Overwrite the instruction(s) at a specialized call's return continuation
    /// (`patch_point`) so the now-stale frame deopts when control returns to it.
    /// x86 has a coherent I-cache and RWX pages, so it just writes the
    /// `jmp deopt` in place.
    #[cfg(jit_x86)]
    fn patch_return_to_deopt(&mut self, patch_point: CodePtr, deopt: &DestLabel) {
        self.jit.apply_jmp_patch_address(patch_point, deopt);
        unsafe { patch_point.as_ptr().write(0xe9) };
    }

    /// aarch64 twin: overwrite the single 4-byte instruction at `patch_point`
    /// with an unconditional `B deopt` (`0x14000000 | imm26`). Unlike x86 this
    /// must (a) make the page writable and (b) synchronize the I-cache, since
    /// AArch64 does not keep I/D caches coherent across self-modifying code —
    /// `set_writable`/`set_executable` handle both (the latter invalidates the
    /// I-cache; both are no-ops on Linux's RWX pages except that invalidation).
    #[cfg(all(jit, not(jit_x86)))]
    fn patch_return_to_deopt(&mut self, patch_point: CodePtr, deopt: &DestLabel) {
        self.jit.set_writable();
        let dest = self.jit.get_label_address(deopt);
        // Byte displacement from the patch point to the deopt handler; both are
        // 4-byte aligned, so imm26 = disp / 4 (B's range is ±128 MiB).
        let disp = dest - patch_point;
        let imm26 = ((disp >> 2) as u32) & 0x03ff_ffff;
        let word = 0x1400_0000u32 | imm26;
        unsafe { (patch_point.as_ptr() as *mut u32).write(word) };
        self.jit.set_executable();
    }

    #[cfg(jit)]
    fn get_deopt_with_return_addr(
        &self,
        return_addr: CodePtr,
    ) -> Option<(Option<CodePtr>, DestLabel)> {
        self.return_addr_table.get(&return_addr).cloned()
    }
}

#[repr(C)]
struct InstanceVarCache {
    class_id: ClassId,
    ivar_id: IvarId,
}

extern "C" fn get_instance_var_with_cache(
    mut base: Value,
    name: IdentId,
    globals: &mut Globals,
    cache: &mut InstanceVarCache,
) -> Value {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => return Value::nil(),
    };
    if class_id == cache.class_id {
        return rval.get_ivar_by_ivarid(cache.ivar_id).unwrap_or_default();
    }
    let ivar_id = globals.store.get_ivar_id(class_id, name);
    *cache = InstanceVarCache { class_id, ivar_id };
    rval.get_ivar_by_ivarid(ivar_id).unwrap_or_default()
}

extern "C" fn set_instance_var_with_cache(
    vm: &mut Executor,
    globals: &mut Globals,
    mut base: Value,
    name: IdentId,
    val: Value,
    cache: &mut InstanceVarCache,
) -> Option<Value> {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => {
            vm.err_cant_modify_frozen(&globals.store, base);
            return None;
        }
    };
    if rval.is_frozen() {
        vm.err_cant_modify_frozen(&globals.store, base);
        return None;
    }
    if class_id == cache.class_id {
        rval.set_ivar_by_ivarid(cache.ivar_id, val);
        // Return the assigned value: an `attr_writer`-generated `name=`
        // method must yield it (e.g. `obj.method(:x=).call(v) == v`).
        // The VM ivar-store path ignores this (uses it only for the
        // None = error check), so this is safe there too.
        return Some(val);
    }
    let ivar_id = globals.store.get_ivar_id(class_id, name);
    let new_cache = InstanceVarCache { class_id, ivar_id };
    *cache = new_cache;
    rval.set_ivar_by_ivarid(ivar_id, val);
    Some(val)
}

extern "C" fn stack_overflow(executor: &mut Executor) -> Option<Value> {
    executor.set_error(MonorubyErr::runtimeerr("StackOverflow"));
    None
}

#[cfg(feature = "profile")]
extern "C" fn guard_fail(vm: &mut Executor, globals: &mut Globals, self_val: Value) {
    let func_id = vm.cfp().lfp().func_id();
    globals.jit_class_guard_failed(func_id, self_val.class());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guard_class() {
        let mut r#gen = Codegen::new();

        for (class, value) in [
            (INTEGER_CLASS, Value::integer(-2558)),
            (INTEGER_CLASS, Value::integer(i32::MAX as i64)),
            (INTEGER_CLASS, Value::integer(i32::MIN as i64)),
            (FLOAT_CLASS, Value::float(1.44e-17)),
            (FLOAT_CLASS, Value::float(0.0)),
            (FLOAT_CLASS, Value::float(f64::NAN)),
            (FLOAT_CLASS, Value::float(f64::INFINITY)),
            (FLOAT_CLASS, Value::float(f64::NEG_INFINITY)),
            (FLOAT_CLASS, Value::float(f64::MAX)),
            (FLOAT_CLASS, Value::float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
            (SYMBOL_CLASS, Value::symbol_from_str("Ruby")),
            // The runtime asm `get_class` returns `BOOL_CLASS` for both
            // booleans so the IC sees a single class id.
            (BOOL_CLASS, Value::bool(true)),
            (BOOL_CLASS, Value::bool(false)),
            (ARRAY_CLASS, Value::array_from_vec(vec![])),
            (HASH_CLASS, Value::hash(RubyMap::default())),
            (STRING_CLASS, Value::string_from_str("Ruby")),
        ] {
            let func = r#gen.jit.get_label_addr(&r#gen.get_class);

            assert_eq!(class, func(value))
        }
    }

    // `f64_to_val` is a JIT-tier helper emitted only by the x86-64
    // `gen_f64_to_val` (`#[cfg(target_arch = "x86_64")]`); the aarch64
    // VM-only backend installs a `brk` trap stub for the label and never
    // calls it at runtime. Invoking the stub from this test would fault,
    // so the test is x86-64-only, matching the code it exercises.
    #[cfg(target_arch = "x86_64")]
    #[test]
    fn test_f64_to_val() {
        let mut r#gen = Codegen::new();

        for f in [
            1.44e-17,
            0.0,
            1285.333,
            -7512.0255,
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MAX,
            f64::MIN,
        ] {
            let f64_to_val = r#gen.f64_to_val.clone();
            let func: extern "C" fn(f64) -> Value = r#gen.jit.get_label_addr(&f64_to_val);
            if f.is_nan() {
                assert!(func(f).try_float().unwrap().is_nan())
            } else {
                assert_eq!(Value::float(f).try_float(), func(f).try_float());
            }
        }
    }
}
