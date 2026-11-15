//!
//! The built-in profiler (`--features prof`): where monoruby itself spends
//! its time and memory, for tuning the interpreter rather than the Ruby
//! program.
//!
//! A `prof` build records nothing unless `MONORUBY_PROF` names what to
//! collect, and a build without the feature carries none of this code.
//!
//! | `MONORUBY_PROF` | what is recorded                                        |
//! | --------------- | ------------------------------------------------------- |
//! | `cpu`           | `SIGPROF` samples of the Ruby stack + where the PC was  |
//! | `alloc`         | every object allocation, by class and allocating method |
//! | `malloc`        | every `malloc`/`free` counted, every N-th one's stacks  |
//! | `all`           | all three                                               |
//!
//! Tuning knobs (environment):
//! - `MONORUBY_PROF_HZ` — CPU sampling rate (default 1000).
//! - `MONORUBY_PROF_CLOCK` — `wall` (default): a high-resolution timer
//!   on the main thread's wall clock, so time blocked in a syscall shows
//!   up as samples in it; `cpu`: the process's CPU time (`ITIMER_PROF`,
//!   every thread, but only as fine as the kernel tick, often 4 ms).
//! - `MONORUBY_PROF_ALLOC_EVERY` — record the full stack of every N-th
//!   object allocation (default 64; the per-class / per-method counts are
//!   exact regardless).
//! - `MONORUBY_PROF_MALLOC_EVERY` — record the stack of every N-th
//!   `malloc` (default 64).
//! - `MONORUBY_PROF_OUT=prefix` — write the report to `prefix.txt` and
//!   the stacks as folded text (flamegraph.pl / speedscope / inferno) to
//!   `prefix.{cpu,alloc,malloc}.folded`. Without it the report goes to
//!   stderr and no stacks are written.
//! - `MONORUBY_PROF_TOP` — rows per table (default 30).
//! - `MONORUBY_PROF_BUF_MB` — sample buffer size (default 512, reserved
//!   lazily; samples past it are counted as dropped).
//!
//! ## How a sample is taken
//!
//! The profiler keeps, per OS thread, a pointer to the `Executor` whose
//! code is running there ([`set_executor`], moved at every fiber and
//! green-thread switch) and a *phase* byte saying which part of the
//! runtime is active (GC, JIT compilation, parsing / bytecode generation,
//! the global allocator). A sample is the phase, the machine PC, the
//! `FuncId`s of the Ruby frames (the `cfp` chain, newest first) and a few
//! native return addresses (the frame-pointer chain).
//!
//! The CPU sampler takes it in the `SIGPROF` handler, so every read of a
//! frame goes through [`Reader`]: addresses in the interrupted thread's
//! own stack are read directly, anything else (heap-allocated frames, a
//! fiber's stack) is first probed with `process_vm_readv`, which fails
//! instead of faulting. That matters for the newest frame only: a call
//! publishes its `cfp` before it stores the frame's `lfp`, so for two
//! instructions the newest `cfp` holds a stale `lfp`. A stale `FuncId` is
//! dropped at report time if it does not name a function.
//!
//! Everything is classified at exit, when the code regions are known: a PC
//! in the VM's code is interpreter time, in a JIT unit is that unit's
//! time, elsewhere in JIT memory is a stub / invoker / native-method
//! wrapper, and outside it is Rust or libc (symbolized).
//!
//! Frameless-inlined callees have no frame of their own, so their time
//! and allocations are reported against the method they were inlined
//! into.
//!

use crate::*;
use std::cell::Cell;
use std::collections::HashMap;
use std::io::Write;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, AtomicUsize, Ordering};

//
// Configuration
//

pub(crate) struct Config {
    pub cpu: bool,
    pub alloc: bool,
    pub malloc: bool,
    hz: u32,
    /// Sample on the main thread's wall clock (a high-resolution timer)
    /// rather than on the process's CPU time (`ITIMER_PROF`, whose
    /// resolution is the kernel tick, typically 4 ms).
    wall: bool,
    alloc_every: u64,
    malloc_every: u64,
    out: Option<String>,
    top: usize,
    buf_words: usize,
}

static CONFIG: OnceLock<Config> = OnceLock::new();

fn env_num(name: &str, default: u64) -> u64 {
    std::env::var(name)
        .ok()
        .and_then(|s| s.trim().parse::<u64>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(default)
}

///
/// The profiler's configuration, read from the environment once.
///
/// Must not be first called from inside the global allocator (reading the
/// environment allocates); [`init`] is called early from `Globals::new`.
///
pub(crate) fn config() -> &'static Config {
    CONFIG.get_or_init(|| {
        let spec = std::env::var("MONORUBY_PROF").unwrap_or_default();
        let mut cfg = Config {
            cpu: false,
            alloc: false,
            malloc: false,
            hz: env_num("MONORUBY_PROF_HZ", 1000).min(100_000) as u32,
            wall: std::env::var("MONORUBY_PROF_CLOCK").as_deref() != Ok("cpu"),
            alloc_every: env_num("MONORUBY_PROF_ALLOC_EVERY", 64),
            malloc_every: env_num("MONORUBY_PROF_MALLOC_EVERY", 64),
            out: std::env::var("MONORUBY_PROF_OUT")
                .ok()
                .filter(|s| !s.is_empty()),
            top: env_num("MONORUBY_PROF_TOP", 30) as usize,
            buf_words: env_num("MONORUBY_PROF_BUF_MB", 512) as usize * (1 << 20) / 8,
        };
        for item in spec.split(',').map(str::trim) {
            match item {
                "cpu" => cfg.cpu = true,
                "alloc" => cfg.alloc = true,
                "malloc" => cfg.malloc = true,
                "all" | "1" => {
                    cfg.cpu = true;
                    cfg.alloc = true;
                    cfg.malloc = true;
                }
                "" => {}
                other => eprintln!("monoruby prof: unknown MONORUBY_PROF item `{other}` ignored"),
            }
        }
        cfg
    })
}

fn enabled() -> bool {
    let c = config();
    c.cpu || c.alloc || c.malloc
}

/// Whether object allocations are profiled. The JIT then leaves out its
/// inline allocation fast path, so that every allocation reaches
/// `RValue::pack`.
pub(crate) fn alloc_enabled() -> bool {
    config().alloc
}

/// `malloc` sampling period, for the global allocator (0 = off). An atomic
/// rather than [`config`]: the allocator must not take the `OnceLock`.
static MALLOC_EVERY: AtomicU64 = AtomicU64::new(0);
/// Object-allocation profiling is on (mirrors `config().alloc`).
static ALLOC_ON: AtomicBool = AtomicBool::new(false);
/// The process that started profiling (a forked child reports nothing).
static OWNER_PID: AtomicUsize = AtomicUsize::new(0);
static INIT: std::sync::Once = std::sync::Once::new();
static START: OnceLock<std::time::Instant> = OnceLock::new();

///
/// Read the configuration and prepare the sample buffer. Idempotent; the
/// CPU timer itself is armed by [`start`], once the main program runs.
///
pub(crate) fn init() {
    INIT.call_once(|| {
        let cfg = config();
        if !enabled() {
            return;
        }
        OWNER_PID.store(std::process::id() as usize, Ordering::Relaxed);
        ACTIVE.store(true, Ordering::Relaxed);
        let _ = START.set(std::time::Instant::now());
        buf_init(cfg.buf_words);
        ALLOC_ON.store(cfg.alloc, Ordering::Relaxed);
        if cfg.malloc {
            MALLOC_EVERY.store(cfg.malloc_every, Ordering::Relaxed);
        }
    });
}

//
// Per-thread context
//

thread_local! {
    /// The `Executor` whose Ruby code runs on this OS thread (0 = none).
    static EXEC: Cell<usize> = const { Cell::new(0) };
    /// Upper end of this OS thread's own stack (0 = unknown).
    static STACK_HI: Cell<usize> = const { Cell::new(0) };
    /// The runtime phase this thread is in ([`Phase`]).
    static PHASE: Cell<u8> = const { Cell::new(0) };
    /// Set while the profiler itself runs on this thread, so that its own
    /// allocations are not sampled.
    static BUSY: Cell<bool> = const { Cell::new(false) };
    static MALLOC_TICK: Cell<u64> = const { Cell::new(0) };
    static ALLOC_TICK: Cell<u64> = const { Cell::new(0) };
}

///
/// What part of the runtime a thread is in. Nested phases restore the
/// outer one ([`PhaseGuard`]); a sample is charged to the innermost.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Phase {
    /// Ruby code, builtins and the rest of the runtime.
    Run = 0,
    Gc = 1,
    JitCompile = 2,
    /// Parsing and bytecode generation.
    Parse = 3,
    /// Inside the global allocator (`malloc` / `free` / `realloc`).
    Malloc = 4,
    /// The profiler's own bookkeeping (allocation hooks, malloc stacks).
    Prof = 5,
}

/// Whether anything is being profiled: without it a phase change is not
/// even recorded, so a `prof` build that profiles nothing stays close to a
/// plain one.
static ACTIVE: AtomicBool = AtomicBool::new(false);

/// Restores the outer phase on drop (`u8::MAX`: nothing to restore).
pub(crate) struct PhaseGuard(u8);

impl Drop for PhaseGuard {
    #[inline]
    fn drop(&mut self) {
        if self.0 != u8::MAX {
            let _ = PHASE.try_with(|p| p.set(self.0));
        }
    }
}

/// Enter *phase* until the guard drops.
#[inline]
pub(crate) fn phase(phase: Phase) -> PhaseGuard {
    if !ACTIVE.load(Ordering::Relaxed) {
        return PhaseGuard(u8::MAX);
    }
    PhaseGuard(PHASE.try_with(|p| p.replace(phase as u8)).unwrap_or(0))
}

///
/// *exec* is now the executor running on this OS thread (`null`: none).
/// Called wherever control moves to another executor: the main program's
/// start, a native thread's body, every fiber switch and green-thread
/// resume.
///
#[inline]
pub(crate) fn set_executor(exec: *const Executor) {
    EXEC.set(exec as usize);
}

///
/// This OS thread starts running Ruby on *exec*: remember the thread's own
/// stack bounds, which the sampler reads without probing.
///
pub(crate) fn enter_thread(exec: *const Executor) {
    set_executor(exec);
    // SAFETY: plain pthread queries on the calling thread; `attr` is
    // initialized by `pthread_getattr_np` before it is read and destroyed
    // afterwards.
    #[cfg(target_os = "linux")]
    unsafe {
        let mut attr: libc::pthread_attr_t = std::mem::zeroed();
        if libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0 {
            let mut addr: *mut libc::c_void = std::ptr::null_mut();
            let mut size: libc::size_t = 0;
            if libc::pthread_attr_getstack(&attr, &mut addr, &mut size) == 0 {
                STACK_HI.set(addr as usize + size);
            }
            libc::pthread_attr_destroy(&mut attr);
        }
    }
    // SAFETY: plain pthread queries on the calling thread.
    #[cfg(target_os = "macos")]
    unsafe {
        STACK_HI.set(libc::pthread_get_stackaddr_np(libc::pthread_self()) as usize);
    }
}

//
// Sample buffer
//
// One reservation of `buf_words` u64s, committed lazily by the kernel, and
// an atomic cursor. A record is reserved with one `fetch_add` once its
// length is known, so a signal handler and the code it interrupted (or two
// threads) never write the same words.
//
// Record layout:
//   w0  kind << 56 | phase << 48 | native_len << 32 | ruby_len
//   w1  CPU: the PC; ALLOC: the ClassId; MALLOC: the size
//   ..  ruby_len FuncIds, newest frame first
//   ..  native_len return addresses, innermost first
//

const KIND_CPU: u64 = 1;
const KIND_ALLOC: u64 = 2;
const KIND_MALLOC: u64 = 3;

const MAX_RUBY: usize = 256;
const MAX_NATIVE: usize = 16;

static BUF: AtomicPtr<u64> = AtomicPtr::new(std::ptr::null_mut());
static BUF_LEN: AtomicUsize = AtomicUsize::new(0);
static BUF_POS: AtomicUsize = AtomicUsize::new(0);
static DROPPED: AtomicUsize = AtomicUsize::new(0);

fn buf_init(words: usize) {
    // SAFETY: an anonymous private mapping, owned by this module for the
    // rest of the process.
    let p = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            words * 8,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE,
            -1,
            0,
        )
    };
    if p == libc::MAP_FAILED {
        eprintln!("monoruby prof: could not reserve the sample buffer");
        return;
    }
    BUF_LEN.store(words, Ordering::Relaxed);
    BUF.store(p as *mut u64, Ordering::Release);
}

fn buf_push(kind: u64, phase: u8, w1: u64, ruby: &[u64], native: &[u64]) {
    let buf = BUF.load(Ordering::Acquire);
    if buf.is_null() {
        return;
    }
    let len = 2 + ruby.len() + native.len();
    let pos = BUF_POS.fetch_add(len, Ordering::Relaxed);
    if pos + len > BUF_LEN.load(Ordering::Relaxed) {
        DROPPED.fetch_add(1, Ordering::Relaxed);
        return;
    }
    // SAFETY: `pos..pos + len` was reserved for this record alone and lies
    // inside the mapping.
    unsafe {
        let p = buf.add(pos);
        *p = kind << 56 | (phase as u64) << 48 | (native.len() as u64) << 32 | ruby.len() as u64;
        *p.add(1) = w1;
        std::ptr::copy_nonoverlapping(ruby.as_ptr(), p.add(2), ruby.len());
        std::ptr::copy_nonoverlapping(native.as_ptr(), p.add(2 + ruby.len()), native.len());
    }
}

//
// Reading frames
//

///
/// A fault-free reader of the interrupted thread's memory.
///
/// `[lo, hi)` is known to be mapped (the live part of the thread's own
/// stack); other addresses are probed with `process_vm_readv`, and the
/// pages that answered are remembered for the rest of the sample.
///
struct Reader {
    /// Synchronous use (an allocation hook): the frames are consistent,
    /// so every address read is trusted and dereferenced directly.
    trusted: bool,
    lo: usize,
    hi: usize,
    pages: [usize; 16],
    n: usize,
}

impl Reader {
    fn new(sp: usize) -> Self {
        let hi = STACK_HI.try_with(|h| h.get()).unwrap_or(0);
        let (lo, hi) = if hi > sp { (sp, hi) } else { (0, 0) };
        Reader {
            trusted: false,
            lo,
            hi,
            pages: [0; 16],
            n: 0,
        }
    }

    /// A reader for code that called the profiler itself, not a signal
    /// handler: no frame is half-built there.
    fn trusted() -> Self {
        Reader {
            trusted: true,
            lo: 0,
            hi: 0,
            pages: [0; 16],
            n: 0,
        }
    }

    fn read(&mut self, addr: usize) -> Option<u64> {
        if addr < 4096 || addr % 8 != 0 {
            return None;
        }
        // SAFETY: a trusted reader only follows the executor's own,
        // consistent frame chain and the frame-pointer chain.
        if self.trusted {
            return Some(unsafe { *(addr as *const u64) });
        }
        // SAFETY: inside the live part of this thread's stack.
        if addr >= self.lo && addr + 8 <= self.hi {
            return Some(unsafe { *(addr as *const u64) });
        }
        let page = addr & !4095;
        // SAFETY: a page a probe has read from during this sample; nothing
        // unmaps memory while the sampled thread is stopped here.
        if self.pages[..self.n].contains(&page) {
            return Some(unsafe { *(addr as *const u64) });
        }
        // Only the CPU sampler reads untrusted memory, and it is Linux only.
        #[cfg(not(target_os = "linux"))]
        return None;
        #[cfg(target_os = "linux")]
        self.probe(addr, page)
    }

    #[cfg(target_os = "linux")]
    fn probe(&mut self, addr: usize, page: usize) -> Option<u64> {
        let mut v = 0u64;
        let local = libc::iovec {
            iov_base: &mut v as *mut u64 as *mut libc::c_void,
            iov_len: 8,
        };
        let remote = libc::iovec {
            iov_base: addr as *mut libc::c_void,
            iov_len: 8,
        };
        // SAFETY: `process_vm_readv` on our own pid reports a bad address
        // as an error instead of faulting; it is async-signal-safe.
        let n = unsafe { libc::process_vm_readv(libc::getpid(), &local, 1, &remote, 1, 0) };
        if n != 8 {
            return None;
        }
        if self.n < self.pages.len() {
            self.pages[self.n] = page;
            self.n += 1;
        }
        Some(v)
    }
}

///
/// The `FuncId`s of *exec*'s Ruby frames, newest first, into *out*.
///
fn walk_ruby(r: &mut Reader, exec: usize, out: &mut [u64]) -> usize {
    if exec == 0 {
        return 0;
    }
    let Some(mut cfp) = r.read(exec + executor::EXECUTOR_CFP as usize) else {
        return 0;
    };
    let mut n = 0;
    while cfp != 0 && n < out.len() {
        let cfp_addr = cfp as usize;
        let Some(lfp) = r.read(cfp_addr.wrapping_sub(executor::CFP_LFP as usize)) else {
            break;
        };
        let Some(meta) = r.read((lfp as usize).wrapping_sub(executor::LFP_META as usize)) else {
            break;
        };
        out[n] = meta & 0xffff_ffff;
        n += 1;
        match r.read(cfp_addr) {
            Some(prev) => cfp = prev,
            None => break,
        }
    }
    n
}

///
/// The `FuncId` of *exec*'s newest frame (0: none), read directly: only
/// for code that called the profiler, where the frames are consistent.
///
fn top_func(exec: usize) -> u32 {
    if exec == 0 {
        return 0;
    }
    // SAFETY: *exec* is the live executor of this thread, whose newest
    // control frame and its local frame are fully built (see `Reader`).
    unsafe {
        let cfp = *((exec + executor::EXECUTOR_CFP as usize) as *const usize);
        if cfp == 0 {
            return 0;
        }
        let lfp = *((cfp - executor::CFP_LFP as usize) as *const usize);
        if lfp < 4096 {
            return 0;
        }
        *((lfp - executor::LFP_META as usize) as *const u32)
    }
}

///
/// Return addresses of the frame-pointer chain starting at *fp*.
///
fn walk_native(r: &mut Reader, mut fp: usize, out: &mut [u64]) -> usize {
    let mut n = 0;
    while n < out.len() {
        let (Some(next), Some(ret)) = (r.read(fp), r.read(fp + 8)) else {
            break;
        };
        if ret == 0 {
            break;
        }
        out[n] = ret;
        n += 1;
        let next = next as usize;
        if next <= fp {
            break;
        }
        fp = next;
    }
    n
}

fn take_sample(kind: u64, w1: u64, mut r: Reader, fp: usize, native_depth: usize) {
    let mut ruby = [0u64; MAX_RUBY];
    let mut native = [0u64; MAX_NATIVE];
    let exec = EXEC.try_with(|e| e.get()).unwrap_or(0);
    let phase = PHASE.try_with(|p| p.get()).unwrap_or(0);
    let nr = walk_ruby(&mut r, exec, &mut ruby);
    let nn = if fp != 0 {
        walk_native(&mut r, fp, &mut native[..native_depth.min(MAX_NATIVE)])
    } else {
        0
    };
    buf_push(kind, phase, w1, &ruby[..nr], &native[..nn]);
}

#[inline(always)]
fn current_fp() -> usize {
    let fp: usize;
    // SAFETY: reads the frame-pointer register (`-Cforce-frame-pointers`
    // keeps it a frame pointer throughout the binary).
    #[cfg(target_arch = "x86_64")]
    unsafe {
        std::arch::asm!("mov {}, rbp", out(reg) fp, options(nomem, nostack, preserves_flags))
    };
    #[cfg(target_arch = "aarch64")]
    unsafe {
        std::arch::asm!("mov {}, x29", out(reg) fp, options(nomem, nostack, preserves_flags))
    };
    fp
}

//
// CPU sampler
//

static CPU_SAMPLES: AtomicUsize = AtomicUsize::new(0);
static CPU_RUNNING: AtomicBool = AtomicBool::new(false);

#[cfg(target_os = "linux")]
extern "C" fn on_sigprof(_sig: libc::c_int, _info: *mut libc::siginfo_t, ctx: *mut libc::c_void) {
    // The handler may run between any two instructions of the program, so
    // it keeps errno (the probe syscall can set it) and allocates nothing.
    // SAFETY: `__errno_location` is this thread's errno; `ctx` is the
    // `ucontext_t` the kernel handed an `SA_SIGINFO` handler.
    unsafe {
        let errno = *libc::__errno_location();
        let uc = ctx as *const libc::ucontext_t;
        #[cfg(target_arch = "x86_64")]
        let (pc, sp, fp) = {
            let g = &(*uc).uc_mcontext.gregs;
            (
                g[libc::REG_RIP as usize] as usize,
                g[libc::REG_RSP as usize] as usize,
                g[libc::REG_RBP as usize] as usize,
            )
        };
        #[cfg(target_arch = "aarch64")]
        let (pc, sp, fp) = {
            let m = &(*uc).uc_mcontext;
            (m.pc as usize, m.sp as usize, m.regs[29] as usize)
        };
        CPU_SAMPLES.fetch_add(1, Ordering::Relaxed);
        take_sample(KIND_CPU, pc as u64, Reader::new(sp), fp, 8);
        *libc::__errno_location() = errno;
    }
}

/// Wall-clock and CPU time when sampling started.
static CLOCK0: Mutex<Option<(std::time::Instant, f64)>> = Mutex::new(None);

fn cpu_seconds() -> f64 {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    // SAFETY: plain clock_gettime(2).
    unsafe { libc::clock_gettime(libc::CLOCK_PROCESS_CPUTIME_ID, &mut ts) };
    ts.tv_sec as f64 + ts.tv_nsec as f64 * 1e-9
}

/// The POSIX timer of the wall-clock mode.
#[cfg(target_os = "linux")]
static WALL_TIMER: AtomicUsize = AtomicUsize::new(0);

#[cfg(target_os = "linux")]
fn arm_timer(cfg: &Config) {
    let nsec = 1_000_000_000 / cfg.hz.max(1) as i64;
    let interval = libc::timespec {
        tv_sec: nsec / 1_000_000_000,
        tv_nsec: nsec % 1_000_000_000,
    };
    // SAFETY: timer_create(2) / setitimer(2) with fully initialized
    // arguments; the timer is owned by this module.
    unsafe {
        if cfg.wall {
            let mut sev: libc::sigevent = std::mem::zeroed();
            sev.sigev_notify = libc::SIGEV_THREAD_ID;
            sev.sigev_signo = libc::SIGPROF;
            sev.sigev_notify_thread_id = libc::gettid();
            let mut timer: libc::timer_t = std::mem::zeroed();
            if libc::timer_create(libc::CLOCK_MONOTONIC, &mut sev, &mut timer) != 0 {
                eprintln!(
                    "monoruby prof: timer_create failed ({})",
                    std::io::Error::last_os_error()
                );
                return;
            }
            let spec = libc::itimerspec {
                it_interval: interval,
                it_value: interval,
            };
            libc::timer_settime(timer, 0, &spec, std::ptr::null_mut());
            WALL_TIMER.store(timer as usize, Ordering::Relaxed);
        } else {
            let tv = libc::timeval {
                tv_sec: interval.tv_sec,
                tv_usec: (interval.tv_nsec / 1000).max(1) as _,
            };
            let it = libc::itimerval {
                it_interval: tv,
                it_value: tv,
            };
            libc::setitimer(libc::ITIMER_PROF, &it, std::ptr::null_mut());
        }
    }
}

#[cfg(target_os = "linux")]
fn disarm_timer(cfg: &Config) {
    // SAFETY: stops the timer `arm_timer` started.
    unsafe {
        if cfg.wall {
            let t = WALL_TIMER.swap(0, Ordering::Relaxed);
            if t != 0 {
                libc::timer_delete(t as libc::timer_t);
            }
        } else {
            let zero: libc::itimerval = std::mem::zeroed();
            libc::setitimer(libc::ITIMER_PROF, &zero, std::ptr::null_mut());
        }
    }
}

///
/// The main program starts on *exec*: from here on its thread is
/// profiled, and the sampling timer runs.
///
pub(crate) fn start(exec: *const Executor) {
    init();
    enter_thread(exec);
    let cfg = config();
    if !cfg.cpu || CPU_RUNNING.swap(true, Ordering::Relaxed) {
        return;
    }
    *CLOCK0.lock().unwrap() = Some((std::time::Instant::now(), cpu_seconds()));
    #[cfg(target_os = "linux")]
    {
        // SAFETY: installs a handler that only touches thread-locals,
        // atomics and the sample buffer.
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = on_sigprof as *const () as usize;
            sa.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
            libc::sigemptyset(&mut sa.sa_mask);
            libc::sigaction(libc::SIGPROF, &sa, std::ptr::null_mut());
        }
        arm_timer(cfg);
    }
    #[cfg(not(target_os = "linux"))]
    eprintln!("monoruby prof: CPU sampling is only supported on Linux");
}

//
// Object allocation
//

/// (allocating method's FuncId, ClassId) -> count.
/// (allocating method's FuncId, ClassId) -> count, merged from the
/// per-thread tables when a thread ends and at [`finish`].
static ALLOCS: Mutex<Option<fxhash::FxHashMap<(u32, u32), u64>>> = Mutex::new(None);

#[derive(Default)]
struct AllocTable(std::cell::RefCell<fxhash::FxHashMap<(u32, u32), u64>>);

impl AllocTable {
    fn merge(&self) {
        let mine = std::mem::take(&mut *self.0.borrow_mut());
        if mine.is_empty() {
            return;
        }
        let mut g = ALLOCS.lock().unwrap();
        let g = g.get_or_insert_with(Default::default);
        for (k, n) in mine {
            *g.entry(k).or_insert(0) += n;
        }
    }
}

impl Drop for AllocTable {
    fn drop(&mut self) {
        self.merge();
    }
}

thread_local! {
    static ALLOC_TABLE: AllocTable = AllocTable::default();
}

///
/// An object of *class* is being allocated (`RValue::pack`).
///
#[inline]
pub(crate) fn on_object_alloc(class: ClassId) {
    if !ALLOC_ON.load(Ordering::Relaxed) || BUSY.get() {
        return;
    }
    BUSY.set(true);
    let _phase = phase(Phase::Prof);
    let func = top_func(EXEC.get());
    let _ =
        ALLOC_TABLE.try_with(|t| *t.0.borrow_mut().entry((func, class.u32())).or_insert(0) += 1);
    let tick = ALLOC_TICK.get() + 1;
    ALLOC_TICK.set(tick);
    if tick % config().alloc_every == 0 {
        take_sample(KIND_ALLOC, class.u32() as u64, Reader::trusted(), 0, 0);
    }
    BUSY.set(false);
}

//
// GC
//

#[derive(Default, Clone, Copy)]
struct GcStat {
    count: u64,
    ns: u64,
    mark_ns: u64,
    marked: u64,
}

/// [minor, major]
static GC_STATS: Mutex<[GcStat; 2]> = Mutex::new(
    [GcStat {
        count: 0,
        ns: 0,
        mark_ns: 0,
        marked: 0,
    }; 2],
);

///
/// A collection finished: *major* or not, its total and mark time, and
/// how many objects it marked.
///
pub(crate) fn on_gc(
    major: bool,
    total: std::time::Duration,
    mark: std::time::Duration,
    marked: usize,
) {
    if !enabled() {
        return;
    }
    let mut g = GC_STATS.lock().unwrap();
    let s = &mut g[major as usize];
    s.count += 1;
    s.ns += total.as_nanos() as u64;
    s.mark_ns += mark.as_nanos() as u64;
    s.marked += marked as u64;
}

//
// malloc
//

static MALLOC_CALLS: AtomicU64 = AtomicU64::new(0);
static MALLOC_BYTES: AtomicU64 = AtomicU64::new(0);
static FREE_CALLS: AtomicU64 = AtomicU64::new(0);
static FREE_BYTES: AtomicU64 = AtomicU64::new(0);

///
/// The global allocator is entered to hand out *size* bytes; the guard
/// keeps the thread in [`Phase::Malloc`] until the allocator returns.
///
#[inline(always)]
pub(crate) fn enter_malloc(size: usize) -> PhaseGuard {
    if !ACTIVE.load(Ordering::Relaxed) {
        return PhaseGuard(u8::MAX);
    }
    enter_malloc_slow(size)
}

#[inline(never)]
fn enter_malloc_slow(size: usize) -> PhaseGuard {
    let guard = phase(Phase::Malloc);
    on_malloc(size);
    guard
}

/// The global allocator is entered to take back *size* bytes.
#[inline(always)]
pub(crate) fn enter_free(size: usize) -> PhaseGuard {
    if !ACTIVE.load(Ordering::Relaxed) {
        return PhaseGuard(u8::MAX);
    }
    enter_free_slow(size)
}

#[inline(never)]
fn enter_free_slow(size: usize) -> PhaseGuard {
    let guard = phase(Phase::Malloc);
    on_free(size);
    guard
}

///
/// The global allocator hands out *size* bytes.
///
fn on_malloc(size: usize) {
    let every = MALLOC_EVERY.load(Ordering::Relaxed);
    if every == 0 {
        return;
    }
    MALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    MALLOC_BYTES.fetch_add(size as u64, Ordering::Relaxed);
    let Ok(tick) = MALLOC_TICK.try_with(|t| {
        let v = t.get() + 1;
        t.set(v);
        v
    }) else {
        return;
    };
    if tick % every != 0 || BUSY.try_with(|b| b.replace(true)).unwrap_or(true) {
        return;
    }
    let _phase = phase(Phase::Prof);
    take_sample(
        KIND_MALLOC,
        size as u64,
        Reader::trusted(),
        current_fp(),
        MAX_NATIVE,
    );
    let _ = BUSY.try_with(|b| b.set(false));
}

/// The global allocator takes back *size* bytes.
fn on_free(size: usize) {
    if MALLOC_EVERY.load(Ordering::Relaxed) == 0 {
        return;
    }
    FREE_CALLS.fetch_add(1, Ordering::Relaxed);
    FREE_BYTES.fetch_add(size as u64, Ordering::Relaxed);
}

//
// JIT
//

struct Unit {
    /// Machine code, main and cold page.
    ranges: [(usize, usize); 2],
    func: FuncId,
    class: ClassId,
    is_loop: bool,
    /// Compile time.
    ns: u64,
    /// The compile was abandoned (no code).
    failed: bool,
}

#[derive(Default)]
struct JitStat {
    units: Vec<Unit>,
    compiles: u64,
    loops: u64,
    failed: u64,
    ns: u64,
    code_bytes: u64,
}

static JIT: Mutex<Option<JitStat>> = Mutex::new(None);

///
/// A compilation of *func* for *class* finished in *elapsed*; its code
/// occupies *ranges* (`None`: the compile was abandoned).
///
pub(crate) fn on_jit_unit(
    func: FuncId,
    class: ClassId,
    is_loop: bool,
    elapsed: std::time::Duration,
    ranges: Option<[(usize, usize); 2]>,
) {
    if !enabled() {
        return;
    }
    let mut g = JIT.lock().unwrap();
    let s = g.get_or_insert_with(JitStat::default);
    s.compiles += 1;
    s.ns += elapsed.as_nanos() as u64;
    if is_loop {
        s.loops += 1;
    }
    if ranges.is_none() {
        s.failed += 1;
    }
    let ranges = ranges.unwrap_or_default();
    s.code_bytes += (ranges[0].1 + ranges[1].1) as u64;
    s.units.push(Unit {
        ranges,
        func,
        class,
        is_loop,
        ns: elapsed.as_nanos() as u64,
        failed: ranges == [(0, 0); 2],
    });
}

//
// Report
//

/// Where a CPU sample's time went.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Tier {
    JitCode,
    Vm,
    Stub,
    Native,
    Gc,
    JitCompile,
    Parse,
    Malloc,
    Prof,
    NoRuby,
}

const TIERS: [Tier; 10] = [
    Tier::JitCode,
    Tier::Vm,
    Tier::Stub,
    Tier::Native,
    Tier::Gc,
    Tier::JitCompile,
    Tier::Parse,
    Tier::Malloc,
    Tier::Prof,
    Tier::NoRuby,
];

impl Tier {
    fn label(self) -> &'static str {
        match self {
            Tier::JitCode => "JIT code",
            Tier::Vm => "VM interpreter",
            Tier::Stub => "JIT stubs / invokers / wrappers",
            Tier::Native => "builtins / runtime (Rust, libc)",
            Tier::Gc => "GC",
            Tier::JitCompile => "JIT compile",
            Tier::Parse => "parse / bytecodegen",
            Tier::Malloc => "malloc / free",
            Tier::Prof => "the profiler itself",
            Tier::NoRuby => "outside Ruby (no Ruby frame)",
        }
    }

    fn short(self) -> &'static str {
        match self {
            Tier::JitCode => "jit",
            Tier::Vm => "vm",
            Tier::Stub => "stub",
            Tier::Native => "native",
            Tier::Gc => "gc",
            Tier::JitCompile => "compile",
            Tier::Parse => "parse",
            Tier::Malloc => "malloc",
            Tier::Prof => "prof",
            Tier::NoRuby => "noruby",
        }
    }
}

struct Record<'a> {
    kind: u64,
    phase: u8,
    w1: u64,
    ruby: &'a [u64],
    native: &'a [u64],
}

fn records() -> Vec<Record<'static>> {
    let buf = BUF.load(Ordering::Acquire);
    let mut v = vec![];
    if buf.is_null() {
        return v;
    }
    let end = BUF_POS
        .load(Ordering::Relaxed)
        .min(BUF_LEN.load(Ordering::Relaxed));
    // SAFETY: `0..end` holds complete records (a record past the end of
    // the buffer was dropped without being written), and nothing writes
    // any more: the timer is off and the hooks are disabled.
    let words = unsafe { std::slice::from_raw_parts(buf, end) };
    let mut i = 0;
    while i + 2 <= words.len() {
        let h = words[i];
        let nr = (h & 0xffff_ffff) as usize;
        let nn = ((h >> 32) & 0xffff) as usize;
        let kind = h >> 56;
        if kind == 0 || i + 2 + nr + nn > words.len() {
            break;
        }
        v.push(Record {
            kind,
            phase: ((h >> 48) & 0xff) as u8,
            w1: words[i + 1],
            ruby: &words[i + 2..i + 2 + nr],
            native: &words[i + 2 + nr..i + 2 + nr + nn],
        });
        i += 2 + nr + nn;
    }
    v
}

/// Code regions for classifying PCs.
struct Regions {
    vm: [(usize, usize); 2],
    /// JIT units sorted by start: (start, end, unit index).
    units: Vec<(usize, usize, usize)>,
}

impl Regions {
    fn unit_of(&self, pc: usize) -> Option<usize> {
        let i = self.units.partition_point(|(s, _, _)| *s <= pc);
        if i == 0 {
            return None;
        }
        let (s, e, u) = self.units[i - 1];
        (s <= pc && pc < e).then_some(u)
    }

    fn in_vm(&self, pc: usize) -> bool {
        self.vm.iter().any(|(s, l)| *s <= pc && pc < *s + *l)
    }
}

struct Names<'a> {
    store: &'a Store,
    cache: HashMap<u32, String>,
}

impl<'a> Names<'a> {
    fn valid(&self, f: u64) -> bool {
        f != 0 && (f as usize) < self.store.func_len()
    }

    fn func(&mut self, f: u64) -> String {
        let f = f as u32;
        if let Some(s) = self.cache.get(&f) {
            return s.clone();
        }
        let s = if f == 0 || (f as usize) >= self.store.func_len() {
            "<unknown>".to_string()
        } else {
            let fid = FuncId::new(f);
            let mut s = self.store.func_description(fid);
            if s.is_empty() {
                s = format!("<func {f}>");
            }
            if let Some(iseq) = self.store[fid].is_iseq() {
                let info = &self.store[iseq];
                s += &format!(
                    " ({}:{})",
                    info.sourceinfo.short_file_name(),
                    info.sourceinfo.get_line(&info.loc)
                );
            } else {
                s += " (native)";
            }
            s
        };
        self.cache.insert(f, s.clone());
        s
    }
}

/// Native code symbolization, cached.
struct Symbols {
    cache: HashMap<usize, String>,
    /// The executable's own function symbols, `(start, size, name)` by
    /// start: `backtrace` misses the local `.llvm.*` ones LLVM renames.
    exe: Vec<(usize, usize, Vec<u8>)>,
    /// The executable's load bias.
    exe_base: usize,
}

///
/// The `STT_FUNC` entries of an ELF64 file's `.symtab`, sorted by
/// address. Empty if the file is not a little-endian ELF64 with one.
///
fn elf_functions(path: &str) -> Vec<(usize, usize, Vec<u8>)> {
    let Ok(b) = std::fs::read(path) else {
        return vec![];
    };
    let u16_at = |o: usize| {
        b.get(o..o + 2)
            .map(|s| u16::from_le_bytes([s[0], s[1]]) as usize)
    };
    let u32_at = |o: usize| {
        b.get(o..o + 4)
            .map(|s| u32::from_le_bytes(s.try_into().unwrap()) as usize)
    };
    let u64_at = |o: usize| {
        b.get(o..o + 8)
            .map(|s| u64::from_le_bytes(s.try_into().unwrap()) as usize)
    };
    if b.get(0..6) != Some(&[0x7f, b'E', b'L', b'F', 2, 1][..]) {
        return vec![];
    }
    let (Some(shoff), Some(shentsize), Some(shnum)) = (u64_at(0x28), u16_at(0x3a), u16_at(0x3c))
    else {
        return vec![];
    };
    let section = |i: usize| -> Option<(usize, usize, usize, usize, usize)> {
        let h = shoff + i * shentsize;
        Some((
            u32_at(h + 4)?,
            u64_at(h + 0x18)?,
            u64_at(h + 0x20)?,
            u32_at(h + 0x28)?,
            u64_at(h + 0x38)?,
        ))
    };
    let mut v = vec![];
    for i in 0..shnum {
        let Some((ty, off, size, link, entsize)) = section(i) else {
            continue;
        };
        // SHT_SYMTAB
        if ty != 2 || entsize < 24 {
            continue;
        }
        let Some((_, stroff, strsize, _, _)) = section(link) else {
            continue;
        };
        for e in (off..off + size).step_by(entsize) {
            let (Some(name), Some(value), Some(sz)) = (u32_at(e), u64_at(e + 8), u64_at(e + 16))
            else {
                break;
            };
            // STT_FUNC
            if b.get(e + 4).map(|i| i & 0xf) != Some(2) || value == 0 || name >= strsize {
                continue;
            }
            let start = stroff + name;
            let len = b[start..stroff + strsize]
                .iter()
                .position(|c| *c == 0)
                .unwrap_or(0);
            v.push((value, sz, b[start..start + len].to_vec()));
        }
    }
    v.sort_by_key(|e| e.0);
    v
}

impl Symbols {
    fn new() -> Self {
        // SAFETY: `dladdr` only reads the loader's tables.
        let exe_base = unsafe {
            let mut info: libc::Dl_info = std::mem::zeroed();
            if libc::dladdr(finish as *const libc::c_void, &mut info) != 0 {
                info.dli_fbase as usize
            } else {
                0
            }
        };
        Symbols {
            cache: HashMap::default(),
            exe: if exe_base != 0 {
                elf_functions("/proc/self/exe")
            } else {
                vec![]
            },
            exe_base,
        }
    }

    /// *addr* in the executable's own symbol table.
    fn exe_name(&self, addr: usize) -> Option<String> {
        // SAFETY: as in `new`.
        let in_exe = unsafe {
            let mut info: libc::Dl_info = std::mem::zeroed();
            libc::dladdr(addr as *const libc::c_void, &mut info) != 0
                && info.dli_fbase as usize == self.exe_base
        };
        if !in_exe {
            return None;
        }
        let off = addr - self.exe_base;
        let i = self.exe.partition_point(|e| e.0 <= off);
        let (start, size, name) = self.exe.get(i.checked_sub(1)?)?;
        let name = format!("{:#}", backtrace::SymbolName::new(name));
        Some(if off < start + (*size).max(1) {
            name
        } else {
            format!("{name}+{:#x}", off - start)
        })
    }

    fn name(&mut self, addr: usize) -> String {
        if let Some(s) = self.cache.get(&addr) {
            return s.clone();
        }
        let mut name = self.exe_name(addr);
        if name.is_none() {
            backtrace::resolve(addr as *mut std::ffi::c_void, |sym| {
                if name.is_none()
                    && let Some(n) = sym.name()
                {
                    name = Some(format!("{n:#}"));
                }
            });
        }
        let s = name.unwrap_or_else(|| {
            // No symbol: name the object and the offset, for addr2line.
            // SAFETY: `dladdr` only reads the loader's tables.
            unsafe {
                let mut info: libc::Dl_info = std::mem::zeroed();
                if libc::dladdr(addr as *const libc::c_void, &mut info) != 0
                    && !info.dli_fname.is_null()
                {
                    let file = std::ffi::CStr::from_ptr(info.dli_fname).to_string_lossy();
                    let file = file.rsplit('/').next().unwrap_or(&file).to_string();
                    return format!("{file}+{:#x}", addr - info.dli_fbase as usize);
                }
            }
            format!("{addr:#x}")
        });
        self.cache.insert(addr, s.clone());
        s
    }

    /// The function a return address belongs to (the call is one byte
    /// before it).
    fn caller(&mut self, ret: usize) -> String {
        self.name(ret.wrapping_sub(1))
    }
}

/// Native frames that only pass an allocation through.
fn is_alloc_plumbing(name: &str) -> bool {
    const PREFIXES: &[&str] = &[
        "monoruby::prof::",
        "monoruby::alloc::",
        "<monoruby::prof::",
        "<monoruby::alloc::RurubyAlloc",
        "__rust",
        "__rdl",
        "__rg_",
        "alloc::",
        "<alloc::",
        "core::",
        "<core::",
        "std::",
        "<std::",
        "hashbrown::",
        "<hashbrown::",
        "smallvec::",
        "<smallvec::",
        "indexmap::",
        "<indexmap::",
        "rubymap::",
        "<rubymap::",
        "malloc",
        "realloc",
        "calloc",
        "_int_",
        "__libc",
        "posix_memalign",
    ];
    PREFIXES.iter().any(|p| name.starts_with(p))
}

fn sanitize(s: &str) -> String {
    s.replace([';', '\n'], " ")
}

fn pct(n: u64, total: u64) -> f64 {
    if total == 0 {
        0.0
    } else {
        n as f64 * 100.0 / total as f64
    }
}

fn add<K: std::hash::Hash + Eq>(m: &mut HashMap<K, u64>, k: K, n: u64) {
    *m.entry(k).or_insert(0) += n;
}

fn sorted<K: Clone + Ord, S>(m: &HashMap<K, u64, S>) -> Vec<(K, u64)> {
    let mut v: Vec<_> = m.iter().map(|(k, n)| (k.clone(), *n)).collect();
    v.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    v
}

fn write_folded(path: &str, stacks: &HashMap<String, u64>) {
    let mut v: Vec<_> = stacks.iter().collect();
    v.sort();
    match std::fs::File::create(path) {
        Ok(f) => {
            let mut f = std::io::BufWriter::new(f);
            for (k, n) in v {
                let _ = writeln!(f, "{k} {n}");
            }
        }
        Err(e) => eprintln!("monoruby prof: cannot write {path}: {e}"),
    }
}

///
/// Stop profiling and write the report. Called once from the exit
/// sequence, after the `at_exit` handlers and finalizers have run.
///
pub(crate) fn finish(globals: &Globals) {
    if !enabled() || OWNER_PID.load(Ordering::Relaxed) != std::process::id() as usize {
        return;
    }
    static DONE: AtomicBool = AtomicBool::new(false);
    if DONE.swap(true, Ordering::Relaxed) {
        return;
    }
    #[cfg(target_os = "linux")]
    if CPU_RUNNING.load(Ordering::Relaxed) {
        disarm_timer(config());
    }
    let clock = CLOCK0
        .lock()
        .unwrap()
        .map(|(t, c)| (t.elapsed().as_secs_f64(), cpu_seconds() - c));
    ALLOC_ON.store(false, Ordering::Relaxed);
    MALLOC_EVERY.store(0, Ordering::Relaxed);
    let _busy = BUSY.replace(true);

    let cfg = config();
    let mut out = String::new();
    report(globals, cfg, clock, &mut out);
    match &cfg.out {
        Some(prefix) => {
            let path = format!("{prefix}.txt");
            match std::fs::write(&path, &out) {
                Ok(()) => eprintln!("monoruby prof: report written to {path}"),
                Err(e) => {
                    eprintln!("monoruby prof: cannot write {path}: {e}");
                    eprint!("{out}");
                }
            }
        }
        None => eprint!("{out}"),
    }
}

fn report(globals: &Globals, cfg: &Config, clock: Option<(f64, f64)>, out: &mut String) {
    use std::fmt::Write;
    let store = &globals.store;
    let records = records();
    let elapsed = START.get().map(|t| t.elapsed()).unwrap_or_default();
    let (regions, jit) = {
        let jit = JIT.lock().unwrap().take().unwrap_or_default();
        let mut units: Vec<_> = jit
            .units
            .iter()
            .enumerate()
            .flat_map(|(i, u)| {
                u.ranges
                    .iter()
                    .filter(|(_, l)| *l > 0)
                    .map(move |(s, l)| (*s, *s + *l, i))
            })
            .collect();
        units.sort();
        let vm = CODEGEN.with(|c| c.borrow().prof_vm_regions());
        (Regions { vm, units }, jit)
    };
    let in_jit_memory = |pc: usize| CODEGEN.with(|c| c.borrow().prof_in_jit_memory(pc));
    let mut names = Names {
        store,
        cache: HashMap::default(),
    };
    let mut syms = Symbols::new();

    let _ = writeln!(out);
    let _ = writeln!(
        out,
        "==================== monoruby prof ({:.3} s) ====================",
        elapsed.as_secs_f64()
    );
    let dropped = DROPPED.load(Ordering::Relaxed);
    if dropped != 0 {
        let _ = writeln!(
            out,
            "WARNING: {dropped} samples dropped (sample buffer full; raise MONORUBY_PROF_BUF_MB)"
        );
    }

    //
    // CPU
    //
    if cfg.cpu {
        let cpu: Vec<_> = records.iter().filter(|r| r.kind == KIND_CPU).collect();
        let total = cpu.len() as u64;
        let mut tiers: HashMap<Tier, u64> = HashMap::default();
        let mut self_by: HashMap<u32, [u64; 10]> = HashMap::default();
        let mut total_by: HashMap<u32, u64> = HashMap::default();
        let mut unit_by: HashMap<usize, u64> = HashMap::default();
        let mut native_by: HashMap<String, u64> = HashMap::default();
        let mut folded: HashMap<String, u64> = HashMap::default();
        let mut seen = Vec::new();
        for r in &cpu {
            let pc = r.w1 as usize;
            let tier = if r.ruby.is_empty() && r.phase == Phase::Run as u8 {
                Tier::NoRuby
            } else {
                match r.phase {
                    1 => Tier::Gc,
                    2 => Tier::JitCompile,
                    3 => Tier::Parse,
                    4 => Tier::Malloc,
                    5 => Tier::Prof,
                    _ => {
                        if regions.in_vm(pc) {
                            Tier::Vm
                        } else if let Some(u) = regions.unit_of(pc) {
                            add(&mut unit_by, u, 1);
                            Tier::JitCode
                        } else if in_jit_memory(pc) {
                            Tier::Stub
                        } else {
                            Tier::Native
                        }
                    }
                }
            };
            add(&mut tiers, tier, 1);
            let frames: Vec<u64> = r.ruby.iter().copied().filter(|f| names.valid(*f)).collect();
            let leaf = frames.first().copied().unwrap_or(0) as u32;
            self_by.entry(leaf).or_insert([0; 10])
                [TIERS.iter().position(|t| *t == tier).unwrap()] += 1;
            seen.clear();
            for f in &frames {
                if !seen.contains(f) {
                    seen.push(*f);
                    add(&mut total_by, *f as u32, 1);
                }
            }
            let mut native_leaf = None;
            if !matches!(tier, Tier::JitCode | Tier::Vm | Tier::Stub) && !in_jit_memory(pc) {
                let n = syms.name(pc);
                add(&mut native_by, format!("[{}] {}", tier.short(), n), 1);
                native_leaf = Some(n);
            }
            if cfg.out.is_some() {
                let mut s = String::new();
                for f in frames.iter().rev() {
                    if !s.is_empty() {
                        s.push(';');
                    }
                    s += &sanitize(&names.func(*f));
                }
                if s.is_empty() {
                    s += "<no ruby frame>";
                }
                s += &format!(";[{}]", tier.short());
                if let Some(n) = native_leaf {
                    s += ";";
                    s += &sanitize(&n);
                }
                add(&mut folded, s, 1);
            }
        }
        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "---- CPU: {total} samples, {} clock at {} Hz ----",
            if cfg.wall {
                "main thread's wall"
            } else {
                "process CPU"
            },
            cfg.hz,
        );
        if let Some((wall, cpu)) = clock {
            let span = if cfg.wall { wall } else { cpu };
            let _ = writeln!(
                out,
                "sampled run: {wall:.3} s wall, {cpu:.3} s CPU; one sample = {:.3} ms (effective {:.0} Hz)",
                span * 1e3 / total.max(1) as f64,
                total as f64 / span.max(1e-9),
            );
        }
        let _ = writeln!(out, "where the time went:");
        for t in TIERS {
            let n = tiers.get(&t).copied().unwrap_or(0);
            if n != 0 {
                let _ = writeln!(out, "  {:34} {:>9} {:>6.1}%", t.label(), n, pct(n, total));
            }
        }

        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "methods by self time (top {}): self = the newest Ruby frame; jit/vm/stub/native/gc/... split self",
            cfg.top
        );
        let _ = writeln!(
            out,
            "  {:>7} {:>6} {:>7} {:>6}  {:>5} {:>5} {:>5} {:>6} {:>5} {:>5} {:>5} {:>5} {:>5}  method",
            "self",
            "%",
            "total",
            "%",
            "jit",
            "vm",
            "stub",
            "native",
            "gc",
            "comp",
            "parse",
            "mallc",
            "prof"
        );
        let mut rows: Vec<_> = self_by
            .iter()
            .map(|(f, a)| (*f, *a, a.iter().sum::<u64>()))
            .collect();
        rows.sort_by(|a, b| b.2.cmp(&a.2).then(a.0.cmp(&b.0)));
        for (f, a, s) in rows.iter().take(cfg.top) {
            let t = total_by.get(f).copied().unwrap_or(0);
            let name = if *f == 0 {
                "<no ruby frame>".to_string()
            } else {
                names.func(*f as u64)
            };
            let _ = writeln!(
                out,
                "  {:>7} {:>5.1}% {:>7} {:>5.1}%  {:>5} {:>5} {:>5} {:>6} {:>5} {:>5} {:>5} {:>5} {:>5}  {}",
                s,
                pct(*s, total),
                t,
                pct(t, total),
                a[0],
                a[1],
                a[2],
                a[3],
                a[4],
                a[5],
                a[6],
                a[7],
                a[8],
                name
            );
        }

        let _ = writeln!(out);
        let _ = writeln!(out, "methods by total time (top {}):", cfg.top);
        for (f, t) in sorted(&total_by).into_iter().take(cfg.top) {
            let _ = writeln!(
                out,
                "  {:>7} {:>5.1}%  {}",
                t,
                pct(t, total),
                names.func(f as u64)
            );
        }

        if !unit_by.is_empty() {
            let _ = writeln!(out);
            let _ = writeln!(out, "JIT units by samples in their code (top {}):", cfg.top);
            for (u, n) in sorted(&unit_by).into_iter().take(cfg.top) {
                let unit = &jit.units[u];
                let _ = writeln!(
                    out,
                    "  {:>7} {:>5.1}%  {:>7}B  {} {} <{}>",
                    n,
                    pct(n, total),
                    unit.ranges[0].1 + unit.ranges[1].1,
                    if unit.is_loop { "loop  " } else { "method" },
                    names.func(unit.func.get() as u64),
                    store.debug_class_name(unit.class)
                );
            }
        }

        if !native_by.is_empty() {
            let _ = writeln!(out);
            let _ = writeln!(
                out,
                "native functions by self samples (top {}), [tier] symbol:",
                cfg.top
            );
            for (n, c) in sorted(&native_by).into_iter().take(cfg.top) {
                let _ = writeln!(out, "  {:>7} {:>5.1}%  {}", c, pct(c, total), n);
            }
        }
        if let Some(prefix) = &cfg.out {
            write_folded(&format!("{prefix}.cpu.folded"), &folded);
        }
    }

    //
    // JIT compilation
    //
    if jit.compiles != 0 {
        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "---- JIT: {} compiles ({} loop, {} abandoned), {:.3} ms, {} bytes of code ----",
            jit.compiles,
            jit.loops,
            jit.failed,
            jit.ns as f64 / 1e6,
            jit.code_bytes
        );
        let mut by_time: Vec<&Unit> = jit.units.iter().collect();
        by_time.sort_by(|a, b| b.ns.cmp(&a.ns));
        let _ = writeln!(out, "compiles by time (top {}):", cfg.top);
        for u in by_time.into_iter().take(cfg.top) {
            let _ = writeln!(
                out,
                "  {:>9.3} ms {:>7}B  {} {} <{}>{}",
                u.ns as f64 / 1e6,
                u.ranges[0].1 + u.ranges[1].1,
                if u.is_loop { "loop  " } else { "method" },
                names.func(u.func.get() as u64),
                store.debug_class_name(u.class),
                if u.failed { "  (abandoned)" } else { "" }
            );
        }
        let mut again: HashMap<(u32, u32, bool), (u64, u64)> = HashMap::default();
        for u in &jit.units {
            let e = again
                .entry((u.func.get(), u.class.u32(), u.is_loop))
                .or_default();
            e.0 += 1;
            e.1 += u.ns;
        }
        let mut again: Vec<_> = again.into_iter().filter(|(_, (n, _))| *n > 1).collect();
        if !again.is_empty() {
            again.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
            let _ = writeln!(
                out,
                "compiled more than once (top {}), times / total ms:",
                cfg.top
            );
            for ((f, c, l), (n, ns)) in again.into_iter().take(cfg.top) {
                let _ = writeln!(
                    out,
                    "  {:>5} x {:>9.3} ms  {} {} <{}>",
                    n,
                    ns as f64 / 1e6,
                    if l { "loop  " } else { "method" },
                    names.func(f as u64),
                    ClassId::from(c)
                        .map(|c| store.debug_class_name(c))
                        .unwrap_or_default()
                );
            }
        }
    }

    //
    // Objects and GC
    //
    if cfg.alloc {
        let _ = ALLOC_TABLE.try_with(|t| t.merge());
        let allocs = ALLOCS.lock().unwrap().take().unwrap_or_default();
        let total: u64 = allocs.values().sum();
        let mut by_class: HashMap<u32, u64> = HashMap::default();
        let mut by_func: HashMap<u32, u64> = HashMap::default();
        for ((f, c), n) in &allocs {
            add(&mut by_class, *c, *n);
            add(&mut by_func, *f, *n);
        }
        let _ = writeln!(out);
        let _ = writeln!(out, "---- objects: {total} allocated ----");
        let _ = writeln!(out, "by class (top {}):", cfg.top);
        for (c, n) in sorted(&by_class).into_iter().take(cfg.top) {
            let name = ClassId::from(c)
                .map(|c| store.debug_class_name(c))
                .unwrap_or_else(|| "<none>".to_string());
            let _ = writeln!(out, "  {:>10} {:>5.1}%  {}", n, pct(n, total), name);
        }
        let _ = writeln!(out, "by allocating method (top {}):", cfg.top);
        for (f, n) in sorted(&by_func).into_iter().take(cfg.top) {
            let name = if names.valid(f as u64) {
                names.func(f as u64)
            } else {
                "<no ruby frame>".to_string()
            };
            let _ = writeln!(out, "  {:>10} {:>5.1}%  {}", n, pct(n, total), name);
        }
        let _ = writeln!(out, "by method and class (top {}):", cfg.top);
        for ((f, c), n) in sorted(&allocs).into_iter().take(cfg.top) {
            let name = if names.valid(f as u64) {
                names.func(f as u64)
            } else {
                "<no ruby frame>".to_string()
            };
            let cname = ClassId::from(c)
                .map(|c| store.debug_class_name(c))
                .unwrap_or_else(|| "<none>".to_string());
            let _ = writeln!(
                out,
                "  {:>10} {:>5.1}%  {:30} {}",
                n,
                pct(n, total),
                cname,
                name
            );
        }
        if let Some(prefix) = &cfg.out {
            let mut folded: HashMap<String, u64> = HashMap::default();
            for r in records.iter().filter(|r| r.kind == KIND_ALLOC) {
                let mut s = String::new();
                let frames: Vec<u64> = r.ruby.iter().copied().filter(|f| names.valid(*f)).collect();
                for f in frames.iter().rev() {
                    if !s.is_empty() {
                        s.push(';');
                    }
                    s += &sanitize(&names.func(*f));
                }
                if s.is_empty() {
                    s += "<no ruby frame>";
                }
                let cname = ClassId::from(r.w1 as u32)
                    .map(|c| store.debug_class_name(c))
                    .unwrap_or_else(|| "<none>".to_string());
                s += &format!(";[{}]", sanitize(&cname));
                add(&mut folded, s, cfg.alloc_every);
            }
            write_folded(&format!("{prefix}.alloc.folded"), &folded);
        }
    }
    {
        let gc = *GC_STATS.lock().unwrap();
        if gc[0].count + gc[1].count != 0 {
            let _ = writeln!(out);
            let _ = writeln!(
                out,
                "---- GC: {} collections, {:.3} ms ----",
                gc[0].count + gc[1].count,
                (gc[0].ns + gc[1].ns) as f64 / 1e6
            );
            for (label, s) in [("minor", gc[0]), ("major", gc[1])] {
                if s.count == 0 {
                    continue;
                }
                let _ = writeln!(
                    out,
                    "  {label}: {:>6} x  {:>10.3} ms (mark {:.3} ms, sweep {:.3} ms)  {:.1} us/GC  {:.0} marked/GC",
                    s.count,
                    s.ns as f64 / 1e6,
                    s.mark_ns as f64 / 1e6,
                    (s.ns - s.mark_ns.min(s.ns)) as f64 / 1e6,
                    s.ns as f64 / 1e3 / s.count as f64,
                    s.marked as f64 / s.count as f64
                );
            }
            alloc::ALLOC.with(|a| {
                let a = a.borrow();
                let _ = writeln!(
                    out,
                    "  heap: {} pages, {} live objects after the last GC, {} objects allocated in all",
                    a.pages_len(),
                    a.live_count(),
                    a.total_allocated()
                );
            });
        }
    }

    //
    // malloc
    //
    if cfg.malloc {
        let (mc, mb, fc, fb) = (
            MALLOC_CALLS.load(Ordering::Relaxed),
            MALLOC_BYTES.load(Ordering::Relaxed),
            FREE_CALLS.load(Ordering::Relaxed),
            FREE_BYTES.load(Ordering::Relaxed),
        );
        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "---- malloc: {mc} allocations ({mb} bytes), {fc} frees ({fb} bytes); stacks of 1 in {} ----",
            cfg.malloc_every
        );
        let samples: Vec<_> = records.iter().filter(|r| r.kind == KIND_MALLOC).collect();
        let mut by_func: HashMap<u32, (u64, u64)> = HashMap::default();
        let mut by_site: HashMap<String, (u64, u64)> = HashMap::default();
        let mut folded: HashMap<String, u64> = HashMap::default();
        for r in &samples {
            let leaf = r
                .ruby
                .iter()
                .copied()
                .find(|f| names.valid(*f))
                .unwrap_or(0) as u32;
            let e = by_func.entry(leaf).or_default();
            e.0 += cfg.malloc_every;
            e.1 += r.w1 * cfg.malloc_every;
            let site = r
                .native
                .iter()
                .map(|a| syms.caller(*a as usize))
                .find(|n| !is_alloc_plumbing(n))
                .unwrap_or_else(|| "<unknown>".to_string());
            let e = by_site.entry(site.clone()).or_default();
            e.0 += cfg.malloc_every;
            e.1 += r.w1 * cfg.malloc_every;
            if cfg.out.is_some() {
                let mut s = String::new();
                let frames: Vec<u64> = r.ruby.iter().copied().filter(|f| names.valid(*f)).collect();
                for f in frames.iter().rev() {
                    if !s.is_empty() {
                        s.push(';');
                    }
                    s += &sanitize(&names.func(*f));
                }
                if s.is_empty() {
                    s += "<no ruby frame>";
                }
                s += ";";
                s += &sanitize(&site);
                add(&mut folded, s, cfg.malloc_every);
            }
        }
        let est: u64 = samples.len() as u64 * cfg.malloc_every;
        let print = |out: &mut String, title: &str, rows: Vec<(String, (u64, u64))>| {
            let _ = writeln!(out, "{title} (top {}), estimated calls / bytes:", cfg.top);
            for (k, (c, b)) in rows.into_iter().take(cfg.top) {
                let _ = writeln!(out, "  {:>10} {:>5.1}% {:>12}  {}", c, pct(c, est), b, k);
            }
        };
        let mut rows: Vec<_> = by_func
            .iter()
            .map(|(f, v)| {
                (
                    if names.valid(*f as u64) {
                        names.func(*f as u64)
                    } else {
                        "<no ruby frame>".to_string()
                    },
                    *v,
                )
            })
            .collect();
        rows.sort_by(|a, b| b.1.0.cmp(&a.1.0).then(a.0.cmp(&b.0)));
        print(out, "by Ruby method", rows);
        let mut rows: Vec<_> = by_site.into_iter().collect();
        rows.sort_by(|a, b| b.1.0.cmp(&a.1.0).then(a.0.cmp(&b.0)));
        print(out, "by Rust call site", rows);
        if let Some(prefix) = &cfg.out {
            write_folded(&format!("{prefix}.malloc.folded"), &folded);
        }
    }
    let _ = writeln!(
        out,
        "=================================================================="
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(target_os = "linux")]
    fn executable_symbols_name_local_functions() {
        let syms = Symbols::new();
        assert!(!syms.exe.is_empty(), "no .symtab read from /proc/self/exe");
        let name = syms
            .exe_name(elf_functions as *const () as usize)
            .expect("own function not in the executable's symbols");
        assert!(name.contains("prof::elf_functions"), "{name}");
    }

    #[test]
    fn records_round_trip() {
        init_test_buffer();
        let before = records().len();
        buf_push(
            KIND_MALLOC,
            Phase::Malloc as u8,
            48,
            &[3, 2, 1],
            &[0x1000, 0x2000],
        );
        buf_push(KIND_CPU, Phase::Run as u8, 0xdead, &[], &[]);
        let r = records();
        assert_eq!(before + 2, r.len());
        let a = &r[before];
        assert_eq!(
            (KIND_MALLOC, Phase::Malloc as u8, 48),
            (a.kind, a.phase, a.w1)
        );
        assert_eq!(&[3, 2, 1], a.ruby);
        assert_eq!(&[0x1000, 0x2000], a.native);
        let b = &r[before + 1];
        assert_eq!(
            (KIND_CPU, 0xdead, 0, 0),
            (b.kind, b.w1, b.ruby.len(), b.native.len())
        );
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn reader_refuses_unmapped_memory() {
        let x = 0x1234_5678u64;
        let mut r = Reader::new(0);
        assert_eq!(Some(0x1234_5678), r.read(&x as *const u64 as usize));
        // Page-aligned, far from anything mapped in user space.
        assert_eq!(None, r.read(0x7fff_0000_0000_0000 >> 8));
        assert_eq!(None, r.read(8));
        assert_eq!(None, r.read(&x as *const u64 as usize + 1));
    }

    #[test]
    fn allocator_plumbing_is_skipped() {
        assert!(is_alloc_plumbing("alloc::raw_vec::finish_grow"));
        assert!(is_alloc_plumbing(
            "<hashbrown::raw::RawTable<u8>>::reserve_rehash"
        ));
        assert!(is_alloc_plumbing(
            "<monoruby::alloc::RurubyAlloc as core::alloc::global::GlobalAlloc>::alloc"
        ));
        assert!(!is_alloc_plumbing("<[u8]>::repeat"));
        assert!(!is_alloc_plumbing("monoruby::bytecodegen::compile_iseq"));
    }

    fn init_test_buffer() {
        static ONCE: std::sync::Once = std::sync::Once::new();
        ONCE.call_once(|| {
            if BUF.load(Ordering::Acquire).is_null() {
                buf_init(1 << 16);
            }
        });
    }
}
