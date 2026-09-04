use crate::RValue;
use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct RurubyAlloc;

unsafe impl GlobalAlloc for RurubyAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // Optional hard cap (MONORUBY_MALLOC_HARD_LIMIT): abort with a
        // report instead of letting a runaway allocation OOM-kill the
        // machine. Checked against layout.size() itself too, so even a
        // single multi-GB request (which a polling watchdog can never
        // catch in time) trips it. Costs one relaxed load when disabled.
        let limit = malloc_hard_limit();
        if limit != 0 && !MALLOC_ABORTING.load(Ordering::Relaxed) {
            let projected = MALLOC_AMOUNT.load(Ordering::Relaxed) + layout.size();
            if projected > limit {
                malloc_hard_limit_abort(layout.size(), projected, limit);
            }
        }
        // Only object-scale buffers count toward the malloc-driven GC trigger;
        // huge one-shot reservations are skipped (see `MALLOC_TRACK_LIMIT`).
        // The same size test gates `dealloc`, so accounting stays symmetric
        // and `MALLOC_AMOUNT` never underflows.
        if layout.size() < MALLOC_TRACK_LIMIT {
            let total = MALLOC_AMOUNT.fetch_add(layout.size(), Ordering::SeqCst) + layout.size();
            request_gc_if_malloc_over(total);
        }
        #[cfg(feature = "gc-log")]
        malloc_stats::record_alloc(layout.size());
        #[cfg(feature = "mimalloc")]
        unsafe {
            mimalloc::MiMalloc.alloc(layout)
        }
        #[cfg(not(feature = "mimalloc"))]
        unsafe {
            System.alloc(layout)
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if layout.size() < MALLOC_TRACK_LIMIT {
            MALLOC_AMOUNT.fetch_sub(layout.size(), Ordering::SeqCst);
        }
        #[cfg(feature = "gc-log")]
        malloc_stats::record_dealloc(layout.size());
        #[cfg(feature = "mimalloc")]
        unsafe {
            mimalloc::MiMalloc.dealloc(ptr, layout)
        }
        #[cfg(not(feature = "mimalloc"))]
        unsafe {
            System.dealloc(ptr, layout)
        }
    }
}

#[global_allocator]
pub static GLOBAL_ALLOC: RurubyAlloc = RurubyAlloc;

/// Size-class histogram of everything that passes through the global
/// allocator (`gc-log` builds only). This is the measurement the payload-
/// locality work keys off: which buffer sizes churn, and how much of the
/// traffic a small-size pool (or a wider inline representation) would
/// absorb. Counter tier — two relaxed atomic adds per alloc/dealloc,
/// printed once at exit next to the GC profile.
#[cfg(feature = "gc-log")]
pub(crate) mod malloc_stats {
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Bucket i covers sizes up to `16 << i`; the last bucket is open-ended.
    pub(crate) const BUCKETS: usize = 13; // 16B .. 32KB, then >32KB
    static ALLOC_COUNT: [AtomicUsize; BUCKETS] =
        [const { AtomicUsize::new(0) }; BUCKETS];
    static ALLOC_BYTES: [AtomicUsize; BUCKETS] =
        [const { AtomicUsize::new(0) }; BUCKETS];
    static DEALLOC_COUNT: [AtomicUsize; BUCKETS] =
        [const { AtomicUsize::new(0) }; BUCKETS];

    fn bucket(size: usize) -> usize {
        // 0..=16 -> 0, 17..=32 -> 1, ... 16KB+1..=32KB -> 11, larger -> 12
        let b = (usize::BITS - size.max(1).saturating_sub(1).leading_zeros()) as usize;
        b.saturating_sub(4).min(BUCKETS - 1)
    }

    pub(crate) fn record_alloc(size: usize) {
        let b = bucket(size);
        ALLOC_COUNT[b].fetch_add(1, Ordering::Relaxed);
        ALLOC_BYTES[b].fetch_add(size, Ordering::Relaxed);
        // Temporary: MONORUBY_ALLOC_TRACE=<bucket> samples call sites for a
        // size class, to attribute the histogram. Symbolizing here is not an
        // option — `Backtrace::force_capture` allocates (re-entering this
        // function) and symbolizing this binary takes long enough to look
        // like a hang. Instead walk the frame-pointer chain, which
        // `-Cforce-frame-pointers=yes` guarantees, record raw return
        // addresses, and resolve them with addr2line after the run.
        if let Some(t) = trace_bucket()
            && t == b
        {
            static N: AtomicUsize = AtomicUsize::new(0);
            if N.fetch_add(1, Ordering::Relaxed) % 20_000 == 0 {
                record_frames();
            }
        }
    }

    /// Ring of sampled call chains: `TRACE_DEPTH` return addresses each.
    const TRACE_DEPTH: usize = 6;
    const TRACE_SLOTS: usize = 64;
    static TRACE_BUF: [[AtomicUsize; TRACE_DEPTH]; TRACE_SLOTS] =
        [const { [const { AtomicUsize::new(0) }; TRACE_DEPTH] }; TRACE_SLOTS];
    static TRACE_USED: AtomicUsize = AtomicUsize::new(0);

    #[cfg(target_arch = "x86_64")]
    fn record_frames() {
        let slot = TRACE_USED.fetch_add(1, Ordering::Relaxed);
        if slot >= TRACE_SLOTS {
            return;
        }
        let mut rbp: usize;
        // SAFETY: reads the frame-pointer register; the walk below only
        // dereferences it while it stays plausible (non-null, aligned,
        // monotonically increasing), so a frameless callee truncates the
        // chain instead of faulting.
        unsafe { std::arch::asm!("mov {}, rbp", out(reg) rbp, options(nomem, nostack)) };
        for d in 0..TRACE_DEPTH {
            if rbp == 0 || rbp % 8 != 0 {
                break;
            }
            // SAFETY: `rbp` points at a saved-rbp/return-address pair in a
            // live frame of this thread's stack.
            let (next, ret) = unsafe { (*(rbp as *const usize), *((rbp + 8) as *const usize)) };
            if next <= rbp {
                break;
            }
            TRACE_BUF[slot][d].store(ret, Ordering::Relaxed);
            rbp = next;
        }
    }

    #[cfg(not(target_arch = "x86_64"))]
    fn record_frames() {}

    fn dump_traces() {
        let used = TRACE_USED.load(Ordering::Relaxed).min(TRACE_SLOTS);
        if used == 0 {
            return;
        }
        // The load base turns runtime addresses into file offsets for
        // addr2line (the binary is position-independent).
        let base = std::fs::read_to_string("/proc/self/maps")
            .ok()
            .and_then(|m| m.lines().next().map(|l| l.split('-').next().unwrap_or("0").to_string()))
            .unwrap_or_else(|| "0".to_string());
        eprintln!("alloc-trace: {used} samples, load base 0x{base}");
        for slot in 0..used {
            let chain: Vec<String> = (0..TRACE_DEPTH)
                .map(|d| format!("{:#x}", TRACE_BUF[slot][d].load(Ordering::Relaxed)))
                .collect();
            eprintln!("  {}", chain.join(" "));
        }
    }

    /// Read `MONORUBY_ALLOC_TRACE` without allocating.
    ///
    /// This runs inside the global allocator, so it must not allocate:
    /// `std::env::var` returns a `String`, and reaching it through a
    /// `OnceLock` re-entered the initializer from the very allocation it
    /// was making and deadlocked the process on its first allocation.
    /// `getenv` hands back a borrowed C string instead.
    fn trace_bucket() -> Option<usize> {
        // 0 = not read yet, 1 = read, no bucket, 2.. = bucket + 2.
        static B: AtomicUsize = AtomicUsize::new(0);
        let cached = B.load(Ordering::Relaxed);
        if cached != 0 {
            return (cached >= 2).then(|| cached - 2);
        }
        // SAFETY: `getenv` returns a pointer into the environment block,
        // valid until the environment is mutated; it is only read here.
        let v = unsafe {
            let p = libc::getenv(c"MONORUBY_ALLOC_TRACE".as_ptr());
            if p.is_null() {
                None
            } else {
                std::ffi::CStr::from_ptr(p).to_str().ok().and_then(|s| s.parse::<usize>().ok())
            }
        };
        B.store(v.map_or(1, |b| b + 2), Ordering::Relaxed);
        v
    }

    pub(crate) fn record_dealloc(size: usize) {
        DEALLOC_COUNT[bucket(size)].fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn dump() {
        dump_traces();
        eprintln!("global-allocator size classes (alloc / dealloc / alloc-bytes):");
        let mut label = 16usize;
        for i in 0..BUCKETS {
            let (a, d, by) = (
                ALLOC_COUNT[i].load(Ordering::Relaxed),
                DEALLOC_COUNT[i].load(Ordering::Relaxed),
                ALLOC_BYTES[i].load(Ordering::Relaxed),
            );
            if a == 0 && d == 0 {
                label <<= 1;
                continue;
            }
            if i == BUCKETS - 1 {
                eprintln!("  >{:>6}B: {:>12} / {:>12} / {:>14}", label >> 1, a, d, by);
            } else {
                eprintln!("  {:>7}B: {:>12} / {:>12} / {:>14}", label, a, d, by);
            }
            label <<= 1;
        }
    }
}

/// Allocations of at least this size are excluded from `MALLOC_AMOUNT` (and
/// from the GC trigger). They are one-shot infrastructure reservations, not
/// Ruby-object churn the GC could reclaim — chiefly monoasm's JIT memory,
/// which reserves three 256 MB pages (768 MB of mostly-untouched virtual
/// address space) at startup. Counting that reservation would peg the malloc
/// threshold near a gigabyte, so real String/Array/Hash growth would never
/// trip a collection. No legitimate single Ruby object buffer approaches this
/// size, and the cap gates `dealloc` identically, so a buffer is always
/// tracked or skipped consistently and `MALLOC_AMOUNT` cannot underflow.
const MALLOC_TRACK_LIMIT: usize = 64 * 1024 * 1024;

/// Net live bytes handed out through the global allocator for object-scale
/// buffers (those under `MALLOC_TRACK_LIMIT`). Tracked so the GC can be
/// triggered by external-buffer growth (large Strings/Arrays/Hashes), not
/// only by GC-arena (`RValue`) pressure — a `String#<<` loop allocates almost
/// no `RValue`s yet can balloon malloc memory unboundedly.
pub static MALLOC_AMOUNT: AtomicUsize = AtomicUsize::new(0);

/// Optional hard ceiling on malloc'd memory, set via the
/// `MONORUBY_MALLOC_HARD_LIMIT` environment variable (bytes, with an
/// optional K/M/G suffix, e.g. `3G`; unset/unparsable = disabled, returns
/// 0). When an allocation would push the live tracked total past the
/// limit, the process prints the requested size plus a backtrace and
/// aborts — converting a runaway allocation into a *diagnosable, named*
/// crash before the OS kills the whole machine. Motivated by the darwin
/// CI runner (7 GB RAM), whose OOM death destroys every log and artifact:
/// a polling memory watchdog can never catch a single multi-GB request in
/// time, but this check sees each allocation synchronously. The GC
/// arena's up-front reservation and monoasm's JIT pages call
/// `System.alloc` directly (or exceed `MALLOC_TRACK_LIMIT`), mirroring
/// what `MALLOC_AMOUNT` tracks, so only real buffer growth counts —
/// though a single oversized request trips the cap regardless via
/// `layout.size()`.
fn malloc_hard_limit() -> usize {
    // usize::MAX = "not initialized yet". Initialization reads the
    // environment, which itself allocates; IN_INIT makes those nested
    // allocations skip the cap instead of recursing unboundedly (and
    // OnceLock::get_or_init would deadlock on such reentrancy).
    static LIMIT: AtomicUsize = AtomicUsize::new(usize::MAX);
    static IN_INIT: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    let v = LIMIT.load(Ordering::Relaxed);
    if v != usize::MAX {
        return v;
    }
    if IN_INIT.swap(true, Ordering::Relaxed) {
        // Nested allocation during init (or a racing thread): treat as
        // unlimited for this one call; LIMIT is stored momentarily.
        return 0;
    }
    let parsed = std::env::var("MONORUBY_MALLOC_HARD_LIMIT")
        .ok()
        .and_then(|s| parse_byte_size(s.trim()))
        .unwrap_or(0);
    LIMIT.store(parsed, Ordering::Relaxed);
    parsed
}

/// `"123"`, `"512K"`, `"64M"`, `"3G"` → bytes. Anything else → None.
fn parse_byte_size(s: &str) -> Option<usize> {
    let (num, mult) = match s.as_bytes().last()? {
        b'k' | b'K' => (&s[..s.len() - 1], 1usize << 10),
        b'm' | b'M' => (&s[..s.len() - 1], 1 << 20),
        b'g' | b'G' => (&s[..s.len() - 1], 1 << 30),
        _ => (s, 1),
    };
    num.parse::<usize>().ok()?.checked_mul(mult)
}

#[cfg(test)]
mod malloc_limit_tests {
    use super::parse_byte_size;

    #[test]
    fn parses_plain_and_suffixed_sizes() {
        assert_eq!(parse_byte_size("123"), Some(123));
        assert_eq!(parse_byte_size("512K"), Some(512 << 10));
        assert_eq!(parse_byte_size("512k"), Some(512 << 10));
        assert_eq!(parse_byte_size("64M"), Some(64 << 20));
        assert_eq!(parse_byte_size("64m"), Some(64 << 20));
        assert_eq!(parse_byte_size("3G"), Some(3usize << 30));
        assert_eq!(parse_byte_size("3g"), Some(3usize << 30));
    }

    #[test]
    fn rejects_garbage_and_overflow() {
        assert_eq!(parse_byte_size(""), None);
        assert_eq!(parse_byte_size("abc"), None);
        assert_eq!(parse_byte_size("G"), None);
        assert_eq!(parse_byte_size("12x"), None);
        // numeric part parses, multiplication overflows
        assert_eq!(parse_byte_size("99999999999999999G"), None);
    }
}

/// Set for the duration of the hard-limit crash report, so the report's
/// own allocations (eprintln formatting, backtrace capture) bypass the
/// cap instead of re-tripping it and aborting mid-report.
static MALLOC_ABORTING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

#[cold]
#[coverage(off)] // crash handler: aborts the process, uncoverable in-test
fn malloc_hard_limit_abort(size: usize, projected: usize, limit: usize) -> ! {
    MALLOC_ABORTING.store(true, Ordering::SeqCst);
    eprintln!(
        "monoruby: MONORUBY_MALLOC_HARD_LIMIT exceeded: \
         requested {size} B (live tracked total would be {projected} B, limit {limit} B) — aborting"
    );
    eprintln!("{}", std::backtrace::Backtrace::force_capture());
    std::process::abort();
}

/// `MALLOC_AMOUNT` ceiling above which a GC is requested. Recomputed after
/// each GC from the post-collection live amount (see `Allocator::gc`).
static MALLOC_GC_THRESHOLD: AtomicUsize = AtomicUsize::new(MALLOC_THRESHOLD);

/// Mirror of `Allocator::gc_enabled`, readable from the global allocator
/// without touching the (non-reentrant) thread-local `Allocator`. When GC is
/// disabled (`--no-gc`), `gc()` voids the request instead of collecting;
/// skipping the request here just avoids even that no-op poll.
static GC_ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

/// Keep `GC_ENABLED` in step with `Allocator::gc_enabled` (see `Globals::gc_enable`).
pub(crate) fn set_gc_enabled(enabled: bool) {
    GC_ENABLED.store(enabled, Ordering::Relaxed);
}

thread_local! {
    /// Set by `GC.start` so the next safepoint collection is a Major
    /// (full) one — running a collection inline from a builtin is
    /// unsafe, so `GC.start` asks for one at the next poll via
    /// [`request_gc`]. Thread-local like the allocator itself: each
    /// test thread runs its own interpreter, and a process-global flag
    /// let one thread's `GC.start` turn another thread's next
    /// collection into a Major (observable through
    /// `GC.stat(:major_gc_count)` — deterministic under `gc-stress`).
    static GC_FORCE_MAJOR: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Live bytes in tracked `malloc` buffers (`GC.stat`'s
/// `malloc_increase_bytes`).
pub(crate) fn malloc_amount() -> usize {
    MALLOC_AMOUNT.load(Ordering::Relaxed)
}

/// The `malloc_amount` ceiling that triggers the next collection
/// (`GC.stat`'s `malloc_increase_bytes_limit`).
pub(crate) fn malloc_gc_threshold() -> usize {
    MALLOC_GC_THRESHOLD.load(Ordering::Relaxed)
}

/// Request a garbage collection at the next VM safepoint (`GC.start`).
///
/// Only arms the poll word's GC lane (and, when `force_major`, records
/// that the pending collection must be Major); the actual collection runs
/// from the safepoint poll where live registers are spilled, so this is
/// safe to call from inside a builtin.
pub(crate) fn request_gc(force_major: bool) {
    if force_major {
        GC_FORCE_MAJOR.with(|f| f.set(true));
    }
    crate::poll_flag::set_gc();
}

/// Request a GC at the next VM safepoint when live malloc has crossed the
/// threshold. Cheap and allocation-free (just a flag store), so it is safe
/// to call from inside the global allocator.
///
/// The requested collection is left minor-eligible (it is not forced to be a
/// major): the buffers behind transient String/Array growth die young, so a
/// minor GC reclaims them, while genuinely old-generation garbage is still
/// caught by the existing major triggers (old-gen growth and
/// `MAX_MINORS_PER_MAJOR`). Forcing a major on every malloc trigger instead
/// made full-heap marking dominate any workload with a large stable old
/// generation, for no memory benefit over minor collection here.
#[inline]
fn request_gc_if_malloc_over(total: usize) {
    if total < MALLOC_GC_THRESHOLD.load(Ordering::Relaxed) {
        return;
    }
    if !GC_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    // Idempotent lane set: staying over threshold across many mallocs
    // re-arms the same request, never anything unbounded. Racing the async
    // signal handler is harmless too — the lanes are separate bytes.
    crate::poll_flag::set_gc();
}

thread_local!(
    pub static ALLOC: RefCell<Allocator<RValue>> = RefCell::new(Allocator::new());
);

const SIZE: usize = 64;
const GCBOX_SIZE: usize = std::mem::size_of::<RValue>();
const PAGE_LEN: usize = 64 * SIZE;
const DATA_LEN: usize = 64 * (SIZE - 1);
const THRESHOLD: usize = 64 * (SIZE - 2);

/// Floor for the allocation budget between collections: pages filling to
/// `THRESHOLD` before the arena requests a collection (32 pages ≈ 127K
/// `RValue`s, 8 MB of cells). Small heaps use exactly this; see
/// [`GC_HEAP_FRACTION`].
///
/// Was 8 (the old `>= 8` poll band). Every minor collection re-scans the
/// whole root set — every ISeq's literals, every class — so on a program
/// that has loaded a Rails-sized amount of code the fixed cost per
/// collection dwarfs the young generation it reclaims: erubi ran 7.3
/// collections per iteration, activerecord 3.8, on a 6 MB heap. Raising
/// the floor to 32 measured graphql -8%, activerecord -6.5%, rack -2%
/// (yjit-bench, x86-64, single runs on a machine that drifts ±10%; erubi
/// did not move in a 3-round rerun) for +3..10 MB RSS, still below
/// CRuby's on the same programs
/// (see doc/yjit_bench_slow_investigation_2026-09.md §8).
const PAGES_PER_GC_TRIGGER: u32 = 32;

/// Above `PAGES_PER_GC_TRIGGER * GC_HEAP_FRACTION` pages in service, the
/// budget instead scales with the heap: a collection is requested once
/// `1/GC_HEAP_FRACTION` of the pages in service have filled since the last
/// one.
///
/// What a collection costs is set by how much is *live* — the root scan,
/// the remembered-set scan and the bitmap walk are all proportional to it
/// — while a fixed budget amortises that over a constant amount of
/// allocation, so a program whose live set keeps growing pays
/// O(live × total allocation). Tying the budget to the heap keeps the
/// ratio of collector work to mutator work bounded instead.
///
/// The cost is floating garbage: up to a `1/GC_HEAP_FRACTION` of the heap
/// is retained longer than it needs to be, which is what the constant
/// trades. 1/16 measured as the knee on a large-heap workload (plb2
/// bedcov: 202 → 83 minor GCs, GC time −32%, RSS +23% and still below
/// CRuby's on the same program); a smaller divisor buys less and less time
/// for steeply more memory.
const GC_HEAP_FRACTION: u32 = 16;
const ALLOC_SIZE: usize = PAGE_LEN * GCBOX_SIZE; // 2^18 = 256kb
const MALLOC_THRESHOLD: usize = 256 * 1024;
const MAX_PAGES: usize = 8192;

/// Byte offset of the cell array inside a page, and the cell stride, so the
/// JIT's inline bump fast path can compute a cell address as
/// `current_page + PAGE_DATA_OFFSET + used_in_current * CELL_SIZE` — the
/// same address `Page::get_cell` returns.
pub(crate) const PAGE_DATA_OFFSET: usize = std::mem::offset_of!(Page<RValue>, data);
pub(crate) const CELL_SIZE: usize = GCBOX_SIZE;
/// The inline path scales the bump index with a shift.
const _: () = assert!(CELL_SIZE.is_power_of_two());
pub(crate) const CELL_SIZE_SHIFT: u32 = CELL_SIZE.trailing_zeros();

/// Bump index at which the inline fast path must hand back to
/// `Allocator::alloc`: at `THRESHOLD` the runtime sets the GC alloc flag and
/// at `DATA_LEN` it starts a new page, neither of which is worth inlining
/// (together they are 64 of every 4032 allocations). Anything below this is
/// a plain bump.
pub(crate) const BUMP_INLINE_LIMIT: usize = THRESHOLD;

/// Hard cap on the number of minor (young-generation) collections between
/// two major (full-heap) collections. This is only a safety bound — to
/// rebuild the remembered set and bound floating old garbage even if the
/// adaptive trigger (`old_major_threshold`) never fires. The usual major
/// trigger is old-generation growth; see `decide_gc_kind`.
const MAX_MINORS_PER_MAJOR: usize = 64;

/// Adaptive major-GC trigger: a major GC is forced once the old generation
/// has grown to this multiple of its size right after the previous major.
/// Mirrors CRuby's `old_objects_limit` (RGENGC_OLD_OBJECT_LIMIT_FACTOR).
/// A stable old generation (e.g. a long-lived data structure) then majors
/// rarely — preserving the generational win — while a workload that keeps
/// promoting short-lived "floating" garbage majors often, reclaiming it
/// and keeping RSS down. See `doc/gc.md`.
const OLD_GROWTH_FACTOR: usize = 2;

/// Floor for the adaptive trigger: never force a major purely on old-gen
/// growth until the old generation reaches this many objects (~a handful
/// of pages). Below this, full-heap marking is cheap, so majoring eagerly
/// would only add overhead.
const OLD_OBJECT_FLOOR: usize = 16384;

/// How many salvaged (all-dead) pages stay resident for reuse before the
/// rest are handed back to the OS: `1/FREE_PAGE_RESERVE_FRACTION` of the
/// pages in service, never fewer than `FREE_PAGE_RESERVE_MIN`, and never
/// less than one collection's allocation budget (`gc_trigger_pages`).
///
/// A page that empties out used to sit on `free_pages` forever, so a
/// workload's *peak* heap stayed resident: lee's collector needed 22
/// pages for its live set while the arena kept ~115 touched (30 MB RSS
/// for 5.5 MB of objects). The reserve has to cover the growth between
/// two collections, otherwise the mutator re-faults the pages it just
/// released: what it allocates between collections is exactly
/// `gc_trigger_pages`, so that is the floor. The fraction alone was only
/// equal to it while the budget was `1/GC_HEAP_FRACTION` of the pages in
/// service — once `PAGES_PER_GC_TRIGGER` became the binding term (a heap
/// whose live set is small next to its allocation rate), the reserve fell
/// to `FREE_PAGE_RESERVE_MIN` while the budget stayed at 32 pages, and
/// every cycle handed back ~30 pages only to fault them in again. On a
/// 20M-`Object.new` loop that was 282K page faults and 794 ms; with the
/// budget as the floor, 5.4K faults and 456 ms.
///
/// The floor is bounded by the budget, not by the peak heap, so the
/// original problem does not come back: a program keeps at most one
/// collection's worth of pages resident (8 MB at
/// `PAGES_PER_GC_TRIGGER`), and only if it actually salvaged that many.
const FREE_PAGE_RESERVE_FRACTION: usize = 8;
const FREE_PAGE_RESERVE_MIN: usize = 2;

/// How deep the mark phase may walk the object graph with plain
/// recursion before it starts deferring children to the mark queue
/// (`Allocator::scan_children`).
///
/// The traversal used to be *purely* recursive, so its stack cost was the
/// depth of the object graph: 75K links of `a = [a]` (or a linked list,
/// or an ivar chain) overflowed the 8MB main stack and aborted the
/// process inside GC. Above this limit the walk switches to a
/// breadth-first queue on the heap, which bounds the native stack at
/// `MARK_RECURSION_LIMIT` nested `mark_children` frames — a few KB — no
/// matter how deep the graph is. That headroom matters because a
/// collection starts at a safepoint, on top of whatever JIT-compiled Ruby
/// frames are already on the stack.
///
/// Keeping a *small* recursive prefix rather than queueing from the very
/// first object is what makes this free: real graphs are shallow and wide,
/// and a queue entry per marked object costs a push + a pop + 8 bytes of
/// memory traffic. Measured on plb2 bedcov (2.7M live objects, the most
/// mark-heavy workload here, 89 collections), mean of 13 interleaved
/// runs: 3205 ms for the old unbounded recursion, 3225 ms (+0.6%) with
/// this prefix, 3479 ms (+8.6%) queueing everything. 32 and 256 measured
/// the same, so this takes the tighter stack bound.
///
/// Setting it to 0 makes the traversal a pure breadth-first walk.
const MARK_RECURSION_LIMIT: u32 = 32;

/// Number of collections an object must survive before it is promoted to
/// the old generation. Aging (rather than promoting on first survival)
/// avoids promoting short-lived objects that merely happened to be live
/// at a collection, which would otherwise accumulate as floating garbage
/// in the old generation until the next major GC. See
/// `doc/gc.md`.
pub(crate) const RGENGC_OLD_AGE: u8 = 3;

pub trait GC<T: GCBox> {
    fn mark(&self, alloc: &mut Allocator<T>);
}

pub trait GCRoot<T: GCBox>: GC<T> {
    #[cfg(feature = "gc-debug")]
    fn startup_flag(&self) -> bool;
}

pub trait GCBox: PartialEq {
    fn free(&mut self);

    fn next(&self) -> Option<std::ptr::NonNull<Self>>;

    fn set_next_none(&mut self);

    fn set_next(&mut self, next: *mut Self);

    fn new_invalid() -> Self;

    ///
    /// Mark the objects directly referenced by `self` (its children),
    /// *without* marking `self` itself. Used to scan remembered-set
    /// entries during a minor GC, where `self` is an old object that is
    /// already (seed-)marked but whose young children must still be
    /// reached. See `doc/gc.md`.
    ///
    fn mark_children(&self, alloc: &mut Allocator<Self>)
    where
        Self: Sized;

    ///
    /// Whether this object may be promoted to the old generation when it
    /// survives a collection. Only objects that are provably safe to
    /// skip in a minor GC should return `true` — currently those with no
    /// outgoing references at promotion time. See
    /// `doc/gc.md`.
    ///
    fn is_promotable(&self) -> bool;

    ///
    /// Set this object's old-generation header flag. Called only after
    /// the mark phase (never while a `&self` from marking is live), so
    /// the `&mut` is sound. See `doc/gc.md`.
    ///
    fn promote_to_old(&mut self);

    ///
    /// Increment this object's survival age and return whether it has now
    /// reached `RGENGC_OLD_AGE` and should be promoted. Called from the
    /// post-mark aging pass (no `&self` from marking is live).
    ///
    fn age_and_check_promote(&mut self) -> bool;

    ///
    /// Mark this object as old-but-not-remembered ("barrier armed"): a
    /// young store to it must take the barrier slow path. Used at
    /// promotion (no young children) and when dropping it from the
    /// remembered set.
    ///
    fn arm_barrier(&mut self);

    ///
    /// Mark this object as recorded in the remembered set (clears the
    /// WB_ARMED flag; an old object with WB_ARMED clear *is*
    /// "remembered" — there is no separate bit). Used when it is added
    /// to the set.
    ///
    fn enter_remembered(&mut self);

    ///
    /// Whether this object currently references any *young* (non-old) heap
    /// object. Used at promotion time (`remember-on-promote`): a freshly
    /// promoted object that still points into the young generation must be
    /// added to the remembered set, because those old→young edges predate
    /// the write barrier. See `doc/gc.md`.
    ///
    fn young_child_exists(&self, alloc: &Allocator<Self>) -> bool
    where
        Self: Sized;
}

///
/// Kind of a garbage collection cycle.
///
/// - `Major`: a full-heap collection. Both bitmaps are cleared, every
///   object (old and young) is re-marked from the roots, and the whole
///   heap is swept.
/// - `Minor`: a young-generation collection. `mark_bits` is seeded from
///   `old_bits` so old-generation objects start out "already marked" —
///   they are neither swept nor re-traversed; young objects reachable
///   only from old ones are reached via the remembered set.
///
/// Promotion is not enabled yet (no object carries the `OLD` flag), so
/// `old_bits` is always empty and a `Minor` cycle currently produces
/// exactly the same result as a `Major` one. See
/// `doc/gc.md`.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GcKind {
    Minor,
    Major,
}

pub struct Allocator<T> {
    /// Current page.
    current_page: PageRef<T>,
    /// Topmost page.
    head_page: PageRef<T>,
    /// Info for allocated pages.
    pages: Vec<PageRef<T>>,
    /// Allocated number of objects in current page.
    used_in_current: usize,
    /// Total allocated objects.
    total_allocated_objects: usize,
    /// Total blocks in free list.
    free_list_count: usize,
    /// Counter of marked objects,
    mark_counter: usize,
    /// List of free objects.
    free: Option<std::ptr::NonNull<T>>,
    /// Deallocated pages.
    free_pages: VecDeque<PageRef<T>>,
    /// Salvaged pages whose memory has been handed back to the OS
    /// (`release_page`): still part of the arena, reused after
    /// `free_pages` runs dry. Their contents are undefined until a
    /// page enters service again, which re-initialises everything it
    /// reads (`clear_old_bits`, then the bump allocator writes cells).
    released_pages: VecDeque<PageRef<T>>,
    /// Pages released to the OS so far (monotonic; `GC.stat`-style).
    total_released_pages: usize,
    /// Counter of GC execution.
    total_gc_counter: usize,
    /// Counter of minor (young-generation) GC executions. Always 0 until
    /// minor GC lands; see `doc/gc.md`.
    #[allow(dead_code)]
    minor_gc_count: usize,
    /// Counter of major (full-heap) GC executions.
    #[allow(dead_code)]
    major_gc_count: usize,
    /// Minor GCs performed since the last major GC. Drives the
    /// minor/major choice in `decide_gc_kind`.
    minors_since_major: usize,
    /// Live count of old-generation objects, maintained incrementally:
    /// `+1` per promotion in `apply_aging`, reset to 0 by `clear_old` at a
    /// major GC (which then re-promotes survivors). Drives the adaptive
    /// major trigger; avoids an O(pages) popcount per GC.
    old_count: usize,
    /// Adaptive major-GC threshold: when `old_count` reaches this, the next
    /// GC is a major. Recomputed at the end of each major as
    /// `max(old_count * OLD_GROWTH_FACTOR, OLD_OBJECT_FLOOR)`.
    old_major_threshold: usize,
    /// Whether the current mark phase should promote surviving
    /// promotable objects to the old generation. Set only during the
    /// real mark of a GC cycle; cleared so the `gc-verify` re-mark has
    /// no promotion side effects.
    promoting: bool,
    /// Whether the mark phase currently running belongs to a *major*
    /// collection. A major keeps the old generation (`old_bits` is not
    /// cleared) and marks old objects along with everything else, so it
    /// is the only phase that has to test `old_bits` before treating a
    /// freshly marked cell as an aging candidate.
    major_mark: bool,
    /// Promotable objects marked (survived) this cycle. Their age is
    /// incremented in the post-mark `apply_aging` pass, and those
    /// reaching `RGENGC_OLD_AGE` are promoted there (old_bits + header
    /// OLD). Deferred so the header writes never alias a `&self` held by
    /// the mark traversal.
    aging: Vec<*mut T>,
    /// Mark-phase work list: objects whose mark bit is set but whose
    /// children have not been scanned yet, because the walk had reached
    /// [`MARK_RECURSION_LIMIT`] when it got to them. Popped front-to-back
    /// by [`Allocator::drain_mark_queue`], so the deep part of the
    /// traversal is a **breadth-first** walk driven by this queue rather
    /// than by the native stack. See `doc/gc.md`.
    ///
    /// The queue holds one pointer per *marked but unscanned* object, so
    /// it peaks below `8 bytes × live objects` against the 64-byte cells
    /// those objects occupy — an eighth of the live heap in the worst
    /// case (every live object queued at once), and far less in practice.
    /// Its capacity is deliberately kept across collections: under
    /// `gc-stress` the mark phase runs once per allocation, and re-growing
    /// the buffer each time would dominate.
    mark_queue: VecDeque<std::ptr::NonNull<T>>,
    /// The object whose *deferred* scan is in progress — the entry
    /// `drain_mark_queue` (or `mark_remembered`) is currently on — or
    /// `None` while the walk is still in the root set.
    ///
    /// Forensics only, for the `DEAD RVALUE reached in mark` abort. The
    /// immediate referrer of a dead cell is always in the backtrace (its
    /// `mark_children` frame is what called the failing `mark`), but the
    /// ancestry above it is cut off at the most recent queue hop, because
    /// that ancestor is no longer a stack frame. This names it, which is
    /// the link the deferral costs. Maintained only where objects leave
    /// the queue — never on the recursive fast path, which is hot. See
    /// `RValue::mark` and `doc/gc.md`.
    mark_scanning: Option<std::ptr::NonNull<T>>,
    /// How many `mark_children` frames the current walk is nested in.
    /// Compared against [`MARK_RECURSION_LIMIT`] to decide between
    /// scanning a newly marked object's children right away and deferring
    /// them to `mark_queue`. Incremented and decremented in matched pairs
    /// by [`Allocator::scan_children`], so it is back at zero whenever the
    /// mark phase returns to the root walk.
    mark_depth: u32,
    /// Generational GC: remembered set — old-generation objects that
    /// hold a reference into the young generation, recorded by the write
    /// barrier (`RValue::write_barrier`). A minor GC scans these as
    /// extra roots; a major GC rebuilds generation state and clears it.
    /// Empty until promotion is enabled in a later phase (no object is
    /// `OLD` yet), so the barrier is currently inert. See
    /// `doc/gc.md`.
    remembered: Vec<std::ptr::NonNull<T>>,
    /// Arena pressure toward the next GC request: pages that filled to
    /// `THRESHOLD` since the last collection. At [`PAGES_PER_GC_TRIGGER`]
    /// the poll word's GC lane is armed; a completed collection resets
    /// this (`ack_gc_request`). The counter lives here — the poll word
    /// itself carries no arithmetic, only lane bits.
    pages_since_gc: u32,
    /// Flag whether GC is enabled or not.
    pub gc_enabled: bool,
    /// Use-after-free forensics (enabled by `MONORUBY_GC_FREE_LOG=1`):
    /// ring buffer of `(address, total_gc_counter, kind, was_old)` for
    /// every slot the sweep frees. When a stale-object assertion fires
    /// (e.g. `Value::as_array` on an `INVALID` header), the crash site
    /// looks the address up to learn *which* GC freed the object — a
    /// Minor implicates the write barrier / remembered set, a Major the
    /// root set.
    free_log: Vec<(usize, u32, u8, bool)>,
    /// Write cursor into `free_log` once it reached capacity.
    free_log_pos: usize,
    /// `GcKind` of the collection currently sweeping (forensics tag).
    current_kind: u8,
    /// Reference instant for `GcProfileRecord::invoke_time`, taken when
    /// the allocator is created (as close to process start as monoruby
    /// gets). CRuby measures its profiler's "Invoke Time" the same way.
    epoch: std::time::Instant,
    /// Wall-clock time spent inside `gc()`, split by phase. Accumulated
    /// only while `measure_time` is set — that is CRuby's
    /// `GC.measure_total_time`, which is on by default.
    gc_time_ns: u64,
    mark_time_ns: u64,
    sweep_time_ns: u64,
    /// `GC.measure_total_time`.
    measure_time: bool,
    /// Objects reclaimed by every sweep so far (`GC.stat`'s
    /// `total_freed_objects`).
    total_freed_objects: usize,
    /// Pages ever put into service, and pages ever salvaged back into
    /// `free_pages`. Both monotonic, as CRuby's counterparts are.
    total_allocated_pages: usize,
    total_freed_pages: usize,
    /// `GC::Profiler`: whether to append a record per collection, and
    /// the records collected so far.
    profile_enabled: bool,
    profile: Vec<GcProfileRecord>,
    /// `GC.stress`: re-arm the poll flag at the end of every collection
    /// so the next safepoint collects again.
    stress: bool,
    /// `GC.config[:rgengc_allow_full_mark]`. When false, `decide_gc_kind`
    /// never chooses a major collection on its own; an explicit
    /// `GC.start` still forces one.
    allow_full_mark: bool,
}

///
/// One collection's profile, recorded while `GC::Profiler` is enabled.
/// The field set mirrors what CRuby's `GC::Profiler.raw_data` reports.
///
#[derive(Debug, Clone, Copy)]
pub struct GcProfileRecord {
    /// Seconds from the allocator's epoch to the start of this
    /// collection (`GC_INVOKE_TIME`).
    pub invoke_time: f64,
    /// Nanoseconds spent in this collection (`GC_TIME`).
    pub gc_time_ns: u64,
    /// Bytes held by live objects after the collection (`HEAP_USE_SIZE`).
    pub heap_use_size: usize,
    /// Bytes of heap the collector owns (`HEAP_TOTAL_SIZE`).
    pub heap_total_size: usize,
    /// Slots in that heap (`HEAP_TOTAL_OBJECTS`).
    pub heap_total_objects: usize,
    /// Whether this was a major (full-heap) collection. CRuby reports
    /// `GC_IS_MARKED`, which is true for every non-lazy collection; for
    /// monoruby the major/minor distinction is the informative bit.
    pub major: bool,
}

/// Whether `MONORUBY_GC_FREE_LOG=1` forensics are enabled (cached).
pub(crate) fn free_log_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_GC_FREE_LOG").is_some())
}

/// Forensics: `MONORUBY_GC_ALL_MAJOR=1` forces every collection to be a
/// Major (full-heap) GC. A generational bug (missed write barrier /
/// remembered-set entry) disappears under this switch; a plain root-scan
/// bug does not. Diagnostic only — never enable in production.
fn all_major_forced() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_GC_ALL_MAJOR").is_some())
}

/// Stale-object crash forensics: report when (and by which GC kind) the
/// heap slot behind `v` was last freed, per the `MONORUBY_GC_FREE_LOG`
/// ring buffer. Called from assertion sites right before panicking on a
/// dead/reused RValue; no-op unless the log is enabled.
pub(crate) fn report_stale_object(site: &str, v: crate::Value) {
    if !free_log_enabled() {
        return;
    }
    if v.is_packed_value() {
        eprintln!(
            "[{site}] stale-object report: value {:016x} is not a heap pointer",
            v.id()
        );
        return;
    }
    let addr = v.id() as usize;
    let _ = ALLOC.try_with(|a| {
        let Ok(a) = a.try_borrow() else {
            eprintln!("[{site}] stale-object report: allocator busy");
            return;
        };
        match a.lookup_free_log(addr) {
            Some((gc, kind)) => eprintln!(
                "[{site}] stale object {addr:016x}: freed by GC #{gc} ({}) — current GC #{}",
                if kind == 0 { "Minor" } else { "Major" },
                a.gc_counter(),
            ),
            None => eprintln!(
                "[{site}] stale object {addr:016x}: no free record (never swept while logging, or evicted) — current GC #{}",
                a.gc_counter(),
            ),
        }
        // Who still holds a reference to the freed slot? An `old=true,
        // remembered=false` holder is the signature of a missed write
        // barrier; no holder at all means the reference lived outside the
        // heap (frame slot, Rust local, JIT register).
        let holders = a.find_holders(addr);
        if holders.is_empty() {
            eprintln!(
                "[{site}]   no heap holder contains this address — held outside the heap"
            );
        }
        for (h, marked, old, remembered) in holders {
            // SAFETY: forensics-only read of a heap cell header.
            let ty = unsafe { &*(h as *const crate::value::rvalue::RValue) }.ty();
            eprintln!(
                "[{site}]   holder {h:016x} ty={ty:?} marked={marked} old={old} remembered={remembered}"
            );
        }
    });
}

const FREE_LOG_CAP: usize = 1 << 21;

/// Forensics: a single heap address to trace (`MONORUBY_GC_TRACK=0x…`).
/// Every allocation returning it, every mark reaching it (with the mark
/// chain via backtrace), and every sweep freeing it are reported. Used
/// with ASLR disabled (`setarch -R`) so the address reproduces across
/// runs: run once to learn the victim address from the stale-object
/// report, re-run tracking it.
pub(crate) fn tracked_addr() -> Option<usize> {
    static ADDR: std::sync::OnceLock<Option<usize>> = std::sync::OnceLock::new();
    *ADDR.get_or_init(|| {
        let v = std::env::var("MONORUBY_GC_TRACK").ok()?;
        usize::from_str_radix(v.trim_start_matches("0x"), 16).ok()
    })
}

impl<T: GCBox> Allocator<T> {
    pub(crate) fn new() -> Self {
        assert_eq!(64, GCBOX_SIZE);
        assert!(std::mem::size_of::<Page<T>>() <= ALLOC_SIZE);
        let layout = Layout::from_size_align(ALLOC_SIZE * MAX_PAGES, ALLOC_SIZE).unwrap();
        let ptr = unsafe { System.alloc(layout) };
        let ptr: PageRef<T> = std::ptr::NonNull::new(ptr as _).unwrap();
        // The arena is freshly `System.alloc`'d (uninitialised). Zero the
        // first page's old-generation bitmap so the first minor GC seeds
        // `mark_bits` from zeros rather than arena garbage. (`mark_bits`
        // is always written by a major clear / minor seed before it is
        // read, so it needs no such pre-zeroing.)
        // SAFETY: `ptr` points at `ALLOC_SIZE` bytes of owned arena; only
        // the `old_bits` field is written here.
        unsafe { (*ptr.as_ptr()).clear_old_bits() };
        Allocator {
            current_page: ptr,
            head_page: ptr,
            pages: vec![],
            used_in_current: 0,
            total_allocated_objects: 0,
            free_list_count: 0,
            mark_counter: 0,
            free: None,
            free_pages: VecDeque::new(),
            released_pages: VecDeque::new(),
            total_released_pages: 0,
            total_gc_counter: 0,
            minor_gc_count: 0,
            major_gc_count: 0,
            minors_since_major: 0,
            old_count: 0,
            old_major_threshold: OLD_OBJECT_FLOOR,
            promoting: false,
            major_mark: false,
            aging: Vec::new(),
            mark_queue: VecDeque::new(),
            mark_scanning: None,
            mark_depth: 0,
            remembered: Vec::new(),
            pages_since_gc: 0,
            gc_enabled: true,
            free_log: Vec::new(),
            free_log_pos: 0,
            current_kind: 0,
            epoch: std::time::Instant::now(),
            gc_time_ns: 0,
            mark_time_ns: 0,
            sweep_time_ns: 0,
            measure_time: true,
            total_freed_objects: 0,
            // The first page is in service from the start.
            total_allocated_pages: 1,
            total_freed_pages: 0,
            profile_enabled: false,
            profile: Vec::new(),
            // Note: `gc-stress` builds do NOT set this — the runtime
            // `GC.stress` flag keeps CRuby-identical semantics. The
            // feature forces a collection at every safepoint directly in
            // `execute_gc` (see poll_flag::StressRearm), independent of
            // this flag.
            stress: false,
            allow_full_mark: true,
        }
    }





    fn new_page(&mut self) -> PageRef<T> {
        let ptr = unsafe { (self.head_page.as_ptr() as *mut u8).add(ALLOC_SIZE) } as _;
        let ptr = std::ptr::NonNull::new(ptr).unwrap();
        self.head_page = ptr;
        ptr
    }

    ///
    /// Address of the free-list head (`self.free`), exposed so the JIT can
    /// inline the free-list allocation fast path (pop a recycled cell)
    /// without a runtime call. `self.free` is `Option<NonNull<T>>`, which is
    /// pointer-sized with a null niche, so reading/writing it as `*mut usize`
    /// is sound (`0` == `None`). The allocator is a single-threaded
    /// thread-local whose address is stable for the process lifetime, and JIT
    /// code only touches it between safepoints (never while Rust holds a
    /// borrow of `ALLOC`, and never during `gc()`), so there is no aliasing.
    ///
    pub(crate) fn free_list_head_addr(&self) -> *mut usize {
        &self.free as *const _ as *mut usize
    }

    ///
    /// Address of `self.free_list_count`, kept in sync by the inline fast
    /// path so the gc-log/gc-debug bookkeeping stays correct.
    ///
    pub(crate) fn free_list_count_addr(&self) -> *mut usize {
        &self.free_list_count as *const _ as *mut usize
    }

    ///
    /// Address of `self.total_allocated_objects`, bumped by the inline fast
    /// path so allocation stats match the runtime path.
    ///
    pub(crate) fn total_allocated_addr(&self) -> *mut usize {
        &self.total_allocated_objects as *const _ as *mut usize
    }

    ///
    /// Address of `self.current_page`, the page bump allocation is carving
    /// cells out of. A `PageRef` is a non-null pointer, so reading it as
    /// `*mut usize` yields the page address. Exposed (with
    /// `used_in_current_addr`) so the JIT can inline the bump fast path;
    /// the free list alone is not enough, because a workload whose objects
    /// stay live never refills it.
    ///
    pub(crate) fn current_page_addr(&self) -> *mut usize {
        &self.current_page as *const _ as *mut usize
    }

    ///
    /// Address of `self.used_in_current`, the bump index into
    /// `current_page`. Incremented by the inline fast path exactly as
    /// `alloc` does.
    ///
    pub(crate) fn used_in_current_addr(&self) -> *mut usize {
        &self.used_in_current as *const _ as *mut usize
    }

    ///
    /// A page just filled to `THRESHOLD`: accumulate arena pressure and
    /// arm the poll word's GC lane once enough pages have filled since
    /// the last collection.
    ///
    fn on_page_pressure(&mut self) {
        self.pages_since_gc += 1;
        if self.pages_since_gc >= self.gc_trigger_pages() {
            crate::poll_flag::set_gc();
        }
    }

    ///
    /// How many pages may fill between collections: a fixed floor for
    /// small heaps, `1/GC_HEAP_FRACTION` of the pages in service once the
    /// heap outgrows it. See [`GC_HEAP_FRACTION`].
    ///
    /// Pages salvaged by a collection have left `pages` for `free_pages`,
    /// so a heap that drops its live set shrinks its own budget back
    /// without any extra bookkeeping.
    ///
    fn gc_trigger_pages(&self) -> u32 {
        // `+1` for `current_page`, which is in service but not in `pages`.
        let in_service = (self.pages.len() as u32).saturating_add(1);
        PAGES_PER_GC_TRIGGER.max(in_service / GC_HEAP_FRACTION)
    }

    ///
    /// A collection answered (or, with GC disabled, voided) the pending
    /// request: clear the GC lane and restart the arena-pressure count.
    /// Other lanes (preempt tick, pending signal) are untouched.
    ///
    fn ack_gc_request(&mut self) {
        self.pages_since_gc = 0;
        crate::poll_flag::clear_gc();
    }

    ///
    /// Returns a number of objects in the free list.
    /// (sweeped objects in the previous GC cycle.)
    ///
    #[allow(unused)]
    pub fn free_count(&self) -> usize {
        self.free_list_count
    }

    ///
    /// Returns a number of live objects in the previous GC cycle.
    ///
    #[allow(unused)]
    pub fn live_count(&self) -> usize {
        self.mark_counter
    }

    ///
    /// Returns a number of total allocated objects.
    ///
    #[allow(unused)]
    pub fn total_allocated(&self) -> usize {
        self.total_allocated_objects
    }

    ///
    /// Returns a number of total gc execution count.
    ///
    #[allow(unused)]
    pub fn total_gc_counter(&self) -> usize {
        self.total_gc_counter
    }

    ///
    /// Returns the number of minor (young-generation) GC executions.
    ///
    #[allow(unused)]
    pub fn minor_gc_count(&self) -> usize {
        self.minor_gc_count
    }

    ///
    /// Returns the number of major (full-heap) GC executions.
    ///
    #[allow(unused)]
    pub fn major_gc_count(&self) -> usize {
        self.major_gc_count
    }

    ///
    /// Pages currently in service: the filled ones plus the page bump
    /// allocation is carving out of.
    ///
    pub fn page_count(&self) -> usize {
        self.pages.len() + 1
    }

    /// Pages salvaged by a previous sweep and waiting to be reused
    /// (CRuby's `heap_empty_pages`).
    pub fn empty_page_count(&self) -> usize {
        self.free_pages.len() + self.released_pages.len()
    }

    /// Salvaged pages currently handed back to the OS (a subset of
    /// [`empty_page_count`](Self::empty_page_count)).
    pub fn released_page_count(&self) -> usize {
        self.released_pages.len()
    }

    /// Pages handed back to the OS so far (monotonic).
    pub fn total_released_pages(&self) -> usize {
        self.total_released_pages
    }

    /// Released pages that still have memory resident behind them, per
    /// `mincore(2)`. Linux drops an `MADV_DONTNEED` range at once, so this
    /// is zero right after a release. Test-only: it costs a syscall per
    /// page, and it is the process-independent way to check the release
    /// actually happened (a whole-process RSS reading is shared with every
    /// other test thread's arena).
    #[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
    pub(crate) fn resident_released_pages(&self) -> usize {
        // SAFETY: `sysconf` has no preconditions.
        let os_page = unsafe { libc::sysconf(libc::_SC_PAGESIZE) } as usize;
        let mut vec = vec![0u8; ALLOC_SIZE / os_page];
        self.released_pages
            .iter()
            .filter(|page| {
                // SAFETY: `page` is an `ALLOC_SIZE`-sized block of the arena
                // mapping and `vec` holds one byte per OS page of it.
                let rc = unsafe {
                    libc::mincore(page.as_ptr() as _, ALLOC_SIZE, vec.as_mut_ptr() as _)
                };
                rc == 0 && vec.iter().any(|b| b & 1 != 0)
            })
            .count()
    }

    /// Object slots the in-service pages hold in total.
    pub fn available_slots(&self) -> usize {
        self.page_count() * DATA_LEN
    }

    /// Slots that can still be handed out without putting another page
    /// into service: the free list plus the tail of the current page.
    pub fn allocatable_slots(&self) -> usize {
        self.free_list_count + (DATA_LEN - self.used_in_current)
    }

    /// Bytes one object slot occupies.
    pub const fn slot_size() -> usize {
        GCBOX_SIZE
    }

    /// Objects reclaimed by every sweep so far.
    pub fn total_freed(&self) -> usize {
        self.total_freed_objects
    }

    /// Pages ever put into service / ever salvaged back.
    pub fn total_allocated_pages(&self) -> usize {
        self.total_allocated_pages
    }

    pub fn total_freed_pages(&self) -> usize {
        self.total_freed_pages
    }

    /// Old-generation objects, and the threshold at which their growth
    /// forces the next major collection.
    pub fn old_count(&self) -> usize {
        self.old_count
    }

    pub fn old_objects_limit(&self) -> usize {
        self.old_major_threshold
    }

    /// Old-generation objects currently in the remembered set (they hold
    /// a reference into the young generation).
    pub fn remembered_count(&self) -> usize {
        self.remembered.len()
    }

    /// Total / mark-phase / sweep-phase nanoseconds spent collecting.
    /// Zero while `GC.measure_total_time` is off.
    pub fn gc_time_ns(&self) -> u64 {
        self.gc_time_ns
    }

    pub fn mark_time_ns(&self) -> u64 {
        self.mark_time_ns
    }

    pub fn sweep_time_ns(&self) -> u64 {
        self.sweep_time_ns
    }

    /// `GC.measure_total_time`.
    pub fn measure_time(&self) -> bool {
        self.measure_time
    }

    pub fn set_measure_time(&mut self, flag: bool) {
        self.measure_time = flag;
    }

    /// `GC.stress`. Turning it on arms the GC lane immediately, so the
    /// next safepoint collects without waiting for allocation pressure.
    pub fn stress(&self) -> bool {
        self.stress
    }

    pub fn set_stress(&mut self, flag: bool) {
        self.stress = flag;
        if flag {
            crate::poll_flag::set_gc();
        }
    }

    /// `GC.config[:rgengc_allow_full_mark]`.
    pub fn allow_full_mark(&self) -> bool {
        self.allow_full_mark
    }

    pub fn set_allow_full_mark(&mut self, flag: bool) {
        self.allow_full_mark = flag;
    }

    /// `GC::Profiler` state and the records collected so far.
    pub fn profile_enabled(&self) -> bool {
        self.profile_enabled
    }

    pub fn set_profile_enabled(&mut self, flag: bool) {
        self.profile_enabled = flag;
    }

    pub fn profile_records(&self) -> &[GcProfileRecord] {
        &self.profile
    }

    pub fn clear_profile(&mut self) {
        self.profile.clear();
    }

    ///
    /// Returns the number of old-generation objects (popcount of every
    /// page's `old_bits`). Confirms that promotion is taking effect, and
    /// cross-checks the incrementally maintained `old_count` field.
    ///
    #[cfg(any(feature = "gc-log", feature = "gc-debug"))]
    pub(crate) fn old_count_popcount(&self) -> usize {
        let mut c = 0;
        unsafe {
            c += self.current_page.as_ref().old_count();
            for p in self.pages.iter() {
                c += p.as_ref().old_count();
            }
        }
        c
    }

    ///
    /// Returns total active pages.
    ///
    #[allow(unused)]
    pub fn pages_len(&self) -> usize {
        self.pages.len() + 1
    }

    ///
    /// Allocate object.
    ///
    pub(crate) fn alloc(&mut self, data: T) -> *mut T {
        self.total_allocated_objects += 1;

        if let Some(gcbox) = self.free {
            // Allocate from the free list.
            let gcbox = gcbox.as_ptr();
            unsafe {
                self.free = (*gcbox).next();
                std::ptr::write(gcbox, data)
            }
            self.free_list_count -= 1;
            if let Some(addr) = tracked_addr()
                && gcbox as usize == addr
            {
                eprintln!(
                    "[GC-TRACK] allocated (free list) after GC #{}:\n{}",
                    self.total_gc_counter,
                    std::backtrace::Backtrace::force_capture()
                );
            }
            return gcbox;
        }

        let gcbox = if self.used_in_current == DATA_LEN {
            // Allocate new page.
            self.used_in_current = 1;
            self.pages.push(self.current_page);
            self.current_page = match self.take_free_page() {
                Some(page) => page,
                None => {
                    self.total_allocated_pages += 1;
                    self.new_page()
                }
            };
            // A page entering service must start with a zeroed
            // old-generation bitmap: fresh arena pages are uninitialised,
            // and salvaged pages may carry stale old bits. This keeps a
            // later minor GC's seed correct. (`mark_bits` is reset by the
            // next major clear / minor seed.)
            // SAFETY: `current_page` is a live, owned page.
            unsafe { self.current_page.as_mut().clear_old_bits() };
            unsafe { self.current_page.as_ref().get_first_cell() }
        } else {
            // Bump allocation.
            if self.used_in_current == THRESHOLD {
                self.on_page_pressure();
            }
            let ptr = unsafe { self.current_page.as_ref().get_cell(self.used_in_current) };
            self.used_in_current += 1;
            ptr
        };

        #[cfg(feature = "gc-debug")]
        {
            assert!(self.used_in_current <= DATA_LEN);
        }

        unsafe { std::ptr::write(gcbox, data) }
        gcbox
    }

    ///
    /// Decide whether the next collection is a minor or a major GC.
    ///
    /// Primary (adaptive) trigger: the old generation has grown to
    /// `old_major_threshold` (= `OLD_GROWTH_FACTOR`× its size right after
    /// the previous major, floored at `OLD_OBJECT_FLOOR`). A stable old
    /// generation majors rarely (keeping the generational win); one that
    /// keeps promoting floating garbage majors often (reclaiming it,
    /// bounding RSS). `MAX_MINORS_PER_MAJOR` is a hard safety cap so the
    /// remembered set is rebuilt and old garbage reclaimed even if the
    /// adaptive trigger never fires.
    ///
    /// A malloc-pressure-triggered collection (see `request_gc_if_malloc_over`)
    /// is intentionally not forced major here: transient buffers die young and
    /// are reclaimed by a minor GC, and old-generation buffer garbage is still
    /// caught by the triggers above.
    ///
    fn decide_gc_kind(&self) -> GcKind {
        // `GC.config[:rgengc_allow_full_mark] = false` takes the major
        // collection off the table entirely; only an explicit `GC.start`
        // can still force one.
        if !self.allow_full_mark {
            return GcKind::Minor;
        }
        if self.old_count >= self.old_major_threshold
            || self.minors_since_major >= MAX_MINORS_PER_MAJOR
        {
            GcKind::Major
        } else {
            GcKind::Minor
        }
    }

    pub(crate) fn gc(&mut self, root: &impl GCRoot<T>) {
        if !self.gc_enabled {
            // Void the request (`--no-gc`): leaving the GC lane armed
            // would send every subsequent safepoint through this no-op.
            self.ack_gc_request();
            return;
        }
        // A pending `GC.start` request forces a Major collection.
        let kind = if GC_FORCE_MAJOR.with(|f| f.replace(false)) || all_major_forced() {
            GcKind::Major
        } else {
            self.decide_gc_kind()
        };
        // Timing is off unless somebody is reading it: `GC.total_time`
        // (via `measure_total_time`) or `GC::Profiler`.
        let clock = (self.measure_time || self.profile_enabled).then(|| {
            (
                self.epoch.elapsed().as_secs_f64(),
                std::time::Instant::now(),
            )
        });
        self.total_gc_counter += 1;
        self.current_kind = match kind {
            GcKind::Minor => 0,
            GcKind::Major => 1,
        };
        match kind {
            GcKind::Minor => {
                self.minor_gc_count += 1;
                self.minors_since_major += 1;
            }
            GcKind::Major => {
                self.major_gc_count += 1;
                self.minors_since_major = 0;
            }
        }
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("#### GC start ({kind:?})");
            eprintln!(
                "allocated: {}  used in current page: {}  allocated pages: {}",
                self.total_allocated_objects,
                self.used_in_current,
                self.pages.len()
            );
        }
        // Prepare the mark bitmaps:
        // - Major: zero `mark_bits` only; every object becomes a
        //   collection candidate and is re-marked from the roots, but
        //   `old_bits` is *kept* — an object that was old and survives a
        //   major stays old (see below).
        // - Minor: seed `mark_bits` from `old_bits`, so old objects start
        //   "already marked" and are skipped by mark and sweep.
        match kind {
            // Surviving old objects keep their generation across a major:
            // demoting them would make the whole live old set re-age and
            // re-promote (an `aging` entry, three header writes and a
            // `young_child_exists` scan each), and would leave the minors
            // right after a major scanning a heap with no old generation
            // at all. Their `old_bits` therefore stay set here; the bits
            // of the ones that turn out to be *dead* are cleared where
            // their cells are actually reclaimed (`sweep` /
            // `salvage_empty_pages`), which is also where `old_count` is
            // decremented.
            //
            // Their remembered/armed classification (header OLD +
            // WB_ARMED, mirrored by `remembered`) is preserved for the
            // same reason: the write barrier keeps it correct for new
            // stores, entries that lost their young children are dropped
            // by `reclassify_remembered` below, and dead entries by
            // `filter_remembered` — so the set stays bounded without a
            // full-old-generation rescan.
            GcKind::Major => self.clear_mark(),
            GcKind::Minor => self.seed_marks(),
        }
        // Surviving objects may be promoted during the real mark.
        self.promoting = true;
        self.major_mark = kind == GcKind::Major;
        // The root walk marks everything within `MARK_RECURSION_LIMIT`
        // levels of a root and queues the rest; the drain then finishes
        // the deep part breadth-first (see `scan_children`). Nothing may
        // read the mark bits between the two.
        root.mark(self);
        self.drain_mark_queue();
        // A minor GC must also reach young objects referenced only from
        // old (already-marked) objects, via the remembered set.
        if kind == GcKind::Minor {
            self.mark_remembered();
            self.drain_mark_queue();
        }
        self.promoting = false;
        self.major_mark = false;
        // Age this cycle's promotable survivors and promote those old
        // enough (deferred from the mark phase to avoid aliasing). Safe:
        // marking is complete.
        self.apply_aging();
        // The incrementally maintained `old_count` must equal the actual
        // number of old cells (popcount of `old_bits`). Dead old cells are
        // still counted in both here; sweep drops them from each together.
        #[cfg(feature = "gc-debug")]
        debug_assert_eq!(self.old_count, self.old_count_popcount());
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("marked: {}  ", self.mark_counter);
        }
        // Drop dead entries from the remembered set before sweep frees
        // them: keep only objects still marked this cycle.
        self.filter_remembered();
        // A major keeps the old generation, so nothing else would ever
        // re-examine entries whose young children have since been
        // promoted or died. Re-classify the (small) surviving set here —
        // the minor path does the same in `mark_remembered`.
        if kind == GcKind::Major {
            self.reclassify_remembered();
        }
        let mark_elapsed = clock.map(|(_, t)| t.elapsed());
        self.salvage_empty_pages();
        self.sweep();
        // Re-arm the adaptive trigger relative to the *live* old
        // generation: major again once it has grown by
        // `OLD_GROWTH_FACTOR` (floored). Computed after sweep, which is
        // where dead old cells leave `old_count`.
        if kind == GcKind::Major {
            self.old_major_threshold = (self.old_count * OLD_GROWTH_FACTOR).max(OLD_OBJECT_FLOOR);
        }
        #[cfg(feature = "gc-debug")]
        debug_assert_eq!(self.old_count, self.old_count_popcount());
        // gc-verify: after a minor GC, independently re-mark the whole
        // live graph from the roots (no seeding, no promotion). If the
        // minor GC freed anything still reachable — a missed write
        // barrier / remembered-set entry — this traversal reaches a freed
        // slot and the `is_live` assertion in `RValue::mark` fires.
        #[cfg(feature = "gc-verify")]
        if kind == GcKind::Minor {
            self.clear_mark();
            // Every mark bit is gone, so this re-mark must reach everything —
            // including the metadata entries a minor is allowed to skip
            // (`Allocator::is_full_mark`).
            self.major_mark = true;
            root.mark(self);
            self.drain_mark_queue();
            self.major_mark = false;
        }
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            assert_eq!(self.free_list_count, self.check_free_list());
            eprintln!("free list: {}", self.free_list_count);
        }
        self.ack_gc_request();
        if let Some((invoke_time, started)) = clock {
            let total = started.elapsed().as_nanos() as u64;
            let mark = mark_elapsed.map_or(0, |d| d.as_nanos() as u64).min(total);
            if self.measure_time {
                self.gc_time_ns += total;
                self.mark_time_ns += mark;
                self.sweep_time_ns += total - mark;
            }
            if self.profile_enabled {
                let pages = self.page_count();
                self.profile.push(GcProfileRecord {
                    invoke_time,
                    gc_time_ns: total,
                    heap_use_size: self.mark_counter * GCBOX_SIZE,
                    heap_total_size: pages * DATA_LEN * GCBOX_SIZE,
                    heap_total_objects: pages * DATA_LEN,
                    major: kind == GcKind::Major,
                });
            }
        }
        // `GC.stress`: re-arm the GC lane so the very next safepoint
        // collects again.
        if self.stress {
            crate::poll_flag::set_gc();
        }
        let malloced = MALLOC_AMOUNT.load(std::sync::atomic::Ordering::SeqCst);
        // Allow malloc to grow by half the live amount (at least
        // MALLOC_THRESHOLD) before the next GC. Additive-only growth would
        // GC every 256 KB even on a multi-GB heap; the multiplicative term
        // keeps the trigger proportional so large but stable heaps don't
        // thrash, while small heaps (e.g. a `String#<<` loop) stay bounded.
        let next_threshold = malloced + (malloced / 2).max(MALLOC_THRESHOLD);
        MALLOC_GC_THRESHOLD.store(next_threshold, Ordering::Relaxed);
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("#### GC End");
        }
    }

    ///
    /// Generational GC: record `ptr` (an old-generation object that now
    /// references the young generation) in the remembered set. The
    /// caller — `RValue::write_barrier` — owns the `is_old` / dedup
    /// checks, so this just appends. See `doc/gc.md`.
    ///
    pub(crate) fn remember(&mut self, ptr: std::ptr::NonNull<T>) {
        self.remembered.push(ptr);
    }

    /// Mark object.
    /// If object is already marked, return true.
    /// If not yet, mark it and return false.
    pub(crate) fn gc_check_and_mark(&mut self, ptr: &T) -> bool {
        let p = ptr as *const T;
        if let Some(addr) = tracked_addr()
            && p as usize == addr
        {
            eprintln!(
                "[GC-TRACK] mark hit at GC #{} ({}):\n{}",
                self.total_gc_counter,
                if self.current_kind == 0 {
                    "Minor"
                } else {
                    "Major"
                },
                std::backtrace::Backtrace::force_capture()
            );
        }
        let page_ptr = self.get_page(p);

        let index = unsafe { (*page_ptr).get_index(p) };
        assert!(index < DATA_LEN);
        let bit_mask = 1 << (index % 64);
        let bitmap = unsafe { &mut (*page_ptr).mark_bits[index / 64] };

        let is_marked = (*bitmap & bit_mask) != 0;
        *bitmap |= bit_mask;
        if !is_marked {
            self.mark_counter += 1;
            // Collect promotable survivors; their age is bumped (and the
            // ones old enough are promoted) after marking, in
            // `apply_aging`, to avoid aliasing the `&self` the mark
            // traversal holds. Already-old objects never reach here in a
            // minor GC (they are seeded-marked and return early above),
            // but a major marks them like everything else — and they are
            // old already, so aging them again would only re-do the
            // promotion they have long since made. `major_mark` keeps the
            // extra bitmap read off the minor path, where it can never
            // find anything.
            if self.promoting
                && ptr.is_promotable()
                && !(self.major_mark
                    && unsafe { (*page_ptr).old_bits[index / 64] } & bit_mask != 0)
            {
                self.aging.push(p as *mut T);
            }
        }
        is_marked
    }

    ///
    /// Scan the children of a freshly marked object, or defer them to the
    /// mark queue once the walk is [`MARK_RECURSION_LIMIT`] levels deep.
    ///
    /// Callers (`RValue::mark`) set the mark bit with
    /// [`Allocator::gc_check_and_mark`] and, when it was not already set,
    /// hand the object here. Deferring is what keeps the mark phase off
    /// the native stack: a chain of N objects (`a = [a]` N times, a linked
    /// list, an ivar chain) used to cost N nested `mark` →
    /// `mark_children` → `mark` frames and overflowed the 8MB main stack
    /// at ~75K links; it now costs at most `MARK_RECURSION_LIMIT` frames
    /// plus queue entries on the heap. See `doc/gc.md`.
    ///
    pub(crate) fn scan_children(&mut self, ptr: &T) {
        if self.mark_depth < MARK_RECURSION_LIMIT {
            self.mark_depth += 1;
            ptr.mark_children(self);
            self.mark_depth -= 1;
            return;
        }
        // SAFETY: `ptr` is a live cell inside one of our pages (it was
        // just marked), so the address is non-null. Nothing frees it
        // before the queue is drained: sweep runs only after the mark
        // phase, by which point the queue is empty.
        self.mark_queue
            .push_back(unsafe { std::ptr::NonNull::new_unchecked(ptr as *const T as *mut T) });
    }

    ///
    /// Scan the children of every queued object until the queue runs dry
    /// — the mark phase's main loop. Children reached here are marked and
    /// either scanned inline or queued in turn (`scan_children`), so one
    /// drain reaches everything the roots did not already cover.
    ///
    /// Must be called after every root-marking step and before anything
    /// reads the mark bits (aging, `filter_remembered`, sweep).
    ///
    fn drain_mark_queue(&mut self) {
        // Entries are queued precisely because the walk had run out of
        // its stack budget; each one restarts it from zero.
        debug_assert_eq!(self.mark_depth, 0);
        while let Some(ptr) = self.mark_queue.pop_front() {
            // Forensics: record the entry, so a stale edge found below
            // can name where the walk resumed from (see `mark_scanning`).
            self.mark_scanning = Some(ptr);
            // SAFETY: entries are live, marked cells (see
            // `scan_children`); `mark_children` takes `&T` while this
            // borrows `self` mutably, and the two never alias — marking
            // only writes the allocator's bitmaps and side tables, never
            // the object. Same pattern as `mark_remembered`.
            unsafe { ptr.as_ref().mark_children(self) };
        }
        self.mark_scanning = None;
    }

    ///
    /// The queue entry whose scan is in progress, or `None` when the walk
    /// is still in the root set. Used only to report the ancestry the
    /// deferral cut out of the backtrace when marking reaches a dead cell.
    ///
    #[coverage(off)] // only read from the abort path, uncoverable in-test
    pub(crate) fn mark_referrer(&self) -> Option<std::ptr::NonNull<T>> {
        self.mark_scanning
    }

    ///
    /// Post-mark aging pass: bump each promotable survivor's age and
    /// promote (set `old_bits` + header `OLD`) those reaching
    /// `RGENGC_OLD_AGE`. Runs after marking, so no `&self` from the mark
    /// traversal is live and the `&mut` writes are sound.
    ///
    fn apply_aging(&mut self) {
        let aging = std::mem::take(&mut self.aging);
        // Pass 1: age every survivor and promote (set old_bits + header
        // OLD) those that reached the threshold. Collect them so the
        // remember-on-promote check runs only *after* all of this cycle's
        // promotions are visible — otherwise an object promoted before its
        // (same-cycle) children would be needlessly remembered.
        let mut promoted = Vec::new();
        for p in aging {
            // SAFETY: `p` was marked (hence live) this cycle and sweep
            // has not run, so the cell is valid and unaliased here.
            if unsafe { (*p).age_and_check_promote() } {
                let page_ptr = self.get_page(p);
                let index = unsafe { (*page_ptr).get_index(p) };
                let bit_mask = 1 << (index % 64);
                unsafe { (*page_ptr).old_bits[index / 64] |= bit_mask };
                unsafe { (*p).promote_to_old() };
                self.old_count += 1;
                promoted.push(p);
            }
        }
        // Pass 2: remember-on-promote. A freshly promoted object that
        // still references the young generation is added to the remembered
        // set (covering old→young edges that predate the write barrier);
        // one with only old children is left "armed" so a future young
        // store takes the barrier.
        for p in promoted {
            if unsafe { (*p).young_child_exists(self) } {
                unsafe { (*p).enter_remembered() };
                self.remembered
                    .push(unsafe { std::ptr::NonNull::new_unchecked(p) });
            } else {
                unsafe { (*p).arm_barrier() };
            }
        }
    }

    ///
    ///
    /// Whether the mark phase now running has to re-mark everything.
    ///
    /// True for a major (its `clear_mark` zeroed every mark bit) and for the
    /// `gc-verify` re-mark, which does the same. False for a minor, whose
    /// bits are seeded from the old generation — which is what lets
    /// `Store::mark` skip the metadata entries it knows hold only old
    /// values.
    ///
    pub(crate) fn is_full_mark(&self) -> bool {
        self.major_mark
    }

    /// Whether `ptr` belongs to the old generation (its `old_bits` is set).
    /// Used by `young_child_exists` for the remember-on-promote check.
    ///
    pub(crate) fn is_old(&self, ptr: &T) -> bool {
        let ptr = ptr as *const T;
        let page_ptr = self.get_page(ptr);
        let index = unsafe { (*page_ptr).get_index(ptr) };
        let bit_mask = 1 << (index % 64);
        unsafe { (*page_ptr).old_bits[index / 64] & bit_mask != 0 }
    }

    ///
    /// Forensics (stale-object reports): scan every heap cell's raw words
    /// for `addr` and return each holder cell's address together with its
    /// `(marked, old, remembered)` status. A holder that is `old` but not
    /// `remembered` is the signature of a missed write barrier. Linear in
    /// heap size; only called right before a stale-object panic.
    ///
    pub(crate) fn find_holders(&self, addr: usize) -> Vec<(usize, bool, bool, bool)> {
        let words = std::mem::size_of::<T>() / std::mem::size_of::<usize>();
        let mut out = Vec::new();
        let mut scan_page = |page: &Page<T>, len: usize| {
            for index in 0..len {
                let cell = page.get_cell(index) as usize;
                if cell == addr {
                    continue;
                }
                // SAFETY: forensics-only raw read of an initialized heap
                // cell (free-list cells are initialized too — their header
                // holds the next-pointer).
                let hit =
                    (0..words).any(|w| unsafe { (cell as *const usize).add(w).read() } == addr);
                if hit {
                    let bit = 1u64 << (index % 64);
                    let marked = page.mark_bits[index / 64] & bit != 0;
                    let old = page.old_bits[index / 64] & bit != 0;
                    let remembered = self.remembered.iter().any(|r| r.as_ptr() as usize == cell);
                    out.push((cell, marked, old, remembered));
                }
            }
        };
        for page in &self.pages {
            scan_page(unsafe { page.as_ref() }, DATA_LEN);
        }
        scan_page(unsafe { self.current_page.as_ref() }, self.used_in_current);
        out
    }
}

impl<T: GCBox> Allocator<T> {
    ///
    /// Clear all mark bitmaps.
    ///
    fn clear_mark(&mut self) {
        unsafe {
            self.current_page.as_mut().clear_bits();
            self.pages
                .iter_mut()
                .for_each(|heap| heap.as_mut().clear_bits());
        }
        self.mark_counter = 0;
    }

    ///
    /// Major GC: re-classify the surviving remembered set, dropping the
    /// entries that no longer reference the young generation (their
    /// children died, or were promoted by this very cycle) and arming
    /// their barrier again.
    ///
    /// This is the major-GC counterpart of the self-cleaning built into
    /// `mark_remembered`, and it is what keeps the set from growing
    /// monotonically now that a major no longer rebuilds it from
    /// scratch. Its cost is proportional to the remembered set — the
    /// live old→young edges — not to the whole old generation, which is
    /// exactly what the old rebuild-everything path cost.
    ///
    /// Must run after `apply_aging` (so this cycle's promotions are
    /// visible) and after `filter_remembered` (so every entry is live).
    /// Marking is complete, so every child of a live old object is live
    /// too: reading them before sweep is sound.
    ///
    fn reclassify_remembered(&mut self) {
        if self.remembered.is_empty() {
            return;
        }
        let remembered = std::mem::take(&mut self.remembered);
        let mut kept = Vec::with_capacity(remembered.len());
        for ptr in remembered {
            // SAFETY: entries are live (dead ones were just filtered out)
            // and sweep has not run, so the cell is valid here.
            if unsafe { ptr.as_ref().young_child_exists(self) } {
                kept.push(ptr);
            } else {
                unsafe { (*ptr.as_ptr()).arm_barrier() };
            }
        }
        self.remembered = kept;
    }

    ///
    /// Seed `mark_bits` from `old_bits` on every page (minor GC): old
    /// objects start out marked, so they are neither re-traversed nor
    /// swept. See `doc/gc.md`.
    ///
    fn seed_marks(&mut self) {
        unsafe {
            self.current_page.as_mut().seed_mark_from_old();
            self.pages
                .iter_mut()
                .for_each(|heap| heap.as_mut().seed_mark_from_old());
        }
        self.mark_counter = 0;
    }

    ///
    /// Minor GC: mark the young children of every remembered (old)
    /// object. The old objects are already (seed-)marked, so we must
    /// reach their children explicitly. (Empty until promotion lands.)
    ///
    fn mark_remembered(&mut self) {
        // Take the set out so the marking closure can borrow `self`
        // mutably; marking never mutates the remembered set (the write
        // barrier is not invoked during GC), so a snapshot is sound.
        let remembered = std::mem::take(&mut self.remembered);
        let mut kept = Vec::with_capacity(remembered.len());
        for ptr in remembered {
            // SAFETY: remembered entries are live old objects (kept
            // marked across the cycle; dead ones are dropped in
            // `filter_remembered` before sweep frees them).
            self.mark_scanning = Some(ptr);
            unsafe { ptr.as_ref().mark_children(self) };
            self.mark_scanning = None;
            // Self-clean: keep the entry only while it still references a
            // young object. Once all its children have themselves been
            // promoted, it no longer needs scanning — dropping it keeps
            // the remembered set (and thus minor GC cost) proportional to
            // the live old→young edges, not to every object ever promoted
            // with a then-young child. See `doc/gc.md`.
            if unsafe { ptr.as_ref().young_child_exists(self) } {
                kept.push(ptr);
            } else {
                // No young children left: drop from the set and re-arm, so
                // a future young store takes the barrier again.
                unsafe { (*ptr.as_ptr()).arm_barrier() };
            }
        }
        self.remembered = kept;
    }

    ///
    /// Drop remembered-set entries that are not marked this cycle (they
    /// are about to be swept). Surviving entries are retained. Must run
    /// after marking and before sweep.
    ///
    fn filter_remembered(&mut self) {
        if self.remembered.is_empty() {
            return;
        }
        let mut remembered = std::mem::take(&mut self.remembered);
        // SAFETY: every entry still points at a valid (not-yet-swept)
        // cell at this point in the cycle.
        remembered.retain(|ptr| self.is_marked(unsafe { ptr.as_ref() }));
        self.remembered = remembered;
    }

    ///
    /// Test whether `ptr` is marked in the current cycle (read-only;
    /// does not set the bit).
    ///
    fn is_marked(&self, ptr: &T) -> bool {
        let ptr = ptr as *const T;
        let page_ptr = self.get_page(ptr);
        let index = unsafe { (*page_ptr).get_index(ptr) };
        let bit_mask = 1 << (index % 64);
        unsafe { (*page_ptr).mark_bits[index / 64] & bit_mask != 0 }
    }

    ///
    /// Salvage empty pages and put into `free_pages`.
    ///
    fn salvage_empty_pages(&mut self) {
        let len = self.pages.len();
        for i in 0..len {
            unsafe {
                // We must check from the last page, because the page can be removed during iteration.
                if self.pages[len - i - 1].as_ref().all_dead() {
                    let mut page = self.pages.remove(len - i - 1);
                    // Every cell here is dead, and the page leaves
                    // `self.pages` before `sweep` runs — so this is the
                    // only chance to retire the old-generation accounting
                    // of the old cells that died on it. (`sweep` does the
                    // same for dead old cells on pages that stay.)
                    let stale_old = page.as_ref().old_count();
                    debug_assert!(stale_old <= self.old_count);
                    self.old_count = self.old_count.saturating_sub(stale_old);
                    page.as_mut().clear_old_bits();
                    page.as_mut().drop_inner_cells();
                    self.free_pages.push_back(page);
                    self.total_freed_pages += 1;
                    #[cfg(feature = "gc-debug")]
                    eprintln!("salvage: {:?}", page);
                }
            }
        }
        self.release_excess_free_pages();
    }

    /// The salvaged pages kept resident: see [`FREE_PAGE_RESERVE_FRACTION`].
    fn free_page_reserve(&self) -> usize {
        (self.page_count() / FREE_PAGE_RESERVE_FRACTION)
            .max(FREE_PAGE_RESERVE_MIN)
            .max(self.gc_trigger_pages() as usize)
    }

    /// Hand every salvaged page beyond the reserve back to the OS. The
    /// pages stay in the arena (on `released_pages`) and come back into
    /// service after the resident ones are used up.
    fn release_excess_free_pages(&mut self) {
        let reserve = self.free_page_reserve();
        while self.free_pages.len() > reserve {
            let page = self.free_pages.pop_back().unwrap();
            release_page(page);
            self.released_pages.push_back(page);
            self.total_released_pages += 1;
        }
    }

    /// A salvaged page for reuse: a resident one first, else one that was
    /// released to the OS (its memory is reclaimed on the way).
    fn take_free_page(&mut self) -> Option<PageRef<T>> {
        if let Some(page) = self.free_pages.pop_front() {
            return Some(page);
        }
        let page = self.released_pages.pop_front()?;
        reclaim_page(page);
        Some(page)
    }

    ///
    /// Sweep unmarked cells.
    ///
    fn sweep(&mut self) {
        fn sweep_bits<T: GCBox>(
            bit: usize,
            mut map: u64,
            ptr: &mut *mut T,
            head: &mut *mut T,
            log: &mut Option<Vec<usize>>,
        ) -> usize {
            let mut c = 0;
            let min = map.trailing_ones() as usize;
            *ptr = unsafe { (*ptr).add(min) };
            map = map.checked_shr(min as u32).unwrap_or(0);
            for _ in min..bit {
                if map & 1 == 0 {
                    unsafe {
                        (**head).set_next(*ptr);
                        *head = *ptr;
                        (**ptr).free();
                        (**ptr).set_next_none();
                        c += 1;
                    }
                    if let Some(log) = log {
                        log.push(*ptr as usize);
                    }
                }
                *ptr = unsafe { (*ptr).add(1) };
                map >>= 1;
            }
            c
        }

        /// Retire the old-generation bits of the dead cells in one
        /// bitmap word, returning how many there were.
        ///
        /// A dead cell goes on the free list and is handed out again as a
        /// fresh *young* object, so its `old_bits` entry must not
        /// survive it: a stale bit would seed-mark the reused cell in the
        /// next minor GC, which would then never scan the new object's
        /// children. Now that a major keeps `old_bits`, sweep is where
        /// dead old cells leave both the bitmap and `old_count`.
        #[inline]
        fn retire_dead_old(old: &mut u64, live: u64) -> usize {
            let dead_old = *old & !live;
            if dead_old == 0 {
                return 0;
            }
            *old &= live;
            dead_old.count_ones() as usize
        }

        let mut c = 0;
        let mut demoted = 0;
        let mut anchor = T::new_invalid();
        let head = &mut ((&mut anchor) as *mut T);
        let mut log = if free_log_enabled() {
            Some(Vec::new())
        } else {
            None
        };

        for pinfo in self.pages.iter_mut() {
            unsafe {
                let page = pinfo.as_mut();
                let mut ptr = page.get_first_cell();
                for w in 0..SIZE - 1 {
                    let map = page.mark_bits[w];
                    demoted += retire_dead_old(&mut page.old_bits[w], map);
                    c += sweep_bits(64, map, &mut ptr, head, &mut log);
                }
            }
        }

        let mut ptr = unsafe { self.current_page.as_ref().get_first_cell() };
        assert!(self.used_in_current <= DATA_LEN);
        let i = self.used_in_current / 64;
        let bit = self.used_in_current % 64;
        let page = unsafe { self.current_page.as_mut() };

        for w in 0..i {
            let map = page.mark_bits[w];
            demoted += retire_dead_old(&mut page.old_bits[w], map);
            c += sweep_bits(64, map, &mut ptr, head, &mut log);
        }

        if i < SIZE - 1 {
            let map = page.mark_bits[i];
            // Cells at or past `used_in_current` have never been handed
            // out, so their old bits are clear and the whole-word mask is
            // as accurate as the partial sweep below it.
            demoted += retire_dead_old(&mut page.old_bits[i], map);
            c += sweep_bits(bit, map, &mut ptr, head, &mut log);
        }

        debug_assert!(demoted <= self.old_count);
        self.old_count = self.old_count.saturating_sub(demoted);
        self.free = anchor.next();
        // Cells that were already on the free list are swept again (the
        // sweep is idempotent), so they appear in both counts: what this
        // collection actually reclaimed is the growth of the list.
        self.total_freed_objects += c.saturating_sub(self.free_list_count);
        self.free_list_count = c;
        if let Some(log) = log {
            let gc = self.total_gc_counter as u32;
            let kind = self.current_kind;
            for addr in log {
                self.push_free_log(addr, gc, kind);
            }
        }
    }

    /// Append one freed-slot record to the forensics ring buffer.
    fn push_free_log(&mut self, addr: usize, gc: u32, kind: u8) {
        if tracked_addr() == Some(addr) {
            eprintln!("[GC-TRACK] freed by sweep of GC #{gc} (kind {kind})");
        }
        if self.free_log.len() < FREE_LOG_CAP {
            self.free_log.push((addr, gc, kind, false));
        } else {
            self.free_log[self.free_log_pos] = (addr, gc, kind, false);
            self.free_log_pos = (self.free_log_pos + 1) % FREE_LOG_CAP;
        }
    }

    /// Most recent forensics record for `addr` (see `free_log`), plus the
    /// current GC counter for "how long ago" context.
    pub(crate) fn lookup_free_log(&self, addr: usize) -> Option<(u32, u8)> {
        let newest = self
            .free_log
            .iter()
            .filter(|(a, ..)| *a == addr)
            .max_by_key(|(_, gc, ..)| *gc)?;
        Some((newest.1, newest.2))
    }

    /// Current value of the GC cycle counter (forensics context).
    pub(crate) fn gc_counter(&self) -> usize {
        self.total_gc_counter
    }

    ///
    /// Get heap page from a pointer to T.
    ///
    fn get_page(&self, ptr: *const T) -> *mut Page<T> {
        let page_ptr: *mut Page<T> = (ptr as usize & !(ALLOC_SIZE - 1)) as _;

        #[cfg(feature = "gc-debug")]
        {
            if self.current_page.as_ptr() != page_ptr
                && self.pages.iter().all(|heap| heap.as_ptr() != page_ptr)
            {
                eprintln!("dump heap pages");
                self.pages.iter().for_each(|x| eprintln!("{:?}", x));
                eprintln!("{:?}", self.current_page);
                panic!("The ptr is not in heap pages. {:?}", ptr);
            };
        }

        page_ptr
    }
}

// For debug
#[cfg(feature = "gc-debug")]
impl<T: GCBox> Allocator<T> {
    fn check_free_list(&self) -> usize {
        let mut c = 0;
        let mut free = self.free;
        while let Some(f) = free {
            let p = f.as_ptr();
            self.get_page(p);
            free = unsafe { (*p).next() };
            c += 1;
        }
        c
    }

    /*fn print_bits(&self, bitmap: &[u64; SIZE - 1]) {
        let mut i = 0;
        bitmap.iter().for_each(|m| {
            eprint!("{:016x} ", m.reverse_bits());
            if i % 8 == 7 {
                eprintln!();
            }
            i += 1;
        });
    }*/

    /*pub(crate) fn print_mark(&self) {
        self.pages.iter().for_each(|pinfo| {
            self.print_bits(pinfo.mark_bits());
            eprintln!("\n");
        });
        self.print_bits(self.current.mark_bits());
        eprintln!("\n");
        eprintln!(
            "GC Info----------------------------------------------------------------------------"
        );
        eprintln!(
            "active pages: {} free pages:{}",
            self.pages.len() + 1,
            self.free_pages.len(),
        );
        assert_eq!(self.free_list_count, self.check_free_list());
        eprintln!(
            "free list:{} allocated:{}  used in current page:{}",
            self.free_list_count, self.allocated, self.used_in_current
        );
    }*/
}

/// Hand a whole, all-dead page back to the OS while keeping its address
/// range in the arena. Linux drops the pages outright (`MADV_DONTNEED`:
/// RSS falls now, a later touch faults in zero pages); macOS is told they
/// are reusable (`MADV_FREE_REUSABLE`, the form whose accounting also
/// lowers RSS immediately). Other platforms keep the page resident. The
/// page is a whole `ALLOC_SIZE`-aligned block, so the advice covers
/// exactly its cells and bitmaps.
fn release_page<T>(page: PageRef<T>) {
    #[cfg(target_os = "linux")]
    let advice = libc::MADV_DONTNEED;
    #[cfg(target_os = "macos")]
    let advice = libc::MADV_FREE_REUSABLE;
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        let _ = page;
        return;
    }
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    // SAFETY: `page` is an `ALLOC_SIZE`-aligned, `ALLOC_SIZE`-sized block
    // of the arena mapping that no live object references (every cell was
    // dropped by `drop_inner_cells` and none is on the free list).
    // Advice failing only means the memory stays resident.
    unsafe {
        let _ = libc::madvise(page.as_ptr() as *mut libc::c_void, ALLOC_SIZE, advice);
    }
}

/// The counterpart of [`release_page`] when a released page re-enters
/// service. Linux needs nothing (the first touch faults the pages back
/// in); macOS wants `MADV_FREE_REUSE` so its accounting counts them again.
fn reclaim_page<T>(page: PageRef<T>) {
    #[cfg(target_os = "macos")]
    // SAFETY: as in `release_page`; the page is about to be re-initialised.
    unsafe {
        let _ = libc::madvise(
            page.as_ptr() as *mut libc::c_void,
            ALLOC_SIZE,
            libc::MADV_FREE_REUSE,
        );
    }
    #[cfg(not(target_os = "macos"))]
    let _ = page;
}

///
/// Heap page struct.
///
/// Single page occupies `ALLOC_SIZE` bytes in memory.
/// This struct contains 64 * (`SIZE` - 1) `GCBox` cells, and bitmap (`SIZE` - 1 bytes each) for marking phase.
///
struct Page<T> {
    data: [T; DATA_LEN],
    mark_bits: [u64; SIZE - 1],
    /// Generational GC: bitmap of old-generation cells, parallel to
    /// `mark_bits`. Reserved here so the page layout is fixed up front;
    /// it is populated and consulted once minor GC lands (see
    /// `doc/gc.md`). Adding it must keep
    /// `size_of::<Page<T>>() <= ALLOC_SIZE` (asserted in `Allocator::new`).
    old_bits: [u64; SIZE - 1],
}

impl<T: GCBox> std::fmt::Debug for Page<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Page")
    }
}

type PageRef<T> = std::ptr::NonNull<Page<T>>;

impl<T: GCBox> Page<T> {
    ///
    /// Drop all T in the page.
    ///
    fn drop_inner_cells(&self) {
        let mut ptr = self.get_first_cell();
        for _ in 0..DATA_LEN {
            unsafe { (*ptr).free() };
            ptr = unsafe { ptr.add(1) };
        }
    }

    ///
    /// Get a raw pointer of T with `index`.
    ///
    fn get_cell(&self, index: usize) -> *mut T {
        &self.data[index] as *const _ as *mut _
    }

    ///
    /// Get a raw pointer of the first T in the page.
    ///
    fn get_first_cell(&self) -> *mut T {
        self.get_cell(0)
    }

    fn get_index(&self, ptr: *const T) -> usize {
        unsafe { ptr.offset_from(self.get_first_cell()) as usize }
    }

    ///
    /// Clear marking bitmap.
    ///
    fn clear_bits(&mut self) {
        self.mark_bits.iter_mut().for_each(|e| *e = 0)
    }

    ///
    /// Clear old-generation bitmap. Used by a major GC, which demotes
    /// every object back to a collection candidate. Reserved for the
    /// generational GC phases; see `doc/gc.md`.
    ///
    fn clear_old_bits(&mut self) {
        self.old_bits.iter_mut().for_each(|e| *e = 0)
    }

    ///
    /// Seed the mark bitmap from the old-generation bitmap (minor GC):
    /// every old cell starts out marked. See `doc/gc.md`.
    ///
    fn seed_mark_from_old(&mut self) {
        self.mark_bits.copy_from_slice(&self.old_bits);
    }

    ///
    /// Number of old-generation cells in this page (popcount of `old_bits`).
    ///
    fn old_count(&self) -> usize {
        self.old_bits.iter().map(|w| w.count_ones() as usize).sum()
    }

    ///
    /// Check whether all objects were dead.
    ///
    fn all_dead(&self) -> bool {
        self.mark_bits.iter().all(|bits| *bits == 0)
    }
}
