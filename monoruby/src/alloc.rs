use crate::RValue;
use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct RurubyAlloc;

unsafe impl GlobalAlloc for RurubyAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        MALLOC_AMOUNT.fetch_add(layout.size(), Ordering::SeqCst);
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        MALLOC_AMOUNT.fetch_sub(layout.size(), Ordering::SeqCst);
        unsafe { System.dealloc(ptr, layout) }
    }
}

#[global_allocator]
pub static GLOBAL_ALLOC: RurubyAlloc = RurubyAlloc;

pub static MALLOC_AMOUNT: AtomicUsize = AtomicUsize::new(0);

thread_local!(
    pub static ALLOC: RefCell<Allocator<RValue>> = RefCell::new(Allocator::new());
);

const SIZE: usize = 64;
const GCBOX_SIZE: usize = std::mem::size_of::<RValue>();
const PAGE_LEN: usize = 64 * SIZE;
const DATA_LEN: usize = 64 * (SIZE - 1);
const THRESHOLD: usize = 64 * (SIZE - 2);
const ALLOC_SIZE: usize = PAGE_LEN * GCBOX_SIZE; // 2^18 = 256kb
const MALLOC_THRESHOLD: usize = 256 * 1024;
const MAX_PAGES: usize = 8192;

/// Number of minor (young-generation) collections allowed between two
/// major (full-heap) collections. A major GC demotes every object and
/// rebuilds generation state; see `doc/generational_gc_plan.md`.
const MINORS_PER_MAJOR: usize = 64;

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
    /// reached. See `doc/generational_gc_plan.md`.
    ///
    fn mark_children(&self, alloc: &mut Allocator<Self>)
    where
        Self: Sized;

    ///
    /// Whether this object may be promoted to the old generation when it
    /// survives a collection. Only objects that are provably safe to
    /// skip in a minor GC should return `true` — currently those with no
    /// outgoing references at promotion time. See
    /// `doc/generational_gc_plan.md`.
    ///
    fn is_promotable(&self) -> bool;
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
/// `doc/generational_gc_plan.md`.
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
    /// Counter of GC execution.
    total_gc_counter: usize,
    /// Counter of minor (young-generation) GC executions. Always 0 until
    /// minor GC lands; see `doc/generational_gc_plan.md`.
    #[allow(dead_code)]
    minor_gc_count: usize,
    /// Counter of major (full-heap) GC executions.
    #[allow(dead_code)]
    major_gc_count: usize,
    /// Minor GCs performed since the last major GC. Drives the
    /// minor/major choice in `decide_gc_kind`.
    minors_since_major: usize,
    /// Whether the current mark phase should promote surviving
    /// promotable objects to the old generation. Set only during the
    /// real mark of a GC cycle; cleared so the `gc-verify` re-mark has
    /// no promotion side effects.
    promoting: bool,
    /// Generational GC: remembered set — old-generation objects that
    /// hold a reference into the young generation, recorded by the write
    /// barrier (`RValue::write_barrier`). A minor GC scans these as
    /// extra roots; a major GC rebuilds generation state and clears it.
    /// Empty until promotion is enabled in a later phase (no object is
    /// `OLD` yet), so the barrier is currently inert. See
    /// `doc/generational_gc_plan.md`.
    remembered: Vec<std::ptr::NonNull<T>>,
    /// Flag for GC timing.
    alloc_flag: Option<*mut u32>,
    /// Flag whether GC is enabled or not.
    pub gc_enabled: bool,
    /// Threshold of malloced memory for invoking GC.
    pub malloc_threshold: usize,
    /// Registry of promoted heap frames (`Box<[u64]>` buffers that
    /// `move_frame_to_heap` / `heap_frame` previously leaked via
    /// `Box::into_raw`). Keyed by the LFP address. Reclaimed by
    /// `sweep_heap_frames` after the mark phase: a frame must stay
    /// unmarked for two consecutive GC cycles before its buffer is
    /// freed (a one-cycle grace covering the promote→root-store
    /// window).
    heap_frames: std::collections::HashMap<usize, FrameRec, AddrHashBuilder>,
}

/// Fast hasher for the heap-frame registry. Keys are LFP addresses
/// (8-byte-aligned `usize`s). The default SipHash is far too slow for
/// a table that is inserted into on every frame promotion and probed
/// on every heap-frame mark — under `gc-stress` that is once per
/// allocation, so SipHash there dominates the whole run. A single
/// Fibonacci-hash multiply spreads the aligned addresses well enough
/// for the Swiss table.
#[derive(Default, Clone, Copy)]
struct AddrHasher(u64);

impl std::hash::Hasher for AddrHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        // Only ever used with `usize` keys (`write_usize`); this path
        // is unreachable but kept total for safety.
        for &b in bytes {
            self.0 = (self.0 ^ b as u64).wrapping_mul(0x0100_0000_01b3);
        }
    }
    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.0 = (i as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }
}

#[derive(Default, Clone, Copy)]
struct AddrHashBuilder;

impl std::hash::BuildHasher for AddrHashBuilder {
    type Hasher = AddrHasher;
    #[inline]
    fn build_hasher(&self) -> AddrHasher {
        AddrHasher(0)
    }
}

#[derive(Clone, Copy)]
struct FrameRec {
    /// Base pointer of the original `Box<[u64]>` allocation.
    base: *mut u64,
    /// Length of that slice in `u64` units.
    len: usize,
    marked: bool,
    /// Consecutive GC cycles seen unmarked.
    unmarked_age: u8,
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
            total_gc_counter: 0,
            minor_gc_count: 0,
            major_gc_count: 0,
            minors_since_major: 0,
            promoting: false,
            remembered: Vec::new(),
            alloc_flag: None,
            gc_enabled: true,
            malloc_threshold: MALLOC_THRESHOLD,
            heap_frames: std::collections::HashMap::default(),
        }
    }

    /// Register a promoted heap frame so the GC can reclaim its
    /// `Box<[u64]>` buffer once it becomes unreachable.
    pub(crate) fn register_heap_frame(&mut self, lfp_addr: usize, base: *mut u64, len: usize) {
        self.heap_frames.insert(
            lfp_addr,
            FrameRec {
                base,
                len,
                // A freshly promoted frame is conceptually live until
                // proven otherwise; start marked so the very next GC
                // (before it is necessarily root-reachable) never frees
                // it.
                marked: true,
                unmarked_age: 0,
            },
        );
    }

    /// Mark a heap frame reached during the GC mark phase. Returns
    /// `true` if it was already marked this cycle (caller then stops
    /// recursing — the outer chain is a DAG). Unknown addresses (a
    /// frame whose registration was skipped) return `false` and are
    /// never reclaimed.
    pub(crate) fn mark_heap_frame(&mut self, lfp_addr: usize) -> bool {
        match self.heap_frames.get_mut(&lfp_addr) {
            Some(rec) => {
                let was = rec.marked;
                rec.marked = true;
                was
            }
            None => false,
        }
    }

    fn clear_frame_marks(&mut self) {
        for rec in self.heap_frames.values_mut() {
            rec.marked = false;
        }
    }

    /// Free the `Box<[u64]>` of every heap frame that has stayed
    /// unmarked for two consecutive GC cycles.
    fn sweep_heap_frames(&mut self) {
        // Single pass, no per-GC scratch allocation: `retain` ages
        // every entry and frees + drops the ones unreachable for two
        // consecutive cycles in place. Avoiding the old `Vec<usize>`
        // matters because under `gc-stress` this runs once per
        // allocation.
        self.heap_frames.retain(|_, rec| {
            if rec.marked {
                rec.unmarked_age = 0;
                return true;
            }
            rec.unmarked_age = rec.unmarked_age.saturating_add(1);
            if rec.unmarked_age < 2 {
                return true;
            }
            // SAFETY: `base`/`len` are exactly the raw parts of the
            // original `Box<[u64]>` (recorded at promotion); the
            // frame has been unreachable for two GC cycles, so no
            // live `Lfp` aliases it.
            unsafe {
                drop(Box::from_raw(std::ptr::slice_from_raw_parts_mut(
                    rec.base, rec.len,
                )));
            }
            false
        });
    }

    fn new_page(&mut self) -> PageRef<T> {
        let ptr = unsafe { (self.head_page.as_ptr() as *mut u8).add(ALLOC_SIZE) } as _;
        let ptr = std::ptr::NonNull::new(ptr).unwrap();
        self.head_page = ptr;
        ptr
    }

    ///
    /// Set address of allocation flag.
    ///
    pub(crate) fn set_alloc_flag_address(&mut self, address: *mut u32) {
        self.alloc_flag = Some(address);
    }

    ///
    /// Set allocation flag.
    ///
    fn set_alloc_flag(&self) {
        if let Some(flag) = self.alloc_flag {
            unsafe { *flag += 1 }
        }
    }

    ///
    /// Unset allocation flag.
    ///
    fn unset_alloc_flag(&self) {
        if let Some(flag) = self.alloc_flag {
            unsafe { *flag = 0 }
        }
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
    #[cfg(feature = "gc-log")]
    pub fn total_allocated(&self) -> usize {
        self.total_allocated_objects
    }

    ///
    /// Returns a number of total gc execution count.
    ///
    #[cfg(feature = "gc-log")]
    pub fn total_gc_counter(&self) -> usize {
        self.total_gc_counter
    }

    ///
    /// Returns the number of minor (young-generation) GC executions.
    ///
    #[cfg(feature = "gc-log")]
    pub fn minor_gc_count(&self) -> usize {
        self.minor_gc_count
    }

    ///
    /// Returns the number of major (full-heap) GC executions.
    ///
    #[cfg(feature = "gc-log")]
    pub fn major_gc_count(&self) -> usize {
        self.major_gc_count
    }

    ///
    /// Returns the number of old-generation objects (popcount of every
    /// page's `old_bits`). Confirms that promotion is taking effect.
    ///
    #[cfg(feature = "gc-log")]
    pub fn old_count(&self) -> usize {
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
            return gcbox;
        }

        let gcbox = if self.used_in_current == DATA_LEN {
            // Allocate new page.
            self.used_in_current = 1;
            self.pages.push(self.current_page);
            self.current_page = self
                .free_pages
                .pop_front()
                .unwrap_or_else(|| self.new_page());
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
                self.set_alloc_flag();
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
    /// A major GC is forced once `MINORS_PER_MAJOR` minor GCs have run
    /// since the last major one, so the old generation is eventually
    /// reclaimed and the remembered set rebuilt. (With promotion not yet
    /// enabled this only affects which counter advances.)
    ///
    fn decide_gc_kind(&self) -> GcKind {
        if self.minors_since_major >= MINORS_PER_MAJOR {
            GcKind::Major
        } else {
            GcKind::Minor
        }
    }

    pub(crate) fn gc(&mut self, root: &impl GCRoot<T>) {
        if !self.gc_enabled {
            return;
        }
        let kind = self.decide_gc_kind();
        self.total_gc_counter += 1;
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
        // - Major: zero `mark_bits` and `old_bits`; every object becomes
        //   a collection candidate and is re-marked from the roots.
        // - Minor: seed `mark_bits` from `old_bits`, so old objects start
        //   "already marked" and are skipped by mark and sweep.
        // (Currently `old_bits` is always empty — nothing is promoted —
        // so the two paths are equivalent. See generational_gc_plan.md.)
        match kind {
            GcKind::Major => {
                self.clear_mark();
                self.clear_old();
            }
            GcKind::Minor => self.seed_marks(),
        }
        // Skip all heap-frame bookkeeping entirely when nothing has
        // been promoted. The common case (optcarrot and most
        // benchmarks promote few or no frames) then pays exactly zero
        // — important under `gc-stress`, where `gc()` runs once per
        // allocation.
        let has_heap_frames = !self.heap_frames.is_empty();
        if has_heap_frames {
            self.clear_frame_marks();
        }
        // Surviving objects may be promoted during the real mark.
        self.promoting = true;
        root.mark(self);
        // A minor GC must also reach young objects referenced only from
        // old (already-marked) objects, via the remembered set.
        if kind == GcKind::Minor {
            self.mark_remembered();
        }
        self.promoting = false;
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("marked: {}  ", self.mark_counter);
        }
        // Drop dead entries from the remembered set before sweep frees
        // them: keep only objects still marked this cycle.
        self.filter_remembered();
        self.salvage_empty_pages();
        self.sweep();
        if has_heap_frames {
            self.sweep_heap_frames();
        }
        // gc-verify: after a minor GC, independently re-mark the whole
        // live graph from the roots (no seeding, no promotion). If the
        // minor GC freed anything still reachable — a missed write
        // barrier / remembered-set entry — this traversal reaches a freed
        // slot and the `is_live` assertion in `RValue::mark` fires.
        #[cfg(feature = "gc-verify")]
        if kind == GcKind::Minor {
            self.clear_mark();
            root.mark(self);
        }
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            assert_eq!(self.free_list_count, self.check_free_list());
            eprintln!("free list: {}", self.free_list_count);
        }
        self.unset_alloc_flag();
        let malloced = MALLOC_AMOUNT.load(std::sync::atomic::Ordering::SeqCst);
        self.malloc_threshold = malloced + MALLOC_THRESHOLD;
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("#### GC End");
        }
    }

    ///
    /// Generational GC: record `ptr` (an old-generation object that now
    /// references the young generation) in the remembered set. The
    /// caller — `RValue::write_barrier` — owns the `is_old` / dedup
    /// checks, so this just appends. See `doc/generational_gc_plan.md`.
    ///
    pub(crate) fn remember(&mut self, ptr: std::ptr::NonNull<T>) {
        self.remembered.push(ptr);
    }

    /// Mark object.
    /// If object is already marked, return true.
    /// If not yet, mark it and return false.
    pub(crate) fn gc_check_and_mark(&mut self, ptr: &T) -> bool {
        let p = ptr as *const T;
        let page_ptr = self.get_page(p);

        let index = unsafe { (*page_ptr).get_index(p) };
        assert!(index < DATA_LEN);
        let bit_mask = 1 << (index % 64);
        let bitmap = unsafe { &mut (*page_ptr).mark_bits[index / 64] };

        let is_marked = (*bitmap & bit_mask) != 0;
        *bitmap |= bit_mask;
        if !is_marked {
            self.mark_counter += 1;
            // Promote a surviving object to the old generation by setting
            // its `old_bits`. Next minor GC seeds `mark_bits` from
            // `old_bits`, so it is then skipped. Only promotable objects
            // (no outgoing references) qualify; see generational_gc_plan.md.
            if self.promoting && ptr.is_promotable() {
                unsafe { (*page_ptr).old_bits[index / 64] |= bit_mask };
            }
        }
        is_marked
    }

    ///
    /// Whether `ptr` belongs to the old generation (its `old_bits` is set).
    /// Single source of truth for old-ness, used by the write barrier.
    ///
    pub(crate) fn is_old_obj(&self, ptr: &T) -> bool {
        let ptr = ptr as *const T;
        let page_ptr = self.get_page(ptr);
        let index = unsafe { (*page_ptr).get_index(ptr) };
        let bit_mask = 1 << (index % 64);
        unsafe { (*page_ptr).old_bits[index / 64] & bit_mask != 0 }
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
    /// Clear all old-generation bitmaps (major GC demotes every object
    /// to a collection candidate). See `doc/generational_gc_plan.md`.
    ///
    fn clear_old(&mut self) {
        unsafe {
            self.current_page.as_mut().clear_old_bits();
            self.pages
                .iter_mut()
                .for_each(|heap| heap.as_mut().clear_old_bits());
        }
    }

    ///
    /// Seed `mark_bits` from `old_bits` on every page (minor GC): old
    /// objects start out marked, so they are neither re-traversed nor
    /// swept. See `doc/generational_gc_plan.md`.
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
        for ptr in remembered.iter() {
            // SAFETY: remembered entries are live old objects (kept
            // marked across the cycle; dead ones are dropped in
            // `filter_remembered` before sweep frees them).
            unsafe { ptr.as_ref().mark_children(self) };
        }
        self.remembered = remembered;
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
                    page.as_mut().drop_inner_cells();
                    self.free_pages.push_back(page);
                    #[cfg(feature = "gc-debug")]
                    eprintln!("salvage: {:?}", page);
                }
            }
        }
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
                }
                *ptr = unsafe { (*ptr).add(1) };
                map >>= 1;
            }
            c
        }

        let mut c = 0;
        let mut anchor = T::new_invalid();
        let head = &mut ((&mut anchor) as *mut T);

        for pinfo in self.pages.iter_mut() {
            unsafe {
                let mut ptr = pinfo.as_ref().get_first_cell();
                for map in pinfo.as_mut().mark_bits.iter() {
                    c += sweep_bits(64, *map, &mut ptr, head);
                }
            }
        }

        let mut ptr = unsafe { self.current_page.as_ref().get_first_cell() };
        assert!(self.used_in_current <= DATA_LEN);
        let i = self.used_in_current / 64;
        let bit = self.used_in_current % 64;
        let bitmap = unsafe { self.current_page.as_mut().mark_bits };

        for map in bitmap.iter().take(i) {
            c += sweep_bits(64, *map, &mut ptr, head);
        }

        if i < SIZE - 1 {
            c += sweep_bits(bit, bitmap[i], &mut ptr, head);
        }

        self.free = anchor.next();
        self.free_list_count = c;
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
    /// `doc/generational_gc_plan.md`). Adding it must keep
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
    /// generational GC phases; see `doc/generational_gc_plan.md`.
    ///
    fn clear_old_bits(&mut self) {
        self.old_bits.iter_mut().for_each(|e| *e = 0)
    }

    ///
    /// Seed the mark bitmap from the old-generation bitmap (minor GC):
    /// every old cell starts out marked. See `doc/generational_gc_plan.md`.
    ///
    fn seed_mark_from_old(&mut self) {
        self.mark_bits.copy_from_slice(&self.old_bits);
    }

    ///
    /// Number of old-generation cells in this page (popcount of `old_bits`).
    ///
    #[cfg(feature = "gc-log")]
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
