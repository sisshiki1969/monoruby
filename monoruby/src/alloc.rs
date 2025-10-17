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
    /// Flag for GC timing.
    alloc_flag: Option<*mut u32>,
    /// Flag whether GC is enabled or not.
    pub gc_enabled: bool,
    /// Threshold of malloced memory for invoking GC.
    pub malloc_threshold: usize,
}

impl<T: GCBox> Allocator<T> {
    pub(crate) fn new() -> Self {
        assert_eq!(64, GCBOX_SIZE);
        assert!(std::mem::size_of::<Page<T>>() <= ALLOC_SIZE);
        let layout = Layout::from_size_align(ALLOC_SIZE * MAX_PAGES, ALLOC_SIZE).unwrap();
        let ptr = unsafe { System.alloc(layout) };
        let ptr = std::ptr::NonNull::new(ptr as _).unwrap();
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
            alloc_flag: None,
            gc_enabled: true,
            malloc_threshold: MALLOC_THRESHOLD,
        }
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

    pub(crate) fn gc(&mut self, root: &impl GCRoot<T>) {
        if !self.gc_enabled {
            return;
        }
        self.total_gc_counter += 1;
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("#### GC start");
            eprintln!(
                "allocated: {}  used in current page: {}  allocated pages: {}",
                self.total_allocated_objects,
                self.used_in_current,
                self.pages.len()
            );
        }
        self.clear_mark();
        root.mark(self);
        #[cfg(feature = "gc-debug")]
        if root.startup_flag() {
            eprintln!("marked: {}  ", self.mark_counter);
        }
        self.salvage_empty_pages();
        self.sweep();
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

    /// Mark object.
    /// If object is already marked, return true.
    /// If not yet, mark it and return false.
    pub(crate) fn gc_check_and_mark(&mut self, ptr: &T) -> bool {
        let ptr = ptr as *const T;
        let page_ptr = self.get_page(ptr);

        let index = unsafe { (*page_ptr).get_index(ptr) };
        assert!(index < DATA_LEN);
        let bit_mask = 1 << (index % 64);
        let bitmap = unsafe { &mut (*page_ptr).mark_bits[index / 64] };

        let is_marked = (*bitmap & bit_mask) != 0;
        *bitmap |= bit_mask;
        if !is_marked {
            self.mark_counter += 1;
        }
        is_marked
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
    /// Check whether all objects were dead.
    ///
    fn all_dead(&self) -> bool {
        self.mark_bits.iter().all(|bits| *bits == 0)
    }
}
