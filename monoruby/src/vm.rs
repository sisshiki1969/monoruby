//! The interpreter instance: everything that used to be a
//! per-OS-thread `thread_local!` singleton, gathered into one struct.
//!
//! monoruby's runtime state has always been "one interpreter per OS
//! thread": the allocator, the JIT, the green-thread scheduler and a
//! handful of caches each lived in their own `thread_local!`, and the
//! test harness relied on that to run one interpreter per test thread.
//! A 1:1 thread model (one kernel thread per Ruby `Thread`) breaks that
//! identification — several OS threads then share one interpreter — so
//! this module makes the identification explicit and *indirect*: the
//! state lives in a [`Vm`], and each OS thread carries only a pointer to
//! the `Vm` it serves ([`CURRENT`]).
//!
//! Nothing about ownership changes yet. The `Vm` is created lazily on
//! the first access from an OS thread that has none (exactly like the
//! `thread_local!`s it replaces), is owned by that thread, and is torn
//! down when the thread exits. The per-field accessors ([`VmField`])
//! keep the `KEY.with(|cell| ...)` shape of `std::thread::LocalKey`, so
//! the call sites read as they always did.
//!
//! What stays a genuine `thread_local!` is what is per *OS thread*
//! rather than per interpreter: the poll-word address `poll_flag` reads
//! from inside the global allocator, the scheduler-entry depth of the
//! running context, and the recursion guards of `hash` / `inspect`.

use std::cell::{Cell, OnceCell, RefCell};
use std::collections::HashMap;

use crate::alloc::Allocator;
use crate::codegen::Codegen;
use crate::gvl::{Gvl, GvlThread};
use crate::scheduler::Scheduler;
use crate::{ClassId, IdentId, RValue};

/// One interpreter instance.
///
/// Fields whose construction is not `const` (or must run *after* the
/// `Vm` is reachable through [`CURRENT`], as `Codegen::new` does, since
/// it registers the poll word and reads the allocator's addresses) are
/// `OnceCell`s initialised on first use — the same laziness the
/// `thread_local!`s had.
pub(crate) struct Vm {
    /// The Global VM Lock, held by the thread that created this `Vm`
    /// (its handle is `main_thread`) until other kernel threads exist.
    gvl: Gvl,
    main_thread: GvlThread,
    /// The GC heap (`alloc::ALLOC`).
    alloc: OnceCell<RefCell<Allocator<RValue>>>,
    /// `GC.start` asked for a Major collection at the next safepoint.
    gc_force_major: Cell<bool>,
    /// The VM-tier and JIT code generator (`codegen::CODEGEN`). Dropped
    /// first: its `Drop` detaches the poll word from the registries.
    codegen: OnceCell<RefCell<Codegen>>,
    /// The green-thread scheduler (`scheduler::SCHEDULER`).
    scheduler: OnceCell<RefCell<Scheduler>>,
    /// The scheduler loop's saved machine context; its address is baked
    /// into the context switch stubs, so it must not move — it lives in
    /// the `Box` this `Vm` is allocated in.
    sched_rsp: Cell<u64>,
    /// The preempt timer's state (`preempt::STATE`).
    preempt: OnceCell<RefCell<crate::preempt::State>>,
    /// Every live `ObjectSpace::WeakMap`, as raw cells (not roots).
    weakmaps: RefCell<Vec<*mut RValue>>,
    /// See [`PROBE_MEMOS`].
    probe_memos: RefCell<Vec<Box<[u64; 2]>>>,
    /// The generic-ivar inline cache of the JIT runtime.
    generic_ivar_table: OnceCell<RefCell<Box<[crate::codegen::runtime::GenericIvarEntry]>>>,
    /// Cached `ClassId` of `Enumerator::ArithmeticSequence`.
    as_class_id_cache: Cell<Option<ClassId>>,
    /// `JSON::Ext::Generator::State` / `JSON::Fragment`, once loaded.
    json_state_class: Cell<Option<ClassId>>,
    json_fragment_class: Cell<Option<ClassId>>,
    /// The constant-epoch tables (`globals::const_epoch`).
    const_epoch_wildcard: Cell<u64>,
    const_epoch_names: OnceCell<RefCell<HashMap<IdentId, u64>>>,
    /// The global `Regexp.timeout`, in nanoseconds (0 = unset).
    regexp_global_timeout: Cell<u64>,
    /// File descriptors owned by a live autoclosing `FileDescriptor`
    /// (`rvalue::io::OWNED_FDS`).
    owned_fds: RefCell<crate::HashSet<i32>>,
}

impl Vm {
    /// Only `const`-constructible state; everything else is lazy. Must
    /// not touch [`CURRENT`]: it is called before the `Vm` is installed.
    fn new() -> Self {
        let (gvl, main_thread) = Gvl::new();
        Self {
            gvl,
            main_thread,
            alloc: OnceCell::new(),
            gc_force_major: Cell::new(false),
            codegen: OnceCell::new(),
            scheduler: OnceCell::new(),
            sched_rsp: Cell::new(0),
            preempt: OnceCell::new(),
            weakmaps: RefCell::new(Vec::new()),
            probe_memos: RefCell::new(Vec::new()),
            generic_ivar_table: OnceCell::new(),
            as_class_id_cache: Cell::new(None),
            json_state_class: Cell::new(None),
            json_fragment_class: Cell::new(None),
            const_epoch_wildcard: Cell::new(0),
            const_epoch_names: OnceCell::new(),
            regexp_global_timeout: Cell::new(0),
            owned_fds: RefCell::new(crate::HashSet::default()),
        }
    }
}

#[cfg(test)]
static DROPPED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

impl Vm {
    /// The Global VM Lock and the creating thread's handle on it.
    pub(crate) fn gvl(&self) -> (&Gvl, &GvlThread) {
        (&self.gvl, &self.main_thread)
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        #[cfg(test)]
        DROPPED.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // The preempt timer (another OS thread) writes into the poll
        // word inside `codegen`'s JIT memory. `Codegen::drop` detaches
        // it through `preempt::codegen_dropped`, which reaches the timer
        // state through `CURRENT` — already cleared by the time the
        // fields drop — so detach here, with the state in hand, before
        // `codegen` (the first field) is freed.
        if let Some(st) = self.preempt.get() {
            crate::preempt::detach(&st.borrow());
        }
    }
}

thread_local! {
    /// The `Vm` this OS thread serves; null until the first access.
    /// A `const` `Cell` (no destructor, no allocation) so the lookup is
    /// a plain TLS load.
    static CURRENT: Cell<*const Vm> = const { Cell::new(std::ptr::null()) };

    /// Ownership of the `Vm` this thread created. Its destructor tears
    /// the `Vm` down at thread exit — after clearing `CURRENT`, so the
    /// teardown never observes a half-dropped instance through it.
    static OWNER: Cell<Option<VmOwner>> = const { Cell::new(None) };
}

struct VmOwner(*mut Vm);

impl Drop for VmOwner {
    fn drop(&mut self) {
        CURRENT.with(|c| c.set(std::ptr::null()));
        // SAFETY: the pointer came from `Box::into_raw` in `vm()`, is
        // owned by this thread alone, and nothing reaches it any more:
        // `CURRENT` no longer points at it.
        drop(unsafe { Box::from_raw(self.0) });
    }
}

/// This OS thread's interpreter, created on first use.
#[inline]
pub(crate) fn vm() -> &'static Vm {
    let p = CURRENT.with(|c| c.get());
    if p.is_null() {
        return create();
    }
    // SAFETY: non-null only between `create` and `VmOwner::drop`, which
    // clears it before freeing the `Vm`. Handing out `'static` mirrors
    // what `LocalKey::with` gives its closure for the thread's lifetime;
    // no reference escapes a `VmField::with` closure, and the `Vm` is
    // never dropped while this thread runs.
    unsafe { &*p }
}

/// Serve an interpreter another OS thread owns: the kernel thread of a
/// Ruby `Thread` in the 1:1 model. The thread must have no interpreter
/// of its own, and must call [`unadopt`] before it exits (the owner
/// tears the `Vm` down, and only after every adopter is gone).
pub(crate) fn adopt(vm: &Vm) {
    CURRENT.with(|c| {
        assert!(c.get().is_null(), "this OS thread already serves a Vm");
        c.set(vm as *const Vm);
    });
}

/// Undo [`adopt`].
pub(crate) fn unadopt() {
    let _ = CURRENT.try_with(|c| c.set(std::ptr::null()));
}

/// Like [`vm`], but `None` when this thread has no interpreter yet or
/// is tearing it down (the `try_with` of a `LocalKey`).
#[inline]
pub(crate) fn vm_try() -> Option<&'static Vm> {
    let p = CURRENT.try_with(|c| c.get()).unwrap_or(std::ptr::null());
    // SAFETY: as in `vm`.
    (!p.is_null()).then(|| unsafe { &*p })
}

#[cold]
fn create() -> &'static Vm {
    let p = Box::into_raw(Box::new(Vm::new()));
    CURRENT.with(|c| c.set(p));
    // During TLS teardown `OWNER` may already be gone; the `Vm` then
    // leaks, which is what an unreachable `thread_local!` did too.
    let _ = OWNER.try_with(|o| o.set(Some(VmOwner(p))));
    // SAFETY: freshly allocated, now owned by this thread (see `vm`).
    unsafe { &*p }
}

/// A field of the current thread's [`Vm`], with the `with` / `try_with`
/// surface of a `std::thread::LocalKey` so the former `thread_local!`
/// singletons keep their call sites.
pub(crate) struct VmField<T: 'static> {
    get: fn(&Vm) -> &T,
}

impl<T: 'static> VmField<T> {
    pub(crate) const fn new(get: fn(&Vm) -> &T) -> Self {
        Self { get }
    }

    #[inline]
    pub(crate) fn with<R>(&'static self, f: impl FnOnce(&T) -> R) -> R {
        f((self.get)(vm()))
    }

    /// `None` when this thread has no interpreter (or is tearing it
    /// down) — a `LocalKey::try_with` that failed.
    #[inline]
    pub(crate) fn try_with<R>(&'static self, f: impl FnOnce(&T) -> R) -> Option<R> {
        vm_try().map(|vm| f((self.get)(vm)))
    }
}

impl<T: 'static> VmField<RefCell<T>> {
    pub(crate) fn with_borrow<R>(&'static self, f: impl FnOnce(&T) -> R) -> R {
        self.with(|c| f(&c.borrow()))
    }

    pub(crate) fn with_borrow_mut<R>(&'static self, f: impl FnOnce(&mut T) -> R) -> R {
        self.with(|c| f(&mut c.borrow_mut()))
    }
}

impl<T: Copy + 'static> VmField<Cell<T>> {
    pub(crate) fn get(&'static self) -> T {
        self.with(|c| c.get())
    }

    pub(crate) fn set(&'static self, v: T) {
        self.with(|c| c.set(v))
    }
}

// The fields, as the statics their modules export. Each getter is a
// plain field projection (or a `get_or_init` for the lazy ones).

pub(crate) static ALLOC: VmField<RefCell<Allocator<RValue>>> =
    VmField::new(|vm| vm.alloc.get_or_init(|| RefCell::new(Allocator::new())));

pub(crate) static GC_FORCE_MAJOR: VmField<Cell<bool>> = VmField::new(|vm| &vm.gc_force_major);

pub(crate) static CODEGEN: VmField<RefCell<Codegen>> =
    VmField::new(|vm| vm.codegen.get_or_init(|| RefCell::new(Codegen::new())));

pub(crate) static SCHEDULER: VmField<RefCell<Scheduler>> =
    VmField::new(|vm| vm.scheduler.get_or_init(|| RefCell::new(Scheduler::new())));

pub(crate) static SCHED_RSP: VmField<Cell<u64>> = VmField::new(|vm| &vm.sched_rsp);

pub(crate) static PREEMPT_STATE: VmField<RefCell<crate::preempt::State>> =
    VmField::new(|vm| vm.preempt.get_or_init(|| RefCell::new(crate::preempt::State::new())));

pub(crate) static WEAKMAPS: VmField<RefCell<Vec<*mut RValue>>> = VmField::new(|vm| &vm.weakmaps);

/// The JIT hash-probe key memos (see `gen_hash_probe`): addresses of the
/// per-site `last_key` words, zeroed at every collection so a dead key's
/// recycled address can never alias a later hit.
pub(crate) static PROBE_MEMOS: VmField<RefCell<Vec<Box<[u64; 2]>>>> =
    VmField::new(|vm| &vm.probe_memos);

/// Allocate one probe-memo word pair `(last_key, last_digest)` and
/// return its address, to be baked into the emitting probe. The Vm owns
/// the box, so the pair lives exactly as long as the code that points
/// at it.
pub(crate) fn alloc_probe_memo() -> usize {
    PROBE_MEMOS.with(|v| {
        let mut v = v.borrow_mut();
        v.push(Box::new([0, 0]));
        v.last().unwrap().as_ptr() as usize
    })
}

/// Zero every registered probe-memo key word — the GC's part of the memo
/// protocol (`Root::clear_weak_refs`): a memo hit is an *identity* match,
/// so a key that did not survive this collection must not leave its
/// address behind for a future String to alias. Zero never matches (a
/// `Value` is `NonZeroU64`), and the digest word needs no clearing — it
/// is unreachable without a key match.
pub(crate) fn zero_probe_memos() {
    PROBE_MEMOS.with(|v| {
        for pair in v.borrow_mut().iter_mut() {
            pair[0] = 0;
        }
    });
}

pub(crate) static GENERIC_IVAR_TABLE: VmField<RefCell<Box<[crate::codegen::runtime::GenericIvarEntry]>>> =
    VmField::new(|vm| {
        vm.generic_ivar_table
            .get_or_init(|| RefCell::new(crate::codegen::runtime::GenericIvarEntry::new_table()))
    });

pub(crate) static AS_CLASS_ID_CACHE: VmField<Cell<Option<ClassId>>> =
    VmField::new(|vm| &vm.as_class_id_cache);

pub(crate) static JSON_STATE_CLASS: VmField<Cell<Option<ClassId>>> =
    VmField::new(|vm| &vm.json_state_class);

pub(crate) static JSON_FRAGMENT_CLASS: VmField<Cell<Option<ClassId>>> =
    VmField::new(|vm| &vm.json_fragment_class);

pub(crate) static CONST_EPOCH_WILDCARD: VmField<Cell<u64>> =
    VmField::new(|vm| &vm.const_epoch_wildcard);

pub(crate) static CONST_EPOCH_NAMES: VmField<RefCell<HashMap<IdentId, u64>>> =
    VmField::new(|vm| vm.const_epoch_names.get_or_init(|| RefCell::new(HashMap::new())));

pub(crate) static REGEXP_GLOBAL_TIMEOUT: VmField<Cell<u64>> =
    VmField::new(|vm| &vm.regexp_global_timeout);

pub(crate) static OWNED_FDS: VmField<RefCell<crate::HashSet<i32>>> = VmField::new(|vm| &vm.owned_fds);

#[cfg(test)]
mod tests {
    use super::*;

    /// Each OS thread gets its own `Vm`, and a field's address is stable
    /// for the thread's lifetime (the switch stubs bake `SCHED_RSP`'s in).
    #[test]
    fn one_vm_per_os_thread_with_stable_fields() {
        let here = SCHED_RSP.with(|c| c.as_ptr() as usize);
        assert_eq!(here, SCHED_RSP.with(|c| c.as_ptr() as usize));
        let there = std::thread::spawn(|| SCHED_RSP.with(|c| c.as_ptr() as usize))
            .join()
            .unwrap();
        assert_ne!(here, there);
        assert!(vm_try().is_some());
    }

    /// A thread's `Vm` — and with it the heap's 2 GB reservation and the
    /// JIT memory — is torn down when the thread exits, as the
    /// `thread_local!`s it replaced were.
    #[test]
    fn the_vm_is_dropped_when_its_thread_exits() {
        use std::sync::atomic::Ordering;
        let before = DROPPED.load(Ordering::SeqCst);
        std::thread::spawn(|| {
            ALLOC.with(|a| assert!(a.try_borrow().is_ok()));
            CODEGEN.with(|c| assert!(c.try_borrow().is_ok()));
        })
        .join()
        .unwrap();
        // `>`: other tests' threads drop their own `Vm`s concurrently.
        assert!(DROPPED.load(Ordering::SeqCst) > before);
    }
}
